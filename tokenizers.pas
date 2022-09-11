unit Tokenizers;

{$mode Delphi}

interface

uses Classes, SysUtils, CharOrNewLineReaders, Recognizers;

type
  TRowCol = record
    Row: Word;
    Col: Word;
  end;

  TPosition = record
    Start: TRowCol;
    Finish: TRowCol;
  end;

  TToken = record
    Text: String;
    // if < 0, token is not matched
    // if >= 0, it's the recognizer that found it
    Kind: Integer;
    Position: TPosition;
  end;

  TTokenizer = class
  private
    FReader: TPushBackCharOrNewLineReader;
    FRecognizers: TTokenRecognizers;
    FRowCol: TRowCol;
  public
    constructor Create(Reader: TPushBackCharOrNewLineReader; Recognizers: TTokenRecognizers);
    destructor Destroy; override;
    function Read: TToken;
  end;

  PTokenNode = ^TTokenNode;
  TTokenNode = record
    Data: TToken;
    Next: PTokenNode;
  end;

  TTokenLinkedList = class
  private
    FHead: PTokenNode;
  public
    constructor Create;
    destructor Destroy; override;
    function IsEmpty: Boolean;
    function Pop: TToken;
    procedure Push(Token: TToken);
    procedure Append(List: TTokenLinkedList);
  end;

  TUndoTokenizer = class
  private
    FTokenizer: TTokenizer;
    FBuffer: TTokenLinkedList;
  public
    constructor Create(Tokenizer: TTokenizer);
    destructor Destroy; override;
    function Read: TToken;
    procedure Undo(Token: TToken); overload;
    procedure Undo(var TokenList: TTokenLinkedList); overload;
  end;

function CreateUndoTokenizer(Stream: TStream; Recognizers: TTokenRecognizers): TUndoTokenizer;

implementation

type
  TRecognizerResponses = class
  private
    FResponses: array of TRecognition;
  public
    constructor Create(Length: Integer);
    function GetLastResponse(Index: Integer): TRecognition;
    procedure SetLastResponse(Index: Integer; Recognition: TRecognition);
    { How many responses exist of the given recognition value }
    function CountByRecognition(Recognition: TRecognition): Integer;
  end;

constructor TRecognizerResponses.Create(Length: Integer);
var
  i: Integer;
begin
  SetLength(FResponses, Length);
  for i := 0 to Length - 1 do
    FResponses[i] := rPartial;
end;

function TRecognizerResponses.GetLastResponse(Index: Integer): TRecognition;
begin
  Result := FResponses[Index];
end;

procedure TRecognizerResponses.SetLastResponse(Index: Integer; Recognition: TRecognition);
begin
  FResponses[Index] := Recognition;
end;

function TRecognizerResponses.CountByRecognition(Recognition: TRecognition): Integer;
var
  i: Integer;
  sum: Integer;
begin
  sum := 0;
  for i := 0 to Length(FResponses) - 1 do
    if FResponses[i] = Recognition then Inc(sum);
  Result := sum;
end;

(* Tokenizer *)

constructor TTokenizer.Create(Reader: TPushBackCharOrNewLineReader; Recognizers: TTokenRecognizers);
begin
  FReader := Reader;
  FRecognizers := Recognizers;
  with FRowCol do
  begin
    Row := 1;
    Col := 1;
  end;
end;

destructor TTokenizer.Destroy;
var
  i: Integer;
begin
  FReader.Free;
  for i := Low(FRecognizers) to High(FRecognizers) do
    FRecognizers[i].Free;
  inherited Destroy;
end;

function TTokenizer.Read: TToken;
var
  Buffer: TCharOrNewLineArray;
  Next: TCharOrNewLine;
  Size: Integer;
  NoMatchOrEof: Boolean;
  i: Integer;
  RecognizerResponses: TRecognizerResponses;
  LastResponse, Recognition: TRecognition;
  Sizes: array of Integer;
  MaxPositiveSize: Integer;
  MaxPositiveIndex: Integer;
begin
  Size := 0;
  NoMatchOrEof := False;

  // initialize recognizer responses
  // prime all with "partial"
  RecognizerResponses := TRecognizerResponses.Create(Length(FRecognizers));
  SetLength(Sizes, Length(FRecognizers));

  repeat
    Next := FReader.Read;
    if Next.Kind <> ckEof then
    begin
      { add read character to buffer}
      Inc(Size);
      SetLength(Buffer, Size);
      Buffer[Size - 1] := Next;

      { find which recognizer can work with the buffer, if any }
      for i := Low(FRecognizers) to High(FRecognizers) do
      begin
        LastResponse := RecognizerResponses.GetLastResponse(i);
        if LastResponse <> rNegative then
        begin
          Recognition := FRecognizers[i].Recognize(Buffer);
          RecognizerResponses.SetLastResponse(i, Recognition);
          if Recognition = rPositive then
          begin
            // remember the Buffer size at this point for this guy
            Sizes[i] := Size;
          end
          else if (Recognition = rNegative) and (LastResponse = rPartial) then
          begin
            // this recognizer got disqualified without ever reaching the goal
            Sizes[i] := 0;
          end
        end
      end;

      // exit loop if everyone is done
      NoMatchOrEof := RecognizerResponses.CountByRecognition(rNegative) = Length(FRecognizers);
    end
    else
    begin
      // EOF
      NoMatchOrEof := True;
    end
  until NoMatchOrEof;

  // out of all the positive responses, which one was the longest?
  // this allows to have tokens '>=' and '>' and let '>=' win
  MaxPositiveSize := 0;
  MaxPositiveIndex := -1;
  for i := Low(FRecognizers) to High(FRecognizers) do
  begin
    if Sizes[i] > MaxPositiveSize then
    begin
      MaxPositiveSize := Sizes[i];
      MaxPositiveIndex := i;
    end;
  end;

  // unread any extra 'CharOrNewLine' back into the reader
  while Size > MaxPositiveSize do begin
    Dec(Size);
    FReader.UnRead(Buffer[Size]);
    SetLength(Buffer, Size);
  end;

  Result.Kind := MaxPositiveIndex;
  if MaxPositiveIndex >= 0 then
  begin
    // fill-in the result based on the buffer
    Result.Position.Start := FRowCol;
    Result.Text := '';
    for i := Low(Buffer) To High(Buffer) do
    begin
      Next := Buffer[i];
      if Next.Kind = ckEol then
      begin
        Inc(FRowCol.Row);
        FRowCol.Col := 1;
        case Next.NewLineKind of
          nlCR: Result.Text := Result.Text + #13;
          nlLF: Result.Text := Result.Text + #10;
          nlCRLF: Result.Text := Result.Text + #13 + #10;
        end;
      end
      else
      begin
        Inc(FRowCol.Col);
        Result.Text := Result.Text + Next.Ch;
      end
    end;
    Result.Position.Finish := FRowCol;
  end;
end;

(* Undo Tokenizer *)

constructor TUndoTokenizer.Create(Tokenizer: TTokenizer);
begin
  FTokenizer := Tokenizer;
  FBuffer := TTokenLinkedList.Create;
end;

destructor TUndoTokenizer.Destroy;
begin
  FTokenizer.Free;
  FBuffer.Free;
  inherited Destroy;
end;

function TUndoTokenizer.Read: TToken;
begin
  if FBuffer.IsEmpty then
    Result := FTokenizer.Read
  else
    Result := FBuffer.Pop
end;

procedure TUndoTokenizer.Undo(Token: TToken);
begin
  FBuffer.Push(Token);
end;

procedure TUndoTokenizer.Undo(var TokenList: TTokenLinkedList);
begin
  while not TokenList.IsEmpty do
    Undo(TokenList.Pop);
  FreeAndNil(TokenList);
end;

constructor TTokenLinkedList.Create;
begin
  FHead := nil;
end;

destructor TTokenLinkedList.Destroy;
var
  temp: PTokenNode;
begin
  while not IsEmpty do
  begin
    temp := FHead;
    FHead := FHead^.Next;
    Dispose(temp);
  end;

  inherited Destroy;
end;

function TTokenLinkedList.IsEmpty: Boolean;
begin
  Result := not Assigned(FHead);
end;

function TTokenLinkedList.Pop: TToken;
var
  temp: PTokenNode;
begin
  if IsEmpty then
    raise Exception.Create('Buffer underflow')
  else
  begin
    Result := FHead^.Data;
    temp := FHead;
    FHead := FHead^.Next;
    Dispose(temp);
  end
end;

procedure TTokenLinkedList.Push(Token: TToken);
var
  temp: PTokenNode;
begin
  New(temp);
  temp^.Data := Token;
  temp^.Next := FHead;
  FHead := temp;
end;

procedure TTokenLinkedList.Append(List: TTokenLinkedList);
var
  temp: PTokenNode;
begin
  temp := FHead;
  while (temp <> nil) and (temp^.Next <> nil) do
    temp := temp^.Next;
  if temp <> nil then
  begin
    temp^.Next := List.FHead;
    List.FHead := nil;
  end
  else
    raise Exception.Create('Cannot append to empty list')
end;

function CreateUndoTokenizer(Stream: TStream; Recognizers: TTokenRecognizers): TUndoTokenizer;
begin
  Result := TUndoTokenizer.Create(
    TTokenizer.Create(CreatePushBackCharOrNewLineReader(Stream), Recognizers)
  );
end;

end.
