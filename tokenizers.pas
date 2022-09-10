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
    function Recognize(Buffer: TCharOrNewLineArray): Integer;
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
  end;

  TUndoTokenizer = class
  private
    FTokenizer: TTokenizer;
    FBuffer: TTokenLinkedList;
  public
    constructor Create(Tokenizer: TTokenizer);
    destructor Destroy; override;
    function Read: TToken;
    procedure Undo(Token: TToken);
  end;

function CreateUndoTokenizer(Stream: TStream; Recognizers: TTokenRecognizers): TUndoTokenizer;

implementation

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
  RecognizerIndex: Integer;
  NextRecognizerIndex: Integer;
  NoMatchOrEof: Boolean;
  i: Integer;
begin
  Size := 0;
  RecognizerIndex := -1;
  NoMatchOrEof := False;
  repeat
    Next := FReader.Read;
    if Next.Kind <> ckEof then
    begin
      { add read character to buffer}
      Inc(Size);
      SetLength(Buffer, Size);
      Buffer[Size - 1] := Next;

      { find which recognizer can work with the buffer, if any }
      NextRecognizerIndex := Recognize(Buffer);
      if NextRecognizerIndex = -1 then
      begin
        NoMatchOrEof := True;
        FReader.UnRead(Next);
        Dec(Size);
        SetLength(Buffer, Size);
      end
      else
      begin
        RecognizerIndex := NextRecognizerIndex;
      end
    end
    else
    begin
      // EOF
      NoMatchOrEof := True;
    end
  until NoMatchOrEof;
  Result.Kind := RecognizerIndex;
  if RecognizerIndex >= 0 then
  begin
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

function TTokenizer.Recognize(Buffer: TCharOrNewLineArray): Integer;
var
  i: Integer;
begin
  i := Low(FRecognizers);
  while (i <= High(FRecognizers)) and (not FRecognizers[i].Recognize(Buffer)) do
    Inc(i);
  if i <= High(FRecognizers) then
    Result := i
  else
    Result := -1
end;

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

function CreateUndoTokenizer(Stream: TStream; Recognizers: TTokenRecognizers): TUndoTokenizer;
begin
  Result := TUndoTokenizer.Create(
    TTokenizer.Create(CreatePushBackCharOrNewLineReader(Stream), Recognizers)
  );
end;

end.
