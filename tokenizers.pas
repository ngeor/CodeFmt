unit Tokenizers;

{$mode Delphi}

interface

uses CharOrNewLineReaders;

type
  TRowCol = record
    Row: Word;
    Col: Word;
  end;

  TTokenPosition = record
    Start: TRowCol;
    Finish: TRowCol;
  end;

  TToken = record
    Data: String;
    Kind: Integer;
    Position: TTokenPosition;
  end;

  TCharOrNewLineArray = array of TCharOrNewLine;

  TTokenRecognizer = class
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; virtual; abstract;
  end;

  TTokenRecognizers = array of TTokenRecognizer;

  TTokenizer = class
  private
    FLineReader: TPushBackCharOrNewLineReader;
    FRecognizers: TTokenRecognizers;
    FRowCol: TRowCol;
    function Recognize(Buffer: TCharOrNewLineArray): Integer;
  public
    constructor Create(LineReader: TPushBackCharOrNewLineReader; Recognizers: TTokenRecognizers);
    destructor Destroy; override;
    function Read: TToken;
  end;

  PTokenNode = ^TTokenNode;
  TTokenNode = record
    Data: TToken;
    Next: PTokenNode;
  end;

  TUndoTokenizer = class
  private
    FTokenizer: TTokenizer;
    FBuffer: PTokenNode;
  public
    constructor Create(Tokenizer: TTokenizer);
    destructor Destroy; override;
    function Read: TToken;
    procedure Undo(Token: TToken);
  end;

implementation

constructor TTokenizer.Create(LineReader: TPushBackCharOrNewLineReader; Recognizers: TTokenRecognizers);
begin
  FLineReader := LineReader;
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
  FLineReader.Free;
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
    Next := FLineReader.Read;
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
        FLineReader.UnRead(Next);
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
    Result.Data := '';
    for i := Low(Buffer) To High(Buffer) do
    begin
      Next := Buffer[i];
      if Next.Kind = ckEol then
      begin
        Inc(FRowCol.Row);
        FRowCol.Col := 1;
        case Next.NewLineKind of
          nlCR: Result.Data := Result.Data + '\r';
          nlLF: Result.Data := Result.Data + '\n';
          nlCRLF: Result.Data := Result.Data + '\r\n';
        end;
      end
      else
      begin
        Inc(FRowCol.Col);
        Result.Data := Result.Data + Next.Ch;
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
  FBuffer := nil;
end;

destructor TUndoTokenizer.Destroy;
var
  temp: PTokenNode;
begin
  FTokenizer.Free;
  while Assigned(FBuffer) do
  begin
    temp := FBuffer;
    FBuffer := FBuffer^.Next;
    Dispose(temp);
  end;
  inherited Destroy;
end;

function TUndoTokenizer.Read: TToken;
var
  temp: PTokenNode;
begin
  if Assigned(FBuffer) then
  begin
    Result := FBuffer^.Data;
    temp := FBuffer;
    FBuffer := FBuffer^.Next;
    Dispose(temp);
  end
  else
    Result := FTokenizer.Read;
end;

procedure TUndoTokenizer.Undo(Token: TToken);
var
  temp: PTokenNode;
begin
  New(temp);
  temp^.Data := Token;
  temp^.Next := FBuffer;
  FBuffer := temp;
end;

end.
