unit StreamTokenizer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  { Parses a stream of characters and returns tokens }
  TStreamTokenizer = class
  private
    FReadBuf: PChar;
    FCurrent: PChar;
    FMark: PChar;
    FPosition: Integer;
    FReadBufSize: Integer;
    function GetCurrent: Char;
    procedure Mark;
    function Token: String;
  public
    constructor Create(InputStream: TStream);
    destructor Destroy; override;

    { Advances the current position by one character }
    procedure Next;

    { Returns the token that has been read so far and prepares the StreamTokenizer
    for reading the following token. }
    function TokenAndMark: String;

    { Returns the next available character without advancing the current position }
    function PeekNext: Char;

    { Returns a string that consists of the next characters without advancing
    the current position }
    function PeekLength(Count: Integer): String;

    { Gets a value indicating whether all the characters have been read or not }
    function IsEof: Boolean;

    { Gets a value indicating whether the next character is an end of line }
    function IsEoln: Boolean;

    { Gets a value indicating whether the current position is equal to the
    marked position }
    function IsEmptyToken: Boolean;

    { Advances the current position as long as the character is within the
    validChars set. The first character must be within the firstChar set.
    Returns true if the position was advanced, false otherwise. }
    function Scan(firstChar, validChars: TSysCharSet): Boolean;

    { Gets the character at the current position of the reader. }
    property Current: Char read GetCurrent;

    { Gets the current position in the stream. }
    property Position: Integer read FPosition;
  end;

implementation

uses Readers;

constructor TStreamTokenizer.Create(InputStream: TStream);
var
  FReadBuf: PChar;
begin
  GetMem(FReadBuf, InputStream.Size + 1);
  FReadBufSize := InputStream.Read(FReadBuf^, InputStream.Size);
  FReadBuf[FReadBufSize] := #0;
  FCurrent := FReadBuf;
  FPosition := 0;
  Mark;
end;

destructor TStreamTokenizer.Destroy;
begin
  FreeMem(FReadBuf);
  inherited Destroy;
end;

function TStreamTokenizer.GetCurrent: Char;
begin
  GetCurrent := FCurrent^;
end;

procedure TStreamTokenizer.Mark;
begin
  FMark := FCurrent;
end;

procedure TStreamTokenizer.Next;
begin
  Inc(FCurrent);
  Inc(FPosition);
end;

function TStreamTokenizer.Token: String;
var
  tokenLen: Integer;
  tokenString: String;
begin
  tokenLen := FCurrent - FMark;
  SetString(tokenString, FMark, tokenLen);
  Token := tokenString;
end;

function TStreamTokenizer.TokenAndMark: String;
begin
  TokenAndMark := Token;
  Mark;
end;

function TStreamTokenizer.PeekNext: Char;
begin
  if IsEof then
    PeekNext := #0
  else
    PeekNext := (FCurrent + 1)^;
end;

function TStreamTokenizer.IsEof: Boolean;
begin
  IsEof := Current = #0;
end;

function TStreamTokenizer.IsEmptyToken: Boolean;
begin
  IsEmptyToken := FCurrent = FMark;
end;

function TStreamTokenizer.IsEoln: Boolean;
begin
  IsEoln := Current in [#13, #10];
end;

function TStreamTokenizer.Scan(firstChar: TSysCharSet; validChars: TSysCharSet): Boolean;
begin
  if Current in firstChar then
  begin
    Next;
    while (not IsEof) and (Current in validChars) do
      Next;

    Scan := True;
  end
  else
    Scan := False;
end;

function TStreamTokenizer.PeekLength(Count: Integer): String;
var
  buffer: String;
  i: Integer;
begin
  buffer := '';
  i := 0;
  while (i < Count) and ((FCurrent + i)^ <> #0) do
  begin
    buffer := buffer + (FCurrent + i)^;
    Inc(i);
  end;

  Result := buffer;
end;


type
  TParseSource = class
  private
    FData: String;
    FPosition: Integer;
    constructor Create(Data: String; Position: Integer); overload;
  public
    constructor Create(Data: String); overload;
    function IsEof: Boolean;
    function CurrentChar: Char;
    function Move: TParseSource;
    function Copy: TParseSource;
  end;

constructor TParseSource.Create(Data: String; Position: Integer);
begin
  FData := Data;
  FPosition := Position;
end;

constructor TParseSource.Create(Data: String);
begin
  FData := Data;
  FPosition := 0;
end;

function TParseSource.IsEof: Boolean;
begin
  Result := FPosition >= Length(FData);
end;

function TParseSource.CurrentChar: Char;
begin
  Result := FData[FPosition];
end;

function TParseSource.Move: TParseSource;
begin
  Result := TParseSource.Create(FData, FPosition + 1);
end;

function TParseSource.Copy: TParseSource;
begin
  Result := TParseSource.Create(FData, FPosition);
end;

type
  TParseResult<T> = record
    Success: Boolean;
    Data: T;
    Remaining: TParseSource;
  end;

  TParser<T> = class
  public
    function Parse(Source: TParseSource): TParseResult<T>; virtual; abstract;
  end;

type
  TCharParser = class(TParser<Char>)
  public
    function Parse(Source: TParseSource): TParseResult<Char>; override;
  end;

function TCharParser.Parse(Source: TParseSource): TParseResult<Char>;
begin
  if Source.IsEof then
  begin
    Result.Success := False;
    Result.Remaining := Source;
  end
  else
  begin
    Result.Success := True;
    Result.Data := Source.CurrentChar;
    Result.Remaining := Source.Move;
    Source.Free;
  end;
end;

type
  TCharPredicate = function(Ch: Char): Boolean;

  TIfParser = class(TParser<Char>)
  private
    FParser: TParser<Char>;
    FPredicate: TCharPredicate;
  public
    constructor Create(Parser: TParser<Char>; Predicate: TCharPredicate);
    function Parse(Source: TParseSource): TParseResult<Char>; override;
  end;

constructor TIfParser.Create(Parser: TParser<Char>; Predicate: TCharPredicate);
begin
  FParser := Parser;
  FPredicate := Predicate;
end;

function TIfParser.Parse(Source: TParseSource): TParseResult<Char>;
var
  NextResult: TParseResult<Char>;
  OriginalSource: TParseSource;
begin
  { keep a copy }
  OriginalSource := Source.Copy;
  NextResult := FParser.Parse(Source);
  if NextResult.Success then
  begin
    { the inner parser has freed `Source` at this point }
    NextResult.Success := FPredicate(NextResult.Data);
    if NextResult.Success then
    begin
      OriginalSource.Free;
      Result := NextResult;
    end
    else
    begin
         { Need to rollback to the original parse source.
         Source is not available because the inner parser has freed it.
         }
      Result := NextResult;
      Result.Remaining := OriginalSource;
      NextResult.Remaining.Free;
    end;
  end
  else
  begin
    OriginalSource.Free;
    Result := NextResult;
  end;
end;

type
  TManyParser = class(TParser<String>)
  private
    FParser: TParser<Char>;
  public
    constructor Create(Parser: TParser<Char>);
    function Parse(Source: TParseSource): TParseResult<String>; override;
  end;

constructor TManyParser.Create(Parser: TParser<Char>);
begin
  FParser := Parser;
end;

function TManyParser.Parse(Source: TParseSource): TParseResult<String>;
var
  Buffer: String;
  ChildResult: TParseResult<Char>;
  CurrentSource: TParseSource;
begin
  Buffer := '';
  CurrentSource := Source;
  repeat
    ChildResult := FParser.Parse(CurrentSource);
    if ChildResult.Success then
    begin
      Buffer := Buffer + ChildResult.Data;
      CurrentSource := ChildResult.Remaining;
    end;
  until not ChildResult.Success;
  with Result do
  begin
    Success := Length(Buffer) > 0;
    Data := Buffer;
    Remaining := CurrentSource;
  end;
end;

function IsWhiteSpace(ch: Char): Boolean;
begin
  Result := ch = ' ';
end;

function WhiteSpaceParser: TParser<String>;
begin
  Result := TManyParser.Create(TIfParser.Create(TCharParser.Create, IsWhiteSpace));
end;

{ editor config line comment parser }

function IsEol(ch: Char): Boolean;
begin
  Result := (ch = '\r') or (ch = '\n');
end;

function IsNotEol(ch: Char): Boolean;
begin
  Result := not IsEol(ch);
end;

function UntilEolParser: TParser<String>;
begin
  Result := TManyParser.Create(TIfParser.Create(TCharParser.Create, IsNotEol));
end;

function IsPound(ch: Char): Boolean;
begin
  Result := ch = '#';
end;

function PoundParser: TParser<Char>;
begin
  Result := TIfParser.Create(TCharParser.Create, IsPound);
end;

type
  TCharToStringParser = class(TParser<String>)
  private
    FParser: TParser<Char>;
  public
    constructor Create(Parser: TParser<Char>);
    function Parse(Source: TParseSource): TParseResult<String>; override;
  end;

constructor TCharToStringParser.Create(Parser: TParser<Char>);
begin
  FParser := Parser;
end;

function TCharToStringParser.Parse(Source: TParseSource): TParseResult<String>;
var
  CharResult: TParseResult<Char>;
begin
  CharResult := FParser.Parse(Source);
  Result.Success := CharResult.Success;
  Result.Data := CharResult.Data;
  Result.Remaining := CharResult.Remaining;
end;

type
  TAndParser = class(TParser<String>)
  private
    FLeft: TParser<String>;
    FRight: TParser<String>;
  public
    constructor Create(Left: TParser<String>; Right: TParser<String>);
    function Parse(Source: TParseSource): TParseResult<String>; override;
  end;

constructor TAndParser.Create(Left: TParser<String>; Right: TParser<String>);
begin
  FLeft := Left;
  FRight := Right;
end;

function TAndParser.Parse(Source: TParseSource): TParseResult<String>;
var
  OriginalSource: TParseSource;
  FirstResult: TParseResult<String>;
  SecondResult: TParseResult<String>;
begin
  OriginalSource := Source.Copy;
  FirstResult := FLeft.Parse(Source);
  if FirstResult.Success then
  begin
    SecondResult := FRight.Parse(FirstResult.Remaining);
    if SecondResult.Success then
    begin
      OriginalSource.Free;
      Result := SecondResult;
      Result.Data := FirstResult.Data + SecondResult.Data;
    end
    else
    begin
      Result.Success := False;
      Result.Remaining := OriginalSource;
      SecondResult.Remaining.Free;
    end;
  end
  else
  begin
    OriginalSource.Free;
    Result := FirstResult;
  end;
end;

function EditorConfigComment: TParser<String>;
begin
  Result := TAndParser.Create(TCharToStringParser.Create(PoundParser),
    UntilEolParser);
end;


type
  TPosition = record
    Row: Integer;
    Col: Integer;
  end;

  TToken = record
    Data: String;
    Kind: Integer;
    Position: TPosition;
  end;

type
  TTokenRecognizer = function(Buffer: TLineReads): Boolean;
  TTokenRecognizers = array of TTokenRecognizer;

  TTokenizer = class
  private
    FLineReader: TLineReader;
    FRecognizers: TTokenRecognizers;
    function Recognize(Buffer: TLineReads): Integer;
  public
    constructor Create(LineReader: TLineReader; Recognizers: TTokenRecognizers);
    destructor Destroy; override;
    function Read: TToken;
  end;

constructor TTokenizer.Create(LineReader: TLineReader; Recognizers: TTokenRecognizers);
begin
  FLineReader := LineReader;
  FRecognizers := Recognizers;
end;

destructor TTokenizer.Destroy;
begin
  FLineReader.Free;
  inherited Destroy;
end;

function TTokenizer.Read: TToken;
var
  Buffer: TLineReads;
  Next: TLineRead;
  Size: Integer;
  RecognizerIndex: Integer;
  NextRecognizerIndex: Integer;
  NoMatchOrEof: Boolean;
begin
  Size := 0;
  RecognizerIndex := -1;
  NoMatchOrEof := False;
  repeat
    Next := FLineReader.Read;
    if Next.HasValue then
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
        // TODO: push back Next into stream
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
  // TODO capture rest of Result fields
end;

function TTokenizer.Recognize(Buffer: TLineReads): Integer;
var
  i: Integer;
begin
  i := Low(FRecognizers);
  while (i <= High(FRecognizers)) and (not FRecognizers[i](Buffer)) do
    Inc(i);
  if i <= High(FRecognizers) then
    Result := i
  else
    Result := -1
end;

{ Sample implementation for TTokenizer }

type
  TSampleTokenKind = (stkDigits, stkLetters, stkOther);

function IsDigit(Ch: Char): Boolean; overload;
begin
  Result := (Ch >= '0') and (Ch <= '9');
end;

function IsDigit1(Buffer: TLineReads): Boolean; overload;
var
  i: Integer;
begin
  i := Low(Buffer);
  while (i <= High(Buffer)) and (Buffer[i].IsChar(IsDigit)) do
    Inc(i);
  Result := i > High(Buffer);
end;

function CreateSampleTokenizer(LineReader: TLineReader): TTokenizer;
begin
  Result := TTokenizer.Create(
    LineReader,
    [IsDigit1]
  );
end;

end.
