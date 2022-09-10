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

procedure TestParser;

implementation

uses Readers, Tokenizers;

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

{
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
  // keep a copy
  OriginalSource := Source.Copy;
  NextResult := FParser.Parse(Source);
  if NextResult.Success then
  begin
    // the inner parser has freed `Source` at this point
    NextResult.Success := FPredicate(NextResult.Data);
    if NextResult.Success then
    begin
      OriginalSource.Free;
      Result := NextResult;
    end
    else
    begin
      // Need to rollback to the original parse source.
      // Source is not available because the inner parser has freed it.
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

(* editor config line comment parser *)

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
}

{ Sample implementation for TTokenizer }

type
  TSampleTokenKind = (stkDigits, stkLetters, stkOther);

function IsDigit(Ch: Char): Boolean;
begin
  Result := (Ch >= '0') and (Ch <= '9');
end;

function IsLetter(Ch: Char): Boolean;
begin
  Result := ((Ch >= 'a') and (Ch <= 'z')) or ((Ch >= 'A') and (Ch <= 'Z'));
end;

type
  TCharPredicate = function(Ch: Char): Boolean;
  TPredicateRecognizer = class(TTokenRecognizer)
  private
    FPredicate: TCharPredicate;
  public
    constructor Create(Predicate: TCharPredicate);
    destructor Destroy; override;
    function Recognize(Buffer: TLineReads): Boolean; override;
  end;

constructor TPredicateRecognizer.Create(Predicate: TCharPredicate);
begin
  FPredicate := Predicate;
end;

destructor TPredicateRecognizer.Destroy;
begin
  inherited Destroy;
end;

function TPredicateRecognizer.Recognize(Buffer: TLineReads): Boolean;
var
  i: Integer;
begin
  i := Low(Buffer);
  while (i <= High(Buffer)) and (Buffer[i].HasValue) and (not Buffer[i].IsEol) and (FPredicate(Buffer[i].Value)) do
    Inc(i);
  Result := i > High(Buffer);
end;

type
  TAnyRecognizer = class(TTokenRecognizer)
  private
  public
    constructor Create;
    destructor Destroy; override;
    function Recognize(Buffer: TLineReads): Boolean; override;
  end;

constructor TAnyRecognizer.Create;
begin
end;

destructor TAnyRecognizer.Destroy;
begin
  inherited Destroy;
end;

function TAnyRecognizer.Recognize(Buffer: TLineReads): Boolean;
begin
  // recognize any single "LineRead"
  Result := Length(Buffer) = 1;
end;

function CreateDigitsRecognizer(): TTokenRecognizer;
begin
  Result := TPredicateRecognizer.Create(IsDigit);
end;

function CreateLettersRecognizer(): TTokenRecognizer;
begin
  Result := TPredicateRecognizer.Create(IsLetter);
end;

function CreateSampleTokenizer(LineReader: TPushBackLineReader): TTokenizer;
begin
  Result := TTokenizer.Create(
    LineReader,
    // order must match TSampleTokenKind enum
    [CreateDigitsRecognizer, CreateLettersRecognizer, TAnyRecognizer.Create]
  );
end;

// parse this text: abc 123 def 456

type
  TSimplePair = record
    Letters: String;
    Numbers: Integer;
  end;
  TSimplePairs = array of TSimplePair;

type
  TParseResult<T> = record
    Success: Boolean;
    Data: T;
  end;

  TParser<T> = class
  public
    function Parse(Source: TUndoTokenizer): TParseResult<T>; virtual; abstract;
    procedure Undo(Source: TUndoTokenizer; Data: T); virtual; abstract;
  end;

type
  TTokenKindParser = class(TParser<TToken>)
  private
    FTokenKind: Word;
  public
    constructor Create(TokenKind: Word);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<TToken>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TToken); override;
  end;

constructor TTokenKindParser.Create(TokenKind: Word);
begin
  FTokenKind := TokenKind;
end;

destructor TTokenKindParser.Destroy;
begin
  inherited Destroy;
end;

function TTokenKindParser.Parse(Source: TUndoTokenizer): TParseResult<TToken>;
var
  Token: TToken;
begin
  Token := Source.Read;
  if Token.Kind = FTokenKind then
  begin
    Result.Success := True;
    Result.Data := Token;
  end
  else
    Source.Undo(Token);
    Result.Success := False;
  begin
  end
end;

procedure TTokenKindParser.Undo(Source: TUndoTokenizer; Data: TToken);
begin
  Source.Undo(Data);
end;


type
  TPair<L, R> = record
    Left: L;
    Right: R;
  end;

  TAndParser<L, R> = class(TParser<TPair<L, R>>)
  private
    FLeft: TParser<L>;
    FRight: TParser<R>;
  public
    constructor Create(Left: TParser<L>; Right: TParser<R>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<TPair<L, R>>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TPair<L, R>); override;
  end;

constructor TAndParser<L, R>.Create(Left: TParser<L>; Right: TParser<R>);
begin
  FLeft := Left;
  FRight := Right;
end;

destructor TAndParser<L, R>.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited Destroy;
end;

function TAndParser<L, R>.Parse(Source: TUndoTokenizer): TParseResult<TPair<L, R>>;
var
  Left: TParseResult<L>;
  Right: TParseResult<R>;
begin
  Left := FLeft.Parse(Source);
  if Left.Success then
  begin
    Right := FRight.Parse(Source);
    if Right.Success then
    begin
      Result.Success := True;
      Result.Data.Left := Left.Data;
      Result.Data.Right := Right.Data;
    end
    else
    begin
      // undo left result !!!
      FLeft.Undo(Source, Left.Data);
      Result.Success := False;
    end
  end
  else
  begin
    Result.Success := False;
  end
end;

procedure TAndParser<L, R>.Undo(Source: TUndoTokenizer; Data: TPair<L, R>);
begin
  FRight.Undo(Source, Data.Right);
  FLeft.Undo(Source, Data.Left);
end;

function Seq<L, R>(Left: TParser<L>; Right: TParser<R>): TAndParser<L, R>;
begin
  Result := TAndParser<L, R>.Create(Left, Right);
end;





type
  TMapper<T, U> = function(x : T): U;
  TMapParser<T, U> = class(TParser<U>)
  private
    FParser: TParser<T>;
    FMapper: TMapper<T, U>;
  public
    constructor Create(Parser: TParser<T>; Mapper: TMapper<T, U>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<U>; override;
    procedure Undo(Source: TUndoTokenizer; Data: U); override;
  end;

constructor TMapParser<T, U>.Create(Parser: TParser<T>; Mapper: TMapper<T, U>);
begin
  FParser := Parser;
  FMapper := Mapper;
end;

destructor TMapParser<T, U>.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TMapParser<T, U>.Parse(Source: TUndoTokenizer): TParseResult<U>;
var
  temp: TParseResult<T>;
begin
  temp := FParser.Parse(Source);
  if temp.Success then
  begin
    Result.Success := True;
    Result.Data := FMapper(temp.Data);
  end
  else
    Result.Success := False;
end;

procedure TMapParser<T, U>.Undo(Source: TUndoTokenizer; Data: U);
begin
  raise Exception.Create('not possible');
end;





type
  TTokenPair = TPair<TToken, TToken>;

function Apply1(Pair: TTokenPair): TSimplePair;
begin
  Result.Letters := Pair.Left.Data;
  Result.Numbers := StrToInt(Pair.Right.Data);
end;

function CreateSimplePairParser: TParser<TSimplePair>;
begin
  Result := TMapParser<TTokenPair, TSimplePair>.Create(
    Seq<TToken, TToken>(
      TTokenKindParser.Create(Ord(stkLetters)),
      TTokenKindParser.Create(Ord(stkDigits))
    ),
    Apply1
  );
end;






type

  TManyParser<T: record> = class(TParser<TList>)
  private
    FParser: TParser<T>;
  public
    constructor Create(Parser: TParser<T>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<TList>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TList); override;
  end;

constructor TManyParser<T: record>.Create(Parser: TParser<T>);
begin
  FParser := Parser;
end;

destructor TManyParser<T: record>.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TManyParser<T: record>.Parse(Source: TUndoTokenizer): TParseResult<TList>;
var
  List: TList;
  Item: TParseResult<T>;
  P: ^T;
begin
  List := TList.Create;
  repeat
    Item := FParser.Parse(Source);
    if Item.Success then
    begin
      New(P);
      P^ := Item.Data;
      List.Add(P);
    end;
  until not Item.Success;
  if List.Count > 0 then
  begin
    Result.Success := True;
    Result.Data := List;
  end
  else
  begin
    Result.Success := False;
    Result.Data := nil;
    List.Free;
  end
end;

procedure TManyParser<T: record>.Undo(Source: TUndoTokenizer; Data: TList);
var
  i: Integer;
  P: ^T;
begin
  i := Data.Count - 1;
  while i >= 0 do
  begin
    P := Data[i];
    FParser.Undo(Source, P^);
    Dispose(P);
    Dec(i);
  end;
  Data.Free;
end;

function Apply2(List: TList): TSimplePairs;
var
  i : Integer;
  p: ^TSimplePair;
begin
  SetLength(Result, List.Count);
  for i := 0 to List.Count - 1 do
  begin
    p := List[i];
    Result[i] := p^;
    Dispose(p);
  end;
  List.Free;
end;


function CreateSimpleParser: TParser<TSimplePairs>;
var
  ItemParser: TParser<TSimplePair>;
  ManyParser: TParser<TList>;
begin
  ItemParser := CreateSimplePairParser;
  ManyParser := TManyParser<TSimplePair>.Create(ItemParser);
  Result := TMapParser<TList, TSimplePairs>.Create(ManyParser, Apply2);
end;



procedure TestParser;
var
  Parser: TParser<TSimplePairs>;
  Result: TParseResult<TSimplePairs>;
  Pairs: TSimplePairs;
begin
  Parser := CreateSimpleParser;
  Result := Parser.Parse(
    TUndoTokenizer.Create(
      CreateSampleTokenizer(
        TPushBackLineReader.Create(
          TLineReader.Create(
            TPushBackReader.Create(
              TStreamReader.Create(
                TStringStream.Create('abc123def456')
              )
            )
          )
        )
      )
    )
  );
  Pairs := Result.Data;
end;

end.
