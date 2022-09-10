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

end.
