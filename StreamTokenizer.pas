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
    FPosition: integer;
    FReadBufSize: integer;
    function GetCurrent: char;
    procedure Mark;
    function Token: string;
  public
    constructor Create(InputStream: TStream);
    destructor Destroy; override;

    { Advances the current position by one character }
    procedure Next;

    { Returns the token that has been read so far and prepares the StreamTokenizer
    for reading the following token. }
    function TokenAndMark: string;

    { Returns the next available character without advancing the current position }
    function PeekNext: char;

    { Returns a string that consists of the next characters without advancing
    the current position }
    function PeekLength(Count: integer): string;

    { Gets a value indicating whether all the characters have been read or not }
    function IsEof: boolean;

    { Gets a value indicating whether the next character is an end of line }
    function IsEoln: boolean;

    { Gets a value indicating whether the current position is equal to the
    marked position }
    function IsEmptyToken: boolean;

    { Advances the current position as long as the character is within the
    validChars set. The first character must be within the firstChar set.
    Returns true if the position was advanced, false otherwise. }
    function Scan(firstChar, validChars: TSysCharSet): boolean;

    { Gets the character at the current position of the reader. }
    property Current: char read GetCurrent;

    { Gets the current position in the stream. }
    property Position: integer read FPosition;
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
end;

function TStreamTokenizer.GetCurrent: char;
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

function TStreamTokenizer.Token: string;
var
  tokenLen: integer;
  tokenString: string;
begin
  tokenLen := FCurrent - FMark;
  SetString(tokenString, FMark, tokenLen);
  Token := tokenString;
end;

function TStreamTokenizer.TokenAndMark: string;
begin
  TokenAndMark := Token;
  Mark;
end;

function TStreamTokenizer.PeekNext: char;
begin
  if IsEof then
    PeekNext := #0
  else
    PeekNext := (FCurrent + 1)^;
end;

function TStreamTokenizer.IsEof: boolean;
begin
  IsEof := Current = #0;
end;

function TStreamTokenizer.IsEmptyToken: boolean;
begin
  IsEmptyToken := FCurrent = FMark;
end;

function TStreamTokenizer.IsEoln: boolean;
begin
  IsEoln := Current in [#13, #10];
end;

function TStreamTokenizer.Scan(firstChar: TSysCharSet; validChars: TSysCharSet): boolean;
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

function TStreamTokenizer.PeekLength(Count: integer): string;
var
  buffer: string;
  i: integer;
begin
  buffer := '';
  i := 0;
  while (i < count) and ((FCurrent + i)^ <> #0) do
  begin
    buffer := buffer + (FCurrent + i)^;
    Inc(i);
  end;

  Result := buffer;
end;

end.
