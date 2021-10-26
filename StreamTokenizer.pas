
Unit StreamTokenizer;

{$mode delphi}

Interface

Uses 
Classes, SysUtils;

Type 
  { Parses a stream of characters and returns tokens }
  TStreamTokenizer = Class
    Private 
      FReadBuf: PChar;
      FCurrent: PChar;
      FMark: PChar;
      FPosition: integer;
      FReadBufSize: integer;
      Function GetCurrent: char;
      Procedure Mark;
      Function Token: string;
    Public 
      constructor Create(InputStream: TStream);
      destructor Destroy;
      override;

    { Advances the current position by one character }
      Procedure Next;


{ Returns the token that has been read so far and prepares the StreamTokenizer
    for reading the following token. }
      Function TokenAndMark: string;

    { Returns the next available character without advancing the current position }
      Function PeekNext: char;


{ Returns a string that consists of the next characters without advancing
    the current position }
      Function PeekLength(Count: integer): string;

    { Gets a value indicating whether all the characters have been read or not }
      Function IsEof: boolean;

    { Gets a value indicating whether the next character is an end of line }
      Function IsEoln: boolean;

    { Gets a value indicating whether the current position is equal to the
    marked position }
      Function IsEmptyToken: boolean;


{ Advances the current position as long as the character is within the
    validChars set. The first character must be within the firstChar set.
    Returns true if the position was advanced, false otherwise. }
      Function Scan(firstChar, validChars: TSysCharSet): boolean;

    { Gets the character at the current position of the reader. }
      property Current: char read GetCurrent;

    { Gets the current position in the stream. }
      property Position: integer read FPosition;
  End;

Implementation

constructor TStreamTokenizer.Create(InputStream: TStream);

Var 
  FReadBuf: PChar;
Begin
  GetMem(FReadBuf, InputStream.Size + 1);
  FReadBufSize := InputStream.Read(FReadBuf^, InputStream.Size);
  FReadBuf[FReadBufSize] := #0;
  FCurrent := FReadBuf;
  FPosition := 0;
  Mark;
End;

destructor TStreamTokenizer.Destroy;
Begin
  FreeMem(FReadBuf);
End;

Function TStreamTokenizer.GetCurrent: char;
Begin
  GetCurrent := FCurrent^;
End;

Procedure TStreamTokenizer.Mark;
Begin
  FMark := FCurrent;
End;

Procedure TStreamTokenizer.Next;
Begin
  Inc(FCurrent);
  Inc(FPosition);
End;

Function TStreamTokenizer.Token: string;

Var 
  tokenLen: integer;
  tokenString: string;
Begin
  tokenLen := FCurrent - FMark;
  SetString(tokenString, FMark, tokenLen);
  Token := tokenString;
End;

Function TStreamTokenizer.TokenAndMark: string;
Begin
  TokenAndMark := Token;
  Mark;
End;

Function TStreamTokenizer.PeekNext: char;
Begin
  If IsEof Then
    PeekNext := #0
  Else
    PeekNext := (FCurrent + 1)^;
End;

Function TStreamTokenizer.IsEof: boolean;
Begin
  IsEof := Current = #0;
End;

Function TStreamTokenizer.IsEmptyToken: boolean;
Begin
  IsEmptyToken := FCurrent = FMark;
End;

Function TStreamTokenizer.IsEoln: boolean;
Begin
  IsEoln := Current In [#13, #10];
End;

Function TStreamTokenizer.Scan(firstChar: TSysCharSet; validChars: TSysCharSet): boolean;
Begin
  If Current In firstChar Then
    Begin
      Next;
      While (Not IsEof) And (Current In validChars) Do
        Next;

      Scan := True;
    End
  Else
    Scan := False;
End;

Function TStreamTokenizer.PeekLength(Count: integer): string;

Var 
  buffer: string;
  i: integer;
Begin
  buffer := '';
  i := 0;
  While (i < count) And ((FCurrent + i)^ <> #0) Do
    Begin
      buffer := buffer + (FCurrent + i)^;
      Inc(i);
    End;

  Result := buffer;
End;

End.
