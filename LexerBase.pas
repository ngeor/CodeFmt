
Unit LexerBase;

{$mode delphi}

Interface

Uses 
Classes, SysUtils, StreamTokenizer, TokenTypes;

Type 
  { A callback method that is called when a token is found. }
  TLexerTokenFound = Procedure (Const Token: String; Const TokenType: TTokenType) Of object;

  { Base class for a lexer }
  TLexerBase = Class
    Private 
    { Holds the callback method to call when a token is found }
      FTokenFound: TLexerTokenFound;

    { Holds the stream tokenizer }
      FStreamTokenizer: TStreamTokenizer;
    Protected 
      Procedure CurrentTokenFound(Const TokenType: TTokenType);
      Procedure Scan;
      virtual;

    { Gets the callback method that is called when a token is found }
      property TokenFound: TLexerTokenFound read FTokenFound;

    { Gets the stream tokenizer }
      property StreamTokenizer: TStreamTokenizer read FStreamTokenizer;
    Public 
      constructor Create(TokenFound: TLexerTokenFound);
      Procedure FormatStream(InputStream: TStream);
  End;


{ Handles CRLF tokens. A CRLF sequence is recognized as a single token. Single
CR and single LF tokens are recognized as individual tokens. }
Procedure HandleCRLF(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);

{ Handles whitespace and invisible characters, except CRLF characters. }
Procedure HandleSpace(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);

{ Handles a double slash line comment }
Procedure HandleSlashesComment(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);


{ Handles a single line comment. The comment denoting characters are
identified by the CommentMark parameter. }
Procedure HandleLineComment(
                            StreamTokenizer: TStreamTokenizer;
                            TokenFound: TLexerTokenFound;
                            CommentMark: String);

Implementation

constructor TLexerBase.Create(TokenFound: TLexerTokenFound);
Begin
  FTokenFound := TokenFound;
End;

Procedure TLexerBase.FormatStream(InputStream: TStream);

Var 
  oldPosition: integer;
Begin
  FStreamTokenizer := TStreamTokenizer.Create(InputStream);
  Try
    While Not FStreamTokenizer.IsEof Do
      Begin
      { capture current position of the stream }
        oldPosition := FStreamTokenizer.Position;

      { scan next token }
        Scan;

      { if nothing was scanned... }
        If oldPosition = FStreamTokenizer.Position Then
          Begin
        (* unexpected token, read one char and print it out immediately *)
            FStreamTokenizer.Next;
            CurrentTokenFound(ttUnknown);
          End;
      End;
  Finally
    FStreamTokenizer.Free;
End;
End;

Procedure TLexerBase.CurrentTokenFound(Const TokenType: TTokenType);
Begin
  TokenFound(FStreamTokenizer.TokenAndMark, TokenType);
End;

Procedure TLexerBase.Scan;
Begin

End;

Procedure HandleCRLF(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);
Begin
  If (StreamTokenizer.Current = #13) And (StreamTokenizer.PeekNext = #10) Then
    Begin
      StreamTokenizer.Next;
      StreamTokenizer.Next;
      TokenFound(StreamTokenizer.TokenAndMark, ttCRLF);
    End
  Else If (StreamTokenizer.Current In [#13, #10]) Then
         Begin
           StreamTokenizer.Next;
           TokenFound(StreamTokenizer.TokenAndMark, ttCRLF);
         End;
End;

Procedure HandleSpace(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);
Begin
  If StreamTokenizer.Scan([#1..#9, #11, #12, #14..#32], [#1..#9, #11, #12, #14..#32]) Then
    TokenFound(StreamTokenizer.TokenAndMark, ttSpace);
End;

Procedure HandleSlashesComment(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);
Begin
  HandleLineComment(StreamTokenizer, TokenFound, '//');
End;

Procedure HandleLineComment(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound;
                            CommentMark: String);
Begin
  If StreamTokenizer.PeekLength(Length(CommentMark)) = CommentMark Then
    Begin
      While (Not StreamTokenizer.IsEof) And (Not StreamTokenizer.IsEoln) Do
        StreamTokenizer.Next;

      TokenFound(StreamTokenizer.TokenAndMark, ttComment);
    End;
End;

End.
