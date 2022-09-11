unit LexerBase;

{$mode delphi}

interface

uses
  Classes, SysUtils, StreamTokenizer, TokenTypes;

type
  { A callback method that is called when a token is found. }
  TLexerTokenFound = procedure(const Token: String;
    const TokenType: THigherTokenType) of object;

  { Base class for a lexer }
  TLexerBase = class
  private
    { Holds the callback method to call when a token is found }
    FTokenFound: TLexerTokenFound;
  protected
    { Gets the callback method that is called when a token is found }
    property TokenFound: TLexerTokenFound read FTokenFound;
  public
    constructor Create(TokenFound: TLexerTokenFound);
    procedure FormatStream(InputStream: TStream); virtual; abstract;
  end;

  TOldLexerBase = class(TLexerBase)
  private
    { Holds the stream tokenizer }
    FStreamTokenizer: TStreamTokenizer;
  protected
    procedure CurrentTokenFound(const TokenType: THigherTokenType);
    procedure Scan; virtual;

    { Gets the stream tokenizer }
    property StreamTokenizer: TStreamTokenizer read FStreamTokenizer;
  public
    procedure FormatStream(InputStream: TStream); override;
  end;

{ Handles CRLF tokens. A CRLF sequence is recognized as a single token. Single
CR and single LF tokens are recognized as individual tokens. }
procedure HandleCRLF(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);

{ Handles whitespace and invisible characters, except CRLF characters. }
procedure HandleSpace(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);

{ Handles a double slash line comment }
procedure HandleSlashesComment(StreamTokenizer: TStreamTokenizer;
  TokenFound: TLexerTokenFound);

{ Handles a single line comment. The comment denoting characters are
identified by the CommentMark parameter. }
procedure HandleLineComment(StreamTokenizer: TStreamTokenizer;
  TokenFound: TLexerTokenFound; CommentMark: String);

implementation

constructor TLexerBase.Create(TokenFound: TLexerTokenFound);
begin
  FTokenFound := TokenFound;
end;

procedure TOldLexerBase.FormatStream(InputStream: TStream);
var
  oldPosition: Integer;
begin
  FStreamTokenizer := TStreamTokenizer.Create(InputStream);
  try
    while not FStreamTokenizer.IsEof do
    begin
      { capture current position of the stream }
      oldPosition := FStreamTokenizer.Position;

      { scan next token }
      Scan;

      { if nothing was scanned... }
      if oldPosition = FStreamTokenizer.Position then
      begin
        (* unexpected token, read one char and print it out immediately *)
        FStreamTokenizer.Next;
        CurrentTokenFound(htUnknown);
      end;
    end;
  finally
    FStreamTokenizer.Free;
  end;
end;

procedure TOldLexerBase.CurrentTokenFound(const TokenType: THigherTokenType);
begin
  TokenFound(FStreamTokenizer.TokenAndMark, TokenType);
end;

procedure TOldLexerBase.Scan;
begin

end;

procedure HandleCRLF(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);
begin
  if (StreamTokenizer.Current = #13) and (StreamTokenizer.PeekNext = #10) then
  begin
    StreamTokenizer.Next;
    StreamTokenizer.Next;
    TokenFound(StreamTokenizer.TokenAndMark, htCRLF);
  end
  else if (StreamTokenizer.Current in [#13, #10]) then
  begin
    StreamTokenizer.Next;
    TokenFound(StreamTokenizer.TokenAndMark, htCRLF);
  end;
end;

procedure HandleSpace(StreamTokenizer: TStreamTokenizer; TokenFound: TLexerTokenFound);
begin
  if StreamTokenizer.Scan([#1..#9, #11, #12, #14..#32],
    [#1..#9, #11, #12, #14..#32]) then
    TokenFound(StreamTokenizer.TokenAndMark, htSpace);
end;

procedure HandleSlashesComment(StreamTokenizer: TStreamTokenizer;
  TokenFound: TLexerTokenFound);
begin
  HandleLineComment(StreamTokenizer, TokenFound, '//');
end;

procedure HandleLineComment(StreamTokenizer: TStreamTokenizer;
  TokenFound: TLexerTokenFound; CommentMark: String);
begin
  if StreamTokenizer.PeekLength(Length(CommentMark)) = CommentMark then
  begin
    while (not StreamTokenizer.IsEof) and (not StreamTokenizer.IsEoln) do
      StreamTokenizer.Next;

    TokenFound(StreamTokenizer.TokenAndMark, htComment);
  end;
end;

end.
