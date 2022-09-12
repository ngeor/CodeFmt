unit LexerBase;

{$mode delphi}

interface

uses
  Classes, SysUtils, TokenTypes;

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

implementation

constructor TLexerBase.Create(TokenFound: TLexerTokenFound);
begin
  FTokenFound := TokenFound;
end;

end.
