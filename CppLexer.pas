unit CppLexer;

{$MODE Delphi}

interface

uses SysUtils, Classes, LexerBase;

type
  TCppLexer = class(TOldLexerBase)
  private
    procedure HandleString;
    procedure HandleIdentifier;
    procedure HandlePreProcessorDirective;
    procedure HandleSymbol;
  protected
    procedure Scan; override;
  end;

implementation

uses TokenTypes;

const
  CppKeyWords: array [0..59] of String =
    ('_cs', '_ds', '_es', '_export', '_fastcall',
    '_loadds', '_saveregs', '_seg', '_ss', 'asm',
    'auto', 'break', 'case', 'cdecl', 'char',
    'class', 'const', 'continue', 'default', 'delete',
    'do', 'double', 'else', 'enum', 'extern',
    'far', 'float', 'for', 'friend', 'goto',
    'huge', 'if', 'inline', 'int', 'interrupt',
    'long', 'near', 'new', 'operator', 'pascal',
    'private', 'protected', 'public', 'register', 'return',
    'short', 'signed', 'sizeof', 'static', 'struct',
    'switch', 'template', 'this', 'typedef', 'union',
    'unsigned', 'virtual', 'void', 'volatile', 'while');

procedure TCppLexer.Scan;
begin
  HandleCRLF(StreamTokenizer, TokenFound);
  HandleSpace(StreamTokenizer, TokenFound);
  HandleSlashesComment(StreamTokenizer, TokenFound);
  HandleString;
  HandleIdentifier;
  HandlePreProcessorDirective;
  HandleSymbol;
end;

procedure TCppLexer.HandleString;
begin
  if StreamTokenizer.Current = '"' then
  begin
    StreamTokenizer.Next;
    while (not StreamTokenizer.IsEof) and (StreamTokenizer.Current <> '"') do
      StreamTokenizer.Next;

    StreamTokenizer.Next;
    CurrentTokenFound(htString);
  end;
end;

function ArrayContains(hay: array of String; needle: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := Low(hay) to High(hay) do
    if hay[i] = needle then
    begin
      Result := True;
      break;
    end;
end;

procedure TCppLexer.HandleIdentifier;
var
  token: String;
  tokenType: THigherTokenType;
begin
  if StreamTokenizer.Scan(['a'..'z', 'A'..'Z'], ['a'..'z', 'A'..'Z', '_']) then
  begin
    token := StreamTokenizer.TokenAndMark;

    if ArrayContains(CppKeyWords, token) then
      tokenType := htKeyWord
    else
      tokenType := htIdentifier;

    TokenFound(token, tokenType);
  end;
end;

procedure TCppLexer.HandlePreProcessorDirective;
begin
  if StreamTokenizer.Current = '#' then
  begin
    while (not StreamTokenizer.IsEof) and (not StreamTokenizer.IsEoln) do
      StreamTokenizer.Next;

    CurrentTokenFound(htPreProcessor);
  end;
end;

procedure TCppLexer.HandleSymbol;
begin
  if StreamTokenizer.Current in ['(', ')', ';', '{', '}', '[', ']'] then
  begin
    StreamTokenizer.Next;
    CurrentTokenFound(htSymbol);
  end;
end;

end.
