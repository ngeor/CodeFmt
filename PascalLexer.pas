unit PascalLexer;

{$MODE Delphi}

interface

uses SysUtils, Classes, LexerBase;

type
  TPascalLexer = class(TOldLexerBase)
  private
    procedure HandleAnsiComments;
    procedure HandleBorC;
    procedure HandleString;
    procedure HandleIdentifier;
    procedure HandleNumber;
    procedure HandleSymbol;
    procedure HandleMultilineComment;
    procedure HandleChar;
    procedure HandleHexNumber;
    function IsDiffKey(aToken: String): Boolean;
    function IsDirective(aToken: String): Boolean;
    function IsKeyword(aToken: String): Boolean;
  protected
    procedure Scan; override;
  end;

implementation

uses TokenTypes;

const
  PasKeywords: array[0..98] of String =
    ('ABSOLUTE', 'ABSTRACT', 'AND', 'ARRAY', 'AS', 'ASM', 'ASSEMBLER',
    'AUTOMATED', 'BEGIN', 'CASE', 'CDECL', 'CLASS', 'CONST', 'CONSTRUCTOR',
    'DEFAULT', 'DESTRUCTOR', 'DISPID', 'DISPINTERFACE', 'DIV', 'DO',
    'DOWNTO', 'DYNAMIC', 'ELSE', 'END', 'EXCEPT', 'EXPORT', 'EXPORTS',
    'EXTERNAL', 'FAR', 'FILE', 'FINALIZATION', 'FINALLY', 'FOR', 'FORWARD',
    'FUNCTION', 'GOTO', 'IF', 'IMPLEMENTATION', 'IN', 'INDEX', 'INHERITED',
    'INITIALIZATION', 'INLINE', 'INTERFACE', 'IS', 'LABEL', 'LIBRARY',
    'MESSAGE', 'MOD', 'NAME', 'NEAR', 'NIL', 'NODEFAULT', 'NOT', 'OBJECT',
    'OF', 'OR', 'OUT', 'OVERRIDE', 'PACKED', 'PASCAL', 'PRIVATE', 'PROCEDURE',
    'PROGRAM', 'PROPERTY', 'PROTECTED', 'PUBLIC', 'PUBLISHED', 'RAISE',
    'READ', 'READONLY', 'RECORD', 'REGISTER', 'REPEAT', 'RESIDENT',
    'RESOURCESTRING', 'SAFECALL', 'SET', 'SHL', 'SHR', 'STDCALL', 'STORED',
    'STRING', 'STRINGRESOURCE', 'THEN', 'THREADVAR', 'TO', 'TRY', 'TYPE',
    'UNIT', 'UNTIL', 'USES', 'VAR', 'VIRTUAL', 'WHILE', 'WITH', 'WRITE',
    'WRITEONLY', 'XOR');

  PasDirectives: array[0..10] of String =
    ('AUTOMATED', 'INDEX', 'NAME', 'NODEFAULT', 'READ', 'READONLY',
    'RESIDENT', 'STORED', 'STRINGRECOURCE', 'WRITE', 'WRITEONLY');

  PasDiffKeys: array[0..6] of String =
    ('END', 'FUNCTION', 'PRIVATE', 'PROCEDURE', 'PRODECTED',
    'PUBLIC', 'PUBLISHED');

procedure TPascalLexer.Scan;
begin
  HandleCRLF(StreamTokenizer, TokenFound);
  HandleSpace(StreamTokenizer, TokenFound);
  HandleSlashesComment(StreamTokenizer, TokenFound);
  HandleAnsiComments;
  HandleIdentifier;
  HandleNumber;
  HandleBorC;
  HandleSymbol;
  HandleString;
  HandleChar;
  HandleHexNumber;
end;

(*
  Handles Ansi style comments, i.e. with parenthesis and stars.
*)
procedure TPascalLexer.HandleAnsiComments;

  function IsEndOfAnsiComment: Boolean;
  begin
    IsEndOfAnsiComment := (StreamTokenizer.Current = '*') and
      (StreamTokenizer.PeekNext = ')');
  end;

begin
  if (StreamTokenizer.Current = '(') and (StreamTokenizer.PeekNext = '*') then
  begin
    { read the '(' and the '*' }
    StreamTokenizer.Next;
    StreamTokenizer.Next;

    while (not StreamTokenizer.IsEof) and (not IsEndOfAnsiComment) do
      HandleMultilineComment;

    if not StreamTokenizer.IsEof then
    begin
      { read the closing *) part of the comment }
      StreamTokenizer.Next;
      StreamTokenizer.Next;
    end;

    CurrentTokenFound(htComment);
  end;
end;

procedure TPascalLexer.HandleMultilineComment;
begin
  if StreamTokenizer.IsEoln then
  begin
    { print accumulated comment so far }
    if not StreamTokenizer.IsEmptyToken then
    begin
      CurrentTokenFound(htComment);
    end;

    { print CRLF }
    HandleCRLF(StreamTokenizer, TokenFound);
  end
  else
  begin
    { carry on }
    StreamTokenizer.Next;
  end;
end;

{
  Handles Borland style comments, i.e. with curly braces.
}
procedure TPascalLexer.HandleBorC;
begin
  if StreamTokenizer.Current = '{' then
  begin
    while (not StreamTokenizer.IsEof) and (StreamTokenizer.Current <> '}') do
      HandleMultilineComment;

    (* read the closing } part of the comment *)
    if not StreamTokenizer.IsEof then
      StreamTokenizer.Next;

    CurrentTokenFound(htComment);
  end;
end;

procedure TPascalLexer.HandleString;
begin
  if StreamTokenizer.Current = #39 then
  begin
    StreamTokenizer.Next;
    while (not StreamTokenizer.IsEof) and (StreamTokenizer.Current <> #39) do
      StreamTokenizer.Next;

    StreamTokenizer.Next;
    CurrentTokenFound(htString);
  end;
end;  { HandleString }

procedure TPascalLexer.HandleChar;
begin
  if StreamTokenizer.Scan(['#'], ['0'..'9']) then
    CurrentTokenFound(htString);
end;

procedure TPascalLexer.HandleHexNumber;
begin
  if StreamTokenizer.Scan(['$'], ['0'..'9', 'A'..'F', 'a'..'f']) then
    CurrentTokenFound(htNumber);
end;

function BinarySearch(hay: array of String; needle: String): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := Low(hay);
  Last := High(hay);
  Result := False;
  Token := UpperCase(needle);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := CompareStr(hay[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
    if Compare < 0 then
      First := I + 1
    else
      Last := I - 1;
  end;
end;

function TPascalLexer.IsDiffKey(aToken: String): Boolean;
begin
  Result := BinarySearch(PasDiffKeys, aToken);
end;  { IsDiffKey }

function TPascalLexer.IsDirective(aToken: String): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: String;
  FDiffer: Boolean;
begin
  First := Low(PasDirectives);
  Last := High(PasDirectives);
  Result := False;
  Token := UpperCase(aToken);
  if CompareStr('PROPERTY', Token) = 0 then
    FDiffer := True;
  if IsDiffKey(Token) then
    FDiffer := False;
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := CompareStr(PasDirectives[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      if FDiffer then
      begin
        Result := False;
        if CompareStr('NAME', Token) = 0 then
          Result := True;
        if CompareStr('RESIDENT', Token) = 0 then
          Result := True;
        if CompareStr('STRINGRESOURCE', Token) = 0 then
          Result := True;
      end;
      break;
    end
    else
    if Compare < 0 then
      First := I + 1
    else
      Last := I - 1;
  end;
end;

function TPascalLexer.IsKeyword(aToken: String): Boolean;
begin
  Result := BinarySearch(PasKeywords, aToken);
end;

procedure TPascalLexer.HandleIdentifier;
var
  token: String;
  tokenType: THigherTokenType;
begin
  (* cannot start with number but it can contain one *)
  if StreamTokenizer.Scan(['A'..'Z', 'a'..'z', '_'],
    ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
  begin
    token := StreamTokenizer.TokenAndMark;

    if IsKeyword(token) then
    begin
      if IsDirective(token) then
        tokenType := htDirective
      else
        tokenType := htKeyWord;
    end
    else
      tokenType := htIdentifier;

    TokenFound(token, tokenType);
  end;
end;

procedure TPascalLexer.HandleNumber;
begin
  if StreamTokenizer.Scan(['0'..'9'], ['0'..'9', '.', 'e', 'E']) then
    CurrentTokenFound(htNumber);
end;

procedure TPascalLexer.HandleSymbol;
begin
  if (StreamTokenizer.Current in ['!', '"', '%', '&', '('..'/', ':'..'@',
    '['..'^', '`', '~']) then
  begin
    StreamTokenizer.Next;
    CurrentTokenFound(htSymbol);
  end;
end;

end.
