unit PascalParser;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Recognizers, Parsers, TokenTypes;

function CreateRecognizers: TTokenRecognizers;
function CreateParser: TParser<TFmt>;

implementation

uses Types, Tokenizers, ParseResult, TokenParsers, CodeFmtParsers;

(* Recognizers *)

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

type
  TTokenType = (
    ttEol,
    ttWhiteSpace,
    ttDigits,
    ttSingleQuote,
    ttDollarSign,
    ttDoubleSlash,
    ttAnsiCommentBegin,
    ttAnsiCommentEnd,
    ttBraceOpen,
    ttBraceClose,
    ttHexNumber,
    ttChar,
    ttKeyword,
    ttIdentifier,
    ttUnknown
  );
  TTokenTypeSet = set of TTokenType;
const
  AllTokenTypes: TTokenTypeSet = [ttEol..ttUnknown];

function IsPound(Ch: Char): Boolean;
begin
  Result := Ch = '#';
end;

function IsDollarSign(Ch: Char): Boolean;
begin
  Result := Ch = '$';
end;

function IsHexDigit(Ch: Char): Boolean;
begin
  Result := Ch in ['0'..'9', 'a'..'f', 'A'..'F'];
end;

function CreateRecognizer(TokenType: TTokenType): TTokenRecognizer;
begin
  case TokenType of
    ttEol: Result := TNewLineRecognizer.Create;
    ttWhiteSpace: Result := TPredicateRecognizer.Create(IsWhiteSpace);
    ttDigits: Result := TPredicateRecognizer.Create(IsDigit);
    ttSingleQuote: Result := TSingleCharRecognizer.Create(#39);
    ttDollarSign: Result := TSingleCharRecognizer.Create('$');
    ttDoubleSlash: Result := TStringRecognizer.Create('//');
    ttAnsiCommentBegin: Result := TStringRecognizer.Create('(*');
    ttAnsiCommentEnd: Result := TStringRecognizer.Create('*)');
    ttBraceOpen: Result := TSingleCharRecognizer.Create('{');
    ttBraceClose: Result := TSingleCharRecognizer.Create('}');
    ttHexNumber: Result := TLeadingPredicateRecognizer.Create(IsDollarSign, IsHexDigit, 1);
    ttChar: Result := TLeadingPredicateRecognizer.Create(IsPound, IsDigit);
    ttKeyword: Result := TKeywordRecognizer.Create(PasKeyWords, csInsensitive);
    ttIdentifier: Result := IdentifierRecognizer;
    ttUnknown: Result := TAnyRecognizer.Create;
    else raise Exception.Create('Unknown token type')
  end;
end;

function CreateRecognizers: TTokenRecognizers;
var
  TokenType: TTokenType;
begin
  SetLength(Result, Ord(High(AllTokenTypes)) - Ord(Low(AllTokenTypes)) + 1);
  for TokenType := Low(AllTokenTypes) to High(AllTokenTypes) do
    Result[Ord(TokenType)] := CreateRecognizer(TokenType);
end;

(* Parsers *)

(* TokenTypeFilterParser *)

function FilterToken(TokenType: TTokenType): TParser<TToken>;
begin
  Result := FilterTokenType(Ord(TokenType));
end;

function FilterTokens(TokenTypes: TTokenTypeSet): TParser<TToken>;
var
  x: TByteDynArray;
  TokenType: TTokenType;
begin
  SetLength(x, 0);
  for TokenType in TokenTypes do
  begin
    SetLength(x, Length(x) + 1);
    x[Length(x) - 1] := Ord(TokenType);
  end;

  Result := FilterTokenTypes(x);
end;

(* Simple Parser maps tokens almost as-is from one enum to another *)

function SimpleParser(TokenType: TTokenType; HigherTokenType: THigherTokenType): TParser<TFmt>; overload;
begin
  Result := TListToFmtMapper.Create(MapTokenToList(FilterToken(TokenType)), HigherTokenType);
end;

function SimpleParser: TParser<TFmt>; overload;
var
  SourceTokens: array of TTokenType = [
    ttEol,
    ttWhiteSpace,
    ttDigits,
    ttHexNumber,
    ttChar,
    ttKeyword,
    ttIdentifier,
    ttUnknown
  ];
  DestTokens: array of THigherTokenType = [
    htCRLF,
    htSpace,
    htNumber,
    htNumber,
    htString,
    htKeyword,
    htIdentifier,
    htUnknown
  ];
  i: Integer;
begin
  Result := nil;
  for i := Low(SourceTokens) to High(SourceTokens) do
    Result := OrElse<TFmt>(Result, SimpleParser(SourceTokens[i], DestTokens[i]));
end;

(* NoEol *)

function NoEol: TParser<TTokenLinkedList>;
begin
  Result := ManyTokens(FilterTokens(AllTokenTypes - [ttEol]));
end;

// Slash comments

function SlashComments: TParser<TTokenLinkedList>;
begin
  Result := Seq(FilterToken(ttDoubleSlash), NoEol);
end;

// String

function StringParser: TParser<TTokenLinkedList>;
begin
  Result := Seq(
    Seq(FilterToken(ttSingleQuote), ManyTokens(FilterTokens(AllTokenTypes - [ttEol, ttSingleQuote]))),
    FilterToken(ttSingleQuote)
  );
end;

// Ansi Comments

function AnsiCommentsParser: TParser<TTokenLinkedList>;
begin
  Result := Seq(
    Seq(
      FilterToken(ttAnsiCommentBegin),
      ManyTokens(FilterTokens(AllTokenTypes - [ttAnsiCommentEnd]))
    ),
    FilterToken(ttAnsiCommentEnd)
  );
end;

// Borland Comments

function BorlandCommentsParser: TParser<TTokenLinkedList>;
begin
  Result := Seq(
    Seq(
      FilterToken(ttBraceOpen),
      ManyTokens(FilterTokens(AllTokenTypes - [ttBraceClose]))
    ),
    FilterToken(ttBraceClose)
  );
end;

function CreateParser: TParser<TFmt>;
begin
  Result := TListToFmtMapper.Create(SlashComments, htComment)
    .OrElse(TListToFmtMapper.Create(StringParser, htString))
    .OrElse(TListToFmtMapper.Create(AnsiCommentsParser, htComment))
    .OrElse(TListToFmtMapper.Create(BorlandCommentsParser, htComment))
    .OrElse(SimpleParser);
end;

end.
