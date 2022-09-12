unit CppParser;

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

type
  TTokenType = (ttEol, ttWhiteSpace, ttDigits, ttPound, ttDoubleQuote, ttDoubleSlash, ttKeyword, ttIdentifier, ttUnknown);
  TTokenTypeSet = set of TTokenType;
const
  AllTokenTypes: TTokenTypeSet = [ttEol..ttUnknown];

function CreateRecognizer(TokenType: TTokenType): TTokenRecognizer;
begin
  case TokenType of
    ttEol: Result := TNewLineRecognizer.Create;
    ttWhiteSpace: Result := TPredicateRecognizer.Create(IsWhiteSpace);
    ttDigits: Result := TPredicateRecognizer.Create(IsDigit);
    ttPound: Result := TSingleCharRecognizer.Create('#');
    ttDoubleQuote: Result := TSingleCharRecognizer.Create('"');
    ttDoubleSlash: Result := TStringRecognizer.Create('//');
    ttKeyword: Result := TKeywordRecognizer.Create(CppKeyWords, csSensitive);
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
  SourceTokens: array of TTokenType = [ttEol, ttWhiteSpace, ttDigits, ttKeyword, ttIdentifier, ttUnknown];
  DestTokens: array of THigherTokenType = [htCRLF, htSpace, htNumber, htKeyword, htIdentifier, htUnknown];
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
    Seq(FilterToken(ttDoubleQuote), ManyTokens(FilterTokens(AllTokenTypes - [ttEol, ttDoubleQuote]))),
    FilterToken(ttDoubleQuote)
  );
end;

// Pre-processor directive

function PreProcessorDirective: TParser<TTokenLinkedList>;
begin
  Result := Seq(FilterToken(ttPound), ManyTokens(FilterTokens([ttIdentifier, ttKeyword])));
end;

function CreateParser: TParser<TFmt>;
begin
  Result := TListToFmtMapper.Create(SlashComments, htComment)
    .OrElse(TListToFmtMapper.Create(StringParser, htString))
    .OrElse(TListToFmtMapper.Create(PreProcessorDirective, htDirective))
    .OrElse(SimpleParser);
end;

end.
