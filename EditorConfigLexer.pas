unit EditorConfigLexer;

{$mode delphi}

interface

uses
  Classes, SysUtils, LexerBase;

type
  TNewEditorConfigLexer = class(TLexerBase)
  public
    procedure FormatStream(InputStream: TStream); override;
  end;


implementation

uses Types, TokenTypes, Tokenizers, Recognizers, Parsers, ParseResult, TokenParsers, CodeFmtParsers;

(* Recognizers *)

type
  TTokenType = (ttEol, ttWhiteSpace, ttDigits, ttPound, ttLeftBracket, ttRightBracket, ttIdentifier, ttUnknown);
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
    ttLeftBracket: Result := TSingleCharRecognizer.Create('[');
    ttRightBracket: Result := TSingleCharRecognizer.Create(']');
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
  Result := TTokenTypeFilterParser.Create(TAnyTokenParser.Create, Ord(TokenType));
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

  Result := TTokenTypeFilterParser.Create(TAnyTokenParser.Create, x);
end;

(* Simple Parser maps tokens almost as-is from one enum to another *)

function SimpleParser(TokenType: TTokenType; HigherTokenType: THigherTokenType): TParser<TFmt>; overload;
begin
  Result := TListToFmtMapper.Create(TMapTokenToTokenListParser.Create(FilterToken(TokenType)), HigherTokenType);
end;

function SimpleParser: TParser<TFmt>; overload;
var
  SourceTokens: array of TTokenType = [ttEol, ttWhiteSpace, ttDigits, ttIdentifier, ttUnknown];
  DestTokens: array of THigherTokenType = [htCRLF, htSpace, htNumber, htIdentifier, htUnknown];
  i: Integer;
begin
  Result := nil;
  for i := Low(SourceTokens) to High(SourceTokens) do
    Result := OrElse<TFmt>(Result, SimpleParser(SourceTokens[i], DestTokens[i]));
end;

// [section]

function SectionParser: TParser<TFmt>;
begin
  Result := TListToFmtMapper.Create(
    Seq(
      Seq(
        FilterToken(ttLeftBracket),
        TManyTokensParser.Create(FilterTokens([ttWhiteSpace, ttDigits, ttIdentifier, ttUnknown]))
      ),
      FilterToken(ttRightBracket)
    ),
    htDirective
  );
end;

function NoEol: TParser<TToken>;
begin
  Result := FilterTokens(AllTokenTypes - [ttEol]);
end;

function CommentParser: TParser<TFmt>;
begin
  Result := TListToFmtMapper.Create(
    Seq(
      FilterToken(ttPound),
      TManyTokensParser.Create(NoEol)
    ),
    htComment
  );
end;

function CreateParser: TParser<TFmt>;
begin
  Result := CommentParser.OrElse(SectionParser).OrElse(SimpleParser);
end;

// TODO move this to a base class or drop the class altogether and keep a function only
// i.e. Process(InputStream, TokenFoundHandler)
procedure TNewEditorConfigLexer.FormatStream(InputStream: TStream);
var
  Tokenizer: TUndoTokenizer;
  Parser: TParser<TFmt>;
  Next: TParseResult<TFmt>;
  Fmt: TFmt;
begin
  Tokenizer := CreateUndoTokenizer(InputStream, CreateRecognizers);
  Parser := CreateParser;
  repeat
    Next := Parser.Parse(Tokenizer);
    if Next.Success then
    begin
      Fmt := Next.Data;
      TokenFound(Fmt.Text, Fmt.Kind);
    end;
  until not Next.Success;
  Tokenizer.Free;
  Parser.Free;
end;

end.
