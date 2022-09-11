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

uses TokenTypes, Tokenizers, Recognizers, Parsers, ParseResult, TokenParsers;

(* Recognizers *)

function IsIdentifierRemaining(Ch: Char): Boolean;
begin
  Result := IsLetter(Ch) or IsDigit(Ch) or (Ch = '_') or (Ch = '_');
end;

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
    ttIdentifier: Result := TLeadingPredicateRecognizer.Create(IsLetter, IsIdentifierRemaining);
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

type
  (* For editor config, covers most simple cases *)
  TSimpleParser = class(TParser<TFmt>)
  public
    function Parse(Source: TUndoTokenizer): TParseResult<TFmt>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TFmt); override;
  end;

function TSimpleParser.Parse(Source: TUndoTokenizer): TParseResult<TFmt>;
var
  Next: TToken;
  SourceTokens: array of TTokenType = [ttEol, ttWhiteSpace, ttDigits, ttIdentifier, ttUnknown];
  DestTokens: array of THigherTokenType = [htCRLF, htSpace, htNumber, htIdentifier, htUnknown];
  i: Integer;
begin
  Next := Source.Read;
  If Next.Kind < 0 Then
    Result := FailedParseResult<TFmt>()
  else
  begin
    i := Low(SourceTokens);
    while (i <= High(SourceTokens)) and not Result.Success do
    begin
      if Ord(SourceTokens[i]) = Next.Kind then
        Result := SuccessParseResult<TFmt>(CreateFmt(Next.Text, DestTokens[i]))
      else
        Inc(i);
    end;
    if not Result.Success then Source.Undo(Next);
  end
end;

procedure TSimpleParser.Undo(Source: TUndoTokenizer; Data: TFmt);
begin
  raise Exception.Create('oops');
end;

(* TokenTypeFilterParser *)

type
  TTokenTypeFilterParser = class(TFilterParser<TToken>)
  private
    FTokenTypes: TTokenTypeSet;
  protected
    function Filter(Data: TToken): Boolean; override;
  public
    constructor Create(Parser: TParser<TToken>; TokenType: TTokenType); overload;
    constructor Create(Parser: TParser<TToken>; TokenTypes: TTokenTypeSet); overload;
  end;

constructor TTokenTypeFilterParser.Create(Parser: TParser<TToken>; TokenType: TTokenType);
begin
  inherited Create(Parser);
  FTokenTypes := [TokenType];
end;

constructor TTokenTypeFilterParser.Create(Parser: TParser<TToken>; TokenTypes: TTokenTypeSet);
begin
  inherited Create(Parser);
  FTokenTypes := TokenTypes;
end;

function TTokenTypeFilterParser.Filter(Data: TToken): Boolean;
begin
  Result := (Data.Kind >= 0) and ( TTokenType(Data.Kind) in FTokenTypes );
end;

function FilterToken(TokenType: TTokenType): TParser<TToken>;
begin
  Result := TTokenTypeFilterParser.Create(TAnyTokenParser.Create, TokenType);
end;

function FilterTokens(TokenTypes: TTokenTypeSet): TParser<TToken>;
begin
  Result := TTokenTypeFilterParser.Create(TAnyTokenParser.Create, TokenTypes);
end;

(* TokenListToFmtMapper *)

type
  TListToFmtMapper = class(TMapParser<TTokenLinkedList, TFmt>)
  private
    FKind: THigherTokenType;
  protected
    function Map(List: TTokenLinkedList): TFmt; override;
  public
    constructor Create(Parser: TParser<TTokenLinkedList>; Kind: THigherTokenType);
    procedure Undo(Source: TUndoTokenizer; Data: TFmt); override;
  end;

constructor TListToFmtMapper.Create(Parser: TParser<TTokenLinkedList>; Kind: THigherTokenType);
begin
  inherited Create(Parser);
  FKind := Kind;
end;

function TListToFmtMapper.Map(List: TTokenLinkedList): TFmt;
var
  Buffer: String;
begin
  Buffer := '';
  while not List.IsEmpty do
    { list is LIFO }
    Buffer := List.Pop.Text + Buffer;
  List.Free;
  Result.Kind := FKind;
  Result.Text := Buffer;
end;

procedure TListToFmtMapper.Undo(Source: TUndoTokenizer; Data: TFmt);
begin
  raise Exception.Create('TListToFmtMapper.Undo is not possible');
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
  Result := CommentParser.OrElse(SectionParser).OrElse(TSimpleParser.Create);
end;

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
