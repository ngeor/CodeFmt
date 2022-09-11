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

uses TokenTypes, Tokenizers, Recognizers, Parsers;

(* Recognizers *)

function IsIdentifierRemaining(Ch: Char): Boolean;
begin
  Result := IsLetter(Ch) or IsDigit(Ch) or (Ch = '_') or (Ch = '_');
end;

type
  TTokenType = (ttEol, ttWhiteSpace, ttDigits, ttPound, ttLeftBracket, ttRightBracket, ttIdentifier, ttUnknown);
  TTokenTypeSet = set of TTokenType;

function CreateRecognizer(TokenType: TTokenType): TTokenRecognizer;
begin
  case TokenType of
    ttEol: Result := TNewLineRecognizer.Create;
    ttWhiteSpace: Result := TPredicateRecognizer.Create(IsWhiteSpace);
    ttDigits: Result := TPredicateRecognizer.Create(IsDigit);
    ttPound: Result := TNeedleRecognizer.Create('#');
    ttLeftBracket: Result := TNeedleRecognizer.Create('[');
    ttRightBracket: Result := TNeedleRecognizer.Create(']');
    ttIdentifier: Result := TLeadingPredicateRecognizer.Create(IsLetter, IsIdentifierRemaining);
    ttUnknown: Result := TAnyRecognizer.Create;
    else raise Exception.Create('Unknown token type')
  end;
end;

function CreateRecognizers: TTokenRecognizers;
var
  TokenType: TTokenType;
  TokenTypes: TTokenTypeSet;
begin
  SetLength(Result, Ord(High(TokenTypes)) - Ord(Low(TokenTypes)) + 1);
  for TokenType := Low(TokenTypes) to High(TokenTypes) do
    Result[Ord(TokenType)] := CreateRecognizer(TokenType);
end;

(* Parsers *)

type
  TFmt = record
    Text: String;
    Kind: THigherTokenType;
  end;

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
    Result.Success := False
  else
  begin
    i := Low(SourceTokens);
    while (i <= High(SourceTokens)) and not Result.Success do
    begin
      if Ord(SourceTokens[i]) = Next.Kind then
      begin
        Result.Success := True;
        Result.Data.Text := Next.Text;
        Result.Data.Kind := DestTokens[i];
      end
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

type
  TUntilEolParser = class(TParser<String>)
  public
    function Parse(Source: TUndoTokenizer): TParseResult<String>; override;
    procedure Undo(Source: TUndoTokenizer; Data: String); override;
  end;

function TUntilEolParser.Parse(Source: TUndoTokenizer): TParseResult<String>;
var
  Next: TToken;
  Buffer: String;
begin
  Buffer := '';
  repeat
    Next := Source.Read;
    if Next.Kind > 0 { not eof nor eol } then
    begin
      Buffer := Buffer + Next.Text;
    end
    else if Next.Kind = 0 { eol } then
      Source.Undo(Next);
  until Next.Kind <= 0;
  Result.Success := True;
  Result.Data := Buffer;
end;

procedure TUntilEolParser.Undo(Source: TUndoTokenizer; Data: String);
begin
  raise Exception.Create('oops');
end;

function MapComment(Input: TPair<TToken, String>): TFmt;
begin
  Result.Text := '#' + Input.Right;
  Result.Kind := htComment;
end;

type
  TTokenAndString = TPair<TToken, String>;

// [test]
function SectionMapper(List: TTokenLinkedList): TFmt;
var
  Buffer: String;
begin
  Buffer := '';
  while not List.IsEmpty do
    { list is LIFO }
    Buffer := List.Pop.Text + Buffer;
  List.Free;
  Result.Kind := htDirective; // just to see it bold
  Result.Text := Buffer;
end;

function SectionParser: TParser<TFmt>;
begin
  Result := Map<TTokenLinkedList, TFmt>(
    Seq(
      Seq(
        FilterToken(ttLeftBracket),
        TManyTokensParser.Create(FilterTokens([ttWhiteSpace, ttDigits, ttIdentifier, ttUnknown]))
      ),
      FilterToken(ttRightBracket)
    ),
    SectionMapper
  );
end;

function CommentParser: TParser<TFmt>;
begin
  Result := Map<TTokenAndString, TFmt>(
    Seq<TToken, String>(
      FilterToken(ttPound),
      TUntilEolParser.Create
    ),
    MapComment
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
