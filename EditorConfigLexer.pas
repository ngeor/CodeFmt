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

function IsIdentifierRemaining(Ch: Char): Boolean;
begin
  Result := IsLetter(Ch) or IsDigit(Ch) or (Ch = '_') or (Ch = '_');
end;

type
  TFmt = record
    Text: String;
    Kind: TTokenType;
  end;

function CreateRecognizers: TTokenRecognizers;
begin
  Result := [
    // 0: T_NEW_LINE
    TNewLineRecognizer.Create,
    // 1: T_WS
    TPredicateRecognizer.Create(IsWhiteSpace),
    // 2: T_DIGITS
    TPredicateRecognizer.Create(IsDigit),
    // 3: T_POUND
    TNeedleRecognizer.Create('#'),
    // 4: T_LEFT_BRACKET
    TNeedleRecognizer.Create('['),
    // 5: T_RIGHT_BRACKET
    TNeedleRecognizer.Create(']'),
    // 6: T_IDENTIFIER
    TLeadingPredicateRecognizer.Create(IsLetter, IsIdentifierRemaining),
    // 7: T_UNKNOWN
    TAnyRecognizer.Create];
end;

type
  (* For editor config, covers everything except for the line comment *)
  TSimpleParser = class(TParser<TFmt>)
  public
    function Parse(Source: TUndoTokenizer): TParseResult<TFmt>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TFmt); override;
  end;

function TSimpleParser.Parse(Source: TUndoTokenizer): TParseResult<TFmt>;
var
  Next: TToken;
begin
  Next := Source.Read;
  case Next.Kind of
    -1: Result.Success := False;
    0: begin
      Result.Success := True;
      Result.Data.Text := Next.Text;
      Result.Data.Kind := ttCRLF;
    end;
    1: begin
      Result.Success := True;
      Result.Data.Text := Next.Text;
      Result.Data.Kind := ttSpace;
    end;
    2: begin
      Result.Success := True;
      Result.Data.Text := Next.Text;
      Result.Data.Kind := ttNumber;
    end;
    6: begin
      Result.Success := True;
      Result.Data.Text := Next.Text;
      Result.Data.Kind := ttIdentifier;
    end;
    7: begin
      Result.Success := True;
      Result.Data.Text := Next.Text;
      Result.Data.Kind := ttUnknown;
    end;
    else
    begin
      Result.Success := False;
      Source.Undo(Next);
    end;
  end;
end;

procedure TSimpleParser.Undo(Source: TUndoTokenizer; Data: TFmt);
begin
  raise Exception.Create('oops');
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
  Result.Kind := ttComment;
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
  Result.Kind := ttDirective; // just to see it bold
  Result.Text := Buffer;
end;

function SectionParser: TParser<TFmt>;
begin
  Result := Map<TTokenLinkedList, TFmt>(
    Seq(
      Seq(
        TMapTokenToTokenListParser.Create(TTokenKindParser.Create(4)),
        TManyTokensParser.Create(TTokenKindParser.Create([1, 2, 6, 7]))
      ),
      TMapTokenToTokenListParser.Create(TTokenKindParser.Create(5))
    ),
    SectionMapper
  );
end;

function CommentParser: TParser<TFmt>;
begin
  Result := Map<TTokenAndString, TFmt>(
    Seq<TToken, String>(
      TTokenKindParser.Create(3),
      TUntilEolParser.Create
    ),
    MapComment
  );
end;

function CreateParser: TParser<TFmt>;
begin
  Result := TOrParser<TFmt>.Create(
    CommentParser,
    TOrParser<TFmt>.Create(
      SectionParser,
      TSimpleParser.Create
    )
  );
end;

procedure TNewEditorConfigLexer.FormatStream(InputStream: TStream);
var
  Tokenizer: TUndoTokenizer;
  Parser: TParser<TFmt>;
  Result: TParseResult<TFmt>;
  Fmt: TFmt;
begin
  Tokenizer := CreateUndoTokenizer(InputStream, CreateRecognizers);
  Parser := CreateParser;
  repeat
    Result := Parser.Parse(Tokenizer);
    if Result.Success then
    begin
      Fmt := Result.Data;
      TokenFound(Fmt.Text, Fmt.Kind);
    end;
  until not Result.Success;
  Tokenizer.Free;
  Parser.Free;
end;

end.
