unit EditorConfigLexer;

{$mode delphi}

interface

uses
  Classes, SysUtils, LexerBase;

type
  TOldEditorConfigLexer = class(TOldLexerBase)
  protected
    procedure Scan; override;
  private
    procedure HandleIdentifier;
    procedure HandleNumber;
    procedure HandleSymbol;
  end;

type
  TNewEditorConfigLexer = class(TLexerBase)
  public
    procedure FormatStream(InputStream: TStream); override;
  end;


implementation

uses TokenTypes, Tokenizers, Recognizers, Parsers;

procedure TOldEditorConfigLexer.Scan;
begin
  HandleCRLF(StreamTokenizer, TokenFound);
  HandleSpace(StreamTokenizer, TokenFound);
  HandleLineComment(StreamTokenizer, TokenFound, '#');
  HandleIdentifier;
  HandleNumber;
  HandleSymbol;
end;

procedure TOldEditorConfigLexer.HandleIdentifier;
begin
  if StreamTokenizer.Scan(['a'..'z'], ['a'..'z', '0'..'9', '-', '_']) then
    CurrentTokenFound(ttIdentifier);
end;

procedure TOldEditorConfigLexer.HandleNumber;
begin
  if StreamTokenizer.Scan(['0'..'9'], ['0'..'9']) then
    CurrentTokenFound(ttNumber);
end;

procedure TOldEditorConfigLexer.HandleSymbol;
begin
  if StreamTokenizer.Current in ['[', ']', '=', '*'] then
  begin
    StreamTokenizer.Next;
    CurrentTokenFound(ttSymbol);
  end;
end;

(* NewEditorConfigLexer *)

function IsIdentifierRemaining(Ch: Char): Boolean;
begin
  Result := IsLetter(Ch) or IsDigit(Ch) or (Ch = '_') or (Ch = '_');
end;

type
  TFmt = record
    Text: String;
    Kind: TTokenType;
  end;
  TFmts = array of TFmt;

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
    // 4: T_SYMBOL
    TNeedleRecognizer.Create('[]=*'),
    // 5: T_IDENTIFIER
    TLeadingPredicateRecognizer.Create(IsLetter, IsIdentifierRemaining),
    // 6: T_UNKNOWN
    TAnyRecognizer.Create
  ];
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
    0: begin Result.Success := True; Result.Data.Text := Next.Text; Result.Data.Kind := ttCRLF; end;
    1: begin Result.Success := True; Result.Data.Text := Next.Text; Result.Data.Kind := ttSpace; end;
    2: begin Result.Success := True; Result.Data.Text := Next.Text; Result.Data.Kind := ttNumber; end;
    4: begin Result.Success := True; Result.Data.Text := Next.Text; Result.Data.Kind := ttSymbol; end;
    5: begin Result.Success := True; Result.Data.Text := Next.Text; Result.Data.Kind := ttIdentifier; end;
    6: begin Result.Success := True; Result.Data.Text := Next.Text; Result.Data.Kind := ttUnknown; end;
    else begin Result.Success := False; Source.Undo(Next); end;
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

function CreateFmtParser: TParser<TFmt>;
begin
  Result := TOrParser<TFmt>.Create(
    CommentParser,
    TSimpleParser.Create
  );
end;

function MapFmtList(List: TList): TFmts;
var
  i: Integer;
  p: ^TFmt;
begin
  SetLength(Result, List.Count);
  for i := 0 to List.Count - 1 do
  begin
    p := List[i];
    Result[i] := p^;
    Dispose(p);
  end;
  List.Free();
end;

function CreateParser: TParser<TFmts>;
begin
  Result := Map<TList, TFmts>(
    TManyParser<TFmt>.Create(CreateFmtParser),
    MapFmtList
  );
end;

procedure TNewEditorConfigLexer.FormatStream(InputStream: TStream);
var
  Tokenizer: TUndoTokenizer;
  Parser: TParser<TFmts>;
  Result: TParseResult<TFmts>;
  Fmts: TFmts;
  i: Integer;
begin
  Tokenizer := CreateUndoTokenizer(InputStream, CreateRecognizers);
  Parser := CreateParser;
  Result := Parser.Parse(Tokenizer);
  if Result.Success then
  begin
    Fmts := Result.Data;
    for i := Low(Fmts) to High(fmts) do
    begin
      TokenFound(Fmts[i].Text, Fmts[i].Kind);
    end;
  end;
  Tokenizer.Free;
  Parser.Free;
end;

end.
