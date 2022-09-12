unit CodeFmtParsers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Recognizers, Tokenizers, Parsers, TokenTypes;

function IsIdentifierRemaining(Ch: Char): Boolean;
function IdentifierRecognizer: TTokenRecognizer;

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

implementation

function IsIdentifierRemaining(Ch: Char): Boolean;
begin
  Result := IsLetter(Ch) or IsDigit(Ch) or (Ch = '_') or (Ch = '_');
end;

function IdentifierRecognizer: TTokenRecognizer;
begin
  Result := TLeadingPredicateRecognizer.Create(IsLetter, IsIdentifierRemaining);
end;

(* TListToFmtMapper *)

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

end.
