unit EditorConfigLexer;

{$mode delphi}

interface

uses
  Classes, SysUtils, LexerBase;

type
  TEditorConfigLexer = class(TLexerBase)
  protected
    procedure Scan; override;
  private
    procedure HandleIdentifier;
    procedure HandleNumber;
    procedure HandleSymbol;
  end;

implementation

uses TokenTypes;

procedure TEditorConfigLexer.Scan;
begin
  HandleCRLF(StreamTokenizer, TokenFound);
  HandleSpace(StreamTokenizer, TokenFound);
  HandleLineComment(StreamTokenizer, TokenFound, '#');
  HandleIdentifier;
  HandleNumber;
  HandleSymbol;
end;

procedure TEditorConfigLexer.HandleIdentifier;
begin
  if StreamTokenizer.Scan(['a'..'z'], ['a'..'z', '0'..'9', '-', '_']) then
    CurrentTokenFound(ttIdentifier);
end;

procedure TEditorConfigLexer.HandleNumber;
begin
  if StreamTokenizer.Scan(['0'..'9'], ['0'..'9']) then
    CurrentTokenFound(ttNumber);
end;

procedure TEditorConfigLexer.HandleSymbol;
begin
  if StreamTokenizer.Current in ['[', ']', '=', '*'] then
  begin
    StreamTokenizer.Next;
    CurrentTokenFound(ttSymbol);
  end;
end;

end.
