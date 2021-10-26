
Unit EditorConfigLexer;

{$mode delphi}

Interface

Uses 
Classes, SysUtils, LexerBase;

Type 
  TEditorConfigLexer = Class(TLexerBase)
    Protected 
      Procedure Scan;
      override;
    Private 
      Procedure HandleIdentifier;
      Procedure HandleNumber;
      Procedure HandleSymbol;
  End;

Implementation

Uses TokenTypes;

Procedure TEditorConfigLexer.Scan;
Begin
  HandleCRLF(StreamTokenizer, TokenFound);
  HandleSpace(StreamTokenizer, TokenFound);
  HandleLineComment(StreamTokenizer, TokenFound, '#');
  HandleIdentifier;
  HandleNumber;
  HandleSymbol;
End;

Procedure TEditorConfigLexer.HandleIdentifier;
Begin
  If StreamTokenizer.Scan(['a'..'z'], ['a'..'z', '0'..'9', '-', '_']) Then
    CurrentTokenFound(ttIdentifier);
End;

Procedure TEditorConfigLexer.HandleNumber;
Begin
  If StreamTokenizer.Scan(['0'..'9'], ['0'..'9']) Then
    CurrentTokenFound(ttNumber);
End;

Procedure TEditorConfigLexer.HandleSymbol;
Begin
  If StreamTokenizer.Current In ['[', ']', '=', '*'] Then
    Begin
      StreamTokenizer.Next;
      CurrentTokenFound(ttSymbol);
    End;
End;

End.
