
Unit CppLexer;

{$MODE Delphi}

Interface

Uses SysUtils, Classes, LexerBase;

Type 
  TCppLexer = Class(TLexerBase)
    Private 
      Procedure HandleString;
      Procedure HandleIdentifier;
      Procedure HandlePreProcessorDirective;
      Procedure HandleSymbol;
    Protected 
      Procedure Scan;
      override;
  End;

Implementation

Uses TokenTypes;

Const 
  CppKeyWords: array [0..59] Of string = 
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

Procedure TCppLexer.Scan;
Begin
  HandleCRLF(StreamTokenizer, TokenFound);
  HandleSpace(StreamTokenizer, TokenFound);
  HandleSlashesComment(StreamTokenizer, TokenFound);
  HandleString;
  HandleIdentifier;
  HandlePreProcessorDirective;
  HandleSymbol;
End;

Procedure TCppLexer.HandleString;
Begin
  If StreamTokenizer.Current = '"' Then
    Begin
      StreamTokenizer.Next;
      While (Not StreamTokenizer.IsEof) And (StreamTokenizer.Current <> '"') Do
        StreamTokenizer.Next;

      StreamTokenizer.Next;
      CurrentTokenFound(ttString);
    End;
End;

Function ArrayContains(hay: Array Of String; needle: String): boolean;

Var 
  i: integer;
Begin
  Result := False;

  For i := Low(hay) To High(hay) Do
    If hay[i] = needle Then
      Begin
        Result := True;
        break;
      End;
End;

Procedure TCppLexer.HandleIdentifier;

Var 
  token: string;
  tokenType: TTokenType;
Begin
  If StreamTokenizer.Scan(['a'..'z', 'A'..'Z'], ['a'..'z', 'A'..'Z', '_']) Then
    Begin
      token := StreamTokenizer.TokenAndMark;

      If ArrayContains(CppKeyWords, token) Then
        tokenType := ttKeyWord
      Else
        tokenType := ttIdentifier;

      TokenFound(token, tokenType);
    End;
End;

Procedure TCppLexer.HandlePreProcessorDirective;
Begin
  If StreamTokenizer.Current = '#' Then
    Begin
      While (Not StreamTokenizer.IsEof) And (Not StreamTokenizer.IsEoln) Do
        StreamTokenizer.Next;

      CurrentTokenFound(ttPreProcessor);
    End;
End;

Procedure TCppLexer.HandleSymbol;
Begin
  If StreamTokenizer.Current In ['(', ')', ';', '{', '}', '[', ']'] Then
    Begin
      StreamTokenizer.Next;
      CurrentTokenFound(ttSymbol);
    End;
End;

End.
