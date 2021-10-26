
Unit PascalLexer;

{$MODE Delphi}

Interface

Uses SysUtils, Classes, LexerBase;

Type 
  TPascalLexer = Class(TLexerBase)
    Private 
      Procedure HandleAnsiComments;
      Procedure HandleBorC;
      Procedure HandleString;
      Procedure HandleIdentifier;
      Procedure HandleNumber;
      Procedure HandleSymbol;
      Procedure HandleMultilineComment;
      Procedure HandleChar;
      Procedure HandleHexNumber;
      Function IsDiffKey(aToken: String): boolean;
      Function IsDirective(aToken: String): boolean;
      Function IsKeyword(aToken: String): boolean;
    Protected 
      Procedure Scan;
      override;
  End;

Implementation

Uses TokenTypes;

Const 
  PasKeywords: array[0..98] Of string = 
                                        ('ABSOLUTE', 'ABSTRACT', 'AND', 'ARRAY', 'AS', 'ASM',
                                         'ASSEMBLER',
                                         'AUTOMATED', 'BEGIN', 'CASE', 'CDECL', 'CLASS', 'CONST',
                                         'CONSTRUCTOR',
                                         'DEFAULT', 'DESTRUCTOR', 'DISPID', 'DISPINTERFACE', 'DIV',
                                         'DO',
                                         'DOWNTO', 'DYNAMIC', 'ELSE', 'END', 'EXCEPT', 'EXPORT',
                                         'EXPORTS',
                                         'EXTERNAL', 'FAR', 'FILE', 'FINALIZATION', 'FINALLY', 'FOR'
                                         , 'FORWARD',
                                         'FUNCTION', 'GOTO', 'IF', 'IMPLEMENTATION', 'IN', 'INDEX',
                                         'INHERITED',
                                         'INITIALIZATION', 'INLINE', 'INTERFACE', 'IS', 'LABEL',
                                         'LIBRARY',
                                         'MESSAGE', 'MOD', 'NAME', 'NEAR', 'NIL', 'NODEFAULT', 'NOT'
                                         , 'OBJECT',
                                         'OF', 'OR', 'OUT', 'OVERRIDE', 'PACKED', 'PASCAL',
                                         'PRIVATE', 'PROCEDURE',
                                         'PROGRAM', 'PROPERTY', 'PROTECTED', 'PUBLIC', 'PUBLISHED',
                                         'RAISE',
                                         'READ', 'READONLY', 'RECORD', 'REGISTER', 'REPEAT',
                                         'RESIDENT',
                                         'RESOURCESTRING', 'SAFECALL', 'SET', 'SHL', 'SHR',
                                         'STDCALL', 'STORED',
                                         'STRING', 'STRINGRESOURCE', 'THEN', 'THREADVAR', 'TO',
                                         'TRY', 'TYPE',
                                         'UNIT', 'UNTIL', 'USES', 'VAR', 'VIRTUAL', 'WHILE', 'WITH',
                                         'WRITE',
                                         'WRITEONLY', 'XOR');

  PasDirectives: array[0..10] Of string = 
                                          ('AUTOMATED', 'INDEX', 'NAME', 'NODEFAULT', 'READ',
                                           'READONLY',
                                           'RESIDENT', 'STORED', 'STRINGRECOURCE', 'WRITE',
                                           'WRITEONLY');

  PasDiffKeys: array[0..6] Of string = 
                                       ('END', 'FUNCTION', 'PRIVATE', 'PROCEDURE', 'PRODECTED',
                                        'PUBLIC', 'PUBLISHED');

Procedure TPascalLexer.Scan;
Begin
  HandleCRLF(StreamTokenizer, TokenFound);
  HandleSpace(StreamTokenizer, TokenFound);
  HandleSlashesComment(StreamTokenizer, TokenFound);
  HandleAnsiComments;
  HandleIdentifier;
  HandleNumber;
  HandleBorC;
  HandleSymbol;
  HandleString;
  HandleChar;
  HandleHexNumber;
End;

(*
  Handles Ansi style comments, i.e. with parenthesis and stars.
*)
Procedure TPascalLexer.HandleAnsiComments;

Function IsEndOfAnsiComment: boolean;
Begin
  IsEndOfAnsiComment := (StreamTokenizer.Current = '*') And (StreamTokenizer.PeekNext = ')');
End;

Begin
  If (StreamTokenizer.Current = '(') And (StreamTokenizer.PeekNext = '*') Then
    Begin
    { read the '(' and the '*' }
      StreamTokenizer.Next;
      StreamTokenizer.Next;

      While (Not StreamTokenizer.IsEof) And (Not IsEndOfAnsiComment) Do
        HandleMultilineComment;

      If Not StreamTokenizer.IsEof Then
        Begin
      { read the closing *) part of the comment }
          StreamTokenizer.Next;
          StreamTokenizer.Next;
        End;

      CurrentTokenFound(ttComment);
    End;
End;

Procedure TPascalLexer.HandleMultilineComment;
Begin
  If StreamTokenizer.IsEoln Then
    Begin
    { print accumulated comment so far }
      If Not StreamTokenizer.IsEmptyToken Then
        Begin
          CurrentTokenFound(ttComment);
        End;

    { print CRLF }
      HandleCRLF(StreamTokenizer, TokenFound);
    End
  Else
    Begin
    { carry on }
      StreamTokenizer.Next;
    End;
End;

{
  Handles Borland style comments, i.e. with curly braces.
}
Procedure TPascalLexer.HandleBorC;
Begin
  If StreamTokenizer.Current = '{' Then
    Begin
      While (Not StreamTokenizer.IsEof) And (StreamTokenizer.Current <> '}') Do
        HandleMultilineComment;

    (* read the closing } part of the comment *)
      If Not StreamTokenizer.IsEof Then
        StreamTokenizer.Next;

      CurrentTokenFound(ttComment);
    End;
End;

Procedure TPascalLexer.HandleString;
Begin
  If StreamTokenizer.Current = #39 Then
    Begin
      StreamTokenizer.Next;
      While (Not StreamTokenizer.IsEof) And (StreamTokenizer.Current <> #39) Do
        StreamTokenizer.Next;

      StreamTokenizer.Next;
      CurrentTokenFound(ttString);
    End;
End;  { HandleString }

Procedure TPascalLexer.HandleChar;
Begin
  If StreamTokenizer.Scan(['#'], ['0'..'9']) Then
    CurrentTokenFound(ttString);
End;

Procedure TPascalLexer.HandleHexNumber;
Begin
  If StreamTokenizer.Scan(['$'], ['0'..'9', 'A'..'F', 'a'..'f']) Then
    CurrentTokenFound(ttNumber);
End;

Function BinarySearch(hay: Array Of String; needle: String): boolean;

Var 
  First, Last, I, Compare: integer;
  Token: string;
Begin
  First := Low(hay);
  Last := High(hay);
  Result := False;
  Token := UpperCase(needle);
  While First <= Last Do
    Begin
      I := (First + Last) shr 1;
      Compare := CompareStr(hay[i], Token);
      If Compare = 0 Then
        Begin
          Result := True;
          break;
        End
      Else
        If Compare < 0 Then
          First := I + 1
      Else
        Last := I - 1;
    End;
End;

Function TPascalLexer.IsDiffKey(aToken: String): boolean;
Begin
  Result := BinarySearch(PasDiffKeys, aToken);
End;  { IsDiffKey }

Function TPascalLexer.IsDirective(aToken: String): boolean;

Var 
  First, Last, I, Compare: integer;
  Token: string;
  FDiffer: boolean;
Begin
  First := Low(PasDirectives);
  Last := High(PasDirectives);
  Result := False;
  Token := UpperCase(aToken);
  If CompareStr('PROPERTY', Token) = 0 Then
    FDiffer := True;
  If IsDiffKey(Token) Then
    FDiffer := False;
  While First <= Last Do
    Begin
      I := (First + Last) shr 1;
      Compare := CompareStr(PasDirectives[i], Token);
      If Compare = 0 Then
        Begin
          Result := True;
          If FDiffer Then
            Begin
              Result := False;
              If CompareStr('NAME', Token) = 0 Then
                Result := True;
              If CompareStr('RESIDENT', Token) = 0 Then
                Result := True;
              If CompareStr('STRINGRESOURCE', Token) = 0 Then
                Result := True;
            End;
          break;
        End
      Else
        If Compare < 0 Then
          First := I + 1
      Else
        Last := I - 1;
    End;
End;

Function TPascalLexer.IsKeyword(aToken: String): boolean;
Begin
  Result := BinarySearch(PasKeywords, aToken);
End;

Procedure TPascalLexer.HandleIdentifier;

Var 
  token: string;
  tokenType: TTokenType;
Begin
  (* cannot start with number but it can contain one *)
  If StreamTokenizer.Scan(['A'..'Z', 'a'..'z', '_'], ['A'..'Z', 'a'..'z', '0'..'9', '_']) Then
    Begin
      token := StreamTokenizer.TokenAndMark;

      If IsKeyword(token) Then
        Begin
          If IsDirective(token) Then
            tokenType := ttDirective
          Else
            tokenType := ttKeyWord;
        End
      Else
        tokenType := ttIdentifier;

      TokenFound(token, tokenType);
    End;
End;

Procedure TPascalLexer.HandleNumber;
Begin
  If StreamTokenizer.Scan(['0'..'9'], ['0'..'9', '.', 'e', 'E']) Then
    CurrentTokenFound(ttNumber);
End;

Procedure TPascalLexer.HandleSymbol;
Begin
  If (StreamTokenizer.Current In ['!', '"', '%', '&', '('..'/', ':'..'@',
     '['..'^', '`', '~']) Then
    Begin
      StreamTokenizer.Next;
      CurrentTokenFound(ttSymbol);
    End;
End;

End.
