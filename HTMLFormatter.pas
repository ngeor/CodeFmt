
Unit HTMLFormatter;

{$mode delphi}

Interface

Uses 
Classes, SysUtils, TokenTypes, FormatterBase;

Type 
  THTMLFormatter = Class(TFormatterBase)
    Private 
      Function SetSpecial(Const str: String): string;
    Public 
      Procedure WriteFooter;
      override;
      Procedure WriteHeader;
      override;
      Procedure WriteToken(Const Token: String; Const TokenType: TTokenType);
      override;
  End;

Implementation

Procedure THTMLFormatter.WriteToken(Const Token: String;
                                    Const TokenType: TTokenType);

Var 
  escapedToken, FormatToken: string;
Begin
  escapedToken := SetSpecial(Token);
  Case TokenType Of 
    ttCRLF:
            FormatToken := '<BR>' + escapedToken;
    ttDirective, ttKeyWord:
                            FormatToken := '<B>' + escapedToken + '</B>';
    ttComment:
               FormatToken := '<FONT COLOR=#000080><I>' + escapedToken + '</I></FONT>';
    ttUnknown:
               FormatToken := '<FONT COLOR=#FF0000><B>' + escapedToken + '</B></FONT>';
    ttPreProcessor:
                    FormatToken := '<FONT COLOR=#808080>' + escapedToken + '</FONT>';
    Else
      FormatToken := escapedToken;
  End;

  Write(FormatToken);
End;

Function THTMLFormatter.SetSpecial(Const str: String): string;

Var 
  i: integer;
Begin
  Result := '';
  For i := 1 To Length(str) Do
    Case str[i] Of 
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
      #9: Result := Result + '&nbsp;&nbsp;'; {TODO: specify tab width}
      ' ':
           If (i < Length(str)) And (str[i + 1] = ' ') Then
             Result := Result + '&nbsp;'
           Else
             Result := Result + ' ';
      Else
        Result := Result + str[i];
    End;
End;

Procedure THTMLFormatter.WriteFooter;
Begin
  WriteLn('</TT></BODY>');
  WriteLn('</HTML>');
End;

Procedure THTMLFormatter.WriteHeader;
Begin
  WriteLn('<HTML>');
  WriteLn('<HEAD>');
  WriteLn('<TITLE></TITLE>');
  WriteLn('</HEAD>');
  WriteLn('<BODY><TT>');
End;

End.
