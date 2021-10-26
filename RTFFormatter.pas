
Unit RTFFormatter;

{$mode delphi}

Interface

Uses 
Classes, SysUtils, TokenTypes, FormatterBase;

Type 
  TRTFFormatter = Class(TFormatterBase)
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

Procedure TRTFFormatter.WriteToken(Const Token: String;
                                   Const TokenType: TTokenType);

Var 
  escapedToken, FormatToken: string;
Begin
  escapedToken := SetSpecial(Token);
  Case TokenType Of 
    ttCRLF:
            FormatToken := '\par' + escapedToken;
    ttDirective, ttKeyword:
                            FormatToken := '\b ' + escapedToken + '\b0 ';
    ttComment:
               FormatToken := '\cf1\i ' + escapedToken + '\cf0\i0 ';
    Else
      FormatToken := escapedToken;
  End;

  Write(FormatToken);
End;

Function TRTFFormatter.SetSpecial(Const str: String): string;

Var 
  i: integer;
Begin
  Result := '';
  For i := 1 To Length(str) Do
    Case str[i] Of 
      '\', '{', '}': Result := Result + '\' + str[i];
      Else
        Result := Result + str[i];
    End;
End;

Procedure TRTFFormatter.WriteFooter;
Begin
  WriteLn('');
  WriteLn('\par}');
End;

Procedure TRTFFormatter.WriteHeader;
Begin
  WriteLn('{\rtf1\ansi\ansicpg1253\deff0\deflang1032');
  WriteLn('');
  WriteLn('{\fonttbl');
  WriteLn('{\f0\fcourier Courier New Greek;}');
  WriteLn('}');
  WriteLn('');
  WriteLn('{\colortbl ;\red0\green0\blue128;}');
  WriteLn('');
  WriteLn('\pard\plain \li120 \fs20');
End;

End.
