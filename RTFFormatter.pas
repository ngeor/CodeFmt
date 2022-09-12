unit RTFFormatter;

{$mode delphi}

interface

uses
  Classes, SysUtils, TokenTypes, FormatterBase;

type
  TRTFFormatter = class(TFormatterBase)
  private
    function SetSpecial(const str: String): String;
  public
    procedure WriteFooter; override;
    procedure WriteHeader; override;
    procedure WriteToken(const Token: String; const TokenType: THigherTokenType); override;
  end;

implementation

procedure TRTFFormatter.WriteToken(const Token: String; const TokenType: THigherTokenType);
var
  escapedToken, FormatToken: String;
begin
  escapedToken := SetSpecial(Token);
  case TokenType of
    htCRLF:
      FormatToken := '\par' + escapedToken;
    htDirective, htKeyWord:
      FormatToken := '\b ' + escapedToken + '\b0 ';
    htComment:
      FormatToken := '\cf1\i ' + escapedToken + '\cf0\i0 ';
    else
      FormatToken := escapedToken;
  end;

  Write(FormatToken);
end;

function TRTFFormatter.SetSpecial(const str: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(str) do
    case str[i] of
      '\', '{', '}': Result := Result + '\' + str[i];
      else
        Result := Result + str[i];
    end;
end;

procedure TRTFFormatter.WriteFooter;
begin
  WriteLn('');
  WriteLn('\par}');
end;

procedure TRTFFormatter.WriteHeader;
begin
  WriteLn('{\rtf1\ansi\ansicpg1253\deff0\deflang1032');
  WriteLn('');
  WriteLn('{\fonttbl');
  WriteLn('{\f0\fcourier Courier New Greek;}');
  WriteLn('}');
  WriteLn('');
  WriteLn('{\colortbl ;\red0\green0\blue128;}');
  WriteLn('');
  WriteLn('\pard\plain \li120 \fs20');
end;

end.
