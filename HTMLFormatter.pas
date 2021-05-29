unit HTMLFormatter;

{$mode delphi}

interface

uses
  Classes, SysUtils, TokenTypes, FormatterBase;

type
  THTMLFormatter = class(TFormatterBase)
  private
    function SetSpecial(const str: string): string;
  public
    procedure WriteFooter; override;
    procedure WriteHeader; override;
    procedure WriteToken(const Token: string; const TokenType: TTokenType); override;
  end;

implementation

procedure THTMLFormatter.WriteToken(const Token: string;
  const TokenType: TTokenType);
var
  escapedToken, FormatToken: string;
begin
  escapedToken := SetSpecial(Token);
  case TokenType of
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
    else
      FormatToken := escapedToken;
  end;

  Write(FormatToken);
end;

function THTMLFormatter.SetSpecial(const str: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(str) do
    case str[i] of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
			#9: Result := Result + '&nbsp;&nbsp;'; {TODO: specify tab width}
      ' ':
        if (i < Length(str)) and (str[i + 1] = ' ') then
          Result := Result + '&nbsp;'
        else
          Result := Result + ' ';
      else
        Result := Result + str[i];
    end;
end;

procedure THTMLFormatter.WriteFooter;
begin
  WriteLn('</TT></BODY>');
  WriteLn('</HTML>');
end;

procedure THTMLFormatter.WriteHeader;
begin
  WriteLn('<HTML>');
  WriteLn('<HEAD>');
  WriteLn('<TITLE></TITLE>');
  WriteLn('</HEAD>');
  WriteLn('<BODY><TT>');
end;

end.
