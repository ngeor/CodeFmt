unit HTMLFormatter;

{$mode delphi}

interface

uses
  Classes, SysUtils, TokenTypes, FormatterBase;

type
  THTMLFormatter = class(TFormatterBase)
  private
    function SetSpecial(const str: String): String;
  public
    procedure WriteFooter; override;
    procedure WriteHeader; override;
    procedure WriteToken(const Token: String; const TokenType: THigherTokenType); override;
  end;

implementation

procedure THTMLFormatter.WriteToken(const Token: String; const TokenType: THigherTokenType);
var
  escapedToken, FormatToken: String;
begin
  escapedToken := SetSpecial(Token);
  case TokenType of
    htCRLF:
      FormatToken := '<BR>' + escapedToken;
    htDirective, htKeyWord:
      FormatToken := '<B>' + escapedToken + '</B>';
    htComment:
      FormatToken := '<FONT COLOR=#000080><I>' + escapedToken + '</I></FONT>';
    htUnknown:
      FormatToken := '<FONT COLOR=#FF0000><B>' + escapedToken + '</B></FONT>';
    htPreProcessor:
      FormatToken := '<FONT COLOR=#808080>' + escapedToken + '</FONT>';
    htString:
      FormatToken := '<FONT COLOR=#000080>' + escapedToken + '</FONT>';
    else
      FormatToken := escapedToken;
  end;

  Write(FormatToken);
end;

function THTMLFormatter.SetSpecial(const str: String): String;
var
  i: Integer;
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
