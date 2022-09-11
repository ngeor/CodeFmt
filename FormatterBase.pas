unit FormatterBase;

{$mode delphi}

interface

uses
  Classes, SysUtils, TokenTypes;

type
  { Base class for formatters. A formatter receives events from a lexer and
  pretty prints tokens. }
  TFormatterBase = class
  private
    { Holds the output stream into which the formatter writes its output }
    FOutputStream: TStream;
  protected
    { Gets the output stream into which the formatter writes its output }
    property OutputStream: TStream read FOutputStream;

    { Writes the given string to the output }
    procedure Write(const str: String);

    { Writes the given string to the output, followed by a newline }
    procedure WriteLn(const str: String);
  public
    constructor Create(OutputStream: TStream);
    procedure WriteHeader; virtual; abstract;
    procedure WriteFooter; virtual; abstract;
    procedure WriteToken(const Token: String; const TokenType: THigherTokenType);
      virtual; abstract;
  end;

implementation

procedure TFormatterBase.Write(const str: String);
var
  b, Buf: PChar;
begin
  if Length(str) > 0 then
  begin
    GetMem(Buf, Length(str) + 1);
    StrCopy(Buf, PChar(str));
    b := Buf;
    OutputStream.Write(Buf^, Length(str));
    FreeMem(b);
  end;
end;

procedure TFormatterBase.WriteLn(const str: String);
begin
  Write(str);
  Write(LineEnding);
end;

constructor TFormatterBase.Create(OutputStream: TStream);
begin
  FOutputStream := OutputStream;
end;

end.
