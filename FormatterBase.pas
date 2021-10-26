
Unit FormatterBase;

{$mode delphi}

Interface

Uses 
Classes, SysUtils, TokenTypes;

Type 

 { Base class for formatters. A formatter receives events from a lexer and
  pretty prints tokens. }
  TFormatterBase = Class
    Private 
    { Holds the output stream into which the formatter writes its output }
      FOutputStream: TStream;
    Protected 
    { Gets the output stream into which the formatter writes its output }
      property OutputStream: TStream read FOutputStream;

    { Writes the given string to the output }
      Procedure Write(Const str: String);

    { Writes the given string to the output, followed by a newline }
      Procedure WriteLn(Const str: String);
    Public 
      constructor Create(OutputStream: TStream);
      Procedure WriteHeader;
      virtual;
      abstract;
      Procedure WriteFooter;
      virtual;
      abstract;
      Procedure WriteToken(Const Token: String; Const TokenType: TTokenType);
      virtual;
      abstract;
  End;

Implementation

Procedure TFormatterBase.Write(Const str: String);

Var 
  b, Buf: PChar;
Begin
  If Length(str) > 0 Then
    Begin
      GetMem(Buf, Length(str) + 1);
      StrCopy(Buf, PChar(str));
      b := Buf;
      OutputStream.Write(Buf^, Length(str));
      FreeMem(b);
    End;
End;

Procedure TFormatterBase.WriteLn(Const str: String);
Begin
  Write(str);
  Write(LineEnding);
End;

constructor TFormatterBase.Create(OutputStream: TStream);
Begin
  FOutputStream := OutputStream;
End;

End.
