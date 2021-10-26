
Unit Factory;

{$mode delphi}

Interface

Uses 
Classes, SysUtils;

Type 
  { The document type to read. This determines which lexer will be used. }
  TDocumentType = (
    { Unknown document type}
                   dtNone,

    { CPP source }
                   dtCpp,

    { Pascal source}
                   dtPascal,

    { EditorConfig configuration file }
                   dtEditorConfig
                  );

  { The output type. This determines which formatter will be used. }
  TFormatterType = (
    { HTML }
                    ftHtml,

    { Rich text format }
                    ftRtf
                   );

{ Formats the given input stream and writes the formatted output into the output stream. }
Procedure Process(
                  FormatterType: TFormatterType;
                  DocumentType: TDocumentType;
                  InputStream, OutputStream: TStream);

Implementation

Uses 
LexerBase, PascalLexer, CppLexer, EditorConfigLexer,
FormatterBase, RTFFormatter, HTMLFormatter;

Function CreateFormatter(FormatterType: TFormatterType; OutputStream: TStream): TFormatterBase;
Begin
  Case FormatterType Of 
    ftHtml:
            Result := THTMLFormatter.Create(OutputStream);
    ftRtf:
           Result := TRTFFormatter.Create(OutputStream);
    Else
      raise Exception.Create('Not implemented!');
  End;
End;

Function CreateLexer(DocumentType: TDocumentType; Formatter: TFormatterBase): TLexerBase;
Begin
  Case DocumentType Of 
    dtCpp:
           Result := TCppLexer.Create(Formatter.WriteToken);
    dtPascal:
              Result := TPascalLexer.Create(Formatter.WriteToken);
    dtEditorConfig:
                    Result := TEditorConfigLexer.Create(Formatter.WriteToken);
    Else
      raise Exception.Create('Not implemented');
  End;
End;

Procedure Process(FormatterType: TFormatterType; DocumentType: TDocumentType; InputStream,
                  OutputStream: TStream);

Var 
  formatter: TFormatterBase;
  lexer: TLexerBase;
Begin
  formatter := CreateFormatter(FormatterType, OutputStream);
  Try
    formatter.WriteHeader;
    lexer := CreateLexer(DocumentType, formatter);
    Try
      lexer.FormatStream(InputStream);
    Finally
      lexer.Free;
End;
formatter.WriteFooter;
Finally
  formatter.Free;
End;
End;

End.
