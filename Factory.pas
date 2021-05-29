unit Factory;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
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
procedure Process(
  FormatterType: TFormatterType;
  DocumentType: TDocumentType;
  InputStream, OutputStream: TStream);

implementation

uses
  LexerBase, PascalLexer, CppLexer, EditorConfigLexer,
  FormatterBase, RTFFormatter, HTMLFormatter;

function CreateFormatter(FormatterType: TFormatterType; OutputStream: TStream): TFormatterBase;
begin
  case FormatterType of
    ftHtml:
      Result := THTMLFormatter.Create(OutputStream);
    ftRtf:
      Result := TRTFFormatter.Create(OutputStream);
    else
      raise Exception.Create('Not implemented!');
  end;
end;

function CreateLexer(DocumentType: TDocumentType; Formatter: TFormatterBase): TLexerBase;
begin
  case DocumentType of
    dtCpp:
      Result := TCppLexer.Create(Formatter.WriteToken);
    dtPascal:
      Result := TPascalLexer.Create(Formatter.WriteToken);
    dtEditorConfig:
      Result := TEditorConfigLexer.Create(Formatter.WriteToken);
    else
      raise Exception.Create('Not implemented');
  end;
end;

procedure Process(FormatterType: TFormatterType; DocumentType: TDocumentType; InputStream, OutputStream: TStream);
var
  formatter: TFormatterBase;
  lexer: TLexerBase;
begin
  formatter := CreateFormatter(FormatterType, OutputStream);
  try
    formatter.WriteHeader;
    lexer := CreateLexer(DocumentType, formatter);
    try
      lexer.FormatStream(InputStream);
    finally
      lexer.Free;
    end;
    formatter.WriteFooter;
  finally
    formatter.Free;
  end;
end;

end.

