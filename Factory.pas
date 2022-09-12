unit Factory;

{$mode delphi}

interface

uses
  Classes, SysUtils, DocumentTypes;

type
  { The output type. This determines which formatter will be used. }
  TFormatterType = (
    { HTML }
    ftHtml,

    { Rich text format }
    ftRtf
    );

{ Formats the given input stream and writes the formatted output into the output stream. }
procedure Process(FormatterType: TFormatterType; DocumentType: TDocumentType;
  InputStream, OutputStream: TStream);

implementation

uses
  PascalParser, CppParser, EditorConfigParser,
  FormatterBase, RTFFormatter, HTMLFormatter, Recognizers, TokenTypes, Parsers, ParseResult, Tokenizers;

function CreateFormatter(FormatterType: TFormatterType;
  OutputStream: TStream): TFormatterBase;
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

function CreateRecognizers(DocumentType: TDocumentType): TTokenRecognizers;
begin
  case DocumentType of
    dtCpp: Result := CppParser.CreateRecognizers;
    dtPascal: Result := PascalParser.CreateRecognizers;
    dtEditorConfig: Result := EditorConfigParser.CreateRecognizers;
    else raise Exception.Create('Not implemented!')
  end;
end;

function CreateParser(DocumentType: TDocumentType): TParser<TFmt>;
begin
  case DocumentType of
    dtCpp: Result := CppParser.CreateParser;
    dtPascal: Result := PascalParser.CreateParser;
    dtEditorConfig: Result := EditorConfigParser.CreateParser;
    else raise Exception.Create('Not implemented!')
  end;
end;

procedure Process(FormatterType: TFormatterType; DocumentType: TDocumentType;
  InputStream, OutputStream: TStream);
var
  formatter: TFormatterBase;
  Recognizers: TTokenRecognizers;
  Parser: TParser<TFmt>;
  Tokenizer: TUndoTokenizer;
  Next: TParseResult<TFmt>;
  Fmt: TFmt;
begin
  formatter := CreateFormatter(FormatterType, OutputStream);
  try
    formatter.WriteHeader;

    Recognizers := CreateRecognizers(DocumentType);
    Tokenizer := CreateUndoTokenizer(InputStream, Recognizers);
    Parser := CreateParser(DocumentType);
    try
      repeat
        Next := Parser.Parse(Tokenizer);
        if Next.Success then
        begin
          Fmt := Next.Data;
          formatter.WriteToken(Fmt.Text, Fmt.Kind);
        end;
      until not Next.Success;
    finally
      Parser.Free;
      Tokenizer.Free;
    end;
    formatter.WriteFooter;
  finally
    formatter.Free;
  end;
end;

end.
