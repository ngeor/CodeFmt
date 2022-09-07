unit DocumentTypes;

{$mode Delphi}

interface

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

function GetDocumentType(const FileName: String): TDocumentType;

implementation

uses SysUtils;

function IsCppFileExtension(const extension: string): boolean;
begin
  IsCppFileExtension := (extension = '.cpp') or (extension = '.c') or (extension = '.h');
end;

function IsPascalFileExtension(const extension: string): boolean;
begin
  IsPascalFileExtension := (extension = '.pas') or (extension = '.dpr') or
    (extension = '.lpr');
end;

function GetDocumentType(const FileName: String): TDocumentType;
var
  s: string;
begin
  s := ExtractFileExt(FileName);
  if IsCppFileExtension(s) then
    Result := dtCpp
  else if IsPascalFileExtension(s) then
    Result := dtPascal
  else if ExtractFileName(FileName) = '.editorconfig' then
    Result := dtEditorConfig
  else
    Result := dtNone;
end;

end.

