program CodeFmt;

{$MODE Delphi}

uses
  SysUtils,
  Classes,
  Forms,
  Interfaces,
  Factory in 'Factory.pas',
  DocumentTypes,
  frmMain in 'frmMain.pas' {MainForm},
  frmAbout in 'frmAbout.pas' {AboutForm};

{$R *.res}

  function RightSeekPos(const substr, s: String): Integer;
  var
    i, k: Integer;
  begin
    k := Length(substr);
    i := Length(s) - k + 1;
    while (i > 0) and (Copy(s, i, k) <> substr) do
      i := i - 1;
    Result := i;
  end;

  function GetFormatterType(s: String): TFormatterType;
  var
    toUpper: String;
  begin
    toUpper := UpperCase(s);
    if toUpper = 'RTF' then
      Result := ftRtf
    else if toUpper = 'HTML' then
      Result := ftHtml
    else
      raise Exception.Create('Unsupported output format: ' + s);
  end;

const
  sExt: array [ftHtml..ftRtf] of String = ('.html', '.rtf');
var
  sFileName: String;
  sName: String;
  sFormatted: String;
  formatterType: TFormatterType;
  i: Integer;
  InputStream, OutputStream: TFileStream;
begin
  if (ParamCount = 2) or (ParamCount = 3) then
  begin
    formatterType := GetFormatterType(ParamStr(1));

    sFileName := ParamStr(2);
    i := RightSeekPos('.', sFileName);
    if (i = 0) and (not FileExists(sFileName)) then
    begin
      sName := sFileName;
      sFileName := sFileName + '.pas';
    end
    else
      sName := Copy(sFileName, 1, i - 1);

    sFormatted := ParamStr(3);
    if sFormatted = '' then
      sFormatted := sName + sExt[formatterType]
    else if RightSeekPos('.', sFormatted) = 0 then
      sFormatted := sFormatted + sExt[formatterType];

    InputStream := TFileStream.Create(sFileName, fmOpenRead);
    try
      OutputStream := TFileStream.Create(sFormatted, fmCreate);
      try
        Process(formatterType, dtPascal, InputStream, OutputStream);
      finally
        OutputStream.Free;
      end;
    finally
      InputStream.Free;
    end;
  end
  else
  begin
    Application.Initialize;
    Application.Title := 'Μορφοποίηση κώδικα';
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TAboutForm, AboutForm);
    if (ParamCount = 1) and FileExists(ParamStr(1)) then
      MainForm.OpenFile(ParamStr(1));
    Application.Run;
  end;
end.
