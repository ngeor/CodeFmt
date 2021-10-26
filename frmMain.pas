
Unit frmMain;

{$MODE Delphi}

Interface

Uses 
LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls,
Forms, Dialogs, ComCtrls, Buttons, ExtCtrls, Menus,
IpHtml,
Factory;

Type 
  { TMainForm }

  TMainForm = Class(TForm)
    IpHtmlPanel1: TIpHtmlPanel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    FileOpen: TMenuItem;
    FileExit: TMenuItem;
    FileSaveAs: TMenuItem;
    ToolsMenu: TMenuItem;
    ToolsPref: TMenuItem;
    HelpMenu: TMenuItem;
    HelpAbout: TMenuItem;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    btnFileOpen: TToolButton;
    btnFileSave: TToolButton;
    StatusBar1: TStatusBar;
    Procedure FileOpenClick(Sender: TObject);
    Procedure FileExitClick(Sender: TObject);
    Procedure ToolsPrefClick(Sender: TObject);
    Procedure HelpAboutClick(Sender: TObject);
    Procedure FileSaveAsClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure SaveDialog1TypeChange(Sender: TObject);
    Private 
      FDocType: TDocumentType;
      FCurFileName: string;
      Procedure SetDocType(Value: TDocumentType);
      Procedure SetCurFileName(Const Value: String);
      Procedure OpenFile(Const FileName: String; aDocType: TDocumentType);
      overload;
      property DocType: TDocumentType read FDocType write SetDocType;
      property CurFileName: string read FCurFileName write SetCurFileName;
    Public 
      Procedure OpenFile(Const FileName: String);
      overload;
  End;

Var 
  MainForm: TMainForm;

Implementation

Uses frmAbout;

{$R *.lfm}

Procedure TMainForm.SetCurFileName(Const Value: String);
Begin
  FCurFileName := Value;
  Statusbar1.SimpleText := Value;
End;

Procedure TMainForm.SetDocType(Value: TDocumentType);
Begin
  FDocType := Value;
  btnFileSave.Enabled := FDocType <> dtNone;
  FileSaveAs.Enabled := FDocType <> dtNone;
End;

Procedure TMainForm.OpenFile(Const FileName: String; aDocType: TDocumentType);

Var 
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
Begin
  InputStream := TFileStream.Create(FileName, fmOpenRead);
  Try
    OutputStream := TMemoryStream.Create;
    Try
      Process(ftHtml, aDocType, InputStream, OutputStream);
      OutputStream.seek(0, 0);
      IpHtmlPanel1.SetHtmlFromStream(OutputStream);
      DocType := aDocType;
      CurFileName := FileName;
    Finally
      OutputStream.Free;
End;
Finally
  InputStream.Free;
End;
End;

Procedure TMainForm.FileOpenClick(Sender: TObject);
Begin
  If OpenDialog1.Execute Then
    OpenFile(OpenDialog1.FileName);
End;

Procedure TMainForm.FileExitClick(Sender: TObject);
Begin
  Close;
End;

{
  Opens the preferences dialog (TODO)
}
Procedure TMainForm.ToolsPrefClick(Sender: TObject);
Begin
End;

Procedure TMainForm.HelpAboutClick(Sender: TObject);
Begin
  AboutForm.ShowModal;
End;

Function IsCppFileExtension(Const extension: String): boolean;
Begin
  IsCppFileExtension := (extension = '.cpp') Or (extension = '.c') Or (extension = '.h');
End;

Function IsPascalFileExtension(Const extension: String): boolean;
Begin
  IsPascalFileExtension := (extension = '.pas') Or (extension = '.dpr') Or
                           (extension = '.lpr');
End;

Procedure TMainForm.OpenFile(Const FileName: String);

Var 
  s: string;
Begin
  s := ExtractFileExt(FileName);
  If IsCppFileExtension(s) Then
    OpenFile(FileName, dtCpp)
  Else If IsPascalFileExtension(s) Then
         OpenFile(FileName, dtPascal)
  Else If ExtractFileName(FileName) = '.editorconfig' Then
         OpenFile(FileName, dtEditorConfig)
  Else
    MessageDlg('Αυτή η μορφή δεν υποστηρίζεται (' + s + ').', mtError, [
               mbOK], 0);
End;

Procedure TMainForm.FileSaveAsClick(Sender: TObject);

Var 
  InputStream, OutputStream: TFileStream;
  formatterType: TFormatterType;
Begin
  If FDocType <> dtNone Then
    Begin
      If SaveDialog1.Execute Then
        Begin
          Case SaveDialog1.FilterIndex Of 
            1: formatterType := ftRtf;
            2: formatterType := ftHtml;
            Else
              raise Exception.Create('Not implemented!');
          End;

          InputStream := TFileStream.Create(FCurFilename, fmOpenRead);
          Try
            OutputStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
            Try
              Process(formatterType, FDocType, InputStream, OutputStream);
            Finally
              OutputStream.Free;
        End;
    Finally
      InputStream.Free;
End;
End;
End;
End;

Procedure TMainForm.FormCreate(Sender: TObject);
Begin
  DocType := dtNone;
End;


{
  Changes the default extension of the save dialog based on the selected
  filter in the drop down list.
}
Procedure TMainForm.SaveDialog1TypeChange(Sender: TObject);
Begin
  Case SaveDialog1.FilterIndex Of 
    1: SaveDialog1.DefaultExt := 'rtf';
    2: SaveDialog1.DefaultExt := 'html';
  End;
End;

End.
