
Unit frmAbout;

{$MODE Delphi}

Interface

Uses 
LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics,
Controls, Forms, Dialogs,
ExtCtrls, StdCtrls;

Type 
  TAboutForm = Class(TForm)
    btnOk: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Private 
    { Private declarations }
    Public 
    { Public declarations }
  End;

Var 
  AboutForm: TAboutForm;

Implementation

{$R *.lfm}




End.
