unit frmAbout;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TAboutForm = class(TForm)
    btnOk: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}




end.
