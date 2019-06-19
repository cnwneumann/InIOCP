program InIOCP_LAN_Admin;

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPAdmin in '..\Form\frmInIOCPAdmin.pas' {FormInIOCPAdmin},
  frmInIOCPLogin in '..\Form\frmInIOCPLogin.pas' {FormInIOCPLogin},
  frmInIOCPRegister in '..\Form\frmInIOCPRegister.pas' {FormInIOCPRegister};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPAdmin, FormInIOCPAdmin);
  Application.Run;
end.
