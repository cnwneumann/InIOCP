program InIOCPInLog;

{   FastMM4 in '..\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\memMgr\scalemm2\ScaleMM2.pas', }

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPInLog in '..\Form\frmInIOCPInLog.pas' {FormInIOCPInLog};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPInLog, FormInIOCPInLog);
  Application.Run;
end.
