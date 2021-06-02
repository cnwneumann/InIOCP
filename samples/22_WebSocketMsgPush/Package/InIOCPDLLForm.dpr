program InIOCPDLLForm;

{ FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\inIocp\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
}

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPDLLFormCaller in '..\Form\frmInIOCPDLLFormCaller.pas' {FormInIOCPDLL},
  iocp_Winsock2 in '..\..\..\InIOCP\source\iocp_Winsock2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPDLL, FormInIOCPDLL);
  Application.Run;
end.
