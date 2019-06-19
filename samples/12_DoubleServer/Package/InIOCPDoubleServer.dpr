program InIOCPDoubleServer;

{ FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\inIocp\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
}

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmDoubleServer in '..\Form\frmDoubleServer.pas' {FormDblServer};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDblServer, FormDblServer);
  Application.Run;
end.
