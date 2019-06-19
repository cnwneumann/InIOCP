program InIOCPCertifyServer;



uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPCertifyServer in '..\Form\frmInIOCPCertifyServer.pas' {FormInIOCPCertifyServer},
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame};

{$R *.res}
{$R uac.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPCertifyServer, FormInIOCPCertifyServer);
  Application.Run;
end.
