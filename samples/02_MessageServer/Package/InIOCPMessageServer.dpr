program InIOCPMessageServer;

uses
  FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
//  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  Windows,
  frmInIOCPMessageServer in '..\Form\frmInIOCPMessageServer.pas' {FormInIOCPMessageServer},
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame};

{$R *.res}
{$R uac.res}

begin
  Application.Initialize;
  if Windows.FindWindow(nil, 'InIOCP 消息服务') > 0 then
    InstanceCount := 1;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPMessageServer, FormInIOCPMessageServer);
  Application.Run;
end.
