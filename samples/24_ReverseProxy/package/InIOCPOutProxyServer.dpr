program InIOCPOutProxyServer;

{ FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\inIocp\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
}
  
uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame},
  frmOuterProxyServer in '..\Form\frmOuterProxyServer.pas' {FormIOCPOutProxySvr};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
//  Application.CreateForm(TFormIOCPOutProxySvr, FormIOCPOutProxySvr);
  Application.CreateForm(TFormIOCPOutProxySvr, FormIOCPOutProxySvr);
  Application.Run;
end.
