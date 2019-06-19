program InIOCPReverseProxy;

{ FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\inIocp\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
}
  
uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  Windows,
  frmIOCPReverseProxySvr in '..\Form\frmIOCPReverseProxySvr.pas' {FormInIOCPRecvProxySvr},
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame};

{$R *.res}

begin
  if FindWindow(nil, 'InIOCP-反向代理服务') > 0 then
    Inc(ProxyWindowCount);
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPRecvProxySvr, FormInIOCPRecvProxySvr);
  Application.Run;
end.
