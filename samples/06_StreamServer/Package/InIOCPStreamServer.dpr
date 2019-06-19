program InIOCPStreamServer;

{ FastMM4 in '..\..\..\InIOCP\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\InIOCP\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  SynScaleMM in '..\..\..\inIocp\memMgr\SynScaleMM\SynScaleMM.pas', }
  
uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPStreamServer in '..\Form\frmInIOCPStreamServer.pas' {FormInIOCPStreamServer},
  fmIOCPSvrInfo in '..\..\..\InIOCP\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPStreamServer, FormInIOCPStreamServer);
  Application.Run;
end.
