program InIOCPWebSocketDBClient;

{ FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\inIocp\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
}

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame},
  frmInIOCPWebSocketDBClient in '..\Form\frmInIOCPWebSocketDBClient.pas' {FormInIOCPWsDBClient},
  iocp_baseModule in '..\..\..\InIOCP\source\module\iocp_baseModule.pas' {InIOCPDataModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPWsDBClient, FormInIOCPWsDBClient);
  Application.Run;
end.
