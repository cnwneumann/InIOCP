program InIOCPHttpServer;

{ FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\inIocp\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
}

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPHttpServer in '..\Form\frmInIOCPHttpServer.pas' {FormInIOCPHttpServer},
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame},
  iocp_baseModule in '..\..\..\InIOCP\source\module\iocp_baseModule.pas' {InIOCPDataModule: TDataModule},
  dm_iniocp_test in '..\Module\dm_iniocp_test.pas' {dmInIOCPTest: TInIOCPDataModule};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPHttpServer, FormInIOCPHttpServer);
  Application.Run;
end.
