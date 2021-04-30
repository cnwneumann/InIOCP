program InIOCPBackgroundServer;

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame},
  frmInIOCPDBServer in '..\Form\frmInIOCPDBServer.pas' {FormInIOCPDBServer},
  iocp_baseModule in '..\..\..\InIOCP\source\module\iocp_baseModule.pas' {InIOCPDataModule: TDataModule},
  dm_iniocp_test in '..\Module\dm_iniocp_test.pas' {dmInIOCPTest: TInIOCPDataModule};

{$R *.res}
{$R uac.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPDBServer, FormInIOCPDBServer);
  Application.Run;
end.
