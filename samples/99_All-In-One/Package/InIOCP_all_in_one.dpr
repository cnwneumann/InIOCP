program InIOCP_all_in_one;

{ FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\inIocp\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
}

uses
  FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
//  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmTestIOCPServer in '..\Form\frmTestIOCPServer.pas' {FormTestIOCPServer},
  ZLibExApi in '..\..\..\InIOCP\source\zlib\ZLibExApi.pas',
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame},
  iocp_baseModule in '..\..\..\InIOCP\source\module\iocp_baseModule.pas' {InIOCPDataModule: TDataModule},
  dm_iniocp_test in '..\module\dm_iniocp_test.pas' {dmInIOCPTest: TInIOCPDataModule},
  iocp_zlib in '..\..\..\InIOCP\source\iocp_zlib.pas',
  http_base in '..\..\..\InIOCP\source\http_base.pas',
  http_objects in '..\..\..\InIOCP\source\http_objects.pas',
  http_utils in '..\..\..\InIOCP\source\http_utils.pas',
  iocp_api in '..\..\..\InIOCP\source\iocp_api.pas',
  iocp_base in '..\..\..\InIOCP\source\iocp_base.pas',
  iocp_clients in '..\..\..\InIOCP\source\iocp_clients.pas',
  iocp_lists in '..\..\..\InIOCP\source\iocp_lists.pas',
  iocp_log in '..\..\..\InIOCP\source\iocp_log.pas',
  iocp_managers in '..\..\..\InIOCP\source\iocp_managers.pas',
  iocp_md5 in '..\..\..\InIOCP\source\iocp_md5.pas',
  iocp_msgPacks in '..\..\..\InIOCP\source\iocp_msgPacks.pas',
  iocp_objPools in '..\..\..\InIOCP\source\iocp_objPools.pas',
  iocp_Qos in '..\..\..\InIOCP\source\iocp_Qos.pas',
  iocp_senders in '..\..\..\InIOCP\source\iocp_senders.pas',
  iocp_server in '..\..\..\InIOCP\source\iocp_server.pas',
  iocp_sockets in '..\..\..\InIOCP\source\iocp_sockets.pas',
  iocp_sqlMgr in '..\..\..\InIOCP\source\iocp_sqlMgr.pas',
  iocp_threads in '..\..\..\InIOCP\source\iocp_threads.pas',
  iocp_utils in '..\..\..\InIOCP\source\iocp_utils.pas',
  iocp_varis in '..\..\..\InIOCP\source\iocp_varis.pas',
  iocp_Winsock2 in '..\..\..\InIOCP\source\iocp_Winsock2.pas',
  iocp_wsExt in '..\..\..\InIOCP\source\iocp_wsExt.pas',
  iocp_receivers in '..\..\..\InIOCP\source\iocp_receivers.pas',
  iocp_WsJSON in '..\..\..\InIOCP\source\iocp_WsJSON.pas',
  iocp_baseObjs in '..\..\..\InIOCP\source\iocp_baseObjs.pas',
  frmInSQLMgrEditor in '..\..\..\InIOCP\source\editor\frmInSQLMgrEditor.pas' {FormInSQLManager},
  iocp_WsClients in '..\..\..\InIOCP\source\iocp_WsClients.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTestIOCPServer, FormTestIOCPServer);
  Application.Run;
end.
