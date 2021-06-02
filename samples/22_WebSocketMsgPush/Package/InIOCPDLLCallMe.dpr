library InIOCPDLLCallMe;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{ FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\inIocp\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
}

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  SysUtils,
  Classes,
  Forms,
  iocp_base, iocp_wsClients, iocp_clientBase,
  frmInIOCPWebSocketMsgClient in '..\Form\frmInIOCPWebSocketMsgClient.pas' {FormInIOCPWsJSONMsgClient};

{$R *.res}

procedure ShowForm(AHandle: THandle); stdcall;
begin
  if Application.Handle = 0 then  // = 0 时 DLL 的客户端无法线程同步
    Application.CreateHandle;
//  Application.Handle := AHandle;  // 不可行
  if not Assigned(FormInIOCPWsJSONMsgClient) then
    FormInIOCPWsJSONMsgClient := TFormInIOCPWsJSONMsgClient.Create(Application);
  FormInIOCPWsJSONMsgClient.Show;
end;

procedure CloseForm; stdcall;
begin
  if Assigned(FormInIOCPWsJSONMsgClient) then
    FreeAndNil(FormInIOCPWsJSONMsgClient);
end;

exports
  ShowForm,
  CloseForm;
  
begin
end.
