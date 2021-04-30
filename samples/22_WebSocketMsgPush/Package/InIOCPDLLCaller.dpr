library InIOCPDLLCaller;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  Forms,
  iocp_base,
  iocp_wsClients,
  iocp_clientBase,
  frmInIOCPWebSocketMsgClient in '..\Form\frmInIOCPWebSocketMsgClient.pas' {FormInIOCPWsJSONMsgClient};

var
  WsConn: TInWSConnection;

{$R *.res}

procedure Init(AHandle: THandle); stdcall;
begin
  FormInIOCPWsJSONMsgClient := TFormInIOCPWsJSONMsgClient.Create(Application);
  WsConn := FormInIOCPWsJSONMsgClient.InWSConnection1;
  FormInIOCPWsJSONMsgClient.Show;
end;

procedure SendMsg; stdcall;
begin
  with WsConn.JSON do
  begin
    S['group'] := 'APP';
    S['msg'] := 'bbbbb';
    Post;
  end;
end;

exports
  Init, SendMsg;

begin
end.
