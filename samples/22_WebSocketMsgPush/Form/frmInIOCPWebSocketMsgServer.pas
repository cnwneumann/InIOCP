unit frmInIOCPWebSocketMsgServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_managers, iocp_base,
  http_objects, iocp_server, iocp_sockets, iocp_wsClients;

type
  TFormInIOCPWsJSONMsgServer = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    InHttpDataProvider1: TInHttpDataProvider;
    InWebSocketManager1: TInWebSocketManager;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InHttpDataProvider1Accept(Sender: TObject; Request: THttpRequest;
      var Accept: Boolean);
    procedure InWebSocketManager1Upgrade(Sender: TObject; const Origin: string;
      var Accept: Boolean);
    procedure InWebSocketManager1Receive(Sender: TObject; Socket: TWebSocket);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInIOCPWsJSONMsgServer: TFormInIOCPWsJSONMsgServer;

implementation

uses
  iocp_varis, iocp_utils, iocp_log;

{$R *.dfm}

procedure TFormInIOCPWsJSONMsgServer.Button1Click(Sender: TObject);
begin
  // 新版兼容标准 WebSocket 消息，增加消息的 JSON 封装，支持大文件传输
  iocp_log.TLogThread.InitLog(iocp_varis.gAppPath + 'log');  // 开启日志
  FrameIOCPSvrInfo1.Start(InIOCPServer1);
  InIOCPServer1.Active := True;
end;

procedure TFormInIOCPWsJSONMsgServer.Button2Click(Sender: TObject);
begin
  FrameIOCPSvrInfo1.Stop;
  InIOCPServer1.Active := False;
  iocp_log.TLogThread.StopLog;  // 停止日志  
end;

procedure TFormInIOCPWsJSONMsgServer.FormCreate(Sender: TObject);
begin
  InIOCPServer1.ServerAddr := 'localhost'; // '1962.168.1.196';
  InIOCPServer1.ServerPort := 80; // '12302';
  iocp_varis.gAppPath := ExtractFilePath(Application.ExeName);  // 程序路径
  iocp_utils.MyCreateDir(iocp_varis.gAppPath + 'log');  // 建日志目录
end;

procedure TFormInIOCPWsJSONMsgServer.InHttpDataProvider1Accept(Sender: TObject;
  Request: THttpRequest; var Accept: Boolean);
begin
  // Accept 默认 = True
end;

procedure TFormInIOCPWsJSONMsgServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
//  Memo1.Lines.Clear;
  Memo1.Lines.Add('IP:' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('Port:' + IntToStr(InIOCPServer1.ServerPort));  
end;

procedure TFormInIOCPWsJSONMsgServer.InWebSocketManager1Receive(Sender: TObject; Socket: TWebSocket);
begin
  // 演示消息推送
  //   1. 浏览器的消息推送
  //   2. InIOCP-JSON 消息的推送（带附件时推送的是 JSON 描述，附近流不推送）

  // Socket 带用户名才能接收推送消息
  if (Socket.UserName = '') then
    Socket.UserName := 'User_' + IntToStr(Socket.JSON.MsgId);

  // 不要推送 ocClose!
  if Socket.OpCode in [ocText, ocBiary] then
    case Socket.MsgType of
      mtDefault:  // 可能是浏览器发来的消息
        InWebSocketManager1.Broadcast(Socket);
      mtJSON: begin
  {      // 可以同时接收附件
        if Socket.JSON.HasAttachment then
          Socket.JSON.Attachment := TFileStream.Create('doc\???', fmOpenRead); }
        InWebSocketManager1.Broadcast(Socket);
  //      Memo1.Lines.Add(Socket.JSON.S['msg']);
      end;
      mtAttachment: begin
        // 接收附件时 Socket.JSON.Attachment <> nil
      end;
    end;

end;

procedure TFormInIOCPWsJSONMsgServer.InWebSocketManager1Upgrade(Sender: TObject;
  const Origin: string; var Accept: Boolean);
begin
  // Origin: 申请升级 Socket 的来源，如：www.aaa.com
  // 在此判断是否允许升级为 WebSocket，默认 Accept=True
end;

end.
