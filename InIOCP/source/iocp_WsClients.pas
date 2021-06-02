(*
 * InIOCP WebSocket 协议客户端单元
 *
 *)
unit iocp_wsClients;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.ExtCtrls,
  System.Variants, Datasnap.DSIntf, Datasnap.DBClient, {$ELSE}
  Windows, Classes, SysUtils, ExtCtrls,
  Variants, DSIntf, DBClient, {$ENDIF}
  iocp_Winsock2, iocp_base, iocp_lists, iocp_senders,
  iocp_receivers, iocp_baseObjs, iocp_utils, iocp_wsExt,
  iocp_msgPacks, iocp_clientBase, iocp_WsJSON;

type

  // ============ WebSocket 客户端 类 ============

  TSendThread  = class;
  TRecvThread  = class;
  TPostThread  = class;

  TJSONMessage = class;
  TJSONResult  = class;

  // ============ 客户端连接 ============

  // 加入任务事件
  TAddWorkEvent = procedure(Sender: TObject; Msg: TJSONMessage) of object;
  
  // 接收标准 WebSocket 数据事件（无消息封装）
  TReceiveData  = procedure(Sender: TObject; const Msg: String) of object;

  // 被动接收到 JSON
  TPassvieEvent = procedure(Sender: TObject; Msg: TJSONResult) of object;

  // 结果返回事件
  TReturnEvent  = procedure(Sender: TObject; Result: TJSONResult) of object;

  TInWSConnection = class(TBaseConnection)
  private
    FMasking: Boolean;         // 使用掩码
    FJSON: TJSONMessage;       // 待发 JSON 消息

    FOnAddWork: TAddWorkEvent;     // 加入任务事件
    FOnReceiveData: TReceiveData;  // 收到无封装的数据
    FOnReceiveMsg: TPassvieEvent;  // 被动接收消息事件
    FOnReturnResult: TReturnEvent; // 处理返回值事件
  private
    function GetJSON: TJSONMessage;
    procedure HandlePushedData(Stream: TMemoryStream);
    procedure HandlePushedMsg(Msg: TJSONResult);
    procedure HandleReturnMsg(Result: TJSONResult);
    procedure ReceiveAttachment;
  protected
    procedure InterBeforeConnect; override;
    procedure InterAfterConnect; override;  // 初始化资源
    procedure InterAfterDisconnect; override;  // 释放资源
  public
    constructor Create(AOwner: TComponent); override;
  public
    property JSON: TJSONMessage read GetJSON;
  published
    property Masking: Boolean read FMasking write FMasking default False;
    property URL;
  published
    property OnAddWork: TAddWorkEvent read FOnAddWork write FOnAddWork;
    property OnReceiveData: TReceiveData read FOnReceiveData write FOnReceiveData;  // 被动消息
    property OnReceiveMsg: TPassvieEvent read FOnReceiveMsg write FOnReceiveMsg;
    property OnReturnResult: TReturnEvent read FOnReturnResult write FOnReturnResult;
  end;

  // ============ 用户发送的 JSON 消息包 ============

  TJSONMessage = class(TSendJSON)
  public
    constructor Create(AOwner: TInWSConnection);
    procedure Post;
    procedure SetRemoteTable(DataSet: TClientDataSet; const TableName: String);
  end;

  // ============ 客户端收到的 JSON 消息包 ============

  TJSONResult = class(TBaseJSON)
  protected
    FOpCode: TWSOpCode;         // 操作类型（关闭）
    FMsgType: TWSMsgType;       // 数据类型
    FStream: TMemoryStream;     // 非 InIOCP-JSON 的原始数据流
  public
    destructor Destroy; override;
    property MsgType: TWSMsgType read FMsgType;
  end;

  // =================== 发送线程 类 ===================

  TSendThread = class(TBaseSendThread)
  private
    FMsgPack: TJSONMessage;   // 当前发送消息包
  protected
    procedure InterSendMsg(RecvThread: TBaseRecvThread); override;
  public
    procedure AddWork(Msg: TBasePackObject); override;
  end;

  // =================== 推送结果的线程 类 ===================
  // 保存接收到的消息到列表，逐一塞进应用层

  TPostThread = class(TBasePostThread)
  private
    FMsg: TJSONResult;         // 从列表取的首消息
    procedure DoInMainThread;
  protected
    procedure HandleMessage(Msg: TBasePackObject); override;
  end;

  // =================== 接收线程 类 ===================

  TRecvThread = class(TBaseRecvThread)
  private
    procedure CheckUpgradeState(Buf: PAnsiChar; Len: Integer);
    procedure OnAttachment(Msg: TBaseJSON);
  protected
    procedure HandleDataPacket; override; // 处理收到的数据包
    procedure OnDataReceive(Msg: TBasePackObject; Part: TMessagePart; RecvCount: Cardinal); override;
  public
    constructor Create(AConnection: TInWSConnection);
  end;

implementation

uses
  http_base;

{ TInWSConnection }

constructor TInWSConnection.Create(AOwner: TComponent);
begin
  inherited;
end;

function TInWSConnection.GetJSON: TJSONMessage;
begin
  if FActive then
  begin
    if (FJSON = nil) then
      FJSON := TJSONMessage.Create(Self);
    Result := FJSON;
  end else
    Result := nil;
end;

procedure TInWSConnection.HandlePushedData(Stream: TMemoryStream);
var
  Msg: AnsiString;
begin
  // 显示未封装的数据
  if Assigned(FOnReceiveData) then
  begin
    SetString(Msg, PAnsiChar(Stream.Memory), Stream.Size);
    Msg := System.Utf8ToAnsi(Msg);
    FOnReceiveData(Self, Msg);
  end;
end;

procedure TInWSConnection.HandlePushedMsg(Msg: TJSONResult);
begin
  // 处理被动接收的 JSON 消息
  if Assigned(FOnReceiveMsg) then
    FOnReceiveMsg(Self, Msg);
end;

procedure TInWSConnection.HandleReturnMsg(Result: TJSONResult);
begin
  // 处理服务端反馈的 JSON 消息
  if Assigned(FOnReturnResult) then
    FOnReturnResult(Self, Result);
end;

procedure TInWSConnection.InterAfterConnect;
begin
  // 已经连接成功
  // 发送数据线程（在前）
  FSendThread := TSendThread.Create(Self, False);
  FSendThread.Resume;

  // 提交消息线程
  FPostThread := TPostThread.Create(Self);
  FPostThread.Resume;

  // 接收数据线程（在后）
  FRecvThread := TRecvThread.Create(Self);
  FRecvThread.Resume;
end;

procedure TInWSConnection.InterAfterDisconnect;
begin
  // 释放资源: Empty
end;

procedure TInWSConnection.InterBeforeConnect;
begin
  // WebSocket 升级请求
  FInitFlag := 'GET ' + URL + ' HTTP/1.1'#13#10 +
               'Host: ' + ServerAddr + #13#10 +  // http.sys 必须带 Host
               'Connection: Upgrade'#13#10 +
               'Upgrade: WebSocket'#13#10 +
               'Sec-WebSocket-Key: w4v7O6xFTi36lq3RNcgctw=='#13#10 +  // 客户端不检测返回KEY，用固定值
               'Sec-WebSocket-Version: 13'#13#10 +
               'Origin: InIOCP-WebSocket'#13#10#13#10;
end;

procedure TInWSConnection.ReceiveAttachment;
begin
  // 有附件流，准备接收
  if Assigned(FOnReturnResult) then
    FOnReturnResult(Self, TRecvThread(FRecvThread).FRecvMsg as TJSONResult);
end;

{ TJSONMessage }

constructor TJSONMessage.Create(AOwner: TInWSConnection);
var
  ErrMsg: String;
begin
  if (AOwner = nil) then  // 不能为 nil
    ErrMsg := '消息 Owner 不能为空.'
  else
  if not AOwner.Active then
    ErrMsg := '连接 AOwner 不可用.';

  if (ErrMsg = '') then
    inherited Create(AOwner)
  else
  if Assigned(AOwner.OnError) then
    AOwner.OnError(Self, ErrMsg)
  else
    raise Exception.Create(ErrMsg);

end;

procedure TJSONMessage.Post;
var
  Connection: TInWSConnection;
begin
  Connection := TInWSConnection(FOwner);
  if Assigned(Connection) then
    try
      Connection.FSendThread.AddWork(Self); // 提交消息
    finally
      if (Self = Connection.FJSON) then
        Connection.FJSON := nil;  // 设 nil
    end;
end;

procedure TJSONMessage.SetRemoteTable(DataSet: TClientDataSet; const TableName: String);
begin
  DataSet.SetOptionalParam(szTABLE_NAME, TableName, True); // 设置数据表
end;

{ TJSONResult }

destructor TJSONResult.Destroy;
begin
  if Assigned(FStream) then
    FStream.Free;  
  inherited;
end;

{ TSendThread }

procedure TSendThread.AddWork(Msg: TBasePackObject);
begin
  // 加消息到任务列表
  if Assigned(TInWSConnection(FConnection).FOnAddWork) then
    TInWSConnection(FConnection).FOnAddWork(Self, TJSONMessage(Msg));
  inherited;
end;

procedure TSendThread.InterSendMsg(RecvThread: TBaseRecvThread);
begin
  // 执行发送任务, 与服务端方法类似，服务端可以连续发送多个数据，不能等待
  //   见：TReturnResult.ReturnResult、TDataReceiver.Prepare
  FMsgPack := TJSONMessage(FSendMsg);
  FSender.Owner := FMsgPack;  // 宿主
  FTotalSize := FMsgPack.Size;
  FMsgPack.InternalSend(FSender, TInWSConnection(FConnection).FMasking);
end;

{ TPostThread }

procedure TPostThread.DoInMainThread;
var
  AConnection: TInWSConnection;
begin
  // 进入主线程，把消息提交给宿主
  // 可能处理消息过程中被用户断开，此时要改变断开模式
  AConnection := TInWSConnection(FConnection);
  AConnection.FInMainThread := True;  // 进入主线程
  try
    try
      if (FMsg.FOpCode = ocClose) then
        AConnection.FTimer.Enabled := True
      else
      if Assigned(FMsg.FStream) then  // 未封装的数据
        AConnection.HandlePushedData(FMsg.FStream)
      else
      if (FMsg.Owner <> AConnection) then  // 推送来的消息
        AConnection.HandlePushedMsg(FMsg)
      else
        AConnection.HandleReturnMsg(FMsg);  // 服务端反馈给自己的消息
     finally
       FMsg.Free;  // 同时释放 FStream
       AConnection.FInMainThread := False;
    end;
  except
    on E: Exception do
    begin
      AConnection.FErrorcode := GetLastError;
      AConnection.DoClientError;  // 在主线程，直接调用
    end;
  end;
end;

procedure TPostThread.HandleMessage(Msg: TBasePackObject);
begin
  FMsg := TJSONResult(Msg);     // TJSONResult
  Synchronize(DoInMainThread);  // 进入业务层
end;

{ TRecvThread }

procedure TRecvThread.CheckUpgradeState(Buf: PAnsiChar; Len: Integer);
begin
  // 检查升级结果（简化，不检查 AcceptKey，可能出现拒绝服务的反馈）
  if not MatchSocketType(Buf, 'HTTP/1.1 101') then  // google 服务不同，HTTP_VER + HTTP_STATES_100[1]
  begin
    TInWSConnection(FConnection).FActive := False;  // 直接赋值
    TInWSConnection(FConnection).FTimer.Enabled := True;
  end;
end;

constructor TRecvThread.Create(AConnection: TInWSConnection);
var
  AReceiver: TWSClientReceiver;
begin
  FRecvMsg := TJSONResult.Create(AConnection);  // 首消息
  AReceiver := TWSClientReceiver.Create(AConnection, TJSONResult(FRecvMsg));

  AReceiver.OnNewMsg := OnCreateMsgObject;
  AReceiver.OnPost := AConnection.PostThread.Add;
  AReceiver.OnReceive := OnDataReceive;
  AReceiver.OnAttachment := OnAttachment;
  AReceiver.OnError := OnRecvError;

  inherited Create(AConnection, AReceiver);
end;

procedure TRecvThread.HandleDataPacket;
begin
  inherited;
  // 处理接收到的数据包
  if FReceiver.Completed then  // 1. 首包数据
  begin
    if MatchSocketType(FRecvBuf.buf, HTTP_VER) then  // HTTP 消息
      CheckUpgradeState(FRecvBuf.buf, FOverlapped.InternalHigh)
    else
      FReceiver.Prepare(FRecvBuf.buf, FOverlapped.InternalHigh);  // 接收
  end else
  begin
    // 2. 后续数据
    FReceiver.Receive(FRecvBuf.buf, FOverlapped.InternalHigh);
  end;

end;

procedure TRecvThread.OnAttachment(Msg: TBaseJSON);
begin
  // 有附件流，同步，让客户端判断是否接收
  TJSONResult(FRecvMsg).FMsgType := mtJSON;
  Synchronize(TInWSConnection(FConnection).ReceiveAttachment);
end;

procedure TRecvThread.OnDataReceive(Msg: TBasePackObject; Part: TMessagePart; RecvCount: Cardinal);
begin
  if (FTotalSize = 0) then
    FTotalSize := Msg.Size;
  Inc(FRecvCount, RecvCount);
  inherited;
end; 

end.

