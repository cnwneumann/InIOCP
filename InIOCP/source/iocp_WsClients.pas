(*
 * IOCP WebSocket 客户端单元
 *
 * 说明：C/S 模式和 WebSocket 协议有相似之处，但前者集成业务，
 *       一下子比较难整合两者的代码，现从 C/S 模式的客户端分离出部分代码，
 *       总体上代码冗余了，但整洁，可读性也强，容易理解。
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
  iocp_receivers, iocp_baseObjs, iocp_utils,
  iocp_msgPacks, iocp_WsJSON;

type

  // =================== WebSocket 客户端 类 ===================

  TSendThread = class;
  TRecvThread = class;
  TPostThread = class;

  TJSONMessage = class;
  TJSONResult  = class;

  // ============ 客户端连接 ============

  // 收到标准 WebSocket 数据事件（无消息封装）
  TOnReceiveData   = procedure(Sender: TObject; const Msg: String) of object;

  // 被动接收到 JSON
  TPassvieMsgEvent = procedure(Sender: TObject; Msg: TJSONResult) of object;

  // 结果返回事件
  TReturnMsgEvent  = procedure(Sender: TObject; Result: TJSONResult) of object;

  // 消息收发事件
  TOnDataTransmit  = procedure(Sender: TObject; MsgId: Int64; MsgSize, CurrentSize: TFileSize) of object;

  // 异常事件
  TConnectionError = procedure(Sender: TObject; const Msg: String) of object;
  
  TInWSConnection = class(TComponent)
  private
    FSocket: TSocket;          // 套接字
    FTimer: TTimer;            // 定时器
    
    FSendThread: TSendThread;  // 发送线程
    FRecvThread: TRecvThread;  // 接收线程
    FPostThread: TPostThread;  // 投放线程

    FRecvCount: Cardinal;      // 共收到
    FSendCount: Cardinal;      // 共发送

    FActive: Boolean;          // 开关/连接状态
    FAutoConnect: Boolean;     // 是否自动连接

    FServerAddr: String;       // 服务器地址
    FServerPort: Word;         // 服务端口
    
    FLocalPath: String;        // 下载文件的本地存放路径
    FMasking: Boolean;         // 使用掩码
    FJSON: TJSONMessage;       // 待发 JSON 消息
    FUTF8CharSet: Boolean;     // 用 UTF8 字符集

    FErrorcode: Integer;       // 异常代码
    FErrMsg: String;           // 异常消息
  private
    FAfterConnect: TNotifyEvent;      // 连接后
    FAfterDisconnect: TNotifyEvent;   // 断开后
    FBeforeConnect: TNotifyEvent;     // 连接前
    FBeforeDisconnect: TNotifyEvent;  // 断开前
    FOnDataReceive: TOnDataTransmit;  // 消息接收事件
    FOnDataSend: TOnDataTransmit;     // 消息发出事件
    FOnReceiveData: TOnReceiveData;   // 收到无封装的数据
    FOnReceiveMsg: TPassvieMsgEvent;  // 被动接收消息事件
    FOnReturnResult: TReturnMsgEvent; // 处理返回值事件
    FOnError: TConnectionError;       // 异常事件
  private
    function GetActive: Boolean;
    function GetJSON: TJSONMessage;

    procedure CreateTimer;
    procedure DoThreadFatalError;

    procedure HandlePushedData(Stream: TMemoryStream);
    procedure HandlePushedMsg(Result: TJSONResult);
    procedure HandleReturnMsg(Result: TJSONResult);
    procedure ReceiveAttachment;  

    procedure InternalOpen;
    procedure InternalClose;

    procedure ShowRecvProgress;
    procedure ShowSendProgress;

    procedure SetActive(Value: Boolean);
    procedure TimerEvent(Sender: TObject);
    procedure TryDisconnect;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Errorcode: Integer read FErrorcode;
    property RecvCount: Cardinal read FRecvCount;
    property SendCount: Cardinal read FSendCount;
    property Socket: TSocket read FSocket;
    property JSON: TJSONMessage read GetJSON;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property AutoConnect: Boolean read FAutoConnect write FAutoConnect default False;
    property LocalPath: String read FLocalPath write FLocalPath;
    property Masking: Boolean read FMasking write FMasking default False;
    property ServerAddr: String read FServerAddr write FServerAddr;
    property ServerPort: Word read FServerPort write FServerPort default DEFAULT_SVC_PORT;
    property UTF8CharSet: Boolean read FUTF8CharSet write FUTF8CharSet default True;
  published
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;

    // 接收被动消息/推送消息事件
    property OnReceiveData: TOnReceiveData read FOnReceiveData write FOnReceiveData;
    property OnReceiveMsg: TPassvieMsgEvent read FOnReceiveMsg write FOnReceiveMsg;
    property OnReturnResult: TReturnMsgEvent read FOnReturnResult write FOnReturnResult;
    
    property OnDataReceive: TOnDataTransmit read FOnDataReceive write FOnDataReceive;
    property OnDataSend: TOnDataTransmit read FOnDataSend write FOnDataSend;
    property OnError: TConnectionError read FOnError write FOnError;
  end;

  TWSConnection = TInWSConnection;

  // ============ 用户发送的 JSON 消息包 ============

  TJSONMessage = class(TSendJSON)
  public
    constructor Create(AOwner: TWSConnection);
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
    property MsgType: TWSMsgType read FMsgType;
  end;

  // =================== 发送线程 类 ===================

  TSendThread = class(TCycleThread)
  private
    FLock: TThreadLock;         // 线程锁
    FConnection: TWSConnection; // 连接
    FSender: TClientTaskSender; // 消息发送器

    FMsgList: TInList;          // 待发消息包列表
    FMsgPack: TJSONMessage;     // 当前发送消息包
    FCurrentSize: TFileSize;    // 当前发出数

    function GetCount: Integer;
    function GetWork: Boolean;
    function GetWorkState: Boolean;

    procedure ClearMsgList;
    procedure AfterSend(DataType: TMessageDataType; OutSize: Integer);
    procedure OnSendError(Sender: TObject);
  protected
    procedure AfterWork; override;
    procedure DoMethod; override;
  public
    constructor Create(AConnection: TWSConnection);
    procedure AddWork(Msg: TJSONMessage);
  public
    property Count: Integer read GetCount;
  end;

  // =================== 推送结果的线程 类 ===================
  // 保存接收到的消息到列表，逐一塞进应用层

  TPostThread = class(TCycleThread)
  private
    FLock: TThreadLock;         // 线程锁
    FConnection: TWSConnection; // 连接
    FResults: TInList;          // 收到的消息列表
    FResult: TJSONResult;       // 当前消息
    procedure ExecInMainThread;
  protected
    procedure AfterWork; override;
    procedure DoMethod; override;
  public
    constructor Create(AConnection: TWSConnection);
    procedure Add(AResult: TBaseJSON; AOpCode: TWSOpCode;
                  AMsgType: TWSMsgType; AStream: TMemoryStream);
  end;

  // =================== 接收线程 类 ===================

  TRecvThread = class(TThread)
  private
    FConnection: TWSConnection; // 连接
    FRecvBuf: TWsaBuf;          // 接收缓存
    FOverlapped: TOverlapped;   // 重叠结构

    FReceiver: TWSClientReceiver; // 数据接收器
    FResult: TJSONResult;       // 当前消息

    FMsgId: Int64;              // 当前消息 Id
    FFrameSize: TFileSize;      // 当前消息长度
    FCurrentSize: TFileSize;    // 当前消息收到的长度
    
    procedure HandleDataPacket; // 处理收到的数据包
    procedure CheckUpgradeState(Buf: PAnsiChar; Len: Integer); 
    procedure OnAttachment(Result: TBaseJSON);
    procedure OnReceive(Result: TBaseJSON; FrameSize, RecvSize: Int64);
  protected
    procedure Execute; override;
  public
    constructor Create(AConnection: TWSConnection);
    procedure Stop;
  end;

implementation

uses
  http_base;

{ var
  FDebug: TStringList;
  FStream: TMemoryStream;  } 

{ TInWSConnection }

constructor TInWSConnection.Create(AOwner: TComponent);
begin
  inherited;
  IniDateTimeFormat;
  FAutoConnect := False; // 不自动连接
  FUTF8CharSet := True;  // 字符集 UTF-8
  FServerPort := DEFAULT_SVC_PORT;
  FSocket := INVALID_SOCKET;  // 无效 Socket
end;

procedure TInWSConnection.CreateTimer;
begin
  // 建定时器(关闭用）
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 80;
  FTimer.OnTimer := TimerEvent;
end;

destructor TInWSConnection.Destroy;
begin
  SetActive(False);
  inherited;
end;

procedure TInWSConnection.DoThreadFatalError;
begin
  // 收发时出现异常
  if Assigned(FOnError) then
    FOnError(Self, IntToStr(FErrorcode) + ',' + FErrMsg);
end;

function TInWSConnection.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Result := FActive
  else
    Result := (FSocket <> INVALID_SOCKET) and FActive;
end;

function TInWSConnection.GetJSON: TJSONMessage;
begin
  if (FJSON = nil) then
    FJSON := TJSONMessage.Create(Self);
  Result := FJSON;
end;

procedure TInWSConnection.HandlePushedData(Stream: TMemoryStream);
var
  Msg: AnsiString;
begin
  // 处理被动接收的未封装的数据
  if Assigned(FOnReceiveData) then
  begin
    SetString(Msg, PAnsiChar(Stream.Memory), Stream.Size);
    Msg := System.Utf8ToAnsi(Msg);
    FOnReceiveData(Self, Msg);
  end;
end;

procedure TInWSConnection.HandlePushedMsg(Result: TJSONResult);
begin
  // 处理被动接收的 JSON 消息
  if Assigned(FOnReceiveMsg) then
    FOnReceiveMsg(Self, Result);
end;

procedure TInWSConnection.HandleReturnMsg(Result: TJSONResult);
begin
  // 处理服务端反馈的 JSON 消息
  if Assigned(FOnReturnResult) then
    FOnReturnResult(Self, Result);
end;

procedure TInWSConnection.InternalClose;
begin
  // 断开连接
  if Assigned(FBeforeDisConnect) then
    FBeforeDisConnect(Self);

  if (FSocket <> INVALID_SOCKET) then
  begin
    // 关闭 Socket
    ShutDown(FSocket, SD_BOTH);
    CloseSocket(FSocket);

    FSocket := INVALID_SOCKET;

    if FActive then
    begin
      FActive := False;

      // 释放接收线程
      if Assigned(FRecvThread) then
      begin
        FRecvThread.Terminate;  // 100 毫秒后退出
        FRecvThread := nil;
      end;

      // 投放线程
      if Assigned(FPostThread) then
      begin
        FPostThread.Stop;
        FPostThread := nil;
      end;

      // 释放发送线程
      if Assigned(FSendThread) then
      begin
        FSendThread.FSender.Stoped := True;
        FSendThread.Stop;
        FSendThread := nil;
      end;

      // 释放定时器
      if Assigned(FTimer) then
      begin
        FTimer.Free;
        FTimer := nil;
      end;      
    end;
  end;

  if not (csDestroying in ComponentState) then
    if Assigned(FAfterDisconnect) then
      FAfterDisconnect(Self);
end;

procedure TInWSConnection.InternalOpen;
const
  // WebSocket 升级请求
  WS_UPGRADE_REQUEST = AnsiString(
                       'GET / HTTP/1.1'#13#10 +
                       'Connection: Upgrade'#13#10 +
                       'Upgrade: WebSocket'#13#10 +
                       'Sec-WebSocket-Key: w4v7O6xFTi36lq3RNcgctw=='#13#10 +
                       'Sec-WebSocket-Version: 13'#13#10 +
                       'Origin: InIOCP-WebSocket'#13#10#13#10);
var
  Addr: TSockAddrIn;
begin
  // 创建 WSASocket，连接到服务器
  if Assigned(FBeforeConnect) then
    FBeforeConnect(Self);

  if (FSocket = INVALID_SOCKET) then
  begin
    // 新建 Socket
    FSocket := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);

    // 设置连接地址、端口，连接
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(FServerPort);
    Addr.sin_addr.s_addr := inet_addr(PAnsiChar(ResolveHostIP(FServerAddr)));

    // 刷新 FActive
    FActive := iocp_Winsock2.WSAConnect(FSocket, TSockAddr(Addr),
                             SizeOf(TSockAddr), nil, nil, nil, nil) = 0;

    if FActive then    // 连接成功
    begin
      // 定时器
      CreateTimer;

      // 立刻发送 WS_UPGRADE_REQUEST，服务端升级为 WebSocket
      iocp_Winsock2.Send(FSocket, WS_UPGRADE_REQUEST[1], Length(WS_UPGRADE_REQUEST), 0);

      // 收发数
      FRecvCount := 0;
      FSendCount := 0;

      // 投放线程
      FPostThread := TPostThread.Create(Self);

      // 收发线程
      FSendThread := TSendThread.Create(Self);
      FRecvThread := TRecvThread.Create(Self);

      FPostThread.Resume;
      FSendThread.Resume;
      FRecvThread.Resume;
    end else
    begin
      ShutDown(FSocket, SD_BOTH);
      CloseSocket(FSocket);
      FSocket := INVALID_SOCKET;
    end;
  end;

  if FActive and Assigned(FAfterConnect) then
    FAfterConnect(Self)
  else
  if not FActive and Assigned(FOnError) then
    FOnError(Self, '无法连接到服务器.');
    
end;

procedure TInWSConnection.Loaded;
begin
  inherited;
  // 装载后，FActive -> 打开  
  if FActive and not (csDesigning in ComponentState) then
    InternalOpen;
end;

procedure TInWSConnection.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      FActive := Value
    else
    if Value and not FActive then
      InternalOpen
    else
    if not Value and FActive then
      InternalClose;
  end;
end;

procedure TInWSConnection.ReceiveAttachment;
begin
  // 有附件流，准备接收
  if Assigned(FOnReturnResult) then
    FOnReturnResult(Self, FRecvThread.FResult);
end;

procedure TInWSConnection.ShowRecvProgress;
begin
  // 显示接收进程
  if Assigned(FOnDataReceive) then
    FOnDataReceive(Self,
                   FRecvThread.FMsgId,
                   FRecvThread.FFrameSize,
                   FRecvThread.FCurrentSize);
end;

procedure TInWSConnection.ShowSendProgress;
begin
  // 显示发送进程
  if Assigned(FOnDataSend) then 
    FOnDataSend(Self,
                FSendThread.FMsgPack.FMsgId,
                FSendThread.FMsgPack.FFrameSize,
                FSendThread.FCurrentSize);
end;

procedure TInWSConnection.TimerEvent(Sender: TObject);
begin
  FTimer.Enabled := False;
  InternalClose;  // 断开连接
end;

procedure TInWSConnection.TryDisconnect;
begin
  // 服务器关闭时，尝试关闭客户端 
  if Assigned(FTimer) then
  begin
    FTimer.OnTimer := TimerEvent;
    FTimer.Enabled := True;
  end;
end;

{ TJSONMessage }

constructor TJSONMessage.Create(AOwner: TWSConnection);
var
  ErrMsg: String;
begin
  if (AOwner = nil) then  // 不能为 nil
    ErrMsg := '消息 Owner 不能为空.'
  else
  if not AOwner.Active then
    ErrMsg := '连接 AOwner 不可用.';
  if (ErrMsg <> '') then
    raise Exception.Create(ErrMsg)
  else
    inherited Create(AOwner);  
end;

procedure TJSONMessage.Post;
begin
  if Assigned(FOwner) then
    with TWSConnection(FOwner) do
    begin
      if (Self = FJSON) then  // 是 Connection.FJSON
        FJSON := nil;         // 设 nil
      if Assigned(FSendThread) then
        FSendThread.AddWork(Self); // 提交消息
    end;
end;

procedure TJSONMessage.SetRemoteTable(DataSet: TClientDataSet; const TableName: String);
begin
  DataSet.SetOptionalParam(szTABLE_NAME, TableName, True); // 设置数据表
end;

// ================== 发送线程 ==================

{ TSendThread }

procedure TSendThread.AddWork(Msg: TJSONMessage);
begin
  // 加消息到任务列表
  //   Msg 是动态生成，不会重复投放
  FLock.Acquire;
  try
    FMsgList.Add(Msg);
  finally
    FLock.Release;
  end;
  Activate;  // 激活线程
end;

procedure TSendThread.AfterSend(DataType: TMessageDataType; OutSize: Integer);
begin
  // 数据成功发出，显示进程
  Inc(FCurrentSize, OutSize);
  Synchronize(FConnection.ShowSendProgress);
end;

procedure TSendThread.AfterWork;
begin
  // 停止线程，释放资源
  ClearMsgList;
  FMsgList.Free;
  FLock.Free;
  FSender.Free;
end;

constructor TSendThread.Create(AConnection: TWSConnection);
begin
  inherited Create;
  FConnection := AConnection;

  FLock := TThreadLock.Create; // 锁
  FMsgList := TInList.Create;  // 待发任务表

  FSender := TClientTaskSender.Create;   // 任务发送器
  FSender.Owner := Self;       // 宿主
  FSender.Socket := FConnection.Socket;  // 发送套接字
      
  FSender.AfterSend := AfterSend;  // 发出事件
  FSender.OnError := OnSendError;  // 发出异常事件
end;

procedure TSendThread.ClearMsgList;
var
  i: Integer;
begin
  // 释放列表的全部消息
  for i := 0 to FMsgList.Count - 1 do
    TJSONMessage(FMsgList.PopFirst).Free;
  if Assigned(FMsgPack) then
    FMsgPack.Free;
end;

procedure TSendThread.DoMethod;
  procedure FreeMsgPack;
  begin
    FLock.Acquire;
    try
      FMsgPack.Free;  // 释放！
      FMsgPack := nil;
    finally
      FLock.Release;
    end;
  end;
begin
  // 循环发送消息
  while (Terminated = False) and FConnection.FActive and GetWork() do
    try
      try
        FMsgPack.FUTF8CharSet := FConnection.FUTF8CharSet;
        FMsgPack.InternalSend(FSender, FConnection.FMasking);
      finally
        FreeMsgPack;  // 释放
      end;
    except
      on E: Exception do
      begin
        FConnection.FErrMsg := E.Message;
        FConnection.FErrorcode := GetLastError;
        Synchronize(FConnection.DoThreadFatalError);
      end;
    end;
end;

function TSendThread.GetCount: Integer;
begin
  // 取任务数
  FLock.Acquire;
  try
    Result := FMsgList.Count;
  finally
    FLock.Release;
  end;
end;

function TSendThread.GetWork: Boolean;
begin
  // 从列表中取一个消息
  FLock.Acquire;
  try
    if Terminated or (FMsgList.Count = 0) or Assigned(FMsgPack) then
      Result := False
    else begin
      FMsgPack := TJSONMessage(FMsgList.PopFirst);  // 取任务
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

function TSendThread.GetWorkState: Boolean;
begin
  // 取工作状态：线程、发送器未停止
  FLock.Acquire;
  try
    Result := (Terminated = False) and (FSender.Stoped = False);
  finally
    FLock.Release;
  end;
end;

procedure TSendThread.OnSendError(Sender: TObject);
begin
  // 处理发送异常
  if (GetWorkState = False) then  // 取消操作
    FConnection.FRecvThread.FReceiver.Reset;
  FConnection.FErrorcode := TClientTaskSender(Sender).ErrorCode;
  Synchronize(FConnection.DoThreadFatalError); // 线程同步
end;

{ TPostThread }

procedure TPostThread.Add(AResult: TBaseJSON; AOpCode: TWSOpCode;
                          AMsgType: TWSMsgType; AStream: TMemoryStream);
begin
  // 加一个消息到列表，激活线程
  with TJSONResult(AResult) do
  begin
    FOpCode := AOpCode;
    FMsgType := AMsgType;
    FStream := AStream;
  end;
  FLock.Acquire;
  try
    FResults.Add(AResult);
  finally
    FLock.Release;
  end;
  Activate;  // 激活
end;

procedure TPostThread.AfterWork;
var
  i: Integer;
begin
  // 清除消息
  for i := 0 to FResults.Count - 1 do
    TJSONResult(FResults.PopFirst).Free;
  FLock.Free;
  FResults.Free;
  inherited;
end;

constructor TPostThread.Create(AConnection: TWSConnection);
begin
  inherited Create;
  FreeOnTerminate := True;
  FConnection := AConnection;
  FLock := TThreadLock.Create; // 锁
  FResults := TInList.Create;  // 收到的消息列表
end;

procedure TPostThread.DoMethod;
begin
  // 循环处理收到的消息
  while (Terminated = False) do
  begin
    FLock.Acquire;
    try
      FResult := FResults.PopFirst;  // 取出第一个
    finally
      FLock.Release;
    end;
    if Assigned(FResult) then
      Synchronize(ExecInMainThread) // 进入应用层
    else
      Break;
  end;
end;

procedure TPostThread.ExecInMainThread;
begin
  // 进入主线程，把消息提交给宿主
  try
    try
      if (FResult.FOpCode = ocClose) then
        FConnection.FTimer.Enabled := True
      else
      if Assigned(FResult.FStream) then  // 未封装的数据
        FConnection.HandlePushedData(FResult.FStream)
      else
      if (FResult.FOwner <> FConnection) then  // 推送来的消息
        FConnection.HandlePushedMsg(FResult)
      else
        FConnection.HandleReturnMsg(FResult);  // 服务端反馈给自己的消息
    finally
      if Assigned(FResult.FStream) then  // 释放！
        FResult.FStream.Free;      
      FResult.Free;
    end;
  except
    on E: Exception do
    begin
      FConnection.FErrMsg := E.Message;
      FConnection.FErrorcode := GetLastError;
      FConnection.DoThreadFatalError;  // 在主线程，直接调用
    end;
  end;

end;

// ================== 接收线程 ==================

// 使用 WSARecv 回调函数，效率高
procedure WorkerRoutine(const dwError, cbTransferred: DWORD;
                        const lpOverlapped: POverlapped;
                        const dwFlags: DWORD); stdcall;
var
  Thread: TRecvThread;
  Connection: TWSConnection;
  ByteCount, Flags: DWORD;
  ErrorCode: Cardinal;
begin
  // 不是主线程 ！
  // 传入的 lpOverlapped^.hEvent = TInRecvThread

  Thread := TRecvThread(lpOverlapped^.hEvent);
  Connection := Thread.FConnection;

  if (dwError <> 0) or (cbTransferred = 0) then // 断开或异常
  begin
    // 服务端关闭时 cbTransferred = 0, 要断开连接：2019-02-28
    if (cbTransferred = 0) then
      Thread.Synchronize(Connection.TryDisconnect); // 同步
    Exit;
  end;

  try
    // 处理一个数据包
    Thread.HandleDataPacket;
  finally
    // 继续执行 WSARecv，等待数据
    FillChar(lpOverlapped^, SizeOf(TOverlapped), 0);
    lpOverlapped^.hEvent := DWORD(Thread);  // 传递自己

    ByteCount := 0;
    Flags := 0;

    // 收到数据时执行 WorkerRoutine
    if (iocp_Winsock2.WSARecv(Connection.FSocket, @Thread.FRecvBuf, 1,
                              ByteCount, Flags, LPWSAOVERLAPPED(lpOverlapped),
                              @WorkerRoutine) = SOCKET_ERROR) then
    begin
      ErrorCode := WSAGetLastError;
      if (ErrorCode <> WSA_IO_PENDING) then  
      begin
        Connection.FErrorcode := ErrorCode;
        Thread.Synchronize(Connection.DoThreadFatalError); // 线程同步
      end;
    end;
  end;
end;

{ TRecvThread }

procedure TRecvThread.CheckUpgradeState(Buf: PAnsiChar; Len: Integer);
begin
  // 检查升级结果（简化，可能出现拒绝服务的反馈）
  if not MatchSocketType(Buf, HTTP_VER + HTTP_STATES_100[1]) then
    FConnection.FTimer.Enabled := True;  // 不检查 Accept Key
end;

constructor TRecvThread.Create(AConnection: TWSConnection);
{ var
  i: Integer;    }
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FConnection := AConnection;

  // 分配接收缓存
  GetMem(FRecvBuf.buf, IO_BUFFER_SIZE_2);
  FRecvBuf.len := IO_BUFFER_SIZE_2;

  // 消息接收器，参数传 TResultParams
  FReceiver := TWSClientReceiver.Create(FConnection,
                                        TJSONResult.Create(FConnection));

  FReceiver.OnAttachment := OnAttachment;  // 接收附件方法
  FReceiver.OnPost := FConnection.FPostThread.Add;  // 投放方法
  FReceiver.OnReceive := OnReceive;  // 接收进程

{  FDebug.LoadFromFile('ms.txt');
  FStream.LoadFromFile('recv2.dat');

  for i := 0 to FDebug.Count - 1 do
  begin
    FOverlapped.InternalHigh := StrToInt(FDebug[i]);
    FStream.Read(FRecvBuf.buf^, FOverlapped.InternalHigh);
    HandleDataPacket;
  end;    }

end;

procedure TRecvThread.Execute;
var
  ByteCount, Flags: DWORD;
begin
  // 执行 WSARecv，等待数据

  try
    FillChar(FOverlapped, SizeOf(TOverlapped), 0);
    FOverlapped.hEvent := DWORD(Self);  // 传递自己

    ByteCount := 0;
    Flags := 0;

    // 有数据传入时操作系统自动触发执行 WorkerRoutine
    iocp_Winsock2.WSARecv(FConnection.FSocket, @FRecvBuf, 1,
                          ByteCount, Flags, @FOverlapped, @WorkerRoutine);

    while (Terminated = False) do  // 不断等待
      if (SleepEx(100, True) = WAIT_IO_COMPLETION) then  // 不能用其他等待模式
      begin
        // Empty
      end;
  finally
    FreeMem(FRecvBuf.buf);
    FReceiver.Free;
  end;
  
end;

procedure TRecvThread.HandleDataPacket;
begin
  // 处理接收到的数据包

  // 接收字节总数
  Inc(FConnection.FRecvCount, FOverlapped.InternalHigh);

  if FReceiver.Complete then  // 1. 首包数据
  begin
    if MatchSocketType(FRecvBuf.buf, HTTP_VER) then  // HTTP 消息
      CheckUpgradeState(FRecvBuf.buf, FOverlapped.InternalHigh)
    else begin
//      FDebug.Add(IntToStr(FOverlapped.InternalHigh));
//      FStream.Write(FRecvBuf.buf^, FOverlapped.InternalHigh);
      FReceiver.Prepare(FRecvBuf.buf, FOverlapped.InternalHigh);  // 接收
    end;
  end else
  begin
    // 2. 后续数据
    FReceiver.Receive(FRecvBuf.buf, FOverlapped.InternalHigh);
  end;

end;

procedure TRecvThread.OnAttachment(Result: TBaseJSON);
begin
  // 有附件流，同步，让客户端判断是否接收
  FResult := TJSONResult(Result);
  FResult.FMsgType := mtJSON;
  Synchronize(FConnection.ReceiveAttachment);
end;

procedure TRecvThread.OnReceive(Result: TBaseJSON; FrameSize, RecvSize: Int64);
begin
  // 显示接收进程
  if (Result.MsgId = FMsgId) then
    Inc(FCurrentSize, RecvSize)
  else begin  // 新的 JSON
    FMsgId := Result.MsgId;
    FFrameSize := FrameSize;
    FCurrentSize := RecvSize;
  end;
  Synchronize(FConnection.ShowRecvProgress);  // 切换到主线程
end;

procedure TRecvThread.Stop;
begin
  inherited;
  Sleep(20);
end; 

{ initialization
  FDebug := TStringList.Create;
  FStream := TMemoryStream.Create;

finalization
  FDebug.SaveToFile('ms.txt');
  FStream.SaveToFile('recv2.dat');

  FStream.Free;
  FDebug.Free;   }

end.

