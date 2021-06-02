(*
 * InIOCP 客户端连接、发送线程、接收线程、投放线程 类
 *   TInConnection、TInWSConnection 从 TBaseConnection 继承
 *   TInStreamConnection 未实现
 *)
unit iocp_clientBase;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.ExtCtrls, {$ELSE}
  Windows, Classes, SysUtils, ExtCtrls, {$ENDIF}
  iocp_base, iocp_baseObjs, iocp_Winsock2, iocp_utils, iocp_wsExt,
  iocp_lists, iocp_msgPacks, iocp_WsJSON, iocp_api,
  iocp_senders, iocp_receivers;

type

  // 线程基类
  TBaseSendThread = class;
  TBaseRecvThread = class;
  TBasePostThread = class;

  // 消息收发事件
  TRecvSendEvent = procedure(Sender: TObject; MsgId: TIOCPMsgId;
                             TotalSize, CurrentSize: TFileSize) of object;

  // 异常事件
  TExceptEvent = procedure(Sender: TObject; const Msg: string) of object;

  TBaseConnection = class(TComponent)
  private
    FAutoConnected: Boolean;   // 是否自动连接
    FLocalPath: String;        // 下载文件的本地存放路径
    FURL: String;              // 请求资源
    FServerAddr: String;       // 服务器地址
    FServerPort: Word;         // 服务端口

    FAfterConnect: TNotifyEvent;     // 连接后
    FAfterDisconnect: TNotifyEvent;  // 断开后
    FBeforeConnect: TNotifyEvent;    // 连接前
    FBeforeDisconnect: TNotifyEvent; // 断开前

    FOnDataReceive: TRecvSendEvent;  // 消息接收事件
    FOnDataSend: TRecvSendEvent;     // 消息发出事件
    FOnError: TExceptEvent;          // 异常事件

    function GetActive: Boolean;
    function GetURL: String;

    procedure CreateTimer;
    procedure Disconnect;

    procedure InternalOpen;
    procedure InternalClose;
    procedure SetActive(Value: Boolean);
    procedure TimerEvent(Sender: TObject);
  protected
    FSendThread: TBaseSendThread;  // 发送线程
    FRecvThread: TBaseRecvThread;  // 接收线程
    FPostThread: TBasePostThread;  // 投放线程

    FSocket: TSocket;          // 套接字
    FTimer: TTimer;            // 定时器

    FActive: Boolean;          // 开关/连接状态
    FErrorcode: Integer;       // 异常代码
    FInitFlag: AnsiString;     // 初始化服务端的字符串
    FInMainThread: Boolean;    // 进入主线程
    FRecvCount: Cardinal;      // 收到总数
    FSendCount: Cardinal;      // 发送总数
    procedure Loaded; override;
  protected
    procedure DoClientError;
    procedure DoServerError; virtual; abstract;
    procedure InterBeforeConnect; virtual; abstract;
    procedure InterAfterConnect; virtual; abstract;  // 子类初始化资源
    procedure InterAfterDisconnect; virtual; abstract;  // 子类释放资源
    procedure RecvMsgProgress; virtual;
    procedure SendMsgProgress; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected    
    property URL: String read GetURL write FURL;
  public
    property Errorcode: Integer read FErrorcode;
    property PostThread: TBasePostThread read FPostThread;
    property RecvCount: Cardinal read FRecvCount;
    property SendCount: Cardinal read FSendCount;
    property SendThread: TBaseSendThread read FSendThread;
    property Socket: TSocket read FSocket;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property AutoConnected: Boolean read FAutoConnected write FAutoConnected default False;
    property LocalPath: string read FLocalPath write FLocalPath;
    property ServerAddr: string read FServerAddr write FServerAddr;
    property ServerPort: Word read FServerPort write FServerPort default DEFAULT_SVC_PORT;
  published
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;

    property OnDataReceive: TRecvSendEvent read FOnDataReceive write FOnDataReceive;
    property OnDataSend: TRecvSendEvent read FOnDataSend write FOnDataSend;
    property OnError: TExceptEvent read FOnError write FOnError;
  end;

  // =================== 发送线程 类 ===================

  // 消息编号数组
  TMsgIdArray = array of TIOCPMsgId;

  TBaseSendThread = class(TCycleThread)
  private
    FLock: TThreadLock;        // 线程锁
    FMsgList: TInList;         // 待发消息包列表
    FCancelIds: TMsgIdArray;   // 分块传输时待取消的消息编号数组
    FEventMode: Boolean;       // 等待事件模式
    
    FGetFeedback: Integer;     // 收到服务器反馈
    FWaitState: Integer;       // 等待反馈状态
    FWaitSemaphore: THandle;   // 等待服务器反馈的信号灯

    function ClearList: Integer;
    function GetCount: Integer;
    function GetWork: Boolean;

    procedure AddCancelMsgId(MsgId: TIOCPMsgId);
    procedure ClearCancelMsgId(MsgId: TIOCPMsgId);
  protected
    FConnection: TBaseConnection; // 连接
    FPostThread: TBasePostThread; // 提交线程

    FSender: TClientTaskSender;   // 消息发送器
    FSendMsg: TBasePackObject;    // 当前发出消息

    FSendCount: TFileSize;        // 当前发出数
    FTotalSize: TFileSize;        // 当前消息总长度

    function GetWorkState: Boolean;
    function InCancelArray(MsgId: TIOCPMsgId): Boolean;

    procedure AfterWork; override;
    procedure DoThreadMethod; override;

    procedure IniWaitState;
    procedure KeepWaiting;
    procedure WaitForFeedback;

    procedure OnDataSend(Msg: TBasePackObject; Part: TMessagePart; OutSize: Integer);
    procedure OnSendError(IOType: TIODataType; ErrorCode: Integer);
  protected
    function ChunkRequest(Msg: TBasePackObject): Boolean; virtual;
    procedure InterSendMsg(RecvThread: TBaseRecvThread); virtual; abstract;
  public
    constructor Create(AConnection: TBaseConnection; EventMode: Boolean);
    procedure AddWork(Msg: TBasePackObject); virtual;
    procedure CancelWork(MsgId: TIOCPMsgId);
    procedure ClearAllWorks(var ACount: Integer);
    procedure ServerFeedback(Accept: Boolean = False); virtual;
  public
    property Count: Integer read GetCount;
  end;

  // ================= 推送结果的线程 类 ===============
  // 保存接收到的消息到列表，逐一塞进应用层

  TBasePostThread = class(TCycleThread)
  protected
    FConnection: TBaseConnection;  // 连接
    FSendThread: TBaseSendThread;  // 发送线程
    FLock: TThreadLock;            // 线程锁
    FMsgList: TInList;             // 收到的消息列表
    FSendMsg: TBasePackObject;     // 发送线程的当前消息
    procedure AfterWork; override;
    procedure DoThreadMethod; override;
    procedure SetSendMsg(Msg: TBasePackObject);
  protected
    procedure HandleMessage(Msg: TBasePackObject); virtual; abstract;  // 子类实现
  public
    constructor Create(AConnection: TBaseConnection);
    procedure Add(Msg: TBasePackObject); virtual;
  end;

  // =================== 接收线程 类 ===================

  TBaseRecvThread = class(TThread)
  protected
    FConnection: TBaseConnection; // 连接
    FOverlapped: TOverlapped;     // 重叠结构
    FRecvBuf: TWsaBuf;            // 接收缓存

    FReceiver: TBaseReceiver;     // 数据接收器
    FRecvMsg: TBasePackObject;    // 当前的接收消息

    FRecvCount: TFileSize;        // 当前发出数
    FTotalSize: TFileSize;        // 当前消息总长度
  protected
    procedure Execute; override;
    procedure HandleDataPacket; virtual;  // 接收消息数据
    procedure OnCreateMsgObject(Msg: TBasePackObject);
    procedure OnDataReceive(Msg: TBasePackObject; Part: TMessagePart; RecvCount: Cardinal); virtual;
    procedure OnRecvError(Msg: TBasePackObject; ErrorCode: Integer);
  public
    constructor Create(AConnection: TBaseConnection; AReceiver: TBaseReceiver);
    procedure Stop;
  end;

const
  ERR_SEND_DATA   = -1;    // 客户端：发送异常
  ERR_USER_CANCEL = -2;    // 客户端：用户取消操作
  ERR_NO_ANWSER   = -3;    // 客户端：服务器无应答.
  ERR_CHECK_CODE  = -4;    // 客户端：接收异常.

implementation

{ TBaseConnection }

constructor TBaseConnection.Create(AOwner: TComponent);
begin
  inherited;
  FSocket := INVALID_SOCKET;  // 无效 Socket
  FServerPort := DEFAULT_SVC_PORT;  // 默认端口
end;

procedure TBaseConnection.CreateTimer;
begin
  // 建定时器(关闭连接用）
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 50;
  FTimer.OnTimer := TimerEvent;
end;

destructor TBaseConnection.Destroy;
begin
  SetActive(False);
  inherited;
end;

procedure TBaseConnection.Disconnect;
begin
  // 尝试关闭客户端
  if Assigned(FTimer) then
    FTimer.Enabled := True;
end;

procedure TBaseConnection.DoClientError;
begin
  // 出现致命异常 -> 断开
  try
    if Assigned(OnError) then
      case FErrorCode of
        ERR_SEND_DATA:
          OnError(Self, '发送异常.');
        ERR_USER_CANCEL:
          OnError(Self, '用户取消操作.');
        ERR_NO_ANWSER:
          OnError(Self, '服务器无应答.');
        ERR_CHECK_CODE:
          OnError(Self, '接收数据异常.');        
        else
          OnError(Self, GetWSAErrorMessage(FErrorCode));
      end;
  finally
    Disconnect;  // 自动断开
  end;
end;

function TBaseConnection.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Result := FActive
  else
    Result := (FSocket <> INVALID_SOCKET) and FActive;
end;

function TBaseConnection.GetURL: String;
begin
  if (FURL = '') or (FURL = '/') then
    Result := '/'
  else
  if (FURL[1] <> '/') then
    Result := '/' + FURL
  else
    Result := FURL;
end;

procedure TBaseConnection.InternalClose;
begin
  // 断开连接
  if Assigned(FBeforeDisConnect) and not (csDestroying in ComponentState) then
    FBeforeDisConnect(Self);

  if (FSocket <> INVALID_SOCKET) then
  begin
    // 关闭 Socket
    FActive := False;
    try
      iocp_Winsock2.ShutDown(FSocket, SD_BOTH);
      iocp_Winsock2.CloseSocket(FSocket);
    finally
      FSocket := INVALID_SOCKET;
    end;

    // 断开，释放子类资源
    InterAfterDisconnect;

    // 释放接收线程
    if Assigned(FRecvThread) then
    begin
      FRecvThread.Stop;  // 100 毫秒后退出
      FRecvThread := nil;
    end;

    // 释放投放线程
    if Assigned(FPostThread) then
    begin
      FPostThread.Stop;
      FPostThread := nil;
    end;

    // 释放发送线程
    if Assigned(FSendThread) then
    begin
      FSendThread.ServerFeedback;
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

  if Assigned(FAfterDisconnect) and not (csDestroying in ComponentState) then
    FAfterDisconnect(Self);

end;

procedure TBaseConnection.InternalOpen;
begin
  // 创建 WSASocket，连接到服务器
  if Assigned(FBeforeConnect) and not (csDestroying in ComponentState) then
    FBeforeConnect(Self);

  FActive := False;
  if (FSocket = INVALID_SOCKET) then
  begin
    // 准备 FInitFlag
    InterBeforeConnect;

    // 建 Socket
    FSocket := iocp_Winsock2.WSASocket(AF_INET, SOCK_STREAM,
                                       IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);

    if iocp_utils.ConnectSocket(FSocket, FServerAddr, FServerPort) then  // 连接成功
    begin
      // 定时器
      CreateTimer;

      // 心跳
      iocp_wsExt.SetKeepAlive(FSocket);

      if (FInitFlag <> '') then  // 发送客户端标志 FInitFlag，服务端转换/准备资源
        iocp_Winsock2.Send(FSocket, FInitFlag[1], Length(FInitFlag), 0);

      // 连接，设置资源（提交线程在子类建）
      InterAfterConnect;

      FSendThread.FPostThread := FPostThread;
      FPostThread.FSendThread := FSendThread;

      FActive := True;  // 激活
      FRecvCount := 0;  // 接收数
      FSendCount := 0;  // 发出数
    end else
      try
        iocp_Winsock2.ShutDown(FSocket, SD_BOTH);
        iocp_Winsock2.CloseSocket(FSocket);
      finally
        FSocket := INVALID_SOCKET;
      end;
  end;

  if not (csDestroying in ComponentState) then
    if FActive then
    begin
      if Assigned(FAfterConnect) then
        FAfterConnect(Self);
    end else
    if Assigned(FOnError) then
      FOnError(Self, '无法连接到服务器.');

end;

procedure TBaseConnection.Loaded;
begin
  inherited;
  // 如果设计时 FActive = True，装载后 -> 连接
  if FActive and not (csDesigning in ComponentState) then
    InternalOpen;
end;

procedure TBaseConnection.RecvMsgProgress;
begin
  // 显示接收进程
  if Assigned(FOnDataReceive) then
    try
      FOnDataReceive(Self, FRecvThread.FRecvMsg.MsgId,
                     FRecvThread.FTotalSize, FRecvThread.FRecvCount);
    except
      raise;
    end;
end;

procedure TBaseConnection.SendMsgProgress;
begin
  // 显示发送进程（主体、附件各一次 100%）
  if Assigned(FOnDataSend) then
    try
      FOnDataSend(Self, FSendThread.FSendMsg.MsgId,
                  FSendThread.FTotalSize, FSendThread.FSendCount);
    except
      raise;
    end;
end;

procedure TBaseConnection.SetActive(Value: Boolean);
begin
  if Value <> FActive then
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      FActive := Value
    else
    if Value and not FActive then
      InternalOpen
    else
    if not Value and FActive then
    begin
      if FInMainThread then  // 正在主线程运行
        FTimer.Enabled := True
      else
        InternalClose;
    end;
end;

procedure TBaseConnection.TimerEvent(Sender: TObject);
begin
  FTimer.Enabled := False;
  InternalClose;  // 断开连接
end;

{ TBaseSendThread }

procedure TBaseSendThread.AddCancelMsgId(MsgId: TIOCPMsgId);
var
  i: Integer;
  Exists: Boolean;
begin
  // 加入待取消的消息编号 MsgId
  Exists := False;
  if (FCancelIds <> nil) then
    for i := 0 to High(FCancelIds) do
      if (FCancelIds[i] = MsgId) then
      begin
        Exists := True;
        Break;
      end;
  if (Exists = False) then
  begin
    SetLength(FCancelIds, Length(FCancelIds) + 1);
    FCancelIds[High(FCancelIds)] := MsgId;
  end;
end;

procedure TBaseSendThread.AddWork(Msg: TBasePackObject);
begin
  FLock.Acquire;
  try
    FMsgList.Add(Msg);
  finally
    FLock.Release;
  end;
  Activate;  // 激活线程
end;

procedure TBaseSendThread.AfterWork;
begin
  inherited;
  // 释放资源
  CloseHandle(FWaitSemaphore);
  ClearList;
  FLock.Free;
  FMsgList.Free;
  FSender.Free;
end;

procedure TBaseSendThread.CancelWork(MsgId: TIOCPMsgId);
var
  i, k: Integer;
  Msg: TBasePackObject;
begin
  // 取消发送，编号 = MsgId
  FLock.Acquire;
  try
    if Assigned(FSendMsg) and
      ChunkRequest(FSendMsg) and (FSendMsg.MsgId = MsgId) then
    begin
      FSender.Stoped := True;
      ServerFeedback;
    end else
    begin
      k := FMsgList.Count;
      for i := 0 to k - 1 do
      begin
        Msg := TBasePackObject(FMsgList.PopFirst);
        if (Msg.MsgId = MsgId) then
          Msg.Free
        else  // 从新加入
          FMsgList.Add(Msg);
      end;
      if (k = FMsgList.Count) then  // 可能是续传
        AddCancelMsgId(MsgId);
    end;
  finally
    FLock.Release;
  end;
end;

function TBaseSendThread.ChunkRequest(Msg: TBasePackObject): Boolean;
begin
  Result := False;
end;

procedure TBaseSendThread.ClearAllWorks(var ACount: Integer);
begin
  // 清空待发消息
  FLock.Acquire;
  try
    ACount := ClearList;  // 取消总数
  finally
    FLock.Release;
  end;
end;

procedure TBaseSendThread.ClearCancelMsgId(MsgId: TIOCPMsgId);
var
  i: Integer;
begin
  // 清除数组内的消息编号 MsgId
  if (FCancelIds <> nil) then
    for i := 0 to High(FCancelIds) do
      if (FCancelIds[i] = MsgId) then
      begin
        FCancelIds[i] := 0;  // 清除
        Break;
      end;
end;

function TBaseSendThread.ClearList: Integer;
var
  i: Integer;
begin
  // 释放列表的全部消息（在外加锁）
  Result := FMsgList.Count;
  for i := 0 to Result - 1 do
    TBasePackObject(FMsgList.PopFirst).Free;
end;

constructor TBaseSendThread.Create(AConnection: TBaseConnection; EventMode: Boolean);
begin
  inherited Create(True);  // 内置信号灯
  FConnection := AConnection;
  FEventMode := EventMode;

  FLock := TThreadLock.Create; // 锁
  FMsgList := TInList.Create;  // 待发任务表
  FSender := TClientTaskSender.Create;   // 任务发送器

  FSender.Socket := FConnection.FSocket; // 发送套接字
  FSender.OnDataSend := OnDataSend;      // 发出事件
  FSender.OnError := OnSendError;        // 发出异常事件

  // 信号灯
  FWaitSemaphore := CreateSemaphore(Nil, 0, 1, Nil);
end;

procedure TBaseSendThread.DoThreadMethod;
  function NoServerFeedback: Boolean;
  begin
    {$IFDEF ANDROID_MODE}
    FLock.Acquire;
    try
      Dec(FGetFeedback);
      Result := (FGetFeedback < 0);
    finally
      FLock.Release;
    end;
    {$ELSE}
    Result := (InterlockedDecrement(FGetFeedback) < 0);
    {$ENDIF}
  end;
begin
  // 取发送任务 -> 发送
  while not Terminated and FConnection.FActive and GetWork() do
    try
      try
        FPostThread.SetSendMsg(FSendMsg);
        InterSendMsg(FConnection.FRecvThread);  // 在子类发送
      finally
        FLock.Acquire;
        try
          FreeAndNil(FSendMsg);  // 不要在子类释放
        finally
          FLock.Release;
        end;
        if FEventMode and NoServerFeedback() then // 等待事件模式, 服务器无应答
        begin
          FConnection.FErrorcode := ERR_NO_ANWSER;
          Synchronize(FConnection.DoClientError);
        end;
      end;
    except
      on E: Exception do
      begin
        FConnection.FErrorcode := GetLastError;
        Synchronize(FConnection.DoClientError);
      end;
    end;
end;

function TBaseSendThread.GetCount: Integer;
begin
  // 取任务数
  FLock.Acquire;
  try
    Result := FMsgList.Count;
  finally
    FLock.Release;
  end;
end;

function TBaseSendThread.GetWork: Boolean;
begin
  // 从列表中取一个消息
  FLock.Acquire;
  try
    if Terminated or (FMsgList.Count = 0) then
    begin
      FSendMsg := nil;
      Result := False;
    end else
    begin
      FSendMsg := TBasePackObject(FMsgList.PopFirst);  // 取任务
      FSender.Stoped := False;
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

function TBaseSendThread.GetWorkState: Boolean;
begin
  // 取工作状态：线程、发送器未停止
  FLock.Acquire;
  try
    Result := (Terminated = False) and (FSender.Stoped = False);
  finally
    FLock.Release;
  end;
end;

function TBaseSendThread.InCancelArray(MsgId: TIOCPMsgId): Boolean;
var
  i: Integer;
begin
  // 检查是否要停止
  FLock.Acquire;
  try
    Result := False;
    if (FCancelIds <> nil) then
      for i := 0 to High(FCancelIds) do
        if (FCancelIds[i] = MsgId) then
        begin
          Result := True;
          Break;
        end;
  finally
    FLock.Release;
  end;
end;

procedure TBaseSendThread.IniWaitState;
begin
  // 初始化等待状态
  {$IFDEF ANDROID_MODE}
  FLock.Acquire;
  try
    FGetFeedback := 0;
    FWaitState := 0;
  finally
    FLock.Release;
  end;
  {$ELSE}
  InterlockedExchange(FGetFeedback, 0); // 未收到反馈
  InterlockedExchange(FWaitState, 0); // 状态=0
  {$ENDIF}
end;

procedure TBaseSendThread.KeepWaiting;
begin
  // 继续等待: FWaitState = 1 -> +1
  {$IFDEF ANDROID_MODE}
  FLock.Acquire;
  try
    Inc(FGetFeedback);
    if (FWaitState = 2) then
    begin
      FWaitState := 1;
      ReleaseSemaphore(FWaitSemaphore, 1, Nil);  // 触发
    end;
  finally
    FLock.Release;
  end;
  {$ELSE}
  InterlockedIncrement(FGetFeedback); // 收到反馈
  if (iocp_api.InterlockedCompareExchange(FWaitState, 2, 1) = 1) then  // 状态+
    ReleaseSemaphore(FWaitSemaphore, 1, Nil);  // 触发
  {$ENDIF}
end;

procedure TBaseSendThread.OnDataSend(Msg: TBasePackObject; Part: TMessagePart; OutSize: Integer);
begin
  // 显示发送进程
  Inc(FSendCount, OutSize);
  Synchronize(FConnection.SendMsgProgress);
end;

procedure TBaseSendThread.OnSendError(IOType: TIODataType; ErrorCode: Integer);
begin
  // 处理发送异常
  if (GetWorkState = False) then  
    ServerFeedback;  // 忽略等待
  FConnection.FErrorcode := ErrorCode;
  Synchronize(FConnection.DoClientError); // 线程同步
end;

procedure TBaseSendThread.ServerFeedback(Accept: Boolean);
begin
  // 服务器反馈 或 忽略等待
  //  1. 取消任务后收到反馈
  //  2. 收到反馈，而未等待
  //   （多线程的调度执行先后不确定，可能已经反馈，但未执行到等待命令）
  {$IFDEF ANDROID_MODE}
  FLock.Acquire;
  try
    Inc(FGetFeedback);    // +1
    Dec(FWaitState);      // 0->-1
    if (FWaitState = 0) then
      ReleaseSemaphore(FWaitSemaphore, 1, Nil);  // 信号量+1
  finally
    FLock.Release;
  end;
  {$ELSE}
  InterlockedIncrement(FGetFeedback); // 收到反馈 +
  if (InterlockedDecrement(FWaitState) = 0) then
    ReleaseSemaphore(FWaitSemaphore, 1, Nil);  // 信号量+1
  {$ENDIF}
end;

procedure TBaseSendThread.WaitForFeedback;
  {$IFDEF ANDROID_MODE}
  function LockedGetWaitState(IncOper: Boolean): Integer;
  begin
    FLock.Acquire;
    try
      if IncOper then  // +
        Inc(FWaitState)  // 1: 未反馈，等待; 0：已经收到反馈
      else  // -
        Dec(FWaitState);
      Result := FWaitState;
    finally
      FLock.Release;
    end;
  end;
  {$ENDIF}
begin
  // 等服务器反馈，等 WAIT_MILLISECONDS 毫秒
  {$IFDEF ANDROID_MODE}
  if (LockedGetWaitState(True) = 1) then
    repeat
      WaitForSingleObject(FWaitSemaphore, WAIT_MILLISECONDS);
    until (LockedGetWaitState(False) <= 0);
  {$ELSE}
  if (InterlockedIncrement(FWaitState) = 1) then
    repeat
      WaitForSingleObject(FWaitSemaphore, WAIT_MILLISECONDS);
    until (InterlockedDecrement(FWaitState) <= 0);
  {$ENDIF}
end;

{ TBasePostThread }

procedure TBasePostThread.Add(Msg: TBasePackObject);
begin
  // 加入消息到列表
  FLock.Acquire;
  try
    FMsgList.Add(Msg);
  finally
    FLock.Release;
  end;
  Activate;  // 激活
end;

procedure TBasePostThread.AfterWork;
var
  i: Integer;
begin
  // 清除消息
  for i := 0 to FMsgList.Count - 1 do
    TBasePackObject(FMsgList.PopFirst).Free;
  FLock.Free;
  FMsgList.Free;
  inherited;
end;

constructor TBasePostThread.Create(AConnection: TBaseConnection);
begin
  inherited Create(True);      // 内置信号灯
  FConnection := AConnection;
  FLock := TThreadLock.Create; // 锁
  FMsgList := TInList.Create;  // 收到的消息列表
end;

procedure TBasePostThread.DoThreadMethod;
var
  Msg: TBasePackObject;
begin
  // 循环处理收到的消息
  while (Terminated = False) do
  begin
    FLock.Acquire;
    try
      Msg := TBasePackObject(FMsgList.PopFirst);  // 取出第一个
    finally
      FLock.Release;
    end;
    if not Assigned(Msg) then  // 跳出
      Break;
    if FSendThread.InCancelArray(Msg.MsgId) then  // 任务被用户取消
    begin
      Msg.Free;
      FSendThread.ServerFeedback;
    end else
      HandleMessage(Msg);  // 进入业务层
  end;
end;

procedure TBasePostThread.SetSendMsg(Msg: TBasePackObject);
begin
  FLock.Acquire;
  try
    FSendMsg := Msg;
  finally
    FLock.Release;
  end;
end;

{ TBaseRecvThread }

// 用 WSARecv 回调函数接收数据，效率高

procedure WorkerRoutine(const dwError, cbTransferred: DWORD;
  const lpOverlapped: POverlapped; const dwFlags: DWORD); stdcall;
var
  Thread: TBaseRecvThread;
  Connection: TBaseConnection;
  ByteCount, Flags: DWORD;
  ErrorCode: Cardinal;
begin
  // 不是主线程 ！
  // 传入的 lpOverlapped^.hEvent = TRecvThread

  if (dwError <> 0) then       // 出现异常，如关闭、断开
    lpOverlapped^.hEvent := 0  // 设为无效 -> 连接即将断开
  else
  if (cbTransferred > 0) then
  begin
    // 有数据
    // HTTP.SYS-WebSocket会无数据
    Thread := TBaseRecvThread(lpOverlapped^.hEvent);
    Connection := Thread.FConnection;

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
          ByteCount, Flags, LPWSAOVERLAPPED(lpOverlapped), @WorkerRoutine) = SOCKET_ERROR) then
      begin
        ErrorCode := WSAGetLastError;
        if (ErrorCode <> WSA_IO_PENDING) then
        begin
          Connection.FErrorcode := ErrorCode;
          Thread.Synchronize(Connection.DoClientError); // 线程同步
        end;
      end;
    end;
  end;
end;

constructor TBaseRecvThread.Create(AConnection: TBaseConnection; AReceiver: TBaseReceiver);
begin
  inherited Create(True);
  FConnection := AConnection;
  FReceiver := AReceiver;

  // 分配接收缓存（不能在线程 Execute 中分配）
  GetMem(FRecvBuf.buf, IO_BUFFER_SIZE_2);
  FRecvBuf.len := IO_BUFFER_SIZE_2;
  
  FreeOnTerminate := True;
end;

procedure TBaseRecvThread.Execute;
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

    while not Terminated and (FOverlapped.hEvent > 0) do
      if (SleepEx(80, True) = WAIT_IO_COMPLETION) then  // 不能用其他等待模式
        { Empty } ;
  finally
    if FConnection.FActive and (FOverlapped.hEvent = 0) then
      Synchronize(FConnection.Disconnect);  // 断开
    FreeMem(FRecvBuf.buf);  // 释放资源
    FreeAndNil(FReceiver);
  end;
end;

procedure TBaseRecvThread.HandleDataPacket;
begin
  // 接收字节总数
  Inc(FConnection.FRecvCount, FOverlapped.InternalHigh);
end;

procedure TBaseRecvThread.OnCreateMsgObject(Msg: TBasePackObject);
begin
  // 准备新的消息
  FRecvMsg := Msg;
  FRecvCount := 0;
  FTotalSize := 0;
end;

procedure TBaseRecvThread.OnDataReceive(Msg: TBasePackObject; Part: TMessagePart; RecvCount: Cardinal);
begin
  Synchronize(FConnection.RecvMsgProgress);
end;

procedure TBaseRecvThread.OnRecvError(Msg: TBasePackObject; ErrorCode: Integer);
begin
  // 接收校验异常
  FConnection.FErrorcode := ErrorCode;
  Synchronize(FConnection.DoClientError);
end;

procedure TBaseRecvThread.Stop;
begin
  if Assigned(FReceiver) then  // 服务器关闭时，接收线程已经停止
    Terminate;
  while Assigned(FReceiver) do
    Sleep(10);
end;

end.
