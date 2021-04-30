(*
 * iocp 关闭线程、业务线程、推送线程等
 *)
unit iocp_threads;

interface

{$I in_iocp.inc}        // 模式设置

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, Winapi.ActiveX, {$ELSE}
  Windows, Classes, SysUtils, ActiveX, {$ENDIF}
  iocp_base, iocp_lists, iocp_objPools,
  iocp_winSock2, iocp_baseObjs, iocp_sockets,
  iocp_managers, iocp_msgPacks, iocp_senders;

type

  // ===================== 专门关闭 Socket 的线程 =====================

  TCloseSocketThread = class(TCycleThread)
  private
    FLock: TThreadLock;   // 锁
    FSockets: TInList;    // 可以删首节点的列表
  protected
    procedure AfterWork; override;
    procedure DoThreadMethod; override;
  public
    constructor Create;
    procedure AddSocket(const Socket: TBaseSocket);
    procedure WaitFor;    // 覆盖
  end;

  // ===================== 执行业务的线程 =====================

  TBusiWorkManager = class;

  TBusiThread = class(TCycleThread)
  private
    FManager: TBusiWorkManager; // 管理器
    FSender: TServerTaskSender; // 任务发送器（引用）
    FSocket: TBaseSocket;       // 当前套接字对象
    FWorker: TBusiWorker;       // 任务执行者（引用）
  protected
    procedure AfterWork; override;
    procedure DoThreadMethod; override;
  public
    constructor Create(AManager: TBusiWorkManager);
  end;

  // ===================== 业务线程调度管理 =====================

  TBusiWorkManager = class(TObject)
  private
    FServer: TObject;            // TInIOCPServer
    FSemaphore: THandle;         // 线程共享的信号灯

    // 线程数组
    FThreads: Array of TBusiThread;

    FThreadCount: Integer;       // 线程数
    FLock: TThreadLock;          // 管理器锁

    FSockets: TInList;           // 可以删首节点的主列表（可能很大）
    FBackSockets: TInList;       // 备用列表（可能很大，增删数模时用）
    FCurrentList: TInList;       // 引用 FSockets/FBackSockets

    FActiveCount: Integer;       // 类别任务数
    FActiveThreadCount: Integer; // 活动线程数
    FWorkTotalCount: IOCP_LARGE_INTEGER;  // 已执行的任务总数
    
    function CreateWorker(Index: Integer): TBusiWorker;
    function GetDataModuleState(Index: Integer): Boolean;
    function GetWork(var Socket: TObject): Boolean;

    procedure InternalAddRemove(Index: Integer; AddMode: Boolean);
    procedure InternalStop;
  public
    constructor Create(AServer: TObject; AThreadCount: Integer);
    destructor Destroy; override;
    procedure AddWork(Socket: TObject; Activate: Boolean = True);
    procedure AddDataModule(Index: Integer);
    procedure RemoveDataModule(Index: Integer);
    procedure StopThreads;
    procedure WaitFor;
  public
    property ActiveCount: Integer read FActiveCount;
    property ActiveThreadCount: Integer read FActiveThreadCount;
    property DataModuleState[Index: Integer]: Boolean read GetDataModuleState;
    property WorkTotalCount: IOCP_LARGE_INTEGER read FWorkTotalCount;
  end;

  // ===================== 推送对象/任务 =====================

  TPushMessage = class(TInList)
  private
    FObjPool: TIOCPSocketPool;  // 对象池
    FBufPool: TIODataPool;      // 内存池
    FPushBuf: PPerIOData;       // 待推送消息
    FCastType: TBroadcastType;  // 广播目的类型
    FGroup: String;             // 客户端分组
    FIngoreSocket: TObject;     // 忽略广播的 TIOCPSocketPool
    FClientCount: Integer;      // 估计广播目的数
    FTickCount: Cardinal;       // 加入时间毫秒
  public
    constructor Create(Msg: PPerIOData; BroadcastType: TBroadcastType = btUnknown); overload;
    constructor Create(ASocketPool: TIOCPSocketPool; ABufferPool: TIODataPool;
                       BroadcastType: TBroadcastType = btUnknown; MsgSize: Cardinal = 0); overload;
    constructor Create(Socket: TBaseSocket; IOKind: TIODataType; MsgSize: Cardinal); overload;
    destructor Destroy; override;
  public
    procedure UpdateMsgId;
    procedure WriteWebSocketMsg(OpCode: TWSOpCode; Buffer: PAnsiChar; Size: Integer);
  public    
    property IngoreSocket: TObject read FIngoreSocket write FIngoreSocket;
    property PushBuf: PPerIOData read FPushBuf;
    property Group: String read FGroup write FGroup;
  end;

  // ===================== 消息推送线程 =====================

  TPushMsgManager = class;

  TPushThread = class(TCycleThread)
  private
    FBusiManager: TBusiWorkManager;  // 业务管理器
    FPushManager: TPushMsgManager;   // 推送管理器
    FMsg: TPushMessage;         // 当前待推送消息
    FSocket: TBaseSocket;       // 当前套接字对象
    procedure PushMesssage;
  protected
    procedure AfterWork; override;
    procedure DoThreadMethod; override;
  public
    constructor Create(APushManager: TPushMsgManager; ABusiManager: TBusiWorkManager);
  end;

  // ===================== 推送消息缓冲池 =====================
  // 1. 推送时在业务线程，Socket 资源被占用，如果直接投放时
  //    会引起推送线程争抢不到资源，做大量无用功。
  // 2. 先把消息投放到缓冲池，每隔一定时间加入推送队列。
  
  TMsgPushPool = class(TBaseThread)
  private
    FLock: TThreadLock;          // 消息锁
    FManager: TPushMsgManager;   // 管理器
    FMsgList: TInList;           // 推送消息池
    procedure InterAdd(Msg: TPushMessage);
  protected
    procedure ExecuteWork; override;
  public
    constructor Create(AManager: TPushMsgManager);
    destructor Destroy; override;
  end;

  // ===================== 消息推送线程管理 =====================

  TPushMsgManager = class(TObject)
  private
    FSemaphore: THandle;         // 线程信号灯
    FWaitSemaphore: THandle;     // 线程等待信号灯

    // 线程数组
    FThreads: array of TPushThread;
    FThreadCount: Integer;       // 线程数
    FLock: TThreadLock;          // 管理器锁

    FPushPool: TMsgPushPool;     // 推送消息池
    FMsgList: TInList;           // 推送消息列表

    FActiveCount: Integer;       // 推送列表任务数
    FActiveThreadCount: Integer; // 活动的推送线程数
    FPushMsgCount: Integer;      // 消息总数（估算）

    // 速度控制
    FMaxPushCount: Integer;      // 允许每秒最大推送消息数
    FNowTickCount: Cardinal;     // 当前毫秒时间
    FTickCount: Cardinal;        // 旧的毫秒时间
    FPushCountPS: Integer;       // 每秒加入的消息数

    function GetWork(var Msg: TPushMessage): Boolean;
    procedure ActivateThreads;
    procedure InterAdd(Msg: TPushMessage);
  public
    constructor Create(ABusiManger: TBusiWorkManager; AThreadCount, AMaxPushCount: Integer);
    destructor Destroy; override;
    function AddWork(Msg: TPushMessage): Boolean;
    procedure StopThreads;
    procedure WaitFor;
  public
    property ActiveCount: Integer read FActiveCount;
    property ActiveThreadCount: Integer read FActiveThreadCount;
    property PushMsgCount: Integer read FPushMsgCount;
  end;

implementation

uses
  iocp_api, iocp_Server,
  iocp_log, iocp_utils, http_objects;

type
  TBaseSocketRef = class(TBaseSocket);

{ TCloseSocketThread }

procedure TCloseSocketThread.AddSocket(const Socket: TBaseSocket);
begin
  // 加入要关闭的 Socket
  FLock.Acquire;
  try
    FSockets.Add(Socket);
  finally
    FLock.Release;
  end;
  Activate;  // 激活
end;

procedure TCloseSocketThread.AfterWork;
begin
  FLock.Free;
  FSockets.Free;  // 释放节点空间即可, 对象由池释放
end;

constructor TCloseSocketThread.Create;
begin
  inherited Create;
  FLock := TThreadLock.Create;
  FSockets := TInList.Create;
end;

procedure TCloseSocketThread.DoThreadMethod;
var
  Socket: TBaseSocket;
begin
  while (Terminated = False) do
  begin
    FLock.Acquire;
    try
      Socket := FSockets.PopFirst;  // 取第一个并删除
      if (Socket = nil) then
        Exit;
    finally
      FLock.Release;
    end;
    try
      Socket.Close;  // 释放资源
    finally
      Socket.ObjPool.Push(Socket.LinkNode);  // 压入池，下次不用新建
    end;
  end;
end;

procedure TCloseSocketThread.WaitFor;
begin
  // 等待全部 Socket 关闭完毕
  while (FSockets.Count > 0) do
    Sleep(10);
end;

{ TBusiThread }

procedure TBusiThread.AfterWork;
begin
  // 启用线程数 -1
  InterlockedDecrement(FManager.FThreadCount);
  FSender.FreeBuffers(TInIOCPServer(FManager.FServer).IODataPool);  // 释放发送缓存
  FSender.Free;
end;

constructor TBusiThread.Create(AManager: TBusiWorkManager);
begin
  inherited Create(False);  // 引用信号灯
  FManager := AManager;
  FSemaphore := FManager.FSemaphore;  // 引用
  FSender := TServerTaskSender.Create(TInIOCPServer(FManager.FServer).IODataPool);
end;

procedure TBusiThread.DoThreadMethod;
begin
  // 取列表第一个 Socket，执行
  while (Terminated = False) and
         FManager.GetWork(TObject(FSocket)) do
    case TBaseSocketRef(FSocket).Lock(False) of  // 加锁，非推送模式

      SOCKET_LOCK_OK: begin  // 加锁成功
        InterlockedIncrement(FManager.FActiveThreadCount);  // 活动业务线程+
        try
          TBaseSocketRef(FSocket).DoWork(FWorker, FSender)  // 传递 FWorker, FSender
        finally
          InterlockedDecrement(FManager.FActiveThreadCount); // 活动业务线程-
          {$IFDEF WIN_64}
          System.AtomicIncrement(FManager.FWorkTotalCount);  // 执行总数+
          {$ELSE}
          InterlockedIncrement(FManager.FWorkTotalCount);  // 执行总数+
          {$ENDIF}
        end;
      end;

      SOCKET_LOCK_FAIL:  // 加锁不成功，重新加入列表，等下次！
        FManager.AddWork(FSocket, False);  // 不激活

      else
        { 已经关闭, 放弃这个节点的 Socket } ;
    end;
end;

{ TBusiWorkManager }

procedure TBusiWorkManager.AddDataModule(Index: Integer);
begin
  // 新建编号为 Index 的数模（要先注册数模）
  InternalAddRemove(Index, True);
end;

procedure TBusiWorkManager.AddWork(Socket: TObject; Activate: Boolean);
begin
  // 工作线程调用！

  // 1. 加入一个任务
  FLock.Acquire;
  try
    FCurrentList.Add(Socket);
    if (FCurrentList = FSockets) then  // 未改变列表
      FActiveCount := FCurrentList.Count;
  finally
    FLock.Release;
  end;

  // 2. 激活业务线程，让其取任务
  if Activate and (FCurrentList = FSockets) then  // 是主列表
    ReleaseSemapHore(FSemaphore, 8, Nil);

end;

constructor TBusiWorkManager.Create(AServer: TObject; AThreadCount: Integer);
var
  i: Integer;
  Thread: TBusiThread;
begin
  inherited Create;

  // 每个工作线程对应一个或多个业务线程（1:n）

  // 1. 共享信号灯：最大值 = MaxInt
  FSemaphore := CreateSemapHore(Nil, 0, MaxInt, Nil);

  // 2. 服务器、业务线程数
  FServer := AServer;
  FThreadCount := AThreadCount;

  SetLength(FThreads, FThreadCount);

  // 3. 锁、Socket 列表
  FLock := TThreadLock.Create;

  FSockets := TInList.Create;        // 主列表
  FBackSockets := TInList.Create;    // 备用列表
  FCurrentList := FSockets;          // 引用主列表

  // 4. 建业务线程、任务执行者
  for i := 0 to FThreadCount - 1 do
  begin
    Thread := TBusiThread.Create(Self);
    Thread.FWorker := CreateWorker(i);
    FThreads[i] := Thread;
    Thread.Resume;
  end;
end;

function TBusiWorkManager.CreateWorker(Index: Integer): TBusiWorker;
begin
  // 为工作线程建一个业务工作者
  //   数模由 DatabaseManager 管理，在 TBusiThread.FWorker 释放
  Result := TBusiWorker.Create(FServer, Index);
  Result.CreateDataModules;  // 建实例
end;

destructor TBusiWorkManager.Destroy;
begin
  InternalStop;  // 要先等待执行完毕
  inherited;
end;

function TBusiWorkManager.GetDataModuleState(Index: Integer): Boolean;
begin
  // 返回数模是否有实例
  Result := TBusiWorker(FThreads[0].FWorker).DataModules[Index] <> nil;
end;

function TBusiWorkManager.GetWork(var Socket: TObject): Boolean;
begin
  // 从主任务表弹出第一个任务
  FLock.Acquire;
  try
    Socket := FSockets.PopFirst;
    FActiveCount := FSockets.Count;
    Result := (Socket <> nil);
  finally
    FLock.Release;
  end;
end;

procedure TBusiWorkManager.InternalAddRemove(Index: Integer; AddMode: Boolean);
  procedure StartBackSocketList;
  begin
    // 启用备用任务列表存储任务
    //  （AddWork 不触发业务线程）
    FLock.Acquire;
    try
      if (FCurrentList = FSockets) then
        FCurrentList := FBackSockets
      else
        FCurrentList := FSockets;
    finally
      FLock.Release;
    end;
  end;
  procedure SetMainSocketList;
  begin
    // 主、备份任务列表互换
    //  （AddWork 触发业务线程，GetWork 从新的 FSockets 取任务）
    FLock.Acquire;
    try
      FCurrentList := FBackSockets;  // 当前引用备份
      FBackSockets := FSockets;      // 主 -> 备份
      FSockets := FCurrentList;      // 备份 -> 主
    finally
      FLock.Release;
    end;
  end;
var
  i: Integer;
begin
  // 删除、新建编号为 Index 的数模实例
  StartBackSocketList;
  try
    while (FActiveThreadCount > 0) do  // 等活动线程执行完毕
      Sleep(10);
    for i := 0 to FThreadCount - 1 do  // 增/删每执行者的对应数模实例
      if AddMode then
        TBusiWorker(FThreads[i].FWorker).AddDataModule(Index)
      else
        TBusiWorker(FThreads[i].FWorker).RemoveDataModule(Index);
  finally
    SetMainSocketList;
  end;
end;

procedure TBusiWorkManager.InternalStop;
var
  i: Integer;
begin
  if Assigned(FLock) then
  begin
    for i := 0 to FThreadCount - 1 do // 停止线程
    begin
      FThreads[i].FWorker.Free;  // FWorker 自动释放数模
      FThreads[i].Stop;
    end;

    while (FThreadCount > 0) do  // 等待全部线程退出
    begin
      ReleaseSemapHore(FSemaphore, 1, Nil);
      Sleep(10);
    end;

    SetLength(FThreads, 0);
    CloseHandle(FSemaphore);  // 关闭
    
    FLock.Free;
    FSockets.Free;
    FBackSockets.Free;

    FLock := Nil;
    FSockets := nil;
    FBackSockets := nil;
  end;
end;

procedure TBusiWorkManager.RemoveDataModule(Index: Integer);
begin
  // 删除编号为 Index 的数模实例（不改变数组 FThreads 长度）
  InternalAddRemove(Index, False);
end;

procedure TBusiWorkManager.StopThreads;
begin
  InternalStop;
end;

procedure TBusiWorkManager.WaitFor;
begin
  // 等待活动线程结束
  while (FActiveThreadCount > 0) do
    Sleep(10);
end;

{ TPushMessage }

constructor TPushMessage.Create(Msg: PPerIOData; BroadcastType: TBroadcastType);
begin
  inherited Create;
  // 发送消息个列表的客户端，或广播
  FCastType := BroadcastType;
  FTickCount := GetTickCount;  // 当前时间
  FObjPool := TBaseSocket(Msg^.Owner).ObjPool;
  FBufPool := TBaseSocket(Msg^.Owner).BufferPool;

  FPushBuf := FBufPool.Pop^.Data;

  FPushBuf^.IOType := ioPush; // 推送
  FPushBuf^.Data.len := Msg^.Overlapped.InternalHigh; // 消息大小
  FPushBuf^.Overlapped.InternalHigh := FPushBuf^.Data.len; // 消息大小

  // 复制 Msg
  System.Move(Msg^.Data.buf^, FPushBuf^.Data.buf^, FPushBuf^.Data.len);
end;

constructor TPushMessage.Create(ASocketPool: TIOCPSocketPool;
  ABufferPool: TIODataPool; BroadcastType: TBroadcastType; MsgSize: Cardinal);
begin
  inherited Create;
  // 广播一条消息，在外部设置 FPushBuf 内容
  FCastType := BroadcastType;
  FTickCount := GetTickCount;  // 当前时间
  FObjPool := ASocketPool;
  FBufPool := ABufferPool;

  FPushBuf := FBufPool.Pop^.Data;

  FPushBuf^.IOType := ioPush; // 固定 = ioPush
  FPushBuf^.Data.len := MsgSize;  // 内容长度
  FPushBuf^.Overlapped.InternalHigh := MsgSize;
end;

constructor TPushMessage.Create(Socket: TBaseSocket; IOKind: TIODataType; MsgSize: Cardinal);
begin
  inherited Create;
  // 建一条给 Socket 的 IOKind 类型消息，由外部构建消息内容
  FCastType := btUnknown;  // 不是广播
  FTickCount := GetTickCount;  // 当前时间
  FObjPool := Socket.ObjPool;
  FBufPool := Socket.BufferPool;

  FPushBuf := FBufPool.Pop^.Data;

  FPushBuf^.IOType := IOKind; // 类型
  FPushBuf^.Data.len := MsgSize;  // 内容长度
  FPushBuf^.Overlapped.InternalHigh := MsgSize;

  inherited Add(Socket); // 只有一个节点
end;

destructor TPushMessage.Destroy;
begin
  FBufPool.Push(FPushBuf^.Node);
  inherited;
end;

procedure TPushMessage.UpdateMsgId;
var
  MsgHead: PMsgHead;
begin
  // 使用新的 MsgId
  MsgHead := PMsgHead(FPushBuf^.Data.buf + IOCP_SOCKET_FLEN);
  MsgHead^.MsgId := TSystemGlobalLock.GetMsgId;
end;

procedure TPushMessage.WriteWebSocketMsg(OpCode: TWSOpCode; Buffer: PAnsiChar; Size: Integer);
var
  Data: PWsaBuf;
begin
  // 写 WebSocket 协议消息
  Data := @(FPushBuf^.Data);
  MakeFrameHeader(Data, OpCode, Size);  // 构建帧，操作：OpCode，长度：Size
  System.Move(Buffer^, (Data^.buf + Data^.len)^, Size);  // 加入 Buffer
  Inc(Data^.len, Size);
end;

{ TPushThread }

procedure TPushThread.AfterWork;
begin
  // 启用线程数 -1
  InterlockedDecrement(FPushManager.FThreadCount);
end;

constructor TPushThread.Create(APushManager: TPushMsgManager; ABusiManager: TBusiWorkManager);
begin
  inherited Create(False);  // 引用信号灯
  FBusiManager := ABusiManager;
  FPushManager := APushManager;
  FSemaphore := APushManager.FSemaphore;  // 引用
end;

procedure TPushThread.DoThreadMethod;
var
  i: Integer;
  Trigger: Boolean;
begin
  // 推送 TPushMessage.FPushBuf 给 TPushMessage 的列表用户
  //   只要没关闭 Socket，一定要发送一次

  Trigger := False;
  while (Terminated = False) and FPushManager.GetWork(FMsg) do
  begin
    // 1. 广播，建在线客户端列表
    if (FMsg.FCastType > btUnknown) then
    begin
      FMsg.FObjPool.GetSockets(FMsg, FMsg.FIngoreSocket,
                               FMsg.FCastType = btAdminOnly, FMsg.Group);
      FMsg.FCastType := btUnknown;  // 只取在此时间点的客户端
    end;

    // 2. 逐一推送
    for i := 1 to FMsg.Count do
    begin
      FSocket := FMsg.PopFirst;  // 待发目的
      FMsg.FPushBuf^.Owner := FSocket;
      if FSocket.Active then
        PushMesssage;
    end;

    // 给多点机会业务线程抢 Socket 资源，等一下
    WaitForSingleObject(FPushManager.FWaitSemaphore, 10);

    // 3. 处理结果
    if (FMsg.Count > 0) then // 未全部发出
    begin
      Trigger := True;
      FPushManager.InterAdd(FMsg);  // 再加入
      Break;  // 下次继续
    end else
    begin
      Trigger := False;
      FMsg.Free;  // 全部发出，释放
    end;
  end;

  if Trigger then
    FPushManager.ActivateThreads;
    
end;

procedure TPushThread.PushMesssage;
begin
  // 推送 FMsg.FPushBuf 给 FSocket
  case TBaseSocketRef(FSocket).Lock(True) of

    SOCKET_LOCK_OK: begin  // 加锁成功
      // 工作量统计到 FTotalCount
      InterlockedIncrement(FPushManager.FActiveThreadCount); // 活动推送线程+
      try
        TBaseSocketRef(FSocket).InternalPush(FMsg.FPushBuf); // 推送
      finally
        InterlockedDecrement(FPushManager.FActiveThreadCount); // 活动业务线程-
        {$IFDEF WIN_64}
        System.AtomicIncrement(FBusiManager.FWorkTotalCount);  // 执行总数+
        {$ELSE}
        InterlockedIncrement(FBusiManager.FWorkTotalCount);  // 执行总数+
        {$ENDIF}
      end;
    end;

    SOCKET_LOCK_FAIL:  // 不成功
      // 可能 Socket 被关闭且再次从池取出，此时不是原来的客户端了，
      // 但理论上也是一个需广播的客户端，可以接收广播消息。
      FMsg.Add(FSocket);  // 重新加入，等下次！

    else
      { 已经关闭, 放弃这个节点的 Socket } ;

  end;
end;

{ TMsgPushPool }

constructor TMsgPushPool.Create(AManager: TPushMsgManager);
begin
  inherited Create(True);
  FManager := AManager;
  FreeOnTerminate := True;  
  FLock := TThreadLock.Create;
  FMsgList := TInList.Create;
  Resume;
end;

destructor TMsgPushPool.Destroy;
var
  i: Integer;
begin
  for i := 0 to FMsgList.Count - 1 do
    TPushMessage(FMsgList.PopFirst).Free;
  FMsgList.Free;
  FLock.Free;
  // FManager 的线程数计算-
  InterlockedDecrement(FManager.FThreadCount);
  inherited;  
end;

procedure TMsgPushPool.ExecuteWork;
const
  WAIT_MILLSECONDS = 50;  // 毫秒数
var
  i: Integer;
  Trigger: Boolean;
  NowTickCount: Cardinal;
  Msg: TPushMessage;
  function GetTickCountDiff: Boolean;
  begin
    if (NowTickCount >= Msg.FTickCount) then
      Result := NowTickCount - Msg.FTickCount >= WAIT_MILLSECONDS  // n 毫秒
    else
      Result := High(Cardinal) - Msg.FTickCount + NowTickCount >= WAIT_MILLSECONDS;
  end;
begin
  // 每 n 毫秒循环一次，
  // 检查池中的消息是否要投放到推送队列。
  while (Terminated = False) do
  begin
    Sleep(WAIT_MILLSECONDS);  // 等 n 毫秒
    FLock.Acquire;
    try
      Trigger := False;
      NowTickCount := GetTickCount;
      for i := 1 to FMsgList.Count do
      begin
        Msg := FMsgList.PopFirst;
        if GetTickCountDiff then  // 时差达 n 毫秒
        begin
          FManager.InterAdd(Msg); // 正式加入推送队列
          Trigger := True;
        end else
          FMsgList.Add(Msg);  // 重新加入
      end;
      if Trigger then  // 激活
        FManager.ActivateThreads;
    finally
      FLock.Release;
    end;
  end;
end;

procedure TMsgPushPool.InterAdd(Msg: TPushMessage);
begin
  // 把消息加到缓冲列表
  FLock.Acquire;
  try
    FMsgList.Add(Msg);
  finally
    FLock.Release;
  end;
end;

{ TPushMsgManager }

procedure TPushMsgManager.ActivateThreads;
var
  Trigger: Boolean;
begin
  // 激活推送线程
  FLock.Acquire;
  try
    Trigger := (FMsgList.Count > 0); // 有消息
  finally
    FLock.Release;
  end;
  if Trigger then
    ReleaseSemapHore(FSemaphore, 8, Nil);
end;

function TPushMsgManager.AddWork(Msg: TPushMessage): Boolean;
  function GetTickCountDiff: Boolean;
  begin
    if (FNowTickCount >= FTickCount) then
      Result := FNowTickCount - FTickCount >= 1000  // 一秒
    else
      Result := High(Cardinal) - FTickCount + FNowTickCount >= 1000;
  end;
begin
  // 加入一个推送消息
  // 速度控制：每秒加入的推送数超 FMaxPushCount 次，放弃！
  //           出现消息堆积时，也放弃！

  Result := True;
  FLock.Acquire;

  try
    if (FMaxPushCount > 0) then
    begin
      if (FPushMsgCount > FMaxPushCount div 5) then // 出现消息堆积（根据设备性能调整）
        Result := False
      else begin
        FNowTickCount := GetTickCount;
        if GetTickCountDiff then  // 时差 1 秒
        begin
          FTickCount := FNowTickCount;
          Result := (FPushCountPS <= FMaxPushCount); // 没超量
          FPushCountPS := 0;
        end;
      end;
    end;
    if Result then  // 统计
    begin
      // 活动数
      FActiveCount := FMsgList.Count;
      // 每秒加入的消息数
      Inc(FPushCountPS, Msg.FClientCount);
    end;
  finally
    FLock.Release;
  end;

  if Result then
    FPushPool.InterAdd(Msg)  // 先加入池
  else // 释放（修正）
    Msg.Free;

end;

constructor TPushMsgManager.Create(ABusiManger: TBusiWorkManager; AThreadCount, AMaxPushCount: Integer);
var
  i: Integer;
begin
  inherited Create;

  // 1. 信号灯：最大值 = MaxInt
  FSemaphore := CreateSemapHore(Nil, 0, MaxInt, Nil);
  FWaitSemaphore := CreateSemapHore(Nil, 0, MaxInt, Nil);

  // 2. 锁、列表
  FLock := TThreadLock.Create;
  FPushPool := TMsgPushPool.Create(Self);  // 消息池
  FMsgList := TInList.Create;  // 推送消息列表

  // 3. 推送线程数，允许每秒最大推送数
  FThreadCount := AThreadCount;
  FMaxPushCount := AMaxPushCount;

  SetLength(FThreads, FThreadCount);

  for i := 0 to High(FThreads) do
  begin
    FThreads[i] := TPushThread.Create(Self, ABusiManger);
    FThreads[i].Resume;
  end;

  // 4. 包含 FPushPool，停止时先释放 FPushPool
  InterlockedIncrement(FThreadCount);
end;

destructor TPushMsgManager.Destroy;
begin
  StopThreads;
  inherited;
end;

function TPushMsgManager.GetWork(var Msg: TPushMessage): Boolean;
begin
  // 取一个推送消息
  FLock.Acquire;
  try
    Msg := FMsgList.PopFirst;
    if (Msg = nil) then
    begin
      FActiveCount := 0;
      Result := False;
    end else
    begin
      FActiveCount := FMsgList.Count;
      Dec(FPushMsgCount, Msg.FClientCount);  // 待推送数-      
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TPushMsgManager.InterAdd(Msg: TPushMessage);
begin
  // 正式把消息加入推送队列
  FLock.Acquire;
  try
    // 估算待推送数
    if (Msg.FCastType > btUnknown) then
      Msg.FClientCount := Msg.FObjPool.UsedCount
    else
      Msg.FClientCount := 1;
    Inc(FPushMsgCount, Msg.FClientCount);
    FMsgList.Add(Msg);
  finally
    FLock.Release;
  end;
end;

procedure TPushMsgManager.StopThreads;
var
  i: Integer;
begin
  if Assigned(FLock) then
  begin
    // 先释放
    FPushPool.Terminate;
    
    // 停止全部推送线程
    for i := 0 to High(FThreads) do
      FThreads[i].Stop;

    // 等待全部线程退出
    while (FThreadCount > 0) do
    begin
      ReleaseSemapHore(FSemaphore, 1, Nil);
      Sleep(10);
    end;

    SetLength(FThreads, 0);
    CloseHandle(FSemaphore);  // 关闭
    CloseHandle(FWaitSemaphore);  // 关闭

    // 释放未发送的消息空间
    for i := 0 to FMsgList.Count - 1 do
      TPushMessage(FMsgList.PopFirst).Free;

    FMsgList.Free;
    FLock.Free;

    FPushPool := nil;
    FMsgList := nil;
    FLock := nil;
  end;
end;

procedure TPushMsgManager.WaitFor;
begin
  // 等待活动线程结束
  while (FActiveThreadCount > 0) do
    Sleep(10);
end;

end.
