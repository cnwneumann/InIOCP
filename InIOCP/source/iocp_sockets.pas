(*
 * iocp 服务端各种套接字封装
 *)
unit iocp_sockets;

interface

{$I in_iocp.inc}        // 模式设置

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils,
  Datasnap.Provider, System.Variants, System.DateUtils, {$ELSE}
  Windows, Classes, SysUtils, Provider, Variants, DateUtils, {$ENDIF}
  iocp_base, iocp_zlib, iocp_api,
  iocp_Winsock2, iocp_wsExt, iocp_utils,
  iocp_baseObjs, iocp_objPools, iocp_senders,
  iocp_receivers, iocp_msgPacks, iocp_log,
  http_objects, iocp_WsJSON;

type

  // ================== 基本套接字 类 ======================

  TRawSocket = class(TObject)
  private
    FConnected: Boolean;       // 是否连接
    FErrorCode: Integer;       // 异常代码
    FPeerIP: String;           // IP
    FPeerIPPort: string;       // IP+Port
    FPeerPort: Integer;        // Port
    FSocket: TSocket;          // 套接字
    procedure InternalClose;    
  protected
    procedure IniSocket(AServer: TObject; ASocket: TSocket; AData: Pointer = nil); virtual;
    procedure SetPeerAddr(const Addr: PSockAddrIn);
  public
    constructor Create(AddSocket: Boolean);
    destructor Destroy; override;
    procedure Close; virtual;    
  public
    property Connected: Boolean read FConnected;
    property ErrorCode: Integer read FErrorCode;
    property PeerIP: String read FPeerIP;
    property PeerPort: Integer read FPeerPort;
    property PeerIPPort: String read FPeerIPPort;
    property Socket: TSocket read FSocket;
  public
    class function GetPeerIP(const Addr: PSockAddrIn): String;
  end;

  // ================== 监听套接字 类 ======================

  TListenSocket = class(TRawSocket)
  public
    function Bind(Port: Integer; const Addr: String = ''): Boolean;
    function StartListen: Boolean;
  end;

  // ================== AcceptEx 投放套接字 ======================

  TAcceptSocket = class(TRawSocket)
  private
    FListenSocket: TSocket;    // 监听套接字
    FIOData: TPerIOData;       // 内存块
    FByteCount: Cardinal;      // 投放用
  public
    constructor Create(ListenSocket: TSocket);
    destructor Destroy; override;
    function AcceptEx: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure NewSocket; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function SetOption: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
  end;

  // ================== 业务执行模块基类 ======================

  TIOCPSocket = class;
  THttpSocket = class;
  TWebSocket  = class;

  TBaseWorker = class(TObject)
  protected
    FServer: TObject;           // TInIOCPServer 服务器
    FGlobalLock: TThreadLock;   // 全局锁
    FThreadIdx: Integer;        // 编号
  protected
    procedure Execute(const ASocket: TIOCPSocket); virtual; abstract;
    procedure HttpExecute(const ASocket: THttpSocket); virtual; abstract;
    procedure WSExecute(const ASocket: TWebSocket); virtual; abstract;
  public
    property GlobalLock: TThreadLock read FGlobalLock; // 可以在业务操时用
    property ThreadIdx: Integer read FThreadIdx;
  end;

  // ================== Socket 基类 ======================
  // FState 状态：
  // 1. 空闲 = 0，关闭 = 9
  // 2. 占用 = 1，TransmitFile 时 +1，任何异常均 +1
  //    （正常值=1,2，其他值即为异常）
  // 3. 尝试关闭：空闲=0，TransmitFile=2（特殊的空闲），均可直接关闭
  // 4. 加锁：空闲=0 -> 成功

  TBaseSocket = class(TRawSocket)
  private
    FLinkNode: PLinkRec;       // 对应客户端池的 PLinkRec，方便回收

    FRecvBuf: PPerIOData;      // 接收用的数据包
    FSender: TBaseTaskSender;  // 数据发送器（引用）

    FObjPool: TIOCPSocketPool; // 对象池
    FServer: TObject;          // TInIOCPServer 服务器
    FWorker: TBaseWorker;      // 业务执行者（引用）

    FByteCount: Cardinal;      // 接收字节数
    FComplete: Boolean;        // 接收完毕/触发业务
    FRefCount: Integer;        // 引用数
    FState: Integer;           // 状态（原子操作变量）
    FTickCount: Cardinal;      // 客户端访问毫秒数
    FUseTransObj: Boolean;     // 使用 TTransmitObject 发送

    FData: Pointer;            // 额外的数据，由用户扩展

    function CheckDelayed(ATickCount: Cardinal): Boolean;
    function GetActive: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function GetBufferPool: TIODataPool; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function GetReference: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function GetSocketState: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}

    procedure InternalRecv(Complete: Boolean);
    procedure OnSendError(Sender: TObject);
  protected
    {$IFDEF TRANSMIT_FILE}      // TransmitFile 发送模式
    FTask: TTransmitObject;     // 待发送数据描述
    FTaskExists: Boolean;       // 存在任务
    procedure InterTransmit;    // 发送数据
    procedure InterFreeRes; virtual; abstract; // 释放发送资源
    {$ENDIF}
    procedure ClearResources; virtual; abstract;
    procedure Clone(Source: TBaseSocket);  // 克隆（转移资源）
    procedure DoWork(AWorker: TBaseWorker; ASender: TBaseTaskSender);  // 业务线程调用入口
    procedure ExecuteWork; virtual; abstract;  // 调用入口
    procedure IniSocket(AServer: TObject; ASocket: TSocket; AData: Pointer = nil); override;
    procedure InterCloseSocket(Sender: TObject); virtual;
    procedure InternalPush(AData: PPerIOData); // 推送入口
    procedure MarkIODataBuf(AData: PPerIOData); virtual;
    procedure SocketError(IOKind: TIODataType); virtual;
  public
    constructor Create(AObjPool: TIOCPSocketPool; ALinkNode: PLinkRec); virtual;
    destructor Destroy; override;

    // 超时检查
    function CheckTimeOut(ANowTickCount: Cardinal): Boolean;

    // 关闭
    procedure Close; override;  

    {$IFDEF TRANSMIT_FILE}
    procedure FreeTransmitRes;  // 释放 TransmitFile 的资源
    {$ENDIF}

    // 工作前加锁
    function Lock(PushMode: Boolean): Integer; 

    // 投递接收
    procedure PostRecv; virtual;

    // 投放事件：被删除、拒绝服务、超时
    procedure PostEvent(IOKind: TIODataType); virtual; abstract;

    // 尝试关闭
    procedure TryClose;
  public
    property Active: Boolean read GetActive;
    property BufferPool: TIODataPool read GetBufferPool;
    property Complete: Boolean read FComplete;
    property LinkNode: PLinkRec read FLinkNode;
    property ObjPool: TIOCPSocketPool read FObjPool;
    property RecvBuf: PPerIOData read FRecvBuf;
    property Reference: Boolean read GetReference;
    property Sender: TBaseTaskSender read FSender;
    property SocketState: Boolean read GetSocketState;
    property Worker: TBaseWorker read FWorker;
  public
    // 属性 Data，用户可以扩展
    property Data: Pointer read FData write FData;
  end;

  TBaseSocketClass = class of TBaseSocket;

  // ================== 原始数据流 Socket ==================

  TStreamSocket = class(TBaseSocket)
  protected
    {$IFDEF TRANSMIT_FILE}
    procedure InterFreeRes; override;
    {$ENDIF}
    procedure ClearResources; override;
    procedure ExecuteWork; override;
  public
    procedure PostEvent(IOKind: TIODataType); override;
    procedure SendData(const Data: PAnsiChar; Size: Cardinal); overload; virtual;
    procedure SendData(const Msg: String); overload; virtual;
    procedure SendData(Handle: THandle); overload; virtual;
    procedure SendData(Stream: TStream); overload; virtual;
    procedure SendDataVar(Data: Variant); virtual; 
  end;

  // ================== C/S 模式业务处理 ==================

  // 1. 服务端接收到的数据

  TReceiveParams = class(TReceivePack)
  private
    FSocket: TIOCPSocket;     // 宿主
    FMsgHead: PMsgHead;       // 协议头位置
    function GetLogName: string;
  protected
    procedure SetUniqueMsgId;
  public
    constructor Create(AOwner: TIOCPSocket); overload;
    procedure CreateAttachment(const ALocalPath: string); override;
  public
    property LogName: string read GetLogName;
    property MsgHead: PMsgHead read FMsgHead;
    property Socket: TIOCPSocket read FSocket;
  end;

  // 2. 反馈给客户端的数据

  TReturnResult = class(TBaseMessage)
  private
    FSocket: TIOCPSocket;     // 宿主
    FSender: TBaseTaskSender; // 任务发送器
  public
    constructor Create(AOwner: TIOCPSocket; AInitialize: Boolean = True);
    procedure LoadFromFile(const AFileName: String; ServerMode: Boolean = False); override;
    procedure LoadFromVariant(const AProviders: array of TDataSetProvider;
                              const ATableNames: array of String); overload; override;
  public
    property Socket: TIOCPSocket read FSocket;
    // 公开协议头属性
    property Action: TActionType read FAction;
    property ActResult: TActionResult read FActResult write FActResult;
    property Offset: TFileSize read FOffset;
    property OffsetEnd: TFileSize read FOffsetEnd;
  end;

  TIOCPSocket = class(TBaseSocket)
  private
    FReceiver: TServerReceiver;// 数据接收器
    FParams: TReceiveParams;   // 接收到的消息（变量化）
    FResult: TReturnResult;    // 返回的数据
    FEnvir: PEnvironmentVar;   // 工作环境信息
    FAction: TActionType;      // 内部事件
    FSessionId: Cardinal;      // 对话凭证 id

    function CheckMsgHead(InBuf: PAnsiChar): Boolean;
    function CreateSession: Cardinal;
    function GetRole: TClientRole;
    function GetLogName: string;
    function SessionValid(ASession: Cardinal): Boolean;

    procedure CreateResources;
    procedure HandleDataPack;
    procedure ReturnHead(ActResult: TActionResult);
    procedure ReturnMessage(ActResult: TActionResult; const ErrMsg: String = '');
    procedure ReturnResult;
    procedure SetLogoutState;
  protected
    {$IFDEF TRANSMIT_FILE}
    procedure InterFreeRes; override;
    {$ENDIF}
    procedure ClearResources; override;
    procedure ExecuteWork; override;  // 调用入口
    procedure IniSocket(AServer: TObject; ASocket: TSocket; AData: Pointer = nil); override;
    procedure InterCloseSocket(Sender: TObject); override;
    procedure SocketError(IOKind: TIODataType); override;
  public
    destructor Destroy; override;
  public
    // 业务模块调用
    procedure PostEvent(IOKind: TIODataType); override;
    procedure SetLogState(AEnvir: PEnvironmentVar);
  public
    property Action: TActionType read FAction;
    property Envir: PEnvironmentVar read FEnvir;
    property LoginName: string read GetLogName;
    property Params: TReceiveParams read FParams;
    property Result: TReturnResult read FResult;
    property Role: TClientRole read GetRole;  // 角色/权限    
    property SessionId: Cardinal read FSessionId;
  end;

  // ================== Http 协议 Socket ==================

  TRequestObject = class(THttpRequest);

  TResponeObject = class(THttpRespone);

  THttpSocket = class(TBaseSocket)
  private
    FRequest: THttpRequest;    // http 请求
    FRespone: THttpRespone;    // http 应答
    FStream: TFileStream;      // 接收文件的流
    FKeepAlive: Boolean;       // 保持连接
    FSessionId: AnsiString;    // Session Id
    procedure UpgradeSocket(SocketPool: TIOCPSocketPool);
    procedure DecodeHttpRequest;
  protected
    {$IFDEF TRANSMIT_FILE}
    procedure InterFreeRes; override;
    {$ENDIF}
    procedure ClearResources; override;
    procedure ExecuteWork; override;
    procedure SocketError(IOKind: TIODataType); override;
  public
    destructor Destroy; override;
    // 推送事件
    procedure PostEvent(IOKind: TIODataType); override;
    // 文件流操作
    procedure CreateStream(const FileName: String);
    procedure WriteStream(Data: PAnsiChar; DataLength: Integer);
    procedure CloseStream;
  public
    property Request: THttpRequest read FRequest;
    property Respone: THttpRespone read FRespone;
    property SessionId: AnsiString read FSessionId;
  end;

  // ================== WebSocket 类 ==================

  // . 待返回的 JSON 消息
  
  TResultJSON = class(TSendJSON)
  public
    property DataSet;
  end;
  
  TWebSocket = class(TStreamSocket)
  private
    FReceiver: TWSServerReceiver;  // 数据接收器
    FJSON: TBaseJSON;          // 收到的 JSON 数据
    FResult: TResultJSON;      // 要返回的 JSON 数据
    FMsgType: TWSMsgType;      // 数据类型
    FOpCode: TWSOpCode;        // WebSocket 操作类型
    FRole: TClientRole;        // 客户权限（预设）
    FUserName: TNameString;    // 用户名称（预设）
  protected
    FData: PAnsiChar;          // 本次收到的数据引用位置
    FMsgSize: UInt64;          // 当前消息收到的累计长度
    FFrameSize: UInt64;        // 当前帧长度
    FFrameRecvSize: UInt64;    // 本次收到的数据长度
    procedure SetProps(AOpCode: TWSOpCode; AMsgType: TWSMsgType;
                       AData: Pointer; AFrameSize: Int64; ARecvSize: Cardinal);
  protected
    procedure ClearOwnerMark;
    procedure ClearResources; override;
    procedure ExecuteWork; override;
    procedure InternalPing;
  public
    constructor Create(AObjPool: TIOCPSocketPool; ALinkNode: PLinkRec); override;
    destructor Destroy; override;

    procedure PostEvent(IOKind: TIODataType); override;
    procedure SendData(const Data: PAnsiChar; Size: Cardinal); overload; override;
    procedure SendData(const Msg: String); overload; override;
    procedure SendData(Handle: THandle); overload; override;
    procedure SendData(Stream: TStream); overload; override;
    procedure SendDataVar(Data: Variant); override;

    procedure SendResult(UTF8CharSet: Boolean = False);
  public
    property Data: PAnsiChar read FData;  // raw
    property FrameRecvSize: UInt64 read FFrameRecvSize; // raw
    property FrameSize: UInt64 read FFrameSize; // raw
    property MsgSize: UInt64 read FMsgSize; // raw

    property JSON: TBaseJSON read FJSON; // JSON
    property Result: TResultJSON read FResult; // JSON
  public
    property MsgType: TWSMsgType read FMsgType; // 数据类型
    property OpCode: TWSOpCode read FOpCode;  // WebSocket 操作
  public
    property Role: TClientRole read FRole write FRole;
    property UserName: TNameString read FUserName write FUserName;
  end;

  // ================== TSocketBroker 代理套接字 ==================

  TSocketBroker = class;

  TAcceptBroker = procedure(Sender: TSocketBroker; const Host: AnsiString;
                            Port: Integer; var Accept: Boolean) of object;

  TBindIPEvent  = procedure(Sender: TSocketBroker; const Data: PAnsiChar;
                            DataSize: Cardinal) of object;

  TOuterPingEvent = TBindIPEvent;

  TSocketBroker = class(TBaseSocket)
  private
    FAction: Integer;          // 初始化
    FBroker: TObject;          // 代理服务
    FCmdConnect: Boolean;      // HTTP 代理模式

    FDualBuf: PPerIOData;      // 关联套接字的接收内存块
    FDualConnected: Boolean;   // 关联套接字连接状态
    FDualSocket: TSocket;      // 关联的套接字

    FRecvState: Integer;       // 接收状态
    FSocketType: TSocketBrokerType;  // 类型
    FTargetHost: AnsiString;   // 关联的主机地址
    FTargetPort: Integer;      // 关联的服务器端口

    FOnBind: TBindIPEvent;     // 绑定事件

    // 新的投放方法 
    procedure BrokerPostRecv(ASocket: TSocket; AData: PPerIOData; ACheckState: Boolean = True);
    // HTTP 协议的绑定
    procedure HttpBindOuter(Connection: TSocketBroker; const Data: PAnsiChar; DataSize: Cardinal);
  protected
    FBrokerId: AnsiString;     // 所属的反向代理 Id
    procedure ClearResources; override;
    procedure ExecuteWork; override;
    procedure IniSocket(AServer: TObject; ASocket: TSocket; AData: Pointer = nil); override;
    procedure InterCloseSocket(Sender: TObject); override;
    procedure MarkIODataBuf(AData: PPerIOData); override;
  protected
    procedure AssociateInner(InnerBroker: TSocketBroker);
    procedure SendInnerFlag;
    procedure SetConnection(AServer: TObject; Connection: TSocket);
  public
    procedure CreateBroker(const AServer: AnsiString; APort: Integer);  // 建连接中继
    procedure PostEvent(IOKind: TIODataType); override;
  end;

implementation

uses
  iocp_server, http_base, http_utils, iocp_threads, iocp_managers;

type
  THeadMessage   = class(TBaseMessage);
  TIOCPBrokerRef = class(TInIOCPBroker);

{ TRawSocket }

procedure TRawSocket.Close;
begin
  if FConnected then
    InternalClose;  // 关闭
end;

constructor TRawSocket.Create(AddSocket: Boolean);
begin
  inherited Create;
  if AddSocket then  // 建一个 Socket
    IniSocket(nil, iocp_utils.CreateSocket);
end;

destructor TRawSocket.Destroy;
begin
  if FConnected then
    InternalClose; // 关闭
  inherited;
end;

class function TRawSocket.GetPeerIP(const Addr: PSockAddrIn): String;
begin
  // 取IP
  Result := iocp_Winsock2.inet_ntoa(Addr^.sin_addr);
end;

procedure TRawSocket.IniSocket(AServer: TObject; ASocket: TSocket; AData: Pointer);
begin
  // 设置 Socket
  FSocket := ASocket;
  FConnected := FSocket <> INVALID_SOCKET;
end;

procedure TRawSocket.InternalClose;
begin
  // 关闭 Socket
  try
    iocp_Winsock2.Shutdown(FSocket, SD_BOTH);
    iocp_Winsock2.CloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  finally
    FConnected := False;
  end;
end;

procedure TRawSocket.SetPeerAddr(const Addr: PSockAddrIn);
begin
  // 从地址信息取 IP、Port
  FPeerIP := iocp_Winsock2.inet_ntoa(Addr^.sin_addr);
  FPeerPort := Addr^.sin_port;
  FPeerIPPort := FPeerIP + ':' + IntToStr(FPeerPort);
end;

{ TListenSocket }

function TListenSocket.Bind(Port: Integer; const Addr: String): Boolean;
var
  SockAddr: TSockAddrIn;
begin
  // 绑定地址
  // htonl(INADDR_ANY); 在任何地址（多块网卡）上监听
  FillChar(SockAddr, SizeOf(TSockAddr), 0);

  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := htons(Port);
  SockAddr.sin_addr.S_addr := inet_addr(PAnsiChar(ResolveHostIP(Addr)));

  if (iocp_Winsock2.bind(FSocket, TSockAddr(SockAddr), SizeOf(TSockAddr)) <> 0) then
  begin
    Result := False;
    FErrorCode := WSAGetLastError;
    iocp_log.WriteLog('TListenSocket.Bind->Error:' + IntToStr(FErrorCode));
  end else
  begin
    Result := True;
    FErrorCode := 0;
  end;
end;

function TListenSocket.StartListen: Boolean;
begin
  // 监听
  if (iocp_Winsock2.listen(FSocket, MaxInt) <> 0) then
  begin
    Result := False;
    FErrorCode := WSAGetLastError;
    iocp_log.WriteLog('TListenSocket.StartListen->Error:' + IntToStr(FErrorCode));
  end else
  begin
    Result := True;
    FErrorCode := 0;
  end;
end;

{ TAcceptSocket }

function TAcceptSocket.AcceptEx: Boolean;
begin
  // 投放 AcceptEx 请求
  FillChar(FIOData.Overlapped, SizeOf(TOverlapped), 0);

  FIOData.Owner := Self;       // 宿主
  FIOData.IOType := ioAccept;  // 再设置
  FByteCount := 0;

  Result := gAcceptEx(FListenSocket, FSocket,
                      Pointer(FIOData.Data.buf), 0,   // 用 0，不等待第一包数据
                      ADDRESS_SIZE_16, ADDRESS_SIZE_16,
                      FByteCount, @FIOData.Overlapped);

  if Result then
    FErrorCode := 0
  else begin
    FErrorCode := WSAGetLastError;
    Result := FErrorCode = WSA_IO_PENDING;
    if (Result = False) then
      iocp_log.WriteLog('TAcceptSocket.AcceptEx->Error:' + IntToStr(FErrorCode));
  end;
end;

constructor TAcceptSocket.Create(ListenSocket: TSocket);
begin
  inherited Create(True);
  // 新建 AcceptEx 用的 Socket
  FListenSocket := ListenSocket;
  GetMem(FIOData.Data.buf, ADDRESS_SIZE_16 * 2);  // 申请一块内存
  FIOData.Data.len := ADDRESS_SIZE_16 * 2;
  FIOData.Node := nil;  // 无
end;

destructor TAcceptSocket.Destroy;
begin
  FreeMem(FIOData.Data.buf);  // 释放内存块
  inherited;
end;

procedure TAcceptSocket.NewSocket;
begin
  // 新建 Socket
  FSocket := iocp_utils.CreateSocket;
end;

function TAcceptSocket.SetOption: Boolean;
begin
  // 复制 FListenSocket 的属性到 FSocket
  Result := iocp_Winsock2.setsockopt(FSocket, SOL_SOCKET,
                 SO_UPDATE_ACCEPT_CONTEXT, PAnsiChar(@FListenSocket),
                 SizeOf(TSocket)) <> SOCKET_ERROR;
end;

{ TBaseSocket }

constructor TBaseSocket.Create(AObjPool: TIOCPSocketPool; ALinkNode: PLinkRec);
begin
  inherited Create(False);
  // FSocket 由客户端接入时分配
  //   见：TInIOCPServer.AcceptClient
  //       TIOCPSocketPool.CreateObjData
  FObjPool := AObjPool; 
  FLinkNode := ALinkNode;
  FUseTransObj := True;  
end;

function TBaseSocket.CheckDelayed(ATickCount: Cardinal): Boolean;
begin
  // 取距最近活动时间的差
  if (ATickCount >= FTickCount) then
    Result := ATickCount - FTickCount <= 3000
  else
    Result := High(Cardinal) - ATickCount + FTickCount <= 3000;
  {$IFDEF TRANSMIT_FILE}
  if Assigned(FTask) then  // TransmitFile 没有任务
    Result := Result and (FTask.Exists = False);
  {$ENDIF}
end;

function TBaseSocket.CheckTimeOut(ANowTickCount: Cardinal): Boolean;
  function GetTickCountDiff: Boolean;
  begin
    if (ANowTickCount >= FTickCount) then
      Result := ANowTickCount - FTickCount >= TInIOCPServer(FServer).TimeOut
    else
      Result := High(Cardinal) - FTickCount + ANowTickCount >= TInIOCPServer(FServer).TimeOut;
  end;
begin
  // 超时检查
  if (FTickCount = NEW_TICKCOUNT) then  // 投放即断开，见：TBaseSocket.SetSocket
  begin
    Inc(FByteCount);  // ByteCount +
    Result := (FByteCount >= 5);  // 连续几次
  end else
    Result := GetTickCountDiff;
end;

procedure TBaseSocket.Clone(Source: TBaseSocket);
begin
  // 对象变换：把 Source 的套接字等资源转移到新对象
  // 由 TIOCPSocketPool 加锁调用，防止被检查为超时

  // 转移 Source 的套接字、地址
  IniSocket(Source.FServer, Source.FSocket, Source.FData);

  FPeerIP := Source.FPeerIP;
  FPeerPort := Source.FPeerPort;
  FPeerIPPort := Source.FPeerIPPort;

  // 清除 Source 的资源值
  // Source.FServer 不变，释放时要检查：TBaseSocket.Destroy
  Source.FData := nil;
  
  Source.FPeerIP := '';
  Source.FPeerPort := 0;
  Source.FPeerIPPort := '';

  Source.FConnected := False;
  Source.FSocket := INVALID_SOCKET;
   
  // 未建 FTask  
end;

procedure TBaseSocket.Close;
begin
  ClearResources;  // 只清空资源，不释放，下次不用新建
  inherited;
end;

destructor TBaseSocket.Destroy;
begin
  {$IFDEF TRANSMIT_FILE}
  if Assigned(FTask) then
    FTask.Free;
  {$ENDIF}
  if TInIOCPServer(FServer).Active and Assigned(FRecvBuf) then
  begin
    BufferPool.Push(FRecvBuf^.Node);  // 回收内存块
    FRecvBuf := Nil;
  end;
  inherited;
end;

procedure TBaseSocket.DoWork(AWorker: TBaseWorker; ASender: TBaseTaskSender);
begin
  // 初始化
  // 任务结束后不设 FWorker、FSender 为 Nil

  {$IFDEF TRANSMIT_FILE}
  if FUseTransObj then
  begin
    if (Assigned(FTask) = False) then
    begin
      FTask := TTransmitObject.Create(Self); // TransmitFile 对象
      FTask.OnError := OnSendError;
    end;
    FTask.Socket := FSocket;
    FTaskExists := False; // 必须
  end;
  {$ENDIF}
  
  FErrorCode := 0;      // 无异常
  FByteCount := FRecvBuf^.Overlapped.InternalHigh;  // 收到字节数

  FWorker := AWorker;   // 执行者
  FSender := ASender;   // 发送器

  FSender.Owner := Self;
  FSender.Socket := FSocket;
  FSender.OnError := OnSendError;

  // 执行任务
  ExecuteWork;
    
end;

{$IFDEF TRANSMIT_FILE}
procedure TBaseSocket.FreeTransmitRes;
begin
  // 工作线程调用：TransmitFile 发送完毕
  if (InterlockedDecrement(FState) = 1) then  // FState=2 -> 正常，否则异常
    InterFreeRes // 在子类实现，正式释放发送资源，判断是否继续投放 WSARecv！
  else
    InterCloseSocket(Self);
end;
{$ENDIF}

function TBaseSocket.GetActive: Boolean;
begin
  // 推送前取初始化状态（接收过数据）
  Result := (iocp_api.InterlockedCompareExchange(Integer(FByteCount), 0, 0) > 0);
end;

function TBaseSocket.GetBufferPool: TIODataPool;
begin
  // 取内存池
  Result := TInIOCPServer(FServer).IODataPool;
end;

function TBaseSocket.GetReference: Boolean;
begin
  // 推送时引用（业务引用 FRecvBuf^.RefCount）
  Result := InterlockedIncrement(FRefCount) = 1;
end;

function TBaseSocket.GetSocketState: Boolean;
begin
  // 取状态, FState = 1 说明正常
  Result := iocp_api.InterlockedCompareExchange(FState, 1, 1) = 1;
end;

procedure TBaseSocket.InternalPush(AData: PPerIOData);
var
  ByteCount, Flags: Cardinal;
begin
  // 推送消息（推送线程调用）
  //  AData：TPushMessage.FPushBuf

  // 清重叠结构
  FillChar(AData^.Overlapped, SizeOf(TOverlapped), 0);

  FErrorCode := 0;
  FRefCount := 0;  // AData:Socket = 1:n
  FTickCount := GetTickCount;  // +

  ByteCount := 0;
  Flags := 0;

  if (InterlockedDecrement(FState) <> 0) then
    InterCloseSocket(Self)
  else
    if (iocp_Winsock2.WSASend(FSocket, @(AData^.Data), 1, ByteCount,
        Flags, LPWSAOVERLAPPED(@AData^.Overlapped), nil) = SOCKET_ERROR) then
    begin
      FErrorCode := WSAGetLastError;
      if (FErrorCode <> ERROR_IO_PENDING) then  // 异常
      begin
        SocketError(AData^.IOType);
        InterCloseSocket(Self);  // 关闭
      end else
        FErrorCode := 0;
    end;

end;

procedure TBaseSocket.InternalRecv(Complete: Boolean);
var
  ByteCount, Flags: DWORD;
begin
  // 在任务结束最后提交一个接收请求

  // 未接收完成，缩短超时时间（防恶意攻击）
  if (Complete = False) and (TInIOCPServer(FServer).TimeOut > 0) then
    Dec(FTickCount, 10000);

  // 清重叠结构
  FillChar(FRecvBuf^.Overlapped, SizeOf(TOverlapped), 0);

  FRecvBuf^.Owner := Self;  // 宿主
  FRecvBuf^.IOType := ioReceive;  // iocp_server 中判断用
  FRecvBuf^.Data.len := IO_BUFFER_SIZE; // 恢复

  ByteCount := 0;
  Flags := 0;

  // 正常时 FState=1，其他任何值都说明出现了异常，
  // FState-，FState <> 0 -> 异常改变了状态，关闭！

  if (InterlockedDecrement(FState) <> 0) then
    InterCloseSocket(Self)
  else  // FRecvBuf^.Overlapped 与 TPerIOData 同地址
    if (iocp_Winsock2.WSARecv(FSocket, @(FRecvBuf^.Data), 1, ByteCount,
        Flags, LPWSAOVERLAPPED(@FRecvBuf^.Overlapped), nil) = SOCKET_ERROR) then
    begin
      FErrorCode := WSAGetLastError;
      if (FErrorCode <> ERROR_IO_PENDING) then  // 异常
      begin
        SocketError(ioReceive);
        InterCloseSocket(Self);  // 关闭
      end else
        FErrorCode := 0;
    end;

  // 投放完成，设 FByteCount > 0, 可以接受推送消息了, 同时注意超时检查
  //   见：CheckTimeOut、TPushThread.DoMethod、TIOCPSocketPool.GetSockets
  if (FByteCount = 0) then
    FByteCount := 1;  // 不能太大，否则超时

  // 任务完成  
  if (FComplete <> Complete) then
    FComplete := Complete;
    
end;

function TBaseSocket.Lock(PushMode: Boolean): Integer;
const
  SOCKET_STATE_IDLE  = 0;  // 空闲
  SOCKET_STATE_BUSY  = 1;  // 在用
  SOCKET_STATE_TRANS = 2;  // TransmitFile 在用 
begin
  // 工作前加锁
  //  状态 FState = 0 -> 1, 且上次任务完成 -> 成功！
  //  以后在 Socket 内部的任何异常都 FState+
  case iocp_api.InterlockedCompareExchange(FState, 1, 0) of  // 返回原值

    SOCKET_STATE_IDLE: begin
      if PushMode then   // 推送模式
      begin
        if FComplete then
          Result := SOCKET_LOCK_OK
        else
          Result := SOCKET_LOCK_FAIL;
      end else
      begin
        // 业务线程模式，见：TWorkThread.HandleIOData
        Result := SOCKET_LOCK_OK;     // 总是运行
      end;
      if (Result = SOCKET_LOCK_FAIL) then // 业务未完成，放弃！
        if (InterlockedDecrement(FState) <> 0) then
          InterCloseSocket(Self);
    end;

    SOCKET_STATE_BUSY:
      Result := SOCKET_LOCK_FAIL;     // 在用

    SOCKET_STATE_TRANS:
      if FUseTransObj then
        Result := SOCKET_LOCK_FAIL    // 在用
      else
        Result := SOCKET_LOCK_CLOSE;  // 异常

    else
      Result := SOCKET_LOCK_CLOSE;    // 已关闭或工作中异常
  end;
end;

procedure TBaseSocket.MarkIODataBuf(AData: PPerIOData);
begin
  // 空
end;

procedure TBaseSocket.IniSocket(AServer: TObject; ASocket: TSocket; AData: Pointer);
begin
  inherited;

  FServer := AServer;// 服务器（在前）
  FData := AData;    // 扩展数据

  // 分配接收内存块（释放时回收）
  if (FRecvBuf = nil) then
  Begin
    FRecvBuf := BufferPool.Pop^.Data; // 在 FServer 赋值后
    FRecvBuf^.IOType := ioReceive;  // 类型
    FRecvBuf^.Owner := Self;  // 宿主
  End;

  FByteCount := 0;   // 接收数据长度
  FComplete := True; // 等待接收
  FErrorCode := 0;   // 无异常
  FState := 9;       // 无效状态，投放 Recv 才算正式使用

  // 最大值，防止被监测为超时，
  //   见：TTimeoutThread.ExecuteWork
  FTickCount := NEW_TICKCOUNT;
end;

procedure TBaseSocket.InterCloseSocket(Sender: TObject);
begin
  // 内部关闭
  InterlockedExchange(Integer(FByteCount), 0); // 不接受推送了
  InterlockedExchange(FState, 9);  // 无效状态
  TInIOCPServer(FServer).CloseSocket(Self);  // 用关闭线程（允许重复关闭）
end;

{$IFDEF TRANSMIT_FILE}
procedure TBaseSocket.InterTransmit;
begin
  if FTask.Exists then
  begin
    FTaskExists := True;
    InterlockedIncrement(FState);  // FState+，正常时=2
    FTask.TransmitFile;
  end;
end;
{$ENDIF}

procedure TBaseSocket.OnSendError(Sender: TObject);
begin
  // 任务发送异常的回调方法
  //   见：TBaseSocket.DoWork、TBaseTaskObject.Send...
  FErrorCode := TBaseTaskObject(Sender).ErrorCode;
  InterlockedIncrement(FState);  // FState+
  SocketError(TBaseTaskObject(Sender).IOType);
end;

procedure TBaseSocket.PostRecv;
begin
  // 接入时投放接收缓存
  //   见：TInIOCPServer.AcceptClient、THttpSocket.ExecuteWork
  FState := 1;  // 设繁忙
  InternalRecv(True); // 投放时 FState-
end;

procedure TBaseSocket.SocketError(IOKind: TIODataType);
const
  PROCEDURE_NAMES: array[ioReceive..ioTimeOut] of string = (
                   'Post WSARecv->', 'Post WSASend->',
                   {$IFDEF TRANSMIT_FILE} 'TransmitFile->', {$ENDIF}
                   'InternalPush->', 'InternalPush->',
                   'InternalPush->');
begin
  // 写异常日志
  if Assigned(FWorker) then  // 主动型代理投放，没有 FWorker
    iocp_log.WriteLog(PROCEDURE_NAMES[IOKind] + PeerIPPort +
                      ',Error:' + IntToStr(FErrorCode) +
                      ',BusiThread:' + IntToStr(FWorker.ThreadIdx));
end;

procedure TBaseSocket.TryClose;
begin
  // 尝试关闭
  // FState+, 原值: 0,2,3... <> 1 -> 关闭
  if (InterlockedIncrement(FState) in [1, 3]) then // <> 2
    InterCloseSocket(Self);
end;

{ TStreamSocket }

procedure TStreamSocket.ClearResources;
begin
  {$IFDEF TRANSMIT_FILE}
  if Assigned(FTask) then
    FTask.FreeResources(True);
  {$ENDIF}
end;

procedure TStreamSocket.ExecuteWork;
begin
  // 直接调用 Server 的 OnDataReceive 事件（未必接收完毕）
  try
    FTickCount := GetTickCount;
    if Assigned(TInIOCPServer(FServer).OnDataReceive) then
      TInIOCPServer(FServer).OnDataReceive(Self, FRecvBuf^.Data.buf, FByteCount);
  finally
    {$IFDEF TRANSMIT_FILE}
    if (FTaskExists = False) then {$ENDIF}
      InternalRecv(True);  // 继续接收
  end;
end;

procedure TStreamSocket.PostEvent(IOKind: TIODataType);
begin
  // Empty
end;

{$IFDEF TRANSMIT_FILE}
procedure TStreamSocket.InterFreeRes;
begin
  // 释放 TransmitFile 的发送资源，继续投放接收！
  try
    ClearResources;
  finally
    InternalRecv(True);
  end;
end;
{$ENDIF}

procedure TStreamSocket.SendData(const Data: PAnsiChar; Size: Cardinal);
var
  Buf: PAnsiChar;
begin
  // 发送内存块数据（复制 Data）
  if Assigned(Data) and (Size > 0) then
  begin
    GetMem(Buf, Size);
    System.Move(Data^, Buf^, Size);
    {$IFDEF TRANSMIT_FILE}
    FTask.SetTask(Buf, Size);
    InterTransmit;
    {$ELSE}
    FSender.Send(Buf, Size);
    {$ENDIF}
  end;
end;

procedure TStreamSocket.SendData(const Msg: String);
begin
  // 发送文本
  if (Msg <> '') then
  begin
    {$IFDEF TRANSMIT_FILE}
    FTask.SetTask(Msg);
    InterTransmit;
    {$ELSE}
    FSender.Send(Msg);
    {$ENDIF}
  end;
end;

procedure TStreamSocket.SendData(Handle: THandle);
begin
  // 发送文件 handle（自动关闭）
  if (Handle > 0) and (Handle <> INVALID_HANDLE_VALUE) then
  begin
    {$IFDEF TRANSMIT_FILE}
    FTask.SetTask(Handle, GetFileSize64(Handle));
    InterTransmit;
    {$ELSE}
    FSender.Send(Handle, GetFileSize64(Handle));
    {$ENDIF}
  end;
end;

procedure TStreamSocket.SendData(Stream: TStream);
begin
  // 发送流数据（自动释放）
  if Assigned(Stream) then
  begin
    {$IFDEF TRANSMIT_FILE}
    FTask.SetTask(Stream, Stream.Size);
    InterTransmit;
    {$ELSE}
    FSender.Send(Stream, Stream.Size, True);
    {$ENDIF}
  end;
end;

procedure TStreamSocket.SendDataVar(Data: Variant);
begin
  // 发送可变类型数据
  if (VarIsNull(Data) = False) then
  begin
    {$IFDEF TRANSMIT_FILE}
    FTask.SetTaskVar(Data);
    InterTransmit;
    {$ELSE}
    FSender.SendVar(Data);
    {$ENDIF}
  end;
end;

{ TReceiveParams }

constructor TReceiveParams.Create(AOwner: TIOCPSocket);
begin
  inherited Create;
  FSocket := AOwner;
end;

procedure TReceiveParams.CreateAttachment(const ALocalPath: string);
begin
  // 建文件流，接收附件
  inherited;
  if Error then  // 出现错误
  begin
    FSocket.FResult.ActResult := arFail;
    FSocket.FResult.Msg := GetSysErrorMessage();
  end else
  begin
    FSocket.FAction := atAfterReceive; // 结束时执行事件
    FSocket.FResult.FActResult := arAccept;  // 允许上传
    FSocket.FReceiver.Complete := False;  // 继续接收附件
    
    // 续传，返回额外的文件信息
    if (FAction in FILE_CHUNK_ACTIONS) then
    begin
      FSocket.FResult.FOffset := FOffset;  // 可能要从新接收，FOffset 被修改
      FSocket.FResult.SetFileName(ExtractFileName(Attachment.FileName));  // 文件同名时被改变
      FSocket.FResult.SetDirectory(EncryptString(ExtractFilePath(Attachment.FileName))); // 加密服务端文件
      FSocket.FResult.SetAttachFileName(GetAttachFileName);  // 客户端的文件全名
    end;

    // 发布 URL（路径应该可以公开）
    if Assigned(TInIOCPServer(FSocket.FServer).HttpDataProvider) then
      SetURL(ChangeSlash(Attachment.FileName));
  end;
end;

function TReceiveParams.GetLogName: string;
begin
  // 取登录时的名称
  Result := FSocket.GetLogName;
end;

procedure TReceiveParams.SetUniqueMsgId;
begin
  // 设置推送消息的 MsgId
  //   推送消息改用服务器的唯一 MsgId
  //   修改缓冲的 MsgId
  //   不要改 FResult.FMsgId，否则客户端把发送反馈当作推送消息处理
  FMsgHead^.MsgId := TSystemGlobalLock.GetMsgId;
end;

{ TReturnResult }

constructor TReturnResult.Create(AOwner: TIOCPSocket; AInitialize: Boolean);
begin
  inherited Create(nil);  // 用接收的 Owner
  FSocket := AOwner;
  PeerIPPort := FSocket.PeerIPPort;
  if AInitialize then
  begin
    FOwner := AOwner.Params.FOwner;
    SetHeadMsg(AOwner.Params.FMsgHead, True);
  end;
end;

procedure TReturnResult.LoadFromFile(const AFileName: String; ServerMode: Boolean);
begin
  inherited;  // 立刻打开文件，等待发送
  if Error then
  begin
    FActResult := arFail;
    FOffset := 0;
    FOffsetEnd := 0;
  end else
  begin
    FActResult := arOK;
    if (FAction = atFileDownChunk) then  // 断点下载
    begin
      FOffset := FSocket.FParams.FOffset;  // 请求位移
      AdjustTransmitRange(FSocket.FParams.FOffsetEnd);  // 期望长度, =客户端 FMaxChunkSize
      SetDirectory(EncryptString(ExtractFilePath(AFileName)));  // 返回加密的路径
    end;
  end;
end;

procedure TReturnResult.LoadFromVariant(const AProviders: array of TDataSetProvider;
  const ATableNames: array of String);
begin
  inherited; // 直接调用
end;

{ TIOCPSocket }

function TIOCPSocket.CheckMsgHead(InBuf: PAnsiChar): Boolean;
  function CheckLogState: TActionResult;
  begin
    // 检查登录状态
    if (FParams.Action = atUserLogin) then
      Result := arOK       // 通过
    else
    if (FParams.SessionId = 0) then
      Result := arErrUser  // 客户端缺少 SessionId, 当非法用户
    else
    if (FParams.SessionId = INI_SESSION_ID) or
       (FParams.SessionId = FSessionId) then
      Result := arOK       // 通过
    else
    if SessionValid(FParams.SessionId) then
      Result := arOK       // 通过
    else
      Result := arOutDate; // 凭证过期
  end;
begin
  // 检查第一请求数据包的有效性、用户登录状态（类似于 http 协议）

  if (FByteCount < IOCP_SOCKET_SIZE) or  // 长度太短
     (MatchSocketType(InBuf, IOCP_SOCKET_FLAG) = False) then // C/S 标志错误
  begin
    // 关闭返回
    InterCloseSocket(Self);
    Result := False;
  end else
  begin
    // 发送器
    Result := True;
    FAction := atUnknown;  // 内部事件（用于附件传输）
    FResult.FSender := FSender;

    // 先更新协议头
    FParams.FMsgHead := PMsgHead(InBuf + IOCP_SOCKET_FLEN); // 推送时用
    FParams.SetHeadMsg(FParams.FMsgHead);
    FResult.SetHeadMsg(FParams.FMsgHead, True);

    if (FParams.Action = atUnknown) then  // 1. 响应服务
      FReceiver.Complete := True
    else begin
      // 2. 检查登录状态
      if Assigned(TInIOCPServer(FServer).ClientManager) then
        FResult.ActResult := CheckLogState
      else begin // 免登录
        FResult.ActResult := arOK;
        if (FParams.FSessionId = INI_SESSION_ID) then
          FResult.FSessionId := CreateSession;
      end;
      if (FResult.ActResult in [arOffline, arOutDate]) then
        FReceiver.Complete := True  // 3. 接收完毕
      else // 4. 准备接收
        FReceiver.Prepare(InBuf, FByteCount);
    end;
  end;
end;

procedure TIOCPSocket.ClearResources;
begin
  // 清除资源
  if Assigned(FResult) then
    FReceiver.Clear;
  if Assigned(FParams) then
    FParams.Clear;
  if Assigned(FResult) then
    FResult.Clear;
  SetLogoutState;     // 登出
  {$IFDEF TRANSMIT_FILE}
  if Assigned(FTask) then  
    FTask.FreeResources(False);  // 已在 FResult.Clear 释放
  {$ENDIF}
end;

procedure TIOCPSocket.CreateResources;
begin
  // 建资源：接收、结果参数/变量表，数据接收器
  if (FReceiver = nil) then
  begin
    FParams := TReceiveParams.Create(Self);  // 在前
    FResult := TReturnResult.Create(Self, False);
    FReceiver := TServerReceiver.Create(FParams); // 在后
  end else
  if FReceiver.Complete then
  begin
    FParams.Clear;
    FResult.Clear;
  end;
end;

function TIOCPSocket.CreateSession: Cardinal;
var
  NowTime: TDateTime;
  Certify: TCertifyNumber;
  LHour, LMinute, LSecond, LMilliSecond: Word;
begin
  // 生成一个登录凭证，有效期为 SESSION_TIMEOUT 分钟
  //   结构：(相对日序号 + 有效分钟) xor 年
  NowTime := Now();

  DecodeTime(NowTime, LHour, LMinute, LSecond, LMilliSecond);

  Certify.DayCount := Trunc(NowTime - 43000);  // 相对日序号
  Certify.Timeout := LHour * 60 + LMinute + SESSION_TIMEOUT;

  if (Certify.Timeout >= 1440) then  // 超过一天的分钟数
  begin
    Inc(Certify.DayCount);  // 后一天
    Dec(Certify.Timeout, 1440);
  end;

  Result := Certify.Session xor Cardinal($AB12);
end;

destructor TIOCPSocket.Destroy;
begin
  // 释放资源
  if Assigned(FReceiver) then
  begin
    FReceiver.Free;
    FReceiver := Nil;
  end;
  if Assigned(FParams) then
  begin
    FParams.Free;
    FParams := Nil;
  end;
  if Assigned(FResult) then
  begin
    FResult.Free;
    FResult := Nil;
  end;
  inherited;
end;

procedure TIOCPSocket.ExecuteWork;
const
  IO_FIRST_PACKET = True;  // 首数据包
  IO_SUBSEQUENCE  = False; // 后续数据包
begin
  // 接收数据 FRecvBuf

  // 建资源
  CreateResources;   

  {$IFNDEF DELPHI_7}
  {$REGION '+ 接收数据'}
  {$ENDIF}
  
  // 1. 接收数据
  FTickCount := GetTickCount;
  
  case FReceiver.Complete of
    IO_FIRST_PACKET:  // 1.1 首数据包，检查有效性 和 用户登录状态
      if (CheckMsgHead(FRecvBuf^.Data.buf) = False) then
        Exit;
    IO_SUBSEQUENCE:   // 1.2 接收后续数据包
      FReceiver.Receive(FRecvBuf^.Data.buf, FByteCount);
  end;

  // 1.3 主体或附件接收完毕均进入应用层
  FComplete := FReceiver.Complete and (FReceiver.Cancel = False);

  {$IFNDEF DELPHI_7}
  {$ENDREGION}

  {$REGION '+ 进入应用层'}
  {$ENDIF}

  // 2. 进入应用层
  try
    if FComplete then   // 接收完毕、文件协议
      if FReceiver.CheckPassed then // 校验成功
        HandleDataPack  // 2.1 触发业务
      else
        ReturnMessage(arErrHash);  // 2.2 校验错误，反馈！
  finally
    // 2.3 继续投放 WSARecv，接收数据！
    {$IFDEF TRANSMIT_FILE}
    // 可能发出成功后任务被清在前！
    if (FTaskExists = False) then {$ENDIF}
      InternalRecv(FComplete);
  end;

  {$IFNDEF DELPHI_7}
  {$ENDREGION}
  {$ENDIF}

end;

function TIOCPSocket.GetLogName: string;
begin
  if Assigned(FEnvir) then
    Result := FEnvir^.BaseInf.Name
  else
    Result := '';
end;

function TIOCPSocket.GetRole: TClientRole;
begin
  if Assigned(FEnvir) then
    Result := Envir^.BaseInf.Role
  else
    Result := crUnknown;
end;

procedure TIOCPSocket.HandleDataPack;
begin
  // 执行客户端请求

  // 1. 响应 -> 直接反馈协议头
  if (FParams.Action = atUnknown) then
    ReturnHead(arOK)
  else

  // 2. 未登录（服务端不关闭）、对话过期 -> 反馈协议头
  if (FResult.ActResult in [arErrUser, arOutDate]) then
    ReturnMessage(FResult.ActResult)
  else

  // 3. 变量解析异常
  if FParams.Error then
    ReturnMessage(arErrAnalyse)
  else begin

    // 4. 进入应用层执行任务
    try
      FWorker.Execute(Self);
    except
      on E: Exception do  // 4.1 异常 -> 反馈
      begin
        ReturnMessage(arErrWork, E.Message);
        Exit;  // 4.2 返回
      end;
    end;

    try
      // 5. 主体+附件接收完毕 -> 清空
      FReceiver.OwnerClear;

      // 6. 非法用户，发送完毕要关闭
      if (FResult.ActResult = arErrUser) then
        InterlockedIncrement(FState);  // FState+

      // 7. 发送结果！
      ReturnResult;

      {$IFNDEF TRANSMIT_FILE}
      // 7.1 发送完成事件（附件未关闭）
      if Assigned(FResult.Attachment) then
      begin
        FAction := atAfterSend;
        FWorker.Execute(Self);
      end;
      {$ENDIF}
    finally
      {$IFNDEF TRANSMIT_FILE}
      if (FReceiver.Complete = False) then  // 附件未接收完毕
        FAction := atAfterReceive;  // 恢复
      if Assigned(FResult.Attachment) then
        FResult.Clear;
      {$ENDIF}
    end;

  end;

end;

procedure TIOCPSocket.IniSocket(AServer: TObject; ASocket: TSocket; AData: Pointer);
begin
  inherited;
  FSessionId := INI_SESSION_ID; // 初始凭证
end;

procedure TIOCPSocket.InterCloseSocket(Sender: TObject);
begin
  // 加入用户操作、名称，方便 OnDisconnect 使用
  if Assigned(FParams) then
  begin
    if (FParams.FAction <> atUserLogout) then
      FResult.FAction := atDisconnect;
    if Assigned(FEnvir) then
      FResult.UserName := FEnvir^.BaseInf.Name;
  end;
  inherited;
end;

{$IFDEF TRANSMIT_FILE}
procedure TIOCPSocket.InterFreeRes;
begin
  // 释放 TransmitFile 的发送资源
  try
    try
      if Assigned(FResult.Attachment) then  // 附件发送完毕
      begin
        FAction := atAfterSend;
        FWorker.Execute(Self);
      end;
    finally
      if (FReceiver.Complete = False) then  // 附件未接收完毕
        FAction := atAfterReceive;  // 恢复
      FResult.NilStreams(True);     // 释放发送资源
      FTask.FreeResources(False);   // False -> 不用再释放
    end;
  finally  // 继续投放 Recv
    InternalRecv(FComplete);
  end;
end;
{$ENDIF}

procedure TIOCPSocket.PostEvent(IOKind: TIODataType);
var
  Msg: TPushMessage;
begin
  // 构造、推送一个协议头消息（给自己）
  //   C/S 服务 IOKind 只有 ioDelete、ioRefuse、ioTimeOut，
  //  其他消息用：Push(ATarget: TBaseSocket; UseUniqueMsgId: Boolean);
  //  同时开启 HTTP 服务时，会在 THttpSocket 发出 arRefuse（未转换资源）

  // 3 秒内活动过，取消超时
  if (IOKind = ioTimeOut) and CheckDelayed(GetTickCount) then
    Exit;

  Msg := TPushMessage.Create(Self, IOKind, IOCP_SOCKET_SIZE);

  case IOKind of
    ioDelete:
      THeadMessage.CreateHead(Msg.PushBuf^.Data.buf, arDeleted);
    ioRefuse: 
      THeadMessage.CreateHead(Msg.PushBuf^.Data.buf, arRefuse);
    ioTimeOut:
      THeadMessage.CreateHead(Msg.PushBuf^.Data.buf, arTimeOut);
  end;

  // 加入推送列表，激活推送线程
  TInIOCPServer(FServer).PushManager.AddWork(Msg);
end;

procedure TIOCPSocket.SetLogoutState;
begin
  // 设置登出状态
  FSessionId := INI_SESSION_ID;
  if Assigned(FEnvir) then
    if FEnvir^.ReuseSession then  // 短连接断开，保留 FData 主要信息
    begin
      FEnvir^.BaseInf.Socket := 0;
      FEnvir^.BaseInf.LogoutTime := Now();
    end else
      try  // 释放登录信息
        TInIOCPServer(FServer).ClientManager.RemoveClient(FEnvir^.BaseInf.Name);
      finally
        FEnvir := Nil;
      end;
end;

procedure TIOCPSocket.ReturnHead(ActResult: TActionResult);
begin
  // 反馈协议头给客户端（响应、其他消息）
  //   格式：IOCP_HEAD_FLAG + TMsgHead

  // 更新协议头信息
  FResult.FDataSize := 0;
  FResult.FAttachSize := 0;
  FResult.FActResult := ActResult;

  if (FResult.FAction = atUnknown) then  // 响应
    FResult.FVarCount := FObjPool.UsedCount // 返回客户端数
  else
    FResult.FVarCount := 0;

  // 直接写入 FSender 的发送缓存
  FResult.LoadHead(FSender.Data);

  // 发送
  FSender.SendBuffers;
end;

procedure TIOCPSocket.ReturnMessage(ActResult: TActionResult; const ErrMsg: String);
begin
  // 反馈协议头给客户端

  FParams.Clear;
  FResult.Clear;
  
  if (ErrMsg <> '') then
  begin
    FResult.Msg := ErrMsg;
    FResult.ActResult := ActResult;
    ReturnResult;
  end else
    ReturnHead(ActResult);

  case ActResult of
    arOffline:
      iocp_log.WriteLog(Self.ClassName + '->客户端未登录.');
    arOutDate:
      iocp_log.WriteLog(Self.ClassName + '->凭证/认证过期.');
    arErrAnalyse:
      iocp_log.WriteLog(Self.ClassName + '->变量解析异常.');
    arErrHash:
      iocp_log.WriteLog(Self.ClassName + '->校验异常！');
    arErrWork:
      iocp_log.WriteLog(Self.ClassName + '->执行异常, ' + ErrMsg);
  end;

end;

procedure TIOCPSocket.ReturnResult;
  procedure SendMsgHeader;
  begin
    // 发送协议头（描述）
    FResult.LoadHead(FSender.Data);
    FSender.SendBuffers;
  end;
  procedure SendMsgEntity;
  begin
    // 发送主体数据流
    {$IFDEF TRANSMIT_FILE}
    // 设置主体数据流
    FTask.SetTask(FResult.FMain, FResult.FDataSize);
    {$ELSE}
    FSender.Send(FResult.FMain, FResult.FDataSize, False);  // 不关闭资源
    {$ENDIF}
  end;
  procedure SendMsgAttachment;
  begin
    // 发送附件数据
    {$IFDEF TRANSMIT_FILE}
    // 设置附件数据流
    if (FResult.FAction = atFileDownChunk) then
      FTask.SetTask(FResult.FAttachment, FResult.FAttachSize,
                    FResult.FOffset, FResult.FOffsetEnd)
    else
      FTask.SetTask(FResult.FAttachment, FResult.FAttachSize);
    {$ELSE}
    if (FResult.FAction = atFileDownChunk) then  // 不关闭资源
      FSender.Send(FResult.FAttachment, FResult.FAttachSize,
                   FResult.FOffset, FResult.FOffsetEnd, False)
    else
      FSender.Send(FResult.FAttachment, FResult.FAttachSize, False);
    {$ENDIF}
  end;
begin
  // 发送结果给客户端，不用请求，直接发送
  //  附件流是 TIOCPDocument，还要用，不能释放
  //   见：TIOCPSocket.HandleDataPack; TClientParams.InternalSend

  // FSender.Socket、Owner 已经设置

  try
    // 1. 准备数据流
    FResult.CreateStreams;

    if (FResult.Error = False) then
    begin
      // 2. 发协议头
      SendMsgHeader;

      // 3. 主体数据（内存流）
      if (FResult.FDataSize > 0) then
        SendMsgEntity;

      // 4. 发送附件数据
      if (FResult.FAttachSize > 0) then
        SendMsgAttachment;
    end;
  finally
    {$IFDEF TRANSMIT_FILE}
    InterTransmit;
    {$ELSE}
    NilStreams(False);  // 5. 清空，暂不释放附件流
    {$ENDIF}
  end;

end;

procedure TIOCPSocket.SocketError(IOKind: TIODataType);
begin
  // 处理收发异常
  if (IOKind in [ioDelete, ioPush, ioRefuse]) then  // 推送
    FResult.ActResult := arErrPush;
  inherited;
end;

procedure TIOCPSocket.SetLogState(AEnvir: PEnvironmentVar);
begin
  // 设置登录/登出信息
  if (AEnvir = nil) then  // 登出
  begin
    SetLogoutState;
    FResult.FSessionId := FSessionId;
    FResult.ActResult := arLogout;  // 不是 arOffline
    if Assigned(FEnvir) then  // 不关联到 Socket
      FEnvir := nil;
  end else
  begin
    FSessionId := CreateSession;  // 重建对话期
    FResult.FSessionId := FSessionId;
    FResult.ActResult := arOK;
    FEnvir := AEnvir;
  end;
end;

function TIOCPSocket.SessionValid(ASession: Cardinal): Boolean;
var
  NowTime: TDateTime;
  Certify: TCertifyNumber;
  LHour, LMinute, LSecond, LMilliSecond: Word;  
begin
  // 检查凭证是否正确且没超时
  //   结构：(相对日序号 + 有效分钟) xor 年
  NowTime := Now();

  DecodeTime(NowTime, LHour, LMinute, LSecond, LMilliSecond);

  LMinute := LHour * 60 + LMinute;  // 临时存放
  LSecond :=  Trunc(NowTime - 43000);  // 临时存放
  Certify.Session := ASession xor Cardinal($AB12);

  Result := (Certify.DayCount = LSecond) and (Certify.Timeout > LMinute) or
            (Certify.DayCount = LSecond + 1) and (Certify.Timeout > (1440 - LMinute));

  if Result then
    FSessionId := Certify.Session;
end;

{ THttpSocket }

procedure THttpSocket.ClearResources;
begin
  // 清除资源
  CloseStream;
  if Assigned(FRequest) then
    FRequest.Clear;
  if Assigned(FRespone) then
    FRespone.Clear;
  {$IFDEF TRANSMIT_FILE}
  if Assigned(FTask) then  
    FTask.FreeResources(False);  // 已在 FResult.Clear 释放
  {$ENDIF}    
end;

procedure THttpSocket.CloseStream;
begin
  if Assigned(FStream) then
  begin
    FStream.Free;
    FStream := Nil;
  end;
end;

procedure THttpSocket.CreateStream(const FileName: String);
begin
  // 建文件流
  FStream := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
end;

procedure THttpSocket.DecodeHttpRequest;
begin
  // 请求命令解码（单数据包）
  //   在业务线程建 FRequest、FRespone，加快接入速度
  if (FRequest = nil) then
  begin
    FRequest := THttpRequest.Create(TInIOCPServer(FServer).HttpDataProvider, Self); // http 请求
    FRespone := THttpRespone.Create(TInIOCPServer(FServer).HttpDataProvider, Self); // http 应答
  end;
  // 更新时间、HTTP 命令解码
  FTickCount := GetTickCount;
  TRequestObject(FRequest).Decode(FSender, FRespone, FRecvBuf);
end;

destructor THttpSocket.Destroy;
begin
  CloseStream;
  if Assigned(FRequest) then
  begin
    FRequest.Free;
    FRequest := Nil;
  end;
  if Assigned(FRespone) then
  begin
    FRespone.Free;
    FRespone := Nil;
  end;
  inherited;
end;

procedure THttpSocket.ExecuteWork;
begin
  // 执行 Http 请求
  try
    // 0. 使用 C/S 协议时，要转换为 TIOCPSocket
    if (FTickCount = NEW_TICKCOUNT) and (FByteCount = IOCP_SOCKET_FLEN) and
      MatchSocketType(FRecvBuf^.Data.Buf, IOCP_SOCKET_FLAG) then
    begin
      UpgradeSocket(TInIOCPServer(FServer).IOCPSocketPool);
      Exit;  // 返回
    end;

    // 1. 命令解码
    DecodeHttpRequest;

    // 2. 检查升级 WebSocket
    if (FRequest.UpgradeState > 0) then
    begin
      if FRequest.Accepted then  // 升级为 WebSocket，不能返回 THttpSocket 了
      begin
        TResponeObject(FRespone).Upgrade;
        UpgradeSocket(TInIOCPServer(FServer).WebSocketPool);
      end else  // 不允许升级，关闭
        InterCloseSocket(Self);
      Exit;     // 返回
    end;

    // 3. 执行业务
    FComplete := FRequest.Complete;   // 是否接收完毕
    FSessionId := FRespone.SessionId; // SendWork 时会被删除
    FRespone.StatusCode := FRequest.StatusCode;

    if FComplete and FRequest.Accepted and (FRequest.StatusCode < 400) then
      FWorker.HttpExecute(Self);

    // 4. 检查是否要保持连接
    if FRequest.Attacked then // 被攻击
      FKeepAlive := False
    else
      if FComplete or (FRespone.StatusCode >= 400) then  // 接收完毕或异常
      begin
        // 是否保存连接
        FKeepAlive := FRespone.KeepAlive;

        // 5. 发送内容给客户端
        TResponeObject(FRespone).SendWork;

        if {$IFNDEF TRANSMIT_FILE} FKeepAlive {$ELSE}
           (FTaskExists = False) {$ENDIF} then // 6. 清资源，准备下次请求
          ClearResources;
      end else
        FKeepAlive := True;   // 未完成，还不能关闭

    // 7. 继续投放或关闭
    if FKeepAlive and (FErrorCode = 0) then  // 继续投放
    begin
      {$IFDEF TRANSMIT_FILE}
      if (FTaskExists = False) then {$ENDIF}
        InternalRecv(FComplete);
    end else
      InterCloseSocket(Self);  // 关闭时清资源
                 
  except
    // 大并发时，在此设断点，调试是否异常
    iocp_log.WriteLog('THttpSocket.ExecuteHttpWork->' + GetSysErrorMessage);
    InterCloseSocket(Self);  // 系统异常
  end;

end;

{$IFDEF TRANSMIT_FILE}
procedure THttpSocket.InterFreeRes;
begin
  // 发送完毕，释放 TransmitFile 的发送资源
  try
    ClearResources;
  finally
    if FKeepAlive and (FErrorCode = 0) then  // 继续投放
      InternalRecv(True)
    else
      InterCloseSocket(Self);
  end;
end;
{$ENDIF}

procedure THttpSocket.PostEvent(IOKind: TIODataType);
const
  REQUEST_NOT_ACCEPTABLE = HTTP_VER + ' 406 Not Acceptable';
        REQUEST_TIME_OUT = HTTP_VER + ' 408 Request Time-out';
var
  Msg: TPushMessage;
  ResponeMsg: AnsiString;
begin
  // 构造、推送一个消息头（给自己）
  //   HTTP 服务只有 arRefuse、arTimeOut

  // TransmitFile 有任务或 3 秒内活动过，取消超时
  if (IOKind = ioTimeOut) and CheckDelayed(GetTickCount) then
    Exit;

  if (IOKind = ioRefuse) then
    ResponeMsg := REQUEST_NOT_ACCEPTABLE + STR_CRLF +
                  'Server: ' + HTTP_SERVER_NAME + STR_CRLF +
                  'Date: ' + GetHttpGMTDateTime + STR_CRLF +
                  'Content-Length: 0' + STR_CRLF +
                  'Connection: Close' + STR_CRLF2
  else
    ResponeMsg := REQUEST_TIME_OUT + STR_CRLF +
                  'Server: ' + HTTP_SERVER_NAME + STR_CRLF +
                  'Date: ' + GetHttpGMTDateTime + STR_CRLF +
                  'Content-Length: 0' + STR_CRLF +
                  'Connection: Close' + STR_CRLF2;

  Msg := TPushMessage.Create(Self, IOKind, Length(ResponeMsg));

  System.Move(ResponeMsg[1], Msg.PushBuf^.Data.buf^, Msg.PushBuf^.Data.len);

  // 加入推送列表，激活线程
  TInIOCPServer(FServer).PushManager.AddWork(Msg);

end;

procedure THttpSocket.SocketError(IOKind: TIODataType);
begin
  // 处理收发异常
  if Assigned(FRespone) then      // 接入时 = Nil
    FRespone.StatusCode := 500;   // 500: Internal Server Error
  inherited;
end;

procedure THttpSocket.UpgradeSocket(SocketPool: TIOCPSocketPool);
var
  oSocket: TBaseSocket;
begin
  // 把 THttpSocket 转换为 TIOCPSocket、TWebSocket，
  try
    oSocket := TBaseSocket(SocketPool.Clone(Self));  // 未建 FTask！
    oSocket.PostRecv;  // 投放
  finally
    InterCloseSocket(Self);  // 关闭自身
  end;
end;

procedure THttpSocket.WriteStream(Data: PAnsiChar; DataLength: Integer);
begin
  // 保存数据到文件流
  if Assigned(FStream) then
    FStream.Write(Data^, DataLength);
end;

{ TWebSocket }

procedure TWebSocket.ClearOwnerMark;
var
  p: PAnsiChar;
begin
  // 清除掩码、设宿主=0
  FReceiver.ClearMark(FData, @FRecvBuf^.Overlapped);
  if (FMsgType = mtJSON) then  // 清除 Owner,  设为 0（表示服务端）
  begin
    p := FData;
    Inc(p, Length(INIOCP_JSON_FLAG));
    if SearchInBuffer(p, FRecvBuf^.Overlapped.InternalHigh, '"__MSG_OWNER":') then // 区分大小写
      while (p^ <> AnsiChar(',')) do
      begin
        p^ := AnsiChar('0');
        Inc(p);
      end;
  end;
end;

procedure TWebSocket.ClearResources;
begin
  if Assigned(FReceiver) then
    FReceiver.Clear;
end;

constructor TWebSocket.Create(AObjPool: TIOCPSocketPool; ALinkNode: PLinkRec);
begin
  inherited;
  FUseTransObj := False;  // 不用 TransmitFile
  FJSON := TBaseJSON.Create(Self);
  FResult := TResultJSON.Create(Self);
  FResult.FServerMode := True;  // 服务器模式
  FReceiver := TWSServerReceiver.Create(Self, FJSON);
end;

destructor TWebSocket.Destroy;
begin
  FJSON.Free;
  FResult.Free;
  FReceiver.Free;
  inherited;
end;

procedure TWebSocket.ExecuteWork;
begin
  // 接收数据，执行任务

  // 1. 接收数据
  FTickCount := GetTickCount;

  if FReceiver.Complete then  // 首数据包
  begin
    // 分析、接收数据
    // 可能改变 FMsgType，见 FReceiver.UnMarkData
    FMsgType := mtDefault;
    FReceiver.Prepare(FRecvBuf^.Data.buf, FByteCount);
    case FReceiver.OpCode of
      ocClose: begin
        InterCloseSocket(Self);  // 关闭，返回
        Exit;
      end;
      ocPing, ocPong: begin
        InternalRecv(True);  // 投放，返回
        Exit;
      end;
    end;
  end else
  begin
    // 接收后续数据包
    FReceiver.Receive(FRecvBuf^.Data.buf, FByteCount);
  end;

  // 是否接收完成
  FComplete := FReceiver.Complete;

  // 2. 进入应用层
  // 2.1 标准操作，每接收一次即进入
  // 2.2 扩展的操作，是 JSON 消息，接收完毕才进入

  try
    if (FMsgType = mtDefault) or FComplete then
    begin
      if (FMsgType <> mtDefault) then
        FResult.Action := FJSON.Action;  // 复制 Action
      FWorker.WSExecute(Self);
    end;
  finally
    if FComplete then   // 接收完毕
    begin
      case FMsgType of
        mtJSON: begin   // 扩展的 JSON
          FJSON.Clear;  // 不清 Attachment
          FResult.Clear;
          FReceiver.Clear;
        end;
        mtAttachment: begin  // 扩展的附件流
          FJSON.Close;  // 关闭附件流
          FResult.Clear;
          FReceiver.Clear;
        end;
      end;
      // ping 客户端
      InternalPing;  
    end;
    // 继续接收
    InternalRecv(FComplete);
  end;

end;

procedure TWebSocket.InternalPing;
begin
  // Ping 客户端，等新的信息缓存接收完成
  MakeFrameHeader(FSender.Data, ocPing);
  FSender.SendBuffers;
end;

procedure TWebSocket.PostEvent(IOKind: TIODataType);
begin
  // Empty
end;

procedure TWebSocket.SendData(const Msg: String);
begin
  // 未封装：发送文本
  if (Msg <> '') then
  begin
    FSender.OpCode := ocText;
    FSender.Send(System.AnsiToUtf8(Msg));
  end;
end;

procedure TWebSocket.SendData(const Data: PAnsiChar; Size: Cardinal);
var
  Buf: PAnsiChar;
begin
  // 未封装：发送内存块数据（复制 Data）
  if Assigned(Data) and (Size > 0) then
  begin
    GetMem(Buf, Size);
    System.Move(Data^, Buf^, Size);
    FSender.OpCode := ocBiary;
    FSender.Send(Buf, Size);
  end;
end;

procedure TWebSocket.SendData(Handle: THandle);
begin
  // 未封装：发送文件 handle（自动关闭）
  if (Handle > 0) and (Handle <> INVALID_HANDLE_VALUE) then
  begin
    FSender.OpCode := ocBiary;
    FSender.Send(Handle, GetFileSize64(Handle));
  end;
end;

procedure TWebSocket.SendData(Stream: TStream);
begin
  // 未封装：发送流数据（自动释放）
  if Assigned(Stream) then
  begin
    FSender.OpCode := ocBiary;
    FSender.Send(Stream, Stream.Size, True);
  end;
end;

procedure TWebSocket.SendDataVar(Data: Variant);
begin
  // 未封装：发送可变类型数据
  if (VarIsNull(Data) = False) then
  begin
    FSender.OpCode := ocBiary;
    FSender.SendVar(Data);
  end;
end;

procedure TWebSocket.SendResult(UTF8CharSet: Boolean);
begin
  // 发送 FResult 给客户端（InIOCP-JSON）
  FResult.FOwner := FJSON.Owner;
  FResult.FUTF8CharSet := UTF8CharSet;
  FResult.InternalSend(FSender, False);
end;

procedure TWebSocket.SetProps(AOpCode: TWSOpCode; AMsgType: TWSMsgType;
                     AData: Pointer; AFrameSize: Int64; ARecvSize: Cardinal);
begin
  // 更新，见：TWSServerReceiver.InitResources
  FMsgType := AMsgType;  // 数据类型
  FOpCode := AOpCode;  // 操作
  FMsgSize := 0;  // 消息长度
  FData := AData; // 引用地址
  FFrameSize := AFrameSize;  // 帧长度
  FFrameRecvSize := ARecvSize;  // 收到帧长度
end;

{ TSocketBroker }

procedure TSocketBroker.AssociateInner(InnerBroker: TSocketBroker);
begin
  // 外部代理：和内部 Socket 关联起来（已经投放 WSARecv）
  try
    // 转移资源
    FDualConnected := True;
    FDualSocket := InnerBroker.FSocket;
    FDualBuf := InnerBroker.FRecvBuf;
    FDualBuf^.Owner := Self;  // 改宿主
    FPeerIPPort := 'Dual:' + FPeerIPPort;
  finally
    // 清除原资源值
    InnerBroker.FConnected := False;
    InnerBroker.FSocket := INVALID_SOCKET;
    InnerBroker.FRecvBuf := nil;
    // 回收 TBaseSocket
    InnerBroker.InterCloseSocket(InnerBroker);
  end;
  if (FSocketType = stWebSocket) or (TInIOCPBroker(FBroker).Protocol = tpNone) then
    FOnBind := nil;  // 删除绑定事件，以后不再绑定！ }
end;

procedure TSocketBroker.BrokerPostRecv(ASocket: TSocket; AData: PPerIOData; ACheckState: Boolean);
var
  ByteCount, Flags: DWORD;
begin
  // 投放 WSRecv: ASocket, AData

  // 正常时 FState=1，其他任何值都说明出现了异常，
  // FState = 1 -> 正常，否则改变了状态，关闭！

  if ACheckState and (InterlockedDecrement(FState) <> 0) then
  begin
    FErrorCode := 9;
    InterCloseSocket(Self);
  end else
  begin
    // 清重叠结构
    FillChar(AData^.Overlapped, SizeOf(TOverlapped), 0);

    AData^.Owner := Self;  // 宿主
    AData^.IOType := ioReceive;  // iocp_server 中判断用
    AData^.Data.len := IO_BUFFER_SIZE;  // 长度

    ByteCount := 0;
    Flags := 0;

    if (iocp_Winsock2.WSARecv(ASocket, @(AData^.Data), 1, ByteCount,
        Flags, LPWSAOVERLAPPED(@AData^.Overlapped), nil) = SOCKET_ERROR) then
    begin
      FErrorCode := WSAGetLastError;
      if (FErrorCode <> ERROR_IO_PENDING) then  // 异常
      begin
        SocketError(ioReceive);
        InterCloseSocket(Self);  // 关闭
      end else
        FErrorCode := 0;
    end;
  end;
end;

procedure TSocketBroker.ClearResources;
begin
  // 反向代理：未关联的连接被断开，向外补发连接
  if TInIOCPBroker(FBroker).ReverseMode and (FSocketType = stOuterSocket) then
    TIOCPBrokerRef(FBroker).ConnectOuter;
  if FDualConnected then  // 尝试关闭
    TryClose;
end;

procedure TSocketBroker.CreateBroker(const AServer: AnsiString; APort: Integer);
begin
  // 新建一个内部代理（中继套接字），不能改变连接

  if (FDualSocket <> INVALID_SOCKET) or
     (TInIOCPServer(FServer).ServerAddr = AServer) and  // 不能连接到服务器自身
     (TInIOCPServer(FServer).ServerPort = APort) then
    Exit;
    
  // 建套接字
  FDualSocket := iocp_utils.CreateSocket;

  if (ConnectSocket(FDualSocket, AServer, APort) = False) then  // 连接
  begin
    FDualConnected := False;
    FErrorCode := GetLastError;
    iocp_log.WriteLog('TSocketBroker.CreateBroker:ConnectSocket->' + GetSysErrorMessage(FErrorCode));
    InterCloseSocket(Self);  // 关闭代理
  end else
  if TInIOCPServer(FServer).IOCPEngine.BindIoCompletionPort(FDualSocket) then  // 绑定
  begin
    FDualConnected := True;
    FTargetHost := AServer;
    FTargetPort := APort;
    FPeerIPPort := 'Dual:' + FPeerIPPort;  // 加入标志

    // 分配接收内存块
    if (FDualBuf = nil) then
      FDualBuf := BufferPool.Pop^.Data;

    BrokerPostRecv(FDualSocket, FDualBuf, False);  // 投放 FDualSocket

    if (FErrorCode > 0) then  // 异常
      FDualConnected := False
    else
    if TInIOCPBroker(FBroker).ReverseMode then  // 反向代理，向外补发连接
    begin
      FSocketType := stDefault;  // 改变（关闭时不补充连接）
      TIOCPBrokerRef(FBroker).ConnectOuter;
    end;

    FOnBind := nil;  // 删除绑定事件，以后不再绑定！
  end else
  begin
    FDualConnected := False;
    FErrorCode := GetLastError;
    InterCloseSocket(Self);  // 关闭代理
  end;

end;

procedure TSocketBroker.ExecuteWork;
  function CheckInnerSocket: Boolean;
  begin
    // ++外部代理模式，两种连接：
    // 1、外部客户端，数据不带 InIOCP_INNER_SOCKET
    // 2、内部的反向代理客户端，数据带 InIOCP_INNER_SOCKET:InnerBrokerId
    if (PInIOCPInnerSocket(FRecvBuf^.Data.buf)^ = InIOCP_INNER_SOCKET) then
    begin
      // 这是内部的反向代理连接，保存到列表，在 TInIOCPBroker.BindBroker 配对
      SetString(FBrokerId, FRecvBuf^.Data.buf + Length(InIOCP_INNER_SOCKET) + 1,
                           Integer(FByteCount) - Length(InIOCP_INNER_SOCKET) - 1);
      TIOCPBrokerRef(FBroker).AddConnection(Self, FBrokerId);
      Result := True;
    end else
      Result := False;  // 这是外部的客户端连接
  end;
  procedure ExecSocketAction;
  begin
    // 发送内部连接标志到外部代理
    try
      if (TInIOCPBroker(FBroker).BrokerId = '') then  // 用默认标志
        FSender.Send(InIOCP_INNER_SOCKET + ':DEFAULT')
      else  // 同时发送代理标志，方便外部代理区分
        FSender.Send(InIOCP_INNER_SOCKET + ':' + UpperCase(TInIOCPBroker(FBroker).BrokerId));
    finally
      FAction := 0;
    end;
  end;
  procedure ForwardDataEx(ASocket, AToSocket: TSocket; AData: PPerIOData; MaskInt: Integer);
  begin
    try
      // 不能简单互换数据块，否则大并发时 AData 被重复投放 -> 995 异常
      FSender.Socket := AToSocket;  // 发给 AToSocket
      FRecvState := FRecvState and MaskInt;  // 去除状态
      TServerTaskSender(FSender).CopySend(AData);  // 发送数据
    finally
      if (FErrorCode = 0) then
        BrokerPostRecv(ASocket, AData)  // 继续投放 WSRecv
      else
        InterCloseSocket(Self);
    end;
  end;
  procedure ForwardData;
  begin
    // 复制、转发数据
    if FCmdConnect then  // Http代理: Connect 请求，响应
    begin
      FCmdConnect := False;
      FSender.Send(HTTP_PROXY_RESPONE);
      BrokerPostRecv(FSocket, FRecvBuf);
    end else
    if (FRecvState and $0001 = 1) then
      ForwardDataEx(FSocket, FDualSocket, FRecvBuf, 2)
    else
      ForwardDataEx(FDualSocket, FSocket, FDualBuf, 1);
  end;
begin
  // 执行：
  //   1、绑定、关联，发送外部数据到 FDualSocket
  //   2、已经关联时直接发送到 FDualSocket

  // 要合理设置 TInIOCPBroker.ProxyType

  FTickCount := GetTickCount;
  
  case TInIOCPBroker(FBroker).ProxyType of
    ptDefault: // 默认代理模式
      if (FAction > 0) then  // 见：SendInnerFlag
      begin
        ExecSocketAction;    // 执行操作，返回
        BrokerPostRecv(FSocket, FRecvBuf);  // 投放
        Exit;
      end;
    ptOuter:   // 外部代理模式
      if (FDualConnected = False) and CheckInnerSocket then  // 是内部反向代理的连接
      begin
        BrokerPostRecv(FSocket, FRecvBuf); // 先投放
        Exit;
      end;
  end;

  if (Assigned(FOnBind) = False) then  // 无绑定方法  
    ForwardData  // 转发数据
  else  // 开始时 FOnBind <> nil
    try
      FOnBind(Self, FRecvBuf^.Data.buf, FByteCount)  // 绑定、关联
    finally
      if FDualConnected then  // 转发数据
        ForwardData;
    end;

end;

procedure TSocketBroker.HttpBindOuter(Connection: TSocketBroker; const Data: PAnsiChar; DataSize: Cardinal);
  function ChangeConnection(const ABrokerId, AServer: AnsiString; APort: Integer): Boolean;
  begin
    // 检查关联目的是否已经改变
    if (TInIOCPBroker(FBroker).ProxyType = ptOuter) then
      Result := (ABrokerId <> FBrokerId)  // 反向代理改变
    else
      Result := not (((AServer = FTargetHost) or
                      (LowerCase(AServer) = 'localhost') and (FTargetHost = '127.0.0.1') or
                      (LowerCase(FTargetHost) = 'localhost') and (AServer = '127.0.0.1')) and (
                      (APort = FTargetPort) or (APort = TInIOCPServer(FServer).ServerPort)));
  end;
  procedure GetConnectHost(var p: PAnsiChar);
  var
    pb: PAnsiChar;
    i: Integer;
  begin
    // 提取主机地址：CONNECT xxx:443 HTTP/1.1
    Delete(FTargetHost, 1, Length(FTargetHost));

    pb := nil;
    Inc(p, 7);  // connect

    for i := 1 to FByteCount do
    begin
      if (p^ = #32) then
        if (pb = nil) then  // 地址开始
          pb := p
        else begin  // 地址结束，简化判断版本
          SetString(FTargetHost, pb, p - pb);
          FTargetHost := Trim(FTargetHost);
          Break;
        end;
      Inc(p);
    end;
  end;
  procedure GetHttpHost(var p: PAnsiChar);
  var
    pb: PAnsiChar;
  begin
    // 提取主机地址：HOST:
    pb := nil;
    Inc(p, 4);
    repeat
      case p^ of
        ':':
          if (pb = nil) then
            pb := p + 1;
        #13: begin
          SetString(FTargetHost, pb, p - pb);
          FTargetHost := Trim(FTargetHost);
          Exit;
        end;
      end;
      Inc(p);
    until (p^ = #10);
  end;
  procedure GetUpgradeType(var p: PAnsiChar);
  var
    S: AnsiString;
    pb: PAnsiChar;
  begin
    // 提取内容长度：UPGRADE: WebSocket
    pb := nil;
    Inc(p, 14);
    repeat
      case p^ of
        ':':
          pb := p + 1;
        #13: begin
          SetString(S, pb, p - pb);
          if (UpperCase(Trim(S)) = 'WEBSOCKET') then
            FSocketType := stWebSocket;
          Exit;
        end;
      end;
      Inc(p);
    until (p^ = #10);
  end;
  procedure ExtractHostPort;
  var
    i, j, k: Integer;
  begin
    // 分离 Host、Port 和 局域网标志

    j := 0;
    k := 0;

    for i := 1 to Length(FTargetHost) do  // 127.0.0.1:800@DEFAULT
      case FTargetHost[i] of
        ':':
          j := i;
        '@':  // HTTP 反向代理扩展，后面为局域网/分公司标志
          k := i;
      end;

    if (k > 0) then  // 反向代理标志
    begin
      if (TInIOCPBroker(FBroker).ProxyType = ptOuter) then  // 外部代理才用
        FBrokerId := Copy(FTargetHost, k + 1, 99);
      Delete(FTargetHost, k, 99);
    end;

    if (j > 0) then  // 内部主机
    begin
      TryStrToInt(Copy(FTargetHost, j + 1, 99), FTargetPort);
      Delete(FTargetHost, j, 99);
    end;
    
  end;
  procedure HttpRequestDecode;
  var
    iState: Integer;
    pE, pb, p: PAnsiChar;
  begin
    // Http 协议：提取请求信息：Host、Upgrade

    p := FRecvBuf^.Data.buf;  // 开始位置
    pE := PAnsiChar(p + FByteCount);  // 结束位置

    // 1、HTTP代理：Connect 命令，代理=443
    if http_utils.CompareBuffer(p, 'CONNECT', True) then
    begin
      FCmdConnect := True;
      GetConnectHost(p);  // p 改变
      ExtractHostPort;
      Exit;
    end;

    // 2、其他 HTTP 命令
    
    iState := 0;  // 信息状态
    FCmdConnect := False;
    FTargetPort := 80;  // 默认端口，代理=443
    pb := nil;

    Inc(p, 12);

    repeat
      case p^ of
        #10:  // 分行符
          pb := p + 1;

        #13:  // 回车符
          if (pb <> nil) then
            if (p = pb) then  // 出现连续的回车换行，报头结束
            begin
              Inc(p, 2);
              Break;
            end else
            if (p - pb >= 15) then
            begin
              if http_utils.CompareBuffer(pb, 'HOST', True) then
              begin
                Inc(iState);
                GetHttpHost(pb);
                ExtractHostPort;
              end else
              if http_utils.CompareBuffer(pb, 'UPGRADE', True) then  // WebSocket
              begin
                Inc(iState, 2);
                GetUpgradeType(pb);
              end;
            end;
      end;

      Inc(p);
    until (p >= pE) or (iState = 3);

  end;
  procedure HttpConnectHost(const AServer: AnsiString; APort: Integer);
  begin
    // Http 协议：连接到请求的主机 HOST，没有时连接到参数指定的
    if (FTargetHost <> '') and (FTargetPort > 0) then
    begin
      CreateBroker(FTargetHost, FTargetPort)
    end else
    if (AServer <> '') and (APort > 0) then  // 用参数指定的
      CreateBroker(AServer, APort)
    else
      InterCloseSocket(Self);  // 关闭
  end;
var
  Accept: Boolean;
begin
  // Http 协议：检查 Connect 命令和其他命令的 Host

  // 提取 Host 信息
  HttpRequestDecode;

  // 不能连接到服务器自身
  if (TInIOCPServer(FServer).ServerAddr = FTargetHost) and
     (TInIOCPServer(FServer).ServerPort = FTargetPort) then
    Exit;

  Accept := True;
  if Assigned(TInIOCPBroker(FBroker).OnAccept) then  // 是否允许连接
    TInIOCPBroker(FBroker).OnAccept(Self, FTargetHost, FTargetPort, Accept);

  if Accept then
    if (TInIOCPBroker(FBroker).ProxyType = ptDefault) then  // 默认代理：新建连接，关联
      HttpConnectHost(TInIOCPBroker(FBroker).InnerServer.ServerAddr,
                      TInIOCPBroker(FBroker).InnerServer.ServerPort)
    else  // 内部代理：从内部连接池选取关联对象
    if (FDualConnected = False) or
      ChangeConnection(FBrokerId, FTargetHost, FTargetPort) then
      TIOCPBrokerRef(FBroker).BindInnerBroker(Connection, Data, DataSize);

end;

procedure TSocketBroker.IniSocket(AServer: TObject; ASocket: TSocket; AData: Pointer);
begin
  inherited;
  FDualConnected := False;  // Dual 未连接
  FDualSocket := INVALID_SOCKET;

  FRecvState := 0;
  FTargetHost := '';
  FTargetPort := 0;
  FUseTransObj := False;  // 不用 TransmitFile

  // 代理管理器
  FBroker := TInIOCPServer(FServer).IOCPBroker;

  if (TInIOCPBroker(FBroker).ProxyType = ptOuter) then  
    FBrokerId := 'DEFAULT';  // 默认的 FBrokerId

  if (TInIOCPBroker(FBroker).Protocol = tpHTTP) then
    FOnBind := HttpBindOuter  // http 协议，直接内部绑定、解析
  else
  if (TInIOCPBroker(FBroker).ProxyType = ptDefault) then
    FOnBind := TInIOCPBroker(FBroker).OnBind  // 绑定组件事件
  else  // 外部代理，绑定组件方法
    FOnBind := TIOCPBrokerRef(FBroker).BindInnerBroker;

end;

procedure TSocketBroker.InterCloseSocket(Sender: TObject);
begin
  // 关闭 DualSocket
  if FDualConnected then
    try
      iocp_Winsock2.Shutdown(FDualSocket, SD_BOTH);
      iocp_Winsock2.CloseSocket(FDualSocket);
    finally
      FDualSocket := INVALID_SOCKET;
      FDualConnected := False;
    end;
  inherited;
end;

procedure TSocketBroker.MarkIODataBuf(AData: PPerIOData);
begin
  // AData 的接收状态
  if (AData = FRecvBuf) then
    FRecvState := FRecvState or $0001  // Windows.InterlockedIncrement(FRecvState)
  else
    FRecvState := FRecvState or $0002; // Windows.InterlockedExchangeAdd(FRecvState, 2);
end;

procedure TSocketBroker.PostEvent(IOKind: TIODataType);
begin
  InterCloseSocket(Self);  // 直接关闭，见 TInIOCPServer.AcceptClient
end;

procedure TSocketBroker.SendInnerFlag;
begin
  // 反向代理：向外部代理发送连接标志，在 ExecSocketAction 执行
  FAction := 1;  // 自身操作
  FState := 0;   // 解锁
  TInIOCPServer(FServer).BusiWorkMgr.AddWork(Self);
end;

procedure TSocketBroker.SetConnection(AServer: TObject; Connection: TSocket);
begin
  // 反向代理：设置主动发起到外部的连接
  IniSocket(AServer, Connection);
  FSocketType := stOuterSocket;  // 连接到外部的
  if (TInIOCPBroker(FBroker).Protocol = tpNone) then
    FOnBind := TInIOCPBroker(FBroker).OnBind; // 绑定关联事件
end;

end.
