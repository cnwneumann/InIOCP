(*
 * 服务端各种管理器单元
 *)
unit iocp_managers;

interface

{$I in_iocp.inc}        // 模式设置

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, {$ELSE}
  Windows, Classes, SysUtils, {$ENDIF}
  iocp_winSock2, iocp_base, iocp_log, iocp_baseObjs,
  iocp_objPools, iocp_lists, iocp_sockets, iocp_msgPacks,
  iocp_utils, iocp_baseModule, http_base, http_objects;

type

  // C/S 模式请求事件
  TRequestEvent = procedure(Sender: TObject;
                            Params: TReceiveParams;
                            Result: TReturnResult) of object;

  // =================== 用户工作环境管理 ===================

  // 用户登录、工作环境信息

  TWorkEnvironment = class(TStringHash)
  private
    function Logined(const UserName: String): Boolean; overload;
    function Logined(const UserName: String; var Socket: TObject): Boolean; overload;
  protected
    procedure FreeItemData(Item: PHashItem); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;
  
  // ================== 管理器 基类 ======================

  // 增加附件事件
  TAttachmentEvent = procedure(Sender: TObject; Params: TReceiveParams) of object;

  TBaseManager = class(TComponent)
  protected    
    FServer: TObject;  // TInIOCPServer 服务器
  private
    FOnAttachBegin: TAttachmentEvent;  // 准备接收附件
    FOnAttachFinish: TAttachmentEvent; // 附件接收完毕
    function GetGlobalLock: TThreadLock; {$IFDEF USE_INLINE} inline; {$ENDIF}
  protected
    procedure Execute(Socket: TIOCPSocket); virtual; abstract;
  protected
    property OnAttachBegin: TAttachmentEvent read FOnAttachBegin write FOnAttachBegin;
    property OnAttachFinish: TAttachmentEvent read FOnAttachFinish write FOnAttachFinish;
  public
    procedure AddToBackground(Socket: TBaseSocket);  // 后台执行
    procedure Wakeup(Socket: TBaseSocket);  // 后台执行完毕后，唤醒客户端
  public
    property GlobalLock: TThreadLock read GetGlobalLock;
  end;

  // ================== 流服务管理 类 ======================

  TDataInEvent   = procedure(Socket: TStreamSocket; const Data: PAnsiChar; Size: Cardinal; var Completed: Boolean) of object;
  TCopyDataEvent = procedure(Socket: TStreamSocket; const ABuffers: PAnsiChar) of object;

  TInStreamManager = class(TBaseManager)
  private
    FClientId: string;         // 待查找的客户端
    FOnReceive: TDataInEvent;  // 接收数据流事件
    function CheckState(const AClientId: String; var ASocket: TStreamSocket): Boolean; overload;
    procedure CallbackMethod(ObjType: TObjectType; var FromObject: Pointer;
                             const Data: TObject; var CancelScan: Boolean);
    procedure InterPushText(AOwner, ATarget: TStreamSocket; const AText: AnsiString = '');
  protected
    procedure Execute(Socket: TIOCPSocket); override;
  public
    function Logined(const AClientId: String): Boolean;
    function GetClientData(const AClientId: String; ABuffers: PAnsiChar; AEvent: TCopyDataEvent): Boolean;
    procedure Broadcast(AOwner: TStreamSocket; const AText: string = '');
    procedure SendTo(AOwner: TStreamSocket; const AToClient: string; const AText: string = '');
  published
    property OnReceive: TDataInEvent read FOnReceive write FOnReceive;
  end;

  // ================== 客户端管理 类 ======================

  // 管理：用户名称、权限，查询... ...

  TInClientManager = class(TBaseManager)
  private
    FClientList: TWorkEnvironment;    // 客户端工作环境列表
    FOnLogin: TRequestEvent;          // 登录事件
    FOnLogout: TRequestEvent;         // 登出事件
    FOnDelete: TRequestEvent;         // 删除客户端
    FOnModify: TRequestEvent;         // 修改客户端
    FOnRegister: TRequestEvent;       // 注册客户端
    FOnQueryState: TRequestEvent;     // 查询客户端状态
    procedure CopyClientInf(ObjType: TObjectType; var Buffer: Pointer;
                            const Data: TObject; var CancelScan: Boolean);
    procedure GetClients(Result: TReturnResult);
    procedure GetConnectedClients(Result: TReturnResult);
    procedure GetLoginedClients(Result: TReturnResult);
  protected
    procedure Execute(Socket: TIOCPSocket); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear;
    procedure Add(IOCPSocket: TIOCPSocket; ClientRole: TClientRole = crClient);
    procedure Disconnect(const UserName: String);
    procedure GetClientState(const UserName: String; Result: TReturnResult);
    procedure RemoveClient(const UserName: string);
  public
    function Logined(const UserName: String): Boolean; overload;
    function Logined(const UserName: String; var Socket: TIOCPSocket): Boolean; overload;
  published
    property OnDelete: TRequestEvent read FOnDelete write FOnDelete;
    property OnModify: TRequestEvent read FOnModify write FOnModify;
    property OnLogin: TRequestEvent read FOnLogin write FOnLogin;
    property OnLogout: TRequestEvent read FOnLogout write FOnLogout;
    property OnRegister: TRequestEvent read FOnRegister write FOnRegister;
    property OnQueryState: TRequestEvent read FOnQueryState write FOnQueryState;
  end;

  // ================== 消息管理器 ======================

  TInMessageManager = class(TBaseManager)
  private
    FMsgWriter: TMessageWriter;  // 消息书写器
    FOnBroadcast: TRequestEvent; // 广播
    FOnGet: TRequestEvent;       // 取离线消息
    FOnListFiles: TRequestEvent; // 取离线消息文件列表
    FOnPush: TRequestEvent;      // 推送
    FOnReceive: TRequestEvent;   // 收到消息
    procedure InterCopyPush(ASource, ATarget: TIOCPSocket; AResult: TReturnResult;
                            AToSockets: TInList = nil; CastType: TBroadcastType = btUnknown);
    procedure InterPushMsg(ASource, ATarget: TIOCPSocket; AToSockets: TInList = nil;
                           CastType: TBroadcastType = btUnknown);
    procedure InterPushResultMsg(Socket: TIOCPSocket; Result: TReturnResult;
                                 UserNameList: AnsiString);
    procedure InterSavePushMsg(AOwner: TIOCPSocket; PushMsg: TInList; UserNameList: AnsiString);
  protected
    procedure Execute(Socket: TIOCPSocket); override;
  public
    destructor Destroy; override;
    procedure CreateMsgWriter(SurportHttp: Boolean);

    // 广播，有权限限制
    procedure Broadcast(AMsg: TReceiveParams); overload;
    procedure Broadcast(AMsg: TReturnResult); overload;
    procedure Broadcast(AOwner: TIOCPSocket; const AMsg: String); overload;

    // 推送消息给指定客户端，无权限限制（AUserNameList为空时广播）
    procedure PushMsg(AMsg: TReceiveParams; const AUserNameList: String = ''); overload;
    procedure PushMsg(AMsg: TReturnResult; const AUserNameList: String = ''); overload;
    procedure PushMsg(AOwner: TIOCPSocket; const AMsg: String; const AUserNameList: String = ''); overload;

    // 推送消息给全部管理员
    procedure PushToAdmin(AMsg: TReceiveParams); overload;
    procedure PushToAdmin(AMsg: TReturnResult); overload;
    procedure PushToAdmin(AOwner: TIOCPSocket; const AMsg: String); overload;

    // 读写离线消息文件
    procedure ReadMsgFile(Params: TReceiveParams; Result: TReturnResult);
    procedure SaveMsgFile(Params: TReceiveParams; IODataSource: Boolean = True);
  published
    property OnBroadcast: TRequestEvent read FOnBroadcast write FOnBroadcast;
    property OnGet: TRequestEvent read FOnGet write FOnGet;
    property OnListFiles: TRequestEvent read FOnListFiles write FOnListFiles;
    property OnPush: TRequestEvent read FOnPush write FOnPush;
    property OnReceive: TRequestEvent read FOnReceive write FOnReceive;
  end;

  // ================== 文件管理器 ======================

  TFileUpDownEvent = procedure(Sender: TObject; Params: TReceiveParams;
                               Document: TIOCPDocument) of object;

  TInFileManager = class(TBaseManager)
  private
    FAfterDownload: TFileUpDownEvent;  // 文件下载完成
    FAfterUpload: TFileUpDownEvent;    // 文件上传完成
    FBeforeUpload: TRequestEvent;      // 上传文件
    FBeforeDownload: TRequestEvent;    // 下载文件
    
    FOnDeleteDir: TRequestEvent;       // 删除目录
    FOnDeleteFile: TRequestEvent;      // 删除文件
    FOnMakeDir: TRequestEvent;         // 新建目录
    FOnQueryFiles: TRequestEvent;      // 查询目录、文件
    FOnRenameDir: TRequestEvent;       // 重命名目录
    FOnRenameFile: TRequestEvent;      // 重命名文件
    FOnSetWorkDir: TRequestEvent;      // 设置当前目录
  protected
    procedure Execute(Socket: TIOCPSocket); override;
  public
    procedure CreateNewFile(Params: TReceiveParams);
    procedure ListFiles(Params: TReceiveParams; Result: TReturnResult; MsgFiles: Boolean = False); overload;
    procedure ListFiles(Result: TResultJSON; const Path: String); overload;
    procedure MakeDir(Params: TReceiveParams; Result: TReturnResult; const Path: String);
    procedure OpenLocalFile(Result: TReturnResult; const FileName: String = '');
    procedure SetWorkDir(Result: TReturnResult; const Dir: String);
  published
    property AfterDownload: TFileUpDownEvent read FAfterDownload write FAfterDownload;
    property AfterUpload: TFileUpDownEvent read FAfterUpload write FAfterUpload;
    property BeforeUpload: TRequestEvent read FBeforeUpload write FBeforeUpload;
    property BeforeDownload: TRequestEvent read FBeforeDownload write FBeforeDownload;
  published
    property OnDeleteDir: TRequestEvent read FOnDeleteDir write FOnDeleteDir;
    property OnDeleteFile: TRequestEvent read FOnDeleteFile write FOnDeleteFile;
    property OnMakeDir: TRequestEvent read FOnMakeDir write FOnMakeDir;
    property OnQueryFiles: TRequestEvent read FOnQueryFiles write FOnQueryFiles;
    property OnRenameDir: TRequestEvent read FOnRenameDir write FOnRenameDir;
    property OnRenameFile: TRequestEvent read FOnRenameFile write FOnRenameFile;
    property OnSetWorkDir: TRequestEvent read FOnSetWorkDir write FOnSetWorkDir;
  end;

  // ================== 数据库管理器 ======================

  TInDatabaseManager = class(TBaseManager)
  private
    // 支持多个数据库模块
    FDataModuleList: TInStringList;    // 数据库模块表
    function GetDataModuleCount: Integer;
    procedure DBConnect(Socket: TIOCPSocket);
    procedure GetDBConnections(Socket: TIOCPSocket);
  protected
    procedure Execute(Socket: TIOCPSocket); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear;
    procedure AddDataModule(ADataModule: TDataModuleClass; const ADescription: String);
    procedure GetDataModuleState(Index: Integer; var AClassName, ADescription: String; var ARunning: Boolean);
    procedure RemoveDataModule(Index: Integer);
    procedure ReplaceDataModule(Index: Integer; ADataModule: TDataModuleClass; const ADescription: String);
  public
    property DataModuleList: TInStringList read FDataModuleList;
    property DataModuleCount: Integer read GetDataModuleCount;
  end;

  // ================== 自定义管理器 ======================

  TInCustomManager = class(TBaseManager)
  private
    FFunctions: TInStringList;    // 函数列表
    FOnReceive: TRequestEvent;    // 接收事件
  protected
    procedure Execute(Socket: TIOCPSocket); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnReceive: TRequestEvent read FOnReceive write FOnReceive;
  published
    property OnAttachBegin;
    property OnAttachFinish;
  end;

  // ================== 远程函数组 ======================

  TInRemoteFunctionGroup = class(TBaseManager)
  private
    FFuncGroupName: String;              // 函数组名称
    FCustomManager: TInCustomManager;    // 自定义信息管理
    FOnExecute: TRequestEvent;           // 执行方法
    procedure SetCustomManager(const Value: TInCustomManager);
  protected
    procedure Execute(Socket: TIOCPSocket); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property CustomManager: TInCustomManager read FCustomManager write SetCustomManager;
    property FunctionGroupName: String read FFuncGroupName write FFuncGroupName;
    property OnExecute: TRequestEvent read FOnExecute write FOnExecute;
  end;

  // ================== WebSocket 管理 类 ======================

  TWebSocketEvent = procedure(Sender: TObject; Socket: TWebSocket) of object;

  TInWebSocketManager = class(TBaseManager)
  private
    FJSONLength: Integer;           // JSON 长度
    FUserName: string;              // 要查找的人
    FOnReceive: TWebSocketEvent;    // 接收事件
    FOnUpgrade: TOnUpgradeEvent;    // 升级为 WebSocket 事件
    procedure CallbackMethod(ObjType: TObjectType; var FromObject: Pointer;
                             const Data: TObject; var CancelScan: Boolean);
    procedure InterPushMsg(Source, Target: TWebSocket); overload;
    procedure InterPushMsg(Socket: TWebSocket; OpCode: TWSOpCode;
                           const Text: AnsiString = ''; const Group: AnsiString = ''); overload;
  protected
    procedure Execute(Socket: TIOCPSocket); override;
  public
    procedure Broadcast(Socket: TWebSocket); overload;
    procedure Broadcast(const Group, Text: string; OpCode: TWSOpCode = ocText); overload;

    procedure Delete(Admin: TWebSocket; const ToUser: String);
    procedure GetUserList(Socket: TWebSocket);

    procedure SendTo(Socket: TWebSocket; const ToUser: string); overload;
    procedure SendTo(const ToUser, Text: string); overload;

    function Logined(const UserName: String; var Socket: TWebSocket): Boolean; overload;
    function Logined(const UserName: String): Boolean; overload;
  published
    property OnReceive: TWebSocketEvent read FOnReceive write FOnReceive;
    property OnUpgrade: TOnUpgradeEvent read FOnUpgrade write FOnUpgrade;
  end;

  // ================== Http 服务 ======================

  TInHttpDataProvider = class(THttpDataProvider)
  private
    FRootDirectory: String;          // http 服务根目录
    FWebSocketManager: TInWebSocketManager;  // WebSocket 管理
    function GetGlobalLock: TThreadLock;
    procedure SetWebSocketManager(const Value: TInWebSocketManager);
  protected
    procedure Execute(Socket: THttpSocket);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateServer;
  published
    property RootDirectory: String read FRootDirectory write FRootDirectory;
    property WebSocketManager: TInWebSocketManager read FWebSocketManager write SetWebSocketManager;
  public
    property GlobalLock: TThreadLock read GetGlobalLock;
  end;

  // ================== 代理服务管理器 ======================

  // TInIOCPBroker 和 TSocketBroker 配合实现反向代理

  TInIOCPBroker = class;

  // 投放内部连接的线程
  TPostSocketThread = class(TThread)
  private
    FOwner: TInIOCPBroker;        // 代理
  protected
    procedure Execute; override;
  end;

  // 外部服务器信息
  TBrokenOptions = class(TPersistent)
  private
    FOwner: TInIOCPBroker;        // 代理
    function GetServerAddr: string;
    function GetServerPort: Word;
    procedure CheckServerPort(const AServerAddr: string; AServerPort: Word);
    procedure SetServerAddr(const Value: string);
    procedure SetServerPort(const Value: Word);
  public
    constructor Create(AOwner: TInIOCPBroker);
  published
    property ServerAddr: string read GetServerAddr write SetServerAddr;
    property ServerPort: Word read GetServerPort write SetServerPort default 80;
  end;

  // 外部服务器信息
  TProxyOptions = class(TBrokenOptions)
  private
    function GetConnectionCount: Word;
    procedure SetConnectionCount(const Value: Word);
  published
    property ConnectionCount: Word read GetConnectionCount write SetConnectionCount default 50;
  end;
  
  TInIOCPBroker = class(TBaseManager)
  private
    FReverseBrokers: TStrings;      // 反向代理列表
    FProtocol: TTransportProtocol;  // 传输协议
    FProxyType: TProxyType;         // 代理类型/部署位置

    FOuterServer: TProxyOptions;    // 外部服务信息
    FInnerServer: TBrokenOptions;   // 内部默认服务器

    FBrokerId: string;              // 代理服务器标志（Id）
    FDefaultInnerAddr: String;      // 内部默认的服务器
    FDefaultInnerPort: Word;        // 内部默认的服务端口

    FServerAddr: String;            // 外部服务器地址
    FServerPort: Word;              // 外部端口
    FConnectionCount: Integer;      // 预投放的连接数
    FCreateCount: Integer;          // 补充连接数
    FThread: TPostSocketThread;     // 投放线程

    FOnAccept: TAcceptBroker;       // 判断是否接受连接
    FOnBind: TBindIPEvent;          // 绑定服务器
    FOnBeforeForwardData: TForwardDataEvent;  // 转发前事件
    
    function GetReverseMode: Boolean;
    procedure PostConnectionsEx;
    procedure PostConnections;
    procedure InterConnectOuter(ACount: Integer);
  protected
    procedure AddConnection(Broker: TSocketBroker; const InnerId: String);
    procedure BindInnerBroker(Connection: TSocketBroker; const Data: PAnsiChar; DataSize: Cardinal);
    procedure CheckServerPort(const AServerAddr: string; AServerPort: Word);
    procedure IncOuterConnection;
    procedure Prepare;
    procedure Stop;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property ReverseMode: Boolean read GetReverseMode;
    property DefaultInnerAddr: string read FDefaultInnerAddr;
    property DefaultInnerPort: Word read FDefaultInnerPort;
    property ServerAddr: string read FServerAddr;
    property ServerPort: Word read FServerPort;
  published
    property BrokerId: string read FBrokerId write FBrokerId;
    property Protocol: TTransportProtocol read FProtocol write FProtocol default tpNone;
    property InnerServer: TBrokenOptions read FInnerServer write FInnerServer;
    property OuterServer: TProxyOptions read FOuterServer write FOuterServer;
    property ProxyType: TProxyType read FProxyType write FProxyType default ptDefault;
  published
    property OnAccept: TAcceptBroker read FOnAccept write FOnAccept;
    property OnBind: TBindIPEvent read FOnBind write FOnBind;
    property OnBeforeForwardData: TForwardDataEvent read FOnBeforeForwardData write FOnBeforeForwardData;
  end;

  // ================== 业务模块调用者 ======================
  // v2.0 从 iocp_server 单元移至本单元，
  // 以隐藏管理器的 Execute 方法。

  TBusiWorker = class(TBaseWorker)
  private
    FDMArray: array of TInIOCPDataModule; // 数模数组（支持多种）
    FDMList: TInStringList;  // 数模注册表（引用）
    FDMCount: Integer;       // 数模数量
    FDataModule: TInIOCPDataModule; // 当前数模
    function GetDataModule(Index: Integer): TInIOCPDataModule;
    procedure SetConnection(Index: Integer);
    procedure SetDefaultDataModule;
  protected
    procedure Execute(const Socket: TIOCPSocket); override;
    procedure StreamExecute(const Socket: TStreamSocket); override;
    procedure HttpExecute(const Socket: THttpSocket); override;
    procedure WebSocketExecute(const Socket: TWebSocket); override;
  public
    constructor Create(AServer: TObject; AThreadIdx: Integer);
    destructor Destroy; override;
    procedure AddDataModule(Index: Integer);
    procedure CreateDataModules;
    procedure RemoveDataModule(Index: Integer);
  public
    property DataModule: TInIOCPDataModule read FDataModule;
    property DataModules[Index: Integer]: TInIOCPDataModule read GetDataModule;
  end;
  
implementation

uses
  iocp_Varis, iocp_server, iocp_threads, iocp_WsJSON, iocp_wsExt;

type

  TIOCPServerRef    = class(TInIOCPServer);
  TBaseSocketRef    = class(TBaseSocket);
  TReceiveParamsRef = class(TReceiveParams);
  TReturnResultRef  = class(TReturnResult);
  TIOCPSocketRef    = class(TIOCPSocket);
  TSocketBrokerRef  = class(TSocketBroker);
  TWebSocketRef     = class(TWebSocket);
  
{ TWorkEnvironment }

function TWorkEnvironment.Logined(const UserName: String): Boolean;
begin
  Result := ValueOf(UserName) <> Nil;
end;

constructor TWorkEnvironment.Create;
begin
  inherited Create;
end;

destructor TWorkEnvironment.Destroy;
begin
  inherited;
end;

procedure TWorkEnvironment.FreeItemData(Item: PHashItem);
begin
  Dispose(PEnvironmentVar(Item^.Value));
end;

function TWorkEnvironment.Logined(const UserName: String; var Socket: TObject): Boolean;
var
  Item: PEnvironmentVar;
begin
  Item := ValueOf(UserName);
  if Assigned(Item) then
  begin
    Socket := TObject(Item^.BaseInf.Socket);
    Result := Assigned(Socket);         // 短连接是 Nil
  end else
  begin
    Socket := Nil;
    Result := False;
  end;
end;

{ TBaseManager }

procedure TBaseManager.AddToBackground(Socket: TBaseSocket);
var
 NewSocket: TBaseSocketRef;
begin
  // 将任务改为后台执行（从新加入业务线程）
  if (Socket.Background = False) and (
     (Socket.ClassType = TIOCPSocket) or (Socket.ClassType = TWebSocket)) then
  begin
    NewSocket := Socket.ObjPool.Pop^.Data;
    NewSocket.IniSocket(FServer, 0);  // 实际上套接字 FSocket 无用
    NewSocket.CopyResources(Socket);
    TInIOCPServer(FServer).BusiWorkMgr.AddWork(NewSocket);
  end;
end;

function TBaseManager.GetGlobalLock: TThreadLock;
begin
  // 取全局锁
  if Assigned(FServer) then
    Result := TInIOCPServer(FServer).GlobalLock
  else
    Result := Nil;
end;

procedure TBaseManager.Wakeup(Socket: TBaseSocket);
begin
  // 后台执行完毕，唤醒客户端（必须要有 UserName ）
  if Assigned(Socket) and Socket.Background then
    try
      if (Socket is TIOCPSocket) and
         Assigned(TInIOCPServer(FServer).MessageManager) then
        TInIOCPServer(FServer).MessageManager.InterPushResultMsg(
                               TIOCPSocket(TBaseSocketRef(Socket).FLinkSocket),
                               TIOCPSocket(Socket).Result, Socket.PeerIP) // PeerIP 转义为用户名称
      else
      if (Socket is TWebSocket) then
        TInIOCPServer(FServer).HttpDataProvider.WebSocketManager.SendTo(
                               TWebSocket(Socket), Socket.PeerIP); // PeerIP 转义为用户名称
    finally
      TBaseSocketRef(Socket).InterCloseSocket(Socket);
    end;
end;

{ TInStreamManager }

procedure TInStreamManager.Broadcast(AOwner: TStreamSocket; const AText: string);
begin
  if Assigned(FServer) then
    InterPushText(AOwner, nil, AText);
end;

procedure TInStreamManager.Execute(Socket: TIOCPSocket);
begin
  // Socket 是 TStreamSocket
  if Assigned(FOnReceive) then
    FOnReceive(TStreamSocket(Socket),
               Socket.RecvBuf.Data.buf,
               TBaseSocket(Socket).ByteCount,
               TBaseSocketRef(Socket).FCompleted);
end;

function TInStreamManager.GetClientData(const AClientId: String;
  ABuffers: PAnsiChar; AEvent: TCopyDataEvent): Boolean;
var
  Socket: TStreamSocket;
begin
  // 查找客户端 AClientId，调用方法 AEvent
  // 取 Socket.Data（连接时用户定义，不知类型、大小）
  TInIOCPServer(FServer).IOCPSocketPool.Lock;
  try
    Socket := nil;
    FClientId := AClientId;  // 查找这个客户端
    TInIOCPServer(FServer).IOCPSocketPool.Scan(Pointer(Socket), CallbackMethod);
    Result := Assigned(Socket);
    if Result then
      AEvent(Socket, ABuffers);  // 调用，保证在加锁状态
  finally
    TInIOCPServer(FServer).IOCPSocketPool.UnLock;
  end;
end;

procedure TInStreamManager.InterPushText(AOwner, ATarget: TStreamSocket; const AText: AnsiString);
var
  CastType: TBroadcastType;
  Msg: TPushMessage;
begin
  // 给 ATarget/全部客户端 推送文本消息 AText（不分组）

  Msg := nil;
  if Assigned(ATarget) then
    CastType := btUnknown
  else
    CastType := btAllClient;
  
  case Length(AText) of
    0:
      Msg := TPushMessage.Create(AOwner.RecvBuf, CastType);
    1..IO_BUFFER_SIZE: begin
      Msg := TPushMessage.Create(AOwner.ObjPool, AOwner.BufferPool, CastType, Length(AText));
      System.Move(AText[1], Msg.PushBuf^.Data.buf^, Msg.PushBuf^.Data.len);
    end;
  end;

  // 忽略自己
  Msg.IngoreSocket := AOwner;

  if Assigned(ATarget) then  // 推送目的
    Msg.Add(ATarget);

  // 推送
  TInIOCPServer(FServer).PushManager.AddWork(Msg);
  
end;

procedure TInStreamManager.CallbackMethod(ObjType: TObjectType;
  var FromObject: Pointer; const Data: TObject; var CancelScan: Boolean);
begin
  // 扫描列表的回调方法，已加锁
  if (TStreamSocket(Data).ClientId = FClientId) then
  begin
    FromObject := Data;
    CancelScan := True; // 退出查找
  end;
end;

function TInStreamManager.CheckState(const AClientId: String; var ASocket: TStreamSocket): Boolean;
begin
  // 检查客户端 AClientId 状态
  TInIOCPServer(FServer).IOCPSocketPool.Lock;
  try
    ASocket := nil;
    FClientId := AClientId;  // 查找这个客户端
    TInIOCPServer(FServer).IOCPSocketPool.Scan(Pointer(ASocket), CallbackMethod);
  finally
    Result := Assigned(ASocket);
    TInIOCPServer(FServer).IOCPSocketPool.UnLock;
  end;
end;

function TInStreamManager.Logined(const AClientId: String): Boolean;
var
  Socket: TStreamSocket;
begin
  // 检查客户端 AClientId 状态
  Result := Assigned(FServer) and CheckState(AClientId, Socket);
end;

procedure TInStreamManager.SendTo(AOwner: TStreamSocket; const AToClient, AText: string);
var
  Target: TStreamSocket;
begin
  // 推送消息给 AToClient
  if Assigned(FServer) and CheckState(AToClient, Target) then
    InterPushText(AOwner, Target, AText);
end;

{ TInClientManager }

procedure TInClientManager.Add(IOCPSocket: TIOCPSocket; ClientRole: TClientRole);
var
  Node: PEnvironmentVar;
  UserName: String;
begin
  // 登记用户登录信息

  // 可能是短连接重新登录
  UserName := LowerCase(IOCPSocket.Params.UserName);  // 统一用小写
  Node := FClientList.ValueOf(UserName);

  if (Assigned(Node) = False) then  // 无用户信息
  begin
    // 分配新的用户信息空间
    Node := New(PEnvironmentVar);
    Node^.BaseInf.Group := IOCPSocket.Params.UserGroup;  // 分组
    Node^.BaseInf.Name := UserName;

    // 工作路径（+用户名+data）
    Node^.WorkDir := AddBackslash(iocp_Varis.gUserDataPath + UserName + '\data\');
    Node^.IniDirLen := Length(Node^.WorkDir);

    // 加入 Hash 表
    FClientList.Add(UserName, Node);
  end;

  Node^.BaseInf.Socket := TServerSocket(IOCPSocket); // Socket
  Node^.BaseInf.LoginTime := Now();
  Node^.BaseInf.LogoutTime := 0.0;
  Node^.BaseInf.PeerIPPort := IOCPSocket.PeerIPPort;
  Node^.BaseInf.Role := ClientRole;
  
  Node^.ReuseSession := IOCPSocket.Params.ReuseSessionId;  // 是否重用凭证

  // 注册信息到 IOCPSocket
  TIOCPSocketRef(IOCPSocket).SetLogState(Node);
end;

procedure TInClientManager.GetClientState(const UserName: String; Result: TReturnResult);
begin
  // 查询用户的登录状态（用户名应该是用 QueryClients 查询出来的）
  // 在外部判断用户是否存在：arMissing
  Result.UserName := UserName;
  if FClientList.Logined(UserName) then
    Result.ActResult := arOnline
  else
    Result.ActResult := arOffline;
end;

procedure TInClientManager.Clear;
begin
  // 清除登录信息
  FClientList.Clear;
end;

procedure TInClientManager.CopyClientInf(ObjType: TObjectType; var Buffer: Pointer;
                           const Data: TObject; var CancelScan: Boolean);
begin
  // 复制客户信息
  if (ObjType = otEnvData) then
  begin
    // 复制登录客户的信息
    PClientInfo(Buffer)^ := PEnvironmentVar(Data)^.BaseInf;
    Inc(PAnsiChar(Buffer), CLIENT_DATA_SIZE);  // 位置推进
  end else
  if (TIOCPSocket(Data).SessionId = 0) then
  begin
    with PClientInfo(Buffer)^ do
    begin
      Socket := TServerSocket(Data);
      Group := 'Unknown';  // 无分组
      Name := 'Unknown';   // 未登录
      Role := crUnknown;
      PeerIPPort := TIOCPSocket(Data).PeerIPPort;
    end;
    Inc(PAnsiChar(Buffer), CLIENT_DATA_SIZE);  // 位置推进
  end;
end;

constructor TInClientManager.Create(AOwner: TComponent);
begin
  inherited;
  FClientList := TWorkEnvironment.Create;
end;

procedure TInClientManager.Disconnect(const UserName: String);
var
  oSocket: TIOCPSocket;
begin
  // 只断开客户端，要在外部删除账户
  if Logined(UserName, oSocket) then
    TBaseSocketRef(oSocket).PostEvent(ioDelete);
end;

destructor TInClientManager.Destroy;
begin
  FClientList.Free;
  inherited;
end;

procedure TInClientManager.Execute(Socket: TIOCPSocket);
begin
  case Socket.Action of
    atAfterSend: begin    // 发送附件完毕
      Socket.Result.Clear;
      Socket.Result.ActResult := arOK;
    end;

    atAfterReceive: begin // 接收附件完毕
      Socket.Params.Clear;
      Socket.Result.ActResult := arOK;
    end;
    
    else  // ==============================
      case Socket.Params.Action of
        atUserLogin:          // 登录
          if Assigned(FOnLogin) then
            FOnLogin(Socket.Worker, Socket.Params, Socket.Result);

        atUserLogout: begin   // 登出
            if Assigned(FOnLogout) then
              FOnLogout(Socket.Worker, Socket.Params, Socket.Result);
            TIOCPSocketRef(Socket).SetLogState(Nil); // 内部登出
          end;

        atUserRegister:       // 注册用户
          if Assigned(FOnRegister) then
            FOnRegister(Socket.Worker, Socket.Params, Socket.Result);

        atUserModify:         // 修改密码
          if Assigned(FOnModify) then
            FOnModify(Socket.Worker, Socket.Params, Socket.Result);

        atUserDelete:         // 删除用户
          if Assigned(FOnDelete) then
            FOnDelete(Socket.Worker, Socket.Params, Socket.Result);

        atUserQuery:          // 查询在线/登录客户端
          GetClients(Socket.Result);

        atUserState:          // 查询用户状态
          if Assigned(FOnQueryState) then
            FOnQueryState(Socket.Worker, Socket.Params, Socket.Result);
      end;
  end;
end;

procedure TInClientManager.GetClients(Result: TReturnResult);
begin
  GetLoginedClients(Result);       // 返回登录的客户端信息
  GetConnectedClients(Result);     // 返回在线未登录客户端信息
  Result.AsInteger['group'] := 2;  // 两组
end;

procedure TInClientManager.GetConnectedClients(Result: TReturnResult);
var
  Size, Count: Integer;
  Buffer, Buffer2: TMemBuffer;
begin
  // 返回在线但未登录的客户端信息（第一组）
  
  TInIOCPServer(FServer).IOCPSocketPool.Lock;
  try
    // 分配自定义的 TMemBuffer 缓存
    Size := TInIOCPServer(FServer).IOCPSocketPool.UsedCount * CLIENT_DATA_SIZE;
    Buffer := GetBuffer(Size);
    Buffer2 := Buffer;

    // 遍历客户列表，复制信息到 Buffer
    TInIOCPServer(FServer).IOCPSocketPool.Scan(Buffer2, CopyClientInf);

    if (Buffer2 = Buffer) then
    begin
      // 没有客户端
      Result.AsBuffer['list_1'] := nil;
      Result.AsInteger['count_1'] := 0;
      FreeBuffer(Buffer);
    end else
    begin
      Count := (PAnsiChar(Buffer2) - PAnsiChar(Buffer)) div CLIENT_DATA_SIZE;
      if (Count <> TInIOCPServer(FServer).IOCPSocketPool.UsedCount) then
      begin
        // 没那么多客户端，不用那么长
        Size := Count * CLIENT_DATA_SIZE;
        Buffer2 := GetBuffer(Size);
        System.Move(Buffer^, Buffer2^, Size);
        FreeBuffer(Buffer);
        Buffer := Buffer2;
      end;
      Result.AsBuffer['list_1'] := Buffer;  // 加入引用
      Result.AsInteger['count_1'] := Count;  // 数量
    end;

  finally
    TInIOCPServer(FServer).IOCPSocketPool.UnLock;
  end;
end;

procedure TInClientManager.GetLoginedClients(Result: TReturnResult);
var
  Buffer, Buffer2: TMemBuffer;
begin
  // 取登录的客户端信息（第二组）
  FClientList.Lock;
  try
    // 分配自定义的 TMemBuffer 缓存
    if (FClientList.Count = 0) then
    begin
      Result.AsBuffer['list_2'] := Nil;
      Result.AsInteger['count_2'] := 0;
    end else
    begin
      Buffer := GetBuffer(FClientList.Count * CLIENT_DATA_SIZE);
      Buffer2 := Buffer;

      // 遍历客户列表，复制信息到 Buffer2
      FClientList.Scan(Buffer2, CopyClientInf);

      Result.AsBuffer['list_2'] := Buffer;  // 加入引用
      Result.AsInteger['count_2'] := FClientList.Count;
    end;
  finally
    FClientList.UnLock;
  end;
end;

function TInClientManager.Logined(const UserName: String; var Socket: TIOCPSocket): Boolean;
begin
  if (UserName = '') then
    Result := False
  else
    Result := FClientList.Logined(LowerCase(UserName), TObject(Socket));
end;

function TInClientManager.Logined(const UserName: String): Boolean;
begin
  if (UserName = '') then
    Result := False
  else
    Result := FClientList.Logined(LowerCase(UserName));
end;

procedure TInClientManager.RemoveClient(const UserName: string);
begin
  // 释放客户端登录信息
  FClientList.Remove(LowerCase(UserName));
end;

{ TInMessageManager }

procedure TInMessageManager.Broadcast(AMsg: TReceiveParams);
begin
  // 广播收到的消息（发给包括未登录的客户端），有权限限制
  if (AMsg = AMsg.Socket.Params) and    // 是传入的消息
     (AMsg.Socket.Role >= crAdmin) and  // 权限>=管理员
     (AMsg.Action <> atUserLogin) then  // 不是登录操作（不带密码）     
    InterPushMsg(AMsg.Socket, nil, nil, btAllClient);
end;

procedure TInMessageManager.Broadcast(AMsg: TReturnResult);
begin
  // 广播要返回的消息（发给包括未登录的客户端），有权限限制
  if (AMsg = AMsg.Socket.Result) and
     (AMsg.Socket.Role >= crAdmin) then
    InterCopyPush(AMsg.Socket, nil, AMsg.Socket.Result, nil, btAllClient);
end;

procedure TInMessageManager.Broadcast(AOwner: TIOCPSocket; const AMsg: String);
var
  Result: TReturnResult;
begin
  // AOwner: 宿主;  AMsg: 消息文本
  // 广播文本，有权限限制
  if Assigned(AOwner) and (AMsg <> '') then
    if (AOwner.Role >= crAdmin) then
    begin
      Result:= TReturnResult.Create(AOwner);  // 新建一个反馈消息包
      try
        Result.Msg := AMsg;  // 消息文本
        InterCopyPush(AOwner, nil, Result, nil, btAllClient);
      finally
        Result.Free;
      end;
    end else
      AOwner.Result.ActResult := arFail;
end;

procedure TInMessageManager.CreateMsgWriter(SurportHttp: Boolean);
begin
  // 消息书写器，开启 Http 服务时同时保存附件的 URL，见：TMessageWriter.SaveMsg
  if (Assigned(FMsgWriter) = False) then
    FMsgWriter := TMessageWriter.Create(SurportHttp);
end;

destructor TInMessageManager.Destroy;
begin
  if Assigned(FMsgWriter) then
    FMsgWriter.Free;
  inherited;
end;

procedure TInMessageManager.Execute(Socket: TIOCPSocket);
begin
  case Socket.Action of
    atAfterSend: begin   // 发送附件完毕
      Socket.Result.Clear;
      Socket.Result.ActResult := arOK;
    end;

    atAfterReceive: begin // 接收附件完毕
      Socket.Params.Clear;
      Socket.Result.ActResult := arOK;
    end;

    else  // ==============================

      case Socket.Params.Action of
        atTextSend:      // 发送文本到服务器
          if Assigned(FOnReceive) then
            FOnReceive(Socket.Worker, Socket.Params, Socket.Result);
        atTextPush:      // 推送消息
          if Assigned(FOnPush) then
            FOnPush(Socket.Worker, Socket.Params, Socket.Result);
        atTextBroadcast: // 广播消息
          if Assigned(FOnBroadcast) then
            FOnBroadcast(Socket.Worker, Socket.Params, Socket.Result);
        atTextGetMsg:       // 离线消息
          if Assigned(FOnGet) then
            FOnGet(Socket.Worker, Socket.Params, Socket.Result);
        atTextFileList:
          if Assigned(FOnListFiles) then
            FOnListFiles(Socket.Worker, Socket.Params, Socket.Result);
      end;
  end;
end;

procedure TInMessageManager.InterCopyPush(ASource, ATarget: TIOCPSocket;
  AResult: TReturnResult; AToSockets: TInList; CastType: TBroadcastType);
var
  Msg: TPushMessage;
begin
  // 发送要返回的消息给 ATarget 或 AToSockets 或广播

  // 消息太长 -> 忽略
  if (AResult.Size >= BROADCAST_MAX_SIZE) then
  begin
    ASource.Result.ActResult := arFail;  // 不是 AResult
    Exit;
  end;

  // 建推送消息
  if (CastType in [btAllClient, btAdminOnly]) then
  begin
    Msg := TPushMessage.Create(ASource.ObjPool, ASource.BufferPool, CastType);
    Msg.IngoreSocket := ASource;  // 忽略自己（注解后自己也能收到）
    if (ASource.Background = False) then
      Msg.Group := ASource.UserGroup;  // 分组
  end else
  begin
    Msg := TPushMessage.Create(ASource.ObjPool, ASource.BufferPool, btUnknown);
    if Assigned(ATarget) then
      Msg.Add(ATarget)  // 推送给 ATarget
    else
    if Assigned(AToSockets) then
      Msg.AddList(AToSockets);  // 推送给列表（不能为nil）
  end;

  // 转为流，写入推送缓存
  TReturnResultRef(AResult).CreateStreams(False);
  TReturnResultRef(AResult).LoadHead(@Msg.PushBuf^.Data);  // 装载、清流

  Msg.UpdateMsgId;  // 用新的 MsgId

  if TInIOCPServer(FServer).PushManager.AddWork(Msg) then  // 加入推送线程
    ASource.Result.ActResult := arOK
  else  // 繁忙，放弃，Msg 已被释放
    ASource.Result.ActResult := arErrBusy;
end;

procedure TInMessageManager.InterPushMsg(ASource, ATarget: TIOCPSocket;
  AToSockets: TInList; CastType: TBroadcastType);
var
  Msg: TPushMessage;
begin
  // 发送收到的消息给 ATarget 或 AToSockets 或广播

  // 消息太长 -> 忽略
  if (ASource.Params.Size >= BROADCAST_MAX_SIZE) then
  begin
    ASource.Result.ActResult := arFail;
    Exit;
  end;

  // 上传文件的操作，ASource.RecvBuf 内容为文件数据，
  // 要把 Params 写回 ASource.RecvBuf
  if Assigned(ASource.Params.Attachment) then
    TReceiveParams(ASource.Params).Write(ASource.RecvBuf);

  // 建推送消息
  if (CastType in [btAllClient, btAdminOnly]) then
  begin
    Msg := TPushMessage.Create(ASource.RecvBuf, CastType);
    Msg.IngoreSocket := ASource;  // 忽略自己（注解后自己也能收到）
    if (ASource.Background = False) then
      Msg.Group := ASource.UserGroup;  // 分组
  end else
  begin
    Msg := TPushMessage.Create(ASource.RecvBuf, btUnknown);
    if Assigned(ATarget) then
      Msg.Add(ATarget)  // 推送给 ATarget
    else
      Msg.AddList(AToSockets);  // 推送给列表（不能为nil）
  end;

  // 用新的 MsgId
  Msg.UpdateMsgId;

  if TInIOCPServer(FServer).PushManager.AddWork(Msg) then  // 加入推送线程
    ASource.Result.ActResult := arOK
  else  // 繁忙，放弃，Msg 已被释放
    ASource.Result.ActResult := arErrBusy;
end;

procedure TInMessageManager.InterPushResultMsg(Socket: TIOCPSocket; Result: TReturnResult; UserNameList: AnsiString);
var
  PushMsg: TPushMessage;
begin
  if (UserNameList = '')  then  // 广播，无权限限制
    InterCopyPush(Socket, nil, Result, nil, btAllClient)
  else
  if (Result.Size < BROADCAST_MAX_SIZE) then
  begin
    // 新建推送消息
    PushMsg := TPushMessage.Create(Socket.ObjPool, Socket.BufferPool, btUnknown);

    // 转为流，写入推送缓存
    TReturnResultRef(Result).CreateStreams(False);
    TReturnResultRef(Result).LoadHead(@PushMsg.PushBuf^.Data);  // 装载、清流

    InterSavePushMsg(Socket, PushMsg, UserNameList);
  end else
    Socket.Result.ActResult := arFail;  // 不是 Result.ActResult
end;

procedure TInMessageManager.InterSavePushMsg(AOwner: TIOCPSocket;
  PushMsg: TInList; UserNameList: AnsiString);
var
  UserName: AnsiString;
  oSocket: TIOCPSocket;
begin
  // 用新的 MsgId
  TPushMessage(PushMsg).UpdateMsgId;

  while ExtractValue(UserName, UserNameList) do  // 提取用户名
  begin
    if Assigned(TInIOCPServer(FServer).ClientManager) and
      TInIOCPServer(FServer).ClientManager.Logined(UserName, oSocket) then
      PushMsg.Add(oSocket)  // 在线，推送
    else  // 离线，保存到消息文件
      FMsgWriter.SaveMsg(TPushMessage(PushMsg).PushBuf, UserName);
  end;

  if TInIOCPServer(FServer).PushManager.AddWork(TPushMessage(PushMsg)) then  // 加入推送线程
    AOwner.Result.ActResult := arOK
  else  // 繁忙，放弃，Msg 已被释放
    AOwner.Result.ActResult := arErrBusy;

end;

procedure TInMessageManager.PushMsg(AMsg: TReceiveParams; const AUserNameList: String);
var
  PushMsg: TPushMessage;
begin
  // 推送收到的消息给 AUserNameList
  if Assigned(AMsg) and (AMsg = AMsg.Socket.Params)then
    if (AUserNameList = '') then  // 广播，无权限限制
      InterPushMsg(AMsg.Socket, nil, nil, btAllClient)
    else
    if (AMsg.Size < BROADCAST_MAX_SIZE) then
    begin
      if Assigned(AMsg.Attachment) then
      begin
        // 文件操作，新建推送消息，把 AMsg 写入 PushMsg.RecvBuf
        PushMsg := TPushMessage.Create(AMsg.Socket.ObjPool,
                                       AMsg.Socket.BufferPool, btUnknown);
        AMsg.Write(PushMsg.PushBuf);
      end else  // 直接复制新建推送消息
        PushMsg := TPushMessage.Create(AMsg.Socket.RecvBuf, btUnknown);

      // 加入推送目的，保持消息到文件  
      InterSavePushMsg(AMsg.Socket, PushMsg, AUserNameList);
    end else
      AMsg.Socket.Result.ActResult := arFail;
end;

procedure TInMessageManager.PushMsg(AMsg: TReturnResult; const AUserNameList: String);
begin
  // 推送要返回的消息给 AUserNameList
  if Assigned(AMsg) and (AMsg = AMsg.Socket.Result) then
    InterPushResultMsg(AMsg.Socket, AMsg, AUserNameList);
end;

procedure TInMessageManager.PushMsg(AOwner: TIOCPSocket; const AMsg, AUserNameList: String);
var
  Result: TReturnResult;
begin
  // AOwner: 宿主；AMsg: 消息文本；AUserNameList: 客户端名称列表
  // 推送文本 AMsg 给 AUserNameList
  if Assigned(AOwner) and (AMsg <> '') then
  begin
    Result:= TReturnResult.Create(AOwner);  // 新建一个返回的消息
    try
      Result.Msg := AMsg;  // 消息文本
      InterPushResultMsg(AOwner, Result, AUserNameList);
    finally
      Result.Free;
    end;
  end;
end;

procedure TInMessageManager.PushToAdmin(AMsg: TReceiveParams);
begin
  // 推送收到的消息给全部管理员，无权限限制
  if Assigned(AMsg) and (AMsg = AMsg.Socket.Params) then
    InterPushMsg(AMsg.Socket, nil, nil, btAdminOnly);
end;

procedure TInMessageManager.PushToAdmin(AMsg: TReturnResult);
begin
  // 推送要返回的消息给全部管理员，无权限限制
  if Assigned(AMsg) and (AMsg = AMsg.Socket.Result) then
    InterCopyPush(AMsg.Socket, nil, AMsg, nil, btAdminOnly);
end;

procedure TInMessageManager.PushToAdmin(AOwner: TIOCPSocket; const AMsg: String);
var
  Result: TReturnResult;
begin
  // AOwner: 宿主；AMsg: 消息文本
  // 推送文本给管理员，无权限限制
  if Assigned(AOwner) and (AMsg <> '') then
  begin
    Result:= TReturnResult.Create(AOwner);  // 新建一个返回的消息
    try
      Result.Msg := AMsg;  // 消息文本
      InterCopyPush(AOwner, nil, Result, nil, btAdminOnly);
    finally
      Result.Free;
    end;
  end;
end;

procedure TInMessageManager.ReadMsgFile(Params: TReceiveParams; Result: TReturnResult);
begin
  // 把用户 UserName 的离线消息文件加到 Result（当作附件）
  FMsgWriter.LoadMsg(Params.UserName, Result);
end;

procedure TInMessageManager.SaveMsgFile(Params: TReceiveParams; IODataSource: Boolean);
begin
  // 此时 ToUser 不为空，把消息包 Params 保存到 ToUser 的消息文件
  TReceiveParamsRef(Params).SetUniqueMsgId;  // 使用服务端的唯一 msgId
  if IODataSource then  // 保存收到的数据块，更快！
    FMsgWriter.SaveMsg(Params.Socket.RecvBuf, Params.ToUser)
  else // 保存变量表，要转换为流，但可增加附件的 URL
    FMsgWriter.SaveMsg(Params, Params.ToUser);
end;

{ TInFileManager }

procedure TInFileManager.CreateNewFile(Params: TReceiveParams);
begin
  // 建文件流，准备接收文件
  if (Params.Action = atFileUpShare) then // 保存到临时路径
    Params.CreateAttachment(iocp_varis.gUserDataPath + Params.UserName + '\temp\')
  else
    Params.CreateAttachment(iocp_varis.gUserDataPath + Params.UserName + '\data\');
end;

procedure TInFileManager.Execute(Socket: TIOCPSocket);
begin
  // 先执行内部操作事件
  //   服务端的附件先发送，才收到客户端的附件

  case Socket.Action of
    atAfterSend:    // 发送附件完毕（内部事件）
      try
        if Assigned(FAfterDownload) then
          if (Socket.Result.Action <> atFileDownChunk) or
             (Socket.Result.OffsetEnd + 1 = TIOCPDocument(Socket.Result.Attachment).OriginSize) then
            FAfterDownload(Socket.Worker, Socket.Params,
                           Socket.Result.Attachment as TIOCPDocument);
      finally
        Socket.Result.Clear;
        Socket.Result.ActResult := arOK;  // 客户端收到整体发送完毕
      end;

    atAfterReceive: // 接收附件完毕（内部事件，不处理续传）
      try
        if Assigned(FAfterUpload) then
          if (Socket.Params.Action <> atFileUpChunk) or
             (Socket.Params.OffsetEnd + 1 = Socket.Params.Attachment.OriginSize) then
            FAfterUpload(Socket.Worker, Socket.Params, Socket.Params.Attachment);
      finally
        Socket.Params.Clear;
        Socket.Result.ActResult := arOK;
      end;

    else  // =================================

      case Socket.Params.Action of
        atFileList:         // 列出文件
          if Assigned(FOnQueryFiles) then
            FOnQueryFiles(Socket.Worker, Socket.Params, Socket.Result);

        atFileSetDir:       // 设置路径
          if Assigned(FOnSetWorkDir) then
            FOnSetWorkDir(Socket.Worker, Socket.Params, Socket.Result);

        atFileRename:       // 重命名文件
          if Assigned(FOnRenameFile) then
            FOnRenameFile(Socket.Worker, Socket.Params, Socket.Result);

        atFileRenameDir:    // 重命名目录
          if Assigned(FOnRenameDir) then
            FOnRenameDir(Socket.Worker, Socket.Params, Socket.Result);

        atFileDelete:       // 删除文件
          if Assigned(FOnDeleteFile) then
            FOnDeleteFile(Socket.Worker, Socket.Params, Socket.Result);

        atFileDeleteDir:    // 删除目录
          if Assigned(FOnDeleteDir) then
            FOnDeleteDir(Socket.Worker, Socket.Params, Socket.Result);

        atFileMakeDir:      // 新建目录
          if Assigned(FOnMakeDir) then
            FOnMakeDir(Socket.Worker, Socket.Params, Socket.Result);

        atFileDownload,     // 下载文件
        atFileDownShare:
          if Assigned(FBeforeDownload) then
            FBeforeDownload(Socket.Worker, Socket.Params, Socket.Result);

        atFileDownChunk:    // 断点下载文件
          if Assigned(FBeforeDownload) then
            if (Socket.Params.Offset = 0) then  // 进入应用层打开文件
              FBeforeDownload(Socket.Worker, Socket.Params, Socket.Result)
            else  // 不进入应用层
              Socket.Result.LoadFromFile(DecryptString(Socket.Params.Directory) +
                                         Socket.Params.FileName, True);

        atFileUpload,
        atFileUpShare:      // 上传文件
          if Assigned(FBeforeUpload) then
          begin
            FBeforeUpload(Socket.Worker, Socket.Params, Socket.Result);
            if Assigned(Socket.Params.Attachment) then  
              Socket.Result.ActResult := arAccept;  // 可能用户设为 arOK, 校正结果
          end;

        atFileUpChunk:      // 断点上传文件
          if Assigned(FBeforeUpload) then
            if (Socket.Params.Offset = 0) then  // 进入应用层打开文件
            begin
              FBeforeUpload(Socket.Worker, Socket.Params, Socket.Result);
              if Assigned(Socket.Params.Attachment) then
                Socket.Result.ActResult := arAccept;  // 可能用户设为 arOK, 校正结果
            end else        // 不进入应用层
              Socket.Params.CreateAttachment(DecryptString(Socket.Params.Directory));
      end;
  end;
end;

procedure TInFileManager.ListFiles(Result: TResultJSON; const Path: String);
var
  i: Integer;
  SRec: TSearchRec;
  FileRec: TCustomJSON;
begin
  // 取目录 Path 的文件列表

  if (DirectoryExists(Path) = False) then
  begin
    Result.I['count'] := -1;  // 错误的目录
    Exit;
  end;

  i := 0;
  FileRec := TCustomJSON.Create;
  FindFirst(Path + '*.*', faAnyFile, SRec);

  try
    repeat
      if (SRec.Name <> '.') and (SRec.Name <> '..') then
      begin
        Inc(i);
        FileRec.S['name'] := SRec.Name;
        FileRec.I64['size'] := SRec.Size;
        FileRec.D['CreationTime'] := FileTimeToDateTime(SRec.FindData.ftCreationTime);
        FileRec.D['LastWriteTime'] := FileTimeToDateTime(SRec.FindData.ftLastWriteTime);

        if (SRec.Attr and faDirectory) = faDirectory then
          FileRec.S['dir'] := 'Y'       // 目录
        else
          FileRec.S['dir'] := 'N';

        // 把文件信息当作一条记录，加到 Result
        Result.R[IntToStr(i)] := FileRec;
      end;
    until FindNext(SRec) > 0;

    Result.I['count'] := i;  // 文件数
  finally
    FileRec.Free;
    FindClose(SRec);
  end;
end;

procedure TInFileManager.ListFiles(Params: TReceiveParams; Result: TReturnResult; MsgFiles: Boolean);
var
  i: Integer;
  SRec: TSearchRec;
  Dir: String;
  FileRec: TCustomPack;
begin
  // 取用户当前目录的文件列表

  // 用户目录文件结果：
  //   1. 主目录：Socket.Data^.WorkDir + UserName
  //   2. 主要数据目录：UserName\Data
  //   3. 离线消息目录：UserName\Msg
  //   4. 临时文件目录: UserName\temp

  if MsgFiles then  // 消息文件路径
    Dir := iocp_varis.gUserDataPath + Params.UserName + '\msg\'
  else
    Dir := Params.Socket.Envir^.WorkDir + Params.Directory;

  if (DirectoryExists(Dir) = False) then
  begin
    Result.ActResult := arFail;        // 错误的目录
    Exit;
  end;

  i := 0;
  FileRec := TCustomPack.Create;
  FindFirst(Dir + '*.*', faAnyFile, SRec);

  try
    repeat
      if (SRec.Name <> '.') and (SRec.Name <> '..') then
      begin
        Inc(i);
        FileRec.AsString['name'] := SRec.Name;
        FileRec.AsInt64['size'] := SRec.Size;
        FileRec.AsDateTime['CreationTime'] := FileTimeToDateTime(SRec.FindData.ftCreationTime);
        FileRec.AsDateTime['LastWriteTime'] := FileTimeToDateTime(SRec.FindData.ftLastWriteTime);

        if (SRec.Attr and faDirectory) = faDirectory then
          FileRec.AsString['dir'] := 'Y'       // 目录
        else
          FileRec.AsString['dir'] := 'N';

        // 把文件信息当作一条记录，加到 Result
        Result.AsRecord[IntToStr(i)] := FileRec;
      end;
    until FindNext(SRec) > 0;

    if (i > 0) then
      Result.ActResult := arOK
    else
      Result.ActResult := arEmpty;      // 空目录
          
//    Result.SaveToFile('temp\svr.txt');
  finally
    FileRec.Free;
    FindClose(SRec);
  end;
end;

procedure TInFileManager.MakeDir(Params: TReceiveParams; Result: TReturnResult; const Path: String);
var
  NewPath: String;
begin
  // 新建一个目录（在工作路径的主目录下）
  NewPath := Params.Socket.Envir^.WorkDir + Path;
  if DirectoryExists(NewPath) then
    Result.ActResult := arExists
  else begin
    MyCreateDir(NewPath);
    Result.ActResult := arOK;
  end;
end;

procedure TInFileManager.OpenLocalFile(Result: TReturnResult; const FileName: String);
begin
  if (FileName <> '') then  // 打开 FileName
    Result.LoadFromFile(FileName, True)
  else  // 打开工作路径下的 Params.FileName
    Result.LoadFromFile(Result.Socket.Envir^.WorkDir +
                        Result.Socket.Params.FileName, True);
end;

procedure TInFileManager.SetWorkDir(Result: TReturnResult; const Dir: String);
  function GetParentDir(var S: String): Integer;
  var
    i, k: Integer;
  begin
    k := Length(S);
    for i := k downto 1 do
      if (i < k) and (S[i] = '\') then
      begin
        Delete(S, i + 1, 99);
        Result := i;
        Exit;
      end;
    Result := k;
  end;
var
  S: String;
  iLen: Integer;
begin
  // 设置工作路径，不能带盘符 :
  if (Result.Socket.Envir = Nil) or (Pos(':', Dir) > 0) then
    Result.ActResult := arFail
  else begin
    // 1. 父目录、2. 子目录
    S := Result.Socket.Envir^.WorkDir;

    if (Dir = '..') then  // 1. 进入父目录
    begin
      iLen := GetParentDir(S);
      if (iLen >= Result.Socket.Envir^.IniDirLen) then  // 长度不少于原始的
        Result.ActResult := arOK
      else
        Result.ActResult := arFail;
    end else

    if (Pos('..', Dir) > 0) then  // 不允许用 ..\xxx 这种方法超出访问范围 ！
      Result.ActResult := arFail

    else begin
      // 2. 子目录
      S := S + AddBackslash(Dir);
      if DirectoryExists(S) then
        Result.ActResult := arOK
      else
        Result.ActResult := arMissing;
    end;

    if (Result.ActResult = arOK) then
      Result.Socket.Envir^.WorkDir := S;
  end;
end;

{ TInDatabaseManager }

procedure TInDatabaseManager.AddDataModule(ADataModule: TDataModuleClass; const ADescription: String);
begin
  // 注册数据模（描述是唯一的）
  if FDataModuleList.IndexOf(ADescription) = -1 then
  begin
    FDataModuleList.Add(ADescription, TObject(ADataModule));
    if Assigned(FServer) and Assigned(TInIOCPServer(FServer).BusiWorkMgr) then   // 运行状态，建实例
      TInIOCPServer(FServer).BusiWorkMgr.AddDataModule(FDataModuleList.Count - 1);
  end;
end;

procedure TInDatabaseManager.DBConnect(Socket: TIOCPSocket);
var
  DBConnection: Integer;
begin
  // 设置要连接的数据模编号(见: TInDBConnection.Connect)
  DBConnection := Socket.Params.Target;
  if (DBConnection >= 0) and (DBConnection < FDataModuleList.Count) then
  begin
    if Assigned(Socket.Envir) then
      Socket.Envir^.DBConnection := DBConnection;
    TBusiWorker(Socket.Worker).SetConnection(DBConnection);
    Socket.Result.ActResult := arOK;
  end else
    Socket.Result.ActResult := arFail;
end;

procedure TInDatabaseManager.Clear;
begin
  FDataModuleList.Clear;
end;

constructor TInDatabaseManager.Create(AOwner: TComponent);
begin
  inherited;
  FDataModuleList := TInStringList.Create;
end;

destructor TInDatabaseManager.Destroy;
begin
  FDataModuleList.Free;
  inherited;
end;

procedure TInDatabaseManager.Execute(Socket: TIOCPSocket);
  procedure InnerSetConnection;
  begin
    if Assigned(Socket.Envir) then  // 准备当前操作的数模实例
      if (Socket.Envir^.DBConnection <> Integer(Socket.Params.Target)) then
        Socket.Envir^.DBConnection := Socket.Params.Target;
    TBusiWorker(Socket.Worker).SetConnection(Socket.Params.Target);
  end;
begin
  // 数据库操作与数模关系密切，不进入界面性业务模块，减少复杂性。
  // 调用前已经设置用当前数据连接，见：TBusiWorker.Execute

  case Socket.Action of
    atAfterSend: begin   // 发送附件完毕
      Socket.Result.Clear;
      Socket.Result.ActResult := arOK;
    end;

    atAfterReceive: begin // 接收附件完毕
      Socket.Params.Clear;
      Socket.Result.ActResult := arOK;
    end;

    else  // ==============================
      case Socket.Params.Action of
        atDBGetConns:       // 查询数据库连接情况（一个数据模块一种（个）数据库连接）
          GetDBConnections(Socket);

        atDBConnect:        // 数据库连接
          DBConnect(Socket);

        atDBExecQuery: begin // SELECT-SQL 查询, 有数据集返回
          InnerSetConnection;
          TBusiWorker(Socket.Worker).DataModule.ExecQuery(Socket.Params, Socket.Result);
        end;

        atDBExecSQL: begin  // 执行 SQL
          InnerSetConnection;
          TBusiWorker(Socket.Worker).DataModule.ExecSQL(Socket.Params, Socket.Result);
        end;

        atDBExecStoredProc: begin // 执行存储过程
          InnerSetConnection;
          TBusiWorker(Socket.Worker).DataModule.ExecStoredProcedure(Socket.Params, Socket.Result);
        end;

        atDBApplyUpdates: begin   // 修改的数据
          InnerSetConnection;
          TBusiWorker(Socket.Worker).DataModule.ApplyUpdates(Socket.Params, Socket.Result);
        end;
      end;
  end;
end;

function TInDatabaseManager.GetDataModuleCount: Integer;
begin
  Result := FDataModuleList.Count;
end;

procedure TInDatabaseManager.GetDataModuleState(Index: Integer;
          var AClassName, ADescription: String; var ARunning: Boolean);
var
  Item: PStringItem;
begin
  // 取编号为 Index 的数模状态
  if (Index >= 0) and (Index < FDataModuleList.Count) then
  begin
    Item := FDataModuleList.Items[Index];
    AClassName := TClass(Item^.FObject).ClassName;  // 类名
    ADescription := Item^.FString;    // 描述
    if Assigned(FServer) and Assigned(TInIOCPServer(FServer).BusiWorkMgr) then
      ARunning := TInIOCPServer(FServer).BusiWorkMgr.DataModuleState[Index]  // 运行状态
    else
      ARunning := False;
  end else
  begin
    AClassName := '(未知)';
    ADescription := '(未注册)';
    ARunning := False;
  end;  
end;

procedure TInDatabaseManager.GetDBConnections(Socket: TIOCPSocket);
begin
  // 取数模列表
  if (FDataModuleList.Count = 0) then
    Socket.Result.ActResult := arMissing
  else begin
    Socket.Result.AsString['dmCount'] := FDataModuleList.DelimitedText;
    Socket.Result.ActResult := arExists;
  end;
end;

procedure TInDatabaseManager.RemoveDataModule(Index: Integer);
begin
  // 删除数模
  if Assigned(FServer) and (Index >= 0) and (Index < FDataModuleList.Count) then
    if Assigned(TInIOCPServer(FServer).BusiWorkMgr) then  // 释放实例，但不能删除列表（防影响后面的数模编号）
      TInIOCPServer(FServer).BusiWorkMgr.RemoveDataModule(Index)
    else
      FDataModuleList.Delete(Index);    // 不是运行状态，直接删除
end;

procedure TInDatabaseManager.ReplaceDataModule(Index: Integer;
  ADataModule: TDataModuleClass; const ADescription: String);
var
  Item: PStringItem;
begin
  // 覆盖一个已经释放实例的数模
  if (Index >= 0) and (Index < FDataModuleList.Count) then
  begin
    Item := FDataModuleList.Items[Index];
    if not Assigned(TInIOCPServer(FServer).BusiWorkMgr) then   // 非运行状态
    begin
      Item^.FObject := TObject(ADataModule); // 类名
      Item^.FString := ADescription;         // 描述
    end else
    if not TInIOCPServer(FServer).BusiWorkMgr.DataModuleState[Index] then   // 运行状态且实例未建
    begin
      Item^.FObject := TObject(ADataModule); // 类名
      Item^.FString := ADescription;         // 描述
      TInIOCPServer(FServer).BusiWorkMgr.AddDataModule(Index);
    end;
  end;
end;

{ TInCustomManager }

constructor TInCustomManager.Create(AOwner: TComponent);
begin
  inherited;
  FFunctions := TInStringList.Create;
end;

destructor TInCustomManager.Destroy;
begin
  FFunctions.Free;
  inherited;
end;

procedure TInCustomManager.Execute(Socket: TIOCPSocket);
var
  FunctionGroup: TInRemoteFunctionGroup;
begin
  // 先执行内部操作事件
  // 见：TInFunctionClient.Call
  //   服务端的附件先发送，才收到客户端的附件
  
  case Socket.Action of
    atAfterSend:     // 发送附件完毕
      try
        // 未公开附件发送完毕事件
      finally
        Socket.Result.Clear;
        Socket.Result.ActResult := arOK;
      end;

    atAfterReceive:  // 接收附件完毕
      try
        if Assigned(FOnAttachFinish) then
          FOnAttachFinish(Socket.Worker, Socket.Params);
      finally
        Socket.Params.Clear;
        Socket.Result.ActResult := arOK;
      end;

  else // =================================

    case Socket.Params.Action of
      atCallFunction:       // 查找，执行远程函数（大写）
        if FFunctions.IndexOf(UpperCase(Socket.Params.FunctionGroup),
                              Pointer(FunctionGroup)) then
          FunctionGroup.Execute(Socket)
        else  // 远程函数组不存在
          Socket.Result.ActResult := arMissing;

      atCustomAction: begin // 自定义操作
        FOnReceive(Socket.Worker, Socket.Params, Socket.Result);
        if(Socket.Params.AttachSize > 0) and
          (Assigned(Socket.Params.Attachment) = False) then // 有附件，未确定要接收附件
          if Assigned(FOnAttachBegin) then
            FOnAttachBegin(Socket.Worker, Socket.Params);
      end;
    end;
  end;
end;

{ TInRemoteFunctionGroup }

procedure TInRemoteFunctionGroup.Execute(Socket: TIOCPSocket);
begin
  case Socket.Action of
    atAfterSend: begin   // 发送附件完毕
      Socket.Result.Clear;
      Socket.Result.ActResult := arOK;
    end;

    atAfterReceive: begin // 接收附件完毕
      Socket.Params.Clear;
      Socket.Result.ActResult := arOK;
    end;
    
    else  // ==============================
      if Assigned(FOnExecute) then
        FOnExecute(Socket.Worker, Socket.Params, Socket.Result);
  end;
end;

procedure TInRemoteFunctionGroup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (operation = opRemove) and (AComponent = FCustomManager) then
    FCustomManager := nil;
end;

procedure TInRemoteFunctionGroup.SetCustomManager(const Value: TInCustomManager);
var
  i: Integer;
begin
  if Assigned(FCustomManager) then  // 删除
  begin
    i := FCustomManager.FFunctions.IndexOf(Self);
    if (i > -1) then
    begin
      FCustomManager.FFunctions.Delete(i);
      FCustomManager.RemoveFreeNotification(Self);
    end;
  end;

  FCustomManager := Value;

  if Assigned(FCustomManager) then
  begin
    i := FCustomManager.FFunctions.IndexOf(Self);
    if (i = -1) then
      if (FFuncGroupName <> '') then
      begin
        FCustomManager.FFunctions.Add(UpperCase(FFuncGroupName), Self);
        FCustomManager.FreeNotification(Self);
      end else
      if not (csDesigning in ComponentState) then
        raise Exception.Create('错误：函数没有名称（必须唯一）');
  end;
end;

{ TInWebSocketManager }

procedure TInWebSocketManager.Broadcast(Socket: TWebSocket);
begin
  // 广播 Socket 收到的消息，不限制权限，聊天室大家都看得到
  InterPushMsg(Socket, nil);
end;

procedure TInWebSocketManager.Broadcast(const Group, Text: string; OpCode: TWSOpCode);
begin
  // 广播消息 Text，不能太长
  //   OpCode = ocClose 时，全部客户端关闭
  if (OpCode in [ocText, ocClose, ocPong]) then
    InterPushMsg(nil, OpCode, System.AnsiToUtf8(Text), Group);
end;

procedure TInWebSocketManager.CallbackMethod(ObjType: TObjectType;
  var FromObject: Pointer; const Data: TObject; var CancelScan: Boolean);
type
  PChars10 = ^TChars10;
  TChars10 = array[0..9] of AnsiChar;
  PChars11 = ^TChars11;
  TChars11 = array[0..11] of AnsiChar;
var
  iSize: Integer;
begin
  if (Length(FUserName) = 0) then
  begin
    // 返回用户列表，先填写到值，后续等下次、末尾
    // [{"NAME":"aaa"},{"NAME":"bbb"},{"NAME":"ccc"}]
    iSize := Length(TWebSocket(Data).UserName);
    if (iSize > 0) then
    begin
      if (FJSONLength = 0) then
      begin
        PChars10(FromObject)^ := AnsiString('[{"NAME":"');
        Inc(PAnsiChar(FromObject), 10);
        Inc(FJSONLength, 10);
      end else
      begin
        PChars11(FromObject)^ := AnsiString('"},{"NAME":"');
        Inc(PAnsiChar(FromObject), 12);
        Inc(FJSONLength, 12);
      end;
      System.Move(TWebSocket(Data).UserName[1], FromObject^, iSize);
      Inc(PAnsiChar(FromObject), iSize);
      Inc(FJSONLength, iSize);
    end;
  end else
  if (TWebSocket(Data).UserName = FUserName) then  // 已加锁
  begin
    FromObject := Data;
    CancelScan := True; // 退出查找
  end;
end;

procedure TInWebSocketManager.Delete(Admin: TWebSocket; const ToUser: String);
var
  oSocket: TWebSocket;
begin
  // 把 ToUser 踢出去（发送一条关闭消息）
  if (Admin.Role >= crAdmin) and Logined(ToUser, oSocket) then
    InterPushMsg(oSocket, ocClose);
end;

procedure TInWebSocketManager.Execute(Socket: TIOCPSocket);
begin
  if Assigned(FOnReceive) then
    FOnReceive(Socket.Worker, TWebSocket(Socket));  // Socket 实际上是 TWebSocket
end;

procedure TInWebSocketManager.GetUserList(Socket: TWebSocket);
var
  JSON: AnsiString;
  Buffers2: Pointer;
begin
  // 返回用户列表, 用 JSON 返回，字段：NAME
  Socket.ObjPool.Lock;
  try
    FJSONLength := 0; // 长度
    FUserName := '';  // 不是查找用户
    SetLength(JSON, TInIOCPServer(FServer).WebSocketPool.UsedCount * (SizeOf(TNameString) + 12));
    Buffers2 := PAnsiChar(@JSON[1]);
    TInIOCPServer(FServer).WebSocketPool.Scan(Buffers2, CallbackMethod);
  finally
    Socket.ObjPool.UnLock;
  end;
  if (FJSONLength = 0) then  // 没有内容
    Socket.SendData('{}')
  else begin
    PThrChars(Buffers2)^ := AnsiString('"}]');
    Inc(FJSONLength, 3);
    System.Delete(JSON, FJSONLength + 1, Length(JSON));
    Socket.SendData(JSON);
  end;
end;

procedure TInWebSocketManager.InterPushMsg(Source, Target: TWebSocket);
var
  Msg: TPushMessage;
  Stream: TMemoryStream;
begin
  // 发收到的消息发给 Target 或广播
  //   提交到推送线程，不知道何时发出、是否成功。

  if (Source.OpCode <> ocText) then
  begin
    Source.SendData('只能推送文本消息.');
    Exit;
  end;

  if (Source.Completed = False) or
     (Source.MsgSize > IO_BUFFER_SIZE - 50) or
     (Source.JSON.Size > IO_BUFFER_SIZE - 50) or
     (Source.Result.Size > IO_BUFFER_SIZE - 50) then
  begin
    Source.SendData('消息未完整接收或太长, 放弃.');
    Exit;
  end;

  if Source.Background then
  begin
    Msg := TPushMessage.Create(Source.ObjPool, Source.BufferPool, btUnknown);
    Msg.Add(Target);  // 加入 Target

    Stream := TMemoryStream.Create;
    try
      Source.Result.SaveToStream(Stream, True);
      Msg.WriteWebSocketMsg(ocText, Stream.Memory, Stream.Size);
    finally
      Stream.Free;
    end;
  end else
  begin
    // 清除掩码、宿主
    TWebSocketRef(Source).ClearOwnerMark;
    if Assigned(Target) then  // 发给 Target
    begin
      Msg := TPushMessage.Create(Source.RecvBuf, btUnknown);
      Msg.Add(Target);  // 加入 Target
    end else
    begin  // 广播
      Msg := TPushMessage.Create(Source.RecvBuf, btAllClient);
      Msg.IngoreSocket := Source;
      Msg.Group := Source.UserGroup;  // 分组
    end;
  end;

  // 加入推送线程
  if TInIOCPServer(FServer).PushManager.AddWork(Msg) then
    TWebSocketRef(Source).InternalPong  // Pong 客户端（延时，必须）
  else
    Source.SendData('系统繁忙, 放弃.'); // 繁忙，放弃，Msg 已被释放

end;

procedure TInWebSocketManager.InterPushMsg(Socket: TWebSocket; OpCode: TWSOpCode;
  const Text, Group: AnsiString);
var
  Msg: TPushMessage;
begin
  // 给 Socket/全部客户端 推送文本消息 Text
  if (Length(Text) <= IO_BUFFER_SIZE - 70) then
  begin
    if Assigned(Socket) then // 给 Socket，ioPush，长度未知 0
    begin
      Msg := TPushMessage.Create(Socket, ioPush, 0);
      Msg.IngoreSocket := Socket;  // 忽略自己
      if (Socket.Background = False) then
        Msg.Group := Socket.UserGroup;  // 分组
    end else
    begin  // 广播
      Msg := TPushMessage.Create(TInIOCPServer(FServer).WebSocketPool,
                                 TInIOCPServer(FServer).IODataPool, btAllClient);
      Msg.IngoreSocket := nil;
      Msg.Group := Group;
    end;

    // 构建帧，操作：OpCode，长度：Length(Text)
    Msg.WriteWebSocketMsg(OpCode, PAnsiChar(Text), Length(Text));

    TInIOCPServer(FServer).PushManager.AddWork(Msg);
  end;
end;

function TInWebSocketManager.Logined(const UserName: String; var Socket: TWebSocket): Boolean;
begin
  // 查找用户 UserName
  // 找到后保存到 Socket，返回真
  TInIOCPServer(FServer).WebSocketPool.Lock;
  try
    Socket := nil;
    FUserName := UserName;  // 查找它
    TInIOCPServer(FServer).WebSocketPool.Scan(Pointer(Socket), CallbackMethod);
  finally
    Result := Assigned(Socket);
    TInIOCPServer(FServer).WebSocketPool.UnLock;
  end;
end;

function TInWebSocketManager.Logined(const UserName: String): Boolean;
var
  Socket: TWebSocket;
begin
  Result := Logined(UserName, Socket);
end;

procedure TInWebSocketManager.SendTo(const ToUser, Text: string);
var
  oSocket: TWebSocket;
begin
  // 发送一条消息给 ToUser，Msg不能太长
  if (Length(Text) > 0) and (Length(ToUser) > 0) and Logined(ToUser, oSocket) then
    InterPushMsg(oSocket, ocText, System.AnsiToUtf8(Text));
end;

procedure TInWebSocketManager.SendTo(Socket: TWebSocket; const ToUser: string);
var
  Target: TWebSocket;
begin
  // 把 Socket 的消息发给 ToUser
  if Logined(ToUser, Target) then
    InterPushMsg(Socket, Target);
end;

{ TInHttpDataProvider }

procedure TInHttpDataProvider.Execute(Socket: THttpSocket);
begin
  case Socket.Request.Method of
    hmGet:
      if Assigned(FOnGet) then
        FOnGet(Socket.Worker, Socket.Request, Socket.Response);
    hmPost:  // 上传完毕才调用 Post
      if Assigned(FOnPost) and Socket.Request.Completed then
        FOnPost(Socket.Worker, Socket.Request, Socket.Response);
    hmConnect:
      { } ;
    hmDelete:
      if Assigned(FOnDelete) then
        FOnDelete(Socket.Worker, Socket.Request, Socket.Response);
    hmPut:
      if Assigned(FOnPut) then
        FOnPut(Socket.Worker, Socket.Request, Socket.Response);
    hmOptions:
      if Assigned(FOnOptions) then
        FOnOptions(Socket.Worker, Socket.Request, Socket.Response);
    hmTrace:
      if Assigned(FOnTrace) then
        FOnTrace(Socket.Worker, Socket.Request, Socket.Response);
    hmHead:  // 设置 Head，稍后发送
      Socket.Response.SetHead;
  end;
end;

function TInHttpDataProvider.GetGlobalLock: TThreadLock;
begin
  // 取全局锁
  if Assigned(FServer) then
    Result := TInIOCPServer(FServer).GlobalLock
  else
    Result := nil;
end;

procedure TInHttpDataProvider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  // 设计时收到删除组件消息
  if (Operation = opRemove) and (AComponent = FWebSocketManager) then
    FWebSocketManager := nil;
end;

procedure TInHttpDataProvider.SetWebSocketManager(const Value: TInWebSocketManager);
begin
  if (FWebSocketManager <> Value) then
  begin
    FWebSocketManager := Value;
    UpdateServer;
  end;
end;

procedure TInHttpDataProvider.UpdateServer;
begin
  if Assigned(FWebSocketManager) then
  begin
    FWebSocketManager.FServer := FServer;
    FOnUpgrade := FWebSocketManager.FOnUpgrade;  // 在父类
    if (TIOCPServerRef(FServer).FBaseMgr = nil) then // 第一任务管理器
      TIOCPServerRef(FServer).FBaseMgr := FWebSocketManager;      
  end;
end;

{ TPostSocketThread }

procedure TPostSocketThread.Execute;
var
  CreateCount: Integer;
  RecreateSockets: Boolean;
  oSocket: TSocketBroker;
begin
  // 建反向连接

  while (Terminated = False) do
  begin
    // 是否要重新建
    TInIOCPServer(FOwner.FServer).GlobalLock.Acquire;
    try
      CreateCount := FOwner.FCreateCount;
      RecreateSockets := (TInIOCPServer(FOwner.FServer).IOCPSocketPool.UsedCount = 0);
      FOwner.FCreateCount := 0;
    finally
      TInIOCPServer(FOwner.FServer).GlobalLock.Release;
    end;

    if (CreateCount > 0) then // 补充投放连接
      FOwner.InterConnectOuter(CreateCount)
    else
    if RecreateSockets then  // 重建
    begin
      // 先建一个套接字，尝试连接
      oSocket := TInIOCPServer(FOwner.FServer).IOCPSocketPool.Pop^.Data;
      TSocketBrokerRef(oSocket).SetConnection(FOwner.FServer, iocp_utils.CreateSocket);

      while (Terminated = False) do
        if iocp_utils.ConnectSocket(oSocket.Socket,
                                    FOwner.FServerAddr,
                                    FOwner.FServerPort) then // 连接
        begin
          iocp_wsExt.SetKeepAlive(oSocket.Socket);  // 心跳
          TInIOCPServer(FOwner.FServer).IOCPEngine.BindIoCompletionPort(oSocket.Socket);  // 绑定
          TSocketBrokerRef(oSocket).SendInnerFlag;  // 发送标志
          Break;
        end else
        if (Terminated = False) then
          Sleep(100);

      // 继续投放连接
      FOwner.InterConnectOuter(FOwner.FConnectionCount - 1);
    end;

    if (Terminated = False) then
      Sleep(100); // 等待
  end;

end;

{ TBrokenOptions }

procedure TBrokenOptions.CheckServerPort(const AServerAddr: string; AServerPort: Word);
begin
  if (AServerAddr = GetServerAddr) and (AServerPort = GetServerPort) then
    raise Exception.Create('内部/外部代理服务端口不能与IOCP服务的一致.');
end;

constructor TBrokenOptions.Create(AOwner: TInIOCPBroker);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TBrokenOptions.GetServerAddr: string;
begin
  if (Self is TProxyOptions) then
    Result := FOwner.FServerAddr
  else
    Result := FOwner.FDefaultInnerAddr;
end;

function TBrokenOptions.GetServerPort: Word;
begin
  if (Self is TProxyOptions) then
    Result := FOwner.FServerPort
  else
    Result := FOwner.FDefaultInnerPort;
end;

procedure TBrokenOptions.SetServerAddr(const Value: string);
begin
  if (Self is TProxyOptions) then
    FOwner.FServerAddr := Value
  else
    FOwner.FDefaultInnerAddr := Value;
end;

procedure TBrokenOptions.SetServerPort(const Value: Word);
begin
  if (Self is TProxyOptions) then
    FOwner.FServerPort := Value
  else
    FOwner.FDefaultInnerPort := Value;
end;

{ TProxyOptions }

function TProxyOptions.GetConnectionCount: Word;
begin
  Result := FOwner.FConnectionCount;
end;

procedure TProxyOptions.SetConnectionCount(const Value: Word);
begin
  FOwner.FConnectionCount := Value;
end;

{ TInIOCPBroker }

procedure TInIOCPBroker.AddConnection(Broker: TSocketBroker; const InnerId: String);
var
  i: Integer;
  Connections: TInList;
begin
  // 加内部连接到列表（已经在 IOCPSocketPool）
  //   每一 InnerId 对应一个反向代理，对应一个局域网。
  GlobalLock.Acquire;
  try
    i := FReverseBrokers.IndexOf(InnerId);  // 大写
    if (i = -1) then  // 新建，加入列表
    begin
      Connections := TInList.Create;
      FReverseBrokers.AddObject(InnerId, Connections);
    end else
      Connections := TInList(FReverseBrokers.Objects[i]);
    Connections.Add(Broker);
  finally
    GlobalLock.Release;
  end;
end;

procedure TInIOCPBroker.BindInnerBroker(Connection: TSocketBroker;
  const Data: PAnsiChar; DataSize: Cardinal);
var
  i, k: Integer;
  oSocket: TSocketBroker;
begin
  // 把外部连接和内部的关联起来，内部连接按 BrokerId 分组
  if (FProxyType = ptOuter) then
  begin
    k := 0;
    repeat
      GlobalLock.Acquire;
      try
        case FReverseBrokers.Count of
          0:
            oSocket := nil;
          1:  // 用第一个
            oSocket := TSocketBroker(TInList(FReverseBrokers.Objects[0]).PopFirst);
          else begin
            i := FReverseBrokers.IndexOf(TSocketBrokerRef(Connection).FBrokerId);
            if (i > -1) then
              oSocket := TSocketBroker(TInList(FReverseBrokers.Objects[i]).PopFirst)
            else
              oSocket := nil;            
          end;
        end;
      finally
        GlobalLock.Release;
      end;
      if Assigned(oSocket) then
        TSocketBrokerRef(Connection).AssociateInner(oSocket)
      else begin
        Inc(k);
        Sleep(10);
      end;
    until TInIOCPServer(FServer).Active and ((k > 100) or Assigned(oSocket));
  end;
end;

procedure TInIOCPBroker.CheckServerPort(const AServerAddr: string; AServerPort: Word);
begin
  // 检查内网服务的端口（不能和 TInIOCPServer 的一样）
  FOuterServer.CheckServerPort(AServerAddr, AServerPort);
  FInnerServer.CheckServerPort(AServerAddr, AServerPort);
end;

constructor TInIOCPBroker.Create(AOwner: TComponent);
begin
  inherited;
  FInnerServer := TBrokenOptions.Create(Self);
  FOuterServer := TProxyOptions.Create(Self);
  FConnectionCount := 50;
  FDefaultInnerPort := 80;
  FServerPort := 80;
end;

destructor TInIOCPBroker.Destroy;
var
  i: Integer;
begin
  // 释放资源
  if Assigned(FThread) then
    FThread.Terminate;
  if (FProxyType = ptOuter) and Assigned(FReverseBrokers) then
  begin
    for i := 0 to FReverseBrokers.Count - 1 do  // 逐一释放
      TInList(FReverseBrokers.Objects[i]).Free;
    FReverseBrokers.Free;
  end;
  FInnerServer.Free;
  FOuterServer.Free;
  inherited;
end;

function TInIOCPBroker.GetReverseMode: Boolean;
begin
  // 是否为反向模式
  Result := (FProxyType = ptDefault) and
            (FServerAddr <> '') and (FServerPort > 0);
end;

procedure TInIOCPBroker.IncOuterConnection;
begin
  // 补充内部连接
  GlobalLock.Acquire;
  try
    if TInIOCPServer(FServer).Active then
      Inc(FCreateCount);
  finally
    GlobalLock.Release;
  end;
end;

procedure TInIOCPBroker.InterConnectOuter(ACount: Integer);
var
  i: Integer;
begin
  // 建代理对象，连接到外部服务器
  for i := 0 to ACount - 1 do
    PostConnectionsEx;
end;

procedure TInIOCPBroker.PostConnections;
begin
  // 用线程建连接
  if not Assigned(FThread) then
  begin
    if (FConnectionCount < 2) then
      FConnectionCount := 2;
    FThread := TPostSocketThread.Create(True);
    FThread.FreeOnTerminate := True;
    FThread.FOwner := Self;
    FThread.Resume;
  end;
end;

procedure TInIOCPBroker.PostConnectionsEx;
var
  lResult: Boolean;
  oSocket: TSocketBroker;
begin
  // 投放连接至外部服务器
  oSocket := TInIOCPServer(FServer).IOCPSocketPool.Pop^.Data;
  TSocketBrokerRef(oSocket).SetConnection(FServer, iocp_utils.CreateSocket);  // 建套接字

  if iocp_utils.ConnectSocket(oSocket.Socket, FServerAddr, FServerPort) then // 连接
  begin
    iocp_wsExt.SetKeepAlive(oSocket.Socket);  // 心跳
    lResult := TInIOCPServer(FServer).IOCPEngine.BindIoCompletionPort(oSocket.Socket);  // 绑定
    if lResult then
      TSocketBrokerRef(oSocket).SendInnerFlag  // 发送标志
    else
      TInIOCPServer(FServer).CloseSocket(oSocket);
  end else
    TInIOCPServer(FServer).CloseSocket(oSocket);
end;

procedure TInIOCPBroker.Prepare;
begin
  case FProxyType of
    ptDefault:  // 建到外部的连接
      if (FServerAddr <> '') and (FServerPort > 0) then
        PostConnections;
    ptOuter:    // 建内部连接列表
      FReverseBrokers := TStringList.Create;
  end;
end;

procedure TInIOCPBroker.Stop;
begin
  // 停止
  if Assigned(FThread) then
    FThread.Terminate;
  GlobalLock.Acquire;
  try
    FThread := nil;
  finally
    GlobalLock.Release;
  end;  
end;

{ TBusiWorker }

procedure TBusiWorker.AddDataModule(Index: Integer);
  function CreateNewDataModule: TInIOCPDataModule;
  begin
    Result := TDataModuleClass(FDMList.Objects[Index]).Create(TComponent(FServer));
    {$IFDEF DEBUG_MODE}
    iocp_log.WriteLog('TBusiWorker.CreateDataModule->创建数模成功: ' + IntToStr(Index));
    {$ENDIF}
  end;
begin
  // 运行状态建模（覆盖或追加到末尾）
  if (Index >= 0) and (Index < FDMCount) then
  begin
    if (FDMArray[Index] = nil) then   // 可以覆盖
      FDMArray[Index] := CreateNewDataModule;
  end else
  if (Index = FDMList.Count - 1) then // 新增，要先注册到列表
  begin
    FDMCount := FDMList.Count;
    SetLength(FDMArray, FDMCount);
    FDMArray[Index] := CreateNewDataModule;
  end;
end;

constructor TBusiWorker.Create(AServer: TObject; AThreadIdx: Integer);
begin
  FDataModule := nil;
  FThreadIdx := AThreadIdx;

  FServer := AServer;    // TInIOCPServer
  FGlobalLock := TInIOCPServer(FServer).GlobalLock;

  if Assigned(TInIOCPServer(FServer).DatabaseManager) then
  begin
    FDMList := TInIOCPServer(FServer).DatabaseManager.DataModuleList; // 引用数模列表
    FDMCount := FDMList.Count;  // 关闭时 FDMList.Clear，记住
  end else
  begin
    FDMList := nil;
    FDMCount := 0;
  end;

  inherited Create;
end;

procedure TBusiWorker.CreateDataModules;
var
  i: Integer;
begin
  // 建数模实例（一个业务执行者中，一个数模一个实例）
  if (FDMCount > 0) then
  begin
    SetLength(FDMArray, FDMCount);
    for i := 0 to FDMCount - 1 do
    begin
      FDMArray[i] := TDataModuleClass(FDMList.Objects[i]).Create(TComponent(FServer));
      {$IFDEF DEBUG_MODE}
      iocp_log.WriteLog('TBusiWorker.CreateDataModule->创建数模成功: ' + IntToStr(i));
      {$ENDIF}
    end;
  end;
end;

destructor TBusiWorker.Destroy;
var
  i: Integer;
begin
  // 释放数模实例
  for i := 0 to FDMCount - 1 do
    if Assigned(FDMArray[i]) then
    begin
      FDMArray[i].Free;
      {$IFDEF DEBUG_MODE}
      iocp_log.WriteLog('TBusiWorker.Destroy->释放数模成功: ' + IntToStr(i));
      {$ENDIF}
    end;
  SetLength(FDMArray, 0);
  inherited;
end;

procedure TBusiWorker.Execute(const Socket: TIOCPSocket);
begin
  // 进入 C/S 业务模块

  // 默认的数据连接
  SetDefaultDataModule;

  with TInIOCPServer(FServer) do
    case Socket.Params.Action of
      atUserLogin..atUserState:    // 客户端管理
        if Assigned(ClientManager) then
          ClientManager.Execute(Socket);

      atTextSend..atTextFileList:  // 消息服务
        if Assigned(MessageManager) then
          MessageManager.Execute(Socket);

      atFileList..atFileDownShare: // 文件管理
        if Assigned(FileManager) then
          FileManager.Execute(Socket);

      atDBGetConns..atDBApplyUpdates: // 数据库管理
        if Assigned(DatabaseManager) and Assigned(FDataModule) then
          DatabaseManager.Execute(Socket);

      atCallFunction..atCustomAction:   // 自定义消息
        if Assigned(CustomManager) then
          CustomManager.Execute(Socket);
    end;
end;

function TBusiWorker.GetDataModule(Index: Integer): TInIOCPDataModule;
begin
  // 取数模实例
  if (Index >= 0) and (Index < FDMCount) then
    Result := FDMArray[Index]
  else
    Result := nil;
end;

procedure TBusiWorker.HttpExecute(const Socket: THttpSocket);
begin
  // 进入 http 服务业务模块
  SetDefaultDataModule;  // 默认的数据连接
  TInIOCPServer(FServer).HttpDataProvider.Execute(Socket);
end;

procedure TBusiWorker.RemoveDataModule(Index: Integer);
begin
  // 热删除，保留空间，防止影响正在使用的应用
  if (Index >= 0) and (Index < FDMCount) then
  begin
    FDMArray[Index].Free;
    FDMArray[Index] := nil;
  end;
end;

procedure TBusiWorker.SetConnection(Index: Integer);
begin
  // 设置当前数模
  if (Index >= 0) and (Index < FDMCount) then
    FDataModule := FDMArray[Index];
end;

procedure TBusiWorker.SetDefaultDataModule;
begin
  // 设置当前的数据连接
  if (FDataModule = Nil) and (FDMCount > 0) then
    FDataModule := FDMArray[0];
end;

procedure TBusiWorker.StreamExecute(const Socket: TStreamSocket);
begin
  SetDefaultDataModule;  // 默认的数据连接
  TInIOCPServer(FServer).StreamManager.Execute(TIOCPSocket(Socket));
end;

procedure TBusiWorker.WebSocketExecute(const Socket: TWebSocket);
begin
  // 进入 WebSocket 业务模块
  SetDefaultDataModule;  // 默认的数据连接
  with TInIOCPServer(FServer).HttpDataProvider do
    WebSocketManager.Execute(TIOCPSocket(Socket));  // 当作 TIOCPSocket
end;

end.
