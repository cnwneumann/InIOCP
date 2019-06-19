(*
 * iocp c/s 服务客户端对象类
 *)
unit iocp_clients;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.ExtCtrls, System.Variants,
  Data.DB, Datasnap.DSIntf, Datasnap.DBClient, VCL.Forms, {$ELSE}
  Windows, Classes, SysUtils, ExtCtrls, Variants, DB, DSIntf, DBClient, Forms, {$ENDIF}
  iocp_Winsock2, iocp_base, iocp_utils, iocp_lists, iocp_senders, iocp_receivers,
  iocp_baseObjs, iocp_msgPacks, MidasLib;    // 使用时请加单元引用 MidasLib！

type

  // =================== IOCP 客户端 类 ===================

  TSendThread = class;

  TRecvThread = class;

  TPostThread = class;

  TClientParams = class;

  TResultParams = class;

  // ============ 客户端组件 基类 ============
  // 不能直接使用

  // 被动接收事件
  TPassvieEvent = procedure(Sender: TObject; Message: TResultParams) of object;

  // 结果返回事件
  TReturnEvent = procedure(Sender: TObject; Result: TResultParams) of object;

  TBaseClientObject = class(TComponent)
  protected
    FOnReceiveMsg: TPassvieEvent;   // 被动接收消息事件
    FOnReturnResult: TReturnEvent;  // 处理返回值事件
    procedure HandleFeedback(Result: TResultParams); virtual;
    procedure HandlePushedMsg(Msg: TResultParams); virtual;
  published
    property OnReturnResult: TReturnEvent read FOnReturnResult write FOnReturnResult;
  end;

  // ============ 客户端连接 ============

  // 加入任务事件
  TAddWorkEvent = procedure(Sender: TObject; Msg: TClientParams) of object;

  // 消息收发事件
  TRecvSendEvent = procedure(Sender: TObject; MsgId: TIOCPMsgId; MsgSize, CurrentSize: TFileSize) of object;

  // 异常事件
  TConnectionError = procedure(Sender: TObject; const Msg: string) of object;

  TInConnection = class(TBaseClientObject)
  private
    FSocket: TSocket;          // 套接字
    FTimer: TTimer;            // 定时器

    FSendThread: TSendThread;  // 发送线程
    FRecvThread: TRecvThread;  // 接收线程
    FPostThread: TPostThread;  // 投放线程

    FRecvCount: Cardinal;      // 共收到
    FSendCount: Cardinal;      // 共发送

    FLocalPath: string;        // 下载文件的本地存放路径
    FUserName: string;         // 登录的用户名（子组件用）
    FServerAddr: string;       // 服务器地址
    FServerPort: Word;         // 服务端口

    FActive: Boolean;          // 开关/连接状态
    FActResult: TActionResult; // 服务器反馈结果
    FAutoConnected: Boolean;   // 是否自动连接
    FCancelCount: Integer;     // 取消任务数
    FLogined: Boolean;         // 登录状态
    FMaxChunkSize: Integer;    // 续传的每次最大传输长度

    FErrorcode: Integer;       // 异常代码
    FErrMsg: string;           // 异常消息

    FReuseSessionId: Boolean;  // 凭证重用（短连接时,下次免登录）
    FRole: TClientRole;        // 权限
    FSessionId: Cardinal;      // 凭证/对话期 ID
  private
    FAfterConnect: TNotifyEvent;     // 连接后
    FAfterDisconnect: TNotifyEvent;  // 断开后
    FBeforeConnect: TNotifyEvent;    // 连接前
    FBeforeDisconnect: TNotifyEvent; // 断开前
    FOnAddWork: TAddWorkEvent;       // 加入任务事件
    FOnDataReceive: TRecvSendEvent;  // 消息接收事件
    FOnDataSend: TRecvSendEvent;     // 消息发出事件
    FOnError: TConnectionError;      // 异常事件
  private
    function GetActive: Boolean;
    procedure CreateTimer;
    procedure DoServerError(Result: TResultParams);
    procedure DoThreadFatalError;
    procedure HandleMsgHead(Result: TResultParams);
    procedure InternalOpen;
    procedure InternalClose;
    procedure ReceiveProgress;
    procedure SendProgress;
    procedure SetActive(Value: Boolean);
    procedure SetMaxChunkSize(Value: Integer);
    procedure TimerEvent(Sender: TObject);
    procedure TryDisconnect;
  protected
    procedure HandleFeedback(Result: TResultParams); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelAllWorks;                 // 取消全部任务
    procedure CancelWork(MsgId: TIOCPMsgId);  // 取消任务
    procedure PauseWork(MsgId: TIOCPMsgId);   // 暂停任务
  public
    property ActResult: TActionResult read FActResult;
    property CancelCount: Integer read FCancelCount;
    property Errorcode: Integer read FErrorcode;
    property Logined: Boolean read FLogined;
    property RecvCount: Cardinal read FRecvCount;
    property SendCount: Cardinal read FSendCount;
    property SessionId: Cardinal read FSessionId;
    property Socket: TSocket read FSocket;
    property UserName: string read FUserName;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property AutoConnected: Boolean read FAutoConnected write FAutoConnected default False;
    property LocalPath: string read FLocalPath write FLocalPath;
    property MaxChunkSize: Integer read FMaxChunkSize write SetMaxChunkSize default MAX_CHUNK_SIZE;
    property ReuseSessionId: Boolean read FReuseSessionId write FReuseSessionId default False;
    property ServerAddr: string read FServerAddr write FServerAddr;
    property ServerPort: Word read FServerPort write FServerPort default DEFAULT_SVC_PORT;
  published
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property OnAddWork: TAddWorkEvent read FOnAddWork write FOnAddWork;
    
    // 接收被动消息/推送消息事件
    property OnReceiveMsg: TPassvieEvent read FOnReceiveMsg write FOnReceiveMsg;
    property OnDataReceive: TRecvSendEvent read FOnDataReceive write FOnDataReceive;
    property OnDataSend: TRecvSendEvent read FOnDataSend write FOnDataSend;
    property OnError: TConnectionError read FOnError write FOnError;
  end;

  // ============ 客户端收到的数据包/变量表 ============

  TResultParams = class(TReceivePack)
  protected
    procedure CreateAttachment(const ALocalPath: string); override;
  end;

  // ============ TInBaseClient 内置的消息包 ============

  TClientParams = class(TBaseMessage)
  private
    FConnection: TInConnection;  // 连接
    FState: TMessagePackState;   // 状态
  protected
    function ReadDownloadInf(AResult: TResultParams): Boolean;
    function ReadUploadInf(AResult: TResultParams): Boolean;
    procedure CreateStreams(ClearList: Boolean = True); override;
    procedure ModifyMessageId;
    procedure OpenLocalFile; override;
  public
    // 协议头属性
    property Action: TActionType read FAction;
    property ActResult: TActionResult read FActResult;
    property AttachSize: TFileSize read FAttachSize;
    property CheckType: TDataCheckType read FCheckType write FCheckType;  // 读写
    property DataSize: Cardinal read FDataSize;
    property MsgId: TIOCPMsgId read FMsgId write FMsgId;  // 用户可以修改
    property Owner: TMessageOwner read FOwner;
    property SessionId: Cardinal read FSessionId;
    property Target: TActionTarget read FTarget;
    property VarCount: Cardinal read FVarCount;
    property ZipLevel: TZipLevel read FZipLevel write FZipLevel;
  public
    // 其他常用属性（读写）
    property Connection: Integer read GetConnection write SetConnection;
    property Directory: string read GetDirectory write SetDirectory;
    property FileName: string read GetFileName write SetFileName;
    property FunctionGroup: string read GetFunctionGroup write SetFunctionGroup;
    property FunctionIndex: Integer read GetFunctionIndex write SetFunctionIndex;
    property HasParams: Boolean read GetHasParams write SetHasParams;
    property LocalPath: string read GetLocalPath write SetLocalPath;
    property NewFileName: string read GetNewFileName write SetNewFileName;
    property Password: string read GetPassword write SetPassword;
    property ReuseSessionId: Boolean read GetReuseSessionId write SetReuseSessionId;
    property StoredProcName: string read GetStoredProcName write SetStoredProcName;
    property SQL: string read GetSQL write SetSQL;
    property SQLName: string read GetSQLName write SetSQLName;
  end;

  // ============ 用户自由定义发送的消息包 ============
  // 增加 Post 方法

  TMessagePack = class(TClientParams)
  private
    FThread: TSendThread;         // 发送线程
    procedure InternalPost(AAction: TActionType);
  public
    constructor Create(AOwner: TBaseClientObject);
    procedure Post(AAction: TActionType);
  end;

  // ============ 客户端组件 基类 ============

  // 列举文件事件
  TListFileEvent = procedure(Sender: TObject; ActResult: TActionResult;
                             No: Integer; Result: TCustomPack) of object;

  TInBaseClient = class(TBaseClientObject)
  private
    FFileList: TStrings;          // 查询文件的列表
    FParams: TClientParams;       // 待发送消息包（不要直接使用）
    function CheckState(CheckLogIn: Boolean = True): Boolean;
    function GetParams: TClientParams;
    procedure InternalPost(Action: TActionType = atUnknown);
    procedure ListReturnFiles(Result: TResultParams);
    procedure SetConnection(const Value: TInConnection);
  protected
    FConnection: TInConnection;   // 客户端连接
    FOnListFiles: TListFileEvent; // 列离线消息文件
  protected
    property Connection: TInConnection read FConnection write SetConnection;
    property Params: TClientParams read GetParams;
  public
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  // ============ 响应服务客户端 ============

  TInEchoClient = class(TInBaseClient)
  public
    procedure Post;
  published
    property Connection;
  end;

  // ============ 认证服务客户端 ============

  // 认证结果事件
  TCertifyEvent = procedure(Sender: TObject; Action: TActionType; ActResult: Boolean) of object;

  // 列举客户端事件
  TListClientsEvent = procedure(Sender: TObject; Count, No: Cardinal; const Client: PClientInfo) of object;

  TInCertifyClient = class(TInBaseClient)
  private
    FGroup: string;       // 分组（未用）
    FUserName: string;    // 名称
    FPassword: string;    // 密码
  private
    FOnCertify: TCertifyEvent;  // 认证（登录/登出）事件
    FOnListClients: TListClientsEvent;  // 显示客户端信息
    function GetLogined: Boolean;
    procedure InterListClients(Result: TResultParams);
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
  protected
    procedure HandleMsgHead(Result: TResultParams);
    procedure HandleFeedback(Result: TResultParams); override;
  public
    procedure Register(const AUserName, APassword: string; Role: TClientRole = crClient);
    procedure GetUserState(const AUserName: string);
    procedure Modify(const AUserName, ANewPassword: string; Role: TClientRole = crClient);
    procedure Delete(const AUserName: string);
    procedure QueryClients;
    procedure Login;
    procedure Logout;
  public
    property Logined: Boolean read GetLogined;
  published
    property Connection;
    property Group: string read FGroup write FGroup;
    property UserName: string read FUserName write SetUserName;
    property Password: string read FPassword write SetPassword;
  published
    property OnCertify: TCertifyEvent read FOnCertify write FOnCertify;
    property OnListClients: TListClientsEvent read FOnListClients write FOnListClients;
  end;

  // ============ 消息传输客户端 ============

  TInMessageClient = class(TInBaseClient)
  protected
    procedure HandleFeedback(Result: TResultParams); override;
  public
    procedure Broadcast(const Msg: string);
    procedure GetOfflineMsgs;
    procedure GetMsgFiles(FileList: TStrings = nil);
    procedure SendMsg(const Msg: string; const ToUserName: string = '');
  published
    property Connection;
    property OnListFiles: TListFileEvent read FOnListFiles write FOnListFiles;
  end;

  // ============ 文件传输客户端 ============
  // 2.0 未实现文件推送代码

  TInFileClient = class(TInBaseClient)
  protected
    procedure HandleFeedback(Result: TResultParams); override;
  public
    procedure SetDir(const Directory: string);
    procedure ListFiles(FileList: TStrings = nil);
    procedure Delete(const AFileName: string);
    procedure Download(const AFileName: string);
    procedure Rename(const AFileName, ANewFileName: string);
    procedure Upload(const AFileName: string); overload;
    procedure Share(const AFileName, AUserNameList: string);
  published
    property Connection;
    property OnListFiles: TListFileEvent read FOnListFiles write FOnListFiles;
  end;

  // ============ 数据库连接客户端 ============

  TInDBConnection = class(TInBaseClient)
  private
    FConnectionIndex: Integer;   // 连接编号
  public
    procedure GetConnections;
    procedure Connect(ANo: Cardinal);
  published
    property Connection;
    property ConnectionIndex: Integer read FConnectionIndex write FConnectionIndex;
  end;

  // ============ 数据库客户端 基类 ============

  TDBBaseClientObject = class(TInBaseClient)
  private
    FDBConnection: TInDBConnection;  // 数据库连接
    procedure SetDBConnection(const Value: TInDBConnection);
    procedure UpdateInConnection;
  public
    procedure ExecStoredProc(const ProcName: string);
  public
    property Params;
  published
    property DBConnection: TInDBConnection read FDBConnection write SetDBConnection;
  end;

  // ============ SQL 命令客户端 ============

  TInDBSQLClient = class(TDBBaseClientObject)
  public
    procedure ExecSQL;
  end;

  // ============ 数据查询客户端 类 ============

  TInDBQueryClient = class(TDBBaseClientObject)
  private
    FClientDataSet: TClientDataSet;  // 关联数据集
    FSubClientDataSets: TList;  // 数据子表
    FTableNames: TStrings; // 要更新的远程表名
    FReadOnly: Boolean;    // 是否只读
  protected
    procedure HandleFeedback(Result: TResultParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddClientDataSets(AClientDataSet: TClientDataSet);
    procedure ApplyUpdates;
    procedure ClearClientDataSets;
    procedure ExecQuery;
  public
    property ReadOnly: Boolean read FReadOnly;
  published
    property ClientDataSet: TClientDataSet read FClientDataSet write FClientDataSet;
  end;

  // ============ 自定义消息客户端 ============

  TInCustomClient = class(TInBaseClient)
  public
    procedure Post;
  public
    property Params;
  published
    property Connection;
  end;

  // ============ 远程函数客户端 ============

  TInFunctionClient = class(TInBaseClient)
  public
    procedure Call(const GroupName: string; FunctionNo: Integer);
  public
    property Params;
  published
    property Connection;
  end;

  // =================== 发送线程 类 ===================

  TMsgIdArray = array of TIOCPMsgId;

  TSendThread = class(TCycleThread)
  private
    FConnection: TInConnection; // 连接
    FLock: TThreadLock;         // 线程锁
    FSender: TClientTaskSender; // 消息发送器

    FCancelIds: TMsgIdArray;    // 待取消的消息编号数组
    FMsgList: TInList;          // 待发消息包列表
    FMsgPack: TClientParams;    // 当前发送消息包

    FMsgId: TFileSize;          // 当前消息 Id
    FTotalSize: TFileSize;      // 当前消息总长度
    FCurrentSize: TFileSize;    // 当前发出数

    FGetFeedback: Integer;      // 收到服务器反馈
    FWaitState: Integer;        // 等待反馈状态
    FWaitSemaphore: THandle;    // 等待服务器反馈的信号灯

    function GetCount: Integer;
    function GetWork: Boolean;
    function GetWorkState: Boolean;
    function InCancelArray(MsgId: TIOCPMsgId): Boolean;
    procedure AddCancelMsgId(MsgId: TIOCPMsgId);
    procedure ClearMsgList;
    procedure KeepWaiting;
    procedure IniWaitState;
    procedure InternalSend;
    procedure OnDataSend(DataType: TMessageDataType; OutSize: Integer);
    procedure OnSendError(Sender: TObject);
    procedure ServerReturn;
    procedure WaitForFeedback;
  protected
    procedure AfterWork; override;
    procedure DoMethod; override;
  public
    constructor Create(AConnection: TInConnection);
    procedure AddWork(Msg: TClientParams);
    procedure CancelWork(MsgId: TIOCPMsgId);
    procedure ClearAllWorks(var ACount: Integer);
  public
    property Count: Integer read GetCount;
  end;

  // =================== 推送结果的线程 类 ===================
  // 保存接收到的消息到列表，逐一塞进应用层

  TPostThread = class(TCycleThread)
  private
    FConnection: TInConnection; // 连接
    FLock: TThreadLock;         // 线程锁

    FResults: TInList;          // 收到的消息列表
    FResult: TResultParams;     // 收到的当前消息
    FResultEx: TResultParams;   // 等待附件发送结果的消息

    FMsgPack: TClientParams;    // 当前发送消息
    FOwner: TBaseClientObject;  // 当前发送消息所有者

    procedure ExecInMainThread;
    procedure HandleMessage(Result: TReceivePack);
  protected
    procedure AfterWork; override;
    procedure DoMethod; override;
  public
    constructor Create(AConnection: TInConnection);
    procedure Add(Result: TReceivePack);
    procedure SetMsgPack(AMsgPack: TClientParams);
  end;

  // =================== 接收线程 类 ===================

  TRecvThread = class(TThread)
  private
    FConnection: TInConnection; // 连接
    FRecvBuf: TWsaBuf;          // 接收缓存
    FOverlapped: TOverlapped;   // 重叠结构

    FReceiver: TClientReceiver; // 数据接收器
    FRecvMsg: TReceivePack;     // 当前消息

    FMsgId: TFileSize;          // 当前消息 Id
    FTotalSize: TFileSize;      // 当前消息长度
    FCurrentSize: TFileSize;    // 当前消息收到的长度

    procedure HandleDataPacket; // 处理收到的数据包
    procedure OnDataReceive(Result: TReceivePack; DataType: TMessageDataType;
                            ReceiveCount: TFileSize; AttachFinished: Boolean);
    procedure OnError(Result: TReceivePack);
  protected
    procedure Execute; override;
  public
    constructor Create(AConnection: TInConnection);
    procedure Stop;
  end;

implementation

uses
  http_base, iocp_api, iocp_wsExt;

// var
//  ExtrMsg: TStrings;
//  FDebug: TStrings;
//  FStream: TMemoryStream;

{ TBaseClientObject }

procedure TBaseClientObject.HandleFeedback(Result: TResultParams);
begin
  // 处理服务器返回的消息
  if Assigned(FOnReturnResult) then
    FOnReturnResult(Self, Result);
end;

procedure TBaseClientObject.HandlePushedMsg(Msg: TResultParams);
begin
  // 接到推送消息（被动收到其他客户端消息）
  if Assigned(FOnReceiveMsg) then
    FOnReceiveMsg(Self, Msg);
end;

{ TInConnection }

procedure TInConnection.CancelAllWorks;
begin
  // 取消全部任务
  if Assigned(FSendThread) then
  begin
    FSendThread.ClearAllWorks(FCancelCount);
    FSendThread.Activate;
    if Assigned(FOnError) then
      FOnError(Self, '取消 ' + IntToStr(FCancelCount) + ' 个任务.');
  end;
end;

procedure TInConnection.CancelWork(MsgId: TIOCPMsgId);
begin
  // 取消指定消息号的任务
  if Assigned(FSendThread) and (MsgId > 0) then
  begin
    FSendThread.CancelWork(MsgId);  // 找发送线程
    if Assigned(FOnError) then
      FOnError(Self, '取消任务，消息标志: ' + IntToStr(MsgId));
  end;
end;

constructor TInConnection.Create(AOwner: TComponent);
begin
  inherited;
  IniDateTimeFormat;

  FAutoConnected := False;  // 不自动连接
  FMaxChunkSize := MAX_CHUNK_SIZE;
  FReuseSessionId := False;

  FSessionId := INI_SESSION_ID;  // 初始凭证
  FServerPort := DEFAULT_SVC_PORT;
  FSocket := INVALID_SOCKET;  // 无效 Socket
end;

procedure TInConnection.CreateTimer;
begin
  // 建定时器
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 80;
  FTimer.OnTimer := TimerEvent;
end;

destructor TInConnection.Destroy;
begin
  SetActive(False);
  inherited;
end;

procedure TInConnection.DoServerError(Result: TResultParams);
begin
  // 收到异常数据，或反馈异常
  //  （在主线程调用执行，此时通讯是正常的）
  try
    FActResult := Result.ActResult;
    if Assigned(FOnError) then
      case FActResult of
        arOutDate:
          FOnError(Self, '服务器：凭证/认证过期.');
        arDeleted:
          FOnError(Self, '服务器：当前用户被管理员删除，断开连接.');
        arRefuse:
          FOnError(Self, '服务器：拒绝服务，断开连接.');
        arTimeOut:
          FOnError(Self, '服务器：超时退出，断开连接.');
        arErrAnalyse:
          FOnError(Self, '服务器：解析变量异常.');
        arErrBusy:
          FOnError(Self, '服务器：系统繁忙，放弃任务.');
        arErrHash:
          FOnError(Self, '服务器：校验异常.');
        arErrHashEx:
          FOnError(Self, '客户端：校验异常.');
        arErrInit:  // 收到异常数据
          FOnError(Self, '客户端：接收初始化异常，断开连接.');
        arErrPush:
          FOnError(Self, '服务器：推送消息异常.');
        arErrUser:  // 不传递 SessionId 的反馈
          FOnError(Self, '服务器：用户未登录或非法.');
        arErrWork:  // 服务端执行任务异常
          FOnError(Self, '服务器：' + Result.Msg);
      end;
  finally
    if (FActResult in [arDeleted, arRefuse, arTimeOut, arErrInit]) then
      FTimer.Enabled := True;  // 自动断开
  end;
end;

procedure TInConnection.DoThreadFatalError;
begin
  // 收发时出现致命异常/停止
  try
    if Assigned(FOnError) then
      if (FActResult = arErrNoAnswer) then
        FOnError(Self, '客户端：服务器无应答.')
      else
      if (FErrorCode > 0) then
        FOnError(Self, '客户端：' + GetWSAErrorMessage(FErrorCode))
      else
      if (FErrorCode = -1) then
        FOnError(Self, '客户端：发送异常.')
      else
      if (FErrorCode = -2) then  // 特殊编码
        FOnError(Self, '客户端：用户取消操作.')
      else
        FOnError(Self, '客户端：' + FErrMsg);
  finally
    if not FSendThread.FSender.Stoped then
      FTimer.Enabled := True;  // 自动断开
  end;
end;

function TInConnection.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Result := FActive
  else
    Result := (FSocket <> INVALID_SOCKET) and FActive;
end;

procedure TInConnection.HandleFeedback(Result: TResultParams);
begin
  HandleMsgHead(Result);
  inherited;
end;

procedure TInConnection.HandleMsgHead(Result: TResultParams);
begin
  // 处理登录、登出结果
  case Result.Action of
    atUserLogin:
      begin  // SessionId > 0 即成功
        FSessionId := Result.SessionId;
        FLogined := (FSessionId > INI_SESSION_ID);
        FRole := Result.Role;
      end;
    atUserLogout:
      begin
      // 短连接，重用凭证时 -> 保留 FSessionId
        FLogined := False;
        if not FReuseSessionId then
        begin
          FSessionId := INI_SESSION_ID;
          FRole := crUnknown;
        end;
      end;
  end;
end;

procedure TInConnection.InternalClose;
begin
  // 断开连接
  if Assigned(FBeforeDisConnect) then
    FBeforeDisConnect(Self);

  if (FSocket <> INVALID_SOCKET) then
  begin
    // 关闭 Socket
    ShutDown(FSocket, SD_BOTH);
    CloseSocket(FSocket);

    FLogined := False;
    FSocket := INVALID_SOCKET;

    if FActive then
    begin
      FActive := False;

      // 短连接：保留凭证，下次免登录
      if not FReuseSessionId then
        FSessionId := INI_SESSION_ID;

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
        FSendThread.Stop;
        FSendThread.FSender.Stoped := True;
        FSendThread.ServerReturn;
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

procedure TInConnection.InternalOpen;
begin
  // 创建 WSASocket，连接到服务器
  if Assigned(FBeforeConnect) then
    FBeforeConnect(Self);

  if (FSocket = INVALID_SOCKET) then
  begin
    // 新建 Socket
    FSocket := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);

    // 尝试连接
    FActive := iocp_utils.ConnectSocket(FSocket, FServerAddr, FServerPort);

    if FActive then  // 连接成功
    begin
      // 定时器
      CreateTimer;

      // 心跳
      iocp_wsExt.SetKeepAlive(FSocket);

      // 立刻发送 IOCP_SOCKET_FLAG，服务端转为 TIOCPSocet
      iocp_Winsock2.Send(FSocket, IOCP_SOCKET_FLAG[1], IOCP_SOCKET_FLEN, 0);

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

procedure TInConnection.Loaded;
begin
  inherited;
  // 装载后，FActive -> 打开
  if FActive and not (csDesigning in ComponentState) then
    InternalOpen;
end;

procedure TInConnection.PauseWork(MsgId: TIOCPMsgId);
begin
  // 暂停任务
  CancelWork(MsgId);  // 无实质性的暂停，任务被取消
end;

procedure TInConnection.ReceiveProgress;
begin
  // 显示接收进程
  if Assigned(FOnDataReceive) then
    FOnDataReceive(Self, FRecvThread.FMsgId,
                   FRecvThread.FTotalSize, FRecvThread.FCurrentSize);
end;

procedure TInConnection.SendProgress;
begin
  // 显示发送进程（主体、附件各一次 100%）
  if Assigned(FOnDataSend) then  // 用 FMsgSize
    FOnDataSend(Self, FSendThread.FMsgId,
                FSendThread.FTotalSize, FSendThread.FCurrentSize);
end;

procedure TInConnection.SetActive(Value: Boolean);
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

procedure TInConnection.SetMaxChunkSize(Value: Integer);
begin
  if (Value >= MAX_CHUNK_SIZE div 4) and (Value <= MAX_CHUNK_SIZE * 2) then
    FMaxChunkSize := Value
  else
    FMaxChunkSize := MAX_CHUNK_SIZE;
end;

procedure TInConnection.TimerEvent(Sender: TObject);
begin
  // 被删除、超时、拒绝服务等触发
  FTimer.Enabled := False;
  InternalClose;  // 断开连接
end;

procedure TInConnection.TryDisconnect;
begin
  // 服务器关闭时，尝试关闭客户端
  if Assigned(FTimer) then
  begin
    FTimer.OnTimer := TimerEvent;
    FTimer.Enabled := True;
  end;
end;

{ TResultParams }

procedure TResultParams.CreateAttachment(const ALocalPath: string);
var
  Msg: TCustomPack;
  InfFileName: string;
begin
  // 先检查本地续传文件(版本不同则删除)
  if (FAction = atFileDownChunk) then
  begin
    // 打开续传信息文件
    InfFileName := ALocalPath + GetFileName + '.download';
    Msg := TCustomPack.Create;

    try
      Msg.Initialize(InfFileName);

      if (Msg.Count = 2) { 开始只有 2 个字段 } or (
         (Msg.AsInt64['_FileSize'] <> GetFileSize) or
         (Msg.AsCardinal['_modifyLow'] <> AsCardinal['_modifyLow']) or
         (Msg.AsCardinal['_modifyHigh'] <> AsCardinal['_modifyHigh'])) then
      begin
        if (Msg.Count >= 5) then  // 本地文件的长度、修改时间改变
        begin
          FOffset := 0;  // 从 0 开始从新下载
          FOffsetEnd := 0;  // 0 长度
          DeleteFile(ALocalPath + GetFileName);  // 删除文件
        end;

        Msg.AsInt64['_FileSize'] := GetFileSize;
        Msg.AsCardinal['_modifyLow'] := AsCardinal['_modifyLow'];
        Msg.AsCardinal['_modifyHigh'] := AsCardinal['_modifyHigh'];

        // 保存文件
        Msg.SaveToFile(InfFileName);
      end;
    finally
      Msg.Free;
    end;
  end;
  inherited; // 执行父类代码
end;

{ TClientParams }

function TClientParams.ReadDownloadInf(AResult: TResultParams): Boolean;
var
  Msg: TCustomPack;
  InfFileName: string;
begin
  // CreateStream 前读取断点下载信息

  // 存放路径：FConnection.FLocalPath
  InfFileName := FConnection.FLocalPath + GetFileName + '.download';
  Msg := TCustomPack.Create;

  try
    Msg.Initialize(InfFileName);

    if (Msg.Count = 0) then
    begin
      FOffset := 0;  // 位移
      FOffsetEnd := FConnection.FMaxChunkSize;  // 块长度
      Msg.AsInt64['_MsgId'] := FMsgId;  // 消息标志
      Msg.AsInt64['_Offset'] := FOffset;  // 请求的位移
      Result := True;
    end else
    begin
      if (AResult = nil) then  // 取本地位移
      begin
        FMsgId := Msg.AsInt64['_MsgId'];  // 消息标志
        FOffset := Msg.AsInt64['_Offset'];
      end else
      begin  // 服务端反馈
        if (AResult.FOffsetEnd = 0) then
          FOffset := 0 // 从新开始
        else
          FOffset := AResult.FOffsetEnd + 1; // 位移推进
        FMsgId := AResult.MsgId;  // 消息标志
        Msg.AsInt64['_Offset'] := FOffset;
        Msg.AsString['_Directory'] := AResult.GetDirectory;
      end;

      // 每块长度(服务端会校正)；服务端路径（加密）
      FOffsetEnd := FConnection.FMaxChunkSize;
      SetDirectory(Msg.AsString['_Directory']);

      Result := (FOffset < Msg.AsInt64['_FileSize']); // >= 时下载完毕
    end;

    if Result then  // 保存传输信息
      Msg.SaveToFile(InfFileName)
    else  // 已经下载完毕
      DeleteFile(InfFileName);

  finally
    Msg.Free;
  end;

end;

function TClientParams.ReadUploadInf(AResult: TResultParams): Boolean;
var
  Msg: TCustomPack;
  InfFileName: string;
begin
  // 打开/新建断点上传信息（每次上传一块）

  InfFileName := FAttachFileName + '.upload';
  Msg := TCustomPack.Create;

  try
    // 读续传资源
    Msg.Initialize(InfFileName);

    if (Msg.Count = 0) or // 无资源（第一次）
       (Msg.AsInt64['_FileSize'] <> AsInt64['_FileSize']) or
       (Msg.AsCardinal['_modifyLow'] <> AsCardinal['_modifyLow']) or
       (Msg.AsCardinal['_modifyHigh'] <> AsCardinal['_modifyHigh']) then
    begin
      // 无资源 或 文件被修改过
      FOffset := 0;  // 位移
      Msg.AsInt64['_MsgId'] := FMsgId;
      Msg.AsInt64['_Offset'] := FOffset;  // 从 0 开始
      Msg.AsInt64['_FileSize'] := AsInt64['_FileSize'];
      Msg.AsCardinal['_modifyLow'] := AsCardinal['_modifyLow'];
      Msg.AsCardinal['_modifyHigh'] := AsCardinal['_modifyHigh'];
    end else
    if (AResult = nil) then  // 本地资源
    begin
      FMsgId := Msg.AsInt64['_MsgId'];
      FOffset := Msg.AsInt64['_Offset'];
    end else
    begin
      FMsgId := AResult.FMsgId;
      if (AResult.FOffset <> Msg.AsInt64['_Offset']) then  // 位移不等
        FOffset := 0  // 从新开始
      else
        FOffset := AResult.FOffsetEnd + 1; // 推进，下一块
    end;

    // 传输范围：FOffset...EOF
    Result := (FOffset < FAttachSize); // 未传输完毕

    if Result then  // 传输完毕
    begin
      if Assigned(AResult) then  // 保存服务端返回信息
      begin
        Msg.AsInt64['_Offset'] := FOffset;  // 即将传输的位移（未完成）
        Msg.AsString['_Directory'] := AResult.GetDirectory;  // 服务端文件（加密）
        Msg.AsString['_FileName'] := AResult.GetFileName; // 服务端文件名
      end else  // 第一次 Directory 为空
      if (FOffset = 0) then
        Msg.AsString['_FileName'] := GetFileName;

      // 服务端的路径（加密）、文件
      SetDirectory(Msg.AsString['_Directory']);
      SetFileName(Msg.AsString['_FileName']); // 改名
        
      // 本地文件名称（返回时用）
      SetAttachFileName(FAttachFileName);

      // 调整传输范围（要先设FOffset）
      AdjustTransmitRange(FConnection.FMaxChunkSize - 1);  // 0..64k
    end;

    if Result then
      Msg.SaveToFile(InfFileName)
    else
      DeleteFile(InfFileName);  // 传输完毕，删除资源

  finally
    Msg.Free;
  end;

end;

procedure TClientParams.CreateStreams(ClearList: Boolean);
begin
  // 检查、调整断点下载范围
  if (FState = msDefault) and (FAction = atFileDownChunk) then
    ReadDownloadInf(nil);
  inherited;
end;

procedure TClientParams.ModifyMessageId;
var
  Msg: TCustomPack;
  MsgFileName: string;
begin
  // 使用续传信息文件的 MsgId
  if (FAction = atFileDownChunk) then
    MsgFileName := FileName + '.download'
  else
    MsgFileName := FAttachFileName + '.upload';

  if FileExists(MsgFileName) then
  begin
    Msg := TCustomPack.Create;
    try
      Msg.Initialize(MsgFileName);
      FMsgId := Msg.AsInt64['_msgId'];
    finally
      Msg.Free;
    end;
  end;
end;

procedure TClientParams.OpenLocalFile;
begin
  inherited; // 先打开文件
  if (FState = msDefault) and (FAction = atFileUpChunk) then
    ReadUploadInf(nil);
end;

{ TMessagePack }

constructor TMessagePack.Create(AOwner: TBaseClientObject);
begin
  if (AOwner = nil) then  // 不能为 nil
    raise Exception.Create('消息 Owner 不能为空.');
  inherited Create(AOwner);
  if (AOwner is TInConnection) then
    FConnection := TInConnection(AOwner)
  else begin
    if (AOwner is TDBBaseClientObject) then
      TDBBaseClientObject(AOwner).UpdateInConnection;
    FConnection := TInBaseClient(AOwner).FConnection;
  end;
  if Assigned(FConnection) then
  begin
    if not FConnection.FActive and FConnection.FAutoConnected then
      FConnection.InternalOpen;
    SetUserName(FConnection.FUserName);  // 默认加入用户名
    FThread := FConnection.FSendThread;
  end;
end;

procedure TMessagePack.InternalPost(AAction: TActionType);
var
  sErrMsg: string;
begin
  // 提交消息
  if Assigned(FThread) then
  begin
    FAction := AAction; // 操作
    if (FAction in [atTextPush, atTextBroadcast]) and (Size > BROADCAST_MAX_SIZE) then
      sErrMsg := '推送的消息太长.'
    else
    if Error then
      sErrMsg := '设置变量异常.'
    else
      FThread.AddWork(Self); // 加消息到发生线程
  end else
    sErrMsg := '未连接到服务器.';

  if (sErrMsg <> '') then
  try
    if Assigned(FConnection.FOnError) then
      FConnection.FOnError(Self, sErrMsg)
    else
      raise Exception.Create(sErrMsg);
  finally
    Free;
  end;
end;

procedure TMessagePack.Post(AAction: TActionType);
begin
  if (AAction = atUserLogin) then  // 登录
    FConnection.FUserName := UserName;
  InternalPost(AAction);  // 提交消息
end;

{ TInBaseClient }

function TInBaseClient.CheckState(CheckLogIn: Boolean): Boolean;
var
  Error: string;
begin
  // 检查组件状态
  if Assigned(Params) and Params.Error then  // 异常
    Error := '错误：设置变量异常.'
  else
  if not Assigned(FConnection) then
    Error := '错误：未指定客户端连接.'
  else
  if not FConnection.Active then
  begin
    if FConnection.FAutoConnected then
      FConnection.InternalOpen;
    if not FConnection.Active then
      Error := '错误：连接服务器失败.';
  end else
  if CheckLogIn and (FConnection.FSessionId = 0) then
    Error := '错误：客户端未登录.';

  if (Error = '') then
    Result := not CheckLogIn or (FConnection.FSessionId > 0)
  else begin
    Result := False;
    if Assigned(FParams) then
      FreeAndNil(FParams);
    if Assigned(FConnection.FOnError) then
      FConnection.FOnError(Self, Error)
    else
      raise Exception.Create(Error);
  end;

end;

destructor TInBaseClient.Destroy;
begin
  if Assigned(FParams) then
    FParams.Free;
  inherited;
end;

function TInBaseClient.GetParams: TClientParams;
begin
  // 动态建一个消息包，发送后设 FParams = nil
  //    第一次调用时要先用 Params 建实例，不要用 FParams。
  if not Assigned(FParams) then
    FParams := TClientParams.Create(Self);
  if Assigned(FConnection) then
  begin
    FParams.FConnection := FConnection;
    FParams.FSessionId := FConnection.FSessionId;
    FParams.UserName := FConnection.FUserName;  // 默认加入用户名
  end;
  Result := FParams;
end;

procedure TInBaseClient.InternalPost(Action: TActionType);
begin
  // 加消息到发送线程
  if Assigned(FParams) then
  try
    FParams.FAction := Action;  // 设置操作
    FConnection.FSendThread.AddWork(FParams);
  finally
    FParams := Nil;  // 清空
  end;
end;

procedure TInBaseClient.ListReturnFiles(Result: TResultParams);
var
  i: Integer;
  RecValues: TBasePack;
begin
  // 列出文件名称
  case Result.ActResult of
    arFail:        // 目录不存在
      if Assigned(FOnListFiles) then
        FOnListFiles(Self, arFail, 0, Nil);
    arEmpty:       // 目录为空
      if Assigned(FOnListFiles) then
        FOnListFiles(Self, arEmpty, 0, Nil);
  else
    try          // 列出文件、一个文件一条记录
      try
        for i := 1 to Result.Count do
        begin
          RecValues := Result.AsRecord[IntToStr(i)];
          if Assigned(RecValues) then
            try
              if Assigned(FFileList) then // 保存到列表
                FFileList.Add(TCustomPack(RecValues).AsString['name'])
              else
              if Assigned(FOnListFiles) then
                FOnListFiles(Self, arExists, i, TCustomPack(RecValues));
            finally
              RecValues.Free;
            end;
        end;
      finally
        if Assigned(FFileList) then
          FFileList := nil;
      end;
    except
      if Assigned(FConnection.FOnError) then
        FConnection.FOnError(Self, 'TInBaseClient.ListReturnFiles读数据流异常.');
    end;
  end;
end;

procedure TInBaseClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FConnection) and (Operation = opRemove) then
    FConnection := nil;  // 关联的 TInConnection 组件被删除
end;

procedure TInBaseClient.SetConnection(const Value: TInConnection);
begin
  // 设置连接组件
  if Assigned(FConnection) then
    FConnection.RemoveFreeNotification(Self);
  FConnection := Value; // 赋值
  if Assigned(FConnection) then
    FConnection.FreeNotification(Self);
end;

{ TInEchoClient }

procedure TInEchoClient.Post;
begin
  // 响应, 不用登录
  if CheckState(False) and Assigned(Params) then
    InternalPost;
end;

{ TInCertifyClient }

procedure TInCertifyClient.Delete(const AUserName: string);
begin
  // 删除用户
  if CheckState() then
  begin
    Params.ToUser := AUserName;  // 待删除用户
    InternalPost(atUserDelete);  // 删除用户
  end;
end;

function TInCertifyClient.GetLogined: Boolean;
begin
  // 取登录状态
  if Assigned(FConnection) then
    Result := FConnection.FLogined
  else
    Result := False;
end;

procedure TInCertifyClient.GetUserState(const AUserName: string);
begin
  // 查询用户状态
  if CheckState() then
  begin
    Params.ToUser := AUserName; // 2.0 改
    InternalPost(atUserState);
  end;
end;

procedure TInCertifyClient.HandleMsgHead(Result: TResultParams);
begin
  // 处理登录、登出结果
  FConnection.HandleMsgHead(Result);
  case Result.Action of
    atUserLogin:   // SessionId > 0 即成功
      if Assigned(FOnCertify) then
        FOnCertify(Self, atUserLogin, FConnection.FLogined);
    atUserLogout:
      if Assigned(FOnCertify) then
        FOnCertify(Self, atUserLogout, True);
  end;
end;

procedure TInCertifyClient.InterListClients(Result: TResultParams);
var
  i, k, iCount: Integer;
  Buf, Buf2: TMemBuffer;
begin
  // 列出客户端信息
  try
    // TMemoryStream(Stream).SaveToFile('clients.txt');
    for i := 1 to Result.AsInteger['group'] do
    begin
      // 内容是 TMemBuffer
      Buf := Result.AsBuffer['list_' + IntToStr(i)];
      iCount := Result.AsInteger['count_' + IntToStr(i)];
      if Assigned(Buf) then
        try
          Buf2 := Buf;
          for k := 1 to iCount do  // 遍历内存块
          begin
            FOnListClients(Self, iCount, k, PClientInfo(Buf2));
            Inc(PAnsiChar(Buf2), CLIENT_DATA_SIZE);
          end;
        finally
          FreeBuffer(Buf);  // 要显式释放
        end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(FConnection.FOnError) then
        FConnection.FOnError(Self, 'TInCertifyClient.InterListClients, ' + E.Message);
    end;
  end;
end;

procedure TInCertifyClient.HandleFeedback(Result: TResultParams);
begin
  try
    case Result.Action of
      atUserLogin, atUserLogout:  // 1. 处理登录、登出
        HandleMsgHead(Result);
      atUserQuery:  // 2. 显示在线客户的的查询结果
        if Assigned(FOnListClients) then
          InterListClients(Result);
    end;
  finally
    inherited HandleFeedback(Result);
  end;
end;

procedure TInCertifyClient.Login;
begin
  // 登录
  if CheckState(False) then  // 不用检查登录状态
  begin
    Params.UserName := FUserName;
    FParams.Password := FPassword;
    FParams.ReuseSessionId := FConnection.ReuseSessionId;
    FConnection.FUserName := FUserName;  // 更新
    InternalPost(atUserLogin);
    ;
  end;
end;

procedure TInCertifyClient.Logout;
begin
  // 登出
  if CheckState() and Assigned(Params) then
  begin
    FConnection.FSendThread.ClearAllWorks(FConnection.FCancelCount); // 先清除任务
    InternalPost(atUserLogout);
  end;
end;

procedure TInCertifyClient.Modify(const AUserName, ANewPassword: string; Role: TClientRole);
begin
  // 修改用户密码、角色
  if CheckState() and (FConnection.FRole >= Role) then
  begin
    Params.ToUser := AUserName;  // 待修改的用户
    FParams.Password := ANewPassword;
    FParams.Role := Role;
    InternalPost(atUserModify);
  end;
end;

procedure TInCertifyClient.QueryClients;
begin
  // 查询全部在线客户端
  if CheckState() and Assigned(Params) then
    InternalPost(atUserQuery);
end;

procedure TInCertifyClient.Register(const AUserName, APassword: string; Role: TClientRole);
begin
  // 注册用户（管理员）
  if CheckState() and (FConnection.FRole >= crAdmin) and (FConnection.FRole >= Role) then
  begin
    Params.ToUser := AUserName;  // 2.0 用 ToUser
    FParams.Password := APassword;
    FParams.Role := Role;
    InternalPost(atUserRegister);
  end;
end;

procedure TInCertifyClient.SetPassword(const Value: string);
begin
  if not Logined and (Value <> FPassword) then
    FPassword := Value;
end;

procedure TInCertifyClient.SetUserName(const Value: string);
begin
  if not Logined and (Value <> FPassword) then
    FUserName := Value;
end;

{ TInMessageClient }

procedure TInMessageClient.Broadcast(const Msg: string);
begin
  // 管理员广播（发送消息给全部在线客户端）
  if CheckState() and (FConnection.FRole >= crAdmin) then
  begin
    Params.Msg := Msg;
    FParams.Role := FConnection.FRole;
    if (FParams.Size <= BROADCAST_MAX_SIZE) then
      InternalPost(atTextBroadcast)
    else begin
      FParams.Clear;
      raise Exception.Create('推送的消息太长.');
    end;
  end;
end;

procedure TInMessageClient.GetOfflineMsgs;
begin
  // 取离线消息
  if CheckState() and Assigned(Params) then
    InternalPost(atTextGetMsg);
end;

procedure TInMessageClient.GetMsgFiles(FileList: TStrings);
begin
  // 查询服务端的离线消息文件
  if CheckState() and Assigned(Params) then
  begin
    if Assigned(FileList) then
      FFileList := FileList;
    InternalPost(atTextFileList);
  end;
end;

procedure TInMessageClient.HandleFeedback(Result: TResultParams);
begin
  // 返回离线消息文件
  try
    if (Result.Action = atTextFileList) then  // 列出文件名称
      ListReturnFiles(Result);
  finally
    inherited HandleFeedback(Result);
  end;
end;

procedure TInMessageClient.SendMsg(const Msg, ToUserName: string);
begin
  // 发送文本
  if CheckState() then
    if (ToUserName = '') then   // 发送到服务器
    begin
      Params.Msg := Msg;
      InternalPost(atTextSend); // 简单发送
    end else
    begin
      Params.Msg := Msg;
      FParams.ToUser := ToUserName; // 发送给某用户
      if (FParams.Size <= BROADCAST_MAX_SIZE) then
        InternalPost(atTextPush)
      else begin
        FParams.Clear;
        raise Exception.Create('推送的消息太长.');
      end;
    end;
end;

{ TInFileClient }

procedure TInFileClient.Delete(const AFileName: string);
begin
  // 删除服务端用户当前路径的文件（应在外部先确认）
  if CheckState() then
  begin
    Params.FileName := AFileName;
    InternalPost(atFileDelete);
  end;
end;

procedure TInFileClient.Download(const AFileName: string);
begin
  // 下载文件
  if CheckState() then
  begin
    Params.FileName := AFileName;
    InternalPost(atFileDownload);
  end;
end;

procedure TInFileClient.HandleFeedback(Result: TResultParams);
begin
  // 返回文件查询结果
  try
    if (Result.Action = atFileList) then  // 列出文件名称
      ListReturnFiles(Result);
  finally
    inherited HandleFeedback(Result);
  end;
end;

procedure TInFileClient.ListFiles(FileList: TStrings);
begin
  // 查询服务器当前目录的文件
  if CheckState() and Assigned(Params) then
  begin
    if Assigned(FileList) then
      FFileList := FileList;
    InternalPost(atFileList);
  end;
end;

procedure TInFileClient.Rename(const AFileName, ANewFileName: string);
begin
  // 服务端文件改名
  if CheckState() then
  begin
    Params.FileName := AFileName;
    FParams.NewFileName := ANewFileName;
    InternalPost(atFileRename);
  end;
end;

procedure TInFileClient.SetDir(const Directory: string);
begin
  // 设置客户端在服务器的工作目录
  if CheckState() and (Directory <> '') then
  begin
    Params.Directory := Directory;
    InternalPost(atFileSetDir);
  end;
end;

procedure TInFileClient.Share(const AFileName, AUserNameList: string);
begin
  // 上传到服务端公共临时路径，共享文档
  if CheckState() then
  begin
    Params.FileName := AFileName;
    FParams.ToUser := AUserNameList;
    InternalPost(atFileUpShare);
  end;
end;

procedure TInFileClient.Upload(const AFileName: string);
begin
  // 上传本地文件 AFileName 到服务器
  if CheckState() and FileExists(AFileName) then
  begin
    Params.LoadFromFile(AFileName);
    InternalPost(atFileUpload);
  end;
end;

{ TInDBConnection }

procedure TInDBConnection.Connect(ANo: Cardinal);
begin
  // 连接到编号为 ANo 的数据库
  if CheckState() then
  begin
    Params.FTarget := ANo;
    FConnectionIndex := ANo;  // 保存
    InternalPost(atDBConnect);
  end;
end;

procedure TInDBConnection.GetConnections;
begin
  // 查询服务器的数据连接数/数模实例数
  if CheckState() and Assigned(Params) then
    InternalPost(atDBGetConns);
end;

{ TDBBaseClientObject }

procedure TDBBaseClientObject.ExecStoredProc(const ProcName: string);
begin
  // 执行存储过程
  //   TInDBQueryClient 处理返回的数据集，TInDBSQLClient 不处理。
  if CheckState() then
  begin
    Params.StoredProcName := ProcName;
    FParams.FTarget := FDBConnection.FConnectionIndex;  // 对应的数模编号
    InternalPost(atDBExecStoredProc);
  end;
end;

procedure TDBBaseClientObject.SetDBConnection(const Value: TInDBConnection);
begin
  if (FDBConnection <> Value) then
  begin
    FDBConnection := Value;
    UpdateInConnection;
  end;
end;

procedure TDBBaseClientObject.UpdateInConnection;
begin
  if Assigned(FDBConnection) then
    FConnection := FDBConnection.FConnection
  else
    FConnection := nil;
end;

{ TInDBSQLClient }

procedure TInDBSQLClient.ExecSQL;
begin
  // 执行 SQL
  UpdateInConnection;  // 更新 FConnection
  if CheckState() and Assigned(Params) then
  begin
    FParams.FTarget := FDBConnection.FConnectionIndex;  // 对应的数模编号
    InternalPost(atDBExecSQL);
  end;
end;

{ TInDBQueryClient }

procedure TInDBQueryClient.AddClientDataSets(AClientDataSet: TClientDataSet);
begin
  // 加入子表 TClientDataSet
  if FSubClientDataSets.IndexOf(AClientDataSet) = -1 then
    FSubClientDataSets.Add(AClientDataSet);
end;

procedure TInDBQueryClient.ApplyUpdates;
var
  i, k: Integer;
  oDataSet: TClientDataSet;
begin
  // 更新全部数据表：从主表到从表
  UpdateInConnection;  // 更新 FConnection
  if CheckState() and (FReadOnly = False) then
  begin
    k := 0;
    for i := 0 to FTableNames.Count - 1 do
    begin
      if (i = 0) then
        oDataSet := FClientDataSet
      else
        oDataSet := TClientDataSet(FSubClientDataSets[i - 1]);

      if (oDataSet.Changecount > 0) then  // 修改过，加入 Delta
      begin
        Inc(k);
        oDataSet.SetOptionalParam(szTABLE_NAME, FTableNames[i], True);
        FParams.AsVariant[FTableNames[i]] := oDataSet.Delta;
      end else  // 加入 NULL 字段
        FParams.AsVariant[FTableNames[i]] := Null;
    end;
    if (k > 0) then  // 有数据表被修改过
      InternalPost(atDBApplyUpdates);
  end;
end;

procedure TInDBQueryClient.ClearClientDataSets;
begin
  // 清除子数据表
  FSubClientDataSets.Clear;
end;

constructor TInDBQueryClient.Create(AOwner: TComponent);
begin
  inherited;
  FSubClientDataSets := TList.Create;
  FTableNames := TStringList.Create;
end;

destructor TInDBQueryClient.Destroy;
begin
  FSubClientDataSets.Free;
  FTableNames.Free;
  inherited;
end;

procedure TInDBQueryClient.ExecQuery;
begin
  // SQL 赋值时已经判断 Action 类型，见：THeaderPack.SetSQL
  UpdateInConnection;  // 更新 FConnection
  if CheckState() and Assigned(FParams) then
  begin
    FParams.FTarget := FDBConnection.FConnectionIndex;  // 对应的数模编号
    if (FParams.SQLName <> '') then  // 可能设 SQLName
      InternalPost(atDBExecQuery)
    else
      InternalPost(FParams.Action);
  end;
end;

procedure TInDBQueryClient.HandleFeedback(Result: TResultParams);

  procedure LoadResultDataSets;
  var
    i: Integer;
    XDataSet: TClientDataSet;
    DataField: TVarField;
  begin
    // 装载查询结果
    
    // 是否只读
    FReadOnly := Result.Action = atDBExecStoredProc;

    // Result 可能包含多个数据集，字段：1,2,3

    FTableNames.Clear;
    for i := 0 to Result.VarCount - 1 do
    begin
      if (i = 0) then  // 主数据表
        XDataSet := FClientDataSet
      else
        XDataSet := FSubClientDataSets[i - 1];

      XDataSet.DisableControls;
      try
        DataField := Result.Fields[i];  // 取字段 1,2,3
        FTableNames.Add(DataField.Name);  // 保存数据表名称

        XDataSet.Data := DataField.AsVariant;  // 数据赋值
        XDataSet.ReadOnly := FReadOnly;
      finally
        XDataSet.EnableControls;
      end;
    end;
  end;

  procedure MergeChangeDataSets;
  var
    i: Integer;
  begin
    // 合并本地的更新内容
    if (FClientDataSet.ChangeCount > 0) then
      FClientDataSet.MergeChangeLog;
    for i := 0 to FSubClientDataSets.Count - 1 do
      with TClientDataSet(FSubClientDataSets[i]) do
        if (ChangeCount > 0) then
          MergeChangeLog;
  end;

begin
  try
    if (Result.ActResult = arOK) then
      case Result.Action of
        atDBExecQuery,       // 1. 查询数据
        atDBExecStoredProc:  // 2. 存储过程返回结果
          if Assigned(FClientDataSet) and (Result.VarCount > 0) and
            (Integer(Result.VarCount) = FSubClientDataSets.Count + 1) then
            LoadResultDataSets;
        atDBApplyUpdates:    // 3. 更新
          MergeChangeDataSets;  // 合并本地的更新内容
      end;
  finally
    inherited HandleFeedback(Result);
  end;
end;

{ TInCustomClient }

procedure TInCustomClient.Post;
begin
  // 发送自定义消息
  if CheckState() and Assigned(FParams) then
    InternalPost(atCustomAction);
end;

{ TInFunctionClient }

procedure TInFunctionClient.Call(const GroupName: string; FunctionNo: Integer);
begin
  // 调用远程函数组 GroupName 的第 FunctionNo 个功能
  //   见：TInCustomManager.Execute
  if CheckState() then
  begin
    Params.FunctionGroup := GroupName;
    FParams.FunctionIndex := FunctionNo;
    InternalPost(atCallFunction);
  end;
end;

// ================== 发送线程 ==================

{ TSendThread }

procedure TSendThread.AddCancelMsgId(MsgId: TIOCPMsgId);
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

procedure TSendThread.AddWork(Msg: TClientParams);

  procedure ClearArrayMsgId;
  var
    i: Integer;
  begin
    // 清除任务数组内的消息编号 MsgId
    if (FCancelIds <> nil) then
      for i := 0 to High(FCancelIds) do
        if (FCancelIds[i] = Msg.MsgId) then
        begin
          FCancelIds[i] := 0;  // 清除过滤
          Break;
        end;
  end;

begin
  // 加消息到任务列表

  //   Msg 是动态生成，不会重复投放
  if (Msg.FAction in FILE_CHUNK_ACTIONS) then
    Msg.ModifyMessageId;  // 续传，修改 MsgId

  if Assigned(FConnection.FOnAddWork) then
    FConnection.FOnAddWork(Self, Msg);

  FLock.Acquire;
  try
    if (Msg.FState = msDefault) then
      ClearArrayMsgId;
    FMsgList.Add(Msg);
  finally
    FLock.Release;
  end;

  Activate;  // 激活线程
end;

procedure TSendThread.AfterWork;
begin
  // 停止线程，释放资源
  SetLength(FCancelIds, 0);
  CloseHandle(FWaitSemaphore);
  ClearMsgList;
  FMsgList.Free;
  FLock.Free;
  FSender.Free;
end;

constructor TSendThread.Create(AConnection: TInConnection);
begin
  inherited Create;
  // 信号灯
  FWaitSemaphore := CreateSemaphore(Nil, 0, 1, Nil);

  FConnection := AConnection;
  FLock := TThreadLock.Create; // 锁
  FMsgList := TInList.Create;  // 待发任务表

  FSender := TClientTaskSender.Create;   // 任务发送器
  FSender.Socket := FConnection.Socket;  // 发送套接字

  FSender.AfterSend := OnDataSend;  // 发出事件
  FSender.OnError := OnSendError;  // 发出异常事件
end;

procedure TSendThread.CancelWork(MsgId: TIOCPMsgId);
var
  i: Integer;
  Msg: TClientParams;
begin
  // 取消发送编号为 MsgId 的消息
  FLock.Acquire;
  try
    // 1. 把 MsgId 加入数组
    AddCancelMsgId(MsgId);

    // 2. 还在列表的消息
    for i := 0 to FMsgList.Count - 1 do
    begin
      Msg := FMsgList.Items[i];
      if (Msg.FMsgId = MsgId) then
      begin
        Msg.FState := msCancel;
        Break;
      end;
    end;
    
    // 3. 正在发送的消息（断点传输时再返回时取消）
    // 下载大文件时，服务端不断发送数据，客户端只能断开连接
    if Assigned(FMsgPack) and (FMsgPack.FMsgId = MsgId) then
    begin
      FMsgPack.FState := msCancel;
      if not (FMsgPack.Action in FILE_CHUNK_ACTIONS) then
      begin
        FSender.Stoped := True;
        ServerReturn;
      end;
    end;

  finally
    FLock.Release;
  end;

end;

procedure TSendThread.ClearAllWorks(var ACount: Integer);
begin
  // 先清空待发消息（不影响接受数据）
  FLock.Acquire;
  try
    ACount := FMsgList.Count;  // 取消数
    if Assigned(FMsgPack) then
    begin
      Inc(ACount);
      FMsgPack.FState := msCancel;
      FSender.Stoped := True;  // 停止
      ServerReturn;  // 忽略等待
    end;
    ClearMsgList;
  finally
    FLock.Release;
  end;
end;

procedure TSendThread.ClearMsgList;
var
  i: Integer;
begin
  // 释放列表的全部消息
  for i := 0 to FMsgList.Count - 1 do
    TClientParams(FMsgList.PopFirst).Free;
  if Assigned(FMsgPack) then
    FMsgPack.Free;
end;

procedure TSendThread.DoMethod;
begin
  // 循环执行任务

  // 当作有任务状态
  InterlockedExchange(FGetFeedback, 1);

  // 未停止，取任务成功 -> 发送
  while not Terminated and
        FConnection.FActive and GetWork() do
    try
      InternalSend;  // 发送
    except
      on E: Exception do
      begin
        FConnection.FErrMsg := E.Message;
        FConnection.FErrorcode := GetLastError;
        Synchronize(FConnection.DoThreadFatalError);
      end;
    end;

  // 检查是否有反馈：FGetFeedback > 0
  if (InterlockedDecrement(FGetFeedback) < 0) then  // 服务端无应答
  begin
    FConnection.FActResult := arErrNoAnswer;
    Synchronize(FConnection.DoThreadFatalError);  // 调用 Synchronize（不同线程）
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
var
  i: Integer;
begin
  // 从列表中取一个消息
  FLock.Acquire;
  try
    if Terminated or (FMsgList.Count = 0) or Assigned(FMsgPack) then
      Result := False
    else begin
      // 取未停止的任务
      for i := 0 to FMsgList.Count - 1 do
      begin
        FMsgPack := TClientParams(FMsgList.PopFirst);  // 任务
        if (FMsgPack.FState = msCancel) or InCancelArray(FMsgPack.FMsgId) then
        begin
          FMsgPack.Free;
          FMsgPack := nil;
        end else
          Break;
      end;
      if Assigned(FMsgPack) then
      begin
        FConnection.FPostThread.SetMsgPack(FMsgPack);  // 当前消息
        FSender.Stoped := False;  // 恢复
        Result := True;
      end else
      begin
        FConnection.FPostThread.SetMsgPack(nil);
        Result := False;
      end;
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

function TSendThread.InCancelArray(MsgId: TIOCPMsgId): Boolean;
var
  i: Integer;
begin
  // 检查是否要停止
  FLock.Acquire;
  try
    Result := Assigned(FMsgPack) and
             (FMsgPack.FMsgId = MsgId) and
             (FMsgPack.FState = msCancel);
    if (Result = False) and (FCancelIds <> nil) then
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

procedure TSendThread.IniWaitState;
begin
  // 初始化等待参数
  InterlockedExchange(FGetFeedback, 0); // 未收到反馈
  InterlockedExchange(FWaitState, 0); // 状态=0
end;

procedure TSendThread.InternalSend;

  procedure SendMsgHeader;
  begin
    IniWaitState;  // 准备等待

    // 保存消息总长度
    FMsgId := FMsgPack.MsgId;
    FTotalSize := FMsgPack.GetMsgSize(False);

    // 断点续传时要加位移
    if (FMsgPack.FAction in FILE_CHUNK_ACTIONS) then
      FCurrentSize := FMsgPack.FOffset
    else
      FCurrentSize := 0;
      
    // 发送协议头+校验码+文件描述
    FMsgPack.LoadHead(FSender.Data);

    FSender.DataType := mdtHead;  // 消息头
    FSender.SendBuffers;
  end;

  procedure SendMsgEntity;
  begin
    FSender.DataType := mdtEntity;  // 消息实体
    FSender.Send(FMsgPack.FMain, FMsgPack.FDataSize, False);  // 不释放资源
  end;

  procedure SendMsgAttachment;
  begin
    // 发送附件数据(不关闭资源)
    IniWaitState;  // 准备等待
    FSender.DataType := mdtAttachment;  // 消息附件
    if (FMsgPack.FAction = atFileUpChunk) then  // 断点续传
      FSender.Send(FMsgPack.FAttachment, FMsgPack.FAttachSize,
                   FMsgPack.FOffset, FMsgPack.FOffsetEnd, False)
    else
      FSender.Send(FMsgPack.FAttachment, FMsgPack.FAttachSize, False);
  end;

begin
  // 执行发送任务, 与服务端方法类似
  //   见：TReturnResult.ReturnResult、TDataReceiver.Prepare
  try
    FSender.Owner := FMsgPack;  // 宿主
    FMsgPack.FSessionId := FConnection.FSessionId; // 登录凭证

    // 1. 本地路径
    if (FMsgPack.LocalPath <> '') then
      FConnection.FRecvThread.FReceiver.LocalPath := AddBackslash(FMsgPack.LocalPath)
    else
      FConnection.FRecvThread.FReceiver.LocalPath := AddBackslash(FConnection.FLocalPath);

    // 2. 准备数据流
    FMsgPack.CreateStreams(False);  // 不清变量表

    if not FMsgPack.Error then
    begin
      // 3. 发协议头
      SendMsgHeader;

      // 4. 主体数据（内存流）
      if (FMsgPack.FDataSize > 0) then
        SendMsgEntity;

      // 5. 等待反馈
      if GetWorkState then
        WaitForFeedback;

      // 6. 发送附件流
      if (FMsgPack.FAttachSize > 0) and
         (FMsgPack.FActResult = arAccept) then
      begin
        SendMsgAttachment;  // 6.1 发送
        if GetWorkState then
          WaitForFeedback;  // 6.2 等待反馈
      end;
    end;
  finally
    // 7. 释放！
    FLock.Acquire;
    try
      FMsgPack.Free;
      FMsgPack := nil;
    finally
      FLock.Release;
    end;
  end;
end;

procedure TSendThread.KeepWaiting;
begin
  // 继续等待: FWaitState = 1 -> +1
  InterlockedIncrement(FGetFeedback); // 收到反馈
  if (iocp_api.InterlockedCompareExchange(FWaitState, 2, 1) = 1) then  // 状态+
    ReleaseSemaphore(FWaitSemaphore, 1, Nil);  // 触发
end;

procedure TSendThread.OnDataSend(DataType: TMessageDataType; OutSize: Integer);
begin
  // 数据成功发出，显示进程
  Inc(FCurrentSize, OutSize);

  // 是否显示发送进程:
  // 只有消息头、没有附件流、是附件流 -> 显示
  if (DataType = mdtHead) and (FMsgPack.FDataSize = 0) and (FMsgPack.AttachSize = 0) or
     (DataType = mdtEntity) and (FMsgPack.AttachSize = 0) or
     (DataType = mdtAttachment) then
    Synchronize(FConnection.SendProgress);
end;

procedure TSendThread.OnSendError(Sender: TObject);
begin
  // 处理发送异常
  if (GetWorkState = False) then  // 取消操作
  begin
    ServerReturn;  // 忽略等待
    FConnection.FRecvThread.FReceiver.Reset;
  end;
  FConnection.FErrorcode := TClientTaskSender(Sender).ErrorCode;
  Synchronize(FConnection.DoThreadFatalError); // 线程同步
end;

procedure TSendThread.ServerReturn;
begin
  // 服务器反馈 或 忽略等待
  //  1. 取消任务后收到反馈
  //  2. 收到反馈，而未等待（单机反馈比等待早）
  InterlockedIncrement(FGetFeedback); // 收到反馈
  if (InterlockedDecrement(FWaitState) = 0) then  // 1->0
    ReleaseSemaphore(FWaitSemaphore, 1, Nil);  // 信号量+1
end;

procedure TSendThread.WaitForFeedback;
begin
  // 等服务器反馈，等 WAIT_MILLISECONDS 毫秒
  if (InterlockedIncrement(FWaitState) = 1) then
    repeat
      WaitForSingleObject(FWaitSemaphore, WAIT_MILLISECONDS);
    until (InterlockedDecrement(FWaitState) <= 0);
end;

{ TPostThread }

procedure TPostThread.Add(Result: TReceivePack);
begin
  // 加一个消息到列表，激活线程

  // 1. 检查附件发送情况
  if (Result.ActResult = arAccept) then   // 服务器接受请求
  begin
    FMsgPack.FActResult := arAccept;      // FMsgPack 在等待
    FResultEx := TResultParams(Result);   // 先保存反馈结果
    FConnection.FSendThread.ServerReturn; // 唤醒
  end else
  begin
    // 2. 投放入线程队列
    // 刚连接时，可能立刻收到广播消息，
    // 此时 FMsgPack=nil，未登录，一样投放
    FLock.Acquire;
    try
      if Assigned(FResultEx) and
        (FResultEx.FMsgId = Result.MsgId) then // 发送附件后的反馈
      begin
        Result.Free;         // 释放附近上传结果（无内容）
        Result := FResultEx; // 使用真正的反馈消息（有内容）
        FResultEx.FActResult := arOK;  // 修改结果 -> 成功
        FResultEx := nil;    // 不用了
      end;
      // 加入列表
      FResults.Add(Result);
    finally
      FLock.Release;
    end;

    Activate;  // 激活
  end;
end;

procedure TPostThread.AfterWork;
var
  i: Integer;
begin
  // 清除消息
  for i := 0 to FResults.Count - 1 do
    TResultParams(FResults.PopFirst).Free;
  FLock.Free;
  FResults.Free;
  inherited;
end;

constructor TPostThread.Create(AConnection: TInConnection);
begin
  inherited Create;
  FreeOnTerminate := True;
  FConnection := AConnection;
  FLock := TThreadLock.Create; // 锁
  FResults := TInList.Create;  // 收到的消息列表
end;

procedure TPostThread.DoMethod;
var
  Result: TResultParams;
begin
  // 循环取出、处理收到的消息
  while (Terminated = False) do
  begin
    FLock.Acquire;
    try
      Result := FResults.PopFirst;  // 取出第一个
    finally
      FLock.Release;
    end;
    if Assigned(Result) then
      HandleMessage(Result) // 处理消息
    else
      Break;
  end;
end;

procedure TPostThread.ExecInMainThread;
const
  SERVER_PUSH_EVENTS =[arDeleted, arRefuse { 不应该存在 }, arTimeOut];
  SELF_ERROR_RESULTS =[arOutDate, arRefuse { c/s 模式发出 }, arErrBusy,
                       arErrHash, arErrHashEx, arErrAnalyse, arErrPush,
                       arErrUser, arErrWork];

  function IsPushedMessage: Boolean;
  begin
    FLock.Acquire;
    try
      Result := (FOwner = nil) or (FMsgPack = nil) or
                (FResult.Owner <> LongWord(FOwner));
    finally
      FLock.Release;
    end;
  end;

  function IsFeedbackMessage: Boolean;
  begin
    FLock.Acquire;
    try
      Result := (FMsgPack <> nil) and (FMsgPack.MsgId = FResult.MsgId);
    finally
      FLock.Release;
    end;
  end;

begin
  // 进入主线程，把消息提交给宿主

  try

    if IsPushedMessage() then

    {$IFNDEF DELPHI_7}
    {$REGION '. 推送来的消息'}
    {$ENDIF}

    try
      if (FResult.ActResult in SERVER_PUSH_EVENTS) then
      begin
        // 3.4 服务器推送的消息
        FConnection.DoServerError(FResult);
      end else
      begin
        // 3.5 其他客户端推送的消息
        FConnection.HandlePushedMsg(FResult);
      end;
    finally
      FResult.Free;
    end

    {$IFNDEF DELPHI_7}
    {$ENDREGION}
    {$ENDIF}

    else  // ====================================

    {$IFNDEF DELPHI_7}
    {$REGION '. 自己操作的反馈消息'}
    {$ENDIF}

    try
      // 允许不登录，更新本地的凭证
      if (FConnection.FSessionId <> FResult.FSessionId) then
        FConnection.FSessionId := FResult.FSessionId;

      if (FResult.ActResult in SELF_ERROR_RESULTS) then
      begin
        // 3.1 反馈执行异常
        FConnection.DoServerError(FResult);  // 传给连接
      end else
      if IsFeedbackMessage() then
      begin
        // 3.2 反馈正常结果
        FOwner.HandleFeedback(FResult); // 传给客户端
      end else
      begin
        // 3.3 MsgId 被服务端修改，自己推送的消息
        FConnection.HandlePushedMsg(FResult)
      end;
    finally
      if FConnection.FActive and IsFeedbackMessage() then
        FConnection.FSendThread.ServerReturn;  // 唤醒
      FResult.Free;
    end;

    {$IFNDEF DELPHI_7}
    {$ENDREGION}
    {$ENDIF}

  except
    on E: Exception do
    begin
      FConnection.FErrMsg := E.Message;
      FConnection.FErrorcode := GetLastError;
      FConnection.DoThreadFatalError;  // 在主线程，直接调用
    end;
  end;

end;

procedure TPostThread.HandleMessage(Result: TReceivePack);
var
  StopAction, ReadInfDone: Boolean;
  Msg: TMessagePack;
begin
  // 预处理消息
  // 最后要提交到主线程执行，要检查断点续传的情况

  FResult := TResultParams(Result);
  StopAction := False;

  if FConnection.FSendThread.InCancelArray(FResult.FMsgId) then
  begin
    FResult.Free;  // 操作被用户终止
    StopAction := True;
  end else
  if (FResult.FAction in FILE_CHUNK_ACTIONS) then
  begin
    // 继续请求
    Msg := TMessagePack.Create(TBaseClientObject(FResult.Owner));
    Msg.FState := msAutoPost;  // 自动提交

    Msg.FAction := atUnknown;  // 未知
    Msg.FActResult := FResult.FActResult; // 发送附件后的反馈结果
    Msg.FCheckType := FResult.FCheckType;
    Msg.FZipLevel := FResult.FZipLevel;

    if (FResult.FAction = atFileUpChunk) then
    begin
      // 断点上传，立刻打开本地文件，读文件信息
      //   见：TBaseMessage.LoadFromFile、TReceiveParams.CreateAttachment
      Msg.LoadFromFile(FResult.GetAttachFileName, True);
      ReadInfDone := not Msg.Error and Msg.ReadUploadInf(FResult);
    end else
    begin
      // 断点下载，设置下载文件，继续
      // FActResult 一般是正常，也可能校验异常
      //   见：TReturnResult.LoadFromFile、TResultParams.CreateAttachment
      Msg.FileName := FResult.FileName;
      ReadInfDone := Msg.ReadDownloadInf(FResult);
    end;

    if ReadInfDone then
      Msg.Post(FResult.FAction)
    else begin
      Msg.Free;
      FResult.Free;
      StopAction := True;
    end;
  end;

  if (StopAction = False) then
    Synchronize(ExecInMainThread)  // 进入应用层
  else
    FConnection.FSendThread.ServerReturn;

end;

procedure TPostThread.SetMsgPack(AMsgPack: TClientParams);
begin
  // 设置当前消息（调用前已加锁）
  FLock.Acquire;
  try
    FResultEx := nil;
    FMsgPack := AMsgPack; // 当前发送消息包
    if Assigned(FMsgPack) then
      FOwner := TBaseClientObject(FMsgPack.Owner)  // 当前消息所有者
    else
      FOwner := nil;
  finally
    FLock.Release;
  end;
end;

// ================== 接收线程 ==================

// 使用 WSARecv 回调函数，效率高
procedure WorkerRoutine(const dwError, cbTransferred: DWORD;
  const lpOverlapped: POverlapped; const dwFlags: DWORD); stdcall;
var
  Thread: TRecvThread;
  Connection: TInConnection;
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
        ByteCount, Flags, LPWSAOVERLAPPED(lpOverlapped), @WorkerRoutine) = SOCKET_ERROR) then
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

constructor TRecvThread.Create(AConnection: TInConnection);
{ var
  i: Integer; }
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FConnection := AConnection;

  // 分配接收缓存
  GetMem(FRecvBuf.buf, IO_BUFFER_SIZE_2);
  FRecvBuf.len := IO_BUFFER_SIZE_2;

  // 消息接收器，参数传 TResultParams
  FReceiver := TClientReceiver.Create(TResultParams);

  FReceiver.OnError := OnError;  // 校验异常事件
  FReceiver.OnPost := FConnection.FPostThread.Add; // 投放方法
  FReceiver.OnReceive := OnDataReceive; // 接收进程

{  FDebug.LoadFromFile('recv\pn2.txt');
  FStream.LoadFromFile('recv\recv2.dat');

  for i := 0 to FDebug.Count - 1 do
  begin
    FOverlapped.InternalHigh := StrToInt(FDebug[i]);
    if FOverlapped.InternalHigh = 93 then
      FStream.Read(FRecvBuf.buf^, FOverlapped.InternalHigh)
    else
      FStream.Read(FRecvBuf.buf^, FOverlapped.InternalHigh);
    HandleDataPacket;
  end;

  ExtrMsg.SaveToFile('msg.txt');    }

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

//  FDebug.Add(IntToStr(FOverlapped.InternalHigh));
//  FStream.Write(FRecvBuf.buf^, FOverlapped.InternalHigh);

  if FReceiver.Complete then  // 1. 首包数据
  begin
    // 1.1 服务器同时开启 HTTP 服务时，可能反馈拒绝服务信息（HTTP协议）
    if MatchSocketType(FRecvBuf.buf, HTTP_VER) then
    begin
      TResultParams(FReceiver.Owner).FActResult := arRefuse;
      FConnection.DoServerError(TResultParams(FReceiver.Owner));
      Exit;
    end;

    // 1.2 C/S 模式数据
    if (FOverlapped.InternalHigh < IOCP_SOCKET_SIZE) or  // 长度太短
      (MatchSocketType(FRecvBuf.buf, IOCP_SOCKET_FLAG) = False) then // C/S 标志错误
    begin
      TResultParams(FReceiver.Owner).FActResult := arErrInit;  // 初始化异常
      FConnection.DoServerError(TResultParams(FReceiver.Owner));
      Exit;
    end;

    if (FReceiver.Owner.ActResult <> arAccept) then
      FReceiver.Prepare(FRecvBuf.buf, FOverlapped.InternalHigh)  // 准备接收
    else begin
      // 上次允许接收附件，再次收到服务器接收完毕的反馈
      TResultParams(FReceiver.Owner).FActResult := arOK; // 投放时改为 arAccept, 修改
      FReceiver.PostMessage;  // 正式投放
    end;

  end else
  begin
    // 2. 后续数据
    FReceiver.Receive(FRecvBuf.buf, FOverlapped.InternalHigh);
  end;

end;

procedure TRecvThread.OnError(Result: TReceivePack);
begin
  // 校验异常
  TResultParams(Result).FActResult := arErrHashEx;
end;

procedure TRecvThread.OnDataReceive(Result: TReceivePack; DataType: TMessageDataType;
   ReceiveCount: TFileSize; AttachFinished: Boolean);
begin
  // 显示接收进程
  // 主体是接收完毕才调用，只一次
  case DataType of
    mdtHead,
    mdtEntity: begin
      FRecvMsg := Result;
      FMsgId := FRecvMsg.MsgId;
      FTotalSize := FRecvMsg.GetMsgSize(True);
      FCurrentSize := ReceiveCount;

      // 切换到主线程, 执行一次
      if (FRecvMsg.AttachSize = 0) then
        Synchronize(FConnection.ReceiveProgress);
    end;

    mdtAttachment: begin
      // 已经全部下载完毕，断点传输时，
      // 因为带消息描述，接收显示的数据长度未必 = 附件长度
      if (FRecvMsg.Action in FILE_CHUNK_ACTIONS) then
      begin
        FCurrentSize := FRecvMsg.Offset + ReceiveCount;
      end else
      begin
        if (ReceiveCount = 0) then
          FCurrentSize := FTotalSize
        else
          FCurrentSize := ReceiveCount;
      end;

      // 发送者要继续等待、切换到主线程
      FConnection.FSendThread.KeepWaiting;
      Synchronize(FConnection.ReceiveProgress);
    end;
  end;
end;

procedure TRecvThread.Stop;
begin
  inherited;
  Sleep(10);
end;

initialization
//  ExtrMsg := TStringList.Create;
//  FDebug := TStringList.Create;
//  FStream := TMemoryStream.Create;



finalization
//  FDebug.SaveToFile('msid.txt');
//  FStream.SaveToFile('recv2.dat');

// ExtrMsg.Free;
//  FStream.Free;
//  FDebug.Free;

end.

