(*
 * iocp c/s 协议客户端组件类
 *)
unit iocp_clients;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.ExtCtrls,
  System.Variants, Data.DB, Datasnap.DSIntf, Datasnap.DBClient, VCL.Forms, {$ELSE}
  Windows, Classes, SysUtils, ExtCtrls, Variants, DB, DSIntf, DBClient, Forms, {$ENDIF}
  iocp_zlib, iocp_Winsock2, iocp_base, iocp_utils, iocp_lists, iocp_senders,
  iocp_receivers, iocp_clientBase, iocp_baseObjs, iocp_msgPacks;
//  MidasLib;    // 使用时请加单元引用 MidasLib！

type

  // ============= IOCP 客户端 类 =============

  TSendThread = class;
  TRecvThread = class;
  TPostThread = class;

  TClientParams = class;
  TResultParams = class;
  TInBaseClient = class;

  // ============ C/S 协议客户端连接 ============

  // 加入任务事件
  TAddWorkEvent = procedure(Sender: TObject; Msg: TClientParams) of object;

  // 被动接收事件
  TPassvieEvent = procedure(Sender: TObject; Msg: TResultParams) of object;

  // 结果返回事件
  TReturnEvent  = procedure(Sender: TObject; Result: TResultParams) of object;

  TInConnection = class(TBaseConnection)
  private
    FActResult: TActionResult; // 服务器反馈结果
    FCancelCount: Integer;     // 取消任务数
    FMaxChunkSize: Integer;    // 续传的每次最大传输长度

    FErrorMsg: String;         // 服务端异常信息
    FUserGroup: string;        // 用户分组（子组件用）
    FUserName: string;         // 登录的用户名（子组件用）

    FReuseSessionId: Boolean;  // 凭证重用（短连接时,下次免登录）
    FRole: TClientRole;        // 权限
    FSessionId: Cardinal;      // 凭证/对话期 ID

    // ======================

    FOnAddWork: TAddWorkEvent;       // 加入任务事件
    FOnReceiveMsg: TPassvieEvent;    // 被动接收消息事件
    FOnReturnResult: TReturnEvent;   // 处理返回值事件
  private
    function GetLogined: Boolean;
    procedure ShowServerError(Msg: TResultParams);
    procedure HandleMsgHead(Result: TResultParams);
    procedure HandleFeedback(Result: TResultParams);
    procedure HandlePushedMsg(Msg: TResultParams);
    procedure SetMaxChunkSize(Value: Integer);
  protected
    procedure DoServerError; override;
    procedure InterBeforeConnect; override;
    procedure InterAfterConnect; override;  // 初始化资源
    procedure InterAfterDisconnect; override;  // 释放资源
  public
    constructor Create(AOwner: TComponent); override;
    procedure CancelAllWorks;                 // 取消全部任务
    procedure CancelWork(MsgId: TIOCPMsgId);  // 取消任务
  public
    property ActResult: TActionResult read FActResult;
    property CancelCount: Integer read FCancelCount;
    property Logined: Boolean read GetLogined;
    property SessionId: Cardinal read FSessionId;
    property UserGroup: string read FUserGroup;
    property UserName: string read FUserName;
  published
    property MaxChunkSize: Integer read FMaxChunkSize write SetMaxChunkSize default MAX_CHUNK_SIZE;
    property ReuseSessionId: Boolean read FReuseSessionId write FReuseSessionId default False;
  published
    property OnAddWork: TAddWorkEvent read FOnAddWork write FOnAddWork;
    property OnReceiveMsg: TPassvieEvent read FOnReceiveMsg write FOnReceiveMsg; // 接收被动消息/推送消息事件
    property OnReturnResult: TReturnEvent read FOnReturnResult write FOnReturnResult;
  end;

  // ============ 客户端收到的数据包/变量表 ============

  TResultParams = class(TReceivePack)
  protected
    procedure CreateAttachment(const ALocalPath: string); override;
  public
    property PeerIPPort;
  end;

  // ============ TInBaseClient 的消息包 ============

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
    // 客户端常用其他属性（读写）
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

  TMessagePack = class(TClientParams)
  private
    procedure InternalPost(AAction: TActionType);
    procedure InitMessage(AConnection: TInConnection);
  public
    constructor Create(AOwner: TInConnection); overload;
    constructor Create(AOwner: TInBaseClient); overload;
    procedure Post(AAction: TActionType);  // 增加 Post 方法
  end;

  // ============ 客户端组件 基类 ============

  // 列举文件事件
  TListFileEvent = procedure(Sender: TObject; ActResult: TActionResult;
                             No: Integer; Result: TCustomPack) of object;

  TInBaseClient = class(TComponent)
  private
    FConnection: TInConnection;    // 客户端连接
    FFileList: TStrings;           // 查询文件的列表
    FParams: TClientParams;        // 待发送消息包（不要直接使用）
    FOnListFiles: TListFileEvent;  // 列离线消息文件
    FOnReturnResult: TReturnEvent; // 处理返回值事件
    function CheckState(CheckLogIn: Boolean = True): Boolean;
    function GetParams: TClientParams;
    procedure InternalPost(Action: TActionType = atUnknown);
    procedure ListReturnFiles(Result: TResultParams);
    procedure SetConnection(const Value: TInConnection);
  protected
    procedure HandleFeedback(Result: TResultParams); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  protected
    property Connection: TInConnection read FConnection write SetConnection;
    property Params: TClientParams read GetParams;
    property OnListFiles: TListFileEvent read FOnListFiles write FOnListFiles;
  public
    destructor Destroy; override;
  published
    property OnReturnResult: TReturnEvent read FOnReturnResult write FOnReturnResult;
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
    FGroup: string;       // 分组
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
    procedure Register(const AGroup, AUserName, APassword: string; Role: TClientRole = crClient);
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
    property OnListFiles;
  end;

  // ============ 文件传输客户端 ============

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
    property OnListFiles;
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
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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

  TAfterLoadData = procedure(DataSet: TClientDataSet; const TableName: String) of object;

  TInDBQueryClient = class(TDBBaseClientObject)
  private
    FClientDataSet: TClientDataSet;  // 关联数据集
    FSubClientDataSets: TList;       // 数据子表
    FTableNames: TStrings;           // 要更新的远程表名
    FReadOnly: Boolean;              // 是否只读
    FAfterLoadData: TAfterLoadData;  // 装载数据后事件
    procedure LoadFromAttachment(Result: TResultParams);
    procedure LoadFromField(Result: TBasePack; Action: TActionType);
  protected
    procedure HandleFeedback(Result: TResultParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddClientDataSet(AClientDataSet: TClientDataSet);
    procedure ApplyUpdates;
    procedure ClearClientDataSets;
    procedure ExecQuery(Action: TActionType = atDBExecQuery);
    procedure LoadFromFile(const FileName: String);
  public
    property ReadOnly: Boolean read FReadOnly;
  published
    property AfterLoadData: TAfterLoadData read FAfterLoadData write FAfterLoadData;  
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

  TSendThread = class(TBaseSendThread)
  private
    FMsgPack: TClientParams;   // 当前发送消息包
  protected
    function ChunkRequest(Msg: TBasePackObject): Boolean; override;
    procedure InterSendMsg(RecvThread: TBaseRecvThread); override;
  public
    procedure AddWork(Msg: TBasePackObject); override;
    procedure ServerFeedback(Accept: Boolean); override;
  end;

  // =================== 投放结果的线程 类 ===================
  // 保存接收到的消息到列表，逐一塞进应用层

  TPostThread = class(TBasePostThread)
  private
    FMsg: TResultParams;       // 从列表取的首消息
    FMsgEx: TResultParams;     // 等待附件发送结果的消息
    procedure DoInMainThread;
  protected
    procedure HandleMessage(Msg: TBasePackObject); override;
  public
    procedure Add(Msg: TBasePackObject); override;  
  end;

  // =================== 接收线程 类 ===================

  TRecvThread = class(TBaseRecvThread)
  protected
    procedure HandleDataPacket; override; // 处理收到的数据包
    procedure OnDataReceive(Msg: TBasePackObject; Part: TMessagePart; RecvCount: Cardinal); override;
  public
    constructor Create(AConnection: TInConnection);
    procedure Reset;
    procedure SetLocalPath(const Path: string);    
  end;

implementation

uses
  http_base, iocp_api, iocp_wsExt;

{ TInConnection }

procedure TInConnection.CancelAllWorks;
begin
  // 取消全部任务
  if Assigned(FSendThread) then
  begin
    FSendThread.ClearAllWorks(FCancelCount);
    FSendThread.Activate;
    if Assigned(OnError) then
      OnError(Self, '取消 ' + IntToStr(FCancelCount) + ' 个任务.');
  end;
end;

procedure TInConnection.CancelWork(MsgId: TIOCPMsgId);
begin
  // 取消指定消息号的任务
  if Assigned(FSendThread) and (MsgId > 0) then
  begin
    FSendThread.CancelWork(MsgId);
    if Assigned(OnError) then
      OnError(Self, '取消任务，消息标志: ' + IntToStr(MsgId));
  end;
end;

constructor TInConnection.Create(AOwner: TComponent);
begin
  inherited;
  FMaxChunkSize := MAX_CHUNK_SIZE;
  FReuseSessionId := False;
end;

procedure TInConnection.DoServerError;
begin
  // 收到异常数据，或反馈异常
  try
    if Assigned(OnError) then
      case FActResult of
        arOutDate:
          OnError(Self, '服务器：凭证/认证过期.');
        arDeleted:
          OnError(Self, '服务器：当前用户被管理员删除，断开连接.');
        arRefuse:
          OnError(Self, '服务器：拒绝服务，断开连接.');
        arTimeOut:
          OnError(Self, '服务器：超时退出，断开连接.');
        arErrAnalyse:
          OnError(Self, '服务器：解析变量异常.');
        arErrBusy:
          OnError(Self, '服务器：系统繁忙，放弃任务.');
        arErrHash:
          OnError(Self, '服务器：校验异常.');
        arErrHashEx:
          OnError(Self, '客户端：校验异常.');
        arErrInit:  // 收到异常数据
          OnError(Self, '客户端：接收初始化异常，断开连接.');
        arErrPush:
          OnError(Self, '服务器：推送消息异常.');
        arErrUser:  // 不传递 SessionId 的反馈
          OnError(Self, '服务器：未登录或非法用户.');
        arErrWork:  // 服务端执行任务异常
          OnError(Self, '服务器：' + FErrorMsg);
      end;
  finally
    if (FActResult in [arDeleted, arRefuse, arTimeOut, arErrInit]) then
      FTimer.Enabled := True;  // 自动断开
  end;
end;

function TInConnection.GetLogined: Boolean;
begin
  Result := FActive and (FSessionId > 0);
end;

procedure TInConnection.InterAfterConnect;
begin
  // 已经连接成功

  // 发送数据线程（在前）
  FSendThread := TSendThread.Create(Self, True);
  FSendThread.Resume;

  // 提交消息线程
  FPostThread := TPostThread.Create(Self);
  FPostThread.Resume;

  // 接收数据线程（在后）
  FRecvThread := TRecvThread.Create(Self);
  FRecvThread.Resume;
end;

procedure TInConnection.InterAfterDisconnect;
begin
  // 父类自动释放各线程
  // 短连接：保留凭证，下次免登录
  if not FReuseSessionId then
  begin
    FSessionId := 0;
    FRole := crUnknown;
  end;
end;

procedure TInConnection.InterBeforeConnect;
begin
  FInitFlag := IOCP_SOCKET_FLAG; // 客户端标志
end;

procedure TInConnection.HandleFeedback(Result: TResultParams);
begin
  // 处理服务器返回的消息
  HandleMsgHead(Result);
  if Assigned(FOnReturnResult) then
    FOnReturnResult(Self, Result);
end;

procedure TInConnection.HandleMsgHead(Result: TResultParams);
begin
  // 处理登录、登出结果
  case Result.Action of
    atUserLogin: begin  // SessionId > 0 即成功
      FSessionId := Result.SessionId;
      FRole := Result.Role;
    end;
    atUserLogout:  // 登出，处理 FSessionId
      InterAfterDisconnect;
  end;
end;

procedure TInConnection.HandlePushedMsg(Msg: TResultParams);
begin
  // 接到推送消息（被动收到其他客户端消息）
  if Assigned(FOnReceiveMsg) then
    FOnReceiveMsg(Self, Msg);
end;

procedure TInConnection.SetMaxChunkSize(Value: Integer);
begin
  if (Value >= MAX_CHUNK_SIZE div 4) and (Value <= MAX_CHUNK_SIZE * 2) then
    FMaxChunkSize := Value
  else
    FMaxChunkSize := MAX_CHUNK_SIZE;
end;

procedure TInConnection.ShowServerError(Msg: TResultParams);
begin
  FActResult := Msg.ActResult;
  FErrorMsg := Msg.Msg;
  DoServerError;
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
      Msg.LoadFromFile(InfFileName);

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
  InfFileName := FConnection.LocalPath + GetFileName + '.download';
  Msg := TCustomPack.Create;

  try
    Msg.LoadFromFile(InfFileName);

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
    Msg.LoadFromFile(InfFileName);

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

  if (FAction = atFileUpChunk) then
    MsgFileName := FAttachFileName + '.upload'
  else
  if (LocalPath <> '') then  // 资源文件的本地路径
    MsgFileName := AddBackslash(LocalPath) + FileName + '.download'
  else
    MsgFileName := AddBackslash(FConnection.LocalPath) + FileName + '.download';

  if FileExists(MsgFileName) then
  begin
    Msg := TCustomPack.Create;
    try
      Msg.LoadFromFile(MsgFileName);
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

constructor TMessagePack.Create(AOwner: TInConnection);
begin
  if (AOwner = nil) then  // 不能为 nil
    raise Exception.Create('Owner 不能为空.');
  inherited Create(AOwner);
  InitMessage(TInConnection(AOwner));
end;

constructor TMessagePack.Create(AOwner: TInBaseClient);
begin
  if (AOwner = nil) then  // 不能为 nil
    raise Exception.Create('Owner 不能为空.');
  inherited Create(AOwner);
  if (AOwner is TDBBaseClientObject) then
    TDBBaseClientObject(AOwner).UpdateInConnection;
  InitMessage(TInBaseClient(AOwner).FConnection);
end;

procedure TMessagePack.InitMessage(AConnection: TInConnection);
begin
  FConnection := AConnection;
  if not FConnection.Active and FConnection.AutoConnected then
    FConnection.Active := True;
  SetUserGroup(FConnection.FUserGroup); // 默认加入分组
  SetUserName(FConnection.FUserName);   // 默认加入用户名
end;

procedure TMessagePack.InternalPost(AAction: TActionType);
var
  sErrMsg: string;
begin
  // 提交消息
  if Assigned(FConnection.FSendThread) then
  begin
    FAction := AAction; // 操作
    if (FAction in [atTextPush, atTextBroadcast]) and (Size > BROADCAST_MAX_SIZE) then
      sErrMsg := '推送的消息太长.'
    else
    if Error then
      sErrMsg := '设置变量异常.'
    else  // 加消息到发送线程
      FConnection.FSendThread.AddWork(Self);
  end else
    sErrMsg := '未连接到服务器.';

  if (sErrMsg <> '') then
  try
    if Assigned(FConnection.OnError) then
      FConnection.OnError(Self, sErrMsg)
    else
      raise Exception.Create(sErrMsg);
  finally
    Free;
  end;
end;

procedure TMessagePack.Post(AAction: TActionType);
begin
  if (AAction = atUserLogin) then  // 登录，更新连接的用户信息
  begin
    FConnection.FUserGroup := GetUserGroup;
    FConnection.FUserName := GetUserName;
  end;
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
    if FConnection.AutoConnected then
      FConnection.Active := True;
    if not FConnection.Active then
      Error := '错误：连接服务器失败.';
  end else
  if CheckLogIn and not FConnection.Logined then
    Error := '错误：客户端未登录.';

  if (Error = '') then
    Result := not CheckLogIn or FConnection.Logined
  else begin
    Result := False;
    if Assigned(FParams) then
      FreeAndNil(FParams);
    if Assigned(FConnection.OnError) then
      FConnection.OnError(Self, Error)
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
  if Assigned(FConnection) and
    (FParams.FConnection <> FConnection) then
  begin
    FParams.FConnection := FConnection;
    FParams.FSessionId := FConnection.FSessionId;
    FParams.UserGroup := FConnection.FUserGroup; // 默认加入分组
    FParams.UserName := FConnection.FUserName;   // 默认加入用户名
  end;
  Result := FParams;
end;

procedure TInBaseClient.HandleFeedback(Result: TResultParams);
begin
  // 处理服务器返回的消息
  if Assigned(FOnReturnResult) then
    FOnReturnResult(Self, Result);
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
    try  // 列出文件、一个文件一条记录
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
      if Assigned(FConnection.OnError) then
        FConnection.OnError(Self, 'TInBaseClient.ListReturnFiles读数据流异常.');
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
    Result := FConnection.FActive and (FConnection.FSessionId > 0)
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
        FOnCertify(Self, atUserLogin, FConnection.Logined);
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
      if Assigned(FConnection.OnError) then
        FConnection.OnError(Self, 'TInCertifyClient.InterListClients, ' + E.Message);
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
    Params.UserGroup := FGroup;
    FParams.UserName := FUserName;
    FParams.Password := FPassword;
    FParams.ReuseSessionId := FConnection.ReuseSessionId;

    // 更新连接的用户信息
    FConnection.FUserGroup := FGroup;
    FConnection.FUserName := FUserName;  

    InternalPost(atUserLogin);
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

procedure TInCertifyClient.Register(const AGroup, AUserName, APassword: string; Role: TClientRole);
begin
  // 注册用户（管理员）
  if CheckState() and (FConnection.FRole >= crAdmin) and (FConnection.FRole >= Role) then
  begin
    Params.ToUser := AUserName;  // 2.0 用 ToUser
    FParams.UserGroup := AGroup;
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

procedure TDBBaseClientObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FDBConnection) and (Operation = opRemove) then
    FDBConnection := nil;  // 关联的 TInDBConnection 组件被删除
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

procedure TInDBQueryClient.AddClientDataSet(AClientDataSet: TClientDataSet);
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

procedure TInDBQueryClient.ExecQuery(Action: TActionType = atDBExecQuery);
begin
  // SQL 赋值时已经判断 Action 类型，见：THeaderPack.SetSQL
  if (Action in [atDBExecQuery, atDBExecStoredProc]) then
  begin
    UpdateInConnection;  // 更新 FConnection
    if CheckState() and Assigned(FParams) then
    begin
      FParams.FTarget := FDBConnection.FConnectionIndex;  // 对应的数模编号
      if (Action <> atDBExecStoredProc) then
        InternalPost(Action)
      else
        InternalPost(FParams.Action);
    end;
  end;
end;

procedure TInDBQueryClient.HandleFeedback(Result: TResultParams);
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
          if Assigned(FClientDataSet) then
            if Assigned(Result.Attachment) then
              LoadFromAttachment(Result)
            else
            if (Result.VarCount > 0) and
              (Integer(Result.VarCount) = Result.AsInteger['__VarCount']) then
              LoadFromField(Result, Result.Action);
        atDBApplyUpdates:    // 3. 更新
          MergeChangeDataSets;  // 合并本地的更新内容
      end;
  finally
    inherited HandleFeedback(Result);
  end;
end;

procedure TInDBQueryClient.LoadFromAttachment(Result: TResultParams);
var
  MsgPack: TBasePack;
  FileName: String;
begin
  // 读入附件流的数据集信息

  // 先关闭附件流
  FileName := Result.Attachment.FileName;
  Result.Attachment.Close;

  // 读入附件流
  MsgPack := TBasePack.Create;
  
  try
    MsgPack.LoadFromFile(FileName);
    LoadFromField(MsgPack, Result.Action);
  finally
    MsgPack.Free;
  end;
end;

procedure TInDBQueryClient.LoadFromFile(const FileName: String);
var
  Data: TResultParams;
begin
  // 从文件读入数据
  Data := TResultParams.Create;
  try
    Data.LoadFromFile(FileName);
    LoadFromField(Data, Data.Action);
  finally
    Data.Free;
  end;
end;

procedure TInDBQueryClient.LoadFromField(Result: TBasePack; Action: TActionType);
var
  i, k: Integer;
  XDataSet: TClientDataSet;
  DataField: TVarField;
  NoTableName: Boolean;
begin
  // 装载查询结果

  // Result 可能包含多个数据集
  FTableNames.Clear;
    
  k := -1;
  FReadOnly := (Action = atDBExecStoredProc); // 是否只读

  for i := 0 to Result.Count - 1 do // 包含 AsInteger['__Variable_Count']
  begin
    DataField := Result.Fields[i];  // 取字段 1,2,3
    if (DataField.VarType = etVariant) then
    begin
      Inc(k);
      if (k = 0) then  // 主数据表
        XDataSet := FClientDataSet
      else
        XDataSet := FSubClientDataSets[k - 1];

      XDataSet.DisableControls;
      try
        FTableNames.Add(DataField.Name);  // 保存数据表名称
        XDataSet.Data := DataField.AsVariant;  // 数据赋值
        NoTableName := (Pos('__@DATASET', DataField.Name) = 1);
        if NoTableName then  // 不能更新的
          XDataSet.ReadOnly := True
        else
          XDataSet.ReadOnly := FReadOnly;  // 是否只读
      finally
        XDataSet.EnableControls;
      end;

      // 执行装载后事件
      if Assigned(FAfterLoadData) then
        if NoTableName then
          FAfterLoadData(XDataSet, '')
        else
          FAfterLoadData(XDataSet, DataField.Name);
    end;
  end;
end;

procedure TInDBQueryClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FClientDataSet) and (Operation = opRemove) then
    FClientDataSet := nil;  // 关联的 FClientDataSet 组件被删除
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

{ TSendThread }

procedure TSendThread.AddWork(Msg: TBasePackObject);
var
  CSMsg: TClientParams;
begin
  // 加消息到任务列表

  // Msg 是动态生成，不会重复投放
  CSMsg := TClientParams(Msg);

  if (CSMsg.FAction in FILE_CHUNK_ACTIONS) then  // 续传，修改 MsgId
    CSMsg.ModifyMessageId;

  if Assigned(TInConnection(FConnection).FOnAddWork) then
    TInConnection(FConnection).FOnAddWork(Self, CSMsg);

  inherited;
end;

function TSendThread.ChunkRequest(Msg: TBasePackObject): Boolean;
begin
  Result := (TClientParams(Msg).Action in [atFileDownChunk, atFileUpChunk]);
end;

procedure TSendThread.InterSendMsg(RecvThread: TBaseRecvThread);
  procedure SendMsgHeader;
  begin
    IniWaitState;  // 准备等待

    // 保存消息总长度
    FTotalSize := FMsgPack.GetMsgSize;

    // 断点续传时要加位移
    if (FMsgPack.FAction in FILE_CHUNK_ACTIONS) then
      FSendCount := FMsgPack.FOffset
    else
      FSendCount := 0;

    // 发送协议头+校验码+文件描述
    FMsgPack.LoadHead(FSender.Data);

    FSender.MsgPart := mdtHead;  // 消息头
    FSender.SendBuffers;
  end;
  procedure SendMsgEntity;
  begin
    FSender.MsgPart := mdtEntity;  // 消息实体
    FSender.Send(FMsgPack.FMain, FMsgPack.FDataSize, False);  // 不释放资源
  end;
  procedure SendMsgAttachment;
  begin
    // 发送附件数据(不关闭资源)
    IniWaitState;  // 准备等待
    FSender.MsgPart := mdtAttachment;  // 消息附件
    if (FMsgPack.FAction = atFileUpChunk) then  // 断点续传
      FSender.Send(FMsgPack.FAttachment, FMsgPack.FAttachSize,
                   FMsgPack.FOffset, FMsgPack.FOffsetEnd, False)
    else
      FSender.Send(FMsgPack.FAttachment, FMsgPack.FAttachSize, False);
  end;
begin
  // 执行发送任务, 与服务端方法类似
  //   见：TReturnResult.ReturnResult、TDataReceiver.Prepare

  FMsgPack := TClientParams(FSendMsg);
  FMsgPack.FSessionId := TInConnection(FConnection).FSessionId; // 登录凭证
  FSender.Owner := FMsgPack;  // 宿主

  // 1. 本地路径
  if (FMsgPack.LocalPath <> '') then
    TRecvThread(RecvThread).SetLocalPath(AddBackslash(FMsgPack.LocalPath))
  else
    TRecvThread(RecvThread).SetLocalPath(AddBackslash(FConnection.LocalPath));

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
end;

procedure TSendThread.ServerFeedback(Accept: Boolean);
begin
  // 因为是多线程，要先赋值 arAccept，后发信号，
  // 否则，可能发送线程 FMsgPack.FActResult = arUnknown
  if Accept then
    FMsgPack.FActResult := arAccept;
  inherited;
end;

{ TPostThread }

procedure TPostThread.Add(Msg: TBasePackObject);
var
  XMsg: TResultParams;
begin
  // Msg：是 TResultParams
  // 加一个消息到列表，激活线程

  // 1. 检查附件发送情况
  XMsg := TResultParams(Msg);

  if (XMsg.ActResult = arAccept) then // 服务器接受请求
  begin
    FMsgEx := XMsg;  // 先保存反馈结果
    FSendThread.ServerFeedback(True); // 唤醒 -> 发送附件
  end else
  begin
    // 2. 投放入线程队列
    // 刚连接时，未登录，可能立刻收到广播消息，
    // 此时 FSendThread.FMsgPack=nil，一样投放
    FLock.Acquire;
    try
      if Assigned(FMsgEx) and
        (FMsgEx.FMsgId = XMsg.MsgId) then // 发送附件后的反馈
      begin
        XMsg.Free;      // 释放附件上传结果（无内容）
        XMsg := FMsgEx; // 使用真正的反馈消息（有内容）
        FMsgEx.FActResult := arOK;  // 修改结果 -> 成功
        FMsgEx := nil;  // 不引用了
      end;
      // 加入列表
      FMsgList.Add(XMsg);
    finally
      FLock.Release;
    end;

    Activate;  // 激活
  end;
end;

procedure TPostThread.DoInMainThread;
const
  SERVER_PUSH_EVENTS =[arDeleted, arRefuse { 不应该存在 }, arTimeOut];
  SELF_ERROR_RESULTS =[arOutDate, arRefuse { c/s 模式发出 }, arErrBusy,
                       arErrHash, arErrHashEx, arErrAnalyse, arErrPush,
                       arErrUser, arErrWork];
  function IsPushedMessage: Boolean;
  begin
    FLock.Acquire;
    try
      Result := (FSendMsg = nil) or
                (FMsg.Owner <> FSendMsg.Owner) or
                (FSendMsg.MsgId <> FMsg.MsgId); // 推送前 MsgId 被修改 
    finally
      FLock.Release;
    end;
  end;
  function IsFeedbackMessage: Boolean;
  begin
    FLock.Acquire;
    try
      Result := (FSendMsg <> nil) and (FSendMsg.MsgId = FMsg.MsgId);
    finally
      FLock.Release;
    end;
  end;
var
  AConnection: TInConnection;
begin
  // 进入主线程，把消息提交给宿主
  // 可能处理消息过程中被用户断开，此时要改变断开模式

  AConnection := TInConnection(FConnection);
  AConnection.FInMainThread := True;  // 进入主线程

  try

    if IsPushedMessage() then

      {$IFNDEF DELPHI_7}
      {$REGION '. 推送来的消息'}
      {$ENDIF}

      try
        if (FMsg.ActResult in SERVER_PUSH_EVENTS) then
        begin
          // 1.1 服务器推送的消息
          AConnection.ShowServerError(FMsg);
        end else
        begin
          // 1.2 其他客户端推送的消息
          AConnection.HandlePushedMsg(FMsg);
        end;
      finally
        FMsg.Free;
        AConnection.FInMainThread := False;        
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
        if (AConnection.FSessionId <> FMsg.FSessionId) then
          AConnection.FSessionId := FMsg.FSessionId;
        if (FMsg.ActResult in SELF_ERROR_RESULTS) then
        begin
          // 2.1 反馈执行异常
          AConnection.ShowServerError(FMsg);
        end else
        if IsFeedbackMessage() then
        begin
          // 2.2 反馈正常结果
          if (FMsg.Owner = AConnection) then
            AConnection.HandleFeedback(FMsg)
          else
            TInBaseClient(FMsg.Owner).HandleFeedback(FMsg);
        end;
      finally
        FMsg.Free;
        if AConnection.FActive then  // 可能已关闭
          AConnection.FSendThread.ServerFeedback;  // 唤醒
        AConnection.FInMainThread := False;
      end;

      {$IFNDEF DELPHI_7}
      {$ENDREGION}
      {$ENDIF}

  except
    on E: Exception do
    begin
      AConnection.FErrorcode := GetLastError;
      AConnection.DoClientError;  // 在主线程，直接调用
    end;
  end;

end;

procedure TPostThread.HandleMessage(Msg: TBasePackObject);
var
  ReadInfDone: Boolean;
  NewMsg: TMessagePack;
begin
  // Msg：是 TResultParams
  
  // 消息预处理
  // 要检查断点续传的情况，最后提交到主线程执行

  FMsg := TResultParams(Msg);

  if (FMsg.FAction in FILE_CHUNK_ACTIONS) then
  begin
    // 继续请求
    if (FMsg.Owner = FConnection) then
      NewMsg := TMessagePack.Create(TInConnection(FMsg.Owner))
    else
      NewMsg := TMessagePack.Create(TInBaseClient(FMsg.Owner));

    NewMsg.FAction := atUnknown;  // 未知
    NewMsg.FActResult := FMsg.FActResult;  // 发送附件后的反馈结果
    NewMsg.FCheckType := FMsg.FCheckType;
    NewMsg.FState := msAutoPost;  // 自动提交
    NewMsg.FZipLevel := FMsg.FZipLevel;

    if (FMsg.FAction = atFileUpChunk) then
    begin
      // 断点上传，立刻打开本地文件，读文件信息
      //   见：TBaseMessage.LoadFromFile、TReceiveParams.CreateAttachment
      NewMsg.LoadFromFile(FMsg.GetAttachFileName, True);
      ReadInfDone := not NewMsg.Error and NewMsg.ReadUploadInf(FMsg);
    end else
    begin
      // 断点下载，设置下载文件，继续
      // FActResult 一般是正常，也可能校验异常
      //   见：TReturnResult.LoadFromFile、TResultParams.CreateAttachment
      NewMsg.FileName := FMsg.FileName;
      ReadInfDone := NewMsg.ReadDownloadInf(FMsg);
    end;

    if ReadInfDone then
      NewMsg.Post(FMsg.FAction)
    else  // 续传完毕
      NewMsg.Free;
  end;

  // 进入业务层
  Synchronize(DoInMainThread);  

end;

{ TRecvThread }

constructor TRecvThread.Create(AConnection: TInConnection);
var
  AReceiver: TClientReceiver;
begin
  AReceiver := TClientReceiver.Create;
  FRecvMsg := AReceiver.MsgPack;  // 首消息, TResultParams

  AReceiver.OnNewMsg := OnCreateMsgObject;
  AReceiver.OnPost := AConnection.PostThread.Add;
  AReceiver.OnReceive := OnDataReceive;
  AReceiver.OnError := OnRecvError;

  inherited Create(AConnection, AReceiver);
end;

procedure TRecvThread.HandleDataPacket;
var
  AConection: TInConnection;
  Msg: TResultParams;
begin
  inherited;
  // 处理接收到的数据包（在接收线程内）

  AConection := TInConnection(FConnection);
  Msg := TResultParams(FRecvMsg);

  if FReceiver.Completed then  // 1. 首包数据
  begin
    // 1.1 服务器同时开启 HTTP 服务时，可能反馈拒绝服务信息（HTTP协议）
    if MatchSocketType(FRecvBuf.buf, HTTP_VER) then
    begin
      AConection.FActResult := arRefuse;
      Synchronize(AConection.DoServerError);
      Exit;
    end;

    // 1.2 C/S 模式数据
    if (FOverlapped.InternalHigh < IOCP_SOCKET_SIZE) or  // 长度太短
      (MatchSocketType(FRecvBuf.buf, IOCP_SOCKET_FLAG) = False) then // C/S 标志错误
    begin
      AConection.FActResult := arErrInit;  // 初始化异常
      Synchronize(AConection.DoServerError);
      Exit;
    end;

    if (Msg.ActResult <> arAccept) then
      FReceiver.Prepare(FRecvBuf.buf, FOverlapped.InternalHigh)  // 准备接收
    else begin
      // 上次允许接收附件，再次收到服务器接收完毕的反馈
      Msg.FActResult := arOK; // 投放时改为 arAccept, 修改
      TClientReceiver(FReceiver).PostMessage;  // 正式投放
    end;

  end else
  begin
    // 2. 后续数据
    FReceiver.Receive(FRecvBuf.buf, FOverlapped.InternalHigh);
  end;

end;

procedure TRecvThread.OnDataReceive(Msg: TBasePackObject; Part: TMessagePart; RecvCount: Cardinal);
var
  XMsg: TResultParams;
  ShowProg: Boolean;
begin
  // 显示接收进程
  // 主体是接收完毕才调用，只一次

  // Msg 就是 FRecvMsg
  XMsg := TResultParams(Msg);
  ShowProg := False;  

  case Part of
    mdtHead,
    mdtEntity: begin  // 只调用一次
      FTotalSize := XMsg.GetMsgSize;
      FRecvCount := RecvCount;
      ShowProg := (XMsg.AttachSize = 0); // 切换到主线程, 执行一次
    end;
    mdtAttachment: begin
      // 已经全部下载完毕。
      // 断点传输时，因为带消息描述，接收显示的数据长度未必 = 附件长度
      if (XMsg.Action in FILE_CHUNK_ACTIONS) then
      begin
        FRecvCount := XMsg.Offset + RecvCount;
        if (RecvCount = 0) and
           (FRecvCount + TInConnection(FConnection).FMaxChunkSize >= FTotalSize) then
          FRecvCount := FTotalSize;  // 传输完成
      end else
      begin
        if (RecvCount = 0) then
          FRecvCount := FTotalSize
        else
          FRecvCount := RecvCount;
      end;
      // 发送者要继续等待
      TSendThread(FConnection.SendThread).KeepWaiting;
      ShowProg := True; // 切换到主线程
    end;
  end;

  if ShowProg then  // 显示进程
    inherited;
end;

procedure TRecvThread.Reset;
begin
  // 重置接收器环境
  if Assigned(FReceiver) then
    FReceiver.Reset;
end;

procedure TRecvThread.SetLocalPath(const Path: string);
begin
  // 设置保存文件的本地路径
  if Assigned(FReceiver) then
    TClientReceiver(FReceiver).LocalPath := Path;
end;

end.

