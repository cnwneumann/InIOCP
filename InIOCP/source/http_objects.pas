(*
 * http 服务各种对象类
 *)
unit http_objects;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes,
  System.SysUtils, System.StrUtils, Data.DB, {$ELSE}
  Windows, Classes, SysUtils, StrUtils, DB, {$ENDIF}
  iocp_Winsock2, iocp_base, iocp_senders, iocp_lists,
  iocp_objPools, iocp_msgPacks, http_base, http_utils,
  iocp_SHA1;

type

  // ================ Http Session 类 ================

  // Set-Cookie: InIOCP_SID=Value

  THttpSession = class(TObject)
  private
    FExpires: Int64;         // 生成 UTC 时间（读取速度快）
    FTimeOut: Integer;       // 超时时间（秒）
    FCount: Integer;         // 累计请求次数
  public
    function CheckAttack(const SessionId: AnsiString): Boolean;
    function CreateSessionId: AnsiString;
    function ValidateSession: Boolean;
    procedure UpdateExpires;
  public
    class function Extract(const SetCookie: AnsiString; var SessionId: AnsiString): Boolean;
  end;

  // ================ Http Session 管理 类 ================

  THttpSessionManager = class(TStringHash)
  private
    procedure CheckSessionEvent(var Data: Pointer);
  protected
    procedure FreeItemData(Item: PHashItem); override;
  public
    function CheckAttack(const SessionId: AnsiString): Boolean;
    procedure DecRef(const SessionId: AnsiString);
    procedure InvalidateSessions;
  end;

  // ================== Http 数据提供 组件类 ======================

  THttpBase         = class;         // 基类
  THttpRequest      = class;         // 请求
  THttpResponse     = class;         // 响应

  TOnAcceptEvent    = procedure(Sender: TObject; Request: THttpRequest;
                                var Accept: Boolean) of object;
  TOnInvalidSession = procedure(Sender: TObject; Request: THttpRequest;
                                Response: THttpResponse) of object;
  TOnReceiveFile    = procedure(Sender: TObject; Request: THttpRequest;
                                const FileName: String; Data: PAnsiChar;
                                DataLength: Integer; State: THttpPostState) of object;

  // 请求事件（Sender 是 Worker）
  THttpRequestEvent = procedure(Sender: TObject;
                                Request: THttpRequest;
                                Response: THttpResponse) of object;

  // 升级为 WebSocket 的事件
  TOnUpgradeEvent = procedure(Sender: TObject; const Origin: String;
                              var Accept: Boolean) of object;
                                                              
  THttpDataProvider = class(TComponent)
  private
    FSessionMgr: THttpSessionManager;     // 全部 Session 管理
    FMaxContentLength: Integer;           // 请求实体的最大长度
    FKeepAlive: Boolean;                  // 保存连接
    FPeerIPList: TPreventAttack;          // 客户端 IP 列表
    FPreventAttack: Boolean;              // IP列表（防攻击）
    procedure CheckSessionState(Request: THttpRequest; Response: THttpResponse);
    procedure SetMaxContentLength(const Value: Integer);
    procedure SetPreventAttack(const Value: Boolean);
  protected
    FServer: TObject;                     // TInIOCPServer 服务器
    FOnAccept: TOnAcceptEvent;            // 是否接受请求
    FOnDelete: THttpRequestEvent;         // 请求：Delete
    FOnGet: THttpRequestEvent;            // 请求：Get
    FOnPost: THttpRequestEvent;           // 请求：Post
    FOnPut: THttpRequestEvent;            // 请求：Put
    FOnOptions: THttpRequestEvent;        // 请求：Options
    FOnReceiveFile: TOnReceiveFile;       // 接收文件事件
    FOnTrace: THttpRequestEvent;          // 请求：Trace
    FOnUpgrade: TOnUpgradeEvent;          // 升级为 WebSocket 事件
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearIPList;
  public
    property SessionMgr: THttpSessionManager read FSessionMgr;
  published
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive default True;
    property MaxContentLength: Integer read FMaxContentLength write SetMaxContentLength default MAX_CONTENT_LENGTH;
    property PreventAttack: Boolean read FPreventAttack write SetPreventAttack default False;
  published
    property OnAccept: TOnAcceptEvent read FOnAccept write FOnAccept;
    property OnDelete: THttpRequestEvent read FOnDelete write FOnDelete;
    property OnGet: THttpRequestEvent read FOnGet write FOnGet;
    property OnPost: THttpRequestEvent read FOnPost write FOnPost;
    property OnPut: THttpRequestEvent read FOnPut write FOnPut;
    property OnOptions: THttpRequestEvent read FOnOptions write FOnOptions;
    property OnReceiveFile: TOnReceiveFile read FOnReceiveFile write FOnReceiveFile;
    property OnTrace: THttpRequestEvent read FOnTrace write FOnTrace;
  end;

  // ================ 客户端表单字段/参数 类 ================

  THttpFormParams = class(TBasePack)
  public  // 公开常用属性（只读）
    property AsBoolean[const Index: String]: Boolean read GetAsBoolean;
    property AsDateTime[const Index: String]: TDateTime read GetAsDateTime;
    property AsFloat[const Index: String]: Double read GetAsFloat;
    property AsInteger[const Index: String]: Integer read GetAsInteger;
    property AsString[const Index: String]: String read GetAsString;
  end;

  // ================ 请求报头的额外信息/参数 类 ================

  THttpHeaderParams = class(THttpFormParams)
  private
    FOwner: THttpBase;
    function GetBoundary: AnsiString;
    function GetContentLength: Integer;
    function GetContentType: THttpContentType;
    function GetUTF8Encode: Boolean;
    function GetKeepAlive: Boolean;
    function GetMultiPart: Boolean;
    function GetRange: AnsiString;
    function GetIfMath: AnsiString;
    function GetLastModified: AnsiString;
  public
    constructor Create(AOwner: THttpBase);
  public
    // 属性：报头常用
    property Boundary: AnsiString read GetBoundary;    // Content-Type: multipart/form-data; Boundary=
    property ContentLength: Integer read GetContentLength; // Content-Length 的值
    property ContentType: THttpContentType read GetContentType; // Content-Type 的值
    property UTF8Encode: Boolean read GetUTF8Encode;  // UTF-8...
    property IfMath: AnsiString read GetIfMath;  // if-math...
    property LastModified: AnsiString read GetLastModified;  // Last-Modified
    property KeepAlive: Boolean read GetKeepAlive;  // Keep-Alive...
    property MultiPart: Boolean read GetMultiPart; // Content-Type：multipart/form-data
    property Range: AnsiString read GetRange; // range
  end;

  // ================ Http 基类 ================

  THttpBase = class(TObject)
  private
    FDataProvider: THttpDataProvider;  // HTTP 支持
    FOwner: TObject;             // THttpSocket 对象
    FContentSize: Int64;         // 实体长度
    FFileName: AnsiString;       // 收到、发送到的文件名
    FKeepAlive: Boolean;         // 保持连接
    FSessionId: AnsiString;      // 对话期 ID
    function GetSocketState: Boolean;
    function GetHasSession: Boolean;
  protected
    FExtParams: THttpHeaderParams;  // 请求 Headers 的额外参数/变量表
    FStatusCode: Integer;        // 状态代码
  public
    constructor Create(ADataProvider: THttpDataProvider; AOwner: TObject);
    procedure Clear; virtual;
  public
    property HasSession: Boolean read GetHasSession;
    property KeepAlive: Boolean read FKeepAlive;    
    property SessionId: AnsiString read FSessionId;
    property SocketState: Boolean read GetSocketState;
    property Owner: TObject read FOwner;
  end;

  // ================ Http 请求 ================

  // 报头数组
  PHeadersArray = ^THeadersArray;
  THeadersArray = array[TRequestHeaderType] of AnsiString;

  THttpRequest = class(THttpBase)
  private
    FAccepted: Boolean;          // 接受请求
    FAttacked: Boolean;          // 被攻击
    FByteCount: Integer;         // 数据包长度

    FHeadersAry: THeadersArray;  // 报头数组（原始数据）
    FParams: THttpFormParams;    // 客户端表单的字段/参数/变量表
    FStream: TInMemStream;       // 变量/参数原始流

    FContentType: THttpContentType;  // 实体类型
    FContentLength: Integer;     // 实体长度
    FMethod: THttpMethod;        // 请求命令
    FRequestURI: AnsiString;     // 请求资源
    FUpgradeState: Integer;      // 升级为 WebSocket 的状态
    FVersion: AnsiString;        // 版本 http/1.1

    function GetCompleted: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function GetHeaderIndex(const Header: AnsiString): TRequestHeaderType; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function GetHeaders(Index: TRequestHeaderType): AnsiString;

    procedure ExtractElements(Data: PAnsiChar; Len: Integer; RecvFileEvent: TOnReceiveFile);
    procedure ExtractHeaders(Data: PAnsiChar; DataLength: Integer);
    procedure ExtractMethod(var Data: PAnsiChar);
    procedure ExtractParams(Data: PAnsiChar; Len: Integer; Decode: Boolean);

    procedure InitializeInStream;
    procedure URLDecodeRequestURI;
    procedure WriteHeader(Index: TRequestHeaderType; const Content: AnsiString);
  protected
    procedure Decode(Sender: TBaseTaskSender; Response: THttpResponse; Data: PPerIOData);
  public
    constructor Create(ADataProvider: THttpDataProvider; AOwner: TObject);
    destructor Destroy; override;
    procedure Clear; override;
  public
    property Accepted: Boolean read FAccepted;
    property Attacked: Boolean read FAttacked;
    property Completed: Boolean read GetCompleted;
    property Entity: TInMemStream read FStream;  // 实体内容
    property Headers[Index: TRequestHeaderType]: AnsiString read GetHeaders;
    property Method: THttpMethod read FMethod;
    property Params: THttpFormParams read FParams;
    property URI: AnsiString read FRequestURI;
    property UpgradeState: Integer read FUpgradeState;
    property StatusCode: Integer read FStatusCode;
  end;

  // ================ 响应报头 类 ================
  // 一般来说，响应报头内容不会很大（小于 IO_BUFFER_SIZE），
  // 为加快速度，响应报头的内存直接引用发送器的 TPerIOData.Data，
  // 用 Add 方法加入报头，用 Append 方法加入小页面的实体（总长 <= IO_BUFFER_SIZE）

  THeaderArray = array[TResponseHeaderType] of Boolean;

  THttpResponseHeaders = class(TObject)
  private
    FHeaders: THeaderArray;     // 已加入的报头
    FData: PWsaBuf;             // 引用发送器缓存
    FBuffer: PAnsiChar;         // 发送缓存当前位置
    FOwner: TServerTaskSender;  // 数据发送器
    function GetSize: Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure InterAdd(const Content: AnsiString; SetCRLF: Boolean = True);
    procedure SetOwner(const Value: TServerTaskSender);
  public
    procedure Clear;

    procedure Add(Code: TResponseHeaderType; const Content: AnsiString = '');
    procedure Append(const Content: AnsiString; SetCRLF: Boolean = True); overload;
    procedure Append(var AHandle: THandle; ASize: Cardinal); overload;
    procedure Append(var AStream: TStream; ASize: Cardinal); overload;
    procedure Append(AList: TInStringList; ASize: Cardinal); overload;
    procedure AddCRLF;

    procedure ChunkDone;
    procedure ChunkSize(ASize: Cardinal);
    procedure SetStatus(Code: Integer);
  public
    property Size: Integer read GetSize;
    property Owner: TServerTaskSender read FOwner write SetOwner; 
  end;

  // ================ Http 响应 ================

  THttpResponse = class(THttpBase)
  private
    FRequest: THttpRequest;    // 引用请求
    FHeaders: THttpResponseHeaders; // 服务器状态、报头
    FContent: TInStringList;   // 列表式实体内容

    FHandle: THandle;          // 要发送的文件
    FStream: TStream;          // 要发送的数据流
    FSender: TBaseTaskSender;  // 任务发送器

    FContentType: AnsiString;  // 内容类型
    FGZipStream: Boolean;      // 是否压缩流
    FLastWriteTime: Int64;     // 文件最后修改时间
    FWorkDone: Boolean;        // 发送结果完毕
    
    function GetFileETag: AnsiString; 
    function GZipCompress(Stream: TStream): TStream;

    procedure AddHeaderList(SendNow: Boolean);  
    procedure AddDataPackets;
    procedure FreeResources;
    
    procedure SendChunkHeaders(const ACharSet: AnsiString = '');
  protected
    // 正式发送数据
    procedure SendWork;
    procedure Upgrade;    
  public
    constructor Create(ADataProvider: THttpDataProvider; AOwner: TObject);
    destructor Destroy; override;

    // 清空资源
    procedure Clear; override;

    // Session：新建、删除
    procedure CreateSession;
    procedure RemoveSession;

    // 设置状态、报头
    procedure SetStatus(Code: Integer);
    procedure AddHeader(Code: TResponseHeaderType; const Content: AnsiString = '');

    // 设置实体
    procedure SetContent(const Content: AnsiString);
    procedure AddContent(const Content: AnsiString);

    // 立刻分块发送（优化，自动发送结束标志）
    procedure SendChunk(Stream: TStream);

    // 设置发送源：数据流、文件
    procedure SendStream(Stream: TStream; Compress: Boolean = False);
    procedure TransmitFile(const FileName: String; AutoView: Boolean = True);

    // 发送 JSON
    procedure SendJSON(DataSet: TDataSet; CharSet: THttpCharSet = hcsDefault); overload;
    procedure SendJSON(const JSON: AnsiString); overload;

    // 设置 Head 信息
    procedure SetHead;

    // 重定位
    procedure Redirect(const URL: AnsiString);
  public
    property ContentType: AnsiString read FContentType;
    property StatusCode: Integer read FStatusCode write FStatusCode;
  end;

implementation

uses
  iocp_log, iocp_varis, iocp_utils,
  iocp_server, iocp_managers, iocp_sockets, iocp_zlib;

type
  TBaseSocketRef = class(TBaseSocket);

{ THttpSession }

function THttpSession.CheckAttack(const SessionId: AnsiString): Boolean;
var
  TickCount: Int64;
begin
  // 10 秒内出现 10 个请求，当作攻击, 15 分钟内禁止连接
  //    用 httptest.exe 测试成功。
  TickCount := GetUTCTickCount;
  if (FExpires > TickCount) then   // 攻击过，未解禁
    Result := True
  else begin
    Result := (FCount >= 10) and (TickCount - FExpires <= 10000);
    if Result then                // 是攻击
    begin
      FExpires := TickCount + 900000;  // 900 秒
      {$IFDEF DEBUG_MODE}
      iocp_log.WriteLog('THttpSession.CheckAttack->拒绝服务，攻击对话期：' + SessionId);
      {$ENDIF}
    end else
      FExpires := TickCount;
    Inc(FCount);
  end;
end;

function THttpSession.CreateSessionId: AnsiString;
const
  // BASE_CHARS 长 62
  BASE_CHARS = AnsiString('0123456789aAbBcCdDeEfFgGhHiIjJ' +
                          'kKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ');
var
  i: Integer;
begin
  Randomize;

  Result := Copy(BASE_CHARS, 1, 32);  // 32 字节
  for i := 1 to 32 do  // 总长 32
    Result[i] := BASE_CHARS[Random(62) + 1];           // 1 <= Random < 62

  FExpires := GetUTCTickCount;  // 当前时间
  FTimeOut := 300;  // 300 秒超时
end;

class function THttpSession.Extract(const SetCookie: AnsiString; var SessionId: AnsiString): Boolean;
var
  pa, p2: PAnsiChar;
begin
  // 提取 SetCookie 的 SessionId
  pa := PAnsiChar(SetCookie);
  p2 := pa;
  if SearchInBuffer(p2, Length(SetCookie), HTTP_SESSION_ID) then
  begin
    SessionId := Copy(SetCookie, Integer(p2 - pa) + 2, 32);  // 32 字节
    Result := (SessionId <> '');
  end else
  begin
    SessionId := '';
    Result := False;
  end;
end;

procedure THttpSession.UpdateExpires;
begin
  // 延迟生命期
  FExpires := GetUTCTickCount;  // 当前时间
end;

function THttpSession.ValidateSession: Boolean;
begin
  // 检查是否有效
  Result := (GetUTCTickCount - FExpires) <= FTimeOut * 1000;
end;

{ THttpSessionManager }

procedure THttpSessionManager.CheckSessionEvent(var Data: Pointer);
var
  Session: THttpSession;
begin
  // 回调：检查 Session 是否有效
  Session := THttpSession(Data);
  if (Session.ValidateSession = False) then
    try
      Session.Free;
    finally
      Data := Nil;
    end;
end;

procedure THttpSessionManager.DecRef(const SessionId: AnsiString);
var
  Session: THttpSession;
begin
  // 减少 SessionId 引用
  if (SessionId <> '') and (SessionId <> HTTP_INVALID_SESSION) then
  begin
    Lock;
    try
      Session := ValueOf2(SessionId);
      if Assigned(Session) and (Session.FCount > 0) then
        Dec(Session.FCount);
    finally
      UnLock;
    end;
  end;
end;

function THttpSessionManager.CheckAttack(const SessionId: AnsiString): Boolean;
var
  Session: THttpSession;
begin
  // 检查是否为 SessionId 攻击（短时间大量使用）
  Lock;
  try
    Session := ValueOf2(SessionId);
    Result := Assigned(Session) and Session.CheckAttack(SessionId);
  finally
    UnLock;
  end;
end;

procedure THttpSessionManager.FreeItemData(Item: PHashItem);
begin
  // 释放 Session 对象
  try
    THttpSession(Item^.Value).Free;
  finally
    Item^.Value := Nil;
  end;
end;

procedure THttpSessionManager.InvalidateSessions;
begin
  // 检查全部 Session 的状态
  //   删除过期的 Session
  Lock;
  try
    Scan(CheckSessionEvent);
  finally
    Unlock;
  end;
end;

{ THttpDataProvider }

procedure THttpDataProvider.CheckSessionState(Request: THttpRequest; Response: THttpResponse);
var
  Session: THttpSession;
  Item: PPHashItem;
begin
  // 检查客户端 Session 在服务端的状态
  //  从 FSessionMgr 中查找客户端 SessionId 对应的 THttpSession
  FSessionMgr.Lock;
  try
    Session := FSessionMgr.ValueOf3(Request.FSessionId, Item);
    if (Assigned(Session) = False) then  // 没有
      Request.FSessionId := ''
    else
    if Session.ValidateSession then  // 有效
    begin
      Response.FSessionId := Request.FSessionId;  // 方便操作界面判断
      Session.UpdateExpires;  // 生命期延后
    end else
    begin
      Request.FSessionId := '';
      FSessionMgr.Remove2(Item);
    end;
  finally
    FSessionMgr.UnLock;
  end;
end;

procedure THttpDataProvider.ClearIPList;
begin
  // 清除 IP 列表内容
  if Assigned(FPeerIPList) then
    TPreventAttack(FPeerIPList).Clear;
end;

constructor THttpDataProvider.Create(AOwner: TComponent);
begin
  inherited;
  FKeepAlive := True; // 默认值
  FMaxContentLength := MAX_CONTENT_LENGTH;
  FPreventAttack := False;
  FSessionMgr := THttpSessionManager.Create(512);
end;

destructor THttpDataProvider.Destroy;
begin
  if Assigned(FPeerIPList) then
    FPeerIPList.Free;
  FSessionMgr.Free;
  inherited;
end;

procedure THttpDataProvider.SetMaxContentLength(const Value: Integer);
begin
  if (Value <= 0) then
    FMaxContentLength := MAX_CONTENT_LENGTH
  else
    FMaxContentLength := Value;
end;

procedure THttpDataProvider.SetPreventAttack(const Value: Boolean);
begin
  FPreventAttack := Value;
  if not (csDesigning in ComponentState) then
    if FPreventAttack then
      FPeerIPList := TPreventAttack.Create
    else
    if Assigned(FPeerIPList) then
      FPeerIPList.Free;
end;

{ THttpHeaderParams }

constructor THttpHeaderParams.Create(AOwner: THttpBase);
begin
  inherited Create;    
  FOwner := AOwner;
end;

function THttpHeaderParams.GetBoundary: AnsiString;
begin
  Result := inherited AsString['BOUNDARY'];
end;

function THttpHeaderParams.GetContentLength: Integer;
begin
  Result := inherited AsInteger['CONTENT-LENGTH'];
end;

function THttpHeaderParams.GetContentType: THttpContentType;
begin
  Result := THttpContentType(inherited AsInteger['CONTENT-TYPE']);
end;

function THttpHeaderParams.GetUTF8Encode: Boolean;
begin
  Result := inherited AsBoolean['UTF8-ENCODE'];
end;

function THttpHeaderParams.GetIfMath: AnsiString;
begin
  Result := inherited AsString['IF-MATH'];
end;

function THttpHeaderParams.GetKeepAlive: Boolean;
begin
  Result := inherited AsBoolean['KEEP-ALIVE'];
end;

function THttpHeaderParams.GetLastModified: AnsiString;
begin
  Result := inherited AsString['LAST-MODIFIED'];
end;

function THttpHeaderParams.GetMultiPart: Boolean;
begin
  Result := inherited AsBoolean['MULTIPART'];
end;

function THttpHeaderParams.GetRange: AnsiString;
begin
  Result := inherited AsString['RANGE'];
end;

{ THttpBase }

procedure THttpBase.Clear;
begin
  // 清空资源：变量，数值
  FContentSize := 0;            
  FFileName := '';
  FSessionId := '';
  FStatusCode := 200;      // 默认       
end;

constructor THttpBase.Create(ADataProvider: THttpDataProvider; AOwner: TObject);
begin
  inherited Create;
  FDataProvider := ADataProvider;  // HTTP 支持
  FOwner := AOwner;  // THttpSocket 对象
end;

function THttpBase.GetSocketState: Boolean;
begin
  Result := THttpSocket(FOwner).SocketState;
end;

function THttpBase.GetHasSession: Boolean;
begin
  Result := (FSessionId <> '');  // 合法时才不为空
end;

{ THttpRequest }

procedure THttpRequest.Clear;
var
  i: TRequestHeaderType;
begin
  inherited;
  FAttacked := False;
  FContentLength := 0; // 下次要比较
  FContentType := hctUnknown;
  
  for i := rqhHost to High(FHeadersAry) do
    Delete(FHeadersAry[i], 1, Length(FHeadersAry[i]));
  Delete(FRequestURI, 1, Length(FRequestURI)); // 无
  Delete(FVersion, 1, Length(FVersion));  // 版本

  FParams.Clear;
  FExtParams.Clear;
  FStream.Clear;
end;

constructor THttpRequest.Create(ADataProvider: THttpDataProvider; AOwner: TObject);
begin
  inherited;
  FExtParams := THttpHeaderParams.Create(Self);
  FParams := THttpFormParams.Create;
  FStream := TInMemStream.Create;
end;

procedure THttpRequest.Decode(Sender: TBaseTaskSender; Response: THttpResponse; Data: PPerIOData);
var
  Buf: PAnsiChar;
begin
  // 解码：分析、提取请求信息
  //  1. 任务完成 -> 开始新任务
  //  2. 任务未接收完成，继续接收     

  Response.FRequest := Self;     // 引用
  Response.FExtParams := FExtParams; // 引用参数表
  Response.FSender := Sender;    // 引用数据发送器

  // 设置报头缓存，把报头对象和发送器关联起来
  Response.FHeaders.Owner := TServerTaskSender(Sender);

  Buf := Data^.Data.buf;  // 开始地址
  FByteCount := Data^.Overlapped.InternalHigh;  // 数据包长度

  if Completed then   // 新的请求
  begin
    // 提取命令行信息（可能带参数）
    ExtractMethod(Buf);

    if (FMethod > hmUnknown) and (FRequestURI <> '') and (FStatusCode = 200) then
    begin
      // 分析报头 Headers（ Buf 已移到第 2 行的首位置）
      ExtractHeaders(Buf, Integer(Data^.Overlapped.InternalHigh) -
                          Integer(Buf - Data^.Data.buf));

      // 复制 FKeepAlive
      Response.FKeepAlive := FKeepAlive;

      // URI 解码, 提取 GET 请求的参数表
      if (FMethod = hmGet) then
        URLDecodeRequestURI;

      if (FSessionId <> '') then   // 检查 FSessionId 是否有效
        FDataProvider.CheckSessionState(Self, Response);

      // 调用事件：是否允许请求
      if Assigned(FDataProvider.FOnAccept) then
        FDataProvider.FOnAccept(THttpSocket(FOwner).Worker,
                                Self, FAccepted);

      if (FAccepted = False) then  // 禁止请求
      begin
        FStatusCode := 403;        // 403: Forbidden
        {$IFDEF DEBUG_MODE}
        iocp_log.WriteLog(THttpSocket(FOwner).PeerIPPort +
                          '->拒绝请求 ' + METHOD_LIST[FMethod] + ' ' +
                          FRequestURI + ', 状态=' + IntToStr(FStatusCode));
        {$ENDIF}
      end else
      if (FUpgradeState = 15) then  // 检查是否要升级为 WebSocket, 15=8+4+2+1
      begin
        if Assigned(FDataProvider.FOnUpgrade) then  // 是否允许升级
          FDataProvider.FOnUpgrade(Self, FHeadersAry[rqhOrigin], FAccepted);
        if (FAccepted = False) then
        begin
          FStatusCode := 406;      // 406 Not Acceptable
          {$IFDEF DEBUG_MODE}
          iocp_log.WriteLog(THttpSocket(FOwner).PeerIPPort + '->拒绝请求升级为 WebSocket.');
          {$ENDIF}
        end;
        Exit;
      end;

    end else
    begin
      FAccepted := False;
      {$IFDEF DEBUG_MODE}
      iocp_log.WriteLog(THttpSocket(FOwner).PeerIPPort +
                        '->错误的请求 ' + METHOD_LIST[FMethod] + ' ' +
                        FRequestURI + ', 状态=' + IntToStr(FStatusCode));
      {$ENDIF}
    end;
      
  end else
  if FAccepted then    // 上传数据包，接收
  begin
    Inc(FContentSize, FByteCount);   // 接收总数 +
    FStream.Write(Buf^, FByteCount); // 写入流
  end;

  if (FStatusCode > 200) then  
  begin
    FExtParams.Clear;  
    FStream.Clear;
  end else             // 接收数据完毕
  if FAccepted and Completed and (FStream.Size > 0) then
    if (FMethod = hmPost) and (FContentType <> hctUnknown) then
      try
        if FExtParams.MultiPart then  // 提取表单字段
          ExtractElements(FStream.Memory, FStream.Size, FDataProvider.FOnReceiveFile)
        else  // 提取普通变量/参数
          ExtractParams(FStream.Memory, FStream.Size, True);
      finally
        FStream.Clear;
      end;

end;

destructor THttpRequest.Destroy;
begin
  FParams.Free;
  FExtParams.Free;  
  FStream.Free;
  inherited;
end;

procedure THttpRequest.ExtractElements(Data: PAnsiChar; Len: Integer;
                                       RecvFileEvent: TOnReceiveFile);
var
  IgnoreSearch: Boolean;
  EleBuf, EleBuf2, TailBuf: PAnsiChar;
  FldType, FldType2: TFormElementType;
  Boundary, EleValue, EleValue2: AnsiString;
begin
  // 提取实体类型为 multipart/form-data 的字段/参数，加入 FParams

  // ---------------------Boundary
  // Content-Disposition: form-data; name="textline2"; filename="测试.txt"
  // <Empty Line>
  // Value Text
  // ---------------------Boundary--

  //  multipart/form-data类型 Form:
  //    不对字符编码。在使用包含文件上传控件的表单时，必须使用该类型。
  // 字段描述：1、普通内容，无 Content-Type
  //           2、文件内容：Content-Type: text/plain

  try
    EleBuf := Nil;   // 字段内容开始
    TailBuf := PAnsiChar(Data + Len);

    // 传输实际内容时要在前面加 '--'， 全部字段内容结束时再在末尾加 '--'
    Boundary := '--' + FExtParams.Boundary;

    FldType := fdtUnknown;
    FldType2 := fdtUnknown;
    IgnoreSearch := False;

    while IgnoreSearch or
          SearchInBuffer(Data, TailBuf - Data, Boundary) do
      if (Data^ in [#13, '-']) then  // 分隔标志或末尾
      begin
        if (EleBuf = Nil) then   // 字段开始，分析描述，定位内容位置
        begin
          // 跳过回车换行
          Inc(Data, 2);

          // 下次要查询 Boundary
          IgnoreSearch := False;

          // 关键的描述缺少时继续循环
          if (SearchInBuffer2(Data, 40, 'FORM-DATA') = False) then
          begin
            FStatusCode := 417;   // 不符预期：Expectation Failed
            Break;
          end;

          // 定位内容位置：描述之后第一个连续出现的两空行（STR_CRLF2）
          EleBuf2 := Data;
          if SearchInBuffer(Data, TailBuf - Data, STR_CRLF2) then
          begin
            EleBuf := Data;
            Data := EleBuf2;
          end else
          begin
            FStatusCode := 417;   // 不符预期：Expectation Failed
            Break;
          end;
                      
          // 已经定位到 FORM-DATA 后一字节，确定字段类型是否为文件
          FldType := ExtractFieldInf(Data, Integer(EleBuf - Data), EleValue);     // name="xxx"，xxx 不能太长
          FldType2 := ExtractFieldInf(Data, Integer(EleBuf - Data), EleValue2);
          Data := EleBuf;       // 到内容位置
          
          if (FldType = fdtUnknown) and (FldType2 = fdtUnknown) then
          begin
            FStatusCode := 417;  // 不符预期：Expectation Failed
            Break;
          end;

          if (FldType2 = fdtFileName) then   // 文件
          begin
            FFileName := EleValue2;
            FldType := fdtName;
          end else
          if (FldType = fdtFileName) then    // 文件，变量值互换
          begin
            FFileName := EleValue;
            EleValue := EleValue2;
            FldType2 := fdtFileName;         // 修改!
          end;

        end else
        begin
          // 当前字段内容结束、新字段内容开始，回退到前字段的结束位置
          EleBuf2 := PAnsiChar(Data - Length(Boundary));

          if (FldType2 = fdtFileName) then
          begin
            // 文件类型字段：如果名称不为空（可能内容为空），则调用外部事件
            try
              FParams.SetAsString(EleValue, FFileName);
              if (FFileName <> '') then
                if Assigned(RecvFileEvent) then
                begin
                  // 执行一次请求
                  RecvFileEvent(THttpSocket(FOwner).Worker, THttpRequest(Self),
                                FFileName, Nil, 0, hpsRequest);
                  RecvFileEvent(THttpSocket(FOwner).Worker, THttpRequest(Self),
                                FFileName, EleBuf, LongWord(EleBuf2 - EleBuf) - 2,
                                hpsRecvData);
                end;
              finally
                Delete(FFileName, 1, Length(FFileName));
              end;
          end else

          if (FldType = fdtName) then
          begin
            // 数值类型字段：加入变量表
            if (EleBuf^ in [#13, #10]) then  // 空值
              FParams.SetAsString(EleValue, '')
            else begin
              // 复制数据内容（没编码，回退2字节）
              SetString(EleValue2, EleBuf, Longword(EleBuf2 - EleBuf) - 2);
              FParams.SetAsString(EleValue, EleValue2);
            end;
          end else
          begin
            FStatusCode := 417;  // 异常
            Break;
          end;

          // 遇到结束标志：Boundary--
          if ((Data^ = '-') and ((Data + 1)^ = '-')) then
            Break;

          // 准备下一字段
          EleBuf := nil;
          IgnoreSearch := True;
        end;
      end;
  except
    on E: Exception do
    begin
      FStatusCode := 500;
      {$IFDEF DEBUG_MODE}
      iocp_log.WriteLog('THttpBase.ExtractElements->' + E.Message);
      {$ENDIF}
    end;
  end;

end;

procedure THttpRequest.ExtractHeaders(Data: PAnsiChar; DataLength: Integer);
var
  i, k, iPos: Integer;
  p: PAnsiChar;
  Header, HeaderValue: AnsiString;
begin
  // 格式：
  //   Accept: image/gif.image/jpeg,*/*
  //   Accept-Language: zh-cn
  //   Referer: <a href="http://www.google.cn/">http://www.google.cn/</a>
  //   Connection: Keep-Alive
  //   Host: localhost
  //   User-Agent: Mozila/4.0(compatible;MSIE5.01;Window NT5.0)
  //   Accept-Encoding: gzip,deflate
  //   Content-Type: text/plain
  //                 application/x-www-form-urlencoded
  //                 multipart/form-data; boundary=---------------------------7e119f8908f8
  //   Content-Length: 21698
  //   Cookie: ...
  //
  //   Body...

  // 1、application/x-www-form-urlencoded:
  //    在发送前编码所有字符（默认），空格转换为 "+" 加号，特殊符号转换为 ASCII HEX 值。
  // 正文：
  //    textline=&textline2=%D2%BB%B8%F6%CE%C4%B1%BE%A1%A3++a&onefile=&morefiles='

  // 2、text/plain: 每字段一行，不对特殊字符编码。
  // 正文（IE、Chrome）：
  //    textline=#13#10textline2=一个文本。  a#13#10onefile=#13#10morefiles=#13#10

  FAttacked := False;   // 非攻击
  FAccepted := True;    // 默认接受请求
  FContentLength := 0;  // 实体长度
  FContentType := hctUnknown;  // 实体类型
  FKeepAlive := False;  // 默认不保持连接
  FUpgradeState := 0;   // 清0, 不升级为 WebSocket

  i := 0;       // 开始位置
  k := 0;       // 行首位置
  iPos := 0;    // 分号位置
  p := Data;    // 指针位置

  while i < DataLength do
  begin
    case p^ of
      CHAR_SC:
        if (iPos = 0) then // 第一个分号 :
          iPos := i;

      CHAR_CR,
      CHAR_LF:             // 行结束
        if (iPos > 0) then
        begin
          // 取参数、值
          SetString(Header, Data + k, iPos - k);
          SetString(HeaderValue, Data + iPos + 1, i - iPos - 1);

          // 保存 Header 到数组：AnsiString -> String
          WriteHeader(GetHeaderIndex(UpperCase(Header)), Trim(HeaderValue));

          // 出现异常
          if (FStatusCode > 200) then
            Exit;
            
          k := i + 1;      // 下行首位置
          iPos := 0;

          if ((p + 1)^ = CHAR_LF) then   // 前进 1 字节，到行尾
          begin
            Inc(k);        // 下行首位置
            Inc(i);
            Inc(p);
            if ((p + 1)^ = CHAR_CR) then // 进入实体 Body
            begin
              Dec(DataLength, i + 3);    // 减 3
              if (DataLength > 0) then   // 有实体内容
              begin
                if (FContentLength > 0) then
                  InitializeInStream;

                if (DataLength <= FContentLength) then
                  FContentSize := DataLength
                else
                  FContentSize := FContentLength;

                FStream.Write((p + 3)^, FContentSize);
              end;
              Break;
            end;
          end;
        end;
    end;
    Inc(i);
    Inc(p);
  end;

  // 未收到实体内容，预设空间
  if (FContentLength > 0) and (FStream.Size = 0) then  // 预设空间
    InitializeInStream;

end;

procedure THttpRequest.ExtractMethod(var Data: PAnsiChar);
  function CheckMethod(const S: AnsiString): THttpMethod;
  var
    i: THttpMethod;
  begin
    // 检查命令是否合法
    for i := hmGet to High(METHOD_LIST) do
      if (S = METHOD_LIST[i]) then
      begin
        Result := i;
        Exit;
      end;
    Result := hmUnknown;
  end;
var
  i, iPos: Integer;
  Method: AnsiString;
  p: PAnsiChar;
begin
  // 分析请求方法、URI和版本号
  // 格式：GET /sn/index.php?user=aaa&password=ppp HTTP/1.1

  FMethod := hmUnknown; // 未知
  FRequestURI := '';    // 无
  FStatusCode := 200;   // 状态

  iPos := 0;  // 空格（内容开始）位置
  p := Data;  // 指针位置

  for i := 0 to FByteCount - 1 do  // 遍历内容
  begin
    case p^ of
      CHAR_CR, CHAR_LF: begin  // 回车换行，首行结束
        SetString(FVersion, Data + iPos + 1, i - iPos - 1);
        Break;  // 第一行分析完毕
      end;
      CHAR_SP, CHAR_TAB:    // 空格，TAB
        if (iPos = 0) then  // 请求命令结束，取请求 GET, POST...
        begin
          if (i < 8) then   // = Length('CONNECT') + 1
          begin
            SetString(Method, Data, i);
            FMethod := CheckMethod(UpperCase(Method));
          end;
          if (FMethod = hmUnknown) then
          begin
            FStatusCode := 400;  // 错误的请求
            Break;
          end else
            iPos := i; // 下一内容的开始位置
        end else
        if (FRequestURI = '') then // URI 结束，取 URI
        begin
          SetString(FRequestURI, Data + iPos + 1, i - iPos - 1);
          iPos := i;   // 下一内容的开始位置
        end;
    end;
    Inc(p); // 下一字符
  end;

  // 支持版本: 1.0, 1.1
  if (FVersion <> HTTP_VER1) and (FVersion <> HTTP_VER) then
    FStatusCode := 505
  else begin
    if (p^ = CHAR_CR) then  // 前进，到下一行首
      Inc(p, 2);
    Data := p;  // Data 指向下行首
  end;

end;

procedure THttpRequest.ExtractParams(Data: PAnsiChar; Len: Integer; Decode: Boolean);
var
  i: Integer;
  Buf: PAnsiChar;
  Param, Value: AnsiString;
begin
  // 分析参数或表单字段（要知道编码）
  // 不编码：1、user=aaa&password=ppp&No=123
  //         2、textline=#13#10textline2=一个文本。  a#13#10onefile=#13#10morefiles=#13#10
  // 默认编码：textline=&textline2=%D2%BB%B8%F6%CE%C4%B1%BE%A1%A3++a&onefile=&morefiles='
  Buf := Data;
  for i := 1 to Len do   // 末尾已经预设为"&"
  begin
    case Data^ of
      '=': begin   // 参数名称
        SetString(Param, Buf, Data - Buf);
        Buf := Data + 1; // 到下一字节
      end;
      
      '&', #13:    // 参数值，加入参数
        if (Param <> '') then
        begin
          if (Buf = Data) then  // 值为空
            FParams.SetAsString(Param, '')
          else begin
            // 非空值（hctMultiPart 类型时不在此处理）
            SetString(Value, Buf, Data - Buf);

            // 字符集转换
            if Decode then
              if FExtParams.UTF8Encode then  // TNetHttpClient：UTF-8 编码
                Value := System.UTF8Decode(Value)
              else 
              if (FContentType = hctUrlEncoded) then  // hctUrlEncoded 表单不是 UTF-8 编码
                Value := DecodeHexText(Value);

            // 加入参数
            FParams.SetAsString(Param, Value);
          end;

          // 推进
          if (Data^ = #13) then // 回车
          begin
            Buf := Data + 2;
            Inc(Data, 2);
          end else
          begin
            Buf := Data + 1;
            Inc(Data);
          end;

          // 清 Param
          Delete(Param, 1, Length(Param));
        end;
    end;

     // 推进
    Inc(Data);
  end;
end;

function THttpRequest.GetCompleted: Boolean;
begin
  // 判断是否接收完毕
  Result := (FContentLength = 0) or (FContentSize >= FContentLength);
end;

function THttpRequest.GetHeaderIndex(const Header: AnsiString): TRequestHeaderType;
var
  i: TRequestHeaderType;
begin
  // 查找 Header 在数组 REQUEST_HEADERS 的位置
  for i := rqhHost to High(TRequestHeaderType) do
    if (REQUEST_HEADERS[i] = Header) then
    begin
      Result := i;
      Exit;
    end;
  Result := rqhUnknown;
end;

function THttpRequest.GetHeaders(Index: TRequestHeaderType): AnsiString;
begin
  Result := FHeadersAry[Index];
end;

procedure THttpRequest.InitializeInStream;
begin
  // 初始化 FStream 空间
  if (FContentType = hctUnknown) then  // hctUnknown 时末尾不加 &
    FStream.Initialize(FContentLength)
  else begin  // 末尾 + &
    FStream.Initialize(FContentLength + 1);
    PAnsiChar(PAnsiChar(FStream.Memory) + FContentLength)^ := AnsiChar('&');
  end;
end;

procedure THttpRequest.URLDecodeRequestURI;
var
  i, k: Integer;
  ParamList: AnsiString;
begin
  // URL 解码，提取参数
  // 分离出 GET 请求的参数表：/aaa/ddd.jsp?code=111&name=WWW
  // 可能为 UFT-8,UFT-16
  if (Pos(AnsiChar('%'), FRequestURI) > 0) then
  begin
    ParamList := DecodeHexText(FRequestURI);
    if CheckUTFEncode(ParamList, k) then  // ParamList 可能是 UTF，中文“一”特殊，歧义？
    begin
      FRequestURI := System.UTF8Decode(ParamList);
      if (Length(FRequestURI) <> k) then  // 有特殊字符
        FRequestURI := ParamList;
    end else
      FRequestURI := ParamList;
  end;  
  i := Pos(AnsiChar('?'), FRequestURI);
  if (i > 0) then
  begin
    k := Length(FRequestURI) - i + 1;
    SetLength(ParamList, k); // 多一个字节
    System.Move(FRequestURI[i + 1], ParamList[1], k - 1);

    ParamList[k] := AnsiChar('&'); // 末尾设为 &
    ExtractParams(PAnsiChar(ParamList), k, False); // 提取参数表

    Delete(FRequestURI, i, Length(FRequestURI));
  end;
end;

procedure THttpRequest.WriteHeader(Index: TRequestHeaderType; const Content: AnsiString);
var
  i: Int64;
  ItemValue: AnsiString;
begin
  // 保存 Content 到报头数组

  FHeadersAry[Index] := Content;

  // 根据情况增加额外的变量/参数
  
  case Index of
    rqhAcceptCharset:   // TNetHttpClient
      FExtParams.SetAsBoolean('UTF8-ENCODE', Pos('UTF-8', UpperCase(Content)) > 0);

    rqhContentLength:
      // Content-Length: 增设长度变量, 文件太大时，IE < 0, Chrome 正常
      if (TryStrToInt64(Content, i) = False) then
        FStatusCode := 417    // 417 Expectation Failed
      else
        if (i < 0) or (i > FDataProvider.FMaxContentLength) then
        begin
          FStatusCode := 413;     // 内容太长：413 Request Entity Too Large
          FExtParams.SetAsInteger('CONTENT-LENGTH', 0);
        end else
        begin
          // 10 秒内同一 IP 出现 3 个 >= 10M 的请求，当作恶意
          if (i >= 10240000) and Assigned(FDataProvider.FPeerIPList) then
            FAttacked := FDataProvider.FPeerIPList.CheckAttack(THttpSocket(FOwner).PeerIP, 10000, 3)
          else
            FAttacked := False;

          if FAttacked then       // 恶意的
            FStatusCode := 403    // 403 Forbidden
          else begin
            FContentLength := i;   // 返回长度 i
            FExtParams.SetAsInt64('CONTENT-LENGTH', i);
          end;
        end;

    rqhConnection:  // Connection: Upgrade、Keep-Alive
      if (UpperCase(Content) = 'UPGRADE') then  // 支持 WebSocket
      begin
        FUpgradeState := FUpgradeState xor 1;
        FKeepAlive := True;
      end else
      if (UpperCase(Content) = 'KEEP-ALIVE') then
      begin
        FKeepAlive := FDataProvider.FKeepAlive;
        FExtParams.SetAsBoolean('KEEP-ALIVE', FKeepAlive);
      end else
        FExtParams.SetAsBoolean('KEEP-ALIVE', False);

    rqhContentType: begin
      // Content-Type: text/plain
      //               application/x-www-form-urlencoded
      //               multipart/form-data; boundary=...
      // 增设三个变量：CONTENT-TYPE、MULTIPART、BOUNDARY
      ItemValue := LowerCase(Content);
      if (FMethod = hmGet) then
      begin
        if (Pos('%', FRequestURI) > 0) then
        begin
          FContentType := hctUrlEncoded;
          FExtParams.SetAsInteger('CONTENT-TYPE', Integer(hctUrlEncoded));
        end;
        if (Pos('utf-8', ItemValue) > 1) then
          FExtParams.SetAsBoolean('UTF8-ENCODE', True);
      end else
        if (ItemValue = 'text/plain') then
        begin
          FContentType := hctTextPlain;
          FExtParams.SetAsString('BOUNDARY', '');
          FExtParams.SetAsInteger('CONTENT-TYPE', Integer(hctTextPlain));
          FExtParams.SetAsBoolean('MULTIPART', False);
        end else
        if (Pos('application/x-www-form-urlencoded', ItemValue) = 1) then
        begin
          FContentType := hctUrlEncoded;
          FExtParams.SetAsString('BOUNDARY', '');
          FExtParams.SetAsInteger('CONTENT-TYPE', Integer(hctUrlEncoded));
          FExtParams.SetAsBoolean('MULTIPART', False);
        end else
        if (Pos('multipart/form-data', ItemValue) = 1) then
        begin
          FStatusCode := 417; // 不符预期
          i := PosEx('=', Content, 28);
          if (i >= 29) then   // multipart/form-data; boundary=...
          begin
            ItemValue := Copy(Content, i + 1, 999);
            if (ItemValue <> '') then
            begin
              FContentType := hctMultiPart;
              FExtParams.SetAsString('BOUNDARY', ItemValue);
              FExtParams.SetAsInteger('CONTENT-TYPE', Integer(hctMultiPart));
              FExtParams.SetAsBoolean('MULTIPART', True);
              FStatusCode := 200;
            end;
          end;
        end else
        begin
          // 当作普通数据流，无参数
          FContentType := hctUnknown;
          FExtParams.SetAsInteger('CONTENT-TYPE', Integer(hctUnknown));
        end;
    end;

    rqhCookie:  // 提取 Cookie 信息
      if THttpSession.Extract(Content, ItemValue) then
      begin
        FAttacked := FDataProvider.FSessionMgr.CheckAttack(ItemValue);
        if FAttacked then  // 被用 SessionId 攻击
          FAccepted := False
        else    // SessionId 有效
        if (ItemValue <> HTTP_INVALID_SESSION) then
          FSessionId := ItemValue;
      end;

    rqhIfMatch,
    rqhIfNoneMatch:
      FExtParams.SetAsString('IF_MATCH', Copy(Content, 2, Length(Content) - 2));

    rqhIfRange: begin  // 断点下载
      ItemValue := Copy(Content, 2, Length(Content) - 2);
      FExtParams.SetAsString('IF_MATCH', ItemValue);
      FExtParams.SetAsString('IF_RANGE', ItemValue);
    end;

    rqhRange:  // 断点下载
      FExtParams.SetAsString('RANGE', Content);

    rqhIfModifiedSince,
    rqhIfUnmodifiedSince:
      FExtParams.SetAsString('LAST_MODIFIED', Content);

    rqhUserAgent:  // 'Mozilla/4.0 (compatible; MSIE 8.0; ...
      if Pos('MSIE ', Content) > 0 then
        FExtParams.SetAsBoolean('MSIE', True);

    // 支持 WebSocket
    //  Connection: Upgrade
    //  Upgrade: websocket    
    //  Sec-WebSocket-Key: x3JJHMbDL1EzLkh9GBhXDw==
    //  Sec-WebSocket-Protocol: chat, superchat
    //  Sec-WebSocket-Version: 13
    //  Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits
    //  Origin: http://example.com

    rqhUpgrade:  // 升级到 WebSocket
      if (UpperCase(Content) = 'WEBSOCKET') then
        FUpgradeState := FUpgradeState xor 2;

    rqhWebSocketKey:
      if (Length(Content) > 0) then
        FUpgradeState := FUpgradeState xor 4;

    rqhWebSocketVersion:
      if (Content = '13') then
        FUpgradeState := FUpgradeState xor 8;
  end;

end;

{ THttpResponseHeaders }

procedure THttpResponseHeaders.Add(Code: TResponseHeaderType; const Content: AnsiString);
begin
  // 增加报头信息
  if (Code = rshUnknown) then  // 自定义
  begin
    if (Content <> '') then  // 空时忽略
      InterAdd(Content);
  end else
  if (FHeaders[Code] = False) then
  begin
    FHeaders[Code] := True;
    case Code of
      rshDate:
        InterAdd(RESPONSE_HEADERS[Code] + CHAR_SC2 + GetHttpGMTDateTime);
      rshServer:
        InterAdd(RESPONSE_HEADERS[Code] + CHAR_SC2 + HTTP_SERVER_NAME);
      else
        InterAdd(RESPONSE_HEADERS[Code] + CHAR_SC2 + Content);
    end;
  end;
end;

procedure THttpResponseHeaders.AddCRLF;
begin
  // 加回车换行
  PStrCRLF(FBuffer)^ := STR_CRLF;
  Inc(FBuffer, 2);
  Inc(FData^.len, 2);
end;

procedure THttpResponseHeaders.Append(var AHandle: THandle; ASize: Cardinal);
begin
  // 追加文件内容（实体）
  try
    ReadFile(AHandle, FBuffer^, ASize, ASize, Nil);
    Inc(FBuffer, ASize);
    Inc(FData^.len, ASize);
  finally
    CloseHandle(AHandle);
    AHandle := 0;  // 必须，否则清资源时异常
  end;
end;

procedure THttpResponseHeaders.Append(var AStream: TStream; ASize: Cardinal);
begin
  // 追加数据流（实体）
  try
    AStream.Read(FBuffer^, ASize);
    Inc(FBuffer, ASize);
    Inc(FData^.len, ASize);
  finally
    AStream.Free;
    AStream := nil;  // 必须，否则清资源时异常 
  end;
end;

procedure THttpResponseHeaders.Append(AList: TInStringList; ASize: Cardinal);
var
  i: Integer;
  S: AnsiString;
begin
  // 追加列表内容（实体）
  try
    for i := 0 to AList.Count - 1 do
    begin
      S := AList.Strings[i];
      System.Move(S[1], FBuffer^, Length(S));
      Inc(FBuffer, Length(S));
    end;
    Inc(FData^.len, ASize);
  finally
    AList.Clear;
  end;
end;

procedure THttpResponseHeaders.Append(const Content: AnsiString; SetCRLF: Boolean);
begin
  // 加入字符串（一行内容）
  InterAdd(Content, SetCRLF);
end;

procedure THttpResponseHeaders.ChunkDone;
begin
  PAnsiChar(FData^.buf)^ := AnsiChar('0');  // 0
  PStrCRLF2(FData^.buf + 1)^ := STR_CRLF2;  // 回车换行, 两个
  FData^.len := 5;
end;

procedure THttpResponseHeaders.ChunkSize(ASize: Cardinal);
begin
  if (ASize > 0) then
  begin
    // 修改 Chunk 长度，在当前位置加 STR_CRLF，设置填充长度
    PChunkSize(FData.buf)^ := PChunkSize(AnsiString(IntToHex(ASize, 4)) + STR_CRLF)^;
    PStrCRLF(FBuffer)^ := STR_CRLF;
    Inc(FData^.len, 8);
  end else
  begin  // 清空，前移 6 字节
    FBuffer := FData^.buf;
    FData^.len := 0;
    Inc(FBuffer, 6);  // 只留出长度描述空间
  end;
end;

procedure THttpResponseHeaders.Clear;
begin
  // 写入地址恢复到开始位置
  FillChar(FHeaders, SizeOf(THeaderArray), 0);
  if Assigned(FBuffer) and (FData^.len > 0) then
  begin
    FBuffer := FData^.buf;
    FData^.len := 0;
  end;
end;

function THttpResponseHeaders.GetSize: Integer;
begin
  // 取发送内容长度
  Result := FData^.len;
end;

procedure THttpResponseHeaders.InterAdd(const Content: AnsiString; SetCRLF: Boolean);
begin
  // 增加报头项目
  // 如：Server: InIOCP/2.0

  // 加入内容
  System.Move(Content[1], FBuffer^, Length(Content));
  Inc(FData^.len, Length(Content));
  Inc(FBuffer, Length(Content));

  // 加回车换行
  if SetCRLF then
  begin
    PStrCRLF(FBuffer)^ := STR_CRLF;
    Inc(FData^.len, 2);    
    Inc(FBuffer, 2);
  end;
end;

procedure THttpResponseHeaders.SetOwner(const Value: TServerTaskSender);
begin
  // 初始化
  FOwner := Value;
  FData := FOwner.Data;
  FData^.len := 0;
  FBuffer := FData^.buf;
end;

procedure THttpResponseHeaders.SetStatus(Code: Integer);
begin
  // 设置响应状态
  Clear;
  case Code div 100 of
    1:
      InterAdd(HTTP_VER + HTTP_STATES_100[Code - 100]);
    2:
      InterAdd(HTTP_VER + HTTP_STATES_200[Code - 200]);
    3:
      InterAdd(HTTP_VER + HTTP_STATES_300[Code - 300]);
    4:
      InterAdd(HTTP_VER + HTTP_STATES_400[Code - 400]);
    else
      InterAdd(HTTP_VER + HTTP_STATES_500[Code - 500]);
  end;
end;

{ THttpResponse }

procedure THttpResponse.AddContent(const Content: AnsiString);
begin
  // 增加实体内容
  FContent.Add(Content);
  FContentSize := FContent.Size;  // 调
end;

procedure THttpResponse.AddDataPackets;
var
  ETag, LastModified: AnsiString;

  procedure AddPacket(Range: AnsiString);
  var
    StartPos, EndPos: Integer;
  begin

    // 分析范围
    EndPos := 0;
    if (Range[1] = '-') then    // -235
    begin
      StartPos := StrToInt(Copy(Range, 2, 99));
      if (FContentSize >= StartPos) then
      begin
        StartPos := FContentSize - StartPos;
        EndPos := FContentSize - 1;
      end else
        FStatusCode := 416;     //  416 Requested range not satisfiable
    end else
    if (Range[Length(Range)] = '-') then    // 235-
    begin
      Delete(Range, Length(Range), 1);
      StartPos := StrToInt(Range);
      if (FContentSize >= StartPos) then
        EndPos := FContentSize - 1
      else
        FStatusCode := 416;     //  416 Requested range not satisfiable
    end else                    // 235-2289
    begin
      EndPos := Pos('-', Range);
      StartPos := StrToInt(Copy(Range, 1, EndPos - 1));
      EndPos := StrToInt(Copy(Range, EndPos + 1, 99));
      if (StartPos > EndPos) or (EndPos >= FContentSize) then
        FStatusCode := 416;     //  416 Requested range not satisfiable
    end;

    if (FStatusCode <> 416) then
    begin
      SetStatus(206);
      FHeaders.Add(rshServer);
      FHeaders.Add(rshDate);

      FHeaders.Add(rshAcceptRanges, 'bytes');
      FHeaders.Add(rshContentType, FContentType);

      if (FSessionId <> '') then  // 加入 Cookie
        FHeaders.Add(rshSetCookie, HTTP_SESSION_ID + '=' + FSessionId);

      // 数据块长度
      FHeaders.Add(rshContentLength, IntToStr(EndPos - StartPos + 1));

      // 范围：起始-截止/总长
      FHeaders.Add(rshContentRange, 'bytes ' + IntToStr(StartPos) + '-' +
                   IntToStr(EndPos) + '/' + IntToStr(FContentSize));

      FHeaders.Add(rshETag, ETag);
      FHeaders.Add(rshLastModified, LastModified);

      if FKeepAlive then
        FHeaders.Add(rshConnection, 'keep-alive')
      else
        FHeaders.Add(rshConnection, 'close');

      // 先发送报头
      FHeaders.AddCRLF;
      FSender.SendBuffers;

      // 后发送文件实体
      if (FSender.ErrorCode = 0) then
        FSender.Send(FHandle, FContentSize, StartPos, EndPos);
    end;
  end;
var
  i: Integer;
  RangeList: AnsiString;
begin
  // 断点下载
  //  设置要发送的数据块范围
  //  可能同时请求多个范围，不支持 TransmitFile 发送

  ETag := '"' + GetFileETag + '"';
  LastModified := GetHttpGMTDateTime(TFileTime(FLastWriteTime));
  RangeList := FExtParams.Range + ','; //  'bytes=123-145,1-1,-23,900-,';

  i := Pos('=', RangeList);
  Delete(RangeList, 1, i);

  i := Pos(',', RangeList);
  repeat
    AddPacket(Copy(RangeList, 1, i - 1));  // 增加一个数据块发送任务
    // 请求范围错误、发送异常 -> 退出
    if (FStatusCode = 416) or (FSender.ErrorCode <> 0) then
      Break
    else begin
      Delete(RangeList, 1, i);
      i := Pos(',', RangeList);
    end;
  until i = 0;

end;

procedure THttpResponse.AddHeader(Code: TResponseHeaderType; const Content: AnsiString);
begin
  // 增加报头信息
  if (FStatusCode = 0) then
    FStatusCode := 200;
  if (FHeaders.Size = 0) then
  begin
    SetStatus(FStatusCode);
    FHeaders.Add(rshServer);
    FHeaders.Add(rshDate);
  end;
  FHeaders.Add(Code, Content);
end;

procedure THttpResponse.AddHeaderList(SendNow: Boolean);
begin
  // 准备状态、报头
  if (FStatusCode = 0) then
    FStatusCode := 200;
  if (FHeaders.Size = 0) then
  begin
    SetStatus(FStatusCode);
    FHeaders.Add(rshServer);
    FHeaders.Add(rshDate);
  end;

  if (FStatusCode < 400) and (FStatusCode <> 204) then
  begin
    // 统一使用 gb2312
    // 用 text/html: IE 8 可能乱码, Chrome 正常
    if (FContentType <> '') then
      FHeaders.Add(rshContentType, FContentType + '; CharSet=gb2312')
    else
    if (FContent.Count > 0) or (FContentSize = 0) then
      FHeaders.Add(rshContentType, 'text/html; CharSet=gb2312')
    else
      FHeaders.Add(rshContentType, 'text/plain; CharSet=gb2312');

    // 发送源
    if (FContent.Count > 0) then
    begin
      // html 脚本
      FHeaders.Add(rshContentLength, IntToStr(FContentSize));
    end else
    if Assigned(FStream) then
    begin
      // 内存/文件流
      if FGZipStream then
        FHeaders.Add(rshContentEncoding, 'gzip');
      FHeaders.Add(rshContentLength, IntToStr(FContentSize));
    end else
    if (FContentSize > 0) and (FLastWriteTime > 0) then
    begin
      // 文件句柄，返回文件标记和文件的 UTC 修改时间（64位，精确到千万分之一秒）
      FHeaders.Add(rshContentLength, IntToStr(FContentSize));
      FHeaders.Add(rshAcceptRanges, 'bytes');
      FHeaders.Add(rshETag, '"' + GetFileETag + '"');
      FHeaders.Add(rshLastModified, GetHttpGMTDateTime(TFileTime(FLastWriteTime)));
      // Content-Disposition，让浏览器下载，不是直接打开。
      if (FFileName <> '') then
        FHeaders.Add(rshUnknown, 'Content-Disposition: attachment; filename=' + FFileName);
    end;

    FHeaders.Add(rshCacheControl, 'No-Cache');

    if FKeepAlive then
      FHeaders.Add(rshConnection, 'keep-alive')
    else
      FHeaders.Add(rshConnection, 'close');
  end else
  begin
    // 异常，返回 FContent 的内容
    FHeaders.Add(rshContentLength, IntToStr(FContent.Size));
    if (FContent.Size > 0) then
    begin
      FHeaders.AddCRLF;
      FHeaders.Append(FContent, FContent.Size);
    end;
  end;

  if (FSessionId <> '') then  // 加入 Cookie
    FHeaders.Add(rshSetCookie, HTTP_SESSION_ID + '=' + FSessionId);

  // 报头结束，发送
  FHeaders.AddCRLF;
  if SendNow then
    FSender.SendBuffers;
  
end;

procedure THttpResponse.Clear;
begin
  inherited;
  FreeResources;
  FContentType := '';
  FGZipStream := False;
  FLastWriteTime := 0;
  FWorkDone := False;
end;

constructor THttpResponse.Create(ADataProvider: THttpDataProvider; AOwner: TObject);
begin
  inherited;
  FContent := TInStringList.Create;   // 消息内容
  FHeaders := THttpResponseHeaders.Create; // 服务器状态、报头
end;

procedure THttpResponse.CreateSession;
var
  Session: THttpSession;
begin
  // 新建 SessionId
  Session := THttpSession.Create;
  FSessionId := Session.CreateSessionId;
  FDataProvider.FSessionMgr.Add(FSessionId, Session);  // 加入 Hash 表的值
end;

destructor THttpResponse.Destroy;
begin
  FreeResources;
  FHeaders.Free;
  FContent.Free;
  inherited;
end;

procedure THttpResponse.FreeResources;
begin
  if Assigned(FStream) then
  begin
    FStream.Free;
    FStream := nil;
  end;
  if (FHandle > 0) then
  begin
    CloseHandle(FHandle);
    FHandle := 0;
  end;
  if (FContent.Count > 0) then
    FContent.Clear;
end;

function THttpResponse.GetFileETag: AnsiString;
begin
  // 取文件标识（原理上是唯一的）
  Result := IntToHex(FLastWriteTime, 2) + '-' + IntToHex(FContentSize, 4);
end;

function THttpResponse.GZipCompress(Stream: TStream): TStream;
begin
  // GZip 压缩流、返回文件流
  Result := TFileStream.Create(iocp_varis.gTempPath + '_' +
                        IntToStr(NativeUInt(Self)) + '.tmp', fmCreate);
  try
    Stream.Position := 0;    // 必须
    iocp_zlib.GZCompressStream(Stream, Result, '');
  finally
    Stream.Free;
  end;
end;

procedure THttpResponse.Redirect(const URL: AnsiString);
begin
  // 定位到指定的 URL
  SetStatus(302);  // 302 Found, 303 See Other
  FHeaders.Add(rshServer);
  FHeaders.Add(rshLocation, URL);
  FHeaders.AddCRLF;       
end;

procedure THttpResponse.RemoveSession;
begin
  // 设置无效的 SessionId，反馈给客户端
  if (FSessionId <> '') and (FSessionId <> HTTP_INVALID_SESSION) then
    FDataProvider.FSessionMgr.Remove(FSessionId);
  FSessionId := HTTP_INVALID_SESSION;  // 无效的
end;

procedure THttpResponse.SendWork;
  procedure AppendEntityData;
  begin
    // 把小实体内容加入到 FHeaders 之后
    if (FHandle > 0) then        // 可能是 Handle 流
      FHeaders.Append(FHandle, FContentSize)
    else
    if (Assigned(FStream)) then  // 发送流
      FHeaders.Append(FStream, FContentSize)
    else
    if (FContent.Count > 0) then // 发送实体列表
      FHeaders.Append(FContent, FContentSize);
  end;
  procedure SendEntityData;
  begin
    // 发送实体内容
    {$IFDEF TRANSMIT_FILE}
    // 1. 用 TransmitFile 发送
    //    发送完成或异常都在 THttpSocket.ClearResources 中
    //    释放资源 FHandle 和 FStream
    with TBaseSocketRef(FOwner) do
      if (FHandle > 0) then  // 文件句柄 Handle
        FTask.SetTask(FHandle, FContentSize)
      else
      if Assigned(FStream) then  // 流
        FTask.SetTask(FStream, FContentSize)
      else
      if (FContent.Count > 0) then  // 发送实体列表
      begin
        FTask.SetTask(FContent.HttpString[False]);
        FContent.Clear;
      end;
    {$ELSE}
    // 2. 用 WSASend 发送, 自动释放 FHandle、FStream
    if (FHandle > 0) then  // 文件句柄 Handle
      FSender.Send(FHandle, FContentSize)
    else
    if Assigned(FStream) then  // 流
      FSender.Send(FStream, FContentSize, True)
    else
    if (FContent.Count > 0) then // 发送实体列表
    begin
      FSender.Send(FContent.HttpString[False]);
      FContent.Clear;
    end;
    {$ENDIF}
  end;
begin
  // 正式发送数据

  if (FStatusCode = 206) then   // 1. 断点下载
  begin
    AddDataPackets;
    if (FStatusCode = 416) then // 请求范围错误
    begin
      Clear;  // 释放资源
      AddHeaderList(True);
    end;
  end else

  if (FWorkDone = False) then   // 2. 普通发送
    if (FStatusCode >= 400) or (FSender.ErrorCode > 0) then   // 2.1 异常
      AddHeaderList(True)       // 立即发送
    else begin
      AddHeaderList(False);     // 先不发送
      if (FContentSize + FHeaders.Size <= IO_BUFFER_SIZE) then
      begin
        // 2.2 发送缓存还装得下实体
        if (FContentSize > 0) then
          AppendEntityData;       // 加入实体内容
        FSender.SendBuffers;  // 正式发送
      end else
      begin
        // 2.3 实体太长, 先发报头，再实体
        FSender.SendBuffers;  // 正式发送
        SendEntityData;  // 发实体
      end;         
    end;

  {$IFDEF DEBUG_MODE}
  iocp_log.WriteLog(TBaseSocket(FOwner).PeerIPPort +
                    '->执行请求 ' + METHOD_LIST[FRequest.FMethod] + ' ' +
                    FRequest.FRequestURI + ', 状态=' + IntToStr(FStatusCode));
  {$ENDIF}

  {$IFDEF TRANSMIT_FILE}
  TBaseSocketRef(FOwner).InterTransmit;
  {$ELSE}
  FHandle := 0;    // 已经发出
  FStream := nil;  // 已经发出
  {$ENDIF}

end;

procedure THttpResponse.SendChunk(Stream: TStream);
begin
  // 立即分块发送（在外部释放 Stream）
  //   HTTP 协议不能对单块进行压缩（整体传输时可以）
  //   不支持 TransmitFile 模式发送数据！
  FreeResources;
  if (Assigned(Stream) = False) then
    FStatusCode := 204
  else
    try
      // 发送的数据结构：
      // 1. 报头；
      // 2. 长度 + 回车换行 + 内容 + 回车换行；
      // 3. "0" + 回车换行 + 回车换行

      // 1. 发送报头
      SendChunkHeaders;

      // 2. 发送调制的实体
      TServerTaskSender(FSender).Chunked := True; // 分块发送！
      FSender.Send(Stream, Stream.Size, False); // 不释放流!
    finally
      FWorkDone := True;
      if (Stream is TMemoryStream) then
        Stream.Size := 0;
    end;
end;

procedure THttpResponse.SendChunkHeaders(const ACharSet: AnsiString);
begin
  // 发送分块描述
  SetStatus(200);

  FHeaders.Add(rshServer);
  FHeaders.Add(rshDate);
  FHeaders.Add(rshContentType, CONTENT_TYPES[0].ContentType + ACharSet);

  if (FSessionId <> '') then // 加入 Cookie: InIOCP_SID=...
    FHeaders.Add(rshSetCookie, HTTP_SESSION_ID + '=' + FSessionId);

  FHeaders.Add(rshTransferEncoding, 'chunked');
  FHeaders.Add(rshCacheControl, 'no-cache, no-store');
  FHeaders.Add(rshPragma, 'no-cache');
  FHeaders.Add(rshExpires, '-1');

  // 报头结束，发送
  FHeaders.AddCRLF;
  FSender.SendBuffers;

end;

procedure THttpResponse.SendJSON(DataSet: TDataSet; CharSet: THttpCharSet);
begin
  // 把大数据集转为 JSON 分块发送（立即发送）
  // CharSet：把记录转换为相应的字符集
  SendChunkHeaders(HTTP_CHAR_SETS[CharSet]);  // 发送报头
  try
    LargeDataSetToJSON(DataSet, FHeaders, CharSet);  // 建 JSON，发送
  finally
    FWorkDone := True; // 改进，在发送器自动发送结束标志
  end;
end;

procedure THttpResponse.SendJSON(const JSON: AnsiString);
begin
  // 设置要发送的 JSON（不立即发送）
  SetContent(JSON);
end;

procedure THttpResponse.SendStream(Stream: TStream; Compress: Boolean);
begin
  // 准备要发送的数据流

  FreeResources;
  if Assigned(Stream) then
  begin
    FContentSize := Stream.Size;
    if (FContentSize > MAX_TRANSMIT_LENGTH div 200) then  // 太大
    begin
      FContentSize := 0;
      FStatusCode := 413;
    end else
    begin
      if Compress then  // 压缩
      begin
        FStream := GZipCompress(Stream);  // 压缩成文件流，释放 Stream
        FContentSize := FStream.Size; // 改变
      end else
        FStream := Stream;  // 不压缩
      FGZipStream := Compress;
      FContentType := CONTENT_TYPES[0].ContentType;
    end;
  end else
    FStatusCode := 204;
    
end;

procedure THttpResponse.SetContent(const Content: AnsiString);
begin
  // 第一次加入实体内容
  if (FContent.Count > 0) then
    FContent.Clear;
  FContent.Add(Content);
  FContentSize := FContent.Size;  // 调
end;

procedure THttpResponse.SetHead;
begin
  // 返回 Head 请求的信息
  SetStatus(200);

  FHeaders.Add(rshServer);
  FHeaders.Add(rshDate);
  FHeaders.Add(rshAllow, 'GET, POST, HEAD');  // CONNECT, DELETE, PUT, OPTIONS, TRACE');
  FHeaders.Add(rshContentType, 'text/html; CharSet=gb2312');
  FHeaders.Add(rshContentLang, 'zh-cn,zh;q=0.8,en-us;q=0.5,en;q=0.3');
  FHeaders.Add(rshContentEncoding, 'gzip, deflate');
  FHeaders.Add(rshContentLength, IntToStr(FDataProvider.FMaxContentLength));
  FHeaders.Add(rshCacheControl, 'no-cache');
  FHeaders.Add(rshTransferEncoding, 'chunked');

  if FKeepAlive then
    FHeaders.Add(rshConnection, 'keep-alive')
  else
    FHeaders.Add(rshConnection, 'close');

  // 报头结束
  FHeaders.AddCRLF;
  
end;

procedure THttpResponse.SetStatus(Code: Integer);
begin
  // 设置响应状态
  FStatusCode := Code;
  FHeaders.SetStatus(Code);
end;

procedure THttpResponse.TransmitFile(const FileName: String; AutoView: Boolean);
var
  ETag: AnsiString;
  TempFileName: String;
  lpCreationTime, lpLastAccessTime: _FILETIME;
begin
  // 打开文件资源
  // http://blog.csdn.net/xiaofei0859/article/details/52883500

  FreeResources;

  TempFileName := AdjustFileName(FileName);
  if (FileExists(TempFileName) = False) then
  begin
    FStatusCode := 404;
    SetContent('<html><body>InIOCP/2.0: 页面不存在！</body></html>');
    {$IFDEF DEBUG_MODE}
    iocp_log.WriteLog('THttpResponse.TransmitFile->文件不存在：' + FileName);
    {$ENDIF}
    Exit;
  end;

  // InternalOpenFile 把 INVALID_HANDLE_VALUE 转为 0
  FHandle := InternalOpenFile(TempFileName);

  if (FHandle > 0) then
  begin
    // 用 文件大小 + 修改时间 计算 ETag
    FContentSize := GetFileSize64(FHandle);

    // 返回 FFileName，chrome 浏览器以后断点下载
    if not AutoView or (FContentSize > 4096000) then
      FFileName := ExtractFileName(FileName);

    GetFileTime(FHandle, @lpCreationTime, @lpLastAccessTime, @FLastWriteTime);

    ETag := GetFileETag;
    if (ETag = FExtParams.IfMath) and
       (GetHttpGMTDateTime(TFileTime(FLastWriteTime)) = FExtParams.LastModified) then
    begin
      if (FExtParams.Range <> '') then  // 请求断点下载
      begin
        FStatusCode := 206;     // 可能请求多个数据块，大小未定
        FContentType := CONTENT_TYPES[0].ContentType;
      end else
      begin
        FStatusCode := 304;     // 304 Not Modified，文件没修改过
        FContentSize := 0;
        FLastWriteTime := 0;
        CloseHandle(FHandle);
        FHandle := 0;
      end;
    end else
    begin
      if (FExtParams.Range <> '') then  // 请求断点下载
      begin
        FStatusCode := 206;     // 可能请求多个数据块，大小未定
        FContentType := CONTENT_TYPES[0].ContentType;
      end else
        FContentType := GetContentType(FileName);
    end;
  end else
  begin
    FStatusCode := 500;
    {$IFDEF DEBUG_MODE}
    iocp_log.WriteLog('THttpResponse.TransmitFile->打开文件异常：' + FileName);
    {$ENDIF}
  end;

end;

procedure THttpResponse.Upgrade;
begin
  // 升级为 WebSocket，反馈
  FHeaders.SetStatus(101);

  FHeaders.Add(rshServer); // 可选
  FHeaders.Add(rshDate);   // 可选
    
  FHeaders.Add(rshConnection, 'Upgrade');
  FHeaders.Add(rshUpgrade, 'WebSocket');
  FHeaders.Add(rshWebSocketAccept, iocp_SHA1.EncodeBase64(
           iocp_SHA1.SHA1StringA(FRequest.GetHeaders(rqhWebSocketKey) +
                                 WSOCKET_MAGIC_GUID)));

  FHeaders.Add(rshContentType, 'text/html; charSet=utf-8'); // 可选
  FHeaders.Add(rshContentLength, '0'); // 可选
  FHeaders.AddCRLF;
  
  FSender.SendBuffers;
end;

end.
