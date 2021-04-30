(*
 * iocp c/s 服务消息封装单元
 *)
unit iocp_msgPacks;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils,
  System.Variants, Datasnap.DBClient, Datasnap.Provider, {$ELSE}
  Windows, Classes, SysUtils, Variants, DBClient, Provider, {$ENDIF}
  iocp_winSock2, iocp_md5, iocp_mmHash,
  iocp_zlib, iocp_base, iocp_lists,
  iocp_baseObjs;

type

  { TMemBuffer: 自定义内存块 }

  TMemBuffer = Pointer;

  // ================ 扩展的内存流 ================
  // 可以把外部内存块绑定到流，无需额外写入

  TInMemStream = class(TMemoryStream)
  private
    FBinding: Boolean;   // 绑定模式
  protected
    function Realloc(var NewCapacity: Longint): Pointer; override;
  public
    procedure Initialize(ASize: Cardinal);
    procedure SetMemory(ABuffer: Pointer; ASize: Longint);
    procedure NilAndFree;
  end;

  // ================== 文件流扩展 类 ======================
  //  打开文件参数与 TFileStream 的不同
  //  自动删除临时文件，参考 THandleStream

  TReceivePack = class;

  TIOCPDocument = class(THandleStream)
  private
    FTempFile: Boolean;        // 临时文件
    FOriginSize: TFileSize;    // 文件长度（关闭后用）
    FCreationTime: TFileTime;  // 建立时间
    FAccessTime: TFileTime;    // 访问时间
    FLastWriteTime: TFileTime; // 修改时间
    procedure InternalCreate(const AFileName: String; CreateNew: Boolean);
  protected
    FFileName: String;         // 文件名
    procedure RenameDoc(var AFileName: String);
    procedure SetFileInf(Params: TReceivePack);
  public
    constructor Create(const AFileName: String = ''; CreateNew: Boolean = False);
    constructor CreateEx(const AFileName: String);
    destructor Destroy; override;
    procedure Close(DeleFile: Boolean = False);    
  public
    property FileName: String read FFileName;
    property OriginSize: TFileSize read FOriginSize;
  end;

  // ================== 字段定义 类 ======================

  // 变量/字段定义(字符串用 AnsiSting)

  TBasePack = class;

  TVarField = class(TObject)
  private
    FName: AnsiString;     // 名称
    FData: TListVariable;  // 内容存放信息
    function FieldSpace: Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function GetDataRef: Pointer; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function GetIsNull: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function GetSize: Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure InterClear;
    procedure InterSetBuffer(AEleType: TElementType; ABuffer: PAnsiChar; ASize: Integer);
    procedure InterSetStream(AEleType: TElementType; ABuffer: PAnsiChar; ASize: Integer);
    procedure IniFieldValue(AValue: PListVariable);
    procedure SetNilValue; {$IFDEF USE_INLINE} inline; {$ENDIF}
  protected
    // 读变量 =======================
    function GetAsBoolean: Boolean;
    function GetAsInteger: Integer;
    function GetAsCardinal: Cardinal;
    function GetAsInt64: Int64;
    function GetAsFloat: Double;
    function GetAsDateTime: TDateTime;
    function GetAsObject: TObject;
    function GetAsString: AnsiString;
    // Buffer、Record、Stream 加入时是引用
    function GetAsBuffer: TMemBuffer;
    function GetAsRecord: TBasePack;
    function GetAsStream: TStream;
    // Variant 类型
    function GetAsVariant: Variant;
  protected
    // 写变量 =======================
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsString(const Value: AnsiString);
    // 引用 Buffer、Record、Stream
    procedure SetAsBuffer(const Value: TMemBuffer);
    procedure SetAsRecord(const Value: TBasePack);
    procedure SetAsStream(const Value: TStream);
    // Variant 类型
    procedure SetAsVariant(const Value: Variant);
  public
    constructor Create(AName: AnsiString); overload;
    constructor Create(AField: TVarField); overload;
    destructor Destroy; override;
    property DataRef: Pointer read GetDataRef;
    property Name: AnsiString read FName;
    property IsNull: Boolean read GetIsNull;
    property VarType: TElementType read FData.EleType;
    property Size: Integer read GetSize;
  public
    property AsBoolean: Boolean read GetAsBoolean;
    property AsBuffer: TMemBuffer read GetAsBuffer;
    property AsCardinal: Cardinal read GetAsCardinal;
    property AsDateTime: TDateTime read GetAsDateTime;
    property AsFloat: Double read GetAsFloat;
    property AsInteger: Integer read GetAsInteger;
    property AsInt64: Int64 read GetAsInt64;
    property AsObject: TObject read GetAsObject;
    property AsRecord: TBasePack read GetAsRecord;
    property AsStream: TStream read GetAsStream;
    property AsString: AnsiString read GetAsString;
    property AsVariant: Variant read GetAsVariant; // 新增
  end;

  // ================== 基本消息包 类 ======================
  // 流 <-> 变量 互换，未公开属性，不能直接使用

  TBasePackObject = class(TObject)
  protected
    FError: Boolean;       // 解析错误  
    FMsgId: TIOCPMsgId;    // 消息 ID
    FOwner: TMessageOwner; // 所有者（组件）、TWebSocket/TWSConnection
    FSize: TFileSize;      // 大小
    function GetOwner: TObject;
    function GetSize: TFileSize; virtual;
  public
    property Error: Boolean read FError;
    property MsgId: TIOCPMsgId read FMsgId;
    property Owner: TObject read GetOwner;
    property Size: TFileSize read GetSize;
  end;

  // IOCP 数据流协议 消息

  TIOCPStream = class(TBasePackObject)
  public                   
    constructor Create(AOwner: TObject);
  end;

  // C/S 协议消息 基类

  TBasePack = class(TBasePackObject)
  protected
    FList: TInList;        // 变量列表
    FTmpVar: Variant;      // 临时变量
  private
    function GetCount: Integer;
    function GetFields(Index: Integer): TVarField;

    // 查找变量/字段
    function FindField(VarName: AnsiString; var Field: TVarField): Boolean;

    // 写字段数据到内存
    procedure WriteBuffers(const Buf: Pointer);
  protected
    // 复制属性
    procedure CopyDataPack(APack: TBasePack); virtual;

    // 检查名称的合法性
    procedure CheckFieldName(const Value: AnsiString); virtual;

    // 保存变量表到内存流
    procedure SaveToMemStream(Stream: TMemoryStream; WriteExtra: Boolean); virtual;
    procedure SaveToStreamExt(Stream: TStream; WriteExtra, ClearPrams: Boolean);

    // 扫描内存块，建变量表
    procedure ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal; ReadExtra: Boolean); virtual;

    // 设置变量值
    procedure SetField(EleType: TElementType; const VarName: AnsiString;
                       const Value: PListVariable; const sValue: AnsiString = '');

    // 字段转为 JSON
    procedure VarToJSON(var Buf: PAnsiChar; const VarName, VarValue: AnsiString;
                        Digital: Boolean; FirstPos: Boolean = False; EndPos: Boolean = False);
  protected
    // 读变量 =======================
    function GetAsBoolean(const Index: String): Boolean;
    function GetAsInteger(const Index: String): Integer;
    function GetAsCardinal(const Index: String): Cardinal;
    function GetAsInt64(const Index: String): Int64;
    function GetAsFloat(const Index: String): Double;
    function GetAsDateTime(const Index: String): TDateTime;

    function GetAsDocument(const Index: String): String;
    function GetAsString(const Index: String): String;

    // Buffer、Record、Stream 加入时是引用
    function GetAsBuffer(const Index: String): TMemBuffer;
    function GetAsRecord(const Index: String): TBasePack;
    function GetAsStream(const Index: String): TStream;
    function GetAsVariant(const Index: String): Variant;
  protected
    procedure InterRefresh; virtual;

    // 写变量 =======================
    procedure SetAsBoolean(const Index: String; const Value: Boolean);
    procedure SetAsInteger(const Index: String; const Value: Integer);
    procedure SetAsCardinal(const Index: String; const Value: Cardinal);
    procedure SetAsInt64(const Index: String; const Value: Int64);
    procedure SetAsFloat(const Index: String; const Value: Double);
    procedure SetAsDateTime(const Index: String; const Value: TDateTime);

    procedure SetAsDocument(const Index: String; const Value: String);
    procedure SetAsString(const Index: String; const Value: String);

    // 引用 Buffer、Record、Stream
    procedure SetAsBuffer(const Index: String; const Value: TMemBuffer);
    procedure SetAsRecord(const Index: String; const Value: TBasePack);
    procedure SetAsStream(const Index: String; const Value: TStream);
    procedure SetAsVariant(const Index: String; const Value: Variant);
  protected
    property AsBoolean[const Index: String]: Boolean read GetAsBoolean write SetAsBoolean;
    property AsBuffer[const Index: String]: TMemBuffer read GetAsBuffer write SetAsBuffer;
    property AsCardinal[const Index: String]: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsDateTime[const Index: String]: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDocument[const Index: String]: String read GetAsDocument write SetAsDocument;
    property AsFloat[const Index: String]: Double read GetAsFloat write SetAsFloat;
    property AsInteger[const Index: String]: Integer read GetAsInteger write SetAsInteger;
    property AsInt64[const Index: String]: Int64 read GetAsInt64 write SetAsInt64;
    property AsRecord[const Index: String]: TBasePack read GetAsRecord write SetAsRecord;
    property AsStream[const Index: String]: TStream read GetAsStream write SetAsStream;
    property AsString[const Index: String]: String read GetAsString write SetAsString;
    property AsVariant[const Index: String]: Variant read GetAsVariant write SetAsVariant;
  public
    constructor Create; overload;
    destructor Destroy; override;
  public  
    procedure Clear; virtual;
    procedure Initialize(Stream: TStream; ClearIt: Boolean = True); overload;
    procedure Initialize(APack: TBasePack); overload;
    procedure LoadFromFile(const AFileName: String);
    procedure SaveToFile(const AFileName: String);
    procedure SaveToStream(Stream: TStream; ClearParams: Boolean = True);
  public
    property Count: Integer read GetCount;
    property Document[const index: String]: TStream read GetAsStream;
    property Fields[index: Integer]: TVarField read GetFields;
  end;

  // ================== 用户消息包 类 ======================
  // TBasePack 的使用形式，公开属性

  TCustomPack = class(TBasePack)
  public
    property AsBoolean;
    property AsBuffer;
    property AsCardinal;
    property AsDateTime;
    property AsDocument;
    property AsFloat;
    property AsInteger;
    property AsInt64;
    property AsRecord;
    property AsString;
    property AsStream;
    property AsVariant;
  end;

  // ================== 协议消息包 基类 ======================
  // 带协议头，接收、发送数据用
  // 预设常用变量/属性的读写方法，根据需要公开

  THeaderPack = class(TBasePack)
  protected
    // ======== 以下元素与 TMsgHead 的字段一致 ==========
    FSessionId: Cardinal;       // 认证/登录 ID
    FDataSize: Cardinal;        // 变量型消息的原始长度（主体）
    FAttachSize: TFileSize;     // 文件、流长度（附件）
    FOffset: TFileSize;         // 断点续传的位移
    FOffsetEnd: TFileSize;      // 断点续传的结束位移
    FCheckType: TDataCheckType; // 校验类型
    FVarCount: Cardinal;        // 变量型消息的变量/元素个数
    FZipLevel: TZipLevel;       // 主体的压缩率
    FTarget: TActionTarget;     // 目的对象类型
    FAction: TActionType;       // 操作分类
    FActResult: TActionResult;  // 操作结果
    // ==================================================
  protected
    FMain: TInMemStream;        // 主体数据流
    procedure CopyDataPack(APack: TBasePack); override;
    procedure SaveToMemStream(Stream: TMemoryStream; WriteExtra: Boolean); override;
    procedure ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal; ReadExtra: Boolean); override;
    procedure ToRecord(var ABuffer: PAnsiChar; var ASize: Cardinal);
  protected
    function GetAttachFileName: String;   // 续传文件全名
    function GetConnection: Integer;      // 数据连接编号
    function GetDateTime: TDateTime;      // 取日期时间
    function GetDirectory: String;        // 工作路径
    function GetFileName: String;         // 传输的文件名
    function GetFileSize: TFileSize;      // 文件大小
    function GetFunctionGroup: string;    // 远程函数组
    function GetFunctionIndex: Integer;   // 远程函数编号
    function GetHasParams: Boolean;       // SQL 是否带参数
    function GetLocalPath: string;        // 取文件存放路径
    function GetMsg: String;              // 消息内容
    function GetNewFileName: String;      // 新的文件名
    function GetPassword: String;         // 密码/口令
    function GetPeerIPPort: String;       // 客户端 IP:Port
    function GetReuseSessionId: Boolean;  // 是否重用凭证
    function GetRole: TClientRole;        // 角色/权限
    function GetSize: TFileSize; override;// 全部变量的空间大小
    function GetSQL: String;              // SQL 文本内容
    function GetSQLName: String;          // SQL 名称（服务端预设）
    function GetStoredProcName: String;   // 存储过程名称
    function GetToUser: String;           // 目的用户名
    function GetURL: String;              // 服务器附件的 URL
    function GetUserGroup: string;        // 用户分组
    function GetUserName: String;         // 用户名、来源
  protected
    procedure SetAttachFileName(const Value: String);
    procedure SetConnection(const Value: Integer);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetDirectory(const Value: String);
    procedure SetFileName(const Value: String);
    procedure SetFileSize(const Value: TFileSize);
    procedure SetFunctionGroup(const Value: String);
    procedure SetFunctionIndex(const Value: Integer);
    procedure SetHasParams(const Value: Boolean);
    procedure SetLocalPath(const Value: String);
    procedure SetMsg(const Value: String);
    procedure SetNewFileName(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetPeerIPPort(const Value: String);
    procedure SetReuseSessionId(const Value: Boolean);
    procedure SetRole(const Value: TClientRole);
    procedure SetSQL(const Value: String);
    procedure SetSQLName(const Value: String);
    procedure SetStoredProcName(const Value: String);
    procedure SetToUser(const Value: String);
    procedure SetURL(const Value: String);
    procedure SetUserGroup(const Value: String);
    procedure SetUserName(const Value: String);    
  public
    function  ToJSON: AnsiString;
    function  GetMsgSize: TFileSize;
    procedure GetHeadMsg(Msg: PMsgHead);
    procedure SetHeadMsg(Msg: PMsgHead; ForReturn: Boolean = False);
  public
    // 公开属性
    property AsBoolean;
    property AsBuffer;
    property AsCardinal;
    property AsDateTime;
    property AsDocument;
    property AsFloat;
    property AsInteger;
    property AsInt64;
    property AsRecord;
    property AsString;
    property AsStream;
    property AsVariant;
  end;

  // ================== 收到的消息包 类 ======================
  // 服务端、客户端收到的消息，只读！
   
  TReceivePack = class(THeaderPack)
  private
    FAttachment: TIOCPDocument; // 附件数据流
  protected
    procedure CreateAttachment(const ALocalPath: String); virtual;
  protected
    property PeerIPPort: String read GetPeerIPPort;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel;
    procedure Clear; override;
    procedure Write(AData: PPerIOData);
  public
    // 主体流，附件流
    property Main: TInMemStream read FMain;
    property Attachment: TIOCPDocument read FAttachment write FAttachment;
  public
    // 协议头属性
    property Action: TActionType read FAction;
    property ActResult: TActionResult read FActResult;
    property AttachSize: TFileSize read FAttachSize;
    property CheckType: TDataCheckType read FCheckType;
    property DataSize: Cardinal read FDataSize;
    property MsgId: TIOCPMsgId read FMsgId;
    property Offset: TFileSize read FOffset;
    property OffsetEnd: TFileSize read FOffsetEnd;
    property SessionId: Cardinal read FSessionId;
    property Target: TActionTarget read FTarget;
    property VarCount: Cardinal read FVarCount;
    property ZipLevel: TZipLevel read FZipLevel;
  public
    // 常用变量/属性
    property Connection: Integer read GetConnection;
    property DateTime: TDateTime read GetDateTime;
    property Directory: String read GetDirectory;
    property FileName: String read GetFileName;
    property FromUser: String read GetUserName;
    property FunctionGroup: String read GetFunctionGroup;
    property FunctionIndex: Integer read GetFunctionIndex;
    property HasParams: Boolean read GetHasParams;
    property Msg: String read GetMsg;
    property NewFileName: String read GetNewFileName;
    property Password: String read GetPassword;
    property ReuseSessionId: Boolean read GetReuseSessionId;
    property Role: TClientRole read GetRole;
    property StoredProcName: String read GetStoredProcName;
    property SQL: String read GetSQL;
    property SQLName: String read GetSQLName;
    property TargetUser: String read GetToUser;
    property ToUser: String read GetToUser;
    property URL: String read GetURL;
    property UserGroup: String read GetUserGroup;
    property UserName: String read GetUserName;
  end;

  // ================== 发送消息包 类 ======================
  // 服务端、客户端发送数据用，读写！

  // 消息分类：
  // 1. 主体：1.1 As... 系列的变量型数据（FVarCount > 0）
  //          1.2 Variant 数据集类型数据（FVarCount = 0）
  // 2. 附件：包括文件和流，它们与数据集类型数据三者互斥

  // 发送方法：
  // 1. 先发主体，后发附件
  // 2. 客户端发送附件必须先请求，等待服务端反馈允许再发送
  // 3. 服务端发送附件无需请求，直接发送

  // 数据格式：
  //   首包：IOCP_HEAD_FLAG + TMsgHead + [校验码 + 校验码] + [主体原始数据]
  // 后续包：[主体或附件的原始数据]

  TBaseMessage = class(THeaderPack)
  protected
    FAttachFileName: String; // 附件的文件名
    FAttachment: TStream;    // 附件数据流
    FAttachZiped: Boolean;   // 附件是否已压缩
  private
    procedure GetCheckCode(AStream: TStream; ToBuf: PAnsiChar;
                           ASize: TFileSize; var Offset: Cardinal);
    procedure InterSetAttachment(AStream: TStream);
  protected
    procedure AdjustTransmitRange(ChunkSize: Integer);
    procedure CreateStreams(ClearList: Boolean = True); virtual;
    procedure GetFileInfo(const AFileName: String);
    procedure LoadFromCDSVariant(const ACDSAry: array of TClientDataSet;
                                 const ATableNames: array of String); virtual; // 客户端不公开
    procedure LoadFromVariant(const AProviders: array of TDataSetProvider;
                              const ATableNames: array of String); overload; virtual; // 客户端不公开
    procedure LoadHead(Data: PWsaBuf);
    procedure NilStreams(CloseAttachment: Boolean);
    procedure OpenLocalFile; virtual;
    class procedure CreateHead(ABuf: PAnsiChar; AResult: TActionResult);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure Clear; override;
    procedure LoadFromFile(const AFileName: String; ServerMode: Boolean = False); virtual;
    procedure LoadFromStream(AStream: TStream; AZipCompressIt: Boolean = False);
  public
    // 主体流，附件流
    property Main: TInMemStream read FMain;
    property Attachment: TStream read FAttachment;
    property AttachFileName: String read FAttachFileName;
  public
    // 服务端、客户端常用属性（读写）
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property FromUser: String read GetUserName write SetUserName;
    property Msg: String read GetMsg write SetMsg;
    property PeerIPPort: String read GetPeerIPPort write SetPeerIPPort;
    property Role: TClientRole read GetRole write SetRole;
    property TargetUser: String read GetToUser write SetToUser;
    property ToUser: String read GetToUser write SetToUser;
    property UserGroup: String read GetUserGroup write SetUserGroup;
    property UserName: String read GetUserName write SetUserName;
  end;

  // ================== 服务端消息保存 类 ======================

  TMessageWriter = class(TObject)
  private
    FLock: TThreadLock;    // 消息锁
    FSurportHttp: Boolean; // 生成附件的 URL
  public
    constructor Create(SurportHttp: Boolean);
    destructor Destroy; override;
  public
    procedure LoadMsg(const UserName: String; Msg: TBaseMessage);
    procedure SaveMsg(Data: PPerIOData; const ToUser: String); overload;
    procedure SaveMsg(Msg: THeaderPack; const ToUser: String); overload;
  end;

  // ================== 客户端离线消息阅读 类 ======================

  TMessageReader = class(TObject)
  private
    FHandle: THandle;  // 文件句柄
    FCount: Integer;   // 消息总数
  public
    destructor Destroy; override;
    procedure Close;
    function Extract(Msg: TReceivePack; LastMsgId: TIOCPMsgId = 0): Boolean;
    procedure Open(const FileName: String);
  public
    property Count: Integer read FCount;
  end;

// 自定义的内存块, 结构：Size + Content
function GetBuffer(const Value: Integer): TMemBuffer;
function FreeBuffer(P: TMemBuffer): Integer;
function BufferSize(P: TMemBuffer): Integer;

// 写大查询数据集到 TBaseMessage 的格式文件
// 对应 TBaseMessage.LoadFromVariant
function SaveToMessageFile(const FileName: String;
         const AProviders: array of TDataSetProvider;
         const ATableNames: array of String): Boolean;

implementation

uses
  iocp_log, iocp_varis, iocp_utils, http_utils;

function GetBuffer(const Value: Integer): TMemBuffer;
begin
  if (Value > 0) then
  begin
    GetMem(Result, Value + SizeOf(Integer));
    PInteger(Result)^ := Value;  // 第一个 Integer 为长度
    Inc(PAnsiChar(Result), SizeOf(Integer));  // 返回第二元素地址
  end else
    Result := Nil;
end;

function FreeBuffer(P: TMemBuffer): Integer;
begin
  Dec(PAnsiChar(P), SizeOf(Integer));  // 后退一个 Integer 位置
  Result := PInteger(P)^;
  FreeMem(P);
end;

function BufferSize(P: TMemBuffer): Integer;
begin
  Dec(PAnsiChar(P), SizeOf(Integer));  // 后退一个 Integer 位置
  Result := PInteger(P)^;
end;

function SaveToMessageFile(const FileName: String;
         const AProviders: array of TDataSetProvider;
         const ATableNames: array of String): Boolean;
var
  Handle: THandle;    // 文件句柄
  iCount: Cardinal;   // 读写字节数

  procedure __WriteHandle(EleType: TElementType; const S: AnsiString;
                          Data: Variant; IntValue: Integer = 0;
                          BoolValue: Boolean = False);
  const
    BUFFER_SIZE = 1024 * 32;
  var
    iSize: Integer;
    Variable: TStreamVariable;  // 字段描述结构
    VarBuffer: PAnsiChar;       // Data 的内部内存
    StrBuf: AnsiString;         // 交换缓存

    Stream: TInMemStream;       // 可绑定内存流
    OutStream: TFileStream;     // 输出文件流
  begin
    // 转换变量/参数表到带格式的内存流
    //   流格式：IntegerElementCount + Variable, Variable2...
    // 变量格式：TStreamVariable + Name + Value | (BufferSize + Buffer)

    Variable.EleType := EleType;
    Variable.NameSize := Length(S);

    WriteFile(Handle, Variable, STREAM_VAR_SIZE, iCount, nil); // EleType
    WriteFile(Handle, S[1], Variable.NameSize, iCount, nil); // Name

    case EleType of
      etBoolean:
        WriteFile(Handle, BoolValue, SizeOf(Boolean), iCount, nil); // Boolean Value
      etInteger:
        WriteFile(Handle, IntValue, SizeOf(Integer), iCount, nil); // Integer Value
      etVariant:
        if VarIsNull(Data) then
        begin
          iSize := 0;
          WriteFile(Handle, iSize, SizeOf(Integer), iCount, nil); // BufferSize=0
        end else
        begin
          //  压缩 Data，写入文件
          Stream := TInMemStream.Create;
          OutStream := TFileStream.Create(FileName + '.z', fmCreate);

          iSize := VarArrayHighBound(Data, 1) - VarArrayLowBound(Data, 1) + 1;
          VarBuffer := VarArrayLock(Data);

          try
            // 把内存 Buffer 绑定到 Stream 处，压缩
            Stream.SetMemory(VarBuffer, iSize);
            iocp_zlib.ZCompressStream(Stream, OutStream);

            // 写 Buffer 长度
            iSize := OutStream.Size;
            WriteFile(Handle, iSize, SizeOf(Integer), iCount, nil); // Buffer Size

            // 循环写入流数据
            OutStream.Position := 0;
            SetLength(StrBuf, BUFFER_SIZE);

            repeat
              ReadFile(THandle(OutStream.Handle), StrBuf[1], BUFFER_SIZE, iCount, nil);
              if (iCount > 0) then
                WriteFile(Handle, StrBuf[1], iCount, iCount, nil);
            until (iCount < BUFFER_SIZE);

          finally
            StrBuf := '';
            OutStream.Free;
            Stream.NilAndFree;  // 解除绑定 Buffer，释放
            VarArrayUnlock(Data);  // 在后
          end;
        end;
    end;
  end;

var
  i, k: Integer;
  VarCount: Integer;  // 字段总数
begin
  // 参考：TBaseMessage.LoadFromVariant
  if (High(AProviders) >= 0) then
  begin
    Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
                         0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if (Handle <> 0) then
      try
        k := High(ATableNames);
        VarCount := High(AProviders) + 2;  // 额外两个字段

        // 首4字节：字段数
        WriteFile(Handle, VarCount, SizeOf(Integer), iCount, nil);

        for i := 0 to VarCount - 2 do
          if (i <= k) and (ATableNames[i] <> '') then  // 带数据表名称
            __WriteHandle(etVariant, ATableNames[i], AProviders[i].Data)
          else  // 无数据表名称，不更新数据表
            __WriteHandle(etVariant, '__@DATASET_' + IntToStr(i), AProviders[i].Data);

        // 加字段：字段数
        __WriteHandle(etInteger, '__VarCount', Null, VarCount);
      finally
        CloseHandle(Handle);
      end;
  end;
end;

{ TInMemStream }

procedure TInMemStream.Initialize(ASize: Cardinal);
begin
  // 设置流的总长度
  Size := ASize;
  Position := 0;
end;

procedure TInMemStream.NilAndFree;
begin
  // 内存为绑定外部的时，设为nil（不能释放）
  FBinding := True;
  SetPointer(nil, 0);
  Capacity := 0;
  Free;  // 释放
end;

function TInMemStream.Realloc(var NewCapacity: Integer): Pointer;
begin
  if FBinding then    // 绑定模式
    Result := Memory  // 使用绑定的内存
  else
    Result := inherited Realloc(NewCapacity);
end;

procedure TInMemStream.SetMemory(ABuffer: Pointer; ASize: Integer);
begin
  // 把外部内存 ABuffers 设为自身内存
  if Assigned(Memory) then
    inherited Clear;
  FBinding := True;
  SetPointer(ABuffer, ASize);  // 设置内存
  Capacity := ASize;  // 必须，否则 Free 时不释放内存
  FBinding := False;
  Position := 0;
end;

{ TIOCPDocument }

constructor TIOCPDocument.Create(const AFileName: String; CreateNew: Boolean);
begin
  inherited Create(0);
  if (AFileName = '') then  // 临时文件，关闭时删除
  begin
    FTempFile := True;
    InternalCreate(iocp_varis.gTempPath + '_' +
                   IntToStr(NativeUInt(Self)) + '.tmp', True);
  end else
  begin  // 建新或打开文件
    FTempFile := False;
    InternalCreate(AFileName, CreateNew);
  end;
end;

constructor TIOCPDocument.CreateEx(const AFileName: String);
begin
  inherited Create(0);
  // 只打开文件（当作附件）
  FHandle := InternalOpenFile(AFileName, True);  // 只读
  if (FHandle > 0) then
  begin
    FFileName := AFileName;
    FOriginSize := GetFileSize64(FHandle);  // 原始长度
    GetFileTime(FHandle, @FCreationTime, @FAccessTime, @FLastWriteTime);
  end else
  begin
    iocp_log.WriteLog('TIOCPDocument.CreateEx->打开文件异常：' + AFileName);
    Raise Exception.Create('打开文件异常.');
  end;
end;

destructor TIOCPDocument.Destroy;
begin
  Self.Close(FTempFile);
  inherited;
end;

procedure TIOCPDocument.InternalCreate(const AFileName: String; CreateNew: Boolean);
begin
  // InternalOpenFile 把 INVALID_HANDLE_VALUE 转为 0
  if CreateNew then  // 新建文件
    FHandle := FileCreate(AFileName, fmCreate or fmOpenWrite or fmShareDenyWrite)
  else begin  // 打开文件
    FHandle := InternalOpenFile(AFileName, False); // 允许写
    FOriginSize := GetFileSize64(FHandle);  // 原始长度
    GetFileTime(FHandle, @FCreationTime, @FAccessTime, @FLastWriteTime);
  end;
  if (FHandle > 0) then
    FFileName := AFileName
  else begin
    iocp_log.WriteLog('TIOCPDocument.InternalCreate->新建/打开文件异常：' + AFileName);
    Raise Exception.Create('新建/打开文件异常.');
  end;
end;

procedure TIOCPDocument.Close(DeleFile: Boolean);
begin
  if (FHandle > 0) then
  begin
    try
      CloseHandle(FHandle);
    finally
      FHandle := 0;
    end;
    if DeleFile or FTempFile then  // 删除临时文件
    begin
      {$IFDEF DELPHI_XE7UP}
      System.SysUtils.DeleteFile(FFileName);
      {$ELSE}
      SysUtils.DeleteFile(FFileName);
      {$ENDIF}
      FFileName := '';
    end;
  end;
end;

procedure TIOCPDocument.SetFileInf(Params: TReceivePack);
begin
  // 已建文件流，设置文件属性
  //   见：TBaseMessage.GetFileInfo

  // 断点续传时 AttachSize 是块长度
  FOriginSize := Params.GetFileSize;
  Size := FOriginSize;
  Position := 0;  // 必须

  if (FTempFile = False) then  // 不是临时文件
  begin
    FCreationTime.dwLowDateTime := Params.AsCardinal['__creationLow'];
    FCreationTime.dwHighDateTime := Params.AsCardinal['__creationHigh'];
    FAccessTime.dwLowDateTime := Params.AsCardinal['__accessLow'];
    FAccessTime.dwHighDateTime := Params.AsCardinal['__accessHigh'];
    FLastWriteTime.dwLowDateTime := Params.AsCardinal['__modifyLow'];
    FLastWriteTime.dwHighDateTime := Params.AsCardinal['__modifyHigh'];

    SetFileTime(FHandle, @FCreationTime, @FAccessTime, @FLastWriteTime);
  end;
end;

procedure TIOCPDocument.RenameDoc(var AFileName: String);
var
  i: Integer;
begin
  Close;
  if RenameFile(FFileName, AFileName) then
    FFileName := AFileName
  else begin
    i := Pos('_UNZIP', FFileName);  // 已解压还原的文件
    if (i = 0) then
      i := Pos('.chunk', FFileName);  // 续传文件
    if (i > 0) and RenameFile(FFileName, Copy(FFileName, 1, i - 1)) then
    begin
      Delete(FFileName, i, 6); // 后缀均为 6 字节
      AFileName := FFileName;
    end;
  end;
end;

{ TVarField }

constructor TVarField.Create(AName: AnsiString);
begin
  inherited Create;
  FName := AName;
  FData.EleType := etNull;
end;

constructor TVarField.Create(AField: TVarField);
begin
  inherited Create;
  // 用复制的方法设置字段值
  FName := AField.FName;
  IniFieldValue(@AField.FData);
end;

destructor TVarField.Destroy;
begin
  InterClear;
  inherited;
end;

function TVarField.FieldSpace: Integer;
begin
  // 取存储空间大小（含描述）
  // TStreamVariable + 内容长度 + 名称 + 内容
  Result := GetSize;
  Inc(Result, STREAM_VAR_SIZE + Length(FName));
  if (FData.EleType >= etBuffer) then
    Inc(Result, SizeOf(Integer));
end;

function TVarField.GetAsBoolean: Boolean;
  function CompareStr(const S: AnsiString): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
  begin
    // True -> 1, 见：SetAsBoolean
    Result := (S = '1') or (S = 'True') or (S = 'Yes');
  end;
begin
  case FData.EleType of
    etBoolean:
      Result := FData.BooleanValue;
    etString:
      Result := CompareStr(AsString);
    etCardinal:
      Result := FData.CardinalValue > 0;
    etFloat:
      Result := FData.FloatValue > 0;
    etInt64:
      Result := FData.Int64Value > 0;
    etInteger:
      Result := FData.IntegerValue > 0;
    else
      Result := False;
  end;
end;

function TVarField.GetAsBuffer: TMemBuffer;
begin
  // 加入时是引用，复制一份，外部要释放 Result
  if (FData.EleType = etBuffer) then
  begin
    Result := GetBuffer(FData.DataSize);
    System.Move(FData.Data^, Result^, FData.DataSize);
  end else
    Result := nil;
end;

function TVarField.GetAsCardinal: Cardinal;
begin
  if (FData.EleType = etString) then
    Result := StrToInt(AsString)
  else
    Result := FData.CardinalValue;
end;

function TVarField.GetAsDateTime: TDateTime;
begin
  if (FData.EleType = etString) then
    Result := StrToDateTime(AsString)
  else
    Result := FData.DateTimeValue;
end;

function TVarField.GetAsFloat: Double;
begin
  if (FData.EleType = etString) then
    Result := StrToFloat(AsString)
  else
    Result := FData.FloatValue;
end;

function TVarField.GetAsInt64: Int64;
begin
  case FData.EleType of
    etInt64:
      Result := FData.Int64Value;
    etCardinal:
      Result := FData.CardinalValue;
    etInteger:
      Result := FData.IntegerValue;
    etString:
      Result := StrToInt64(AsString);
    else
      Result := 0;
  end;
end;

function TVarField.GetAsInteger: Integer;
begin
  if (FData.EleType = etString) then
    Result := StrToInt(AsString)
  else
    Result := FData.IntegerValue;
end;

function TVarField.GetAsObject: TObject;
begin
  case FData.EleType of
    etCardinal:
      Result := TObject(GetAsCardinal);
    etInteger:
      Result := TObject(GetAsInteger);
    etInt64:
      Result := TObject(GetAsInt64);
    else
      Result := nil;
  end;
end;

function TVarField.GetAsRecord: TBasePack;
begin
  // 复制一份，外部要释放 Result（加入时为 TMemoryStream 流引用）
  if (FData.EleType = etRecord) then
  begin
    Result := TCustomPack.Create;  // 用 TCustomPack，外部能使用属性
    Result.ScanBuffers(TMemoryStream(FData.Data).Memory, FData.DataSize, False); // 分析变量
  end else
    Result := nil;
end;

function TVarField.GetAsStream: TStream;
begin
  // 复制一份，外部要释放 Result（加入时为 TStream 流引用）
  case FData.EleType of
    etRecord,
    etStream: begin  // 加入时是引用，复制
      Result := TMemoryStream.Create;
      TStream(FData.Data).Position := 0;  // 必须
      Result.CopyFrom(TStream(FData.Data), FData.DataSize);
      Result.Position := 0;
    end;
    etBuffer,
    etString,
    etVariant: begin // 转为流
      Result := TMemoryStream.Create;
      Result.Size := FData.DataSize;
      Result.Write(FData.Data^, FData.DataSize);
    end;
    else
      Result := Nil;
  end;
end;

function TVarField.GetAsString: AnsiString;
const
  BOOLEAN_STRS: array[Boolean] of AnsiString = ('0', '1');
begin
  // 不是 String 的类型转换
  case FData.EleType of
    etBuffer,
    etString,
    etVariant:
      SetString(Result, PAnsiChar(FData.Data), FData.DataSize);
    etRecord,
    etStream:
      if (TStream(FData.Data) is TMemoryStream) then
        SetString(Result, PAnsiChar(TMemoryStream(FData.Data).Memory), FData.DataSize)
      else begin
        SetLength(Result, FData.DataSize);
        TStream(FData.Data).Read(Result[1], FData.DataSize);
      end;
    etBoolean:
      Result := BOOLEAN_STRS[FData.BooleanValue];
    etCardinal:
      Result := IntToStr(FData.CardinalValue);
    etDateTime:
      Result := DateTimeToStr(FData.DateTimeValue);
    etFloat:
      Result := FloatToStr(FData.FloatValue);
    etInt64:
      Result := IntToStr(FData.Int64Value);
    etInteger:
      Result := IntToStr(FData.IntegerValue);
    else
      Result := '';
  end;
end;

function TVarField.GetAsVariant: Variant;
  function _StreamToVariant: Variant;
  var
    p: Pointer;
  begin
    // 把流转换为 varByte-Variant 类型（数据集或 Delta）
    Result := VarArrayCreate([0, FData.DataSize - 1], varByte);
    p := VarArrayLock(Result);
    try
      TStream(FData.Data).Read(p, FData.DataSize);
    finally
      VarArrayUnlock(Result);
    end;
  end;
  function _BufferToVariant: Variant;
  var
    p, OutBuf: Pointer;
    OutSize: Integer;
  begin
    // 解压内存，转为 varByte-Variant
    ZDecompress(FData.Data, FData.DataSize, OutBuf,
                OutSize, FData.DataSize * 5);
    Result := VarArrayCreate([0, OutSize - 1], varByte);
    p := VarArrayLock(Result);
    try
      System.Move(OutBuf^, p^, OutSize);  // 写入数据
    finally
      VarArrayUnlock(Result);
      FreeMem(OutBuf, OutSize);  // 释放临时内存
    end;
  end;
var
  S: AnsiString;
begin
  // 把字段转为 Variant
  if (FData.EleType = etNull) or (FData.DataSize = 0) or (FData.Data = nil) then
    Result := ''  // 用空字符，通用。Null
  else
    case FData.EleType of
      etBoolean:
        Result := FData.BooleanValue;
      etCardinal:
        Result := FData.CardinalValue;
      etFloat:
        Result := FData.FloatValue;
      etInteger:
        Result := FData.IntegerValue;
      etInt64:
        Result := FData.Int64Value;
      etDateTime:
        Result := FData.DateTimeValue;
      etString,
      etBuffer: begin // 转为 String
        SetString(S, PAnsiChar(FData.Data), FData.DataSize);
        Result := S;
      end;
      etRecord,
      etStream:      // 解压、转换为 varByte-Variant 类型（JSON 用 etStream）
        Result := _StreamToVariant;
      etVariant:     // 真正的 Variant 类型（要解压）
        Result := _BufferToVariant;
    end;
end;

function TVarField.GetDataRef: Pointer;
begin
  Result := FData.Data;  // 变长数据引用地址
end;

function TVarField.GetIsNull: Boolean;
begin
  Result := (FData.EleType = etNull) or
            (FData.EleType >= etBuffer) and (FData.DataSize = 0);
end;

function TVarField.GetSize: Integer;
begin
  case FData.EleType of
    etBoolean:
      Result := SizeOf(Boolean);
    etDateTime:
      Result := SizeOf(TDateTime);
    etCardinal:
      Result := SizeOf(Cardinal);
    etFloat:
      Result := SizeOf(Double);
    etInt64:
      Result := SizeOf(Int64);
    etInteger:
      Result := SizeOf(Integer);
    etBuffer, etString,
    etRecord, etStream,
    etVariant:
      Result := FData.DataSize;
    else  // etNull
      Result := 0;
  end;
end;

procedure TVarField.InterClear;
begin
  // 清除空间或释放对象（也可能重复赋值）
  case FData.EleType of
    etBuffer:  // 自定义的
      if Assigned(FData.Data) then
      begin
        FreeBuffer(FData.Data);
        FData.Data := nil;
      end;
    etString, etVariant:
      if Assigned(FData.Data) then
      begin
        FreeMem(FData.Data);
        FData.Data := nil;
      end;
    etRecord, etStream:
      if Assigned(FData.Data) then
      begin
        TStream(FData.Data).Free;
        FData.Data := nil;
      end;
  end;
end;

procedure TVarField.InterSetBuffer(AEleType: TElementType; ABuffer: PAnsiChar; ASize: Integer);
begin
  // 解析时从提取一段内存内容
  if (ASize = 0) then
    SetNilValue
  else begin
    FData.EleType := AEleType;
    FData.DataSize := ASize;
    if (AEleType = etBuffer) then
      FData.Data := GetBuffer(ASize)
    else
      GetMem(FData.Data, ASize);
    System.Move(ABuffer^, FData.Data^, ASize);
  end;
end;

procedure TVarField.InterSetStream(AEleType: TElementType; ABuffer: PAnsiChar; ASize: Integer);
begin
  // 解析时从内存提取 Stream 内容
  if (ASize = 0) then
    SetNilValue
  else begin
    FData.EleType := AEleType;
    FData.DataSize := ASize;
    FData.Data := TMemoryStream.Create;
    TMemoryStream(FData.Data).Size := ASize;
    System.Move(ABuffer^, TMemoryStream(FData.Data).Memory^, ASize);
  end;
end;

procedure TVarField.IniFieldValue(AValue: PListVariable);
begin
  // 复制字段内容
  case AValue^.EleType of
    etBoolean:  // 逻辑
      SetAsBoolean(AValue^.BooleanValue);
    etCardinal: // 无符号整型
      SetAsCardinal(AValue^.CardinalValue);
    etDateTime: // 时间日期
      SetAsDateTime(AValue^.DateTimeValue);
    etFloat:    // 浮点型
      SetAsFloat(AValue^.FloatValue);
    etInt64:    // 64 位整数
      SetAsInt64(AValue^.Int64Value);
    etInteger:  // 整型
      SetAsInteger(AValue^.IntegerValue);
    // 以下必须为复制
    etBuffer, etString,   // 内存\字符串
    etRecord, etStream,   // 记录\流
    etVariant:            // Variant
      InterSetBuffer(AValue^.EleType, AValue^.Data, AValue^.DataSize);
  end;
end;

procedure TVarField.SetAsBoolean(const Value: Boolean);
begin
  FData.EleType := etBoolean;
  FData.BooleanValue := Value;
end;

procedure TVarField.SetAsBuffer(const Value: TMemBuffer);
begin
  // 加入 TMemBuffer 的引用，外边不能释放 Value
  if Assigned(Value) then
  begin
    FData.EleType := etBuffer;
    FData.DataSize := BufferSize(Value);
    FData.Data := Value;
  end else
    SetNilValue;
end;

procedure TVarField.SetAsCardinal(const Value: Cardinal);
begin
  FData.EleType := etCardinal;
  FData.CardinalValue := Value;
end;

procedure TVarField.SetAsDateTime(const Value: TDateTime);
begin
  FData.EleType := etDateTime;
  FData.DateTimeValue := Value;
end;

procedure TVarField.SetAsFloat(const Value: Double);
begin
  FData.EleType := etFloat;
  FData.FloatValue := Value;
end;

procedure TVarField.SetAsInt64(const Value: Int64);
begin
  FData.EleType := etInt64;
  FData.Int64Value := Value;
end;

procedure TVarField.SetAsInteger(const Value: Integer);
begin
  FData.EleType := etInteger;
  FData.IntegerValue := Value;
end;

procedure TVarField.SetAsRecord(const Value: TBasePack);
var
  Stream: TMemoryStream;
begin
  // 复制、加入记录流, 外部要释放 Value
  if Assigned(Value) then
  begin
    Stream := TMemoryStream.Create;
    Value.SaveToStream(Stream, False);
    FData.EleType := etRecord;
    FData.DataSize := Stream.Size;  // 2018-09-09
    FData.Data := Stream;
  end else
    SetNilValue;
end;

procedure TVarField.SetAsStream(const Value: TStream);
begin
  // 加入流引用, 外部不能释放 Value
  if Assigned(Value) then
  begin
    FData.EleType := etStream;
    FData.DataSize := Value.Size;
    FData.Data := Value;
  end else
    SetNilValue;
end;

procedure TVarField.SetAsString(const Value: AnsiString);
begin
  // 加入字符串（复制内容）
  InterSetBuffer(etString, PAnsiChar(Value), Length(Value));
end;

procedure TVarField.SetAsVariant(const Value: Variant);
var
  i: Integer;
  p: Pointer;
begin
  // 设置 Variant 字段到 FData.Data
  // 自动压缩，读时要解压，见：GetAsVariant
  if VarIsNull(Value) then
    SetNilValue
  else begin
    i := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1;
    p := VarArrayLock(Value);
    try
      FData.EleType := etVariant;
      ZCompress(p, i, FData.Data, FData.DataSize, zcDefault);
    finally
      VarArrayUnlock(Value);
    end;
  end;
end;

procedure TVarField.SetNilValue;
begin
  FData.EleType := etNull;  // 空值
  FData.DataSize := 0;
  FData.Data := nil;
end;

{ TBasePackObject }

function TBasePackObject.GetOwner: TObject;
begin
  Result := TObject(FOwner);
end;

function TBasePackObject.GetSize: TFileSize;
begin
  Result := FSize;  // 取消息长度
end;

{ TIOCPStream }

constructor TIOCPStream.Create(AOwner: TObject);
begin
  inherited Create;
  if Assigned(AOwner) then  // 是客户端
  begin
    FOwner := TMessageOwner(AOwner);
    FMsgId := GetUTCTickCountEh(Self);
  end;
end;

{ TBasePack }

constructor TBasePack.Create;
begin
  inherited;
  FList := TInList.Create;
end;

destructor TBasePack.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TBasePack.CheckFieldName(const Value: AnsiString);
begin
  if not (Length(Value) in [1..128]) then
    raise Exception.Create('变量名称不能为空或太长.');
end;

procedure TBasePack.Clear;
var
  i: Integer;
begin
  FSize := 0;
  for i := 0 to FList.Count - 1 do
    TVarField(FList.PopFirst).Free;
end;

procedure TBasePack.CopyDataPack(APack: TBasePack);
var
  i: Integer;
begin
  // 复制字段值
  for i := 0 to APack.Count - 1 do
    FList.Add(TVarField.Create(APack.Fields[i])); // TVarField(FList.Items[Index]);
end;

function TBasePack.FindField(VarName: AnsiString; var Field: TVarField): Boolean;
var
  i: Integer;
begin
  // 查找变量/字段
  //  VarName 用 AnsiString，大写
  VarName := UpperCase(VarName);
  for i := 0 to FList.Count - 1 do
  begin
    Field := TVarField(FList.Items[i]);
    if (Field.FName = VarName) then  // 同为 AnsiString
    begin
      Result := True;
      Exit;
    end;
  end;
  Field := nil;
  Result := False;
end;

function TBasePack.GetAsBoolean(const Index: String): Boolean;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsBoolean
  else
    Result := False;
end;

function TBasePack.GetAsBuffer(const Index: String): TMemBuffer;
var
  Field: TVarField;
begin
  // 复制一份，外部要释放 Result（加入时是引用）
  if FindField(Index, Field) then
    Result := Field.AsBuffer
  else
    Result := nil;
end;

function TBasePack.GetAsCardinal(const Index: String): Cardinal;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsCardinal
  else
    Result := 0;
end;

function TBasePack.GetAsDateTime(const Index: String): TDateTime;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsDateTime
  else
    Result := 0.0;
end;

function TBasePack.GetAsDocument(const Index: String): String;
begin
  Raise Exception.Create('用 Document[] 属性提取文件流。');
end;

function TBasePack.GetAsFloat(const Index: String): Double;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsFloat
  else
    Result := 0.0;
end;

function TBasePack.GetAsInt64(const Index: String): Int64;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsInt64
  else
    Result := 0;
end;

function TBasePack.GetAsInteger(const Index: String): Integer;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsInteger
  else
    Result := 0;
end;

function TBasePack.GetAsRecord(const Index: String): TBasePack;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsRecord
  else
    Result := nil;
end;            

function TBasePack.GetAsStream(const Index: String): TStream;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsStream
  else
    Result := nil;
end;

function TBasePack.GetAsString(const Index: String): String;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsString
  else
    Result := '';
end;

function TBasePack.GetAsVariant(const Index: String): Variant;
var
  Field: TVarField;
begin
  if FindField(Index, Field) then
    Result := Field.AsVariant
  else
    Result := Null;
end;

function TBasePack.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBasePack.GetFields(Index: Integer): TVarField;
begin
  // 取索引为 Index 的字段
  Result := TVarField(FList.Items[Index]);
end;

procedure TBasePack.SaveToFile(const AFileName: String);
var
  Stream: TFileStream;
begin
  // 保存变量表到文件
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    if (Stream.Handle > 0) then
      SaveToStreamExt(Stream, True, True); // 写协议值，清除字段
  finally
    Stream.Free;
  end;
end;

procedure TBasePack.SaveToMemStream(Stream: TMemoryStream; WriteExtra: Boolean);
begin
  // 写字段数据到内存流
  if (Stream.Size <> FSize) then  // 预设长度
    Stream.Size := FSize;
  WriteBuffers(Stream.Memory);    // 写入内存
end;

procedure TBasePack.SaveToStreamExt(Stream: TStream; WriteExtra, ClearPrams: Boolean);
var
  mStream: TMemoryStream;
begin
  // 转换变量/参数表到流（带格式描述）
  if Assigned(Stream) and (FSize > 0) then
    try
      if (Stream is TMemoryStream) then  // 内存流
        SaveToMemStream(TMemoryStream(Stream), WriteExtra)
      else begin
        // 先转换到内存流
        Stream.Position := 0;
        mStream := TMemoryStream.Create;
        try
          SaveToMemStream(mStream, WriteExtra);
          Stream.Write(mStream.Memory^, mStream.Size);
        finally
          mStream.Free;
        end;
      end;
    finally
      Stream.Position := 0;
      if ClearPrams then  // 清除变量表
        Clear;
    end;
end;

procedure TBasePack.SaveToStream(Stream: TStream; ClearParams: Boolean);
begin
  // 转换变量/参数表到流
  SaveToStreamExt(Stream, False, ClearParams); // False 不写协议值
end;

procedure TBasePack.ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal; ReadExtra: Boolean);
var
  i, iCount: Integer;
  Field: TVarField;
  VarName: AnsiString;
  Source: PListVariable;
begin
  // 分析内存流，建变量列表
  //   流格式：IntegerElementCount + Variable, Variable2...
  // Variable 格式：TStreamVariable + Name + Value | BufferSize + Buffer

  // 总长度
  FSize := ASize;

  // 变量总数
  iCount := PInteger(ABuffer)^;
  Inc(ABuffer, SizeOf(Integer));

  for i := 0 to iCount - 1 do
  begin
    // 可能传输异常致 NameSize 太大 -> 内存不足
    Source := PListVariable(ABuffer);
    if (Source^.NameSize > 128) then
    begin
      FError := True;
      Break;
    end;

    // 取变量名称: VarName
    Inc(ABuffer, STREAM_VAR_SIZE);

    SetLength(VarName, Source^.NameSize);
    System.Move(ABuffer^, VarName[1], Source^.NameSize);

    // 增加变量：默认为 etNull
    Field := TVarField.Create(VarName);
    FList.Add(Field); // inherited Add(Field);

    // 跳到变量值描述位置
    Inc(ABuffer, Source^.NameSize);
    
    // 变量值或长度: Value | BufferSize
    case Source^.EleType of
      etBoolean: begin   // 逻辑
        Field.SetAsBoolean(PBoolean(ABuffer)^);
        Inc(ABuffer, SizeOf(Boolean));
      end;
      etInteger: begin   // 整型
        Field.SetAsInteger(PInteger(ABuffer)^);
        Inc(ABuffer, SizeOf(Integer));
      end;
      etCardinal: begin  // 无符号整型
        Field.SetAsCardinal(PCardinal(ABuffer)^);
        Inc(ABuffer, SizeOf(Cardinal));
      end;
      etFloat: begin     // 浮点型
        Field.SetAsFloat(PDouble(ABuffer)^);
        Inc(ABuffer, SizeOf(Double));
      end;
      etInt64: begin     // 64 位整数
        Field.SetAsInt64(PInt64(ABuffer)^);
        Inc(ABuffer, SizeOf(Int64));
      end;
      etDateTime: begin  // 时间日期
        Field.SetAsDateTime(PDateTime(ABuffer)^);
        Inc(ABuffer, SizeOf(TDateTime));
      end;

      etBuffer,          // 自定义内存块
      etString,
      etVariant: begin   // 字符串
        Field.InterSetBuffer(Source^.EleType, ABuffer + SizeOf(Integer),
                             PInteger(ABuffer)^);
        Inc(ABuffer, SizeOf(Integer) + PInteger(ABuffer)^);
      end;

      etRecord,          // 记录
      etStream: begin    // 流
        Field.InterSetStream(Source^.EleType, ABuffer + SizeOf(Integer),
                             PInteger(ABuffer)^);
        Inc(ABuffer, SizeOf(Integer) + PInteger(ABuffer)^);      
      end;
    end;
  end;
end;

procedure TBasePack.SetAsBoolean(const Index: String; const Value: Boolean);
var
  Variable: TListVariable;
begin
  Variable.BooleanValue := Value;
  SetField(etBoolean, Index, @Variable);
end;

procedure TBasePack.SetAsBuffer(const Index: String; const Value: TMemBuffer);
var
  Variable: TListVariable;
begin
  Variable.Data := Value;
  SetField(etBuffer, Index, @Variable);
end;

procedure TBasePack.SetAsCardinal(const Index: String; const Value: Cardinal);
var
  Variable: TListVariable;
begin
  Variable.CardinalValue := Value;
  SetField(etCardinal, Index, @Variable);
end;

procedure TBasePack.SetAsDateTime(const Index: String; const Value: TDateTime);
var
  Variable: TListVariable;
begin
  Variable.DateTimeValue := Value;
  SetField(etDateTime, Index, @Variable);
end;

procedure TBasePack.SetAsDocument(const Index, Value: String);
var
  Stream: TFileStream;
  Variable: TListVariable;
begin
  // 打开、加入一个文件流（文件不能太大），用 Document[] 提取文件
  FError := True;
  if FileExists(Value) then
  begin
    // TFileStream.Handle 高版本为 THandle 
    Stream := TFileStream.Create(Value, fmOpenRead or fmShareDenyWrite);
    if (Stream.Handle <= 0) or (Stream.Size = 0) then
    begin
      Stream.Free;
      FError := False;
      Variable.Data := nil;
      SetField(etStream, Index, @Variable);
    end else
    begin
      FError := False;
      Variable.Data := Stream;
      SetField(etStream, Index, @Variable);
    end;
  end;
end;

procedure TBasePack.SetAsFloat(const Index: String; const Value: Double);
var
  Variable: TListVariable;
begin
  Variable.FloatValue := Value;
  SetField(etFloat, Index, @Variable);
end;

procedure TBasePack.SetAsInt64(const Index: String; const Value: Int64);
var
  Variable: TListVariable;
begin
  Variable.Int64Value := Value;
  SetField(etInt64, Index, @Variable);
end;

procedure TBasePack.SetAsInteger(const Index: String; const Value: Integer);
var
  Variable: TListVariable;
begin
  Variable.IntegerValue := Value;
  SetField(etInteger, Index, @Variable);
end;

procedure TBasePack.SetAsRecord(const Index: String; const Value: TBasePack);
var
  Variable: TListVariable;
begin
  Variable.Data := Value;
  SetField(etRecord, Index, @Variable);
end;

procedure TBasePack.SetAsStream(const Index: String; const Value: TStream);
var
  Variable: TListVariable;
begin
  Variable.Data := Value;
  SetField(etStream, Index, @Variable);
end;

procedure TBasePack.SetAsString(const Index, Value: String);
begin
  SetField(etString, Index, nil, Value);
end;

procedure TBasePack.SetAsVariant(const Index: String; const Value: Variant);
var
  Variable: TListVariable;
begin
  // 设置变型变量（自动压缩，Value 不适合大数据）
  case TVarData(Value).VType of
    varNull,
    varByte + varArray: begin
      FTmpVar := Value;  // 借用临时变量
      SetField(etVariant, Index, @Variable);
    end;
    varSmallInt, varInteger,
    varSingle, varShortInt,
    varWord, varLongWord:
      SetAsInteger(Index, Value);
    varDouble, varCurrency:
      SetAsFloat(Index, Value);
    varDate:
      SetAsDateTime(Index, Value);
    varBoolean:
      SetAsBoolean(Index, Value);
    varInt64:
      SetAsInt64(Index, Value);
    varString
    {$IF CompilerVersion >= 20}, varUString {$IFEND}:
      SetAsString(Index, Value);
  end;  
end;

procedure TBasePack.SetField(EleType: TElementType; const VarName: AnsiString;
                    const Value: PListVariable; const sValue: AnsiString = '');
var
  Field: TVarField;
begin
  // 加入变量

  // etBuffer、etStream、etRecord 三种变长类型，
  // 只加入引用，SaveToMemStream 时读入，Clear 时释放。

  // 变量名称合法性
  CheckFieldName(VarName);

  // 检查变量是否存在
  if FindField(VarName, Field) then
  begin
    Dec(FSize, Field.FieldSpace); // 先-后+
    Field.InterClear;  // 可能改变类型，释放空间
  end else begin  // 增加一个
    Field := TVarField.Create(UpperCase(VarName));
    FList.Add(Field);
  end;

  case EleType of
    etBoolean:  // 逻辑
      Field.SetAsBoolean(Value^.BooleanValue);
    etCardinal: // 无符号整型
      Field.SetAsCardinal(Value^.CardinalValue);
    etDateTime: // 时间日期 8 字节
      Field.SetAsDateTime(Value^.DateTimeValue);
    etFloat:    // 浮点型
      Field.SetAsFloat(Value^.FloatValue);
    etInt64:    // 64 位整数
      Field.SetAsInt64(Value^.Int64Value);
    etInteger:  // 整型
      Field.SetAsInteger(Value^.IntegerValue);
    etBuffer:   // 内存引用
      Field.SetAsBuffer(Value^.Data);
    etString:   // 字符串
      Field.SetAsString(SValue);  // 是 AnsiString
    etRecord:   // 记录引用
      Field.SetAsRecord(Value^.Data);
    etStream:   // 流引用
      Field.SetAsStream(Value^.Data);
    etVariant:  // 变长类型
      Field.SetAsVariant(FTmpVar);  // 借用临时变量
  end;

  // 转换到流的总长度 = FSize
  //     格式：IntegerElementCount + Variable, Variable2...
  // 变量格式：TStreamVariable + Name + Value | (BufferSize + Buffer)
  //   见：TBasePack.SaveToMemStream

  // 变量总数空间
  if (FSize = 0) then
    Inc(FSize, SizeOf(Integer));

  // 存储空间 +
  Inc(FSize, Field.FieldSpace);

  InterRefresh;
end;

procedure TBasePack.VarToJSON(var Buf: PAnsiChar; const VarName,
  VarValue: AnsiString; Digital: Boolean; FirstPos, EndPos: Boolean);
begin
  // 把字段保存为 JSON

  if FirstPos then  // 开始
  begin
    PDblChars(Buf)^ := AnsiString('{"');
    Inc(Buf, 2);
  end else
  begin
    PAnsiChar(Buf)^ := AnsiChar('"');
    Inc(Buf);
  end;

  // 名称
  System.Move(VarName[1], Buf^, Length(VarName));
  Inc(Buf, Length(VarName));

  // 冒号
  if Digital then  // 数字类型
  begin
    PDblChars(Buf)^ := AnsiString('":');
    Inc(Buf, 2)
  end else
  begin
    PThrChars(Buf)^ := AnsiString('":"');
    Inc(Buf, 3);
  end;

  // 值
  if (Length(VarValue) > 0) then
  begin
    System.Move(VarValue[1], Buf^, Length(VarValue));
    Inc(Buf, Length(VarValue));
  end;

  if EndPos then  // 末尾
  begin
    if Digital then  // 只加 }
    begin
      PAnsiChar(Buf)^ := AnsiChar('}');
      Inc(Buf);
    end else
    begin
      PDblChars(Buf)^ := AnsiString('"}');
      Inc(Buf, 2);
    end;
  end else
  begin
    if Digital then  // 数字类型
    begin
      PAnsiChar(Buf)^ := AnsiChar(',');
      Inc(Buf);
    end else
    begin
      PDblChars(Buf)^ := AnsiString('",');
      Inc(Buf, 2);
    end;
  end;
end;

procedure TBasePack.WriteBuffers(const Buf: Pointer);
var
  i: Integer;
  p: PAnsiChar;
  Field: TVarField;
  Target: PListVariable;
begin
  // 转换变量/参数表到带格式的内存流
  //   流格式：IntegerElementCount + Variable, Variable2...
  // 变量格式：TStreamVariable + Name + Value | (BufferSize + Buffer)

  // 元素个数
  p := PAnsiChar(Buf);

  PInteger(p)^ := FList.Count;  // 变量总数
  Inc(p, SizeOf(Integer));

  for i := 0 to FList.Count - 1 do
  begin
    // 字段
    Field := TVarField(FList.Items[i]);
    Target := PListVariable(p);

    // 类型、名称长度：TStreamVariable
    Target^.EleType := Field.FData.EleType;
    Target^.NameSize := Length(Field.FName);
    Inc(p, STREAM_VAR_SIZE);

    // 变量名称：Name: AnsiString
    System.Move(Field.FName[1], p^, Target^.NameSize);
    Inc(p, Target^.NameSize);

    // 数值及数据长度： Value | BufferSize
    case Field.FData.EleType of
      etNull:            // 空值
        { Empty } ;
      etBoolean: begin   // 逻辑
        PBoolean(p)^ := Field.FData.BooleanValue;
        Inc(p, SizeOf(Boolean));
      end;
      etInteger: begin   // 整型
        PInteger(p)^ := Field.FData.IntegerValue;
        Inc(p, SizeOf(Integer));
      end;
      etCardinal: begin  // 无符号整型
        PCardinal(p)^ := Field.FData.CardinalValue;
        Inc(p, SizeOf(Cardinal));
      end;
      etFloat: begin     // 浮点型
        PDouble(p)^ := Field.FData.FloatValue;
        Inc(p, SizeOf(Double));
      end;
      etInt64: begin     // 64 位整数
        PInt64(p)^ := Field.FData.Int64Value;
        Inc(p, SizeOf(Int64));
      end;
      etDateTime: begin  // 日期时间
        PDateTime(p)^ := Field.FData.DateTimeValue;
        Inc(p, SizeOf(TDateTime));
      end;

      // 变长类型: BufferSize + Buffer

      etBuffer,         // 自定义内存块
      etString,         // 字符串
      etVariant: begin  // 变长类型
        PInteger(p)^ := Field.FData.DataSize;
        Inc(p, SizeOf(Integer));
        if (Field.FData.DataSize > 0) then
        begin
          System.Move(Field.FData.Data^, p^, Field.FData.DataSize);
          Inc(p, Field.FData.DataSize);
        end;
      end;

      etStream,         // 流
      etRecord: begin   // 记录流
        PInteger(p)^ := Field.FData.DataSize;
        Inc(p, SizeOf(Integer));
        if (Field.FData.DataSize > 0) then
        begin
          TStream(Field.FData.Data).Position := 0;  // 开始位置
          TStream(Field.FData.Data).Read(p^, Field.FData.DataSize);
          Inc(p, Field.FData.DataSize);
        end;
      end;
    end;
  end;
end;

procedure TBasePack.Initialize(Stream: TStream; ClearIt: Boolean);
var
  mStream: TMemoryStream;
begin
  // 从流读入、解析变量表信息（要在外部释放流）
  if Assigned(Stream) then
    try
      if (FSize > 0) then
        Clear;
      FError := False;
      if (Stream is TMemoryStream) then  // 内存流
        with TMemoryStream(Stream) do
          try
            ScanBuffers(Memory, Size, False);
          finally
            if ClearIt then
              Clear;  // 清除输入内容
          end
      else begin
        Stream.Position := 0;
        mStream := TMemoryStream.Create; // 建临时流
        mStream.LoadFromStream(Stream);
        try
          ScanBuffers(mStream.Memory, mStream.Size, False);
        finally
          mStream.Free;
        end;
      end;
    except
      on E: Exception do
      begin
        FError := True;  // 异常
        iocp_log.WriteLog('TBasePack.Initialize->' + E.Message);
      end;
    end;
end;

procedure TBasePack.Initialize(APack: TBasePack);
begin
  // 复制 APack
  CopyDataPack(APack);
end;

procedure TBasePack.InterRefresh;
begin
  // 刷新（供 TCustomJSON 用）
end;

procedure TBasePack.LoadFromFile(const AFileName: String);
var
  Handle: THandle;
  FileSize, NoUsed: Cardinal;
  Stream: TMemoryStream;
begin
  // 从文件读入、解析变量表信息（文件不能太大）
  if FileExists(AFileName) then
  begin
    if (FSize > 0) then
      Clear;

    FError := False;
    Handle := InternalOpenFile(AFileName);
    FileSize := GetFileSize(Handle, nil); // 不支持大文件

    Stream := TMemoryStream.Create;
    Stream.Size := FileSize;

    try
      try
        // 读入到内存流
        ReadFile(Handle, Stream.Memory^, FileSize, NoUsed, nil);
        ScanBuffers(Stream.Memory, FileSize, True); // True 读入协议值
      finally
        CloseHandle(Handle);
        Stream.Free;
      end;
    except
      on E: Exception do
      begin
        FError := True;  // 异常
        iocp_log.WriteLog('TBasePack.Initialize->' + E.Message);
      end;
    end;
  end;
end;

{ THeaderPack }

procedure THeaderPack.CopyDataPack(APack: TBasePack);
var
  Header: THeaderPack;
begin
  inherited;
  if (APack is THeaderPack) then
  begin
    Header := THeaderPack(APack);
    FOwner := Header.FOwner;           // 所有者（组件）
    FSessionId := Header.FSessionId;   // 认证/登录 ID
    FMsgId := Header.FMsgId;           // 消息 ID
    FDataSize := Header.FDataSize;     // 变量型消息的原始长度（主体）
    FAttachSize := Header.FAttachSize; // 文件、流长度（附件）
    FOffset := Header.FOffset;         // 断点续传的位移
    FOffsetEnd := Header.FOffsetEnd;   // 断点续传的结束位移
    FCheckType := Header.FCheckType;   // 校验类型
    FVarCount := Header.FVarCount;     // 变量型消息的变量/元素个数
    FZipLevel := Header.FZipLevel;     // 主体的压缩率
    FTarget := Header.FTarget;         // 目的对象类型
    FAction := Header.FAction;         // 操作分类
    FActResult := Header.FActResult;   // 操作结果
  end;
end;

function THeaderPack.GetAttachFileName: String;
begin
  // 续传文件全名（带路径）
  Result := AsString['__AttachFileName'];
end;

function THeaderPack.GetConnection: Integer;
begin
  // 数据连接编号
  Result := AsInteger['__Connection'];
end;

function THeaderPack.GetDateTime: TDateTime;
begin
  // 消息生成时间
  Result := AsDateTime['__DateTime'];
end;

function THeaderPack.GetDirectory: String;
begin
  // 工作路径
  Result := AsString['__Directory'];
end;

function THeaderPack.GetFileName: String;
begin
  // 传输的文件名称
  Result := AsString['__FileName'];
end;

function THeaderPack.GetFileSize: TFileSize;
begin
  // 取文件大小
  Result := AsInt64['__FileSize'];
end;

function THeaderPack.GetFunctionGroup: string;
begin
  // 远程函数组
  Result := AsString['__FunctionGroup'];
end;

function THeaderPack.GetFunctionIndex: Integer;
begin
  // 远程函数编号
  Result := AsInteger['__FunctionIndex'];
end;

function THeaderPack.GetHasParams: Boolean;
begin
  // SQL 是否带参数
  Result := AsBoolean['__HasParams'];
end;

procedure THeaderPack.GetHeadMsg(Msg: PMsgHead);
begin
  // 反馈时：复制信息头内容到 Msg
  Msg^.Owner := FOwner;
  Msg^.SessionId := FSessionId;

  Msg^.MsgId := FMsgId;
  Msg^.DataSize := FDataSize;  // FSize
  Msg^.AttachSize := FAttachSize;
  Msg^.Offset := FOffset;
  Msg^.OffsetEnd := FOffsetEnd;

  Msg^.CheckType := FCheckType;
  Msg^.VarCount := FVarCount;
  Msg^.ZipLevel := FZipLevel;

  Msg^.Target := FTarget;
  Msg^.Action := FAction;
  Msg^.ActResult := FActResult;
end;

function THeaderPack.GetLocalPath: string;
begin
  // 文件存放路径
  Result := AsString['__LocalPath'];
end;

function THeaderPack.GetMsg: String;
begin
  // 消息内容
  Result := AsString['__Msg'];
end;

function THeaderPack.GetMsgSize: TFileSize;
begin
  // 取消息内容的长
  if (FSize > 0) then  // 字段已经存在
    Result := IOCP_SOCKET_SIZE + FSize
  else
    Result := IOCP_SOCKET_SIZE + FDataSize;

  if (FAction in FILE_CHUNK_ACTIONS) then
    Inc(Result, GetFileSize)
  else
    Inc(Result, FAttachSize);

  case FCheckType of
    ctMurmurHash: begin
      if (FDataSize > 0) then
        Inc(Result, HASH_CODE_SIZE);
      if (FAttachSize > 0) then
        Inc(Result, HASH_CODE_SIZE);
    end;

    ctMD5: begin
      if (FDataSize > 0) then
        Inc(Result, HASH_CODE_SIZE * 2);
      if (FAttachSize > 0) then
        Inc(Result, HASH_CODE_SIZE * 2);
    end;
  end;

end;

function THeaderPack.GetNewFileName: String;
begin
  // 新的文件名
  Result := AsString['__NewFileName'];
end;

function THeaderPack.GetPassword: String;
begin
  // 密码/口令
  Result := AsString['__Password'];
end;

function THeaderPack.GetPeerIPPort: String;
begin
  // 对应客户端的 IP:Port
  Result := AsString['__PeerIPPort'];
end;

function THeaderPack.GetReuseSessionId: Boolean;
begin
  // 是否重用凭证
  Result := AsBoolean['__ReuseSessionId'];
end;

function THeaderPack.GetRole: TClientRole;
begin
  // 角色/权限
  Result := TClientRole(AsInteger['__Role']);
end;

function THeaderPack.GetSize: TFileSize;
begin
  // 全部变量的空间大小（覆盖基类）
  if (FList.Count > 0) then
    Result := FSize
  else
    Result := FDataSize;  // 32 位
end;

function THeaderPack.GetSQL: String;
begin
  // SQL 文本内容
  Result := AsString['__SQLText'];
end;

function THeaderPack.GetSQLName: String;
begin
  // SQL 名称（服务端预设，见 iocp_sqlMgr.TInSQLManager）
  Result := AsString['__SQLName'];
end;

function THeaderPack.GetStoredProcName: String;
begin
  // 存储过程名称
  Result := AsString['__StoredProcName'];
end;

function THeaderPack.GetToUser: String;
begin
  // 目的用户名
  Result := AsString['__ToUser'];
end;

function THeaderPack.GetURL: String;
begin
  // 服务器附件的 URL
  Result := AsString['__URL'];
end;

function THeaderPack.GetUserName: String;
begin
  // 用户名、来源
  Result := AsString['__UserName'];
end;

function THeaderPack.GetUserGroup: string;
begin
  // 远程函数组
  Result := AsString['__UserGroup'];
end;

procedure THeaderPack.SaveToMemStream(Stream: TMemoryStream; WriteExtra: Boolean);
var
  Head: PMsgHead;
begin
  // 要把协议头写入
  if (WriteExtra = False) then
    inherited
  else begin
    Stream.Size := FSize + MSG_HEAD_SIZE;
    Head := PMsgHead(Stream.Memory);

    Head^.Owner := FOwner;
    Head^.SessionId := FSessionId;
    Head^.MsgId := FMsgId;

    Head^.DataSize := FDataSize;
    Head^.AttachSize := FAttachSize;
    Head^.Offset := FOffset;
    Head^.OffsetEnd := FOffsetEnd;

    Head^.CheckType := FCheckType;
    Head^.VarCount := FVarCount;
    Head^.ZipLevel := FZipLevel;

    Head^.Target := FTarget;
    Head^.Action := FAction;
    Head^.ActResult := FActResult;

    WriteBuffers(PAnsiChar(Stream.Memory) + MSG_HEAD_SIZE);
  end;
end;

procedure THeaderPack.ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal; ReadExtra: Boolean);
var
  Head: PMsgHead;
begin
  if ReadExtra then // 先读协议值
  begin
    Head := PMsgHead(ABuffer);

    FOwner := Head^.Owner;
    FSessionId := Head^.SessionId;
    FMsgId := Head^.MsgId;

    FDataSize := Head^.DataSize;
    FAttachSize := Head^.AttachSize;
    FOffset := Head^.Offset;
    FOffsetEnd := Head^.OffsetEnd;

    FCheckType := Head^.CheckType;
    FVarCount := Head^.VarCount;
    FZipLevel := Head^.ZipLevel;

    FTarget := Head^.Target;
    FAction := Head^.Action;
    FActResult := Head^.ActResult;

    Dec(ASize, MSG_HEAD_SIZE);
    Inc(ABuffer, MSG_HEAD_SIZE);
  end;
  inherited ScanBuffers(ABuffer, ASize, False);
end;

procedure THeaderPack.SetAttachFileName(const Value: String);
begin
  // 预设：续传文件全名（带路径）
  AsString['__AttachFileName'] := Value;
end;

procedure THeaderPack.SetConnection(const Value: Integer);
begin
  // 预设：数据库连接编号
  AsInteger['__Connection'] := Value;
end;

procedure THeaderPack.SetDateTime(const Value: TDateTime);
begin
  // 预设：日期时间
  AsDateTime['__DateTime'] := Value;
end;

procedure THeaderPack.SetDirectory(const Value: String);
begin
  // 预设：工作路径
  AsString['__Directory'] := Value;
end;

procedure THeaderPack.SetFileName(const Value: String);
begin
  // 预设：传输的文件名称
  AsString['__FileName'] := Value;
end;

procedure THeaderPack.SetFileSize(const Value: TFileSize);
begin
  // 预设：文件大小
  AsInt64['__FileSize'] := Value;
end;

procedure THeaderPack.SetFunctionGroup(const Value: String);
begin
  // 预设：远程函数组
  AsString['__FunctionGroup'] := Value;
end;

procedure THeaderPack.SetFunctionIndex(const Value: Integer);
begin
  // 预设：远程函数编号目的
  AsInteger['__FunctionIndex'] := Value;
end;

procedure THeaderPack.SetHasParams(const Value: Boolean);
begin
  // 预设：SQL 是否带参数
  AsBoolean['__HasParams'] := Value;
end;

procedure THeaderPack.SetHeadMsg(Msg: PMsgHead; ForReturn: Boolean);
begin
  // 接收时：复制 Msg 内容到协议头
  FError := False;   // !!!
  FOwner := Msg^.Owner;
  FSessionId := Msg^.SessionId;
  FMsgId := Msg^.MsgId;

  if ForReturn then  // 服务端返回情形
  begin
    FDataSize := 0;
    FAttachSize := 0;
    FVarCount := 0;
  end else
  begin
    FDataSize := Msg^.DataSize;
    FAttachSize := Msg^.AttachSize;
    FVarCount := Msg^.VarCount;
  end;

  FOffset := Msg^.Offset;
  FOffsetEnd := Msg^.OffsetEnd;

  FCheckType := Msg^.CheckType;
  FZipLevel := Msg^.ZipLevel;

  FTarget := Msg^.Target;
  FAction := Msg^.Action;
  FActResult := Msg^.ActResult;

  // 非法的操作
  if (FAction in ECHO_SVC_ACTIONS) then
    FAction := atUnknown;
end;

procedure THeaderPack.SetLocalPath(const Value: String);
begin
  // 预设：文件存放路径
  AsString['__LocalPath'] := Value;
end;

procedure THeaderPack.SetMsg(const Value: String);
begin
  // 预设：消息内容
  AsString['__Msg'] := Value;
end;

procedure THeaderPack.SetNewFileName(const Value: String);
begin
  // 预设：新的文件名
  AsString['__NewFileName'] := Value;
end;

procedure THeaderPack.SetPassword(const Value: String);
begin
  // 预设：密码/口令
  AsString['__Password'] := Value;
end;

procedure THeaderPack.SetPeerIPPort(const Value: String);
begin
  // 预设：对应客户端的 IP:Port
  AsString['__PeerIPPort'] := Value;
end;

procedure THeaderPack.SetReuseSessionId(const Value: Boolean);
begin
  // 预设：是否重用凭证
  AsBoolean['__ReuseSessionId'] := Value;
end;

procedure THeaderPack.SetRole(const Value: TClientRole);
begin
  // 预设：角色/权限
  AsInteger['__Role'] := Integer(Value);
end;

procedure THeaderPack.SetSQL(const Value: String);
begin
  // 预设：SQL 文本内容
  AsString['__SQLText'] := Value;
  if (UpperCase(TrimLeft(Copy(Value, 1, 7))) = 'SELECT ') then
    FAction := atDBExecQuery
  else
    FAction := atDBExecSQL;
end;

procedure THeaderPack.SetSQLName(const Value: String);
begin
  // 预设：SQL 名称（服务端预设，见 iocp_sqlMgr.TInSQLManager）
  AsString['__SQLName'] := Value;
end;

procedure THeaderPack.SetStoredProcName(const Value: String);
begin
  // 预设：存储过程名称
  AsString['__StoredProcName'] := Value;
end;

procedure THeaderPack.SetToUser(const Value: String);
begin
  // 预设：目的用户名 = TargetUser
  AsString['__ToUser'] := Value;
end;

procedure THeaderPack.SetURL(const Value: String);
begin
  // 预设：服务器附件的 URL
  AsString['__URL'] := Value;
end;

procedure THeaderPack.SetUserName(const Value: String);
begin
  // 预设：用户名、来源
  AsString['__UserName'] := Value;
end;

procedure THeaderPack.SetUserGroup(const Value: String);
begin
  // 预设：用户分组
  AsString['__UserGroup'] := Value;
end;

function THeaderPack.ToJSON: AnsiString;
const
  HEADER_SIZE = 150;  // 协议头共需 150 字节左右
  BOOL_VALUES: array[Boolean] of string = ('False', 'True');
var
  p: PAnsiChar;
  k, i: Integer;
begin
  // 把消息转换为 JSON（包含协议头）

  k := FList.Count;
  if (k = 0) then
    SetLength(Result, HEADER_SIZE)
  else begin
    if (FDataSize = 0) then
      SetLength(Result, HEADER_SIZE + Size + Size div 2)
    else
      SetLength(Result, HEADER_SIZE + FDataSize + FDataSize div 2);
  end;

  // 开始位置
  p := PAnsiChar(Result);

  // 消息格式改变，协议头的这几个字段没有用了：
  // FDataSize、FAttachSize、FCheckType、FZipLevel

  VarToJSON(p, 'Owner', IntToStr(FOwner), True, True);  // UInt64
  VarToJSON(p, 'SessionId', IntToStr(FSessionId), True);
  VarToJSON(p, 'MsgId', IntToStr(FMsgId), True);   // UInt64
  VarToJSON(p, 'VarCount', IntToStr(k + 7), True); // Count + 协议头字段数
  VarToJSON(p, 'Target', IntToStr(FTarget), True);
  VarToJSON(p, 'Action', IntToStr(Integer(FAction)), True);
  VarToJSON(p, 'ActResult', IntToStr(Integer(FActResult)), True, False, k = 0);

  if (k > 0) then
    for i := 0 to k - 1 do
      with Fields[i] do
        case VarType of
          etNull:
            VarToJSON(p, Name, 'Null', True, False, i = k - 1);
          etBoolean:
            VarToJSON(p, Name, BOOL_VALUES[AsBoolean], True, False, i = k - 1);          
          etCardinal..etInteger:
            VarToJSON(p, Name, AsString, True, False, i = k - 1);
          else
            VarToJSON(p, Name, AsString, False, False, i = k - 1);          
        end;
        
  Delete(Result, p - PAnsiChar(Result) + 1, Length(Result));

end;

procedure THeaderPack.ToRecord(var ABuffer: PAnsiChar; var ASize: Cardinal);
var
  Dest: PAnsiChar;
  Rec: PStreamVariable;
  ClearMem: Boolean;
begin
  // 把变量表转换为 TElementType.etRecord 记录
  //   格式：TStreamVariable + TMsgHead + [Buffer]
  //   TMsgHead 内含长度

  // 分配内存
  ASize := STREAM_VAR_SIZE + MSG_HEAD_SIZE + FSize;
  GetMem(ABuffer, ASize);

  // 描述：记录，名称长度=0，数据长度=FSize
  Rec := PStreamVariable(ABuffer);
  Rec^.NameSize := 0;

  // 复制协议头
  Dest := ABuffer;
  Inc(Dest, STREAM_VAR_SIZE);

  GetHeadMsg(PMsgHead(Dest));

  if (FList.Count = 0) or (FSize = 0) then  // 空值
    Rec^.EleType := etNull
  else begin  // 有数据
    Rec^.EleType := etRecord;
    ClearMem := Assigned(FMain.Memory) = False;

    if ClearMem then
      SaveToMemStream(FMain, False);  // 转到内存流

    // 复制流 Memory 到 ABuffer 末尾
    Inc(Dest, MSG_HEAD_SIZE);
    System.Move(FMain.Memory^, Dest^, FSize);

    if ClearMem then  // 还原状态
      FMain.Clear;
  end;
end;

{ TReceivePack }

procedure TReceivePack.Cancel;
begin
  if Assigned(FAttachment) then  // 释放附件
    FAttachment.Close(not (FAction in FILE_CHUNK_ACTIONS));
  Clear;
end;

procedure TReceivePack.Clear;
begin
  // 见：TServerReceiver.OwnerClear
  FActResult := arUnknown; // 取消时，重置
  if Assigned(FMain) then  // 清空，不释放
    FMain.Clear;
  if Assigned(FAttachment) then  // 释放附件
    FreeAndNil(FAttachment);
  inherited;
end;

constructor TReceivePack.Create;
begin
  inherited;
  FMain := TInMemStream.Create;  // 主体数据流
end;

procedure TReceivePack.CreateAttachment(const ALocalPath: String);
var
  NewCreated: Boolean;
  LocalFileName: String;
begin
  // 建附件的文件流，先用其他文件名，接收完毕再改名或解压

  FError := True;

  try
    // 路径+文件名称
    LocalFileName := ALocalPath + GetFileName;

    if (FileExists(LocalFileName) = False) then
      NewCreated := True
    else begin
      NewCreated := (FAction in FILE_CHUNK_ACTIONS = False); // 续传不新建
      if NewCreated then begin
        LocalFileName := LocalFileName + '_改名使用' + IntToStr(GetTickCount);
        SetFileName(ExtractFileName(LocalFileName));  // 名字改变，传给对方
      end;
    end;

    // 打开或新建
    FAttachment := TIOCPDocument.Create(LocalFileName, NewCreated);

    if (FAttachment.Handle > 0) then  // Handle 无效时 = 0
    begin
      if NewCreated then  // 新建文件
      begin
        FAttachment.SetFileInf(Self); // 设置文件属性
        FOffset := 0;  // 续传时要从新开始
      end;
      if (FAction in FILE_CHUNK_ACTIONS) then
        FAttachment.Position := FOffset;  // 写入点位移
      FError := False;
    end;
  except
    FAttachment.Free;
    FAttachment := nil;
  end;
end;

destructor TReceivePack.Destroy;
begin
  if Assigned(FMain) then
  begin
    FMain.Free;
    FMain := nil;  // 防 Clear 异常
  end;
  inherited;
end;

procedure TReceivePack.Write(AData: PPerIOData);
var
  Msg: PMsgHead;
begin
  // 把短消息写到 AData（类似于 TBaseMessage.LoadHead）
  // 格式：IOCP_HEAD_FLAG + TMsgHead + [主体原始数据]
  
  if (IOCP_SOCKET_SIZE + FSize > BROADCAST_MAX_SIZE) then
    Exit;

  // 1. 写 C/S 标志
  System.Move(IOCP_SOCKET_FLAG[1], AData^.Data.buf^, IOCP_SOCKET_FLEN);

  // 2. 写入协议头
  Msg := PMsgHead(AData^.Data.buf + IOCP_SOCKET_FLEN);
  GetHeadMsg(Msg);  // 加入

  // 3. 附件长度=0，无校验码，无压缩
  Msg^.AttachSize := 0;
  Msg^.CheckType := ctNone;
  Msg^.DataSize := FSize;
  Msg^.ZipLevel := zcNone;

  // 4. 写字段数据
  WriteBuffers(AData^.Data.buf + IOCP_SOCKET_SIZE);

  // 5. 内存数据长度
  AData^.Data.len := IOCP_SOCKET_SIZE + FSize;
  AData^.Overlapped.InternalHigh := AData^.Data.len;  // 必须

end;

{ TBaseMessage }

procedure TBaseMessage.CreateStreams(ClearList: Boolean);
var
  mStream: TStream;
begin
  // 准备要发送的数据流
  //   1. 附件：打开文件、压缩
  //   2. 主体：变量类型数据 -> 流

  FError := False;
  FVarCount := FList.Count;     // 变量个数

  // 1. 先处理附件数据流（要加入变量）

  if (FAction = atUnknown) then  // 1. 响应服务，清除附件
  begin
    FAttachFileName := '';
    InterSetAttachment(nil);
  end else

  if (FAttachFileName <> '') then  // 2. 打开文件
  begin
    if not Assigned(FAttachment) then // 断点时已打开
    begin
      OpenLocalFile;
      if FError and Assigned(FMain) then  // 异常
        FMain.Clear;
    end;
  end else

  if Assigned(FAttachment) and (FZipLevel <> zcNone) and // 3. 数据流
    (FAttachZiped = False) and not (FAction in FILE_CHUNK_ACTIONS) then  // 未压缩，不是分块传输
  begin
    FAttachZiped := True;
    mStream := TIOCPDocument.Create;
    iocp_zlib.ZCompressStream(FAttachment, mStream);
    InterSetAttachment(mStream);  // 自动释放已有的 FStream
  end;
        
  // 2. 主体数据流
  //    Variant 类型主体时 FVarCount = 0

  if (FVarCount > 0) then // 有变量类型数据
  begin
    FDataSize := FSize;   // 变量空间长度
    if (FZipLevel = zcNone) or (FAction in FILE_CHUNK_ACTIONS) then
      SaveToStream(FMain, False)  // 保留变量（不压缩）
    else begin  // 要压缩
      mStream := TMemoryStream.Create;
      try
        SaveToStream(mStream, False);  // 保留变量
        iocp_zlib.ZCompressStream(mStream, FMain);  // 压缩到 FMain
        FDataSize := FMain.Size;  // 改变
        FMain.Position := 0;
      finally
        mStream.Free;
      end;
    end;
    if ClearList then  // 清除变量
      inherited Clear;
  end;

end;

procedure TBaseMessage.AdjustTransmitRange(ChunkSize: Integer);
begin
  // 调整传输范围（断点传输）
  //   要先打开文件设 FOffset、FAttachSize
  if (FOffset + ChunkSize <= FAttachSize) then
  begin
    FAttachSize := ChunkSize;  // 每次最大上传长度
    FOffsetEnd := FOffset + ChunkSize - 1;  // 截止位移
  end else
  begin
    FOffsetEnd := FAttachSize - 1;
    Dec(FAttachSize, FOffset);
  end;
end;

procedure TBaseMessage.Clear;
begin
  //  清除资源
  //    可能未释放 FAttachment，见：TReturnResult.ReturnResult
  FVarCount := 0;
  FDataSize := 0;
  FAttachSize := 0;
  FAttachFileName := '';
  if Assigned(FAttachment) then
    FreeAndNil(FAttachment);
  inherited;
end;

constructor TBaseMessage.Create(AOwner: TObject);
begin
  inherited Create;
  FMain := TInMemStream.Create; // 主体流
  FAction := atUnknown;    // 未知操作
  FZipLevel := zcNone;     // 压缩率
  if Assigned(AOwner) then // 是客户端
  begin
    FOwner := TMessageOwner(AOwner);
    FMsgId := GetUTCTickCountEh(Self);
  end;  
end;

class procedure TBaseMessage.CreateHead(ABuf: PAnsiChar; AResult: TActionResult);
begin
  // 服务端：构造一条消息（拒绝服务、超时、被删除）
  System.Move(IOCP_SOCKET_FLAG[1], ABuf^, IOCP_SOCKET_FLEN); // C/S 标志
  with PMsgHead(ABuf + IOCP_SOCKET_FLEN)^ do
  begin
    Owner := 0;
    MsgId := 0;
    DataSize := 0;
    AttachSize := 0;
    Target := 0;
    VarCount := 0;
    Action := atServerEvent;
    ActResult := AResult;
  end;
end;

destructor TBaseMessage.Destroy;
begin
  // 释放主体流
  //   在 NilStreams 释放 FAttachment
  if Assigned(FMain) then
    FMain.Free;
  inherited;
end;

procedure TBaseMessage.GetCheckCode(AStream: TStream; ToBuf: PAnsiChar;
                                    ASize: TFileSize; var Offset: Cardinal);
begin
  // 加入主体和附件的校验码
  //   见：TBaseReceiver.GetCheckCodes
  case FCheckType of
    ctMurmurHash: begin  // MurmurHash 校验
      if (AStream is TMemoryStream) then  // 整个流
        PMurmurHash(ToBuf)^ := iocp_mmHash.MurmurHash64(TMemoryStream(AStream).Memory, ASize)
      else
      if (FAction in FILE_CHUNK_ACTIONS) then  // 续传，文件的一段
        PMurmurHash(ToBuf)^ := iocp_mmHash.MurmurHashPart64(TIOCPDocument(AStream).Handle,
                                                            FOffset, FAttachSize)
      else  // 整个文件
        PMurmurHash(ToBuf)^ := iocp_mmHash.MurmurHash64(TIOCPDocument(AStream).Handle);
      Inc(Offset, HASH_CODE_SIZE);
    end;
    ctMD5: begin  // MD5 校验
      if (AStream is TMemoryStream) then  // 整个流
        PMD5Digest(ToBuf)^ := iocp_md5.MD5Buffer(TMemoryStream(AStream).Memory, ASize)
      else
      if (FAction in FILE_CHUNK_ACTIONS) then  // 续传，文件的一段
        PMD5Digest(ToBuf)^ := iocp_md5.MD5Part(TIOCPDocument(AStream).Handle,
                                               FOffset, FAttachSize)
      else  // 整个文件
        PMD5Digest(ToBuf)^ := iocp_md5.MD5File(TIOCPDocument(AStream).Handle);
      Inc(Offset, HASH_CODE_SIZE * 2);
    end;
  end;
end;

procedure TBaseMessage.GetFileInfo(const AFileName: String);
var
  FileSize: TFileSize;
  CreationTime, AccessTime, LastWriteTime: TFileTime;
begin
  // 取文件基本信息：低32位大小（文件不能太大）、各种时间
  //   见：TIOCPDocument.SetFileInf
  GetLocalFileInf(AFileName, FileSize, CreationTime, AccessTime, LastWriteTime);
  SetFileSize(FileSize);
  AsCardinal['__creationLow'] := CreationTime.dwLowDateTime;
  AsCardinal['__creationHigh'] := CreationTime.dwHighDateTime;
  AsCardinal['__accessLow'] := AccessTime.dwLowDateTime;
  AsCardinal['__accessHigh'] := AccessTime.dwHighDateTime;
  AsCardinal['__modifyLow'] := LastWriteTime.dwLowDateTime;
  AsCardinal['__modifyHigh'] := LastWriteTime.dwHighDateTime;
end;

procedure TBaseMessage.InterSetAttachment(AStream: TStream);
begin
  // 设置附件流及长度
  if Assigned(FAttachment) then
    FAttachment.Free;
  FAttachment := AStream;
  if Assigned(FAttachment) then
  begin
    FAttachSize := FAttachment.Size;
    FAttachment.Position := 0;  // 必须
    if (FAction <> atFileUpChunk) and  // 大文件校验非常耗时，取消！
       (FAttachSize > MAX_CHECKCODE_SIZE) and (FCheckType > ctNone) then
      FCheckType := ctNone;
  end else
    FAttachSize := 0;
end;

procedure TBaseMessage.LoadHead(Data: PWsaBuf);
var
  Msg: PMsgHead; 
begin
  // 构建带协议头的消息包
  //   首包：IOCP_HEAD_FLAG + TMsgHead + [校验码 + 校验码] + [主体原始数据]
  Data^.len := IOCP_SOCKET_SIZE;  // 内容长度
  Msg := PMsgHead(Data^.buf + IOCP_SOCKET_FLEN);

  System.Move(IOCP_SOCKET_FLAG[1], Data^.buf^, IOCP_SOCKET_FLEN); // C/S 标志
  GetHeadMsg(Msg);  // 加入协议头

  // 加入校验码
  if (FCheckType > ctNone) then
  begin
    if (FDataSize > 0) then
      GetCheckCode(FMain, Data^.buf + Data^.len, FDataSize,  Data^.len);
    if (FAttachSize > 0) then
      GetCheckCode(FAttachment, Data^.buf + Data^.len, FAttachSize, Data^.len);
  end;

  // 加快数据发送：FHeader 内容不大时，一起发送，
  //   服务端收发缓存空间有限制，最大为 IO_BUFFER_SIZE
  if (FDataSize > 0) and (IO_BUFFER_SIZE >= FDataSize + Data^.len) then
  begin
    System.Move(FMain.Memory^, (Data^.buf + Data^.len)^, FDataSize);
    Inc(Data^.len, FDataSize);
    FMain.Clear;     // 清空
    FDataSize := 0;  // 清零        
  end;
end;

procedure TBaseMessage.LoadFromCDSVariant(
  const ACDSAry: array of TClientDataSet;
  const ATableNames: array of String);
var
  i, k: Integer;
begin
  // 使用方法参考 LoadFromVariant。
  if (High(ACDSAry) >= 0) then
  begin
    k := High(ATableNames);
    for i := 0 to High(ACDSAry) do
      if (i <= k) and (ATableNames[i] <> '') then  // 带数据表名称
        AsVariant[ATableNames[i]] := ACDSAry[i].Data
      else  // 无数据表名称，不更新数据表
        AsVariant['__@DATASET_' + IntToStr(i)] := ACDSAry[i].Data;
    FVarCount := FList.Count + 1;
    AsInteger['__VarCount'] := FVarCount;   // 标志字段，在后
  end;
end;

procedure TBaseMessage.LoadFromFile(const AFileName: String; ServerMode: Boolean);
begin
  // 设置要传输的文件名
  if FileExists(AFileName) then
  begin
    FError := False;
    FAttachFileName := AFileName;
    if (FZipLevel = zcNone) then
      FZipLevel := GetCompressionLevel(FAttachFileName);  // 设压缩率

    GetFileInfo(FAttachFileName);  // 文件基本消息
    SetFileName(ExtractFileName(FAttachFileName));  // 接收方建文件流用

    if ServerMode then  // 服务器端
      OpenLocalFile;  // 立刻打开，防止被删除
  end else
  begin
    FAttachFileName := '';
    FError := True;
  end;
end;

procedure TBaseMessage.LoadFromStream(AStream: TStream; AZipCompressIt: Boolean);
var
  mStream: TStream;
begin
  // 设置要传输的数据流
  //   可能现在不压缩，但通过 ZipLevel 属性设为压缩
  if Assigned(AStream) then
  begin
    if AZipCompressIt then
      FZipLevel := zcDefault;
    FAttachZiped := (FZipLevel <> zcNone);

    if (FZipLevel = zcNone) then
      InterSetAttachment(AStream)
    else
    if Assigned(AStream) then  // 压缩到文件流
    begin
      mStream := TIOCPDocument.Create;
      try
        iocp_zlib.ZCompressStream(AStream, mStream);
        InterSetAttachment(mStream);
      finally
        AStream.Free;  // 释放
      end;
    end;

    // 给个文件名
    inherited SetFileName('_stream.strm');
  end;
end;

procedure TBaseMessage.LoadFromVariant(
  const AProviders: array of TDataSetProvider;
  const ATableNames: array of String);
var
  i, k: Integer;
begin
  // 加入 Variant 数组（数据集列表），ATableNames 为数据表名称数组
  //  参数：[数据集a, 数据集b, 数据集c], ['数据表a', '数据表b', '数据表c'])
  //  1. 数据表n 是 数据集n 对应的数据表名称
  //  2. 不更新数据表时第 2 参数可设为 [] 或表名称为空
  //  3. 如果有多个数据集，第一个为主表
  if (High(AProviders) >= 0) then
  begin
    k := High(ATableNames);
    for i := 0 to High(AProviders) do
      if (i <= k) and (ATableNames[i] <> '') then  // 带数据表名称
        AsVariant[ATableNames[i]] := AProviders[i].Data
      else  // 无数据表名称，不更新数据表
        AsVariant['__@DATASET_' + IntToStr(i)] := AProviders[i].Data;
    FVarCount := FList.Count + 1;
    AsInteger['__VarCount'] := FVarCount;   // 字段数
  end;
end;

procedure TBaseMessage.NilStreams(CloseAttachment: Boolean);
begin
  // 发送完毕，清数据源
  //   C/S 模式数据发送器不自动关附件流，
  //   见：TClientParams.InternalSend、TReturnResult.ReturnResult;
  if (FSize > 0) then
    inherited Clear;
  FDataSize := 0;
  FVarCount := 0;
  FAttachSize := 0;
  FAttachFileName := '';
  if Assigned(FMain.Memory) then
    FMain.Clear;
  if Assigned(FAttachment) and CloseAttachment then
    FreeAndNil(FAttachment);
end;

procedure TBaseMessage.OpenLocalFile;
var
  mStream, mZStream: THandleStream;
begin
  // 打开要传输的文件
  mStream := TIOCPDocument.CreateEx(FAttachFileName);
  if (mStream.Handle > 0) then
  begin
    if (mStream.Size > 1021*1024*32) then  // 文件太大，不压缩
      FZipLevel := zcNone;
    if (FZipLevel = zcNone) or (FAction in FILE_CHUNK_ACTIONS) then
      InterSetAttachment(mStream)
    else begin
      // 压缩到临时文件!
      mZStream := TIOCPDocument.Create;
      try
        iocp_zlib.ZCompressStream(mStream, mZStream, zcDefault);
        InterSetAttachment(mZStream);  // 在前
        SetFileSize(FAttachSize);  // 在后，调整文件大小
      finally
        mStream.Free;  // 释放原文件
      end;
    end;
  end else
  begin
    mStream.Free;
    InterSetAttachment(nil);
    FError := True;
  end;
end;

{ TMessageWriter }

constructor TMessageWriter.Create(SurportHttp: Boolean);
begin
  inherited Create;
  FSurportHttp := SurportHttp;
  FLock := TThreadLock.Create;
end;

destructor TMessageWriter.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TMessageWriter.LoadMsg(const UserName: String; Msg: TBaseMessage);
var
  FileName, NewFileName: String;
begin
  // 读用户消息文件
  //   用户为 Params.UserName，把消息文件当作附件装入
  //   不要直接用 Msg.LoadFromFile 的方法

  // 文件在用户的消息目录
  FileName := iocp_varis.gUserDataPath + UserName + '\msg\main.msg';
  NewFileName := FileName + '_' + IntToStr(GetUTCTickCount);

  FLock.Acquire;
  try
    if FileExists(FileName) then
      RenameFile(FileName, NewFileName)  // 文件改名
    else begin
      Msg.FActResult := arMissing;
      Exit;
    end;
  finally
    FLock.Release;
  end;

  // 立刻打开文件 True
  Msg.LoadFromFile(NewFileName, True);
  Msg.FActResult := arOK;
  
end;

procedure TMessageWriter.SaveMsg(Data: PPerIOData; const ToUser: String);
var
  Count: Integer;    // 消息数
  iValue: Cardinal;
  Handle: THandle;
  Rec: TStreamVariable;
begin
  // 写消息文件
  // 把收到的数据块保存到文件，不经变量与流的转换过程，
  // 速度更快，但不能加入附件的 URL 引用
  // 文件格式：OFFLINE_MSG_FLAG + ElementCount + Record... ...

  Rec.EleType := etRecord; // 是一条记录
  Rec.NameSize := 0; // 没有记录名称

  FLock.Acquire;

  // 打开/新建消息文件（自动检查文件标志）
  // InternalOpenMsgFile 把 INVALID_HANDLE_VALUE 转为 0
  Handle := InternalOpenMsgFile(iocp_varis.gUserDataPath +
                                ToUser + '\msg\main.msg', True);

  try
    if (Handle > 0) then
    begin
      // 读消息数
      Count := 0;
      ReadFile(Handle, Count, SizeOf(Integer), iValue, nil);

      // 消息数 + 1，到 OFFLINE_MS_FLAG 后，写回
      Inc(Count);  // +

      SetFilePointer(Handle, SizeOf(Integer), nil, FILE_BEGIN);
      WriteFile(Handle, Count, SizeOf(Integer), iValue, nil);

      // 到文件末位置，写消息数据
      SetFilePointer(Handle, 0, nil, FILE_END);

      WriteFile(Handle, Rec, STREAM_VAR_SIZE, iValue, nil);   // 1.写类型
      WriteFile(Handle, (Data^.Data.buf + IOCP_SOCKET_FLEN)^, // 2.写 Data
                        Data^.Overlapped.InternalHigh - IOCP_SOCKET_FLEN,
                        iValue, nil);
    end;
  finally
    if (Handle > 0) then
      CloseHandle(Handle);
    FLock.Release;
  end;
end;

procedure TMessageWriter.SaveMsg(Msg: THeaderPack; const ToUser: String);
var
  Count: Integer;    // 消息数
  iSize, iValue: Cardinal;
  Handle: THandle;
  Buffer: PAnsiChar;
begin
  // 写消息文件
  // 文件格式：OFFLINE_MSG_FLAG + ElementCount + Record... ...

  // 存放于临时路径 gTempDirectory，接收方 Params.ToUser
  // 可转为 JSON，Msg 为 THeaderPack，可以保存 TReceivePack、TSendMessage

  if FSurportHttp and (Msg is TReceivePack) then
    if Assigned(TReceivePack(Msg).Attachment) then
    begin
      // 加入附件的 URL 引用
      // <a href="/web_site/downloads/filename.doc">FileName.doc</a>
      Msg.SetURL('<a href="' + Msg.GetURL + '">' +
          ExtractFileName(TReceivePack(Msg).Attachment.FileName) + '</a>');
    end;

  // Msg 转换为记录
  Msg.ToRecord(Buffer, iSize);

  // 锁定
  FLock.Acquire;

  // 打开/新建消息文件（自动检查文件标志）
  // 新增用户时，为每用户建一个存放数据的子目录
  // MyCreateDir(gTempDirectory + Msg.GetToUser);

  // InternalOpenMsgFile 把 INVALID_HANDLE_VALUE 转为 0
  Handle := InternalOpenMsgFile(iocp_varis.gUserDataPath +
                                ToUser + '\msg\main.msg', True);

  try
    if (Handle > 0) then
    begin
      // 读消息数
      Count := 0;
      ReadFile(Handle, Count, SizeOf(Integer), iValue, nil);

      // 消息数 + 1，到 OFFLINE_MS_FLAG 后，写回
      Inc(Count);  // +

      SetFilePointer(Handle, SizeOf(Integer), nil, FILE_BEGIN);
      WriteFile(Handle, Count, SizeOf(Integer), iValue, nil);

      // 到文件末位置，把 Buffer 写入文件
      SetFilePointer(Handle, 0, nil, FILE_END);
      WriteFile(Handle, Buffer^, iSize, iValue, nil);
    end;
  finally
    FreeMem(Buffer);
    if (Handle > 0) then
      CloseHandle(Handle);
    FLock.Release;
  end;
end;

{ TMessageReader }

procedure TMessageReader.Close;
begin
  // 关文件句柄
  FCount := 0;
  if (FHandle > 0) then
    CloseHandle(FHandle);
end;

destructor TMessageReader.Destroy;
begin
  Close;
  inherited;
end;

function TMessageReader.Extract(Msg: TReceivePack; LastMsgId: TIOCPMsgId): Boolean;
var
  Rec: TStreamVariable;
  MsgHead: TMsgHead;
  EleType, iCount: Cardinal;
  function LocateNewMessage: Boolean;
  begin
    // 读入描述、协议头
    ReadFile(FHandle, Rec, STREAM_VAR_SIZE, EleType, nil);
    ReadFile(FHandle, MsgHead, MSG_HEAD_SIZE, iCount, nil);
    if (EleType <> STREAM_VAR_SIZE) or (iCount <> MSG_HEAD_SIZE) then
    begin
      Result := False;
      Rec.EleType := etNull;
    end else begin
      Result := (LastMsgId = 0) or (MsgHead.MsgId > LastMsgId);
      if (Result = False) then  // 推进，到下一条位置
        SetFilePointer(FHandle, MsgHead.DataSize, nil, FILE_CURRENT);
    end;
  end;
var
  Buffer: PAnsiChar;
begin
  // 提取一条消息记录
  // LastMsgId 为已读的最大消息 id，读出 id 比它大的消息
  // LastMsgId = 0 -> 全部读出
  // 文件格式：OFFLINE_MSG_FLAG + ElementCount + Record... ...

  Msg.Clear;
  Msg.FAction := atUnknown;

  // 遍历查找
  while (LocateNewMessage = False) and (Rec.EleType <> etNull) do
    { 找第一条新记录：MsgId > LastMsgId } ;

  // 读入新消息
  Result := (Rec.EleType <> etNull);
  
  if Result then
  begin
    Msg.SetHeadMsg(@MsgHead);  // 协议头
    GetMem(Buffer, MsgHead.DataSize);
    try
      ReadFile(FHandle, Buffer^, MsgHead.DataSize, iCount, nil);
      if (MsgHead.DataSize = iCount) then
      begin
        Msg.ScanBuffers(Buffer, MsgHead.DataSize, False);
        Result := True;
      end;
    finally
      FreeMem(Buffer);
    end;
  end;
  
end;

procedure TMessageReader.Open(const FileName: String);
var
  iValue: Cardinal;
begin
  // 打开离线消息文件
  // InternalOpenMsgFile 把 INVALID_HANDLE_VALUE 转为 0
  FHandle := InternalOpenMsgFile(FileName);  // 会自动检查文件标志
  if (FHandle > 0) then  // 读入消息总数，后面的是消息
    ReadFile(FHandle, FCount, SizeOf(Integer), iValue, nil)
  else
    FCount := 0;
end;

end.
