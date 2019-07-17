(*
 * iocp c/s 服务基本类型常量等
 *)
unit iocp_base;

//
// 全部 record 不要用 packed 类型，读写速度更快。
//

interface

{$I in_iocp.inc}               

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, {$ELSE}
  Windows, Classes, {$ENDIF}
  iocp_Winsock2, iocp_wsExt, iocp_zlib;

type

  // 收发内存块的用途
  TIODataType = (
    ioAccept,                 // 连接
    ioReceive,                // 接收
    ioSend,                   // 发送
    {$IFDEF TRANSMIT_FILE}
    ioTransmit,               // TransmitFile 发送
    {$ENDIF}
    ioPush,                   // 推送
    ioDelete,                 // 被在线删除
    ioTimeOut,                // 超时
    ioRefuse                  // 拒绝服务
  );

  // TLinkRec.data 存放的数据类型
  TObjectType = (
    otEnvData,                // 客户端工作环境空间
    otTaskInf,                // 发送数据描述信息              
    otIOData,                 // IO 结构数据
    
    otSocket,                 // TIOCPSocket 对象
    otHttpSocket,             // THttpSocket 对象
    otStreamSocket,           // TStreamSocket 对象
    otWebSocket,              // TWebSocket 对象（保留）
    otBroker                  // 代理对象
  );

  // 双向链表 结构
  PLinkRec = ^TLinkRec;
  TLinkRec = record
    {$IFDEF DEBUG_MODE}
    No: Integer;              // 节点编号
    {$ENDIF}
    Data: Pointer;            // 存放任何类型数据
    Prev, Next: PLinkRec;     // 前后一节点
    Auto: Boolean;            // 是否由 SetLength 自动生成
    InUsed: Boolean;          // 是否占用
  end;

  // 单 IO 数据结构（见：_WSABUF）

  PPerIOData = ^TPerIOData;
  TPerIOData = record
    Overlapped: TOverlapped;  // 必须放在第一位置
    Data: TWsaBuf;            // 缓存大小、地址
    IOType: TIODataType;      // 用途分类
    Owner: TObject;           // 宿主：TBaseSocket
    Node: PLinkRec;           // 保存对应的 PLinkRec，方便回收
  end;

  // 操作类型
  //   v2.0 取消协议 TServiceType，根据操作类型确定协议，
  // 扩展操作类型时要注意范围，见：
  //   TBusiWorker.Execute、TRecvThread.HandleInMainThread

  TActionType = (
    // stEcho：响应服务
    atUnknown,                //0 未知

    // stCertify：认证服务
    atUserLogin,              // 登录
    atUserLogout,             // 断开、登出
    atUserRegister,           // 注册用户
    atUserModify,             // 修改密码
    atUserDelete,             // 删除用户
    atUserQuery,              // 查询在线客户端
    atUserState,              // 查询用户状态

    // stMessage：文本消息服务
    atTextSend,               // 文本消息 ++
    atTextPush,               // 发消息给其他客户端
    atTextBroadcast,          // 广播
    atTextGetMsg,             // 取离线消息
    atTextFileList,           // 取离线消息文件列表

    // stFile：文件服务
    atFileList,               // 列出目录、文件
    atFileSetDir,             // 设置当前目录
    atFileRename,             // 重命名文件
    atFileRenameDir,          // 重命名目录

    atFileDelete,             // 删除文件
    atFileDeleteDir,          // 删除目录
    atFileMakeDir,            // 新建目录

    atFileDownload,           // 下载文件
    atFileUpload,             // 上传文件
    atFileDownChunk,          // 下载一段文件（断点下载）
    atFileUpChunk,            // 上传一段文件（断点上传）

    atFileUpShare,            // 上传到临时路径，共享
    atFileDownShare,          // 下载临时路径的共享文件

    // stDatabase：数据库服务
    atDBGetConns,             // 查询数据库类型
    atDBConnect,              // 数据库连接
    atDBExecSQL,              // 执行 SQL
    atDBExecQuery,            // 数据库查询
    atDBExecStoredProc,       // 执行存储过程
    atDBApplyUpdates,         // Delta 更新数据库

    // stCustom：自定义
    atCallFunction,           // 调用远程函数
    atCustomAction,           // 自定义（TCustomClient 操作）

    // 服务端内部操作
    atAfterReceive,           // 接收附件完毕
    atAfterSend,              // 发送附件完毕
    atDisconnect,             // 非登出的断开
    atServerEvent             // 服务器事件
  );

  // 操作结果/状态

  TActionResult = (
    // ===== 客户端操作结果 =====
    arUnknown,                // 未定

    arOK,                     // 成功/允许
    arFail,                   // 失败
    arCancel,                 // 取消/拒绝
    
    // 对象状态
    arEmpty,                  // 内容为空
    arExists,                 // 已存在
    arMissing,                // 不存在/丢失
    arOccupied,               // 被占用
    arOutDate,                // 文件/凭证过期

    // 数据传输状态
    arAccept,                 // 接收
    arRefuse,                 // 拒绝服务
    arRequest,                // 请求

    // 用户状态/操作
    arLogout,                 // 登出 
    arOnline,                 // 在线
    arOffline,                // 离线（未登录）

    arDeleted,                // 被删除，断开
    arTimeOut,                // 超时关闭

    // ===== 异常操作结果 =====
    arErrAnalyse,             // 变量解析异常
    arErrBusy,                // 系统繁忙
    arErrHash,                // 服务端校验码错误
    arErrHashEx,              // 客户端校验码错误
    arErrInit,                // 初始化异常
    arErrNoAnswer,            // 无应答
    arErrPush,                // 推送异常
    arErrUser,                // 非法用户
    arErrWork                // 执行任务异常
  );

  // C/S 消息的数据类型
  TMessageDataType  = (
    mdtHead,                  // 消息头
    mdtEntity,                // 实体内容
    mdtAttachment             // 附近内容
  );

  // C/S 数据校验类型
  TDataCheckType  = (
    ctNone,                   // 无校验
    ctMD5,                    // MD 5
    ctMurmurHash              // MurmurHash
  );

  // C/S 消息包状态
  TMessagePackState = (
    msDefault,                // 用户提交
    msAutoPost,               // 自动提交
    msCancel                  // 取消操作
  );

  {$IFDEF WIN_64}
  IOCP_LARGE_INTEGER = Int64;   // 64 bits
  {$ELSE}
  IOCP_LARGE_INTEGER = Integer; // 32 bits
  {$ENDIF}

  {$IFDEF DELPHI_7}
  TServerSocket = Int64;
  TMessageOwner = Int64;
  TMurmurHash   = Int64;
  {$ELSE}
  TServerSocket = UInt64;
  TMessageOwner = UInt64;     // 兼容 64+32 位系统
  TMurmurHash   = UInt64;
  {$ENDIF}

  PMurmurHash   = ^TMurmurHash;
  
  TFileSize     = Int64;      // 支持大文件
  TIOCPMsgId    = Int64;      // 64 位

  TActionTarget = Cardinal;   // 操作目的对象
  TZipLevel     = TZCompressionLevel;

  // 首消息包的协议头
  //   修改时必须同时修改 THeaderPack 的对应字段

  PMsgHead = ^TMsgHead;
  TMsgHead = record
    Owner: TMessageOwner;      // 所有者（组件）
    SessionId: Cardinal;       // 认证/登录 ID
    MsgId: TIOCPMsgId;         // 消息 ID

    DataSize: Cardinal;        // 变量型消息的原始长度（主体）
    AttachSize: TFileSize;     // 文件、流长度（附件）
    Offset: TFileSize;         // 断点续传的位移
    OffsetEnd: TFileSize;      // 断点续传的结束位移

    CheckType: TDataCheckType; // 校验类型
    VarCount: Cardinal;        // 变量型消息的变量/元素个数
    ZipLevel: TZipLevel;       // 主体的压缩率

    Target: TActionTarget;     // 目的对象类型
    Action: TActionType;       // 操作分类
    ActResult: TActionResult;  // 操作结果
  end;

  // =============== WebSocket 相关 ===============

  // 操作类型
  TWSOpCode = (
    ocContinuation = 0,
    ocText         = 1,
    ocBiary        = 2,
    ocClose        = 8,
    ocPing         = 9,
    ocPong         = 10
  );

  // 消息类型
  TWSMsgType = (
    mtDefault,      // 标准 WebSocket 协议消息
    mtJSON,         // 扩展的 JSON 消息
    mtAttachment    // 扩展的附件流
  );

  // 广播的目的类型

  TBroadcastType = (
    btUnknown,      // 不广播
    btAllClient,    // 给全部客户端
    btAdminOnly     // 只给管理员
  );

  // 掩码
  PWSMask = ^TWSMask;
  TWSMask = array[0..3] of Byte;

  // 长度：WebSocket 帧结构头
  PWebSocketFrame = ^TWebSocketFrame;
  TWebSocketFrame = array[0..9] of AnsiChar; // 2+8

  PByteAry = ^TByteAry;
  TByteAry = array of Byte;

  // 存放标志字段的空间
  PInIOCPJSONField = ^TInIOCPJSONField;
  TInIOCPJSONField = array[0..18] of AnsiChar;

  // 双字节、三字节类型
  PDblChars = ^TDblChars;
  TDblChars = array[0..1] of AnsiChar;

  PThrChars = ^TThrChars;
  TThrChars = array[0..2] of AnsiChar;

  // =============== 代理服务 ===============

  // 代理模式（位置）

  TProxyType = (
    ptDefault,
    ptOuter
  );

  // 传输协议

  TTransportProtocol = (
    tpNone,
    tpHTTP
  );

  TSocketBrokerType = (
    stDefault,
    stOuterSocket,
    stWebSocket
  );

  // 内部连接标志（对应InIOCP_INNER_SOCKET）
  PInIOCPInnerSocket = ^TInIOCPInnerSocket;
  TInIOCPInnerSocket = array[0..18] of AnsiChar;

  // =============== 元素/变量/字段/参数 ===============
  // 数据流转换的描述信息

  // 元素/变量/字段/参数类型（不要调位置）

  TElementType = (             // *11
    etNull,                    // 空值
    etBoolean,                 // 逻辑
    etCardinal,                // 无符号整型
    etFloat,                   // 浮点型
    etInteger,                 // 整型
    etInt64,                   // 64 位整数
    // 以下转 JSON 时当字符串
    etDateTime,                // 时间日期 8 字节
    etBuffer,                  // 内存引用
    etString,                  // 字符串
    etRecord,                  // 记录引用
    etStream                   // 流引用
  );
  
  // 在列表存储时的信息
  PListVariable = ^TListVariable;
  TListVariable = record
    EleType: TElementType;
    NameSize: SmallInt;
    case Integer of
      0: (BooleanValue: Boolean);
      1: (IntegerValue: Integer);
      2: (CardinalValue: Cardinal);
      3: (Int64Value: Int64);
      4: (FloatValue: Double);
      5: (DateTimeValue: TDateTime);
      6: (DataSize: Integer; Data: Pointer);  // 用于文本、流等变长类型数据
  end;

  // 在传输流中的描述信息
  PStreamVariable = ^TStreamVariable;
  TStreamVariable = record
    EleType: TElementType;
    NameSize: SmallInt;
    // 紧接着：Name + DataSize + DataContent
  end;

  // =============== 任务发送 信息 ===============

  // 待发送数据信息
  // 前四字段不能改位置，与 TTransmitFileBuffers 的一致

  PTransmitTask = ^TTransmitTask;
  TTransmitTask = record
    Head: Pointer;            // 先发送的数据
    HeadLength: DWORD;
    Tail: Pointer;            // 最后发送的数据
    TailLength: DWORD;
  
    // 数据源（释放用）
    Handle: THandle;          // 文件句柄
    RefStr: AnsiString;       // 字符串
    Stream: TStream;          // 流对象，对应 Head
    Stream2: TStream;         // 流对象 2，对应 Tail

    // 长度，位移
    Size: TFileSize;          // 大小
    Offset: TFileSize;        // 开始位置
    OffsetEnd: TFileSize;     // 结束位置

    ObjType: TClass;          // 对象类
    AutoFree: Boolean;        // 自动释放流
  end;

  // =============== 客户端/业务环境 信息 ===============

  // 角色/权限
  TClientRole = (
    crUnknown = 0,            // 未登录用户
    crClient  = 1,            // 普通
    crAdmin   = 8,            // 管理员
    crSuper   = 9             // 超级管理员
  );

  // 用户基本信息
  // 双字节环境 String[n] 也是单字节

  TNameString = string[30];

  PClientInfo = ^TClientInfo;
  TClientInfo = record
    Socket: TServerSocket;     // TIOCPSocket，兼容 64+32 位系统
    Role: TClientRole;         // 角色、权限
    Name: TNameString;         // 名称
    LoginTime: TDateTime;      // 登录时间
    LogoutTime: TDateTime;     // 登出时间
    PeerIPPort: TNameString;   // IP:Port
    Tag: TNameString;          // 其他信息
  end;

  // 用户工作环境
  PEnvironmentVar = ^TEnvironmentVar;
  TEnvironmentVar = record
    BaseInf: TClientInfo;      // 基本信息
    WorkDir: string[128];      // 工作路径
    IniDirLen: Integer;        // 初始路径长度（防超边用）
    DBConnection: Integer;     // 数模连接编号
    ReuseSession: Boolean;     // 重用认证（不释放）
  end;

  // 防攻击记录
  PAttackInfo = ^TAttackInfo;
  TAttackInfo = record
    PeerIP: String[20];        // 客户端 IP
    TickCount: Int64;          // 更新的 UTC/_FILETIME 时间
    Count: Integer;            // 接入次数
  end;

  // ==================== 凭证/安全 ====================

  PCertifyNumber = ^TCertifyNumber;
  TCertifyNumber = record
    case Integer of
      0: (Session: Cardinal);
      1: (DayCount: SmallInt; Timeout: SmallInt);      
  end;

  // ==================== 线程、流量统计 ==================== 

  // 全部 工作线程 的概况结构
  PWorkThreadSummary = ^TWorkThreadSummary;
  TWorkThreadSummary = record
    ThreadCount: LongInt;      // 总数
    WorkingCount: LongInt;     // 工作数
    ActiveCount: LongInt;      // 活动数

    PackCount: LongInt;        // 处理数据包数
    PackInCount: LongInt;      // 收到数据包数
    PackOutCount: LongInt;     // 发送数据包数

    ByteCount: LongInt;        // 单位时间收发字节数
    ByteInCount: LongInt;      // 单位时间接收字节数
    ByteOutCount: LongInt;     // 单位时间发出字节数
  end;

  TWorkThreadMaxInf = record
    MaxPackIn: LongInt;        // 收到最大数据包数
    MaxPackOut: LongInt;       // 发出的最大数据包数
    MaxByteIn: LongInt;        // 每秒接收的最大字节数
    MaxByteOut: LongInt;       // 每秒发出的最大字节数
  end;

  // 单一 工作线程 的明细结构
  PWorkThreadDetail = ^TWorkThreadDetail;
  TWorkThreadDetail = record
    Working: Boolean;          // 工作状态
    Index: Integer;            // 编号

    PackCount: LongInt;        // 处理数据包
    PackInCount: LongInt;      // 收到数据包
    PackOutCount: LongInt;     // 发送数据包

    ByteCount: LongInt;        // 单位时间收发字节数
    ByteInCount: LongInt;      // 单位时间接收字节数
    ByteOutCount: LongInt;     // 单位时间发出字节数
  end;

const
  // 服务状态
  SERVER_STOPED       = $00;         // 停止
  SERVER_RUNNING      = $10;         // 运行
  SERVER_IGNORED      = $20;         // 忽略，不处理

  SOCKET_LOCK_FAIL    = 0;           // 加锁失败
  SOCKET_LOCK_OK      = 1;           // 加锁成功
  SOCKET_LOCK_CLOSE   = 2;           // 已经关闭、放弃操作

  IO_BUFFER_SIZE      = 8192;        // 服务端收发缓存长度（优化分块发送，不能大于65535，否则异常）
  IO_BUFFER_SIZE_2    = 32768;       // 客户端收发缓存长度 4096 * 8

  DEFAULT_SVC_PORT    = 12302;       // 默认端口
  MAX_CLIENT_COUNT    = 300;         // 预设客户端连接数

  INI_SESSION_ID      = 1;           // 免登录的凭证
  MAX_FILE_VAR_SIZE   = 5120000;     // 文件型变量的最大长度 5M

  SESSION_TIMEOUT     = 30;          // 短连接凭证的有效时间，30 分钟
  TIME_OUT_INTERVAL   = 180000;      // 检查死连接的时间间隔, 180 秒
  WAIT_MILLISECONDS   = 150000;       // 客户端发出数据后等待反馈的时间

  INVALID_FILE_HANDLE = 0;           // 无效的文件句柄（不用 INVALID_HANDLE_VALUE)

  // 断点传输每次最大发送长度
  //   大数值传输速度快，但竞争网络资源多
  MAX_CHUNK_SIZE     = 65536;       // 64k

  MAX_CHECKCODE_SIZE = 52800000;    // 文件大于 52.8M 时，取消校验（大文件非常耗时）！
  OFFLINE_MSG_FLAG   = 2356795438;  // 离线消息文件的开始标志

  HASH_CODE_SIZE     = SizeOf(TMurmurHash); // Hash 长度
  MSG_HEAD_SIZE      = SizeOf(TMsgHead);    // 信息头长度

  POINTER_SIZE       = SizeOf(Pointer);     // 指针长度
  NEW_TICKCOUNT      = Cardinal(not 0);     // Socket 未接收过数据的状态

  STREAM_VAR_SIZE    = SizeOf(TStreamVariable);// 传输流的变量描述长度
  CLIENT_DATA_SIZE   = SizeOf(TClientInfo);    // 客户端信息长度

  // 待发数据描述大小
  TASK_SPACE_SIZE    = SizeOf(TTransmitTask) - SizeOf(TClass) - SizeOf(Boolean);

  // Socket 地址大小
  ADDRESS_SIZE_16    = SizeOf(TSockAddr) + 16;

  // Echo 操作
  ECHO_SVC_ACTIONS   = [atUnknown, atAfterReceive, atAfterSend, atServerEvent];

  // 断点续传操作
  FILE_CHUNK_ACTIONS = [atFileDownChunk, atFileUpChunk];

  // C/S 模式标志
  IOCP_SOCKET_FLAG   = AnsiString('IOCP/2.5'#32);
  IOCP_SOCKET_FLEN   = Cardinal(Length(IOCP_SOCKET_FLAG));
  IOCP_SOCKET_SIZE   = IOCP_SOCKET_FLEN + MSG_HEAD_SIZE;

  // C/S 模式取消任务
  IOCP_SOCKET_CANCEL = AnsiString('IOCP/2.5 CANCEL');
  IOCP_CANCEL_LENGTH = DWORD(Length(IOCP_SOCKET_CANCEL));

  // 点对点、广播消息最大长度，HASH_CODE_SIZE * 4 = 2 个 MD5 长度
  BROADCAST_MAX_SIZE = IO_BUFFER_SIZE - IOCP_SOCKET_SIZE - HASH_CODE_SIZE * 4;

  // ================ webSocekt ====================

  // WebSocekt 的 MAGIC-GUID（不能修改！）
  WSOCKET_MAGIC_GUID = AnsiString('258EAFA5-E914-47DA-95CA-C5AB0DC85B11');

  // webSocekt 的操作代码
  WEBSOCKET_OPCODES  = [0, 1, 2, 8, 9, 10];

  // InIOCP 扩展 WebSocket 的 JSON 首字段，长度=19（不要改）
  // 见：TBaseJSON.SaveToStream
  INIOCP_JSON_FLAG     = AnsiString('{"_InIOCP_Ver":2.5,');
  INIOCP_JSON_FLAG_LEN = Length(INIOCP_JSON_FLAG);

  // InIOCP 扩展 WebSocket 的 JSON 次字段，长度=19（不要改）
  // 见：TBaseJSON.SaveToStream
  JSON_CHARSET_DEF   = AnsiString('"_UTF8_CHARSET":0,"');  // 默认字符集
  JSON_CHARSET_UTF8  = AnsiString('"_UTF8_CHARSET":1,"');  // UTF-8

  // ================ 代理服务 ====================

  InIOCP_INNER_SOCKET = AnsiString('InIOCP_INNER_SOCKET');

implementation

var
  _WSAResult: Integer = 1;

procedure _WSAStartup;
var
  WSAData: TWSAData;
begin
  // 初始化 Socket 环境
  _WSAResult := iocp_Winsock2.WSAStartup(WINSOCK_VERSION, WSAData);
end;

procedure _WSACleanUp;
begin
  // 清除 Socket 环境
  if (_WSAResult = 0) then
    iocp_Winsock2.WSACleanUp;
end;

initialization
  _WSAStartup;

finalization
  _WSACleanUp;

end.
