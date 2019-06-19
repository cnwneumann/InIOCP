(*
 *  接收数据的专用单元
 *
 *  C/S、WebSocket 模式数据接收器
 *)
unit iocp_receivers;

interface

{$I in_iocp.inc}        // 模式设置

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, {$ELSE}
  Windows, Classes, SysUtils, {$ENDIF}
  iocp_base, iocp_md5, iocp_mmHash,
  iocp_msgPacks, iocp_WsJSON;

type


  // ============ 数据接收器 基类 =============

  TBaseReceiver = class(TObject)
  protected
    FBuffer: PAnsiChar;         // 输入内存块
    FBufSize: Cardinal;         // 未处理的数据长度
    FRecvCount: TFileSize;      // 每消息收到的字节数
    FCancel: Boolean;           // 取消操作（保留给WS）
    FCheckPassed: Boolean;      // 校验成功（保留给WS）
    FComplete: Boolean;         // 主体或全部接收完毕
    FErrorCode: Integer;        // 异常代码
    procedure IncBufferPos(Offset: Cardinal); {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure InterInit(ACancel, AComplete: Boolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure SetComplete(const Value: Boolean); 
  public
    constructor Create;
    procedure Clear; virtual;
    procedure Prepare(const AData: PAnsiChar; const ASize: Cardinal); virtual; abstract;
    procedure Receive(const AData: PAnsiChar; const ASize: Cardinal); virtual; abstract;
    procedure Reset; virtual;   // 保留给 WS
  public
    property CheckPassed: Boolean read FCheckPassed;  
    property Complete: Boolean read FComplete write SetComplete;
    property ErrorCode: Integer read FErrorCode;    
  end;

  // ============ C/S 模式数据接收器 基类 =============

  TCSBaseReceiver = class(TBaseReceiver)
  private
    FOwner: TReceivePack;       // 接收消息包
    FCheckCode: TIOCPHashCode;  // 主体校验码
    FCheckCode2: TIOCPHashCode; // 附件校验码
    FMainLackSize: Cardinal;    // 主体欠缺的内容长度
    FAttachLackSize: TFilesize; // 附件欠缺的内容长度
    FReadHead: Boolean;         // 已经读取协议头信息
    procedure ExtractMessage;
    procedure VerifyMainStream;
    procedure VerifyAttachmentStream;
    procedure UZipPretreatMainStream;
    procedure UZipTreatAttachmentStream;
  protected
    procedure IncRecvCount(RecvCount, DataType: Cardinal); virtual; 
    procedure RecvSubqueueData; virtual;
    procedure ReceiveMainFinish; virtual;
    procedure ReceiveAttachmentFinish; virtual;
    procedure WriteMainStream(ByteCount: Cardinal); virtual;
    procedure WriteAttachmentStream(ByteCount: Cardinal); virtual;
  protected
    procedure CreateAttachment; virtual; abstract;
    procedure GetCheckCodes; virtual; abstract;
  public
    procedure Clear; override;
    procedure Reset; override;
    procedure OwnerClear;
  public
    property Cancel: Boolean read FCancel;
    property Owner: TReceivePack read FOwner;
  end;

  // ============ 服务端数据接收器 类 =============
  // 主体或附件数据接收完毕 -> 进入应用层

  TServerReceiver = class(TCSBaseReceiver)
  protected
    procedure IncRecvCount(RecvCount, DataType: Cardinal); override;
  protected
    procedure CreateAttachment; override;
    procedure GetCheckCodes; override;
  public
    constructor Create(AOwner: TReceivePack);
    procedure Prepare(const AData: PAnsiChar; const ASize: Cardinal); override;
    procedure Receive(const AData: PAnsiChar; const ASize: Cardinal); override;
  end;

  // ============ 客户端数据接收器 类 =============
  // 可能同时收到反馈消息和推送消息，一个消息接收完整后即投放到应用层，
  // 数据包混有推送消息时，可能协议头或校验码被折断。

  // 协议头空间
  TMsgHeadBuffers   = array[0..IOCP_SOCKET_SIZE - 1] of AnsiChar;

  // 校验异常事件
  TCheckErrorEvent  = procedure(Result: TReceivePack) of object;

  // 提交到应用层事件
  TPostMessageEvent = procedure(Result: TReceivePack) of object;

  // 接收消息事件
  TReceiveDataEvent = procedure(Result: TReceivePack; DataType: TMessageDataType;
                                ReceiveCount: TFileSize; AttachFinished: Boolean) of object;

  TClientReceiver = class(TCSBaseReceiver)
  private
    // 接收消息类
    FOwnerClass: TReceivePackClass;
    
    // 被折断的协议头
    FHeadBuffers: TMsgHeadBuffers;
    FHeadLackSize: Cardinal;  // 协议的缺少内容长度

    FHashCode: PAnsiChar;     // 待写校验码地址
    FCodeLackSize: Cardinal;  // 校验码的缺少长度
    FLocalPath: string;       // 附件存放路径

    FOnError: TCheckErrorEvent;    // 校验异常事件
    FOnPost: TPostMessageEvent;    // 投放消息方法
    FOnReceive: TReceiveDataEvent; // 消息接收事件

    procedure ScanRecvBuffers;
    procedure WriteHashCode(ByteCount: Cardinal);
  protected
    procedure IncRecvCount(RecvCount, DataType: Cardinal); override;
    procedure ReceiveMainFinish; override;
    procedure ReceiveAttachmentFinish; override;
    procedure WriteAttachmentStream(ByteCount: Cardinal); override;
  protected
    procedure CreateAttachment; override;
    procedure GetCheckCodes; override;
  public
    constructor Create(OwnerClass: TReceivePackClass);
    destructor Destroy; override;
    procedure PostMessage;
    procedure Prepare(const AData: PAnsiChar; const ASize: Cardinal); override;
    procedure Receive(const AData: PAnsiChar; const ASize: Cardinal); override;
  public
    property Complete: Boolean read FComplete;
    property LocalPath: String read FLocalPath write FLocalPath;
  public
    property OnError: TCheckErrorEvent read FOnError write FOnError;
    property OnPost: TPostMessageEvent read FOnPost write FOnPost;
    property OnReceive: TReceiveDataEvent read FOnReceive write FOnReceive;
  end;

  // ============ WebSocket 数据接收器 =============

{ byte: 0               1               2               3
   bit: 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
       +-+-+-+-+-------+-+-------------+-------------------------------+
       |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
       |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
       |N|V|V|V|       |S|             |   (if payload len==126/127)   |
       | |1|2|3|       |K|             |                               |
       +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
       |     Extended payload length continued, if payload len == 127  |
       + - - - - - - - - - - - - - - - +-------------------------------+
       |                               |Masking-key, if MASK set to 1  |
       +-------------------------------+-------------------------------+
       | Masking-key (continued)       |          Payload Data...      |
       +-------------------------------- - - - - - - - - - - - - - - - + }

  // 注：WebSocket 支持多帧，但单帧已经支持大数据传输，
  //    当多帧数据，且每帧很小时，增加解码的复杂性，这种情况比较少用。
  // 一块数据有多帧时：服务器只接收单帧，客户端全部接收处理。

  TWSBaseReceiver = class(TBaseReceiver)
  private
    FOwner: TObject;           // 宿主
    
    FHeader: TWebSocketFrame;  // 折断的协议头
    FHeadAddr: PAnsiChar;      // 写入 FFrameHeader 的位置
    FLackSize: Cardinal;       // 帧内容不足的长度

    FData: PAnsiChar;          // 数据开始位置
    FFrameSize: UInt64;        // 当前帧数据大小
    FFrameRecvSize: UInt64;    // 当前帧累计收到长度

    FLastFrame: Boolean;       // 最后一帧
    FOpCode: TWSOpCode;        // 操作类型
    FMsgType: TWSMsgType;      // 当前、期待的消息类型

    FMask: TWSMask;            // 掩码
    FMaskBit: PByte;           // 掩码指示位置
    FMaskExists: Boolean;      // 有否掩码

    FJSON: TBaseJSON;          // JSON（引用）
    FStream: TMemoryStream;    // 原始/JSON 数据流

    function  CheckInIOCPFlag(ABuf: PAnsiChar; ASize: Integer): TWSMsgType; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  GetContentSize(InSize: Cardinal): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  GetFrameSize(Byte2: Byte): Cardinal; {$IFDEF USE_INLINE} inline; {$ENDIF}

    procedure ExtractFrame(ABuf: PAnsiChar; ASize: Integer; RecvData: Boolean);
    procedure IncRecvCount(RecvCount: Cardinal); {$IFDEF USE_INLINE} inline; {$ENDIF}

    procedure ScanRecvWSBuffers;
  protected
    procedure InitResources(ASize: Cardinal); virtual; abstract;
    procedure InterReceiveData(ASize: Cardinal); virtual; abstract;
    procedure SaveRemainData; virtual; abstract;
    procedure WriteStream(ASize: Cardinal); virtual;abstract;
  public
    constructor Create(AOwner: TObject; AJSON: TBaseJSON);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Prepare(const AData: PAnsiChar; const ASize: Cardinal); override;
  public
    property OpCode: TWSOpCode read FOpCode;
  end;

  // ============ WebSocket 服务端数据接收器 =============

  // 非扩展的消息不保存到数据流
  
  TWSServerReceiver = class(TWSBaseReceiver)
  private
    procedure UnMarkData(ASize: Cardinal);
  protected
    procedure InitResources(ASize: Cardinal); override;
    procedure InterReceiveData(ASize: Cardinal); override;
    procedure SaveRemainData; override;
    procedure WriteStream(ASize: Cardinal); override;
  public
    procedure ClearMark(var Data: PAnsiChar; Overlapped: POverlapped);
    procedure Receive(const AData: PAnsiChar; const ASize: Cardinal); override;
  end;

  // ============ WebSocket 客户端数据接收器 =============

  TOnReceiveData   = procedure(Result: TBaseJSON; FrameSize, RecvSize: Int64) of object;

  // 准备接收附件事件
  TAttachmentEvent = procedure(Result: TBaseJSON) of object;

  // 提交到应用层事件
  TPostJSONEvent   = procedure(Result: TBaseJSON; OpCode: TWSOpCode;
                               MsgType: TWSMsgType; Stream: TMemoryStream) of object;

  // 保存非扩展的消息到数据流

  TWSClientReceiver = class(TWSBaseReceiver)
  private
    FOnAttachment: TAttachmentEvent; // 附件事件
    FOnPost: TPostJSONEvent;         // 提交事件
    FOnReceive: TOnReceiveData;      // 接收数据事件
    procedure PostJSON(AMsgType: TWSMsgType);
  protected
    procedure InitResources(ASize: Cardinal); override;
    procedure InterReceiveData(ASize: Cardinal); override;
    procedure SaveRemainData; override;
    procedure WriteStream(ASize: Cardinal); override;
  public
    destructor Destroy; override;
    procedure Receive(const AData: PAnsiChar; const ASize: Cardinal); override;
  public
    property OnAttachment: TAttachmentEvent read FOnAttachment write FOnAttachment;
    property OnPost: TPostJSONEvent read FOnPost write FOnPost;
    property OnReceive: TOnReceiveData read FOnReceive write FOnReceive;    
  end;

implementation

uses
  iocp_zlib, iocp_utils, iocp_sockets, iocp_clients, iocp_wsClients;

type
  TIOCPDocumentRef = class(TIOCPDocument);
  TResultParamsRef = class(TResultParams);
  TServerWebSocket = class(TWebSocket);

{ TBaseReceiver }

procedure TBaseReceiver.Clear;
begin
  InterInit(False, True);
end;

constructor TBaseReceiver.Create;
begin
  inherited;
  FCheckPassed := True;
  FComplete := True;
end;

procedure TBaseReceiver.IncBufferPos(Offset: Cardinal);
begin
  Inc(FBuffer, Offset);  // 内存地址前移
  Dec(FBufSize, Offset); // 未处理的数据减少
  Inc(FRecvCount, Offset);  // 总数+
end;

procedure TBaseReceiver.InterInit(ACancel, AComplete: Boolean);
begin
  FCancel := ACancel;
  FCheckPassed := True;
  FComplete := AComplete;
  FErrorCode := 0;
end;

procedure TBaseReceiver.Reset;
begin
  InterInit(True, True);
end;

procedure TBaseReceiver.SetComplete(const Value: Boolean);
begin
  InterInit(False, Value);
end;

{ TCSBaseReceiver }

procedure TCSBaseReceiver.Clear;
begin
  inherited;
  FOwner.Clear;
end;

procedure TCSBaseReceiver.ExtractMessage;
begin
  // 提取一个消息
  //   ReadHead: 要取协议信息
  //       格式：IOCP_SOCKET_FLAG + TMsgHead + [Hash + Hash] + [Data]
  // 推送消息不能带附件，自身消息的附件不在此数据包

  // 1. 取协议信息
  if FReadHead then  // 取协议信息
  begin
    FOwner.SetHeadMsg(PMsgHead(FBuffer + IOCP_SOCKET_FLEN));
    IncBufferPos(IOCP_SOCKET_SIZE);  // 到 Hash 位置
    FReadHead := False;
  end;

  // 1.1 取长度，在前
  FComplete := False;  // 未完成
  FMainLackSize := FOwner.DataSize;
  FAttachLackSize := FOwner.AttachSize;

  if (FBufSize = 0) then  // 只有协议头
    IncRecvCount(0, 1)
  else  // 1.2 取校验码
    case FOwner.CheckType of
      ctMurmurHash:
        GetCheckCodes;
      ctMD5:
        GetCheckCodes;
    end;

  // 2. 处理主体数据
  if (FMainLackSize = 0) then
    ReceiveMainFinish   // 接收完毕
  else begin
    // 准备空间
    FOwner.Main.Initialize(FMainLackSize, False);
    if (FBufSize > 0) then  // 有数据
      if (FBufSize > FMainLackSize) then // 有多个消息（服务端不会出现）
        WriteMainStream(FMainLackSize)
      else
        WriteMainStream(FBufSize);
  end;

end;

procedure TCSBaseReceiver.IncRecvCount(RecvCount, DataType: Cardinal);
begin
  // 收到一段数据，调整统计数字
  IncBufferPos(RecvCount);  // 推进
  case DataType of // 欠缺长度-
    1: Dec(FMainLackSize, RecvCount);
    2: Dec(FAttachLackSize, RecvCount);
  end;
end;

procedure TCSBaseReceiver.OwnerClear;
begin
  // 清除 FOwner 数据
  if (FMainLackSize = 0) and (FAttachLackSize = 0) then
    FOwner.Clear;
end;

procedure TCSBaseReceiver.ReceiveAttachmentFinish;
begin
  // 附件数据接收完毕
  // 1. 校验数据
  if (FOwner.CheckType > ctNone) then
    VerifyAttachmentStream
  else  // 不用校验
    FCheckPassed := True;
  // 2. 解压附件流（可能是续传）
  if FCheckPassed and Assigned(FOwner.Attachment) then
    UZipTreatAttachmentStream;   // 接收完毕
end;

procedure TCSBaseReceiver.ReceiveMainFinish;
begin
  // 主体数据接收完毕（可能只有协议头）
  // 1. 校验主体； 2. 解压、解析变量； 3. 继续写附件

  if (FOwner.DataSize = 0) then
    FCheckPassed := True
  else begin
    // 1. 校验数据
    if (FOwner.CheckType > ctNone) then
      VerifyMainStream
    else  // 不用校验
      FCheckPassed := True;

    // 2. 解压数据流、解析变量
    if FCheckPassed then
    begin
      UZipPretreatMainStream;  // 解压预处理
      if (FOwner.VarCount > 0) then  // 解析变量
        FOwner.Initialize(FOwner.Main);
    end;
  end;

  // 3. 建附件流，写内容
  if FCheckPassed and (FOwner.AttachSize > 0) then
  begin
    CreateAttachment;  // 服务端不自动建附件流
    if (FBufSize > 0) and Assigned(FOwner.Attachment) then // 写剩余内容
      if (FBufSize <= FOwner.AttachSize) then
        WriteAttachmentStream(FBufSize)
      else
        WriteAttachmentStream(FOwner.AttachSize);
  end;

end;

procedure TCSBaseReceiver.RecvSubqueueData;
begin
  // 接收后续数据

  // 1. 写主体流
  if (FMainLackSize > 0) then
    if (FBufSize <= FMainLackSize) then   // 1.1 未接收完毕
      WriteMainStream(FBufSize)
    else  // 1.2 太长，只写主体部分
      WriteMainStream(FMainLackSize);

  // 2. 写附件流
  if (FBufSize > 0) and (FAttachLackSize > 0) then
    if (FBufSize <= FAttachLackSize) then // 2.1 附件未结束
      WriteAttachmentStream(FBufSize)
    else  // 2.2 太长，只写附件部分
      WriteAttachmentStream(FAttachLackSize);

  // 可能 FBufSize > 0
end;

procedure TCSBaseReceiver.Reset;
begin
  inherited;
  // 取消，重置接收器
  FOwner.Cancel;
  FMainLackSize := 0;
  FAttachLackSize := 0;
end;

procedure TCSBaseReceiver.UZipTreatAttachmentStream;
var
  OldFileName, RealFileName: String;
  mStream: TIOCPDocument;
begin
  // 解压、处理附件
  
  // 1. 文件未压缩 -> 改为原名（可能失败）
  // 2. 已压缩，解压到新文件 -> 改为原名

  // TIOCPDocument 文件流被关闭，但不释放，可以使用 TIOCPDocument.FileName

  // 如果是续传，但未完全接收完毕，只简单关闭！
  if (FOwner.Action in FILE_CHUNK_ACTIONS) and
     (FOwner.OffsetEnd + 1 < FOwner.Attachment.OriginSize) then
  begin
    FOwner.Attachment.Close; // 只关闭，OriginSize 不变
    Exit;
  end;

  OldFileName := FOwner.Attachment.FileName;
  RealFileName := ExtractFilePath(OldFileName) + FOwner.FileName;

  if (FOwner.ZipLevel = zcNone) then
  begin
    // 直接关闭、改名
    FOwner.Attachment.Close;
    TIOCPDocumentRef(FOwner.Attachment).RenameDoc(RealFileName);
  end else
  begin
    // 先解压流到新文件，再改名
    mStream := TIOCPDocument.Create(OldFileName + '_UNZIP', True);
    try
      try
        FOwner.Attachment.Position := 0;
        iocp_zlib.ZDecompressStream(FOwner.Attachment, mStream);
      finally
        FOwner.Attachment.Close(True); // 关闭，同时删除文件
        TIOCPDocumentRef(mStream).RenameDoc(RealFileName);  // 自动关闭, 改名
        TIOCPDocumentRef(FOwner.Attachment).FFileName := RealFileName; // 直接改名
        mStream.Free;  // 释放
      end;
    except
      FErrorCode := GetLastError;
    end;
  end;

end;

procedure TCSBaseReceiver.UZipPretreatMainStream;
var
  NewBuffers: Pointer;
  NewSize: Integer;
begin
  // 解压、预处理主体流
  //   内容被解压到 NewBuffers，NewBuffers 挂到 FOwner.Main 下
  if (FOwner.ZipLevel = zcNone) then
    FOwner.Main.Position := 0
  else
    try
      try
        iocp_zlib.ZDecompress(FOwner.Main.Memory, FOwner.DataSize,
                              NewBuffers, NewSize, FOwner.DataSize);
      finally
        FOwner.Main.SetMemory(NewBuffers, NewSize);
      end;
    except
      FErrorCode := GetLastError;
    end;
end;

procedure TCSBaseReceiver.VerifyAttachmentStream;
begin
  // 检查附件校验码
  case FOwner.CheckType of
    ctMurmurHash:  // MurmurHash
      if (FOwner.Action in FILE_CHUNK_ACTIONS) then  // 检查一段范围的 Hash
        FCheckPassed := (FCheckCode2.MurmurHash =
                         iocp_mmHash.MurmurHashPart64(FOwner.Attachment.Handle,
                                                      FOwner.Offset, FOwner.AttachSize))
      else  // 检查整个文件的 Hash
        FCheckPassed := (FCheckCode2.MurmurHash =
                         iocp_mmHash.MurmurHash64(FOwner.Attachment.Handle));
    ctMD5:  // MD5
      if (FOwner.Action in FILE_CHUNK_ACTIONS) then // 检查一段范围的 MD5
        FCheckPassed := MD5MatchEx(@FCheckCode2.MD5Code,
                                   iocp_md5.MD5Part(FOwner.Attachment.Handle,
                                                    FOwner.Offset, FOwner.AttachSize))
      else  // 检查整个文件的 MD5
        FCheckPassed := MD5MatchEx(@FCheckCode2.MD5Code,
                                   iocp_md5.MD5File(FOwner.Attachment.Handle));
    else
      FCheckPassed := True;
  end;
end;

procedure TCSBaseReceiver.VerifyMainStream;
begin
  // 检查主体校验码
  case FOwner.CheckType of
    ctMurmurHash:  // MurmurHash
      FCheckPassed := (FCheckCode.MurmurHash =
                         iocp_mmHash.MurmurHash64(FOwner.Main.Memory, FOwner.DataSize));
    ctMD5:  // MD5
      FCheckPassed := MD5MatchEx(@FCheckCode.MD5Code,
                        iocp_md5.MD5Buffer(FOwner.Main.Memory, FOwner.DataSize));
    else
      FCheckPassed := True;
  end;   
end;

procedure TCSBaseReceiver.WriteAttachmentStream(ByteCount: Cardinal);
begin
  // 写数据到附件
  if Assigned(FOwner.Attachment) then
  begin
    FOwner.Attachment.Write(FBuffer^, ByteCount);
    IncRecvCount(ByteCount, 2); 
    if FComplete then  // 接收完毕 -> 校验、解压
      ReceiveAttachmentFinish;
  end else
    IncRecvCount(ByteCount, 2);
end;

procedure TCSBaseReceiver.WriteMainStream(ByteCount: Cardinal);
begin
  // 写数据到主体（先写入，后推进）
  FOwner.Main.Write(FBuffer^, ByteCount);
  IncRecvCount(ByteCount, 1);  // 推进
  if (FMainLackSize = 0) then  // 完毕
    ReceiveMainFinish;
end;

{ TServerReceiver }

constructor TServerReceiver.Create(AOwner: TReceivePack);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TServerReceiver.CreateAttachment;
begin
  // 在应用层建附件
end;

procedure TServerReceiver.GetCheckCodes;
begin
  // 取校验码：
  //   格式：IOCP_HEAD_FLAG + TMsgHead + [校验码 + 校验码] + [主体数据]
  //     见：TBaseMessage.GetCheckCode
  case FOwner.CheckType of
    ctMurmurHash: begin  // MurmurHash=64位
      if (FOwner.DataSize > 0) then   // 主体校验码
      begin
        FCheckCode.MurmurHash := PMurmurHash(FBuffer)^;
        IncBufferPos(HASH_CODE_SIZE);
      end;
      if (FOwner.AttachSize > 0) then // 附件校验码
      begin
        FCheckCode2.MurmurHash := PMurmurHash(FBuffer)^;
        IncBufferPos(HASH_CODE_SIZE);
      end;
    end;
    ctMD5: begin  // MD5=128位
      if (FOwner.DataSize > 0) then   // 主体校验码
      begin
        FCheckCode.MD5Code := PMD5Digest(FBuffer)^;
        IncBufferPos(HASH_CODE_SIZE * 2);
      end;
      if (FOwner.AttachSize > 0) then // 附件校验码
      begin
        FCheckCode2.MD5Code := PMD5Digest(FBuffer)^;
        IncBufferPos(HASH_CODE_SIZE * 2);
      end;
    end;
  end;
end;

procedure TServerReceiver.IncRecvCount(RecvCount, DataType: Cardinal);
begin
  inherited;
  // 主体完成 或 附件完成 均进入应用层
  case DataType of
    1: FComplete := (FMainLackSize = 0);
    2: FComplete := (FAttachLackSize = 0);
  end;
end;

procedure TServerReceiver.Prepare(const AData: PAnsiChar; const ASize: Cardinal);
begin
  // 接收首数据包，准备资源
  //   只有一个主体，附件数据不在当前数据包!
  FBuffer := AData;
  FBufSize := ASize;

  FCancel := False;
  FReadHead := False; // 调用前已读取协议信息
  IncBufferPos(IOCP_SOCKET_SIZE);  // 跳到内容处

  ExtractMessage;  // 提取信息
end;

procedure TServerReceiver.Receive(const AData: PAnsiChar; const ASize: Cardinal);
begin
  // 接收后续数据
  //  客户端发送附件时要请求，主体和附件数据不会混在一起
  FBuffer := AData;
  FBufSize := ASize;
  if (ASize = IOCP_CANCEL_LENGTH) and MatchSocketType(FBuffer, IOCP_SOCKET_CANCEL) then
    Reset
  else
    RecvSubqueueData;
end;

{ TClientReceiver }

constructor TClientReceiver.Create(OwnerClass: TReceivePackClass);
begin
  inherited Create;
  FOwnerClass := OwnerClass;   // 是 TResultParams
  FOwner := OwnerClass.Create; // 先建一个
end;

procedure TClientReceiver.CreateAttachment;
begin
  // 自动建附件流
  TResultParamsRef(FOwner).CreateAttachment(FLocalPath);
end;

destructor TClientReceiver.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Free;
  inherited;
end;

procedure TClientReceiver.GetCheckCodes;
  procedure WriteMurmurHash(Hash: PIOCPHashCode);
  begin
    if (FBufSize >= HASH_CODE_SIZE) then  // 内容够长
    begin
      Hash^.MurmurHash := PMurmurHash(FBuffer)^;
      IncBufferPos(HASH_CODE_SIZE);
    end else
    begin
      // 内容不够长，被折断！
      FCodeLackSize := HASH_CODE_SIZE; // 不足字节数
      FHashCode := @Hash^.MurmurHash;  // 设置写校验码的地址
      if (FBufSize > 0) then
        WriteHashCode(FBufSize);
    end;
  end;
  procedure WriteMD5(MD5: PIOCPHashCode);
  begin
    if (FBufSize >= HASH_CODE_SIZE * 2) then // 内容够长
    begin
      MD5^.MD5Code := PMD5Digest(FBuffer)^;
      IncBufferPos(HASH_CODE_SIZE * 2);
    end else
    begin
      // 内容不够长，被折断！
      FCodeLackSize := HASH_CODE_SIZE * 2;
      FHashCode := @MD5^.MD5Code;
      if (FBufSize > 0) then
        WriteHashCode(FBufSize);
    end;
  end;
begin
  // 取校验码：
  //   接收自身消息和广播消息，可能校验码被折断
  if (FBufSize > 0) then
    case FOwner.CheckType of
      ctMurmurHash: begin  // MurmurHash=64位
        if (FOwner.DataSize > 0) then    // 主体校验码
          WriteMurmurHash(@FCheckCode);
        if (FBufSize > 0) and (FOwner.AttachSize > 0) then  // 附件校验码
          WriteMurmurHash(@FCheckCode2);
      end;
      ctMD5: begin  // MD5=128位
        if (FOwner.DataSize > 0) then    // 主体校验码
          WriteMD5(@FCheckCode);
        if (FBufSize > 0) and (FOwner.AttachSize > 0) then  // 附件校验码
          WriteMD5(@FCheckCode2);
      end;
    end;
end;

procedure TClientReceiver.IncRecvCount(RecvCount, DataType: Cardinal);
begin
  inherited;
  // 接收数增加，主体+附件全接收完毕 -> 应用层
  FComplete := (FMainLackSize = 0) and (FAttachLackSize = 0);
end;

procedure TClientReceiver.PostMessage;
begin
  // 把消息加入投放线程
  try
    FOnPost(FOwner);
  finally
    FReadHead := True;  // 要重新读协议头了
    FRecvCount := 0;    // 接收长度清零
    FOwner := FOwnerClass.Create; // 建新主体
  end;
{  FOwner.Free;  // debug
  FOwner := FOwnerClass.Create; // 建新主体  }
end;

procedure TClientReceiver.Prepare(const AData: PAnsiChar; const ASize: Cardinal);
begin
  FBuffer := AData;
  FBufSize := ASize;

  FCancel := False;
  FHeadLackSize := 0;  // 协议头未折断
  FHashCode := nil;    // 校验码未折断
  FReadHead := True;   // 取协议信息情况

  ScanRecvBuffers;     // 扫描提取消息
end;

procedure TClientReceiver.Receive(const AData: PAnsiChar; const ASize: Cardinal);
begin
  // 接收后续数据，保存到流
  //   可能同时包含主体和附件数据，也可能包含推送消息
  FBuffer := AData;
  FBufSize := ASize;

  // 1. 检查推送消息的折断
  //    1.1 协议头被折断；1.2 校验码被折断

  if (FHeadLackSize > 0) then  // 1.1 协议头被折断
  begin
    if (FBufSize >= FHeadLackSize) then
    begin
      // 内容够长，准备协议头信息
      System.Move(AData^, FHeadBuffers[IOCP_SOCKET_SIZE - FHeadLackSize], FHeadLackSize);

      FOwner.SetHeadMsg(PMsgHead(@FHeadBuffers[IOCP_SOCKET_FLEN]));
      IncBufferPos(FHeadLackSize);  // 到 Hash 位置

      FReadHead := False;  // 不用再次读协议头
      FHeadLackSize := 0;

      ScanRecvBuffers;  // 扫描、提取消息
    end else
    begin
      // 内容不够长
      System.Move(AData^, FHeadBuffers[IOCP_SOCKET_SIZE - FHeadLackSize], FBufSize);
      Dec(FHeadLackSize, FBufSize);  // 先减
      IncBufferPos(FBufSize); // 推进 -> FBufSize = 0
    end;
  end else
  if Assigned(FHashCode) then  // 1.2 校验码被折断
  begin
    if (FBufSize >= FCodeLackSize) then
      WriteHashCode(FCodeLackSize)
    else
      WriteHashCode(FBufSize);
  end;

  // 2. 协议头 + 校验码处理完毕
  if (FHeadLackSize = 0) and (FHashCode = nil) then
  begin
    // 2.1 接收后继或附件数据
    if (FBufSize > 0) then
      RecvSubqueueData;
    // 2.2 继续提取消息
    if (FBufSize > 0) then
      ScanRecvBuffers;
  end;
end;

procedure TClientReceiver.ReceiveAttachmentFinish;
begin
  inherited;
  try
    if (FCheckPassed = False) and Assigned(FOnError) then
      FOnError(FOwner); // 校验异常 -> 清空
  finally
    PostMessage;  // 全部接收完毕，投放
  end;
end;

procedure TClientReceiver.ReceiveMainFinish;
begin
  inherited;
  // 接收完主体数据、解释变量后才调用 FOnReceive
  try
    if (FCheckPassed = False) and Assigned(FOnError) then
      FOnError(FOwner)  // 校验异常 -> 清空
    else
    if Assigned(FOnReceive) then  // 调用接收事件
      FOnReceive(FOwner, mdtEntity, FRecvCount, FAttachLackSize = 0);
  finally
    if (FCheckPassed = False) or (FOwner.AttachSize = 0) then
      PostMessage;  // 当作接收完毕，投放
  end;
end;

procedure TClientReceiver.ScanRecvBuffers;
begin
  // 接收首数据包，准备资源
  //   格式：IOCP_SOCKET_FLAG + TMsgHead + [Hash + Hash] + [Data]
  //   1. 客户端可能有多个消息（推送来的），要分解！
  //   2. 多消息时，协议头可能被折断

  // 逐个提取消息
  while (FReadHead = False) and (FBufSize > 0) or (FBufSize >= IOCP_SOCKET_SIZE) do
    ExtractMessage; 

  // 还有剩余，应小于 IOCP_SOCKET_SIZE
  if (FBufSize > 0) then
  begin
    // 剩余长度 < 协议头长度, 被折断！
    // 保存剩余内容到 FHeadBuffers，下次拼接起来
    FComplete := False;
    FHeadLackSize := IOCP_SOCKET_SIZE - FBufSize;  // 缺少字节数
    System.Move(FBuffer^, FHeadBuffers[0], FBufSize);
    IncBufferPos(FBufSize);
  end;
end;

procedure TClientReceiver.WriteAttachmentStream(ByteCount: Cardinal);
begin
  inherited; // ByteCount：当前收到字节数
  if Assigned(FOnReceive) then  // 调用接收事件
    FOnReceive(FOwner, mdtAttachment, FRecvCount, FAttachLackSize = 0);
end;

procedure TClientReceiver.WriteHashCode(ByteCount: Cardinal);
begin
  // 校验码被折断，先保存收到的部分
  System.Move(FBuffer^, FHashCode^, ByteCount);
  IncBufferPos(ByteCount);     // 推进
  Inc(FHashCode, ByteCount);   // 推进，下次写的位置
  Dec(FCodeLackSize, ByteCount);  // 不足字节数-
  if (FCodeLackSize = 0) then  // 写完校验码
    FHashCode := nil;
end;

{ TWSBaseReceiver }

function TWSBaseReceiver.CheckInIOCPFlag(ABuf: PAnsiChar; ASize: Integer): TWSMsgType;
begin
  // 检查扩展的 INIOCP_JSON_FLAG
  if (ABuf = nil) then
    Result := mtDefault
  else
  if (ASize >= INIOCP_JSON_FLAG_LEN) and
     (PInIOCPJSONField(ABuf)^ = INIOCP_JSON_FLAG) then
    Result := mtJSON
  else
    Result := mtDefault;
end;

procedure TWSBaseReceiver.Clear;
begin
  // 清除收到的信息 
  FData := nil;
  FFrameSize := 0;
  FFrameRecvSize := 0;
  if Assigned(FStream) and (FStream.Size > 0) then
    FStream.Clear;
  inherited;
end;

constructor TWSBaseReceiver.Create(AOwner: TObject; AJSON: TBaseJSON);
begin
  inherited Create;
  FOwner := AOwner;
  FJSON := AJSON;
end;

destructor TWSBaseReceiver.Destroy;
begin
  Clear;
  if Assigned(FStream) then
    FStream.Free;
  inherited;
end;

procedure TWSBaseReceiver.ExtractFrame(ABuf: PAnsiChar; ASize: Integer; RecvData: Boolean);
var
  i: Integer;
  iByte: Byte;
  OffSet: Integer;

  procedure _MovePos(Step: Integer);
  begin
    Inc(ABuf, Step);
    Dec(ASize, Step);  // 可能 < 0
    Inc(OffSet, Step);
  end;

begin
  // 从帧结构取信息
  // 必须避免帧描述长度不足 ASize >= 2

  OffSet := 0;
  FFrameRecvSize := 0;  // 当前帧接收量=0

  // 1. 第 1 字节

  // 是否为末帧
  iByte := PByte(ABuf)^;
  FLastFrame := (iByte shr 7 > 0);

  // 不处理保留的数据位 RSV1、RSV2、RSV3

  // 取操作代码
  iByte := (iByte and $0F);
  if (iByte in WEBSOCKET_OPCODES) then
    FOpCode := TWSOpCode(iByte)
  else
    FOpCode := ocClose;  // 操作异常，关闭！

  // 2. 第 2 字节
  _MovePos(1);

  // 掩码位置
  FMaskBit := PByte(ABuf);
  iByte := PByte(ABuf)^;

  // 是否带掩码：高位，客户端无
  FMaskExists := (iByte shr 7 > 0);

  // 帧长度：低7位
  FFrameSize := (iByte and $7F);

  // 3. 取帧长度
  _MovePos(1);     // 到第 3 字节

  case FFrameSize of
    126: begin     // <= max(UInt16)，前2字节 -> 长度
      FFrameSize := 0;
      if (ASize >= 2) then
      begin
        TByteAry(@FFrameSize)[1] := TByteAry(ABuf)[0];
        TByteAry(@FFrameSize)[0] := TByteAry(ABuf)[1];
      end;
      _MovePos(2); // 跳过
    end;
    127: begin     // <= max(UInt64)，长前8字节 -> 长度
      FFrameSize := 0;
      if (ASize >= 8) then
        for i := 0 to 7 do
          TByteAry(@FFrameSize)[7 - i] := TByteAry(ABuf)[i];
      _MovePos(8); // 跳过
    end;
  end;

  // 4. 取掩码（第4字节，客户端无）
  if FMaskExists then  // 服务端
  begin
    if (ASize >= 4) then
      FMask := PWSMask(ABuf)^;
    _MovePos(4);   // 跳过
  end;

  // 5. 保存数据
  //    ASize = 0 -> 描述刚结束
  //    ASize > 0 -> 后面带数据
  if RecvData and (ASize >= 0) then
  begin
    IncBufferPos(OffSet);  // 移动 FBuffer
    InterReceiveData(ASize);  // 接收数据
  end;

end;

function TWSBaseReceiver.GetContentSize(InSize: Cardinal): Integer;
begin
  // 判断要写入流的长度
  if (InSize = 0) then
    Result := 0
  else
  if (FFrameRecvSize + InSize <= FFrameSize) then
    Result := InSize
  else
    Result := FFrameSize - FFrameRecvSize;
end;

function TWSBaseReceiver.GetFrameSize(Byte2: Byte): Cardinal;
begin
  // 检查帧描述的长度
  //   客户端：Close/Ping/Pong 的描述 2 字节（最小值）
  case (Byte2 and $7F) of
    126: Result := 4;
    127: Result := 10;
    else Result := 2;
  end;
end;

procedure TWSBaseReceiver.IncRecvCount(RecvCount: Cardinal);
begin
  // 接收长度+
  Inc(FFrameRecvSize, RecvCount);
  // 是否接收完毕
  FComplete := FLastFrame and (FFrameRecvSize = FFrameSize);
end;

procedure TWSBaseReceiver.Prepare(const AData: PAnsiChar; const ASize: Cardinal);
begin
  FBuffer := AData;
  FBufSize := ASize;
  ScanRecvWSBuffers;  // 扫描接收
end;

procedure TWSBaseReceiver.ScanRecvWSBuffers;
begin
  // 接收首数据包
  //   1. 接收快时可能有多个消息，要分解！
  //   2. 多消息时，协议头可能被折断

  if Assigned(FHeadAddr) then   // 从拼接内容取帧信息
  begin
    ExtractFrame(@FHeader[0], FHeadAddr - PAnsiChar(@FHeader[0]), False);  // 内容刚够长
    InterReceiveData(FBufSize); // 接收前帧数据，自动取 GetContentSize(FBufSize)
    FHeadAddr := nil;
  end;
     
  while (FBufSize >= 2) and (FBufSize >= GetFrameSize(Byte((FBuffer + 1)^))) do
    ExtractFrame(FBuffer, FBufSize, True);

  if (FBufSize > 0) then  // 是下一消息的内容
    SaveRemainData;
end;

{ TWSServerReceiver }

procedure TWSServerReceiver.ClearMark(var Data: PAnsiChar; Overlapped: POverlapped);
var
  i: Integer;
  Buf: PAnsiChar;
begin
  // 修改消息，以便发回给客户端或广播
  //  清除掩码指示、掩码，内容前移
  if FMaskExists and Assigned(FMaskBit) then
  begin
    FData := Data; // Data 是 TWSData.Data
    Buf := PAnsiChar(FData - 4);
    for i := 1 to FFrameRecvSize do
    begin
      Buf^ := FData^;
      Inc(Buf); Inc(FData);
    end;

    // 清除掩码标志
    FMaskBit^ := FMaskBit^ and Byte($7F);
    FMaskBit := nil;

    Dec(Overlapped^.InternalHigh, 4);
    Dec(Data, 4);    
  end;
end;

procedure TWSServerReceiver.InitResources(ASize: Cardinal);
begin
  // 初始化资源：
  //  JSON 标志、数据流、更新 Socket 属性

  FData := FBuffer;  // 数据开始位置
  if (FOpCode = ocText) then  // 检查 JSON 标志，见：TBaseJSON.SaveToStream
    FMsgType := CheckInIOCPFlag(FBuffer, ASize);

  case FMsgType of
    mtDefault:      // 不用设置流
      TServerWebSocket(FOwner).SetProps(FOpCode, mtDefault, Pointer(FData),
                                        FFrameSize, ASize);

    mtJSON: begin   // JSON 消息
      if (Assigned(FStream) = False) then
        FStream := TMemoryStream.Create;
      FStream.Size := FFrameSize; // 有后续帧时写入时自动扩大
      TServerWebSocket(FOwner).SetProps(FOpCode, mtJSON,
                                        Pointer(FData), 0, 0);
    end;
    
    mtAttachment:
      TServerWebSocket(FOwner).SetProps(FOpCode, mtAttachment,
                                        Pointer(FData), 0, 0);
  end;
end;

procedure TWSServerReceiver.InterReceiveData(ASize: Cardinal);
var
  RecvCount: Cardinal;
begin
  // 准备接收数据
  RecvCount := GetContentSize(ASize);  // 取有效长度
  try
    // 带掩码 -> 解码
    if (RecvCount > 0) then
      UnMarkData(RecvCount);
    // 准备资源
    InitResources(RecvCount);
    // 保存到流
    if (RecvCount > 0) then
      WriteStream(RecvCount);
  finally
    // 最后推进
    if (RecvCount > 0) then
      IncBufferPos(RecvCount);
  end;
end;

procedure TWSServerReceiver.Receive(const AData: PAnsiChar; const ASize: Cardinal);
var
  RecvCount: Integer;
begin
  // 接收后续数据

  FBuffer := AData;
  FBufSize := ASize;
  FData := FBuffer;  // 数据开始位置

  // 1. 处理当前帧的接收
  if (FFrameRecvSize < FFrameSize) then  // FLackSize > 0 时  FFrameSize = 0
  begin
    RecvCount := GetContentSize(FBufSize);
    UnMarkData(RecvCount);   // 解码（服务端）
    WriteStream(RecvCount);  // 保存到流
    IncBufferPos(RecvCount); // 前移
  end;

  // 2. 剩余的内容是下一消息的
  if (FBufSize > 0) then
    SaveRemainData;
    
end;

procedure TWSServerReceiver.SaveRemainData;
begin
  // 保存剩余内容，下次接收（保留）
  // 客户端发送完毕时稍微停顿，避免有剩余
end;

procedure TWSServerReceiver.UnMarkData(ASize: Cardinal);
var
  i: Integer;
  p: PByte;
begin
  // 带掩码 -> xor 解码
  if (ASize > 0) and FMaskExists then
  begin
    p := PByte(FBuffer);
    for i := FFrameRecvSize to FFrameRecvSize + ASize - 1 do
    begin
      p^ := p^ xor FMask[i mod 4];
      Inc(p);
    end;
  end;
end;

procedure TWSServerReceiver.WriteStream(ASize: Cardinal);
begin
  // 保存数据到流

  // 增加接收数
  IncRecvCount(ASize);

  // 1. 写数据流
  case FMsgType of
    mtDefault:     // 更新 mtDefault 的消息长度
      Inc(TServerWebSocket(FOwner).FMsgSize, ASize);
    mtJSON:        // 扩展的 JSON 消息
      FStream.Write(FBuffer^, ASize);
    mtAttachment:  // 附件流
      if Assigned(FJSON.Attachment) then
        FJSON.Attachment.Write(FBuffer^, ASize);
  end;

  // 2. 接收完毕
  if FComplete then
    case FMsgType of
      mtJSON: begin  // JSON 消息
        FJSON.Initialize(FStream, True);  // 转换 JSON, 同时清除 FStream
        if FJSON.HasAttachment then // 带附件
          FMsgType := mtAttachment  // 下次为附件流
        else
          FMsgType := mtDefault;
      end;
      mtAttachment:  // 附件流
        FMsgType := mtDefault;  // 下次为 mtDefault
    end;
    
end;     

{ TWSClientReceiver }

destructor TWSClientReceiver.Destroy;
begin
  if Assigned(FJSON) then
    FJSON.Free;
  inherited;
end;

procedure TWSClientReceiver.InitResources(ASize: Cardinal);
begin
  // 准备数据流
  //   JSON、非扩展的数据均保存到数据流
  if (Assigned(FStream) = False) then
    FStream := TMemoryStream.Create;
end;

procedure TWSClientReceiver.InterReceiveData(ASize: Cardinal);
var
  RecvCount: Cardinal;          
begin
  // 准备接收数据
  RecvCount := GetContentSize(ASize);  // 取有效长度
  try
    // 准备资源
    InitResources(RecvCount);
    // 保存到流
    WriteStream(RecvCount);
  finally
    // 最后推进
    if (RecvCount > 0) then
      IncBufferPos(RecvCount);
  end;
end;

procedure TWSClientReceiver.PostJSON(AMsgType: TWSMsgType);
begin
  // 投放 JSON 消息
  try
    if (AMsgType = mtDefault) then  // 投放流
      FOnPost(FJSON, FOpCode, AMsgType, FStream)
    else
      FOnPost(FJSON, FOpCode, AMsgType, nil);
  finally
    if (AMsgType = mtDefault) then
      FStream := TMemoryStream.Create;
    FJSON := TJSONResult.Create(FOwner);
  end;
end;

procedure TWSClientReceiver.Receive(const AData: PAnsiChar; const ASize: Cardinal);
var
  RecvCount: Integer;
begin
  // 接收后续数据

  FBuffer := AData;
  FBufSize := ASize;

  // 1. 处理当前帧的接收
  if (FFrameRecvSize < FFrameSize) then  // FLackSize > 0 时  FFrameSize = 0
  begin
    RecvCount := GetContentSize(FBufSize);
    WriteStream(RecvCount);  // 保存到流
    IncBufferPos(RecvCount); // 前移
  end;

  // 2. 处理剩余数据

  // 2.1 处理帧描述折断问题
  // 特殊：客户端接收到推算消息时，帧描述会被折断分块

  if (FBufSize > 0) and (FLackSize > 0) then
  begin
    if (FLackSize = 9) then  // 转义，填帧的第二字节
    begin
      FHeader[1] := FBuffer^;
      FLackSize := GetFrameSize(Byte(FHeader[1])) - 2;  // 重取
      IncBufferPos(1);
    end;

    // 可能 ocClose、ocPing、ocPong -> RecvCount = 0
    if (FBufSize < FLackSize) then
      RecvCount := FBufSize
    else
      RecvCount := FLackSize;

    if (RecvCount > 0) then
    begin
      System.Move(FBuffer^, FHeadAddr^, RecvCount);
      IncBufferPos(RecvCount);
      Inc(FHeadAddr, RecvCount);
      Dec(FLackSize, RecvCount);
    end;
  end;

  // 3. 帧描述完整，继续提取数据
  if (FLackSize = 0) then
    ScanRecvWSBuffers;

end;

procedure TWSClientReceiver.SaveRemainData;
var
  i: Integer;
begin
  // 描述长度至少 2 字节（ocClose、ocPing、ocPong）
  // 不足时保存内容到 FHeader，下次拼接

  if (FBufSize = 1) then
  begin
    FHeader[0] := FBuffer^;
    FHeadAddr := @FHeader[2];  // 第3字节位置，见 TWSClientReceiver.Receive
    FLackSize := 9;  // 转义， 少第二字节
  end else
  begin
    for i := 0 to FBufSize - 1 do
      FHeader[i] := (FBuffer + i)^;
    // 不足的长度
    FLackSize := GetFrameSize(Byte(FHeader[1])) - FBufSize;
    if (FLackSize > 0) then
      FHeadAddr := @FHeader[FBufSize]
    else begin  // 帧描述完整，仅取信息
      ExtractFrame(PAnsiChar(@FHeader[0]), FBufSize, False);
      FComplete := FFrameSize = 0;  // ocClose、ocPing、 ocPong
    end;
  end;

  if (FLackSize > 0) then
  begin
    FFrameSize := 0;
    FFrameRecvSize := 0;
    FComplete := False;
  end;

  FBufSize := 0;
    
end;

procedure TWSClientReceiver.WriteStream(ASize: Cardinal);
begin
  // 1. 增加接收数
  IncRecvCount(ASize);

  // 2. 保存到流（FMsgType未知）
  if (ASize > 0) then
    if (FOpCode = ocText) then  // 文本消息（未知、InIOCP-JSON）
      FStream.Write(FBuffer^, ASize)
    else
    if Assigned(FJSON.Attachment) then  // 附件
    begin
      FJSON.Attachment.Write(FBuffer^, ASize);
      FOnReceive(FJSON, FFrameSize, ASize);  // 附件的接收进程
    end;

  // 5. 接收完毕
  if FComplete then
  begin
    // 检查数据类型
    if (FOpCode = ocBiary) then
      FMsgType := mtAttachment
    else
      FMsgType := CheckInIOCPFlag(PAnsiChar(FStream.Memory), FStream.Size);

    // 清零
    FFrameSize := 0;
    FFrameRecvSize := 0;
    
    case FMsgType of
      mtDefault:
        if (FOpCode <= ocClose) then
          PostJSON(mtDefault);  // 同时投放 FStream

      mtJSON: begin
        FJSON.Initialize(FStream, True);  // 转换 JSON, 同时清除 FStream
        if FJSON.HasAttachment then  // 同步
        begin
          FMsgType := mtAttachment;  // 下次为附件流
          if Assigned(FOnAttachment) then
            FOnAttachment(FJSON); // 在其中判断是否接收文件流
        end else  // 直接投放
        begin
          FMsgType := mtDefault;  // 下次为 mtDefault
          PostJSON(mtJSON);
        end;
      end;

      mtAttachment: begin
        FMsgType := mtDefault;   // 下次为 mtDefault
        if Assigned(FJSON.Attachment) then
          FJSON.Attachment.Position := 0;
        PostJSON(mtAttachment);  // 投放附件
      end;
    end;

  end;

end;

end.

