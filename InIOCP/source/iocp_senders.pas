(*
 *  发送数据的专用单元
 *
 *  注：1. 服务端每个业务线程一个数据发送器，
 *         见 iocp_threads.TBusiThread；
 *      2. 客户端每个发送线程一个数据发送器；
 *      3. TransmitFile 模式发送时，每 TBaseSocket 对象一个
 *         TTransmitObject 发送器
 *)
unit iocp_senders;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.Variants, System.Sysutils, {$ELSE}
  Windows, Classes, Variants, Sysutils, {$ENDIF}
  iocp_Winsock2, iocp_base, iocp_objPools, iocp_wsExt;

type

  // ================= 待发数据信息 基类 =================
  // 服务端、客户端均用 TPerIOData，但客户端用 Send 发送；
  // 发送器不检查待发送数据的有效性！要在调用前检查。

  TBaseTaskObject = class(TObject)
  private
    FOwner: TObject;             // 宿主
    FSocket: TSocket;            // 客户端对应的套接字
    FTask: TTransmitTask;        // 待发送数据描述
    FErrorCode: Integer;         // 异常代码
    FOnError: TNotifyEvent;      // 异常事件
    function GetIOType: TIODataType;
    procedure SetOwner(const Value: TObject);
  protected
    FSendBuf: PPerIOData;        // 重叠结构
    procedure InterSetTask(const Data: PAnsiChar; Size: Cardinal; AutoFree: Boolean); overload;
    procedure InterSetTask(const Data: AnsiString; AutoFree: Boolean); overload;
    procedure InterSetTask(Handle: THandle; Size, Offset, OffsetEnd: TFileSize; AutoFree: Boolean); overload;
    procedure InterSetTaskVar(const Data: Variant);
  public
    procedure FreeResources(FreeRes: Boolean = True); virtual; abstract;
  public
    property ErrorCode: Integer read FErrorCode;
    property IOType: TIODataType read GetIOType;
    property Owner: TObject read FOwner write SetOwner; // r/w
    property Socket: TSocket read FSocket write FSocket; // r/w
  public
    property OnError: TNotifyEvent read FOnError write FOnError; // r/w
  end;

  // ================= 套接字对象数据发送器 类 =================
  // 服务端用 TransmitFile() 模式发送的数据描述
  // 此类对象附属于 TBaseSocket，开启编译项 TRANSMIT_FILE 有效
  
  {$IFDEF TRANSMIT_FILE}

  TTransmitObject = class(TBaseTaskObject)
  private
    FExists: Integer;       // 是否有数据
    function GetExists: Boolean;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure FreeResources(FreeRes: Boolean = True); override;

    procedure SetTask(const Data: PAnsiChar; Size: Cardinal); overload;
    procedure SetTask(const Data: AnsiString); overload;
    procedure SetTask(Handle: THandle; Size: TFileSize); overload;

    procedure SetTask(Stream: TMemoryStream; Size: Cardinal); overload;
    procedure SetTask(Stream: TStream; Size: TFileSize;
                      Offset: TFileSize = 0; OffsetEnd: TFileSize = 0); overload;

    procedure SetTaskVar(const Data: Variant);

    procedure TransmitFile;
  public
    property Exists: Boolean read GetExists;
  end;

  {$ENDIF}

  // ================= 线程数据发送器 基类 =================

  TBaseTaskSender = class(TBaseTaskObject)
  private
    FBufferSize: Cardinal;  // 发送缓存长度
    FChunked: Boolean;      // HTTP 服务端分块发送数据

    FMasking: Boolean;      // WebSocket 使用掩码
    FOpCode: TWSOpCode;     // WebSocket 操作
    FWebSocket: Boolean;    // WebSocket 类型
    FWSCount: UInt64;       // WebSocket 发出数据计数
    FWSMask: TWSMask;       // WebSocket 掩码

    function GetData: PWsaBuf;
    procedure ChunkDone;
    procedure MakeFrameInf(Payload: UInt64);
    procedure InitHeadTail(DataLength: Cardinal; Fulled: Boolean);
    procedure InterSendBuffer(Data: PAnsiChar; ByteCount, Offset, OffsetEnd: Cardinal);
    procedure InterSendFile;
    procedure InternalSend;

    procedure SetChunked(const Value: Boolean);
    procedure SetOpCode(Value: TWSOpCode);
  protected
    procedure DirectSend(OutBuf: PAnsiChar; OutSize, FrameSize: Integer); virtual; abstract;
    procedure ReadSendBuffers(InBuf: PAnsiChar; ReadCount, FrameSize: Integer); virtual; abstract;
  public
    procedure FreeResources(FreeRes: Boolean = True); override;

    procedure Send(const Data: PAnsiChar; Size: Cardinal; AutoFree: Boolean = True); overload;
    procedure Send(const Data: AnsiString); overload;

    procedure Send(Handle: THandle; Size: TFileSize;
                   Offset: TFileSize = 0; OffsetEnd: TFileSize = 0;
                   AutoFree: Boolean = True); overload;

    procedure Send(Stream: TStream; Size: TFileSize; Offset: TFileSize = 0;
                   OffsetEnd: TFileSize = 0; AutoFree: Boolean = True); overload;

    procedure Send(Stream: TStream; Size: TFileSize; AutoFree: Boolean = True); overload;
    procedure SendVar(const Data: Variant);
    
    procedure SendBuffers;
  protected
    property Chunked: Boolean read FChunked write SetChunked;
  public
    property Data: PWsaBuf read GetData;
    property Masking: Boolean read FMasking write FMasking;
    property OpCode: TWSOpCode read FOpCode write SetOpCode; // r/w
  end;

  // ================= 业务数据发送器 类 =================
  // 用 WSASend 发送 TPerIOData.Data.Buf
  // 此类对象附属于业务线程 TBusiThread

  TServerTaskSender = class(TBaseTaskSender)
  protected
    procedure DirectSend(OutBuf: PAnsiChar; OutSize, FrameSize: Integer); override;
    procedure ReadSendBuffers(InBuf: PAnsiChar; ReadCount, FrameSize: Integer); override;
  public
    constructor Create(BufferPool: TIODataPool);
    procedure CopySend(ARecvBuf: PPerIOData);
    procedure FreeBuffers(BufferPool: TIODataPool);
  public
    property Chunked;
  end;

  // ================= 客户端数据发送器 类 =================
  // 用 Send 发送 TPerIOData.Data.Buf

  TAfterSendEvent = procedure(DataType: TMessageDataType; OutSize: Integer) of object;

  TClientTaskSender = class(TBaseTaskSender)
  private
    FAfterSend: TAfterSendEvent;   // 发出事件
    FDataType: TMessageDataType;   // 数据类型
    FStoped: Integer;              // 工作状态(0=工作、1=停止)
    function GetStoped: Boolean;
    procedure SetStoped(const Value: Boolean);
  protected
    procedure DirectSend(OutBuf: PAnsiChar; OutSize, FrameSize: Integer); override;
    procedure ReadSendBuffers(InBuf: PAnsiChar; ReadCount, FrameSize: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property DataType: TMessageDataType read FDataType write FDataType;
    property Stoped: Boolean read GetStoped write SetStoped;
  public
    property AfterSend: TAfterSendEvent read FAfterSend write FAfterSend;  
  end;

procedure MakeFrameHeader(const Data: PWsaBuf; OpCode: TWSOpCode; Payload: UInt64 = 0);

implementation

uses
  iocp_api, iocp_sockets, http_base;

procedure MakeFrameHeader(const Data: PWsaBuf; OpCode: TWSOpCode; Payload: UInt64);
var
  i: Integer;
  p: PByte;
begin
  // 服务端：构造短消息的 WebSocket 帧信息
  //   忽略 RSV1/RSV2/RSV3

  p := PByte(Data^.buf);

  p^ := Byte($80) + Byte(OpCode);
  Inc(p);

  case Payload of
    0..125: begin
      if (OpCode >= ocClose) then
        p^ := 0
      else
        p^ := Payload;
      Inc(p);
    end;
    126..$FFFF: begin
      p^ := 126;
      Inc(p);
      TByteAry(p)[0] := TByteAry(@Payload)[1];
      TByteAry(p)[1] := TByteAry(@Payload)[0];
      Inc(p, 2);
    end;
    else begin
      p^ := 127;
      Inc(p);
      for i := 0 to 7 do
        TByteAry(p)[i] := TByteAry(@Payload)[7 - i];
      Inc(p, 8);
    end;
  end;

  Data^.len := PAnsiChar(p) - Data^.buf;

end;

{ TBaseTaskObject }

procedure TBaseTaskObject.InterSetTask(const Data: AnsiString; AutoFree: Boolean);
begin
  // 发送 AnsiString（不是双字节的 String）
  FTask.RefStr := Data;
  FTask.Head := PAnsiChar(Data);
  FTask.HeadLength := Length(Data);
  FTask.AutoFree := AutoFree;
end;

procedure TBaseTaskObject.InterSetTask(const Data: PAnsiChar; Size: Cardinal;
                          AutoFree: Boolean);
begin
  // 发送一段内存 Buffer
  FTask.Head := Data;
  FTask.HeadLength := Size;
  FTask.AutoFree := AutoFree;
end;

function TBaseTaskObject.GetIOType: TIODataType;
begin
  // 取 FSendBuf^.IOType（客户端无意义）
  Result := FSendBuf^.IOType;
end;

procedure TBaseTaskObject.InterSetTask(Handle: THandle; Size, Offset, 
                          OffsetEnd: TFileSize; AutoFree: Boolean);
begin
  // 发送文件句柄 Handle
  //  Handle > 0 为有效句柄，见 iocp_utils.InternalOpenFile
  FTask.Handle := Handle;
  FTask.Size := Size;
  FTask.Offset := Offset;
  FTask.OffsetEnd := OffsetEnd;
  FTask.AutoFree := AutoFree;
end;

procedure TBaseTaskObject.InterSetTaskVar(const Data: Variant);
var
  p, Buf: Pointer;
  BufLength: Integer;
begin
  // 发送可变类型数据（数据集）
  if VarIsNull(Data) then
    Exit;

  BufLength := VarArrayHighBound(Data, 1) - VarArrayLowBound(Data, 1) + 1;
  GetMem(Buf, BufLength);

  p := VarArrayLock(Data);
  try
    System.Move(p^, Buf^, BufLength);
  finally
    VarArrayUnlock(Data);
  end;

  // 发送内存数据
  FTask.Head := Buf;
  FTask.HeadLength := BufLength;
  FTask.AutoFree := True;
end;

procedure TBaseTaskObject.SetOwner(const Value: TObject);
begin
  FOwner := Value;
  FTask.ObjType := Value.ClassType;
end;

{ TTransmitObject }

{$IFDEF TRANSMIT_FILE}

constructor TTransmitObject.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FTask.ObjType := AOwner.ClassType;
  GetMem(FSendBuf, SizeOf(TPerIOData));  // 直接分配
  FSendBuf^.Data.len := 0;
  FSendBuf^.Node := nil;  // 不是池里面的节点
end;

destructor TTransmitObject.Destroy;
begin
  // 释放资源
  FreeResources;
  FreeMem(FSendBuf);
  inherited;
end;

procedure TTransmitObject.FreeResources(FreeRes: Boolean);
begin
  // 释放 TransmitFile 模式的数据资源
  //   已在外部释放时，参数 FreeRes=False
  try
    try
      if (InterlockedDecrement(FExists) = 0) and FreeRes then
        if (FTask.ObjType = TIOCPSocket) then
        begin
          // 1. 只有 Stream、Stream2 两资源
          if Assigned(FTask.Stream) then  // 主体流
            FTask.Stream.Free;
          if Assigned(FTask.Stream2) then // 附件流
            FTask.Stream2.Free;
        end else
        if (FTask.ObjType = THttpSocket) then
        begin
          // 2. 只有 AnsiString、Handle、Stream 三资源，
          //    且相互排斥，见：THttpRespone.SendWork;
          if Assigned(FTask.Stream) then  // 内存流、文件流
            FTask.Stream.Free
          else
          if (FTask.Handle > 0) then  // 纯粹的文件句柄
            CloseHandle(FTask.Handle)
          else
          if (FTask.RefStr <> '') then
            FTask.RefStr := '';
        end else
        begin
          // 3. TStreamSocket 或其他对象的各种发送资源，相互排斥
          if Assigned(FTask.Stream) then  // 内存流、文件流
            FTask.Stream.Free
          else
          if (FTask.Handle > 0) then  // 纯粹的文件句柄
            CloseHandle(FTask.Handle)
          else
          if (FTask.RefStr <> '') then
            FTask.RefStr := ''
          else begin
            if Assigned(FTask.Head) then
              FreeMem(FTask.Head);
            if Assigned(FTask.Tail) then
              FreeMem(FTask.Tail);
          end;
        end;
    finally
      FillChar(FTask, TASK_SPACE_SIZE, 0);  // 清零
      FTask.ObjType := FOwner.ClassType;    // 会被清
    end;
  except
    // 正常不应该有
  end;
end;

function TTransmitObject.GetExists: Boolean;
begin
  // 是否有发送数据
  Result := (iocp_api.InterlockedCompareExchange(FExists, 0, 0) > 0);
end;

procedure TTransmitObject.SetTask(const Data: AnsiString);
begin
  // 设置字符串
  InterSetTask(Data, False);
  FExists := 1;
end;

procedure TTransmitObject.SetTask(const Data: PAnsiChar; Size: Cardinal);
begin
  // 设置内存块
  InterSetTask(Data, Size, False);
  FExists := 1;
end;

procedure TTransmitObject.SetTask(Handle: THandle; Size: TFileSize);
begin
  // 设置文件句柄（整个文件）
  InterSetTask(Handle, Size, 0, 0, False);
  FExists := 1;
end;

procedure TTransmitObject.SetTask(Stream: TMemoryStream; Size: Cardinal);
begin
  // 设置 C/S 主体流（不是 HTTP 时实体）
  FTask.Stream := Stream;  // 对应 Stream
  FTask.Head := Stream.Memory;
  FTask.HeadLength := Size;
  FExists := 1;
end;

procedure TTransmitObject.SetTask(Stream: TStream; Size, Offset, OffsetEnd: TFileSize);
begin
  // 增加一个流，作为：
  //   1. C/S 模式的附件流
  //   2. HTTP 的实体数据流
  if (FTask.ObjType = THttpSocket) then  // THttpSocket
  begin
    FTask.Stream := Stream;
    if (Stream is TMemoryStream) then
    begin
      // 发送内存流
      FTask.Head := TMemoryStream(Stream).Memory;
      FTask.HeadLength := Size;
    end else
    begin
      // 发送文件流的句柄
      FTask.Handle := THandleStream(Stream).Handle;
      FTask.Size := Size;
      FTask.Offset := Offset;
      FTask.OffsetEnd := OffsetEnd;
    end;
  end else
  begin
    FTask.Stream2 := Stream;  // TBaseSocket 对应 Stream2
    if (Stream is TMemoryStream) then
    begin
      // 作为尾
      FTask.Tail := TMemoryStream(Stream).Memory;
      FTask.TailLength := Size;
    end else
    begin
      // 发送文件流的句柄
      FTask.Handle := THandleStream(Stream).Handle;
      FTask.Size := Size;
      FTask.Offset := Offset;
      FTask.OffsetEnd := OffsetEnd;
    end;
  end;
  FExists := 1;
end;

procedure TTransmitObject.SetTaskVar(const Data: Variant);
begin
  // 设置变长类型
  InterSetTaskVar(Data);
  FExists := 1;
end;

procedure TTransmitObject.TransmitFile;
var
  XOffset: LARGE_INTEGER;
begin
  // 用 TransmitFile() 发送 Task 的内容
  // 提交后有三种结果：
  // 1. 失败，在当前线程处理
  // 2. 提交成功，有两种情况：
  //    A. 全部发送完毕，被工作线程监测到（只一次），执行 TBaseSocket.FreeTransmitRes 释放资源
  //    B. 发送出现异常，也被工作线程监测到，执行 TBaseSocket.TryClose 尝试关闭

  // 清重叠结构
  FillChar(FSendBuf^.Overlapped, SizeOf(TOverlapped), 0);

  FSendBuf^.Owner := FOwner;  // 宿主
  FSendBuf^.IOType := ioTransmit;  // iocp_server 中判断用

  // 设置位移（用 LARGE_INTEGER 确定位移）
  if (FTask.Handle > 0) then
  begin
    XOffset.QuadPart := FTask.Offset;
    if (FTask.OffsetEnd > 0) then
    begin
      FSendBuf^.Overlapped.Offset := XOffset.LowPart;  // 设置位移
      FSendBuf^.Overlapped.OffsetHigh := XOffset.HighPart; // 位移高位
      FTask.Size := FTask.OffsetEnd - FTask.Offset + 1; // 发送长度
    end;
    SetFilePointer(FTask.Handle, XOffset.LowPart, @XOffset.HighPart, FILE_BEGIN);
  end;
  
  if (iocp_wsExt.gTransmitFile(
                 FSocket,            // 套接字
                 FTask.Handle,       // 文件句柄，可为 0
                 FTask.Size,         // 发送长度，可为 0
                 IO_BUFFER_SIZE * 8, // 每次发送长度
                 @FSendBuf^.Overlapped,     // 重叠结构
                 PTransmitBuffers(@FTask),  // 头尾数据块
                 TF_USE_KERNEL_APC   // 用内核线程
                 ) = False) then
  begin
    FErrorCode := WSAGetLastError;
    if (FErrorCode <> ERROR_IO_PENDING) then  // 异常
      FOnError(Self)
    else
      FErrorCode := 0;
  end else
    FErrorCode := 0;

end;  

{$ENDIF}

{ TBaseTaskSender }

procedure TBaseTaskSender.ChunkDone;
begin
  // 发送分块结束标志
  with FSendBuf^.Data do
  begin
    PAnsiChar(buf)^ := AnsiChar('0');  // 0
    PStrCRLF2(buf + 1)^ := STR_CRLF2;  // 回车换行, 两个
  end;
  DirectSend(nil, 5, 0);  // 发送 5 字节
end;

procedure TBaseTaskSender.FreeResources(FreeRes: Boolean);
begin
  // 释放 WSASend、Send 模式的数据资源
  try
    if Assigned(FTask.Stream) then
    begin
      if FTask.AutoFree then
        FTask.Stream.Free;
    end else
    if (FTask.Handle > 0) then
      CloseHandle(FTask.Handle)
    else
    if (FTask.RefStr <> '') then
    begin
      FTask.RefStr := '';
      FTask.Head := nil;
    end else
    if Assigned(FTask.Head) then  // 未用 FTask.Tail
    begin
      if FTask.AutoFree then
        FreeMem(FTask.Head);
    end;
  finally
    FillChar(FTask, TASK_SPACE_SIZE, 0);  // 清零
    FTask.ObjType := FOwner.ClassType;    // 会被清
  end;
end;

function TBaseTaskSender.GetData: PWsaBuf;
begin
  // 返回发送缓存地址，让外部直接写数据，与 SendBuffers 对应
  Result := @FSendBuf^.Data;
end;

procedure TBaseTaskSender.InitHeadTail(DataLength: Cardinal; Fulled: Boolean);
begin
  // 填写分块的头尾：长度描述（6字节）、回车换行
  PChunkSize(FSendBuf^.Data.buf)^ := PChunkSize(AnsiString(IntToHex(DataLength, 4)) + STR_CRLF)^;
  PStrCRLF(FSendBuf^.Data.buf + DataLength + 6)^ := STR_CRLF;
end;

procedure TBaseTaskSender.MakeFrameInf(Payload: UInt64);
var
  i: Integer;
  iByte: Byte;
  p: PByte;
begin
  // 构造 WebSocket 帧信息
  //   忽略 RSV1/RSV2/RSV3

  p := PByte(FSendBuf^.Data.buf);

  p^ := Byte($80) + Byte(FOpCode);
  Inc(p);

  if FMasking then  // 加掩码
    iByte := Byte($80)
  else
    iByte := 0;
    
  case Payload of
    0..125: begin
      if (OpCode >= ocClose) then
        p^ := iByte
      else
        p^ := iByte + Payload;
      Inc(p);
    end;
    126..$FFFF: begin
      p^ := iByte + 126;
      Inc(p);
      TByteAry(p)[0] := TByteAry(@Payload)[1];
      TByteAry(p)[1] := TByteAry(@Payload)[0];
      Inc(p, 2);
    end;
    else begin
      p^ := iByte + 127;
      Inc(p);
      for i := 0 to 7 do
        TByteAry(p)[i] := TByteAry(@Payload)[7 - i];
      Inc(p, 8);
    end;
  end;

  if FMasking then  // 客户端的掩码
  begin
    Cardinal(FWsMask) := GetTickCount;
    FWsMask[3] := FWsMask[1] xor $28;  // = 0 -> 修正
    PCardinal(p)^ := Cardinal(FWsMask);
    Inc(p, 4);
  end;

  FSendBuf^.Data.len := PAnsiChar(p) - FSendBuf^.Data.buf;
  FWSCount := 0;
  
end;

procedure TBaseTaskSender.InternalSend;
begin
  // 发送任务 FTask 描述的数据（几种数据不共存）
  FErrorCode := 0;  // 无异常
  try
    try
      if Assigned(FTask.Head) then  // 1. 发送内存块
        InterSendBuffer(FTask.Head, FTask.HeadLength, FTask.Offset, FTask.OffsetEnd)
      else
      if (FTask.Handle > 0) then    // 2. 发送文件
        InterSendFile
      else
      if Assigned(FTask.Tail) then  // 3. 发送内存块
        InterSendBuffer(FTask.Tail, FTask.TailLength, FTask.Offset, FTask.OffsetEnd);
    finally
      FreeResources;  // 4. 释放资源
      if FChunked then
        FChunked := False;
    end;
  except
    FErrorCode := GetLastError;
  end;
end;

procedure TBaseTaskSender.InterSendBuffer(Data: PAnsiChar;
                          ByteCount, Offset, OffsetEnd: Cardinal);
var
  FrameSize: Cardinal;   // 数据描述长度
  BufLength: Cardinal;   // 分块模式的缓存长度
  BytesToRead: Cardinal; // 期望读入长度
begin
  // 发送一段内存（不应很长）
  // 范围：Offset - OffsetEnd

  // Chunk 发送的格式：
  // 长度描述（6字节） + 回车换行 + 内容 + 回车换行（2字节）
  // 内容大于 IO_BUFFER_SIZE 时，先预填 长度描述、末尾的回车换行
  
  if (OffsetEnd > Offset) then  // 发送部分内容，定位
  begin
    Inc(Data, Offset);
    ByteCount := OffsetEnd - Offset + 1;
  end;

  if FWebSocket then  // 发送 webSocket 数据
  begin
    MakeFrameInf(ByteCount);
    FrameSize := FSendBuf^.Data.len; // 描述长度
    BufLength := FBufferSize - FrameSize;  // 最大读入长度
  end else
    if FChunked then  // Chunk 发送
    begin
      FrameSize := 6; // 描述长度
      BufLength := FBufferSize - 8; // 减头尾长度，6+2
      if (ByteCount >= BufLength) then  // 预填头尾
        InitHeadTail(BufLength, True);
    end else
    begin
      FrameSize := 0;
      BufLength := FBufferSize;
    end;

  while (ByteCount > 0) do
  begin
    if (ByteCount >= BufLength) then  // 超长
      BytesToRead := BufLength
    else begin
      BytesToRead := ByteCount;
      if FChunked then  // 内容未满
        InitHeadTail(BytesToRead, False); // 调整头尾
    end;

    // 读入数据，发送
    ReadSendBuffers(Data, BytesToRead, FrameSize);

    if (FErrorCode <> 0) then  // 退出
      Break;

    Inc(Data, BytesToRead);  // 地址向前
    Dec(ByteCount, BytesToRead);  // 剩余数 -

    if FWebSocket and (FrameSize > 0) then  // 下次不带描述
    begin
      FrameSize := 0;
      BufLength := FBufferSize;  // 恢复最大长度
    end;
  end;

  if FChunked then  // 发送分块结束标志
    ChunkDone;

end;

procedure TBaseTaskSender.InterSendFile;
var
  FrameSize: Cardinal;   // WebSocket帧结构长度
  BufLength: Cardinal;   // 分块模式的缓存长度
  ByteCount: TFileSize;  // 总长度
  BytesToRead, BytesReaded: Cardinal;
  Offset: LARGE_INTEGER;
begin
  // 发送一段文件
  // 范围：Task^.Offset ... Task^.OffsetEnd
  try
    if (FTask.Offset = 0) and (FTask.OffsetEnd = 0) then
      ByteCount := FTask.Size
    else
      ByteCount := FTask.OffsetEnd - FTask.Offset + 1;

    // 定位（可能大于 2G，用 LARGE_INTEGER 确定位移）
    Offset.QuadPart := FTask.Offset;
    SetFilePointer(FTask.Handle, Offset.LowPart, @Offset.HighPart, FILE_BEGIN);

    if FWebSocket then  // 发送 webSocket 数据
    begin
      MakeFrameInf(ByteCount);
      FrameSize := FSendBuf^.Data.len;
      BufLength := FBufferSize - FrameSize;  // 最大读入长度
    end else
      if FChunked then  // Chunk 发送
      begin
        FrameSize := 6; // 有描述
        BufLength := FBufferSize - 8; // 减头尾长度，6+2
        if (ByteCount >= BufLength) then  // 预填头尾
          InitHeadTail(BufLength, True);
      end else
      begin
        FrameSize := 0;
        BufLength := FBufferSize;
      end;

    while (ByteCount > 0) do
    begin
      if (ByteCount >= BufLength) then  // 超长
        BytesToRead := BufLength
      else begin
        BytesToRead := ByteCount;
        if FChunked then  // 内容未满
          InitHeadTail(BytesToRead, False); // 调整头尾
      end;

      // 先读入一块数据
      if (FrameSize > 0) then  // 读入到长度描述后位置
        ReadFile(FTask.Handle, (FSendBuf^.Data.buf + FrameSize)^,
                 BytesToRead, BytesReaded, nil)
      else
        ReadFile(FTask.Handle, FSendBuf^.Data.buf^,
                 BytesToRead, BytesReaded, nil);

      if (BytesToRead = BytesReaded) then  // 读入成功
      begin
        if FWebSocket then
          DirectSend(FSendBuf^.Data.buf, BytesToRead + FrameSize, FrameSize) // 发送，长度+FrameSize
        else
        if FChunked then
          DirectSend(FSendBuf^.Data.buf, BytesToRead + 8, 0)  // 发送，长度+6+2
        else
          DirectSend(FSendBuf^.Data.buf, BytesToRead, 0); // 发送，长度不变

        if (FErrorCode <> 0) then  // 退出
          Break;

        Dec(ByteCount, BytesToRead);  // 剩余数 -
        if FWebSocket and (FrameSize > 0) then  // 下次不带描述
        begin
          FrameSize := 0;
          BufLength := FBufferSize;
        end;
      end else
      begin
        FErrorCode := GetLastError;
        Break;
      end;
    end;

    if FChunked then  // 发送分块结束标志
      ChunkDone;

  except
    FErrorCode := GetLastError;
  end;
end;

procedure TBaseTaskSender.Send(const Data: PAnsiChar; Size: Cardinal; AutoFree: Boolean);
begin
  // 发送 Buffer
  InterSetTask(Data, Size, AutoFree);
  InternalSend;
end;

procedure TBaseTaskSender.Send(Handle: THandle; Size, Offset,
                               OffsetEnd: TFileSize; AutoFree: Boolean);
begin
  // 发送文件 Handle
  //  Handle > 0 为有效句柄，见 iocp_utils.InternalOpenFile
  InterSetTask(Handle, Size, Offset, OffsetEnd, AutoFree);
  InternalSend;
end;

procedure TBaseTaskSender.Send(const Data: AnsiString);
begin
  // 发送 AnsiString（不是双字节的 String）
  InterSetTask(Data, True);
  InternalSend;
end;

procedure TBaseTaskSender.Send(Stream: TStream; Size, Offset,
                          OffsetEnd: TFileSize; AutoFree: Boolean);
begin
  // 设置 C/S 模式的附件流, 根据 AutoFree 释放
  FTask.Stream := Stream; 
  FTask.AutoFree := AutoFree;
  if (Stream is TMemoryStream) then
  begin
    // 发送内存流
    FTask.Head := TMemoryStream(Stream).Memory;
    FTask.HeadLength := Size;
  end else
  begin
    // 发送文件流的句柄
    FTask.Handle := THandleStream(Stream).Handle;
    FTask.Size := Size;
    FTask.Offset := Offset;
    FTask.OffsetEnd := OffsetEnd;
  end;
  InternalSend;
end;

procedure TBaseTaskSender.Send(Stream: TStream; Size: TFileSize; AutoFree: Boolean);
begin
  // 发送整个数据流，根据 AutoFree 决定是否释放
  Send(Stream, Size, 0, 0, AutoFree);
end;

procedure TBaseTaskSender.SendBuffers;
begin
  // 数据已经填写到 Buf，直接发送
  //（FramSize = 0，WebSocket客户端不能使用） 
  DirectSend(FSendBuf^.Data.Buf, FSendBuf^.Data.len, 0);
end;

procedure TBaseTaskSender.SendVar(const Data: Variant);
begin
  // 发送可变类型数据
  InterSetTaskVar(Data);
  InternalSend;
end;

procedure TBaseTaskSender.SetChunked(const Value: Boolean);
begin
  // 设置 HTTP 分块模式
  FChunked := Value;
  FOpCode := ocContinuation;
  FWebSocket := False;  // 不是 WebSocket 发送
end;

procedure TBaseTaskSender.SetOpCode(Value: TWSOpCode);
begin
  // 设置 WebSocket 协议的操作
  FOpCode := Value;
  FWebSocket := True;
  FChunked := False;  // 不是分块发送了
end;

{ TServerTaskSender }

procedure TServerTaskSender.CopySend(ARecvBuf: PPerIOData);
begin
  // 复制 ARecvBuf 发送
  FSendBuf^.Data.len := ARecvBuf^.Overlapped.InternalHigh;
  System.Move(ARecvBuf^.Data.buf^, FSendBuf^.Data.buf^, FSendBuf^.Data.len);
  DirectSend(nil, FSendBuf^.Data.len, 0);  // 直接发送
end;

constructor TServerTaskSender.Create(BufferPool: TIODataPool);
begin
  inherited Create;
  FSendBuf := BufferPool.Pop^.Data;
  FSendBuf^.Data.len := IO_BUFFER_SIZE;
  FBufferSize := IO_BUFFER_SIZE;
  FWebSocket := False;
end;

procedure TServerTaskSender.DirectSend(OutBuf: PAnsiChar; OutSize, FrameSize: Integer);
var
  ByteCount, Flags: Cardinal;
begin
  // 直接发送 FSendBuf 的内容（忽略参数 OutBuf、FrameSize）

  // 清重叠结构
  FillChar(FSendBuf^.Overlapped, SizeOf(TOverlapped), 0);

  FSendBuf^.Owner := FOwner;  // 宿主
  FSendBuf^.IOType := ioSend;  // iocp_server 中判断用
  FSendBuf^.Data.len := OutSize;  // 长度

  ByteCount := 0;
  Flags := 0;        

  // FSendBuf^.Overlapped 与 TPerIOData 同地址
  if (iocp_Winsock2.WSASend(FSocket, @(FSendBuf^.Data), 1, ByteCount,
      Flags, LPWSAOVERLAPPED(@FSendBuf^.Overlapped), nil) = SOCKET_ERROR) then
  begin
    FErrorCode := WSAGetLastError;
    if (FErrorCode <> ERROR_IO_PENDING) then  // 异常
      FOnError(Self)  // 执行 TBaseSocket 方法
    else begin
      // 发出时会收到消息，
      // 严格来说要在工作线程处理，那要调整发送器
      WaitForSingleObject(FSocket, INFINITE);
      FErrorCode := 0;
    end;
  end else
    FErrorCode := 0;

end;

procedure TServerTaskSender.FreeBuffers(BufferPool: TIODataPool);
begin
  BufferPool.Push(FSendBuf^.Node);
  FSendBuf := nil;
end;

procedure TServerTaskSender.ReadSendBuffers(InBuf: PAnsiChar; ReadCount, FrameSize: Integer);
begin
  // 读数据到发送缓存，发送
  if FChunked or (FrameSize > 0) then  // 要分块发送
  begin
    // 1. 发送 Chunk, WebSocket 首次数据
    System.Move(InBuf^, (FSendBuf^.Data.buf + FrameSize)^, ReadCount);  // 内容
    if FChunked then
      DirectSend(nil, ReadCount + 8, 0)  // ReadCount + 描述长度 + 末尾的回车换行
    else  // WebSocket 数据
      DirectSend(nil, ReadCount + FrameSize, FrameSize);
  end else
  begin
    // 2. 无分块描述的数据，直接读入
    System.Move(InBuf^, FSendBuf^.Data.buf^, ReadCount);
    DirectSend(nil, ReadCount, 0);
  end;
end;

{ TClientTaskSender }

constructor TClientTaskSender.Create;
begin
  inherited;
  // 直接分配发送内存块  
  FSendBuf := New(PPerIOData);
  GetMem(FSendBuf^.Data.Buf, IO_BUFFER_SIZE_2);
  FSendBuf^.Data.len := IO_BUFFER_SIZE_2;
  FBufferSize := IO_BUFFER_SIZE_2;
  FWebSocket := False;
end;

destructor TClientTaskSender.Destroy;
begin
  // 释放发送内存块
  FreeMem(FSendBuf^.Data.Buf);
  Dispose(FSendBuf);
  inherited;
end;

procedure TClientTaskSender.DirectSend(OutBuf: PAnsiChar; OutSize, FrameSize: Integer);
var
  i: Integer;
  TotalCount: UInt64; // 总发出
  p: PByte;
begin
  // 发送数据块

  if FWebSocket and FMasking then  // 对内容作掩码处理
  begin
    p := PByte(FSendBuf^.Data.buf + FrameSize);
    TotalCount := FWSCount + OutSize - FrameSize;
    for i := FWSCount to TotalCount - 1 do
    begin
      p^ := p^ xor FWSMask[i mod 4];
      Inc(p);
    end;
    FWSCount := TotalCount;
  end;

  if (Stoped = False) then
    FErrorCode := iocp_Winsock2.Send(FSocket, OutBuf^, OutSize, 0)
  else begin
    if (FDataType <> mdtHead) then  // 发送取消标志
      iocp_Winsock2.Send(FSocket, IOCP_SOCKET_CANCEL[1], IOCP_CANCEL_LENGTH, 0);
    FErrorCode := -2;   // 停止，特殊编码
  end;

  if Stoped or (FErrorCode <= 0) then
  begin
    if Assigned(FOnError) then
      FOnError(FOwner);
  end else
  begin
    FErrorCode := 0;  // 无异常
    if Assigned(FAfterSend) then
      FAfterSend(FDataType, OutSize);
  end;
end;

function TClientTaskSender.GetStoped: Boolean;
begin
  // FState=1，停止了
  Result := iocp_api.InterlockedCompareExchange(FStoped, 1, 1) = 1;
end;

procedure TClientTaskSender.ReadSendBuffers(InBuf: PAnsiChar; ReadCount, FrameSize: Integer);
begin
  // 发送缓存
  if FWebSocket then  // 读内容到缓存
  begin
    System.Move(InBuf^, (FSendBuf^.Data.buf + FrameSize)^, ReadCount);
    DirectSend(FSendBuf^.Data.buf, ReadCount + FrameSize, FrameSize);
  end else
    DirectSend(InBuf, ReadCount, FrameSize);
end;

procedure TClientTaskSender.SetStoped(const Value: Boolean);
begin
  // 设置状态
  //   工作：Value=False，FStoped=0
  //   停止：Value=True，FStoped=1
  InterlockedExchange(FStoped, Ord(Value));
{  if (Value = False) then  // 不停止
    FFirst := True;     }
end;
  
end.
