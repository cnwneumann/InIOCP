(*
 * 日志线程单元，特点：
 *   1、快速、稳定
 *   2、大负载、写入内存零分配
 *   3、简单易用
 *
 * 使用方法：
 *   1、开启日志线程：TLogThread.InitLog(日志存放路径);
 *   2、写入日志：iocp_log.WriteLog('日志内容')；
 *   3、停止日志线程：TLogThread.StopLog;
 *
 * 使用经验：
 *    1. 经 2 万并发测试表明，在实际应用环境下，分配的日志缓存不超 2 块，
 * 说明预设 1-2M 的日志缓存适合绝大多数中小应用。
 *    2. 用循环的方法写千万次，分配的日志缓存达几十兆，但实际应用不会
 * 有这种做法。
 *
 *)
unit iocp_log;

interface

{$I in_iocp.inc}        // 模式设置

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SyncObjs, System.SysUtils {$ELSE}
  Windows, Classes, SyncObjs, SysUtils {$ENDIF};

type

  // 缓存结构

  PBufferNode = ^TBufferNode;
  TBufferNode = record
    buf: PAnsiChar;         // 缓存
    len: Integer;           // 剩余空间长度
    next: PBufferNode;      // 后续节点
  end;

  // 日志缓存管理

  TLogBuffers = class(TObject)
  private
    FHead: PBufferNode;     // 头节点
    FTail: PBufferNode;     // 尾节点
    FCurrent: PBufferNode;  // 当前节点
    FBuffers: PAnsiChar;    // 写入地址
    FBufferSize: Integer;   // 缓存长度
    FSize: Integer;         // 写入的总长度
    procedure InterAdd;
  public
    constructor Create(ABufferSize: Integer);
    destructor Destroy; override;
  public
    procedure Clear;
    procedure Reset;
    procedure Write(const Msg: PAnsiChar; MsgSize: Integer); overload;
    procedure Write(Handle: THandle); overload;
  end;

  // 日志线程

  TLogThread = class(TThread)
  private
    FSection: TRTLCriticalSection;  // 临界区
    FLogPath: String;        // 日志存放路径
    FMaster: TLogBuffers;    // 主列表
    FSlave: TLogBuffers;     // 从列表
    FCurrent: TLogBuffers;   // 当前列表（引用）
    FWorking: Boolean;       // 工作状态
    procedure InterClear;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Write(const Msg: AnsiString);
    procedure Stop;
  public
    class procedure Clear;
    class procedure InitLog(const LogPath: String = 'log');
    class procedure StopLog;
  end;

// 写日志过程 
procedure WriteLog(const Msg: AnsiString);

implementation

var
  LogThread: TLogThread = nil;

type
  // 双字节类型

  PDblChars = ^TDblChars;
  TDblChars = array[0..1] of AnsiChar;

  // 日期时间对应的结构体

  PDateTimeFormat = ^TDateTimeFormat;
  TDateTimeFormat = packed record
      Year: TDblChars; Year2: TDblChars; S: AnsiChar;
     Month: TDblChars;    S2: AnsiChar;
       Day: TDblChars;    S3: AnsiChar;
      Hour: TDblChars;    S4: AnsiChar;
    Minute: TDblChars;    S5: AnsiChar;
    Second: TDblChars;    S6: AnsiChar;
      MSec: TDblChars; MSec2: AnsiChar
  end;

procedure FormatNowToBuf(const dTime: PDateTimeFormat);
const
  DBL_NUMBERS: array[0..99] of TDblChars = (
     '00', '01', '02', '03', '04', '05', '06', '07', '08', '09',
     '10', '11', '12', '13', '14', '15', '16', '17', '18', '19',
     '20', '21', '22', '23', '24', '25', '26', '27', '28', '29',
     '30', '31', '32', '33', '34', '35', '36', '37', '38', '39',
     '40', '41', '42', '43', '44', '45', '46', '47', '48', '49',
     '50', '51', '52', '53', '54', '55', '56', '57', '58', '59',
     '60', '61', '62', '63', '64', '65', '66', '67', '68', '69',
     '70', '71', '72', '73', '74', '75', '76', '77', '78', '79',
     '80', '81', '82', '83', '84', '85', '86', '87', '88', '89',
     '90', '91', '92', '93', '94', '95', '96', '97', '98', '99');
var
  SysTime: TSystemTime;
  i: Integer;
begin
  // 转换时间格式为字符串，用单字节
  // 格式：2018-06-09 10:30:28 678，长度 = 23

  GetLocalTime(SysTime);

  // 年
  i := SysTime.wYear div 100;
  dTime^.Year := DBL_NUMBERS[i];
  dTime^.Year2 := DBL_NUMBERS[SysTime.wYear - i * 100];

  // 月、日
  dTime^.Month := DBL_NUMBERS[SysTime.wMonth];
  dTime^.Day := DBL_NUMBERS[SysTime.wDay];

  // 时、分、秒
  dTime^.Hour := DBL_NUMBERS[SysTime.wHour];
  dTime^.Minute := DBL_NUMBERS[SysTime.wMinute];
  dTime^.Second := DBL_NUMBERS[SysTime.wSecond];

  // 毫秒
  i := SysTime.wMilliseconds div 10;
  dTime^.MSec := DBL_NUMBERS[i];
  dTime^.MSec2 := DBL_NUMBERS[SysTime.wMilliseconds - i * 10][1];

  // 间隔符
  dTime^.S  := AnsiChar('-');
  dTime^.S2 := AnsiChar('-');
  dTime^.S3 := AnsiChar(#32);
  dTime^.S4 := AnsiChar(':');
  dTime^.S5 := AnsiChar(':');
  dTime^.S6 := AnsiChar(#32);

end;

procedure WriteLog(const Msg: AnsiString);
begin
  // 写日志
  if Assigned(LogThread) and (LogThread.Terminated = False) then
    LogThread.Write(Msg);
end;

{ TLogBuffers }

procedure TLogBuffers.Clear;
var
  Node: PBufferNode;
begin
  // 释放全部节点、缓存
  while Assigned(FHead) do
  begin
    FreeMem(FHead^.buf);
    Node := FHead^.next;
    FreeMem(FHead);
    FHead := Node;
  end;
  FTail := nil;
end;

constructor TLogBuffers.Create(ABufferSize: Integer);
begin
  inherited Create;
  FBufferSize := ABufferSize;  // 每块长度
  InterAdd;  // 预设一块缓存
end;

destructor TLogBuffers.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLogBuffers.InterAdd;
begin
  // 分配一块缓存
  GetMem(FCurrent, SizeOf(TBufferNode));  // 分配节点
  GetMem(FBuffers, FBufferSize);  // 分配写入缓存

  FCurrent^.buf := FBuffers;
  FCurrent^.len := FBufferSize;   // 可用空间长度
  FCurrent^.next := nil;  // 无后继节点

  if (FHead = nil) then
    FHead := FCurrent
  else
    FTail^.next := FCurrent;  // 加到末尾

  FTail := FCurrent;  // 置后
end;

procedure TLogBuffers.Reset;
begin
  // 重置，使用首缓存块
  FCurrent := FHead;
  FCurrent^.len := FBufferSize;
  FBuffers := FCurrent^.buf;
  FSize := 0;
end;

procedure TLogBuffers.Write(Handle: THandle);
var
  Node: PBufferNode;
  NoUsed: Cardinal;
begin
  // 遍历缓存块链表，写入日志文件
  Node := FHead;
  while Assigned(Node) do
  begin
    WriteFile(Handle, Node^.buf^, FBufferSize - Node^.len, NoUsed, nil);
    Node := Node^.next;
  end;
end;

procedure TLogBuffers.Write(const Msg: PAnsiChar; MsgSize: Integer);
begin
  // 写日志到缓存空间
  // 除了可能分配新的缓存块，没有分配其他内存
  
  if (FCurrent^.len < MsgSize) then  // 包含：时间、回车换行的长度
    if Assigned(FCurrent^.next) then // 用后续缓存块
    begin
      FCurrent := FCurrent^.next;
      FCurrent^.len := FBufferSize;
      FBuffers := FCurrent^.buf;
    end else
      InterAdd;  // 分配新的缓存块

  // 日志时间
  FormatNowToBuf(PDateTimeFormat(FBuffers));
  Inc(FBuffers, 23);

  // 时间后的间隔符（两字节）
  PDblChars(FBuffers)^ := AnsiString(':'#32);
  Inc(FBuffers, 2);

  // 日志内容
  System.Move(Msg^, FBuffers^, MsgSize - 27);
  Inc(FBuffers, MsgSize - 27);

  // 回车换行
  PDblChars(FBuffers)^ := AnsiString(#13#10);
  Inc(FBuffers, 2);

  // 可用空间-，总大小+
  Dec(FCurrent^.len, MsgSize);
  Inc(FSize, MsgSize);
end;

{ TLogThread }

constructor TLogThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  InitializeCriticalSection(FSection); // 临界区
  FMaster := TLogBuffers.Create(1024 * 1024);  // 主缓存
  FSlave := TLogBuffers.Create(1024 * 1024);   // 从缓存
  FCurrent := FMaster;  // 引用缓存
end;

destructor TLogThread.Destroy;
begin
  FMaster.Free;
  FSlave.Free;
  DeleteCriticalSection(FSection);
  inherited;
end;

procedure TLogThread.Execute;
  function CreateLogFile(var FileIndex: Integer): THandle;
  begin
    // 建编号为 FileIndex 的日志文件
    Result := CreateFile(PChar(FLogPath + IntToStr(FileIndex) + '.log'),
                         GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
                         FILE_ATTRIBUTE_NORMAL, 0);
    Inc(FileIndex);
  end;
var
  FileIndex: Integer;
  FileHandle: THandle;
  TotalSize: Integer;
  SaveBuffers: TLogBuffers;
begin
  inherited;

  // 开始工作
  FWorking := True;

  // 建日志文件
  FileIndex := 0;   // 文件编号
  FileHandle := CreateLogFile(FileIndex);

  TotalSize := 0;   // 日志文件大小

  while (Terminated = False) do
  begin
    Sleep(28);  // 越小，日志文件越多，但内存块使用更少
    
    EnterCriticalSection(FSection);
    try
      SaveBuffers := FCurrent;
      if (FCurrent = FMaster) then
        FCurrent := FSlave   // 从列表 -> 当前列表
      else
        FCurrent := FMaster; // 主列表 -> 当前列表
    finally
      LeaveCriticalSection(FSection);
    end;

    if (SaveBuffers.FSize > 0) then
    begin
      Inc(TotalSize, SaveBuffers.FSize);  // 累计写入字节数（大概）
      SaveBuffers.Write(FileHandle);  // 写文件
      SaveBuffers.Reset;   // 重置

      // 大于 5M，换新的日志文件
      if (TotalSize >= 5242880) then
      begin
        CloseHandle(FileHandle);  // 关闭文件
        FileHandle := CreateLogFile(FileIndex); // 建新文件
        TotalSize := 0; // 清零
      end;
    end;
  end;

  // 结束，写剩余数据
  if (FCurrent.FSize > 0) then
    FCurrent.Write(FileHandle);

  CloseHandle(FileHandle);

  // 停止工作
  FWorking := False;
  
end;

procedure TLogThread.InterClear;
begin
  // 释放日志缓存
  EnterCriticalSection(FSection);
  try
    FMaster.Clear;
    FSlave.Clear;
    FMaster.InterAdd;  // 加一块缓存
    FSlave.InterAdd;
  finally
    LeaveCriticalSection(FSection);
  end;
end;

procedure TLogThread.Stop;
begin
  Terminate;  // 停止
  while FWorking do
    Sleep(10);
end;

procedure TLogThread.Write(const Msg: AnsiString);
begin
  // 写日志到当前列表（Msg <> ''）
  EnterCriticalSection(FSection);
  try
    // 格式：时间 + 间隔符 + Msg + 回车换行，额外加 27 字节
    FCurrent.Write(PAnsiChar(Msg), Length(Msg) + 27);
  finally
    LeaveCriticalSection(FSection);
  end;
end;

class procedure TLogThread.Clear;
begin
  // 清除日志缓存
  if Assigned(LogThread) and (LogThread.Terminated = False) then
    LogThread.InterClear;
end;

class procedure TLogThread.InitLog(const LogPath: String);
begin
  // 开启日志
  if (LogPath = '') then
    raise Exception.Create('日志路径不能为空.');
  if not DirectoryExists(LogPath) then
    raise Exception.Create('日志路径不存在: ' + LogPath);
  if not Assigned(LogThread) then
  begin
    LogThread := TLogThread.Create;
    if (LogPath[Length(LogPath)] <> '\') then
      LogThread.FLogPath := LogPath + '\'
    else
      LogThread.FLogPath := LogPath;
    LogThread.FLogPath := LogThread.FLogPath +
                          FormatDateTime('yyyy-mm-dd-hh-mm-ss-', now);
    LogThread.Resume;
  end;
end;

class procedure TLogThread.StopLog;
begin
  // 停止日志
  if Assigned(LogThread) then
  begin
    LogThread.Stop;
    LogThread := nil;
  end;
end;

end.
