(*
 * icop 各种函数、过程单元
 *)
unit iocp_utils;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils,
  System.Win.Registry, Winapi.ActiveX, Winapi.PsAPI,
  System.Variants, Data.DB, Data.Win.ADODB, {$ELSE}
  Windows, Classes, SysUtils, Registry, ActiveX,
  PsAPI, Variants, DB, ADODB, {$ENDIF}
  iocp_Winsock2, iocp_zlib, iocp_base,
  iocp_lists, http_base, http_utils;

function CreateSocket: TSocket;
function ConnectSocket(Socket: TSocket; const Server: AnsiString; Port: Word): Boolean;
function GetCPUCount: Integer;

function GetSysErrorMessage(ErrorCode: DWORD = 0): String;
function GetWSAErrorMessage(ErrorCode: DWORD = 0): String;

function AddBackslash(const Path: AnsiString): AnsiString;
function DropBackSlash(const Path: AnsiString): AnsiString;
function MyCreateDir(const Path: String): Boolean;

function FileTimeToDateTime(AFileTime: TFileTime): TDateTime;
function CreateNewFileName(const FileName: AnsiString): AnsiString;
function GetCompressionLevel(const FileName: String): TZipLevel;

function InternalOpenFile(const FileName: String; ReadOnly: Boolean = True): THandle; overload;
function InternalOpenMsgFile(const FileName: String; AutoCreate: Boolean = False): THandle;

function ExtractValue(var LeftValue, ValueList: AnsiString): Boolean;

// 变型转内存流
function VariantToStream(Data: Variant; ZipCompress: Boolean = False; const FileName: String = ''): TStream;

// 流转变型
function StreamToVariant(Stream: TStream; ZipDecompress: Boolean = False): Variant;

// 内存转为变型
function BufferToVariant(Buffer: PAnsiChar; Size: Integer; ZipDecompress: Boolean = False): Variant;

// 变型转为内存
function VariantToBuffer(Data: Variant; var OutSize: Integer; ZipCompress: Boolean = False): Pointer;

// 取文件大小
function GetFileSize64(Handle: THandle): Int64;

// 简单的加密、解密
function EncryptString(const S: AnsiString): AnsiString;
function DecryptString(const S: AnsiString): AnsiString;

// 检查套接字的连接类型
function MatchSocketType(InBuf: PAnsiChar; const SocketFlag: AnsiString): Boolean;

// 数据集转为 json
function DataSetToJSON(DataSet: TDataSet; CharSet: THttpCharSet = hcsDefault): AnsiString;
procedure LargeDataSetToJSON(DataSet: TDataSet; Headers: TObject; CharSet: THttpCharSet);

// 取 UTC/TFileTime 时间
function GetUTCTickCount: Int64; 
function GetUTCTickCountEh(Seed: Pointer = nil): UInt64;

// 取本机、远程主机 IP
function GetLocalIp(): AnsiString;
function ResolveHostIP(const Host: AnsiString): AnsiString;

// 转换时长
function GetTimeLength(TimeLength: Cardinal): String;
function GetTimeLengthEx(TimeLength: Cardinal): String;

// 转换数据量到文本
function GetTransmitSpeed(const Value: Int64; const MaxValue: Int64 = 0): String;

// 取内存使用情况
function GetProcMemoryUsed: Cardinal;

// 取系统各种路径
function GetWindowsDir: String;
function GetSystemDir: String;
function GetProgramFileDir: String;
function GetProgramDataDir: String;
function GetSystemTempDir: String;

// 格式化日期
// procedure FormatNowToBuf(const Buf: PAnsiChar);
function FormatDataTimeNow: AnsiString;  // 未用

// 日期时间 格式
procedure IniDateTimeFormat;

// 注册、设置 Access ODBC DataSource（方便例子测试）
procedure RegMSAccessDSN(const DataSourceName, AccessFileName: String; const Description: String = '');
procedure SetMSAccessDSN(ADO: TADOConnection; DataSourceOrFileName: String; DSNFile: String = '');

// 取文件信息
procedure GetLocalFileInf(const FileName: string; var FileSize: TFileSize;
                          var CreationTime, AccessTime, LastWriteTime: TFileTime);

// 整理内存
procedure ClearSysMemory;

implementation

uses
  iocp_log, iocp_msgPacks, http_objects, iocp_senders;

function CreateSocket: TSocket;
begin
  // 新建一个 Socket(未出现过 INVALID_SOCKET）
  Result := iocp_Winsock2.WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP,
                                    nil, 0, WSA_FLAG_OVERLAPPED);
end;

function ConnectSocket(Socket: TSocket; const Server: AnsiString; Port: Word): Boolean;
var
  Addr: TSockAddrIn;
begin
  // Socket 连接到服务器
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr.s_addr := inet_addr(PAnsiChar(ResolveHostIP(Server)));

  Result := iocp_Winsock2.WSAConnect(Socket, TSockAddr(Addr), SizeOf(TSockAddr),
                                     nil, nil, nil, nil) = 0;
end;

function GetCPUCount: Integer;
var
  SysInfo: TSystemInfo;
begin
  // 取 CPU 数
  FillChar(SysInfo, SizeOf(SysInfo), 0);
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;

function GetErrMessage(ErrorCode: DWORD): String;
var
  Buffer: array[0..255] of Char;
var
  Len: Integer;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or
                       FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrorCode, 0, Buffer,
                       SizeOf(Buffer), nil);
  while (Len > 0) and {$IFDEF USE_UNICODE}
    CharInSet(Buffer[Len - 1], [#0..#32, '.']) {$ELSE}
   (Buffer[Len - 1] in [#0..#32, '.']) {$ENDIF} do
    Dec(Len);
  SetString(Result, Buffer, Len);
end;

function GetSysErrorMessage(ErrorCode: DWORD): String;
begin
  if ErrorCode = 0 then
    Result := GetErrMessage(GetLastError)
  else
    Result := GetErrMessage(ErrorCode);
end;

function GetWSAErrorMessage(ErrorCode: DWORD): String;
begin
  if ErrorCode = 0 then
    Result := GetErrMessage(WSAGetLastError)
  else
    Result := GetErrMessage(ErrorCode);
end;

function AddBackslash(const Path: AnsiString): AnsiString;
begin
  if (Path <> '') and (Path[Length(Path)] <> '\') then
    Result := Path + '\'
  else
    Result := Path;
end;

function DropBackslash(const Path: AnsiString): AnsiString;
begin
  if (Path <> '') and (Path[Length(Path)] = '\') then
    Result := Copy(Path, 1, Length(Path) - 1)
  else
    Result := Path;
end;

function GetTimeLength(TimeLength: Cardinal): String;
var
  DivVal: Cardinal;
begin
  // 计算时长
  if TimeLength >= 31536000 then       // 年
  begin
    DivVal := TimeLength div 31536000;
    if DivVal > 0 then
    begin
      TimeLength := TimeLength - DivVal * 31536000;
      Result := IntToStr(DivVal) + '年';
    end;
  end;

  if TimeLength >= 86400 then         // 天
  begin
    DivVal := TimeLength div 86400;
    if DivVal > 0 then
    begin
      TimeLength := TimeLength - DivVal * 86400;
      Result := Result + IntToStr(DivVal) + '天';
    end;
  end;

  if TimeLength >= 3600 then          // 时
  begin
    DivVal := TimeLength div 3600;
    if DivVal > 0 then
    begin
      TimeLength := TimeLength - DivVal * 3600;
      Result := Result + IntToStr(DivVal) + '时';
    end;
  end;

  if TimeLength >= 60 then            // 分
  begin
    DivVal := TimeLength div 60;
    if DivVal > 0 then
    begin
      TimeLength := TimeLength - DivVal * 60;
      Result := Result + IntToStr(DivVal) + '分';
    end;
  end;

  if TimeLength >= 1 then            // 秒
    Result := Result + IntToStr(TimeLength) + '秒';

end;

function GetTimeLengthEx(TimeLength: Cardinal): String;
type
  TTimeRang = record
    Index: Cardinal;
    Name: string[2];
  end;
const
  TIME_RANGES: array[0..4] of TTimeRang = (
    (Index: 31536000; Name: '年'),
    (Index: 86400; Name: '天'), (Index: 3600; Name: '时'),
    (Index: 60; Name: '分'), (Index: 1; Name: '秒')
  );
var
  i: Integer;
  DivVal: Cardinal;
begin
  // 计算时长（2）
  Result := '';
  for i := 0 to 4 do
    if TimeLength >= TIME_RANGES[i].Index then
      if i <= 3 then
      begin
        DivVal := TimeLength div TIME_RANGES[i].Index;
        if DivVal > 0 then
        begin
          TimeLength := TimeLength - DivVal * TIME_RANGES[i].Index;
          Result := Result + IntToStr(DivVal) + TIME_RANGES[i].Name;
        end;
      end else          // 秒
        Result := Result + IntToStr(TimeLength) + TIME_RANGES[i].Name;
end;

function GetTransmitSpeed(const Value, MaxValue: Int64): String;
var
  gCount: Double;
  function CalculateValue(const InValue: Int64): String;
  begin
    // 计算传输量
    case InValue of
      0:
        Result := '0';
      1..1023:
        Result := IntToStr(InValue);
      1024..1024575:         // 1024*1024=1024576
        Result := FloatToStrF(InValue / 1024, ffFixed, 15, 2);
      1024576..1073741823:   // 1024*1024*1024=1073741824
        Result := FloatToStrF(InValue / 1024576, ffFixed, 15, 2);
      else begin
        gCount := InValue / 1073741824;
        if gCount < 1024 then
          Result := FloatToStrF(gCount, ffFixed, 15, 2)
        else
          Result := FloatToStrF(gCount / 1024, ffFixed, 15, 2);
      end;
    end;
  end;
  function CalculateValue2(const InValue: Int64): String;
  begin
    case InValue of
      0:
        Result := '0';
      1..1023:
        Result := IntToStr(InValue) + 'b';
      1024..1024575:         // 1024*1024=1024576
        Result := FloatToStrF(InValue / 1024, ffFixed, 15, 2) + 'kb';
      1024576..1073741823:   // 1024*1024*1024=1073741824
        Result := FloatToStrF(InValue / 1024576, ffFixed, 15, 2) + 'mb';
      else begin
        gCount := InValue / 1073741824;
        if gCount < 1024 then
          Result := FloatToStrF(gCount, ffFixed, 15, 2) + 'gb'
        else
          Result := FloatToStrF(gCount / 1024, ffFixed, 15, 2) + 'tb';
      end;
    end;
  end;
begin
  // 计算传输量
  if (MaxValue = 0) then
    Result := CalculateValue2(Value)
  else
    Result := CalculateValue(Value) + '/' + CalculateValue2(MaxValue);
end;          

function GetProcMemoryUsed: Cardinal;
var
  Info: PPROCESS_MEMORY_COUNTERS;
  ProcHandle: HWND;
begin
  // 查询当前进程的内存使用大小
  Result := 0;
  ProcHandle := 0;
  Info := New(PPROCESS_MEMORY_COUNTERS);
  Info^.cb := SizeOf(_PROCESS_MEMORY_COUNTERS);
  try
    //由 CurrentProcessId 取得进程对象的句柄
    ProcHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                              False, GetCurrentProcessId);
    if GetProcessMemoryInfo(ProcHandle, Info, Info^.cb) then
      Result := Info^.WorkingSetSize;
  finally
    if (ProcHandle <> 0) then
      CloseHandle(ProcHandle);
    Dispose(Info);
  end;
end;

function GetLocalIP: AnsiString;
var
  Ent: PHostEnt;
  Host: array of AnsiChar;
begin
  // 取本机第一个 IP
  SetLength(Host, 128);
  try
    iocp_Winsock2.GetHostName(PAnsiChar(Host), 128);
    Ent := iocp_Winsock2.GetHostByName(PAnsiChar(Host));
    Result := IntToStr(Byte(Ent^.h_addr^[0])) + '.' +
              IntToStr(Byte(Ent^.h_addr^[1])) + '.' +
              IntToStr(Byte(Ent^.h_addr^[2])) + '.' +
              IntToStr(Byte(Ent^.h_addr^[3]));
  finally
    SetLength(Host, 0);
  end;
end;

function ResolveHostIP(const Host: AnsiString): AnsiString;
  function CheckIP(const S: AnsiString): Boolean;
  var
    i: Integer;
  begin
    for i := 1 to Length(S) do
      if not (S[i] in ['0'..'9', '.']) then
      begin
        Result := False;
        Exit;
      end;
    Result := True;
  end;
var
  Ent: PHostEnt;
begin
  // 取主机 IP: AnsiString
  Result := '0.0.0.0';
  if (Host <> '') then
    if CheckIP(Host) then
    begin
      if inet_addr(PAnsiChar(Host)) <> INADDR_NONE then
        Result := Host;
    end else
    begin
      Ent := iocp_Winsock2.GetHostByName(PAnsiChar(Host));
      if (Ent <> nil) then  // 取第一个
        Result := IntToStr(Byte(Ent^.h_addr^[0])) + '.' +
                  IntToStr(Byte(Ent^.h_addr^[1])) + '.' +
                  IntToStr(Byte(Ent^.h_addr^[2])) + '.' +
                  IntToStr(Byte(Ent^.h_addr^[3]));
    end;
end;

type
  TSystemPath = (
    spWindows,
    spSystem,
    spProgramFile,
    spProgramData,
    spSystemTemp
  );

procedure InternalGetPath(PathType: TSystemPath; var Path: String);
begin
  SetLength(Path, 256);
  case PathType of
    spWindows:
      GetWindowsDirectory(PChar(Path), 256);    // 后面不带 '\'
    spSystem:
      GetSystemDirectory(PChar(Path), 256);     // 后面不带 '\'
    spProgramFile:
      with TRegistry.Create do begin
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey('software\microsoft\windows\currentversion', False) then
          Path := ReadString('ProgramFilesDir');
        Free;
      end;
    spProgramData:
      with TRegistry.Create do begin
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', False) then
          Path := ReadString('Common AppData'); 
        Free;
      end;
    spSystemTemp:
      GetTempPath(256, PChar(Path));           // 后面带 '\'
  end;
  SetLength(Path, StrLen(PChar(Path)));
  if Copy(Path, Length(Path), 1) <> '\' then
    Path := Path + '\';
end;

function GetWindowsDir: String;
begin
  InternalGetPath(spWindows, Result);
end;

function GetSystemDir: String;
begin
  InternalGetPath(spSystem, Result);
end;

function GetProgramFileDir: String;
begin
  InternalGetPath(spProgramFile, Result);
end;

function GetProgramDataDir: String;
begin
  InternalGetPath(spProgramData, Result);
end;

function GetSystemTempDir: String;
begin
  InternalGetPath(spSystemTemp, Result);
end;

function MyCreateDir(const Path: String): Boolean;
begin
  // 建目录
  if DirectoryExists(Path) then
    Result := True
  else
    Result := ForceDirectories(Path);
end;

function FileTimeToDateTime(AFileTime: TFileTime): TDateTime;
var
 SysTime: TSystemTime;
 Temp: TFileTime;
begin
  // 转换文件的时间到 delphi 格式
  FileTimeToLocalFileTime(AFileTime, Temp);
  FileTimeToSystemTime(Temp, SysTime);
  Result := SystemTimeToDateTime(SysTime);
end;

function CreateNewFileName(const FileName: AnsiString): AnsiString;
var
  i: Integer;
begin
  // 在已有文件名基础上调整名称
  for i := Length(FileName) downto 1 do
    if (FileName[i] = '.') then
    begin
      Result := Copy(FileName, 1, i - 1) + '_' + IntToStr(GetTickCount) +
                Copy(FileName, i, 99);
      Exit;
    end;
  Result := FileName;
end;

function GetCompressionLevel(const FileName: String): TZipLevel;
var
  Ext: String;
begin
  // 返回压缩率（已压缩的文件不再压缩传输、二进制文件的压缩率低）
  Ext := UpperCase(ExtractFileExt(FileName));
  if (Ext = '.RAR')  or (Ext = '.ZIP')  or (Ext = '.7Z')   or
     (Ext = '.JPG')  or (Ext = '.DOCX') or (Ext = '.XLSX') or
     (Ext = '.PPTX') or (Ext = '.TAR')  or (Ext = '.CAB')  or
     (Ext = '.GZIP') or (Ext = '.BZ2')  or (Ext = '.JAR')  or
     (Ext = '.ISO')  or (Ext = '.GHO')  or (Ext = '.UUE')  or
     (Ext = '.ACE')  or (Ext = '.LZH')  or (Ext = '.ARJ')  or
     (Ext = '.EXE')  or (Ext = '.AVI')  or (Ext = '.VDI')  or 
     (Ext = '.DAT')  or (Ext = '.Z')
  then
    Result := zcNone
  else
    Result := zcDefault;
end;

function InternalOpenFile(const FileName: String; ReadOnly: Boolean): THandle;
begin
  // 打开文件
  //   不能使用：OPEN_EXISTING or CREATE_ALWAYS
  //   带参数 FILE_FLAG_OVERLAPPED 时，ReadFile 要 Overlapped，否则异常。
  if ReadOnly then
    Result := CreateFile(PChar(FileName), GENERIC_READ,
                         FILE_SHARE_READ, nil, OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0)
  else
    Result := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
                         FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if (Result = INVALID_HANDLE_VALUE) then
    Result := INVALID_FILE_HANDLE;
end;

function ExtractValue(var LeftValue, ValueList: AnsiString): Boolean;
var
  i: Integer;
  p: PAnsiChar;
begin
  if (Length(ValueList) = 0) then
  begin
    Result := False;
    Exit;
  end;
  
  p := PAnsiChar(ValueList);
  for i := 1 to Length(ValueList) do
  begin
    if (p^ in [',', ';']) then
      Break;
    Inc(p);
  end;

  LeftValue := Copy(ValueList, 1, i - 1);
  Delete(ValueList, 1, i);

  Result := (LeftValue <> '');
  
end;

function InternalOpenMsgFile(const FileName: String; AutoCreate: Boolean): THandle;
var
  iFlag, iValue: Cardinal;
begin
  // 打开、新建离线消息文件，新建时写入文件标志 OFFLINE_MS_FLAG
  //   消息文件大于 2m 则换文件！
  Result := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,  // 允许写
                       0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (Result = INVALID_HANDLE_VALUE) then  // 新建文件
  begin
    if AutoCreate then
    begin
      iFlag := OFFLINE_MSG_FLAG;
      Result := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
                           0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      WriteFile(Result, iFlag, SizeOf(Integer), iValue, nil);
    end;
  end else
  begin
    iFlag := 0;
    ReadFile(Result, iFlag, SizeOf(Integer), iValue, nil);
    if (iFlag <> OFFLINE_MSG_FLAG) then  // 标志错误
    begin
      CloseHandle(Result);
      Result := INVALID_HANDLE_VALUE;
    end else
    if (GetFileSize(Result, nil) > 2014000) then
    begin
      // 文件太大，改名，新建消息文件
      CloseHandle(Result);
      RenameFile(FileName, FileName + '@' + IntToStr(GetUTCTickCount));
      Result := InternalOpenMsgFile(FileName, AutoCreate);
    end;
  end;
  if (Result = INVALID_HANDLE_VALUE) then
    Result := INVALID_FILE_HANDLE;  
end;

function GetFileSize64(Handle: THandle): Int64;
var
  Size: ULARGE_INTEGER;
begin
  // 取文件大小
  Size.LowPart := GetFileSize(Handle, @Size.HighPart);
  Result := Size.QuadPart;
end;

function VariantToStream(Data: Variant; ZipCompress: Boolean; const FileName: String): TStream;
var
  p: Pointer;
  iSize, ZipSize: Integer;
  OutBuffer: Pointer;
begin
  // 把变长类型数据转为数据流，支持压缩，可以为文件流
  //   Data 不要传入 String 等其他类型数据!

  if VarIsNull(Data) then
  begin
    Result := nil;
    Exit;
  end;

  if (FileName = '') then  // 用In内存流
    Result := TInMemStream.Create
  else  // 用文件流
    Result := TFileStream.Create(FileName, fmCreate);

  iSize := VarArrayHighBound(Data, 1) - VarArrayLowBound(Data, 1) + 1;
  p := VarArrayLock(Data);

  try
    if ZipCompress then  // 直接压缩内容，把压缩后内存挂到 Result 处
    begin
      iocp_zlib.ZCompress(p, iSize, OutBuffer, ZipSize, zcDefault);
      if (FileName = '') then
        TInMemStream(Result).SetMemory(OutBuffer, ZipSize)
      else begin
        Result.WriteBuffer(OutBuffer^, ZipSize);
        FreeMem(OutBuffer);
      end;
    end else
      Result.Write(p^, iSize);
//    TInMemStream(Result).SaveToFile('q.dat');
  finally
    VarArrayUnlock(Data);
  end;
  
end;

function StreamToVariant(Stream: TStream; ZipDecompress: Boolean): Variant;
var
  Source: TStream;
  iSize: Integer;
  p: Pointer;
begin
  // 把流转换为 varByte Variant 类型（数据集或 Delta）
  if Assigned(Stream) then
  begin
    iSize := Stream.Size;
    if (iSize > 0) then
    begin
      if ZipDecompress then  // 先解压
      begin
        Stream.Position := 0;
        Source := TMemoryStream.Create;
        iocp_zlib.ZDecompressStream(Stream, Source);
        iSize := Source.Size;
      end else
        Source := Stream;

      Result := VarArrayCreate([0, iSize - 1], varByte);
      p := VarArrayLock(Result);

      try
        Source.Position := 0;
        Source.Read(p^, iSize);
      finally
        if ZipDecompress then
          Source.Free;
        VarArrayUnlock(Result);
      end;
      
    end else
      Result := NULL;
  end else
    Result := NULL;
end;

function BufferToVariant(Buffer: PAnsiChar; Size: Integer; ZipDecompress: Boolean): Variant;
var
  p, Data, OutBuf: Pointer;
  OutSize: Integer;
begin
  // 内存转为 Variant，支持压缩
  
  if ZipDecompress then  // 压缩 Buffer
  begin
    ZDecompress(Buffer, Size, OutBuf, OutSize, Size * 5);
    Data := OutBuf;
    Size := OutSize;
  end else
    Data := Buffer;

  // 建 varByte-Variant
  Result := VarArrayCreate([0, Size - 1], varByte);
  p := VarArrayLock(Result);

  try
    System.Move(Data^, p^, Size);  // 写入数据
  finally
    VarArrayUnlock(Result);
    if ZipDecompress then
      FreeMem(OutBuf, OutSize);
  end;
      
end;

function VariantToBuffer(Data: Variant; var OutSize: Integer; ZipCompress: Boolean): Pointer;
var
  p: Pointer;
  iSize: Integer;
begin
  // 把变长类型数据转到内存快，支持压缩
  //   Data 不要传入 String 等其他类型数据!

  if VarIsNull(Data) then
  begin
    Result := nil;
    Exit;
  end;

  iSize := VarArrayHighBound(Data, 1) - VarArrayLowBound(Data, 1) + 1;
  p := VarArrayLock(Data);

  try
    if ZipCompress then  // 压缩内容到 Result
      ZCompress(p, iSize, Result, OutSize, zcDefault)
    else begin
      OutSize := iSize;
      GetMem(Result, OutSize);
      System.Move(p^, Result^, OutSize);
    end;
  finally
    VarArrayUnlock(Data);
  end;
  
end;

function EncryptString(const S: AnsiString): AnsiString;
var
  i: Integer;
  p: PByte;
begin
  Result := S;
  p := PByte(Result);
  for i := 1 to Length(S) do
  begin
    case i of
      1, 3, 5, 7, 9:
        p^ := p^ xor Byte(19);
      2, 4, 6, 8, 10:
        p^ := p^ xor Byte(37);
      else
        p^ := p^ xor Byte(51);
    end;
    Inc(p);
  end;
end;

function DecryptString(const S: AnsiString): AnsiString;
begin
  Result := EncryptString(S);
end;

function MatchSocketType(InBuf: PAnsiChar; const SocketFlag: AnsiString): Boolean;
var
  i: Integer;
begin
  // 检查协议头标志：IOCP_SOCKET_FLAG
  for i := 1 to Length(SocketFlag) do
  begin
    if (InBuf^ <> SocketFlag[i]) then
    begin
      Result := False;
      Exit;
    end;
    Inc(InBuf);
  end;
  Result := True;
end;

procedure InterDataSetToJSON(DataSet: TDataSet; Headers: THttpResponseHeaders;
                             List: TInStringList; CharSet: THttpCharSet);
  function CharSetText(const S: AnsiString): AnsiString;
  begin
    // 根据字符集转换字符串
    case CharSet of
      hcsUTF8:
        Result := System.UTF8Encode(S); // 浏览器的 JSON Object 未必正常显示，要转换
      hcsURLEncode:
        Result := http_utils.InURLEncode(S); // 浏览器 AJAX 要转换正常
      else
        Result := S;
    end;
  end;
var
  BufLength: Integer;
  i, k, n, m, Idx: integer;
  p: PAnsiChar;
  Desc, JSON: AnsiString;
  Names: TStringAry;
  Field: TField;
begin
  // 快速把数据集转为 JSON（Blob字段内容未读入）
  //  1. Headers <> nil, 分块发送
  //  2. List <> nil, 保存到列表

  // 注意：每字段长度不能超过 IO_BUFFER_SIZE

  if not DataSet.Active or DataSet.IsEmpty then
    Exit;
    
  Dataset.DisableControls;
  Dataset.First;

  try
    // 1. 先保存字段描述到数组（字段名区分大小写）

    n := 5;  // 整条记录的 JSON 长度，开始为 Length('["},]')
    k := Dataset.FieldCount;

    SetLength(Names, k);
    for i := 0 to k - 1 do
    begin
      Field := Dataset.Fields[i];
      if (i = 0) then
      begin
        Desc := '{"' + CharSetText(LowerCase(Field.FieldName)) + '":"';  // 用小写
      end else
        Desc := '","' + CharSetText(LowerCase(Field.FieldName)) + '":"';
      Names[i] := Desc;
      Inc(n, Length(Desc) + Field.Size + 10);
    end;

    // 2. 每条记录转为 JSON，缓存满时发送 或 加入列表

    if Assigned(Headers) then
    begin
      BufLength := IO_BUFFER_SIZE - 8;
      Headers.ChunkSize(0);  // 初始化
      Headers.Append('[', False);
    end else
    begin
      BufLength := 0;
      if Assigned(List) and (List.Size > 0) then
        List.Clear;
    end;

    while not Dataset.Eof do
    begin
      SetLength(JSON, n);  // 预设记录空间
      p := PAnsiChar(JSON);
      Idx := 0;  // 内容的实际长度

      for i := 0 to k - 1 do
      begin
        Field := Dataset.Fields[i];
        if (i = k - 1) then  // [{"Id":"1","Name":"我"},{"Id":"2","Name":"你"}]
          Desc := Names[i] + CharSetText(Field.Text) + '"}'
        else
          Desc := Names[i] + CharSetText(Field.Text);
        m := Length(Desc);

        if (Idx + m > n) then  // 调整记录空间
        begin
          n := Idx + m + 50;
          SetLength(JSON, n);
          p := PAnsiChar(JSON) + Idx;
        end;

        System.Move(Desc[1], p^, m);

        Inc(p, m);  // 指针+
        Inc(Idx, m);  // 累计+
      end;

      Dataset.Next;   // 下一条
      
      if Assigned(List) then
      begin
        Delete(JSON, Idx + 1, n - Idx);   // 删除多余内容
        List.Add(JSON); // 加入列表
      end else
      begin
        Inc(Idx);  // 要增加记录后紧跟符号 , 或 ]
        Delete(JSON, Idx + 1, n - Idx);   // 删除多余内容（最后预留分隔符空间）

        if (Headers.Size + Idx > BufLength) then  // 超过分块最大长度，先发送
        begin
          Headers.ChunkSize(Headers.Size);  // 设置分块长度
          Headers.Owner.SendBuffers;  // 发送
          Headers.ChunkSize(0);  // 复位
        end;

        // 加入，下次满时发送
        if Dataset.Eof then
        begin
          JSON[Idx] := ']';  // 结束符
          Headers.Append(JSON, False);
        end else
        begin
          JSON[Idx] := ',';  // 未结束
          Headers.Append(JSON, False);
        end;
      end;

    end;

    if Assigned(Headers) then
    begin
      if (Headers.Size > 0) then
      begin
        Headers.ChunkSize(Headers.Size);  // 设置分块长度
        Headers.Owner.SendBuffers;  // 发送
        Headers.Clear;  // 清除
      end;
      Headers.ChunkDone;
      Headers.Owner.SendBuffers;  // 发送
      Headers.Clear;
    end;

  finally
    Dataset.EnableControls;
  end;

end;

function DataSetToJSON(DataSet: TDataSet; CharSet: THttpCharSet): AnsiString;
var
  List: TInStringList;
begin
  // 快速把数据集全部记录转换为 JSON
  List := TInStringList.Create;
  try
    InterDataSetToJSON(DataSet, nil, List, CharSet);
    Result := List.JSON;  // 合并 JSON 记录
  finally
    List.Free;
  end;
end;

procedure LargeDataSetToJSON(DataSet: TDataSet; Headers: TObject; CharSet: THttpCharSet);
begin
  // 用分块方法发送大数据集 JSON
  InterDataSetToJSON(DataSet, THttpResponseHeaders(Headers), nil, CharSet);
end;

function GetUTCTickCount: Int64;
var
  UtcFt: _FILETIME;
begin
  // 精确到 100ns = 千万分之一秒
  //   返回与 GetTickCount 一样的毫秒
  GetSystemTimeAsFileTime(UtcFt);
  Result := (Int64(UtcFt) div 10000); // 1,000,000,0
end;

function GetUTCTickCountEh(Seed: Pointer): UInt64;
var
  UtcFt: _FILETIME;
begin
  // 精确到千万分之一秒，与 Seed 运算产生唯一值
  GetSystemTimeAsFileTime(UtcFt);
  if (Seed <> nil) then
    {$IFDEF WIN_64}
    UInt64(UtcFt) := UInt64(UtcFt) xor UInt64(Seed);
    {$ELSE}
    UtcFt.dwLowDateTime := UtcFt.dwLowDateTime xor LongWord(Seed);
    {$ENDIF}
  Result := UInt64(UtcFt);
end;

procedure FormatNowToBuf(const Buf: PAnsiChar);
type
  TDblChar = array[0..1] of AnsiChar;

  PDateTimeFormat = ^TDateTimeFormat;
  TDateTimeFormat = packed record
      Year: TDblChar; Year2: TDblChar; S: AnsiChar;
     Month: TDblChar;    S2: AnsiChar;
       Day: TDblChar;    S3: AnsiChar;
      Hour: TDblChar;    S4: AnsiChar;
    Minute: TDblChar;    S5: AnsiChar;
    Second: TDblChar;    S6: AnsiChar;
      MSec: TDblChar; MSec2: AnsiChar
  end;

const
  DBL_NUMBERS: array[0..99] of TDblChar = (
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
  Data: PDateTimeFormat;
  i: Integer;
begin
  // 转换时间格式为字符串，支持双字节，先在外部设置：
  //   S: String = '2017-11-05 11:12:52 321'; 传入 PChar(@S[1])

  GetLocalTime(SysTime);

  Data := PDateTimeFormat(Buf);

  // 年
  i := SysTime.wYear div 100;
  Data^.Year := DBL_NUMBERS[i];
  Data^.Year2 := DBL_NUMBERS[SysTime.wYear - i * 100];

  // 月、日
  Data^.Month := DBL_NUMBERS[SysTime.wMonth];
  Data^.Day := DBL_NUMBERS[SysTime.wDay];

  // 时、分、秒
  Data^.Hour := DBL_NUMBERS[SysTime.wHour];
  Data^.Minute := DBL_NUMBERS[SysTime.wMinute];
  Data^.Second := DBL_NUMBERS[SysTime.wSecond];

  // 毫秒
  i := SysTime.wMilliseconds div 10;
  Data^.MSec := DBL_NUMBERS[i];
  Data^.MSec2 := DBL_NUMBERS[SysTime.wMilliseconds - i * 10][1];

end;  

function FormatDataTimeNow: AnsiString;
begin
  Result := '2017-11-05 11:12:52 321';
  FormatNowToBuf(PAnsiChar(@Result[1]));
end;

procedure IniDateTimeFormat;
begin
  {$IFDEF DELPHI_XE}     // xe 或更高
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  {$ELSE}
  DateSeparator := '-';
  TimeSeparator := ':';
  ShortDateFormat := 'yyyy-mm-dd';
  {$ENDIF}
end;

procedure GetLocalFileInf(const FileName: String; var FileSize: TFileSize;
                          var CreationTime, AccessTime, LastWriteTime: TFileTime);
var
  Data: TWin32FindData;
  Handle: THandle;
begin
  // 取文件大小（低32位，不允许大文件传输）、时间
  Handle := FindFirstFile(PChar(FileName), Data);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    if (Data.nFileSizeHigh = 0) then
      FileSize := Data.nFileSizeLow
    else  // MAXDWORD = DWORD($FFFFFFFF);
      FileSize := Int64($FFFFFFFF) + Data.nFileSizeLow + 1;
    CreationTime := Data.ftCreationTime;
    AccessTime := Data.ftLastAccessTime;
    LastWriteTime := Data.ftLastWriteTime;
    {$IFDEF DELPHI_XE7UP}
    Winapi.Windows.FindClose(Handle); {$ELSE}
    Windows.FindClose(Handle); {$ENDIF}
  end;
end;

procedure RegMSAccessDSN(const DataSourceName, AccessFileName, Description: String);
const
  DRIVER_KEY_32 = 'Microsoft Access Driver (*.mdb)';
  DRIVER_KEY_64 = 'Microsoft Access Driver (*.mdb, *.accdb)';
var
  Reg: TRegistry;
  Driver, DriverKey: String;
begin
  // 设置 Access ODBC
  // 驱动文件位置，32 位：Microsoft Access Driver (*.mdb)
  //               64 位：Microsoft Access Driver (*.mdb, *.accdb)
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE; // 根 HKEY_LOCAL_MACHINE

  try
    // 取驱动文件、名称
    if Reg.OpenKey('Software\ODBC\ODBCINST.INI\' + DRIVER_KEY_64, False) then
    begin
      Driver := Reg.ReadString('Driver');
      DriverKey := DRIVER_KEY_64;
    end else
    if Reg.OpenKey('Software\ODBC\ODBCINST.INI\' + DRIVER_KEY_32, False) then
    begin
      Driver := Reg.ReadString('Driver');
      DriverKey := DRIVER_KEY_32;
    end;

    Reg.CloseKey;
    if Reg.OpenKey('Software\ODBC\ODBC.INI\' + DataSourceName, True) then
    begin
      Reg.WriteString('DBQ', AccessFileName);  // 数据库文件
      Reg.WriteString('Description', Description); // 描述
      Reg.WriteString('Driver', Driver);  // 驱动
      Reg.WriteInteger('DriverId', 25);   // 驱动标识
      Reg.WriteString('FIL', 'Ms Access;');    // Filter 依据
      Reg.WriteInteger('SafeTransaction', 0);  // 事务操作数
      Reg.WriteString('UID', '');  // 用户名称

      Reg.CloseKey;
      if Reg.OpenKey('Software\ODBC\ODBC.INI\' + DataSourceName + '\Engines\Jet', True) then
      begin
        Reg.WriteString('ImplicitCommitSync', 'Yes');
        Reg.WriteInteger('MaxBufferSize', 512); // 缓冲区大小
        Reg.WriteInteger('PageTimeout', 10); // 页超时
        Reg.WriteInteger('Threads', 3);  // 支持的线程数
        Reg.WriteString('UserCommitSync', 'Yes');

        Reg.CloseKey;
        if Reg.OpenKey('Software\ODBC\ODBC.INI\ODBC Data Sources', True) then
          Reg.WriteString(DataSourceName, DriverKey);
      end;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

procedure SetMSAccessDSN(ADO: TADOConnection; DataSourceOrFileName, DSNFile: String);
const
  CONNECTION_STR = 'Provider=MSDASQL.1;Persist Security Info=False;Extended Properties="DBQ=%s;DefaultDir=%s;Driver={Microsoft Access Driver (*.mdb)};';
  CONNECTION_STR2 = 'DriverId=25;FIL=MS Access;FILEDSN=%s;MaxBufferSize=2048;MaxScanRows=8;PageTimeout=5;SafeTransactions=0;Threads=3;UID=admin;UserCommitSync=Yes;"';
  CONNECTION_STR3 = 'Provider=MSDASQL.1;Persist Security Info=False;Data Source=';
begin
  // 设置 Access ADO 连接
  if ADO.Connected then
    ADO.Connected := False;
  if (DSNFile = '') then // 用 ODBC
    ADO.ConnectionString := CONNECTION_STR3 + DataSourceOrFileName
  else   // 用 DatabaseFile + DSN FileName
    ADO.ConnectionString := Format(CONNECTION_STR + CONNECTION_STR2,
        [DataSourceOrFileName, ExtractFilePath(DataSourceOrFileName), DSNFile]);
end;

procedure ClearSysMemory;
begin
  // 清理内存，减少内存占有量（移至缓存）
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF);
end;

end.
