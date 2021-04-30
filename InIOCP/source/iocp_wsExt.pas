//===============================
//
//   AcceptEx 模式使用的单元
//
//===============================

unit iocp_wsExt;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, Winapi.PsAPI, {$ELSE}
  Windows, Classes, SysUtils, PsAPI, {$ELSE}
  {$ENDIF} iocp_Winsock2;

const
  // transmit file flag values
  {$EXTERNALSYM TF_DISCONNECT}
  TF_DISCONNECT      = $01;
  {$EXTERNALSYM TF_REUSE_SOCKET}
  tf_reuse_socket    = $02;
  {$EXTERNALSYM TF_WRITE_BEHIND}
  tf_write_behind    = $04;
  {$EXTERNALSYM TF_USE_DEFAULT_WORKER}
  tf_use_default_worker = $00;
  {$EXTERNALSYM TF_USE_SYSTEM_THREAD}
  tf_use_system_thread = $10;
  {$EXTERNALSYM TF_USE_KERNEL_APC}
  tf_use_kernel_apc   = $20;

{ Other NT-specific options. }

  {$EXTERNALSYM SO_MAXDG}
  SO_MAXDG        = $7009;
  {$EXTERNALSYM SO_MAXPATHDG}
  SO_MAXPATHDG    = $700A;
  {$EXTERNALSYM SO_UPDATE_ACCEPT_CONTEXT}
  SO_UPDATE_ACCEPT_CONTEXT = $700B;
  {$EXTERNALSYM SO_CONNECT_TIME}
  SO_CONNECT_TIME = $700C;

type

  // TransmitFile 发送内存结构

  PTransmitFileBuffers = ^TTransmitFileBuffers;
  TTransmitFileBuffers = packed record
    Head: Pointer;       // 先发送的数据
    HeadLength: DWORD;
    Tail: Pointer;       // 最后发送的数据
    TailLength: DWORD;
  end;
  PTransmitBuffers = PTransmitFileBuffers;

  // ============== TCP 心跳包 ==============
  // TCP 心跳包结构

  PTCPKeepAlive = ^TTCPKeepAlive;
  TTCPKeepAlive = record
    OnOff: Integer;              // 是否启用
    KeepAliveTime: Integer;      // 心跳周期
    KeepAliveInterVal: Integer;  // 多长时间无心跳即探测
  end;

  TAcceptEx = function(sListenSocket, sAcceptSocket: TSocket;
         lpOutputBuffer: Pointer; dwReceiveDataLength, dwLocalAddressLength,
         dwRemoteAddressLength: LongWord; var lpdwBytesReceived: LongWord;
         lpOverlapped: POverlapped): BOOL; stdcall;

  TGetAcceptExSockAddrs = procedure(lpOutputBuffer: Pointer;
         dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
         var LocalSockaddr: PSockAddr; var LocalSockaddrLength: Integer;
         var RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: Integer);stdcall;

  TConnectEx = function(const hSocket: TSocket; const name: PSOCKADDR; const
         namelen: Integer; lpSendBuffer: Pointer; dwSendDataLength : DWORD; var
         lpdwBytesSent: LongWord; lpOverlapped: POverlapped): BOOL; stdcall;

  TDisconnectEx = function(const hSocket: TSocket; lpOverlapped: POverlapped;
         const dwFlags: LongWord; const dwReserved: LongWord) : BOOL; stdcall;

  TTransmitFileProc = function(hSocket: TSocket; hFile: THandle;
         nNumberOfBytesToWrite, nNumberOfBytesPerSend: DWORD; lpOverlapped: POVERLAPPED;
         lpTransmitBuffers: PTransmitFileBuffers; dwFlags: DWORD): BOOL; stdcall;

var
  gAcceptEx: TAcceptEx = nil;
  gGetAcceptExSockAddrs: TGetAcceptExSockAddrs = nil;
  gConnectEx: TConnectEx = nil;
  gDisconnectEx: TDisconnectEx = nil;
  gTransmitFile: TTransmitFileProc = nil;

procedure GetWSExtFuncs(Socket: TSocket);
function SetKeepAlive(Socket: TSocket; InterValue: Integer = 8000): Boolean;

implementation

uses
  iocp_log, iocp_base, iocp_utils, iocp_msgPacks, iocp_Server;

const
  WSAID_ACCEPTEX: System.TGuid =
    (D1:$b5367df1;D2:$cbac;D3:$11cf;D4:($95,$ca,$00,$80,$5f,$48,$a1,$92));
  WSAID_GETACCEPTEXSOCKADDRS: System.TGuid =
    (D1:$b5367df2;D2:$cbac;D3:$11cf;D4:($95,$ca,$00,$80,$5f,$48,$a1,$92));
  WSAID_CONNECTEX: System.TGuid =
    (D1:$25a207b9;D2:$ddf3;D3:$4660;D4:($8e,$e9,$76,$e5,$8c,$74,$06,$3e));
  WSAID_DISCONNECTEX: System.TGuid =
    (D1:$7fda2e11;D2:$8630;D3:$436f;D4:($a0,$31,$f5,$36,$a6,$ee,$c1,$57));
  WSAID_TRANSMITFILE: System.TGuid =
    (D1:$B5367DF0;D2:$CBAC;D3:$11CF;D4:($95,$CA,$00,$80,$5F,$48,$A1,$92));

procedure GetWSExtFuncs(Socket: TSocket);
var
  ByteCount: Cardinal;
begin
  // 查询 Socket 扩展函数（AcceptEx 模式）

//  Socket := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_IP, nil, 0, WSA_FLAG_OVERLAPPED);

  if (WSAIoctl(Socket, SIO_GET_EXTENSION_FUNCTION_POINTER,
              @WSAID_ACCEPTEX, SizeOf(WSAID_ACCEPTEX),
              @@gAcceptEx, SizeOf(Pointer),
              ByteCount, nil, nil) = SOCKET_ERROR) then
  begin
    @gAcceptEx := nil;
    {$IFDEF DEBUG_MODE}
    iocp_log.WriteLog('GetWSExtFuncs->AcceptEx Error.');
    {$ENDIF}
    Exit;
  end;

  if (WSAIoctl(Socket, SIO_GET_EXTENSION_FUNCTION_POINTER,
               @WSAID_GETACCEPTEXSOCKADDRS, SizeOf(WSAID_GETACCEPTEXSOCKADDRS),
               @@gGetAcceptExSockAddrs, SizeOf(Pointer),
               ByteCount, nil, nil) = SOCKET_ERROR) then
  begin
    @gGetAcceptExSockAddrs := nil;
    {$IFDEF DEBUG_MODE}
    iocp_log.WriteLog('GetWSExtFuncs->GetAcceptExSockAddrs Error.');
    {$ENDIF}
    Exit;
  end;

  if (WSAIoctl(Socket, SIO_GET_EXTENSION_FUNCTION_POINTER,
               @WSAID_CONNECTEX, SizeOf(WSAID_CONNECTEX),
               @@gConnectEx, SizeOf(Pointer),
               ByteCount, nil, nil) = SOCKET_ERROR) then
  begin
    @gConnectEx := nil;
    {$IFDEF DEBUG_MODE}
    iocp_log.WriteLog('GetWSExtFuncs->ConnectEx Error.');
    {$ENDIF}
    Exit;                                                  
  end;

  if (WSAIoctl(Socket, SIO_GET_EXTENSION_FUNCTION_POINTER,
               @WSAID_DISCONNECTEX, SizeOf(WSAID_DISCONNECTEX),
               @@gDisconnectEx, SizeOf(Pointer),
               ByteCount, nil, nil) = SOCKET_ERROR) then
  begin
    @gDisconnectEx := nil;
    {$IFDEF DEBUG_MODE}
    iocp_log.WriteLog('GetWSExtFuncs->DisconnectEx Error.');
    {$ENDIF}
    Exit;
  end;

  if (WSAIoctl(Socket, SIO_GET_EXTENSION_FUNCTION_POINTER,
               @WSAID_TRANSMITFILE, SizeOf(WSAID_TRANSMITFILE),
               @@gTransmitFile, SizeOf(Pointer),
               ByteCount, nil, nil) = SOCKET_ERROR) then
  begin
    @gTransmitFile := nil;
    {$IFDEF DEBUG_MODE}  
    iocp_log.WriteLog('GetWSExtFuncs->TransmitFile Error.');
    {$ENDIF}
    Exit;
  end;
end;

function SetKeepAlive(Socket: TSocket; InterValue: Integer): Boolean;
var
  Opt: Integer;
  OutByte: DWORD;
  InSize, OutSize: Integer;
  KeepAliveIn, KeepAliveOut: TTCPKeepAlive;
begin
  // 设置 Socket 心跳
  Opt := 1;
  if (iocp_Winsock2.SetSockopt(Socket, SOL_SOCKET, SO_KEEPALIVE,
                               @Opt, SizeOf(Integer)) <> SOCKET_ERROR) then
  begin
    KeepAliveIn.OnOff := 1;
    KeepAliveIn.KeepAliveTime := InterValue;  // 心跳周期
    if (InterValue <= 8000) then
      KeepAliveIn.KeepAliveInterVal := 10000  // 10 秒无心跳即探测
    else
      KeepAliveIn.KeepAliveInterVal := InterValue + 2000;   // 超2秒无心跳即探测

    InSize := SizeOf(TTCPKeepAlive);
    OutSize := SizeOf(TTCPKeepAlive);

    Result := (iocp_Winsock2.WSAIoctl(Socket, IOC_IN or IOC_VENDOR or 4, // = SIO_KEEPALIVE_VALS
                             @KeepAliveIn, InSize, @KeepAliveOut, OutSize,
                             OutByte, nil, nil) <> SOCKET_ERROR);
  end else
    Result := False;
end;

end.
