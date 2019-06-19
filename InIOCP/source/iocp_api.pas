(*
 * 常用 windows API
 *)
unit iocp_api;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows {$ELSE}
  Windows {$ENDIF};

const
  {$IFDEF DELPHI_7}

  HEAP_NO_SERIALIZE = $00000001;
  {$EXTERNALSYM HEAP_NO_SERIALIZE}
  HEAP_ZERO_MEMORY  = $00000008;
  {$EXTERNALSYM HEAP_ZERO_MEMORY}

  {$ENDIF}

  WT_EXECUTELONGFUNCTION = ULONG($00000010);

type
  {$IFDEF WIN64}
  UIntPtr = NativeUInt;    // Delphi 2007: SizeOf(NativeUInt) = 8
  {$ELSE}
  UIntPtr = Cardinal;      // Delphi 2007: SizeOf(Cardinal) = 4
  {$ENDIF}

// =================== Windows 函数 ===================

function CreateIoCompletionPort(FileHandle, ExistingCompletionPort: THandle;
  CompletionKey: ULONG_PTR; NumberOfConcurrentThreads: DWORD): THandle; stdcall;
{$EXTERNALSYM CreateIoCompletionPort}
function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred: DWORD; var lpCompletionKey: ULONG_PTR;
  var lpOverlapped: POverlapped; dwMilliseconds: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetQueuedCompletionStatus}
function PostQueuedCompletionStatus(CompletionPort: THandle; dwNumberOfBytesTransferred: DWORD;
  dwCompletionKey: UIntPtr; lpOverlapped: POverlapped): BOOL; stdcall;
{$EXTERNALSYM PostQueuedCompletionStatus}

{$IFDEF WIN64}
function InterlockedCompareExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer; stdcall; inline;
{$ELSE}
function InterlockedCompareExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer; stdcall;
{$ENDIF}
{$EXTERNALSYM InterlockedCompareExchange}

// 系统的任务队列
function QueueUserWorkItem(FuncName: TThreadStartRoutine; Context: Pointer; Flags: ULONG): BOOL; stdcall;
 
implementation

const
  kernel32 = 'kernel32.dll';

function CreateIoCompletionPort; external kernel32 name 'CreateIoCompletionPort';
function GetQueuedCompletionStatus; external kernel32 name 'GetQueuedCompletionStatus';
function PostQueuedCompletionStatus; external kernel32 name 'PostQueuedCompletionStatus';

{$IFDEF WIN64}
function InterlockedCompareExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer;
begin
  Result := AtomicCmpExchange(Destination, Exchange, Comparand);
end;
{$ELSE}
function InterlockedCompareExchange; external kernel32 name 'InterlockedCompareExchange';
{$ENDIF}

function QueueUserWorkItem; external kernel32 name 'QueueUserWorkItem';

end.
