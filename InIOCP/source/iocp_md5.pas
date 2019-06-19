(*
 * 计算 MD5（代码来自网络，有修改、优化，支持大文件）
 *)

unit iocp_md5;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.SysUtils, {$ELSE}
  Windows, SysUtils, {$ENDIF}
  iocp_utils;

type

  MD5Count  = array[0..01] of DWORD;
  MD5State  = array[0..03] of DWORD;
  MD5Block  = array[0..15] of DWORD;                   
  MD5CBits  = array[0..07] of byte;
  MD5Digest = array[0..15] of byte;
  MD5Buff   = array[0..63] of byte;

  TMD5Digest = MD5Digest;
  PMD5Digest = ^TMD5Digest;
  
  MD5Context = record
    State: MD5State;
    Count: MD5Count;
    Buffer: MD5Buff;
  end;

  // 加
  PIOCPHashCode = ^TIOCPHashCode;
  TIOCPHashCode = record
    case Integer of
      0: (MurmurHash, Tail: UInt64);  // = TMurmurHash
      1: (MD5Code: TMD5Digest);  // MD5 = 128 Bits
  end;

  {$IF CompilerVersion < 18.5}
  FILE_SIZE = Cardinal;
  {$ELSE}
  FILE_SIZE = Int64;
  {$IFEND}

function MD5Buffer(Buffer: PAnsiChar; Len: Integer): MD5Digest;
function MD5String(const S: AnsiString): MD5Digest;

function MD5File(const FileName: String): MD5Digest; overload; 
function MD5File(Handle: THandle): MD5Digest; overload;
function MD5Part(Handle: THandle; Offset: FILE_SIZE; PartSize: Cardinal): MD5Digest;

function MD5Match(const D1, D2: MD5Digest): Boolean;
function MD5MatchEx(D1: PMD5Digest; const D2: MD5Digest): Boolean;

function MD5Print(D: MD5Digest): AnsiString;  

implementation

var
  PADDING: MD5Buff = (
    $80, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
  );

function F(x, y, z: DWORD): DWORD; 
begin 
  Result := (x and y) or ((not x) and z);
end; 

function G(x, y, z: DWORD): DWORD; 
begin 
  Result := (x and z) or (y and (not z));
end; 

function H(x, y, z: DWORD): DWORD; 
begin 
  Result := x xor y xor z;
end; 

function I(x, y, z: DWORD): DWORD; 
begin 
  Result := y xor (x or (not z));
end; 

procedure rot(var x: DWORD; n: BYTE); 
begin 
  x := (x shl n) or (x shr (32 - n));
end; 

procedure FF(var a: DWORD; b, c, d, x: DWORD; s: BYTE; ac: DWORD); 
begin 
  inc(a, F(b, c, d) + x + ac);
  rot(a, s);
  inc(a, b);
end; 

procedure GG(var a: DWORD; b, c, d, x: DWORD; s: BYTE; ac: DWORD); 
begin 
  inc(a, G(b, c, d) + x + ac);
  rot(a, s);
  inc(a, b);
end; 

procedure HH(var a: DWORD; b, c, d, x: DWORD; s: BYTE; ac: DWORD); 
begin 
  inc(a, H(b, c, d) + x + ac);
  rot(a, s);
  inc(a, b);
end; 

procedure II(var a: DWORD; b, c, d, x: DWORD; s: BYTE; ac: DWORD); 
begin 
  inc(a, I(b, c, d) + x + ac);
  rot(a, s);
  inc(a, b);
end; 

procedure Encode(Source, Target: pointer; Count: longword); 
var 
  S: PByte;
  T: PDWORD;
  I: longword;
begin 
  S := Source;
  T := Target;
  for I := 1 to Count div 4 do begin
    T^ := S^;
    inc(S);
    T^ := T^ or (S^ shl 8);
    inc(S);
    T^ := T^ or (S^ shl 16);
    inc(S);
    T^ := T^ or (S^ shl 24);
    inc(S);
    inc(T);
  end;
end;

procedure Decode(Source, Target: pointer; Count: longword);
var
  S: PDWORD;
  T: PByte;
  I: longword;
begin 
  S := Source;
  T := Target;
  for I := 1 to Count do
  begin
    T^ := S^ and $ff;
    inc(T);
    T^ := (S^ shr 8) and $ff;
    inc(T);
    T^ := (S^ shr 16) and $ff;
    inc(T);
    T^ := (S^ shr 24) and $ff;
    inc(T);
    inc(S);
  end;
end;

procedure Transform(Buffer: pointer; var State: MD5State); {$IFDEF USE_INLINE} inline; {$ENDIF}
var 
  a, b, c, d: DWORD;
  Block: MD5Block;
begin
  Encode(Buffer, @Block, 64);
  a := State[0];
  b := State[1];
  c := State[2];
  d := State[3];
  FF(a, b, c, d, Block[ 0], 7,  $d76aa478);
  FF(d, a, b, c, Block[ 1], 12, $e8c7b756); 
  FF(c, d, a, b, Block[ 2], 17, $242070db);
  FF(b, c, d, a, Block[ 3], 22, $c1bdceee);
  FF(a, b, c, d, Block[ 4], 7,  $f57c0faf);
  FF(d, a, b, c, Block[ 5], 12, $4787c62a);
  FF(c, d, a, b, Block[ 6], 17, $a8304613); 
  FF(b, c, d, a, Block[ 7], 22, $fd469501);
  FF(a, b, c, d, Block[ 8], 7,  $698098d8);
  FF(d, a, b, c, Block[ 9], 12, $8b44f7af); 
  FF(c, d, a, b, Block[10], 17, $ffff5bb1);
  FF(b, c, d, a, Block[11], 22, $895cd7be);
  FF(a, b, c, d, Block[12], 7,  $6b901122);
  FF(d, a, b, c, Block[13], 12, $fd987193);
  FF(c, d, a, b, Block[14], 17, $a679438e);
  FF(b, c, d, a, Block[15], 22, $49b40821); 
  GG(a, b, c, d, Block[ 1], 5,  $f61e2562);
  GG(d, a, b, c, Block[ 6], 9,  $c040b340);
  GG(c, d, a, b, Block[11], 14, $265e5a51); 
  GG(b, c, d, a, Block[ 0], 20, $e9b6c7aa); 
  GG(a, b, c, d, Block[ 5], 5,  $d62f105d);
  GG(d, a, b, c, Block[10], 9,  $2441453);
  GG(c, d, a, b, Block[15], 14, $d8a1e681);
  GG(b, c, d, a, Block[ 4], 20, $e7d3fbc8);
  GG(a, b, c, d, Block[ 9], 5,  $21e1cde6);
  GG(d, a, b, c, Block[14], 9,  $c33707d6);
  GG(c, d, a, b, Block[ 3], 14, $f4d50d87); 
  GG(b, c, d, a, Block[ 8], 20, $455a14ed); 
  GG(a, b, c, d, Block[13], 5,  $a9e3e905);
  GG(d, a, b, c, Block[ 2], 9,  $fcefa3f8);
  GG(c, d, a, b, Block[ 7], 14, $676f02d9);
  GG(b, c, d, a, Block[12], 20, $8d2a4c8a);
  HH(a, b, c, d, Block[ 5], 4,  $fffa3942);
  HH(d, a, b, c, Block[ 8], 11, $8771f681);
  HH(c, d, a, b, Block[11], 16, $6d9d6122);
  HH(b, c, d, a, Block[14], 23, $fde5380c);
  HH(a, b, c, d, Block[ 1], 4,  $a4beea44);
  HH(d, a, b, c, Block[ 4], 11, $4bdecfa9);
  HH(c, d, a, b, Block[ 7], 16, $f6bb4b60);
  HH(b, c, d, a, Block[10], 23, $bebfbc70);
  HH(a, b, c, d, Block[13], 4,  $289b7ec6);
  HH(d, a, b, c, Block[ 0], 11, $eaa127fa); 
  HH(c, d, a, b, Block[ 3], 16, $d4ef3085); 
  HH(b, c, d, a, Block[ 6], 23, $4881d05);
  HH(a, b, c, d, Block[ 9], 4,  $d9d4d039);
  HH(d, a, b, c, Block[12], 11, $e6db99e5);
  HH(c, d, a, b, Block[15], 16, $1fa27cf8);
  HH(b, c, d, a, Block[ 2], 23, $c4ac5665); 
  II(a, b, c, d, Block[ 0], 6,  $f4292244);
  II(d, a, b, c, Block[ 7], 10, $432aff97);
  II(c, d, a, b, Block[14], 15, $ab9423a7);
  II(b, c, d, a, Block[ 5], 21, $fc93a039);
  II(a, b, c, d, Block[12], 6,  $655b59c3);
  II(d, a, b, c, Block[ 3], 10, $8f0ccc92);
  II(c, d, a, b, Block[10], 15, $ffeff47d); 
  II(b, c, d, a, Block[ 1], 21, $85845dd1); 
  II(a, b, c, d, Block[ 8], 6,  $6fa87e4f);
  II(d, a, b, c, Block[15], 10, $fe2ce6e0); 
  II(c, d, a, b, Block[ 6], 15, $a3014314); 
  II(b, c, d, a, Block[13], 21, $4e0811a1);
  II(a, b, c, d, Block[ 4], 6,  $f7537e82); 
  II(d, a, b, c, Block[11], 10, $bd3af235);
  II(c, d, a, b, Block[ 2], 15, $2ad7d2bb);
  II(b, c, d, a, Block[ 9], 21, $eb86d391);
  inc(State[0], a);
  inc(State[1], b);
  inc(State[2], c);
  inc(State[3], d);
end;

procedure MD5Init(var Context: MD5Context);
begin 
  with Context do
  begin
    State[0] := $67452301;
    State[1] := $efcdab89;
    State[2] := $98badcfe;
    State[3] := $10325476;
    Count[0] := 0;
    Count[1] := 0;
    ZeroMemory(@Buffer, SizeOf(MD5Buff));
  end;
end; 

procedure MD5Update(var Context: MD5Context; Input: PAnsiChar; Length: longword);
var 
  Index: longword;
  PartLen: longword;
  I: longword;
begin 
  with Context do
  begin
    Index := (Count[0] shr 3) and $3f;
    inc(Count[0], Length shl 3);
    if Count[0] < (Length shl 3) then inc(Count[1]);
    inc(Count[1], Length shr 29);
  end;

  PartLen := 64 - Index;
  if Length >= PartLen then
  begin
    CopyMemory(@Context.Buffer[Index], Input, PartLen);
    Transform(@Context.Buffer, Context.State);
    I := PartLen;
    while I + 63 < Length do
    begin
      Transform(@Input[I], Context.State);
      inc(I, 64);
    end;
    Index := 0;
  end else
    I := 0;

  CopyMemory(@Context.Buffer[Index], @Input[I], Length - I);
end;

procedure MD5Final(var Context: MD5Context; var Digest: MD5Digest);
var
  Bits: MD5CBits;
  Index: longword;
  PadLen: longword;
begin 
  Decode(@Context.Count, @Bits, 2);
  Index := (Context.Count[0] shr 3) and $3f;
  if Index < 56 then
    PadLen := 56 - Index
  else
    PadLen := 120 - Index;

  MD5Update(Context, @PADDING, PadLen);
  MD5Update(Context, @Bits, 8);
  Decode(@Context.State, @Digest, 4);

  ZeroMemory(@Context, SizeOf(MD5Context));
end;

function MD5Buffer(Buffer: PAnsiChar; Len: Integer): MD5Digest;
var
  Context: MD5Context;
begin
  // 计算内存块的 MD5
  MD5Init(Context);
  MD5Update(Context, Buffer, Len);
  MD5Final(Context, Result);
end;

function MD5String(const S: AnsiString): MD5Digest;
var 
  Context: MD5Context;
begin 
  // 计算字符串的 MD5
  MD5Init(Context);
  MD5Update(Context, PAnsiChar(S), Length(S));
  MD5Final(Context, Result);
end; 

function MD5File(const FileName: string): MD5Digest;
var
  Handle: THandle;
begin
  // 计算文件的 MD5（支持大文件）
  Handle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
                       nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  try
    Result := MD5File(Handle);
  finally
    if (Handle <> INVALID_HANDLE_VALUE) then
      CloseHandle(Handle);
  end;
end;

function MD5File(Handle: THandle): MD5Digest;
var
  Inf: SYSTEM_INFO;
  MapHandle: THandle;
  FileSize, Offset: FILE_SIZE;
  Granularity, PartSize: Cardinal;
  MapOffset: LARGE_INTEGER;
  ViewPointer: Pointer;
  Context: MD5Context;
begin
  // 计算文件的 MurmurHash（支持大文件）
  // https://blog.csdn.net/chenxing888/article/details/5912183

  MD5Init(Context);

  if (Handle > 0) and (Handle <> INVALID_HANDLE_VALUE) then
  begin
    MapHandle := CreateFileMapping(Handle, nil, PAGE_READONLY, 0, 0, nil);
    if (MapHandle <> 0) then
      try
        {$IFDEF DELPHI_7}
        GetSystemInfo(Inf);  // 取内存分配精度
        {$ELSE}
        GetSystemInfo(&Inf);  // 取内存分配精度
        {$ENDIF}

        FileSize := GetFileSize64(Handle);
        Granularity := Inf.dwAllocationGranularity * 100;

        Offset := 0;
        while (FileSize > 0) do
        begin
          if (FileSize > Granularity) then
            PartSize := Granularity
          else
            PartSize := FileSize;

          // 映射: 位移=Offset, 长度=MapSize
          MapOffset.QuadPart := Offset;
          ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ,
                                       MapOffset.HighPart, MapOffset.LowPart,
                                       PartSize);

          Inc(Offset, PartSize);
          Dec(FileSize, PartSize);

          if (ViewPointer <> nil) then
            try
              MD5Update(Context, ViewPointer, PartSize);
            finally
              UnmapViewOfFile(ViewPointer);
            end;
        end;
      finally
        CloseHandle(MapHandle);
      end;
  end;

  MD5Final(Context, Result);

end;

function MD5Part(Handle: THandle; Offset: FILE_SIZE; PartSize: Cardinal): MD5Digest;
var
  MapHandle: THandle;
  MapOffset: LARGE_INTEGER;
  ViewPointer: Pointer;
  Context: MD5Context;
begin
  // 计算文件范围的 MD5（支持大文件）

  MD5Init(Context);

  if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) or (PartSize = 0) then
  begin
    MD5Final(Context, Result);
    Exit;
  end;

  MapHandle := CreateFileMapping(Handle, nil, PAGE_READONLY, 0, 0, nil);
  if (MapHandle <> 0) then
    try
      // 映射范围: 位移=Offset, 长度=PartSize
      MapOffset.QuadPart := Offset;
      ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ,
                                   MapOffset.HighPart, MapOffset.LowPart,
                                   PartSize);
      if (ViewPointer <> nil) then
        try
          MD5Update(Context, ViewPointer, PartSize);
        finally
          UnmapViewOfFile(ViewPointer);
        end;

    finally
      CloseHandle(MapHandle);
    end;

  MD5Final(Context, Result);

end;

function MD5Match(const D1, D2: MD5Digest): boolean;
var
  i: byte;
begin
  // 比较 MD5
  for i := 0 to 15 do
    if D1[i] <> D2[i] then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

function MD5MatchEx(D1: PMD5Digest; const D2: MD5Digest): Boolean;  // 比较 MD5
 var
  PD2: PIOCPHashCode;
begin
  // 比较 MD5
  PD2 := PIOCPHashCode(@D2);
  Result := (PIOCPHashCode(D1)^.MurmurHash = PD2^.MurmurHash) and
            (PIOCPHashCode(D1)^.Tail = PD2^.Tail);
end;

function MD5Print(D: MD5Digest): AnsiString;
const
  Digits: array[0..15] of AnsiChar =
     ('0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i, k: byte;
begin
  // 转换 MD5 到字符串
  Result := '012345678901234567890123456789AB';
  for i := 0 to 15 do
  begin
    k := i * 2 + 1;
    Result[k] := Digits[(D[I] shr 4) and $0f];
    Result[k + 1] := Digits[D[I] and $0f];
  end;
end;

end.

