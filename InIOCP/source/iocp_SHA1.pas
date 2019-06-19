(*=============================================================================
 *
 *                              SHA1 计 算
 *        来源：本代码摘自 CnPack
 *        说明：只保留 WebSocket 握手必须的 SHA1 计算
 *        优化: 高凉新农
 *
 =============================================================================*)

{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：SHA1算法单元
* 单元作者：刘啸（Liu Xiao）
* 备    注：
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnSHA1.pas 426 2010-02-09 07:01:49Z liuxiao $
* 修改记录：2015.08.14 V1.2
*               汇编切换至 Pascal 以支持跨平台
*           2014.10.22 V1.1
*               加入 HMAC 方法
*           2010.07.14 V1.0
*               创建单元。从网上佚名代码移植而来，加入部分功能
*
================================================================================
|</PRE>}

unit iocp_SHA1;

interface

{$I in_iocp.inc}

{$IF CompilerVersion >= 18.5}
{$DEFINE USE_INLINE}
{$ELSE}
{$DEFINE DELPHI_7}
{$IFEND}

uses
  {$IFDEF DELPHI_XE7UP}
  WinAPI.Windows, System.Classes, System.SysUtils {$ELSE}
  Windows, Classes, SysUtils {$ENDIF};

type

  PSHA1Hash   = ^TSHA1Hash;
  TSHA1Hash   = array[0..4] of DWORD;

  PHashRecord = ^THashRecord;
  THashRecord = record
    case Integer of
      0: (A, B, C, D, E: DWORD);
      1: (Hash: TSHA1Hash);
  end;
  
  PSHA1Digest = ^TSHA1Digest;
  TSHA1Digest = array[0..19] of Byte;

  PSHA1Block  = ^TSHA1Block;
  TSHA1Block  = array[0..63] of Byte;

  PSHA1Data   = ^TSHA1Data;
  TSHA1Data   = array[0..79] of DWORD;

  TSHA1Context = record
    Hash: TSHA1Hash;
    Hi, Lo: DWORD;
    Buffer: TSHA1Block;
    Index: Integer;
  end;

// 计算 AnsiString
function SHA1StringA(const Str: AnsiString): TSHA1Digest;

// 计算内存块
function SHA1StringB(const Buffers: Pointer; Len: Integer): TSHA1Digest;

// TSHA1Digest 转为 Base64，生成 WebSocket 握手的 WebSocket-Accept key
function EncodeBase64(const Digest: TSHA1Digest): String;

implementation

type
  PPacket = ^TPacket;
  TPacket = packed record
    case Integer of
      0: (b0, b1, b2, b3: Byte);
      1: (i: Integer);
      2: (a: array[0..3] of Byte);
      3: (c: array[0..3] of AnsiChar);
  end;

const
  SHA_INIT_HASH: TSHA1Hash = (
    $67452301, $EFCDAB89, $98BADCFE, $10325476, $C3D2E1F0
  );

  SHA_ZERO_BUFFER: TSHA1Block = (
    0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,  // 0..19
    0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,  // 20..39
    0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,  // 40..59
    0,0,0,0  // 60..63
  );

  EncodeTable: array[0..63] of AnsiChar =
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
               'abcdefghijklmnopqrstuvwxyz' +
               '0123456789+/';

function RB(A: DWORD): DWORD; {$IFDEF USE_INLINE} inline {$ENDIF}
begin
  Result := (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
end;

function LRot32(X: DWORD; c: Integer): DWORD; {$IFDEF USE_INLINE} inline {$ENDIF}
begin
  Result := X shl (c and 31) + X shr (32 - c and 31);
end;

procedure SHA1Init(var Context: TSHA1Context); {$IFDEF USE_INLINE} inline {$ENDIF}
begin
  Context.Hi := 0;
  Context.Lo := 0;
  Context.Index := 0;
  Context.Buffer := SHA_ZERO_BUFFER;
  Context.Hash := SHA_INIT_HASH;
end;
  
procedure SHA1UpdateLen(var Context: TSHA1Context; Len: Integer); {$IFDEF USE_INLINE} inline {$ENDIF}
var
  i, k: DWORD;
begin
  for k := 0 to 7 do
  begin
    i := Context.Lo;
    Inc(Context.Lo, Len);
    if Context.Lo < i then
      Inc(Context.Hi);
  end;
end;

procedure SHA1Compress(var Data: TSHA1Context; Block: PSHA1Data = nil; SetNull: Boolean = True);
  procedure _FW(var W: TSHA1Data; Bk: PSHA1Data); {$IFDEF USE_INLINE} inline {$ENDIF}
  var
    i: Integer;
  begin
    for i := 0 to 15 do
      W[i] := RB(Bk^[i]);
  end;
  function _F1(const HR: THashRecord): DWORD; {$IFDEF USE_INLINE} inline {$ENDIF}
  begin
    Result := HR.D xor (HR.B and (HR.C xor HR.D));
  end;
  function _F2(const HR: THashRecord): DWORD; {$IFDEF USE_INLINE} inline {$ENDIF}
  begin
    Result := HR.B xor HR.C xor HR.D;
  end;
  function _F3(const HR: THashRecord): DWORD; {$IFDEF USE_INLINE} inline {$ENDIF}
  begin
    Result := (HR.B and HR.C) or (HR.D and (HR.B or HR.C));
  end;
var
  i: Integer;
  T: DWORD;
  HR: THashRecord;
  W: TSHA1Data;
begin

  if (Block = nil) then
  begin
    _FW(W, PSHA1Data(@Data.Buffer));
    Data.Buffer := SHA_ZERO_BUFFER;  // 清空
  end else
    _FW(W, Block); 

  for i := 16 to 79 do  // 16-3=13, 79-16=63
    W[i] := LRot32(W[i - 3] xor W[i - 8] xor W[i - 14] xor W[i - 16], 1);

  HR.Hash := Data.Hash;

  for i := 0 to 19 do
  begin
    T := LRot32(HR.A, 5) + _F1(HR) + HR.E + W[i] + $5A827999;
    HR.E := HR.D;
    HR.D := HR.C;
    HR.C := LRot32(HR.B, 30);
    HR.B := HR.A;
    HR.A := T;
  end;

  for i := 20 to 39 do
  begin
    T := LRot32(HR.A, 5) + _F2(HR) + HR.E + W[i] + $6ED9EBA1;
    HR.E := HR.D;
    HR.D := HR.C;
    HR.C := LRot32(HR.B, 30);
    HR.B := HR.A;
    HR.A := T;
  end;

  for i := 40 to 59 do
  begin
    T := LRot32(HR.A, 5) + _F3(HR) + HR.E + W[i] + $8F1BBCDC;
    HR.E := HR.D;
    HR.D := HR.C;
    HR.C := LRot32(HR.B, 30);
    HR.B := HR.A;
    HR.A := T;
  end;

  for i := 60 to 79 do
  begin
    T := LRot32(HR.A, 5) + _F2(HR) + HR.E + W[i] + $CA62C1D6;
    HR.E := HR.D;
    HR.D := HR.C;
    HR.C := LRot32(HR.B, 30);
    HR.B := HR.A;
    HR.A := T;
  end;
  
  Inc(Data.Hash[0], HR.A);
  Inc(Data.Hash[1], HR.B);
  Inc(Data.Hash[2], HR.C);
  Inc(Data.Hash[3], HR.D);
  Inc(Data.Hash[4], HR.E);
end;   

procedure SHA1Update(var Context: TSHA1Context; Buffer: Pointer; Len: Integer);
var
  i: Integer;
begin
  SHA1UpdateLen(Context, Len);

  if (Context.Index = 0) then
    while (Len >= 64) do
    begin
      SHA1Compress(Context, PSHA1Data(Buffer), False);  // 直接传原数据
      Inc(PByte(Buffer), 64);
      Dec(Len, 64);      
    end;

  while (Len > 0) do
  begin
    i := 64 - Context.Index;  // 剩余空间
    if (Len < i)  then  // 剩余空间能容纳剩余内容
      i := Len;

    Move(Buffer^, Context.Buffer[Context.Index], i);
    Inc(PByte(Buffer), i);
    Inc(Context.Index, i);
    Dec(Len, i);

    if Context.Index = 64 then
    begin
      Context.Index := 0;
      SHA1Compress(Context);
    end;
  end;
end;

procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);
begin
  Context.Buffer[Context.Index] := $80;
  if Context.Index >= 56 then
    SHA1Compress(Context);

  PDWord(@Context.Buffer[56])^ := RB(Context.Hi);
  PDWord(@Context.Buffer[60])^ := RB(Context.Lo);
  SHA1Compress(Context);

  Context.Hash[0] := RB(Context.Hash[0]);
  Context.Hash[1] := RB(Context.Hash[1]);
  Context.Hash[2] := RB(Context.Hash[2]);
  Context.Hash[3] := RB(Context.Hash[3]);
  Context.Hash[4] := RB(Context.Hash[4]);
  
  Digest := PSHA1Digest(@Context.Hash)^;
end;

// ============================================================

function SHA1StringA(const Str: AnsiString): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  // 计算 AnsiString 的 SHA1 值
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(Str), Length(Str));
  SHA1Final(Context, Result);
end;

function SHA1StringB(const Buffers: Pointer; Len: Integer): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  // 计算 Buffers 的 SHA1 值
  SHA1Init(Context);
  SHA1Update(Context, Buffers, Len);
  SHA1Final(Context, Result);
end;

// ============================================================

procedure EncodePacket(const Packet: TPacket; NumChars: Integer; OutBuf: PAnsiChar);
begin
  OutBuf[0] := EnCodeTable[Packet.a[0] shr 2];
  OutBuf[1] := EnCodeTable[((Packet.a[0] shl 4) or (Packet.a[1] shr 4)) and $0000003f];
  if NumChars < 2 then
    OutBuf[2] := '='
  else
    OutBuf[2] := EnCodeTable[((Packet.a[1] shl 2) or (Packet.a[2] shr 6)) and $0000003f];
  if NumChars < 3 then
    OutBuf[3] := '='
  else
    OutBuf[3] := EnCodeTable[Packet.a[2] and $0000003f];
end;

function EncodeBase64(const Digest: TSHA1Digest): String;
var
  OutBuf: array[0..56] of AnsiChar;
  BufPtr: PAnsiChar;
  I, J, K, BytesRead: Integer;
  Packet: TPacket;
begin
  // 摘自 delphi 2007, 单元 encddecd.pas
  
  I := 0;
  K := 0;
  BytesRead := SizeOf(TSHA1Digest);
  BufPtr := OutBuf;

  while I < BytesRead do
  begin
    if BytesRead - I < 3 then
      J := BytesRead - I
    else
      J := 3;

    Packet.i := 0;
    Packet.b0 := Digest[I];

    if J > 1 then
      Packet.b1 := Digest[I + 1];
    if J > 2 then
      Packet.b2 := Digest[I + 2];

    EncodePacket(Packet, J, BufPtr);
    Inc(I, 3);
    Inc(BufPtr, 4);
    Inc(K, 4);

    if K > 75 then
    begin
      BufPtr[0] := #$0D;
      BufPtr[1] := #$0A;
      Inc(BufPtr, 2);
      K := 0;
    end;
  end;

  SetString(Result, OutBuf, BufPtr - PAnsiChar(@OutBuf));

end;

end.
