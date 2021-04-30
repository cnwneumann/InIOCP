//
// icop 的数据压缩单元
//   主要摘自 Zlib 目录下相应单元，有修改
//

unit iocp_zlib;

interface

uses
  Classes, SysUtils, DateUtils, ZLibEx, ZLibExApi, ZLibExGZ;

 type

   TZipLevel = (    // = TZCompressionLevel
    zcNone,
    zcFastest,
    zcDefault,
    zcMax,
    zcLevel1,
    zcLevel2,
    zcLevel3,
    zcLevel4,
    zcLevel5,
    zcLevel6,
    zcLevel7,
    zcLevel8,
    zcLevel9
  );

// Zip：尝试压缩一段内存到 outBuffer
function TryZCompress(const inBuffer: Pointer; inSize: Integer;
                      const outBuffer: Pointer; var outSize: Integer;
                      Level: TZipLevel): Boolean;

// Zip：压缩一段内存到 outBuffer
procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
                    out outBuffer: Pointer; out outSize: Integer;
                    Level: TZipLevel);

// Zip：解压一段内存到 outBuffer（估算长度 outEstimate）
procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
                      out outBuffer: Pointer; out outSize: Integer;
                      outEstimate: Integer = 0);

// Zip：压缩流
procedure ZCompressStream(inStream, outStream: TStream;
                          level: TZipLevel = zcDefault);

// Zip：解压流
procedure ZDecompressStream(inStream, outStream: TStream);

// ===============================================================

// GZip：压缩一段内存到 outBuffer
procedure GZCompress(const inBuffer: Pointer; inSize: Integer;
                     out outBuffer: Pointer; out outSize: Integer;
                     Level: TZipLevel);

// GZip：压缩文件、流
procedure GZCompressFile(const SourceFileName, TargetFileName: String);
procedure GZCompressStream(inStream, outStream: TStream; const FileName: String = '');

// GZip: 解压流，返回文件名称
procedure GZDecompressStream(inStream, outStream: TStream; var FileName: String);

implementation

function ZCompressCheck(code: Integer): Integer;
begin
  result := code;
  if code < 0 then
    raise Exception.Create('压缩异常');
end;

function ZDecompressCheck(code: Integer; raiseBufferError: Boolean = True): Integer;
begin
  result := code;
  if code < 0 then
    if (code <> Z_BUF_ERROR) or raiseBufferError then
      raise Exception.Create('解压异常');
end;

procedure ZInternalCompress(var zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer);
const
  delta = 256;
var
  zresult: Integer;
begin
  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;

  outBuffer := Nil;

  try
    try
      zstream.next_in := inBuffer;
      zstream.avail_in := inSize;

      repeat
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PByte(NativeUInt(outBuffer) + zstream.total_out);
        zstream.avail_out := NativeUInt(outSize) - zstream.total_out;

        zresult := ZCompressCheck(deflate(zstream, ZFlushes[zfNoFlush]));

        Inc(outSize, delta);
      until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

      while zresult <> Z_STREAM_END do
      begin
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PByte(NativeUInt(outBuffer) + zstream.total_out);
        zstream.avail_out := NativeUInt(outSize) - zstream.total_out;

        zresult := ZCompressCheck(deflate(zstream, ZFlushes[zfFinish]));

        Inc(outSize, delta);
      end;
    finally             
      ZCompressCheck(deflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);

    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZInternalDecompress(zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer;
  outEstimate: Integer);
var
  zresult: Integer;
  delta  : Integer;
begin
  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then       // 估计数
    outSize := delta
  else
    outSize := outEstimate;

  outBuffer := Nil;

  try
    try
      zresult := Z_OK;

      zstream.avail_in := inSize;
      zstream.next_in := inBuffer;

      while (zresult <> Z_STREAM_END) and (zstream.avail_in > 0) do
        repeat
          ReallocMem(outBuffer, outSize);
                  
          zstream.next_out := PByte(NativeUInt(outBuffer) + zstream.total_out);
          zstream.avail_out := NativeUInt(outSize) - zstream.total_out;

          zresult := ZDecompressCheck(inflate(zstream, ZFlushes[zfNoFlush]), False);

          Inc(outSize, delta);
        until (zresult = Z_STREAM_END) or (zstream.avail_out > 0);
    finally
      ZDecompressCheck(inflateEnd(zstream));
    end;
    
    ReallocMem(outBuffer, zstream.total_out);

    outSize := zstream.total_out;
  except
    if Assigned(outBuffer) then
      FreeMem(outBuffer);  
    raise;
  end;
end;

procedure ZInternalCompressStream(zStream: TZStreamRec; inStream, outStream: TStream);
const
  bufferSize = 32768;
var
  zresult  : Integer;
  inBuffer : Array [0..bufferSize - 1] of Byte;
  outBuffer: Array [0..bufferSize - 1] of Byte;
  outSize  : Integer;
begin
  zresult := Z_STREAM_END;

  zstream.avail_in := inStream.Read(inBuffer, bufferSize);

  while zstream.avail_in > 0 do
  begin
    zstream.next_in := @inBuffer;

    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;

      zresult := ZCompressCheck(deflate(zStream, ZFlushes[zfNoFlush]));

      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

    zstream.avail_in := inStream.Read(inBuffer, bufferSize);
  end;

  while zresult <> Z_STREAM_END do
  begin
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(deflate(zstream, ZFlushes[zfFinish]));

    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  end;

  ZCompressCheck(deflateEnd(zstream));
end;

procedure ZInternalDecompressStream(zstream: TZStreamRec; inStream, outStream: TStream);
const
  bufferSize = 32768;
var
  zresult  : Integer;
  inBuffer : Array [0..bufferSize-1] of Byte;
  outBuffer: Array [0..bufferSize-1] of Byte;
  outSize  : Integer;
begin
  try
    zresult := Z_OK;

    zstream.avail_in := inStream.Read(inBuffer, bufferSize);

    while (zresult <> Z_STREAM_END) and (zstream.avail_in > 0) do
    begin
      zstream.next_in := @inBuffer;

      repeat
        zstream.next_out := @outBuffer;
        zstream.avail_out := bufferSize;

        zresult := ZDecompressCheck(inflate(zstream, ZFlushes[zfNoFlush]), False);

        outSize := bufferSize - zstream.avail_out;

        outStream.Write(outBuffer, outSize);
      until (zresult = Z_STREAM_END) or (zstream.avail_out > 0);

      if zstream.avail_in > 0 then
      begin
        inStream.Position := inStream.Position - zstream.avail_in;
      end;

      if zresult <> Z_STREAM_END then
      begin
        zstream.avail_in := inStream.Read(inBuffer, bufferSize);
      end;
    end;
  finally
    ZDecompressCheck(inflateEnd(zstream));
  end;
end;

function TryZCompress(const inBuffer: Pointer; inSize: Integer;
                      const outBuffer: Pointer; var outSize: Integer;
                      Level: TZipLevel): Boolean;
var
  TempBuffer: Pointer;
  ZipSize: Integer;
begin
  // 尝试压缩一段内存 inBuffer 到 outBuffer
  // outSize: 传入的内存长度，压缩后改为压缩数据长度
  TempBuffer := nil;
  try
    ZCompress(inBuffer, inSize, TempBuffer, ZipSize, Level);
    if (ZipSize < outSize) then
    begin
      // 压缩后数据变少
      System.Move(TempBuffer^, outBuffer^, ZipSize);
      outSize := ZipSize;
      Result := True;
    end else
    begin
      // 放弃压缩
      outSize := inSize;
      System.Move(inBuffer^, outBuffer^, inSize);
      Result := False;
    end;
  finally
    if Assigned(TempBuffer) then
      FreeMem(TempBuffer);
  end;
end;

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
                    out outBuffer: Pointer; out outSize: Integer;
                    Level: TZipLevel);
var
  zStream: TZStreamRec;
begin
  FillChar(zStream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(deflateInit_(zStream, ZLevels[TZCompressionLevel(level)], ZLIB_VERSION, SizeOf(TZStreamRec)));

  ZInternalCompress(zStream, inBuffer, inSize, outBuffer, outSize);
end;

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
          out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer = 0);
var
  zStream: TZStreamRec;
begin
  FillChar(zStream, SizeOf(TZStreamRec), 0);

  ZDecompressCheck(inflateInit_(zStream, ZLIB_VERSION, SizeOf(TZStreamRec)));

  ZInternalDecompress(zStream, inBuffer, inSize, outBuffer, outSize, outEstimate);
end;

procedure ZCompressStream(inStream, outStream: TStream; level: TZipLevel);
var
  zStream: TZStreamRec;
begin
  FillChar(zStream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(deflateInit_(zStream, ZLevels[TZCompressionLevel(level)], ZLIB_VERSION, SizeOf(TZStreamRec)));

  ZInternalCompressStream(zStream, inStream, outStream);
end;

procedure ZDecompressStream(inStream, outStream: TStream);
var
  zStream: TZStreamRec;
begin
  FillChar(zStream, SizeOf(TZStreamRec), 0);

  ZDecompressCheck(inflateInit_(zStream, ZLIB_VERSION, SizeOf(TZStreamRec)));

  ZInternalDecompressStream(zStream, inStream, outStream);
end;

procedure GZCompress(const inBuffer: Pointer; inSize: Integer;
                     out outBuffer: Pointer; out outSize: Integer;
                     Level: TZipLevel);
var
  header : TGZHeader;
  trailer: TGZTrailer;
  oBuffer: Pointer;
  zStream: TZStreamRec;
begin
  // GZip 压缩一个数据包
  //  （压缩后的空间可能变大）

  // 1. 准备头
//  FillChar(header, SizeOf(TGZHeader), 0);   // 优化删除

  header.Id1 := $1F;
  header.Id2 := $8B;
  header.Method := Z_DEFLATED;

  header.ExtraFlags := GZ_EXTRA_DEFAULT;
  header.Flags := 0;
  header.OS := 0;
  header.Time := 0;
  
  // 2. 计算 CRC32
//  FillChar(trailer, SizeOf(TGZTrailer), 0);  // 优化删除

  trailer.Crc := crc32(0, inBuffer^, inSize);
  trailer.Size := inSize;

  // 3. 压缩 inBuffer
  FillChar(zStream, SizeOf(TZStreamRec), 0);

  // .. 初始化
  ZCompressCheck(deflateInit2_(zStream, ZLevels[TZCompressionLevel(level)], Z_DEFLATED,
                 GZ_ZLIB_WINDOWBITS, GZ_ZLIB_MEMLEVEL, Z_DEFAULT_STRATEGY,
                 ZLIB_VERSION, SizeOf(TZStreamRec)));

  // .. 压缩
  ZInternalCompress(zStream, inBuffer, inSize, oBuffer, outSize);

  // 4. 结果：头部 + oBuffer + 尾
  try
    GetMem(outBuffer, outSize + SizeOf(TGZHeader) + SizeOf(TGZTrailer));

    System.Move(header,   outBuffer^, SizeOf(TGZHeader));
    System.Move(oBuffer^, Pointer(LongWord(outBuffer) + SizeOf(TGZHeader))^, outSize);
    System.Move(trailer,  Pointer(LongWord(outBuffer) + SizeOf(TGZHeader) +
                          LongWord(outSize))^, SizeOf(TGZTrailer));

    Inc(outSize, SizeOf(TGZHeader) + SizeOf(TGZTrailer));
  finally
    FreeMem(oBuffer);
  end;

end;

procedure GZCompressFile(const SourceFileName, TargetFileName: String);
var
  Source, Target: TStream;
begin
  Source := TFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
  Target := TFileStream.Create(TargetFileName, fmCreate);
  try
    GZCompressStream(Source, Target);
  finally
    if Assigned(Source) then
      Source.Free;
    if Assigned(Target) then
      Target.Free;
  end;

end;

procedure GZCompressStream(inStream, outStream: TStream; const FileName: String);
const
  bufferSize = 32768;
var
  zStream  : TZStreamRec;
  header   : TGZHeader;
  trailer  : TGZTrailer;
  buffer   : Array [0..bufferSize-1] of Byte;
  count    : Integer;
  position : Int64;
  nilString: String;
begin
//  FillChar(header, SizeOf(TGZHeader), 0);  // 优化删除

  header.Id1 := $1F;
  header.Id2 := $8B;
  header.Method := Z_DEFLATED;

  header.ExtraFlags := GZ_EXTRA_DEFAULT;

  if (Length(FileName) > 0) then
    header.Flags := 0 or GZ_FILENAME
  else
    header.Flags := 0;

  header.OS := 0;
  header.Time := 0;  // DateTimeToUnix(Now);

//  FillChar(trailer, SizeOf(TGZTrailer), 0);  // 优化删除

  trailer.Crc := 0;
  position := inStream.Position;

  while inStream.Position < inStream.Size do
  begin
    count := inStream.Read(buffer[0],bufferSize);

    trailer.Crc := Crc32(trailer.Crc, buffer[0], count);
  end;

  inStream.Position := position;

  trailer.Size := inStream.Size - inStream.Position;  // mark

  outStream.Write(header, SizeOf(TGZHeader));

  if Length(filename) > 0 then
  begin
    nilString := fileName + #$00;

    outStream.Write(nilString[1], Length(nilString));
  end;

  // .. 压缩
  FillChar(zStream, SizeOf(TZStreamRec), 0);
                                                  
  ZCompressCheck(deflateInit2_(zStream, ZLevels[TZCompressionLevel(zcDefault)], Z_DEFLATED,
                 GZ_ZLIB_WINDOWBITS, GZ_ZLIB_MEMLEVEL, Z_DEFAULT_STRATEGY,
                 ZLIB_VERSION, SizeOf(TZStreamRec)));

  ZInternalCompressStream(zStream, inStream, outStream);

  outStream.Write(trailer, SizeOf(TGZTrailer));
end;

procedure GZDecompressStream(inStream, outStream: TStream; var FileName: String);
const
  bufferSize = 32768;
var
  header     : TGZHeader;
  trailer    : TGZTrailer;
  zStream    : TZStreamRec;
  buffer     : Array [0..bufferSize-1] of Byte;
  count      : Integer;
  position   : Int64;        // delphi 7 不能用
  endPosition: Int64;
  size       : Integer;
  crc        : Longint;
  c          : AnsiChar;
begin
  if inStream.Read(header,SizeOf(TGZHeader)) <> SizeOf(TGZHeader) then
    raise Exception.Create('解压异常');

  if (header.Id1 <> $1F)
    or (header.Id2 <> $8B)
    or (header.Method <> Z_DEFLATED)
    or ((header.Flags and GZ_RESERVED) <> 0) then
    raise Exception.Create('解压异常');

  if (header.Flags and GZ_EXTRA_FIELD) <> 0 then
  begin
    if inStream.Read(size,SizeOf(Word)) <> SizeOf(Word) then
      raise Exception.Create('解压异常');
    inStream.Position := inStream.Position + size;
  end;

  fileName := '';

  if (header.Flags and GZ_FILENAME) <> 0 then
  begin
    c := ' ';

    while (inStream.Position < inStream.Size) and (c <> #$00) do
    begin
      inStream.Read(c,1);

      if c <> #$00 then fileName := fileName + c;
    end;
  end;

  if (header.Flags and GZ_HEADER_CRC) <> 0 then
  begin
    // todo: validate header crc

    inStream.Position := inStream.Position + SizeOf(Word);
  end;

  if inStream.Position >= inStream.Size then
    raise Exception.Create('解压异常');

  position := outStream.Position;

  // 解压流
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZDecompressCheck(inflateInit2(zStream, GZ_ZLIB_WINDOWBITS));

  ZInternalDecompressStream(zStream, inStream, outStream);

  // ...
  
  endPosition := outStream.Position;

  if inStream.Read(trailer,SizeOf(TGZTrailer)) <> SizeOf(TGZTrailer) then
    raise Exception.Create('解压异常');;

  crc := 0;

  outStream.Position := position;

  while outStream.Position < endPosition do
  begin
    size := bufferSize;

    if size > (endPosition - outStream.Position) then
    begin
      size := endPosition - outStream.Position;
    end;

    count := outStream.Read(buffer[0], size);

    crc := Crc32(crc, buffer[0], count);
  end;

  if (trailer.Crc <> crc)
    or (trailer.Size <> Cardinal(endPosition - position)) then
    raise Exception.Create('解压异常');;
end;

end.
