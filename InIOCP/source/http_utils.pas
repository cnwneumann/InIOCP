(*
 * http 服务常用函数
 *)
unit http_utils;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.SysUtils, System.DateUtils, {$ELSE}
  Windows, SysUtils, DateUtils, {$ENDIF}
  http_base;

function AdjustFileName(const FileName: AnsiString): AnsiString;
function ChangeSlash(const FileName: AnsiString): AnsiString;

function CreateBoundary: AnsiString;
function CompareBuffer(Buf: PAnsiChar; const S: AnsiString; IgnoreCase: Boolean = False): Boolean;

function DecodeHexText(const Text: AnsiString): AnsiString;
function ExtractFieldInf(var Buf: PAnsiChar; Len: Integer; var FieldValue: AnsiString): TFormElementType;

function GetFieldType(const ParamVal: AnsiString): THttpFieldType;
function GetContentType(const FileName: AnsiString): AnsiString;
function GetHexSize(Size: Integer): AnsiString; 

function GetHttpGMTDateTime: AnsiString; overload;
function GetHttpGMTDateTime(const FileTime: TFileTime): AnsiString; overload;
function GetLocalDateTime(const HttpGMTDateTime: AnsiString): TDateTime;

function FindInBuffer(Buf: PAnsiChar; Len: Integer; const Text: AnsiString): Boolean;
function FindInBuffer2(Buf: PAnsiChar; Len: Integer; const Text: AnsiString): Boolean;

function SearchInBuffer(var Buf: PAnsiChar; Len: Integer; const Text: AnsiString): Boolean;
function SearchInBuffer2(var Buf: PAnsiChar; Len: Integer; const Text: AnsiString): Boolean;

function URLDecode(const URL: AnsiString): AnsiString;
function URLEncode(const URL: AnsiString): AnsiString;

implementation

uses
  iocp_utils;
                 
// =======================================

function AdjustFileName(const FileName: AnsiString): AnsiString;
var
  i: Integer;
  p, p2: PAnsiChar;
  IsSlash: Boolean;
begin
  // 调整文件名称：/,// 变 \

  if (FileName = '') then
    Exit;
    
  SetLength(Result, Length(FileName));

  p := PAnsiChar(FileName);
  p2 := PAnsiChar(Result);

  IsSlash := (p^ = '/');
  for i := 1 to Length(FileName) do
    if (p^ <> '/') then
    begin
      p2^ := p^;
      Inc(p); Inc(p2);
      IsSlash := False;
    end else
    begin
      if (i = 1) or (IsSlash = False) then
      begin
        p2^ := p^;
        Inc(p); Inc(p2);
      end else
        Inc(p);
      IsSlash := True;
    end;

  Delete(Result, p2 - PAnsiChar(Result) + 1, 9999);

end;

function ChangeSlash(const FileName: AnsiString): AnsiString;
var
  i: Integer;
  p, p2: PAnsiChar;
begin
  // 路径 \ 变 /
  SetLength(Result, Length(FileName));
  p := PAnsiChar(FileName);
  p2 := PAnsiChar(Result);
  for i := 1 to Length(FileName) do
  begin
    if (p^ = '\') then
      p2^ := '/'
    else
      p2^ := p^;
    Inc(p);
    Inc(p2);
  end;
end;

function CreateBoundary: AnsiString;
begin
  // 生成分隔行(保留)
  Result := '-----iniocp-boundary-' + IntToHex(GetTickCount, 2);
end;

function CompareBuffer(Buf: PAnsiChar; const S: AnsiString;
         IgnoreCase: Boolean = False): Boolean;
var
  i: Integer;
  p: PAnsiChar;
begin
  // 比较 Buf、S 的内容是否相同，IgnoreCase = True 时，忽略大小写。
  Result := True;
  p := PAnsiChar(S);
  for i := 1 to Length(S) do
    if (IgnoreCase = False) and (Buf^ = p^) or
       IgnoreCase and ((Buf^ = p^) or (AnsiChar(Ord(Buf^) - 32) = p^)) then
    begin
      Inc(Buf);
      Inc(p);
    end else
    begin
      Result := False;
      Break;
    end;
end;

function _DecodeHexChar(var Buf: PAnsiChar): AnsiChar; {$IFDEF USE_INLINE} inline; {$ENDIF}
var
  Value: SmallInt;
begin
  // 解码十六进制字符（前一字符应该为 %）
  // %D2%BB%B8%F6%CE%C4%B1%BE%A1%A3
  Inc(Buf);
  if (Ord(Buf^) >= 97) then          // 小写 A..Z = 97..
    Buf^ := AnsiChar(Ord(Buf^) - 32);

  if (Buf^ in ['0'..'9']) then       // Asc(0) = 48
    Value := (Ord(Buf^) - 48) shl 4
  else
    Value := (Ord(Buf^) - 55) shl 4; // Asc(A) = 65, 对应 10

  Inc(Buf);
  if (Buf^ in ['0'..'9']) then
    Inc(Value, Ord(Buf^) - 48)
  else
    Inc(Value, Ord(Buf^) - 55);

  Result := AnsiChar(Value);
end;

function DecodeHexText(const Text: AnsiString): AnsiString;
var
  p, p2: PAnsiChar;
begin
  // 解码十六进制字符串

  // %D2%BB%B8%F6%CE%C4%B1%BE%A1%A3++a

  SetLength(Result, Length(Text));

  p := PAnsiChar(Text);
  p2 := PAnsiChar(Result);

  try
    while (p^ <> #0) do
    begin
      case p^ of
        #37:  // 百分号 MODULUS %
          p2^ := _DecodeHexChar(p);
        #43:  // 加号 +
          p2^ := #32;
        else  // 其他
          p2^ := p^;
      end;
      Inc(p);
      Inc(p2);
    end;

    SetLength(Result, p2 - PAnsiChar(Result));
  except
    Result := Text;
  end;

end;

function ExtractFieldInf(var Buf: PAnsiChar; Len: Integer;
         var FieldValue: AnsiString): TFormElementType;
var
  i: Integer;
  b, eq: PAnsiChar;
begin
  // 提取元素引号内的内容：名称或文件名
  
  // ---------------------Boundary
  // Content-Disposition: form-data; name="textline2"; filename="测试.txt"
  // <Empty Line>
  // Value Text

  b := Nil;                 // 名称开始
  eq := Nil;                // 等号位置

  FieldValue := '';
  Result := fdtUnknown;

  for i := 1 to Len do
  begin
    case Buf^ of
      'a'..'z',
      'A'..'Z', '_':        // 元素名称、类型描述
        if (b = nil) then   // = 右侧
          b := Buf;

      '=': begin            // 类型结束
       //  FieldValue := GetStringValue(b, Integer(Buf - b));
        SetString(FieldValue, b, Integer(Buf - b));
        b := Nil;
        eq := Buf;

        // 判断是否为文件类型
        FieldValue := UpperCase(FieldValue);
        if (FieldValue = 'NAME') then
          Result := fdtName
        else
        if (FieldValue = 'FILENAME') then
          Result := fdtFileName;
      end;

      '''', '"',
       ';', #13:             // 名称 或 文件名称结束
        if (eq <> Nil) then  // = 右侧内容
          if (b = nil) then
            b := PAnsiChar(Buf + 1)
          else begin
          //  FieldValue := GetStringValue(b, Integer(Buf - b));
            SetString(FieldValue, b, Integer(Buf - b));
            if (i < Len) then
              Inc(Buf);
            Break;
          end;
    end;

    Inc(Buf);
  end;

end;

function GetFieldType(const ParamVal: AnsiString): THttpFieldType;
var
  i: Integer;
  DotCount: Integer;
begin
  // 检查字符串是否为数字串
  Result := hftString;
  if (ParamVal = '') or (Length(ParamVal) >= 10) then
    Exit;
  DotCount := 0;
  for i := 1 to Length(ParamVal) do
    case ParamVal[i] of
      '-', '+':
        if (i > 1) then   // 不是负号、正号
          Exit;
      '0'..'9':
        { Empty } ;
      '.': begin
        Inc(DotCount);
        if (DotCount > 1) then
          Exit;
      end;
      else
        Exit;
    end;
  if (DotCount = 1) then
    Result := hftFloat
  else
    Result := hftInteger;
end;

function GetContentType(const FileName: AnsiString): AnsiString;
var
  i: Integer;
  Extension: AnsiString;
begin
  // 取文件类型: Content-type
  Extension := LowerCase(ExtractFileExt(FileName));
  for i := 1 to High(CONTENT_TYPES) do
    if (Extension = CONTENT_TYPES[i].Extension) then
    begin
      Result := CONTENT_TYPES[i].ContentType;
      Exit;
    end;
  Result := CONTENT_TYPES[0].ContentType;
end;

function GetHexSize(Size: Integer): AnsiString;
begin
  // 把长度转为十六进制字符串
  Result := IntToHex(Size, 2);
end;

function _FileTimeToHttpGMTDateTime(const FileTime: TFileTime): AnsiString;
type
  TDblChar = array[0..1] of AnsiChar;
  TThrChar = array[0..2] of AnsiChar;

  PDateTimeFormat = ^TDateTimeFormat;
  TDateTimeFormat = packed record
    DayOfWeek: TThrChar; S: TDblChar;
    Day: TDblChar; S2: AnsiChar;
    Month: TThrChar; S3: AnsiChar;
    Year: TDblChar; Year2: TDblChar; S4: AnsiChar;
    Hour: TDblChar; S5: AnsiChar;
    Minute: TDblChar; S6: AnsiChar;
    Second: TDblChar; S7: AnsiChar;
  end;
  
const
  WEEK_DAYS: array[0..6] of TThrChar = (
    'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');

  MONTH_NAMES: array[1..12] of TThrChar = (
     'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',  'Jul',
     'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

  NUMBERS: array[0..99] of TDblChar = (
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
  p: PDateTimeFormat;
  i: Integer;  
begin
  // 取 GMT 时间
  //   Wed, 09 Jun 2021 10:18:14 GMT

  FileTimeToSystemTime(FileTime, SysTime);  // UTC

  Result := 'Wed, 09 Jun 2021 10:18:19 GMT';
  p := PDateTimeFormat(@Result[1]);

  // 星期
  p^.DayOfWeek := WEEK_DAYS[SysTime.wDayOfWeek];

  // 日、月
  p^.Day := NUMBERS[SysTime.wDay];
  p^.Month := MONTH_NAMES[SysTime.wMonth];

  // 年
  i := SysTime.wYear div 100;
  p^.Year := NUMBERS[i];
  p^.year2 := NUMBERS[SysTime.wYear - i * 100];

  // 时、分、秒
  p^.Hour := NUMBERS[SysTime.wHour];
  p^.Minute := NUMBERS[SysTime.wMinute];
  p^.Second := NUMBERS[SysTime.wSecond];

end;

function GetHttpGMTDateTime: AnsiString;
var
  FileTime: TFileTime;
begin
  GetSystemTimeAsFileTime(FileTime);
  Result := _FileTimeToHttpGMTDateTime(FileTime);
end;

function GetHttpGMTDateTime(const FileTime: TFileTime): AnsiString;
begin
  Result := _FileTimeToHttpGMTDateTime(FileTime);
end;

function GetLocalDateTime(const HttpGMTDateTime: AnsiString): TDateTime;
const
  MONTH_NAMES: array[1..12] of AnsiString = (
          'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',  'JUL',
          'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
  function GetMonthIndex(const S: AnsiString): Integer;
  var
    i: Integer;
  begin
    for i := 1 to High(MONTH_NAMES) do
      if (MONTH_NAMES[i] = S) then
      begin
        Result := i;
        Exit;
      end;
    Result := 0;
  end;
var
  i: Integer;
  S: AnsiString;
  wYear, wDay, wMonth: Word;
  wHour, wMin, wSec: Word;
begin
  // Http GMT 时间转本地时间
  //    Wed, 27 Nov 2017 10:26:16 GMT
  if (Length(HttpGMTDateTime) < 22) then  // 可以不含“,”之前的内容和 “GMT”
    Exit;

  i := Pos(',', HttpGMTDateTime);

  if (i > 0) then
    try
      // 日
      if (HttpGMTDateTime[i + 1] = #32) then
        Inc(i, 2)
      else
        Inc(i);

      S := Copy(HttpGMTDateTime, i, 2);
      wDay := StrToInt(S);

      // 月
      Inc(i, 3);
      S := Copy(HttpGMTDateTime, i, 3);
      wMonth := GetMonthIndex(UpperCase(S));

      if (wMonth = 0) then
        Exit;

      // 年
      Inc(i, 4);
      S := Copy(HttpGMTDateTime, i, 4);
      wYear := StrToInt(S);

      // 时:分:秒
      Inc(i, 5);
      S := Copy(HttpGMTDateTime, i, 2);
      wHour := StrToInt(S);

      S := Copy(HttpGMTDateTime, i + 3, 2);
      wMin := StrToInt(S);

      S := Copy(HttpGMTDateTime, i + 6, 2);
      wSec := StrToInt(S);

      // 尝试转换
      if TryEncodeDateTime(wYear, wMonth, wDay,
                           wHour, wMin, wSec, 0, Result) then
        Result := Result + 0.33333333   // + 8 小时
      else
        Result := -1;

    except
      //
    end;
end;

function FindInBuffer(Buf: PAnsiChar; Len: Integer; const Text: AnsiString): Boolean;
begin
  Result := SearchInBuffer(Buf, Len, Text);
end;

function FindInBuffer2(Buf: PAnsiChar; Len: Integer; const Text: AnsiString): Boolean;
begin
  Result := SearchInBuffer2(Buf, Len, Text);
end;

function SearchInBuffer(var Buf: PAnsiChar; Len: Integer; const Text: AnsiString): Boolean;
var
  i, n, k: Integer;
  s, s2: PAnsiChar;
begin
  // 定位 Text（区分大小写）在 Buf 的位置后一字节，Buf 同时移动
  //   AB1231ACD1234AEnaMe, 1234, NAME -> 10, 16

  s := PAnsiChar(Text);  // Text 开始位置
  s2 := s;               // Text 当前位置

  k := Length(Text);     // Text 长度
  n := 0;                // 匹配次数

  Result := False;
  for i := 1 to Len do
  begin
    if (Buf^ = s2^) then
    begin
      Inc(n);            // 匹配数+
      if (n = k) then    // 全部匹配成功
      begin
        Inc(Buf);        // 到下一字节
        Result := True;  // 位置 = LongWord(buf - b) - k + 1;
        Exit;
      end else
        Inc(s2);         // 下一字节
    end else
    if (n > 0) then      // 匹配过
    begin
      n := 0;            // 重置 0
      s2 := s;           // 定位到开始位置
    end;

    Inc(Buf);
  end;

end;

function SearchInBuffer2(var Buf: PAnsiChar; Len: Integer; const Text: AnsiString): Boolean;
var
  i, n, k: Integer;
  s, s2: PAnsiChar;
begin
  // 定位 Text（不分大小写）在 Buf 的位置后一字节，Buf 同时移动
  //   AB1231ACD1234AEnaMe, 1234, NAME -> 10, 16

  s := PAnsiChar(Text);  // Text 开始位置
  s2 := s;               // Text 当前位置

  k := Length(Text);     // Text 长度
  n := 0;                // 匹配次数

  Result := False;
  for i := 1 to Len do
  begin
    if (Buf^ = s2^) or   // = 或是小写
       (Ord(Buf^) in [97..122]) and (AnsiChar(Ord(Buf^) - 32) = s2^) then
    begin
      Inc(n);            // 匹配数+
      if (n = k) then    // 全部匹配成功
      begin
        Inc(Buf);        // 到下一字节
        Result := True;  // 位置 = LongWord(buf - b) - k + 1;
        Exit;
      end else
        Inc(s2);         // 下一字节
    end else
    if (n > 0) then      // 匹配过
    begin
      n := 0;            // 重置 0
      s2 := s;           // 定位到开始位置
    end;

    Inc(Buf);
  end;

end;

function URLDecode(const URL: AnsiString): AnsiString;
var
  p, p2: PAnsiChar;
begin
  // 解码 Hex 类型的 URL
  //   中文 -> UTF8 -> Hex
  // %E4%B8%AD%E5%9B%BDa%E4%B8%AD%E5%9B%BD.txt

  SetLength(Result, Length(URL));

  p := PAnsiChar(URL);
  p2 := PAnsiChar(Result);

  try
    while (p^ <> #0) do
    begin
      case p^ of
        #37:   // 百分号 MODULUS %
          p2^ := _DecodeHexChar(p);
        #43:  // 加号 +
          p2^ := #32;
        else  // 其他
          p2^ := p^;
      end;
      Inc(p);
      Inc(p2);
    end;

    SetLength(Result, p2 - PAnsiChar(Result));

    Result := System.Utf8ToAnsi(Result);
  except
    Result := URL;
  end;

end;

function URLEncode(const URL: AnsiString): AnsiString;
var
  S: AnsiString;
  p, p2: PAnsiChar;
begin
  // 转换中文 URL 字符串为 Hex 类型
  //   源码见 httpApp.HTTPEncode
  SetLength(Result, Length(S) * 3);

  p := PAnsiChar(S);
  p2 := PAnsiChar(Result);

  while (p^ <> #0) do
  begin
    case p^ of
      'A'..'Z', 'a'..'z', '0'..'9', // 不转换，RFC 1738
      '*', '@', '.', '_', '-',
      '$', '!', '''', '(', ')':
        p2^ := p^;
      #32:  // 空格
        p2^ := '+';
      else
        begin
          FormatBuf(p2^, 3, '%%%.2x', 6, [Ord(p^)]);
          Inc(p2, 2);
        end;
    end;
    Inc(p);
    Inc(p2);
  end;

  SetLength(Result, p2 - PAnsiChar(Result));

end;

end.
