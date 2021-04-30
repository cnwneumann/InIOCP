(*
 * iocp WebSocket 服务消息 JSON 封装单元
 *)
unit iocp_WsJSON;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes,
  System.SysUtils, System.Variants, Data.DB, {$ELSE}
  Windows, Classes, SysUtils, Variants, DB, {$ENDIF}
  iocp_base, iocp_senders, iocp_msgPacks;

type

  // 说明：这是简单的 JSON 封装，只支持单记录！

  TCustomJSON = class(TBasePack)
  private
    FText: AnsiString;         // JSON 文本  
    function GetAsRecord(const Index: String): TCustomJSON;
    function GetJSONText: AnsiString;
    procedure SetAsRecord(const Index: String; const Value: TCustomJSON);
    procedure SetJSONText(const Value: AnsiString);
    procedure WriteToBuffers(var JSON: AnsiString; WriteExtra: Boolean);
  protected
    // 检查名称的合法性
    procedure CheckFieldName(const Value: AnsiString); override;
    // 刷新 FText
    procedure InterRefresh; override;  
    // 保存变量表到内存流
    procedure SaveToMemStream(Stream: TMemoryStream; WriteExtra: Boolean); override;
    // 扫描内存块，建变量表
    procedure ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal; ReadExtra: Boolean); override;
    // 写额外字段
    procedure WriteSystemInfo(var Buffer: PAnsiChar); virtual;
  public
    procedure Clear; override;
  public
    property B[const Name: String]: Boolean read GetAsBoolean write SetAsBoolean;
    property D[const Name: String]: TDateTime read GetAsDateTime write SetAsDateTime;
    property F[const Name: String]: Double read GetAsFloat write SetAsFloat;
    property I[const Name: String]: Integer read GetAsInteger write SetAsInteger;
    property I64[const Name: String]: Int64 read GetAsInt64 write SetAsInt64;
    property R[const Name: String]: TCustomJSON read GetAsRecord write SetAsRecord;  // 记录
    property S[const Name: String]: String read GetAsString write SetAsString;
    property V[const Name: String]: Variant read GetAsVariant write SetAsVariant;  // 变长
    property Text: AnsiString read GetJSONText write SetJSONText;  // 改为读写 
  end;

  TBaseJSON = class(TCustomJSON)
  protected
    FAttachment: TStream;      // 附件流
    FMsgId: Int64;             // 消息Id    
  private
    function GetAction: Integer;
    function GetHasAttachment: Boolean;
    procedure SetAction(const Value: Integer);
    procedure SetAttachment(const Value: TStream);
  protected
    // 扫描内存块，建变量表
    procedure ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal; ReadExtra: Boolean); override;
    // 写额外字段
    procedure WriteSystemInfo(var Buffer: PAnsiChar); override;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure Close; virtual;
  public
    // 预设属性
    property Action: Integer read GetAction write SetAction;
    property Attachment: TStream read FAttachment write SetAttachment;
    property HasAttachment: Boolean read GetHasAttachment;
    property MsgId: Int64 read FMsgId;
  end;

  // 发送用的 JSON 消息
  
  TSendJSON = class(TBaseJSON)
  protected
    FDataSet: TDataSet;        // 要发送的数据集（当附件）
    FFrameSize: Int64;         // 帧长度
    FServerMode: Boolean;      // 服务端使用
  private
    procedure InterSendDataSet(ASender: TBaseTaskSender);
    procedure SetDataSet(Value: TDataset);
  protected
    procedure InternalSend(ASender: TBaseTaskSender; AMasking: Boolean);
  protected
    property DataSet: TDataSet read FDataSet write SetDataSet;  // 服务端公开
  public
    procedure Close; override;
  end;

implementation

uses
  iocp_Winsock2, iocp_lists, http_utils, iocp_utils;
  
{ TCustomJSON }

procedure TCustomJSON.CheckFieldName(const Value: AnsiString);
var
  i: Integer;
begin
  inherited;
  for i := 1 to Length(Value) do
    if (Value[i] in ['''', '"', ':', ',', '{', '}']) then
      raise Exception.Create('变量名称不合法.');
end;

procedure TCustomJSON.Clear;
begin
  inherited;
  FText := '';    
end;

function TCustomJSON.GetAsRecord(const Index: String): TCustomJSON;
var
  Stream: TStream;
begin
  // 先转为流，再转为 TCustomJSON
  Stream := GetAsStream(Index);
  if Assigned(Stream) then
    try
      Result := TCustomJSON.Create;
      Result.Initialize(Stream);
    finally
      Stream.Free;
    end
  else
    Result := nil;
end;

function TCustomJSON.GetJSONText: AnsiString;
begin
  if (FList.Count = 0) then
    Result := '[]'
  else begin
    if (FText = '') then
      WriteToBuffers(FText, True);
    Result := FText;
  end;
end;

procedure TCustomJSON.InterRefresh;
begin
  // 内容改变，删除 FText
  if (FText <> '') then
    FText := '';
end;

procedure TCustomJSON.SaveToMemStream(Stream: TMemoryStream; WriteExtra: Boolean);
var
  JSON: AnsiString;
begin
  // 写入流
  WriteToBuffers(JSON, WriteExtra);
  Stream.Write(JSON[1], Length(JSON));
end;

procedure TCustomJSON.ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal; ReadExtra: Boolean);
  procedure ExtractStream(var Stream: TMemoryStream;
                          var p: PAnsiChar; iLen: Integer);
  begin
    // 提取内容到流
    // 注：Stream.Position := 0;
    Stream := TMemoryStream.Create;
    Stream.Size := iLen;
    Inc(p, 2);
    System.Move(p^, Stream.Memory^, iLen);
    Inc(p, iLen);
  end;
  procedure GetFieldData(var p: PAnsiChar; var DataType: TElementType;
                         var Str: AnsiString; var Stream: TMemoryStream;
                         var VarValue: Variant);
  var
    pa: PAnsiChar;
    Len: Integer;    
  begin
    // 读取带长度描述的字段内容，格式：
    //   {"Length":5,"Data":"abcde"}
    pa := nil;
    Len := 0;
    Inc(p, 8);  // 到 : 前位置附近

    repeat
      case p^ of
        ':':
          if (Len = 0) then  // 长度位置
            pa := p + 1
          else  // 内容位置
            if CompareBuffer(p - 9, ',"Stream":"') then  // Stream
            begin
              DataType := etStream;
              ExtractStream(Stream, p, Len);
            end else
            if CompareBuffer(p - 9, ',"Record":"') then  // JSON 记录
            begin
              DataType := etRecord;
              ExtractStream(Stream, p, Len);
            end else
            if CompareBuffer(p - 9, ',"String":"') then  // 字符串
            begin
              DataType := etString;
              Inc(p, 2);
              SetString(Str, p, Len);
              Inc(p, Len);
            end else
            begin  // ',"Variant":"' -- Variant 类型
              DataType := etVariant;
              Inc(p, 2);
              VarValue := BufferToVariant(p, Len, True);  // 自动解压
              Inc(p, Len);
            end;

        ',': begin  // 取长度
          SetString(Str, pa, p - pa);
          Len := StrToInt(Str);
          Inc(p, 8);
        end;
      end;

      Inc(p);
    until (p^ = '}');
  end;
  procedure AddJSONField(const AName: String; AValue: AnsiString; StringType: Boolean);
  begin
    // 增加一个变量/字段
    if StringType then // DateTime 用 String 表示
      SetAsString(AName, AValue)
    else
    if (AValue = 'True') then
      SetAsBoolean(AName, True)
    else
    if (AValue = 'False') then
      SetAsBoolean(AName, False)
    else
    if (AValue = 'Null') or (AValue = '""') or (AValue = '') then
      SetAsString(AName, '')
    else
    if (Pos('.', AValue) > 0) then  // 普通数字
      SetAsFloat(AName, StrToFloat(AValue))
    else
    if (Length(AValue) < 10) then  // 2147 4836 47
      SetAsInteger(AName, StrToInt(AValue))
    else
    if (AValue[1] in ['0'..'9', '-', '+']) then
      SetAsInt64(AName, StrToInt64(AValue));
  end;

var
  Level: Integer;          // 括号层次
  DblQuo: Boolean;         // 双引号
  WaitVal: Boolean;        // 等待字段值

  p, pEnd: PAnsiChar;
  pD, pD2: PAnsiChar;

  DataType: TElementType;  // 数据类型
  FldName: AnsiString;     // 字段名称

  FldValue: AnsiString;    // String 字段值
  VarValue: Variant;       // Variant 字段值
  Stream: TMemoryStream;   // Stream 字段值
  JSONRec: TCustomJSON;    // 记录字段值
begin
  // 扫描一段内存，分析出 JSON 字段、字段值
  // （全部值转为字符串，不支持数组，否则异常）

  // 扫描范围
  p := ABuffer;
  pEnd := PAnsiChar(p + ASize);

  // 数据开始、结束位置
  pD := nil;
  pD2 := nil;

  Level   := 0;      // 层次
  DblQuo  := False;
  WaitVal := False;  // 等待字段值

  // 遍历，提取字段、值

  repeat

(*  {"Id":123,"Name":"我","Boolean":True,"Stream":Null,
     "_Variant":{"Length":5,"Data":"aaaa"},"_zzz":2345}  *)

    case p^ of  // 特殊符号在双引号后当作名称或内容的一部分
      '{':
        if (DblQuo = False) then  // 括号外
        begin
          Inc(Level);
          if (Level > 1) then  // 内层，存放 Variant 数据
          begin
            DblQuo := False;
            WaitVal := False;

            // 分析内层的内容
            GetFieldData(p, DataType, FldValue, Stream, VarValue);

            case DataType of
              etString:
                SetAsString(FldName, FldValue);
              etStream:       // 数据流
                SetAsStream(FldName, Stream);
              etRecord: begin // 记录类型
                JSONRec := TCustomJSON.Create;
                JSONRec.Initialize(Stream);
                SetAsRecord(FldName, JSONRec);
              end;
              etVariant:      // Variant
                SetAsVariant(FldName, VarValue);
            end;

            Dec(Level);  // 回外层
          end;
        end;

      '"':  // 外层：Level = 1
        if (DblQuo = False) then
          DblQuo := True
        else
        if ((p + 1)^ in [':', ',', '}']) then // 引号结束
        begin
          DblQuo := False;
          pD2 := p;
        end;

      ':':  // 外层,括号："Name":”
        if (DblQuo = False) and (Level = 1) then
        begin
          WaitVal := True;
          SetString(FldName, pD, pD2 - pD);
          FldName := TrimRight(FldName);
          pD := nil;
          pD2 := nil;
        end;

      ',', '}':  // 值结束：xx,"  xx","  xx},"
        if (p^ = '}') or (p^ = ',') and ((p + 1)^ = '"') then
          if (DblQuo = False) and WaitVal then  // Length(FldName) > 0
          begin
            if (pD2 = nil) then  // 前面没有引号
            begin
              SetString(FldValue, pD, p - pD);
              AddJSONField(FldName, Trim(FldValue), False);
            end else
            begin
              SetString(FldValue, pD, pD2 - pD);
              AddJSONField(FldName, FldValue, True);  // 不要 Trim(FldValue)
            end;
            pD := nil;
            pD2 := nil;
            WaitVal := False;
          end;

      else
        if (DblQuo or WaitVal) and (pD = nil) then  // 名称、内容开始
          pD := p;
    end;

    Inc(p);

  until (p > pEnd);

end;

procedure TCustomJSON.SetAsRecord(const Index: String; const Value: TCustomJSON);
var
  Variable: TListVariable;
begin
  Variable.Data := Value;
  SetField(etRecord, Index, @Variable);
end;

procedure TCustomJSON.SetJSONText(const Value: AnsiString);
begin
  // 用 JSON 文本初始化变量表
  Clear;
  if (Value <> '') then
    ScanBuffers(PAnsiChar(Value), Length(Value), False);
end;

procedure TCustomJSON.WriteSystemInfo(var Buffer: PAnsiChar);
begin
  // Empty
end;

procedure TCustomJSON.WriteToBuffers(var JSON: AnsiString; WriteExtra: Boolean);
const
  BOOL_VALUES: array[Boolean] of AnsiString = ('False', 'True');
  FIELD_TYPES: array[etString..etVariant] of AnsiString = (
               ',"String":"', ',"Record":"', ',"Stream":"', ',"Variant":"');
  function SetFieldNameLength(var Addr: PAnsiChar; AName: AnsiString;
                              ASize: Integer; AType: TElementType): Integer;
  var
    S: AnsiString;
  begin
    // 写入字段长度描述
    // 可能含保留符，加入长度信息，其他解析器可能无法识别
    // 格式："VarName":{"Length":1234,"String":"???"}
    S := '"' + AName + '":{"Length":' + IntToStr(ASize) + FIELD_TYPES[AType];
    Result := Length(S);
    System.Move(S[1], Addr^, Result);
    Inc(Addr, Result);
  end;
var
  i: Integer;
  p: PAnsiChar;
begin
  // 保存消息到 JSON 文本（不支持数组）

  // 1. JSON 长度 = 每字段多几个字符的描述 +
  //                INIOCP_JSON_HEADER + JSON_CHARSET_UTF8 + MsgOwner
  SetLength(JSON, Integer(FSize) + FList.Count * 25 + 80);
  p := PAnsiChar(JSON);

  // 2. 写标志性字段
  WriteSystemInfo(p);

  // 3. 加入列表字段
  for i := 0 to FList.Count - 1 do
    with Fields[i] do
      case VarType of
        etNull:
          VarToJSON(p, Name, 'Null', True, False, i = FList.Count - 1);
        etBoolean:
          VarToJSON(p, Name, BOOL_VALUES[AsBoolean], True, False, i = FList.Count - 1);
        etCardinal..etInt64:
          VarToJSON(p, Name, AsString, True, False, i = FList.Count - 1);
        etDateTime:  // 当作字符串
          VarToJSON(p, Name, AsString, False, False, i = FList.Count - 1);

        etString, etRecord,
        etStream, etVariant: begin  // 其他类型未用

          // 加入字段名称、长度
          SetFieldNameLength(p, Name, Size, VarType);

          if (Size > 0) then  // 加入内容: "内容"
            case VarType of
              etString: begin
                System.Move(AsString[1], p^, Size);  // 复制内容
                Inc(p, Size);  // 前移
              end;
              etRecord, etStream: begin  // 直接用流写入，减少复制次数
                TStream(DataRef).Position := 0;
                TStream(DataRef).Read(p^, Size);
                Inc(p, Size);  // 前移
              end;
              etVariant: begin  // 复制内容
                System.Move(DataRef^, p^, Size);
                Inc(p, Size);  // 前移
              end;
            end;

          if (i = FList.Count - 1) then
            PThrChars(p)^ := AnsiString('"}}')
          else
            PThrChars(p)^ := AnsiString('"},');

          Inc(p, 3);
        end;
      end;

  // 4. 删除多余空间
  Delete(JSON, p - PAnsiChar(JSON) + 1, Length(JSON));

end;

{ TBaseJSON }

procedure TBaseJSON.Close;
begin
  // 关闭附件流
  if Assigned(FAttachment) then
  begin
    FAttachment.Free;
    FAttachment := nil;
  end;
end;

constructor TBaseJSON.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := UInt64(AOwner);
  FMsgId := GetUTCTickCount;
end;

destructor TBaseJSON.Destroy;
begin
  Close;
  inherited;  // 自动 Clear;
end;

function TBaseJSON.GetAction: Integer;
begin
  Result := GetAsInteger('__action');  // 操作类型
end;

function TBaseJSON.GetHasAttachment: Boolean;
begin
  Result := GetAsBoolean('__has_attach');  // 是否带附件
end;

procedure TBaseJSON.ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal; ReadExtra: Boolean);
begin
  inherited;
  // 修改消息宿主（大写）
  FOwner := GetAsInt64('__MSG_OWNER');  
end;

procedure TBaseJSON.SetAction(const Value: Integer);
begin
  SetAsInteger('__action', Value);  // 操作类型
end;

procedure TBaseJSON.SetAttachment(const Value: TStream);
begin
  // 设置附件流，加一个 __has_attach 变量
  Close;  // 释放现有的流
  FAttachment := Value;
  SetAsBoolean('__has_attach', Assigned(FAttachment) and (FAttachment.Size > 0));
end;

procedure TBaseJSON.WriteSystemInfo(var Buffer: PAnsiChar);
var
  S: AnsiString;
begin
  // 写入系统信息字段：InIOCP 标志、宿主（大写）
  //                   {"_InIOCP_Ver":2.8,"__MSG_OWNER":12345678,
  S := INIOCP_JSON_FLAG + '"__MSG_OWNER":' + IntToStr(UInt64(FOwner)) + ',';
  System.Move(S[1], Buffer^, Length(S));
  Inc(Buffer, Length(S));
end;

{ TSendJSON }

procedure TSendJSON.Close;
begin
  inherited;
  if Assigned(FDataSet) then
    FDataSet := nil;
end;

procedure TSendJSON.InternalSend(ASender: TBaseTaskSender; AMasking: Boolean);
var
  JSON: TMemoryStream;
begin
  // 发送数据

  if (FList.Count = 0) then
    Exit;

  // 1. 字段内容转换到 JSON 流
  JSON := TMemoryStream.Create;

  SaveToStream(JSON, True);  // 自动清除变量表

  FFrameSize := JSON.Size;

  // 2. 发送
  ASender.Masking := AMasking;  // 掩码设置
  ASender.OpCode := ocText;  // JSON 当作文本，不能改

  ASender.Send(JSON, FFrameSize, True);  // 自动释放

  // 3. 发送附件：数据流或数据集
  if Assigned(FAttachment) then  // 3.1 数据流
    try
      FFrameSize := FAttachment.Size;  // 改变
      if (FFrameSize = 0) then
        FAttachment.Free   // 直接释放
      else begin
        if (FServerMode = False) then
          Sleep(10);
        ASender.OpCode := ocBiary;  // 附件 当作二进制，不能改
        ASender.Send(FAttachment, FFrameSize, True);  // 自动释放
      end;
    finally
      FAttachment := nil;  // 已经释放
    end
  else
    if Assigned(FDataSet) then  // 3.2 数据集
      try
        if (FServerMode = False) then
          Sleep(10);
        InterSendDataSet(ASender);
      finally
        FDataSet.Active := False;
        FDataSet := nil;
      end;

  // 快速投放时，服务端可能几个消息粘连在一起（Win7 64 位容易出现），
  // 致接收异常，面的消息被放弃
  if (FServerMode = False) then
    Sleep(10);

end;

procedure TSendJSON.InterSendDataSet(ASender: TBaseTaskSender);
  procedure MarkFrameSize(AData: PWsaBuf; AFrameSize: Integer; ALastFrame: Byte);
  var
    pb: PByte;
  begin
    // 服务端：构造 WebSocket 帧信息
    //   忽略 RSV1/RSV2/RSV3
    pb := PByte(AData^.buf);
    pb^ := ALastFrame + Byte(ocBiary);  // 有后继帧（高位 = 0），附件
    Inc(pb);

    pb^ := 126;  // 用 126，客户端从 3、4字节取长度
    Inc(pb);

    TByteAry(pb)[0] := TByteAry(@AFrameSize)[1];
    TByteAry(pb)[1] := TByteAry(@AFrameSize)[0];

    // 发送内容长度
    AData^.len := AFrameSize + 4;
  end;

var
  XData: PWsaBuf;  // 填充空间

  i, k, n, m, Idx: integer;
  EmptySize, Offset: Integer;
  p: PAnsiChar;

  Desc, JSON: AnsiString;
  Names: TStringAry;
  Field: TField;

begin
  // 快速把数据集转为 JSON（忽略 Blob 字段内容）
  // 注意：不带数据描述，每字段长度不能超过 IO_BUFFER_SIZE

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
        Desc := '{"' + LowerCase(Field.FieldName) + '":"';  // 用小写
      end else
        Desc := '","' + LowerCase(Field.FieldName) + '":"';
      Names[i] := Desc;
      Inc(n, Length(Desc) + Field.Size + 10);
    end;

    // 2. 每条记录转为 JSON，缓存满时发送

    XData := ASender.Data;  // 发送器的填充空间

    // 数组开始，帧描述 4 字节
    (XData.buf + 4)^ := AnsiChar('[');

    EmptySize := IO_BUFFER_SIZE - 5;  // 空间长度
    Offset := 5;  // 写入位移

    while not Dataset.Eof do
    begin
      SetLength(JSON, n);    // 预设记录空间
      p := PAnsiChar(JSON);
      Idx := 0;              // 内容的实际长度

      // 记录 -> JSON
      for i := 0 to k - 1 do
      begin
        Field := Dataset.Fields[i];
        if (i = k - 1) then  // [{"Id":"1","Name":"我"},{"Id":"2","Name":"你"}]
          Desc := Names[i] + Field.Text + '"}'
        else
          Desc := Names[i] + Field.Text;
        m := Length(Desc);
        System.Move(Desc[1], p^, m);
        Inc(p, m);
        Inc(Idx, m);
      end;


      Inc(Idx);  // 记录后紧跟符号 , 或 ]
      Delete(JSON, Idx + 1, n - Idx);   // 删除多余内容

      // 空间不足 -> 先发送已填写的内容
      if (Idx > EmptySize) then
      begin
        MarkFrameSize(XData, Offset - 4, 0);  // 设置帧信息
        ASender.SendBuffers;  // 立刻发送！

        EmptySize := IO_BUFFER_SIZE - 4; // 复原
        Offset := 4;  // 复原
      end;
      
      // 下一条记录
      Dataset.Next;
      
      // 加入 JSON，下次满时发送
      if Dataset.Eof then
        JSON[Idx] := AnsiChar(']')  // 结束符
      else
        JSON[Idx] := AnsiChar(','); // 未结束

      System.Move(JSON[1], (XData.buf + Offset)^, Idx);
      Dec(EmptySize, Idx); // 空间-
      Inc(Offset, Idx);    // 位移+      
      
    end;

    // 发送最后一帧
    if (Offset > 4) then
    begin
      MarkFrameSize(XData, Offset - 4, Byte($80));  // 设置帧信息
      ASender.SendBuffers;  // 立刻发送！
    end;

  finally
    Dataset.EnableControls;
  end;

end;

procedure TSendJSON.SetDataSet(Value: TDataset);
begin
  Close;  // 关闭现有附件
  FDataSet := Value;
  SetAsBoolean('__has_attach', Assigned(Value) and Value.Active and not Value.IsEmpty);
end;

end.
