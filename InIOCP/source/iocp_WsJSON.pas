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
  protected
    FUTF8CharSet: Boolean;     // UTF-8 字符集
  private
    function GetAsRecord(const Index: String): TCustomJSON;
    procedure SetAsRecord(const Index: String; const Value: TCustomJSON);
  protected
    // 写额外字段
    procedure WriteExtraFields(var Buffer: PAnsiChar); virtual;
    // 检查名称的合法性
    procedure CheckFieldName(const Value: AnsiString); override;
    // 检查内容的合法性
    procedure CheckStringValue(const Value: AnsiString); override;
    // 保存变量表到内存流
    procedure SaveToMemStream(Stream: TMemoryStream); override;
    // 扫描内存块，建变量表
    procedure ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal); override;
  public
    property B[const Name: String]: Boolean read GetAsBoolean write SetAsBoolean;
    property D[const Name: String]: TDateTime read GetAsDateTime write SetAsDateTime;
    property F[const Name: String]: Double read GetAsFloat write SetAsFloat;
    property I[const Name: String]: Integer read GetAsInteger write SetAsInteger;
    property I64[const Name: String]: Int64 read GetAsInt64 write SetAsInt64;
    property R[const Name: String]: TCustomJSON read GetAsRecord write SetAsRecord;  // 记录
    property S[const Name: String]: String read GetAsString write SetAsString;
    property V[const Name: String]: Variant read GetAsVariant write SetAsVariant;  // 变长
  end;

  TBaseJSON = class(TCustomJSON)
  protected
    FOwner: TObject;           // 宿主
    FAttachment: TStream;      // 附件流
    FMsgId: Int64;             // 消息 Id
  private
    function GetAction: Integer;
    function GetHasAttachment: Boolean;
    procedure SetAction(const Value: Integer);
    procedure SetAttachment(const Value: TStream);
    procedure SetJSONText(const Value: AnsiString);    
  protected
    // 写额外字段
    procedure WriteExtraFields(var Buffer: PAnsiChar); override;
    // 扫描内存块，建变量表
    procedure ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal); override;    
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
    property Owner: TObject read FOwner;
    property Text: AnsiString write SetJSONText;  // 只写
    property UTF8CharSet: Boolean read FUTF8CharSet;
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

procedure TCustomJSON.CheckStringValue(const Value: AnsiString);
begin
  if Pos('",', Value) > 0 then
    raise Exception.Create('内容不能带保留符号.');
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

procedure TCustomJSON.SaveToMemStream(Stream: TMemoryStream);
const
  BOOL_VALUES: array[Boolean] of string = ('False', 'True');
var
  i: Integer;
  S, JSON: AnsiString;
  p: PAnsiChar;
begin
  // 保存消息到 JSON 文本（不支持数组）

  // 1. JSON 长度 = 每字段多几个字符的描述 +
  //                INIOCP_JSON_HEADER + JSON_CHARSET_UTF8 + MsgOwner
  SetLength(JSON, Integer(FSize) + FList.Count * 25 + 60);
  p := PAnsiChar(JSON);

  // 2. 写额外字段
  WriteExtraFields(p);

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
        etString:
          if FUTF8CharSet then
            VarToJSON(p, Name, System.AnsiToUtf8(AsString), False, False, i = FList.Count - 1)
          else
            VarToJSON(p, Name, AsString, False, False, i = FList.Count - 1);
        etRecord, etStream: begin  // 其他类型未用
          // 可能含保留符，加入长度信息，其他解析器可能无法识别
          // "_Variant":{"Length":1234,"Data":"aaaa... ..."}
          if (VarType = etRecord) then
            S := '"' + Name + '":{"Length":' + IntToStr(Size) + ',"Record":'
          else
            S := '"' + Name + '":{"Length":' + IntToStr(Size) + ',"Data":"';

          System.Move(S[1], p^, Length(S));
          Inc(p, Length(S));

          // 直接用流写入，减少复制次数
          TStream(DataRef).Position := 0;
          TStream(DataRef).Read(p^, Size);
          Inc(p, Size);

          if (VarType = etRecord) then
          begin
            if (i = FList.Count - 1) then
              PDblChars(p)^ := AnsiString('}}')
            else
              PDblChars(p)^ := AnsiString('},');
            Inc(p, 2);
          end else
          begin
            if (i = FList.Count - 1) then
              PThrChars(p)^ := AnsiString('"}}')
            else
              PThrChars(p)^ := AnsiString('"},');
            Inc(p, 3);
          end;
        end;
      end;

  // 4. 删除多余空间
  Delete(JSON, p - PAnsiChar(JSON) + 1, Length(JSON));

  // 5. 写入流
  Stream.Write(JSON[1], Length(JSON));

end;

procedure TCustomJSON.ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal);

  function ExtractData(var p: PAnsiChar; var StreamType: Boolean): TMemoryStream;
  var
    Len: Integer;
    pa: PAnsiChar;
    S: AnsiString;
  begin
    // 读取 Variant 变量数据，格式：
    //   {"Length":5,"Data":"abcde"}
    pa := nil;
    Result := nil;

    Len := 0;
    Inc(p, 8);  // 到 : 前附近

    repeat
      case p^ of
        ':':
          if (Len = 0) then  // 长度位置
            pa := p + 1
          else begin  // 内容位置
            Result := TMemoryStream.Create;
            StreamType := CompareBuffer(p - 6, '"Data"');
            if StreamType then // 是数据流
            begin
              Inc(p, 2);
              Result.Size := Len;
              System.Move(p^, Result.Memory^, Len);
              Inc(p, Len);
            end else
            begin  // 是 JSON 记录
              Inc(p);
              Result.Size := Len;
              System.Move(p^, Result.Memory^, Len);
              Inc(p, Len - 1);
            end;
            // 注：Result.Position := 0;
          end;
        ',': begin  // 取长度
          SetString(S, pa, p - pa);
          Len := StrToInt(S);
        end;
      end;

      Inc(p);
    until (p^ = '}');

  end;

  procedure AddJSONField(const AName: String; AValue: AnsiString; StringType: Boolean);
  begin
    // 增加一个变量/字段
    if StringType then // DateTime 也设为 String
    begin
      if FUTF8CharSet then
        SetAsString(AName, System.UTF8Decode(AValue))
      else
        SetAsString(AName, AValue);
    end else
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
      SetAsInt64(AName, StrToInt64(AValue));
  end;

var
  Level: Integer;   // 括号层次
  DblQuo: Boolean;  // 双引号
  WaitVal: Boolean; // 等待值
  p, pEnd: PAnsiChar;
  pD, pD2: PAnsiChar;

  FldName: String;
  FldValue: AnsiString;
  StreamType: Boolean;  // 是否为数据流
  Stream: TMemoryStream;
  JSONRec: TCustomJSON;
begin
  // 扫描一段内存，分析出 JSON 字段、字段值
  // （全部值转为字符串，不支持数组，否则异常）

  // 先检查字符集：区分大小写
  p := PAnsiChar(ABuffer + Length(INIOCP_JSON_FLAG));
  FUTF8CharSet := SearchInBuffer(p, 50, JSON_CHARSET_UTF8);

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
            Stream := ExtractData(p, StreamType);
            if StreamType then  // 数据流
              SetAsStream(FldName, Stream)  // 不用 String，否则 CheckStringValue
            else begin
              // 记录类型
              JSONRec := TCustomJSON.Create;
              JSONRec.Initialize(Stream);
              SetAsRecord(FldName, JSONRec);
            end;
            Dec(Level);  // 回外层
          end;
        end;

      '"':  // 外层：Level = 1
        if (DblQuo = False) then
          DblQuo := True
        else begin
          DblQuo := False;
          pD2 := p;
        end;

      ':':  // 外层,括号："Name":
        if (DblQuo = False) and (Level = 1) then
        begin
          WaitVal := True;
          SetString(FldName, pD, pD2 - pD);
          FldName := TrimRight(FldName);
          pD := nil;
          pD2 := nil;
        end;

      ',', '}':  // 值结束：xx,
        if WaitVal then  // Length(FldName) > 0
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

  until (p >= pEnd);

  FUTF8CharSet := False;  // 已经转换
  
end;

procedure TCustomJSON.SetAsRecord(const Index: String; const Value: TCustomJSON);
var
  Variable: TListVariable;
begin
  Variable.Data := Value;
  SetField(etRecord, Index, @Variable);
end;

procedure TCustomJSON.WriteExtraFields(var Buffer: PAnsiChar);
begin
  // 加入字符集字段：JSON_CHARSET_UTF8、JSON_CHARSET_DEF
  Buffer^ := AnsiChar('{');
  Inc(Buffer);
  if FUTF8CharSet then
    PInIOCPJSONField(Buffer)^ := JSON_CHARSET_UTF8
  else
    PInIOCPJSONField(Buffer)^ := JSON_CHARSET_DEF;
  Inc(Buffer, Length(JSON_CHARSET_UTF8) - 1);  // 后面的 " 不要
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
  FOwner := AOwner;
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

procedure TBaseJSON.ScanBuffers(ABuffer: PAnsiChar; ASize: Cardinal);
begin
  inherited;
  FOwner := TObject(GetAsInt64('__MSG_OWNER'));  // 消息宿主（大写）
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

procedure TBaseJSON.SetJSONText(const Value: AnsiString);
begin
  // 用 JSON 文本初始化变量表
  if (FList.Count > 0) then
    Clear;
  if (Value <> '') then
    ScanBuffers(PAnsiChar(Value), Length(Value));
end;

procedure TBaseJSON.WriteExtraFields(var Buffer: PAnsiChar);
var
  S: AnsiString;
begin
  // 写入附加信息
  
  // 1. 加入 InIOCP 标志字段：INIOCP_JSON_HEADER
  PInIOCPJSONField(Buffer)^ := INIOCP_JSON_FLAG;
  Inc(Buffer, Length(INIOCP_JSON_FLAG));

  // 2. 加入字符集字段：JSON_CHARSET_UTF8、JSON_CHARSET_DEF
  if FUTF8CharSet then
    PInIOCPJSONField(Buffer)^ := JSON_CHARSET_UTF8
  else
    PInIOCPJSONField(Buffer)^ := JSON_CHARSET_DEF;
  Inc(Buffer, Length(JSON_CHARSET_UTF8));

  // 3. 加入宿主（要读取，大写）
  S := '__MSG_OWNER":' + IntToStr(Int64(FOwner)) + ',';
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
          Sleep(5);
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
          Sleep(5);
        InterSendDataSet(ASender);
      finally
        FDataSet.Active := False;
        FDataSet := nil;
      end;

  // 快速投放时，服务端可能几个消息粘连在一起（Win7 64 位容易出现），
  // 致接收异常，面的消息被放弃
  if (FServerMode = False) then
    Sleep(15);

end;

procedure TSendJSON.InterSendDataSet(ASender: TBaseTaskSender);
  function CharSetText(const S: AnsiString): AnsiString; 
  begin
    if FUTF8CharSet then  // UTF-8 字符集
      Result := System.UTF8Encode(S)
    else
      Result := S;
  end;

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
        Desc := '{"' + CharSetText(LowerCase(Field.FieldName)) + '":"';  // 用小写
      end else
        Desc := '","' + CharSetText(LowerCase(Field.FieldName)) + '":"';
      Names[i] := Desc;
      Inc(n, Length(Desc) + Field.Size);
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
          Desc := Names[i] + CharSetText(Field.Text) + '"}'
        else
          Desc := Names[i] + CharSetText(Field.Text);
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
