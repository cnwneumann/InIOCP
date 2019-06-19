(*
 * 各种列表单元
 *)
unit iocp_lists;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, {$ELSE}
  Windows, Classes, SysUtils, {$ENDIF}
  iocp_base;

type

  // ===================== 自定义列表 类 =====================

  // 只能删除首节点的列表：
  //    每次加入时，只分配一个节点空间，只能删除首节点

  //    此列表能充分利用碎片内存块，不足是频繁分配、释放，
  //    重复利用释放的节点空间可以避免不足。

  // Delphi 2007 和 XE 10 的 PPointerList 定义不同

  PItemArray = ^TItemArray;
  TItemArray = array[0..MaxInt div 16 - 1] of Pointer;

  TInList = class(TObject)
  private
    FHead: PItemArray;      // 头节点
    FTail: PItemArray;      // 尾节点
    FCount: Integer;        // 对象数
    function GetItems(Index: Integer): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Add(Item: Pointer); {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure AddList(List: TInList); {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure Clear; virtual;
    function Exists(Item: Pointer): Boolean;
    function PopFirst: Pointer; {$IFDEF USE_INLINE} inline; {$ENDIF}
  public
    property Count: Integer read FCount;
    property Items[Index: Integer]: Pointer read GetItems;
  end;

  // TInDataList 比 TList 效率高

  TInDataList = class(TObject)
  private
    FItems: PItemArray;     // 对象表
    FLength: Integer;       // FItems 长度
    FCount: Integer;        // 对象总数
    function GetItems(Index: Integer): Pointer;
    procedure ClearItems; {$IFDEF USE_INLINE} inline; {$ENDIF} // 快速清除
    procedure SetItems(Index: Integer; const Value: Pointer); virtual;
    procedure SetCount(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer); {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    function PopFirst: Pointer; {$IFDEF USE_INLINE} inline; {$ENDIF} // 未用
  public
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read GetItems write SetItems;
  end;

  // TInStringList 字符串列表，比 TStringList 效率高
  // 不要加入空字符串！
  
  PStringItem = ^TStringItem;
  TStringItem = record
    FString: AnsiString;  // 用单字节
    FObject: TObject;
  end;

  // JSON 分块位置
  TJSONChunkPosition = (
    cpAll,            // 全部
    cpFirst,          // 第一块
    cpMiddle,         // 中间块
    cpLast            // 最后块
  );

  TInStringList = class(TInDataList)
  private
    FSize: Integer;
    function UnionStrings(Buf: PAnsiChar; InterChr: AnsiChar): PAnsiChar;
    function GetDelimitedText: AnsiString;
    function GetHttpString(Headers: Boolean): AnsiString;
    function GetJSON: AnsiString;
    function GetJSONChunk(Position: TJSONChunkPosition): AnsiString;
    function GetObjects(Index: Integer): Pointer;
    function GetStrings(Index: Integer): AnsiString;
    function GetText: AnsiString;
    procedure SetItems(Index: Integer; const Value: Pointer); override;
  public
    function IndexOf(const Key: AnsiString): Integer; overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function IndexOf(const Key: AnsiString; var Item: Pointer): Boolean; overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function IndexOf(Item: Pointer): Integer; overload; {$IFDEF USE_INLINE} inline; {$ENDIF}

    procedure Add(const Key: AnsiString; Item: Pointer); overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure Add(const S: AnsiString); overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure AddStrings(Strings: TInStringList); {$IFDEF USE_INLINE} inline; {$ENDIF}

    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure DeleteItem(Item: Pointer);
    procedure SaveToFile(const FileName: String);
    procedure WriteLog(Handle: THandle);    
  public
    property DelimitedText: AnsiString read GetDelimitedText;
    property HttpString[Headers: Boolean]: AnsiString read GetHttpString;
    property JSON: AnsiString read GetJSON;
    property JSONChunk[Position: TJSONChunkPosition]: AnsiString read GetJSONChunk;
    property Objects[Index: Integer]: Pointer read GetObjects;
    property Size: Integer read FSize;
    property Strings[Index: Integer]: AnsiString read GetStrings;
    property Text: AnsiString read GetText;
  end;

  // ==================== JSON 相关 ====================

  // 字符串数组
  TStringAry = array of AnsiString;
  PStringAry = ^TStringAry;

  // List 的回调事件（Source：TTaskList）
  TCallbackEvent = procedure(List: TInStringList; Position: TJSONChunkPosition) of object;

implementation

uses
  iocp_msgPacks, http_base;

const
  ALLOCATE_COUNT = 100;   // TInDataList 每次申请空间数

{ TInList }

procedure TInList.Add(Item: Pointer);
var
  NewBlock: PItemArray;
begin
  GetMem(NewBlock, 2 * POINTER_SIZE);

  if (FCount = 0) then
  begin
    FHead := NewBlock;
    FTail := NewBlock;
  end else
  begin
    FTail^[1] := NewBlock;  // 连起来
    FTail := NewBlock;
  end;

  FTail^[0] := Item;        // Item 占用
  FTail^[1] := Nil;

  Inc(FCount);
end;

procedure TInList.AddList(List: TInList);
begin
  // 转移节点
  FHead := List.FHead;
  FTail := List.FTail;
  FCount := List.FCount;
  List.FHead := nil;
  List.FTail := nil;
  List.FCount := 0;
end;

procedure TInList.Clear;
var
  NextBlock: PItemArray;
begin
  if Assigned(FHead) then
  begin
    repeat
      NextBlock := FHead^[1];
      FreeMem(FHead);
      FHead := NextBlock;
    until FHead = Nil;
    FTail := nil;
    FCount := 0;
  end;
end;

constructor TInList.Create;
begin
  inherited;
  FHead := Nil;
  FCount := 0;
end;

destructor TInList.Destroy;
begin
  if Assigned(FHead) then
    Clear;
  inherited;
end;

function TInList.PopFirst: Pointer;
var
  Block: PItemArray;
begin
  // 弹出首节点
  if (FCount > 0) then
  begin
    Result := FHead^[0];
    Block := FHead^[1];
    FreeMem(FHead);      // 同时删除存储空间
    FHead := Block;
    Dec(FCount);
    if (FCount = 0) then
      FHead := Nil;
  end else
    Result := Nil;
end;

function TInList.GetItems(Index: Integer): Pointer;
var
  i: Integer;
  Block: PItemArray;
begin
  if (Index < 0) or (Index >= FCount) then
    Result := nil
  else begin
    i := 0;
    Block := FHead;
    Result := Block[0];
    while Assigned(Block) and (i < Index) do
    begin
      Block := PItemArray(Block)^[1];
      Result := Block[0];
      Inc(i);
    end;
  end;
end;

function TInList.Exists(Item: Pointer): Boolean;
var
  Block: PItemArray;
begin
  Result := False;
  if (FCount > 0) then
  begin
    Block := FHead;
    while Assigned(Block) do
    begin
      if (Block^[0] = Item) then
      begin
        Result := True;
        Break;
      end;
      Block := Block^[1];
    end;
  end;
end;

{ TInDataList }

procedure TInDataList.Add(Item: Pointer);
begin
  if (FCount * POINTER_SIZE >= FLength) then
  begin
    Inc(FLength, POINTER_SIZE * ALLOCATE_COUNT);  // 预设 n 个
    ReallocMem(FItems, FLength);
  end;
  FItems^[FCount] := Item;             // 占用
  Inc(FCount);              
end;

procedure TInDataList.Clear;
begin
  ClearItems;
end;

procedure TInDataList.ClearItems;
begin
  FreeMem(FItems);
  FItems := nil;
  FCount := 0;
  FLength := 0;
end;

constructor TInDataList.Create;
begin
  inherited;
  FCount := 0;
  FLength := 0;
  FItems := nil;
end;

procedure TInDataList.SetCount(Value: Integer);
begin
  if (Value = 0) then
    Clear
  else begin
    FCount := Value;
    FLength := FCount * POINTER_SIZE;
    ReallocMem(FItems, FLength);
  end;
end;

procedure TInDataList.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    Dec(FCount);    // 在前
    if (FCount = 0) then
      ClearItems             
    else begin
      System.Move(FItems^[Index + 1], FItems^[Index],
                 (FCount - Index) * POINTER_SIZE);
      if (FLength > POINTER_SIZE * (FCount + ALLOCATE_COUNT)) then   // 空余空间太长
      begin
        FLength := POINTER_SIZE * (FCount + ALLOCATE_COUNT);
        ReallocMem(FItems, FLength);
      end;
    end;
  end;
end;

destructor TInDataList.Destroy;
begin
  if (FLength > 0) then
    Clear;
  inherited;
end;

function TInDataList.GetItems(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FItems^[Index]
  else
    Result := Nil;
end;

procedure TInDataList.SetItems(Index: Integer; const Value: Pointer);
begin
  if (Index >= 0) and (Index < FCount) then
    FItems^[Index] := Value;
end;

function TInDataList.PopFirst: Pointer;
begin
  // 取第一个并删除
  Result := GetItems(0);
  if (Result <> nil) then
    Delete(0);
end;

{ TInStringList }

procedure TInStringList.Add(const Key: AnsiString; Item: Pointer);
var
  pItem: PStringItem;
begin
  pItem := New(PStringItem);
  pItem^.FString := Key;  // 单字节
  pItem^.FObject := Item;
  Inc(FSize, Length(pItem^.FString));
  inherited Add(pItem);
end;

procedure TInStringList.Add(const S: AnsiString);
var
  pItem: PStringItem;
begin
  pItem := New(PStringItem);
  pItem^.FString := S;   // 单字节
  pItem^.FObject := nil;
  Inc(FSize, Length(pItem^.FString));
  inherited Add(pItem);
end;

procedure TInStringList.AddStrings(Strings: TInStringList);
var
  i: Integer;
  pItem: PStringItem;
begin
  // 快速实现：
  //   把 Strings 的 TStringItem 节点移到当前列表，
  //   再把 Strings 的数组空间清除。
  for i := 0 to Strings.FCount - 1 do
  begin
    pItem := Strings.GetItems(i);
    Inc(FSize, Length(pItem^.FString));
    inherited Add(pItem);
  end;
  TInDataList(Strings).ClearItems;
  Strings.FCount := 0;
end;

procedure TInStringList.Clear;
var
  i: Integer;
  pItem: PStringItem;
begin
  FSize := 0;
  for i := 0 to FCount - 1 do
  begin
    pItem := GetItems(i);
    if Assigned(pItem) then  // 可能被设为 nil
      Dispose(pItem);
  end;
  inherited;
end;

procedure TInStringList.DeleteItem(Item: Pointer);
var
  i: Integer;
  pItem: PStringItem;
begin
  for i := 0 to FCount - 1 do
  begin
    pItem := GetItems(i);
    if (pItem^.FObject = Item) then
    begin
      Dec(FSize, Length(pItem^.FString));
      Dispose(pItem);
      inherited Delete(i);
      Break;
    end;
  end;
end;

procedure TInStringList.Delete(Index: Integer);
var
  pItem: PStringItem;
begin
  pItem := GetItems(Index);
  if Assigned(pItem) then
  begin
    Dec(FSize, Length(pItem^.FString));
    Dispose(pItem);
    inherited Delete(Index);
  end;
end;

function TInStringList.GetHttpString(Headers: Boolean): AnsiString;
var
  i, m: Integer;
  p: PAnsiChar;        // 用指针更快
  pItem: PStringItem;
begin
  // 合并列表的字符串
  //    Headers: True, 加入回车换行符

  // 如果是双字节，则 Move 复制不完整

  if Headers then     // 每行后要加回车换行
    SetLength(Result, FSize + FCount * 2 + 2)
  else
    SetLength(Result, FSize);

  p := PAnsiChar(Result);
  
  for i := 0 to FCount - 1 do
  begin
    pItem := GetItems(i);

    m := Length(pItem^.FString);
    System.Move(pItem^.FString[1], p^, m);

    Inc(p, m);
    if Headers then  // 加回车、换行
    begin
      p^ := AnsiChar(CHAR_CR);
      (p + 1)^ := AnsiChar(CHAR_LF);
      Inc(p, 2);
    end;
  end;

  if Headers then
  begin
    p^ := AnsiChar(CHAR_CR);
    (p + 1)^ := AnsiChar(CHAR_LF);
  end;
end;

function TInStringList.GetJSON: AnsiString;
begin
  // 把列表的字符串转为 JSON
  if (FSize > 0) then
    Result := GetJSONChunk(cpAll)
  else
    Result := '';
end;

function TInStringList.GetJSONChunk(Position: TJSONChunkPosition): AnsiString;
var
  p: PAnsiChar;
begin
  // 转换列表到 JSON 分块

  if (FSize = 0) then
  begin
    Result := '';
    Exit;
  end;

  if (Position in [cpMiddle, cpLast]) then
  begin
    // 包含末尾的 , 或 ] 
    SetLength(Result, FSize + FCount);
    p := PAnsiChar(Result);
  end else
  begin
    // 包含开始的 [ 和末尾的 ] 或 ,
    SetLength(Result, FSize + FCount + 1);
    p := PAnsiChar(Result);
    p^ := '[';
    Inc(p);
  end;

  // 合并内容，分隔符为 ,
  p := UnionStrings(p, ',');

  if (Position in [cpAll, cpLast]) then
    p^ := AnsiChar(']')       // 末数据块，加结束符
  else
    p^ := AnsiChar(',');      // 最前、中间数据块，加分隔符

end;

function TInStringList.GetDelimitedText: AnsiString;
begin
  if (FSize > 0) then
  begin
    SetLength(Result, FSize + FCount - 1);  // 预设空间
    UnionStrings(@Result[1], ',');          // 用 , 分隔
  end;
end;

function TInStringList.GetObjects(Index: Integer): Pointer;
var
  pItem: PStringItem;
begin
  pItem := GetItems(Index);
  if Assigned(pItem) then
    Result := pItem^.FObject
  else
    Result := nil;
end;

function TInStringList.GetStrings(Index: Integer): AnsiString;
var
  pItem: PStringItem;
begin
  pItem := GetItems(Index);
  if Assigned(pItem) then
    Result := pItem^.FString;
end;

function TInStringList.GetText: AnsiString;
begin
  if (FSize > 0) then
  begin
    SetLength(Result, FSize + FCount - 1);  // 预设空间
    UnionStrings(@Result[1], #32);          // 用空格分隔（SQL命令）
  end;
end;

function TInStringList.IndexOf(const Key: AnsiString; var Item: Pointer): Boolean;
var
  i: Integer;
  pItem: PStringItem;
begin
  for i := 0 to FCount - 1 do
  begin
    pItem := GetItems(i);
    if (pItem^.FString = Key) then
    begin
      Item := pItem^.FObject;
      Result := True;
      Exit;
    end;
  end;
  Item := Nil;
  Result := False;
end;

function TInStringList.IndexOf(const Key: AnsiString): Integer;
var
  i: Integer;
  pItem: PStringItem;
begin
  for i := 0 to FCount - 1 do
  begin
    pItem := GetItems(i);
    if (pItem^.FString = Key) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TInStringList.IndexOf(Item: Pointer): Integer;
var
  i: Integer;
  pItem: PStringItem;
begin
  for i := 0 to FCount - 1 do
  begin
    pItem := GetItems(i);
    if (pItem^.FObject = Item) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TInStringList.SaveToFile(const FileName: String);
var
  i: Integer;
  pItem: PStringItem;
  Stream: TStream;
begin
  // 路径不存在时异常
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    for i := 0 to FCount - 1 do
    begin
      pItem := GetItems(i);
      Stream.Write(pItem^.FString[1], Length(pItem^.FString));
      Stream.Write(AnsiString(STR_CRLF)[1], 2);  // 行尾加回车换行
    end;
  finally
    Stream.Free;
  end;
end;

procedure TInStringList.SetItems(Index: Integer; const Value: Pointer);
var
  pItem: PStringItem;
begin
  // 可以设为 Nil，但不改变存储空间
  pItem := GetItems(Index);
  if Assigned(pItem) then
  begin
    Dec(FSize, Length(pItem^.FString));
    Dispose(pItem);
    inherited;
  end;
end;

function TInStringList.UnionStrings(Buf: PAnsiChar; InterChr: AnsiChar): PAnsiChar;
var
  i, k: Integer;
  pItem: PStringItem;
begin
  // 合并字符串（已预设空间）
  // 起始地址 Buf，用 InterChr 分隔
  for i := 0 to FCount - 1 do
  begin
    pItem := GetItems(i);
    k := Length(pItem^.FString);

    // 如果是双字节，则 Move 复制不完整
    System.Move(pItem^.FString[1], Buf^, k);
    Inc(Buf, k);

    if (i < FCount - 1) then
    begin
      Buf^ := InterChr;     // 加分隔符
      Inc(Buf);
    end;
  end;
  Result := Buf;
end;

procedure TInStringList.WriteLog(Handle: THandle);
var
  i: Integer;
  NoUsed: Cardinal;
  pItem: PStringItem;
begin
  // 把列表写入日志文件（行尾带回车换行）
  //  为减少内存分配，直接写磁盘。
  for i := 0 to FCount - 1 do
  begin
    pItem := GetItems(i);
    WriteFile(Handle, pItem^.FString[1], Length(pItem^.FString), NoUsed, nil);
    WriteFile(Handle, STR_CRLF[1], 2, NoUsed, nil); // 回车换行
  end;
end;

end.
