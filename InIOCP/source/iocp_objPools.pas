(*
 * icop 对象池、哈希表等
 *)
unit iocp_objPools;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7UP}
  Winapi.Windows, System.Classes, System.SysUtils, {$ELSE}
  Windows, Classes, SysUtils, {$ENDIF}
  iocp_api, iocp_base, iocp_lists, iocp_utils,
  iocp_baseObjs;

type

  // ===================== 对象管理池 类 =====================

  // 遍历对象池的回调方法
  TScanListEvent = procedure(ObjType: TObjectType; var FromObject: Pointer;
                             const Data: TObject; var CancelScan: Boolean) of object;

  TObjectPool = class(TObject)
  private
    FLock: TThreadLock;         // 锁
    FObjectType: TObjectType;   // 对象类型

    FBuffer: Array of TLinkRec; // 自动分配的内存块 FBuffer
    FAutoAllocate: Boolean;     // 自动分配 FBuffer

    FFirstNode: PLinkRec;       // 当前 在用 链表顶
    FFreeNode: PLinkRec;        // 当前 空闲 链表顶

    // 统计数字
    FIniCount: Integer;         // 初始大小
    FNodeCount: Integer;        // 全部节点数（对象数）
    FUsedCount: Integer;        // 在用节点数（对象数）

    function AddNode: PLinkRec;
    function GetFull: Boolean;

    procedure CreateObjLink(ASize: Integer);
    procedure DeleteObjLink(var FirstNode: PLinkRec; FreeObject: Boolean);
    procedure DefaultFreeResources;
    procedure FreeNodeObject(ANode: PLinkRec);
  protected
    procedure CreateObjData(const ALinkNode: PLinkRec); virtual;
    procedure FreeListObjects(List: TInDataList);
    procedure OptimizeDetail(List: TInDataList);
  public
    constructor Create(AObjectType: TObjectType; ASize: Integer);
    destructor Destroy; override;
  public
    procedure Clear;
    function Pop: PLinkRec; overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure Push(const ANode: PLinkRec); {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure Lock; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure UnLock; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure Optimize; virtual;
    procedure Scan(var FromObject: Pointer; Callback: TScanListEvent);
  public
    property FirstNode: PLinkRec read FFirstNode;
    property Full: Boolean read GetFull;
    property IniCount: Integer read FIniCount;
    property NodeCount: Integer read FNodeCount;
    property ObjectType: TObjectType read FObjectType;
    property UsedCount: Integer read FUsedCount;
  end;

  // ====================== 带列表的池 类 ======================

  TDataListPool = class(TObjectPool)
  private
    FNodeList: TInDataList;   // 资源节点列表
  protected
    procedure CreateObjData(const ALinkNode: PLinkRec); override;
  public
    constructor Create(AObjectType: TObjectType; ASize: Integer);
    destructor Destroy; override;
  public
    procedure Optimize; override;
  end;

  // ====================== 服务端 Socket 管理 类 ======================

  TIOCPSocketPool = class(TDataListPool)
  private
    FSocketClass: TClass;      // TBaseSocket 类
  protected
    procedure CreateObjData(const ALinkNode: PLinkRec); override;
  public
    constructor Create(AObjectType: TObjectType; ASize: Integer);
  public
    function Clone(Source: TObject): TObject;
    procedure GetSockets(List: TInList; IngoreSocket: TObject = nil; AdminType: Boolean = False);
  end;

  // ===================== 收、发内存管理 类 =====================

  TIODataPool = class(TDataListPool)
  protected
    procedure CreateObjData(const ALinkNode: PLinkRec); override;
  public
    constructor Create(ASize: Integer);
    destructor Destroy; override;
  end;

  // ====================== TStringHash 表 ======================

  // 用 Delphi 2007 的 TStringHash 修改

  PHashItem  = ^THashItem;
  PPHashItem = ^PHashItem;
  
  THashItem = record
    Key: AnsiString;       // 用单字节
    Value: Pointer;        // 改！存放指针
    Next: PHashItem;
  end;

  TScanHashEvent = procedure(var Data: Pointer) of object;
  
  TStringHash = class(TObject)
  private
    FLock: TThreadLock;    // 增加锁
    FCount: Integer;
    FBuckets: array of PHashItem;
    function Find(const Key: AnsiString): PPHashItem;
  protected
    function HashOf(const Key: AnsiString): Cardinal; virtual;
    procedure FreeItemData(Item: PHashItem); virtual;
  public
    constructor Create(Size: Cardinal = 256);
    destructor Destroy; override;

    function Modify(const Key: AnsiString; Value: Pointer): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function ValueOf(const Key: AnsiString): Pointer; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function ValueOf2(const Key: AnsiString): Pointer; {$IFDEF USE_INLINE} inline; {$ENDIF}

    procedure Add(const Key: AnsiString; Value: Pointer); {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure Clear;
    procedure Lock; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure UnLock; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure Remove(const Key: AnsiString); {$IFDEF USE_INLINE} inline; {$ENDIF}

    procedure Scan(var Dest: Pointer; CallEvent: TScanListEvent); overload;
    procedure Scan(CallEvent: TScanHashEvent); overload;
  public
    property Count: Integer read FCount;
  end;

  // ================== 防攻击 管理 ==================

  TPreventAttack = class(TStringHash)
  protected
    procedure FreeItemData(Item: PHashItem); override;
  public
    function CheckAttack(const PeerIP: String; MSecond, InterCount: Integer): Boolean;
    procedure DecRef(const PeerIP: String);
  end;
  
implementation

uses
  iocp_sockets;

type
  TBaseSocketRef = class(TBaseSocket);
  
{ TObjectPool }

function TObjectPool.AddNode: PLinkRec;
begin
  // 增加新节点, 加到空闲链表顶
  Inc(FNodeCount);

  Result := New(PLinkRec);
  Result^.Auto := False;
  
  {$IFDEF DEBUG_MODE}
  Result^.No := FNodeCount;
  {$ENDIF}

  CreateObjData(Result);  // 建关联的对象

  Result^.Prev := Nil;
  Result^.Next := FFreeNode;
  if (FFreeNode <> Nil) then
    FFreeNode^.Prev := Result;

  FFreeNode := Result;
end;

procedure TObjectPool.Clear;
begin
  FLock.Acquire;
  try
    DefaultFreeResources;
  finally
    FLock.Release;
  end;
end;

constructor TObjectPool.Create(AObjectType: TObjectType; ASize: Integer);
begin
  inherited Create;
  FIniCount := ASize;
  FObjectType := AObjectType;
  FLock := TThreadLock.Create;
  CreateObjLink(ASize);
end;

procedure TObjectPool.CreateObjLink(ASize: Integer);
var
  i: Integer;
  PNode: PLinkRec;
begin
  // 建链表
  
  FFreeNode := nil;
  FFirstNode := Nil;
  
  FUsedCount := 0;
  FNodeCount := ASize;
  FAutoAllocate := ASize > 0;     // > 0 自动分配节点空间

  if FAutoAllocate then
  begin
    // 分配节点空间
    SetLength(FBuffer, ASize);

    FFreeNode := @FBuffer[0];
    for i := 0 to ASize - 1 do    // 建双链表
    begin
      PNode := @FBuffer[i];       // 0...

      PNode^.Auto := True;        // 自动分配
      PNode^.InUsed := False;

      {$IFDEF DEBUG_MODE}
      PNode^.No := i + 1;
      {$ENDIF}

      if (i = 0) then             // 首
        PNode^.Prev := Nil
      else
        PNode^.Prev := @FBuffer[i - 1];

      if (i < ASize - 1) then     // 尾
        PNode^.Next := @FBuffer[i + 1]
      else
        PNode^.Next := Nil;

      CreateObjData(PNode);      // 建关联对象
    end;
  end;
end;

procedure TObjectPool.CreateObjData(const ALinkNode: PLinkRec);
begin
  // Empty
end;

procedure TObjectPool.DefaultFreeResources;
begin
  // 默认释放方法：遍历链表释放节点及关联对象
  if Assigned(FFirstNode) then
    DeleteObjLink(FFirstNode, True);

  if Assigned(FFreeNode) then
    DeleteObjLink(FFreeNode, True);

  if FAutoAllocate then
  begin
    SetLength(FBuffer, 0);
    FAutoAllocate := False;
  end;

  FNodeCount := 0;
  FUsedCount := 0;

end;

procedure TObjectPool.DeleteObjLink(var FirstNode: PLinkRec; FreeObject: Boolean);
var
  PToFree, PNode: PLinkRec;
begin
  // 删除 TopNode 的链表空间及关联对象
  FLock.Acquire;
  try
    PNode := FirstNode;
    while (PNode <> Nil) do   // 遍历链表
    begin
      if Assigned(PNode^.Data) then
        FreeNodeObject(PNode);
      PToFree := PNode;
      PNode := PNode^.Next;
      if (PToFree^.Auto = False) then    // 用 AddNode() 新增的节点
        Dispose(PToFree);
    end;
    FirstNode := nil;
  finally
    FLock.Release;
  end;
end;

destructor TObjectPool.Destroy;
begin
  DefaultFreeResources;
  FLock.Free;
  inherited;
end;

procedure TObjectPool.FreeListObjects(List: TInDataList);
var
  i: Integer;
  Node: PLinkRec;
begin
  // 额外的释放方法：遍历列表释放节点及关联对象
  for i := 0 to List.Count - 1 do
  begin
    Node := List.Items[i];
    if Assigned(Node) then
    begin
      if Assigned(Node^.Data) then
        FreeNodeObject(Node);
      if (Node^.Auto = False) then   // 用 AddNode() 新增的节点
        Dispose(Node);
    end;
  end;

  FFirstNode := nil;  // 防止 DefaultFreeResources 重复释放链表
  FFreeNode := nil;

  DefaultFreeResources;
end;

procedure TObjectPool.FreeNodeObject(ANode: PLinkRec);
begin
  // 释放节点 ANode 关联的对象/内存空间
  case FObjectType of
    otEnvData:         // 客户端工作环境空间
      { 不在此释放 } ;
    otTaskInf:         // 发送数据描述
      FreeMem(ANode^.Data);    
    otIOData: begin    // 收发内存块
      FreeMem(PPerIOData(ANode^.Data)^.Data.buf);
      FreeMem(ANode^.Data);
    end;
    else  // 对象 TBaseSocket
      TBaseSocket(ANode^.Data).Free;
  end;
  ANode^.Data := Nil;
end;

function TObjectPool.GetFull: Boolean;
begin
  // 检查对象池是否用完
  FLock.Acquire;
  try
    Result := FUsedCount >= FInICount;
  finally
    FLock.Release;
  end;
end;

procedure TObjectPool.Lock;
begin
  FLock.Acquire;
end;

function TObjectPool.Pop: PLinkRec;
begin
  // 弹出 空闲 链表的顶，加入到在用链表顶
  FLock.Acquire;
  try
    if (FFreeNode = nil) then
      Result := AddNode  // 增加节点（同时建实例）
    else
      Result := FFreeNode;

    // 下一空闲节点为顶
    FFreeNode := FFreeNode^.Next;  // 下一节点上移
    if (FFreeNode <> Nil) then     // 不为空，指向空
      FFreeNode^.Prev := Nil;

    // 加 Result 到 在用 链表顶
    Result^.Prev := nil;
    Result^.Next := FFirstNode;
    Result^.InUsed := True;

    if (FFirstNode <> Nil) then
      FFirstNode^.Prev := Result;
    FFirstNode := Result;  // 为顶

    Inc(FUsedCount);
  finally
    FLock.Release;
  end;
end;

procedure TObjectPool.Push(const ANode: PLinkRec);
begin
  // 把 在用/队列 链表的 ANode 加入到空闲链表
  FLock.Acquire;
  try
    if ANode^.InUsed and (FUsedCount > 0) then
    begin
      // 从 在用 链表中断开 ANode
      if (ANode^.Prev = Nil) then  // 是顶
      begin
        FFirstNode := ANode^.Next; // 下一节点上移
        if (FFirstNode <> Nil) then
          FFirstNode^.Prev := nil;
      end else
      begin  // 上节点连下节点
        ANode^.Prev^.Next := ANode^.Next;
        if (ANode^.Next <> Nil) then  // 不是底
          ANode^.Next^.Prev := ANode^.Prev;
      end;

      // 加入到 空闲 链表的顶
      ANode^.Prev := Nil;
      ANode^.Next := FFreeNode;
      ANode^.InUsed := False;

      if (FFreeNode <> Nil) then
        FFreeNode^.Prev := ANode;
      FFreeNode := ANode;  // 变顶

      Dec(FUsedCount);    // 只在此处 -
    end;
  finally
    FLock.Release;
  end;
end;

procedure TObjectPool.OptimizeDetail(List: TInDataList);
var
  i: Integer;
  PNode, PTail: PLinkRec;
  PFreeLink: PLinkRec;
  UsedNodes: TInDataList;
begin
  // 优化资源，恢复到初始化时的状态。
  // （释放非自动增加的节点 Auto = False）

  // 遍历空闲链表，建非自动建节点的链表 -> 释放

  FLock.Acquire;
  UsedNodes := TInDataList.Create;
  
  try
    PTail := nil;            // 尾
    PFreeLink := Nil;        // 空链表

    PNode := FFreeNode;      // 当前节点
    while (PNode <> nil) do  // 遍历空闲链表
    begin
      if (PNode^.Auto = False) then
      begin
        // 从空闲表中脱开节点 PNode
        if (PNode^.Prev = Nil) then  // 是顶
        begin
          FFreeNode := PNode^.Next;  // 下一节点上移
          if (FFreeNode <> Nil) then
            FFreeNode^.Prev := nil;
        end else                     // 上节点连下节点
        begin
          PNode^.Prev^.Next := PNode^.Next;
          if (PNode^.Next <> Nil) then // 不是底
            PNode^.Next^.Prev := PNode^.Prev;
        end;

        // 建链表
        if (PFreeLink = nil) then
          PFreeLink := PNode
        else
          PTail^.Next := PNode;

        // 当作尾
        PTail := PNode;
        PTail^.InUsed := False;

        // 取下一节点
        PNode := PNode^.Next;

        // 在后
        PTail^.Next := nil;

        Dec(FNodeCount);
      end else
        PNode := PNode^.Next;
    end;

    if Assigned(List) then
    begin
      // 优化管理列表
      // 把新增、使用中的节点移到 FIniCount 位置之前
      for i := FIniCount to List.Count - 1 do
      begin
        PNode := List.Items[i];
        if Assigned(PNode) and PNode^.InUsed then
          UsedNodes.Add(PNode);
      end;

      // UsedNodes 的节点前移
      for i := 0 to UsedNodes.Count - 1 do
        List.Items[FIniCount + i] := UsedNodes.Items[i];

      // 调整节点数
      List.Count := FNodeCount;
    end;
  finally
    UsedNodes.Free;
    FLock.Release;
  end;

  // 释放链表 PFreeLink 的节点资源

  while (PFreeLink <> Nil) do
  begin
    PNode := PFreeLink;
    PFreeLink := PFreeLink^.Next;
    if (PNode^.Data <> Nil) then
      FreeNodeObject(PNode);
    Dispose(PNode);
  end;
  
end;

procedure TObjectPool.Optimize;
begin
  OptimizeDetail(nil);    // 优化资源，恢复到初始化时的状态。
end;

procedure TObjectPool.Scan(var FromObject: Pointer; Callback: TScanListEvent);
var
  CancelScan: Boolean;
  PNode: PLinkRec;
begin
  // 遍历 在用 链表对象(调用前要加锁)
  //  FromObject: 一个对象或内存开始位置
  CancelScan := False;
  PNode := FFirstNode;
  while (PNode <> Nil) and (CancelScan = False) do
  begin
    Callback(FObjectType, FromObject, PNode^.Data, CancelScan);
    PNode := PNode^.Next;
  end;
end;

procedure TObjectPool.UnLock;
begin
  FLock.Release;
end;

{ TDataListPool }

constructor TDataListPool.Create(AObjectType: TObjectType; ASize: Integer);
begin
  // 增加一个资源节点列表，释放时加快速度
  FNodeList := TInDataList.Create;
  inherited;
end;

procedure TDataListPool.CreateObjData(const ALinkNode: PLinkRec);
begin
  FNodeList.Add(ALinkNode);  // 登记到列表
end;

destructor TDataListPool.Destroy;
begin
  FreeListObjects(FNodeList);  // 用列表法删除资源
  FNodeList.Free;
  inherited;
end;

procedure TDataListPool.Optimize;
begin
  OptimizeDetail(FNodeList);
end;

{ TIOCPSocketPool }

function TIOCPSocketPool.Clone(Source: TObject): TObject;
var
 Socket: TBaseSocketRef;
begin
  // 复制一个 TBaseSocket
  FLock.Acquire;
  try
    Socket := Pop^.Data;
    Socket.Clone(TBaseSocket(Source));
    Result := Socket;
  finally
    FLock.Release;
  end;
end;

constructor TIOCPSocketPool.Create(AObjectType: TObjectType; ASize: Integer);
begin
  case AObjectType of
    otBroker:        // 代理 
      FSocketClass := TSocketBroker;
    otSocket:        // TIOCPSocket
      FSocketClass := TIOCPSocket;
    otHttpSocket:    // THttpSocket
      FSocketClass := THttpSocket;
    otStreamSocket:  // TStreamSocket
      FSocketClass := TStreamSocket;
    otWebSocket:
      FSocketClass := TWebSocket;
    else
      raise Exception.Create('TBaseSocket 类型错误.');
  end;
  inherited;
end;

procedure TIOCPSocketPool.CreateObjData(const ALinkNode: PLinkRec);
begin
  inherited;
  // 链表节点的 LinkNode.Data 存放一个 Socket 对象,
  //   Socket.LinkNode 记录链表节点（双向记录，方便回收空间）
  ALinkNode^.Data := TBaseSocketClass(FSocketClass).Create(Self, ALinkNode);
end;

procedure TIOCPSocketPool.GetSockets(List: TInList; IngoreSocket: TObject; AdminType: Boolean);
var
  PNode: PLinkRec;
begin
  // 推送时，取接收过数据的全部节点
  // 不能推送给未投放 WSARecv 成功 Socket，否则异常，CPU 大涨
  FLock.Acquire;
  try
    PNode := FFirstNode;
    while (PNode <> Nil) do  // 遍历在用表
    begin
      if (PNode^.Data <> IngoreSocket) and (
         (AdminType = False) or (TIOCPSocket(PNode^.Data).Role >= crAdmin)) then
         List.Add(PNode^.Data);
      PNode := PNode^.Next;
    end;
  finally
    FLock.Release;
  end;
end;

{ TIODataPool }

constructor TIODataPool.Create(ASize: Integer);
begin
  inherited Create(otIOData, ASize * 5); // 5 倍缓存
end;

procedure TIODataPool.CreateObjData(const ALinkNode: PLinkRec);
var
  IOData: PPerIOData;
begin
  inherited;
  // 链表节点的 LinkNode.Data 指向 TPerIOData 内存块
  GetMem(ALinkNode^.Data, SizeOf(TPerIOData));

  IOData := PPerIOData(ALinkNode^.Data);
  IOData^.Node := ALinkNode;
  IOData^.Data.len := IO_BUFFER_SIZE;

  // 内存、虚拟内存要充裕，否则会 out of memory！
  GetMem(IOData^.Data.buf, IO_BUFFER_SIZE);
end;

destructor TIODataPool.Destroy;
begin
  inherited;
end;

{ TStringHash }

procedure TStringHash.Add(const Key: AnsiString; Value: Pointer);
var
  Hash: Integer;
  Bucket: PHashItem;
begin
  FLock.Acquire;
  try
    Hash := HashOf(Key) mod Cardinal(Length(FBuckets));
    New(Bucket);
    Bucket^.Key := Key;
    Bucket^.Value := Value;
    Bucket^.Next := FBuckets[Hash];
    FBuckets[Hash] := Bucket;
    Inc(FCount);
  finally
    FLock.Release;
  end;
end;

procedure TStringHash.Clear;
var
  i: Integer;
  Prev, Next: PHashItem;
begin
  FLock.Acquire;
  try
    if (FCount > 0) then
      for i := 0 to Length(FBuckets) - 1 do
      begin
        Prev := FBuckets[i];
        FBuckets[i] := nil;
        while Assigned(Prev) do
        begin
          Next := Prev^.Next;
          FreeItemData(Prev);  // 增加
          Dispose(Prev);
          Prev := Next;
          Dec(FCount);
        end;
      end;
  finally
    FLock.Release;
  end;
end;

constructor TStringHash.Create(Size: Cardinal);
begin
  SetLength(FBuckets, Size);
  FLock := TThreadLock.Create;  
end;

destructor TStringHash.Destroy;
begin
  Clear;
  FLock.Free;
  SetLength(FBuckets, 0);
  inherited Destroy;
end;

function TStringHash.Find(const Key: AnsiString): PPHashItem;
var
  Hash: Integer;
begin
  // Key 用 AnsiString
  Hash := HashOf(Key) mod Cardinal(Length(FBuckets));
  Result := @FBuckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

procedure TStringHash.FreeItemData(Item: PHashItem);
begin
  // 释放节点关联的数据
end;

function TStringHash.HashOf(const Key: AnsiString): Cardinal;
var
  i: Integer;
begin
  // Key 用 AnsiString
  Result := 0;
  for i := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(Key[i]);
end;

procedure TStringHash.Lock;
begin
  FLock.Acquire;
end;

function TStringHash.Modify(const Key: AnsiString; Value: Pointer): Boolean;
var
  P: PHashItem;
begin
  FLock.Acquire;
  try
    P := Find(Key)^;
    if Assigned(P) then
    begin
      Result := True;
      P^.Value := Value;
    end else
      Result := False;
  finally
    FLock.Release;
  end;
end;

procedure TStringHash.Remove(const Key: AnsiString);
var
  P: PHashItem;
  Prev: PPHashItem;
begin
  FLock.Acquire;
  try
    Prev := Find(Key);
    P := Prev^;
    if Assigned(P) then
    begin
      Prev^ := P^.Next;   // 断开 p
      FreeItemData(P);    // 增加
      Dispose(P);         // 删除 p
      Dec(FCount);
    end;
  finally
    FLock.Release;
  end;
end;

procedure TStringHash.Scan(var Dest: Pointer; CallEvent: TScanListEvent);
var
  i: Integer;
  P: PHashItem;
  CancelScan: Boolean;
begin
  // 遍历节点（要在外部加锁）
  //   见：TInClientManager.GetLoginedClients
  if (FCount > 0) then
  begin
    CancelScan := False;
    for i := 0 to Length(FBuckets) - 1 do
    begin
      P := FBuckets[i];
      while Assigned(P) do
      begin
        CallEvent(otEnvData, Dest, TObject(P^.Value), CancelScan);
        if CancelScan then
          Exit;
        P := P^.Next;
      end;
    end;
  end;
end;

procedure TStringHash.Scan(CallEvent: TScanHashEvent);
var
  i: Integer;
  Prev: PPHashItem;
  P: PHashItem;
begin
  // 遍历节点（要在外部加锁）
  if (FCount > 0) then
    for i := 0 to Length(FBuckets) - 1 do
    begin
      Prev := @FBuckets[i];
      P := Prev^;
      while Assigned(P) do
      begin
        if Assigned(CallEvent) then
          CallEvent(P^.Value);    // 引用对象被释放 -> P^.Value = Nil
        if (P^.Value = Nil) then  // P^.Value = Nil 对象被释放
        begin
          Dec(FCount);
          Prev^ := P^.Next;
          Dispose(P);             // 释放节点空间
          P := Prev^;
        end else
        begin
          Prev := @P;
          P := P^.Next;
        end;
      end;
    end;
end;

procedure TStringHash.UnLock;
begin
  FLock.Release;
end;

function TStringHash.ValueOf(const Key: AnsiString): Pointer;
var
  P: PHashItem;
begin
  FLock.Acquire;
  try
    P := Find(Key)^;
    if Assigned(P) then
      Result := P^.Value
    else
      Result := Nil;
  finally
    FLock.Release;
  end;
end;

function TStringHash.ValueOf2(const Key: AnsiString): Pointer;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if Assigned(P) then
    Result := P^.Value
  else
    Result := Nil;
end;

{ TPreventAttack }

function TPreventAttack.CheckAttack(const PeerIP: String; MSecond, InterCount: Integer): Boolean;
var
  Item: PAttackInfo;
  TickCount: Int64;
begin
  // 检查恶意攻击
  //   正常的客户端连接数不多，也不频繁
  TickCount := GetUTCTickCount;
  Lock;
  try
    Item := Self.ValueOf2(PeerIP);
    if Assigned(Item) then
    begin
      // MSecond 毫秒内出现 InterCount 个客户端连接，
      // 当作攻击, 15 分钟内禁止连接
      if (Item^.TickCount > TickCount) then  // 攻击过，未解禁
        Result := True
      else begin
        Result := (Item^.Count >= InterCount) and
                  (TickCount - Item^.TickCount <= MSecond);
        if Result then  // 是攻击
          Item^.TickCount := TickCount + 900000  // 900 秒
        else
          Item^.TickCount := TickCount;
        Inc(Item^.Count);
      end;
    end else
    begin
      // 未重复连接过，登记
      Item := New(PAttackInfo);
      Item^.PeerIP := PeerIP;
      Item^.TickCount := TickCount;
      Item^.Count := 1;

      // 加入 Hash 表
      Self.Add(PeerIP, Item);

      Result := False;
    end;
  finally
    UnLock;
  end;

end;

procedure TPreventAttack.DecRef(const PeerIP: String);
var
  Item: PAttackInfo;
begin
  // 减少 IP 引用次数
  Lock;
  try
    Item := Self.ValueOf2(PeerIP);
    if Assigned(Item) and (Item^.Count > 0) then
      Dec(Item^.Count);
  finally
    UnLock;
  end;
end;

procedure TPreventAttack.FreeItemData(Item: PHashItem);
begin
  // 释放节点空间
  Dispose(PAttackInfo(Item^.Value));
end;

end.


