(*
 * iocp 服务端 SQL 管理器（可选）
 *)

unit iocp_sqlMgr;

interface

uses
  Classes, SysUtils,
  iocp_lists;

type

  // 一条 SQL 命令对象

  TSQLObject = class(TObject)
  private
    FSQL: TStrings;     // SQL 命令
    FSQLText: String;   // SQL 命令(运行时)
    FSQLName: String;   // 名称
    procedure ToStringEx;
  public
    constructor Create(const AName: String);
    destructor Destroy; override;
    property SQL: TStrings read FSQL;
    property SQLName: String read FSQLName;
  end;

  // 一组 SQL 命令的管理器

  TInSQLManager = class(TComponent)
  private
    FNames: TInList;          // SQL 命名列表
    FSQLs: TStrings;          // SQL 文本文件资源

    function GetCount: Integer;
    function GetItems(Index: Integer): TSQLObject;
    procedure ClearNames;
    procedure OnSQLChange(Sender: TObject);
    procedure SetSQLs(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSQL(Index: Integer): String; overload;
    function GetSQL(const AName: String): String; overload;
  public
    property Items[Index: Integer]: TSQLObject read GetItems;
  published
    property Count: Integer read GetCount;
    property SQLs: TStrings read FSQLs write SetSQLs;
  end;

implementation
  
{ TSQLObject }

constructor TSQLObject.Create(const AName: String);
begin
  inherited Create;
  FSQLName := AName;
  FSQL := TStringList.Create;
end;

destructor TSQLObject.Destroy;
begin
  if Assigned(FSQL) then
    FSQL.Free;
  inherited;
end;

procedure TSQLObject.ToStringEx;
begin
  FSQLText := FSQL.Text;
  FSQL.Clear;  // 也清空
end;

{ TInSQLManager }

procedure TInSQLManager.OnSQLChange(Sender: TObject);
var
  i: Integer;
  S: String;
  SQLObj: TSQLObject;
begin
  // FSQLs 内容改变，提取各段内容
  if (FSQLs.Count = 0) then
    Exit;

  // 提取 SQL 命令
  // 每一 Section 为一条命令，Section 为命名

  ClearNames;
  SQLObj := nil;

  for i := 0 to FSQLs.Count - 1 do
  begin
    S := Trim(FSQLs.Strings[i]);
    if (Length(S) >= 3) and (S[1] = '[') and (S[Length(S)] = ']') then  // Section 开始
    begin
      SQLObj := TSQLObject.Create(Copy(S, 2, Length(S) - 2));    // 区分大小写
      FNames.Add(SQLObj);
    end else
    if Assigned(SQLObj) then
    begin
      if (csDestroying in ComponentState) then
        SQLObj.FSQL.Add(S)
      else
      if (Length(S) > 0) and (S[1] <> '/') then
        SQLObj.FSQL.Add(S);
    end;
  end;

  // 运行状态，把命令转为 String，清空 FSQLs
  if not (csDesigning in ComponentState) then
  begin
    for i := 0 to FNames.Count - 1 do
      TSQLObject(FNames.Items[i]).ToStringEx;
    FSQLs.Clear;
  end;
end;

procedure TInSQLManager.ClearNames;
var
  i: Integer;
begin
  // 清除 SQL 命名表
  for i := 0 to FNames.Count - 1 do
    TSQLObject(FNames.Items[i]).Free;
  FNames.Clear;
end;

constructor TInSQLManager.Create(AOwner: TComponent);
begin
  inherited;
  FNames := TInList.Create;
  FSQLs := TStringList.Create;
  TStringList(FSQLs).OnChange := OnSQLChange;
end;

destructor TInSQLManager.Destroy;
begin
  ClearNames;
  FNames.Free;
  FSQLs.Free;
  inherited;
end;

function TInSQLManager.GetCount: Integer;
begin
  Result := FNames.Count;
end;

function TInSQLManager.GetItems(Index: Integer): TSQLObject;
begin
  Result := TSQLObject(FNames.Items[Index]);
end;

function TInSQLManager.GetSQL(Index: Integer): String;
begin
  // 运行状态调用！
  Result := TSQLObject(FNames.Items[Index]).FSQLText;
end;

function TInSQLManager.GetSQL(const AName: String): String;
var
  i: Integer;
  Obj: TSQLObject;
begin
  // 运行状态调用！
  for i := 0 to FNames.Count - 1 do
  begin
    Obj := TSQLObject(FNames.Items[i]);
    if (AName = Obj.FSQLName) then  // 区分大小写
    begin
      Result := Obj.FSQLText;
      Exit;
    end;
  end;;
end;

procedure TInSQLManager.SetSQLs(const Value: TStrings);
begin
  ClearNames;
  FSQLs.Clear;
  if Assigned(Value) then
    FSQLs.AddStrings(Value);
end;

end.
