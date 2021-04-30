unit iocp_baseModule;

interface

uses
  SysUtils, Classes, DB, DBClient, Provider, IniFiles, Variants,
  iocp_base, iocp_sockets, http_objects, iocp_msgPacks, iocp_WsJSON;

type

  // 数模基类
  // 开发时从本数模继承子类，在以下事件（属性）中操作数据库：
  //   OnApplyUpdates、OnExecQuery、OnExecSQL、OnExecStoredProcedure
  //   OnHttpExecQuery、OnHttpExecSQL

  // 请在子类的文件中加单元引用：MidasLib

  TExecSQLEvent      = procedure(AParams: TReceiveParams; AResult: TReturnResult) of object;
  TApplyUpdatesEvent = procedure(AParams: TReceiveParams; AResult: TReturnResult; out ErrorCount: Integer) of object;
  THttpRequestEvent  = procedure(Sender: TObject; Request: THttpRequest; Response: THttpResponse) of object;
  TWebSocketAction   = procedure(Sender: TObject; JSON: TReceiveJSON; Result: TResultJSON) of object;

  // 增加两个方法 AddToBackground、Wakeup，用于在数模实例中实现后台执行
  // 新增：后台、唤醒事件
  TBackgroundEvent   = procedure(Socket: TBaseSocket) of object;

  TInIOCPDataModule = class(TDataModule)
  private
    { Private declarations }
    FOnApplyUpdates: TApplyUpdatesEvent;   // 用 Delta 数据更新
    FOnExecQuery: TExecSQLEvent;           // 执行 SELECT-SQL，返回一个数据集
    FOnExecStoredProc: TExecSQLEvent;      // 执行存储过程，可能返回数据集
    FOnExecSQL: TExecSQLEvent;             // 执行 SQL 命令不返回数据集
    FOnHttpExecQuery: THttpRequestEvent;   // 执行 HTTP 查询
    FOnHttpExecSQL: THttpRequestEvent;     // 执行 HTTP 命令
    FOnWebSocketQuery: TWebSocketAction;   // WebSocket 查询
    FOnWebSocketUpdates: TWebSocketAction; // WebSocket 更新
  protected
    FBackgroundMethod: TBackgroundEvent;   // 加入后台事件
    FWakeupMethod: TBackgroundEvent;       // 唤醒事件
    procedure InstallDatabase(const AClassName); virtual;  // 配置数据库连接
    procedure InterApplyUpdates(const DataSetProviders: array of TDataSetProvider;
                                Params: TReceiveParams; out ErrorCount: Integer);
    procedure AddToBackground(Socket: TBaseSocket);  // 后台执行
    procedure Wakeup(Socket: TBaseSocket);  // 后台执行完毕后，唤醒客户端 
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure ApplyUpdates(AParams: TReceiveParams; AResult: TReturnResult);
    procedure ExecQuery(AParams: TReceiveParams; AResult: TReturnResult);
    procedure ExecSQL(AParams: TReceiveParams; AResult: TReturnResult);
    procedure ExecStoredProcedure(AParams: TReceiveParams; AResult: TReturnResult);
    procedure HttpExecQuery(Request: THttpRequest; Response: THttpResponse);
    procedure HttpExecSQL(Request: THttpRequest; Response: THttpResponse);
    procedure WebSocketQuery(JSON: TReceiveJSON; Result: TResultJSON);
    procedure WebSocketUpdates(JSON: TReceiveJSON; Result: TResultJSON);
  published
    property OnApplyUpdates: TApplyUpdatesEvent read FOnApplyUpdates write FOnApplyUpdates;
    property OnExecQuery: TExecSQLEvent read FOnExecQuery write FOnExecQuery;
    property OnExecSQL: TExecSQLEvent read FOnExecSQL write FOnExecSQL;
    property OnExecStoredProcedure: TExecSQLEvent read FOnExecStoredProc write FOnExecStoredProc;
    property OnHttpExecQuery: THttpRequestEvent read FOnHttpExecQuery write FOnHttpExecQuery;
    property OnHttpExecSQL: THttpRequestEvent read FOnHttpExecSQL write FOnHttpExecSQL;
    property OnWebSocketQuery: TWebSocketAction read FOnWebSocketQuery write FOnWebSocketQuery;
    property OnWebSocketUpdates: TWebSocketAction read FOnWebSocketUpdates write FOnWebSocketUpdates;
  end;

  TDataModuleClass = class of TInIOCPDataModule;
  
implementation

uses
  iocp_server;
  
{$R *.dfm}

{ TInIOCPDataModule }

procedure TInIOCPDataModule.AddToBackground(Socket: TBaseSocket);
begin
  if Assigned(FBackgroundMethod) then  // 加入后台
    FBackgroundMethod(Socket);
end;

procedure TInIOCPDataModule.ApplyUpdates(AParams: TReceiveParams; AResult: TReturnResult);
var
  ErrorCount: Integer;
begin
  if (Self = nil) then
  begin
    AResult.ActResult := arFail;
    AResult.Msg := '未建数模实例.';
  end else
  if Assigned(FOnApplyUpdates) then
    try
      if (AParams.VarCount > 0) then
        FOnApplyUpdates(AParams, AResult, ErrorCount)
      else begin
        AResult.ActResult := arFail;
        AResult.Msg := 'Delta 为空.';
      end;
    except
      on E: Exception do
      begin
        AResult.ActResult := arFail;
        AResult.Msg := E.Message;
      end;
    end;
end;

constructor TInIOCPDataModule.Create(AOwner: TComponent);
begin
  inherited;
  // AOwner 是 TInIOCPServer，见：TBusiWorker.CreateDataModules;
  if not (csDesigning in ComponentState) and
    Assigned(TInIOCPServer(AOwner).BaseManager) then
  begin
    FBackgroundMethod := TInIOCPServer(AOwner).BaseManager.AddToBackground; // 加入后台事件
    FWakeupMethod := TInIOCPServer(AOwner).BaseManager.Wakeup;  // 唤醒事件
  end;
end;

procedure TInIOCPDataModule.ExecQuery(AParams: TReceiveParams; AResult: TReturnResult);
begin
  if (Self = nil) then
  begin
    AResult.ActResult := arFail;
    AResult.Msg := '未建数模实例.';
  end else
  if Assigned(FOnExecQuery) then
    try
      FOnExecQuery(AParams, AResult);
    except
      on E: Exception do
      begin
        AResult.ActResult := arFail;
        AResult.Msg := E.Message;
      end;
    end;
end;

procedure TInIOCPDataModule.ExecSQL(AParams: TReceiveParams; AResult: TReturnResult);
begin
  if (Self = nil) then
  begin
    AResult.ActResult := arFail;
    AResult.Msg := '未建数模实例.';
  end else
  if Assigned(FOnExecSQL) then
    try
      FOnExecSQL(AParams, AResult);
    except
      on E: Exception do
      begin
        AResult.ActResult := arFail;
        AResult.Msg := E.Message;
      end;
    end;
end;

procedure TInIOCPDataModule.ExecStoredProcedure(AParams: TReceiveParams; AResult: TReturnResult);
begin
  if (Self = nil) then
  begin
    AResult.ActResult := arFail;
    AResult.Msg := '未建数模实例.';
  end else
  if Assigned(FOnExecStoredProc) then
    try
      FOnExecStoredProc(AParams, AResult);
    except
      on E: Exception do
      begin
        AResult.ActResult := arFail;
        AResult.Msg := E.Message;
      end;
    end;
end;

procedure TInIOCPDataModule.HttpExecQuery(Request: THttpRequest; Response: THttpResponse);
begin
  if (Self = nil) then        // 返回 JSON 格式的异常
  begin
    Response.SendJSON('{"Error":"未建数模实例."}');
  end else
  if Assigned(FOnHttpExecQuery) then
    try
      FOnHttpExecQuery(Self, Request, Response);
    except
      on E: Exception do      // 返回 JSON 格式的异常
        Response.SendJSON('{"Error":"' + E.Message + '"}');
    end;
end;

procedure TInIOCPDataModule.HttpExecSQL(Request: THttpRequest; Response: THttpResponse);
begin
  if (Self = nil) then       // 返回 JSON 格式的异常
  begin
    Response.SendJSON('{"Error":"未建数模实例."}');
  end else
  if Assigned(FOnHttpExecSQL) then
    try
      FOnHttpExecSQL(Self, Request, Response);
    except
      on E: Exception do     // 返回 JSON 格式的异常
        Response.SendJSON('{"Error":"' + E.Message + '"}');
    end;
end;

procedure TInIOCPDataModule.InstallDatabase(const AClassName);
begin
  // 如果各子类数模都用同一套数据库组件，可以在设计时加数据库
  // 组件到 TInIOCPDataModule，在此写通用的配置数据库连接方法，
  // 子类调用即可。
{  with TIniFile.Create('db_options.ini') do
  begin
    DatabaseConnection.DatabaseName := ReadString(AClassName, 'DatabaseName', '');
    ... ...
  end; }
end;

procedure TInIOCPDataModule.InterApplyUpdates(
          const DataSetProviders: array of TDataSetProvider;
          Params: TReceiveParams; out ErrorCount: Integer);
var
  i, k, n: Integer;
  DeltaField: TVarField;
begin
  // 用 Delta 更新数据表
  // 新版改变：
  //   1. 第1、2个字段为用户名称 __UserGroup, __UserName，
  //   2. 以后字段为 Delta 数据，可能有多个。

  // 遍历各 Variant 元素（可能为 Null）
  //   子表可能有外键，先更新子表，最后更新主表
  try
    k := 0;
    n := 0;
    for i := Params.VarCount - 1 downto 2 do  // 忽略第1、2字段：__UserGroup, __UserName
    begin
      DeltaField := Params.Fields[i];  // 3,2,1
      if (DeltaField.VarType = etVariant) then  // Delta 类型数据
      begin
        if (DeltaField.IsNull = False) then  // 不为 Null
          try
            DataSetProviders[n].ApplyUpdates(DeltaField.AsVariant, 0, ErrorCount);
          finally
            Inc(k, ErrorCount);
          end;
        Inc(n);
      end;
    end;
    ErrorCount := k;
  except
    raise;  // 重新触发异常
  end;
end;

procedure TInIOCPDataModule.Wakeup(Socket: TBaseSocket);
begin
  if Assigned(FWakeupMethod) then  // 唤醒
    FWakeupMethod(Socket);
end;

procedure TInIOCPDataModule.WebSocketQuery(JSON: TReceiveJSON; Result: TResultJSON);
begin
  if (Self = nil) then       // 返回 JSON 格式的异常
  begin
    Result.S['Error'] := '未建数模实例.';
  end else
  if Assigned(FOnWebSocketQuery) then
    try
      FOnWebSocketQuery(TWebSocket(JSON.Socket).Worker, JSON, Result);
    except
      on E: Exception do     // 返回 JSON 格式的异常
        Result.S['Error'] := E.Message;
    end;
end;

procedure TInIOCPDataModule.WebSocketUpdates(JSON: TReceiveJSON; Result: TResultJSON);
begin
  if (Self = nil) then
    Result.S['Error'] := '未建数模实例.'
  else
  if Assigned(FOnWebSocketUpdates) then
    try
      FOnWebSocketUpdates(Self, JSON, Result);
    except
      on E: Exception do     // 返回 JSON 格式的异常
        Result.S['Error'] := E.Message;
    end;
end;

end.
