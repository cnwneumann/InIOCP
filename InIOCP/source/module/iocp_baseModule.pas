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
  TApplyUpdatesEvent = procedure(Params: TReceiveParams; out ErrorCount: Integer; AResult: TReturnResult) of object;
  THttpRequestEvent  = procedure(Sender: TObject; Request: THttpRequest; Respone: THttpRespone) of object;
  TWebSocketAction   = procedure(Sender: TObject; JSON: TBaseJSON; Result: TResultJSON) of object;

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
    // 配置数据库连接
    procedure InstallDatabase(const AClassName); virtual;
    procedure InterApplyUpdates(const DataSetProviders: array of TDataSetProvider;
                                Params: TReceiveParams; out ErrorCount: Integer);
  public
    { Public declarations }
    procedure ApplyUpdates(AParams: TReceiveParams; AResult: TReturnResult);
    procedure ExecQuery(AParams: TReceiveParams; AResult: TReturnResult);
    procedure ExecSQL(AParams: TReceiveParams; AResult: TReturnResult);
    procedure ExecStoredProcedure(AParams: TReceiveParams; AResult: TReturnResult);
    procedure HttpExecQuery(Request: THttpRequest; Respone: THttpRespone);
    procedure HttpExecSQL(Request: THttpRequest; Respone: THttpRespone);
    procedure WebSocketQuery(JSON: TBaseJSON; Result: TResultJSON);
    procedure WebSocketUpdates(JSON: TBaseJSON; Result: TResultJSON);
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
  
{$R *.dfm}

{ TInIOCPDataModule }

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
        FOnApplyUpdates(AParams, ErrorCount, AResult)
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

procedure TInIOCPDataModule.HttpExecQuery(Request: THttpRequest; Respone: THttpRespone);
begin
  if (Self = nil) then        // 返回 JSON 格式的异常
  begin
    Respone.SendJSON('{"Error":"未建数模实例."}');
  end else
  if Assigned(FOnHttpExecQuery) then
    try
      FOnHttpExecQuery(Self, Request, Respone);
    except
      on E: Exception do      // 返回 JSON 格式的异常
        Respone.SendJSON('{"Error":"' + E.Message + '"}');
    end;
end;

procedure TInIOCPDataModule.HttpExecSQL(Request: THttpRequest; Respone: THttpRespone);
begin
  if (Self = nil) then       // 返回 JSON 格式的异常
  begin
    Respone.SendJSON('{"Error":"未建数模实例."}');
  end else
  if Assigned(FOnHttpExecSQL) then
    try
      FOnHttpExecSQL(Self, Request, Respone);
    except
      on E: Exception do     // 返回 JSON 格式的异常
        Respone.SendJSON('{"Error":"' + E.Message + '"}');
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
  i, k: Integer;
  DeltaField: TVarField;
begin
  // 用 Delta 更新数据表
  // 新版改变：
  //   1. 第一个字段为用户名称 _UserName，
  //   2. 以后字段为 Delta 数据，可能有多个。

  // 遍历各 Variant 元素（可能为 Null）
  //   子表可能有外键，先更新子表，最后更新主表
  try
    k := 0;
    for i := Params.VarCount - 1 downto 1 do  // 忽略第一字段：_UserName
    begin
      DeltaField := Params.Fields[i];  // 3,2,1
      if (DeltaField.IsNull = False) then  // 有 Delta 数据
        try
          DataSetProviders[i - 1].ApplyUpdates(DeltaField.AsVariant, 0, ErrorCount);
        finally
          Inc(k, ErrorCount);
        end;
    end;
    ErrorCount := k;
  except
    raise;  // 重新触发异常
  end;
end;

procedure TInIOCPDataModule.WebSocketQuery(JSON: TBaseJSON; Result: TResultJSON);
begin
  if (Self = nil) then       // 返回 JSON 格式的异常
  begin
    Result.S['Error'] := '未建数模实例.';
  end else
  if Assigned(FOnWebSocketQuery) then
    try
      FOnWebSocketQuery(Self, JSON, Result);
    except
      on E: Exception do     // 返回 JSON 格式的异常
        Result.S['Error'] := E.Message;
    end;
end;

procedure TInIOCPDataModule.WebSocketUpdates(JSON: TBaseJSON; Result: TResultJSON);
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
