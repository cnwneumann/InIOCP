unit dm_iniocp_sqlite3;

interface

uses
  // 使用时请加单元引用 MidasLib！
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DB, DBClient, Provider,
  iocp_baseModule, iocp_base, iocp_objPools, iocp_sockets,
  iocp_sqlMgr, http_base, http_objects, iocp_WsJSON, MidasLib,
  // 使用 ZeosDBO 连接 SQLite3
  ZAbstractRODataset, ZAbstractDataset, ZDataset, ZConnection,
  ZSqlUpdate, ZDbcIntfs;

type

  // 数据库操作
  // 从 iocp_baseModule.TInIOCPDataModule 继承新建

  // 操作数据库的事件属性包括：
  //   OnApplyUpdates、OnExecQuery、OnExecSQL、OnExecStoredProcedure
  //   OnHttpExecQuery、OnHttpExecSQL

  TdmInIOCPSQLite3 = class(TInIOCPDataModule)
    DataSetProvider1: TDataSetProvider;
    InSQLManager1: TInSQLManager;
    procedure InIOCPDataModuleCreate(Sender: TObject);
    procedure InIOCPDataModuleDestroy(Sender: TObject);
    procedure InIOCPDataModuleApplyUpdates(Params: TReceiveParams;
      out ErrorCount: Integer; AResult: TReturnResult);
    procedure InIOCPDataModuleExecQuery(AParams: TReceiveParams;
      AResult: TReturnResult);
    procedure InIOCPDataModuleExecSQL(AParams: TReceiveParams;
      AResult: TReturnResult);
    procedure InIOCPDataModuleExecStoredProcedure(AParams: TReceiveParams;
      AResult: TReturnResult);
    procedure InIOCPDataModuleHttpExecQuery(Sender: TObject;
      Request: THttpRequest; Respone: THttpRespone);
    procedure InIOCPDataModuleHttpExecSQL(Sender: TObject;
      Request: THttpRequest; Respone: THttpRespone);
    procedure InIOCPDataModuleWebSocketQuery(Sender: TObject; JSON: TBaseJSON;
      Result: TResultJSON);
    procedure InIOCPDataModuleWebSocketUpdates(Sender: TObject; JSON: TBaseJSON;
      Result: TResultJSON);
  private
    { Private declarations }
    FConnection: TZConnection;
    FQuery, FQueryUsers: TZQuery;
    FQueryLogin: TZQuery;
    FQueryUserInfo: TZQuery;
    FExecSQL: TZQuery;
    FCurrentSQLName: String;
    procedure InterExecQuery(AQuery: TZQuery);
    procedure InterExecSQL(AExecSQL: TZQuery);
    procedure CommitTrans;
    procedure RollbackTrans;
    procedure StartTrans;
  public
    { Public declarations }
    // 增设用户管理的方法
    procedure DeleteUser(AParams: TReceiveParams; AResult: TReturnResult);
    procedure ModifyUser(AParams: TReceiveParams; AResult: TReturnResult);
    procedure QueryUser(AParams: TReceiveParams; AResult: TReturnResult);
    procedure RegisterUser(AParams: TReceiveParams; AResult: TReturnResult);
    procedure UserLogin(AParams: TReceiveParams; AResult: TReturnResult);
    procedure UserLogout(AParams: TReceiveParams; AResult: TReturnResult);
  end;

implementation

uses
  iocp_Varis, iocp_baseObjs, iocp_utils;

var
  FSQLite3Lock: TThreadLock = nil;

{$R *.dfm}

procedure TdmInIOCPSQLite3.CommitTrans;
begin
  if FConnection.InTransaction then
    FConnection.Commit;
end;

procedure TdmInIOCPSQLite3.RollbackTrans;
begin
  if FConnection.InTransaction then
    FConnection.Rollback;
end;

procedure TdmInIOCPSQLite3.StartTrans;
begin
  if not FConnection.InTransaction then
    FConnection.StartTransaction;
end;

procedure TdmInIOCPSQLite3.InterExecQuery(AQuery: TZQuery);
begin
  // SQLite3 独占事务，查询后直接回滚
  FSQLite3Lock.Acquire;
  try
    try
      if not FConnection.InTransaction then
        FConnection.StartTransaction;
      AQuery.Active := True;
      FConnection.Commit;
    finally
      FSQLite3Lock.Release;
    end;
  except
    Raise;
  end;
end;

procedure TdmInIOCPSQLite3.InterExecSQL(AExecSQL: TZQuery);
begin
  // SQLite3 独占事务，提交后不启用事务
  FSQLite3Lock.Acquire;
  try
    try
      if not FConnection.InTransaction then
        FConnection.StartTransaction;
      AExecSQL.ExecSQL;
      FConnection.Commit; 
    except
      if FConnection.InTransaction then
        FConnection.Rollback;
      Raise;
    end;
  finally
    FSQLite3Lock.Release;
  end;
end;

// ========================

procedure TdmInIOCPSQLite3.InIOCPDataModuleCreate(Sender: TObject);
begin
  inherited;

  // 用 InSQLManager1.SQLs 装入 SQL 资源文件（文本文件）
  if FileExists('sql\数据库操作.sql') then
    InSQLManager1.SQLs.LoadFromFile('sql\数据库操作.sql');

  // 用 ZeroLib 连接 SQLite3 数据库，内含数据表:
  (* CREATE TABLE tbl_users (
       USER_CODE CHAR(6) NOT NULL UNIQUE,
       USER_NAME VARCHAR(20) NOT NULL PRIMARY KEY,
       USER_PASSWORD CHAR(10) NOT NULL,
       USER_LEVEL INTEGER NOT NULL,
       USER_REAL_NAME VARCHAR(20),
       USER_TELEPHONE VARCHAR(30),
       USER_LOGIN_TIME TIMESTAMP,
       USER_LOGOUT_TIME TIMESTAMP,
       ACT_EXECUTOR VARCHAR(20) )  *)

  FConnection := TZConnection.Create(Self);
  FConnection.AutoCommit := False;
  FConnection.Database := 'data\app_data.qdb';
  FConnection.LoginPrompt := False;
  FConnection.Protocol := 'sqlite-3';
  FConnection.TransactIsolationLevel := tiReadCommitted;

  FQuery := TZQuery.Create(Self);    // 通用的查询
  FExecSQL := TZQuery.Create(Self);  // 通用的执行 SQL

  FQueryUsers := TZQuery.Create(Self); // 查询用户列表
  FQueryLogin := TZQuery.Create(Self); // 查询登录帐号密码
  FQueryUserInfo := TZQuery.Create(Self); // 查询用户是否存在

  FQuery.Connection := FConnection;
  FExecSQL.Connection := FConnection;

  FQueryUsers.Connection := FConnection;
  FQueryLogin.Connection := FConnection;
  FQueryUserInfo.Connection := FConnection;
  
  // SQL 固定，预设！
  FQueryUsers.SQL.Text := InSQLManager1.GetSQL('USER_QUERY_ALL');  // 区分大小写
  FQueryLogin.SQL.Text := InSQLManager1.GetSQL('USER_LOGIN');
  FQueryUserInfo.SQL.Text := InSQLManager1.GetSQL('USER_QUERY_INFO');
  
  // 自动解析 SQL 参数
  FQuery.ParamCheck := True;
  FExecSQL.ParamCheck := True;

  DataSetProvider1.DataSet := FQuery;
  FConnection.Connected := True;
end;

procedure TdmInIOCPSQLite3.InIOCPDataModuleDestroy(Sender: TObject);
begin
  inherited;
  FQuery.Free;
  FExecSQL.Free;
  FQueryUsers.Free;
  FQueryLogin.Free;  
  FQueryUserInfo.Free;
  FConnection.Free;
end;

procedure TdmInIOCPSQLite3.InIOCPDataModuleApplyUpdates(Params: TReceiveParams;
  out ErrorCount: Integer; AResult: TReturnResult);
begin
  // 用 DataSetPrivoder.Delta 更新
  FSQLite3Lock.Acquire;
  try
    try
      StartTrans;
      
      // 新版改变：
      //   1. 第一个字段为用户名称 _UserName，
      //   2. 以后字段为 Delta 数据，可能有多个，本例子只有一个。

      // 参考：TBaseMessage.LoadFromVariant
      //  Params.Fields[0]：用户名 _UserName
      //  Params.Fields[1].Name：字段名称，对应数据表名称
      //  Params.Fields[1].AsVariant：Delta 数据

      // 执行父类的更新方法
      // 用一组 TDataSetProvider 更新，本例子只有一个
      InterApplyUpdates([DataSetProvider1], Params, ErrorCount);
    except
      RollbackTrans;
      Raise;    // 基类有异常处理，要 Raise
    end;
  finally
    if ErrorCount = 0 then
    begin
      CommitTrans;
      AResult.ActResult := arOK;
    end else
    begin
      RollbackTrans;
      AResult.ActResult := arFail;
      AResult.AsInteger['ErrorCount'] := ErrorCount;
    end;
    FSQLite3Lock.Release;
  end;
  
end;

procedure TdmInIOCPSQLite3.InIOCPDataModuleExecQuery(AParams: TReceiveParams; AResult: TReturnResult);
var
  SQLName: String;
begin
  // 查询数据
  // 基类有异常处理

  // 2.0 预设了 SQL、SQLName 属性
  //     查找服务端名称为 SQLName 的 SQL 语句，执行
  //     要和客户端的命令配合

  SQLName := AParams.SQLName;
  if (SQLName = '') then  // 改用 SQL（未必就是 SELECT-SQL）
    with FQuery do
    begin
      SQL.Clear;
      SQL.Add(AParams.SQL);
    end
  else
  if (SQLName <> FCurrentSQLName) then
  begin
    FCurrentSQLName := SQLName;
    with FQuery do
    begin
      SQL.Clear;
      SQL.Add(InSQLManager1.GetSQL(SQLName));
    end;
  end;

  InterExecQuery(FQuery);

  // 新版改进：
  //   把一组数据集转换为流，返回给客户端，执行结果为 arOK
  // AResult.LoadFromVariant([数据集a, 数据集b, 数据集c], ['数据表a', '数据表b', '数据表c']);
  //   数据表n 是 数据集n 对应的数据表名称，用于更新
  // 如果有多个数据集，第一个为主表
  
  AResult.LoadFromVariant([DataSetProvider1], ['tbl_xzqh']);
  AResult.ActResult := arOK;

  FQuery.Active := False;   // 关闭
end;

procedure TdmInIOCPSQLite3.InIOCPDataModuleExecSQL(AParams: TReceiveParams; AResult: TReturnResult);
var
  i: Integer;
  SQLName: string;
begin
  // 执行 SQL
  // 基类有异常处理
  try
    // 取 SQL 名称
    SQLName := AParams.SQLName;

    if (SQLName = '') then  // 用 SQL 文本
      FExecSQL.SQL.Text := AParams.SQL
    else
    if (SQLName <> FCurrentSQLName) then  // 用 SQL 名称
    begin
      FCurrentSQLName := SQLName;
      FExecSQL.SQL.Text := InSQLManager1.GetSQL(SQLName);
    end;

    if AParams.HasParams then  // 客户端设定有参数
      for i := 0 to FExecSQL.Params.Count - 1 do  // 不更新 BLOB
        with FExecSQL.Params[i] do
          Value := AParams.AsString[Name];

    InterExecSQL(FExecSQL);   // 独占执行

    AResult.ActResult := arOK;  // 执行成功 arOK
  except
    Raise;    // 基类有异常处理，要 Raise
  end;
end;

procedure TdmInIOCPSQLite3.InIOCPDataModuleExecStoredProcedure(
  AParams: TReceiveParams; AResult: TReturnResult);
begin
  // 执行存储过程
  try
    // 这是存储过程名称：
    // ProcedureName := AParams.StoredProcName;
    // 见：TInDBQueryClient.ExecStoredProc
    //     TInDBSQLClient.ExecStoredProc

    // 这样返回数据集：
    // AResult.LoadFromVariant([DataSetProvider1], ['tbl_xzqh']);

    if AParams.StoredProcName = 'ExecuteStoredProc2' then  // 测试存储过程（数据未实现）
      InIOCPDataModuleExecQuery(AParams, AResult)  // 返回一个数据集
    else
      AResult.ActResult := arOK;
  except
    Raise;    // 基类有异常处理，要 Raise
  end;
end;

procedure TdmInIOCPSQLite3.InIOCPDataModuleHttpExecQuery(Sender: TObject;
  Request: THttpRequest; Respone: THttpRespone);
var
  i: Integer;
  SQLName: String;
begin
  // Http 服务：在这里执行 SQL 查询，用 Respone 返回结果
  try
    try

      // 用 SQL 名称查找对应的 SQL 文本
      // Http 的 Request.Params 没有预设 sql, sqlName 属性

      SQLName := Request.Params.AsString['SQL'];

      if (FCurrentSQLName <> SQLName) then   // 名称改变，重设 SQL
      begin
        FQuery.SQL.Clear;
        FQuery.SQL.Add(InSQLManager1.GetSQL(SQLName));
        FCurrentSQLName := SQLName;
      end;

      // 用 Request.ConectionState 或 Respone.ConectionState
      // 检查连接状态是否正常, 不正常无需再查询发送
      if Request.SocketState then  // 旧版：ConnectionState
      begin
        // 通用一点的赋值方法：
        //   Select xxx from ttt where code=:code and no=:no and datetime=:datetime
        with FQuery do
          for i := 0 to Params.Count - 1 do
            Params.Items[i].Value := Request.Params.AsString[Params.Items[i].Name];
      end;

      InterExecQuery(FQuery);
      
      // 转换全部记录为 JSON，用 Respone 返回
      //   小数据集可用：
      //      Respone.CharSet := hcsUTF8;  // 指定字符集
      //      Respone.SendJSON(iocp_utils.DataSetToJSON(FQuery, Respone.CharSet))
      //   推荐用 Respone.SendJSON(FQuery)，分块发送
      // 见：iocp_utils 单元 DataSetToJSON、LargeDataSetToJSON、InterDataSetToJSON
      if Request.SocketState then
      begin
        Respone.SendJSON(FQuery);  // 用默认字符集 gb2312
//        Respone.SendJSON(FQuery, hcsUTF8);  // 转为 UTF-8 字符集
      end;

    finally
      FQuery.Active := False;
    end;
  except
    Raise;
  end;
end;

procedure TdmInIOCPSQLite3.InIOCPDataModuleHttpExecSQL(Sender: TObject;
  Request: THttpRequest; Respone: THttpRespone);
begin
  // Http 服务：在这里执行 SQL 命令，用 Respone 返回结果
end;

procedure TdmInIOCPSQLite3.InIOCPDataModuleWebSocketQuery(Sender: TObject; JSON: TBaseJSON; Result: TResultJSON);
begin
  // 执行 WebSocket 的操作
  FQuery.SQL.Text := 'SELECT * FROM tbl_xzqh';

  InterExecQuery(FQuery);

  // A. 把数据集当作变量发送给客户端
  //    自动压缩，客户端自动解压
  Result.V['_data'] := DataSetProvider1.Data;
  Result.S['_table'] := 'tbl_xzqh';  
  FQuery.Active := False;  // FQuery 要关闭，返回待更新数据表给客户端

  // 可以继续加入明细表
//  Result.V['_detail'] := DataSetProvider2.Data;
//  Result.S['_table2'] := 'tbl_details';

  // B. 如果用 FireDAC，可以把数据集保存到 JSON，
  //    用 Attachment 返回给客户端，如：
  // FQuery.SaveToFile('e:\aaa.json', sfJSON);
  // Result.Attachment := TFileStream.Create('e:\aaa.json', fmOpenRead);
  // Result.S['attach'] := 'query.dat';  //附件名称

  // C. 用以下方法返回不带字段描述信息的 JSON 给客户端：
  // Result.DataSet := FQuery;  // 发送完毕会自动关闭 FQuery
  
end;

procedure TdmInIOCPSQLite3.InIOCPDataModuleWebSocketUpdates(Sender: TObject;
  JSON: TBaseJSON; Result: TResultJSON);
var
  ErrorCount: Integer;
begin
  FSQLite3Lock.Acquire;
  try
    try
      // _delta 是客户端传过来的变更数据
      DataSetProvider1.ApplyUpdates(JSON.V['_delta'], 0, ErrorCount);
    except
      RollbackTrans;
      Raise;    // 基类有异常处理，要 Raise
    end;
  finally
    if ErrorCount = 0 then
      CommitTrans
    else
      RollbackTrans;
    FSQLite3Lock.Release;
  end;
end;

procedure TdmInIOCPSQLite3.DeleteUser(AParams: TReceiveParams; AResult: TReturnResult);
begin
  // [USER_LOGOUT]
  // 删除 ToUser，不是 UserName
  try
    FExecSQL.SQL.Text := InSQLManager1.GetSQL('USER_DELETE');
    FExecSQL.ParamByName('USER_NAME').AsString := AParams.ToUser;

    // 独占执行
    InterExecSQL(FExecSQL);

    AResult.ActResult := arOK;
  except
    on E: Exception do  // 要另加入
    begin
      AResult.ActResult := arFail;
      AResult.Msg := E.Message;
    end;
  end;
end;

procedure TdmInIOCPSQLite3.ModifyUser(AParams: TReceiveParams; AResult: TReturnResult);
var
  i: Integer;
begin
  // [USER_MODIFY]
  // 修改用户信息
  try
    FExecSQL.SQL.Text := InSQLManager1.GetSQL('USER_MODIFY');

    for i := 0 to FExecSQL.Params.Count - 1 do
      with FExecSQL.Params[i] do
        Value := AParams.AsString[Name];

    // 独占执行
    InterExecSQL(FExecSQL);

    AResult.ActResult := arOK;  // 存在
  except
    on E: Exception do  // 要另加入
    begin
      AResult.ActResult := arFail;
      AResult.Msg := E.Message;
    end;
  end;
end;

procedure TdmInIOCPSQLite3.UserLogin(AParams: TReceiveParams; AResult: TReturnResult);
begin
  // [USER_LOGIN]
  // 登录时 AParams.UserName 是用户名
  try
    FQueryLogin.ParamByName('USER_NAME').AsString := AParams.UserName;
    FQueryLogin.ParamByName('USER_PASSWORD').AsString := AParams.Password;

    // 独占查询
    InterExecQuery(FQueryLogin);

    if FQueryLogin.Eof then
    begin
      FQueryLogin.Active := False;
      AResult.ActResult := arFail; // 失败
    end else
    begin
      AResult.Role := TClientRole(FQueryLogin.FieldByName('USER_LEVEL').AsInteger);
      FQueryLogin.Active := False;

      // 保存登录时间到数据库
      FExecSQL.SQL.Text := InSQLManager1.GetSQL('USER_LOGIN_UPDATE');
      FExecSQL.ParamByName('USER_NAME').AsString := AParams.UserName;

      InterExecSQL(FExecSQL);

      AResult.ActResult := arOK;  // 存在
    end;
  except
    on E: Exception do  // 要另加入
    begin
      AResult.ActResult := arFail;
      AResult.Msg := E.Message;
    end;
  end;
end;

procedure TdmInIOCPSQLite3.UserLogout(AParams: TReceiveParams; AResult: TReturnResult);
begin
  // [USER_LOGOUT]
  // 登出
  try
    FExecSQL.SQL.Text := InSQLManager1.GetSQL('USER_LOGOUT');
    FExecSQL.ParamByName('USER_NAME').AsString := AParams.UserName;

    // 独占执行
    InterExecSQL(FExecSQL);
  except
    on E: Exception do  // 要另加入
    begin
      AResult.ActResult := arFail;
      AResult.Msg := E.Message;
    end;
  end;
end;

procedure TdmInIOCPSQLite3.QueryUser(AParams: TReceiveParams; AResult: TReturnResult);
begin
  // [USER_QUERY_INFO]
  // 查询用户 ToUser 是否存在
  try
    FQueryUserInfo.ParamByName('USER_NAME').AsString := AParams.ToUser;

    // 独占查询
    InterExecQuery(FQueryUserInfo);

    if FQueryUserInfo.Eof then
      AResult.ActResult := arFail
    else begin
      AResult.Role := TClientRole(FQueryUserInfo.FieldByName('USER_LEVEL').AsInteger);
      AResult.ActResult := arOK;  // 存在
    end;

    FQueryUserInfo.Active := False;
  except
    on E: Exception do  // 要另加入
    begin
      AResult.ActResult := arFail;
      AResult.Msg := E.Message;
    end;
  end;
end;

procedure TdmInIOCPSQLite3.RegisterUser(AParams: TReceiveParams; AResult: TReturnResult);
var
  i: Integer;
begin
  // [USER_REGISTER]
  // 注册新用户
  try
    FExecSQL.SQL.Text := InSQLManager1.GetSQL('USER_REGISTER');

    for i := 0 to FExecSQL.Params.Count - 1 do
      with FExecSQL.Params[i] do
        Value := AParams.AsString[Name];

    InterExecSQL(FExecSQL);  // 独占执行

    AResult.ActResult := arOK;  // 执行成功 arOK
  except
    on E: Exception do  // 要另加入
    begin
      AResult.ActResult := arFail;
      AResult.Msg := E.Message;
    end;
  end;
end;

initialization
  FSQLite3Lock := TThreadLock.Create;  // SQLite3 是独占写数据库


finalization
  FSQLite3Lock.Free;

end.
