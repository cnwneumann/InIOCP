unit dm_iniocp_test;

interface

uses
  // 使用时请加单元引用 MidasLib！
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DB, DBClient, Provider,
  // ADODB 的高版本是 Data.Win.ADODB
  {$IF CompilerVersion >= 32} Data.Win.ADODB, {$ELSE} ADODB, {$IFEND}
  iocp_baseModule, iocp_base, iocp_objPools, iocp_sockets,
  iocp_sqlMgr, http_base, http_objects, iocp_WsJSON, MidasLib;

type

  // 数据库操作
  // 从 iocp_baseModule.TInIOCPDataModule 继承新建

  // 操作数据库的事件属性包括：
  //   OnApplyUpdates、OnExecQuery、OnExecSQL、OnExecStoredProcedure
  //   OnHttpExecQuery、OnHttpExecSQL

  TdmInIOCPTest = class(TInIOCPDataModule)
    DataSetProvider1: TDataSetProvider;
    InSQLManager1: TInSQLManager;
    procedure InIOCPDataModuleCreate(Sender: TObject);
    procedure InIOCPDataModuleDestroy(Sender: TObject);
    procedure InIOCPDataModuleApplyUpdates(AParams: TReceiveParams;
      AResult: TReturnResult; out ErrorCount: Integer);
    procedure InIOCPDataModuleExecQuery(AParams: TReceiveParams;
      AResult: TReturnResult);
    procedure InIOCPDataModuleExecSQL(AParams: TReceiveParams;
      AResult: TReturnResult);
    procedure InIOCPDataModuleExecStoredProcedure(AParams: TReceiveParams;
      AResult: TReturnResult);
    procedure InIOCPDataModuleWebSocketQuery(Sender: TObject;
      JSON: TReceiveJSON; Result: TResultJSON);
    procedure InIOCPDataModuleWebSocketUpdates(Sender: TObject;
      JSON: TReceiveJSON; Result: TResultJSON);
  private
    { Private declarations }
    FConnection: TADOConnection;
    FQuery: TADOQuery;
    FExecSQL: TADOCommand;
    FCurrentSQLName: String;
    procedure CommitTransaction;
  public
    { Public declarations }
  end;

{ var
    dmInIOCPTest: TdmInIOCPTest; // 注解, 注册到系统后自动建实例 }

implementation

uses
  iocp_Varis, iocp_utils;

{$R *.dfm}

procedure TdmInIOCPTest.CommitTransaction;
begin
//  GlobalLock.Acquire;   // Ado 无需加锁
//  try
    if FConnection.InTransaction then
      FConnection.CommitTrans;
    if not FConnection.InTransaction then
      FConnection.BeginTrans;
{  finally
    GlobalLock.Release;
  end;  }
end;

procedure TdmInIOCPTest.InIOCPDataModuleApplyUpdates(AParams: TReceiveParams;
  AResult: TReturnResult; out ErrorCount: Integer);
begin
  // 用 DataSetPrivoder.Delta 更新

  if not FConnection.InTransaction then
    FConnection.BeginTrans;

  try
    try
      // 新版改变：
      //   1. 第一个字段为用户名称 _UserName，
      //   2. 以后字段为 Delta 数据，可能有多个，本例子只有一个。

      // 参考：TBaseMessage.LoadFromVariant
      //  Params.Fields[0]：用户名 _UserName
      //  Params.Fields[1].Name：字段名称，对应数据表名称
      //  Params.Fields[1].AsVariant：Delta 数据

      // 执行父类的更新方法
      // 用一组 TDataSetProvider 更新，本例子只有一个
      InterApplyUpdates([DataSetProvider1], AParams, ErrorCount);
      
    finally
      if ErrorCount = 0 then
      begin
        CommitTransaction;
        AResult.ActResult := arOK;
      end else
      begin
        if FConnection.InTransaction then
          FConnection.RollbackTrans;
        AResult.ActResult := arFail;
        AResult.AsInteger['ErrorCount'] := ErrorCount;
      end;
    end;
  except
    if FConnection.InTransaction then
      FConnection.RollbackTrans;
    Raise;    // 基类有异常处理，要 Raise
  end;
end;

procedure TdmInIOCPTest.InIOCPDataModuleCreate(Sender: TObject);
begin
  inherited;

  // 用 InSQLManager1.SQLs 装入 SQL 资源文件（文本文件）
  InSQLManager1.SQLs.LoadFromFile('sql\' + ClassName + '.sql');

  // 为方便编译，新版本改用 ADO 连接 access 数据库（内含行政区划数据表）
  FConnection := TADOConnection.Create(Self);
  FConnection.LoginPrompt := False;

  // 注册 Access-ODBC、设置 ODBC 连接
  RegMSAccessDSN('acc_db', iocp_varis.gAppPath + '..\00_data\acc_db.mdb', 'InIOCP测试');
    
  SetMSAccessDSN(FConnection, 'acc_db');
  
  FQuery := TADOQuery.Create(Self);
  FExecSQL := TADOCommand.Create(Self);

  FQuery.Connection := FConnection;
  FExecSQL.Connection := FConnection;

  // 自动解析 SQL 参数
  FQuery.ParamCheck := True;
  FExecSQL.ParamCheck := True;

  DataSetProvider1.DataSet := FQuery;
  FConnection.Connected := True;
end;

procedure TdmInIOCPTest.InIOCPDataModuleDestroy(Sender: TObject);
begin
  inherited;
  FQuery.Free;
  FExecSQL.Free;
  FConnection.Free;
end;

procedure TdmInIOCPTest.InIOCPDataModuleExecQuery(AParams: TReceiveParams;
  AResult: TReturnResult);
var
  SQLName: String;
begin
  // 查询数据
  // 基类有异常处理

  // 用后台执行查询，结束后推送消息给客户端！
  
  if (AParams.Socket.Background = False) then
  begin
    AResult.ActResult := arOK;
    AddToBackGround(AParams.Socket);  // 加入后台执行
    Exit;
  end;

  if not FConnection.InTransaction then
    FConnection.BeginTrans;

  // 2.0 预设了 SQL、SQLName 属性
  //     查找服务端名称为 SQLName 的 SQL 语句，执行
  //     要和客户端的命令配合

  SQLName := AParams.SQLName;
  if (SQLName = '') then  // 改用 SQL（未必就是 SELECT-SQL）
    with FQuery do
    begin
      SQL.Clear;
      SQL.Add(AParams.SQL);
      Active := True;
    end
  else
  if (SQLName <> FCurrentSQLName) then
  begin
    FCurrentSQLName := SQLName;
    with FQuery do
    begin
      SQL.Clear;
      SQL.Add(InSQLManager1.GetSQL(SQLName));
      Active := True;
    end;
  end;

  // LoadFromVariant 改进：
  //  参数：[数据集a, 数据集b, 数据集c], ['数据表a', '数据表b', '数据表c'])
  //  1. 数据表n 是 数据集n 对应的数据表名称
  //  2. 不更新数据表时第 2 参数可设为 [] 或表名称为空
  //  3. 如果有多个数据集，第一个为主表

  // 先装入数据、再保存，清空 AResult：

  AResult.LoadFromVariant([DataSetProvider1], ['tbl_xzqh']);  // [] 或 [''] 则为只读
  AResult.SaveToFile('data\background.dat');  // 不要有写文件冲突（自动清除Clear）

  // 也可以用 VariantToStream(DataSetProvider1.Data, TFileStream) 保存到文件，
  // 客户端下载后用 TClientDataSet.LoadFromFile() 显示

  FQuery.Active := False;   // 关闭

  // 注意，要推送消息给客户端，客户端下载 AsString['data_file']，
  //   下载时进入文件服务管理器！
  AResult.AsString['data_file'] := 'background.dat';
  AResult.ActResult := arOK;

  Wakeup(AResult.Socket);

end;

procedure TdmInIOCPTest.InIOCPDataModuleExecSQL(AParams: TReceiveParams;
  AResult: TReturnResult);
var
  SQLName: string;
begin
  // 执行 SQL
  // 基类有异常处理
  if not FConnection.InTransaction then
    FConnection.BeginTrans;

  try

    // 取 SQL
    SQLName := AParams.SQLName;
    if (SQLName = '') then  // 用 SQL
      FExecSQL.CommandText := AParams.SQL
    else
    if (SQLName <> FCurrentSQLName) then  // 用名称
    begin
      FCurrentSQLName := SQLName;
      FExecSQL.CommandText := InSQLManager1.GetSQL(SQLName);
    end;

    if not AParams.HasParams then  // 客户端设定“没有参数”
    begin
      FExecSQL.Execute;  // 直接执行
    end else
      with FExecSQL do
      begin  // 参数赋值
        Parameters.ParamByName('picutre').LoadFromStream(AParams.AsStream['picture'], ftBlob);
        Parameters.ParamByName('code').Value := AParams.AsString['code'];
        Execute;
      end;

    CommitTransaction;
    AResult.ActResult := arOK;  // 执行成功 arOK
  except
    if FConnection.InTransaction then
      FConnection.RollbackTrans;
    Raise;    // 基类有异常处理，要 Raise
  end;
end;

procedure TdmInIOCPTest.InIOCPDataModuleExecStoredProcedure(
  AParams: TReceiveParams; AResult: TReturnResult);
begin
  // 执行存储过程
  try
    // 这是存储过程名称：
    // ProcedureName := AParams.StoredProcName;
    // 见：TInDBQueryClient.ExecStoredProc
    //     TInDBSQLClient.ExecStoredProc

    // 这样返回数据集：
    // AResult.LoadFromVariant(DataSetProvider1.Data);

    if AParams.StoredProcName = 'ExecuteStoredProc2' then  // 测试存储过程（数据未实现）
      InIOCPDataModuleExecQuery(AParams, AResult)     // 返回一个数据集
    else
      AResult.ActResult := arOK;
  except
    if FConnection.InTransaction then
      FConnection.RollbackTrans;
    Raise;    // 基类有异常处理，要 Raise
  end;
end;

procedure TdmInIOCPTest.InIOCPDataModuleWebSocketQuery(Sender: TObject;
  JSON: TReceiveJSON; Result: TResultJSON);
var
  Stream: TStream;
begin
  // 用后台执行
  if (JSON.Socket.Background = False) then
   begin
     // 加入后台执行
     AddToBackground(JSON.Socket);  // 在前

     JSON.Socket.SendResult;  // 要显式发送结果（C/S模式不用）
   end else
   begin
     // 正式执行...，结束后唤醒：
     try
       FQuery.SQL.Clear;
       FQuery.SQL.Add('Select * from tbl_xzqh');
       FQuery.Active := True;

       Stream := VariantToStream(DataSetProvider1.Data, False, 'temp\_query_result.dat'); // 不压缩（客户端直接读入）
       Stream.Free;
     finally
       FQuery.Active := False;
     end;

     Result.S['_tableName'] := 'tbl_xzqh';  // 数据表名称
     Result.S['_dataFile'] := '_query_result.dat';  // 推送文件名称给客户端

     Wakeup(JSON.Socket);  // 推送 Result 给 JSON.Socket
   end;
end;

procedure TdmInIOCPTest.InIOCPDataModuleWebSocketUpdates(Sender: TObject;
  JSON: TReceiveJSON; Result: TResultJSON);
var
  ErrorCount: Integer;
begin
  if not FConnection.InTransaction then
    FConnection.BeginTrans;
  try
    try
      // _delta 是客户端传过来的变更数据
      DataSetProvider1.ApplyUpdates(JSON.V['_delta'], 0, ErrorCount);
    finally
      if ErrorCount = 0 then
      begin
        CommitTransaction;
        Result.S['result'] := '更新成功.';
      end else
      begin
        if FConnection.InTransaction then
          FConnection.RollbackTrans;
        Result.S['result'] := '更新失败.';
      end;
    end;
  except
    if FConnection.InTransaction then
      FConnection.RollbackTrans;
    Raise;    // 基类有异常处理，要 Raise
  end;
end;

end.
