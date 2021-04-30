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
    procedure InIOCPDataModuleHttpExecQuery(Sender: TObject;
      Request: THttpRequest; Response: THttpResponse);
    procedure InIOCPDataModuleHttpExecSQL(Sender: TObject;
      Request: THttpRequest; Response: THttpResponse);
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

procedure TdmInIOCPTest.InIOCPDataModuleCreate(Sender: TObject);
begin
  inherited;

  // 用 InSQLManager1.SQLs 装入 SQL 资源文件（文本文件）
  InSQLManager1.SQLs.LoadFromFile('sql\' + ClassName + '.sql');

  // 为方便编译，新版本改用 ADO 连接 access 数据库（内含行政区划数据表）
  FConnection := TADOConnection.Create(Self);
  FConnection.LoginPrompt := False;

  // 注册 Access-ODBC、设置 ODBC 连接
  if DirectoryExists('data') then
    RegMSAccessDSN('acc_db', iocp_varis.gAppPath + 'data\acc_db.mdb', 'InIOCP测试')
  else  // 发布为例子时
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

procedure TdmInIOCPTest.InIOCPDataModuleHttpExecQuery(Sender: TObject;
  Request: THttpRequest; Response: THttpResponse);
var
  i: Integer;
  SQLName: String;
begin
  // Http 服务：在这里执行 SQL 查询，用 Respone 返回结果
  with FQuery do
  try
    try

      // 用 SQL 名称查找对应的 SQL 文本
      // Http 的 Request.Params 没有预设 sql, sqlName 属性
      SQLName := Request.Params.AsString['SQL'];

      if (FCurrentSQLName <> SQLName) then   // 名称改变，重设 SQL
      begin
        SQL.Clear;
        SQL.Add(InSQLManager1.GetSQL(SQLName));
        FCurrentSQLName := SQLName;
      end;

      // 可以用 Request.SocketState 或 Respone.SocketState
      // 检查连接状态是否正常, 不正常无需再查询发送：
      // if Request.SocketState then

      // 通用一点的赋值方法：
      // Select xxx from ttt where code=:code and no=:no and datetime=:datetime

      if (FCurrentSQLName = 'Select_tbl_xzqh2') then
      begin
        // 用 ab.exe 大并发测试
        Parameters.Items[0].Value := 110102;  // 带参数 code
        Active := True;
        // ab.exe 不支持分块传输，不能用 Respone.SendJSON(FQuery)
        SQLName := iocp_utils.DataSetToJSON(FQuery);
        Response.SendJSON(SQLName);
      end else
      begin
        // 参数赋值
        for i := 0 to Parameters.Count - 1 do
          Parameters.Items[i].Value := Request.Params.AsString[Parameters.Items[i].Name];

        Active := True;

        // 转换全部记录为 JSON，用 Respone 返回
        // 1. 小数据集发送：
        //     Response.SendJSON(iocp_utils.DataSetToJSON(FQuery))
        // 2. 数据量大时推荐用 Response.SendJSON(FQuery) 分块发送!
        // 见：iocp_utils 单元 DataSetToJSON、LargeDataSetToJSON、InterDataSetToJSON

        Response.SendJSON(FQuery);  // 用默认字符集 gb2312
//        Response.SendJSON(FQuery, hcsUTF8);  // 转为 UTF-8 字符集
      end;

    finally
      Active := False;
    end;
  except
    Raise;
  end;
end;

procedure TdmInIOCPTest.InIOCPDataModuleHttpExecSQL(Sender: TObject;
  Request: THttpRequest; Response: THttpResponse);
begin
  // Http 服务：在这里执行 SQL 命令，用 Respone 返回结果
end;

end.
