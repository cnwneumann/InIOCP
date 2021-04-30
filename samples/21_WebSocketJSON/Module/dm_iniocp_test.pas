unit dm_iniocp_test;

interface

uses
  // 使用时请加单元引用 MidasLib！
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs,
  {$IFDEF VER320} Data.Win.ADODB {$ELSE} ADODB {$ENDIF},
  iocp_baseModule, iocp_base, iocp_objPools, iocp_sockets,
  iocp_sqlMgr, http_base, http_objects, iocp_WsJSON, Provider;

type

  // 数据库操作
  // 从 iocp_baseModule.TInIOCPDataModule 继承新建

  // 操作数据库的事件属性包括：
  //   OnApplyUpdates、OnExecQuery、OnExecSQL、OnExecStoredProcedure
  //   OnHttpExecQuery、OnHttpExecSQL

  TdmInIOCPTest = class(TInIOCPDataModule)
    DataSetProvider1: TDataSetProvider;
    procedure InIOCPDataModuleCreate(Sender: TObject);
    procedure InIOCPDataModuleDestroy(Sender: TObject);
    procedure InIOCPDataModuleWebSocketUpdates(Sender: TObject;
      JSON: TReceiveJSON; Result: TResultJSON);
    procedure InIOCPDataModuleWebSocketQuery(Sender: TObject;
      JSON: TReceiveJSON; Result: TResultJSON);
  private
    { Private declarations }
    FConnection: TADOConnection;
    FQuery: TADOQuery;
    FExecSQL: TADOCommand;
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

procedure TdmInIOCPTest.InIOCPDataModuleWebSocketQuery(Sender: TObject;
  JSON: TReceiveJSON; Result: TResultJSON);
begin
  // 执行 WebSocket 的操作
  FQuery.SQL.Text := 'SELECT * FROM tbl_xzqh';
  FQuery.Active := True;

  // A. 把数据集当作变量发送给客户端
  //    自动压缩，客户端自动解压
  Result.V['_data'] := DataSetProvider1.Data;
  Result.S['_table'] := 'tbl_xzqh';  // FQuery 要关闭，返回待更新数据表给客户端
  FQuery.Active := False;

  // 可以继续加入明细表
//  Result.V['_detail'] := DataSetProvider2.Data;
//  Result.S['_table2'] := 'tbl_details';

  // B. 如果用 FireDAC，可以把数据集保存到 JSON，
  //    用 Attachment 返回给客户端，如：
  // FQuery.SaveToFile('e:\aaa.json', sfJSON);
  // Result.Attachment := TFileStream.Create('e:\aaa.json', fmOpenRead);
  // Result.S['attach'] := 'query.dat';  //附件名称

  // C. 用以下方法返回不带描述信息的 JSON 给客户端：
  // Result.DataSet := FQuery;  // 发送完毕会自动关闭 FQuery

  // 后台执行，可以参考例子 14，如：

{  if (JSON.Socket.Background = False) then
   begin
     // 加入后台执行
     AddToBackground(JSON.Socket);
     Result.S['_table'] := '_background';  // 标志
     Result.Action := JSON.Action;
   end else
   begin
     // 正式执行...，结束后唤醒：
     DataSetProvider1.Data
     Result.S['_table'] := 'tbl_xzqh';  // 更新的表名
     Result.Action := JSON.Action;
     Wakeup(JSON.Socket);
   end;  } 

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
