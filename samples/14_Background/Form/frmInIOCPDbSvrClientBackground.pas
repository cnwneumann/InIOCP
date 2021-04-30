unit frmInIOCPDbSvrClientBackground;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iocp_base, iocp_clients, iocp_msgPacks, StdCtrls, DB, DBClient,
  Grids, DBGrids, ExtCtrls, iocp_wsClients;

type
  TFormInIOCPDbSvrClientBg = class(TForm)
    Memo1: TMemo;
    InConnection1: TInConnection;
    InCertifyClient1: TInCertifyClient;
    btnLogin: TButton;
    edtLoginUser: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnLogout: TButton;
    btnDBUpdate: TButton;
    btnDBQuery2: TButton;
    btnDBQuery: TButton;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    InDBConnection1: TInDBConnection;
    InDBQueryClient1: TInDBQueryClient;
    InDBSQLClient1: TInDBSQLClient;
    btnQueryDBConnections: TButton;
    btnSetDBConnection: TButton;
    DBGrid1: TDBGrid;
    ComboBox1: TComboBox;
    Image1: TImage;
    edtIP: TEdit;
    edtPort: TEdit;
    Button1: TButton;
    InWSConnection1: TInWSConnection;
    Button2: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure InCertifyClient1Certify(Sender: TObject; Action: TActionType;
      ActResult: Boolean);
    procedure btnLogoutClick(Sender: TObject);
    procedure btnQueryDBConnectionsClick(Sender: TObject);
    procedure btnSetDBConnectionClick(Sender: TObject);
    procedure InDBConnection1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure ComboBox1Change(Sender: TObject);
    procedure btnDBQueryClick(Sender: TObject);
    procedure btnDBUpdateClick(Sender: TObject);
    procedure InDBSQLClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InDBQueryClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure ClientDataSet1AfterScroll(DataSet: TDataSet);
    procedure InConnection1Error(Sender: TObject; const Msg: string);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure InConnection1ReceiveMsg(Sender: TObject; Message: TResultParams);
    procedure InConnection1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure btnDBQuery2Click(Sender: TObject);
    procedure InWSConnection1ReceiveMsg(Sender: TObject; Msg: TJSONResult);
    procedure InWSConnection1ReturnResult(Sender: TObject; Result: TJSONResult);
    procedure InWSConnection1Error(Sender: TObject; const Msg: string);
    procedure Button2Click(Sender: TObject);
    procedure InDBQueryClient1AfterLoadData(DataSet: TClientDataSet;
      const TableName: string);
  private
    { Private declarations }
    FUpdateTable: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPDbSvrClientBg: TFormInIOCPDbSvrClientBg;

implementation

uses
  MidasLib, jpeg, iocp_utils;
  
{$R *.dfm}

procedure TFormInIOCPDbSvrClientBg.Button1Click(Sender: TObject);
begin
//  InDBQueryClient1.LoadFromFile('data\background.dat');
end;

procedure TFormInIOCPDbSvrClientBg.Button2Click(Sender: TObject);
begin
  // WebSocket 协议更新远程数据表
  with InWSConnection1.JSON do
  begin
    Action := 990;

    SetRemoteTable(ClientDataSet1, FUpdateTable); // 设置要更新的数据表（服务端已经关闭）

    // 用 Variant 类型，自动压缩，服务端取 V['_delta'] 更新，自动解压
    V['_delta'] := ClientDataSet1.Delta;
    Post;
  end;
end;

procedure TFormInIOCPDbSvrClientBg.btnConnectClick(Sender: TObject);
begin
  InConnection1.ServerAddr := edtIP.Text;
  InConnection1.ServerPort := StrToInt(edtPort.Text);
  InConnection1.Active := True;

  InWSConnection1.ServerAddr := edtIP.Text;
  InWSConnection1.ServerPort := StrToInt(edtPort.Text);
  InWSConnection1.Active := True;
end;

procedure TFormInIOCPDbSvrClientBg.btnDisconnectClick(Sender: TObject);
begin
  InConnection1.Active := False;
  InWSConnection1.Active := False;
end;

procedure TFormInIOCPDbSvrClientBg.btnLoginClick(Sender: TObject);
begin
  InCertifyClient1.UserName := edtLoginUser.Text;
  InCertifyClient1.Password := 'pppp';
  InCertifyClient1.Login;

  // WebSocket 登录
  with InWSConnection1.JSON do
  begin
    Action := 1;
    S['_userName'] := 'user_aaa';
    Post;
  end;

end;

procedure TFormInIOCPDbSvrClientBg.btnLogoutClick(Sender: TObject);
begin
  InCertifyClient1.Logout;
  InWSConnection1.Active := False;  
end;

procedure TFormInIOCPDbSvrClientBg.btnQueryDBConnectionsClick(Sender: TObject);
begin
  // 查询数模数(数据库连接数，只有一个数模时默认使用第一个)
  InDBConnection1.GetConnections;
end;

procedure TFormInIOCPDbSvrClientBg.btnDBQuery2Click(Sender: TObject);
begin
  // 用 WebSocket 协议查询数据，服务端用后台执行！
  // 见：TFormInIOCPDBServerBGThread.InWebSocketManager1Receive
  with TJSONMessage.Create(InWSConnection1) do
  begin
    Action := 999;  // 服务端后台执行
    S['MSG'] := '服务端后台执行';
    Post;
  end;
end;

procedure TFormInIOCPDbSvrClientBg.btnDBQueryClick(Sender: TObject);
begin
  // 查询数据库，服务端使用后台执行，见：
  //  数模单元 TdmInIOCPTest.InIOCPDataModuleExecQuery
  //  客户端 TFormInIOCPDbSvrClientBg.InConnection1ReceiveMsg
  with InDBQueryClient1 do
  begin
    // 执行服务端名称为 Select_tbl_xzqh 的 SQL 命令
    //   当然可以直接传 SQL 命令，见：sql\TdmInIOCPTest.sql
    Params.SQLName := 'Select_tbl_xzqh';  // 区分大小写，见：TInSQLManager.GetSQL
    ExecQuery;  // 新版取消参数
  end;
end;

procedure TFormInIOCPDbSvrClientBg.btnDBUpdateClick(Sender: TObject);
begin
  // C/S 协议：更新远程数据表
  InDBQueryClient1.ApplyUpdates;
end;

procedure TFormInIOCPDbSvrClientBg.btnSetDBConnectionClick(Sender: TObject);
begin
  // 连接到指定编号的数据库连接, 只有一个数模时可以不用设置
  if ComboBox1.ItemIndex > -1 then
    InDBConnection1.Connect(ComboBox1.ItemIndex);
end;

procedure TFormInIOCPDbSvrClientBg.ClientDataSet1AfterScroll(DataSet: TDataSet);
var
  Field: TField;
  Stream: TMemoryStream;
  JpegPic: TJpegImage;
begin
  if ClientDataSet1.Active then
  begin
    Field := ClientDataSet1.FieldByName('picture');
    if Field.IsNull then
      Image1.Picture.Graphic := nil
    else begin
      Stream := TMemoryStream.Create;
      JpegPic := TJpegImage.Create;
      try
        TBlobField(Field).SaveToStream(Stream);
        Stream.Position := 0;           // 必须
        JpegPic.LoadFromStream(Stream);
        Image1.Picture.Graphic := JpegPic;
      finally
        JpegPic.Free;
        Stream.Free;
      end;
    end;
  end;
end;

procedure TFormInIOCPDbSvrClientBg.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex > -1 then
    btnSetDBConnection.Enabled := True;
end;

procedure TFormInIOCPDbSvrClientBg.FormCreate(Sender: TObject);
begin
  edtIP.Text := '127.0.0.1';    // GetLocalIP();   
  MyCreateDir(InConnection1.LocalPath); // 下载文件存放路径
end;

procedure TFormInIOCPDbSvrClientBg.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
begin
  case Action of
    atUserLogin:       // 登录
      if ActResult then
        Memo1.Lines.Add(InConnection1.UserName + '登录成功')
      else
        Memo1.Lines.Add(InConnection1.UserName + '登录失败');
    atUserLogout:      // 登出
      if ActResult then
        Memo1.Lines.Add(InConnection1.UserName + '登出成功')
      else
        Memo1.Lines.Add(InConnection1.UserName + '登出失败');
  end;
end;

procedure TFormInIOCPDbSvrClientBg.InConnection1Error(Sender: TObject; const Msg: string);
begin
  Memo1.Lines.Add(Msg);
end;

procedure TFormInIOCPDbSvrClientBg.InConnection1ReceiveMsg(Sender: TObject;
  Message: TResultParams);
begin
  // 服务端使用后台处理数据查询，完成后唤醒客户端，见：
  //   数模单元 TdmInIOCPTest.InIOCPDataModuleExecQuery
  if (Message.AsString['data_file'] <> '') then  // 这是数据文件
    with TMessagePack.Create(InConnection1) do // 宿主，事件返回在 InConnection1 
    begin
      LocalPath := 'temp';   // 存放到临时路径
      FileName := Message.AsString['data_file'];  // 文件名
      Post(atFileDownload);  // 下载
    end;
end;

procedure TFormInIOCPDbSvrClientBg.InConnection1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 见：TFormInIOCPDBServerBGThread.InFileManager1BeforeDownload
  if Result.Action = atFileDownload then  // 成功下载查询结果，显示
    if Result.ActResult = arOK then
      InDBQueryClient1.LoadFromFile('temp\' + Result.FileName);  // 装载
end;

procedure TFormInIOCPDbSvrClientBg.InDBConnection1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.Action of
    atDBGetConns:        // 查询连接
      case Result.ActResult of
        arExists: begin  // 有数据库连接
          ComboBox1.Items.DelimitedText := Result.AsString['dmCount'];
          Memo1.Lines.Add('数据连接数 = ' + IntToStr(ComboBox1.Items.Count));
        end;
        arMissing:      // 没有
          { empty } ;
      end;
    atDBConnect:        // 设置连接
      case Result.ActResult of
        arOK:
          Memo1.Lines.Add('设置数据连接成功.');
        arFail:
          Memo1.Lines.Add('设置数据连接失败！');        
      end;
  end;

end;

procedure TFormInIOCPDbSvrClientBg.InDBQueryClient1AfterLoadData(
  DataSet: TClientDataSet; const TableName: string);
begin
  // 数据已经装载到 DataSet，数据集合对应的表名称为 TableName
  // 此事件在 OnReturnResult 之前执行
end;

procedure TFormInIOCPDbSvrClientBg.InDBQueryClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 查询、更新返回结果
  case Result.Action of
    atDBExecQuery,
    atDBExecStoredProc:
      if Result.ActResult = arOK then
      begin
        ClientDataSet1AfterScroll(nil);
        Memo1.Lines.Add('查询/执行成功。');
      end else
        Memo1.Lines.Add('查询/执行失败:' + Result.Msg);
    atDBApplyUpdates:
      if Result.ActResult = arOK then
        Memo1.Lines.Add('远程更新成功.')
      else
        Memo1.Lines.Add('远程更新失败:' + Result.Msg);
  end;
end;

procedure TFormInIOCPDbSvrClientBg.InDBSQLClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 执行 SQL 返回结果
  case Result.Action of
    atDBExecSQL:
      case Result.ActResult of
        arOK:
          Memo1.Lines.Add('远程更新成功.');
        arFail:
          Memo1.Lines.Add('远程更新失败:' + Result.Msg);
      end;
    atDBExecStoredProc:
      case Result.ActResult of
        arOK:
          Memo1.Lines.Add('执行存储过程成功.');
        arFail:
          Memo1.Lines.Add('执行存储过程失败:' + Result.Msg);
      end;
  end;
end;

procedure TFormInIOCPDbSvrClientBg.InWSConnection1Error(Sender: TObject;
  const Msg: string);
begin
  Memo1.Lines.Add(Msg);
end;

procedure TFormInIOCPDbSvrClientBg.InWSConnection1ReceiveMsg(Sender: TObject;
  Msg: TJSONResult);
begin
  // 收到唤醒消息，见：TdmInIOCPTest.InIOCPDataModuleWebSocketQuery
  if Msg.MsgType = mtJSON then
    if (Msg.Action = 999) and (Msg.S['_dataFile'] <> '') then  // 下载文件！
      with InWSConnection1.JSON do
      begin
        Action := 999;
        S['_tableName'] := Msg.S['_tableName'];  // 数据表名称
        S['_dataFile'] := Msg.S['_dataFile'];  // 数据文件名称
        Post;
      end;
end;

procedure TFormInIOCPDbSvrClientBg.InWSConnection1ReturnResult(Sender: TObject;
  Result: TJSONResult);
begin
  if (Result.Action = 990) then  // 更新完毕
  begin
    ClientDataSet1.MergeChangeLog;  // 合并数据
    Memo1.Lines.Add('WebSocket更新：' + Result.S['result']);
  end else
  if (Result.Action = 999) then
    case Result.MsgType of
      mtJSON:
        if (Result.S['_tableName'] = 'tbl_xzqh') then  // 下载查询结果！
        begin
          if Result.HasAttachment then  // 有附件流，接收
            Result.Attachment := TFileStream.Create('temp\_query_down.dat', fmCreate);
        end;
      mtAttachment:
        if Assigned(Result.Attachment) then
        begin
          FUpdateTable := Result.S['_tableName'];
          ClientDataSet1.LoadFromStream(Result.Attachment); // 数据流未释放（内部自动释放）
          Memo1.Lines.Add('下载装载后台执行的查询结果 ' + Result.S['_dataFile']);
        end;
    end;
end;

end.
