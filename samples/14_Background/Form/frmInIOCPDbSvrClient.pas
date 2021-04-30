unit frmInIOCPDbSvrClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iocp_base, iocp_clients, iocp_msgPacks, StdCtrls, DB, DBClient,
  Grids, DBGrids, ExtCtrls;

type
  TFormInIOCPDbSvrClient = class(TForm)
    Memo1: TMemo;
    InConnection1: TInConnection;
    InCertifyClient1: TInCertifyClient;
    btnLogin: TButton;
    edtLoginUser: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnLogout: TButton;
    btnDBUpdate: TButton;
    btnDBUpdate2: TButton;
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
    btnStoredProc: TButton;
    btnStoredProc2: TButton;
    edtIP: TEdit;
    edtPort: TEdit;
    Button1: TButton;
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
    procedure btnDBUpdate2Click(Sender: TObject);
    procedure InDBSQLClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InDBQueryClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure ClientDataSet1AfterScroll(DataSet: TDataSet);
    procedure InConnection1Error(Sender: TObject; const Msg: string);
    procedure btnStoredProcClick(Sender: TObject);
    procedure btnStoredProc2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInIOCPDbSvrClient: TFormInIOCPDbSvrClient;

implementation

uses
  MidasLib, jpeg, iocp_utils;
  
{$R *.dfm}

procedure TFormInIOCPDbSvrClient.btnStoredProcClick(Sender: TObject);
begin
  // 执行存储过程
  // 用 TInDBSQLClient.ExecStoredProc 不处理返回的数据集
  // 用 TInDBQueryClient.ExecStoredProc 则显示返回的数据集

  // 这是例子，服务端并无实际操作
  //  见：TdmInIOCPTest.InIOCPDataModuleExecStoredProcedure

  with InDBSQLClient1 do
  begin
    Params.AsString['param'] := '文本内容';
    Params.AsInteger['param2'] := 999;
    ExecStoredProc('ExecuteStoredProc');
  end;

end;

procedure TFormInIOCPDbSvrClient.Button1Click(Sender: TObject);
var
  inQry: TInDBQueryClient;
begin
  inQry := TInDBQueryClient.Create(Self);
  try
    inQry.DBConnection := InDBConnection1;
    InDBConnection1.ConnectionIndex := 1; // 可以不执行 InDBConnection1.Connect(1);
    inQry.Params.SQL := 'SELECT * FROM tbl_xzqh';
    inQry.ExecQuery;  // 新版不带参数
  finally
    inQry.Free;
  end;
end;

procedure TFormInIOCPDbSvrClient.btnStoredProc2Click(Sender: TObject);
begin
  // 执行存储过程 2
  // 用 TInDBQueryClient.ExecStoredProc 查询，返回一个数据集
  // 此时的数据是只读的，不能改！

  // 这是例子，服务端并无执行 ExecuteStoredProc2，
  // 只是用 SQL 查询结果返回，
  // 见：TdmInIOCPTest.InIOCPDataModuleExecStoredProcedure，sql\TdmInIOCPTest.sql
  with InDBQueryClient1 do
  begin
    Params.SQLName := 'Stored_select';  // 执行这个 SQL 名称
    ExecStoredProc('ExecuteStoredProc2');
  end;
end;

procedure TFormInIOCPDbSvrClient.btnConnectClick(Sender: TObject);
begin
  InConnection1.ServerAddr := edtIP.Text;
  InConnection1.ServerPort := StrToInt(edtPort.Text);  
  InConnection1.Active := True;
end;

procedure TFormInIOCPDbSvrClient.btnDisconnectClick(Sender: TObject);
begin
  InConnection1.Active := False;
end;

procedure TFormInIOCPDbSvrClient.btnLoginClick(Sender: TObject);
begin
  InCertifyClient1.UserName := edtLoginUser.Text;
  InCertifyClient1.Password := 'pppp';
  InCertifyClient1.Login;
end;

procedure TFormInIOCPDbSvrClient.btnLogoutClick(Sender: TObject);
begin
  InCertifyClient1.Logout;
end;

procedure TFormInIOCPDbSvrClient.btnQueryDBConnectionsClick(Sender: TObject);
begin
  // 查询数模数(数据库连接数，只有一个数模时默认使用第一个)
  InDBConnection1.GetConnections;
end;

procedure TFormInIOCPDbSvrClient.btnDBQueryClick(Sender: TObject);
begin
  // 查询数据，结果输出到 ClientDataSet1
  //   客户端的 SQL 要和服务端的配合，2.0 增加 SQL、SQLName 属性

  // 查询结果有多个数据集时，要先加入子 TClientDataSet
  // InDBQueryClient1.AddClientDataSets(???);
    
  with InDBQueryClient1 do
  begin
    // 执行服务端名称为 Select_tbl_xzqh 的 SQL 命令
    //   当然可以直接传 SQL 命令，见：sql\TdmInIOCPTest.sql
    Params.SQLName := 'Select_tbl_xzqh';  // 区分大小写，见：TInSQLManager.GetSQL
    ExecQuery;  // 新版取消参数
  end;
end;

procedure TFormInIOCPDbSvrClient.btnDBUpdate2Click(Sender: TObject);
begin
  // 执行 SQL 命令
  // 更新当前记录的 picture 图片
  if ClientDataSet1.Active then
    with InDBSQLClient1 do
    begin
      // 直接用 SQL 更新, 见：TdmInIOCPTest.InIOCPDataModuleExecSQL
      Params.SQL := 'UPDATE tbl_xzqh SET picture = :picutre WHERE code = :code';
      Params.AsStream['picture'] := TFileStream.Create('pic\test.jpg', fmOpenRead);
      Params.AsString['code'] := ClientDataSet1.FieldByName('code').AsString;
      Params.HasParams := True;  // 带参数！
      ExecSQL;
    end;
end;

procedure TFormInIOCPDbSvrClient.btnDBUpdateClick(Sender: TObject);
begin
  // 更新必须设置属性：InDBQueryClient1.TableName
  InDBQueryClient1.ApplyUpdates;     // 带参数可以临时更新其他数据表
end;

procedure TFormInIOCPDbSvrClient.btnSetDBConnectionClick(Sender: TObject);
begin
  // 连接到指定编号的数据库连接, 只有一个数模时可以不用设置
  if ComboBox1.ItemIndex > -1 then
    InDBConnection1.Connect(ComboBox1.ItemIndex);
end;

procedure TFormInIOCPDbSvrClient.ClientDataSet1AfterScroll(DataSet: TDataSet);
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

procedure TFormInIOCPDbSvrClient.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex > -1 then
    btnSetDBConnection.Enabled := True;
end;

procedure TFormInIOCPDbSvrClient.FormCreate(Sender: TObject);
begin
  edtIP.Text := '127.0.0.1';    // GetLocalIP();   
  iocp_utils.IniDateTimeFormat; // 设置日期时间格式
  MyCreateDir(InConnection1.LocalPath); // 下载文件存放路径
end;

procedure TFormInIOCPDbSvrClient.InCertifyClient1Certify(Sender: TObject;
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

procedure TFormInIOCPDbSvrClient.InConnection1Error(Sender: TObject; const Msg: string);
begin
  Memo1.Lines.Add(Msg);
end;

procedure TFormInIOCPDbSvrClient.InDBConnection1ReturnResult(Sender: TObject;
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

procedure TFormInIOCPDbSvrClient.InDBQueryClient1ReturnResult(Sender: TObject;
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

procedure TFormInIOCPDbSvrClient.InDBSQLClient1ReturnResult(Sender: TObject;
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

end.
