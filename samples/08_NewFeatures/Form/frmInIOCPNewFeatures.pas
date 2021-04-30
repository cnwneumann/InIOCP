unit frmInIOCPNewFeatures;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, iocp_base, iocp_clients, DB, DBClient, Grids,
  iocp_msgPacks, DBGrids;

type
  TFormInIOCPNewFeatures = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    InConnection1: TInConnection;
    InCertifyClient1: TInCertifyClient;
    InMessageClient1: TInMessageClient;
    InCustomClient1: TInCustomClient;
    InFileClient1: TInFileClient;
    InDBSQLClient1: TInDBSQLClient;
    InDBQueryClient1: TInDBQueryClient;
    InFunctionClient1: TInFunctionClient;
    btnDisconnect: TButton;
    btnLogout: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Button6: TButton;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Button8: TButton;
    Button9: TButton;
    Button5: TButton;
    EditServer: TEdit;
    Button7: TButton;
    InDBConnection1: TInDBConnection;
    procedure Button2Click(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnLogoutClick(Sender: TObject);
    procedure InCertifyClient1ListClients(Sender: TObject; Count, No: Cardinal;
              const Client: PClientInfo);
    procedure InCertifyClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure Button1Click(Sender: TObject);
    procedure InMessageClient1MsgReceive(Sender: TObject; Socket: UInt64;
      Msg: string);
    procedure InFileClient1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure FormCreate(Sender: TObject);
    procedure InDBSQLClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure Button8Click(Sender: TObject);
    procedure InFunctionClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure Button9Click(Sender: TObject);
    procedure InCertifyClient1Certify(Sender: TObject; Action: TActionType;
      ActResult: Boolean);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure InCustomClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InConnection1ReceiveMsg(Sender: TObject; Msg: TResultParams);
    procedure InConnection1Error(Sender: TObject; const Msg: string);
    procedure Button5Click(Sender: TObject);
    procedure InFileClient1ListFiles(Sender: TObject; ActResult: TActionResult;
      No: Integer; Result: TCustomPack);
    procedure InMessageClient1ListFiles(Sender: TObject;
      ActResult: TActionResult; No: Integer; Result: TCustomPack);
    procedure Button7Click(Sender: TObject);
    procedure EditServerDblClick(Sender: TObject);
    procedure InMessageClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInIOCPNewFeatures: TFormInIOCPNewFeatures;

procedure GetApplicationHandle;

implementation

uses
  TlHelp32, iocp_Varis, iocp_utils;

var
  WinCount: Integer = 0;

procedure GetApplicationHandle;
var
  h:Cardinal;
  pe:PROCESSENTRY32;
  b:Boolean;
begin
  // 取自己 INIOCPNEWFEATURES.EXE 的运行次数
  h := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);  //遍历进程
  if h = INVALID_HANDLE_VALUE then //failed
    Exit;

  ZeroMemory(@pe, SizeOf(pe));    //initial

  pe.dwSize := SizeOf(pe);        //important
  b := Process32First(h, pe);

  while b do
  begin
    if Pos('INIOCPNEWFEATURES.EXE', UpperCase(pe.szExeFile)) > 0 then
      Inc(WinCount);
    b := Process32Next(h, pe)
  end;
end;
  
{$R *.dfm}

procedure TFormInIOCPNewFeatures.btnDisconnectClick(Sender: TObject);
begin
  InConnection1.Active := False;
end;

procedure TFormInIOCPNewFeatures.btnLogoutClick(Sender: TObject);
begin
  InCertifyClient1.Logout;
end;

procedure TFormInIOCPNewFeatures.Button1Click(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // 取离线消息，宿主 InMessageClient1
  Msg := TMessagePack.Create(InMessageClient1);
  Msg.Post(atTextGetMsg);

  // 取离线消息文件列表，宿主 InMessageClient1
  Msg := TMessagePack.Create(InMessageClient1);
  Msg.Post(atTextFileList);

  // 发送消息，宿主 InMessageClient1
  Msg := TMessagePack.Create(InMessageClient1);
  Msg.Msg := '发消息到服务器';
  
  Msg.CheckType := ctMD5;  // +校验
  Msg.Post(atTextSend);

  // 推送消息（发给某人），宿主 InMessageClient1
  if (Edit1.Text = 'USER_A') then
  begin
    Msg := TMessagePack.Create(InMessageClient1);
    Msg.Msg := '推送一条消息给 ' + 'USER_B';
    Msg.ToUser := 'USER_B';  // 目的地
    Msg.Post(atTextPush);
  end;

  // 新版本支持推送消息给自己
{  Msg := TMessagePack.Create(InMessageClient1);
  Msg.Msg := '推送一条消息给自己';
  Msg.ToUser := 'user_a';  // 目的地

  Msg.CheckType := ctMurmurHash;  // +校验
  Msg.Post(atTextPush);    }

  // 广播消息（发给全部人），宿主 InMessageClient1
  Msg := TMessagePack.Create(InMessageClient1);
  Msg.Msg := '广播一条消息';
  Msg.Post(atTextBroadcast);

end;

procedure TFormInIOCPNewFeatures.Button2Click(Sender: TObject);
begin
  InConnection1.ServerAddr := EditServer.Text;
  InConnection1.Active := True;
end;

procedure TFormInIOCPNewFeatures.Button3Click(Sender: TObject);
begin
  // 常规的登录
  InCertifyClient1.UserName := Edit1.Text;
  InCertifyClient1.Password := 'pppp';
  InCertifyClient1.Login;
end;

procedure TFormInIOCPNewFeatures.Button4Click(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // 除响应服务，全部消息都可以带附件，
  // 服务端只公开文件、数据流的附件事件，
  // 在不公开事件的管理组件中，可以这样接收附件：
  // InFileManager1.CreateNewFile(Params.Socket);

  // 也可以这样:
  // if Params.AttachSize > 0 then
  //   Params.CreateAttachment(save_to_path);

  // 上传文件
  Msg := TMessagePack.Create(InFileClient1);

  // 把文件当作附件上传，不能用方法：
  //  Msg.FileName := 'DAEMON_Tools_Lite_green.rar'
  // 内部自动检测文件是否要压缩
  
  Msg.LoadFromFile('upload_me.exe');

  Msg.CheckType := ctMurmurHash;  // 可以加校验
  Msg.Post(atFileUpload);

  // 下载文件
  Msg := TMessagePack.Create(InFileClient1);
  Msg.FileName := 'upload_me.exe';
  
  Msg.CheckType := ctMD5;  // 可以加校验
  Msg.Post(atFileDownload);

  // 列出服务端当前路径下的文件
  Msg := TMessagePack.Create(InFileClient1);
  Msg.Post(atFileList);
  
end;

procedure TFormInIOCPNewFeatures.Button5Click(Sender: TObject);
begin
  // 更新远程数据库
  InDBQueryClient1.ApplyUpdates();
end;

procedure TFormInIOCPNewFeatures.Button6Click(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // 查询数据（SQL 要和服务端配合）

  // 执行服务端名称为 Select_tbl_xzqh 的 SQL 命令, 见：sql\TdmInIOCPTest.sql
  // 返回的结果在 InDBQueryClient1 中处理

  // 显示查询结果时要用 TInDBQueryClient 类型宿主

  // 测试结果：服务端给 1000 个 http 连接，繁忙时的等待时间长，
  //           在这里反复操作，可能没收到反馈，超时退出。

  Msg := TMessagePack.Create(InDBQueryClient1);
  Msg.SQLName := 'Select_tbl_xzqh'; // 区分大小写，见：TInSQLManager.GetSQL
  Msg.Post(atDBExecQuery);

  // 用 SQL 名称，执行一条命令，不带参数
  // 见：sql\TdmInIOCPTest.sql
  
  Msg := TMessagePack.Create(InDBSQLClient1);
  Msg.SQLName := 'Update_xzqh'; // 区分大小写
  Msg.HasParams := False;  // 没有参数
  Msg.Post(atDBExecSQL);

  // 执行一条 Update-SQL
  Msg := TMessagePack.Create(InDBSQLClient1);
  Msg.SQL := 'UPDATE tbl_xzqh SET code = 001 WHERE code IS NULL';
  Msg.HasParams := False;  // 没有参数
  Msg.Post(atDBExecSQL);
    
end;

procedure TFormInIOCPNewFeatures.Button7Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TFormInIOCPNewFeatures.Button8Click(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // 注：如果在广播或同时接收推送消息的情况下出现异常，
  //     很可能是客户端接收器的问题，接收代码有隐患，有没考虑的特殊情况。

  // 远程函数: 执行分组 TEST_GROUP 的第 1 个函数
  Msg := TMessagePack.Create(InFunctionClient1);
  Msg.FunctionGroup := 'TEST_GROUP';
  Msg.FunctionIndex := 1;
  Msg.Post(atCallFunction);

  // 自定义操作（上传流）
  Msg := TMessagePack.Create(InCustomClient1);

  Msg.Msg := '发送数据流....';
  Msg.AsString['中文变量'] := '测试中文....';

  // 要上传的流也是附件，服务端在 InCustomManager1 管理，
  //   系统把流当作 _stream.strm 文件，见 TBaseMessage.LoadFromStream

  Msg.LoadFromStream(TFileStream.Create('upload_me.exe', 0), True);  // 压缩上传

  Msg.Post(atCustomAction);  // 自定义操作

  // 自定义操作（上传文件）
  Msg := TMessagePack.Create(InCustomClient1);
  Msg.Msg := '开始上传文件....';
  Msg.AsString['中文变量'] := '中文....';

  Msg.LoadFromFile('upload_me.exe');
  Msg.Post(atCustomAction);  // 自定义操作

end;

procedure TFormInIOCPNewFeatures.Button9Click(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // TMessagePack 的宿主可以是 TInConnection 或其他客户端组件，
  // 返回的结果都在宿主的 OnReturnResult 中处理，因不公开
  // TInConnection 用户查询、文件查询返回事件，建议尽量使用
  // 与请求操作（Action）相应的客户端组件作宿主。

  // Msg 被提交后，会被加入发送线程，发送完毕自动释放！

  // TMessagePack 的宿主是 InCertifyClient1
  // 用 TInCertifyClient 的事件处理反馈消息

  // 登录
  Msg := TMessagePack.Create(InCertifyClient1);
  Msg.UserName := 'user_a';
  Msg.Password := 'ppp';
  Msg.Post(atUserLogin);

  // 取在线用户信息
  Msg := TMessagePack.Create(InCertifyClient1);
  Msg.Post(atUserQuery);

  // 查询某用户状态
  Msg := TMessagePack.Create(InCertifyClient1);
  Msg.ToUser := 'user_b';  // 目的用户
  Msg.Post(atUserState);

end;

procedure TFormInIOCPNewFeatures.EditServerDblClick(Sender: TObject);
begin
  EditServer.Text := '127.0.0.1';
end;

procedure TFormInIOCPNewFeatures.FormCreate(Sender: TObject);
begin
  EditServer.Text := '127.0.0.1'; // GetLocalIp;
  Edit1.Text := 'USER_' + AnsiChar(64 + WinCount);  // 用户名

  // 准备工作路径
  MyCreateDir(ExtractFilePath(Application.ExeName) + InConnection1.LocalPath);    // 建目录

  // 复制自己, 等会上传
  CopyFile(PChar('InIOCPNewFeatures.exe'), PChar('upload_me.exe'), False);

end;

procedure TFormInIOCPNewFeatures.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
begin
  // 在此判断登录、登出结果
  case Action of
    atUserLogin: begin   // 登录
      if ActResult then
        Memo1.Lines.Add(InConnection1.UserName + ': 登录成功')
      else
        Memo1.Lines.Add(InConnection1.UserName + ': 登录失败');
    end;
    atUserLogout: begin  // 登出
      if ActResult then
        Memo1.Lines.Add(InConnection1.UserName + ': 登出成功')
      else
        Memo1.Lines.Add(InConnection1.UserName + ': 登出失败');
    end;
  end;
end;

procedure TFormInIOCPNewFeatures.InCertifyClient1ListClients(Sender: TObject; Count,
  No: Cardinal; const Client: PClientInfo);
begin
  // 在此列出查询到的客户端信息
  memo1.Lines.Add(IntToStr(No) + '/' + IntToStr(Count) + ', ' +
             Client^.Name + '  ->  ' + IntToStr(Cardinal(Client^.Socket)));

end;

procedure TFormInIOCPNewFeatures.InCertifyClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 服务器返回结果：用户状态
  case Result.Action of
    atUserState:  // 查询用户状态
      if Result.ActResult = arOnline then
        Memo1.Lines.Add('在线（已登录）')
      else
        Memo1.Lines.Add('离线（未登录）');
  end;
end;

procedure TFormInIOCPNewFeatures.InConnection1Error(Sender: TObject;
  const Msg: string);
begin
  Memo1.Lines.Add('异常：' + Msg);
end;

procedure TFormInIOCPNewFeatures.InConnection1ReceiveMsg(Sender: TObject;
  Msg: TResultParams);
begin
  Memo1.Lines.Add('收到消息：' + Msg.Msg + ', 来自' + Msg.UserName);
end;

procedure TFormInIOCPNewFeatures.InCustomClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  if Result.ActResult = arOK then
    Memo1.Lines.Add('执行成功.')
  else
    Memo1.Lines.Add('执行失败.' + Result.Msg);
end;

procedure TFormInIOCPNewFeatures.InDBSQLClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  if Result.Action = atDBApplyUpdates then
  begin
    if Result.ActResult = arOK then
      Memo1.Lines.Add('远程更新成功.')
    else
      Memo1.Lines.Add('远程更新失败.' + Result.Msg);
  end else
  begin
    if Result.ActResult = arOK then
      Memo1.Lines.Add('执行成功.')
    else
      Memo1.Lines.Add('执行失败.' + Result.Msg);
  end;
end;

procedure TFormInIOCPNewFeatures.InFileClient1ListFiles(Sender: TObject;
  ActResult: TActionResult; No: Integer; Result: TCustomPack);
begin
  // 列出当前工作目录的文件
  case ActResult of
    arFail:
      Memo1.Lines.Add('目录不存在.');
    arEmpty:
      Memo1.Lines.Add('目录为空.');
    arExists:  // 列出服务端当前工作路径下的文件
      Memo1.Lines.Add(IntToStr(No) + ': ' +
                      Result.AsString['name'] + ', ' +
                      IntToStr(Result.AsInt64['size']) + ', ' +
                      DateTimeToStr(Result.AsDateTime['CreationTime']) + ', ' +
                      DateTimeToStr(Result.AsDateTime['LastWriteTime']) + ', ' +
                      Result.AsString['dir']);
  end;
end;

procedure TFormInIOCPNewFeatures.InFileClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.Action of
    atFileDownload:
      case Result.ActResult of    // 文件不存在
        arMissing:
          Memo1.Lines.Add('服务器文件不存在/丢失.');
        arOK:
          Memo1.Lines.Add('下载文件完毕.');
      end;
    atFileUpload:
      case Result.ActResult of
        arFail:
          Memo1.Lines.Add('服务端建文件失败.');
        arOK:
          Memo1.Lines.Add('上传文件完毕.');
      end;
  end;
end;

procedure TFormInIOCPNewFeatures.InFunctionClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  if Result.ActResult = arOK then
    Memo1.Lines.Add('执行函数成功.')
  else
    Memo1.Lines.Add('执行函数失败.');
end;

procedure TFormInIOCPNewFeatures.InMessageClient1ListFiles(Sender: TObject;
  ActResult: TActionResult; No: Integer; Result: TCustomPack);
begin
  // atGetMsgFiles 的返回结果
  case ActResult of
    arFail:
      Memo1.Lines.Add('目录不存在.');
    arEmpty:
      Memo1.Lines.Add('目录为空.');
    arExists:  // 列出服务端当前工作路径下的文件
      Memo1.Lines.Add(IntToStr(No) + ': ' +
                      Result.AsString['name'] + ', ' +
                      IntToStr(Result.AsInt64['size']) + ', ' +
                      DateTimeToStr(Result.AsDateTime['CreationTime']) + ', ' +
                      DateTimeToStr(Result.AsDateTime['LastWriteTime']) + ', ' +
                      Result.AsString['dir']);
  end;
end;

procedure TFormInIOCPNewFeatures.InMessageClient1MsgReceive(Sender: TObject; Socket: UInt64;
  Msg: string);
begin
  // 被动接收消息
  Memo1.Lines.Add('收到其他客户端消息：' + Msg);   // 来自 Socket

end;

procedure TFormInIOCPNewFeatures.InMessageClient1ReturnResult(Sender: TObject; Result: TResultParams);
begin
  Memo1.Lines.Add('消息服务，返回结果');
end;

end.
