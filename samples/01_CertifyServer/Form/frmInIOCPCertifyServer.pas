unit frmInIOCPCertifyServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_base, iocp_clients, iocp_server,
  iocp_managers, iocp_sockets, ExtCtrls;

type
  TFormInIOCPCertifyServer = class(TForm)
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    InConnection1: TInConnection;
    btnStart: TButton;
    btnStop: TButton;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnLogin: TButton;
    InCertifyClient1: TInCertifyClient;
    btnLogout: TButton;
    InClientManager1: TInClientManager;
    InConnection2: TInConnection;
    InCertifyClient2: TInCertifyClient;
    btnConnect2: TButton;
    btnDisconnect2: TButton;
    btnLogin2: TButton;
    btnLogout2: TButton;
    btnQueryClient: TButton;
    btnCheckState: TButton;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure InConnection1Error(Sender: TObject; const Msg: string);
    procedure btnLoginClick(Sender: TObject);
    procedure InCertifyClient1Certify(Sender: TObject; Action: TActionType;
      ActResult: Boolean);
    procedure btnLogoutClick(Sender: TObject);
    procedure InClientManager1Login(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure FormCreate(Sender: TObject);
    procedure InClientManager1Delete(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1Logout(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1Modify(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1Register(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InCertifyClient1ListClients(Sender: TObject; Count, No: Cardinal;
                                          const Client: PClientInfo);
    procedure btnConnect2Click(Sender: TObject);
    procedure btnDisconnect2Click(Sender: TObject);
    procedure btnLogin2Click(Sender: TObject);
    procedure btnLogout2Click(Sender: TObject);
    procedure btnQueryClientClick(Sender: TObject);
    procedure InCertifyClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure btnCheckStateClick(Sender: TObject);
    procedure InClientManager1QueryState(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InConnection1ReceiveMsg(Sender: TObject; Msg: TResultParams);
    procedure InConnection1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure InConnection1AfterConnect(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPCertifyServer: TFormInIOCPCertifyServer;

implementation

uses
  iocp_log, iocp_varis, iocp_utils;

{$R *.dfm}

procedure TFormInIOCPCertifyServer.btnCheckStateClick(Sender: TObject);
begin
  // 查询用户 USER_B 的状态
  InCertifyClient1.GetUserState('USER_B');
end;

procedure TFormInIOCPCertifyServer.btnConnect2Click(Sender: TObject);
begin
  InConnection2.Active := True;
end;

procedure TFormInIOCPCertifyServer.btnConnectClick(Sender: TObject);
begin
  InConnection1.Active := True;
end;

procedure TFormInIOCPCertifyServer.btnDisconnect2Click(Sender: TObject);
begin
  InConnection2.Active := False;
end;

procedure TFormInIOCPCertifyServer.btnDisconnectClick(Sender: TObject);
begin
  InConnection1.Active := False;
end;

procedure TFormInIOCPCertifyServer.btnLogin2Click(Sender: TObject);
begin
  InCertifyClient2.Group := LabeledEdit2.Text;
  InCertifyClient2.Login;
end;

procedure TFormInIOCPCertifyServer.btnLoginClick(Sender: TObject);
begin
  // 注意：一个 InConnection 对应一个客户端！
  // 用户：USER_TEST、PASS-AAA
  InCertifyClient1.Group := LabeledEdit1.Text;
  InCertifyClient1.Login; // 登录，在 InCertifyClient1Certify 处返回结果

{  with TMessagePack.Create(InConnection1) do
  begin
    UserName := 'AAA';
    Password := 'aaa';
    Post(atUserLogin);
  end;     }
end;

procedure TFormInIOCPCertifyServer.btnLogout2Click(Sender: TObject);
begin
  InCertifyClient2.Logout;
end;

procedure TFormInIOCPCertifyServer.btnLogoutClick(Sender: TObject);
begin
  InCertifyClient1.Logout;      // 退出
end;

procedure TFormInIOCPCertifyServer.btnQueryClientClick(Sender: TObject);
begin
  // 查询全部连接的客户端（可能未登录）
  //   在 InCertifyClient1ListClients 处返回结果，
  //   见：TInClientManager.GetClients
  InCertifyClient1.QueryClients;
end;

procedure TFormInIOCPCertifyServer.btnStartClick(Sender: TObject);
begin
  // 压力测试时不要设 InIOCPServer1.PreventAttack（防攻击） 为 True
  Memo1.Lines.Clear;
  iocp_log.TLogThread.InitLog;                // 开启日志
  InIOCPServer1.Active := True;               // 开启服务
  FrameIOCPSvrInfo1.Start(InIOCPServer1);     // 开始统计
end;

procedure TFormInIOCPCertifyServer.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False;   // 停止服务
  FrameIOCPSvrInfo1.Stop;          // 停止统计
  iocp_log.TLogThread.StopLog;     // 停止日志
end;

procedure TFormInIOCPCertifyServer.FormCreate(Sender: TObject);
begin
  // 准备工作路径
  FAppDir := ExtractFilePath(Application.ExeName);

  // 客户端数据存放路径（2.0改名称）
  iocp_Varis.gUserDataPath := FAppDir + 'client_data\';

  MyCreateDir(FAppDir + 'log');    // 建目录
  MyCreateDir(FAppDir + 'temp');   // 建目录

  // 建测试的用户路径
  MyCreateDir(iocp_Varis.gUserDataPath);  // 建目录

  MyCreateDir(iocp_Varis.gUserDataPath + 'user_a');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_a\data');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_a\msg');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_a\temp');

  MyCreateDir(iocp_Varis.gUserDataPath + 'user_b');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_b\data');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_b\msg');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_b\temp');

end;

procedure TFormInIOCPCertifyServer.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
begin
  // 在此判断登录、登出结果
  // Sender: TInCertifyClient，见：TInCertifyClient.HandleMsgHead
  case Action of
    atUserLogin: begin      // 登录
      if ActResult then
        Memo1.Lines.Add(TInCertifyClient(Sender).UserName + ': 登录成功，客户端')
      else
        Memo1.Lines.Add(TInCertifyClient(Sender).UserName + ': 登录失败，客户端');
    end;
    atUserLogout: begin     // 登出
      if ActResult then
        Memo1.Lines.Add(TInCertifyClient(Sender).UserName + ': 登出成功，客户端')
      else
        Memo1.Lines.Add(TInCertifyClient(Sender).UserName + ': 登出失败，客户端');
    end;
  end;
end;

procedure TFormInIOCPCertifyServer.InCertifyClient1ListClients(Sender: TObject; Count,
  No: Cardinal; const Client: PClientInfo);
begin
  // 在此列出查询到的客户端信息
  //  Sender：InCertifyClient
  //   Count：总数
  //      No: 当前编号
  //  Client：客户端信息 PClientInfo
  //  见：iocp_base.TClientInfo，TInFileClient.HandleFeedback，
  //      TInBaseClient.ListReturnFiles
  memo1.Lines.Add(IntToStr(No) + '/' + IntToStr(Count) + ', ' + Client^.Group + ', ' +
             Client^.Name + '  ->  ' + IntToStr(Cardinal(Client^.Socket)) { 服务端的 Socket } + ', ' +
             Client^.PeerIPPort + ', ' + DateTimeToStr(Client^.LoginTime) + ', ' +
             DateTimeToStr(Client^.LogoutTime));
end;

procedure TFormInIOCPCertifyServer.InCertifyClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 服务器返回结果
  // Result 是服务端返回的结果，
  // Result.Action 是客户端发出的请求命令，
  // Result.ActResult 是服务端返回的结果值
  case Result.Action of
    atUserState:  // 查询用户状态
      if Result.ActResult = arOnline then
        Memo1.Lines.Add(Result.ToUser + ': 在线（已登录）')
      else
        Memo1.Lines.Add(Result.ToUser + ': 离线（未登录）');
  end;
end;

procedure TFormInIOCPCertifyServer.InClientManager1Delete(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 传来的参数在 Params, 可根据自己的情况加以修改！
  // Params.UserName 是发出命令的用户名、Params.ToUser 是待删除的用户名
  // 见：iocp_clients.TInCertifyClient.Delete  
end;

procedure TFormInIOCPCertifyServer.InClientManager1Login(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 客户端登录，检查帐号密码！
  //   用户名统一改为小写！
  //  可以结合数据组件 TInDatabaseManager
  if (Params.UserName <> '') and (Params.Password <> '') then // 测试
  begin
    Result.Msg := '登录成功';   // 返回一个消息
    Result.Role := crAdmin;     // 测试广播要用的权限（2.0改）
    Result.ActResult := arOK;
    
    // 登记属性, 自动设置用户数据路径（注册时建）
    InClientManager1.Add(Params.Socket, crAdmin);  // 用户名统一改为小写！

    // 把离线消息当作附件加入（可能含文件互传消息）
    // 也可以在客户端发送 arTextGet 取离线消息
  //  InMessageManager1.ReadMsgFile(Params, Result);
  end else
  begin
    Result.Msg := '登录失败';
    Result.ActResult := arFail;  // arErrUser 是非法用户，会被断开
  end;
end;

procedure TFormInIOCPCertifyServer.InClientManager1Logout(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 传来的参数在 Params,
  // Params.UserName 是发出命令的用户名
  // 见: iocp_clients.TInCertifyClient.Logout

  // 不管 Result.ActResult 为何值，
  // 内部都执行 logout，见：TInClientManager.Execute
end;

procedure TFormInIOCPCertifyServer.InClientManager1Modify(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 传来的参数在 Params
  // Params.UserName 是发出命令的用户名、Params.ToUser 是待修改的用户名
  // 见：iocp_clients.TInCertifyClient.Modify
end;

procedure TFormInIOCPCertifyServer.InClientManager1QueryState(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 传来的参数在 Params
  // Params.UserName 是发出命令的用户名、Params.ToUser 是待查询的用户名（2.0改）
  // 见：iocp_clients.TInCertifyClient.GetUserState

  //  要先查询用户是否存在于数据库！
  Result.ToUser := Params.ToUser;
  if InClientManager1.Logined(Params.ToUser) then
    Result.ActResult := arOnline    // 在线
  else
    Result.ActResult := arOffline;  // 离线
    
end;

procedure TFormInIOCPCertifyServer.InClientManager1Register(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 传来的参数在 Params
  // Params.UserName 是发出命令的用户名、Params.ToUser 是待注册的用户名（2.0改）
  // 见：iocp_clients.TInCertifyClient.Register

  // 注册新用户到数据库（本例子无数据库连接）

  // 方法：
  //   INSERT INTO xxx (user_name, password, Role) VALUES (
  //     Params.UserName, Params.Password, Integer(Params.Role))；
  // 调用：
  //   TBusiWorker(Sender).DataModule.ExecSQL(Params, Result);

  // 用户数据主目录为 iocp_varis.gUserDataPath，要在其下建
  // 用户 Params.ToUser 的文件目录，再建三个子目录：
  //   1. ToUser\Data: 存放主要文件
  //   2. ToUser\Msg:  存放离线消息文件
  //   3. ToUser\Temp: 存放互传的临时文件
  //  见：磁盘文件

{  MyCreateDir(iocp_varis.gUserDataPath + Params.ToUser);
  MyCreateDir(iocp_varis.gUserDataPath + Params.ToUser + '\data');
  MyCreateDir(iocp_varis.gUserDataPath + Params.ToUser + '\msg');
  MyCreateDir(iocp_varis.gUserDataPath + Params.ToUser + '\temp'); }

  Result.ActResult := arOK;

end;

procedure TFormInIOCPCertifyServer.InConnection1AfterConnect(Sender: TObject);
begin
  btnDisconnect.Enabled := True;
end;

procedure TFormInIOCPCertifyServer.InConnection1Error(Sender: TObject; const Msg: string);
begin
  // 出现异常，有几种异常会自动断开连接：
  // 见：TInConnection.DoServerError、TInConnection.DoThreadFatalError
  Memo1.Lines.Add(Msg);
end;

procedure TFormInIOCPCertifyServer.InConnection1ReceiveMsg(Sender: TObject;  Msg: TResultParams);
begin
  // 这是接收推送消息的事件，本例没消息服务。
end;

procedure TFormInIOCPCertifyServer.InConnection1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
{  用下面的方法发送消息时，会在此事件返回结果
  Msg := TMessagePack.Create(InConnection1);  // 宿主是 InConnection1
  Msg.Post(arTextSend);  }
end;

procedure TFormInIOCPCertifyServer.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

procedure TFormInIOCPCertifyServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
  memo1.Lines.Add('IP:' + InIOCPServer1.ServerAddr);
  memo1.Lines.Add('Port:' + IntToStr(InIOCPServer1.ServerPort));
end;

end.
