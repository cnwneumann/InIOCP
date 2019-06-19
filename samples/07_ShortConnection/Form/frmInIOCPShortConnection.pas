unit frmInIOCPShortConnection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_base, iocp_clients, iocp_server,
  iocp_sockets, iocp_managers;

type
  TFormInIOCPShortConnection = class(TForm)
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    InConnection1: TInConnection;
    btnStart: TButton;
    btnStop: TButton;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnSend: TButton;
    InMessageManager1: TInMessageManager;
    InMessageClient1: TInMessageClient;
    EditTarget: TEdit;
    btnBroad: TButton;
    InClientManager1: TInClientManager;
    InCertifyClient1: TInCertifyClient;
    btnLogin: TButton;
    btnLogout: TButton;
    EditUserName: TEdit;
    edtPort: TEdit;
    lbl1: TLabel;
    btnQuery: TButton;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    Button1: TButton;
    InEchoClient1: TInEchoClient;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure InConnection1Error(Sender: TObject; const Msg: string);
    procedure btnSendClick(Sender: TObject);
    procedure btnBroadClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure InClientManager1Login(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure FormCreate(Sender: TObject);
    procedure InMessageClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure btnLogoutClick(Sender: TObject);
    procedure InCertifyClient1Certify(Sender: TObject; Action: TActionType;
      ActResult: Boolean);
    procedure InMessageManager1Receive(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure EditUserNameDblClick(Sender: TObject);
    procedure btnQueryClick(Sender: TObject);
    procedure InCertifyClient1ListClients(Sender: TObject; Count, No: Cardinal;
      Client: PClientInfo);
    procedure InMessageManager1Push(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InMessageManager1Broadcast(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure Button1Click(Sender: TObject);
    procedure InEchoClient1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure InConnection1ReceiveMsg(Sender: TObject; Msg: TResultParams);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPShortConnection: TFormInIOCPShortConnection;
  InstanceCount: Integer = 0;
  
implementation

uses
  iocp_log, iocp_varis, iocp_utils;

{$R *.dfm}

procedure TFormInIOCPShortConnection.btnBroadClick(Sender: TObject);
begin
  // 管理员权限才能广播
  InMessageClient1.Broadcast('广播消息, aaa, bbb');
end;

procedure TFormInIOCPShortConnection.btnConnectClick(Sender: TObject);
begin
  // 注意：
  //  InConnection1.AutoConnect := True;
  //  InConnection1.ReuseSession := True;
  InConnection1.Active := True;
end;

procedure TFormInIOCPShortConnection.btnDisconnectClick(Sender: TObject);
begin
  InConnection1.Active := False;
end;

procedure TFormInIOCPShortConnection.btnLoginClick(Sender: TObject);
begin
  // 重用 Session：
  //   InConnection1.ReuseSession := True;
  InCertifyClient1.UserName := EditUserName.Text;
  InCertifyClient1.Password := 'AAABBB';
  InCertifyClient1.Login;
end;

procedure TFormInIOCPShortConnection.btnLogoutClick(Sender: TObject);
begin
  // InConnection1.ReuseSession := True;
  //   -> 登出后保留 InConnection1.Session

  // 30分钟内再次连接后不用重新登录！可以直接发送消息等...

  InCertifyClient1.Logout;
end;

procedure TFormInIOCPShortConnection.btnSendClick(Sender: TObject);
begin
  // 发送前要先登录
  //   请再运行本程序测试相互之间的消息发送
  if EditTarget.Text <> '' then
    InMessageClient1.SendMsg('发消息给 ' + EditTarget.Text, EditTarget.Text)
  else
    InMessageClient1.SendMsg('发消息到服务端（无意义）');
end;

procedure TFormInIOCPShortConnection.btnStartClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  iocp_log.TLogThread.InitLog;              // 开启日志

  InIOCPServer1.ServerPort := StrToInt(edtPort.Text);
  InIOCPServer1.Active := True;               // 开启服务

  FrameIOCPSvrInfo1.Start(InIOCPServer1);     // 开始统计
end;

procedure TFormInIOCPShortConnection.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False; // 停止服务
  FrameIOCPSvrInfo1.Stop;        // 停止统计
  iocp_log.TLogThread.StopLog;   // 停止日志
end;

procedure TFormInIOCPShortConnection.Button1Click(Sender: TObject);
begin
  InEchoClient1.Post;  // 测试一下服务器的反应
end;

procedure TFormInIOCPShortConnection.btnQueryClick(Sender: TObject);
begin
  // 查询全部连接的客户端（可能未登录）
  //   在 InCertifyClient1ListClients 处返回结果
  InCertifyClient1.QueryClients;
end;

procedure TFormInIOCPShortConnection.EditUserNameDblClick(Sender: TObject);
begin
  EditUserName.Text := 'USER_B';
end;

procedure TFormInIOCPShortConnection.FormCreate(Sender: TObject);
begin
  if (InstanceCount = 1) then
  begin
    btnStart.Enabled := False;
    btnStop.Enabled := False;
    EditUserName.Text := 'USER_B';
    EditTarget.Text := 'USER_A';
  end;
  
  // 准备工作路径
  FAppDir := ExtractFilePath(Application.ExeName);
  iocp_utils.IniDateTimeFormat;    // 设置日期时间格式

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

procedure TFormInIOCPShortConnection.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
begin
  case Action of
    atUserLogin:       // 登录
      if ActResult then
      begin
        Memo1.Lines.Add('登录成功，30 分钟内 Session 有效，请断开, 连接->发送,');
        Memo1.Lines.Add('不用每次连接后都登录了')
      end else
        Memo1.Lines.Add('登录失败');
    atUserLogout:      // 登出
      if ActResult then
        Memo1.Lines.Add('登出成功')
      else
        Memo1.Lines.Add('登出失败');
  end;
end;

procedure TFormInIOCPShortConnection.InCertifyClient1ListClients(Sender: TObject;
  Count, No: Cardinal; Client: PClientInfo);
begin
  // 在此列出查询到的客户端信息
  //  Client^.Socket = 0 的为短连接客户端
  if Client^.Socket = 0 then
    memo1.Lines.Add(IntToStr(No) + '/' + IntToStr(Count) + ', ' +
             Client^.Name + '  ->  ' + IntToStr(Cardinal(Client^.Socket)) + '(短连接)')
  else
    memo1.Lines.Add(IntToStr(No) + '/' + IntToStr(Count) + ', ' +
             Client^.Name + '  ->  ' + IntToStr(Cardinal(Client^.Socket)));
end;

procedure TFormInIOCPShortConnection.InClientManager1Login(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  if (Params.Password <> '') then
  begin
    Result.Role := crAdmin;  // 2.0 改，返回 crAdmin 权限，能广播
    Result.ActResult := arOK;
    // 登记属性, 自动设置用户数据路径（注册时建）
    InClientManager1.Add(Params.Socket, crAdmin);
  end else
    Result.ActResult := arFail;
end;

procedure TFormInIOCPShortConnection.InConnection1Error(Sender: TObject; const Msg: string);
begin
  Memo1.Lines.Add(Msg);  // 显示异常提示
end;

procedure TFormInIOCPShortConnection.InConnection1ReceiveMsg(Sender: TObject;
  Msg: TResultParams);
begin
  // 收到其他客户发来的消息（被动接收）
  Memo1.Lines.Add(InConnection1.UserName + ' 收到 ' + IntToStr(Msg.Owner) + ' 的消息：' + Msg.Msg);
end;

procedure TFormInIOCPShortConnection.InEchoClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 收到响应的反馈, Result.VarCount 是连接数
  Memo1.Lines.Add('服务器反馈, 客户端连接数=' + IntToStr(Result.VarCount));
end;

procedure TFormInIOCPShortConnection.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

procedure TFormInIOCPShortConnection.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
  Memo1.Lines.Add('ip: ' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('port: ' + IntToStr(InIOCPServer1.ServerPort));
end;

procedure TFormInIOCPShortConnection.InMessageClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 服务器反馈事件
  case Result.ActResult of
    arOK:
      Memo1.Lines.Add('消息发送成功.');
    arOffline:
      Memo1.Lines.Add('反馈：对方离线.');
    arOutDate:
      Memo1.Lines.Add('凭证过期，请重新登录.');
  end;
end;

procedure TFormInIOCPShortConnection.InMessageManager1Broadcast(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 广播：发送消息给全部在线客户端
  InMessageManager1.Broadcast(Params);
  Result.ActResult := arOK;
end;

procedure TFormInIOCPShortConnection.InMessageManager1Push(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 发送消息给其他客户端: ToUser、TargetUser
  if (Params.ToUser <> '') then
  begin
    InMessageManager1.PushMsg(Params, Params.ToUser);
    Result.Msg := '推送消息成功已投放';
  end else
    Result.Msg := '消息未投放';
end;

procedure TFormInIOCPShortConnection.InMessageManager1Receive(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  Memo1.Lines.Add('服务器：收到消息 ' + Params.Msg);
  Result.ActResult := arOK;
end;

end.
