unit frmInIOCPMessageServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_base, iocp_clients, iocp_server,
  iocp_sockets, iocp_managers, iocp_msgPacks, ExtCtrls, Buttons;

type
  TFormInIOCPMessageServer = class(TForm)
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
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    BitBtn1: TBitBtn;
    InFileManager1: TInFileManager;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    InCustomManager1: TInCustomManager;
    InCustomClient1: TInCustomClient;
    Button1: TButton;
    LabeledEdit1: TLabeledEdit;
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
    procedure InMessageClient1MsgReceive(Sender: TObject; Socket: UInt64;
      Msg: string);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InMessageManager1Broadcast(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InMessageManager1Get(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InMessageManager1Push(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InConnection1ReceiveMsg(Sender: TObject; Msg: TResultParams);
    procedure InCertifyClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure BitBtn1Click(Sender: TObject);
    procedure InMessageClient1ListFiles(Sender: TObject;
      ActResult: TActionResult; No: Integer; Result: TCustomPack);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure InCustomManager1Receive(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InIOCPServer1Connect(Sender: TObject; Socket: TBaseSocket);
    procedure InIOCPServer1Disconnect(Sender: TObject; Socket: TBaseSocket);
    procedure InMessageManager1ListFiles(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
  private
    { Private declarations }
    FAppDir: String;
    FLastMsgId: UInt64;  // 本地记录的最大推送消息编号
  public
    { Public declarations }
  end;

var
  FormInIOCPMessageServer: TFormInIOCPMessageServer;
  InstanceCount: Integer;

implementation

uses
  iocp_log, iocp_varis, iocp_utils;

{$R *.dfm}

procedure TFormInIOCPMessageServer.BitBtn1Click(Sender: TObject);
begin
  // 取离线消息，在 InMessageClient1.OnReturnResult 处理
  InMessageClient1.GetOfflineMsgs;
end;

procedure TFormInIOCPMessageServer.BitBtn2Click(Sender: TObject);
begin
  // 列出用户在服务端的消息文件表
  // 在 InMessageClient1.OnListFiles 处理结果
  InMessageClient1.GetMsgFiles;
end;

procedure TFormInIOCPMessageServer.BitBtn3Click(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // 用新特性直接发送消息
  Msg := TMessagePack.Create(InMessageClient1); // 宿主为 InMessageClient1
  Msg.Msg := '新特性消息.';
  Msg.Post(atTextSend);                              

  // 发送消息
  Msg := TMessagePack.Create(InMessageClient1); // 宿主为 InMessageClient1
  Msg.Msg := '新特性消息+附件.';
  Msg.LoadFromFile('doc\jediapilib.inc'); // 顺便加个附件
  Msg.Post(atTextSend);

end;

procedure TFormInIOCPMessageServer.btnBroadClick(Sender: TObject);
begin
  // 管理员权限才能广播
  InMessageClient1.Broadcast('广播消息：... ...');
end;

procedure TFormInIOCPMessageServer.btnConnectClick(Sender: TObject);
begin
  InConnection1.Active := True;
end;

procedure TFormInIOCPMessageServer.btnDisconnectClick(Sender: TObject);
begin
  InConnection1.Active := False;
end;

procedure TFormInIOCPMessageServer.btnLoginClick(Sender: TObject);
begin
  InCertifyClient1.Group := LabeledEdit1.Text;
  InCertifyClient1.UserName := EditUserName.Text;
  InCertifyClient1.Password := 'AAABBB';
  InCertifyClient1.Login;
end;

procedure TFormInIOCPMessageServer.btnLogoutClick(Sender: TObject);
begin
  InCertifyClient1.Logout;
end;

procedure TFormInIOCPMessageServer.btnSendClick(Sender: TObject);
begin
  // 请再运行本程序测试相互之间的消息发送
  // 消息的发出方为 InConnection.UserName，内部自动设置
  // 在 InConnection.OnReceiveMsg 中处理被动接收的消息（推送消息）
  if EditTarget.Text <> '' then
    InMessageClient1.SendMsg('这是发给 ' + EditTarget.Text + ' 的消息.', EditTarget.Text)
  else
    InMessageClient1.SendMsg('发消息到服务端（无意义）');
end;

procedure TFormInIOCPMessageServer.btnStartClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  
  iocp_log.TLogThread.InitLog;   // 开启日志

  InIOCPServer1.ServerPort := StrToInt(edtPort.Text);
  InIOCPServer1.Active := True;  // 开启服务

  FrameIOCPSvrInfo1.Start(InIOCPServer1);  // 开始统计

end;

procedure TFormInIOCPMessageServer.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False; // 停止服务
  FrameIOCPSvrInfo1.Stop;        // 停止统计
  iocp_log.TLogThread.StopLog;   // 停止日志
end;

procedure TFormInIOCPMessageServer.Button1Click(Sender: TObject);
begin
  // 发送自定义消息给 EditTarget
  //   见: InCustomManager1Receive、InConnection1ReceiveMsg
  with InCustomClient1 do
  begin
    Params.ToUser := EditTarget.Text; // 目的
    Params.AsString['xxxx'] := '推送自定义消息, aaaa 1234667890, bbb';
    Post;
  end;
end;

procedure TFormInIOCPMessageServer.EditUserNameDblClick(Sender: TObject);
begin
  EditUserName.Text := 'USER_b';
end;

procedure TFormInIOCPMessageServer.FormCreate(Sender: TObject);
begin
  if InstanceCount = 1 then
  begin
    EditUserName.Text := 'user_b';
    EditTarget.Text := 'user_a';
    btnStart.Enabled := False;
    btnStop.Enabled := False;
  end;

  FLastMsgId := 0;

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

procedure TFormInIOCPMessageServer.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
begin
  case Action of
    atUserLogin: begin      // 登录
      if ActResult then
        Memo1.Lines.Add(TInCertifyClient(Sender).UserName + ': 登录成功')
      else
        Memo1.Lines.Add(TInCertifyClient(Sender).UserName + ': 登录失败');
    end;
    atUserLogout: begin     // 登出
      if ActResult then
        Memo1.Lines.Add(TInCertifyClient(Sender).UserName + ': 登出成功')
      else
        Memo1.Lines.Add(TInCertifyClient(Sender).UserName + ': 登出失败');
    end;
  end;
end;

procedure TFormInIOCPMessageServer.InCertifyClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
var
  i, k: Integer;
  Msg: TReceivePack;  // 接收消息包
  Reader: TMessageReader;  // 离线消息阅读器
begin
  // 登录时，如果服务器发送离线消息，在这里处理
  if Assigned(Result.Attachment) then // 有附件，可能是离线消息，读出！
  begin
    Memo1.Lines.Add('有离线消息.');
    Msg := TReceivePack.Create;
    Reader := TMessageReader.Create;
    try
      // Attachment 已经关闭，但未释放
      // 打开文件，如果不是消息文件 -> Count = 0
      Reader.Open(Result.Attachment.FileName);

      // 请把以前读过的最大 MsgId 保存到磁盘，
      // 登录前读入并设置 LastMsgId = ???，从离线
      // 消息文件中读出比 LastMsgId 大的消息。

      for i := 0 to Reader.Count - 1 do
      begin
        if Reader.Extract(Msg, FLastMsgId) then  // 读出比 LastMsgId 大的消息
          for k := 0 to Msg.Count - 1 do
            with Msg.Fields[k] do
              Memo1.Lines.Add(Name + '=' + AsString);
      end;

      // 这是最大的消息号
      if Msg.Action <> atUnknown then
        FLastMsgId := Msg.MsgId;

    finally
      Msg.Free;
      Reader.Free;
    end;
  end;
end;

procedure TFormInIOCPMessageServer.InClientManager1Login(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 客户端登录，检查帐号密码！
  //   可以结合数据组件 TInDatabaseManager

  // 例子预设了 user_a、user_b 两个用户的数据目录
  // 用其他用户名时，要建相应的数据目录

  if Params.Password <> '' then // 测试
  begin
    Result.Msg := '登录成功';   // 返回一个消息
    Result.Role := crAdmin;     // 测试广播要用的权限（2.0改）
    Result.ActResult := arOK;
    
    // 登记属性, 自动设置用户数据路径（注册时建）
    InClientManager1.Add(Params.Socket, crAdmin);

    // 把离线消息当作附件加入（可能含文件互传消息）
    // 也可以在客户端发送 arTextGet 取离线消息
    InMessageManager1.ReadMsgFile(Params, Result);
  end else
  begin
    Result.Msg := '登录失败';
    Result.ActResult := arFail;  // arErrUser 是非法用户，会被断开
  end;
end;

procedure TFormInIOCPMessageServer.InConnection1Error(Sender: TObject;
  const Msg: string);
begin
  // 返回服务器或客户端的各种异常！
  // 见：TInConnection.DoServerError、TInConnection.DoThreadFatalError
  Memo1.Lines.Add(Msg);
end;

procedure TFormInIOCPMessageServer.InConnection1ReceiveMsg(Sender: TObject;
  Msg: TResultParams);
begin
  // 在这里处理收到的推送消息（被动接收消息）
  case Msg.Action of
    atTextPush, atTextBroadcast:
      Memo1.Lines.Add('收到推送消息：' + Msg.Msg + ', 来自：' + Msg.UserName);
    atCustomAction:  // 自定义消息
      Memo1.Lines.Add(Msg.AsString['xxxx']);
  end;
end;

procedure TFormInIOCPMessageServer.InCustomManager1Receive(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
var
  oSocket: TIOCPSocket;
begin
  // 收到自定义消息
  if InClientManager1.Logined(Params.ToUser, oSocket) then
  begin
  //  InMessageManager1.PushMsg(Params, oSocket); // = Params.Socket.Push(oSocket);
    Result.ActResult := arOK; // 投放出去了，但不知道结果
  end else
    Result.ActResult := arOffline;   // 对方离线
end;

procedure TFormInIOCPMessageServer.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

procedure TFormInIOCPMessageServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
  memo1.Lines.Add('IP:' + InIOCPServer1.ServerAddr);
  memo1.Lines.Add('Port:' + IntToStr(InIOCPServer1.ServerPort));
end;

procedure TFormInIOCPMessageServer.InIOCPServer1Connect(Sender: TObject;
  Socket: TBaseSocket);
begin
  // 使用 Data 属性，扩展一下功能
  // 如禁止接入可以直接 Socket.Close;
{  Socket.Data := TInMemStream.Create;   }
end;

procedure TFormInIOCPMessageServer.InIOCPServer1Disconnect(Sender: TObject;
  Socket: TBaseSocket);
begin
  // 要判断释放 Data 的数据
  // 系统默认的客户端对象是 THttpSocket，
  // Socket 资源转换为对应协议的 TBaseSocket 后，会关闭 Socket，
  // 此时要判断一下，如果 Socket.Connected = False 则是资源转换后的 Socket，不用处理
{  if Socket.Connected and Assigned(Socket.Data) then
    TInMemStream(Socket.Data).Free;   }
end;

procedure TFormInIOCPMessageServer.InMessageClient1ListFiles(Sender: TObject;
  ActResult: TActionResult; No: Integer; Result: TCustomPack);
begin
  // atFileList, atTextGetFiles 操作会先执行本事件,再执行 OnReturnResult
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

procedure TFormInIOCPMessageServer.InMessageClient1MsgReceive(Sender: TObject;
  Socket: UInt64; Msg: string);
begin
  // 收到其他客户发来的消息（被动的）
  Memo1.Lines.Add(InConnection1.UserName + ' 收到 ' + IntToStr(Socket) + ' 的消息：' + Msg);
end;

procedure TFormInIOCPMessageServer.InMessageClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 服务器反馈事件
  // atFileList 操作会先执行 OnListFiles,再执行本事件    
  case Result.ActResult of
    arOK:
      Memo1.Lines.Add('消息发送成功.' + Result.Msg);
    arOffline:
      Memo1.Lines.Add('反馈：对方离线.');
  end;

  // 可能有离线消息
  InCertifyClient1ReturnResult(Sender, Result);
end;

procedure TFormInIOCPMessageServer.InMessageManager1Broadcast(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 广播：发送消息给全部客户端
  //       把消息写给全部客户端的方便不可行，这里不处理！
//  Memo1.Lines.Add(Params.Msg);
  // 广播，所有客户端（包括未登录）都收到
  InMessageManager1.Broadcast(Params);
end;

procedure TFormInIOCPMessageServer.InMessageManager1Get(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 取离线消息（客户端发送 arTextGet 请求）
  InMessageManager1.ReadMsgFile(Params, Result);  // 未必有离线消息
  Result.Msg := '请求离线消息。';
  Result.ActResult := arOK;
end;

procedure TFormInIOCPMessageServer.InMessageManager1ListFiles(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 离线消息大小为 2m，超过时文件被改名，在这里用文件管理器列出,
  // 见：InternalOpenMsgFile
  // 用文件管理器取信息，客户端还是用消息组件的 OnListFiles 显示
  InFileManager1.ListFiles(Params, Result, True);
end;

procedure TFormInIOCPMessageServer.InMessageManager1Push(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 推送消息给其他客户端: ToUser、TargetUser
  
  // 用第三方工具推送时可以用 JSON 格式的消息
  //  mmoServer.Lines.Add(Params.ToJSON);

  // 1、推送给单客户端（ToUser只包含一个客户端名称）

  // 1.1 可以先保存到消息文件
  // InMessageManager1.SaveMsgFile(Params);

  // 1.2 检查客户端是否在线
{  Result.UserName := Params.UserName;

  if InClientManager1.Logined(Params.ToUser, oSocket) then
  begin
    InMessageManager1.PushMsg(Params);
    Result.ActResult := arOK;
    Result.Msg := '已推送消息: ' + Params.ToUser;
  end else
  begin
    Result.ActResult := arOffline;  // 对方离线
    Result.Msg := '用户离线: ' + Params.ToUser;
  end;     }

  // 2、如果不返回客户端的在线状态，可以直接这样：

  // 2.1 推送给 ToUser（多个客户端以","、";"分隔）
  InMessageManager1.PushMsg(Params, Params.ToUser);

  // 2.2 返回总体的投放情况
  if (Result.ActResult = arOK) then
    Result.Msg := '投放推送消息成功.'
  else
    Result.Msg := '投放推送消息失败.';
end;

procedure TFormInIOCPMessageServer.InMessageManager1Receive(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 均衡服务时不显示
  Memo1.Lines.Add('服务器收到消息：' + Params.Msg); //

  // 把一个文件当作附件返回给客户端，客户端存放路径:
  //   InConnection1.LocalPath
  Result.LoadFromFile('doc\jediapilib.inc');

  Result.ActResult := arOK;

  // 消息管理器没公开接收附件的方法，可以用两种方法接收附件：

  // 1. 推荐用文件管理器
  if (Params.AttachSize > 0) then
    InFileManager1.CreateNewFile(Params); // 保存到用户的数据路径
//  InFileManager1.CreateNewFile(Params, True); // 保存到用户的临时路径

  // 2. 也可以直接指定存放路径
{  if (Params.AttachSize > 0) then
    Params.CreateAttachment(iocp_varis.gUserDataPath +
                          Params.UserName + '\temp\');    }

end;

end.
