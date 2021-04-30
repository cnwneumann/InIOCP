unit frmInIOCPCustomServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_base, iocp_clients, iocp_server,
  iocp_managers, iocp_sockets;

type
  TFormInIOCPCustomServer = class(TForm)
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
    btnCustomSend: TButton;
    btnExecRemFunction: TButton;
    InCustomClient1: TInCustomClient;
    InFunctionClient1: TInFunctionClient;
    InCustomManager1: TInCustomManager;
    InRemoteFunctionGroup1: TInRemoteFunctionGroup;
    InRemoteFunctionGroup2: TInRemoteFunctionGroup;
    InFunctionClient2: TInFunctionClient;
    btnCall2: TButton;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    InFileManager1: TInFileManager;
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
    procedure InClientManager1Logout(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure btnExecRemFunctionClick(Sender: TObject);
    procedure btnCustomSendClick(Sender: TObject);
    procedure InCustomManager1Receive(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InCustomClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InRemoteFunctionGroup1Execute(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure btnCall2Click(Sender: TObject);
    procedure InFunctionClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InRemoteFunctionGroup2Execute(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InFunctionClient2ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InCustomManager1AttachBegin(Sender: TObject;
      Params: TReceiveParams);
    procedure InCustomManager1AttachFinish(Sender: TObject;
      Params: TReceiveParams);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPCustomServer: TFormInIOCPCustomServer;

implementation

uses
  iocp_log, iocp_varis, iocp_utils;

{$R *.dfm}

procedure TFormInIOCPCustomServer.btnCall2Click(Sender: TObject);
begin
  // 服务端的 InRemoteFunctionGroup2 要引用 InCustomManager1

  // 执行远程函数组 TEST_GROUP2 对应的第  2 个功能
  InFunctionClient2.Call('TEST_GROUP2', 2);
end;

procedure TFormInIOCPCustomServer.btnConnectClick(Sender: TObject);
begin
  InConnection1.Active := True;
end;

procedure TFormInIOCPCustomServer.btnCustomSendClick(Sender: TObject);
begin
  // 其他组件都是按功能分类开发的
  //   TInCustomClient 是由用户自定义操作，
  //  发送参数自定义，服务端的操作自定义

  with InCustomClient1 do
  begin
    Params.DateTime := Now; // 时间
    Params.Msg := '一个文本消息';

    Params.AsBoolean['boolean'] := True;
    Params.AsInteger['integer'] := 9999;
    Params.AsString['string'] := '文本内容';

    // 两种方法发送小文件（AsStream 的方法不要自己释放流）
  //  Params.AsStream['stream'] := TFileStream.Create('InIOCP小旋风服务套件.txt', fmOpenRead);
    Params.AsDocument['doc'] := 'doc\jediapilib.inc';

    // 加个文件发送（当作附件，支持大文件）
    Params.LoadFromFile('doc\jediapilib.inc');
    
    Post;
  end;
end;

procedure TFormInIOCPCustomServer.btnDisconnectClick(Sender: TObject);
begin
  InConnection1.Active := False;
end;

procedure TFormInIOCPCustomServer.btnExecRemFunctionClick(Sender: TObject);
begin
  // 服务端的 InRemoteFunctionGroup1 要引用 InCustomManager1

  // 执行远程函数组 TEST_GROUP 对应的第 1 个功能
  InFunctionClient1.Call('TEST_GROUP', 1);

end;

procedure TFormInIOCPCustomServer.btnLoginClick(Sender: TObject);
begin
  // USER_TEST、PASS-AAA
  // 注意：一个 InConnection 对应一个客户端！
  InCertifyClient1.Login;         // 登录，在 InCertifyClient1Certify 处返回结果
end;

procedure TFormInIOCPCustomServer.btnLogoutClick(Sender: TObject);
begin
  InCertifyClient1.Logout;      // 退出
end;

procedure TFormInIOCPCustomServer.btnStartClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  iocp_log.TLogThread.InitLog;              // 开启日志
  InIOCPServer1.Active := True;               // 开启服务
  FrameIOCPSvrInfo1.Start(InIOCPServer1);     // 开始统计
end;

procedure TFormInIOCPCustomServer.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False;   // 停止服务
  FrameIOCPSvrInfo1.Stop;          // 停止统计
  iocp_log.TLogThread.StopLog;   // 停止日志
end;

procedure TFormInIOCPCustomServer.FormCreate(Sender: TObject);
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

procedure TFormInIOCPCustomServer.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
begin
  // 在此判断登录、登出结果
  case Action of
    atUserLogin:       // 登录
      if ActResult then
        Memo1.Lines.Add(InConnection1.UserName + ' 登录成功')
      else
        Memo1.Lines.Add(InConnection1.UserName + ' 登录失败');
    atUserLogout:      // 登出
      if ActResult then
        Memo1.Lines.Add(InConnection1.UserName + ' 登出成功')
      else
        Memo1.Lines.Add(InConnection1.UserName + ' 登出失败');
  end;
end;

procedure TFormInIOCPCustomServer.InClientManager1Login(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 客户端登录，检查帐号密码！
  //   可以结合数据组件 TInDatabaseManager
  if Params.password <> '' then // 测试 AsString['password']
  begin
    Result.Msg := 'Login OK';   // 返回一个消息
    Result.Role := crAdmin;     // 测试广播要用的权限
    Result.ActResult := arOK;
    
    // 登记属性、根据用户定制工作路径
    InClientManager1.Add(Params.Socket, crAdmin);

    // 有离线消息时加入（如文件互传）
  end else
  begin
    Result.Msg := 'Login Fail';
    Result.ActResult := arFail; // 返回 arErrUser 会被关闭
  end;
end;

procedure TFormInIOCPCustomServer.InClientManager1Logout(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 传来的参数在 Params, 参考 iocp_clients.TInCertifyClient.Logout，
  //   可根据自己的情况加以修改！
  // 不管是否操作，内部都退出：logout，见：TInClientManager.Execute
end;

procedure TFormInIOCPCustomServer.InConnection1Error(Sender: TObject;
  const Msg: string);
begin
  Memo1.Lines.Add(Msg);  // 显示异常提示
end;

procedure TFormInIOCPCustomServer.InCustomClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 服务器返回内容

   // 用流方式保存小文件（2.0 流不是引用，要释放！）
  with TMemoryStream(Result.AsStream['doc']) do
    try
      SaveToFile('temp\_返回的小文件.txt');
    finally
      Free;
    end;

  Memo1.Lines.Add('客户端收到结果：' + Result.Msg);

end;

procedure TFormInIOCPCustomServer.InCustomManager1AttachBegin(Sender: TObject;
  Params: TReceiveParams);
begin
  // TInCustomManager 公开了开始接收附件的方法
  // 这个消息带附件，建附件流（文件流），此时 Params.Attachment = nil

  Memo1.Lines.Add('准备接收附件：' + Params.FileName);

  // 1. 推荐用文件管理器，保存到用户的临时路径
  InFileManager1.CreateNewFile(Params);

  // 2. 也可以直接指定存放路径
//  Params.CreateAttachment(iocp_varis.gUserDataPath +
//                          Params.UserName + '\temp\');
end;

procedure TFormInIOCPCustomServer.InCustomManager1AttachFinish(Sender: TObject;
  Params: TReceiveParams);
begin
  // TInCustomManager 公开了附件接收完毕的方法
  //   Params.Attachment，是 TIOCPDocument 文件流，
  //   在此不要 Free 附件，系统自动释放！
  Memo1.Lines.Add('附件接收完毕：' + Params.Attachment.FileName);
end;

procedure TFormInIOCPCustomServer.InCustomManager1Receive(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 根据参数处理自定义事务，用 Result 返回数据

  Memo1.Lines.Add('服务端：');
  Memo1.Lines.Add('msg=' + Params.Msg);
  Memo1.Lines.Add('dateTime=' + DateTimeToStr(Params.DateTime));

  Memo1.Lines.Add('boolean=' + BoolToStr(Params.AsBoolean['boolean']));
  Memo1.Lines.Add('integer=' + IntToStr(Params.AsInteger['integer']));
  Memo1.Lines.Add('string=' + Params.AsString['string']);

  // 用流方式保存小文件（2.0 流不是引用，要释放！）
  with TMemoryStream(Params.AsStream['doc']) do
    try
      SaveToFile('temp\收到小文件.txt');
    finally
      Free;
    end;

  // 返回一点内容给客户端
  Result.Msg := '服务器执行成功！';
  Result.AsDocument['doc'] := 'temp\收到小文件.txt';   // 返回小文件

  // 可以把文件当作附件返回给客户端
  Result.LoadFromFile('doc\jediapilib.inc');
    
  // 可根据需要设置 ActResult
  Result.ActResult := arOK;

end;

procedure TFormInIOCPCustomServer.InFunctionClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.ActResult of
    arOK: begin
      Memo1.Lines.Add('客户端：aaa=' + Result.AsString['aaa']);
      // 用流方式保存小文件
      with TMemoryStream(Result.AsStream['doc']) do
        try
          SaveToFile('temp\_调用返回小文件.txt');
        finally
          Free;
        end;
    end;
    arMissing:
      Memo1.Lines.Add('函数组不存在！');
    arFail:
      Memo1.Lines.Add('执行远程函数失败');
  end;
end;

procedure TFormInIOCPCustomServer.InFunctionClient2ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.ActResult of
    arOK:
      Memo1.Lines.Add('客户端：call2=' + Result.AsString['call2']);
    arMissing:
      Memo1.Lines.Add('函数组不存在');
  end;
end;

procedure TFormInIOCPCustomServer.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

procedure TFormInIOCPCustomServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
  Memo1.Lines.Add('ip: ' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('port: ' + IntToStr(InIOCPServer1.ServerPort));
end;

procedure TFormInIOCPCustomServer.InRemoteFunctionGroup1Execute(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 执行远程函数组中某一编号的功能
  case Params.FunctionIndex of
    1: begin
      Result.AsString['aaa'] := 'call remote function group 1-1.';
      Result.AsDocument['doc'] := 'doc\jediapilib.inc';  // 返回小文件
      Result.ActResult := arOK;    // 可能是其他情况
    end;
    2: begin
      // 调用其他功能
      Result.ActResult := arOK;
    end;
    else
      Result.ActResult := arFail;
  end;
end;

procedure TFormInIOCPCustomServer.InRemoteFunctionGroup2Execute(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 执行远程函数组中某一编号的功能
  case Params.FunctionIndex of
    1: begin
      Result.ActResult := arOK;
    end;
    2: begin
      Result.AsString['call2'] := 'call remote function group 2-2.';
      Result.ActResult := arOK;
    end;
  end;
end;

end.
