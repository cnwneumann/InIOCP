unit frmInIOCPMsgServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, fmIOCPSvrInfo,
  iocp_server, http_objects, iocp_msgPacks, iocp_managers, iocp_sockets;

type
  TFormInIOCPMsgServer = class(TForm)
    InIOCPServer1: TInIOCPServer;
    InFileManager1: TInFileManager;
    InClientManager1: TInClientManager;
    InMessageManager1: TInMessageManager;
    InDatabaseManager1: TInDatabaseManager;
    InHttpDataProvider1: TInHttpDataProvider;
    btnStart: TButton;
    btnStop: TButton;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    Memo1: TMemo;
    lbEditIP: TLabeledEdit;
    lbEditPort: TLabeledEdit;
    Button1: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InHttpDataProvider1Get(Sender: TObject; Request: THttpRequest;
      Respone: THttpRespone);
    procedure InClientManager1Delete(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1Login(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1QueryState(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InIOCPServer1Disconnect(Sender: TObject; Socket: TBaseSocket);
    procedure InClientManager1Modify(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1Register(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1Logout(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InMessageManager1Get(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InMessageManager1Broadcast(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InMessageManager1ListFiles(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InMessageManager1Push(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InMessageManager1Receive(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InFileManager1AfterDownload(Sender: TObject;
      Params: TReceiveParams; Document: TIOCPDocument);
    procedure InFileManager1AfterUpload(Sender: TObject; Params: TReceiveParams;
      Document: TIOCPDocument);
    procedure InFileManager1BeforeUpload(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InFileManager1BeforeDownload(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInIOCPMsgServer: TFormInIOCPMsgServer;

implementation

uses
  iocp_varis, iocp_log, iocp_base, iocp_utils,
  dm_iniocp_sqlite3;

{$R *.dfm}

procedure TFormInIOCPMsgServer.btnStartClick(Sender: TObject);
begin
  if InIOCPServer1.Active then
  begin
    InIOCPServer1.Active := False;  // 启动服务，自动清除注册数模信息
    FrameIOCPSvrInfo1.Stop;  // 统计模板
    iocp_log.TLogThread.StopLog;
  end else
  begin
    iocp_log.TLogThread.InitLog('log');  // 启动日志，写日志：iocp_log.WriteLog()
    InDatabaseManager1.AddDataModule(TdmInIOCPSQLite3, '客户端数据');  // 注册数模

    InIOCPServer1.ServerAddr := lbEditIP.Text;
    InIOCPServer1.ServerPort := StrToInt(lbEditPort.Text);

    InIOCPServer1.Active := True;  // 启动服务
    FrameIOCPSvrInfo1.Start(InIOCPServer1);  // 统计模板
  end;
end;

procedure TFormInIOCPMsgServer.FormCreate(Sender: TObject);
begin
  gUserDataPath := gAppPath + 'data\data_server\';
  MyCreateDir(gAppPath + 'log');  // 日志目录
  MyCreateDir(gAppPath + 'SQL');  // SQL 目录
  MyCreateDir(gUserDataPath);     // 服务端用户数据目录
end;

procedure TFormInIOCPMsgServer.InClientManager1Delete(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // Sender 是 TBusiWorker，其 DataModule 是启动服务时注册的 TdmInIOCPSQLite3 的实例
  // 删除用户

  Result.UserName := Params.UserName;
  if (Params.ToUser <> Params.LogName) then  // LogName 是登录时的名称，Params.LogName = Params.Socket.LogName
  begin
    // 查询需删除的用户 ToUser 的权限
    TdmInIOCPSQLite3(TBusiWorker(Sender).DataModule).QueryUser(Params, Result);

    if (Result.ActResult <> arOK) then
    begin
      Result.Msg := '用户 ' + Params.ToUser + ' 不存在.';
    end else
    if (TIOCPSocket(Params.Socket).Role > Result.Role) then // 有权限
    begin
      TdmInIOCPSQLite3(TBusiWorker(Sender).DataModule).DeleteUser(Params, Result);

      if (Result.ActResult = arOK) then  // 在数据表删除成功
      begin
        InClientManager1.Disconnect(Params.ToUser);  // 断开客户端
        Result.Msg := '删除用户成功: ' + Params.ToUser;
      end;
    end else
    begin
      Result.ActResult := arFail;
      Result.Msg := '删除用户, 权限不足.';
    end;
  end else
  begin
    Result.ActResult := arFail;
    Result.Msg := '不能删除自己.';
  end;

  // 推送给除自己外的全部管理员
  InMessageManager1.PushToAdmin(Result);
  
end;

procedure TFormInIOCPMsgServer.InClientManager1Login(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // Sender 是 TBusiWorker，其 DataModule 是启动服务时注册的 TdmInIOCPSQLite3 的实例
  // 登录
  
  // 取用户信息：见客户端和文件“数据库操作.SQL"的 [USER_LOGIN]
  TdmInIOCPSQLite3(TBusiWorker(Sender).DataModule).UserLogin(Params, Result);

  if (Result.ActResult = arOK) then  // 用户存在
  begin
    Result.Msg := 'Login OK';
    Result.UserName := Params.UserName;
    Result.AsDateTime['action_time'] := Now;

    // 登记信息在前：设 Role
    // 保存登录名、用户的数据路径（注册时建）等信息到 Socket.Evir
    //   Params.UserName 会被保存到 Socket.LogName 中
    InClientManager1.Add(Params.Socket, Result.Role);  // 用户名统一改为小写！

    // 推送给除自己外的全部管理员
    InMessageManager1.PushToAdmin(Result);

    // 管理员权限时，可以直接广播（不包括附件）
//    InMessageManager1.Broadcast(Result);  // 有权限限制
  end else
  begin
    Result.Msg := 'Login Fail';
    // Result.ActResult 返回 arErrUser 会断开连接
  end;
end;

procedure TFormInIOCPMsgServer.InClientManager1Logout(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // Sender 是 TBusiWorker，其 DataModule 是启动服务时注册的 TdmInIOCPSQLite3 的实例
  // 登出
  
  if (Params.UserName = Params.LogName) then  // LogName 是登录时的名称
  begin
    // 更新登出时间，见客户端和文件“数据库操作.SQL"的 [USER_LOGOUT]
    TdmInIOCPSQLite3(TBusiWorker(Sender).DataModule).UserLogout(Params, Result);

    if (Result.ActResult = arOK) then
    begin
      // 加入客户端所需的信息
      Result.Msg := 'User Logout.';
      Result.UserName := Params.UserName;
      Result.AsDateTime['action_time'] := Now;

      // 推送给除自己外的全部管理员
      InMessageManager1.PushToAdmin(Result);

      // 管理员权限时，可以直接广播（不包括附件）
//      InMessageManager1.Broadcast(Result);  // 有权限限制
    end;
  end else
  begin
    // 可能是客户端参数值用错
    Result.ActResult := arFail;
    Result.Msg := '客户端名称与登录名名称不同.';
  end;

end;

procedure TFormInIOCPMsgServer.InClientManager1Modify(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 修改（未检查权限）
  
  // 见客户端和文件“数据库操作.SQL"的 [USER_MODIFY]
  TdmInIOCPSQLite3(TBusiWorker(Sender).DataModule).ModifyUser(Params, Result);

  Result.UserName := Params.UserName;
  if Result.ActResult = arOK  then
    Result.Msg := '修改用户信息成功: ' + Params.ToUser;

  // 推送给除自己外的全部管理员
  InMessageManager1.PushToAdmin(Result);
  
end;

procedure TFormInIOCPMsgServer.InClientManager1QueryState(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 查询 Params.ToUser 的状态（查询后可能立刻改变）

  // 先检查 Params.ToUser 是否存在，再检查是否登录
  TdmInIOCPSQLite3(TBusiWorker(Sender).DataModule).QueryUser(Params, Result);
  if (Result.ActResult = arOK) then
    InClientManager1.GetClientState(Params.ToUser, Result);
end;

procedure TFormInIOCPMsgServer.InClientManager1Register(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 注册新用户 Params.ToUser 到数据库（未连接登录）

  // 用户数据主目录为 iocp_varis.gUserDataPath，要在其下建
  // 用户 Params.ToUser 的文件目录，再建三个子目录：
  //   1. ToUser\Data: 存放主要文件
  //   2. ToUser\Msg:  存放离线消息文件
  //   3. ToUser\Temp: 存放互传的临时文件
  //  见：磁盘文件

  if (TIOCPSocket(Params.Socket).Role >= crAdmin) then // 权限允许
  begin
    // 见客户端和文件“数据库操作.SQL"的 [USER_REGISTER]
    TdmInIOCPSQLite3(TBusiWorker(Sender).DataModule).RegisterUser(Params, Result);

    Result.UserName := Params.UserName;

    if (Result.ActResult = arOK) then
    begin
      MyCreateDir(iocp_varis.gUserDataPath + Params.ToUser + '\data');
      MyCreateDir(iocp_varis.gUserDataPath + Params.ToUser + '\msg');
      MyCreateDir(iocp_varis.gUserDataPath + Params.ToUser + '\temp');
      Result.Msg := '注册用户成功: ' + Params.ToUser;
    end;
  end else
  begin
    Result.ActResult := arFail;
    Result.Msg := '注册用户, ' + Params.UserName + '权限不足.';
  end;

  // 推送给除自己外地全部管理员
  InMessageManager1.PushToAdmin(Result);
  
end;

procedure TFormInIOCPMsgServer.InFileManager1AfterDownload(Sender: TObject;
  Params: TReceiveParams; Document: TIOCPDocument);
begin
  // 文件下载完毕触发此事件
end;

procedure TFormInIOCPMsgServer.InFileManager1AfterUpload(Sender: TObject;
  Params: TReceiveParams; Document: TIOCPDocument);
begin
  // 文件上传完毕触发此事件，要注意文件的路径安全问题
  // 新版调整推送方法，ToUser 可以是列表，直接调用即可.
  if (Params.Action = atFileUpShare) and (Params.ToUser <> '') then
    InMessageManager1.PushMsg(Params, Params.ToUser);  // 唤醒或保存到离线文件
end;

procedure TFormInIOCPMsgServer.InFileManager1BeforeDownload(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 打开文件，以备下载
  if Params.Action = atFileDownShare then  // 下载共享文件
    InFileManager1.OpenLocalFile(Result, 'data\public_temp\' + Params.FileName)
  else
    InFileManager1.OpenLocalFile(Result, 'bin\' + Params.FileName);  // 严格来说应打开工作路径的文件
  Result.Msg := '下载文件';
end;

procedure TFormInIOCPMsgServer.InFileManager1BeforeUpload(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 要上传文件，先建文件流
  // 也可以使用这种方法接收：
  // Params.CreateAttachment('存放路径');

  // 推送消息时，如果包含文件的路径，会涉及安全问题
  //   建议 atFileShare 时用一个公用的临时路径存放文件

  if Params.Action = atFileUpShare then
    Params.CreateAttachment('data\public_temp\')
  else
    InFileManager1.CreateNewFile(Params);

  Result.Msg := '上传文件：' + Params.Attachment.FileName;
// 默认返回结果：Result.ActResult := arAccept; 
  
  Memo1.Lines.Add('上传文件：' + Params.Attachment.FileName);
    
end;

procedure TFormInIOCPMsgServer.InHttpDataProvider1Get(Sender: TObject;
  Request: THttpRequest; Respone: THttpRespone);
begin
  // HTTP 服务，返回信息：
  Respone.SetContent('InIOCP HTTP Server v2.5 IS RUNING!');
end;

procedure TFormInIOCPMsgServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := not btnStart.Enabled;
end;

procedure TFormInIOCPMsgServer.InIOCPServer1Disconnect(Sender: TObject; Socket: TBaseSocket);
var
  Result: TReturnResult;
begin
  // Socket 即将被关闭，可能未登录，忽略登出所致的关闭
  if (Socket is TIOCPSocket) then
  begin
    Result := TIOCPSocket(Socket).Result;
    if Assigned(Result) and (Result.Action <> atUserLogout) then  // 不是登出的关闭
    begin
      Result.Msg := '断开连接';
      InMessageManager1.Broadcast(Result);  // 复制结果，广播
    end;
  end;
end;

procedure TFormInIOCPMsgServer.InMessageManager1Broadcast(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 广播：发送消息给全部客户端，除了自己，有权限
  Result.UserName := Params.UserName;
  Result.Msg := '广播消息，已发出.';
  Result.ActResult := arOK;

  // 3 种广播方法：
  InMessageManager1.Broadcast(Params);
//  InMessageManager1.Broadcast(Result);
//  InMessageManager1.Broadcast(Params.Socket, '广播服务端的文本');

end;

procedure TFormInIOCPMsgServer.InMessageManager1Get(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 把离线消息当作附件，返回给客户端
  InMessageManager1.ReadMsgFile(Params, Result);
end;

procedure TFormInIOCPMsgServer.InMessageManager1ListFiles(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 列出离线消息文件名称
  InFileManager1.ListFiles(Params, Result, True);
end;

procedure TFormInIOCPMsgServer.InMessageManager1Push(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
//var
//  oSocket: TIOCPSocket;
begin
  // 推送消息给其他客户端: ToUser、TargetUser

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
  end;    }

  // 2、如果不返回客户端的在线状态，可以直接这样：

  // 2.1 推送给 ToUser（多个客户端以","、";"分隔）
  InMessageManager1.PushMsg(Params, Params.ToUser);  // 缺第二参数时为广播

  // 2.2 第二种推送方法
//  InMessageManager1.PushMsg(Params.Socket, '推送服务端的消息', Params.ToUser);  // 缺第三参数时为广播

  // 返回总体的投放情况
  if (Result.ActResult = arOK) then
    Result.Msg := '投放推送消息成功.'
  else
    Result.Msg := '投放推送消息失败.';

  // 2.3 第三种推送方法
//  InMessageManager1.PushMsg(Result, Params.ToUser);  // 缺第二参数时为广播

end;

procedure TFormInIOCPMsgServer.InMessageManager1Receive(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 收到客户端发来的普通消息
  if (Params.Msg = 'DISCONNECT') then  // 断开指定客户端
  begin
    InClientManager1.Disconnect(Params.ToUser);
    Result.UserName := Params.UserName;
    Result.Msg := '发送断开连接命令.';
    Result.ActResult := arOK;
  end;
end;

end.
