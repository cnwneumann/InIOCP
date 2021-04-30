unit frmInIOCPClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  iocp_base, iocp_msgPacks, iocp_clients;

type                                       
  TFormInIOCPClient = class(TForm)
    InConnection1: TInConnection;
    InMessageClient1: TInMessageClient;
    btnInOut: TButton;
    InCertifyClient1: TInCertifyClient;
    Memo1: TMemo;
    btnLogin2: TButton;
    lbEditIP: TLabeledEdit;
    lbEditPort: TLabeledEdit;
    InFileClient1: TInFileClient;
    procedure FormCreate(Sender: TObject);
    procedure btnInOutClick(Sender: TObject);
    procedure InConnection1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure InCertifyClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InMessageClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InCertifyClient1Certify(Sender: TObject; Action: TActionType;
      ActResult: Boolean);
    procedure InConnection1ReceiveMsg(Sender: TObject; Message: TResultParams);
    procedure btnLogin2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure InConnection1Error(Sender: TObject; const Msg: string);
    procedure InFileClient1ReturnResult(Sender: TObject; Result: TResultParams);
  private
    { Private declarations }
    FLastMsgId: UInt64;
    procedure ListOfflineMsgs(Result: TResultParams);
    procedure SetInConnectionHost;
  public
    { Public declarations }
  end;

var
  FormInIOCPClient: TFormInIOCPClient;

implementation

uses
  iocp_varis, iocp_utils;

{$R *.dfm}

procedure TFormInIOCPClient.btnInOutClick(Sender: TObject);
begin
  // 方法 1：
  // TMessagePack 的宿主为 InConnection1，
  // 在 InConnection1.OnReturnResult 反馈结果

  SetInConnectionHost;

{  if InConnection1.Logined then
    TMessagePack.Create(InConnection1).Post(atUserLogout)
  else
    with TMessagePack.Create(InConnection1) do
    begin
      UserName := 'aaa';
      Password := 'aaa';
      Post(atUserLogin);
    end;           }

  // 方法 2：在 InCertifyClient1.OnReturnResult 反馈结果
  if InConnection1.Logined then
    InCertifyClient1.Logout
  else
    with InCertifyClient1 do
    begin
      Group := 'Group_a';  // 分组
      UserName := 'aaa';
      Password := 'aaa';
      Login;
    end;
end;

procedure TFormInIOCPClient.btnLogin2Click(Sender: TObject);
begin
  SetInConnectionHost;
  if InConnection1.Logined then
    InCertifyClient1.Logout
  else
    with InCertifyClient1 do
    begin
      Group := 'Group_a';  // 分组
      UserName := 'bbb';
      Password := 'bbb';
      Login;
    end;
end;

procedure TFormInIOCPClient.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if InConnection1.Logined then
    InCertifyClient1.Logout;
end;

procedure TFormInIOCPClient.FormCreate(Sender: TObject);
begin
  InConnection1.LocalPath := gAppPath + 'data\data_client';  // 数据目录
  CreateDir(InConnection1.LocalPath);
end;

procedure TFormInIOCPClient.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
begin
  // 登录、登出结果，在 OnReturnResult 之前执行，
  //   可以在 OnReturnResult 中判断客户端状态
  case Action of
    atUserLogin:
      if ActResult then
      begin
        btnInOut.Caption := '登出';
        btnLogin2.Caption := '登出2';
      end;
    atUserLogout: begin
      btnInOut.Caption := '登录';
      btnLogin2.Caption := '登录2';
    end;

    else
      { 无其他操作 } ;
  end;
end;

procedure TFormInIOCPClient.InCertifyClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.Action of
    atUserLogin: begin
      Memo1.Lines.Add(Result.Msg);
      if (Result.ActResult = arOK) then
        InMessageClient1.GetOfflineMsgs; // 取离线消息
      end;
    atUserLogout: begin
      Memo1.Lines.Add(Result.Msg);
      InConnection1.Active := False;
    end;
  end;
end;

procedure TFormInIOCPClient.InConnection1Error(Sender: TObject;
  const Msg: string);
begin
  Memo1.lines.Add(Msg);
end;

procedure TFormInIOCPClient.InConnection1ReceiveMsg(Sender: TObject; Message: TResultParams);
begin
  // 收到其他客户端的推送消息
  // 在服务端使用 InMessageManager1.Broadcast()、PushMsg() 发出
  // Msg.Action、Mag.ActResult 与服务端发出处的一致   
  Memo1.lines.Add('收到推送消息：' +
    Message.PeerIPPort + ', 用户：' + Message.UserName + ', 消息：' +  Message.Msg);

 if Message.Action = atFileUpShare then // 要下载共享文件
   with TMessagePack.Create(InFileClient1) do
   begin
     FileName := Message.FileName; // 服务端文件名
     LocalPath := 'temp';      // 下载存放到 temp 目录
     Post(atFileDownShare);
   end;

end;

procedure TFormInIOCPClient.InConnection1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.Action of
    atUserLogin: begin
      Memo1.Lines.Add(Result.Msg);
      if (Result.ActResult = arOK) then
      begin
        btnInOut.Caption := '登出';
        btnLogin2.Caption := '登出2';
        InMessageClient1.GetOfflineMsgs; // 取离线消息
      end;
    end;

    atUserLogout: begin
      Memo1.Lines.Add(Result.Msg);
      InConnection1.Active := False;
      btnInOut.Caption := '登录';
      btnLogin2.Caption := '登录2';
    end;

    else
      { 其他操作结果 } ;
  end;
end;

procedure TFormInIOCPClient.InFileClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  if Result.Action = atFileUpShare then 
    if Result.ActResult = arOK then
      Memo1.Lines.Add('文件下载完毕');
end;

procedure TFormInIOCPClient.InMessageClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.Action of
    atTextGetMsg:  // 离线消息
      ListOfflineMsgs(Result);
  end;
end;

procedure TFormInIOCPClient.ListOfflineMsgs(Result: TResultParams);
var
  i, k: Integer;
  PackMsg: TReceivePack;  // 接收消息包
  Reader: TMessageReader;  // 离线消息阅读器
begin
  // === 读出离线消息 ===
  
  if not Assigned(Result.Attachment) then
    Exit;

  // 有附件，可能是离线消息，读出！
  Memo1.Lines.Add('离线消息: ' + Result.Msg);

  PackMsg := TReceivePack.Create;
  Reader := TMessageReader.Create;

  try
    // 此时 Attachment 已经关闭，但未释放
    // 打开文件，如果不是消息文件 -> Count = 0
    Reader.Open(Result.Attachment.FileName);

    // 请把以前读过的最大 MsgId 保存到磁盘，
    // 登录前读入并设置 LastMsgId = ???，从离线
    // 消息文件中读出比 LastMsgId 大的消息。

    for i := 0 to Reader.Count - 1 do
    begin
      if Reader.Extract(PackMsg, FLastMsgId) then  // 读出比 LastMsgId 大的消息
      begin
        for k := 0 to PackMsg.Count - 1 do  // 遍历字段
          with PackMsg.Fields[k] do
            Memo1.Lines.Add(Name + '=' + AsString);

        if PackMsg.Action = atFileUpShare then  // 这个是共享文件，下载它
        begin
          Memo1.Lines.Add('下载文件:' + PackMsg.FileName);
          with TMessagePack.Create(InFileClient1) do
          begin
            FileName := PackMsg.FileName;
            LocalPath := 'temp';   // 存放到 temp 目录
            Post(atFileDownShare); // 下载共享文件
          end;
        end;

      end;
    end;

    // 保存最大的消息号
    if PackMsg.MsgId > FLastMsgId then
    begin
      FLastMsgId := PackMsg.MsgId;
      with TStringList.Create do
        try
          Add(IntToStr(FLastMsgId));
          SaveToFile('data\MaxMsgId.ini');
        finally
          Free;
        end;
    end;

  finally
    PackMsg.Free;
    Reader.Free;
  end;
end;

procedure TFormInIOCPClient.SetInConnectionHost;
begin
  if not InConnection1.Active then
  begin
    InConnection1.ServerAddr := lbEditIP.Text;
    InConnection1.ServerPort := StrToInt(lbEditPort.Text);
  end;
end;

end.
