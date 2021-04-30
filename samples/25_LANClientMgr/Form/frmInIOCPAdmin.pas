unit frmInIOCPAdmin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, iocp_clients, iocp_base, ComCtrls;

type
  TFormInIOCPAdmin = class(TForm)
    InCertifyClient1: TInCertifyClient;
    InConnection1: TInConnection;
    InMessageClient1: TInMessageClient;                
    btnLogin: TButton;
    btnBroacast: TButton;
    btnCapScreen: TButton;
    btnDisconnect: TButton;
    btnClose: TButton;
    ActionList1: TActionList;
    actLogin: TAction;
    actBroadcast: TAction;
    actTransmitFile: TAction;
    actDisconnect: TAction;
    actLogout: TAction;
    actClose: TAction;
    btnRegister: TButton;
    btnModify: TButton;
    actRegister: TAction;
    actModify: TAction;
    actSendMsg: TAction;
    btnSendMsg: TButton;
    lvClientView: TListView;
    pgcInfo: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Button1: TButton;
    actBackground: TAction;
    procedure FormCreate(Sender: TObject);
    procedure InCertifyClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure actLoginExecute(Sender: TObject);
    procedure actLogoutExecute(Sender: TObject);
    procedure InCertifyClient1Certify(Sender: TObject; Action: TActionType;
      ActResult: Boolean);
    procedure actBroadcastExecute(Sender: TObject);
    procedure actTransmitFileExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actCloseUpdate(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure InCertifyClient1ListClients(Sender: TObject; Count, No: Cardinal;
      const Client: PClientInfo);
    procedure InConnection1ReceiveMsg(Sender: TObject; Msg: TResultParams);
    procedure actRegisterExecute(Sender: TObject);
    procedure InConnection1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure actModifyExecute(Sender: TObject);
    procedure actSendMsgExecute(Sender: TObject);
    procedure actBackgroundExecute(Sender: TObject);
  private
    { Private declarations }
    function FindClientItem(const ClientName: String): TListItem;
    procedure AddMemoMessage(Msg: TResultParams); 
    procedure AddClientItem(Msg: TResultParams); overload;
    procedure AddClientItem(No: Integer; Info: PClientInfo); overload;
    procedure UpdateClientItem(Item: TListItem; Msg: TResultParams; Login: Boolean);
  public
    { Public declarations }
  end;

var
  FormInIOCPAdmin: TFormInIOCPAdmin;

implementation

uses
  iocp_varis, iocp_utils, frmInIOCPLogin, frmInIOCPRegister;

var
  FormInIOCPLogin: TFormInIOCPLogin = nil;

{$R *.dfm}

procedure TFormInIOCPAdmin.actBackgroundExecute(Sender: TObject);
begin
  // 服务端用后台线程执行，完毕后推送消息，客户端下载一个文件（假设是列出文件的结果）
  // 见：客户端 TFormInIOCPAdmin.InConnection1ReceiveMsg、服务端 InFileManager1QueryFiles
  with TMessagePack.Create(InConnection1) do
  begin
    Msg := '列出文件，假设很耗时。';
    Post(atFileList);  // 列出文件
  end;
end;

procedure TFormInIOCPAdmin.actBroadcastExecute(Sender: TObject);
begin
  // 广播，但不推送给自己
  // 请先登录普通客户端
  with TMessagePack.Create(InConnection1) do
  begin
    Msg := '广播一条消息AAA';
    Post(atTextBroadcast);  // 广播
  end;
end;

procedure TFormInIOCPAdmin.actTransmitFileExecute(Sender: TObject);
begin
  // 上传文件某客户端
  // 请让客户端 aaa 离线、在线，观察客户端消息
   
  with TMessagePack.Create(InConnection1) do
  begin
    Msg := '请下载这个文件，更新本地的相应文件.';
    ToUser := 'aaa,bbb';  // 告诉 aaa,bbb 下载这个文件
    LoadFromFile('bin\sqlite3-w64.dll');
    Post(atFileUpShare);  // 共享给对方
  end;

  // 下载服务端的文件
  // （默认的存放路径：InConnection1.LocalPath）
{  with TMessagePack.Create(InConnection1) do
  begin
    FileName := 'sqlite3-w64.dll';  // 具体位置在服务端解析
    LocalPath := 'temp';   // 指定临时的存放路径, 新版增加
    Post(atFileDownload);  // 下载
  end;

  // 断点上传
  with TMessagePack.Create(InConnection1) do
  begin
    LoadFromFile('F:\Backup\MSOffice2003.7z');
    Post(atFileUpChunk); 
  end;   }

end;

procedure TFormInIOCPAdmin.actCloseExecute(Sender: TObject);
begin
  if InConnection1.Logined then
    InCertifyClient1.Logout;
  if InConnection1.Active then
    InConnection1.Active := False;
  Close;
end;

procedure TFormInIOCPAdmin.actCloseUpdate(Sender: TObject);
begin
  if InConnection1.Logined then
    btnLogin.Action := actLogout   // 登出
  else
    btnLogin.Action := actLogin;   // 登录

  actRegister.Enabled := InConnection1.Logined;
  actBroadcast.Enabled := InConnection1.Logined;
  actTransmitFile.Enabled := InConnection1.Logined;
  actModify.Enabled := InConnection1.Logined;
  actSendMsg.Enabled := InConnection1.Logined;
  actDisconnect.Enabled := InConnection1.Logined;
  actBackground.Enabled := InConnection1.Logined;
  
end;

procedure TFormInIOCPAdmin.actDisconnectExecute(Sender: TObject);
begin
  // 断开，无对应的操作 TActionType，
  // 可以发一条普通文本消息到服务端，服务端根据消息含义中断特定客户端
  with TMessagePack.Create(InConnection1) do
  begin
    Msg := 'DISCONNECT';  // 断开
    ToUser := 'aaa';      // 断开 aaa
    Post(atTextSend);     // 发送文本消息
  end;
end;

procedure TFormInIOCPAdmin.actLoginExecute(Sender: TObject);
begin
  // 登录
  FormInIOCPLogin := TFormInIOCPLogin.Create(Self);
  with FormInIOCPLogin do
  begin
    FConnection := InConnection1;
    FCertifyClient := InCertifyClient1;
    ShowModal;
    if InConnection1.Logined then
    begin
      lvClientView.Items.Clear;  // 清除客户端列表
      InCertifyClient1.QueryClients;  // 查询全部连接过的客户端
    end;
    Free;
  end;
end;

procedure TFormInIOCPAdmin.actLogoutExecute(Sender: TObject);
begin
  // 登出
  lvClientView.Items.Clear;
  InCertifyClient1.Logout;  // 服务端 SQL 名称 [USER_LOGOUT]
end;

procedure TFormInIOCPAdmin.actModifyExecute(Sender: TObject);
begin
  // 服务端会检查登录用户的权限，权限大才能删除

{  with TMessagePack.Create(InConnection1) do
  begin
    ToUser := 'ADDNEW';  // 要修改的用户 = TargetUser
    AsString['user_password'] := 'aaa';
    AsInteger['user_level'] := 1;
    AsString['user_real_name'] := '实名';
    AsString['user_telephone'] := '01023456789';
    Post(atUserModify);  // 修改
  end;   }

  // 注意：UserName 是登录用户名，不能删除自己
  with TMessagePack.Create(InConnection1) do
  begin
    ToUser := 'ADDNEW';  // 要删除的用户 = TargetUser
    Post(atUserDelete);  // 删除
  end;

end;

procedure TFormInIOCPAdmin.actRegisterExecute(Sender: TObject);
begin
  // 注册
  pgcInfo.ActivePageIndex := 1;
  with TFormInIOCPRegister.Create(Self) do
  begin
    FConnection := InConnection1;
    ShowModal;
    Free;
  end;
end;

procedure TFormInIOCPAdmin.actSendMsgExecute(Sender: TObject);
begin
  // 请先登录普通客户端
  with TMessagePack.Create(InConnection1) do
  begin
    ToUser := 'aaa,bbb';  // 目的用户 = TargetUser，可以推送给多用户: aaa,bbb,ccc
    Msg := '推送一条消息';
    Post(atTextPush); // 提交
  end;
end;

procedure TFormInIOCPAdmin.AddClientItem(Msg: TResultParams);
var
  Item: TListItem;
begin
  // 客户端登录：加入信息
  Item := lvClientView.Items.Add;
  Item.Caption := IntToStr(Item.Index + 1);
  Item.SubItems.Add(Msg.UserGroup + ':' + Msg.UserName);
  Item.SubItems.Add(Msg.PeerIPPort);
  Item.SubItems.Add(IntToStr(Integer(Msg.Role)));
  Item.SubItems.Add(DateTimeToStr(Msg.AsDateTime['action_time']));
  Item.SubItems.Add('-');
end;

procedure TFormInIOCPAdmin.AddClientItem(No: Integer; Info: PClientInfo);
var
  Item: TListItem;
begin
  // 查询客户端：加入信息（见 TClientInfo）
  Item := lvClientView.Items.Add;
  Item.Caption := IntToStr(No);
  Item.SubItems.Add(Info^.Group + ':' + Info^.Name);
  Item.SubItems.Add(Info^.PeerIPPort);
  Item.SubItems.Add(IntToStr(Integer(Info^.Role)));
  Item.SubItems.Add(DateTimeToStr(Info^.LoginTime));
  if (Info^.LogoutTime = 0) then
    Item.SubItems.Add('-')
  else
    Item.SubItems.Add(DateTimeToStr(Info^.LogoutTime));
end;

procedure TFormInIOCPAdmin.AddMemoMessage(Msg: TResultParams);
begin
  Memo1.Lines.Add(DateTimeToStr(Now) + '>' +
                  Msg.PeerIPPort + ',' + Msg.UserName + ',' + Msg.Msg);
end;

function TFormInIOCPAdmin.FindClientItem(const ClientName: String): TListItem;
var
  i: Integer;
begin
  for i := 0 to lvClientView.Items.Count - 1 do
  begin
    Result := lvClientView.Items[i];
    if (Result.SubItems[0] = ClientName) then  // 找到客户端
      Exit;  // 返回
  end;
  Result := Nil;
end;

procedure TFormInIOCPAdmin.FormCreate(Sender: TObject);
begin
  InConnection1.LocalPath := gAppPath + 'data\data_admin';  // 文件存放路径
  CreateDir(InConnection1.LocalPath);
end;

procedure TFormInIOCPAdmin.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
begin
  if not InConnection1.Logined then // 此时 Action = atUserLogout and ActResult = True
    InConnection1.Active := False;  // 同时断开
end;

procedure TFormInIOCPAdmin.InCertifyClient1ListClients(Sender: TObject; Count,
  No: Cardinal; const Client: PClientInfo);
var
  Item: TListItem;
begin
  // 列出全部连接过的客户端信息
  if (Client^.LogoutTime = 0) then  // 在线已登录的客户端
  begin
    Item := FindClientItem(Client^.Group + ':' + Client^.Name);
    if Assigned(Item) then  // 更新客户端信息
      Item.Caption := IntToStr(No)
    else  // 加入客户端信息（见 TClientInfo）
      AddClientItem(No, Client);
  end;
end;

procedure TFormInIOCPAdmin.InCertifyClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 返回 InCertifyClient1 的操作结果
  case Result.Action of
    atUserLogin: begin
      if InConnection1.Logined then
        FormInIOCPLogin.Close;
      AddMemoMessage(Result);
    end;
    atUserQuery:
      { 已在 OnListClients 中列出客户端 } ;
  end;
end;

procedure TFormInIOCPAdmin.InConnection1ReceiveMsg(Sender: TObject; Msg: TResultParams);
var
  Item: TListItem;
begin
  // 客户端登录时，服务端要推送足够多的信息
  case Msg.Action of
    atUserLogin: begin
      Item := FindClientItem(Msg.UserGroup + ':' + Msg.UserName);
      if Assigned(Item) then
        UpdateClientItem(Item, Msg, True)
      else
        AddClientItem(Msg);
    end;
    atUserLogout: begin
      Item := FindClientItem(Msg.UserGroup + ':' + Msg.UserName);
      if Assigned(Item) then
        UpdateClientItem(Item, Msg, False);
      // 也可以清除端列表，重新查询（最保险，但延迟）
      // lvClientView.Items.Clear;  // 清除客户端列表
      // InCertifyClient1.QueryClients;   // 查询全部已连接的客户端
    end;
    atFileList: begin
      Memo1.Lines.Add('服务端用后台执行，唤醒客户端了，可以在此下载服务端的结果.');
      with TMessagePack.Create(InConnection1) do
      begin
        LocalPath := 'temp';  // 临时的文件存放路径
        FileName := 'sqlite运行库.txt';  // 下载后台执行的结果文件!
        Post(atFileDownload);
      end;
    end;
  end;
  // 加入客户端活动 memo
  AddMemoMessage(Msg);
end;

procedure TFormInIOCPAdmin.InConnection1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 加入客户端活动 memo
  AddMemoMessage(Result);
end;

procedure TFormInIOCPAdmin.UpdateClientItem(Item: TListItem; Msg: TResultParams; Login: Boolean);
begin
  // 更新客户端信息
  if Login then  // 登录
  begin
    Item.SubItems[1] := Msg.PeerIPPort;
    Item.SubItems[2] := IntToStr(Integer(Msg.Role));
    Item.SubItems[3] := DateTimeToStr(Msg.AsDateTime['action_time']);
    Item.SubItems[4] := '-';
  end else
    Item.SubItems[4] := DateTimeToStr(Msg.AsDateTime['action_time']);
end;

end.
