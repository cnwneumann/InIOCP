unit frmTestIOCPServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, DateUtils,
  Forms, iocp_zlib, Dialogs, StdCtrls, SyncObjs, ExtCtrls, iocp_base, iocp_log,
  iocp_server, iocp_msgPacks, iocp_sockets, iocp_objPools, IniFiles, iocp_md5,
  iocp_clients, iocp_managers, fmIOCPSvrInfo, Grids, iocp_sqlMgr,
  DBGrids, ComCtrls, DB, DBClient, Provider, http_base, http_objects,
  iocp_wsClients;

type
  TFormTestIOCPServer = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    mmoServer: TMemo;
    edtHost: TEdit;
    chkSilence: TCheckBox;
    edtTo: TEdit;
    btnSendTo: TButton;
    InIOCPServer1: TInIOCPServer;
    InDatabaseManager1: TInDatabaseManager;
    InClientManager1: TInClientManager;
    InFileManager1: TInFileManager;
    InConnection1: TInConnection;
    btnConnect: TButton;
    btnEcho: TButton;
    InCertifyClient1: TInCertifyClient;
    btnLoginx: TButton;
    InMessageClient1: TInMessageClient;
    btnDownload: TButton;
    btnClearMemo: TButton;
    btnListClient: TButton;
    InConnection2: TInConnection;
    InCertifyClient2: TInCertifyClient;
    InMessageClient2: TInMessageClient;
    btnCon: TButton;
    btnLog: TButton;
    btnCast: TButton;
    InCustomClient1: TInCustomClient;
    InEchoClient1: TInEchoClient;
    InCustomManager1: TInCustomManager;
    btnCustmSnd: TButton;
    InFileClient1: TInFileClient;
    InFileClient2: TInFileClient;
    btnSndTo2: TButton;
    edtTo2: TEdit;
    btnUpload: TButton;
    btnQueryDir: TButton;
    bvl2: TBevel;
    btn1: TButton;
    btnExecSQL: TButton;
    btnExecQuery: TButton;
    btnCast2: TButton;
    btnSetDir: TButton;
    InFunctionClient1: TInFunctionClient;
    btnCallFunc: TButton;
    edtPort: TEdit;
    btnCancel: TButton;
    InRemoteFunctionGroup1: TInRemoteFunctionGroup;
    InDBConnection1: TInDBConnection;
    ComboBox1: TComboBox;
    btnDBConnection: TButton;
    InDBQueryClient1: TInDBQueryClient;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    mmoClient: TMemo;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ts1: TTabSheet;
    lbl1: TLabel;
    lblFileB: TLabel;
    lblCancelB: TLabel;
    lbl2: TLabel;
    lblTarget: TLabel;
    lblTargetCancel: TLabel;
    InHttpDataProvider1: TInHttpDataProvider;
    TabSheet3: TTabSheet;
    MemoDMInfos: TMemo;
    edtDMNo: TEdit;
    DeleteDM: TButton;
    listDM: TButton;
    addDM: TButton;
    InMessageManager1: TInMessageManager;
    Button1: TButton;
    Timer1: TTimer;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    InWebSocketManager1: TInWebSocketManager;
    InWSConnection1: TInWSConnection;
    btnWSConnect: TButton;
    btnWSSendFiles: TButton;
    btnWSListFiles: TButton;
    InDBSQLClient1: TInDBSQLClient;
    ClientDataSet1: TClientDataSet;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnListClientClick(Sender: TObject);
    procedure btnSendToClick(Sender: TObject);
    procedure edtHostDblClick(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure InConnection1AfterConnect(Sender: TObject);
    procedure btnEchoClick(Sender: TObject);
    procedure btnLoginxClick(Sender: TObject);
    procedure InCertifyClient1ListClients(Sender: TObject; Count, No: Cardinal;
              const Client: PClientInfo);
    procedure InMessageManager1Receive(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure btnConClick(Sender: TObject);
    procedure btnLogClick(Sender: TObject);
    procedure btnCastClick(Sender: TObject);
    procedure InClientManager1Login(Sender: TObject;
              Params: TReceiveParams; Result: TReturnResult);
    procedure InClientManager1Logout(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure btnClearMemoClick(Sender: TObject);
    procedure btnCustmSndClick(Sender: TObject);
    procedure InCustomClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InCertifyClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InCustomManager1Receive(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1Delete(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1Register(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InClientManager1Modify(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InConnection2AfterConnect(Sender: TObject);
    procedure btnSndTo2Click(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure btnUploadClick(Sender: TObject);
    procedure InFileClient1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure InFileClient1Request(Sender: TObject; Action: TActionType);
    procedure InFileManager1BeforeDownload(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InFileManager1BeforeUpload(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InConnection2TimeOut(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btnCast2Click(Sender: TObject);
    procedure btnQueryDirClick(Sender: TObject);
    procedure InFileClient2ReturnResult(Sender: TObject; Result: TResultParams);
    procedure btnSetDirClick(Sender: TObject);
    procedure btnCallFuncClick(Sender: TObject);
    procedure InFunctionClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure FormDestroy(Sender: TObject);
    procedure InFileManager1QueryFiles(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InMessageClient2ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InMessageClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure btnCancelClick(Sender: TObject);
    procedure InRemoteFunctionGroup1Execute(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InClientManager1QueryState(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InFileManager1DeleteFile(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InFileManager1RenameFile(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure btnDBConnectionClick(Sender: TObject);
    procedure InDBConnection1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure btnExecQueryClick(Sender: TObject);
    procedure btnExecSQLClick(Sender: TObject);
    procedure InDBQueryClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure InDBSQLClient1ReturnResult(Sender: TObject;
      Result: TResultParams);
    procedure InFileClient2ListFiles(Sender: TObject; ActResult: TActionResult;
      No: Integer; Result: TCustomPack);
    procedure InFileClient2WaitForAnswer(Sender: TObject;
      Result: TResultParams);
    procedure InCertifyClient1Certify(Sender: TObject; Action: TActionType;
      ActResult: Boolean);
    procedure InHttpDataProvider1Get(Sender: TObject; Request: THttpRequest;
      Respone: THttpRespone);
    procedure InHttpDataProvider1Post(Sender: TObject;
      Request: THttpRequest; Respone: THttpRespone);
    procedure InFileManager1AfterUpload(Sender: TObject;
                 Params: TReceiveParams; Document: TIOCPDocument);
    procedure InFileManager1AfterDownload(Sender: TObject;
                 Params: TReceiveParams; Document: TIOCPDocument);
    procedure InHttpDataProvider1InvalidSession(Sender: TObject;
      Request: THttpRequest; Respone: THttpRespone);
    procedure InIOCPServer1DataSend(Sender: TBaseSocket; Size: Cardinal);
    procedure InIOCPServer1DataReceive(Sender: TBaseSocket;
      const Data: PAnsiChar; Size: Cardinal);
    procedure InHttpDataProvider1Accept(Sender: TObject; Request: THttpRequest;
      var Accept: Boolean);
    procedure InHttpDataProvider1ReceiveFile(Sender: TObject;
      Request: THttpRequest; const FileName: String; Data: PAnsiChar;
      DataLength: Integer; State: THttpPostState);
    procedure listDMClick(Sender: TObject);
    procedure addDMClick(Sender: TObject);
    procedure DeleteDMClick(Sender: TObject);
    procedure InEchoClient1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure InMessageClient1MsgReceive(Sender: TObject; Msg: TResultParams);
    procedure InConnection1ReceiveMsg(Sender: TObject; Msg: TResultParams);
    procedure InMessageManager1Broadcast(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InConnection2ReceiveMsg(Sender: TObject; Msg: TResultParams);
    procedure InMessageManager1Push(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InCustomManager1AttachBegin(Sender: TObject;
      Params: TReceiveParams);
    procedure InCustomManager1AttachFinish(Sender: TObject;
      Params: TReceiveParams);
    procedure InConnection1Error(Sender: TObject; const Msg: string);
    procedure Button1Click(Sender: TObject);
    procedure InConnection1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure InMessageManager1Get(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure Timer1Timer(Sender: TObject);
    procedure InFileManager1SetWorkDir(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InConnection2ReturnResult(Sender: TObject; Result: TResultParams);
    procedure InConnection1DataReceive(Sender: TObject; MsgId, MsgSize,
      CurrentSize: Int64);
    procedure InConnection1DataSend(Sender: TObject; MsgId, MsgSize,
      CurrentSize: Int64);
    procedure InWebSocketManager1Upgrade(Sender: TObject; const Origin: string;
      var Accept: Boolean);
    procedure InWebSocketManager1Receive(Sender: TObject; Socket: TWebSocket);
    procedure btnWSConnectClick(Sender: TObject);
    procedure btnWSSendFilesClick(Sender: TObject);
    procedure InWSConnection1ReceiveMsg(Sender: TObject; Msg: TJSONResult);
    procedure InWSConnection1ReceiveData(Sender: TObject; const Msg: string);
    procedure InWSConnection1ReturnResult(Sender: TObject; Result: TJSONResult);
    procedure btnWSListFilesClick(Sender: TObject);
    procedure InWSConnection1AfterConnect(Sender: TObject);
    procedure InIOCPServer1Connect(Sender: TObject; Socket: TBaseSocket);
    procedure InIOCPServer1Disconnect(Sender: TObject; Socket: TBaseSocket);
    procedure InMessageManager1ListFiles(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
  private
    { Private declarations }
    FAppDir: String;
    FCount: Cardinal;
    FLastMsgId: UInt64;

    FSvrSocket: TServerSocket;
    FFileName: String;
    FFromClient: String;
    FToUserName: String;
    procedure AddMessage(Memo: TMemo; const Msg: String);
  public
    { Public declarations }
  end;

var
  FormTestIOCPServer: TFormTestIOCPServer;

implementation

uses
  { frmInSQLMgrEditor, }
  TypInfo, DSIntf, iocp_Lists, iocp_utils, iocp_varis, http_utils,
  dm_iniocp_test, iocp_threads, iocp_mmHash, iocp_SHA1;

{$R *.dfm}

procedure TFormTestIOCPServer.addDMClick(Sender: TObject);
begin
  // 增加一个数模：类名 + 描述
  InDatabaseManager1.AddDataModule(TdmInIOCPTest, 'SQLite3_xzqh_add');

  // 卸载一个数模，用新的代替：编号 + 类名 + 描述
//  InDatabaseManager1.RemoveDataModule(1);
//  InDatabaseManager1.ReplaceDataModule(1, TdmSQLite3, 'SQLite3_xzqh_replace');
end;

procedure TFormTestIOCPServer.AddMessage(Memo: TMemo; const Msg: String);
begin
  // ++
  if (Memo = mmoClient) or (chkSilence.Checked = False) then
  begin
    Inc(FCount);
    Memo.Lines.Add(IntToStr(FCount) + '-> ' + Msg);
  end;
end;

procedure TFormTestIOCPServer.btnStartClick(Sender: TObject);
begin
  // 开启日志

  iocp_log.TLogThread.InitLog(FAppDir + 'log');

  iocp_utils.IniDateTimeFormat;   // 设置时间格式

  if edtHost.Text <> '' then
    InIOCPServer1.ServerAddr := edtHost.Text;
  if edtPort.Text <> '' then
    InIOCPServer1.ServerPort := StrToInt(edtPort.Text);

  // 从 TInIOCPDataModule 继承新的数模 TdmInIOCPTest，
  //    注册数模类型即可（系统自动建数模实例）
  //    停止服务时会清空数模列表
  InDatabaseManager1.AddDataModule(TdmInIOCPTest, 'ADO_xzqh');
  InDatabaseManager1.AddDataModule(TdmInIOCPTest, 'ADO_xzqh2');   // 用其他名称再注册一次

  InIOCPServer1.Active := True;
  FrameIOCPSvrInfo1.Start(InIOCPServer1);

  btnStart.Enabled := False;  
end;

procedure TFormTestIOCPServer.btnStopClick(Sender: TObject);
begin
  // ++
  FrameIOCPSvrInfo1.Stop;         // 停止统计
  InIOCPServer1.Active := False;  // 停止服务
  iocp_log.TLogThread.StopLog;    // 停止日志
  mmoServer.Lines.Clear; // .Add(IntToStr(FBuffersCount)); // .Clear;
end;

procedure TFormTestIOCPServer.btnUploadClick(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // 上传文件（支持大文件，> 2G）
  Msg := TMessagePack.Create(InConnection2);

  // 要上传的文件属附件，不能用方法：
  //  Msg.FileName := 'DAEMON_Tools_Lite_green.rar'

  Msg.LoadFromFile('F:\Backup\Ghost\winxp20161209.GHO'); // DAEMON_Tools_Lite_green.rar
//  Msg.CheckType := ctMurmurHash;  // ctMD5，校验方法

  // 分块上传
  // 自动在文件路径建一个传输信息文件 .upload，传完删除

  Msg.Post(atFileUpload); // atFileUpChunk

//  InFileClient2.Upload('DAEMON_Tools_Lite_green.rar');

end;

procedure TFormTestIOCPServer.Button1Click(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // 用新版的 TMessagePack 发送、提取消息
  // 全部客户端组件的属性 Params 都与 TMessagePack 同源

  // 消息被提交到发送线程，发送完毕后会自动释放

  // 客户端接收时，每完整接收一个消息即塞到投放线程 TPostThread 中，
  // 如果要上传附件，则暂不投放，叫醒发送线程上传附件，等再次收到
  // 服务器反馈才真正投放（保证只有一次进入应用层）。

  // 返回结果触发对应的宿主事件 OnReturnResult
  //   宿主：InConnection1 -> 返回结果在 InConnection1.OnReturnResult
  //         InMessageClient1 -> 返回结果在 InMessageClient1.OnReturnResult ...

  // 注：如果在广播或同时接收推送消息的情况下出现异常，
  //     很可能是客户端接收器的问题，接收代码有隐患，有没考虑的特殊情况。

  // 请先连接 InConnection1、InConnection2！

  // ======== 广播消息
  // 自身会收到，客户端接收数据的方法相对复杂一点

//  FDebug.Clear;
//  FStream.Clear;

  Msg := TMessagePack.Create(InConnection2);
  Msg.Msg := '这是连接 2 的广播消息';
  Msg.AsString['text'] := '加一个文本';

  Msg.Post(atTextBroadcast);  // 广播一条消息

  Msg := TMessagePack.Create(InConnection2);
  Msg.Msg := '这是连接 2 的广播消息 bbbb';
  Msg.DateTime := Now;  // 增加时间

  Msg.Post(atTextBroadcast);

  // ======== 登录

  Msg := TMessagePack.Create(InCertifyClient1);
  Msg.UserName := 'aaaa';
  Msg.Password := 'ppp';

  // 一样可以加附件
  // Msg.LoadFromFile('DAEMON_Tools_Lite_green.rar');

  Msg.Post(atUserLogin);

  // ======== 发送消息到服务器

  Msg := TMessagePack.Create(InConnection1);
  Msg.Msg := '这是 TMessagePack 消息';

  Msg.Post(atTextSend);

  // ======== 取离线消息
  // 宿主：InCertifyClient1
  // 在 InCertifyClient1.OnReturnResult 中打开离线消息文件，读出

  Msg := TMessagePack.Create(InCertifyClient1);
  Msg.Post(atTextGetMsg);

  // ======== 下载文件（在服务端的当前工作路径）

  Msg := TMessagePack.Create(InConnection1);
  Msg.FileName := 'DAEMON_Tools_Lite_green.rar'; // 'autoClick.7z'; // 'DAEMON_Tools_Lite_green.rar';

  Msg.Post(atFileDownload);

  // ======== 上传文件

  // 上传附件：
  //   除响应服务，全部消息都可以带附件.
  
  // 服务端只公开文件、数据流的附件事件，
  // 在不公开事件的管理组件中，可以这样接收附件：
  //   if Params.AttachSize > 0 then
  //      Params.CreateAttachment('save_to_path\');

  Msg := TMessagePack.Create(InConnection1);

  // 要上传的文件属附件，不能用方法：
  //  Msg.FileName := 'DAEMON_Tools_Lite_green.rar'

  Msg.LoadFromFile('DAEMON_Tools_Lite_green.rar');
  Msg.Post(atFileUpload);

  // ======== 上传数据流
  //  服务端在 InCustomManager1 管理
  //  服务端的其他管理组件的接收流事件未公开，
  //  可以这样接收（当作文件流）：
  //   if Params.AttachSize > 0 then
  //      Params.CreateAttachment('save_to_path\');

  Msg := TMessagePack.Create(InConnection1);

  Msg.AsString['中文变量'] := '测试中文....';
  
  // 要上传的流也是附件，服务端在 InCustomManager1 管理，
  //   系统把流当作 _stream.strm 文件，见 TBaseMessage.LoadFromStream

  Msg.LoadFromStream(TFileStream.Create('测试2.txt', 0), True);  // 压缩上传

  Msg.Post(atCustomAction);

end;

procedure TFormTestIOCPServer.btnWSConnectClick(Sender: TObject);
begin
  InWSConnection1.ServerPort := StrToInt(edtPort.Text);
  InWSConnection1.Active := True;
end;

procedure TFormTestIOCPServer.btnWSSendFilesClick(Sender: TObject);
var
  JSON: TJSONMessage;
begin

 Inc(FCount);

  // 方法 1：
{  with InWSConnection1.JSON do
  begin
    S['aaa'] := 'WebSocket test + 中文.';
    S['BBB'] := 'ba +中文';
    Post;
  end;     }

  // 方法 2：
  JSON := TJSONMessage.Create(InWSConnection1);
  JSON.S['aaa'] := '测试 InIOCP 的 WebSocket 客户端，中文.';
  JSON.S['BBB'] := '中';

  JSON.Attachment := TFileStream.Create('autoClick.7z', fmOpenRead);
  JSON.S['attach'] := IntToStr(FCount) + '_autoClick.7z';  // 给附件命名，方便服务端保存

  JSON.Post;

end;

procedure TFormTestIOCPServer.btnWSListFilesClick(Sender: TObject);
begin
  // 先查询服务端 Form 下的文件
  // InIOCP-JSON 支持记录对象 R[]，每文件当中一条记录，
  //   见单元 iocp_WsJSON，iocp_managers 的 TInFileManager.ListFiles
  with InWSConnection1.JSON do
  begin
    Action := 20;
    S['Path'] := 'form\';
    Post;
  end;
end;

procedure TFormTestIOCPServer.DeleteDMClick(Sender: TObject);
begin
  // 删除一个数模，编号：edtDMNo.Text
  InDatabaseManager1.RemoveDataModule(StrToInt(edtDMNo.Text));
end;

procedure TFormTestIOCPServer.btnCallFuncClick(Sender: TObject);
begin
  // 执行远程函数：组名, 编号
//  FDebug.Clear;
//  FStream.Clear;
  InFunctionClient1.Call('TEST_GROUP', 1);
end;

procedure TFormTestIOCPServer.btnCancelClick(Sender: TObject);
begin
  // 取消连接内的全部任务
  InConnection2.CancelAllWorks;
end;

procedure TFormTestIOCPServer.btnCast2Click(Sender: TObject);
begin
  // ++ 发送消息给全部在线客户端（用原方法，未改进）
  //    管理员才能广播！  
  InMessageClient2.Broadcast('这是 USER_B 的广播消息.');
end;

procedure TFormTestIOCPServer.edtHostDblClick(Sender: TObject);
begin
  // ++
  if edtHost.Text = '192.168.1.128' then
    edtHost.Text := '192.168.1.196'
  else
    edtHost.Text := '192.168.1.128';
end;

procedure TFormTestIOCPServer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ClientDataSet1.Active := False;
  
  InConnection1.Active := False;
  InConnection2.Active := False;

  FrameIOCPSvrInfo1.Stop;
  InIOCPServer1.Active := False;

  iocp_log.TLogThread.StopLog;      // 停止日志
end;

procedure TFormTestIOCPServer.FormCreate(Sender: TObject);
var
  Strs: TStrings;
begin
  // ++
  // win7 注册表修改，取消TCP连接数的限制
  // Windows 下单机最大TCP连接数
  // https://www.cnblogs.com/eaglet/archive/2010/09/21/1832233.html
  // http://blog.csdn.net/houhu_cs/article/details/5315077

//  _ResetMMProc := ScaleMM2.ResetScaleMM;

  FCount := 0;
  FLastMsgId := 0;  // 收到的推送消息最大编号
//  edtHost.Text := GetLocalIP();   // '127.0.0.1';   //

  // 准备路径
  FAppDir := ExtractFilePath(Application.ExeName);

  // 程序路径
  iocp_Varis.gAppPath := FAppDir;

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

  MyCreateDir(AddBackslash(InHttpDataProvider1.RootDirectory) + 'uploads');

end;

procedure TFormTestIOCPServer.FormDestroy(Sender: TObject);
begin
//  CliDebug.Free;

end;

procedure TFormTestIOCPServer.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
var
  UserName: String;
  btnButton: TButton;
begin
  // ++
  // 登录/登出返回事件
  UserName := TInCertifyClient(Sender).UserName;
  if Sender = InCertifyClient1 then
    btnButton := btnLoginx
  else
    btnButton := btnLog;

  case Action of
    atUserLogin:
      if ActResult then
      begin
        AddMessage(mmoClient, UserName + ': 登录成功.');
        btnButton.Caption := 'Logout';
      end else
      begin
        AddMessage(mmoClient, UserName + ': 登录失败.');
        btnButton.Caption := 'Login';
      end;
    atUserLogout: begin
      AddMessage(mmoClient, UserName + ': 客户端退出.');
      btnButton.Caption := 'Login';
    end;
  end;
end;

procedure TFormTestIOCPServer.InCertifyClient1ListClients(Sender: TObject;
          Count, No: Cardinal; const Client: PClientInfo);
begin
  // ++
  AddMessage(mmoClient, IntToStr(No) + '/' + IntToStr(Count) + ', ' +
             Client^.Name + '  ->  ' + IntToStr(Cardinal(Client^.Socket)) + ', ' +
             Client^.PeerIPPort + ', ' + DateTimeToStr(Client^.LoginTime) + ', ' +
             DateTimeToStr(Client^.LogoutTime));
end;

procedure TFormTestIOCPServer.InCertifyClient1ReturnResult(Sender: TObject; Result: TResultParams);
var
  i, k: Integer;
  Msg: TReceivePack;  // 接收消息包
  Reader: TMessageReader;  // 离线消息阅读器
begin
  if Result.Action = atUserState then
  begin
    if Result.ActResult = arOnline then
      AddMessage(mmoClient, '查询: ' + Result.UserName + ' 在线')
    else
      AddMessage(mmoClient, '查询: ' + Result.UserName + ' 离线')
  end else
  begin
    AddMessage(mmoClient, '返回消息: ' + Result.Msg);

    // === 读离线消息 ===

    // 有附件，可能是离线消息，读出！
    if Assigned(Result.Attachment) then
    begin
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
                mmoClient.Lines.Add(Name + '=' + AsString);
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
end;

procedure TFormTestIOCPServer.InClientManager1Delete(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
var
  DeleName: String;
  oSocket: TIOCPSocket;
begin
  // 删除一个用户
  // 见：TInCertifyClient.Delete

  DeleName := Params.ToUser;  // 待删用户，2.0 改为 ToUser、TargetUser

  // 1. 判断是否存在，不存在返回 arFail
  // SELECT Role FROM tbl_xxx WHERE userName = '''' + DeleName + ''''

  // Result.Msg := '删除用户失败';
  //  Result.ActResult := arFail;

  // 2. 存在 -> 比较权限，命令发出者权限更高才能删除，否则返回 arFail
  // if (Sender.Data^.BaseInf.Role > 字段 Role) then

  // 3. 从数据库删除 DeleName，如果它在线，发出被删除消息

  // DELETE FROM tbl_xxx WHERE userName = '''' + DeleName + ''''

  InClientManager1.Disconnect(DeleName);

  Result.Msg := '删除用户成功';
  Result.ActResult := arOK;
  
end;

procedure TFormTestIOCPServer.InClientManager1Login(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // Sender：TBusiWorker 实例，有全局锁 GlobalLock 属性
  // Params: 客户端发来的变量表
  // Result: 返回给客户端的结果

  mmoServer.Lines.Add('login');

  if Params.Password <> '' then
  begin
    Result.Msg := 'Login OK';
    Result.Role := crAdmin;   // 测试广播要用的权限
    Result.ActResult := arOK;

    // 登记信息在前：设 Role
    // 保存登录名、用户的数据路径（注册时建）等信息到 Socket.Evir
    //   Params.UserName 会被保存到 Socket.LogName 中
    InClientManager1.Add(Params.Socket, crAdmin);

    // 把离线消息当作附件加入（可能含文件互传消息）
    // 可以在客户端发送 arTextGet 取离线消息，见：InMessageManager1.OnGet
    InMessageManager1.ReadMsgFile(Params, Result);
  end else
  begin
    Result.Msg := 'Login Fail';
    Result.ActResult := arFail;   // 返回 arErrUser 会断开连接
  end;

end;

procedure TFormTestIOCPServer.InClientManager1Logout(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 登出，内部自动删除登录信息（客户端不断开）
  AddMessage(mmoServer, 'Server: 客户端退出, ' + Params.UserName);
  Result.Msg := Params.UserName + '，你成功登出.';   // 返回一点内容
end;

procedure TFormTestIOCPServer.InClientManager1Modify(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 修改用户的登录密码、权限（不能改名称，下次登录生效）
  // 方法：
  //   UPDATE xxx SET
  //     passwrod = Params.Password, Role = Integer(Params.role)
  //   WHERE user_name = Params.UserName
  // 调用：
  // TBusiWorker(Sender).DataModule.ExecSQL(Params, Result);
end;

procedure TFormTestIOCPServer.InClientManager1QueryState(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 查询用户是否在线
  // 1. 先从数据库等用户表检查是否有此用户, 没有则返回 arMissing,
  // 2. 如果用户名称来自 QueryClients 的查询则可以不查询数据库，省时
  
  // if xxx then
  //   Result.ActResult := arMissing
  // else  // 2.0 改为 ToUser/TargetUser，UserName 是命令发出者
       InClientManager1.GetClientState(Params.ToUser, Result);

end;

procedure TFormTestIOCPServer.InClientManager1Register(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 注册新用户到数据库（未连接登录）

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

procedure TFormTestIOCPServer.InConnection1AfterConnect(Sender: TObject);
begin
  // ++
  if InConnection1.Active then
  begin
    btnConnect.Caption := 'Disconnect';
  end else
  begin
    btnConnect.Caption := 'Connect';
    btnLoginx.Caption := 'Login';
  end;
end;

procedure TFormTestIOCPServer.InConnection1DataReceive(Sender: TObject; MsgId,
  MsgSize, CurrentSize: Int64);
begin
  // 消息接收进程
  //  MsgSize：当前消息大小
  //  CurrentSize：当前消息累计收到长度
//  mmoClient.Lines.Add('接收: ' + IntToStr(CurrentSize) + '/' + IntToStr(MsgSize));

end;

procedure TFormTestIOCPServer.InConnection1DataSend(Sender: TObject; MsgId,
  MsgSize, CurrentSize: Int64);
begin
  // 消息发送进程
  //  MsgSize：当前消息大小
  //  CurrentSize：当前消息累计发出长度
//  mmoClient.Lines.Add('发送: ' + IntToStr(CurrentSize) + '/' + IntToStr(MsgSize));

end;

procedure TFormTestIOCPServer.InConnection1Error(Sender: TObject; const Msg: string);
begin
  // 出现异常，有几种异常会自动断开连接：
  // 见：TInConnection.DoServerError、TInConnection.DoThreadFatalError
  AddMessage(mmoClient, Msg);
end;

procedure TFormTestIOCPServer.InConnection1ReceiveMsg(Sender: TObject;
  Msg: TResultParams);
//var
//  FileName: AnsiString;
begin
  // 收到其他客户端的推送消息！
  // 统一把推送消息放到连接的 ReceiveMsg 事件中处理

  // 更新最大消息号！
  FLastMsgId := Msg.MsgId;

  AddMessage(mmoClient, 'USER_A 收到客户消息：' +
             Msg.Msg + ', 来自:' + Msg.UserName);

end;


procedure TFormTestIOCPServer.InConnection1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 收到服务端通知
  case Result.ActResult of  // 目前只有 2 种情况
    arOutDate:
      mmoClient.Lines.Add('凭证过期.');
    arOffline:
      mmoClient.Lines.Add(Result.UserName + '用户未登录.');
  end;
end;

procedure TFormTestIOCPServer.InConnection2AfterConnect(Sender: TObject);
begin
  // ++
  if InConnection2.Active then
  begin
    btnCon.Caption := 'Disconnect';
  end else begin
    btnCon.Caption := 'Connect';
    btnLog.Caption := 'Login';
  end;
end;

procedure TFormTestIOCPServer.InConnection2ReceiveMsg(Sender: TObject;
  Msg: TResultParams);
begin
  // 收到其他客户端的消息
  AddMessage(mmoClient, 'USER_B 收到客户消息：' +
             Msg.Msg + ', 来自:' + Msg.UserName);
end;

procedure TFormTestIOCPServer.InConnection2ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.Action of
    atFileDownload,
    atFileDownChunk:
      case Result.ActResult of    // 文件不存在
        arFail:
          AddMessage(mmoClient, '服务器打开文件失败.');
        arMissing:
          AddMessage(mmoClient, '服务器文件不存在/丢失.');
        arOK:
          if Result.Action = atFileDownload then
            AddMessage(mmoClient, '下载文件完毕.')
          else
            AddMessage(mmoClient, '下载文件完毕（续传）.');          
      end;
  end;
end;

procedure TFormTestIOCPServer.InConnection2TimeOut(Sender: TObject);
begin
  AddMessage(mmoClient, 'Client 2 超时退出.');
end;

procedure TFormTestIOCPServer.InCustomClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 客户端收到服务器反馈结果

  with mmoClient.Lines do
  begin
    Add('DateTime=' + DateToStr(Result.DateTime));
    Add('DateTime=' + Result.Msg);
    Add('Result=' + Result.AsString['result']);
    Add('int=' + IntToStr(Result.AsInteger['int']));
    Add('string=' + Result.AsString['string']);
  end;

  mmoClient.Lines.Add('返回文件，保存到 temp\Cli_*.txt');

  with TMemoryStream(Result.AsStream['doc']) do
    try
      SaveToFile('temp\cli_doc.txt');
    finally
      Free;
    end;


end;

procedure TFormTestIOCPServer.InCustomManager1AttachBegin(Sender: TObject;
  Params: TReceiveParams);
begin
  // 这个消息带附件，建附件流（文件流），此时 Params.Attachment = nil
  mmoServer.Lines.Add('准备接收附件：临时路径, ' + Params.FileName);

  // 1. 推荐用文件管理器，保存到用户的临时路径
  InFileManager1.CreateNewFile(Params);

  // 2. 也可以直接指定存放路径
//  Params.CreateAttachment(iocp_varis.gUserDataPath +
//                          Params.UserName + '\temp\');

end;

procedure TFormTestIOCPServer.InCustomManager1AttachFinish(Sender: TObject;
  Params: TReceiveParams);
begin
  // 附件接收完毕！
  //   Params.Attachment，是 TIOCPDocument 文件流，
  //   在此不要 Free，系统自动释放！
  mmoServer.Lines.Add('附件接收完毕：' + Params.Attachment.FileName);
end;

procedure TFormTestIOCPServer.InCustomManager1Receive(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
var
  Strm: TMemoryStream;
begin
  // 自定义消息管理 服务器端

  mmoServer.Lines.Add('中文变量=' + Params.AsString['中文变量']);

  // 并发测试时会出现写文件冲突，必须存在！
  Strm := TMemoryStream(Params.AsStream['doc']);
  if Assigned(Strm) then
    try
      Strm.SaveToFile('temp\svr_doc.txt');
    finally
      Strm.Free;
    end;

  // 返回信息给客户端

  Result.ActResult := arOK;  // 操作成功
  Result.DateTime := Now;
  Result.Msg := '返回文本给客户端';

  Result.AsBoolean['result'] := True;
  Result.AsInteger['int'] := 12345689;
  Result.AsString['string'] := '成功，返回文本...';

  Result.AsDocument['doc'] := '测试2.txt'; // 返回小文件给客户端

  // 把一个文件当中附件发送给客户端，文件大小 < 2G
  Result.LoadFromFile('DAEMON_Tools_Lite_green.rar', True);

end;

procedure TFormTestIOCPServer.InDBConnection1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 查询数据库连接 -> 返回名称列表
  //   见：TInDatabaseManager.GetDBConnections
  case Result.ActResult of
    arExists:   // 有数据库连接
      ComboBox1.Items.DelimitedText := Result.AsString['dmCount'];
    arMissing:  // 没有
      { empty } ;
  end;
end;

procedure TFormTestIOCPServer.InDBQueryClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 查询、更新都用 InDBQueryClient1
//  mmoClient.Lines.Add('ThreadId:' + IntToStr(GetCurrentThreadId));
  case Result.Action of
    atDBExecQuery,
    atDBExecStoredProc:
      if Result.ActResult = arOK then
        mmoClient.Lines.Add('查询成功. ')
      else
        mmoClient.Lines.Add('查询失败: ' + Result.Msg);
    atDBApplyUpdates:
      if Result.ActResult = arOK then
        mmoClient.Lines.Add('远程更新成功.')
      else
        mmoClient.Lines.Add('远程更新失败: ' + Result.Msg);
    else
      if Result.ActResult = arOK then
        mmoClient.Lines.Add('执行成功.')
      else
        mmoClient.Lines.Add('执行失败: ' + Result.Msg);
  end;
end;

procedure TFormTestIOCPServer.InDBSQLClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.ActResult of
    arOK:
      mmoClient.Lines.Add('远程更新成功.');
    arFail:
      mmoClient.Lines.Add('远程更新失败.');
  end;
end;

procedure TFormTestIOCPServer.InEchoClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 收到响应服务反馈（和 v 1.6 的参数不同）
  //   Result 只包含协议信息，不含实体数据
  //   Result.VarCount 是客户端连接数，见：TReturnResult.ReturnHead
  AddMessage(mmoClient, '收到响应服务反馈，连接数=' + IntToStr(Result.VarCount));
end;

procedure TFormTestIOCPServer.InFileClient1Request(Sender: TObject;
  Action: TActionType);
begin
  // 发出请求操作
end;

procedure TFormTestIOCPServer.InFileClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 返回结果
  // atFileList 操作会先执行 OnListFiles,再执行本事件  
  if Result.Action = atFileDownload then
      case Result.ActResult of    // 文件不存在
        arMissing:
          AddMessage(mmoClient, '服务器文件不存在/丢失.');
        arOutDate:
          AddMessage(mmoClient, '文件过期被删除.');
        arOK:
          AddMessage(mmoClient, '下载文件完毕.');
      end;
end;

procedure TFormTestIOCPServer.InFileClient2ListFiles(Sender: TObject;
  ActResult: TActionResult; No: Integer; Result: TCustomPack);
begin
  // atFileList, atTextGetFiles 操作会先执行本事件,再执行 OnReturnResult
  case ActResult of
    arFail:
      AddMessage(mmoClient, '目录不存在.');
    arEmpty:
      AddMessage(mmoClient, '目录为空.');
    arExists:     // 列出服务端当前工作路径下的文件
      AddMessage(mmoClient, IntToStr(No) + ': ' +
                      Result.AsString['name'] + ', ' +
                      IntToStr(Result.AsInt64['size']) + ', ' +
                      DateTimeToStr(Result.AsDateTime['CreationTime']) + ', ' +
                      DateTimeToStr(Result.AsDateTime['LastWriteTime']) + ', ' +
                      Result.AsString['dir']);
  end;
end;

procedure TFormTestIOCPServer.InFileClient2ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.Action of
    atFileSetDir:
      case Result.ActResult of
        arOK:
          AddMessage(mmoClient, '设置目录成功.');
        arFail:
          AddMessage(mmoClient, '目录名称错误.');
        arMissing:
          AddMessage(mmoClient, '目录不存在.');
      end;

    atFileDownload,
    atFileDownChunk:
      case Result.ActResult of    // 文件不存在
        arFail:
          AddMessage(mmoClient, '服务器打开文件失败.');
        arMissing:
          AddMessage(mmoClient, '服务器文件不存在/丢失.');
        arOK:
          AddMessage(mmoClient, '下载文件完毕.');
      end;
                          
    atFileUpload,
    atFileUpChunk:
      case Result.ActResult of  // 2.0 不会有 arExists 的结果了
        arFail:
          AddMessage(mmoClient, '服务器建文件异常.');
        arOK:
          AddMessage(mmoClient, '上传文件完毕.');
      end;
  end;

end;

procedure TFormTestIOCPServer.InFileClient2WaitForAnswer(Sender: TObject;
  Result: TResultParams);
begin
  // ============== 第二步，发出方被动接收到接收方的在线应答 ================
{  case Result.ActResult of
    arOK:       // 对方选择接收，延迟传输文件给对方
      InFileClient2.SendOnline(Result.FileName, Result.FromUser);
    arCancel:
      AddMessage(mmoClient, '对方拒绝接收.');
  end;    }
end;

procedure TFormTestIOCPServer.InFileManager1AfterDownload(Sender: TObject;
  Params: TReceiveParams; Document: TIOCPDocument);
begin
  if (Params.Action = atFileDownChunk) then
    mmoServer.Lines.Add('下载完毕（续传）：' + ExtractFileName(Document.FileName))
  else
    mmoServer.Lines.Add('下载完毕：' + ExtractFileName(Document.FileName));

//  AddMessage(mmoServer, '文件下载完毕：' + ExtractFileName(Document.FileName));

end;

procedure TFormTestIOCPServer.InFileManager1AfterUpload(Sender: TObject;
  Params: TReceiveParams; Document: TIOCPDocument);
var
  oToSocket: TIOCPSocket;
begin
  // 上传文件完毕
  //   Sender: TBusiWorker
  //   Socket：TIOCPSocket
  // Document：TIOCPDocument

  // 有 3 种上传方式：atFileUpload、atFileUpChunk、atFileShare

  if (Params.Action = atFileUpChunk) then
    mmoServer.Lines.Add('文件续传完毕: ' + ExtractFileName(Document.FileName))
  else
    mmoServer.Lines.Add('上传完毕：' + ExtractFileName(Document.FileName));

  // 如果 Params.ToUser 不为空，
  // 则说明是互传文件，要通知对方下载或保存信息给对方登录时提取。

  // 新版调整推送方法，ToUser 可以是列表，直接调用即可：
  if (Params.ToUser <> '') then  // 互传的文件，通知 ToUser 下载
    InMessageManager1.PushMsg(Params, Params.ToUser);  // 唤醒或保存到离线文件

end;

procedure TFormTestIOCPServer.InFileManager1BeforeDownload(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
var
  FileName: AnsiString;
begin
  // 下载文件（要使用相对路径）
  FileName := gUserDataPath + Params.FileName;
  if (Params.Action = atFileDownChunk) then
    mmoServer.Lines.Add('准备下载（续传）：' + FileName)
  else
    mmoServer.Lines.Add('准备下载：' + FileName);
end;

procedure TFormTestIOCPServer.InFileManager1BeforeUpload(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 上传文件（到用户数据路径）
  //   2.0 在内部自动判断文件是否存在，存在则换一个文件名
  if (Params.Action = atFileUpChunk) then
    mmoServer.Lines.Add('准备接收文件（续传）: ' + Params.FileName)
  else
    mmoServer.Lines.Add('准备接收文件: ' + Params.FileName);

  // 如果免登录，可以使用这种方法接收：
  // Params.CreateAttachment('存放路径');
  
  InFileManager1.CreateNewFile(Params);

end;

procedure TFormTestIOCPServer.InFileManager1DeleteFile(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 请求删除文件，应在客户端先确认
  if DeleteFile(Params.Socket.Envir^.WorkDir + Params.FileName) then
    Result.ActResult := arOK
  else
    Result.ActResult := arFail;
end;

procedure TFormTestIOCPServer.InFileManager1QueryFiles(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 查询当前工作目录下的文件
  InFileManager1.ListFiles(Params, Result);
end;

procedure TFormTestIOCPServer.InFileManager1RenameFile(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 改工作目录下的文件名
  if RenameFile(Params.Socket.Envir^.WorkDir + Params.FileName,
                Params.Socket.Envir^.WorkDir + Params.NewFileName) then
    Result.ActResult := arOK
  else
    Result.ActResult := arFail;
end;

procedure TFormTestIOCPServer.InFileManager1SetWorkDir(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 设置工作目录（不能超出允许的工作目录范围）
//  if True then
    InFileManager1.SetWorkDir(Result, Params.Directory);
//  else
//    Result.ActResult := arFail;
end;

procedure TFormTestIOCPServer.InFunctionClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // 执行远程函数返回结果
  case Result.ActResult of
    arOK: begin
      with TMemoryStream(Result.AsStream['doc']) do
        try
          SaveToFile('temp\call_rtn_doc.txt');
        finally
          Free;
        end;
      mmoClient.Lines.Add('调用远程函数，返回文件 call_rtn_doc.txt。');
      mmoClient.Lines.Add('aaa=' + Result.AsString['aaa']);
    end;
    arMissing:
      AddMessage(mmoClient, '远程函数不存在！');
  end;
end;

procedure TFormTestIOCPServer.InHttpDataProvider1Accept(Sender: TObject;
  Request: THttpRequest; var Accept: Boolean);
begin
  // 在此判断是否接受请求:
  //   Request.Method: 方法
  //      Request.URI：路径/资源
  case Request.Method of
    hmGet:
      Accept := True;
    hmPost:
      Accept := True;
    else    // 测试其他方法
      Accept := True;
  end;
end;

procedure TFormTestIOCPServer.InHttpDataProvider1Get(Sender: TObject;
  Request: THttpRequest; Respone: THttpRespone);
var
  Source, Target: String;
  Stream: TStream;
  FileName: AnsiString;
begin
  // Get: 检查请求命令，反馈数据
  // InHttpDataProvider1.RootDirectory 是网站路径

  // 1. 下载页面、文件

  // 测试 excel 框架嵌入大 htm 文件
  // 文件太大时客户端当作附件下载，不显示在 excel 表单上，
  // 先压缩为 GZip 文件，发送 GZip 文件，TransmitFile 支持 ETag 标识，
  // 文件没修改过时，发送状态 304 给客户端。

{  if (Request.URI = '/favicon.ico') then
  begin
    Respone.StatusCode := 204;   // 没有东西
    Exit;
  end else
  begin
    if Request.URI = '/' then
      Source := 'web_site\index.htm'
    else
      Source := AdjustFileName('web_site\' + Request.URI);

    Target := Source + '.gzip';
    if FileExists(Target) = False then
      iocp_zlib.GZCompressFile(Source, Target);  // 先压缩（注意多线程，或者开启服务前先压缩）

    Respone.TransmitFile(Target);  // 发送压缩后的文件
    Respone.AddHeader(rshContentEncoding, 'gzip'); // gzip 压缩
  end;

  // 发送文件时，要比较版本，此时 StatusCode 有多种情况，最后再设置协议头。
  if Respone.StatusCode <> 304 then // // 304 Not Modified，文件没修改过
    Respone.AddHeader(rshContentType, 'text/html; charset=gb2312');

  Exit;  // ==================     }

  if (Request.URI = '/hello') then
    Respone.SetContent('hello world')
  else
{  if (Request.URI = '/') then
    Respone.TransmitFile('web_site\html\index.htm')
  else       }

  if Pos('/downloads', Request.URI) > 0 then
  begin
    FileName := FAppDir + Request.URI;
    if Request.URI = '/web_site/downloads/中文-A09.txt' then
      Respone.TransmitFile(FileName)          // IE浏览器自动显示
    else
    if Request.URI = '/web_site/downloads/中文-A10.txt' then
      Respone.TransmitFile(FileName, False)   // 让浏览器不自动显示
    else
    if Request.URI = '/web_site/downloads/httptest.exe' then
      Respone.TransmitFile(FileName)
    else
    if Request.URI = '/web_site/downloads/InIOCP技术要点.doc' then
    begin
   //   Respone.TransmitFile(FileName);
      Stream := TIOCPDocument.Create(AdjustFileName(FileName));
      Respone.SendStream(Stream);         // 发送文件流（自动释放）
    end else
    if Request.URI = '/web_site/downloads/InIOCP技术要点2.doc' then
    begin
      FileName := FAppDir + '/web_site/downloads/InIOCP技术要点.doc';
      Stream := TIOCPDocument.Create(AdjustFileName(FileName));
      Respone.SendStream(Stream, True);   // 压缩文件流（自动释放）
    end else
    if Request.URI = '/web_site/downloads/jdk-8u77-windows-i586.exe' then
    begin
      // 测试大文件下载
      if FileExists('F:\Backup\jdk-8u77-windows-i586.exe') then
        Respone.TransmitFile('F:\Backup\jdk-8u77-windows-i586.exe')
      else
        Respone.TransmitFile('web_site\downloads\jdk-8u77-windows-i586.exe');
    end else
    if Request.URI = '/web_site/downloads/test.jpg' then
    begin
      Respone.TransmitFile('web_site\downloads\test.jpg');
    end else    
    begin           // 测试 chunk，分块发送
      Stream := TIOCPDocument.Create(AdjustFileName(FileName));
      try
        Respone.SendChunk(Stream);  // 立刻发送，不释放（改进，内部自动发送结束标志）
      finally
        Stream.Free;
      end;
    end;

  end else

  // 2. ajax 动态页面
  if Pos('/ajax', Request.URI) > 0 then
  begin
    if Request.URI = '/ajax/login' then  // 登录
      Respone.TransmitFile('web_site\ajax\login.htm')
    else
    if Request.URI = '/ajax/ajax_text.txt' then
    begin
      // AJAX 请求文本，IE 可能乱码，chrome 正常
      if Respone.HasSession then
        Respone.TransmitFile('web_site\ajax\ajax_text.txt')
      else  //  用转义符，返回 INVALID_SESSION，客户端作相应检查
        Respone.SetContent(HTTP_INVALID_SESSION);
    end else
    if Request.URI = '/ajax/server_time.pas' then
    begin
      // AJAX 取服务器时间
      if Respone.HasSession then
        Respone.SetContent('<p>服务器时间：' + GetHttpGMTDateTime + '</p>')
      else  // 用转义符，返回 INVALID_SESSION，客户端作相应检查
        Respone.SetContent(HTTP_INVALID_SESSION);
    end else
    if Request.URI = '/ajax/query_xzqh.pas' then
    begin
      // AJAX 查询数据表，方法：
      // 1. 使用默认数模：TBusiWorker(Sender).DataModule.HttpExecQuery(Request, Respone)
      // 2. 指定数模：TBusiWorker(Sender).DataModules[1].HttpExecQuery(Request, Respone)

      // 测试大并发查询数据库
      //   使用工具 httpTest.exe，URL 用：
      //   /ajax/query_xzqh.pas?code=110112&SQL=Select_tbl_xzqh2
      //   使用 Select_tbl_xzqh2 对应的 SQL 命令查询数据
      TBusiWorker(Sender).DataModule.HttpExecQuery(Request, Respone);

{      if Respone.HasSession then
        TBusiWorker(Sender).DataModules[1].HttpExecQuery(Request, Respone)
      else
        Respone.SetContent(HTTP_INVALID_SESSION); }

    end else
    if Request.URI = '/ajax/quit' then     // 退出登录
    begin
      // 删除 Sessions，安全退出
      //   参考页面 ajax.htm 的函数 function getExit()，用 GET 方法，状态码 = 200
      if Respone.HasSession then
        Respone.InvalidSession;
    end;
  end else
  begin

    // 3. 普通页面
    // 三种类型的表单，POST 的参数编码不同，解码不同

    if (Request.URI = '/') then
      Respone.TransmitFile('web_site\html\index.htm')
    else
    if (Request.URI = '/favicon.ico') then
      Respone.StatusCode := 204   // 没有东西
    else  // 页面增加后缀名 .htm
      Respone.TransmitFile('web_site\html\' + Request.URI);

  end;

end;

procedure TFormTestIOCPServer.InHttpDataProvider1InvalidSession(Sender: TObject;
  Request: THttpRequest; Respone: THttpRespone);
begin
  // 请求带 Session，但 Session 无效时调用此事件
  if Pos('/ajax', Request.URI) = 1 then
    if (Request.URI = '/ajax/login') then
      Respone.TransmitFile('web_site\ajax\login.htm')
    else
      // 浏览器 ajax 中无法响应 302 状态，不能用 Redirect
      //   用转义符，返回 INVALID_SESSION，客户端作相应检查，
      //   也可以使用其他方法，如 JSON 数据
      Respone.SetContent(HTTP_INVALID_SESSION);
end;

procedure TFormTestIOCPServer.InHttpDataProvider1Post(Sender: TObject;
  Request: THttpRequest; Respone: THttpRespone);
begin
  // Post：已经接收完毕，调用此事件
  //   此时：Request.Complete = True
//  Request.Entity.SaveToFile('x.txt');

  if Request.URI = '/ajax/login' then  // 动态页面
  begin
    with mmoServer.Lines do
    begin
      Add('登录信息:');
      Add(' userName=' + Request.Params.AsString['user_name']);
      Add(' password=' + Request.Params.AsString['user_password']);
    end;
    if (Request.Params.AsString['user_name'] <> '') and
      (Request.Params.AsString['user_password'] <> '') then   // 登录成功
    begin
      Respone.CreateSession;  // 生成 Session！
      Respone.TransmitFile('web_site\ajax\ajax.htm');
    end else
      Respone.Redirect('/ajax/login');  // 重定位到登录页面
  end else
  begin
    with mmoServer.Lines do
    begin
      Add('HTTP 服务:');
      Add('   textline=' + Request.Params.AsString['textline']);
      Add('  textline2=' + Request.Params.AsString['textline2']);
      Add('    onefile=' + Request.Params.AsString['onefile']);
      Add('  morefiles=' + Request.Params.AsString['morefiles']);
    end;
    Respone.SetContent('<html><body>InIOCP HTTP 服务！<br>提交成功！<br>');
    Respone.AddContent('<a href="' + Request.URI + '">返回</a><br></body></html>');
  end;
end;

procedure TFormTestIOCPServer.InHttpDataProvider1ReceiveFile(Sender: TObject;
  Request: THttpRequest; const FileName: String; Data: PAnsiChar;
  DataLength: Integer; State: THttpPostState);
var
  S: AnsiString;
begin
  // Post: 已经接收完毕，收到上传的文件，保存到文件流
  // Sender: 是 TBusiWorker
  // Request.Owner: 是 THttpSocket
  case State of
    hpsRequest: begin       // 请求状态
      S := ExtractFileName(FileName);
      if not FileExists('web_site\uploads\' + S) then
        THttpSocket(Request.Owner).CreateStream('web_site\uploads\' + S);
    end;
    hpsRecvData: begin     // 保存、关闭文件流
      THttpSocket(Request.Owner).WriteStream(Data, DataLength);
      THttpSocket(Request.Owner).CloseStream;
    end;
  end;

end;

procedure TFormTestIOCPServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  // ++
  btnStop.Enabled := InIOCPServer1.Active;
  btnStart.Enabled := not btnStop.Enabled;
  mmoClient.Lines.Clear;
  mmoServer.Lines.Clear;  
end;

procedure TFormTestIOCPServer.InIOCPServer1Connect(Sender: TObject;
  Socket: TBaseSocket);
begin
  // 客户端接入，Socket 即将投放 WSARecv（见 TInIOCPServer.AcceptClient）
  // Socket.Data 属性由用户扩展，可以存放自定义数据，如：
  // Socket.Data := TMemoryStream.Create;
  // 如果禁止接入，可以直接 Socket.Close;
end;

procedure TFormTestIOCPServer.InIOCPServer1DataReceive(Sender: TBaseSocket;
  const Data: PAnsiChar; Size: Cardinal);
begin
  // 已在 TWorkThread.IOIncrement 加锁，
  //   线程安全：严格来说不要访问主线程的任何控件
{  AddMessage(mmoServer, '收到数据长度=' + IntToStr(Size) + ' Bytes');
  if (Sender is TStreamSocket) then
    mmoServer.Lines.Add('原始的流数据');   }
end;

procedure TFormTestIOCPServer.InIOCPServer1DataSend(Sender: TBaseSocket;
  Size: Cardinal);
begin
  // 已在 TWorkThread.IOIncrement 加锁
  //   线程安全：严格来说不要访问主线程的任何控件
//  AddMessage(mmoServer, '发出数据长度=' + IntToStr(Size) + ' Bytes');
end;

procedure TFormTestIOCPServer.InIOCPServer1Disconnect(Sender: TObject;
  Socket: TBaseSocket);
begin
  // 客户端即将断开（见 TInIOCPServer.CloseSocket）
  // 禁止接入时也触发当前事件
  // Socket.Data 属性是用户扩展的数据，要自己释放或作进一步处理

  // 系统默认的客户端对象是 THttpSocket，
  // Socket 资源转移给对应协议的 TBaseSocket 后，会被关闭，
  // 此时要判断一下，如果 Socket.Connected = False 则说明资源已被转移，不用处理
  // 见 TBaseSocket.Clone。

  // 一般可以这样：
  // if Socket.Connected and Assigned(Socket.Data) then
  //   TMemoryStream(Socket.Data).Free;  // 释放 Data 资源

end;

procedure TFormTestIOCPServer.InMessageClient1MsgReceive(Sender: TObject;
  Msg: TResultParams);
begin
  // 收到其他客户端的消息
  AddMessage(mmoClient, 'USER_A 收到客户消息：' +
             Msg.Msg + ', 来自:' + IntToStr(Msg.Owner));
end;

procedure TFormTestIOCPServer.InMessageClient1ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // atTextGetFiles 操作会先执行 OnListFiles,再执行本事件
  if Result.ActResult = arOK then
    mmoClient.Lines.Add('USER_A 消息发送成功');
end;

procedure TFormTestIOCPServer.InMessageClient2ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  case Result.ActResult of
    arOK:
      mmoClient.Lines.Add('USER_B 消息发送成功.');
    arOffline:
      mmoClient.Lines.Add('服务器：对方离线.');
  end;
end;

procedure TFormTestIOCPServer.InMessageManager1Broadcast(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 广播：发送消息给全部客户端
  //       把消息写给全部客户端的方便不可行，这里不处理！
//  mmoClient.Lines.Add(Params.Msg);
  InMessageManager1.Broadcast(Params);  // 内部设置 ActResult
end;

procedure TFormTestIOCPServer.InMessageManager1Get(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 取离线消息（客户端发送 arTextGet 请求）
  InMessageManager1.ReadMsgFile(Params, Result);  // 未必有离线消息
  Result.Msg := '这是离线消息。';
  Result.ActResult := arOK;
end;

procedure TFormTestIOCPServer.InMessageManager1ListFiles(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 离线消息大小为 2m，超过时文件被改名，在这里用文件管理器列出,
  // 见：InternalOpenMsgFile

  // 用文件管理器取信息，客户端还是用消息组件的 OnListFiles 显示
  InFileManager1.ListFiles(Params, Result, True);
end;

procedure TFormTestIOCPServer.InMessageManager1Push(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
var
  oSocket: TIOCPSocket;
begin
  // 发送消息给其他客户端: ToUser、TargetUser

  // 用第三方工具推送时可以用 JSON 格式的消息
  // mmoServer.Lines.Add(Params.ToJSON);
   
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

  // 2.2 返回总体的投放情况
  if (Result.ActResult = arOK) then
    Result.Msg := '投放推送消息成功.'
  else
    Result.Msg := '投放推送消息失败.';

end;

procedure TFormTestIOCPServer.InMessageManager1Receive(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 服务端收到消息
//  AddMessage(mmoServer, 'Server 收到消息：' + Params.Msg);
  Result.LoadFromFile('DAEMON_Tools_Lite_green.rar', True);
end;

procedure TFormTestIOCPServer.InRemoteFunctionGroup1Execute(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 远程函数返回结果给客户端
  case Params.FunctionIndex of
    1: begin
      Result.AsString['aaa'] := 'test call.';
      Result.AsDocument['doc'] := '测试2.txt';  // 返回小文件
      Result.ActResult := arOK;    // 可能是其他情况
    end;
    2: begin
       // 调用其他功能
    end;
  end;
end;

procedure TFormTestIOCPServer.InWebSocketManager1Receive(Sender: TObject; Socket: TWebSocket);
var
  S: AnsiString;
begin
  // WebSocket 收到消息触发此事件
  // WebSocket 按帧传输，默认的 WebSocket 消息未封装，建议用 JSON 封装传输

  // Socket 收到的数据分三种，属性 MsgType 的值：
  //  1.    mtDefault: 非 InIOCP-JSON 封装的标准 WebSocket 数据；
  //  2.       mtJSON: 用 InIOCP-JSON 封装的扩展 JSON 消息；
  //  3. mtAttachment: 用 InIOCP-JSON 封装的附件流。

  // MsgType 为 mtDefault 时的 Socket 相关属性：
  // 1、         Data：本次收到的数据内容首位置
  // 2、FrameRecvSize：本次收到的内容长度
  // 3、    FrameSize：当前帧的内容总长度
  // 4、      MsgSize：当前消息累计收到的内容长度（可能含多帧数据）
  // 5、     Complete：当前消息是否接收完毕，True 时 MsgSize 为消息的实际长度
  // 6、       OpCode：操作，关闭时也触发本事件

  // InIOCP-JSON 封装的消息不支持上述第 1-4 的属性，重要属性：
  //        7、  JSON: 收到的 JSON 消息
  //        8、Result: 要反馈的 JSON 消息，用 SendResult 发送。

  // 可以用 SendResult 连续发送数据给客户端！

  case Socket.MsgType of  // 3种类型的数据
    mtJSON: // 1. InIOCP 扩展的 JSON 消息（此时 Socket.Complete = True）
      case Socket.JSON.Action of
        33:  // 广播
          InWebSocketManager1.Broadcast(Socket);

        11:  begin  // 执行数据库查询
          mmoServer.Lines.Add('aaa=' + Socket.JSON.S['aaa']);
          // 查询
          Socket.Result.Action := 11;
          TBusiWorker(Sender).DataModule.WebSocketQuery(Socket.JSON, Socket.Result);
          Socket.SendResult;  // 用默认字符集，非 UTF-8
        end;

        12: begin // 更新数据表
          Socket.Result.Action := 12;

          TBusiWorker(Sender).DataModule.WebSocketUpdates(Socket.JSON, Socket.Result);

          Socket.SendResult;
        end;

        20: begin // 查询文件
          Socket.Result.Action := 20;

          // 查询路径：gAppPath + 'form\'
          InFileManager1.ListFiles(Socket.Result, gAppPath + Socket.JSON.S['path']);

          Socket.SendResult;

          // 可以把文件当作附件，逐一发送:
        { for i := 0 to Count - 1 do
          begin
            Socket.Result.Attachment := TFileStream.Create('??' + , fmShareDenyWrite);
            Socket.Result.S['fileName'] := '??';
            Socket.SendResult;
          end;  }

        end;

        else begin
          // 其他消息
          mmoServer.Lines.Add('aaa=' + Socket.JSON.S['aaa']);
          mmoServer.Lines.Add('bbb=' + Socket.JSON.S['BBB']);
          mmoServer.Lines.Add('附件流名称=' + Socket.JSON.S['attach']);

          if Socket.JSON.HasAttachment then // 带附件 -> 建文件流接收附件，不建时忽略
            Socket.JSON.Attachment := TFileStream.Create('temp\服务端收到' + Socket.JSON.S['attach'], fmCreate);

          Socket.UserName := 'JSON';
    //     InWebSocketManager1.SendTo(Socket, 'ToUser');

    //      Socket.Result.S['return'] := 'test 返回消息';
    //      Socket.SendResult;  // 发送结果给客户端
        end;

      end;

    mtAttachment: begin
      // 2. InIOCP 扩展的 附件流 数据（此时 Socket.Complete = True）
      //    如果 Socket.JSON.Attachment 未空，照样会执行到此

      // 系统会自动关闭附件流 Socket.JSON.Attachment
      mmoServer.Lines.Add('附件接收完毕（系统会自动关闭附件流）。');

      // 返回消息
      Socket.Result.S['return'] := 'test 返回消息+附件流';

      // A. 返回附件流
      Socket.Result.Attachment := TFileStream.Create('Doc\Form.7z', fmShareDenyWrite);
      Socket.Result.S['attach'] := 'Form.7z';
      Socket.SendResult;  // 用默认字符集，非 UTF-8

      // 连续发送数据
      // B. 查询数据库，返回 Data 内容（也是附件）
      //    返回 Data 会清已设的附件流，两者互斥。

 {     TBusiWorker(Sender).DataModule.WebSocketQuery(Socket.JSON, Socket.Result);
      Socket.SendResult;  // 用默认字符集，非 UTF-8  }

    end;

    else begin

      // 3. 标准的 WebSocket 数据（如浏览器发送来的，Socket.Complete 未必为 True）
      
      if Socket.Complete then // 消息接收完毕
      begin
        // 双字节的 UTF8To 系列函数的传入参数以 AnsiString 为主
        // 定义 S 为 AnsiString 更方便操作
        SetString(S, Socket.Data, Socket.FrameRecvSize); // 把消息转为 String

        Socket.UserName := System.Utf8ToAnsi(S); // XE10 还可以用 UTF8ToString(S)
        mmoServer.Lines.Add(Socket.UserName);
        
        // 返回全部客户端名称列表
//        InWebSocketManager1.GetUserList(Socket);

        InWebSocketManager1.Broadcast(Socket);

        // 告诉全部客户端，UserName 上线了
//        InWebSocketManager1.Broadcast('聊天室广播：' + Socket.UserName + '上线了.');

{       // 以下方法测试正常：

        // 1. 反馈消息给客户端
        Socket.SendData('服务器反馈：' + Socket.UserName);

        // 2. 返回在线且已经登记用户名的全部用户的
        //    名称列表给客户端（JSON格式，字段为 NAME，默认字符集）
        InWebSocketManager1.GetUserList(Socket);

        // 3. 广播 Socket 收到的消息
        InWebSocketManager1.Broadcast(Socket);

        // 4. 把收到的消息发给 ToUser
        InWebSocketManager1.SendTo(Socket, 'ToUser');

        // 5. 广播一个文本消息
        InWebSocketManager1.Broadcast('测试广播');

        // 6. 推送一个文本给 ToUser
        InWebSocketManager1.SendTo('ToUser', '发送给ToUser');

        // 7. 用删除客户端 AAA
        if (Socket.Role >= crAdmin) then  // 有权限
          InWebSocketManager1.Delete(Socket, '用户A'); }

    //    Memo1.Lines.Add('收到 WebSocket 消息：' + S);  // 大并发不要显示
      end;
    end;
  end;
end;

procedure TFormTestIOCPServer.InWebSocketManager1Upgrade(Sender: TObject;
  const Origin: string; var Accept: Boolean);
begin
  // 在此判断是否允许来自 Origin 的 Socket 升级为 WebSocket
  Accept := Length(Origin) > 0;
end;

procedure TFormTestIOCPServer.InWSConnection1AfterConnect(Sender: TObject);
begin
  if InWSConnection1.Active then
  begin
    btnWSConnect.Caption := 'WSDisconnect';
    btnWSSendFiles.Enabled := True;
    btnWSListFiles.Enabled := True;
  end else
  begin
    btnWSConnect.Caption := 'WSConnect';
    btnWSSendFiles.Enabled := False;
    btnWSListFiles.Enabled := False;    
  end;
end;

procedure TFormTestIOCPServer.InWSConnection1ReceiveData(Sender: TObject; const Msg: string);
begin
  // Sender: 是 TInWSConnection
  //    Msg：收到消息文本

  // 收到的不是 InIOCP-JSON 消息（可能未封装的数据）
  // 测试方法：用浏览器发送一条消息，在服务端广播它

  mmoClient.Lines.Add(Msg);
end;

procedure TFormTestIOCPServer.InWSConnection1ReceiveMsg(Sender: TObject; Msg: TJSONResult);
begin
  //  Sender: 是 TInWSConnection
  //     Msg: JSON 消息，系统会自动释放

  // 测试方法：用 TJSONMessage 发送消息，在服务端广播它

  mmoClient.Lines.Add('收到推送消息（被动接收）：' + Msg.S['push']);
end;

procedure TFormTestIOCPServer.InWSConnection1ReturnResult(Sender: TObject; Result: TJSONResult);
var
  i: Integer;
begin
  // Sender: 是 TInWSConnection
  // Result: JSON 消息，系统会自动释放
  
  // Result.MsgType: 消息类型
  //   1. mtJSON：Result 是 JSON 消息
  //   2. mtAttachment：Result 是附件流（系统自动释放，不要 Result.Attachment.Free）

  // 测试方法：服务端收到消息后，设置 Socket.Result，用 Socket.SendResult 发送回来。

  if Result.MsgType = mtJSON then
  begin
    if Result.Action = 20 then
    begin
      if Result.I['count'] = -1 then  // 路径错误
        MemoDMInfos.Lines.Add('路径错误.')
      else
      for i := 1 to Result.I['count'] do
        with Result.R[IntToStr(i)] do  // 逐一取记录
        begin
          mmoClient.Lines.Add(S['name']);  // 可以见请求消息，下载
          Free; // 释放记录 R[IntToStr(i)]
        end;
    end else
      mmoClient.Lines.Add('服务端返回给自己的 JSON 消息: ' + Result.S['return']);
      
    if Result.HasAttachment then  // 有附件流，接收（可以不接收）
      Result.Attachment := TFileStream.Create('temp\__' + Result.S['attach'], fmCreate);

  end else  // 是附件流
  if Assigned(Result.Attachment) then  // 收到文件流
    mmoClient.Lines.Add('接收文件完毕')  
  else
    mmoClient.Lines.Add('没有接收文件');

end;

procedure TFormTestIOCPServer.listDMClick(Sender: TObject);
var
  i: Integer;        // 编号
  AClassName, Description: String;  // 类名、描述
  Running: Boolean;  // 是否已建实例
begin
  // 列出全部数模
  if InDatabaseManager1.DataModuleCount = 0 then
    MemoDMInfos.Lines.Add('Empty--------')
  else
    for i := 0 to InDatabaseManager1.DataModuleCount - 1 do
    begin
      InDatabaseManager1.GetDataModuleState(i, AClassName, Description, Running);
      if Running then
        MemoDMInfos.Lines.Add(IntToStr(i) + ',' + AClassName + ',' + Description + ',True')
      else
        MemoDMInfos.Lines.Add(IntToStr(i) + ',' + AClassName + ',' + Description + ',False');          
    end;  
end;

procedure TFormTestIOCPServer.Timer1Timer(Sender: TObject);
var
  Msg: TMessagePack;
begin
  Msg := TMessagePack.Create(InConnection1);
  Msg.Msg := '测试广播消息';
  Msg.CheckType := ctMD5;
  Msg.AsString['text'] := '加一个文本';

  Msg.Post(atTextBroadcast);  // 广播一条消息

{  Msg := TMessagePack.Create(InConnection1);
  Msg.Msg := '测试的广播消息（连接未登录也能收到）';
  Msg.DateTime := Now;  // 增加时间

  Msg.Post(atTextBroadcast); }
end;

procedure TFormTestIOCPServer.btnSendToClick(Sender: TObject);
begin
  // 发送文本
  if edtTo.Text = '' then     // 给服务器
    InMessageClient1.SendMsg('发送消息到服务端a')
  else  // 给其他客户（用原方法，未改进）
    InMessageClient1.SendMsg('测试推送消息，目的: ' + edtTo.Text, edtTo.Text);
  // InMessageClient1 是发出消息包的宿主，见：TBaseMessage.Create(AOwner: TObject);
  AddMessage(mmoClient, '发出方：' + IntToStr(LongWord(InMessageClient1)));
end;

procedure TFormTestIOCPServer.btnSetDirClick(Sender: TObject);
begin
  // 设置服务端的用户工作路径
  if btnSetDir.Caption = 'Set sub' then
  begin
    InFileClient2.SetDir('form');
    btnSetDir.Caption := 'Set ..\';
  end else
  begin
    btnSetDir.Caption := 'Set sub';
    InFileClient2.SetDir('..');
  end;
end;

procedure TFormTestIOCPServer.btnDBConnectionClick(Sender: TObject);
begin
  // 查询数模数(数据库连接数)
  if (ComboBox1.Items.Count = 0) then
    InDBConnection1.GetConnections
  else
  if ComboBox1.ItemIndex > -1 then
    InDBConnection1.Connect(ComboBox1.ItemIndex);
end;

procedure TFormTestIOCPServer.btnDownloadClick(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // 下载服务端当前工作路径下的文件

  Msg := TMessagePack.Create(InConnection2);
  Msg.FileName := 'MSOffice2003.7z'; // DAEMON_Tools_Lite_green.rar';
//  Msg.CheckType := ctMD5;

  // 分块下载
  // 文件保存在 InConnection2.localPath 下，
  // 自动建一个传输信息文件 .download，传完删除

  Msg.Post(atFileDownload);  // atFileDownload atFileDownChunk

//  InFileClient2.Download('DAEMON_Tools_Lite_green.rar');
end;

procedure TFormTestIOCPServer.btnSndTo2Click(Sender: TObject);
begin
  if edtTo2.Text = '' then // 发消息到服务器
    InMessageClient2.SendMsg('发送消息到服务端')
  else  // 给其他客户（用原方法，未改进）
    InMessageClient2.SendMsg('用户2 推送消息，目的: ' + edtTo2.Text, edtTo2.Text);
  // InMessageClient2 是发出消息包的宿主，见：TBaseMessage.Create(AOwner: TObject);
  AddMessage(mmoClient, '发出方：' + IntToStr(LongWord(InMessageClient2)));
end;

procedure TFormTestIOCPServer.btnClearMemoClick(Sender: TObject);
{ type
  PByteAry = ^TByteAry;
  TByteAry = array of Byte;
  TMaskRec = record
    case Integer of
      0: (Ary: array[0..3] of Byte);
      1: (Value: Cardinal);
  end;
var
  i, k, n, Size: Integer;
  Masks: TMaskRec;
  P: PCardinal;
  A, B: PByte;
  Tick: Cardinal;
  Strm, Strm2: TMemoryStream;       

const
  GUID_S = '62548B39-8766-4181-8ECC-D09AEB56ECD6-----4EF415A5-8ABB-4160-87A7-69B28ED32263';
var
  i: Integer;
  Dest: TSHA1Digest;
  Tick: Cardinal;      
var
  NowTime: TDateTime;
  Certify: TCertifyNumber;
  LHour, LMinute, LSecond, LMilliSecond: Word;  }
begin
  // 生成一个登录凭证，有效期为 SESSION_TIMEOUT 分钟
  //   结构：(相对日序号 + 有效分钟) xor 年

  mmoServer.Lines.Clear;
  mmoClient.Lines.Clear;

{  NowTime := Now();

  DecodeTime(NowTime, LHour, LMinute, LSecond, LMilliSecond);

  Certify.DayCount := Trunc(NowTime - 43000);  // 相对日序号
  Certify.Timeout := LHour * 60 + LMinute + SESSION_TIMEOUT;

  if (Certify.Timeout >= 1440) then
  begin
    Inc(Certify.DayCount);  // 后一天
    Certify.Timeout := 1440 - Certify.Timeout;
  end;


  mmoServer.Lines.Add(IntToStr(Certify.Session xor (Cardinal($AB00) + YearOf(NowTime))));
}

//  mmoClient.Lines.Add(BoolToStr(False));

{  mmoClient.Lines.Add('计算SHA1码');
  mmoClient.Lines.Add('内容：' + GUID_S);

  Tick := GetTickCount;

  for i := 1 to 1024 * 1024 do
    iocp_SHA1.SHA1StringA(GUID_S);

  mmoClient.Lines.Add('优化：' + IntToStr(GetTickCount - Tick));

  Tick := GetTickCount;
  for i := 1 to 1024 * 1024 do
    iocp_cnSHA1.SHA1StringA(GUID_S);

  mmoClient.Lines.Add('源码：' + IntToStr(GetTickCount - Tick));

  Strm := TMemoryStream.Create;
  Strm.LoadFromFile('autoClick.7z');

  Strm2 := TMemoryStream.Create;
  Strm2.LoadFromFile('autoClick.7z');
  
  Size := Strm.Size;
  Tick := GetTickCount;

  Masks.Value := Tick;
  Inc(Masks.Ary[3], 5);

  for n := 1 to 20000 do
  begin
    P := Strm.Memory;
    k := Size;
    while (k >= 4) do // 4 字节 xor
    begin
      P^ := P^ xor Masks.Value;
      Dec(k, 4);
      Inc(P);
    end;
    if k > 0 then  // 剩余的单字节 xor
      for i := Size - k to Size do
      begin
        PByte(P)^ := PByte(P)^ xor Masks.Ary[i mod 4];
        Inc(PByte(P));
      end;
  end;

  mmoClient.Lines.Add('4字节xor=' + IntToStr(GetTickCount - Tick));

  Tick := GetTickCount;
    
  for n := 1 to 20000 do
  begin
    B := Strm.Memory;
    for i := 0 to Strm.Size - 1 do // 全部单字节 xor
    begin
      B^ := B^ xor Masks.Ary[i mod 4];
      Inc(B);
    end;
  end;

  mmoClient.Lines.Add('单字节xor=' + IntToStr(GetTickCount - Tick));  }

{
  // 比较结果是否一致
  
  A := Strm.Memory;
  B := Strm2.Memory;

  for i := 0 to Strm.Size - 1 do
    if A^ <> B^ then
    begin
      MemoDMInfos.Lines.Add(IntToStr(i) + ',' + PAnsiChar(A)^ + '<>' + PAnsiChar(B)^);
      Break;
    end;       }

end;

procedure TFormTestIOCPServer.btnEchoClick(Sender: TObject);
begin
  // ++ 测试响应
  InEchoClient1.Post;
end;

procedure TFormTestIOCPServer.btnExecQueryClick(Sender: TObject);
begin
  // 查询数据库，结果输出到 ClientDataSet1
//  mmoClient.Lines.Add('ThreadId:' + IntToStr(GetCurrentThreadId));

  // 查询结果有多个数据集时，要先加入子 TClientDataSet
  // InDBQueryClient1.AddClientDataSets(???);

  with InDBQueryClient1 do
  begin
    // 第一个 SQL
//    Params.SQL := 'UPDATE tbl_xzqh SET code = 001 WHERE code IS NULL';
//    ExecQuery;

    // 查询结果
    Params.SQL := 'SELECT * FROM tbl_xzqh ORDER BY code';
    ExecQuery;  // 新版不带参数
  end;

  PageControl1.ActivePageIndex := 1;
end;

procedure TFormTestIOCPServer.btnExecSQLClick(Sender: TObject);
begin
  // 更新数据库

  PageControl1.ActivePageIndex := 0;
  
  // 1. 用命令（成功）

{  with InDBSQLClient1 do
  begin
    Params.SQL := 'UPDATE tbl_xzqh SET picture = :picutre WHERE code = :code';
    Params.AsStream['picture'] := TFileStream.Create('doc\upd.jpg', fmOpenRead);
    Params.AsString['code'] := '110100';
    ExecSQL;
  end;

  // 在服务端设置一个 SQL 名称 Update_xzqh，可以这样操作
  //   见文件：SQL\TdmInIOCPTest.sql，单元 iocp_sqlMgr.pas

  with InDBSQLClient1 do
  begin
    Params.SQLName := 'Update_xzqh';
    ExecSQL;
  end;

}

  // 2. 用 InDBQueryClient1
  InDBQueryClient1.ApplyUpdates;  // 新版不带参数

end;

procedure TFormTestIOCPServer.btn1Click(Sender: TObject);
begin
  // 推送文件（消息部分）给其他客户端
  //    2.0 未实现，准备用推送消息的方法实现。
end;

procedure TFormTestIOCPServer.btnCastClick(Sender: TObject);
begin
  // ++ 发送消息给全部在线客户端（用原方法，未改进）
  //    管理员才能广播！见：TFormTestIOCPServer.InClientManager1Login

  // 测试，不断广播，可以另运行“新特性应用”，执行各种操作，测试效果，
  // 少量推送没问题，大量推送效果没测试。

  Timer1.Enabled := not Timer1.Enabled; 

//  InMessageClient1.Broadcast('这是 USER_A 的广播消息.');
//  InMessageClient1.Broadcast('这是 USER_A 的广播消息222222.');

end;

procedure TFormTestIOCPServer.btnCustmSndClick(Sender: TObject);
begin
  // 自定义客户端组件
  //   服务端用 InCustomManager1 管理
  with InCustomClient1 do
  begin
    Params.UserName := '预设用户';
    Params.Password := '预设密码';
    Params.Msg := '给个消息文本';

    Params.AsBoolean['bool'] := True;
    Params.AsInteger['int'] := 123;
    Params.AsCardinal['cardinal'] := 8899;
    Params.AsFloat['float'] := 123.123;
    Params.AsInt64['int64'] := 6464;
    Params.AsDateTime['datetime'] := Now;
    Params.AsString['中文变量'] := '中文文本。。。。。。';

    // 把小文件当作变量/参数/字段发送
    Params.AsStream['strm'] := TFileStream.Create('测试2.txt', fmShareDenyWrite);
//    Params.AsDocument['doc'] := '测试3.txt';

    // 把文件当作附件发送
    Params.LoadFromFile('autoClick.7z');

    Params.CheckType := ctMD5; // ctMurmurHash;
    Params.ZipLevel := zcDefault; // 压缩率

    Post;

    // Post 后 Params 被提交到发送线程，可以继续发送消息
{    Params.AsString['con'] := '继续发送消息';
    Params.AsString['中文变量'] := '中文文本！！！';
    Params.AsDocument['doc'] := '测试3.txt';

    Post;    }

  end;

end;

procedure TFormTestIOCPServer.btnConClick(Sender: TObject);
begin
  // ++
  if (edtHost.Text <> '') then
    InConnection2.ServerAddr := edtHost.Text;
  if edtPort.Text <> '' then
    InConnection2.ServerPort := StrToInt(edtPort.Text);
  InConnection2.Active := not InConnection2.Active;
end;

procedure TFormTestIOCPServer.btnLogClick(Sender: TObject);
begin
  // ++
  if InCertifyClient2.Logined then
    InCertifyClient2.Logout
  else
    InCertifyClient2.Login;
end;

procedure TFormTestIOCPServer.btnLoginxClick(Sender: TObject);
begin
  // ++
  if InCertifyClient1.Logined then
    InCertifyClient1.Logout
  else
    InCertifyClient1.Login;
end;

procedure TFormTestIOCPServer.btnQueryDirClick(Sender: TObject);
begin
  // 列出服务端工作路径的全部文件
  // 返回事件: TFormTestIOCPServer.InFileClient2ListFiles

  InFileClient2.ListFiles;

  // 用 InMessageClient1 查询全部离线消息文件
  //   查历史消息有用
//  InMessageClient1.GetMsgFiles;

end;

procedure TFormTestIOCPServer.btnConnectClick(Sender: TObject);
begin
  // ++
  // 连接成功后自动发标志 IOCP_SOCKET_FLAG 到服务端，
  //   服务端把资源转换 TIOCPSocket，见：TInConnection.InternalOpen、
  //   THttpSocket.ExecuteWork;
  if (edtHost.Text <> '') then
    InConnection1.ServerAddr := edtHost.Text;
  if edtPort.Text <> '' then
    InConnection1.ServerPort := StrToInt(edtPort.Text);
  InConnection1.Active := not InConnection1.Active;
end;

procedure TFormTestIOCPServer.btnListClientClick(Sender: TObject);
begin
  // ++
  // 查询在线客户端
  InCertifyClient1.QueryClients;
end;

end.
