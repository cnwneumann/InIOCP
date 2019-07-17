unit frmInIOCPStreamServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, iocp_sockets, iocp_server, fmIOCPSvrInfo, iocp_managers;

type
  TFormInIOCPStreamServer = class(TForm)
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    btnStart: TButton;
    btnStop: TButton;
    Edit1: TEdit;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    InStreamManager1: TInStreamManager;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure InIOCPServer1DataSend(Sender: TBaseSocket; Size: Cardinal);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
    procedure InIOCPServer1Connect(Sender: TObject; Socket: TBaseSocket);
    procedure InIOCPServer1Disconnect(Sender: TObject; Socket: TBaseSocket);
    procedure InStreamManager1Receive(Socket: TStreamSocket;
      const Data: PAnsiChar; Size: Cardinal);
    procedure InIOCPServer1DataReceive(Sender: TBaseSocket; Size: Cardinal);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPStreamServer: TFormInIOCPStreamServer;

implementation

uses
  iocp_log, iocp_utils, iocp_base, iocp_msgPacks, http_utils;
  
{$R *.dfm}

procedure TFormInIOCPStreamServer.btnStartClick(Sender: TObject);
begin
//  Memo1.Lines.Clear;
  iocp_log.TLogThread.InitLog;              // 开启日志
  InIOCPServer1.ServerAddr := Edit1.Text;     // 地址
  InIOCPServer1.Active := True;               // 开启服务
  FrameIOCPSvrInfo1.Start(InIOCPServer1);     // 开始统计
end;

procedure TFormInIOCPStreamServer.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False;   // 停止服务
  FrameIOCPSvrInfo1.Stop;          // 停止统计
  iocp_log.TLogThread.StopLog;   // 停止日志
end;

procedure TFormInIOCPStreamServer.FormCreate(Sender: TObject);
begin
  // 本地路径
//  Edit1.Text := GetLocalIp;
  FAppDir := ExtractFilePath(Application.ExeName);     
  MyCreateDir(FAppDir + 'log');
end;

procedure TFormInIOCPStreamServer.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

procedure TFormInIOCPStreamServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
  Memo1.Lines.Add('ip: ' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('port: ' + IntToStr(InIOCPServer1.ServerPort));
end;

procedure TFormInIOCPStreamServer.InIOCPServer1Connect(Sender: TObject;
  Socket: TBaseSocket);
begin
  // Socket 接入，即将被投放接收数据，可以使用 Socket.Close 禁止接入;
end;

procedure TFormInIOCPStreamServer.InIOCPServer1DataReceive(Sender: TBaseSocket;
  Size: Cardinal);
begin
  // 服务端收到数据
  // 1. 重大调整，新版增加流服务管理器 TInStreamManager，
  //    在 TInStreamManager.OnReceive 中处理收到的数据
  // 2. 由 TWorkThread.ExecIOEvent 调用，未加锁！
  //    线程安全：严格来说不要访问主线程的任何控件。
end;

procedure TFormInIOCPStreamServer.InIOCPServer1DataSend(Sender: TBaseSocket;
  Size: Cardinal);
begin
  // 服务端发出数据
  // 由 TWorkThread.ExecIOEvent 调用，未加锁！
  //    线程安全：严格来说不要访问主线程的任何控件。
end;

procedure TFormInIOCPStreamServer.InIOCPServer1Disconnect(Sender: TObject;
  Socket: TBaseSocket);
begin
  // Socket 即将被关闭
end;

procedure TFormInIOCPStreamServer.InStreamManager1Receive(Socket: TStreamSocket;
  const Data: PAnsiChar; Size: Cardinal);
var
  S: String;
  i: Integer;
//  Stream: TFileStream;
begin
  // 重大调整，新版增加流服务管理器 TInStreamManager，在本事件处理收到的数据

  // 收到一个数据包（未必接收完毕）
  // Socket: 是 TStreamSocket!
  //   Data: 数据
  //   Size：数据长度

  // 把数据转为 String 显示
  SetString(S, Data, Size);
  memo1.lines.Add(S);

  // 数据开头为 客户端Id
  if (Socket.ClientId = '') then  // 未登记客户端 Id
  begin
    i := Pos(':', S);
    if (i > 0) then
      Socket.ClientId := Copy(S, 1, i - 1);
    if (Socket.ClientId = 'ADMIN') and (Socket.Role = crUnknown) then
      Socket.Role := crAdmin;
  end;

  // 客户端一定要能接收服务端的推送消息（被动接收消息）！

  // 客户端例子用 TIdTCPClient，反馈的数据要支持其协议
  //   TIdTCPClient 不直接支持被动接收消息，例子的客户端无法正常显示
  
{  if (Socket.Role = crAdmin) then  // 用 Indy TCP Client 的格式返回信息
    InStreamManager1.Broadcast(Socket, '123 GET_STATE'#13#10)  // 广播，收集客户端消息
  else
    InStreamManager1.SendTo(Socket, 'ADMIN', '123 GET_STATE'#13#10); // 推送状态消息给管理端 }

  // 可以先把设备发送来的数据保存到 Socket.Data 中
  Socket.SendData('123 RETURN OK'#13#10);

  // 有多种方式发送数据给客户端

  // 1. 发送内存块
//  Socket.SendData(Data, Size);

  // 2. 发送文本
//  Socket.SendData('Test Text 中文');

  // 3. 发送一个文件（retrun_stream.txt 内容是 html，包含报头+内容）
{  Stream := TFileStream.Create('retrun_stream.txt', fmShareDenyWrite);  // 要共享读，否则很慢
  Socket.SendData(Stream);   // 自动释放 Stream  }

  // 4. 直接打开文件发送 Handle(共享读)
//  Socket.SendData(InternalOpenFile('retrun_stream.txt'));

  // 5. 发送一个 Variant
//  Socket.SendDataVar(Value);  

end;

end.
