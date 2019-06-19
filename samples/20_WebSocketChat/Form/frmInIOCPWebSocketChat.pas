unit frmInIOCPWebSocketChat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_managers,
  http_objects, iocp_server, iocp_sockets;

type
  TFormInIOCPWSChat = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    InHttpDataProvider1: TInHttpDataProvider;
    InWebSocketManager1: TInWebSocketManager;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InHttpDataProvider1Accept(Sender: TObject; Request: THttpRequest;
      var Accept: Boolean);
    procedure InWebSocketManager1Upgrade(Sender: TObject; const Origin: string;
      var Accept: Boolean);
    procedure InWebSocketManager1Receive(Sender: TObject; Socket: TWebSocket);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInIOCPWSChat: TFormInIOCPWSChat;

implementation

uses
  iocp_base, iocp_varis, iocp_utils, iocp_log;

{$R *.dfm}

procedure TFormInIOCPWSChat.Button1Click(Sender: TObject);
begin
  iocp_log.TLogThread.InitLog(iocp_varis.gAppPath + 'log');  // 开启日志
  FrameIOCPSvrInfo1.Start(InIOCPServer1);
  InIOCPServer1.Active := True;
end;

procedure TFormInIOCPWSChat.Button2Click(Sender: TObject);
begin
  FrameIOCPSvrInfo1.Stop;
  InIOCPServer1.Active := False;
  iocp_log.TLogThread.StopLog;  // 停止日志  
end;

procedure TFormInIOCPWSChat.FormCreate(Sender: TObject);
begin
  iocp_varis.gAppPath := ExtractFilePath(Application.ExeName);  // 程序路径
  iocp_utils.MyCreateDir(iocp_varis.gAppPath + 'log');  // 建日志目录
end;

procedure TFormInIOCPWSChat.InHttpDataProvider1Accept(Sender: TObject;
  Request: THttpRequest; var Accept: Boolean);
begin
  // Accept 默认 = True
end;

procedure TFormInIOCPWSChat.InIOCPServer1AfterOpen(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('IP:' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('Port:' + IntToStr(InIOCPServer1.ServerPort));  
end;

procedure TFormInIOCPWSChat.InWebSocketManager1Receive(Sender: TObject; Socket: TWebSocket);
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
 
  if Socket.Complete then // 消息接收完毕
  begin
    // 双字节的 UTF8To 系列函数的传入参数以 AnsiString 为主
    // 定义 S 为 AnsiString 更方便操作
    SetString(S, Socket.Data, Socket.FrameRecvSize); // 把消息转为 String
    Socket.UserName := System.Utf8ToAnsi(S); // XE10 还可以用 UTF8ToString(S)

    // 显示
    Memo1.Lines.Add(Socket.UserName);

    // 返回全部客户端名称列表
    InWebSocketManager1.GetUserList(Socket);

    // 告诉全部客户端，UserName 上线了
    InWebSocketManager1.Broadcast('聊天室广播：' + Socket.UserName + '上线了.');
    
{    // 1. 反馈消息给客户端
   Socket.SendData('服务器反馈：' + Socket.UserName);

   // 以下方法测试正常：

    // 2. 返回在线且已经登记用户名的全部用户的
    //    名称列表给客户端（JSON格式，字段为 NAME，UTF-8 字符集）
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
    Socket.Role := crAdmin;
    if (Socket.Role >= crAdmin) and (Socket.UserName <> 'USER_A') then  // 有权限
      InWebSocketManager1.Delete(Socket, 'USER_A');    }

//    mmoServer.Lines.Add('收到 WebSocket 消息：' + S);  // 大并发不要显示   
  end;
end;

procedure TFormInIOCPWSChat.InWebSocketManager1Upgrade(Sender: TObject;
  const Origin: string; var Accept: Boolean);
begin
  // Origin: 申请升级 Socket 的来源，如：www.aaa.com

  // 在此判断是否允许升级为 WebSocket，默认 Accept=True
end;

end.
