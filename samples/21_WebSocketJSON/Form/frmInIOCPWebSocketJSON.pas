unit frmInIOCPWebSocketJSON;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_managers, iocp_base, 
  http_objects, iocp_server, iocp_sockets, iocp_wsClients;

type
  TFormInIOCPWsJSON = class(TForm)             
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    InHttpDataProvider1: TInHttpDataProvider;
    InWebSocketManager1: TInWebSocketManager;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    Button3: TButton;
    InDatabaseManager1: TInDatabaseManager;
    InFileManager1: TInFileManager;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InHttpDataProvider1Accept(Sender: TObject; Request: THttpRequest;
      var Accept: Boolean);
    procedure InWebSocketManager1Upgrade(Sender: TObject; const Origin: string;
      var Accept: Boolean);
    procedure InWebSocketManager1Receive(Sender: TObject; Socket: TWebSocket);
    procedure InWSConnection1ReceiveData(Sender: TObject; const Msg: string);
    procedure InWSConnection1ReceiveMsg(Sender: TObject; Msg: TJSONResult);
    procedure InWSConnection1ReturnResult(Sender: TObject; Result: TJSONResult);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInIOCPWsJSON: TFormInIOCPWsJSON;

implementation

uses
  iocp_varis, iocp_utils, iocp_log, dm_iniocp_test;

{$R *.dfm}

procedure TFormInIOCPWsJSON.Button1Click(Sender: TObject);
begin
  // 新版兼容标准 WebSocket 消息，增加消息的 JSON 封装，支持大文件传输

  // 从 TInIOCPDataModule 继承新的数模 TdmInIOCPTest，
  //    注册数模类型即可（系统自动建数模实例）
  //    停止服务时会清空数模列表
  InDatabaseManager1.AddDataModule(TdmInIOCPTest, 'ADO_xzqh');

  iocp_log.TLogThread.InitLog(iocp_varis.gAppPath + 'log');  // 开启日志
  FrameIOCPSvrInfo1.Start(InIOCPServer1);
  InIOCPServer1.Active := True;
end;

procedure TFormInIOCPWsJSON.Button2Click(Sender: TObject);
begin
  FrameIOCPSvrInfo1.Stop;
  InIOCPServer1.Active := False;
  iocp_log.TLogThread.StopLog;  // 停止日志  
end;

procedure TFormInIOCPWsJSON.Button3Click(Sender: TObject);
begin
  InIOCPServer1AfterOpen(nil);
end;

procedure TFormInIOCPWsJSON.Button4Click(Sender: TObject);
var
  S, S2: AnsiString;
begin
  S := 'WebSocket 数据库查询.';
  S2 := System.UTF8Encode(S);

  memo1.Lines.Add('S2=' + IntToStr(Length(S2)));

  S := System.UTF8Decode(S2);

  memo1.Lines.Add(S);
end;

procedure TFormInIOCPWsJSON.FormCreate(Sender: TObject);
begin
  iocp_varis.gAppPath := ExtractFilePath(Application.ExeName);  // 程序路径
  iocp_utils.MyCreateDir(iocp_varis.gAppPath + 'log');  // 建日志目录
end;

procedure TFormInIOCPWsJSON.InHttpDataProvider1Accept(Sender: TObject;
  Request: THttpRequest; var Accept: Boolean);
begin
  // Accept 默认 = True
end;

procedure TFormInIOCPWsJSON.InIOCPServer1AfterOpen(Sender: TObject);
begin
//  Memo1.Lines.Clear;
  Memo1.Lines.Add('IP:' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('Port:' + IntToStr(InIOCPServer1.ServerPort));
end;

procedure TFormInIOCPWsJSON.InWebSocketManager1Receive(Sender: TObject; Socket: TWebSocket);
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
  // 1、       InData：本次收到的数据内容首位置（不是Data，以前版本有误）
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

        11: begin  // 执行数据库查询
          Memo1.Lines.Add('aaa=' + Socket.JSON.S['aaa']);
          // 查询
//          Socket.Result.Action := 11;  // 新版自动设置 Action
          TBusiWorker(Sender).DataModule.WebSocketQuery(Socket.JSON, Socket.Result);
          Socket.SendResult;  // 用默认字符集，非 UTF-8
        end;

        12: begin // 更新数据表
//          Socket.Result.Action := 12;   // 新版自动设置 Action

          TBusiWorker(Sender).DataModule.WebSocketUpdates(Socket.JSON, Socket.Result);

          Socket.SendResult;
        end;

        20: begin // 查询文件
//          Socket.Result.Action := 20;  // 新版自动设置 Action

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
          Memo1.Lines.Add('aaa=' + Socket.JSON.S['aaa']);
          Memo1.Lines.Add('bbb=' + Socket.JSON.S['BBB']);
          Memo1.Lines.Add('附件流名称=' + Socket.JSON.S['attach']);

          if Socket.JSON.HasAttachment then // 带附件 -> 建文件流接收附件，不建时忽略
            Socket.JSON.Attachment := TFileStream.Create('temp\服务端收到' + Socket.JSON.S['attach'], fmCreate);

          Socket.UserName := 'JSON';  // 新版支持分组 Socket.UserGroup！
    //     InWebSocketManager1.SendTo(Socket, 'ToUser');

    //      Socket.Result.S['return'] := 'test 返回消息';
    //      Socket.SendResult;  // 发送结果给客户端
        end;

      end;

    mtAttachment: begin
      // 2. InIOCP 扩展的 附件流 数据（此时 Socket.Complete = True）
      //    如果 Socket.JSON.Attachment 为空，照样会执行到此

      // 系统会自动关闭附件流 Socket.JSON.Attachment
      Memo1.Lines.Add('附件接收完毕（系统会自动关闭附件流）。');

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
      
      if Socket.Completed then // 消息接收完毕
      begin
        // 双字节的 UTF8To 系列函数的传入参数以 AnsiString 为主
        // 定义 S 为 AnsiString 更方便操作
        SetString(S, Socket.InData, Socket.FrameRecvSize); // 把消息转为 String

        Socket.UserName := System.Utf8ToAnsi(S); // XE10 还可以用 UTF8ToString(S)
        Memo1.Lines.Add(Socket.UserName);
        
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

procedure TFormInIOCPWsJSON.InWebSocketManager1Upgrade(Sender: TObject;
  const Origin: string; var Accept: Boolean);
begin
  // Origin: 申请升级 Socket 的来源，如：www.aaa.com
  // 在此判断是否允许升级为 WebSocket，默认 Accept=True
end;

procedure TFormInIOCPWsJSON.InWSConnection1ReceiveData(Sender: TObject; const Msg: string);
begin
  // Sender: 是 TInWSConnection
  //    Msg：收到消息文本

  // 这里收到的不是 InIOCP-JSON 消息（可能未封装的数据）
  // 测试方法：用浏览器发送一条消息，在服务端广播它

  Memo1.Lines.Add(Msg);
end;

procedure TFormInIOCPWsJSON.InWSConnection1ReceiveMsg(Sender: TObject; Msg: TJSONResult);
begin
  //  Sender: 是 TInWSConnection
  //     Msg: JSON 消息，系统会自动释放

  // 这里收到的是其他客户端推送来到 InIOCP-JSON 消息（被动接收）
  // 测试方法：用 TJSONMessage 发送消息，在服务端广播它

  Memo1.Lines.Add('收到推送消息（被动接收）：' + Msg.S['push']);
end;

procedure TFormInIOCPWsJSON.InWSConnection1ReturnResult(Sender: TObject; Result: TJSONResult);
begin
  // Sender: 是 TInWSConnection
  // Result: JSON 消息，系统会自动释放
  
  // Result.MsgType: 消息类型
  //   1. mtJSON：Result 是 JSON 消息
  //   2. mtAttachment：Result 是附件流（系统自动释放，不要 Result.Attachment.Free）

  // Result.AttachType: 附件类型，两种：
  //   1. 数据流；2. 数据集

  // 测试方法：服务端收到消息后，设置 Socket.Result，用 Socket.SendResult 发送回来。

  Memo1.Lines.Add('====== 客户端 ======');

  if Result.MsgType = mtJSON then
  begin
    if Result.HasAttachment then  // 有附件流，接收（可以不接收）
      Result.Attachment := TFileStream.Create('temp\客户端收到' + Result.S['attach'], fmCreate);
    Memo1.Lines.Add('服务端返回给自己的 JSON 消息: ' + Result.S['return']);
  end else  // 是附件流
  if Assigned(Result.Attachment) then  // 收到文件流
  begin
    if Result.HasAttachment then
      Memo1.Lines.Add('接收文件完毕（系统会自动关闭附件流）')
    else
      Memo1.Lines.Add('接收数据集完毕（JSON格式）')
  end else
    Memo1.Lines.Add('没有接收文件');
end;

end.
