unit frmInIOCPHttpServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, iocp_sockets, iocp_managers, iocp_server,
  http_base, http_objects, fmIOCPSvrInfo;

type
  TFormInIOCPHttpServer = class(TForm)
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    InHttpDataProvider1: TInHttpDataProvider;
    btnStart: TButton;
    btnStop: TButton;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    InDatabaseManager1: TInDatabaseManager;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure InHttpDataProvider1Get(Sender: TObject; Request: THttpRequest;
      Response: THttpResponse);
    procedure InHttpDataProvider1Post(Sender: TObject;
      Request: THttpRequest; Response: THttpResponse);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure InHttpDataProvider1Accept(Sender: TObject; Request: THttpRequest;
      var Accept: Boolean);
    procedure InHttpDataProvider1ReceiveFile(Sender: TObject;
      Request: THttpRequest; const FileName: string; Data: PAnsiChar;
      DataLength: Integer; State: THttpPostState);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPHttpServer: TFormInIOCPHttpServer;

implementation

uses
  iocp_log, iocp_utils, iocp_msgPacks, http_utils, dm_iniocp_test;
  
{$R *.dfm}

procedure TFormInIOCPHttpServer.btnStartClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  iocp_log.TLogThread.InitLog;              // 开启日志

  // 注册数模类名到 InDatabaseManager1
  //   测试：用不同说明，注册两次 TdmInIOCPTest
  InDatabaseManager1.AddDataModule(TdmInIOCPTest, 'http_dataModule');
  InDatabaseManager1.AddDataModule(TdmInIOCPTest, 'http_dataModule2');

  InIOCPServer1.Active := True;               // 开启服务
  FrameIOCPSvrInfo1.Start(InIOCPServer1);     // 开始统计
end;

procedure TFormInIOCPHttpServer.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False;   // 停止服务
  FrameIOCPSvrInfo1.Stop;          // 停止统计
  iocp_log.TLogThread.StopLog;   // 停止日志
end;

procedure TFormInIOCPHttpServer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  btnStopClick(nil);
end;

procedure TFormInIOCPHttpServer.FormCreate(Sender: TObject);
var
  WebSite: String;
begin
  // 本地路径
  FAppDir := ExtractFilePath(Application.ExeName);
  WebSite := AddBackslash(InHttpDataProvider1.RootDirectory);

  MyCreateDir(FAppDir + 'log');

  MyCreateDir(WebSite + 'downloads');
  MyCreateDir(WebSite + 'uploads');
end;

procedure TFormInIOCPHttpServer.InHttpDataProvider1Accept(Sender: TObject;
  Request: THttpRequest; var Accept: Boolean);
begin
  // 参数表有调整！
  // 在此判断是否接受请求:
  //   Request.Method: 方法
  //      Request.URI：路径/资源
  case Request.Method of
    hmGet:
      Accept := True; // (Request.URI = '/') or (Request.URI = '/a') or (Request.URI = '/t');
    hmPost:
      Accept := True; // (Request.URI = '/') or (Request.URI = '/a') or (Request.URI = '/t');
    else    // 测试其他方法
      Accept := True;
  end;
end;

procedure TFormInIOCPHttpServer.InHttpDataProvider1Get(Sender: TObject;
  Request: THttpRequest; Response: THttpResponse);
var
  Stream: TStream;
  FileName: string;
begin
  // Get: 检查请求命令，反馈数据

  // 系统的全部服务端管理组件的事件中，类型为 TObject 的 Sender
  // 都是任务执行者实例（Worker），其中的 TBusiWorker(Sender).DataModule
  // 是数模实例（要加入数据库管理组件），可以用来操作数据库，如：

  //  TBusiWorker(Sender).DataModule.HttpExecSQL(Request, Response);
  //  TBusiWorker(Sender).DataModule.HttpExecQuery(Request, Response);
  //  TBusiWorker(Sender).DataModules[1].HttpExecSQL(Request, Response);
  //  TBusiWorker(Sender).DataModules[1].HttpExecQuery(Request, Response);

  // 用 iocp_utils.DataSetToJSON() 把数据集快速转为 JSON。

  // 详细请看数模基类单元 iocp_datModuleIB.pas。
  // 本例子的数模是 mdTestDatabase.TInIOCPDataModuleTest 的实例。

  // 1. 下载文件 ==============================
  // InHttpDataProvider1.RootDirectory 是网站路径

  if Pos('/downloads', Request.URI) > 0 then
  begin
    FileName := FAppDir + Request.URI;
    if Request.URI = '/web_site/downloads/中文-A09.txt' then
      Response.TransmitFile(FileName)          // IE浏览器自动显示
    else
    if Request.URI = '/web_site/downloads/httptest.exe' then
      Response.TransmitFile(FileName)
    else
    if Request.URI = '/web_site/downloads/InIOCP技术要点.doc' then
    begin
      Stream := TIOCPDocument.Create(AdjustFileName(FileName));
      Response.SendStream(Stream);         // 发送文件流（自动释放）
    end else
    if Request.URI = '/web_site/downloads/InIOCP技术要点2.doc' then
    begin
      FileName := FAppDir + '/web_site/downloads/InIOCP技术要点.doc';
      Stream := TIOCPDocument.Create(AdjustFileName(FileName));
      Response.SendStream(Stream, True);   // 压缩文件流（自动释放）
    end else
    if Request.URI = '/web_site/downloads/jdk-8u77-windows-i586.exe' then
    begin
      // 测试大文件下载（支持断点续传）
      Response.TransmitFile('F:\Backup\jdk-8u77-windows-i586.exe');
    end else
    if Request.URI = '/web_site/downloads/test.jpg' then
    begin
      Response.TransmitFile('web_site\downloads\test.jpg');
    end else        
    begin           // 测试 chunk，分块发送
      Stream := TIOCPDocument.Create(AdjustFileName(FileName));
      try
        Response.SendChunk(Stream);  // 立刻发送，不释放（改进，内部自动发送结束标志）
      finally
        Stream.Free;
      end;
    end;

  end else

  // 2. ajax 动态页面，参考网页文件 login.htm、ajax.htm
  if Pos('/ajax', Request.URI) > 0 then
  begin

    // 放在前面，不检查 Session，方便 ab.exe 大并发测试 
    if Request.URI = '/ajax/query_xzqh.pas' then
    begin
      // AJAX 查询数据表，方法：
      // 1. 使用默认数模：TBusiWorker(Sender).DataModule.HttpExecQuery(Request, Respone)
      // 2. 指定数模：TBusiWorker(Sender).DataModules[1].HttpExecQuery(Request, Respone)

      // 测试大并发查询数据库
      //   使用工具 ab.exe，URL 用：
      //   /ajax/query_xzqh.pas?SQL=Select_tbl_xzqh2
      //   使用 Select_tbl_xzqh2 对应的 SQL 命令查询数据
      TBusiWorker(Sender).DataModule.HttpExecQuery(Request, Response);

      // 有多个数模时，使用 DataModules[x]
{     if Respone.HasSession then
        TBusiWorker(Sender).DataModules[1].HttpExecQuery(Request, Respone)
      else
        Respone.SetContent(HTTP_INVALID_SESSION); }
    end else

    if (Request.URI = '/ajax/login') then  // 登录
      Response.TransmitFile('web_site\ajax\login.htm')
    else

    // 退出登录 或 Session 无效
    if (Request.URI = '/ajax/quit') or not Response.HasSession then
    begin
      // 客户端的 ajax 代码内部不能监测重定位 302
      // 调整：InvalidSession 改为 RemoveSession，不直接发送 HTTP_INVALID_SESSION      
      Response.RemoveSession;  // 删除、返回无效的 Cookie, 安全退出
      Response.SetContent(HTTP_INVALID_SESSION);  // 客户端重定位到登录页面
    end else

    if Request.URI = '/ajax/ajax_text.txt' then
      // AJAX 请求文本，IE 可能乱码，chrome 正常
      Response.TransmitFile('web_site\ajax\ajax_text.txt')
    else

    if Request.URI = '/ajax/server_time.pas' then
      // AJAX 取服务器时间
      Response.SetContent('<p>服务器时间：' + GetHttpGMTDateTime + '</p>');

  end else
  begin

    // 3. 普通页面 ==============================
    // 三种类型的表单，POST 的参数编码不同，解码不同
    if Request.URI = '/test_a.htm' then   // 上传文件，表单类型：multipart/form-data
      Response.TransmitFile('web_site\html\test_a.htm')
    else
    if Request.URI = '/test_b.htm' then   // 表单类型：application/x-www-form-urlencoded
      Response.TransmitFile('web_site\html\test_b.htm')
    else
    if Request.URI = '/test_c.htm' then   // 表单类型：text/plain
      Response.TransmitFile('web_site\html\test_c.htm')
    else                              // 首页
    if (Request.URI = '/favicon.ico') then
      Response.StatusCode := 204       // 没有东西
    else
      Response.TransmitFile('web_site\html\index.htm');
  end;

end;

procedure TFormInIOCPHttpServer.InHttpDataProvider1Post(Sender: TObject;
  Request: THttpRequest; Response: THttpResponse);
begin
  // Post：已经接收完毕，调用此事件
  //   此时：Request.Complete = True
  if Request.URI = '/ajax/login' then  // 动态页面
  begin
    with memo1.Lines do
    begin
      Add('登录信息:');
      Add(' userName=' + Request.Params.AsString['user_name']);
      Add(' password=' + Request.Params.AsString['user_password']);
    end;
    if (Request.Params.AsString['user_name'] <> '') and
      (Request.Params.AsString['user_password'] <> '') then   // 登录成功
    begin
      Response.CreateSession;  // 生成 Session！
      Response.TransmitFile('web_site\ajax\ajax.htm');
    end else
      Response.Redirect('/ajax/login');  // 重定位到登录页面
  end else
  begin
    with memo1.Lines do
    begin
      Add('HTTP 服务:');
      Add('   textline=' + Request.Params.AsString['textline']);
      Add('  textline2=' + Request.Params.AsString['textline2']);
      Add('    onefile=' + Request.Params.AsString['onefile']);
      Add('  morefiles=' + Request.Params.AsString['morefiles']);
    end;
    Response.SetContent('<html><body>In-IOCP HTTP 服务！<br>提交成功！<br>');
    Response.AddContent('<a href="' + Request.URI + '">返回</a><br></body></html>');
  end;
end;

procedure TFormInIOCPHttpServer.InHttpDataProvider1ReceiveFile(Sender: TObject;
  Request: THttpRequest; const FileName: string; Data: PAnsiChar;
  DataLength: Integer; State: THttpPostState);
var
  S: String;
begin
  // 参数表有调整！
  // Post: 已经接收完毕，收到上传的文件，保存到文件流
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

procedure TFormInIOCPHttpServer.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

procedure TFormInIOCPHttpServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
  Memo1.Lines.Add('ip: ' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('port: ' + IntToStr(InIOCPServer1.ServerPort));
end;

end.
