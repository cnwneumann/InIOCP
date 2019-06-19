unit svInIOCPService;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  http_objects, iocp_managers, iocp_server;

type
  TInIOCP_HTTP_Service = class(TService)
    InIOCPServer1: TInIOCPServer;
    InHttpDataProvider1: TInHttpDataProvider;
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure InHttpDataProvider1Get(Sender: TObject; Request: THttpRequest;
      Respone: THttpRespone);
    procedure InHttpDataProvider1Post(Sender: TObject; Request: THttpRequest;
      Respone: THttpRespone);
    procedure InHttpDataProvider1InvalidSession(Sender: TObject;
      Request: THttpRequest; Respone: THttpRespone);
  private
    { Private declarations }
    FWebSitePath: String;
  public
    { Public declarations }
    function GetServiceController: TServiceController; override;
  end;

var
  InIOCP_HTTP_Service: TInIOCP_HTTP_Service;

implementation

uses
  iocp_varis, iocp_utils, iocp_log, http_base;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  InIOCP_HTTP_Service.Controller(CtrlCode);
end;

function TInIOCP_HTTP_Service.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TInIOCP_HTTP_Service.InHttpDataProvider1Get(Sender: TObject;
  Request: THttpRequest; Respone: THttpRespone);
begin
  // URI要与磁盘的文件对应
  if Request.URI = '/' then
    Respone.TransmitFile(FWebSitePath + 'index.htm')
  else  // 调用函数时要单独判断
    Respone.TransmitFile(FWebSitePath + Request.URI);
end;

procedure TInIOCP_HTTP_Service.InHttpDataProvider1InvalidSession(
  Sender: TObject; Request: THttpRequest; Respone: THttpRespone);
begin
  // 请求带 Session，但 Session 无效时调用此事件
  if (Request.URI = '/ajax/login.htm') then
    Respone.TransmitFile(FWebSitePath + '\ajax\login.htm')
  else
    Respone.SetContent(http_base.HTTP_INVALID_SESSION);
end;

procedure TInIOCP_HTTP_Service.InHttpDataProvider1Post(Sender: TObject;
  Request: THttpRequest; Respone: THttpRespone);
begin
  // Post：已经接收完毕，调用此事件
  //   此时：Request.Complete = True
  if Request.URI = '/ajax/login.htm' then  // 动态页面
  begin
    if (Request.Params.AsString['user_name'] <> '') and
      (Request.Params.AsString['user_password'] <> '') then   // 登录成功
    begin
      Respone.CreateSession;  // 生成 Session！
      Respone.TransmitFile(FWebSitePath + 'ajax\ajax.htm');
    end else
      Respone.Redirect('ajax/login.htm');  // 重定位到登录页面
  end else
  begin
    Respone.SetContent('<html><body>In-IOCP HTTP 服务！<br>提交成功！<br>');
    Respone.AddContent('<a href="' + Request.URI + '">返回</a><br></body></html>');
  end;
end;

procedure TInIOCP_HTTP_Service.ServiceCreate(Sender: TObject);
begin
  iocp_varis.gAppPath := ExtractFilePath(ParamStr(0));  // 程序路径
  FWebSitePath := iocp_varis.gAppPath + iocp_utils.AddBackslash(InHttpDataProvider1.RootDirectory);
  iocp_utils.MyCreateDir(iocp_varis.gAppPath + 'log');  // 建日志目录
end;

procedure TInIOCP_HTTP_Service.ServiceStart(Sender: TService; var Started: Boolean);
begin
  iocp_log.TLogThread.InitLog(iocp_varis.gAppPath + 'log');  // 开启日志
  InIOCPServer1.Active := True;
  Started := InIOCPServer1.Active;
end;

procedure TInIOCP_HTTP_Service.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Stopped := True;
  InIOCPServer1.Active := False;
  iocp_log.TLogThread.StopLog;
end;

end.
