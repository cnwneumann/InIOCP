unit frmIOCPProxyServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iocp_server, iocp_managers, fmIOCPSvrInfo, StdCtrls,
  iocp_sockets;

type
  TFormInIOCPProxySvr = class(TForm)
    InIOCPBroker1: TInIOCPBroker;
    InIOCPServer1: TInIOCPServer;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    Label1: TLabel;
    Edit1: TEdit;
    EditPort: TEdit;
    Label2: TLabel;
    procedure InIOCPBroker1Bind(Sender: TSocketBroker; const Data: PAnsiChar;
      DataSize: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure InIOCPBroker1Accept(Sender: TSocketBroker; const Host: string;
      Port: Integer; var Accept: Boolean);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPProxySvr: TFormInIOCPProxySvr;

implementation

uses
  iocp_log, IniFiles, iocp_varis, iocp_utils, iocp_base, http_utils;

{$R *.dfm}

procedure TFormInIOCPProxySvr.Button1Click(Sender: TObject);
begin
  // 开启日志
  iocp_log.TLogThread.InitLog(FAppDir + 'log');
  iocp_utils.IniDateTimeFormat;   // 设置时间格式

  // InIOCPBroker1 的 ProxyType = ptDefault 且 ReverseProxy.ServerAddr 为空时
  // 是普通代理，要部署于用户能直接访问得到的地方。

  InIOCPServer1.ServerAddr := Edit1.Text;
  InIOCPServer1.ServerPort := StrToInt(EditPort.Text);
    
  InIOCPServer1.Active := True;
  FrameIOCPSvrInfo1.Start(InIOCPServer1);
end;

procedure TFormInIOCPProxySvr.Button2Click(Sender: TObject);
begin
  FrameIOCPSvrInfo1.Stop;         // 停止统计
  InIOCPServer1.Active := False;  // 停止服务
  iocp_log.TLogThread.StopLog;    // 停止日志
end;

procedure TFormInIOCPProxySvr.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  InIOCPServer1.Active := False;
end;

procedure TFormInIOCPProxySvr.FormCreate(Sender: TObject);
begin
  // 准备路径
  FAppDir := ExtractFilePath(Application.ExeName);
  iocp_Varis.gAppPath := FAppDir;
  MyCreateDir(FAppDir + 'log');   // 日志路径

  // 读入参数：
  // 设置 TInIOCPServer 的 IOCPBroker 组件即可（此时其他管理组件无效）；
  // 普通代理：InIOCPBroker.ProxyType = ptDefault 且 OuterProxy.ServerAddr 为空

  with TIniFile.Create(FAppDir + 'settings.ini') do
  begin
    Edit1.Text := ReadString('Options', 'LocalHost', '127.0.0.1');
    EditPort.Text := ReadString('Options', 'LocalPort', '80');

    if (ReadString('Options', 'Protocol', 'HTTP') = 'HTTP') then  // HTTP 协议
      InIOCPBroker1.Protocol := tpHTTP // 内网可以多主机
    else begin
      // 未定协议，设置内部默认主机
      InIOCPBroker1.Protocol := tpNone;
      InIOCPBroker1.InnerServer.ServerAddr := ReadString('Options', 'InnerServerAddr', '127.0.0.1');
      InIOCPBroker1.InnerServer.ServerPort := ReadInteger('Options', 'InnerServerPort', 1200);
    end;

    Free;
  end;

end;

procedure TFormInIOCPProxySvr.InIOCPBroker1Accept(Sender: TSocketBroker;
  const Host: string; Port: Integer; var Accept: Boolean);
begin
  // 版本：2.5.30.1221 增加，判断是否允许连接到 Host 的 Port 端口
  // 默认 Accept := True，Accept := False 不允许
  // TInIOCPBroker.ProxyType = ptOuter 时忽略此事件
end;

procedure TFormInIOCPProxySvr.InIOCPBroker1Bind(Sender: TSocketBroker;
  const Data: PAnsiChar; DataSize: Cardinal);
begin
  //   Sender: 为代理对象 TSocketBroker
  //     Data: 收到的数据地址（禁止释放）
  // DataSize: 收到的数据的长度

  // Sender 的过程：
  //    CreateBroker: 建一个内部代理对象

  // 一、通用用法：
  //     内部只有一个主机时可以直接 CreateBroker
  Sender.CreateBroker(InIOCPBroker1.InnerServer.ServerAddr,
                      InIOCPBroker1.InnerServer.ServerPort);  // 连接默认的内部主机

  // 二、HTTP、WebSocket 协议的用法：
  //   1. 只有一个主机时直接用 CreateBroker，效率最高；
  //   2. 设 TInIOCPBroker.Protocol = tpHTTP，系统内部自动分析请求命令，
  //      提取报头的 Host 信息，建立对应的连接，没有 Host 时会连接到
  //      TInIOCPBroker.InnerServer 指定的主机（默认的内部主机）；
  //   3. TInIOCPBroker.Protocol = tpHTTP 时效率稍低，但无须在当前事件写任何代码。

end;

procedure TFormInIOCPProxySvr.InIOCPServer1AfterOpen(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('LocalHost=' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('LocalPort=' + IntToStr(InIOCPServer1.ServerPort));

  Memo1.Lines.Add('InnerServerAddr=' + InIOCPBroker1.InnerServer.ServerAddr);
  Memo1.Lines.Add('InnerServerPort=' + IntToStr(InIOCPBroker1.InnerServer.ServerPort));

  if InIOCPBroker1.Protocol = tpHTTP then
    Memo1.Lines.Add('Protocol=HTTP')
  else
    Memo1.Lines.Add('Protocol=None');

  Button1.Enabled := not InIOCPServer1.Active;
  Button2.Enabled := InIOCPServer1.Active;
end;

end.
