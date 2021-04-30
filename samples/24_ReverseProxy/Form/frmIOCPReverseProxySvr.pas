unit frmIOCPReverseProxySvr;

interface

uses         
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iocp_server, iocp_managers, fmIOCPSvrInfo, StdCtrls,
  iocp_sockets;

type
  TFormInIOCPRecvProxySvr = class(TForm)
    InIOCPBroker1: TInIOCPBroker;
    InIOCPServer1: TInIOCPServer;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    Edit1: TEdit;
    Label1: TLabel;
    EditPort: TEdit;
    procedure InIOCPBroker1Bind(Sender: TSocketBroker; const Data: PAnsiChar;
      DataSize: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit1DblClick(Sender: TObject);
    procedure InIOCPBroker1Accept(Sender: TSocketBroker; const Host: string;
      Port: Integer; var Accept: Boolean);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPRecvProxySvr: TFormInIOCPRecvProxySvr;
  ProxyWindowCount: Integer = 1;
  
implementation

uses
  iocp_log, iocp_varis, iocp_base, iocp_utils, http_utils, IniFiles;

{$R *.dfm}

procedure TFormInIOCPRecvProxySvr.Button1Click(Sender: TObject);
begin
  // TInIOCPBroker.ProxyType = ptDefault 且 OuterServer.ServerAddr 不为空时
  // 是反向代理，要部署于能访问外部代理服务器的地方。
  // 反向代理会主动向外部代理发起请求，与外部代理建立连接，
  // 通过这些连接与内部服务进行通讯。

  // 开启日志
  iocp_log.TLogThread.InitLog(FAppDir + 'log');

  InIOCPServer1.ServerAddr := Edit1.Text;
  InIOCPServer1.ServerPort := StrToInt(EditPort.Text);
  InIOCPServer1.Active := True;

  FrameIOCPSvrInfo1.Start(InIOCPServer1);
end;

procedure TFormInIOCPRecvProxySvr.Button2Click(Sender: TObject);
begin
  FrameIOCPSvrInfo1.Stop;         // 停止统计
  InIOCPServer1.Active := False;  // 停止服务
  iocp_log.TLogThread.StopLog;    // 停止日志
end;

procedure TFormInIOCPRecvProxySvr.Edit1DblClick(Sender: TObject);
begin
  if Edit1.Text = '127.0.0.1' then
    Edit1.Text := '192.168.1.196'
  else
    Edit1.Text := '127.0.0.1';
end;

procedure TFormInIOCPRecvProxySvr.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Button2Click(nil);
end;

procedure TFormInIOCPRecvProxySvr.FormCreate(Sender: TObject);
begin
  // 准备路径
  FAppDir := ExtractFilePath(Application.ExeName);
  iocp_Varis.gAppPath := FAppDir;
  MyCreateDir(FAppDir + 'log');   // 日志路径

  // 读入参数
  // 反向代理要向外部服务器发起连接，此时：
  // TInIOCPBroker.ProxyType = ptDefault 且 OuterProxy.ServerAddr 不为空

  with TIniFile.Create(FAppDir + 'settings.ini') do
  begin
    Edit1.Text := ReadString('ReverseOptions', 'LocalHost', '127.0.0.1');
    EditPort.Text := ReadString('ReverseOptions', 'LocalPort', '900');

    // 设置服务协议
    if (ReadString('ReverseOptions', 'Protocol', 'HTTP') = 'HTTP') then
      InIOCPBroker1.Protocol := tpHTTP 
    else
      InIOCPBroker1.Protocol := tpNone;

    // 此三参数对 HTTP 协议无效
    InIOCPBroker1.BrokerId := ReadString('ReverseOptions', 'BrokerId', '分公司A');  // 测试多个反向代理，模拟多局域网
    InIOCPBroker1.InnerServer.ServerAddr := ReadString('ReverseOptions', 'InnerServerAddr', '127.0.0.1');
    InIOCPBroker1.InnerServer.ServerPort := ReadInteger('ReverseOptions', 'InnerServerPort', 3060);

    // 反向代理的内部服务必须设置 OuterServer 地址端口
    InIOCPBroker1.OuterServer.ServerAddr := ReadString('OuterOptions', 'LocalHost', '127.0.0.1');
    InIOCPBroker1.OuterServer.ServerPort := ReadInteger('OuterOptions', 'LocalPort', 900);

    Free;
  end;  
end;

procedure TFormInIOCPRecvProxySvr.InIOCPBroker1Accept(Sender: TSocketBroker;
  const Host: string; Port: Integer; var Accept: Boolean);
begin
  // 版本：2.5.30.1221 增加，判断是否允许连接到 Host 的 Port 端口
  // 默认 Accept := True，Accept := False 不允许
  // TInIOCPBroker.ProxyType = ptOuter 时忽略此事件  
end;

procedure TFormInIOCPRecvProxySvr.InIOCPBroker1Bind(Sender: TSocketBroker;
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

procedure TFormInIOCPRecvProxySvr.InIOCPServer1AfterOpen(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('LocalHost=' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('LocalPort=' + IntToStr(InIOCPServer1.ServerPort));
  Memo1.Lines.Add('BrokerId=' + InIOCPBroker1.BrokerId);
  Memo1.Lines.Add('InnerServerAddr=' + InIOCPBroker1.InnerServer.ServerAddr);
  Memo1.Lines.Add('InnerServerPort=' + IntToStr(InIOCPBroker1.InnerServer.ServerPort));
  Memo1.Lines.Add('OuterServerAddr=' + InIOCPBroker1.OuterServer.ServerAddr);
  Memo1.Lines.Add('OuterServerPort=' + IntToStr(InIOCPBroker1.OuterServer.ServerPort));

  Button1.Enabled := not InIOCPServer1.Active;
  Button2.Enabled := InIOCPServer1.Active;
end;

end.
