unit frmOuterProxyServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, fmIOCPSvrInfo, StdCtrls, iocp_managers, iocp_server, iocp_sockets;

type
  TFormIOCPOutProxySvr = class(TForm)
    Button1: TButton;
    Button2: TButton;
    InIOCPServer1: TInIOCPServer;
    InIOCPBroker1: TInIOCPBroker;
    Label1: TLabel;
    Edit1: TEdit;
    EditPort: TEdit;
    Memo1: TMemo;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1DblClick(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
    procedure InIOCPBroker1Bind(Sender: TSocketBroker; const Data: PAnsiChar;
      DataSize: Cardinal);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormIOCPOutProxySvr: TFormIOCPOutProxySvr;

implementation

uses
  iocp_varis, iocp_log, iocp_base, iocp_utils, IniFiles;

{$R *.dfm}

procedure TFormIOCPOutProxySvr.Button1Click(Sender: TObject);
begin
  // 注意：TInIOCPBroker.ProxyType 必须为 ptOuter！
  
  // 开启日志
  iocp_log.TLogThread.InitLog(FAppDir + 'log');

  InIOCPServer1.ServerAddr := Edit1.Text;
  InIOCPServer1.ServerPort := StrToInt(EditPort.Text);

  InIOCPServer1.Active := True;

  FrameIOCPSvrInfo1.Start(InIOCPServer1);
end;

procedure TFormIOCPOutProxySvr.Button2Click(Sender: TObject);
begin
  FrameIOCPSvrInfo1.Stop;         // 停止统计
  InIOCPServer1.Active := False;  // 停止服务
  iocp_log.TLogThread.StopLog;    // 停止日志
end;

procedure TFormIOCPOutProxySvr.Edit1DblClick(Sender: TObject);
begin
  if Edit1.Text = '127.0.0.1' then
    Edit1.Text := '192.168.1.196'
  else
    Edit1.Text := '127.0.0.1';
end;

procedure TFormIOCPOutProxySvr.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Button2Click(nil);
end;

procedure TFormIOCPOutProxySvr.FormCreate(Sender: TObject);
begin
  // 准备路径
  FAppDir := ExtractFilePath(Application.ExeName);
  iocp_Varis.gAppPath := FAppDir;
  MyCreateDir(FAppDir + 'log');   // 日志路径

  // 读入参数
  // 外部服务设置 TInIOCPBroker.ProxyType = ptOuter 即可，
  // 与协议 TInIOCPBroker.Protocol 无关

  // 此时内部的反向代理设置为：
  // TInIOCPBroker.ProxyType = ptDefault 且 OuterProxy.ServerAddr 不为空

  with TIniFile.Create(FAppDir + 'settings.ini') do
  begin
    Edit1.Text := ReadString('OuterOptions', 'LocalHost', '127.0.0.1');
    EditPort.Text := ReadString('OuterOptions', 'LocalPort', '80');

    Free;
  end;
end;

procedure TFormIOCPOutProxySvr.InIOCPBroker1Bind(Sender: TSocketBroker;
  const Data: PAnsiChar; DataSize: Cardinal);
begin
  // 说明：
  // Sender 是外网客户端，把它和反向代理的连接关联，
  // 建立起通讯通道，Sender 通过这个通道与反向代理通讯，
  // 反向代理再与内部服务器通讯。

  // 外部代理 TInIOCPBroker 的属性设置：
  //   必须设 TInIOCPBroker.ProxyType = ptOuter
  //   内部自动绑定内部连接，不用在此事件写任何代码！

  // 一、内部只有一个反向代理时，不用再设置其他属性值，
  //     此情况只作简单的数据转发，支持任何协议；

  // 二、如果使用 HTTP 协议，且有多个反向代理时（多个局域网，每局域网一个反向代理）
  //     要同时设 TInIOCPBroker.Protocol = tpHTTP，并要对 HTTP 客户端请求报头的 Host
  //     作必要的调整，用如下格式：
  //         Host: 局域网主机IP:端口@反向代理标志
  //     如 Host:127.0.0.1:12302@分公司A，意思就是要连接到 分公司A 的主机 127.0.0.1:12302

  //  反向代理标志（分公司A）就是反向代理的 TInIOCPBroker.BrokerId，且标志唯一。

  // 三、多反向代理只支持 HTTP 协议。
  
end;

procedure TFormIOCPOutProxySvr.InIOCPServer1AfterClose(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('LocalHost=' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('LocalPort=' + IntToStr(InIOCPServer1.ServerPort));

  Button1.Enabled := not InIOCPServer1.Active;
  Button2.Enabled := InIOCPServer1.Active;
end;

end.
