unit frmInIOCPDBServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_base, iocp_clients, iocp_server,
  iocp_sockets, iocp_managers, iocp_msgPacks;

type
  TFormInIOCPDBServer = class(TForm)
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    btnStart: TButton;
    btnStop: TButton;
    InClientManager1: TInClientManager;
    InDatabaseManager1: TInDatabaseManager;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure InClientManager1Login(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure FormCreate(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPDBServer: TFormInIOCPDBServer;

implementation

uses
  iocp_log, iocp_varis, iocp_utils, dm_iniocp_test;

{$R *.dfm}

procedure TFormInIOCPDBServer.btnStartClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  
  iocp_log.TLogThread.InitLog;  // 开启日志

  // 注册数模类（可以多种、多个数据库连接）
  InDatabaseManager1.AddDataModule(TdmInIOCPTest, 'Access-行政区划');
//  InDatabaseManager1.AddDataModule(TdmFirebird, 'Firebird-设备');
//  InDatabaseManager1.AddDataModule(TdmFirebird2, 'Firebird-人力资源');

  InIOCPServer1.Active := True; // 开启服务
  FrameIOCPSvrInfo1.Start(InIOCPServer1);  // 开始统计
end;

procedure TFormInIOCPDBServer.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False;   // 停止服务
  FrameIOCPSvrInfo1.Stop;          // 停止统计
  iocp_log.TLogThread.StopLog;   // 停止日志
end;

procedure TFormInIOCPDBServer.FormCreate(Sender: TObject);
begin
  // 准备工作路径
  FAppDir := ExtractFilePath(Application.ExeName);

  // 客户端数据存放路径（2.0改名称）
  iocp_Varis.gUserDataPath := FAppDir + 'client_data\';

  MyCreateDir(FAppDir + 'log');    // 建目录
  MyCreateDir(FAppDir + 'temp');   // 建目录
  MyCreateDir(iocp_Varis.gUserDataPath);  // 建目录
end;

procedure TFormInIOCPDBServer.InClientManager1Login(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  if (Params.Password <> '') then
  begin
    Result.Role := crAdmin;   // 返回 crAdmin 权限，能广播
    Result.ActResult := arOK;
    // 登记属性、根据用户定制工作路径
    InClientManager1.Add(Params.Socket, crAdmin);
  end else
    Result.ActResult := arFail;
end;

procedure TFormInIOCPDBServer.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

procedure TFormInIOCPDBServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
  Memo1.Lines.Add('server ip: ' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('port: ' + IntToStr(InIOCPServer1.ServerPort));
end;

end.
