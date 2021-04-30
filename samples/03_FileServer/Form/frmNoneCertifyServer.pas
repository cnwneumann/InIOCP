unit frmNoneCertifyServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iocp_managers, iocp_server, StdCtrls, iocp_clients, iocp_sockets,
  iocp_clientBase;

type
  TFormNoneCertifyServer = class(TForm)
    InIOCPServer1: TInIOCPServer;
    InFileManager1: TInFileManager;
    Memo1: TMemo;
    btnStart: TButton;
    btnStop: TButton;
    Button3: TButton;
    Button4: TButton;
    btnConnect: TButton;
    InFileClient1: TInFileClient;
    InConnection1: TInConnection;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
    procedure InFileManager1BeforeUpload(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure Button3Click(Sender: TObject);
    procedure InFileManager1BeforeDownload(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure Button4Click(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure InConnection1AfterConnect(Sender: TObject);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormNoneCertifyServer: TFormNoneCertifyServer;

implementation

uses
  iocp_log, iocp_base, iocp_varis, iocp_utils;

{$R *.dfm}

procedure TFormNoneCertifyServer.btnConnectClick(Sender: TObject);
begin
  InConnection1.Active := not InConnection1.Active; 
end;

procedure TFormNoneCertifyServer.btnStartClick(Sender: TObject);
begin
  iocp_log.TLogThread.InitLog;     // 开启日志
  InIOCPServer1.Active := True;    // 开启服务
end;

procedure TFormNoneCertifyServer.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False; // 停止服务
  iocp_log.TLogThread.StopLog;   // 停止日志
end;

procedure TFormNoneCertifyServer.Button3Click(Sender: TObject);
begin
  // 下载 S_DownloadFile
  // 方法一，参数少
  InFileClient1.Upload('Upload_file.txt');

  // 方法二，可以加入更多参数，服务端方便操作
{  with TMessagePack.Create(InFileClient1) do
  begin
    AsString['path'] := 'none_certify';
    LoadFromFile('Upload_file.txt');  // 不能用 FileName :=
    Post(atFileUpload);
  end;  }
end;

procedure TFormNoneCertifyServer.Button4Click(Sender: TObject);
begin
  // 下载 S_DownloadFile
  // 方法一，参数少，保存到路径 InConnection1.LocalPath
  InFileClient1.Download('S_DownloadFile.7z');

  // 方法二，可以加入更多参数，服务端方便操作
{  with TMessagePack.Create(InFileClient1) do
  begin
    AsString['path'] := 'none_certify';  // 服务端路径
    LocalPath := 'temp\';  // 本地存放路径
    FileName := 'S_DownloadFile.7z';
    Post(atFileDownload);
  end; }
end;

procedure TFormNoneCertifyServer.FormCreate(Sender: TObject);
begin
  // 准备工作路径
  FAppDir := ExtractFilePath(Application.ExeName);

  // 客户端数据存放路径
  iocp_Varis.gUserDataPath := FAppDir + 'none_certify\';

  MyCreateDir(FAppDir + 'log');    // 建目录
  MyCreateDir(iocp_Varis.gUserDataPath);   // 建目录

end;

procedure TFormNoneCertifyServer.InConnection1AfterConnect(Sender: TObject);
begin
  if InConnection1.Active then
    btnConnect.Caption := '断开'
  else
    btnConnect.Caption := '连接';    
end;

procedure TFormNoneCertifyServer.InFileManager1BeforeDownload(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 打开文件流供下载
  if (Params.AsString['path'] = '') then  // 没有路径
    InFileManager1.OpenLocalFile(Result, 'none_certify\' + Params.FileName)
  else
    InFileManager1.OpenLocalFile(Result, Params.AsString['path'] + '\' + Params.FileName);
end;

procedure TFormNoneCertifyServer.InFileManager1BeforeUpload(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 建文件流供上传，如果登录可以用 InFileManager1.CreateNewFile()
  // 因没有登录，用以下方法：
  if (Params.AsString['path'] = '') then
    Params.CreateAttachment('none_certify\')   
  else
    Params.CreateAttachment(Params.AsString['path'] + '\');  // 客户端指定的路径  
end;

procedure TFormNoneCertifyServer.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

end.
