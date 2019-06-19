unit frmInIOCPFileServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_base, iocp_clients, iocp_server,
  iocp_sockets, iocp_managers, iocp_msgPacks;

type
  TFormInIOCPFileServer = class(TForm)
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    btnStart: TButton;
    btnStop: TButton;
    InClientManager1: TInClientManager;
    InFileManager1: TInFileManager;
    InMessageManager1: TInMessageManager;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure InClientManager1Login(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure FormCreate(Sender: TObject);
    procedure InFileManager1BeforeDownload(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InFileManager1BeforeUpload(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InFileManager1QueryFiles(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InFileManager1AfterDownload(Sender: TObject;
      Params: TReceiveParams; Document: TIOCPDocument);
    procedure InFileManager1AfterUpload(Sender: TObject; Params: TReceiveParams;
      Document: TIOCPDocument);
    procedure InFileManager1DeleteFile(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InFileManager1RenameFile(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InFileManager1SetWorkDir(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPFileServer: TFormInIOCPFileServer;

implementation

uses
  iocp_log, iocp_varis, iocp_utils;

{$R *.dfm}

procedure TFormInIOCPFileServer.btnStartClick(Sender: TObject);
begin
  // 注：InFileManager1.ShareStream = True 可以共享下载文件流
  Memo1.Lines.Clear;
  iocp_log.TLogThread.InitLog;              // 开启日志
  InIOCPServer1.Active := True;               // 开启服务
  FrameIOCPSvrInfo1.Start(InIOCPServer1);     // 开始统计
end;

procedure TFormInIOCPFileServer.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False;   // 停止服务
  FrameIOCPSvrInfo1.Stop;          // 停止统计
  iocp_log.TLogThread.StopLog;   // 停止日志
end;

procedure TFormInIOCPFileServer.FormCreate(Sender: TObject);
begin
  // 准备工作路径
  FAppDir := ExtractFilePath(Application.ExeName);
  iocp_utils.IniDateTimeFormat;    // 设置日期时间格式
    
  // 客户端数据存放路径（2.0改名称）
  iocp_Varis.gUserDataPath := FAppDir + 'client_data\';

  MyCreateDir(FAppDir + 'log');    // 建目录
  MyCreateDir(FAppDir + 'temp');   // 建目录

  // 建测试的用户路径
  MyCreateDir(iocp_Varis.gUserDataPath);  // 建目录

  MyCreateDir(iocp_Varis.gUserDataPath + 'user_a');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_a\data');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_a\msg');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_a\temp');

  MyCreateDir(iocp_Varis.gUserDataPath + 'user_b');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_b\data');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_b\msg');
  MyCreateDir(iocp_Varis.gUserDataPath + 'user_b\temp');

end;

procedure TFormInIOCPFileServer.InClientManager1Login(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  if (Params.Password <> '') then 
  begin
    Result.Role := crAdmin;     // 测试广播要用的权限（2.0改）
    Result.ActResult := arOK;

    // 登记属性, 自动设置用户数据路径（注册时建）
    InClientManager1.Add(Params.Socket, crAdmin);

    // 有离线消息时要加入（如文件互传）
  end else
    Result.ActResult := arFail;
end;

procedure TFormInIOCPFileServer.InFileManager1AfterDownload(Sender: TObject;
  Params: TReceiveParams; Document: TIOCPDocument);
begin
  memo1.Lines.Add('下载完毕：' + ExtractFileName(Document.FileName));
end;

procedure TFormInIOCPFileServer.InFileManager1AfterUpload(Sender: TObject;
  Params: TReceiveParams; Document: TIOCPDocument);
begin
  // 上传文件完毕
  //   Sender: TBusiWorker
  //   Socket：TIOCPSocket
  // Document：TIOCPDocument

  //   有两种上传方式：atFileUpload、atFileSendTo
  //   如果 Document.UserName 不为空，
  //  则说明是互传文件，要通知对方下载或保存信息给对方登录时提取。

  memo1.Lines.Add('上传完毕：' + ExtractFileName(Document.FileName));

  // 新版调整推送方法，ToUser 可以是列表，直接调用即可：
  if (Params.ToUser <> '') then  // 互传的文件，通知 ToUser 下载
    InMessageManager1.PushMsg(Params, Params.ToUser);  // 唤醒或保存到离线文件

end;

procedure TFormInIOCPFileServer.InFileManager1BeforeDownload(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 下载文件（要使用相对路径）
  if (Params.Action = atFileDownChunk) then
  begin
    // 测试下载大文件
    memo1.Lines.Add('准备下载（续传）：' + 'F:\Backup\jdk-8u77-windows-i586.exe');  // F:\Backup\Ghost\WIN-7-20190228.GHO
    InFileManager1.OpenLocalFile(Result, 'F:\Backup\jdk-8u77-windows-i586.exe'); // F:\Backup\Ghost\WIN-7-20190228.GHO
  end else
  begin
    memo1.Lines.Add('准备下载：' + 'F:\Backup\jdk-8u77-windows-i586.exe'); // gUserDataPath + Params.FileName);
    InFileManager1.OpenLocalFile(Result, 'F:\Backup\jdk-8u77-windows-i586.exe');
  end;
end;

procedure TFormInIOCPFileServer.InFileManager1BeforeUpload(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 上传文件（到用户数据路径）
  //   2.0 在内部自动判断文件是否存在，存在则换一个文件名
  Memo1.Lines.Add('准备上传: ' + Params.FileName);

  // 如果免登录，可以使用这种方法接收：
  // Params.CreateAttachment('存放路径');
 
  InFileManager1.CreateNewFile(Params);
end;

procedure TFormInIOCPFileServer.InFileManager1DeleteFile(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 请求删除文件，应在客户端先确认
  if DeleteFile(Params.Socket.Envir^.WorkDir + Params.FileName) then
    Result.ActResult := arOK
  else
    Result.ActResult := arFail;
end;

procedure TFormInIOCPFileServer.InFileManager1QueryFiles(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 查询当前工作目录下的文件
  InFileManager1.ListFiles(Params, Result);
end;

procedure TFormInIOCPFileServer.InFileManager1RenameFile(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 改工作目录下的文件名
  if RenameFile(Params.Socket.Envir^.WorkDir + Params.FileName,
                Params.Socket.Envir^.WorkDir + Params.NewFileName) then
    Result.ActResult := arOK
  else
    Result.ActResult := arFail;
end;

procedure TFormInIOCPFileServer.InFileManager1SetWorkDir(Sender: TObject;
  Params: TReceiveParams; Result: TReturnResult);
begin
  // 设置工作目录（不能超出允许的工作目录范围）
//  if True then
    InFileManager1.SetWorkDir(Result, Params.Directory);  // 2.0 改名
//  else
//    Result.ActResult := arFail;
end;

procedure TFormInIOCPFileServer.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

procedure TFormInIOCPFileServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
  memo1.Lines.Add('IP:' + InIOCPServer1.ServerAddr);
  memo1.Lines.Add('Port:' + IntToStr(InIOCPServer1.ServerPort));
end;

end.
