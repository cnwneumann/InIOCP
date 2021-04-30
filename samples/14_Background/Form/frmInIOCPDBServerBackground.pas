unit frmInIOCPDBServerBackground;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_base, iocp_clients, iocp_server,
  iocp_sockets, iocp_managers, iocp_msgPacks, http_objects;

type
  TFormInIOCPDBServerBGThread = class(TForm)
    Memo1: TMemo;
    InIOCPServer1: TInIOCPServer;
    btnStart: TButton;
    btnStop: TButton;
    InClientManager1: TInClientManager;
    InDatabaseManager1: TInDatabaseManager;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    InMessageManager1: TInMessageManager;
    InFileManager1: TInFileManager;
    InHttpDataProvider1: TInHttpDataProvider;
    InWebSocketManager1: TInWebSocketManager;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure InClientManager1Login(Sender: TObject; Params: TReceiveParams;
      Result: TReturnResult);
    procedure FormCreate(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
    procedure InIOCPServer1AfterClose(Sender: TObject);
    procedure InMessageManager1ListFiles(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InFileManager1BeforeDownload(Sender: TObject;
      Params: TReceiveParams; Result: TReturnResult);
    procedure InWebSocketManager1Receive(Sender: TObject; Socket: TWebSocket);
  private
    { Private declarations }
    FAppDir: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPDBServerBGThread: TFormInIOCPDBServerBGThread;

implementation

uses
  iocp_log, iocp_varis, iocp_utils, dm_iniocp_test;

{$R *.dfm}

procedure TFormInIOCPDBServerBGThread.btnStartClick(Sender: TObject);
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

procedure TFormInIOCPDBServerBGThread.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False;   // 停止服务
  FrameIOCPSvrInfo1.Stop;          // 停止统计
  iocp_log.TLogThread.StopLog;   // 停止日志
end;

procedure TFormInIOCPDBServerBGThread.FormCreate(Sender: TObject);
begin
  // 准备工作路径
  FAppDir := ExtractFilePath(Application.ExeName);

  // 客户端数据存放路径（2.0改名称）
  iocp_Varis.gUserDataPath := FAppDir + 'data\';

  MyCreateDir(FAppDir + 'log');    // 建目录
  MyCreateDir(FAppDir + 'temp');   // 建目录
  MyCreateDir(iocp_Varis.gUserDataPath);  // 建目录
end;

procedure TFormInIOCPDBServerBGThread.InClientManager1Login(Sender: TObject;
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

procedure TFormInIOCPDBServerBGThread.InFileManager1BeforeDownload(
  Sender: TObject; Params: TReceiveParams; Result: TReturnResult);
begin
  // 客户端下载查询结果
  InFileManager1.OpenLocalFile(Result, 'data\' + Params.FileName);  // 严格来说应打开工作路径的文件
  Result.ActResult := arOK;
end;

procedure TFormInIOCPDBServerBGThread.InIOCPServer1AfterClose(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

procedure TFormInIOCPDBServerBGThread.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
  Memo1.Lines.Add('server ip: ' + InIOCPServer1.ServerAddr);
  Memo1.Lines.Add('port: ' + IntToStr(InIOCPServer1.ServerPort));
end;

procedure TFormInIOCPDBServerBGThread.InMessageManager1ListFiles(
  Sender: TObject; Params: TReceiveParams; Result: TReturnResult);
begin
  // 请参考 例-25 的方法 InFileManager1QueryFiles：
  
  // 1、默认情况 Params.Socket.Background = False, 后台执行时为 True；
  // 2、用任一管理器把任务投放到后台执行，如：InFileManager1.AddToBackground()；
  // 3、后台线程也执行本方法，但 Params.Socket.Background = True，结束时要 Wakeup 客户端；
  // 4、后台执行时不能直接反馈 Result，只能推送（用任一管理器），Result 不能太大。

  // TWebSocket 同样支持后台执行！
  // 新版本在 TInIOCPDataModule 中增加两个方法 AddToBackground、Wakeup，
  // 参考本方法在其子类中用实现后台执行。

end;

procedure TFormInIOCPDBServerBGThread.InWebSocketManager1Receive(
  Sender: TObject; Socket: TWebSocket);
begin
  // 见：TdmInIOCPTest.InIOCPDataModuleWebSocketQuery
  if Socket.MsgType = mtJSON then
    case Socket.JSON.Action of
      1:  // 注册
        Socket.UserName := Socket.JSON.S['_userName'];  // 必须有名称

      999:         // 数据查询操作
        if Socket.JSON.S['_dataFile'] = '' then
          TBusiWorker(Sender).DataModule.WebSocketQuery(Socket.JSON, Socket.Result)
        else begin   // 下载查询结果
          Socket.Result.S['_tableName'] := Socket.JSON.S['_tableName'];
          Socket.Result.S['_dataFile'] := Socket.JSON.S['_dataFile'];
          Socket.Result.Attachment := TFileStream.Create('temp\' + Socket.JSON.S['_dataFile'], fmShareDenyWrite);
          Socket.SendResult;  // 用默认字符集，非 UTF-8
        end;

      990: begin // 更新数据表
        TBusiWorker(Sender).DataModule.WebSocketUpdates(Socket.JSON, Socket.Result);
        Socket.SendResult;
      end;
    end;
end;

end.
