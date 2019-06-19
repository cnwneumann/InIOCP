unit frmDoubleServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, iocp_server, http_objects, iocp_managers;

type
  TFormDblServer = class(TForm)
    btn1: TButton;
    Button2: TButton;
    InIOCPServer1: TInIOCPServer;
    InIOCPServer2: TInIOCPServer;
    Memo1: TMemo;
    InHttpDataProvider1: TInHttpDataProvider;
    InHttpDataProvider2: TInHttpDataProvider;
    procedure InHttpDataProvider1Get(Sender: TObject; Request: THttpRequest;
      Respone: THttpRespone);
    procedure btn1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure InIOCPServer2AfterOpen(Sender: TObject);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
  private
    { Private declarations }
    FRefCount: Integer;
  public
    { Public declarations }
  end;

var
  FormDblServer: TFormDblServer;

implementation

uses
  iocp_log, http_utils;
  
{$R *.dfm}

procedure TFormDblServer.btn1Click(Sender: TObject);
begin
  if not InIOCPServer1.Active then
    iocp_log.TLogThread.InitLog;   // 开启日志
  InIOCPServer1.Active := not InIOCPServer1.Active;
end;

procedure TFormDblServer.Button2Click(Sender: TObject);
begin
  if not InIOCPServer2.Active then
    iocp_log.TLogThread.InitLog;   // 开启日志
  InIOCPServer2.Active := not InIOCPServer2.Active;
end;

procedure TFormDblServer.InHttpDataProvider1Get(Sender: TObject; Request: THttpRequest;
  Respone: THttpRespone);
begin
  Respone.SetContent('<p>Hello World.</p>');
  Respone.AddContent('<p>服务器时间：' + GetHttpGMTDateTime + '</p>');
end;

procedure TFormDblServer.InIOCPServer1AfterOpen(Sender: TObject);
begin
  if InIOCPServer1.Active then
  begin
    Inc(FRefCount);  // 日志次数 +
    btn1.Caption := '停止A';
  end else
  begin
    Dec(FRefCount);  // 日志次数 -
    btn1.Caption := '启动A';
  end;
  if (FRefCount = 0) then
    iocp_log.TLogThread.StopLog;  // 停止日志
end;

procedure TFormDblServer.InIOCPServer2AfterOpen(Sender: TObject);
begin
  if InIOCPServer2.Active then
  begin
    Inc(FRefCount);  // 日志次数 +
    Button2.Caption := '停止B'
  end else
  begin
    Dec(FRefCount);  // 日志次数 -
    Button2.Caption := '启动B';
  end;
  if (FRefCount = 0) then
    iocp_log.TLogThread.StopLog;  // 停止日志  
end;

end.
