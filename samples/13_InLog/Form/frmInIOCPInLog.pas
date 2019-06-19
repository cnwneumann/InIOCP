unit frmInIOCPInLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormInIOCPInLog = class(TForm)
    btnTest: TButton;
    Label1: TLabel;
    Label2: TLabel;
    btnTest2: TButton;
    Memo1: TMemo;
    procedure btnTestClick(Sender: TObject);
    procedure btnTest2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInIOCPInLog: TFormInIOCPInLog;

implementation

uses
  iocp_log;

type

  TTestThread = class(TThread)
  protected
    FIndex: AnsiString;
    procedure Execute; override;
  public
    constructor Create; reintroduce;
  end;

var
  FActiveCount: Integer = 0;  // 开设线程数

{ TTestThread }

constructor TTestThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

procedure TTestThread.Execute;
var
  i: Integer;
begin
  for i := 1 to 100000 do   // 写10万次
    iocp_log.WriteLog(FIndex + '->测试日志线程，测试日志线程.');
  windows.InterlockedDecrement(FActiveCount);
end;

{$R *.dfm}

procedure TFormInIOCPInLog.btnTest2Click(Sender: TObject);
var
  i: Integer;
  TickCount: Cardinal;
begin
  // 开启日志，设置文件路径
  TLogThread.InitLog('log');

  FActiveCount := 10;
  TickCount := GetTickCount;

  // 开 10 个线程
  for i := 1 to FActiveCount do
    with TTestThread.Create do
    begin
      FIndex := IntToStr(i);
      Resume;
    end;

  // 等全部线程结束
  while FActiveCount > 0 do
    Sleep(10);

  // 完成
  Memo1.Lines.Add('10个线程写日志百万次...');
  Memo1.Lines.Add('耗时（毫米）: ' + IntToStr(GetTickCount - TickCount));
                    
  // 停止日志
  TLogThread.StopLog;

end;

procedure TFormInIOCPInLog.btnTestClick(Sender: TObject);
var
  i: Integer;
  TickCount: Cardinal;
begin
  // 开启日志，设置文件路径
  TLogThread.InitLog('log');

  TickCount := GetTickCount;

  for i := 1 to 1000000 do
    iocp_log.WriteLog('->测试日志线程，测试日志线程.');

  // 完成
  Memo1.Lines.Add('单线程循环写日志百万次...');
  Memo1.Lines.Add('耗时（毫米）: ' + IntToStr(GetTickCount - TickCount));

  // 停止日志
  TLogThread.StopLog;

end;

end.
