unit fmIOCPSvrInfo;

interface

{$I in_iocp.inc}        // 模式设置

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  Grids, iocp_server;

type
  TFrameIOCPSvrInfo = class(TFrame)
    lbl1: TLabel;
    lblStartTime: TLabel;
    lblCliPool: TLabel;
    lblClientInfo: TLabel;
    lbl3: TLabel;
    lblIODataInfo: TLabel;
    lbl6: TLabel;
    lblThreadInfo: TLabel;
    lblLeftEdge: TLabel;
    lblDataPackInf: TLabel;
    lbl14: TLabel;
    lblDBConCount: TLabel;
    lbl16: TLabel;
    lblDataByteInfo: TLabel;
    lblMemeryUsed: TLabel;
    lblMemUsed: TLabel;
    lbl19: TLabel;
    lblWorkTimeLength: TLabel;
    bvl1: TBevel;
    lbl12: TLabel;
    lblCheckTime: TLabel;
    lblAcceptExCount: TLabel;
    lblAcceptExCnt: TLabel;
    Label3: TLabel;
    lblWorkCount: TLabel;
  private
    { Private declarations }
    FServer: TInIOCPServer;    // 服务器
    FTimer: TTimer;            // 计算器
    FRefreshCount: Cardinal;   // 刷新次数
    FShowing: Boolean;         // 是否在前端显示
    procedure GetServerInfo(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start(AServer: TInIOCPServer);
    procedure Stop;
    property Showing: Boolean read FShowing write FShowing;
  end;

implementation

uses
  iocp_base, iocp_utils;

var
  FMaxInfo: TWorkThreadMaxInf; // 最大处理速度

{$R *.dfm}

{ TFrameIOCPSvrInfo }

constructor TFrameIOCPSvrInfo.Create(AOwner: TComponent);
begin
  inherited;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 1000;     // 间隔一秒
  FTimer.OnTimer := GetServerInfo;

  FShowing := True;
  lblStartTime.Caption := '未启动';
end;

destructor TFrameIOCPSvrInfo.Destroy;
begin
  inherited;
end;

procedure TFrameIOCPSvrInfo.GetServerInfo(Sender: TObject);
var
  ActiveCount: Integer;
  CountA, CountB: Integer;
  CountC, CountD, CountE: Integer;
  WorkTotalCount: IOCP_LARGE_INTEGER;
  CheckTimeOut: TDateTime;
  ThreadSummary: TWorkThreadSummary;
begin
   // 服务停止或不在前端，无需调用
  Inc(FRefreshCount, 1);  // 增加 1 秒
  if (FShowing = False) or (FServer.Active = False) then
    Exit;

  FTimer.Enabled := False;
  try

    // 改为蓝色
    if (Tag = 0) then  
    begin
      Tag := 1;
      lblWorkTimeLength.Font.Color := clBlue;
      lblClientInfo.Font.Color := clBlue;
      lblWorkCount.Font.Color := clBlue;
      lblIODataInfo.Font.Color := clBlue;
      lblThreadInfo.Font.Color := clBlue;
      lblDataPackInf.Font.Color := clBlue;
      lblDataByteInfo.Font.Color := clBlue;
      lblCheckTime.Font.Color := clBlue;
      lblAcceptExCnt.Font.Color := clBlue;
      lblMemUsed.Font.Color := clBlue;
      lblDBConCount.Font.Color := clBlue;
    end;

    lblWorkTimeLength.Caption := GetTimeLengthEx(FRefreshCount);

    // 1. C/S、HTTP 模式：Socket 总数、连接数，活动数=在业务线程数
    FServer.GetClientInfo(CountA, CountB, CountC,
                          CountD, ActiveCount, WorkTotalCount);

    if Assigned(FServer.IOCPBroker) then  // 代理模式无Http、推送功能
      lblClientInfo.Caption := '总计:' + IntToStr(CountA + CountC) +
                               ',C/S:' + IntToStr(CountB) { 连接的 } +
                               ',活动:' + IntToStr(ActiveCount) + '.' { 在列的业务+推送数 }
    else
      lblClientInfo.Caption := '总计:' + IntToStr(CountA + CountC) +
                               ',C/S:' + IntToStr(CountB) { 连接的 } +
                               ',HTTP:' + IntToStr(CountD) { 连接的 } +
                               ',活动:' + IntToStr(ActiveCount) + '.' { 在列的业务+推送数 } ;

    lblWorkCount.Caption := IntToStr(WorkTotalCount);

    // 2. TPerIOData 池：总数、C/S 使用、HTTP使用、推送列表使用
    //    Socket 的 RecvBuf 不回收，推送的 TPerIOData 回收，所以：
    //    CountA >= CountB + CountC + FServer.BusiWorkMgr.ThreadCount

    FServer.GetIODataInfo(CountA, CountB, CountC, CountD, CountE);

    if (Assigned(FServer.PushManager) = False) then  // 无 Http、推送功能
      lblIODataInfo.Caption := '总计:' + IntToStr(CountA) +
                               ',C/S:' + IntToStr(CountB) { 活动的 } +
                               ',发送器:' + IntToStr(CountE)
    else
      lblIODataInfo.Caption := '总计:' + IntToStr(CountA) +
                               ',C/S:' + IntToStr(CountB) { 活动的 } +
                               ',HTTP:' + IntToStr(CountC) { 活动的 } +
                               ',发送器:' + IntToStr(CountE) +
                               ',推送:' + IntToStr(CountD) + '.';

    // 3. 线程使用：工作线程、超时检查、关闭套接字、
    //              业务线程、推送线程(+1)、活动数
    FServer.GetThreadInfo(@ThreadSummary, CountA, CountB, CountC, CountD, CheckTimeOut);

    if (Assigned(FServer.PushManager) = False) then  // 代理模式无推送功能
      lblThreadInfo.Caption := '总计:' + IntToStr(FServer.WorkThreadCount + CountA + CountC + 2) +
                               ',工作:' + IntToStr(ThreadSummary.ActiveCount) { 活动的 } + '/' +
                                          IntToStr(FServer.WorkThreadCount) +
                               ',业务:' + IntToStr(CountB) { 活动的 } + '/' + IntToStr(CountA) +
                               ',超时:1,关闭:1.'
    else
      lblThreadInfo.Caption := '总计:' + IntToStr(FServer.WorkThreadCount + CountA + CountC + 3) +
                               ',工作:' + IntToStr(ThreadSummary.ActiveCount) { 活动的 } + '/' +
                                          IntToStr(FServer.WorkThreadCount) +
                               ',业务:' + IntToStr(CountB) { 活动的 } + '/' + IntToStr(CountA) +
                               ',推送:' + IntToStr(CountD) { 活动的 } + '/' + IntToStr(CountC) + '+1' +
                               ',超时:1,关闭:1.';    

    // 4. 处理速度(包/秒）
    if (ThreadSummary.PackInCount > FMaxInfo.MaxPackIn) then
      FMaxInfo.MaxPackIn := ThreadSummary.PackInCount;
    if (ThreadSummary.PackOutCount > FMaxInfo.MaxPackOut) then
      FMaxInfo.MaxPackOut := ThreadSummary.PackOutCount;

    lblDataPackInf.Caption := '总计:' + IntToStr(ThreadSummary.PackCount) + { '/' +
                                        IntToStr(FMaxInfo.MaxPackIn + FMaxInfo.MaxPackOut) + }
                              ',接收:' + IntToStr(ThreadSummary.PackInCount) + '/' +
                                        IntToStr(FMaxInfo.MaxPackIn) +
                              ',发送:' + IntToStr(ThreadSummary.PackOutCount) + '/' +
                                        IntToStr(FMaxInfo.MaxPackOut);


    // 5. 处理速度(字节/秒）
    if (ThreadSummary.ByteInCount > FMaxInfo.MaxByteIn) then
      FMaxInfo.MaxByteIn := ThreadSummary.ByteInCount;
    if (ThreadSummary.ByteOutCount > FMaxInfo.MaxByteOut) then
      FMaxInfo.MaxByteOut := ThreadSummary.ByteOutCount;

    lblDataByteInfo.Caption := '总计:' + GetTransmitSpeed(ThreadSummary.ByteCount {,
                                                          FMaxInfo.MaxByteIn + FMaxInfo.MaxByteOut } ) +
                               ',接收:' + GetTransmitSpeed(ThreadSummary.ByteInCount, FMaxInfo.MaxByteIn) +
                               ',发送:' + GetTransmitSpeed(ThreadSummary.ByteOutCount, FMaxInfo.MaxByteOut);

    // 6. 超时检查时间
    if (CheckTimeOut > 0.1) then
      lblCheckTime.Caption := TimeToStr(CheckTimeOut);

    // 7. Socket 投放数, 内存使用情况
    FServer.GetAcceptExCount(ActiveCount);

    lblAcceptExCnt.Caption := IntToStr(ActiveCount);
    lblMemUsed.Caption := GetTransmitSpeed(GetProcMemoryUsed);

    // 7.1 数模实例数
    if (lblDBConCount.Caption = '-') and
      Assigned(TInIOCPServer(FServer).DatabaseManager) then
      lblDBConCount.Caption := IntToStr(CountA) + '*' +  // CountA 未改变
                               IntToStr(TInIOCPServer(FServer).DatabaseManager.DataModuleCount);

  finally
    // 解锁屏幕，刷新
    FTimer.Enabled := True;
  end;
end;

procedure TFrameIOCPSvrInfo.Start(AServer: TInIOCPServer);
begin
  FServer := AServer;
  FillChar(FMaxInfo, SizeOf(TWorkThreadMaxInf), 0);
  if not FServer.Active then
    FServer.Active := True;
  if FServer.Active then
  begin
    FRefreshCount := 0;
    lblStartTime.Font.Color := clBlue;
    lblStartTime.Caption := FormatDateTime('yyyy-mm-dd hh:mm:ss', Now);
    lblCheckTime.Caption := '';
    FTimer.Enabled := True;
  end;
end;

procedure TFrameIOCPSvrInfo.Stop;
begin
  if Assigned(FServer) then
  begin
    if FTimer.Enabled then
      FTimer.Enabled := False;
    if FServer.Active then
      FServer.Active := False;
    lblStartTime.Font.Color := clRed;
    lblStartTime.Caption := '服务停止';
  end;
end;

end.
