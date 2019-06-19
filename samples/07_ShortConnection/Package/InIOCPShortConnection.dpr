program InIOCPShortConnection;

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  windows,
  Forms,
  frmInIOCPShortConnection in '..\Form\frmInIOCPShortConnection.pas' {FormInIOCPShortConnection},
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame};

{$R *.res}
{$R uac.res}

begin
  Application.Initialize;
  if Windows.FindWindow(nil, 'InIOCP 短连接应用') > 0 then
    InstanceCount := 1;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPShortConnection, FormInIOCPShortConnection);
  Application.Run;
end.
