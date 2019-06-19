program InIOCPWebQueryScores;

{ FastMM4 in '..\..\..\inIocp\memMgr\FastMM\FastMM4.pas',
  FastMM4Messages in '..\..\..\inIocp\memMgr\FastMM\FastMM4Messages.pas',
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
}

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPWebQueryScores in '..\Form\frmInIOCPWebQueryScores.pas' {FormInIOCPWebQueryScores},
  fmIOCPSvrInfo in '..\..\..\inIocp\source\frame\fmIOCPSvrInfo.pas' {FrameIOCPSvrInfo: TFrame},
  iocp_baseModule in '..\..\..\InIOCP\source\module\iocp_baseModule.pas' {InIOCPDataModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPWebQueryScores, FormInIOCPWebQueryScores);
  Application.Run;
end.
