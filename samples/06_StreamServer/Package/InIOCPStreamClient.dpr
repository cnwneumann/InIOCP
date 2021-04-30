program InIOCPStreamClient;

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPStreamClient in '..\Form\frmInIOCPStreamClient.pas' {FormInIOCPStreamClient};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPStreamClient, FormInIOCPStreamClient);
  Application.Run;
end.
