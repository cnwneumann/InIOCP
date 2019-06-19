program InIOCPNewFeatures;

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPNewFeatures in '..\Form\frmInIOCPNewFeatures.pas' {FormInIOCPNewFeatures};

{$R *.res}

begin
  GetApplicationHandle;
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPNewFeatures, FormInIOCPNewFeatures);
  Application.Run;
end.
