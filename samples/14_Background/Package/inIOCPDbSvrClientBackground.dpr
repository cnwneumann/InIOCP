program inIOCPDbSvrClientBackground;

uses
  ScaleMM2 in '..\..\..\InIOCP\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPDbSvrClientBackground in '..\Form\frmInIOCPDbSvrClientBackground.pas' {FormInIOCPDbSvrClientBg};

{$R *.res}
{$R uac.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPDbSvrClientBg, FormInIOCPDbSvrClientBg);
  Application.Run;
end.
