program inIOCPDbSvrClient;

uses
  ScaleMM2 in '..\..\..\InIOCP\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPDbSvrClient in '..\Form\frmInIOCPDbSvrClient.pas' {FormInIOCPDbSvrClient};

{$R *.res}
{$R uac.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPDbSvrClient, FormInIOCPDbSvrClient);
  Application.Run;
end.
