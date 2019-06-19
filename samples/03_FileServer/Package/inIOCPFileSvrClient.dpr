program inIOCPFileSvrClient;

uses
  ScaleMM2 in '..\..\..\InIOCP\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPFileSvrClient in '..\Form\frmInIOCPFileSvrClient.pas' {FormInIOCPFileSvrClient};

{$R *.res}
{$R uac.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPFileSvrClient, FormInIOCPFileSvrClient);
  Application.Run;
end.
