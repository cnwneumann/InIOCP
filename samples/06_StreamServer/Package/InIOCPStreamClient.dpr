program InIOCPStreamClient;

uses
  Forms,
  frmInIOCPStreamClient in '..\Form\frmInIOCPStreamClient.pas' {FormInIOCPStreamClient};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPStreamClient, FormInIOCPStreamClient);
  Application.Run;
end.
