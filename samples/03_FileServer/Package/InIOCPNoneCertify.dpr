program InIOCPNoneCertify;

uses
  Forms,
  frmNoneCertifyServer in '..\Form\frmNoneCertifyServer.pas' {FormNoneCertifyServer};

{$R *.res}
{$R uac.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormNoneCertifyServer, FormNoneCertifyServer);
  Application.Run;
end.
