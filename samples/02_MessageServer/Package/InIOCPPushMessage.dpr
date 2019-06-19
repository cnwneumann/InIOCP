program InIOCPPushMessage;

uses
  Forms,
  frmInIOCPPushMessage in '..\Form\frmInIOCPPushMessage.pas' {Form3},
  iocp_clients in '..\..\..\InIOCP\source\iocp_clients.pas',
  iocp_receivers in '..\..\..\InIOCP\source\iocp_receivers.pas',
  iocp_msgPacks in '..\..\..\InIOCP\source\iocp_msgPacks.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
