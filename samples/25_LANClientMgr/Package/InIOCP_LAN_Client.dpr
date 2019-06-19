program InIOCP_LAN_Client;

// ScaleMM2 in '..\ThirdCodes\scalemm2\ScaleMM2.pas',

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  Forms,
  frmInIOCPClient in '..\Form\frmInIOCPClient.pas' {FormInIOCPClient};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPClient, FormInIOCPClient);
  Application.Run;
end.
