program InIOCPDLLForm;

uses
  Forms,
  frmInIOCPDLLFormCaller in '..\Form\frmInIOCPDLLFormCaller.pas' {FormInIOCPDLL},
  iocp_Winsock2 in '..\..\..\InIOCP\source\iocp_Winsock2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInIOCPDLL, FormInIOCPDLL);
  Application.Run;
end.
