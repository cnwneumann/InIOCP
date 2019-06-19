program DoubleServer;

uses
  Forms,
  frmDoubleServer in '..\Form\frmDoubleServer.pas' {FormDblServer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDblServer, FormDblServer);
  Application.Run;
end.
