program InIOCPHTTPService;

{%File '..\..\09_HttpServer\Package\InIOCPHttpServer.bdsproj'}

uses
  ScaleMM2 in '..\..\..\inIocp\memMgr\scalemm2\ScaleMM2.pas',
  SvcMgr,
  svInIOCPService in '..\Form\svInIOCPService.pas' {InIOCP_HTTP_Service: TService};

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TInIOCP_HTTP_Service, InIOCP_HTTP_Service);
  Application.Run;
end.
