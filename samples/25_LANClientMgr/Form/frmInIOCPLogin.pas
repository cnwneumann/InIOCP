unit frmInIOCPLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, iocp_clients;

type
  TFormInIOCPLogin = class(TForm)
    ledtHost: TLabeledEdit;
    ledtPort: TLabeledEdit;
    ledtUser: TLabeledEdit;
    ledtPassword: TLabeledEdit;
    btnLogin: TButton;
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FConnection: TInConnection;
    FCertifyClient: TInCertifyClient;
  end;

implementation

{$R *.dfm}

procedure TFormInIOCPLogin.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormInIOCPLogin.btnLoginClick(Sender: TObject);
begin
  FConnection.ServerAddr := ledtHost.Text;
  FConnection.ServerPort := StrToInt(ledtPort.Text);
  FCertifyClient.Group := 'Group_a';  // 分组
  FCertifyClient.UserName := ledtUser.Text;  // 服务端的 SQL 为 [USER_LOGIN]
  FCertifyClient.Password := ledtPassword.Text;
  FCertifyClient.Login;
end;

end.
