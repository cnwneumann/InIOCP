unit frmInIOCPPushMessage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, iocp_clients, ExtCtrls;
                                   
type
  TForm3 = class(TForm)
    Memo1: TMemo;
    InConnection: TInConnection;
    InCertifyClient1: TInCertifyClient;
    lbl1: TLabel;
    Label4: TLabel;
    edtPort: TEdit;
    edtAddress: TEdit;
    Button1: TButton;
    Timer1: TTimer;
    lbEditGroup: TLabeledEdit;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure InConnectionError(Sender: TObject; const Msg: string);
    procedure InConnectionReceiveMsg(Sender: TObject; Msg: TResultParams);
    procedure FormCreate(Sender: TObject);
    procedure InConnectionAfterConnect(Sender: TObject);
    procedure InConnectionReturnResult(Sender: TObject; Result: TResultParams);
  private
    { Private declarations }
    FCount: Integer;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  iocp_base;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  if not InConnection.Active then
  begin
    Button1.Caption := '停止';
    InConnection.ServerAddr := edtAddress.Text;
    InConnection.ServerPort := StrToInt(edtPort.Text);
    InConnection.Active := True;

    // 新版改变：
    // 服务端无认证服务管理时，客户端免登录，连接后即可立刻发送请求。
    InCertifyClient1.Group := lbEditGroup.Text;  // 分组（群）
    InCertifyClient1.UserName := 'USER_A' + IntToStr(GetTickCount);
    InCertifyClient1.Password := 'AAAA';
    InCertifyClient1.Login; 

    Timer1.Enabled := True;
    Memo1.Lines.Add('Start');
  end else
  begin
    Timer1.Enabled := False;
    InConnection.Active := False;
    Button1.Caption := '广播';    
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  edtAddress.Text := '127.0.0.1';  // '192.168.1.196'; //
  edtPort.Text := '12302';  // '80';    
end;

procedure TForm3.InConnectionAfterConnect(Sender: TObject);
begin
{  InCertifyClient1.UserName := 'USER_A' + IntToStr(GetTickCount);
  InCertifyClient1.Password := 'AAAA';
  InCertifyClient1.Login;
  Timer1.Enabled := True;    }
end;

procedure TForm3.InConnectionError(Sender: TObject; const Msg: string);
begin
  Memo1.Lines.Add(Msg);
end;

procedure TForm3.InConnectionReceiveMsg(Sender: TObject; Msg: TResultParams);
begin
  // 在这里处理收到的推送消息（被动接收消息），大并发时不要显示
  Memo1.Lines.Add(Msg.Msg);
end;

procedure TForm3.InConnectionReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  // Memo1.Lines.Add(Result.Msg);
end;

procedure TForm3.Timer1Timer(Sender: TObject);
const
  TEXT_MSG = 'AFSFSFLSLFSLFLLLSSLDKDFKDFDSLFKDSKFSLFDKSFLKSLFSLFLSFDSFSLFS';
var
  Msg: TMessagePack;
begin
  if not InConnection.Active then
    Button1Click(nil);

  Inc(FCount);
  if (FCount > Length(TEXT_MSG)) then
    FCount := 1;
    
  // 用新特性直接发送消息
  Msg := TMessagePack.Create(InConnection);
  Msg.Msg := '推送消息 AAAAAAAAAAA' + Copy(Text_msg, 1, FCount);

  // 加校验码（其实对推送没必要）
  // 如果客户端的校验码折断处理不当，推送量大时有可能出现异常
{  Msg.CheckType := ctMurmurHash;
  Msg.CheckType := ctMD5;  }

  Msg.Post(atTextBroadcast);

  // 发送消息
{  Msg := TMessagePack.Create(InConnection);
  Msg.Msg := '推送消息 BBBBBBBBBBBBBB' + Copy(Text_msg, 1, FCount);
  Msg.Post(atTextBroadcast); }

//  Memo1.Lines.Add('Push' + IntToStr(FCount));
end;

end.
