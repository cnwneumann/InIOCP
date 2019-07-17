unit frmInIOCPStreamClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, StdCtrls,
  ExtCtrls;

type
  TFormInIOCPStreamClient = class(TForm)
    LabeledEdit1: TLabeledEdit;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FIdTCPClient: TIdTCPClient;
  public
    { Public declarations }
  end;

var
  FormInIOCPStreamClient: TFormInIOCPStreamClient;

implementation

{$R *.dfm}

procedure TFormInIOCPStreamClient.Button1Click(Sender: TObject);
begin
  FIdTCPClient.Host := '127.0.0.1';
  FIdTCPClient.Port := 800;
  FIdTCPClient.Connect;
end;

procedure TFormInIOCPStreamClient.Button2Click(Sender: TObject);
begin
  // 开头为客户端名称
  FIdTCPClient.SendCmd(LabeledEdit1.Text + ':SEND STATE TO SERVER');
  if FIdTCPClient.LastCmdResult.ReplyExists then   // 显示服务端的反馈消息
    Memo1.Lines.AddStrings(FIdTCPClient.LastCmdResult.Text);
end;

procedure TFormInIOCPStreamClient.FormCreate(Sender: TObject);
begin       
  FIdTCPClient := TIdTCPClient.Create(Self);
end;

procedure TFormInIOCPStreamClient.FormDestroy(Sender: TObject);
begin
  FIdTCPClient.Free;
end;

end.
