unit frmInIOCPDLLFormCaller;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormInIOCPDLL = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInIOCPDLL: TFormInIOCPDLL;

procedure ShowForm(AHandle: THandle); stdcall;
procedure CloseForm; stdcall;

implementation

{$R *.dfm}

procedure ShowForm(AHandle: THandle); external 'InIOCPDLLCallMe.dll' name 'ShowForm';
procedure CloseForm; external 'InIOCPDLLCallMe.dll' name 'CloseForm';

procedure TFormInIOCPDLL.Button1Click(Sender: TObject);
begin
  ShowForm(Application.Handle);
end;

procedure TFormInIOCPDLL.Button2Click(Sender: TObject);
begin
  CloseForm;
end;

end.
