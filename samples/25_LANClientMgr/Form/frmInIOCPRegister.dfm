object FormInIOCPRegister: TFormInIOCPRegister
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'InIOCP-'#27880#20876
  ClientHeight = 161
  ClientWidth = 210
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 161
    Top = 17
    Width = 36
    Height = 12
    Caption = '('#21807#19968')'
  end
  object Label2: TLabel
    Left = 161
    Top = 42
    Width = 36
    Height = 12
    Caption = '('#21807#19968')'
  end
  object btnRegister: TButton
    Left = 30
    Top = 126
    Width = 68
    Height = 23
    Caption = #27880#20876
    TabOrder = 4
    OnClick = btnRegisterClick
  end
  object ledtUserCode: TLabeledEdit
    Left = 63
    Top = 13
    Width = 93
    Height = 20
    EditLabel.Width = 48
    EditLabel.Height = 12
    EditLabel.Caption = #32534#12288#30721#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '123456'
  end
  object ledtAddUser: TLabeledEdit
    Left = 63
    Top = 39
    Width = 93
    Height = 20
    EditLabel.Width = 48
    EditLabel.Height = 12
    EditLabel.Caption = #29992#25143#21517#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = 'ADDNEW'
  end
  object ledtPassword: TLabeledEdit
    Left = 63
    Top = 66
    Width = 93
    Height = 20
    EditLabel.Width = 48
    EditLabel.Height = 12
    EditLabel.Caption = #23494#12288#30721#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '111'
  end
  object ledtRole: TLabeledEdit
    Left = 63
    Top = 94
    Width = 93
    Height = 20
    EditLabel.Width = 48
    EditLabel.Height = 12
    EditLabel.Caption = #26435#12288#38480#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 3
    Text = '1'
  end
  object btnClose: TButton
    Left = 113
    Top = 126
    Width = 68
    Height = 23
    Caption = #20851#38381
    TabOrder = 5
    OnClick = btnCloseClick
  end
end
