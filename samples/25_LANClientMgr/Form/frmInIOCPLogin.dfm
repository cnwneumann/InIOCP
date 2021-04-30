object FormInIOCPLogin: TFormInIOCPLogin
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'InIOCP-'#30331#24405
  ClientHeight = 108
  ClientWidth = 252
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
  object ledtHost: TLabeledEdit
    Left = 46
    Top = 12
    Width = 92
    Height = 20
    EditLabel.Width = 36
    EditLabel.Height = 12
    EditLabel.Caption = #20027#26426#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object ledtPort: TLabeledEdit
    Left = 191
    Top = 12
    Width = 42
    Height = 20
    EditLabel.Width = 36
    EditLabel.Height = 12
    EditLabel.Caption = #31471#21475#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '800'
  end
  object ledtUser: TLabeledEdit
    Left = 46
    Top = 54
    Width = 92
    Height = 20
    EditLabel.Width = 36
    EditLabel.Height = 12
    EditLabel.Caption = #29992#25143#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = 'admin'
  end
  object ledtPassword: TLabeledEdit
    Left = 46
    Top = 78
    Width = 92
    Height = 20
    EditLabel.Width = 36
    EditLabel.Height = 12
    EditLabel.Caption = #23494#30721#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 3
    Text = 'admin'
  end
  object btnLogin: TButton
    Left = 158
    Top = 46
    Width = 68
    Height = 23
    Caption = #30331#24405
    TabOrder = 4
    OnClick = btnLoginClick
  end
  object btnClose: TButton
    Left = 158
    Top = 77
    Width = 68
    Height = 22
    Caption = #20851#38381
    TabOrder = 5
    OnClick = btnCloseClick
  end
end
