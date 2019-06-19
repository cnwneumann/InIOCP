object FormInIOCPLogin: TFormInIOCPLogin
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'InIOCP-'#30331#24405
  ClientHeight = 135
  ClientWidth = 315
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 120
  TextHeight = 15
  object ledtHost: TLabeledEdit
    Left = 58
    Top = 15
    Width = 115
    Height = 23
    EditLabel.Width = 45
    EditLabel.Height = 15
    EditLabel.Caption = #20027#26426#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object ledtPort: TLabeledEdit
    Left = 239
    Top = 15
    Width = 52
    Height = 23
    EditLabel.Width = 45
    EditLabel.Height = 15
    EditLabel.Caption = #31471#21475#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '800'
  end
  object ledtUser: TLabeledEdit
    Left = 58
    Top = 68
    Width = 115
    Height = 23
    EditLabel.Width = 45
    EditLabel.Height = 15
    EditLabel.Caption = #29992#25143#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = 'admin'
  end
  object ledtPassword: TLabeledEdit
    Left = 58
    Top = 98
    Width = 115
    Height = 23
    EditLabel.Width = 45
    EditLabel.Height = 15
    EditLabel.Caption = #23494#30721#65306
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    LabelPosition = lpLeft
    TabOrder = 3
    Text = 'admin'
  end
  object btnLogin: TButton
    Left = 198
    Top = 58
    Width = 85
    Height = 28
    Caption = #30331#24405
    TabOrder = 4
    OnClick = btnLoginClick
  end
  object btnClose: TButton
    Left = 198
    Top = 96
    Width = 85
    Height = 28
    Caption = #20851#38381
    TabOrder = 5
    OnClick = btnCloseClick
  end
end
