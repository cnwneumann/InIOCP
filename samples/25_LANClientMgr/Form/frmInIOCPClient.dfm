object FormInIOCPClient: TFormInIOCPClient
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'InIOCP-'#26222#36890#23458#25143#31471
  ClientHeight = 228
  ClientWidth = 525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnInOut: TButton
    Left = 394
    Top = 128
    Width = 75
    Height = 25
    Caption = #30331#24405
    TabOrder = 0
    OnClick = btnInOutClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 361
    Height = 207
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #27880#24847#65306#28040#24687#21253#39044#35774#30340#23646#24615' userName '#26159#30331#24405#32773#21517#31216#65292#30331#24405#21518
      #34987#20445#23384#21040' InConnect.UserName'#65292#23427#21487#33021#34987#25913#21464#65292#21644#29992#25143#25968#25454
      #34920#30340' user_name '#19981#21516#65292#20351#29992#26102#19981#35201#25630#28151#12290)
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object btnLogin2: TButton
    Left = 394
    Top = 171
    Width = 75
    Height = 25
    Caption = #30331#24405'2'
    TabOrder = 2
    OnClick = btnLogin2Click
  end
  object lbEditIP: TLabeledEdit
    Left = 394
    Top = 29
    Width = 98
    Height = 21
    EditLabel.Width = 14
    EditLabel.Height = 13
    EditLabel.Caption = 'IP:'
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 3
    Text = '127.0.0.1'
  end
  object lbEditPort: TLabeledEdit
    Left = 394
    Top = 69
    Width = 49
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = 'Port:'
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 4
    Text = '800'
  end
  object InConnection1: TInConnection
    OnReturnResult = InConnection1ReturnResult
    AutoConnected = True
    ReuseSessionId = True
    ServerAddr = '127.0.0.1'
    ServerPort = 800
    OnReceiveMsg = InConnection1ReceiveMsg
    OnError = InConnection1Error
    Left = 40
    Top = 72
  end
  object InMessageClient1: TInMessageClient
    OnReturnResult = InMessageClient1ReturnResult
    Connection = InConnection1
    Left = 104
    Top = 120
  end
  object InCertifyClient1: TInCertifyClient
    OnReturnResult = InCertifyClient1ReturnResult
    Connection = InConnection1
    OnCertify = InCertifyClient1Certify
    Left = 40
    Top = 120
  end
  object InFileClient1: TInFileClient
    OnReturnResult = InFileClient1ReturnResult
    Connection = InConnection1
    Left = 176
    Top = 120
  end
end
