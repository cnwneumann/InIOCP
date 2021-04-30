object FormInIOCPWsJSONMsgClient: TFormInIOCPWsJSONMsgClient
  Left = 249
  Top = 433
  Caption = 'InIOCP WebSocket '#26381#21153'-'#28040#24687#25512#36865#23458#25143#31471
  ClientHeight = 225
  ClientWidth = 657
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
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 657
    Height = 184
    Align = alClient
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #27599'100'#27627#31186#24191#25773#19968#26465#28040#24687#65292
      #21487#20197#21516#26102#25171#24320#20197#19979#32593#31449#27979#35797' WebSocket '#25512#36865#25928#26524#65306
      ''
      'http://www.websocketest.com/'
      'http://www.blue-zero.com/WebSocket/')
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitTop = 33
    ExplicitWidth = 595
    ExplicitHeight = 192
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 657
    Height = 41
    Align = alTop
    Color = clSkyBlue
    ParentBackground = False
    TabOrder = 1
    object Button1: TButton
      Left = 21
      Top = 7
      Width = 63
      Height = 27
      Caption = #25512#36865
      TabOrder = 0
      OnClick = Button1Click
    end
    object chkShowMsgs: TCheckBox
      Left = 272
      Top = 13
      Width = 233
      Height = 17
      Caption = #26174#31034#25910#21040#30340#28040#24687#65288#36816#34892#22810#27425#26412#23458#25143#31471#65289
      TabOrder = 1
    end
    object lbEditGroup: TLabeledEdit
      Left = 184
      Top = 10
      Width = 73
      Height = 20
      EditLabel.Width = 60
      EditLabel.Height = 12
      EditLabel.Caption = #29992#25143#20998#32452#65306
      ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
      LabelPosition = lpLeft
      TabOrder = 2
      Text = 'XXX'
    end
    object Button2: TButton
      Left = 541
      Top = 8
      Width = 84
      Height = 27
      Caption = #27979#35797
      TabOrder = 3
      OnClick = Button2Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 200
    OnTimer = Timer1Timer
    Left = 320
    Top = 96
  end
  object InWSConnection1: TInWSConnection
    AfterConnect = InWSConnection1AfterConnect
    URL = '/ws'
    OnReceiveData = InWSConnection1ReceiveData
    OnReceiveMsg = InWSConnection1ReceiveMsg
    OnReturnResult = InWSConnection1ReturnResult
    Left = 224
    Top = 96
  end
end
