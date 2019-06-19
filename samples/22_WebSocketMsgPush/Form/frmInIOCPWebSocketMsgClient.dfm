object FormInIOCPWsJSONMsgClient: TFormInIOCPWsJSONMsgClient
  Left = 249
  Top = 433
  Caption = 'InIOCP WebSocket '#26381#21153'-'#28040#24687#25512#36865#23458#25143#31471
  ClientHeight = 184
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Memo1: TMemo
    Left = 0
    Top = 33
    Width = 518
    Height = 151
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
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 518
    Height = 33
    Align = alTop
    Color = clSkyBlue
    ParentBackground = False
    TabOrder = 1
    object Button1: TButton
      Left = 21
      Top = 5
      Width = 60
      Height = 23
      Caption = #25512#36865
      TabOrder = 0
      OnClick = Button1Click
    end
    object chkShowMsgs: TCheckBox
      Left = 328
      Top = 10
      Width = 129
      Height = 17
      Caption = #26174#31034#25910#21040#30340#28040#24687
      TabOrder = 1
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 312
    Top = 72
  end
  object InWSConnection1: TInWSConnection
    ServerPort = 80
    AfterConnect = InWSConnection1AfterConnect
    OnReceiveData = InWSConnection1ReceiveData
    OnReceiveMsg = InWSConnection1ReceiveMsg
    OnReturnResult = InWSConnection1ReturnResult
    Left = 408
    Top = 72
  end
end
