object Form3: TForm3
  Left = 312
  Top = 260
  Caption = 'InIOCP'#25512#36865#28040#24687
  ClientHeight = 267
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 15
  object lbl1: TLabel
    Left = 204
    Top = 8
    Width = 45
    Height = 15
    Caption = #31471#21475#65306
  end
  object Label4: TLabel
    Left = 14
    Top = 8
    Width = 75
    Height = 15
    Caption = #26381#21153#22320#22336#65306
  end
  object Memo1: TMemo
    Left = 11
    Top = 61
    Width = 384
    Height = 198
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #27599' 50 '#27627#31186#24191#25773#19968#26465#21464#38271#28040#24687#65292
      #35831#36816#34892#28040#24687#26381#21153#31471' InIOCPMessageServer.exe'#65281)
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object edtPort: TEdit
    Left = 204
    Top = 29
    Width = 57
    Height = 23
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 1
    Text = '80'
  end
  object edtAddress: TEdit
    Left = 14
    Top = 29
    Width = 167
    Height = 23
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 2
    Text = '192.168.1.168'
  end
  object Button1: TButton
    Left = 293
    Top = 29
    Width = 78
    Height = 26
    Caption = #24191#25773
    TabOrder = 3
    OnClick = Button1Click
  end
  object InConnection: TInConnection
    OnReturnResult = InConnectionReturnResult
    LocalPath = 'temp\'
    ServerAddr = 'localhost'
    ServerPort = 80
    AfterConnect = InConnectionAfterConnect
    OnReceiveMsg = InConnectionReceiveMsg
    OnError = InConnectionError
    Left = 80
    Top = 80
  end
  object InCertifyClient1: TInCertifyClient
    Connection = InConnection
    Left = 152
    Top = 80
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 216
    Top = 80
  end
end
