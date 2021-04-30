object FormNoneCertifyServer: TFormNoneCertifyServer
  Left = 0
  Top = 0
  Caption = 'InIOCP '#26080#35748#35777#26381#21153
  ClientHeight = 194
  ClientWidth = 460
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
    Left = 9
    Top = 6
    Width = 347
    Height = 139
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      'InIOCP'#26381#21153#31471' C/S '#21327#35758#25903#25345#26080#35748#35777#26381#21153#65288#19981#20351#29992#35748#35777#31649#29702#32452#20214
      #65289
      #23458#25143#31471#26080#39035#30331#24405#20063#21487#20197#23454#29616#21508#31181#25805#20316#65292#35831#20999#35760#65292
      #27492#26102#26381#21153#31471#27809#26377#23458#25143#31471#30340#29992#25143#20449#24687#65288#22914'UserName'#65289#65292
      #20961#26159#28041#21450#21040#36825#20123#20449#24687#20351#29992#30340#22320#26041#37117#35201#20316#30456#24212#35843#25972#12290
      ''
      #36825#31181#24773#20917#24314#35758#23458#25143#31471#20351#29992' TMessagePack'#65292#26041#20415#21152#20837#21508#31181#21442#25968#12290)
    TabOrder = 0
  end
  object btnStart: TButton
    Left = 378
    Top = 18
    Width = 58
    Height = 21
    Caption = #21551#21160
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 378
    Top = 50
    Width = 58
    Height = 21
    Caption = #20572#27490
    Enabled = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  object Button3: TButton
    Left = 87
    Top = 160
    Width = 58
    Height = 21
    Caption = #19978#20256
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 149
    Top = 160
    Width = 58
    Height = 21
    Caption = #19979#36733
    TabOrder = 4
    OnClick = Button4Click
  end
  object btnConnect: TButton
    Left = 9
    Top = 160
    Width = 58
    Height = 21
    Caption = #36830#25509
    TabOrder = 5
    OnClick = btnConnectClick
  end
  object InIOCPServer1: TInIOCPServer
    IOCPManagers.FileManager = InFileManager1
    ServerAddr = '127.0.0.1'
    ThreadOptions.BusinessThreadCount = 4
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 2
    AfterOpen = InIOCPServer1AfterClose
    AfterClose = InIOCPServer1AfterClose
    Left = 24
    Top = 16
  end
  object InFileManager1: TInFileManager
    BeforeUpload = InFileManager1BeforeUpload
    BeforeDownload = InFileManager1BeforeDownload
    Left = 72
    Top = 16
  end
  object InFileClient1: TInFileClient
    Connection = InConnection1
    Left = 72
    Top = 104
  end
  object InConnection1: TInConnection
    LocalPath = 'temp'
    ServerAddr = '127.0.0.1'
    AfterConnect = InConnection1AfterConnect
    AfterDisconnect = InConnection1AfterConnect
    Left = 24
    Top = 104
  end
end
