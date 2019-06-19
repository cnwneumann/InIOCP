object FormInIOCPDBServer: TFormInIOCPDBServer
  Left = 0
  Top = 0
  Caption = 'InIOCP '#25968#25454#24211#26381#21153
  ClientHeight = 419
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object Memo1: TMemo
    Left = 8
    Top = 225
    Width = 433
    Height = 176
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      '1'#12289#24341#29992' InIOCP '#32479#35745#21333#20803' frame\fmIOCPSvrInfo'
      '2'#12289#24341#29992' InIOCP '#25968#27169#21333#20803' module\iocp_baseModule'
      ''
      '3'#12289#21152#20837#32479#35745#21333#20803#26694#26550' FrameIOCPSvrInfo1'
      '4'#12289#20174' InIOCPDataModule '#31867#32487#25215#26032#24314#19968#20010#25968#27169#65292#36830#25509' SQLite3 '#25968#25454#24211
      ''
      '5'#12289#21152#20837#26381#21153#31471#32452#20214' InIOCPServer1'#65292#35774#32622'IP/Port'#65306'127.0.0.1/12302'
      '6'#12289#21152#20837#12289#35774#32622#29992#25143#31649#29702#32452#20214#65306'InClientManager1'
      '7'#12289#21152#20837#12289#35774#32622#25968#25454#24211#31649#29702#32452#20214' InDatabaseManager1'
      ''
      #36816#34892#23458#25143#31471#31243#24207#65292#27979#35797#26597#35810#12289#26356#26032#12290)
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object btnStart: TButton
    Left = 463
    Top = 244
    Width = 75
    Height = 28
    Caption = #21551#21160
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 463
    Top = 288
    Width = 75
    Height = 28
    Caption = #20572#27490
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  inline FrameIOCPSvrInfo1: TFrameIOCPSvrInfo
    Left = 4
    Top = 8
    Width = 547
    Height = 201
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    ExplicitLeft = 4
    ExplicitTop = 8
    ExplicitWidth = 547
  end
  object InIOCPServer1: TInIOCPServer
    IOCPManagers.ClientManager = InClientManager1
    IOCPManagers.DatabaseManager = InDatabaseManager1
    ServerAddr = '127.0.0.1'
    StartParams.TimeOut = 0
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterClose
    Left = 256
    Top = 232
  end
  object InClientManager1: TInClientManager
    OnLogin = InClientManager1Login
    Left = 296
    Top = 232
  end
  object InDatabaseManager1: TInDatabaseManager
    Left = 344
    Top = 232
  end
end
