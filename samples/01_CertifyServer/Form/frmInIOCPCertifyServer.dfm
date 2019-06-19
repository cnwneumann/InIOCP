object FormInIOCPCertifyServer: TFormInIOCPCertifyServer
  Left = 0
  Top = 0
  Caption = 'InIOCP '#35748#35777#26381#21153
  ClientHeight = 449
  ClientWidth = 767
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 14
  object Memo1: TMemo
    Left = 8
    Top = 223
    Width = 545
    Height = 198
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #26032#29256#25913#21464#65306
      'TInIOCPServer '#20851#32852#32452#20214' TInClientManager '#26102#65292#23458#25143#31471#24517#39035#30331#24405#65292
      #21542#21017#20813#30331#24405#65292#19981#36807#27492#26102#19981#33021#20351#29992#35748#35777#26381#21153#30340#21151#33021#12290)
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object btnStart: TButton
    Left = 574
    Top = 65
    Width = 75
    Height = 25
    Caption = #21551#21160
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 574
    Top = 96
    Width = 75
    Height = 25
    Caption = #20572#27490
    Enabled = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  object btnConnect: TButton
    Left = 574
    Top = 232
    Width = 75
    Height = 25
    Caption = #36830#25509
    TabOrder = 3
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 574
    Top = 261
    Width = 75
    Height = 25
    Caption = #26029#24320
    TabOrder = 4
    OnClick = btnDisconnectClick
  end
  object btnLogin: TButton
    Left = 574
    Top = 299
    Width = 75
    Height = 25
    Caption = #30331#24405
    TabOrder = 5
    OnClick = btnLoginClick
  end
  object btnLogout: TButton
    Left = 574
    Top = 329
    Width = 75
    Height = 25
    Caption = #30331#20986
    TabOrder = 6
    OnClick = btnLogoutClick
  end
  object btnConnect2: TButton
    Left = 674
    Top = 231
    Width = 75
    Height = 25
    Caption = #36830#25509'2'
    TabOrder = 7
    OnClick = btnConnect2Click
  end
  object btnDisconnect2: TButton
    Left = 674
    Top = 260
    Width = 75
    Height = 25
    Caption = #26029#24320'2'
    TabOrder = 8
    OnClick = btnDisconnect2Click
  end
  object btnLogin2: TButton
    Left = 674
    Top = 298
    Width = 75
    Height = 25
    Caption = #30331#24405'2'
    TabOrder = 9
    OnClick = btnLogin2Click
  end
  object btnLogout2: TButton
    Left = 674
    Top = 328
    Width = 75
    Height = 25
    Caption = #30331#20986'2'
    TabOrder = 10
    OnClick = btnLogout2Click
  end
  object btnQueryClient: TButton
    Left = 574
    Top = 373
    Width = 75
    Height = 25
    Caption = #26597#35810#29992#25143
    TabOrder = 11
    OnClick = btnQueryClientClick
  end
  object btnCheckState: TButton
    Left = 574
    Top = 405
    Width = 75
    Height = 25
    Caption = #26597#35810#29366#24577
    TabOrder = 12
    OnClick = btnCheckStateClick
  end
  inline FrameIOCPSvrInfo1: TFrameIOCPSvrInfo
    Left = 8
    Top = 16
    Width = 547
    Height = 201
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    ExplicitLeft = 8
    ExplicitTop = 16
    ExplicitWidth = 547
  end
  object InIOCPServer1: TInIOCPServer
    IOCPManagers.ClientManager = InClientManager1
    ServerAddr = '127.0.0.1'
    StartParams.TimeOut = 0
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterClose
    Left = 48
    Top = 304
  end
  object InConnection1: TInConnection
    OnReturnResult = InConnection1ReturnResult
    LocalPath = 'temp\'
    ServerAddr = '127.0.0.1'
    AfterConnect = InConnection1AfterConnect
    OnReceiveMsg = InConnection1ReceiveMsg
    OnError = InConnection1Error
    Left = 224
    Top = 304
  end
  object InCertifyClient1: TInCertifyClient
    OnReturnResult = InCertifyClient1ReturnResult
    Connection = InConnection1
    UserName = 'USER_A'
    Password = 'PASS-AA'
    OnCertify = InCertifyClient1Certify
    OnListClients = InCertifyClient1ListClients
    Left = 264
    Top = 304
  end
  object InClientManager1: TInClientManager
    OnDelete = InClientManager1Delete
    OnModify = InClientManager1Modify
    OnLogin = InClientManager1Login
    OnLogout = InClientManager1Logout
    OnRegister = InClientManager1Register
    OnQueryState = InClientManager1QueryState
    Left = 144
    Top = 304
  end
  object InConnection2: TInConnection
    LocalPath = 'temp\'
    ServerAddr = '127.0.0.1'
    OnReceiveMsg = InConnection1ReceiveMsg
    OnError = InConnection1Error
    Left = 328
    Top = 344
  end
  object InCertifyClient2: TInCertifyClient
    Connection = InConnection2
    UserName = 'USER_B'
    Password = 'PASS-BB'
    OnCertify = InCertifyClient1Certify
    OnListClients = InCertifyClient1ListClients
    Left = 368
    Top = 344
  end
end
