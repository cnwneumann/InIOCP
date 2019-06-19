object FormInIOCPMessageServer: TFormInIOCPMessageServer
  Left = 0
  Top = 0
  Caption = 'InIOCP '#28040#24687#26381#21153
  ClientHeight = 491
  ClientWidth = 884
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 12
  object Bevel2: TBevel
    Left = 569
    Top = 318
    Width = 307
    Height = 113
  end
  object lbl1: TLabel
    Left = 591
    Top = 11
    Width = 36
    Height = 12
    Caption = #31471#21475#65306
  end
  object Label1: TLabel
    Left = 591
    Top = 201
    Width = 36
    Height = 12
    Caption = #29992#25143#65306
  end
  object Label2: TLabel
    Left = 575
    Top = 329
    Width = 282
    Height = 24
    Caption = #35831#20877#36816#34892#19968#27425#26412#31243#24207#65288#19981#35201#21551#21160#26381#21153#65289#65292#29992#21478#19968#29992#25143' user_b '#30331#24405#65292#30456#20114#20043#38388#21457#22312#32447#28040#24687#25110#31163#32447#28040#24687#12290
    WordWrap = True
  end
  object Label3: TLabel
    Left = 602
    Top = 381
    Width = 48
    Height = 12
    Caption = #21457#36865#21040#65306
  end
  object Bevel1: TBevel
    Left = 569
    Top = 72
    Width = 307
    Height = 233
  end
  object Memo1: TMemo
    Left = 8
    Top = 223
    Width = 547
    Height = 264
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      '1'#12289#24341#29992' InIOCP '#30340#32479#35745#21333#20803' frame\fmIOCPSvrInfo'
      '2'#12289#21152#20837#32479#35745#21333#20803#26694#26550' FrameIOCPSvrInfo1'
      '3'#12289#21152#20837#26381#21153#31471#32452#20214' InIOCPServer1'#65292#35774#32622'IP/Port'#65306'127.0.0.1/12302'
      
        '4'#12289#21152#20837#26381#21153#31471#32452#20214' InMessageManager1'#65292#35774#32622' InIOCPServer1.InMessageManager :=' +
        ' InMessageManager1'
      ''
      '5'#12289#21152#20837#12289#35774#32622#29992#25143#31649#29702#32452#20214#65306'InClientManager1'#12289'InCertifyClient1'
      ''
      '6'#12289#21152#20837#23458#25143#31471#32452#20214' InConnection1'#12289'InMessageClient1'
      
        '7'#12289#35774#32622#65306'InMessageClient1.Connection := InConnection1 '#21644' InConnection' +
        '1 '#30340'IP/Port'
      ''
      '8'#12289#35774#32622#25628#32034#36335#24452#65292#36816#34892#65292#21551#21160#26381#21153
      '9'#12289#23458#25143#31471#36830#25509#65292#30331#24405#65292#21457#36865#12289#24191#25773)
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object btnStart: TButton
    Left = 680
    Top = 30
    Width = 75
    Height = 25
    Caption = #21551#21160
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 769
    Top = 30
    Width = 75
    Height = 25
    Caption = #20572#27490
    Enabled = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  object btnConnect: TButton
    Left = 591
    Top = 92
    Width = 75
    Height = 25
    Caption = #36830#25509
    TabOrder = 3
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 591
    Top = 125
    Width = 75
    Height = 25
    Caption = #26029#24320
    TabOrder = 4
    OnClick = btnDisconnectClick
  end
  object btnSend: TButton
    Left = 750
    Top = 368
    Width = 75
    Height = 25
    Caption = #21457#36865
    TabOrder = 5
    OnClick = btnSendClick
  end
  object EditTarget: TEdit
    Left = 656
    Top = 373
    Width = 88
    Height = 20
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 6
    Text = 'user_b'
  end
  object btnBroad: TButton
    Left = 750
    Top = 399
    Width = 75
    Height = 25
    Caption = #24191#25773
    TabOrder = 7
    OnClick = btnBroadClick
  end
  object btnLogin: TButton
    Left = 591
    Top = 232
    Width = 75
    Height = 25
    Caption = #30331#24405
    TabOrder = 8
    OnClick = btnLoginClick
  end
  object btnLogout: TButton
    Left = 591
    Top = 265
    Width = 75
    Height = 25
    Caption = #30331#20986
    TabOrder = 9
    OnClick = btnLogoutClick
  end
  object EditUserName: TEdit
    Left = 591
    Top = 203
    Width = 75
    Height = 20
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 10
    Text = 'USER_A'
    OnDblClick = EditUserNameDblClick
  end
  object edtPort: TEdit
    Left = 591
    Top = 34
    Width = 75
    Height = 20
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 11
    Text = '12302'
    OnDblClick = EditUserNameDblClick
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
    TabOrder = 12
    ExplicitLeft = 8
    ExplicitTop = 16
    ExplicitWidth = 547
  end
  object BitBtn1: TBitBtn
    Left = 734
    Top = 201
    Width = 75
    Height = 25
    Caption = #31163#32447#28040#24687
    TabOrder = 13
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 734
    Top = 232
    Width = 75
    Height = 25
    Caption = #28040#24687#25991#20214
    TabOrder = 14
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 734
    Top = 265
    Width = 75
    Height = 25
    Caption = #28040#24687#21253
    TabOrder = 15
    OnClick = BitBtn3Click
  end
  object Button1: TButton
    Left = 575
    Top = 446
    Width = 75
    Height = 25
    Caption = #33258#23450#20041
    TabOrder = 16
    OnClick = Button1Click
  end
  object InIOCPServer1: TInIOCPServer
    IOCPManagers.ClientManager = InClientManager1
    IOCPManagers.CustomManager = InCustomManager1
    IOCPManagers.FileManager = InFileManager1
    IOCPManagers.MessageManager = InMessageManager1
    ServerAddr = 'localhost'
    ServerPort = 80
    StartParams.TimeOut = 0
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    OnConnect = InIOCPServer1Connect
    OnDisconnect = InIOCPServer1Disconnect
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterClose
    Left = 256
    Top = 232
  end
  object InConnection1: TInConnection
    LocalPath = 'temp\'
    ServerAddr = '127.0.0.1'
    OnReceiveMsg = InConnection1ReceiveMsg
    OnError = InConnection1Error
    Left = 256
    Top = 336
  end
  object InMessageManager1: TInMessageManager
    OnBroadcast = InMessageManager1Broadcast
    OnGet = InMessageManager1Get
    OnListFiles = InMessageManager1ListFiles
    OnPush = InMessageManager1Push
    OnReceive = InMessageManager1Receive
    Left = 336
    Top = 232
  end
  object InMessageClient1: TInMessageClient
    OnReturnResult = InMessageClient1ReturnResult
    Connection = InConnection1
    OnListFiles = InMessageClient1ListFiles
    Left = 336
    Top = 336
  end
  object InClientManager1: TInClientManager
    OnLogin = InClientManager1Login
    Left = 296
    Top = 232
  end
  object InCertifyClient1: TInCertifyClient
    OnReturnResult = InCertifyClient1ReturnResult
    Connection = InConnection1
    OnCertify = InCertifyClient1Certify
    Left = 296
    Top = 336
  end
  object InFileManager1: TInFileManager
    Left = 392
    Top = 232
  end
  object InCustomManager1: TInCustomManager
    OnReceive = InCustomManager1Receive
    Left = 496
    Top = 232
  end
  object InCustomClient1: TInCustomClient
    Connection = InConnection1
    Left = 256
    Top = 416
  end
end
