object FormInIOCPMsgServer: TFormInIOCPMsgServer
  Left = 0
  Top = 0
  Caption = 'InIOCP-'#28040#24687#26381#21153
  ClientHeight = 373
  ClientWidth = 828
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 15
  object btnStart: TButton
    Left = 678
    Top = 145
    Width = 93
    Height = 31
    Caption = #21551#21160
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 678
    Top = 193
    Width = 93
    Height = 31
    Caption = #20572#27490
    Enabled = False
    TabOrder = 1
    OnClick = btnStartClick
  end
  inline FrameIOCPSvrInfo1: TFrameIOCPSvrInfo
    Left = 0
    Top = 0
    Width = 659
    Height = 251
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    ExplicitWidth = 659
    ExplicitHeight = 251
    inherited Label3: TLabel
      Left = 425
      Top = 34
      Width = 68
      Height = 15
      ExplicitLeft = 425
      ExplicitTop = 34
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lblWorkCount: TLabel
      Left = 498
      Top = 34
      Width = 8
      Height = 15
      ExplicitLeft = 498
      ExplicitTop = 34
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited bvl1: TBevel
      Top = 59
      Width = 680
      ExplicitTop = 59
      ExplicitWidth = 680
    end
    inherited lbl1: TLabel
      Left = 61
      Top = 8
      Width = 68
      Height = 15
      ExplicitLeft = 61
      ExplicitTop = 8
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lbl12: TLabel
      Left = 425
      Top = 8
      Width = 68
      Height = 15
      ExplicitLeft = 425
      ExplicitTop = 8
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lblAcceptExCount: TLabel
      Left = 42
      Top = 220
      Width = 87
      Height = 15
      ExplicitLeft = 42
      ExplicitTop = 220
      ExplicitWidth = 87
      ExplicitHeight = 15
    end
    inherited lbl14: TLabel
      Left = 237
      Top = 220
      Width = 68
      Height = 15
      ExplicitLeft = 237
      ExplicitTop = 220
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lbl16: TLabel
      Left = 52
      Top = 191
      Width = 77
      Height = 15
      ExplicitLeft = 52
      ExplicitTop = 191
      ExplicitWidth = 77
      ExplicitHeight = 15
    end
    inherited lblMemeryUsed: TLabel
      Left = 425
      Top = 220
      Width = 68
      Height = 15
      ExplicitLeft = 425
      ExplicitTop = 220
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lbl19: TLabel
      Left = 61
      Top = 34
      Width = 68
      Height = 15
      ExplicitLeft = 61
      ExplicitTop = 34
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lbl3: TLabel
      Left = 76
      Top = 101
      Width = 53
      Height = 15
      ExplicitLeft = 76
      ExplicitTop = 101
      ExplicitWidth = 53
      ExplicitHeight = 15
    end
    inherited lbl6: TLabel
      Left = 61
      Top = 131
      Width = 68
      Height = 15
      ExplicitLeft = 61
      ExplicitTop = 131
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lblLeftEdge: TLabel
      Left = 37
      Top = 161
      Width = 92
      Height = 15
      ExplicitLeft = 37
      ExplicitTop = 161
      ExplicitWidth = 92
      ExplicitHeight = 15
    end
    inherited lblAcceptExCnt: TLabel
      Left = 134
      Top = 220
      Width = 8
      Height = 15
      ExplicitLeft = 134
      ExplicitTop = 220
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblDataByteInfo: TLabel
      Left = 134
      Top = 191
      Width = 8
      Height = 15
      ExplicitLeft = 134
      ExplicitTop = 191
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblCheckTime: TLabel
      Left = 498
      Top = 8
      Width = 8
      Height = 15
      ExplicitLeft = 498
      ExplicitTop = 8
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblClientInfo: TLabel
      Left = 134
      Top = 71
      Width = 8
      Height = 15
      ExplicitLeft = 134
      ExplicitTop = 71
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblCliPool: TLabel
      Left = 76
      Top = 71
      Width = 53
      Height = 15
      ExplicitLeft = 76
      ExplicitTop = 71
      ExplicitWidth = 53
      ExplicitHeight = 15
    end
    inherited lblDBConCount: TLabel
      Left = 310
      Top = 220
      Width = 8
      Height = 15
      ExplicitLeft = 310
      ExplicitTop = 220
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblIODataInfo: TLabel
      Left = 134
      Top = 101
      Width = 8
      Height = 15
      ExplicitLeft = 134
      ExplicitTop = 101
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblMemUsed: TLabel
      Left = 498
      Top = 220
      Width = 8
      Height = 15
      ExplicitLeft = 498
      ExplicitTop = 220
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblDataPackInf: TLabel
      Left = 134
      Top = 161
      Width = 8
      Height = 15
      ExplicitLeft = 134
      ExplicitTop = 161
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblStartTime: TLabel
      Left = 134
      Top = 8
      Width = 8
      Height = 15
      ExplicitLeft = 134
      ExplicitTop = 8
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblThreadInfo: TLabel
      Left = 134
      Top = 131
      Width = 8
      Height = 15
      ExplicitLeft = 134
      ExplicitTop = 131
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblWorkTimeLength: TLabel
      Left = 134
      Top = 34
      Width = 8
      Height = 15
      ExplicitLeft = 134
      ExplicitTop = 34
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
  end
  object Memo1: TMemo
    Left = 8
    Top = 259
    Width = 651
    Height = 102
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #27880#24847#65306#23458#25143#31471#30331#24405#21518#65292#30331#24405#21517' userName '#34987#20445#23384#21040' TIOCPSocket.Evir '#20013#65292
      #21518#32487#28040#24687#21253#30340' userName '#21487#33021#34987#25913#21464#12290#23427#21644#29992#25143#25968#25454#34920#30340' user_name '#19981#21516#65292
      #20351#29992#26102#19981#35201#25630#28151#12290
      #36825#26159#31034#33539#20195#30721#65292#20197#21518#26410#24517#32487#32493#26356#26032#12290)
    TabOrder = 3
  end
  object lbEditIP: TLabeledEdit
    Left = 678
    Top = 30
    Width = 127
    Height = 23
    EditLabel.Width = 24
    EditLabel.Height = 15
    EditLabel.Caption = 'IP:'
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 4
    Text = '127.0.0.1'
  end
  object lbEditPort: TLabeledEdit
    Left = 678
    Top = 83
    Width = 65
    Height = 23
    EditLabel.Width = 40
    EditLabel.Height = 15
    EditLabel.Caption = 'Port:'
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 5
    Text = '800'
  end
  object Button1: TButton
    Left = 680
    Top = 250
    Width = 94
    Height = 31
    Caption = 'Test'
    TabOrder = 6
  end
  object InIOCPServer1: TInIOCPServer
    HttpDataProvider = InHttpDataProvider1
    IOCPManagers.ClientManager = InClientManager1
    IOCPManagers.DatabaseManager = InDatabaseManager1
    IOCPManagers.FileManager = InFileManager1
    IOCPManagers.MessageManager = InMessageManager1
    ServerAddr = '127.0.0.1'
    ServerPort = 80
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    OnDisconnect = InIOCPServer1Disconnect
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterOpen
    Left = 272
    Top = 48
  end
  object InFileManager1: TInFileManager
    AfterDownload = InFileManager1AfterDownload
    AfterUpload = InFileManager1AfterUpload
    BeforeUpload = InFileManager1BeforeUpload
    BeforeDownload = InFileManager1BeforeDownload
    OnQueryFiles = InFileManager1QueryFiles
    Left = 368
    Top = 88
  end
  object InClientManager1: TInClientManager
    OnDelete = InClientManager1Delete
    OnModify = InClientManager1Modify
    OnLogin = InClientManager1Login
    OnLogout = InClientManager1Logout
    OnRegister = InClientManager1Register
    OnQueryState = InClientManager1QueryState
    Left = 272
    Top = 88
  end
  object InMessageManager1: TInMessageManager
    OnBroadcast = InMessageManager1Broadcast
    OnGet = InMessageManager1Get
    OnListFiles = InMessageManager1ListFiles
    OnPush = InMessageManager1Push
    OnReceive = InMessageManager1Receive
    Left = 320
    Top = 88
  end
  object InDatabaseManager1: TInDatabaseManager
    Left = 416
    Top = 88
  end
  object InHttpDataProvider1: TInHttpDataProvider
    OnGet = InHttpDataProvider1Get
    Left = 464
    Top = 88
  end
end
