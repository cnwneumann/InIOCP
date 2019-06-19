object FormInIOCPHttpServer: TFormInIOCPHttpServer
  Left = 339
  Top = 271
  Caption = 'InIOCP HTTP '#26381#21153
  ClientHeight = 514
  ClientWidth = 728
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 15
  object Memo1: TMemo
    Left = 8
    Top = 268
    Width = 583
    Height = 227
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #26032#29256#25903#25345' http '#22522#26412#26381#21153#65281
      ''
      #25171#24320#27983#35272#22120#65292#35775#38382' 127.0.0.1:80'
      ''
      #21487#32467#21512' TInClientManager '#31649#29702#29992#25143#20449#24687#65292'Session '#30340#20351#29992#65306
      ''
      '1'#12289#29983#25104#65306'Respone.CreateSession'
      '2'#12289#21024#38500#65306'Respone.InvalidSession'
      ''
      #25968#25454#24211#20351#29992#65292#20808#21152#20837#21333#20803' iocp_baseModule'#65292#20174#20854#32487#25215#19968#20010#19994#21153#29992#30340#31867' A'#65292
      #27880#20876#31867' A '#21040#25968#25454#31649#29702#22120#65292#36825#26679#25805#20316#25968#25454#24211#65306
      '1'#12289' TBusiWorker(Sender).DataModule.HttpExecSQL(Request, Respone);'
      
        '2'#12289' TBusiWorker(Sender).DataModule.HttpExecQuery(Request, Respone' +
        ');'
      
        '3'#12289' TBusiWorker(Sender).DataModules[0].HttpExecSQL(Request, Respo' +
        'ne);'
      
        '4'#12289' TBusiWorker(Sender).DataModules[0].HttpExecQuery(Request, Res' +
        'pone);')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object btnStart: TButton
    Left = 613
    Top = 299
    Width = 93
    Height = 31
    Caption = #21551#21160
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 613
    Top = 349
    Width = 93
    Height = 30
    Caption = #20572#27490
    Enabled = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  inline FrameIOCPSvrInfo1: TFrameIOCPSvrInfo
    Left = 22
    Top = 2
    Width = 684
    Height = 251
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    ExplicitLeft = 22
    ExplicitTop = 2
    ExplicitWidth = 684
    ExplicitHeight = 251
    inherited Label3: TLabel
      Left = 412
      Top = 34
      Width = 81
      Height = 18
      ExplicitLeft = 412
      ExplicitTop = 34
      ExplicitWidth = 81
      ExplicitHeight = 18
    end
    inherited lblWorkCount: TLabel
      Left = 498
      Top = 34
      Width = 9
      Height = 18
      ExplicitLeft = 498
      ExplicitTop = 34
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited bvl1: TBevel
      Top = 59
      Width = 680
      ExplicitTop = 59
      ExplicitWidth = 680
    end
    inherited lbl1: TLabel
      Left = 48
      Top = 8
      Width = 81
      Height = 18
      ExplicitLeft = 48
      ExplicitTop = 8
      ExplicitWidth = 81
      ExplicitHeight = 18
    end
    inherited lbl12: TLabel
      Left = 412
      Top = 8
      Width = 81
      Height = 18
      ExplicitLeft = 412
      ExplicitTop = 8
      ExplicitWidth = 81
      ExplicitHeight = 18
    end
    inherited lblAcceptExCount: TLabel
      Left = 30
      Top = 220
      Width = 99
      Height = 18
      ExplicitLeft = 30
      ExplicitTop = 220
      ExplicitWidth = 99
      ExplicitHeight = 18
    end
    inherited lbl14: TLabel
      Left = 224
      Top = 220
      Width = 81
      Height = 18
      ExplicitLeft = 224
      ExplicitTop = 220
      ExplicitWidth = 81
      ExplicitHeight = 18
    end
    inherited lbl16: TLabel
      Left = 39
      Top = 191
      Width = 90
      Height = 18
      ExplicitLeft = 39
      ExplicitTop = 191
      ExplicitWidth = 90
      ExplicitHeight = 18
    end
    inherited lblMemeryUsed: TLabel
      Left = 412
      Top = 220
      Width = 81
      Height = 18
      ExplicitLeft = 412
      ExplicitTop = 220
      ExplicitWidth = 81
      ExplicitHeight = 18
    end
    inherited lbl19: TLabel
      Left = 48
      Top = 34
      Width = 81
      Height = 18
      ExplicitLeft = 48
      ExplicitTop = 34
      ExplicitWidth = 81
      ExplicitHeight = 18
    end
    inherited lbl3: TLabel
      Left = 66
      Top = 101
      Width = 63
      Height = 18
      ExplicitLeft = 66
      ExplicitTop = 101
      ExplicitWidth = 63
      ExplicitHeight = 18
    end
    inherited lbl6: TLabel
      Left = 48
      Top = 131
      Width = 81
      Height = 18
      ExplicitLeft = 48
      ExplicitTop = 131
      ExplicitWidth = 81
      ExplicitHeight = 18
    end
    inherited lblLeftEdge: TLabel
      Left = 21
      Top = 161
      Width = 108
      Height = 18
      ExplicitLeft = 21
      ExplicitTop = 161
      ExplicitWidth = 108
      ExplicitHeight = 18
    end
    inherited lblAcceptExCnt: TLabel
      Left = 134
      Top = 220
      Width = 9
      Height = 18
      ExplicitLeft = 134
      ExplicitTop = 220
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblDataByteInfo: TLabel
      Left = 134
      Top = 191
      Width = 9
      Height = 18
      ExplicitLeft = 134
      ExplicitTop = 191
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblCheckTime: TLabel
      Left = 498
      Top = 8
      Width = 9
      Height = 18
      ExplicitLeft = 498
      ExplicitTop = 8
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblClientInfo: TLabel
      Left = 134
      Top = 71
      Width = 9
      Height = 18
      ExplicitLeft = 134
      ExplicitTop = 71
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblCliPool: TLabel
      Left = 66
      Top = 71
      Width = 63
      Height = 18
      ExplicitLeft = 66
      ExplicitTop = 71
      ExplicitWidth = 63
      ExplicitHeight = 18
    end
    inherited lblDBConCount: TLabel
      Left = 310
      Top = 220
      Width = 9
      Height = 18
      ExplicitLeft = 310
      ExplicitTop = 220
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblIODataInfo: TLabel
      Left = 134
      Top = 101
      Width = 9
      Height = 18
      ExplicitLeft = 134
      ExplicitTop = 101
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblMemUsed: TLabel
      Left = 498
      Top = 220
      Width = 9
      Height = 18
      ExplicitLeft = 498
      ExplicitTop = 220
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblDataPackInf: TLabel
      Left = 134
      Top = 161
      Width = 9
      Height = 18
      ExplicitLeft = 134
      ExplicitTop = 161
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblStartTime: TLabel
      Left = 134
      Top = 8
      Width = 9
      Height = 18
      ExplicitLeft = 134
      ExplicitTop = 8
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblThreadInfo: TLabel
      Left = 134
      Top = 131
      Width = 9
      Height = 18
      ExplicitLeft = 134
      ExplicitTop = 131
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
    inherited lblWorkTimeLength: TLabel
      Left = 134
      Top = 34
      Width = 9
      Height = 18
      ExplicitLeft = 134
      ExplicitTop = 34
      ExplicitWidth = 9
      ExplicitHeight = 18
    end
  end
  object InIOCPServer1: TInIOCPServer
    HttpDataProvider = InHttpDataProvider1
    IOCPManagers.DatabaseManager = InDatabaseManager1
    ServerAddr = '127.0.0.1'
    ServerPort = 80
    StartParams.TimeOut = 0
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterClose
    Left = 320
    Top = 96
  end
  object InHttpDataProvider1: TInHttpDataProvider
    OnAccept = InHttpDataProvider1Accept
    OnGet = InHttpDataProvider1Get
    OnInvalidSession = InHttpDataProvider1InvalidSession
    OnPost = InHttpDataProvider1Post
    OnReceiveFile = InHttpDataProvider1ReceiveFile
    RootDirectory = 'web_site'
    Left = 432
    Top = 96
  end
  object InDatabaseManager1: TInDatabaseManager
    Left = 320
    Top = 160
  end
end
