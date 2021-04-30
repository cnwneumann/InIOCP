object FormInIOCPDBServerBGThread: TFormInIOCPDBServerBGThread
  Left = 0
  Top = 0
  Caption = 'InIOCP '#28040#24687#12289#25968#25454#24211#26381#21153' - '#21518#21488#32447#31243
  ClientHeight = 342
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
  PixelsPerInch = 96
  TextHeight = 17
  object Memo1: TMemo
    Left = 8
    Top = 225
    Width = 433
    Height = 104
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #27492#20363#23376#38598#21512#28040#24687#21644#25968#25454#24211#26381#21153#65292#20294#26381#21153#31471#20351#29992#21518#21488#22788#29702#25968#25454#26597#35810#65292#35265#65306
      'TdmInIOCPTest.InIOCPDataModuleExecQuery')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object btnStart: TButton
    Left = 463
    Top = 225
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
    Top = 263
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
    inherited Label3: TLabel
      Left = 326
      Width = 68
      Height = 15
      ExplicitLeft = 326
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lblWorkCount: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lbl1: TLabel
      Left = 35
      Width = 68
      Height = 15
      ExplicitLeft = 35
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lbl12: TLabel
      Left = 326
      Width = 68
      Height = 15
      ExplicitLeft = 326
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lblAcceptExCount: TLabel
      Left = 16
      Width = 87
      Height = 15
      ExplicitLeft = 16
      ExplicitWidth = 87
      ExplicitHeight = 15
    end
    inherited lbl14: TLabel
      Left = 176
      Width = 68
      Height = 15
      ExplicitLeft = 176
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lbl16: TLabel
      Left = 26
      Width = 77
      Height = 15
      ExplicitLeft = 26
      ExplicitWidth = 77
      ExplicitHeight = 15
    end
    inherited lblMemeryUsed: TLabel
      Left = 326
      Width = 68
      Height = 15
      ExplicitLeft = 326
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lbl19: TLabel
      Left = 35
      Width = 68
      Height = 15
      ExplicitLeft = 35
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lbl3: TLabel
      Left = 50
      Width = 53
      Height = 15
      ExplicitLeft = 50
      ExplicitWidth = 53
      ExplicitHeight = 15
    end
    inherited lbl6: TLabel
      Left = 35
      Width = 68
      Height = 15
      ExplicitLeft = 35
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lblLeftEdge: TLabel
      Left = 11
      Width = 92
      Height = 15
      ExplicitLeft = 11
      ExplicitWidth = 92
      ExplicitHeight = 15
    end
    inherited lblAcceptExCnt: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblDataByteInfo: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblCheckTime: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblClientInfo: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblCliPool: TLabel
      Left = 50
      Width = 53
      Height = 15
      ExplicitLeft = 50
      ExplicitWidth = 53
      ExplicitHeight = 15
    end
    inherited lblDBConCount: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblIODataInfo: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblMemUsed: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblDataPackInf: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblStartTime: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblThreadInfo: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblWorkTimeLength: TLabel
      Width = 8
      Height = 15
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
  end
  object InIOCPServer1: TInIOCPServer
    HttpDataProvider = InHttpDataProvider1
    IOCPManagers.ClientManager = InClientManager1
    IOCPManagers.DatabaseManager = InDatabaseManager1
    IOCPManagers.FileManager = InFileManager1
    IOCPManagers.MessageManager = InMessageManager1
    ServerAddr = '127.0.0.1'
    StartParams.TimeOut = 0
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterClose
    Left = 104
    Top = 240
  end
  object InClientManager1: TInClientManager
    OnLogin = InClientManager1Login
    Left = 160
    Top = 240
  end
  object InDatabaseManager1: TInDatabaseManager
    Left = 344
    Top = 264
  end
  object InMessageManager1: TInMessageManager
    OnListFiles = InMessageManager1ListFiles
    Left = 224
    Top = 240
  end
  object InFileManager1: TInFileManager
    BeforeDownload = InFileManager1BeforeDownload
    Left = 296
    Top = 264
  end
  object InHttpDataProvider1: TInHttpDataProvider
    WebSocketManager = InWebSocketManager1
    Left = 296
    Top = 96
  end
  object InWebSocketManager1: TInWebSocketManager
    OnReceive = InWebSocketManager1Receive
    Left = 352
    Top = 96
  end
end
