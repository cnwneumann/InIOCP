object FormInIOCPWSChat: TFormInIOCPWSChat
  Left = 0
  Top = 0
  Caption = 'InIOCP WebSocket '#26381#21153
  ClientHeight = 436
  ClientWidth = 693
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
  object Button1: TButton
    Left = 600
    Top = 35
    Width = 75
    Height = 28
    Caption = #21551#21160
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 600
    Top = 83
    Width = 75
    Height = 28
    Caption = #20572#27490
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 224
    Width = 570
    Height = 201
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #22240#22686#21152#20102#26381#21153#31471#32452#20214' TInWebSocketManager'#65292#35831#37325#26032#32534#35793' InIOCP'#12290
      ''
      #35201#25226' TInHttpDataProvider '#21644' TInWebSocketManager '#20851#32852#36215#26469#65292
      ''
      #26412#20363#20013#21482#28436#31034' WebSocket '#30340#29992#27861#65292#22914#26524#27979#35797#22823#24182#21457#65292#35831#19981#35201#22312#31383#21475#26174#31034#28040#24687#65292
      ''
      #21487#20197#25171#24320#20197#19979#32593#31449#27979#35797' WebSocket '#25928#26524#65306
      'http://www.websocketest.com/'
      'http://www.blue-zero.com/WebSocket/')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  inline FrameIOCPSvrInfo1: TFrameIOCPSvrInfo
    Left = 8
    Top = 8
    Width = 561
    Height = 201
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 561
    inherited Label3: TLabel
      Left = 326
      Top = 28
      Width = 68
      Height = 15
      ExplicitLeft = 326
      ExplicitTop = 28
      ExplicitWidth = 68
      ExplicitHeight = 15
    end
    inherited lblWorkCount: TLabel
      Top = 28
      Width = 8
      Height = 15
      ExplicitTop = 28
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited bvl1: TBevel
      Top = 48
      Height = 1
      ExplicitTop = 48
      ExplicitHeight = 1
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
      Top = 28
      Width = 68
      Height = 15
      ExplicitLeft = 35
      ExplicitTop = 28
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
      Left = 108
      Width = 8
      Height = 15
      ExplicitLeft = 108
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblDataByteInfo: TLabel
      Left = 108
      Width = 8
      Height = 15
      ExplicitLeft = 108
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
      Left = 108
      Top = 58
      Width = 8
      Height = 15
      ExplicitLeft = 108
      ExplicitTop = 58
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblCliPool: TLabel
      Left = 50
      Top = 58
      Width = 53
      Height = 15
      ExplicitLeft = 50
      ExplicitTop = 58
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
      Left = 108
      Width = 8
      Height = 15
      ExplicitLeft = 108
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
      Left = 108
      Width = 8
      Height = 15
      ExplicitLeft = 108
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblStartTime: TLabel
      Left = 108
      Width = 8
      Height = 15
      ExplicitLeft = 108
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblThreadInfo: TLabel
      Left = 108
      Width = 8
      Height = 15
      ExplicitLeft = 108
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
    inherited lblWorkTimeLength: TLabel
      Left = 108
      Top = 28
      Width = 8
      Height = 15
      ExplicitLeft = 108
      ExplicitTop = 28
      ExplicitWidth = 8
      ExplicitHeight = 15
    end
  end
  object InIOCPServer1: TInIOCPServer
    HttpDataProvider = InHttpDataProvider1
    ServerAddr = 'localhost'
    ServerPort = 80
    StartParams.TimeOut = 0
    ThreadOptions.BusinessThreadCount = 4
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 2
    AfterOpen = InIOCPServer1AfterOpen
    Left = 168
    Top = 240
  end
  object InHttpDataProvider1: TInHttpDataProvider
    OnAccept = InHttpDataProvider1Accept
    WebSocketManager = InWebSocketManager1
    Left = 232
    Top = 240
  end
  object InWebSocketManager1: TInWebSocketManager
    OnReceive = InWebSocketManager1Receive
    OnUpgrade = InWebSocketManager1Upgrade
    Left = 296
    Top = 240
  end
end
