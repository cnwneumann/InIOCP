object FormInIOCPWSChat: TFormInIOCPWSChat
  Left = 0
  Top = 0
  Caption = 'InIOCP WebSocket '#26381#21153
  ClientHeight = 349
  ClientWidth = 554
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
  object Button1: TButton
    Left = 480
    Top = 28
    Width = 60
    Height = 22
    Caption = #21551#21160
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 480
    Top = 66
    Width = 60
    Height = 23
    Caption = #20572#27490
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 6
    Top = 179
    Width = 456
    Height = 161
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
    Left = 6
    Top = 6
    Width = 449
    Height = 161
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    ExplicitLeft = 6
    ExplicitTop = 6
    ExplicitWidth = 449
    ExplicitHeight = 161
    inherited Label3: TLabel
      Left = 261
      Top = 22
      ExplicitLeft = 261
      ExplicitTop = 22
    end
    inherited lblWorkCount: TLabel
      Left = 318
      Top = 22
      ExplicitLeft = 318
      ExplicitTop = 22
    end
    inherited bvl1: TBevel
      Top = 38
      Width = 435
      Height = 1
      ExplicitTop = 38
      ExplicitWidth = 435
      ExplicitHeight = 1
    end
    inherited lbl1: TLabel
      Left = 28
      Top = 5
      ExplicitLeft = 28
      ExplicitTop = 5
    end
    inherited lbl12: TLabel
      Left = 261
      Top = 5
      ExplicitLeft = 261
      ExplicitTop = 5
    end
    inherited lblAcceptExCount: TLabel
      Left = 16
      Top = 141
      ExplicitLeft = 16
      ExplicitTop = 141
    end
    inherited lbl14: TLabel
      Left = 141
      Top = 141
      ExplicitLeft = 141
      ExplicitTop = 141
    end
    inherited lbl16: TLabel
      Left = 22
      Top = 122
      ExplicitLeft = 22
      ExplicitTop = 122
    end
    inherited lblMemeryUsed: TLabel
      Left = 261
      Top = 141
      ExplicitLeft = 261
      ExplicitTop = 141
    end
    inherited lbl19: TLabel
      Left = 28
      Top = 22
      ExplicitLeft = 28
      ExplicitTop = 22
    end
    inherited lbl3: TLabel
      Left = 40
      Top = 65
      ExplicitLeft = 40
      ExplicitTop = 65
    end
    inherited lbl6: TLabel
      Left = 28
      Top = 84
      ExplicitLeft = 28
      ExplicitTop = 84
    end
    inherited lblLeftEdge: TLabel
      Left = 10
      Top = 103
      ExplicitLeft = 10
      ExplicitTop = 103
    end
    inherited lblAcceptExCnt: TLabel
      Left = 86
      Top = 141
      ExplicitLeft = 86
      ExplicitTop = 141
    end
    inherited lblDataByteInfo: TLabel
      Left = 86
      Top = 122
      ExplicitLeft = 86
      ExplicitTop = 122
    end
    inherited lblCheckTime: TLabel
      Left = 318
      Top = 5
      ExplicitLeft = 318
      ExplicitTop = 5
    end
    inherited lblClientInfo: TLabel
      Left = 86
      Top = 46
      ExplicitLeft = 86
      ExplicitTop = 46
    end
    inherited lblCliPool: TLabel
      Left = 40
      Top = 46
      ExplicitLeft = 40
      ExplicitTop = 46
    end
    inherited lblDBConCount: TLabel
      Left = 198
      Top = 141
      ExplicitLeft = 198
      ExplicitTop = 141
    end
    inherited lblIODataInfo: TLabel
      Left = 86
      Top = 65
      ExplicitLeft = 86
      ExplicitTop = 65
    end
    inherited lblMemUsed: TLabel
      Left = 318
      Top = 141
      ExplicitLeft = 318
      ExplicitTop = 141
    end
    inherited lblDataPackInf: TLabel
      Left = 86
      Top = 103
      ExplicitLeft = 86
      ExplicitTop = 103
    end
    inherited lblStartTime: TLabel
      Left = 86
      Top = 5
      ExplicitLeft = 86
      ExplicitTop = 5
    end
    inherited lblThreadInfo: TLabel
      Left = 86
      Top = 84
      ExplicitLeft = 86
      ExplicitTop = 84
    end
    inherited lblWorkTimeLength: TLabel
      Left = 86
      Top = 22
      ExplicitLeft = 86
      ExplicitTop = 22
    end
  end
  object InIOCPServer1: TInIOCPServer
    HttpDataProvider = InHttpDataProvider1
    ServerAddr = 'localhost'
    ServerPort = 80
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
