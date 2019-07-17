object FormInIOCPProxySvr: TFormInIOCPProxySvr
  Left = 0
  Top = 0
  Caption = 'InIOCP-'#20195#29702#26381#21153
  ClientHeight = 411
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 15
  object Label1: TLabel
    Left = 167
    Top = 375
    Width = 31
    Height = 15
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'IP'#65306
  end
  object Label2: TLabel
    Left = 327
    Top = 375
    Width = 47
    Height = 15
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Port'#65306
  end
  object Button1: TButton
    Left = 435
    Top = 367
    Width = 94
    Height = 32
    Caption = #21551#21160
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 553
    Top = 367
    Width = 93
    Height = 32
    Caption = #20572#27490
    Enabled = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 17
    Top = 269
    Width = 639
    Height = 92
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      'InIOCP'#20195#29702#26381#21153#65281
      ''
      #35774#32622' TInIOCPServer '#30340' IOCPBroker '#32452#20214#21363#21487#65288#27492#26102#20854#20182#31649#29702#32452#20214#26080#25928#65289#65307
      'InIOCPBroker.ProxyType = ptDefault '#19988' OuterProxy.ServerAddr '#20026#31354#26102
      #26159#26222#36890#20195#29702#65292#35201#37096#32626#20110#29992#25143#33021#30452#25509#35775#38382#24471#21040#30340#22320#26041#12290)
    TabOrder = 2
  end
  inline FrameIOCPSvrInfo1: TFrameIOCPSvrInfo
    Left = 6
    Top = 10
    Width = 659
    Height = 251
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    ExplicitLeft = 6
    ExplicitTop = 10
    ExplicitWidth = 659
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
  object Edit1: TEdit
    Left = 198
    Top = 372
    Width = 118
    Height = 23
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 4
    Text = '127.0.0.1'
  end
  object EditPort: TEdit
    Left = 372
    Top = 372
    Width = 47
    Height = 23
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 5
    Text = '80'
  end
  object InIOCPBroker1: TInIOCPBroker
    Protocol = tpHTTP
    InnerServer.ServerAddr = '127.0.0.1'
    InnerServer.ServerPort = 900
    OnAccept = InIOCPBroker1Accept
    OnBind = InIOCPBroker1Bind
    Left = 312
    Top = 88
  end
  object InIOCPServer1: TInIOCPServer
    IOCPBroker = InIOCPBroker1
    ServerAddr = '127.0.0.1'
    ServerPort = 800
    StartParams.TimeOut = 0
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterOpen
    Left = 216
    Top = 88
  end
end
