object FormIOCPOutProxySvr: TFormIOCPOutProxySvr
  Left = 0
  Top = 0
  Caption = 'InIOCP - '#21453#21521#20195#29702#26381#21153#65288#22806#37096#65289
  ClientHeight = 459
  ClientWidth = 676
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
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 221
    Top = 416
    Width = 61
    Height = 15
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #26381#21153'IP'#65306
  end
  object Button1: TButton
    Left = 466
    Top = 406
    Width = 94
    Height = 33
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #21551#21160
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 574
    Top = 406
    Width = 94
    Height = 33
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = #20572#27490
    Enabled = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 286
    Top = 413
    Width = 118
    Height = 23
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 2
    Text = '127.0.0.1'
    OnDblClick = Edit1DblClick
  end
  object EditPort: TEdit
    Left = 408
    Top = 413
    Width = 47
    Height = 23
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 3
    Text = '80'
  end
  object Memo1: TMemo
    Left = 9
    Top = 268
    Width = 659
    Height = 125
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      'InIOCP'#21453#21521#20195#29702#30340#22806#37096#26381#21153#65281
      ''
      'InIOCPBroker1 '#30340' ProxyType = ptOuter '#26102#65292#26159#22806#37096#20195#29702#65292
      #35201#37096#32626#20110#29992#25143#33021#35775#38382#24471#21040#30340#22320#26041#65292#23427#21644#21453#21521#20195#29702#30340#20869#37096#26381#21153#37197#21512#35775#38382#20869#32593#12290
      ''
      #22806#37096#20195#29702#19982#21327#35758' Protocol '#26080#20851#12290)
    TabOrder = 4
  end
  inline FrameIOCPSvrInfo1: TFrameIOCPSvrInfo
    Left = 9
    Top = 10
    Width = 659
    Height = 251
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    ExplicitLeft = 9
    ExplicitTop = 10
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
  object InIOCPServer1: TInIOCPServer
    IOCPBroker = InIOCPBroker1
    ServerAddr = '127.0.0.1'
    ServerPort = 900
    StartParams.TimeOut = 0
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    AfterOpen = InIOCPServer1AfterClose
    AfterClose = InIOCPServer1AfterClose
    Left = 208
    Top = 88
  end
  object InIOCPBroker1: TInIOCPBroker
    ProxyType = ptOuter
    OnBind = InIOCPBroker1Bind
    Left = 296
    Top = 88
  end
end
