object FormInIOCPCustomServer: TFormInIOCPCustomServer
  Left = 0
  Top = 0
  Caption = 'InIOCP '#33258#23450#20041#28040#24687#12289#36828#31243#20989#25968#26381#21153
  ClientHeight = 407
  ClientWidth = 672
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
  object Memo1: TMemo
    Left = 8
    Top = 215
    Width = 545
    Height = 175
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      '1'#12289#24341#29992' InIOCP '#30340#32479#35745#21333#20803' frame\fmIOCPSvrInfo'
      '2'#12289#21152#20837#32479#35745#21333#20803#26694#26550'  FrameIOCPSvrInfo1'
      '3'#12289#21152#20837#12289#35774#32622#20854#20182#26381#21153#31471#12289#23458#25143#31471#32452#20214
      ''
      '4'#12289#35774#32622#25628#32034#36335#24452#65292#36816#34892#65292#21551#21160#26381#21153
      '5'#12289#21457#36865#65292#25191#34892)
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object btnStart: TButton
    Left = 576
    Top = 14
    Width = 75
    Height = 26
    Caption = #21551#21160
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 576
    Top = 45
    Width = 75
    Height = 26
    Caption = #20572#27490
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  object btnConnect: TButton
    Left = 576
    Top = 109
    Width = 75
    Height = 26
    Caption = #36830#25509
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 576
    Top = 139
    Width = 75
    Height = 26
    Caption = #26029#24320
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = btnDisconnectClick
  end
  object btnLogin: TButton
    Left = 576
    Top = 191
    Width = 75
    Height = 26
    Caption = #30331#24405
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = btnLoginClick
  end
  object btnLogout: TButton
    Left = 576
    Top = 223
    Width = 75
    Height = 26
    Caption = #30331#20986
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = btnLogoutClick
  end
  object btnCustomSend: TButton
    Left = 576
    Top = 285
    Width = 75
    Height = 26
    Caption = #21457#36865
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = btnCustomSendClick
  end
  object btnExecRemFunction: TButton
    Left = 576
    Top = 315
    Width = 75
    Height = 26
    Caption = #25191#34892
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = btnExecRemFunctionClick
  end
  object btnCall2: TButton
    Left = 576
    Top = 346
    Width = 75
    Height = 26
    Caption = #25191#34892'2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = btnCall2Click
  end
  inline FrameIOCPSvrInfo1: TFrameIOCPSvrInfo
    Left = 8
    Top = 8
    Width = 547
    Height = 201
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    ExplicitLeft = 8
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
    IOCPManagers.ClientManager = InClientManager1
    IOCPManagers.CustomManager = InCustomManager1
    IOCPManagers.FileManager = InFileManager1
    ServerAddr = '127.0.0.1'
    StartParams.TimeOut = 0
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterClose
    Left = 208
    Top = 232
  end
  object InConnection1: TInConnection
    LocalPath = 'temp'
    ServerAddr = '127.0.0.1'
    OnError = InConnection1Error
    Left = 208
    Top = 288
  end
  object InCertifyClient1: TInCertifyClient
    Connection = InConnection1
    UserName = 'USER_a'
    Password = 'PASS-AAA'
    OnCertify = InCertifyClient1Certify
    Left = 256
    Top = 288
  end
  object InClientManager1: TInClientManager
    OnLogin = InClientManager1Login
    OnLogout = InClientManager1Logout
    Left = 256
    Top = 232
  end
  object InCustomClient1: TInCustomClient
    OnReturnResult = InCustomClient1ReturnResult
    Connection = InConnection1
    Left = 304
    Top = 288
  end
  object InFunctionClient1: TInFunctionClient
    OnReturnResult = InFunctionClient1ReturnResult
    Connection = InConnection1
    Left = 352
    Top = 288
  end
  object InCustomManager1: TInCustomManager
    OnReceive = InCustomManager1Receive
    OnAttachBegin = InCustomManager1AttachBegin
    OnAttachFinish = InCustomManager1AttachFinish
    Left = 304
    Top = 232
  end
  object InRemoteFunctionGroup1: TInRemoteFunctionGroup
    CustomManager = InCustomManager1
    FunctionGroupName = 'TEST_GROUP'
    OnExecute = InRemoteFunctionGroup1Execute
    Left = 352
    Top = 232
  end
  object InRemoteFunctionGroup2: TInRemoteFunctionGroup
    CustomManager = InCustomManager1
    FunctionGroupName = 'TEST_GROUP2'
    OnExecute = InRemoteFunctionGroup2Execute
    Left = 416
    Top = 232
  end
  object InFunctionClient2: TInFunctionClient
    OnReturnResult = InFunctionClient2ReturnResult
    Connection = InConnection1
    Left = 416
    Top = 288
  end
  object InFileManager1: TInFileManager
    Left = 304
    Top = 192
  end
end
