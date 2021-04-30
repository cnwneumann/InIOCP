object FormTestIOCPServer: TFormTestIOCPServer
  Left = 268
  Top = 181
  Caption = 'InIOCP v2.6'
  ClientHeight = 615
  ClientWidth = 987
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object bvl2: TBevel
    Left = 719
    Top = 14
    Width = 5
    Height = 597
    Style = bsRaised
  end
  object mmoServer: TMemo
    Left = 8
    Top = 215
    Width = 580
    Height = 164
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      
        '    InIOCP '#26159#19968#22871#22522#20110'IOCP'#65288'Delphi '#29256#65289#30340#24320#28304#26694#26550#32452#20214#65292#20869#37096#20316#20102#28040#24687#23553#35013#65292#37319#29992#29420#29305#30340#20256#36755#26426#21046#65292#24110#21161#24555#36895#23454#29616#32593 +
        #32476#24320#21457#12290
      ''
      '    '#65288#19968#65289#20027#35201#21151#33021#65306
      ''
      '    1'#12289#21407#22987#25968#25454#27969#26381#21153#65307
      ''
      '    2'#12289#35748#35777#26381#21153#65306#30331#24405#12289#30331#20986#12289#26597#35810#23458#25143#31471#65307
      ''
      '    3'#12289#21363#26102#28040#24687#26381#21153#65306#28040#24687#30340#21457#36865#12289#25512#36865#21644#24191#25773#65307
      ''
      '    4'#12289#25991#20214#26381#21153#65306#25903#25345#22823#25991#20214#19978#20256#12289#19979#36733#65292#25903#25345#26029#28857#32493#20256#12289#26597#35810#30913#30424#25991#20214#31561#65307
      ''
      '    5'#12289#25968#25454#24211#26381#21153#65306#25903#25345' SQL '#21629#20196#26597#35810#12289#26356#26032#65292#25903#25345' DataSetPrivoder '#26356#26032#65307
      ''
      '    6'#12289#33258#23450#20041#26381#21153#65306#23458#25143#31471#33258#23450#20041#20256#36755#20449#24687#65292#26381#21153#31471#33258#23450#20041#25805#20316#65292#25191#34892#36828#31243#20989#25968#65307
      ''
      '    7'#12289'HTTP'#22522#26412#26381#21153#65306#19968#20010#26381#21153#22120#21516#26102#25903#25345' C/S '#26381#21153#27169#24335#21644' HTTP '#22522#26412#26381#21153#65307
      ''
      '    8'#12289#25903#25345' WebSocket '#21327#35758#65292#29992' InIOCP-JSON '#25193#23637#65292#24102#23458#25143#31471#32452#20214#65292#25903#25345#28040#24687#25512#36865#65307
      ''
      '    9'#12289#25903#25345#20195#29702#26381#21153#12290
      ''
      '    '#65288#20108#65289#20027#35201#29305#28857#65306
      ''
      '    '#24037#20316#32447#31243#36127#36131#20256#36755#65292#19994#21153#32447#31243#36127#36131#19994#21153#25805#20316#65292#21152#24555#21709#24212#36895#24230#65307
      ''
      '    '#25903#25345' IP '#32423#21035#30340#38450#25915#20987#12289#25903#25345#25968#27169#28909#25554#25300#65288#21160#24577#37197#32622#65289#65307
      ''
      '    C/S'#27169#24335#25903#25345#21387#32553#12289#26657#39564#30721#12289#36830#25509#36229#26102#26816#26597#12289#36164#28304#33258#21160#20248#21270#12289#30701#36830#25509#65307
      ''
      '    HTTP '#26381#21153#25903#25345' ETag '#21644#26029#28857#19979#36733#65292#25903#25345' Cookie'#12289'Content-Length '#32423#21035#30340#38450#25915#20987#12290)
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
    WordWrap = False
  end
  object btnStart: TButton
    Left = 604
    Top = 53
    Width = 96
    Height = 31
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 604
    Top = 88
    Width = 96
    Height = 31
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = btnStopClick
  end
  object chkSilence: TCheckBox
    Left = 604
    Top = 129
    Width = 80
    Height = 17
    Caption = 'Silence'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object btnConnect: TButton
    Left = 604
    Top = 164
    Width = 96
    Height = 32
    Caption = 'Connect'
    TabOrder = 4
    OnClick = btnConnectClick
  end
  object btnEcho: TButton
    Left = 604
    Top = 197
    Width = 96
    Height = 34
    Caption = 'Echo'
    TabOrder = 5
    OnClick = btnEchoClick
  end
  object btnLoginx: TButton
    Left = 604
    Top = 236
    Width = 96
    Height = 32
    Caption = 'Login'
    TabOrder = 6
    OnClick = btnLoginxClick
  end
  object edtHost: TEdit
    Left = 604
    Top = 14
    Width = 110
    Height = 20
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 7
    Text = 'localhost'
    OnDblClick = edtHostDblClick
  end
  object btnDownload: TButton
    Left = 736
    Top = 449
    Width = 98
    Height = 35
    Caption = #32493#20256'Download'
    TabOrder = 8
    OnClick = btnDownloadClick
  end
  object edtTo: TEdit
    Left = 604
    Top = 314
    Width = 96
    Height = 20
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 9
  end
  object btnClearMemo: TButton
    Left = 738
    Top = 565
    Width = 96
    Height = 32
    Caption = 'ClearMemo'
    TabOrder = 10
    OnClick = btnClearMemoClick
  end
  object btnListClient: TButton
    Left = 604
    Top = 274
    Width = 96
    Height = 32
    Caption = 'List Clients'
    TabOrder = 11
    OnClick = btnListClientClick
  end
  object btnCon: TButton
    Left = 736
    Top = 53
    Width = 98
    Height = 30
    Caption = 'Connect'
    TabOrder = 12
    OnClick = btnConClick
  end
  object btnLog: TButton
    Left = 736
    Top = 88
    Width = 98
    Height = 30
    Caption = 'Login'
    TabOrder = 13
    OnClick = btnLogClick
  end
  object btnCast: TButton
    Left = 604
    Top = 376
    Width = 96
    Height = 34
    Caption = 'Broadcast'
    TabOrder = 14
    OnClick = btnCastClick
  end
  object btnSendTo: TButton
    Left = 604
    Top = 343
    Width = 96
    Height = 33
    Caption = 'Send Text'
    TabOrder = 15
    OnClick = btnSendToClick
  end
  object btnCustmSnd: TButton
    Left = 604
    Top = 416
    Width = 96
    Height = 30
    Caption = 'Custom Pack'
    TabOrder = 16
    OnClick = btnCustmSndClick
  end
  object btnSndTo2: TButton
    Left = 736
    Top = 168
    Width = 98
    Height = 32
    Caption = 'Send Text'
    TabOrder = 17
    OnClick = btnSndTo2Click
  end
  object edtTo2: TEdit
    Left = 736
    Top = 128
    Width = 98
    Height = 20
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 18
    Text = 'USER_A'
  end
  object btnUpload: TButton
    Left = 736
    Top = 410
    Width = 98
    Height = 33
    Caption = #32493#20256'Upload'
    TabOrder = 19
    OnClick = btnUploadClick
  end
  object btnQueryDir: TButton
    Left = 736
    Top = 333
    Width = 98
    Height = 33
    Caption = 'Query Files'
    TabOrder = 20
    OnClick = btnQueryDirClick
  end
  object btn1: TButton
    Left = 736
    Top = 248
    Width = 98
    Height = 32
    Caption = 'Send File'
    Enabled = False
    TabOrder = 21
    OnClick = btn1Click
  end
  object btnExecSQL: TButton
    Left = 602
    Top = 563
    Width = 98
    Height = 32
    Caption = 'SQL Update'
    TabOrder = 22
    OnClick = btnExecSQLClick
  end
  object btnExecQuery: TButton
    Left = 604
    Top = 528
    Width = 97
    Height = 35
    Caption = 'ExecQuery'
    TabOrder = 23
    OnClick = btnExecQueryClick
  end
  object btnCast2: TButton
    Left = 736
    Top = 208
    Width = 97
    Height = 32
    Caption = 'Broadcast'
    TabOrder = 24
    OnClick = btnCast2Click
  end
  object btnSetDir: TButton
    Left = 736
    Top = 295
    Width = 98
    Height = 35
    Caption = 'Set sub'
    TabOrder = 25
    OnClick = btnSetDirClick
  end
  object btnCallFunc: TButton
    Left = 736
    Top = 371
    Width = 98
    Height = 34
    Caption = 'Call Func'
    TabOrder = 26
    OnClick = btnCallFuncClick
  end
  object edtPort: TEdit
    Left = 736
    Top = 15
    Width = 86
    Height = 20
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 27
    Text = '80'
    OnDblClick = edtHostDblClick
  end
  object btnCancel: TButton
    Left = 736
    Top = 488
    Width = 98
    Height = 33
    Caption = 'Cancel'
    TabOrder = 28
    OnClick = btnCancelClick
  end
  object ComboBox1: TComboBox
    Left = 600
    Top = 460
    Width = 111
    Height = 20
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    ItemHeight = 12
    TabOrder = 29
  end
  object btnDBConnection: TButton
    Left = 604
    Top = 493
    Width = 97
    Height = 32
    Caption = 'Get Conns'
    TabOrder = 30
    OnClick = btnDBConnectionClick
  end
  object PageControl1: TPageControl
    Left = 10
    Top = 382
    Width = 582
    Height = 219
    ActivePage = TabSheet1
    MultiLine = True
    TabOrder = 31
    object TabSheet1: TTabSheet
      Caption = #25805#20316#20449#24687
      object mmoClient: TMemo
        Left = 0
        Top = 0
        Width = 574
        Height = 191
        Align = alClient
        ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
        Lines.Strings = (
          'mmoClient')
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = #26597#35810#32467#26524
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 574
        Height = 191
        Align = alClient
        DataSource = DataSource1
        ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -12
        TitleFont.Name = #23435#20307
        TitleFont.Style = []
      end
    end
    object ts1: TTabSheet
      Caption = #25991#20214#20114#20256
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbl1: TLabel
        Left = 24
        Top = 13
        Width = 48
        Height = 12
        Caption = #21457#36215#26041#65306
      end
      object lblFileB: TLabel
        Left = 64
        Top = 33
        Width = 50
        Height = 18
        Caption = '..........'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lblCancelB: TLabel
        Left = 460
        Top = 33
        Width = 60
        Height = 18
        Cursor = crHandPoint
        Caption = #21462#28040#21457#36865
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lbl2: TLabel
        Left = 24
        Top = 84
        Width = 48
        Height = 12
        Caption = #25509#25910#26041#65306
      end
      object lblTarget: TLabel
        Left = 64
        Top = 105
        Width = 50
        Height = 18
        Cursor = crHandPoint
        Caption = '..........'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lblTargetCancel: TLabel
        Left = 460
        Top = 105
        Width = 66
        Height = 18
        Cursor = crHandPoint
        Caption = #21462#28040'/'#25298#32477
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
    object TabSheet3: TTabSheet
      Caption = #25968#27169#31649#29702
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MemoDMInfos: TMemo
        Left = 13
        Top = 16
        Width = 436
        Height = 153
        ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object edtDMNo: TEdit
        Left = 464
        Top = 107
        Width = 83
        Height = 20
        ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
        TabOrder = 1
        Text = '0'
      end
      object DeleteDM: TButton
        Left = 464
        Top = 136
        Width = 83
        Height = 25
        Caption = 'Delete'
        TabOrder = 2
        OnClick = DeleteDMClick
      end
      object listDM: TButton
        Left = 464
        Top = 24
        Width = 83
        Height = 25
        Caption = 'List'
        TabOrder = 3
        OnClick = listDMClick
      end
      object addDM: TButton
        Left = 464
        Top = 61
        Width = 83
        Height = 25
        Caption = 'Add'
        TabOrder = 4
        OnClick = addDMClick
      end
    end
  end
  object Button1: TButton
    Left = 737
    Top = 527
    Width = 97
    Height = 32
    Caption = #26032#29305#24615#65281
    TabOrder = 32
    OnClick = Button1Click
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
    TabOrder = 33
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
  object btnWSConnect: TButton
    Left = 856
    Top = 53
    Width = 98
    Height = 30
    Caption = 'WSConnect'
    TabOrder = 34
    OnClick = btnWSConnectClick
  end
  object btnWSSendFiles: TButton
    Left = 856
    Top = 98
    Width = 98
    Height = 30
    Caption = 'WS Send'
    Enabled = False
    TabOrder = 35
    OnClick = btnWSSendFilesClick
  end
  object btnWSListFiles: TButton
    Left = 856
    Top = 135
    Width = 98
    Height = 31
    Caption = #26597#35810#25991#20214
    Enabled = False
    TabOrder = 36
    OnClick = btnWSListFilesClick
  end
  object Button2: TButton
    Left = 856
    Top = 209
    Width = 98
    Height = 31
    Caption = 'Test'
    TabOrder = 37
    OnClick = Button2Click
  end
  object InIOCPServer1: TInIOCPServer
    HttpDataProvider = InHttpDataProvider1
    IOCPManagers.ClientManager = InClientManager1
    IOCPManagers.CustomManager = InCustomManager1
    IOCPManagers.DatabaseManager = InDatabaseManager1
    IOCPManagers.FileManager = InFileManager1
    IOCPManagers.MessageManager = InMessageManager1
    ServerAddr = 'localhost'
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    OnConnect = InIOCPServer1Connect
    OnDataReceive = InIOCPServer1DataReceive
    OnDataSend = InIOCPServer1DataSend
    OnDisconnect = InIOCPServer1Disconnect
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterOpen
    Left = 40
    Top = 240
  end
  object InDatabaseManager1: TInDatabaseManager
    Left = 368
    Top = 240
  end
  object InClientManager1: TInClientManager
    OnDelete = InClientManager1Delete
    OnModify = InClientManager1Modify
    OnLogin = InClientManager1Login
    OnLogout = InClientManager1Logout
    OnRegister = InClientManager1Register
    OnQueryState = InClientManager1QueryState
    Left = 136
    Top = 240
  end
  object InFileManager1: TInFileManager
    AfterDownload = InFileManager1AfterDownload
    AfterUpload = InFileManager1AfterUpload
    BeforeUpload = InFileManager1BeforeUpload
    BeforeDownload = InFileManager1BeforeDownload
    OnDeleteFile = InFileManager1DeleteFile
    OnQueryFiles = InFileManager1QueryFiles
    OnRenameFile = InFileManager1RenameFile
    OnSetWorkDir = InFileManager1SetWorkDir
    Left = 216
    Top = 240
  end
  object InConnection1: TInConnection
    AutoConnected = True
    LocalPath = 'temp\'
    ServerAddr = 'localhost'
    ServerPort = 0
    AfterConnect = InConnection1AfterConnect
    AfterDisconnect = InConnection1AfterConnect
    OnDataReceive = InConnection1DataReceive
    OnDataSend = InConnection1DataSend
    OnError = InConnection1Error
    ReuseSessionId = True
    OnReceiveMsg = InConnection1ReceiveMsg
    OnReturnResult = InConnection1ReturnResult
    Left = 40
    Top = 296
  end
  object InCertifyClient1: TInCertifyClient
    OnReturnResult = InCertifyClient1ReturnResult
    Connection = InConnection1
    Group = 'AAA'
    UserName = 'USER_A'
    Password = 'pppp'
    OnCertify = InCertifyClient1Certify
    OnListClients = InCertifyClient1ListClients
    Left = 136
    Top = 296
  end
  object InMessageClient1: TInMessageClient
    OnReturnResult = InMessageClient1ReturnResult
    Connection = InConnection1
    OnListFiles = InFileClient2ListFiles
    Left = 176
    Top = 297
  end
  object InConnection2: TInConnection
    AutoConnected = True
    LocalPath = 'temp\'
    ServerAddr = 'localhost'
    ServerPort = 0
    AfterConnect = InConnection2AfterConnect
    AfterDisconnect = InConnection2AfterConnect
    OnError = InConnection1Error
    OnReceiveMsg = InConnection2ReceiveMsg
    OnReturnResult = InConnection2ReturnResult
    Left = 40
    Top = 336
  end
  object InCertifyClient2: TInCertifyClient
    Connection = InConnection2
    Group = 'AAA'
    UserName = 'USER_B'
    Password = 'BBBB'
    OnCertify = InCertifyClient1Certify
    Left = 136
    Top = 336
  end
  object InMessageClient2: TInMessageClient
    OnReturnResult = InMessageClient2ReturnResult
    Connection = InConnection2
    Left = 176
    Top = 337
  end
  object InCustomClient1: TInCustomClient
    OnReturnResult = InCustomClient1ReturnResult
    Connection = InConnection1
    Left = 272
    Top = 296
  end
  object InEchoClient1: TInEchoClient
    OnReturnResult = InEchoClient1ReturnResult
    Connection = InConnection1
    Left = 136
    Top = 424
  end
  object InCustomManager1: TInCustomManager
    OnReceive = InCustomManager1Receive
    OnAttachBegin = InCustomManager1AttachBegin
    OnAttachFinish = InCustomManager1AttachFinish
    Left = 272
    Top = 240
  end
  object InFileClient1: TInFileClient
    OnReturnResult = InFileClient1ReturnResult
    Connection = InConnection1
    OnListFiles = InFileClient2ListFiles
    Left = 216
    Top = 296
  end
  object InFileClient2: TInFileClient
    OnReturnResult = InFileClient2ReturnResult
    Connection = InConnection2
    OnListFiles = InFileClient2ListFiles
    Left = 216
    Top = 336
  end
  object InFunctionClient1: TInFunctionClient
    OnReturnResult = InFunctionClient1ReturnResult
    Connection = InConnection2
    Left = 272
    Top = 336
  end
  object InRemoteFunctionGroup1: TInRemoteFunctionGroup
    CustomManager = InCustomManager1
    FunctionGroupName = 'TEST_GROUP'
    OnExecute = InRemoteFunctionGroup1Execute
    Left = 320
    Top = 240
  end
  object InDBConnection1: TInDBConnection
    OnReturnResult = InDBConnection1ReturnResult
    Connection = InConnection1
    ConnectionIndex = 0
    Left = 320
    Top = 296
  end
  object InDBQueryClient1: TInDBQueryClient
    OnReturnResult = InDBQueryClient1ReturnResult
    DBConnection = InDBConnection1
    AfterLoadData = InDBQueryClient1AfterLoadData
    ClientDataSet = ClientDataSet1
    Left = 408
    Top = 336
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 416
    Top = 432
  end
  object InHttpDataProvider1: TInHttpDataProvider
    OnAccept = InHttpDataProvider1Accept
    OnGet = InHttpDataProvider1Get
    OnPost = InHttpDataProvider1Post
    OnReceiveFile = InHttpDataProvider1ReceiveFile
    RootDirectory = 'web_site'
    WebSocketManager = InWebSocketManager1
    Left = 464
    Top = 240
  end
  object InMessageManager1: TInMessageManager
    OnBroadcast = InMessageManager1Broadcast
    OnGet = InMessageManager1Get
    OnListFiles = InMessageManager1ListFiles
    OnPush = InMessageManager1Push
    OnReceive = InMessageManager1Receive
    Left = 176
    Top = 240
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 208
    Top = 424
  end
  object InWebSocketManager1: TInWebSocketManager
    OnReceive = InWebSocketManager1Receive
    OnUpgrade = InWebSocketManager1Upgrade
    Left = 520
    Top = 240
  end
  object InWSConnection1: TInWSConnection
    ServerAddr = 'localhost'
    ServerPort = 80
    AfterConnect = InWSConnection1AfterConnect
    AfterDisconnect = InWSConnection1AfterConnect
    Masking = True
    URL = '/'
    OnReceiveData = InWSConnection1ReceiveData
    OnReceiveMsg = InWSConnection1ReceiveMsg
    OnReturnResult = InWSConnection1ReturnResult
    Left = 520
    Top = 296
  end
  object InDBSQLClient1: TInDBSQLClient
    DBConnection = InDBConnection1
    Left = 368
    Top = 336
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 368
    Top = 432
  end
end
