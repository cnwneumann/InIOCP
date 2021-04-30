object FormInIOCPAdmin: TFormInIOCPAdmin
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'InIOCP-'#31649#29702#21592
  ClientHeight = 424
  ClientWidth = 879
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
  object btnLogin: TButton
    Left = 31
    Top = 380
    Width = 75
    Height = 28
    Action = actLogin
    TabOrder = 0
  end
  object btnBroacast: TButton
    Left = 463
    Top = 380
    Width = 75
    Height = 28
    Action = actBroadcast
    TabOrder = 5
  end
  object btnCapScreen: TButton
    Left = 548
    Top = 380
    Width = 75
    Height = 28
    Action = actTransmitFile
    TabOrder = 6
  end
  object btnDisconnect: TButton
    Left = 635
    Top = 380
    Width = 75
    Height = 28
    Action = actDisconnect
    TabOrder = 7
  end
  object btnClose: TButton
    Left = 113
    Top = 380
    Width = 75
    Height = 28
    Action = actClose
    TabOrder = 1
  end
  object btnRegister: TButton
    Left = 206
    Top = 380
    Width = 75
    Height = 28
    Action = actRegister
    TabOrder = 2
  end
  object btnModify: TButton
    Left = 293
    Top = 380
    Width = 75
    Height = 28
    Action = actModify
    Caption = #21024#25913
    TabOrder = 3
  end
  object btnSendMsg: TButton
    Left = 378
    Top = 380
    Width = 75
    Height = 28
    Action = actSendMsg
    TabOrder = 4
  end
  object pgcInfo: TPageControl
    Left = 5
    Top = 5
    Width = 869
    Height = 360
    ActivePage = TabSheet1
    MultiLine = True
    TabOrder = 8
    TabPosition = tpLeft
    object TabSheet1: TTabSheet
      Caption = #23458#25143#31471
      object lvClientView: TListView
        Left = 0
        Top = 0
        Width = 839
        Height = 352
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = #32534#21495
            Width = 63
          end
          item
            Caption = #23458#25143#31471
            Width = 113
          end
          item
            Caption = 'IP'
            Width = 150
          end
          item
            Caption = #26435#38480
            Width = 75
          end
          item
            Caption = #30331#24405#26102#38388
            Width = 188
          end
          item
            Caption = #30331#20986#26102#38388
            Width = 188
          end>
        Ctl3D = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TabSheet2: TTabSheet
      Caption = #23458#25143#27963#21160
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 839
        Height = 353
        Align = alClient
        Ctl3D = False
        ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
        ParentCtl3D = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object Button1: TButton
    Left = 739
    Top = 380
    Width = 75
    Height = 28
    Action = actBackground
    TabOrder = 9
  end
  object InCertifyClient1: TInCertifyClient
    OnReturnResult = InCertifyClient1ReturnResult
    Connection = InConnection1
    OnCertify = InCertifyClient1Certify
    OnListClients = InCertifyClient1ListClients
    Left = 40
    Top = 120
  end
  object InConnection1: TInConnection
    OnReturnResult = InConnection1ReturnResult
    AutoConnected = True
    ReuseSessionId = True
    ServerAddr = '127.0.0.1'
    ServerPort = 80
    OnReceiveMsg = InConnection1ReceiveMsg
    Left = 40
    Top = 64
  end
  object InMessageClient1: TInMessageClient
    Connection = InConnection1
    Left = 96
    Top = 120
  end
  object ActionList1: TActionList
    Left = 176
    Top = 64
    object actLogin: TAction
      Caption = #30331#24405
      OnExecute = actLoginExecute
    end
    object actLogout: TAction
      Caption = #30331#20986
      OnExecute = actLogoutExecute
    end
    object actClose: TAction
      Caption = #20851#38381
      OnExecute = actCloseExecute
      OnUpdate = actCloseUpdate
    end
    object actRegister: TAction
      Caption = #27880#20876
      Enabled = False
      OnExecute = actRegisterExecute
    end
    object actModify: TAction
      Caption = #20462#25913
      Enabled = False
      OnExecute = actModifyExecute
    end
    object actSendMsg: TAction
      Caption = #21457#36865
      Enabled = False
      OnExecute = actSendMsgExecute
    end
    object actBroadcast: TAction
      Caption = #24191#25773
      Enabled = False
      OnExecute = actBroadcastExecute
    end
    object actTransmitFile: TAction
      Caption = #20256#25991#20214
      Enabled = False
      OnExecute = actTransmitFileExecute
    end
    object actDisconnect: TAction
      Caption = #26029#24320
      Enabled = False
      OnExecute = actDisconnectExecute
    end
    object actBackground: TAction
      Caption = #21518#21488#25191#34892
      OnExecute = actBackgroundExecute
    end
  end
end
