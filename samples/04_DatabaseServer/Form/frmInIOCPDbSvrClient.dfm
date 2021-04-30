object FormInIOCPDbSvrClient: TFormInIOCPDbSvrClient
  Left = 320
  Top = 284
  Caption = 'InIOCP '#25968#25454#24211#26381#21153#23458#25143#31471
  ClientHeight = 449
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 13
  object Image1: TImage
    Left = 481
    Top = 8
    Width = 160
    Height = 202
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 341
    Height = 202
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #26381#21153#31471#26377#22810#31181#25968#27169#26102#65292#35201#29992' TInDBConnection '#32452#20214#26597#35810
      #20449#24687#12289#35774#32622#24403#21069#36830#25509#65292#21542#21017#40664#35748#20026#31532#19968#20010#12290)
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnLogin: TButton
    Left = 370
    Top = 156
    Width = 75
    Height = 26
    Caption = #30331#24405
    TabOrder = 1
    OnClick = btnLoginClick
  end
  object edtLoginUser: TEdit
    Left = 370
    Top = 130
    Width = 75
    Height = 21
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 2
    Text = 'USER_AAA'
  end
  object btnConnect: TButton
    Left = 370
    Top = 66
    Width = 75
    Height = 25
    Caption = #36830#25509
    TabOrder = 3
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 370
    Top = 94
    Width = 75
    Height = 26
    Caption = #26029#24320
    TabOrder = 4
    OnClick = btnDisconnectClick
  end
  object btnLogout: TButton
    Left = 370
    Top = 185
    Width = 75
    Height = 25
    Caption = #30331#20986
    TabOrder = 5
    OnClick = btnLogoutClick
  end
  object btnDBUpdate: TButton
    Left = 566
    Top = 290
    Width = 75
    Height = 25
    Caption = #26356#26032
    TabOrder = 6
    OnClick = btnDBUpdateClick
  end
  object btnDBUpdate2: TButton
    Left = 566
    Top = 330
    Width = 75
    Height = 25
    Caption = 'UPDATE'
    TabOrder = 7
    OnClick = btnDBUpdate2Click
  end
  object btnDBQuery: TButton
    Left = 566
    Top = 259
    Width = 75
    Height = 25
    Caption = #26597#35810
    TabOrder = 8
    OnClick = btnDBQueryClick
  end
  object btnQueryDBConnections: TButton
    Left = 183
    Top = 223
    Width = 75
    Height = 26
    Caption = #26597#35810#36830#25509
    TabOrder = 9
    OnClick = btnQueryDBConnectionsClick
  end
  object btnSetDBConnection: TButton
    Left = 274
    Top = 223
    Width = 75
    Height = 26
    Caption = #35774#32622#36830#25509
    Enabled = False
    TabOrder = 10
    OnClick = btnSetDBConnectionClick
  end
  object DBGrid1: TDBGrid
    Left = 14
    Top = 259
    Width = 537
    Height = 175
    DataSource = DataSource1
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 11
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ComboBox1: TComboBox
    Left = 14
    Top = 225
    Width = 163
    Height = 21
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    ItemHeight = 13
    TabOrder = 12
    OnChange = ComboBox1Change
  end
  object btnStoredProc: TButton
    Left = 566
    Top = 367
    Width = 75
    Height = 25
    Caption = #23384#20648#36807#31243
    TabOrder = 13
    OnClick = btnStoredProcClick
  end
  object btnStoredProc2: TButton
    Left = 566
    Top = 398
    Width = 75
    Height = 25
    Caption = #23384#20648#36807#31243'2'
    TabOrder = 14
    OnClick = btnStoredProc2Click
  end
  object edtIP: TEdit
    Left = 370
    Top = 8
    Width = 87
    Height = 21
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 15
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 370
    Top = 35
    Width = 75
    Height = 21
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 16
    Text = '12302'
  end
  object InConnection1: TInConnection
    LocalPath = 'temp\'
    ServerAddr = '127.0.0.1'
    OnError = InConnection1Error
    Left = 64
    Top = 56
  end
  object InCertifyClient1: TInCertifyClient
    Connection = InConnection1
    OnCertify = InCertifyClient1Certify
    Left = 144
    Top = 56
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 216
    Top = 120
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    AfterScroll = ClientDataSet1AfterScroll
    Left = 144
    Top = 120
  end
  object InDBConnection1: TInDBConnection
    OnReturnResult = InDBConnection1ReturnResult
    Connection = InConnection1
    ConnectionIndex = 0
    Left = 216
    Top = 56
  end
  object InDBQueryClient1: TInDBQueryClient
    OnReturnResult = InDBQueryClient1ReturnResult
    DBConnection = InDBConnection1
    AfterLoadData = InDBQueryClient1AfterLoadData
    ClientDataSet = ClientDataSet1
    Left = 64
    Top = 120
  end
  object InDBSQLClient1: TInDBSQLClient
    OnReturnResult = InDBSQLClient1ReturnResult
    DBConnection = InDBConnection1
    Left = 264
    Top = 120
  end
end
