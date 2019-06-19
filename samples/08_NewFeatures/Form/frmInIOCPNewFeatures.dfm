object FormInIOCPNewFeatures: TFormInIOCPNewFeatures
  Left = 0
  Top = 0
  Caption = 'InIOCP '#26032#29305#24615#24212#29992
  ClientHeight = 506
  ClientWidth = 708
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 14
  object Memo1: TMemo
    Left = 23
    Top = 10
    Width = 541
    Height = 175
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      '2.0 '#21462#28040#25209#20219#21153#65292#29992' TMessagePack '#23454#29616#21508#31181#25805#20316#65292#35831#36816#34892#20363#23376' All-In-One'#65292#24320#21551#26381#21153#12290
      ''
      'TMessagePack '#30340#23487#20027#21487#20197#26159' TInConnection '#25110#20854#20182#23458#25143#31471#32452#20214#65292
      #36820#22238#30340#32467#26524#37117#22312#23487#20027#30340' OnReturnResult '#20013#22788#29702#65292#22240#19981#20844#24320
      'TInConnection '#29992#25143#26597#35810#12289#25991#20214#26597#35810#36820#22238#20107#20214#65292#24314#35758#23613#37327#20351#29992
      #19982#35831#27714#25805#20316#65288'Action'#65289#30456#24212#30340#23458#25143#31471#32452#20214#20316#23487#20027#12290
      ''
      'TMessagePack '#34987#25552#20132#21518#65292#20250#34987#21152#20837#21457#36865#32447#31243#65292#21457#36865#23436#27605#33258#21160#37322#25918#65281)
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 118
    Top = 197
    Width = 75
    Height = 25
    Caption = #28040#24687#26381#21153
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 581
    Top = 39
    Width = 75
    Height = 25
    Caption = #36830#25509
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 581
    Top = 126
    Width = 75
    Height = 25
    Caption = #30331#24405
    TabOrder = 3
    OnClick = Button3Click
  end
  object btnDisconnect: TButton
    Left = 581
    Top = 67
    Width = 75
    Height = 25
    Caption = #26029#24320
    TabOrder = 4
    OnClick = btnDisconnectClick
  end
  object btnLogout: TButton
    Left = 581
    Top = 154
    Width = 75
    Height = 25
    Caption = #30331#20986
    TabOrder = 5
    OnClick = btnLogoutClick
  end
  object Button4: TButton
    Left = 214
    Top = 197
    Width = 75
    Height = 25
    Caption = #25991#20214#26381#21153
    TabOrder = 6
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 581
    Top = 103
    Width = 75
    Height = 22
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 7
    Text = 'USER_A'
  end
  object Button6: TButton
    Left = 309
    Top = 197
    Width = 75
    Height = 25
    Caption = #25968#25454#24211#26381#21153
    TabOrder = 8
    OnClick = Button6Click
  end
  object DBGrid1: TDBGrid
    Left = 13
    Top = 235
    Width = 551
    Height = 256
    DataSource = DataSource1
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 9
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button8: TButton
    Left = 405
    Top = 197
    Width = 75
    Height = 25
    Caption = #33258#23450#20041#26381#21153
    TabOrder = 10
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 23
    Top = 197
    Width = 75
    Height = 25
    Caption = #35748#35777#26381#21153
    TabOrder = 11
    OnClick = Button9Click
  end
  object Button5: TButton
    Left = 581
    Top = 266
    Width = 75
    Height = 25
    Caption = #26356#26032
    TabOrder = 12
    OnClick = Button5Click
  end
  object EditServer: TEdit
    Left = 581
    Top = 10
    Width = 98
    Height = 22
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 13
    Text = '192.168.1.196'
    OnDblClick = EditServerDblClick
  end
  object Button7: TButton
    Left = 581
    Top = 402
    Width = 75
    Height = 25
    Caption = #28165#38500
    TabOrder = 14
    OnClick = Button7Click
  end
  object InConnection1: TInConnection
    LocalPath = 'temp'
    ServerAddr = '127.0.0.1'
    ServerPort = 800
    OnReceiveMsg = InConnection1ReceiveMsg
    OnError = InConnection1Error
    Left = 112
    Top = 272
  end
  object InCertifyClient1: TInCertifyClient
    OnReturnResult = InCertifyClient1ReturnResult
    Connection = InConnection1
    OnCertify = InCertifyClient1Certify
    OnListClients = InCertifyClient1ListClients
    Left = 184
    Top = 272
  end
  object InMessageClient1: TInMessageClient
    OnReturnResult = InMessageClient1ReturnResult
    Connection = InConnection1
    OnListFiles = InMessageClient1ListFiles
    Left = 240
    Top = 272
  end
  object InCustomClient1: TInCustomClient
    OnReturnResult = InCustomClient1ReturnResult
    Connection = InConnection1
    Left = 360
    Top = 272
  end
  object InFileClient1: TInFileClient
    OnReturnResult = InFileClient1ReturnResult
    Connection = InConnection1
    OnListFiles = InFileClient1ListFiles
    Left = 297
    Top = 272
  end
  object InDBSQLClient1: TInDBSQLClient
    OnReturnResult = InDBSQLClient1ReturnResult
    DBConnection = InDBConnection1
    Left = 440
    Top = 288
  end
  object InDBQueryClient1: TInDBQueryClient
    OnReturnResult = InDBSQLClient1ReturnResult
    DBConnection = InDBConnection1
    ClientDataSet = ClientDataSet1
    Left = 496
    Top = 288
  end
  object InFunctionClient1: TInFunctionClient
    OnReturnResult = InFunctionClient1ReturnResult
    Connection = InConnection1
    Left = 360
    Top = 328
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 440
    Top = 328
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 496
    Top = 328
  end
  object InDBConnection1: TInDBConnection
    Connection = InConnection1
    ConnectionIndex = 0
    Left = 440
    Top = 248
  end
end
