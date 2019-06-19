object FormInIOCPWsDBClient: TFormInIOCPWsDBClient
  Left = 0
  Top = 0
  Caption = 'InIOCP WebSocket '#26381#21153'-'#25968#25454#24211#25805#20316#23458#25143#31471
  ClientHeight = 428
  ClientWidth = 598
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
  PixelsPerInch = 96
  TextHeight = 12
  object Image1: TImage
    Left = 460
    Top = 259
    Width = 128
    Height = 161
  end
  object btnConnect: TButton
    Left = 486
    Top = 14
    Width = 83
    Height = 26
    Caption = #36830#25509
    TabOrder = 0
    OnClick = btnConnectClick
  end
  object btnSend: TButton
    Left = 486
    Top = 46
    Width = 83
    Height = 26
    Caption = #21457#36865#25991#20214
    Enabled = False
    TabOrder = 1
    OnClick = btnSendClick
  end
  object btnDBQuery: TButton
    Left = 486
    Top = 161
    Width = 83
    Height = 26
    Caption = #26597#35810
    Enabled = False
    TabOrder = 2
    OnClick = btnDBQueryClick
  end
  object Memo1: TMemo
    Left = 6
    Top = 6
    Width = 443
    Height = 116
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object DBGrid1: TDBGrid
    Left = 6
    Top = 127
    Width = 443
    Height = 293
    DataSource = DataSource1
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = #23435#20307
    TitleFont.Style = []
  end
  object btnDBUpdate: TButton
    Left = 486
    Top = 192
    Width = 83
    Height = 26
    Caption = #26356#26032
    Enabled = False
    TabOrder = 5
    OnClick = btnDBUpdateClick
  end
  object btnListFiles: TButton
    Left = 486
    Top = 78
    Width = 83
    Height = 26
    Caption = #26597#35810#25991#20214
    Enabled = False
    TabOrder = 6
    OnClick = btnListFilesClick
  end
  object Button3: TButton
    Left = 486
    Top = 116
    Width = 83
    Height = 26
    Caption = #28165#31354
    TabOrder = 7
    OnClick = Button3Click
  end
  object InWSConnection1: TInWSConnection
    ServerAddr = 'localhost'
    ServerPort = 80
    AfterConnect = InWSConnection1AfterConnect
    AfterDisconnect = InWSConnection1AfterConnect
    OnReceiveData = InWSConnection1ReceiveData
    OnReceiveMsg = InWSConnection1ReceiveMsg
    OnReturnResult = InWSConnection1ReturnResult
    Left = 368
    Top = 56
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    AfterScroll = ClientDataSet1AfterScroll
    Left = 368
    Top = 144
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 368
    Top = 192
  end
end
