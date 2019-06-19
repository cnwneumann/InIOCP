object FormInIOCPFileSvrClient: TFormInIOCPFileSvrClient
  Left = 355
  Top = 104
  Caption = 'InIOCP '#25991#20214#26381#21153#23458#25143#31471
  ClientHeight = 495
  ClientWidth = 709
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
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 497
    Height = 169
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #20363#23376#28436#31034#19978#20256#12289#19979#36733#65292#26029#28857#32493#20256#65292#21462#28040#12289#37325#21551#31561#25991#20214#25805#20316#12290
      ''
      #21487#33021#23384#22312#38382#39064#30340#21151#33021#65306#22823#25991#20214#32493#20256#26102#26242#20572#12289#37325#21551#65307
      #26410#23454#29616#25991#20214#25512#36865#65288#25512#36865#28040#24687#20027#20307#65289)
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnLogin: TButton
    Left = 518
    Top = 119
    Width = 75
    Height = 25
    Caption = #30331#24405
    TabOrder = 1
    OnClick = btnLoginClick
  end
  object edtLoginUser: TEdit
    Left = 518
    Top = 90
    Width = 75
    Height = 21
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 2
    Text = 'USER_A'
  end
  object btnConnect: TButton
    Left = 518
    Top = 8
    Width = 75
    Height = 25
    Caption = #36830#25509
    TabOrder = 3
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 518
    Top = 36
    Width = 75
    Height = 25
    Caption = #26029#24320
    TabOrder = 4
    OnClick = btnDisconnectClick
  end
  object btnLogout: TButton
    Left = 518
    Top = 147
    Width = 75
    Height = 25
    Caption = #30331#20986
    TabOrder = 5
    OnClick = btnLogoutClick
  end
  object btnUpload: TButton
    Left = 518
    Top = 292
    Width = 75
    Height = 25
    Caption = #19978#20256
    TabOrder = 6
    OnClick = btnUploadClick
  end
  object btnDownload: TButton
    Left = 518
    Top = 326
    Width = 75
    Height = 25
    Caption = #19979#36733
    TabOrder = 7
    OnClick = btnDownloadClick
  end
  object btnQueryFiles: TButton
    Left = 518
    Top = 259
    Width = 75
    Height = 25
    Caption = #26597#35810
    TabOrder = 8
    OnClick = btnQueryFilesClick
  end
  object btnSetDir: TButton
    Left = 518
    Top = 226
    Width = 75
    Height = 25
    Caption = 'cd sub'
    TabOrder = 9
    OnClick = btnSetDirClick
  end
  object ListView1: TListView
    Left = 8
    Top = 214
    Width = 497
    Height = 163
    Columns = <
      item
        Caption = #32534#21495
        Width = 40
      end
      item
        Caption = #20869#23481
        Width = 150
      end
      item
        Caption = 'MsgId'
        Width = 120
      end
      item
        Caption = #25805#20316
        Width = 80
      end
      item
        Caption = #32467#26524
        Width = 80
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 10
    ViewStyle = vsReport
    OnClick = ListView1Click
  end
  object BtnCancel: TButton
    Left = 201
    Top = 393
    Width = 121
    Height = 25
    Caption = #21462#28040'/'#26242#20572#25805#20316
    Enabled = False
    TabOrder = 11
    OnClick = BtnCancelClick
  end
  object btnRestart: TButton
    Left = 345
    Top = 393
    Width = 121
    Height = 25
    Caption = #22823#25991#20214#32493#20256
    Enabled = False
    TabOrder = 12
    OnClick = btnRestartClick
  end
  object btnClearList: TButton
    Left = 8
    Top = 393
    Width = 75
    Height = 25
    Caption = #28165#38500
    TabOrder = 13
    OnClick = btnClearListClick
  end
  object btnUpChunk: TButton
    Left = 614
    Top = 292
    Width = 75
    Height = 25
    Caption = #26029#28857#19978#20256
    TabOrder = 14
    OnClick = btnUpChunkClick
  end
  object btnDownChunk: TButton
    Left = 614
    Top = 326
    Width = 75
    Height = 25
    Caption = #26029#28857#19979#36733
    TabOrder = 15
    OnClick = btnDownChunkClick
  end
  object Button1: TButton
    Left = 518
    Top = 369
    Width = 75
    Height = 25
    Caption = 'MsgFile'
    TabOrder = 16
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 614
    Top = 259
    Width = 75
    Height = 25
    Caption = #26597#35810#19979#36733
    TabOrder = 17
    OnClick = Button2Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 190
    Width = 497
    Height = 18
    Step = 1
    TabOrder = 18
  end
  object Button3: TButton
    Left = 435
    Top = 457
    Width = 75
    Height = 25
    Caption = #27604#36739'MD5'
    TabOrder = 19
    OnClick = Button3Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 431
    Width = 417
    Height = 21
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 20
    Text = 'temp\jdk-8u77-windows-i586.exe'
  end
  object Edit2: TEdit
    Left = 8
    Top = 458
    Width = 417
    Height = 21
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 21
    Text = 'F:\Backup\jdk-8u77-windows-i586.exe'
  end
  object InConnection1: TInConnection
    LocalPath = 'temp\'
    ServerAddr = '127.0.0.1'
    OnAddWork = InConnection1AddWork
    OnDataReceive = InConnection1DataReceive
    OnDataSend = InConnection1DataSend
    OnError = InConnection1Error
    Left = 104
    Top = 104
  end
  object InCertifyClient1: TInCertifyClient
    Connection = InConnection1
    OnCertify = InCertifyClient1Certify
    Left = 176
    Top = 104
  end
  object InFileClient1: TInFileClient
    OnReturnResult = InFileClient1ReturnResult
    Connection = InConnection1
    OnListFiles = InFileClient1ListFiles
    Left = 248
    Top = 104
  end
end
