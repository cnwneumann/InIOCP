object FormDblServer: TFormDblServer
  Left = 0
  Top = 0
  Caption = 'InIOCP - '#21452#26381#21153#22120
  ClientHeight = 247
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 14
  object btn1: TButton
    Left = 31
    Top = 20
    Width = 95
    Height = 32
    Caption = #21551#21160'A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btn1Click
  end
  object Button2: TButton
    Left = 316
    Top = 20
    Width = 95
    Height = 32
    Caption = #21551#21160'B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 11
    Top = 81
    Width = 602
    Height = 144
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -22
    Font.Name = #40657#20307
    Font.Style = []
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #26032#29256#25903#25345#21516#19968#20010#24212#29992#31243#24207#20013#24320#21551#22810#20010' InIOCP '#26381#21153#65292
      #35831#24320#21551#20004#20010#26381#21153#65292#21516#26102#29992' ab.exe '#27979#35797#31283#23450#24615#65306
      ''
      'ab -n 100000 -c 1000 -k -r http://127.0.0.1:850/'
      'ab -n 100000 -c 1000 -k -r http://127.0.0.1:860/')
    ParentFont = False
    TabOrder = 2
  end
  object InIOCPServer1: TInIOCPServer
    HttpDataProvider = InHttpDataProvider1
    ServerPort = 850
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    AfterOpen = InIOCPServer1AfterOpen
    AfterClose = InIOCPServer1AfterOpen
    Left = 120
    Top = 16
  end
  object InIOCPServer2: TInIOCPServer
    HttpDataProvider = InHttpDataProvider2
    ServerPort = 860
    ThreadOptions.BusinessThreadCount = 8
    ThreadOptions.PushThreadCount = 4
    ThreadOptions.WorkThreadCount = 4
    AfterOpen = InIOCPServer2AfterOpen
    AfterClose = InIOCPServer2AfterOpen
    Left = 344
    Top = 16
  end
  object InHttpDataProvider1: TInHttpDataProvider
    OnGet = InHttpDataProvider1Get
    Left = 160
    Top = 16
  end
  object InHttpDataProvider2: TInHttpDataProvider
    OnGet = InHttpDataProvider1Get
    Left = 384
    Top = 16
  end
end
