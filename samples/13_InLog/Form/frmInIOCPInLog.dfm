object FormInIOCPInLog: TFormInIOCPInLog
  Left = 0
  Top = 0
  Caption = 'InIOCP -- '#24555#36895#26085#24535
  ClientHeight = 317
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 120
  TextHeight = 19
  object Label1: TLabel
    Left = 15
    Top = 16
    Width = 258
    Height = 19
    Caption = #29992#21333#32447#31243#24490#29615#20889#26085#24535#30334#19975#27425'...'
  end
  object Label2: TLabel
    Left = 15
    Top = 62
    Width = 240
    Height = 19
    Caption = #29992'10'#20010#32447#31243#20889#26085#24535#30334#19975#27425'...'
  end
  object btnTest: TButton
    Left = 322
    Top = 11
    Width = 95
    Height = 32
    Caption = #27979#35797'A'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object btnTest2: TButton
    Left = 322
    Top = 57
    Width = 95
    Height = 32
    Caption = #27979#35797'B'
    TabOrder = 1
    OnClick = btnTest2Click
  end
  object Memo1: TMemo
    Left = 15
    Top = 104
    Width = 418
    Height = 205
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
