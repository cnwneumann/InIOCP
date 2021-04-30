object FormInIOCPStreamClient: TFormInIOCPStreamClient
  Left = 0
  Top = 0
  Caption = 'FormInIOCPStreamClient'
  ClientHeight = 267
  ClientWidth = 478
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object LabeledEdit1: TLabeledEdit
    Left = 366
    Top = 24
    Width = 91
    Height = 20
    EditLabel.Width = 48
    EditLabel.Height = 12
    EditLabel.Caption = #23458#25143#31471'Id'
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    TabOrder = 0
    Text = '11111'
  end
  object Button1: TButton
    Left = 366
    Top = 59
    Width = 75
    Height = 28
    Caption = 'Connect'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 9
    Top = 6
    Width = 336
    Height = 253
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      'InIOCP'#25968#25454#27969#26381#21153#23458#25143#31471
      ''
      #35831#36816#34892#20960#27425#26412#23458#25143#31471#65292#29992#19981#21516#30340#23458#25143#31471'Id'#36830#25509#21040#26381#21153#22120
      ''
      #23458#25143#31471#27599#25910#21040#26381#21153#31471#28040#24687#23601#21457#36865#28040#24687#21040#26381#21153#31471#65292#26684#24335#65306
      ''
      'ClientId:AAABBBCCCDDD'
      ''
      '')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Button2: TButton
    Left = 366
    Top = 99
    Width = 75
    Height = 28
    Caption = 'Send'
    TabOrder = 3
    OnClick = Button2Click
  end
end
