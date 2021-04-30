object FormInIOCPDLL: TFormInIOCPDLL
  Left = 0
  Top = 0
  Caption = 'DLL Form Caller'
  ClientHeight = 237
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 24
    Width = 137
    Height = 25
    Caption = 'Call DLL Form'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 216
    Top = 24
    Width = 137
    Height = 25
    Caption = 'Close DLL Form'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 24
    Top = 71
    Width = 369
    Height = 129
    ImeName = #35895#27468#25340#38899#36755#20837#27861' 2'
    Lines.Strings = (
      #27880#65306#23458#25143#31471#35201#32447#31243#21516#27493#65292'DLL '#24037#31243#20013#35201#26377#26377#25928#30340' Application.Handle'#65292
      #21487#20197#21442#32771#22914#19979#30340#21021#22987#21270' Handle '#30340#26041#27861#65306
      '  if Application.Handle = 0 then'
      '    Application.CreateHandle;')
    TabOrder = 2
  end
end
