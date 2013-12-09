object Form1: TForm1
  Left = 239
  Top = 139
  Width = 943
  Height = 690
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object LblTimeInfo: TLabel
    Left = 16
    Top = 112
    Width = 83
    Height = 13
    Caption = 'Time values in [s]'
  end
  object BtnXmlDirectWrite: TButton
    Left = 16
    Top = 48
    Width = 193
    Height = 25
    Caption = 'Xml Direct Write - simple.xml'
    TabOrder = 4
    OnClick = BtnXmlDirectWriteClick
  end
  object BtnResaveWithDOM: TButton
    Left = 224
    Top = 16
    Width = 177
    Height = 25
    Caption = 'Read/Write Performance Test'
    TabOrder = 1
    OnClick = BtnResaveWithDOMClick
  end
  object BtnTestXPath: TButton
    Left = 16
    Top = 16
    Width = 193
    Height = 25
    Caption = 'XPath Test'
    TabOrder = 0
    OnClick = BtnTestXPathClick
  end
  object Memo1: TMemo
    Left = 56
    Top = 128
    Width = 289
    Height = 521
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 11
  end
  object Memo2: TMemo
    Left = 352
    Top = 128
    Width = 289
    Height = 521
    Lines.Strings = (
      'Memo2')
    ScrollBars = ssBoth
    TabOrder = 12
  end
  object BtnTestSAX: TButton
    Left = 16
    Top = 80
    Width = 193
    Height = 25
    Caption = 'Test SAX Read'
    TabOrder = 8
    OnClick = BtnTestSAXClick
  end
  object BtnInterfaceCreate: TButton
    Left = 224
    Top = 48
    Width = 177
    Height = 25
    Caption = 'Create DOM Performance Test'
    TabOrder = 5
    OnClick = BtnInterfaceCreateClick
  end
  object BtnTestWriteInvalid: TButton
    Left = 576
    Top = 16
    Width = 177
    Height = 25
    Caption = 'Test write invalid document'
    TabOrder = 3
    OnClick = BtnTestWriteInvalidClick
  end
  object BtnEncodingTest: TButton
    Left = 224
    Top = 80
    Width = 177
    Height = 25
    Caption = 'Custom Encoding Test'
    TabOrder = 9
    OnClick = BtnEncodingTestClick
  end
  object BtnIterateTest: TButton
    Left = 416
    Top = 16
    Width = 145
    Height = 25
    Caption = 'Iterate Test'
    TabOrder = 2
    OnClick = BtnIterateTestClick
  end
  object BtnSequentialTest: TButton
    Left = 416
    Top = 48
    Width = 145
    Height = 25
    Caption = 'Sequential Test'
    TabOrder = 6
    OnClick = BtnSequentialTestClick
  end
  object BtnTestReadInvalid: TButton
    Left = 576
    Top = 48
    Width = 177
    Height = 25
    Caption = 'Test read invalid document'
    TabOrder = 7
    OnClick = BtnTestReadInvalidClick
  end
  object BtnDOMTest: TButton
    Left = 416
    Top = 80
    Width = 145
    Height = 25
    Caption = 'DOM Level 1.0 Test'
    TabOrder = 10
    OnClick = BtnDOMTestClick
  end
end
