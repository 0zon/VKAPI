object Form1: TForm1
  Left = 256
  Top = 124
  Width = 490
  Height = 203
  Caption = 'demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 17
    AutoSize = False
    Caption = 'App ID'
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 41
    Height = 17
    AutoSize = False
    Caption = 'Login'
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 41
    Height = 17
    AutoSize = False
    Caption = 'Pass'
    Layout = tlCenter
  end
  object Label4: TLabel
    Left = 8
    Top = 112
    Width = 161
    Height = 49
    AutoSize = False
    WordWrap = True
  end
  object Edit1: TEdit
    Left = 48
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '2232793'
  end
  object Edit2: TEdit
    Left = 48
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 80
    Width = 73
    Height = 25
    Caption = 'Login'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Edit3: TEdit
    Left = 48
    Top = 56
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 176
    Top = 40
    Width = 297
    Height = 121
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object Button2: TButton
    Left = 176
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Get friends'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 256
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Get online friends'
    TabOrder = 6
    WordWrap = True
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 336
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Get mutual friends'
    TabOrder = 7
    WordWrap = True
    OnClick = Button4Click
  end
  object Edit4: TEdit
    Left = 416
    Top = 8
    Width = 57
    Height = 21
    TabOrder = 8
    Text = '1'
  end
end
