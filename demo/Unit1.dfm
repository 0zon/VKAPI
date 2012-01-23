object Form1: TForm1
  Left = 221
  Top = 122
  Width = 729
  Height = 363
  Caption = 'Test VK API'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    713
    325)
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
    Height = 129
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
    Left = 120
    Top = 80
    Width = 49
    Height = 25
    Caption = 'Login'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Edit3: TEdit
    Left = 48
    Top = 56
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 176
    Top = 72
    Width = 537
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 7
  end
  object RadioButton1: TRadioButton
    Left = 8
    Top = 80
    Width = 113
    Height = 17
    Caption = 'OAuth (no browser)'
    TabOrder = 3
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 8
    Top = 96
    Width = 105
    Height = 17
    Caption = 'test mode'
    TabOrder = 4
    OnClick = RadioButton1Click
  end
  object PageControl1: TPageControl
    Left = 176
    Top = 0
    Width = 537
    Height = 65
    ActivePage = TabSheet1
    TabOrder = 6
    object TabSheet1: TTabSheet
      Caption = 'Friends'
      object Edit4: TEdit
        Left = 224
        Top = 8
        Width = 57
        Height = 21
        TabOrder = 0
        Text = '%UserID%'
      end
      object Button2: TButton
        Left = 0
        Top = 0
        Width = 73
        Height = 33
        Caption = 'Get friends'
        TabOrder = 1
        WordWrap = True
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 72
        Top = 0
        Width = 73
        Height = 33
        Caption = 'Get online friends'
        TabOrder = 2
        WordWrap = True
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 144
        Top = 0
        Width = 73
        Height = 33
        Caption = 'Get mutual friends'
        TabOrder = 3
        WordWrap = True
        OnClick = Button4Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Audio'
      ImageIndex = 1
      object Button5: TButton
        Left = 0
        Top = 0
        Width = 73
        Height = 33
        Caption = 'Search audio'
        TabOrder = 0
        WordWrap = True
        OnClick = Button5Click
      end
      object Edit5: TEdit
        Left = 80
        Top = 8
        Width = 57
        Height = 21
        TabOrder = 1
        Text = 'bi-2'
      end
      object Button7: TButton
        Left = 144
        Top = 0
        Width = 73
        Height = 33
        Caption = 'Get audio by user id'
        TabOrder = 2
        WordWrap = True
        OnClick = Button7Click
      end
      object Edit6: TEdit
        Left = 224
        Top = 8
        Width = 57
        Height = 21
        TabOrder = 3
        Text = '%UserID%'
      end
      object Button6: TButton
        Left = 288
        Top = 0
        Width = 73
        Height = 33
        Caption = 'Add audio to play list'
        TabOrder = 4
        WordWrap = True
        OnClick = Button6Click
      end
      object Edit7: TEdit
        Left = 368
        Top = -1
        Width = 57
        Height = 21
        TabOrder = 5
        Text = '%AudioID%'
      end
      object Edit8: TEdit
        Left = 368
        Top = 18
        Width = 57
        Height = 21
        TabOrder = 6
        Text = '%OwnerID%'
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Wall'
      ImageIndex = 2
      object Button8: TButton
        Left = 0
        Top = 0
        Width = 73
        Height = 33
        Caption = 'Post'
        TabOrder = 0
        WordWrap = True
        OnClick = Button8Click
      end
      object Edit10: TEdit
        Left = 208
        Top = -1
        Width = 57
        Height = 21
        TabOrder = 1
        Text = '%AudioID%'
      end
      object Edit9: TEdit
        Left = 208
        Top = 18
        Width = 57
        Height = 21
        TabOrder = 2
        Text = '%OwnerID%'
      end
      object Edit11: TEdit
        Left = 80
        Top = -1
        Width = 57
        Height = 21
        TabOrder = 3
        Text = '%UserID%'
      end
      object Edit12: TEdit
        Left = 80
        Top = 18
        Width = 57
        Height = 21
        TabOrder = 4
        Text = '%Msg%'
      end
      object Edit13: TEdit
        Left = 144
        Top = -1
        Width = 57
        Height = 21
        TabOrder = 5
        Text = 'audio'
      end
    end
  end
end
