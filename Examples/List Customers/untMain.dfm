object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 306
  ClientWidth = 244
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbCustomers: TListBox
    Left = 0
    Top = 25
    Width = 244
    Height = 281
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    ExplicitTop = 46
    ExplicitHeight = 260
  end
  object btnGetCustomers: TButton
    Left = 0
    Top = 0
    Width = 244
    Height = 25
    Align = alTop
    Caption = 'List Customers'
    TabOrder = 1
    OnClick = btnGetCustomersClick
  end
end
