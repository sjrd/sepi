object SdAboutForm: TSdAboutForm
  Left = 314
  Top = 217
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = #192' propos'
  ClientHeight = 201
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ImageProgramIcon: TImage
    Left = 16
    Top = 16
    Width = 65
    Height = 65
    Center = True
  end
  object LabelProgramName: TLabel
    Left = 96
    Top = 24
    Width = 92
    Height = 13
    Caption = 'Nom du programme'
  end
  object LabelProgramVersion: TLabel
    Left = 96
    Top = 56
    Width = 105
    Height = 13
    Caption = 'Version du programme'
  end
  object LabelAuthor: TLabel
    Left = 16
    Top = 88
    Width = 101
    Height = 13
    Caption = 'Auteur du programme'
  end
  object ButtonOK: TButton
    Left = 104
    Top = 160
    Width = 89
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
end
