object SdPasswordForm: TSdPasswordForm
  Left = 347
  Top = 247
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Mot de passe'
  ClientHeight = 89
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001000800680500001600000028000000100000002000
    000001000800000000000000000000000000000000000000000000000000BCBA
    BE0000000000B2AFB000ABA0A200AEA9A900504E4E0046454500C6C5C500AAA9
    A9009B858300AB8B8600A0837E00B1A4A200E2DCDA007F797600E1DFDE00D2D0
    CF00A09B9700B6ABA100C1BBB500B0ADAA00A6A4A2009B99970096949200C9C7
    C500BCBAB800B5B4B300CAB09500AE9C8800AFAAA400A3A9A5009BE1E6008CF5
    FF00B1F5FF0069A5B00080E9FF0089E2F500B7F1FE005CA6BB0077C1D40072D8
    F600778A920070C5E90071B0CD00347B9F00408AB00061CCFF005CAED5006DAC
    CC00595B5C00BCC0C20040A2D60053C2FF0055A7D300315C730066B3DE007EC4
    EB003B484F00C6EBFF004CB2EE003C87B70039789D005DC2FD0072CAFF0073CA
    FD009FCDE8009BC2D90049B7FF0046A8E90051B5F90054BAFF00316C940093D1
    FC005C7F9600A7D6F5005A6D7A00A8C2D400717F88002A91DB00339AE3002B80
    BE0047B1FF0067B1E60076A0BF002D3A440089909500328BD5003185C70043AC
    FF003680BF0049AFFF0046627800A8C9E40072849200378CDB003B87CC004DAB
    FF004B93D4005788B600BCCBD90089909600BADAFB00545B620030313400E9E9
    EA00E0E0E000DFDFDF00DADADA00D8D8D800D6D6D600D4D4D400D2D2D200CCCC
    CC00CBCBCB00C7C7C700C1C1C100BFBFBF00BBBBBB00B7B7B700B4B4B400B1B1
    B100A6A6A600A2A2A2008E8E8E008989890073737300626262005D5D5D005959
    59005454540053535300505050004F4F4F004E4E4E004D4D4D003E3E3E003D3D
    3D00313131000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000101
    010101017B7B010101010101010101010101017B828901010101010101010101
    01016E6B738801010101397F010101010101707177060101012B268301010101
    010172727A85010142382F7D01010101010175747B7E010130402C7D01010101
    010179150E80014C2A3B3601010101018186195549674B3544334D0101010176
    75080237343C2D3E4E3D010101016A70780D412E5820234F5066010101016D6C
    166428435E22245A57547C0101016F68115321605F09295246477C0101010769
    0F63653A3F270B59515B01010101131C3114325C251F0A61567C01010101101B
    178487055D031E456201010101010118121D1A040C004A48010101010101FCFF
    0000F8FF0000F0F3E4D8F0E3DBCAF0C30000F0C30065F0879845C0070000800F
    0000000F0000000700E50007DCF1000F0000000FAC57001F5E4F803F0000}
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPrompt: TLabel
    Left = 8
    Top = 8
    Width = 139
    Height = 13
    Caption = 'Indiquez votre mot de passe :'
  end
  object EditPassword: TEdit
    Left = 8
    Top = 24
    Width = 233
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object BoutonOK: TBitBtn
    Left = 36
    Top = 56
    Width = 81
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object BoutonAnnuler: TBitBtn
    Left = 132
    Top = 56
    Width = 81
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
