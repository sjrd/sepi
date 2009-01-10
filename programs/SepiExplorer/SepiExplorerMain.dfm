object ExplorerForm: TExplorerForm
  Left = 0
  Top = 0
  Width = 607
  Height = 494
  Caption = 'Explorateur Sepi'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterLeft: TSplitter
    Left = 289
    Top = 0
    Height = 440
  end
  object TreeView: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 289
    Height = 440
    Align = alLeft
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    NodeDataSize = 4
    TabOrder = 0
    OnChange = TreeViewChange
    OnGetText = TreeViewGetText
    OnInitChildren = TreeViewInitChildren
    OnInitNode = TreeViewInitNode
    Columns = <>
  end
  inline MetaExplorer: TFrameMetaExplorer
    Left = 292
    Top = 0
    Width = 307
    Height = 440
    Align = alClient
    TabOrder = 1
    TabStop = True
    inherited MemoInfo: TMemo
      Width = 307
      Height = 440
    end
  end
  object MainMenu: TMainMenu
    Images = ImageList
    Left = 384
    Top = 280
    object BigMenuFile: TMenuItem
      Caption = '&Fichier'
      object MenuLoadUnit: TMenuItem
        Action = ActionLoadUnit
      end
      object MenuSepFile1: TMenuItem
        Caption = '-'
      end
      object MenuExit: TMenuItem
        Action = ActionExit
      end
    end
    object BigMenuOptions: TMenuItem
      Caption = '&Options'
      object MenuEditBrowsingPath: TMenuItem
        Action = ActionEditBrowsingPath
      end
    end
  end
  object ActionList: TActionList
    Images = ImageList
    Left = 352
    Top = 280
    object ActionLoadUnit: TAction
      Category = 'File'
      Caption = 'Charger une unit'#233
      Hint = 'Charger une unit'#233' Sepi'
      ShortCut = 16463
      OnExecute = ActionLoadUnitExecute
    end
    object ActionExit: TAction
      Category = 'File'
      Caption = 'Quitter'
      Hint = 'Quitter l'#39'explorateur Sepi'
      ShortCut = 16465
      OnExecute = ActionExitExecute
    end
    object ActionEditBrowsingPath: TAction
      Category = 'Options'
      Caption = 'Chemin de recherche'
      Hint = 'Modifier le chemin de recherche'
      OnExecute = ActionEditBrowsingPathExecute
    end
  end
  object ImageList: TImageList
    Left = 416
    Top = 280
  end
end
