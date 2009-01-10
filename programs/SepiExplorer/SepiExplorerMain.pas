{-------------------------------------------------------------------------------
SepiExplorer - Example program for Sepi
As an example program, SepiExplorer is free of any usage. It is released in the
public domain.
-------------------------------------------------------------------------------}

{*
  Fiche principale de l'explorateur
  @author sjrd
  @version 1.0
*}
unit SepiExplorerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, Menus, VirtualTrees, SdDialogs,
  SepiReflectionCore, SepiRuntime, ExplorerOptions, ExplorerConsts, ExtCtrls,
  MetaExplorer;

type
  {*
    Fiche principale de l'explorateur
    @author sjrd
    @version 1.0
  *}
  TExplorerForm = class(TForm)
    TreeView: TVirtualStringTree;
    MainMenu: TMainMenu;
    BigMenuFile: TMenuItem;
    MenuLoadUnit: TMenuItem;
    MenuSepFile1: TMenuItem;
    MenuExit: TMenuItem;
    ActionList: TActionList;
    ImageList: TImageList;
    ActionLoadUnit: TAction;
    ActionExit: TAction;
    ActionEditBrowsingPath: TAction;
    BigMenuOptions: TMenuItem;
    MenuEditBrowsingPath: TMenuItem;
    SplitterLeft: TSplitter;
    MetaExplorer: TFrameMetaExplorer;
    procedure TreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ActionEditBrowsingPathExecute(Sender: TObject);
    procedure TreeViewInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure TreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionLoadUnitExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
  private
    FOptions: TExplorerOptions; /// Options de l'explorateur
    FSepiRoot: TSepiRoot;       /// Racine Sepi

    function RootLoadUnit(Sender: TSepiRoot;
      const UnitName: string): TSepiUnit;

    function GetNodeMeta(Node: PVirtualNode): TSepiMeta;

    procedure LoadUnit(const UnitName: string);
  public
    function FindMetaNode(Meta: TSepiMeta): PVirtualNode;

    property Options: TExplorerOptions read FOptions;
    property SepiRoot: TSepiRoot read FSepiRoot;
  end;

var
  /// Form principale
  ExplorerForm: TExplorerForm;

implementation

{$R *.dfm}

type
  PNodeData = ^TSepiMeta;

{---------------------}
{ TExplorerForm class }
{---------------------}

{*
  Création de la fiche
*}
procedure TExplorerForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FOptions := TExplorerOptions.Create;

  FSepiRoot := TSepiRoot.Create;
  FSepiRoot.OnLoadUnit := RootLoadUnit;

  // Load all units passed on the command line
  for I := 1 to ParamCount do
  begin
    try
      SepiRoot.LoadUnit(ParamStr(I));
    except
      on Error: ESepiUnitNotFoundError do;
    end;
  end;

  // Tell the treeview how many units there are
  TreeView.RootNodeCount := SepiRoot.UnitCount;
end;

{*
  Destruction de la fiche
*}
procedure TExplorerForm.FormDestroy(Sender: TObject);
begin
  FSepiRoot.Free;

  FOptions.Free;
end;

{*
  Charge une unité
  @param Sender     Racine Sepi
  @param UnitName   Nom de l'unité à charger
  @return Unité chargée, ou nil si non trouvée
*}
function TExplorerForm.RootLoadUnit(Sender: TSepiRoot;
  const UnitName: string): TSepiUnit;
var
  UnitFileName: string;
  Stream: TStream;
  LazyLoad: Boolean;
begin
  UnitFileName := Options.SearchFile(UnitName + CompiledIntfExt);

  if UnitFileName <> '' then
  begin
    Stream := TFileStream.Create(UnitFileName, fmOpenRead);
    LazyLoad := Stream.Size > MaxSizeBeforeLazyLoad;
    try
      Result := TSepiUnit.LoadFromStream(Sender, Stream, LazyLoad);
      if LazyLoad then
        Result.AcquireObjResource(Stream);
    finally
      Stream.Free;
    end;
  end else
  begin
    UnitFileName := Options.SearchFile(UnitName + CompiledUnitExt);

    if UnitFileName <> '' then
      Result := TSepiRuntimeUnit.Create(SepiRoot, UnitFileName).SepiUnit
    else
      Result := nil;
  end;
end;

{*
  Obtient le meta représenté par un noeud donné
  @param Node   Noeud de l'arbre
  @return Meta représenté par ce noeud
*}
function TExplorerForm.GetNodeMeta(Node: PVirtualNode): TSepiMeta;
begin
  Result := PNodeData(TreeView.GetNodeData(Node))^;
end;

{*
  Trouve le noeud de l'arbre correspondant à un meta donné
  Il n'y a aucun test de vérification d'erreur
  @param Meta   Meta recherché
  @return Noeud correspondant
*}
function TExplorerForm.FindMetaNode(Meta: TSepiMeta): PVirtualNode;
begin
  Result := PVirtualNode(Meta.Tag);
  Assert(GetNodeMeta(Result) = Meta);
end;

{*
  Charge une unité et l'affiche dans l'arbre
  @param UnitName   Nom de l'unité à charger
*}
procedure TExplorerForm.LoadUnit(const UnitName: string);
begin
  try
    SepiRoot.LoadUnit(UnitName);
    TreeView.RootNodeCount := SepiRoot.UnitCount;
  except
    on Error: ESepiUnitNotFoundError do
      ShowDialog('Unité non trouvée', Error.Message, dtError);
  end;
end;

{*
  Initialise un noeud
*}
procedure TExplorerForm.TreeViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Index: Integer;
  Parent: TSepiMeta;
  Meta: TSepiMeta;
begin
  if ParentNode = nil then
    Parent := SepiRoot
  else
    Parent := GetNodeMeta(ParentNode);

  Index := Node.Index;
  Meta := Parent.Children[Index];
  PNodeData(Sender.GetNodeData(Node))^ := Meta;
  Meta.Tag := Integer(Node);

  if Meta.ChildCount > 0 then
    Include(Node.States, vsHasChildren);

  if Pos('$', Meta.Name) > 0 then
    Exclude(Node.States, vsVisible);
end;

{*
  Initialise le nombre d'enfants d'un noeud
*}
procedure TExplorerForm.TreeViewInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := GetNodeMeta(Node).ChildCount;
end;

{*
  Récupérer le nom d'un noeud
*}
procedure TExplorerForm.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Meta: TSepiMeta;
begin
  Meta := GetNodeMeta(Node);
  CellText := Meta.Name;
end;

{*
  Changement de sélection dans le treeview
*}
procedure TExplorerForm.TreeViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Node = nil then
    MetaExplorer.SepiMeta := nil
  else
    MetaExplorer.SepiMeta := GetNodeMeta(Node);
end;

{*
  Action Charger une unité
*}
procedure TExplorerForm.ActionLoadUnitExecute(Sender: TObject);
var
  UnitName: string;
begin
  // Load unit
  if InputQuery('Charger une unité',
    'Veuillez entrer le nom de l''unité à charger', UnitName) then
    LoadUnit(UnitName);
end;

{*
  Action Quitter
*}
procedure TExplorerForm.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

{*
  Action Modifier le chemin de recherche
*}
procedure TExplorerForm.ActionEditBrowsingPathExecute(Sender: TObject);
var
  BrowsingPath: string;
begin
  BrowsingPath := Options.BrowsingPath;
  if InputQuery('Chemin de recherche', 'Chemin de recherche', BrowsingPath) then
    Options.BrowsingPath := BrowsingPath;
end;

end.

