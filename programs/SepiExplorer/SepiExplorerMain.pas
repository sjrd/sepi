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
  SepiReflectionCore, SepiMembers, SepiSystemUnit, SepiRuntime, ExplorerOptions,
  ExplorerConsts, ExtCtrls, ComponentExplorer;

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
    ComponentExplorer: TFrameComponentExplorer;
    procedure TreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ActionEditBrowsingPathExecute(Sender: TObject);
    procedure TreeViewInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure TreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF});
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionLoadUnitExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
  private
    FOptions: TExplorerOptions; /// Options de l'explorateur
    FSepiRoot: TSepiRoot;       /// Racine Sepi
    FRuntimeUnits: TStrings;    /// Unités de type run-time

    FDisableLazyLoad: Boolean; /// Désactive le lazy-load temporairement

    function RootLoadUnit(Sender: TSepiRoot;
      const UnitName: string): TSepiUnit;

    function GetNodeComponent(Node: PVirtualNode): TSepiComponent;

    function GetRuntimeMethod(SepiMethod: TSepiMethod): TSepiRuntimeMethod;

    procedure LoadUnit(const UnitName: string);
  public
    function FindComponentNode(Component: TSepiComponent): PVirtualNode;

    property Options: TExplorerOptions read FOptions;
    property SepiRoot: TSepiRoot read FSepiRoot;
  end;

var
  /// Form principale
  ExplorerForm: TExplorerForm;

implementation

{$R *.dfm}

type
  PNodeData = ^TSepiComponent;

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

  FRuntimeUnits := TStringList.Create;

  ComponentExplorer.OnGetRuntimeMethod := GetRuntimeMethod;

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
  FRuntimeUnits.Free;
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
  DisableLazyLoad, LazyLoad: Boolean;
  RuntimeUnit: TSepiRuntimeUnit;
begin
  UnitFileName := Options.SearchFile(UnitName + CompiledIntfExt);

  DisableLazyLoad := FDisableLazyLoad;
  FDisableLazyLoad := False;

  if UnitFileName <> '' then
  begin
    Stream := TFileStream.Create(UnitFileName, fmOpenRead);
    try
      if AnsiSameText(UnitName, SystemUnitName) then
        Result := TSepiSystemUnit.LoadFromStream(Sender, Stream)
      else
      begin
        LazyLoad := (not DisableLazyLoad) and
          (Stream.Size > MaxSizeBeforeLazyLoad);
        Result := TSepiUnit.LoadFromStream(Sender, Stream, LazyLoad);
        if LazyLoad then
          Result.AcquireObjResource(Stream);
      end;
    finally
      Stream.Free;
    end;
  end else
  begin
    UnitFileName := Options.SearchFile(UnitName + CompiledUnitExt);

    if UnitFileName <> '' then
    begin
      RuntimeUnit := TSepiRuntimeUnit.Create(SepiRoot, UnitFileName);
      FRuntimeUnits.AddObject(RuntimeUnit.SepiUnit.Name, RuntimeUnit);
      Result := RuntimeUnit.SepiUnit;
    end else
      Result := nil;
  end;
end;

{*
  Obtient le meta représenté par un noeud donné
  @param Node   Noeud de l'arbre
  @return Component représenté par ce noeud
*}
function TExplorerForm.GetNodeComponent(Node: PVirtualNode): TSepiComponent;
begin
  Result := PNodeData(TreeView.GetNodeData(Node))^;
end;

{*
  Trouve la méthode run-time correspondant à une méthode Sepi
  @param SepiMethod   Méthode Sepi
  @return Méthode run-time correspondante, ou nil si non trouvée
*}
function TExplorerForm.GetRuntimeMethod(
  SepiMethod: TSepiMethod): TSepiRuntimeMethod;
var
  Index, I: Integer;
  RuntimeUnit: TSepiRuntimeUnit;
begin
  Index := FRuntimeUnits.IndexOf(SepiMethod.OwningUnit.Name);
  if Index < 0 then
    Result := nil
  else
  begin
    RuntimeUnit := TSepiRuntimeUnit(FRuntimeUnits.Objects[Index]);

    for I := 0 to RuntimeUnit.MethodCount-1 do
    begin
      Result := RuntimeUnit.Methods[I];
      if Result.SepiMethod = SepiMethod then
        Exit;
    end;

    Result := nil;
  end;
end;

{*
  Trouve le noeud de l'arbre correspondant à un meta donné
  Il n'y a aucun test de vérification d'erreur
  @param Component   Component recherché
  @return Noeud correspondant
*}
function TExplorerForm.FindComponentNode(Component: TSepiComponent): PVirtualNode;
begin
  Result := PVirtualNode(Component.Tag);
  Assert(GetNodeComponent(Result) = Component);
end;

{*
  Charge une unité et l'affiche dans l'arbre
  @param UnitName   Nom de l'unité à charger
*}
procedure TExplorerForm.LoadUnit(const UnitName: string);
begin
  try
    FDisableLazyLoad := True;
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
  Parent: TSepiComponent;
  Component: TSepiComponent;
begin
  if ParentNode = nil then
    Parent := SepiRoot
  else
    Parent := GetNodeComponent(ParentNode);

  Index := Node.Index;
  Component := Parent.Children[Index];
  PNodeData(Sender.GetNodeData(Node))^ := Component;
  Component.Tag := Integer(Node);

  if Component.ChildCount > 0 then
    Include(Node.States, vsHasChildren);

  if Pos('$', Component.Name) > 0 then
    Exclude(Node.States, vsVisible);
end;

{*
  Initialise le nombre d'enfants d'un noeud
*}
procedure TExplorerForm.TreeViewInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := GetNodeComponent(Node).ChildCount;
end;

{*
  Récupérer le nom d'un noeud
*}
procedure TExplorerForm.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF});
var
  Component: TSepiComponent;
begin
  Component := GetNodeComponent(Node);
  CellText := Component.Name;
end;

{*
  Changement de sélection dans le treeview
*}
procedure TExplorerForm.TreeViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Node = nil then
    ComponentExplorer.SepiComponent := nil
  else
    ComponentExplorer.SepiComponent := GetNodeComponent(Node);
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

initialization
  SepiUnregisterImportedUnit(SystemUnitName);
end.

