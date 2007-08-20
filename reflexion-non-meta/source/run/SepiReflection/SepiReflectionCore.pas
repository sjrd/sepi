{*
  Définit les classes coeur de réflexion
  @author sjrd
  @version 1.0
*}
unit SepiReflectionCore;

interface

uses
  Windows, SysUtils, Classes, Contnrs, RTLConsts, IniFiles, TypInfo, Variants,
  StrUtils, ScUtils, ScStrUtils, ScSyncObjs, ScCompilerMagic, SepiCore,
  SepiReflectionConsts;

type
  TSepiReflectionItem = class;
  TSepiRoot = class;
  TSepiUnitFile = class;
  TSepiAsynchronousRootManager = class;

  {*
    État d'un élément de réflexion
  *}
  TSepiReflectionItemState = (rsNormal, rsConstructing, rsLoading,
    rsDestroying);

  {*
    Visibilité d'un membre d'une classe, d'un objet, ou d'une unité
  *}
  TMemberVisibility = (mvStrictPrivate, mvPrivate, mvStrictProtected,
    mvProtected, mvPublic, mvPublished);

  {*
    Type de l'événement OnLoadUnit de TSepiRoot
    @param Sender     Racine Sepi qui demande le chargement d'une unité
    @param UnitName   Nom de l'unité à charger
    @return L'unité chargée, ou nil si l'unité n'est pas trouvée
  *}
  TSepiLoadUnitEvent = function(Sender : TSepiRoot;
    const UnitName : string) : TSepiUnitFile of object;

  {*
    Type de l'événement OnGetMethodCode de TSepiUnitFile
    @param Sender   Méthode déclenchant l'événement (toujours TSepiMethod)
    @param Code     Adresse de code de la méthode
  *}
  TGetMethodCodeEvent = procedure(Sender : TObject;
    var Code : Pointer) of object;

  {*
    Déclenchée si l'on veut recréer un élément (second appel au constructeur)
    @author sjrd
    @version 1.0
  *}
  ESepiItemAlreadyCreated = class(ESepiError);

  {*
    Déclenchée lorsque la recherche d'un élément s'est soldée par un échec
    @author sjrd
    @version 1.0
  *}
  ESepiItemNotFoundError = class(ESepiError);

  {*
    Déclenchée lorsque la recherche d'une unité s'est soldée par un échec
    @author sjrd
    @version 1.0
  *}
  ESepiUnitNotFoundError = class(ESepiItemNotFoundError);

  {*
    Déclenchée si l'on tente de créer une constante avec un mauvais type
    @author sjrd
    @version 1.0
  *}
  ESepiBadConstTypeError = class(ESepiError);

  {*
    Liste d'éléments de l'arborescence de réflexion
    @author sjrd
    @version 1.0
  *}
  TSepiReflectionItemList = class(THashedStringList)
  private
    function GetItems(Index : integer) : TSepiReflectionItem;
    procedure SetItems(Index : integer; Value : TSepiReflectionItem);
    function GetItemFromName(const Name : string) : TSepiReflectionItem;
    procedure SetItemFromName(const Name : string; Value : TSepiReflectionItem);
  protected
    procedure InsertItem(Index : Integer; const S : string;
      AObject : TObject); override;
  public
    constructor Create;

    function AddItem(Item : TSepiReflectionItem) : integer;
    function IndexOfItem(Item : TSepiReflectionItem) : integer;
    function Remove(Item : TSepiReflectionItem) : integer;

    property Items[index : integer] : TSepiReflectionItem
      read GetItems write SetItems; default;
    property ItemFromName[const Name : string] : TSepiReflectionItem
      read GetItemFromName write SetItemFromName;
  end;

  {*
    Élément de l'arborescence de réflexion
    L'arborescence de réflexion est un arbre de données qui représente la
    structure statique d'unités à l'exécution.
    @author sjrd
    @version 1.0
  *}
  TSepiReflectionItem = class
  private
    FIsForward : boolean;                /// True si l'élément est forward
    FState : TSepiReflectionItemState;   /// État
    FOwner : TSepiReflectionItem;        /// Propriétaire
    FRoot : TSepiRoot;                   /// Racine
    FOwningUnit : TSepiUnitFile;         /// Unité contenante
    FName : string;                      /// Nom
    FForwards : TStrings;                /// Liste des enfants forwards
    FChildren : TSepiReflectionItemList; /// Liste des enfants
    FObjResources : TObjectList;         /// Liste des ressources objet
    FPtrResources : TList;               /// Liste des ressources pointeur

    procedure AddChild(Child : TSepiReflectionItem);
    procedure RemoveChild(Child : TSepiReflectionItem);

    function GetChildCount : integer;
    function GetChildren(Index : integer) : TSepiReflectionItem;
  protected
    FVisibility : TMemberVisibility; /// Visibilité

    procedure LoadChildren(Stream : TStream);
    procedure SaveChildren(Stream : TStream);

    procedure AddForward(const ChildName : string; Child : TObject);
    procedure ChildAdded(Child : TSepiReflectionItem); virtual;
    procedure ChildRemoving(Child : TSepiReflectionItem); virtual;

    procedure Loaded; virtual;

    procedure ListReferences; virtual;
    procedure Save(Stream : TStream); virtual;

    procedure Destroying; virtual;

    property State : TSepiReflectionItemState read FState;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); virtual;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function NewInstance : TObject; override;

    function GetFullName : string;
    function GetChild(const Name : string) : TSepiReflectionItem;
    function FindChild(const Name : string) : TSepiReflectionItem;

    procedure AddObjResource(Obj : TObject);
    procedure AddPtrResource(Ptr : Pointer);

    property IsForward : boolean read FIsForward;
    property Owner : TSepiReflectionItem read FOwner;
    property Root : TSepiRoot read FRoot;
    property OwningUnit : TSepiUnitFile read FOwningUnit;
    property Name : string read FName;
    property Visibility : TMemberVisibility read FVisibility write FVisibility;

    property ChildCount : integer read GetChildCount;
    property Children[index : integer] : TSepiReflectionItem read GetChildren;
  end;

  {*
    Classe de TSepiReflectionItem
  *}
  TSepiReflectionItemClass = class of TSepiReflectionItem;

  {*
    Comportement d'un type lorsqu'il est passé en paramètre
    @author sjrd
    @version 1.0
  *}
  TSepiTypeParamBehavior = record
    AlwaysByAddress : boolean; /// Le type est toujours passé par adresse
    AlwaysByStack : boolean;   /// Le type est toujours passé sur la pile
  end;

  {*
    Type
    @author sjrd
    @version 1.0
  *}
  TSepiType = class(TSepiReflectionItem)
  private
    FKind : TTypeKind;         /// Type de type
    FNative : boolean;         /// Indique si le type est un type natif Delphi
    FTypeInfoLength : integer; /// Taille des RTTI créées (ou 0 si non créées)
    FTypeInfo : PTypeInfo;     /// RTTI (Runtime Type Information)
    FTypeData : PTypeData;     /// RTTD (Runtime Type Data)
    FTypeInfoRef : PPTypeInfo; /// Référence aux RTTI
  protected
    FSize : integer;     /// Taille d'une variable de ce type
    FNeedInit : boolean; /// Indique si ce type requiert une initialisation

    FParamBehavior : TSepiTypeParamBehavior; /// Comportement comme paramètre

    procedure Save(Stream : TStream); override;

    procedure ForceNative(ATypeInfo : PTypeInfo = nil);
    procedure AllocateTypeInfo(TypeDataLength : integer = 0);
    procedure ExtractTypeData; virtual;

    function GetAlignment : integer; virtual;

    property TypeInfoRef : PPTypeInfo read FTypeInfoRef;
  public
    constructor RegisterTypeInfo(AOwner : TSepiReflectionItem;
      ATypeInfo : PTypeInfo); virtual;
    constructor Clone(AOwner : TSepiReflectionItem; const AName : string;
      Source : TSepiType); virtual;
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      AKind : TTypeKind);
    destructor Destroy; override;

    class function NewInstance : TObject; override;

    class function LoadFromTypeInfo(AOwner : TSepiReflectionItem;
      ATypeInfo : PTypeInfo) : TSepiType;

    procedure AlignOffset(var Offset : integer);

    function CompatibleWith(AType : TSepiType) : boolean; virtual;

    property Kind : TTypeKind read FKind;
    property Native : boolean read FNative;
    property TypeInfo : PTypeInfo read FTypeInfo;
    property TypeData : PTypeData read FTypeData;
    property Size : integer read FSize;
    property NeedInit : boolean read FNeedInit;
    property Alignment : integer read GetAlignment;
    property ParamBehavior : TSepiTypeParamBehavior read FParamBehavior;
  end;

  {*
    Classe de TSepiType
  *}
  TSepiTypeClass = class of TSepiType;

  {*
    Racine de l'arbre de réflexion
    @author sjrd
    @version 1.0
  *}
  TSepiRoot = class(TSepiReflectionItem)
  private
    FSearchOrder : TObjectList;       /// Ordre de recherche
    FOnLoadUnit : TSepiLoadUnitEvent; /// Déclenché au chargement d'une unité

    function GetUnitCount : integer;
    function GetUnits(Index : integer) : TSepiUnitFile;
  protected
    procedure ChildAdded(Child : TSepiReflectionItem); override;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadUnit(const UnitName : string) : TSepiUnitFile;
    procedure UnloadUnit(const UnitName : string);

    function GetType(TypeInfo : PTypeInfo) : TSepiType; overload;
    function GetType(const TypeName : string) : TSepiType; overload;

    function FindType(TypeInfo : PTypeInfo) : TSepiType; overload;
    function FindType(const TypeName : string) : TSepiType; overload;

    property UnitCount : integer read GetUnitCount;
    property Units[index : integer] : TSepiUnitFile read GetUnits;

    property OnLoadUnit : TSepiLoadUnitEvent read FOnLoadUnit write FOnLoadUnit;
  end;

  {*
    Unité
    @author sjrd
    @version 1.0
  *}
  TSepiUnitFile = class(TSepiReflectionItem)
  private
    /// Déclenché pour chaque méthode au chargement, pour obtenir son code
    FOnGetMethodCode : TGetMethodCodeEvent;

    FRefCount : integer;                    /// Compteur de références
    FUsesList : TStrings;                   /// Liste des uses
    FCurrentVisibility : TMemberVisibility; /// Visibilité courante

    FReferences : array of TStrings; /// Références en chargement/sauvegarde

    function AddUses(AUnit : TSepiUnitFile) : integer;

    procedure SetCurrentVisibility(Value : TMemberVisibility);

    function GetUsedUnitCount : integer;
    function GetUsedUnits(Index : integer) : TSepiUnitFile;
  protected
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      const AUses : array of string);
    destructor Destroy; override;

    procedure MoreUses(const AUses : array of string);

    procedure Complete;

    procedure SaveToStream(Stream : TStream);
    class function LoadFromStream(AOwner : TSepiReflectionItem;
      Stream : TStream;
      const AOnGetMethodCode : TGetMethodCodeEvent = nil) : TSepiUnitFile;

    procedure ReadRef(Stream : TStream; out Ref);
    procedure AddRef(Ref : TSepiReflectionItem);
    procedure WriteRef(Stream : TStream; Ref : TSepiReflectionItem);

    property CurrentVisibility : TMemberVisibility
      read FCurrentVisibility write SetCurrentVisibility;

    property UsedUnitCount : integer read GetUsedUnitCount;
    property UsedUnits[index : integer] : TSepiUnitFile read GetUsedUnits;

    property OnGetMethodCode : TGetMethodCodeEvent read FOnGetMethodCode;
  end;

  {*
    Alias de type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeAlias = class(TSepiReflectionItem)
  private
    FDest : TSepiType; /// Type destination
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      ADest : TSepiType); overload;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      ADest : PTypeInfo); overload;
    constructor Create(AOwner : TSepiReflectionItem;
      const AName, ADest : string); overload;

    property Dest : TSepiType read FDest;
  end;

  {*
    Constante
    @author sjrd
    @version 1.0
  *}
  TSepiConstant = class(TSepiReflectionItem)
  private
    FType : TSepiType;   /// Type de la constante
    FValuePtr : Pointer; /// Pointeur sur la valeur
    FStrValue : string;  /// Contenu de la valeur de type chaîne
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      const AValue : Variant; AType : TSepiType); overload;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      const AValue : Variant; ATypeInfo : PTypeInfo = nil); overload;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      const AValue : Variant; const ATypeName : string); overload;
    destructor Destroy; override;

    property ConstType : TSepiType read FType;
    property ValuePtr : Pointer read FValuePtr;
  end;

  {*
    Variable (ou constante typée)
    @author sjrd
    @version 1.0
  *}
  TSepiVariable = class(TSepiReflectionItem)
  private
    FIsConst : boolean;  /// Indique si la variable est une constante typée
    FType : TSepiType;   /// Type de la constante
    FValue : Pointer;    /// Pointeur sur la variable
    FOwnValue : boolean; /// Indique si Sepi a alloué la variable
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;

    procedure Destroying; override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;

    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      const AValue; AType : TSepiType; AIsConst : boolean = False); overload;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      const AValue; ATypeInfo : PTypeInfo;
      AIsConst : boolean = False); overload;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      const AValue; const ATypeName : string;
      AIsConst : boolean = False); overload;

    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      AType : TSepiType; AIsConst : boolean = False); overload;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      ATypeInfo : PTypeInfo; AIsConst : boolean = False); overload;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      const ATypeName : string; AIsConst : boolean = False); overload;

    destructor Destroy; override;

    property IsConst : boolean read FIsConst;
    property VarType : TSepiType read FType;
    property Value : Pointer read FValue;
  end;

  {*
    Tâche de chargement/déchargement d'une unité Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiUnitLoadTask = class(TScTask)
  private
    FRoot : TSepiRoot;  /// Racine Sepi
    FUnitName : string; /// Nom de l'unité à charger/décharger
    FIsLoad : boolean;  /// True pour charger, False pour décharger
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner : TScCustomTaskQueue; ARoot : TSepiRoot;
      const AUnitName : string; AIsLoad : boolean;
      AFreeOnFinished : boolean); overload;
    constructor Create(AOwner : TScCustomTaskQueue; ARoot : TSepiRoot;
      const AUnitName : string; AIsLoad : boolean); overload;

    property Root : TSepiRoot read FRoot;
    property UnitName : string read FUnitName;
    property IsLoad : boolean read FIsLoad;
  end;

  {*
    Gestionnaire asynchrone de racine Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiAsynchronousRootManager = class(TScTaskQueue)
  private
    FOwnsRoot : boolean; /// Indique si l'on possède la racine
    FRoot : TSepiRoot;   /// Racine gérée
  public
    constructor Create(ARoot : TSepiRoot = nil);
    destructor Destroy; override;

    procedure LoadUnit(const UnitName : string);
    procedure UnloadUnit(const UnitName : string);

    property Root : TSepiRoot read FRoot;
  end;

  {*
    Type de routine call-back pour l'import d'une unité sous Sepi
  *}
  TSepiImportUnitFunc = function(Root : TSepiRoot) : TSepiUnitFile;

procedure SepiRegisterReflectionItemClasses(
  const ItemClasses : array of TSepiReflectionItemClass);

procedure SepiRegisterImportedUnit(const UnitName : string;
  ImportFunc : TSepiImportUnitFunc);
procedure SepiUnregisterImportedUnit(const UnitName : string);

const {don't localize}
  SystemUnitName = 'System'; /// Nom de l'unité System.pas

const
  /// Comportement par des défaut d'un type comme paramètre
  DefaultTypeParamBehavior : TSepiTypeParamBehavior = (
    AlwaysByAddress : False; AlwaysByStack : False
  );

implementation

uses
  SepiOrdTypes, SepiStrTypes, SepiArrayTypes, SepiMembers, SepiImportsSystem;

var
  SepiReflectionItemClasses : TStrings = nil;
  SepiImportedUnits : TStrings = nil;

{*
  Recense des classes d'éléments de réflexion
  @param ItemClasses   Classes de réflexion à recenser
*}
procedure SepiRegisterReflectionItemClasses(
  const ItemClasses : array of TSepiReflectionItemClass);
var I : integer;
begin
  if not Assigned(SepiReflectionItemClasses) then
  begin
    SepiReflectionItemClasses := TStringList.Create;
    with TStringList(SepiReflectionItemClasses) do
    begin
      CaseSensitive := False;
      Sorted := True;
      Duplicates := dupIgnore;
    end;
  end;

  for I := Low(ItemClasses) to High(ItemClasses) do
  begin
    SepiReflectionItemClasses.AddObject(
      ItemClasses[I].ClassName, TObject(ItemClasses[I]));
  end;
end;

{*
  Cherche une classe d'élément de réflexion par son nom
  La classe recherchée doit avoir été recensée au préalable avec
  SepiRegisterReflectionItemClasses.
  @param ItemClassName   Nom de la classe d'élément de réflexion
  @return La classe d'élément de réflexion dont le nom correspond
  @throws EClassNotFound La classe recherchée n'existe pas
*}
function SepiFindReflectionItemClass(
  const ItemClassName : string) : TSepiReflectionItemClass;
var Index : integer;
begin
  if not Assigned(SepiReflectionItemClasses) then Index := -1 else
    Index := SepiReflectionItemClasses.IndexOf(ItemClassName);
  if Index < 0 then
    EClassNotFound.CreateFmt(SClassNotFound, [ItemClassName]);

  Result := TSepiReflectionItemClass(SepiReflectionItemClasses.Objects[Index]);
end;

{*
  Recense une routine d'import d'unité
  @param UnitName     Nom de l'unité
  @param ImportFunc   Routine de call-back pour l'import de l'unité
*}
procedure SepiRegisterImportedUnit(const UnitName : string;
  ImportFunc : TSepiImportUnitFunc);
begin
  if not Assigned(SepiImportedUnits) then
  begin
    SepiImportedUnits := TStringList.Create;
    with TStringList(SepiImportedUnits) do
    begin
      CaseSensitive := False;
      Duplicates := dupIgnore;
    end;
  end;

  SepiImportedUnits.AddObject(UnitName, TObject(@ImportFunc));
end;

{*
  Supprime un recensement d'import d'unité
  @param UnitName   Nom de l'unité
*}
procedure SepiUnregisterImportedUnit(const UnitName : string);
var Index : integer;
begin
  if not Assigned(SepiImportedUnits) then exit;
  Index := SepiImportedUnits.IndexOf(UnitName);
  if Index >= 0 then
    SepiImportedUnits.Delete(Index);
end;

{*
  Cherche une routine d'import d'une unité
  Cette routine doit avoir été recensée au prélable avec
  SepiRegisterImportedUnit.
  @param UnitName   Nom de l'unité
  @return Routine de call-back pour l'import de l'unité, ou nil si n'existe pas
*}
function SepiImportedUnit(const UnitName : string) : TSepiImportUnitFunc;
var Index : integer;
begin
  if not Assigned(SepiImportedUnits) then Result := nil else
  begin
    Index := SepiImportedUnits.IndexOf(UnitName);
    if Index < 0 then Result := nil else
      Result := TSepiImportUnitFunc(SepiImportedUnits.Objects[Index]);
  end;
end;

{--------------------------------}
{ Classe TSepiReflectionItemList }
{--------------------------------}

{*
  Crée une instance de TSepiReflectionItemList
*}
constructor TSepiReflectionItemList.Create;
begin
  inherited;
  CaseSensitive := False;
end;

{*
  Liste zero-based des éléments contenus
  @param Index   Index de l'élément à obtenir
  @return Élément à l'index spécifié
*}
function TSepiReflectionItemList.GetItems(
  Index : integer) : TSepiReflectionItem;
begin
  Result := TSepiReflectionItem(Objects[Index]);
end;

{*
  Assigne la référence à un élément
  Un élément ne peut pas être modifé une fois assigné, il ne peut donc être
  assigné qu'une et une seule fois.
  @param Index   Index de l'élément à modifier
  @param Value   Référence au nouvel élément
*}
procedure TSepiReflectionItemList.SetItems(Index : integer;
  Value : TSepiReflectionItem);
begin
  if Assigned(Objects[Index]) then
    Error(@SSepiItemAlreadyAssigned, Index);
  Objects[Index] := Value;
end;

{*
  Liste des éléments indexée par leurs noms
  @param Name   Nom d'un élément
  @return L'élément dont le nom a été spécifié, ou nil s'il n'existe pas
*}
function TSepiReflectionItemList.GetItemFromName(
  const Name : string) : TSepiReflectionItem;
var Index : integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then Result := nil else
    Result := TSepiReflectionItem(Objects[Index]);
end;

{*
  Assigne ou ajoute un élément par son nom
  Un élément ne peut pas être modifé une fois assigné, il ne peut donc être
  assigné qu'une et une seule fois.
  @param Name    Nom de l'élément
  @param Value   Référence à l'élément
*}
procedure TSepiReflectionItemList.SetItemFromName(const Name : string;
  Value : TSepiReflectionItem);
var Index : integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then AddObject(Name, Value) else
    Items[Index] := Value;
end;

{*
  Ajoute un élément dans la liste de Items
  @param Index     Index où ajouter l'élément
  @param S         Chaîne à ajouter
  @param AObject   Objet à ajouter
*}
procedure TSepiReflectionItemList.InsertItem(Index : Integer; const S : string;
  AObject : TObject);
begin
  if IndexOf(S) >= 0 then
    raise EListError.CreateFmt(SSepiItemAlreadyExists, [S]);
  inherited;
end;

{*
  Ajoute un élément
  @param Item   Élément à ajouter
  @return Index de l'élément nouvellement ajouté
*}
function TSepiReflectionItemList.AddItem(Item : TSepiReflectionItem) : integer;
begin
  Result := AddObject(Item.Name, Item);
end;

{*
  Cherche un élément dans la liste
  @param Item   Élément à chercher
  @return L'index de l'élément dans la liste, ou nil s'il n'existe pas
*}
function TSepiReflectionItemList.IndexOfItem(
  Item : TSepiReflectionItem) : integer;
begin
  Result := IndexOf(Item.Name);
end;

{*
  Supprime un élément de la liste
  @param Item   Élément à supprimer
  @return L'index auquel se trouvait l'élément
*}
function TSepiReflectionItemList.Remove(Item : TSepiReflectionItem) : integer;
begin
  Result := IndexOfItem(Item);
  Delete(Result);
end;

{----------------------------}
{ Classe TSepiReflectionItem }
{----------------------------}

{*
  Charge un élément de réflexion depuis un flux
  @param AOwner   Propriétaire de l'élément
  @param Stream   Flux depuis lequel charger l'élément
*}
constructor TSepiReflectionItem.Load(AOwner : TSepiReflectionItem;
  Stream : TStream);
begin
  Create(AOwner, ReadStrFromStream(Stream));
  Stream.ReadBuffer(FVisibility, 1);
  FState := rsLoading;
end;

{*
  Crée un nouvel élément de réflexion
  @param AOwner   Propriétaire de l'élément
  @param AName    Nom de l'élément
*}
constructor TSepiReflectionItem.Create(AOwner : TSepiReflectionItem;
  const AName : string);
begin
  if not IsForward then
    raise ESepiItemAlreadyCreated.CreateFmt(SSepiItemAlreadyCreated, [Name]);

  inherited Create;
  FIsForward := False;
  FState := rsConstructing;
  FOwner := AOwner;
  FName := AName;
  FVisibility := mvPublic;
  FForwards := THashedStringList.Create;
  FChildren := TSepiReflectionItemList.Create;
  FObjResources := TObjectList.Create;
  FPtrResources := TList.Create;

  if Assigned(FOwner) then
  begin
    FRoot := FOwner.Root;
    FOwningUnit := FOwner.OwningUnit;
    FOwner.AddChild(Self);
  end else
  begin
    FRoot := nil;
    FOwningUnit := nil;
  end;
end;

{*
  Détruit l'instance
*}
destructor TSepiReflectionItem.Destroy;
var I : integer;
begin
  if State <> rsDestroying then // only if an error has occured in constructor
    Destroying;

  if Assigned(FPtrResources) then
  begin
    for I := 0 to FPtrResources.Count-1 do
      FreeMem(FPtrResources[I]);
    FPtrResources.Free;
  end;
  FObjResources.Free;

  if Assigned(FChildren) then
  begin
    for I := 0 to FChildren.Count-1 do
      FChildren.Objects[I].Free;
    FChildren.Free;
  end;
  FForwards.Free;

  if Assigned(FOwner) then
    FOwner.RemoveChild(Self);
  inherited Destroy;
end;

{*
  Ajoute un enfant
  AddChild est appelée dans le constructeur de l'élément enfant, et ne doit pas
  être appelée ailleurs.
  @param Child   Enfant à ajouter
*}
procedure TSepiReflectionItem.AddChild(Child : TSepiReflectionItem);
begin
  FChildren.AddItem(Child);
end;

{*
  Supprime un enfant
  RemoveChild est appelée dans le destructeur de l'élément enfant, et ne doit
  pas être appelée ailleurs.
  @param Child   Enfant à supprimer
*}
procedure TSepiReflectionItem.RemoveChild(Child : TSepiReflectionItem);
var Index : integer;
begin
  if State <> rsDestroying then
  begin
    FChildren.Remove(Child);

    Index := FForwards.IndexOfObject(Child);
    if Index >= 0 then FForwards.Delete(Index);
  end;
end;

{*
  Nombre d'enfants
  @return Nombre d'enfants
*}
function TSepiReflectionItem.GetChildCount : integer;
begin
  Result := FChildren.Count;
end;

{*
  Tableau zero-based des enfants
  @param Index   Index de l'enfant à récupérer
  @return Enfant à l'index spécifié
*}
function TSepiReflectionItem.GetChildren(Index : integer) : TSepiReflectionItem;
begin
  Result := TSepiReflectionItem(FChildren[Index]);
end;

{*
  Charge les enfants depuis un flux
  @param Stream   Flux depuis lequel charger les enfants
*}
procedure TSepiReflectionItem.LoadChildren(Stream : TStream);
var Count, I : integer;
    ForwardChild : TObject;
    IsForward : boolean;
begin
  // Forwards
  Stream.ReadBuffer(Count, 4);
  for I := 0 to Count-1 do
  begin
    ForwardChild := SepiFindReflectionItemClass(
      ReadStrFromStream(Stream)).NewInstance;
    AddForward(ReadStrFromStream(Stream), ForwardChild);
  end;

  // Actual loading
  Stream.ReadBuffer(Count, 4);
  for I := 0 to Count-1 do
  begin
    Stream.ReadBuffer(IsForward, 1);
    if IsForward then
      FindChild(ReadStrFromStream(Stream)).Load(Self, Stream)
    else
      SepiFindReflectionItemClass(ReadStrFromStream(Stream)).Load(Self, Stream);
  end;
end;

{*
  Enregistre les enfants dans un flux
  @param Stream   Flux dans lequel enregistrer les enfants
*}
procedure TSepiReflectionItem.SaveChildren(Stream : TStream);
var Count, I : integer;
    Child : TSepiReflectionItem;
    IsForward : boolean;
begin
  // Forwards
  Count := FForwards.Count;
  Stream.WriteBuffer(Count, 4);
  for I := 0 to Count-1 do
  begin
    WriteStrToStream(Stream, FForwards.Objects[I].ClassName);
    WriteStrToStream(Stream, FForwards[I]);
  end;

  // Actual saving
  Count := FChildren.Count;
  Stream.WriteBuffer(Count, 4);
  for I := 0 to Count-1 do
  begin
    Child := FChildren[I];

    IsForward := FForwards.IndexOfObject(Child) >= 0;
    Stream.WriteBuffer(IsForward, 1);

    if IsForward then
      WriteStrToStream(Stream, Child.Name)
    else
      WriteStrToStream(Stream, Child.ClassName);

    Child.Save(Stream);
  end;
end;

{*
  Ajoute un enfant forward
  @param ChildName   Nom de l'enfant
  @param Child       Enfant à ajouter
*}
procedure TSepiReflectionItem.AddForward(const ChildName : string;
  Child : TObject);
begin
  FForwards.AddObject(ChildName, Child);
end;

{*
  Appelé lorsqu'un enfant vient d'être ajouté
  @param Child   Enfant qui vient d'être ajouté
*}
procedure TSepiReflectionItem.ChildAdded(Child : TSepiReflectionItem);
begin
end;

{*
  Appelé lorsqu'un enfant va être supprimé
  @param Child   Enfant sur le point d'être supprimé
*}
procedure TSepiReflectionItem.ChildRemoving(Child : TSepiReflectionItem);
begin
end;

{*
  Appelé lorsque l'unité contenante est complètement chargée/créée
*}
procedure TSepiReflectionItem.Loaded;
var I : integer;
begin
  // Already created children
  for I := 0 to FChildren.Count-1 do
    FChildren[I].Loaded;

  // Forward declarations which have not been created yet
  for I := 0 to FForwards.Count-1 do
    with TSepiReflectionItem(FForwards.Objects[I]) do
      if IsForward then Loaded;

  // Changing the state
  FState := rsNormal;
end;

{*
  Liste les références auprès de l'unité contenante, pour préparer la sauvegarde
*}
procedure TSepiReflectionItem.ListReferences;
var I : integer;
begin
  for I := 0 to FChildren.Count-1 do
    FChildren[I].ListReferences;
end;

{*
  Enregistre l'élément dans un flux
  @param Stream   Flux dans lequel enregistrer l'élément
*}
procedure TSepiReflectionItem.Save(Stream : TStream);
begin
  WriteStrToStream(Stream, Name);
  Stream.WriteBuffer(FVisibility, 1);
end;

{*
  Appelé lorsque l'environnement Sepi est sur le point d'être détruit
*}
procedure TSepiReflectionItem.Destroying;
var I : integer;
begin
  FState := rsDestroying;
  if Assigned(FChildren) then // could not be if an error occured in constructor
    for I := 0 to ChildCount-1 do
      Children[I].Destroying;
end;

{*
  Appelé juste après l'exécution du dernier constructeur
*}
procedure TSepiReflectionItem.AfterConstruction;
begin
  inherited;
  if Assigned(Owner) then
    Owner.ChildAdded(Self);
end;

{*
  Appelé juste avant l'exécution du premier destructeur
*}
procedure TSepiReflectionItem.BeforeDestruction;
begin
  if FState <> rsDestroying then
    Destroying;

  inherited;

  if Assigned(Owner) then
    Owner.ChildRemoving(Self);
end;

{*
  Crée une nouvelle instance de TSepiReflectionItem
  @return Instance créée
*}
class function TSepiReflectionItem.NewInstance : TObject;
begin
  Result := inherited NewInstance;
  TSepiReflectionItem(Result).FIsForward := True;
end;

{*
  Nom qualifié de l'élément, depuis l'unité contenante
  @return Nom qualifié de l'élément
*}
function TSepiReflectionItem.GetFullName : string;
begin
  if Assigned(FOwner) and (FOwner.Name <> '') then
    Result := FOwner.GetFullName+'.'+Name
  else
    Result := Name;
end;

{*
  Cherche un élément enfant
  @param Name   Nom de l'élément à trouver
  @return L'élément correspondant, ou nil s'il n'a pas été trouvé
*}
function TSepiReflectionItem.GetChild(
  const Name : string) : TSepiReflectionItem;
var I : integer;
    ChildName, Field : string;
begin
  if not SplitToken(Name, '.', ChildName, Field) then
    Field := '';
  Result := FChildren.ItemFromName[ChildName];

  if (Result = nil) and (Field = '') then
  begin
    I := FForwards.IndexOf(ChildName);
    if I >= 0 then
      Result := TSepiReflectionItem(FForwards.Objects[I]);
  end;

  if not Assigned(Result) then exit;
  while Result is TSepiTypeAlias do
    Result := TSepiTypeAlias(Result).Dest;
  if Field <> '' then
    Result := Result.GetChild(Field);
end;

{*
  Cherche un élément enfant
  @param Name   Nom de l'élément à trouver
  @return L'élément correspondant
  @throws ESepiItemNotFoundError L'élément n'a pas été trouvé
*}
function TSepiReflectionItem.FindChild(
  const Name : string) : TSepiReflectionItem;
begin
  Result := GetChild(Name);
  if (Result = nil) and (Name <> '') then
    raise ESepiItemNotFoundError.CreateFmt(SSepiObjectNotFound, [Name]);
end;

{*
  Ajoute un objet aux ressources de l'élément
  Tous les objets ajoutés aux ressources de l'élément seront libérés lorsque
  l'élément sera libéré lui-même.
  @param Obj   Objet à ajouter aux ressources
*}
procedure TSepiReflectionItem.AddObjResource(Obj : TObject);
begin
  FObjResources.Add(Obj);
end;

{*
  Ajoute un pointeur aux ressources de l'élément
  Tous les pointeurs ajoutés aux ressources de l'élément seront libérés lorsque
  l'élément sera libéré lui-même.
  @param Ptr   Pointeur à ajouter aux ressources
*}
procedure TSepiReflectionItem.AddPtrResource(Ptr : Pointer);
begin
  FPtrResources.Add(Ptr);
end;

{------------------}
{ Classe TSepiType }
{------------------}

{*
  Recense un type natif
  @param AOwner      Propriétaire du type
  @param ATypeInfo   RTTI du type à recenser
*}
constructor TSepiType.RegisterTypeInfo(AOwner : TSepiReflectionItem;
  ATypeInfo : PTypeInfo);
begin
  inherited Create(AOwner, AnsiReplaceStr(ATypeInfo.Name, '.', '$$'));

  FKind := ATypeInfo.Kind;
  FNative := True;
  FTypeInfoLength := 0;
  FTypeInfo := ATypeInfo;
  FTypeData := GetTypeData(FTypeInfo);
  FSize := 0;
  FParamBehavior := DefaultTypeParamBehavior;
end;

{*
  Clone un type
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
  @param Source   Type à cloner
*}
constructor TSepiType.Clone(AOwner : TSepiReflectionItem; const AName : string;
  Source : TSepiType);
begin
  inherited Create(AOwner, AName);

  FKind := Source.Kind;
  FNative := False;
  FTypeInfoLength := 0;
  FTypeInfo := nil;
  FTypeData := nil;
  FSize := 0;
  FNeedInit := False;
  FParamBehavior := DefaultTypeParamBehavior;
end;

{*
  Charge un type depuis un flux
  @param AOwner   Propriétaire du type
  @param Stream   Flux depuis lequel charger le type
*}
constructor TSepiType.Load(AOwner : TSepiReflectionItem; Stream : TStream);
begin
  inherited;

  Stream.ReadBuffer(FKind, sizeof(TTypeKind));
  FNative := False;
  FTypeInfoLength := 0;
  FTypeInfo := nil;
  FTypeData := nil;
  FSize := 0;
  FNeedInit := False;
  FParamBehavior := DefaultTypeParamBehavior;
end;

{*
  Crée un nouveau type
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
  @param AKind    Type de type
*}
constructor TSepiType.Create(AOwner : TSepiReflectionItem; const AName : string;
  AKind : TTypeKind);
begin
  inherited Create(AOwner, AName);

  FKind := AKind;
  FNative := False;
  FTypeInfoLength := 0;
  FTypeInfo := nil;
  FTypeData := nil;
  FSize := 0;
  FNeedInit := False;
  FParamBehavior := DefaultTypeParamBehavior;
end;

{*
  [@inheritDoc]
*}
destructor TSepiType.Destroy;
begin
  if FTypeInfoLength > 0 then
    FreeMem(FTypeInfo, FTypeInfoLength);

  inherited Destroy;
end;

procedure TSepiType.Save(Stream : TStream);
begin
  inherited;
  Stream.WriteBuffer(FKind, 1);
end;

{*
  Force le type comme étant natif, en modifiant également les RTTI
  Cette méthode est utilisée par les types record et tableau statique, qui n'ont
  pas toujours, même natifs, de RTTI.
  @param ATypeInfo   RTTI à fixer (peut être nil)
*}
procedure TSepiType.ForceNative(ATypeInfo : PTypeInfo = nil);
begin
  FNative := True;
  FTypeInfo := ATypeInfo;
  if FTypeInfo = nil then
    FTypeData := nil
  else
    FTypeData := GetTypeData(FTypeInfo);
end;

{*
  Alloue une zone mémoire pour les RTTI
  Alloue une zone mémoire adaptée au nom du type et à la taille des données de
  type, et remplit les champs de TypeInfo (TypeData reste non initialisé).
  La zone mémoire ainsi allouée sera automatiquement libérée à la destruction du
  type.
  @param TypeDataLength   Taille des données de type
*}
procedure TSepiType.AllocateTypeInfo(TypeDataLength : integer = 0);
var ShortName : ShortString;
    NameLength : integer;
begin
  ShortName := Name;
  NameLength := Length(Name)+1; // 1 byte for string length

  FTypeInfoLength := sizeof(TTypeKind) + NameLength + TypeDataLength;
  GetMem(FTypeInfo, FTypeInfoLength);

  FTypeInfo.Kind := FKind;
  Move(ShortName, FTypeInfo.Name, NameLength);
  FTypeData := GetTypeData(FTypeInfo);
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiType.ExtractTypeData;
begin
end;

{*
  Alignement du type
  Les variables de ce type seront placées en mémoire à un adresse divisible par
  l'alignement.
  @return Alignement du type
*}
function TSepiType.GetAlignment : integer;
begin
  Result := Size;
end;

{*
  Crée une nouvelle instance de TSepiType
  @return Instance créée
*}
class function TSepiType.NewInstance : TObject;
begin
  Result := inherited NewInstance;
  TSepiType(Result).FTypeInfoRef := @TSepiType(Result).FTypeInfo;
end;

{*
  Recense un type natif à partir de ses RTTI
  @param AOwner      Propriétaire du type
  @param ATypeInfo   RTTI du type à recenser
  @return Type nouvellement créé
*}
class function TSepiType.LoadFromTypeInfo(AOwner : TSepiReflectionItem;
  ATypeInfo : PTypeInfo) : TSepiType;
const
  TypeClasses : array[TTypeKind] of TSepiTypeClass = (
    nil, TSepiIntegerType, TSepiCharType, TSepiEnumType, TSepiFloatType,
    TSepiShortStringType, TSepiSetType, TSepiClass, TSepiMethodRefType,
    TSepiCharType, TSepiStringType, TSepiStringType, TSepiVariantType,
    nil, nil, TSepiInterface, TSepiInt64Type, TSepiDynArrayType
  );
var TypeClass : TSepiTypeClass;
begin
  TypeClass := TypeClasses[ATypeInfo.Kind];
  if Assigned(TypeClass) then
  begin
    if TypeClass = TSepiEnumType then
    begin
      with GetTypeData(ATypeInfo)^ do
        if (BaseType^ = System.TypeInfo(Boolean)) or
           (GetTypeData(BaseType^).MinValue < 0) then
          TypeClass := TSepiBooleanType;
    end;

    Result := TypeClass.RegisterTypeInfo(AOwner, ATypeInfo)
  end else raise EAbstractError.Create(SSepiNoRegisterTypeInfo);
end;

{*
  Aligne l'offset donné conformément à la propriété Alignment
  @param Offset   Offset à aligner
*}
procedure TSepiType.AlignOffset(var Offset : integer);
var Disalign : integer;
begin
  Disalign := Offset mod Alignment;
  if Disalign > 0 then
    inc(Offset, Alignment-Disalign);
end;

{*
  Teste si un type est compatible avec un autre
  Il faut appeler CompatibleWith sur le type de la variable affectée, et avec en
  paramètre le type de l'expression à droite de l'assignation.
  @param AType   Type avec lequel tester la compatibilité
  @return True si les types sont compatibles, False sinon
*}
function TSepiType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := AType.FKind = FKind;
end;

{------------------}
{ Classe TSepiRoot }
{------------------}

{*
  Crée une instance de TSepiRoot
*}
constructor TSepiRoot.Create;
begin
  inherited Create(nil, '');
  FRoot := Self;
  FState := rsNormal;
  FSearchOrder := TObjectList.Create(False);
  FOnLoadUnit := nil;

  LoadUnit(SystemUnitName);
end;

{*
  [@inheritDoc]
*}
destructor TSepiRoot.Destroy;
begin
  FSearchOrder.Free;
  inherited;
end;

{*
  Nombre d'unités
  @return Nombre d'unités
*}
function TSepiRoot.GetUnitCount : integer;
begin
  Result := FChildren.Count;
end;

{*
  Tableau zero-based des unités
  @param Index   Index de l'unité à récupérer
  @return Unité à l'index spécifié
*}
function TSepiRoot.GetUnits(Index : integer) : TSepiUnitFile;
begin
  Result := TSepiUnitFile(FChildren.Objects[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRoot.ChildAdded(Child : TSepiReflectionItem);
var CurrentUnit : TSepiUnitFile;
    I : integer;
begin
  inherited;

  CurrentUnit := Child as TSepiUnitFile;
  FSearchOrder.Clear;
  FSearchOrder.Add(Self);

  FSearchOrder.Add(CurrentUnit);
  for I := CurrentUnit.FUsesList.Count-1 downto 0 do
    FSearchOrder.Add(CurrentUnit.FUsesList.Objects[I]);
  for I := 0 to ChildCount-1 do
    if FSearchOrder.IndexOf(Children[I]) < 0 then
      FSearchOrder.Add(Children[I]);
end;

{*
  Charge une unité
  @param UnitName   Nom de l'unité à charger
  @return Unité chargée
*}
function TSepiRoot.LoadUnit(const UnitName : string) : TSepiUnitFile;
var ImportFunc : TSepiImportUnitFunc;
begin
  Result := TSepiUnitFile(GetChild(UnitName));

  if Result = nil then
  begin
    ImportFunc := SepiImportedUnit(UnitName);
    if Assigned(ImportFunc) then
      Result := ImportFunc(Self);

    if (Result = nil) and Assigned(OnLoadUnit) then
      Result := OnLoadUnit(Self, UnitName);
  end;

  if Result = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  inc(Result.FRefCount);
end;

{*
  Décharge une unité
  @param UnitName   Nom de l'unité à charger
*}
procedure TSepiRoot.UnloadUnit(const UnitName : string);
var SepiUnit : TSepiUnitFile;
begin
  if State = rsDestroying then exit;

  SepiUnit := TSepiUnitFile(GetChild(UnitName));
  if SepiUnit = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  dec(SepiUnit.FRefCount);
  if SepiUnit.FRefCount = 0 then
  begin
    Assert(FChildren.IndexOfObject(SepiUnit) = FChildren.Count-1);
    SepiUnit.Free;
  end;
end;

{*
  Cherche un type enregistré à partir de ses informations de type
  @param TypeInfo   Informations de type du type recherché
  @return Le type correspondant (nil si non trouvé)
*}
function TSepiRoot.GetType(TypeInfo : PTypeInfo) : TSepiType;
var TypeName : string;
    I : integer;
    Item : TSepiReflectionItem;
    First : TSepiType;
begin
  if TypeInfo = nil then
  begin
    Result := nil;
    exit;
  end;

  First := nil;

  TypeName := AnsiReplaceStr(TypeInfo.Name, '.', '$$');
  for I := 0 to FSearchOrder.Count-1 do
  begin
    Item := TSepiReflectionItem(FSearchOrder[I]).GetChild(TypeName);

    if Item is TSepiType then
    begin
      if TSepiType(Item).TypeInfo = TypeInfo then
      begin
        Result := TSepiType(Item);
        exit;
      end else
      begin
        if First = nil then
          First := TSepiType(Item);
      end;
    end;
  end;

  Result := First;
end;

{*
  Cherche un type enregistré à partir de son nom
  @param TypeName   Nom du type recherché
  @return Le type correspondant (ou nil si non trouvé)
*}
function TSepiRoot.GetType(const TypeName : string) : TSepiType;
var I : integer;
    Item : TSepiReflectionItem;
begin
  if TypeName = '' then
  begin
    Result := nil;
    exit;
  end;

  for I := 0 to FSearchOrder.Count-1 do
  begin
    Item := TSepiReflectionItem(FSearchOrder[I]).GetChild(TypeName);
    if Item is TSepiType then
    begin
      Result := TSepiType(Item);
      exit;
    end;
  end;

  Result := nil;
end;

{*
  Trouve un type enregistré à partir de ses informations de type
  @param TypeInfo   Informations de type du type recherché
  @return Le type correspondant aux informations de type données
  @throw ESepiItemNotFoundError Aucun type enregistré correspondant
*}
function TSepiRoot.FindType(TypeInfo : PTypeInfo) : TSepiType;
begin
  if TypeInfo = nil then Result := nil else
  begin
    Result := GetType(TypeInfo);
    if Result = nil then
      raise ESepiItemNotFoundError.CreateFmt(SSepiObjectNotFound,
        [TypeInfo.Name]);
  end;
end;

{*
  Trouve un type enregistré à partir de son nom
  @param TypeName   Nom du type recherché
  @return Le type correspondant au nom donné
  @throw ESepiItemNotFoundError Aucun type enregistré correspondant
*}
function TSepiRoot.FindType(const TypeName : string) : TSepiType;
begin
  if TypeName = '' then Result := nil else
  begin
    Result := GetType(TypeName);
    if Result = nil then
      raise ESepiItemNotFoundError.CreateFmt(SSepiObjectNotFound, [TypeName]);
  end;
end;

{----------------------}
{ Classe TSepiUnitFile }
{----------------------}

{*
  Charge une unité depuis un flux
*}
constructor TSepiUnitFile.Load(AOwner : TSepiReflectionItem; Stream : TStream);
var UsesCount, RefCount, I, J : integer;
    Str : string;
begin
  Stream.ReadBuffer(UsesCount, 4);
  SetLength(FReferences, UsesCount+1);
  FillChar(FReferences[0], 4*(UsesCount+1), 0);

  try
    // Loading uses and setting up the references lists
    FUsesList := TStringList.Create;
    for I := 0 to UsesCount do
    begin
      if I > 0 then
      begin
        Str := ReadStrFromStream(Stream);
        FUsesList.AddObject(Str, TSepiRoot(AOwner).LoadUnit(Str));
      end;

      FReferences[I] := TStringList.Create;
      Stream.ReadBuffer(RefCount, 4);
      for J := 0 to RefCount-1 do
        FReferences[I].Add(ReadStrFromStream(Stream));
    end;

    // Now, you can add yourself to the root children
    inherited;

    FOwningUnit := Self;
    FCurrentVisibility := mvPublic;

    LoadChildren(Stream);

    Loaded;
  finally
    for I := 0 to UsesCount do
      if Assigned(FReferences[I]) then
        FReferences[I].Free;
    SetLength(FReferences, 0);
  end;
end;

{*
  Crée une nouvelle unité
  @param AOwner   Propriétaire de l'unité (la racine)
  @param AName    Nom de l'unité
*}
constructor TSepiUnitFile.Create(AOwner : TSepiReflectionItem;
  const AName : string; const AUses : array of string);
begin
  Assert(AOwner is TSepiRoot);

  FOwner := AOwner;
  FRefCount := 0;

  FUsesList := TStringList.Create;
  if not AnsiSameText(AName, SystemUnitName) then
    AddUses(TSepiRoot(AOwner).LoadUnit(SystemUnitName));
  MoreUses(AUses);

  inherited Create(AOwner, AName);

  FOwningUnit := Self;
  FCurrentVisibility := mvPublic;
end;

{*
  [@inheritDoc]
*}
destructor TSepiUnitFile.Destroy;
var I : integer;
begin
  inherited;

  if Assigned(FUsesList) and Assigned(Root) then
  begin
    for I := FUsesList.Count-1 downto 0 do
      Root.UnloadUnit(FUsesList[I]);
    FUsesList.Free;
  end;
end;

{*
  Ajoute une unité aux uses de cette unité
  @param AUnit   Unité à ajouter aux uses
  @return Index de la nouvelle unité dans les uses
*}
function TSepiUnitFile.AddUses(AUnit : TSepiUnitFile) : integer;
begin
  if AUnit = Self then Result := -1 else
  begin
    Result := FUsesList.IndexOfObject(AUnit);
    if Result < 0 then
    begin
      Result := FUsesList.AddObject(AUnit.Name, AUnit);

      if Length(FReferences) > 0 then // saving
      begin
        // Yes, this is ugly programming, sorry :-(
        SetLength(FReferences, Result+2);
        FReferences[Result+1] := TStringList.Create;
      end;
    end;
  end;
end;

{*
  Change la visibilité courante
  @param Value   Nouvelle visibilité
*}
procedure TSepiUnitFile.SetCurrentVisibility(Value : TMemberVisibility);
begin
  if Value in [mvPublic, mvPublished] then
    FCurrentVisibility := mvPublic
  else
    FCurrentVisibility := mvPrivate;
end;

{*
  Nombre d'unités utilisées
  @return Nombre d'unité utilisées
*}
function TSepiUnitFile.GetUsedUnitCount : integer;
begin
  Result := FUsesList.Count;
end;

{*
  Tableau zero-based des unités utilisées
  @param Index   Index de l'unité utilisée
  @return Unité utilisée dont l'index est spécifié
*}
function TSepiUnitFile.GetUsedUnits(Index : integer) : TSepiUnitFile;
begin
  Result := TSepiUnitFile(FUsesList.Objects[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnitFile.Save(Stream : TStream);
begin
  inherited;
  SaveChildren(Stream);
end;

{*
  Ajoute des uses à l'unité
  Cette méthode ne peut être appelée que pour une unité en cours de
  construction.
  @param AUses   Uses à ajouter
*}
procedure TSepiUnitFile.MoreUses(const AUses : array of string);
var I : integer;
begin
  for I := Low(AUses) to High(AUses) do
    if FUsesList.IndexOf(AUses[I]) < 0 then
      AddUses(TSepiRoot(Owner).LoadUnit(AUses[I]));
end;

{*
  Complète l'unité créée
*}
procedure TSepiUnitFile.Complete;
begin
  Assert(State = rsConstructing);
  Loaded;
end;

{*
  Enregistre l'unité dans un flux
*}
procedure TSepiUnitFile.SaveToStream(Stream : TStream);
var UsesCount, RefCount, I, J : integer;
    RefList : TStrings;
begin
  UsesCount := FUsesList.Count;
  SetLength(FReferences, UsesCount+1);
  FillChar(FReferences[0], 4*(UsesCount+1), 0);
  try
    // Listing the references
    for I := 0 to UsesCount do
      FReferences[I] := TStringList.Create;
    ListReferences;

    // Writing the references lists to stream
    UsesCount := FUsesList.Count; // this could have changed meanwhile
    Stream.WriteBuffer(UsesCount, 4);
    for I := 0 to UsesCount do
    begin
      if I > 0 then
        WriteStrToStream(Stream, FUsesList[I-1]);

      RefList := FReferences[I];
      RefCount := RefList.Count;
      Stream.WriteBuffer(RefCount, 4);

      for J := 0 to RefCount-1 do
        WriteStrToStream(Stream, RefList[J]);
    end;

    // Actually saving the unit
    Save(Stream);
  finally
    for I := 0 to UsesCount do
      if Assigned(FReferences[I]) then
        FReferences[I].Free;
    SetLength(FReferences, 0);
  end;
end;

{*
  Charge l'unité depuis un flux
  @param AOwner             Propriétaire de l'unité
  @param Stream             Flux depuis lequel charger l'unité
  @param AOnGetMethodCode   Méthode de call-back pour récupérer le code d'une
                            méthode
*}
class function TSepiUnitFile.LoadFromStream(AOwner : TSepiReflectionItem;
  Stream : TStream;
  const AOnGetMethodCode : TGetMethodCodeEvent = nil) : TSepiUnitFile;
begin
  Result := TSepiUnitFile(NewInstance);
  Result.FOnGetMethodCode := AOnGetMethodCode;
  Result.Load(AOwner, Stream);
  Result.FOnGetMethodCode := nil;
end;

{*
  Lit une référence depuis un flux
  Cette méthode ne peut être appelée que depuis le constructeur Load des Items
  contenus dans cette unité. À tout autre moment, c'est la violation d'accès
  assurée.
  @param Stream   Flux dans lequel écrire la référence
  @param Ref      Variable de type TSepiReflectionItem où stocker la référence
*}
procedure TSepiUnitFile.ReadRef(Stream : TStream; out Ref);
var UnitIndex, RefIndex : integer;
    RefList : TStrings;
begin
  // Reading unit index and checking for nil reference
  UnitIndex := 0;
  Stream.ReadBuffer(UnitIndex, 2);
  if UnitIndex = $FFFF then
  begin
    TObject(Ref) := nil;
    exit;
  end;

  // Reading reference index
  Stream.ReadBuffer(RefIndex, 4);
  RefList := FReferences[UnitIndex];

  TObject(Ref) := RefList.Objects[RefIndex];

  if TObject(Ref) = nil then
  begin
    if UnitIndex = 0 then // local reference
      TObject(Ref) := FindChild(RefList[RefIndex])
    else // remote reference
      TObject(Ref) := TSepiUnitFile(FUsesList.Objects[UnitIndex-1])
        .FindChild(RefList[RefIndex]);
    RefList.Objects[RefIndex] := TObject(Ref);
  end;
end;

{*
  Ajoute une référence qui va devoir être enregistrée
  Cette méthode ne peut être appelée que depuis la méthode ListReferences des
  Items contenus dans cette unité. À tout autre moment, c'est la violation
  d'accès assurée.
  @param Ref   Référence à ajouter
*}
procedure TSepiUnitFile.AddRef(Ref : TSepiReflectionItem);
var UnitIndex : integer;
    RefList : TStrings;
begin
  if Ref = nil then exit;

  UnitIndex := AddUses(Ref.OwningUnit)+1;
  RefList := FReferences[UnitIndex];

  if RefList.IndexOfObject(Ref) < 0 then
    RefList.AddObject(Copy(
      Ref.GetFullName, Length(Ref.OwningUnit.Name)+2, MaxInt), Ref);
end;

{*
  Écrit une référence dans un flux
  Toute référence à écrire doit être ajoutée au moyen de la méthode AddRef dans
  la méthode ListReferences de son référenceur.
  Cette méthode ne peut être appelée que depuis la méthode Save des Items
  contenus dans cette unité. À tout autre moment, c'est la violation d'accès
  assurée.
  @param Stream   Flux dans lequel écrire la référence
  @param Ref      Référence à écrire
*}
procedure TSepiUnitFile.WriteRef(Stream : TStream; Ref : TSepiReflectionItem);
var UnitIndex, RefIndex : integer;
begin
  if Ref = nil then
  begin
    UnitIndex := $FFFF;
    Stream.WriteBuffer(UnitIndex, 2);
  end else
  begin
    UnitIndex := FUsesList.IndexOfObject(Ref.OwningUnit)+1;
    RefIndex := FReferences[UnitIndex].IndexOfObject(Ref);
    Assert(RefIndex >= 0);

    Stream.WriteBuffer(UnitIndex, 2);
    Stream.WriteBuffer(RefIndex, 4);
  end;
end;

{-----------------------}
{ Classe TSepiTypeAlias }
{-----------------------}

{*
  Charge un alias de type depuis un flux
*}
constructor TSepiTypeAlias.Load(AOwner : TSepiReflectionItem; Stream : TStream);
begin
  inherited;
  OwningUnit.ReadRef(Stream, FDest);
end;

{*
  Crée un nouvel alias de type
  @param AOwner   Propriétaire de l'alias de type
  @param AName    Nom de l'alias de type
  @param ADest    Destination de l'alias
*}
constructor TSepiTypeAlias.Create(AOwner : TSepiReflectionItem;
  const AName : string; ADest : TSepiType);
begin
  inherited Create(AOwner, AName);
  FDest := ADest;
end;

{*
  Crée un nouvel alias de type
  @param AOwner   Propriétaire de l'alias de type
  @param AName    Nom de l'alias de type
  @param ADest    RTTI de la destination de l'alias
*}
constructor TSepiTypeAlias.Create(AOwner : TSepiReflectionItem;
  const AName : string; ADest : PTypeInfo);
begin
  Create(AOwner, AName, AOwner.Root.FindType(ADest));
end;

{*
  Crée un nouvel alias de type
  @param AOwner   Propriétaire de l'alias de type
  @param AName    Nom de l'alias de type
  @param ADest    Nom de la destination de l'alias
*}
constructor TSepiTypeAlias.Create(AOwner : TSepiReflectionItem;
  const AName, ADest : string);
begin
  Create(AOwner, AName, AOwner.Root.FindType(ADest));
end;

{*
  [@inheritDoc]
*}
procedure TSepiTypeAlias.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FDest);
end;

{*
  [@inheritDoc]
*}
procedure TSepiTypeAlias.Save(Stream : TStream);
begin
  inherited;
  OwningUnit.WriteRef(Stream, FDest);
end;

{----------------------}
{ Classe TSepiConstant }
{----------------------}

{*
  Charge une constante depuis un flux
*}
constructor TSepiConstant.Load(AOwner : TSepiReflectionItem; Stream : TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FType);
  GetMem(FValuePtr, FType.Size);

  if FType is TSepiStringType then
  begin
    FStrValue := ReadStrFromStream(Stream);
    PPointer(FValuePtr)^ := Pointer(FStrValue);
  end else
    Stream.ReadBuffer(FValuePtr^, FType.Size);
end;

{*
  Crée une nouvelle vraie constante
  @param AOwner   Propriétaire de la constante
  @param AName    Nom de la constante
  @param AValue   Valeur de la constante
  @param AType    Type de la constante
*}
constructor TSepiConstant.Create(AOwner : TSepiReflectionItem;
  const AName : string; const AValue : Variant; AType : TSepiType);
begin
  inherited Create(AOwner, AName);

  FType := AType;
  GetMem(FValuePtr, FType.Size);

  if FType is TSepiStringType then
  begin
    FStrValue := AValue;
    PPointer(FValuePtr)^ := Pointer(FStrValue);
  end else
    Move(TVarData(AValue).VAny, FValuePtr^, FType.Size);
end;

{*
  Crée une nouvelle vraie constante
  @param AOwner      Propriétaire de la constante
  @param AName       Nom de la constante
  @param AValue      Valeur de la constante
  @param ATypeInfo   RTTI du type de la constante (déterminé par VType si nil)
*}
constructor TSepiConstant.Create(AOwner : TSepiReflectionItem;
  const AName : string; const AValue : Variant; ATypeInfo : PTypeInfo = nil);
begin
  if ATypeInfo = nil then
  begin
    case VarType(AValue) of
      varSmallint : ATypeInfo := System.TypeInfo(Smallint);
      varInteger  : ATypeInfo := System.TypeInfo(Integer);
      varSingle   : ATypeInfo := System.TypeInfo(Single);
      varDouble   : ATypeInfo := System.TypeInfo(Double);
      varCurrency : ATypeInfo := System.TypeInfo(Currency);
      varDate     : ATypeInfo := System.TypeInfo(TDateTime);
      varError    : ATypeInfo := System.TypeInfo(HRESULT);
      varBoolean  : ATypeInfo := System.TypeInfo(Boolean);
      varShortInt : ATypeInfo := System.TypeInfo(ShortInt);
      varByte     : ATypeInfo := System.TypeInfo(Byte);
      varWord     : ATypeInfo := System.TypeInfo(Word);
      varLongWord : ATypeInfo := System.TypeInfo(LongWord);
      varInt64    : ATypeInfo := System.TypeInfo(Int64);

      varOleStr, varStrArg, varString : ATypeInfo := System.TypeInfo(string);

      else raise ESepiBadConstTypeError.CreateFmt(
        SSepiBadConstType, [VarType(AValue)]);
    end;
  end;

  Create(AOwner, AName, AValue, AOwner.Root.FindType(ATypeInfo));
end;

{*
  Crée une nouvelle vraie constante
  @param AOwner      Propriétaire de la constante
  @param AName       Nom de la constante
  @param AValue      Valeur de la constante
  @param ATypeName   Nom du type de la constante
*}
constructor TSepiConstant.Create(AOwner : TSepiReflectionItem;
  const AName : string; const AValue : Variant; const ATypeName : string);
begin
  Create(AOwner, AName, AValue, AOwner.Root.FindType(ATypeName));
end;

{*
  [@inheritDoc]
*}
destructor TSepiConstant.Destroy;
begin
  FreeMem(FValuePtr);
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiConstant.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiConstant.Save(Stream : TStream);
begin
  inherited;
  OwningUnit.WriteRef(Stream, FType);

  if FType is TSepiStringType then
    WriteStrToStream(Stream, FStrValue)
  else
    Stream.WriteBuffer(FValuePtr^, FType.Size);
end;

{----------------------}
{ Classe TSepiVariable }
{----------------------}

{*
  Charge une variable ou une constante typée depuis un flux
*}
constructor TSepiVariable.Load(AOwner : TSepiReflectionItem; Stream : TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FType);

  GetMem(FValue, FType.Size);
  FOwnValue := True;

  if FType is TSepiStringType then
  begin
    Pointer(FValue^) := nil;
    string(FValue^) := ReadStrFromStream(Stream);
  end else
    Stream.ReadBuffer(FValue^, FType.Size);
end;

{*
  Importe une variable ou constante typée native
  @param AOwner     Propriétaire de la variable
  @param AName      Nom de la variable
  @param AValue     Valeur de la variable
  @param AType      Type de la variable
  @param AIsConst   Indique si c'est une constante typée
*}
constructor TSepiVariable.Create(AOwner : TSepiReflectionItem;
  const AName : string; const AValue; AType : TSepiType;
  AIsConst : boolean = False);
begin
  inherited Create(AOwner, AName);

  FIsConst := AIsConst;
  FType := AType;
  FValue := @AValue;
  FOwnValue := False;
end;

{*
  Importe une variable ou constante typée native
  @param AOwner      Propriétaire de la variable
  @param AName       Nom de la variable
  @param AValue      Valeur de la variable
  @param ATypeInfo   RTTI du type de la variable
  @param AIsConst    Indique si c'est une constante typée
*}
constructor TSepiVariable.Create(AOwner : TSepiReflectionItem;
  const AName : string; const AValue; ATypeInfo : PTypeInfo;
  AIsConst : boolean = False);
begin
  Create(AOwner, AName, AValue, AOwner.Root.FindType(ATypeInfo), AIsConst);
end;

{*
  Importe une variable ou constante typée native
  @param AOwner      Propriétaire de la variable
  @param AName       Nom de la variable
  @param AValue      Valeur de la variable
  @param ATypeName   Nom du type de la variable
  @param AIsConst    Indique si c'est une constante typée
*}
constructor TSepiVariable.Create(AOwner : TSepiReflectionItem;
  const AName : string; const AValue; const ATypeName : string;
  AIsConst : boolean = False);
begin
  Create(AOwner, AName, AValue, AOwner.Root.FindType(ATypeName), AIsConst);
end;

{*
  Crée une nouvelle variable ou constante typée
  @param AOwner     Propriétaire de la variable
  @param AName      Nom de la variable
  @param AType      Type de la variable
  @param AIsConst   Indique si c'est une constante typée
*}
constructor TSepiVariable.Create(AOwner : TSepiReflectionItem;
  const AName : string; AType : TSepiType; AIsConst : boolean = False);
begin
  inherited Create(AOwner, AName);

  FIsConst := AIsConst;
  FType := AType;

  GetMem(FValue, FType.Size);
  FillChar(FValue^, FType.Size, 0);
  FOwnValue := True;
end;

{*
  Crée une nouvelle variable ou constante typée
  @param AOwner      Propriétaire de la variable
  @param AName       Nom de la variable
  @param ATypeInfo   RTTI du type de la variable
  @param AIsConst    Indique si c'est une constante typée
*}
constructor TSepiVariable.Create(AOwner : TSepiReflectionItem;
  const AName : string; ATypeInfo : PTypeInfo; AIsConst : boolean = False);
begin
  Create(AOwner, AName, AOwner.Root.FindType(ATypeInfo), AIsConst);
end;

{*
  Crée une nouvelle variable ou constante typée
  @param AOwner      Propriétaire de la variable
  @param AName       Nom de la variable
  @param ATypeName   Nom du type de la variable
  @param AIsConst    Indique si c'est une constante typée
*}
constructor TSepiVariable.Create(AOwner : TSepiReflectionItem;
  const AName : string; const ATypeName : string; AIsConst : boolean = False);
begin
  Create(AOwner, AName, AOwner.Root.FindType(ATypeName), AIsConst);
end;

{*
  [@inheritDoc]
*}
destructor TSepiVariable.Destroy;
begin
  if FOwnValue then
    FreeMem(FValue);

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiVariable.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiVariable.Save(Stream : TStream);
begin
  inherited;
  OwningUnit.WriteRef(Stream, FType);

  if FType is TSepiStringType then
    WriteStrToStream(Stream, string(FValue^))
  else
    Stream.WriteBuffer(FValue^, FType.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiVariable.Destroying;
begin
  inherited;

  if FOwnValue and FType.NeedInit then
    Finalize(FValue^, FType.TypeInfo);
end;

{--------------------------}
{ Classe TSepiUnitLoadTask }
{--------------------------}

{*
  Crée une nouvelle tâche de chargement/déchargement d'une unité Sepi
  @param AOwner            Gestionnaire propriétaire
  @param ARoot             Racine Sepi
  @param AUnitName         Nom de l'unité à charger/décharger
  @param AIsLoad           True pour charger, False pour décharger
  @param AFreeOnFinished   Indique si doit se détruire automatiquement
*}
constructor TSepiUnitLoadTask.Create(AOwner : TScCustomTaskQueue;
  ARoot : TSepiRoot; const AUnitName : string; AIsLoad : boolean;
  AFreeOnFinished : boolean);
begin
  inherited Create(AOwner, AFreeOnFinished);
  FRoot := ARoot;
  FUnitName := AUnitName;
  FIsLoad := AIsLoad;
end;

{*
  Crée une nouvelle tâche de chargement/déchargement d'une unité Sepi
  @param AOwner      Gestionnaire propriétaire
  @param ARoot       Racine Sepi
  @param AUnitName   Nom de l'unité à charger/décharger
  @param AIsLoad     True pour charger, False pour décharger
*}
constructor TSepiUnitLoadTask.Create(AOwner : TScCustomTaskQueue;
  ARoot : TSepiRoot; const AUnitName : string; AIsLoad : boolean);
begin
  inherited Create(AOwner);
  FRoot := ARoot;
  FUnitName := AUnitName;
  FIsLoad := AIsLoad;
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnitLoadTask.Execute;
begin
  if IsLoad then
    Root.LoadUnit(UnitName)
  else
    Root.UnloadUnit(UnitName);
end;

{-------------------------------------}
{ Classe TSepiAsynchronousRootManager }
{-------------------------------------}

{*
  Crée un nouveau gestionnaire de racine
  @param ARoot   Racine à gérer (si nil, la racine est créée et libérée)
*}
constructor TSepiAsynchronousRootManager.Create(ARoot : TSepiRoot = nil);
begin
  inherited Create;

  FOwnsRoot := ARoot = nil;
  if FOwnsRoot then
    FRoot := TSepiRoot.Create
  else
    FRoot := ARoot;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsynchronousRootManager.Destroy;
begin
  if FOwnsRoot then
    FRoot.Free;

  inherited;
end;

{*
  Demande le chargement d'une unité
  @param UnitName   Unité à charger
*}
procedure TSepiAsynchronousRootManager.LoadUnit(const UnitName : string);
begin
  TSepiUnitLoadTask.Create(Self, Root, UnitName, True);
end;

{*
  Demande le déchargement d'une unité
  @param UnitName   Unité à charger
*}
procedure TSepiAsynchronousRootManager.UnloadUnit(const UnitName : string);
begin
  TSepiUnitLoadTask.Create(Self, Root, UnitName, False);
end;

initialization
  SepiRegisterReflectionItemClasses([
    TSepiUnitFile, TSepiTypeAlias, TSepiConstant, TSepiVariable
  ]);
finalization
  SepiReflectionItemClasses.Free;
  SepiImportedUnits.Free;
end.

