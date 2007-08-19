{*
  D�finit les classes de gestion des meta-unit�s
  @author sjrd
  @version 1.0
*}
unit SepiMetaUnits;

interface

uses
  Windows, SysUtils, Classes, Contnrs, SysConst, RTLConsts, SepiCore, ScUtils,
  IniFiles, TypInfo, Variants, StrUtils, ScLists, ScStrUtils, ScDelphiLanguage,
  ScSyncObjs, ScCompilerMagic, SepiBinariesConsts;

type
  TSepiMeta = class;
  TSepiMetaRoot = class;
  TSepiMetaUnit = class;
  TSepiAsynchronousRootManager = class;

  {*
    �tat d'un meta
  *}
  TSepiMetaState = (msNormal, msConstructing, msLoading, msDestroying);

  {*
    Visibilit� d'un membre d'une classe, d'un objet, ou d'une unit�
  *}
  TMemberVisibility = (mvStrictPrivate, mvPrivate, mvStrictProtected,
    mvProtected, mvPublic, mvPublished);

  {*
    Type de l'�v�nement OnLoadUnit de TSepiMetaRoot
    @param Sender     Racine Sepi qui demande le chargement d'une unit�
    @param UnitName   Nom de l'unit� � charger
    @return L'unit� charg�e, ou nil si l'unit� n'est pas trouv�e
  *}
  TSepiLoadUnitEvent = function(Sender : TSepiMetaRoot;
    const UnitName : string) : TSepiMetaUnit of object;

  {*
    Type de l'�v�nement OnGetMethodCode de TSepiMetaUnit
    @param Sender   M�thode d�clenchant l'�v�nement (toujours TSepiMetaMethod)
    @param Code     Adresse de code de la m�thode
  *}
  TGetMethodCodeEvent = procedure(Sender : TObject;
    var Code : Pointer) of object;

  {*
    D�clench�e si l'on tente de recr�er un meta (second appel au constructeur)
    @author sjrd
    @version 1.0
  *}
  ESepiMetaAlreadyCreated = class(ESepiError);

  {*
    D�clench�e lorsque la recherche d'un meta s'est sold�e par un �chec
    @author sjrd
    @version 1.0
  *}
  ESepiMetaNotFoundError = class(ESepiError);

  {*
    D�clench�e lorsque la recherche d'une unit� s'est sold�e par un �chec
    @author sjrd
    @version 1.0
  *}
  ESepiUnitNotFoundError = class(ESepiMetaNotFoundError);

  {*
    D�clench�e si l'on tente de cr�er une constante avec un mauvais type
    @author sjrd
    @version 1.0
  *}
  ESepiBadConstTypeError = class(ESepiError);

  {*
    Liste de meta
    @author sjrd
    @version 1.0
  *}
  TSepiMetaList = class(THashedStringList)
  private
    function GetMetas(Index : integer) : TSepiMeta;
    procedure SetMetas(Index : integer; Value : TSepiMeta);
    function GetMetaFromName(const Name : string) : TSepiMeta;
    procedure SetMetaFromName(const Name : string; Value : TSepiMeta);
  protected
    procedure InsertItem(Index : Integer; const S : string;
      AObject : TObject); override;
  public
    constructor Create;

    function AddMeta(Meta : TSepiMeta) : integer;
    function IndexOfMeta(Meta : TSepiMeta) : integer;
    function Remove(Meta : TSepiMeta) : integer;

    property Metas[index : integer] : TSepiMeta
      read GetMetas write SetMetas; default;
    property MetaFromName[const Name : string] : TSepiMeta
      read GetMetaFromName write SetMetaFromName;
  end;

  {*
    Meta g�n�rique
    Les meta sont les informations statiques qui repr�sentent les unit�s
    compil�es.
    @author sjrd
    @version 1.0
  *}
  TSepiMeta = class
  private
    FIsForward : boolean;        /// True tant que le meta n'a pas �t� construit
    FState : TSepiMetaState;     /// �tat
    FOwner : TSepiMeta;          /// Propri�taire
    FRoot : TSepiMetaRoot;       /// Racine
    FOwningUnit : TSepiMetaUnit; /// Unit� contenante
    FName : string;              /// Nom
    FForwards : TStrings;        /// Liste des enfants forwards
    FChildren : TSepiMetaList;   /// Liste des enfants
    FObjResources : TObjectList; /// Liste des ressources objet
    FPtrResources : TList;       /// Liste des ressources pointeur

    procedure AddChild(Child : TSepiMeta);
    procedure RemoveChild(Child : TSepiMeta);

    function GetChildCount : integer;
    function GetChildren(Index : integer) : TSepiMeta;
  protected
    FVisibility : TMemberVisibility; /// Visibilit�

    procedure LoadChildren(Stream : TStream);
    procedure SaveChildren(Stream : TStream);

    procedure AddForward(const ChildName : string; Child : TObject);
    procedure ChildAdded(Child : TSepiMeta); virtual;
    procedure ChildRemoving(Child : TSepiMeta); virtual;

    procedure Loaded; virtual;

    procedure ListReferences; virtual;
    procedure Save(Stream : TStream); virtual;

    procedure Destroying; virtual;

    property State : TSepiMetaState read FState;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); virtual;
    constructor Create(AOwner : TSepiMeta; const AName : string);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function NewInstance : TObject; override;

    function GetFullName : string;
    function GetMeta(const Name : string) : TSepiMeta;
    function FindMeta(const Name : string) : TSepiMeta;

    procedure AddObjResource(Obj : TObject);
    procedure AddPtrResource(Ptr : Pointer);

    property IsForward : boolean read FIsForward;
    property Owner : TSepiMeta read FOwner;
    property Root : TSepiMetaRoot read FRoot;
    property OwningUnit : TSepiMetaUnit read FOwningUnit;
    property Name : string read FName;
    property Visibility : TMemberVisibility read FVisibility write FVisibility;

    property ChildCount : integer read GetChildCount;
    property Children[index : integer] : TSepiMeta read GetChildren;
  end;

  {*
    Classe de TSepiMeta
  *}
  TSepiMetaClass = class of TSepiMeta;

  {*
    Comportement d'un type lorsqu'il est pass� en param�tre
    @author sjrd
    @version 1.0
  *}
  TSepiTypeParamBehavior = record
    AlwaysByAddress : boolean; /// Le type est toujours pass� par adresse
    AlwaysByStack : boolean;   /// Le type est toujours pass� sur la pile
  end;

  {*
    Type
    @author sjrd
    @version 1.0
  *}
  TSepiType = class(TSepiMeta)
  private
    FKind : TTypeKind;         /// Type de type
    FNative : boolean;         /// Indique si le type est un type natif Delphi
    FTypeInfoLength : integer; /// Taille des RTTI cr��es (ou 0 si non cr��es)
    FTypeInfo : PTypeInfo;     /// RTTI (Runtime Type Information)
    FTypeData : PTypeData;     /// RTTD (Runtime Type Data)
    FTypeInfoRef : PPTypeInfo; /// R�f�rence aux RTTI
  protected
    FSize : integer;     /// Taille d'une variable de ce type
    FNeedInit : boolean; /// Indique si ce type requiert une initialisation

    FParamBehavior : TSepiTypeParamBehavior; /// Comportement comme param�tre

    procedure Save(Stream : TStream); override;

    procedure ForceNative(ATypeInfo : PTypeInfo = nil);
    procedure AllocateTypeInfo(TypeDataLength : integer = 0);
    procedure ExtractTypeData; virtual;

    function GetAlignment : integer; virtual;

    property TypeInfoRef : PPTypeInfo read FTypeInfoRef;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); virtual;
    constructor Clone(AOwner : TSepiMeta; const AName : string;
      Source : TSepiType); virtual;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AKind : TTypeKind);
    destructor Destroy; override;

    class function NewInstance : TObject; override;

    class function LoadFromTypeInfo(AOwner : TSepiMeta;
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
    Meta-racine
    @author sjrd
    @version 1.0
  *}
  TSepiMetaRoot = class(TSepiMeta)
  private
    FSearchOrder : TObjectList;       /// Ordre de recherche
    FOnLoadUnit : TSepiLoadUnitEvent; /// D�clench� au chargement d'une unit�

    function GetUnitCount : integer;
    function GetUnits(Index : integer) : TSepiMetaUnit;
  protected
    procedure ChildAdded(Child : TSepiMeta); override;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadUnit(const UnitName : string) : TSepiMetaUnit;
    procedure UnloadUnit(const UnitName : string);

    function GetType(TypeInfo : PTypeInfo) : TSepiType; overload;
    function GetType(const TypeName : string) : TSepiType; overload;

    function FindType(TypeInfo : PTypeInfo) : TSepiType; overload;
    function FindType(const TypeName : string) : TSepiType; overload;

    property UnitCount : integer read GetUnitCount;
    property Units[index : integer] : TSepiMetaUnit read GetUnits;

    property OnLoadUnit : TSepiLoadUnitEvent read FOnLoadUnit write FOnLoadUnit;
  end;

  {*
    Meta-unit�
    @author sjrd
    @version 1.0
  *}
  TSepiMetaUnit = class(TSepiType)
  private
    /// D�clench� pour chaque m�thode au chargement, pour obtenir son code
    FOnGetMethodCode : TGetMethodCodeEvent;

    FRefCount : integer;                    /// Compteur de r�f�rences
    FUsesList : TStrings;                   /// Liste des uses
    FCurrentVisibility : TMemberVisibility; /// Visibilit� courante

    FReferences : array of TStrings; /// R�f�rences en chargement/sauvegarde

    function AddUses(AUnit : TSepiMetaUnit) : integer;

    procedure SetCurrentVisibility(Value : TMemberVisibility);

    function GetUsedUnitCount : integer;
    function GetUsedUnits(Index : integer) : TSepiMetaUnit;
  protected
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const AUses : array of string);
    destructor Destroy; override;

    procedure MoreUses(const AUses : array of string);

    procedure Complete;

    procedure SaveToStream(Stream : TStream);
    class function LoadFromStream(AOwner : TSepiMeta; Stream : TStream;
      const AOnGetMethodCode : TGetMethodCodeEvent = nil) : TSepiMetaUnit;

    procedure ReadRef(Stream : TStream; out Ref);
    procedure AddRef(Ref : TSepiMeta);
    procedure WriteRef(Stream : TStream; Ref : TSepiMeta);

    property CurrentVisibility : TMemberVisibility
      read FCurrentVisibility write SetCurrentVisibility;

    property UsedUnitCount : integer read GetUsedUnitCount;
    property UsedUnits[index : integer] : TSepiMetaUnit read GetUsedUnits;

    property OnGetMethodCode : TGetMethodCodeEvent read FOnGetMethodCode;
  end;

  {*
    Alias de type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeAlias = class(TSepiMeta)
  private
    FDest : TSepiType; /// Type destination
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      ADest : TSepiType); overload;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      ADest : PTypeInfo); overload;
    constructor Create(AOwner : TSepiMeta;
      const AName, ADest : string); overload;

    property Dest : TSepiType read FDest;
  end;

  {*
    Constante
    @author sjrd
    @version 1.0
  *}
  TSepiConstant = class(TSepiMeta)
  private
    FType : TSepiType;   /// Type de la constante
    FValuePtr : Pointer; /// Pointeur sur la valeur
    FStrValue : string;  /// Contenu de la valeur de type cha�ne
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const AValue : Variant; AType : TSepiType); overload;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const AValue : Variant; ATypeInfo : PTypeInfo = nil); overload;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const AValue : Variant; const ATypeName : string); overload;
    destructor Destroy; override;

    property ConstType : TSepiType read FType;
    property ValuePtr : Pointer read FValuePtr;
  end;

  {*
    Variable (ou constante typ�e)
    @author sjrd
    @version 1.0
  *}
  TSepiVariable = class(TSepiMeta)
  private
    FIsConst : boolean;  /// Indique si la variable est une constante typ�e
    FType : TSepiType;   /// Type de la constante
    FValue : Pointer;    /// Pointeur sur la variable
    FOwnValue : boolean; /// Indique si Sepi a allou� la variable
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;

    procedure Destroying; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;

    constructor Create(AOwner : TSepiMeta; const AName : string;
      const AValue; AType : TSepiType; AIsConst : boolean = False); overload;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const AValue; ATypeInfo : PTypeInfo;
      AIsConst : boolean = False); overload;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const AValue; const ATypeName : string;
      AIsConst : boolean = False); overload;

    constructor Create(AOwner : TSepiMeta; const AName : string;
      AType : TSepiType; AIsConst : boolean = False); overload;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      ATypeInfo : PTypeInfo; AIsConst : boolean = False); overload;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const ATypeName : string; AIsConst : boolean = False); overload;

    destructor Destroy; override;

    property IsConst : boolean read FIsConst;
    property VarType : TSepiType read FType;
    property Value : Pointer read FValue;
  end;

  {*
    T�che de chargement/d�chargement d'une unit� Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiUnitLoadTask = class(TScTask)
  private
    FRoot : TSepiMetaRoot; /// Racine Sepi
    FUnitName : string;    /// Nom de l'unit� � charger/d�charger
    FIsLoad : boolean;     /// True pour charger, False pour d�charger
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner : TScCustomTaskQueue; ARoot : TSepiMetaRoot;
      const AUnitName : string; AIsLoad : boolean;
      AFreeOnFinished : boolean); overload;
    constructor Create(AOwner : TScCustomTaskQueue; ARoot : TSepiMetaRoot;
      const AUnitName : string; AIsLoad : boolean); overload;

    property Root : TSepiMetaRoot read FRoot;
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
    FOwnsRoot : boolean;   /// Indique si l'on poss�de la racine
    FRoot : TSepiMetaRoot; /// Meta-racine g�r�e
  public
    constructor Create(ARoot : TSepiMetaRoot = nil);
    destructor Destroy; override;

    procedure LoadUnit(const UnitName : string);
    procedure UnloadUnit(const UnitName : string);

    property Root : TSepiMetaRoot read FRoot;
  end;

  {*
    Type de routine call-back pour l'import d'une unit� sous Sepi
  *}
  TSepiImportUnitFunc = function(Root : TSepiMetaRoot) : TSepiMetaUnit;

procedure SepiRegisterMetaClasses(const MetaClasses : array of TSepiMetaClass);

procedure SepiRegisterImportedUnit(const UnitName : string;
  ImportFunc : TSepiImportUnitFunc);
procedure SepiUnregisterImportedUnit(const UnitName : string);

const {don't localize}
  SystemUnitName = 'System'; /// Nom de l'unit� System.pas

const
  /// Comportement par des d�faut d'un type comme param�tre
  DefaultTypeParamBehavior : TSepiTypeParamBehavior = (
    AlwaysByAddress : False; AlwaysByStack : False
  );

implementation

uses
  SepiOrdTypes, SepiStrTypes, SepiArrayTypes, SepiCompTypes, SepiImportsSystem;

var
  SepiMetaClasses : TStrings = nil;
  SepiImportedUnits : TStrings = nil;

{*
  Recense des classes de meta
  @param MetaClasses   Classes de meta � recenser
*}
procedure SepiRegisterMetaClasses(const MetaClasses : array of TSepiMetaClass);
var I : integer;
begin
  if not Assigned(SepiMetaClasses) then
  begin
    SepiMetaClasses := TStringList.Create;
    with TStringList(SepiMetaClasses) do
    begin
      CaseSensitive := False;
      Sorted := True;
      Duplicates := dupIgnore;
    end;
  end;

  for I := Low(MetaClasses) to High(MetaClasses) do
  begin
    SepiMetaClasses.AddObject(
      MetaClasses[I].ClassName, TObject(MetaClasses[I]));
  end;
end;

{*
  Cherche une classe de meta par son nom
  La classe recherch�e doit avoir �t� recens�e au pr�alable avec
  SepiRegisterMetaClasses.
  @param MetaClassName   Nom de la classe de meta
  @return La classe de meta dont le nom correspond
  @throws EClassNotFound La classe recherch�e n'existe pas
*}
function SepiFindMetaClass(const MetaClassName : string) : TSepiMetaClass;
var Index : integer;
begin
  if not Assigned(SepiMetaClasses) then Index := -1 else
    Index := SepiMetaClasses.IndexOf(MetaClassName);
  if Index < 0 then
    EClassNotFound.CreateFmt(SClassNotFound, [MetaClassName]);

  Result := TSepiMetaClass(SepiMetaClasses.Objects[Index]);
end;

{*
  Recense une routine d'import d'unit�
  @param UnitName     Nom de l'unit�
  @param ImportFunc   Routine de call-back pour l'import de l'unit�
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
  Supprime un recensement d'import d'unit�
  @param UnitName   Nom de l'unit�
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
  Cherche une routine d'import d'une unit�
  Cette routine doit avoir �t� recens�e au pr�lable avec
  SepiRegisterImportedUnit.
  @param UnitName   Nom de l'unit�
  @return Routine de call-back pour l'import de l'unit�, ou nil si n'existe pas
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

{----------------------}
{ Classe TSepiMetaList }
{----------------------}

{*
  Cr�e une instance de TSepiMetaList
*}
constructor TSepiMetaList.Create;
begin
  inherited;
  CaseSensitive := False;
end;

{*
  Liste zero-based des metas
  @param Index   Index du meta � obtenir
  @return Meta � l'index sp�cifi�
*}
function TSepiMetaList.GetMetas(Index : integer) : TSepiMeta;
begin
  Result := TSepiMeta(Objects[Index]);
end;

{*
  Assigne la r�f�rence � un meta
  Un meta ne peut pas �tre modif� une fois assign�, il ne peut donc �tre assign�
  qu'une et une seule fois.
  @param Index   Index du meta � modifier
  @param Value   R�f�rence au nouveau meta
*}
procedure TSepiMetaList.SetMetas(Index : integer; Value : TSepiMeta);
begin
  if Assigned(Objects[Index]) then
    Error(@SSepiMetaAlreadyAssigned, Index);
  Objects[Index] := Value;
end;

{*
  Liste des metas index�e par leurs noms
  @param Name   Nom d'un meta
  @return Le meta dont le nom a �t� sp�cifi�, ou nil s'il n'existe pas
*}
function TSepiMetaList.GetMetaFromName(const Name : string) : TSepiMeta;
var Index : integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then Result := nil else
    Result := TSepiMeta(Objects[Index]);
end;

{*
  Assigne ou ajoute un meta par son nom
  Un meta ne peut pas �tre modif� une fois assign�, il ne peut donc �tre assign�
  qu'une et une seule fois.
  @param Name    Nom du meta
  @param Value   R�f�rence au meta
*}
procedure TSepiMetaList.SetMetaFromName(const Name : string; Value : TSepiMeta);
var Index : integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then AddObject(Name, Value) else
    Metas[Index] := Value;
end;

{*
  Ajoute un �l�ment dans la liste de metas
  @param Index     Index o� ajouter l'�l�ment
  @param S         Cha�ne � ajouter
  @param AObject   Objet � ajouter
*}
procedure TSepiMetaList.InsertItem(Index : Integer; const S : string;
  AObject : TObject);
begin
  if IndexOf(S) >= 0 then
    raise EListError.CreateFmt(SSepiMetaAlreadyExists, [S]);
  inherited;
end;

{*
  Ajoute un meta
  @param Meta   Meta � ajouter
  @return Index du meta nouvellement ajout�
*}
function TSepiMetaList.AddMeta(Meta : TSepiMeta) : integer;
begin
  Result := AddObject(Meta.Name, Meta);
end;

{*
  Cherche un meta dans la liste
  @param Meta   Meta � chercher
  @return L'index du meta dans la liste, ou nil s'il n'existe pas
*}
function TSepiMetaList.IndexOfMeta(Meta : TSepiMeta) : integer;
begin
  Result := IndexOf(Meta.Name);
end;

{*
  Supprime un meta de la liste
  @param Meta   Meta � supprimer
  @return L'index auquel se trouvait le meta
*}
function TSepiMetaList.Remove(Meta : TSepiMeta) : integer;
begin
  Result := IndexOfMeta(Meta);
  Delete(Result);
end;

{------------------}
{ Classe TSepiMeta }
{------------------}

{*
  Charge un meta depuis un flux
  @param AOwner   Propri�taire du meta
  @param Stream   Flux depuis lequel charger le meta
*}
constructor TSepiMeta.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  Create(AOwner, ReadStrFromStream(Stream));
  Stream.ReadBuffer(FVisibility, 1);
  FState := msLoading;
end;

{*
  Cr�e un nouveau meta
  @param AOwner   Propri�taire du meta
  @param AName    Nom du meta
*}
constructor TSepiMeta.Create(AOwner : TSepiMeta; const AName : string);
begin
  if not IsForward then
    raise ESepiMetaAlreadyCreated.CreateFmt(SSepiMetaAlreadyCreated, [Name]);

  inherited Create;
  FIsForward := False;
  FState := msConstructing;
  FOwner := AOwner;
  FName := AName;
  FVisibility := mvPublic;
  FForwards := THashedStringList.Create;
  FChildren := TSepiMetaList.Create;
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
  D�truit l'instance
*}
destructor TSepiMeta.Destroy;
var I : integer;
begin
  if State <> msDestroying then // only if an error has occured in constructor
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
  AddChild est appel�e dans le constructeur du meta enfant, et ne doit pas �tre
  appel�e ailleurs.
  @param Child   Enfant � ajouter
*}
procedure TSepiMeta.AddChild(Child : TSepiMeta);
begin
  FChildren.AddMeta(Child);
end;

{*
  Supprime un enfant
  RemoveChild est appel�e dans le destructeur du meta enfant, et ne doit pas
  �tre appel�e ailleurs.
  @param Child   Enfant � supprimer
*}
procedure TSepiMeta.RemoveChild(Child : TSepiMeta);
var Index : integer;
begin
  if State <> msDestroying then
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
function TSepiMeta.GetChildCount : integer;
begin
  Result := FChildren.Count;
end;

{*
  Tableau zero-based des enfants
  @param Index   Index de l'enfant � r�cup�rer
  @return Enfant � l'index sp�cifi�
*}
function TSepiMeta.GetChildren(Index : integer) : TSepiMeta;
begin
  Result := TSepiMeta(FChildren[Index]);
end;

{*
  Charge les enfants depuis un flux
  @param Stream   Flux depuis lequel charger les enfants
*}
procedure TSepiMeta.LoadChildren(Stream : TStream);
var Count, I : integer;
    ForwardChild : TObject;
    IsForward : boolean;
begin
  // Forwards
  Stream.ReadBuffer(Count, 4);
  for I := 0 to Count-1 do
  begin
    ForwardChild := SepiFindMetaClass(ReadStrFromStream(Stream)).NewInstance;
    AddForward(ReadStrFromStream(Stream), ForwardChild);
  end;

  // Actual loading
  Stream.ReadBuffer(Count, 4);
  for I := 0 to Count-1 do
  begin
    Stream.ReadBuffer(IsForward, 1);
    if IsForward then
      FindMeta(ReadStrFromStream(Stream)).Load(Self, Stream)
    else
      SepiFindMetaClass(ReadStrFromStream(Stream)).Load(Self, Stream);
  end;
end;

{*
  Enregistre les enfants dans un flux
  @param Stream   Flux dans lequel enregistrer les enfants
*}
procedure TSepiMeta.SaveChildren(Stream : TStream);
var Count, I : integer;
    Child : TSepiMeta;
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
  @param Child       Enfant � ajouter
*}
procedure TSepiMeta.AddForward(const ChildName : string; Child : TObject);
begin
  FForwards.AddObject(ChildName, Child);
end;

{*
  Appel� lorsqu'un enfant vient d'�tre ajout�
  @param Child   Enfant qui vient d'�tre ajout�
*}
procedure TSepiMeta.ChildAdded(Child : TSepiMeta);
begin
end;

{*
  Appel� lorsqu'un enfant va �tre supprim�
  @param Child   Enfant sur le point d'�tre supprim�
*}
procedure TSepiMeta.ChildRemoving(Child : TSepiMeta);
begin
end;

{*
  Appel� lorsque l'unit� contenante est compl�tement charg�e/cr��e
*}
procedure TSepiMeta.Loaded;
var I : integer;
begin
  // Already created children
  for I := 0 to FChildren.Count-1 do
    FChildren[I].Loaded;

  // Forward declarations which have not been created yet
  for I := 0 to FForwards.Count-1 do
    with TSepiMeta(FForwards.Objects[I]) do
      if IsForward then Loaded;

  // Changing the state
  FState := msNormal;
end;

{*
  Liste les r�f�rences aupr�s de l'unit� contenante, pour pr�parer la sauvegarde
*}
procedure TSepiMeta.ListReferences;
var I : integer;
begin
  for I := 0 to FChildren.Count-1 do
    FChildren[I].ListReferences;
end;

{*
  Enregistre le meta dans un flux
  @param Stream   Flux dans lequel enregistrer le meta
*}
procedure TSepiMeta.Save(Stream : TStream);
begin
  WriteStrToStream(Stream, Name);
  Stream.WriteBuffer(FVisibility, 1);
end;

{*
  Appel� lorsque l'environnement Sepi est sur le point d'�tre d�truit
*}
procedure TSepiMeta.Destroying;
var I : integer;
begin
  FState := msDestroying;
  if Assigned(FChildren) then // could not be if an error occured in constructor
    for I := 0 to ChildCount-1 do
      Children[I].Destroying;
end;

{*
  Appel� juste apr�s l'ex�cution du dernier constructeur
*}
procedure TSepiMeta.AfterConstruction;
begin
  inherited;
  if Assigned(Owner) then
    Owner.ChildAdded(Self);
end;

{*
  Appel� juste avant l'ex�cution du premier destructeur
*}
procedure TSepiMeta.BeforeDestruction;
begin
  if FState <> msDestroying then
    Destroying;

  inherited;

  if Assigned(Owner) then
    Owner.ChildRemoving(Self);
end;

{*
  Cr�e une nouvelle instance de TSepiMeta
  @return Instance cr��e
*}
class function TSepiMeta.NewInstance : TObject;
begin
  Result := inherited NewInstance;
  TSepiMeta(Result).FIsForward := True;
end;

{*
  Nom qualifi� du meta, depuis l'unit� contenante
  @return Nom qualifi� du meta
*}
function TSepiMeta.GetFullName : string;
begin
  if Assigned(FOwner) and (FOwner.Name <> '') then
    Result := FOwner.GetFullName+'.'+Name
  else
    Result := Name;
end;

{*
  Cherche un meta enfant
  @param Name   Nom du meta � trouver
  @return Le meta correspondant, ou nil s'il n'a pas �t� trouv�
*}
function TSepiMeta.GetMeta(const Name : string) : TSepiMeta;
var I : integer;
    MetaName, Field : string;
begin
  if not SplitToken(Name, '.', MetaName, Field) then
    Field := '';
  Result := FChildren.MetaFromName[MetaName];

  if (Result = nil) and (Field = '') then
  begin
    I := FForwards.IndexOf(MetaName);
    if I >= 0 then
      Result := TSepiMeta(FForwards.Objects[I]);
  end;

  if not Assigned(Result) then exit;
  while Result is TSepiTypeAlias do
    Result := TSepiTypeAlias(Result).Dest;
  if Field <> '' then
    Result := Result.GetMeta(Field);
end;

{*
  Cherche un meta enfant
  @param Name   Nom du meta � trouver
  @return Le meta correspondant
  @throws ESepiMetaNotFoundError Le meta n'a pas �t� trouv�
*}
function TSepiMeta.FindMeta(const Name : string) : TSepiMeta;
begin
  Result := GetMeta(Name);
  if (Result = nil) and (Name <> '') then
    raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound, [Name]);
end;

{*
  Ajoute un objet aux ressources du meta
  Tous les objets ajout�s aux ressources du meta seront lib�r�s lorsque le meta
  sera lib�r� lui-m�me.
  @param Obj   Objet � ajouter aux ressources
*}
procedure TSepiMeta.AddObjResource(Obj : TObject);
begin
  FObjResources.Add(Obj);
end;

{*
  Ajoute un pointeur aux ressources du meta
  Tous les pointeurs ajout�s aux ressources du meta seront lib�r�s lorsque le
  meta sera lib�r� lui-m�me.
  @param Ptr   Pointeur � ajouter aux ressources
*}
procedure TSepiMeta.AddPtrResource(Ptr : Pointer);
begin
  FPtrResources.Add(Ptr);
end;

{------------------}
{ Classe TSepiType }
{------------------}

{*
  Recense un type natif
  @param AOwner      Propri�taire du type
  @param ATypeInfo   RTTI du type � recenser
*}
constructor TSepiType.RegisterTypeInfo(AOwner : TSepiMeta;
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
  @param AOwner   Propri�taire du type
  @param AName    Nom du type
  @param Source   Type � cloner
*}
constructor TSepiType.Clone(AOwner : TSepiMeta; const AName : string;
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
  @param AOwner   Propri�taire du type
  @param Stream   Flux depuis lequel charger le type
*}
constructor TSepiType.Load(AOwner : TSepiMeta; Stream : TStream);
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
  Cr�e un nouveau type
  @param AOwner   Propri�taire du type
  @param AName    Nom du type
  @param AKind    Type de type
*}
constructor TSepiType.Create(AOwner : TSepiMeta; const AName : string;
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
  Force le type comme �tant natif, en modifiant �galement les RTTI
  Cette m�thode est utilis�e par les types record et tableau statique, qui n'ont
  pas toujours, m�me natifs, de RTTI.
  @param ATypeInfo   RTTI � fixer (peut �tre nil)
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
  Alloue une zone m�moire pour les RTTI
  Alloue une zone m�moire adapt�e au nom du type et � la taille des donn�es de
  type, et remplit les champs de TypeInfo (TypeData reste non initialis�).
  La zone m�moire ainsi allou�e sera automatiquement lib�r�e � la destruction du
  type.
  @param TypeDataLength   Taille des donn�es de type
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
  Extrait les informations les plus importantes depuis les donn�es de type
*}
procedure TSepiType.ExtractTypeData;
begin
end;

{*
  Alignement du type
  Les variables de ce type seront plac�es en m�moire � un adresse divisible par
  l'alignement.
  @return Alignement du type
*}
function TSepiType.GetAlignment : integer;
begin
  Result := Size;
end;

{*
  Cr�e une nouvelle instance de TSepiType
  @return Instance cr��e
*}
class function TSepiType.NewInstance : TObject;
begin
  Result := inherited NewInstance;
  TSepiType(Result).FTypeInfoRef := @TSepiType(Result).FTypeInfo;
end;

{*
  Recense un type natif � partir de ses RTTI
  @param AOwner      Propri�taire du type
  @param ATypeInfo   RTTI du type � recenser
  @return Type nouvellement cr��
*}
class function TSepiType.LoadFromTypeInfo(AOwner : TSepiMeta;
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
  Aligne l'offset donn� conform�ment � la propri�t� Alignment
  @param Offset   Offset � aligner
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
  Il faut appeler CompatibleWith sur le type de la variable affect�e, et avec en
  param�tre le type de l'expression � droite de l'assignation.
  @param AType   Type avec lequel tester la compatibilit�
  @return True si les types sont compatibles, False sinon
*}
function TSepiType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := AType.FKind = FKind;
end;

{----------------------}
{ Classe TSepiMetaRoot }
{----------------------}

{*
  Cr�e une instance de TSepiMetaRoot
*}
constructor TSepiMetaRoot.Create;
begin
  inherited Create(nil, '');
  FRoot := Self;
  FState := msNormal;
  FSearchOrder := TObjectList.Create(False);
  FOnLoadUnit := nil;

  LoadUnit(SystemUnitName);
end;

{*
  [@inheritDoc]
*}
destructor TSepiMetaRoot.Destroy;
begin
  FSearchOrder.Free;
  inherited;
end;

{*
  Nombre d'unit�s
  @return Nombre d'unit�s
*}
function TSepiMetaRoot.GetUnitCount : integer;
begin
  Result := FChildren.Count;
end;

{*
  Tableau zero-based des unit�s
  @param Index   Index de l'unit� � r�cup�rer
  @return Unit� � l'index sp�cifi�
*}
function TSepiMetaRoot.GetUnits(Index : integer) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit(FChildren.Objects[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaRoot.ChildAdded(Child : TSepiMeta);
var CurrentUnit : TSepiMetaUnit;
    I : integer;
begin
  inherited;

  CurrentUnit := Child as TSepiMetaUnit;
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
  Charge une unit�
  @param UnitName   Nom de l'unit� � charger
  @return Unit� charg�e
*}
function TSepiMetaRoot.LoadUnit(const UnitName : string) : TSepiMetaUnit;
var ImportFunc : TSepiImportUnitFunc;
begin
  Result := TSepiMetaUnit(GetMeta(UnitName));

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
  D�charge une unit�
  @param UnitName   Nom de l'unit� � charger
*}
procedure TSepiMetaRoot.UnloadUnit(const UnitName : string);
var MetaUnit : TSepiMetaUnit;
begin
  if State = msDestroying then exit;

  MetaUnit := TSepiMetaUnit(GetMeta(UnitName));
  if MetaUnit = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  dec(MetaUnit.FRefCount);
  if MetaUnit.FRefCount = 0 then
  begin
    Assert(FChildren.IndexOfObject(MetaUnit) = FChildren.Count-1);
    MetaUnit.Free;
  end;
end;

{*
  Cherche un type enregistr� � partir de ses informations de type
  @param TypeInfo   Informations de type du type recherch�
  @return Le type correspondant (nil si non trouv�)
*}
function TSepiMetaRoot.GetType(TypeInfo : PTypeInfo) : TSepiType;
var TypeName : string;
    I : integer;
    Meta : TSepiMeta;
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
    Meta := TSepiMeta(FSearchOrder[I]).GetMeta(TypeName);

    if Meta is TSepiType then
    begin
      if TSepiType(Meta).TypeInfo = TypeInfo then
      begin
        Result := TSepiType(Meta);
        exit;
      end else
      begin
        if First = nil then
          First := TSepiType(Meta);
      end;
    end;
  end;

  Result := First;
end;

{*
  Cherche un type enregistr� � partir de son nom
  @param TypeName   Nom du type recherch�
  @return Le type correspondant (ou nil si non trouv�)
*}
function TSepiMetaRoot.GetType(const TypeName : string) : TSepiType;
var I : integer;
    Meta : TSepiMeta;
begin
  if TypeName = '' then
  begin
    Result := nil;
    exit;
  end;

  for I := 0 to FSearchOrder.Count-1 do
  begin
    Meta := TSepiMeta(FSearchOrder[I]).GetMeta(TypeName);
    if Meta is TSepiType then
    begin
      Result := TSepiType(Meta);
      exit;
    end;
  end;

  Result := nil;
end;

{*
  Trouve un type enregistr� � partir de ses informations de type
  @param TypeInfo   Informations de type du type recherch�
  @return Le type correspondant aux informations de type donn�es
  @throw ESepiMetaNotFoundError Aucun type enregistr� correspondant
*}
function TSepiMetaRoot.FindType(TypeInfo : PTypeInfo) : TSepiType;
begin
  if TypeInfo = nil then Result := nil else
  begin
    Result := GetType(TypeInfo);
    if Result = nil then
      raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound,
        [TypeInfo.Name]);
  end;
end;

{*
  Trouve un type enregistr� � partir de son nom
  @param TypeName   Nom du type recherch�
  @return Le type correspondant au nom donn�
  @throw ESepiMetaNotFoundError Aucun type enregistr� correspondant
*}
function TSepiMetaRoot.FindType(const TypeName : string) : TSepiType;
begin
  if TypeName = '' then Result := nil else
  begin
    Result := GetType(TypeName);
    if Result = nil then
      raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound, [TypeName]);
  end;
end;

{----------------------}
{ Classe TSepiMetaUnit }
{----------------------}

{*
  Charge une unit� depuis un flux
*}
constructor TSepiMetaUnit.Load(AOwner : TSepiMeta; Stream : TStream);
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
        FUsesList.AddObject(Str, TSepiMetaRoot(AOwner).LoadUnit(Str));
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
  Cr�e une nouvelle unit�
  @param AOwner   Propri�taire de l'unit� (la racine)
  @param AName    Nom de l'unit�
*}
constructor TSepiMetaUnit.Create(AOwner : TSepiMeta; const AName : string;
  const AUses : array of string);
begin
  Assert(AOwner is TSepiMetaRoot);

  FOwner := AOwner;
  FRefCount := 0;

  FUsesList := TStringList.Create;
  if not AnsiSameText(AName, SystemUnitName) then
    AddUses(TSepiMetaRoot(AOwner).LoadUnit(SystemUnitName));
  MoreUses(AUses);

  inherited Create(AOwner, AName, tkUnknown);

  FOwningUnit := Self;
  FCurrentVisibility := mvPublic;
end;

{*
  [@inheritDoc]
*}
destructor TSepiMetaUnit.Destroy;
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
  Ajoute une unit� aux uses de cette unit�
  @param AUnit   Unit� � ajouter aux uses
  @return Index de la nouvelle unit� dans les uses
*}
function TSepiMetaUnit.AddUses(AUnit : TSepiMetaUnit) : integer;
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
  Change la visibilit� courante
  @param Value   Nouvelle visibilit�
*}
procedure TSepiMetaUnit.SetCurrentVisibility(Value : TMemberVisibility);
begin
  if Value in [mvPublic, mvPublished] then
    FCurrentVisibility := mvPublic
  else
    FCurrentVisibility := mvPrivate;
end;

{*
  Nombre d'unit�s utilis�es
  @return Nombre d'unit� utilis�es
*}
function TSepiMetaUnit.GetUsedUnitCount : integer;
begin
  Result := FUsesList.Count;
end;

{*
  Tableau zero-based des unit�s utilis�es
  @param Index   Index de l'unit� utilis�e
  @return Unit� utilis�e dont l'index est sp�cifi�
*}
function TSepiMetaUnit.GetUsedUnits(Index : integer) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit(FUsesList.Objects[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaUnit.Save(Stream : TStream);
begin
  inherited;
  SaveChildren(Stream);
end;

{*
  Ajoute des uses � l'unit�
  Cette m�thode ne peut �tre appel�e que pour une unit� en cours de
  construction.
  @param AUses   Uses � ajouter
*}
procedure TSepiMetaUnit.MoreUses(const AUses : array of string);
var I : integer;
begin
  for I := Low(AUses) to High(AUses) do
    if FUsesList.IndexOf(AUses[I]) < 0 then
      AddUses(TSepiMetaRoot(Owner).LoadUnit(AUses[I]));
end;

{*
  Compl�te l'unit� cr��e
*}
procedure TSepiMetaUnit.Complete;
begin
  Assert(State = msConstructing);
  Loaded;
end;

{*
  Enregistre l'unit� dans un flux
*}
procedure TSepiMetaUnit.SaveToStream(Stream : TStream);
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
  Charge l'unit� depuis un flux
  @param AOwner             Propri�taire de l'unit�
  @param Stream             Flux depuis lequel charger l'unit�
  @param AOnGetMethodCode   M�thode de call-back pour r�cup�rer le code d'une
                            m�thode
*}
class function TSepiMetaUnit.LoadFromStream(AOwner : TSepiMeta;
  Stream : TStream;
  const AOnGetMethodCode : TGetMethodCodeEvent = nil) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit(NewInstance);
  Result.FOnGetMethodCode := AOnGetMethodCode;
  Result.Load(AOwner, Stream);
  Result.FOnGetMethodCode := nil;
end;

{*
  Lit une r�f�rence depuis un flux
  Cette m�thode ne peut �tre appel�e que depuis le constructeur Load des metas
  contenus dans cette unit�. � tout autre moment, c'est la violation d'acc�s
  assur�e.
  @param Stream   Flux dans lequel �crire la r�f�rence
  @param Ref      Variable de type TSepiMeta o� stocker la r�f�rence lue
*}
procedure TSepiMetaUnit.ReadRef(Stream : TStream; out Ref);
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
      TObject(Ref) := FindMeta(RefList[RefIndex])
    else // remote reference
      TObject(Ref) := TSepiMetaUnit(FUsesList.Objects[UnitIndex-1])
        .FindMeta(RefList[RefIndex]);
    RefList.Objects[RefIndex] := TObject(Ref);
  end;
end;

{*
  Ajoute une r�f�rence qui va devoir �tre enregistr�e
  Cette m�thode ne peut �tre appel�e que depuis la m�thode ListReferences des
  metas contenus dans cette unit�. � tout autre moment, c'est la violation
  d'acc�s assur�e.
  @param Ref   R�f�rence � ajouter
*}
procedure TSepiMetaUnit.AddRef(Ref : TSepiMeta);
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
  �crit une r�f�rence dans un flux
  Toute r�f�rence � �crire doit �tre ajout�e au moyen de la m�thode AddRef dans
  la m�thode ListReferences de son r�f�renceur.
  Cette m�thode ne peut �tre appel�e que depuis la m�thode Save des metas
  contenus dans cette unit�. � tout autre moment, c'est la violation d'acc�s
  assur�e.
  @param Stream   Flux dans lequel �crire la r�f�rence
  @param Ref      R�f�rence � �crire
*}
procedure TSepiMetaUnit.WriteRef(Stream : TStream; Ref : TSepiMeta);
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
constructor TSepiTypeAlias.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  OwningUnit.ReadRef(Stream, FDest);
end;

{*
  Cr�e un nouvel alias de type
  @param AOwner   Propri�taire de l'alias de type
  @param AName    Nom de l'alias de type
  @param ADest    Destination de l'alias
*}
constructor TSepiTypeAlias.Create(AOwner : TSepiMeta; const AName : string;
  ADest : TSepiType);
begin
  inherited Create(AOwner, AName);
  FDest := ADest;
end;

{*
  Cr�e un nouvel alias de type
  @param AOwner   Propri�taire de l'alias de type
  @param AName    Nom de l'alias de type
  @param ADest    RTTI de la destination de l'alias
*}
constructor TSepiTypeAlias.Create(AOwner : TSepiMeta; const AName : string;
  ADest : PTypeInfo);
begin
  Create(AOwner, AName, AOwner.Root.FindType(ADest));
end;

{*
  Cr�e un nouvel alias de type
  @param AOwner   Propri�taire de l'alias de type
  @param AName    Nom de l'alias de type
  @param ADest    Nom de la destination de l'alias
*}
constructor TSepiTypeAlias.Create(AOwner : TSepiMeta;
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
constructor TSepiConstant.Load(AOwner : TSepiMeta; Stream : TStream);
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
  Cr�e une nouvelle vraie constante
  @param AOwner   Propri�taire de la constante
  @param AName    Nom de la constante
  @param AValue   Valeur de la constante
  @param AType    Type de la constante
*}
constructor TSepiConstant.Create(AOwner : TSepiMeta;
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
  Cr�e une nouvelle vraie constante
  @param AOwner      Propri�taire de la constante
  @param AName       Nom de la constante
  @param AValue      Valeur de la constante
  @param ATypeInfo   RTTI du type de la constante (d�termin� par VType si nil)
*}
constructor TSepiConstant.Create(AOwner : TSepiMeta;
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
  Cr�e une nouvelle vraie constante
  @param AOwner      Propri�taire de la constante
  @param AName       Nom de la constante
  @param AValue      Valeur de la constante
  @param ATypeName   Nom du type de la constante
*}
constructor TSepiConstant.Create(AOwner : TSepiMeta;
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
  Charge une variable ou une constante typ�e depuis un flux
*}
constructor TSepiVariable.Load(AOwner : TSepiMeta; Stream : TStream);
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
  Importe une variable ou constante typ�e native
  @param AOwner     Propri�taire de la variable
  @param AName      Nom de la variable
  @param AValue     Valeur de la variable
  @param AType      Type de la variable
  @param AIsConst   Indique si c'est une constante typ�e
*}
constructor TSepiVariable.Create(AOwner : TSepiMeta; const AName : string;
  const AValue; AType : TSepiType; AIsConst : boolean = False);
begin
  inherited Create(AOwner, AName);

  FIsConst := AIsConst;
  FType := AType;
  FValue := @AValue;
  FOwnValue := False;
end;

{*
  Importe une variable ou constante typ�e native
  @param AOwner      Propri�taire de la variable
  @param AName       Nom de la variable
  @param AValue      Valeur de la variable
  @param ATypeInfo   RTTI du type de la variable
  @param AIsConst    Indique si c'est une constante typ�e
*}
constructor TSepiVariable.Create(AOwner : TSepiMeta; const AName : string;
  const AValue; ATypeInfo : PTypeInfo; AIsConst : boolean = False);
begin
  Create(AOwner, AName, AValue, AOwner.Root.FindType(ATypeInfo), AIsConst);
end;

{*
  Importe une variable ou constante typ�e native
  @param AOwner      Propri�taire de la variable
  @param AName       Nom de la variable
  @param AValue      Valeur de la variable
  @param ATypeName   Nom du type de la variable
  @param AIsConst    Indique si c'est une constante typ�e
*}
constructor TSepiVariable.Create(AOwner : TSepiMeta; const AName : string;
  const AValue; const ATypeName : string; AIsConst : boolean = False);
begin
  Create(AOwner, AName, AValue, AOwner.Root.FindType(ATypeName), AIsConst);
end;

{*
  Cr�e une nouvelle variable ou constante typ�e
  @param AOwner     Propri�taire de la variable
  @param AName      Nom de la variable
  @param AType      Type de la variable
  @param AIsConst   Indique si c'est une constante typ�e
*}
constructor TSepiVariable.Create(AOwner : TSepiMeta; const AName : string;
  AType : TSepiType; AIsConst : boolean = False);
begin
  inherited Create(AOwner, AName);

  FIsConst := AIsConst;
  FType := AType;

  GetMem(FValue, FType.Size);
  FillChar(FValue^, FType.Size, 0);
  FOwnValue := True;
end;

{*
  Cr�e une nouvelle variable ou constante typ�e
  @param AOwner      Propri�taire de la variable
  @param AName       Nom de la variable
  @param ATypeInfo   RTTI du type de la variable
  @param AIsConst    Indique si c'est une constante typ�e
*}
constructor TSepiVariable.Create(AOwner : TSepiMeta; const AName : string;
  ATypeInfo : PTypeInfo; AIsConst : boolean = False);
begin
  Create(AOwner, AName, AOwner.Root.FindType(ATypeInfo), AIsConst);
end;

{*
  Cr�e une nouvelle variable ou constante typ�e
  @param AOwner      Propri�taire de la variable
  @param AName       Nom de la variable
  @param ATypeName   Nom du type de la variable
  @param AIsConst    Indique si c'est une constante typ�e
*}
constructor TSepiVariable.Create(AOwner : TSepiMeta; const AName : string;
  const ATypeName : string; AIsConst : boolean = False);
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
  Cr�e une nouvelle t�che de chargement/d�chargement d'une unit� Sepi
  @param AOwner            Gestionnaire propri�taire
  @param ARoot             Racine Sepi
  @param AUnitName         Nom de l'unit� � charger/d�charger
  @param AIsLoad           True pour charger, False pour d�charger
  @param AFreeOnFinished   Indique si doit se d�truire automatiquement
*}
constructor TSepiUnitLoadTask.Create(AOwner : TScCustomTaskQueue;
  ARoot : TSepiMetaRoot; const AUnitName : string; AIsLoad : boolean;
  AFreeOnFinished : boolean);
begin
  inherited Create(AOwner, AFreeOnFinished);
  FRoot := ARoot;
  FUnitName := AUnitName;
  FIsLoad := AIsLoad;
end;

{*
  Cr�e une nouvelle t�che de chargement/d�chargement d'une unit� Sepi
  @param AOwner      Gestionnaire propri�taire
  @param ARoot       Racine Sepi
  @param AUnitName   Nom de l'unit� � charger/d�charger
  @param AIsLoad     True pour charger, False pour d�charger
*}
constructor TSepiUnitLoadTask.Create(AOwner : TScCustomTaskQueue;
  ARoot : TSepiMetaRoot; const AUnitName : string; AIsLoad : boolean);
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
  Cr�e un nouveau gestionnaire de racine
  @param ARoot   Racine � g�rer (si nil, la racine est cr��e et lib�r�e)
*}
constructor TSepiAsynchronousRootManager.Create(ARoot : TSepiMetaRoot = nil);
begin
  inherited Create;

  FOwnsRoot := ARoot = nil;
  if FOwnsRoot then
    FRoot := TSepiMetaRoot.Create
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
  Demande le chargement d'une unit�
  @param UnitName   Unit� � charger
*}
procedure TSepiAsynchronousRootManager.LoadUnit(const UnitName : string);
begin
  TSepiUnitLoadTask.Create(Self, Root, UnitName, True);
end;

{*
  Demande le d�chargement d'une unit�
  @param UnitName   Unit� � charger
*}
procedure TSepiAsynchronousRootManager.UnloadUnit(const UnitName : string);
begin
  TSepiUnitLoadTask.Create(Self, Root, UnitName, False);
end;

initialization
  SepiRegisterMetaClasses([
    TSepiMetaUnit, TSepiTypeAlias, TSepiConstant, TSepiVariable
  ]);
finalization
  SepiMetaClasses.Free;
  SepiImportedUnits.Free;
end.

