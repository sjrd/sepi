{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------}

{*
  Définit les classes de gestion des meta-unités
  @author sjrd
  @version 1.0
*}
unit SepiReflectionCore;

interface

uses
  Types, Windows, SysUtils, Classes, Contnrs, RTLConsts, IniFiles, TypInfo,
  Variants, StrUtils, ScUtils, ScStrUtils, ScSyncObjs, ScCompilerMagic,
  ScSerializer, ScTypInfo, SepiCore, SepiReflectionConsts;

type
  TSepiMeta = class;
  TSepiRoot = class;
  TSepiUnit = class;
  TSepiAsynchronousRootManager = class;

  {*
    État d'un meta
  *}
  TSepiMetaState = (msNormal, msConstructing, msLoading, msDestroying);

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
  TSepiLoadUnitEvent = function(Sender: TSepiRoot;
    const UnitName: string): TSepiUnit of object;

  {*
    Type de l'événement OnGetMethodCode de TSepiUnit
    @param Sender        Méthode déclenchant l'événement (toujours TSepiMethod)
    @param Code          Adresse de code de la méthode
    @param CodeHandler   Gestionnaire de code
  *}
  TGetMethodCodeEvent = procedure(Sender: TObject; var Code: Pointer;
    var CodeHandler: TObject) of object;

  {*
    Type de l'événement OnGetTypeInfo de TSepiUnit
    Si TypeInfo est laissé à nil et si Found est laissé à False, Sepi créera
    ses propres RTTI pour le type. Ce cas ne devrait se présenter que pour des
    types anonymes pour lesquels le code est incapable de trouver les RTTI
    natives.
    @param Sender     Type déclenchant l'événement (toujours TSepiType)
    @param TypeInfo   RTTI du type
    @param Found      Positionnez à True si les RTTI ont été trouvées
  *}
  TGetTypeInfoEvent = procedure(Sender: TObject; var TypeInfo: PTypeInfo;
    var Found: Boolean) of object;

  {*
    Type de l'événement OnGetVarAddress de TSepiUnit
    Si VarAddress est laissée à nil, Sepi allouera lui-même un espace mémoire
    pour la variable. Cela ne devrait toutefois normalement jamais arriver. 
    @param Sender       Variable déclenchant l'événement (type TSepiVariable)
    @param VarAddress   Adresse de la variable
  *}
  TGetVarAddressEvent = procedure(Sender: TObject;
    var VarAddress: Pointer) of object;

  {*
    Déclenchée si l'on tente de recréer un meta (second appel au constructeur)
    @author sjrd
    @version 1.0
  *}
  ESepiMetaAlreadyCreated = class(ESepiError);

  {*
    Déclenchée lorsque la recherche d'un meta s'est soldée par un échec
    @author sjrd
    @version 1.0
  *}
  ESepiMetaNotFoundError = class(ESepiError);

  {*
    Déclenchée lorsque la recherche d'une unité s'est soldée par un échec
    @author sjrd
    @version 1.0
  *}
  ESepiUnitNotFoundError = class(ESepiMetaNotFoundError);

  {*
    Déclenchée si l'on tente de créer une constante avec un mauvais type
    @author sjrd
    @version 1.0
  *}
  ESepiBadConstTypeError = class(ESepiError);

  {*
    Déclenchée lorsqu'on tente de modifier un élément déjà complété
    @author sjrd
    @version 1.0
  *}
  ESepiAlreadyCompleted = class(ESepiError);

  {*
    Liste de meta
    @author sjrd
    @version 1.0
  *}
  TSepiMetaList = class(THashedStringList)
  private
    function GetMetas(Index: Integer): TSepiMeta;
    procedure SetMetas(Index: Integer; Value: TSepiMeta);
    function GetMetaFromName(const Name: string): TSepiMeta;
    procedure SetMetaFromName(const Name: string; Value: TSepiMeta);
  protected
    procedure InsertItem(Index: Integer; const S: string;
      AObject: TObject); override;
  public
    constructor Create;

    function AddMeta(Meta: TSepiMeta): Integer;
    function IndexOfMeta(Meta: TSepiMeta): Integer;
    function Remove(Meta: TSepiMeta): Integer;

    property Metas[Index: Integer]: TSepiMeta
      read GetMetas write SetMetas; default;
    property MetaFromName[const Name: string]: TSepiMeta
      read GetMetaFromName write SetMetaFromName;
  end;

  {*
    Meta générique
    Les meta sont les informations statiques qui représentent les unités
    compilées.
    @author sjrd
    @version 1.0
  *}
  TSepiMeta = class
  private
    /// True tant que le meta n'a pas été construit
    FIsForward: Boolean;
    FState: TSepiMetaState;         /// État
    FOwner: TSepiMeta;              /// Propriétaire
    FRoot: TSepiRoot;               /// Racine
    FOwningUnit: TSepiUnit;         /// Unité contenante
    FName: string;                  /// Nom
    FVisibility: TMemberVisibility; /// Visibilité
    /// Visibilité courante des enfants
    FCurrentVisibility: TMemberVisibility;
    FTag: Integer;                  /// Tag
    FForwards: TStrings;            /// Liste des enfants forwards
    FChildren: TSepiMetaList;       /// Liste des enfants
    FUnnamedChildCount: Integer;    /// Nombre d'enfants créés anonymes
    FObjResources: TObjectList;     /// Liste des ressources objet
    FPtrResources: TList;           /// Liste des ressources pointeur

    function GetWasForward: Boolean;

    function GetChildCount: Integer;
    function GetChildren(Index: Integer): TSepiMeta;

    function GetChildByName(const ChildName: string): TSepiMeta;
  protected
    procedure AddChild(Child: TSepiMeta); virtual;
    procedure RemoveChild(Child: TSepiMeta); virtual;
    procedure ReAddChild(Child: TSepiMeta); virtual;

    procedure LoadForwards(Stream: TStream); virtual;
    function LoadChild(Stream: TStream): TSepiMeta; virtual;
    procedure SaveForwards(Stream: TStream); virtual;
    procedure SaveChild(Stream: TStream; Child: TSepiMeta); virtual;

    procedure LoadChildren(Stream: TStream); virtual;
    procedure SaveChildren(Stream: TStream); virtual;

    procedure AddForward(const ChildName: string; Child: TObject);
    procedure ChildAdded(Child: TSepiMeta); virtual;
    procedure ChildRemoving(Child: TSepiMeta); virtual;

    procedure Loaded; virtual;

    procedure ListReferences; virtual;
    procedure Save(Stream: TStream); virtual;

    procedure Destroying; virtual;

    function InternalGetMeta(const Name: string): TSepiMeta; virtual;

    function InternalLookFor(const Name: string; FromUnit: TSepiUnit;
      FromClass: TSepiMeta = nil): TSepiMeta; virtual;

    function GetDisplayName: string; virtual;

    property State: TSepiMetaState read FState;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); virtual;
    constructor Create(AOwner: TSepiMeta; const AName: string);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function NewInstance: TObject; override;

    function GetFullName: string;
    function GetShorterNameFrom(From: TSepiMeta): string;
    function GetMeta(const Name: string): TSepiMeta;
    function FindMeta(const Name: string): TSepiMeta;

    function IsVisibleFrom(FromUnit: TSepiUnit;
      FromClass: TSepiMeta = nil): Boolean;
    function LookFor(const Name: string; FromUnit: TSepiUnit;
      FromClass: TSepiMeta = nil): TSepiMeta; overload;
    function LookFor(const Name: string): TSepiMeta; overload;

    function MakeUnnamedChildName: string;

    procedure AddObjResource(Obj: TObject);
    procedure AcquireObjResource(var Obj);
    procedure AddPtrResource(Ptr: Pointer);
    procedure AcquirePtrResource(var Ptr);

    property IsForward: Boolean read FIsForward;
    property WasForward: Boolean read GetWasForward;
    property Owner: TSepiMeta read FOwner;
    property Root: TSepiRoot read FRoot;
    property OwningUnit: TSepiUnit read FOwningUnit;
    property Name: string read FName;
    property DisplayName: string read GetDisplayName;
    property Visibility: TMemberVisibility read FVisibility write FVisibility;
    property CurrentVisibility: TMemberVisibility
      read FCurrentVisibility write FCurrentVisibility;
    property Tag: Integer read FTag write FTag;

    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: TSepiMeta read GetChildren;
    property ChildByName[const ChildName: string]: TSepiMeta
      read GetChildByName; default;
  end;

  {*
    Classe de TSepiMeta
  *}
  TSepiMetaClass = class of TSepiMeta;

  {*
    Comportement d'un type lorsqu'il est passé en paramètre
    @author sjrd
    @version 1.0
  *}
  TSepiTypeParamBehavior = record
    AlwaysByAddress: Boolean; /// Le type est toujours passé par adresse
    AlwaysByStack: Boolean;   /// Le type est toujours passé sur la pile
  end;

  {*
    Comportement d'un type lorsqu'il est valeur de retour
    - rbNone : pas de résultat (procédure)
    - rbOrdinal : renvoyé comme un ordinal, via EAX
    - rbInt64 : renvoyé comme Int64, via EDX:EAX
    - rbSingle : renvoyé comme Single, via ST(0)
    - rbDouble : renvoyé comme Double, via ST(0)
    - rbExtended : renvoyé comme Extended, via ST(0)
    - rbCurrency : renvoyé comme Currency, via ST(0)
    - rbParameter : l'adresse du résultat est passée en paramètre
    @author sjrd
    @version 1.0
  *}
  TSepiTypeResultBehavior = (
    rbNone, rbOrdinal, rbInt64, rbSingle, rbDouble, rbExtended, rbCurrency,
    rbParameter
  );

  {*
    Type
    @author sjrd
    @version 1.0
  *}
  TSepiType = class(TSepiMeta)
  private
    FKind: TTypeKind;         /// Type de type
    FNative: Boolean;         /// Indique si le type est un type natif Delphi
    FTypeInfoLength: Integer; /// Taille des RTTI créées (ou 0 si non créées)
    FTypeInfo: PTypeInfo;     /// RTTI (Runtime Type Information)
    FTypeData: PTypeData;     /// RTTD (Runtime Type Data)
    FTypeInfoRef: PPTypeInfo; /// Référence aux RTTI
  protected
    FSize: Integer;     /// Taille d'une variable de ce type
    FNeedInit: Boolean; /// Indique si ce type requiert une initialisation

    FParamBehavior: TSepiTypeParamBehavior;   /// Comportement comme paramètre
    FResultBehavior: TSepiTypeResultBehavior; /// Comportement comme résultat

    procedure Save(Stream: TStream); override;

    procedure ForceNative(ATypeInfo: PTypeInfo = nil);
    procedure AllocateTypeInfo(TypeDataLength: Integer = 0);
    procedure ExtractTypeData; virtual;

    function GetAlignment: Integer; virtual;
    function GetSafeResultBehavior: TSepiTypeResultBehavior;

    function GetDisplayName: string; override;
    function GetDescription: string; virtual;

    property TypeInfoRef: PPTypeInfo read FTypeInfoRef;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); virtual;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AKind: TTypeKind);
    constructor Clone(AOwner: TSepiMeta; const AName: string;
      Source: TSepiType); virtual;
    destructor Destroy; override;

    class function NewInstance: TObject; override;

    class function LoadFromTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo): TSepiType;

    procedure AlignOffset(var Offset: Integer);

    procedure InitializeValue(var Value);
    procedure FinalizeValue(var Value);
    function NewValue: Pointer;
    procedure DisposeValue(Value: Pointer);
    procedure CopyData(const Source; var Dest);

    function Equals(Other: TSepiType): Boolean; virtual;
    function CompatibleWith(AType: TSepiType): Boolean; virtual;

    property Kind: TTypeKind read FKind;
    property Native: Boolean read FNative;
    property TypeInfo: PTypeInfo read FTypeInfo;
    property TypeData: PTypeData read FTypeData;
    property Size: Integer read FSize;
    property NeedInit: Boolean read FNeedInit;
    property Alignment: Integer read GetAlignment;
    property ParamBehavior: TSepiTypeParamBehavior read FParamBehavior;
    property ResultBehavior: TSepiTypeResultBehavior read FResultBehavior;
    property SafeResultBehavior: TSepiTypeResultBehavior
      read GetSafeResultBehavior;
    property Description: string read GetDescription;
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
  TSepiRoot = class(TSepiMeta)
  private
    FSearchOrder: TObjectList;       /// Ordre de recherche
    FOnLoadUnit: TSepiLoadUnitEvent; /// Déclenché au chargement d'une unité

    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): TSepiUnit;
  protected
    procedure AddChild(Child: TSepiMeta); override;
    procedure RemoveChild(Child: TSepiMeta); override;
    procedure ReAddChild(Child: TSepiMeta); override;

    function InternalLookFor(const Name: string; FromUnit: TSepiUnit;
      FromClass: TSepiMeta = nil): TSepiMeta; override;

    function IncUnitRefCount(SepiUnit: TSepiUnit): Integer; virtual;
    function DecUnitRefCount(SepiUnit: TSepiUnit): Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadUnit(const UnitName: string): TSepiUnit; virtual;
    procedure UnloadUnit(const UnitName: string); virtual;

    function GetType(TypeInfo: PTypeInfo): TSepiType; overload;
    function GetType(const TypeName: string): TSepiType; overload;

    function FindType(TypeInfo: PTypeInfo): TSepiType; overload;
    function FindType(const TypeName: string): TSepiType; overload;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: TSepiUnit read GetUnits;

    /// Unité System - devrait être du type SepiSystemUnit.TSepiSystemUnit
    property SystemUnit: TSepiUnit index 0 read GetUnits;

    property OnLoadUnit: TSepiLoadUnitEvent
      read FOnLoadUnit write FOnLoadUnit;
  end;

  {*
    Fork d'une racine Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiRootFork = class(TSepiRoot)
  private
    FOriginalRoot: TSepiRoot;    /// Racine de base
    FForeignRefCounts: TStrings; /// Ref-counts des unités étrangères
    FDestroying: Boolean;        /// True lorsqu'en destruction
  protected
    procedure AddChild(Child: TSepiMeta); override;
    procedure RemoveChild(Child: TSepiMeta); override;

    function IncUnitRefCount(SepiUnit: TSepiUnit): Integer; override;
    function DecUnitRefCount(SepiUnit: TSepiUnit): Integer; override;
  public
    constructor Create(AOriginalRoot: TSepiRoot);
    destructor Destroy; override;

    procedure BeforeDestruction; override;

    function LoadUnit(const UnitName: string): TSepiUnit; override;
    procedure UnloadUnit(const UnitName: string); override;

    property OriginalRoot: TSepiRoot read FOriginalRoot;
  end;

  {*
    Type d'enfant pour le lazy-load
    - ckOther : autre
    - ckClass : classe
    - ckInterface : interface
  *}
  TSepiLazyLoadChildKind = (ckOther, ckClass, ckInterface);

  {*
    Données de lazy-load pour un enfant
    @author sjrd
    @version 1.0
  *}
  TSepiLazyLoadChildData = record
    Name: string;                 /// Nom de l'enfant
    Position: Int64;              /// Position dans le flux
    Kind: TSepiLazyLoadChildKind; /// Type d'enfant
    Inheritance: TStringDynArray; /// Héritage d'une classe/interface
  end;

  /// Tableau de données lazy-load pour les enfants
  TSepiLazyLoadChildrenData = array of TSepiLazyLoadChildData;

  {*
    Données de lazy-load
    @author sjrd
    @version 1.0
  *}
  TSepiLazyLoadData = class(TObject)
  private
    FOwner: TSepiMeta; /// Propriétaire des enfants chargés
    FStream: TStream;  /// Flux où charger les enfants

    FChildrenNames: TStrings;                       /// Noms des enfants
    FChildrenData: array of TSepiLazyLoadChildData; /// Données sur les enfants

    FLoadingClassIntfChildren: TStrings; /// Classes et interfaces en chargement
    FForwardClassIntfChildren: TObjectList; /// Classes et interfaces forward

    function GetChildData(const Name: string): TSepiLazyLoadChildData;
    function CanLoad(const Data: TSepiLazyLoadChildData): Boolean;
    procedure InternalLoadChild(const Data: TSepiLazyLoadChildData;
      var Child: TSepiMeta);
    procedure RetryForwards;
  public
    constructor Create(AOwner: TSepiMeta; AStream: TStream);
    destructor Destroy; override;

    procedure LoadFromStream(SkipChildrenInfo: Boolean);
    function ChildExists(const Name: string): Boolean;
    function LoadChild(const Name: string): TSepiMeta;

    class procedure SaveToStream(Owner: TSepiMeta; Stream: TStream);

    property Owner: TSepiMeta read FOwner;
    property Stream: TStream read FStream;
  end;

  {*
    Unité
    @author sjrd
    @version 1.0
  *}
  TSepiUnit = class(TSepiMeta)
  private
    /// Déclenché pour chaque méthode au chargement, pour obtenir son code
    FOnGetMethodCode: TGetMethodCodeEvent;
    /// Déclenché pour chaque type au chargement, pour obtenir ses RTTI
    FOnGetTypeInfo: TGetTypeInfoEvent;
    /// Déclenché pour chaque variable au chargement, pour obtenir son adresse
    FOnGetVarAddress: TGetVarAddressEvent;

    FRefCount: Integer;                    /// Compteur de références
    FUsesList: TStrings;                   /// Liste des uses
    FCurrentVisibility: TMemberVisibility; /// Visibilité courante

    FReferences: array of TStrings; /// Références en chargement/sauvegarde

    FLazyLoad: Boolean;               /// True si mode lazy-load
    FLazyLoadData: TSepiLazyLoadData; /// Données de lazy-load

    function AddUses(AUnit: TSepiUnit): Integer;

    function LazyLoadChild(const Name: string): TSepiMeta;

    procedure SetCurrentVisibility(Value: TMemberVisibility);

    function GetUsedUnitCount: Integer;
    function GetUsedUnits(Index: Integer): TSepiUnit;
  protected
    procedure LoadChildren(Stream: TStream); override;
    procedure SaveChildren(Stream: TStream); override;

    procedure Save(Stream: TStream); override;

    function InternalGetMeta(const Name: string): TSepiMeta; override;

    function InternalLookFor(const Name: string; FromUnit: TSepiUnit;
      FromClass: TSepiMeta = nil): TSepiMeta; override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const AUses: array of string);
    destructor Destroy; override;

    procedure MoreUses(const AUses: array of string);

    procedure Complete;

    procedure SaveToStream(Stream: TStream);
    class function LoadFromStream(AOwner: TSepiMeta; Stream: TStream;
      ALazyLoad: Boolean = False;
      const AOnGetMethodCode: TGetMethodCodeEvent = nil;
      const AOnGetTypeInfo: TGetTypeInfoEvent = nil;
      const AOnGetVarAddress: TGetVarAddressEvent = nil): TSepiUnit;

    procedure ReadRef(Stream: TStream; out Ref);
    procedure AddRef(Ref: TSepiMeta);
    procedure WriteRef(Stream: TStream; Ref: TSepiMeta);

    property CurrentVisibility: TMemberVisibility
      read FCurrentVisibility write SetCurrentVisibility;

    property UsedUnitCount: Integer read GetUsedUnitCount;
    property UsedUnits[Index: Integer]: TSepiUnit read GetUsedUnits;

    property LazyLoad: Boolean read FLazyLoad;

    property OnGetMethodCode: TGetMethodCodeEvent read FOnGetMethodCode;
    property OnGetTypeInfo: TGetTypeInfoEvent read FOnGetTypeInfo;
    property OnGetVarAddress: TGetVarAddressEvent read FOnGetVarAddress;
  end;

  {*
    Alias de type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeAlias = class(TSepiMeta)
  private
    FDest: TSepiType; /// Type destination
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      ADest: TSepiType); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      ADest: PTypeInfo); overload;
    constructor Create(AOwner: TSepiMeta;
      const AName, ADest: string); overload;

    property Dest: TSepiType read FDest;
  end;

  {*
    Constante
    @author sjrd
    @version 1.0
  *}
  TSepiConstant = class(TSepiMeta)
  private
    FType: TSepiType;   /// Type de la constante
    FValuePtr: Pointer; /// Pointeur sur la valeur
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AType: TSepiType); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AType: TSepiType; const AValue); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const AValue: Variant; AType: TSepiType); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const AValue: Variant; ATypeInfo: PTypeInfo = nil); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const AValue: Variant; const ATypeName: string); overload;
    destructor Destroy; override;

    property ConstType: TSepiType read FType;
    property ValuePtr: Pointer read FValuePtr;
  end;

  {*
    Variable (ou constante typée)
    @author sjrd
    @version 1.0
  *}
  TSepiVariable = class(TSepiMeta)
  private
    FIsConst: Boolean;  /// Indique si la variable est une constante typée
    FType: TSepiType;   /// Type de la constante
    FValue: Pointer;    /// Pointeur sur la variable
    FOwnValue: Boolean; /// Indique si Sepi a alloué la variable
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure Destroying; override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;

    constructor Create(AOwner: TSepiMeta; const AName: string;
      const AValue; AType: TSepiType; AIsConst: Boolean = False); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const AValue; ATypeInfo: PTypeInfo;
      AIsConst: Boolean = False); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const AValue; const ATypeName: string;
      AIsConst: Boolean = False); overload;

    constructor Create(AOwner: TSepiMeta; const AName: string;
      AType: TSepiType; AIsConst: Boolean = False); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      ATypeInfo: PTypeInfo; AIsConst: Boolean = False); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const ATypeName: string; AIsConst: Boolean = False); overload;

    destructor Destroy; override;

    property IsConst: Boolean read FIsConst;
    property VarType: TSepiType read FType;
    property Value: Pointer read FValue;
  end;

  {*
    Espace de noms (d'étendue plus petite qu'une unité)
    Un espace de noms peut avoir un propriétaire virtuel. S'il en a, la méthode
    LookFor continue sa recherche d'un identificateur via le propriétaire
    virtuel, plutôt que le propriétaire réel.
    @author sjrd
    @version 1.0
  *}
  TSepiNamespace = class(TSepiMeta)
  private
    FVirtualOwner: TSepiMeta; /// Propriétaire virtuel
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    function InternalLookFor(const Name: string; FromUnit: TSepiUnit;
      FromClass: TSepiMeta = nil): TSepiMeta; override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AVirtualOwner: TSepiMeta = nil);

    procedure Complete;

    property VirtualOwner: TSepiMeta read FVirtualOwner;
  end;

  {*
    Tâche de chargement/déchargement d'une unité Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiUnitLoadTask = class(TScTask)
  private
    FRoot: TSepiRoot;  /// Racine Sepi
    FUnitName: string; /// Nom de l'unité à charger/décharger
    FIsLoad: Boolean;  /// True pour charger, False pour décharger
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TScCustomTaskQueue; ARoot: TSepiRoot;
      const AUnitName: string; AIsLoad: Boolean;
      AFreeOnFinished: Boolean); overload;
    constructor Create(AOwner: TScCustomTaskQueue; ARoot: TSepiRoot;
      const AUnitName: string; AIsLoad: Boolean); overload;

    property Root: TSepiRoot read FRoot;
    property UnitName: string read FUnitName;
    property IsLoad: Boolean read FIsLoad;
  end;

  {*
    Gestionnaire asynchrone de racine Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiAsynchronousRootManager = class(TScTaskQueue)
  private
    FOwnsRoot: Boolean; /// Indique si l'on possède la racine
    FRoot: TSepiRoot;   /// Meta-racine gérée
  public
    constructor Create(ARoot: TSepiRoot = nil);
    destructor Destroy; override;

    procedure LoadUnit(const UnitName: string);
    procedure UnloadUnit(const UnitName: string);

    property Root: TSepiRoot read FRoot;
  end;

  {*
    Type de routine call-back pour l'import d'une unité sous Sepi
  *}
  TSepiImportUnitFunc = function(Root: TSepiRoot): TSepiUnit;

procedure SepiRegisterMetaClasses(const MetaClasses: array of TSepiMetaClass);

procedure SepiRegisterImportedUnit(const UnitName: string;
  ImportFunc: TSepiImportUnitFunc);
procedure SepiUnregisterImportedUnit(const UnitName: string);
function SepiImportedUnit(const UnitName: string): TSepiImportUnitFunc;

const {don't localize}
  SystemUnitName = 'System'; /// Nom de l'unité System.pas

const
  /// Comportement par des défaut d'un type comme paramètre
  DefaultTypeParamBehavior: TSepiTypeParamBehavior = (
    AlwaysByAddress: False; AlwaysByStack: False
  );

  /// Chaînes des visibilités
  VisibilityStrings: array[TMemberVisibility] of string = (
    'strict private', 'private', 'strict protected', 'protected', 'public',
    'published'
  );

implementation

uses
  SepiOrdTypes, SepiStrTypes, SepiArrayTypes, SepiMembers, SepiImportsSystem;

var
  SepiMetaClasses: TStrings = nil;
  SepiImportedUnits: TStrings = nil;

{*
  Recense des classes de meta
  @param MetaClasses   Classes de meta à recenser
*}
procedure SepiRegisterMetaClasses(const MetaClasses: array of TSepiMetaClass);
var
  I: Integer;
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
  La classe recherchée doit avoir été recensée au préalable avec
  SepiRegisterMetaClasses.
  @param MetaClassName   Nom de la classe de meta
  @return La classe de meta dont le nom correspond
  @throws EClassNotFound La classe recherchée n'existe pas
*}
function SepiFindMetaClass(const MetaClassName: string): TSepiMetaClass;
var
  Index: Integer;
begin
  if not Assigned(SepiMetaClasses) then
    Index := -1
  else
    Index := SepiMetaClasses.IndexOf(MetaClassName);
  if Index < 0 then
    raise EClassNotFound.CreateFmt(SClassNotFound, [MetaClassName]);

  Result := TSepiMetaClass(SepiMetaClasses.Objects[Index]);
end;

{*
  Recense une routine d'import d'unité
  @param UnitName     Nom de l'unité
  @param ImportFunc   Routine de call-back pour l'import de l'unité
*}
procedure SepiRegisterImportedUnit(const UnitName: string;
  ImportFunc: TSepiImportUnitFunc);
var
  Index: Integer;
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

  Index := SepiImportedUnits.IndexOf(UnitName);

  if Index < 0 then
    SepiImportedUnits.AddObject(UnitName, TObject(@ImportFunc))
  else
    SepiImportedUnits.Objects[Index] := TObject(@ImportFunc);
end;

{*
  Supprime un recensement d'import d'unité
  @param UnitName   Nom de l'unité
*}
procedure SepiUnregisterImportedUnit(const UnitName: string);
var
  Index: Integer;
begin
  if not Assigned(SepiImportedUnits) then
    Exit;
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
function SepiImportedUnit(const UnitName: string): TSepiImportUnitFunc;
var
  Index: Integer;
begin
  if not Assigned(SepiImportedUnits) then
    Result := nil
  else
  begin
    Index := SepiImportedUnits.IndexOf(UnitName);
    if Index < 0 then
      Result := nil
    else
      Result := TSepiImportUnitFunc(SepiImportedUnits.Objects[Index]);
  end;
end;

{----------------------}
{ Classe TSepiMetaList }
{----------------------}

{*
  Crée une instance de TSepiMetaList
*}
constructor TSepiMetaList.Create;
begin
  inherited;
  CaseSensitive := False;
end;

{*
  Liste zero-based des metas
  @param Index   Index du meta à obtenir
  @return Meta à l'index spécifié
*}
function TSepiMetaList.GetMetas(Index: Integer): TSepiMeta;
begin
  Result := TSepiMeta(Objects[Index]);
end;

{*
  Assigne la référence à un meta
  Un meta ne peut pas être modifé une fois assigné, il ne peut donc être assigné
  qu'une et une seule fois.
  @param Index   Index du meta à modifier
  @param Value   Référence au nouveau meta
*}
procedure TSepiMetaList.SetMetas(Index: Integer; Value: TSepiMeta);
begin
  if Assigned(Objects[Index]) then
    Error(@SSepiMetaAlreadyAssigned, Index);
  Objects[Index] := Value;
end;

{*
  Liste des metas indexée par leurs noms
  @param Name   Nom d'un meta
  @return Le meta dont le nom a été spécifié, ou nil s'il n'existe pas
*}
function TSepiMetaList.GetMetaFromName(const Name: string): TSepiMeta;
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then
    Result := nil
  else
    Result := TSepiMeta(Objects[Index]);
end;

{*
  Assigne ou ajoute un meta par son nom
  Un meta ne peut pas être modifé une fois assigné, il ne peut donc être assigné
  qu'une et une seule fois.
  @param Name    Nom du meta
  @param Value   Référence au meta
*}
procedure TSepiMetaList.SetMetaFromName(const Name: string;
  Value: TSepiMeta);
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then
    AddObject(Name, Value)
  else
    Metas[Index] := Value;
end;

{*
  Ajoute un élément dans la liste de metas
  @param Index     Index où ajouter l'élément
  @param S         Chaîne à ajouter
  @param AObject   Objet à ajouter
*}
procedure TSepiMetaList.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
begin
  if IndexOf(S) >= 0 then
    raise EListError.CreateFmt(SSepiMetaAlreadyExists, [S]);
  inherited;
end;

{*
  Ajoute un meta
  @param Meta   Meta à ajouter
  @return Index du meta nouvellement ajouté
*}
function TSepiMetaList.AddMeta(Meta: TSepiMeta): Integer;
begin
  Result := AddObject(Meta.Name, Meta);
end;

{*
  Cherche un meta dans la liste
  @param Meta   Meta à chercher
  @return L'index du meta dans la liste, ou nil s'il n'existe pas
*}
function TSepiMetaList.IndexOfMeta(Meta: TSepiMeta): Integer;
begin
  Result := IndexOf(Meta.Name);
end;

{*
  Supprime un meta de la liste
  @param Meta   Meta à supprimer
  @return L'index auquel se trouvait le meta
*}
function TSepiMetaList.Remove(Meta: TSepiMeta): Integer;
begin
  Result := IndexOfMeta(Meta);
  if Result >= 0 then
    Delete(Result);
end;

{------------------}
{ Classe TSepiMeta }
{------------------}

{*
  Charge un meta depuis un flux
  @param AOwner   Propriétaire du meta
  @param Stream   Flux depuis lequel charger le meta
*}
constructor TSepiMeta.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  Create(AOwner, ReadStrFromStream(Stream));
  Stream.ReadBuffer(FVisibility, 1);
  Stream.ReadBuffer(FTag, SizeOf(Integer));
  FState := msLoading;
end;

{*
  Crée un nouveau meta
  @param AOwner   Propriétaire du meta
  @param AName    Nom du meta
*}
constructor TSepiMeta.Create(AOwner: TSepiMeta; const AName: string);
begin
  if not IsForward then
    raise ESepiMetaAlreadyCreated.CreateFmt(SSepiMetaAlreadyCreated, [Name]);

  inherited Create;

  FIsForward := False;
  FState := msConstructing;
  FOwner := AOwner;

  if (AName = '') and (Owner <> nil) then
    FName := Owner.MakeUnnamedChildName
  else
    FName := AName;

  if Owner = nil then
    FVisibility := mvPublic
  else
    FVisibility := Owner.CurrentVisibility;
    
  FCurrentVisibility := mvPublic;
  FTag := 0;
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
  Détruit l'instance
*}
destructor TSepiMeta.Destroy;
var
  I: Integer;
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
  True si le meta a été créé forward, False sinon
  @return True si le meta a été créé forward, False sinon
*}
function TSepiMeta.GetWasForward: Boolean;
begin
  Result := FOwner.FForwards.IndexOfObject(Self) >= 0;
end;

{*
  Nombre d'enfants
  @return Nombre d'enfants
*}
function TSepiMeta.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

{*
  Tableau zero-based des enfants
  @param Index   Index de l'enfant à récupérer
  @return Enfant à l'index spécifié
*}
function TSepiMeta.GetChildren(Index: Integer): TSepiMeta;
begin
  Result := TSepiMeta(FChildren[Index]);
end;

{*
  Tableau des enfants indexés par leurs noms
  À l'inverse de la fonction FindMeta, les noms composés ne sont pas acceptés.
  @param ChildName   Nom de l'enfant recherché
  @return Enfant dont le nom est ChildName
  @throws ESepiMetaNotFoundError L'enfant n'a pas été trouvé
*}
function TSepiMeta.GetChildByName(const ChildName: string): TSepiMeta;
begin
  Result := FChildren.MetaFromName[ChildName];
  if Result = nil then
    raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound, [ChildName]);
end;

{*
  Ajoute un enfant
  AddChild est appelée dans le constructeur du meta enfant, et ne doit pas être
  appelée ailleurs.
  @param Child   Enfant à ajouter
*}
procedure TSepiMeta.AddChild(Child: TSepiMeta);
begin
  FChildren.AddMeta(Child);
end;

{*
  Supprime un enfant
  RemoveChild est appelée dans le destructeur du meta enfant, et ne doit pas
  être appelée ailleurs.
  @param Child   Enfant à supprimer
*}
procedure TSepiMeta.RemoveChild(Child: TSepiMeta);
var
  Index: Integer;
begin
  if State <> msDestroying then
  begin
    FChildren.Remove(Child);

    Index := FForwards.IndexOfObject(Child);
    if Index >= 0 then
      FForwards.Delete(Index);
  end;
end;

{*
  Ajoute de nouveau un meta à la liste des enfants
  Cela a pour effet de replacer Child à la fin de la liste des enfants ajoutés
  jusque là. Les types composites se servent de cette méthode pour se replacer
  derrière les types anonymes qu'ils ont créés.
  @param Child   Enfant concerné
*}
procedure TSepiMeta.ReAddChild(Child: TSepiMeta);
var
  Index: Integer;
begin
  Index := FChildren.IndexOfMeta(Child);
  if Index >= 0 then
    FChildren.Move(Index, FChildren.Count-1);
end;

{*
  Charge les forwards depuis un flux
  @param Stream   Flux source
*}
procedure TSepiMeta.LoadForwards(Stream: TStream);
var
  Count, I: Integer;
  ForwardChild: TObject;
begin
  Stream.ReadBuffer(Count, 4);
  for I := 0 to Count-1 do
  begin
    ForwardChild := SepiFindMetaClass(ReadStrFromStream(Stream)).NewInstance;
    AddForward(ReadStrFromStream(Stream), ForwardChild);
  end;
end;

{*
  Charge un enfant depuis un flux
  @param Stream   Flux source
  @return Enfant chargé
*}
function TSepiMeta.LoadChild(Stream: TStream): TSepiMeta;
var
  IsForward: Boolean;
  Name, ClassName: string;
begin
  Stream.ReadBuffer(IsForward, 1);

  if IsForward then
  begin
    Name := ReadStrFromStream(Stream);
    Result := GetMeta(Name);
  end else
    Result := nil;

  ClassName := ReadStrFromStream(Stream);

  if Result <> nil then
    Result.Load(Self, Stream)
  else
    Result := SepiFindMetaClass(ClassName).Load(Self, Stream);
end;

{*
  Enregistre les forwards dans un flux
  @param Stream   Flux destination
*}
procedure TSepiMeta.SaveForwards(Stream: TStream);
var
  Count, I: Integer;
begin
  Count := FForwards.Count;
  Stream.WriteBuffer(Count, 4);
  for I := 0 to Count-1 do
  begin
    WriteStrToStream(Stream, FForwards.Objects[I].ClassName);
    WriteStrToStream(Stream, FForwards[I]);
  end;
end;

{*
  Enregistre un enfant dans un flux
  @param Stream   Flux destination
  @param Child    Enfant à enregistrer
*}
procedure TSepiMeta.SaveChild(Stream: TStream; Child: TSepiMeta);
var
  IsForward: Boolean;
begin
  IsForward := FForwards.IndexOfObject(Child) >= 0;
  Stream.WriteBuffer(IsForward, 1);

  if IsForward then
    WriteStrToStream(Stream, Child.Name);

  WriteStrToStream(Stream, Child.ClassName);
  Child.Save(Stream);
end;

{*
  Charge les enfants depuis un flux
  @param Stream   Flux depuis lequel charger les enfants
*}
procedure TSepiMeta.LoadChildren(Stream: TStream);
var
  Count, I: Integer;
begin
  LoadForwards(Stream);

  Stream.ReadBuffer(Count, 4);
  for I := 0 to Count-1 do
    LoadChild(Stream);
end;

{*
  Enregistre les enfants dans un flux
  @param Stream   Flux dans lequel enregistrer les enfants
*}
procedure TSepiMeta.SaveChildren(Stream: TStream);
var
  Count, I: Integer;
begin
  SaveForwards(Stream);

  Count := FChildren.Count;
  Stream.WriteBuffer(Count, 4);
  for I := 0 to Count-1 do
    SaveChild(Stream, FChildren[I]);
end;

{*
  Ajoute un enfant forward
  @param ChildName   Nom de l'enfant
  @param Child       Enfant à ajouter
*}
procedure TSepiMeta.AddForward(const ChildName: string; Child: TObject);
begin
  (Child as TSepiMeta).Visibility := CurrentVisibility;
  TSepiMeta(Child).FName := ChildName;
  FForwards.AddObject(ChildName, Child);
end;

{*
  Appelé lorsqu'un enfant vient d'être ajouté
  @param Child   Enfant qui vient d'être ajouté
*}
procedure TSepiMeta.ChildAdded(Child: TSepiMeta);
begin
end;

{*
  Appelé lorsqu'un enfant va être supprimé
  @param Child   Enfant sur le point d'être supprimé
*}
procedure TSepiMeta.ChildRemoving(Child: TSepiMeta);
begin
end;

{*
  Appelé lorsque l'unité contenante est complètement chargée/créée
*}
procedure TSepiMeta.Loaded;
var
  I: Integer;
begin
  // Already created children
  for I := 0 to FChildren.Count-1 do
    FChildren[I].Loaded;

  // Forward declarations which have not been created yet
  for I := 0 to FForwards.Count-1 do
    with TSepiMeta(FForwards.Objects[I]) do
      if IsForward then
        Loaded;

  // Changing the state
  FState := msNormal;
end;

{*
  Liste les références auprès de l'unité contenante, pour préparer la sauvegarde
*}
procedure TSepiMeta.ListReferences;
var
  I: Integer;
begin
  for I := 0 to FChildren.Count-1 do
    FChildren[I].ListReferences;
end;

{*
  Enregistre le meta dans un flux
  @param Stream   Flux dans lequel enregistrer le meta
*}
procedure TSepiMeta.Save(Stream: TStream);
begin
  WriteStrToStream(Stream, Name);
  Stream.WriteBuffer(FVisibility, 1);
  Stream.WriteBuffer(FTag, SizeOf(Integer));
end;

{*
  Appelé lorsque l'environnement Sepi est sur le point d'être détruit
*}
procedure TSepiMeta.Destroying;
var
  I: Integer;
begin
  FState := msDestroying;
  if Assigned(FChildren) then
    // could not be if an error occured in constructor
    for I := 0 to ChildCount-1 do
      Children[I].Destroying;
end;

{*
  Appelé juste après l'exécution du dernier constructeur
*}
procedure TSepiMeta.AfterConstruction;
begin
  inherited;
  if Assigned(Owner) then
    Owner.ChildAdded(Self);
end;

{*
  Appelé juste avant l'exécution du premier destructeur
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
  Cherche un meta enfant
  InternalGetMeta ne doit pas être appelée directement : passez par GetMeta.
  GetMeta n'appelle InternalGetMeta qu'avec un nom non-composé (sans .). De
  plus, si InternalGetMeta renvoie un TSepiTypeAlias, GetMeta se chargera de le
  "déréférencer".
  @param Name   Nom du meta à trouver
  @return Le meta correspondant, ou nil s'il n'a pas été trouvé
*}
function TSepiMeta.InternalGetMeta(const Name: string): TSepiMeta;
var
  Index: Integer;
begin
  Result := FChildren.MetaFromName[Name];

  if Result = nil then
  begin
    Index := FForwards.IndexOf(Name);
    if Index >= 0 then
      Result := TSepiMeta(FForwards.Objects[Index]);
  end;
end;

{*
  Recherche un meta à partir de son nom
  InternalLookFor ne doit pas être appelée directement : passez par LookFor.
  LookFor n'appelle InternalLookFor qu'avec un nom non-composé (sans .), et un
  paramètre FromClass qui est toujours de type TSepiClass.
  @param Name        Nom du meta recherché
  @param FromUnit    Unité depuis laquelle on recherche
  @param FromClass   Classe depuis laquelle on recherche (défaut = nil)
  @return Le meta recherché, ou nil si non trouvé
*}
function TSepiMeta.InternalLookFor(const Name: string; FromUnit: TSepiUnit;
  FromClass: TSepiMeta = nil): TSepiMeta;
begin
  // Basic search
  Result := GetMeta(Name);

  // Check for visibility
  if (Result <> nil) and (not Result.IsVisibleFrom(FromUnit, FromClass)) then
    Result := nil;

  // If not found, continue search a level up
  if (Result = nil) and (Owner <> nil) then
    Result := Owner.InternalLookFor(Name, FromUnit, FromClass);
end;

{*
  Nom destiné à l'affichage
  @return Nom destiné à l'affichage
*}
function TSepiMeta.GetDisplayName: string;
begin
  Result := Name;
end;

{*
  Crée une nouvelle instance de TSepiMeta
  @return Instance créée
*}
class function TSepiMeta.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSepiMeta(Result).FIsForward := True;
end;

{*
  Nom qualifié du meta, depuis l'unité contenante
  @return Nom qualifié du meta
*}
function TSepiMeta.GetFullName: string;
begin
  if Assigned(FOwner) and (FOwner.Name <> '') then
    Result := FOwner.GetFullName+'.'+Name
  else
    Result := Name;
end;

{*
  Obtient le nom le plus court possible pour être référencé depuis un autre meta
  @param From   Meta depuis lequel être référencé
  @return Nom le plus court possible, ou une chaîne vide si aucun n'est possible
*}
function TSepiMeta.GetShorterNameFrom(From: TSepiMeta): string;
var
  Current: TSepiMeta;
begin
  Current := Self;
  Result := Name;

  if Result = '' then
    Exit;

  while From.LookFor(Result) <> Self do
  begin
    Current := Current.Owner;

    if (Current = nil) or (Current.Name = '') then
    begin
      // No possible answer
      Result := '';
      Exit;
    end;

    Result := Current.Name + '.' + Result;
  end;
end;

{*
  Cherche un meta enfant
  @param Name   Nom du meta à trouver
  @return Le meta correspondant, ou nil s'il n'a pas été trouvé
*}
function TSepiMeta.GetMeta(const Name: string): TSepiMeta;
var
  MetaName, Field: string;
begin
  if not SplitToken(Name, '.', MetaName, Field) then
    Field := '';

  Result := InternalGetMeta(MetaName);

  if not Assigned(Result) then
    Exit;
  while Result is TSepiTypeAlias do
    Result := TSepiTypeAlias(Result).Dest;
  if Field <> '' then
    Result := Result.GetMeta(Field);
end;

{*
  Cherche un meta enfant
  @param Name   Nom du meta à trouver
  @return Le meta correspondant
  @throws ESepiMetaNotFoundError Le meta n'a pas été trouvé
*}
function TSepiMeta.FindMeta(const Name: string): TSepiMeta;
begin
  Result := GetMeta(Name);
  if (Result = nil) and (Name <> '') then
    raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound, [Name]);
end;

{*
  Teste si ce meta est visible depuis un endroit donné du programme
  @param FromUnit    Unité depuis laquelle on regarde
  @param FromClass   Classe depuis laquelle on regarde (défaut = nil)
  @return True si le meta est visible, False sinon
*}
function TSepiMeta.IsVisibleFrom(FromUnit: TSepiUnit;
  FromClass: TSepiMeta = nil): Boolean;
begin
  Result := True;

  if Visibility in [mvPublic, mvPublished] then
    Exit;
  if (Visibility in [mvPrivate, mvProtected]) and (FromUnit = OwningUnit) then
    Exit;
  if FromClass = Owner then
    Exit;

  if Visibility in [mvStrictProtected, mvProtected] then
  begin
    if (FromClass is TSepiClass) and (Owner is TSepiClass) and
      TSepiClass(FromClass).ClassInheritsFrom(TSepiClass(Owner)) then
      Exit;
  end;

  Result := False;
end;

{*
  Recherche un meta à partir de son nom
  LookFor, au contraire de GetMeta/FindMeta, tient compte des héritages, des
  visibilités, des uses, etc.
  Si Name est un nom composé, la première partie est recherchée selon
  l'algorithme LookFor, et les suivantes selon l'algorithme GetMeta.
  Le paramètre FromClass doit être de type TSepiClass.
  @param Name        Nom du meta recherché
  @param FromUnit    Unité depuis laquelle on recherche
  @param FromClass   Classe depuis laquelle on recherche (défaut = nil)
  @return Le meta recherché, ou nil si non trouvé
*}
function TSepiMeta.LookFor(const Name: string; FromUnit: TSepiUnit;
  FromClass: TSepiMeta = nil): TSepiMeta;
var
  FirstName, ChildName: string;
begin
  if not SplitToken(Name, '.', FirstName, ChildName) then
    ChildName := '';

  Result := InternalLookFor(FirstName, FromUnit, FromClass as TSepiClass);
  if (Result <> nil) and (ChildName <> '') then
    Result := Result.GetMeta(ChildName);
end;

{*
  Recherche un meta à partir de son nom
  Cette version de LookFor détermine les FromUnit et FromClass qui
  correspondent à ce meta.
*}
function TSepiMeta.LookFor(const Name: string): TSepiMeta;
var
  FromClass: TSepiMeta;
begin
  FromClass := Self;
  while (FromClass <> nil) and (not (FromClass is TSepiClass)) do
    FromClass := FromClass.Owner;

  Result := LookFor(Name, OwningUnit, FromClass);
end;

{*
  Construit un nom pour un enfant créé anonyme
  @return Nom pour l'enfant
*}
function TSepiMeta.MakeUnnamedChildName: string;
begin
  Inc(FUnnamedChildCount);
  Result := '$' + IntToStr(FUnnamedChildCount);
end;

{*
  Ajoute un objet aux ressources du meta
  Tous les objets ajoutés aux ressources du meta seront libérés lorsque le meta
  sera libéré lui-même.
  @param Obj   Objet à ajouter aux ressources
*}
procedure TSepiMeta.AddObjResource(Obj: TObject);
begin
  FObjResources.Add(Obj);
end;

{*
  S'approprie une ressource objet
  L'objet est ajouté aux ressources objet du meta, puis le paramètre Obj est
  mis à nil, afin qu'un Free ultérieur sur celui-ci ne fasse plus rien.
  @param Obj   Objet à ajouter aux ressources
*}
procedure TSepiMeta.AcquireObjResource(var Obj);
begin
  AddObjResource(TObject(Obj));
  TObject(Obj) := nil;
end;

{*
  Ajoute un pointeur aux ressources du meta
  Tous les pointeurs ajoutés aux ressources du meta seront libérés lorsque le
  meta sera libéré lui-même.
  @param Ptr   Pointeur à ajouter aux ressources
*}
procedure TSepiMeta.AddPtrResource(Ptr: Pointer);
begin
  FPtrResources.Add(Ptr);
end;

{*
  S'approprie une ressource pointeur
  Le pointeur est ajouté aux ressources pointeur du meta, puis le paramètre
  Ptr est mis à nil, afin que le code appelant puisse tester l'égalité à nil
  avant de le libérer lui-même.
  @param Ptr   Pointeur à ajouter aux ressources
*}
procedure TSepiMeta.AcquirePtrResource(var Ptr);
begin
  AddPtrResource(Pointer(Ptr));
  Pointer(Ptr) := nil;
end;

{------------------}
{ Classe TSepiType }
{------------------}

{*
  Recense un type natif
  @param AOwner      Propriétaire du type
  @param ATypeInfo   RTTI du type à recenser
*}
constructor TSepiType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited Create(AOwner, AnsiReplaceStr(ATypeInfo.Name, '.', '$$'));

  FKind := ATypeInfo.Kind;
  FNative := True;
  FTypeInfoLength := 0;
  FTypeInfo := ATypeInfo;
  FTypeData := GetTypeData(FTypeInfo);
  FSize := 0;
  FParamBehavior := DefaultTypeParamBehavior;
  FResultBehavior := rbOrdinal;
end;

{*
  Charge un type depuis un flux
  @param AOwner   Propriétaire du type
  @param Stream   Flux depuis lequel charger le type
*}
constructor TSepiType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FKind, SizeOf(TTypeKind));
  FNative := False;
  FTypeInfoLength := 0;
  FTypeInfo := nil;
  FTypeData := nil;
  FSize := 0;
  FNeedInit := False;
  FParamBehavior := DefaultTypeParamBehavior;
  FResultBehavior := rbOrdinal;

  if Assigned(OwningUnit.OnGetTypeInfo) then
  begin
    OwningUnit.OnGetTypeInfo(Self, FTypeInfo, FNative);
    if FTypeInfo <> nil then
    begin
      FNative := True;
      FTypeData := GetTypeData(FTypeInfo);
    end;
  end;
end;

{*
  Crée un nouveau type
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
  @param AKind    Type de type
*}
constructor TSepiType.Create(AOwner: TSepiMeta; const AName: string;
  AKind: TTypeKind);
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
  FResultBehavior := rbOrdinal;
end;

{*
  Clone un type
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
  @param Source   Type à cloner
*}
constructor TSepiType.Clone(AOwner: TSepiMeta; const AName: string;
  Source: TSepiType);
begin
  raise ESepiError.CreateFmt(SCantCloneType, [Source.Name]);
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

{*
  [@inheritDoc]
*}
procedure TSepiType.Save(Stream: TStream);
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
procedure TSepiType.ForceNative(ATypeInfo: PTypeInfo = nil);
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
procedure TSepiType.AllocateTypeInfo(TypeDataLength: Integer = 0);
var
  ShortName: ShortString;
  NameLength: Integer;
begin
  ShortName := Name;
  NameLength := Length(Name)+1; // 1 byte for string length

  FTypeInfoLength := SizeOf(TTypeKind) + NameLength + TypeDataLength;
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
function TSepiType.GetAlignment: Integer;
begin
  Result := Size;
end;

{*
  Variante de ResultBehavior valide aussi sur un type nil
  @return rbNone si le type est nil, ResultBehavior sinon
*}
function TSepiType.GetSafeResultBehavior: TSepiTypeResultBehavior;
begin
  if Self = nil then
    Result := rbNone
  else
    Result := FResultBehavior;
end;

{*
  [@inheritDoc]
*}
function TSepiType.GetDisplayName: string;
begin
  if Pos('$', Name) = 0 then
    Result := Name
  else
    Result := Description;
end;

{*
  Description courte
  @return Description courte
*}
function TSepiType.GetDescription: string;
begin
  Result := Name;
end;

{*
  Crée une nouvelle instance de TSepiType
  @return Instance créée
*}
class function TSepiType.NewInstance: TObject;
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
class function TSepiType.LoadFromTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo): TSepiType;
const
  TypeClasses: array[TTypeKind] of TSepiTypeClass = (
    nil, TSepiIntegerType, TSepiCharType, TSepiEnumType, TSepiFloatType,
    TSepiShortStringType, TSepiSetType, TSepiClass, TSepiMethodRefType,
    TSepiCharType, TSepiStringType, TSepiStringType, TSepiVariantType,
    nil, nil, TSepiInterface, TSepiInt64Type, TSepiDynArrayType
  );
var
  TypeClass: TSepiTypeClass;
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

    Result := TypeClass.RegisterTypeInfo(AOwner, ATypeInfo);
  end else
    raise EAbstractError.Create(SSepiNoRegisterTypeInfo);
end;

{*
  Aligne l'offset donné conformément à la propriété Alignment
  @param Offset   Offset à aligner
*}
procedure TSepiType.AlignOffset(var Offset: Integer);
var
  Disalign: Integer;
begin
  Disalign := Offset mod Alignment;
  if Disalign > 0 then
    Inc(Offset, Alignment-Disalign);
end;

{*
  Initialise une valeur de ce type
  @param Value   Valeur à initialiser
*}
procedure TSepiType.InitializeValue(var Value);
begin
  if NeedInit then
    Initialize(Value, TypeInfo);
end;

{*
  Finalise une valeur de ce type
  @param Value   Valeur à finaliser
*}
procedure TSepiType.FinalizeValue(var Value);
begin
  if NeedInit then
    Finalize(Value, TypeInfo);
end;

{*
  Alloue une nouvelle valeur de ce type, et l'initialise si besoin
  @return Pointeur sur la nouvelle valeur
*}
function TSepiType.NewValue: Pointer;
begin
  GetMem(Result, Size);
  InitializeValue(Result^);
end;

{*
  Libère une valeur de ce type, et la finalise si besoin
  @param Value   Pointeur sur la valeur à libérer
*}
procedure TSepiType.DisposeValue(Value: Pointer);
begin
  FinalizeValue(Value^);
  FreeMem(Value);
end;

{*
  Copie des données de ce type
  @param Source   Source
  @param Dest     Destination
*}
procedure TSepiType.CopyData(const Source; var Dest);
begin
  ScTypInfo.CopyData(Source, Dest, Size, TypeInfo);
end;

{*
  Teste si un type est égal à un autre
  Deux types sont égaux si et seulement si leurs variables ont la même structure
  interne. Dans ce cas, une copie via CopyData est valide.
  La relation établie avec Equals est une relation d'équivalence.
  @param Other   Autre type à comparer
  @return True si les types sont égaux, False sinon
*}
function TSepiType.Equals(Other: TSepiType): Boolean;
begin
  Result := ClassType = Other.ClassType;
end;

{*
  Teste si un type est compatible avec un autre
  Il faut appeler CompatibleWith sur le type de la variable affectée, et avec en
  paramètre le type de l'expression à droite de l'assignation.
  @param AType   Type avec lequel tester la compatibilité
  @return True si les types sont compatibles, False sinon
*}
function TSepiType.CompatibleWith(AType: TSepiType): Boolean;
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
  FState := msNormal;
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
function TSepiRoot.GetUnitCount: Integer;
begin
  Result := FChildren.Count;
end;

{*
  Tableau zero-based des unités
  @param Index   Index de l'unité à récupérer
  @return Unité à l'index spécifié
*}
function TSepiRoot.GetUnits(Index: Integer): TSepiUnit;
begin
  Result := TSepiUnit(FChildren.Objects[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRoot.AddChild(Child: TSepiMeta);
var
  CurrentUnit: TSepiUnit;
  I: Integer;
begin
  inherited;

  CurrentUnit := Child as TSepiUnit;
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
  [@inheritDoc]
*}
procedure TSepiRoot.RemoveChild(Child: TSepiMeta);
begin
  FSearchOrder.Remove(Child);
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRoot.ReAddChild(Child: TSepiMeta);
var
  Index: Integer;
begin
  inherited;

  Index := FSearchOrder.IndexOf(Child);
  if Index >= 0 then
    FSearchOrder.Move(Index, FSearchOrder.Count-1);
end;

{*
  [@inheritDoc]
*}
function TSepiRoot.InternalLookFor(const Name: string; FromUnit: TSepiUnit;
  FromClass: TSepiMeta = nil): TSepiMeta;
var
  I: Integer;
begin
  // Search in root
  Result := inherited InternalLookFor(Name, FromUnit, FromClass);
  if Result <> nil then
    Exit;

  // Search in FromUnit
  if FromUnit <> nil then
  begin
    Result := FromUnit.InternalLookFor(Name, FromUnit, FromClass);
    if Result <> nil then
      Exit;
  end;

  // Search in other units
  for I := 0 to ChildCount-1 do
  begin
    if Children[I] = FromUnit then
      Continue;
    Result := Children[I].InternalLookFor(Name, FromUnit, FromClass);
    if Result <> nil then
      Exit;
  end;
end;

{*
  Incrémente le compteur de références d'une unité
  @param SepiUnit   Unité dont incrémenter le compteur de références
  @return Nouvelle valeur du compteur de références
*}
function TSepiRoot.IncUnitRefCount(SepiUnit: TSepiUnit): Integer;
begin
  Result := InterlockedIncrement(SepiUnit.FRefCount);
end;

{*
  Décrémente le compteur de références d'une unité
  @param SepiUnit   Unité dont décrémenter le compteur de références
  @return Nouvelle valeur du compteur de références
*}
function TSepiRoot.DecUnitRefCount(SepiUnit: TSepiUnit): Integer;
begin
  Result := InterlockedDecrement(SepiUnit.FRefCount);
end;

{*
  Charge une unité
  @param UnitName   Nom de l'unité à charger
  @return Unité chargée
*}
function TSepiRoot.LoadUnit(const UnitName: string): TSepiUnit;
var
  ImportFunc: TSepiImportUnitFunc;
begin
  Result := TSepiUnit(GetMeta(UnitName));

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

  IncUnitRefCount(Result);
end;

{*
  Décharge une unité
  @param UnitName   Nom de l'unité à charger
*}
procedure TSepiRoot.UnloadUnit(const UnitName: string);
var
  SepiUnit: TSepiUnit;
begin
  if State = msDestroying then
    Exit;

  SepiUnit := TSepiUnit(GetMeta(UnitName));
  if SepiUnit = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  if DecUnitRefCount(SepiUnit) = 0 then
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
function TSepiRoot.GetType(TypeInfo: PTypeInfo): TSepiType;
var
  TypeName: string;
  I: Integer;
  Meta: TSepiMeta;
  First: TSepiType;
begin
  if TypeInfo = nil then
  begin
    Result := nil;
    Exit;
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
        Exit;
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
  Cherche un type enregistré à partir de son nom
  @param TypeName   Nom du type recherché
  @return Le type correspondant (ou nil si non trouvé)
*}
function TSepiRoot.GetType(const TypeName: string): TSepiType;
var
  I: Integer;
  Meta: TSepiMeta;
begin
  if TypeName = '' then
  begin
    Result := nil;
    Exit;
  end;

  for I := 0 to FSearchOrder.Count-1 do
  begin
    Meta := TSepiMeta(FSearchOrder[I]).GetMeta(TypeName);
    if Meta is TSepiType then
    begin
      Result := TSepiType(Meta);
      Exit;
    end;
  end;

  Result := nil;
end;

{*
  Trouve un type enregistré à partir de ses informations de type
  @param TypeInfo   Informations de type du type recherché
  @return Le type correspondant aux informations de type données
  @throw ESepiMetaNotFoundError Aucun type enregistré correspondant
*}
function TSepiRoot.FindType(TypeInfo: PTypeInfo): TSepiType;
begin
  if TypeInfo = nil then
    Result := nil
  else
  begin
    Result := GetType(TypeInfo);
    if Result = nil then
      raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound,
        [TypeInfo.Name]);
  end;
end;

{*
  Trouve un type enregistré à partir de son nom
  @param TypeName   Nom du type recherché
  @return Le type correspondant au nom donné
  @throw ESepiMetaNotFoundError Aucun type enregistré correspondant
*}
function TSepiRoot.FindType(const TypeName: string): TSepiType;
begin
  if TypeName = '' then
    Result := nil
  else
  begin
    Result := GetType(TypeName);
    if Result = nil then
      raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound, [TypeName]);
  end;
end;

{---------------------}
{ TSepiRootFork class }
{---------------------}

{*
  Crée un nouveau fork de racine Sepi
  @param AOriginalRoot   Racine Sepi originale
*}
constructor TSepiRootFork.Create(AOriginalRoot: TSepiRoot);
begin
  FOriginalRoot := AOriginalRoot;
  FForeignRefCounts := TStringList.Create;

  inherited Create;
end;

{*
  [@inheritDoc]
*}
destructor TSepiRootFork.Destroy;
begin
  inherited;

  FForeignRefCounts.Free;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.AddChild(Child: TSepiMeta);
begin
  inherited;

  FForeignRefCounts.Add(Child.Name);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.RemoveChild(Child: TSepiMeta);
begin
  FForeignRefCounts.Delete(FForeignRefCounts.IndexOf(Child.Name));

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiRootFork.IncUnitRefCount(SepiUnit: TSepiUnit): Integer;
var
  Index: Integer;
begin
  if SepiUnit.Owner = Self then
  begin
    // Own unit
    Result := inherited IncUnitRefCount(SepiUnit);
  end else
  begin
    // Foreign unit
    with FForeignRefCounts do
    begin
      Index := IndexOf(SepiUnit.Name);
      Assert(Index >= 0);
      Result := Integer(Objects[Index]) + 1;
      Objects[Index] := TObject(Result);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiRootFork.DecUnitRefCount(SepiUnit: TSepiUnit): Integer;
var
  Index: Integer;
begin
  if SepiUnit.Owner = Self then
  begin
    // Own unit
    Result := inherited DecUnitRefCount(SepiUnit);
  end else
  begin
    // Foreign unit
    with FForeignRefCounts do
    begin
      Index := IndexOf(SepiUnit.Name);
      Assert(Index >= 0);
      Result := Integer(Objects[Index]) - 1;
      Objects[Index] := TObject(Result);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.BeforeDestruction;
var
  I: Integer;
  SepiUnit: TSepiUnit;
begin
  FDestroying := True;

  // Mark my own units as destructing
  for I := UnitCount-1 downto 0 do
    if Units[I].Owner = Self then
      TSepiRootFork(Units[I]).Destroying;

  // Free units the way I want to
  for I := UnitCount-1 downto 0 do
  begin
    SepiUnit := Units[I];

    if SepiUnit.Owner = Self then
    begin
      OutputDebugString(PChar('Freeing own unit: '+SepiUnit.Name));
      SepiUnit.Free;
    end else
    begin
      OutputDebugString(PChar('Unloading foreign unit: '+SepiUnit.Name));
      ChildRemoving(SepiUnit);
      RemoveChild(SepiUnit);

      OriginalRoot.UnloadUnit(SepiUnit.Name);
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiRootFork.LoadUnit(const UnitName: string): TSepiUnit;
var
  ImportFunc: TSepiImportUnitFunc;
begin
  Result := TSepiUnit(GetMeta(UnitName));

  if Result = nil then
  begin
    ImportFunc := SepiImportedUnit(UnitName);

    // For imported units, check the original root directly
    if Assigned(ImportFunc) then
    begin
      Result := (OriginalRoot.GetMeta(UnitName) as TSepiUnit);

      if Result <> nil then
      begin
        OutputDebugString(PChar('Loading foreing unit: '+UnitName));
        OriginalRoot.LoadUnit(UnitName);
        AddChild(Result);
        ChildAdded(Result);
      end else
        Result := ImportFunc(Self);
    end;

    // Otherwise, load the unit in this fork
    if (Result = nil) and Assigned(OnLoadUnit) then
      Result := OnLoadUnit(Self, UnitName);

    // If we didn't find anything, check the original root
    if Result = nil then
    begin
      Result := (OriginalRoot.GetMeta(UnitName) as TSepiUnit);

      if Result <> nil then
      begin
        OutputDebugString(PChar('Loading foreign unit: '+UnitName));
        OriginalRoot.LoadUnit(UnitName);
        AddChild(Result);
        ChildAdded(Result);
      end;
    end;
  end;

  // Unit not found
  if Result = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  // Increment ref-count
  IncUnitRefCount(Result);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.UnloadUnit(const UnitName: string);
var
  SepiUnit: TSepiUnit;
begin
  if FDestroying or (State = msDestroying) then
    Exit;

  SepiUnit := TSepiUnit(GetMeta(UnitName));
  if SepiUnit = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  if DecUnitRefCount(SepiUnit) = 0 then
  begin
    // Remove the unit
    if SepiUnit.Owner = Self then
      SepiUnit.Free
    else
    begin
      ChildRemoving(SepiUnit);
      RemoveChild(SepiUnit);
      OriginalRoot.UnloadUnit(SepiUnit.Name);
    end;
  end;
end;

{-------------------------}
{ TSepiLazyLoadData class }
{-------------------------}

{*
  Crée les données de lazy-load
  @param AOwner    Propriétaire des enfants à charger
  @param AStream   Flux depuis lequel charger les enfants
*}
constructor TSepiLazyLoadData.Create(AOwner: TSepiMeta; AStream: TStream);
begin
  inherited Create;

  FOwner := AOwner;
  FStream := AStream;

  FChildrenNames := THashedStringList.Create;
  FLoadingClassIntfChildren := TStringList.Create;
  FForwardClassIntfChildren := TObjectList.Create(False);
end;

{*
  [@inheritDoc]
*}
destructor TSepiLazyLoadData.Destroy;
begin
  FForwardClassIntfChildren.Free;
  FLoadingClassIntfChildren.Free;
  FChildrenNames.Free;

  inherited;
end;

{*
  Récupère les informations de lazy-load pour un enfant
  @param Name   Nom de l'enfant à charger
  @param Data   En sortie : données sur cet enfant
  @raise ESepiMetaNotFoundError Aucun enfant de ce nom trouvé
*}
function TSepiLazyLoadData.GetChildData(
  const Name: string): TSepiLazyLoadChildData;
var
  Index: Integer;
begin
  Index := FChildrenNames.IndexOf(Name);
  if Index < 0 then
    raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound, [Name]);

  Result := FChildrenData[Index];
end;

{*
  Teste si un enfant peut être chargé maintenant
  @param Data   Données de chargement d'un enfant
  @return True si l'enfant décrit par ces données peut être chargé, False sinon
*}
function TSepiLazyLoadData.CanLoad(const Data: TSepiLazyLoadChildData): Boolean;
var
  I: Integer;
begin
  { A class or interface child whom an ancestor is being loaded can't be
    loaded now. }
  if Data.Kind in [ckClass, ckInterface] then
  begin
    for I := 0 to Length(Data.Inheritance)-1 do
    begin
      if FLoadingClassIntfChildren.IndexOf(Data.Inheritance[I]) >= 0 then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

  Result := True;
end;

{*
  Charge un enfant depuis le flux
  @param Data    Données sur l'enfant à charger
  @param Child   En entrée et/ou en sortie : l'enfant à charger
*}
procedure TSepiLazyLoadData.InternalLoadChild(
  const Data: TSepiLazyLoadChildData; var Child: TSepiMeta);
var
  ClassOrIntf: Boolean;
  OldPosition: Int64;
  IsForward: Boolean;
  MetaClassName: string;
begin
  ClassOrIntf := Data.Kind in [ckClass, ckInterface];
  if ClassOrIntf then
    FLoadingClassIntfChildren.Add(Data.Name);
  try
    OldPosition := Stream.Position;
    try
      Stream.Position := Data.Position;

      // Ignore forward information
      Stream.ReadBuffer(IsForward, 1);
      if IsForward then
        ReadStrFromStream(Stream);

      // Load child
      MetaClassName := ReadStrFromStream(Stream);
      if Child = nil then
      begin
        Child := SepiFindMetaClass(MetaClassName).Load(Owner, Stream);
      end else
      begin
        FForwardClassIntfChildren.Remove(Child);
        Child.Load(Owner, Stream);
      end;
    finally
      Stream.Position := OldPosition;
    end;
  finally
    if ClassOrIntf then
      FLoadingClassIntfChildren.Delete(
        FLoadingClassIntfChildren.IndexOf(Data.Name));
  end;

  // Retry forwards if Child is a class or an interface
  if ClassOrIntf then
    RetryForwards;
end;

{*
  Réessaye de charger les enfants qui sont forwards
*}
procedure TSepiLazyLoadData.RetryForwards;
var
  I: Integer;
  Child: TSepiMeta;
  Data: TSepiLazyLoadChildData;
begin
  { Looking downwards is more efficient, because we expect recent demands to
    be easier to satisfy. }
  for I := FForwardClassIntfChildren.Count-1 downto 0 do
  begin
    Child := TSepiMeta(FForwardClassIntfChildren[I]);
    Data := GetChildData(Child.Name);

    if CanLoad(Data) then
    begin
      InternalLoadChild(Data, Child); // Will call RetryForwards recursively
      Exit;
    end;
  end;
end;

{*
  Charge les données depuis le flux
  @param SkipChildrenInfo   Si True, saute les définitions des enfants
*}
procedure TSepiLazyLoadData.LoadFromStream(SkipChildrenInfo: Boolean);
var
  I: Integer;
  BasePosition, DataLength: Int64;
begin
  // Read data
  FChildrenNames.Clear;
  ReadDataFromStream(Stream, FChildrenData,
    TypeInfo(TSepiLazyLoadChildrenData));
  Stream.ReadBuffer(DataLength, SizeOf(Int64));

  // Make positions absolute and make name hash table
  BasePosition := Stream.Position;
  for I := 0 to Length(FChildrenData)-1 do
  begin
    FChildrenNames.Add(FChildrenData[I].Name);
    Inc(FChildrenData[I].Position, BasePosition);
  end;

  // Skip actual definition of children
  if SkipChildrenInfo then
    Stream.Seek(DataLength, soFromCurrent);
end;

{*
  Test si un enfant existe
  @param Name   Nom de l'enfant recherché
  @return True si un enfant de ce nom existe, False sinon
*}
function TSepiLazyLoadData.ChildExists(const Name: string): Boolean;
begin
  Result := FChildrenNames.IndexOf(Name) >= 0;
end;

{*
  Charge un enfant depuis son nom
  @param Name   Nom de l'enfant à charger
  @return Enfant chargé
*}
function TSepiLazyLoadData.LoadChild(const Name: string): TSepiMeta;
var
  Data: TSepiLazyLoadChildData;
begin
  Data := GetChildData(Name);

  // Avoid loading a class or interface whose ancestor is being loaded
  if not CanLoad(Data) then
  begin
    case Data.Kind of
      ckClass:
        Result := TSepiClass.ForwardDecl(Owner, Data.Name);
      ckInterface:
        Result := TSepiInterface.ForwardDecl(Owner, Data.Name);
    else
      Assert(False);
      Result := nil;
    end;

    if Result <> nil then
      FForwardClassIntfChildren.Add(Result);
    Exit;
  end;

  // Now do the job
  Result := nil;
  InternalLoadChild(Data, Result);
end;

{*
  Construit l'héritage (dans la même unité) d'une classe
  @param SepiClass       Classe dont construire l'héritage
  @param AncestorClass   Liste des noms des ancêtres
*}
procedure MakeClassInheritance(SepiClass: TSepiClass;
  AncestorList: TStrings);
var
  AncestorClass: TSepiClass;
begin
  AncestorClass := SepiClass.Parent;
  while (AncestorClass <> nil) and (AncestorClass.Owner = SepiClass.Owner) do
  begin
    AncestorList.Add(AncestorClass.Name);
    AncestorClass := AncestorClass.Parent;
  end;
end;

{*
  Construit l'héritage (dans la même unité) d'une interface
  @param SepiIntf        Interface dont construire l'héritage
  @param AncestorClass   Liste des noms des ancêtres
*}
procedure MakeIntfInheritance(SepiIntf: TSepiInterface;
  AncestorList: TStrings);
var
  AncestorIntf: TSepiInterface;
begin
  AncestorIntf := SepiIntf.Parent;
  while (AncestorIntf <> nil) and (AncestorIntf.Owner = SepiIntf.Owner) do
  begin
    AncestorList.Add(AncestorIntf.Name);
    AncestorIntf := AncestorIntf.Parent;
  end;
end;

{*
  Construit l'héritage d'une classe ou interface (dans la même unité)
  @param ClassIntf     Classe ou interface
  @param Inheritance   En sortie : héritage de la classe ou interface
*}
procedure MakeClassIntfInheritance(ClassIntf: TSepiType;
  out Inheritance: TStringDynArray);
var
  AncestorList: TStrings;
  I: Integer;
begin
  AncestorList := TStringList.Create;
  try
    if ClassIntf is TSepiClass then
      MakeClassInheritance(TSepiClass(ClassIntf), AncestorList)
    else
      MakeIntfInheritance(ClassIntf as TSepiInterface, AncestorList);

    // Fill in the Inheritance param
    SetLength(Inheritance, AncestorList.Count);
    for I := 0 to AncestorList.Count-1 do
      Inheritance[I] := AncestorList[I];
  finally
    AncestorList.Free;
  end;
end;

{*
  Produit les données de lazy-load pour un enfant
  @param Child      Enfant concerné
  @param Position   Position dans le flux
  @param Data       En sortie : données pour cet enfant
*}
procedure MakeLazyLoadChildData(Child: TSepiMeta; Position: Int64;
  out Data: TSepiLazyLoadChildData);
begin
  Data.Name := Child.Name;
  Data.Position := Position;

  if Child is TSepiClass then
  begin
    Data.Kind := ckClass;
    MakeClassIntfInheritance(TSepiClass(Child), Data.Inheritance);
  end else if Child is TSepiInterface then
  begin
    Data.Kind := ckInterface;
    MakeClassIntfInheritance(TSepiInterface(Child), Data.Inheritance);
  end else
  begin
    Data.Kind := ckOther;
  end;
end;

{*
  Enregistre les données d'un meta dans un flux
  @param Owner    Meta propriétaire
  @param Stream   Flux de destination
*}
class procedure TSepiLazyLoadData.SaveToStream(Owner: TSepiMeta;
  Stream: TStream);
var
  Count, I: Integer;
  TempStream: TMemoryStream;
  ChildrenData: TSepiLazyLoadChildrenData;
  Child: TSepiMeta;
  ChildrenDataLength: Int64;
begin
  Count := Owner.ChildCount;
  SetLength(ChildrenData, Count);

  TempStream := TMemoryStream.Create;
  try
    // Save forwards to temp stream
    Owner.SaveForwards(TempStream);

    // Save children to temp stream - meanwhile, make children data table
    TempStream.WriteBuffer(Count, 4);
    for I := 0 to Count-1 do
    begin
      Child := Owner.Children[I];
      MakeLazyLoadChildData(Child, TempStream.Position, ChildrenData[I]);
      Owner.SaveChild(TempStream, Child);
    end;

    // Write lazy-load data table
    WriteDataToStream(Stream, ChildrenData,
      TypeInfo(TSepiLazyLoadChildrenData));

    // Write children data length
    ChildrenDataLength := TempStream.Size;
    Stream.WriteBuffer(ChildrenDataLength, SizeOf(Int64));

    // Copy temp stream (forwards and children) into dest stream
    Stream.CopyFrom(TempStream, 0);
  finally
    TempStream.Free;
  end;
end;

{------------------}
{ Classe TSepiUnit }
{------------------}

{*
  Charge une unité depuis un flux
*}
constructor TSepiUnit.Load(AOwner: TSepiMeta; Stream: TStream);
var
  UsesCount, RefCount, I, J: Integer;
  Str: string;
begin
  if LazyLoad then
    FLazyLoadData := TSepiLazyLoadData.Create(Self, Stream);

  Stream.ReadBuffer(UsesCount, 4);
  SetLength(FReferences, UsesCount+1);
  FillChar(FReferences[0], 4*(UsesCount+1), 0);

  try
    // Load uses and set up the references lists
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
    if not LazyLoad then
    begin
      for I := 0 to UsesCount do
        FReferences[I].Free;
      SetLength(FReferences, 0);
    end;
  end;
end;

{*
  Crée une nouvelle unité
  @param AOwner   Propriétaire de l'unité (la racine)
  @param AName    Nom de l'unité
*}
constructor TSepiUnit.Create(AOwner: TSepiMeta; const AName: string;
  const AUses: array of string);
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
destructor TSepiUnit.Destroy;
var
  I: Integer;
begin
  if LazyLoad then
  begin
    for I := 0 to Length(FReferences)-1 do
      FReferences[I].Free;

    FLazyLoadData.Free;
  end;

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
function TSepiUnit.AddUses(AUnit: TSepiUnit): Integer;
begin
  if AUnit = Self then
    Result := -1
  else
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
  Charge un enfant, s'il existe
  @param Name   Nom de l'enfant
  @return Enfant chargé, ou nil si n'existe pas
*}
function TSepiUnit.LazyLoadChild(const Name: string): TSepiMeta;
begin
  Assert(LazyLoad);
  if FLazyLoadData.ChildExists(Name) then
    Result := FLazyLoadData.LoadChild(Name)
  else
    Result := nil;
end;

{*
  [@inheritDoc]
*}
function TSepiUnit.InternalGetMeta(const Name: string): TSepiMeta;
begin
  Result := inherited InternalGetMeta(Name);

  if LazyLoad and (Result = nil) then
    Result := LazyLoadChild(Name);
end;

{*
  Change la visibilité courante
  @param Value   Nouvelle visibilité
*}
procedure TSepiUnit.SetCurrentVisibility(Value: TMemberVisibility);
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
function TSepiUnit.GetUsedUnitCount: Integer;
begin
  Result := FUsesList.Count;
end;

{*
  Tableau zero-based des unités utilisées
  @param Index   Index de l'unité utilisée
  @return Unité utilisée dont l'index est spécifié
*}
function TSepiUnit.GetUsedUnits(Index: Integer): TSepiUnit;
begin
  Result := TSepiUnit(FUsesList.Objects[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnit.LoadChildren(Stream: TStream);
var
  LazyLoadData: TSepiLazyLoadData;
begin
  if LazyLoad then
    LazyLoadData := FLazyLoadData
  else
    LazyLoadData := TSepiLazyLoadData.Create(Self, Stream);

  try
    LazyLoadData.LoadFromStream(LazyLoad);
  finally
    if not LazyLoad then
      LazyLoadData.Free;
  end;

  if not LazyLoad then
    inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnit.SaveChildren(Stream: TStream);
begin
  TSepiLazyLoadData.SaveToStream(Self, Stream);
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnit.Save(Stream: TStream);
var
  UsesCount, RefCount, I, J: Integer;
  RefList: TStrings;
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
    inherited;
    SaveChildren(Stream);
  finally
    for I := 0 to UsesCount do
      if Assigned(FReferences[I]) then
        FReferences[I].Free;
    SetLength(FReferences, 0);
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiUnit.InternalLookFor(const Name: string; FromUnit: TSepiUnit;
  FromClass: TSepiMeta = nil): TSepiMeta;
var
  I: Integer;
begin
  // Basic search
  Result := GetMeta(Name);
  if (Result <> nil) and (not Result.IsVisibleFrom(FromUnit, FromClass)) then
    Result := nil;
  if Result <> nil then
    Exit;

  // Search in units themselves - not their children

  if AnsiSameText(Self.Name, Name) then
  begin
    Result := Self;
    Exit;
  end;

  for I := UsedUnitCount-1 downto 0 do
  begin
    Result := UsedUnits[I];
    if AnsiSameText(Result.Name, Name) then
      Exit;
  end;

  Result := nil;

  // Search in used units - only if FromUnit = Self
  if FromUnit <> Self then
    Exit;
  for I := UsedUnitCount-1 downto 0 do
  begin
    Result := UsedUnits[I].InternalLookFor(Name, FromUnit, FromClass);
    if Result <> nil then
      Exit;
  end;
end;

{*
  Ajoute des uses à l'unité
  Cette méthode ne peut être appelée que pour une unité en cours de
  construction.
  @param AUses   Uses à ajouter
*}
procedure TSepiUnit.MoreUses(const AUses: array of string);
var
  I: Integer;
begin
  // Add uses
  for I := Low(AUses) to High(AUses) do
    if FUsesList.IndexOf(AUses[I]) < 0 then
      AddUses(TSepiRoot(Owner).LoadUnit(AUses[I]));

  // Re-add myself to the root
  if Assigned(Owner) then // could not be if called from Create
    Owner.ReAddChild(Self);
end;

{*
  Complète l'unité créée
*}
procedure TSepiUnit.Complete;
begin
  Assert(State = msConstructing);
  Loaded;
end;

{*
  Enregistre l'unité dans un flux
*}
procedure TSepiUnit.SaveToStream(Stream: TStream);
begin
  if LazyLoad then
    raise ESepiError.Create(SSepiCantSaveLazyLoadUnit);

  Save(Stream);
end;

{*
  Charge l'unité depuis un flux
  @param AOwner             Propriétaire de l'unité
  @param Stream             Flux depuis lequel charger l'unité
  @param AOnGetMethodCode   Méthode de call-back pour récupérer le code d'une
                            méthode
  @param AOnGetTypeInfo     Méthode de call-back pour récupérer les RTTI d'un
                            type
*}
class function TSepiUnit.LoadFromStream(AOwner: TSepiMeta; Stream: TStream;
  ALazyLoad: Boolean = False;
  const AOnGetMethodCode: TGetMethodCodeEvent = nil;
  const AOnGetTypeInfo: TGetTypeInfoEvent = nil;
  const AOnGetVarAddress: TGetVarAddressEvent = nil): TSepiUnit;
begin
  Result := TSepiUnit(NewInstance);

  Result.FLazyLoad := ALazyLoad;
  Result.FOnGetMethodCode := AOnGetMethodCode;
  Result.FOnGetTypeInfo := AOnGetTypeInfo;
  Result.FOnGetVarAddress := AOnGetVarAddress;

  Result.Load(AOwner, Stream);

  Result.FOnGetMethodCode := nil;
  Result.FOnGetTypeInfo := nil;
  Result.FOnGetVarAddress := nil;
end;

{*
  Lit une référence depuis un flux
  Cette méthode ne peut être appelée que depuis le constructeur Load des metas
  contenus dans cette unité. À tout autre moment, c'est la violation d'accès
  assurée.
  @param Stream   Flux dans lequel écrire la référence
  @param Ref      Variable de type TSepiMeta où stocker la référence lue
*}
procedure TSepiUnit.ReadRef(Stream: TStream; out Ref);
var
  UnitIndex, RefIndex: Integer;
  RefList: TStrings;
begin
  // Reading unit index and checking for nil reference
  UnitIndex := 0;
  Stream.ReadBuffer(UnitIndex, 2);
  if UnitIndex = $FFFF then
  begin
    TObject(Ref) := nil;
    Exit;
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
      TObject(Ref) := TSepiUnit(FUsesList.Objects[UnitIndex-1])
        .FindMeta(RefList[RefIndex]);
    RefList.Objects[RefIndex] := TObject(Ref);
  end;
end;

{*
  Ajoute une référence qui va devoir être enregistrée
  Cette méthode ne peut être appelée que depuis la méthode ListReferences des
  metas contenus dans cette unité. À tout autre moment, c'est la violation
  d'accès assurée.
  @param Ref   Référence à ajouter
*}
procedure TSepiUnit.AddRef(Ref: TSepiMeta);
var
  UnitIndex: Integer;
  RefList: TStrings;
begin
  if Ref = nil then
    Exit;

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
  Cette méthode ne peut être appelée que depuis la méthode Save des metas
  contenus dans cette unité. À tout autre moment, c'est la violation d'accès
  assurée.
  @param Stream   Flux dans lequel écrire la référence
  @param Ref      Référence à écrire
*}
procedure TSepiUnit.WriteRef(Stream: TStream; Ref: TSepiMeta);
var
  UnitIndex, RefIndex: Integer;
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
constructor TSepiTypeAlias.Load(AOwner: TSepiMeta; Stream: TStream);
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
constructor TSepiTypeAlias.Create(AOwner: TSepiMeta; const AName: string;
  ADest: TSepiType);
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
constructor TSepiTypeAlias.Create(AOwner: TSepiMeta; const AName: string;
  ADest: PTypeInfo);
begin
  Create(AOwner, AName, AOwner.Root.FindType(ADest));
end;

{*
  Crée un nouvel alias de type
  @param AOwner   Propriétaire de l'alias de type
  @param AName    Nom de l'alias de type
  @param ADest    Nom de la destination de l'alias
*}
constructor TSepiTypeAlias.Create(AOwner: TSepiMeta;
  const AName, ADest: string);
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
procedure TSepiTypeAlias.Save(Stream: TStream);
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
constructor TSepiConstant.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FType);
  FValuePtr := ConstType.NewValue;

  ReadDataFromStream(Stream, FValuePtr^, ConstType.Size, ConstType.TypeInfo);
end;

{*
  Crée une nouvelle vraie constante sans initialiser sa valeur
  @param AOwner   Propriétaire de la constante
  @param AName    Nom de la constante
  @param AType    Type de la constante
*}
constructor TSepiConstant.Create(AOwner: TSepiMeta;
  const AName: string; AType: TSepiType);
begin
  inherited Create(AOwner, AName);

  FType := AType;
  FValuePtr := ConstType.NewValue;
end;

{*
  Crée une nouvelle vraie constante sans initialiser sa valeur
  @param AOwner   Propriétaire de la constante
  @param AName    Nom de la constante
  @param AType    Type de la constante
*}
constructor TSepiConstant.Create(AOwner: TSepiMeta;
  const AName: string; AType: TSepiType; const AValue);
begin
  Create(AOwner, AName, AType);
  ConstType.CopyData(AValue, ValuePtr^);
end;

{*
  Crée une nouvelle vraie constante
  @param AOwner   Propriétaire de la constante
  @param AName    Nom de la constante
  @param AValue   Valeur de la constante
  @param AType    Type de la constante
*}
constructor TSepiConstant.Create(AOwner: TSepiMeta;
  const AName: string; const AValue: Variant; AType: TSepiType);
var
  BoolValue: LongWord;
begin
  if AType is TSepiBooleanType then
  begin
    if TVarData(AValue).VBoolean then
      BoolValue := 1
    else
      BoolValue := 0;

    Create(AOwner, AName, AType, BoolValue);
  end else
    Create(AOwner, AName, AType, TVarData(AValue).VAny);
end;

{*
  Crée une nouvelle vraie constante
  @param AOwner      Propriétaire de la constante
  @param AName       Nom de la constante
  @param AValue      Valeur de la constante
  @param ATypeInfo   RTTI du type de la constante (déterminé par VType si nil)
*}
constructor TSepiConstant.Create(AOwner: TSepiMeta;
  const AName: string; const AValue: Variant; ATypeInfo: PTypeInfo = nil);
begin
  if ATypeInfo = nil then
  begin
    case VarType(AValue) of
      varSmallint: ATypeInfo := System.TypeInfo(Smallint);
      varInteger:  ATypeInfo := System.TypeInfo(Integer);
      varSingle:   ATypeInfo := System.TypeInfo(Single);
      varDouble:   ATypeInfo := System.TypeInfo(Double);
      varCurrency: ATypeInfo := System.TypeInfo(Currency);
      varDate:     ATypeInfo := System.TypeInfo(TDateTime);
      varError:    ATypeInfo := System.TypeInfo(HRESULT);
      varBoolean:  ATypeInfo := System.TypeInfo(Boolean);
      varShortInt: ATypeInfo := System.TypeInfo(Shortint);
      varByte:     ATypeInfo := System.TypeInfo(Byte);
      varWord:     ATypeInfo := System.TypeInfo(Word);
      varLongWord: ATypeInfo := System.TypeInfo(LongWord);
      varInt64:    ATypeInfo := System.TypeInfo(Int64);

      varOleStr, varStrArg, varString: ATypeInfo := System.TypeInfo(string);
    else
      raise ESepiBadConstTypeError.CreateFmt(
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
constructor TSepiConstant.Create(AOwner: TSepiMeta;
  const AName: string; const AValue: Variant; const ATypeName: string);
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
procedure TSepiConstant.Save(Stream: TStream);
begin
  inherited;

  OwningUnit.WriteRef(Stream, FType);
  WriteDataToStream(Stream, FValuePtr^, FType.Size, FType.TypeInfo);
end;

{----------------------}
{ Classe TSepiVariable }
{----------------------}

{*
  Charge une variable ou une constante typée depuis un flux
*}
constructor TSepiVariable.Load(AOwner: TSepiMeta; Stream: TStream);
var
  DataStored: Boolean;
  DummyValue: Pointer;
begin
  inherited;

  Stream.ReadBuffer(FIsConst, 1);
  OwningUnit.ReadRef(Stream, FType);

  if Assigned(OwningUnit.OnGetVarAddress) then
    OwningUnit.OnGetVarAddress(Self, FValue);

  if FValue = nil then
  begin
    // Allocate value
    GetMem(FValue, FType.Size);
    FillChar(FValue^, FType.Size, 0);
    FOwnValue := True;

    // Load value contents from stream, if stored
    Stream.ReadBuffer(DataStored, 1);
    if DataStored then
      ReadDataFromStream(Stream, FValue^, FType.Size, FType.TypeInfo);
  end else
  begin
    Stream.ReadBuffer(DataStored, 1);

    { If data are stored, we must skip it from the stream.  But since there is
      no way of knowing the amount of bytes to skip, we must actually read the
      data into dummy memory. }
    if DataStored then
    begin
      DummyValue := FType.NewValue;
      try
        ReadDataFromStream(Stream, DummyValue^, FType.Size, FType.TypeInfo);
      finally
        FType.DisposeValue(DummyValue);
      end;
    end;
  end;
end;

{*
  Importe une variable ou constante typée native
  @param AOwner     Propriétaire de la variable
  @param AName      Nom de la variable
  @param AValue     Valeur de la variable
  @param AType      Type de la variable
  @param AIsConst   Indique si c'est une constante typée
*}
constructor TSepiVariable.Create(AOwner: TSepiMeta; const AName: string;
  const AValue; AType: TSepiType; AIsConst: Boolean = False);
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
constructor TSepiVariable.Create(AOwner: TSepiMeta; const AName: string;
  const AValue; ATypeInfo: PTypeInfo; AIsConst: Boolean = False);
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
constructor TSepiVariable.Create(AOwner: TSepiMeta; const AName: string;
  const AValue; const ATypeName: string; AIsConst: Boolean = False);
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
constructor TSepiVariable.Create(AOwner: TSepiMeta; const AName: string;
  AType: TSepiType; AIsConst: Boolean = False);
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
constructor TSepiVariable.Create(AOwner: TSepiMeta; const AName: string;
  ATypeInfo: PTypeInfo; AIsConst: Boolean = False);
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
constructor TSepiVariable.Create(AOwner: TSepiMeta; const AName: string;
  const ATypeName: string; AIsConst: Boolean = False);
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
procedure TSepiVariable.Save(Stream: TStream);
var
  DataStored: Boolean;
  I: Integer;
begin
  inherited;

  Stream.WriteBuffer(FIsConst, 1);
  OwningUnit.WriteRef(Stream, FType);

  DataStored := False;
  for I := 0 to FType.Size-1 do
  begin
    if PByte(Cardinal(FValue) + I)^ <> 0 then
    begin
      DataStored := True;
      Break;
    end;
  end;

  Stream.WriteBuffer(DataStored, 1);
  if DataStored then
    WriteDataToStream(Stream, FValue^, FType.Size, FType.TypeInfo);
end;

{*
  [@inheritDoc]
*}
procedure TSepiVariable.Destroying;
begin
  inherited;

  if FOwnValue then
    FType.FinalizeValue(FValue^);
end;

{----------------------}
{ TSepiNamespace class }
{----------------------}

{*
  [@inheritDoc]
*}
constructor TSepiNamespace.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FVirtualOwner);
  LoadChildren(Stream);
end;

{*
  Crée un espace de noms
  @param AOwner          Propriétaire de l'espace de noms
  @param AName           Nom de l'espace de noms
  @param AVirtualOwner   Propriétaire virtuel (défaut = nil, utilise Owner)
*}
constructor TSepiNamespace.Create(AOwner: TSepiMeta; const AName: string;
  AVirtualOwner: TSepiMeta);
begin
  inherited Create(AOwner, AName);

  FVirtualOwner := AVirtualOwner;
end;

{*
  [@inheritDoc]
*}
procedure TSepiNamespace.ListReferences;
begin
  inherited;

  OwningUnit.AddRef(FVirtualOwner);
end;

{*
  [@inheritDoc]
*}
procedure TSepiNamespace.Save(Stream: TStream);
begin
  inherited;

  OwningUnit.WriteRef(Stream, FVirtualOwner);
  SaveChildren(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiNamespace.InternalLookFor(const Name: string; FromUnit: TSepiUnit;
  FromClass: TSepiMeta): TSepiMeta;
begin
  if VirtualOwner = nil then
    Result := inherited InternalLookFor(Name, FromUnit, FromClass)
  else
  begin
    // Basic search
    Result := GetMeta(Name);

    // Check for visibility
    if (Result <> nil) and (not Result.IsVisibleFrom(FromUnit, FromClass)) then
      Result := nil;

    // If not found, continue search in the virtual owner
    if Result = nil then
      Result := VirtualOwner.InternalLookFor(Name, FromUnit, FromClass);
  end;
end;

{*
  Complète l'espace de noms
*}
procedure TSepiNamespace.Complete;
begin
  Owner.ReAddChild(Self);
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
constructor TSepiUnitLoadTask.Create(AOwner: TScCustomTaskQueue;
  ARoot: TSepiRoot; const AUnitName: string; AIsLoad: Boolean;
  AFreeOnFinished: Boolean);
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
constructor TSepiUnitLoadTask.Create(AOwner: TScCustomTaskQueue;
  ARoot: TSepiRoot; const AUnitName: string; AIsLoad: Boolean);
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
constructor TSepiAsynchronousRootManager.Create(ARoot: TSepiRoot = nil);
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
procedure TSepiAsynchronousRootManager.LoadUnit(const UnitName: string);
begin
  TSepiUnitLoadTask.Create(Self, Root, UnitName, True);
end;

{*
  Demande le déchargement d'une unité
  @param UnitName   Unité à charger
*}
procedure TSepiAsynchronousRootManager.UnloadUnit(const UnitName: string);
begin
  TSepiUnitLoadTask.Create(Self, Root, UnitName, False);
end;

initialization
  SepiRegisterMetaClasses([
    TSepiUnit, TSepiTypeAlias, TSepiConstant, TSepiVariable, TSepiNamespace
  ]);
finalization
  SepiMetaClasses.Free;
  SepiImportedUnits.Free;
end.

