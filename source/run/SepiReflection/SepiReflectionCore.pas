{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  S�bastien Doeraene
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

Linking this library statically or dynamically with other modules is making a
combined work based on this library.  Thus, the terms and conditions of the GNU
General Public License cover the whole combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.
-------------------------------------------------------------------------------}

{*
  D�finit les classes de gestion des meta-unit�s
  @author sjrd
  @version 1.0
*}
unit SepiReflectionCore;

interface

{$ASSERTIONS ON}

uses
  Types, Windows, SysUtils, Classes, Contnrs, RTLConsts, IniFiles, TypInfo,
  Variants, StrUtils, ScUtils, ScStrUtils, ScSyncObjs, ScCompilerMagic,
  ScSerializer, ScTypInfo, ScMD5, SepiCore, SepiReflectionConsts;

type
  TSepiComponent = class;
  TSepiContainerType = class;
  TSepiRoot = class;
  TSepiUnit = class;
  TSepiAsynchronousRootManager = class;

  /// Digest de compatibilit� d'un composant Sepi
  TSepiDigest = TMD5Digest;

  {*
    �tat d'un composant
  *}
  TSepiComponentState = (msForward, msLoading, msConstructing, msNormal,
    msDestroying);

  {*
    Visibilit� d'un composant
  *}
  TMemberVisibility = (mvStrictPrivate, mvPrivate, mvStrictProtected,
    mvProtected, mvPublic, mvPublished);

  {*
    Type de l'�v�nement OnLoadUnit de TSepiRoot
    @param Sender     Racine Sepi qui demande le chargement d'une unit�
    @param UnitName   Nom de l'unit� � charger
    @return L'unit� charg�e, ou nil si l'unit� n'est pas trouv�e
  *}
  TSepiLoadUnitEvent = function(Sender: TSepiRoot;
    const UnitName: string): TSepiUnit of object;

  {*
    Type de l'�v�nement OnGetMethodCode de TSepiUnit
    @param Sender        M�thode d�clenchant l'�v�nement (toujours TSepiMethod)
    @param Code          Adresse de code de la m�thode
    @param CodeHandler   Gestionnaire de code
  *}
  TGetMethodCodeEvent = procedure(Sender: TObject; var Code: Pointer;
    var CodeHandler: TObject) of object;

  {*
    Type de l'�v�nement OnGetTypeInfo de TSepiUnit
    Si TypeInfo est laiss� � nil et si Found est laiss� � False, Sepi cr�era
    ses propres RTTI pour le type. Ce cas ne devrait se pr�senter que pour des
    types anonymes pour lesquels le code est incapable de trouver les RTTI
    natives.
    @param Sender     Type d�clenchant l'�v�nement (toujours TSepiType)
    @param TypeInfo   RTTI du type
    @param Found      Positionnez � True si les RTTI ont �t� trouv�es
  *}
  TGetTypeInfoEvent = procedure(Sender: TObject; var TypeInfo: PTypeInfo;
    var Found: Boolean) of object;

  {*
    Type de l'�v�nement OnGetVarAddress de TSepiUnit
    Si VarAddress est laiss�e � nil, Sepi allouera lui-m�me un espace m�moire
    pour la variable. Cela ne devrait toutefois normalement jamais arriver. 
    @param Sender       Variable d�clenchant l'�v�nement (type TSepiVariable)
    @param VarAddress   Adresse de la variable
  *}
  TGetVarAddressEvent = procedure(Sender: TObject;
    var VarAddress: Pointer) of object;

  {*
    D�clench�e si l'on tente de recr�er un composant
    (second appel au constructeur)
    @author sjrd
    @version 1.0
  *}
  ESepiComponentAlreadyCreated = class(ESepiError);

  {*
    D�clench�e lorsque la recherche d'un composant s'est sold�e par un �chec
    @author sjrd
    @version 1.0
  *}
  ESepiComponentNotFoundError = class(ESepiError);

  {*
    D�clench�e lorsque la recherche d'une unit� s'est sold�e par un �chec
    @author sjrd
    @version 1.0
  *}
  ESepiUnitNotFoundError = class(ESepiComponentNotFoundError);

  {*
    D�clench�e si une unit� utilis�e est incompatible avec les digests connus
    @author sjrd
    @version 1.0
  *}
  ESepiIncompatibleUsedUnitError = class(ESepiError);

  {*
    D�clench�e si l'on tente de cr�er une constante avec un mauvais type
    @author sjrd
    @version 1.0
  *}
  ESepiBadConstTypeError = class(ESepiError);

  {*
    D�clench�e lorsqu'on tente de modifier un �l�ment d�j� compl�t�
    @author sjrd
    @version 1.0
  *}
  ESepiAlreadyCompleted = class(ESepiError);

  {*
    Liste de composants Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiComponentList = class(THashedStringList)
  private
    function GetComponents(Index: Integer): TSepiComponent;
    procedure SetComponents(Index: Integer; Value: TSepiComponent);
    function GetComponentFromName(const Name: string): TSepiComponent;
    procedure SetComponentFromName(const Name: string; Value: TSepiComponent);
  protected
    procedure InsertItem(Index: Integer; const S: string;
      AObject: TObject); override;
  public
    constructor Create;

    function AddComponent(Component: TSepiComponent): Integer;
    function IndexOfComponent(Component: TSepiComponent): Integer;
    function Remove(Component: TSepiComponent): Integer;

    property Components[Index: Integer]: TSepiComponent
      read GetComponents write SetComponents; default;
    property ComponentFromName[const Name: string]: TSepiComponent
      read GetComponentFromName write SetComponentFromName;
  end;

  {*
    Composant Sepi
    Les composants Sepi forment un arbre (dont la racine est de type TSepiRoot)
    de tous les �l�ments de r�flexion connus de Sepi.
    @author sjrd
    @version 1.0
  *}
  TSepiComponent = class
  private
    FState: TSepiComponentState;    /// �tat
    FOwner: TSepiComponent;         /// Propri�taire
    FRoot: TSepiRoot;               /// Racine
    FOwningUnit: TSepiUnit;         /// Unit� contenante
    FName: string;                  /// Nom
    FVisibility: TMemberVisibility; /// Visibilit�
    /// Visibilit� avec laquelle les enfants de ce composant seront cr��s
    FCurrentVisibility: TMemberVisibility;
    FDigestBuilt: Boolean;          /// True ssi FDigest a d�j� �t� construit
    FDigest: TSepiDigest;           /// Digest de compatibilit�
    FTag: Integer;                  /// Tag
    FForwards: TStrings;            /// Liste des enfants forwards
    FChildren: TSepiComponentList;  /// Liste des enfants
    FUnnamedChildCount: Integer;    /// Nombre d'enfants cr��s anonymes
    FObjResources: TObjectList;     /// Liste des ressources objet
    FPtrResources: TList;           /// Liste des ressources pointeur

    procedure EnsureDigestBuilt;

    procedure AddForward(Child: TSepiComponent);

    function GetIsForward: Boolean;
    function GetWasForward: Boolean;

    function GetTypeInfoName: TypeInfoString;
    function GetTypeInfoNameSize: Integer;

    function GetDigest: TSepiDigest;

    function GetChildCount: Integer;
    function GetChildren(Index: Integer): TSepiComponent;

    function GetChildByName(const ChildName: string): TSepiComponent;
  protected
    procedure AddChild(Child: TSepiComponent); virtual;
    procedure RemoveChild(Child: TSepiComponent); virtual;
    procedure ReAddChild(Child: TSepiComponent); virtual;

    procedure LoadForwards(Stream: TStream); virtual;
    function LoadChild(Stream: TStream): TSepiComponent; virtual;
    procedure SaveForwards(Stream: TStream); virtual;
    procedure SaveChild(Stream: TStream; Child: TSepiComponent); virtual;

    procedure LoadChildren(Stream: TStream); virtual;
    procedure SaveChildren(Stream: TStream); virtual;

    procedure ChildAdded(Child: TSepiComponent); virtual;
    procedure ChildRemoving(Child: TSepiComponent); virtual;

    procedure Loaded; virtual;

    procedure ListReferences; virtual;
    procedure Save(Stream: TStream); virtual;

    procedure Destroying; virtual;

    function InternalGetComponent(const Name: string): TSepiComponent; virtual;

    function InternalLookFor(const Name: string;
      FromComponent: TSepiComponent): TSepiComponent; virtual;

    function GetDisplayName: string; virtual;

    procedure WriteDigestData(Stream: TStream); virtual;

    property State: TSepiComponentState read FState;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); virtual;
    constructor Create(AOwner: TSepiComponent; const AName: string);
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function GetFullName: string;
    function GetShorterNameFrom(From: TSepiComponent): string;
    function GetComponent(const Name: string): TSepiComponent;
    function FindComponent(const Name: string): TSepiComponent;

    function FindContainer: TSepiContainerType;
    function IsAncestor(Ancestor: TSepiComponent): Boolean;

    function IsVisibleFrom(FromComponent: TSepiComponent): Boolean; virtual;

    function LookFor(const Name: string;
      FromComponent: TSepiComponent): TSepiComponent; overload;
    function LookFor(const Name: string): TSepiComponent; overload;

    function StoreTypeInfoName(Dest: PTypeInfoString): Pointer;

    procedure WriteDigestToStream(Stream: TStream);

    function CheckDigest(const ADigest: TSepiDigest): Boolean; overload;
    function CheckDigest(const ADigest: string): Boolean; overload;

    function MakeUnnamedChildName: string;

    procedure AddObjResource(Obj: TObject);
    procedure AcquireObjResource(var Obj);
    procedure AddPtrResource(Ptr: Pointer);
    procedure AcquirePtrResource(var Ptr);

    property IsForward: Boolean read GetIsForward;
    property WasForward: Boolean read GetWasForward;
    property Owner: TSepiComponent read FOwner;
    property Root: TSepiRoot read FRoot;
    property OwningUnit: TSepiUnit read FOwningUnit;
    property Name: string read FName;
    property TypeInfoName: TypeInfoString read GetTypeInfoName;
    property TypeInfoNameSize: Integer read GetTypeInfoNameSize;
    property DisplayName: string read GetDisplayName;
    property Visibility: TMemberVisibility read FVisibility write FVisibility;
    property CurrentVisibility: TMemberVisibility
      read FCurrentVisibility write FCurrentVisibility;
    property Digest: TSepiDigest read GetDigest;
    property Tag: Integer read FTag write FTag;

    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: TSepiComponent read GetChildren;
    property ChildByName[const ChildName: string]: TSepiComponent
      read GetChildByName; default;
  end;

  {*
    Classe de TSepiComponent
  *}
  TSepiComponentClass = class of TSepiComponent;

  {*
    Comportement d'un type lorsqu'il est pass� en param�tre
    @author sjrd
    @version 1.0
  *}
  TSepiTypeParamBehavior = record
    AlwaysByAddress: Boolean; /// Le type est toujours pass� par adresse
    AlwaysByStack: Boolean;   /// Le type est toujours pass� sur la pile
  end;

  {*
    Comportement d'un type lorsqu'il est valeur de retour
    - rbNone : pas de r�sultat (proc�dure)
    - rbOrdinal : renvoy� comme un ordinal, via EAX
    - rbInt64 : renvoy� comme Int64, via EDX:EAX
    - rbSingle : renvoy� comme Single, via ST(0)
    - rbDouble : renvoy� comme Double, via ST(0)
    - rbExtended : renvoy� comme Extended, via ST(0)
    - rbCurrency : renvoy� comme Currency, via ST(0)
    - rbParameter : l'adresse du r�sultat est pass�e en param�tre
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
  TSepiType = class(TSepiComponent)
  private
    FKind: TTypeKind;         /// Type de type
    FCompleted: Boolean;      /// Indique si le type est complet
    FIsReady: Boolean;        /// Indique si le type est pr�t
    FNative: Boolean;         /// Indique si le type est un type natif Delphi
    FTypeInfoLength: Integer; /// Taille des RTTI cr��es (ou 0 si non cr��es)
    FTypeInfo: PTypeInfo;     /// RTTI (Runtime Type Information)
    FTypeData: PTypeData;     /// RTTD (Runtime Type Data)
    FTypeInfoRef: PPTypeInfo; /// R�f�rence aux RTTI
  protected
    FSize: Integer;     /// Taille d'une variable de ce type
    FNeedInit: Boolean; /// Indique si ce type requiert une initialisation

    FParamBehavior: TSepiTypeParamBehavior;   /// Comportement comme param�tre
    FResultBehavior: TSepiTypeResultBehavior; /// Comportement comme r�sultat

    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    class function ForwardDecl(AOwner: TSepiComponent;
      const AName: string): TSepiType;

    procedure AllocateTypeInfo(TypeDataLength: Integer = 0);

    function HasTypeInfo: Boolean; virtual;
    procedure SetupProperties; virtual;
    procedure MakeTypeInfo; virtual;
    procedure MakeRuntimeInfo; virtual;

    function IsComposite: Boolean; virtual;
    function IsStrongComposite: Boolean; virtual;
    procedure Complete; virtual;

    function GetAlignment: Integer; virtual;
    function GetSafeResultBehavior: TSepiTypeResultBehavior;

    function GetDisplayName: string; override;
    function GetDescription: string; virtual;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AKind: TTypeKind);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); virtual;
    destructor Destroy; override;

    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;

    procedure AlignOffset(var Offset: Integer);

    procedure InitializeValue(var Value);
    procedure FinalizeValue(var Value);
    function NewValue: Pointer;
    procedure DisposeValue(Value: Pointer);
    procedure CopyData(const Source; var Dest);
    function ValueToString(const Value): string; virtual;

    function Equals(Other: TSepiType): Boolean;
      {$IF RTLVersion >= 20.0} reintroduce; {$IFEND} virtual;
    function CompatibleWith(AType: TSepiType): Boolean; virtual;

    property Kind: TTypeKind read FKind;
    property Completed: Boolean read FCompleted;
    property IsReady: Boolean read FIsReady;
    property Native: Boolean read FNative;
    property TypeInfo: PTypeInfo read FTypeInfo;
    property TypeData: PTypeData read FTypeData;
    property TypeInfoRef: PPTypeInfo read FTypeInfoRef;
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
    Type non typ�
    @author sjrd
    @version 1.0
  *}
  TSepiUntypedType = class(TSepiType)
  protected
    procedure SetupProperties; override;

    function GetAlignment: Integer; override;

    function GetDescription: string; override;
  public
    constructor Create(AOwner: TSepiComponent; const AName: string);

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;
  end;

  {*
    Membre d'un type conteneur
    Les classes qui h�ritent de TSepiMember ne sont pas n�cessairement toujours
    des membres. Ce sont les composants qui sont susceptibles, dans certains
    circonstances, d'�tre des membres.
    @author sjrd
    @version 1.0
  *}
  TSepiMember = class(TSepiComponent)
  private
    function GetContainer: TSepiContainerType;
  public
    property Container: TSepiContainerType read GetContainer;
  end;

  {*
    Type conteneur
    @author sjrd
    @version 1.0
  *}
  TSepiContainerType = class(TSepiType)
  private
    FLoadingStream: TStream; /// Flux de chargement
  protected
    function IsComposite: Boolean; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;

    procedure AfterConstruction; override;

    function LookForMember(const MemberName: string;
      FromComponent: TSepiComponent): TSepiMember; overload; virtual;
    function LookForMember(const MemberName: string): TSepiMember; overload;
  end;

  {*
    Racine de l'arbre de r�flexion
    @author sjrd
    @version 1.0
  *}
  TSepiRoot = class(TSepiComponent)
  private
    FSearchOrder: TObjectList;       /// Ordre de recherche
    FOnLoadUnit: TSepiLoadUnitEvent; /// D�clench� au chargement d'une unit�

    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): TSepiUnit;
  protected
    procedure AddChild(Child: TSepiComponent); override;
    procedure RemoveChild(Child: TSepiComponent); override;
    procedure ReAddChild(Child: TSepiComponent); override;

    function InternalLookFor(const Name: string;
      FromComponent: TSepiComponent): TSepiComponent; override;

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

    function GetClass(DelphiClass: TClass): TSepiType;
    function FindClass(DelphiClass: TClass): TSepiType;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: TSepiUnit read GetUnits;

    /// Unit� System - devrait �tre du type SepiSystemUnit.TSepiSystemUnit
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
    FForeignRefCounts: TStrings; /// Ref-counts des unit�s �trang�res
    FDestroying: Boolean;        /// True lorsqu'en destruction

    function LoadUnitFromOriginalRoot(const UnitName: string): TSepiUnit;
  protected
    procedure AddChild(Child: TSepiComponent); override;
    procedure RemoveChild(Child: TSepiComponent); override;

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
    Donn�es de lazy-load pour un enfant
    @author sjrd
    @version 1.0
  *}
  TSepiLazyLoadChildData = record
    Name: string;                 /// Nom de l'enfant
    Position: Int64;              /// Position dans le flux
    Kind: TSepiLazyLoadChildKind; /// Type d'enfant
    Inheritance: TStringDynArray; /// H�ritage d'une classe/interface
  end;

  /// Tableau de donn�es lazy-load pour les enfants
  TSepiLazyLoadChildrenData = array of TSepiLazyLoadChildData;

  {*
    Donn�es de lazy-load
    @author sjrd
    @version 1.0
  *}
  TSepiLazyLoadData = class(TObject)
  private
    FOwner: TSepiComponent; /// Propri�taire des enfants charg�s
    FStream: TStream;  /// Flux o� charger les enfants

    FChildrenNames: TStrings;                       /// Noms des enfants
    FChildrenData: array of TSepiLazyLoadChildData; /// Donn�es sur les enfants

    FLoadingClassIntfChildren: TStrings; /// Classes et interfaces en chargement
    FForwardClassIntfChildren: TObjectList; /// Classes et interfaces forward

    function GetChildData(const Name: string): TSepiLazyLoadChildData;
    function CanLoad(const Data: TSepiLazyLoadChildData): Boolean;
    procedure InternalLoadChild(const Data: TSepiLazyLoadChildData;
      var Child: TSepiComponent);
    procedure RetryForwards;
  public
    constructor Create(AOwner: TSepiComponent; AStream: TStream);
    destructor Destroy; override;

    procedure LoadFromStream(SkipChildrenInfo: Boolean);
    function ChildExists(const Name: string): Boolean;
    function LoadChild(const Name: string): TSepiComponent;

    class procedure SaveToStream(Owner: TSepiComponent; Stream: TStream);

    property Owner: TSepiComponent read FOwner;
    property Stream: TStream read FStream;
  end;

  {*
    Unit�
    @author sjrd
    @version 1.0
  *}
  TSepiUnit = class(TSepiComponent)
  private
    /// D�clench� pour chaque m�thode au chargement, pour obtenir son code
    FOnGetMethodCode: TGetMethodCodeEvent;
    /// D�clench� pour chaque type au chargement, pour obtenir ses RTTI
    FOnGetTypeInfo: TGetTypeInfoEvent;
    /// D�clench� pour chaque variable au chargement, pour obtenir son adresse
    FOnGetVarAddress: TGetVarAddressEvent;

    FRefCount: Integer;                    /// Compteur de r�f�rences
    FUsesList: TStrings;                   /// Liste des uses
    FCurrentVisibility: TMemberVisibility; /// Visibilit� courante

    FReferences: array of TStrings; /// R�f�rences en chargement/sauvegarde

    FLazyLoad: Boolean;               /// True si mode lazy-load
    FLazyLoadData: TSepiLazyLoadData; /// Donn�es de lazy-load

    function AddUses(AUnit: TSepiUnit): Integer;

    function LazyLoadChild(const Name: string): TSepiComponent;

    procedure SetCurrentVisibility(Value: TMemberVisibility);

    function GetUsedUnitCount: Integer;
    function GetUsedUnits(Index: Integer): TSepiUnit;
  protected
    procedure LoadChildren(Stream: TStream); override;
    procedure SaveChildren(Stream: TStream); override;

    procedure Save(Stream: TStream); override;

    function InternalGetComponent(const Name: string): TSepiComponent; override;

    function InternalLookFor(const Name: string;
      FromComponent: TSepiComponent): TSepiComponent; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      const AUses: array of string);
    destructor Destroy; override;

    procedure MoreUses(const AUses: array of string);

    procedure Complete;

    procedure SaveToStream(Stream: TStream);
    class function LoadFromStream(AOwner: TSepiComponent; Stream: TStream;
      ALazyLoad: Boolean = False;
      const AOnGetMethodCode: TGetMethodCodeEvent = nil;
      const AOnGetTypeInfo: TGetTypeInfoEvent = nil;
      const AOnGetVarAddress: TGetVarAddressEvent = nil): TSepiUnit;

    procedure ReadRef(Stream: TStream; out Ref);
    procedure AddRef(Ref: TSepiComponent);
    procedure WriteRef(Stream: TStream; Ref: TSepiComponent);

    function GetClass(DelphiClass: TClass): TSepiType;
    function FindClass(DelphiClass: TClass): TSepiType;

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
    Alias d'unit�
    Un alias d'unit� permet de regrouper une � plusieurs unit�s r�elle sous un
    m�me nom d'unit�. Les unit�s r�elles sont en fait stock�es comme les uses de
    l'alias d'unit�.
    Ainsi, lorsqu'on cherche un composant dans un alias d'unit�, la recherche
    s'effectue aussi dans les unit�s utilis�es par cet alias, m�me si on vient
    d'une autre unit�.
    @author sjrd
    @version 1.0
  *}
  TSepiUnitAlias = class(TSepiUnit)
  protected
    function InternalLookFor(const Name: string;
      FromComponent: TSepiComponent): TSepiComponent; override;
  end;

  {*
    Alias de type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeAlias = class(TSepiComponent)
  private
    FDest: TSepiType; /// Type destination
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      ADest: TSepiType);

    property Dest: TSepiType read FDest;
  end;

  {*
    Constante
    @author sjrd
    @version 1.0
  *}
  TSepiConstant = class(TSepiComponent)
  private
    FType: TSepiType;   /// Type de la constante
    FValuePtr: Pointer; /// Pointeur sur la valeur
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AType: TSepiType); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AType: TSepiType; const AValue); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      const AValue: Variant; AType: TSepiType); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      const AValue: Variant); overload;
    destructor Destroy; override;

    property ConstType: TSepiType read FType;
    property ValuePtr: Pointer read FValuePtr;
  end;

  {*
    Variable (ou constante typ�e)
    TSepiVariable est aussi la classe utilis�e pour les champs de classe, ce qui
    explique qu'elle h�rite de TSepiMember.
    @author sjrd
    @version 1.0
  *}
  TSepiVariable = class(TSepiMember)
  private
    FIsConst: Boolean;  /// Indique si la variable est une constante typ�e
    FType: TSepiType;   /// Type de la constante
    FValue: Pointer;    /// Pointeur sur la variable
    FOwnValue: Boolean; /// Indique si Sepi a allou� la variable
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure Destroying; override;

    procedure WriteDigestData(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      const AValue; AType: TSepiType; AIsConst: Boolean = False); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AType: TSepiType; AIsConst: Boolean = False); overload;
    destructor Destroy; override;

    property IsConst: Boolean read FIsConst;
    property VarType: TSepiType read FType;
    property Value: Pointer read FValue;
  end;

  {*
    Espace de noms (d'�tendue plus petite qu'une unit�)
    Un espace de noms peut avoir un propri�taire virtuel. S'il en a, la m�thode
    LookFor continue sa recherche d'un identificateur via le propri�taire
    virtuel, plut�t que le propri�taire r�el.
    @author sjrd
    @version 1.0
  *}
  TSepiNamespace = class(TSepiComponent)
  private
    FVirtualOwner: TSepiComponent; /// Propri�taire virtuel
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    function InternalLookFor(const Name: string;
      FromComponent: TSepiComponent): TSepiComponent; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AVirtualOwner: TSepiComponent = nil);

    procedure Complete;

    property VirtualOwner: TSepiComponent read FVirtualOwner;
  end;

  {*
    T�che de chargement/d�chargement d'une unit� Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiUnitLoadTask = class(TScTask)
  private
    FRoot: TSepiRoot;  /// Racine Sepi
    FName: string;     /// Nom de l'unit� � charger/d�charger
    FIsLoad: Boolean;  /// True pour charger, False pour d�charger
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TScCustomTaskQueue; ARoot: TSepiRoot;
      const AUnitName: string; AIsLoad: Boolean;
      AFreeOnFinished: Boolean); overload;
    constructor Create(AOwner: TScCustomTaskQueue; ARoot: TSepiRoot;
      const AUnitName: string; AIsLoad: Boolean); overload;

    property Root: TSepiRoot read FRoot;
    property Name: string read FName;
    property IsLoad: Boolean read FIsLoad;
  end;

  {*
    Gestionnaire asynchrone de racine Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiAsynchronousRootManager = class(TScTaskQueue)
  private
    FOwnsRoot: Boolean; /// Indique si l'on poss�de la racine
    FRoot: TSepiRoot;   /// Component-racine g�r�e
  public
    constructor Create(ARoot: TSepiRoot = nil);
    destructor Destroy; override;

    procedure LoadUnit(const UnitName: string);
    procedure UnloadUnit(const UnitName: string);

    property Root: TSepiRoot read FRoot;
  end;

  {*
    Type de routine call-back pour l'import d'une unit� sous Sepi
  *}
  TSepiImportUnitFunc = function(Root: TSepiRoot): TSepiUnit;

procedure SepiRegisterComponentClasses(
  const ComponentClasses: array of TSepiComponentClass);

procedure SepiRegisterImportedUnit(const UnitName: string;
  ImportFunc: TSepiImportUnitFunc);
procedure SepiUnregisterImportedUnit(const UnitName: string);
function SepiImportedUnit(const UnitName: string): TSepiImportUnitFunc;

const {don't localize}
  SystemUnitName = 'System'; /// Nom de l'unit� System.pas

const
  /// Comportement par des d�faut d'un type comme param�tre
  DefaultTypeParamBehavior: TSepiTypeParamBehavior = (
    AlwaysByAddress: False; AlwaysByStack: False
  );

  /// Cha�nes des visibilit�s
  VisibilityStrings: array[TMemberVisibility] of string = (
    'strict private', 'private', 'strict protected', 'protected', 'public',
    'published'
  );

implementation

uses
  SepiOrdTypes, SepiStrTypes, SepiArrayTypes, SepiMembers, SepiSystemUnit;

var
  SepiComponentClasses: TStrings = nil;
  SepiImportedUnits: TStrings = nil;

{*
  Recense des classes de composants Sepi
  @param ComponentClasses   Classes de composants � recenser
*}
procedure SepiRegisterComponentClasses(
  const ComponentClasses: array of TSepiComponentClass);
var
  I: Integer;
begin
  if not Assigned(SepiComponentClasses) then
  begin
    SepiComponentClasses := TStringList.Create;
    with TStringList(SepiComponentClasses) do
    begin
      CaseSensitive := False;
      Sorted := True;
      Duplicates := dupIgnore;
    end;
  end;

  for I := Low(ComponentClasses) to High(ComponentClasses) do
  begin
    SepiComponentClasses.AddObject(
      ComponentClasses[I].ClassName, TObject(ComponentClasses[I]));
  end;
end;

{*
  Cherche une classe de composant par son nom
  La classe recherch�e doit avoir �t� recens�e au pr�alable avec
  SepiRegisterComponentClasses.
  @param ComponentClassName   Nom de la classe de composant
  @return La classe de composant dont le nom correspond
  @throws EClassNotFound La classe recherch�e n'existe pas
*}
function SepiFindComponentClass(
  const ComponentClassName: string): TSepiComponentClass;
var
  Index: Integer;
begin
  if not Assigned(SepiComponentClasses) then
    Index := -1
  else
    Index := SepiComponentClasses.IndexOf(ComponentClassName);
  if Index < 0 then
    raise EClassNotFound.CreateFmt(SClassNotFound, [ComponentClassName]);

  Result := TSepiComponentClass(SepiComponentClasses.Objects[Index]);
end;

{*
  Recense une routine d'import d'unit�
  @param UnitName     Nom de l'unit�
  @param ImportFunc   Routine de call-back pour l'import de l'unit�
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
  Supprime un recensement d'import d'unit�
  @param UnitName   Nom de l'unit�
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
  Cherche une routine d'import d'une unit�
  Cette routine doit avoir �t� recens�e au pr�lable avec
  SepiRegisterImportedUnit.
  @param UnitName   Nom de l'unit�
  @return Routine de call-back pour l'import de l'unit�, ou nil si n'existe pas
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

{---------------------------}
{ Classe TSepiComponentList }
{---------------------------}

{*
  Cr�e une instance de TSepiComponentList
*}
constructor TSepiComponentList.Create;
begin
  inherited;
  CaseSensitive := False;
end;

{*
  Liste zero-based des composants
  @param Index   Index du composant � obtenir
  @return Composant � l'index sp�cifi�
*}
function TSepiComponentList.GetComponents(Index: Integer): TSepiComponent;
begin
  Result := TSepiComponent(Objects[Index]);
end;

{*
  Assigne la r�f�rence � un composant
  Un composant ne peut pas �tre modif� une fois assign�, il ne peut donc �tre
  assign� qu'une et une seule fois.
  @param Index   Index du meta � modifier
  @param Value   R�f�rence au nouveau composant
*}
procedure TSepiComponentList.SetComponents(Index: Integer;
  Value: TSepiComponent);
begin
  if Assigned(Objects[Index]) then
    Error(@SSepiComponentAlreadyAssigned, Index);
  Objects[Index] := Value;
end;

{*
  Liste des composants index�e par leurs noms
  @param Name   Nom d'un composant
  @return Le composant dont le nom a �t� sp�cifi�, ou nil s'il n'existe pas
*}
function TSepiComponentList.GetComponentFromName(
  const Name: string): TSepiComponent;
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then
    Result := nil
  else
    Result := TSepiComponent(Objects[Index]);
end;

{*
  Assigne ou ajoute un composant par son nom
  Un composant ne peut pas �tre modif� une fois assign�, il ne peut donc �tre
  assign� qu'une et une seule fois.
  @param Name    Nom du composant
  @param Value   R�f�rence au composant
*}
procedure TSepiComponentList.SetComponentFromName(const Name: string;
  Value: TSepiComponent);
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then
    AddObject(Name, Value)
  else
    Components[Index] := Value;
end;

{*
  Ajoute un �l�ment dans la liste de composants
  @param Index     Index o� ajouter l'�l�ment
  @param S         Cha�ne � ajouter
  @param AObject   Objet � ajouter
*}
procedure TSepiComponentList.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
begin
  if IndexOf(S) >= 0 then
    raise EListError.CreateFmt(SSepiComponentAlreadyExists, [S]);
  inherited;
end;

{*
  Ajoute un composant
  @param Component   Composant � ajouter
  @return Index du composant nouvellement ajout�
*}
function TSepiComponentList.AddComponent(Component: TSepiComponent): Integer;
begin
  Result := AddObject(Component.Name, Component);
end;

{*
  Cherche un composant dans la liste
  @param Component   Composant � chercher
  @return L'index du composant dans la liste, ou nil s'il n'existe pas
*}
function TSepiComponentList.IndexOfComponent(
  Component: TSepiComponent): Integer;
begin
  Result := IndexOf(Component.Name);
end;

{*
  Supprime un composant de la liste
  @param Component   Composant � supprimer
  @return L'index auquel se trouvait le composant
*}
function TSepiComponentList.Remove(Component: TSepiComponent): Integer;
begin
  Result := IndexOfComponent(Component);
  if Result >= 0 then
    Delete(Result);
end;

{-----------------------}
{ Classe TSepiComponent }
{-----------------------}

{*
  Charge un composant depuis un flux
  @param AOwner   Propri�taire du composant
  @param Stream   Flux depuis lequel charger le composant
*}
constructor TSepiComponent.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  Create(AOwner, ReadStrFromStream(Stream));

  FState := msLoading;

  Stream.ReadBuffer(FVisibility, 1);
  Stream.ReadBuffer(FTag, SizeOf(Integer));
end;

{*
  Cr�e un nouveau composant
  @param AOwner   Propri�taire du composant
  @param AName    Nom du composant
*}
constructor TSepiComponent.Create(AOwner: TSepiComponent; const AName: string);
begin
  if not IsForward then
    raise ESepiComponentAlreadyCreated.CreateFmt(
      SSepiComponentAlreadyCreated, [Name]);

  inherited Create;

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
  FForwards := THashedStringList.Create;
  FChildren := TSepiComponentList.Create;
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
  [@inheritDoc]
*}
destructor TSepiComponent.Destroy;
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
    for I := FChildren.Count-1 downto 0 do
      FChildren.Objects[I].Free;
    FChildren.Free;
  end;

  FForwards.Free;

  if Assigned(FOwner) then
    FOwner.RemoveChild(Self);

  inherited;
end;

{*
  S'assure que le digest de compatibilit� a �t� construit
*}
procedure TSepiComponent.EnsureDigestBuilt;
var
  Stream: TStream;
begin
  if not FDigestBuilt then
  begin
    Stream := TMemoryStream.Create;
    try
      WriteDigestData(Stream);
      FDigest := MD5Stream(Stream);
      FDigestBuilt := True;
    finally
      Stream.Free;
    end;
  end;
end;

{*
  Ajoute un enfant forward
  @param Child   Enfant � ajouter
*}
procedure TSepiComponent.AddForward(Child: TSepiComponent);
begin
  FForwards.AddObject(Child.Name, Child);
end;

{*
  Indique si le composant est encore en forward
  @return True si le composant est encored en forward, False sinon
*}
function TSepiComponent.GetIsForward: Boolean;
begin
  Result := FState = msForward;
end;

{*
  Indique si le composant a �t� cr�� forward
  @return True si le composant a �t� cr�� forward, False sinon
*}
function TSepiComponent.GetWasForward: Boolean;
begin
  Result := (FOwner <> nil) and (FOwner.FForwards.IndexOfObject(Self) >= 0);
end;

{*
  Nom � stocker dans les RTTI
  @return Nom � stocker dans les RTTI
*}
function TSepiComponent.GetTypeInfoName: TypeInfoString;
begin
  Result := TypeInfoEncode(Name);
end;

{*
  Taille du nom � stocker dans les RTTI
  @return Taille du nom � stocker dans les RTTI
*}
function TSepiComponent.GetTypeInfoNameSize: Integer;
begin
  Result := Length(TypeInfoName) + 1;
end;

{*
  Digest de compatibilit�
  @return Digest de compatibilit�
*}
function TSepiComponent.GetDigest: TSepiDigest;
begin
  EnsureDigestBuilt;
  Result := FDigest;
end;

{*
  Nombre d'enfants
  @return Nombre d'enfants
*}
function TSepiComponent.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

{*
  Tableau zero-based des enfants
  @param Index   Index de l'enfant � r�cup�rer
  @return Enfant � l'index sp�cifi�
*}
function TSepiComponent.GetChildren(Index: Integer): TSepiComponent;
begin
  Result := TSepiComponent(FChildren[Index]);
end;

{*
  Tableau des enfants index�s par leurs noms
  � l'inverse de la fonction FindComponent, les noms compos�s ne sont pas
  accept�s.
  @param ChildName   Nom de l'enfant recherch�
  @return Enfant dont le nom est ChildName
  @throws ESepiComponentNotFoundError L'enfant n'a pas �t� trouv�
*}
function TSepiComponent.GetChildByName(const ChildName: string): TSepiComponent;
begin
  Result := FChildren.ComponentFromName[ChildName];
  if Result = nil then
    raise ESepiComponentNotFoundError.CreateFmt(
      SSepiComponentNotFound, [ChildName]);
end;

{*
  Ajoute un enfant
  AddChild est appel�e dans le constructeur du meta enfant, et ne doit pas �tre
  appel�e ailleurs.
  @param Child   Enfant � ajouter
*}
procedure TSepiComponent.AddChild(Child: TSepiComponent);
begin
  FChildren.AddComponent(Child);
end;

{*
  Supprime un enfant
  RemoveChild est appel�e dans le destructeur du meta enfant, et ne doit pas
  �tre appel�e ailleurs.
  @param Child   Enfant � supprimer
*}
procedure TSepiComponent.RemoveChild(Child: TSepiComponent);
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
  Ajoute de nouveau un composant � la liste des enfants
  Cela a pour effet de replacer Child � la fin de la liste des enfants ajout�s
  jusque l�. Les types composites se servent de cette m�thode pour se replacer
  derri�re les types anonymes qu'ils ont cr��s.
  @param Child   Enfant concern�
*}
procedure TSepiComponent.ReAddChild(Child: TSepiComponent);
var
  Index: Integer;
begin
  Index := FChildren.IndexOfComponent(Child);
  if Index >= 0 then
    FChildren.Move(Index, FChildren.Count-1);
end;

{*
  Charge les forwards depuis un flux
  @param Stream   Flux source
*}
procedure TSepiComponent.LoadForwards(Stream: TStream);
var
  Count, I: Integer;
  ChildClass: TSepiTypeClass;
  ChildName: string;
begin
  Stream.ReadBuffer(Count, 4);
  for I := 0 to Count-1 do
  begin
    ChildClass := TSepiTypeClass(SepiFindComponentClass(
      ReadStrFromStream(Stream)));
    ChildName := ReadStrFromStream(Stream);

    ChildClass.ForwardDecl(Self, ChildName);
  end;
end;

{*
  Charge un enfant depuis un flux
  @param Stream   Flux source
  @return Enfant charg�
*}
function TSepiComponent.LoadChild(Stream: TStream): TSepiComponent;
var
  IsForward: Boolean;
  Name, ClassName: string;
begin
  Stream.ReadBuffer(IsForward, 1);

  if IsForward then
  begin
    Name := ReadStrFromStream(Stream);
    Result := GetComponent(Name);
  end else
    Result := nil;

  ClassName := ReadStrFromStream(Stream);

  if Result <> nil then
    Result.Load(Self, Stream)
  else
    Result := SepiFindComponentClass(ClassName).Load(Self, Stream);
end;

{*
  Enregistre les forwards dans un flux
  @param Stream   Flux destination
*}
procedure TSepiComponent.SaveForwards(Stream: TStream);
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
  @param Child    Enfant � enregistrer
*}
procedure TSepiComponent.SaveChild(Stream: TStream; Child: TSepiComponent);
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
procedure TSepiComponent.LoadChildren(Stream: TStream);
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
procedure TSepiComponent.SaveChildren(Stream: TStream);
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
  Appel� lorsqu'un enfant vient d'�tre ajout�
  @param Child   Enfant qui vient d'�tre ajout�
*}
procedure TSepiComponent.ChildAdded(Child: TSepiComponent);
begin
end;

{*
  Appel� lorsqu'un enfant va �tre supprim�
  @param Child   Enfant sur le point d'�tre supprim�
*}
procedure TSepiComponent.ChildRemoving(Child: TSepiComponent);
begin
end;

{*
  Appel� lorsque l'unit� contenante est compl�tement charg�e/cr��e
*}
procedure TSepiComponent.Loaded;
var
  I: Integer;
begin
  // Already created children
  for I := 0 to FChildren.Count-1 do
    FChildren[I].Loaded;

  // Forward declarations which have not been created yet
  for I := 0 to FForwards.Count-1 do
    with TSepiComponent(FForwards.Objects[I]) do
      if IsForward then
        Loaded;

  // Changing the state
  FState := msNormal;
end;

{*
  Liste les r�f�rences aupr�s de l'unit� contenante, pour pr�parer la sauvegarde
*}
procedure TSepiComponent.ListReferences;
var
  I: Integer;
begin
  for I := 0 to FChildren.Count-1 do
    FChildren[I].ListReferences;
end;

{*
  Enregistre le composant dans un flux
  @param Stream   Flux dans lequel enregistrer le composant
*}
procedure TSepiComponent.Save(Stream: TStream);
begin
  WriteStrToStream(Stream, Name);
  Stream.WriteBuffer(FVisibility, 1);
  Stream.WriteBuffer(FTag, SizeOf(Integer));
end;

{*
  Appel� lorsque l'environnement Sepi est sur le point d'�tre d�truit
*}
procedure TSepiComponent.Destroying;
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
  [@inheritDoc]
*}
procedure TSepiComponent.AfterConstruction;
begin
  inherited;

  if Assigned(Owner) then
    Owner.ChildAdded(Self);
end;

{*
  [@inheritDoc]
*}
procedure TSepiComponent.BeforeDestruction;
begin
  if FState <> msDestroying then
    Destroying;

  inherited;

  if Assigned(Owner) then
    Owner.ChildRemoving(Self);
end;

{*
  Cherche un composant enfant
  InternalGetComponent ne doit pas �tre appel�e directement : passez par
  GetComponent. GetComponent n'appelle InternalGetComponent qu'avec un nom
  non-compos� (sans .). De plus, si InternalGetComponent renvoie un
  TSepiTypeAlias, GetComponent se chargera de le "d�r�f�rencer".
  @param Name   Nom du composant � trouver
  @return Le composant correspondant, ou nil s'il n'a pas �t� trouv�
*}
function TSepiComponent.InternalGetComponent(
  const Name: string): TSepiComponent;
var
  Index: Integer;
begin
  Result := FChildren.ComponentFromName[Name];

  if Result = nil then
  begin
    Index := FForwards.IndexOf(Name);
    if Index >= 0 then
      Result := TSepiComponent(FForwards.Objects[Index]);
  end;
end;

{*
  Recherche un composant � partir de son nom
  InternalLookFor ne doit pas �tre appel�e directement : passez par LookFor.
  LookFor n'appelle InternalLookFor qu'avec un nom non-compos� (sans .).
  InternalLookFor doit tenir compte des visibilit�s, en testant ses candidats
  avec Candidate.IsVisibleFrom(FromComponent).
  @param Name            Nom du meta recherch�
  @param FromComponent   Composant depuis lequel on recherche
  @return Le composant recherch�, ou nil si non trouv�
*}
function TSepiComponent.InternalLookFor(const Name: string;
  FromComponent: TSepiComponent): TSepiComponent;
begin
  // Basic search
  Result := GetComponent(Name);

  // Check for visibility
  if (Result <> nil) and (not Result.IsVisibleFrom(FromComponent)) then
    Result := nil;

  // If not found, continue search a level up
  if (Result = nil) and (Owner <> nil) then
    Result := Owner.InternalLookFor(Name, FromComponent);
end;

{*
  Nom destin� � l'affichage
  @return Nom destin� � l'affichage
*}
function TSepiComponent.GetDisplayName: string;
begin
  Result := Name;
end;

{*
  �crit les donn�es n�cessaires au digest de compatibilit� dans un flux
  @param Stream   Flux destination
*}
procedure TSepiComponent.WriteDigestData(Stream: TStream);
begin
  WriteStrToStream(Stream, ClassName);
end;

{*
  Nom qualifi� du composant, depuis l'unit� contenante
  @return Nom qualifi� du composant
*}
function TSepiComponent.GetFullName: string;
begin
  if Assigned(FOwner) and (FOwner.Name <> '') then
    Result := FOwner.GetFullName+'.'+Name
  else
    Result := Name;
end;

{*
  Nom le plus court possible pour �tre r�f�renc� depuis un autre composant
  @param From   Composant depuis lequel �tre r�f�renc�
  @return Nom le plus court possible, ou une cha�ne vide si aucun n'est possible
*}
function TSepiComponent.GetShorterNameFrom(From: TSepiComponent): string;
var
  Current: TSepiComponent;
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
  Cherche un composant enfant
  @param Name   Nom du composant � trouver
  @return Le composant correspondant, ou nil s'il n'a pas �t� trouv�
*}
function TSepiComponent.GetComponent(const Name: string): TSepiComponent;
var
  ComponentName, Field: string;
begin
  SplitToken(Name, '.', ComponentName, Field);

  Result := InternalGetComponent(ComponentName);

  if not Assigned(Result) then
    Exit;
  while Result is TSepiTypeAlias do
    Result := TSepiTypeAlias(Result).Dest;
  if Field <> '' then
    Result := Result.GetComponent(Field);
end;

{*
  Cherche un composant enfant
  @param Name   Nom du composant � trouver
  @return Le composant correspondant
  @throws ESepiComponentNotFoundError Le composant n'a pas �t� trouv�
*}
function TSepiComponent.FindComponent(const Name: string): TSepiComponent;
begin
  Result := GetComponent(Name);
  if (Result = nil) and (Name <> '') then
    raise ESepiComponentNotFoundError.CreateFmt(SSepiComponentNotFound, [Name]);
end;

{*
  Trouve le conteneur de ce composnat
  @return Conteneur de ce composant, ou nil s'il n'y en a pas
*}
function TSepiComponent.FindContainer: TSepiContainerType;
var
  Ancestor: TSepiComponent;
begin
  Ancestor := Self;
  while (Ancestor <> nil) and (not (Ancestor is TSepiContainerType)) do
    Ancestor := Ancestor.Owner;

  Result := TSepiContainerType(Ancestor);
end;

{*
  Teste si un composant donn� est un anc�tre de ce composnat
  @param Ancestor   Anc�tre potentiel
  @return True si Ancestor est un anc�tre de ce composant, False sinon
*}
function TSepiComponent.IsAncestor(Ancestor: TSepiComponent): Boolean;
begin
  Result := (Ancestor = Self) or
    ((Owner <> nil) and Owner.IsAncestor(Ancestor));
end;

{*
  Teste si ce composant est visible depuis un composant donn�
  @param FromComponent   Composant depuis lequel on regarde
  @return True si le composant est visible, False sinon
*}
function TSepiComponent.IsVisibleFrom(FromComponent: TSepiComponent): Boolean;
var
  FromContainer: TSepiContainerType;
begin
  Result := True;

  if Visibility in [mvPublic, mvPublished] then
    Exit;
  if (Visibility in [mvPrivate, mvProtected]) and
    (FromComponent.OwningUnit = OwningUnit) then
    Exit;

  if FromComponent.IsAncestor(Owner) then
    Exit;

  if Visibility in [mvStrictProtected, mvProtected] then
  begin
    FromContainer := FromComponent.FindContainer;

    if (FromContainer is TSepiInheritableContainerType) and
      (Owner is TSepiInheritableContainerType) and
      TSepiInheritableContainerType(FromContainer).ContainerInheritsFrom(
      TSepiInheritableContainerType(Owner)) then
      Exit;
  end;

  Result := False;
end;

{*
  Recherche un composant � partir de son nom
  LookFor, au contraire de GetComponent/FindComponent, tient compte des
  h�ritages, des visibilit�s, des uses, etc.
  Si Name est un nom compos�, la premi�re partie est recherch�e selon
  l'algorithme LookFor, et les suivantes selon l'algorithme GetComponent.
  @param Name            Nom du composant recherch�
  @param FromComponent   Composant depuis lequel on regarde
  @return Le composant recherch�, ou nil si non trouv�
*}
function TSepiComponent.LookFor(const Name: string;
  FromComponent: TSepiComponent): TSepiComponent;
var
  FirstName, ChildName: string;
begin
  SplitToken(Name, '.', FirstName, ChildName);

  Result := InternalLookFor(FirstName, FromComponent);
  if (Result <> nil) and (ChildName <> '') then
    Result := Result.GetComponent(ChildName);
end;

{*
  Recherche un composant � partir de son nom � partir de ce composant
  @param Name   Nom du composant recherch�
  @return Le composant recherch�, ou nil si non trouv�
*}
function TSepiComponent.LookFor(const Name: string): TSepiComponent;
begin
  Result := LookFor(Name, Self);
end;

{*
  Enregistre le nom de ce composant dans des RTTI
  @param Dest   Pointeur sur la destination
  @return Adresse du premier octet apr�s la cha�ne
*}
function TSepiComponent.StoreTypeInfoName(Dest: PTypeInfoString): Pointer;
var
  AName: TypeInfoString;
  Size: Integer;
begin
  AName := TypeInfoName;
  Size := Length(AName)+1;
  Move(AName[0], Dest^, Size);
  Result := Pointer(Integer(Dest) + Size);
end;

{*
  �crit le digest de compatibilit� dans un flux
  @param Stream   Flux destination
*}
procedure TSepiComponent.WriteDigestToStream(Stream: TStream);
const
  NilDigest: TSepiDigest = (A: 0; B: 0; C: 0; D: 0);
begin
  if Self = nil then
  begin
    Stream.WriteBuffer(NilDigest, SizeOf(TSepiDigest));
  end else
  begin
    EnsureDigestBuilt;
    Stream.WriteBuffer(FDigest, SizeOf(TSepiDigest));
  end;
end;

{*
  V�rifie le digest de compatibilit�
  @param ADigest   Digest attendu
  @return True si le digest est correct, False sinon
*}
function TSepiComponent.CheckDigest(const ADigest: TSepiDigest): Boolean;
begin
  EnsureDigestBuilt;
  Result := MD5DigestCompare(ADigest, FDigest);
end;

{*
  V�rifie le digest de compatibilit�
  @param ADigest   Digest attendu sous forme de cha�ne
  @return True si le digest est correct, False sinon
*}
function TSepiComponent.CheckDigest(const ADigest: string): Boolean;
begin
  Result := CheckDigest(StrToMD5Digest(ADigest));
end;

{*
  Construit un nom pour un enfant cr�� anonyme
  @return Nom pour l'enfant
*}
function TSepiComponent.MakeUnnamedChildName: string;
begin
  Inc(FUnnamedChildCount);
  Result := '$' + IntToStr(FUnnamedChildCount);
end;

{*
  Ajoute un objet aux ressources du composant
  Tous les objets ajout�s aux ressources du meta seront lib�r�s lorsque le
  composant sera lib�r� lui-m�me.
  @param Obj   Objet � ajouter aux ressources
*}
procedure TSepiComponent.AddObjResource(Obj: TObject);
begin
  FObjResources.Add(Obj);
end;

{*
  S'approprie une ressource objet
  L'objet est ajout� aux ressources objet du composant, puis le param�tre Obj
  est mis � nil, afin qu'un Free ult�rieur sur celui-ci ne fasse plus rien.
  @param Obj   Objet � ajouter aux ressources
*}
procedure TSepiComponent.AcquireObjResource(var Obj);
begin
  AddObjResource(TObject(Obj));
  TObject(Obj) := nil;
end;

{*
  Ajoute un pointeur aux ressources du composant
  Tous les pointeurs ajout�s aux ressources du composant seront lib�r�s lorsque
  le composant sera lib�r� lui-m�me.
  @param Ptr   Pointeur � ajouter aux ressources
*}
procedure TSepiComponent.AddPtrResource(Ptr: Pointer);
begin
  FPtrResources.Add(Ptr);
end;

{*
  S'approprie une ressource pointeur
  Le pointeur est ajout� aux ressources pointeur du composant, puis le param�tre
  Ptr est mis � nil, afin que le code appelant puisse tester l'�galit� � nil
  avant de le lib�rer lui-m�me.
  @param Ptr   Pointeur � ajouter aux ressources
*}
procedure TSepiComponent.AcquirePtrResource(var Ptr);
begin
  AddPtrResource(Pointer(Ptr));
  Pointer(Ptr) := nil;
end;

{------------------}
{ Classe TSepiType }
{------------------}

{*
  Charge un type depuis un flux
  @param AOwner   Propri�taire du type
  @param Stream   Flux depuis lequel charger le type
*}
constructor TSepiType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FKind, SizeOf(TTypeKind));

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
  Cr�e un nouveau type
  @param AOwner   Propri�taire du type
  @param AName    Nom du type
  @param AKind    Type de type
*}
constructor TSepiType.Create(AOwner: TSepiComponent; const AName: string;
  AKind: TTypeKind);
begin
  inherited Create(AOwner, AName);

  FKind := AKind;
end;

{*
  Clone un type
  @param AOwner   Propri�taire du type
  @param AName    Nom du type
  @param Source   Type � cloner
*}
constructor TSepiType.Clone(AOwner: TSepiComponent; const AName: string;
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
  [@inheritDoc]
*}
procedure TSepiType.WriteDigestData(Stream: TStream);
var
  Alignment: Integer;
begin
  inherited;

  Alignment := GetAlignment;

  Stream.WriteBuffer(FKind, 1);
  Stream.WriteBuffer(FSize, SizeOf(Integer));
  Stream.WriteBuffer(Alignment, SizeOf(Integer));
end;

{*
  D�clare un type en forward
  @param AOwner   Propri�taire
  @param AName    Nom du type
  @return Type cr��
*}
class function TSepiType.ForwardDecl(AOwner: TSepiComponent;
  const AName: string): TSepiType;
begin
  Result := TSepiType(NewInstance);
  Result.FOwner := AOwner;
  Result.FName := AName;
  Result.FVisibility := AOwner.CurrentVisibility;

  AOwner.AddForward(Result);

  Result.SetupProperties;
end;

{*
  Alloue une zone m�moire pour les RTTI
  Alloue une zone m�moire adapt�e au nom du type et � la taille des donn�es de
  type, et remplit les champs de TypeInfo (TypeData reste non initialis�).
  La zone m�moire ainsi allou�e sera automatiquement lib�r�e � la destruction du
  type.
  @param TypeDataLength   Taille des donn�es de type
*}
procedure TSepiType.AllocateTypeInfo(TypeDataLength: Integer = 0);
begin
  FTypeInfoLength := SizeOf(TTypeKind) + TypeInfoNameSize + TypeDataLength
    {$IF CompilerVersion >= 21} + SizeOf(TAttrData) {$IFEND};
  GetMem(FTypeInfo, FTypeInfoLength);
  FillChar(FTypeInfo^, FTypeInfoLength, 0);

  FTypeInfo.Kind := FKind;
  StoreTypeInfoName(@FTypeInfo.Name);

  FTypeData := GetTypeData(FTypeInfo);
end;

{*
  Indique si ce type a des RTTI (TypeInfo)
  @return True si ce type a des RTTI, False sinon
*}
function TSepiType.HasTypeInfo: Boolean;
begin
  Result := Kind <> tkUnknown;
end;

{*
  Renseigne les propri�t�s de ce type
*}
procedure TSepiType.SetupProperties;
begin
  FIsReady := True;

  FParamBehavior := DefaultTypeParamBehavior;
  FResultBehavior := rbOrdinal;
end;

{*
  Construit les RTTI du type
*}
procedure TSepiType.MakeTypeInfo;
begin
end;

{*
  Construit les donn�es de runtime du type
*}
procedure TSepiType.MakeRuntimeInfo;
begin
  if HasTypeInfo then
    MakeTypeInfo;
end;

{*
  Indique si ce type est un type composite
  Les types composites ne sont pas compl�t�s automatiquement dans
  AfterConstruction.
*}
function TSepiType.IsComposite: Boolean;
begin
  Result := False;
end;

{*
  Indique si ce type est un type composite fort
  Les types composites forts sont des types composites dont les propri�t�s
  d�pendent des compos�s. La m�thode SetupProperties ne peut donc pas �tre
  appel�e tant que le type n'a pas �t� compl�t�.
*}
function TSepiType.IsStrongComposite: Boolean;
begin
  Result := False;
end;

{*
  Compl�te le type
*}
procedure TSepiType.Complete;
begin
  if Completed then
    Exit;

  FCompleted := True;

  if not IsReady then
    SetupProperties;

  if not Native then
    MakeRuntimeInfo;
end;

{*
  Alignement du type
  Les variables de ce type seront plac�es en m�moire � un adresse divisible par
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
  [@inheritDoc]
*}
class function TSepiType.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSepiType(Result).FTypeInfoRef := @TSepiType(Result).FTypeInfo;
end;

{*
  [@inheritDoc]
*}
procedure TSepiType.AfterConstruction;
begin
  inherited;

  if not IsComposite then
    Complete
  else if (not IsReady) and (not IsStrongComposite) then
    SetupProperties;
end;

{*
  Aligne l'offset donn� conform�ment � la propri�t� Alignment
  @param Offset   Offset � aligner
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
  @param Value   Valeur � initialiser
*}
procedure TSepiType.InitializeValue(var Value);
begin
  if NeedInit then
    Initialize(Value, TypeInfo);
end;

{*
  Finalise une valeur de ce type
  @param Value   Valeur � finaliser
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
  Lib�re une valeur de ce type, et la finalise si besoin
  @param Value   Pointeur sur la valeur � lib�rer
*}
procedure TSepiType.DisposeValue(Value: Pointer);
begin
  FinalizeValue(Value^);
  FreeMem(Value);
end;

{*
  Copie des donn�es de ce type
  @param Source   Source
  @param Dest     Destination
*}
procedure TSepiType.CopyData(const Source; var Dest);
begin
  ScTypInfo.CopyData(Source, Dest, Size, TypeInfo);
end;

{*
  String representation of a value of this type
  This method is useful for debugging purpose
  @param Value   Value to represent
  @return String representation of Value
*}
function TSepiType.ValueToString(const Value): string;
begin
  Result := Format(SUnknownValue, [DisplayName]);
end;

{*
  Teste si un type est �gal � un autre
  Deux types sont �gaux si et seulement si leurs variables ont la m�me structure
  interne. Dans ce cas, une copie via CopyData est valide.
  La relation �tablie avec Equals est une relation d'�quivalence.
  @param Other   Autre type � comparer
  @return True si les types sont �gaux, False sinon
*}
function TSepiType.Equals(Other: TSepiType): Boolean;
begin
  Result := ClassType = Other.ClassType;
end;

{*
  Teste si un type est compatible avec un autre
  Il faut appeler CompatibleWith sur le type de la variable affect�e, et avec en
  param�tre le type de l'expression � droite de l'assignation.
  @param AType   Type avec lequel tester la compatibilit�
  @return True si les types sont compatibles, False sinon
*}
function TSepiType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := AType.FKind = FKind;
end;

{------------------------}
{ TSepiUntypedType class }
{------------------------}

{*
  Cr�e une instance de TSepiUntypedType
  @param AOwner   Propri�taire
  @param AName    Nom du type
*}
constructor TSepiUntypedType.Create(AOwner: TSepiComponent;
  const AName: string);
begin
  inherited Create(AOwner, AName, tkUnknown);
end;

{*
  [@inheritDoc]
*}
procedure TSepiUntypedType.SetupProperties;
begin
  inherited;

  FParamBehavior.AlwaysByAddress := True;
  FResultBehavior := rbNone;
end;

{*
  [@inheritDoc]
*}
function TSepiUntypedType.GetAlignment: Integer;
begin
  Result := 1;
end;

{*
  [@inheritDoc]
*}
function TSepiUntypedType.GetDescription: string;
begin
  Result := SUntypedTypeDescription;
end;

{*
  [@inheritDoc]
*}
function TSepiUntypedType.Equals(Other: TSepiType): Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
function TSepiUntypedType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := False;
end;

{-------------------}
{ TSepiMember class }
{-------------------}

{*
  Conteneur de ce membre
  Pour les instances de TSepiMethodBase et de TSepiVariable, GetContainer peut
  renvoyer nil, si elles sont contenues dans une unit�, et non dans une classe.
  @return Conteneur de ce membre
*}
function TSepiMember.GetContainer: TSepiContainerType;
begin
  if Owner is TSepiContainerType then
    Result := TSepiContainerType(Owner)
  else
    Result := nil;
end;

{--------------------------}
{ TSepiContainerType class }
{--------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiContainerType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  FLoadingStream := Stream;
end;

{*
  [@inheritDoc]
*}
function TSepiContainerType.IsComposite: Boolean;
begin
  Result := True;
end;

{*
  [@inheritDoc]
*}
procedure TSepiContainerType.AfterConstruction;
begin
  inherited;

  if State = msLoading then
  begin
    LoadChildren(FLoadingStream);
    FLoadingStream := nil;

    Complete;
  end;
end;

{*
  Recherche un membre dans ce conteneur depuis celui-ci
  @param MemberName      Nom du membre recherch�
  @param FromComponent   Composant depuis lequel on recherche
  @return Le membre correspondant, ou nil si non trouv�
*}
function TSepiContainerType.LookForMember(const MemberName: string;
  FromComponent: TSepiComponent): TSepiMember;
var
  Component: TSepiComponent;
begin
  if MemberName = '' then
  begin
    Result := nil;
    Exit;
  end;

  Component := GetComponent(MemberName);

  if not (Component is TSepiMember) then
    Result := nil
  else if (not Component.IsVisibleFrom(FromComponent)) then
    Result := nil
  else
    Result := TSepiMember(Component);
end;

{*
  Recherche un membre dans ce conteneur depuis celui-ci
  @param MemberName   Nom du membre recherch�
  @return Le membre correspondant, ou nil si non trouv�
*}
function TSepiContainerType.LookForMember(
  const MemberName: string): TSepiMember;
begin
  Result := LookForMember(MemberName, Self);
end;

{------------------}
{ Classe TSepiRoot }
{------------------}

{*
  Cr�e une instance de TSepiRoot
*}
constructor TSepiRoot.Create;
begin
  inherited Create(nil, '');

  FRoot := Self;
  FState := msNormal;
  FSearchOrder := TObjectList.Create(False);
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
  Nombre d'unit�s
  @return Nombre d'unit�s
*}
function TSepiRoot.GetUnitCount: Integer;
begin
  Result := FChildren.Count;
end;

{*
  Tableau zero-based des unit�s
  @param Index   Index de l'unit� � r�cup�rer
  @return Unit� � l'index sp�cifi�
*}
function TSepiRoot.GetUnits(Index: Integer): TSepiUnit;
begin
  Result := TSepiUnit(FChildren.Objects[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRoot.AddChild(Child: TSepiComponent);
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
procedure TSepiRoot.RemoveChild(Child: TSepiComponent);
begin
  FSearchOrder.Remove(Child);
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRoot.ReAddChild(Child: TSepiComponent);
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
function TSepiRoot.InternalLookFor(const Name: string;
  FromComponent: TSepiComponent): TSepiComponent;
var
  I: Integer;
begin
  // Search in root
  Result := inherited InternalLookFor(Name, FromComponent);
  if Result <> nil then
    Exit;

  // Search in origin unit
  if FromComponent.OwningUnit <> nil then
  begin
    Result := FromComponent.OwningUnit.InternalLookFor(Name, FromComponent);
    if Result <> nil then
      Exit;
  end;

  // Search in other units
  for I := 0 to ChildCount-1 do
  begin
    if Children[I] = FromComponent.OwningUnit then
      Continue;
    Result := Children[I].InternalLookFor(Name, FromComponent);
    if Result <> nil then
      Exit;
  end;
end;

{*
  Incr�mente le compteur de r�f�rences d'une unit�
  @param SepiUnit   Unit� dont incr�menter le compteur de r�f�rences
  @return Nouvelle valeur du compteur de r�f�rences
*}
function TSepiRoot.IncUnitRefCount(SepiUnit: TSepiUnit): Integer;
begin
  Result := InterlockedIncrement(SepiUnit.FRefCount);
end;

{*
  D�cr�mente le compteur de r�f�rences d'une unit�
  @param SepiUnit   Unit� dont d�cr�menter le compteur de r�f�rences
  @return Nouvelle valeur du compteur de r�f�rences
*}
function TSepiRoot.DecUnitRefCount(SepiUnit: TSepiUnit): Integer;
begin
  Result := InterlockedDecrement(SepiUnit.FRefCount);
end;

{*
  Charge une unit�
  @param UnitName   Nom de l'unit� � charger
  @return Unit� charg�e
*}
function TSepiRoot.LoadUnit(const UnitName: string): TSepiUnit;
var
  ImportFunc: TSepiImportUnitFunc;
begin
  Result := TSepiUnit(GetComponent(UnitName));

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
  D�charge une unit�
  @param UnitName   Nom de l'unit� � charger
*}
procedure TSepiRoot.UnloadUnit(const UnitName: string);
var
  SepiUnit: TSepiUnit;
begin
  if State = msDestroying then
    Exit;

  SepiUnit := TSepiUnit(GetComponent(UnitName));
  if SepiUnit = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  if DecUnitRefCount(SepiUnit) = 0 then
  begin
    Assert(FChildren.IndexOfObject(SepiUnit) = FChildren.Count-1);
    SepiUnit.Free;
  end;
end;

{*
  Cherche un type enregistr� � partir de ses informations de type
  @param TypeInfo   Informations de type du type recherch�
  @return Le type correspondant (nil si non trouv�)
*}
function TSepiRoot.GetType(TypeInfo: PTypeInfo): TSepiType;
var
  TypeName: string;
  I: Integer;
  Component: TSepiComponent;
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
    Component := TSepiComponent(FSearchOrder[I]).GetComponent(TypeName);

    if Component is TSepiType then
    begin
      if TSepiType(Component).TypeInfo = TypeInfo then
      begin
        Result := TSepiType(Component);
        Exit;
      end else
      begin
        if First = nil then
          First := TSepiType(Component);
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
function TSepiRoot.GetType(const TypeName: string): TSepiType;
var
  I: Integer;
  Component: TSepiComponent;
begin
  if TypeName = '' then
  begin
    Result := nil;
    Exit;
  end;

  for I := 0 to FSearchOrder.Count-1 do
  begin
    Component := TSepiComponent(FSearchOrder[I]).GetComponent(TypeName);
    if Component is TSepiType then
    begin
      Result := TSepiType(Component);
      Exit;
    end;
  end;

  Result := nil;
end;

{*
  Trouve un type enregistr� � partir de ses informations de type
  @param TypeInfo   Informations de type du type recherch�
  @return Le type correspondant aux informations de type donn�es
  @throw ESepiComponentNotFoundError Aucun type enregistr� correspondant
*}
function TSepiRoot.FindType(TypeInfo: PTypeInfo): TSepiType;
begin
  if TypeInfo = nil then
    Result := nil
  else
  begin
    Result := GetType(TypeInfo);
    if Result = nil then
      raise ESepiComponentNotFoundError.CreateFmt(
        SSepiComponentNotFound, [TypeInfo.Name]);
  end;
end;

{*
  Trouve un type enregistr� � partir de son nom
  @param TypeName   Nom du type recherch�
  @return Le type correspondant au nom donn�
  @throw ESepiComponentNotFoundError Aucun type enregistr� correspondant
*}
function TSepiRoot.FindType(const TypeName: string): TSepiType;
begin
  if TypeName = '' then
    Result := nil
  else
  begin
    Result := GetType(TypeName);
    if Result = nil then
      raise ESepiComponentNotFoundError.CreateFmt(
        SSepiComponentNotFound, [TypeName]);
  end;
end;

{*
  Trouve une classe enregistr�e � partir de la classe Delphi
  Le r�sultat peut toujours �tre *transtyp�* en TSepiClass.
  Renvoie nil si non trouv�.
  @param DelphiClass   Classe Delphi recherch�e
  @return Le type correspondant � la classe donn�e (garanti �tre TSepiClass)
*}
function TSepiRoot.GetClass(DelphiClass: TClass): TSepiType;
var
  ClassInfo: PTypeInfo;
  ClassUnitName: string;
  SepiUnit: TSepiUnit;
  I: Integer;
begin
  // Try and look directly into the unit name given by TypeInfo
  ClassInfo := DelphiClass.ClassInfo;
  if ClassInfo <> nil then
  begin
    ClassUnitName := GetTypeData(ClassInfo).UnitName;
    SepiUnit := GetComponent(ClassUnitName) as TSepiUnit;

    if SepiUnit <> nil then
    begin
      Result := SepiUnit.GetClass(DelphiClass);
      if Result <> nil then
        Exit;
    end;
  end;

  // Search the entire unit list
  for I := 0 to UnitCount-1 do
  begin
    Result := Units[I].GetClass(DelphiClass);
    if Result <> nil then
      Exit;
  end;

  // Not found
  Result := nil;
end;

{*
  Trouve une classe enregistr�e � partir de la classe Delphi
  Le r�sultat peut toujours �tre *transtyp�* en TSepiClass.
  @param DelphiClass   Classe Delphi recherch�e
  @return Le type correspondant � la classe donn�e (garanti �tre TSepiClass)
  @throw ESepiComponentNotFoundError Aucune classe enregistr�e correspondant
*}
function TSepiRoot.FindClass(DelphiClass: TClass): TSepiType;
begin
  Result := GetClass(DelphiClass);

  if Result = nil then
    raise ESepiComponentNotFoundError.CreateFmt(
      SSepiComponentNotFound, [DelphiClass.ClassName]);
end;

{---------------------}
{ TSepiRootFork class }
{---------------------}

{*
  Cr�e un nouveau fork de racine Sepi
  @param AOriginalRoot   Racine Sepi originale
*}
constructor TSepiRootFork.Create(AOriginalRoot: TSepiRoot);
begin
  FOriginalRoot := AOriginalRoot;
  FForeignRefCounts := TStringList.Create;

  inherited Create;

  LoadUnit(SystemUnitName);
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
  Charge une unit� depuis la racine de base
  @param UnitName   Nom de l'unit�
  @return Unit� Sepi charg�e, ou nil si non trouv�e
*}
function TSepiRootFork.LoadUnitFromOriginalRoot(
  const UnitName: string): TSepiUnit;
var
  PreviousRoot: TSepiRoot;
begin
  PreviousRoot := Self;
  Result := nil;

  while (Result = nil) and (PreviousRoot is TSepiRootFork) do
  begin
    PreviousRoot := TSepiRootFork(PreviousRoot).OriginalRoot;
    Result := (PreviousRoot.GetComponent(UnitName) as TSepiUnit);
  end;

  if Result <> nil then
  begin
    OriginalRoot.LoadUnit(UnitName);
    AddChild(Result);
    ChildAdded(Result);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.AddChild(Child: TSepiComponent);
begin
  inherited;

  FForeignRefCounts.Add(Child.Name);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.RemoveChild(Child: TSepiComponent);
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
      SepiUnit.Free;
    end else
    begin
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
  Result := TSepiUnit(GetComponent(UnitName));

  if Result = nil then
  begin
    ImportFunc := SepiImportedUnit(UnitName);

    if Assigned(ImportFunc) then
    begin
      // For imported units, check the original root directly
      Result := LoadUnitFromOriginalRoot(UnitName);
      if Result = nil then
        Result := ImportFunc(Self);
    end else
    begin
      // For fully Sepi units, first try and load the unit with OnLoadUnit
      if Assigned(OnLoadUnit) then
        Result := OnLoadUnit(Self, UnitName);
      if Result = nil then
        Result := LoadUnitFromOriginalRoot(UnitName);
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

  SepiUnit := TSepiUnit(GetComponent(UnitName));
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
  Cr�e les donn�es de lazy-load
  @param AOwner    Propri�taire des enfants � charger
  @param AStream   Flux depuis lequel charger les enfants
*}
constructor TSepiLazyLoadData.Create(AOwner: TSepiComponent; AStream: TStream);
begin
  inherited Create;

  FOwner := AOwner;
  FStream := AStream;

  FChildrenNames := THashedStringList.Create;
  FLoadingClassIntfChildren := TStringList.Create;
  FForwardClassIntfChildren := TObjectList.Create(False);

  with TStringList(FLoadingClassIntfChildren) do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
  end;
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
  R�cup�re les informations de lazy-load pour un enfant
  @param Name   Nom de l'enfant � charger
  @param Data   En sortie : donn�es sur cet enfant
  @raise ESepiComponentNotFoundError Aucun enfant de ce nom trouv�
*}
function TSepiLazyLoadData.GetChildData(
  const Name: string): TSepiLazyLoadChildData;
var
  Index: Integer;
begin
  Index := FChildrenNames.IndexOf(Name);
  if Index < 0 then
    raise ESepiComponentNotFoundError.CreateFmt(SSepiComponentNotFound, [Name]);

  Result := FChildrenData[Index];
end;

{*
  Teste si un enfant peut �tre charg� maintenant
  @param Data   Donn�es de chargement d'un enfant
  @return True si l'enfant d�crit par ces donn�es peut �tre charg�, False sinon
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
  @param Data    Donn�es sur l'enfant � charger
  @param Child   En entr�e et/ou en sortie : l'enfant � charger
*}
procedure TSepiLazyLoadData.InternalLoadChild(
  const Data: TSepiLazyLoadChildData; var Child: TSepiComponent);
var
  ClassOrIntf: Boolean;
  OldPosition: Int64;
  IsForward: Boolean;
  ComponentClassName: string;
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
      ComponentClassName := ReadStrFromStream(Stream);
      if Child = nil then
      begin
        Child := SepiFindComponentClass(ComponentClassName).Load(Owner, Stream);
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
  R�essaye de charger les enfants qui sont forwards
*}
procedure TSepiLazyLoadData.RetryForwards;
var
  I: Integer;
  Child: TSepiComponent;
  Data: TSepiLazyLoadChildData;
begin
  { Looking downwards is more efficient, because we expect recent demands to
    be easier to satisfy. }
  for I := FForwardClassIntfChildren.Count-1 downto 0 do
  begin
    Child := TSepiComponent(FForwardClassIntfChildren[I]);
    Data := GetChildData(Child.Name);

    if CanLoad(Data) then
    begin
      InternalLoadChild(Data, Child); // Will call RetryForwards recursively
      Exit;
    end;
  end;
end;

{*
  Charge les donn�es depuis le flux
  @param SkipChildrenInfo   Si True, saute les d�finitions des enfants
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
  @param Name   Nom de l'enfant recherch�
  @return True si un enfant de ce nom existe, False sinon
*}
function TSepiLazyLoadData.ChildExists(const Name: string): Boolean;
begin
  Result := FChildrenNames.IndexOf(Name) >= 0;
end;

{*
  Charge un enfant depuis son nom
  @param Name   Nom de l'enfant � charger
  @return Enfant charg�
*}
function TSepiLazyLoadData.LoadChild(const Name: string): TSepiComponent;
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
    begin
      FForwardClassIntfChildren.Add(Result);
      FLoadingClassIntfChildren.Add(Data.Name);
    end;
    Exit;
  end;

  // Now do the job
  Result := nil;
  InternalLoadChild(Data, Result);
end;

{*
  Construit l'h�ritage (dans la m�me unit�) d'une classe
  @param SepiClass       Classe dont construire l'h�ritage
  @param AncestorClass   Liste des noms des anc�tres
*}
procedure MakeClassInheritance(SepiClass: TSepiClass;
  AncestorList: TStrings);
var
  I: Integer;
  AncestorClass: TSepiClass;
  AncestorIntf: TSepiInterface;
begin
  AncestorClass := SepiClass.Parent;
  while (AncestorClass <> nil) and (AncestorClass.Owner = SepiClass.Owner) do
  begin
    AncestorList.Add(AncestorClass.Name);
    AncestorClass := AncestorClass.Parent;
  end;

  for I := 0 to SepiClass.InterfaceCount-1 do
  begin
    AncestorIntf := SepiClass.Interfaces[I];
    while (AncestorIntf <> nil) and (AncestorIntf.Owner = SepiClass.Owner) do
    begin
      AncestorList.Add(AncestorIntf.Name);
      AncestorIntf := AncestorIntf.Parent;
    end;
  end;
end;

{*
  Construit l'h�ritage (dans la m�me unit�) d'une interface
  @param SepiIntf        Interface dont construire l'h�ritage
  @param AncestorClass   Liste des noms des anc�tres
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
  Construit l'h�ritage d'une classe ou interface (dans la m�me unit�)
  @param ClassIntf     Classe ou interface
  @param Inheritance   En sortie : h�ritage de la classe ou interface
*}
procedure MakeClassIntfInheritance(ClassIntf: TSepiType;
  out Inheritance: TStringDynArray);
var
  AncestorList: TStringList;
  I: Integer;
begin
  AncestorList := TStringList.Create;
  try
    AncestorList.Sorted := True;
    AncestorList.Duplicates := dupIgnore;

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
  Produit les donn�es de lazy-load pour un enfant
  @param Child      Enfant concern�
  @param Position   Position dans le flux
  @param Data       En sortie : donn�es pour cet enfant
*}
procedure MakeLazyLoadChildData(Child: TSepiComponent; Position: Int64;
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
  Enregistre les donn�es d'un meta dans un flux
  @param Owner    Component propri�taire
  @param Stream   Flux de destination
*}
class procedure TSepiLazyLoadData.SaveToStream(Owner: TSepiComponent;
  Stream: TStream);
var
  Count, I: Integer;
  TempStream: TMemoryStream;
  ChildrenData: TSepiLazyLoadChildrenData;
  Child: TSepiComponent;
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
  Charge une unit� depuis un flux
*}
constructor TSepiUnit.Load(AOwner: TSepiComponent; Stream: TStream);
var
  UsesCount, RefCount, I, J: Integer;
  Str, StrDigest: string;
  Digest: TSepiDigest;
begin
  if LazyLoad then
    FLazyLoadData := TSepiLazyLoadData.Create(Self, Stream);

  Stream.ReadBuffer(UsesCount, 4);
  SetLength(FReferences, UsesCount+1);
  FillChar(FReferences[0], 4*(UsesCount+1), 0);

  FillChar(Digest, SizeOf(TSepiDigest), 0);
  StrDigest := MD5DigestToStr(Digest);

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
      begin
        Str := ReadStrFromStream(Stream);

        if I > 0 then
        begin
          Stream.ReadBuffer(Digest, SizeOf(TSepiDigest));
          StrDigest := MD5DigestToStr(Digest);
        end;

        FReferences[I].Values[Str] := StrDigest;
      end;
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
  Cr�e une nouvelle unit�
  @param AOwner   Propri�taire de l'unit� (la racine)
  @param AName    Nom de l'unit�
*}
constructor TSepiUnit.Create(AOwner: TSepiComponent; const AName: string;
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
  Ajoute une unit� aux uses de cette unit�
  @param AUnit   Unit� � ajouter aux uses
  @return Index de la nouvelle unit� dans les uses
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
  @return Enfant charg�, ou nil si n'existe pas
*}
function TSepiUnit.LazyLoadChild(const Name: string): TSepiComponent;
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
function TSepiUnit.InternalGetComponent(const Name: string): TSepiComponent;
begin
  Result := inherited InternalGetComponent(Name);

  if LazyLoad and (Result = nil) then
    Result := LazyLoadChild(Name);
end;

{*
  Change la visibilit� courante
  @param Value   Nouvelle visibilit�
*}
procedure TSepiUnit.SetCurrentVisibility(Value: TMemberVisibility);
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
function TSepiUnit.GetUsedUnitCount: Integer;
begin
  Result := FUsesList.Count;
end;

{*
  Tableau zero-based des unit�s utilis�es
  @param Index   Index de l'unit� utilis�e
  @return Unit� utilis�e dont l'index est sp�cifi�
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
  Digest: TSepiDigest;
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
      begin
        WriteStrToStream(Stream, RefList.Names[J]);

        if I > 0 then
        begin
          Digest := TSepiComponent(RefList.Objects[J]).Digest;
          Stream.WriteBuffer(Digest, SizeOf(TSepiDigest));
        end;
      end;
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
function TSepiUnit.InternalLookFor(const Name: string;
  FromComponent: TSepiComponent): TSepiComponent;
var
  I: Integer;
begin
  // Basic search
  Result := GetComponent(Name);
  if (Result <> nil) and (not Result.IsVisibleFrom(FromComponent)) then
    Result := nil;
  if Result <> nil then
    Exit;

  // If coming from another unit, stop there
  if FromComponent.OwningUnit <> Self then
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

  // Search in used units
  for I := UsedUnitCount-1 downto 0 do
  begin
    Result := UsedUnits[I].InternalLookFor(Name, FromComponent);
    if Result <> nil then
      Exit;
  end;

  Result := nil;
end;

{*
  Ajoute des uses � l'unit�
  Cette m�thode ne peut �tre appel�e que pour une unit� en cours de
  construction.
  @param AUses   Uses � ajouter
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
  Compl�te l'unit� cr��e
*}
procedure TSepiUnit.Complete;
begin
  Assert(State = msConstructing);
  Loaded;
end;

{*
  Enregistre l'unit� dans un flux
*}
procedure TSepiUnit.SaveToStream(Stream: TStream);
begin
  if LazyLoad then
    raise ESepiError.Create(SSepiCantSaveLazyLoadUnit);

  Save(Stream);
end;

{*
  Charge l'unit� depuis un flux
  @param AOwner             Propri�taire de l'unit�
  @param Stream             Flux depuis lequel charger l'unit�
  @param AOnGetMethodCode   M�thode de call-back pour r�cup�rer le code d'une
                            m�thode
  @param AOnGetTypeInfo     M�thode de call-back pour r�cup�rer les RTTI d'un
                            type
*}
class function TSepiUnit.LoadFromStream(AOwner: TSepiComponent; Stream: TStream;
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
  Lit une r�f�rence depuis un flux
  Cette m�thode ne peut �tre appel�e que depuis le constructeur Load des metas
  contenus dans cette unit�. � tout autre moment, c'est la violation d'acc�s
  assur�e.
  @param Stream   Flux dans lequel �crire la r�f�rence
  @param Ref      Variable de type TSepiComponent o� stocker la r�f�rence lue
*}
procedure TSepiUnit.ReadRef(Stream: TStream; out Ref);
var
  UnitIndex, RefIndex: Integer;
  RefList: TStrings;
  SepiUnit: TSepiUnit;
  ComponentName: string;
  Component: TSepiComponent;
  DigestOK: Boolean;
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
    if UnitIndex = 0 then
      SepiUnit := Self
    else
      SepiUnit := TSepiUnit(FUsesList.Objects[UnitIndex-1]);

    ComponentName := RefList.Names[RefIndex];
    Component := SepiUnit.GetComponent(ComponentName);

    if Component = nil then
      raise ESepiIncompatibleUsedUnitError.CreateFmt(
        SSepiIncompatibleUsedUnitComponentNotFound,
        [Name, SepiUnit.Name, ComponentName]);

    DigestOK := (UnitIndex = 0) or
      Component.CheckDigest(RefList.ValueFromIndex[RefIndex]);

    if not DigestOK then
      raise ESepiIncompatibleUsedUnitError.CreateFmt(
        SSepiIncompatibleUsedUnitIncompatibleComponent,
        [Name, SepiUnit.Name, ComponentName]);

    RefList.Objects[RefIndex] := Component;
    TObject(Ref) := Component;
  end;
end;

{*
  Ajoute une r�f�rence qui va devoir �tre enregistr�e
  Cette m�thode ne peut �tre appel�e que depuis la m�thode ListReferences des
  composants contenus dans cette unit�. � tout autre moment, c'est la violation
  d'acc�s assur�e.
  @param Ref   R�f�rence � ajouter
*}
procedure TSepiUnit.AddRef(Ref: TSepiComponent);
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
      Ref.GetFullName, Length(Ref.OwningUnit.Name)+2, MaxInt)+'=', Ref);
end;

{*
  �crit une r�f�rence dans un flux
  Toute r�f�rence � �crire doit �tre ajout�e au moyen de la m�thode AddRef dans
  la m�thode ListReferences de son r�f�renceur.
  Cette m�thode ne peut �tre appel�e que depuis la m�thode Save des composants
  contenus dans cette unit�. � tout autre moment, c'est la violation d'acc�s
  assur�e.
  @param Stream   Flux dans lequel �crire la r�f�rence
  @param Ref      R�f�rence � �crire
*}
procedure TSepiUnit.WriteRef(Stream: TStream; Ref: TSepiComponent);
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

{*
  Trouve une classe enregistr�e � partir de la classe Delphi dans cette unit�
  Le r�sultat peut toujours �tre *transtyp�* en TSepiClass.
  Renvoie nil si non trouv�
  @param DelphiClass   Classe Delphi recherch�e
  @return Le type correspondant � la classe donn�e (garanti �tre TSepiClass)
*}
function TSepiUnit.GetClass(DelphiClass: TClass): TSepiType;
var
  Component: TSepiComponent;
begin
  Result := nil;

  Component := GetComponent(DelphiClass.ClassName);
  if not (Component is TSepiClass) then
    Exit;

  if TSepiClass(Component).DelphiClass = DelphiClass then
    Result := TSepiClass(Component);
end;

{*
  Trouve une classe enregistr�e � partir de la classe Delphi dans cette unit�
  Le r�sultat peut toujours �tre *transtyp�* en TSepiClass.
  @param DelphiClass   Classe Delphi recherch�e
  @return Le type correspondant � la classe donn�e (garanti �tre TSepiClass)
  @throw ESepiComponentNotFoundError Aucune classe enregistr�e correspondant
*}
function TSepiUnit.FindClass(DelphiClass: TClass): TSepiType;
begin
  Result := GetClass(DelphiClass);

  if Result = nil then
    raise ESepiComponentNotFoundError.CreateFmt(
      SSepiComponentNotFound, [DelphiClass.ClassName]);
end;

{-----------------------}
{ Classe TSepiUnitAlias }
{-----------------------}

{*
  [@inheritDoc]
*}
function TSepiUnitAlias.InternalLookFor(const Name: string;
  FromComponent: TSepiComponent): TSepiComponent;
var
  I: Integer;
begin
  // Basic search
  Result := inherited InternalLookFor(Name, FromComponent);
  if Result <> nil then
    Exit;

  // If coming from this unit, stop there (already handled in inherited method)
  if FromComponent.OwningUnit = Self then
    Exit;

  // Search in used units
  for I := UsedUnitCount-1 downto 1 {ignore System} do
  begin
    Result := UsedUnits[I].InternalLookFor(Name, FromComponent);
    if Result <> nil then
      Exit;
  end;

  Result := nil;
end;

{-----------------------}
{ Classe TSepiTypeAlias }
{-----------------------}

{*
  Charge un alias de type depuis un flux
*}
constructor TSepiTypeAlias.Load(AOwner: TSepiComponent; Stream: TStream);
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
constructor TSepiTypeAlias.Create(AOwner: TSepiComponent; const AName: string;
  ADest: TSepiType);
begin
  inherited Create(AOwner, AName);
  FDest := ADest;
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

{*
  [@inheritDoc]
*}
procedure TSepiTypeAlias.WriteDigestData(Stream: TStream);
begin
  inherited;

  Dest.WriteDigestToStream(Stream);
end;

{----------------------}
{ Classe TSepiConstant }
{----------------------}

{*
  Charge une constante depuis un flux
*}
constructor TSepiConstant.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FType);
  FValuePtr := ConstType.NewValue;

  ReadDataFromStream(Stream, FValuePtr^, ConstType.Size, ConstType.TypeInfo);
end;

{*
  Cr�e une nouvelle vraie constante sans initialiser sa valeur
  @param AOwner   Propri�taire de la constante
  @param AName    Nom de la constante
  @param AType    Type de la constante
*}
constructor TSepiConstant.Create(AOwner: TSepiComponent;
  const AName: string; AType: TSepiType);
begin
  inherited Create(AOwner, AName);

  FType := AType;
  FValuePtr := ConstType.NewValue;
end;

{*
  Cr�e une nouvelle vraie constante sans initialiser sa valeur
  @param AOwner   Propri�taire de la constante
  @param AName    Nom de la constante
  @param AType    Type de la constante
*}
constructor TSepiConstant.Create(AOwner: TSepiComponent;
  const AName: string; AType: TSepiType; const AValue);
begin
  Create(AOwner, AName, AType);
  ConstType.CopyData(AValue, ValuePtr^);
end;

{*
  Cr�e une nouvelle vraie constante
  @param AOwner   Propri�taire de la constante
  @param AName    Nom de la constante
  @param AValue   Valeur de la constante
  @param AType    Type de la constante
*}
constructor TSepiConstant.Create(AOwner: TSepiComponent;
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
  Cr�e une nouvelle vraie constante
  @param AOwner      Propri�taire de la constante
  @param AName       Nom de la constante
  @param AValue      Valeur de la constante
*}
constructor TSepiConstant.Create(AOwner: TSepiComponent;
  const AName: string; const AValue: Variant);
var
  SystemUnit: TSepiSystemUnit;
  AType: TSepiType;
begin
  SystemUnit := TSepiSystemUnit.Get(AOwner);

    case VarType(AValue) of
    varSmallint: AType := SystemUnit.Smallint;
    varInteger:  AType := SystemUnit.Integer;
    varSingle:   AType := SystemUnit.Single;
    varDouble:   AType := SystemUnit.Double;
    varCurrency: AType := SystemUnit.Currency;
    varDate:     AType := SystemUnit.TDateTime;
    varError:    AType := SystemUnit.HRESULT;
    varBoolean:  AType := SystemUnit.Boolean;
    varShortInt: AType := SystemUnit.Shortint;
    varByte:     AType := SystemUnit.Byte;
    varWord:     AType := SystemUnit.Word;
    varLongWord: AType := SystemUnit.LongWord;
    varInt64:    AType := SystemUnit.Int64;
    {$IF Declared(varUInt64)}
    varUInt64:   AType := SystemUnit.UInt64;
    {$IFEND}

    varOleStr, varStrArg, varString: AType := SystemUnit.LongString;
      {$IF Declared(varUString)}
    varUString: AType := SystemUnit.LongString;
      {$IFEND}
    else
      raise ESepiBadConstTypeError.CreateFmt(
        SSepiBadConstType, [VarType(AValue)]);
    end;

  Create(AOwner, AName, AValue, AType);
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

{*
  [@inheritDoc]
*}
procedure TSepiConstant.WriteDigestData(Stream: TStream);
begin
  inherited;

  ConstType.WriteDigestToStream(Stream);
  WriteDataToStream(Stream, FValuePtr^, FType.Size, FType.TypeInfo);
end;

{----------------------}
{ Classe TSepiVariable }
{----------------------}

{*
  Charge une variable ou une constante typ�e depuis un flux
*}
constructor TSepiVariable.Load(AOwner: TSepiComponent; Stream: TStream);
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
  Importe une variable ou constante typ�e native
  @param AOwner     Propri�taire de la variable
  @param AName      Nom de la variable
  @param AValue     Valeur de la variable
  @param AType      Type de la variable
  @param AIsConst   Indique si c'est une constante typ�e
*}
constructor TSepiVariable.Create(AOwner: TSepiComponent; const AName: string;
  const AValue; AType: TSepiType; AIsConst: Boolean = False);
begin
  inherited Create(AOwner, AName);

  FIsConst := AIsConst;
  FType := AType;
  FValue := @AValue;
  FOwnValue := False;
end;

{*
  Cr�e une nouvelle variable ou constante typ�e
  @param AOwner     Propri�taire de la variable
  @param AName      Nom de la variable
  @param AType      Type de la variable
  @param AIsConst   Indique si c'est une constante typ�e
*}
constructor TSepiVariable.Create(AOwner: TSepiComponent; const AName: string;
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

{*
  [@inheritDoc]
*}
procedure TSepiVariable.WriteDigestData(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FIsConst, SizeOf(Boolean));
  VarType.WriteDigestToStream(Stream);
end;

{----------------------}
{ TSepiNamespace class }
{----------------------}

{*
  [@inheritDoc]
*}
constructor TSepiNamespace.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FVirtualOwner);
  LoadChildren(Stream);
end;

{*
  Cr�e un espace de noms
  @param AOwner          Propri�taire de l'espace de noms
  @param AName           Nom de l'espace de noms
  @param AVirtualOwner   Propri�taire virtuel (d�faut = nil, utilise Owner)
*}
constructor TSepiNamespace.Create(AOwner: TSepiComponent; const AName: string;
  AVirtualOwner: TSepiComponent);
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
function TSepiNamespace.InternalLookFor(const Name: string;
  FromComponent: TSepiComponent): TSepiComponent;
begin
  if VirtualOwner = nil then
    Result := inherited InternalLookFor(Name, FromComponent)
  else
  begin
    // Basic search
    Result := GetComponent(Name);

    // Check for visibility
    if (Result <> nil) and (not Result.IsVisibleFrom(FromComponent)) then
      Result := nil;

    // If not found, continue search in the virtual owner
    if Result = nil then
      Result := VirtualOwner.InternalLookFor(Name, FromComponent);
  end;
end;

{*
  Compl�te l'espace de noms
*}
procedure TSepiNamespace.Complete;
begin
  Owner.ReAddChild(Self);
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
constructor TSepiUnitLoadTask.Create(AOwner: TScCustomTaskQueue;
  ARoot: TSepiRoot; const AUnitName: string; AIsLoad: Boolean;
  AFreeOnFinished: Boolean);
begin
  inherited Create(AOwner, AFreeOnFinished);
  FRoot := ARoot;
  FName := AUnitName;
  FIsLoad := AIsLoad;
end;

{*
  Cr�e une nouvelle t�che de chargement/d�chargement d'une unit� Sepi
  @param AOwner      Gestionnaire propri�taire
  @param ARoot       Racine Sepi
  @param AUnitName   Nom de l'unit� � charger/d�charger
  @param AIsLoad     True pour charger, False pour d�charger
*}
constructor TSepiUnitLoadTask.Create(AOwner: TScCustomTaskQueue;
  ARoot: TSepiRoot; const AUnitName: string; AIsLoad: Boolean);
begin
  inherited Create(AOwner);
  FRoot := ARoot;
  FName := AUnitName;
  FIsLoad := AIsLoad;
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnitLoadTask.Execute;
begin
  if IsLoad then
    Root.LoadUnit(Name)
  else
    Root.UnloadUnit(Name);
end;

{-------------------------------------}
{ Classe TSepiAsynchronousRootManager }
{-------------------------------------}

{*
  Cr�e un nouveau gestionnaire de racine
  @param ARoot   Racine � g�rer (si nil, la racine est cr��e et lib�r�e)
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
  Demande le chargement d'une unit�
  @param UnitName   Unit� � charger
*}
procedure TSepiAsynchronousRootManager.LoadUnit(const UnitName: string);
begin
  TSepiUnitLoadTask.Create(Self, Root, UnitName, True);
end;

{*
  Demande le d�chargement d'une unit�
  @param UnitName   Unit� � charger
*}
procedure TSepiAsynchronousRootManager.UnloadUnit(const UnitName: string);
begin
  TSepiUnitLoadTask.Create(Self, Root, UnitName, False);
end;

initialization
  SepiRegisterComponentClasses([
    TSepiUntypedType, TSepiUnit, TSepiTypeAlias, TSepiConstant, TSepiVariable,
    TSepiNamespace
  ]);
finalization
  SepiComponentClasses.Free;
  SepiImportedUnits.Free;
end.

