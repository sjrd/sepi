{*
  D�finit les classes de gestion des types composites
  @author sjrd
  @version 1.0
*}
unit SepiMembers;

interface

uses
  Windows, Classes, SysUtils, StrUtils, RTLConsts, Contnrs, TypInfo, ScUtils,
  ScStrUtils, ScDelphiLanguage, ScCompilerMagic, SepiReflectionCore,
  SepiReflectionConsts;

const
  /// Pas d'index
  NoIndex = integer($80000000);

  /// Pas de valeur par d�faut
  NoDefaultValue = integer($80000000);

type
  {*
    Type de param�tre cach�
    - hpNormal : param�tre normal
    - hpSelf : param�tre Self des m�thodes
    - hpResult : param�tre Result des fonctions
    - hpAlloc : param�tre $Alloc des constructeurs
    - hpFree : param�tre $Free des destructeurs
    - hpOpenArrayHighValue : valeur maximale d'indice du tableau ouvert
  *}
  TSepiHiddenParamKind = (hpNormal, hpSelf, hpResult, hpAlloc, hpFree,
    hpOpenArrayHighValue);

  {*
    Type de param�tre
    - pkValue : transmis par valeur
    - pkVar : param�tre var
    - pkConst : param�tre const
    - pkOut : param�tre out
  *}
  TSepiParamKind = (pkValue, pkVar, pkConst, pkOut);

  {*
    Emplacement d'un param�tre
    - ppEAX : registre EAX
    - ppEDX : registre EDX
    - ppECX : registre ECX
    - ppStack : sur la pile
  *}
  TSepiParamPlace = (ppEAX, ppEDX, ppECX, ppStack);

  {*
    Type de liaison d'une m�thode
    - mlkStatic : m�thode statique (ni virtuelle ni dynamique)
    - mlkVirtual : m�thode � liaison virtuelle (via VMT)
    - mlkDynamic : m�thode � liaison dynamique (via DMT)
    - mlkMessage : m�thode d'interception de message (via DMT)
    - mlkInterface : m�thode � liaison d'interface (via IMT)
    - mlkOverride : d�termine le type de liaison depuis la m�thode h�rit�e
  *}
  TMethodLinkKind = (mlkStatic, mlkVirtual, mlkDynamic, mlkMessage,
    mlkInterface, mlkOverride);

  {*
    Convention d'appel d'une m�thode
  *}
  TCallingConvention = (ccRegister, ccCdecl, ccPascal, ccStdCall, ccSafeCall);

  {*
    Type d'accesseur d'une propri�t�
  *}
  TPropertyAccessKind = (pakNone, pakField, pakMethod);

  {*
    Type de stockage d'une propri�t�
  *}
  TPropertyStorageKind = (pskConstant, pskField, pskMethod);

  TSepiMetaOverloadedMethod = class;

  {*
    Meta-variable
    @author sjrd
    @version 1.0
  *}
  TSepiMetaField = class(TSepiReflectionItem)
  private
    FType : TSepiType; /// Type du champ
    FOffset : integer; /// Offset
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      AType : TSepiType; AOffset : integer);

    property FieldType : TSepiType read FType;
    property Offset : integer read FOffset;
  end;

  {*
    Informations d'appel d'un param�tre (ou et comment le passer)
    @author sjrd
    @version 1.0
  *}
  TSepiParamCallInfo = record
    ByAddress : boolean;     /// True pour le passer par adresse
    Place : TSepiParamPlace; /// Endroit o� le placer
    StackOffset : Word;      /// Offset o� le placer dans la pile
  end;

  {*
    Meta-param�tre de m�thode
    Les param�tres cach�s ont aussi leur existence en tant que TSepiMetaParam.
    Les cinq types de param�tres cach�s sont Self (class ou instance), Result
    (qui peut �tre pass� par adresse ou pas), le param�tre sp�cial $Alloc des
    constructeurs, le param�tre sp�cial $Free des destructeurs et les
    param�tres High$xxx suivant chaque param�tre de type "open array".
    @author sjrd
    @version 1.0
  *}
  TSepiMetaParam = class(TSepiReflectionItem)
  private
    FHiddenKind : TSepiHiddenParamKind; /// Type de param�tre cach�
    FKind : TSepiParamKind;             /// Type de param�tre
    FByRef : boolean;                   /// True si pass� par r�f�rence
    FOpenArray : boolean;               /// True pour un tableau ouvert
    FType : TSepiType;                  /// Type du param�tre (peut �tre nil)

    FFlags : TParamFlags;           /// Flags du param�tre
    FCallInfo : TSepiParamCallInfo; /// Informations d'appel du param�tre

    constructor CreateHidden(AOwner : TSepiReflectionItem;
      AHiddenKind : TSepiHiddenParamKind; AType : TSepiType = nil);
    constructor RegisterParamData(AOwner : TSepiReflectionItem;
      var ParamData : Pointer);
    class procedure CreateFromString(AOwner : TSepiReflectionItem;
      const Definition : string);
    procedure MakeFlags;
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      AType : TSepiType; AKind : TSepiParamKind = pkValue;
      AOpenArray : boolean = False);

    function Equals(AParam : TSepiMetaParam) : boolean;
    function CompatibleWith(AType : TSepiType) : boolean;

    property HiddenKind : TSepiHiddenParamKind read FHiddenKind;
    property Kind : TSepiParamKind read FKind;
    property ByRef : boolean read FByRef;
    property OpenArray : boolean read FOpenArray;
    property ParamType : TSepiType read FType;

    property Flags : TParamFlags read FFlags;
    property CallInfo : TSepiParamCallInfo read FCallInfo;
  end;

  {*
    Signature d'une m�thode
    @author sjrd
    @version 1.0
  *}
  TSepiMethodSignature = class
  private
    FOwner : TSepiReflectionItem;            /// Propri�taire de la signature
    FKind : TMethodKind;                     /// Type de m�thode
    FReturnType : TSepiType;                 /// Type de retour
    FCallingConvention : TCallingConvention; /// Convention d'appel

    FRegUsage : Byte;   /// Nombre de registres utilis�s (entre 0 et 3)
    FStackUsage : Word; /// Taille utilis�e sur la pile (en octets)

    procedure MakeCallInfo;

    function CheckInherited(ASignature : TSepiMethodSignature) : boolean;

    function GetParamCount : integer;
    function GetParams(Index : integer) : TSepiMetaParam;
    function GetActualParamCount : integer;
    function GetActualParams(Index : integer) : TSepiMetaParam;

    function GetHiddenParam(Kind : TSepiHiddenParamKind) : TSepiMetaParam;
  protected
    procedure ListReferences;
    procedure Save(Stream : TStream);
  public
    constructor RegisterTypeData(AOwner : TSepiReflectionItem;
      ATypeData : PTypeData);
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream);
    constructor Create(AOwner : TSepiReflectionItem; const ASignature : string;
      ACallingConvention : TCallingConvention = ccRegister);

    function Equals(ASignature : TSepiMethodSignature) : boolean;
    function CompatibleWith(const ATypes : array of TSepiType) : boolean;

    property Owner : TSepiReflectionItem read FOwner;
    property Kind : TMethodKind read FKind;

    property ParamCount : integer read GetParamCount;
    property Params[index : integer] : TSepiMetaParam read GetParams;
    property ActualParamCount : integer read GetActualParamCount;
    property ActualParams[index : integer] : TSepiMetaParam
      read GetActualParams;

    property ReturnType : TSepiType read FReturnType;
    property CallingConvention : TCallingConvention read FCallingConvention;

    property HiddenParam[Kind : TSepiHiddenParamKind] : TSepiMetaParam
      read GetHiddenParam;
    property SelfParam   : TSepiMetaParam index hpSelf   read GetHiddenParam;
    property ResultParam : TSepiMetaParam index hpResult read GetHiddenParam;
    property AllocParam  : TSepiMetaParam index hpAlloc  read GetHiddenParam;
    property FreeParam   : TSepiMetaParam index hpFree   read GetHiddenParam;

    property RegUsage : Byte read FRegUsage;
    property StackUsage : Word read FStackUsage;
  end;

  {*
    Meta-m�thode
    @author sjrd
    @version 1.0
  *}
  TSepiMetaMethod = class(TSepiReflectionItem)
  private
    FCode : Pointer;                   /// Adresse de code natif
    FCodeJumper : TJmpInstruction;     /// Jumper sur le code, si non native
    FSignature : TSepiMethodSignature; /// Signature de la m�thode
    FLinkKind : TMethodLinkKind;       /// Type de liaison d'appel
    FFirstDeclaration : boolean;       /// Faux uniquement quand 'override'
    FAbstract : boolean;               /// Indique si la m�thode est abstraite
    FInherited : TSepiMetaMethod;      /// M�thode h�rit�e

    FLinkIndex : integer; /// Offset de VMT ou index de DMT, selon la liaison

    FIsOverloaded : boolean;                 /// Indique si surcharg�e
    FOverloaded : TSepiMetaOverloadedMethod; /// M�thode surcharg�e (ou nil)
    FOverloadIndex : Byte;                   /// Index de surcharge

    procedure MakeLink;
    procedure FindNativeCode;

    function GetRealName : string;
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      ACode : Pointer; const ASignature : string;
      ALinkKind : TMethodLinkKind = mlkStatic; AAbstract : boolean = False;
      AMsgID : integer = 0;
      ACallingConvention : TCallingConvention = ccRegister);
    constructor CreateOverloaded(AOwner : TSepiReflectionItem;
      const AName : string; ACode : Pointer; const ASignature : string;
      ALinkKind : TMethodLinkKind = mlkStatic; AAbstract : boolean = False;
      AMsgID : integer = 0;
      ACallingConvention : TCallingConvention = ccRegister);
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure SetCode(ACode : Pointer);
    procedure SetCodeMethod(const AMethod : TMethod);

    property Code : Pointer read FCode;
    property Signature : TSepiMethodSignature read FSignature;
    property LinkKind : TMethodLinkKind read FLinkKind;
    property FirstDeclaration : boolean read FFirstDeclaration;
    property IsAbstract : boolean read FAbstract;
    property InheritedMethod : TSepiMetaMethod read FInherited;

    property VMTOffset : integer read FLinkIndex;
    property DMTIndex : integer read FLinkIndex;
    property MsgID : integer read FLinkIndex;
    property IMTIndex : integer read FLinkIndex;

    property IsOverloaded : boolean read FIsOverloaded;
    property Overloaded : TSepiMetaOverloadedMethod read FOverloaded;
    property OverloadIndex : Byte read FOverloadIndex;
    property RealName : string read GetRealName;
  end;

  {*
    Meta-m�thode surcharg�e
    @author sjrd
    @version 1.0
  *}
  TSepiMetaOverloadedMethod = class(TSepiReflectionItem)
  private
    FMethods : TObjectList; /// Liste des m�thodes de m�me nom

    function FindInherited(
      ASignature : TSepiMethodSignature) : TSepiMetaMethod;

    function GetMethodCount : integer;
    function GetMethods(Index : integer) : TSepiMetaMethod;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string);
    destructor Destroy; override;

    function FindMethod(
      ASignature : TSepiMethodSignature) : TSepiMetaMethod; overload;
    function FindMethod(
      const ATypes : array of TSepiType) : TSepiMetaMethod; overload;

    property MethodCount : integer read GetMethodCount;
    property Methods[index : integer] : TSepiMetaMethod read GetMethods;
    property DefaultMethod : TSepiMetaMethod index 0 read GetMethods;
  end;

  {*
    Acc�s de propri�t�
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyAccess = record
    Kind : TPropertyAccessKind;               /// Type d'acc�s
    case TPropertyAccessKind of
      pakNone : (Meta : TSepiReflectionItem); /// Meta d'acc�s (neutre)
      pakField : (Field : TSepiMetaField);    /// Meta-champ d'acc�s
      pakMethod : (Method : TSepiMetaMethod); /// Meta-m�thode d'acc�s
  end;

  {*
    Stockage de propri�t�
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyStorage = record
    Kind : TPropertyStorageKind;                  /// Type de stockage
    Stored : boolean;                             /// Constante de stockage
    case TPropertyStorageKind of
      pskConstant : (Meta : TSepiReflectionItem); /// Meta de stockage (neutre)
      pskField : (Field : TSepiMetaField);        /// Meta-champ de stockage
      pskMethod : (Method : TSepiMetaMethod);     /// Meta-m�thode de stockage
  end;

  {*
    Meta-propri�t�
    @author sjrd
    @version 1.0
  *}
  TSepiMetaProperty = class(TSepiReflectionItem)
  private
    FSignature : TSepiMethodSignature; /// Signature

    FReadAccess : TSepiPropertyAccess;  /// Acc�s en lecture
    FWriteAccess : TSepiPropertyAccess; /// Acc�s en �criture
    FIndex : integer;                   /// Index d'acc�s

    FDefaultValue : integer;         /// Valeur par d�faut
    FStorage : TSepiPropertyStorage; /// Sp�cificateur de stockage

    FIsDefault : boolean; /// Indique si c'est la propri�t� par d�faut

    procedure MakePropInfo(PropInfo : PPropInfo);

    function GetPropType : TSepiType;
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;

    procedure Destroying; override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;

    constructor Create(AOwner : TSepiReflectionItem;
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIndex : integer = NoIndex; ADefaultValue : integer = NoDefaultValue;
      const AStorage : string = ''; AIsDefault : boolean = False); overload;
    constructor Create(AOwner : TSepiReflectionItem;
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIsDefault : boolean); overload;

    constructor Redefine(AOwner : TSepiReflectionItem; const AName : string;
      const AReadAccess : string = ''; const AWriteAccess : string = '';
      const AStorage : string = ''); overload;
    constructor Redefine(AOwner : TSepiReflectionItem;
      const AName, AReadAccess, AWriteAccess : string; ADefaultValue : integer;
      const AStorage : string = ''); overload;

    destructor Destroy; override;

    property Signature : TSepiMethodSignature read FSignature;
    property PropType : TSepiType read GetPropType;

    property ReadAccess : TSepiPropertyAccess read FReadAccess;
    property WriteAccess : TSepiPropertyAccess read FWriteAccess;
    property Index : integer read FIndex;

    property DefaultValue : integer read FIndex;
    property Storage : TSepiPropertyStorage read FStorage;

    property IsDefault : boolean read FIsDefault;
  end;

  {*
    Type enregistrement
    @author sjrd
    @version 1.0
  *}
  TSepiRecordType = class(TSepiType)
  private
    FPacked : boolean;    /// Indique si le record est packed
    FAlignment : integer; /// Alignement
    FCompleted : boolean; /// Indique si le record est enti�rement d�fini

    function PrivAddField(const FieldName : string; FieldType : TSepiType;
      After : TSepiMetaField;
      ForcePack : boolean = False) : TSepiMetaField;

    procedure MakeTypeInfo;
  protected
    procedure ChildAdded(Child : TSepiReflectionItem); override;

    procedure Save(Stream : TStream); override;

    function GetAlignment : integer; override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      APacked : boolean = False; AIsNative : boolean = False;
      ATypeInfo : PTypeInfo = nil);

    function AddField(const FieldName : string; FieldType : TSepiType;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddField(const FieldName : string; FieldTypeInfo : PTypeInfo;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddField(const FieldName, FieldTypeName : string;
      ForcePack : boolean = False) : TSepiMetaField; overload;

    function AddFieldAfter(const FieldName : string; FieldType : TSepiType;
      const After : string;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddFieldAfter(const FieldName : string; FieldTypeInfo : PTypeInfo;
      const After : string;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddFieldAfter(const FieldName, FieldTypeName : string;
      const After : string;
      ForcePack : boolean = False) : TSepiMetaField; overload;

    procedure Complete;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property IsPacked : boolean read FPacked;
    property Completed : boolean read FCompleted;
  end;

  {*
    Interface
    @author sjrd
    @version 1.0
  *}
  TSepiInterface = class(TSepiType)
  private
    FParent : TSepiInterface; /// Interface parent (ou nil - IInterface)
    FCompleted : boolean;     /// Indique si l'interface est enti�rement d�finie

    FHasGUID : boolean;         /// Indique si l'interface poss�de un GUID
    FIsDispInterface : boolean; /// Indique si c'est une disp interface
    FIsDispatch : boolean;      /// Indique si c'est une IDispatch
    FGUID : TGUID;              /// GUID de l'interface, si elle en a un

    FIMTSize : integer; /// Taille de l'IMT

    procedure MakeTypeInfo;
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiReflectionItem;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      AParent : TSepiInterface; const AGUID : TGUID;
      AIsDispInterface : boolean = False);

    class function NewInstance : TObject; override;

    class function ForwardDecl(AOwner : TSepiReflectionItem;
      ATypeInfo : PTypeInfo) : TSepiInterface; overload;
    class function ForwardDecl(AOwner : TSepiReflectionItem;
      const AName : string) : TSepiInterface; overload;

    function AddMethod(const MethodName, ASignature : string;
      ACallingConvention : TCallingConvention = ccRegister) : TSepiMetaMethod;

    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIndex : integer = NoIndex;
      AIsDefault : boolean = False) : TSepiMetaProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIsDefault : boolean) : TSepiMetaProperty; overload;

    procedure Complete;

    function IntfInheritsFrom(AParent : TSepiInterface) : boolean;

    function LookForMember(const MemberName : string) : TSepiReflectionItem;

    property Parent : TSepiInterface read FParent;
    property Completed : boolean read FCompleted;

    property HasGUID : boolean read FHasGUID;
    property IsDispInterface : boolean read FIsDispInterface;
    property IsDispatch : boolean read FIsDispatch;
    property GUID : TGUID read FGUID;

    property IMTSize : integer read FIMTSize;
  end;

  {*
    Pointeur vers TSepiInterfaceEntry
  *}
  PSepiInterfaceEntry = ^TSepiInterfaceEntry;

  {*
    Entr�e d'interface pour les classes Sepi
    Chaque classe a, pour chaque interface qu'elle impl�mente, une entr�e de
    type TSepiInterfaceEntry.
  *}
  TSepiInterfaceEntry = record
    IntfRef : TSepiInterface; /// Interface impl�ment�e
    Relocates : Pointer;      /// Thunks de relocalisation
    IMT : Pointer;            /// Interface Method Table
    Offset : integer;         /// Offset du champ interface dans l'objet
  end;

  {*
    Classe (type objet)
    @author sjrd
    @version 1.0
  *}
  TSepiClass = class(TSepiType)
  private
    FDelphiClass : TClass; /// Classe Delphi
    FParent : TSepiClass;  /// Classe parent (nil si n'existe pas - TObject)
    FCompleted : boolean;  /// Indique si la classe est enti�rement d�finie

    /// Interfaces support�es par la classe
    FInterfaces : array of TSepiInterfaceEntry;

    FInstSize : integer;     /// Taille d'une instance de la classe
    FVMTSize : integer;      /// Taille de la VMT dans les index positifs
    FDMTNextIndex : integer; /// Prochain index � utiliser dans la DMT

    FCurrentVisibility : TMemberVisibility; /// Visibilit� courante des enfants

    procedure MakeIMT(IntfEntry : PSepiInterfaceEntry);
    procedure MakeIMTs;

    procedure MakeTypeInfo;
    procedure MakeIntfTable;
    procedure MakeInitTable;
    procedure MakeFieldTable;
    procedure MakeMethodTable;
    procedure MakeDMT;
    procedure MakeVMT;

    function GetInterfaceCount : integer;
    function GetInterfaces(Index : integer) : TSepiInterface;

    function GetVMTEntries(Index : integer) : Pointer;
    procedure SetVMTEntries(Index : integer; Value : Pointer);
  protected
    procedure ChildAdded(Child : TSepiReflectionItem); override;

    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;

    property VMTEntries[index : integer] : Pointer
      read GetVMTEntries write SetVMTEntries;
  public
    constructor RegisterTypeInfo(AOwner : TSepiReflectionItem;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      AParent : TSepiClass);
    destructor Destroy; override;

    class function NewInstance : TObject; override;

    class function ForwardDecl(AOwner : TSepiReflectionItem;
      ATypeInfo : PTypeInfo) : TSepiClass; overload;
    class function ForwardDecl(AOwner : TSepiReflectionItem;
      const AName : string) : TSepiClass; overload;

    procedure AddInterface(AInterface : TSepiInterface); overload;
    procedure AddInterface(AIntfTypeInfo : PTypeInfo); overload;
    procedure AddInterface(const AIntfName : string); overload;

    function AddField(const FieldName : string; FieldType : TSepiType;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddField(const FieldName : string; FieldTypeInfo : PTypeInfo;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddField(const FieldName, FieldTypeName : string;
      ForcePack : boolean = False) : TSepiMetaField; overload;

    function AddMethod(const MethodName : string; ACode: Pointer;
      const ASignature : string; ALinkKind : TMethodLinkKind = mlkStatic;
      AAbstract : boolean = False; AMsgID : integer = 0;
      ACallingConvention : TCallingConvention = ccRegister) : TSepiMetaMethod;
      overload;
    function AddMethod(const MethodName : string; ACode: Pointer;
      const ASignature : string;
      ACallingConvention : TCallingConvention) : TSepiMetaMethod; overload;
    function AddOverloadedMethod(const MethodName : string; ACode: Pointer;
      const ASignature : string; ALinkKind : TMethodLinkKind = mlkStatic;
      AAbstract : boolean = False; AMsgID : integer = 0;
      ACallingConvention : TCallingConvention = ccRegister) : TSepiMetaMethod;
      overload;
    function AddOverloadedMethod(const MethodName : string; ACode: Pointer;
      const ASignature : string;
      ACallingConvention : TCallingConvention) : TSepiMetaMethod; overload;

    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIndex : integer = NoIndex; ADefaultValue : integer = NoDefaultValue;
      const AStorage : string = '';
      AIsDefault : boolean = False) : TSepiMetaProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIsDefault : boolean) : TSepiMetaProperty; overload;

    function RedefineProperty(const AName : string;
      const AReadAccess : string = ''; const AWriteAccess : string = '';
      const AStorage : string = '') : TSepiMetaProperty; overload;
    function RedefineProperty(
      const AName, AReadAccess, AWriteAccess : string; ADefaultValue : integer;
      const AStorage : string = '') : TSepiMetaProperty; overload;

    procedure Complete;

    function CompatibleWith(AType : TSepiType) : boolean; override;
    function ClassInheritsFrom(AParent : TSepiClass) : boolean;

    function LookForMember(const MemberName : string; FromUnit : TSepiUnit;
      FromClass : TSepiClass = nil) : TSepiReflectionItem;

    property DelphiClass : TClass read FDelphiClass;
    property Parent : TSepiClass read FParent;
    property Completed : boolean read FCompleted;

    property InterfaceCount : integer read GetInterfaceCount;
    property Interfaces[index : integer] : TSepiInterface read GetInterfaces;

    property InstSize : integer read FInstSize;
    property VMTSize : integer read FVMTSize;

    property CurrentVisibility : TMemberVisibility
      read FCurrentVisibility write FCurrentVisibility;
  end;

  {*
    Meta-classe (type classe)
    @author sjrd
    @version 1.0
  *}
  TSepiMetaClass = class(TSepiType)
  private
    FClass : TSepiClass; /// Classe correspondante
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      AClass : TSepiClass; AIsNative : boolean = False); overload;
    constructor Create(AOwner : TSepiReflectionItem; const AName : string;
      AClassInfo : PTypeInfo; AIsNative : boolean = False); overload;
    constructor Create(AOwner : TSepiReflectionItem;
      const AName, AClassName : string; AIsNative : boolean = False); overload;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property SepiClass : TSepiClass read FClass;
  end;

  {*
    Type r�f�rence de m�thode
    @author sjrd
    @version 1.0
  *}
  TSepiMethodRefType = class(TSepiType)
  private
    FSignature : TSepiMethodSignature; /// Signature
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiReflectionItem;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
    constructor Create(AOwner : TSepiReflectionItem;
      const AName, ASignature : string; AOfObject : boolean = False;
      ACallingConvention : TCallingConvention = ccRegister);
    destructor Destroy; override;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property Signature : TSepiMethodSignature read FSignature;
  end;

  {*
    Type variant
    Note : il est impossible de cr�er un nouveau type variant.
    @author sjrd
    @version 1.0
  *}
  TSepiVariantType = class(TSepiType)
  protected
    function GetAlignment : integer; override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiReflectionItem;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiReflectionItem; Stream : TStream); override;
  end;

const
  /// Proc�dure d'unit�
  mkUnitProcedure = TMethodKind(integer(High(TMethodKind))+1);
  /// Fonction d'unit�
  mkUnitFunction = TMethodKind(integer(High(TMethodKind))+2);
  /// Propri�t�
  mkProperty = TMethodKind(integer(High(TMethodKind))+3);

  /// Types de m�thodes d'objet
  mkOfObject = [mkProcedure..mkClassConstructor];

  /// Format du nom d'une m�thode surcharg�e
  OverloadedNameFormat = 'OL$%s$%d';

  /// Noms des param�tres cach�s
  HiddenParamNames : array[TSepiHiddenParamKind] of string = (
    '', 'Self', 'Result', '$Alloc', '$Free', 'High$'
  );

  /// Cha�nes des types de param�tre
  ParamKindStrings : array[TSepiParamKind] of string = (
    '', 'var', 'const', 'out'
  );

  /// Cha�nes des types de m�thode
  MethodKindStrings : array[mkProcedure..mkProperty] of string = (
    'procedure', 'function', 'constructor', 'destructor',
    'class procedure', 'class function', 'class constructor',
    '', '', '', 'unit procedure', 'unit function', 'property'
  );

implementation

type
  TInitInfo = packed record
    TypeInfo : PPTypeInfo;
    Offset : Cardinal;
  end;

  PInitTable = ^TInitTable;
  TInitTable = packed record
    Size : Cardinal;
    Count : Cardinal;
    Fields : array[0..0] of TInitInfo;
  end;

  PFieldInfo = ^TFieldInfo;
  TFieldInfo = packed record
    Offset : Cardinal;
    Index : Word;
    Name : ShortString;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    FieldCount : Word;
    Unknown : Pointer;
    {Fields : array[1..FieldCount] of TFieldInfo;}
  end;

  PMethodInfo = ^TMethodInfo;
  TMethodInfo = packed record
    Size : Word;
    Code : Pointer;
    Name : ShortString;
  end;

  PMethodTable = ^TMethodTable;
  TMethodTable = packed record
    MethodCount : Word;
    {Methods : array[1..MethodCount] of TMethodInfo;}
  end;

const
  // Tailles de structure TTypeData en fonction des types
  RecordTypeDataLengthBase = 2*sizeof(Cardinal);
  IntfTypeDataLengthBase =
    sizeof(Pointer) + sizeof(TIntfFlagsBase) + sizeof(TGUID) + 2*sizeof(Word);
  ClassTypeDataLengthBase =
    sizeof(TClass) + sizeof(Pointer) + sizeof(SmallInt) + sizeof(Word);
  PropInfoLengthBase = sizeof(TPropInfo) - sizeof(ShortString);
  InitTableLengthBase = 2*sizeof(Byte) + 2*sizeof(Cardinal);
  FieldTableLengthBase = sizeof(TFieldTable);
  MethodTableLengthBase = sizeof(TMethodTable);

  vmtMinIndex = vmtSelfPtr;
  vmtMinMethodIndex = vmtParent + 4;

procedure MakePropertyAccessKind(var Access : TSepiPropertyAccess);
begin
  with Access do
  begin
    if Meta is TSepiMetaField then
      Kind := pakField
    else if Meta is TSepiMetaMethod then
      Kind := pakMethod
    else
      Kind := pakNone;
  end;
end;

procedure MakePropertyStorage(out Storage : TSepiPropertyStorage;
  const AStorage : string; Owner : TSepiClass);
begin
  if (AStorage = '') or AnsiSameText(AStorage, BooleanIdents[True]) then
  begin
    Storage.Kind := pskConstant;
    Storage.Stored := True;
    Storage.Meta := nil;
  end else
  if AnsiSameText(AStorage, BooleanIdents[False]) then
  begin
    Storage.Kind := pskConstant;
    Storage.Stored := False;
    Storage.Meta := nil;
  end else
  begin
    Storage.Stored := False;
    Storage.Meta := Owner.LookForMember(AStorage, Owner.OwningUnit, Owner);

    if Storage.Meta is TSepiMetaField then
      Storage.Kind := pskField
    else
      Storage.Kind := pskMethod;
  end;
end;

{-----------------------}
{ Classe TSepiMetaField }
{-----------------------}

{*
  Charge un champ depuis un flux
*}
constructor TSepiMetaField.Load(AOwner : TSepiReflectionItem; Stream : TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FType);
  Stream.ReadBuffer(FOffset, 4);
end;

{*
  Cr�e un nouveau champ
  @param AOwner   Propri�taire du champ
  @param AName    Nom du champ
  @param AType    Type du champ
*}
constructor TSepiMetaField.Create(AOwner : TSepiReflectionItem;
  const AName : string; AType : TSepiType; AOffset : integer);
begin
  inherited Create(AOwner, AName);
  FType := AType;
  FOffset := AOffset;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaField.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaField.Save(Stream : TStream);
begin
  inherited;
  OwningUnit.WriteRef(Stream, FType);
  Stream.WriteBuffer(FOffset, 4);
end;

{-----------------------}
{ Classe TSepiMetaParam }
{-----------------------}

{*
  Cr�e un param�tre cach�
  @param AOwner        Propri�taire du param�tre
  @param AHiddenKind   Type de param�tre cach� (doit �tre diff�rent de pkNormal)
  @param AType         Type du param�tre (uniquement pour AKind = pkResult)
*}
constructor TSepiMetaParam.CreateHidden(AOwner : TSepiReflectionItem;
  AHiddenKind : TSepiHiddenParamKind; AType : TSepiType = nil);
var AName : string;
begin
  Assert(AHiddenKind <> hpNormal);
  AName := HiddenParamNames[AHiddenKind];
  if AHiddenKind = hpOpenArrayHighValue then
    AName := AName + AOwner.Children[AOwner.ChildCount-1].Name;

  inherited Create(AOwner, AName);

  FHiddenKind := AHiddenKind;
  FKind := pkValue;
  FByRef := False;
  FOpenArray := False;

  case HiddenKind of
    hpSelf :
    begin
      if Owner.Owner is TSepiClass then
        FType := TSepiClass(Owner.Owner)
      else
        FType := Root.FindType(TypeInfo(TObject));
    end;
    hpResult :
    begin
      FKind := pkOut;
      FByRef := True;
      FType := AType;
    end;
    hpOpenArrayHighValue : FType := Root.FindType(TypeInfo(SmallInt));
    else FType := Root.FindType(TypeInfo(Boolean));
  end;

  MakeFlags;
end;

{*
  Cr�e un param�tre depuis les donn�es de type d'une r�f�rence de m�thode
  En sortie, le pointeur ParamData a avanc� jusqu'au param�tre suivant
  @param AOwner      Propri�taire du param�tre
  @param ParamData   Pointeur vers les donn�es du param�tre
*}
constructor TSepiMetaParam.RegisterParamData(AOwner : TSepiReflectionItem;
  var ParamData : Pointer);
var AFlags : TParamFlags;
    AName, ATypeStr : string;
    AKind : TSepiParamKind;
    AOpenArray : boolean;
begin
  FHiddenKind := hpNormal;
  AFlags := TParamFlags(ParamData^);
  inc(LongInt(ParamData), sizeof(TParamFlags));
  AName := PShortString(ParamData)^;
  inc(LongInt(ParamData), PByte(ParamData)^ + 1);
  ATypeStr := PShortString(ParamData)^;
  inc(LongInt(ParamData), PByte(ParamData)^ + 1);

  if pfVar   in AFlags then AKind := pkVar   else
  if pfConst in AFlags then AKind := pkConst else
  if pfOut   in AFlags then AKind := pkOut   else
  AKind := pkValue;

  AOpenArray := pfArray in AFlags;

  { Work around of bug of the Delphi compiler with parameters written like:
      var Param: ShortString
    The compiler writes into the RTTI that it is a 'string', in this case.
    Fortunately, this does not happen when the parameter is an open array, so
    we can spot this by checking the pfReference flag, which is to be present
    in case of ShortString, and not in case of string. }
  if (AKind = pkVar) and (pfReference in AFlags) and (not AOpenArray) and
     AnsiSameText(ATypeStr, 'string') then {don't localize}
    ATypeStr := 'ShortString';

  Create(AOwner, AName, AOwner.Root.FindType(ATypeStr), AKind, AOpenArray);

  Assert(FFlags = AFlags, Format('FFlags (%s) <> AFlags (%s) for %s',
    [EnumSetToStr(FFlags, TypeInfo(TParamFlags)),
     EnumSetToStr(AFlags, TypeInfo(TParamFlags)),
     GetFullName]));
end;

{*
  Cr�e un ou plusieurs param�tre(s) depuis sa d�finition Delphi
  @param AOwner       Propri�taire du ou des param�tre(s)
  @param Definition   D�finition Delphi du ou des param�tre(s)
*}
class procedure TSepiMetaParam.CreateFromString(AOwner : TSepiReflectionItem;
  const Definition : string);
var NamePart, NamePart2, TypePart, KindStr, AName, ATypeStr : string;
    AKind : TSepiParamKind;
    AOpenArray : boolean;
    AType : TSepiType;
begin
  if not SplitToken(Definition, ':', NamePart, TypePart) then
    TypePart := '';
  TypePart := GetFirstToken(TypePart, '=');

  // Partie du nom - � gauche du :
  if SplitToken(Trim(NamePart), ' ', KindStr, NamePart2) then
  begin
    ShortInt(AKind) := AnsiIndexText(KindStr, ParamKindStrings);
    if ShortInt(AKind) < 0 then
    begin
      AKind := pkValue;
      NamePart2 := KindStr + ' ' + NamePart2;
    end;
  end else AKind := pkValue;
  NamePart := NamePart2;

  // Partie du type - � droite du :
  TypePart := Trim(TypePart);
  if AnsiStartsText('array of ', TypePart) then {don't localize}
  begin
    AOpenArray := True;
    ATypeStr := TrimLeft(Copy(TypePart, 10, MaxInt)); // 10 is 'array of |'

    if AnsiSameText(ATypeStr, 'const') then {don't localize}
      ATypeStr := '';
  end else
  begin
    AOpenArray := False;
    ATypeStr := TypePart;
  end;
  AType := AOwner.Root.FindType(ATypeStr);

  // Cr�ation du (ou des) param�tre(s)
  while SplitToken(NamePart, ',', AName, NamePart2) do
  begin
    Create(AOwner, Trim(AName), AType, AKind, AOpenArray);
    NamePart := NamePart2;
  end;
  Create(AOwner, Trim(AName), AType, AKind, AOpenArray);
end;

{*
  Charge un param�tre depuis un flux
*}
constructor TSepiMetaParam.Load(AOwner : TSepiReflectionItem; Stream : TStream);
begin
  inherited;

  Stream.ReadBuffer(FHiddenKind, 1);
  Stream.ReadBuffer(FKind, 1);
  FByRef := FKind in [pkVar, pkOut];
  Stream.ReadBuffer(FOpenArray, 1);
  OwningUnit.ReadRef(Stream, FType);

  MakeFlags;
  Stream.ReadBuffer(FCallInfo, sizeof(TSepiParamCallInfo));
end;

{*
  Cr�e un nouveau param�tre
  @param AOwner       Propri�taire du param�tre
  @param AName        Nom du param�tre
  @param AType        Type du param�tre
  @param AKind        Type de param�tre
  @param AOpenArray   True pour un param�tre tableau ouvert
*}
constructor TSepiMetaParam.Create(AOwner : TSepiReflectionItem;
  const AName : string; AType : TSepiType; AKind : TSepiParamKind = pkValue;
  AOpenArray : boolean = False);
begin
  inherited Create(AOwner, AName);

  FHiddenKind := hpNormal;
  FKind := AKind;
  FByRef := FKind in [pkVar, pkOut];
  FOpenArray := AOpenArray;
  FType := AType;

  MakeFlags;

  if OpenArray then
    TSepiMetaParam.CreateHidden(Owner, hpOpenArrayHighValue);
end;

{*
  Construit la propri�t� Flags
*}
procedure TSepiMetaParam.MakeFlags;
begin
  // Param kind flag
  case FKind of
    pkValue : FFlags := [];
    pkVar   : FFlags := [pfVar];
    pkConst : FFlags := [pfConst];
    pkOut   : FFlags := [pfOut];
  end;

  // Flags pfArray, pfAddress and pfReference
  if FOpenArray then
  begin
    Include(FFlags, pfArray);
    Include(FFlags, pfReference);
  end else
  begin
    if (FType is TSepiClass) or (FType is TSepiInterface) then
      Include(FFlags, pfAddress);
    if (FType <> nil) and (FType.ParamBehavior.AlwaysByAddress) then
      Include(FFlags, pfReference);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaParam.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaParam.Save(Stream : TStream);
begin
  inherited;

  Stream.WriteBuffer(FHiddenKind, 1);
  Stream.WriteBuffer(FKind, 1);
  Stream.WriteBuffer(FOpenArray, 1);
  OwningUnit.WriteRef(Stream, FType);

  Stream.WriteBuffer(FCallInfo, sizeof(TSepiParamCallInfo));
end;

{*
  D�termine si deux param�tres sont identiques
  @param AParam   Param�tre � comparer
  @return True si les param�tres sont identiques, False sinon
*}
function TSepiMetaParam.Equals(AParam : TSepiMetaParam) : boolean;
begin
  Result := HiddenKind = AParam.HiddenKind;
  if Result and (HiddenKind = hpNormal) then
    Result := (Kind = AParam.Kind) and (OpenArray = AParam.OpenArray) and
      (ParamType = AParam.ParamType);
end;

{*
  D�termine si un type est compatible avec le param�tre
  @param AType   Type � Tester
  @return True si le type est compatible, False sinon
*}
function TSepiMetaParam.CompatibleWith(AType : TSepiType) : boolean;
begin
  if ByRef then
    Result := FType = AType
  else
    Result := FType.CompatibleWith(AType);
end;

{-----------------------------}
{ Classe TSepiMethodSignature }
{-----------------------------}

{*
  Cr�e une signature � partir des donn�es de type d'un type m�thode
  @param AOwner      Propri�taire de la signature
  @param ATypeData   Donn�es de type
*}
constructor TSepiMethodSignature.RegisterTypeData(AOwner : TSepiReflectionItem;
  ATypeData : PTypeData);
var ParamData : Pointer;
    I : integer;
begin
  inherited Create;

  FOwner := AOwner;
  FKind := ATypeData.MethodKind;
  ParamData := @ATypeData.ParamList;

  TSepiMetaParam.CreateHidden(FOwner, hpSelf);
  for I := 1 to ATypeData.ParamCount do
    TSepiMetaParam.RegisterParamData(FOwner, ParamData);

  FCallingConvention := ccRegister;

  if Kind in [mkFunction, mkClassFunction]then
  begin
    FReturnType := FOwner.Root.FindType(PShortString(ParamData)^);
    TSepiMetaParam.CreateHidden(FOwner, hpResult, ReturnType);
  end else
    FReturnType := nil;

  MakeCallInfo;
end;

{*
  Charge une signature depuis un flux
  @param AOwner   Propri�taire de la signature
  @param Stream   Flux depuis lequel charger la signature
*}
constructor TSepiMethodSignature.Load(AOwner : TSepiReflectionItem;
  Stream : TStream);
begin
  inherited Create;

  FOwner := AOwner;
  Stream.ReadBuffer(FKind, 1);
  FOwner.OwningUnit.ReadRef(Stream, FReturnType);
  Stream.ReadBuffer(FCallingConvention, 1);
  Stream.ReadBuffer(FRegUsage, 1);
  Stream.ReadBuffer(FStackUsage, 2);

  // Parameters should be loaded by the owner, for they are children of it
end;

{*
  Cr�e une signature de m�thode
  @param AOwner               Propri�taire de la signature
  @param ASignature           Signature Delphi
  @param ACallingConvention   Convention d'appel
*}
constructor TSepiMethodSignature.Create(AOwner : TSepiReflectionItem;
  const ASignature : string;
  ACallingConvention : TCallingConvention = ccRegister);

  function MultiPos(const Chars : array of Char; const Str : string;
    Offset : integer = 1) : integer;
  var I : integer;
  begin
    for I := Low(Chars) to High(Chars) do
    begin
      Result := PosEx(Chars[I], Str, Offset);
      if Result > 0 then exit;
    end;
    Result := Length(Str)+1;
  end;

var ParamPos, ReturnTypePos, ParamEnd : integer;
begin
  inherited Create;

  FOwner := AOwner;
  FKind := mkProcedure;
  FReturnType := nil;
  FCallingConvention := ACallingConvention;

  // Type de m�thode
  ParamPos := MultiPos(['(', '[', ':'], ASignature);
  FKind := TMethodKind(AnsiIndexText(
    Trim(Copy(ASignature, 1, ParamPos-1)), MethodKindStrings));

  // Type de retour
  if (Kind = mkFunction) or (Kind = mkClassFunction) or
     (Kind = mkUnitFunction) or (Kind = mkProperty) then
  begin
    ReturnTypePos := RightPos(':', ASignature);
    FReturnType := FOwner.Root.FindType(
      Trim(Copy(ASignature, ReturnTypePos+1, MaxInt)));
  end else ReturnTypePos := Length(ASignature);

  // Param�tres
  if (Kind in mkOfObject) and (CallingConvention <> ccPascal) then
    TSepiMetaParam.CreateHidden(FOwner, hpSelf);

  if Kind = mkConstructor then
    TSepiMetaParam.CreateHidden(FOwner, hpAlloc)
  else if Kind = mkDestructor then
    TSepiMetaParam.CreateHidden(FOwner, hpFree);

  if ParamPos < ReturnTypePos then
  begin
    while not (ASignature[ParamPos] in [')', ']', ':']) do
    begin
      inc(ParamPos);
      ParamEnd := MultiPos([';', ')', ']', ':'], ASignature, ParamPos);
      TSepiMetaParam.CreateFromString(FOwner,
        Copy(ASignature, ParamPos, ParamEnd-ParamPos));
      ParamPos := ParamEnd;
    end;
  end;

  if ReturnType <> nil then
    TSepiMetaParam.CreateHidden(FOwner, hpResult, ReturnType);

  if (Kind in mkOfObject) and (CallingConvention = ccPascal) then
    TSepiMetaParam.CreateHidden(FOwner, hpSelf);

  MakeCallInfo;
end;

{*
  Construit les informations d'appel de la signature
*}
procedure TSepiMethodSignature.MakeCallInfo;
var I, ParamCount : integer;
    Param : TSepiMetaParam;
begin
  if CallingConvention = ccRegister then
    FRegUsage := 0
  else
    FRegUsage := 3;
  FStackUsage := 0;

  ParamCount := ActualParamCount;

  for I := 0 to ParamCount-1 do
  begin
    // pascal calling convention works in the opposite direction
    if CallingConvention = ccPascal then
      Param := ActualParams[ParamCount-I-1]
    else
      Param := ActualParams[I];

    with Param.ParamType, Param, FCallInfo do
    begin
      // By address or by value?
      if HiddenKind <> hpResult then
      begin
        ByAddress := ByRef or (pfReference in Flags) or (ParamType = nil);

        if ByAddress or (not ParamBehavior.AlwaysByStack) then
          Byte(Place) := FRegUsage
        else
          Place := ppStack;
      end else
      begin
        ByAddress := ParamBehavior.AlwaysByAddress;
        Place := ppEAX;
        StackOffset := 0;
      end;

      // Where to place it?
      if ByAddress or (HiddenKind <> hpResult) then
      begin
        if Place = ppStack then
        begin
          StackOffset := FStackUsage;
          if ByAddress then
            inc(FStackUsage, 4) else
          begin
            inc(FStackUsage, Size);
            if FStackUsage mod 4 <> 0 then
              FStackUsage := (FStackUsage and $FFFC) + 4;
          end;
        end else
        begin
          inc(FRegUsage);
          StackOffset := 0;
        end;
      end;
    end;
  end;

  if CallingConvention <> ccRegister then
    FRegUsage := 0;
end;

{*
  V�rifie que la signature peut h�riter d'une autre
  CheckInherited met � jour la convention d'appel si besoin est, contrairement
  � Equals.
  @param ASignature   Signature � comparer
  @return True si les signatures sont identiques, False sinon
*}
function TSepiMethodSignature.CheckInherited(
  ASignature : TSepiMethodSignature) : boolean;
var OldCallingConv : TCallingConvention;
begin
  Result := False;
  OldCallingConv := CallingConvention;
  FCallingConvention := ASignature.CallingConvention;

  try
    Result := Equals(ASignature);
  finally
    if not Result then
      FCallingConvention := OldCallingConv
    else if FCallingConvention <> OldCallingConv then
      MakeCallInfo;
  end;
end;

{*
  Nombre de param�tres
  @return Nombre de param�tres
*}
function TSepiMethodSignature.GetParamCount : integer;
var I : integer;
    Child : TSepiReflectionItem;
begin
  Result := 0;
  for I := 0 to Owner.ChildCount-1 do
  begin
    Child := Owner.Children[I];
    if (Child is TSepiMetaParam) and
       (TSepiMetaParam(Child).HiddenKind = hpNormal) then
      inc(Result);
  end;
end;

{*
  Tableau zero-based des param�tres
  @param Index   Index du param�tre � r�cup�rer
  @return Param�tre situ� � l'index Index
*}
function TSepiMethodSignature.GetParams(Index : integer) : TSepiMetaParam;
var I : integer;
    Child : TSepiReflectionItem;
begin
  for I := 0 to Owner.ChildCount-1 do
  begin
    Child := Owner.Children[I];
    if (Child is TSepiMetaParam) and
       (TSepiMetaParam(Child).HiddenKind = hpNormal) then
    begin
      if Index > 0 then dec(Index) else
      begin
        Result := TSepiMetaParam(Child);
        exit;
      end;
    end;
  end;

  raise EListError.CreateFmt(SListIndexError, [Index]);
end;

{*
  Nombre de param�tres r�els (incluant les param�tres cach�s)
  @return Nombre de param�tres r�els
*}
function TSepiMethodSignature.GetActualParamCount : integer;
var I : integer;
    Child : TSepiReflectionItem;
begin
  Result := 0;
  for I := 0 to Owner.ChildCount-1 do
  begin
    Child := Owner.Children[I];
    if Child is TSepiMetaParam then
      inc(Result);
  end;
end;

{*
  Tableau zero-based des param�tres r�els (incluant les param�tres cach�s)
  @param Index   Index du param�tre � r�cup�rer
  @return Param�tre situ� � l'index Index
*}
function TSepiMethodSignature.GetActualParams(Index : integer) : TSepiMetaParam;
var I : integer;
    Child : TSepiReflectionItem;
begin
  for I := 0 to Owner.ChildCount-1 do
  begin
    Child := Owner.Children[I];
    if Child is TSepiMetaParam then
    begin
      if Index > 0 then dec(Index) else
      begin
        Result := TSepiMetaParam(Child);
        exit;
      end;
    end;
  end;

  raise EListError.CreateFmt(SListIndexError, [Index]);
end;

{*
  Param�tres cach�s d'apr�s leur type
  @param Kind   Type de param�tre cach�
  @return Le param�tre cach� correspondant, ou nil s'il n'y en a pas
*}
function TSepiMethodSignature.GetHiddenParam(
  Kind : TSepiHiddenParamKind) : TSepiMetaParam;
var I : integer;
    Child : TSepiReflectionItem;
begin
  Assert(Kind in [hpSelf, hpResult, hpAlloc, hpFree]);

  for I := 0 to Owner.ChildCount-1 do
  begin
    Child := Owner.Children[I];
    if (Child is TSepiMetaParam) and
       (TSepiMetaParam(Child).HiddenKind = Kind) then
    begin
      Result := TSepiMetaParam(Child);
      exit;
    end;
  end;

  Result := nil;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodSignature.ListReferences;
begin
  Owner.OwningUnit.AddRef(FReturnType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodSignature.Save(Stream : TStream);
begin
  Stream.WriteBuffer(FKind, 1);
  Owner.OwningUnit.WriteRef(Stream, FReturnType);
  Stream.WriteBuffer(FCallingConvention, 1);
  Stream.WriteBuffer(FRegUsage, 1);
  Stream.WriteBuffer(FStackUsage, 2);

  // Parameters should be saved by the owner, for they are children of it
end;

{*
  D�termine si deux signatures sont identiques
  @param ASignature   Signature � comparer
  @return True si les signatures sont identiques, False sinon
*}
function TSepiMethodSignature.Equals(
  ASignature : TSepiMethodSignature) : boolean;
var I : integer;
begin
  Result := False;

  if Kind <> ASignature.Kind then exit;
  if CallingConvention <> ASignature.CallingConvention then exit;

  if ParamCount <> ASignature.ParamCount then exit;
  for I := 0 to ParamCount-1 do
    if not Params[I].Equals(ASignature.Params[I]) then exit;
  if ReturnType <> ASignature.ReturnType then exit;

  Result := True;
end;

{*
  D�termine si une liste de types est compatible avec la signature
  @param ATypes   Liste des types � tester
  @return True si la liste de types est compatible, False sinon
*}
function TSepiMethodSignature.CompatibleWith(
  const ATypes : array of TSepiType) : boolean;
var I : integer;
begin
  Result := False;

  if ParamCount <> Length(ATypes) then exit;
  for I := 0 to ParamCount-1 do
    if not Params[I].CompatibleWith(ATypes[Low(ATypes)+I]) then exit;

  Result := True;
end;

{------------------------}
{ Classe TSepiMetaMethod }
{------------------------}

{*
  Charge une meta-m�thode depuis un flux
*}
constructor TSepiMetaMethod.Load(AOwner : TSepiReflectionItem;
  Stream : TStream);
begin
  inherited;

  FCode := nil;
  FSignature := TSepiMethodSignature.Load(Self, Stream);
  LoadChildren(Stream);
  Stream.ReadBuffer(FLinkKind, 1);
  FFirstDeclaration := FLinkKind <> mlkOverride;
  Stream.ReadBuffer(FAbstract, 1);
  Stream.ReadBuffer(FLinkIndex, 2); // only for messages

  Stream.ReadBuffer(FIsOverloaded, 1);
  if FIsOverloaded then
  begin
    OwningUnit.ReadRef(Stream, FOverloaded);
    Stream.ReadBuffer(FOverloadIndex, 1);
  end else
  begin
    FOverloaded := nil;
    FOverloadIndex := 0;
  end;

  MakeLink;

  if not IsAbstract then
  begin
    if Assigned(OwningUnit.OnGetMethodCode) then
      OwningUnit.OnGetMethodCode(Self, FCode);
    if not Assigned(FCode) then
      FCode := @FCodeJumper;
  end;
end;

{*
  Cr�e une nouvelle meta-m�thode
  @param AOwner       Propri�taire de la m�thode
  @param AName        Nom de la m�thode
  @param ACode        Pointeur sur le code de la m�thode
  @param ASignature   Signature Delphi de la m�thode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la m�thode est abstraite
  @param AMsgID       Pour les m�thodes de message, le message intercept�
  @param ACallingConvention   Convention d'appel de la m�thode
*}
constructor TSepiMetaMethod.Create(AOwner : TSepiReflectionItem;
  const AName : string; ACode : Pointer; const ASignature : string;
  ALinkKind : TMethodLinkKind = mlkStatic; AAbstract : boolean = False;
  AMsgID : integer = 0; ACallingConvention : TCallingConvention = ccRegister);
var SignPrefix : string;
begin
  inherited Create(AOwner, AName);

  if Owner is TSepiUnit then
    SignPrefix := 'unit '
  else
    SignPrefix := '';

  FCode := ACode;
  FSignature := TSepiMethodSignature.Create(Self,
    SignPrefix + ASignature, ACallingConvention);
  FLinkKind := ALinkKind;
  FFirstDeclaration := FLinkKind <> mlkOverride;
  FAbstract := AAbstract and (FLinkKind in [mlkVirtual, mlkDynamic]);
  FLinkIndex := AMsgID; // only for messages

  MakeLink;

  if (not IsAbstract) and (not Assigned(FCode)) then
  begin
    if (Owner as TSepiType).Native then
      FindNativeCode
    else
      FCode := @FCodeJumper;
  end;
end;

{*
  Cr�e une nouvelle meta-m�thode surcharg�e
  @param AOwner       Propri�taire de la m�thode
  @param AName        Nom de la m�thode
  @param ACode        Pointeur sur le code de la m�thode
  @param ASignature   Signature Delphi de la m�thode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la m�thode est abstraite
  @param AMsgID       Pour les m�thodes de message, le message intercept�
  @param ACallingConvention   Convention d'appel de la m�thode
*}
constructor TSepiMetaMethod.CreateOverloaded(AOwner : TSepiReflectionItem;
  const AName : string; ACode : Pointer; const ASignature : string;
  ALinkKind : TMethodLinkKind = mlkStatic; AAbstract : boolean = False;
  AMsgID : integer = 0; ACallingConvention : TCallingConvention = ccRegister);
begin
  FIsOverloaded := True;
  FOverloaded := AOwner.GetChild(AName) as TSepiMetaOverloadedMethod;

  if FOverloaded = nil then
    FOverloaded := TSepiMetaOverloadedMethod.Create(AOwner, AName);
  FOverloadIndex := FOverloaded.MethodCount;

  FOverloaded.FMethods.Add(Self);

  Create(AOwner, Format(OverloadedNameFormat, [AName, FOverloadIndex]),
    ACode, ASignature, ALinkKind, AAbstract, AMsgID, ACallingConvention);
end;

{*
  D�truit l'instance
*}
destructor TSepiMetaMethod.Destroy;
begin
  FSignature.Free;
  inherited Destroy;
end;

{*
  Met au point la liaison et l'index de liaison
  Par extension, recherche aussi la m�thode h�rit�e.
*}
procedure TSepiMetaMethod.MakeLink;
var OwningClass, Ancestor : TSepiClass;
    Meta : TSepiReflectionItem;
    I : integer;
begin
  FInherited := nil;

  // If owner is an interface, always an IMT index
  if Owner is TSepiInterface then
  begin
    FLinkKind := mlkInterface;
    FLinkIndex := TSepiInterface(Owner).FIMTSize;
    inc(TSepiInterface(Owner).FIMTSize, 4);
    exit;
  end;

  // If not a method, then nothing to do, but be sure link kind is static
  if not (Owner is TSepiClass) then
  begin
    FLinkKind := mlkStatic;
    FLinkIndex := 0;
    exit;
  end;

  OwningClass := TSepiClass(Owner);

  if OwningClass.Parent <> nil then
  begin
    if FLinkKind <> mlkMessage then
    begin
      // Looking for the inherited method
      Meta := OwningClass.Parent.LookForMember(RealName,
        OwningUnit, OwningClass);
      if Meta is TSepiMetaMethod then
      begin
        if Signature.CheckInherited(TSepiMetaMethod(Meta).Signature) then
          FInherited := TSepiMetaMethod(Meta);
      end else
      if Meta is TSepiMetaOverloadedMethod then
        FInherited := TSepiMetaOverloadedMethod(Meta).FindInherited(Signature);
    end else
    begin
      // Looking for an ancestor method which intercepts the same message ID
      Ancestor := OwningClass.Parent;
      while (FInherited = nil) and (Ancestor <> nil) do
      begin
        for I := 0 to Ancestor.ChildCount-1 do
        begin
          Meta := Ancestor.Children[I];
          if (Meta is TSepiMetaMethod) and
             (TSepiMetaMethod(Meta).LinkKind = mlkMessage) and
             (TSepiMetaMethod(Meta).MsgID = FLinkIndex) then
          begin
            FInherited := TSepiMetaMethod(Meta);
            Break;
          end;
        end;

        Ancestor := Ancestor.Parent;
      end;
    end;
  end;

  // Setting up link kind and index
  case FLinkKind of
    mlkStatic : FLinkIndex := 0;
    mlkVirtual :
    begin
      FLinkIndex := OwningClass.FVMTSize;
      inc(OwningClass.FVMTSize, 4);
    end;
    mlkDynamic :
    begin
      FLinkIndex := OwningClass.FDMTNextIndex;
      dec(OwningClass.FDMTNextIndex);
    end;
    mlkMessage : ; // nothing to do, FLinkIndex already set
    mlkOverride :
    begin
      FLinkKind := FInherited.LinkKind;
      FLinkIndex := FInherited.FLinkIndex;
    end;
  end;
end;

{*
  Cherche le code d'une m�thode native
*}
procedure TSepiMetaMethod.FindNativeCode;
begin
  case LinkKind of
    mlkVirtual : FCode := TSepiClass(Owner).VMTEntries[VMTOffset];
    mlkDynamic, mlkMessage :
    try
      FCode := GetClassDynamicCode(TSepiClass(Owner).DelphiClass, DMTIndex);
    except
      on Error : EAbstractError do;
    end;
  end;
end;

{*
  Nom r�el, tel que d�clar� (ind�pendant d'une surcharge ou non)
  @return Nom r�el
*}
function TSepiMetaMethod.GetRealName : string;
begin
  if IsOverloaded then
    Result := Overloaded.Name
  else
    Result := Name;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaMethod.ListReferences;
begin
  inherited;
  Signature.ListReferences;
  OwningUnit.AddRef(FOverloaded);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaMethod.Save(Stream : TStream);
var ALinkKind : TMethodLinkKind;
begin
  inherited;

  Signature.Save(Stream);
  SaveChildren(Stream);

  if FirstDeclaration then
    ALinkKind := FLinkKind
  else
    ALinkKind := mlkOverride;
  Stream.WriteBuffer(ALinkKind, 1);

  Stream.WriteBuffer(FAbstract, 1);
  Stream.WriteBuffer(FLinkIndex, 2); // only for messages

  Stream.WriteBuffer(FIsOverloaded, 1);
  if IsOverloaded then
  begin
    OwningUnit.WriteRef(Stream, FOverloaded);
    Stream.WriteBuffer(FOverloadIndex, 1);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaMethod.AfterConstruction;
begin
  inherited;
  if IsOverloaded then
    Overloaded.FMethods.Add(Self);
end;

{*
  Donne l'adresse de d�but du code de la m�thode
  Cette m�thode ne peut �tre appel�e qu'une seule fois par m�thode, et
  seulement pour les m�thodes non natives.
  @param ACode   Adresse de code
*}
procedure TSepiMetaMethod.SetCode(ACode : Pointer);
begin
  Assert(FCode = @FCodeJumper);
  Assert(FCodeJumper.OpCode = 0);
  MakeJmp(FCodeJumper, ACode);
end;

{*
  Donne l'adresse de d�but du code de la m�thode, � partir d'une m�thode
  Cette m�thode ne peut �tre appel�e qu'une seule fois par m�thode, et
  seulement pour les m�thodes non natives.
  @param AMethod   M�thode de code
*}
procedure TSepiMetaMethod.SetCodeMethod(const AMethod : TMethod);
var ACode : Pointer;
begin
  case Signature.CallingConvention of
    ccRegister :
      ACode := MakeProcOfRegisterMethod(AMethod, Signature.RegUsage);
    ccCDecl :
      ACode := MakeProcOfCDeclMethod(AMethod);
    ccPascal :
      ACode := MakeProcOfPascalMethod(AMethod);
    else
      ACode := MakeProcOfStdCallMethod(AMethod);
  end;

  AddPtrResource(ACode);
  SetCode(ACode);
end;

{----------------------------------}
{ Classe TSepiMetaOverloadedMethod }
{----------------------------------}

{*
  Charge une m�thode surcharg�e depuis un flux
*}
constructor TSepiMetaOverloadedMethod.Load(AOwner : TSepiReflectionItem;
  Stream : TStream);
begin
  inherited;
  FMethods := TObjectList.Create(False);
end;

{*
  Cr�e une nouvelle m�thode surcharg�e
  @param AOwner   Propri�taire de la m�thode
  @param AName    Nom de la m�thode
*}
constructor TSepiMetaOverloadedMethod.Create(AOwner : TSepiReflectionItem;
  const AName : string);
begin
  inherited Create(AOwner, AName);
  FMethods := TObjectList.Create(False);
end;

{*
  [@inheritDoc]
*}
destructor TSepiMetaOverloadedMethod.Destroy;
begin
  FMethods.Free;
  inherited;
end;

{*
  Trouve la m�thode effective dont une signature donn�e h�rite
  @param ASignature   Signature � rechercher
  @return M�thode effective correspondante
*}
function TSepiMetaOverloadedMethod.FindInherited(
  ASignature : TSepiMethodSignature) : TSepiMetaMethod;
var I : integer;
begin
  for I := 0 to MethodCount-1 do
  begin
    Result := Methods[I];
    if ASignature.CheckInherited(Result.Signature) then exit;
  end;
  Result := nil;
end;

{*
  Nombre de m�thodes de m�me nom
  @return Nombre de m�thodes de m�me nom
*}
function TSepiMetaOverloadedMethod.GetMethodCount : integer;
begin
  Result := FMethods.Count;
end;

{*
  Tableau zero-based des m�thodes de m�me nom
  @param Index   Index de la m�thode
  @return M�thode de m�me nom � l'index sp�cifi�
*}
function TSepiMetaOverloadedMethod.GetMethods(
  Index : integer) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod(FMethods[Index]);
end;

{*
  Trouve la m�thode effective qui correspond � une signature donn�e
  @param ASignature   Signature � rechercher
  @return M�thode effective correspondante
*}
function TSepiMetaOverloadedMethod.FindMethod(
  ASignature : TSepiMethodSignature) : TSepiMetaMethod;
var I : integer;
begin
  for I := 0 to MethodCount-1 do
  begin
    Result := Methods[I];
    if Result.Signature.Equals(ASignature) then exit;
  end;
  Result := nil;
end;

{*
  Trouve la m�thode effective qui correspond � une liste de types de param�tres
  @param ATypes   Liste des types de param�tres
  @return M�thode effective correspondante
*}
function TSepiMetaOverloadedMethod.FindMethod(
  const ATypes : array of TSepiType) : TSepiMetaMethod;
var I : integer;
begin
  for I := 0 to MethodCount-1 do
  begin
    Result := Methods[I];
    if Result.Signature.CompatibleWith(ATypes) then exit;
  end;
  Result := nil;
end;

{--------------------------}
{ Classe TSepiMetaProperty }
{--------------------------}

{*
  Charge une propri�t� depuis un flux
*}
constructor TSepiMetaProperty.Load(AOwner : TSepiReflectionItem;
  Stream : TStream);
begin
  inherited;

  FSignature := TSepiMethodSignature.Load(Self, Stream);
  LoadChildren(Stream);

  OwningUnit.ReadRef(Stream, FReadAccess.Meta);
  MakePropertyAccessKind(FReadAccess);

  OwningUnit.ReadRef(Stream, FWriteAccess.Meta);
  MakePropertyAccessKind(FWriteAccess);

  Stream.ReadBuffer(FIndex, 4);
  Stream.ReadBuffer(FDefaultValue, 4);
  Stream.ReadBuffer(FIsDefault, 1);
end;

{*
  Cr�e une nouvelle propri�t�
  @param AOwner          Propri�taire de la propri�t�
  @param AName           Nom de la propri�t�
  @param ASignature      Signature
  @param AReadAccess     Acc�s en lecture � la propri�t� (peut �tre vide)
  @param AWriteAccess    Acc�s en �criture � la propri�t� (peut �tre vide)
  @param AIndex          Index d'acc�s
  @param ADefaultValue   Valeur par d�faut de la propri�t�
  @param AStorage        Sp�cificateur de stockage
  @param AIsDefault      Indique si c'est la propri�t� tableau par d�faut
*}
constructor TSepiMetaProperty.Create(AOwner : TSepiReflectionItem;
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIndex : integer = NoIndex; ADefaultValue : integer = NoDefaultValue;
  const AStorage : string = ''; AIsDefault : boolean = False);

  function FindAccess(const AAccess : string) : TSepiReflectionItem;
  begin
    if Owner is TSepiClass then
      Result := TSepiClass(Owner).LookForMember(
        AAccess, OwningUnit, TSepiClass(Owner))
    else
      Result := TSepiInterface(Owner).LookForMember(AAccess);
  end;

begin
  inherited Create(AOwner, AName);

  FSignature := TSepiMethodSignature.Create(Self, ASignature);

  FReadAccess.Meta := FindAccess(AReadAccess);
  MakePropertyAccessKind(FReadAccess);

  FWriteAccess.Meta := FindAccess(AWriteAccess);
  MakePropertyAccessKind(FWriteAccess);

  FIndex := AIndex;

  FDefaultValue := ADefaultValue;
  MakePropertyStorage(FStorage, AStorage, TSepiClass(Owner));

  FIsDefault := AIsDefault and (Signature.ParamCount > 0);
end;

{*
  Cr�e une nouvelle propri�t�
  @param AOwner         Propri�taire de la propri�t�
  @param AName          Nom de la propri�t�
  @param ASignature     Signature
  @param AReadAccess    Acc�s en lecture � la propri�t� (peut �tre vide)
  @param AWriteAccess   Acc�s en �criture � la propri�t� (peut �tre vide)
  @param AIsDefault     Indique si c'est la propri�t� tableau par d�faut
*}
constructor TSepiMetaProperty.Create(AOwner : TSepiReflectionItem;
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIsDefault : boolean);
begin
  Create(AOwner, AName, ASignature, AReadAccess, AWriteAccess,
    NoIndex, NoDefaultValue, '', AIsDefault);
end;

{*
  Red�finit une propri�t� h�rit�e
  @param AOwner         Propri�taire de la propri�t�
  @param AName          Nom de la propri�t�
  @param AReadAccess    Acc�s en lecture � la propri�t�
  @param AWriteAccess   Acc�s en �criture � la propri�t�
  @param AStorage       Sp�cificateur de stockage
*}
constructor TSepiMetaProperty.Redefine(AOwner : TSepiReflectionItem;
  const AName : string; const AReadAccess : string = '';
  const AWriteAccess : string = ''; const AStorage : string = '');
var Previous : TSepiMetaProperty;
begin
  Previous := (AOwner as TSepiClass).Parent.LookForMember(
    AName, AOwner.OwningUnit, TSepiClass(AOwner)) as TSepiMetaProperty;

  inherited Create(AOwner, AName);

  FSignature := Previous.Signature;
  FIndex := Previous.Index;
  FDefaultValue := Previous.FDefaultValue;
  FIsDefault := Previous.IsDefault;

  if AReadAccess = '' then FReadAccess := Previous.ReadAccess else
  begin
    FReadAccess.Meta := TSepiClass(Owner).LookForMember(
      AReadAccess, OwningUnit, TSepiClass(Owner));
    MakePropertyAccessKind(FReadAccess);
  end;

  if AWriteAccess = '' then FWriteAccess := Previous.WriteAccess else
  begin
    FWriteAccess.Meta := TSepiClass(Owner).LookForMember(
      AWriteAccess, OwningUnit, TSepiClass(Owner));
    MakePropertyAccessKind(FWriteAccess);
  end;

  if AStorage = '' then FStorage := Previous.Storage else
    MakePropertyStorage(FStorage, AStorage, TSepiClass(Owner));
end;

{*
  Red�finit une propri�t� h�rit�e
  @param AOwner          Propri�taire de la propri�t�
  @param AName           Nom de la propri�t�
  @param AReadAccess     Acc�s en lecture � la propri�t� (peut �tre vide)
  @param AWriteAccess    Acc�s en �criture � la propri�t� (peut �tre vide)
  @param ADefaultValue   Valeur par d�faut de la propri�t�
  @param AStorage        Sp�cificateur de stockage
*}
constructor TSepiMetaProperty.Redefine(AOwner : TSepiReflectionItem;
  const AName, AReadAccess, AWriteAccess : string; ADefaultValue : integer;
  const AStorage : string = '');
begin
  Redefine(AOwner, AName, AReadAccess, AWriteAccess, AStorage);

  FDefaultValue := ADefaultValue;
end;

{*
  D�truit l'instance
*}
destructor TSepiMetaProperty.Destroy;
begin
  FSignature.Free;
  inherited;
end;

{*
  Construit les informations de propri�t�s pour les RTTI
  Cette m�thode ne doit �tre appel�e que pour des propri�t�s de classe, pas pour
  des propri�t�s d'interface.
  @param PropInfo   Destination des informations
*}
procedure TSepiMetaProperty.MakePropInfo(PropInfo : PPropInfo);
var ShortName : ShortString;
begin
  // Property type RTTI
  PropInfo.PropType := TSepiMetaClass(PropType).TypeInfoRef;

  // Read access
  with PropInfo^, ReadAccess do
  begin
    case Kind of
      pakNone : GetProc := nil;
      pakField : GetProc := Pointer($FF000000 or Field.Offset);
      pakMethod :
      begin
        if Method.LinkKind = mlkStatic then
          GetProc := Method.Code
        else
          GetProc := Pointer($FE000000 or Method.VMTOffset);
      end;
    end;
  end;

  // Write access
  with PropInfo^, WriteAccess do
  begin
    case Kind of
      pakNone : SetProc := nil;
      pakField : SetProc := Pointer($FF000000 or Field.Offset);
      pakMethod :
      begin
        if Method.LinkKind = mlkStatic then
          SetProc := Method.Code
        else
          SetProc := Pointer($FE000000 or Method.VMTOffset);
      end;
    end;
  end;

  // Storage
  with PropInfo^, Storage do
  begin
    case Kind of
      pskConstant : StoredProc := Pointer($FFFFFF00 or LongWord(Stored));
      pskField : StoredProc := Pointer($FF000000 or Field.Offset);
      pskMethod :
      begin
        if Method.LinkKind = mlkStatic then
          StoredProc := Method.Code
        else
          StoredProc := Pointer($FE000000 or Method.VMTOffset);
      end;
    end;
  end;

  // Some information
  PropInfo.Index := Index;
  PropInfo.Default := DefaultValue;
  PropInfo.NameIndex := 0; // Unknown, give 0 and pray...

  // Property name
  ShortName := Name;
  Move(ShortName[0], PropInfo.Name[0], Length(ShortName)+1);
end;

{*
  Type de la propri�t�
  @return Type de la propri�t�
*}
function TSepiMetaProperty.GetPropType : TSepiType;
begin
  Result := Signature.ReturnType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaProperty.ListReferences;
begin
  inherited;
  Signature.ListReferences;
  OwningUnit.AddRef(FReadAccess.Meta);
  OwningUnit.AddRef(FWriteAccess.Meta);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaProperty.Save(Stream : TStream);
begin
  inherited;

  Signature.Save(Stream);
  SaveChildren(Stream);

  OwningUnit.WriteRef(Stream, FReadAccess.Meta);
  OwningUnit.WriteRef(Stream, FWriteAccess.Meta);

  Stream.WriteBuffer(FIndex, 4);
  Stream.WriteBuffer(FDefaultValue, 4);
  Stream.WriteBuffer(FIsDefault, 1);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaProperty.Destroying;
begin
  inherited;

  if FSignature.Owner <> Self then
    FSignature := nil;
end;

{------------------------}
{ Classe TSepiRecordType }
{------------------------}

{*
  Charge un type record depuis un flux
*}
constructor TSepiRecordType.Load(AOwner : TSepiReflectionItem;
  Stream : TStream);
begin
  inherited;

  Stream.ReadBuffer(FPacked, 1);
  FAlignment := 1;
  FCompleted := False;

  LoadChildren(Stream);

  Complete;
end;

{*
  Cr�e un nouveau type record
  @param AOwner   Propri�taire du type
  @param AName    Nom du type
*}
constructor TSepiRecordType.Create(AOwner : TSepiReflectionItem;
  const AName : string; APacked : boolean = False; AIsNative : boolean = False;
  ATypeInfo : PTypeInfo = nil);
begin
  inherited Create(AOwner, AName, tkRecord);

  FPacked := APacked;
  FAlignment := 1;
  FCompleted := False;

  if AIsNative then
    ForceNative(ATypeInfo);
end;

{*
  Ajoute un champ au record
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param After       Champ pr�c�dent en m�moire
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.PrivAddField(const FieldName : string;
  FieldType : TSepiType; After : TSepiMetaField;
  ForcePack : boolean = False) : TSepiMetaField;
var Offset : integer;
begin
  if After = nil then Offset := 0 else
    Offset := After.Offset + After.FieldType.Size;
  if (not IsPacked) and (not ForcePack) then
    FieldType.AlignOffset(Offset);

  Result := TSepiMetaField.Create(Self, FieldName, FieldType, Offset);
end;

{*
  Construit les RTTI, si besoin
*}
procedure TSepiRecordType.MakeTypeInfo;
var Fields : TObjectList;
    I : integer;
    FieldTable : PInitTable;
begin
  if not NeedInit then exit;

  Fields := TObjectList.Create(False);
  try
    // Listings the fields which need initialization
    for I := 0 to ChildCount-1 do
      if TSepiMetaField(Children[I]).FieldType.NeedInit then
        Fields.Add(Children[I]);

    // Creating the RTTI
    AllocateTypeInfo(
      RecordTypeDataLengthBase + Fields.Count*sizeof(TInitInfo));
    FieldTable := PInitTable(TypeData);

    // Basic information
    FieldTable.Size := FSize;
    FieldTable.Count := Fields.Count;

    // Field information
    for I := 0 to Fields.Count-1 do
    begin
      with TSepiMetaField(Fields[I]) do
      begin
        FieldTable.Fields[I].TypeInfo :=
          TSepiRecordType(FieldType).TypeInfoRef;
        FieldTable.Fields[I].Offset := Offset;
      end;
    end;
  finally
    Fields.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRecordType.ChildAdded(Child : TSepiReflectionItem);
begin
  inherited;

  if Child is TSepiMetaField then with TSepiMetaField(Child) do
  begin
    if FieldType.NeedInit then
      FNeedInit := True;
    if Offset + FieldType.Size > FSize then
      FSize := Offset + FieldType.Size;
    if FieldType.Alignment > FAlignment then
      FAlignment := FieldType.Alignment;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRecordType.Save(Stream : TStream);
begin
  inherited;
  Stream.WriteBuffer(FPacked, 1);
  SaveChildren(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.GetAlignment : integer;
begin
  Result := FAlignment;
end;

{*
  Ajoute un champ au record
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldType : TSepiType; ForcePack : boolean = False) : TSepiMetaField;
var LastField : TSepiMetaField;
begin
  if ChildCount = 0 then LastField := nil else
    LastField := TSepiMetaField(Children[ChildCount-1]);

  Result := PrivAddField(FieldName, FieldType, LastField, ForcePack);
end;

{*
  Ajoute un champ au record
  @param FieldName       Nom du champ
  @param FieldTypeInto   RTTI du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldTypeInfo : PTypeInfo; ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeInfo), ForcePack);
end;

{*
  Ajoute un champ au record
  @param FieldName       Nom du champ
  @param FieldTypeName   Nom du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.AddField(const FieldName, FieldTypeName : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeName), ForcePack);
end;

{*
  Ajoute un champ au record apr�s un champ donn� en m�moire
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param After       Nom du champ pr�c�dent en m�moire (vide pour le d�but)
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.AddFieldAfter(const FieldName : string;
  FieldType : TSepiType; const After : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := PrivAddField(FieldName, FieldType,
    TSepiMetaField(FindChild(After)), ForcePack);
end;

{*
  Ajoute un champ au record apr�s un champ donn� en m�moire
  @param FieldName       Nom du champ
  @param FieldTypeInfo   RTTI du type du champ
  @param After           Nom du champ pr�c�dent en m�moire (vide pour le d�but)
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.AddFieldAfter(const FieldName : string;
  FieldTypeInfo : PTypeInfo; const After : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := PrivAddField(FieldName, Root.FindType(FieldTypeInfo),
    TSepiMetaField(FindChild(After)), ForcePack);
end;

{*
  Ajoute un champ au record apr�s un champ donn� en m�moire
  @param FieldName       Nom du champ
  @param FieldTypeName   Nom du type du champ
  @param After           Nom du champ pr�c�dent en m�moire (vide pour le d�but)
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.AddFieldAfter(
  const FieldName, FieldTypeName, After : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := PrivAddField(FieldName, Root.FindType(FieldTypeName),
    TSepiMetaField(FindChild(After)), ForcePack);
end;

{*
  Termine le record et construit ses RTTI si ce n'est pas d�j� fait
*}
procedure TSepiRecordType.Complete;
begin
  if FCompleted then exit;

  FCompleted := True;
  if not IsPacked then
    AlignOffset(FSize);
  if Size > 4 then
    FParamBehavior.AlwaysByAddress := True;
  if TypeInfo = nil then
    MakeTypeInfo;
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := Self = AType;
end;

{-----------------------}
{ Classe TSepiInterface }
{-----------------------}

{*
  Recense une interface native
*}
constructor TSepiInterface.RegisterTypeInfo(AOwner : TSepiReflectionItem;
  ATypeInfo : PTypeInfo);
var Flags : TIntfFlags;
begin
  inherited;

  FSize := 4;
  FNeedInit := True;

  if Assigned(TypeData.IntfParent) then
  begin
    FParent := TSepiInterface(Root.FindType(TypeData.IntfParent^));
    FIMTSize := Parent.IMTSize;
  end else
  begin
    // This is IInterface
    FParent := nil;
    FIMTSize := 0;
  end;
  FCompleted := False;

  Flags := TypeData.IntfFlags;
  FHasGUID := ifHasGuid in Flags;
  FIsDispInterface := ifDispInterface in Flags;
  FIsDispatch := ifDispatch in Flags;

  if not FHasGUID then FGUID := NoGUID else
    FGUID := TypeData.Guid;
end;

{*
  Charge une interface depuis un flux
*}
constructor TSepiInterface.Load(AOwner : TSepiReflectionItem; Stream : TStream);
begin
  inherited;

  FSize := 4;
  FNeedInit := True;

  OwningUnit.ReadRef(Stream, FParent);
  FCompleted := False;

  Stream.ReadBuffer(FHasGUID, 1);
  Stream.ReadBuffer(FIsDispInterface, 1);
  Stream.ReadBuffer(FIsDispatch, 1);
  Stream.ReadBuffer(FGUID, sizeof(TGUID));

  FIMTSize := Parent.IMTSize;

  LoadChildren(Stream);

  Complete;
end;

{*
  Cr�e une nouvelle interface
  @param AOwner    Propri�taire du type
  @param AName     Nom du type
  @param AParent   Classe parent
*}
constructor TSepiInterface.Create(AOwner : TSepiReflectionItem;
  const AName : string; AParent : TSepiInterface; const AGUID : TGUID;
  AIsDispInterface : boolean = False);
begin
  inherited Create(AOwner, AName, tkInterface);

  FSize := 4;
  FNeedInit := True;

  if Assigned(AParent) then FParent := AParent else
    FParent := TSepiInterface(Root.FindType(System.TypeInfo(IInterface)));
  FCompleted := False;

  FHasGUID := not IsNoGUID(AGUID);
  FIsDispInterface := AIsDispInterface;
  FIsDispatch := IntfInheritsFrom(
    TSepiInterface(Root.FindType(System.TypeInfo(IDispatch))));
  FGUID := AGUID;

  FIMTSize := Parent.IMTSize;
end;

{*
  Construit les RTTI
*}
procedure TSepiInterface.MakeTypeInfo;
var Flags : TIntfFlags;
    OwningUnitName : ShortString;
    Count : PWord;
begin
  // Creating the RTTI
  AllocateTypeInfo(IntfTypeDataLengthBase + Length(OwningUnit.Name) + 1);
  TypeData.IntfParent := FParent.TypeInfoRef;

  // Interface flags
  Flags := [];
  if FHasGUID then Include(Flags, ifHasGuid);
  if FIsDispInterface then Include(Flags, ifDispInterface);
  if FIsDispatch then Include(Flags, ifDispatch);
  TypeData.IntfFlags := Flags;

  // GUID
  TypeData.Guid := FGUID;

  // Owning unit name
  OwningUnitName := OwningUnit.Name;
  Move(OwningUnitName[0], TypeData.IntfUnit[0], Length(OwningUnitName)+1);

  // Method count in the interface
  Count := SkipPackedShortString(@TypeData.IntfUnit);
  Count^ := ChildCount;
  inc(Integer(Count), 2);
  Count^ := $FFFF; // no more information available
end;

{*
  [@inheritDoc]
*}
procedure TSepiInterface.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FParent);
end;

{*
  [@inheritDoc]
*}
procedure TSepiInterface.Save(Stream : TStream);
begin
  inherited;

  OwningUnit.WriteRef(Stream, FParent);

  Stream.WriteBuffer(FHasGUID, 1);
  Stream.WriteBuffer(FIsDispInterface, 1);
  Stream.WriteBuffer(FIsDispatch, 1);
  Stream.WriteBuffer(FGUID, sizeof(TGUID));

  SaveChildren(Stream);
end;

{*
  Cr�e une nouvelle instance de TSepiInterface
  @return Instance cr��e
*}
class function TSepiInterface.NewInstance : TObject;
begin
  Result := inherited NewInstance;
  TSepiInterface(Result).FSize := 4;
  TSepiInterface(Result).FNeedInit := True;
end;

{*
  D�clare un type interface en forward
  @param AOwner      Propri�taire du type
  @param ATypeName   RTTI de l'interface
*}
class function TSepiInterface.ForwardDecl(AOwner : TSepiReflectionItem;
  ATypeInfo : PTypeInfo) : TSepiInterface;
begin
  Result := TSepiInterface(NewInstance);
  TSepiInterface(Result).TypeInfoRef^ := ATypeInfo;
  TSepiInterface(AOwner).AddForward(ATypeInfo.Name, Result);
end;

{*
  D�clare un type interface en forward
  @param AOwner   Propri�taire du type
  @param AName    Nom de l'interface
*}
class function TSepiInterface.ForwardDecl(AOwner : TSepiReflectionItem;
  const AName : string) : TSepiInterface;
begin
  Result := TSepiInterface(NewInstance);
  TSepiInterface(AOwner).AddForward(AName, Result);
end;

{*
  Ajoute une m�thode � l'interface
  @param MethodName           Nom de la m�thode
  @param ASignature           Signature Delphi de la m�thode
  @param ACallingConvention   Convention d'appel de la m�thode
*}
function TSepiInterface.AddMethod(const MethodName, ASignature : string;
  ACallingConvention : TCallingConvention = ccRegister) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod.Create(Self, MethodName, nil, ASignature,
    mlkInterface, False, 0, ACallingConvention);
end;

{*
  Ajoute une propri�t� � l'interface
  @param AName           Nom de la propri�t�
  @param ASignature      Signature
  @param AReadAccess     Acc�s en lecture � la propri�t� (peut �tre vide)
  @param AWriteAccess    Acc�s en �criture � la propri�t� (peut �tre vide)
  @param AIndex          Index d'acc�s
  @param AIsDefault      Indique si c'est la propri�t� tableau par d�faut
*}
function TSepiInterface.AddProperty(
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIndex : integer = NoIndex; AIsDefault : boolean = False) : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIndex, NoDefaultValue, '', AIsDefault);
end;

{*
  Ajoute une propri�t� � l'interface
  @param AName           Nom de la propri�t�
  @param ASignature      Signature
  @param AReadAccess     Acc�s en lecture � la propri�t� (peut �tre vide)
  @param AWriteAccess    Acc�s en �criture � la propri�t� (peut �tre vide)
  @param AIsDefault      Indique si c'est la propri�t� tableau par d�faut
*}
function TSepiInterface.AddProperty(
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIsDefault : boolean) : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIsDefault);
end;

{*
  Termine l'interface et construit ses RTTI si ce n'est pas d�j� fait
*}
procedure TSepiInterface.Complete;
begin
  if FCompleted then exit;

  FCompleted := True;
  if not Native then
    MakeTypeInfo;
end;

{*
  D�termine si l'interface h�rite d'une interface donn�e
  @param AParent   Anc�tre � tester
  @return True si l'interface h�rite de AParent, False sinon
*}
function TSepiInterface.IntfInheritsFrom(AParent : TSepiInterface) : boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FParent) and FParent.IntfInheritsFrom(AParent));
end;

{*
  Recherche un membre dans l'interface
  @param MemberName   Nom du membre recherch�
  @return Le membre correspondant, ou nil si non trouv�
*}
function TSepiInterface.LookForMember(
  const MemberName : string) : TSepiReflectionItem;
begin
  if MemberName = '' then
  begin
    Result := nil;
    exit;
  end;

  Result := GetChild(MemberName);
  if (Result = nil) and (Parent <> nil) then
    Result := Parent.LookForMember(MemberName);
end;

{-------------------}
{ Classe TSepiClass }
{-------------------}

{*
  Recense une classe native
*}
constructor TSepiClass.RegisterTypeInfo(AOwner : TSepiReflectionItem;
  ATypeInfo : PTypeInfo);
begin
  inherited;

  FSize := 4;
  FDelphiClass := TypeData.ClassType;
  if Assigned(TypeData.ParentInfo) then
  begin
    FParent := TSepiClass(Root.FindType(TypeData.ParentInfo^));
    FInstSize := Parent.InstSize;
    FVMTSize := Parent.VMTSize;
    FDMTNextIndex := Parent.FDMTNextIndex;
  end else
  begin
    // This is TObject
    FParent := nil;
    FInstSize := 4; // pointer to VMT
    FVMTSize := vmtMinMethodIndex;
    FDMTNextIndex := -1;
  end;
  FCompleted := False;

  FCurrentVisibility := mvPublic;
end;

{*
  Charge une classe depuis un flux
*}
constructor TSepiClass.Load(AOwner : TSepiReflectionItem; Stream : TStream);
var IntfCount, I : integer;
begin
  inherited;

  FSize := 4;
  FDelphiClass := nil;
  OwningUnit.ReadRef(Stream, FParent);
  FCompleted := False;

  Stream.ReadBuffer(IntfCount, 4);
  SetLength(FInterfaces, IntfCount);
  FillChar(FInterfaces[0], IntfCount*sizeof(TSepiInterfaceEntry), 0);
  for I := 0 to IntfCount-1 do
    OwningUnit.ReadRef(Stream, FInterfaces[I].IntfRef);

  FInstSize := Parent.InstSize;
  FVMTSize := Parent.VMTSize;
  FDMTNextIndex := Parent.FDMTNextIndex;

  FCurrentVisibility := mvPublic;

  LoadChildren(Stream);

  Complete;
end;

{*
  Cr�e une nouvelle classe
  @param AOwner    Propri�taire du type
  @param AName     Nom du type
  @param AParent   Classe parent
*}
constructor TSepiClass.Create(AOwner : TSepiReflectionItem;
  const AName : string; AParent : TSepiClass);
begin
  inherited Create(AOwner, AName, tkClass);

  FSize := 4;
  FDelphiClass := nil;
  if Assigned(AParent) then FParent := AParent else
    FParent := TSepiClass(Root.FindType(System.TypeInfo(TObject)));
  FCompleted := False;

  FInstSize := Parent.InstSize;
  FVMTSize := Parent.VMTSize;
  FDMTNextIndex := Parent.FDMTNextIndex;

  FCurrentVisibility := mvPublic;
end;

{*
  D�truit l'instance
*}
destructor TSepiClass.Destroy;
const
  Tables : array[0..4] of integer = (
    vmtDynamicTable, vmtMethodTable, vmtFieldTable, vmtInitTable, vmtIntfTable
  );
var I : integer;
    PTable : Pointer;
begin
  if (not Native) and (FDelphiClass <> nil) then
  begin
    // Destroying the IMTs
    for I := 0 to High(FInterfaces) do with FInterfaces[I] do
    begin
      if Assigned(Relocates) then
        FreeMem(Relocates);
      if Assigned(IMT) then
        FreeMem(IMT);
    end;

    // Destroying the tables
    for I := Low(Tables) to High(Tables) do
    begin
      PTable := VMTEntries[Tables[I]];
      if Assigned(PTable) then
        FreeMem(PTable);
    end;

    // Destroying the VMT
    PTable := Pointer(Integer(FDelphiClass) + vmtMinIndex);
    FreeMem(PTable, FVMTSize);
  end;

  inherited;
end;

{*
  Construit une IMT non native
  @param Index   Index de l'IMT � construire (dans le tableau FInterfaces)
*}
procedure TSepiClass.MakeIMT(IntfEntry : PSepiInterfaceEntry);
const
  AdjustInstrSizes : array[boolean, boolean] of ShortInt = (
    (5, 8), (3, 5)
  );
  JumpInstrSize = 5;
var Offset : LongInt;
    BigOffset : boolean;
    Methods : TObjectList;
    Method, RealMethod : TSepiMetaMethod;
    Intf : TSepiInterface;
    I, RelocLength, AdjustInstrSize : integer;
    RelocEntry, IMTEntry : integer;
begin
  Offset := IntfEntry.Offset;
  BigOffset := Offset >= $80;
  Offset := -Offset;

  Methods := TObjectList.Create(False);
  try
    // Computing the relocates thunks length and listing the methods
    Intf := IntfEntry.IntfRef;
    RelocLength := 0;

    while Intf <> nil do
    begin
      for I := Intf.ChildCount-1 downto 0 do
      begin
        if not (Intf.Children[I] is TSepiMetaMethod) then Continue;
        Method := TSepiMetaMethod(Intf.Children[I]);

        AdjustInstrSize := AdjustInstrSizes[
          Method.Signature.CallingConvention = ccRegister, BigOffset];

        inc(RelocLength, AdjustInstrSize);
        inc(RelocLength, JumpInstrSize);

        Methods.Insert(0, Method);
      end;

      Intf := Intf.Parent;
    end;

    // Creating the relocates thunks and the IMT
    GetMem(IntfEntry.Relocates, RelocLength);
    RelocEntry := integer(IntfEntry.Relocates);
    GetMem(IntfEntry.IMT, IntfEntry.IntfRef.IMTSize);
    IMTEntry := integer(IntfEntry.IMT);

    // Filling the relocates thunks and the IMT
    for I := 0 to Methods.Count-1 do
    begin
      Method := TSepiMetaMethod(Methods[I]);

      // IMT entry
      PLongInt(IMTEntry)^ := RelocEntry;
      inc(IMTEntry, 4);

      // Adjust instruction
      if Method.Signature.CallingConvention = ccRegister then
      begin
        // add eax, Offset
        if BigOffset then
        begin
          // 05 xx xx xx xx
          PByte(RelocEntry)^ := $05;
          PLongInt(RelocEntry+1)^ := Offset;
          inc(RelocEntry, 5);
        end else
        begin
          // 83 C0 xx
          PWord(RelocEntry)^ := $C083;
          PShortInt(RelocEntry+2)^ := Offset;
          inc(RelocEntry, 3);
        end;
      end else
      begin
        // add dwortd ptr [esp+4], Offset
        if BigOffset then
        begin
          // 81 44 24 04 xx xx xx xx
          PLongWord(RelocEntry)^ := $04244481;
          PLongInt(RelocEntry+4)^ := Offset;
          inc(RelocEntry, 8);
        end else
        begin
          // 83 44 24 04 xx
          PLongWord(RelocEntry)^ := $04244483;
          PShortInt(RelocEntry+4)^ := Offset;
          inc(RelocEntry, 5);
        end;
      end;

      // JumpInstruction
      RealMethod :=
        LookforMember(Method.Name, OwningUnit, Self) as TSepiMetaMethod;

      // jmp Method.Code   E9 xx xx xx xx
      PByte(RelocEntry)^ := $E9;
      PLongInt(RelocEntry+1)^ := integer(RealMethod.Code) - RelocEntry - 5;
      inc(RelocEntry, 5);
    end;
  finally
    Methods.Free;
  end;
end;

{*
  Construit les IMTs
*}
procedure TSepiClass.MakeIMTs;
var IntfCount, I : integer;
    IntfTable : PInterfaceTable;
begin
  // If no interface supported, then exit
  IntfCount := InterfaceCount;
  if IntfCount = 0 then exit;

  // Fetch native interface table
  if Native then
    IntfTable := DelphiClass.GetInterfaceTable
  else
    IntfTable := nil;

  for I := 0 to IntfCount-1 do
  begin
    FInterfaces[I].Offset := FInstSize;
    inc(FInstSize, 4);

    // If native, get information from IntfTable; otherwise, create the IMT
    if Native then
    begin
      FInterfaces[I].IMT := IntfTable.Entries[I].VTable;
      FInterfaces[I].Offset := IntfTable.Entries[I].IOffset;
    end else MakeIMT(@FInterfaces[I]);
  end;
end;

{*
  Construit les RTTI
*}
procedure TSepiClass.MakeTypeInfo;
var OwningUnitName : ShortString;
    TypeDataLength, I : integer;
    Props : TObjectList;
    Prop : TSepiMetaProperty;
    PropCount : PWord;
    PropInfo : PPropInfo;
begin
  OwningUnitName := OwningUnit.Name;
  Props := TObjectList.Create(False);
  try
    TypeDataLength := ClassTypeDataLengthBase;
    inc(TypeDataLength, Length(OwningUnitName));
    inc(TypeDataLength);

    // Listing the published properties, and computing the type data length
    for I := 0 to ChildCount-1 do if Children[I] is TSepiMetaProperty then
    begin
      Prop := TSepiMetaProperty(Children[I]);
      if Prop.Visibility <> mvPublished then Continue;

      Props.Add(Prop);
      inc(TypeDataLength, PropInfoLengthBase);
      inc(TypeDataLength, Length(Prop.Name));
      inc(TypeDataLength);
    end;

    // Creating the RTTI
    AllocateTypeInfo(TypeDataLength);

    // Basic information
    TypeData.ClassType := DelphiClass;
    TypeData.ParentInfo := FParent.TypeInfoRef;
    TypeData.PropCount := Props.Count;
    Move(OwningUnitName[0], TypeData.UnitName[0], Length(OwningUnitName)+1);

    // Property count
    PropCount := SkipPackedShortString(@TypeData.UnitName);
    PropCount^ := Props.Count;

    // Property information
    PropInfo := PPropInfo(Integer(PropCount) + 2);
    for I := 0 to Props.Count-1 do
    begin
      TSepiMetaProperty(Props[I]).MakePropInfo(PropInfo);
      PropInfo := SkipPackedShortString(@PropInfo.Name);
    end;
  finally
    Props.Free;
  end;
end;

{*
  Construit la table des interfaces
  Range �galement l'adresse de cette table � l'emplacement pr�vu de la VMT.
*}
procedure TSepiClass.MakeIntfTable;
var IntfCount, I : integer;
    IntfTable : PInterfaceTable;
begin
  // If no interface supported, then exit
  IntfCount := InterfaceCount;
  if IntfCount = 0 then exit;

  // Creating the interface table
  GetMem(IntfTable, sizeof(integer) + IntfCount*sizeof(TInterfaceEntry));
  VMTEntries[vmtIntfTable] := IntfTable;

  // Basic information
  IntfTable.EntryCount := IntfCount;

  // Interface information
  for I := 0 to IntfCount-1 do with IntfTable.Entries[I], FInterfaces[I] do
  begin
    IID := IntfRef.GUID;
    VTable := IMT;
    IOffset := Offset;
    ImplGetter := 0;
  end;
end;

{*
  Construit la table d'initialisation
  Range �galement l'adresse de cette table � l'emplacement pr�vu de la VMT.
*}
procedure TSepiClass.MakeInitTable;
var Fields : TObjectList;
    I : integer;
    Meta : TSepiReflectionItem;
    InitTable : PInitTable;
begin
  Fields := TObjectList.Create(False);
  try
    // Listing the fields which need initialization
    for I := 0 to ChildCount-1 do
    begin
      Meta := Children[I];
      if (Meta is TSepiMetaField) and
         TSepiMetaField(Meta).FieldType.NeedInit then
        Fields.Add(Meta);
    end;

    // If no field to be finalized, then exit
    if Fields.Count = 0 then exit;

    // Creating the init table
    GetMem(InitTable, InitTableLengthBase + Fields.Count*sizeof(TInitInfo));
    VMTEntries[vmtInitTable] := InitTable;

    // Basic information
    PWord(InitTable)^ := 0;
    inc(Integer(InitTable), 2);
    InitTable.Size := 0;
    InitTable.Count := Fields.Count;

    // Field information
    for I := 0 to Fields.Count-1 do
    begin
      with TSepiMetaField(Fields[I]) do
      begin
        InitTable.Fields[I].TypeInfo :=
          TSepiRecordType(FieldType).TypeInfoRef;
        InitTable.Fields[I].Offset := Offset;
      end;
    end;
  finally
    Fields.Free;
  end;
end;

{*
  Construit la table des champs publi�s
  Range �galement l'adresse de cette table � l'emplacement pr�vu de la VMT.
*}
procedure TSepiClass.MakeFieldTable;
var Fields : TObjectList;
    I : integer;
    Meta : TSepiReflectionItem;
    FieldTable : PFieldTable;
    FieldInfo : PFieldInfo;
    ShortName : ShortString;
begin
  Fields := TObjectList.Create(False);
  try
    // Listing the published fields
    for I := 0 to ChildCount-1 do
    begin
      Meta := Children[I];
      if (Meta is TSepiMetaField) and (Meta.Visibility = mvPublished) then
        Fields.Add(Meta);
    end;

    // If no published field, then exit
    if Fields.Count = 0 then exit;

    // Creating the field table
    GetMem(FieldTable, FieldTableLengthBase + Fields.Count*sizeof(TFieldInfo));
    VMTEntries[vmtFieldTable] := FieldTable;

    // Basic information
    FieldTable.FieldCount := Fields.Count;
    FieldTable.Unknown := nil;

    // Field information
    FieldInfo := PFieldInfo(Integer(FieldTable) + sizeof(TFieldTable));
    for I := 0 to Fields.Count-1 do
    begin
      with TSepiMetaField(Fields[I]) do
      begin
        FieldInfo.Offset := Offset;
        FieldInfo.Index := I;
        ShortName := Name;
        Move(ShortName[0], FieldInfo.Name[0], Length(ShortName)+1);

        inc(Integer(FieldInfo), 7 + Length(ShortName));
      end;
    end;
  finally
    Fields.Free;
  end;
end;

{*
  Construit la table des m�thodes publi�es
  Range �galement l'adresse de cette table � l'emplacement pr�vu de la VMT.
*}
procedure TSepiClass.MakeMethodTable;
var Methods : TObjectList;
    I : integer;
    Meta : TSepiReflectionItem;
    MethodTable : PMethodTable;
    MethodInfo : PMethodInfo;
    ShortName : ShortString;
begin
  Methods := TObjectList.Create(False);
  try
    // Listing the published methods
    for I := 0 to ChildCount-1 do
    begin
      Meta := Children[I];
      if (Meta is TSepiMetaMethod) and (Meta.Visibility = mvPublished) then
        Methods.Add(Meta);
    end;

    // If no published method, then exit
    if Methods.Count = 0 then exit;

    // Creating the method table
    GetMem(MethodTable, MethodTableLengthBase +
      Methods.Count*sizeof(TMethodInfo));
    VMTEntries[vmtMethodTable] := MethodTable;

    // Basic information
    MethodTable.MethodCount := Methods.Count;

    // Field information
    MethodInfo := PMethodInfo(Integer(MethodTable) + sizeof(TMethodTable));
    for I := 0 to Methods.Count-1 do
    begin
      with TSepiMetaMethod(Methods[I]) do
      begin
        ShortName := Name;
        MethodInfo.Size := 7 + Length(ShortName);
        MethodInfo.Code := Code;
        Move(ShortName[0], MethodInfo.Name[0], Length(ShortName)+1);

        inc(Integer(MethodInfo), MethodInfo.Size);
      end;
    end;
  finally
    Methods.Free;
  end;
end;

{*
  Construit la DMT
  Range �galement l'adresse de la DMT � l'emplacement pr�vu de la VMT.
*}
procedure TSepiClass.MakeDMT;
var PDMT : Pointer;
    I, Count : integer;
    Meta : TSepiReflectionItem;
    IndexList, CodeList : integer;
    Methods : TObjectList;
begin
  Methods := TObjectList.Create(False);
  try
    // Listing dynamic methods
    for I := 0 to ChildCount-1 do
    begin
      Meta := Children[I];
      if (Meta is TSepiMetaMethod) and
         (TSepiMetaMethod(Meta).LinkKind in [mlkDynamic, mlkMessage]) and
         (not TSepiMetaMethod(Meta).IsAbstract) then
        Methods.Add(Meta);
    end;
    Count := Methods.Count;

    // Creating the DMT
    GetMem(PDMT, 2 + 6*Count);
    VMTEntries[vmtDynamicTable] := PDMT;
    PWord(PDMT)^ := Count;
    IndexList := Integer(PDMT) + 2;
    CodeList := IndexList + 2*Count;

    // Filling the DMT
    for I := 0 to Count-1 do with TSepiMetaMethod(Methods[I]) do
    begin
      PSmallInt(IndexList + 2*I)^ := DMTIndex; // alias MsgID
      PPointer(CodeList + 4*I)^ := Code;
    end;
  finally
    Methods.Free;
  end;
end;

{*
  Construit la VMT
*}
procedure TSepiClass.MakeVMT;
var PVMT : Pointer;
    AbstractErrorProcAddress : Pointer;
    I : integer;
    Method : TSepiMetaMethod;
begin
  // Creating the VMT
  GetMem(PVMT, FVMTSize - vmtMinIndex);
  FillChar(PVMT^, FVMTSize - vmtMinIndex, 0);
  dec(integer(PVMT), vmtMinIndex);
  FDelphiClass := TClass(PVMT);

  // Creating the RTTI
  MakeTypeInfo;

  // Setting class properties
  if FVMTSize > 0 then
    VMTEntries[vmtSelfPtr] := PVMT
  else
    VMTEntries[vmtSelfPtr] := @TypeInfo.Name;

  VMTEntries[vmtTypeInfo] := TypeInfo;
  VMTEntries[vmtClassName] := @TypeInfo.Name;
  VMTEntries[vmtInstanceSize] := Pointer(InstSize);
  VMTEntries[vmtParent] := @Parent.FDelphiClass;

  // Copy the parent VMT
  Move(Pointer(Integer(Parent.DelphiClass) + vmtMinMethodIndex)^,
    Pointer(Integer(PVMT) + vmtMinMethodIndex)^,
    Parent.VMTSize - vmtMinMethodIndex);

  // Setting the new method addresses
  AbstractErrorProcAddress := CompilerMagicRoutineAddress(@AbstractError);
  for I := 0 to ChildCount-1 do if Children[I] is TSepiMetaMethod then
  begin
    Method := TSepiMetaMethod(Children[I]);
    if Method.LinkKind = mlkVirtual then
    begin
      if Method.IsAbstract then
        VMTEntries[Method.VMTOffset] := AbstractErrorProcAddress
      else
        VMTEntries[Method.VMTOffset] := Method.Code;
    end;
  end;

  // Making the other tables
  MakeIntfTable;
  MakeInitTable;
  MakeFieldTable;
  MakeMethodTable;
  MakeDMT;
end;

{*
  Nombre d'interfaces support�es
  @return Nombre d'interfaces support�es
*}
function TSepiClass.GetInterfaceCount : integer;
begin
  Result := Length(FInterfaces);
end;

{*
  Tableau zero-based des interfaces support�es
  @param Index   Index dans le tableau
  @return Interface support�e � l'index sp�cifi�
*}
function TSepiClass.GetInterfaces(Index : integer) : TSepiInterface;
begin
  Result := FInterfaces[Index].IntfRef;
end;

{*
  VMT de la classe, index�e par les constantes vmtXXX
  @param Index   Index dans la VMT
  @return Information contenue dans la VMT � l'index sp�cifi�
*}
function TSepiClass.GetVMTEntries(Index : integer) : Pointer;
begin
  Result := PPointer(Integer(FDelphiClass) + Index)^;
end;

{*
  Modifie la VMT de la classe, index�e par les constantes vmtXXX
  Cette m�thode ne fonctionne que pour des VMT cr��es par Sepi. Dans le cas o�
  la VMT serait cr��e � la compilation, cette m�thode provoquera une violation
  d'acc�s.
  @param Index   Index dans la VMT
  @param Value   Information � stocker dans la VMT � l'index sp�cifi�
*}
procedure TSepiClass.SetVMTEntries(Index : integer; Value : Pointer);
begin
  PPointer(Integer(FDelphiClass) + Index)^ := Value;
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.ChildAdded(Child : TSepiReflectionItem);
begin
  inherited;

  if State = rsConstructing then
    TSepiClass(Child).FVisibility := FCurrentVisibility;

  if Child is TSepiMetaField then with TSepiMetaField(Child) do
    FInstSize := Offset + FieldType.Size;
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.ListReferences;
var I : integer;
begin
  inherited;
  OwningUnit.AddRef(FParent);
  for I := 0 to InterfaceCount-1 do
    OwningUnit.AddRef(Interfaces[I]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.Save(Stream : TStream);
var IntfCount, I : integer;
begin
  inherited;
  OwningUnit.WriteRef(Stream, FParent);

  IntfCount := InterfaceCount;
  Stream.WriteBuffer(IntfCount, 4);
  for I := 0 to IntfCount-1 do
    OwningUnit.WriteRef(Stream, Interfaces[I]);

  SaveChildren(Stream);
end;

{*
  Cr�e une nouvelle instance de TSepiClass
  @return Instance cr��e
*}
class function TSepiClass.NewInstance : TObject;
begin
  Result := inherited NewInstance;
  TSepiClass(Result).FSize := 4;
  TSepiClass(Result).FNeedInit := False;
end;

{*
  D�clare un type classe en forward
  @param AOwner      Propri�taire du type
  @param ATypeInfo   RTTI de la classe
*}
class function TSepiClass.ForwardDecl(AOwner : TSepiReflectionItem;
  ATypeInfo : PTypeInfo) : TSepiClass;
begin
  Result := TSepiClass(NewInstance);
  TSepiClass(Result).TypeInfoRef^ := ATypeInfo;
  TSepiClass(AOwner).AddForward(ATypeInfo.Name, Result);
end;

{*
  D�clare un type classe en forward
  @param AOwner   Propri�taire du type
  @param AName    Nom de la classe
*}
class function TSepiClass.ForwardDecl(AOwner : TSepiReflectionItem;
  const AName : string) : TSepiClass;
begin
  Result := TSepiClass(NewInstance);
  TSepiClass(AOwner).AddForward(AName, Result);
end;

{*
  Ajoute le support d'une interface
  @param AInterface   Interface � supporter
*}
procedure TSepiClass.AddInterface(AInterface : TSepiInterface);
var Index : integer;
begin
  Index := Length(FInterfaces);
  SetLength(FInterfaces, Index+1);

  with FInterfaces[Index] do
  begin
    IntfRef := AInterface;
    Relocates := nil;
    IMT := nil;
    Offset := 0;
  end;
end;

{*
  Ajoute le support d'une interface
  @param AIntfTypeInfo   RTTI de l'interface � supporter
*}
procedure TSepiClass.AddInterface(AIntfTypeInfo : PTypeInfo);
begin
  AddInterface(Root.FindType(AIntfTypeInfo) as TSepiInterface);
end;

{*
  Ajoute le support d'une interface
  @param AIntfName   Nom de l'interface � supporter
*}
procedure TSepiClass.AddInterface(const AIntfName : string);
begin
  AddInterface(Root.FindType(AIntfName) as TSepiInterface);
end;

{*
  Ajoute un champ � la classe
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiClass.AddField(const FieldName : string; FieldType : TSepiType;
  ForcePack : boolean = False) : TSepiMetaField;
var Offset : integer;
begin
  Offset := InstSize;
  if not ForcePack then
    FieldType.AlignOffset(Offset);

  Result := TSepiMetaField.Create(Self, FieldName, FieldType, Offset);
end;

{*
  Ajoute un champ � la classe
  @param FieldName       Nom du champ
  @param FieldTypeInto   RTTI du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiClass.AddField(const FieldName : string;
  FieldTypeInfo : PTypeInfo; ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeInfo), ForcePack);
end;

{*
  Ajoute un champ � la classe
  @param FieldName       Nom du champ
  @param FieldTypeName   Nom du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiClass.AddField(const FieldName, FieldTypeName : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeName), ForcePack);
end;

{*
  Ajoute une m�thode � la classe
  @param MethodName   Nom de la m�thode
  @param ACode        Pointeur sur le code de la m�thode
  @param ASignature   Signature Delphi de la m�thode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la m�thode est abstraite
  @param AMsgID       Pour les m�thodes de message, le message intercept�
  @param ACallingConvention   Convention d'appel de la m�thode
*}
function TSepiClass.AddMethod(const MethodName : string; ACode: Pointer;
  const ASignature : string; ALinkKind : TMethodLinkKind = mlkStatic;
  AAbstract : boolean = False; AMsgID : integer = 0;
  ACallingConvention : TCallingConvention = ccRegister) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod.Create(Self, MethodName, ACode, ASignature,
    ALinkKind, AAbstract, AMsgID, ACallingConvention);
end;

{*
  Ajoute une m�thode � la classe
  @param MethodName           Nom de la m�thode
  @param ACode                Pointeur sur le code de la m�thode
  @param ASignature           Signature Delphi de la m�thode
  @param ACallingConvention   Convention d'appel de la m�thode
*}
function TSepiClass.AddMethod(const MethodName : string; ACode: Pointer;
  const ASignature : string;
  ACallingConvention : TCallingConvention) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod.Create(Self, MethodName, ACode, ASignature,
    mlkStatic, False, 0, ACallingConvention);
end;

{*
  Ajoute une m�thode surcharg�e � la classe
  @param MethodName   Nom de la m�thode
  @param ACode        Pointeur sur le code de la m�thode
  @param ASignature   Signature Delphi de la m�thode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la m�thode est abstraite
  @param AMsgID       Pour les m�thodes de message, le message intercept�
  @param ACallingConvention   Convention d'appel de la m�thode
*}
function TSepiClass.AddOverloadedMethod(const MethodName : string;
  ACode: Pointer; const ASignature : string;
  ALinkKind : TMethodLinkKind = mlkStatic; AAbstract : boolean = False;
  AMsgID : integer = 0;
  ACallingConvention : TCallingConvention = ccRegister) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod.CreateOverloaded(Self, MethodName, ACode,
    ASignature, ALinkKind, AAbstract, AMsgID, ACallingConvention);
end;

{*
  Ajoute une m�thode surcharg�e � la classe
  @param MethodName           Nom de la m�thode
  @param ACode                Pointeur sur le code de la m�thode
  @param ASignature           Signature Delphi de la m�thode
  @param ACallingConvention   Convention d'appel de la m�thode
*}
function TSepiClass.AddOverloadedMethod(const MethodName : string;
  ACode: Pointer; const ASignature : string;
  ACallingConvention : TCallingConvention) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod.CreateOverloaded(Self, MethodName, ACode,
    ASignature, mlkStatic, False, 0, ACallingConvention);
end;

{*
  Ajoute une propri�t� � la classe
  @param AName           Nom de la propri�t�
  @param ASignature      Signature
  @param AReadAccess     Acc�s en lecture � la propri�t� (peut �tre vide)
  @param AWriteAccess    Acc�s en �criture � la propri�t� (peut �tre vide)
  @param AIndex          Index d'acc�s
  @param ADefaultValue   Valeur par d�faut de la propri�t�
  @param AIsDefault      Indique si c'est la propri�t� tableau par d�faut
*}
function TSepiClass.AddProperty(
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIndex : integer = NoIndex; ADefaultValue : integer = NoDefaultValue;
  const AStorage : string = '';
  AIsDefault : boolean = False) : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIndex, ADefaultValue, AStorage, AIsDefault);
end;

{*
  Ajoute une propri�t� � la classe
  @param AName           Nom de la propri�t�
  @param ASignature      Signature
  @param AReadAccess     Acc�s en lecture � la propri�t� (peut �tre vide)
  @param AWriteAccess    Acc�s en �criture � la propri�t� (peut �tre vide)
  @param AIsDefault      Indique si c'est la propri�t� tableau par d�faut
*}
function TSepiClass.AddProperty(
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIsDefault : boolean) : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIsDefault);
end;

{*
  Red�finit une propri�t� h�rit�e
  @param AName          Nom de la propri�t�
  @param AReadAccess    Acc�s en lecture � la propri�t�
  @param AWriteAccess   Acc�s en �criture � la propri�t�
  @param AStorage       Sp�cificateur de stockage
*}
function TSepiClass.RedefineProperty(const AName : string;
  const AReadAccess : string = ''; const AWriteAccess : string = '';
  const AStorage : string = '') : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Redefine(Self, AName,
    AReadAccess, AWriteAccess, AStorage);
end;

{*
  Red�finit une propri�t� h�rit�e
  @param AName           Nom de la propri�t�
  @param AReadAccess     Acc�s en lecture � la propri�t�
  @param AWriteAccess    Acc�s en �criture � la propri�t�
  @param ADefaultValue   Valeur par d�faut
  @param AStorage        Sp�cificateur de stockage
*}
function TSepiClass.RedefineProperty(
  const AName, AReadAccess, AWriteAccess : string; ADefaultValue : integer;
  const AStorage : string = '') : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Redefine(Self, AName,
    AReadAccess, AWriteAccess, ADefaultValue, AStorage);
end;

{*
  Termine la classe et construit ses RTTI si ce n'est pas d�j� fait
*}
procedure TSepiClass.Complete;
begin
  if FCompleted then exit;

  FCompleted := True;
  AlignOffset(FInstSize);
  MakeIMTs;
  if not Native then
    MakeVMT;
end;

{*
  [@inheritDoc]
*}
function TSepiClass.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType is TSepiClass) and
    TSepiClass(AType).ClassInheritsFrom(Self);
end;

{*
  D�termine si la classe h�rite d'une classe donn�e
  @param AParent   Anc�tre � tester
  @return True si la classe h�rite de AParent, False sinon
*}
function TSepiClass.ClassInheritsFrom(AParent : TSepiClass) : boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FParent) and FParent.ClassInheritsFrom(AParent));
end;

{*
  Recherche un membre dans la classe, en tenant compte des visibilit�s
  @param MemberName   Nom du membre recherch�
  @param FromUnit     Unit� d'o� l'on cherche
  @param FromClass    Classe d'o� l'on cherche (ou nil si pas de classe)
  @return Le membre correspondant, ou nil si non trouv�
*}
function TSepiClass.LookForMember(const MemberName : string;
  FromUnit : TSepiUnit; FromClass : TSepiClass = nil) : TSepiReflectionItem;
begin
  if MemberName = '' then
  begin
    Result := nil;
    exit;
  end;

  Result := GetChild(MemberName);

  if Result <> nil then
  begin
    case Result.Visibility of
      mvStrictPrivate  : if FromClass <> Self then Result := nil;
      mvPrivate : if FromUnit <> OwningUnit then Result := nil;
      mvStrictProtected :
        if (FromClass = nil) or (not FromClass.ClassInheritsFrom(Self)) then
          Result := nil;
      mvProtected :
        if (FromUnit <> OwningUnit) and
           ((FromClass = nil) or (not FromClass.ClassInheritsFrom(Self))) then
          Result := nil;
    end;
  end;

  if (Result = nil) and (Parent <> nil) then
    Result := Parent.LookForMember(MemberName, FromUnit, FromClass);
end;

{-----------------------}
{ Classe TSepiMetaClass }
{-----------------------}

{*
  Charge une meta-classe depuis un flux
*}
constructor TSepiMetaClass.Load(AOwner : TSepiReflectionItem; Stream : TStream);
begin
  inherited;
  FSize := 4;
  OwningUnit.ReadRef(Stream, FClass);
end;

{*
  Cr�e une nouvelle meta-classe
  @param AOwner   Propri�taire du type
  @param AName    Nom du type
  @param AClass   Classe correspondante
*}
constructor TSepiMetaClass.Create(AOwner : TSepiReflectionItem;
  const AName : string; AClass : TSepiClass; AIsNative : boolean = False);
begin
  inherited Create(AOwner, AName, tkClass);
  FSize := 4;
  FClass := AClass;

  if AIsNative then
    ForceNative;
end;

{*
  Cr�e une nouvelle meta-classe
  @param AOwner       Propri�taire du type
  @param AName        Nom du type
  @param AClassInfo   RTTI de la classe correspondante
*}
constructor TSepiMetaClass.Create(AOwner : TSepiReflectionItem;
  const AName : string; AClassInfo : PTypeInfo; AIsNative : boolean = False);
begin
  Create(AOwner, AName, AOwner.Root.FindType(AClassInfo) as TSepiClass,
    AIsNative);
end;

{*
  Cr�e une nouvelle meta-classe
  @param AOwner       Propri�taire du type
  @param AName        Nom du type
  @param AClassName   Nom de la classe correspondante
*}
constructor TSepiMetaClass.Create(AOwner : TSepiReflectionItem;
  const AName, AClassName : string; AIsNative : boolean = False);
begin
  Create(AOwner, AName, AOwner.Root.FindType(AClassName) as TSepiClass,
    AIsNative);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaClass.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FClass);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaClass.Save(Stream : TStream);
begin
  inherited;
  OwningUnit.WriteRef(Stream, FClass);
end;

{*
  [@inheritDoc]
*}
function TSepiMetaClass.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType is TSepiMetaClass) and
    TSepiMetaClass(AType).SepiClass.ClassInheritsFrom(SepiClass);
end;

{---------------------------}
{ Classe TSepiMethodRefType }
{---------------------------}

{*
  Recense un type r�f�rence de m�thode natif
*}
constructor TSepiMethodRefType.RegisterTypeInfo(AOwner : TSepiReflectionItem;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  FSignature := TSepiMethodSignature.RegisterTypeData(Self, TypeData);

  if Signature.Kind in mkOfObject then
  begin
    FSize := 8;
    FParamBehavior.AlwaysByStack := True;
  end else
    FSize := 4;
end;

{*
  Charge un type r�f�rence de m�thode depuis un flux
*}
constructor TSepiMethodRefType.Load(AOwner : TSepiReflectionItem;
  Stream : TStream);
begin
  inherited;
  FSignature := TSepiMethodSignature.Load(Self, Stream);
  LoadChildren(Stream);

  if Signature.Kind in mkOfObject then
  begin
    FSize := 8;
    FParamBehavior.AlwaysByStack := True;
  end else
    FSize := 4;
end;

{*
  Cr�e un nouveau type r�f�rence de m�thode
  @param AOwner            Propri�taire du type
  @param AName             Nom du type
  @param ASignature        Signature
  @param AOfObject         Indique s'il s'agit d'une m�thode
  @param ACallingConvention   Convention d'appel
*}
constructor TSepiMethodRefType.Create(AOwner : TSepiReflectionItem;
  const AName, ASignature : string; AOfObject : boolean = False;
  ACallingConvention : TCallingConvention = ccRegister);
var Prefix : string;
begin
  inherited Create(AOwner, AName, tkMethod);

  if AOfObject then Prefix := '' else Prefix := 'unit ';
  FSignature := TSepiMethodSignature.Create(Self,
    Prefix + ASignature, ACallingConvention);

  if Signature.Kind in mkOfObject then
  begin
    FSize := 8;
    FParamBehavior.AlwaysByStack := True;
  end else
    FSize := 4;
end;

{*
  D�truit l'instance
*}
destructor TSepiMethodRefType.Destroy;
begin
  FSignature.Free;
  inherited Destroy;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodRefType.ListReferences;
begin
  inherited;
  Signature.ListReferences;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodRefType.Save(Stream : TStream);
begin
  inherited;
  Signature.Save(Stream);
  SaveChildren(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiMethodRefType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType.Kind = tkMethod) and
    FSignature.Equals(TSepiMethodRefType(AType).FSignature);
end;

{-------------------------}
{ Classe TSepiVariantType }
{-------------------------}

{*
  Recense un type variant natif
*}
constructor TSepiVariantType.RegisterTypeInfo(AOwner : TSepiReflectionItem;
  ATypeInfo : PTypeInfo);
begin
  inherited;

  FSize := 16;
  FNeedInit := True;
  FParamBehavior.AlwaysByAddress := True;
end;

{*
  Charge un type variant depuis un flux
*}
constructor TSepiVariantType.Load(AOwner : TSepiReflectionItem;
  Stream : TStream);
begin
  inherited;

  AllocateTypeInfo;

  FSize := 16;
  FNeedInit := True;
  FParamBehavior.AlwaysByAddress := True;
end;

{*
  [@inheritDoc]
*}
function TSepiVariantType.GetAlignment : integer;
begin
  Result := 8;
end;

initialization
  SepiRegisterReflectionItemClasses([
    TSepiMetaField, TSepiMetaParam, TSepiMetaMethod, TSepiMetaOverloadedMethod,
    TSepiMetaProperty, TSepiRecordType, TSepiInterface, TSepiClass,
    TSepiMethodRefType, TSepiVariantType
  ]);
end.

