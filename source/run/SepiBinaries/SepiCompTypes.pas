{*
  Définit les classes de gestion des types composites
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiCompTypes;

interface

uses
  Windows, Classes, SysUtils, ScUtils, SepiMetaUnits, SysConst, TypInfo,
  Contnrs, ScLists, StrUtils, ScStrUtils, ScDelphiLanguage, SepiBinariesConsts,
  SepiCore;

const
  /// Pas d'index
  NoIndex = integer($80000000);

  /// Pas de valeur par défaut
  NoDefaultValue = integer($80000000);

type
  {*
    Type de liaison d'une méthode
    - mlkStatic : méthode statique (ni virtuelle ni dynamique)
    - mlkVirtual : méthode à liaison virtuelle (via VMT)
    - mlkDynamic : méthode à liaison dynamique (via DMT)
    - mlkMessage : méthode d'interception de message (via DMT)
    - mlkInterface : méthode à liaison d'interface (via IMT)
    - mlkOverride : détermine le type de liaison depuis la méthode héritée
  *}
  TMethodLinkKind = (mlkStatic, mlkVirtual, mlkDynamic, mlkMessage,
    mlkInterface, mlkOverride);

  {*
    Convention d'appel d'une méthode
  *}
  TCallConvention = (ccRegister, ccStdCall, ccPascal, ccCDecl);

  {*
    Type d'accesseur d'une propriété
  *}
  TPropertyAccessKind = (pakNone, pakField, pakMethod);

  {*
    Meta-variable
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaField = class(TSepiMeta)
  private
    FType : TSepiType; /// Type du champ
    FOffset : integer; /// Offset
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AType : TSepiType; AOffset : integer);

    property FieldType : TSepiType read FType;
    property Offset : integer read FOffset;
  end;

  {*
    Meta-paramètre de méthode
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaParam = class(TSepiMeta)
  private
    FType : TSepiType;    /// Type du paramètre (peut être nil)
    FFlags : TParamFlags; /// Flags du paramètre

    constructor RegisterParamData(AOwner : TSepiMeta; var ParamData : Pointer);
    constructor CreateFromString(AOwner : TSepiMeta; const Definition : string);
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AType : TSepiType; AFlags : TParamFlags = []);

    function Equals(AParam : TSepiMetaParam) : boolean;
    function CompatibleWith(AType : TSepiType) : boolean;

    property ParamType : TSepiType read FType;
    property Flags : TParamFlags read FFlags;
  end;

  {*
    Signature d'une méthode
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMethodSignature = class
  private
    FOwner : TSepiMeta;                /// Propriétaire de la signature
    FKind : TMethodKind;               /// Type de méthode
    FReturnType : TSepiType;           /// Type de retour
    FCallConvention : TCallConvention; /// Convention d'appel

    function GetParamCount : integer;
    function GetParams(Index : integer) : TSepiMetaParam;
  protected
    procedure ListReferences;
    procedure Save(Stream : TStream);
  public
    constructor RegisterTypeData(AOwner : TSepiMeta; ATypeData : PTypeData);
    constructor Load(AOwner : TSepiMeta; Stream : TStream);
    constructor Create(AOwner : TSepiMeta; const ASignature : string;
      ACallConvention : TCallConvention = ccRegister);

    function Equals(ASignature : TSepiMethodSignature) : boolean;
    function CompatibleWith(const ATypes : array of TSepiType) : boolean;

    property Owner : TSepiMeta read FOwner;
    property Kind : TMethodKind read FKind;
    property ParamCount : integer read GetParamCount;
    property Params[index : integer] : TSepiMetaParam read GetParams;
    property ReturnType : TSepiType read FReturnType;
    property CallConvention : TCallConvention read FCallConvention;
  end;

  {*
    Meta-méthode
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaMethod = class(TSepiMeta)
  private
    FCode : Pointer;                   /// Adresse de code natif
    FSignature : TSepiMethodSignature; /// Signature de la méthode
    FLinkKind : TMethodLinkKind;       /// Type de liaison d'appel
    FFirstDeclaration : boolean;       /// Faux uniquement quand 'override'
    FAbstract : boolean;               /// Indique si la méthode est abstraite
    FInherited : TSepiMetaMethod;      /// Méthode héritée

    FLinkIndex : integer; /// Offset de VMT ou index de DMT, selon la liaison

    procedure MakeLink;
    procedure FindNativeCode;
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      ACode : Pointer; const ASignature : string;
      ALinkKind : TMethodLinkKind = mlkStatic; AAbstract : boolean = False;
      AMsgID : integer = 0; ACallConvention : TCallConvention = ccRegister);
    destructor Destroy; override;

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
  end;

  {*
    Meta-méthode surchargée
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaOverloadedMethod = class(TSepiMeta)
  private
    FMethodCount : integer; /// Nombre de méthodes de même nom
  protected
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AMethodCount : integer = 0);

    function NextID : integer;
    function FindMethod(
      ASignature : TSepiMethodSignature) : TSepiMetaMethod; overload;
    function FindMethod(
      const ATypes : array of TSepiType) : TSepiMetaMethod; overload;
  end;

  {*
    Accès de propriété
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiPropertyAccess = record
    Kind : TPropertyAccessKind;               /// Type d'accès
    case TPropertyAccessKind of
      pakNone : (Meta : TSepiMeta);           /// Meta d'accès (neutre)
      pakField : (Field : TSepiMetaField);    /// Meta-champ d'accès
      pakMethod : (Method : TSepiMetaMethod); /// Meta-méthode d'accès
  end;

  {*
    Meta-propriété
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaProperty = class(TSepiMeta)
  private
    FSignature : TSepiMethodSignature; /// Signature

    FReadAccess : TSepiPropertyAccess;  /// Accès en lecture
    FWriteAccess : TSepiPropertyAccess; /// Accès en écriture
    FIndex : integer;                   /// Index d'accès
    FDefaultValue : integer;            /// Valeur par défaut

    FIsDefault : boolean; /// Indique si c'est la propriété par défaut

    procedure MakePropInfo(PropInfo : PPropInfo);

    function GetPropType : TSepiType;
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta;
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIndex : integer = NoIndex; ADefaultValue : integer = NoDefaultValue;
      AIsDefault : boolean = False); overload;
    constructor Create(AOwner : TSepiMeta;
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIsDefault : boolean); overload;
    destructor Destroy; override;

    property Signature : TSepiMethodSignature read FSignature;
    property PropType : TSepiType read GetPropType;

    property ReadAccess : TSepiPropertyAccess read FReadAccess;
    property WriteAccess : TSepiPropertyAccess read FWriteAccess;
    property Index : integer read FIndex;
    property DefaultValue : integer read FIndex;
    property IsDefault : boolean read FIsDefault;
  end;

  {*
    Type enregistrement
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiRecordType = class(TSepiType)
  private
    FPacked : boolean;    /// Indique si le record est packed
    FAlignment : integer; /// Alignement
    FCompleted : boolean; /// Indique si le record est entièrement défini

    function AddField(const FieldName : string; FieldType : TSepiType;
      After : TSepiMetaField;
      ForcePack : boolean = False) : TSepiMetaField; overload;

    procedure MakeTypeInfo;
  protected
    procedure ChildAdded(Child : TSepiMeta); override;

    procedure Save(Stream : TStream); override;

    function GetAlignment : integer; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      APacked : boolean = False; AIsNative : boolean = False;
      ATypeInfo : PTypeInfo = nil);

    function AddField(const FieldName : string; FieldType : TSepiType;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddField(const FieldName : string; FieldType : TSepiType;
      const After : string;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddField(const FieldName : string; FieldTypeInfo : PTypeInfo;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddField(const FieldName : string; FieldTypeInfo : PTypeInfo;
      const After : string;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddField(const FieldName, FieldTypeName : string;
      ForcePack : boolean = False) : TSepiMetaField; overload;
    function AddField(const FieldName, FieldTypeName : string;
      const After : string;
      ForcePack : boolean = False) : TSepiMetaField; overload;

    procedure Complete;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property IsPacked : boolean read FPacked;
    property Completed : boolean read FCompleted;
  end;

  {*
    Interface
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiInterface = class(TSepiType)
  private
    FParent : TSepiInterface; /// Interface parent (ou nil - IInterface)
    FCompleted : boolean;     /// Indique si l'interface est entièrement définie

    FHasGUID : boolean;         /// Indique si l'interface possède un GUID
    FIsDispInterface : boolean; /// Indique si c'est une disp interface
    FIsDispatch : boolean;      /// Indique si c'est une IDispatch
    FGUID : TGUID;              /// GUID de l'interface, si elle en a un

    FIMTSize : integer; /// Taille de l'IMT

    procedure MakeTypeInfo;
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AParent : TSepiInterface; const AGUID : TGUID;
      AIsDispInterface : boolean = False);

    class function NewInstance : TObject; override;

    class function ForwardDecl(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo) : TSepiInterface; overload;
    class function ForwardDecl(AOwner : TSepiMeta;
      const AName : string) : TSepiInterface; overload;

    function AddMethod(const MethodName, ASignature : string;
      ACallConvention : TCallConvention = ccRegister) : TSepiMetaMethod;

    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIndex : integer = NoIndex;
      AIsDefault : boolean = False) : TSepiMetaProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIsDefault : boolean) : TSepiMetaProperty; overload;

    procedure Complete;

    function IntfInheritsFrom(AParent : TSepiInterface) : boolean;

    function LookForMember(const MemberName : string) : TSepiMeta;

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
    Entrée d'interface pour les classes Sepi
    Chaque classe a, pour chaque interface qu'elle implémente, une entrée de
    type TSepiInterfaceEntry.
  *}
  TSepiInterfaceEntry = record
    IntfRef : TSepiInterface; /// Interface implémentée
    Relocates : Pointer;      /// Thunks de relocalisation
    IMT : Pointer;            /// Interface Method Table
    Offset : integer;         /// Offset du champ interface dans l'objet
  end;

  {*
    Classe (type objet)
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiClass = class(TSepiType)
  private
    FDelphiClass : TClass; /// Classe Delphi
    FParent : TSepiClass;  /// Classe parent (nil si n'existe pas - TObject)
    FCompleted : boolean;  /// Indique si la classe est entièrement définie

    /// Interfaces supportées par la classe
    FInterfaces : array of TSepiInterfaceEntry;

    FInstSize : integer;     /// Taille d'une instance de la classe
    FVMTSize : integer;      /// Taille de la VMT dans les index positifs
    FDMTNextIndex : integer; /// Prochain index à utiliser dans la DMT

    FCurrentVisibility : TMemberVisibility; /// Visibilité courante des enfants

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
    procedure ChildAdded(Child : TSepiMeta); override;

    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;

    property VMTEntries[index : integer] : Pointer
      read GetVMTEntries write SetVMTEntries;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AParent : TSepiClass);
    destructor Destroy; override;

    class function NewInstance : TObject; override;

    class function ForwardDecl(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo) : TSepiClass; overload;
    class function ForwardDecl(AOwner : TSepiMeta;
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
      ACallConvention : TCallConvention = ccRegister) : TSepiMetaMethod;
      overload;
    function AddMethod(const MethodName : string; ACode: Pointer;
      const ASignature : string;
      ACallConvention : TCallConvention) : TSepiMetaMethod; overload;

    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIndex : integer = NoIndex; ADefaultValue : integer = NoDefaultValue;
      AIsDefault : boolean = False) : TSepiMetaProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess : string;
      AIsDefault : boolean) : TSepiMetaProperty; overload;

    procedure Complete;

    function CompatibleWith(AType : TSepiType) : boolean; override;
    function ClassInheritsFrom(AParent : TSepiClass) : boolean;

    function LookForMember(const MemberName : string; FromUnit : TSepiMetaUnit;
      FromClass : TSepiClass = nil) : TSepiMeta;

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
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaClass = class(TSepiType)
  private
    FClass : TSepiClass; /// Classe correspondante
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AClass : TSepiClass; AIsNative : boolean = False); overload;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AClassInfo : PTypeInfo; AIsNative : boolean = False); overload;
    constructor Create(AOwner : TSepiMeta; const AName, AClassName : string;
      AIsNative : boolean = False); overload;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property SepiClass : TSepiClass read FClass;
  end;

  {*
    Type référence de méthode
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMethodRefType = class(TSepiType)
  private
    FSignature : TSepiMethodSignature; /// Signature
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName, ASignature : string;
      AOfObject : boolean = False;
      ACallConvention : TCallConvention = ccRegister);
    destructor Destroy; override;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property Signature : TSepiMethodSignature read FSignature;
  end;

  {*
    Type variant
    Note : il est impossible de créer un nouveau type variant.
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiVariantType = class(TSepiType)
  protected
    function GetAlignment : integer; override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
  end;

const
  /// Procédure d'unité
  mkUnitProcedure = TMethodKind(integer(High(TMethodKind))+1);
  /// Fonction d'unité
  mkUnitFunction = TMethodKind(integer(High(TMethodKind))+2);
  /// Propriété
  mkProperty = TMethodKind(integer(High(TMethodKind))+3);

  /// Types de méthodes d'objet
  mkOfObject = [mkProcedure..mkClassConstructor];

  /// Chaînes des types de méthode
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

{-----------------------}
{ Classe TSepiMetaField }
{-----------------------}

{*
  Charge un champ depuis un flux
*}
constructor TSepiMetaField.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FType);
  Stream.ReadBuffer(FOffset, 4);
end;

{*
  Crée un nouveau champ
  @param AOwner   Propriétaire du champ
  @param AName    Nom du champ
  @param AType    Type du champ
*}
constructor TSepiMetaField.Create(AOwner : TSepiMeta; const AName : string;
  AType : TSepiType; AOffset : integer);
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
  Crée un paramètre depuis les données de type d'une référence de méthode
  En sortie, le pointeur ParamData a avancé jusqu'au paramètre suivant
  @param AOwner      Propriétaire du paramètre
  @param ParamData   Pointeur vers les données du paramètre
*}
constructor TSepiMetaParam.RegisterParamData(AOwner : TSepiMeta;
  var ParamData : Pointer);
var AFlags : TParamFlags;
    AName, ATypeStr : string;
begin
  AFlags := TParamFlags(ParamData^);
  inc(LongInt(ParamData), sizeof(TParamFlags));
  AName := PShortString(ParamData)^;
  inc(LongInt(ParamData), PByte(ParamData)^ + 1);
  ATypeStr := PShortString(ParamData)^;
  inc(LongInt(ParamData), PByte(ParamData)^ + 1);

  Create(AOwner, AName, AOwner.Root.FindType(ATypeStr), AFlags);
end;

{*
  Crée un paramètre depuis sa définition Delphi
  @param AOwner       Propriétaire du paramètre
  @param Definition   Définition Delphi du paramètre
*}
constructor TSepiMetaParam.CreateFromString(AOwner : TSepiMeta;
  const Definition : string);
var AFlags : TParamFlags;
    NamePart, TypePart, FlagStr, AName, ATypeStr : string;
begin
  AFlags := [];
  if not SplitToken(Definition, ':', NamePart, TypePart) then
    TypePart := '';
  TypePart := GetFirstToken(TypePart, '=');

  // Partie du nom - à gauche du :
  if SplitToken(Trim(NamePart), ' ', FlagStr, AName) then
  begin
    case AnsiIndexText(FlagStr, ['var', 'const', 'out']) of {don't localize}
      0 : Include(AFlags, pfVar);
      1 : Include(AFlags, pfConst);
      2 : Include(AFlags, pfOut);
    end;
  end;

  // Partie du type - à droite du :
  TypePart := Trim(TypePart);
  if AnsiStartsText('array of ', TypePart) then {don't localize}
  begin
    Include(AFlags, pfArray);
    ATypeStr := TrimLeft(Copy(TypePart, 10, MaxInt)); // 10 is 'array of |'

    if AnsiSameText(ATypeStr, 'const') then {don't localize}
      ATypeStr := '';
  end else ATypeStr := TypePart;

  Create(AOwner, AName, AOwner.Root.FindType(ATypeStr), AFlags);
end;

{*
  Charge un paramètre depuis un flux
*}
constructor TSepiMetaParam.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FType);
  Stream.ReadBuffer(FFlags, 1);
end;

{*
  Crée un nouveau paramètre
  @param AOwner   Propriétaire du paramètre
  @param AName    Nom du paramètre
  @param AType    Type du paramètre
  @param AFlags   Flags
*}
constructor TSepiMetaParam.Create(AOwner : TSepiMeta; const AName : string;
  AType : TSepiType; AFlags : TParamFlags = []);
begin
  inherited Create(AOwner, AName);

  FType := AType;
  FFlags := AFlags;
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
  OwningUnit.WriteRef(Stream, FType);
  Stream.WriteBuffer(FFlags, 1);
end;

{*
  Détermine si deux paramètres sont identiques
  @param AParam   Paramètre à comparer
  @return True si les paramètres sont identiques, False sinon
*}
function TSepiMetaParam.Equals(AParam : TSepiMetaParam) : boolean;
begin
  Result := (ParamType = AParam.ParamType) and (Flags = AParam.Flags);
end;

{*
  Détermine si un type est compatible avec le paramètre
  @param AType   Type à Tester
  @return True si le type est compatible, False sinon
*}
function TSepiMetaParam.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := FType.CompatibleWith(AType);
end;

{-----------------------------}
{ Classe TSepiMethodSignature }
{-----------------------------}

{*
  Crée une signature à partir des données de type d'un type méthode
  @param AOwner      Propriétaire de la signature
  @param ATypeData   Données de type
*}
constructor TSepiMethodSignature.RegisterTypeData(AOwner : TSepiMeta;
  ATypeData : PTypeData);
var ParamData : Pointer;
    I : integer;
begin
  inherited Create;

  FOwner := AOwner;
  FKind := ATypeData.MethodKind;
  ParamData := @ATypeData.ParamList;

  for I := 1 to ATypeData.ParamCount do
    TSepiMetaParam.RegisterParamData(FOwner, ParamData);

  FReturnType := FOwner.Root.FindType(PShortString(ParamData)^);
  FCallConvention := ccRegister;
end;

{*
  Charge une signature depuis un flux
  @param AOwner   Propriétaire de la signature
  @param Stream   Flux depuis lequel charger la signature
*}
constructor TSepiMethodSignature.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited Create;

  FOwner := AOwner;
  Stream.ReadBuffer(FKind, 1);
  FOwner.OwningUnit.ReadRef(Stream, FReturnType);
  Stream.ReadBuffer(FCallConvention, 1);

  // Parameters should be loaded by the owner, for they are children of it
end;

{*
  Crée une signature de méthode
  @param AOwner            Propriétaire de la signature
  @param ASignature        Signature Delphi
  @param ACallConvention   Convention d'appel
*}
constructor TSepiMethodSignature.Create(AOwner : TSepiMeta;
  const ASignature : string; ACallConvention : TCallConvention = ccRegister);

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
  FCallConvention := ACallConvention;

  // Type de méthode
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

  // Paramètres
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
end;

{*
  Nombre de paramètres
  @return Nombre de paramètres
*}
function TSepiMethodSignature.GetParamCount : integer;
begin
  Result := FOwner.ChildCount;
end;

{*
  Tableau zero-based des paramètres
  @param Index   Index du paramètre à récupérer
  @return Paramètre situé à l'index Index
*}
function TSepiMethodSignature.GetParams(Index : integer) : TSepiMetaParam;
begin
  { Here, we can't guarantee that all children are TSepiMetaParam, so we
    check it with an as operator. }
  Result := FOwner.Children[Index] as TSepiMetaParam;
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
  Stream.WriteBuffer(FCallConvention, 1);

  // Parameters should be saved by the owner, for they are children of it
end;

{*
  Détermine si deux signatures sont identiques
  @param ASignature   Signature à comparer
  @return True si les signatures sont identiques, False sinon
*}
function TSepiMethodSignature.Equals(
  ASignature : TSepiMethodSignature) : boolean;
var I : integer;
begin
  Result := False;

  if Kind <> ASignature.Kind then exit;
  if ParamCount <> ASignature.ParamCount then exit;
  for I := 0 to ParamCount-1 do
    if not Params[I].Equals(ASignature.Params[I]) then exit;
  if FReturnType <> ASignature.ReturnType then exit;

  Result := True;
end;

{*
  Détermine si une liste de types est compatible avec la signature
  @param ATypes   Liste des types à tester
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
  Charge une meta-méthode depuis un flux
*}
constructor TSepiMetaMethod.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  FCode := nil;
  FSignature := TSepiMethodSignature.Load(Self, Stream);
  LoadChildren(Stream);
  Stream.ReadBuffer(FLinkKind, 1);
  FFirstDeclaration := FLinkKind <> mlkOverride;
  Stream.ReadBuffer(FAbstract, 1);
  Stream.ReadBuffer(FLinkIndex, 2); // only for messages

  if Assigned(Root.OnGetMethodCode) then
    Root.OnGetMethodCode(Self, FCode);

  MakeLink;
end;

{*
  Crée une nouvelle meta-méthode
  @param AOwner            Propriétaire de la méthode
  @param AName             Nom de la méthode
  @param ACode             Pointeur sur le code de la méthode
  @param ASignature        Signature Delphi de la méthode
  @param ALinkKind         Type de liaison
  @param AAbstract         Indique si la méthode est abstraite
  @param AMsgID            Pour les méthodes de message, le message intercepté
  @param ACallConvention   Convention d'appel de la méthode
*}
constructor TSepiMetaMethod.Create(AOwner : TSepiMeta; const AName : string;
  ACode : Pointer; const ASignature : string;
  ALinkKind : TMethodLinkKind = mlkStatic; AAbstract : boolean = False;
  AMsgID : integer = 0; ACallConvention : TCallConvention = ccRegister);
var SignPrefix : string;
begin
  inherited Create(AOwner, AName);

  if Owner is TSepiMetaUnit then
    SignPrefix := 'unit '
  else
    SignPrefix := '';

  FCode := ACode;
  FSignature := TSepiMethodSignature.Create(Self,
    SignPrefix + ASignature, ACallConvention);
  FLinkKind := ALinkKind;
  FFirstDeclaration := FLinkKind <> mlkOverride;
  FAbstract := AAbstract and (FLinkKind in [mlkVirtual, mlkDynamic]);
  FLinkIndex := AMsgID; // only for messages

  MakeLink;

  if (Code = nil) and (Owner as TSepiType).Native then
    FindNativeCode;
end;

{*
  Détruit l'instance
*}
destructor TSepiMetaMethod.Destroy;
begin
  FSignature.Free;
  inherited Destroy;
end;

{*
  Met au point la liaison et l'index de liaison
  Par extension, recherche aussi la méthode héritée.
*}
procedure TSepiMetaMethod.MakeLink;
var OwningClass, Ancestor : TSepiClass;
    LookFor : string;
    Meta : TSepiMeta;
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
      // Setting the inherited method name
      LookFor := Name;
      if Copy(Name, 1, 3) = 'OL$' then // overloaded
      begin
        Delete(LookFor, 1, 3);
        Delete(LookFor, Pos('$', LookFor), MaxInt);
      end;

      // Looking for the inherited method
      Meta := OwningClass.Parent.LookForMember(LookFor,
        OwningUnit, OwningClass);
      if Meta is TSepiMetaMethod then
      begin
        if TSepiMetaMethod(Meta).Signature.Equals(Signature) then
          FInherited := TSepiMetaMethod(Meta);
      end else
      if Meta is TSepiMetaOverloadedMethod then
        FInherited := TSepiMetaOverloadedMethod(Meta).FindMethod(Signature);
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
  Cherche le code d'une méthode native
*}
procedure TSepiMetaMethod.FindNativeCode;

  function GetDynamicCode(AClass : TClass; DMTIndex : integer) : Pointer;
  asm
        { ->    EAX     Pointer to class        }
        {       EDX     DMTIndex                }
        { <-    EAX     Pointer to method       }
        PUSH    ECX
        CALL    System.@FindDynaClass
        POP     ECX
  end;

begin
  case LinkKind of
    mlkVirtual : FCode := TSepiClass(Owner).VMTEntries[VMTOffset];
    mlkDynamic, mlkMessage :
    try
      FCode := GetDynamicCode(TSepiClass(Owner).DelphiClass, DMTIndex);
    except
      on Error : EAbstractError do;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaMethod.ListReferences;
begin
  inherited;
  Signature.ListReferences;
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
end;

{----------------------------------}
{ Classe TSepiMetaOverloadedMethod }
{----------------------------------}

{*
  Charge une méthode surchargée depuis un flux
*}
constructor TSepiMetaOverloadedMethod.Load(AOwner : TSepiMeta;
  Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FMethodCount, 4);
end;

{*
  Crée une nouvelle méthode surchargée
  @param AOwner   Propriétaire de la méthode
  @param AName    Nom de la méthode
*}
constructor TSepiMetaOverloadedMethod.Create(AOwner : TSepiMeta;
  const AName : string; AMethodCount : integer = 0);
begin
  inherited Create(AOwner, AName);
  FMethodCount := AMethodCount;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaOverloadedMethod.Save(Stream : TStream);
begin
  inherited;
  Stream.WriteBuffer(FMethodCount, 4);
end;

{*
  Détermine l'ID de méthode surchargée suivant
  @return Nouvel ID
*}
function TSepiMetaOverloadedMethod.NextID : integer;
begin
  Result := FMethodCount;
  inc(FMethodCount);
end;

{*
  Trouve la méthode effective qui correspond à une signature donnée
  @param ASignature   Signature à rechercher
  @return Méthode effective correspondante
*}
function TSepiMetaOverloadedMethod.FindMethod(
  ASignature : TSepiMethodSignature) : TSepiMetaMethod;
var I : integer;
begin
  for I := 0 to FMethodCount-1 do
  begin
    Result := TSepiMetaMethod(Owner.FindMeta(Format('OL$%s$%d', [Name, I])));
    if Result.Signature.Equals(ASignature) then exit;
  end;
  Result := nil;
end;

{*
  Trouve la méthode effective qui correspond à une liste de types de paramètres
  @param ATypes   Liste des types de paramètres
  @return Méthode effective correspondante
*}
function TSepiMetaOverloadedMethod.FindMethod(
  const ATypes : array of TSepiType) : TSepiMetaMethod;
var I : integer;
begin
  for I := 0 to FMethodCount-1 do
  begin
    Result := TSepiMetaMethod(Owner.FindMeta(Format('OL$%s$%d', [Name, I])));
    if Result.Signature.CompatibleWith(ATypes) then exit;
  end;
  Result := nil;
end;

{--------------------------}
{ Classe TSepiMetaProperty }
{--------------------------}

{*
  Charge une propriété depuis un flux
*}
constructor TSepiMetaProperty.Load(AOwner : TSepiMeta; Stream : TStream);
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
  Crée une nouvelle propriété
  @param AOwner          Propriétaire de la propriété
  @param AName           Nom de la propriété
  @param ASignature      Signature
  @param AReadAccess     Accès en lecture à la propriété (peut être vide)
  @param AWriteAccess    Accès en écriture à la propriété (peut être vide)
  @param AIndex          Index d'accès
  @param ADefaultValue   Valeur par défaut de la propriété
  @param AIsDefault      Indique si c'est la propriété tableau par défaut
*}
constructor TSepiMetaProperty.Create(AOwner : TSepiMeta;
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIndex : integer = NoIndex; ADefaultValue : integer = NoDefaultValue;
  AIsDefault : boolean = False);

  function FindAccess(const AAccess : string) : TSepiMeta;
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

  FIsDefault := AIsDefault and (Signature.ParamCount > 0);
end;

{*
  Crée une nouvelle propriété
  @param AOwner         Propriétaire de la propriété
  @param AName          Nom de la propriété
  @param ASignature     Signature
  @param AReadAccess    Accès en lecture à la propriété (peut être vide)
  @param AWriteAccess   Accès en écriture à la propriété (peut être vide)
  @param AIsDefault     Indique si c'est la propriété tableau par défaut
*}
constructor TSepiMetaProperty.Create(AOwner : TSepiMeta;
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIsDefault : boolean);
begin
  Create(AOwner, AName, ASignature, AReadAccess, AWriteAccess,
    NoIndex, NoDefaultValue, AIsDefault);
end;

{*
  Détruit l'instance
*}
destructor TSepiMetaProperty.Destroy;
begin
  FSignature.Free;
  inherited;
end;

{*
  Construit les informations de propriétés pour les RTTI
  Cette méthode ne doit être appelée que pour des propriétés de classe, pas pour
  des propriétés d'interface.
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

  // Some information
  { TODO 1 -Metaunités : Renseigner convenablement PropInfo.StoredProc }
  PropInfo.StoredProc := nil;
  PropInfo.Index := Index;
  PropInfo.Default := DefaultValue;
  PropInfo.NameIndex := 0; // Unknown, give 0 and pray...

  // Property name
  ShortName := Name;
  Move(ShortName[0], PropInfo.Name[0], Length(ShortName)+1);
end;

{*
  Type de la propriété
  @return Type de la propriété
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

{------------------------}
{ Classe TSepiRecordType }
{------------------------}

{*
  Charge un type record depuis un flux
*}
constructor TSepiRecordType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  Stream.ReadBuffer(FPacked, 1);
  FAlignment := 1;
  FCompleted := False;

  LoadChildren(Stream);

  Complete;
end;

{*
  Crée un nouveau type record
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
*}
constructor TSepiRecordType.Create(AOwner : TSepiMeta; const AName : string;
  APacked : boolean = False; AIsNative : boolean = False;
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
  @param After       Champ précédent en mémoire
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
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
procedure TSepiRecordType.ChildAdded(Child : TSepiMeta);
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
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldType : TSepiType; ForcePack : boolean = False) : TSepiMetaField;
var LastField : TSepiMetaField;
begin
  if ChildCount = 0 then LastField := nil else
    LastField := TSepiMetaField(Children[ChildCount-1]);

  Result := AddField(FieldName, FieldType, LastField, ForcePack);
end;

{*
  Ajoute un champ au record après un champ donné en mémoire
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param After       Nom du champ précédent en mémoire (vide pour le début)
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldType : TSepiType; const After : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, FieldType,
    TSepiMetaField(FindMeta(After)), ForcePack);
end;

{*
  Ajoute un champ au record
  @param FieldName       Nom du champ
  @param FieldTypeInto   RTTI du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldTypeInfo : PTypeInfo; ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeInfo), ForcePack);
end;

{*
  Ajoute un champ au record après un champ donné en mémoire
  @param FieldName       Nom du champ
  @param FieldTypeInfo   RTTI du type du champ
  @param After           Nom du champ précédent en mémoire (vide pour le début)
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldTypeInfo : PTypeInfo; const After : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeInfo),
    TSepiMetaField(FindMeta(After)), ForcePack);
end;

{*
  Ajoute un champ au record
  @param FieldName       Nom du champ
  @param FieldTypeName   Nom du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName, FieldTypeName : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeName), ForcePack);
end;

{*
  Ajoute un champ au record après un champ donné en mémoire
  @param FieldName       Nom du champ
  @param FieldTypeName   Nom du type du champ
  @param After           Nom du champ précédent en mémoire (vide pour le début)
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(
  const FieldName, FieldTypeName, After : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeName),
    TSepiMetaField(FindMeta(After)), ForcePack);
end;

{*
  Termine le record et construit ses RTTI si ce n'est pas déjà fait
*}
procedure TSepiRecordType.Complete;
begin
  if FCompleted then exit;

  FCompleted := True;
  AlignOffset(FSize);
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
constructor TSepiInterface.RegisterTypeInfo(AOwner : TSepiMeta;
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
constructor TSepiInterface.Load(AOwner : TSepiMeta; Stream : TStream);
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
  Crée une nouvelle interface
  @param AOwner    Propriétaire du type
  @param AName     Nom du type
  @param AParent   Classe parent
*}
constructor TSepiInterface.Create(AOwner : TSepiMeta; const AName : string;
  AParent : TSepiInterface; const AGUID : TGUID;
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
  Crée une nouvelle instance de TSepiInterface
  @return Instance créée
*}
class function TSepiInterface.NewInstance : TObject;
begin
  Result := inherited NewInstance;
  TSepiInterface(Result).FSize := 4;
  TSepiInterface(Result).FNeedInit := True;
end;

{*
  Déclare un type interface en forward
  @param AOwner      Propriétaire du type
  @param ATypeName   RTTI de l'interface
*}
class function TSepiInterface.ForwardDecl(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo) : TSepiInterface;
begin
  Result := TSepiInterface(NewInstance);
  TSepiInterface(Result).TypeInfoRef^ := ATypeInfo;
  TSepiInterface(AOwner).AddForward(ATypeInfo.Name, Result);
end;

{*
  Déclare un type interface en forward
  @param AOwner   Propriétaire du type
  @param AName    Nom de l'interface
*}
class function TSepiInterface.ForwardDecl(AOwner : TSepiMeta;
  const AName : string) : TSepiInterface;
begin
  Result := TSepiInterface(NewInstance);
  TSepiInterface(AOwner).AddForward(AName, Result);
end;

{*
  Ajoute une méthode à l'interface
  @param MethodName        Nom de la méthode
  @param ASignature        Signature Delphi de la méthode
  @param ACallConvention   Convention d'appel de la méthode
*}
function TSepiInterface.AddMethod(const MethodName, ASignature : string;
  ACallConvention : TCallConvention = ccRegister) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod.Create(Self, MethodName, nil, ASignature,
    mlkInterface, False, 0, ACallConvention);
end;

{*
  Ajoute une propriété à l'interface
  @param AName           Nom de la propriété
  @param ASignature      Signature
  @param AReadAccess     Accès en lecture à la propriété (peut être vide)
  @param AWriteAccess    Accès en écriture à la propriété (peut être vide)
  @param AIndex          Index d'accès
  @param AIsDefault      Indique si c'est la propriété tableau par défaut
*}
function TSepiInterface.AddProperty(
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIndex : integer = NoIndex; AIsDefault : boolean = False) : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIndex, NoDefaultValue, AIsDefault);
end;

{*
  Ajoute une propriété à l'interface
  @param AName           Nom de la propriété
  @param ASignature      Signature
  @param AReadAccess     Accès en lecture à la propriété (peut être vide)
  @param AWriteAccess    Accès en écriture à la propriété (peut être vide)
  @param AIsDefault      Indique si c'est la propriété tableau par défaut
*}
function TSepiInterface.AddProperty(
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIsDefault : boolean) : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIsDefault);
end;

{*
  Termine l'interface et construit ses RTTI si ce n'est pas déjà fait
*}
procedure TSepiInterface.Complete;
begin
  if FCompleted then exit;

  FCompleted := True;
  if not Native then
    MakeTypeInfo;
end;

{*
  Détermine si l'interface hérite d'une interface donnée
  @param AParent   Ancêtre à tester
  @return True si l'interface hérite de AParent, False sinon
*}
function TSepiInterface.IntfInheritsFrom(AParent : TSepiInterface) : boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FParent) and FParent.IntfInheritsFrom(AParent));
end;

{*
  Recherche un membre dans l'interface
  @param MemberName   Nom du membre recherché
  @return Le membre correspondant, ou nil si non trouvé
*}
function TSepiInterface.LookForMember(const MemberName : string) : TSepiMeta;
begin
  if MemberName = '' then
  begin
    Result := nil;
    exit;
  end;

  Result := GetMeta(MemberName);
  if (Result = nil) and (Parent <> nil) then
    Result := Parent.LookForMember(MemberName);
end;

{-------------------}
{ Classe TSepiClass }
{-------------------}

{*
  Recense une classe native
*}
constructor TSepiClass.RegisterTypeInfo(AOwner : TSepiMeta;
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
constructor TSepiClass.Load(AOwner : TSepiMeta; Stream : TStream);
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
  Crée une nouvelle classe
  @param AOwner    Propriétaire du type
  @param AName     Nom du type
  @param AParent   Classe parent
*}
constructor TSepiClass.Create(AOwner : TSepiMeta; const AName : string;
  AParent : TSepiClass);
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
  Détruit l'instance
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
  @param Index   Index de l'IMT à construire (dans le tableau FInterfaces)
*}
procedure TSepiClass.MakeIMT(IntfEntry : PSepiInterfaceEntry);
const
  AdjustInstrSizes : array[TCallConvention, boolean] of ShortInt = (
    (3, 5), (5, 9), (-1, -1), (5, 8)
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

        AdjustInstrSize :=
          AdjustInstrSizes[Method.Signature.CallConvention, BigOffset];

        // Call convention Pascal not supported at the moment
        { TODO 1 -cMetaunités : Support de ccPascal dans les IMT }
        if AdjustInstrSize < 0 then
          raise ESepiUnsupportedFeatureException.CreateFmt(
            SSepiUnsupportedIntfCallConvention, ['pascal']);

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
      case Method.Signature.CallConvention of
        ccRegister :
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
        end;
        ccStdCall, ccCDecl :
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
  Range également l'adresse de cette table à l'emplacement prévu de la VMT.
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
  Range également l'adresse de cette table à l'emplacement prévu de la VMT.
*}
procedure TSepiClass.MakeInitTable;
var Fields : TObjectList;
    I : integer;
    Meta : TSepiMeta;
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
  Construit la table des champs publiés
  Range également l'adresse de cette table à l'emplacement prévu de la VMT.
*}
procedure TSepiClass.MakeFieldTable;
var Fields : TObjectList;
    I : integer;
    Meta : TSepiMeta;
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
  Construit la table des méthodes publiées
  Range également l'adresse de cette table à l'emplacement prévu de la VMT.
*}
procedure TSepiClass.MakeMethodTable;
var Methods : TObjectList;
    I : integer;
    Meta : TSepiMeta;
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
  Range également l'adresse de la DMT à l'emplacement prévu de la VMT.
*}
procedure TSepiClass.MakeDMT;
var PDMT : Pointer;
    I, Count : integer;
    Meta : TSepiMeta;
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
         (TSepiMetaMethod(Meta).LinkKind in [mlkDynamic, mlkMessage]) then
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
  for I := 0 to ChildCount-1 do if Children[I] is TSepiMetaMethod then
  begin
    Method := TSepiMetaMethod(Children[I]);
    if Method.LinkKind = mlkVirtual then
      VMTEntries[Method.VMTOffset] := Method.Code;
  end;

  // Making the other tables
  MakeIntfTable;
  MakeInitTable;
  MakeFieldTable;
  MakeMethodTable;
  MakeDMT;
end;

{*
  Nombre d'interfaces supportées
  @return Nombre d'interfaces supportées
*}
function TSepiClass.GetInterfaceCount : integer;
begin
  Result := Length(FInterfaces);
end;

{*
  Tableau zero-based des interfaces supportées
  @param Index   Index dans le tableau
  @return Interface supportée à l'index spécifié
*}
function TSepiClass.GetInterfaces(Index : integer) : TSepiInterface;
begin
  Result := FInterfaces[Index].IntfRef;
end;

{*
  VMT de la classe, indexée par les constantes vmtXXX
  @param Index   Index dans la VMT
  @return Information contenue dans la VMT à l'index spécifié
*}
function TSepiClass.GetVMTEntries(Index : integer) : Pointer;
begin
  Result := PPointer(Integer(FDelphiClass) + Index)^;
end;

{*
  Modifie la VMT de la classe, indexée par les constantes vmtXXX
  Cette méthode ne fonctionne que pour des VMT créées par Sepi. Dans le cas où
  la VMT serait créée à la compilation, cette méthode provoquera une violation
  d'accès.
  @param Index   Index dans la VMT
  @param Value   Information à stocker dans la VMT à l'index spécifié
*}
procedure TSepiClass.SetVMTEntries(Index : integer; Value : Pointer);
begin
  PPointer(Integer(FDelphiClass) + Index)^ := Value;
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.ChildAdded(Child : TSepiMeta);
begin
  inherited;

  if State = msConstructing then
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
  Crée une nouvelle instance de TSepiClass
  @return Instance créée
*}
class function TSepiClass.NewInstance : TObject;
begin
  Result := inherited NewInstance;
  TSepiClass(Result).FSize := 4;
  TSepiClass(Result).FNeedInit := False;
end;

{*
  Déclare un type classe en forward
  @param AOwner      Propriétaire du type
  @param ATypeInfo   RTTI de la classe
*}
class function TSepiClass.ForwardDecl(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo) : TSepiClass;
begin
  Result := TSepiClass(NewInstance);
  TSepiClass(Result).TypeInfoRef^ := ATypeInfo;
  TSepiClass(AOwner).AddForward(ATypeInfo.Name, Result);
end;

{*
  Déclare un type classe en forward
  @param AOwner   Propriétaire du type
  @param AName    Nom de la classe
*}
class function TSepiClass.ForwardDecl(AOwner : TSepiMeta;
  const AName : string) : TSepiClass;
begin
  Result := TSepiClass(NewInstance);
  TSepiClass(AOwner).AddForward(AName, Result);
end;

{*
  Ajoute le support d'une interface
  @param AInterface   Interface à supporter
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
  @param AIntfTypeInfo   RTTI de l'interface à supporter
*}
procedure TSepiClass.AddInterface(AIntfTypeInfo : PTypeInfo);
begin
  AddInterface(Root.FindType(AIntfTypeInfo) as TSepiInterface);
end;

{*
  Ajoute le support d'une interface
  @param AIntfName   Nom de l'interface à supporter
*}
procedure TSepiClass.AddInterface(const AIntfName : string);
begin
  AddInterface(Root.FindType(AIntfName) as TSepiInterface);
end;

{*
  Ajoute un champ à la classe
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
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
  Ajoute un champ à la classe
  @param FieldName       Nom du champ
  @param FieldTypeInto   RTTI du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiClass.AddField(const FieldName : string;
  FieldTypeInfo : PTypeInfo; ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeInfo), ForcePack);
end;

{*
  Ajoute un champ à la classe
  @param FieldName       Nom du champ
  @param FieldTypeName   Nom du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiClass.AddField(const FieldName, FieldTypeName : string;
  ForcePack : boolean = False) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeName), ForcePack);
end;

{*
  Ajoute une méthode à la classe
  @param MethodName        Nom de la méthode
  @param ACode             Pointeur sur le code de la méthode
  @param ASignature        Signature Delphi de la méthode
  @param ALinkKind         Type de liaison
  @param AAbstract         Indique si la méthode est abstraite
  @param AMsgID            Pour les méthodes de message, le message intercepté
  @param ACallConvention   Convention d'appel de la méthode
*}
function TSepiClass.AddMethod(const MethodName : string; ACode: Pointer;
  const ASignature : string; ALinkKind : TMethodLinkKind = mlkStatic;
  AAbstract : boolean = False; AMsgID : integer = 0;
  ACallConvention : TCallConvention = ccRegister) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod.Create(Self, MethodName, ACode, ASignature,
    ALinkKind, AAbstract, AMsgID, ACallConvention);
end;

{*
  Ajoute une méthode à la classe
  @param MethodName        Nom de la méthode
  @param ACode             Pointeur sur le code de la méthode
  @param ASignature        Signature Delphi de la méthode
  @param ACallConvention   Convention d'appel de la méthode
*}
function TSepiClass.AddMethod(const MethodName : string; ACode: Pointer;
  const ASignature : string;
  ACallConvention : TCallConvention) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod.Create(Self, MethodName, ACode, ASignature,
    mlkStatic, False, 0, ACallConvention);
end;

{*
  Ajoute une propriété à la classe
  @param AName           Nom de la propriété
  @param ASignature      Signature
  @param AReadAccess     Accès en lecture à la propriété (peut être vide)
  @param AWriteAccess    Accès en écriture à la propriété (peut être vide)
  @param AIndex          Index d'accès
  @param ADefaultValue   Valeur par défaut de la propriété
  @param AIsDefault      Indique si c'est la propriété tableau par défaut
*}
function TSepiClass.AddProperty(
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIndex : integer = NoIndex; ADefaultValue : integer = NoDefaultValue;
  AIsDefault : boolean = False) : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIndex, ADefaultValue, AIsDefault);
end;

{*
  Ajoute une propriété à la classe
  @param AName           Nom de la propriété
  @param ASignature      Signature
  @param AReadAccess     Accès en lecture à la propriété (peut être vide)
  @param AWriteAccess    Accès en écriture à la propriété (peut être vide)
  @param AIsDefault      Indique si c'est la propriété tableau par défaut
*}
function TSepiClass.AddProperty(
  const AName, ASignature, AReadAccess, AWriteAccess : string;
  AIsDefault : boolean) : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIsDefault);
end;

{*
  Termine la classe et construit ses RTTI si ce n'est pas déjà fait
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
  Détermine si la classe hérite d'une classe donnée
  @param AParent   Ancêtre à tester
  @return True si la classe hérite de AParent, False sinon
*}
function TSepiClass.ClassInheritsFrom(AParent : TSepiClass) : boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FParent) and FParent.ClassInheritsFrom(AParent));
end;

{*
  Recherche un membre dans la classe, en tenant compte des visibilités
  @param MemberName   Nom du membre recherché
  @param FromUnit     Unité d'où l'on cherche
  @param FromClass    Classe d'où l'on cherche (ou nil si pas de classe)
  @return Le membre correspondant, ou nil si non trouvé
*}
function TSepiClass.LookForMember(const MemberName : string;
  FromUnit : TSepiMetaUnit; FromClass : TSepiClass = nil) : TSepiMeta;
begin
  if MemberName = '' then
  begin
    Result := nil;
    exit;
  end;

  Result := GetMeta(MemberName);

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
constructor TSepiMetaClass.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FSize := 4;
  OwningUnit.ReadRef(Stream, FClass);
end;

{*
  Crée une nouvelle meta-classe
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
  @param AClass   Classe correspondante
*}
constructor TSepiMetaClass.Create(AOwner : TSepiMeta; const AName : string;
  AClass : TSepiClass; AIsNative : boolean = False);
begin
  inherited Create(AOwner, AName, tkClass);
  FSize := 4;
  FClass := AClass;

  if AIsNative then
    ForceNative;
end;

{*
  Crée une nouvelle meta-classe
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AClassInfo   RTTI de la classe correspondante
*}
constructor TSepiMetaClass.Create(AOwner : TSepiMeta; const AName : string;
  AClassInfo : PTypeInfo; AIsNative : boolean = False);
begin
  Create(AOwner, AName, AOwner.Root.FindType(AClassInfo) as TSepiClass,
    AIsNative);
end;

{*
  Crée une nouvelle meta-classe
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AClassName   Nom de la classe correspondante
*}
constructor TSepiMetaClass.Create(AOwner : TSepiMeta;
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
  Recense un type référence de méthode natif
*}
constructor TSepiMethodRefType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  FSignature := TSepiMethodSignature.RegisterTypeData(Self, TypeData);

  if Signature.Kind in mkOfObject then
    FSize := 8
  else
    FSize := 4;
end;

{*
  Charge un type référence de méthode depuis un flux
*}
constructor TSepiMethodRefType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FSignature := TSepiMethodSignature.Load(Self, Stream);
  LoadChildren(Stream);

  if Signature.Kind in mkOfObject then
    FSize := 8
  else
    FSize := 4;
end;

{*
  Crée un nouveau type référence de méthode
  @param AOwner            Propriétaire du type
  @param AName             Nom du type
  @param ASignature        Signature
  @param AOfObject         Indique s'il s'agit d'une méthode
  @param ACallConvention   Convention d'appel
*}
constructor TSepiMethodRefType.Create(AOwner : TSepiMeta;
  const AName, ASignature : string; AOfObject : boolean = False;
  ACallConvention : TCallConvention = ccRegister);
var Prefix : string;
begin
  inherited Create(AOwner, AName, tkMethod);

  if AOfObject then Prefix := '' else Prefix := 'unit ';
  FSignature := TSepiMethodSignature.Create(Self,
    Prefix + ASignature, ACallConvention);

  if Signature.Kind in mkOfObject then
    FSize := 8
  else
    FSize := 4;
end;

{*
  Détruit l'instance
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
constructor TSepiVariantType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;

  FSize := 16;
  FNeedInit := True;
end;

{*
  Charge un type variant depuis un flux
*}
constructor TSepiVariantType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo;

  FSize := 16;
  FNeedInit := True;
end;

{*
  [@inheritDoc]
*}
function TSepiVariantType.GetAlignment : integer;
begin
  Result := 8;
end;

initialization
  SepiRegisterMetaClasses([
    TSepiMetaField, TSepiMetaParam, TSepiMetaMethod, TSepiMetaOverloadedMethod,
    TSepiMetaProperty, TSepiRecordType, TSepiInterface, TSepiClass,
    TSepiMethodRefType, TSepiVariantType
  ]);
end.

