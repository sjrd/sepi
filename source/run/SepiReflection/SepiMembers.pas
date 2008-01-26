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
  Définit les classes de gestion des types composites
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
  NoIndex = Integer($80000000);

  /// Pas de valeur par défaut
  NoDefaultValue = Integer($80000000);

type
  TSepiSignature = class;

  {*
    Type de paramètre caché
    - hpNormal : paramètre normal
    - hpSelf : paramètre Self des méthodes
    - hpResult : paramètre Result des fonctions
    - hpAlloc : paramètre $Alloc des constructeurs
    - hpFree : paramètre $Free des destructeurs
    - hpOpenArrayHighValue : valeur maximale d'indice du tableau ouvert
  *}
  TSepiHiddenParamKind = (hpNormal, hpSelf, hpResult, hpAlloc, hpFree,
    hpOpenArrayHighValue);

  {*
    Type de paramètre
    - pkValue : transmis par valeur
    - pkVar : paramètre var
    - pkConst : paramètre const
    - pkOut : paramètre out
  *}
  TSepiParamKind = (pkValue, pkVar, pkConst, pkOut);

  {*
    Emplacement d'un paramètre
    - ppEAX : registre EAX
    - ppEDX : registre EDX
    - ppECX : registre ECX
    - ppStack : sur la pile
  *}
  TSepiParamPlace = (ppEAX, ppEDX, ppECX, ppStack);

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
  TCallingConvention = (ccRegister, ccCDecl, ccPascal, ccStdCall, ccSafeCall);

  {*
    Type d'accesseur d'une propriété
  *}
  TPropertyAccessKind = (pakNone, pakField, pakMethod);

  {*
    Type de stockage d'une propriété
  *}
  TPropertyStorageKind = (pskConstant, pskField, pskMethod);

  TSepiOverloadedMethod = class;

  {*
    Champ
    @author sjrd
    @version 1.0
  *}
  TSepiField = class(TSepiMeta)
  private
    FType: TSepiType; /// Type du champ
    FOffset: Integer; /// Offset
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AType: TSepiType; AOffset: Integer);

    property FieldType: TSepiType read FType;
    property Offset: Integer read FOffset;
  end;

  {*
    Informations d'appel d'un paramètre (ou et comment le passer)
    @author sjrd
    @version 1.0
  *}
  TSepiParamCallInfo = record
    ByAddress: Boolean;     /// True pour le passer par adresse
    Place: TSepiParamPlace; /// Endroit où le placer
    StackOffset: Word;      /// Offset où le placer dans la pile
    SepiStackOffset: Word;  /// Offset dans la pile pour des appels Sepi
  end;

  {*
    Paramètre de méthode
    Les paramètres cachés ont aussi leur existence en tant que TSepiMetaParam.
    Les cinq types de paramètres cachés sont Self (classe ou instance), Result
    (lorsqu'il est passé par adresse), le paramètre spécial $Alloc des
    constructeurs, le paramètre spécial $Free des destructeurs et les
    paramètres High$xxx suivant chaque paramètre de type "open array".
    @author sjrd
    @version 1.0
  *}
  TSepiParam = class(TObject)
  private
    FOwner: TSepiSignature; /// Signature propriétaire
    FName: string;          /// Nom
    FLoading: Boolean;      /// True si en chargement depuis un flux

    FHiddenKind: TSepiHiddenParamKind; /// Type de paramètre caché
    FKind: TSepiParamKind;             /// Type de paramètre
    FByRef: Boolean;                   /// True si passé par référence
    FOpenArray: Boolean;               /// True pour un tableau ouvert
    FType: TSepiType;                  /// Type du paramètre (peut être nil)

    FFlags: TParamFlags;           /// Flags du paramètre
    FCallInfo: TSepiParamCallInfo; /// Informations d'appel du paramètre

    constructor CreateHidden(AOwner: TSepiSignature;
      AHiddenKind: TSepiHiddenParamKind; AType: TSepiType = nil);
    constructor RegisterParamData(AOwner: TSepiSignature;
      var ParamData: Pointer);
    constructor Load(AOwner: TSepiSignature; Stream: TStream);
    class procedure CreateFromString(AOwner: TSepiSignature;
      const Definition: string);
    procedure MakeFlags;

    procedure ListReferences;
    procedure Save(Stream: TStream);
  public
    constructor Create(AOwner: TSepiSignature; const AName: string;
      AType: TSepiType; AKind: TSepiParamKind = pkValue;
      AOpenArray: Boolean = False);

    procedure AfterConstruction; override;

    function Equals(AParam: TSepiParam): Boolean;
    function CompatibleWith(AType: TSepiType): Boolean;

    property Owner: TSepiSignature read FOwner;
    property Name: string read FName;

    property HiddenKind: TSepiHiddenParamKind read FHiddenKind;
    property Kind: TSepiParamKind read FKind;
    property ByRef: Boolean read FByRef;
    property OpenArray: Boolean read FOpenArray;
    property ParamType: TSepiType read FType;

    property Flags: TParamFlags read FFlags;
    property CallInfo: TSepiParamCallInfo read FCallInfo;
  end;

  {*
    Signature de routine, méthode ou propriété
    @author sjrd
    @version 1.0
  *}
  TSepiSignature = class
  private
    FOwner: TSepiMeta;      /// Propriétaire de la signature
    FRoot: TSepiRoot;       /// Racine
    FOwningUnit: TSepiUnit; /// Unité contenante

    FKind: TMethodKind;                     /// Type de méthode
    FReturnType: TSepiType;                 /// Type de retour
    FCallingConvention: TCallingConvention; /// Convention d'appel

    FParams: TObjectList;       /// Paramètres déclarés (visibles)
    FActualParams: TObjectList; /// Paramètres réels

    FRegUsage: Byte;       /// Nombre de registres utilisés (entre 0 et 3)
    FStackUsage: Word;     /// Taille utilisée sur la pile (en octets)
    FSepiStackUsage: Word; /// Taille utilisée sur la pile Sepi (en octets)

    constructor BaseCreate(AOwner: TSepiMeta);

    procedure MakeCallInfo;

    function CheckInherited(ASignature: TSepiSignature): Boolean;

    function GetParamCount: Integer;
    function GetParams(Index: Integer): TSepiParam;
    function GetActualParamCount: Integer;
    function GetActualParams(Index: Integer): TSepiParam;
    function GetParamByName(const ParamName: string): TSepiParam;

    function GetHiddenParam(Kind: TSepiHiddenParamKind): TSepiParam;
  protected
    procedure ListReferences;
    procedure Save(Stream: TStream);
  public
    constructor RegisterTypeData(AOwner: TSepiMeta; ATypeData: PTypeData);
    constructor Load(AOwner: TSepiMeta; Stream: TStream);
    constructor Create(AOwner: TSepiMeta; const ASignature: string;
      ACallingConvention: TCallingConvention = ccRegister);
    destructor Destroy; override;

    function GetParam(const ParamName: string): TSepiParam;

    function Equals(ASignature: TSepiSignature): Boolean;
    function CompatibleWith(const ATypes: array of TSepiType): Boolean;

    property Owner: TSepiMeta read FOwner;
    property Root: TSepiRoot read FRoot;
    property OwningUnit: TSepiUnit read FOwningUnit;
    property Kind: TMethodKind read FKind;

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: TSepiParam read GetParams;
    property ActualParamCount: Integer read GetActualParamCount;
    property ActualParams[Index: Integer]: TSepiParam
      read GetActualParams;
    property ParamByName[const ParamName: string]: TSepiParam
      read GetParamByName; default;

    property ReturnType: TSepiType read FReturnType;
    property CallingConvention: TCallingConvention read FCallingConvention;

    property HiddenParam[Kind: TSepiHiddenParamKind]: TSepiParam
      read GetHiddenParam;
    property SelfParam: TSepiParam index hpSelf read GetHiddenParam;
    property ResultParam: TSepiParam index hpResult read GetHiddenParam;
    property AllocParam: TSepiParam index hpAlloc read GetHiddenParam;
    property FreeParam: TSepiParam index hpFree read GetHiddenParam;

    property RegUsage: Byte read FRegUsage;
    property StackUsage: Word read FStackUsage;
    property SepiStackUsage: Word read FSepiStackUsage;
  end;

  {*
    Méthode ou routine
    @author sjrd
    @version 1.0
  *}
  TSepiMethod = class(TSepiMeta)
  private
    FCode: Pointer;               /// Adresse de code natif
    FCodeJumper: TJmpInstruction; /// Jumper sur le code, si non native
    FCodeHandler: TObject;        /// Gestionnaire de code

    FSignature: TSepiSignature; /// Signature de la méthode
    FLinkKind: TMethodLinkKind; /// Type de liaison d'appel
    FFirstDeclaration: Boolean; /// Faux uniquement quand 'override'
    FAbstract: Boolean;         /// Indique si la méthode est abstraite
    FInherited: TSepiMethod;    /// Méthode héritée

    FLinkIndex: Integer; /// Offset de VMT ou index de DMT, selon la liaison

    FIsOverloaded: Boolean;             /// Indique si surchargée
    FOverloaded: TSepiOverloadedMethod; /// Méthode surchargée (ou nil)
    FOverloadIndex: Byte;               /// Index de surcharge

    procedure MakeLink;
    procedure FindNativeCode;

    function GetRealName: string;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      ACode: Pointer; const ASignature: string;
      ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
      AMsgID: Integer = 0;
      ACallingConvention: TCallingConvention = ccRegister);
    constructor CreateOverloaded(AOwner: TSepiMeta; const AName: string;
      ACode: Pointer; const ASignature: string;
      ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
      AMsgID: Integer = 0;
      ACallingConvention: TCallingConvention = ccRegister);
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure SetCode(ACode: Pointer; ACodeHandler: TObject = nil);
    procedure SetCodeMethod(const AMethod: TMethod;
      ACodeHandler: TObject = nil);

    property Code: Pointer read FCode;
    property CodeHandler: TObject read FCodeHandler;

    property Signature: TSepiSignature read FSignature;
    property LinkKind: TMethodLinkKind read FLinkKind;
    property FirstDeclaration: Boolean read FFirstDeclaration;
    property IsAbstract: Boolean read FAbstract;
    property InheritedMethod: TSepiMethod read FInherited;

    property VMTOffset: Integer read FLinkIndex;
    property DMTIndex: Integer read FLinkIndex;
    property MsgID: Integer read FLinkIndex;
    property IMTIndex: Integer read FLinkIndex;

    property IsOverloaded: Boolean read FIsOverloaded;
    property Overloaded: TSepiOverloadedMethod read FOverloaded;
    property OverloadIndex: Byte read FOverloadIndex;
    property RealName: string read GetRealName;
  end;

  {*
    Méthode ou routine surchargée
    @author sjrd
    @version 1.0
  *}
  TSepiOverloadedMethod = class(TSepiMeta)
  private
    FMethods: TObjectList; /// Liste des méthodes de même nom

    function FindInherited(
      ASignature: TSepiSignature): TSepiMethod;

    function GetMethodCount: Integer;
    function GetMethods(Index: Integer): TSepiMethod;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string);
    destructor Destroy; override;

    function FindMethod(
      ASignature: TSepiSignature): TSepiMethod; overload;
    function FindMethod(
      const ATypes: array of TSepiType): TSepiMethod; overload;

    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TSepiMethod read GetMethods;
    property DefaultMethod: TSepiMethod index 0 read GetMethods;
  end;

  {*
    Accès de propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyAccess = record
    Kind: TPropertyAccessKind;          /// Type d'accès
    case TPropertyAccessKind of
      pakNone: (Meta: TSepiMeta);       /// Meta d'accès (neutre)
      pakField: (Field: TSepiField);    /// Champ d'accès
      pakMethod: (Method: TSepiMethod); /// Méthode d'accès
  end;

  {*
    Stockage de propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyStorage = record
    Kind: TPropertyStorageKind;         /// Type de stockage
    Stored: Boolean;                    /// Constante de stockage
    case TPropertyStorageKind of
      pskConstant: (Meta: TSepiMeta);   /// Meta de stockage (neutre)
      pskField: (Field: TSepiField);    /// Champ de stockage
      pskMethod: (Method: TSepiMethod); /// Méthode de stockage
  end;

  {*
    Propriété
    @author sjrd
    @version 1.0
  *}
  TSepiProperty = class(TSepiMeta)
  private
    FSignature: TSepiSignature; /// Signature

    FReadAccess: TSepiPropertyAccess;  /// Accès en lecture
    FWriteAccess: TSepiPropertyAccess; /// Accès en écriture
    FIndex: Integer;                   /// Index d'accès

    FDefaultValue: Integer;         /// Valeur par défaut
    FStorage: TSepiPropertyStorage; /// Spécificateur de stockage

    FIsDefault: Boolean; /// Indique si c'est la propriété par défaut

    procedure MakePropInfo(PropInfo: PPropInfo);

    function GetPropType: TSepiType;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure Destroying; override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;

    constructor Create(AOwner: TSepiMeta;
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIndex: Integer = NoIndex; ADefaultValue: Integer = NoDefaultValue;
      const AStorage: string = ''; AIsDefault: Boolean = False); overload;
    constructor Create(AOwner: TSepiMeta;
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIsDefault: Boolean); overload;

    constructor Redefine(AOwner: TSepiMeta; const AName: string;
      const AReadAccess: string = ''; const AWriteAccess: string = '';
      const AStorage: string = ''); overload;
    constructor Redefine(AOwner: TSepiMeta;
      const AName, AReadAccess, AWriteAccess: string; ADefaultValue: Integer;
      const AStorage: string = ''); overload;

    destructor Destroy; override;

    property Signature: TSepiSignature read FSignature;
    property PropType: TSepiType read GetPropType;

    property ReadAccess: TSepiPropertyAccess read FReadAccess;
    property WriteAccess: TSepiPropertyAccess read FWriteAccess;
    property Index: Integer read FIndex;

    property DefaultValue: Integer read FIndex;
    property Storage: TSepiPropertyStorage read FStorage;

    property IsDefault: Boolean read FIsDefault;
  end;

  {*
    Type enregistrement
    @author sjrd
    @version 1.0
  *}
  TSepiRecordType = class(TSepiType)
  private
    FPacked: Boolean;    /// Indique si le record est packed
    FAlignment: Integer; /// Alignement
    FCompleted: Boolean; /// Indique si le record est entièrement défini

    function PrivAddField(const FieldName: string; FieldType: TSepiType;
      After: TSepiField;
      ForcePack: Boolean = False): TSepiField;

    procedure MakeTypeInfo;
  protected
    procedure ChildAdded(Child: TSepiMeta); override;

    procedure Save(Stream: TStream); override;

    function GetAlignment: Integer; override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      APacked: Boolean = False; AIsNative: Boolean = False;
      ATypeInfo: PTypeInfo = nil);

    function AddField(const FieldName: string; FieldType: TSepiType;
      ForcePack: Boolean = False): TSepiField; overload;
    function AddField(const FieldName: string; FieldTypeInfo: PTypeInfo;
      ForcePack: Boolean = False): TSepiField; overload;
    function AddField(const FieldName, FieldTypeName: string;
      ForcePack: Boolean = False): TSepiField; overload;

    function AddFieldAfter(const FieldName: string; FieldType: TSepiType;
      const After: string;
      ForcePack: Boolean = False): TSepiField; overload;
    function AddFieldAfter(const FieldName: string; FieldTypeInfo: PTypeInfo;
      const After: string;
      ForcePack: Boolean = False): TSepiField; overload;
    function AddFieldAfter(const FieldName, FieldTypeName: string;
      const After: string;
      ForcePack: Boolean = False): TSepiField; overload;

    procedure Complete;

    function CompatibleWith(AType: TSepiType): Boolean; override;

    property IsPacked: Boolean read FPacked;
    property Completed: Boolean read FCompleted;
  end;

  {*
    Interface
    @author sjrd
    @version 1.0
  *}
  TSepiInterface = class(TSepiType)
  private
    FParent: TSepiInterface; /// Interface parent (ou nil - IInterface)
    FCompleted: Boolean;
    /// Indique si l'interface est entièrement définie

    FHasGUID: Boolean;         /// Indique si l'interface possède un GUID
    FIsDispInterface: Boolean; /// Indique si c'est une disp interface
    FIsDispatch: Boolean;      /// Indique si c'est une IDispatch
    FGUID: TGUID;              /// GUID de l'interface, si elle en a un

    FIMTSize: Integer; /// Taille de l'IMT

    procedure MakeTypeInfo;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    function InternalLookFor(const Name: string; FromUnit: TSepiUnit;
      FromClass: TSepiMeta = nil): TSepiMeta; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AParent: TSepiInterface; const AGUID: TGUID;
      AIsDispInterface: Boolean = False);

    class function NewInstance: TObject; override;

    class function ForwardDecl(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo): TSepiInterface; overload;
    class function ForwardDecl(AOwner: TSepiMeta;
      const AName: string): TSepiInterface; overload;

    function AddMethod(const MethodName, ASignature: string;
      ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;

    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIndex: Integer = NoIndex;
      AIsDefault: Boolean = False): TSepiProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIsDefault: Boolean): TSepiProperty; overload;

    procedure Complete;

    function IntfInheritsFrom(AParent: TSepiInterface): Boolean;

    function LookForMember(const MemberName: string): TSepiMeta;

    property Parent: TSepiInterface read FParent;
    property Completed: Boolean read FCompleted;

    property HasGUID: Boolean read FHasGUID;
    property IsDispInterface: Boolean read FIsDispInterface;
    property IsDispatch: Boolean read FIsDispatch;
    property GUID: TGUID read FGUID;

    property IMTSize: Integer read FIMTSize;
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
    IntfRef: TSepiInterface; /// Interface implémentée
    Relocates: Pointer;      /// Thunks de relocalisation
    IMT: Pointer;            /// Interface Method Table
    Offset: Integer;         /// Offset du champ interface dans l'objet
  end;

  {*
    Classe (type objet)
    @author sjrd
    @version 1.0
  *}
  TSepiClass = class(TSepiType)
  private
    FDelphiClass: TClass; /// Classe Delphi
    FParent: TSepiClass;  /// Classe parent (nil si n'existe pas - TObject)
    FCompleted: Boolean;  /// Indique si la classe est entièrement définie

    /// Interfaces supportées par la classe
    FInterfaces: array of TSepiInterfaceEntry;

    FInstSize: Integer;     /// Taille d'une instance de la classe
    FVMTSize: Integer;      /// Taille de la VMT dans les index positifs
    FDMTNextIndex: Integer; /// Prochain index à utiliser dans la DMT

    FCurrentVisibility: TMemberVisibility; /// Visibilité courante des enfants

    procedure MakeIMT(IntfEntry: PSepiInterfaceEntry);
    procedure MakeIMTs;

    procedure MakeTypeInfo;
    procedure MakeIntfTable;
    procedure MakeInitTable;
    procedure MakeFieldTable;
    procedure MakeMethodTable;
    procedure MakeDMT;
    procedure MakeVMT;

    function GetInterfaceCount: Integer;
    function GetInterfaces(Index: Integer): TSepiInterface;

    function GetVMTEntries(Index: Integer): Pointer;
    procedure SetVMTEntries(Index: Integer; Value: Pointer);
  protected
    procedure ChildAdded(Child: TSepiMeta); override;

    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    function InternalLookFor(const Name: string; FromUnit: TSepiUnit;
      FromClass: TSepiMeta = nil): TSepiMeta; override;

    property VMTEntries[Index: Integer]: Pointer
      read GetVMTEntries write SetVMTEntries;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AParent: TSepiClass);
    destructor Destroy; override;

    class function NewInstance: TObject; override;

    class function ForwardDecl(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo): TSepiClass; overload;
    class function ForwardDecl(AOwner: TSepiMeta;
      const AName: string): TSepiClass; overload;

    procedure AddInterface(AInterface: TSepiInterface); overload;
    procedure AddInterface(AIntfTypeInfo: PTypeInfo); overload;
    procedure AddInterface(const AIntfName: string); overload;

    function AddField(const FieldName: string; FieldType: TSepiType;
      ForcePack: Boolean = False): TSepiField; overload;
    function AddField(const FieldName: string; FieldTypeInfo: PTypeInfo;
      ForcePack: Boolean = False): TSepiField; overload;
    function AddField(const FieldName, FieldTypeName: string;
      ForcePack: Boolean = False): TSepiField; overload;

    function AddMethod(const MethodName: string; ACode: Pointer;
      const ASignature: string; ALinkKind: TMethodLinkKind = mlkStatic;
      AAbstract: Boolean = False; AMsgID: Integer = 0;
      ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
      overload;
    function AddMethod(const MethodName: string; ACode: Pointer;
      const ASignature: string;
      ACallingConvention: TCallingConvention): TSepiMethod; overload;
    function AddOverloadedMethod(const MethodName: string; ACode: Pointer;
      const ASignature: string; ALinkKind: TMethodLinkKind = mlkStatic;
      AAbstract: Boolean = False; AMsgID: Integer = 0;
      ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
      overload;
    function AddOverloadedMethod(const MethodName: string; ACode: Pointer;
      const ASignature: string;
      ACallingConvention: TCallingConvention): TSepiMethod; overload;

    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIndex: Integer = NoIndex; ADefaultValue: Integer = NoDefaultValue;
      const AStorage: string = '';
      AIsDefault: Boolean = False): TSepiProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIsDefault: Boolean): TSepiProperty; overload;

    function RedefineProperty(const AName: string;
      const AReadAccess: string = ''; const AWriteAccess: string = '';
      const AStorage: string = ''): TSepiProperty; overload;
    function RedefineProperty(
      const AName, AReadAccess, AWriteAccess: string; ADefaultValue: Integer;
      const AStorage: string = ''): TSepiProperty; overload;

    procedure Complete;

    function CompatibleWith(AType: TSepiType): Boolean; override;
    function ClassInheritsFrom(AParent: TSepiClass): Boolean;

    function LookForMember(const MemberName: string; FromUnit: TSepiUnit;
      FromClass: TSepiClass = nil): TSepiMeta;

    property DelphiClass: TClass read FDelphiClass;
    property Parent: TSepiClass read FParent;
    property Completed: Boolean read FCompleted;

    property InterfaceCount: Integer read GetInterfaceCount;
    property Interfaces[Index: Integer]: TSepiInterface read GetInterfaces;

    property InstSize: Integer read FInstSize;
    property VMTSize: Integer read FVMTSize;

    property CurrentVisibility: TMemberVisibility
      read FCurrentVisibility write FCurrentVisibility;
  end;

  {*
    Meta-classe (type classe)
    @author sjrd
    @version 1.0
  *}
  TSepiMetaClass = class(TSepiType)
  private
    FClass: TSepiClass; /// Classe correspondante
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AClass: TSepiClass; AIsNative: Boolean = False); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AClassInfo: PTypeInfo; AIsNative: Boolean = False); overload;
    constructor Create(AOwner: TSepiMeta; const AName, AClassName: string;
      AIsNative: Boolean = False); overload;

    function CompatibleWith(AType: TSepiType): Boolean; override;

    property SepiClass: TSepiClass read FClass;
  end;

  {*
    Type référence de méthode
    @author sjrd
    @version 1.0
  *}
  TSepiMethodRefType = class(TSepiType)
  private
    FSignature: TSepiSignature; /// Signature
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName, ASignature: string;
      AOfObject: Boolean = False;
      ACallingConvention: TCallingConvention = ccRegister);
    destructor Destroy; override;

    function CompatibleWith(AType: TSepiType): Boolean; override;

    property Signature: TSepiSignature read FSignature;
  end;

  {*
    Type variant
    Note : il est impossible de créer un nouveau type variant.
    @author sjrd
    @version 1.0
  *}
  TSepiVariantType = class(TSepiType)
  protected
    function GetAlignment: Integer; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
  end;

const
  /// Procédure d'unité
  mkUnitProcedure = TMethodKind(Integer(High(TMethodKind))+1);
  /// Fonction d'unité
  mkUnitFunction = TMethodKind(Integer(High(TMethodKind))+2);
  /// Propriété
  mkProperty = TMethodKind(Integer(High(TMethodKind))+3);

  /// Types de méthodes d'objet
  mkOfObject = [mkProcedure..mkClassConstructor];

  /// Format du nom d'une méthode surchargée
  OverloadedNameFormat = 'OL$%s$%d';

  /// Noms des paramètres cachés
  HiddenParamNames: array[TSepiHiddenParamKind] of string = (
    '', 'Self', 'Result', '$Alloc', '$Free', 'High$'
  );

  /// Chaînes des types de paramètre
  ParamKindStrings: array[TSepiParamKind] of string = (
    '', 'var', 'const', 'out'
  );

  /// Chaînes des types de méthode
  MethodKindStrings: array[mkProcedure..mkProperty] of string = (
    'procedure', 'function', 'constructor', 'destructor',
    'class procedure', 'class function', 'class constructor',
    '', '', '', 'unit procedure', 'unit function', 'property'
  );

implementation

type
  TInitInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: Cardinal;
  end;

  PInitTable = ^TInitTable;
  TInitTable = packed record
    Size: Cardinal;
    Count: Cardinal;
    Fields: array[0..0] of TInitInfo;
  end;

  PFieldInfo = ^TFieldInfo;
  TFieldInfo = packed record
    Offset: Cardinal;
    Index: Word;
    Name: ShortString;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    FieldCount: Word;
    Unknown: Pointer;
    {Fields : array[1..FieldCount] of TFieldInfo;}
  end;

  PMethodInfo = ^TMethodInfo;
  TMethodInfo = packed record
    Size: Word;
    Code: Pointer;
    Name: ShortString;
  end;

  PMethodTable = ^TMethodTable;
  TMethodTable = packed record
    MethodCount: Word;
    {Methods : array[1..MethodCount] of TMethodInfo;}
  end;

const
  // Tailles de structure TTypeData en fonction des types
  RecordTypeDataLengthBase = 2*SizeOf(Cardinal);
  IntfTypeDataLengthBase =
    SizeOf(Pointer) + SizeOf(TIntfFlagsBase) + SizeOf(TGUID) + 2*SizeOf(Word);
  ClassTypeDataLengthBase =
    SizeOf(TClass) + SizeOf(Pointer) + SizeOf(Smallint) + SizeOf(Word);
  PropInfoLengthBase = SizeOf(TPropInfo) - SizeOf(ShortString);
  InitTableLengthBase = 2*SizeOf(Byte) + 2*SizeOf(Cardinal);
  FieldTableLengthBase = SizeOf(TFieldTable);
  MethodTableLengthBase = SizeOf(TMethodTable);

  vmtMinIndex = vmtSelfPtr;
  vmtMinMethodIndex = vmtParent + 4;

procedure MakePropertyAccessKind(var Access: TSepiPropertyAccess);
begin
  with Access do
  begin
    if Meta is TSepiField then
      Kind := pakField
    else if Meta is TSepiMethod then
      Kind := pakMethod
    else
      Kind := pakNone;
  end;
end;

procedure MakePropertyStorage(out Storage: TSepiPropertyStorage;
  const AStorage: string; Owner: TSepiClass);
begin
  if (AStorage = '') or AnsiSameText(AStorage, BooleanIdents[True]) then
  begin
    Storage.Kind := pskConstant;
    Storage.Stored := True;
    Storage.Meta := nil;
  end else if AnsiSameText(AStorage, BooleanIdents[False]) then
  begin
    Storage.Kind := pskConstant;
    Storage.Stored := False;
    Storage.Meta := nil;
  end else
  begin
    Storage.Stored := False;
    Storage.Meta := Owner.LookForMember(AStorage, Owner.OwningUnit, Owner);

    if Storage.Meta is TSepiField then
      Storage.Kind := pskField
    else
      Storage.Kind := pskMethod;
  end;
end;

{-------------------}
{ Classe TSepiField }
{-------------------}

{*
  Charge un champ depuis un flux
*}
constructor TSepiField.Load(AOwner: TSepiMeta; Stream: TStream);
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
constructor TSepiField.Create(AOwner: TSepiMeta; const AName: string;
  AType: TSepiType; AOffset: Integer);
begin
  inherited Create(AOwner, AName);
  FType := AType;
  FOffset := AOffset;
end;

{*
  [@inheritDoc]
*}
procedure TSepiField.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiField.Save(Stream: TStream);
begin
  inherited;
  OwningUnit.WriteRef(Stream, FType);
  Stream.WriteBuffer(FOffset, 4);
end;

{-------------------}
{ Classe TSepiParam }
{-------------------}

{*
  Crée un paramètre caché
  @param AOwner        Propriétaire du paramètre
  @param AHiddenKind   Type de paramètre caché (doit être différent de pkNormal)
  @param AType         Type du paramètre (uniquement pour AKind = pkResult)
*}
constructor TSepiParam.CreateHidden(AOwner: TSepiSignature;
  AHiddenKind: TSepiHiddenParamKind; AType: TSepiType = nil);
begin
  Assert(AHiddenKind <> hpNormal);
  inherited Create;

  FOwner := AOwner;
  FName := HiddenParamNames[AHiddenKind];
  if AHiddenKind = hpOpenArrayHighValue then
    FName := FName + AOwner.ActualParams[AOwner.ActualParamCount-1].Name;
  FLoading := False;

  FHiddenKind := AHiddenKind;
  FKind := pkValue;
  FByRef := False;
  FOpenArray := False;

  case HiddenKind of
    hpSelf:
    begin
      // Signature.Method.Class
      if Owner.Owner.Owner is TSepiClass then
        FType := TSepiClass(Owner.Owner.Owner)
      else
        FType := Owner.Root.FindType(TypeInfo(TObject));
    end;
    hpResult:
    begin
      FKind := pkOut;
      FByRef := True;
      FType := AType;
    end;
    hpOpenArrayHighValue: FType := Owner.Root.FindType(TypeInfo(Smallint));
  else
    FType := Owner.Root.FindType(TypeInfo(Boolean));
  end;

  MakeFlags;
end;

{*
  Crée un paramètre depuis les données de type d'une référence de méthode
  En sortie, le pointeur ParamData a avancé jusqu'au paramètre suivant
  @param AOwner      Propriétaire du paramètre
  @param ParamData   Pointeur vers les données du paramètre
*}
constructor TSepiParam.RegisterParamData(AOwner: TSepiSignature;
  var ParamData: Pointer);
var
  AFlags: TParamFlags;
  AName, ATypeStr: string;
  AKind: TSepiParamKind;
  AOpenArray: Boolean;
begin
  FHiddenKind := hpNormal;
  AFlags := TParamFlags(ParamData^);
  Inc(Longint(ParamData), SizeOf(TParamFlags));
  AName := PShortString(ParamData)^;
  Inc(Longint(ParamData), PByte(ParamData)^ + 1);
  ATypeStr := PShortString(ParamData)^;
  Inc(Longint(ParamData), PByte(ParamData)^ + 1);

  if pfVar in AFlags then
    AKind := pkVar
  else if pfConst in AFlags then
    AKind := pkConst
  else if pfOut in AFlags then
    AKind := pkOut
  else
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
    Owner.Owner.GetFullName+'.'+Name]));
end;

{*
  Charge un paramètre depuis un flux
*}
constructor TSepiParam.Load(AOwner: TSepiSignature; Stream: TStream);
begin
  inherited Create;

  FOwner := AOwner;
  FName := ReadStrFromStream(Stream);
  FLoading := True;

  Stream.ReadBuffer(FHiddenKind, 1);
  Stream.ReadBuffer(FKind, 1);
  FByRef := FKind in [pkVar, pkOut];
  Stream.ReadBuffer(FOpenArray, 1);
  Owner.OwningUnit.ReadRef(Stream, FType);

  MakeFlags;
  Stream.ReadBuffer(FCallInfo, SizeOf(TSepiParamCallInfo));
end;

{*
  Crée un ou plusieurs paramètre(s) depuis sa définition Delphi
  @param AOwner       Propriétaire du ou des paramètre(s)
  @param Definition   Définition Delphi du ou des paramètre(s)
*}
class procedure TSepiParam.CreateFromString(AOwner: TSepiSignature;
  const Definition: string);
var
  NamePart, NamePart2, TypePart, KindStr, AName, ATypeStr: string;
  AKind: TSepiParamKind;
  AOpenArray: Boolean;
  AType: TSepiType;
begin
  if not SplitToken(Definition, ':', NamePart, TypePart) then
    TypePart := '';
  TypePart := GetFirstToken(TypePart, '=');

  // Name part - before the colon (:)
  if SplitToken(Trim(NamePart), ' ', KindStr, NamePart2) then
  begin
    Shortint(AKind) := AnsiIndexText(KindStr, ParamKindStrings);
    if Shortint(AKind) < 0 then
    begin
      AKind := pkValue;
      NamePart2 := KindStr + ' ' + NamePart2;
    end;
  end else
    AKind := pkValue;
  NamePart := NamePart2;

  // Type part - after the colon (:)
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

  // Création du (ou des) paramètre(s)
  while SplitToken(NamePart, ',', AName, NamePart2) do
  begin
    Create(AOwner, Trim(AName), AType, AKind, AOpenArray);
    NamePart := NamePart2;
  end;
  Create(AOwner, Trim(AName), AType, AKind, AOpenArray);
end;

{*
  Crée un nouveau paramètre
  @param AOwner       Propriétaire du paramètre
  @param AName        Nom du paramètre
  @param AType        Type du paramètre
  @param AKind        Type de paramètre
  @param AOpenArray   True pour un paramètre tableau ouvert
*}
constructor TSepiParam.Create(AOwner: TSepiSignature; const AName: string;
  AType: TSepiType; AKind: TSepiParamKind = pkValue;
  AOpenArray: Boolean = False);
begin
  inherited Create;

  FOwner := AOwner;
  FName := AName;
  FLoading := False;

  FHiddenKind := hpNormal;
  FKind := AKind;
  FByRef := FKind in [pkVar, pkOut];
  FOpenArray := AOpenArray;
  FType := AType;

  MakeFlags;
end;

{*
  Construit la propriété Flags
*}
procedure TSepiParam.MakeFlags;
begin
  // Param kind flag
  case FKind of
    pkValue: FFlags := [];
    pkVar:   FFlags := [pfVar];
    pkConst: FFlags := [pfConst];
    pkOut:   FFlags := [pfOut];
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
procedure TSepiParam.ListReferences;
begin
  Owner.OwningUnit.AddRef(FType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiParam.Save(Stream: TStream);
begin
  WriteStrToStream(Stream, Name);

  Stream.WriteBuffer(FHiddenKind, 1);
  Stream.WriteBuffer(FKind, 1);
  Stream.WriteBuffer(FOpenArray, 1);
  Owner.OwningUnit.WriteRef(Stream, FType);

  Stream.WriteBuffer(FCallInfo, SizeOf(TSepiParamCallInfo));
end;

{*
  [@inheritDoc]
*}
procedure TSepiParam.AfterConstruction;
begin
  inherited;

  FOwner.FActualParams.Add(Self);
  if HiddenKind = hpNormal then
    FOwner.FParams.Add(Self);

  if OpenArray and (not FLoading) then
    TSepiParam.CreateHidden(Owner, hpOpenArrayHighValue);
  FLoading := False;
end;

{*
  Détermine si deux paramètres sont identiques
  @param AParam   Paramètre à comparer
  @return True si les paramètres sont identiques, False sinon
*}
function TSepiParam.Equals(AParam: TSepiParam): Boolean;
begin
  Result := HiddenKind = AParam.HiddenKind;
  if Result and (HiddenKind = hpNormal) then
    Result := (Kind = AParam.Kind) and (OpenArray = AParam.OpenArray) and
      (ParamType = AParam.ParamType);
end;

{*
  Détermine si un type est compatible avec le paramètre
  @param AType   Type à Tester
  @return True si le type est compatible, False sinon
*}
function TSepiParam.CompatibleWith(AType: TSepiType): Boolean;
begin
  if ByRef then
    Result := FType = AType
  else
    Result := FType.CompatibleWith(AType);
end;

{-----------------------------}
{ Classe TSepiSignature }
{-----------------------------}

{*
  Constructeur de base
  @param AOwner   Propriétaire de la signature
*}
constructor TSepiSignature.BaseCreate(AOwner: TSepiMeta);
begin
  inherited Create;

  FOwner := AOwner;
  FRoot := FOwner.Root;
  FOwningUnit := FOwner.OwningUnit;

  FParams := TObjectList.Create(False);
  FActualParams := TObjectList.Create;
end;

{*
  Crée une signature à partir des données de type d'un type méthode
  @param AOwner      Propriétaire de la signature
  @param ATypeData   Données de type
*}
constructor TSepiSignature.RegisterTypeData(AOwner: TSepiMeta;
  ATypeData: PTypeData);
var
  ParamData: Pointer;
  I: Integer;
begin
  BaseCreate(AOwner);

  FKind := ATypeData.MethodKind;
  ParamData := @ATypeData.ParamList;

  TSepiParam.CreateHidden(Self, hpSelf);
  for I := 1 to ATypeData.ParamCount do
    TSepiParam.RegisterParamData(Self, ParamData);

  FCallingConvention := ccRegister;

  if Kind in [mkFunction, mkClassFunction] then
  begin
    FReturnType := Root.FindType(PShortString(ParamData)^);
    if ReturnType.ResultBehavior = rbParameter then
      TSepiParam.CreateHidden(Self, hpResult, ReturnType);
  end else
    FReturnType := nil;

  MakeCallInfo;
end;

{*
  Charge une signature depuis un flux
  @param AOwner   Propriétaire de la signature
  @param Stream   Flux depuis lequel charger la signature
*}
constructor TSepiSignature.Load(AOwner: TSepiMeta; Stream: TStream);
var
  Count: Integer;
begin
  BaseCreate(AOwner);

  Stream.ReadBuffer(FKind, 1);
  FOwner.OwningUnit.ReadRef(Stream, FReturnType);
  Stream.ReadBuffer(FCallingConvention, 1);
  Stream.ReadBuffer(FRegUsage, 1);
  Stream.ReadBuffer(FStackUsage, 2);
  Stream.ReadBuffer(FSepiStackUsage, 2);

  Stream.ReadBuffer(Count, 4);
  while Count > 0 do
  begin
    TSepiParam.Load(Self, Stream);
    Dec(Count);
  end;
end;

{*
  Crée une signature de méthode
  @param AOwner               Propriétaire de la signature
  @param ASignature           Signature Delphi
  @param ACallingConvention   Convention d'appel
*}
constructor TSepiSignature.Create(AOwner: TSepiMeta;
  const ASignature: string;
  ACallingConvention: TCallingConvention = ccRegister);

  function MultiPos(const Chars: array of Char; const Str: string;
    Offset: Integer = 1): Integer;
  var
    I: Integer;
  begin
    for I := Low(Chars) to High(Chars) do
    begin
      Result := PosEx(Chars[I], Str, Offset);
      if Result > 0 then
        Exit;
    end;
    Result := Length(Str)+1;
  end;

var
  ParamPos, ReturnTypePos, ParamEnd: Integer;
begin
  BaseCreate(AOwner);

  FKind := mkProcedure;
  FReturnType := nil;
  FCallingConvention := ACallingConvention;

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
  end else
    ReturnTypePos := Length(ASignature);

  // Paramètres
  if (Kind in mkOfObject) and (CallingConvention <> ccPascal) then
    TSepiParam.CreateHidden(Self, hpSelf);

  if Kind = mkConstructor then
    TSepiParam.CreateHidden(Self, hpAlloc)
  else if Kind = mkDestructor then
    TSepiParam.CreateHidden(Self, hpFree);

  if ParamPos < ReturnTypePos then
  begin
    while not (ASignature[ParamPos] in [')', ']', ':']) do
    begin
      Inc(ParamPos);
      ParamEnd := MultiPos([';', ')', ']', ':'], ASignature, ParamPos);
      TSepiParam.CreateFromString(Self,
        Copy(ASignature, ParamPos, ParamEnd-ParamPos));
      ParamPos := ParamEnd;
    end;
  end;

  if (ReturnType <> nil) and (ReturnType.ResultBehavior = rbParameter) then
    TSepiParam.CreateHidden(Self, hpResult, ReturnType);

  if (Kind in mkOfObject) and (CallingConvention = ccPascal) then
    TSepiParam.CreateHidden(Self, hpSelf);

  MakeCallInfo;
end;

{*
  [@inheritDoc]
*}
destructor TSepiSignature.Destroy;
begin
  FParams.Free;
  FActualParams.Free;

  inherited;
end;

{*
  Construit les informations d'appel de la signature
*}
procedure TSepiSignature.MakeCallInfo;
var
  I, ParamCount: Integer;
  Param: TSepiParam;
begin
  ParamCount := ActualParamCount;

  // First pass: by address vs by value and register vs stack
  FRegUsage := 0;
  for I := 0 to ParamCount-1 do
  begin
    Param := ActualParams[I];
    with Param.ParamType, Param, FCallInfo do
    begin
      // By address or by value?
      ByAddress := ByRef or (pfReference in Flags) or (ParamType = nil);

      // In a register or in the stack?
      if (CallingConvention = ccRegister) and (FRegUsage < 3) and
        (ByAddress or (not ParamBehavior.AlwaysByStack)) then
      begin
        Byte(Place) := FRegUsage;
        Inc(FRegUsage);
      end else
        Place := ppStack;
    end;
  end;

  // Second pass: compute stack offsets and stack usage
  FStackUsage := 0;
  for I := 0 to ParamCount-1 do
  begin
    // register and pascal calling conventions work in the opposite direction
    if CallingConvention in [ccRegister, ccPascal] then
      Param := ActualParams[ParamCount-I-1]
    else
      Param := ActualParams[I];

    with Param.ParamType, Param, FCallInfo do
    begin
      // Set stack offset and increment stack usage
      if Place = ppStack then
      begin
        StackOffset := FStackUsage;
        SepiStackOffset := FStackUsage;
        if ByAddress then
          Inc(FStackUsage, 4)
        else
        begin
          Inc(FStackUsage, Size);
          if FStackUsage mod 4 <> 0 then
            FStackUsage := (FStackUsage and $FFFC) + 4;
        end;
      end;
    end;
  end;

  // Third pass: adapt SepiStackOffset for register methods
  FSepiStackUsage := FStackUsage;
  if CallingConvention = ccRegister then
  begin
    Inc(FSepiStackUsage, 4*RegUsage);

    for I := 0 to ParamCount-1 do
    begin
      with ActualParams[I].FCallInfo do
      begin
        if Place = ppStack then
          Inc(SepiStackOffset, 4*RegUsage)
        else
          SepiStackOffset := 4*Ord(Place);
      end;
    end;
  end;
end;

{*
  Vérifie que la signature peut hériter d'une autre
  CheckInherited met à jour la convention d'appel si besoin est, contrairement
  à Equals.
  @param ASignature   Signature à comparer
  @return True si les signatures sont identiques, False sinon
*}
function TSepiSignature.CheckInherited(
  ASignature: TSepiSignature): Boolean;
var
  OldCallingConv: TCallingConvention;
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
  Nombre de paramètres déclarés (visibles)
  @return Nombre de paramètres
*}
function TSepiSignature.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

{*
  Tableau zero-based des paramètres déclarés (visibles)
  @param Index   Index du paramètre à récupérer
  @return Paramètre situé à l'index Index
*}
function TSepiSignature.GetParams(Index: Integer): TSepiParam;
begin
  Result := TSepiParam(FParams[Index]);
end;

{*
  Nombre de paramètres réels (incluant les paramètres cachés)
  @return Nombre de paramètres réels
*}
function TSepiSignature.GetActualParamCount: Integer;
begin
  Result := FActualParams.Count;
end;

{*
  Tableau zero-based des paramètres réels (incluant les paramètres cachés)
  @param Index   Index du paramètre à récupérer
  @return Paramètre situé à l'index Index
*}
function TSepiSignature.GetActualParams(Index: Integer): TSepiParam;
begin
  Result := TSepiParam(FActualParams[Index]);
end;

{*
  Tableau des paramètres indexés par leurs noms
  @param ParamName   Nom du paramètre recherché
  @return Paramètre dont le nom est ParamName
  @throws ESepiMetaNotFoundError Le paramètre n'a pas été trouvé
*}
function TSepiSignature.GetParamByName(const ParamName: string): TSepiParam;
begin
  Result := GetParam(ParamName);
  if Result = nil then
    raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound, [ParamName]);
end;

{*
  Paramètres cachés d'après leur type
  @param Kind   Type de paramètre caché
  @return Le paramètre caché correspondant, ou nil s'il n'y en a pas
*}
function TSepiSignature.GetHiddenParam(
  Kind: TSepiHiddenParamKind): TSepiParam;
var
  I: Integer;
begin
  Assert(Kind in [hpSelf, hpResult, hpAlloc, hpFree]);

  for I := 0 to ActualParamCount-1 do
  begin
    Result := ActualParams[I];
    if Result.HiddenKind = Kind then
      Exit;
  end;

  Result := nil;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSignature.ListReferences;
var
  I: Integer;
begin
  for I := 0 to ActualParamCount-1 do
    ActualParams[I].ListReferences;
  Owner.OwningUnit.AddRef(FReturnType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSignature.Save(Stream: TStream);
var
  I, Count: Integer;
begin
  Stream.WriteBuffer(FKind, 1);
  Owner.OwningUnit.WriteRef(Stream, FReturnType);
  Stream.WriteBuffer(FCallingConvention, 1);
  Stream.WriteBuffer(FRegUsage, 1);
  Stream.WriteBuffer(FStackUsage, 2);
  Stream.WriteBuffer(FSepiStackUsage, 2);

  Count := ActualParamCount;
  Stream.WriteBuffer(Count, 4);
  for I := 0 to Count-1 do
    ActualParams[I].Save(Stream);
end;

{*
  Cherche un paramètre par son nom
  @param ParamName   Nom du paramètre
  @return Paramètre correspondant, ou nil si non trouvé
*}
function TSepiSignature.GetParam(const ParamName: string): TSepiParam;
var
  I: Integer;
begin
  for I := 0 to FActualParams.Count-1 do
  begin
    Result := TSepiParam(FActualParams[I]);
    if AnsiSameText(Result.Name, ParamName) then
      Exit;
  end;

  Result := nil;
end;

{*
  Détermine si deux signatures sont identiques
  @param ASignature   Signature à comparer
  @return True si les signatures sont identiques, False sinon
*}
function TSepiSignature.Equals(
  ASignature: TSepiSignature): Boolean;
var
  I: Integer;
begin
  Result := False;

  if Kind <> ASignature.Kind then
    Exit;
  if CallingConvention <> ASignature.CallingConvention then
    Exit;

  if ParamCount <> ASignature.ParamCount then
    Exit;
  for I := 0 to ParamCount-1 do
    if not Params[I].Equals(ASignature.Params[I]) then
      Exit;
  if ReturnType <> ASignature.ReturnType then
    Exit;

  Result := True;
end;

{*
  Détermine si une liste de types est compatible avec la signature
  @param ATypes   Liste des types à tester
  @return True si la liste de types est compatible, False sinon
*}
function TSepiSignature.CompatibleWith(
  const ATypes: array of TSepiType): Boolean;
var
  I: Integer;
begin
  Result := False;

  if ParamCount <> Length(ATypes) then
    Exit;
  for I := 0 to ParamCount-1 do
    if not Params[I].CompatibleWith(ATypes[Low(ATypes)+I]) then
      Exit;

  Result := True;
end;

{--------------------}
{ Classe TSepiMethod }
{--------------------}

{*
  Charge une meta-méthode depuis un flux
*}
constructor TSepiMethod.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  FCode := nil;
  FCodeHandler := nil;
  
  FSignature := TSepiSignature.Load(Self, Stream);
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
      OwningUnit.OnGetMethodCode(Self, FCode, FCodeHandler);
    if not Assigned(FCode) then
      FCode := @FCodeJumper;
  end;
end;

{*
  Crée une nouvelle meta-méthode
  @param AOwner       Propriétaire de la méthode
  @param AName        Nom de la méthode
  @param ACode        Pointeur sur le code de la méthode
  @param ASignature   Signature Delphi de la méthode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la méthode est abstraite
  @param AMsgID       Pour les méthodes de message, le message intercepté
  @param ACallingConvention   Convention d'appel de la méthode
*}
constructor TSepiMethod.Create(AOwner: TSepiMeta; const AName: string;
  ACode: Pointer; const ASignature: string;
  ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
  AMsgID: Integer = 0; ACallingConvention: TCallingConvention = ccRegister);
var
  SignPrefix: string;
begin
  inherited Create(AOwner, AName);

  if Owner is TSepiUnit then
    SignPrefix := 'unit '
  else
    SignPrefix := '';

  FCode := ACode;
  FCodeHandler := nil;

  FSignature := TSepiSignature.Create(Self,
    SignPrefix + ASignature, ACallingConvention);
  FLinkKind := ALinkKind;
  FFirstDeclaration := FLinkKind <> mlkOverride;
  FAbstract := AAbstract and (FLinkKind in [mlkVirtual, mlkDynamic]);
  FLinkIndex := AMsgID; // only for messages

  MakeLink;

  if (not IsAbstract) and (not Assigned(FCode)) then
  begin
    if (Owner is TSepiClass) and TSepiClass(Owner).Native then
      FindNativeCode
    else
      FCode := @FCodeJumper;
  end;
end;

{*
  Crée une nouvelle meta-méthode surchargée
  @param AOwner       Propriétaire de la méthode
  @param AName        Nom de la méthode
  @param ACode        Pointeur sur le code de la méthode
  @param ASignature   Signature Delphi de la méthode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la méthode est abstraite
  @param AMsgID       Pour les méthodes de message, le message intercepté
  @param ACallingConvention   Convention d'appel de la méthode
*}
constructor TSepiMethod.CreateOverloaded(AOwner: TSepiMeta;
  const AName: string; ACode: Pointer; const ASignature: string;
  ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
  AMsgID: Integer = 0; ACallingConvention: TCallingConvention = ccRegister);
begin
  FIsOverloaded := True;
  FOverloaded := AOwner.GetMeta(AName) as TSepiOverloadedMethod;

  if FOverloaded = nil then
    FOverloaded := TSepiOverloadedMethod.Create(AOwner, AName);
  FOverloadIndex := FOverloaded.MethodCount;

  FOverloaded.FMethods.Add(Self);

  Create(AOwner, Format(OverloadedNameFormat, [AName, FOverloadIndex]),
    ACode, ASignature, ALinkKind, AAbstract, AMsgID, ACallingConvention);
end;

{*
  Détruit l'instance
*}
destructor TSepiMethod.Destroy;
begin
  FSignature.Free;
  inherited Destroy;
end;

{*
  Met au point la liaison et l'index de liaison
  Par extension, recherche aussi la méthode héritée.
*}
procedure TSepiMethod.MakeLink;
var
  OwningClass, Ancestor: TSepiClass;
  Meta: TSepiMeta;
  I: Integer;
begin
  FInherited := nil;

  // If owner is an interface, always an IMT index
  if Owner is TSepiInterface then
  begin
    FLinkKind := mlkInterface;
    FLinkIndex := TSepiInterface(Owner).FIMTSize;
    Inc(TSepiInterface(Owner).FIMTSize, 4);
    Exit;
  end;

  // If not a method, then nothing to do, but be sure link kind is static
  if not (Owner is TSepiClass) then
  begin
    FLinkKind := mlkStatic;
    FLinkIndex := 0;
    Exit;
  end;

  OwningClass := TSepiClass(Owner);

  if OwningClass.Parent <> nil then
  begin
    if FLinkKind <> mlkMessage then
    begin
      // Looking for the inherited method
      Meta := OwningClass.Parent.LookForMember(RealName,
        OwningUnit, OwningClass);
      if Meta is TSepiMethod then
      begin
        if Signature.CheckInherited(TSepiMethod(Meta).Signature) then
          FInherited := TSepiMethod(Meta);
      end else if Meta is TSepiOverloadedMethod then
        FInherited := TSepiOverloadedMethod(Meta).FindInherited(Signature);
    end else
    begin
      // Looking for an ancestor method which intercepts the same message ID
      Ancestor := OwningClass.Parent;
      while (FInherited = nil) and (Ancestor <> nil) do
      begin
        for I := 0 to Ancestor.ChildCount-1 do
        begin
          Meta := Ancestor.Children[I];
          if (Meta is TSepiMethod) and
            (TSepiMethod(Meta).LinkKind = mlkMessage) and
            (TSepiMethod(Meta).MsgID = FLinkIndex) then
          begin
            FInherited := TSepiMethod(Meta);
            Break;
          end;
        end;

        Ancestor := Ancestor.Parent;
      end;
    end;
  end;

  // Setting up link kind and index
  case FLinkKind of
    mlkStatic: FLinkIndex := 0;
    mlkVirtual:
    begin
      FLinkIndex := OwningClass.FVMTSize;
      Inc(OwningClass.FVMTSize, 4);
    end;
    mlkDynamic:
    begin
      FLinkIndex := OwningClass.FDMTNextIndex;
      Dec(OwningClass.FDMTNextIndex);
    end;
    mlkMessage: ; // nothing to do, FLinkIndex already set
    mlkOverride:
    begin
      FLinkKind := FInherited.LinkKind;
      FLinkIndex := FInherited.FLinkIndex;
    end;
  end;
end;

{*
  Cherche le code d'une méthode native
*}
procedure TSepiMethod.FindNativeCode;
begin
  case LinkKind of
    mlkVirtual: FCode := TSepiClass(Owner).VMTEntries[VMTOffset];
    mlkDynamic, mlkMessage:
    begin
      try
        FCode := GetClassDynamicCode(TSepiClass(Owner).DelphiClass, DMTIndex);
      except
        on Error: EAbstractError do;
      end;
    end;
  end;
end;

{*
  Nom réel, tel que déclaré (indépendant d'une surcharge ou non)
  @return Nom réel
*}
function TSepiMethod.GetRealName: string;
begin
  if IsOverloaded then
    Result := Overloaded.Name
  else
    Result := Name;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethod.ListReferences;
begin
  inherited;
  Signature.ListReferences;
  OwningUnit.AddRef(FOverloaded);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethod.Save(Stream: TStream);
var
  ALinkKind: TMethodLinkKind;
begin
  inherited;

  Signature.Save(Stream);

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
procedure TSepiMethod.AfterConstruction;
begin
  inherited;
  if IsOverloaded then
    Overloaded.FMethods.Add(Self);
end;

{*
  Donne l'adresse de début du code de la méthode
  Cette méthode ne peut être appelée qu'une seule fois par méthode, et
  seulement pour les méthodes non natives.
  @param ACode          Adresse de code
  @param ACodeHandler   Gestionnaire de code
*}
procedure TSepiMethod.SetCode(ACode: Pointer; ACodeHandler: TObject = nil);
begin
  Assert(FCode = @FCodeJumper);
  Assert(FCodeJumper.OpCode = 0);
  MakeJmp(FCodeJumper, ACode);
  FCodeHandler := ACodeHandler;
end;

{*
  Donne l'adresse de début du code de la méthode, à partir d'une méthode
  Cette méthode ne peut être appelée qu'une seule fois par méthode, et
  seulement pour les méthodes non natives.
  @param AMethod        Méthode de code
  @param ACodeHandler   Gestionnaire de code
*}
procedure TSepiMethod.SetCodeMethod(const AMethod: TMethod;
  ACodeHandler: TObject = nil);
var
  I, MoveStackCount: Integer;
  ACode: Pointer;
begin
  with Signature do
  begin
    case CallingConvention of
      ccRegister:
      begin
        if RegUsage < 3 then
          MoveStackCount := 0
        else
        begin
          // MoveStackCount = (last param before ECX).StackOffset div 4
          MoveStackCount := StackUsage div 4;
          for I := 0 to ActualParamCount-1 do
          begin
            with ActualParams[I] do
            begin
              case CallInfo.Place of
                ppECX: Break;
                ppStack: MoveStackCount := CallInfo.StackOffset div 4;
              end;
            end;
          end;
        end;

        ACode := MakeProcOfRegisterMethod(AMethod, RegUsage, MoveStackCount);
      end;

      ccCDecl: ACode := MakeProcOfCDeclMethod(AMethod);
      ccPascal: ACode := MakeProcOfPascalMethod(AMethod);
    else
      ACode := MakeProcOfStdCallMethod(AMethod);
    end;
  end;

  AddPtrResource(ACode);
  SetCode(ACode, ACodeHandler);
end;

{------------------------------}
{ Classe TSepiOverloadedMethod }
{------------------------------}

{*
  Charge une méthode surchargée depuis un flux
*}
constructor TSepiOverloadedMethod.Load(AOwner: TSepiMeta;
  Stream: TStream);
begin
  inherited;
  FMethods := TObjectList.Create(False);
end;

{*
  Crée une nouvelle méthode surchargée
  @param AOwner   Propriétaire de la méthode
  @param AName    Nom de la méthode
*}
constructor TSepiOverloadedMethod.Create(AOwner: TSepiMeta;
  const AName: string);
begin
  inherited Create(AOwner, AName);
  FMethods := TObjectList.Create(False);
end;

{*
  [@inheritDoc]
*}
destructor TSepiOverloadedMethod.Destroy;
begin
  FMethods.Free;
  inherited;
end;

{*
  Trouve la méthode effective dont une signature donnée hérite
  @param ASignature   Signature à rechercher
  @return Méthode effective correspondante
*}
function TSepiOverloadedMethod.FindInherited(
  ASignature: TSepiSignature): TSepiMethod;
var
  I: Integer;
begin
  for I := 0 to MethodCount-1 do
  begin
    Result := Methods[I];
    if ASignature.CheckInherited(Result.Signature) then
      Exit;
  end;
  Result := nil;
end;

{*
  Nombre de méthodes de même nom
  @return Nombre de méthodes de même nom
*}
function TSepiOverloadedMethod.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
end;

{*
  Tableau zero-based des méthodes de même nom
  @param Index   Index de la méthode
  @return Méthode de même nom à l'index spécifié
*}
function TSepiOverloadedMethod.GetMethods(
  Index: Integer): TSepiMethod;
begin
  Result := TSepiMethod(FMethods[Index]);
end;

{*
  Trouve la méthode effective qui correspond à une signature donnée
  @param ASignature   Signature à rechercher
  @return Méthode effective correspondante
*}
function TSepiOverloadedMethod.FindMethod(
  ASignature: TSepiSignature): TSepiMethod;
var
  I: Integer;
begin
  for I := 0 to MethodCount-1 do
  begin
    Result := Methods[I];
    if Result.Signature.Equals(ASignature) then
      Exit;
  end;
  Result := nil;
end;

{*
  Trouve la méthode effective qui correspond à une liste de types de paramètres
  @param ATypes   Liste des types de paramètres
  @return Méthode effective correspondante
*}
function TSepiOverloadedMethod.FindMethod(
  const ATypes: array of TSepiType): TSepiMethod;
var
  I: Integer;
begin
  for I := 0 to MethodCount-1 do
  begin
    Result := Methods[I];
    if Result.Signature.CompatibleWith(ATypes) then
      Exit;
  end;
  Result := nil;
end;

{----------------------}
{ Classe TSepiProperty }
{----------------------}

{*
  Charge une propriété depuis un flux
*}
constructor TSepiProperty.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  FSignature := TSepiSignature.Load(Self, Stream);
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
  @param AStorage        Spécificateur de stockage
  @param AIsDefault      Indique si c'est la propriété tableau par défaut
*}
constructor TSepiProperty.Create(AOwner: TSepiMeta;
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIndex: Integer = NoIndex; ADefaultValue: Integer = NoDefaultValue;
  const AStorage: string = ''; AIsDefault: Boolean = False);

  function FindAccess(const AAccess: string): TSepiMeta;
  begin
    if Owner is TSepiClass then
      Result := TSepiClass(Owner).LookForMember(
        AAccess, OwningUnit, TSepiClass(Owner))
    else
      Result := TSepiInterface(Owner).LookForMember(AAccess);
  end;

begin
  inherited Create(AOwner, AName);

  FSignature := TSepiSignature.Create(Self, ASignature);

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
  Crée une nouvelle propriété
  @param AOwner         Propriétaire de la propriété
  @param AName          Nom de la propriété
  @param ASignature     Signature
  @param AReadAccess    Accès en lecture à la propriété (peut être vide)
  @param AWriteAccess   Accès en écriture à la propriété (peut être vide)
  @param AIsDefault     Indique si c'est la propriété tableau par défaut
*}
constructor TSepiProperty.Create(AOwner: TSepiMeta;
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIsDefault: Boolean);
begin
  Create(AOwner, AName, ASignature, AReadAccess, AWriteAccess,
    NoIndex, NoDefaultValue, '', AIsDefault);
end;

{*
  Redéfinit une propriété héritée
  @param AOwner         Propriétaire de la propriété
  @param AName          Nom de la propriété
  @param AReadAccess    Accès en lecture à la propriété
  @param AWriteAccess   Accès en écriture à la propriété
  @param AStorage       Spécificateur de stockage
*}
constructor TSepiProperty.Redefine(AOwner: TSepiMeta;
  const AName: string; const AReadAccess: string = '';
  const AWriteAccess: string = ''; const AStorage: string = '');
var
  Previous: TSepiProperty;
begin
  Previous := (AOwner as TSepiClass).Parent.LookForMember(
    AName, AOwner.OwningUnit, TSepiClass(AOwner)) as TSepiProperty;

  inherited Create(AOwner, AName);

  FSignature := Previous.Signature;
  FIndex := Previous.Index;
  FDefaultValue := Previous.FDefaultValue;
  FIsDefault := Previous.IsDefault;

  if AReadAccess = '' then
    FReadAccess := Previous.ReadAccess
  else
  begin
    FReadAccess.Meta := TSepiClass(Owner).LookForMember(
      AReadAccess, OwningUnit, TSepiClass(Owner));
    MakePropertyAccessKind(FReadAccess);
  end;

  if AWriteAccess = '' then
    FWriteAccess := Previous.WriteAccess
  else
  begin
    FWriteAccess.Meta := TSepiClass(Owner).LookForMember(
      AWriteAccess, OwningUnit, TSepiClass(Owner));
    MakePropertyAccessKind(FWriteAccess);
  end;

  if AStorage = '' then
    FStorage := Previous.Storage
  else
    MakePropertyStorage(FStorage, AStorage, TSepiClass(Owner));
end;

{*
  Redéfinit une propriété héritée
  @param AOwner          Propriétaire de la propriété
  @param AName           Nom de la propriété
  @param AReadAccess     Accès en lecture à la propriété (peut être vide)
  @param AWriteAccess    Accès en écriture à la propriété (peut être vide)
  @param ADefaultValue   Valeur par défaut de la propriété
  @param AStorage        Spécificateur de stockage
*}
constructor TSepiProperty.Redefine(AOwner: TSepiMeta;
  const AName, AReadAccess, AWriteAccess: string; ADefaultValue: Integer;
  const AStorage: string = '');
begin
  Redefine(AOwner, AName, AReadAccess, AWriteAccess, AStorage);

  FDefaultValue := ADefaultValue;
end;

{*
  Détruit l'instance
*}
destructor TSepiProperty.Destroy;
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
procedure TSepiProperty.MakePropInfo(PropInfo: PPropInfo);
var
  ShortName: ShortString;
begin
  // Property type RTTI
  PropInfo.PropType := TSepiMetaClass(PropType).TypeInfoRef;

  // Read access
  with PropInfo^, ReadAccess do
  begin
    case Kind of
      pakNone: GetProc := nil;
      pakField: GetProc := Pointer($FF000000 or Field.Offset);
      pakMethod:
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
      pakNone: SetProc := nil;
      pakField: SetProc := Pointer($FF000000 or Field.Offset);
      pakMethod:
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
      pskConstant: StoredProc := Pointer($FFFFFF00 or LongWord(Stored));
      pskField: StoredProc := Pointer($FF000000 or Field.Offset);
      pskMethod:
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
  Type de la propriété
  @return Type de la propriété
*}
function TSepiProperty.GetPropType: TSepiType;
begin
  Result := Signature.ReturnType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiProperty.ListReferences;
begin
  inherited;
  Signature.ListReferences;
  OwningUnit.AddRef(FReadAccess.Meta);
  OwningUnit.AddRef(FWriteAccess.Meta);
end;

{*
  [@inheritDoc]
*}
procedure TSepiProperty.Save(Stream: TStream);
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
procedure TSepiProperty.Destroying;
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
constructor TSepiRecordType.Load(AOwner: TSepiMeta; Stream: TStream);
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
constructor TSepiRecordType.Create(AOwner: TSepiMeta; const AName: string;
  APacked: Boolean = False; AIsNative: Boolean = False;
  ATypeInfo: PTypeInfo = nil);
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
function TSepiRecordType.PrivAddField(const FieldName: string;
  FieldType: TSepiType; After: TSepiField;
  ForcePack: Boolean = False): TSepiField;
var
  Offset: Integer;
begin
  if After = nil then
    Offset := 0
  else
    Offset := After.Offset + After.FieldType.Size;
  if (not IsPacked) and (not ForcePack) then
    FieldType.AlignOffset(Offset);

  Result := TSepiField.Create(Self, FieldName, FieldType, Offset);
end;

{*
  Construit les RTTI, si besoin
*}
procedure TSepiRecordType.MakeTypeInfo;
var
  Fields: TObjectList;
  I: Integer;
  FieldTable: PInitTable;
begin
  if not NeedInit then
    Exit;

  Fields := TObjectList.Create(False);
  try
    // Listings the fields which need initialization
    for I := 0 to ChildCount-1 do
      if TSepiField(Children[I]).FieldType.NeedInit then
        Fields.Add(Children[I]);

    // Creating the RTTI
    AllocateTypeInfo(
      RecordTypeDataLengthBase + Fields.Count*SizeOf(TInitInfo));
    FieldTable := PInitTable(TypeData);

    // Basic information
    FieldTable.Size := FSize;
    FieldTable.Count := Fields.Count;

    // Field information
    for I := 0 to Fields.Count-1 do
    begin
      with TSepiField(Fields[I]) do
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
procedure TSepiRecordType.ChildAdded(Child: TSepiMeta);
begin
  inherited;

  if Child is TSepiField then
  begin
    with TSepiField(Child) do
    begin
      if FieldType.NeedInit then
        FNeedInit := True;
      if Offset + FieldType.Size > FSize then
        FSize := Offset + FieldType.Size;
      if FieldType.Alignment > FAlignment then
        FAlignment := FieldType.Alignment;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRecordType.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(FPacked, 1);
  SaveChildren(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.GetAlignment: Integer;
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
function TSepiRecordType.AddField(const FieldName: string;
  FieldType: TSepiType; ForcePack: Boolean = False): TSepiField;
var
  LastField: TSepiField;
begin
  if ChildCount = 0 then
    LastField := nil
  else
    LastField := TSepiField(Children[ChildCount-1]);

  Result := PrivAddField(FieldName, FieldType, LastField, ForcePack);
end;

{*
  Ajoute un champ au record
  @param FieldName       Nom du champ
  @param FieldTypeInto   RTTI du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName: string;
  FieldTypeInfo: PTypeInfo; ForcePack: Boolean = False): TSepiField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeInfo), ForcePack);
end;

{*
  Ajoute un champ au record
  @param FieldName       Nom du champ
  @param FieldTypeName   Nom du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName, FieldTypeName: string;
  ForcePack: Boolean = False): TSepiField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeName), ForcePack);
end;

{*
  Ajoute un champ au record après un champ donné en mémoire
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param After       Nom du champ précédent en mémoire (vide pour le début)
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddFieldAfter(const FieldName: string;
  FieldType: TSepiType; const After: string;
  ForcePack: Boolean = False): TSepiField;
begin
  Result := PrivAddField(FieldName, FieldType,
    TSepiField(FindMeta(After)), ForcePack);
end;

{*
  Ajoute un champ au record après un champ donné en mémoire
  @param FieldName       Nom du champ
  @param FieldTypeInfo   RTTI du type du champ
  @param After           Nom du champ précédent en mémoire (vide pour le début)
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddFieldAfter(const FieldName: string;
  FieldTypeInfo: PTypeInfo; const After: string;
  ForcePack: Boolean = False): TSepiField;
begin
  Result := PrivAddField(FieldName, Root.FindType(FieldTypeInfo),
    TSepiField(FindMeta(After)), ForcePack);
end;

{*
  Ajoute un champ au record après un champ donné en mémoire
  @param FieldName       Nom du champ
  @param FieldTypeName   Nom du type du champ
  @param After           Nom du champ précédent en mémoire (vide pour le début)
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddFieldAfter(
  const FieldName, FieldTypeName, After: string;
  ForcePack: Boolean = False): TSepiField;
begin
  Result := PrivAddField(FieldName, Root.FindType(FieldTypeName),
    TSepiField(FindMeta(After)), ForcePack);
end;

{*
  Termine le record et construit ses RTTI si ce n'est pas déjà fait
*}
procedure TSepiRecordType.Complete;
begin
  if FCompleted then
    Exit;

  FCompleted := True;
  if not IsPacked then
    AlignOffset(FSize);

  if Size > 4 then
  begin
    FParamBehavior.AlwaysByAddress := True;
    FResultBehavior := rbParameter;
  end;

  if TypeInfo = nil then
    MakeTypeInfo;
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := Self = AType;
end;

{-----------------------}
{ Classe TSepiInterface }
{-----------------------}

{*
  Recense une interface native
*}
constructor TSepiInterface.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
var
  Flags: TIntfFlags;
begin
  inherited;

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;

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

  if not FHasGUID then
    FGUID := NoGUID
  else
    FGUID := TypeData.Guid;
end;

{*
  Charge une interface depuis un flux
*}
constructor TSepiInterface.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;

  OwningUnit.ReadRef(Stream, FParent);
  FCompleted := False;

  Stream.ReadBuffer(FHasGUID, 1);
  Stream.ReadBuffer(FIsDispInterface, 1);
  Stream.ReadBuffer(FIsDispatch, 1);
  Stream.ReadBuffer(FGUID, SizeOf(TGUID));

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
constructor TSepiInterface.Create(AOwner: TSepiMeta; const AName: string;
  AParent: TSepiInterface; const AGUID: TGUID;
  AIsDispInterface: Boolean = False);
begin
  inherited Create(AOwner, AName, tkInterface);

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;

  if Assigned(AParent) then
    FParent := AParent
  else
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
var
  Flags: TIntfFlags;
  OwningUnitName: ShortString;
  Count: PWord;
begin
  // Creating the RTTI
  AllocateTypeInfo(IntfTypeDataLengthBase + Length(OwningUnit.Name) + 1);
  TypeData.IntfParent := FParent.TypeInfoRef;

  // Interface flags
  Flags := [];
  if FHasGUID then
    Include(Flags, ifHasGuid);
  if FIsDispInterface then
    Include(Flags, ifDispInterface);
  if FIsDispatch then
    Include(Flags, ifDispatch);
  TypeData.IntfFlags := Flags;

  // GUID
  TypeData.Guid := FGUID;

  // Owning unit name
  OwningUnitName := OwningUnit.Name;
  Move(OwningUnitName[0], TypeData.IntfUnit[0], Length(OwningUnitName)+1);

  // Method count in the interface
  Count := SkipPackedShortString(@TypeData.IntfUnit);
  Count^ := ChildCount;
  Inc(Integer(Count), 2);
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
procedure TSepiInterface.Save(Stream: TStream);
begin
  inherited;

  OwningUnit.WriteRef(Stream, FParent);

  Stream.WriteBuffer(FHasGUID, 1);
  Stream.WriteBuffer(FIsDispInterface, 1);
  Stream.WriteBuffer(FIsDispatch, 1);
  Stream.WriteBuffer(FGUID, SizeOf(TGUID));

  SaveChildren(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiInterface.InternalLookFor(const Name: string;
  FromUnit: TSepiUnit; FromClass: TSepiMeta = nil): TSepiMeta;
begin
  // Look for a member first
  Result := LookForMember(Name);

  // If not found, continue search a level up
  if (Result = nil) and (Owner <> nil) then
    Result := TSepiInterface(Owner).InternalLookFor(Name, FromUnit, FromClass);
end;

{*
  Crée une nouvelle instance de TSepiInterface
  @return Instance créée
*}
class function TSepiInterface.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  with TSepiInterface(Result) do
  begin
    FSize := 4;
    FNeedInit := True;
    FResultBehavior := rbParameter;
  end;
end;

{*
  Déclare un type interface en forward
  @param AOwner      Propriétaire du type
  @param ATypeName   RTTI de l'interface
*}
class function TSepiInterface.ForwardDecl(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo): TSepiInterface;
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
class function TSepiInterface.ForwardDecl(AOwner: TSepiMeta;
  const AName: string): TSepiInterface;
begin
  Result := TSepiInterface(NewInstance);
  TSepiInterface(AOwner).AddForward(AName, Result);
end;

{*
  Ajoute une méthode à l'interface
  @param MethodName           Nom de la méthode
  @param ASignature           Signature Delphi de la méthode
  @param ACallingConvention   Convention d'appel de la méthode
*}
function TSepiInterface.AddMethod(const MethodName, ASignature: string;
  ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
begin
  Result := TSepiMethod.Create(Self, MethodName, nil, ASignature,
    mlkInterface, False, 0, ACallingConvention);
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
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIndex: Integer = NoIndex; AIsDefault: Boolean = False): TSepiProperty;
begin
  Result := TSepiProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIndex, NoDefaultValue, '', AIsDefault);
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
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIsDefault: Boolean): TSepiProperty;
begin
  Result := TSepiProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIsDefault);
end;

{*
  Termine l'interface et construit ses RTTI si ce n'est pas déjà fait
*}
procedure TSepiInterface.Complete;
begin
  if FCompleted then
    Exit;

  FCompleted := True;
  if not Native then
    MakeTypeInfo;
end;

{*
  Détermine si l'interface hérite d'une interface donnée
  @param AParent   Ancêtre à tester
  @return True si l'interface hérite de AParent, False sinon
*}
function TSepiInterface.IntfInheritsFrom(AParent: TSepiInterface): Boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FParent) and FParent.IntfInheritsFrom(AParent));
end;

{*
  Recherche un membre dans l'interface
  @param MemberName   Nom du membre recherché
  @return Le membre correspondant, ou nil si non trouvé
*}
function TSepiInterface.LookForMember(const MemberName: string): TSepiMeta;
begin
  if MemberName = '' then
  begin
    Result := nil;
    Exit;
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
constructor TSepiClass.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
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
constructor TSepiClass.Load(AOwner: TSepiMeta; Stream: TStream);
var
  IntfCount, I: Integer;
begin
  inherited;

  FSize := 4;
  FDelphiClass := nil;
  OwningUnit.ReadRef(Stream, FParent);
  FCompleted := False;

  Stream.ReadBuffer(IntfCount, 4);
  SetLength(FInterfaces, IntfCount);
  FillChar(FInterfaces[0], IntfCount*SizeOf(TSepiInterfaceEntry), 0);
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
constructor TSepiClass.Create(AOwner: TSepiMeta; const AName: string;
  AParent: TSepiClass);
begin
  inherited Create(AOwner, AName, tkClass);

  FSize := 4;
  FDelphiClass := nil;
  if Assigned(AParent) then
    FParent := AParent
  else
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
  Tables: array[0..4] of Integer = (
    vmtDynamicTable, vmtMethodTable, vmtFieldTable, vmtInitTable, vmtIntfTable
  );
var
  I: Integer;
  PTable: Pointer;
begin
  if (not Native) and (FDelphiClass <> nil) then
  begin
    // Destroying the IMTs
    for I := 0 to High(FInterfaces) do
    begin
      with FInterfaces[I] do
      begin
        if Assigned(Relocates) then
          FreeMem(Relocates);
        if Assigned(IMT) then
          FreeMem(IMT);
      end;
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
procedure TSepiClass.MakeIMT(IntfEntry: PSepiInterfaceEntry);
const
  AdjustInstrSizes: array[Boolean, Boolean] of Shortint = (
    (5, 8), (3, 5)
  );
  JumpInstrSize = 5;
var
  Offset: Longint;
  BigOffset: Boolean;
  Methods: TObjectList;
  Method, RealMethod: TSepiMethod;
  Intf: TSepiInterface;
  I, RelocLength, AdjustInstrSize: Integer;
  RelocEntry, IMTEntry: Integer;
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
        if not (Intf.Children[I] is TSepiMethod) then
          Continue;
        Method := TSepiMethod(Intf.Children[I]);

        AdjustInstrSize := AdjustInstrSizes[
          Method.Signature.CallingConvention = ccRegister, BigOffset];

        Inc(RelocLength, AdjustInstrSize);
        Inc(RelocLength, JumpInstrSize);

        Methods.Insert(0, Method);
      end;

      Intf := Intf.Parent;
    end;

    // Creating the relocates thunks and the IMT
    GetMem(IntfEntry.Relocates, RelocLength);
    RelocEntry := Integer(IntfEntry.Relocates);
    GetMem(IntfEntry.IMT, IntfEntry.IntfRef.IMTSize);
    IMTEntry := Integer(IntfEntry.IMT);

    // Filling the relocates thunks and the IMT
    for I := 0 to Methods.Count-1 do
    begin
      Method := TSepiMethod(Methods[I]);

      // IMT entry
      PLongInt(IMTEntry)^ := RelocEntry;
      Inc(IMTEntry, 4);

      // Adjust instruction
      if Method.Signature.CallingConvention = ccRegister then
      begin
        // add eax, Offset
        if BigOffset then
        begin
          // 05 xx xx xx xx
          PByte(RelocEntry)^ := $05;
          PLongInt(RelocEntry+1)^ := Offset;
          Inc(RelocEntry, 5);
        end else
        begin
          // 83 C0 xx
          PWord(RelocEntry)^ := $C083;
          PShortInt(RelocEntry+2)^ := Offset;
          Inc(RelocEntry, 3);
        end;
      end else
      begin
        // add dwortd ptr [esp+4], Offset
        if BigOffset then
        begin
          // 81 44 24 04 xx xx xx xx
          PLongWord(RelocEntry)^ := $04244481;
          PLongInt(RelocEntry+4)^ := Offset;
          Inc(RelocEntry, 8);
        end else
        begin
          // 83 44 24 04 xx
          PLongWord(RelocEntry)^ := $04244483;
          PShortInt(RelocEntry+4)^ := Offset;
          Inc(RelocEntry, 5);
        end;
      end;

      // JumpInstruction
      RealMethod :=
        LookforMember(Method.Name, OwningUnit, Self) as TSepiMethod;

      // jmp Method.Code   E9 xx xx xx xx
      PByte(RelocEntry)^ := $E9;
      PLongInt(RelocEntry+1)^ := Integer(RealMethod.Code) - RelocEntry - 5;
      Inc(RelocEntry, 5);
    end;
  finally
    Methods.Free;
  end;
end;

{*
  Construit les IMTs
*}
procedure TSepiClass.MakeIMTs;
var
  IntfCount, I: Integer;
  IntfTable: PInterfaceTable;
begin
  // If no interface supported, then exit
  IntfCount := InterfaceCount;
  if IntfCount = 0 then
    Exit;

  // Fetch native interface table
  if Native then
    IntfTable := DelphiClass.GetInterfaceTable
  else
    IntfTable := nil;

  for I := 0 to IntfCount-1 do
  begin
    FInterfaces[I].Offset := FInstSize;
    Inc(FInstSize, 4);

    // If native, get information from IntfTable; otherwise, create the IMT
    if Native then
    begin
      FInterfaces[I].IMT := IntfTable.Entries[I].VTable;
      FInterfaces[I].Offset := IntfTable.Entries[I].IOffset;
    end else
      MakeIMT(@FInterfaces[I]);
  end;
end;

{*
  Construit les RTTI
*}
procedure TSepiClass.MakeTypeInfo;
var
  OwningUnitName: ShortString;
  TypeDataLength, I: Integer;
  Props: TObjectList;
  Prop: TSepiProperty;
  PropCount: PWord;
  PropInfo: PPropInfo;
begin
  OwningUnitName := OwningUnit.Name;
  Props := TObjectList.Create(False);
  try
    TypeDataLength := ClassTypeDataLengthBase;
    Inc(TypeDataLength, Length(OwningUnitName));
    Inc(TypeDataLength);

    // Listing the published properties, and computing the type data length
    for I := 0 to ChildCount-1 do
    begin
      if Children[I] is TSepiProperty then
      begin
        Prop := TSepiProperty(Children[I]);
        if Prop.Visibility <> mvPublished then
          Continue;

        Props.Add(Prop);
        Inc(TypeDataLength, PropInfoLengthBase);
        Inc(TypeDataLength, Length(Prop.Name));
        Inc(TypeDataLength);
      end;
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
      TSepiProperty(Props[I]).MakePropInfo(PropInfo);
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
var
  IntfCount, I: Integer;
  IntfTable: PInterfaceTable;
begin
  // If no interface supported, then exit
  IntfCount := InterfaceCount;
  if IntfCount = 0 then
    Exit;

  // Creating the interface table
  GetMem(IntfTable, SizeOf(Integer) + IntfCount*SizeOf(TInterfaceEntry));
  VMTEntries[vmtIntfTable] := IntfTable;

  // Basic information
  IntfTable.EntryCount := IntfCount;

  // Interface information
  for I := 0 to IntfCount-1 do
  begin
    with IntfTable.Entries[I], FInterfaces[I] do
    begin
      IID := IntfRef.GUID;
      VTable := IMT;
      IOffset := Offset;
      ImplGetter := 0;
    end;
  end;
end;

{*
  Construit la table d'initialisation
  Range également l'adresse de cette table à l'emplacement prévu de la VMT.
*}
procedure TSepiClass.MakeInitTable;
var
  Fields: TObjectList;
  I: Integer;
  Meta: TSepiMeta;
  InitTable: PInitTable;
begin
  Fields := TObjectList.Create(False);
  try
    // Listing the fields which need initialization
    for I := 0 to ChildCount-1 do
    begin
      Meta := Children[I];
      if (Meta is TSepiField) and
        TSepiField(Meta).FieldType.NeedInit then
        Fields.Add(Meta);
    end;

    // If no field to be finalized, then exit
    if Fields.Count = 0 then
      Exit;

    // Creating the init table
    GetMem(InitTable, InitTableLengthBase + Fields.Count*SizeOf(TInitInfo));
    VMTEntries[vmtInitTable] := InitTable;

    // Basic information
    PWord(InitTable)^ := 0;
    Inc(Integer(InitTable), 2);
    InitTable.Size := 0;
    InitTable.Count := Fields.Count;

    // Field information
    for I := 0 to Fields.Count-1 do
    begin
      with TSepiField(Fields[I]) do
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
var
  Fields: TObjectList;
  I: Integer;
  Meta: TSepiMeta;
  FieldTable: PFieldTable;
  FieldInfo: PFieldInfo;
  ShortName: ShortString;
begin
  Fields := TObjectList.Create(False);
  try
    // Listing the published fields
    for I := 0 to ChildCount-1 do
    begin
      Meta := Children[I];
      if (Meta is TSepiField) and (Meta.Visibility = mvPublished) then
        Fields.Add(Meta);
    end;

    // If no published field, then exit
    if Fields.Count = 0 then
      Exit;

    // Creating the field table
    GetMem(FieldTable, FieldTableLengthBase + Fields.Count*SizeOf(TFieldInfo));
    VMTEntries[vmtFieldTable] := FieldTable;

    // Basic information
    FieldTable.FieldCount := Fields.Count;
    FieldTable.Unknown := nil;

    // Field information
    FieldInfo := PFieldInfo(Integer(FieldTable) + SizeOf(TFieldTable));
    for I := 0 to Fields.Count-1 do
    begin
      with TSepiField(Fields[I]) do
      begin
        FieldInfo.Offset := Offset;
        FieldInfo.Index := I;
        ShortName := Name;
        Move(ShortName[0], FieldInfo.Name[0], Length(ShortName)+1);

        Inc(Integer(FieldInfo), 7 + Length(ShortName));
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
var
  Methods: TObjectList;
  I: Integer;
  Meta: TSepiMeta;
  MethodTable: PMethodTable;
  MethodInfo: PMethodInfo;
  ShortName: ShortString;
begin
  Methods := TObjectList.Create(False);
  try
    // Listing the published methods
    for I := 0 to ChildCount-1 do
    begin
      Meta := Children[I];
      if (Meta is TSepiMethod) and (Meta.Visibility = mvPublished) then
        Methods.Add(Meta);
    end;

    // If no published method, then exit
    if Methods.Count = 0 then
      Exit;

    // Creating the method table
    GetMem(MethodTable, MethodTableLengthBase +
      Methods.Count*SizeOf(TMethodInfo));
    VMTEntries[vmtMethodTable] := MethodTable;

    // Basic information
    MethodTable.MethodCount := Methods.Count;

    // Field information
    MethodInfo := PMethodInfo(Integer(MethodTable) + SizeOf(TMethodTable));
    for I := 0 to Methods.Count-1 do
    begin
      with TSepiMethod(Methods[I]) do
      begin
        ShortName := Name;
        MethodInfo.Size := 7 + Length(ShortName);
        MethodInfo.Code := Code;
        Move(ShortName[0], MethodInfo.Name[0], Length(ShortName)+1);

        Inc(Integer(MethodInfo), MethodInfo.Size);
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
var
  PDMT: Pointer;
  I, Count: Integer;
  Meta: TSepiMeta;
  IndexList, CodeList: Integer;
  Methods: TObjectList;
begin
  Methods := TObjectList.Create(False);
  try
    // Listing dynamic methods
    for I := 0 to ChildCount-1 do
    begin
      Meta := Children[I];
      if (Meta is TSepiMethod) and
        (TSepiMethod(Meta).LinkKind in [mlkDynamic, mlkMessage]) and
        (not TSepiMethod(Meta).IsAbstract) then
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
    for I := 0 to Count-1 do
    begin
      with TSepiMethod(Methods[I]) do
      begin
        PSmallInt(IndexList + 2*I)^ := DMTIndex; // alias MsgID
        PPointer(CodeList + 4*I)^ := Code;
      end;
    end;
  finally
    Methods.Free;
  end;
end;

{*
  Construit la VMT
*}
procedure TSepiClass.MakeVMT;
var
  PVMT: Pointer;
  AbstractErrorProcAddress: Pointer;
  I: Integer;
  Method: TSepiMethod;
begin
  // Creating the VMT
  GetMem(PVMT, FVMTSize - vmtMinIndex);
  FillChar(PVMT^, FVMTSize - vmtMinIndex, 0);
  Dec(Integer(PVMT), vmtMinIndex);
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
  for I := 0 to ChildCount-1 do
  begin
    if Children[I] is TSepiMethod then
    begin
      Method := TSepiMethod(Children[I]);
      if Method.LinkKind = mlkVirtual then
      begin
        if Method.IsAbstract then
          VMTEntries[Method.VMTOffset] := AbstractErrorProcAddress
        else
          VMTEntries[Method.VMTOffset] := Method.Code;
      end;
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
  Nombre d'interfaces supportées
  @return Nombre d'interfaces supportées
*}
function TSepiClass.GetInterfaceCount: Integer;
begin
  Result := Length(FInterfaces);
end;

{*
  Tableau zero-based des interfaces supportées
  @param Index   Index dans le tableau
  @return Interface supportée à l'index spécifié
*}
function TSepiClass.GetInterfaces(Index: Integer): TSepiInterface;
begin
  Result := FInterfaces[Index].IntfRef;
end;

{*
  VMT de la classe, indexée par les constantes vmtXXX
  @param Index   Index dans la VMT
  @return Information contenue dans la VMT à l'index spécifié
*}
function TSepiClass.GetVMTEntries(Index: Integer): Pointer;
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
procedure TSepiClass.SetVMTEntries(Index: Integer; Value: Pointer);
begin
  PPointer(Integer(FDelphiClass) + Index)^ := Value;
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.ChildAdded(Child: TSepiMeta);
begin
  inherited;

  if State = msConstructing then
    Child.Visibility := FCurrentVisibility;

  if Child is TSepiField then
    with TSepiField(Child) do
      FInstSize := Offset + FieldType.Size;
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.ListReferences;
var
  I: Integer;
begin
  inherited;
  OwningUnit.AddRef(FParent);
  for I := 0 to InterfaceCount-1 do
    OwningUnit.AddRef(Interfaces[I]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.Save(Stream: TStream);
var
  IntfCount, I: Integer;
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
  [@inheritDoc]
*}
function TSepiClass.InternalLookFor(const Name: string; FromUnit: TSepiUnit;
  FromClass: TSepiMeta = nil): TSepiMeta;
begin
  // Look for a member first
  Result := LookForMember(Name, FromUnit, TSepiClass(FromClass));

  // If not found, continue search a level up
  if (Result = nil) and (Owner <> nil) then
    Result := TSepiClass(Owner).InternalLookFor(Name, FromUnit, FromClass);
end;

{*
  Crée une nouvelle instance de TSepiClass
  @return Instance créée
*}
class function TSepiClass.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  with TSepiClass(Result) do
  begin
    FSize := 4;
    FNeedInit := False;
    FResultBehavior := rbOrdinal;
  end;
end;

{*
  Déclare un type classe en forward
  @param AOwner      Propriétaire du type
  @param ATypeInfo   RTTI de la classe
*}
class function TSepiClass.ForwardDecl(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo): TSepiClass;
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
class function TSepiClass.ForwardDecl(AOwner: TSepiMeta;
  const AName: string): TSepiClass;
begin
  Result := TSepiClass(NewInstance);
  TSepiClass(AOwner).AddForward(AName, Result);
end;

{*
  Ajoute le support d'une interface
  @param AInterface   Interface à supporter
*}
procedure TSepiClass.AddInterface(AInterface: TSepiInterface);
var
  Index: Integer;
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
procedure TSepiClass.AddInterface(AIntfTypeInfo: PTypeInfo);
begin
  AddInterface(Root.FindType(AIntfTypeInfo) as TSepiInterface);
end;

{*
  Ajoute le support d'une interface
  @param AIntfName   Nom de l'interface à supporter
*}
procedure TSepiClass.AddInterface(const AIntfName: string);
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
function TSepiClass.AddField(const FieldName: string; FieldType: TSepiType;
  ForcePack: Boolean = False): TSepiField;
var
  Offset: Integer;
begin
  Offset := InstSize;
  if not ForcePack then
    FieldType.AlignOffset(Offset);

  Result := TSepiField.Create(Self, FieldName, FieldType, Offset);
end;

{*
  Ajoute un champ à la classe
  @param FieldName       Nom du champ
  @param FieldTypeInto   RTTI du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajouté
*}
function TSepiClass.AddField(const FieldName: string;
  FieldTypeInfo: PTypeInfo; ForcePack: Boolean = False): TSepiField;
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
function TSepiClass.AddField(const FieldName, FieldTypeName: string;
  ForcePack: Boolean = False): TSepiField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeName), ForcePack);
end;

{*
  Ajoute une méthode à la classe
  @param MethodName   Nom de la méthode
  @param ACode        Pointeur sur le code de la méthode
  @param ASignature   Signature Delphi de la méthode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la méthode est abstraite
  @param AMsgID       Pour les méthodes de message, le message intercepté
  @param ACallingConvention   Convention d'appel de la méthode
*}
function TSepiClass.AddMethod(const MethodName: string; ACode: Pointer;
  const ASignature: string; ALinkKind: TMethodLinkKind = mlkStatic;
  AAbstract: Boolean = False; AMsgID: Integer = 0;
  ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
begin
  Result := TSepiMethod.Create(Self, MethodName, ACode, ASignature,
    ALinkKind, AAbstract, AMsgID, ACallingConvention);
end;

{*
  Ajoute une méthode à la classe
  @param MethodName           Nom de la méthode
  @param ACode                Pointeur sur le code de la méthode
  @param ASignature           Signature Delphi de la méthode
  @param ACallingConvention   Convention d'appel de la méthode
*}
function TSepiClass.AddMethod(const MethodName: string; ACode: Pointer;
  const ASignature: string;
  ACallingConvention: TCallingConvention): TSepiMethod;
begin
  Result := TSepiMethod.Create(Self, MethodName, ACode, ASignature,
    mlkStatic, False, 0, ACallingConvention);
end;

{*
  Ajoute une méthode surchargée à la classe
  @param MethodName   Nom de la méthode
  @param ACode        Pointeur sur le code de la méthode
  @param ASignature   Signature Delphi de la méthode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la méthode est abstraite
  @param AMsgID       Pour les méthodes de message, le message intercepté
  @param ACallingConvention   Convention d'appel de la méthode
*}
function TSepiClass.AddOverloadedMethod(const MethodName: string;
  ACode: Pointer;
  const ASignature: string; ALinkKind: TMethodLinkKind = mlkStatic;
  AAbstract: Boolean = False; AMsgID: Integer = 0;
  ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
begin
  Result := TSepiMethod.CreateOverloaded(Self, MethodName, ACode,
    ASignature, ALinkKind, AAbstract, AMsgID, ACallingConvention);
end;

{*
  Ajoute une méthode surchargée à la classe
  @param MethodName           Nom de la méthode
  @param ACode                Pointeur sur le code de la méthode
  @param ASignature           Signature Delphi de la méthode
  @param ACallingConvention   Convention d'appel de la méthode
*}
function TSepiClass.AddOverloadedMethod(const MethodName: string;
  ACode: Pointer; const ASignature: string;
  ACallingConvention: TCallingConvention): TSepiMethod;
begin
  Result := TSepiMethod.CreateOverloaded(Self, MethodName, ACode,
    ASignature, mlkStatic, False, 0, ACallingConvention);
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
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIndex: Integer = NoIndex; ADefaultValue: Integer = NoDefaultValue;
  const AStorage: string = '';
  AIsDefault: Boolean = False): TSepiProperty;
begin
  Result := TSepiProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIndex, ADefaultValue, AStorage, AIsDefault);
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
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIsDefault: Boolean): TSepiProperty;
begin
  Result := TSepiProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIsDefault);
end;

{*
  Redéfinit une propriété héritée
  @param AName          Nom de la propriété
  @param AReadAccess    Accès en lecture à la propriété
  @param AWriteAccess   Accès en écriture à la propriété
  @param AStorage       Spécificateur de stockage
*}
function TSepiClass.RedefineProperty(const AName: string;
  const AReadAccess: string = ''; const AWriteAccess: string = '';
  const AStorage: string = ''): TSepiProperty;
begin
  Result := TSepiProperty.Redefine(Self, AName,
    AReadAccess, AWriteAccess, AStorage);
end;

{*
  Redéfinit une propriété héritée
  @param AName           Nom de la propriété
  @param AReadAccess     Accès en lecture à la propriété
  @param AWriteAccess    Accès en écriture à la propriété
  @param ADefaultValue   Valeur par défaut
  @param AStorage        Spécificateur de stockage
*}
function TSepiClass.RedefineProperty(
  const AName, AReadAccess, AWriteAccess: string; ADefaultValue: Integer;
  const AStorage: string = ''): TSepiProperty;
begin
  Result := TSepiProperty.Redefine(Self, AName,
    AReadAccess, AWriteAccess, ADefaultValue, AStorage);
end;

{*
  Termine la classe et construit ses RTTI si ce n'est pas déjà fait
*}
procedure TSepiClass.Complete;
begin
  if FCompleted then
    Exit;

  FCompleted := True;
  AlignOffset(FInstSize);
  MakeIMTs;
  if not Native then
    MakeVMT;
end;

{*
  [@inheritDoc]
*}
function TSepiClass.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := (AType is TSepiClass) and
    TSepiClass(AType).ClassInheritsFrom(Self);
end;

{*
  Détermine si la classe hérite d'une classe donnée
  @param AParent   Ancêtre à tester
  @return True si la classe hérite de AParent, False sinon
*}
function TSepiClass.ClassInheritsFrom(AParent: TSepiClass): Boolean;
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
function TSepiClass.LookForMember(const MemberName: string;
  FromUnit: TSepiUnit; FromClass: TSepiClass = nil): TSepiMeta;
begin
  if MemberName = '' then
  begin
    Result := nil;
    Exit;
  end;

  Result := GetMeta(MemberName);

  if (Result <> nil) and (not Result.IsVisibleFrom(FromUnit, FromClass)) then
    Result := nil;

  if (Result = nil) and (Parent <> nil) then
    Result := Parent.LookForMember(MemberName, FromUnit, FromClass);
end;

{-----------------------}
{ Classe TSepiMetaClass }
{-----------------------}

{*
  Charge une meta-classe depuis un flux
*}
constructor TSepiMetaClass.Load(AOwner: TSepiMeta; Stream: TStream);
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
constructor TSepiMetaClass.Create(AOwner: TSepiMeta; const AName: string;
  AClass: TSepiClass; AIsNative: Boolean = False);
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
constructor TSepiMetaClass.Create(AOwner: TSepiMeta; const AName: string;
  AClassInfo: PTypeInfo; AIsNative: Boolean = False);
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
constructor TSepiMetaClass.Create(AOwner: TSepiMeta;
  const AName, AClassName: string; AIsNative: Boolean = False);
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
procedure TSepiMetaClass.Save(Stream: TStream);
begin
  inherited;
  OwningUnit.WriteRef(Stream, FClass);
end;

{*
  [@inheritDoc]
*}
function TSepiMetaClass.CompatibleWith(AType: TSepiType): Boolean;
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
constructor TSepiMethodRefType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  FSignature := TSepiSignature.RegisterTypeData(Self, TypeData);

  if Signature.Kind in mkOfObject then
  begin
    FSize := 8;
    FParamBehavior.AlwaysByStack := True;
    FResultBehavior := rbParameter;
  end else
    FSize := 4;
end;

{*
  Charge un type référence de méthode depuis un flux
*}
constructor TSepiMethodRefType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;
  FSignature := TSepiSignature.Load(Self, Stream);

  if Signature.Kind in mkOfObject then
  begin
    FSize := 8;
    FParamBehavior.AlwaysByStack := True;
    FResultBehavior := rbParameter;
  end else
    FSize := 4;
end;

{*
  Crée un nouveau type référence de méthode
  @param AOwner            Propriétaire du type
  @param AName             Nom du type
  @param ASignature        Signature
  @param AOfObject         Indique s'il s'agit d'une méthode
  @param ACallingConvention   Convention d'appel
*}
constructor TSepiMethodRefType.Create(AOwner: TSepiMeta;
  const AName, ASignature: string; AOfObject: Boolean = False;
  ACallingConvention: TCallingConvention = ccRegister);
var
  Prefix: string;
begin
  inherited Create(AOwner, AName, tkMethod);

  if AOfObject then
    Prefix := ''
  else
    Prefix := 'unit ';
  FSignature := TSepiSignature.Create(Self,
    Prefix + ASignature, ACallingConvention);

  if Signature.Kind in mkOfObject then
  begin
    FSize := 8;
    FParamBehavior.AlwaysByStack := True;
    FResultBehavior := rbParameter;
  end else
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
procedure TSepiMethodRefType.Save(Stream: TStream);
begin
  inherited;
  Signature.Save(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiMethodRefType.CompatibleWith(AType: TSepiType): Boolean;
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
constructor TSepiVariantType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;

  FSize := 16;
  FNeedInit := True;
  FParamBehavior.AlwaysByAddress := True;
  FResultBehavior := rbParameter;
end;

{*
  Charge un type variant depuis un flux
*}
constructor TSepiVariantType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  AllocateTypeInfo;

  FSize := 16;
  FNeedInit := True;
  FParamBehavior.AlwaysByAddress := True;
  FResultBehavior := rbParameter;
end;

{*
  [@inheritDoc]
*}
function TSepiVariantType.GetAlignment: Integer;
begin
  Result := 8;
end;

initialization
  SepiRegisterMetaClasses([
    TSepiField, TSepiMethod, TSepiOverloadedMethod, TSepiProperty,
    TSepiRecordType, TSepiInterface, TSepiClass, TSepiMethodRefType,
    TSepiVariantType
  ]);
end.

