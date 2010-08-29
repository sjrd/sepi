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
  D�finit les classes de gestion des types composites
  @author sjrd
  @version 1.0
*}
unit SepiMembers;

interface

{$ASSERTIONS ON}

{.$DEFINE TRYING_TO_UNDERSTAND_THE_GAP_IN_CLASSES}

uses
  Windows, Classes, SysUtils, StrUtils, RTLConsts, Contnrs, TypInfo, ScUtils,
  ScStrUtils, ScTypInfo, ScDelphiLanguage, ScSerializer, ScCompilerMagic,
  SepiReflectionCore, SepiReflectionConsts, SepiArrayTypes;

const
  /// Pas d'index
  NoIndex = Integer($80000000);

  /// Pas de valeur par d�faut
  NoDefaultValue = Integer($80000000);

type
  {*
    Type de signature
    - skStaticProcedure : proc�dure statique (pas de Self) ;
    - skStaticFunction : fonction statique (pas de Self) ;
    - skObjectProcedure : proc�dure d'objet ;
    - skObjectFunction : fonction d'objet ;
    - skConstructor : constructeur ;
    - skDestructor : destructeur ;
    - skClassProcedure : proc�dure de classe ;
    - skClassFunction : fonction de classe ;
    - skClassConstructor : constructeur de classe (pour usage futur) ;
    - skOperator : surcharge d'op�rateur (pour usage futur) ;
    - skProperty : propri�t� ;
    - skClassProperty : propri�t� de classe ;
    - skClassDestructor : destructeur de classe (pour usage futur).
  *}
  TSepiSignatureKind = (
    skStaticProcedure, skStaticFunction,
    skObjectProcedure, skObjectFunction, skConstructor, skDestructor,
    skClassProcedure, skClassFunction, skClassConstructor,
    skOperator, skProperty, skClassProperty, skClassDestructor
  );

const
  /// Tous les types de signature
  skAll = [Low(TSepiSignatureKind)..High(TSepiSignatureKind)];

  /// Proc�dure (statique, d'objet ou de classe)
  skProcedure = [skStaticProcedure, skObjectProcedure, skClassProcedure];

  /// Fonction (statique, d'objet ou de classe)
  skFunction = [skStaticFunction, skObjectFunction, skClassFunction];

  /// Propri�t� (d'objet ou de classe)
  skAnyProperty = [skProperty, skClassProperty];

  /// Types de signature avec valeur de retour
  skWithReturnType = skFunction + [skOperator] + skAnyProperty;

  /// Types de signature avec un param�tre Self
  skWithSelfParam = [skObjectProcedure..skClassConstructor];

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
  TCallingConvention = (ccRegister, ccCDecl, ccPascal, ccStdCall, ccSafeCall);

  {*
    Option de comparaison de signatures (et param�tres)
    - scoKind : Compare les types de signature
    - scoCallingConvention : Compare les conventions d'appel
    - scoParams : Compare les param�tres
    - scoReturnType : Compare les types de retour
    - pcoHiddenKind : Compare les types de param�tre cach� des param�tres
    - pcoKind : Compare les types de param�tre (value/const/var/out)
    - pcoName : Compare les noms des param�tres
    - pcoType : Compare les types des param�tres
    - pcoDefaultValue : Compare les valeurs par d�faut (pour usage futur)
  *}
  TSepiSignatureCompareOption = (
    scoKind, scoCallingConvention, scoParams, scoReturnType,
    pcoHiddenKind, pcoKind, pcoName, pcoType, pcoDefaultValue
  );

  {*
    Options de comparaison de signatures (et param�tres)
  *}
  TSepiSignatureCompareOptions = set of TSepiSignatureCompareOption;

const
  /// Comparaison stricte des signatures
  scoAll = [scoKind..pcoDefaultValue];

  /// Comparaison des signatures pour compatibilit� d'assignation
  scoCompatibility = scoAll - [pcoName, pcoDefaultValue];

  /// Comparaison des signatures pour recherche de la m�thode h�rit�e
  scoInheritance = scoCompatibility - [scoCallingConvention];

  /// Comparaison des signatures pour recherche de la d�claration
  scoDeclaration = scoAll - [scoCallingConvention];

  /// Comparaison stricte des param�tres
  pcoAll = [pcoHiddenKind..pcoDefaultValue];

  /// Comparaison des param�tres pour compatibilit� d'assignation
  pcoCompatibility = pcoAll - [pcoName, pcoDefaultValue];

  /// Comparaison des param�tres pour recherche de la m�thode h�rit�e
  pcoInheritance = pcoCompatibility;

  /// Comparaison des param�tres pour recherche de la d�claration
  pcoDeclaration = pcoAll;

type
  {*
    Type d'accesseur d'une propri�t�
  *}
  TPropertyAccessKind = (pakNone, pakField, pakMethod, pakClassField);

  {*
    Type de stockage d'une propri�t�
  *}
  TPropertyStorageKind = (pskConstant, pskField, pskMethod);

  TSepiSignature = class;
  TSepiOverloadedMethod = class;

  /// Champ de classe
  TSepiClassField = TSepiVariable;

  {*
    Champ
    @author sjrd
    @version 1.0
  *}
  TSepiField = class(TSepiMember)
  private
    FType: TSepiType; /// Type du champ
    FOffset: Integer; /// Offset
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AType: TSepiType; AOffset: Integer);

    property FieldType: TSepiType read FType;
    property Offset: Integer read FOffset;
  end;

  {*
    Informations d'appel d'un param�tre (o� et comment le passer)
    @author sjrd
    @version 1.0
  *}
  TSepiParamCallInfo = record
    ByAddress: Boolean;     /// True pour le passer par adresse
    Place: TSepiParamPlace; /// Endroit o� le placer
    StackOffset: Word;      /// Offset o� le placer dans la pile
    SepiStackOffset: Word;  /// Offset dans la pile pour des appels Sepi
  end;

  {*
    Param�tre de m�thode
    Les param�tres cach�s ont aussi leur existence en tant que
    TSepiComponentParam.
    Les cinq types de param�tres cach�s sont Self (classe ou instance), Result
    (lorsqu'il est pass� par adresse), le param�tre sp�cial $Alloc des
    constructeurs, le param�tre sp�cial $Free des destructeurs et les
    param�tres High$xxx suivant chaque param�tre de type "open array".
    @author sjrd
    @version 1.0
  *}
  TSepiParam = class(TObject)
  private
    FOwner: TSepiSignature; /// Signature propri�taire
    FName: string;          /// Nom
    FLoading: Boolean;      /// True si en chargement depuis un flux

    FHiddenKind: TSepiHiddenParamKind; /// Type de param�tre cach�
    FKind: TSepiParamKind;             /// Type de param�tre
    FByRef: Boolean;                   /// True si pass� par r�f�rence
    FType: TSepiType;                  /// Type du param�tre
    FIsUntyped: Boolean;               /// Indique si non typ�
    FOpenArray: Boolean;               /// Indique si c'est un tableau ouvert
    FElementType: TSepiType;           /// Type des �l�ments du tableau ouvert

    FFlags: TParamFlags;           /// Flags du param�tre
    FCallInfo: TSepiParamCallInfo; /// Informations d'appel du param�tre

    FDefaultValuePtr: Pointer; /// Pointeur sur la valeur par d�faut

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

    procedure WriteDigestData(Stream: TStream);

    function GetHasDefaultValue: Boolean;

    function GetDescription: string;
  public
    constructor Create(AOwner: TSepiSignature; const AName: string;
      AType: TSepiType; AKind: TSepiParamKind = pkValue;
      AOpenArray: Boolean = False);
    constructor Clone(AOwner: TSepiSignature; Source: TSepiParam);
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure AllocDefaultValue;

    function Equals(AParam: TSepiParam;
      Options: TSepiSignatureCompareOptions = pcoAll): Boolean;
      {$IF RTLVersion >= 20.0} reintroduce; {$IFEND}
    function CompatibleWith(AType: TSepiType): Boolean;

    property Owner: TSepiSignature read FOwner;
    property Name: string read FName;

    property HiddenKind: TSepiHiddenParamKind read FHiddenKind;
    property Kind: TSepiParamKind read FKind;
    property ByRef: Boolean read FByRef;
    property ParamType: TSepiType read FType;
    property IsUntyped: Boolean read FIsUntyped;
    property OpenArray: Boolean read FOpenArray;
    property ElementType: TSepiType read FElementType;

    property Flags: TParamFlags read FFlags;
    property CallInfo: TSepiParamCallInfo read FCallInfo;

    property HasDefaultValue: Boolean read GetHasDefaultValue;
    property DefaultValuePtr: Pointer read FDefaultValuePtr;

    property Description: string read GetDescription;
  end;

  {*
    Signature de routine, m�thode ou propri�t�
    @author sjrd
    @version 1.0
  *}
  TSepiSignature = class
  private
    FOwner: TSepiComponent; /// Propri�taire de la signature
    FRoot: TSepiRoot;       /// Racine
    FOwningUnit: TSepiUnit; /// Unit� contenante
    FContext: TSepiType;    /// Contexte

    FLoadingCloning: Boolean;   /// True si en chargement ou en cl�nage
    FAutoCreateHidden: Boolean; /// True pour cr�er automatiquement les cach�s
    FCompleted: Boolean;        /// True si la signature est compl�t�e

    FKind: TSepiSignatureKind;              /// Type de signature
    FReturnType: TSepiType;                 /// Type de retour
    FCallingConvention: TCallingConvention; /// Convention d'appel

    FParams: TObjectList;       /// Param�tres d�clar�s (visibles)
    FActualParams: TObjectList; /// Param�tres r�els

    FRegUsage: Byte;       /// Nombre de registres utilis�s (entre 0 et 3)
    FStackUsage: Word;     /// Taille utilis�e sur la pile (en octets)
    FSepiStackUsage: Word; /// Taille utilis�e sur la pile Sepi (en octets)

    constructor BaseCreate(AOwner: TSepiComponent);

    procedure CheckNotCompleted;
    procedure AddParam(Param: TSepiParam);

    procedure MakeCallInfo;

    function CheckInherited(ASignature: TSepiSignature): Boolean;

    procedure SetKind(Value: TSepiSignatureKind);
    procedure SetReturnType(Value: TSepiType);
    procedure SetCallingConvention(Value: TCallingConvention);

    function GetParamCount: Integer;
    function GetParams(Index: Integer): TSepiParam;
    function GetActualParamCount: Integer;
    function GetActualParams(Index: Integer): TSepiParam;
    function GetParamByName(const ParamName: string): TSepiParam;

    function GetHiddenParam(Kind: TSepiHiddenParamKind): TSepiParam;

    function GetDescription: string;
  protected
    procedure ListReferences;
    procedure Save(Stream: TStream);

    procedure WriteDigestData(Stream: TStream);
  public
    constructor RegisterTypeData(AOwner: TSepiComponent; ATypeData: PTypeData);
    constructor Load(AOwner: TSepiComponent; Stream: TStream);
    constructor Create(AOwner: TSepiComponent; const ASignature: string;
      ACallingConvention: TCallingConvention = ccRegister);
    constructor CreateConstructing(ASepiUnit: TSepiUnit;
      AContext: TSepiType = nil);
    constructor Clone(AOwner: TSepiComponent; Source: TSepiSignature);
    destructor Destroy; override;

    procedure Complete;

    function GetParam(const ParamName: string): TSepiParam;

    function Equals(ASignature: TSepiSignature;
      Options: TSepiSignatureCompareOptions = scoAll): Boolean;
      {$IF RTLVersion >= 20.0} reintroduce; {$IFEND}
    function CompatibleWith(const ATypes: array of TSepiType): Boolean;

    property Owner: TSepiComponent read FOwner;
    property Root: TSepiRoot read FRoot;
    property OwningUnit: TSepiUnit read FOwningUnit;
    property Context: TSepiType read FContext;
    property Kind: TSepiSignatureKind read FKind write SetKind;

    property Completed: Boolean read FCompleted;

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: TSepiParam read GetParams;
    property ActualParamCount: Integer read GetActualParamCount;
    property ActualParams[Index: Integer]: TSepiParam
      read GetActualParams;
    property ParamByName[const ParamName: string]: TSepiParam
      read GetParamByName; default;

    property ReturnType: TSepiType read FReturnType write SetReturnType;
    property CallingConvention: TCallingConvention
      read FCallingConvention write SetCallingConvention;

    property HiddenParam[Kind: TSepiHiddenParamKind]: TSepiParam
      read GetHiddenParam;
    property SelfParam: TSepiParam index hpSelf read GetHiddenParam;
    property ResultParam: TSepiParam index hpResult read GetHiddenParam;
    property AllocParam: TSepiParam index hpAlloc read GetHiddenParam;
    property FreeParam: TSepiParam index hpFree read GetHiddenParam;

    property RegUsage: Byte read FRegUsage;
    property StackUsage: Word read FStackUsage;
    property SepiStackUsage: Word read FSepiStackUsage;

    property Description: string read GetDescription;
  end;

  {*
    Classe de base pour les classes TSepiMethod et TSepiOverloadedMethod
    @author sjrd
    @version 1.0
  *}
  TSepiMethodBase = class(TSepiMember)
  end;

  {*
    M�thode ou routine
    @author sjrd
    @version 1.0
  *}
  TSepiMethod = class(TSepiMethodBase)
  private
    FCode: Pointer;               /// Adresse de code natif
    FCodeJumper: TJmpInstruction; /// Jumper sur le code, si non native
    FCodeHandler: TObject;        /// Gestionnaire de code

    FSignature: TSepiSignature; /// Signature de la m�thode
    FLinkKind: TMethodLinkKind; /// Type de liaison d'appel
    FFirstDeclaration: Boolean; /// Faux uniquement quand 'override'
    FAbstract: Boolean;         /// Indique si la m�thode est abstraite
    FInherited: TSepiMethod;    /// M�thode h�rit�e

    FLinkIndex: Integer; /// Offset de VMT ou index de DMT, selon la liaison

    FIsOverloaded: Boolean;             /// Indique si surcharg�e
    FOverloaded: TSepiOverloadedMethod; /// M�thode surcharg�e (ou nil)
    FOverloadIndex: Byte;               /// Index de surcharge

    procedure MakeLink;
    procedure FindNativeCode;

    procedure CommonCreate(ACode: Pointer; ALinkKind: TMethodLinkKind;
      AAbstract: Boolean; AMsgID: Integer);

    function GetRealName: string;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;

    // Not overloaded constructors
    constructor Create(AOwner: TSepiComponent; const AName: string;
      ACode: Pointer; const ASignature: string;
      ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
      AMsgID: Integer = 0;
      ACallingConvention: TCallingConvention = ccRegister); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      ACode: Pointer; ASignature: TSepiSignature;
      ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
      AMsgID: Integer = 0); overload;

    // Overloaded constructors
    constructor CreateOverloaded(AOwner: TSepiComponent; const AName: string;
      ACode: Pointer; const ASignature: string;
      ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
      AMsgID: Integer = 0;
      ACallingConvention: TCallingConvention = ccRegister); overload;
    constructor CreateOverloaded(AOwner: TSepiComponent; const AName: string;
      ACode: Pointer; ASignature: TSepiSignature;
      ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
      AMsgID: Integer = 0); overload;

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
    M�thode ou routine surcharg�e
    @author sjrd
    @version 1.0
  *}
  TSepiOverloadedMethod = class(TSepiMethodBase)
  private
    FMethods: TObjectList; /// Liste des m�thodes de m�me nom

    function FindInherited(
      ASignature: TSepiSignature): TSepiMethod;

    function GetMethodCount: Integer;
    function GetMethods(Index: Integer): TSepiMethod;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string);
    destructor Destroy; override;

    function IsVisibleFrom(FromComponent: TSepiComponent): Boolean; override;

    function FindMethod(ASignature: TSepiSignature;
      Options: TSepiSignatureCompareOptions = scoAll): TSepiMethod; overload;
    function FindMethod(
      const ATypes: array of TSepiType): TSepiMethod; overload;

    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TSepiMethod read GetMethods;
    property DefaultMethod: TSepiMethod index 0 read GetMethods;
  end;

  {*
    Acc�s de propri�t�
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyAccess = record
    Kind: TPropertyAccessKind;                      /// Type d'acc�s
    case TPropertyAccessKind of
      pakNone: (Component: TSepiMember);            /// Acc�s neutre
      pakField: (Field: TSepiField);                /// Acc�s champ
      pakMethod: (Method: TSepiMethod);             /// Acc�s m�thode
      pakClassField: (ClassField: TSepiClassField); /// Acc�s champ de classe
  end;

  {*
    Stockage de propri�t�
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyStorage = record
    Kind: TPropertyStorageKind;              /// Type de stockage
    Stored: Boolean;                         /// Constante de stockage
    case TPropertyStorageKind of
      pskConstant: (Component: TSepiMember); /// Membre de stockage
      pskField: (Field: TSepiField);         /// Champ de stockage
      pskMethod: (Method: TSepiMethod);      /// M�thode de stockage
  end;

  {*
    Propri�t�
    @author sjrd
    @version 1.0
  *}
  TSepiProperty = class(TSepiMember)
  private
    FSignature: TSepiSignature; /// Signature

    FReadAccess: TSepiPropertyAccess;  /// Acc�s en lecture
    FWriteAccess: TSepiPropertyAccess; /// Acc�s en �criture
    FIndex: Integer;                   /// Index d'acc�s

    FDefaultValue: Integer;         /// Valeur par d�faut
    FStorage: TSepiPropertyStorage; /// Sp�cificateur de stockage

    FIsDefault: Boolean; /// Indique si c'est la propri�t� par d�faut

    FNameIndex: Integer; /// Index de cette propri�t� dans les PropInfos

    procedure MakePropInfo(PropInfo: PPropInfo);

    function GetPropType: TSepiType;
    function GetIsClassProperty: Boolean;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure Destroying; override;

    procedure WriteDigestData(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;

    constructor Create(AOwner: TSepiComponent; const AName: string;
      ASignature: TSepiSignature; AReadAccess, AWriteAccess: TSepiMember;
      AIndex, ADefaultValue: Integer; const AStorage: TSepiPropertyStorage;
      AIsDefault: Boolean); overload;

    constructor Create(AOwner: TSepiComponent;
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIndex: Integer = NoIndex; ADefaultValue: Integer = NoDefaultValue;
      const AStorage: string = ''; AIsDefault: Boolean = False); overload;
    constructor Create(AOwner: TSepiComponent;
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIsDefault: Boolean); overload;

    constructor Redefine(AOwner: TSepiComponent; const AName: string;
      AReadAccess, AWriteAccess: TSepiMember; ADefaultValue: Integer;
      const AStorage: TSepiPropertyStorage); overload;

    constructor Redefine(AOwner: TSepiComponent; const AName: string;
      const AReadAccess: string = ''; const AWriteAccess: string = '';
      const AStorage: string = ''); overload;
    constructor Redefine(AOwner: TSepiComponent;
      const AName, AReadAccess, AWriteAccess: string; ADefaultValue: Integer;
      const AStorage: string = ''); overload;

    destructor Destroy; override;

    property Signature: TSepiSignature read FSignature;
    property PropType: TSepiType read GetPropType;
    property IsClassProperty: Boolean read GetIsClassProperty;

    property ReadAccess: TSepiPropertyAccess read FReadAccess;
    property WriteAccess: TSepiPropertyAccess read FWriteAccess;
    property Index: Integer read FIndex;

    property DefaultValue: Integer read FDefaultValue;
    property Storage: TSepiPropertyStorage read FStorage;

    property IsDefault: Boolean read FIsDefault;
  end;

  {*
    Type enregistrement
    @author sjrd
    @version 1.0
  *}
  TSepiRecordType = class(TSepiContainerType)
  private
    FPacked: Boolean;     /// Indique si le record est packed
    FAlignment: Integer;  /// Alignement
    FCompleted: Boolean;  /// Indique si le record est enti�rement d�fini
    FFields: TObjectList; /// Champs

    function PrivAddField(const FieldName: string; FieldType: TSepiType;
      After: TSepiField;
      ForcePack: Boolean = False): TSepiField;

    procedure MakeTypeInfo;

    function GetFieldCount: Integer;
    function GetFields(Index: Integer): TSepiField;
  protected
    procedure ChildAdded(Child: TSepiComponent); override;

    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    function GetAlignment: Integer; override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      APacked: Boolean = False; AIsNative: Boolean = False;
      ATypeInfo: PTypeInfo = nil);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

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

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;

    function ValueToString(const Value): string; override;

    property IsPacked: Boolean read FPacked;
    property Completed: Boolean read FCompleted;

    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TSepiField read GetFields;
  end;

  {*
    Type conteneur qui peut �tre h�rit�
    @author sjrd
    @version 1.0
  *}
  TSepiInheritableContainerType = class(TSepiContainerType)
  protected
    function InternalLookFor(const Name: string;
      FromComponent: TSepiComponent): TSepiComponent; override;

    procedure WriteDigestData(Stream: TStream); override;

    function GetParentContainer:
      TSepiInheritableContainerType; virtual; abstract;
  public
    function LookForMember(const MemberName: string;
      FromComponent: TSepiComponent): TSepiMember; override;

    function ContainerInheritsFrom(
      Ancestor: TSepiInheritableContainerType): Boolean;

    property ParentContainer: TSepiInheritableContainerType
      read GetParentContainer;
  end;

  {*
    Interface
    @author sjrd
    @version 1.0
  *}
  TSepiInterface = class(TSepiInheritableContainerType)
  private
    FParent: TSepiInterface; /// Interface parent (ou nil - IInterface)
    FCompleted: Boolean;
    /// Indique si l'interface est enti�rement d�finie

    FHasGUID: Boolean;         /// Indique si l'interface poss�de un GUID
    FIsDispInterface: Boolean; /// Indique si c'est une disp interface
    FIsDispatch: Boolean;      /// Indique si c'est une IDispatch
    FGUID: TGUID;              /// GUID de l'interface, si elle en a un

    FIMTSize: Integer; /// Taille de l'IMT

    procedure MakeTypeInfo;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    function GetDescription: string; override;
    function GetParentContainer: TSepiInheritableContainerType; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AParent: TSepiInterface; const AGUID: TGUID;
      AIsDispInterface: Boolean = False);

    class function NewInstance: TObject; override;

    class function ForwardDecl(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo): TSepiInterface; overload;
    class function ForwardDecl(AOwner: TSepiComponent;
      const AName: string): TSepiInterface; overload;

    function AddMethod(const MethodName: string;
      ASignature: TSepiSignature): TSepiMethod; overload;
    function AddMethod(const MethodName, ASignature: string;
      ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
      overload;

    function AddProperty(const AName: string; ASignature: TSepiSignature;
      AReadAccess, AWriteAccess: TSepiMember; AIndex: Integer;
      AIsDefault: Boolean): TSepiProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIndex: Integer = NoIndex;
      AIsDefault: Boolean = False): TSepiProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIsDefault: Boolean): TSepiProperty; overload;

    procedure Complete;

    function Equals(Other: TSepiType): Boolean; override;

    function IntfInheritsFrom(AParent: TSepiInterface): Boolean;

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
    Entr�e d'interface pour les classes Sepi
    Chaque classe a, pour chaque interface qu'elle impl�mente, une entr�e de
    type TSepiInterfaceEntry.
  *}
  TSepiInterfaceEntry = record
    IntfRef: TSepiInterface; /// Interface impl�ment�e
    Relocates: Pointer;      /// Thunks de relocalisation
    IMT: Pointer;            /// Interface Method Table
    Offset: Integer;         /// Offset du champ interface dans l'objet
    OwnsIMT: Boolean;        /// Indique si cette entr�e poss�de son IMT
  end;

  {*
    Redirecteur de m�thode d'interface pour une classe Sepi
  *}
  TSepiIntfMethodRedirector = record
    Intf: TSepiInterface;    /// Interface
    IntfMethod: TSepiMethod; /// M�thode de l'interface
    RedirectorName: string;  /// Nom de la m�thode de redirection
  end;

  {*
    Classe (type objet)
    @author sjrd
    @version 1.0
  *}
  TSepiClass = class(TSepiInheritableContainerType)
  private
    FDelphiClass: TClass; /// Classe Delphi
    FParent: TSepiClass;  /// Classe parent (nil si n'existe pas - TObject)
    FCompleted: Boolean;  /// Indique si la classe est enti�rement d�finie

    /// Interfaces support�es par la classe
    FInterfaces: array of TSepiInterfaceEntry;

    /// Redirecteurs de m�thodes d'interface
    FIntfMethodRedirectors: array of TSepiIntfMethodRedirector;

    FInstSize: Integer;           /// Taille d'une instance de la classe
    FVMTSize: Integer;            /// Taille de la VMT dans les index positifs
    FDMTNextIndex: Integer;       /// Prochain index � utiliser dans la DMT
    FPublishedPropCount: Integer; /// Nombre de propri�t�s publi�es

    FStoredInstSize: Integer; /// Valeur de FInstSize telle que stock�e

    FDefaultProperty: TSepiProperty; /// Propri�t� tableau par d�faut

    procedure LoadInitialDataFromParent;

    procedure MakeIMT(IntfEntry: PSepiInterfaceEntry);
    procedure ListCompleteIMTInterfaces(CompleteIMTInterfaces: TObjectList);
    procedure MakeCompleteIMTs(CompleteIMTInterfaces: TObjectList);
    procedure MakeIMTRedirects(CompleteIMTInterfaces: TObjectList);
    procedure MakeIMTs;
    procedure ReadNativeIMTs;

    procedure MakeTypeInfo;
    procedure MakeIntfTable;
    procedure MakeInitTable;
    procedure MakeFieldTable;
    procedure MakeMethodTable;
    procedure MakeDMT;
    procedure MakeVMT;

    function GetInterfaceCount: Integer;
    function GetInterfaces(Index: Integer): TSepiInterface;

    function GetTotalInstSize: Integer;

    function GetVMTEntries(Index: Integer): Pointer;
    procedure SetVMTEntries(Index: Integer; Value: Pointer);
  protected
    procedure ChildAdded(Child: TSepiComponent); override;

    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    function GetDescription: string; override;
    function GetParentContainer: TSepiInheritableContainerType; override;

    function IsIntfMethodRedirected(Intf: TSepiInterface;
      IntfMethod: TSepiMethod): Boolean;
    function FindIntfMethodImpl(Intf: TSepiInterface;
      IntfMethod: TSepiMethod; FromClass: TSepiClass): TSepiMethod;

    function HasAnyRedirectorFor(Intf: TSepiInterface): Boolean;

    property VMTEntries[Index: Integer]: Pointer
      read GetVMTEntries write SetVMTEntries;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AParent: TSepiClass);
    destructor Destroy; override;

    class function NewInstance: TObject; override;

    class function ForwardDecl(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo): TSepiClass; overload;
    class function ForwardDecl(AOwner: TSepiComponent;
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
      ASignature: TSepiSignature; ALinkKind: TMethodLinkKind = mlkStatic;
      AAbstract: Boolean = False; AMsgID: Integer = 0): TSepiMethod; overload;
    function AddMethod(const MethodName: string; ACode: Pointer;
      const ASignature: string; ALinkKind: TMethodLinkKind = mlkStatic;
      AAbstract: Boolean = False; AMsgID: Integer = 0;
      ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
      overload;
    function AddMethod(const MethodName: string; ACode: Pointer;
      const ASignature: string;
      ACallingConvention: TCallingConvention): TSepiMethod; overload;

    function AddOverloadedMethod(const MethodName: string; ACode: Pointer;
      ASignature: TSepiSignature; ALinkKind: TMethodLinkKind = mlkStatic;
      AAbstract: Boolean = False; AMsgID: Integer = 0): TSepiMethod; overload;
    function AddOverloadedMethod(const MethodName: string; ACode: Pointer;
      const ASignature: string; ALinkKind: TMethodLinkKind = mlkStatic;
      AAbstract: Boolean = False; AMsgID: Integer = 0;
      ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
      overload;
    function AddOverloadedMethod(const MethodName: string; ACode: Pointer;
      const ASignature: string;
      ACallingConvention: TCallingConvention): TSepiMethod; overload;

    function AddProperty(const AName: string; ASignature: TSepiSignature;
      AReadAccess, AWriteAccess: TSepiMember; AIndex, ADefaultValue: Integer;
      const AStorage: TSepiPropertyStorage;
      AIsDefault: Boolean): TSepiProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIndex: Integer = NoIndex; ADefaultValue: Integer = NoDefaultValue;
      const AStorage: string = '';
      AIsDefault: Boolean = False): TSepiProperty; overload;
    function AddProperty(
      const AName, ASignature, AReadAccess, AWriteAccess: string;
      AIsDefault: Boolean): TSepiProperty; overload;

    function RedefineProperty(const AName: string;
      AReadAccess, AWriteAccess: TSepiMember; ADefaultValue: Integer;
      const AStorage: TSepiPropertyStorage): TSepiProperty; overload;
    function RedefineProperty(const AName: string;
      const AReadAccess: string = ''; const AWriteAccess: string = '';
      const AStorage: string = ''): TSepiProperty; overload;
    function RedefineProperty(
      const AName, AReadAccess, AWriteAccess: string; ADefaultValue: Integer;
      const AStorage: string = ''): TSepiProperty; overload;

    procedure AddIntfMethodRedirector(Intf: TSepiInterface;
      IntfMethod: TSepiMethod; const RedirectorName: string);

    procedure Complete;

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;

    function ClassInheritsFrom(AParent: TSepiClass): Boolean;
    function ClassImplementsInterface(AInterface: TSepiInterface): Boolean;

    property DelphiClass: TClass read FDelphiClass;
    property Parent: TSepiClass read FParent;
    property Completed: Boolean read FCompleted;

    property InterfaceCount: Integer read GetInterfaceCount;
    property Interfaces[Index: Integer]: TSepiInterface read GetInterfaces;

    property InstSize: Integer read FInstSize;
    property TotalInstSize: Integer read GetTotalInstSize;
    property VMTSize: Integer read FVMTSize;

    property DefaultProperty: TSepiProperty read FDefaultProperty;
  end;

  {*
    Meta-classe (type classe)
    @author sjrd
    @version 1.0
  *}
  TSepiMetaClass = class(TSepiType)
  private
    FClass: TSepiClass; /// Classe correspondante

    procedure MakeTypeInfo;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AClass: TSepiClass; AIsNative: Boolean = False); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AClassInfo: PTypeInfo; AIsNative: Boolean = False); overload;
    constructor Create(AOwner: TSepiComponent; const AName, AClassName: string;
      AIsNative: Boolean = False); overload;
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;

    property SepiClass: TSepiClass read FClass;
  end;

  {*
    Type r�f�rence de m�thode
    @author sjrd
    @version 1.0
  *}
  TSepiMethodRefType = class(TSepiType)
  private
    FSignature: TSepiSignature; /// Signature

    procedure MakeSize;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    function GetDescription: string; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName, ASignature: string;
      AOfObject: Boolean = False;
      ACallingConvention: TCallingConvention = ccRegister); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      ASignature: TSepiSignature); overload;
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;
    destructor Destroy; override;

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;

    property Signature: TSepiSignature read FSignature;
  end;

  {*
    Type variant
    Note : il est impossible de cr�er un nouveau type variant.
    @author sjrd
    @version 1.0
  *}
  TSepiVariantType = class(TSepiType)
  protected
    function GetAlignment: Integer; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;
  end;

const
  /// Format du nom d'une m�thode surcharg�e
  OverloadedNameFormat = 'OL$%s$%d';

  /// Noms des param�tres cach�s
  HiddenParamNames: array[TSepiHiddenParamKind] of string = (
    '', 'Self', 'Result', '$Alloc', '$Free', 'High$'
  );

  /// Cha�nes des types de param�tre
  ParamKindStrings: array[TSepiParamKind] of string = (
    '', 'var', 'const', 'out'
  );

  /// Cha�nes des types de m�thode
  SignatureKindStrings: array[TSepiSignatureKind] of string = (
    'static procedure', 'static function',
    'object procedure', 'object function', 'constructor', 'destructor',
    'class procedure', 'class function', 'class constructor',
    'operator', 'property', 'class property', 'class destructor'
  );

  /// Cha�nes des types de liaison de m�thodes
  LinkKindStrings: array[TMethodLinkKind] of string = (
    '', 'virtual', 'dynamic', 'message', '', 'override'
  );

  /// Cha�nes des conventions d'appel
  CallingConventionStrings: array[TCallingConvention] of string = (
    'register', 'cdecl', 'pascal', 'stdcall', 'safecall'
  );

{$IF Declared(hfFieldSize)}
  /// Taille des champs cach�s dans un objet
  hfFieldSize = System.hfFieldSize;
{$ELSE}
  /// Taille des champs cach�s dans un objet
  hfFieldSize = 0;
{$IFEND}

implementation

uses
  SepiSystemUnit;

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
  RecordTypeDataLengthBase = 2*SizeOf(Cardinal)
    {$IF CompilerVersion >= 21} + SizeOf(Byte) + SizeOf(Integer) {$IFEND};
  IntfTypeDataLengthBase =
    SizeOf(Pointer) + SizeOf(TIntfFlagsBase) + SizeOf(TGUID) + 2*SizeOf(Word);
  ClassTypeDataLengthBase =
    SizeOf(TClass) + SizeOf(Pointer) + SizeOf(Smallint) + SizeOf(Word);
  PropInfoLengthBase = SizeOf(TPropInfo) - SizeOf(ShortString);
  InitTableLengthBase = 2*SizeOf(Byte) + 2*SizeOf(Cardinal);
  FieldTableLengthBase = SizeOf(TFieldTable);
  MethodTableLengthBase = SizeOf(TMethodTable);

  {$IF CompilerVersion >= 21}
  MetaClassTypeDataLength = SizeOf(PPTypeInfo);
  {$IFEND}

  vmtMinIndex = vmtSelfPtr;
  vmtMinMethodIndex = vmtParent + 4;

function FullSignature(const Signature: string; OfObject: Boolean): string;
begin
  if AnsiStartsText('procedure', Signature) or
    AnsiStartsText('function', Signature) then
  begin
    if OfObject then
      Result := 'object '+Signature
    else
      Result := 'static '+Signature;
  end else
  begin
    Result := Signature;
  end;
end;

procedure MakePropertyAccessKind(var Access: TSepiPropertyAccess);
begin
  with Access do
  begin
    if Component is TSepiField then
      Kind := pakField
    else if Component is TSepiMethod then
      Kind := pakMethod
    else if Component is TSepiClassField then
      Kind := pakClassField
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
    Storage.Component := nil;
  end else if AnsiSameText(AStorage, BooleanIdents[False]) then
  begin
    Storage.Kind := pskConstant;
    Storage.Stored := False;
    Storage.Component := nil;
  end else
  begin
    Storage.Stored := False;
    Storage.Component := Owner.LookForMember(AStorage);

    if Storage.Component is TSepiField then
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
constructor TSepiField.Load(AOwner: TSepiComponent; Stream: TStream);
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
constructor TSepiField.Create(AOwner: TSepiComponent; const AName: string;
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

{*
  [@inheritDoc]
*}
procedure TSepiField.WriteDigestData(Stream: TStream);
begin
  inherited;

  FieldType.WriteDigestToStream(Stream);
  Stream.WriteBuffer(FOffset, 4);
end;

{-------------------}
{ Classe TSepiParam }
{-------------------}

{*
  Cr�e un param�tre cach�
  @param AOwner        Propri�taire du param�tre
  @param AHiddenKind   Type de param�tre cach� (doit �tre diff�rent de pkNormal)
  @param AType         Type du param�tre (uniquement pour AKind = pkResult)
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
  FIsUntyped := False;

  case HiddenKind of
    hpSelf:
    begin
      if Owner.Context is TSepiClass then
        FType := TSepiClass(Owner.Context)
      else
        FType := (Owner.Root.SystemUnit as TSepiSystemUnit).TObject;
    end;
    hpResult:
    begin
      FKind := pkOut;
      FByRef := True;
      FType := AType;
    end;
    hpOpenArrayHighValue: FType :=
      (Owner.Root.SystemUnit as TSepiSystemUnit).Integer;
  else
    FType := (Owner.Root.SystemUnit as TSepiSystemUnit).Boolean;
  end;

  MakeFlags;
end;

{*
  Cr�e un param�tre depuis les donn�es de type d'une r�f�rence de m�thode
  En sortie, le pointeur ParamData a avanc� jusqu'au param�tre suivant
  @param AOwner      Propri�taire du param�tre
  @param ParamData   Pointeur vers les donn�es du param�tre
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
  AName := TypeInfoDecode(PShortString(ParamData)^);
  Inc(Longint(ParamData), PByte(ParamData)^ + 1);
  ATypeStr := TypeInfoDecode(PShortString(ParamData)^);
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
  Charge un param�tre depuis un flux
*}
constructor TSepiParam.Load(AOwner: TSepiSignature; Stream: TStream);
var
  AHasDefaultValue: Boolean;
begin
  inherited Create;

  FOwner := AOwner;
  FName := ReadStrFromStream(Stream);
  FLoading := True;

  Stream.ReadBuffer(FHiddenKind, 1);
  Stream.ReadBuffer(FKind, 1);
  Owner.OwningUnit.ReadRef(Stream, FType);
  FIsUntyped := FType is TSepiUntypedType;
  FByRef := (FKind in [pkVar, pkOut]) or FIsUntyped;

  FOpenArray := FType is TSepiOpenArrayType;
  if FOpenArray then
    FElementType := TSepiOpenArrayType(FType).ElementType;

  MakeFlags;
  Stream.ReadBuffer(FCallInfo, SizeOf(TSepiParamCallInfo));

  Stream.ReadBuffer(AHasDefaultValue, 1);
  if AHasDefaultValue then
  begin
    AllocDefaultValue;
    ReadDataFromStream(Stream, DefaultValuePtr^, ParamType.Size,
      ParamType.TypeInfo);
  end;
end;

{*
  Cr�e un ou plusieurs param�tre(s) depuis sa d�finition Delphi
  @param AOwner       Propri�taire du ou des param�tre(s)
  @param Definition   D�finition Delphi du ou des param�tre(s)
*}
class procedure TSepiParam.CreateFromString(AOwner: TSepiSignature;
  const Definition: string);
var
  NamePart, NamePart2, TypePart, KindStr, AName, ATypeStr: string;
  AKind: TSepiParamKind;
  AOpenArray: Boolean;
  AType: TSepiType;
begin
  SplitToken(Definition, ':', NamePart, TypePart);
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
    NamePart := NamePart2;
  end else
  begin
    AKind := pkValue;
    NamePart := KindStr;
  end;

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

  // Cr�ation du (ou des) param�tre(s)
  while SplitToken(NamePart, ',', AName, NamePart2) do
  begin
    Create(AOwner, Trim(AName), AType, AKind, AOpenArray);
    NamePart := NamePart2;
  end;
  Create(AOwner, Trim(AName), AType, AKind, AOpenArray);
end;

{*
  Cr�e un nouveau param�tre
  @param AOwner       Propri�taire du param�tre
  @param AName        Nom du param�tre
  @param AType        Type du param�tre
  @param AKind        Type de param�tre
  @param AOpenArray   True pour un param�tre tableau ouvert
*}
constructor TSepiParam.Create(AOwner: TSepiSignature; const AName: string;
  AType: TSepiType; AKind: TSepiParamKind = pkValue;
  AOpenArray: Boolean = False);
begin
  // Open array types must be created and customized in here
  Assert(not (AType is TSepiOpenArrayType));

  if AOpenArray then
  begin
    AType := TSepiOpenArrayType.Create(AOwner.OwningUnit, '', AType,
      HiddenParamNames[hpOpenArrayHighValue]+AName);
  end else if AType = nil then
  begin
    AType := (AOwner.Root.SystemUnit as TSepiSystemUnit).Untyped;
  end;

  inherited Create;

  FOwner := AOwner;
  FName := AName;
  FLoading := False;

  FHiddenKind := hpNormal;
  FKind := AKind;
  FType := AType;
  FIsUntyped := FType is TSepiUntypedType;
  FByRef := (FKind in [pkVar, pkOut]) or FIsUntyped;

  FOpenArray := FType is TSepiOpenArrayType;
  if FOpenArray then
    FElementType := TSepiOpenArrayType(FType).ElementType;

  MakeFlags;
end;

{*
  Cl�ne un param�tre
  @param AOwner       Propri�taire du param�tre
  @param AName        Nom du param�tre
  @param AType        Type du param�tre
  @param AKind        Type de param�tre
  @param AOpenArray   True pour un param�tre tableau ouvert
*}
constructor TSepiParam.Clone(AOwner: TSepiSignature; Source: TSepiParam);
begin
  inherited Create;

  FOwner := AOwner;

  FName        := Source.Name;
  FLoading     := True;
  FHiddenKind  := Source.HiddenKind;
  FKind        := Source.Kind;
  FByRef       := Source.ByRef;
  FType        := Source.ParamType;
  FIsUntyped   := Source.IsUntyped;
  FOpenArray   := Source.OpenArray;
  FElementType := Source.ElementType;
  FFlags       := Source.Flags;

  if Source.HasDefaultValue then
  begin
    AllocDefaultValue;
    ParamType.CopyData(Source.DefaultValuePtr^, DefaultValuePtr^);
  end;
end;

{*
  [@inheritDoc]
*}
destructor TSepiParam.Destroy;
begin
  if FDefaultValuePtr <> nil then
    ParamType.DisposeValue(FDefaultValuePtr);

  inherited;
end;

{*
  Construit la propri�t� Flags
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
    if (not IsUntyped) and (FType.ParamBehavior.AlwaysByAddress) then
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
var
  AHasDefaultValue: Boolean;
begin
  WriteStrToStream(Stream, Name);

  Stream.WriteBuffer(FHiddenKind, 1);
  Stream.WriteBuffer(FKind, 1);
  Owner.OwningUnit.WriteRef(Stream, FType);

  Stream.WriteBuffer(FCallInfo, SizeOf(TSepiParamCallInfo));

  AHasDefaultValue := HasDefaultValue;
  Stream.WriteBuffer(AHasDefaultValue, 1);
  if AHasDefaultValue then
    WriteDataToStream(Stream, DefaultValuePtr^, ParamType.Size,
      ParamType.TypeInfo);
end;

{*
  �crit les donn�es n�cessaires au digest de compatibilit� dans un flux
  @param Stream   Flux destination
*}
procedure TSepiParam.WriteDigestData(Stream: TStream);
begin
  Stream.WriteBuffer(FHiddenKind, SizeOf(TSepiHiddenParamKind));
  Stream.WriteBuffer(FKind, SizeOf(TSepiParamKind));
  ParamType.WriteDigestToStream(Stream);
end;

{*
  Indique si ce param�tre a une valeur par d�faut
  @return True si ce param�tre a une valeur par d�faut, False sinon
*}
function TSepiParam.GetHasDefaultValue: Boolean;
begin
  Result := FDefaultValuePtr <> nil;
end;

{*
  Description du param�tre
*}
function TSepiParam.GetDescription: string;
begin
  // Don't localize strings in this method

  case Kind of
    pkValue: Result := '';
    pkVar: Result := 'var ';
    pkConst: Result := 'const ';
    pkOut: Result := 'out ';
  end;

  Result := Result + Name;

  if not IsUntyped then
    Result := Result + ': ' + ParamType.DisplayName;

  if HasDefaultValue then
    Result := Result + ' = ...'; // TODO Print a nice default value in param
end;

{*
  [@inheritDoc]
*}
procedure TSepiParam.AfterConstruction;
begin
  inherited;

  FOwner.AddParam(Self);
  FLoading := False;
end;

{*
  Alloue une valeur par d�faut pour ce param�tre
*}
procedure TSepiParam.AllocDefaultValue;
begin
  if FDefaultValuePtr <> nil then
    Exit;

  Assert((HiddenKind = hpNormal) and (not ByRef) and (not OpenArray) and
    (ParamType <> nil));

  FDefaultValuePtr := ParamType.NewValue;
end;

{*
  D�termine si deux param�tres sont identiques
  @param AParam    Param�tre � comparer
  @param Options   Options de comparaison
  @return True si les param�tres sont identiques, False sinon
*}
function TSepiParam.Equals(AParam: TSepiParam;
  Options: TSepiSignatureCompareOptions = pcoAll): Boolean;
begin
  Result := False;

  if (pcoHiddenKind in Options) and (HiddenKind <> AParam.HiddenKind) then
    Exit;

  if HiddenKind = hpNormal then
  begin
    if (pcoKind in Options) and (Kind <> AParam.Kind) then
      Exit;

    if (pcoName in Options) and (not AnsiSameText(Name, AParam.Name)) then
      Exit;

    if pcoType in Options then
    begin
      if not ((IsUntyped and AParam.IsUntyped) or
        ParamType.Equals(AParam.ParamType)) then
        Exit;
    end;
  end;

  Result := True;
end;

{*
  D�termine si un type est compatible avec le param�tre
  @param AType   Type � Tester
  @return True si le type est compatible, False sinon
*}
function TSepiParam.CompatibleWith(AType: TSepiType): Boolean;
begin
  if IsUntyped then
    Result := True
  else if ByRef then
    Result := FType = AType
  else
    Result := FType.CompatibleWith(AType);
end;

{-----------------------}
{ Classe TSepiSignature }
{-----------------------}

{*
  Constructeur de base
  @param AOwner   Propri�taire de la signature
*}
constructor TSepiSignature.BaseCreate(AOwner: TSepiComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FRoot := FOwner.Root;
  FOwningUnit := FOwner.OwningUnit;

  if FOwner.Owner is TSepiType then
    FContext := TSepiType(FOwner.Owner);

  FAutoCreateHidden := True;

  FParams := TObjectList.Create(False);
  FActualParams := TObjectList.Create;
end;

{*
  Cr�e une signature � partir des donn�es de type d'un type m�thode
  @param AOwner      Propri�taire de la signature
  @param ATypeData   Donn�es de type
*}
constructor TSepiSignature.RegisterTypeData(AOwner: TSepiComponent;
  ATypeData: PTypeData);
const
  MethodKindToSignatureKind: array[TMethodKind] of TSepiSignatureKind = (
    skObjectProcedure, skObjectFunction, skConstructor, skDestructor,
    skClassProcedure, skClassFunction, skClassConstructor,
    {$IF Declared(mkClassDestructor)} skClassDestructor, {$IFEND}
    skOperator, skObjectProcedure, skObjectFunction
  );
var
  ParamData: Pointer;
  I: Integer;
begin
  BaseCreate(AOwner);

  FKind := MethodKindToSignatureKind[ATypeData.MethodKind];
  ParamData := @ATypeData.ParamList;

  for I := 1 to ATypeData.ParamCount do
    TSepiParam.RegisterParamData(Self, ParamData);

  FCallingConvention := ccRegister;

  if Kind in skWithReturnType then
    FReturnType := Root.FindType(TypeInfoDecode(PShortString(ParamData)^))
  else
    FReturnType := nil;

  Complete;
end;

{*
  Charge une signature depuis un flux
  @param AOwner   Propri�taire de la signature
  @param Stream   Flux depuis lequel charger la signature
*}
constructor TSepiSignature.Load(AOwner: TSepiComponent; Stream: TStream);
var
  Count: Integer;
begin
  BaseCreate(AOwner);

  FLoadingCloning := True;
  FAutoCreateHidden := False;

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

  Complete;
end;

{*
  Cr�e une signature de m�thode
  @param AOwner               Propri�taire de la signature
  @param ASignature           Signature Delphi
  @param ACallingConvention   Convention d'appel
*}
constructor TSepiSignature.Create(AOwner: TSepiComponent;
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

  FReturnType := nil;
  FCallingConvention := ACallingConvention;

  // Type de m�thode
  ParamPos := MultiPos(['(', '[', ':'], ASignature);
  FKind := TSepiSignatureKind(AnsiIndexText(
    Trim(Copy(ASignature, 1, ParamPos-1)), SignatureKindStrings));
  Assert(Ord(FKind) <> -1);

  // Type de retour
  if Kind in skWithReturnType then
  begin
    ReturnTypePos := RightPos(':', ASignature);
    FReturnType := FOwner.Root.FindType(
      Trim(Copy(ASignature, ReturnTypePos+1, MaxInt)));
  end else
    ReturnTypePos := Length(ASignature);

  // Param�tres
  if ParamPos < ReturnTypePos then
  begin
    while not CharInSet(ASignature[ParamPos], [')', ']', ':']) do
    begin
      Inc(ParamPos);
      ParamEnd := MultiPos([';', ')', ']', ':'], ASignature, ParamPos);
      TSepiParam.CreateFromString(Self,
        Copy(ASignature, ParamPos, ParamEnd-ParamPos));
      ParamPos := ParamEnd;
    end;
  end;

  Complete;
end;

{*
  Cr�e une signature en construction
  @param ASepiUnit   Unit� contenante
  @param AContext    Contexte (classe ou interface)
*}
constructor TSepiSignature.CreateConstructing(ASepiUnit: TSepiUnit;
  AContext: TSepiType = nil);
begin
  inherited Create;

  FOwner := nil;
  FRoot := ASepiUnit.Root;
  FOwningUnit := ASepiUnit;
  FContext := AContext;
  FAutoCreateHidden := True;

  FParams := TObjectList.Create(False);
  FActualParams := TObjectList.Create;
end;

{*
  Cl�ne une signature
  @param AOwner   Propri�taire
  @param Source   Signature � cl�ner
*}
constructor TSepiSignature.Clone(AOwner: TSepiComponent;
  Source: TSepiSignature);
var
  I: Integer;
begin
  BaseCreate(AOwner);

  FLoadingCloning := True;
  FAutoCreateHidden := False;
  FKind := Source.Kind;
  FReturnType := Source.ReturnType;
  FCallingConvention := Source.CallingConvention;

  for I := 0 to Source.ActualParamCount-1 do
    TSepiParam.Clone(Self, Source.ActualParams[I]);

  Complete;
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
  V�rifie que la signature n'est pas encore compl�t�e
*}
procedure TSepiSignature.CheckNotCompleted;
begin
  if Completed then
    raise ESepiAlreadyCompleted.Create(SSignatureAlreadyCompleted);
end;

{*
  Recense l'ajout d'un param�tre
  @param Param   Param�tre ajout�
*}
procedure TSepiSignature.AddParam(Param: TSepiParam);
begin
  CheckNotCompleted;

  if FLoadingCloning then
  begin
    FActualParams.Add(Param);
    if Param.HiddenKind = hpNormal then
      FParams.Add(Param);
  end else
  begin
    if Param.HiddenKind in [hpAlloc, hpFree] then
      FActualParams.Insert(0, Param)
    else if (Param.HiddenKind = hpSelf) and (CallingConvention <> ccPascal) then
      FActualParams.Insert(0, Param)
    else
    begin
      FActualParams.Add(Param);
      if Param.HiddenKind = hpNormal then
        FParams.Add(Param);

      if Param.OpenArray and FAutoCreateHidden then
        TSepiParam.CreateHidden(Self, hpOpenArrayHighValue);
    end;
  end;
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
  V�rifie que la signature peut h�riter d'une autre
  CheckInherited met � jour la convention d'appel si besoin est, contrairement
  � Equals.
  @param ASignature   Signature � comparer
  @return True si les signatures sont identiques, False sinon
*}
function TSepiSignature.CheckInherited(
  ASignature: TSepiSignature): Boolean;
begin
  Result := Equals(ASignature, scoInheritance);

  if Result and (CallingConvention <> ASignature.CallingConvention) then
  begin
    FCallingConvention := ASignature.CallingConvention;
    MakeCallInfo;
  end;
end;

{*
  Modifie le type de signature
  @param Value   Nouveau type de signature
*}
procedure TSepiSignature.SetKind(Value: TSepiSignatureKind);
begin
  CheckNotCompleted;
  FKind := Value;
end;

{*
  Modifie le type de retour
  @param Value   Nouveau type de retour
*}
procedure TSepiSignature.SetReturnType(Value: TSepiType);
begin
  CheckNotCompleted;
  FReturnType := Value;
end;

{*
  Modifie la convention d'appel
  @param Value   Nouvelle convention d'appel
*}
procedure TSepiSignature.SetCallingConvention(Value: TCallingConvention);
begin
  CheckNotCompleted;
  FCallingConvention := Value;
end;

{*
  Nombre de param�tres d�clar�s (visibles)
  @return Nombre de param�tres
*}
function TSepiSignature.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

{*
  Tableau zero-based des param�tres d�clar�s (visibles)
  @param Index   Index du param�tre � r�cup�rer
  @return Param�tre situ� � l'index Index
*}
function TSepiSignature.GetParams(Index: Integer): TSepiParam;
begin
  Result := TSepiParam(FParams[Index]);
end;

{*
  Nombre de param�tres r�els (incluant les param�tres cach�s)
  @return Nombre de param�tres r�els
*}
function TSepiSignature.GetActualParamCount: Integer;
begin
  Result := FActualParams.Count;
end;

{*
  Tableau zero-based des param�tres r�els (incluant les param�tres cach�s)
  @param Index   Index du param�tre � r�cup�rer
  @return Param�tre situ� � l'index Index
*}
function TSepiSignature.GetActualParams(Index: Integer): TSepiParam;
begin
  Result := TSepiParam(FActualParams[Index]);
end;

{*
  Tableau des param�tres index�s par leurs noms
  @param ParamName   Nom du param�tre recherch�
  @return Param�tre dont le nom est ParamName
  @throws ESepiComponentNotFoundError Le param�tre n'a pas �t� trouv�
*}
function TSepiSignature.GetParamByName(const ParamName: string): TSepiParam;
begin
  Result := GetParam(ParamName);
  if Result = nil then
    raise ESepiComponentNotFoundError.CreateFmt(
      SSepiComponentNotFound, [ParamName]);
end;

{*
  Param�tres cach�s d'apr�s leur type
  @param Kind   Type de param�tre cach�
  @return Le param�tre cach� correspondant, ou nil s'il n'y en a pas
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
function TSepiSignature.GetDescription: string;
var
  I: Integer;
begin
  Result := SignatureKindStrings[Kind];

  if ParamCount > 0 then
  begin
    Result := Result + '(';

    for I := 0 to ParamCount-1 do
    begin
      if I <> 0 then
        Result := Result + '; ';
      Result := Result + Params[I].Description;
    end;

    Result := Result + ')';
  end;

  if ReturnType <> nil then
    Result := Result + ': ' + ReturnType.DisplayName;

  if CallingConvention <> ccRegister then
  begin
    Result := Result + '; ' + LowerCase(Copy(
      GetEnumName(TypeInfo(TCallingConvention), Integer(CallingConvention)),
      3, MaxInt));
  end;
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
  �crit les donn�es n�cessaires au digest de compatibilit� dans un flux
  @param Stream   Flux destination
*}
procedure TSepiSignature.WriteDigestData(Stream: TStream);
var
  I, Count: Integer;
begin
  Stream.WriteBuffer(FKind, 1);
  ReturnType.WriteDigestToStream(Stream);
  Stream.WriteBuffer(FCallingConvention, 1);

  Count := ActualParamCount;
  Stream.WriteBuffer(Count, 4);
  for I := 0 to Count-1 do
    ActualParams[I].WriteDigestData(Stream);
end;

{*
  Compl�te la signature
*}
procedure TSepiSignature.Complete;
begin
  CheckNotCompleted;

  try
    if FAutoCreateHidden then
    begin
      if (ReturnType <> nil) and (ReturnType.ResultBehavior = rbParameter) then
        TSepiParam.CreateHidden(Self, hpResult, ReturnType);

      if Kind in skWithSelfParam then
      begin
        if Kind = skConstructor then
          TSepiParam.CreateHidden(Self, hpAlloc)
        else if Kind = skDestructor then
          TSepiParam.CreateHidden(Self, hpFree);

        TSepiParam.CreateHidden(Self, hpSelf);
      end;
    end;
  finally
    FCompleted := True;
  end;

  MakeCallInfo;
end;

{*
  Cherche un param�tre par son nom
  @param ParamName   Nom du param�tre
  @return Param�tre correspondant, ou nil si non trouv�
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
  D�termine si deux signatures sont identiques
  @param ASignature   Signature � comparer
  @param Options      Options de comparaison
  @return True si les signatures sont identiques, False sinon
*}
function TSepiSignature.Equals(ASignature: TSepiSignature;
  Options: TSepiSignatureCompareOptions = scoAll): Boolean;
var
  I: Integer;
begin
  Result := False;

  if (scoKind in Options) and (Kind <> ASignature.Kind) then
    Exit;
  if (scoCallingConvention in Options) and
    (CallingConvention <> ASignature.CallingConvention) then
    Exit;

  if scoParams in Options then
  begin
    if ParamCount <> ASignature.ParamCount then
      Exit;
    for I := 0 to ParamCount-1 do
      if not Params[I].Equals(ASignature.Params[I], Options) then
        Exit;
  end;

  if (scoReturnType in Options) and (ReturnType <> ASignature.ReturnType) then
    Exit;

  Result := True;
end;

{*
  D�termine si une liste de types est compatible avec la signature
  @param ATypes   Liste des types � tester
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
  Charge une meta-m�thode depuis un flux
*}
constructor TSepiMethod.Load(AOwner: TSepiComponent; Stream: TStream);
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

  LoadChildren(Stream);

  if not IsAbstract then
  begin
    if (Owner is TSepiClass) and TSepiClass(Owner).Native then
      FindNativeCode;

    if (not Assigned(FCode)) and Assigned(OwningUnit.OnGetMethodCode) then
      OwningUnit.OnGetMethodCode(Self, FCode, FCodeHandler);

    if not Assigned(FCode) then
      FCode := @FCodeJumper;
  end;
end;

{*
  Cr�e une nouvelle m�thode
  @param AOwner       Propri�taire de la m�thode
  @param AName        Nom de la m�thode
  @param ACode        Pointeur sur le code de la m�thode
  @param ASignature   Signature Delphi de la m�thode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la m�thode est abstraite
  @param AMsgID       Pour les m�thodes de message, le message intercept�
  @param ACallingConvention   Convention d'appel de la m�thode
*}
constructor TSepiMethod.Create(AOwner: TSepiComponent; const AName: string;
  ACode: Pointer; const ASignature: string;
  ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
  AMsgID: Integer = 0; ACallingConvention: TCallingConvention = ccRegister);
begin
  inherited Create(AOwner, AName);

  FSignature := TSepiSignature.Create(Self,
    FullSignature(ASignature, not (Owner is TSepiUnit)), ACallingConvention);

  CommonCreate(ACode, ALinkKind, AAbstract, AMsgID);
end;

{*
  Cr�e une nouvelle m�thode d'apr�s une signature d�j� construite
  @param AOwner       Propri�taire de la m�thode
  @param AName        Nom de la m�thode
  @param ACode        Pointeur sur le code de la m�thode
  @param ASignature   Signature
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la m�thode est abstraite
  @param AMsgID       Pour les m�thodes de message, le message intercept�
*}
constructor TSepiMethod.Create(AOwner: TSepiComponent; const AName: string;
  ACode: Pointer; ASignature: TSepiSignature;
  ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
  AMsgID: Integer = 0);
begin
  inherited Create(AOwner, AName);

  FSignature := TSepiSignature.Clone(Self, ASignature);

  CommonCreate(ACode, ALinkKind, AAbstract, AMsgID);
end;

{*
  Cr�e une nouvelle m�thode surcharg�e
  @param AOwner       Propri�taire de la m�thode
  @param AName        Nom de la m�thode
  @param ACode        Pointeur sur le code de la m�thode
  @param ASignature   Signature Delphi de la m�thode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la m�thode est abstraite
  @param AMsgID       Pour les m�thodes de message, le message intercept�
  @param ACallingConvention   Convention d'appel de la m�thode
*}
constructor TSepiMethod.CreateOverloaded(AOwner: TSepiComponent;
  const AName: string; ACode: Pointer; const ASignature: string;
  ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
  AMsgID: Integer = 0; ACallingConvention: TCallingConvention = ccRegister);
begin
  FIsOverloaded := True;
  FOverloaded := AOwner.GetComponent(AName) as TSepiOverloadedMethod;

  if FOverloaded = nil then
    FOverloaded := TSepiOverloadedMethod.Create(AOwner, AName);
  FOverloadIndex := FOverloaded.MethodCount;

  Create(AOwner, Format(OverloadedNameFormat, [AName, FOverloadIndex]),
    ACode, ASignature, ALinkKind, AAbstract, AMsgID, ACallingConvention);
end;

{*
  Cr�e une nouvelle m�thode surcharg�e d'apr�s une signature d�j� construite
  @param AOwner       Propri�taire de la m�thode
  @param AName        Nom de la m�thode
  @param ACode        Pointeur sur le code de la m�thode
  @param ASignature   Signature Delphi de la m�thode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la m�thode est abstraite
  @param AMsgID       Pour les m�thodes de message, le message intercept�
*}
constructor TSepiMethod.CreateOverloaded(AOwner: TSepiComponent;
  const AName: string; ACode: Pointer; ASignature: TSepiSignature;
  ALinkKind: TMethodLinkKind = mlkStatic; AAbstract: Boolean = False;
  AMsgID: Integer = 0);
begin
  FIsOverloaded := True;
  FOverloaded := AOwner.GetComponent(AName) as TSepiOverloadedMethod;

  if FOverloaded = nil then
    FOverloaded := TSepiOverloadedMethod.Create(AOwner, AName);
  FOverloadIndex := FOverloaded.MethodCount;

  Create(AOwner, Format(OverloadedNameFormat, [AName, FOverloadIndex]),
    ACode, ASignature, ALinkKind, AAbstract, AMsgID);
end;

{*
  D�truit l'instance
*}
destructor TSepiMethod.Destroy;
begin
  FSignature.Free;
  inherited Destroy;
end;

{*
  Met au point la liaison et l'index de liaison
  Par extension, recherche aussi la m�thode h�rit�e.
*}
procedure TSepiMethod.MakeLink;
var
  OwningClass, Ancestor: TSepiClass;
  Component: TSepiComponent;
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
      Component := OwningClass.Parent.LookForMember(RealName, OwningClass);
      if Component is TSepiMethod then
      begin
        if Signature.CheckInherited(TSepiMethod(Component).Signature) then
          FInherited := TSepiMethod(Component);
      end else if Component is TSepiOverloadedMethod then
        FInherited := TSepiOverloadedMethod(Component).FindInherited(Signature);
    end else
    begin
      // Looking for an ancestor method which intercepts the same message ID
      Ancestor := OwningClass.Parent;
      while (FInherited = nil) and (Ancestor <> nil) do
      begin
        for I := 0 to Ancestor.ChildCount-1 do
        begin
          Component := Ancestor.Children[I];
          if (Component is TSepiMethod) and
            (TSepiMethod(Component).LinkKind = mlkMessage) and
            (TSepiMethod(Component).MsgID = FLinkIndex) then
          begin
            FInherited := TSepiMethod(Component);
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
      Assert(FInherited <> nil);
      FLinkKind := FInherited.LinkKind;
      FLinkIndex := FInherited.FLinkIndex;
    end;
  end;
end;

{*
  Cherche le code d'une m�thode native
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
  Partie commune � tous les constructeurs
*}
procedure TSepiMethod.CommonCreate(ACode: Pointer; ALinkKind: TMethodLinkKind;
  AAbstract: Boolean; AMsgID: Integer);
begin
  FCode := ACode;
  FCodeHandler := nil;

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
  Nom r�el, tel que d�clar� (ind�pendant d'une surcharge ou non)
  @return Nom r�el
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

  SaveChildren(Stream);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethod.WriteDigestData(Stream: TStream);
begin
  inherited;

  Signature.WriteDigestData(Stream);
  Stream.WriteBuffer(FLinkKind, 1);
  Stream.WriteBuffer(FAbstract, 1);
  Stream.WriteBuffer(FLinkIndex, 2);
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
  Donne l'adresse de d�but du code de la m�thode
  Cette m�thode ne peut �tre appel�e qu'une seule fois par m�thode, et
  seulement pour les m�thodes non natives.
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
  Donne l'adresse de d�but du code de la m�thode, � partir d'une m�thode
  Cette m�thode ne peut �tre appel�e qu'une seule fois par m�thode, et
  seulement pour les m�thodes non natives.
  @param AMethod        M�thode de code
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
  Charge une m�thode surcharg�e depuis un flux
*}
constructor TSepiOverloadedMethod.Load(AOwner: TSepiComponent;
  Stream: TStream);
begin
  inherited;
  FMethods := TObjectList.Create(False);
end;

{*
  Cr�e une nouvelle m�thode surcharg�e
  @param AOwner   Propri�taire de la m�thode
  @param AName    Nom de la m�thode
*}
constructor TSepiOverloadedMethod.Create(AOwner: TSepiComponent;
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
  Trouve la m�thode effective dont une signature donn�e h�rite
  @param ASignature   Signature � rechercher
  @return M�thode effective correspondante
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
  Nombre de m�thodes de m�me nom
  @return Nombre de m�thodes de m�me nom
*}
function TSepiOverloadedMethod.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
end;

{*
  Tableau zero-based des m�thodes de m�me nom
  @param Index   Index de la m�thode
  @return M�thode de m�me nom � l'index sp�cifi�
*}
function TSepiOverloadedMethod.GetMethods(
  Index: Integer): TSepiMethod;
begin
  Result := TSepiMethod(FMethods[Index]);
end;

{*
  [@inheritDoc]
*}
function TSepiOverloadedMethod.IsVisibleFrom(
  FromComponent: TSepiComponent): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to MethodCount-1 do
    if Methods[I].IsVisibleFrom(FromComponent) then
      Exit;
  Result := False;
end;

{*
  Trouve la m�thode effective qui correspond � une signature donn�e
  @param ASignature   Signature � rechercher
  @param Options      Options de comparaison
  @return M�thode effective correspondante
*}
function TSepiOverloadedMethod.FindMethod(ASignature: TSepiSignature;
  Options: TSepiSignatureCompareOptions = scoAll): TSepiMethod;
var
  I: Integer;
begin
  for I := 0 to MethodCount-1 do
  begin
    Result := Methods[I];
    if Result.Signature.Equals(ASignature, Options) then
      Exit;
  end;
  Result := nil;
end;

{*
  Trouve la m�thode effective qui correspond � une liste de types de param�tres
  @param ATypes   Liste des types de param�tres
  @return M�thode effective correspondante
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
  Charge une propri�t� depuis un flux
*}
constructor TSepiProperty.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  FSignature := TSepiSignature.Load(Self, Stream);
  LoadChildren(Stream);

  OwningUnit.ReadRef(Stream, FReadAccess.Component);
  MakePropertyAccessKind(FReadAccess);

  OwningUnit.ReadRef(Stream, FWriteAccess.Component);
  MakePropertyAccessKind(FWriteAccess);

  Stream.ReadBuffer(FIndex, 4);
  Stream.ReadBuffer(FDefaultValue, 4);

  Stream.ReadBuffer(FStorage.Kind, 1);
  if FStorage.Kind = pskConstant then
    Stream.ReadBuffer(FStorage.Stored, 1)
  else
    OwningUnit.ReadRef(Stream, FStorage.Component);

  Stream.ReadBuffer(FIsDefault, 1);
end;

{*
  Cr�e une nouvelle propri�t�
  @param AOwner          Propri�taire de la propri�t�
  @param AName           Nom de la propri�t�
  @param ASignature      Signature
  @param AReadAccess     Acc�s en lecture � la propri�t� (peut �tre nil)
  @param AWriteAccess    Acc�s en �criture � la propri�t� (peut �tre nil)
  @param AIndex          Index d'acc�s
  @param ADefaultValue   Valeur par d�faut de la propri�t�
  @param AStorage        Sp�cificateur de stockage
  @param AIsDefault      Indique si c'est la propri�t� tableau par d�faut
*}
constructor TSepiProperty.Create(AOwner: TSepiComponent; const AName: string;
  ASignature: TSepiSignature; AReadAccess, AWriteAccess: TSepiMember;
  AIndex, ADefaultValue: Integer; const AStorage: TSepiPropertyStorage;
  AIsDefault: Boolean);
begin
  inherited Create(AOwner, AName);

  FSignature := TSepiSignature.Clone(Self, ASignature);

  FReadAccess.Component := AReadAccess;
  MakePropertyAccessKind(FReadAccess);

  FWriteAccess.Component := AWriteAccess;
  MakePropertyAccessKind(FWriteAccess);

  FIndex := AIndex;

  FDefaultValue := ADefaultValue;
  FStorage := AStorage;

  FIsDefault := AIsDefault and (Signature.ParamCount > 0);
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
constructor TSepiProperty.Create(AOwner: TSepiComponent;
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIndex: Integer = NoIndex; ADefaultValue: Integer = NoDefaultValue;
  const AStorage: string = ''; AIsDefault: Boolean = False);

  function FindAccess(const AAccess: string): TSepiMember;
  begin
    Result := (Owner as TSepiContainerType).LookForMember(AAccess);
  end;

begin
  inherited Create(AOwner, AName);

  FSignature := TSepiSignature.Create(Self, ASignature);

  FReadAccess.Component := FindAccess(AReadAccess);
  MakePropertyAccessKind(FReadAccess);

  FWriteAccess.Component := FindAccess(AWriteAccess);
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
constructor TSepiProperty.Create(AOwner: TSepiComponent;
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIsDefault: Boolean);
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
constructor TSepiProperty.Redefine(AOwner: TSepiComponent; const AName: string;
  AReadAccess, AWriteAccess: TSepiMember; ADefaultValue: Integer;
  const AStorage: TSepiPropertyStorage);
var
  Previous: TSepiProperty;
begin
  Previous := (AOwner as TSepiClass).Parent.LookForMember(
    AName, AOwner) as TSepiProperty;

  inherited Create(AOwner, AName);

  FSignature := TSepiSignature.Clone(Self, Previous.Signature);

  FReadAccess.Component := AReadAccess;
  MakePropertyAccessKind(FReadAccess);

  FWriteAccess.Component := AWriteAccess;
  MakePropertyAccessKind(FWriteAccess);

  FIndex := Previous.Index;
  
  FDefaultValue := ADefaultValue;
  FStorage := AStorage;

  FIsDefault := Previous.IsDefault;
end;

{*
  Red�finit une propri�t� h�rit�e
  @param AOwner         Propri�taire de la propri�t�
  @param AName          Nom de la propri�t�
  @param AReadAccess    Acc�s en lecture � la propri�t�
  @param AWriteAccess   Acc�s en �criture � la propri�t�
  @param AStorage       Sp�cificateur de stockage
*}
constructor TSepiProperty.Redefine(AOwner: TSepiComponent;
  const AName: string; const AReadAccess: string = '';
  const AWriteAccess: string = ''; const AStorage: string = '');
var
  AContainer: TSepiInheritableContainerType;
  Previous: TSepiProperty;
  BReadAccess, BWriteAccess: TSepiMember;
  BStorage: TSepiPropertyStorage;
begin
  AContainer := AOwner as TSepiInheritableContainerType;
  Previous := AContainer.ParentContainer.LookForMember(
    AName, AOwner) as TSepiProperty;

  if AReadAccess = '' then
    BReadAccess := Previous.ReadAccess.Component
  else
    BReadAccess := AContainer.LookForMember(AReadAccess);

  if AWriteAccess = '' then
    BWriteAccess := Previous.WriteAccess.Component
  else
    BWriteAccess := AContainer.LookForMember(AWriteAccess);

  if AStorage = '' then
    BStorage := Previous.Storage
  else
    MakePropertyStorage(BStorage, AStorage, AOwner as TSepiClass);

  Redefine(AOwner, AName, BReadAccess, BWriteAccess,
    Previous.DefaultValue, BStorage);
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
constructor TSepiProperty.Redefine(AOwner: TSepiComponent;
  const AName, AReadAccess, AWriteAccess: string; ADefaultValue: Integer;
  const AStorage: string = '');
begin
  Redefine(AOwner, AName, AReadAccess, AWriteAccess, AStorage);

  FDefaultValue := ADefaultValue;
end;

{*
  D�truit l'instance
*}
destructor TSepiProperty.Destroy;
begin
  FSignature.Free;
  inherited;
end;

{*
  Construit les informations de propri�t�s pour les RTTI
  Cette m�thode ne doit �tre appel�e que pour des propri�t�s d'objet, pas pour
  des propri�t�s d'interface ou de classe.
  @param PropInfo   Destination des informations
*}
procedure TSepiProperty.MakePropInfo(PropInfo: PPropInfo);
var
  EncName: TypeInfoString;
begin
  Assert(Owner is TSepiClass);
  Assert(Signature.Kind = skProperty);

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
      pskConstant: StoredProc := Pointer(Stored);
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
  PropInfo.NameIndex := FNameIndex;

  // Property name
  EncName := TypeInfoEncode(Name);
  Move(EncName[0], PropInfo.Name[0], Length(EncName)+1);
end;

{*
  Type de la propri�t�
  @return Type de la propri�t�
*}
function TSepiProperty.GetPropType: TSepiType;
begin
  Result := Signature.ReturnType;
end;

{*
  Indique si c'est une propri�t� de classe
  @return True si c'est une propri�t� de classe, False sinon
*}
function TSepiProperty.GetIsClassProperty: Boolean;
begin
  Result := Signature.Kind = skClassProperty;
end;

{*
  [@inheritDoc]
*}
procedure TSepiProperty.ListReferences;
begin
  inherited;

  Signature.ListReferences;
  OwningUnit.AddRef(FReadAccess.Component);
  OwningUnit.AddRef(FWriteAccess.Component);

  if FStorage.Kind <> pskConstant then
    OwningUnit.AddRef(FStorage.Component);
end;

{*
  [@inheritDoc]
*}
procedure TSepiProperty.Save(Stream: TStream);
begin
  inherited;

  Signature.Save(Stream);
  SaveChildren(Stream);

  OwningUnit.WriteRef(Stream, FReadAccess.Component);
  OwningUnit.WriteRef(Stream, FWriteAccess.Component);

  Stream.WriteBuffer(FIndex, 4);
  Stream.WriteBuffer(FDefaultValue, 4);

  Stream.WriteBuffer(FStorage.Kind, 1);
  if FStorage.Kind = pskConstant then
    Stream.WriteBuffer(FStorage.Stored, 1)
  else
    OwningUnit.WriteRef(Stream, FStorage.Component);

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

{*
  [@inheritDoc]
*}
procedure TSepiProperty.WriteDigestData(Stream: TStream);
begin
  inherited;

  Signature.WriteDigestData(Stream);
  FReadAccess.Component.WriteDigestToStream(Stream);
  FWriteAccess.Component.WriteDigestToStream(Stream);
  Stream.WriteBuffer(FIndex, 4);
end;

{------------------------}
{ Classe TSepiRecordType }
{------------------------}

{*
  Charge un type record depuis un flux
*}
constructor TSepiRecordType.Load(AOwner: TSepiComponent; Stream: TStream);
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
constructor TSepiRecordType.Create(AOwner: TSepiComponent; const AName: string;
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
  [@inheritDoc]
*}
constructor TSepiRecordType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
var
  SrcRec: TSepiRecordType;
  I: Integer;
begin
  SrcRec := Source as TSepiRecordType;

  Create(AOwner, AName, SrcRec.IsPacked);

  for I := 0 to SrcRec.ChildCount-1 do
    if SrcRec.Children[I] is TSepiField then
      with TSepiField(SrcRec.Children[I]) do
        TSepiField.Create(Self, Name, FieldType, Offset);

  Complete;
end;

{*
  Ajoute un champ au record
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param After       Champ pr�c�dent en m�moire
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
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
{$IF CompilerVersion >= 21}
type
  PDataEx = ^TDataEx;
  TDataEx = packed record
    OpCount: Byte; // always 0
    FieldCount: Integer;
    FieldList: array[0..0] of TRecordTypeField;
  end;
{$IFEND}
var
  Field: TSepiField;
  I, FieldCount, ManagedCount, TypeDataLength: Integer;
  RecordData: PRecordTypeData;
  ManagedField: PManagedField;
{$IF CompilerVersion >= 21}
  DataEx: PDataEx;
  FieldData: PRecordTypeField;
{$IFEND}
begin
{$IF CompilerVersion < 21}
  if not NeedInit then
    Exit;
{$IFEND}

  // Measurements
  FieldCount := 0;
  ManagedCount := 0;
  TypeDataLength := RecordTypeDataLengthBase;

  for I := 0 to FieldCount-1 do
  begin
    Field := Fields[I];
    Inc(FieldCount);

    if Field.FieldType.NeedInit then
    begin
      Inc(ManagedCount);
      Inc(TypeDataLength, SizeOf(TManagedField));
    end;

    {$IF CompilerVersion >= 21}
    Inc(TypeDataLength, SizeOf(TManagedField) + SizeOf(Byte) +
      Field.TypeInfoNameSize);
    {$IFEND}
  end;

  // Allocate type info
  AllocateTypeInfo(TypeDataLength);
  RecordData := PRecordTypeData(TypeData);
  ManagedField := @RecordData.ManagedFields[0];
  {$IF CompilerVersion >= 21}
  DataEx := PDataEx(@RecordData.ManagedFields[ManagedCount]);
  FieldData := @DataEx.FieldList[0];
  {$IFEND}

  // Basic information
  RecordData.Size := FSize;
  RecordData.ManagedCount := ManagedCount;
  {$IF CompilerVersion >= 21}
  DataEx.OpCount := 0; // Sepi does not know about record ops anyway
  DataEx.FieldCount := FieldCount;
  {$IFEND}

  // Field information
  for I := 0 to FieldCount-1 do
  begin
    Field := Fields[I];

    if Field.FieldType.NeedInit then
    begin
      ManagedField.TypeRef := TSepiRecordType(Field.FieldType).TypeInfoRef;
      ManagedField.FldOffset := Field.Offset;
      Inc(ManagedField);
    end;

    {$IF CompilerVersion >= 21}
    FieldData.Field.TypeRef := TSepiRecordType(Field.FieldType).TypeInfoRef;
    FieldData.Field.FldOffset := Field.Offset;
    FieldData.Flags := 0; // Unknown
    FieldData := Field.StoreTypeInfoName(@FieldData.Name);
    {$IFEND}
  end;
end;

{*
  Nombre de champs
  @return Nombre de champs
*}
function TSepiRecordType.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

{*
  Tableau zero-based des champs
  @param Index   Index de champ compris entre 0 inclus et FieldCount exclu
  @return Champ � l'index sp�cifi�
*}
function TSepiRecordType.GetFields(Index: Integer): TSepiField;
begin
  Result := TSepiField(FFields[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRecordType.ChildAdded(Child: TSepiComponent);
begin
  inherited;

  if Child is TSepiField then
  begin
    FFields.Add(Child);

    with TSepiField(Child) do
    begin
      if FieldType.NeedInit then
        FNeedInit := True;
      if Offset + FieldType.Size > FSize then
        FSize := Offset + FieldType.Size;
      if (not IsPacked) and (FieldType.Alignment > FAlignment) then
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
procedure TSepiRecordType.WriteDigestData(Stream: TStream);
var
  I: Integer;
begin
  inherited;

  for I := 0 to ChildCount-1 do
    if Children[I] is TSepiField then
      Children[I].WriteDigestToStream(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.GetAlignment: Integer;
begin
  Result := FAlignment;
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.GetDescription: string;
begin
  Result := 'record';
end;

{*
  Ajoute un champ au record
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param ForcePack   Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.AddField(const FieldName: string;
  FieldType: TSepiType; ForcePack: Boolean = False): TSepiField;
var
  LastField: TSepiField;
begin
  if FieldCount = 0 then
    LastField := nil
  else
    LastField := Fields[FieldCount-1];

  Result := PrivAddField(FieldName, FieldType, LastField, ForcePack);
end;

{*
  Ajoute un champ au record
  @param FieldName       Nom du champ
  @param FieldTypeInto   RTTI du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
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
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.AddField(const FieldName, FieldTypeName: string;
  ForcePack: Boolean = False): TSepiField;
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
function TSepiRecordType.AddFieldAfter(const FieldName: string;
  FieldType: TSepiType; const After: string;
  ForcePack: Boolean = False): TSepiField;
begin
  Result := PrivAddField(FieldName, FieldType,
    FindComponent(After) as TSepiField, ForcePack);
end;

{*
  Ajoute un champ au record apr�s un champ donn� en m�moire
  @param FieldName       Nom du champ
  @param FieldTypeInfo   RTTI du type du champ
  @param After           Nom du champ pr�c�dent en m�moire (vide pour le d�but)
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiRecordType.AddFieldAfter(const FieldName: string;
  FieldTypeInfo: PTypeInfo; const After: string;
  ForcePack: Boolean = False): TSepiField;
begin
  Result := PrivAddField(FieldName, Root.FindType(FieldTypeInfo),
    FindComponent(After) as TSepiField, ForcePack);
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
  const FieldName, FieldTypeName, After: string;
  ForcePack: Boolean = False): TSepiField;
begin
  Result := PrivAddField(FieldName, Root.FindType(FieldTypeName),
    FindComponent(After) as TSepiField, ForcePack);
end;

{*
  Termine le record et construit ses RTTI si ce n'est pas d�j� fait
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

  if not Native then
    MakeTypeInfo;

  TSepiRecordType(Owner).ReAddChild(Self);
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.Equals(Other: TSepiType): Boolean;
begin
  Result := Self = Other;
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := Self = AType;
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.ValueToString(const Value): string;
var
  ValuePtr: Cardinal;
  I: Integer;
  Field: TSepiField;
  FieldValue: string;
begin
  if FieldCount = 0 then
  begin
    Result := '()';
    Exit;
  end;

  ValuePtr := Cardinal(@Value);
  Result := '';

  for I := 0 to FieldCount-1 do
  begin
    Field := Fields[I];
    FieldValue := Field.FieldType.ValueToString(
      Pointer(ValuePtr+Field.Offset)^);
    Result := Result + Format(' %s: %s;', [Field.Name, FieldValue]);
  end;

  Result[1] := '(';
  Result[Length(Result)] := ')';
end;

{-------------------------------------}
{ TSepiInheritableContainerType class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
function TSepiInheritableContainerType.InternalLookFor(const Name: string;
  FromComponent: TSepiComponent): TSepiComponent;
begin
  // Look for a member first
  Result := LookForMember(Name, FromComponent);
  if Result <> nil then
    Exit;

  // If not found, continue search normally
  Result := inherited InternalLookFor(Name, FromComponent);
end;

{*
  [@inheritDoc]
*}
procedure TSepiInheritableContainerType.WriteDigestData(Stream: TStream);
begin
  inherited;

  ParentContainer.WriteDigestToStream(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiInheritableContainerType.LookForMember(const MemberName: string;
  FromComponent: TSepiComponent): TSepiMember;
begin
  Result := inherited LookForMember(MemberName, FromComponent);

  if (Result = nil) and (ParentContainer <> nil) then
    Result := ParentContainer.LookForMember(MemberName, FromComponent);
end;

{*
  Teste si ce conteneur h�rite d'un conteneur donn�
  @param Ancestor   Anc�tre � tester
  @return True si ce conteneur h�rite de Ancestor, False sinon
*}
function TSepiInheritableContainerType.ContainerInheritsFrom(
  Ancestor: TSepiInheritableContainerType): Boolean;
begin
  if Ancestor = Self then
    Result := True
  else if ParentContainer = nil then
    Result := False
  else
    Result := ParentContainer.ContainerInheritsFrom(Ancestor);
end;

{-----------------------}
{ Classe TSepiInterface }
{-----------------------}

{*
  Recense une interface native
*}
constructor TSepiInterface.RegisterTypeInfo(AOwner: TSepiComponent;
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
    Assert(FParent.Completed);
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
constructor TSepiInterface.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;

  OwningUnit.ReadRef(Stream, FParent);
  Assert((FParent = nil) or FParent.Completed);
  FCompleted := False;

  Stream.ReadBuffer(FHasGUID, 1);
  Stream.ReadBuffer(FIsDispInterface, 1);
  Stream.ReadBuffer(FIsDispatch, 1);
  Stream.ReadBuffer(FGUID, SizeOf(TGUID));

  if Parent <> nil then
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
constructor TSepiInterface.Create(AOwner: TSepiComponent; const AName: string;
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
    FParent := (Root.SystemUnit as TSepiSystemUnit).IInterface;
  Assert((FParent = nil) or FParent.Completed);
  FCompleted := False;

  FHasGUID := not IsNoGUID(AGUID);
  FIsDispInterface := AIsDispInterface;
  FIsDispatch := IntfInheritsFrom(
    (Root.SystemUnit as TSepiSystemUnit).IDispatch);
  FGUID := AGUID;

  if Parent <> nil then
    FIMTSize := Parent.IMTSize;
end;

{*
  Construit les RTTI
*}
procedure TSepiInterface.MakeTypeInfo;
var
  Flags: TIntfFlags;
  OwningUnitName: TypeInfoString;
  Count: PWord;
begin
  OwningUnitName := TypeInfoEncode(OwningUnit.Name);

  // Creating the RTTI
  AllocateTypeInfo(IntfTypeDataLengthBase + Length(OwningUnitName) + 1);
  if Parent = nil then
    TypeData.IntfParent := nil
  else
    TypeData.IntfParent := Parent.TypeInfoRef;

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
procedure TSepiInterface.WriteDigestData(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FGUID, SizeOf(TGUID));
end;

{*
  [@inheritDoc]
*}
function TSepiInterface.GetDescription: string;
begin
  Result := 'interface';
end;

{*
  [@inheritDoc]
*}
function TSepiInterface.GetParentContainer: TSepiInheritableContainerType;
begin
  Result := Parent;
end;

{*
  Cr�e une nouvelle instance de TSepiInterface
  @return Instance cr��e
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
  [@inheritDoc]
*}
function TSepiInterface.Equals(Other: TSepiType): Boolean;
begin
  Result := Self = Other;
end;

{*
  D�clare un type interface en forward
  @param AOwner      Propri�taire du type
  @param ATypeName   RTTI de l'interface
*}
class function TSepiInterface.ForwardDecl(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo): TSepiInterface;
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
class function TSepiInterface.ForwardDecl(AOwner: TSepiComponent;
  const AName: string): TSepiInterface;
begin
  Result := TSepiInterface(NewInstance);
  TSepiInterface(AOwner).AddForward(AName, Result);
end;

{*
  Ajoute une m�thode � l'interface
  @param MethodName   Nom de la m�thode
  @param ASignature   Signature
*}
function TSepiInterface.AddMethod(const MethodName: string;
  ASignature: TSepiSignature): TSepiMethod;
begin
  Result := TSepiMethod.Create(Self, MethodName, nil, ASignature,
    mlkInterface);
end;

{*
  Ajoute une m�thode � l'interface
  @param MethodName           Nom de la m�thode
  @param ASignature           Signature Delphi de la m�thode
  @param ACallingConvention   Convention d'appel de la m�thode
*}
function TSepiInterface.AddMethod(const MethodName, ASignature: string;
  ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
begin
  Result := TSepiMethod.Create(Self, MethodName, nil, ASignature,
    mlkInterface, False, 0, ACallingConvention);
end;

{*
  Ajoute une propri�t� � l'interface
  @param AName           Nom de la propri�t�
  @param ASignature      Signature
  @param AReadAccess     Acc�s en lecture � la propri�t� (peut �tre nil)
  @param AWriteAccess    Acc�s en �criture � la propri�t� (peut �tre nil)
  @param AIndex          Index d'acc�s
  @param AIsDefault      Indique si c'est la propri�t� tableau par d�faut
*}
function TSepiInterface.AddProperty(const AName: string;
  ASignature: TSepiSignature; AReadAccess, AWriteAccess: TSepiMember;
  AIndex: Integer; AIsDefault: Boolean): TSepiProperty;
const
  AStorage: TSepiPropertyStorage = (
    Kind: pskConstant; Stored: True; Component: nil
  );
begin
  Result := TSepiProperty.Create(Self, AName, ASignature, AReadAccess,
    AWriteAccess, AIndex, NoDefaultValue, AStorage, AIsDefault);
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
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIndex: Integer = NoIndex; AIsDefault: Boolean = False): TSepiProperty;
begin
  Result := TSepiProperty.Create(Self, AName, ASignature,
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
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIsDefault: Boolean): TSepiProperty;
begin
  Result := TSepiProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIsDefault);
end;

{*
  Termine l'interface et construit ses RTTI si ce n'est pas d�j� fait
*}
procedure TSepiInterface.Complete;
begin
  if FCompleted then
    Exit;

  FCompleted := True;
  if not Native then
    MakeTypeInfo;

  TSepiInterface(Owner).ReAddChild(Self);
end;

{*
  D�termine si l'interface h�rite d'une interface donn�e
  @param AParent   Anc�tre � tester
  @return True si l'interface h�rite de AParent, False sinon
*}
function TSepiInterface.IntfInheritsFrom(AParent: TSepiInterface): Boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FParent) and FParent.IntfInheritsFrom(AParent));
end;

{-------------------}
{ Classe TSepiClass }
{-------------------}

{*
  Recense une classe native
*}
constructor TSepiClass.RegisterTypeInfo(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo);
begin
  inherited;

  FSize := 4;
  FDelphiClass := TypeData.ClassType;
  if Assigned(TypeData.ParentInfo) then
    FParent := TSepiClass(Root.FindType(TypeData.ParentInfo^));

  LoadInitialDataFromParent;

  FStoredInstSize := DelphiClass.InstanceSize - hfFieldSize;
end;

{*
  Charge une classe depuis un flux
*}
constructor TSepiClass.Load(AOwner: TSepiComponent; Stream: TStream);
var
  IntfCount, RedirectorCount, I: Integer;
begin
  inherited;

  FSize := 4;
  if Native then
    FDelphiClass := TypeData.ClassType
  else
    FDelphiClass := nil;
  OwningUnit.ReadRef(Stream, FParent);

  Stream.ReadBuffer(IntfCount, 4);
  SetLength(FInterfaces, IntfCount);
  FillChar(FInterfaces[0], IntfCount*SizeOf(TSepiInterfaceEntry), 0);
  for I := 0 to IntfCount-1 do
    OwningUnit.ReadRef(Stream, FInterfaces[I].IntfRef);

  Stream.ReadBuffer(RedirectorCount, 4);
  SetLength(FIntfMethodRedirectors, RedirectorCount);
  for I := 0 to RedirectorCount-1 do
  begin
    with FIntfMethodRedirectors[I] do
    begin
      OwningUnit.ReadRef(Stream, Intf);
      OwningUnit.ReadRef(Stream, IntfMethod);
      RedirectorName := ReadStrFromStream(Stream);
    end;
  end;

  LoadInitialDataFromParent;

  Stream.ReadBuffer(FStoredInstSize, 4);

  {$IFDEF TRYING_TO_UNDERSTAND_THE_GAP_IN_CLASSES}
  if Native then
  begin
    if (DelphiClass.InstanceSize-FStoredInstSize) <>
      (Parent.DelphiClass.InstanceSize-Parent.FStoredInstSize) then
    begin
      {$I-}
      WriteLn(ErrOutput, Format('InstanceSize;%s;%s;%d;%d;%s;%s;%d;%d',
        [OwningUnit.Name, Name, FStoredInstSize, DelphiClass.InstanceSize,
        Parent.OwningUnit.Name, Parent.Name, Parent.FStoredInstSize,
        Parent.DelphiClass.InstanceSize]));
      {$I+}
    end;
  end;
  {$ENDIF}

  LoadChildren(Stream);

  Complete;
end;

{*
  Cr�e une nouvelle classe
  @param AOwner    Propri�taire du type
  @param AName     Nom du type
  @param AParent   Classe parent
*}
constructor TSepiClass.Create(AOwner: TSepiComponent; const AName: string;
  AParent: TSepiClass);
begin
  inherited Create(AOwner, AName, tkClass);

  FSize := 4;
  FDelphiClass := nil;
  if Assigned(AParent) then
    FParent := AParent
  else
    FParent := (Root.SystemUnit as TSepiSystemUnit).TObject;

  LoadInitialDataFromParent;
end;

{*
  D�truit l'instance
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
        if Assigned(IMT) and OwnsIMT then
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
  Charge les donn�es initiales depuis le parent de cette classe
*}
procedure TSepiClass.LoadInitialDataFromParent;
begin
  if Parent <> nil then
  begin
    Assert(FParent.Completed);

    FInstSize := Parent.InstSize;
    FVMTSize := Parent.VMTSize;
    FDMTNextIndex := Parent.FDMTNextIndex;
    FPublishedPropCount := Parent.FPublishedPropCount;
    FDefaultProperty := Parent.DefaultProperty;
  end else
  begin
    // This is TObject
    FInstSize := 4; // pointer to VMT
    FVMTSize := vmtMinMethodIndex;
    FDMTNextIndex := -1;
  end;
end;

{*
  Construit une IMT non native
  @param IntfEntry   Entr�e de l'IMT � construire
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
    IntfEntry.OwnsIMT := True;
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
      RealMethod := FindIntfMethodImpl(IntfEntry.IntfRef, Method, Self);

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
  Liste les interfaces qui doivent avoir une IMT compl�te
  @param CompleteIMTInterfaces   Liste des interfaces � IMT compl�te
*}
procedure TSepiClass.ListCompleteIMTInterfaces(
  CompleteIMTInterfaces: TObjectList);
var
  Candidates: TObjectList;
  I, J: Integer;
  Intf: TSepiInterface;
begin
  Candidates := TObjectList.Create(False);
  try
    for I := 0 to InterfaceCount-1 do
    begin
      Intf := FInterfaces[I].IntfRef;

      if HasAnyRedirectorFor(Intf) then
        CompleteIMTInterfaces.Add(Intf)
      else
        Candidates.Add(Intf);
    end;

    for I := 0 to InterfaceCount-1 do
    begin
      Intf := FInterfaces[I].IntfRef;

      if Candidates.IndexOf(Intf) < 0 then
        Continue;

      for J := 0 to Candidates.Count-1 do
      begin
        if (Candidates[J] <> Intf) and
          TSepiInterface(Candidates[J]).IntfInheritsFrom(Intf) then
        begin
          Intf := nil;
          Break;
        end;
      end;

      if Intf <> nil then
        CompleteIMTInterfaces.Add(Intf);
    end;
  finally
    Candidates.Free;
  end;
end;

{*
  Construit les IMTs compl�tes
  @param CompleteIMTInterfaces   Liste des interfaces � IMT compl�te
*}
procedure TSepiClass.MakeCompleteIMTs(CompleteIMTInterfaces: TObjectList);
var
  I: Integer;
begin
  for I := 0 to InterfaceCount-1 do
  begin
    if CompleteIMTInterfaces.IndexOf(FInterfaces[I].IntfRef) < 0 then
      Continue;

    FInterfaces[I].Offset := FInstSize;
    Inc(FInstSize, 4);

    MakeIMT(@FInterfaces[I]);
  end;
end;

{*
  Construit les IMTs redirig�es vers d'autres IMTs (compl�tes)
  @param CompleteIMTInterfaces   Liste des interfaces � IMT compl�te
*}
procedure TSepiClass.MakeIMTRedirects(CompleteIMTInterfaces: TObjectList);
var
  I, Existing: Integer;
  Intf, ExistingIntf: TSepiInterface;
begin
  for I := 0 to InterfaceCount-1 do
  begin
    Intf := FInterfaces[I].IntfRef;

    if CompleteIMTInterfaces.IndexOf(Intf) >= 0 then
      Continue;

    for Existing := 0 to InterfaceCount-1 do
    begin
      ExistingIntf := FInterfaces[Existing].IntfRef;

      if (CompleteIMTInterfaces.IndexOf(ExistingIntf) >= 0) and
        ExistingIntf.IntfInheritsFrom(Intf) then
      begin
        FInterfaces[I].IMT := FInterfaces[Existing].IMT;
        FInterfaces[I].Offset := FInterfaces[Existing].Offset;

        Break;
      end;
    end;

    Assert(FInterfaces[I].Offset <> 0);
  end;
end;

{*
  Construit les IMTs
*}
procedure TSepiClass.MakeIMTs;
var
  CompleteIMTInterfaces: TObjectList;
begin
  if InterfaceCount = 0 then
    Exit;

  CompleteIMTInterfaces := TObjectList.Create(False);
  try
    ListCompleteIMTInterfaces(CompleteIMTInterfaces);
    MakeCompleteIMTs(CompleteIMTInterfaces);
    MakeIMTRedirects(CompleteIMTInterfaces);
  finally
    CompleteIMTInterfaces.Free;
  end;
end;

{*
  Lit la table des IMTs natives pour obtenir les offsets r�els
*}
procedure TSepiClass.ReadNativeIMTs;
var
  IntfTable: PInterfaceTable;
  I: Integer;
begin
  IntfTable := DelphiClass.GetInterfaceTable;

  for I := 0 to InterfaceCount-1 do
  begin
    FInterfaces[I].IMT := IntfTable.Entries[I].VTable;
    FInterfaces[I].Offset := IntfTable.Entries[I].IOffset;

    if FInterfaces[I].Offset + 4 > FInstSize then
      FInstSize := FInterfaces[I].Offset + 4;
  end;
end;

{*
  Construit les RTTI
*}
procedure TSepiClass.MakeTypeInfo;
var
  OwningUnitName: TypeInfoString;
  TypeDataLength, I: Integer;
  Props: TObjectList;
  Prop: TSepiProperty;
  PropCount: PWord;
  PropInfo: PPropInfo;
begin
  OwningUnitName := TypeInfoEncode(OwningUnit.Name);
  Props := TObjectList.Create(False);
  try
    TypeDataLength := ClassTypeDataLengthBase;
    Inc(TypeDataLength, Length(OwningUnitName)+1);

    // Listing the published properties, and computing the type data length
    for I := 0 to ChildCount-1 do
    begin
      if Children[I] is TSepiProperty then
      begin
        Prop := TSepiProperty(Children[I]);
        if (Prop.Visibility <> mvPublished) or Prop.IsClassProperty then
          Continue;

        Props.Add(Prop);
        Inc(TypeDataLength, PropInfoLengthBase);
        Inc(TypeDataLength, Length(TypeInfoEncode(Prop.Name))+1);
      end;
    end;

    // Creating the RTTI
    AllocateTypeInfo(TypeDataLength);

    // Basic information
    TypeData.ClassType := DelphiClass;
    if FParent = nil then
      TypeData.ParentInfo := nil
    else
      TypeData.ParentInfo := FParent.TypeInfoRef;
    TypeData.PropCount := FPublishedPropCount;
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
  Range �galement l'adresse de cette table � l'emplacement pr�vu de la VMT.
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
  Range �galement l'adresse de cette table � l'emplacement pr�vu de la VMT.
*}
procedure TSepiClass.MakeInitTable;
var
  Fields: TObjectList;
  I: Integer;
  Component: TSepiComponent;
  InitTable: PInitTable;
begin
  Fields := TObjectList.Create(False);
  try
    // Listing the fields which need initialization
    for I := 0 to ChildCount-1 do
    begin
      Component := Children[I];
      if (Component is TSepiField) and
        TSepiField(Component).FieldType.NeedInit then
        Fields.Add(Component);
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
  Construit la table des champs publi�s
  Range �galement l'adresse de cette table � l'emplacement pr�vu de la VMT.
*}
procedure TSepiClass.MakeFieldTable;
var
  Fields: TObjectList;
  I: Integer;
  Component: TSepiComponent;
  FieldTable: PFieldTable;
  FieldInfo: PFieldInfo;
{$IF CompilerVersion >= 21}
  InfoEx: PWord;
{$IFEND}
begin
  Fields := TObjectList.Create(False);
  try
    // Listing the published fields
    for I := 0 to ChildCount-1 do
    begin
      Component := Children[I];
      if (Component is TSepiField) and (Component.Visibility = mvPublished) then
        Fields.Add(Component);
    end;

    // If no published field, then exit
    if Fields.Count = 0 then
      Exit;

    // Creating the field table
    GetMem(FieldTable, FieldTableLengthBase + Fields.Count*SizeOf(TFieldInfo)
      {$IF CompilerVersion >= 21} + SizeOf(Word) {$IFEND});
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
        FieldInfo := StoreTypeInfoName(@FieldInfo.Name);
      end;
    end;

    {$IF CompilerVersion >= 21}
    // Extended information - we write nothing at the moment
    InfoEx := PWord(FieldInfo);
    InfoEx^ := 0;
    {$IFEND}
  finally
    Fields.Free;
  end;
end;

{*
  Construit la table des m�thodes publi�es
  Range �galement l'adresse de cette table � l'emplacement pr�vu de la VMT.
*}
procedure TSepiClass.MakeMethodTable;
var
  Methods: TObjectList;
  I: Integer;
  Component: TSepiComponent;
  MethodTable: PMethodTable;
  MethodInfo, NextMethodInfo: PMethodInfo;
{$IF CompilerVersion >= 21}
  InfoEx: PWord;
{$IFEND}
begin
  Methods := TObjectList.Create(False);
  try
    // Listing the published methods
    for I := 0 to ChildCount-1 do
    begin
      Component := Children[I];
      if (Component is TSepiMethod) and
        (Component.Visibility = mvPublished) then
        Methods.Add(Component);
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
        NextMethodInfo := StoreTypeInfoName(@MethodInfo.Name);

        MethodInfo.Size := Integer(NextMethodInfo) - Integer(MethodInfo);
        MethodInfo.Code := Code;

        MethodInfo := NextMethodInfo;
      end;
    end;

    {$IF CompilerVersion >= 21}
    // Extended information - we write nothing at the moment
    InfoEx := PWord(MethodInfo);
    InfoEx^ := 0;
    {$IFEND}
  finally
    Methods.Free;
  end;
end;

{*
  Construit la DMT
  Range �galement l'adresse de la DMT � l'emplacement pr�vu de la VMT.
*}
procedure TSepiClass.MakeDMT;
var
  PDMT: Pointer;
  I, Count: Integer;
  Component: TSepiComponent;
  IndexList, CodeList: Integer;
  Methods: TObjectList;
begin
  Methods := TObjectList.Create(False);
  try
    // Listing dynamic methods
    for I := 0 to ChildCount-1 do
    begin
      Component := Children[I];
      if (Component is TSepiMethod) and
        (TSepiMethod(Component).LinkKind in [mlkDynamic, mlkMessage]) and
        (not TSepiMethod(Component).IsAbstract) then
        Methods.Add(Component);
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
  VMTEntries[vmtInstanceSize] := Pointer(TotalInstSize);
  VMTEntries[vmtParent] := @Parent.FDelphiClass;

  // Copy the parent VMT
  if Parent <> nil then
  begin
    Move(Pointer(Integer(Parent.DelphiClass) + vmtMinMethodIndex)^,
      Pointer(Integer(PVMT) + vmtMinMethodIndex)^,
      Parent.VMTSize - vmtMinMethodIndex);
  end;

  // Setting the new method addresses
  AbstractErrorProcAddress := DereferenceJump(@AbstractError);
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
  Nombre d'interfaces support�es
  @return Nombre d'interfaces support�es
*}
function TSepiClass.GetInterfaceCount: Integer;
begin
  Result := Length(FInterfaces);
end;

{*
  Tableau zero-based des interfaces support�es
  @param Index   Index dans le tableau
  @return Interface support�e � l'index sp�cifi�
*}
function TSepiClass.GetInterfaces(Index: Integer): TSepiInterface;
begin
  Result := FInterfaces[Index].IntfRef;
end;

{*
  Taille totale d'instance (incluant les champs cach�s)
  @return Taille totale d'instance (incluant les champs cach�s)
*}
function TSepiClass.GetTotalInstSize: Integer;
begin
  Result := InstSize + hfFieldSize;
end;

{*
  VMT de la classe, index�e par les constantes vmtXXX
  @param Index   Index dans la VMT
  @return Information contenue dans la VMT � l'index sp�cifi�
*}
function TSepiClass.GetVMTEntries(Index: Integer): Pointer;
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
procedure TSepiClass.SetVMTEntries(Index: Integer; Value: Pointer);
begin
  PPointer(Integer(FDelphiClass) + Index)^ := Value;
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.ChildAdded(Child: TSepiComponent);
var
  Prop: TSepiProperty;
  PreviousProp: TSepiMember;
begin
  inherited;

  if Child is TSepiField then
  begin
    with TSepiField(Child) do
      if Offset + FieldType.Size > FInstSize then
        FInstSize := Offset + FieldType.Size;
  end else if Child is TSepiProperty then
  begin
    Prop := TSepiProperty(Child);

    if Prop.Visibility = mvPublished then
    begin
      if Parent = nil then
        PreviousProp := nil
      else
        PreviousProp := Parent.LookForMember(Prop.Name);

      if (PreviousProp is TSepiProperty) and
        (PreviousProp.Visibility = mvPublished) then
      begin
        Prop.FNameIndex := TSepiProperty(PreviousProp).FNameIndex;
      end else
      begin
        Prop.FNameIndex := FPublishedPropCount;
        Inc(FPublishedPropCount);
      end;
    end;

    if Prop.IsDefault then
      FDefaultProperty := Prop;
  end;
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

  for I := 0 to Length(FIntfMethodRedirectors)-1 do
  begin
    with FIntfMethodRedirectors[I] do
    begin
      OwningUnit.AddRef(Intf);
      OwningUnit.AddRef(IntfMethod);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.Save(Stream: TStream);
var
  IntfCount, RedirectorCount, I: Integer;
begin
  inherited;
  OwningUnit.WriteRef(Stream, FParent);

  IntfCount := InterfaceCount;
  Stream.WriteBuffer(IntfCount, 4);
  for I := 0 to IntfCount-1 do
    OwningUnit.WriteRef(Stream, Interfaces[I]);

  RedirectorCount := Length(FIntfMethodRedirectors);
  Stream.WriteBuffer(RedirectorCount, 4);
  for I := 0 to RedirectorCount-1 do
  begin
    with FIntfMethodRedirectors[I] do
    begin
      OwningUnit.WriteRef(Stream, Intf);
      OwningUnit.WriteRef(Stream, IntfMethod);
      WriteStrToStream(Stream, RedirectorName);
    end;
  end;

  Stream.WriteBuffer(FInstSize, 4);

  SaveChildren(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiClass.GetDescription: string;
begin
  Result := 'class';
end;

{*
  [@inheritDoc]
*}
function TSepiClass.GetParentContainer: TSepiInheritableContainerType;
begin
  Result := Parent;
end;

{*
  Teste si une m�thode d'interface est redirig�e dans cette class
  @param Intf         Interface � tester
  @param IntfMethod   M�thode d'interface � tester
  @return True si cette m�thode est redirig�e, False sinon
*}
function TSepiClass.IsIntfMethodRedirected(Intf: TSepiInterface;
  IntfMethod: TSepiMethod): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 0 to Length(FIntfMethodRedirectors)-1 do
    if (FIntfMethodRedirectors[I].Intf = Intf) and
      (FIntfMethodRedirectors[I].IntfMethod = IntfMethod) then
      Exit;

  if Parent <> nil then
    Result := Parent.IsIntfMethodRedirected(Intf, IntfMethod)
  else
    Result := False;
end;

{*
  Trouve la m�thode qui impl�mente une m�thode d'interface
  @param Intf         Interface dont on cherche l'impl�mentation
  @param IntfMethod   M�thode d'interface dont on cherche l'impl�mentation
  @param FromClass    Classe depuis laquelle on cherche
  @return M�thode qui impl�mente, ou nil si non trouv�e
*}
function TSepiClass.FindIntfMethodImpl(Intf: TSepiInterface;
  IntfMethod: TSepiMethod; FromClass: TSepiClass): TSepiMethod;
var
  I: Integer;
begin
  for I := 0 to Length(FIntfMethodRedirectors)-1 do
  begin
    if (FIntfMethodRedirectors[I].Intf = Intf) and
      (FIntfMethodRedirectors[I].IntfMethod = IntfMethod) then
    begin
      Result := FromClass.LookForMember(
        FIntfMethodRedirectors[I].RedirectorName) as TSepiMethod;
      Exit;
    end;
  end;

  if Parent <> nil then
    Result := Parent.FindIntfMethodImpl(Intf, IntfMethod, FromClass)
  else
    Result := FromClass.LookForMember(IntfMethod.Name) as TSepiMethod;
end;

{*
  Teste si cette classe a un quelconque redirecteur pour une interface donn�e
  @param Intf   Interface � tester
*}
function TSepiClass.HasAnyRedirectorFor(Intf: TSepiInterface): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 0 to Length(FIntfMethodRedirectors)-1 do
    if FIntfMethodRedirectors[I].Intf = Intf then
      Exit;

  if Parent <> nil then
    Result := Parent.HasAnyRedirectorFor(Intf)
  else
    Result := False;
end;

{*
  Cr�e une nouvelle instance de TSepiClass
  @return Instance cr��e
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
  D�clare un type classe en forward
  @param AOwner      Propri�taire du type
  @param ATypeInfo   RTTI de la classe
*}
class function TSepiClass.ForwardDecl(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo): TSepiClass;
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
class function TSepiClass.ForwardDecl(AOwner: TSepiComponent;
  const AName: string): TSepiClass;
begin
  Result := TSepiClass(NewInstance);
  TSepiClass(AOwner).AddForward(AName, Result);
end;

{*
  Ajoute le support d'une interface
  @param AInterface   Interface � supporter
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
    OwnsIMT := False;
  end;
end;

{*
  Ajoute le support d'une interface
  @param AIntfTypeInfo   RTTI de l'interface � supporter
*}
procedure TSepiClass.AddInterface(AIntfTypeInfo: PTypeInfo);
begin
  AddInterface(Root.FindType(AIntfTypeInfo) as TSepiInterface);
end;

{*
  Ajoute le support d'une interface
  @param AIntfName   Nom de l'interface � supporter
*}
procedure TSepiClass.AddInterface(const AIntfName: string);
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
  Ajoute un champ � la classe
  @param FieldName       Nom du champ
  @param FieldTypeInto   RTTI du type du champ
  @param ForcePack       Force un pack sur ce champ si True
  @return Champ nouvellement ajout�
*}
function TSepiClass.AddField(const FieldName: string;
  FieldTypeInfo: PTypeInfo; ForcePack: Boolean = False): TSepiField;
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
function TSepiClass.AddField(const FieldName, FieldTypeName: string;
  ForcePack: Boolean = False): TSepiField;
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
function TSepiClass.AddMethod(const MethodName: string; ACode: Pointer;
  ASignature: TSepiSignature; ALinkKind: TMethodLinkKind; AAbstract: Boolean;
  AMsgID: Integer): TSepiMethod;
begin
  Result := TSepiMethod.Create(Self, MethodName, ACode, ASignature,
    ALinkKind, AAbstract, AMsgID);
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
function TSepiClass.AddMethod(const MethodName: string; ACode: Pointer;
  const ASignature: string; ALinkKind: TMethodLinkKind = mlkStatic;
  AAbstract: Boolean = False; AMsgID: Integer = 0;
  ACallingConvention: TCallingConvention = ccRegister): TSepiMethod;
begin
  Result := TSepiMethod.Create(Self, MethodName, ACode, ASignature,
    ALinkKind, AAbstract, AMsgID, ACallingConvention);
end;

{*
  Ajoute une m�thode � la classe
  @param MethodName           Nom de la m�thode
  @param ACode                Pointeur sur le code de la m�thode
  @param ASignature           Signature Delphi de la m�thode
  @param ACallingConvention   Convention d'appel de la m�thode
*}
function TSepiClass.AddMethod(const MethodName: string; ACode: Pointer;
  const ASignature: string;
  ACallingConvention: TCallingConvention): TSepiMethod;
begin
  Result := TSepiMethod.Create(Self, MethodName, ACode, ASignature,
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
function TSepiClass.AddOverloadedMethod(const MethodName: string;
  ACode: Pointer; ASignature: TSepiSignature; ALinkKind: TMethodLinkKind;
  AAbstract: Boolean; AMsgID: Integer): TSepiMethod;
begin
  Result := TSepiMethod.CreateOverloaded(Self, MethodName, ACode,
    ASignature, ALinkKind, AAbstract, AMsgID);
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
  Ajoute une m�thode surcharg�e � la classe
  @param MethodName           Nom de la m�thode
  @param ACode                Pointeur sur le code de la m�thode
  @param ASignature           Signature Delphi de la m�thode
  @param ACallingConvention   Convention d'appel de la m�thode
*}
function TSepiClass.AddOverloadedMethod(const MethodName: string;
  ACode: Pointer; const ASignature: string;
  ACallingConvention: TCallingConvention): TSepiMethod;
begin
  Result := TSepiMethod.CreateOverloaded(Self, MethodName, ACode,
    ASignature, mlkStatic, False, 0, ACallingConvention);
end;

{*
  Ajoute une propri�t� � la classe
  @param AName           Nom de la propri�t�
  @param ASignature      Signature
  @param AReadAccess     Acc�s en lecture � la propri�t� (peut �tre nil)
  @param AWriteAccess    Acc�s en �criture � la propri�t� (peut �tre nil)
  @param AIndex          Index d'acc�s
  @param ADefaultValue   Valeur par d�faut de la propri�t�
  @param AIsDefault      Indique si c'est la propri�t� tableau par d�faut
*}
function TSepiClass.AddProperty(const AName: string; ASignature: TSepiSignature;
  AReadAccess, AWriteAccess: TSepiMember; AIndex, ADefaultValue: Integer;
  const AStorage: TSepiPropertyStorage; AIsDefault: Boolean): TSepiProperty;
begin
  Result := TSepiProperty.Create(Self, AName, ASignature, AReadAccess,
    AWriteAccess, AIndex, ADefaultValue, AStorage, AIsDefault);
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
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIndex: Integer = NoIndex; ADefaultValue: Integer = NoDefaultValue;
  const AStorage: string = '';
  AIsDefault: Boolean = False): TSepiProperty;
begin
  Result := TSepiProperty.Create(Self, AName, ASignature,
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
  const AName, ASignature, AReadAccess, AWriteAccess: string;
  AIsDefault: Boolean): TSepiProperty;
begin
  Result := TSepiProperty.Create(Self, AName, ASignature,
    AReadAccess, AWriteAccess, AIsDefault);
end;

{*
  Red�finit une propri�t� h�rit�e
  @param AName           Nom de la propri�t�
  @param AReadAccess     Acc�s en lecture � la propri�t�
  @param AWriteAccess    Acc�s en �criture � la propri�t�
  @param ADefaultValue   Valeur par d�faut
  @param AStorage        Sp�cificateur de stockage
*}
function TSepiClass.RedefineProperty(const AName: string; AReadAccess,
  AWriteAccess: TSepiMember; ADefaultValue: Integer;
  const AStorage: TSepiPropertyStorage): TSepiProperty;
begin
  Result := TSepiProperty.Redefine(Self, AName, AReadAccess, AWriteAccess,
    ADefaultValue, AStorage);
end;

{*
  Red�finit une propri�t� h�rit�e
  @param AName          Nom de la propri�t�
  @param AReadAccess    Acc�s en lecture � la propri�t�
  @param AWriteAccess   Acc�s en �criture � la propri�t�
  @param AStorage       Sp�cificateur de stockage
*}
function TSepiClass.RedefineProperty(const AName: string;
  const AReadAccess: string = ''; const AWriteAccess: string = '';
  const AStorage: string = ''): TSepiProperty;
begin
  Result := TSepiProperty.Redefine(Self, AName,
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
  const AName, AReadAccess, AWriteAccess: string; ADefaultValue: Integer;
  const AStorage: string = ''): TSepiProperty;
begin
  Result := TSepiProperty.Redefine(Self, AName,
    AReadAccess, AWriteAccess, ADefaultValue, AStorage);
end;

{*
  Ajoute un redirecteur de m�thode d'interface
  @param Intf             Interface dont rediriger une m�thode
  @param IntfMethod       M�thode d'interface � rediriger
  @param RedirectorName   Nom du redirecteur
*}
procedure TSepiClass.AddIntfMethodRedirector(Intf: TSepiInterface;
  IntfMethod: TSepiMethod; const RedirectorName: string);
var
  Index: Integer;
begin
  Index := Length(FIntfMethodRedirectors);
  SetLength(FIntfMethodRedirectors, Index+1);

  FIntfMethodRedirectors[Index].Intf := Intf;
  FIntfMethodRedirectors[Index].IntfMethod := IntfMethod;
  FIntfMethodRedirectors[Index].RedirectorName := RedirectorName;
end;

{*
  Termine la classe et construit ses RTTI si ce n'est pas d�j� fait
*}
procedure TSepiClass.Complete;
begin
  if FCompleted then
    Exit;

  FCompleted := True;
  AlignOffset(FInstSize);

  if Native then
    ReadNativeIMTs
  else
  begin
    MakeIMTs;
    MakeVMT;
  end;

  TSepiClass(Owner).ReAddChild(Self);
end;

{*
  [@inheritDoc]
*}
function TSepiClass.Equals(Other: TSepiType): Boolean;
begin
  Result := Self = Other;
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
  D�termine si la classe h�rite d'une classe donn�e
  @param AParent   Anc�tre � tester
  @return True si la classe h�rite de AParent, False sinon
*}
function TSepiClass.ClassInheritsFrom(AParent: TSepiClass): Boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FParent) and FParent.ClassInheritsFrom(AParent));
end;

{*
  D�termine si la classe impl�mente une interface donn�e
  @param AInterface   Interface � tester
  @return True si la classe impl�mente AInterface, False sinon
*}
function TSepiClass.ClassImplementsInterface(
  AInterface: TSepiInterface): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to InterfaceCount-1 do
    if Interfaces[I] = AInterface then
      Exit;

  if Parent = nil then
    Result := False
  else
    Result := Parent.ClassImplementsInterface(AInterface);
end;

{-----------------------}
{ Classe TSepiMetaClass }
{-----------------------}

{*
  Charge une meta-classe depuis un flux
*}
constructor TSepiMetaClass.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;
  FSize := 4;
  OwningUnit.ReadRef(Stream, FClass);

  if not Native then
    ForceNative;
end;

{*
  Cr�e une nouvelle meta-classe
  @param AOwner   Propri�taire du type
  @param AName    Nom du type
  @param AClass   Classe correspondante
*}
constructor TSepiMetaClass.Create(AOwner: TSepiComponent; const AName: string;
  AClass: TSepiClass; AIsNative: Boolean = False);
begin
  inherited Create(AOwner, AName, tkClassRefOrUnknown);
  FSize := 4;
  FClass := AClass;

  MakeTypeInfo;

  if AIsNative then
    ForceNative;
end;

{*
  Cr�e une nouvelle meta-classe
  @param AOwner       Propri�taire du type
  @param AName        Nom du type
  @param AClassInfo   RTTI de la classe correspondante
*}
constructor TSepiMetaClass.Create(AOwner: TSepiComponent; const AName: string;
  AClassInfo: PTypeInfo; AIsNative: Boolean = False);
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
constructor TSepiMetaClass.Create(AOwner: TSepiComponent;
  const AName, AClassName: string; AIsNative: Boolean = False);
begin
  Create(AOwner, AName, AOwner.Root.FindType(AClassName) as TSepiClass,
    AIsNative);
end;

{*
  [@inheritDoc]
*}
constructor TSepiMetaClass.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  Create(AOwner, AName, (Source as TSepiMetaClass).SepiClass);
end;

{*
  Cr�e les RTTI de ce type
*}
procedure TSepiMetaClass.MakeTypeInfo;
begin
  {$IF CompilerVersion >= 21}
  AllocateTypeInfo(MetaClassTypeDataLength);

  TypeData.InstanceType := TSepiMetaClass(SepiClass).TypeInfoRef;
  {$IFEND}
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
procedure TSepiMetaClass.WriteDigestData(Stream: TStream);
begin
  inherited;

  SepiClass.WriteDigestToStream(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiMetaClass.GetDescription: string;
begin
  Result := 'class of '+SepiClass.DisplayName;
end;

{*
  [@inheritDoc]
*}
function TSepiMetaClass.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    SepiClass.Equals(TSepiMetaClass(Other).SepiClass);
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
  Recense un type r�f�rence de m�thode natif
*}
constructor TSepiMethodRefType.RegisterTypeInfo(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  FSignature := TSepiSignature.RegisterTypeData(Self, TypeData);

  MakeSize;
end;

{*
  Charge un type r�f�rence de m�thode depuis un flux
*}
constructor TSepiMethodRefType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;
  FSignature := TSepiSignature.Load(Self, Stream);

  MakeSize;
end;

{*
  Cr�e un nouveau type r�f�rence de m�thode
  @param AOwner               Propri�taire du type
  @param AName                Nom du type
  @param ASignature           Signature
  @param AOfObject            Indique s'il s'agit d'une m�thode
  @param ACallingConvention   Convention d'appel
*}
constructor TSepiMethodRefType.Create(AOwner: TSepiComponent;
  const AName, ASignature: string; AOfObject: Boolean = False;
  ACallingConvention: TCallingConvention = ccRegister);
begin
  inherited Create(AOwner, AName, tkMethod);

  FSignature := TSepiSignature.Create(Self,
    FullSignature(ASignature, AOfObject), ACallingConvention);

  MakeSize;
end;

{*
  Cr�e un nouveau type r�f�rence de m�thode
  @param AOwner       Propri�taire du type
  @param AName        Nom du type
  @param ASignature   Signature
*}
constructor TSepiMethodRefType.Create(AOwner: TSepiComponent;
  const AName: string; ASignature: TSepiSignature);
begin
  inherited Create(AOwner, AName, tkMethod);

  FSignature := TSepiSignature.Clone(Self, ASignature);

  MakeSize;
end;

{*
  [@inheritDoc]
*}
constructor TSepiMethodRefType.Clone(AOwner: TSepiComponent;
  const AName: string; Source: TSepiType);
begin
  inherited Create(AOwner, AName, tkMethod);

  FSignature := TSepiSignature.Clone(Self,
    (Source as TSepiMethodRefType).Signature);

  MakeSize;
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
  Calcule la taille de ce type
*}
procedure TSepiMethodRefType.MakeSize;
begin
  if Signature.Kind in skWithSelfParam then
  begin
    FSize := 8;
    FParamBehavior.AlwaysByStack := True;
    FResultBehavior := rbParameter;
  end else
    FSize := 4;
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
procedure TSepiMethodRefType.WriteDigestData(Stream: TStream);
begin
  inherited;

  Signature.WriteDigestData(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiMethodRefType.GetDescription: string;
begin
  Result := 'method ref';
end;

{*
  [@inheritDoc]
*}
function TSepiMethodRefType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    Signature.Equals(TSepiMethodRefType(Other).Signature, scoCompatibility);
end;

{*
  [@inheritDoc]
*}
function TSepiMethodRefType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := (AType.Kind = tkMethod) and
    FSignature.Equals(TSepiMethodRefType(AType).FSignature, scoCompatibility);
end;

{-------------------------}
{ Classe TSepiVariantType }
{-------------------------}

{*
  Recense un type variant natif
*}
constructor TSepiVariantType.RegisterTypeInfo(AOwner: TSepiComponent;
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
constructor TSepiVariantType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  if not Native then
    AllocateTypeInfo;

  FSize := 16;
  FNeedInit := True;
  FParamBehavior.AlwaysByAddress := True;
  FResultBehavior := rbParameter;
end;

{*
  [@inheritDoc]
*}
constructor TSepiVariantType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  inherited Create(AOwner, AName, tkVariant);

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
  SepiRegisterComponentClasses([
    TSepiField, TSepiMethod, TSepiOverloadedMethod, TSepiProperty,
    TSepiRecordType, TSepiInterface, TSepiClass, TSepiMetaClass,
    TSepiMethodRefType, TSepiVariantType
  ]);
end.

