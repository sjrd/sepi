{*
  Définit les classes de gestion des types
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiTypes;

interface

uses
  Classes, SysUtils, ScUtils, SepiMetaUnits, SysConst, TypInfo, ScLists;

const
  MinInt64 = Int64($FFFFFFFFFFFFFFFF);
  MaxInt64 = Int64($7FFFFFFFFFFFFFFF);

type
  {*
    Type de booléen
  *}
  TBooleanKind = (bkBoolean, bkByteBool, bkWordBool, bkLongBool);

  {*
    Type entier
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiIntegerType = class(TSepiType)
  private
    FMinValue : Longint;       /// Valeur minimale
    FMaxValue : Longint;       /// Valeur maximale
    FSigned : boolean;         /// Indique si l'entier est signé ou non
    FNeedRangeCheck : boolean; /// Indique s'il faut vérifier les étendues

    procedure ExtractTypeData;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AMinValue : Longint = -MaxInt-1; AMaxValue : Longint = MaxInt;
      ASigned : boolean = True);

    function InRange(Value : Longint) : boolean;
    procedure CheckInRange(Value : Longint);

    property MinValue : Longint read FMinValue;
    property MaxValue : Longint read FMaxValue;
    property Signed : boolean read FSigned;
    property NeedRangeCheck : boolean read FNeedRangeCheck;
  end;

  {*
    Type caractère
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiCharType = class(TSepiType)
  private
    FIsUnicode : boolean;      /// Indique si le caractère est Unicode
    FMinValue : WideChar;      /// Valeur minimale
    FMaxValue : WideChar;      /// Valeur maximale
    FNeedRangeCheck : boolean; /// Indique s'il faut vérifier les étendues

    procedure ExtractTypeData;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AMinValue : WideChar = #0; AMaxValue : WideChar = #255;
      AForceUnicode : boolean = False);

    function InRange(Value : WideChar) : boolean;
    procedure CheckInRange(Value : WideChar);

    property IsUnicode : boolean read FIsUnicode;
    property MinValue : WideChar read FMinValue;
    property MaxValue : WideChar read FMaxValue;
    property NeedRangeCheck : boolean read FNeedRangeCheck;
  end;

  {*
    Type entier long (64 bits)
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiInt64Type = class(TSepiType)
  private
    FMinValue : Int64;         /// Valeur minimale
    FMaxValue : Int64;         /// Valeur maximale
    FNeedRangeCheck : boolean; /// Indique s'il faut vérifier les étendues

    procedure ExtractTypeData;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AMinValue : Int64 = MinInt64; AMaxValue : Int64 = MaxInt64);

    function InRange(Value : Int64) : boolean;
    procedure CheckInRange(Value : Int64);

    property MinValue : Int64 read FMinValue;
    property MaxValue : Int64 read FMaxValue;
    property NeedRangeCheck : boolean read FNeedRangeCheck;
  end;

  {*
    Type nombre à virgule flottante
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiFloatType = class(TSepiType)
  private
    FFloatType : TFloatType; /// Type de flottant

    procedure ExtractTypeData;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AFloatType : TFloatType = ftDouble);

    property FloatType : TFloatType read FFloatType;
  end;

  {*
    Type chaîne de caractères courte
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiShortStringType = class(TSepiType)
  private
    FMaxLength : integer; /// Longueur maximale

    procedure ExtractTypeData;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AMaxLength : integer = 255);

    property MaxLength : integer read FMaxLength;
  end;

  {*
    Type chaîne de caractères longue
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiStringType = class(TSepiType)
  private
    FIsUnicode : boolean; /// Indique si la chaîne est Unicode ou non

    procedure ExtractTypeData;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AIsUnicode : boolean = False);

    property IsUnicode : boolean read FIsUnicode;
  end;

  {*
    Type booléen
    Note : il est impossible de créer un nouveau type booléen.
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiBooleanType = class(TSepiType)
  private
    FBooleanKind : TBooleanKind;

    procedure ExtractTypeData;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;

    property BooleanKind : TBooleanKind read FBooleanKind;
  end;

  {*
    Type énumération
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiEnumType = class(TSepiType)
  private
    FValues : array of string; /// Noms des valeurs

    procedure ExtractTypeData;

    function GetValueCount : integer;
    function GetValues(Index : integer) : string;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const AValues : array of string);

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property ValueCount : integer read GetValueCount;
    property Values[index : integer] : string read GetValues;
  end;

  {*
    Type ensemble
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiSetType = class(TSepiType)
  private
    FItemType : TSepiType; /// Type des éléments
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AItemType : TSepiType);

    function CompatibleWith(AType : TSepiType) : boolean; override;
  end;

  {*
    Type enregistrement
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiRecordType = class(TSepiType)
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string);

    function CompatibleWith(AType : TSepiType) : boolean; override;
  end;

  TSepiObjectType = class end;

  {*
    Type classe
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiClassType = class(TSepiType)
  private
    FAncestor : TSepiClassType;    /// Ancêtre
    FDelphiClass : TClass;         /// Classe Delphi d'importation
    FObjectType : TSepiObjectType; /// Type objet correspondant
  protected
    procedure ChildAdded(Child : TSepiMeta); override;

    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AAncestor : TSepiClassType; ADelphiClass : TClass;
      AObjectType : TSepiObjectType);

    function CompatibleWith(AType : TSepiType) : boolean; override;
    function InheritsFrom(AParent : TSepiClassType) : boolean;

    property Ancestor : TSepiClassType read FAncestor;
    property DelphiClass : TClass read FDelphiClass;
    property ObjectType : TSepiObjectType read FObjectType;
  end;

  (*{*
    Type objet
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiObjectType = class(TSepiMetaMemberContainer)
  private
    FAncestor : TSepiObjectType;                /// Ancêtre
    FDelphiClass : TClass;                      /// Classe Delphi d'importation
    FClassType : TSepiClassType;                /// Type classe correspondant
    FCurrentVisibility : TSepiMemberVisibility; /// Visibilité courante
  protected
    procedure ChildAdded(Child : TSepiMeta); override;

    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AAncestor : TSepiObjectType; ADelphiClass : TClass);

    function CompatibleWith(AType : TSepiType) : boolean; override;
    function InheritsFrom(AParent : TSepiObjectType) : boolean;

    property Ancestor : TSepiObjectType read FAncestor;
    property ObjClassType : TSepiClassType read FClassType;
    property CurrentVisibility : TSepiMemberVisibility
      read FCurrentVisibility write FCurrentVisibility;
  end;*)

  {*
    Type tableau statique (à N dimensions)
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiArrayType = class(TSepiType)
  private
    FDimensions : array of integer; /// Taille dans chaque dimension
    FItemType : TSepiType;          /// Type des éléments

    function GetDimCount : integer;
    function GetDimensions(Index : integer) : integer;
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const ADimensions : array of integer; AItemType : TSepiType);
    destructor Destroy; override;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property DimCount : integer read GetDimCount;
    property Dimensions[index : integer] : integer read GetDimensions;
    property ItemType : TSepiType read FItemType;
  end;

  {*
    Type tableau dynamique (à une dimension)
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiDynArrayType = class(TSepiType)
  private
    FItemType : TSepiType; /// Type des éléments
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AItemType : TSepiType);

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property ItemType : TSepiType read FItemType;
  end;

  {*
    Type référence de méthode
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMethodRefType = class(TSepiType)
  private
    FSignature : TSepiMethodSignature; /// Signature
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string);
    destructor Destroy; override;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property Signature : TSepiMethodSignature read FSignature;
  end;

implementation

const
  // Tailles de structure TTypeData en fonction des types
  IntegerTypeDataLength = sizeof(TOrdType) + 2*sizeof(Longint);
  CharTypeDataLength = IntegerTypeDataLength;
  Int64TypeDataLength = 2*sizeof(Int64);
  FloatTypeDataLength = sizeof(TFloatType);
  ShortStringTypeDataLength = sizeof(Byte);
  StringTypeDataLength = 0;
  EnumTypeDataLength = sizeof(TOrdType) + 2*sizeof(Longint) + sizeof(Pointer);

{-------------------------}
{ Classe TSepiIntegerType }
{-------------------------}

{*
  Recense un type entier natif
*}
constructor TSepiIntegerType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type entier depuis un flux
*}
constructor TSepiIntegerType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(IntegerTypeDataLength);
  Stream.ReadBuffer(TypeData^, IntegerTypeDataLength);

  ExtractTypeData;
end;

{*
  Crée un nouveau type entier
  @param AOwner      Propriétaire du type
  @param AName       Nom du type
  @param AMinValue   Valeur minimale
  @param AMaxValue   Valeur maximale
  @param ASigned     Indique si l'entier est signé ou non
*}
constructor TSepiIntegerType.Create(AOwner : TSepiMeta; const AName : string;
  AMinValue : Longint = -MaxInt-1; AMaxValue : Longint = MaxInt;
  ASigned : boolean = True);
begin
  inherited Create(AOwner, AName, tkInteger);

  AllocateTypeInfo(IntegerTypeDataLength);

  if AMinValue < 0 then
  begin
    // Signed
    if (AMinValue < -32768) or (AMaxValue >= 32768) then
      TypeData.OrdType := otSLong else
    if (AMinValue < -128) or (AMaxValue >= 128) then
      TypeData.OrdType := otSWord else
    TypeData.OrdType := otSByte;
  end else
  begin
    // Unsigned
    if AMaxValue >= 65536 then
      TypeData.OrdType := otULong else
    if AMaxValue >= 256 then
      TypeData.OrdType := otUWord else
    TypeData.OrdType := otUByte;
  end;

  TypeData.MinValue := AMinValue;
  TypeData.MaxValue := AMaxValue;

  ExtractTypeData;
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiIntegerType.ExtractTypeData;
begin
  FMinValue := TypeData.MinValue;
  FMaxValue := TypeData.MaxValue;
  FSigned := TypeData.OrdType in [otSByte, otSWord, otSLong];

  FNeedRangeCheck := (FMinValue <> -MaxInt - 1) or (FMaxValue <> MaxInt);
end;

{*
  Teste si une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @return True si Value est dans l'intervalle supporté, False sinon
*}
function TSepiIntegerType.InRange(Value : Longint) : boolean;
begin
  Result := (Value >= FMinValue) and (Value <= FMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @throws ERangeError Value n'était pas dans l'intervalle supporté
*}
procedure TSepiIntegerType.CheckInRange(Value : Longint);
begin
  if not InRange(Value) then
    raise ERangeError.CreateRes(@SRangeError);
end;

{----------------------}
{ Classe TSepiCharType }
{----------------------}

{*
  Recense un type caractère natif
*}
constructor TSepiCharType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type caractère depuis un flux
*}
constructor TSepiCharType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(CharTypeDataLength);
  Stream.ReadBuffer(TypeData^, CharTypeDataLength);

  ExtractTypeData;
end;

{*
  Crée un nouveau type caractère
  @param AOwner          Propriétaire du type
  @param AName           Nom du type
  @param AMinValue       Valeur minimale
  @param AMaxValue       Valeur maximale
  @param AForceUnicode   Positionné à True, force l'utilisation d'Unicode
*}
constructor TSepiCharType.Create(AOwner : TSepiMeta; const AName : string;
  AMinValue : WideChar = #0; AMaxValue : WideChar = #255;
  AForceUnicode : boolean = False);
var AKind : TTypeKind;
begin
  if AForceUnicode or (AMaxValue > #255) then
    AKind := tkWChar
  else
    AKind := tkChar;
  inherited Create(AOwner, AName, AKind);

  AllocateTypeInfo(IntegerTypeDataLength);

  if Kind = tkWChar then
    TypeData.OrdType := otUWord
  else
    TypeData.OrdType := otUByte;

  TypeData.MinValue := Longint(AMinValue);
  TypeData.MaxValue := Longint(AMaxValue);

  ExtractTypeData;
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiCharType.ExtractTypeData;
begin
  FIsUnicode := Kind = tkWChar;
  FMinValue := WideChar(TypeData.MinValue);
  FMaxValue := WideChar(TypeData.MaxValue);

  if FIsUnicode then
    FNeedRangeCheck := (FMinValue <> #0) or (FMaxValue <> #65535)
  else
    FNeedRangeCheck := (FMinValue <> #0) or (FMaxValue <> #255);
end;

{*
  Teste si une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @return True si Value est dans l'intervalle supporté, False sinon
*}
function TSepiCharType.InRange(Value : WideChar) : boolean;
begin
  Result := (Value >= FMinValue) and (Value <= FMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @throws ERangeError Value n'était pas dans l'intervalle supporté
*}
procedure TSepiCharType.CheckInRange(Value : WideChar);
begin
  if not InRange(Value) then
    raise ERangeError.CreateRes(@SRangeError);
end;

{-----------------------}
{ Classe TSepiInt64Type }
{-----------------------}

{*
  Recense un type entier 64 bits natif
*}
constructor TSepiInt64Type.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type entier 64 bits depuis un flux
*}
constructor TSepiInt64Type.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(Int64TypeDataLength);
  Stream.ReadBuffer(TypeData^, Int64TypeDataLength);

  ExtractTypeData;
end;

{*
  Crée un nouveau type entier 64 bits
  @param AOwner      Propriétaire du type
  @param AName       Nom du type
  @param AMinValue   Valeur minimale
  @param AMaxValue   Valeur maximale
*}
constructor TSepiInt64Type.Create(AOwner : TSepiMeta; const AName : string;
  AMinValue : Int64 = MinInt64; AMaxValue : Int64 = MaxInt64);
begin
  inherited Create(AOwner, AName, tkInt64);

  AllocateTypeInfo(Int64TypeDataLength);

  TypeData.MinValue := AMinValue;
  TypeData.MaxValue := AMaxValue;

  ExtractTypeData;
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiInt64Type.ExtractTypeData;
begin
  FMinValue := TypeData.MinInt64Value;
  FMaxValue := TypeData.MaxInt64Value;

  FNeedRangeCheck := (FMinValue <> MinInt64) or (FMaxValue <> MaxInt64);
end;

{*
  Teste si une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @return True si Value est dans l'intervalle supporté, False sinon
*}
function TSepiInt64Type.InRange(Value : Int64) : boolean;
begin
  Result := (Value >= FMinValue) and (Value <= FMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @throws ERangeError Value n'était pas dans l'intervalle supporté
*}
procedure TSepiInt64Type.CheckInRange(Value : Int64);
begin
  if not InRange(Value) then
    raise ERangeError.CreateRes(@SRangeError);
end;

{-----------------------}
{ Classe TSepiFloatType }
{-----------------------}

{*
  Recense un type flottant natif
*}
constructor TSepiFloatType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type flottant depuis un flux
*}
constructor TSepiFloatType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(FloatTypeDataLength);
  Stream.ReadBuffer(TypeData^, FloatTypeDataLength);

  ExtractTypeData;
end;

{*
  Crée un nouveau type flottant
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AFloatType   Type de flottant
*}
constructor TSepiFloatType.Create(AOwner : TSepiMeta; const AName : string;
  AFloatType : TFloatType = ftDouble);
begin
  inherited Create(AOwner, AName, tkFloat);

  AllocateTypeInfo(FloatTypeDataLength);
  TypeData.FloatType := AFloatType;

  ExtractTypeData;
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiFloatType.ExtractTypeData;
begin
  FFloatType := TypeData.FloatType;
end;

{-----------------------------}
{ Classe TSepiShortStringType }
{-----------------------------}

{*
  Recense un type chaîne courte natif
*}
constructor TSepiShortStringType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type chaîne courte depuis un flux
*}
constructor TSepiShortStringType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(ShortStringTypeDataLength);
  Stream.ReadBuffer(TypeData^, ShortStringTypeDataLength);

  ExtractTypeData;
end;

{*
  Crée un nouveau type chaîne courte
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AMaxLength   Longueur maximale
*}
constructor TSepiShortStringType.Create(AOwner : TSepiMeta;
  const AName : string; AMaxLength : integer = 255);
begin
  inherited Create(AOwner, AName, tkString);

  AllocateTypeInfo(ShortStringTypeDataLength);
  TypeData.MaxLength := AMaxLength;

  ExtractTypeData;
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiShortStringType.ExtractTypeData;
begin
  FMaxLength := TypeData.MaxLength;
end;

{------------------------}
{ Classe TSepiStringType }
{------------------------}

{*
  Recense un type chaîne longue natif
*}
constructor TSepiStringType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type chaîne longue depuis un flux
*}
constructor TSepiStringType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(StringTypeDataLength);
  Stream.ReadBuffer(TypeData^, StringTypeDataLength);

  ExtractTypeData;
end;

{*
  Crée un nouveau type chaîne longue
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AIsUnicode   Indique si la chaîne est Unicode ou non
*}
constructor TSepiStringType.Create(AOwner : TSepiMeta; const AName : string;
  AIsUnicode : boolean = False);
var AKind : TTypeKind;
begin
  if AIsUnicode then AKind := tkWString else AKind := tkLString;
  inherited Create(AOwner, AName, AKind);

  AllocateTypeInfo(StringTypeDataLength);

  ExtractTypeData;
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiStringType.ExtractTypeData;
begin
  FIsUnicode := Kind = tkWString;
end;

{-------------------------}
{ Classe TSepiBooleanType }
{-------------------------}

{*
  Recense un type booléen natif
*}
constructor TSepiBooleanType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type booléen depuis un flux
*}
constructor TSepiBooleanType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(EnumTypeDataLength);
  Stream.ReadBuffer(TypeData^, EnumTypeDataLength);

  ExtractTypeData;
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiBooleanType.ExtractTypeData;
begin
  case TypeData.OrdType of
    otSByte : FBooleanKind := bkByteBool;
    otSWord : FBooleanKind := bkWordBool;
    otSLong : FBooleanKind := bkLongBool;
    else FBooleanKind := bkBoolean;
  end;
end;

{----------------------}
{ Classe TSepiEnumType }
{----------------------}

{*
  Recense un type énuméré natif
*}
constructor TSepiEnumType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type énuméré depuis un flux
*}
constructor TSepiEnumType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(EnumTypeDataLength);
  Stream.ReadBuffer(TypeData^, StringTypeDataLength);

  ExtractTypeData;
end;

{*
  Crée un nouveau type énuméré
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AIsUnicode   Indique si la chaîne est Unicode ou non
*}
constructor TSepiEnumType.Create(AOwner : TSepiMeta; const AName : string;
  const AValues : array of string);
begin
  inherited Create(AOwner, AName, tkEnumeration);

  AllocateTypeInfo(EnumTypeDataLength);

  ExtractTypeData;
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiEnumType.ExtractTypeData;
begin
end;

{constructor TSepiEnumType.Load(AOwner : TSepiMeta; Stream : TStream);
var Count, I : integer;
    Str : string;
begin
  inherited;
  Count := 0;
  Stream.ReadBuffer(Count, 1);
  SetLength(FValues, Count);
  for I := 0 to Count-1 do
  begin
    Str := ReadStrFromStream(Stream);
    FValues[I] := Str;
    TSepiConstant.Create(AOwner, Str, Self).Value.WriteBuffer(I, 4);
  end;
end;

constructor TSepiEnumType.Create(AOwner : TSepiMeta; const AName : string;
  const AValues : array of string);
var I : integer;
begin
  inherited Create(AOwner, AName, tkEnumeration);
  SetLength(FValues, Length(AValues));
  for I := 0 to Length(AValues)-1 do
  begin
    FValues[I] := AValues[I];
    TSepiConstant.Create(AOwner, AValues[I], Self).Value.WriteBuffer(I, 4);
  end;
end;}

{*
  Nombre de valeurs du type énuméré
  @retun Nombre de valeurs
*}
function TSepiEnumType.GetValueCount : integer;
begin
  Result := Length(FValues);
end;

{*
  Tableau zero-based des noms des valeurs
*}
function TSepiEnumType.GetValues(Index : integer) : string;
begin
  Result := FValues[Index];
end;

{*
  [@inheritDoc]
*}
function TSepiEnumType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (TypeData.CompType^) = (AType.TypeData.CompType^);
end;

{---------------------}
{ Classe TSepiSetType }
{---------------------}

constructor TSepiSetType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FItemType, 4);
end;

constructor TSepiSetType.Create(AOwner : TSepiMeta; const AName : string;
  AItemType : TSepiType);
begin
  inherited Create(AOwner, AName, tkSet);
  FItemType := AItemType;
end;

procedure TSepiSetType.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FItemType);
end;

function TSepiSetType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType.Kind = tkSet) and
    FItemType.CompatibleWith(TSepiSetType(AType).FItemType);
end;

{------------------------}
{ Classe TSepiRecordType }
{------------------------}

constructor TSepiRecordType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
end;

constructor TSepiRecordType.Create(AOwner : TSepiMeta; const AName : string);
begin
  inherited Create(AOwner, AName, tkRecord);
end;

function TSepiRecordType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := Self = AType;
end;

{-----------------------}
{ Classe TSepiClassType }
{-----------------------}

constructor TSepiClassType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FAncestor, 4);
  FDelphiClass := nil;
  Stream.ReadBuffer(FObjectType, 4);
end;

constructor TSepiClassType.Create(AOwner : TSepiMeta; const AName : string;
  AAncestor : TSepiClassType; ADelphiClass : TClass;
  AObjectType : TSepiObjectType);
begin
  inherited Create(AOwner, AName, tkClass);
  FAncestor := AAncestor;
  FDelphiClass := ADelphiClass;
  FObjectType := AObjectType;
end;

procedure TSepiClassType.ChildAdded(Child : TSepiMeta);
begin
  inherited;
  //Child.Visibility := FObjectType.FCurrentVisibility;
end;

procedure TSepiClassType.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FAncestor);
  FDelphiClass := FAncestor.DelphiClass;
  OwningUnit.LoadRef(FObjectType);
end;

function TSepiClassType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType.Kind = tkClass) and
    TSepiClassType(AType).InheritsFrom(Self);
end;

function TSepiClassType.InheritsFrom(AParent : TSepiClassType) : boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FAncestor) and FAncestor.InheritsFrom(AParent));
end;

(*{------------------------}
{ Classe TSepiObjectType }
{------------------------}

constructor TSepiObjectType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FAncestor, 4);
  FDelphiClass := nil;
  Stream.ReadBuffer(FClassType, 4);
  FCurrentVisibility := mvPrivate;
end;

constructor TSepiObjectType.Create(AOwner : TSepiMeta; const AName : string;
  AAncestor : TSepiObjectType; ADelphiClass : TClass);
begin
  inherited Create(AOwner, AName, tkUnknown);
  FAncestor := AAncestor;
  FDelphiClass := ADelphiClass;
  FClassType := TSepiClassType.Create(AOwner, 'CLASS$'+AName,
    AAncestor.ObjClassType, ADelphiClass, Self);
  FCurrentVisibility := mvPrivate;
end;

procedure TSepiObjectType.ChildAdded(Child : TSepiMeta);
begin
  inherited;
  Child.Visibility := FCurrentVisibility;
end;

procedure TSepiObjectType.Loaded;
begin
  OwningUnit.LoadRef(FAncestor);
  OwningUnit.LoadRef(FClassType);
  FDelphiClass := FClassType.DelphiClass;
end;

function TSepiObjectType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType.Kind = tkObject) and
    TSepiObjectType(AType).InheritsFrom(Self);
end;

function TSepiObjectType.InheritsFrom(AParent : TSepiObjectType) : boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FAncestor) and FAncestor.InheritsFrom(AParent));
end;*)

{-----------------------}
{ Classe TSepiArrayType }
{-----------------------}

constructor TSepiArrayType.Load(AOwner : TSepiMeta; Stream : TStream);
var DimCount : integer;
begin
  inherited;
  DimCount := 0;
  Stream.ReadBuffer(DimCount, 1);
  SetLength(FDimensions, DimCount);
  Stream.ReadBuffer(FDimensions[0], 4*DimCount);
  Stream.ReadBuffer(FItemType, 4);
end;

constructor TSepiArrayType.Create(AOwner : TSepiMeta; const AName : string;
  const ADimensions : array of integer; AItemType : TSepiType);
var I : integer;
begin
  inherited Create(AOwner, AName, tkArray);
  SetLength(FDimensions, Length(ADimensions));
  for I := 0 to Length(ADimensions) do
    FDimensions[I] := ADimensions[I];
  FItemType := AItemType;
end;

destructor TSepiArrayType.Destroy;
begin
  SetLength(FDimensions, 0);
  inherited Destroy;
end;

function TSepiArrayType.GetDimCount : integer;
begin
  Result := Length(FDimensions);
end;

function TSepiArrayType.GetDimensions(Index : integer) : integer;
begin
  Result := FDimensions[Index];
end;

procedure TSepiArrayType.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FItemType);
end;

function TSepiArrayType.CompatibleWith(AType : TSepiType) : boolean;
var ArrayType : TSepiArrayType;
    I : integer;
begin
  if Self = AType then
  begin
    Result := True;
    exit;
  end;

  Result := False;
  if AType.Kind <> tkArray then exit;
  ArrayType := TSepiArrayType(AType);

  if FItemType <> ArrayType.FItemType then exit;
  if Length(FDimensions) <> Length(ArrayType.FDimensions) then exit;
  for I := 0 to Length(FDimensions)-1 do
    if FDimensions[I] <> ArrayType.FDimensions[I] then exit;

  Result := True;
end;

{--------------------------}
{ Classe TSepiDynArrayType }
{--------------------------}

constructor TSepiDynArrayType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FItemType, 4);
end;

constructor TSepiDynArrayType.Create(AOwner : TSepiMeta; const AName : string;
  AItemType : TSepiType);
begin
  inherited Create(AOwner, AName, tkDynArray);
  FItemType := AItemType;
end;

procedure TSepiDynArrayType.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FItemType);
end;

function TSepiDynArrayType.CompatibleWith(AType : TSepiType) : boolean;
begin
  if Self = AType then
  begin
    Result := True;
    exit;
  end;

  if AType.Kind = tkDynArray then
  begin
    Result := FItemType = TSepiDynArrayType(AType).FItemType;
  end else
  if AType.Kind = tkArray then
  begin
    Result := (FItemType = TSepiArrayType(AType).FItemType) and
      (TSepiArrayType(AType).DimCount = 0);
  end else
  begin
    Result := False;
  end;
end;

{---------------------------}
{ Classe TSepiMethodRefType }
{---------------------------}

constructor TSepiMethodRefType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FSignature := TSepiMethodSignature.Create(Self);
  FSignature.Load(Stream);
end;

constructor TSepiMethodRefType.Create(AOwner : TSepiMeta; const AName : string);
begin
  inherited Create(AOwner, AName, tkMethod);
  FSignature := TSepiMethodSignature.Create(Self);
end;

destructor TSepiMethodRefType.Destroy;
begin
  FSignature.Free;
  inherited Destroy;
end;

function TSepiMethodRefType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType.Kind = tkMethod) and
    FSignature.CompatibleWith(TSepiMethodRefType(AType).FSignature);
end;

initialization
  SepiRegisterMetaClasses([
    TSepiIntegerType, TSepiCharType, TSepiInt64Type, TSepiFloatType,
    TSepiStringType, TSepiEnumType, TSepiSetType, TSepiRecordType,
    TSepiClassType, TSepiArrayType, TSepiDynArrayType, TSepiMethodRefType
  ]);
end.

