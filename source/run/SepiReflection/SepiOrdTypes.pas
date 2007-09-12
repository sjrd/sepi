{*
  Définit les classes de gestion des types ordinaux
  @author sjrd
  @version 1.0
*}
unit SepiOrdTypes;

interface

uses
  Classes, SysUtils, SysConst, TypInfo, ScDelphiLanguage, SepiReflectionCore;

const
  MinInt64 = Int64($FFFFFFFFFFFFFFFF);
  MaxInt64 = Int64($7FFFFFFFFFFFFFFF);

  otUnknown = TOrdType(-1);

type
  {*
    Type de booléen
  *}
  TBooleanKind = (bkBoolean, bkByteBool, bkWordBool, bkLongBool);

  TSepiOrdType = class(TSepiType)
  private
    FMinValue: Longint; /// Valeur minimale
    FMaxValue: Longint; /// Valeur maximale
  protected
    procedure ExtractTypeData; override;
  public
    property MinValue: Longint read FMinValue;
    property MaxValue: Longint read FMaxValue;
  end;

  {*
    Type entier
    @author sjrd
    @version 1.0
  *}
  TSepiIntegerType = class(TSepiOrdType)
  private
    FSigned: Boolean;         /// Indique si l'entier est signé ou non
    FNeedRangeCheck: Boolean; /// Indique s'il faut vérifier les étendues
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AMinValue: Longint = -MaxInt-1; AMaxValue: Longint = MaxInt;
      ASigned: Boolean = True);

    function InRange(Value: Longint): Boolean;
    procedure CheckInRange(Value: Longint);

    property Signed: Boolean read FSigned;
    property NeedRangeCheck: Boolean read FNeedRangeCheck;
  end;

  {*
    Type caractère
    @author sjrd
    @version 1.0
  *}
  TSepiCharType = class(TSepiOrdType)
  private
    FIsUnicode: Boolean;      /// Indique si le caractère est Unicode
    FChrMinValue: WideChar;   /// Valeur minimale
    FChrMaxValue: WideChar;   /// Valeur maximale
    FNeedRangeCheck: Boolean; /// Indique s'il faut vérifier les étendues
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AMinValue: WideChar = #0; AMaxValue: WideChar = #255;
      AForceUnicode: Boolean = False);

    function InRange(Value: WideChar): Boolean;
    procedure CheckInRange(Value: WideChar);

    property IsUnicode: Boolean read FIsUnicode;
    property ChrMinValue: WideChar read FChrMinValue;
    property ChrMaxValue: WideChar read FChrMaxValue;
    property NeedRangeCheck: Boolean read FNeedRangeCheck;
  end;

  {*
    Type entier long (64 bits)
    @author sjrd
    @version 1.0
  *}
  TSepiInt64Type = class(TSepiType)
  private
    FMinValue: Int64;         /// Valeur minimale
    FMaxValue: Int64;         /// Valeur maximale
    FNeedRangeCheck: Boolean; /// Indique s'il faut vérifier les étendues
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AMinValue: Int64 = MinInt64; AMaxValue: Int64 = MaxInt64);

    function InRange(Value: Int64): Boolean;
    procedure CheckInRange(Value: Int64);

    property MinValue: Int64 read FMinValue;
    property MaxValue: Int64 read FMaxValue;
    property NeedRangeCheck: Boolean read FNeedRangeCheck;
  end;

  {*
    Type nombre à virgule flottante
    @author sjrd
    @version 1.0
  *}
  TSepiFloatType = class(TSepiType)
  private
    FFloatType: TFloatType; /// Type de flottant
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;

    function GetAlignment: Integer; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AFloatType: TFloatType = ftDouble);

    property FloatType: TFloatType read FFloatType;
  end;

  {*
    Type booléen
    Note : il est impossible de créer un nouveau type booléen.
    @author sjrd
    @version 1.0
  *}
  TSepiBooleanType = class(TSepiOrdType)
  private
    FBooleanKind: TBooleanKind;
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;

    property BooleanKind: TBooleanKind read FBooleanKind;
  end;

  {*
    Type énumération
    @author sjrd
    @version 1.0
  *}
  TSepiEnumType = class(TSepiOrdType)
  private
    TypeDataLength: Integer;        /// Taille des données de type (non natif)
    FBaseType: TSepiEnumType;       /// Type de base (peut être Self)
    FValueCount: Integer;           /// Nombre de valeurs
    FValues: array of PShortString; /// Noms des valeurs

    procedure CreateConstants;

    function GetNames(Value: Integer): string;
    function GetValues(const Name: string): Integer;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const AValues: array of string);

    function CompatibleWith(AType: TSepiType): Boolean; override;

    property BaseType: TSepiEnumType read FBaseType;
    property ValueCount: Integer read FValueCount;
    property Names[Value: Integer]: string read GetNames;
    property Values[const Name: string]: Integer read GetValues;
  end;

  {*
    Type ensemble
    @author sjrd
    @version 1.0
  *}
  TSepiSetType = class(TSepiType)
  private
    FCompType: TSepiOrdType;  /// Type des éléments
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;

    function GetAlignment: Integer; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      ACompType: TSepiOrdType); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      ACompType: PTypeInfo); overload;

    function CompatibleWith(AType: TSepiType): Boolean; override;

    property CompType: TSepiOrdType read FCompType;
  end;

  {*
    Information sur un pointeur déclaré en forward
    @author sjrd
    @version 1.0
  *}
  TSepiForwardPointerInfo = record
    Owner: TSepiMeta;
    Name: string;
    PointToTypeInfo: PTypeInfo;
    PointToName: string;
    IsNative: Boolean;
  end;
  PSepiForwardPointerInfo = ^TSepiForwardPointerInfo;

  {*
    Type pointeur
    @author sjrd
    @version 1.0
  *}
  TSepiPointerType = class(TSepiOrdType)
  private
    FPointTo: TSepiType; /// Type vers lequel pointe le pointeur

    FForwardInfo: PSepiForwardPointerInfo; /// Infos conservées en forward
  protected
    procedure Loaded; override;

    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      APointTo: TSepiType; AIsNative: Boolean = False); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      APointTo: PTypeInfo; AIsNative: Boolean = False); overload;
    constructor Create(AOwner: TSepiMeta; const AName, APointTo: string;
      AIsNative: Boolean = False); overload;

    class function NewInstance: TObject; override;

    property PointTo: TSepiType read FPointTo;
  end;

implementation

const
  // Tailles de structure TTypeData en fonction des types
  IntegerTypeDataLength = sizeof(TOrdType) + 2*sizeof(Longint);
  CharTypeDataLength = IntegerTypeDataLength;
  Int64TypeDataLength = 2*sizeof(Int64);
  FloatTypeDataLength = sizeof(TFloatType);
  EnumTypeDataLength = sizeof(TOrdType) + 2*sizeof(Longint) + sizeof(Pointer);
  SetTypeDataLength = sizeof(TOrdType) + sizeof(Pointer);

{---------------------}
{ Classe TSepiOrdType }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TSepiOrdType.ExtractTypeData;
begin
  inherited;

  case TypeData.OrdType of
    otSByte, otUByte: FSize := 1;
    otSWord, otUWord: FSize := 2;
    otSLong, otULong: FSize := 4;
  end;

  FMinValue := TypeData.MinValue;
  FMaxValue := TypeData.MaxValue;
end;

{-------------------------}
{ Classe TSepiIntegerType }
{-------------------------}

{*
  Recense un type entier natif
*}
constructor TSepiIntegerType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type entier depuis un flux
*}
constructor TSepiIntegerType.Load(AOwner: TSepiMeta; Stream: TStream);
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
constructor TSepiIntegerType.Create(AOwner: TSepiMeta; const AName: string;
  AMinValue: Longint = -MaxInt-1; AMaxValue: Longint = MaxInt;
  ASigned: Boolean = True);
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
  [@inheritDoc]
*}
procedure TSepiIntegerType.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, IntegerTypeDataLength);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIntegerType.ExtractTypeData;
begin
  inherited;

  FSigned := TypeData.OrdType in [otSByte, otSWord, otSLong];
  FNeedRangeCheck := (FMinValue <> -MaxInt - 1) or (FMaxValue <> MaxInt);
end;

{*
  Teste si une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @return True si Value est dans l'intervalle supporté, False sinon
*}
function TSepiIntegerType.InRange(Value: Longint): Boolean;
begin
  Result := (Value >= FMinValue) and (Value <= FMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @throws ERangeError Value n'était pas dans l'intervalle supporté
*}
procedure TSepiIntegerType.CheckInRange(Value: Longint);
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
constructor TSepiCharType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type caractère depuis un flux
*}
constructor TSepiCharType.Load(AOwner: TSepiMeta; Stream: TStream);
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
constructor TSepiCharType.Create(AOwner: TSepiMeta; const AName: string;
  AMinValue: WideChar = #0; AMaxValue: WideChar = #255;
  AForceUnicode: Boolean = False);
var
  AKind: TTypeKind;
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
  [@inheritDoc]
*}
procedure TSepiCharType.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, CharTypeDataLength);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiCharType.ExtractTypeData;
begin
  inherited;

  FIsUnicode := Kind = tkWChar;
  FChrMinValue := WideChar(MinValue);
  FChrMaxValue := WideChar(MaxValue);

  if FIsUnicode then
    FNeedRangeCheck := (FChrMinValue <> #0) or (FChrMaxValue <> #65535)
  else
    FNeedRangeCheck := (FChrMinValue <> #0) or (FChrMaxValue <> #255);
end;

{*
  Teste si une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @return True si Value est dans l'intervalle supporté, False sinon
*}
function TSepiCharType.InRange(Value: WideChar): Boolean;
begin
  Result := (Value >= FChrMinValue) and (Value <= FChrMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @throws ERangeError Value n'était pas dans l'intervalle supporté
*}
procedure TSepiCharType.CheckInRange(Value: WideChar);
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
constructor TSepiInt64Type.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type entier 64 bits depuis un flux
*}
constructor TSepiInt64Type.Load(AOwner: TSepiMeta; Stream: TStream);
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
constructor TSepiInt64Type.Create(AOwner: TSepiMeta; const AName: string;
  AMinValue: Int64 = MinInt64; AMaxValue: Int64 = MaxInt64);
begin
  inherited Create(AOwner, AName, tkInt64);

  AllocateTypeInfo(Int64TypeDataLength);

  TypeData.MinValue := AMinValue;
  TypeData.MaxValue := AMaxValue;

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
procedure TSepiInt64Type.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, Int64TypeDataLength);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiInt64Type.ExtractTypeData;
begin
  inherited;

  FSize := 8;
  FParamBehavior.AlwaysByStack := True;
  FMinValue := TypeData.MinInt64Value;
  FMaxValue := TypeData.MaxInt64Value;

  FNeedRangeCheck := (FMinValue <> MinInt64) or (FMaxValue <> MaxInt64);
end;

{*
  Teste si une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @return True si Value est dans l'intervalle supporté, False sinon
*}
function TSepiInt64Type.InRange(Value: Int64): Boolean;
begin
  Result := (Value >= FMinValue) and (Value <= FMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @throws ERangeError Value n'était pas dans l'intervalle supporté
*}
procedure TSepiInt64Type.CheckInRange(Value: Int64);
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
constructor TSepiFloatType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type flottant depuis un flux
*}
constructor TSepiFloatType.Load(AOwner: TSepiMeta; Stream: TStream);
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
constructor TSepiFloatType.Create(AOwner: TSepiMeta; const AName: string;
  AFloatType: TFloatType = ftDouble);
begin
  inherited Create(AOwner, AName, tkFloat);

  AllocateTypeInfo(FloatTypeDataLength);
  TypeData.FloatType := AFloatType;

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
procedure TSepiFloatType.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, FloatTypeDataLength);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiFloatType.ExtractTypeData;
begin
  inherited;

  FFloatType := TypeData.FloatType;

  case FFloatType of
    ftSingle: FSize := 4;
    ftDouble, ftComp, ftCurr: FSize := 8;
    ftExtended: FSize := 10;
  end;

  FParamBehavior.AlwaysByStack := True;
end;

{*
  [@inheritDoc]
*}
function TSepiFloatType.GetAlignment: Integer;
begin
  if FloatType = ftExtended then
    Result := 8
  else
    Result := Size;
end;

{-------------------------}
{ Classe TSepiBooleanType }
{-------------------------}

{*
  Recense un type booléen natif
*}
constructor TSepiBooleanType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type booléen depuis un flux
*}
constructor TSepiBooleanType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  AllocateTypeInfo(EnumTypeDataLength);
  Stream.ReadBuffer(TypeData^, EnumTypeDataLength);

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
procedure TSepiBooleanType.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, EnumTypeDataLength);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiBooleanType.ExtractTypeData;
begin
  inherited;

  case TypeData.OrdType of
    otSByte: FBooleanKind := bkByteBool;
    otSWord: FBooleanKind := bkWordBool;
    otSLong: FBooleanKind := bkLongBool;
    else FBooleanKind := bkBoolean;
  end;
end;

{----------------------}
{ Classe TSepiEnumType }
{----------------------}

{*
  Recense un type énuméré natif
*}
constructor TSepiEnumType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  TypeDataLength := 0; // not used
  ExtractTypeData;
  CreateConstants;
end;

{*
  Charge un type énuméré depuis un flux
*}
constructor TSepiEnumType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(TypeDataLength, 4);
  AllocateTypeInfo(TypeDataLength);
  Stream.ReadBuffer(TypeData^, TypeDataLength);

  OwningUnit.ReadRef(Stream, FBaseType);
  TypeData.BaseType := FBaseType.TypeInfoRef;

  ExtractTypeData;
end;

{*
  Crée un nouveau type énuméré
  @param AOwner    Propriétaire du type
  @param AName     Nom du type
  @param AValues   Noms des éléments de l'énumération
*}
constructor TSepiEnumType.Create(AOwner: TSepiMeta; const AName: string;
  const AValues: array of string);
var
  I, Len: Integer;
  Current: PChar;
  OwningUnitName: ShortString;
begin
  inherited Create(AOwner, AName, tkEnumeration);

  FBaseType := Self;

  // Calcul de la taille des données de type et allocation de celles-ci
  TypeDataLength := EnumTypeDataLength;
  for I := Low(AValues) to High(AValues) do
    Inc(TypeDataLength, Length(AValues[I]));
  Inc(TypeDataLength, Length(AValues)); // Longueurs des chaînes
  Inc(TypeDataLength, Length(OwningUnit.Name)+1);
  AllocateTypeInfo(TypeDataLength);

  // Initialisation des variables privées
  FValueCount := Length(AValues);
  SetLength(FValues, ValueCount);

  // Initialisation des informations scalaires des données de type
  TypeData.OrdType := otUByte;
  TypeData.MinValue := 0;
  TypeData.MaxValue := ValueCount-1;
  TypeData.BaseType := TypeInfoRef;
  inherited ExtractTypeData;

  // Enregistrement des noms d'énumération dans les données de type
  Current := @TypeData.NameList;
  for I := 0 to ValueCount-1 do
  begin
    Len := Length(AValues[Low(AValues)+I]);

    // Enregistrement de l'adresse dans le tableau FValues
    FValues[I] := PShortString(Current);

    // Recopie du nom dans les données de type
    Current[0] := Chr(Len);
    Inc(Current);
    Move(AValues[Low(AValues)+I][1], Current^, Len);

    // Passage à l'élément suivant
    Inc(Current, Len);
  end;

  // Enregistrement du nom de l'unité dans les données de type
  OwningUnitName := OwningUnit.Name;
  Move(OwningUnitName[0], Current[0], Length(OwningUnitName)+1);

  // Créer les constantes énumérées
  CreateConstants;
end;

{*
  Crée les constantes énumérées
*}
procedure TSepiEnumType.CreateConstants;
var
  Value: Integer;
begin
  if BaseType <> Self then Exit;
  for Value := MinValue to MaxValue do
    TSepiConstant.Create(Owner, Self.Names[Value], Value, Self);
end;

{*
  Tableau des noms par leurs valeurs
  @param Value   Valeur d'élément d'énumération
  @return Nom de la valeur Value
*}
function TSepiEnumType.GetNames(Value: Integer): string;
begin
  Result := BaseType.FValues[Value]^;
end;

{*
  Tableau des valeurs par leurs noms
  @param Name   Nom d'élément d'énumération
  @return Valeur du nom donné
*}
function TSepiEnumType.GetValues(const Name: string): Integer;
begin
  Result := GetEnumValue(TypeInfo, Name);
end;

{*
  [@inheritDoc]
*}
procedure TSepiEnumType.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FBaseType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiEnumType.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeDataLength, 4);
  Stream.WriteBuffer(TypeData^, TypeDataLength);
  OwningUnit.WriteRef(Stream, FBaseType);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiEnumType.ExtractTypeData;
var
  Current: PShortString;
  I: Integer;
begin
  inherited;

  if TypeData.BaseType^ = TypeInfo then
  begin
    FBaseType := Self;
    FValueCount := MaxValue+1;

    // Mise au point du tableau des références vers les noms
    SetLength(FValues, ValueCount);
    Current := @TypeData.NameList;
    for I := 0 to ValueCount-1 do
    begin
      FValues[I] := Current;
      Current := SkipPackedShortString(Current);
    end;
  end else
  begin
    FBaseType := Root.FindType(TypeData.BaseType^) as TSepiEnumType;
    FValueCount := BaseType.ValueCount;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiEnumType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := (AType is TSepiEnumType) and
    (BaseType = TSepiEnumType(AType).BaseType);
end;

{---------------------}
{ Classe TSepiSetType }
{---------------------}

{*
  Recense un type ensemble natif
*}
constructor TSepiSetType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  with GetTypeData(ATypeInfo)^ do
    if CompType^.Name[1] = '.' then
      TSepiType.LoadFromTypeInfo(AOwner, CompType^);

  inherited;

  FCompType := Root.FindType(TypeData.CompType^) as TSepiOrdType;

  ExtractTypeData;
end;

{*
  Charge un type ensemble depuis un flux
*}
constructor TSepiSetType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  AllocateTypeInfo(SetTypeDataLength);
  Stream.ReadBuffer(TypeData^, SetTypeDataLength);

  OwningUnit.ReadRef(Stream, FCompType);
  TypeData.CompType := FCompType.TypeInfoRef;

  ExtractTypeData;
end;

{*
  Crée un nouveau type ensemble
  @param AOwner      Propriétaire du type
  @param AName       Nom du type
  @param ACompType   Type des éléments de l'ensemble
*}
constructor TSepiSetType.Create(AOwner: TSepiMeta; const AName: string;
  ACompType: TSepiOrdType);
begin
  inherited Create(AOwner, AName, tkSet);

  FCompType := ACompType;

  AllocateTypeInfo(SetTypeDataLength);

  case CompType.MaxValue - CompType.MinValue + 1 of
    1..8: TypeData.OrdType := otUByte;
    9..16: TypeData.OrdType := otUWord;
    17..32: TypeData.OrdType := otULong;
    else TypeData.OrdType := otUnknown;
  end;

  TypeData.CompType := FCompType.TypeInfoRef;

  ExtractTypeData;
end;

{*
  Crée un nouveau type ensemble
  @param AOwner      Propriétaire du type
  @param AName       Nom du type
  @param ACompType   RTTI du type des éléments de l'ensemble
*}
constructor TSepiSetType.Create(AOwner: TSepiMeta; const AName: string;
  ACompType: PTypeInfo);
begin
  Create(AOwner, AName,
    AOwner.Root.FindType(ACompType) as TSepiOrdType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetType.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FCompType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetType.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, SetTypeDataLength);
  OwningUnit.WriteRef(Stream, FCompType);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiSetType.ExtractTypeData;
begin
  inherited;

  FSize := (FCompType.MaxValue - FCompType.MinValue) div 8 + 1;
  if FSize = 3 then FSize := 4;

  FParamBehavior.AlwaysByAddress := Size > 4;
end;

{*
  [@inheritDoc]
*}
function TSepiSetType.GetAlignment: Integer;
begin
  if Size <= 4 then
    Result := Size
  else
    Result := 1;
end;

{*
  [@inheritDoc]
*}
function TSepiSetType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := (AType.Kind = tkSet) and
    FCompType.CompatibleWith(TSepiSetType(AType).FCompType);
end;

{-------------------------}
{ Classe TSepiPointerType }
{-------------------------}

{*
  Charge un type pointeur depuis un flux
*}
constructor TSepiPointerType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  FSize := 4;
  OwningUnit.ReadRef(Stream, FPointTo);
end;

{*
  Crée un nouveau type pointeur
  @param AOwner     Propriétaire du type
  @param AName      Nom du type
  @param APointTo   Type vers lequel pointe le pointeur
*}
constructor TSepiPointerType.Create(AOwner: TSepiMeta; const AName: string;
  APointTo: TSepiType; AIsNative: Boolean = False);
begin
  inherited Create(AOwner, AName, tkUnknown);

  FSize := 4;
  FPointTo := APointTo;

  if AIsNative then
    ForceNative;
end;

{*
  Crée un nouveau type pointeur
  @param AOwner     Propriétaire du type
  @param AName      Nom du type
  @param APointTo   RTTI tu type vers lequel pointe le pointeur
*}
constructor TSepiPointerType.Create(AOwner: TSepiMeta; const AName: string;
  APointTo: PTypeInfo; AIsNative: Boolean = False);
var
  APointToType: TSepiType;
begin
  if FForwardInfo = nil then
    APointToType := AOwner.Root.GetType(APointTo)
  else
    APointToType := AOwner.Root.FindType(APointTo);

  if (APointTo = nil) or (APointToType <> nil) then
    Create(AOwner, AName, APointToType, AIsNative) else
  begin
    New(FForwardInfo);
    FForwardInfo.Owner := AOwner;
    FForwardInfo.Name := AName;
    FForwardInfo.PointToTypeInfo := APointTo;
    Pointer(FForwardInfo.PointToName) := nil;
    FForwardInfo.IsNative := AIsNative;
    TSepiPointerType(AOwner).AddForward(AName, Self);
  end;
end;

{*
  Crée un nouveau type pointeur
  @param AOwner     Propriétaire du type
  @param AName      Nom du type
  @param APointTo   Nom tu type vers lequel pointe le pointeur
*}
constructor TSepiPointerType.Create(AOwner: TSepiMeta;
  const AName, APointTo: string; AIsNative: Boolean = False);
var
  APointToType: TSepiType;
begin
  if FForwardInfo = nil then
    APointToType := AOwner.Root.GetType(APointTo)
  else
    APointToType := AOwner.Root.FindType(APointTo);

  if (APointTo = '') or (APointToType <> nil) then
    Create(AOwner, AName, APointToType, AIsNative) else
  begin
    New(FForwardInfo);
    FForwardInfo.Owner := AOwner;
    FForwardInfo.Name := AName;
    FForwardInfo.PointToTypeInfo := nil;
    Pointer(FForwardInfo.PointToName) := nil;
    FForwardInfo.PointToName := APointTo;
    FForwardInfo.IsNative := AIsNative;
    TSepiPointerType(AOwner).AddForward(AName, Self);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiPointerType.Loaded;
begin
  if IsForward and (FForwardInfo <> nil) then with FForwardInfo^ do
  begin
    if PointToTypeInfo <> nil then
      Create(Owner, Name, PointToTypeInfo, IsNative)
    else
      Create(Owner, Name, PointToName, IsNative);
    Dispose(FForwardInfo);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiPointerType.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FPointTo);
end;

{*
  [@inheritDoc]
*}
procedure TSepiPointerType.Save(Stream: TStream);
begin
  inherited;
  OwningUnit.WriteRef(Stream, FPointTo);
end;

{*
  Crée une nouvelle instance de TSepiPointerType
  @return Instance créée
*}
class function TSepiPointerType.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSepiPointerType(Result).FSize := 4;
  TSepiPointerType(Result).FNeedInit := False;
end;

initialization
  SepiRegisterMetaClasses([
    TSepiIntegerType, TSepiCharType, TSepiInt64Type, TSepiFloatType,
    TSepiBooleanType, TSepiEnumType, TSepiSetType, TSepiPointerType
    ]);
end.

