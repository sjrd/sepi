{*
  Définit les classes de gestion des types
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiTypes;

interface

uses
  Classes, SysUtils, ScUtils, SepiMetaUnits, SysConst, TypInfo, ScLists;

type
  {*
    Type de stockage d'une chaîne de caractères
  *}
  TSepiStringStorage = (ssString, ssShortString, ssPChar);

  {*
    Type entier
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiIntegerType = class(TSepiType)
  private
    FMinValue : integer;       /// Valeur minimale
    FMaxValue : integer;       /// Valeur maximale
    FSigned : boolean;         /// Indique si l'entier est signé ou non
    FNeedRangeCheck : boolean;
    procedure ExtractTypeData; /// Indique s'il faut vérifier les étendues
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor RegisterTypeInfo(AOwner : TSepiMeta; ATypeInfo : PTypeInfo);
    constructor Create(AOwner : TSepiMeta; AName : string;
      AMinValue : integer = -MaxInt-1; AMaxValue : integer = MaxInt;
      ASigned : boolean = True);

    function InRange(Value : integer) : boolean;
    procedure CheckInRange(Value : integer);

    property MinValue : integer read FMinValue;
    property MaxValue : integer read FMaxValue;
    property Signed : boolean read FSigned;
    property NeedRangeCheck : boolean read FNeedRangeCheck;
  end;

  {*
    Type entier long (64 bits)
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiInt64Type = class(TSepiType)
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string);
  end;

  {*
    Type nombre à virgule flottante
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiDoubleType = class(TSepiType)
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string);
  end;

  {*
    Type chaîne de caractères
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiStringType = class(TSepiType)
  private
    FStorage : TSepiStringStorage; /// Type de stockage
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
      AStorage : TSepiStringStorage = ssString);
  end;

  {*
    Type énumération
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiEnumType = class(TSepiType)
  private
    FValues : array of string; /// Noms des valeurs

    function GetValueCount : integer;
    function GetValues(Index : integer) : string;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
      AValues : array of string);

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
    constructor Create(AOwner : TSepiMeta; AName : string;
      AItemType : TSepiType);

    function CompatibleWith(AType : TSepiType) : boolean; override;
  end;

  {*
    Type enregistrement
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiRecordType = class(TSepiMetaVariableContainer)
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string);

    function CompatibleWith(AType : TSepiType) : boolean; override;
  end;

  TSepiObjectType = class end;

  {*
    Type classe
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiClassType = class(TSepiMetaOmniContainer)
  private
    FAncestor : TSepiClassType;    /// Ancêtre
    FDelphiClass : TClass;         /// Classe Delphi d'importation
    FObjectType : TSepiObjectType; /// Type objet correspondant
  protected
    procedure ChildAdded(Child : TSepiMeta); override;

    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
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
    constructor Create(AOwner : TSepiMeta; AName : string;
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
    constructor Create(AOwner : TSepiMeta; AName : string;
      ADimensions : array of integer; AItemType : TSepiType);
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
    constructor Create(AOwner : TSepiMeta; AName : string;
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
    constructor Create(AOwner : TSepiMeta; AName : string);
    destructor Destroy; override;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property Signature : TSepiMethodSignature read FSignature;
  end;

  (*{*
    Type délégation
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiDelegateType = class(TSepiType)
  private
    FSignature : TSepiMethodSignature; /// Signature
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string);
    destructor Destroy; override;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property Signature : TSepiMethodSignature read FSignature;
  end;*)

implementation

const
  IntegerTypeDataLength = sizeof(TOrdType) + 2*sizeof(Longint);

{-------------------------}
{ Classe TSepiIntegerType }
{-------------------------}

{*
  Charge un type entier depuis un flux
*}
constructor TSepiIntegerType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FKind := tkInteger;

  AllocateTypeInfo(IntegerTypeDataLength);
  Stream.ReadBuffer(FTypeData^, IntegerTypeDataLength);

  ExtractTypeData;
end;

{*
  Recense un type entier natif
*}
constructor TSepiIntegerType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited Create(AOwner, ATypeInfo.Name);
  FKind := tkInteger;

  FTypeInfo := ATypeInfo;
  FTypeData := GetTypeData(FTypeInfo);

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
constructor TSepiIntegerType.Create(AOwner : TSepiMeta; AName : string;
  AMinValue : integer = -MaxInt-1; AMaxValue : integer = MaxInt;
  ASigned : boolean = True);
begin
  inherited Create(AOwner, AName);
  FKind := tkInteger;

  AllocateTypeInfo(IntegerTypeDataLength);

  if AMinValue < 0 then
  begin
    // Signed
    if (AMinValue < -32768) or (AMaxValue >= 32768) then
      FTypeData.OrdType := otSLong else
    if (AMinValue < -128) or (AMaxValue >= 128) then
      FTypeData.OrdType := otSWord else
    FTypeData.OrdType := otSByte;
  end else
  begin
    // Unsigned
    if AMaxValue >= 65536 then
      FTypeData.OrdType := otULong else
    if AMaxValue >= 256 then
      FTypeData.OrdType := otUWord else
    FTypeData.OrdType := otUByte;
  end;

  FTypeData.MinValue := AMinValue;
  FTypeData.MaxValue := AMaxValue;

  ExtractTypeData;
end;

{*
  Extrait les informations les plus importantes depuis les données de type
*}
procedure TSepiIntegerType.ExtractTypeData;
begin
  FMinValue := FTypeData.MinValue;
  FMaxValue := FTypeData.MaxValue;
  FSigned := FTypeData.OrdType in [otSByte, otSWord, otSLong];

  FNeedRangeCheck := (FMinValue <> -MaxInt - 1) or (FMaxValue <> MaxInt);
end;

{*
  Teste si une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @return True si Value est dans l'intervalle supporté, False sinon
*}
function TSepiIntegerType.InRange(Value : integer) : boolean;
begin
  Result := (Value >= FMinValue) and (Value <= FMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle supporté par le type
  @param Value   Valeur à tester
  @throws ERangeError Value n'était pas dans l'intervalle supporté
*}
procedure TSepiIntegerType.CheckInRange(Value : integer);
begin
  if not InRange(Value) then
    raise ERangeError.CreateRes(@SRangeError);
end;

{-----------------------}
{ Classe TSepiInt64Type }
{-----------------------}

constructor TSepiInt64Type.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FKind := tkInt64;
end;

constructor TSepiInt64Type.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create(AOwner, AName);
  FKind := tkInt64;
end;

{-----------------------}
{ Classe TSepiFloatType }
{-----------------------}

constructor TSepiDoubleType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FKind := tkFloat;
end;

constructor TSepiDoubleType.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create(AOwner, AName);
  FKind := tkFloat;
end;

{------------------------}
{ Classe TSepiStringType }
{------------------------}

constructor TSepiStringType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FKind := tkString;
  FStorage := ssString;
end;

constructor TSepiStringType.Create(AOwner : TSepiMeta; AName : string;
  AStorage : TSepiStringStorage = ssString);
begin
  inherited Create(AOwner, AName);
  FKind := tkString;
  FStorage := AStorage;
end;

{----------------------}
{ Classe TSepiEnumType }
{----------------------}

constructor TSepiEnumType.Load(AOwner : TSepiMeta; Stream : TStream);
var Count, I : integer;
    Str : string;
begin
  inherited;
  FKind := tkEnumeration;
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

constructor TSepiEnumType.Create(AOwner : TSepiMeta; AName : string;
  AValues : array of string);
var I : integer;
begin
  inherited Create(AOwner, AName);
  FKind := tkEnumeration;
  SetLength(FValues, Length(AValues));
  for I := 0 to Length(AValues)-1 do
  begin
    FValues[I] := AValues[I];
    TSepiConstant.Create(AOwner, AValues[I], Self).Value.WriteBuffer(I, 4);
  end;
end;

function TSepiEnumType.GetValueCount : integer;
begin
  Result := Length(FValues);
end;

function TSepiEnumType.GetValues(Index : integer) : string;
begin
  Result := FValues[Index];
end;

function TSepiEnumType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := Self = AType;
end;

{---------------------}
{ Classe TSepiSetType }
{---------------------}

constructor TSepiSetType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FKind := tkSet;
  Stream.ReadBuffer(FItemType, 4);
end;

constructor TSepiSetType.Create(AOwner : TSepiMeta; AName : string;
  AItemType : TSepiType);
begin
  inherited Create(AOwner, AName);
  FKind := tkSet;
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
  FKind := tkRecord;
end;

constructor TSepiRecordType.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create(AOwner, AName);
  FKind := tkRecord;
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
  FKind := tkClass;
  Stream.ReadBuffer(FAncestor, 4);
  FDelphiClass := nil;
  Stream.ReadBuffer(FObjectType, 4);
end;

constructor TSepiClassType.Create(AOwner : TSepiMeta; AName : string;
  AAncestor : TSepiClassType; ADelphiClass : TClass;
  AObjectType : TSepiObjectType);
begin
  inherited Create(AOwner, AName);
  FKind := tkClass;
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
  FKind := tkObject;
  Stream.ReadBuffer(FAncestor, 4);
  FDelphiClass := nil;
  Stream.ReadBuffer(FClassType, 4);
  FCurrentVisibility := mvPrivate;
end;

constructor TSepiObjectType.Create(AOwner : TSepiMeta; AName : string;
  AAncestor : TSepiObjectType; ADelphiClass : TClass);
begin
  inherited Create(AOwner, AName);
  FKind := tkObject;
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
  FKind := tkArray;
  DimCount := 0;
  Stream.ReadBuffer(DimCount, 1);
  SetLength(FDimensions, DimCount);
  Stream.ReadBuffer(FDimensions[0], 4*DimCount);
  Stream.ReadBuffer(FItemType, 4);
end;

constructor TSepiArrayType.Create(AOwner : TSepiMeta; AName : string;
  ADimensions : array of integer; AItemType : TSepiType);
var I : integer;
begin
  inherited Create(AOwner, AName);
  FKind := tkArray;
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
  FKind := tkDynArray;
  Stream.ReadBuffer(FItemType, 4);
end;

constructor TSepiDynArrayType.Create(AOwner : TSepiMeta; AName : string;
  AItemType : TSepiType);
begin
  inherited Create(AOwner, AName);
  FKind := tkDynArray;
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
  FKind := tkMethod;
  FSignature := TSepiMethodSignature.Create(Self);
  FSignature.Load(Stream);
end;

constructor TSepiMethodRefType.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create(AOwner, AName);
  FKind := tkMethod;
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

end.

