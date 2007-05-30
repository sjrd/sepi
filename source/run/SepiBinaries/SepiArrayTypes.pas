{*
  Définit les classes de gestion des types tableau
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiArrayTypes;

interface

uses
  Classes, SysUtils, ScUtils, SepiMetaUnits, SysConst, TypInfo, ScLists;

type
  {*
    Informations sur une dimension de tableau
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TDimInfo = record
    MinValue : integer;
    MaxValue : integer;
  end;

  {*
    Type tableau statique (à N dimensions)
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiArrayType = class(TSepiType)
  private
    FDimCount : integer;             /// Nombre de dimensions
    FDimensions : array of TDimInfo; /// Dimensions
    FElementType : TSepiType;        /// Type des éléments

    procedure MakeSize;
    procedure MakeTypeInfo;

    function GetDimensions(Index, Kind : integer) : integer;
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      const ADimensions : array of integer; AElementType : TSepiType;
      AIsNative : boolean = False; ATypeInfo : PTypeInfo = nil);

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property DimCount : integer read FDimCount;

    /// Bornes inférieures des dimensions
    property MinValues [index : integer] : integer index 1 read GetDimensions;
    /// Bornes supérieures des dimensions
    property MaxValues [index : integer] : integer index 2 read GetDimensions;
    /// Nombres d'éléments des dimensions
    property Dimensions[index : integer] : integer index 3 read GetDimensions;

    property ElementType : TSepiType read FElementType;
  end;

  {*
    Type tableau dynamique (à une dimension)
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiDynArrayType = class(TSepiType)
  private
    FElementType : TSepiType;     /// Type des éléments

    procedure MakeTypeInfo;
  protected
    procedure ListReferences; override;
    procedure Save(Stream : TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AElementType : TSepiType);

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property ElementType : TSepiType read FElementType;
  end;

implementation

type
  PArrayTypeData = ^TArrayTypeData;
  TArrayTypeData = packed record
    Size : Cardinal;
    Count : Cardinal;
    ElemType : PPTypeInfo;
    ElemOffset : Cardinal; // always 0
  end;

const
  // Tailles de structure TTypeData en fonction des types
  ArrayTypeDataLength = sizeof(TArrayTypeData);
  DynArrayTypeDataLengthBase =
    sizeof(Longint) + 2*sizeof(Pointer) + sizeof(integer);

{-----------------------}
{ Classe TSepiArrayType }
{-----------------------}

{*
  Charge un type entier depuis un flux
*}
constructor TSepiArrayType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  FDimCount := 0;
  Stream.ReadBuffer(FDimCount, 1);

  SetLength(FDimensions, FDimCount);
  Stream.ReadBuffer(FDimensions[0], FDimCount*sizeof(TDimInfo));
  OwningUnit.ReadRef(Stream, FElementType);

  MakeSize;
  FNeedInit := FElementType.NeedInit;

  MakeTypeInfo;
end;

{*
  Crée un nouveau type tableau
  @param AOwner         Propriétaire du type
  @param AName          Nom du type
  @param ADimensions    Dimensions du tableau [Min1, Max1, Min2, Max2, ...]
  @param AElementType   Type des éléments
*}
constructor TSepiArrayType.Create(AOwner : TSepiMeta; const AName : string;
  const ADimensions : array of integer; AElementType : TSepiType;
  AIsNative : boolean = False; ATypeInfo : PTypeInfo = nil);
begin
  inherited Create(AOwner, AName, tkArray);

  FDimCount := Length(ADimensions) div 2;
  SetLength(FDimensions, FDimCount);
  Move(ADimensions[Low(ADimensions)], FDimensions[0],
    FDimCount*sizeof(TDimInfo));

  FElementType := AElementType;

  MakeSize;
  FNeedInit := FElementType.NeedInit;

  if AIsNative then
    ForceNative(ATypeInfo)
  else
    MakeTypeInfo;
end;

{*
  Calcule la taille du tableau et la range dans FSize
*}
procedure TSepiArrayType.MakeSize;
var I : integer;
begin
  FSize := FElementType.Size;
  for I := 0 to DimCount-1 do
    FSize := FSize * Dimensions[I];
end;

{*
  Construit les RTTI (si besoin)
*}
procedure TSepiArrayType.MakeTypeInfo;
begin
  if not NeedInit then exit;

  AllocateTypeInfo(ArrayTypeDataLength);
  with PArrayTypeData(TypeData)^ do
  begin
    Size := FSize;
    Count := FSize div FElementType.Size;
    ElemType := TSepiArrayType(FElementType).TypeInfoRef;
    ElemOffset := 0;
  end;
end;

{*
  Récupère une information sur une dimension
*}
function TSepiArrayType.GetDimensions(Index, Kind : integer) : integer;
begin
  with FDimensions[Index] do case Kind of
    1 : Result := MinValue;
    2 : Result := MaxValue;
    else Result := MaxValue-MinValue+1;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiArrayType.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FElementType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiArrayType.Save(Stream : TStream);
begin
  inherited;
  Stream.WriteBuffer(FDimCount, 1);
  Stream.WriteBuffer(FDimensions[0], FDimCount*sizeof(TDimInfo));
  OwningUnit.WriteRef(Stream, FElementType);
end;

{*
  [@inheritDoc]
*}
function TSepiArrayType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := False;
end;

{--------------------------}
{ Classe TSepiDynArrayType }
{--------------------------}

{*
  Recense un type tableau dynamique natif
*}
constructor TSepiDynArrayType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type tableau dynamique depuis un flux
*}
constructor TSepiDynArrayType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FElementType);

  MakeTypeInfo;
end;

{*
  Crée un nouveau type tableau dynamique
  @param AOwner         Propriétaire du type
  @param AName          Nom du type
  @param AElementType   Type des éléments
*}
constructor TSepiDynArrayType.Create(AOwner : TSepiMeta; const AName : string;
  AElementType : TSepiType);
begin
  inherited Create(AOwner, AName, tkDynArray);

  FElementType := AElementType;
  MakeTypeInfo;
end;

{*
  Construit les RTTI du type tableau dynamique
*}
procedure TSepiDynArrayType.MakeTypeInfo;
var UnitName : ShortString;
    TypeDataLength : integer;
begin
  UnitName := OwningUnit.Name;
  TypeDataLength := DynArrayTypeDataLengthBase + Length(UnitName) + 1;
  AllocateTypeInfo(TypeDataLength);

  FSize := 4;
  FNeedInit := True;

  // Element size
  TypeData.elSize := FElementType.Size;

  // Element RTTI, if need initialization
  // Types which need initialization always have got RTTI
  if FElementType.NeedInit then
    TypeData.elType := TSepiDynArrayType(FElementType).TypeInfoRef
  else
    TypeData.elType := nil;

  // OLE Variant equivalent - always set to -1 at the moment
  { TODO 1 -cMetaunités : OLE Variant dans les RTTI des dyn array }
  TypeData.varType := -1;

  // Element RTTI, independant of cleanup
  // Whe have to check for nul-RTTI, because of records and static arrays
  if Assigned(FElementType.TypeInfo) then
    TypeData.elType2 := TSepiDynArrayType(FElementType).TypeInfoRef
  else
    TypeData.elType2 := nil;

  // Unit name
  Move(UnitName[0], TypeData.DynUnitName[0], Length(UnitName)+1);
end;

{*
  [@inheritDoc]
*}
procedure TSepiDynArrayType.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FElementType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiDynArrayType.Save(Stream : TStream);
begin
  inherited;
  OwningUnit.WriteRef(Stream, FElementType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiDynArrayType.ExtractTypeData;
begin
  inherited;

  FSize := 4;
  FNeedInit := True;

  FElementType := Root.FindType(TypeData.elType2^);
end;

{*
  [@inheritDoc]
*}
function TSepiDynArrayType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := False;
end;

initialization
  SepiRegisterMetaClasses([
    TSepiArrayType, TSepiDynArrayType
  ]);
end.

