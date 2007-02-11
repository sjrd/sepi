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

implementation

const
  // Tailles de structure TTypeData en fonction des types
  ArrayTypeDataLength = 0;
  DynArrayTypeDataLengthBase =
    sizeof(Longint) + 2*sizeof(Pointer) + sizeof(integer);

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

initialization
  SepiRegisterMetaClasses([
    TSepiArrayType, TSepiDynArrayType
  ]);
end.

