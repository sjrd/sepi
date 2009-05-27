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
  Définit les classes de gestion des types tableau
  @author sjrd
  @version 1.0
*}
unit SepiArrayTypes;

interface

uses
  Classes, SysUtils, SysConst, TypInfo, ScUtils, SepiReflectionCore,
  SepiOrdTypes;

type
  {*
    Type tableau
    @author sjrd
    @version 1.0
  *}
  TSepiArrayType = class(TSepiType)
  private
    FElementType: TSepiType; /// Type des éléments
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    {*
      Type de l'index
      @return Type de l'index
    *}
    function GetIndexType: TSepiOrdType; virtual; abstract;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AKind: TTypeKind; AElementType: TSepiType);

    function Equals(Other: TSepiType): Boolean; override;

    property IndexType: TSepiOrdType read GetIndexType;
    property ElementType: TSepiType read FElementType;
  end;

  {*
    Type tableau statique
    @author sjrd
    @version 1.0
  *}
  TSepiStaticArrayType = class(TSepiArrayType)
  private
    FIndexType: TSepiOrdType; /// Type de l'index
    FLowerBound: Integer;     /// Borne inférieure
    FHigherBound: Integer;    /// Borne supérieure

    procedure MakeSize;
    procedure MakeTypeInfo;

    function GetLength: Integer;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    function GetAlignment: Integer; override;
    function GetIndexType: TSepiOrdType; override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;

    constructor Create(AOwner: TSepiMeta; const AName: string;
      AIndexType: TSepiOrdType; ALowerBound, AHigherBound: Integer;
      AElementType: TSepiType; AIsNative: Boolean = False;
      ATypeInfo: PTypeInfo = nil); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AIndexTypeInfo: PTypeInfo; ALowerBound, AHigherBound: Integer;
      AElementTypeInfo: PTypeInfo; AIsNative: Boolean = False;
      ATypeInfo: PTypeInfo = nil); overload;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const AIndexTypeName: string; ALowerBound, AHigherBound: Integer;
      const AElementTypeName: string; AIsNative: Boolean = False;
      ATypeInfo: PTypeInfo = nil); overload;

    constructor Create(AOwner: TSepiMeta; const AName: string;
      const ADimensions: array of Integer; AElementType: TSepiType;
      AIsNative: Boolean = False; ATypeInfo: PTypeInfo = nil); overload;
      deprecated;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const ADimensions: array of Integer; AElementTypeInfo: PTypeInfo;
      AIsNative: Boolean = False; ATypeInfo: PTypeInfo = nil); overload;
      deprecated;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      const ADimensions: array of Integer; const AElementTypeName: string;
      AIsNative: Boolean = False; ATypeInfo: PTypeInfo = nil); overload;
      deprecated;

    constructor Clone(AOwner: TSepiMeta; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;

    property LowerBound: Integer read FLowerBound;
    property HigherBound: Integer read FHigherBound;
    property ArrayLength: Integer read GetLength;
  end;

  {*
    Type tableau dynamique
    @author sjrd
    @version 1.0
  *}
  TSepiDynArrayType = class(TSepiArrayType)
  private
    procedure MakeTypeInfo;
  protected
    procedure ExtractTypeData; override;

    function GetIndexType: TSepiOrdType; override;

    function GetDescription: string; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AElementType: TSepiType);
    constructor Clone(AOwner: TSepiMeta; const AName: string;
      Source: TSepiType); override;

    procedure SetElementType(AElementType: TSepiType); overload;
    procedure SetElementType(const AElementTypeName: string); overload;

    function CompatibleWith(AType: TSepiType): Boolean; override;
  end;

implementation

uses
  SepiSystemUnit;

type
  PArrayTypeData = ^TArrayTypeData;
  TArrayTypeData = packed record
    Size: Cardinal;
    Count: Cardinal;
    ElemType: PPTypeInfo;
    ElemOffset: Cardinal; // always 0
  end;

const
  // Tailles de structure TTypeData en fonction des types
  ArrayTypeDataLength = SizeOf(TArrayTypeData);
  DynArrayTypeDataLengthBase =
    SizeOf(Longint) + 2*SizeOf(Pointer) + SizeOf(Integer);

{----------------------}
{ TSepiArrayType class }
{----------------------}

{*
  Charge un type tableau depuis un flux
*}
constructor TSepiArrayType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FElementType);
end;

{*
  Crée un nouveau type tableau
  @param AOwner        Propriétaire du type
  @param AElemenType   Type des éléments
  @param AIsNative     Indique si le type tableau est natif
  @param ATypeInfo     RTTI du type tableau natif
*}
constructor TSepiArrayType.Create(AOwner: TSepiMeta; const AName: string;
  AKind: TTypeKind; AElementType: TSepiType);
begin
  inherited Create(AOwner, AName, AKind);

  FElementType := AElementType;
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
procedure TSepiArrayType.Save(Stream: TStream);
begin
  inherited;

  OwningUnit.WriteRef(Stream, FElementType);
end;

{*
  [@inheritDoc]
*}
function TSepiArrayType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    (ElementType = TSepiArrayType(Other).ElementType);
end;

{-----------------------------}
{ Classe TSepiStaticArrayType }
{-----------------------------}

{*
  Charge un type tableau statique depuis un flux
*}
constructor TSepiStaticArrayType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FIndexType);
  Stream.ReadBuffer(FLowerBound, 4);
  Stream.ReadBuffer(FHigherBound, 4);

  MakeSize;
  FNeedInit := FElementType.NeedInit;
  FParamBehavior.AlwaysByAddress := Size > 4;
  if FParamBehavior.AlwaysByAddress then
    FResultBehavior := rbParameter;

  if not Native then
    MakeTypeInfo;
end;

{*
  Crée un nouveau type tableau
  @param AOwner         Propriétaire du type
  @param AIndexType     Type de l'index
  @param ALowerBound    Borne inférieure
  @param AHigherBound   Borne supérieure
  @param AElemenType    Type des éléments
  @param AIsNative      Indique si le type tableau est natif
  @param ATypeInfo      RTTI du type tableau natif
*}
constructor TSepiStaticArrayType.Create(AOwner: TSepiMeta; const AName: string;
  AIndexType: TSepiOrdType; ALowerBound, AHigherBound: Integer;
  AElementType: TSepiType; AIsNative: Boolean = False;
  ATypeInfo: PTypeInfo = nil);
begin
  inherited Create(AOwner, AName, tkArray, AElementType);

  FIndexType := AIndexType;
  FLowerBound := ALowerBound;
  FHigherBound := AHigherBound;

  FElementType := AElementType;

  MakeSize;
  FNeedInit := FElementType.NeedInit;
  FParamBehavior.AlwaysByAddress := Size > 4;
  if FParamBehavior.AlwaysByAddress then
    FResultBehavior := rbParameter;

  if AIsNative then
    ForceNative(ATypeInfo);
  if ATypeInfo = nil then
    MakeTypeInfo;
end;

{*
  Crée un nouveau type tableau
  @param AOwner            Propriétaire du type
  @param AIndexTypeInfo    RTTI du type de l'index
  @param ALowerBound       Borne inférieure
  @param AHigherBound      Borne supérieure
  @param AElemenTypeInfo   RTTI du type des éléments
  @param AIsNative         Indique si le type tableau est natif
  @param ATypeInfo         RTTI du type tableau natif
*}
constructor TSepiStaticArrayType.Create(AOwner: TSepiMeta; const AName: string;
  AIndexTypeInfo: PTypeInfo; ALowerBound, AHigherBound: Integer;
  AElementTypeInfo: PTypeInfo; AIsNative: Boolean = False;
  ATypeInfo: PTypeInfo = nil);
begin
  Create(AOwner, AName, AOwner.Root.FindType(AIndexTypeInfo) as TSepiOrdType,
    ALowerBound, AHigherBound, AOwner.Root.FindType(AElementTypeInfo),
    AIsNative, ATypeInfo);
end;

{*
  Crée un nouveau type tableau
  @param AOwner            Propriétaire du type
  @param AIndexTypeName    Nom du type de l'index
  @param ALowerBound       Borne inférieure
  @param AHigherBound      Borne supérieure
  @param AElemenTypeName   Nom du type des éléments
  @param AIsNative         Indique si le type tableau est natif
  @param ATypeInfo         RTTI du type tableau natif
*}
constructor TSepiStaticArrayType.Create(AOwner: TSepiMeta; const AName: string;
  const AIndexTypeName: string; ALowerBound, AHigherBound: Integer;
  const AElementTypeName: string; AIsNative: Boolean = False;
  ATypeInfo: PTypeInfo = nil);
begin
  Create(AOwner, AName, AOwner.Root.FindType(AIndexTypeName) as TSepiOrdType,
    ALowerBound, AHigherBound, AOwner.Root.FindType(AElementTypeName),
    AIsNative, ATypeInfo);
end;

{*
  Crée un nouveau type tableau
  @param AOwner         Propriétaire du type
  @param AName          Nom du type
  @param ADimensions    Dimensions du tableau [Min1, Max1, Min2, Max2, ...]
  @param AElementType   Type des éléments
  @param AIsNative      Indique si le type tableau est natif
  @param ATypeInfo      RTTI du type tableau natif
*}
constructor TSepiStaticArrayType.Create(AOwner: TSepiMeta; const AName: string;
  const ADimensions: array of Integer; AElementType: TSepiType;
  AIsNative: Boolean = False; ATypeInfo: PTypeInfo = nil);
var
  IntegerType: TSepiOrdType;
  DimCount, CurDim: Integer;
begin
  IntegerType := (AOwner.Root.SystemUnit as TSepiSystemUnit).Integer;

  DimCount := Length(ADimensions) div 2;

  if DimCount > 1 then
  begin
    for CurDim := DimCount-1 downto 1 do
    begin
      AElementType := TSepiStaticArrayType.Create(AOwner, '', IntegerType,
        ADimensions[2*CurDim], ADimensions[2*CurDim+1], AElementType,
        AIsNative);
    end;
  end;

  Create(AOwner, AName, IntegerType, ADimensions[0], ADimensions[1],
    AElementType, AIsNative, ATypeInfo);
end;

{$WARN SYMBOL_DEPRECATED OFF}

{*
  Crée un nouveau type tableau
  @param AOwner             Propriétaire du type
  @param AName              Nom du type
  @param ADimensions        Dimensions du tableau [Min1, Max1, Min2, Max2, ...]
  @param AElementTypeInfo   RTTI du type des éléments
  @param AIsNative          Indique si le type tableau est natif
  @param ATypeInfo          RTTI du type tableau natif
*}
constructor TSepiStaticArrayType.Create(AOwner: TSepiMeta; const AName: string;
  const ADimensions: array of Integer; AElementTypeInfo: PTypeInfo;
  AIsNative: Boolean = False; ATypeInfo: PTypeInfo = nil);
begin
  Create(AOwner, AName, ADimensions, AOwner.Root.FindType(AElementTypeInfo),
    AIsNative, ATypeInfo);
end;

{*
  Crée un nouveau type tableau
  @param AOwner             Propriétaire du type
  @param AName              Nom du type
  @param ADimensions        Dimensions du tableau [Min1, Max1, Min2, Max2, ...]
  @param AElementTypeName   Nom du type des éléments
  @param AIsNative          Indique si le type tableau est natif
  @param ATypeInfo          RTTI du type tableau natif
*}
constructor TSepiStaticArrayType.Create(AOwner: TSepiMeta; const AName: string;
  const ADimensions: array of Integer; const AElementTypeName: string;
  AIsNative: Boolean = False; ATypeInfo: PTypeInfo = nil);
begin
  Create(AOwner, AName, ADimensions, AOwner.Root.FindType(AElementTypeName),
    AIsNative, ATypeInfo);
end;

{$WARN SYMBOL_DEPRECATED ON}

{*
  [@inheritDoc]
*}
constructor TSepiStaticArrayType.Clone(AOwner: TSepiMeta; const AName: string;
  Source: TSepiType);
begin
  with Source as TSepiStaticArrayType do
    Create(AOwner, AName, IndexType, LowerBound, HigherBound, ElementType);
end;

{*
  Calcule la taille du tableau et la range dans FSize
*}
procedure TSepiStaticArrayType.MakeSize;
begin
  FSize := ArrayLength * FElementType.Size;
end;

{*
  Construit les RTTI (si besoin)
*}
procedure TSepiStaticArrayType.MakeTypeInfo;
var
  AElementType: TSepiType;
begin
  if not NeedInit then
    Exit;

  AElementType := ElementType;
  while AElementType is TSepiStaticArrayType do
    AElementType := TSepiStaticArrayType(AElementType).ElementType;

  AllocateTypeInfo(ArrayTypeDataLength);
  with PArrayTypeData(TypeData)^ do
  begin
    Size := FSize;
    Count := FSize div AElementType.Size;
    ElemType := TSepiStaticArrayType(AElementType).TypeInfoRef;
    ElemOffset := 0;
  end;
end;

{*
  Longueur du tableau
  @return Longueur du tableau
*}
function TSepiStaticArrayType.GetLength: Integer;
begin
  Result := HigherBound - LowerBound + 1;
end;

{*
  [@inheritDoc]
*}
procedure TSepiStaticArrayType.ListReferences;
begin
  inherited;
  OwningUnit.AddRef(FIndexType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiStaticArrayType.Save(Stream: TStream);
begin
  inherited;

  OwningUnit.WriteRef(Stream, FIndexType);
  Stream.WriteBuffer(FLowerBound, 4);
  Stream.WriteBuffer(FHigherBound, 4);
end;

{*
  [@inheritDoc]
*}
function TSepiStaticArrayType.GetAlignment: Integer;
begin
  Result := ElementType.Alignment;
end;

{*
  [@inheritDoc]
*}
function TSepiStaticArrayType.GetIndexType: TSepiOrdType;
begin
  Result := FIndexType;
end;

{*
  [@inheritDoc]
*}
function TSepiStaticArrayType.GetDescription: string;
var
  InnerType: TSepiType;
  InnerArray: TSepiStaticArrayType;
begin
  Result := 'array[';

  InnerType := Self;
  while InnerType is TSepiStaticArrayType do
  begin
    InnerArray := TSepiStaticArrayType(InnerType);
    if InnerArray <> Self then
      Result := Result + ', ';

    with InnerArray do
    begin
      if (LowerBound = IndexType.MinValue) and
        (HigherBound = IndexType.MaxValue) then
      begin
        Result := Result + IndexType.DisplayName;
      end else
      begin
        if IndexType is TSepiIntegerType then
        begin
          Result := Result + Format('%d..%d', [LowerBound, HigherBound]);
        end else if IndexType is TSepiCharType then
        begin
          Result := Result + Format('#%d..#%d', [LowerBound, HigherBound]);
        end else if IndexType is TSepiEnumType then
        begin
          with TSepiEnumType(IndexType) do
            Result := Result + Format('%s..%s',
              [Names[LowerBound], Names[HigherBound]]);
        end else
        begin
          Result := Result + Format('%s(%d)..%0:s(%2:d)',
            [IndexType.Name, LowerBound, HigherBound]);
        end;
      end;
    end;

    InnerType := InnerArray.ElementType;
  end;

  Result := Result + '] of ' + InnerType.DisplayName;
end;

{*
  [@inheritDoc]
*}
function TSepiStaticArrayType.Equals(Other: TSepiType): Boolean;
begin
  Result := (inherited Equals(Other)) and
    IndexType.Equals(TSepiStaticArrayType(Other).IndexType) and
    (LowerBound = TSepiStaticArrayType(Other).LowerBound) and
    (HigherBound = TSepiStaticArrayType(Other).HigherBound);
end;

{*
  [@inheritDoc]
*}
function TSepiStaticArrayType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := False;
end;

{--------------------------}
{ Classe TSepiDynArrayType }
{--------------------------}

{*
  Recense un type tableau dynamique natif
*}
constructor TSepiDynArrayType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type tableau dynamique depuis un flux
*}
constructor TSepiDynArrayType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  if not Native then
    MakeTypeInfo;
end;

{*
  Crée un nouveau type tableau dynamique
  @param AOwner         Propriétaire du type
  @param AName          Nom du type
  @param AElementType   Type des éléments
*}
constructor TSepiDynArrayType.Create(AOwner: TSepiMeta; const AName: string;
  AElementType: TSepiType);
begin
  inherited Create(AOwner, AName, tkDynArray, AElementType);

  MakeTypeInfo;
end;

{*
  [@inheritDoc]
*}
constructor TSepiDynArrayType.Clone(AOwner: TSepiMeta; const AName: string;
  Source: TSepiType);
begin
  Create(AOwner, AName, (Source as TSepiDynArrayType).ElementType);
end;

{*
  Construit les RTTI du type tableau dynamique
*}
procedure TSepiDynArrayType.MakeTypeInfo;
var
  UnitName: ShortString;
  TypeDataLength: Integer;
begin
  UnitName := OwningUnit.Name;
  TypeDataLength := DynArrayTypeDataLengthBase + Length(UnitName) + 1;
  AllocateTypeInfo(TypeDataLength);

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;

  // Element size
  TypeData.elSize := ElementType.Size;

  // Element RTTI, if need initialization
  // Types which need initialization always have got RTTI
  if ElementType.NeedInit then
    TypeData.elType := TSepiDynArrayType(ElementType).TypeInfoRef
  else
    TypeData.elType := nil;

  // OLE Variant equivalent - always set to -1 at the moment
  { TODO 1 -cMetaunités : OLE Variant dans les RTTI des dyn array }
  TypeData.varType := -1;

  // Element RTTI, independant of cleanup
  // Whe have to check for nul-RTTI, because of records and static arrays
  if Assigned(ElementType.TypeInfo) then
    TypeData.elType2 := TSepiDynArrayType(ElementType).TypeInfoRef
  else
    TypeData.elType2 := nil;

  // Unit name
  Move(UnitName[0], TypeData.DynUnitName[0], Length(UnitName)+1);
end;

{*
  [@inheritDoc]
*}
procedure TSepiDynArrayType.ExtractTypeData;
begin
  inherited;

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;

  if Assigned(TypeData.elType2) then
    FElementType := Root.FindType(TypeData.elType2^);
  // Otherwise, the element type should be set with SetElementType
end;

{*
  [@inheritDoc]
*}
function TSepiDynArrayType.GetIndexType: TSepiOrdType;
begin
  Result := (Root.SystemUnit as TSepiSystemUnit).Integer;
end;

{*
  [@inheritDoc]
*}
function TSepiDynArrayType.GetDescription: string;
begin
  Result := 'array of '+ElementType.DisplayName;
end;

{*
  Renseigne le type des éléments
  Cette méthode ne doit être appelée que pour un tableau dynamique natif, dont
  les éléments n'ont pas de RTTI, et juste après le constructeur
  RegisterTypeInfo.
  @param AElementType   Type des éléments
*}
procedure TSepiDynArrayType.SetElementType(AElementType: TSepiType);
begin
  Assert(Native and (FElementType = nil));
  FElementType := AElementType;
end;

{*
  Renseigne le type des éléments
  Cette méthode ne doit être appelée que pour un tableau dynamique natif, dont
  les éléments n'ont pas de RTTI, et juste après le constructeur
  RegisterTypeInfo.
  @param AElementTypeName   Nom du type des éléments
*}
procedure TSepiDynArrayType.SetElementType(const AElementTypeName: string);
begin
  SetElementType(Root.FindType(AElementTypeName));
end;

{*
  [@inheritDoc]
*}
function TSepiDynArrayType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := False;
end;

initialization
  SepiRegisterMetaClasses([
    TSepiStaticArrayType, TSepiDynArrayType
  ]);
end.

