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
  D�finit les classes de gestion des types tableau
  @author sjrd
  @version 1.0
*}
unit SepiArrayTypes;

interface

uses
  Classes, SysUtils, SysConst, TypInfo, ScUtils, ScTypInfo, SepiReflectionCore,
  SepiOrdTypes;

type
  {*
    Type tableau
    @author sjrd
    @version 1.0
  *}
  TSepiArrayType = class(TSepiType)
  private
    FElementType: TSepiType; /// Type des �l�ments
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    function InternalValueToString(Items: Pointer; Count: Integer): string;

    {*
      Type de l'index
      @return Type de l'index
    *}
    function GetIndexType: TSepiOrdType; virtual; abstract;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
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
    FLowerBound: Integer;     /// Borne inf�rieure
    FHigherBound: Integer;    /// Borne sup�rieure

    procedure MakeSize;
    procedure MakeTypeInfo;

    function GetLength: Integer;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    function GetAlignment: Integer; override;
    function GetIndexType: TSepiOrdType; override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;

    constructor Create(AOwner: TSepiComponent; const AName: string;
      AIndexType: TSepiOrdType; ALowerBound, AHigherBound: Integer;
      AElementType: TSepiType; AIsNative: Boolean = False;
      ATypeInfo: PTypeInfo = nil); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AIndexTypeInfo: PTypeInfo; ALowerBound, AHigherBound: Integer;
      AElementTypeInfo: PTypeInfo; AIsNative: Boolean = False;
      ATypeInfo: PTypeInfo = nil); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      const AIndexTypeName: string; ALowerBound, AHigherBound: Integer;
      const AElementTypeName: string; AIsNative: Boolean = False;
      ATypeInfo: PTypeInfo = nil); overload;

    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;

    function ValueToString(const Value): string; override;

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
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AElementType: TSepiType);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    procedure SetElementType(AElementType: TSepiType); overload;
    procedure SetElementType(const AElementTypeName: string); overload;

    function CompatibleWith(AType: TSepiType): Boolean; override;

    function ValueToString(const Value): string; override;
  end;

  {*
    Type tableau ouvert
    @author sjrd
    @version 1.0
  *}
  TSepiOpenArrayType = class(TSepiArrayType)
  private
    FIsArrayOfConst: Boolean; /// Indique si c'est un array of const
    FHighVarName: string;     /// Nom de la variable reprenant la valeur de High
  protected
    procedure Save(Stream: TStream); override;

    function GetIndexType: TSepiOrdType; override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AElementType: TSepiType; const AHighVarName: string);

    function CompatibleWith(AType: TSepiType): Boolean; override;

    function ValueToString(const Value): string; override;

    property IsArrayOfConst: Boolean read FIsArrayOfConst;
    property HighVarName: string read FHighVarName;
  end;

implementation

uses
  ScCompilerMagic, ScDelphiLanguage, SepiSystemUnit;

const
  // Tailles de structure TTypeData en fonction des types
  ArrayTypeDataLengthBase = 2*SizeOf(Integer) + SizeOf(PPTypeInfo)
    {$IF CompilerVersion >= 21} + SizeOf(Byte) {$IFEND};
  DynArrayTypeDataLengthBase =
    SizeOf(Longint) + 2*SizeOf(Pointer) + SizeOf(Integer);

{----------------------}
{ TSepiArrayType class }
{----------------------}

{*
  Charge un type tableau depuis un flux
*}
constructor TSepiArrayType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FElementType);
end;

{*
  Cr�e un nouveau type tableau
  @param AOwner        Propri�taire du type
  @param AElemenType   Type des �l�ments
  @param AIsNative     Indique si le type tableau est natif
  @param ATypeInfo     RTTI du type tableau natif
*}
constructor TSepiArrayType.Create(AOwner: TSepiComponent; const AName: string;
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
procedure TSepiArrayType.WriteDigestData(Stream: TStream);
begin
  inherited;

  ElementType.WriteDigestToStream(Stream);
end;

{*
  [@inheritDoc]
*}
function TSepiArrayType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    (ElementType = TSepiArrayType(Other).ElementType);
end;

{*
  Repr�sentation sous forme de cha�ne d'un tableau de ce type
  @param Items   Pointeur sur le premier �l�ment du tableau
  @param Count   Nombre d'�l�ments
  @return Repr�sentation sous forme de cha�ne du tableau
*}
function TSepiArrayType.InternalValueToString(Items: Pointer;
  Count: Integer): string;
var
  I: Integer;
  Item: Pointer;
begin
  if Count = 0 then
  begin
    Result := '()';
    Exit;
  end;

  Result := '';
  Item := Items;

  for I := 0 to Count-1 do
  begin
    Result := Result + ' ' + ElementType.ValueToString(Item^) + ',';
    Inc(Cardinal(Item), ElementType.Size);
  end;

  Result[1] := '(';
  Result[Length(Result)] := ')';
end;

{-----------------------------}
{ Classe TSepiStaticArrayType }
{-----------------------------}

{*
  Charge un type tableau statique depuis un flux
*}
constructor TSepiStaticArrayType.Load(AOwner: TSepiComponent; Stream: TStream);
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
  Cr�e un nouveau type tableau
  @param AOwner         Propri�taire du type
  @param AIndexType     Type de l'index
  @param ALowerBound    Borne inf�rieure
  @param AHigherBound   Borne sup�rieure
  @param AElemenType    Type des �l�ments
  @param AIsNative      Indique si le type tableau est natif
  @param ATypeInfo      RTTI du type tableau natif
*}
constructor TSepiStaticArrayType.Create(AOwner: TSepiComponent;
  const AName: string; AIndexType: TSepiOrdType;
  ALowerBound, AHigherBound: Integer; AElementType: TSepiType;
  AIsNative: Boolean = False; ATypeInfo: PTypeInfo = nil);
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
  Cr�e un nouveau type tableau
  @param AOwner            Propri�taire du type
  @param AIndexTypeInfo    RTTI du type de l'index
  @param ALowerBound       Borne inf�rieure
  @param AHigherBound      Borne sup�rieure
  @param AElemenTypeInfo   RTTI du type des �l�ments
  @param AIsNative         Indique si le type tableau est natif
  @param ATypeInfo         RTTI du type tableau natif
*}
constructor TSepiStaticArrayType.Create(AOwner: TSepiComponent;
  const AName: string; AIndexTypeInfo: PTypeInfo;
  ALowerBound, AHigherBound: Integer; AElementTypeInfo: PTypeInfo;
  AIsNative: Boolean = False; ATypeInfo: PTypeInfo = nil);
begin
  Create(AOwner, AName, AOwner.Root.FindType(AIndexTypeInfo) as TSepiOrdType,
    ALowerBound, AHigherBound, AOwner.Root.FindType(AElementTypeInfo),
    AIsNative, ATypeInfo);
end;

{*
  Cr�e un nouveau type tableau
  @param AOwner            Propri�taire du type
  @param AIndexTypeName    Nom du type de l'index
  @param ALowerBound       Borne inf�rieure
  @param AHigherBound      Borne sup�rieure
  @param AElemenTypeName   Nom du type des �l�ments
  @param AIsNative         Indique si le type tableau est natif
  @param ATypeInfo         RTTI du type tableau natif
*}
constructor TSepiStaticArrayType.Create(AOwner: TSepiComponent;
  const AName: string; const AIndexTypeName: string;
  ALowerBound, AHigherBound: Integer; const AElementTypeName: string;
  AIsNative: Boolean = False; ATypeInfo: PTypeInfo = nil);
begin
  Create(AOwner, AName, AOwner.Root.FindType(AIndexTypeName) as TSepiOrdType,
    ALowerBound, AHigherBound, AOwner.Root.FindType(AElementTypeName),
    AIsNative, ATypeInfo);
end;

{*
  [@inheritDoc]
*}
constructor TSepiStaticArrayType.Clone(AOwner: TSepiComponent;
  const AName: string; Source: TSepiType);
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
  DimCount: Integer;
  TypeDataLength: Integer;
  ArrayTypeData: PArrayTypeData;
{$IF CompilerVersion >= 21}
  I: Integer;
{$IFEND}
begin
{$IF CompilerVersion < 21}
  if not NeedInit then
    Exit;
{$IFEND}

  AElementType := ElementType;
  DimCount := 1;

  while AElementType is TSepiStaticArrayType do
  begin
    AElementType := TSepiStaticArrayType(AElementType).ElementType;
    Inc(DimCount);
  end;

  TypeDataLength := ArrayTypeDataLengthBase
    {$IF CompilerVersion >= 21} + DimCount * SizeOf(PPTypeInfo) {$IFEND};

  AllocateTypeInfo(TypeDataLength);
  ArrayTypeData := PArrayTypeData(TypeData);

  ArrayTypeData.Size := FSize;
  ArrayTypeData.ElCount := FSize div AElementType.Size;
  ArrayTypeData.ElType := TSepiStaticArrayType(AElementType).TypeInfoRef;

{$IF CompilerVersion >= 21}
  ArrayTypeData.DimCount := DimCount;

  AElementType := Self;
  for I := 0 to DimCount-1 do
  begin
    AElementType := TSepiStaticArrayType(AElementType).ElementType;
    ArrayTypeData.Dims[I] := TSepiStaticArrayType(AElementType).TypeInfoRef;
  end;
{$IFEND}
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
procedure TSepiStaticArrayType.WriteDigestData(Stream: TStream);
begin
  inherited;

  IndexType.WriteDigestToStream(Stream);
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

{*
  [@inheritDoc]
*}
function TSepiStaticArrayType.ValueToString(const Value): string;
begin
  Result := InternalValueToString(@Value, ArrayLength);
end;

{--------------------------}
{ Classe TSepiDynArrayType }
{--------------------------}

{*
  Recense un type tableau dynamique natif
*}
constructor TSepiDynArrayType.RegisterTypeInfo(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo);
begin
  inherited;

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;

  ExtractTypeData;
end;

{*
  Charge un type tableau dynamique depuis un flux
*}
constructor TSepiDynArrayType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;

  if not Native then
    MakeTypeInfo;
end;

{*
  Cr�e un nouveau type tableau dynamique
  @param AOwner         Propri�taire du type
  @param AName          Nom du type
  @param AElementType   Type des �l�ments
*}
constructor TSepiDynArrayType.Create(AOwner: TSepiComponent; const AName: string;
  AElementType: TSepiType);
begin
  inherited Create(AOwner, AName, tkDynArray, AElementType);

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;

  MakeTypeInfo;
end;

{*
  [@inheritDoc]
*}
constructor TSepiDynArrayType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  Create(AOwner, AName, (Source as TSepiDynArrayType).ElementType);
end;

{*
  Construit les RTTI du type tableau dynamique
*}
procedure TSepiDynArrayType.MakeTypeInfo;
var
  UnitName: TypeInfoString;
  TypeDataLength: Integer;
{$IF CompilerVersion >= 21}
  DynArrElTypePtr: ^PPTypeInfo;
{$IFEND}
begin
  UnitName := TypeInfoEncode(OwningUnit.Name);
  TypeDataLength := DynArrayTypeDataLengthBase + Length(UnitName) + 1
    {$IF CompilerVersion >= 21} + SizeOf(PPTypeInfo) {$IFEND};
  AllocateTypeInfo(TypeDataLength);

  // Element size
  TypeData.elSize := ElementType.Size;

  // Element RTTI, if need initialization
  // Types which need initialization always have got RTTI
  if ElementType.NeedInit then
    TypeData.elType := TSepiDynArrayType(ElementType).TypeInfoRef
  else
    TypeData.elType := nil;

  // OLE Variant equivalent - always set to -1 at the moment
  { TODO 1 : OLE Variant dans les RTTI des dyn array }
  TypeData.varType := -1;

  // Element RTTI, independant of cleanup
  // Whe have to check for nul-RTTI, because of records and static arrays
  if Assigned(ElementType.TypeInfo) then
    TypeData.elType2 := TSepiDynArrayType(ElementType).TypeInfoRef
  else
    TypeData.elType2 := nil;

  // Unit name
  Byte(TypeData.DynUnitName[0]) := Length(UnitName);
  Move(UnitName[1], TypeData.DynUnitName[1], Length(UnitName));

  {$IF CompilerVersion >= 21}
  // DynArrElType
  DynArrElTypePtr := SkipPackedShortString(@TypeData.DynUnitName);
  DynArrElTypePtr^ := TypeData.elType2;
  { TODO 1 : Understand difference between elType2 and DynArrElType }
  {$IFEND}
end;

{*
  [@inheritDoc]
*}
procedure TSepiDynArrayType.ExtractTypeData;
begin
  inherited;

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
  Renseigne le type des �l�ments
  Cette m�thode ne doit �tre appel�e que pour un tableau dynamique natif, dont
  les �l�ments n'ont pas de RTTI, et juste apr�s le constructeur
  RegisterTypeInfo.
  @param AElementType   Type des �l�ments
*}
procedure TSepiDynArrayType.SetElementType(AElementType: TSepiType);
begin
  Assert(Native and (FElementType = nil));
  FElementType := AElementType;
end;

{*
  Renseigne le type des �l�ments
  Cette m�thode ne doit �tre appel�e que pour un tableau dynamique natif, dont
  les �l�ments n'ont pas de RTTI, et juste apr�s le constructeur
  RegisterTypeInfo.
  @param AElementTypeName   Nom du type des �l�ments
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

{*
  [@inheritDoc]
*}
function TSepiDynArrayType.ValueToString(const Value): string;
begin
  Result := InternalValueToString(Pointer(Value), DynArrayLength(Value));
end;

{--------------------------}
{ TSepiOpenArrayType class }
{--------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiOpenArrayType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FIsArrayOfConst, SizeOf(Boolean));
  FHighVarName := ReadStrFromStream(Stream);

  FParamBehavior.AlwaysByAddress := True;
  FResultBehavior := rbNone;
end;

{*
  Cr�e une instance TSepiOpenArrayType
  @param AOwner         Propri�taire
  @param AName          Nom du type
  @param AElementType   Type d'�l�ment
  @param AHighVarName   Nom de la variable High
*}
constructor TSepiOpenArrayType.Create(AOwner: TSepiComponent;
  const AName: string; AElementType: TSepiType; const AHighVarName: string);
begin
  FIsArrayOfConst := (AElementType = nil) or (AElementType is TSepiUntypedType);
  if FIsArrayOfConst then
    AElementType := (AOwner.Root.SystemUnit as TSepiSystemUnit).TVarRec;

  inherited Create(AOwner, AName, tkUnknown, AElementType);

  FHighVarName := AHighVarName;

  FParamBehavior.AlwaysByAddress := True;
  FResultBehavior := rbNone;
end;

{*
  [@inheritDoc]
*}
procedure TSepiOpenArrayType.Save(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FIsArrayOfConst, SizeOf(Boolean));
  WriteStrToStream(Stream, FHighVarName);
end;

{*
  [@inheritDoc]
*}
function TSepiOpenArrayType.GetIndexType: TSepiOrdType;
begin
  Result := (Root.SystemUnit as TSepiSystemUnit).Integer;
end;

{*
  [@inheritDoc]
*}
function TSepiOpenArrayType.GetDescription: string;
begin
  if IsArrayOfConst then
    Result := 'array of const'
  else
    Result := 'array of '+ElementType.DisplayName;
end;

{*
  [@inheritDoc]
*}
function TSepiOpenArrayType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := (AType is TSepiArrayType) and
    TSepiArrayType(AType).ElementType.Equals(ElementType);
end;

{*
  [@inheritDoc]
*}
function TSepiOpenArrayType.ValueToString(const Value): string;
begin
  Result := Description;
end;

initialization
  SepiRegisterComponentClasses([
    TSepiStaticArrayType, TSepiDynArrayType, TSepiOpenArrayType
  ]);
end.

