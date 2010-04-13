{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  Sébastien Doeraene
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
  Définit les classes de gestion des types ordinaux
  @author sjrd
  @version 1.0
*}
unit SepiOrdTypes;

interface

uses
  Classes, SysUtils, SysConst, TypInfo, ScUtils, ScDelphiLanguage,
  SepiReflectionCore;

const
  MinInt64 = Int64($FFFFFFFFFFFFFFFF);
  MaxInt64 = Int64($7FFFFFFFFFFFFFFF);

  otUnknown = TOrdType(-1);

type
  {*
    Type de booléen
  *}
  TBooleanKind = (bkBoolean, bkByteBool, bkWordBool, bkLongBool);

  {*
    Taille minimale d'un type énuméré
  *}
  TSepiMinEnumSize = (mesByte, mesWord, mesLongWord);

  {*
    Type ordinal
    @author sjrd
    @version 1.0
  *}
  TSepiOrdType = class(TSepiType)
  private
    FMinValue: Longint; /// Valeur minimale
    FMaxValue: Longint; /// Valeur maximale
  protected
    procedure WriteDigestData(Stream: TStream); override;

    procedure ExtractTypeData; override;
  public
    function Equals(Other: TSepiType): Boolean; override;

    function ValueAsInteger(const Value): Integer;
    function ValueAsCardinal(const Value): Cardinal;

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

    function GetDescription: string; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AMinValue: Longint = -MaxInt-1; AMaxValue: Longint = MaxInt);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

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

    procedure WriteDigestData(Stream: TStream); override;

    procedure ExtractTypeData; override;

    function GetDescription: string; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AMinValue: WideChar = #0; AMaxValue: WideChar = #255;
      AForceUnicode: Boolean = False);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

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

    procedure WriteDigestData(Stream: TStream); override;

    procedure ExtractTypeData; override;

    function GetDescription: string; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AMinValue: Int64 = MinInt64; AMaxValue: Int64 = MaxInt64);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

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

    procedure WriteDigestData(Stream: TStream); override;

    procedure ExtractTypeData; override;

    function GetAlignment: Integer; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AFloatType: TFloatType = ftDouble);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;

    function ValueAsExtended(const Value): Extended;
    procedure SetValueAsExtended(var Dest; Source: Extended);

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

    procedure MakeTypeInfo;
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

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

    procedure WriteDigestData(Stream: TStream); override;

    procedure ExtractTypeData; override;

    function GetDescription: string; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      const AValues: array of string;
      AMinEnumSize: TSepiMinEnumSize = mesByte); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      ABaseType: TSepiEnumType; AMinValue, AMaxValue: Integer); overload;
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;
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

    procedure WriteDigestData(Stream: TStream); override;

    procedure ExtractTypeData; override;

    function GetAlignment: Integer; override;

    function GetDescription: string; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiComponent;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      ACompType: TSepiOrdType); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      ACompType: PTypeInfo); overload;
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;

    property CompType: TSepiOrdType read FCompType;
  end;

  {*
    Information sur un pointeur déclaré en forward
    @author sjrd
    @version 1.0
  *}
  TSepiForwardPointerInfo = record
    Owner: TSepiComponent;
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
    FIsUntyped: Boolean; /// Indique si c'est un pointeur non typé

    FForwardInfo: PSepiForwardPointerInfo; /// Infos conservées en forward
  protected
    procedure Loaded; override;

    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      APointTo: TSepiType; AIsNative: Boolean = False); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      APointTo: PTypeInfo; AIsNative: Boolean = False); overload;
    constructor Create(AOwner: TSepiComponent; const AName, APointTo: string;
      AIsNative: Boolean = False); overload;
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    class function NewInstance: TObject; override;

    function Equals(Other: TSepiType): Boolean; override;

    property PointTo: TSepiType read FPointTo;
    property IsUntyped: Boolean read FIsUntyped;
  end;

implementation

uses
  SepiSystemUnit;

const
  /// Chaînes 'False' et 'True' en packed ShortString
  PackedShortStrFalseTrue: array[0..10] of Char = (
    #5, 'F', 'a', 'l', 's', 'e', #4, 'T', 'r', 'u', 'e' {don't localize}
  );

  // Tailles de structure TTypeData en fonction des types
  IntegerTypeDataLength = SizeOf(TOrdType) + 2*SizeOf(Longint);
  CharTypeDataLength = IntegerTypeDataLength;
  Int64TypeDataLength = 2*SizeOf(Int64);
  FloatTypeDataLength = SizeOf(TFloatType);
  EnumTypeDataLength = SizeOf(TOrdType) + 2*SizeOf(Longint) + SizeOf(Pointer);
  BooleanTypeDataLength = EnumTypeDataLength + SizeOf(PackedShortStrFalseTrue);
  SetTypeDataLength = SizeOf(TOrdType) + SizeOf(Pointer);

{---------------------}
{ Classe TSepiOrdType }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TSepiOrdType.WriteDigestData(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FMinValue, 4);
  Stream.WriteBuffer(FMaxValue, 4);
end;

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

{*
  [@inheritDoc]
*}
function TSepiOrdType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    (TypeData.OrdType = Other.TypeData.OrdType);
end;

{*
  Lit une valeur de ce type comme un Integer
  @param Value   Valeur de ce type
  @return Value comme Integer
*}
function TSepiOrdType.ValueAsInteger(const Value): Integer;
begin
  case TypeData.OrdType of
    otSByte: Result := Shortint(Value);
    otUByte: Result := Byte(Value);
    otSWord: Result := Smallint(Value);
    otUWord: Result := Word(Value);
  else
    Result := Longint(Value);
  end;
end;

{*
  Lit une valeur de ce type comme un Cardinal
  @param Value   Valeur de ce type
  @return Value comme Cardinal
*}
function TSepiOrdType.ValueAsCardinal(const Value): Cardinal;
begin
  case TypeData.OrdType of
    otSByte: Result := Shortint(Value);
    otUByte: Result := Byte(Value);
    otSWord: Result := Smallint(Value);
    otUWord: Result := Word(Value);
  else
    Result := LongWord(Value);
  end;
end;

{-------------------------}
{ Classe TSepiIntegerType }
{-------------------------}

{*
  Recense un type entier natif
*}
constructor TSepiIntegerType.RegisterTypeInfo(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type entier depuis un flux
*}
constructor TSepiIntegerType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  if not Native then
  begin
    AllocateTypeInfo(IntegerTypeDataLength);
    Stream.ReadBuffer(TypeData^, IntegerTypeDataLength);
  end else
    Stream.Seek(IntegerTypeDataLength, soFromCurrent);

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
constructor TSepiIntegerType.Create(AOwner: TSepiComponent; const AName: string;
  AMinValue: Longint = -MaxInt-1; AMaxValue: Longint = MaxInt);
begin
  inherited Create(AOwner, AName, tkInteger);

  AllocateTypeInfo(IntegerTypeDataLength);

  if AMinValue < 0 then
  begin
    // Signed
    if (AMinValue < -32768) or (AMaxValue >= 32768) then
      TypeData.OrdType := otSLong
    else if (AMinValue < -128) or (AMaxValue >= 128) then
      TypeData.OrdType := otSWord
    else
      TypeData.OrdType := otSByte;
  end else
  begin
    // Unsigned
    if Cardinal(AMaxValue) >= 65536 then
      TypeData.OrdType := otULong
    else if AMaxValue >= 256 then
      TypeData.OrdType := otUWord
    else
      TypeData.OrdType := otUByte;
  end;

  TypeData.MinValue := AMinValue;
  TypeData.MaxValue := AMaxValue;

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
constructor TSepiIntegerType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  with Source as TSepiIntegerType do
    Self.Create(AOwner, AName, MinValue, MaxValue);
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
  [@inheritDoc]
*}
function TSepiIntegerType.GetDescription: string;
begin
  Result := IntToStr(MinValue)+'..'+IntToStr(MaxValue);
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
constructor TSepiCharType.RegisterTypeInfo(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type caractère depuis un flux
*}
constructor TSepiCharType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  if not Native then
  begin
    AllocateTypeInfo(CharTypeDataLength);
    Stream.ReadBuffer(TypeData^, CharTypeDataLength);
  end else
    Stream.Seek(CharTypeDataLength, soFromCurrent);

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
constructor TSepiCharType.Create(AOwner: TSepiComponent; const AName: string;
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
constructor TSepiCharType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  with Source as TSepiCharType do
    Self.Create(AOwner, AName, ChrMinValue, ChrMaxValue, IsUnicode);
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
  [@inheritDoc]
*}
procedure TSepiCharType.WriteDigestData(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FIsUnicode, 1);
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
  [@inheritDoc]
*}
function TSepiCharType.GetDescription: string;
begin
  if ChrMinValue < #$100 then
    Result := CharToCharRepres(AnsiChar(ChrMinValue)) + '..'
  else
    Result := '#'+IntToStr(MinValue) + '..';

  if ChrMaxValue < #$100 then
    Result := Result + CharToCharRepres(AnsiChar(ChrMaxValue))
  else
    Result := Result + '#'+IntToStr(MaxValue);
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
constructor TSepiInt64Type.RegisterTypeInfo(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type entier 64 bits depuis un flux
*}
constructor TSepiInt64Type.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  if not Native then
  begin
    AllocateTypeInfo(Int64TypeDataLength);
    Stream.ReadBuffer(TypeData^, Int64TypeDataLength);
  end else
    Stream.Seek(Int64TypeDataLength, soFromCurrent);

  ExtractTypeData;
end;

{*
  Crée un nouveau type entier 64 bits
  @param AOwner      Propriétaire du type
  @param AName       Nom du type
  @param AMinValue   Valeur minimale
  @param AMaxValue   Valeur maximale
*}
constructor TSepiInt64Type.Create(AOwner: TSepiComponent; const AName: string;
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
constructor TSepiInt64Type.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  with Source as TSepiInt64Type do
    Self.Create(AOwner, AName, MinValue, MaxValue);
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
  [@inheritDoc]
*}
procedure TSepiInt64Type.WriteDigestData(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FMinValue, 8);
  Stream.WriteBuffer(FMaxValue, 8);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiInt64Type.ExtractTypeData;
begin
  inherited;

  FSize := 8;
  FParamBehavior.AlwaysByStack := True;
  FResultBehavior := rbInt64;
  FMinValue := TypeData.MinInt64Value;
  FMaxValue := TypeData.MaxInt64Value;

  FNeedRangeCheck := (FMinValue <> MinInt64) or (FMaxValue <> MaxInt64);
end;

{*
  [@inheritDoc]
*}
function TSepiInt64Type.GetDescription: string;
begin
  Result := IntToStr(MinInt64)+'..'+IntToStr(MaxInt64);
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
constructor TSepiFloatType.RegisterTypeInfo(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type flottant depuis un flux
*}
constructor TSepiFloatType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  if not Native then
  begin
    AllocateTypeInfo(FloatTypeDataLength);
    Stream.ReadBuffer(TypeData^, FloatTypeDataLength);
  end else
    Stream.Seek(FloatTypeDataLength, soFromCurrent);

  ExtractTypeData;
end;

{*
  Crée un nouveau type flottant
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AFloatType   Type de flottant
*}
constructor TSepiFloatType.Create(AOwner: TSepiComponent; const AName: string;
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
constructor TSepiFloatType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  Create(AOwner, AName, (Source as TSepiFloatType).FloatType);
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
  [@inheritDoc]
*}
procedure TSepiFloatType.WriteDigestData(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FFloatType, 1);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiFloatType.ExtractTypeData;
const
  Sizes: array[TFloatType] of Integer = (4, 8, 10, 8, 8);
  ResultBehaviors: array[TFloatType] of TSepiTypeResultBehavior = (
    rbSingle, rbDouble, rbExtended, rbCurrency, rbCurrency
  );
begin
  inherited;

  FFloatType := TypeData.FloatType;

  FSize := Sizes[FFloatType];
  FParamBehavior.AlwaysByStack := True;
  FResultBehavior := ResultBehaviors[FFloatType];
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

{*
  [@inheritDoc]
*}
function TSepiFloatType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    (FloatType = TSepiFloatType(Other).FloatType);
end;

{*
  Lit une valeur de ce type comme valeur Extended
  @param Value   Valeur à lire
  @return Valeur comme Extended
*}
function TSepiFloatType.ValueAsExtended(const Value): Extended;
begin
  case FloatType of
    ftSingle: Result := Single  (Value);
    ftDouble: Result := Double  (Value);
    ftCurr:   Result := Currency(Value);
    ftComp:   Result := Comp    (Value);
  else
    Result := Extended(Value);
  end;
end;

{*
  Enregistre une valeur de ce type comme Extended
  @param Dest     Valeur destination
  @param Source   Source
*}
procedure TSepiFloatType.SetValueAsExtended(var Dest; Source: Extended);
begin
  case FloatType of
    ftSingle: Single  (Dest) := Source;
    ftDouble: Double  (Dest) := Source;
    ftCurr:   Currency(Dest) := Source;
    ftComp:   Comp    (Dest) := Source;
  else
    Extended(Dest) := Source;
  end;
end;

{-------------------------}
{ Classe TSepiBooleanType }
{-------------------------}

{*
  Recense un type booléen natif
*}
constructor TSepiBooleanType.RegisterTypeInfo(AOwner: TSepiComponent;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type booléen depuis un flux
*}
constructor TSepiBooleanType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FBooleanKind, SizeOf(TBooleanKind));
  if not Native then
    MakeTypeInfo;

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
constructor TSepiBooleanType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  inherited Create(AOwner, AName, tkEnumeration);

  FBooleanKind := (Source as TSepiBooleanType).BooleanKind;
  MakeTypeInfo;

  ExtractTypeData;
end;

{*
  Construit les RTTI
*}
procedure TSepiBooleanType.MakeTypeInfo;
const
  KindToOrdType: array[TBooleanKind] of TOrdType =
    (otUByte, otSByte, otSWord, otSLong);
var
  BaseTypeInfo: PTypeInfo;
begin
  AllocateTypeInfo(BooleanTypeDataLength);

  with TypeData^ do
  begin
    // OrdType
    OrdType := KindToOrdType[BooleanKind];

    // MinValue and MaxValue
    if BooleanKind = bkBoolean then
    begin
      MinValue := 0;
      MaxValue := 1;
    end else
    begin
      MinValue := -MaxInt-1;
      MaxValue := MaxInt;
    end;

    // BaseType
    case BooleanKind of
      bkByteBool: BaseTypeInfo := System.TypeInfo(ByteBool);
      bkWordBool: BaseTypeInfo := System.TypeInfo(WordBool);
      bkLongBool: BaseTypeInfo := System.TypeInfo(LongBool);
    else
      BaseTypeInfo := System.TypeInfo(Boolean);
    end;

    BaseType := GetTypeData(BaseTypeInfo).BaseType;

    // NameList
    Move(PackedShortStrFalseTrue, NameList, SizeOf(PackedShortStrFalseTrue));
  end;
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
  else
    FBooleanKind := bkBoolean;
  end;
end;

{----------------------}
{ Classe TSepiEnumType }
{----------------------}

{*
  Recense un type énuméré natif
*}
constructor TSepiEnumType.RegisterTypeInfo(AOwner: TSepiComponent;
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
constructor TSepiEnumType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(TypeDataLength, 4);
  if not Native then
  begin
    AllocateTypeInfo(TypeDataLength);
    Stream.ReadBuffer(TypeData^, TypeDataLength);
  end else
    Stream.Seek(TypeDataLength, soFromCurrent);

  OwningUnit.ReadRef(Stream, FBaseType);
  if not Native then
    TypeData.BaseType := FBaseType.TypeInfoRef;

  ExtractTypeData;
end;

{*
  Crée un nouveau type énuméré
  @param AOwner         Propriétaire du type
  @param AName          Nom du type
  @param AValues        Noms des éléments de l'énumération
  @param AMinimumSize   Taille minimale (pour compatibilité avec C/C++)
*}
constructor TSepiEnumType.Create(AOwner: TSepiComponent; const AName: string;
  const AValues: array of string; AMinEnumSize: TSepiMinEnumSize = mesByte);
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
  with TypeData^ do
  begin
    case AMinEnumSize of
      mesWord: OrdType := otUWord;
      mesLongWord: OrdType := otULong;
    else
      if ValueCount > 256 then
        OrdType := otUWord
      else
        OrdType := otUByte;
    end;

    MinValue := 0;
    MaxValue := ValueCount-1;
    BaseType := TypeInfoRef;
  end;

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
  Crée un nouveau type énuméré basé sur un type existant
  @param AOwner      Propriétaire du type
  @param AName       Nom du type
  @param ABaseType   Type énuméré de base
  @param AMinValue   Valeur minimale
  @param AMaxValue   Valeur maximale
*}
constructor TSepiEnumType.Create(AOwner: TSepiComponent; const AName: string;
  ABaseType: TSepiEnumType; AMinValue, AMaxValue: Integer);
var
  OwningUnitName: ShortString;
begin
  inherited Create(AOwner, AName, tkEnumeration);

  ABaseType := ABaseType.BaseType;
  OwningUnitName := OwningUnit.Name;

  // Allocate type info
  TypeDataLength := EnumTypeDataLength + 1 + Length(OwningUnitName);
  AllocateTypeInfo(TypeDataLength);

  // Copy source type data and set MinValue and MaxValue
  Move(ABaseType.TypeData^, TypeData^, EnumTypeDataLength);
  TypeData.MinValue := AMinValue;
  TypeData.MaxValue := AMaxValue;

  // Write unit name
  Move(OwningUnitName[0], TypeData.NameList[0], Length(OwningUnitName)+1);

  // Set fields
  inherited ExtractTypeData;
  FBaseType := ABaseType;
  FValueCount := ABaseType.ValueCount;
end;

{*
  [@inheritDoc]
*}
constructor TSepiEnumType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  with Source as TSepiEnumType do
    Self.Create(AOwner, AName, BaseType, MinValue, MaxValue);
end;

{*
  Crée les constantes énumérées
*}
procedure TSepiEnumType.CreateConstants;
var
  Value: Integer;
begin
  if BaseType <> Self then
    Exit;
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
  [@inheritDoc]
*}
procedure TSepiEnumType.WriteDigestData(Stream: TStream);
var
  I: Integer;
begin
  inherited;

  if BaseType = Self then
  begin
    for I := 0 to ValueCount-1 do
      WriteStrToStream(Stream, Names[I]);
  end else
  begin
    BaseType.WriteDigestToStream(Stream);
  end;
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
function TSepiEnumType.GetDescription: string;
var
  I: Integer;
begin
  if BaseType = Self then
  begin
    for I := 0 to ValueCount-1 do
      Result := Result + ' ' + Names[I] + ',';
    Result[1] := '(';
    Result[Length(Result)] := ')';
  end else
  begin
    Result := Names[MinValue]+'..'+Names[MaxValue];
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiEnumType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    (BaseType = TSepiEnumType(Other).BaseType);
end;

{*
  [@inheritDoc]
*}
function TSepiEnumType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := Equals(AType);
end;

{---------------------}
{ Classe TSepiSetType }
{---------------------}

{*
  Recense un type ensemble natif
*}
constructor TSepiSetType.RegisterTypeInfo(AOwner: TSepiComponent;
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
constructor TSepiSetType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  if not Native then
  begin
    AllocateTypeInfo(SetTypeDataLength);
    Stream.ReadBuffer(TypeData^, SetTypeDataLength);
  end else
    Stream.Seek(SetTypeDataLength, soFromCurrent);

  OwningUnit.ReadRef(Stream, FCompType);
  if not Native then
    TypeData.CompType := FCompType.TypeInfoRef;

  ExtractTypeData;
end;

{*
  Crée un nouveau type ensemble
  @param AOwner      Propriétaire du type
  @param AName       Nom du type
  @param ACompType   Type des éléments de l'ensemble
*}
constructor TSepiSetType.Create(AOwner: TSepiComponent; const AName: string;
  ACompType: TSepiOrdType);
begin
  inherited Create(AOwner, AName, tkSet);

  FCompType := ACompType;

  AllocateTypeInfo(SetTypeDataLength);

  case CompType.MaxValue - CompType.MinValue + 1 of
    1..8: TypeData.OrdType := otUByte;
    9..16: TypeData.OrdType := otUWord;
    17..32: TypeData.OrdType := otULong;
  else
    TypeData.OrdType := otUnknown;
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
constructor TSepiSetType.Create(AOwner: TSepiComponent; const AName: string;
  ACompType: PTypeInfo);
begin
  Create(AOwner, AName,
    AOwner.Root.FindType(ACompType) as TSepiOrdType);
end;

{*
  [@inheritDoc]
*}
constructor TSepiSetType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  Create(AOwner, AName, (Source as TSepiSetType).CompType);
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
  [@inheritDoc]
*}
procedure TSepiSetType.WriteDigestData(Stream: TStream);
begin
  inherited;

  CompType.WriteDigestToStream(Stream);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiSetType.ExtractTypeData;
begin
  inherited;

  FSize := (FCompType.MaxValue - FCompType.MinValue) div 8 + 1;
  if FSize = 3 then
    FSize := 4;

  FParamBehavior.AlwaysByAddress := Size > 4;
  if FParamBehavior.AlwaysByAddress then
    FResultBehavior := rbParameter;
end;

{*
  [@inheritDoc]
*}
function TSepiSetType.GetAlignment: Integer;
begin
  if Size <= 4 then
    Result := Size
  else
    Result := 8;
end;

{*
  [@inheritDoc]
*}
function TSepiSetType.GetDescription: string;
begin
  Result := 'set of '+CompType.DisplayName;
end;

{*
  [@inheritDoc]
*}
function TSepiSetType.Equals(Other: TSepiType): Boolean;
var
  OtherCompType: TSepiOrdType;
begin
  if ClassType = Other.ClassType then
  begin
    OtherCompType := TSepiSetType(Other).CompType;

    Result := CompType.Equals(OtherCompType) and
      (CompType.MinValue = OtherCompType.MinValue) and
      (CompType.MaxValue = OtherCompType.MaxValue);
  end else
  begin
    Result := False;
  end;
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
constructor TSepiPointerType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  FSize := 4;
  OwningUnit.ReadRef(Stream, FPointTo);
  FIsUntyped := FPointTo is TSepiUntypedType;
end;

{*
  Crée un nouveau type pointeur
  @param AOwner     Propriétaire du type
  @param AName      Nom du type
  @param APointTo   Type vers lequel pointe le pointeur
*}
constructor TSepiPointerType.Create(AOwner: TSepiComponent; const AName: string;
  APointTo: TSepiType; AIsNative: Boolean = False);
begin
  if APointTo = nil then
    APointTo := (AOwner.Root.SystemUnit as TSepiSystemUnit).Untyped;

  inherited Create(AOwner, AName, tkUnknown);

  FSize := 4;
  FPointTo := APointTo;
  FIsUntyped := APointTo is TSepiUntypedType;

  if AIsNative then
    ForceNative;
end;

{*
  Crée un nouveau type pointeur
  @param AOwner     Propriétaire du type
  @param AName      Nom du type
  @param APointTo   RTTI tu type vers lequel pointe le pointeur
*}
constructor TSepiPointerType.Create(AOwner: TSepiComponent; const AName: string;
  APointTo: PTypeInfo; AIsNative: Boolean = False);
var
  APointToType: TSepiType;
begin
  if FForwardInfo = nil then
    APointToType := AOwner.Root.GetType(APointTo)
  else
    APointToType := AOwner.Root.FindType(APointTo);

  if (APointTo = nil) or (APointToType <> nil) then
    Create(AOwner, AName, APointToType, AIsNative)
  else
  begin
    FPointTo := (AOwner.Root.SystemUnit as TSepiSystemUnit).Untyped;
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
constructor TSepiPointerType.Create(AOwner: TSepiComponent;
  const AName, APointTo: string; AIsNative: Boolean = False);
var
  APointToType: TSepiType;
begin
  if FForwardInfo = nil then
    APointToType := AOwner.Root.GetType(APointTo)
  else
    APointToType := AOwner.Root.FindType(APointTo);

  if (APointTo = '') or (APointToType <> nil) then
    Create(AOwner, AName, APointToType, AIsNative)
  else
  begin
    FPointTo := (AOwner.Root.SystemUnit as TSepiSystemUnit).Untyped;
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
constructor TSepiPointerType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  if not Source.IsForward then
  begin
    Create(AOwner, AName, (Source as TSepiPointerType).PointTo);
  end else
  begin
    with (Source as TSepiPointerType).FForwardInfo^ do
    begin
      if PointToTypeInfo <> nil then
        Create(Owner, AName, PointToTypeInfo)
      else
        Create(Owner, AName, PointToName);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiPointerType.Loaded;
begin
  if IsForward and (FForwardInfo <> nil) then
  begin
    with FForwardInfo^ do
    begin
      if PointToTypeInfo <> nil then
        Create(Owner, Name, PointToTypeInfo, IsNative)
      else
        Create(Owner, Name, PointToName, IsNative);
      Dispose(FForwardInfo);
    end;

    TSepiPointerType(Owner).ReAddChild(Self);
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
  [@inheritDoc]
*}
procedure TSepiPointerType.WriteDigestData(Stream: TStream);
begin
  inherited;

  WriteStrToStream(Stream, PointTo.GetFullName);
end;

{*
  [@inheritDoc]
*}
function TSepiPointerType.GetDescription: string;
begin
  if IsUntyped then
    Result := 'Pointer'
  else
    Result := '^'+PointTo.DisplayName;
end;

{*
  Crée une nouvelle instance de TSepiPointerType
  @return Instance créée
*}
class function TSepiPointerType.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  with TSepiPointerType(Result) do
  begin
    FSize := 4;
    FNeedInit := False;
    FResultBehavior := rbOrdinal;
    FIsUntyped := True;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiPointerType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    (PointTo.Equals(TSepiPointerType(Other).PointTo) or
    IsUntyped or TSepiPointerType(Other).IsUntyped);
end;

initialization
  SepiRegisterComponentClasses([
    TSepiIntegerType, TSepiCharType, TSepiInt64Type, TSepiFloatType,
    TSepiBooleanType, TSepiEnumType, TSepiSetType, TSepiPointerType
  ]);
end.

