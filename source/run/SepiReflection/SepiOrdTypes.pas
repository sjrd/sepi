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
  D�finit les classes de gestion des types ordinaux
  @author sjrd
  @version 1.0
*}
unit SepiOrdTypes;

interface

uses
  Types, Classes, SysUtils, SysConst, TypInfo, ScUtils, ScTypInfo,
  ScDelphiLanguage, ScSerializer, SepiReflectionCore, SepiReflectionConsts;

const
  MinInt64 = Int64($8000000000000000);
  MaxInt64 = Int64($7FFFFFFFFFFFFFFF);

  otUnknown = TOrdType(-1);

type
  {*
    Type de bool�en
  *}
  TBooleanKind = (bkBoolean, bkByteBool, bkWordBool, bkLongBool);

  {*
    Taille minimale d'un type �num�r�
  *}
  TSepiMinEnumSize = (mesByte, mesWord, mesLongWord);

  {*
    Type ordinal
    @author sjrd
    @version 1.0
  *}
  TSepiOrdType = class(TSepiType)
  private
    FOrdType: TOrdType; /// Type d'ordinal
    FMinValue: Longint; /// Valeur minimale
    FMaxValue: Longint; /// Valeur maximale
  protected
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    procedure SetupProperties; override;
    procedure MakeTypeInfo; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AKind: TTypeKind; AOrdType: TOrdType;
      AMinValue, AMaxValue: Longint); overload;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AKind: TTypeKind; AMinValue, AMaxValue: Longint); overload;

    function Equals(Other: TSepiType): Boolean; override;

    function ValueAsInteger(const Value): Integer;
    function ValueAsCardinal(const Value): Cardinal;

    property OrdType: TOrdType read FOrdType;
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
    FSigned: Boolean;         /// Indique si l'entier est sign� ou non
    FNeedRangeCheck: Boolean; /// Indique s'il faut v�rifier les �tendues
  protected
    function GetDescription: string; override;

    procedure SetupProperties; override;
    procedure MakeTypeInfo; override;
  public
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AMinValue: Longint = -MaxInt-1; AMaxValue: Longint = MaxInt);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function ValueToString(const Value): string; override;

    function InRange(Value: Longint): Boolean; overload;
    function InRange(Value: Int64): Boolean; overload;
    procedure CheckInRange(Value: Longint);

    property Signed: Boolean read FSigned;
    property NeedRangeCheck: Boolean read FNeedRangeCheck;
  end;

  {*
    Type caract�re
    @author sjrd
    @version 1.0
  *}
  TSepiCharType = class(TSepiOrdType)
  private
    FIsUnicode: Boolean;      /// Indique si le caract�re est Unicode
    FChrMinValue: WideChar;   /// Valeur minimale
    FChrMaxValue: WideChar;   /// Valeur maximale
    FNeedRangeCheck: Boolean; /// Indique s'il faut v�rifier les �tendues
  protected
    function GetDescription: string; override;

    procedure SetupProperties; override;
    procedure MakeTypeInfo; override;
  public
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AMinValue: WideChar = #0; AMaxValue: WideChar = #255;
      AForceUnicode: Boolean = False);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function ValueToString(const Value): string; override;

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
    FNeedRangeCheck: Boolean; /// Indique s'il faut v�rifier les �tendues
  protected
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    procedure SetupProperties; override;
    procedure MakeTypeInfo; override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AMinValue: Int64 = MinInt64; AMaxValue: Int64 = MaxInt64);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function ValueToString(const Value): string; override;

    function InRange(Value: Int64): Boolean;
    procedure CheckInRange(Value: Int64);

    property MinValue: Int64 read FMinValue;
    property MaxValue: Int64 read FMaxValue;
    property NeedRangeCheck: Boolean read FNeedRangeCheck;
  end;

  {*
    Type nombre � virgule flottante
    @author sjrd
    @version 1.0
  *}
  TSepiFloatType = class(TSepiType)
  private
    FFloatType: TFloatType; /// Type de flottant
  protected
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    procedure SetupProperties; override;
    procedure MakeTypeInfo; override;

    function GetAlignment: Integer; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AFloatType: TFloatType = ftDouble);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;

    function ValueToString(const Value): string; override;

    function ValueAsExtended(const Value): Extended;
    procedure SetValueAsExtended(var Dest; Source: Extended);

    property FloatType: TFloatType read FFloatType;
  end;

  {*
    Type bool�en
    @author sjrd
    @version 1.0
  *}
  TSepiBooleanType = class(TSepiOrdType)
  private
    FBooleanKind: TBooleanKind; /// Type de bool�en
  protected
    procedure Save(Stream: TStream); override;

    procedure MakeTypeInfo; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      ABooleanKind: TBooleanKind = bkBoolean);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function ValueToString(const Value): string; override;

    function ValueAsBoolean(const Value): Boolean;

    property BooleanKind: TBooleanKind read FBooleanKind;
  end;

  {*
    Type �num�ration
    @author sjrd
    @version 1.0
  *}
  TSepiEnumType = class(TSepiOrdType)
  private
    FBaseType: TSepiEnumType;       /// Type de base (peut �tre Self)
    FValueCount: Integer;           /// Nombre de valeurs
    FValues: TStringDynArray; /// Noms des valeurs

    procedure CreateConstants;

    function GetNames(Value: Integer): string;
    function GetValues(const Name: string): Integer;
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    function GetDescription: string; override;

    procedure MakeTypeInfo; override;

    procedure Complete; override;
  public
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

    function ValueToString(const Value): string; override;

    property BaseType: TSepiEnumType read FBaseType;
    property ValueCount: Integer read FValueCount;
    property Names[Value: Integer]: string read GetNames;
    property Values[const Name: string]: Integer read GetValues;
  end;

  {*
    Valeur d'un faux enum
    @author sjrd
    @version 1.0
  *}
  TSepiFakeEnumValue = record
    Name: string;   /// Name of the value
    Value: Longint; /// Ordinal value of this value
  end;

  /// Tableau de valeurs d'un faux enum
  TSepiFakeEnumValueDynArray = array of TSepiFakeEnumValue;

  {*
    Fausse �num�ration
    @author sjrd
    @version 1.0
  *}
  TSepiFakeEnumType = class(TSepiOrdType)
  private
    FValueCount: Integer;                /// Nombre de valeurs
    FValues: TSepiFakeEnumValueDynArray; /// Valeurs

    procedure CreateConstants;

    function GetValues(Index: Integer): TSepiFakeEnumValue;
  protected
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    procedure Complete; override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      const AValues: array of TSepiFakeEnumValue;
      AMinEnumSize: TSepiMinEnumSize = mesByte);

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;

    function ValueToString(const Value): string; override;

    property ValueCount: Integer read FValueCount;
    property Values[Index: Integer]: TSepiFakeEnumValue read GetValues;
  end;

  {*
    Type ensemble
    @author sjrd
    @version 1.0
  *}
  TSepiSetType = class(TSepiType)
  private
    FCompType: TSepiOrdType;  /// Type des �l�ments
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    procedure SetupProperties; override;
    procedure MakeTypeInfo; override;

    function GetAlignment: Integer; override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      ACompType: TSepiOrdType);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;
    function CompatibleWith(AType: TSepiType): Boolean; override;

    function ValueToString(const Value): string; override;

    property CompType: TSepiOrdType read FCompType;
  end;

  {*
    Type pointeur
    @author sjrd
    @version 1.0
  *}
  TSepiPointerType = class(TSepiOrdType)
  private
    FPointTo: TSepiType; /// Type vers lequel pointe le pointeur
    FIsUntyped: Boolean; /// Indique si c'est un pointeur non typ�
  protected
    procedure ListReferences; override;
    procedure Save(Stream: TStream); override;

    procedure WriteDigestData(Stream: TStream); override;

    procedure SetupProperties; override;
    procedure MakeTypeInfo; override;

    function GetDescription: string; override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      APointTo: TSepiType);
    constructor Clone(AOwner: TSepiComponent; const AName: string;
      Source: TSepiType); override;

    class function ForwardDecl(AOwner: TSepiComponent;
      const AName: string): TSepiPointerType;

    function Equals(Other: TSepiType): Boolean; override;

    function ValueToString(const Value): string; override;

    property PointTo: TSepiType read FPointTo;
    property IsUntyped: Boolean read FIsUntyped;
  end;

function IntegerSize(Value: Integer; ZeroGivesZero: Boolean = False): Integer;
function CardinalSize(Value: Cardinal;
  ZeroGivesZero: Boolean = False): Integer;

function ValueAsInteger(const Value; OrdType: TOrdType): Integer; overload;
function ValueAsCardinal(const Value; OrdType: TOrdType): Cardinal; overload;

function ValueAsInteger(const Value; Size: Integer): Integer; overload;
function ValueAsCardinal(const Value; Size: Integer): Cardinal; overload;

function IsIntegerType(SepiType: TSepiType): Boolean; inline;

implementation

uses
  SepiSystemUnit;

const
  /// Cha�nes 'False' et 'True' en packed ShortString
  PackedShortStrFalseTrue: array[0..10] of AnsiChar = (
    #5, 'F', 'a', 'l', 's', 'e', #4, 'T', 'r', 'u', 'e' {don't localize}
  );

  // Tailles de structure TTypeData en fonction des types
  IntegerTypeDataLength = SizeOf(TOrdType) + 2*SizeOf(Longint);
  CharTypeDataLength = IntegerTypeDataLength;
  Int64TypeDataLength = 2*SizeOf(Int64);
  FloatTypeDataLength = SizeOf(TFloatType);
  EnumTypeDataLengthBase = IntegerTypeDataLength + SizeOf(PPTypeInfo);
  BooleanTypeDataLength = EnumTypeDataLengthBase +
    SizeOf(PackedShortStrFalseTrue);
  SetTypeDataLength = SizeOf(TOrdType) + SizeOf(Pointer);

  {$IF CompilerVersion >= 21}
  PointerTypeDataLength = SizeOf(PPTypeInfo);
  {$IFEND}

{-----------------}
{ Global routines }
{-----------------}

{*
  Calcule la taille en octets d'un entier sign�
  @param Value           Entier sign�
  @param ZeroGivesZero   Si True, alors Value = 0 renvoie 0
  @return Taille de l'entier sign�
*}
function IntegerSize(Value: Integer; ZeroGivesZero: Boolean = False): Integer;
begin
  if ZeroGivesZero and (Value = 0) then
    Result := 0
  else
  begin
    if Value < 0 then
      Value := not Value;

    if Value and Integer($FFFFFF80) = 0 then
      Result := 1
    else if Value and Integer($FFFF8000) = 0 then
      Result := 2
    else
      Result := 4;
  end;
end;

{*
  Calcule la taille en octets d'un entier non sign�
  @param Value           Entier non sign�
  @param ZeroGivesZero   Si True, alors Value = 0 renvoie 0
  @return Taille de l'entier non sign�
*}
function CardinalSize(Value: Cardinal;
  ZeroGivesZero: Boolean = False): Integer;
begin
  if ZeroGivesZero and (Value = 0) then
    Result := 0
  else if Value and Cardinal($FFFFFF00) = 0 then
    Result := 1
  else if Value and Cardinal($FFFF0000) = 0 then
    Result := 2
  else
    Result := 4;
end;

{*
  Calcule le type d'ordinal pour un type �num�r�
  @param MaxValue      Valeur maximale du type �num�r�
  @param MinEnumSize   Taille minimale de type �num�r�
  @return Type d'ordinal correspondant
*}
function ComputeEnumOrdType(MaxValue: Cardinal;
  MinEnumSize: TSepiMinEnumSize): TOrdType;
begin
  if (MinEnumSize = mesLongWord) or (MaxValue >= $10000) then
    Result := otULong
  else if (MinEnumSize = mesWord) or (MaxValue >= $100) then
    Result := otUWord
  else
    Result := otUByte;
end;

{*
  Lit une valeur ordinale comme un Integer
  @param Value     Valeur de ce type
  @param OrdType   Type d'ordinal
  @return Value comme Integer
*}
function ValueAsInteger(const Value; OrdType: TOrdType): Integer;
begin
  case OrdType of
    otSByte: Result := Shortint(Value);
    otUByte: Result := Byte(Value);
    otSWord: Result := Smallint(Value);
    otUWord: Result := Word(Value);
  else
    Result := Longint(Value);
  end;
end;

{*
  Lit une valeur ordinale comme un Cardinal
  @param Value     Valeur de ce type
  @param OrdType   Type d'ordinal
  @return Value comme Cardinal
*}
function ValueAsCardinal(const Value; OrdType: TOrdType): Cardinal;
begin
  case OrdType of
    otSByte: Result := Shortint(Value);
    otUByte: Result := Byte(Value);
    otSWord: Result := Smallint(Value);
    otUWord: Result := Word(Value);
  else
    Result := LongWord(Value);
  end;
end;

{*
  Lit une valeur ordinale comme un Integer
  @param Value   Valeur de ce type
  @param Size    Taille du type (1, 2 ou 4)
  @return Value comme Integer
*}
function ValueAsInteger(const Value; Size: Integer): Integer;
const
  SizeToOrdType: array[1..4] of TOrdType = (otSByte, otSWord, otSLong, otSLong);
begin
  Result := ValueAsInteger(Value, SizeToOrdType[Size]);
end;

{*
  Lit une valeur ordinale comme un Cardinal
  @param Value   Valeur de ce type
  @param Size    Taille du type (1, 2 ou 4)
  @return Value comme Cardinal
*}
function ValueAsCardinal(const Value; Size: Integer): Cardinal;
const
  SizeToOrdType: array[1..4] of TOrdType = (otUByte, otUWord, otULong, otULong);
begin
  Result := ValueAsCardinal(Value, SizeToOrdType[Size]);
end;

{*
  Teste si un type donn� est un type entier (TSepiIntegerType ou TSepiInt64Type)
  @param SepiType   Type � tester
  @return True si SepiType est un type entier, False sinon
*}
function IsIntegerType(SepiType: TSepiType): Boolean;
begin
  Result := (SepiType is TSepiIntegerType) or (SepiType is TSepiInt64Type);
end;

{---------------------}
{ Classe TSepiOrdType }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TSepiOrdType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FOrdType, SizeOf(TOrdType));
  Stream.ReadBuffer(FMinValue, SizeOf(Longint));
  Stream.ReadBuffer(FMaxValue, SizeOf(Longint));
end;

{*
  Cr�e un type ordinal
  @param AOwner      Propri�taire
  @param AName       Nom du type
  @param AOrdType    Type d'ordinal
  @param AMinValue   Valeur minimale
  @param AMaxValue   Valeur maximale
*}
constructor TSepiOrdType.Create(AOwner: TSepiComponent; const AName: string;
  AKind: TTypeKind; AOrdType: TOrdType; AMinValue, AMaxValue: Integer);
begin
  inherited Create(AOwner, AName, AKind);

  FOrdType := AOrdType;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
end;

{*
  Cr�e un type ordinal
  @param AOwner      Propri�taire
  @param AName       Nom du type
  @param AMinValue   Valeur minimale
  @param AMaxValue   Valeur maximale
*}
constructor TSepiOrdType.Create(AOwner: TSepiComponent; const AName: string;
  AKind: TTypeKind; AMinValue, AMaxValue: Integer);
var
  AOrdType: TOrdType;
begin
  if AMinValue < 0 then
  begin
    // Signed
    if (AMinValue < -32768) or (AMaxValue >= 32768) then
      AOrdType := otSLong
    else if (AMinValue < -128) or (AMaxValue >= 128) then
      AOrdType := otSWord
    else
      AOrdType := otSByte;
  end else
  begin
    // Unsigned
    if Cardinal(AMaxValue) >= 65536 then
      AOrdType := otULong
    else if AMaxValue >= 256 then
      AOrdType := otUWord
    else
      AOrdType := otUByte;
  end;

  Create(AOwner, AName, AKind, AOrdType, AMinValue, AMaxValue);
end;

{*
  [@inheritDoc]
*}
procedure TSepiOrdType.Save(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FOrdType, SizeOf(TOrdType));
  Stream.WriteBuffer(FMinValue, SizeOf(Longint));
  Stream.WriteBuffer(FMaxValue, SizeOf(Longint));
end;

{*
  [@inheritDoc]
*}
procedure TSepiOrdType.WriteDigestData(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FOrdType, SizeOf(TOrdType));
  Stream.WriteBuffer(FMinValue, SizeOf(Longint));
  Stream.WriteBuffer(FMaxValue, SizeOf(Longint));
end;

{*
  Sp�cifie les propri�t�s du type
*}
procedure TSepiOrdType.SetupProperties;
const
  OrdTypeToSize: array[TOrdType] of Integer = (1, 1, 2, 2, 4, 4);
begin
  inherited;

  FSize := OrdTypeToSize[OrdType];
end;

{*
  Cr�e les RTTI du type
*}
procedure TSepiOrdType.MakeTypeInfo;
begin
  Assert(HasTypeInfo);

  TypeData.OrdType := OrdType;
  TypeData.MinValue := MinValue;
  TypeData.MaxValue := MaxValue;
end;

{*
  [@inheritDoc]
*}
function TSepiOrdType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    (OrdType = TSepiOrdType(Other).OrdType);
end;

{*
  Lit une valeur de ce type comme un Integer
  @param Value   Valeur de ce type
  @return Value comme Integer
*}
function TSepiOrdType.ValueAsInteger(const Value): Integer;
begin
  Result := SepiOrdTypes.ValueAsInteger(Value, OrdType);
end;

{*
  Lit une valeur de ce type comme un Cardinal
  @param Value   Valeur de ce type
  @return Value comme Cardinal
*}
function TSepiOrdType.ValueAsCardinal(const Value): Cardinal;
begin
  Result := SepiOrdTypes.ValueAsCardinal(Value, OrdType);
end;

{-------------------------}
{ Classe TSepiIntegerType }
{-------------------------}

{*
  Cr�e un nouveau type entier
  @param AOwner      Propri�taire du type
  @param AName       Nom du type
  @param AMinValue   Valeur minimale
  @param AMaxValue   Valeur maximale
  @param ASigned     Indique si l'entier est sign� ou non
*}
constructor TSepiIntegerType.Create(AOwner: TSepiComponent; const AName: string;
  AMinValue: Longint = -MaxInt-1; AMaxValue: Longint = MaxInt);
begin
  inherited Create(AOwner, AName, tkInteger, AMinValue, AMaxValue);
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
function TSepiIntegerType.GetDescription: string;
begin
  Result := IntToStr(MinValue)+'..'+IntToStr(MaxValue);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIntegerType.SetupProperties;
begin
  inherited;

  FSigned := OrdType in [otSByte, otSWord, otSLong];
  FNeedRangeCheck := (FMinValue <> -MaxInt-1) or (FMaxValue <> MaxInt);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIntegerType.MakeTypeInfo;
begin
  AllocateTypeInfo(IntegerTypeDataLength);

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiIntegerType.ValueToString(const Value): string;
begin
  if Signed then
    Result := IntToStr(ValueAsInteger(Value))
  else
    Result := IntToStr(Int64(ValueAsCardinal(Value)));
end;

{*
  Teste si une valeur est dans l'intervalle support� par le type
  @param Value   Valeur � tester
  @return True si Value est dans l'intervalle support�, False sinon
*}
function TSepiIntegerType.InRange(Value: Longint): Boolean;
begin
  Result := (Value >= FMinValue) and (Value <= FMaxValue);
end;

{*
  Teste si une valeur est dans l'intervalle support� par le type
  @param Value   Valeur � tester
  @return True si Value est dans l'intervalle support�, False sinon
*}
function TSepiIntegerType.InRange(Value: Int64): Boolean;
begin
  Result := (Value >= FMinValue) and (Value <= FMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle support� par le type
  @param Value   Valeur � tester
  @throws ERangeError Value n'�tait pas dans l'intervalle support�
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
  Cr�e un nouveau type caract�re
  @param AOwner          Propri�taire du type
  @param AName           Nom du type
  @param AMinValue       Valeur minimale
  @param AMaxValue       Valeur maximale
  @param AForceUnicode   Positionn� � True, force l'utilisation d'Unicode
*}
constructor TSepiCharType.Create(AOwner: TSepiComponent; const AName: string;
  AMinValue: WideChar = #0; AMaxValue: WideChar = #255;
  AForceUnicode: Boolean = False);
var
  AKind: TTypeKind;
  AOrdType: TOrdType;
begin
  if AForceUnicode or (AMaxValue > #255) then
  begin
    AKind := tkWChar;
    AOrdType := otUWord;
  end else
  begin
    AKind := tkChar;
    AOrdType := otUByte;
  end;

  inherited Create(AOwner, AName, AKind, AOrdType, Longint(AMinValue),
    Longint(AMaxValue));
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
  [@inheritedDoc]
*}
procedure TSepiCharType.SetupProperties;
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
  [@inheritedDoc]
*}
procedure TSepiCharType.MakeTypeInfo;
begin
  AllocateTypeInfo(CharTypeDataLength);

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiCharType.ValueToString(const Value): string;
begin
  if (not IsUnicode) or (WideChar(Value) < #$100) then
    Result := CharToCharRepres(AnsiChar(Value))
  else
    Result := '#'+IntToStr(Word(Value));
end;

{*
  Teste si une valeur est dans l'intervalle support� par le type
  @param Value   Valeur � tester
  @return True si Value est dans l'intervalle support�, False sinon
*}
function TSepiCharType.InRange(Value: WideChar): Boolean;
begin
  Result := (Value >= FChrMinValue) and (Value <= FChrMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle support� par le type
  @param Value   Valeur � tester
  @throws ERangeError Value n'�tait pas dans l'intervalle support�
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
  Charge un type entier 64 bits depuis un flux
*}
constructor TSepiInt64Type.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FMinValue, SizeOf(Int64));
  Stream.ReadBuffer(FMaxValue, SizeOf(Int64));
end;

{*
  Cr�e un nouveau type entier 64 bits
  @param AOwner      Propri�taire du type
  @param AName       Nom du type
  @param AMinValue   Valeur minimale
  @param AMaxValue   Valeur maximale
*}
constructor TSepiInt64Type.Create(AOwner: TSepiComponent; const AName: string;
  AMinValue: Int64 = MinInt64; AMaxValue: Int64 = MaxInt64);
begin
  inherited Create(AOwner, AName, tkInt64);

  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
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

  Stream.WriteBuffer(FMinValue, SizeOf(Int64));
  Stream.WriteBuffer(FMaxValue, SizeOf(Int64));
end;

{*
  [@inheritDoc]
*}
procedure TSepiInt64Type.WriteDigestData(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FMinValue, SizeOf(Int64));
  Stream.WriteBuffer(FMaxValue, SizeOf(Int64));
end;

{*
  [@inheritedDoc]
*}
procedure TSepiInt64Type.SetupProperties;
begin
  inherited;

  FSize := 8;
  FParamBehavior.AlwaysByStack := True;
  FResultBehavior := rbInt64;

  FNeedRangeCheck := (FMinValue <> MinInt64) or (FMaxValue <> MaxInt64);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiInt64Type.MakeTypeInfo;
begin
  AllocateTypeInfo(Int64TypeDataLength);

  TypeData.MinInt64Value := MinValue;
  TypeData.MaxInt64Value := MaxValue;
end;

{*
  [@inheritDoc]
*}
function TSepiInt64Type.GetDescription: string;
begin
  Result := IntToStr(MinValue)+'..'+IntToStr(MaxValue);
end;

{*
  [@inheritDoc]
*}
function TSepiInt64Type.ValueToString(const Value): string;
begin
  Result := IntToStr(Int64(Value));
end;

{*
  Teste si une valeur est dans l'intervalle support� par le type
  @param Value   Valeur � tester
  @return True si Value est dans l'intervalle support�, False sinon
*}
function TSepiInt64Type.InRange(Value: Int64): Boolean;
begin
  Result := (Value >= FMinValue) and (Value <= FMaxValue);
end;

{*
  S'assure qu'une valeur est dans l'intervalle support� par le type
  @param Value   Valeur � tester
  @throws ERangeError Value n'�tait pas dans l'intervalle support�
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
  Charge un type flottant depuis un flux
*}
constructor TSepiFloatType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FFloatType, SizeOf(TFloatType));
end;

{*
  Cr�e un nouveau type flottant
  @param AOwner       Propri�taire du type
  @param AName        Nom du type
  @param AFloatType   Type de flottant
*}
constructor TSepiFloatType.Create(AOwner: TSepiComponent; const AName: string;
  AFloatType: TFloatType = ftDouble);
begin
  inherited Create(AOwner, AName, tkFloat);

  FFloatType := AFloatType;
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

  Stream.WriteBuffer(FFloatType, SizeOf(TFloatType));
end;

{*
  [@inheritDoc]
*}
procedure TSepiFloatType.WriteDigestData(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FFloatType, SizeOf(TFloatType));
end;

{*
  [@inheritDoc]
*}
procedure TSepiFloatType.SetupProperties;
const
  Sizes: array[TFloatType] of Integer = (4, 8, 10, 8, 8);
  ResultBehaviors: array[TFloatType] of TSepiTypeResultBehavior = (
    rbSingle, rbDouble, rbExtended, rbCurrency, rbCurrency
  );
begin
  inherited;

  FSize := Sizes[FloatType];
  FParamBehavior.AlwaysByStack := True;
  FResultBehavior := ResultBehaviors[FloatType];
end;

{*
  [@inheritDoc]
*}
procedure TSepiFloatType.MakeTypeInfo;
begin
  AllocateTypeInfo(FloatTypeDataLength);

  TypeData.FloatType := FloatType;
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
  [@inheritDoc]
*}
function TSepiFloatType.ValueToString(const Value): string;
begin
  Result := FloatToStr(ValueAsExtended(Value));
end;

{*
  Lit une valeur de ce type comme valeur Extended
  @param Value   Valeur � lire
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
  Charge un type bool�en depuis un flux
*}
constructor TSepiBooleanType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FBooleanKind, SizeOf(TBooleanKind));
end;

{*
  Cr�e un type bool�en
  @param AOwner         Propri�taire
  @param AName          Nom du type
  @param ABooleanKind   Type de bool�en (d�faut : bkBoolean)
*}
constructor TSepiBooleanType.Create(AOwner: TSepiComponent; const AName: string;
  ABooleanKind: TBooleanKind = bkBoolean);
const
  KindToOrdType: array[TBooleanKind] of TOrdType =
    (otUByte, otSByte, otSWord, otSLong);
begin
  if ABooleanKind = bkBoolean then
    inherited Create(AOwner, AName, tkEnumeration, otUByte, 0, 1)
  else
  begin
    inherited Create(AOwner, AName, tkEnumeration,
      KindToOrdType[ABooleanKind], -MaxInt-1, MaxInt);
  end;

  FBooleanKind := ABooleanKind;
end;

{*
  [@inheritDoc]
*}
constructor TSepiBooleanType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  Create(AOwner, AName, (Source as TSepiBooleanType).BooleanKind);
end;

{*
  Construit les RTTI
*}
procedure TSepiBooleanType.MakeTypeInfo;
var
  BaseTypeInfo: PTypeInfo;
begin
  AllocateTypeInfo(BooleanTypeDataLength);

  inherited;

  // Find base type
    case BooleanKind of
      bkByteBool: BaseTypeInfo := System.TypeInfo(ByteBool);
      bkWordBool: BaseTypeInfo := System.TypeInfo(WordBool);
      bkLongBool: BaseTypeInfo := System.TypeInfo(LongBool);
    else
      BaseTypeInfo := System.TypeInfo(Boolean);
    end;

  // Fill type data
  TypeData.BaseType := GetTypeData(BaseTypeInfo).BaseType;

  Move(PackedShortStrFalseTrue, TypeData.NameList,
    SizeOf(PackedShortStrFalseTrue));
end;

{*
  [@inheritDoc]
*}
procedure TSepiBooleanType.Save(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FBooleanKind, SizeOf(TBooleanKind));
end;

{*
  [@inheritedDoc]
*}
function TSepiBooleanType.ValueToString(const Value): string;
begin
  Result := BooleanIdents[ValueAsBoolean(Value)];
end;

{*
  Lit une valeur de ce type comme valeur Boolean
  @param Value   Valeur � lire
  @return Valeur comme Boolean
*}
function TSepiBooleanType.ValueAsBoolean(const Value): Boolean;
begin
  Result := ValueAsInteger(Value) <> 0;
end;

{----------------------}
{ Classe TSepiEnumType }
{----------------------}

{*
  Charge un type �num�r� depuis un flux
*}
constructor TSepiEnumType.Load(AOwner: TSepiComponent; Stream: TStream);
var
  IsBaseEnum: Boolean;
begin
  inherited;

  Stream.ReadBuffer(IsBaseEnum, SizeOf(Boolean));

  if IsBaseEnum then
  begin
    FBaseType := Self;
    ReadDataFromStream(Stream, FValues, System.TypeInfo(TStringDynArray));
    FValueCount := Length(FValues);
  end else
  begin
  OwningUnit.ReadRef(Stream, FBaseType);
    FValueCount := FBaseType.ValueCount;
  end;
end;

{*
  Cr�e un nouveau type �num�r�
  @param AOwner         Propri�taire du type
  @param AName          Nom du type
  @param AValues        Noms des �l�ments de l'�num�ration
  @param AMinimumSize   Taille minimale (pour compatibilit� avec C/C++)
*}
constructor TSepiEnumType.Create(AOwner: TSepiComponent; const AName: string;
  const AValues: array of string; AMinEnumSize: TSepiMinEnumSize = mesByte);
var
  AMaxValue: Integer;
  I: Integer;
begin
  AMaxValue := High(AValues);

  inherited Create(AOwner, AName, tkEnumeration,
    ComputeEnumOrdType(AMaxValue, AMinEnumSize), 0, AMaxValue);

  FBaseType := Self;
  FValueCount := Length(AValues);

  SetLength(FValues, ValueCount);
  for I := 0 to ValueCount-1 do
    FValues[I] := AValues[I];
end;

{*
  Cr�e un nouveau type �num�r� bas� sur un type existant
  @param AOwner      Propri�taire du type
  @param AName       Nom du type
  @param ABaseType   Type �num�r� de base
  @param AMinValue   Valeur minimale
  @param AMaxValue   Valeur maximale
*}
constructor TSepiEnumType.Create(AOwner: TSepiComponent; const AName: string;
  ABaseType: TSepiEnumType; AMinValue, AMaxValue: Integer);
begin
  inherited Create(AOwner, AName, tkEnumeration, ABaseType.OrdType,
    AMinValue, AMaxValue);

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
  Cr�e les constantes �num�r�es
*}
procedure TSepiEnumType.CreateConstants;
var
  Value: Integer;
begin
  for Value := MinValue to MaxValue do
    TSepiConstant.Create(Owner, Self.Names[Value], Value, Self);
end;

{*
  Tableau des noms par leurs valeurs
  @param Value   Valeur d'�l�ment d'�num�ration
  @return Nom de la valeur Value
*}
function TSepiEnumType.GetNames(Value: Integer): string;
begin
  Result := BaseType.FValues[Value];
end;

{*
  Tableau des valeurs par leurs noms
  @param Name   Nom d'�l�ment d'�num�ration
  @return Valeur du nom donn�
*}
function TSepiEnumType.GetValues(const Name: string): Integer;
var
  I: Integer;
begin
  if BaseType <> Self then
    Result := BaseType.GetValues(Name)
  else
  begin
    for I := 0 to ValueCount-1 do
    begin
      if AnsiSameText(Name, FValues[I]) then
      begin
        Result := I;
        Exit;
      end;
    end;

    Result := -1;
  end;
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
var
  IsBaseEnum: Boolean;
begin
  inherited;

  IsBaseEnum := BaseType = Self;
  Stream.WriteBuffer(IsBaseEnum, SizeOf(Boolean));

  if IsBaseEnum then
    WriteDataToStream(Stream, FValues, System.TypeInfo(TStringDynArray))
  else
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
procedure TSepiEnumType.MakeTypeInfo;
var
  EncValues: array of TypeInfoString;
  TypeDataLength: Integer;
  I, Size: Integer;
  Current: PTypeInfoString;
begin
  // Compute TypeDataLength
  TypeDataLength := EnumTypeDataLengthBase + OwningUnit.TypeInfoNameSize;

  if BaseType = Self then
  begin
    SetLength(EncValues, ValueCount);

    for I := 0 to ValueCount-1 do
  begin
      EncValues[I] := TypeInfoEncode(FValues[I]);
      Inc(TypeDataLength, Length(EncValues[I])+1);
  end;
  end;

  // Allocate type info
  AllocateTypeInfo(TypeDataLength);
  Current := @TypeData.NameList;

  // Fill in ordinal type data
  inherited;

  // Store enumeration names
  if BaseType = Self then
  begin
    for I := 0 to ValueCount-1 do
    begin
      Size := Length(EncValues[I]) + 1;
      Move(EncValues[I], Current^, Size);
      Inc(Cardinal(Current), Size);
    end;
  end;

  // Store unit name
  OwningUnit.StoreTypeInfoName(Current);
end;

{*
  [@inheritDoc]
*}
procedure TSepiEnumType.Complete;
begin
  inherited;

  if (State = msConstructing) and (BaseType = Self) then
    CreateConstants;
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

{*
  [@inheritDoc]
*}
function TSepiEnumType.ValueToString(const Value): string;
var
  IntValue: Integer;
begin
  IntValue := ValueAsInteger(Value);

  if (IntValue >= 0) and (IntValue < ValueCount) then
    Result := Names[IntValue]
  else
    Result := Format(SOutOfBoundsEnumValue, [IntValue]);
end;

{-------------------------}
{ TSepiFakeEnumType class }
{-------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiFakeEnumType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  ReadDataFromStream(Stream, FValues,
    System.TypeInfo(TSepiFakeEnumValueDynArray));
  FValueCount := Length(FValues);
end;

{*
  Cr�e un type fausse �num�ration
  @param AOwner    Composant propri�taire
  @param AName     Nom du type
  @param AValues   Valeurs du type
*}
constructor TSepiFakeEnumType.Create(AOwner: TSepiComponent;
  const AName: string; const AValues: array of TSepiFakeEnumValue;
  AMinEnumSize: TSepiMinEnumSize = mesByte);
var
  I, PreviousValue, AMinValue, AMaxValue, Value: Integer;
begin
  FValueCount := Length(AValues);
  SetLength(FValues, ValueCount);

  PreviousValue := -1;

  AMinValue := MaxInt;
  AMaxValue := 0;

  for I := 0 to ValueCount-1 do
  begin
    FValues[I].Name := AValues[I].Name;
    Value := AValues[I].Value;

    if AValues[I].Value < 0 then
      Value := PreviousValue + 1;

    FValues[I].Value := Value;
    PreviousValue := Value;

    if Value < AMinValue then
      AMinValue := Value;
    if Value > AMaxValue then
      AMaxValue := Value;
  end;

  inherited Create(AOwner, AName, tkUnknown,
    ComputeEnumOrdType(AMaxValue, AMinEnumSize), AMinValue, AMaxValue);
end;

{*
  Cr�e les constantes �num�r�es
*}
procedure TSepiFakeEnumType.CreateConstants;
var
  I: Integer;
begin
  for I := 0 to ValueCount-1 do
    with FValues[I] do
      TSepiConstant.Create(Owner, Name, Value, Self);
end;

{*
  Tableau zero-based des valeurs de ce type fausse �num�ration
  @param Index   Index compris entre 0 inclus et ValueCount exclu
  @return La valeur � l'index sp�cifi�
*}
function TSepiFakeEnumType.GetValues(Index: Integer): TSepiFakeEnumValue;
begin
  Result := FValues[Index];
end;

{*
  [@inheritDoc]
*}
procedure TSepiFakeEnumType.Save(Stream: TStream);
begin
  inherited;

  WriteDataToStream(Stream, FValues,
    System.TypeInfo(TSepiFakeEnumValueDynArray));
end;

{*
  [@inheritDoc]
*}
procedure TSepiFakeEnumType.WriteDigestData(Stream: TStream);
begin
  inherited;

  WriteDataToStream(Stream, FValues,
    System.TypeInfo(TSepiFakeEnumValueDynArray));
end;

{*
  [@inheritDoc]
*}
procedure TSepiFakeEnumType.Complete;
begin
  inherited;

  if State = msConstructing then
    CreateConstants;
end;

{*
  [@inheritDoc]
*}
function TSepiFakeEnumType.GetDescription: string;
var
  I: Integer;
begin
  for I := 0 to ValueCount-1 do
    with FValues[I] do
      Result := Result + ' ' + Name + ' = ' + IntToStr(Value) + ',';
  Result[1] := '(';
  Result[Length(Result)] := ')';
end;

{*
  [@inheritDoc]
*}
function TSepiFakeEnumType.Equals(Other: TSepiType): Boolean;
begin
  Result := Other = Self;
end;

{*
  [@inheritDoc]
*}
function TSepiFakeEnumType.CompatibleWith(AType: TSepiType): Boolean;
begin
  Result := AType = Self;
end;

{*
  [@inheritDoc]
*}
function TSepiFakeEnumType.ValueToString(const Value): string;
begin
  Result := Format('%s(%d)', [Name, ValueAsInteger(Value)]);
end;

{---------------------}
{ Classe TSepiSetType }
{---------------------}

{*
  Charge un type ensemble depuis un flux
*}
constructor TSepiSetType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  inherited;

  OwningUnit.ReadRef(Stream, FCompType);
end;

{*
  Cr�e un nouveau type ensemble
  @param AOwner      Propri�taire du type
  @param AName       Nom du type
  @param ACompType   Type des �l�ments de l'ensemble
*}
constructor TSepiSetType.Create(AOwner: TSepiComponent; const AName: string;
  ACompType: TSepiOrdType);
begin
  inherited Create(AOwner, AName, tkSet);

  FCompType := ACompType;
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
  [@inheritDoc]
*}
procedure TSepiSetType.SetupProperties;
begin
  inherited;

  FSize := (CompType.MaxValue - CompType.MinValue) div 8 + 1;
  if FSize = 3 then
    FSize := 4;

  FParamBehavior.AlwaysByAddress := Size > 4;
  if FParamBehavior.AlwaysByAddress then
    FResultBehavior := rbParameter;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetType.MakeTypeInfo;
begin
  AllocateTypeInfo(SetTypeDataLength);

  case CompType.MaxValue - CompType.MinValue + 1 of
    1..8: TypeData.OrdType := otUByte;
    9..16: TypeData.OrdType := otUWord;
    17..32: TypeData.OrdType := otULong;
  else
    TypeData.OrdType := otUnknown;
  end;

  TypeData.CompType := FCompType.TypeInfoRef;
end;

{*
  [@inheritDoc]
*}
function TSepiSetType.GetAlignment: Integer;
begin
  {$IF CompilerVersion < 20}
    if Size <= 4 then
      Result := Size
    else
      Result := 8;
  {$ELSE}
    Result := 1;
  {$IFEND}
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

{*
  [@inheritDoc]
*}
function TSepiSetType.ValueToString(const Value): string;
type
  TSet = set of Byte;
var
  Min, Max, Current: Integer;
begin
  Result := '[';

  Min := CompType.MinValue;
  Max := CompType.MaxValue;
  Current := Min;

  while Current <= Max do
  begin
    if not (Current-Min in TSet(Value)) then
    begin
      Inc(Current);
      Continue;
    end;

    Result := Result + CompType.ValueToString(Current);
    Inc(Current);

    if Current-Min in TSet(Value) then
    begin
      repeat
        Inc(Current);
      until (Current > Max) or not (Current-Min in TSet(Value));

      Dec(Current);
      Result := Result + '..' + CompType.ValueToString(Current);
    end;

    Result := Result + ', ';
    Inc(Current);
  end;

  if Length(Result) > 1 then
    SetLength(Result, Length(Result)-2);

  Result := Result + ']';
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

  OwningUnit.ReadRef(Stream, FPointTo);
  FIsUntyped := FPointTo is TSepiUntypedType;
end;

{*
  Cr�e un nouveau type pointeur
  @param AOwner     Propri�taire du type
  @param AName      Nom du type
  @param APointTo   Type vers lequel pointe le pointeur
*}
constructor TSepiPointerType.Create(AOwner: TSepiComponent; const AName: string;
  APointTo: TSepiType);
begin
  if APointTo = nil then
    APointTo := TSepiSystemUnit.Get(AOwner.Root).Untyped;

  inherited Create(AOwner, AName, tkPointerOrUnknown);

  FPointTo := APointTo;
  FIsUntyped := APointTo is TSepiUntypedType;
end;

{*
  [@inheritDoc]
*}
constructor TSepiPointerType.Clone(AOwner: TSepiComponent; const AName: string;
  Source: TSepiType);
begin
  Create(AOwner, AName, (Source as TSepiPointerType).PointTo);
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
procedure TSepiPointerType.MakeTypeInfo;
begin
  {$IF CompilerVersion >= 21}
  AllocateTypeInfo(PointerTypeDataLength);

  TypeData.RefType := TSepiPointerType(PointTo).TypeInfoRef;
  {$IFEND}
end;

{*
  [@inheritDoc]
*}
procedure TSepiPointerType.SetupProperties;
begin
  inherited;

  FSize := 4;
  FNeedInit := False;
  FResultBehavior := rbOrdinal;
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
  D�clare un type pointeur en forward
  @param AOwner   Propri�taire du type
  @param AName    Nom du type pointeur
*}
class function TSepiPointerType.ForwardDecl(AOwner: TSepiComponent;
  const AName: string): TSepiPointerType;
begin
  Result := TSepiPointerType(inherited ForwardDecl(AOwner, AName));
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

{*
  [@inheritDoc]
*}
function TSepiPointerType.ValueToString(const Value): string;
begin
  Result := Format('%s($%s)', [Name, IntToHex(LongWord(Value), 8)]);
end;

initialization
  SepiRegisterComponentClasses([
    TSepiIntegerType, TSepiCharType, TSepiInt64Type, TSepiFloatType,
    TSepiBooleanType, TSepiEnumType, TSepiFakeEnumType, TSepiSetType,
    TSepiPointerType
  ]);
end.

