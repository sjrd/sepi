{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of the SCL (Sepi Code Library), which is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.

Linking this library -the SCL- statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and conditions
of the GNU General Public License cover the whole combination.

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
  Étend les informations et les routines proposées par l'unité TypInfo
  @author sjrd
  @version 1.0
*}
unit ScTypInfo;

interface

uses
  TypInfo;

const
  /// Types qui requièrent une initialisation
  { tkArray and tkRecord are listed here, though static arrays and record don't
    always need initialization, because these types have got RTTI if and only
    if the particular type needs initialization. }
  NeedInitTypeKinds = [
    tkLString, tkWString, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray
  ];

type
  /// Pointeur vers TRecordField
  PRecordField = ^TRecordField;

  {*
    Champ d'un record (qui requiert une initialisation)
    @author sjrd
    @version 1.0
  *}
  TRecordField = packed record
    TypeInfo: PPTypeInfo; /// RTTI du type du champ
    Offset: Integer;      /// Offset du champ dans le record
  end;

  /// Pointeur vers TRecordTypeData
  PRecordTypeData = ^TRecordTypeData;

  {*
    Données d'un type record
    @author sjrd
    @version 1.0
  *}
  TRecordTypeData = packed record
    Size: Integer;                       /// Taille du type
    FieldCount: Integer;                 /// Nombre de champs
    Fields: array[0..0] of TRecordField; /// Champs (0..FieldCount-1)
  end;

  /// Pointeur vers TArrayTypeData
  PArrayTypeData = ^TArrayTypeData;

  {*
    Données d'un type tableau statique
    @author sjrd
    @version 1.0
  *}
  TArrayTypeData = packed record
    Size: Integer;      /// Taille du type
    Count: Integer;     /// Nombre d'éléments (linéarisés)
    ElType: PPTypeInfo; /// RTTI du type des éléments
  end;

function TypeSize(TypeInfo: PTypeInfo): Integer;

procedure CopyData(const Source; var Dest; Size: Integer;
  TypeInfo: PTypeInfo); overload;
procedure CopyData(const Source; var Dest; TypeInfo: PTypeInfo); overload;

implementation

uses
  ScCompilerMagic;

{*
  Détermine la taille d'un type à partir de ses RTTI
  Le seul cas dans lequel TypeSize est incapable de déterminer la taille du
  type (et donc renvoie -1), est si TypeInfo.Kind = tkUnknown, ce qui ne
  devrait jamais arriver.
  @param TypeInfo   RTTI du type
  @return Taille du type, ou -1 si c'est impossible à déterminer
*}
function TypeSize(TypeInfo: PTypeInfo): Integer;
const
  TypeKindToSize: array[TTypeKind] of Integer = (
    -1, 0, 1, 0, 0, 0, 0, 4, 8, 2, 4, 4, 16, 0, 0, 4, 8, 4
  );
  OrdTypeToSize: array[TOrdType] of Integer = (1, 1, 2, 2, 4, 4);
  FloatTypeToSize: array[TFloatType] of Integer = (4, 8, 10, 8, 8);
var
  TypeData: PTypeData;
begin
  Result := TypeKindToSize[TypeInfo.Kind];

  if Result = 0 then
  begin
    TypeData := GetTypeData(TypeInfo);
    case TypeInfo.Kind of
      tkInteger,
      tkEnumeration: Result := OrdTypeToSize[TypeData.OrdType];
      tkFloat: Result := FloatTypeToSize[TypeData.FloatType];
      tkString: Result := TypeData.MaxLength+1;
      tkArray: Result := PArrayTypeData(TypeData).Size;
      tkRecord: Result := PRecordTypeData(TypeData).Size;

      { Though tkSet has also the OrdType field, it isn't always reliable,
        since it can be out of range for large sets. }
      tkSet:
      begin
        with GetTypeData(TypeData.CompType^)^ do
          Result := (MaxValue - MinValue) div 8 + 1;
        if Result = 3 then
          Result := 4;
      end;
    end;
  end;
end;

{*
  Copie une variable
  Le paramètre TypeInfo n'est requis que si le type de données requiert une
  initialisation. Mais ce n'est pas une erreur de le renseigner même dans les
  autres cas.
  @param Source     Variable source
  @param Dest       Variable destination
  @param Size       Taille du type de la variable
  @param TypeInfo   RTTI du type de la variable (si requiert une initialisation)
*}
procedure CopyData(const Source; var Dest; Size: Integer;
  TypeInfo: PTypeInfo);
begin
  if Assigned(TypeInfo) and (TypeInfo.Kind in NeedInitTypeKinds) then
    CopyData(Source, Dest, TypeInfo)
  else
    Move(Source, Dest, Size);
end;

{*
  Copie une variable dont le type possède des RTTI
  Utilisez cette variante de CopyData si vous ne connaissez pas la taille du
  type de données. En revanche, vous devez fournir des RTTI non-nulles du type.
  @param Source     Variable source
  @param Dest       Variable destination
  @param TypeInfo   RTTI du type de la variable
*}
procedure CopyData(const Source; var Dest; TypeInfo: PTypeInfo);
begin
  case TypeInfo.Kind of
    tkLString: AnsiString(Dest) := AnsiString(Source);
    tkWString: WideString(Dest) := WideString(Source);
    tkVariant: Variant(Dest) := Variant(Source);
    tkArray:
      with PArrayTypeData(GetTypeData(TypeInfo))^ do
        CopyArray(@Dest, @Source, ElType^, Count);
    tkRecord: CopyRecord(@Dest, @Source, TypeInfo);
    tkInterface: IInterface(Dest) := IInterface(Source);
    tkDynArray: DynArrayCopy(Pointer(Source), TypeInfo, Pointer(Dest));
  else
    Move(Source, Dest, TypeSize(TypeInfo));
  end;
end;

end.

