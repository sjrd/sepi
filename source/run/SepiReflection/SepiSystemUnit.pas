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
  Définit les classes de gestion de l'unité System
  @author sjrd
  @version 1.0
*}
unit SepiSystemUnit;

interface

{$ASSERTIONS ON}

uses
  Classes, StrUtils, TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes,
  SepiArrayTypes, SepiMembers;

type
  {*
    Types de base de l'unité System
    @author sjrd
    @version 1.0
  *}
  TSepiSystemTypes = packed record
    { Update the SystemTypeCount constant below when you modify this record. }

    // Integer types
    Integer: TSepiOrdType;
    Cardinal: TSepiOrdType;
    Shortint: TSepiOrdType;
    Smallint: TSepiOrdType;
    Longint: TSepiOrdType;
    Int64: TSepiInt64Type;
    Byte: TSepiOrdType;
    Word: TSepiOrdType;
    LongWord: TSepiOrdType;

    // Character types
    Char: TSepiCharType;
    AnsiChar: TSepiCharType;
    WideChar: TSepiCharType;

    // Boolean types
    Boolean: TSepiBooleanType;
    ByteBool: TSepiBooleanType;
    WordBool: TSepiBooleanType;
    LongBool: TSepiBooleanType;

    // Float types
    Single: TSepiFloatType;
    Double: TSepiFloatType;
    Extended: TSepiFloatType;
    Comp: TSepiFloatType;
    Currency: TSepiFloatType;
    TDateTime: TSepiFloatType;

    // String types
    ShortString: TSepiShortStringType;
    LongString: TSepiStringType;
    AnsiString: TSepiStringType;
    WideString: TSepiStringType;

    // Variant types
    Variant: TSepiVariantType;
    OleVariant: TSepiVariantType;

    // Pointer types
    Pointer: TSepiPointerType;
    PByte: TSepiPointerType;
    PChar: TSepiPointerType;
    PAnsiChar: TSepiPointerType;
    PWideChar: TSepiPointerType;

    // Special container types
    TObject: TSepiClass;
    TClass: TSepiMetaClass;
    IInterface: TSepiInterface;
    IUnknown: TSepiInterface;
    IDispatch: TSepiInterface;
    TGUID: TSepiRecordType;
    TMethod: TSepiRecordType;
  end;

  {*
    Unité System
    @author sjrd
    @version 1.0
  *}
  TSepiSystemUnit = class(TSepiUnit)
  private
    FTypes: TSepiSystemTypes; /// Types système

    FRemainingUnknownTypeCount: Integer; /// Nombre de types encore inconnus
  protected
    procedure ChildAdded(Child: TSepiComponent); override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent);

    property Types: TSepiSystemTypes read FTypes;

    // Integer types
    property Integer: TSepiOrdType read FTypes.Integer;
    property Cardinal: TSepiOrdType read FTypes.Cardinal;
    property Shortint: TSepiOrdType read FTypes.Shortint;
    property Smallint: TSepiOrdType read FTypes.Smallint;
    property Longint: TSepiOrdType read FTypes.Longint;
    property Int64: TSepiInt64Type read FTypes.Int64;
    property Byte: TSepiOrdType read FTypes.Byte;
    property Word: TSepiOrdType read FTypes.Word;
    property LongWord: TSepiOrdType read FTypes.LongWord;

    // Character types
    property Char: TSepiCharType read FTypes.Char;
    property AnsiChar: TSepiCharType read FTypes.AnsiChar;
    property WideChar: TSepiCharType read FTypes.WideChar;

    // Boolean types
    property Boolean: TSepiBooleanType read FTypes.Boolean;
    property ByteBool: TSepiBooleanType read FTypes.ByteBool;
    property WordBool: TSepiBooleanType read FTypes.WordBool;
    property LongBool: TSepiBooleanType read FTypes.LongBool;

    // Float types
    property Single: TSepiFloatType read FTypes.Single;
    property Double: TSepiFloatType read FTypes.Double;
    property Extended: TSepiFloatType read FTypes.Extended;
    property Comp: TSepiFloatType read FTypes.Comp;
    property Currency: TSepiFloatType read FTypes.Currency;
    property TDateTime: TSepiFloatType read FTypes.TDateTime;

    // String types
    property ShortString: TSepiShortStringType read FTypes.ShortString;
    property LongString: TSepiStringType read FTypes.LongString;
    property AnsiString: TSepiStringType read FTypes.AnsiString;
    property WideString: TSepiStringType read FTypes.WideString;

    // Variant types
    property Variant: TSepiVariantType read FTypes.Variant;
    property OleVariant: TSepiVariantType read FTypes.OleVariant;

    // Pointer types
    property Pointer: TSepiPointerType read FTypes.Pointer;
    property PByte: TSepiPointerType read FTypes.PByte;
    property PChar: TSepiPointerType read FTypes.PChar;
    property PAnsiChar: TSepiPointerType read FTypes.PAnsiChar;
    property PWideChar: TSepiPointerType read FTypes.PWideChar;

    // Special container types
    property TObject: TSepiClass read FTypes.TObject;
    property TClass: TSepiMetaClass read FTypes.TClass;
    property IInterface: TSepiInterface read FTypes.IInterface;
    property IUnknown: TSepiInterface read FTypes.IUnknown;
    property IDispatch: TSepiInterface read FTypes.IDispatch;
    property TGUID: TSepiRecordType read FTypes.TGUID;
    property TMethod: TSepiRecordType read FTypes.TMethod;
  end;

implementation

const // don't localize
  /// Nombre de types système
  SystemTypeCount = SizeOf(TSepiSystemTypes) div SizeOf(TSepiType);

  /// Noms des types sytème
  SystemTypeNames: array[0..SystemTypeCount-1] of string = (
    // Integer types
    'Integer',
    'Cardinal',
    'Shortint',
    'Smallint',
    'Longint',
    'Int64',
    'Byte',
    'Word',
    'LongWord',

    // Character types
    'Char',
    'AnsiChar',
    'WideChar',

    // Boolean types
    'Boolean',
    'ByteBool',
    'WordBool',
    'LongBool',

    // Float types
    'Single',
    'Double',
    'Extended',
    'Comp',
    'Currency',
    'TDateTime',

    // String types
    'ShortString',
    'string',
    'AnsiString',
    'WideString',

    // Variant types
    'Variant',
    'OleVariant',

    // Pointer types
    'Pointer',
    'PByte',
    'PChar',
    'PAnsiChar',
    'PWideChar',

    // Special container types
    'TObject',
    'TClass',
    'IInterface',
    'IUnknown',
    'IDispatch',
    'TGUID',
    'TMethod'
  );

{-----------------------}
{ TSepiSystemUnit class }
{-----------------------}

{*
  [@inheritDoc]
*}
constructor TSepiSystemUnit.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  Assert(False, 'No support for loading the System unit at the moment');
  inherited;

  FRemainingUnknownTypeCount := SizeOf(TSepiSystemTypes) div SizeOf(TSepiType);
end;

{*
  Crée l'unité System
  @param AOwner   Propriétaire de l'unité
*}
constructor TSepiSystemUnit.Create(AOwner: TSepiComponent);
begin
  inherited Create(AOwner, SystemUnitName, []);

  FRemainingUnknownTypeCount := SizeOf(TSepiSystemTypes) div SizeOf(TSepiType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSystemUnit.ChildAdded(Child: TSepiComponent);
type
  TTypeArray = packed array[0..SystemTypeCount-1] of TSepiType;
var
  Index: System.Integer;
  SepiType: TSepiType;
begin
  inherited;

  if FRemainingUnknownTypeCount > 0 then
  begin
    Index := AnsiIndexText(Child.Name, SystemTypeNames);
    if Index >= 0 then
    begin
      if Child is TSepiTypeAlias then
        SepiType := TSepiTypeAlias(Child).Dest
      else
        SepiType := Child as TSepiType;

      TTypeArray(FTypes)[Index] := SepiType;
      Dec(FRemainingUnknownTypeCount);
    end;
  end;
end;

end.

