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
  Définit les classes de gestion de l'unité System
  @author sjrd
  @version 1.0
*}
unit SepiSystemUnit;

interface

{$ASSERTIONS ON}

uses
  Classes, StrUtils, TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes,
  SepiArrayTypes, SepiMembers, SepiReflectionConsts;

type
  {*
    Types de base de l'unité System
    @author sjrd
    @version 1.0
  *}
  TSepiSystemTypes = packed record
    { Update the SystemTypeNames constant below when you modify this record. }

    // Special types
    Untyped: TSepiUntypedType;

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
    UInt64: TSepiInt64Type;

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
    UnicodeString: TSepiStringType;

    // Variant types
    Variant: TSepiVariantType;
    OleVariant: TSepiVariantType;

    // Pointer types
    Pointer: TSepiPointerType;
    PByte: TSepiPointerType;
    PChar: TSepiPointerType;
    PAnsiChar: TSepiPointerType;
    PWideChar: TSepiPointerType;

    // HRESULT
    HRESULT: TSepiIntegerType;

    // Special container types
    TObject: TSepiClass;
    TClass: TSepiMetaClass;
    IInterface: TSepiInterface;
    IUnknown: TSepiInterface;
    IDispatch: TSepiInterface;
    TGUID: TSepiRecordType;
    TMethod: TSepiRecordType;
    TVarRec: TSepiRecordType;
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

    procedure CreateBuiltins;

    class function Get(Component: TSepiComponent): TSepiSystemUnit;
      {$IF CompilerVersion >= 20} static; {$IFEND}

    function GetSystemType(TypeKind: TTypeKind): TSepiType; overload;
    function GetSystemType(TypeInfo: PTypeInfo): TSepiType; overload;
    function GetSystemType(OrdType: TOrdType): TSepiOrdType; overload;
    function GetSystemType(FloatType: TFloatType): TSepiFloatType; overload;

    property Types: TSepiSystemTypes read FTypes;

    // Special types
    property Untyped: TSepiUntypedType read FTypes.Untyped;

    // Integer types - warning: UInt64 may be nil
    property Integer: TSepiOrdType read FTypes.Integer;
    property Cardinal: TSepiOrdType read FTypes.Cardinal;
    property Shortint: TSepiOrdType read FTypes.Shortint;
    property Smallint: TSepiOrdType read FTypes.Smallint;
    property Longint: TSepiOrdType read FTypes.Longint;
    property Int64: TSepiInt64Type read FTypes.Int64;
    property Byte: TSepiOrdType read FTypes.Byte;
    property Word: TSepiOrdType read FTypes.Word;
    property LongWord: TSepiOrdType read FTypes.LongWord;
    property UInt64: TSepiInt64Type read FTypes.UInt64;

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

    // String types - warning: UnicodeString may be nil
    property ShortString: TSepiShortStringType read FTypes.ShortString;
    property LongString: TSepiStringType read FTypes.LongString;
    property AnsiString: TSepiStringType read FTypes.AnsiString;
    property WideString: TSepiStringType read FTypes.WideString;
    property UnicodeString: TSepiStringType read FTypes.UnicodeString;

    // Variant types
    property Variant: TSepiVariantType read FTypes.Variant;
    property OleVariant: TSepiVariantType read FTypes.OleVariant;

    // Pointer types
    property Pointer: TSepiPointerType read FTypes.Pointer;
    property PByte: TSepiPointerType read FTypes.PByte;
    property PChar: TSepiPointerType read FTypes.PChar;
    property PAnsiChar: TSepiPointerType read FTypes.PAnsiChar;
    property PWideChar: TSepiPointerType read FTypes.PWideChar;

    // HRESULT
    property HRESULT: TSepiIntegerType read FTypes.HRESULT;

    // Special container types
    property TObject: TSepiClass read FTypes.TObject;
    property TClass: TSepiMetaClass read FTypes.TClass;
    property IInterface: TSepiInterface read FTypes.IInterface;
    property IUnknown: TSepiInterface read FTypes.IUnknown;
    property IDispatch: TSepiInterface read FTypes.IDispatch;
    property TGUID: TSepiRecordType read FTypes.TGUID;
    property TMethod: TSepiRecordType read FTypes.TMethod;
    property TVarRec: TSepiRecordType read FTypes.TVarRec;
  end;

implementation

const // don't localize
  /// Nombre de types système
  SystemTypeCount = SizeOf(TSepiSystemTypes) div SizeOf(TSepiType);

  /// Noms des types sytème
  SystemTypeNames: array[0..SystemTypeCount-1] of string = (
    // Special types
    SUntypedTypeName,

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
    'UInt64',

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
    'UnicodeString',

    // Variant types
    'Variant',
    'OleVariant',

    // Pointer types
    'Pointer',
    'PByte',
    'PChar',
    'PAnsiChar',
    'PWideChar',

    // HRESULT
    'HRESULT',

    // Special container types
    'TObject',
    'TClass',
    'IInterface',
    'IUnknown',
    'IDispatch',
    'TGUID',
    'TMethod',
    'TVarRec'
  );

{-----------------}
{ Global routines }
{-----------------}

procedure Write(const S: string);
begin
  System.Write(S);
end;

procedure Writeln(const S: string);
begin
  System.Writeln(S);
end;

procedure Readln(var S: string);
begin
  System.Readln(S);
end;

{*
  Create builtin types
*}
procedure InternalCreateBuiltinTypes(Self: TSepiSystemUnit);
begin
  // Special types
  TSepiUntypedType.Create(Self, SUntypedTypeName);

  // Integer types
  TSepiIntegerType.Create(Self, 'Integer', Low(Integer), High(Integer));
  TSepiIntegerType.Create(Self, 'Cardinal', Low(Cardinal),
    Integer(High(Cardinal)));
  TSepiIntegerType.Create(Self, 'Shortint', Low(Shortint), High(Shortint));
  TSepiIntegerType.Create(Self, 'Smallint', Low(Smallint), High(Smallint));
  TSepiTypeAlias.Create(Self, 'Longint', Self.Integer);
  TSepiInt64Type.Create(Self, 'Int64', Low(Int64), High(Int64));
  TSepiIntegerType.Create(Self, 'Byte', Low(Byte), High(Byte));
  TSepiIntegerType.Create(Self, 'Word', Low(Word), High(Word));
  TSepiTypeAlias.Create(Self, 'LongWord', Self.Cardinal);
  {$IF Declared(UInt64)}
  TSepiInt64Type.Create(Self, 'UInt64', Low(UInt64), Int64(High(UInt64)));
  {$IFEND}

  // Character types
  TSepiCharType.Create(Self, 'Char', Low(Char), High(Char));
  {$IFDEF UNICODE}
  TSepiCharType.Create(Self, 'AnsiChar', Low(AnsiChar), High(AnsiChar));
  TSepiTypeAlias.Create(Self, 'WideChar', Self.Char);
  {$ELSE}
  TSepiTypeAlias.Create(Self, 'AnsiChar', Self.Char);
  TSepiCharType.Create(Self, 'WideChar', Low(WideChar), High(WideChar));
  {$ENDIF}

  // Boolean types
  TSepiBooleanType.Create(Self, 'Boolean', bkBoolean);
  TSepiBooleanType.Create(Self, 'ByteBool', bkByteBool);
  TSepiBooleanType.Create(Self, 'WordBool', bkWordBool);
  TSepiBooleanType.Create(Self, 'LongBool', bkLongBool);

  // Float types
  TSepiFloatType.Create(Self, 'Single', ftSingle);
  TSepiFloatType.Create(Self, 'Double', ftDouble);
  TSepiFloatType.Create(Self, 'Extended', ftExtended);
  TSepiFloatType.Create(Self, 'Comp', ftComp);
  TSepiFloatType.Create(Self, 'Currency', ftCurr);

  // String types
  TSepiStringType.Create(Self, 'string', DefaultStringKind);
  TSepiShortStringType.Create(Self, 'ShortString');
  {$IFDEF UNICODE}
  TSepiStringType.Create(Self, 'AnsiString', skAnsiString,
    GetTypeData(TypeInfo(AnsiString)).CodePage);
  TSepiStringType.Create(Self, 'WideString', skWideString);
  TSepiTypeAlias.Create(Self, 'UnicodeString', Self.LongString);
  {$ELSE}
  TSepiTypeAlias.Create(Self, 'AnsiString', Self.LongString);
  TSepiStringType.Create(Self, 'WideString', skWideString);
  {$ENDIF}

  // Variant types
  TSepiVariantType.Create(Self, 'Variant');
  TSepiVariantType.Create(Self, 'OleVariant');

  // Pointer types
  TSepiPointerType.Create(Self, 'Pointer', Self.Untyped);
  TSepiPointerType.Create(Self, 'PChar', Self.Char);
  TSepiPointerType.Create(Self, 'PAnsiChar', Self.AnsiChar);
  TSepiPointerType.Create(Self, 'PWideChar', Self.WideChar);

  // Text file type
  TSepiTextFileType.Create(Self, 'Text');
end;

{*
  Create builtin constants
*}
procedure InternalCreateBuiltinConstants(Self: TSepiSystemUnit);
begin
  TSepiConstant.Create(Self, 'CompilerVersion', CompilerVersion);
  TSepiConstant.Create(Self, 'True', True);
  TSepiConstant.Create(Self, 'False', False);
  TSepiConstant.Create(Self, 'MaxInt', MaxInt);
  TSepiConstant.Create(Self, 'MaxLongint', MaxLongint);

  { We declare Pi as a constant, though it is a method in the actual Delphi
    compiler, because it makes no difference in appearence, and it is better
    handled by Sepi as a constant.
    Well, actually, we think that the Delphi compiler makes the opposite
    reasoning, and declares it as a method because it handles it better that
    way. }
  TSepiConstant.Create(Self, 'Pi', Pi);

  { The pseudo-constant nil isn't declared here, for it has many different
    types, depending on the situation. Each compiler should understand the nil
    value for what it is: a special value, not a simple constant. }
end;

{*
  Create builtin methods
*}
procedure InternalCreateBuiltinMethods(Self: TSepiSystemUnit);
begin
  { Write, Writeln and Readln routines
    We create these three routines as overloaded, though there are only one of
    them each. This will force the unit importer to generate indirect calls to
    these methods, which is needed because they do not have an actual address
    in System.pas. Besides, these methods being overloaded better matches the
    actual implementation, since the Delphi compiler accepts many different
    kinds of argument. So, it appears like we simply do not support other
    types of argument. }
  TSepiMethod.CreateOverloaded(Self, 'Write', @Write,
    'static procedure(const S: string)');
  TSepiMethod.CreateOverloaded(Self, 'Writeln', @Writeln,
    'static procedure(const S: string)');
  TSepiMethod.CreateOverloaded(Self, 'Readln', @Readln,
    'static procedure(var S: string)');
end;

{*
  Create builtins
*}
procedure InternalCreateBuiltins(Self: TSepiSystemUnit);
begin
  InternalCreateBuiltinTypes(Self);
  InternalCreateBuiltinConstants(Self);
  InternalCreateBuiltinMethods(Self);
end;

{-----------------------}
{ TSepiSystemUnit class }
{-----------------------}

{*
  [@inheritDoc]
*}
constructor TSepiSystemUnit.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  FRemainingUnknownTypeCount := SizeOf(TSepiSystemTypes) div SizeOf(TSepiType);

  inherited;
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

{*
  Create builtins - components that are not even written in the System.pas file
*}
procedure TSepiSystemUnit.CreateBuiltins;
begin
  InternalCreateBuiltins(Self);
end;

{*
  Obtient l'unité System pour un composant Sepi donné
  @param Component   Composant
  @return Unité System correspondant au composant spécifié
*}
class function TSepiSystemUnit.Get(Component: TSepiComponent): TSepiSystemUnit;
begin
  Result := Component.Root.SystemUnit as TSepiSystemUnit;
end;

{*
  Trouve le type système par défaut pour un type de type donné
  @param TypeKind   Type de type attendu
  @return Type système par défaut du type attendu
*}
function TSepiSystemUnit.GetSystemType(TypeKind: TTypeKind): TSepiType;
begin
  case TypeKind of
    tkInteger: Result := Integer;
    tkChar: Result := AnsiChar;
    tkFloat: Result := Extended;
    tkString: Result := ShortString;
    tkClass: Result := TObject;
    tkWChar: Result := WideChar;
    tkLString: Result := AnsiString;
    tkWString: Result := WideString;
    tkVariant: Result := Variant;
    tkInterface: Result := IInterface;
    tkInt64: Result := Int64;
    {$IF Declared(tkUString)}
    tkUString: Result := UnicodeString;
    {$IFEND}
  else
    Assert(False, 'No system type found');
    Result := nil;
  end;
end;

{*
  Trouve le type système par défaut correspondant à un type donné
  @param TypeInfo   RTTI d'un type
  @return Type système par défaut correspondant au type spécifié
*}
function TSepiSystemUnit.GetSystemType(TypeInfo: PTypeInfo): TSepiType;
begin
  Assert(TypeInfo <> nil, 'No system type found');
  Result := GetSystemType(TypeInfo.Kind);
end;

{*
  Trouve le type ordinal par défaut pour un type d'ordinal donné
  @param OrdType   Type d'ordinal attendu
  @return Type ordinal par défaut du type attendu
*}
function TSepiSystemUnit.GetSystemType(OrdType: TOrdType): TSepiOrdType;
begin
  case OrdType of
    otSByte: Result := Shortint;
    otUByte: Result := Byte;
    otSWord: Result := Smallint;
    otUWord: Result := Word;
    otSLong: Result := Longint;
    otULong: Result := LongWord;
  else
    Assert(False, 'No system type found');
    Result := nil;
  end;
end;

{*
  Trouve le type flottant par défaut pour un type de flottant donné
  @param FloatType   Type de flottant attendu
  @return Type flottant par défaut du type attendu
*}
function TSepiSystemUnit.GetSystemType(FloatType: TFloatType): TSepiFloatType;
begin
  case FloatType of
    ftSingle: Result := Single;
    ftDouble: Result := Double;
    ftExtended: Result := Extended;
    ftComp: Result := Comp;
    ftCurr: Result := Currency;
  else
    Assert(False, 'No system type found');
    Result := nil;
  end;
end;

end.

