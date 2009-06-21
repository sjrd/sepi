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
  Liaison Sepi-PS à la compilation
  @author sjrd
  @version 1.0
*}
unit SepiPSCompileTime;

interface

uses
  SepiReflectionCore, uPSCompiler;

procedure SepiImportUnitInPSCompiler(SepiUnit: TSepiUnit;
  PSCompiler: TPSPascalCompiler); overload;
procedure SepiImportUnitInPSCompiler(Root: TSepiRoot;
  const UnitName: string; PSCompiler: TPSPascalCompiler); overload;

implementation

uses
  SysUtils, TypInfo, StrUtils, ScUtils, ScStrUtils, SepiOrdTypes, SepiStrTypes,
  SepiArrayTypes, SepiMembers, uPSUtils, SepiPSUtils;

procedure ImportType(PSCompiler: TPSPascalCompiler;
  SepiType: TSepiType); forward;

const {don't localize}
  /// Identificateurs Delphi que PS reconnaît comme mots-clefs
  InvalidIdentifiers: array[0..3] of string = (
    'Chr', 'Ord', 'High', 'Low'
  );

{-----------------}
{ Global routines }
{-----------------}

{*
  Forward un type pointeur, classe ou interface
  @param PSCompiler    Compilateur Pascal Script
  @param ForwardType   Type à forwarder
*}
procedure ImportForwardType(PSCompiler: TPSPascalCompiler;
  ForwardType: TSepiType);
var
  BaseType: TPSBaseType;
begin
  if PSCompiler.FindType(ForwardType.Name) <> nil then
    Exit;

  // Pascal Script doesn't support pointers: we use integers instead
  if ForwardType is TSepiPointerType then
    BaseType := btU32
  else if ForwardType is TSepiClass then
    BaseType := btClass
  else
  begin
    Assert(False);
    Exit;
  end;

  PSCompiler.AddType(ForwardType.Name, BaseType);
end;

{*
  Convertit un paramètre en sa déclaration Delphi
  @param PSCompiler   Compilateur Pascal Script
  @param Param        Paramètre à convertir
  @return Déclaration Delphi du paramètre
*}
function ParamToString(PSCompiler: TPSPascalCompiler;
  Param: TSepiParam): string;
begin
  with Param do
  begin
    case Kind of
      pkVar: Result := 'var ';
      pkConst: Result := 'const ';
      pkOut: Result := 'out ';
    else
      Result := '';
    end;

    Result := Result + Name;
    if AnsiIndexText(Name, InvalidIdentifiers) >= 0 then
      Result := Result + '_';
    Result := Result + ': ';

    if OpenArray then
      Result := Result + 'array of ';

    if ParamType = nil then
      Result := Result + 'const'
    else
    begin
      ImportType(PSCompiler, ParamType);
      Result := Result + ParamType.Name;
    end;
  end;
end;

{*
  Convertit une signature en sa déclaration Delphi
  @param PSCompiler   Compilateur Pascal Script
  @param Signature    Signature à convertir
  @return Déclaration Delphi de la signature
*}
function SignatureToString(PSCompiler: TPSPascalCompiler;
  Signature: TSepiSignature; const Name: string = ''): string;
var
  I: Integer;
  StrParam: string;
begin
  Result := MethodKindStrings[Signature.Kind];
  if AnsiStartsText('unit ', Result) then
    Delete(Result, 1, 5);
  if Name <> '' then
    Result := Result + ' ' + Name;

  with Signature do
  begin
    if ParamCount > 0 then
    begin
      Result := Result + '(';
      for I := 0 to ParamCount-1 do
      begin
        if I <> 0 then
          Result := Result + '; ';
        StrParam := ParamToString(PSCompiler, Params[I]);

        if StrParam = '' then
        begin
          Result := '';
          Exit;
        end;

        Result := Result + StrParam;
      end;
      Result := Result + ')';
    end;

    if ReturnType <> nil then
    begin
      // Pascal Script doesn't stand a PChar return type
      ImportType(PSCompiler, ReturnType);
      Result := Result + ': ' + ReturnType.Name;
    end;

    if (Name = '') and (Kind in [mkFunction, mkProcedure]) then
      Result := Result + ' of object';
  end;

  Result := Result + ';';
end;

{*
  Importe un type entier
  @param PSCompiler   Compilateur Pascal Script
  @param IntType      Type à importer
*}
procedure ImportIntegerType(PSCompiler: TPSPascalCompiler;
  IntType: TSepiIntegerType);
begin
  with IntType do
  begin
    case TypeData.OrdType of
      otUByte: PSCompiler.AddType(Name, btU8);
      otSByte: PSCompiler.AddType(Name, btS8);
      otUWord: PSCompiler.AddType(Name, btU16);
      otSWord: PSCompiler.AddType(Name, btS16);
      otULong: PSCompiler.AddType(Name, btU32);
      otSLong: PSCompiler.AddType(Name, btS32);
    end;
  end;
end;

{*
  Importe un type caractère
  @param PSCompiler   Compilateur Pascal Script
  @param CharType     Type à importer
*}
procedure ImportCharType(PSCompiler: TPSPascalCompiler;
  CharType: TSepiCharType);
begin
  with CharType do
  begin
    if IsUnicode then
      PSCompiler.AddType(Name, btWideChar)
    else
      PSCompiler.AddType(Name, btChar);
  end;
end;

{*
  Importe un type entier 64 bits
  @param PSCompiler   Compilateur Pascal Script
  @param Int64Type    Type à importer
*}
procedure ImportInt64Type(PSCompiler: TPSPascalCompiler;
  Int64Type: TSepiInt64Type);
begin
  PSCompiler.AddType(Int64Type.Name, btS64);
end;

{*
  Importe un type flottant
  @param PSCompiler   Compilateur Pascal Script
  @param FloatType    Type à importer
*}
procedure ImportFloatType(PSCompiler: TPSPascalCompiler;
  FloatType: TSepiFloatType);
begin
  with FloatType do
  begin
    case FloatType of
      ftSingle:   PSCompiler.AddType(Name, btSingle);
      ftDouble:   PSCompiler.AddType(Name, btDouble);
      ftExtended: PSCompiler.AddType(Name, btExtended);
      ftCurr:     PSCompiler.AddType(Name, btCurrency);
    end;
  end;
end;

{*
  Importe un type booléen
  @param PSCompiler    Compilateur Pascal Script
  @param BooleanType   Type à importer
*}
procedure ImportBooleanType(PSCompiler: TPSPascalCompiler;
  BooleanType: TSepiBooleanType);
begin
  with BooleanType do
  begin
    case BooleanKind of
      bkBoolean:  PSCompiler.AddTypeCopyN(Name, 'Boolean');
      bkLongBool: PSCompiler.AddTypeCopyN(Name, 'LongBool');
      // Pascal Script doesn't support other boolean types
    end;
  end;
end;

{*
  Importe un type énuméré
  @param PSCompiler   Compilateur Pascal Script
  @param EnumType     Type à importer
*}
procedure ImportEnumType(PSCompiler: TPSPascalCompiler;
  EnumType: TSepiEnumType);
var
  Str: string;
  Value: Integer;
begin
  with EnumType do
  begin
    if BaseType = EnumType then
    begin
      Str := '(';
      for Value := MinValue to MaxValue-1 do
        Str := Str + Names[Value] + ', ';
      Str := Str + Names[MaxValue];
      Str := Str + ')';

      PSCompiler.AddTypeS(Name, Str);
    end else
    begin
      // Pascal Script doesn't support base-typed enumerated types
      ImportType(PSCompiler, BaseType);
      PSCompiler.AddTypeCopyN(Name, BaseType.Name);
    end;
  end;
end;

{*
  Importe un type ensemble
  @param PSCompiler   Compilateur Pascal Script
  @param SetType      Type à importer
*}
procedure ImportSetType(PSCompiler: TPSPascalCompiler;
  SetType: TSepiSetType);
begin
  with SetType, TPSSetType(PSCompiler.AddTypeS(Name, 'set of Byte')) do
    SetType := PSCompiler.FindType(CompType.Name);
end;

{*
  Importe un type variant
  @param PSCompiler    Compilateur Pascal Script
  @param VariantType   Type à importer
*}
procedure ImportVariantType(PSCompiler: TPSPascalCompiler;
  VariantType: TSepiVariantType);
begin
  PSCompiler.AddType(VariantType.Name, btVariant);
end;

{*
  Importe un type chaîne courte
  @param PSCompiler        Compilateur Pascal Script
  @param ShortStringType   Type à importer
*}
procedure ImportShortStringType(PSCompiler: TPSPascalCompiler;
  ShortStringType: TSepiShortStringType);
begin
  // As far as I know, Pascal Script doesn't support short strings.
end;

{*
  Importe un type chaîne
  @param PSCompiler   Compilateur Pascal Script
  @param StringType   Type à importer
*}
procedure ImportStringType(PSCompiler: TPSPascalCompiler;
  StringType: TSepiStringType);
begin
  with StringType do
  begin
    if IsUnicode then
      PSCompiler.AddType(Name, btString)
    else
      PSCompiler.AddType(Name, btWideString);
  end;
end;

{*
  Importe un type tableau
  @param PSCompiler   Compilateur Pascal Script
  @param ArrayType    Type à importer
*}
procedure ImportArrayType(PSCompiler: TPSPascalCompiler;
  ArrayType: TSepiStaticArrayType);
var
  Str: string;
begin
  with ArrayType do
  begin
    Str := 'array[';
    Str := Str + IntToStr(LowerBound);
    Str := Str + '..' + IntToStr(HigherBound);
    Str := Str + '] of ' + ElementType.Name;

    PSCompiler.AddTypeS(Name, Str);
  end;
end;

{*
  Importe un type tableau dynamique
  @param PSCompiler     Compilateur Pascal Script
  @param DynArrayType   Type à importer
*}
procedure ImportDynArrayType(PSCompiler: TPSPascalCompiler;
  DynArrayType: TSepiDynArrayType);
begin
  with DynArrayType do
    PSCompiler.AddTypeS(Name, 'array of ' + ElementType.Name);
end;

{*
  Importe un type record
  @param PSCompiler   Compilateur Pascal Script
  @param RecordType   Type à importer
*}
procedure ImportRecordType(PSCompiler: TPSPascalCompiler;
  RecordType: TSepiRecordType);
var
  I: Integer;
  Child: TSepiComponent;
begin
  with RecordType, TPSRecordType(PSCompiler.AddType(Name, btRecord)) do
  begin
    for I := 0 to ChildCount-1 do
    begin
      Child := Children[I];
      if not (Child is TSepiField) then
        Continue;

      with TSepiField(Child), AddRecVal do
      begin
        FieldOrgName := Name;
        aType := PSCompiler.FindType(FieldType.Name);
      end;
    end;
  end;
end;

{*
  Importe un type interface
  @param PSCompiler   Compilateur Pascal Script
  @param IntfType     Type à importer
*}
procedure ImportInterfaceType(PSCompiler: TPSPascalCompiler;
  IntfType: TSepiInterface);
begin
  PSCompiler.AddInterface(nil, NoGUID, IntfType.Name);
end;

{*
  Importe une meta-classe
  @param PSCompiler   Compilateur Pascal Script
  @param ComponentClass    Component-classe à importer
*}
procedure ImportComponentClass(PSCompiler: TPSPascalCompiler;
  ComponentClass: TSepiComponentClass);
begin
  // Pascal Script doesn't support meta-classes: we use integers instead
  PSCompiler.AddType(ComponentClass.Name, btU32);
end;

{*
  Importe un type référence de méthode
  @param PSCompiler      Compilateur Pascal Script
  @param MethodRefType   Type à importer
*}
procedure ImportMethodRefType(PSCompiler: TPSPascalCompiler;
  MethodRefType: TSepiMethodRefType);
var
  StrSignature: string;
begin
  with MethodRefType do
  begin
    if Signature.CallingConvention = ccRegister then
    begin
      StrSignature := SignatureToString(PSCompiler, Signature);
      if StrSignature <> '' then
        PSCompiler.AddTypeS(Name, StrSignature);
    end;
  end;
end;

{*
  Importe un type
  @param PSCompiler   Compilateur Pascal Script
  @param SepiType     Type à importer
*}
procedure ImportType(PSCompiler: TPSPascalCompiler; SepiType: TSepiType);
begin
  if PSCompiler.FindType(SepiType.Name) <> nil then
    Exit;

  if SepiType is TSepiIntegerType then
    ImportIntegerType(PSCompiler, TSepiIntegerType(SepiType))
  else if SepiType is TSepiCharType then
    ImportCharType(PSCompiler, TSepiCharType(SepiType))
  else if SepiType is TSepiInt64Type then
    ImportInt64Type(PSCompiler, TSepiInt64Type(SepiType))
  else if SepiType is TSepiFloatType then
    ImportFloatType(PSCompiler, TSepiFloatType(SepiType))
  else if SepiType is TSepiBooleanType then
    ImportBooleanType(PSCompiler, TSepiBooleanType(SepiType))
  else if SepiType is TSepiEnumType then
    ImportEnumType(PSCompiler, TSepiEnumType(SepiType))
  else if SepiType is TSepiSetType then
    ImportSetType(PSCompiler, TSepiSetType(SepiType))
  else if SepiType is TSepiVariantType then
    ImportVariantType(PSCompiler, TSepiVariantType(SepiType))
  else if SepiType is TSepiPointerType then
    ImportForwardType(PSCompiler, TSepiPointerType(SepiType))

  else if SepiType is TSepiShortStringType then
    ImportShortStringType(PSCompiler, TSepiShortStringType(SepiType))
  else if SepiType is TSepiStringType then
    ImportStringType(PSCompiler, TSepiStringType(SepiType))

  else if SepiType is TSepiStaticArrayType then
    ImportArrayType(PSCompiler, TSepiStaticArrayType(SepiType))
  else if SepiType is TSepiDynArrayType then
    ImportDynArrayType(PSCompiler, TSepiDynArrayType(SepiType))

  else if SepiType is TSepiRecordType then
    ImportRecordType(PSCompiler, TSepiRecordType(SepiType))
  else if SepiType is TSepiInterface then
    ImportInterfaceType(PSCompiler, TSepiInterface(SepiType))
  else if SepiType is TSepiClass then
    ImportForwardType(PSCompiler, TSepiClass(SepiType))
  else if SepiType is TSepiMetaClass then
    ImportComponentClass(PSCompiler, TSepiMetaClass(SepiType))

  else if SepiType is TSepiMethodRefType then
    ImportMethodRefType(PSCompiler, TSepiMethodRefType(SepiType));
end;

{*
  Importe un alias de type
  @param PSCompiler   Compilateur Pascal Script
  @param TypeAlias    Alias à importer
*}
procedure ImportTypeAlias(PSCompiler: TPSPascalCompiler;
  TypeAlias: TSepiTypeAlias);
begin
  if PSCompiler.FindType(TypeAlias.Name) <> nil then
    Exit;

  with TypeAlias do
    PSCompiler.AddTypeCopyN(Name, Dest.Name);
end;

{*
  Importe une constante
  @param PSCompiler   Compilateur Pascal Script
  @param Constant     Constante à importer
*}
procedure ImportConst(PSCompiler: TPSPascalCompiler;
  Constant: TSepiConstant);
begin
  if PSCompiler.GetConstant(Constant.Name) <> nil then
    Exit;

  with Constant do
  begin
    ImportType(PSCompiler, ConstType);
    with PSCompiler.AddConstantN(Name, ConstType.Name) do
    begin
      case ConstType.Kind of
        tkInteger, tkEnumeration:
          case ConstType.TypeData.OrdType of
            otUByte: Value.tu8 := PByte(ValuePtr)^;
            otSByte: Value.ts8 := PShortInt(ValuePtr)^;
            otUWord: Value.tu16 := PWord(ValuePtr)^;
            otSWord: Value.ts16 := PSmallInt(ValuePtr)^;
            otULong: Value.tu32 := PLongWord(ValuePtr)^;
            otSLong: Value.ts32 := PLongInt(ValuePtr)^;
          end;
        tkChar: Value.tchar := PChar(ValuePtr)^;
        tkWChar: Value.twidechar := PWideChar(ValuePtr)^;
        tkInt64: Value.ts64 := PInt64(ValuePtr)^;
        tkFloat:
          case TSepiFloatType(ConstType).FloatType of
            ftSingle: Value.tsingle := PSingle(ValuePtr)^;
            ftDouble: Value.tdouble := PDouble(ValuePtr)^;
            ftExtended: Value.textended := PExtended(ValuePtr)^;
            ftComp: Free;
            ftCurr: Value.tcurrency := PCurrency(ValuePtr)^;
          end;
        tkLString: Value.tstring := ValuePtr;
        tkWString: Value.twidestring := ValuePtr;
      else
        Free;
      end;
    end;
  end;
end;

{*
  Importe une variable globale
  @param PSCompiler   Compilateur Pascal Script
  @param Variable     Variable à importer
*}
procedure ImportVariable(PSCompiler: TPSPascalCompiler;
  Variable: TSepiVariable);
var
  UpperName: string;
  Hash, I: Integer;
begin
  if Variable.VarType is TSepiMethodRefType then
    Exit;

  // A pity TPSPascalCompiler doesn't provide a FindVar method :-(
  UpperName := FastUppercase(Variable.Name);
  Hash := MakeHash(UpperName);
  for I := 0 to PSCompiler.GetVarCount-1 do
  begin
    with PSCompiler.GetVar(I) do
      if (NameHash = Hash) and (Name = UpperName) then
        Exit;
  end;

  with Variable do
  begin
    ImportType(PSCompiler, VarType);
    PSCompiler.AddUsedPTRVariableN(Name, VarType.Name);
  end;
end;

{*
  Importe une routine
  @param PSCompiler   Compilateur Pascal Script
  @param Routine      Routine à importer
*}
procedure ImportRoutine(PSCompiler: TPSPascalCompiler;
  Routine: TSepiMethod);
var
  StrSignature, PSName, SecondName: string;
begin
  with Routine do
  begin
    if PSCompiler.FindProc(Name) <> InvalidVal then
      Exit;

    StrSignature := SignatureToString(PSCompiler, Signature, '%s');
    if StrSignature <> '' then
    begin
      if HandleOverloaded(Name, PSName, SecondName) then
        PSCompiler.AddDelphiFunction(Format(StrSignature, [SecondName]));
      PSCompiler.AddDelphiFunction(Format(StrSignature, [PSName]));
    end;
  end;
end;

{*
  Importe une interface
  @param PSCompiler   Compilateur Pascal Script
  @param SepiIntf     Interface à importer
  @return Interface Pascal Script importée
*}
function ImportInterface(PSCompiler: TPSPascalCompiler;
  SepiIntf: TSepiInterface): TPSInterface;
var
  ParentIntf: TPSInterface;
  PSType: TPSInterfaceType;
  I: Integer;
  Child: TSepiComponent;
  StrSignature: string;
begin
  Result := PSCompiler.FindInterface(SepiIntf.Name);
  if Result <> nil then
    Exit;

  ImportType(PSCompiler, SepiIntf);

  if SepiIntf.Parent = nil then
    ParentIntf := nil
  else
    ParentIntf := ImportInterface(PSCompiler, SepiIntf.Parent);

  PSType := TPSInterfaceType(PSCompiler.FindType(SepiIntf.Name));
  PSType.Intf := PSCompiler.AddInterface(
    ParentIntf, SepiIntf.GUID, SepiIntf.Name);

  with SepiIntf, PSType.Intf do
  begin
    for I := 0 to ChildCount-1 do
    begin
      Child := Children[I];

      if Child is TSepiMethod then
      begin
        with TSepiMethod(Child) do
        begin
          StrSignature := SignatureToString(PSCompiler, Signature, Name);
          if StrSignature <> '' then
            RegisterMethod(StrSignature,
              CallingConvSepiToPS[Signature.CallingConvention]);
        end;
      end;
    end;
  end;
end;

{*
  Importe une classe
  @param PSCompiler   Compilateur Pascal Script
  @param SepiClass    Classe à importer
  @return Classe Pascal Script importée
*}
function ImportClass(PSCompiler: TPSPascalCompiler;
  SepiClass: TSepiClass): TPSCompileTimeClass;
var
  ParentClass: TPSCompileTimeClass;
  PSType: TPSClassType;
  I, J: Integer;
  Child: TSepiComponent;
  Access: TPSPropType;
  StrSignature, PSName, SecondName, StrPropType: string;
begin
  Result := PSCompiler.FindClass(SepiClass.Name);
  if Result <> nil then
    Exit;
  ImportType(PSCompiler, SepiClass);

  if SepiClass.Parent = nil then
    ParentClass := nil
  else
    ParentClass := ImportClass(PSCompiler, SepiClass.Parent);

  PSType := TPSClassType(PSCompiler.FindType(SepiClass.Name));
  PSType.Cl := PSCompiler.AddClass(ParentClass, SepiClass.DelphiClass);

  with SepiClass, PSType.Cl do
  begin
    for I := 0 to ChildCount-1 do
    begin
      Child := Children[I];
      if not (Child.Visibility in [mvPublic, mvPublished]) then
        Continue;

      // FIELD
      if Child is TSepiField then
      begin
        with TSepiField(Child) do
        begin
          if not (FieldType.Kind in [tkArray, tkDynArray]) then
          begin
            ImportType(PSCompiler, FieldType);
            RegisterProperty(Name, FieldType.Name, iptRW);
          end;
        end;
      end else

      // METHOD
      if Child is TSepiMethod then
      begin
        with TSepiMethod(Child) do
        begin
          if (FirstDeclaration or
            (not (InheritedMethod.Visibility in [mvPublic, mvPublished]))) then
          begin
            StrSignature := SignatureToString(PSCompiler, Signature, '%s');
            if StrSignature <> '' then
            begin
              if HandleOverloaded(Name, PSName, SecondName) then
                RegisterMethod(Format(StrSignature, [SecondName]));
              RegisterMethod(Format(StrSignature, [PSName]));
            end;
          end;
        end;
      end else

      // PROPERTY
      if Child is TSepiProperty then
      begin
        with TSepiProperty(Child) do
        begin
          if ReadAccess.Kind = pakNone then
            Access := iptW
          else if WriteAccess.Kind = pakNone then
            Access := iptR
          else
            Access := iptRW;

          if SignatureToString(PSCompiler, Signature) = '' then
            Continue;
          StrPropType := PropType.Name;
          for J := 0 to Signature.ParamCount-1 do
            StrPropType := StrPropType + ' ' +
              Signature.Params[J].ParamType.Name;

          RegisterProperty(Name, StrPropType, Access);
          if IsDefault then
            SetDefaultPropery(Name);
        end;
      end;
    end;
  end;
end;

{*
  Importe le contenu d'une unité Sepi dans un compilateur Pascal Script
  @param SepiUnit     Unité Sepi
  @param PSCompiler   Compilateur Pascal Script
*}
procedure SepiImportUnitInPSCompiler(SepiUnit: TSepiUnit;
  PSCompiler: TPSPascalCompiler);
var
  I: Integer;
  Child: TSepiComponent;
  PSType: TPSType;
begin
  Child := nil;

  // Forward pointers, classes and interfaces
  for I := 0 to SepiUnit.ChildCount-1 do
  try
    Child := SepiUnit.Children[I];

    if Child is TSepiInterface then
      ImportInterfaceType(PSCompiler, TSepiInterface(Child))
    else if (Child is TSepiPointerType) or (Child is TSepiClass) then
      ImportForwardType(PSCompiler, TSepiType(Child));
  except
    on Error: Exception do
      PSCompiler.MakeWarning(SepiUnit.Name, ewCustomWarning,
        Format('%s (in %s)', [Error.Message, Child.GetFullName]));
  end;

  // Children in this unit
  for I := 0 to SepiUnit.ChildCount-1 do
  try
    Child := SepiUnit.Children[I];

    if Child is TSepiClass then
      ImportClass(PSCompiler, TSepiClass(Child))
    else if Child is TSepiInterface then
      ImportInterface(PSCompiler, TSepiInterface(Child))
    else if Child is TSepiType then
      ImportType(PSCompiler, TSepiType(Child))
    else if Child is TSepiTypeAlias then
      ImportTypeAlias(PSCompiler, TSepiTypeAlias(Child))
    else if Child is TSepiConstant then
      ImportConst(PSCompiler, TSepiConstant(Child))
    else if Child is TSepiVariable then
      ImportVariable(PSCompiler, TSepiVariable(Child))
    else if Child is TSepiMethod then
      ImportRoutine(PSCompiler, TSepiMethod(Child));
  except
    on Error: Exception do
      PSCompiler.MakeWarning(SepiUnit.Name, ewCustomWarning, Error.Message);
  end;

  // Classes and interfaces used from other units which were not imported
  for I := 0 to PSCompiler.GetTypeCount-1 do
  try
    PSType := PSCompiler.GetType(I);

    if PSType is TPSClassType then
    begin
      if TPSClassType(PSType).Cl = nil then
        ImportClass(PSCompiler,
          SepiUnit.Root.FindType(PSType.Name) as TSepiClass);
    end else if PSType is TPSInterfaceType then
      ImportInterface(PSCompiler,
        SepiUnit.Root.FindType(PSType.Name) as TSepiInterface);
  except
    on Error: Exception do
      PSCompiler.MakeWarning(SepiUnit.Name, ewCustomWarning, Error.Message);
  end;
end;

{*
  Charge une unité Sepi et l'importe dans un compilateur Pascal Script
  @param Root         Racine Sepi
  @param UnitName     Nom de l'unité Sepi
  @param PSCompiler   Compilateur Pascal Script
*}
procedure SepiImportUnitInPSCompiler(Root: TSepiRoot;
  const UnitName: string; PSCompiler: TPSPascalCompiler);
begin
  SepiImportUnitInPSCompiler(Root.LoadUnit(UnitName), PSCompiler);
end;

end.

