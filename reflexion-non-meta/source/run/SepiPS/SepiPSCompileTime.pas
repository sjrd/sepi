{*
  Liaison Sepi-PS � la compilation
  @author sjrd
  @version 1.0
*}
unit SepiPSCompileTime;

interface

uses
  SepiMetaUnits, uPSCompiler;

procedure SepiImportUnitInPSCompiler(SepiUnit : TSepiMetaUnit;
  PSCompiler : TPSPascalCompiler); overload;
procedure SepiImportUnitInPSCompiler(Root : TSepiMetaRoot;
  const UnitName : string; PSCompiler : TPSPascalCompiler); overload;

implementation

uses
  SysUtils, TypInfo, StrUtils, ScUtils, ScStrUtils, SepiOrdTypes, SepiStrTypes,
  SepiArrayTypes, SepiCompTypes, uPSUtils, SepiPSUtils;

procedure ImportType(PSCompiler : TPSPascalCompiler;
  SepiType : TSepiType); forward;

const {don't localize}
  /// Identificateurs Delphi que PS reconna�t comme mots-clefs
  InvalidIdentifiers : array[0..3] of string = (
    'Chr', 'Ord', 'High', 'Low'
  );

{-----------------}
{ Global routines }
{-----------------}

{*
  Forward un type pointeur, classe ou interface
  @param PSCompiler    Compilateur Pascal Script
  @param ForwardType   Type � forwarder
*}
procedure ImportForwardType(PSCompiler : TPSPascalCompiler;
  ForwardType : TSepiType);
var BaseType : TPSBaseType;
begin
  if PSCompiler.FindType(ForwardType.Name) <> nil then
    exit;

  // Pascal Script doesn't support pointers: we use integers instead
  if ForwardType is TSepiPointerType then BaseType := btU32 else
  if ForwardType is TSepiClass then BaseType := btClass else
  begin
    Assert(False);
    exit;
  end;

  with ForwardType do
    PSCompiler.AddType(Name, BaseType).DeclareUnit := OwningUnit.Name;
end;

{*
  Convertit un param�tre en sa d�claration Delphi
  @param PSCompiler   Compilateur Pascal Script
  @param Param        Param�tre � convertir
  @return D�claration Delphi du param�tre
*}
function ParamToString(PSCompiler : TPSPascalCompiler;
  Param : TSepiMetaParam) : string;
begin
  with Param do
  begin
    if pfVar   in Flags then Result := 'var ' else
    if pfConst in Flags then Result := 'const ' else
    if pfOut   in Flags then Result := 'out ' else
    Result := '';

    Result := Result + Name;
    if AnsiIndexText(Name, InvalidIdentifiers) >= 0 then
      Result := Result + '_';
    Result := Result + ': ';

    if pfArray in Flags then
      Result := Result + 'array of ';

    if ParamType = nil then
      Result := Result + 'const' else
    begin
      ImportType(PSCompiler, ParamType);
      Result := Result + ParamType.Name;
    end;
  end;
end;

{*
  Convertit une signature en sa d�claration Delphi
  @param PSCompiler   Compilateur Pascal Script
  @param Signature    Signature � convertir
  @return D�claration Delphi de la signature
*}
function SignatureToString(PSCompiler : TPSPascalCompiler;
  Signature : TSepiMethodSignature; const Name : string = '') : string;
var I : integer;
    StrParam : string;
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
        if I <> 0 then Result := Result + '; ';
        StrParam := ParamToString(PSCompiler, Params[I]);

        if StrParam = '' then
        begin
          Result := '';
          exit;
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
  @param IntType      Type � importer
*}
procedure ImportIntegerType(PSCompiler : TPSPascalCompiler;
  IntType : TSepiIntegerType);
var PSType : TPSType;
begin
  PSType := nil;
  with IntType do
  begin
    case TypeData.OrdType of
      otUByte : PSType := PSCompiler.AddType(Name, btU8);
      otSByte : PSType := PSCompiler.AddType(Name, btS8);
      otUWord : PSType := PSCompiler.AddType(Name, btU16);
      otSWord : PSType := PSCompiler.AddType(Name, btS16);
      otULong : PSType := PSCompiler.AddType(Name, btU32);
      otSLong : PSType := PSCompiler.AddType(Name, btS32);
    end;

    PSType.DeclareUnit := OwningUnit.Name;
  end;
end;

{*
  Importe un type caract�re
  @param PSCompiler   Compilateur Pascal Script
  @param CharType     Type � importer
*}
procedure ImportCharType(PSCompiler : TPSPascalCompiler;
  CharType : TSepiCharType);
begin
  with CharType do
  begin
    if IsUnicode then
      PSCompiler.AddType(Name, btWideChar).DeclareUnit := OwningUnit.Name
    else
      PSCompiler.AddType(Name, btChar).DeclareUnit := OwningUnit.Name;
  end;
end;

{*
  Importe un type entier 64 bits
  @param PSCompiler   Compilateur Pascal Script
  @param Int64Type    Type � importer
*}
procedure ImportInt64Type(PSCompiler : TPSPascalCompiler;
  Int64Type : TSepiInt64Type);
begin
  with Int64Type do
    PSCompiler.AddType(Name, btS64).DeclareUnit := OwningUnit.Name;
end;

{*
  Importe un type flottant
  @param PSCompiler   Compilateur Pascal Script
  @param FloatType    Type � importer
*}
procedure ImportFloatType(PSCompiler : TPSPascalCompiler;
  FloatType : TSepiFloatType);
var PSType : TPSType;
begin
  with FloatType do
  begin
    case FloatType of
      ftSingle   : PSType := PSCompiler.AddType(Name, btSingle);
      ftDouble   : PSType := PSCompiler.AddType(Name, btDouble);
      ftExtended : PSType := PSCompiler.AddType(Name, btExtended);
      ftCurr     : PSType := PSCompiler.AddType(Name, btCurrency);
      else exit;
    end;

    PSType.DeclareUnit := OwningUnit.Name;
  end;
end;

{*
  Importe un type bool�en
  @param PSCompiler    Compilateur Pascal Script
  @param BooleanType   Type � importer
*}
procedure ImportBooleanType(PSCompiler : TPSPascalCompiler;
  BooleanType : TSepiBooleanType);
var PSType : TPSType;
begin
  with BooleanType do
  begin
    case BooleanKind of
      bkBoolean  : PSType := PSCompiler.AddTypeCopyN(Name, 'Boolean');
      bkLongBool : PSType := PSCompiler.AddTypeCopyN(Name, 'LongBool');
      else exit; // Pascal Script doesn't support other boolean types
    end;
    PSType.DeclareUnit := OwningUnit.Name;
  end;
end;

{*
  Importe un type �num�r�
  @param PSCompiler   Compilateur Pascal Script
  @param EnumType     Type � importer
*}
procedure ImportEnumType(PSCompiler : TPSPascalCompiler;
  EnumType : TSepiEnumType);
var Str : string;
    Value : integer;
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

      PSCompiler.AddTypeS(Name, Str).DeclareUnit := OwningUnit.Name;
    end else
    begin
      // Pascal Script doesn't support base-typed enumerated types
      ImportType(PSCompiler, BaseType);
      PSCompiler.AddTypeCopyN(Name, BaseType.Name).DeclareUnit :=
        OwningUnit.Name;
    end;
  end;
end;

{*
  Importe un type ensemble
  @param PSCompiler   Compilateur Pascal Script
  @param SetType      Type � importer
*}
procedure ImportSetType(PSCompiler : TPSPascalCompiler;
  SetType : TSepiSetType);
begin
  with SetType, TPSSetType(PSCompiler.AddTypeS(Name, 'set of Byte')) do
  begin
    DeclareUnit := OwningUnit.Name;
    SetType := PSCompiler.FindType(CompType.Name);
  end;
end;

{*
  Importe un type variant
  @param PSCompiler    Compilateur Pascal Script
  @param VariantType   Type � importer
*}
procedure ImportVariantType(PSCompiler : TPSPascalCompiler;
  VariantType : TSepiVariantType);
begin
  with VariantType do
    PSCompiler.AddType(Name, btVariant).DeclareUnit := OwningUnit.Name;
end;

{*
  Importe un type cha�ne courte
  @param PSCompiler        Compilateur Pascal Script
  @param ShortStringType   Type � importer
*}
procedure ImportShortStringType(PSCompiler : TPSPascalCompiler;
  ShortStringType : TSepiShortStringType);
begin
  // As far as I know, Pascal Script doesn't support short strings.
end;

{*
  Importe un type cha�ne
  @param PSCompiler   Compilateur Pascal Script
  @param StringType   Type � importer
*}
procedure ImportStringType(PSCompiler : TPSPascalCompiler;
  StringType : TSepiStringType);
begin
  with StringType do
  begin
    if IsUnicode then
      PSCompiler.AddType(Name, btString).DeclareUnit := OwningUnit.Name
    else
      PSCompiler.AddType(Name, btWideString).DeclareUnit := OwningUnit.Name;
  end;
end;

{*
  Importe un type tableau
  @param PSCompiler   Compilateur Pascal Script
  @param ArrayType    Type � importer
*}
procedure ImportArrayType(PSCompiler : TPSPascalCompiler;
  ArrayType : TSepiArrayType);
var Str : string;
    I : integer;
begin
  with ArrayType do
  begin
    Str := 'array';
    for I := 0 to DimCount-1 do
    begin
      if I = 0 then Str := Str + '[' else Str := Str + ', ';
      Str := Str + IntToStr(MinValues[I]);
      Str := Str + '..' + IntToStr(MaxValues[I]);
    end;
    Str := Str + '] of ' + ElementType.Name;

    PSCompiler.AddTypeS(Name, Str).DeclareUnit := OwningUnit.Name;
  end;
end;

{*
  Importe un type tableau dynamique
  @param PSCompiler     Compilateur Pascal Script
  @param DynArrayType   Type � importer
*}
procedure ImportDynArrayType(PSCompiler : TPSPascalCompiler;
  DynArrayType : TSepiDynArrayType);
begin
  with DynArrayType do
    PSCompiler.AddTypeS(Name, 'array of ' + ElementType.Name).DeclareUnit :=
      OwningUnit.Name;
end;

{*
  Importe un type record
  @param PSCompiler   Compilateur Pascal Script
  @param RecordType   Type � importer
*}
procedure ImportRecordType(PSCompiler : TPSPascalCompiler;
  RecordType : TSepiRecordType);
var I : integer;
    Child : TSepiMeta;
begin
  with RecordType, TPSRecordType(PSCompiler.AddType(Name, btRecord)) do
  begin
    DeclareUnit := OwningUnit.Name;
    for I := 0 to ChildCount-1 do
    begin
      Child := Children[I];
      if not (Child is TSepiMetaField) then
        Continue;

      with TSepiMetaField(Child), AddRecVal do
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
  @param IntfType     Type � importer
*}
procedure ImportInterfaceType(PSCompiler : TPSPascalCompiler;
  IntfType : TSepiInterface);
begin
  with IntfType, PSCompiler.AddInterface(nil, NoGUID, IntfType.Name) do
    aType.DeclareUnit := OwningUnit.Name;
end;

{*
  Importe une meta-classe
  @param PSCompiler   Compilateur Pascal Script
  @param MetaClass    Meta-classe � importer
*}
procedure ImportMetaClass(PSCompiler : TPSPascalCompiler;
  MetaClass : TSepiMetaClass);
begin
  // Pascal Script doesn't support meta-classes: we use integers instead
  with MetaClass do
    PSCompiler.AddType(Name, btU32).DeclareUnit := OwningUnit.Name;
end;

{*
  Importe un type r�f�rence de m�thode
  @param PSCompiler      Compilateur Pascal Script
  @param MethodRefType   Type � importer
*}
procedure ImportMethodRefType(PSCompiler : TPSPascalCompiler;
  MethodRefType : TSepiMethodRefType);
var StrSignature : string;
begin
  with MethodRefType do
  begin
    if Signature.CallingConvention = ccRegister then
    begin
      StrSignature := SignatureToString(PSCompiler, Signature);
      if StrSignature <> '' then
        PSCompiler.AddTypeS(Name, StrSignature).DeclareUnit := OwningUnit.Name;
    end;
  end;
end;

{*
  Importe un type
  @param PSCompiler   Compilateur Pascal Script
  @param SepiType     Type � importer
*}
procedure ImportType(PSCompiler : TPSPascalCompiler; SepiType : TSepiType);
begin
  if PSCompiler.FindType(SepiType.Name) <> nil then
    exit;

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

  else if SepiType is TSepiArrayType then
    ImportArrayType(PSCompiler, TSepiArrayType(SepiType))
  else if SepiType is TSepiDynArrayType then
    ImportDynArrayType(PSCompiler, TSepiDynArrayType(SepiType))

  else if SepiType is TSepiRecordType then
    ImportRecordType(PSCompiler, TSepiRecordType(SepiType))
  else if SepiType is TSepiInterface then
    ImportInterfaceType(PSCompiler, TSepiInterface(SepiType))
  else if SepiType is TSepiClass then
    ImportForwardType(PSCompiler, TSepiClass(SepiType))
  else if SepiType is TSepiMetaClass then
    ImportMetaClass(PSCompiler, TSepiMetaClass(SepiType))

  else if SepiType is TSepiMethodRefType then
    ImportMethodRefType(PSCompiler, TSepiMethodRefType(SepiType));
end;

{*
  Importe un alias de type
  @param PSCompiler   Compilateur Pascal Script
  @param TypeAlias    Alias � importer
*}
procedure ImportTypeAlias(PSCompiler : TPSPascalCompiler;
  TypeAlias : TSepiTypeAlias);
begin
  if PSCompiler.FindType(TypeAlias.Name) <> nil then
    exit;

  with TypeAlias, PSCompiler.AddTypeCopyN(Name, Dest.Name) do
    DeclareUnit := OwningUnit.Name;
end;

{*
  Importe une constante
  @param PSCompiler   Compilateur Pascal Script
  @param Constant     Constante � importer
*}
procedure ImportConst(PSCompiler : TPSPascalCompiler; Constant : TSepiConstant);
begin
  if PSCompiler.GetConstant(Constant.Name) <> nil then
    exit;

  with Constant do
  begin
    ImportType(PSCompiler, ConstType);
    with PSCompiler.AddConstantN(Name, ConstType.Name) do
    begin
      DeclareUnit := OwningUnit.Name;
      case ConstType.Kind of
        tkInteger, tkEnumeration :
          case ConstType.TypeData.OrdType of
            otUByte : Value.tu8  := PByte(ValuePtr)^;
            otSByte : Value.ts8  := PShortInt(ValuePtr)^;
            otUWord : Value.tu16 := PWord(ValuePtr)^;
            otSWord : Value.ts16 := PSmallInt(ValuePtr)^;
            otULong : Value.tu32 := PLongWord(ValuePtr)^;
            otSLong : Value.ts32 := PLongInt(ValuePtr)^;
          end;
        tkChar : Value.tchar := PChar(ValuePtr)^;
        tkWChar : Value.twidechar := PWideChar(ValuePtr)^;
        tkInt64 : Value.ts64 := PInt64(ValuePtr)^;
        tkFloat :
          case TSepiFloatType(ConstType).FloatType of
            ftSingle : Value.tsingle := PSingle(ValuePtr)^;
            ftDouble : Value.tdouble := PDouble(ValuePtr)^;
            ftExtended : Value.textended := PExtended(ValuePtr)^;
            ftComp : Free;
            ftCurr : Value.tcurrency := PCurrency(ValuePtr)^;
          end;
        tkLString : Value.tstring := ValuePtr;
        tkWString : Value.twidestring := ValuePtr;
        else Free;
      end;
    end;
  end;
end;

{*
  Importe une variable globale
  @param PSCompiler   Compilateur Pascal Script
  @param Variable     Variable � importer
*}
procedure ImportVariable(PSCompiler : TPSPascalCompiler;
  Variable : TSepiVariable);
var UpperName : string;
    Hash, I : integer;
begin
  if Variable.VarType is TSepiMethodRefType then
    exit;

  // A pity TPSPascalCompiler doesn't provide a FindVar method :-(
  UpperName := FastUppercase(Variable.Name);
  Hash := MakeHash(UpperName);
  for I := 0 to PSCompiler.GetVarCount-1 do
  begin
    with PSCompiler.GetVar(I) do
      if (NameHash = Hash) and (Name = UpperName) then
        exit;
  end;

  with Variable do
  begin
    ImportType(PSCompiler, VarType);
    with PSCompiler.AddUsedPTRVariableN(Name, VarType.Name) do
      DeclareUnit := OwningUnit.Name;
  end;
end;

{*
  Importe une routine
  @param PSCompiler   Compilateur Pascal Script
  @param Routine      Routine � importer
*}
procedure ImportRoutine(PSCompiler : TPSPascalCompiler;
  Routine : TSepiMetaMethod);
var StrSignature, PSName, SecondName : string;
begin
  with Routine do
  begin
    if PSCompiler.FindProc(Name) <> InvalidVal then
      exit;

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
  @param SepiIntf     Interface � importer
  @return Interface Pascal Script import�e
*}
function ImportInterface(PSCompiler : TPSPascalCompiler;
  SepiIntf : TSepiInterface) : TPSInterface;
var ParentIntf : TPSInterface;
    PSType : TPSInterfaceType;
    I : integer;
    Child : TSepiMeta;
    StrSignature : string;
begin
  Result := PSCompiler.FindInterface(SepiIntf.Name);
  if Result <> nil then
    exit;

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

      if Child is TSepiMetaMethod then with TSepiMetaMethod(Child) do
      begin
        StrSignature := SignatureToString(PSCompiler, Signature, Name);
        if StrSignature <> '' then
          RegisterMethod(StrSignature,
            CallingConvSepiToPS[Signature.CallingConvention]);
      end;
    end;
  end;
end;

{*
  Importe une classe
  @param PSCompiler   Compilateur Pascal Script
  @param SepiClass    Classe � importer
  @return Classe Pascal Script import�e
*}
function ImportClass(PSCompiler : TPSPascalCompiler;
  SepiClass : TSepiClass) : TPSCompileTimeClass;
var ParentClass : TPSCompileTimeClass;
    PSType : TPSClassType;
    I, J : integer;
    Child : TSepiMeta;
    Access : TPSPropType;
    StrSignature, PSName, SecondName, StrPropType : string;
begin
  Result := PSCompiler.FindClass(SepiClass.Name);
  if Result <> nil then
    exit;
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
      if Child is TSepiMetaField then with TSepiMetaField(Child) do
      begin
        if not (FieldType.Kind in [tkArray, tkDynArray]) then
        begin
          ImportType(PSCompiler, FieldType);
          RegisterProperty(Name, FieldType.Name, iptRW);
        end;
      end else

      // METHOD
      if Child is TSepiMetaMethod then with TSepiMetaMethod(Child) do
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
      end else

      // PROPERTY
      if Child is TSepiMetaProperty then with TSepiMetaProperty(Child) do
      begin
        if ReadAccess.Kind = pakNone then Access := iptW else
        if WriteAccess.Kind = pakNone then Access := iptR else
        Access := iptRW;

        if SignatureToString(PSCompiler, Signature) = '' then
          Continue;
        StrPropType := PropType.Name;
        for J := 0 to Signature.ParamCount-1 do
          StrPropType := StrPropType + ' ' + Signature.Params[J].ParamType.Name;

        RegisterProperty(Name, StrPropType, Access);
        if IsDefault then
          SetDefaultPropery(Name);
      end;
    end;
  end;
end;

{*
  Importe le contenu d'une unit� Sepi dans un compilateur Pascal Script
  @param SepiUnit     Unit� Sepi
  @param PSCompiler   Compilateur Pascal Script
*}
procedure SepiImportUnitInPSCompiler(SepiUnit : TSepiMetaUnit;
  PSCompiler : TPSPascalCompiler);
var I : integer;
    Child : TSepiMeta;
    PSType : TPSType;
begin
  Child := nil;

  // Forward pointers, classes and interfaces
  for I := 0 to SepiUnit.ChildCount-1 do
  try
    Child := SepiUnit.Children[I];

    if Child is TSepiInterface then
      ImportInterfaceType(PSCompiler, TSepiInterface(Child)) else
    if (Child is TSepiPointerType) or (Child is TSepiClass) then
      ImportForwardType(PSCompiler, TSepiType(Child));
  except
    on Error : Exception do
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
    else if Child is TSepiMetaMethod then
      ImportRoutine(PSCompiler, TSepiMetaMethod(Child));
  except
    on Error : Exception do
      PSCompiler.MakeWarning(SepiUnit.Name, ewCustomWarning, Error.Message);
  end;

  // Classes and interfaces used from other units which were not imported
  for I := 0 to PSCompiler.GetTypeCount-1 do
  try
    PSType := PSCompiler.GetType(I);

    if PSType is TPSClassType then
    begin
      if TPSClassType(PSType).Cl = nil then
        ImportClass(PSCompiler, SepiUnit.Root.FindType(
          PSType.DeclareUnit+'.'+PSType.Name) as TSepiClass);
    end else
    if PSType is TPSInterfaceType then
      ImportInterface(PSCompiler, SepiUnit.Root.FindType(
        PSType.DeclareUnit+'.'+PSType.Name) as TSepiInterface);
  except
    on Error : Exception do
      PSCompiler.MakeWarning(SepiUnit.Name, ewCustomWarning, Error.Message);
  end;
end;

{*
  Charge une unit� Sepi et l'importe dans un compilateur Pascal Script
  @param Root         Racine Sepi
  @param UnitName     Nom de l'unit� Sepi
  @param PSCompiler   Compilateur Pascal Script
*}
procedure SepiImportUnitInPSCompiler(Root : TSepiMetaRoot;
  const UnitName : string; PSCompiler : TPSPascalCompiler);
begin
  SepiImportUnitInPSCompiler(Root.LoadUnit(UnitName), PSCompiler);
end;

end.

