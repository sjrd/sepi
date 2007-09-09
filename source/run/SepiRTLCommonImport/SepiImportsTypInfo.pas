{*
  Importe l'unité TypInfo dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsTypInfo;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Variants;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTPublishableVariantType = class(TPublishableVariantType)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsEPropertyError = class(EPropertyError)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsEPropertyConvertError = class(EPropertyConvertError)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

{--------------------------------}
{ TPublishableVariantType import }
{--------------------------------}

class function TSepiImportsTPublishableVariantType.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPublishableVariantType));

  with Result do
  begin
    AddInterface(System.TypeInfo(IVarInstanceReference));

    CurrentVisibility := mvProtected;

    AddMethod('GetInstance', nil,
      'function(const V: TVarData): TObject',
      mlkVirtual, True);

    CurrentVisibility := mvPublic;

    AddMethod('GetProperty', @TSepiImportsTPublishableVariantType.GetProperty,
      'function(var Dest: TVarData; const V: TVarData; const Name: string ) : Boolean',
      mlkOverride);
    AddMethod('SetProperty', @TSepiImportsTPublishableVariantType.SetProperty,
      'function(const V: TVarData; const Name: string; const Value: TVarData ) : Boolean',
      mlkOverride);

    Complete;
  end;
end;

{------------------}
{ TTypeInfo import }
{------------------}

function SepiImportTTypeInfo(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TTypeInfo', False, True);

  with Result do
  begin
    AddField('Kind', System.TypeInfo(TTypeKind));
    AddField('Name', System.TypeInfo(ShortString));

    Complete;
  end;
end;

{------------------}
{ TTypeData import }
{------------------}

function SepiImportTTypeData(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TTypeData', True, True);

  with Result do
  begin
    AddFieldAfter('OrdType', System.TypeInfo(TOrdType), '');
    AddFieldAfter('MinValue', System.TypeInfo(Longint), 'OrdType');
    AddFieldAfter('MaxValue', System.TypeInfo(Longint), 'MinValue');
    AddFieldAfter('BaseType', 'PPTypeInfo', 'MaxValue');
    AddFieldAfter('NameList', System.TypeInfo(ShortStringBase), 'BaseType');
    AddFieldAfter('EnumUnitName', System.TypeInfo(ShortStringBase), 'NameList');
    AddFieldAfter('CompType', 'PPTypeInfo', 'OrdType');
    AddFieldAfter('FloatType', System.TypeInfo(TFloatType), '');
    AddFieldAfter('MaxLength', System.TypeInfo(Byte), '');
    AddFieldAfter('ClassType', 'TClass', '');
    AddFieldAfter('ParentInfo', 'PPTypeInfo', 'ClassType');
    AddFieldAfter('PropCount', System.TypeInfo(SmallInt), 'ParentInfo');
    AddFieldAfter('UnitName', System.TypeInfo(ShortStringBase), 'PropCount');
    AddFieldAfter('MethodKind', System.TypeInfo(TMethodKind), '');
    AddFieldAfter('ParamCount', System.TypeInfo(Byte), 'MethodKind');
    AddFieldAfter('ParamList', '$1', 'ParamCount');
    AddFieldAfter('IntfParent', 'PPTypeInfo', '');
    AddFieldAfter('IntfFlags', System.TypeInfo(TIntfFlagsBase), 'IntfParent');
    AddFieldAfter('Guid', 'TGUID', 'IntfFlags');
    AddFieldAfter('IntfUnit', System.TypeInfo(ShortStringBase), 'Guid');
    AddFieldAfter('MinInt64Value', System.TypeInfo(Int64), '');
    AddField('MaxInt64Value', System.TypeInfo(Int64), True);
    AddFieldAfter('elSize', System.TypeInfo(Longint), '');
    AddFieldAfter('elType', 'PPTypeInfo', 'elSize');
    AddFieldAfter('varType', System.TypeInfo(Integer), 'elType');
    AddFieldAfter('elType2', 'PPTypeInfo', 'varType');
    AddFieldAfter('DynUnitName', System.TypeInfo(ShortStringBase), 'elType2');

    Complete;
  end;
end;

{-----------}
{ $2 import }
{-----------}

function SepiImport_2(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '$2', False, True);

  with Result do
  begin

    Complete;
  end;
end;

{------------------}
{ TPropData import }
{------------------}

function SepiImportTPropData(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TPropData', True, True);

  with Result do
  begin
    AddField('PropCount', System.TypeInfo(Word));
    AddField('PropList', '$2');

    Complete;
  end;
end;

{------------------}
{ TPropInfo import }
{------------------}

function SepiImportTPropInfo(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TPropInfo', True, True);

  with Result do
  begin
    AddField('PropType', 'PPTypeInfo');
    AddField('GetProc', 'Pointer');
    AddField('SetProc', 'Pointer');
    AddField('StoredProc', 'Pointer');
    AddField('Index', System.TypeInfo(Integer));
    AddField('Default', System.TypeInfo(Longint));
    AddField('NameIndex', System.TypeInfo(SmallInt));
    AddField('Name', System.TypeInfo(ShortString));

    Complete;
  end;
end;

{-----------------------}
{ EPropertyError import }
{-----------------------}

class function TSepiImportsEPropertyError.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EPropertyError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------------}
{ EPropertyConvertError import }
{------------------------------}

class function TSepiImportsEPropertyConvertError.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EPropertyConvertError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function PropType_0(Instance: TObject; const PropName: string): TTypeKind;
begin
  Result := PropType(Instance, PropName);
end;

function PropType_1(AClass: TClass; const PropName: string): TTypeKind;
begin
  Result := PropType(AClass, PropName);
end;

function PropIsType_0(Instance: TObject; const PropName: string; TypeKind: TTypeKind ) : Boolean;
begin
  Result := PropIsType(Instance, PropName, TypeKind);
end;

function PropIsType_1(AClass: TClass; const PropName: string; TypeKind: TTypeKind ) : Boolean;
begin
  Result := PropIsType(AClass, PropName, TypeKind);
end;

function IsStoredProp_0(Instance: TObject; const PropName: string): Boolean;
begin
  Result := IsStoredProp(Instance, PropName);
end;

function IsPublishedProp_0(Instance: TObject; const PropName: string): Boolean;
begin
  Result := IsPublishedProp(Instance, PropName);
end;

function IsPublishedProp_1(AClass: TClass; const PropName: string): Boolean;
begin
  Result := IsPublishedProp(AClass, PropName);
end;

function GetOrdProp_0(Instance: TObject; const PropName: string): Longint;
begin
  Result := GetOrdProp(Instance, PropName);
end;

procedure SetOrdProp_0(Instance: TObject; const PropName: string; Value: Longint );
begin
  SetOrdProp(Instance, PropName, Value);
end;

function GetEnumProp_0(Instance: TObject; const PropName: string): string;
begin
  Result := GetEnumProp(Instance, PropName);
end;

procedure SetEnumProp_0(Instance: TObject; const PropName: string; const Value: string );
begin
  SetEnumProp(Instance, PropName, Value);
end;

function GetSetProp_0(Instance: TObject; const PropName: string; Brackets: Boolean = False ) : string;
begin
  Result := GetSetProp(Instance, PropName, Brackets);
end;

procedure SetSetProp_0(Instance: TObject; const PropName: string; const Value: string );
begin
  SetSetProp(Instance, PropName, Value);
end;

function GetObjectProp_0(Instance: TObject; const PropName: string; MinClass: TClass = nil ) : TObject;
begin
  Result := GetObjectProp(Instance, PropName, MinClass);
end;

procedure SetObjectProp_0(Instance: TObject; const PropName: string; Value: TObject );
begin
  SetObjectProp(Instance, PropName, Value);
end;

function GetObjectPropClass_0(Instance: TObject; const PropName: string): TClass;
begin
  Result := GetObjectPropClass(Instance, PropName);
end;

function GetStrProp_0(Instance: TObject; const PropName: string): string;
begin
  Result := GetStrProp(Instance, PropName);
end;

procedure SetStrProp_0(Instance: TObject; const PropName: string; const Value: string );
begin
  SetStrProp(Instance, PropName, Value);
end;

function GetWideStrProp_0(Instance: TObject; const PropName: string): WideString;
begin
  Result := GetWideStrProp(Instance, PropName);
end;

procedure SetWideStrProp_0(Instance: TObject; const PropName: string; const Value: WideString );
begin
  SetWideStrProp(Instance, PropName, Value);
end;

function GetFloatProp_0(Instance: TObject; const PropName: string): Extended;
begin
  Result := GetFloatProp(Instance, PropName);
end;

procedure SetFloatProp_0(Instance: TObject; const PropName: string; const Value: Extended );
begin
  SetFloatProp(Instance, PropName, Value);
end;

function GetVariantProp_0(Instance: TObject; const PropName: string): Variant;
begin
  Result := GetVariantProp(Instance, PropName);
end;

procedure SetVariantProp_0(Instance: TObject; const PropName: string; const Value: Variant );
begin
  SetVariantProp(Instance, PropName, Value);
end;

function GetMethodProp_0(Instance: TObject; const PropName: string): TMethod;
begin
  Result := GetMethodProp(Instance, PropName);
end;

procedure SetMethodProp_0(Instance: TObject; const PropName: string; const Value: TMethod );
begin
  SetMethodProp(Instance, PropName, Value);
end;

function GetInt64Prop_0(Instance: TObject; const PropName: string): Int64;
begin
  Result := GetInt64Prop(Instance, PropName);
end;

procedure SetInt64Prop_0(Instance: TObject; const PropName: string; const Value: Int64 );
begin
  SetInt64Prop(Instance, PropName, Value);
end;

function GetInterfaceProp_0(Instance: TObject; const PropName: string): IInterface;
begin
  Result := GetInterfaceProp(Instance, PropName);
end;

procedure SetInterfaceProp_0(Instance: TObject; const PropName: string; const Value: IInterface );
begin
  SetInterfaceProp(Instance, PropName, Value);
end;

function GetDynArrayProp_0(Instance: TObject; const PropName: string): Pointer;
begin
  Result := GetDynArrayProp(Instance, PropName);
end;

procedure SetDynArrayProp_0(Instance: TObject; const PropName: string; const Value: Pointer );
begin
  SetDynArrayProp(Instance, PropName, Value);
end;

function GetPropValue_0(Instance: TObject; const PropName: string; PreferStrings: Boolean = True ) : Variant;
begin
  Result := GetPropValue(Instance, PropName, PreferStrings);
end;

procedure SetPropValue_0(Instance: TObject; const PropName: string; const Value: Variant );
begin
  SetPropValue(Instance, PropName, Value);
end;

function GetPropInfo_0(Instance: TObject; const PropName: string; AKinds: TTypeKinds = [] ) : PPropInfo;
begin
  Result := GetPropInfo(Instance, PropName, AKinds);
end;

function GetPropInfo_1(AClass: TClass; const PropName: string; AKinds: TTypeKinds = [] ) : PPropInfo;
begin
  Result := GetPropInfo(AClass, PropName, AKinds);
end;

function GetPropInfo_2(TypeInfo: PTypeInfo; const PropName: string ) : PPropInfo;
begin
  Result := GetPropInfo(TypeInfo, PropName);
end;

function GetPropInfo_3(TypeInfo: PTypeInfo; const PropName: string; AKinds: TTypeKinds ) : PPropInfo;
begin
  Result := GetPropInfo(TypeInfo, PropName, AKinds);
end;

function GetPropList_0(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; PropList: PPropList ; SortList: Boolean = True ) : Integer;
begin
  Result := GetPropList(TypeInfo, TypeKinds, PropList, SortList);
end;

function GetPropList_1(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
begin
  Result := GetPropList(TypeInfo, PropList);
end;

function GetPropList_2(AObject: TObject; out PropList: PPropList): Integer;
begin
  Result := GetPropList(AObject, PropList);
end;

function IsStoredProp_1(Instance: TObject; PropInfo: PPropInfo): Boolean;
begin
  Result := IsStoredProp(Instance, PropInfo);
end;

function GetPropValue_1(Instance: TObject; PropInfo: PPropInfo; PreferStrings: Boolean = True ) : Variant;
begin
  Result := GetPropValue(Instance, PropInfo, PreferStrings);
end;

procedure SetPropValue_1(Instance: TObject; PropInfo: PPropInfo; const Value: Variant );
begin
  SetPropValue(Instance, PropInfo, Value);
end;

function GetOrdProp_1(Instance: TObject; PropInfo: PPropInfo): Longint;
begin
  Result := GetOrdProp(Instance, PropInfo);
end;

procedure SetOrdProp_1(Instance: TObject; PropInfo: PPropInfo; Value: Longint );
begin
  SetOrdProp(Instance, PropInfo, Value);
end;

function GetEnumProp_1(Instance: TObject; PropInfo: PPropInfo): string;
begin
  Result := GetEnumProp(Instance, PropInfo);
end;

procedure SetEnumProp_1(Instance: TObject; PropInfo: PPropInfo; const Value: string );
begin
  SetEnumProp(Instance, PropInfo, Value);
end;

function GetSetProp_1(Instance: TObject; PropInfo: PPropInfo; Brackets: Boolean = False ) : string;
begin
  Result := GetSetProp(Instance, PropInfo, Brackets);
end;

procedure SetSetProp_1(Instance: TObject; PropInfo: PPropInfo; const Value: string );
begin
  SetSetProp(Instance, PropInfo, Value);
end;

function GetObjectProp_1(Instance: TObject; PropInfo: PPropInfo; MinClass: TClass = nil ) : TObject;
begin
  Result := GetObjectProp(Instance, PropInfo, MinClass);
end;

procedure SetObjectProp_1(Instance: TObject; PropInfo: PPropInfo; Value: TObject ; ValidateClass: Boolean = True );
begin
  SetObjectProp(Instance, PropInfo, Value, ValidateClass);
end;

function GetObjectPropClass_1(Instance: TObject; PropInfo: PPropInfo): TClass;
begin
  Result := GetObjectPropClass(Instance, PropInfo);
end;

function GetObjectPropClass_2(PropInfo: PPropInfo): TClass;
begin
  Result := GetObjectPropClass(PropInfo);
end;

function GetStrProp_1(Instance: TObject; PropInfo: PPropInfo): string;
begin
  Result := GetStrProp(Instance, PropInfo);
end;

procedure SetStrProp_1(Instance: TObject; PropInfo: PPropInfo; const Value: string );
begin
  SetStrProp(Instance, PropInfo, Value);
end;

function GetWideStrProp_1(Instance: TObject; PropInfo: PPropInfo): WideString;
begin
  Result := GetWideStrProp(Instance, PropInfo);
end;

procedure SetWideStrProp_1(Instance: TObject; PropInfo: PPropInfo; const Value: WideString );
begin
  SetWideStrProp(Instance, PropInfo, Value);
end;

function GetFloatProp_1(Instance: TObject; PropInfo: PPropInfo): Extended;
begin
  Result := GetFloatProp(Instance, PropInfo);
end;

procedure SetFloatProp_1(Instance: TObject; PropInfo: PPropInfo; const Value: Extended );
begin
  SetFloatProp(Instance, PropInfo, Value);
end;

function GetVariantProp_1(Instance: TObject; PropInfo: PPropInfo): Variant;
begin
  Result := GetVariantProp(Instance, PropInfo);
end;

procedure SetVariantProp_1(Instance: TObject; PropInfo: PPropInfo; const Value: Variant );
begin
  SetVariantProp(Instance, PropInfo, Value);
end;

function GetMethodProp_1(Instance: TObject; PropInfo: PPropInfo): TMethod;
begin
  Result := GetMethodProp(Instance, PropInfo);
end;

procedure SetMethodProp_1(Instance: TObject; PropInfo: PPropInfo; const Value: TMethod );
begin
  SetMethodProp(Instance, PropInfo, Value);
end;

function GetInt64Prop_1(Instance: TObject; PropInfo: PPropInfo): Int64;
begin
  Result := GetInt64Prop(Instance, PropInfo);
end;

procedure SetInt64Prop_1(Instance: TObject; PropInfo: PPropInfo; const Value: Int64 );
begin
  SetInt64Prop(Instance, PropInfo, Value);
end;

function GetInterfaceProp_1(Instance: TObject; PropInfo: PPropInfo): IInterface;
begin
  Result := GetInterfaceProp(Instance, PropInfo);
end;

procedure SetInterfaceProp_1(Instance: TObject; PropInfo: PPropInfo; const Value: IInterface );
begin
  SetInterfaceProp(Instance, PropInfo, Value);
end;

function GetDynArrayProp_1(Instance: TObject; PropInfo: PPropInfo): Pointer;
begin
  Result := GetDynArrayProp(Instance, PropInfo);
end;

procedure SetDynArrayProp_1(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer );
begin
  SetDynArrayProp(Instance, PropInfo, Value);
end;

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'TypInfo',
    ['Variants', 'SysUtils']);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTypeKind));

  // Routines
  TSepiOverloadedMethod.Create(Result, 'PropType');
  TSepiMethod.Create(Result, 'OL$PropType$0', @PropType_0,
    'function(Instance: TObject; const PropName: string): TTypeKind');
  TSepiMethod.Create(Result, 'OL$PropType$1', @PropType_1,
    'function(AClass: TClass; const PropName: string): TTypeKind');
  TSepiOverloadedMethod.Create(Result, 'PropIsType');
  TSepiMethod.Create(Result, 'OL$PropIsType$0', @PropIsType_0,
    'function(Instance: TObject; const PropName: string; TypeKind: TTypeKind ) : Boolean');
  TSepiMethod.Create(Result, 'OL$PropIsType$1', @PropIsType_1,
    'function(AClass: TClass; const PropName: string; TypeKind: TTypeKind ) : Boolean');
  TSepiOverloadedMethod.Create(Result, 'IsStoredProp');
  TSepiMethod.Create(Result, 'OL$IsStoredProp$0', @IsStoredProp_0,
    'function(Instance: TObject; const PropName: string): Boolean');
  TSepiOverloadedMethod.Create(Result, 'IsPublishedProp');
  TSepiMethod.Create(Result, 'OL$IsPublishedProp$0', @IsPublishedProp_0,
    'function(Instance: TObject; const PropName: string): Boolean');
  TSepiMethod.Create(Result, 'OL$IsPublishedProp$1', @IsPublishedProp_1,
    'function(AClass: TClass; const PropName: string): Boolean');
  TSepiOverloadedMethod.Create(Result, 'GetOrdProp');
  TSepiMethod.Create(Result, 'OL$GetOrdProp$0', @GetOrdProp_0,
    'function(Instance: TObject; const PropName: string): Longint');
  TSepiOverloadedMethod.Create(Result, 'SetOrdProp');
  TSepiMethod.Create(Result, 'OL$SetOrdProp$0', @SetOrdProp_0,
    'procedure(Instance: TObject; const PropName: string; Value: Longint )');
  TSepiOverloadedMethod.Create(Result, 'GetEnumProp');
  TSepiMethod.Create(Result, 'OL$GetEnumProp$0', @GetEnumProp_0,
    'function(Instance: TObject; const PropName: string): string');
  TSepiOverloadedMethod.Create(Result, 'SetEnumProp');
  TSepiMethod.Create(Result, 'OL$SetEnumProp$0', @SetEnumProp_0,
    'procedure(Instance: TObject; const PropName: string; const Value: string )');
  TSepiOverloadedMethod.Create(Result, 'GetSetProp');
  TSepiMethod.Create(Result, 'OL$GetSetProp$0', @GetSetProp_0,
    'function(Instance: TObject; const PropName: string; Brackets: Boolean = False ) : string');
  TSepiOverloadedMethod.Create(Result, 'SetSetProp');
  TSepiMethod.Create(Result, 'OL$SetSetProp$0', @SetSetProp_0,
    'procedure(Instance: TObject; const PropName: string; const Value: string )');
  TSepiOverloadedMethod.Create(Result, 'GetObjectProp');
  TSepiMethod.Create(Result, 'OL$GetObjectProp$0', @GetObjectProp_0,
    'function(Instance: TObject; const PropName: string; MinClass: TClass = nil ) : TObject');
  TSepiOverloadedMethod.Create(Result, 'SetObjectProp');
  TSepiMethod.Create(Result, 'OL$SetObjectProp$0', @SetObjectProp_0,
    'procedure(Instance: TObject; const PropName: string; Value: TObject )');
  TSepiOverloadedMethod.Create(Result, 'GetObjectPropClass');
  TSepiMethod.Create(Result, 'OL$GetObjectPropClass$0', @GetObjectPropClass_0,
    'function(Instance: TObject; const PropName: string): TClass');
  TSepiOverloadedMethod.Create(Result, 'GetStrProp');
  TSepiMethod.Create(Result, 'OL$GetStrProp$0', @GetStrProp_0,
    'function(Instance: TObject; const PropName: string): string');
  TSepiOverloadedMethod.Create(Result, 'SetStrProp');
  TSepiMethod.Create(Result, 'OL$SetStrProp$0', @SetStrProp_0,
    'procedure(Instance: TObject; const PropName: string; const Value: string )');
  TSepiOverloadedMethod.Create(Result, 'GetWideStrProp');
  TSepiMethod.Create(Result, 'OL$GetWideStrProp$0', @GetWideStrProp_0,
    'function(Instance: TObject; const PropName: string): WideString');
  TSepiOverloadedMethod.Create(Result, 'SetWideStrProp');
  TSepiMethod.Create(Result, 'OL$SetWideStrProp$0', @SetWideStrProp_0,
    'procedure(Instance: TObject; const PropName: string; const Value: WideString )');
  TSepiOverloadedMethod.Create(Result, 'GetFloatProp');
  TSepiMethod.Create(Result, 'OL$GetFloatProp$0', @GetFloatProp_0,
    'function(Instance: TObject; const PropName: string): Extended');
  TSepiOverloadedMethod.Create(Result, 'SetFloatProp');
  TSepiMethod.Create(Result, 'OL$SetFloatProp$0', @SetFloatProp_0,
    'procedure(Instance: TObject; const PropName: string; const Value: Extended )');
  TSepiOverloadedMethod.Create(Result, 'GetVariantProp');
  TSepiMethod.Create(Result, 'OL$GetVariantProp$0', @GetVariantProp_0,
    'function(Instance: TObject; const PropName: string): Variant');
  TSepiOverloadedMethod.Create(Result, 'SetVariantProp');
  TSepiMethod.Create(Result, 'OL$SetVariantProp$0', @SetVariantProp_0,
    'procedure(Instance: TObject; const PropName: string; const Value: Variant )');
  TSepiOverloadedMethod.Create(Result, 'GetMethodProp');
  TSepiMethod.Create(Result, 'OL$GetMethodProp$0', @GetMethodProp_0,
    'function(Instance: TObject; const PropName: string): TMethod');
  TSepiOverloadedMethod.Create(Result, 'SetMethodProp');
  TSepiMethod.Create(Result, 'OL$SetMethodProp$0', @SetMethodProp_0,
    'procedure(Instance: TObject; const PropName: string; const Value: TMethod )');
  TSepiOverloadedMethod.Create(Result, 'GetInt64Prop');
  TSepiMethod.Create(Result, 'OL$GetInt64Prop$0', @GetInt64Prop_0,
    'function(Instance: TObject; const PropName: string): Int64');
  TSepiOverloadedMethod.Create(Result, 'SetInt64Prop');
  TSepiMethod.Create(Result, 'OL$SetInt64Prop$0', @SetInt64Prop_0,
    'procedure(Instance: TObject; const PropName: string; const Value: Int64 )');
  TSepiOverloadedMethod.Create(Result, 'GetInterfaceProp');
  TSepiMethod.Create(Result, 'OL$GetInterfaceProp$0', @GetInterfaceProp_0,
    'function(Instance: TObject; const PropName: string): IInterface');
  TSepiOverloadedMethod.Create(Result, 'SetInterfaceProp');
  TSepiMethod.Create(Result, 'OL$SetInterfaceProp$0', @SetInterfaceProp_0,
    'procedure(Instance: TObject; const PropName: string; const Value: IInterface )');
  TSepiOverloadedMethod.Create(Result, 'GetDynArrayProp');
  TSepiMethod.Create(Result, 'OL$GetDynArrayProp$0', @GetDynArrayProp_0,
    'function(Instance: TObject; const PropName: string): Pointer');
  TSepiOverloadedMethod.Create(Result, 'SetDynArrayProp');
  TSepiMethod.Create(Result, 'OL$SetDynArrayProp$0', @SetDynArrayProp_0,
    'procedure(Instance: TObject; const PropName: string; const Value: Pointer )');
  TSepiOverloadedMethod.Create(Result, 'GetPropValue');
  TSepiMethod.Create(Result, 'OL$GetPropValue$0', @GetPropValue_0,
    'function(Instance: TObject; const PropName: string; PreferStrings: Boolean = True ) : Variant');
  TSepiOverloadedMethod.Create(Result, 'SetPropValue');
  TSepiMethod.Create(Result, 'OL$SetPropValue$0', @SetPropValue_0,
    'procedure(Instance: TObject; const PropName: string; const Value: Variant )');
  TSepiMethod.Create(Result, 'FreeAndNilProperties', @FreeAndNilProperties,
    'procedure(AObject: TObject)');

  // Types
  TSepiImportsTPublishableVariantType.SepiImport(Result);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTypeKinds));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOrdType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFloatType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMethodKind));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TParamFlag));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TParamFlags));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TParamFlagsBase));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TIntfFlag));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TIntfFlags));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TIntfFlagsBase));

  // Constants
  //TSepiConstant.Create(Result, 'tkAny', tkAny);
  //TSepiConstant.Create(Result, 'tkMethods', tkMethods);
  //TSepiConstant.Create(Result, 'tkProperties', tkProperties);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(ShortStringBase));
  TSepiPointerType.Create(Result, 'PPTypeInfo', 'PTypeInfo', True);
  TSepiPointerType.Create(Result, 'PTypeInfo', 'TTypeInfo', True);
  SepiImportTTypeInfo(Result);
  TSepiPointerType.Create(Result, 'PTypeData', 'TTypeData', True);
  TSepiArrayType.Create(Result, '$1',
    [0, 1023], TypeInfo(Char), True);
  SepiImportTTypeData(Result);
  SepiImport_2(Result);
  SepiImportTPropData(Result);
  TSepiPointerType.Create(Result, 'PPropInfo', 'TPropInfo', True);
  SepiImportTPropInfo(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPropInfoProc));
  TSepiPointerType.Create(Result, 'PPropList', 'TPropList', True);
  TSepiArrayType.Create(Result, 'TPropList',
    [0, 16379], 'PPropInfo', True);
  TSepiImportsEPropertyError.SepiImport(Result);
  TSepiImportsEPropertyConvertError.SepiImport(Result);

  // Routines
  TSepiMethod.Create(Result, 'GetTypeData', @GetTypeData,
    'function(TypeInfo: PTypeInfo): PTypeData');
  TSepiMethod.Create(Result, 'GetEnumName', @GetEnumName,
    'function(TypeInfo: PTypeInfo; Value: Integer): string');
  TSepiMethod.Create(Result, 'GetEnumValue', @GetEnumValue,
    'function(TypeInfo: PTypeInfo; const Name: string): Integer');
  TSepiOverloadedMethod.Create(Result, 'GetPropInfo');
  TSepiMethod.Create(Result, 'OL$GetPropInfo$0', @GetPropInfo_0,
    'function(Instance: TObject; const PropName: string; AKinds: TTypeKinds = [] ) : PPropInfo');
  TSepiMethod.Create(Result, 'OL$GetPropInfo$1', @GetPropInfo_1,
    'function(AClass: TClass; const PropName: string; AKinds: TTypeKinds = [] ) : PPropInfo');
  TSepiMethod.Create(Result, 'OL$GetPropInfo$2', @GetPropInfo_2,
    'function(TypeInfo: PTypeInfo; const PropName: string ) : PPropInfo');
  TSepiMethod.Create(Result, 'OL$GetPropInfo$3', @GetPropInfo_3,
    'function(TypeInfo: PTypeInfo; const PropName: string; AKinds: TTypeKinds ) : PPropInfo');
  TSepiMethod.Create(Result, 'GetPropInfos', @GetPropInfos,
    'procedure(TypeInfo: PTypeInfo; PropList: PPropList)');
  TSepiOverloadedMethod.Create(Result, 'GetPropList');
  TSepiMethod.Create(Result, 'OL$GetPropList$0', @GetPropList_0,
    'function(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; PropList: PPropList ; SortList: Boolean = True ) : Integer');
  TSepiMethod.Create(Result, 'OL$GetPropList$1', @GetPropList_1,
    'function(TypeInfo: PTypeInfo; out PropList: PPropList): Integer');
  TSepiMethod.Create(Result, 'OL$GetPropList$2', @GetPropList_2,
    'function(AObject: TObject; out PropList: PPropList): Integer');
  TSepiMethod.Create(Result, 'SortPropList', @SortPropList,
    'procedure(PropList: PPropList; PropCount: Integer)');
  TSepiMethod.Create(Result, 'OL$IsStoredProp$1', @IsStoredProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): Boolean');
  TSepiMethod.Create(Result, 'OL$GetPropValue$1', @GetPropValue_1,
    'function(Instance: TObject; PropInfo: PPropInfo; PreferStrings: Boolean = True ) : Variant');
  TSepiMethod.Create(Result, 'OL$SetPropValue$1', @SetPropValue_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: Variant )');
  TSepiMethod.Create(Result, 'OL$GetOrdProp$1', @GetOrdProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): Longint');
  TSepiMethod.Create(Result, 'OL$SetOrdProp$1', @SetOrdProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; Value: Longint )');
  TSepiMethod.Create(Result, 'OL$GetEnumProp$1', @GetEnumProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): string');
  TSepiMethod.Create(Result, 'OL$SetEnumProp$1', @SetEnumProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: string )');
  TSepiMethod.Create(Result, 'OL$GetSetProp$1', @GetSetProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo; Brackets: Boolean = False ) : string');
  TSepiMethod.Create(Result, 'OL$SetSetProp$1', @SetSetProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: string )');
  TSepiMethod.Create(Result, 'OL$GetObjectProp$1', @GetObjectProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo; MinClass: TClass = nil ) : TObject');
  TSepiMethod.Create(Result, 'OL$SetObjectProp$1', @SetObjectProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; Value: TObject ; ValidateClass: Boolean = True )');
  TSepiMethod.Create(Result, 'OL$GetObjectPropClass$1', @GetObjectPropClass_1,
    'function(Instance: TObject; PropInfo: PPropInfo): TClass');
  TSepiMethod.Create(Result, 'OL$GetObjectPropClass$2', @GetObjectPropClass_2,
    'function(PropInfo: PPropInfo): TClass');
  TSepiMethod.Create(Result, 'OL$GetStrProp$1', @GetStrProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): string');
  TSepiMethod.Create(Result, 'OL$SetStrProp$1', @SetStrProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: string )');
  TSepiMethod.Create(Result, 'OL$GetWideStrProp$1', @GetWideStrProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): WideString');
  TSepiMethod.Create(Result, 'OL$SetWideStrProp$1', @SetWideStrProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: WideString )');
  TSepiMethod.Create(Result, 'OL$GetFloatProp$1', @GetFloatProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): Extended');
  TSepiMethod.Create(Result, 'OL$SetFloatProp$1', @SetFloatProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: Extended )');
  TSepiMethod.Create(Result, 'OL$GetVariantProp$1', @GetVariantProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): Variant');
  TSepiMethod.Create(Result, 'OL$SetVariantProp$1', @SetVariantProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: Variant )');
  TSepiMethod.Create(Result, 'OL$GetMethodProp$1', @GetMethodProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): TMethod');
  TSepiMethod.Create(Result, 'OL$SetMethodProp$1', @SetMethodProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: TMethod )');
  TSepiMethod.Create(Result, 'OL$GetInt64Prop$1', @GetInt64Prop_1,
    'function(Instance: TObject; PropInfo: PPropInfo): Int64');
  TSepiMethod.Create(Result, 'OL$SetInt64Prop$1', @SetInt64Prop_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: Int64 )');
  TSepiMethod.Create(Result, 'OL$GetInterfaceProp$1', @GetInterfaceProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): IInterface');
  TSepiMethod.Create(Result, 'OL$SetInterfaceProp$1', @SetInterfaceProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: IInterface )');
  TSepiMethod.Create(Result, 'OL$GetDynArrayProp$1', @GetDynArrayProp_1,
    'function(Instance: TObject; PropInfo: PPropInfo): Pointer');
  TSepiMethod.Create(Result, 'OL$SetDynArrayProp$1', @SetDynArrayProp_1,
    'procedure(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer )');

  // Global variables
  TSepiArrayType.Create(Result, '$3',
    [Integer(Low(Boolean)), Integer(High(Boolean))], TypeInfo(string), True);
  TSepiVariable.Create(Result, 'BooleanIdents',
     BooleanIdents, '$3');
  TSepiVariable.Create(Result, 'DotSep',
     DotSep, TypeInfo(string));

  // Routines
  TSepiMethod.Create(Result, 'SetToString', @SetToString,
    'function(PropInfo: PPropInfo; Value: Integer; Brackets: Boolean = False): string');
  TSepiMethod.Create(Result, 'StringToSet', @StringToSet,
    'function(PropInfo: PPropInfo; const Value: string): Integer');
  TSepiMethod.Create(Result, 'GetSetElementName', @GetSetElementName,
    'function(TypeInfo: PTypeInfo; Value: Integer): string');
  TSepiMethod.Create(Result, 'GetSetElementValue', @GetSetElementValue,
    'function(TypeInfo: PTypeInfo; const Name: string): Integer');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('TypInfo', ImportUnit);
end.

