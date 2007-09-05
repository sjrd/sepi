{*
  Importe l'unité ConvUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsConvUtils;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ConvUtils;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEConversionError = class(EConversionError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTConvTypeInfo = class(TConvTypeInfo)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTConvTypeFactor = class(TConvTypeFactor)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTConvTypeProcs = class(TConvTypeProcs)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{-------------------------}
{ EConversionError import }
{-------------------------}

class function TSepiImportsEConversionError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EConversionError));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------}
{ TConvTypeInfo import }
{----------------------}

class function TSepiImportsTConvTypeInfo.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TConvTypeInfo));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDescription', System.TypeInfo(string));
    AddField('FConvFamily', System.TypeInfo(TConvFamily));
    AddField('FConvType', System.TypeInfo(TConvType));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTConvTypeInfo.Create,
      'constructor(const AConvFamily: TConvFamily; const ADescription: string)');
    AddMethod('ToCommon', nil,
      'function(const AValue: Double): Double',
      mlkVirtual, True);
    AddMethod('FromCommon', nil,
      'function(const AValue: Double): Double',
      mlkVirtual, True);

    AddProperty('ConvFamily', 'property: TConvFamily',
      'FConvFamily', '');
    AddProperty('ConvType', 'property: TConvType',
      'FConvType', '');
    AddProperty('Description', 'property: string',
      'FDescription', '');

    Complete;
  end;
end;

{------------------------}
{ TConvTypeFactor import }
{------------------------}

class function TSepiImportsTConvTypeFactor.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TConvTypeFactor));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFactor', System.TypeInfo(Double));

    CurrentVisibility := mvProtected;

    AddProperty('Factor', 'property: Double',
      'FFactor', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTConvTypeFactor.Create,
      'constructor(const AConvFamily: TConvFamily; const ADescription: string; const AFactor: Double )');
    AddMethod('ToCommon', @TSepiImportsTConvTypeFactor.ToCommon,
      'function(const AValue: Double): Double',
      mlkOverride);
    AddMethod('FromCommon', @TSepiImportsTConvTypeFactor.FromCommon,
      'function(const AValue: Double): Double',
      mlkOverride);

    Complete;
  end;
end;

{-----------------------}
{ TConvTypeProcs import }
{-----------------------}

class function TSepiImportsTConvTypeProcs.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TConvTypeProcs));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FToCommonProc', 'TConversionProc');
    AddField('FFromCommonProc', 'TConversionProc');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTConvTypeProcs.Create,
      'constructor(const AConvFamily: TConvFamily; const ADescription: string; const AToCommonProc, AFromCommonProc: TConversionProc )');
    AddMethod('ToCommon', @TSepiImportsTConvTypeProcs.ToCommon,
      'function(const AValue: Double): Double',
      mlkOverride);
    AddMethod('FromCommon', @TSepiImportsTConvTypeProcs.FromCommon,
      'function(const AValue: Double): Double',
      mlkOverride);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function Convert_0(const AValue: Double; const AFrom, ATo: TConvType ) : Double;
begin
  Result := Convert(AValue, AFrom, ATo);
end;

function Convert_1(const AValue: Double; const AFrom1, AFrom2, ATo1, ATo2: TConvType ) : Double;
begin
  Result := Convert(AValue, AFrom1, AFrom2, ATo1, ATo2);
end;

function ConvUnitInc_0(const AValue: Double; const AType, AAmountType: TConvType ) : Double;
begin
  Result := ConvUnitInc(AValue, AType, AAmountType);
end;

function ConvUnitInc_1(const AValue: Double; const AType: TConvType; const AAmount: Double ; const AAmountType: TConvType ) : Double;
begin
  Result := ConvUnitInc(AValue, AType, AAmount, AAmountType);
end;

function ConvUnitDec_0(const AValue: Double; const AType, AAmountType: TConvType ) : Double;
begin
  Result := ConvUnitDec(AValue, AType, AAmountType);
end;

function ConvUnitDec_1(const AValue: Double; const AType: TConvType; const AAmount: Double ; const AAmountType: TConvType ) : Double;
begin
  Result := ConvUnitDec(AValue, AType, AAmount, AAmountType);
end;

function RegisterConversionType_0(const AFamily: TConvFamily; const ADescription: string ; const AFactor: Double ) : TConvType;
begin
  Result := RegisterConversionType(AFamily, ADescription, AFactor);
end;

function RegisterConversionType_1(const AFamily: TConvFamily; const ADescription: string ; const AToCommonProc, AFromCommonProc : TConversionProc ) : TConvType;
begin
  Result := RegisterConversionType(AFamily, ADescription, AToCommonProc, AFromCommonProc);
end;

function DescriptionToConvType_0(const ADescription: string; out AType: TConvType ) : Boolean;
begin
  Result := DescriptionToConvType(ADescription, AType);
end;

function DescriptionToConvType_1(const AFamily: TConvFamily; const ADescription: string ; out AType: TConvType ) : Boolean;
begin
  Result := DescriptionToConvType(AFamily, ADescription, AType);
end;

function ConvTypeToFamily_0(const AType: TConvType): TConvFamily;
begin
  Result := ConvTypeToFamily(AType);
end;

function TryConvTypeToFamily_0(const AType: TConvType; out AFamily: TConvFamily ) : Boolean;
begin
  Result := TryConvTypeToFamily(AType, AFamily);
end;

function ConvTypeToFamily_1(const AFrom, ATo: TConvType): TConvFamily;
begin
  Result := ConvTypeToFamily(AFrom, ATo);
end;

function TryConvTypeToFamily_1(const AFrom, ATo: TConvType; out AFamily: TConvFamily ) : Boolean;
begin
  Result := TryConvTypeToFamily(AFrom, ATo, AFamily);
end;

procedure RaiseConversionError_0(const AText: string);
begin
  RaiseConversionError(AText);
end;

procedure RaiseConversionError_1(const AText: string; const AArgs: array of const );
begin
  RaiseConversionError(AText, AArgs);
end;

function RegisterConversionType_2(AConvTypeInfo: TConvTypeInfo; out AType: TConvType ) : Boolean;
begin
  Result := RegisterConversionType(AConvTypeInfo, AType);
end;

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ConvUtils',
    ['SysUtils', 'Math', 'Types']);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConvFamily));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConvType));
  TSepiMethodRefType.Create(Result, 'TConversionProc',
    'function(const AValue: Double): Double');
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConvTypeArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConvFamilyArray));

  // Routines
  TSepiMetaOverloadedMethod.Create(Result, 'Convert');
  TSepiMetaMethod.Create(Result, 'OL$Convert$0', @Convert_0,
    'function(const AValue: Double; const AFrom, ATo: TConvType ) : Double');
  TSepiMetaMethod.Create(Result, 'OL$Convert$1', @Convert_1,
    'function(const AValue: Double; const AFrom1, AFrom2, ATo1, ATo2: TConvType ) : Double');
  TSepiMetaMethod.Create(Result, 'ConvertFrom', @ConvertFrom,
    'function(const AFrom: TConvType; const AValue: Double): Double');
  TSepiMetaMethod.Create(Result, 'ConvertTo', @ConvertTo,
    'function(const AValue: Double; const ATo: TConvType): Double');
  TSepiMetaMethod.Create(Result, 'ConvUnitAdd', @ConvUnitAdd,
    'function(const AValue1: Double; const AType1: TConvType; const AValue2: Double ; const AType2, AResultType: TConvType ) : Double');
  TSepiMetaMethod.Create(Result, 'ConvUnitDiff', @ConvUnitDiff,
    'function(const AValue1: Double; const AType1: TConvType; const AValue2: Double ; const AType2, AResultType: TConvType ) : Double');
  TSepiMetaOverloadedMethod.Create(Result, 'ConvUnitInc');
  TSepiMetaMethod.Create(Result, 'OL$ConvUnitInc$0', @ConvUnitInc_0,
    'function(const AValue: Double; const AType, AAmountType: TConvType ) : Double');
  TSepiMetaMethod.Create(Result, 'OL$ConvUnitInc$1', @ConvUnitInc_1,
    'function(const AValue: Double; const AType: TConvType; const AAmount: Double ; const AAmountType: TConvType ) : Double');
  TSepiMetaOverloadedMethod.Create(Result, 'ConvUnitDec');
  TSepiMetaMethod.Create(Result, 'OL$ConvUnitDec$0', @ConvUnitDec_0,
    'function(const AValue: Double; const AType, AAmountType: TConvType ) : Double');
  TSepiMetaMethod.Create(Result, 'OL$ConvUnitDec$1', @ConvUnitDec_1,
    'function(const AValue: Double; const AType: TConvType; const AAmount: Double ; const AAmountType: TConvType ) : Double');
  TSepiMetaMethod.Create(Result, 'ConvUnitWithinPrevious', @ConvUnitWithinPrevious,
    'function(const AValue, ATest: Double; const AType: TConvType ; const AAmount: Double ; const AAmountType: TConvType ) : Boolean');
  TSepiMetaMethod.Create(Result, 'ConvUnitWithinNext', @ConvUnitWithinNext,
    'function(const AValue, ATest: Double; const AType: TConvType; const AAmount: Double ; const AAmountType: TConvType ) : Boolean');
  TSepiMetaMethod.Create(Result, 'ConvUnitCompareValue', @ConvUnitCompareValue,
    'function(const AValue1: Double; const AType1: TConvType; const AValue2: Double ; const AType2: TConvType ) : TValueRelationship');
  TSepiMetaMethod.Create(Result, 'ConvUnitSameValue', @ConvUnitSameValue,
    'function(const AValue1: Double; const AType1: TConvType; const AValue2: Double ; const AType2: TConvType ) : Boolean');
  TSepiMetaOverloadedMethod.Create(Result, 'RegisterConversionType');
  TSepiMetaMethod.Create(Result, 'OL$RegisterConversionType$0', @RegisterConversionType_0,
    'function(const AFamily: TConvFamily; const ADescription: string ; const AFactor: Double ) : TConvType');
  TSepiMetaMethod.Create(Result, 'OL$RegisterConversionType$1', @RegisterConversionType_1,
    'function(const AFamily: TConvFamily; const ADescription: string ; const AToCommonProc, AFromCommonProc : TConversionProc ) : TConvType');
  TSepiMetaMethod.Create(Result, 'UnregisterConversionType', @UnregisterConversionType,
    'procedure(const AType: TConvType)');
  TSepiMetaMethod.Create(Result, 'RegisterConversionFamily', @RegisterConversionFamily,
    'function(const ADescription: string): TConvFamily');
  TSepiMetaMethod.Create(Result, 'UnregisterConversionFamily', @UnregisterConversionFamily,
    'procedure(const AFamily: TConvFamily)');
  TSepiMetaMethod.Create(Result, 'CompatibleConversionTypes', @CompatibleConversionTypes,
    'function(const AFrom, ATo: TConvType): Boolean');
  TSepiMetaMethod.Create(Result, 'CompatibleConversionType', @CompatibleConversionType,
    'function(const AType: TConvType; const AFamily: TConvFamily ) : Boolean');
  TSepiMetaMethod.Create(Result, 'GetConvTypes', @GetConvTypes,
    'procedure(const AFamily: TConvFamily; out ATypes: TConvTypeArray)');
  TSepiMetaMethod.Create(Result, 'GetConvFamilies', @GetConvFamilies,
    'procedure(out AFamilies: TConvFamilyArray)');
  TSepiMetaMethod.Create(Result, 'StrToConvUnit', @StrToConvUnit,
    'function(AText: string; out AType: TConvType): Double');
  TSepiMetaMethod.Create(Result, 'TryStrToConvUnit', @TryStrToConvUnit,
    'function(AText: string; out AValue: Double; out AType: TConvType ) : Boolean');
  TSepiMetaMethod.Create(Result, 'ConvUnitToStr', @ConvUnitToStr,
    'function(const AValue: Double; const AType: TConvType): string');
  TSepiMetaMethod.Create(Result, 'ConvTypeToDescription', @ConvTypeToDescription,
    'function(const AType: TConvType): string');
  TSepiMetaMethod.Create(Result, 'ConvFamilyToDescription', @ConvFamilyToDescription,
    'function(const AFamily: TConvFamily): string');
  TSepiMetaOverloadedMethod.Create(Result, 'DescriptionToConvType');
  TSepiMetaMethod.Create(Result, 'OL$DescriptionToConvType$0', @DescriptionToConvType_0,
    'function(const ADescription: string; out AType: TConvType ) : Boolean');
  TSepiMetaMethod.Create(Result, 'OL$DescriptionToConvType$1', @DescriptionToConvType_1,
    'function(const AFamily: TConvFamily; const ADescription: string ; out AType: TConvType ) : Boolean');
  TSepiMetaMethod.Create(Result, 'DescriptionToConvFamily', @DescriptionToConvFamily,
    'function(const ADescription: string; out AFamily: TConvFamily ) : Boolean');
  TSepiMetaOverloadedMethod.Create(Result, 'ConvTypeToFamily');
  TSepiMetaMethod.Create(Result, 'OL$ConvTypeToFamily$0', @ConvTypeToFamily_0,
    'function(const AType: TConvType): TConvFamily');
  TSepiMetaOverloadedMethod.Create(Result, 'TryConvTypeToFamily');
  TSepiMetaMethod.Create(Result, 'OL$TryConvTypeToFamily$0', @TryConvTypeToFamily_0,
    'function(const AType: TConvType; out AFamily: TConvFamily ) : Boolean');
  TSepiMetaMethod.Create(Result, 'OL$ConvTypeToFamily$1', @ConvTypeToFamily_1,
    'function(const AFrom, ATo: TConvType): TConvFamily');
  TSepiMetaMethod.Create(Result, 'OL$TryConvTypeToFamily$1', @TryConvTypeToFamily_1,
    'function(const AFrom, ATo: TConvType; out AFamily: TConvFamily ) : Boolean');
  TSepiMetaOverloadedMethod.Create(Result, 'RaiseConversionError');
  TSepiMetaMethod.Create(Result, 'OL$RaiseConversionError$0', @RaiseConversionError_0,
    'procedure(const AText: string)');
  TSepiMetaMethod.Create(Result, 'OL$RaiseConversionError$1', @RaiseConversionError_1,
    'procedure(const AText: string; const AArgs: array of const )');
  TSepiMetaMethod.Create(Result, 'RaiseConversionRegError', @RaiseConversionRegError,
    'procedure(AFamily: TConvFamily; const ADescription: string )');

  // Types
  TSepiImportsEConversionError.SepiImport(Result);

  // Constants
  TSepiVariable.Create(Result, 'CIllegalConvFamily',
     CIllegalConvFamily, TypeInfo(TConvFamily), True);
  TSepiVariable.Create(Result, 'CIllegalConvType',
     CIllegalConvType, TypeInfo(TConvType), True);

  // Global variables
  TSepiVariable.Create(Result, 'GConvUnitToStrFmt',
     GConvUnitToStrFmt, TypeInfo(string));

  // Types
  TSepiImportsTConvTypeInfo.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConvTypeList));
  TSepiImportsTConvTypeFactor.SepiImport(Result);
  TSepiImportsTConvTypeProcs.SepiImport(Result);

  // Routines
  TSepiMetaMethod.Create(Result, 'OL$RegisterConversionType$2', @RegisterConversionType_2,
    'function(AConvTypeInfo: TConvTypeInfo; out AType: TConvType ) : Boolean');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ConvUtils', ImportUnit);
end.

