{*
  Importe l'unit� Variants dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsVariants;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Variants;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTCustomVariantType = class(TCustomVariantType)
  private
    procedure VarDataCastTo_0(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType);
    procedure VarDataCastTo_1(var Dest: TVarData; const AVarType: TVarType);
    constructor Create_0;
    constructor Create_1(RequestedVarType: TVarType);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTInvokeableVariantType = class(TInvokeableVariantType)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantInvalidOpError = class(EVariantInvalidOpError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantTypeCastError = class(EVariantTypeCastError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantOverflowError = class(EVariantOverflowError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantInvalidArgError = class(EVariantInvalidArgError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantBadVarTypeError = class(EVariantBadVarTypeError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantBadIndexError = class(EVariantBadIndexError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantArrayLockedError = class(EVariantArrayLockedError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantNotAnArrayError = class(EVariantNotAnArrayError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantArrayCreateError = class(EVariantArrayCreateError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantNotImplError = class(EVariantNotImplError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantOutOfMemoryError = class(EVariantOutOfMemoryError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantUnexpectedError = class(EVariantUnexpectedError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantDispatchError = class(EVariantDispatchError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantRangeCheckError = class(EVariantRangeCheckError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantInvalidNullOpError = class(EVariantInvalidNullOpError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{---------------------------}
{ TCustomVariantType import }
{---------------------------}

procedure TSepiImportsTCustomVariantType.VarDataCastTo_0(
  var
    Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
begin
  VarDataCastTo(Dest, Source, AVarType);
end;

procedure TSepiImportsTCustomVariantType.VarDataCastTo_1(
  var
    Dest: TVarData; const AVarType: TVarType);
begin
  VarDataCastTo(Dest, AVarType);
end;

constructor TSepiImportsTCustomVariantType.Create_0;
begin
  Create;
end;

constructor TSepiImportsTCustomVariantType.Create_1(
  RequestedVarType: TVarType);
begin
  Create(RequestedVarType);
end;

class function TSepiImportsTCustomVariantType.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomVariantType));

  with Result do
  begin
    AddInterface(System.TypeInfo(IInterface));

    TSepiOverloadedMethod.Create(Result, 'VarDataCastTo');
    TSepiOverloadedMethod.Create(Result, 'Create');
    CurrentVisibility := mvPrivate;

    AddField('FVarType', System.TypeInfo(TVarType));

    CurrentVisibility := mvProtected;

    AddMethod('QueryInterface', @TSepiImportsTCustomVariantType.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('_AddRef', @TSepiImportsTCustomVariantType._AddRef,
      'function: Integer',
      ccStdCall);
    AddMethod('_Release', @TSepiImportsTCustomVariantType._Release,
      'function: Integer',
      ccStdCall);
    AddMethod('SimplisticClear',
      @TSepiImportsTCustomVariantType.SimplisticClear,
      'procedure(var V: TVarData)');
    AddMethod('SimplisticCopy', @TSepiImportsTCustomVariantType.SimplisticCopy,
      'procedure(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean = False )');
    AddMethod('RaiseInvalidOp', @TSepiImportsTCustomVariantType.RaiseInvalidOp,
      'procedure');
    AddMethod('RaiseCastError', @TSepiImportsTCustomVariantType.RaiseCastError,
      'procedure');
    AddMethod('RaiseDispError', @TSepiImportsTCustomVariantType.RaiseDispError,
      'procedure');
    AddMethod('LeftPromotion', @TSepiImportsTCustomVariantType.LeftPromotion,
      'function(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType ) : Boolean',
      mlkVirtual);
    AddMethod('RightPromotion', @TSepiImportsTCustomVariantType.RightPromotion,
      'function(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType ) : Boolean',
      mlkVirtual);
    AddMethod('OlePromotion', @TSepiImportsTCustomVariantType.OlePromotion,
      'function(const V: TVarData; out RequiredVarType: TVarType ) : Boolean',
      mlkVirtual);
    AddMethod('DispInvoke', @TSepiImportsTCustomVariantType.DispInvoke,
      'procedure(Dest: PVarData; const Source: TVarData; CallDesc: PCallDesc ; Params: Pointer )',
      mlkVirtual);
    AddMethod('VarDataInit', @TSepiImportsTCustomVariantType.VarDataInit,
      'procedure(var Dest: TVarData)');
    AddMethod('VarDataClear', @TSepiImportsTCustomVariantType.VarDataClear,
      'procedure(var Dest: TVarData)');
    AddMethod('VarDataCopy', @TSepiImportsTCustomVariantType.VarDataCopy,
      'procedure(var Dest: TVarData; const Source: TVarData)');
    AddMethod('VarDataCopyNoInd',
      @TSepiImportsTCustomVariantType.VarDataCopyNoInd,
      'procedure(var Dest: TVarData; const Source: TVarData)');
    AddMethod('VarDataCast', @TSepiImportsTCustomVariantType.VarDataCast,
      'procedure(var Dest: TVarData; const Source: TVarData)');
    AddMethod('OL$VarDataCastTo$0',
      @TSepiImportsTCustomVariantType.VarDataCastTo_0,
      'procedure(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType )');
    AddMethod('OL$VarDataCastTo$1',
      @TSepiImportsTCustomVariantType.VarDataCastTo_1,
      'procedure(var Dest: TVarData; const AVarType: TVarType)');
    AddMethod('VarDataCastToOleStr',
      @TSepiImportsTCustomVariantType.VarDataCastToOleStr,
      'procedure(var Dest: TVarData)');
    AddMethod('VarDataFromStr', @TSepiImportsTCustomVariantType.VarDataFromStr,
      'procedure(var V: TVarData; const Value: string)');
    AddMethod('VarDataFromOleStr',
      @TSepiImportsTCustomVariantType.VarDataFromOleStr,
      'procedure(var V: TVarData; const Value: WideString)');
    AddMethod('VarDataToStr', @TSepiImportsTCustomVariantType.VarDataToStr,
      'function(const V: TVarData): string');
    AddMethod('VarDataIsEmptyParam',
      @TSepiImportsTCustomVariantType.VarDataIsEmptyParam,
      'function(const V: TVarData): Boolean');
    AddMethod('VarDataIsByRef', @TSepiImportsTCustomVariantType.VarDataIsByRef,
      'function(const V: TVarData): Boolean');
    AddMethod('VarDataIsArray', @TSepiImportsTCustomVariantType.VarDataIsArray,
      'function(const V: TVarData): Boolean');
    AddMethod('VarDataIsOrdinal',
      @TSepiImportsTCustomVariantType.VarDataIsOrdinal,
      'function(const V: TVarData): Boolean');
    AddMethod('VarDataIsFloat', @TSepiImportsTCustomVariantType.VarDataIsFloat,
      'function(const V: TVarData): Boolean');
    AddMethod('VarDataIsNumeric',
      @TSepiImportsTCustomVariantType.VarDataIsNumeric,
      'function(const V: TVarData): Boolean');
    AddMethod('VarDataIsStr', @TSepiImportsTCustomVariantType.VarDataIsStr,
      'function(const V: TVarData): Boolean');

    CurrentVisibility := mvPublic;

    AddMethod('OL$Create$0', @TSepiImportsTCustomVariantType.Create_0,
      'constructor');
    AddMethod('OL$Create$1', @TSepiImportsTCustomVariantType.Create_1,
      'constructor(RequestedVarType: TVarType)');
    AddMethod('Destroy', @TSepiImportsTCustomVariantType.Destroy,
      'destructor',
      mlkOverride);

    AddProperty('VarType', 'property: TVarType',
      'FVarType', '');

    AddMethod('IsClear', @TSepiImportsTCustomVariantType.IsClear,
      'function(const V: TVarData): Boolean',
      mlkVirtual);
    AddMethod('Cast', @TSepiImportsTCustomVariantType.Cast,
      'procedure(var Dest: TVarData; const Source: TVarData)',
      mlkVirtual);
    AddMethod('CastTo', @TSepiImportsTCustomVariantType.CastTo,
      'procedure(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType )',
      mlkVirtual);
    AddMethod('CastToOle', @TSepiImportsTCustomVariantType.CastToOle,
      'procedure(var Dest: TVarData; const Source: TVarData)',
      mlkVirtual);
    AddMethod('Clear', {@TSepiImportsTCustomVariantType.Clear}nil,
      'procedure(var V: TVarData)',
      mlkVirtual, True);
    AddMethod('Copy', {@TSepiImportsTCustomVariantType.Copy}nil,
      'procedure(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean )',
      mlkVirtual, True);
    AddMethod('BinaryOp', @TSepiImportsTCustomVariantType.BinaryOp,
      'procedure(var Left: TVarData; const Right: TVarData; const Operator: TVarOp )',
      mlkVirtual);
    AddMethod('UnaryOp', @TSepiImportsTCustomVariantType.UnaryOp,
      'procedure(var Right: TVarData; const Operator: TVarOp)',
      mlkVirtual);
    AddMethod('CompareOp', @TSepiImportsTCustomVariantType.CompareOp,
      'function(const Left, Right: TVarData; const Operator: TVarOp ) : Boolean',
      mlkVirtual);
    AddMethod('Compare', @TSepiImportsTCustomVariantType.Compare,
      'procedure(const Left, Right: TVarData; var Relationship: TVarCompareResult )',
      mlkVirtual);

    Complete;
  end;
end;

{-----------------------}
{ IVarInvokeable import }
{-----------------------}

function SepiImportIVarInvokeable(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IVarInvokeable));

  with Result do
  begin
    AddMethod('DoFunction',
      'function(var Dest: TVarData; const V: TVarData; const Name: string ; const Arguments: TVarDataArray ) : Boolean',
      ccRegister);
    AddMethod('DoProcedure',
      'function(const V: TVarData; const Name: string; const Arguments: TVarDataArray ) : Boolean',
      ccRegister);
    AddMethod('GetProperty',
      'function(var Dest: TVarData; const V: TVarData; const Name: string ) : Boolean',
      ccRegister);
    AddMethod('SetProperty',
      'function(const V: TVarData; const Name: string; const Value: TVarData ) : Boolean',
      ccRegister);

    Complete;
  end;
end;

{-------------------------------}
{ TInvokeableVariantType import }
{-------------------------------}

class function TSepiImportsTInvokeableVariantType.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TInvokeableVariantType));

  with Result do
  begin
    AddInterface(System.TypeInfo(IVarInvokeable));

    CurrentVisibility := mvProtected;

    AddMethod('FixupIdent', @TSepiImportsTInvokeableVariantType.FixupIdent,
      'function(const AText: string): string',
      mlkVirtual);
    AddMethod('DispInvoke', @TSepiImportsTInvokeableVariantType.DispInvoke,
      'procedure(Dest: PVarData; const Source: TVarData; CallDesc: PCallDesc ; Params: Pointer )',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('DoFunction', @TSepiImportsTInvokeableVariantType.DoFunction,
      'function(var Dest: TVarData; const V: TVarData; const Name: string ; const Arguments: TVarDataArray ) : Boolean',
      mlkVirtual);
    AddMethod('DoProcedure', @TSepiImportsTInvokeableVariantType.DoProcedure,
      'function(const V: TVarData; const Name: string; const Arguments: TVarDataArray ) : Boolean',
      mlkVirtual);
    AddMethod('GetProperty', @TSepiImportsTInvokeableVariantType.GetProperty,
      'function(var Dest: TVarData; const V: TVarData; const Name: string ) : Boolean',
      mlkVirtual);
    AddMethod('SetProperty', @TSepiImportsTInvokeableVariantType.SetProperty,
      'function(const V: TVarData; const Name: string; const Value: TVarData ) : Boolean',
      mlkVirtual);

    Complete;
  end;
end;

{------------------------------}
{ IVarInstanceReference import }
{------------------------------}

function SepiImportIVarInstanceReference(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IVarInstanceReference));

  with Result do
  begin
    AddMethod('GetInstance',
      'function(const V: TVarData): TObject', ccRegister);

    Complete;
  end;
end;

{-------------------------------}
{ EVariantInvalidOpError import }
{-------------------------------}

class function TSepiImportsEVariantInvalidOpError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantInvalidOpError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------------}
{ EVariantTypeCastError import }
{------------------------------}

class function TSepiImportsEVariantTypeCastError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantTypeCastError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------------}
{ EVariantOverflowError import }
{------------------------------}

class function TSepiImportsEVariantOverflowError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantOverflowError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------------------}
{ EVariantInvalidArgError import }
{--------------------------------}

class function TSepiImportsEVariantInvalidArgError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantInvalidArgError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------------------}
{ EVariantBadVarTypeError import }
{--------------------------------}

class function TSepiImportsEVariantBadVarTypeError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantBadVarTypeError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------------}
{ EVariantBadIndexError import }
{------------------------------}

class function TSepiImportsEVariantBadIndexError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantBadIndexError));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------------------}
{ EVariantArrayLockedError import }
{---------------------------------}

class function TSepiImportsEVariantArrayLockedError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantArrayLockedError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------------------}
{ EVariantNotAnArrayError import }
{--------------------------------}

class function TSepiImportsEVariantNotAnArrayError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantNotAnArrayError));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------------------}
{ EVariantArrayCreateError import }
{---------------------------------}

class function TSepiImportsEVariantArrayCreateError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantArrayCreateError));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------------}
{ EVariantNotImplError import }
{-----------------------------}

class function TSepiImportsEVariantNotImplError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantNotImplError));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------------------}
{ EVariantOutOfMemoryError import }
{---------------------------------}

class function TSepiImportsEVariantOutOfMemoryError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantOutOfMemoryError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------------------}
{ EVariantUnexpectedError import }
{--------------------------------}

class function TSepiImportsEVariantUnexpectedError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantUnexpectedError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------------}
{ EVariantDispatchError import }
{------------------------------}

class function TSepiImportsEVariantDispatchError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantDispatchError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------------------}
{ EVariantRangeCheckError import }
{--------------------------------}

class function TSepiImportsEVariantRangeCheckError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantRangeCheckError));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------------------}
{ EVariantInvalidNullOpError import }
{-----------------------------------}

class function TSepiImportsEVariantInvalidNullOpError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantInvalidNullOpError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function VarIsType_0(const V: Variant; AVarType: TVarType): Boolean;
begin
  Result := VarIsType(V, AVarType);
end;

function VarIsType_1(const V: Variant;
  const AVarTypes: array of TVarType): Boolean;
begin
  Result := VarIsType(V, AVarTypes);
end;

function VarIsError_0(const V: Variant; out AResult: HRESULT): Boolean;
begin
  Result := VarIsError(V, AResult);
end;

function VarIsError_1(const V: Variant): Boolean;
begin
  Result := VarIsError(V);
end;

function VarSupports_0(const V: Variant; const IID: TGUID; out Intf): Boolean;
begin
  Result := VarSupports(V, IID, Intf);
end;

function VarSupports_1(const V: Variant; const IID: TGUID): Boolean;
begin
  Result := VarSupports(V, IID);
end;

function VarIsArray_0(const A: Variant): Boolean;
begin
  Result := VarIsArray(A);
end;

function VarIsArray_1(const A: Variant; AResolveByRef: Boolean): Boolean;
begin
  Result := VarIsArray(A, AResolveByRef);
end;

function FindCustomVariantType_0(const AVarType: TVarType;
  out CustomVariantType: TCustomVariantType): Boolean;
begin
  Result := FindCustomVariantType(AVarType, CustomVariantType);
end;

function FindCustomVariantType_1(const TypeName: string;
  out CustomVariantType: TCustomVariantType): Boolean;
begin
  Result := FindCustomVariantType(TypeName, CustomVariantType);
end;

procedure VarCastError_0;
begin
  VarCastError;
end;

procedure VarCastError_1(const ASourceType, ADestType: TVarType);
begin
  VarCastError(ASourceType, ADestType);
end;

procedure VarResultCheck_0(AResult: HRESULT);
begin
  VarResultCheck(AResult);
end;

procedure VarResultCheck_1(AResult: HRESULT; ASourceType, ADestType: TVarType);
begin
  VarResultCheck(AResult, ASourceType, ADestType);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'Variants',
    ['Types', 'SysUtils']);

  // Routines
  TSepiMethod.Create(Result, 'VarType', @VarType,
    'function(const V: Variant): TVarType');
  TSepiMethod.Create(Result, 'VarAsType', @VarAsType,
    'function(const V: Variant; AVarType: TVarType): Variant');
  TSepiOverloadedMethod.Create(Result, 'VarIsType');
  TSepiMethod.Create(Result, 'OL$VarIsType$0', @VarIsType_0,
    'function(const V: Variant; AVarType: TVarType): Boolean');
  TSepiMethod.Create(Result, 'OL$VarIsType$1', @VarIsType_1,
    'function(const V: Variant; const AVarTypes: array of TVarType): Boolean');
  TSepiMethod.Create(Result, 'VarIsByRef', @VarIsByRef,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarIsEmpty', @VarIsEmpty,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarCheckEmpty', @VarCheckEmpty,
    'procedure(const V: Variant)');
  TSepiMethod.Create(Result, 'VarIsNull', @VarIsNull,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarIsClear', @VarIsClear,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarIsCustom', @VarIsCustom,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarIsOrdinal', @VarIsOrdinal,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarIsFloat', @VarIsFloat,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarIsNumeric', @VarIsNumeric,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarIsStr', @VarIsStr,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarToStr', @VarToStr,
    'function(const V: Variant): string');
  TSepiMethod.Create(Result, 'VarToStrDef', @VarToStrDef,
    'function(const V: Variant; const ADefault: string): string');
  TSepiMethod.Create(Result, 'VarToWideStr', @VarToWideStr,
    'function(const V: Variant): WideString');
  TSepiMethod.Create(Result, 'VarToWideStrDef', @VarToWideStrDef,
    'function(const V: Variant; const ADefault: WideString): WideString');
  TSepiMethod.Create(Result, 'VarToDateTime', @VarToDateTime,
    'function(const V: Variant): TDateTime');
  TSepiMethod.Create(Result, 'VarFromDateTime', @VarFromDateTime,
    'function(const DateTime: TDateTime): Variant');
  TSepiMethod.Create(Result, 'VarInRange', @VarInRange,
    'function(const AValue, AMin, AMax: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarEnsureRange', @VarEnsureRange,
    'function(const AValue, AMin, AMax: Variant): Variant');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TVariantRelationship));

  // Routines
  TSepiMethod.Create(Result, 'VarSameValue', @VarSameValue,
    'function(const A, B: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarCompareValue', @VarCompareValue,
    'function(const A, B: Variant): TVariantRelationship');
  TSepiMethod.Create(Result, 'VarIsEmptyParam', @VarIsEmptyParam,
    'function(const V: Variant): Boolean');
  TSepiOverloadedMethod.Create(Result, 'VarIsError');
  TSepiMethod.Create(Result, 'OL$VarIsError$0', @VarIsError_0,
    'function(const V: Variant; out AResult: HRESULT): Boolean');
  TSepiMethod.Create(Result, 'OL$VarIsError$1', @VarIsError_1,
    'function(const V: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarAsError', @VarAsError,
    'function(AResult: HRESULT): Variant');
  TSepiOverloadedMethod.Create(Result, 'VarSupports');
  TSepiMethod.Create(Result, 'OL$VarSupports$0', @VarSupports_0,
    'function(const V: Variant; const IID: TGUID; out Intf): Boolean');
  TSepiMethod.Create(Result, 'OL$VarSupports$1', @VarSupports_1,
    'function(const V: Variant; const IID: TGUID): Boolean');
  TSepiMethod.Create(Result, 'VarCopyNoInd', @VarCopyNoInd,
    'procedure(var Dest: Variant; const Source: Variant)');
  TSepiOverloadedMethod.Create(Result, 'VarIsArray');
  TSepiMethod.Create(Result, 'OL$VarIsArray$0', @VarIsArray_0,
    'function(const A: Variant): Boolean');
  TSepiMethod.Create(Result, 'OL$VarIsArray$1', @VarIsArray_1,
    'function(const A: Variant; AResolveByRef: Boolean): Boolean');
  TSepiMethod.Create(Result, 'VarArrayCreate', @VarArrayCreate,
    'function(const Bounds: array of Integer; AVarType: TVarType): Variant');
  TSepiMethod.Create(Result, 'VarArrayOf', @VarArrayOf,
    'function(const Values: array of Variant): Variant');
  TSepiMethod.Create(Result, 'VarArrayRef', @VarArrayRef,
    'function(const A: Variant): Variant');
  TSepiMethod.Create(Result, 'VarTypeIsValidArrayType',
    @VarTypeIsValidArrayType,
    'function(const AVarType: TVarType): Boolean');
  TSepiMethod.Create(Result, 'VarTypeIsValidElementType',
    @VarTypeIsValidElementType,
    'function(const AVarType: TVarType): Boolean');
  TSepiMethod.Create(Result, 'VarArrayDimCount', @VarArrayDimCount,
    'function(const A: Variant): Integer');
  TSepiMethod.Create(Result, 'VarArrayLowBound', @VarArrayLowBound,
    'function(const A: Variant; Dim: Integer): Integer');
  TSepiMethod.Create(Result, 'VarArrayHighBound', @VarArrayHighBound,
    'function(const A: Variant; Dim: Integer): Integer');
  TSepiMethod.Create(Result, 'VarArrayLock', @VarArrayLock,
    'function(const A: Variant): Pointer');
  TSepiMethod.Create(Result, 'VarArrayUnlock', @VarArrayUnlock,
    'procedure(const A: Variant)');
  TSepiMethod.Create(Result, 'VarArrayAsPSafeArray', @VarArrayAsPSafeArray,
    'function(const A: Variant): PVarArray');
  TSepiMethod.Create(Result, 'VarArrayGet', @VarArrayGet,
    'function(const A: Variant; const Indices: array of Integer): Variant');
  TSepiMethod.Create(Result, 'VarArrayPut', @VarArrayPut,
    'procedure(var A: Variant; const Value: Variant; const Indices: array of Integer)');
  TSepiMethod.Create(Result, 'DynArrayToVariant', @DynArrayToVariant,
    'procedure(var V: Variant; const DynArray: Pointer; TypeInfo: Pointer)');
  TSepiMethod.Create(Result, 'DynArrayFromVariant', @DynArrayFromVariant,
    'procedure(var DynArray: Pointer; const V: Variant; TypeInfo: Pointer)');
  TSepiMethod.Create(Result, 'Unassigned', @Unassigned,
    'function: Variant');
  TSepiMethod.Create(Result, 'Null', @Null,
    'function: Variant');

  // Global variables
  TSepiVariable.Create(Result, 'EmptyParam',
    EmptyParam, TypeInfo(OleVariant));

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TVarCompareResult));
  TSepiImportsTCustomVariantType.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TCustomVariantTypeClass',
    TypeInfo(TCustomVariantType), True);
  TSepiDynArrayType(TSepiType.LoadFromTypeInfo(Result,
    TypeInfo(TVarDataArray)))
    .SetElementType('TVarData');
  SepiImportIVarInvokeable(Result);
  TSepiImportsTInvokeableVariantType.SepiImport(Result);
  SepiImportIVarInstanceReference(Result);

  // Routines
  TSepiOverloadedMethod.Create(Result, 'FindCustomVariantType');
  TSepiMethod.Create(Result, 'OL$FindCustomVariantType$0',
    @FindCustomVariantType_0,
    'function(const AVarType: TVarType; out CustomVariantType: TCustomVariantType ) : Boolean');
  TSepiMethod.Create(Result, 'OL$FindCustomVariantType$1',
    @FindCustomVariantType_1,
    'function(const TypeName: string; out CustomVariantType: TCustomVariantType ) : Boolean');

  // Types
  TSepiImportsEVariantInvalidOpError.SepiImport(Result);
  TSepiImportsEVariantTypeCastError.SepiImport(Result);
  TSepiImportsEVariantOverflowError.SepiImport(Result);
  TSepiImportsEVariantInvalidArgError.SepiImport(Result);
  TSepiImportsEVariantBadVarTypeError.SepiImport(Result);
  TSepiImportsEVariantBadIndexError.SepiImport(Result);
  TSepiImportsEVariantArrayLockedError.SepiImport(Result);
  TSepiImportsEVariantNotAnArrayError.SepiImport(Result);
  TSepiImportsEVariantArrayCreateError.SepiImport(Result);
  TSepiImportsEVariantNotImplError.SepiImport(Result);
  TSepiImportsEVariantOutOfMemoryError.SepiImport(Result);
  TSepiImportsEVariantUnexpectedError.SepiImport(Result);
  TSepiImportsEVariantDispatchError.SepiImport(Result);
  TSepiImportsEVariantRangeCheckError.SepiImport(Result);
  TSepiImportsEVariantInvalidNullOpError.SepiImport(Result);

  // Routines
  TSepiOverloadedMethod.Create(Result, 'VarCastError');
  TSepiMethod.Create(Result, 'OL$VarCastError$0', @VarCastError_0,
    'procedure');
  TSepiMethod.Create(Result, 'OL$VarCastError$1', @VarCastError_1,
    'procedure(const ASourceType, ADestType: TVarType)');
  TSepiMethod.Create(Result, 'VarInvalidOp', @VarInvalidOp,
    'procedure');
  TSepiMethod.Create(Result, 'VarInvalidNullOp', @VarInvalidNullOp,
    'procedure');
  TSepiMethod.Create(Result, 'VarOverflowError', @VarOverflowError,
    'procedure(const ASourceType, ADestType: TVarType)');
  TSepiMethod.Create(Result, 'VarRangeCheckError', @VarRangeCheckError,
    'procedure(const ASourceType, ADestType: TVarType)');
  TSepiMethod.Create(Result, 'VarArrayCreateError', @VarArrayCreateError,
    'procedure');
  TSepiOverloadedMethod.Create(Result, 'VarResultCheck');
  TSepiMethod.Create(Result, 'OL$VarResultCheck$0', @VarResultCheck_0,
    'procedure(AResult: HRESULT)');
  TSepiMethod.Create(Result, 'OL$VarResultCheck$1', @VarResultCheck_1,
    'procedure(AResult: HRESULT; ASourceType, ADestType: TVarType)');
  TSepiMethod.Create(Result, 'HandleConversionException',
    @HandleConversionException,
    'procedure(const ASourceType, ADestType: TVarType)');
  TSepiMethod.Create(Result, 'VarTypeAsText', @VarTypeAsText,
    'function(const AType: TVarType): string');
  TSepiMethod.Create(Result, 'FindVarData', @FindVarData,
    'function(const V: Variant): PVarData');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TNullCompareRule));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBooleanToStringRule));

  // Global variables
  TSepiVariable.Create(Result, 'NullEqualityRule',
    NullEqualityRule, TypeInfo(TNullCompareRule));
  TSepiVariable.Create(Result, 'NullMagnitudeRule',
    NullMagnitudeRule, TypeInfo(TNullCompareRule));
  TSepiVariable.Create(Result, 'NullStrictConvert',
    NullStrictConvert, TypeInfo(Boolean));
  TSepiVariable.Create(Result, 'NullAsStringValue',
    NullAsStringValue, TypeInfo(string));
  TSepiVariable.Create(Result, 'PackVarCreation',
    PackVarCreation, TypeInfo(Boolean));
  TSepiVariable.Create(Result, 'BooleanToStringRule',
    BooleanToStringRule, TypeInfo(TBooleanToStringRule));

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Variants', ImportUnit);
end.

