{*
  Importe l'unit� ObjAuto dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsObjAuto;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ObjAuto;

implementation

{ You must not localize any of the strings this unit contains! }

{--------------------------}
{ TMethodInfoHeader import }
{--------------------------}

function SepiImportTMethodInfoHeader(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TMethodInfoHeader', True, True);

  with Result do
  begin
    AddField('Len', System.TypeInfo(Word));
    AddField('Addr', 'Pointer');
    AddField('Name', System.TypeInfo(ShortString));

    Complete;
  end;
end;

{--------------------}
{ TReturnInfo import }
{--------------------}

function SepiImportTReturnInfo(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TReturnInfo', True, True);

  with Result do
  begin
    AddField('Version', System.TypeInfo(Byte));
    AddField('CallingConvention', System.TypeInfo(TCallingConvention));
    AddField('ReturnType', '$1');
    AddField('ParamSize', System.TypeInfo(Word));

    Complete;
  end;
end;

{-------------------}
{ TParamInfo import }
{-------------------}

function SepiImportTParamInfo(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TParamInfo', True, True);

  with Result do
  begin
    AddField('Flags', System.TypeInfo(TParamFlags));
    AddField('ParamType', '$2');
    AddField('Access', System.TypeInfo(Word));
    AddField('Name', System.TypeInfo(ShortString));

    Complete;
  end;
end;

{-----------------------}
{ IMethodHandler import }
{-----------------------}

function SepiImportIMethodHandler(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IMethodHandler));

  with Result do
  begin
    AddMethod('Execute',
      'function(const Args: array of Variant): Variant', SepiCompTypes.ccRegister);
    AddMethod('InstanceToVariant',
      'function(Instance: TObject): Variant', SepiCompTypes.ccRegister);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ObjAuto',
    ['TypInfo']);

  // Constants
  TSepiConstant.Create(Result, 'paEAX', paEAX);
  TSepiConstant.Create(Result, 'paEDX', paEDX);
  TSepiConstant.Create(Result, 'paECX', paECX);
  TSepiConstant.Create(Result, 'paStack', paStack);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCallingConvention));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TParamFlags));
  TSepiPointerType.Create(Result, 'PPointer', 'Pointer', True);
  TSepiPointerType.Create(Result, 'PWord', TypeInfo(Word), True);
  TSepiPointerType.Create(Result, 'PMethodInfoHeader', 'TMethodInfoHeader', True);
  SepiImportTMethodInfoHeader(Result);
  TSepiPointerType.Create(Result, 'PReturnInfo', 'TReturnInfo', True);
  TSepiPointerType.Create(Result, '$1', 'PTypeInfo', True);
  SepiImportTReturnInfo(Result);
  TSepiPointerType.Create(Result, 'PParamInfo', 'TParamInfo', True);
  TSepiPointerType.Create(Result, '$2', 'PTypeInfo', True);
  SepiImportTParamInfo(Result);

  // Routines
  TSepiMetaMethod.Create(Result, 'ObjectInvoke', @ObjectInvoke,
    'function(Instance: TObject; MethodHeader: PMethodInfoHeader; const ParamIndexes: array of Integer ; const Params: array of Variant ) : Variant');
  TSepiMetaMethod.Create(Result, 'GetMethodInfo', @GetMethodInfo,
    'function(Instance: TObject; const MethodName: ShortString ) : PMethodInfoHeader');

  // Types
  SepiImportIMethodHandler(Result);

  // Routines
  TSepiMetaMethod.Create(Result, 'CreateMethodPointer', @CreateMethodPointer,
    'function(const MethodHandler: IMethodHandler; TypeData: PTypeData): TMethod');
  TSepiMetaMethod.Create(Result, 'ReleaseMethodPointer', @ReleaseMethodPointer,
    'procedure(MethodPointer: TMethod)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ObjAuto', ImportUnit);
end.

