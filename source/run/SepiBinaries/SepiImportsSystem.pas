{*
  Importe l'unité System dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsSystem;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTObject = class(TObject)
  private
    class function SepiImportTObject(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTInterfacedObject = class(TInterfacedObject)
  private
    class function SepiImportTInterfacedObject(
      Owner : TSepiMetaUnit) : TSepiClass;
  end;

{----------------}
{ TObject import }
{----------------}

class function TSepiImportsTObject.SepiImportTObject(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(Owner, TypeInfo(TObject));
  TSepiMetaClass.Create(Owner, 'TClass', Result, True);

  with Result do
  begin
    AddMethod('Create', @TObject.Create,
      'constructor');
    AddMethod('Free', @TObject.Free,
      'procedure');
    AddMethod('InitInstance', @TObject.InitInstance,
      'class function(Instance: Pointer): TObject');
    AddMethod('CleanupInstance', @TObject.CleanupInstance,
      'procedure');
    AddMethod('ClassType', @TObject.ClassType,
      'function: TClass');
    AddMethod('ClassName', @TObject.ClassName,
      'class function: ShortString');
    AddMethod('ClassNameIs', @TObject.ClassNameIs,
      'class function(const Name: string): Boolean');
    AddMethod('ClassParent', @TObject.ClassParent,
      'class function: TClass');
    AddMethod('ClassInfo', @TObject.ClassInfo,
      'class function: Pointer');
    AddMethod('InstanceSize', @TObject.InstanceSize,
      'class function: Longint');
    AddMethod('InheritsFrom', @TObject.InheritsFrom,
      'class function(AClass: TClass): Boolean');
    AddMethod('MethodAddress', @TObject.MethodAddress,
      'class function(const Name: ShortString): Pointer');
    AddMethod('MethodName', @TObject.MethodName,
      'class function(Address: Pointer): ShortString');
    AddMethod('FieldAddress', @TObject.FieldAddress,
      'function(const Name: ShortString): Pointer');
    AddMethod('GetInterface', @TObject.GetInterface,
      'function(const IID: TGUID; out Obj): Boolean');
    AddMethod('GetInterfaceEntry', @TObject.GetInterfaceEntry,
      'class function(const IID: TGUID): PInterfaceEntry');
    AddMethod('GetInterfaceTable', @TObject.GetInterfaceTable,
      'class function: PInterfaceTable');
    AddMethod('SafeCallException', @TObject.SafeCallException,
      'function(ExceptObject: TObject; ExceptAddr: Pointer): HResult',
      mlkVirtual);
    AddMethod('AfterConstruction', @TObject.AfterConstruction,
      'procedure', mlkVirtual);
    AddMethod('BeforeDestruction', @TObject.BeforeDestruction,
      'procedure', mlkVirtual);
    AddMethod('Dispatch', @TObject.Dispatch,
      'procedure(var Message)', mlkVirtual);
    AddMethod('DefaultHandler', @TObject.DefaultHandler,
      'procedure(var Message)', mlkVirtual);
    AddMethod('NewInstance', @TObject.NewInstance,
      'class function: TObject', mlkVirtual);
    AddMethod('FreeInstance', @TObject.FreeInstance,
      'procedure', mlkVirtual);
    AddMethod('Destroy', @TObject.Destroy,
      'destructor', mlkVirtual);

    Complete;
  end;
end;

{-------------------}
{ IInterface import }
{-------------------}

function SepiImportIInterface(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(Owner, TypeInfo(IInterface));

  with Result do
  begin
    AddMethod('QueryInterface',
      'function(const IID: TGUID; out Obj): HResult', ccStdCall);
    AddMethod('_AddRef',
      'function: Integer', ccStdCall);
    AddMethod('_Release',
      'function: Integer', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ IInvokable import }
{-------------------}

function SepiImportIInvokable(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(Owner, TypeInfo(IInvokable));
  Result.Complete;
end;

{-----------------}
{ IDipatch import }
{-----------------}

function SepiImportIDispatch(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(Owner, TypeInfo(IDispatch));

  with Result do
  begin
    AddMethod('GetTypeInfoCount',
      'function(out Count: Integer): HResult', ccStdCall);
    AddMethod('GetTypeInfo',
      'function(Index, LocaleID: Integer; out TypeInfo): HResult', ccStdCall);
    AddMethod('GetIDsOfNames',
      'function(const IID: TGUID; Names: Pointer;'+
      '  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult', ccStdCall);
    AddMethod('Invoke',
      'function(DispID: Integer; const IID: TGUID; LocaleID: Integer;'+
      '  Flags: Word; var Params;'+
      '  VarResult, ExcepInfo, ArgErr: Pointer): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ TInterfacedObject import }
{--------------------------}

class function TSepiImportsTInterfacedObject.SepiImportTInterfacedObject(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(Owner, TypeInfo(TInterfacedObject));
  TSepiMetaClass.Create(Owner, 'TInterfacedClass', Result, True);

  with Result do
  begin
    AddInterface(System.TypeInfo(IInterface));

    CurrentVisibility := mvProtected;

    AddField('FRefCount', System.TypeInfo(Integer));

    AddMethod('QueryInterface', @TSepiImportsTInterfacedObject.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult', mlkStatic,
      False, 0, ccStdCall);
    AddMethod('_AddRef', @TSepiImportsTInterfacedObject._AddRef,
      'function: Integer', mlkStatic, False, 0, ccStdCall);
    AddMethod('_Release', @TSepiImportsTInterfacedObject._Release,
      'function: Integer', mlkStatic, False, 0, ccStdCall);

    CurrentVisibility := mvPublic;

    AddMethod('AfterConstruction', @TInterfacedObject.AfterConstruction,
      'procedure', mlkOverride);
    AddMethod('BeforeDestruction', @TInterfacedObject.BeforeDestruction,
      'procedure', mlkOverride);
    AddMethod('NewInstance', @TInterfacedObject.NewInstance,
      'class function: TObject', mlkOverride);

    AddProperty('RefCount', 'property: Integer', 'FRefCount', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
var PointerType : TSepiPointerType;
    TGUIDRecord, TIntfEntryRecord, TIntfTableRecord : TSepiRecordType;
begin
  Result := TSepiMetaUnit.Create(Root, 'System');

  // Integer types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Integer));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Cardinal));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Shortint));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Smallint));
  TSepiTypeAlias.Create(Result, 'Longint', TypeInfo(Longint));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Int64));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Byte));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Word));
  TSepiTypeAlias.Create(Result, 'Longword', TypeInfo(Longword));

  // Character types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Char));
  TSepiTypeAlias.Create(Result, 'AnsiChar', TypeInfo(AnsiChar));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(WideChar));

  // Boolean types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Boolean));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(ByteBool));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(WordBool));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(LongBool));

  // Float types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Single));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Double));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Extended));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Currency));

  // String types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(string));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(ShortString));
  TSepiTypeAlias.Create(Result, 'AnsiString', TypeInfo(AnsiString));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(WideString));

  // Pointer types
  PointerType := TSepiPointerType.Create(Result, 'Pointer',
    TSepiType(nil), True);
  TSepiPointerType.Create(Result, 'PChar', TypeInfo(Char), True);

  { System constants }
  TSepiConstant.Create(Result, 'RTLVersion', RTLVersion);
  TSepiConstant.Create(Result, 'CompilerVersion', CompilerVersion);
  {$IF DECLARED(GPL)}
    TSepiConstant.Create(Result, 'GPL', GPL);
  {$IFEND}
  TSepiConstant.Create(Result, 'True', True);
  TSepiConstant.Create(Result, 'False', False);
  { The pseudo-constant nil isn't declared here, for it has many different
    types, depending on the situation. Each compiler should understand the nil
    value for what it is: a special value, not a simple constant. }

  { Variant type codes }
  TSepiConstant.Create(Result, 'varEmpty', varEmpty);
  TSepiConstant.Create(Result, 'varNull', varNull);
  TSepiConstant.Create(Result, 'varSmallint', varSmallint);
  TSepiConstant.Create(Result, 'varInteger', varInteger);
  TSepiConstant.Create(Result, 'varSingle', varSingle);
  TSepiConstant.Create(Result, 'varDouble', varDouble);
  TSepiConstant.Create(Result, 'varCurrency', varCurrency);
  TSepiConstant.Create(Result, 'varDate', varDate);
  TSepiConstant.Create(Result, 'varOleStr', varOleStr);
  TSepiConstant.Create(Result, 'varDispatch', varDispatch);
  TSepiConstant.Create(Result, 'varError', varError);
  TSepiConstant.Create(Result, 'varBoolean', varBoolean);
  TSepiConstant.Create(Result, 'varVariant', varVariant);
  TSepiConstant.Create(Result, 'varUnknown', varUnknown);
  TSepiConstant.Create(Result, 'varShortInt', varShortInt);
  TSepiConstant.Create(Result, 'varByte', varByte);
  TSepiConstant.Create(Result, 'varWord', varWord);
  TSepiConstant.Create(Result, 'varLongWord', varLongWord);
  TSepiConstant.Create(Result, 'varInt64', varInt64);

  TSepiConstant.Create(Result, 'varStrArg', varStrArg);
  TSepiConstant.Create(Result, 'varString', varString);
  TSepiConstant.Create(Result, 'varAny', varAny);

  TSepiConstant.Create(Result, 'varTypeMask', varTypeMask);
  TSepiConstant.Create(Result, 'varArray', varArray);
  TSepiConstant.Create(Result, 'varByRef', varByRef);

  { TVarRec.VType values }
  TSepiConstant.Create(Result, 'vtInteger', vtInteger);
  TSepiConstant.Create(Result, 'vtBoolean', vtBoolean);
  TSepiConstant.Create(Result, 'vtChar', vtChar);
  TSepiConstant.Create(Result, 'vtExtended', vtExtended);
  TSepiConstant.Create(Result, 'vtString', vtString);
  TSepiConstant.Create(Result, 'vtPointer', vtPointer);
  TSepiConstant.Create(Result, 'vtPChar', vtPChar);
  TSepiConstant.Create(Result, 'vtObject', vtObject);
  TSepiConstant.Create(Result, 'vtClass', vtClass);
  TSepiConstant.Create(Result, 'vtWideChar', vtWideChar);
  TSepiConstant.Create(Result, 'vtPWideChar', vtPWideChar);
  TSepiConstant.Create(Result, 'vtAnsiString', vtAnsiString);
  TSepiConstant.Create(Result, 'vtCurrency', vtCurrency);
  TSepiConstant.Create(Result, 'vtVariant', vtVariant);
  TSepiConstant.Create(Result, 'vtInterface', vtInterface);
  TSepiConstant.Create(Result, 'vtWideString', vtWideString);
  TSepiConstant.Create(Result, 'vtInt64', vtInt64);

  { Virtual method table entries }
  TSepiConstant.Create(Result, 'vmtSelfPtr', vmtSelfPtr);
  TSepiConstant.Create(Result, 'vmtIntfTable', vmtIntfTable);
  TSepiConstant.Create(Result, 'vmtAutoTable', vmtAutoTable);
  TSepiConstant.Create(Result, 'vmtInitTable', vmtInitTable);
  TSepiConstant.Create(Result, 'vmtTypeInfo', vmtTypeInfo);
  TSepiConstant.Create(Result, 'vmtFieldTable', vmtFieldTable);
  TSepiConstant.Create(Result, 'vmtMethodTable', vmtMethodTable);
  TSepiConstant.Create(Result, 'vmtDynamicTable', vmtDynamicTable);
  TSepiConstant.Create(Result, 'vmtClassName', vmtClassName);
  TSepiConstant.Create(Result, 'vmtInstanceSize', vmtInstanceSize);
  TSepiConstant.Create(Result, 'vmtParent', vmtParent);

  { Types declared in System.pas }
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(HRESULT));

  { TGUID record }
  TGUIDRecord := TSepiRecordType.Create(Result, 'TGUID', True, True);
  with TGUIDRecord do
  begin
    AddField('D1', System.TypeInfo(LongWord));
    AddField('D2', System.TypeInfo(Word));
    AddField('D3', System.TypeInfo(Word));
    AddField('D4', TSepiArrayType.Create(Result, 'TGUID$D4$Type', [0, 7],
      Root.FindType(System.TypeInfo(Byte)), True));
    Complete;
  end;
  TSepiPointerType.Create(Result, 'PGUID', TGUIDRecord, True);

  { TInterfaceEntry record }
  TIntfEntryRecord := TSepiRecordType.Create(Result, 'TInterfaceEntry',
    True, True);
  with TIntfEntryRecord do
  begin
    AddField('IID', TGUIDRecord);
    AddField('VTable', PointerType);
    AddField('IOffset', System.TypeInfo(Integer));
    AddField('ImplGetter', System.TypeInfo(Integer));
    Complete;
  end;
  TSepiPointerType.Create(Result, 'PInterfaceEntry', TIntfEntryRecord, True);

  { TInterfaceTable record }
  TIntfTableRecord := TSepiRecordType.Create(Result, 'TInterfaceTable',
    True, True);
  with TIntfTableRecord do
  begin
    AddField('EntryCount', System.TypeInfo(Integer));
    AddField('Entries', TSepiArrayType.Create(Result,
      'TInterfaceTable$Entries$Type', [0, 9999], TIntfEntryRecord, True));
    Complete;
  end;
  TSepiPointerType.Create(Result, 'PInterfaceTable', TIntfTableRecord, True);

  { TMethod record }
  with TSepiRecordType.Create(Result, 'TMethod', False, True) do
  begin
    AddField('Code', PointerType);
    AddField('Data', PointerType);
    Complete;
  end;

  { TDispatchMessage record }
  with TSepiRecordType.Create(Result, 'TDispatchMessage', False, True) do
  begin
    AddField('MsgID', System.TypeInfo(Word));
    Complete;
  end;

  { TObject class }
  TSepiImportsTObject.SepiImportTObject(Result);

  { Result code constants }
  TSepiConstant.Create(Result, 'S_OK', S_OK);
  TSepiConstant.Create(Result, 'S_FALSE', S_FALSE);
  TSepiConstant.Create(Result, 'E_NOINTERFACE',
    E_NOINTERFACE, TypeInfo(HRESULT));
  TSepiConstant.Create(Result, 'E_UNEXPECTED', E_UNEXPECTED, TypeInfo(HRESULT));
  TSepiConstant.Create(Result, 'E_NOTIMPL', E_NOTIMPL, TypeInfo(HRESULT));

  { IInterface interface }
  SepiImportIInterface(Result);

  { IUnknown interface }
  TSepiTypeAlias.Create(Result, 'IUnknown', TypeInfo(IUnknown));

  { IIvokable interface }
  SepiImportIInvokable(Result);

  { IDispatch interface }
  SepiImportIDispatch(Result);

  { TInterfacedObject class }
  TSepiImportsTInterfacedObject.SepiImportTInterfacedObject(Result);

  { ... }

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('System', ImportUnit);
end.

