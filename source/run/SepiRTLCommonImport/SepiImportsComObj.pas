{*
  Importe l'unité ComObj dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsComObj;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, SysUtils, ActiveX, ComObj;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTComServerObject = class(TComServerObject)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTComClassManager = class(TComClassManager)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTComObject = class(TComObject)
  private
    function GetController: IUnknown;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTComObjectFactory = class(TComObjectFactory)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTTypedComObject = class(TTypedComObject)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTTypedComObjectFactory = class(TTypedComObjectFactory)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTAutoObject = class(TAutoObject)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTAutoObjectFactory = class(TAutoObjectFactory)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTAutoIntfObject = class(TAutoIntfObject)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOleError = class(EOleError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOleSysError = class(EOleSysError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOleException = class(EOleException)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOleRegistrationError = class(EOleRegistrationError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{-------------------------}
{ TComServerObject import }
{-------------------------}

class function TSepiImportsTComServerObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComServerObject));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('CountObject', nil,
      'function(Created: Boolean): Integer',
      mlkVirtual, True);
    AddMethod('CountFactory', nil,
      'function(Created: Boolean): Integer',
      mlkVirtual, True);
    AddMethod('GetHelpFileName', nil,
      'function: string',
      mlkVirtual, True);
    AddMethod('GetServerFileName', nil,
      'function: string',
      mlkVirtual, True);
    AddMethod('GetServerKey', nil,
      'function: string',
      mlkVirtual, True);
    AddMethod('GetServerName', nil,
      'function: string',
      mlkVirtual, True);
    AddMethod('GetStartSuspended', nil,
      'function: Boolean',
      mlkVirtual, True);
    AddMethod('GetTypeLib', nil,
      'function: ITypeLib',
      mlkVirtual, True);
    AddMethod('SetHelpFileName', nil,
      'procedure(const Value: string)',
      mlkVirtual, True);

    CurrentVisibility := mvPublic;

    AddProperty('HelpFileName', 'property: string',
      'GetHelpFileName', 'SetHelpFileName');
    AddProperty('ServerFileName', 'property: string',
      'GetServerFileName', '');
    AddProperty('ServerKey', 'property: string',
      'GetServerKey', '');
    AddProperty('ServerName', 'property: string',
      'GetServerName', '');
    AddProperty('TypeLib', 'property: ITypeLib',
      'GetTypeLib', '');
    AddProperty('StartSuspended', 'property: Boolean',
      'GetStartSuspended', '');

    Complete;
  end;
end;

{-------------------------}
{ TComClassManager import }
{-------------------------}

class function TSepiImportsTComClassManager.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComClassManager));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFactoryList', System.TypeInfo(TComObjectFactory));
    AddField('FLock', System.TypeInfo(TMultiReadExclusiveWriteSynchronizer));

    AddMethod('AddObjectFactory', nil,
      'procedure(Factory: TComObjectFactory)');
    AddMethod('RemoveObjectFactory', nil,
      'procedure(Factory: TComObjectFactory)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTComClassManager.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTComClassManager.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('ForEachFactory', @TSepiImportsTComClassManager.ForEachFactory,
      'procedure(ComServer: TComServerObject; FactoryProc: TFactoryProc )');
    AddMethod('GetFactoryFromClass', @TSepiImportsTComClassManager.GetFactoryFromClass,
      'function(ComClass: TClass): TComObjectFactory');
    AddMethod('GetFactoryFromClassID', @TSepiImportsTComClassManager.GetFactoryFromClassID,
      'function(const ClassID: TGUID): TComObjectFactory');

    Complete;
  end;
end;

{--------------------------------}
{ IServerExceptionHandler import }
{--------------------------------}

function SepiImportIServerExceptionHandler(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IServerExceptionHandler));

  with Result do
  begin
    AddMethod('OnException',
      'procedure( const ServerClass, ExceptionClass, ErrorMessage: WideString ; ExceptAddr: Integer ; const ErrorIID, ProgID: WideString ; var Handled: Integer ; var Result: HResult )', ccRegister);

    Complete;
  end;
end;

{-------------------}
{ TComObject import }
{-------------------}

function TSepiImportsTComObject.GetController: IUnknown;
begin
  Result := Controller;
end;

class function TSepiImportsTComObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComObject));

  with Result do
  begin
    AddInterface(System.TypeInfo(IUnknown));
    AddInterface(System.TypeInfo(ISupportErrorInfo));

    CurrentVisibility := mvPrivate;

    AddField('FController', 'Pointer');
    AddField('FFactory', System.TypeInfo(TComObjectFactory));
    AddField('FNonCountedObject', System.TypeInfo(Boolean));
    AddField('FRefCount', System.TypeInfo(Integer));
    AddField('FServerExceptionHandler', System.TypeInfo(IServerExceptionHandler));

    AddMethod('GetController', @TSepiImportsTComObject.GetController,
      'function: IUnknown');

    CurrentVisibility := mvProtected;


    AddMethod('QueryInterface', @TSepiImportsTComObject.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      ccStdCall);
    AddMethod('_AddRef', @TSepiImportsTComObject._AddRef,
      'function: Integer',
      ccStdCall);
    AddMethod('_Release', @TSepiImportsTComObject._Release,
      'function: Integer',
      ccStdCall);
    AddMethod('InterfaceSupportsErrorInfo', @TSepiImportsTComObject.InterfaceSupportsErrorInfo,
      'function(const iid: TIID): HResult',
      ccStdCall);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTComObject.Create,
      'constructor');
    AddMethod('CreateAggregated', @TSepiImportsTComObject.CreateAggregated,
      'constructor(const Controller: IUnknown)');
    AddMethod('CreateFromFactory', @TSepiImportsTComObject.CreateFromFactory,
      'constructor(Factory: TComObjectFactory; const Controller: IUnknown )');
    AddMethod('Destroy', @TSepiImportsTComObject.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Initialize', @TSepiImportsTComObject.Initialize,
      'procedure',
      mlkVirtual);
    AddMethod('ObjAddRef', @TSepiImportsTComObject.ObjAddRef,
      'function: Integer',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('ObjQueryInterface', @TSepiImportsTComObject.ObjQueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('ObjRelease', @TSepiImportsTComObject.ObjRelease,
      'function: Integer',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('SafeCallException', @TSepiImportsTComObject.SafeCallException,
      'function(ExceptObject: TObject; ExceptAddr: Pointer ) : HResult',
      mlkOverride);

    AddProperty('Controller', 'property: IUnknown',
      'GetController', '');
    AddProperty('Factory', 'property: TComObjectFactory',
      'FFactory', '');
    AddProperty('RefCount', 'property: Integer',
      'FRefCount', '');
    AddProperty('ServerExceptionHandler', 'property: IServerExceptionHandler',
      'FServerExceptionHandler', 'FServerExceptionHandler');

    Complete;
  end;
end;

{--------------------------}
{ TComObjectFactory import }
{--------------------------}

class function TSepiImportsTComObjectFactory.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TComObjectFactory'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TComObjectFactory));

  with Result do
  begin
    AddInterface(System.TypeInfo(IUnknown));
    AddInterface(System.TypeInfo(IClassFactory));
    AddInterface(System.TypeInfo(IClassFactory2));

    CurrentVisibility := mvPrivate;

    AddField('FNext', System.TypeInfo(TComObjectFactory));
    AddField('FComServer', System.TypeInfo(TComServerObject));
    AddField('FComClass', 'TClass');
    AddField('FClassID', 'TGUID');
    AddField('FClassName', System.TypeInfo(string));
    AddField('FDescription', System.TypeInfo(string));
    AddField('FErrorIID', 'TGUID');
    AddField('FInstancing', System.TypeInfo(TClassInstancing));
    AddField('FLicString', System.TypeInfo(WideString));
    AddField('FRegister', System.TypeInfo(Longint));
    AddField('FShowErrors', System.TypeInfo(Boolean));
    AddField('FSupportsLicensing', System.TypeInfo(Boolean));
    AddField('FThreadingModel', System.TypeInfo(TThreadingModel));

    CurrentVisibility := mvProtected;

    AddMethod('GetProgID', @TSepiImportsTComObjectFactory.GetProgID,
      'function: string',
      mlkVirtual);
    AddMethod('GetLicenseString', @TSepiImportsTComObjectFactory.GetLicenseString,
      'function: WideString',
      mlkVirtual);
    AddMethod('HasMachineLicense', @TSepiImportsTComObjectFactory.HasMachineLicense,
      'function: Boolean',
      mlkVirtual);
    AddMethod('ValidateUserLicense', @TSepiImportsTComObjectFactory.ValidateUserLicense,
      'function(const LicStr: WideString): Boolean',
      mlkVirtual);
    AddMethod('QueryInterface', @TSepiImportsTComObjectFactory.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      ccStdCall);
    AddMethod('_AddRef', @TSepiImportsTComObjectFactory._AddRef,
      'function: Integer',
      ccStdCall);
    AddMethod('_Release', @TSepiImportsTComObjectFactory._Release,
      'function: Integer',
      ccStdCall);
    AddMethod('CreateInstance', @TSepiImportsTComObjectFactory.CreateInstance,
      'function(const UnkOuter: IUnknown; const IID: TGUID; out Obj ) : HResult',
      ccStdCall);
    AddMethod('LockServer', @TSepiImportsTComObjectFactory.LockServer,
      'function(fLock: BOOL): HResult',
      ccStdCall);
    AddMethod('GetLicInfo', @TSepiImportsTComObjectFactory.GetLicInfo,
      'function(var licInfo: TLicInfo): HResult',
      ccStdCall);
    AddMethod('RequestLicKey', @TSepiImportsTComObjectFactory.RequestLicKey,
      'function(dwResrved: Longint; out bstrKey: WideString): HResult',
      ccStdCall);
    AddMethod('CreateInstanceLic', @TSepiImportsTComObjectFactory.CreateInstanceLic,
      'function(const unkOuter: IUnknown; const unkReserved: IUnknown; const iid: TIID ; const bstrKey: WideString ; out vObject ) : HResult',
      ccStdCall);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTComObjectFactory.Create,
      'constructor(ComServer: TComServerObject; ComClass: TComClass; const ClassID: TGUID ; const ClassName, Description: string ; Instancing: TClassInstancing ; ThreadingModel: TThreadingModel = tmSingle )');
    AddMethod('Destroy', @TSepiImportsTComObjectFactory.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('CreateComObject', @TSepiImportsTComObjectFactory.CreateComObject,
      'function(const Controller: IUnknown): TComObject',
      mlkVirtual);
    AddMethod('RegisterClassObject', @TSepiImportsTComObjectFactory.RegisterClassObject,
      'procedure');
    AddMethod('UpdateRegistry', @TSepiImportsTComObjectFactory.UpdateRegistry,
      'procedure(Register: Boolean)',
      mlkVirtual);

    AddProperty('ClassID', 'property: TGUID',
      'FClassID', '');
    AddProperty('ClassName', 'property: string',
      'FClassName', '');
    AddProperty('ComClass', 'property: TClass',
      'FComClass', '');
    AddProperty('ComServer', 'property: TComServerObject',
      'FComServer', '');
    AddProperty('Description', 'property: string',
      'FDescription', '');
    AddProperty('ErrorIID', 'property: TGUID',
      'FErrorIID', 'FErrorIID');
    AddProperty('LicString', 'property: WideString',
      'FLicString', 'FLicString');
    AddProperty('ProgID', 'property: string',
      'GetProgID', '');
    AddProperty('Instancing', 'property: TClassInstancing',
      'FInstancing', '');
    AddProperty('ShowErrors', 'property: Boolean',
      'FShowErrors', 'FShowErrors');
    AddProperty('SupportsLicensing', 'property: Boolean',
      'FSupportsLicensing', 'FSupportsLicensing');
    AddProperty('ThreadingModel', 'property: TThreadingModel',
      'FThreadingModel', '');

    Complete;
  end;
end;

{------------------------}
{ TTypedComObject import }
{------------------------}

class function TSepiImportsTTypedComObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTypedComObject));

  with Result do
  begin
    AddInterface(System.TypeInfo(IProvideClassInfo));

    CurrentVisibility := mvProtected;

    AddMethod('GetClassInfo', @TSepiImportsTTypedComObject.GetClassInfo,
      'function(out TypeInfo: ITypeInfo): HResult',
      ccStdCall);

    Complete;
  end;
end;

{-------------------------------}
{ TTypedComObjectFactory import }
{-------------------------------}

class function TSepiImportsTTypedComObjectFactory.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTypedComObjectFactory));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FClassInfo', System.TypeInfo(ITypeInfo));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTTypedComObjectFactory.Create,
      'constructor(ComServer: TComServerObject; TypedComClass: TTypedComClass ; const ClassID: TGUID ; Instancing: TClassInstancing ; ThreadingModel: TThreadingModel = tmSingle )');
    AddMethod('GetInterfaceTypeInfo', @TSepiImportsTTypedComObjectFactory.GetInterfaceTypeInfo,
      'function(TypeFlags: Integer): ITypeInfo');
    AddMethod('UpdateRegistry', @TSepiImportsTTypedComObjectFactory.UpdateRegistry,
      'procedure(Register: Boolean)',
      mlkOverride);

    AddProperty('ClassInfo', 'property: ITypeInfo',
      'FClassInfo', '');

    Complete;
  end;
end;

{--------------------}
{ TAutoObject import }
{--------------------}

class function TSepiImportsTAutoObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TAutoObject));

  with Result do
  begin
    AddInterface(System.TypeInfo(IDispatch));

    CurrentVisibility := mvPrivate;

    AddField('FEventSink', System.TypeInfo(IUnknown));
    AddField('FAutoFactory', System.TypeInfo(TAutoObjectFactory));

    CurrentVisibility := mvProtected;

    AddMethod('GetIDsOfNames', @TSepiImportsTAutoObject.GetIDsOfNames,
      'function(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer ; DispIDs: Pointer ) : HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('GetTypeInfo', @TSepiImportsTAutoObject.GetTypeInfo,
      'function(Index, LocaleID: Integer; out TypeInfo): HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('GetTypeInfoCount', @TSepiImportsTAutoObject.GetTypeInfoCount,
      'function(out Count: Integer): HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('Invoke', @TSepiImportsTAutoObject.Invoke,
      'function(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word ; var Params ; VarResult, ExcepInfo, ArgErr: Pointer ) : HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('EventConnect', @TSepiImportsTAutoObject.EventConnect,
      'procedure(const Sink: IUnknown; Connecting: Boolean)');
    AddMethod('EventSinkChanged', @TSepiImportsTAutoObject.EventSinkChanged,
      'procedure(const EventSink: IUnknown)',
      mlkVirtual);

    AddProperty('AutoFactory', 'property: TAutoObjectFactory',
      'FAutoFactory', '');
    AddProperty('EventSink', 'property: IUnknown',
      'FEventSink', 'FEventSink');

    CurrentVisibility := mvPublic;

    AddMethod('Initialize', @TSepiImportsTAutoObject.Initialize,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{---------------------------}
{ TAutoObjectFactory import }
{---------------------------}

class function TSepiImportsTAutoObjectFactory.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TAutoObjectFactory'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TAutoObjectFactory));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDispTypeInfo', System.TypeInfo(ITypeInfo));
    AddField('FDispIntfEntry', 'PInterfaceEntry');
    AddField('FEventIID', 'TGUID');
    AddField('FEventTypeInfo', System.TypeInfo(ITypeInfo));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTAutoObjectFactory.Create,
      'constructor(ComServer: TComServerObject; AutoClass: TAutoClass; const ClassID: TGUID ; Instancing: TClassInstancing ; ThreadingModel: TThreadingModel = tmSingle )');
    AddMethod('GetIntfEntry', @TSepiImportsTAutoObjectFactory.GetIntfEntry,
      'function(Guid: TGUID): PInterfaceEntry',
      mlkVirtual);

    AddProperty('DispIntfEntry', 'property: PInterfaceEntry',
      'FDispIntfEntry', '');
    AddProperty('DispTypeInfo', 'property: ITypeInfo',
      'FDispTypeInfo', '');
    AddProperty('EventIID', 'property: TGUID',
      'FEventIID', '');
    AddProperty('EventTypeInfo', 'property: ITypeInfo',
      'FEventTypeInfo', '');

    Complete;
  end;
end;

{------------------------}
{ TAutoIntfObject import }
{------------------------}

class function TSepiImportsTAutoIntfObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TAutoIntfObject));

  with Result do
  begin
    AddInterface(System.TypeInfo(IDispatch));
    AddInterface(System.TypeInfo(ISupportErrorInfo));

    CurrentVisibility := mvPrivate;

    AddField('FDispTypeInfo', System.TypeInfo(ITypeInfo));
    AddField('FDispIntfEntry', 'PInterfaceEntry');
    AddField('FDispIID', 'TGUID');

    CurrentVisibility := mvProtected;

    AddMethod('GetIDsOfNames', @TSepiImportsTAutoIntfObject.GetIDsOfNames,
      'function(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer ; DispIDs: Pointer ) : HResult',
      ccStdCall);
    AddMethod('GetTypeInfo', @TSepiImportsTAutoIntfObject.GetTypeInfo,
      'function(Index, LocaleID: Integer; out TypeInfo): HResult',
      ccStdCall);
    AddMethod('GetTypeInfoCount', @TSepiImportsTAutoIntfObject.GetTypeInfoCount,
      'function(out Count: Integer): HResult',
      ccStdCall);
    AddMethod('Invoke', @TSepiImportsTAutoIntfObject.Invoke,
      'function(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word ; var Params ; VarResult, ExcepInfo, ArgErr: Pointer ) : HResult',
      ccStdCall);
    AddMethod('InterfaceSupportsErrorInfo', @TSepiImportsTAutoIntfObject.InterfaceSupportsErrorInfo,
      'function(const iid: TIID): HResult',
      ccStdCall);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTAutoIntfObject.Create,
      'constructor(const TypeLib: ITypeLib; const DispIntf: TGUID)');
    AddMethod('SafeCallException', @TSepiImportsTAutoIntfObject.SafeCallException,
      'function(ExceptObject: TObject; ExceptAddr: Pointer ) : HResult',
      mlkOverride);

    AddProperty('DispIntfEntry', 'property: PInterfaceEntry',
      'FDispIntfEntry', '');
    AddProperty('DispTypeInfo', 'property: ITypeInfo',
      'FDispTypeInfo', '');
    AddProperty('DispIID', 'property: TGUID',
      'FDispIID', '');

    Complete;
  end;
end;

{------------------}
{ EOleError import }
{------------------}

class function TSepiImportsEOleError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOleError));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------}
{ EOleSysError import }
{---------------------}

class function TSepiImportsEOleSysError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOleSysError));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FErrorCode', System.TypeInfo(HRESULT));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsEOleSysError.Create,
      'constructor(const Message: string; ErrorCode: HRESULT; HelpContext: Integer )');

    AddProperty('ErrorCode', 'property: HRESULT',
      'FErrorCode', 'FErrorCode');

    Complete;
  end;
end;

{----------------------}
{ EOleException import }
{----------------------}

class function TSepiImportsEOleException.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOleException));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FSource', System.TypeInfo(string));
    AddField('FHelpFile', System.TypeInfo(string));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsEOleException.Create,
      'constructor(const Message: string; ErrorCode: HRESULT; const Source, HelpFile: string ; HelpContext: Integer )');

    AddProperty('HelpFile', 'property: string',
      'FHelpFile', 'FHelpFile');
    AddProperty('Source', 'property: string',
      'FSource', 'FSource');

    Complete;
  end;
end;

{------------------------------}
{ EOleRegistrationError import }
{------------------------------}

class function TSepiImportsEOleRegistrationError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOleRegistrationError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function GetDispatchPropValue_0(Disp: IDispatch; DispID: Integer): OleVariant;
begin
  Result := GetDispatchPropValue(Disp, DispID);
end;

function GetDispatchPropValue_1(Disp: IDispatch; Name: WideString): OleVariant;
begin
  Result := GetDispatchPropValue(Disp, Name);
end;

procedure SetDispatchPropValue_0(Disp: IDispatch; DispID: Integer; const Value: OleVariant );
begin
  SetDispatchPropValue(Disp, DispID, Value);
end;

procedure SetDispatchPropValue_1(Disp: IDispatch; Name: WideString; const Value: OleVariant );
begin
  SetDispatchPropValue(Disp, Name, Value);
end;

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ComObj',
    ['Variants', 'Windows', 'ActiveXTypes', 'SysUtils']);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TComObjectFactory));
  TSepiImportsTComServerObject.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFactoryProc));
  TSepiImportsTComClassManager.SepiImport(Result);
  SepiImportIServerExceptionHandler(Result);
  TSepiImportsTComObject.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TComClass', TypeInfo(TComObject), True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TClassInstancing));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TThreadingModel));
  TSepiImportsTComObjectFactory.SepiImport(Result);
  TSepiImportsTTypedComObject.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TTypedComClass', TypeInfo(TTypedComObject), True);
  TSepiImportsTTypedComObjectFactory.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConnectEvent));
  TSepiClass.ForwardDecl(Result, TypeInfo(TAutoObjectFactory));
  TSepiImportsTAutoObject.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TAutoClass', TypeInfo(TAutoObject), True);
  TSepiImportsTAutoObjectFactory.SepiImport(Result);
  TSepiImportsTAutoIntfObject.SepiImport(Result);
  TSepiImportsEOleError.SepiImport(Result);
  TSepiImportsEOleSysError.SepiImport(Result);
  TSepiImportsEOleException.SepiImport(Result);
  TSepiImportsEOleRegistrationError.SepiImport(Result);

  // Routines
  TSepiMetaMethod.Create(Result, 'DispatchInvoke', @DispatchInvoke,
    'procedure(const Dispatch: IDispatch; CallDesc: PCallDesc; DispIDs: PDispIDList ; Params: Pointer ; Result: PVariant )');
  TSepiMetaMethod.Create(Result, 'DispatchInvokeError', @DispatchInvokeError,
    'procedure(Status: Integer; const ExcepInfo: TExcepInfo)');
  TSepiMetaMethod.Create(Result, 'HandleSafeCallException', @HandleSafeCallException,
    'function(ExceptObject: TObject; ExceptAddr: Pointer ; const ErrorIID: TGUID ; const ProgID, HelpFileName : WideString ) : HResult');
  TSepiMetaMethod.Create(Result, 'CreateComObject', @CreateComObject,
    'function(const ClassID: TGUID): IUnknown');
  TSepiMetaMethod.Create(Result, 'CreateRemoteComObject', @CreateRemoteComObject,
    'function(const MachineName: WideString; const ClassID: TGUID): IUnknown');
  TSepiMetaMethod.Create(Result, 'CreateOleObject', @CreateOleObject,
    'function(const ClassName: string): IDispatch');
  TSepiMetaMethod.Create(Result, 'GetActiveOleObject', @GetActiveOleObject,
    'function(const ClassName: string): IDispatch');
  TSepiMetaMethod.Create(Result, 'OleError', @OleError,
    'procedure(ErrorCode: HResult)');
  TSepiMetaMethod.Create(Result, 'OleCheck', @OleCheck,
    'procedure(Result: HResult)');
  TSepiMetaMethod.Create(Result, 'StringToGUID', @StringToGUID,
    'function(const S: string): TGUID');
  TSepiMetaMethod.Create(Result, 'GUIDToString', @GUIDToString,
    'function(const ClassID: TGUID): string');
  TSepiMetaMethod.Create(Result, 'ProgIDToClassID', @ProgIDToClassID,
    'function(const ProgID: string): TGUID');
  TSepiMetaMethod.Create(Result, 'ClassIDToProgID', @ClassIDToProgID,
    'function(const ClassID: TGUID): string');
  TSepiMetaMethod.Create(Result, 'CreateRegKey', @CreateRegKey,
    'procedure(const Key, ValueName, Value: string; RootKey: DWord = HKEY_CLASSES_ROOT)');
  TSepiMetaMethod.Create(Result, 'DeleteRegKey', @DeleteRegKey,
    'procedure(const Key: string; RootKey: DWord = HKEY_CLASSES_ROOT)');
  TSepiMetaMethod.Create(Result, 'GetRegStringValue', @GetRegStringValue,
    'function(const Key, ValueName: string; RootKey: DWord = HKEY_CLASSES_ROOT): string');
  TSepiMetaMethod.Create(Result, 'StringToLPOLESTR', @StringToLPOLESTR,
    'function(const Source: string): POleStr');
  TSepiMetaMethod.Create(Result, 'RegisterComServer', @RegisterComServer,
    'procedure(const DLLName: string)');
  TSepiMetaMethod.Create(Result, 'RegisterAsService', @RegisterAsService,
    'procedure(const ClassID, ServiceName: string)');
  TSepiMetaMethod.Create(Result, 'CreateClassID', @CreateClassID,
    'function: string');
  TSepiMetaMethod.Create(Result, 'InterfaceConnect', @InterfaceConnect,
    'procedure(const Source: IUnknown; const IID: TIID; const Sink: IUnknown ; var Connection: Longint )');
  TSepiMetaMethod.Create(Result, 'InterfaceDisconnect', @InterfaceDisconnect,
    'procedure(const Source: IUnknown; const IID: TIID; var Connection: Longint )');
  TSepiMetaOverloadedMethod.Create(Result, 'GetDispatchPropValue');
  TSepiMetaMethod.Create(Result, 'OL$GetDispatchPropValue$0', @GetDispatchPropValue_0,
    'function(Disp: IDispatch; DispID: Integer): OleVariant');
  TSepiMetaMethod.Create(Result, 'OL$GetDispatchPropValue$1', @GetDispatchPropValue_1,
    'function(Disp: IDispatch; Name: WideString): OleVariant');
  TSepiMetaOverloadedMethod.Create(Result, 'SetDispatchPropValue');
  TSepiMetaMethod.Create(Result, 'OL$SetDispatchPropValue$0', @SetDispatchPropValue_0,
    'procedure(Disp: IDispatch; DispID: Integer; const Value: OleVariant )');
  TSepiMetaMethod.Create(Result, 'OL$SetDispatchPropValue$1', @SetDispatchPropValue_1,
    'procedure(Disp: IDispatch; Name: WideString; const Value: OleVariant )');

  // Types
  TSepiMethodRefType.Create(Result, 'TCoCreateInstanceExProc',
    'function(const clsid: TCLSID; unkOuter: IUnknown ; dwClsCtx: Longint ; ServerInfo: PCoServerInfo ; dwCount: Longint ; rgmqResults: PMultiQIArray ) : HResult', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TCoInitializeExProc',
    'function(pvReserved: Pointer; coInit: Longint ) : HResult', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TCoAddRefServerProcessProc',
    'function:Longint', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TCoReleaseServerProcessProc',
    'function:Longint', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TCoResumeClassObjectsProc',
    'function:HResult', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TCoSuspendClassObjectsProc',
    'function:HResult', False, ccStdCall);

  // Global variables
  TSepiVariable.Create(Result, 'CoCreateInstanceEx',
    @CoCreateInstanceEx, 'TCoCreateInstanceExProc');
  TSepiVariable.Create(Result, 'CoInitializeEx',
    @CoInitializeEx, 'TCoInitializeExProc');
  TSepiVariable.Create(Result, 'CoAddRefServerProcess',
    @CoAddRefServerProcess, 'TCoAddRefServerProcessProc');
  TSepiVariable.Create(Result, 'CoReleaseServerProcess',
    @CoReleaseServerProcess, 'TCoReleaseServerProcessProc');
  TSepiVariable.Create(Result, 'CoResumeClassObjects',
    @CoResumeClassObjects, 'TCoResumeClassObjectsProc');
  TSepiVariable.Create(Result, 'CoSuspendClassObjects',
    @CoSuspendClassObjects, 'TCoSuspendClassObjectsProc');

  // Global variables
  TSepiVariable.Create(Result, 'CoInitFlags',
     CoInitFlags, TypeInfo(Integer));

  // Routines
  TSepiMetaMethod.Create(Result, 'ComClassManager', @ComClassManager,
    'function: TComClassManager');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ComObj', ImportUnit);
end.

