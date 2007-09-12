{*
  Importe l'unité SyncObjs dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsSyncObjs;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, SyncObjs, Windows;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTSynchroObject = class(TSynchroObject)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTHandleObject = class(THandleObject)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTEvent = class(TEvent)
  private
    constructor Create_0(EventAttributes: PSecurityAttributes;
      ManualReset, InitialState: Boolean; const Name: string;
      UseCOMWait: Boolean = False);
    constructor Create_1(UseCOMWait: Boolean = False);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTSimpleEvent = class(TSimpleEvent)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTMutex = class(TMutex)
  private
    constructor Create_0(UseCOMWait: Boolean = False);
    constructor Create_1(MutexAttributes: PSecurityAttributes;
      InitialOwner: Boolean; const Name: string; UseCOMWait: Boolean = False);
    constructor Create_2(DesiredAccess: LongWord; InheritHandle: Boolean;
      const Name: string; UseCOMWait: Boolean = False);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCriticalSection = class(TCriticalSection)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{-----------------------}
{ TSynchroObject import }
{-----------------------}

class function TSepiImportsTSynchroObject.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSynchroObject));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Acquire', @TSepiImportsTSynchroObject.Acquire,
      'procedure',
      mlkVirtual);
    AddMethod('Release', @TSepiImportsTSynchroObject.Release,
      'procedure',
      mlkVirtual);

    Complete;
  end;
end;

{----------------------}
{ THandleObject import }
{----------------------}

class function TSepiImportsTHandleObject.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(THandleObject));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FHandle', System.TypeInfo(THandle));
    AddField('FLastError', System.TypeInfo(Integer));
    AddField('FUseCOMWait', System.TypeInfo(Boolean));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTHandleObject.Create,
      'constructor(UseCOMWait: Boolean = False)');
    AddMethod('Destroy', @TSepiImportsTHandleObject.Destroy,
      'destructor',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('WaitFor', @TSepiImportsTHandleObject.WaitFor,
      'function(Timeout: LongWord): TWaitResult',
      mlkVirtual);

    AddProperty('LastError', 'property: Integer',
      'FLastError', '');
    AddProperty('Handle', 'property: THandle',
      'FHandle', '');

    Complete;
  end;
end;

{---------------}
{ TEvent import }
{---------------}

constructor TSepiImportsTEvent.Create_0(EventAttributes: PSecurityAttributes;
  ManualReset, InitialState: Boolean; const Name: string;
  UseCOMWait: Boolean = False);
begin
  Create(EventAttributes, ManualReset, InitialState, Name, UseCOMWait);
end;

constructor TSepiImportsTEvent.Create_1(UseCOMWait: Boolean = False);
begin
  Create(UseCOMWait);
end;

class function TSepiImportsTEvent.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TEvent));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddOverloadedMethod('Create', @TSepiImportsTEvent.Create_0,
      'constructor(EventAttributes: PSecurityAttributes; ManualReset, InitialState : Boolean ; const Name: string ; UseCOMWait: Boolean = False )');
    AddOverloadedMethod('Create', @TSepiImportsTEvent.Create_1,
      'constructor(UseCOMWait: Boolean = False)');
    AddMethod('SetEvent', @TSepiImportsTEvent.SetEvent,
      'procedure');
    AddMethod('ResetEvent', @TSepiImportsTEvent.ResetEvent,
      'procedure');

    Complete;
  end;
end;

{---------------------}
{ TSimpleEvent import }
{---------------------}

class function TSepiImportsTSimpleEvent.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSimpleEvent));

  with Result do
  begin

    Complete;
  end;
end;

{---------------}
{ TMutex import }
{---------------}

constructor TSepiImportsTMutex.Create_0(UseCOMWait: Boolean = False);
begin
  Create(UseCOMWait);
end;

constructor TSepiImportsTMutex.Create_1(MutexAttributes: PSecurityAttributes;
  InitialOwner: Boolean; const Name: string; UseCOMWait: Boolean = False);
begin
  Create(MutexAttributes, InitialOwner, Name, UseCOMWait);
end;

constructor TSepiImportsTMutex.Create_2(DesiredAccess: LongWord;
  InheritHandle: Boolean; const Name: string; UseCOMWait: Boolean = False);
begin
  Create(DesiredAccess, InheritHandle, Name, UseCOMWait);
end;

class function TSepiImportsTMutex.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMutex));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddOverloadedMethod('Create', @TSepiImportsTMutex.Create_0,
      'constructor(UseCOMWait: Boolean = False)');
    AddOverloadedMethod('Create', @TSepiImportsTMutex.Create_1,
      'constructor(MutexAttributes: PSecurityAttributes; InitialOwner: Boolean; const Name: string; UseCOMWait: Boolean = False)');
    AddOverloadedMethod('Create', @TSepiImportsTMutex.Create_2,
      'constructor(DesiredAccess: LongWord; InheritHandle: Boolean; const Name: string; UseCOMWait: Boolean = False)');
    AddMethod('Acquire', @TSepiImportsTMutex.Acquire,
      'procedure',
      mlkOverride);
    AddMethod('Release', @TSepiImportsTMutex.Release,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{-------------------------}
{ TCriticalSection import }
{-------------------------}

class function TSepiImportsTCriticalSection.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCriticalSection));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FSection', 'TRTLCriticalSection');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCriticalSection.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTCriticalSection.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Acquire', @TSepiImportsTCriticalSection.Acquire,
      'procedure',
      mlkOverride);
    AddMethod('Release', @TSepiImportsTCriticalSection.Release,
      'procedure',
      mlkOverride);
    AddMethod('TryEnter', @TSepiImportsTCriticalSection.TryEnter,
      'function: Boolean');
    AddMethod('Enter', @TSepiImportsTCriticalSection.Enter,
      'procedure');
    AddMethod('Leave', @TSepiImportsTCriticalSection.Leave,
      'procedure');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'SyncObjs',
    ['Windows', 'Messages', 'SysUtils', 'Classes']);

  // Types
  TSepiImportsTSynchroObject.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TWaitResult));
  TSepiImportsTHandleObject.SepiImport(Result);
  TSepiImportsTEvent.SepiImport(Result);
  TSepiImportsTSimpleEvent.SepiImport(Result);
  TSepiImportsTMutex.SepiImport(Result);
  TSepiImportsTCriticalSection.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('SyncObjs', ImportUnit);
end.

