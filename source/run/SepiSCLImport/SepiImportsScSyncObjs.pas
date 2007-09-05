{*
  Importe l'unité ScSyncObjs dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScSyncObjs;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, SyncObjs, ScLists, ScSyncObjs;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTScTask = class(TScTask)
  private
    constructor Create_0(AOwner : TScCustomTaskQueue; AFreeOnFinished : boolean );
    constructor Create_1(AOwner : TScCustomTaskQueue);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScCustomTaskQueue = class(TScCustomTaskQueue)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScTaskQueue = class(TScTaskQueue)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScCustomMessageTask = class(TScCustomMessageTask)
  private
    constructor Create_0(AOwner : TScCustomTaskQueue; const AMsg; AMsgSize : integer ; ADestObj : TObject ; AFreeOnFinished : boolean );
    constructor Create_1(AOwner : TScCustomTaskQueue; const AMsg; AMsgSize : integer ; ADestObj : TObject );
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScMessageTask = class(TScMessageTask)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{----------------}
{ TScTask import }
{----------------}

constructor TSepiImportsTScTask.Create_0(AOwner : TScCustomTaskQueue; AFreeOnFinished : boolean );
begin
  Create(AOwner, AFreeOnFinished);
end;

constructor TSepiImportsTScTask.Create_1(AOwner : TScCustomTaskQueue);
begin
  Create(AOwner);
end;

class function TSepiImportsTScTask.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScTask));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOwner', System.TypeInfo(TScCustomTaskQueue));
    AddField('FState', System.TypeInfo(TScTaskState));
    AddField('FFreeOnFinished', System.TypeInfo(boolean));
    AddField('FWaitForCount', System.TypeInfo(integer));
    AddField('FFatalException', System.TypeInfo(TObject));

    AddMethod('Finish', nil,
      'procedure(AFatalException : TObject = nil)');

    CurrentVisibility := mvProtected;

    AddMethod('Execute', nil,
      'procedure',
      mlkVirtual, True);
    AddMethod('Cancel', @TSepiImportsTScTask.Cancel,
      'function(ForceFree : boolean = False) : boolean');

    AddProperty('Owner', 'property: TScCustomTaskQueue',
      'FOwner', '');
    AddProperty('State', 'property: TScTaskState',
      'FState', '');
    AddProperty('FreeOnFinished', 'property: boolean',
      'FFreeOnFinished', 'FFreeOnFinished');
    AddProperty('FatalException', 'property: TObject',
      'FFatalException', '');

    CurrentVisibility := mvPublic;

    AddOverloadedMethod('Create', @TSepiImportsTScTask.Create_0,
      'constructor(AOwner : TScCustomTaskQueue; AFreeOnFinished : boolean )');
    AddOverloadedMethod('Create', @TSepiImportsTScTask.Create_1,
      'constructor(AOwner : TScCustomTaskQueue)');
    AddMethod('Destroy', @TSepiImportsTScTask.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AfterConstruction', @TSepiImportsTScTask.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('BeforeDestruction', @TSepiImportsTScTask.BeforeDestruction,
      'procedure',
      mlkOverride);
    AddMethod('WaitFor', @TSepiImportsTScTask.WaitFor,
      'procedure');
    AddMethod('RaiseException', @TSepiImportsTScTask.RaiseException,
      'procedure');

    Complete;
  end;
end;

{---------------------------}
{ TScCustomTaskQueue import }
{---------------------------}

class function TSepiImportsTScCustomTaskQueue.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TScCustomTaskQueue'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TScCustomTaskQueue));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FWaitingQueue', System.TypeInfo(TScWaitingObjectQueue));
    AddField('FCriticalSection', System.TypeInfo(TCriticalSection));
    AddField('FDefaultFreeOnFinished', System.TypeInfo(boolean));
    AddField('FTerminateOnException', System.TypeInfo(boolean));
    AddField('FReady', System.TypeInfo(boolean));

    AddMethod('Push', nil,
      'procedure(Task : TScTask)');
    AddMethod('Cancel', nil,
      'procedure(Task : TScTask)');
    AddMethod('Pop', nil,
      'function: TScTask');

    CurrentVisibility := mvProtected;

    AddMethod('Execute', @TSepiImportsTScCustomTaskQueue.Execute,
      'procedure',
      mlkOverride);

    AddProperty('WaitingQueue', 'property: TScWaitingObjectQueue',
      'FWaitingQueue', '');
    AddProperty('CriticalSection', 'property: TCriticalSection',
      'FCriticalSection', '');
    AddProperty('DefaultFreeOnFinished', 'property: boolean',
      'FDefaultFreeOnFinished', 'FDefaultFreeOnFinished');
    AddProperty('TerminateOnException', 'property: boolean',
      'FTerminateOnException', 'FTerminateOnException');
    AddProperty('Ready', 'property: boolean',
      'FReady', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScCustomTaskQueue.Create,
      'constructor(ADefaultFreeOnFinished : boolean = True; ATerminateOnException : boolean = True )');
    AddMethod('Destroy', @TSepiImportsTScCustomTaskQueue.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('BeforeDestruction', @TSepiImportsTScCustomTaskQueue.BeforeDestruction,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ TScTaskQueue import }
{---------------------}

class function TSepiImportsTScTaskQueue.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScTaskQueue));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    RedefineProperty('DefaultFreeOnFinished');
    RedefineProperty('TerminateOnException');
    RedefineProperty('Ready');

    Complete;
  end;
end;

{-----------------------------}
{ TScCustomMessageTask import }
{-----------------------------}

constructor TSepiImportsTScCustomMessageTask.Create_0(AOwner : TScCustomTaskQueue; const AMsg; AMsgSize : integer ; ADestObj : TObject ; AFreeOnFinished : boolean );
begin
  Create(AOwner, AMsg, AMsgSize, ADestObj, AFreeOnFinished);
end;

constructor TSepiImportsTScCustomMessageTask.Create_1(AOwner : TScCustomTaskQueue; const AMsg; AMsgSize : integer ; ADestObj : TObject );
begin
  Create(AOwner, AMsg, AMsgSize, ADestObj);
end;

class function TSepiImportsTScCustomMessageTask.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScCustomMessageTask));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMsg', 'Pointer');
    AddField('FMsgSize', System.TypeInfo(integer));
    AddField('FDestObj', System.TypeInfo(TObject));

    CurrentVisibility := mvProtected;

    AddMethod('Execute', @TSepiImportsTScCustomMessageTask.Execute,
      'procedure',
      mlkOverride);
    AddMethod('GetMsg', @TSepiImportsTScCustomMessageTask.GetMsg,
      'procedure(out Msg)');

    AddProperty('Msg', 'property: Pointer',
      'FMsg', '');
    AddProperty('MsgSize', 'property: integer',
      'FMsgSize', '');
    AddProperty('DestObj', 'property: TObject',
      'FDestObj', '');

    CurrentVisibility := mvPublic;

    AddOverloadedMethod('Create', @TSepiImportsTScCustomMessageTask.Create_0,
      'constructor(AOwner : TScCustomTaskQueue; const AMsg; AMsgSize : integer ; ADestObj : TObject ; AFreeOnFinished : boolean )');
    AddOverloadedMethod('Create', @TSepiImportsTScCustomMessageTask.Create_1,
      'constructor(AOwner : TScCustomTaskQueue; const AMsg; AMsgSize : integer ; ADestObj : TObject )');
    AddMethod('Destroy', @TSepiImportsTScCustomMessageTask.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{-----------------------}
{ TScMessageTask import }
{-----------------------}

class function TSepiImportsTScMessageTask.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScMessageTask));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Cancel', @TSepiImportsTScMessageTask.Cancel,
      'function(ForceFree : boolean = False) : boolean');
    AddMethod('GetMsg', @TSepiImportsTScMessageTask.GetMsg,
      'procedure(out Msg)');

    RedefineProperty('Owner');
    RedefineProperty('State');
    RedefineProperty('FreeOnFinished');
    RedefineProperty('Msg');
    RedefineProperty('DestObj');
    RedefineProperty('FatalException');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScSyncObjs',
    ['Classes', 'Contnrs', 'SyncObjs', 'ScLists']);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TScCustomTaskQueue));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TScTaskState));
  TSepiImportsTScTask.SepiImport(Result);
  TSepiImportsTScCustomTaskQueue.SepiImport(Result);
  TSepiImportsTScTaskQueue.SepiImport(Result);
  TSepiImportsTScCustomMessageTask.SepiImport(Result);
  TSepiImportsTScMessageTask.SepiImport(Result);

  // Routines
  TSepiMetaMethod.Create(Result, 'InterlockedIncrement', @InterlockedIncrement,
    'function(var I : integer) : integer');
  TSepiMetaMethod.Create(Result, 'InterlockedDecrement', @InterlockedDecrement,
    'function(var I : integer) : integer');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScSyncObjs', ImportUnit);
end.

