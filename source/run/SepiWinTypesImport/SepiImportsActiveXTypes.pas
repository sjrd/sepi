{*
  Importe l'unité ActiveX dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsActiveXTypes;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ActiveX, Windows;

implementation

{ You must not localize any of the strings this unit contains! }

{------------------}
{ _OBJECTID import }
{------------------}

function SepiImport_OBJECTID(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_OBJECTID', False, True);

  with Result do
  begin
    AddField('Lineage', 'TGUID');
    AddField('Uniquifier', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ _COSERVERINFO import }
{----------------------}

function SepiImport_COSERVERINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_COSERVERINFO', False, True);

  with Result do
  begin
    AddField('dwReserved1', System.TypeInfo(Longint));
    AddField('pwszName', 'LPWSTR');
    AddField('pAuthInfo', 'Pointer');
    AddField('dwReserved2', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ tagMULTI_QI import }
{--------------------}

function SepiImporttagMULTI_QI(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagMULTI_QI', False, True,
    TypeInfo(tagMULTI_QI));

  with Result do
  begin
    AddField('IID', 'PIID');
    AddField('Itf', System.TypeInfo(IUnknown));
    AddField('hr', System.TypeInfo(HRESULT));

    Complete;
  end;
end;

{--------------------------}
{ tagSAFEARRAYBOUND import }
{--------------------------}

function SepiImporttagSAFEARRAYBOUND(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSAFEARRAYBOUND', False, True);

  with Result do
  begin
    AddField('cElements', System.TypeInfo(Longint));
    AddField('lLbound', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ tagSAFEARRAY import }
{---------------------}

function SepiImporttagSAFEARRAY(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSAFEARRAY', False, True);

  with Result do
  begin
    AddField('cDims', System.TypeInfo(Word));
    AddField('fFeatures', System.TypeInfo(Word));
    AddField('cbElements', System.TypeInfo(Longint));
    AddField('cLocks', System.TypeInfo(Longint));
    AddField('pvData', 'Pointer');
    AddField('rgsabound', '$1');

    Complete;
  end;
end;

{------------------}
{ tagPOINTF import }
{------------------}

function SepiImporttagPOINTF(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPOINTF', False, True);

  with Result do
  begin
    AddField('x', System.TypeInfo(Single));
    AddField('y', System.TypeInfo(Single));

    Complete;
  end;
end;

{-----------------------}
{ tagCONTROLINFO import }
{-----------------------}

function SepiImporttagCONTROLINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCONTROLINFO', False, True);

  with Result do
  begin
    AddField('cb', System.TypeInfo(Longint));
    AddField('hAccel', System.TypeInfo(HAccel));
    AddField('cAccel', System.TypeInfo(Word));
    AddField('dwFlags', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ IClassFactory import }
{----------------------}

function SepiImportIClassFactory(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IClassFactory));

  with Result do
  begin
    AddMethod('CreateInstance',
      'function(const unkOuter: IUnknown; const iid: TIID; out obj ) : HResult', ccStdCall);
    AddMethod('LockServer',
      'function(fLock: BOOL): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------}
{ IMarshal import }
{-----------------}

function SepiImportIMarshal(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IMarshal));

  with Result do
  begin
    AddMethod('GetUnmarshalClass',
      'function(const iid: TIID; pv: Pointer; dwDestContext: Longint ; pvDestContext: Pointer ; mshlflags: Longint ; out cid: TCLSID ) : HResult', ccStdCall);
    AddMethod('GetMarshalSizeMax',
      'function(const iid: TIID; pv: Pointer; dwDestContext: Longint ; pvDestContext: Pointer ; mshlflags: Longint ; out size: Longint ) : HResult', ccStdCall);
    AddMethod('MarshalInterface',
      'function(const stm: IStream; const iid: TIID; pv: Pointer; dwDestContext: Longint ; pvDestContext: Pointer ; mshlflags: Longint ) : HResult', ccStdCall);
    AddMethod('UnmarshalInterface',
      'function(const stm: IStream; const iid: TIID; out pv ) : HResult', ccStdCall);
    AddMethod('ReleaseMarshalData',
      'function(const stm: IStream): HResult', ccStdCall);
    AddMethod('DisconnectObject',
      'function(dwReserved: Longint): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------}
{ IMalloc import }
{----------------}

function SepiImportIMalloc(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IMalloc));

  with Result do
  begin
    AddMethod('Alloc',
      'function(cb: Longint): Pointer', ccStdCall);
    AddMethod('Realloc',
      'function(pv: Pointer; cb: Longint): Pointer', ccStdCall);
    AddMethod('Free',
      'procedure(pv: Pointer)', ccStdCall);
    AddMethod('GetSize',
      'function(pv: Pointer): Longint', ccStdCall);
    AddMethod('DidAlloc',
      'function(pv: Pointer): Integer', ccStdCall);
    AddMethod('HeapMinimize',
      'procedure', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ IMallocSpy import }
{-------------------}

function SepiImportIMallocSpy(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IMallocSpy));

  with Result do
  begin
    AddMethod('PreAlloc',
      'function(cbRequest: Longword): Longint', ccStdCall);
    AddMethod('PostAlloc',
      'function(pActual: Pointer): Pointer', ccStdCall);
    AddMethod('PreFree',
      'function(pRequest: Pointer; fSpyed: BOOL): Pointer', ccStdCall);
    AddMethod('PostFree',
      'procedure(fSpyed: BOOL)', ccStdCall);
    AddMethod('PreRealloc',
      'function(pRequest: Pointer; cbRequest: Longword; out ppNewRequest: Pointer ; fSpyed: BOOL ) : Longword', ccStdCall);
    AddMethod('PostRealloc',
      'function(pActual: Pointer; fSpyed: BOOL): Pointer', ccStdCall);
    AddMethod('PreGetSize',
      'function(pRequest: Pointer; fSpyed: BOOL): Pointer', ccStdCall);
    AddMethod('PostGetSize',
      'function(pActual: Longword; fSpyed: BOOL): Longword', ccStdCall);
    AddMethod('PreDidAlloc',
      'function(pRequest: Pointer; fSpyed: BOOL): Pointer', ccStdCall);
    AddMethod('PostDidAlloc',
      'function(pRequest: Pointer; fSpyed: BOOL; fActual: Integer): Integer', ccStdCall);
    AddMethod('PreHeapMinimize',
      'procedure', ccStdCall);
    AddMethod('PostHeapMinimize',
      'procedure', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IStdMarshalInfo import }
{------------------------}

function SepiImportIStdMarshalInfo(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IStdMarshalInfo));

  with Result do
  begin
    AddMethod('GetClassForHandler',
      'function(dwDestContext: Longint; pvDestContext: Pointer; out clsid: TCLSID ) : HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------}
{ IExternalConnection import }
{----------------------------}

function SepiImportIExternalConnection(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IExternalConnection));

  with Result do
  begin
    AddMethod('AddConnection',
      'function(extconn: Longint; reserved: Longint): Longint', ccStdCall);
    AddMethod('ReleaseConnection',
      'function(extconn: Longint; reserved: Longint; fLastReleaseCloses: BOOL ) : Longint', ccStdCall);

    Complete;
  end;
end;

{-----------------}
{ IWeakRef import }
{-----------------}

function SepiImportIWeakRef(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IWeakRef));

  with Result do
  begin
    AddMethod('ChangeWeakCount',
      'function(delta: Longint): Longint', ccStdCall);
    AddMethod('ReleaseKeepAlive',
      'function(const unkReleased: IUnknown; reserved: Longint ) : Longint', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IEnumUnknown import }
{---------------------}

function SepiImportIEnumUnknown(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IEnumUnknown));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: Longint; out elt; pceltFetched: PLongint ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: Longint): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out enm: IEnumUnknown): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ tagBIND_OPTS import }
{---------------------}

function SepiImporttagBIND_OPTS(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagBIND_OPTS', False, True);

  with Result do
  begin
    AddField('cbStruct', System.TypeInfo(Longint));
    AddField('grfFlags', System.TypeInfo(Longint));
    AddField('grfMode', System.TypeInfo(Longint));
    AddField('dwTickCountDeadline', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------}
{ IBindCtx import }
{-----------------}

function SepiImportIBindCtx(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IBindCtx));

  with Result do
  begin
    AddMethod('RegisterObjectBound',
      'function(const unk: IUnknown): HResult', ccStdCall);
    AddMethod('RevokeObjectBound',
      'function(const unk: IUnknown): HResult', ccStdCall);
    AddMethod('ReleaseBoundObjects',
      'function: HResult', ccStdCall);
    AddMethod('SetBindOptions',
      'function(const bindopts: TBindOpts): HResult', ccStdCall);
    AddMethod('GetBindOptions',
      'function(var bindopts: TBindOpts): HResult', ccStdCall);
    AddMethod('GetRunningObjectTable',
      'function(out rot: IRunningObjectTable): HResult', ccStdCall);
    AddMethod('RegisterObjectParam',
      'function(pszKey: POleStr; const unk: IUnknown): HResult', ccStdCall);
    AddMethod('GetObjectParam',
      'function(pszKey: POleStr; out unk: IUnknown): HResult', ccStdCall);
    AddMethod('EnumObjectParam',
      'function(out Enum: IEnumString): HResult', ccStdCall);
    AddMethod('RevokeObjectParam',
      'function(pszKey: POleStr): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IEnumMoniker import }
{---------------------}

function SepiImportIEnumMoniker(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IEnumMoniker));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: Longint; out elt; pceltFetched: PLongint ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: Longint): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out enm: IEnumMoniker): HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IRunnableObject import }
{------------------------}

function SepiImportIRunnableObject(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IRunnableObject));

  with Result do
  begin
    AddMethod('GetRunningClass',
      'function(out clsid: TCLSID): HResult', ccStdCall);
    AddMethod('Run',
      'function(const bc: IBindCtx): HResult', ccStdCall);
    AddMethod('IsRunning',
      'function: BOOL', ccStdCall);
    AddMethod('LockRunning',
      'function(fLock: BOOL; fLastUnlockCloses: BOOL): HResult', ccStdCall);
    AddMethod('SetContainedObject',
      'function(fContained: BOOL): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------}
{ IRunningObjectTable import }
{----------------------------}

function SepiImportIRunningObjectTable(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IRunningObjectTable'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IRunningObjectTable));

  with Result do
  begin
    AddMethod('Register',
      'function(grfFlags: Longint; const unkObject: IUnknown; const mkObjectName: IMoniker ; out dwRegister: Longint ) : HResult', ccStdCall);
    AddMethod('Revoke',
      'function(dwRegister: Longint): HResult', ccStdCall);
    AddMethod('IsRunning',
      'function(const mkObjectName: IMoniker): HResult', ccStdCall);
    AddMethod('GetObject',
      'function(const mkObjectName: IMoniker; out unkObject: IUnknown ) : HResult', ccStdCall);
    AddMethod('NoteChangeTime',
      'function(dwRegister: Longint; const filetime: TFileTime ) : HResult', ccStdCall);
    AddMethod('GetTimeOfLastChange',
      'function(const mkObjectName: IMoniker; out filetime: TFileTime ) : HResult', ccStdCall);
    AddMethod('EnumRunning',
      'function(out enumMoniker: IEnumMoniker): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------}
{ IPersist import }
{-----------------}

function SepiImportIPersist(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPersist));

  with Result do
  begin
    AddMethod('GetClassID',
      'function(out classID: TCLSID): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ IPersistStream import }
{-----------------------}

function SepiImportIPersistStream(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPersistStream));

  with Result do
  begin
    AddMethod('IsDirty',
      'function: HResult', ccStdCall);
    AddMethod('Load',
      'function(const stm: IStream): HResult', ccStdCall);
    AddMethod('Save',
      'function(const stm: IStream; fClearDirty: BOOL): HResult', ccStdCall);
    AddMethod('GetSizeMax',
      'function(out cbSize: Largeint): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------}
{ IMoniker import }
{-----------------}

function SepiImportIMoniker(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IMoniker'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IMoniker));

  with Result do
  begin
    AddMethod('BindToObject',
      'function(const bc: IBindCtx; const mkToLeft: IMoniker; const iidResult: TIID ; out vResult ) : HResult', ccStdCall);
    AddMethod('BindToStorage',
      'function(const bc: IBindCtx; const mkToLeft: IMoniker; const iid: TIID ; out vObj ) : HResult', ccStdCall);
    AddMethod('Reduce',
      'function(const bc: IBindCtx; dwReduceHowFar: Longint; mkToLeft: PIMoniker ; out mkReduced: IMoniker ) : HResult', ccStdCall);
    AddMethod('ComposeWith',
      'function(const mkRight: IMoniker; fOnlyIfNotGeneric: BOOL; out mkComposite: IMoniker ) : HResult', ccStdCall);
    AddMethod('Enum',
      'function(fForward: BOOL; out enumMoniker: IEnumMoniker): HResult', ccStdCall);
    AddMethod('IsEqual',
      'function(const mkOtherMoniker: IMoniker): HResult', ccStdCall);
    AddMethod('Hash',
      'function(out dwHash: Longint): HResult', ccStdCall);
    AddMethod('IsRunning',
      'function(const bc: IBindCtx; const mkToLeft: IMoniker; const mkNewlyRunning: IMoniker ) : HResult', ccStdCall);
    AddMethod('GetTimeOfLastChange',
      'function(const bc: IBindCtx; const mkToLeft: IMoniker; out filetime: TFileTime ) : HResult', ccStdCall);
    AddMethod('Inverse',
      'function(out mk: IMoniker): HResult', ccStdCall);
    AddMethod('CommonPrefixWith',
      'function(const mkOther: IMoniker; out mkPrefix: IMoniker ) : HResult', ccStdCall);
    AddMethod('RelativePathTo',
      'function(const mkOther: IMoniker; out mkRelPath: IMoniker ) : HResult', ccStdCall);
    AddMethod('GetDisplayName',
      'function(const bc: IBindCtx; const mkToLeft: IMoniker; out pszDisplayName: POleStr ) : HResult', ccStdCall);
    AddMethod('ParseDisplayName',
      'function(const bc: IBindCtx; const mkToLeft: IMoniker; pszDisplayName: POleStr ; out chEaten: Longint ; out mkOut: IMoniker ) : HResult', ccStdCall);
    AddMethod('IsSystemMoniker',
      'function(out dwMksys: Longint): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ IEnumString import }
{--------------------}

function SepiImportIEnumString(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IEnumString'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IEnumString));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: Longint; out elt; pceltFetched: PLongint ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: Longint): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out enm: IEnumString): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ tagSTATSTG import }
{-------------------}

function SepiImporttagSTATSTG(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSTATSTG', False, True);

  with Result do
  begin
    AddField('pwcsName', 'POleStr');
    AddField('dwType', System.TypeInfo(Longint));
    AddField('cbSize', System.TypeInfo(Largeint));
    AddField('mtime', 'TFileTime');
    AddField('ctime', 'TFileTime');
    AddField('atime', 'TFileTime');
    AddField('grfMode', System.TypeInfo(Longint));
    AddField('grfLocksSupported', System.TypeInfo(Longint));
    AddField('clsid', 'TCLSID');
    AddField('grfStateBits', System.TypeInfo(Longint));
    AddField('reserved', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------------}
{ ISequentialStream import }
{--------------------------}

function SepiImportISequentialStream(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ISequentialStream));

  with Result do
  begin
    AddMethod('Read',
      'function(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult', ccStdCall);
    AddMethod('Write',
      'function(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------}
{ IStream import }
{----------------}

function SepiImportIStream(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IStream'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IStream));

  with Result do
  begin
    AddMethod('Seek',
      'function(dlibMove: Largeint; dwOrigin: Longint; out libNewPosition: Largeint ) : HResult', ccStdCall);
    AddMethod('SetSize',
      'function(libNewSize: Largeint): HResult', ccStdCall);
    AddMethod('CopyTo',
      'function(stm: IStream; cb: Largeint; out cbRead: Largeint; out cbWritten: Largeint ) : HResult', ccStdCall);
    AddMethod('Commit',
      'function(grfCommitFlags: Longint): HResult', ccStdCall);
    AddMethod('Revert',
      'function: HResult', ccStdCall);
    AddMethod('LockRegion',
      'function(libOffset: Largeint; cb: Largeint; dwLockType: Longint ) : HResult', ccStdCall);
    AddMethod('UnlockRegion',
      'function(libOffset: Largeint; cb: Largeint; dwLockType: Longint ) : HResult', ccStdCall);
    AddMethod('Stat',
      'function(out statstg: TStatStg; grfStatFlag: Longint): HResult', ccStdCall);
    AddMethod('Clone',
      'function(out stm: IStream): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IEnumStatStg import }
{---------------------}

function SepiImportIEnumStatStg(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IEnumStatStg));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: Longint; out elt; pceltFetched: PLongint ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: Longint): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out enm: IEnumStatStg): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------}
{ IStorage import }
{-----------------}

function SepiImportIStorage(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IStorage));

  with Result do
  begin
    AddMethod('CreateStream',
      'function(pwcsName: POleStr; grfMode: Longint; reserved1: Longint; reserved2: Longint ; out stm: IStream ) : HResult', ccStdCall);
    AddMethod('OpenStream',
      'function(pwcsName: POleStr; reserved1: Pointer; grfMode: Longint; reserved2: Longint ; out stm: IStream ) : HResult', ccStdCall);
    AddMethod('CreateStorage',
      'function(pwcsName: POleStr; grfMode: Longint; dwStgFmt: Longint ; reserved2: Longint ; out stg: IStorage ) : HResult', ccStdCall);
    AddMethod('OpenStorage',
      'function(pwcsName: POleStr; const stgPriority: IStorage; grfMode: Longint ; snbExclude: TSNB ; reserved: Longint ; out stg: IStorage ) : HResult', ccStdCall);
    AddMethod('CopyTo',
      'function(ciidExclude: Longint; rgiidExclude: PIID; snbExclude: TSNB ; const stgDest: IStorage ) : HResult', ccStdCall);
    AddMethod('MoveElementTo',
      'function(pwcsName: POleStr; const stgDest: IStorage; pwcsNewName: POleStr ; grfFlags: Longint ) : HResult', ccStdCall);
    AddMethod('Commit',
      'function(grfCommitFlags: Longint): HResult', ccStdCall);
    AddMethod('Revert',
      'function: HResult', ccStdCall);
    AddMethod('EnumElements',
      'function(reserved1: Longint; reserved2: Pointer; reserved3: Longint; out enm: IEnumStatStg ) : HResult', ccStdCall);
    AddMethod('DestroyElement',
      'function(pwcsName: POleStr): HResult', ccStdCall);
    AddMethod('RenameElement',
      'function(pwcsOldName: POleStr; pwcsNewName: POleStr ) : HResult', ccStdCall);
    AddMethod('SetElementTimes',
      'function(pwcsName: POleStr; const ctime: TFileTime; const atime: TFileTime ; const mtime: TFileTime ) : HResult', ccStdCall);
    AddMethod('SetClass',
      'function(const clsid: TCLSID): HResult', ccStdCall);
    AddMethod('SetStateBits',
      'function(grfStateBits: Longint; grfMask: Longint): HResult', ccStdCall);
    AddMethod('Stat',
      'function(out statstg: TStatStg; grfStatFlag: Longint): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IPersistFile import }
{---------------------}

function SepiImportIPersistFile(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPersistFile));

  with Result do
  begin
    AddMethod('IsDirty',
      'function: HResult', ccStdCall);
    AddMethod('Load',
      'function(pszFileName: POleStr; dwMode: Longint): HResult', ccStdCall);
    AddMethod('Save',
      'function(pszFileName: POleStr; fRemember: BOOL): HResult', ccStdCall);
    AddMethod('SaveCompleted',
      'function(pszFileName: POleStr): HResult', ccStdCall);
    AddMethod('GetCurFile',
      'function(out pszFileName: POleStr): HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IPersistStorage import }
{------------------------}

function SepiImportIPersistStorage(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPersistStorage));

  with Result do
  begin
    AddMethod('IsDirty',
      'function: HResult', ccStdCall);
    AddMethod('InitNew',
      'function(const stg: IStorage): HResult', ccStdCall);
    AddMethod('Load',
      'function(const stg: IStorage): HResult', ccStdCall);
    AddMethod('Save',
      'function(const stgSave: IStorage; fSameAsLoad: BOOL): HResult', ccStdCall);
    AddMethod('SaveCompleted',
      'function(const stgNew: IStorage): HResult', ccStdCall);
    AddMethod('HandsOffStorage',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ ILockBytes import }
{-------------------}

function SepiImportILockBytes(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ILockBytes));

  with Result do
  begin
    AddMethod('ReadAt',
      'function(ulOffset: Largeint; pv: Pointer; cb: Longint; pcbRead: PLongint ) : HResult', ccStdCall);
    AddMethod('WriteAt',
      'function(ulOffset: Largeint; pv: Pointer; cb: Longint; pcbWritten: PLongint ) : HResult', ccStdCall);
    AddMethod('Flush',
      'function: HResult', ccStdCall);
    AddMethod('SetSize',
      'function(cb: Largeint): HResult', ccStdCall);
    AddMethod('LockRegion',
      'function(libOffset: Largeint; cb: Largeint; dwLockType: Longint ) : HResult', ccStdCall);
    AddMethod('UnlockRegion',
      'function(libOffset: Largeint; cb: Largeint; dwLockType: Longint ) : HResult', ccStdCall);
    AddMethod('Stat',
      'function(out statstg: TStatStg; grfStatFlag: Longint): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------}
{ $2 import }
{-----------}

function SepiImport_2(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '$2', False, True);

  with Result do
  begin

    Complete;
  end;
end;

{--------------------------}
{ tagDVTARGETDEVICE import }
{--------------------------}

function SepiImporttagDVTARGETDEVICE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagDVTARGETDEVICE', False, True);

  with Result do
  begin
    AddField('tdSize', System.TypeInfo(Longint));
    AddField('tdDriverNameOffset', System.TypeInfo(Word));
    AddField('tdDeviceNameOffset', System.TypeInfo(Word));
    AddField('tdPortNameOffset', System.TypeInfo(Word));
    AddField('tdExtDevmodeOffset', System.TypeInfo(Word));
    AddField('tdData', '$2');

    Complete;
  end;
end;

{---------------------}
{ tagFORMATETC import }
{---------------------}

function SepiImporttagFORMATETC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagFORMATETC', False, True);

  with Result do
  begin
    AddField('cfFormat', System.TypeInfo(TClipFormat));
    AddField('ptd', 'PDVTargetDevice');
    AddField('dwAspect', System.TypeInfo(Longint));
    AddField('lindex', System.TypeInfo(Longint));
    AddField('tymed', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ IEnumFORMATETC import }
{-----------------------}

function SepiImportIEnumFORMATETC(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IEnumFORMATETC));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: Longint; out elt; pceltFetched: PLongint ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: Longint): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out Enum: IEnumFormatEtc): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ tagSTATDATA import }
{--------------------}

function SepiImporttagSTATDATA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSTATDATA', False, True,
    TypeInfo(tagSTATDATA));

  with Result do
  begin
    AddField('formatetc', 'TFormatEtc');
    AddField('advf', System.TypeInfo(Longint));
    AddField('advSink', System.TypeInfo(IAdviseSink));
    AddField('dwConnection', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ IEnumSTATDATA import }
{----------------------}

function SepiImportIEnumSTATDATA(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IEnumSTATDATA));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: Longint; out elt; pceltFetched: PLongint ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: Longint): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out Enum: IEnumStatData): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IRootStorage import }
{---------------------}

function SepiImportIRootStorage(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IRootStorage));

  with Result do
  begin
    AddMethod('SwitchToFile',
      'function(pszFile: POleStr): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------}
{ $3 import }
{-----------}

function SepiImport_3(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '$3', False, True);

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ tagRemSTGMEDIUM import }
{------------------------}

function SepiImporttagRemSTGMEDIUM(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagRemSTGMEDIUM', False, True);

  with Result do
  begin
    AddField('tymed', System.TypeInfo(Longint));
    AddField('dwHandleType', System.TypeInfo(Longint));
    AddField('pData', System.TypeInfo(Longint));
    AddField('pUnkForRelease', System.TypeInfo(Longint));
    AddField('cbData', System.TypeInfo(Longint));
    AddField('data', '$3');

    Complete;
  end;
end;

{---------------------}
{ tagSTGMEDIUM import }
{---------------------}

function SepiImporttagSTGMEDIUM(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSTGMEDIUM', False, True);

  with Result do
  begin
    AddField('tymed', System.TypeInfo(Longint));
    AddFieldAfter('hBitmap', System.TypeInfo(HBitmap), 'tymed');
    AddFieldAfter('unkForRelease', 'Pointer', 'hBitmap');
    AddFieldAfter('hMetaFilePict', System.TypeInfo(THandle), 'tymed');
    AddFieldAfter('hEnhMetaFile', System.TypeInfo(THandle), 'tymed');
    AddFieldAfter('hGlobal', System.TypeInfo(HGlobal), 'tymed');
    AddFieldAfter('lpszFileName', 'POleStr', 'tymed');
    AddFieldAfter('stm', 'Pointer', 'tymed');
    AddFieldAfter('stg', 'Pointer', 'tymed');

    Complete;
  end;
end;

{--------------------}
{ IAdviseSink import }
{--------------------}

function SepiImportIAdviseSink(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IAdviseSink'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IAdviseSink));

  with Result do
  begin
    AddMethod('OnDataChange',
      'procedure(const formatetc: TFormatEtc; const stgmed: TStgMedium)', ccStdCall);
    AddMethod('OnViewChange',
      'procedure(dwAspect: Longint; lindex: Longint)', ccStdCall);
    AddMethod('OnRename',
      'procedure(const mk: IMoniker)', ccStdCall);
    AddMethod('OnSave',
      'procedure', ccStdCall);
    AddMethod('OnClose',
      'procedure', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IAdviseSink2 import }
{---------------------}

function SepiImportIAdviseSink2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IAdviseSink2));

  with Result do
  begin
    AddMethod('OnLinkSrcChange',
      'procedure(const mk: IMoniker)', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ IDataObject import }
{--------------------}

function SepiImportIDataObject(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDataObject));

  with Result do
  begin
    AddMethod('GetData',
      'function(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult', ccStdCall);
    AddMethod('GetDataHere',
      'function(const formatetc: TFormatEtc; out medium: TStgMedium): HResult', ccStdCall);
    AddMethod('QueryGetData',
      'function(const formatetc: TFormatEtc): HResult', ccStdCall);
    AddMethod('GetCanonicalFormatEtc',
      'function(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc ) : HResult', ccStdCall);
    AddMethod('SetData',
      'function(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL ) : HResult', ccStdCall);
    AddMethod('EnumFormatEtc',
      'function(dwDirection: Longint; out enumFormatEtc:  IEnumFormatEtc  ) : HResult', ccStdCall);
    AddMethod('DAdvise',
      'function(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink ; out dwConnection: Longint ) : HResult', ccStdCall);
    AddMethod('DUnadvise',
      'function(dwConnection: Longint): HResult', ccStdCall);
    AddMethod('EnumDAdvise',
      'function(out enumAdvise: IEnumStatData): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IDataAdviseHolder import }
{--------------------------}

function SepiImportIDataAdviseHolder(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDataAdviseHolder));

  with Result do
  begin
    AddMethod('Advise',
      'function(const dataObject: IDataObject; const fetc: TFormatEtc; advf: Longint ; const advise: IAdviseSink ; out pdwConnection: Longint ) : HResult', ccStdCall);
    AddMethod('Unadvise',
      'function(dwConnection: Longint): HResult', ccStdCall);
    AddMethod('EnumAdvise',
      'function(out enumAdvise: IEnumStatData): HResult', ccStdCall);
    AddMethod('SendOnDataChange',
      'function(const dataObject: IDataObject; dwReserved: Longint; advf: Longint ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ tagINTERFACEINFO import }
{-------------------------}

function SepiImporttagINTERFACEINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagINTERFACEINFO', False, True,
    TypeInfo(tagINTERFACEINFO));

  with Result do
  begin
    AddField('unk', System.TypeInfo(IUnknown));
    AddField('iid', 'TIID');
    AddField('wMethod', System.TypeInfo(Word));

    Complete;
  end;
end;

{-----------------------}
{ IMessageFilter import }
{-----------------------}

function SepiImportIMessageFilter(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IMessageFilter));

  with Result do
  begin
    AddMethod('HandleInComingCall',
      'function(dwCallType: Longint; htaskCaller: HTask; dwTickCount: Longint ; lpInterfaceInfo: PInterfaceInfo ) : Longint', ccStdCall);
    AddMethod('RetryRejectedCall',
      'function(htaskCallee: HTask; dwTickCount: Longint; dwRejectType: Longint ) : Longint', ccStdCall);
    AddMethod('MessagePending',
      'function(htaskCallee: HTask; dwTickCount: Longint; dwPendingType: Longint ) : Longint', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ tagRPCOLEMESSAGE import }
{-------------------------}

function SepiImporttagRPCOLEMESSAGE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagRPCOLEMESSAGE', False, True);

  with Result do
  begin
    AddField('reserved1', 'Pointer');
    AddField('dataRepresentation', System.TypeInfo(TRpcOleDataRep));
    AddField('Buffer', 'Pointer');
    AddField('cbBuffer', System.TypeInfo(Longint));
    AddField('iMethod', System.TypeInfo(Longint));
    AddField('reserved2', '$4');
    AddField('rpcFlags', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------------}
{ IRpcChannelBuffer import }
{--------------------------}

function SepiImportIRpcChannelBuffer(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IRpcChannelBuffer));

  with Result do
  begin
    AddMethod('GetBuffer',
      'function(var message: TRpcOleMessage; iid: TIID): HResult', ccStdCall);
    AddMethod('SendReceive',
      'function(var message: TRpcOleMessage; var status: Longint ) : HResult', ccStdCall);
    AddMethod('FreeBuffer',
      'function(var message: TRpcOleMessage): HResult', ccStdCall);
    AddMethod('GetDestCtx',
      'function(out dwDestContext: Longint; out pvDestContext ) : HResult', ccStdCall);
    AddMethod('IsConnected',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IRpcProxyBuffer import }
{------------------------}

function SepiImportIRpcProxyBuffer(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IRpcProxyBuffer));

  with Result do
  begin
    AddMethod('Connect',
      'function(const rpcChannelBuffer: IRpcChannelBuffer): HResult', ccStdCall);
    AddMethod('Disconnect',
      'procedure', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ IRpcStubBuffer import }
{-----------------------}

function SepiImportIRpcStubBuffer(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IRpcStubBuffer));

  with Result do
  begin
    AddMethod('Connect',
      'function(const unkServer: IUnknown): HResult', ccStdCall);
    AddMethod('Disconnect',
      'procedure', ccStdCall);
    AddMethod('Invoke',
      'function(var rpcmsg: TRpcOleMessage; rpcChannelBuffer:  IRpcChannelBuffer  ) : HResult', ccStdCall);
    AddMethod('IsIIDSupported',
      'function(const iid: TIID): Pointer', ccStdCall);
    AddMethod('CountRefs',
      'function: Longint', ccStdCall);
    AddMethod('DebugServerQueryInterface',
      'function(var pv): HResult', ccStdCall);
    AddMethod('DebugServerRelease',
      'procedure(pv: Pointer)', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ IPSFactoryBuffer import }
{-------------------------}

function SepiImportIPSFactoryBuffer(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPSFactoryBuffer));

  with Result do
  begin
    AddMethod('CreateProxy',
      'function(const unkOuter: IUnknown; const iid: TIID; out proxy: IRpcProxyBuffer ; out pv ) : HResult', ccStdCall);
    AddMethod('CreateStub',
      'function(const iid: TIID; const unkServer: IUnknown; out stub: IRpcStubBuffer ) : HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IChannelHook import }
{---------------------}

function SepiImportIChannelHook(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IChannelHook));

  with Result do
  begin
    AddMethod('ClientGetSize',
      'procedure(const uExtent: TGUID; const iid: TIID; out DataSize: Longint )', ccStdCall);
    AddMethod('ClientFillBuffer',
      'procedure(const uExtent: TGUID; const iid: TIID; var DataSize: Longint ; var DataBuffer )', ccStdCall);
    AddMethod('ClientNotify',
      'procedure(const uExtent: TGUID; const iid: TIID; DataSize: Longint ; var DataBuffer ; lDataRep: Longint ; hrFault: HResult )', ccStdCall);
    AddMethod('ServerNotify',
      'procedure(const uExtent: TGUID; const iid: TIID; DataSize: Longint ; var DataBuffer ; lDataRep: Longint )', ccStdCall);
    AddMethod('ServerGetSize',
      'procedure(const uExtent: TGUID; const iid: TIID; hrFault: HResult ; out DataSize: Longint )', ccStdCall);
    AddMethod('ServerFillBuffer',
      'procedure(const uExtent: TGUID; const iid: TIID; var DataSize: Longint ; var DataBuffer ; hrFault: HResult )', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ IFillLockBytes import }
{-----------------------}

function SepiImportIFillLockBytes(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IFillLockBytes));

  with Result do
  begin
    AddMethod('FillAppend',
      'function(const pv; cb: Longint; out cbWritten: Longint ) : HResult', ccStdCall);
    AddMethod('FillAt',
      'function(Offset: Longint; const pv; cb: Longint; out cbWritten: Longint ) : HResult', ccStdCall);
    AddMethod('SetFillSize',
      'function(Offset: Longint): HResult', ccStdCall);
    AddMethod('Terminate',
      'function(bCanceled: Boolean): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------}
{ tagDEC import }
{---------------}

function SepiImporttagDEC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagDEC', True, True);

  with Result do
  begin
    AddField('wReserved', System.TypeInfo(Word));
    AddFieldAfter('scale', System.TypeInfo(Byte), 'wReserved');
    AddField('sign', System.TypeInfo(Byte), True);
    AddFieldAfter('Hi32', System.TypeInfo(Longint), 'sign');
    AddFieldAfter('Lo32', System.TypeInfo(Longint), 'Hi32');
    AddField('Mid32', System.TypeInfo(Longint), True);
    AddFieldAfter('Lo64', System.TypeInfo(LONGLONG), 'Hi32');
    AddFieldAfter('signscale', System.TypeInfo(Word), 'wReserved');

    Complete;
  end;
end;

{----------------}
{ tagBLOB import }
{----------------}

function SepiImporttagBLOB(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagBLOB', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(Longint));
    AddField('pBlobData', 'Pointer');

    Complete;
  end;
end;

{--------------------}
{ tagCLIPDATA import }
{--------------------}

function SepiImporttagCLIPDATA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCLIPDATA', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(Longint));
    AddField('ulClipFmt', System.TypeInfo(Longint));
    AddField('pClipData', 'Pointer');

    Complete;
  end;
end;

{----------------}
{ tagCAUB import }
{----------------}

function SepiImporttagCAUB(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAUB', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PByte');

    Complete;
  end;
end;

{---------------}
{ tagCAI import }
{---------------}

function SepiImporttagCAI(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAI', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PShortInt');

    Complete;
  end;
end;

{----------------}
{ tagCAUI import }
{----------------}

function SepiImporttagCAUI(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAUI', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PWord');

    Complete;
  end;
end;

{---------------}
{ tagCAL import }
{---------------}

function SepiImporttagCAL(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAL', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PLongint');

    Complete;
  end;
end;

{----------------}
{ tagCAUL import }
{----------------}

function SepiImporttagCAUL(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAUL', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PULONG');

    Complete;
  end;
end;

{-----------------}
{ tagCAFLT import }
{-----------------}

function SepiImporttagCAFLT(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAFLT', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PSingle');

    Complete;
  end;
end;

{-----------------}
{ tagCADBL import }
{-----------------}

function SepiImporttagCADBL(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCADBL', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PDouble');

    Complete;
  end;
end;

{----------------}
{ tagCACY import }
{----------------}

function SepiImporttagCACY(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCACY', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PCurrency');

    Complete;
  end;
end;

{------------------}
{ tagCADATE import }
{------------------}

function SepiImporttagCADATE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCADATE', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'POleDate');

    Complete;
  end;
end;

{------------------}
{ tagCABSTR import }
{------------------}

function SepiImporttagCABSTR(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCABSTR', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PBSTR');

    Complete;
  end;
end;

{------------------}
{ tagCABOOL import }
{------------------}

function SepiImporttagCABOOL(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCABOOL', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'POleBool');

    Complete;
  end;
end;

{-------------------}
{ tagCASCODE import }
{-------------------}

function SepiImporttagCASCODE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCASCODE', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PSCODE');

    Complete;
  end;
end;

{-------------------------}
{ tagCAPROPVARIANT import }
{-------------------------}

function SepiImporttagCAPROPVARIANT(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAPROPVARIANT', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PPropVariant');

    Complete;
  end;
end;

{---------------}
{ tagCAH import }
{---------------}

function SepiImporttagCAH(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAH', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PLargeInteger');

    Complete;
  end;
end;

{----------------}
{ tagCAUH import }
{----------------}

function SepiImporttagCAUH(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAUH', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PULargeInteger');

    Complete;
  end;
end;

{-------------------}
{ tagCALPSTR import }
{-------------------}

function SepiImporttagCALPSTR(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCALPSTR', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PLPSTR');

    Complete;
  end;
end;

{--------------------}
{ tagCALPWSTR import }
{--------------------}

function SepiImporttagCALPWSTR(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCALPWSTR', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PLPWSTR');

    Complete;
  end;
end;

{----------------------}
{ tagCAFILETIME import }
{----------------------}

function SepiImporttagCAFILETIME(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAFILETIME', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PFileTime');

    Complete;
  end;
end;

{----------------------}
{ tagCACLIPDATA import }
{----------------------}

function SepiImporttagCACLIPDATA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCACLIPDATA', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PClipData');

    Complete;
  end;
end;

{-------------------}
{ tagCACLSID import }
{-------------------}

function SepiImporttagCACLSID(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCACLSID', True, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(ULONG));
    AddField('pElems', 'PCLSID');

    Complete;
  end;
end;

{-----------------------}
{ tagPROPVARIANT import }
{-----------------------}

function SepiImporttagPROPVARIANT(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPROPVARIANT', True, True);

  with Result do
  begin
    AddField('vt', System.TypeInfo(TVarType));
    AddField('wReserved1', System.TypeInfo(Word));
    AddField('wReserved2', System.TypeInfo(Word));
    AddField('wReserved3', System.TypeInfo(Word));
    AddFieldAfter('bVal', System.TypeInfo(Byte), 'wReserved3');
    AddFieldAfter('iVal', System.TypeInfo(SmallInt), 'wReserved3');
    AddFieldAfter('uiVal', System.TypeInfo(Word), 'wReserved3');
    AddFieldAfter('boolVal', System.TypeInfo(TOleBool), 'wReserved3');
    AddFieldAfter('bool', System.TypeInfo(TOleBool), 'wReserved3');
    AddFieldAfter('lVal', System.TypeInfo(Longint), 'wReserved3');
    AddFieldAfter('ulVal', System.TypeInfo(Cardinal), 'wReserved3');
    AddFieldAfter('fltVal', System.TypeInfo(Single), 'wReserved3');
    AddFieldAfter('scode', System.TypeInfo(SCODE), 'wReserved3');
    AddFieldAfter('hVal', 'LARGE_INTEGER', 'wReserved3');
    AddFieldAfter('uhVal', 'ULARGE_INTEGER', 'wReserved3');
    AddFieldAfter('dblVal', System.TypeInfo(Double), 'wReserved3');
    AddFieldAfter('cyVal', System.TypeInfo(Currency), 'wReserved3');
    AddFieldAfter('date', System.TypeInfo(TOleDate), 'wReserved3');
    AddFieldAfter('filetime', 'TFileTime', 'wReserved3');
    AddFieldAfter('puuid', 'PGUID', 'wReserved3');
    AddFieldAfter('blob', 'TBlob', 'wReserved3');
    AddFieldAfter('pclipdata', 'PClipData', 'wReserved3');
    AddFieldAfter('pStream', 'Pointer', 'wReserved3');
    AddFieldAfter('pStorage', 'Pointer', 'wReserved3');
    AddFieldAfter('bstrVal', 'TBStr', 'wReserved3');
    AddFieldAfter('pszVal', 'PAnsiChar', 'wReserved3');
    AddFieldAfter('pwszVal', 'PWideChar', 'wReserved3');
    AddFieldAfter('caub', 'TCAUB', 'wReserved3');
    AddFieldAfter('cai', 'TCAI', 'wReserved3');
    AddFieldAfter('caui', 'TCAUI', 'wReserved3');
    AddFieldAfter('cabool', 'TCABOOL', 'wReserved3');
    AddFieldAfter('cal', 'TCAL', 'wReserved3');
    AddFieldAfter('caul', 'TCAUL', 'wReserved3');
    AddFieldAfter('caflt', 'TCAFLT', 'wReserved3');
    AddFieldAfter('cascode', 'TCASCODE', 'wReserved3');
    AddFieldAfter('cah', 'TCAH', 'wReserved3');
    AddFieldAfter('cauh', 'TCAUH', 'wReserved3');
    AddFieldAfter('cadbl', 'TCADBL', 'wReserved3');
    AddFieldAfter('cacy', 'TCACY', 'wReserved3');
    AddFieldAfter('cadate', 'TCADATE', 'wReserved3');
    AddFieldAfter('cafiletime', 'TCAFILETIME', 'wReserved3');
    AddFieldAfter('cauuid', 'TCACLSID', 'wReserved3');
    AddFieldAfter('caclipdata', 'TCACLIPDATA', 'wReserved3');
    AddFieldAfter('cabstr', 'TCABSTR', 'wReserved3');
    AddFieldAfter('calpstr', 'TCALPSTR', 'wReserved3');
    AddFieldAfter('calpwstr', 'TCALPWSTR', 'wReserved3');
    AddFieldAfter('capropvar', 'TCAPROPVARIANT', 'wReserved3');

    Complete;
  end;
end;

{--------------------}
{ tagPROPSPEC import }
{--------------------}

function SepiImporttagPROPSPEC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPROPSPEC', True, True);

  with Result do
  begin
    AddField('ulKind', System.TypeInfo(ULONG));
    AddFieldAfter('propid', System.TypeInfo(TPropID), 'ulKind');
    AddFieldAfter('lpwstr', 'POleStr', 'ulKind');

    Complete;
  end;
end;

{-----------------------}
{ tagSTATPROPSTG import }
{-----------------------}

function SepiImporttagSTATPROPSTG(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSTATPROPSTG', False, True);

  with Result do
  begin
    AddField('lpwstrName', 'POleStr');
    AddField('propid', System.TypeInfo(TPropID));
    AddField('vt', System.TypeInfo(TVarType));

    Complete;
  end;
end;

{--------------------------}
{ tagSTATPROPSETSTG import }
{--------------------------}

function SepiImporttagSTATPROPSETSTG(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSTATPROPSETSTG', True, True);

  with Result do
  begin
    AddField('fmtid', 'TFmtID');
    AddField('clsid', 'TClsID');
    AddField('grfFlags', System.TypeInfo(DWORD));
    AddField('mtime', 'TFileTime');
    AddField('ctime', 'TFileTime');
    AddField('atime', 'TFileTime');
    AddField('dwOSVersion', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------------------}
{ IPropertyStorage import }
{-------------------------}

function SepiImportIPropertyStorage(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IPropertyStorage'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IPropertyStorage));

  with Result do
  begin
    AddMethod('ReadMultiple',
      'function(cpspec: ULONG; rgpspec : PPropSpec; rgpropvar: PPropVariant): HResult', ccStdCall);
    AddMethod('WriteMultiple',
      'function(cpspec: ULONG; rgpspec : PPropSpec; rgpropvar: PPropVariant; propidNameFirst: TPropID ) : HResult', ccStdCall);
    AddMethod('DeleteMultiple',
      'function(cpspec: ULONG; rgpspec: PPropSpec): HResult', ccStdCall);
    AddMethod('ReadPropertyNames',
      'function(cpropid: ULONG; rgpropid: PPropID; rglpwstrName: PPOleStr ) : HResult', ccStdCall);
    AddMethod('WritePropertyNames',
      'function(cpropid: ULONG; rgpropid: PPropID; rglpwstrName: PPOleStr ) : HResult', ccStdCall);
    AddMethod('DeletePropertyNames',
      'function(cpropid: ULONG; rgpropid: PPropID): HResult', ccStdCall);
    AddMethod('Commit',
      'function(grfCommitFlags: DWORD): HResult', ccStdCall);
    AddMethod('Revert',
      'function: HResult', ccStdCall);
    AddMethod('Enum',
      'function(out ppenum: IEnumSTATPROPSTG): HResult', ccStdCall);
    AddMethod('SetTimes',
      'function(const pctime, patime, pmtime: TFileTime): HResult', ccStdCall);
    AddMethod('SetClass',
      'function(const clsid: TCLSID): HResult', ccStdCall);
    AddMethod('Stat',
      'function(pstatpsstg: PStatPropSetStg): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------}
{ IPropertySetStorage import }
{----------------------------}

function SepiImportIPropertySetStorage(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IPropertySetStorage'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IPropertySetStorage));

  with Result do
  begin
    AddMethod('Create',
      'function(const rfmtid: TFmtID; const pclsid: TCLSID; grfFlags, grfMode : DWORD ; out ppprstg: IPropertyStorage ) : HResult', ccStdCall);
    AddMethod('Open',
      'function(const rfmtid: TFmtID; grfMode: DWORD; out ppprstg: IPropertyStorage ) : HResult', ccStdCall);
    AddMethod('Delete',
      'function(const rfmtid: TFmtID): HResult', ccStdCall);
    AddMethod('Enum',
      'function(out ppenum: IEnumSTATPROPSETSTG): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ IEnumSTATPROPSTG import }
{-------------------------}

function SepiImportIEnumSTATPROPSTG(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IEnumSTATPROPSTG'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IEnumSTATPROPSTG));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: ULONG; out rgelt; pceltFetched: PULONG): HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: ULONG): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out ppenum: IEnumSTATPROPSTG): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------}
{ IEnumSTATPROPSETSTG import }
{----------------------------}

function SepiImportIEnumSTATPROPSETSTG(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IEnumSTATPROPSETSTG'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IEnumSTATPROPSETSTG));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: ULONG; out rgelt; pceltFetched: PULONG): HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: ULONG): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out ppenum: IEnumSTATPROPSETSTG): HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------------}
{ IGlobalInterfaceTable import }
{------------------------------}

function SepiImportIGlobalInterfaceTable(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IGlobalInterfaceTable'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IGlobalInterfaceTable));

  with Result do
  begin
    AddMethod('RegisterInterfaceInGlobal',
      'function(const pUnk: IUnknown; const riid: TIID; out dwCookie: DWORD ) : HResult', ccStdCall);
    AddMethod('RevokeInterfaceFromGlobal',
      'function(dwCookie: DWORD): HResult', ccStdCall);
    AddMethod('GetInterfaceFromGlobal',
      'function(dwCookie: DWORD; const riid: TIID; out ppv ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ tagVARIANT import }
{-------------------}

function SepiImporttagVARIANT(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagVARIANT', False, True);

  with Result do
  begin
    AddField('vt', System.TypeInfo(TVarType));
    AddField('wReserved1', System.TypeInfo(Word));
    AddField('wReserved2', System.TypeInfo(Word));
    AddField('wReserved3', System.TypeInfo(Word));
    AddFieldAfter('bVal', System.TypeInfo(Byte), 'wReserved3');
    AddFieldAfter('iVal', System.TypeInfo(Smallint), 'wReserved3');
    AddFieldAfter('lVal', System.TypeInfo(Longint), 'wReserved3');
    AddFieldAfter('fltVal', System.TypeInfo(Single), 'wReserved3');
    AddFieldAfter('dblVal', System.TypeInfo(Double), 'wReserved3');
    AddFieldAfter('vbool', System.TypeInfo(TOleBool), 'wReserved3');
    AddFieldAfter('scode', System.TypeInfo(HResult), 'wReserved3');
    AddFieldAfter('cyVal', System.TypeInfo(Currency), 'wReserved3');
    AddFieldAfter('date', System.TypeInfo(TOleDate), 'wReserved3');
    AddFieldAfter('bstrVal', 'PWideChar', 'wReserved3');
    AddFieldAfter('unkVal', 'Pointer', 'wReserved3');
    AddFieldAfter('dispVal', 'Pointer', 'wReserved3');
    AddFieldAfter('parray', 'PSafeArray', 'wReserved3');
    AddFieldAfter('pbVal', '$5', 'wReserved3');
    AddFieldAfter('piVal', '$6', 'wReserved3');
    AddFieldAfter('plVal', '$7', 'wReserved3');
    AddFieldAfter('pfltVal', '$8', 'wReserved3');
    AddFieldAfter('pdblVal', '$9', 'wReserved3');
    AddFieldAfter('pbool', '$10', 'wReserved3');
    AddFieldAfter('pscode', '$11', 'wReserved3');
    AddFieldAfter('pcyVal', '$12', 'wReserved3');
    AddFieldAfter('pdate', '$13', 'wReserved3');
    AddFieldAfter('pbstrVal', '$14', 'wReserved3');
    AddFieldAfter('punkVal', '$15', 'wReserved3');
    AddFieldAfter('pdispVal', '$16', 'wReserved3');
    AddFieldAfter('pparray', '$17', 'wReserved3');
    AddFieldAfter('pvarVal', 'PVariant', 'wReserved3');
    AddFieldAfter('byRef', 'Pointer', 'wReserved3');
    AddFieldAfter('cVal', System.TypeInfo(Char), 'wReserved3');
    AddFieldAfter('uiVal', System.TypeInfo(Word), 'wReserved3');
    AddFieldAfter('ulVal', System.TypeInfo(LongWord), 'wReserved3');
    AddFieldAfter('intVal', System.TypeInfo(Integer), 'wReserved3');
    AddFieldAfter('uintVal', System.TypeInfo(LongWord), 'wReserved3');
    AddFieldAfter('pdecVal', 'PDecimal', 'wReserved3');
    AddFieldAfter('pcVal', 'PChar', 'wReserved3');
    AddFieldAfter('puiVal', 'PWord', 'wReserved3');
    AddFieldAfter('pulVal', 'PInteger', 'wReserved3');
    AddFieldAfter('pintVal', 'PInteger', 'wReserved3');
    AddFieldAfter('puintVal', 'PLongWord', 'wReserved3');

    Complete;
  end;
end;

{--------------------}
{ tagTYPEDESC import }
{--------------------}

function SepiImporttagTYPEDESC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTYPEDESC', False, True);

  with Result do
  begin
    AddFieldAfter('ptdesc', 'PTypeDesc', '');
    AddFieldAfter('vt', System.TypeInfo(TVarType), 'ptdesc');
    AddFieldAfter('padesc', 'PArrayDesc', '');
    AddFieldAfter('hreftype', System.TypeInfo(HRefType), '');

    Complete;
  end;
end;

{---------------------}
{ tagARRAYDESC import }
{---------------------}

function SepiImporttagARRAYDESC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagARRAYDESC', False, True);

  with Result do
  begin
    AddField('tdescElem', 'TTypeDesc');
    AddField('cDims', System.TypeInfo(Word));
    AddField('rgbounds', '$18');

    Complete;
  end;
end;

{-------------------}
{ tagIDLDESC import }
{-------------------}

function SepiImporttagIDLDESC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagIDLDESC', False, True);

  with Result do
  begin
    AddField('dwReserved', System.TypeInfo(Longint));
    AddField('wIDLFlags', System.TypeInfo(Word));

    Complete;
  end;
end;

{-----------------------}
{ tagPARAMDESCEX import }
{-----------------------}

function SepiImporttagPARAMDESCEX(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPARAMDESCEX', False, True);

  with Result do
  begin
    AddField('cBytes', System.TypeInfo(Longint));
    AddField('FourBytePad', System.TypeInfo(Longint));
    AddField('varDefaultValue', 'TVariantArg');

    Complete;
  end;
end;

{---------------------}
{ tagPARAMDESC import }
{---------------------}

function SepiImporttagPARAMDESC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPARAMDESC', False, True);

  with Result do
  begin
    AddField('pparamdescex', 'PParamDescEx');
    AddField('wParamFlags', System.TypeInfo(Word));

    Complete;
  end;
end;

{--------------------}
{ tagELEMDESC import }
{--------------------}

function SepiImporttagELEMDESC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagELEMDESC', False, True);

  with Result do
  begin
    AddField('tdesc', 'TTypeDesc');
    AddFieldAfter('idldesc', 'TIDLDesc', 'tdesc');
    AddFieldAfter('paramdesc', 'TParamDesc', 'tdesc');

    Complete;
  end;
end;

{--------------------}
{ tagTYPEATTR import }
{--------------------}

function SepiImporttagTYPEATTR(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTYPEATTR', False, True);

  with Result do
  begin
    AddField('guid', 'TGUID');
    AddField('lcid', System.TypeInfo(TLCID));
    AddField('dwReserved', System.TypeInfo(Longint));
    AddField('memidConstructor', System.TypeInfo(TMemberID));
    AddField('memidDestructor', System.TypeInfo(TMemberID));
    AddField('lpstrSchema', 'POleStr');
    AddField('cbSizeInstance', System.TypeInfo(Longint));
    AddField('typekind', System.TypeInfo(TTypeKind));
    AddField('cFuncs', System.TypeInfo(Word));
    AddField('cVars', System.TypeInfo(Word));
    AddField('cImplTypes', System.TypeInfo(Word));
    AddField('cbSizeVft', System.TypeInfo(Word));
    AddField('cbAlignment', System.TypeInfo(Word));
    AddField('wTypeFlags', System.TypeInfo(Word));
    AddField('wMajorVerNum', System.TypeInfo(Word));
    AddField('wMinorVerNum', System.TypeInfo(Word));
    AddField('tdescAlias', 'TTypeDesc');
    AddField('idldescType', 'TIDLDesc');

    Complete;
  end;
end;

{----------------------}
{ tagDISPPARAMS import }
{----------------------}

function SepiImporttagDISPPARAMS(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagDISPPARAMS', False, True);

  with Result do
  begin
    AddField('rgvarg', 'PVariantArgList');
    AddField('rgdispidNamedArgs', 'PDispIDList');
    AddField('cArgs', System.TypeInfo(Longint));
    AddField('cNamedArgs', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ tagEXCEPINFO import }
{---------------------}

function SepiImporttagEXCEPINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagEXCEPINFO', False, True,
    TypeInfo(tagEXCEPINFO));

  with Result do
  begin
    AddField('wCode', System.TypeInfo(Word));
    AddField('wReserved', System.TypeInfo(Word));
    AddField('bstrSource', System.TypeInfo(WideString));
    AddField('bstrDescription', System.TypeInfo(WideString));
    AddField('bstrHelpFile', System.TypeInfo(WideString));
    AddField('dwHelpContext', System.TypeInfo(Longint));
    AddField('pvReserved', 'Pointer');
    AddField('pfnDeferredFillIn', 'TFNDeferredFillIn');
    AddField('scode', System.TypeInfo(HResult));

    Complete;
  end;
end;

{--------------------}
{ tagFUNCDESC import }
{--------------------}

function SepiImporttagFUNCDESC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagFUNCDESC', False, True);

  with Result do
  begin
    AddField('memid', System.TypeInfo(TMemberID));
    AddField('lprgscode', 'PResultList');
    AddField('lprgelemdescParam', 'PElemDescList');
    AddField('funckind', System.TypeInfo(TFuncKind));
    AddField('invkind', System.TypeInfo(TInvokeKind));
    AddField('callconv', System.TypeInfo(TCallConv));
    AddField('cParams', System.TypeInfo(Smallint));
    AddField('cParamsOpt', System.TypeInfo(Smallint));
    AddField('oVft', System.TypeInfo(Smallint));
    AddField('cScodes', System.TypeInfo(Smallint));
    AddField('elemdescFunc', 'TElemDesc');
    AddField('wFuncFlags', System.TypeInfo(Word));

    Complete;
  end;
end;

{-------------------}
{ tagVARDESC import }
{-------------------}

function SepiImporttagVARDESC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagVARDESC', False, True);

  with Result do
  begin
    AddField('memid', System.TypeInfo(TMemberID));
    AddField('lpstrSchema', 'POleStr');
    AddFieldAfter('oInst', System.TypeInfo(Longint), 'lpstrSchema');
    AddFieldAfter('elemdescVar', 'TElemDesc', 'oInst');
    AddFieldAfter('wVarFlags', System.TypeInfo(Word), 'elemdescVar');
    AddFieldAfter('varkind', System.TypeInfo(TVarKind), 'wVarFlags');
    AddFieldAfter('lpvarValue', 'POleVariant', 'lpstrSchema');

    Complete;
  end;
end;

{------------------------}
{ ICreateTypeInfo import }
{------------------------}

function SepiImportICreateTypeInfo(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ICreateTypeInfo));

  with Result do
  begin
    AddMethod('SetGuid',
      'function(const guid: TGUID): HResult', ccStdCall);
    AddMethod('SetTypeFlags',
      'function(uTypeFlags: Integer): HResult', ccStdCall);
    AddMethod('SetDocString',
      'function(pstrDoc: POleStr): HResult', ccStdCall);
    AddMethod('SetHelpContext',
      'function(dwHelpContext: Longint): HResult', ccStdCall);
    AddMethod('SetVersion',
      'function(wMajorVerNum: Word; wMinorVerNum: Word): HResult', ccStdCall);
    AddMethod('AddRefTypeInfo',
      'function(const tinfo: ITypeInfo; out reftype: HRefType): HResult', ccStdCall);
    AddMethod('AddFuncDesc',
      'function(index: Integer; const funcdesc: TFuncDesc): HResult', ccStdCall);
    AddMethod('AddImplType',
      'function(index: Integer; reftype: HRefType): HResult', ccStdCall);
    AddMethod('SetImplTypeFlags',
      'function(index: Integer; impltypeflags: Integer): HResult', ccStdCall);
    AddMethod('SetAlignment',
      'function(cbAlignment: Word): HResult', ccStdCall);
    AddMethod('SetSchema',
      'function(lpstrSchema: POleStr): HResult', ccStdCall);
    AddMethod('AddVarDesc',
      'function(index: Integer; const vardesc: TVarDesc): HResult', ccStdCall);
    AddMethod('SetFuncAndParamNames',
      'function(index: Integer; rgszNames: POleStrList; cNames: Integer ) : HResult', ccStdCall);
    AddMethod('SetVarName',
      'function(index: Integer; szName: POleStr): HResult', ccStdCall);
    AddMethod('SetTypeDescAlias',
      'function(const descAlias: TTypeDesc): HResult', ccStdCall);
    AddMethod('DefineFuncAsDllEntry',
      'function(index: Integer; szDllName: POleStr; szProcName: POleStr ) : HResult', ccStdCall);
    AddMethod('SetFuncDocString',
      'function(index: Integer; szDocString: POleStr): HResult', ccStdCall);
    AddMethod('SetVarDocString',
      'function(index: Integer; szDocString: POleStr): HResult', ccStdCall);
    AddMethod('SetFuncHelpContext',
      'function(index: Integer; dwHelpContext: Longint): HResult', ccStdCall);
    AddMethod('SetVarHelpContext',
      'function(index: Integer; dwHelpContext: Longint): HResult', ccStdCall);
    AddMethod('SetMops',
      'function(index: Integer; const bstrMops: WideString): HResult', ccStdCall);
    AddMethod('SetTypeIdldesc',
      'function(const idldesc: TIDLDesc): HResult', ccStdCall);
    AddMethod('LayOut',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ ICreateTypeInfo2 import }
{-------------------------}

function SepiImportICreateTypeInfo2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ICreateTypeInfo2));

  with Result do
  begin
    AddMethod('DeleteFuncDesc',
      'function(index: Integer): HResult', ccStdCall);
    AddMethod('DeleteFuncDescByMemId',
      'function(memid: TMemberID; invKind: TInvokeKind): HResult', ccStdCall);
    AddMethod('DeleteVarDesc',
      'function(index: Integer): HResult', ccStdCall);
    AddMethod('DeleteVarDescByMemId',
      'function(memid: TMemberID): HResult', ccStdCall);
    AddMethod('DeleteImplType',
      'function(index: Integer): HResult', ccStdCall);
    AddMethod('SetCustData',
      'function(const guid: TGUID; pVarVal: POleVariant): HResult', ccStdCall);
    AddMethod('SetFuncCustData',
      'function(index: Integer; const guid: TGUID; pVarVal: POleVariant ) : HResult', ccStdCall);
    AddMethod('SetParamCustData',
      'function(indexFunc: Integer; indexParam: Integer; const guid: TGUID ; pVarVal: POleVariant ) : HResult', ccStdCall);
    AddMethod('SetVarCustData',
      'function(index: Integer; const guid: TGUID; pVarVal: POleVariant ) : HResult', ccStdCall);
    AddMethod('SetImplTypeCustData',
      'function(index: Integer; const guid: TGUID; pVarVal: POleVariant ) : HResult', ccStdCall);
    AddMethod('SetHelpStringContext',
      'function(dwHelpStringContext: Longint): HResult', ccStdCall);
    AddMethod('SetFuncHelpStringContext',
      'function(index: Integer; dwHelpStringContext: Longint ) : HResult', ccStdCall);
    AddMethod('SetVarHelpStringContext',
      'function(index: Integer; dwHelpStringContext: Longint ) : HResult', ccStdCall);
    AddMethod('Invalidate',
      'function: HResult', ccStdCall);
    AddMethod('SetName',
      'function(szName: POleStr): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ ICreateTypeLib import }
{-----------------------}

function SepiImportICreateTypeLib(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ICreateTypeLib));

  with Result do
  begin
    AddMethod('CreateTypeInfo',
      'function(szName: POleStr; tkind: TTypeKind; out ictinfo: ICreateTypeInfo ) : HResult', ccStdCall);
    AddMethod('SetName',
      'function(szName: POleStr): HResult', ccStdCall);
    AddMethod('SetVersion',
      'function(wMajorVerNum: Word; wMinorVerNum: Word): HResult', ccStdCall);
    AddMethod('SetGuid',
      'function(const guid: TGUID): HResult', ccStdCall);
    AddMethod('SetDocString',
      'function(szDoc: POleStr): HResult', ccStdCall);
    AddMethod('SetHelpFileName',
      'function(szHelpFileName: POleStr): HResult', ccStdCall);
    AddMethod('SetHelpContext',
      'function(dwHelpContext: Longint): HResult', ccStdCall);
    AddMethod('SetLcid',
      'function(lcid: TLCID): HResult', ccStdCall);
    AddMethod('SetLibFlags',
      'function(uLibFlags: Integer): HResult', ccStdCall);
    AddMethod('SaveAllChanges',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ ICreateTypeLib2 import }
{------------------------}

function SepiImportICreateTypeLib2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ICreateTypeLib2));

  with Result do
  begin
    AddMethod('DeleteTypeInfo',
      'function(szName: PWideChar): HResult', ccStdCall);
    AddMethod('SetCustData',
      'function(const guid: TGUID; pVarVal: POleVariant): HResult', ccStdCall);
    AddMethod('SetHelpStringContext',
      'function(dwHelpStringContext: Longint): HResult', ccStdCall);
    AddMethod('SetHelpStringDll',
      'function(szFileName: PWideChar): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IEnumVariant import }
{---------------------}

function SepiImportIEnumVariant(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IEnumVariant));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: LongWord; var rgvar : OleVariant; out pceltFetched: LongWord ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: LongWord): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out Enum: IEnumVariant): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ tagBINDPTR import }
{-------------------}

function SepiImporttagBINDPTR(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagBINDPTR', False, True);

  with Result do
  begin
    AddFieldAfter('lpfuncdesc', 'PFuncDesc', '');
    AddFieldAfter('lpvardesc', 'PVarDesc', '');
    AddFieldAfter('lptcomp', 'Pointer', '');

    Complete;
  end;
end;

{------------------}
{ ITypeComp import }
{------------------}

function SepiImportITypeComp(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('ITypeComp'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(ITypeComp));

  with Result do
  begin
    AddMethod('Bind',
      'function(szName: POleStr; lHashVal: Longint; wflags: Word; out tinfo: ITypeInfo ; out desckind: TDescKind ; out bindptr: TBindPtr ) : HResult', ccStdCall);
    AddMethod('BindType',
      'function(szName: POleStr; lHashVal: Longint; out tinfo: ITypeInfo ; out tcomp: ITypeComp ) : HResult', ccStdCall);

    Complete;
  end;
end;

{------------------}
{ ITypeInfo import }
{------------------}

function SepiImportITypeInfo(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('ITypeInfo'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(ITypeInfo));

  with Result do
  begin
    AddMethod('GetTypeAttr',
      'function(out ptypeattr: PTypeAttr): HResult', ccStdCall);
    AddMethod('GetTypeComp',
      'function(out tcomp: ITypeComp): HResult', ccStdCall);
    AddMethod('GetFuncDesc',
      'function(index: Integer; out pfuncdesc: PFuncDesc): HResult', ccStdCall);
    AddMethod('GetVarDesc',
      'function(index: Integer; out pvardesc: PVarDesc): HResult', ccStdCall);
    AddMethod('GetNames',
      'function(memid: TMemberID; rgbstrNames: PBStrList; cMaxNames: Integer ; out cNames: Integer ) : HResult', ccStdCall);
    AddMethod('GetRefTypeOfImplType',
      'function(index: Integer; out reftype: HRefType): HResult', ccStdCall);
    AddMethod('GetImplTypeFlags',
      'function(index: Integer; out impltypeflags: Integer): HResult', ccStdCall);
    AddMethod('GetIDsOfNames',
      'function(rgpszNames: POleStrList; cNames: Integer; rgmemid: PMemberIDList ) : HResult', ccStdCall);
    AddMethod('Invoke',
      'function(pvInstance: Pointer; memid: TMemberID; flags: Word; var dispParams: TDispParams ; varResult: PVariant ; excepInfo: PExcepInfo ; argErr: PInteger ) : HResult', ccStdCall);
    AddMethod('GetDocumentation',
      'function(memid: TMemberID; pbstrName: PWideString; pbstrDocString: PWideString ; pdwHelpContext: PLongint ; pbstrHelpFile: PWideString ) : HResult', ccStdCall);
    AddMethod('GetDllEntry',
      'function(memid: TMemberID; invkind: TInvokeKind; bstrDllName, bstrName: PWideString ; wOrdinal: PWord ) : HResult', ccStdCall);
    AddMethod('GetRefTypeInfo',
      'function(reftype: HRefType; out tinfo: ITypeInfo): HResult', ccStdCall);
    AddMethod('AddressOfMember',
      'function(memid: TMemberID; invkind: TInvokeKind; out ppv: Pointer ) : HResult', ccStdCall);
    AddMethod('CreateInstance',
      'function(const unkOuter: IUnknown; const iid: TIID; out vObj ) : HResult', ccStdCall);
    AddMethod('GetMops',
      'function(memid: TMemberID; out bstrMops: WideString): HResult', ccStdCall);
    AddMethod('GetContainingTypeLib',
      'function(out tlib: ITypeLib; out pindex: Integer): HResult', ccStdCall);
    AddMethod('ReleaseTypeAttr',
      'procedure(ptypeattr: PTypeAttr)', ccStdCall);
    AddMethod('ReleaseFuncDesc',
      'procedure(pfuncdesc: PFuncDesc)', ccStdCall);
    AddMethod('ReleaseVarDesc',
      'procedure(pvardesc: PVarDesc)', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ tagTLIBATTR import }
{--------------------}

function SepiImporttagTLIBATTR(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTLIBATTR', False, True);

  with Result do
  begin
    AddField('guid', 'TGUID');
    AddField('lcid', System.TypeInfo(TLCID));
    AddField('syskind', System.TypeInfo(TSysKind));
    AddField('wMajorVerNum', System.TypeInfo(Word));
    AddField('wMinorVerNum', System.TypeInfo(Word));
    AddField('wLibFlags', System.TypeInfo(Word));

    Complete;
  end;
end;

{-----------------}
{ ITypeLib import }
{-----------------}

function SepiImportITypeLib(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('ITypeLib'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(ITypeLib));

  with Result do
  begin
    AddMethod('GetTypeInfoCount',
      'function: Integer', ccStdCall);
    AddMethod('GetTypeInfo',
      'function(index: Integer; out tinfo: ITypeInfo): HResult', ccStdCall);
    AddMethod('GetTypeInfoType',
      'function(index: Integer; out tkind: TTypeKind): HResult', ccStdCall);
    AddMethod('GetTypeInfoOfGuid',
      'function(const guid: TGUID; out tinfo: ITypeInfo): HResult', ccStdCall);
    AddMethod('GetLibAttr',
      'function(out ptlibattr: PTLibAttr): HResult', ccStdCall);
    AddMethod('GetTypeComp',
      'function(out tcomp: ITypeComp): HResult', ccStdCall);
    AddMethod('GetDocumentation',
      'function(index: Integer; pbstrName: PWideString; pbstrDocString: PWideString ; pdwHelpContext: PLongint ; pbstrHelpFile: PWideString ) : HResult', ccStdCall);
    AddMethod('IsName',
      'function(szNameBuf: POleStr; lHashVal: Longint; out fName: BOOL): HResult', ccStdCall);
    AddMethod('FindName',
      'function(szNameBuf: POleStr; lHashVal: Longint; rgptinfo: PTypeInfoList ; rgmemid: PMemberIDList ; out pcFound: Word ) : HResult', ccStdCall);
    AddMethod('ReleaseTLibAttr',
      'procedure(ptlibattr: PTLibAttr)', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ tagCUSTDATAITEM import }
{------------------------}

function SepiImporttagCUSTDATAITEM(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCUSTDATAITEM', False, True);

  with Result do
  begin
    AddField('guid', 'TGUID');
    AddField('varValue', 'TVariantArg');

    Complete;
  end;
end;

{--------------------}
{ tagCUSTDATA import }
{--------------------}

function SepiImporttagCUSTDATA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCUSTDATA', False, True);

  with Result do
  begin
    AddField('cCustData', System.TypeInfo(DWORD));
    AddField('prgCustData', 'PCustDataItemList');

    Complete;
  end;
end;

{------------------}
{ ITypeLib2 import }
{------------------}

function SepiImportITypeLib2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('ITypeLib2'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(ITypeLib2));

  with Result do
  begin
    AddMethod('GetCustData',
      'function(guid: TGUID; out pVarVal: OleVariant ) : HResult', ccStdCall);
    AddMethod('GetLibStatistics',
      'function(pcUniqueNames: PLongInt; out pcchUniqueNames: LongInt ) : HResult', ccStdCall);
    AddMethod('GetDocumentation2',
      'function(index: Integer; lcid: TLCID; pbstrHelpString: PWideString ; pdwHelpStringContext: PDWORD ; pbstrHelpStringDll: PWideString ) : HResult', ccStdCall);
    AddMethod('GetAllCustData',
      'function(out pCustData: TCustData): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ ITypeInfo2 import }
{-------------------}

function SepiImportITypeInfo2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('ITypeInfo2'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(ITypeInfo2));

  with Result do
  begin
    AddMethod('GetTypeKind',
      'function(out pTypeKind: TTypeKind): HResult', ccStdCall);
    AddMethod('GetTypeFlags',
      'function(out pTypeFlags: LongInt): HResult', ccStdCall);
    AddMethod('GetFuncIndexOfMemId',
      'function(memid: TMemberID; invKind: TInvokeKind; out pFuncIndex: UINT ) : HResult', ccStdCall);
    AddMethod('GetVarIndexOfMemId',
      'function(memid: TMemberID; out pVarIndex: UINT): HResult', ccStdCall);
    AddMethod('GetCustData',
      'function(guid: TGUID; out pVarVal: OleVariant): HResult', ccStdCall);
    AddMethod('GetFuncCustData',
      'function(index: UINT; guid: TGUID; out pVarVal: OleVariant ) : HResult', ccStdCall);
    AddMethod('GetParamCustData',
      'function(indexFunc, indexParam: UINT; guid: TGUID; out pVarVal: OleVariant ) : HResult', ccStdCall);
    AddMethod('GetVarCustData',
      'function(index: UINT; guid: TGUID; out pVarVal: OleVariant ) : HResult', ccStdCall);
    AddMethod('GetImplTypeCustData',
      'function(index: UINT; guid: TGUID; out pVarVal: OleVariant ) : HResult', ccStdCall);
    AddMethod('GetDocumentation2',
      'function(memid: TMemberID; lcid: TLCID; pbstrHelpString: PWideString ; pdwHelpStringContext: PDWORD ; pbstrHelpStringDll: PWideString ) : HResult', ccStdCall);
    AddMethod('GetAllCustData',
      'function(out pCustData: TCustData): HResult', ccStdCall);
    AddMethod('GetAllFuncCustData',
      'function(index: UINT; out pCustData: TCustData): HResult', ccStdCall);
    AddMethod('GetAllParamCustData',
      'function(indexFunc, indexParam: UINT; out pCustData: TCustData ) : HResult', ccStdCall);
    AddMethod('GetAllVarCustData',
      'function(index: UINT; out pCustData: TCustData): HResult', ccStdCall);
    AddMethod('GetAllImplTypeCustData',
      'function(index: UINT; out pCustData: TCustData): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ IErrorInfo import }
{-------------------}

function SepiImportIErrorInfo(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IErrorInfo));

  with Result do
  begin
    AddMethod('GetGUID',
      'function(out guid: TGUID): HResult', ccStdCall);
    AddMethod('GetSource',
      'function(out bstrSource: WideString): HResult', ccStdCall);
    AddMethod('GetDescription',
      'function(out bstrDescription: WideString): HResult', ccStdCall);
    AddMethod('GetHelpFile',
      'function(out bstrHelpFile: WideString): HResult', ccStdCall);
    AddMethod('GetHelpContext',
      'function(out dwHelpContext: Longint): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ ICreateErrorInfo import }
{-------------------------}

function SepiImportICreateErrorInfo(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ICreateErrorInfo));

  with Result do
  begin
    AddMethod('SetGUID',
      'function(const guid: TGUID): HResult', ccStdCall);
    AddMethod('SetSource',
      'function(szSource: POleStr): HResult', ccStdCall);
    AddMethod('SetDescription',
      'function(szDescription: POleStr): HResult', ccStdCall);
    AddMethod('SetHelpFile',
      'function(szHelpFile: POleStr): HResult', ccStdCall);
    AddMethod('SetHelpContext',
      'function(dwHelpContext: Longint): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ ISupportErrorInfo import }
{--------------------------}

function SepiImportISupportErrorInfo(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ISupportErrorInfo));

  with Result do
  begin
    AddMethod('InterfaceSupportsErrorInfo',
      'function(const iid: TIID): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ tagPARAMDATA import }
{---------------------}

function SepiImporttagPARAMDATA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPARAMDATA', False, True);

  with Result do
  begin
    AddField('szName', 'POleStr');
    AddField('vt', System.TypeInfo(TVarType));

    Complete;
  end;
end;

{----------------------}
{ tagMETHODDATA import }
{----------------------}

function SepiImporttagMETHODDATA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagMETHODDATA', False, True);

  with Result do
  begin
    AddField('szName', 'POleStr');
    AddField('ppdata', 'PParamDataList');
    AddField('dispid', System.TypeInfo(TDispID));
    AddField('iMeth', System.TypeInfo(Integer));
    AddField('cc', System.TypeInfo(TCallConv));
    AddField('cArgs', System.TypeInfo(Integer));
    AddField('wFlags', System.TypeInfo(Word));
    AddField('vtReturn', System.TypeInfo(TVarType));

    Complete;
  end;
end;

{-------------------------}
{ tagINTERFACEDATA import }
{-------------------------}

function SepiImporttagINTERFACEDATA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagINTERFACEDATA', False, True);

  with Result do
  begin
    AddField('pmethdata', 'PMethodDataList');
    AddField('cMembers', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------------}
{ IOleAdviseHolder import }
{-------------------------}

function SepiImportIOleAdviseHolder(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleAdviseHolder));

  with Result do
  begin
    AddMethod('Advise',
      'function(const advise: IAdviseSink; out dwConnection: Longint): HResult', ccStdCall);
    AddMethod('Unadvise',
      'function(dwConnection: Longint): HResult', ccStdCall);
    AddMethod('EnumAdvise',
      'function(out enumAdvise: IEnumStatData): HResult', ccStdCall);
    AddMethod('SendOnRename',
      'function(const mk: IMoniker): HResult', ccStdCall);
    AddMethod('SendOnSave',
      'function: HResult', ccStdCall);
    AddMethod('SendOnClose',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{------------------}
{ IOleCache import }
{------------------}

function SepiImportIOleCache(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleCache));

  with Result do
  begin
    AddMethod('Cache',
      'function(const formatetc: TFormatEtc; advf: Longint; out dwConnection: Longint ) : HResult', ccStdCall);
    AddMethod('Uncache',
      'function(dwConnection: Longint): HResult', ccStdCall);
    AddMethod('EnumCache',
      'function(out enumStatData: IEnumStatData): HResult', ccStdCall);
    AddMethod('InitCache',
      'function(const dataObject: IDataObject): HResult', ccStdCall);
    AddMethod('SetData',
      'function(const formatetc: TFormatEtc; const medium: TStgMedium; fRelease: BOOL ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ IOleCache2 import }
{-------------------}

function SepiImportIOleCache2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleCache2));

  with Result do
  begin
    AddMethod('UpdateCache',
      'function(const dataObject: IDataObject; grfUpdf: Longint; pReserved: Pointer ) : HResult', ccStdCall);
    AddMethod('DiscardCache',
      'function(dwDiscardOptions: Longint): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ IOleCacheControl import }
{-------------------------}

function SepiImportIOleCacheControl(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleCacheControl));

  with Result do
  begin
    AddMethod('OnRun',
      'function(const dataObject: IDataObject): HResult', ccStdCall);
    AddMethod('OnStop',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IParseDisplayName import }
{--------------------------}

function SepiImportIParseDisplayName(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IParseDisplayName));

  with Result do
  begin
    AddMethod('ParseDisplayName',
      'function(const bc: IBindCtx; pszDisplayName: POleStr; out chEaten: Longint ; out mkOut: IMoniker ) : HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------}
{ IOleContainer import }
{----------------------}

function SepiImportIOleContainer(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleContainer));

  with Result do
  begin
    AddMethod('EnumObjects',
      'function(grfFlags: Longint; out Enum: IEnumUnknown): HResult', ccStdCall);
    AddMethod('LockContainer',
      'function(fLock: BOOL): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ IOleClientSite import }
{-----------------------}

function SepiImportIOleClientSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleClientSite));

  with Result do
  begin
    AddMethod('SaveObject',
      'function: HResult', ccStdCall);
    AddMethod('GetMoniker',
      'function(dwAssign: Longint; dwWhichMoniker: Longint; out mk: IMoniker ) : HResult', ccStdCall);
    AddMethod('GetContainer',
      'function(out container: IOleContainer): HResult', ccStdCall);
    AddMethod('ShowObject',
      'function: HResult', ccStdCall);
    AddMethod('OnShowWindow',
      'function(fShow: BOOL): HResult', ccStdCall);
    AddMethod('RequestNewObjectLayout',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ IOleObject import }
{-------------------}

function SepiImportIOleObject(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleObject));

  with Result do
  begin
    AddMethod('SetClientSite',
      'function(const clientSite: IOleClientSite): HResult', ccStdCall);
    AddMethod('GetClientSite',
      'function(out clientSite: IOleClientSite): HResult', ccStdCall);
    AddMethod('SetHostNames',
      'function(szContainerApp: POleStr; szContainerObj: POleStr ) : HResult', ccStdCall);
    AddMethod('Close',
      'function(dwSaveOption: Longint): HResult', ccStdCall);
    AddMethod('SetMoniker',
      'function(dwWhichMoniker: Longint; const mk: IMoniker): HResult', ccStdCall);
    AddMethod('GetMoniker',
      'function(dwAssign: Longint; dwWhichMoniker: Longint; out mk: IMoniker ) : HResult', ccStdCall);
    AddMethod('InitFromData',
      'function(const dataObject: IDataObject; fCreation: BOOL; dwReserved: Longint ) : HResult', ccStdCall);
    AddMethod('GetClipboardData',
      'function(dwReserved: Longint; out dataObject: IDataObject ) : HResult', ccStdCall);
    AddMethod('DoVerb',
      'function(iVerb: Longint; msg: PMsg; const activeSite: IOleClientSite; lindex: Longint ; hwndParent: HWND ; const posRect: TRect ) : HResult', ccStdCall);
    AddMethod('EnumVerbs',
      'function(out enumOleVerb: IEnumOleVerb): HResult', ccStdCall);
    AddMethod('Update',
      'function: HResult', ccStdCall);
    AddMethod('IsUpToDate',
      'function: HResult', ccStdCall);
    AddMethod('GetUserClassID',
      'function(out clsid: TCLSID): HResult', ccStdCall);
    AddMethod('GetUserType',
      'function(dwFormOfType: Longint; out pszUserType: POleStr): HResult', ccStdCall);
    AddMethod('SetExtent',
      'function(dwDrawAspect: Longint; const size: TPoint): HResult', ccStdCall);
    AddMethod('GetExtent',
      'function(dwDrawAspect: Longint; out size: TPoint): HResult', ccStdCall);
    AddMethod('Advise',
      'function(const advSink: IAdviseSink; out dwConnection: Longint): HResult', ccStdCall);
    AddMethod('Unadvise',
      'function(dwConnection: Longint): HResult', ccStdCall);
    AddMethod('EnumAdvise',
      'function(out enumAdvise: IEnumStatData): HResult', ccStdCall);
    AddMethod('GetMiscStatus',
      'function(dwAspect: Longint; out dwStatus: Longint): HResult', ccStdCall);
    AddMethod('SetColorScheme',
      'function(const logpal: TLogPalette): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------}
{ tagOBJECTDESCRIPTOR import }
{----------------------------}

function SepiImporttagOBJECTDESCRIPTOR(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagOBJECTDESCRIPTOR', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(Longint));
    AddField('clsid', 'TCLSID');
    AddField('dwDrawAspect', System.TypeInfo(Longint));
    AddField('size', 'TPoint');
    AddField('point', 'TPoint');
    AddField('dwStatus', System.TypeInfo(Longint));
    AddField('dwFullUserTypeName', System.TypeInfo(Longint));
    AddField('dwSrcOfCopy', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ IOleWindow import }
{-------------------}

function SepiImportIOleWindow(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleWindow));

  with Result do
  begin
    AddMethod('GetWindow',
      'function(out wnd: HWnd): HResult', ccStdCall);
    AddMethod('ContextSensitiveHelp',
      'function(fEnterMode: BOOL): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------}
{ IOleLink import }
{-----------------}

function SepiImportIOleLink(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleLink));

  with Result do
  begin
    AddMethod('SetUpdateOptions',
      'function(dwUpdateOpt: Longint): HResult', ccStdCall);
    AddMethod('GetUpdateOptions',
      'function(out dwUpdateOpt: Longint): HResult', ccStdCall);
    AddMethod('SetSourceMoniker',
      'function(const mk: IMoniker; const clsid: TCLSID): HResult', ccStdCall);
    AddMethod('GetSourceMoniker',
      'function(out mk: IMoniker): HResult', ccStdCall);
    AddMethod('SetSourceDisplayName',
      'function(pszDisplayName: POleStr): HResult', ccStdCall);
    AddMethod('GetSourceDisplayName',
      'function(out pszDisplayName: POleStr): HResult', ccStdCall);
    AddMethod('BindToSource',
      'function(bindflags: Longint; const bc: IBindCtx): HResult', ccStdCall);
    AddMethod('BindIfRunning',
      'function: HResult', ccStdCall);
    AddMethod('GetBoundSource',
      'function(out unk: IUnknown): HResult', ccStdCall);
    AddMethod('UnbindSource',
      'function: HResult', ccStdCall);
    AddMethod('Update',
      'function(const bc: IBindCtx): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IOleItemContainer import }
{--------------------------}

function SepiImportIOleItemContainer(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleItemContainer));

  with Result do
  begin
    AddMethod('GetObject',
      'function(pszItem: POleStr; dwSpeedNeeded: Longint; const bc: IBindCtx ; const iid: TIID ; out vObject ) : HResult', ccStdCall);
    AddMethod('GetObjectStorage',
      'function(pszItem: POleStr; const bc: IBindCtx; const iid: TIID ; out vStorage ) : HResult', ccStdCall);
    AddMethod('IsRunning',
      'function(pszItem: POleStr): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------}
{ IOleInPlaceUIWindow import }
{----------------------------}

function SepiImportIOleInPlaceUIWindow(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleInPlaceUIWindow));

  with Result do
  begin
    AddMethod('GetBorder',
      'function(out rectBorder: TRect): HResult', ccStdCall);
    AddMethod('RequestBorderSpace',
      'function(const borderwidths: TRect): HResult', ccStdCall);
    AddMethod('SetBorderSpace',
      'function(pborderwidths: PRect): HResult', ccStdCall);
    AddMethod('SetActiveObject',
      'function(const activeObject: IOleInPlaceActiveObject; pszObjName: POleStr ) : HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------------}
{ IOleInPlaceActiveObject import }
{--------------------------------}

function SepiImportIOleInPlaceActiveObject(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IOleInPlaceActiveObject'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IOleInPlaceActiveObject));

  with Result do
  begin
    AddMethod('TranslateAccelerator',
      'function(var msg: TMsg): HResult', ccStdCall);
    AddMethod('OnFrameWindowActivate',
      'function(fActivate: BOOL): HResult', ccStdCall);
    AddMethod('OnDocWindowActivate',
      'function(fActivate: BOOL): HResult', ccStdCall);
    AddMethod('ResizeBorder',
      'function(const rcBorder: TRect; const uiWindow: IOleInPlaceUIWindow; fFrameWindow: BOOL ) : HResult', ccStdCall);
    AddMethod('EnableModeless',
      'function(fEnable: BOOL): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------}
{ tagOIFI import }
{----------------}

function SepiImporttagOIFI(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagOIFI', False, True);

  with Result do
  begin
    AddField('cb', System.TypeInfo(Integer));
    AddField('fMDIApp', System.TypeInfo(BOOL));
    AddField('hwndFrame', System.TypeInfo(HWND));
    AddField('haccel', System.TypeInfo(HAccel));
    AddField('cAccelEntries', System.TypeInfo(Integer));

    Complete;
  end;
end;

{------------------------------}
{ tagOleMenuGroupWidths import }
{------------------------------}

function SepiImporttagOleMenuGroupWidths(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagOleMenuGroupWidths', False, True);

  with Result do
  begin
    AddField('width', '$19');

    Complete;
  end;
end;

{-------------------------}
{ IOleInPlaceFrame import }
{-------------------------}

function SepiImportIOleInPlaceFrame(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleInPlaceFrame));

  with Result do
  begin
    AddMethod('InsertMenus',
      'function(hmenuShared: HMenu; var menuWidths: TOleMenuGroupWidths ) : HResult', ccStdCall);
    AddMethod('SetMenu',
      'function(hmenuShared: HMenu; holemenu: HMenu; hwndActiveObject: HWnd ) : HResult', ccStdCall);
    AddMethod('RemoveMenus',
      'function(hmenuShared: HMenu): HResult', ccStdCall);
    AddMethod('SetStatusText',
      'function(pszStatusText: POleStr): HResult', ccStdCall);
    AddMethod('EnableModeless',
      'function(fEnable: BOOL): HResult', ccStdCall);
    AddMethod('TranslateAccelerator',
      'function(var msg: TMsg; wID: Word): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IOleInPlaceObject import }
{--------------------------}

function SepiImportIOleInPlaceObject(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleInPlaceObject));

  with Result do
  begin
    AddMethod('InPlaceDeactivate',
      'function: HResult', ccStdCall);
    AddMethod('UIDeactivate',
      'function: HResult', ccStdCall);
    AddMethod('SetObjectRects',
      'function(const rcPosRect: TRect; const rcClipRect: TRect ) : HResult', ccStdCall);
    AddMethod('ReactivateAndUndo',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IOleInPlaceSite import }
{------------------------}

function SepiImportIOleInPlaceSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleInPlaceSite));

  with Result do
  begin
    AddMethod('CanInPlaceActivate',
      'function: HResult', ccStdCall);
    AddMethod('OnInPlaceActivate',
      'function: HResult', ccStdCall);
    AddMethod('OnUIActivate',
      'function: HResult', ccStdCall);
    AddMethod('GetWindowContext',
      'function(out frame: IOleInPlaceFrame; out doc: IOleInPlaceUIWindow ; out rcPosRect: TRect ; out rcClipRect: TRect ; out frameInfo: TOleInPlaceFrameInfo ) : HResult', ccStdCall);
    AddMethod('Scroll',
      'function(scrollExtent: TPoint): HResult', ccStdCall);
    AddMethod('OnUIDeactivate',
      'function(fUndoable: BOOL): HResult', ccStdCall);
    AddMethod('OnInPlaceDeactivate',
      'function: HResult', ccStdCall);
    AddMethod('DiscardUndoState',
      'function: HResult', ccStdCall);
    AddMethod('DeactivateAndUndo',
      'function: HResult', ccStdCall);
    AddMethod('OnPosRectChange',
      'function(const rcPosRect: TRect): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ IViewObject import }
{--------------------}

function SepiImportIViewObject(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IViewObject));

  with Result do
  begin
    AddMethod('Draw',
      'function(dwDrawAspect: Longint; lindex: Longint; pvAspect: Pointer; ptd: PDVTargetDevice ; hicTargetDev: HDC ; hdcDraw: HDC ; prcBounds: PRect ; prcWBounds: PRect ; fnContinue: TContinueFunc ; dwContinue: Longint ) : HResult', ccStdCall);
    AddMethod('GetColorSet',
      'function(dwDrawAspect: Longint; lindex: Longint; pvAspect: Pointer ; ptd: PDVTargetDevice ; hicTargetDev: HDC ; out colorSet: PLogPalette ) : HResult', ccStdCall);
    AddMethod('Freeze',
      'function(dwDrawAspect: Longint; lindex: Longint; pvAspect: Pointer; out dwFreeze: Longint ) : HResult', ccStdCall);
    AddMethod('Unfreeze',
      'function(dwFreeze: Longint): HResult', ccStdCall);
    AddMethod('SetAdvise',
      'function(aspects: Longint; advf: Longint; const advSink: IAdviseSink ) : HResult', ccStdCall);
    AddMethod('GetAdvise',
      'function(pAspects: PLongint; pAdvf: PLongint; out advSink: IAdviseSink ) : HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IViewObject2 import }
{---------------------}

function SepiImportIViewObject2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IViewObject2));

  with Result do
  begin
    AddMethod('GetExtent',
      'function(dwDrawAspect: Longint; lindex: Longint; ptd: PDVTargetDevice ; out size: TPoint ) : HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ IDropSource import }
{--------------------}

function SepiImportIDropSource(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDropSource));

  with Result do
  begin
    AddMethod('QueryContinueDrag',
      'function(fEscapePressed: BOOL; grfKeyState: Longint ) : HResult', ccStdCall);
    AddMethod('GiveFeedback',
      'function(dwEffect: Longint): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ IDropTarget import }
{--------------------}

function SepiImportIDropTarget(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDropTarget));

  with Result do
  begin
    AddMethod('DragEnter',
      'function(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint ; var dwEffect: Longint ) : HResult', ccStdCall);
    AddMethod('DragOver',
      'function(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ) : HResult', ccStdCall);
    AddMethod('DragLeave',
      'function: HResult', ccStdCall);
    AddMethod('Drop',
      'function(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ tagOLEVERB import }
{-------------------}

function SepiImporttagOLEVERB(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagOLEVERB', False, True);

  with Result do
  begin
    AddField('lVerb', System.TypeInfo(Longint));
    AddField('lpszVerbName', 'POleStr');
    AddField('fuFlags', System.TypeInfo(Longint));
    AddField('grfAttribs', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ IEnumOLEVERB import }
{---------------------}

function SepiImportIEnumOLEVERB(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IEnumOLEVERB'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IEnumOLEVERB));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: Longint; out elt; pceltFetched: PLongint ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: Longint): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out enm: IEnumOleVerb): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ IOleControl import }
{--------------------}

function SepiImportIOleControl(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IOleControl'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IOleControl));

  with Result do
  begin
    AddMethod('GetControlInfo',
      'function(var ci: TControlInfo): HResult', ccStdCall);
    AddMethod('OnMnemonic',
      'function(msg: PMsg): HResult', ccStdCall);
    AddMethod('OnAmbientPropertyChange',
      'function(dispid: TDispID): HResult', ccStdCall);
    AddMethod('FreezeEvents',
      'function(bFreeze: BOOL): HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IOleControlSite import }
{------------------------}

function SepiImportIOleControlSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IOleControlSite'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IOleControlSite));

  with Result do
  begin
    AddMethod('OnControlInfoChanged',
      'function: HResult', ccStdCall);
    AddMethod('LockInPlaceActive',
      'function(fLock: BOOL): HResult', ccStdCall);
    AddMethod('GetExtendedControl',
      'function(out disp: IDispatch): HResult', ccStdCall);
    AddMethod('TransformCoords',
      'function(var ptlHimetric: TPoint; var ptfContainer: TPointF; flags: Longint ) : HResult', ccStdCall);
    AddMethod('TranslateAccelerator',
      'function(msg: PMsg; grfModifiers: Longint): HResult', ccStdCall);
    AddMethod('OnFocus',
      'function(fGotFocus: BOOL): HResult', ccStdCall);
    AddMethod('ShowPropertyFrame',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ ISimpleFrameSite import }
{-------------------------}

function SepiImportISimpleFrameSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('ISimpleFrameSite'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(ISimpleFrameSite));

  with Result do
  begin
    AddMethod('PreMessageFilter',
      'function(wnd: HWnd; msg, wp, lp: Integer; out res: Integer ; out Cookie: Longint ) : HResult', ccStdCall);
    AddMethod('PostMessageFilter',
      'function(wnd: HWnd; msg, wp, lp: Integer; out res: Integer ; Cookie: Longint ) : HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IObjectWithSite import }
{------------------------}

function SepiImportIObjectWithSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IObjectWithSite));

  with Result do
  begin
    AddMethod('SetSite',
      'function(const pUnkSite: IUnknown ):HResult', ccStdCall);
    AddMethod('GetSite',
      'function(const riid: TIID; out site: IUnknown):HResult', ccStdCall);

    Complete;
  end;
end;

{------------------}
{ IErrorLog import }
{------------------}

function SepiImportIErrorLog(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IErrorLog));

  with Result do
  begin
    AddMethod('AddError',
      'function(pszPropName: POleStr; pExcepInfo: PExcepInfo): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IPropertyBag import }
{---------------------}

function SepiImportIPropertyBag(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPropertyBag));

  with Result do
  begin
    AddMethod('Read',
      'function(pszPropName: POleStr; var pvar: OleVariant; const pErrorLog: IErrorLog ) : HResult', ccStdCall);
    AddMethod('Write',
      'function(pszPropName: POleStr; const pvar: OleVariant): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------}
{ IPersistPropertyBag import }
{----------------------------}

function SepiImportIPersistPropertyBag(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IPersistPropertyBag'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IPersistPropertyBag));

  with Result do
  begin
    AddMethod('InitNew',
      'function: HResult', ccStdCall);
    AddMethod('Load',
      'function(const pPropBag: IPropertyBag; const pErrorLog: IErrorLog ) : HResult', ccStdCall);
    AddMethod('Save',
      'function(const pPropBag: IPropertyBag; fClearDirty: BOOL; fSaveAllProperties: BOOL ) : HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------------}
{ IPersistStreamInit import }
{---------------------------}

function SepiImportIPersistStreamInit(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IPersistStreamInit'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IPersistStreamInit));

  with Result do
  begin
    AddMethod('InitNew',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------}
{ IPropertyNotifySink import }
{----------------------------}

function SepiImportIPropertyNotifySink(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IPropertyNotifySink'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IPropertyNotifySink));

  with Result do
  begin
    AddMethod('OnChanged',
      'function(dispid: TDispID): HResult', ccStdCall);
    AddMethod('OnRequestEdit',
      'function(dispid: TDispID): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IProvideClassInfo import }
{--------------------------}

function SepiImportIProvideClassInfo(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IProvideClassInfo'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IProvideClassInfo));

  with Result do
  begin
    AddMethod('GetClassInfo',
      'function(out ti: ITypeInfo): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------------}
{ IConnectionPointContainer import }
{----------------------------------}

function SepiImportIConnectionPointContainer(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IConnectionPointContainer'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IConnectionPointContainer));

  with Result do
  begin
    AddMethod('EnumConnectionPoints',
      'function(out Enum: IEnumConnectionPoints): HResult', ccStdCall);
    AddMethod('FindConnectionPoint',
      'function(const iid: TIID; out cp: IConnectionPoint ) : HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------------}
{ IEnumConnectionPoints import }
{------------------------------}

function SepiImportIEnumConnectionPoints(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IEnumConnectionPoints'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IEnumConnectionPoints));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: Longint; out elt; pceltFetched: PLongint ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: Longint): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out Enum: IEnumConnectionPoints): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ IConnectionPoint import }
{-------------------------}

function SepiImportIConnectionPoint(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IConnectionPoint'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IConnectionPoint));

  with Result do
  begin
    AddMethod('GetConnectionInterface',
      'function(out iid: TIID): HResult', ccStdCall);
    AddMethod('GetConnectionPointContainer',
      'function(out cpc: IConnectionPointContainer): HResult', ccStdCall);
    AddMethod('Advise',
      'function(const unkSink: IUnknown; out dwCookie: Longint): HResult', ccStdCall);
    AddMethod('Unadvise',
      'function(dwCookie: Longint): HResult', ccStdCall);
    AddMethod('EnumConnections',
      'function(out Enum: IEnumConnections): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ tagCONNECTDATA import }
{-----------------------}

function SepiImporttagCONNECTDATA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCONNECTDATA', False, True,
    TypeInfo(tagCONNECTDATA));

  with Result do
  begin
    AddField('pUnk', System.TypeInfo(IUnknown));
    AddField('dwCookie', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ IEnumConnections import }
{-------------------------}

function SepiImportIEnumConnections(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IEnumConnections'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IEnumConnections));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: Longint; out elt; pceltFetched: PLongint ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: Longint): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out Enum: IEnumConnections): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ tagLICINFO import }
{-------------------}

function SepiImporttagLICINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLICINFO', False, True);

  with Result do
  begin
    AddField('cbLicInfo', System.TypeInfo(Longint));
    AddField('fRuntimeKeyAvail', System.TypeInfo(BOOL));
    AddField('fLicVerified', System.TypeInfo(BOOL));

    Complete;
  end;
end;

{-----------------------}
{ IClassFactory2 import }
{-----------------------}

function SepiImportIClassFactory2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IClassFactory2'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IClassFactory2));

  with Result do
  begin
    AddMethod('GetLicInfo',
      'function(var licInfo: TLicInfo): HResult', ccStdCall);
    AddMethod('RequestLicKey',
      'function(dwResrved: Longint; out bstrKey: WideString): HResult', ccStdCall);
    AddMethod('CreateInstanceLic',
      'function(const unkOuter: IUnknown; const unkReserved: IUnknown; const iid: TIID ; const bstrKey: WideString ; out vObject ) : HResult', ccStdCall);

    Complete;
  end;
end;

{------------------}
{ tagCAUUID import }
{------------------}

function SepiImporttagCAUUID(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCAUUID', False, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(Longint));
    AddField('pElems', 'PGUIDList');

    Complete;
  end;
end;

{----------------------}
{ tagCALPOLESTR import }
{----------------------}

function SepiImporttagCALPOLESTR(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCALPOLESTR', False, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(Longint));
    AddField('pElems', 'POleStrList');

    Complete;
  end;
end;

{-------------------}
{ tagCADWORD import }
{-------------------}

function SepiImporttagCADWORD(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCADWORD', False, True);

  with Result do
  begin
    AddField('cElems', System.TypeInfo(Longint));
    AddField('pElems', 'PLongintList');

    Complete;
  end;
end;

{------------------------------------}
{ IOleInPlaceObjectWindowless import }
{------------------------------------}

function SepiImportIOleInPlaceObjectWindowless(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleInPlaceObjectWindowless));

  with Result do
  begin
    AddMethod('OnWindowMessage',
      'function(msg: LongWord; wParam: WPARAM; lParam: LPARAM; var lResult: LRESULT ) : HResult', ccStdCall);
    AddMethod('GetDropTarget',
      'function(out pDropTarget: IDropTarget):HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IOleInPlaceSiteEx import }
{--------------------------}

function SepiImportIOleInPlaceSiteEx(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleInPlaceSiteEx));

  with Result do
  begin
    AddMethod('OnInPlaceActivateEx',
      'function(fNoRedraw: PBOOL; dwFlags: DWORD ) : HResult', ccStdCall);
    AddMethod('OnInPlaceDeActivateEx',
      'function(fNoRedraw: BOOL): HResult', ccStdCall);
    AddMethod('RequestUIActivate',
      'function: HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------------}
{ IOleInPlaceSiteWindowless import }
{----------------------------------}

function SepiImportIOleInPlaceSiteWindowless(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleInPlaceSiteWindowless));

  with Result do
  begin
    AddMethod('CanWindowlessActivate',
      'function: HResult', ccStdCall);
    AddMethod('GetCapture',
      'function: HResult', ccStdCall);
    AddMethod('SetCapture',
      'function(fCapture: BOOL): HResult', ccStdCall);
    AddMethod('GetFocus',
      'function: HResult', ccStdCall);
    AddMethod('SetFocus',
      'function(fFocus: BOOL): HResult', ccStdCall);
    AddMethod('GetDC',
      'function(var Rect: TRect; qrfFlags: DWORD; var hDC: HDC ) : HResult', ccStdCall);
    AddMethod('ReleaseDC',
      'function(hDC: HDC): HResult', ccStdCall);
    AddMethod('InvalidateRect',
      'function(var Rect: TRect; fErase: BOOL): HResult', ccStdCall);
    AddMethod('InvalidateRgn',
      'function(hRGN: HRGN; fErase: BOOL): HResult', ccStdCall);
    AddMethod('ScrollRect',
      'function(dx, dy: Integer; var RectScroll: TRect; var RectClip: TRect ) : HResult', ccStdCall);
    AddMethod('AdjustRect',
      'function(var rc: TRect): HResult', ccStdCall);
    AddMethod('OnDefWindowMessage',
      'function(msg: LongWord; wParam: WPARAM; lParam: LPARAM ; var LResult: LRESULT ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ tagOCPFIPARAMS import }
{-----------------------}

function SepiImporttagOCPFIPARAMS(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagOCPFIPARAMS', False, True);

  with Result do
  begin
    AddField('cbStructSize', System.TypeInfo(Longint));
    AddField('hWndOwner', System.TypeInfo(HWnd));
    AddField('x', System.TypeInfo(Integer));
    AddField('y', System.TypeInfo(Integer));
    AddField('lpszCaption', 'POleStr');
    AddField('cObjects', System.TypeInfo(Longint));
    AddField('pObjects', 'Pointer');
    AddField('cPages', System.TypeInfo(Longint));
    AddField('pPages', 'Pointer');
    AddField('lcid', System.TypeInfo(TLCID));
    AddField('dispidInitialProperty', System.TypeInfo(TDispID));

    Complete;
  end;
end;

{------------------------}
{ tagPROPPAGEINFO import }
{------------------------}

function SepiImporttagPROPPAGEINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPROPPAGEINFO', False, True);

  with Result do
  begin
    AddField('cb', System.TypeInfo(Longint));
    AddField('pszTitle', 'POleStr');
    AddField('size', 'TSize');
    AddField('pszDocString', 'POleStr');
    AddField('pszHelpFile', 'POleStr');
    AddField('dwHelpContext', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------------}
{ ISpecifyPropertyPages import }
{------------------------------}

function SepiImportISpecifyPropertyPages(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('ISpecifyPropertyPages'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(ISpecifyPropertyPages));

  with Result do
  begin
    AddMethod('GetPages',
      'function(out pages: TCAGUID): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------------}
{ IPerPropertyBrowsing import }
{-----------------------------}

function SepiImportIPerPropertyBrowsing(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IPerPropertyBrowsing'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IPerPropertyBrowsing));

  with Result do
  begin
    AddMethod('GetDisplayString',
      'function(dispid: TDispID; out bstr: WideString): HResult', ccStdCall);
    AddMethod('MapPropertyToPage',
      'function(dispid: TDispID; out clsid: TCLSID): HResult', ccStdCall);
    AddMethod('GetPredefinedStrings',
      'function(dispid: TDispID; out caStringsOut: TCAPOleStr; out caCookiesOut: TCALongint ) : HResult', ccStdCall);
    AddMethod('GetPredefinedValue',
      'function(dispid: TDispID; dwCookie: Longint; out varOut: OleVariant ) : HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IPropertyPageSite import }
{--------------------------}

function SepiImportIPropertyPageSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IPropertyPageSite'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IPropertyPageSite));

  with Result do
  begin
    AddMethod('OnStatusChange',
      'function(flags: Longint): HResult', ccStdCall);
    AddMethod('GetLocaleID',
      'function(out localeID: TLCID): HResult', ccStdCall);
    AddMethod('GetPageContainer',
      'function(out unk: IUnknown): HResult', ccStdCall);
    AddMethod('TranslateAccelerator',
      'function(msg: PMsg): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------}
{ IPropertyPage import }
{----------------------}

function SepiImportIPropertyPage(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IPropertyPage'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IPropertyPage));

  with Result do
  begin
    AddMethod('SetPageSite',
      'function(const pageSite: IPropertyPageSite): HResult', ccStdCall);
    AddMethod('Activate',
      'function(hwndParent: HWnd; const rc: TRect; bModal: BOOL): HResult', ccStdCall);
    AddMethod('Deactivate',
      'function: HResult', ccStdCall);
    AddMethod('GetPageInfo',
      'function(out pageInfo: TPropPageInfo): HResult', ccStdCall);
    AddMethod('SetObjects',
      'function(cObjects: Longint; pUnkList: PUnknownList): HResult', ccStdCall);
    AddMethod('Show',
      'function(nCmdShow: Integer): HResult', ccStdCall);
    AddMethod('Move',
      'function(const rect: TRect): HResult', ccStdCall);
    AddMethod('IsPageDirty',
      'function: HResult', ccStdCall);
    AddMethod('Apply',
      'function: HResult', ccStdCall);
    AddMethod('Help',
      'function(pszHelpDir: POleStr): HResult', ccStdCall);
    AddMethod('TranslateAccelerator',
      'function(msg: PMsg): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ IPropertyPage2 import }
{-----------------------}

function SepiImportIPropertyPage2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IPropertyPage2'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IPropertyPage2));

  with Result do
  begin
    AddMethod('EditProperty',
      'function(dispid: TDispID): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------}
{ IFont import }
{--------------}

function SepiImportIFont(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IFont));

  with Result do
  begin
    AddMethod('get_Name',
      'function(out name: WideString): HResult', ccStdCall);
    AddMethod('put_Name',
      'function(name: WideString): HResult', ccStdCall);
    AddMethod('get_Size',
      'function(out size: Currency): HResult', ccStdCall);
    AddMethod('put_Size',
      'function(size: Currency): HResult', ccStdCall);
    AddMethod('get_Bold',
      'function(out bold: BOOL): HResult', ccStdCall);
    AddMethod('put_Bold',
      'function(bold: BOOL): HResult', ccStdCall);
    AddMethod('get_Italic',
      'function(out italic: BOOL): HResult', ccStdCall);
    AddMethod('put_Italic',
      'function(italic: BOOL): HResult', ccStdCall);
    AddMethod('get_Underline',
      'function(out underline: BOOL): HResult', ccStdCall);
    AddMethod('put_Underline',
      'function(underline: BOOL): HResult', ccStdCall);
    AddMethod('get_Strikethrough',
      'function(out strikethrough: BOOL): HResult', ccStdCall);
    AddMethod('put_Strikethrough',
      'function(strikethrough: BOOL): HResult', ccStdCall);
    AddMethod('get_Weight',
      'function(out weight: Smallint): HResult', ccStdCall);
    AddMethod('put_Weight',
      'function(weight: Smallint): HResult', ccStdCall);
    AddMethod('get_Charset',
      'function(out charset: Smallint): HResult', ccStdCall);
    AddMethod('put_Charset',
      'function(charset: Smallint): HResult', ccStdCall);
    AddMethod('get_hFont',
      'function(out font: HFont): HResult', ccStdCall);
    AddMethod('Clone',
      'function(out font: IFont): HResult', ccStdCall);
    AddMethod('IsEqual',
      'function(const fontOther: IFont): HResult', ccStdCall);
    AddMethod('SetRatio',
      'function(cyLogical, cyHimetric: Longint): HResult', ccStdCall);
    AddMethod('QueryTextMetrics',
      'function(out tm: TTextMetricOle): HResult', ccStdCall);
    AddMethod('AddRefHfont',
      'function(font: HFont): HResult', ccStdCall);
    AddMethod('ReleaseHfont',
      'function(font: HFont): HResult', ccStdCall);

    Complete;
  end;
end;

{------------------}
{ IFontDisp import }
{------------------}

function SepiImportIFontDisp(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IFontDisp));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------------------------}
{ tagSOLE_AUTHENTICATION_SERVICE import }
{---------------------------------------}

function SepiImporttagSOLE_AUTHENTICATION_SERVICE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSOLE_AUTHENTICATION_SERVICE', False, True);

  with Result do
  begin
    AddField('dwAuthnSvc', System.TypeInfo(Longint));
    AddField('dwAuthzSvc', System.TypeInfo(Longint));
    AddField('pPrincipalName', 'POleStr');
    AddField('hr', System.TypeInfo(HResult));

    Complete;
  end;
end;

{--------------------}
{ tagFONTDESC import }
{--------------------}

function SepiImporttagFONTDESC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagFONTDESC', False, True);

  with Result do
  begin
    AddField('cbSizeofstruct', System.TypeInfo(Integer));
    AddField('lpstrName', 'POleStr');
    AddField('cySize', System.TypeInfo(Currency));
    AddField('sWeight', System.TypeInfo(Smallint));
    AddField('sCharset', System.TypeInfo(Smallint));
    AddField('fItalic', System.TypeInfo(BOOL));
    AddField('fUnderline', System.TypeInfo(BOOL));
    AddField('fStrikethrough', System.TypeInfo(BOOL));

    Complete;
  end;
end;

{-----------------}
{ IPicture import }
{-----------------}

function SepiImportIPicture(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPicture));

  with Result do
  begin
    AddMethod('get_Handle',
      'function(out handle: OLE_HANDLE): HResult', ccStdCall);
    AddMethod('get_hPal',
      'function(out handle: OLE_HANDLE): HResult', ccStdCall);
    AddMethod('get_Type',
      'function(out typ: Smallint): HResult', ccStdCall);
    AddMethod('get_Width',
      'function(out width: OLE_XSIZE_HIMETRIC): HResult', ccStdCall);
    AddMethod('get_Height',
      'function(out height: OLE_YSIZE_HIMETRIC): HResult', ccStdCall);
    AddMethod('Render',
      'function(dc: HDC; x, y, cx, cy: Longint; xSrc: OLE_XPOS_HIMETRIC ; ySrc: OLE_YPOS_HIMETRIC ; cxSrc: OLE_XSIZE_HIMETRIC ; cySrc: OLE_YSIZE_HIMETRIC ; const rcWBounds: TRect ) : HResult', ccStdCall);
    AddMethod('set_hPal',
      'function(hpal: OLE_HANDLE): HResult', ccStdCall);
    AddMethod('get_CurDC',
      'function(out dcOut: HDC): HResult', ccStdCall);
    AddMethod('SelectPicture',
      'function(dcIn: HDC; out hdcOut: HDC; out bmpOut: OLE_HANDLE ) : HResult', ccStdCall);
    AddMethod('get_KeepOriginalFormat',
      'function(out fkeep: BOOL): HResult', ccStdCall);
    AddMethod('put_KeepOriginalFormat',
      'function(fkeep: BOOL): HResult', ccStdCall);
    AddMethod('PictureChanged',
      'function: HResult', ccStdCall);
    AddMethod('SaveAsFile',
      'function(const stream: IStream; fSaveMemCopy: BOOL; out cbSize: Longint ) : HResult', ccStdCall);
    AddMethod('get_Attributes',
      'function(out dwAttr: Longint): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IPictureDisp import }
{---------------------}

function SepiImportIPictureDisp(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPictureDisp));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------}
{ tagPICTDESC import }
{--------------------}

function SepiImporttagPICTDESC(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPICTDESC', False, True);

  with Result do
  begin
    AddField('cbSizeofstruct', System.TypeInfo(Integer));
    AddField('picType', System.TypeInfo(Integer));
    AddFieldAfter('hbitmap', System.TypeInfo(THandle), 'picType');
    AddFieldAfter('hpal', System.TypeInfo(THandle), 'hbitmap');
    AddFieldAfter('hMeta', System.TypeInfo(THandle), 'picType');
    AddFieldAfter('xExt', System.TypeInfo(Integer), 'hMeta');
    AddField('yExt', System.TypeInfo(Integer), True);
    AddFieldAfter('hIcon', System.TypeInfo(THandle), 'picType');
    AddFieldAfter('hemf', System.TypeInfo(THandle), 'picType');

    Complete;
  end;
end;

{-------------------------}
{ IOleDocumentView import }
{-------------------------}

function SepiImportIOleDocumentView(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleDocumentView));

  with Result do
  begin
    AddMethod('SetInPlaceSite',
      'function(Site: IOleInPlaceSite):HResult', ccStdCall);
    AddMethod('GetInPlaceSite',
      'function(out Site: IOleInPlaceSite):HResult', ccStdCall);
    AddMethod('GetDocument',
      'function(out P: IUnknown):HResult', ccStdCall);
    AddMethod('SetRect',
      'function(const View: TRECT):HResult', ccStdCall);
    AddMethod('GetRect',
      'function(var View: TRECT):HResult', ccStdCall);
    AddMethod('SetRectComplex',
      'function(const View, HScroll, VScroll, SizeBox):HResult', ccStdCall);
    AddMethod('Show',
      'function(fShow: BOOL):HResult', ccStdCall);
    AddMethod('UIActivate',
      'function(fUIActivate: BOOL):HResult', ccStdCall);
    AddMethod('Open',
      'function:HResult', ccStdCall);
    AddMethod('CloseView',
      'function(dwReserved: DWORD):HResult', ccStdCall);
    AddMethod('SaveViewState',
      'function(pstm: IStream):HResult', ccStdCall);
    AddMethod('ApplyViewState',
      'function(pstm: IStream):HResult', ccStdCall);
    AddMethod('Clone',
      'function(NewSite: IOleInPlaceSite; out NewView: IOleDocumentView):HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------------}
{ IEnumOleDocumentViews import }
{------------------------------}

function SepiImportIEnumOleDocumentViews(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IEnumOleDocumentViews));

  with Result do
  begin
    AddMethod('Next',
      'function(Count: Longint; out View: IOleDocumentView; var Fetched: Longint):HResult', ccStdCall);
    AddMethod('Skip',
      'function(Count: Longint):HResult', ccStdCall);
    AddMethod('Reset',
      'function:HResult', ccStdCall);
    AddMethod('Clone',
      'function(out Enum: IEnumOleDocumentViews):HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IOleDocument import }
{---------------------}

function SepiImportIOleDocument(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleDocument));

  with Result do
  begin
    AddMethod('CreateView',
      'function(Site: IOleInPlaceSite; Stream: IStream; rsrvd: DWORD; out View: IOleDocumentView ) : HResult', ccStdCall);
    AddMethod('GetDocMiscStatus',
      'function(var Status: DWORD):HResult', ccStdCall);
    AddMethod('EnumViews',
      'function(out Enum: IEnumOleDocumentViews; out View: IOleDocumentView ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ IOleDocumentSite import }
{-------------------------}

function SepiImportIOleDocumentSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleDocumentSite));

  with Result do
  begin
    AddMethod('ActivateMe',
      'function(View: IOleDocumentView): HRESULT', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IContinueCallback import }
{--------------------------}

function SepiImportIContinueCallback(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IContinueCallback));

  with Result do
  begin
    AddMethod('Continue',
      'function: HResult', ccStdCall);
    AddMethod('ContinuePrinting',
      'function( nCntPrinted, nCurPage: Longint; PrintStatus: PWideChar ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ IServiceProvider import }
{-------------------------}

function SepiImportIServiceProvider(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IServiceProvider));

  with Result do
  begin
    AddMethod('QueryService',
      'function(const rsid, iid: TGuid; out Obj): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ tagPAGERANGE import }
{---------------------}

function SepiImporttagPAGERANGE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPAGERANGE', False, True);

  with Result do
  begin
    AddField('nFromPage', System.TypeInfo(Longint));
    AddField('nToPage', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ tagPAGESET import }
{-------------------}

function SepiImporttagPAGESET(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPAGESET', False, True);

  with Result do
  begin
    AddField('cbStruct', System.TypeInfo(Cardinal));
    AddField('fOddPages', System.TypeInfo(BOOL));
    AddField('fEvenPages', System.TypeInfo(BOOL));
    AddField('cPageRange', System.TypeInfo(Cardinal));
    AddField('rgPages', '$20');

    Complete;
  end;
end;

{---------------}
{ IPrint import }
{---------------}

function SepiImportIPrint(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPrint));

  with Result do
  begin
    AddMethod('SetInitialPageNum',
      'function(nFirstPage: Longint): HResult', ccStdCall);
    AddMethod('GetPageInfo',
      'function(var pnFirstPage, pcPages: Longint): HResult', ccStdCall);
    AddMethod('Print',
      'function(grfFlags: DWORD; var td: TDVTARGETDEVICE; PageSet: PPageSet ; stgmOptions: PStgMedium ; Callback: IContinueCallback ; FirstPage: Longint ; pcPagesPrinted, pnLastPage: PLongint ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ _tagOLECMD import }
{-------------------}

function SepiImport_tagOLECMD(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_tagOLECMD', False, True);

  with Result do
  begin
    AddField('cmdID', System.TypeInfo(Cardinal));
    AddField('cmdf', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ _tagOLECMDTEXT import }
{-----------------------}

function SepiImport_tagOLECMDTEXT(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_tagOLECMDTEXT', False, True);

  with Result do
  begin
    AddField('cmdtextf', System.TypeInfo(Longint));
    AddField('cwActual', System.TypeInfo(Cardinal));
    AddField('cwBuf', System.TypeInfo(Cardinal));
    AddField('rgwz', '$21');

    Complete;
  end;
end;

{--------------------------}
{ IOleCommandTarget import }
{--------------------------}

function SepiImportIOleCommandTarget(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleCommandTarget));

  with Result do
  begin
    AddMethod('QueryStatus',
      'function(CmdGroup: PGUID; cCmds: Cardinal; prgCmds: POleCmd ; CmdText: POleCmdText ) : HResult', ccStdCall);
    AddMethod('Exec',
      'function(CmdGroup: PGUID; nCmdID, nCmdexecopt: DWORD; const vaIn: OleVariant ; var vaOut: OleVariant ) : HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IActiveDesigner import }
{------------------------}

function SepiImportIActiveDesigner(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IActiveDesigner));

  with Result do
  begin
    AddMethod('GetRuntimeClassID',
      'function(var clsid: TGUID): HResult', ccStdCall);
    AddMethod('GetRuntimeMiscStatusFlags',
      'function(var dwMiscFlags: DWORD): HResult', ccStdCall);
    AddMethod('QueryPersistenceInterface',
      'function(const iid: TGUID): HResult', ccStdCall);
    AddMethod('SaveRuntimeState',
      'function(const iidItf: TGUID; const iidObj: TGUID; Obj: IUnknown): HResult', ccStdCall);
    AddMethod('GetExtensibilityObject',
      'function(var ppvObjOut: IDispatch): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------------}
{ IPersistTextStream import }
{---------------------------}

function SepiImportIPersistTextStream(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPersistTextStream));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------------}
{ IProvideRuntimeText import }
{----------------------------}

function SepiImportIProvideRuntimeText(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IProvideRuntimeText));

  with Result do
  begin
    AddMethod('GetRuntimeText',
      'function( var strRuntimeText: TBSTR ): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------}
{ TCATEGORYINFO import }
{----------------------}

function SepiImportTCATEGORYINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCATEGORYINFO', False, True);

  with Result do
  begin
    AddField('catid', 'TGUID');
    AddField('lcid', System.TypeInfo(UINT));
    AddField('szDescription', '$22');

    Complete;
  end;
end;

{------------------}
{ IEnumGUID import }
{------------------}

function SepiImportIEnumGUID(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IEnumGUID'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IEnumGUID));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: UINT; out rgelt: TGUID; out pceltFetched: UINT): HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: UINT): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out ppenum: IEnumGUID): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IEnumCATEGORYINFO import }
{--------------------------}

function SepiImportIEnumCATEGORYINFO(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IEnumCATEGORYINFO'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IEnumCATEGORYINFO));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: UINT; out rgelt: TCATEGORYINFO; out pceltFetched: UINT): HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: UINT): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out ppenum: IEnumCATEGORYINFO): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ ICatRegister import }
{---------------------}

function SepiImportICatRegister(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('ICatRegister'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(ICatRegister));

  with Result do
  begin
    AddMethod('RegisterCategories',
      'function(cCategories: UINT; rgCategoryInfo: PCATEGORYINFO): HResult', ccStdCall);
    AddMethod('UnRegisterCategories',
      'function(cCategories: UINT; rgcatid: Pointer): HResult', ccStdCall);
    AddMethod('RegisterClassImplCategories',
      'function(const rclsid: TGUID; cCategories: UINT; rgcatid: Pointer): HResult', ccStdCall);
    AddMethod('UnRegisterClassImplCategories',
      'function(const rclsid: TGUID; cCategories: UINT; rgcatid: Pointer): HResult', ccStdCall);
    AddMethod('RegisterClassReqCategories',
      'function(const rclsid: TGUID; cCategories: UINT; rgcatid: Pointer): HResult', ccStdCall);
    AddMethod('UnRegisterClassReqCategories',
      'function(const rclsid: TGUID; cCategories: UINT; rgcatid: Pointer): HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ ICatInformation import }
{------------------------}

function SepiImportICatInformation(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('ICatInformation'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(ICatInformation));

  with Result do
  begin
    AddMethod('EnumCategories',
      'function(lcid: UINT; out ppenumCategoryInfo: IEnumCATEGORYINFO): HResult', ccStdCall);
    AddMethod('GetCategoryDesc',
      'function(const rcatid: TGUID; lcid: UINT; out pszDesc: PWideChar): HResult', ccStdCall);
    AddMethod('EnumClassesOfCategories',
      'function(cImplemented: UINT; rgcatidImpl: Pointer; cRequired: UINT; rgcatidReq: Pointer; out ppenumClsid: IEnumGUID): HResult', ccStdCall);
    AddMethod('IsClassOfCategories',
      'function(const rclsid: TGUID; cImplemented: UINT; rgcatidImpl: Pointer; cRequired: UINT; rgcatidReq: Pointer): HResult', ccStdCall);
    AddMethod('EnumImplCategoriesOfClass',
      'function(var rclsid: TGUID; out ppenumCatid: IEnumGUID): HResult', ccStdCall);
    AddMethod('EnumReqCategoriesOfClass',
      'function(var rclsid: TGUID; out ppenumCatid: IEnumGUID): HResult', ccStdCall);

    Complete;
  end;
end;

{------------------}
{ IBindHost import }
{------------------}

function SepiImportIBindHost(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IBindHost));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ IOleUndoManager import }
{------------------------}

function SepiImportIOleUndoManager(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleUndoManager));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ tagQACONTAINER import }
{-----------------------}

function SepiImporttagQACONTAINER(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagQACONTAINER', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(LongInt));
    AddField('pClientSite', System.TypeInfo(IOleClientSite));
    AddField('pAdviseSink', System.TypeInfo(IAdviseSink));
    AddField('pPropertyNotifySink', System.TypeInfo(IPropertyNotifySink));
    AddField('pUnkEventSink', System.TypeInfo(IUnknown));
    AddField('dwAmbientFlags', System.TypeInfo(LongInt));
    AddField('colorFore', System.TypeInfo(OLE_COLOR));
    AddField('colorBack', System.TypeInfo(OLE_COLOR));
    AddField('pFont', System.TypeInfo(IFont));
    AddField('pUndoMgr', System.TypeInfo(IOleUndoManager));
    AddField('dwAppearance', System.TypeInfo(LongInt));
    AddField('lcid', System.TypeInfo(LongInt));
    AddField('hpal', System.TypeInfo(HPALETTE));
    AddField('pBindHost', System.TypeInfo(IBindHost));

    Complete;
  end;
end;

{---------------------}
{ tagQACONTROL import }
{---------------------}

function SepiImporttagQACONTROL(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagQACONTROL', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(LongInt));
    AddField('dwMiscStatus', System.TypeInfo(LongInt));
    AddField('dwViewStatus', System.TypeInfo(LongInt));
    AddField('dwEventCookie', System.TypeInfo(LongInt));
    AddField('dwPropNotifyCookie', System.TypeInfo(LongInt));
    AddField('dwPointerActivationPolicy', System.TypeInfo(LongInt));

    Complete;
  end;
end;

{-----------------------}
{ IQuickActivate import }
{-----------------------}

function SepiImportIQuickActivate(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IQuickActivate));

  with Result do
  begin
    AddMethod('QuickActivate',
      'function(var qaCont: tagQACONTAINER; var qaCtrl: tagQACONTROL): HResult', ccStdCall);
    AddMethod('SetContentExtent',
      'function(const sizel: TPoint): HResult', ccStdCall);
    AddMethod('GetContentExtent',
      'function(out sizel: TPoint): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------}
{ IObjectSafety import }
{----------------------}

function SepiImportIObjectSafety(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IObjectSafety));

  with Result do
  begin
    AddMethod('GetInterfaceSafetyOptions',
      'function(const IID: TIID; pdwSupportedOptions, pdwEnabledOptions : PDWORD ) : HResult', ccStdCall);
    AddMethod('SetInterfaceSafetyOptions',
      'function(const IID: TIID; dwOptionSetMask, dwEnabledOptions : DWORD ) : HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ IDispatchEx import }
{--------------------}

function SepiImportIDispatchEx(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDispatchEx));

  with Result do
  begin
    AddMethod('GetDispID',
      'function(const bstrName: TBSTR; const grfdex: DWORD; out id: TDispID ) : HResult', ccStdCall);
    AddMethod('InvokeEx',
      'function(const id: TDispID; const lcid: LCID; const wflags:  WORD  ; const pdp: PDispParams ; out varRes: OleVariant ; out pei:  TExcepInfo  ; const pspCaller: PServiceProvider ) : HResult', ccStdCall);
    AddMethod('DeleteMemberByName',
      'function(const bstr: TBSTR; const grfdex: DWORD ) : HResult', ccStdCall);
    AddMethod('DeleteMemberByDispID',
      'function(const id: TDispID): HResult', ccStdCall);
    AddMethod('GetMemberProperties',
      'function(const id: TDispID; const grfdexFetch:  DWORD  ; out grfdex: DWORD ) : HResult', ccStdCall);
    AddMethod('GetMemberName',
      'function(const id: TDispID; out bstrName: TBSTR): HResult', ccStdCall);
    AddMethod('GetNextDispID',
      'function(const grfdex: DWORD; const id: TDispID; out nid: TDispID ) : HResult', ccStdCall);
    AddMethod('GetNameSpaceParent',
      'function(out unk: IUnknown): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ActiveXTypes',
    ['Messages', 'Windows']);

  // Types
  TSepiTypeAlias.Create(Result, 'PROPID', TypeInfo(ULONG));
  TSepiPointerType.Create(Result, 'PPropID', TypeInfo(TPropID), True);
  TSepiTypeAlias.Create(Result, 'TPropID', 'PROPID');

  // Types
  TSepiTypeAlias.Create(Result, 'BORDERWIDTHS', 'TRect');
  TSepiPointerType.Create(Result, 'PBorderWidths', 'TBorderWidths', True);
  TSepiTypeAlias.Create(Result, 'TBorderWidths', 'BORDERWIDTHS');

  // Types
  TSepiPointerType.Create(Result, 'PHResult', TypeInfo(HResult), True);
  TSepiPointerType.Create(Result, 'PSCODE', TypeInfo(Integer), True);
  TSepiTypeAlias.Create(Result, 'SCODE', TypeInfo(Integer));
  TSepiPointerType.Create(Result, 'PSYSINT', TypeInfo(SYSINT), True);
  TSepiTypeAlias.Create(Result, 'SYSINT', TypeInfo(Integer));
  TSepiPointerType.Create(Result, 'PSYSUINT', TypeInfo(SYSUINT), True);
  TSepiTypeAlias.Create(Result, 'SYSUINT', TypeInfo(LongWord));
  TSepiPointerType.Create(Result, 'PResultList', 'TResultList', True);
  TSepiArrayType.Create(Result, 'TResultList',
    [0, 65535], TypeInfo(HRESULT), True);
  TSepiPointerType.Create(Result, 'PUnknownList', TypeInfo(TUnknownList), True);
  TSepiArrayType.Create(Result, 'TUnknownList',
    [0, 65535], TypeInfo(IUnknown), True, TypeInfo(TUnknownList));
  TSepiTypeAlias.Create(Result, 'TOleChar', TypeInfo(WideChar));
  TSepiTypeAlias.Create(Result, 'POleStr', 'PWideChar');
  TSepiPointerType.Create(Result, 'PPOleStr', 'POleStr', True);
  TSepiPointerType.Create(Result, 'POleStrList', 'TOleStrList', True);
  TSepiArrayType.Create(Result, 'TOleStrList',
    [0, 65535], 'POleStr', True);
  TSepiTypeAlias.Create(Result, 'Largeint', TypeInfo(Int64));
  TSepiPointerType.Create(Result, 'PLargeuint', TypeInfo(Largeuint), True);
  TSepiTypeAlias.Create(Result, 'Largeuint', TypeInfo(Int64));
  TSepiTypeAlias.Create(Result, 'PIID', 'PGUID');
  TSepiTypeAlias.Create(Result, 'TIID', 'TGUID');
  TSepiTypeAlias.Create(Result, 'PCLSID', 'PGUID');
  TSepiTypeAlias.Create(Result, 'TCLSID', 'TGUID');
  TSepiPointerType.Create(Result, 'PObjectID', 'TObjectID', True);
  SepiImport_OBJECTID(Result);
  TSepiTypeAlias.Create(Result, 'TObjectID', '_OBJECTID');
  TSepiTypeAlias.Create(Result, 'OBJECTID', 'TObjectID');
  TSepiTypeAlias.Create(Result, 'TLCID', TypeInfo(DWORD));
  TSepiTypeAlias.Create(Result, 'FMTID', 'TGUID');
  TSepiPointerType.Create(Result, 'PFmtID', 'TFmtID', True);
  TSepiTypeAlias.Create(Result, 'TFmtID', 'TGUID');
  TSepiTypeAlias.Create(Result, 'PTextMetricOle', 'PTextMetricW');
  TSepiTypeAlias.Create(Result, 'TTextMetricOle', 'TTextMetricW');
  TSepiTypeAlias.Create(Result, 'OLE_COLOR', TypeInfo(DWORD));
  TSepiTypeAlias.Create(Result, 'TOleColor', TypeInfo(OLE_COLOR));
  TSepiPointerType.Create(Result, 'PCoServerInfo', 'TCoServerInfo', True);
  SepiImport_COSERVERINFO(Result);
  TSepiTypeAlias.Create(Result, 'TCoServerInfo', '_COSERVERINFO');
  TSepiTypeAlias.Create(Result, 'COSERVERINFO', 'TCoServerInfo');
  TSepiPointerType.Create(Result, 'PMultiQI', TypeInfo(TMultiQI), True);
  SepiImporttagMULTI_QI(Result);
  TSepiTypeAlias.Create(Result, 'TMultiQI', TypeInfo(tagMULTI_QI));
  TSepiTypeAlias.Create(Result, 'MULTI_QI', TypeInfo(TMultiQI));
  TSepiPointerType.Create(Result, 'PMultiQIArray', TypeInfo(TMultiQIArray), True);
  TSepiArrayType.Create(Result, 'TMultiQIArray',
    [0, 65535], TypeInfo(TMultiQI), True, TypeInfo(TMultiQIArray));
  TSepiPointerType.Create(Result, 'PSafeArrayBound', 'TSafeArrayBound', True);
  SepiImporttagSAFEARRAYBOUND(Result);
  TSepiTypeAlias.Create(Result, 'TSafeArrayBound', 'tagSAFEARRAYBOUND');
  TSepiTypeAlias.Create(Result, 'SAFEARRAYBOUND', 'TSafeArrayBound');
  TSepiPointerType.Create(Result, 'PSafeArray', 'TSafeArray', True);
  TSepiArrayType.Create(Result, '$1',
    [0, 0], 'TSafeArrayBound', True);
  SepiImporttagSAFEARRAY(Result);
  TSepiTypeAlias.Create(Result, 'TSafeArray', 'tagSAFEARRAY');
  TSepiTypeAlias.Create(Result, 'SAFEARRAY', 'TSafeArray');
  TSepiTypeAlias.Create(Result, 'TOleDate', TypeInfo(Double));
  TSepiPointerType.Create(Result, 'POleDate', TypeInfo(TOleDate), True);
  TSepiTypeAlias.Create(Result, 'TOleBool', TypeInfo(WordBool));
  TSepiPointerType.Create(Result, 'POleBool', TypeInfo(TOleBool), True);
  TSepiTypeAlias.Create(Result, 'TVarType', TypeInfo(Word));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOldOleEnum));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOleEnum));
  TSepiTypeAlias.Create(Result, 'OLE_XPOS_PIXELS', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'OLE_YPOS_PIXELS', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'OLE_XSIZE_PIXELS', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'OLE_YSIZE_PIXELS', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'OLE_XPOS_HIMETRIC', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'OLE_YPOS_HIMETRIC', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'OLE_XSIZE_HIMETRIC', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'OLE_YSIZE_HIMETRIC', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'OLE_XPOS_CONTAINER', TypeInfo(Single));
  TSepiTypeAlias.Create(Result, 'OLE_YPOS_CONTAINER', TypeInfo(Single));
  TSepiTypeAlias.Create(Result, 'OLE_XSIZE_CONTAINER', TypeInfo(Single));
  TSepiTypeAlias.Create(Result, 'OLE_YSIZE_CONTAINER', TypeInfo(Single));
  TSepiTypeAlias.Create(Result, 'OLE_TRISTATE', TypeInfo(SmallInt));

  // Types
  TSepiTypeAlias.Create(Result, 'OLE_OPTEXCLUSIVE', TypeInfo(WordBool));
  TSepiTypeAlias.Create(Result, 'OLE_CANCELBOOL', TypeInfo(WordBool));
  TSepiTypeAlias.Create(Result, 'OLE_ENABLEDEFAULTBOOL', TypeInfo(WordBool));
  TSepiTypeAlias.Create(Result, 'OLE_HANDLE', TypeInfo(LongWord));
  TSepiTypeAlias.Create(Result, 'FONTNAME', TypeInfo(WideString));
  TSepiTypeAlias.Create(Result, 'FONTSIZE', TypeInfo(Currency));
  TSepiTypeAlias.Create(Result, 'FONTBOLD', TypeInfo(WordBool));
  TSepiTypeAlias.Create(Result, 'FONTITALIC', TypeInfo(WordBool));
  TSepiTypeAlias.Create(Result, 'FONTUNDERSCORE', TypeInfo(WordBool));
  TSepiTypeAlias.Create(Result, 'FONTSTRIKETHROUGH', TypeInfo(WordBool));
  TSepiMethodRefType.Create(Result, 'TDLLRegisterServer',
    'function: HResult', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TDLLUnregisterServer',
    'function: HResult', False, ccStdCall);
  TSepiPointerType.Create(Result, 'PPointF', 'TPointF', True);
  SepiImporttagPOINTF(Result);
  TSepiTypeAlias.Create(Result, 'TPointF', 'tagPOINTF');
  TSepiTypeAlias.Create(Result, 'POINTF', 'TPointF');
  TSepiPointerType.Create(Result, 'PControlInfo', 'TControlInfo', True);
  SepiImporttagCONTROLINFO(Result);
  TSepiTypeAlias.Create(Result, 'TControlInfo', 'tagCONTROLINFO');
  TSepiTypeAlias.Create(Result, 'CONTROLINFO', 'TControlInfo');
  TSepiInterface.ForwardDecl(Result, TypeInfo(IStream));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IRunningObjectTable));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IEnumString));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IMoniker));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IAdviseSink));
  TSepiInterface.ForwardDecl(Result, TypeInfo(ITypeInfo));
  TSepiInterface.ForwardDecl(Result, TypeInfo(ITypeInfo2));
  TSepiInterface.ForwardDecl(Result, TypeInfo(ITypeComp));
  TSepiInterface.ForwardDecl(Result, TypeInfo(ITypeLib));
  TSepiInterface.ForwardDecl(Result, TypeInfo(ITypeLib2));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IEnumOLEVERB));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IOleInPlaceActiveObject));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IOleControl));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IOleControlSite));
  TSepiInterface.ForwardDecl(Result, TypeInfo(ISimpleFrameSite));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IPersistStreamInit));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IPersistPropertyBag));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IPropertyNotifySink));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IProvideClassInfo));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IConnectionPointContainer));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IEnumConnectionPoints));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IConnectionPoint));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IEnumConnections));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IClassFactory2));
  TSepiInterface.ForwardDecl(Result, TypeInfo(ISpecifyPropertyPages));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IPerPropertyBrowsing));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IPropertyPageSite));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IPropertyPage));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IPropertyPage2));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IPropertySetStorage));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IPropertyStorage));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IEnumSTATPROPSTG));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IEnumSTATPROPSETSTG));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IGlobalInterfaceTable));
  SepiImportIClassFactory(Result);
  SepiImportIMarshal(Result);
  SepiImportIMalloc(Result);
  SepiImportIMallocSpy(Result);
  SepiImportIStdMarshalInfo(Result);
  SepiImportIExternalConnection(Result);
  SepiImportIWeakRef(Result);
  SepiImportIEnumUnknown(Result);
  TSepiPointerType.Create(Result, 'PBindOpts', 'TBindOpts', True);
  SepiImporttagBIND_OPTS(Result);
  TSepiTypeAlias.Create(Result, 'TBindOpts', 'tagBIND_OPTS');
  TSepiTypeAlias.Create(Result, 'BIND_OPTS', 'TBindOpts');
  SepiImportIBindCtx(Result);
  SepiImportIEnumMoniker(Result);
  SepiImportIRunnableObject(Result);
  SepiImportIRunningObjectTable(Result);
  SepiImportIPersist(Result);
  SepiImportIPersistStream(Result);
  TSepiPointerType.Create(Result, 'PIMoniker', TypeInfo(IMoniker), True);
  SepiImportIMoniker(Result);
  SepiImportIEnumString(Result);
  TSepiPointerType.Create(Result, 'PStatStg', 'TStatStg', True);
  SepiImporttagSTATSTG(Result);
  TSepiTypeAlias.Create(Result, 'TStatStg', 'tagSTATSTG');
  TSepiTypeAlias.Create(Result, 'STATSTG', 'TStatStg');
  SepiImportISequentialStream(Result);
  SepiImportIStream(Result);
  SepiImportIEnumStatStg(Result);
  TSepiPointerType.Create(Result, 'TSNB', 'POleStr', True);
  SepiImportIStorage(Result);
  SepiImportIPersistFile(Result);
  SepiImportIPersistStorage(Result);
  SepiImportILockBytes(Result);
  TSepiPointerType.Create(Result, 'PDVTargetDevice', 'TDVTargetDevice', True);
  SepiImport_2(Result);
  SepiImporttagDVTARGETDEVICE(Result);
  TSepiTypeAlias.Create(Result, 'TDVTargetDevice', 'tagDVTARGETDEVICE');
  TSepiTypeAlias.Create(Result, 'DVTARGETDEVICE', 'TDVTargetDevice');
  TSepiPointerType.Create(Result, 'PClipFormat', TypeInfo(TClipFormat), True);
  TSepiTypeAlias.Create(Result, 'TClipFormat', TypeInfo(Word));
  TSepiPointerType.Create(Result, 'PFormatEtc', 'TFormatEtc', True);
  SepiImporttagFORMATETC(Result);
  TSepiTypeAlias.Create(Result, 'TFormatEtc', 'tagFORMATETC');
  TSepiTypeAlias.Create(Result, 'FORMATETC', 'TFormatEtc');
  SepiImportIEnumFORMATETC(Result);
  TSepiPointerType.Create(Result, 'PStatData', TypeInfo(TStatData), True);
  SepiImporttagSTATDATA(Result);
  TSepiTypeAlias.Create(Result, 'TStatData', TypeInfo(tagSTATDATA));
  TSepiTypeAlias.Create(Result, 'STATDATA', TypeInfo(TStatData));
  SepiImportIEnumSTATDATA(Result);
  SepiImportIRootStorage(Result);
  TSepiPointerType.Create(Result, 'PRemStgMedium', 'TRemStgMedium', True);
  SepiImport_3(Result);
  SepiImporttagRemSTGMEDIUM(Result);
  TSepiTypeAlias.Create(Result, 'TRemStgMedium', 'tagRemSTGMEDIUM');
  TSepiTypeAlias.Create(Result, 'RemSTGMEDIUM', 'TRemStgMedium');
  TSepiPointerType.Create(Result, 'PStgMedium', 'TStgMedium', True);
  SepiImporttagSTGMEDIUM(Result);
  TSepiTypeAlias.Create(Result, 'TStgMedium', 'tagSTGMEDIUM');
  TSepiTypeAlias.Create(Result, 'STGMEDIUM', 'TStgMedium');
  SepiImportIAdviseSink(Result);
  SepiImportIAdviseSink2(Result);
  SepiImportIDataObject(Result);
  SepiImportIDataAdviseHolder(Result);
  TSepiPointerType.Create(Result, 'PInterfaceInfo', TypeInfo(TInterfaceInfo), True);
  SepiImporttagINTERFACEINFO(Result);
  TSepiTypeAlias.Create(Result, 'TInterfaceInfo', TypeInfo(tagINTERFACEINFO));
  TSepiTypeAlias.Create(Result, 'INTERFACEINFO', TypeInfo(TInterfaceInfo));
  SepiImportIMessageFilter(Result);
  TSepiTypeAlias.Create(Result, 'TRpcOleDataRep', TypeInfo(DWORD));
  TSepiPointerType.Create(Result, 'PRpcOleMessage', 'TRpcOleMessage', True);
  TSepiArrayType.Create(Result, '$4',
    [0, 4], 'Pointer', True);
  SepiImporttagRPCOLEMESSAGE(Result);
  TSepiTypeAlias.Create(Result, 'TRpcOleMessage', 'tagRPCOLEMESSAGE');
  TSepiTypeAlias.Create(Result, 'RPCOLEMESSAGE', 'TRpcOleMessage');
  SepiImportIRpcChannelBuffer(Result);
  SepiImportIRpcProxyBuffer(Result);
  SepiImportIRpcStubBuffer(Result);
  SepiImportIPSFactoryBuffer(Result);
  SepiImportIChannelHook(Result);
  SepiImportIFillLockBytes(Result);
  TSepiPointerType.Create(Result, 'PBStr', 'TBStr', True);
  TSepiTypeAlias.Create(Result, 'TBStr', 'POleStr');
  TSepiPointerType.Create(Result, 'PBStrList', 'TBStrList', True);
  TSepiArrayType.Create(Result, 'TBStrList',
    [0, 65535], 'TBStr', True);
  TSepiPointerType.Create(Result, 'PComp', TypeInfo(Comp), True);
  TSepiPointerType.Create(Result, 'PDecimal', 'TDecimal', True);
  SepiImporttagDEC(Result);
  TSepiTypeAlias.Create(Result, 'TDecimal', 'tagDEC');
  TSepiTypeAlias.Create(Result, 'DECIMAL', 'TDecimal');
  TSepiPointerType.Create(Result, 'PBlob', 'TBlob', True);
  SepiImporttagBLOB(Result);
  TSepiTypeAlias.Create(Result, 'TBlob', 'tagBLOB');
  TSepiTypeAlias.Create(Result, 'BLOB', 'TBlob');
  TSepiPointerType.Create(Result, 'PClipData', 'TClipData', True);
  SepiImporttagCLIPDATA(Result);
  TSepiTypeAlias.Create(Result, 'TClipData', 'tagCLIPDATA');
  TSepiTypeAlias.Create(Result, 'CLIPDATA', 'TClipData');
  TSepiPointerType.Create(Result, 'PPropVariant', 'TPropVariant', True);
  SepiImporttagCAUB(Result);
  TSepiTypeAlias.Create(Result, 'CAUB', 'tagCAUB');
  TSepiPointerType.Create(Result, 'PCAUB', 'TCAUB', True);
  TSepiTypeAlias.Create(Result, 'TCAUB', 'tagCAUB');
  SepiImporttagCAI(Result);
  TSepiTypeAlias.Create(Result, 'CAI', 'tagCAI');
  TSepiPointerType.Create(Result, 'PCAI', 'TCAI', True);
  TSepiTypeAlias.Create(Result, 'TCAI', 'tagCAI');
  SepiImporttagCAUI(Result);
  TSepiTypeAlias.Create(Result, 'CAUI', 'tagCAUI');
  TSepiPointerType.Create(Result, 'PCAUI', 'TCAUI', True);
  TSepiTypeAlias.Create(Result, 'TCAUI', 'tagCAUI');
  SepiImporttagCAL(Result);
  TSepiTypeAlias.Create(Result, 'CAL', 'tagCAL');
  TSepiPointerType.Create(Result, 'PCAL', 'TCAL', True);
  TSepiTypeAlias.Create(Result, 'TCAL', 'tagCAL');
  SepiImporttagCAUL(Result);
  TSepiTypeAlias.Create(Result, 'CAUL', 'tagCAUL');
  TSepiPointerType.Create(Result, 'PCAUL', 'TCAUL', True);
  TSepiTypeAlias.Create(Result, 'TCAUL', 'tagCAUL');
  SepiImporttagCAFLT(Result);
  TSepiTypeAlias.Create(Result, 'CAFLT', 'tagCAFLT');
  TSepiPointerType.Create(Result, 'PCAFLT', 'TCAFLT', True);
  TSepiTypeAlias.Create(Result, 'TCAFLT', 'tagCAFLT');
  SepiImporttagCADBL(Result);
  TSepiTypeAlias.Create(Result, 'CADBL', 'tagCADBL');
  TSepiPointerType.Create(Result, 'PCADBL', 'TCADBL', True);
  TSepiTypeAlias.Create(Result, 'TCADBL', 'tagCADBL');
  SepiImporttagCACY(Result);
  TSepiTypeAlias.Create(Result, 'CACY', 'tagCACY');
  TSepiPointerType.Create(Result, 'PCACY', 'TCACY', True);
  TSepiTypeAlias.Create(Result, 'TCACY', 'tagCACY');
  SepiImporttagCADATE(Result);
  TSepiTypeAlias.Create(Result, 'CADATE', 'tagCADATE');
  TSepiPointerType.Create(Result, 'PCADATE', 'TCADATE', True);
  TSepiTypeAlias.Create(Result, 'TCADATE', 'tagCADATE');
  SepiImporttagCABSTR(Result);
  TSepiTypeAlias.Create(Result, 'CABSTR', 'tagCABSTR');
  TSepiPointerType.Create(Result, 'PCABSTR', 'TCABSTR', True);
  TSepiTypeAlias.Create(Result, 'TCABSTR', 'tagCABSTR');
  SepiImporttagCABOOL(Result);
  TSepiTypeAlias.Create(Result, 'CABOOL', 'tagCABOOL');
  TSepiPointerType.Create(Result, 'PCABOOL', 'TCABOOL', True);
  TSepiTypeAlias.Create(Result, 'TCABOOL', 'tagCABOOL');
  SepiImporttagCASCODE(Result);
  TSepiTypeAlias.Create(Result, 'CASCODE', 'tagCASCODE');
  TSepiPointerType.Create(Result, 'PCASCODE', 'TCASCODE', True);
  TSepiTypeAlias.Create(Result, 'TCASCODE', 'tagCASCODE');
  SepiImporttagCAPROPVARIANT(Result);
  TSepiTypeAlias.Create(Result, 'CAPROPVARIANT', 'tagCAPROPVARIANT');
  TSepiPointerType.Create(Result, 'PCAPROPVARIANT', 'TCAPROPVARIANT', True);
  TSepiTypeAlias.Create(Result, 'TCAPROPVARIANT', 'tagCAPROPVARIANT');
  SepiImporttagCAH(Result);
  TSepiTypeAlias.Create(Result, 'CAH', 'tagCAH');
  TSepiPointerType.Create(Result, 'PCAH', 'TCAH', True);
  TSepiTypeAlias.Create(Result, 'TCAH', 'tagCAH');
  SepiImporttagCAUH(Result);
  TSepiTypeAlias.Create(Result, 'CAUH', 'tagCAUH');
  TSepiPointerType.Create(Result, 'PCAUH', 'TCAUH', True);
  TSepiTypeAlias.Create(Result, 'TCAUH', 'tagCAUH');
  SepiImporttagCALPSTR(Result);
  TSepiTypeAlias.Create(Result, 'CALPSTR', 'tagCALPSTR');
  TSepiPointerType.Create(Result, 'PCALPSTR', 'TCALPSTR', True);
  TSepiTypeAlias.Create(Result, 'TCALPSTR', 'tagCALPSTR');
  SepiImporttagCALPWSTR(Result);
  TSepiTypeAlias.Create(Result, 'CALPWSTR', 'tagCALPWSTR');
  TSepiPointerType.Create(Result, 'PCALPWSTR', 'TCALPWSTR', True);
  TSepiTypeAlias.Create(Result, 'TCALPWSTR', 'tagCALPWSTR');
  SepiImporttagCAFILETIME(Result);
  TSepiTypeAlias.Create(Result, 'CAFILETIME', 'tagCAFILETIME');
  TSepiPointerType.Create(Result, 'PCAFILETIME', 'TCAFILETIME', True);
  TSepiTypeAlias.Create(Result, 'TCAFILETIME', 'tagCAFILETIME');
  SepiImporttagCACLIPDATA(Result);
  TSepiTypeAlias.Create(Result, 'CACLIPDATA', 'tagCACLIPDATA');
  TSepiPointerType.Create(Result, 'PCACLIPDATA', 'TCACLIPDATA', True);
  TSepiTypeAlias.Create(Result, 'TCACLIPDATA', 'tagCACLIPDATA');
  SepiImporttagCACLSID(Result);
  TSepiTypeAlias.Create(Result, 'CACLSID', 'tagCACLSID');
  TSepiPointerType.Create(Result, 'PCACLSID', 'TCACLSID', True);
  TSepiTypeAlias.Create(Result, 'TCACLSID', 'tagCACLSID');
  SepiImporttagPROPVARIANT(Result);
  TSepiTypeAlias.Create(Result, 'PROPVARIANT', 'tagPROPVARIANT');
  TSepiTypeAlias.Create(Result, 'TPropVariant', 'tagPROPVARIANT');
  SepiImporttagPROPSPEC(Result);
  TSepiTypeAlias.Create(Result, 'PROPSPEC', 'tagPROPSPEC');
  TSepiPointerType.Create(Result, 'PPropSpec', 'TPropSpec', True);
  TSepiTypeAlias.Create(Result, 'TPropSpec', 'tagPROPSPEC');
  SepiImporttagSTATPROPSTG(Result);
  TSepiTypeAlias.Create(Result, 'STATPROPSTG', 'tagSTATPROPSTG');
  TSepiPointerType.Create(Result, 'PStatPropStg', 'TStatPropStg', True);
  TSepiTypeAlias.Create(Result, 'TStatPropStg', 'tagSTATPROPSTG');
  SepiImporttagSTATPROPSETSTG(Result);
  TSepiTypeAlias.Create(Result, 'STATPROPSETSTG', 'tagSTATPROPSETSTG');
  TSepiPointerType.Create(Result, 'PStatPropSetStg', 'TStatPropSetStg', True);
  TSepiTypeAlias.Create(Result, 'TStatPropSetStg', 'tagSTATPROPSETSTG');
  SepiImportIPropertyStorage(Result);
  SepiImportIPropertySetStorage(Result);
  SepiImportIEnumSTATPROPSTG(Result);
  SepiImportIEnumSTATPROPSETSTG(Result);
  SepiImportIGlobalInterfaceTable(Result);
  TSepiPointerType.Create(Result, 'PVariantArg', 'TVariantArg', True);
  TSepiPointerType.Create(Result, '$5', TypeInfo(Byte), True);
  TSepiPointerType.Create(Result, '$6', TypeInfo(Smallint), True);
  TSepiPointerType.Create(Result, '$7', TypeInfo(Longint), True);
  TSepiPointerType.Create(Result, '$8', TypeInfo(Single), True);
  TSepiPointerType.Create(Result, '$9', TypeInfo(Double), True);
  TSepiPointerType.Create(Result, '$10', TypeInfo(TOleBool), True);
  TSepiPointerType.Create(Result, '$11', TypeInfo(HResult), True);
  TSepiPointerType.Create(Result, '$12', TypeInfo(Currency), True);
  TSepiPointerType.Create(Result, '$13', TypeInfo(TOleDate), True);
  TSepiPointerType.Create(Result, '$14', TypeInfo(WideString), True);
  TSepiPointerType.Create(Result, '$15', TypeInfo(IUnknown), True);
  TSepiPointerType.Create(Result, '$16', TypeInfo(IDispatch), True);
  TSepiPointerType.Create(Result, '$17', 'PSafeArray', True);
  SepiImporttagVARIANT(Result);
  TSepiTypeAlias.Create(Result, 'TVariantArg', 'tagVARIANT');
  TSepiPointerType.Create(Result, 'PVariantArgList', 'TVariantArgList', True);
  TSepiArrayType.Create(Result, 'TVariantArgList',
    [0, 65535], 'TVariantArg', True);
  TSepiTypeAlias.Create(Result, 'TDispID', TypeInfo(Longint));
  TSepiPointerType.Create(Result, 'PDispIDList', 'TDispIDList', True);
  TSepiArrayType.Create(Result, 'TDispIDList',
    [0, 65535], TypeInfo(TDispID), True);
  TSepiTypeAlias.Create(Result, 'TMemberID', TypeInfo(TDispID));
  TSepiPointerType.Create(Result, 'PMemberIDList', 'TMemberIDList', True);
  TSepiArrayType.Create(Result, 'TMemberIDList',
    [0, 65535], TypeInfo(TMemberID), True);
  TSepiTypeAlias.Create(Result, 'HRefType', TypeInfo(DWORD));
  TSepiTypeAlias.Create(Result, 'tagTYPEKIND', TypeInfo(DWORD));
  TSepiTypeAlias.Create(Result, 'TTypeKind', 'tagTYPEKIND');
  TSepiPointerType.Create(Result, 'PArrayDesc', 'TArrayDesc', True);
  TSepiPointerType.Create(Result, 'PTypeDesc', 'TTypeDesc', True);
  SepiImporttagTYPEDESC(Result);
  TSepiTypeAlias.Create(Result, 'TTypeDesc', 'tagTYPEDESC');
  TSepiTypeAlias.Create(Result, 'TYPEDESC', 'TTypeDesc');
  TSepiArrayType.Create(Result, '$18',
    [0, 0], 'TSafeArrayBound', True);
  SepiImporttagARRAYDESC(Result);
  TSepiTypeAlias.Create(Result, 'TArrayDesc', 'tagARRAYDESC');
  TSepiTypeAlias.Create(Result, 'ARRAYDESC', 'TArrayDesc');
  TSepiPointerType.Create(Result, 'PIDLDesc', 'TIDLDesc', True);
  SepiImporttagIDLDESC(Result);
  TSepiTypeAlias.Create(Result, 'TIDLDesc', 'tagIDLDESC');
  TSepiTypeAlias.Create(Result, 'IDLDESC', 'TIDLDesc');
  TSepiPointerType.Create(Result, 'PParamDescEx', 'TParamDescEx', True);
  SepiImporttagPARAMDESCEX(Result);
  TSepiTypeAlias.Create(Result, 'TParamDescEx', 'tagPARAMDESCEX');
  TSepiTypeAlias.Create(Result, 'PARAMDESCEX', 'TParamDescEx');
  TSepiPointerType.Create(Result, 'PParamDesc', 'TParamDesc', True);
  SepiImporttagPARAMDESC(Result);
  TSepiTypeAlias.Create(Result, 'TParamDesc', 'tagPARAMDESC');
  TSepiTypeAlias.Create(Result, 'PARAMDESC', 'TParamDesc');
  TSepiPointerType.Create(Result, 'PElemDesc', 'TElemDesc', True);
  SepiImporttagELEMDESC(Result);
  TSepiTypeAlias.Create(Result, 'TElemDesc', 'tagELEMDESC');
  TSepiTypeAlias.Create(Result, 'ELEMDESC', 'TElemDesc');
  TSepiPointerType.Create(Result, 'PElemDescList', 'TElemDescList', True);
  TSepiArrayType.Create(Result, 'TElemDescList',
    [0, 65535], 'TElemDesc', True);
  TSepiPointerType.Create(Result, 'PTypeAttr', 'TTypeAttr', True);
  SepiImporttagTYPEATTR(Result);
  TSepiTypeAlias.Create(Result, 'TTypeAttr', 'tagTYPEATTR');
  TSepiTypeAlias.Create(Result, 'TYPEATTR', 'TTypeAttr');
  TSepiPointerType.Create(Result, 'PDispParams', 'TDispParams', True);
  SepiImporttagDISPPARAMS(Result);
  TSepiTypeAlias.Create(Result, 'TDispParams', 'tagDISPPARAMS');
  TSepiTypeAlias.Create(Result, 'DISPPARAMS', 'TDispParams');
  TSepiPointerType.Create(Result, 'PExcepInfo', TypeInfo(TExcepInfo), True);
  TSepiMethodRefType.Create(Result, 'TFNDeferredFillIn',
    'function(ExInfo: PExcepInfo): HResult', False, ccStdCall);
  SepiImporttagEXCEPINFO(Result);
  TSepiTypeAlias.Create(Result, 'TExcepInfo', TypeInfo(tagEXCEPINFO));
  TSepiTypeAlias.Create(Result, 'EXCEPINFO', TypeInfo(TExcepInfo));
  TSepiTypeAlias.Create(Result, 'tagFUNCKIND', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'TFuncKind', 'tagFUNCKIND');
  TSepiTypeAlias.Create(Result, 'tagINVOKEKIND', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'TInvokeKind', 'tagINVOKEKIND');
  TSepiTypeAlias.Create(Result, 'tagCALLCONV', TypeInfo(Longint));
  TSepiTypeAlias.Create(Result, 'TCallConv', 'tagCALLCONV');
  TSepiPointerType.Create(Result, 'PFuncDesc', 'TFuncDesc', True);
  SepiImporttagFUNCDESC(Result);
  TSepiTypeAlias.Create(Result, 'TFuncDesc', 'tagFUNCDESC');
  TSepiTypeAlias.Create(Result, 'FUNCDESC', 'TFuncDesc');
  TSepiTypeAlias.Create(Result, 'TVarKind', TypeInfo(Longint));
  TSepiPointerType.Create(Result, 'PVarDesc', 'TVarDesc', True);
  SepiImporttagVARDESC(Result);
  TSepiTypeAlias.Create(Result, 'TVarDesc', 'tagVARDESC');
  TSepiTypeAlias.Create(Result, 'VARDESC', 'TVarDesc');
  SepiImportICreateTypeInfo(Result);
  SepiImportICreateTypeInfo2(Result);
  SepiImportICreateTypeLib(Result);
  SepiImportICreateTypeLib2(Result);
  SepiImportIEnumVariant(Result);
  TSepiTypeAlias.Create(Result, 'TDescKind', TypeInfo(Longint));
  TSepiPointerType.Create(Result, 'PBindPtr', 'TBindPtr', True);
  SepiImporttagBINDPTR(Result);
  TSepiTypeAlias.Create(Result, 'TBindPtr', 'tagBINDPTR');
  TSepiTypeAlias.Create(Result, 'BINDPTR', 'TBindPtr');
  SepiImportITypeComp(Result);
  SepiImportITypeInfo(Result);
  TSepiTypeAlias.Create(Result, 'TSysKind', TypeInfo(Longint));
  TSepiPointerType.Create(Result, 'PTLibAttr', 'TTLibAttr', True);
  SepiImporttagTLIBATTR(Result);
  TSepiTypeAlias.Create(Result, 'TTLibAttr', 'tagTLIBATTR');
  TSepiTypeAlias.Create(Result, 'TLIBATTR', 'TTLibAttr');
  TSepiPointerType.Create(Result, 'PTypeInfoList', TypeInfo(TTypeInfoList), True);
  TSepiArrayType.Create(Result, 'TTypeInfoList',
    [0, 65535], TypeInfo(ITypeInfo), True, TypeInfo(TTypeInfoList));
  SepiImportITypeLib(Result);
  TSepiPointerType.Create(Result, 'PCustDataItem', 'TCustDataItem', True);
  SepiImporttagCUSTDATAITEM(Result);
  TSepiTypeAlias.Create(Result, 'TCustDataItem', 'tagCUSTDATAITEM');
  TSepiTypeAlias.Create(Result, 'CUSTDATAITEM', 'TCustDataItem');
  TSepiPointerType.Create(Result, 'PCustDataItemList', 'TCustDataItemList', True);
  TSepiArrayType.Create(Result, 'TCustDataItemList',
    [0, 65535], 'TCustDataItem', True);
  TSepiPointerType.Create(Result, 'PCustData', 'TCustData', True);
  SepiImporttagCUSTDATA(Result);
  TSepiTypeAlias.Create(Result, 'TCustData', 'tagCUSTDATA');
  TSepiTypeAlias.Create(Result, 'CUSTDATA', 'TCustData');
  SepiImportITypeLib2(Result);
  SepiImportITypeInfo2(Result);
  SepiImportIErrorInfo(Result);
  SepiImportICreateErrorInfo(Result);
  SepiImportISupportErrorInfo(Result);
  TSepiPointerType.Create(Result, 'PParamData', 'TParamData', True);
  SepiImporttagPARAMDATA(Result);
  TSepiTypeAlias.Create(Result, 'TParamData', 'tagPARAMDATA');
  TSepiTypeAlias.Create(Result, 'PARAMDATA', 'TParamData');
  TSepiPointerType.Create(Result, 'PParamDataList', 'TParamDataList', True);
  TSepiArrayType.Create(Result, 'TParamDataList',
    [0, 65535], 'TParamData', True);
  TSepiPointerType.Create(Result, 'PMethodData', 'TMethodData', True);
  SepiImporttagMETHODDATA(Result);
  TSepiTypeAlias.Create(Result, 'TMethodData', 'tagMETHODDATA');
  TSepiTypeAlias.Create(Result, 'METHODDATA', 'TMethodData');
  TSepiPointerType.Create(Result, 'PMethodDataList', 'TMethodDataList', True);
  TSepiArrayType.Create(Result, 'TMethodDataList',
    [0, 65535], 'TMethodData', True);
  TSepiPointerType.Create(Result, 'PInterfaceData', 'TInterfaceData', True);
  SepiImporttagINTERFACEDATA(Result);
  TSepiTypeAlias.Create(Result, 'TInterfaceData', 'tagINTERFACEDATA');
  TSepiTypeAlias.Create(Result, 'INTERFACEDATA', 'TInterfaceData');
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(tagREGKIND));
  TSepiTypeAlias.Create(Result, 'TRegKind', TypeInfo(tagREGKIND));
  SepiImportIOleAdviseHolder(Result);
  SepiImportIOleCache(Result);
  SepiImportIOleCache2(Result);
  SepiImportIOleCacheControl(Result);
  SepiImportIParseDisplayName(Result);
  SepiImportIOleContainer(Result);
  SepiImportIOleClientSite(Result);
  SepiImportIOleObject(Result);
  TSepiPointerType.Create(Result, 'PObjectDescriptor', 'TObjectDescriptor', True);
  SepiImporttagOBJECTDESCRIPTOR(Result);
  TSepiTypeAlias.Create(Result, 'TObjectDescriptor', 'tagOBJECTDESCRIPTOR');
  TSepiTypeAlias.Create(Result, 'OBJECTDESCRIPTOR', 'TObjectDescriptor');
  TSepiTypeAlias.Create(Result, 'PLinkSrcDescriptor', 'PObjectDescriptor');
  TSepiTypeAlias.Create(Result, 'TLinkSrcDescriptor', 'TObjectDescriptor');
  SepiImportIOleWindow(Result);
  SepiImportIOleLink(Result);
  SepiImportIOleItemContainer(Result);
  SepiImportIOleInPlaceUIWindow(Result);
  SepiImportIOleInPlaceActiveObject(Result);
  TSepiPointerType.Create(Result, 'POleInPlaceFrameInfo', 'TOleInPlaceFrameInfo', True);
  SepiImporttagOIFI(Result);
  TSepiTypeAlias.Create(Result, 'TOleInPlaceFrameInfo', 'tagOIFI');
  TSepiTypeAlias.Create(Result, 'OLEINPLACEFRAMEINFO', 'TOleInPlaceFrameInfo');
  TSepiPointerType.Create(Result, 'POleMenuGroupWidths', 'TOleMenuGroupWidths', True);
  TSepiArrayType.Create(Result, '$19',
    [0, 5], TypeInfo(Longint), True);
  SepiImporttagOleMenuGroupWidths(Result);
  TSepiTypeAlias.Create(Result, 'TOleMenuGroupWidths', 'tagOleMenuGroupWidths');
  TSepiTypeAlias.Create(Result, 'OLEMENUGROUPWIDTHS', 'TOleMenuGroupWidths');
  SepiImportIOleInPlaceFrame(Result);
  SepiImportIOleInPlaceObject(Result);
  SepiImportIOleInPlaceSite(Result);
  TSepiMethodRefType.Create(Result, 'TContinueFunc',
    'function(dwContinue: Longint): BOOL', False, ccStdCall);
  SepiImportIViewObject(Result);
  SepiImportIViewObject2(Result);
  SepiImportIDropSource(Result);
  SepiImportIDropTarget(Result);
  TSepiPointerType.Create(Result, 'POleVerb', 'TOleVerb', True);
  SepiImporttagOLEVERB(Result);
  TSepiTypeAlias.Create(Result, 'TOleVerb', 'tagOLEVERB');
  TSepiTypeAlias.Create(Result, 'OLEVERB', 'TOleVerb');
  SepiImportIEnumOLEVERB(Result);
  SepiImportIOleControl(Result);
  SepiImportIOleControlSite(Result);
  SepiImportISimpleFrameSite(Result);
  SepiImportIObjectWithSite(Result);
  SepiImportIErrorLog(Result);
  SepiImportIPropertyBag(Result);
  SepiImportIPersistPropertyBag(Result);
  SepiImportIPersistStreamInit(Result);
  SepiImportIPropertyNotifySink(Result);
  SepiImportIProvideClassInfo(Result);
  SepiImportIConnectionPointContainer(Result);
  SepiImportIEnumConnectionPoints(Result);
  SepiImportIConnectionPoint(Result);
  TSepiPointerType.Create(Result, 'PConnectData', TypeInfo(TConnectData), True);
  SepiImporttagCONNECTDATA(Result);
  TSepiTypeAlias.Create(Result, 'TConnectData', TypeInfo(tagCONNECTDATA));
  TSepiTypeAlias.Create(Result, 'CONNECTDATA', TypeInfo(TConnectData));
  SepiImportIEnumConnections(Result);
  TSepiPointerType.Create(Result, 'PLicInfo', 'TLicInfo', True);
  SepiImporttagLICINFO(Result);
  TSepiTypeAlias.Create(Result, 'TLicInfo', 'tagLICINFO');
  TSepiTypeAlias.Create(Result, 'LICINFO', 'TLicInfo');
  SepiImportIClassFactory2(Result);
  TSepiPointerType.Create(Result, 'PGUIDList', 'TGUIDList', True);
  TSepiArrayType.Create(Result, 'TGUIDList',
    [0, 65535], 'TGUID', True);
  TSepiPointerType.Create(Result, 'PCAGUID', 'TCAGUID', True);
  SepiImporttagCAUUID(Result);
  TSepiTypeAlias.Create(Result, 'TCAGUID', 'tagCAUUID');
  TSepiTypeAlias.Create(Result, 'CAUUID', 'TCAGUID');
  TSepiPointerType.Create(Result, 'PCAPOleStr', 'TCAPOleStr', True);
  SepiImporttagCALPOLESTR(Result);
  TSepiTypeAlias.Create(Result, 'CALPOLESTR', 'tagCALPOLESTR');
  TSepiTypeAlias.Create(Result, 'TCAPOleStr', 'tagCALPOLESTR');
  TSepiPointerType.Create(Result, 'PLongintList', 'TLongintList', True);
  TSepiArrayType.Create(Result, 'TLongintList',
    [0, 65535], TypeInfo(Longint), True);
  TSepiPointerType.Create(Result, 'PCALongint', 'TCALongint', True);
  SepiImporttagCADWORD(Result);
  TSepiTypeAlias.Create(Result, 'CADWORD', 'tagCADWORD');
  TSepiTypeAlias.Create(Result, 'TCALongint', 'tagCADWORD');
  SepiImportIOleInPlaceObjectWindowless(Result);
  SepiImportIOleInPlaceSiteEx(Result);
  SepiImportIOleInPlaceSiteWindowless(Result);
  TSepiPointerType.Create(Result, 'POCPFIParams', 'TOCPFIParams', True);
  SepiImporttagOCPFIPARAMS(Result);
  TSepiTypeAlias.Create(Result, 'TOCPFIParams', 'tagOCPFIPARAMS');
  TSepiTypeAlias.Create(Result, 'OCPFIPARAMS', 'TOCPFIParams');
  TSepiPointerType.Create(Result, 'PPropPageInfo', 'TPropPageInfo', True);
  SepiImporttagPROPPAGEINFO(Result);
  TSepiTypeAlias.Create(Result, 'TPropPageInfo', 'tagPROPPAGEINFO');
  TSepiTypeAlias.Create(Result, 'PROPPAGEINFO', 'TPropPageInfo');
  SepiImportISpecifyPropertyPages(Result);
  SepiImportIPerPropertyBrowsing(Result);
  SepiImportIPropertyPageSite(Result);
  SepiImportIPropertyPage(Result);
  SepiImportIPropertyPage2(Result);
  SepiImportIFont(Result);
  SepiImportIFontDisp(Result);
  TSepiTypeAlias.Create(Result, 'Font', TypeInfo(IFontDisp));
  TSepiPointerType.Create(Result, 'PSOleAuthenticationService', 'TSOleAuthenticationService', True);
  SepiImporttagSOLE_AUTHENTICATION_SERVICE(Result);
  TSepiTypeAlias.Create(Result, 'TSOleAuthenticationService', 'tagSOLE_AUTHENTICATION_SERVICE');
  TSepiTypeAlias.Create(Result, 'SOLE_AUTHENTICATION_SERVICE', 'TSOleAuthenticationService');
  TSepiPointerType.Create(Result, 'PFontDesc', 'TFontDesc', True);
  SepiImporttagFONTDESC(Result);
  TSepiTypeAlias.Create(Result, 'TFontDesc', 'tagFONTDESC');
  TSepiTypeAlias.Create(Result, 'FONTDESC', 'TFontDesc');
  SepiImportIPicture(Result);
  SepiImportIPictureDisp(Result);
  TSepiTypeAlias.Create(Result, 'Picture', TypeInfo(IPictureDisp));
  TSepiPointerType.Create(Result, 'PPictDesc', 'TPictDesc', True);
  SepiImporttagPICTDESC(Result);
  TSepiTypeAlias.Create(Result, 'TPictDesc', 'tagPICTDESC');
  TSepiTypeAlias.Create(Result, 'PICTDESC', 'TPictDesc');
  SepiImportIOleDocumentView(Result);
  SepiImportIEnumOleDocumentViews(Result);
  SepiImportIOleDocument(Result);
  SepiImportIOleDocumentSite(Result);
  SepiImportIContinueCallback(Result);
  SepiImportIServiceProvider(Result);
  TSepiPointerType.Create(Result, 'PServiceProvider', TypeInfo(IServiceProvider), True);

  // Types
  TSepiPointerType.Create(Result, 'PPageRange', 'TPageRange', True);
  SepiImporttagPAGERANGE(Result);
  TSepiTypeAlias.Create(Result, 'PAGERANGE', 'tagPAGERANGE');
  TSepiTypeAlias.Create(Result, 'TPageRange', 'tagPAGERANGE');
  TSepiPointerType.Create(Result, 'PPageSet', 'TPageSet', True);
  TSepiArrayType.Create(Result, '$20',
    [0, 0], 'TPageRange', True);
  SepiImporttagPAGESET(Result);
  TSepiTypeAlias.Create(Result, 'PAGESET', 'tagPAGESET');
  TSepiTypeAlias.Create(Result, 'TPageSet', 'tagPAGESET');
  SepiImportIPrint(Result);

  // Types
  TSepiPointerType.Create(Result, 'POleCmd', 'TOleCmd', True);
  SepiImport_tagOLECMD(Result);
  TSepiTypeAlias.Create(Result, 'OLECMD', '_tagOLECMD');
  TSepiTypeAlias.Create(Result, 'TOleCmd', '_tagOLECMD');
  TSepiPointerType.Create(Result, 'POleCmdText', 'TOleCmdText', True);
  TSepiArrayType.Create(Result, '$21',
    [0, 0], TypeInfo(WideChar), True);
  SepiImport_tagOLECMDTEXT(Result);
  TSepiTypeAlias.Create(Result, 'OLECMDTEXT', '_tagOLECMDTEXT');
  TSepiTypeAlias.Create(Result, 'TOleCmdText', '_tagOLECMDTEXT');
  SepiImportIOleCommandTarget(Result);

  // Types
  SepiImportIActiveDesigner(Result);

  // Types
  SepiImportIPersistTextStream(Result);
  SepiImportIProvideRuntimeText(Result);

  // Types
  TSepiInterface.ForwardDecl(Result, TypeInfo(IEnumGUID));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IEnumCATEGORYINFO));
  TSepiInterface.ForwardDecl(Result, TypeInfo(ICatRegister));
  TSepiInterface.ForwardDecl(Result, TypeInfo(ICatInformation));
  TSepiPointerType.Create(Result, 'PCATEGORYINFO', 'TCATEGORYINFO', True);
  TSepiArrayType.Create(Result, '$22',
    [0, 127], TypeInfo(WideChar), True);
  SepiImportTCATEGORYINFO(Result);
  SepiImportIEnumGUID(Result);
  SepiImportIEnumCATEGORYINFO(Result);
  SepiImportICatRegister(Result);
  SepiImportICatInformation(Result);
  SepiImportIBindHost(Result);
  SepiImportIOleUndoManager(Result);
  SepiImporttagQACONTAINER(Result);
  TSepiPointerType.Create(Result, 'PQaContainer', 'tagQACONTAINER', True);
  TSepiTypeAlias.Create(Result, 'TQaContainer', 'tagQACONTAINER');
  SepiImporttagQACONTROL(Result);
  TSepiPointerType.Create(Result, 'PQaControl', 'TQaControl', True);
  TSepiTypeAlias.Create(Result, 'TQaControl', 'tagQACONTROL');
  SepiImportIQuickActivate(Result);

  // Types
  SepiImportIObjectSafety(Result);

  // Types
  SepiImportIDispatchEx(Result);

  // Types
  TSepiMethodRefType.Create(Result, 'TDLLGetClassObject',
    'function(const clsid: TCLSID; const iid: TIID; out pv ) : HResult', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TDLLCanUnloadNow',
    'function: HResult', False, ccStdCall);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ActiveXTypes', ImportUnit);
end.

