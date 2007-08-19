{*
  Importe l'unité Classes dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsClasses;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, SysUtils, Windows, ActiveX, Classes;

implementation

{ You must not localize any of the strings this unit contains! }

{$WARN SYMBOL_DEPRECATED OFF}

type
  TSepiImportsEStreamError = class(EStreamError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEFileStreamError = class(EFileStreamError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEFCreateError = class(EFCreateError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEFOpenError = class(EFOpenError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEFilerError = class(EFilerError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEReadError = class(EReadError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEWriteError = class(EWriteError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEClassNotFound = class(EClassNotFound)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEMethodNotFound = class(EMethodNotFound)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEInvalidImage = class(EInvalidImage)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEResNotFound = class(EResNotFound)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEListError = class(EListError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEBitsError = class(EBitsError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEStringListError = class(EStringListError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEComponentError = class(EComponentError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEParserError = class(EParserError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOutOfResources = class(EOutOfResources)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEInvalidOperation = class(EInvalidOperation)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTListEnumerator = class(TListEnumerator)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTList = class(TList)
  private
    class procedure Error_1(Msg: PResStringRec; Data: Integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTThreadList = class(TThreadList)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTInterfaceListEnumerator = class(TInterfaceListEnumerator)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTInterfaceList = class(TInterfaceList)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTBits = class(TBits)
  private
    procedure SetSize(Value: Integer);
    procedure SetBit(Index: Integer; Value: Boolean);
    function GetBit(Index: Integer): Boolean;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTPersistent = class(TPersistent)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTInterfacedPersistent = class(TInterfacedPersistent)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTRecall = class(TRecall)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCollectionItem = class(TCollectionItem)
  private
    function GetIndex: Integer;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCollectionEnumerator = class(TCollectionEnumerator)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCollection = class(TCollection)
  private
    function GetCount: Integer;
    function GetPropName: string;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTOwnedCollection = class(TOwnedCollection)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTStringsEnumerator = class(TStringsEnumerator)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTStrings = class(TStrings)
  private
    function GetCommaText: string;
    function GetDelimitedText: string;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    procedure SetCommaText(const Value: string);
    procedure SetDelimitedText(const Value: string);
    procedure SetStringsAdapter(const Value: IStringsAdapter);
    procedure SetValue(const Name, Value: string);
    function GetDelimiter: Char;
    procedure SetDelimiter(const Value: Char);
    function GetLineBreak: string;
    procedure SetLineBreak(const Value: string);
    function GetQuoteChar: Char;
    procedure SetQuoteChar(const Value: Char);
    function GetNameValueSeparator: Char;
    procedure SetNameValueSeparator(const Value: Char);
    function GetValueFromIndex(Index: Integer): string;
    procedure SetValueFromIndex(Index: Integer; const Value: string);
    procedure Error_0(const Msg: string; Data: Integer);
    procedure Error_1(Msg: PResStringRec; Data: Integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTStringList = class(TStringList)
  private
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTStream = class(TStream)
  private
    function GetPosition: Int64;
    procedure SetPosition(const Pos: Int64);
    procedure SetSize64(const NewSize: Int64);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTHandleStream = class(THandleStream)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTFileStream = class(TFileStream)
  private
    constructor Create_0(const FileName: string; Mode: Word);
    constructor Create_1(const FileName: string; Mode: Word; Rights: Cardinal);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomMemoryStream = class(TCustomMemoryStream)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMemoryStream = class(TMemoryStream)
  private
    procedure SetCapacity(NewCapacity: Longint);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTStringStream = class(TStringStream)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTResourceStream = class(TResourceStream)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTStreamAdapter = class(TStreamAdapter)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTClassFinder = class(TClassFinder)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTFiler = class(TFiler)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTReader = class(TReader)
  private
    function GetPosition: Longint;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTWriter = class(TWriter)
  private
    function GetPosition: Longint;
    procedure SetPosition(Value: Longint);
    procedure WriteInteger_0(Value: Longint);
    procedure WriteInteger_1(Value: Int64);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTParser = class(TParser)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEThread = class(EThread)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTThread = class(TThread)
  private
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    procedure SetSuspended(Value: Boolean);
    procedure CheckThreadError_0(ErrCode: Integer);
    procedure CheckThreadError_1(Success: Boolean);
    procedure Queue_0(AMethod: TThreadMethod);
    procedure Synchronize_1(AMethod: TThreadMethod);
    class procedure Queue_1(AThread: TThread; AMethod: TThreadMethod);
    class procedure Synchronize_2(AThread: TThread; AMethod: TThreadMethod);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTComponentEnumerator = class(TComponentEnumerator)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTComponent = class(TComponent)
  private
    function GetComObject: IUnknown;
    function GetComponent(AIndex: Integer): TComponent;
    function GetComponentCount: Integer;
    function GetComponentIndex: Integer;
    procedure SetComponentIndex(Value: Integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTBasicActionLink = class(TBasicActionLink)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTBasicAction = class(TBasicAction)
  private
    procedure SetActionComponent(const Value: TComponent);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDataModule = class(TDataModule)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TUnnamed_1 = procedure (DataModule: TDataModule) of object;

  TUnnamed_2 = procedure (DataModule: TDataModule) of object;

  TUnnamed_3 = procedure (Sender: TObject) of object;

  TUnnamed_4 = procedure (E: Exception) of object;

{---------------------}
{ EStreamError import }
{---------------------}

class function TSepiImportsEStreamError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EStreamError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------------}
{ EFileStreamError import }
{-------------------------}

class function TSepiImportsEFileStreamError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EFileStreamError));

  with Result do
  begin
    AddMethod('Create', @TSepiImportsEFileStreamError.Create,
      'constructor(ResStringRec: PResStringRec; const FileName: string)');

    Complete;
  end;
end;

{----------------------}
{ EFCreateError import }
{----------------------}

class function TSepiImportsEFCreateError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EFCreateError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------}
{ EFOpenError import }
{--------------------}

class function TSepiImportsEFOpenError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EFOpenError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------}
{ EFilerError import }
{--------------------}

class function TSepiImportsEFilerError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EFilerError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------}
{ EReadError import }
{-------------------}

class function TSepiImportsEReadError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EReadError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------}
{ EWriteError import }
{--------------------}

class function TSepiImportsEWriteError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EWriteError));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ EClassNotFound import }
{-----------------------}

class function TSepiImportsEClassNotFound.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EClassNotFound));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ EMethodNotFound import }
{------------------------}

class function TSepiImportsEMethodNotFound.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EMethodNotFound));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------}
{ EInvalidImage import }
{----------------------}

class function TSepiImportsEInvalidImage.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidImage));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------}
{ EResNotFound import }
{---------------------}

class function TSepiImportsEResNotFound.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EResNotFound));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------}
{ EListError import }
{-------------------}

class function TSepiImportsEListError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EListError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------}
{ EBitsError import }
{-------------------}

class function TSepiImportsEBitsError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EBitsError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------------}
{ EStringListError import }
{-------------------------}

class function TSepiImportsEStringListError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EStringListError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ EComponentError import }
{------------------------}

class function TSepiImportsEComponentError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EComponentError));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------}
{ EParserError import }
{---------------------}

class function TSepiImportsEParserError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EParserError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ EOutOfResources import }
{------------------------}

class function TSepiImportsEOutOfResources.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOutOfResources));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------------}
{ EInvalidOperation import }
{--------------------------}

class function TSepiImportsEInvalidOperation.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidOperation));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ TListEnumerator import }
{------------------------}

class function TSepiImportsTListEnumerator.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TListEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FList', System.TypeInfo(TList));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTListEnumerator.Create,
      'constructor(AList: TList)');
    AddMethod('GetCurrent', @TSepiImportsTListEnumerator.GetCurrent,
      'function: Pointer');
    AddMethod('MoveNext', @TSepiImportsTListEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: Pointer',
      'GetCurrent', '');

    Complete;
  end;
end;

{--------------}
{ TList import }
{--------------}

class procedure TSepiImportsTList.Error_1(Msg: PResStringRec; Data: Integer);
begin
  Error(Msg, Data);
end;

class function TSepiImportsTList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FList', 'PPointerList');
    AddField('FCount', System.TypeInfo(Integer));
    AddField('FCapacity', System.TypeInfo(Integer));

    CurrentVisibility := mvProtected;

    AddMethod('Get', @TSepiImportsTList.Get,
      'function(Index: Integer): Pointer');
    AddMethod('Grow', @TSepiImportsTList.Grow,
      'procedure',
      mlkVirtual);
    AddMethod('Put', @TSepiImportsTList.Put,
      'procedure(Index: Integer; Item: Pointer)');
    AddMethod('Notify', @TSepiImportsTList.Notify,
      'procedure(Ptr: Pointer; Action: TListNotification)',
      mlkVirtual);
    AddMethod('SetCapacity', @TSepiImportsTList.SetCapacity,
      'procedure(NewCapacity: Integer)');
    AddMethod('SetCount', @TSepiImportsTList.SetCount,
      'procedure(NewCount: Integer)');

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTList.Add,
      'function(Item: Pointer): Integer');
    AddMethod('Clear', @TSepiImportsTList.Clear,
      'procedure',
      mlkVirtual);
    AddMethod('Delete', @TSepiImportsTList.Delete,
      'procedure(Index: Integer)');
    AddOverloadedMethod('Error', nil,
      'class procedure(const Msg: string; Data: Integer)',
      mlkVirtual);
    AddOverloadedMethod('Error', @TSepiImportsTList.Error_1,
      'class procedure(Msg: PResStringRec; Data: Integer)');
    AddMethod('Exchange', @TSepiImportsTList.Exchange,
      'procedure(Index1, Index2: Integer)');
    AddMethod('Expand', @TSepiImportsTList.Expand,
      'function: TList');
    AddMethod('Extract', @TSepiImportsTList.Extract,
      'function(Item: Pointer): Pointer');
    AddMethod('First', @TSepiImportsTList.First,
      'function: Pointer');
    AddMethod('GetEnumerator', @TSepiImportsTList.GetEnumerator,
      'function: TListEnumerator');
    AddMethod('IndexOf', @TSepiImportsTList.IndexOf,
      'function(Item: Pointer): Integer');
    AddMethod('Insert', @TSepiImportsTList.Insert,
      'procedure(Index: Integer; Item: Pointer)');
    AddMethod('Last', @TSepiImportsTList.Last,
      'function: Pointer');
    AddMethod('Move', @TSepiImportsTList.Move,
      'procedure(CurIndex, NewIndex: Integer)');
    AddMethod('Remove', @TSepiImportsTList.Remove,
      'function(Item: Pointer): Integer');
    AddMethod('Pack', @TSepiImportsTList.Pack,
      'procedure');
    AddMethod('Sort', @TSepiImportsTList.Sort,
      'procedure(Compare: TListSortCompare)');
    AddMethod('Assign', @TSepiImportsTList.Assign,
      'procedure(ListA: TList; AOperator: TListAssignOp = laCopy; ListB: TList = nil)');

    AddProperty('Capacity', 'property: Integer',
      'FCapacity', 'SetCapacity');
    AddProperty('Count', 'property: Integer',
      'FCount', 'SetCount');
    AddProperty('Items', 'property[Index: Integer]: Pointer',
      'Get', 'Put', True);
    AddProperty('List', 'property: PPointerList',
      'FList', '');

    Complete;
  end;
end;

{--------------------}
{ TThreadList import }
{--------------------}

class function TSepiImportsTThreadList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TThreadList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FList', System.TypeInfo(TList));
    AddField('FLock', 'TRTLCriticalSection');
    AddField('FDuplicates', System.TypeInfo(TDuplicates));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTThreadList.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTThreadList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTThreadList.Add,
      'procedure(Item: Pointer)');
    AddMethod('Clear', @TSepiImportsTThreadList.Clear,
      'procedure');
    AddMethod('LockList', @TSepiImportsTThreadList.LockList,
      'function: TList');
    AddMethod('Remove', @TSepiImportsTThreadList.Remove,
      'procedure(Item: Pointer)');
    AddMethod('UnlockList', @TSepiImportsTThreadList.UnlockList,
      'procedure');

    AddProperty('Duplicates', 'property: TDuplicates',
      'FDuplicates', 'FDuplicates');

    Complete;
  end;
end;

{-----------------------}
{ IInterfaceList import }
{-----------------------}

function SepiImportIInterfaceList(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IInterfaceList));

  with Result do
  begin
    AddMethod('Get',
      'function(Index: Integer): IInterface', ccRegister);
    AddMethod('GetCapacity',
      'function: Integer', ccRegister);
    AddMethod('GetCount',
      'function: Integer', ccRegister);
    AddMethod('Put',
      'procedure(Index: Integer; const Item: IInterface)', ccRegister);
    AddMethod('SetCapacity',
      'procedure(NewCapacity: Integer)', ccRegister);
    AddMethod('SetCount',
      'procedure(NewCount: Integer)', ccRegister);
    AddMethod('Clear',
      'procedure', ccRegister);
    AddMethod('Delete',
      'procedure(Index: Integer)', ccRegister);
    AddMethod('Exchange',
      'procedure(Index1, Index2: Integer)', ccRegister);
    AddMethod('First',
      'function: IInterface', ccRegister);
    AddMethod('IndexOf',
      'function(const Item: IInterface): Integer', ccRegister);
    AddMethod('Add',
      'function(const Item: IInterface): Integer', ccRegister);
    AddMethod('Insert',
      'procedure(Index: Integer; const Item: IInterface)', ccRegister);
    AddMethod('Last',
      'function: IInterface', ccRegister);
    AddMethod('Remove',
      'function(const Item: IInterface): Integer', ccRegister);
    AddMethod('Lock',
      'procedure', ccRegister);
    AddMethod('Unlock',
      'procedure', ccRegister);
    AddProperty('Capacity', 'property: Integer',
      'GetCapacity', 'SetCapacity');
    AddProperty('Count', 'property: Integer',
      'GetCount', 'SetCount');
    AddProperty('Items', 'property[Index: Integer]: IInterface',
      'Get', 'Put');

    Complete;
  end;
end;

{---------------------------------}
{ TInterfaceListEnumerator import }
{---------------------------------}

class function TSepiImportsTInterfaceListEnumerator.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TInterfaceListEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FInterfaceList', System.TypeInfo(TInterfaceList));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTInterfaceListEnumerator.Create,
      'constructor(AInterfaceList: TInterfaceList)');
    AddMethod('GetCurrent', @TSepiImportsTInterfaceListEnumerator.GetCurrent,
      'function: IInterface');
    AddMethod('MoveNext', @TSepiImportsTInterfaceListEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: IInterface',
      'GetCurrent', '');

    Complete;
  end;
end;

{-----------------------}
{ TInterfaceList import }
{-----------------------}

class function TSepiImportsTInterfaceList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TInterfaceList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TInterfaceList));

  with Result do
  begin
    AddInterface(System.TypeInfo(IInterfaceList));

    CurrentVisibility := mvPrivate;

    AddField('FList', System.TypeInfo(TThreadList));

    CurrentVisibility := mvProtected;

    AddMethod('Get', @TSepiImportsTInterfaceList.Get,
      'function(Index: Integer): IInterface');
    AddMethod('GetCapacity', @TSepiImportsTInterfaceList.GetCapacity,
      'function: Integer');
    AddMethod('GetCount', @TSepiImportsTInterfaceList.GetCount,
      'function: Integer');
    AddMethod('Put', @TSepiImportsTInterfaceList.Put,
      'procedure(Index: Integer; const Item: IInterface)');
    AddMethod('SetCapacity', @TSepiImportsTInterfaceList.SetCapacity,
      'procedure(NewCapacity: Integer)');
    AddMethod('SetCount', @TSepiImportsTInterfaceList.SetCount,
      'procedure(NewCount: Integer)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTInterfaceList.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTInterfaceList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTInterfaceList.Clear,
      'procedure');
    AddMethod('Delete', @TSepiImportsTInterfaceList.Delete,
      'procedure(Index: Integer)');
    AddMethod('Exchange', @TSepiImportsTInterfaceList.Exchange,
      'procedure(Index1, Index2: Integer)');
    AddMethod('Expand', @TSepiImportsTInterfaceList.Expand,
      'function: TInterfaceList');
    AddMethod('First', @TSepiImportsTInterfaceList.First,
      'function: IInterface');
    AddMethod('GetEnumerator', @TSepiImportsTInterfaceList.GetEnumerator,
      'function: TInterfaceListEnumerator');
    AddMethod('IndexOf', @TSepiImportsTInterfaceList.IndexOf,
      'function(const Item: IInterface): Integer');
    AddMethod('Add', @TSepiImportsTInterfaceList.Add,
      'function(const Item: IInterface): Integer');
    AddMethod('Insert', @TSepiImportsTInterfaceList.Insert,
      'procedure(Index: Integer; const Item: IInterface)');
    AddMethod('Last', @TSepiImportsTInterfaceList.Last,
      'function: IInterface');
    AddMethod('Remove', @TSepiImportsTInterfaceList.Remove,
      'function(const Item: IInterface): Integer');
    AddMethod('Lock', @TSepiImportsTInterfaceList.Lock,
      'procedure');
    AddMethod('Unlock', @TSepiImportsTInterfaceList.Unlock,
      'procedure');

    AddProperty('Capacity', 'property: Integer',
      'GetCapacity', 'SetCapacity');
    AddProperty('Count', 'property: Integer',
      'GetCount', 'SetCount');
    AddProperty('Items', 'property[Index: Integer]: IInterface',
      'Get', 'Put', True);

    Complete;
  end;
end;

{--------------}
{ TBits import }
{--------------}

procedure TSepiImportsTBits.SetSize(Value: Integer);
begin
  Size := Value;
end;

procedure TSepiImportsTBits.SetBit(Index: Integer; Value: Boolean);
begin
  Bits[Index] := Value;
end;

function TSepiImportsTBits.GetBit(Index: Integer): Boolean;
begin
  Result := Bits[Index];
end;

class function TSepiImportsTBits.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBits));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FSize', System.TypeInfo(Integer));
    AddField('FBits', 'Pointer');

    AddMethod('Error', nil,
      'procedure');
    AddMethod('SetSize', @TSepiImportsTBits.SetSize,
      'procedure(Value: Integer)');
    AddMethod('SetBit', @TSepiImportsTBits.SetBit,
      'procedure(Index: Integer; Value: Boolean)');
    AddMethod('GetBit', @TSepiImportsTBits.GetBit,
      'function(Index: Integer): Boolean');

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTBits.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('OpenBit', @TSepiImportsTBits.OpenBit,
      'function: Integer');

    AddProperty('Bits', 'property[Index: Integer]: Boolean',
      'GetBit', 'SetBit', True);
    AddProperty('Size', 'property: Integer',
      'FSize', 'SetSize');

    Complete;
  end;
end;

{--------------------}
{ TPersistent import }
{--------------------}

class function TSepiImportsTPersistent.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPersistent));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('AssignError', nil,
      'procedure(Source: TPersistent)');

    CurrentVisibility := mvProtected;

    AddMethod('AssignTo', @TSepiImportsTPersistent.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkVirtual);
    AddMethod('DefineProperties', @TSepiImportsTPersistent.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkVirtual);
    AddMethod('GetOwner', @TSepiImportsTPersistent.GetOwner,
      'function: TPersistent',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTPersistent.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTPersistent.Assign,
      'procedure(Source: TPersistent)',
      mlkVirtual);
    AddMethod('GetNamePath', @TSepiImportsTPersistent.GetNamePath,
      'function: string',
      mlkDynamic);

    Complete;
  end;
end;

{------------------------------}
{ TInterfacedPersistent import }
{------------------------------}

class function TSepiImportsTInterfacedPersistent.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TInterfacedPersistent));

  with Result do
  begin
    AddInterface(System.TypeInfo(IInterface));

    CurrentVisibility := mvPrivate;

    AddField('FOwnerInterface', System.TypeInfo(IInterface));

    CurrentVisibility := mvProtected;

    AddMethod('_AddRef', @TSepiImportsTInterfacedPersistent._AddRef,
      'function: Integer',
      ccStdCall);
    AddMethod('_Release', @TSepiImportsTInterfacedPersistent._Release,
      'function: Integer',
      ccStdCall);

    CurrentVisibility := mvPublic;

    AddMethod('QueryInterface', @TSepiImportsTInterfacedPersistent.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('AfterConstruction', @TSepiImportsTInterfacedPersistent.AfterConstruction,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{----------------}
{ TRecall import }
{----------------}

class function TSepiImportsTRecall.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TRecall));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStorage', System.TypeInfo(TPersistent));
    AddField('FReference', System.TypeInfo(TPersistent), True);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTRecall.Create,
      'constructor(AStorage, AReference: TPersistent)');
    AddMethod('Destroy', @TSepiImportsTRecall.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Store', @TSepiImportsTRecall.Store,
      'procedure');
    AddMethod('Forget', @TSepiImportsTRecall.Forget,
      'procedure');

    AddProperty('Reference', 'property: TPersistent',
      'FReference', '');

    Complete;
  end;
end;

{------------------------}
{ TCollectionItem import }
{------------------------}

function TSepiImportsTCollectionItem.GetIndex: Integer;
begin
  Result := Index;
end;

class function TSepiImportsTCollectionItem.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCollectionItem));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCollection', System.TypeInfo(TCollection));
    AddField('FID', System.TypeInfo(Integer));

    AddMethod('GetIndex', @TSepiImportsTCollectionItem.GetIndex,
      'function: Integer');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTCollectionItem.Changed,
      'procedure(AllItems: Boolean)');
    AddMethod('GetOwner', @TSepiImportsTCollectionItem.GetOwner,
      'function: TPersistent',
      mlkOverride);
    AddMethod('GetDisplayName', @TSepiImportsTCollectionItem.GetDisplayName,
      'function: string',
      mlkVirtual);
    AddMethod('SetCollection', @TSepiImportsTCollectionItem.SetCollection,
      'procedure(Value: TCollection)',
      mlkVirtual);
    AddMethod('SetIndex', @TSepiImportsTCollectionItem.SetIndex,
      'procedure(Value: Integer)',
      mlkVirtual);
    AddMethod('SetDisplayName', @TSepiImportsTCollectionItem.SetDisplayName,
      'procedure(const Value: string)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCollectionItem.Create,
      'constructor(Collection: TCollection)',
      mlkVirtual);
    AddMethod('Destroy', @TSepiImportsTCollectionItem.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('GetNamePath', @TSepiImportsTCollectionItem.GetNamePath,
      'function: string',
      mlkOverride);

    AddProperty('Collection', 'property: TCollection',
      'FCollection', 'SetCollection');
    AddProperty('ID', 'property: Integer',
      'FID', '');
    AddProperty('Index', 'property: Integer',
      'GetIndex', 'SetIndex');
    AddProperty('DisplayName', 'property: string',
      'GetDisplayName', 'SetDisplayName');

    Complete;
  end;
end;

{------------------------------}
{ TCollectionEnumerator import }
{------------------------------}

class function TSepiImportsTCollectionEnumerator.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCollectionEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FCollection', System.TypeInfo(TCollection));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCollectionEnumerator.Create,
      'constructor(ACollection: TCollection)');
    AddMethod('GetCurrent', @TSepiImportsTCollectionEnumerator.GetCurrent,
      'function: TCollectionItem');
    AddMethod('MoveNext', @TSepiImportsTCollectionEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: TCollectionItem',
      'GetCurrent', '');

    Complete;
  end;
end;

{--------------------}
{ TCollection import }
{--------------------}

function TSepiImportsTCollection.GetCount: Integer;
begin
  Result := Count;
end;

function TSepiImportsTCollection.GetPropName: string;
begin
  Result := PropName;
end;

class function TSepiImportsTCollection.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCollection'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCollection));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FItemClass', 'TCollectionItemClass');
    AddField('FItems', System.TypeInfo(TList));
    AddField('FUpdateCount', System.TypeInfo(Integer));
    AddField('FNextID', System.TypeInfo(Integer));
    AddField('FPropName', System.TypeInfo(string));

    AddMethod('GetCount', @TSepiImportsTCollection.GetCount,
      'function: Integer');
    AddMethod('GetPropName', @TSepiImportsTCollection.GetPropName,
      'function: string');
    AddMethod('InsertItem', nil,
      'procedure(Item: TCollectionItem)');
    AddMethod('RemoveItem', nil,
      'procedure(Item: TCollectionItem)');

    CurrentVisibility := mvProtected;

    AddMethod('Added', @TSepiImportsTCollection.Added,
      'procedure(var Item: TCollectionItem)',
      mlkVirtual);
    AddMethod('Deleting', @TSepiImportsTCollection.Deleting,
      'procedure(Item: TCollectionItem)',
      mlkVirtual);

    AddProperty('NextID', 'property: Integer',
      'FNextID', '');

    AddMethod('Notify', @TSepiImportsTCollection.Notify,
      'procedure(Item: TCollectionItem; Action: TCollectionNotification)',
      mlkVirtual);
    AddMethod('GetAttrCount', @TSepiImportsTCollection.GetAttrCount,
      'function: Integer',
      mlkDynamic);
    AddMethod('GetAttr', @TSepiImportsTCollection.GetAttr,
      'function(Index: Integer): string',
      mlkDynamic);
    AddMethod('GetItemAttr', @TSepiImportsTCollection.GetItemAttr,
      'function(Index, ItemIndex: Integer): string',
      mlkDynamic);
    AddMethod('Changed', @TSepiImportsTCollection.Changed,
      'procedure');
    AddMethod('GetItem', @TSepiImportsTCollection.GetItem,
      'function(Index: Integer): TCollectionItem');
    AddMethod('SetItem', @TSepiImportsTCollection.SetItem,
      'procedure(Index: Integer; Value: TCollectionItem)');
    AddMethod('SetItemName', @TSepiImportsTCollection.SetItemName,
      'procedure(Item: TCollectionItem)',
      mlkVirtual);
    AddMethod('Update', @TSepiImportsTCollection.Update,
      'procedure(Item: TCollectionItem)',
      mlkVirtual);

    AddProperty('PropName', 'property: string',
      'GetPropName', 'FPropName');
    AddProperty('UpdateCount', 'property: Integer',
      'FUpdateCount', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCollection.Create,
      'constructor(ItemClass: TCollectionItemClass)');
    AddMethod('Destroy', @TSepiImportsTCollection.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Owner', @TSepiImportsTCollection.Owner,
      'function: TPersistent');
    AddMethod('Add', @TSepiImportsTCollection.Add,
      'function: TCollectionItem');
    AddMethod('Assign', @TSepiImportsTCollection.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('BeginUpdate', @TSepiImportsTCollection.BeginUpdate,
      'procedure',
      mlkVirtual);
    AddMethod('Clear', @TSepiImportsTCollection.Clear,
      'procedure');
    AddMethod('Delete', @TSepiImportsTCollection.Delete,
      'procedure(Index: Integer)');
    AddMethod('EndUpdate', @TSepiImportsTCollection.EndUpdate,
      'procedure',
      mlkVirtual);
    AddMethod('FindItemID', @TSepiImportsTCollection.FindItemID,
      'function(ID: Integer): TCollectionItem');
    AddMethod('GetEnumerator', @TSepiImportsTCollection.GetEnumerator,
      'function: TCollectionEnumerator');
    AddMethod('GetNamePath', @TSepiImportsTCollection.GetNamePath,
      'function: string',
      mlkOverride);
    AddMethod('Insert', @TSepiImportsTCollection.Insert,
      'function(Index: Integer): TCollectionItem');

    AddProperty('Count', 'property: Integer',
      'GetCount', '');
    AddProperty('ItemClass', 'property: TCollectionItemClass',
      'FItemClass', '');
    AddProperty('Items', 'property[Index: Integer]: TCollectionItem',
      'GetItem', 'SetItem');

    Complete;
  end;
end;

{-------------------------}
{ TOwnedCollection import }
{-------------------------}

class function TSepiImportsTOwnedCollection.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TOwnedCollection));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOwner', System.TypeInfo(TPersistent));

    CurrentVisibility := mvProtected;

    AddMethod('GetOwner', @TSepiImportsTOwnedCollection.GetOwner,
      'function: TPersistent',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTOwnedCollection.Create,
      'constructor(AOwner: TPersistent; ItemClass: TCollectionItemClass)');

    Complete;
  end;
end;

{------------------------}
{ IStringsAdapter import }
{------------------------}

function SepiImportIStringsAdapter(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IStringsAdapter));

  with Result do
  begin
    AddMethod('ReferenceStrings',
      'procedure(S: TStrings)', ccRegister);
    AddMethod('ReleaseStrings',
      'procedure', ccRegister);

    Complete;
  end;
end;

{---------------------------}
{ TStringsEnumerator import }
{---------------------------}

class function TSepiImportsTStringsEnumerator.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TStringsEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FStrings', System.TypeInfo(TStrings));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTStringsEnumerator.Create,
      'constructor(AStrings: TStrings)');
    AddMethod('GetCurrent', @TSepiImportsTStringsEnumerator.GetCurrent,
      'function: string');
    AddMethod('MoveNext', @TSepiImportsTStringsEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: string',
      'GetCurrent', '');

    Complete;
  end;
end;

{-----------------}
{ TStrings import }
{-----------------}

function TSepiImportsTStrings.GetCommaText: string;
begin
  Result := CommaText;
end;

function TSepiImportsTStrings.GetDelimitedText: string;
begin
  Result := DelimitedText;
end;

function TSepiImportsTStrings.GetName(Index: Integer): string;
begin
  Result := Names[Index];
end;

function TSepiImportsTStrings.GetValue(const Name: string): string;
begin
  Result := Values[Name];
end;

procedure TSepiImportsTStrings.SetCommaText(const Value: string);
begin
  CommaText := Value;
end;

procedure TSepiImportsTStrings.SetDelimitedText(const Value: string);
begin
  DelimitedText := Value;
end;

procedure TSepiImportsTStrings.SetStringsAdapter(const Value: IStringsAdapter);
begin
  StringsAdapter := Value;
end;

procedure TSepiImportsTStrings.SetValue(const Name, Value: string);
begin
  Values[Name] := Value;
end;

function TSepiImportsTStrings.GetDelimiter: Char;
begin
  Result := Delimiter;
end;

procedure TSepiImportsTStrings.SetDelimiter(const Value: Char);
begin
  Delimiter := Value;
end;

function TSepiImportsTStrings.GetLineBreak: string;
begin
  Result := LineBreak;
end;

procedure TSepiImportsTStrings.SetLineBreak(const Value: string);
begin
  LineBreak := Value;
end;

function TSepiImportsTStrings.GetQuoteChar: Char;
begin
  Result := QuoteChar;
end;

procedure TSepiImportsTStrings.SetQuoteChar(const Value: Char);
begin
  QuoteChar := Value;
end;

function TSepiImportsTStrings.GetNameValueSeparator: Char;
begin
  Result := NameValueSeparator;
end;

procedure TSepiImportsTStrings.SetNameValueSeparator(const Value: Char);
begin
  NameValueSeparator := Value;
end;

function TSepiImportsTStrings.GetValueFromIndex(Index: Integer): string;
begin
  Result := ValueFromIndex[Index];
end;

procedure TSepiImportsTStrings.SetValueFromIndex(Index: Integer; const Value: string);
begin
  ValueFromIndex[Index] := Value;
end;

procedure TSepiImportsTStrings.Error_0(const Msg: string; Data: Integer);
begin
  Error(Msg, Data);
end;

procedure TSepiImportsTStrings.Error_1(Msg: PResStringRec; Data: Integer);
begin
  Error(Msg, Data);
end;

class function TSepiImportsTStrings.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TStrings'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TStrings));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDefined', System.TypeInfo(TStringsDefined));
    AddField('FDelimiter', System.TypeInfo(Char));
    AddField('FLineBreak', System.TypeInfo(string));
    AddField('FQuoteChar', System.TypeInfo(Char));
    AddField('FNameValueSeparator', System.TypeInfo(Char));
    AddField('FUpdateCount', System.TypeInfo(Integer));
    AddField('FAdapter', System.TypeInfo(IStringsAdapter));

    AddMethod('GetCommaText', @TSepiImportsTStrings.GetCommaText,
      'function: string');
    AddMethod('GetDelimitedText', @TSepiImportsTStrings.GetDelimitedText,
      'function: string');
    AddMethod('GetName', @TSepiImportsTStrings.GetName,
      'function(Index: Integer): string');
    AddMethod('GetValue', @TSepiImportsTStrings.GetValue,
      'function(const Name: string): string');
    AddMethod('ReadData', nil,
      'procedure(Reader: TReader)');
    AddMethod('SetCommaText', @TSepiImportsTStrings.SetCommaText,
      'procedure(const Value: string)');
    AddMethod('SetDelimitedText', @TSepiImportsTStrings.SetDelimitedText,
      'procedure(const Value: string)');
    AddMethod('SetStringsAdapter', @TSepiImportsTStrings.SetStringsAdapter,
      'procedure(const Value: IStringsAdapter)');
    AddMethod('SetValue', @TSepiImportsTStrings.SetValue,
      'procedure(const Name, Value: string)');
    AddMethod('WriteData', nil,
      'procedure(Writer: TWriter)');
    AddMethod('GetDelimiter', @TSepiImportsTStrings.GetDelimiter,
      'function: Char');
    AddMethod('SetDelimiter', @TSepiImportsTStrings.SetDelimiter,
      'procedure(const Value: Char)');
    AddMethod('GetLineBreak', @TSepiImportsTStrings.GetLineBreak,
      'function: string');
    AddMethod('SetLineBreak', @TSepiImportsTStrings.SetLineBreak,
      'procedure(const Value: string)');
    AddMethod('GetQuoteChar', @TSepiImportsTStrings.GetQuoteChar,
      'function: Char');
    AddMethod('SetQuoteChar', @TSepiImportsTStrings.SetQuoteChar,
      'procedure(const Value: Char)');
    AddMethod('GetNameValueSeparator', @TSepiImportsTStrings.GetNameValueSeparator,
      'function: Char');
    AddMethod('SetNameValueSeparator', @TSepiImportsTStrings.SetNameValueSeparator,
      'procedure(const Value: Char)');
    AddMethod('GetValueFromIndex', @TSepiImportsTStrings.GetValueFromIndex,
      'function(Index: Integer): string');
    AddMethod('SetValueFromIndex', @TSepiImportsTStrings.SetValueFromIndex,
      'procedure(Index: Integer; const Value: string)');

    CurrentVisibility := mvProtected;

    AddMethod('DefineProperties', @TSepiImportsTStrings.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddOverloadedMethod('Error', @TSepiImportsTStrings.Error_0,
      'procedure(const Msg: string; Data: Integer)');
    AddOverloadedMethod('Error', @TSepiImportsTStrings.Error_1,
      'procedure(Msg: PResStringRec; Data: Integer)');
    AddMethod('ExtractName', @TSepiImportsTStrings.ExtractName,
      'function(const S: string): string');
    AddMethod('Get', nil,
      'function(Index: Integer): string',
      mlkVirtual, True);
    AddMethod('GetCapacity', @TSepiImportsTStrings.GetCapacity,
      'function: Integer',
      mlkVirtual);
    AddMethod('GetCount', nil,
      'function: Integer',
      mlkVirtual, True);
    AddMethod('GetObject', @TSepiImportsTStrings.GetObject,
      'function(Index: Integer): TObject',
      mlkVirtual);
    AddMethod('GetTextStr', @TSepiImportsTStrings.GetTextStr,
      'function: string',
      mlkVirtual);
    AddMethod('Put', @TSepiImportsTStrings.Put,
      'procedure(Index: Integer; const S: string)',
      mlkVirtual);
    AddMethod('PutObject', @TSepiImportsTStrings.PutObject,
      'procedure(Index: Integer; AObject: TObject)',
      mlkVirtual);
    AddMethod('SetCapacity', @TSepiImportsTStrings.SetCapacity,
      'procedure(NewCapacity: Integer)',
      mlkVirtual);
    AddMethod('SetTextStr', @TSepiImportsTStrings.SetTextStr,
      'procedure(const Value: string)',
      mlkVirtual);
    AddMethod('SetUpdateState', @TSepiImportsTStrings.SetUpdateState,
      'procedure(Updating: Boolean)',
      mlkVirtual);

    AddProperty('UpdateCount', 'property: Integer',
      'FUpdateCount', '');

    AddMethod('CompareStrings', @TSepiImportsTStrings.CompareStrings,
      'function(const S1, S2: string): Integer',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTStrings.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTStrings.Add,
      'function(const S: string): Integer',
      mlkVirtual);
    AddMethod('AddObject', @TSepiImportsTStrings.AddObject,
      'function(const S: string; AObject: TObject): Integer',
      mlkVirtual);
    AddMethod('Append', @TSepiImportsTStrings.Append,
      'procedure(const S: string)');
    AddMethod('AddStrings', @TSepiImportsTStrings.AddStrings,
      'procedure(Strings: TStrings)',
      mlkVirtual);
    AddMethod('Assign', @TSepiImportsTStrings.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('BeginUpdate', @TSepiImportsTStrings.BeginUpdate,
      'procedure');
    AddMethod('Clear', nil,
      'procedure',
      mlkVirtual, True);
    AddMethod('Delete', nil,
      'procedure(Index: Integer)',
      mlkVirtual, True);
    AddMethod('EndUpdate', @TSepiImportsTStrings.EndUpdate,
      'procedure');
    AddMethod('Equals', @TSepiImportsTStrings.Equals,
      'function(Strings: TStrings): Boolean');
    AddMethod('Exchange', @TSepiImportsTStrings.Exchange,
      'procedure(Index1, Index2: Integer)',
      mlkVirtual);
    AddMethod('GetEnumerator', @TSepiImportsTStrings.GetEnumerator,
      'function: TStringsEnumerator');
    AddMethod('GetText', @TSepiImportsTStrings.GetText,
      'function: PChar',
      mlkVirtual);
    AddMethod('IndexOf', @TSepiImportsTStrings.IndexOf,
      'function(const S: string): Integer',
      mlkVirtual);
    AddMethod('IndexOfName', @TSepiImportsTStrings.IndexOfName,
      'function(const Name: string): Integer',
      mlkVirtual);
    AddMethod('IndexOfObject', @TSepiImportsTStrings.IndexOfObject,
      'function(AObject: TObject): Integer',
      mlkVirtual);
    AddMethod('Insert', nil,
      'procedure(Index: Integer; const S: string)',
      mlkVirtual, True);
    AddMethod('InsertObject', @TSepiImportsTStrings.InsertObject,
      'procedure(Index: Integer; const S: string; AObject: TObject )',
      mlkVirtual);
    AddMethod('LoadFromFile', @TSepiImportsTStrings.LoadFromFile,
      'procedure(const FileName: string)',
      mlkVirtual);
    AddMethod('LoadFromStream', @TSepiImportsTStrings.LoadFromStream,
      'procedure(Stream: TStream)',
      mlkVirtual);
    AddMethod('Move', @TSepiImportsTStrings.Move,
      'procedure(CurIndex, NewIndex: Integer)',
      mlkVirtual);
    AddMethod('SaveToFile', @TSepiImportsTStrings.SaveToFile,
      'procedure(const FileName: string)',
      mlkVirtual);
    AddMethod('SaveToStream', @TSepiImportsTStrings.SaveToStream,
      'procedure(Stream: TStream)',
      mlkVirtual);
    AddMethod('SetText', @TSepiImportsTStrings.SetText,
      'procedure(Text: PChar)',
      mlkVirtual);

    AddProperty('Capacity', 'property: Integer',
      'GetCapacity', 'SetCapacity');
    AddProperty('CommaText', 'property: string',
      'GetCommaText', 'SetCommaText');
    AddProperty('Count', 'property: Integer',
      'GetCount', '');
    AddProperty('Delimiter', 'property: Char',
      'GetDelimiter', 'SetDelimiter');
    AddProperty('DelimitedText', 'property: string',
      'GetDelimitedText', 'SetDelimitedText');
    AddProperty('LineBreak', 'property: string',
      'GetLineBreak', 'SetLineBreak');
    AddProperty('Names', 'property[Index: Integer]: string',
      'GetName', '');
    AddProperty('Objects', 'property[Index: Integer]: TObject',
      'GetObject', 'PutObject');
    AddProperty('QuoteChar', 'property: Char',
      'GetQuoteChar', 'SetQuoteChar');
    AddProperty('Values', 'property[const Name: string]: string',
      'GetValue', 'SetValue');
    AddProperty('ValueFromIndex', 'property[Index: Integer]: string',
      'GetValueFromIndex', 'SetValueFromIndex');
    AddProperty('NameValueSeparator', 'property: Char',
      'GetNameValueSeparator', 'SetNameValueSeparator');
    AddProperty('Strings', 'property[Index: Integer]: string',
      'Get', 'Put', True);
    AddProperty('Text', 'property: string',
      'GetTextStr', 'SetTextStr');
    AddProperty('StringsAdapter', 'property: IStringsAdapter',
      'FAdapter', 'SetStringsAdapter');

    Complete;
  end;
end;

{--------------------}
{ TStringItem import }
{--------------------}

function SepiImportTStringItem(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TStringItem', False, True,
    TypeInfo(TStringItem));

  with Result do
  begin
    AddField('FString', System.TypeInfo(string));
    AddField('FObject', System.TypeInfo(TObject));

    Complete;
  end;
end;

{--------------------}
{ TStringList import }
{--------------------}

procedure TSepiImportsTStringList.SetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TSepiImportsTStringList.SetCaseSensitive(const Value: Boolean);
begin
  CaseSensitive := Value;
end;

class function TSepiImportsTStringList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TStringList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TStringList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FList', 'PStringItemList');
    AddField('FCount', System.TypeInfo(Integer));
    AddField('FCapacity', System.TypeInfo(Integer));
    AddField('FSorted', System.TypeInfo(Boolean));
    AddField('FDuplicates', System.TypeInfo(TDuplicates));
    AddField('FCaseSensitive', System.TypeInfo(Boolean));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnChanging', System.TypeInfo(TNotifyEvent));

    AddMethod('ExchangeItems', nil,
      'procedure(Index1, Index2: Integer)');
    AddMethod('Grow', nil,
      'procedure');
    AddMethod('QuickSort', nil,
      'procedure(L, R: Integer; SCompare: TStringListSortCompare)');
    AddMethod('SetSorted', @TSepiImportsTStringList.SetSorted,
      'procedure(Value: Boolean)');
    AddMethod('SetCaseSensitive', @TSepiImportsTStringList.SetCaseSensitive,
      'procedure(const Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTStringList.Changed,
      'procedure',
      mlkVirtual);
    AddMethod('Changing', @TSepiImportsTStringList.Changing,
      'procedure',
      mlkVirtual);
    AddMethod('Get', @TSepiImportsTStringList.Get,
      'function(Index: Integer): string',
      mlkOverride);
    AddMethod('GetCapacity', @TSepiImportsTStringList.GetCapacity,
      'function: Integer',
      mlkOverride);
    AddMethod('GetCount', @TSepiImportsTStringList.GetCount,
      'function: Integer',
      mlkOverride);
    AddMethod('GetObject', @TSepiImportsTStringList.GetObject,
      'function(Index: Integer): TObject',
      mlkOverride);
    AddMethod('Put', @TSepiImportsTStringList.Put,
      'procedure(Index: Integer; const S: string)',
      mlkOverride);
    AddMethod('PutObject', @TSepiImportsTStringList.PutObject,
      'procedure(Index: Integer; AObject: TObject)',
      mlkOverride);
    AddMethod('SetCapacity', @TSepiImportsTStringList.SetCapacity,
      'procedure(NewCapacity: Integer)',
      mlkOverride);
    AddMethod('SetUpdateState', @TSepiImportsTStringList.SetUpdateState,
      'procedure(Updating: Boolean)',
      mlkOverride);
    AddMethod('CompareStrings', @TSepiImportsTStringList.CompareStrings,
      'function(const S1, S2: string): Integer',
      mlkOverride);
    AddMethod('InsertItem', @TSepiImportsTStringList.InsertItem,
      'procedure(Index: Integer; const S: string; AObject: TObject)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTStringList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTStringList.Add,
      'function(const S: string): Integer',
      mlkOverride);
    AddMethod('AddObject', @TSepiImportsTStringList.AddObject,
      'function(const S: string; AObject: TObject): Integer',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTStringList.Clear,
      'procedure',
      mlkOverride);
    AddMethod('Delete', @TSepiImportsTStringList.Delete,
      'procedure(Index: Integer)',
      mlkOverride);
    AddMethod('Exchange', @TSepiImportsTStringList.Exchange,
      'procedure(Index1, Index2: Integer)',
      mlkOverride);
    AddMethod('Find', @TSepiImportsTStringList.Find,
      'function(const S: string; var Index: Integer): Boolean',
      mlkVirtual);
    AddMethod('IndexOf', @TSepiImportsTStringList.IndexOf,
      'function(const S: string): Integer',
      mlkOverride);
    AddMethod('Insert', @TSepiImportsTStringList.Insert,
      'procedure(Index: Integer; const S: string)',
      mlkOverride);
    AddMethod('InsertObject', @TSepiImportsTStringList.InsertObject,
      'procedure(Index: Integer; const S: string; AObject: TObject )',
      mlkOverride);
    AddMethod('Sort', @TSepiImportsTStringList.Sort,
      'procedure',
      mlkVirtual);
    AddMethod('CustomSort', @TSepiImportsTStringList.CustomSort,
      'procedure(Compare: TStringListSortCompare)',
      mlkVirtual);

    AddProperty('Duplicates', 'property: TDuplicates',
      'FDuplicates', 'FDuplicates');
    AddProperty('Sorted', 'property: Boolean',
      'FSorted', 'SetSorted');
    AddProperty('CaseSensitive', 'property: Boolean',
      'FCaseSensitive', 'SetCaseSensitive');
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OnChanging', 'property: TNotifyEvent',
      'FOnChanging', 'FOnChanging');

    Complete;
  end;
end;

{----------------}
{ TStream import }
{----------------}

function TSepiImportsTStream.GetPosition: Int64;
begin
  Result := Position;
end;

procedure TSepiImportsTStream.SetPosition(const Pos: Int64);
begin
  Position := Pos;
end;

procedure TSepiImportsTStream.SetSize64(const NewSize: Int64);
begin
  Size := NewSize;
end;

class function TSepiImportsTStream.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TStream'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TStream));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetPosition', @TSepiImportsTStream.GetPosition,
      'function: Int64');
    AddMethod('SetPosition', @TSepiImportsTStream.SetPosition,
      'procedure(const Pos: Int64)');
    AddMethod('SetSize64', @TSepiImportsTStream.SetSize64,
      'procedure(const NewSize: Int64)');

    CurrentVisibility := mvProtected;

    AddMethod('GetSize', @TSepiImportsTStream.GetSize,
      'function: Int64',
      mlkVirtual);
    AddOverloadedMethod('SetSize', nil,
      'procedure(NewSize: Longint)',
      mlkVirtual);
    AddOverloadedMethod('SetSize', nil,
      'procedure(const NewSize: Int64)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Read', nil,
      'function(var Buffer; Count: Longint): Longint',
      mlkVirtual, True);
    AddMethod('Write', nil,
      'function(const Buffer; Count: Longint): Longint',
      mlkVirtual, True);
    AddOverloadedMethod('Seek', nil,
      'function(Offset: Longint; Origin: Word): Longint',
      mlkVirtual);
    AddOverloadedMethod('Seek', nil,
      'function(const Offset: Int64; Origin: TSeekOrigin): Int64',
      mlkVirtual);
    AddMethod('ReadBuffer', @TSepiImportsTStream.ReadBuffer,
      'procedure(var Buffer; Count: Longint)');
    AddMethod('WriteBuffer', @TSepiImportsTStream.WriteBuffer,
      'procedure(const Buffer; Count: Longint)');
    AddMethod('CopyFrom', @TSepiImportsTStream.CopyFrom,
      'function(Source: TStream; Count: Int64): Int64');
    AddMethod('ReadComponent', @TSepiImportsTStream.ReadComponent,
      'function(Instance: TComponent): TComponent');
    AddMethod('ReadComponentRes', @TSepiImportsTStream.ReadComponentRes,
      'function(Instance: TComponent): TComponent');
    AddMethod('WriteComponent', @TSepiImportsTStream.WriteComponent,
      'procedure(Instance: TComponent)');
    AddMethod('WriteComponentRes', @TSepiImportsTStream.WriteComponentRes,
      'procedure(const ResName: string; Instance: TComponent)');
    AddMethod('WriteDescendent', @TSepiImportsTStream.WriteDescendent,
      'procedure(Instance, Ancestor: TComponent)');
    AddMethod('WriteDescendentRes', @TSepiImportsTStream.WriteDescendentRes,
      'procedure(const ResName: string; Instance, Ancestor: TComponent)');
    AddMethod('WriteResourceHeader', @TSepiImportsTStream.WriteResourceHeader,
      'procedure(const ResName: string; out FixupInfo: Integer)');
    AddMethod('FixupResourceHeader', @TSepiImportsTStream.FixupResourceHeader,
      'procedure(FixupInfo: Integer)');
    AddMethod('ReadResHeader', @TSepiImportsTStream.ReadResHeader,
      'procedure');

    AddProperty('Position', 'property: Int64',
      'GetPosition', 'SetPosition');
    AddProperty('Size', 'property: Int64',
      'GetSize', 'SetSize64');

    Complete;
  end;
end;

{-----------------------}
{ IStreamPersist import }
{-----------------------}

function SepiImportIStreamPersist(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IStreamPersist));

  with Result do
  begin
    AddMethod('LoadFromStream',
      'procedure(Stream: TStream)', ccRegister);
    AddMethod('SaveToStream',
      'procedure(Stream: TStream)', ccRegister);

    Complete;
  end;
end;

{----------------------}
{ THandleStream import }
{----------------------}

class function TSepiImportsTHandleStream.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(THandleStream));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FHandle', System.TypeInfo(Integer));

    AddOverloadedMethod('SetSize', nil,
      'procedure(NewSize: Longint)',
      mlkOverride);
    AddOverloadedMethod('SetSize', nil,
      'procedure(const NewSize: Int64)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTHandleStream.Create,
      'constructor(AHandle: Integer)');
    AddMethod('Read', @TSepiImportsTHandleStream.Read,
      'function(var Buffer; Count: Longint): Longint',
      mlkOverride);
    AddMethod('Write', @TSepiImportsTHandleStream.Write,
      'function(const Buffer; Count: Longint): Longint',
      mlkOverride);
    AddMethod('Seek', @TSepiImportsTHandleStream.Seek,
      'function(const Offset: Int64; Origin: TSeekOrigin): Int64',
      mlkOverride);

    AddProperty('Handle', 'property: Integer',
      'FHandle', '');

    Complete;
  end;
end;

{--------------------}
{ TFileStream import }
{--------------------}

constructor TSepiImportsTFileStream.Create_0(const FileName: string; Mode: Word);
begin
  Create(FileName, Mode);
end;

constructor TSepiImportsTFileStream.Create_1(const FileName: string; Mode: Word; Rights: Cardinal);
begin
  Create(FileName, Mode, Rights);
end;

class function TSepiImportsTFileStream.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TFileStream));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddOverloadedMethod('Create', @TSepiImportsTFileStream.Create_0,
      'constructor(const FileName: string; Mode: Word)');
    AddOverloadedMethod('Create', @TSepiImportsTFileStream.Create_1,
      'constructor(const FileName: string; Mode: Word; Rights: Cardinal)');
    AddMethod('Destroy', @TSepiImportsTFileStream.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{----------------------------}
{ TCustomMemoryStream import }
{----------------------------}

class function TSepiImportsTCustomMemoryStream.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomMemoryStream));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMemory', 'Pointer');
    AddField('FSize', System.TypeInfo(Longint));
    AddField('FPosition', System.TypeInfo(Longint), True);

    CurrentVisibility := mvProtected;

    AddMethod('SetPointer', @TSepiImportsTCustomMemoryStream.SetPointer,
      'procedure(Ptr: Pointer; Size: Longint)');

    CurrentVisibility := mvPublic;

    AddMethod('Read', @TSepiImportsTCustomMemoryStream.Read,
      'function(var Buffer; Count: Longint): Longint',
      mlkOverride);
    AddMethod('Seek', @TSepiImportsTCustomMemoryStream.Seek,
      'function(Offset: Longint; Origin: Word): Longint',
      mlkOverride);
    AddMethod('SaveToStream', @TSepiImportsTCustomMemoryStream.SaveToStream,
      'procedure(Stream: TStream)');
    AddMethod('SaveToFile', @TSepiImportsTCustomMemoryStream.SaveToFile,
      'procedure(const FileName: string)');

    AddProperty('Memory', 'property: Pointer',
      'FMemory', '');

    Complete;
  end;
end;

{----------------------}
{ TMemoryStream import }
{----------------------}

procedure TSepiImportsTMemoryStream.SetCapacity(NewCapacity: Longint);
begin
  Capacity := NewCapacity;
end;

class function TSepiImportsTMemoryStream.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMemoryStream));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCapacity', System.TypeInfo(Longint));

    AddMethod('SetCapacity', @TSepiImportsTMemoryStream.SetCapacity,
      'procedure(NewCapacity: Longint)');

    CurrentVisibility := mvProtected;

    AddMethod('Realloc', @TSepiImportsTMemoryStream.Realloc,
      'function(var NewCapacity: Longint): Pointer',
      mlkVirtual);

    AddProperty('Capacity', 'property: Longint',
      'FCapacity', 'SetCapacity');

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTMemoryStream.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTMemoryStream.Clear,
      'procedure');
    AddMethod('LoadFromStream', @TSepiImportsTMemoryStream.LoadFromStream,
      'procedure(Stream: TStream)');
    AddMethod('LoadFromFile', @TSepiImportsTMemoryStream.LoadFromFile,
      'procedure(const FileName: string)');
    AddMethod('SetSize', @TSepiImportsTMemoryStream.SetSize,
      'procedure(NewSize: Longint)',
      mlkOverride);
    AddMethod('Write', @TSepiImportsTMemoryStream.Write,
      'function(const Buffer; Count: Longint): Longint',
      mlkOverride);

    Complete;
  end;
end;

{----------------------}
{ TStringStream import }
{----------------------}

class function TSepiImportsTStringStream.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TStringStream));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDataString', System.TypeInfo(string));
    AddField('FPosition', System.TypeInfo(Integer));

    CurrentVisibility := mvProtected;

    AddMethod('SetSize', @TSepiImportsTStringStream.SetSize,
      'procedure(NewSize: Longint)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTStringStream.Create,
      'constructor(const AString: string)');
    AddMethod('Read', @TSepiImportsTStringStream.Read,
      'function(var Buffer; Count: Longint): Longint',
      mlkOverride);
    AddMethod('ReadString', @TSepiImportsTStringStream.ReadString,
      'function(Count: Longint): string');
    AddMethod('Seek', @TSepiImportsTStringStream.Seek,
      'function(Offset: Longint; Origin: Word): Longint',
      mlkOverride);
    AddMethod('Write', @TSepiImportsTStringStream.Write,
      'function(const Buffer; Count: Longint): Longint',
      mlkOverride);
    AddMethod('WriteString', @TSepiImportsTStringStream.WriteString,
      'procedure(const AString: string)');

    AddProperty('DataString', 'property: string',
      'FDataString', '');

    Complete;
  end;
end;

{------------------------}
{ TResourceStream import }
{------------------------}

class function TSepiImportsTResourceStream.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TResourceStream));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('HResInfo', System.TypeInfo(THandle));
    AddField('HGlobal', System.TypeInfo(THandle));

    AddMethod('Initialize', nil,
      'procedure(Instance: THandle; Name, ResType: PChar; FromID: Boolean)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTResourceStream.Create,
      'constructor(Instance: THandle; const ResName: string; ResType: PChar)');
    AddMethod('CreateFromID', @TSepiImportsTResourceStream.CreateFromID,
      'constructor(Instance: THandle; ResID: Integer; ResType: PChar)');
    AddMethod('Destroy', @TSepiImportsTResourceStream.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Write', @TSepiImportsTResourceStream.Write,
      'function(const Buffer; Count: Longint): Longint',
      mlkOverride);

    Complete;
  end;
end;

{-----------------------}
{ TStreamAdapter import }
{-----------------------}

class function TSepiImportsTStreamAdapter.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TStreamAdapter));

  with Result do
  begin
    AddInterface(System.TypeInfo(IStream));

    CurrentVisibility := mvPrivate;

    AddField('FStream', System.TypeInfo(TStream));
    AddField('FOwnership', System.TypeInfo(TStreamOwnership));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTStreamAdapter.Create,
      'constructor(Stream: TStream; Ownership: TStreamOwnership = soReference)');
    AddMethod('Destroy', @TSepiImportsTStreamAdapter.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Read', @TSepiImportsTStreamAdapter.Read,
      'function(pv: Pointer; cb: Longint; pcbRead: PLongint ) : HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('Write', @TSepiImportsTStreamAdapter.Write,
      'function(pv: Pointer; cb: Longint; pcbWritten: PLongint ) : HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('Seek', @TSepiImportsTStreamAdapter.Seek,
      'function(dlibMove: Largeint; dwOrigin: Longint; out libNewPosition: Largeint ) : HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('SetSize', @TSepiImportsTStreamAdapter.SetSize,
      'function(libNewSize: Largeint): HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('CopyTo', @TSepiImportsTStreamAdapter.CopyTo,
      'function(stm: IStream; cb: Largeint; out cbRead: Largeint; out cbWritten: Largeint ) : HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('Commit', @TSepiImportsTStreamAdapter.Commit,
      'function(grfCommitFlags: Longint): HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('Revert', @TSepiImportsTStreamAdapter.Revert,
      'function: HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('LockRegion', @TSepiImportsTStreamAdapter.LockRegion,
      'function(libOffset: Largeint; cb: Largeint; dwLockType: Longint ) : HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('UnlockRegion', @TSepiImportsTStreamAdapter.UnlockRegion,
      'function(libOffset: Largeint; cb: Largeint; dwLockType: Longint ) : HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('Stat', @TSepiImportsTStreamAdapter.Stat,
      'function(out statstg: TStatStg; grfStatFlag: Longint ) : HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('Clone', @TSepiImportsTStreamAdapter.Clone,
      'function(out stm: IStream): HResult',
      mlkVirtual, False, 0, ccStdCall);

    AddProperty('Stream', 'property: TStream',
      'FStream', '');
    AddProperty('StreamOwnership', 'property: TStreamOwnership',
      'FOwnership', 'FOwnership');

    Complete;
  end;
end;

{---------------------}
{ TClassFinder import }
{---------------------}

class function TSepiImportsTClassFinder.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TClassFinder));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FGroups', System.TypeInfo(TList));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTClassFinder.Create,
      'constructor(AClass: TPersistentClass = nil; AIncludeActiveGroups: Boolean = False )');
    AddMethod('Destroy', @TSepiImportsTClassFinder.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('GetClass', @TSepiImportsTClassFinder.GetClass,
      'function(const AClassName: string): TPersistentClass');
    AddMethod('GetClasses', @TSepiImportsTClassFinder.GetClasses,
      'procedure(Proc: TGetClass)');

    Complete;
  end;
end;

{-------------------------------------}
{ IInterfaceComponentReference import }
{-------------------------------------}

function SepiImportIInterfaceComponentReference(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IInterfaceComponentReference));

  with Result do
  begin
    AddMethod('GetComponent',
      'function: TComponent', ccRegister);

    Complete;
  end;
end;

{---------------}
{ TFiler import }
{---------------}

class function TSepiImportsTFiler.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TFiler'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TFiler));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStream', System.TypeInfo(TStream));
    AddField('FBuffer', 'Pointer');
    AddField('FBufSize', System.TypeInfo(Integer));
    AddField('FBufPos', System.TypeInfo(Integer));
    AddField('FBufEnd', System.TypeInfo(Integer));
    AddField('FRoot', System.TypeInfo(TComponent));
    AddField('FLookupRoot', System.TypeInfo(TComponent));
    AddField('FAncestor', System.TypeInfo(TPersistent));
    AddField('FIgnoreChildren', System.TypeInfo(Boolean));

    CurrentVisibility := mvProtected;

    AddMethod('SetRoot', @TSepiImportsTFiler.SetRoot,
      'procedure(Value: TComponent)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTFiler.Create,
      'constructor(Stream: TStream; BufSize: Integer)');
    AddMethod('Destroy', @TSepiImportsTFiler.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('DefineProperty', nil,
      'procedure(const Name: string; ReadData: TReaderProc ; WriteData: TWriterProc ; HasData: Boolean )',
      mlkVirtual, True);
    AddMethod('DefineBinaryProperty', nil,
      'procedure(const Name: string; ReadData, WriteData: TStreamProc ; HasData: Boolean )',
      mlkVirtual, True);
    AddMethod('FlushBuffer', nil,
      'procedure',
      mlkVirtual, True);

    AddProperty('Root', 'property: TComponent',
      'FRoot', 'SetRoot');
    AddProperty('LookupRoot', 'property: TComponent',
      'FLookupRoot', '');
    AddProperty('Ancestor', 'property: TPersistent',
      'FAncestor', 'FAncestor');
    AddProperty('IgnoreChildren', 'property: Boolean',
      'FIgnoreChildren', 'FIgnoreChildren');

    Complete;
  end;
end;

{-----------------------}
{ IVarStreamable import }
{-----------------------}

function SepiImportIVarStreamable(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IVarStreamable));

  with Result do
  begin
    AddMethod('StreamIn',
      'procedure(var Dest: TVarData; const Stream: TStream)', ccRegister);
    AddMethod('StreamOut',
      'procedure(const Source: TVarData; const Stream: TStream)', ccRegister);

    Complete;
  end;
end;

{----------------}
{ TReader import }
{----------------}

function TSepiImportsTReader.GetPosition: Longint;
begin
  Result := Position;
end;

class function TSepiImportsTReader.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TReader'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TReader));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOwner', System.TypeInfo(TComponent));
    AddField('FParent', System.TypeInfo(TComponent));
    AddField('FFixups', System.TypeInfo(TList));
    AddField('FLoaded', System.TypeInfo(TList));
    AddField('FOnFindMethod', System.TypeInfo(TFindMethodEvent));
    AddField('FOnFindMethodInstance', System.TypeInfo(TFindMethodInstanceEvent));
    AddField('FOnSetName', System.TypeInfo(TSetNameEvent));
    AddField('FOnReferenceName', System.TypeInfo(TReferenceNameEvent));
    AddField('FOnAncestorNotFound', System.TypeInfo(TAncestorNotFoundEvent));
    AddField('FOnError', System.TypeInfo(TReaderError));
    AddField('FOnFindComponentClass', System.TypeInfo(TFindComponentClassEvent));
    AddField('FOnCreateComponent', System.TypeInfo(TCreateComponentEvent));
    AddField('FOnFindComponentInstance', System.TypeInfo(TFindComponentInstanceEvent));
    AddField('FPropName', System.TypeInfo(string));
    AddField('FFinder', System.TypeInfo(TClassFinder));
    AddField('FCanHandleExcepts', System.TypeInfo(Boolean));

    AddMethod('DoFixupReferences', nil,
      'procedure');
    AddMethod('FreeFixups', nil,
      'procedure');
    AddMethod('GetFieldClass', nil,
      'function(Instance: TObject; const ClassName: string): TPersistentClass');
    AddMethod('GetPosition', @TSepiImportsTReader.GetPosition,
      'function: Longint');
    AddMethod('ReadBuffer', nil,
      'procedure');
    AddMethod('ReadDataInner', nil,
      'procedure(Instance: TComponent)');
    AddMethod('FindComponentClass', nil,
      'function(const ClassName: string): TComponentClass');

    CurrentVisibility := mvProtected;

    AddMethod('Error', @TSepiImportsTReader.Error,
      'function(const Message: string): Boolean',
      mlkVirtual);
    AddMethod('FindAncestorComponent', @TSepiImportsTReader.FindAncestorComponent,
      'function(const Name: string; ComponentClass: TPersistentClass ) : TComponent',
      mlkVirtual);
    AddMethod('FindMethodInstance', @TSepiImportsTReader.FindMethodInstance,
      'function(Root: TComponent; const MethodName: string): TMethod',
      mlkVirtual);
    AddMethod('FindMethod', @TSepiImportsTReader.FindMethod,
      'function(Root: TComponent; const MethodName: string): Pointer',
      mlkVirtual);
    AddMethod('SetName', @TSepiImportsTReader.SetName,
      'procedure(Component: TComponent; var Name: string)',
      mlkVirtual);
    AddMethod('ReadProperty', @TSepiImportsTReader.ReadProperty,
      'procedure(AInstance: TPersistent)');
    AddMethod('ReadPropValue', @TSepiImportsTReader.ReadPropValue,
      'procedure(Instance: TPersistent; PropInfo: Pointer)');
    AddMethod('ReferenceName', @TSepiImportsTReader.ReferenceName,
      'procedure(var Name: string)',
      mlkVirtual);
    AddMethod('PropertyError', @TSepiImportsTReader.PropertyError,
      'procedure(const Name: string)');
    AddMethod('ReadData', @TSepiImportsTReader.ReadData,
      'procedure(Instance: TComponent)');
    AddMethod('ReadSet', @TSepiImportsTReader.ReadSet,
      'function(SetType: Pointer): Integer');
    AddMethod('SetPosition', @TSepiImportsTReader.SetPosition,
      'procedure(Value: Longint)');
    AddMethod('SkipBytes', @TSepiImportsTReader.SkipBytes,
      'procedure(Count: Integer)');
    AddMethod('SkipSetBody', @TSepiImportsTReader.SkipSetBody,
      'procedure');
    AddMethod('SkipProperty', @TSepiImportsTReader.SkipProperty,
      'procedure');
    AddMethod('SkipComponent', @TSepiImportsTReader.SkipComponent,
      'procedure(SkipHeader: Boolean)');

    AddProperty('PropName', 'property: string',
      'FPropName', '');
    AddProperty('CanHandleExceptions', 'property: Boolean',
      'FCanHandleExcepts', '');

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTReader.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('BeginReferences', @TSepiImportsTReader.BeginReferences,
      'procedure');
    AddMethod('CheckValue', @TSepiImportsTReader.CheckValue,
      'procedure(Value: TValueType)');
    AddMethod('DefineProperty', @TSepiImportsTReader.DefineProperty,
      'procedure(const Name: string; ReadData: TReaderProc ; WriteData: TWriterProc ; HasData: Boolean )',
      mlkOverride);
    AddMethod('DefineBinaryProperty', @TSepiImportsTReader.DefineBinaryProperty,
      'procedure(const Name: string; ReadData, WriteData: TStreamProc ; HasData: Boolean )',
      mlkOverride);
    AddMethod('EndOfList', @TSepiImportsTReader.EndOfList,
      'function: Boolean');
    AddMethod('EndReferences', @TSepiImportsTReader.EndReferences,
      'procedure');
    AddMethod('FixupReferences', @TSepiImportsTReader.FixupReferences,
      'procedure');
    AddMethod('FlushBuffer', @TSepiImportsTReader.FlushBuffer,
      'procedure',
      mlkOverride);
    AddMethod('NextValue', @TSepiImportsTReader.NextValue,
      'function: TValueType');
    AddMethod('Read', @TSepiImportsTReader.Read,
      'procedure(var Buf; Count: Longint)');
    AddMethod('ReadBoolean', @TSepiImportsTReader.ReadBoolean,
      'function: Boolean');
    AddMethod('ReadChar', @TSepiImportsTReader.ReadChar,
      'function: Char');
    AddMethod('ReadWideChar', @TSepiImportsTReader.ReadWideChar,
      'function: WideChar');
    AddMethod('ReadCollection', @TSepiImportsTReader.ReadCollection,
      'procedure(Collection: TCollection)');
    AddMethod('ReadComponent', @TSepiImportsTReader.ReadComponent,
      'function(Component: TComponent): TComponent');
    AddMethod('ReadComponents', @TSepiImportsTReader.ReadComponents,
      'procedure(AOwner, AParent: TComponent; Proc: TReadComponentsProc )');
    AddMethod('ReadFloat', @TSepiImportsTReader.ReadFloat,
      'function: Extended');
    AddMethod('ReadSingle', @TSepiImportsTReader.ReadSingle,
      'function: Single');
    AddMethod('ReadDouble', @TSepiImportsTReader.ReadDouble,
      'function: Double');
    AddMethod('ReadCurrency', @TSepiImportsTReader.ReadCurrency,
      'function: Currency');
    AddMethod('ReadDate', @TSepiImportsTReader.ReadDate,
      'function: TDateTime');
    AddMethod('ReadIdent', @TSepiImportsTReader.ReadIdent,
      'function: string');
    AddMethod('ReadInteger', @TSepiImportsTReader.ReadInteger,
      'function: Longint');
    AddMethod('ReadInt64', @TSepiImportsTReader.ReadInt64,
      'function: Int64');
    AddMethod('ReadListBegin', @TSepiImportsTReader.ReadListBegin,
      'procedure');
    AddMethod('ReadListEnd', @TSepiImportsTReader.ReadListEnd,
      'procedure');
    AddMethod('ReadPrefix', @TSepiImportsTReader.ReadPrefix,
      'procedure(var Flags: TFilerFlags; var AChildPos: Integer)',
      mlkVirtual);
    AddMethod('ReadRootComponent', @TSepiImportsTReader.ReadRootComponent,
      'function(Root: TComponent): TComponent');
    AddMethod('ReadSignature', @TSepiImportsTReader.ReadSignature,
      'procedure');
    AddMethod('ReadStr', @TSepiImportsTReader.ReadStr,
      'function: string');
    AddMethod('ReadString', @TSepiImportsTReader.ReadString,
      'function: string');
    AddMethod('ReadWideString', @TSepiImportsTReader.ReadWideString,
      'function: WideString');
    AddMethod('ReadValue', @TSepiImportsTReader.ReadValue,
      'function: TValueType');
    AddMethod('ReadVariant', @TSepiImportsTReader.ReadVariant,
      'function: Variant');
    AddMethod('CopyValue', @TSepiImportsTReader.CopyValue,
      'procedure(Writer: TWriter)');
    AddMethod('SkipValue', @TSepiImportsTReader.SkipValue,
      'procedure');

    AddProperty('Owner', 'property: TComponent',
      'FOwner', 'FOwner');
    AddProperty('Parent', 'property: TComponent',
      'FParent', 'FParent');
    AddProperty('Position', 'property: Longint',
      'GetPosition', 'SetPosition');
    AddProperty('OnError', 'property: TReaderError',
      'FOnError', 'FOnError');
    AddProperty('OnFindMethod', 'property: TFindMethodEvent',
      'FOnFindMethod', 'FOnFindMethod');
    AddProperty('OnFindMethodInstance', 'property: TFindMethodInstanceEvent',
      'FOnFindMethodInstance', 'FOnFindMethodInstance');
    AddProperty('OnSetName', 'property: TSetNameEvent',
      'FOnSetName', 'FOnSetName');
    AddProperty('OnReferenceName', 'property: TReferenceNameEvent',
      'FOnReferenceName', 'FOnReferenceName');
    AddProperty('OnAncestorNotFound', 'property: TAncestorNotFoundEvent',
      'FOnAncestorNotFound', 'FOnAncestorNotFound');
    AddProperty('OnCreateComponent', 'property: TCreateComponentEvent',
      'FOnCreateComponent', 'FOnCreateComponent');
    AddProperty('OnFindComponentClass', 'property: TFindComponentClassEvent',
      'FOnFindComponentClass', 'FOnFindComponentClass');
    AddProperty('OnFindComponentInstance', 'property: TFindComponentInstanceEvent',
      'FOnFindComponentInstance', 'FOnFindComponentInstance');

    Complete;
  end;
end;

{----------------}
{ TWriter import }
{----------------}

function TSepiImportsTWriter.GetPosition: Longint;
begin
  Result := Position;
end;

procedure TSepiImportsTWriter.SetPosition(Value: Longint);
begin
  Position := Value;
end;

procedure TSepiImportsTWriter.WriteInteger_0(Value: Longint);
begin
  WriteInteger(Value);
end;

procedure TSepiImportsTWriter.WriteInteger_1(Value: Int64);
begin
  WriteInteger(Value);
end;

class function TSepiImportsTWriter.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TWriter'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TWriter));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FRootAncestor', System.TypeInfo(TComponent));
    AddField('FPropPath', System.TypeInfo(string));
    AddField('FAncestorList', System.TypeInfo(TList));
    AddField('FAncestorPos', System.TypeInfo(Integer));
    AddField('FChildPos', System.TypeInfo(Integer));
    AddField('FOnFindAncestor', System.TypeInfo(TFindAncestorEvent));
    AddField('FOnFindMethodName', System.TypeInfo(TFindMethodNameEvent));
    AddField('FUseQualifiedNames', System.TypeInfo(Boolean));

    AddMethod('AddAncestor', nil,
      'procedure(Component: TComponent)');
    AddMethod('GetPosition', @TSepiImportsTWriter.GetPosition,
      'function: Longint');
    AddMethod('SetPosition', @TSepiImportsTWriter.SetPosition,
      'procedure(Value: Longint)');
    AddMethod('WriteBuffer', nil,
      'procedure');
    AddMethod('WriteData', nil,
      'procedure(Instance: TComponent)',
      mlkVirtual);
    AddMethod('WriteMinStr', nil,
      'procedure(const LocaleStr: string; const UTF8Str: UTF8String)');
    AddMethod('GetLookupInfo', nil,
      'procedure(var Ancestor: TPersistent; var Root, LookupRoot, RootAncestor: TComponent )');

    CurrentVisibility := mvProtected;

    AddMethod('FindMethodName', @TSepiImportsTWriter.FindMethodName,
      'function(Method: TMethod): string',
      mlkVirtual);
    AddMethod('SetRoot', @TSepiImportsTWriter.SetRoot,
      'procedure(Value: TComponent)',
      mlkOverride);
    AddMethod('WriteBinary', @TSepiImportsTWriter.WriteBinary,
      'procedure(WriteData: TStreamProc)');
    AddMethod('WritePrefix', @TSepiImportsTWriter.WritePrefix,
      'procedure(Flags: TFilerFlags; AChildPos: Integer)');
    AddMethod('WriteProperty', @TSepiImportsTWriter.WriteProperty,
      'procedure(Instance: TPersistent; PropInfo: PPropInfo)');
    AddMethod('WriteProperties', @TSepiImportsTWriter.WriteProperties,
      'procedure(Instance: TPersistent)');
    AddMethod('WritePropName', @TSepiImportsTWriter.WritePropName,
      'procedure(const PropName: string)');
    AddMethod('WriteValue', @TSepiImportsTWriter.WriteValue,
      'procedure(Value: TValueType)');

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTWriter.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('DefineProperty', @TSepiImportsTWriter.DefineProperty,
      'procedure(const Name: string; ReadData: TReaderProc ; WriteData: TWriterProc ; HasData: Boolean )',
      mlkOverride);
    AddMethod('DefineBinaryProperty', @TSepiImportsTWriter.DefineBinaryProperty,
      'procedure(const Name: string; ReadData, WriteData: TStreamProc ; HasData: Boolean )',
      mlkOverride);
    AddMethod('FlushBuffer', @TSepiImportsTWriter.FlushBuffer,
      'procedure',
      mlkOverride);
    AddMethod('Write', @TSepiImportsTWriter.Write,
      'procedure(const Buf; Count: Longint)');
    AddMethod('WriteBoolean', @TSepiImportsTWriter.WriteBoolean,
      'procedure(Value: Boolean)');
    AddMethod('WriteCollection', @TSepiImportsTWriter.WriteCollection,
      'procedure(Value: TCollection)');
    AddMethod('WriteComponent', @TSepiImportsTWriter.WriteComponent,
      'procedure(Component: TComponent)');
    AddMethod('WriteChar', @TSepiImportsTWriter.WriteChar,
      'procedure(Value: Char)');
    AddMethod('WriteWideChar', @TSepiImportsTWriter.WriteWideChar,
      'procedure(Value: WideChar)');
    AddMethod('WriteDescendent', @TSepiImportsTWriter.WriteDescendent,
      'procedure(Root: TComponent; AAncestor: TComponent)');
    AddMethod('WriteFloat', @TSepiImportsTWriter.WriteFloat,
      'procedure(const Value: Extended)');
    AddMethod('WriteSingle', @TSepiImportsTWriter.WriteSingle,
      'procedure(const Value: Single)');
    AddMethod('WriteDouble', @TSepiImportsTWriter.WriteDouble,
      'procedure(const Value: Double)');
    AddMethod('WriteCurrency', @TSepiImportsTWriter.WriteCurrency,
      'procedure(const Value: Currency)');
    AddMethod('WriteDate', @TSepiImportsTWriter.WriteDate,
      'procedure(const Value: TDateTime)');
    AddMethod('WriteIdent', @TSepiImportsTWriter.WriteIdent,
      'procedure(const Ident: string)');
    AddOverloadedMethod('WriteInteger', @TSepiImportsTWriter.WriteInteger_0,
      'procedure(Value: Longint)');
    AddOverloadedMethod('WriteInteger', @TSepiImportsTWriter.WriteInteger_1,
      'procedure(Value: Int64)');
    AddMethod('WriteListBegin', @TSepiImportsTWriter.WriteListBegin,
      'procedure');
    AddMethod('WriteListEnd', @TSepiImportsTWriter.WriteListEnd,
      'procedure');
    AddMethod('WriteRootComponent', @TSepiImportsTWriter.WriteRootComponent,
      'procedure(Root: TComponent)');
    AddMethod('WriteSignature', @TSepiImportsTWriter.WriteSignature,
      'procedure');
    AddMethod('WriteStr', @TSepiImportsTWriter.WriteStr,
      'procedure(const Value: string)');
    AddMethod('WriteString', @TSepiImportsTWriter.WriteString,
      'procedure(const Value: string)');
    AddMethod('WriteWideString', @TSepiImportsTWriter.WriteWideString,
      'procedure(const Value: WideString)');
    AddMethod('WriteVariant', @TSepiImportsTWriter.WriteVariant,
      'procedure(const Value: Variant)');

    AddProperty('Position', 'property: Longint',
      'GetPosition', 'SetPosition');
    AddProperty('RootAncestor', 'property: TComponent',
      'FRootAncestor', 'FRootAncestor');
    AddProperty('OnFindAncestor', 'property: TFindAncestorEvent',
      'FOnFindAncestor', 'FOnFindAncestor');
    AddProperty('OnFindMethodName', 'property: TFindMethodNameEvent',
      'FOnFindMethodName', 'FOnFindMethodName');
    AddProperty('UseQualifiedNames', 'property: Boolean',
      'FUseQualifiedNames', 'FUseQualifiedNames');

    Complete;
  end;
end;

{----------------}
{ TParser import }
{----------------}

class function TSepiImportsTParser.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TParser));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStream', System.TypeInfo(TStream));
    AddField('FOrigin', System.TypeInfo(Longint));
    AddField('FBuffer', 'PChar');
    AddField('FBufPtr', 'PChar');
    AddField('FBufEnd', 'PChar');
    AddField('FSourcePtr', 'PChar');
    AddField('FSourceEnd', 'PChar');
    AddField('FTokenPtr', 'PChar');
    AddField('FStringPtr', 'PChar');
    AddField('FSourceLine', System.TypeInfo(Integer));
    AddField('FSaveChar', System.TypeInfo(Char));
    AddField('FToken', System.TypeInfo(Char));
    AddField('FFloatType', System.TypeInfo(Char));
    AddField('FWideStr', System.TypeInfo(WideString));
    AddField('FOnError', System.TypeInfo(TParserErrorEvent));

    AddMethod('ReadBuffer', nil,
      'procedure');
    AddMethod('SkipBlanks', nil,
      'procedure');

    CurrentVisibility := mvProtected;

    AddMethod('GetLinePos', @TSepiImportsTParser.GetLinePos,
      'function: Integer');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTParser.Create,
      'constructor(Stream: TStream; AOnError: TParserErrorEvent = nil)');
    AddMethod('Destroy', @TSepiImportsTParser.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('CheckToken', @TSepiImportsTParser.CheckToken,
      'procedure(T: Char)');
    AddMethod('CheckTokenSymbol', @TSepiImportsTParser.CheckTokenSymbol,
      'procedure(const S: string)');
    AddMethod('Error', @TSepiImportsTParser.Error,
      'procedure(const Ident: string)');
    AddMethod('ErrorFmt', @TSepiImportsTParser.ErrorFmt,
      'procedure(const Ident: string; const Args: array of const)');
    AddMethod('ErrorStr', @TSepiImportsTParser.ErrorStr,
      'procedure(const Message: string)');
    AddMethod('HexToBinary', @TSepiImportsTParser.HexToBinary,
      'procedure(Stream: TStream)');
    AddMethod('NextToken', @TSepiImportsTParser.NextToken,
      'function: Char');
    AddMethod('SourcePos', @TSepiImportsTParser.SourcePos,
      'function: Longint');
    AddMethod('TokenComponentIdent', @TSepiImportsTParser.TokenComponentIdent,
      'function: string');
    AddMethod('TokenFloat', @TSepiImportsTParser.TokenFloat,
      'function: Extended');
    AddMethod('TokenInt', @TSepiImportsTParser.TokenInt,
      'function: Int64');
    AddMethod('TokenString', @TSepiImportsTParser.TokenString,
      'function: string');
    AddMethod('TokenWideString', @TSepiImportsTParser.TokenWideString,
      'function: WideString');
    AddMethod('TokenSymbolIs', @TSepiImportsTParser.TokenSymbolIs,
      'function(const S: string): Boolean');

    AddProperty('FloatType', 'property: Char',
      'FFloatType', '');
    AddProperty('SourceLine', 'property: Integer',
      'FSourceLine', '');
    AddProperty('LinePos', 'property: Integer',
      'GetLinePos', '');
    AddProperty('Token', 'property: Char',
      'FToken', '');
    AddProperty('OnError', 'property: TParserErrorEvent',
      'FOnError', 'FOnError');

    Complete;
  end;
end;

{----------------}
{ EThread import }
{----------------}

class function TSepiImportsEThread.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EThread));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------------}
{ TSynchronizeRecord import }
{---------------------------}

function SepiImportTSynchronizeRecord(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TSynchronizeRecord', False, True);

  with Result do
  begin
    AddField('FThread', System.TypeInfo(TObject));
    AddField('FMethod', System.TypeInfo(TThreadMethod));
    AddField('FSynchronizeException', System.TypeInfo(TObject));

    Complete;
  end;
end;

{----------------}
{ TThread import }
{----------------}

function TSepiImportsTThread.GetPriority: TThreadPriority;
begin
  Result := Priority;
end;

procedure TSepiImportsTThread.SetPriority(Value: TThreadPriority);
begin
  Priority := Value;
end;

procedure TSepiImportsTThread.SetSuspended(Value: Boolean);
begin
  Suspended := Value;
end;

procedure TSepiImportsTThread.CheckThreadError_0(ErrCode: Integer);
begin
  CheckThreadError(ErrCode);
end;

procedure TSepiImportsTThread.CheckThreadError_1(Success: Boolean);
begin
  CheckThreadError(Success);
end;

procedure TSepiImportsTThread.Queue_0(AMethod: TThreadMethod);
begin
  Queue(AMethod);
end;

procedure TSepiImportsTThread.Synchronize_1(AMethod: TThreadMethod);
begin
  Synchronize(AMethod);
end;

class procedure TSepiImportsTThread.Queue_1(AThread: TThread; AMethod: TThreadMethod);
begin
  Queue(AThread, AMethod);
end;

class procedure TSepiImportsTThread.Synchronize_2(AThread: TThread; AMethod: TThreadMethod);
begin
  Synchronize(AThread, AMethod);
end;

class function TSepiImportsTThread.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TThread));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHandle', System.TypeInfo(THandle));
    AddField('FThreadID', System.TypeInfo(THandle));
    AddField('FCreateSuspended', System.TypeInfo(Boolean));
    AddField('FTerminated', System.TypeInfo(Boolean));
    AddField('FSuspended', System.TypeInfo(Boolean));
    AddField('FFreeOnTerminate', System.TypeInfo(Boolean));
    AddField('FFinished', System.TypeInfo(Boolean));
    AddField('FReturnValue', System.TypeInfo(Integer));
    AddField('FOnTerminate', System.TypeInfo(TNotifyEvent));
    AddField('FSynchronize', 'TSynchronizeRecord');
    AddField('FFatalException', System.TypeInfo(TObject));

    AddMethod('CallOnTerminate', nil,
      'procedure');
    AddOverloadedMethod('Synchronize', nil,
      'class procedure(ASyncRec: PSynchronizeRecord; QueueEvent: Boolean = False)');
    AddMethod('GetPriority', @TSepiImportsTThread.GetPriority,
      'function: TThreadPriority');
    AddMethod('SetPriority', @TSepiImportsTThread.SetPriority,
      'procedure(Value: TThreadPriority)');
    AddMethod('SetSuspended', @TSepiImportsTThread.SetSuspended,
      'procedure(Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddOverloadedMethod('CheckThreadError', @TSepiImportsTThread.CheckThreadError_0,
      'procedure(ErrCode: Integer)');
    AddOverloadedMethod('CheckThreadError', @TSepiImportsTThread.CheckThreadError_1,
      'procedure(Success: Boolean)');
    AddMethod('DoTerminate', @TSepiImportsTThread.DoTerminate,
      'procedure',
      mlkVirtual);
    AddMethod('Execute', nil,
      'procedure',
      mlkVirtual, True);
    AddOverloadedMethod('Queue', @TSepiImportsTThread.Queue_0,
      'procedure(AMethod: TThreadMethod)');
    AddOverloadedMethod('Synchronize', @TSepiImportsTThread.Synchronize_1,
      'procedure(AMethod: TThreadMethod)');

    AddProperty('ReturnValue', 'property: Integer',
      'FReturnValue', 'FReturnValue');
    AddProperty('Terminated', 'property: Boolean',
      'FTerminated', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTThread.Create,
      'constructor(CreateSuspended: Boolean)');
    AddMethod('Destroy', @TSepiImportsTThread.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AfterConstruction', @TSepiImportsTThread.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('Resume', @TSepiImportsTThread.Resume,
      'procedure');
    AddMethod('Suspend', @TSepiImportsTThread.Suspend,
      'procedure');
    AddMethod('Terminate', @TSepiImportsTThread.Terminate,
      'procedure');
    AddMethod('WaitFor', @TSepiImportsTThread.WaitFor,
      'function: LongWord');
    AddOverloadedMethod('Queue', @TSepiImportsTThread.Queue_1,
      'class procedure(AThread: TThread; AMethod: TThreadMethod)');
    AddMethod('RemoveQueuedEvents', @TSepiImportsTThread.RemoveQueuedEvents,
      'class procedure(AThread: TThread; AMethod: TThreadMethod)');
    AddMethod('StaticQueue', @TSepiImportsTThread.StaticQueue,
      'class procedure(AThread: TThread; AMethod: TThreadMethod)');
    AddOverloadedMethod('Synchronize', @TSepiImportsTThread.Synchronize_2,
      'class procedure(AThread: TThread; AMethod: TThreadMethod)');
    AddMethod('StaticSynchronize', @TSepiImportsTThread.StaticSynchronize,
      'class procedure(AThread: TThread; AMethod: TThreadMethod)');

    AddProperty('FatalException', 'property: TObject',
      'FFatalException', '');
    AddProperty('FreeOnTerminate', 'property: Boolean',
      'FFreeOnTerminate', 'FFreeOnTerminate');
    AddProperty('Handle', 'property: THandle',
      'FHandle', '');
    AddProperty('Priority', 'property: TThreadPriority',
      'GetPriority', 'SetPriority');
    AddProperty('Suspended', 'property: Boolean',
      'FSuspended', 'SetSuspended');
    AddProperty('ThreadID', 'property: THandle',
      'FThreadID', '');
    AddProperty('OnTerminate', 'property: TNotifyEvent',
      'FOnTerminate', 'FOnTerminate');

    Complete;
  end;
end;

{-----------------------------}
{ TComponentEnumerator import }
{-----------------------------}

class function TSepiImportsTComponentEnumerator.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComponentEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FComponent', System.TypeInfo(TComponent));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTComponentEnumerator.Create,
      'constructor(AComponent: TComponent)');
    AddMethod('GetCurrent', @TSepiImportsTComponentEnumerator.GetCurrent,
      'function: TComponent');
    AddMethod('MoveNext', @TSepiImportsTComponentEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: TComponent',
      'GetCurrent', '');

    Complete;
  end;
end;

{----------------------}
{ IVCLComObject import }
{----------------------}

function SepiImportIVCLComObject(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IVCLComObject));

  with Result do
  begin
    AddMethod('GetTypeInfoCount',
      'function(out Count: Integer): HResult', ccStdCall);
    AddMethod('GetTypeInfo',
      'function(Index, LocaleID: Integer; out TypeInfo): HResult', ccStdCall);
    AddMethod('GetIDsOfNames',
      'function(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer ; DispIDs: Pointer ) : HResult', ccStdCall);
    AddMethod('Invoke',
      'function(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word ; var Params ; VarResult, ExcepInfo, ArgErr: Pointer ) : HResult', ccStdCall);
    AddMethod('SafeCallException',
      'function(ExceptObject: TObject; ExceptAddr: Pointer ) : HResult', ccRegister);
    AddMethod('FreeOnRelease',
      'procedure', ccRegister);

    Complete;
  end;
end;

{------------------------}
{ IDesignerNotify import }
{------------------------}

function SepiImportIDesignerNotify(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDesignerNotify));

  with Result do
  begin
    AddMethod('Modified',
      'procedure', ccRegister);
    AddMethod('Notification',
      'procedure(AnObject: TPersistent; Operation: TOperation)', ccRegister);

    Complete;
  end;
end;

{-------------------}
{ TComponent import }
{-------------------}

function TSepiImportsTComponent.GetComObject: IUnknown;
begin
  Result := ComObject;
end;

function TSepiImportsTComponent.GetComponent(AIndex: Integer): TComponent;
begin
  Result := Components[AIndex];
end;

function TSepiImportsTComponent.GetComponentCount: Integer;
begin
  Result := ComponentCount;
end;

function TSepiImportsTComponent.GetComponentIndex: Integer;
begin
  Result := ComponentIndex;
end;

procedure TSepiImportsTComponent.SetComponentIndex(Value: Integer);
begin
  ComponentIndex := Value;
end;

class function TSepiImportsTComponent.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TComponent'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TComponent));

  with Result do
  begin
    AddInterface(System.TypeInfo(IInterface));
    AddInterface(System.TypeInfo(IInterfaceComponentReference));

    CurrentVisibility := mvPrivate;

    AddField('FOwner', System.TypeInfo(TComponent));
    AddField('FName', System.TypeInfo(TComponentName));
    AddField('FTag', System.TypeInfo(Longint));
    AddField('FComponents', System.TypeInfo(TList));
    AddField('FFreeNotifies', System.TypeInfo(TList));
    AddField('FDesignInfo', System.TypeInfo(Longint));
    AddField('FComponentState', System.TypeInfo(TComponentState));
    AddField('FVCLComObject', 'Pointer');

    AddMethod('GetComObject', @TSepiImportsTComponent.GetComObject,
      'function: IUnknown');
    AddMethod('GetComponent', @TSepiImportsTComponent.GetComponent,
      'function(AIndex: Integer): TComponent');
    AddMethod('GetComponentCount', @TSepiImportsTComponent.GetComponentCount,
      'function: Integer');
    AddMethod('GetComponentIndex', @TSepiImportsTComponent.GetComponentIndex,
      'function: Integer');
    AddMethod('Insert', nil,
      'procedure(AComponent: TComponent)');
    AddMethod('ReadLeft', nil,
      'procedure(Reader: TReader)');
    AddMethod('ReadTop', nil,
      'procedure(Reader: TReader)');
    AddMethod('Remove', nil,
      'procedure(AComponent: TComponent)');
    AddMethod('RemoveNotification', nil,
      'procedure(AComponent: TComponent)');
    AddMethod('SetComponentIndex', @TSepiImportsTComponent.SetComponentIndex,
      'procedure(Value: Integer)');
    AddMethod('SetReference', nil,
      'procedure(Enable: Boolean)');
    AddMethod('WriteLeft', nil,
      'procedure(Writer: TWriter)');
    AddMethod('WriteTop', nil,
      'procedure(Writer: TWriter)');


    AddMethod('IntfGetComponent', nil,
      'function: TComponent');

    CurrentVisibility := mvProtected;

    AddField('FComponentStyle', System.TypeInfo(TComponentStyle));

    AddMethod('ChangeName', @TSepiImportsTComponent.ChangeName,
      'procedure(const NewName: TComponentName)');
    AddMethod('DefineProperties', @TSepiImportsTComponent.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('GetChildren', @TSepiImportsTComponent.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkDynamic);
    AddMethod('GetChildOwner', @TSepiImportsTComponent.GetChildOwner,
      'function: TComponent',
      mlkDynamic);
    AddMethod('GetChildParent', @TSepiImportsTComponent.GetChildParent,
      'function: TComponent',
      mlkDynamic);
    AddMethod('GetOwner', @TSepiImportsTComponent.GetOwner,
      'function: TPersistent',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTComponent.Loaded,
      'procedure',
      mlkVirtual);
    AddMethod('Notification', @TSepiImportsTComponent.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation )',
      mlkVirtual);
    AddMethod('PaletteCreated', @TSepiImportsTComponent.PaletteCreated,
      'procedure',
      mlkDynamic);
    AddMethod('ReadState', @TSepiImportsTComponent.ReadState,
      'procedure(Reader: TReader)',
      mlkVirtual);
    AddMethod('SetAncestor', @TSepiImportsTComponent.SetAncestor,
      'procedure(Value: Boolean)');
    AddMethod('SetDesigning', @TSepiImportsTComponent.SetDesigning,
      'procedure(Value: Boolean; SetChildren: Boolean = True)');
    AddMethod('SetInline', @TSepiImportsTComponent.SetInline,
      'procedure(Value: Boolean)');
    AddMethod('SetDesignInstance', @TSepiImportsTComponent.SetDesignInstance,
      'procedure(Value: Boolean)');
    AddMethod('SetName', @TSepiImportsTComponent.SetName,
      'procedure(const NewName: TComponentName)',
      mlkVirtual);
    AddMethod('SetChildOrder', @TSepiImportsTComponent.SetChildOrder,
      'procedure(Child: TComponent; Order: Integer)',
      mlkDynamic);
    AddMethod('SetParentComponent', @TSepiImportsTComponent.SetParentComponent,
      'procedure(Value: TComponent)',
      mlkDynamic);
    AddMethod('Updating', @TSepiImportsTComponent.Updating,
      'procedure',
      mlkDynamic);
    AddMethod('Updated', @TSepiImportsTComponent.Updated,
      'procedure',
      mlkDynamic);
    AddMethod('UpdateRegistry', @TSepiImportsTComponent.UpdateRegistry,
      'class procedure(Register: Boolean; const ClassID, ProgID: string)',
      mlkVirtual);
    AddMethod('ValidateRename', @TSepiImportsTComponent.ValidateRename,
      'procedure(AComponent: TComponent; const CurName, NewName: string )',
      mlkVirtual);
    AddMethod('ValidateContainer', @TSepiImportsTComponent.ValidateContainer,
      'procedure(AComponent: TComponent)',
      mlkDynamic);
    AddMethod('ValidateInsert', @TSepiImportsTComponent.ValidateInsert,
      'procedure(AComponent: TComponent)',
      mlkDynamic);
    AddMethod('WriteState', @TSepiImportsTComponent.WriteState,
      'procedure(Writer: TWriter)',
      mlkVirtual);
    AddMethod('QueryInterface', @TSepiImportsTComponent.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      mlkVirtual, False, 0, ccStdCall);
    AddMethod('_AddRef', @TSepiImportsTComponent._AddRef,
      'function: Integer',
      ccStdCall);
    AddMethod('_Release', @TSepiImportsTComponent._Release,
      'function: Integer',
      ccStdCall);
    AddMethod('GetTypeInfoCount', @TSepiImportsTComponent.GetTypeInfoCount,
      'function(out Count: Integer): HResult',
      ccStdCall);
    AddMethod('GetTypeInfo', @TSepiImportsTComponent.GetTypeInfo,
      'function(Index, LocaleID: Integer; out TypeInfo): HResult',
      ccStdCall);
    AddMethod('GetIDsOfNames', @TSepiImportsTComponent.GetIDsOfNames,
      'function(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer ; DispIDs: Pointer ) : HResult',
      ccStdCall);
    AddMethod('Invoke', @TSepiImportsTComponent.Invoke,
      'function(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word ; var Params ; VarResult, ExcepInfo, ArgErr: Pointer ) : HResult',
      ccStdCall);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTComponent.Create,
      'constructor(AOwner: TComponent)',
      mlkVirtual);
    AddMethod('Destroy', @TSepiImportsTComponent.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('BeforeDestruction', @TSepiImportsTComponent.BeforeDestruction,
      'procedure',
      mlkOverride);
    AddMethod('DestroyComponents', @TSepiImportsTComponent.DestroyComponents,
      'procedure');
    AddMethod('Destroying', @TSepiImportsTComponent.Destroying,
      'procedure');
    AddMethod('ExecuteAction', @TSepiImportsTComponent.ExecuteAction,
      'function(Action: TBasicAction): Boolean',
      mlkDynamic);
    AddMethod('FindComponent', @TSepiImportsTComponent.FindComponent,
      'function(const AName: string): TComponent');
    AddMethod('FreeNotification', @TSepiImportsTComponent.FreeNotification,
      'procedure(AComponent: TComponent)');
    AddMethod('RemoveFreeNotification', @TSepiImportsTComponent.RemoveFreeNotification,
      'procedure(AComponent: TComponent)');
    AddMethod('FreeOnRelease', @TSepiImportsTComponent.FreeOnRelease,
      'procedure');
    AddMethod('GetEnumerator', @TSepiImportsTComponent.GetEnumerator,
      'function: TComponentEnumerator');
    AddMethod('GetParentComponent', @TSepiImportsTComponent.GetParentComponent,
      'function: TComponent',
      mlkDynamic);
    AddMethod('GetNamePath', @TSepiImportsTComponent.GetNamePath,
      'function: string',
      mlkOverride);
    AddMethod('HasParent', @TSepiImportsTComponent.HasParent,
      'function: Boolean',
      mlkDynamic);
    AddMethod('InsertComponent', @TSepiImportsTComponent.InsertComponent,
      'procedure(AComponent: TComponent)');
    AddMethod('RemoveComponent', @TSepiImportsTComponent.RemoveComponent,
      'procedure(AComponent: TComponent)');
    AddMethod('SetSubComponent', @TSepiImportsTComponent.SetSubComponent,
      'procedure(IsSubComponent: Boolean)');
    AddMethod('SafeCallException', @TSepiImportsTComponent.SafeCallException,
      'function(ExceptObject: TObject; ExceptAddr: Pointer ) : HResult',
      mlkOverride);
    AddMethod('UpdateAction', @TSepiImportsTComponent.UpdateAction,
      'function(Action: TBasicAction): Boolean',
      mlkDynamic);
    AddMethod('IsImplementorOf', @TSepiImportsTComponent.IsImplementorOf,
      'function(const I: IInterface): Boolean');
    AddMethod('ReferenceInterface', @TSepiImportsTComponent.ReferenceInterface,
      'function(const I: IInterface; Operation: TOperation): Boolean');

    AddProperty('ComObject', 'property: IUnknown',
      'GetComObject', '');
    AddProperty('Components', 'property[Index: Integer]: TComponent',
      'GetComponent', '');
    AddProperty('ComponentCount', 'property: Integer',
      'GetComponentCount', '');
    AddProperty('ComponentIndex', 'property: Integer',
      'GetComponentIndex', 'SetComponentIndex');
    AddProperty('ComponentState', 'property: TComponentState',
      'FComponentState', '');
    AddProperty('ComponentStyle', 'property: TComponentStyle',
      'FComponentStyle', '');
    AddProperty('DesignInfo', 'property: Longint',
      'FDesignInfo', 'FDesignInfo');
    AddProperty('Owner', 'property: TComponent',
      'FOwner', '');
    AddProperty('VCLComObject', 'property: Pointer',
      'FVCLComObject', 'FVCLComObject');

    CurrentVisibility := mvPublished;

    AddProperty('Name', 'property: TComponentName',
      'FName', 'SetName',
      NoIndex, NoDefaultValue, 'False');
    AddProperty('Tag', 'property: Longint',
      'FTag', 'FTag',
      NoIndex, 0);

    Complete;
  end;
end;

{-------------------------}
{ TBasicActionLink import }
{-------------------------}

class function TSepiImportsTBasicActionLink.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBasicActionLink));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOnChange', System.TypeInfo(TNotifyEvent));

    CurrentVisibility := mvProtected;

    AddField('FAction', System.TypeInfo(TBasicAction));

    AddMethod('AssignClient', @TSepiImportsTBasicActionLink.AssignClient,
      'procedure(AClient: TObject)',
      mlkVirtual);
    AddMethod('Change', @TSepiImportsTBasicActionLink.Change,
      'procedure',
      mlkVirtual);
    AddMethod('IsOnExecuteLinked', @TSepiImportsTBasicActionLink.IsOnExecuteLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('SetAction', @TSepiImportsTBasicActionLink.SetAction,
      'procedure(Value: TBasicAction)',
      mlkVirtual);
    AddMethod('SetOnExecute', @TSepiImportsTBasicActionLink.SetOnExecute,
      'procedure(Value: TNotifyEvent)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBasicActionLink.Create,
      'constructor(AClient: TObject)',
      mlkVirtual);
    AddMethod('Destroy', @TSepiImportsTBasicActionLink.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Execute', @TSepiImportsTBasicActionLink.Execute,
      'function(AComponent: TComponent = nil): Boolean',
      mlkVirtual);
    AddMethod('Update', @TSepiImportsTBasicActionLink.Update,
      'function: Boolean',
      mlkVirtual);

    AddProperty('Action', 'property: TBasicAction',
      'FAction', 'SetAction');
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');

    Complete;
  end;
end;

{---------------------}
{ TBasicAction import }
{---------------------}

procedure TSepiImportsTBasicAction.SetActionComponent(const Value: TComponent);
begin
  ActionComponent := Value;
end;

class function TSepiImportsTBasicAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TBasicAction'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TBasicAction));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FActionComponent', System.TypeInfo(TComponent));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnExecute', System.TypeInfo(TNotifyEvent));
    AddField('FOnUpdate', System.TypeInfo(TNotifyEvent));

    AddMethod('SetActionComponent', @TSepiImportsTBasicAction.SetActionComponent,
      'procedure(const Value: TComponent)');

    CurrentVisibility := mvProtected;

    AddField('FClients', System.TypeInfo(TList));

    AddMethod('Change', @TSepiImportsTBasicAction.Change,
      'procedure',
      mlkVirtual);
    AddMethod('SetOnExecute', @TSepiImportsTBasicAction.SetOnExecute,
      'procedure(Value: TNotifyEvent)',
      mlkVirtual);

    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');

    AddMethod('Notification', @TSepiImportsTBasicAction.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBasicAction.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTBasicAction.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('HandlesTarget', @TSepiImportsTBasicAction.HandlesTarget,
      'function(Target: TObject): Boolean',
      mlkVirtual);
    AddMethod('UpdateTarget', @TSepiImportsTBasicAction.UpdateTarget,
      'procedure(Target: TObject)',
      mlkVirtual);
    AddMethod('ExecuteTarget', @TSepiImportsTBasicAction.ExecuteTarget,
      'procedure(Target: TObject)',
      mlkVirtual);
    AddMethod('Execute', @TSepiImportsTBasicAction.Execute,
      'function: Boolean',
      mlkDynamic);
    AddMethod('RegisterChanges', @TSepiImportsTBasicAction.RegisterChanges,
      'procedure(Value: TBasicActionLink)');
    AddMethod('UnRegisterChanges', @TSepiImportsTBasicAction.UnRegisterChanges,
      'procedure(Value: TBasicActionLink)');
    AddMethod('Update', @TSepiImportsTBasicAction.Update,
      'function: Boolean',
      mlkVirtual);

    AddProperty('ActionComponent', 'property: TComponent',
      'FActionComponent', 'SetActionComponent');
    AddProperty('OnExecute', 'property: TNotifyEvent',
      'FOnExecute', 'SetOnExecute');
    AddProperty('OnUpdate', 'property: TNotifyEvent',
      'FOnUpdate', 'FOnUpdate');

    Complete;
  end;
end;

{--------------------}
{ TDataModule import }
{--------------------}

class function TSepiImportsTDataModule.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDataModule));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDesignSize', 'TPoint');
    AddField('FDesignOffset', 'TPoint');
    AddField('FOnCreate', System.TypeInfo(TNotifyEvent));
    AddField('FOnDestroy', System.TypeInfo(TNotifyEvent));
    AddField('FOldCreateOrder', System.TypeInfo(Boolean));

    AddMethod('ReadHeight', nil,
      'procedure(Reader: TReader)');
    AddMethod('ReadHorizontalOffset', nil,
      'procedure(Reader: TReader)');
    AddMethod('ReadVerticalOffset', nil,
      'procedure(Reader: TReader)');
    AddMethod('ReadWidth', nil,
      'procedure(Reader: TReader)');
    AddMethod('WriteWidth', nil,
      'procedure(Writer: TWriter)');
    AddMethod('WriteHorizontalOffset', nil,
      'procedure(Writer: TWriter)');
    AddMethod('WriteVerticalOffset', nil,
      'procedure(Writer: TWriter)');
    AddMethod('WriteHeight', nil,
      'procedure(Writer: TWriter)');

    CurrentVisibility := mvProtected;

    AddMethod('DoCreate', @TSepiImportsTDataModule.DoCreate,
      'procedure',
      mlkVirtual);
    AddMethod('DoDestroy', @TSepiImportsTDataModule.DoDestroy,
      'procedure',
      mlkVirtual);
    AddMethod('DefineProperties', @TSepiImportsTDataModule.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('GetChildren', @TSepiImportsTDataModule.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('HandleCreateException', @TSepiImportsTDataModule.HandleCreateException,
      'function: Boolean',
      mlkDynamic);
    AddMethod('ReadState', @TSepiImportsTDataModule.ReadState,
      'procedure(Reader: TReader)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTDataModule.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('CreateNew', @TSepiImportsTDataModule.CreateNew,
      'constructor(AOwner: TComponent; Dummy: Integer = 0)',
      mlkVirtual);
    AddMethod('Destroy', @TSepiImportsTDataModule.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AfterConstruction', @TSepiImportsTDataModule.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('BeforeDestruction', @TSepiImportsTDataModule.BeforeDestruction,
      'procedure',
      mlkOverride);

    AddProperty('DesignOffset', 'property: TPoint',
      'FDesignOffset', 'FDesignOffset');
    AddProperty('DesignSize', 'property: TPoint',
      'FDesignSize', 'FDesignSize');

    CurrentVisibility := mvPublished;

    AddProperty('OldCreateOrder', 'property: Boolean',
      'FOldCreateOrder', 'FOldCreateOrder');
    AddProperty('OnCreate', 'property: TNotifyEvent',
      'FOnCreate', 'FOnCreate');
    AddProperty('OnDestroy', 'property: TNotifyEvent',
      'FOnDestroy', 'FOnDestroy');

    Complete;
  end;
end;

{-----------------------}
{ TIdentMapEntry import }
{-----------------------}

function SepiImportTIdentMapEntry(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TIdentMapEntry', False, True,
    TypeInfo(TIdentMapEntry));

  with Result do
  begin
    AddField('Value', System.TypeInfo(Integer));
    AddField('Name', System.TypeInfo(String));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function PointsEqual_0(const P1, P2: TPoint): Boolean;
begin
  Result := PointsEqual(P1, P2);
end;

function PointsEqual_1(const P1, P2: TSmallPoint): Boolean;
begin
  Result := PointsEqual(P1, P2);
end;

function InvalidPoint_0(X, Y: Integer): Boolean;
begin
  Result := InvalidPoint(X, Y);
end;

function InvalidPoint_1(const At: TPoint): Boolean;
begin
  Result := InvalidPoint(At);
end;

function InvalidPoint_2(const At: TSmallPoint): Boolean;
begin
  Result := InvalidPoint(At);
end;

function Rect_0(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  Result := Rect(ALeft, ATop, ARight, ABottom);
end;

function Rect_1(const ATopLeft, ABottomRight: TPoint): TRect;
begin
  Result := Rect(ATopLeft, ABottomRight);
end;

function ClassGroupOf_0(AClass: TPersistentClass): TPersistentClass;
begin
  Result := ClassGroupOf(AClass);
end;

function ClassGroupOf_1(Instance: TPersistent): TPersistentClass;
begin
  Result := ClassGroupOf(Instance);
end;

function GetUltimateOwner_0(ACollectionItem: TCollectionItem): TPersistent;
begin
  Result := GetUltimateOwner(ACollectionItem);
end;

function GetUltimateOwner_1(ACollection: TCollection): TPersistent;
begin
  Result := GetUltimateOwner(ACollection);
end;

function GetUltimateOwner_2(APersistent: TPersistent): TPersistent;
begin
  Result := GetUltimateOwner(APersistent);
end;

procedure ObjectBinaryToText_0(Input, Output: TStream);
begin
  ObjectBinaryToText(Input, Output);
end;

procedure ObjectBinaryToText_1(Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat );
begin
  ObjectBinaryToText(Input, Output, OriginalFormat);
end;

procedure ObjectTextToBinary_0(Input, Output: TStream);
begin
  ObjectTextToBinary(Input, Output);
end;

procedure ObjectTextToBinary_1(Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat );
begin
  ObjectTextToBinary(Input, Output, OriginalFormat);
end;

procedure ObjectResourceToText_0(Input, Output: TStream);
begin
  ObjectResourceToText(Input, Output);
end;

procedure ObjectResourceToText_1(Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat );
begin
  ObjectResourceToText(Input, Output, OriginalFormat);
end;

procedure ObjectTextToResource_0(Input, Output: TStream);
begin
  ObjectTextToResource(Input, Output);
end;

procedure ObjectTextToResource_1(Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat );
begin
  ObjectTextToResource(Input, Output, OriginalFormat);
end;

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'Classes',
    ['Windows', 'Messages', 'SysUtils', 'Variants', 'TypInfo', 'ActiveXTypes']);

  // Constants
  TSepiConstant.Create(Result, 'MaxListSize', MaxListSize);
  TSepiConstant.Create(Result, 'soFromBeginning', soFromBeginning);
  TSepiConstant.Create(Result, 'soFromCurrent', soFromCurrent);
  TSepiConstant.Create(Result, 'soFromEnd', soFromEnd);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSeekOrigin));

  // Constants
  TSepiConstant.Create(Result, 'fmCreate', fmCreate);
  TSepiConstant.Create(Result, 'toEOF', toEOF);
  TSepiConstant.Create(Result, 'toSymbol', toSymbol);
  TSepiConstant.Create(Result, 'toString', toString);
  TSepiConstant.Create(Result, 'toInteger', toInteger);
  TSepiConstant.Create(Result, 'toFloat', toFloat);
  TSepiConstant.Create(Result, 'toWString', toWString);
  TSepiConstant.Create(Result, 'scShift', scShift);
  TSepiConstant.Create(Result, 'scCtrl', scCtrl);
  TSepiConstant.Create(Result, 'scAlt', scAlt);
  TSepiConstant.Create(Result, 'scNone', scNone);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAlignment));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLeftRight));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBiDiMode));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TVerticalAlignment));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTopBottom));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TShiftState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THelpContext));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THelpType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TShortCut));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TNotifyEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetStrProc));
  TSepiImportsEStreamError.SepiImport(Result);
  TSepiImportsEFileStreamError.SepiImport(Result);
  TSepiImportsEFCreateError.SepiImport(Result);
  TSepiImportsEFOpenError.SepiImport(Result);
  TSepiImportsEFilerError.SepiImport(Result);
  TSepiImportsEReadError.SepiImport(Result);
  TSepiImportsEWriteError.SepiImport(Result);
  TSepiImportsEClassNotFound.SepiImport(Result);
  TSepiImportsEMethodNotFound.SepiImport(Result);
  TSepiImportsEInvalidImage.SepiImport(Result);
  TSepiImportsEResNotFound.SepiImport(Result);
  TSepiImportsEListError.SepiImport(Result);
  TSepiImportsEBitsError.SepiImport(Result);
  TSepiImportsEStringListError.SepiImport(Result);
  TSepiImportsEComponentError.SepiImport(Result);
  TSepiImportsEParserError.SepiImport(Result);
  TSepiImportsEOutOfResources.SepiImport(Result);
  TSepiImportsEInvalidOperation.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDuplicates));
  TSepiClass.ForwardDecl(Result, TypeInfo(TStream));
  TSepiClass.ForwardDecl(Result, TypeInfo(TFiler));
  TSepiClass.ForwardDecl(Result, TypeInfo(TReader));
  TSepiClass.ForwardDecl(Result, TypeInfo(TWriter));
  TSepiClass.ForwardDecl(Result, TypeInfo(TComponent));
  TSepiPointerType.Create(Result, 'PPointerList', 'TPointerList', True);
  TSepiArrayType.Create(Result, 'TPointerList',
    [0, MaxListSize - 1], 'Pointer', True);
  TSepiMethodRefType.Create(Result, 'TListSortCompare',
    'function(Item1, Item2: Pointer): Integer');
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TListNotification));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TListAssignOp));
  TSepiClass.ForwardDecl(Result, TypeInfo(TList));
  TSepiImportsTListEnumerator.SepiImport(Result);
  TSepiImportsTList.SepiImport(Result);
  TSepiImportsTThreadList.SepiImport(Result);
  SepiImportIInterfaceList(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TInterfaceList));
  TSepiImportsTInterfaceListEnumerator.SepiImport(Result);
  TSepiImportsTInterfaceList.SepiImport(Result);
  TSepiImportsTBits.SepiImport(Result);
  TSepiImportsTPersistent.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TPersistentClass', TypeInfo(TPersistent), True);
  TSepiImportsTInterfacedPersistent.SepiImport(Result);
  TSepiImportsTRecall.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCollection));
  TSepiImportsTCollectionItem.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TCollectionItemClass', TypeInfo(TCollectionItem), True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCollectionNotification));
  TSepiImportsTCollectionEnumerator.SepiImport(Result);
  TSepiImportsTCollection.SepiImport(Result);
  TSepiImportsTOwnedCollection.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TStrings));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetModuleProc));
  SepiImportIStringsAdapter(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStringsDefined));
  TSepiImportsTStringsEnumerator.SepiImport(Result);
  TSepiImportsTStrings.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TStringList));
  TSepiPointerType.Create(Result, 'PStringItem', TypeInfo(TStringItem), True);
  SepiImportTStringItem(Result);
  TSepiPointerType.Create(Result, 'PStringItemList', TypeInfo(TStringItemList), True);
  TSepiArrayType.Create(Result, 'TStringItemList',
    [0, MaxListSize], TypeInfo(TStringItem), True, TypeInfo(TStringItemList));
  TSepiMethodRefType.Create(Result, 'TStringListSortCompare',
    'function(List: TStringList; Index1, Index2: Integer): Integer');
  TSepiImportsTStringList.SepiImport(Result);
  TSepiImportsTStream.SepiImport(Result);
  SepiImportIStreamPersist(Result);
  TSepiImportsTHandleStream.SepiImport(Result);
  TSepiImportsTFileStream.SepiImport(Result);
  TSepiImportsTCustomMemoryStream.SepiImport(Result);
  TSepiImportsTMemoryStream.SepiImport(Result);
  TSepiImportsTStringStream.SepiImport(Result);
  TSepiImportsTResourceStream.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStreamOwnership));
  TSepiImportsTStreamAdapter.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetClass));
  TSepiImportsTClassFinder.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TValueType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFilerFlag));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFilerFlags));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TReaderProc));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TWriterProc));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStreamProc));
  SepiImportIInterfaceComponentReference(Result);
  TSepiImportsTFiler.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TComponentClass', TypeInfo(TComponent), True);
  SepiImportIVarStreamable(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFindMethodEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSetNameEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TReferenceNameEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAncestorNotFoundEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TReadComponentsProc));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TReaderError));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFindComponentClassEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCreateComponentEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFindMethodInstanceEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFindComponentInstanceEvent));
  TSepiImportsTReader.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFindAncestorEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFindMethodNameEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetLookupInfoEvent));
  TSepiImportsTWriter.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TParserErrorEvent));
  TSepiImportsTParser.SepiImport(Result);
  TSepiImportsEThread.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TThreadMethod));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TThreadPriority));
  TSepiPointerType.Create(Result, 'PSynchronizeRecord', 'TSynchronizeRecord', True);
  SepiImportTSynchronizeRecord(Result);
  TSepiImportsTThread.SepiImport(Result);
  TSepiImportsTComponentEnumerator.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOperation));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TComponentState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TComponentStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetChildProc));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TComponentName));
  SepiImportIVCLComObject(Result);
  SepiImportIDesignerNotify(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TBasicAction));
  TSepiImportsTComponent.SepiImport(Result);
  TSepiImportsTBasicActionLink.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TBasicActionLinkClass', TypeInfo(TBasicActionLink), True);
  TSepiImportsTBasicAction.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TBasicActionClass', TypeInfo(TBasicAction), True);
  TSepiImportsTDataModule.SepiImport(Result);

  // Global variables
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_1));
  TSepiVariable.Create(Result, 'AddDataModule',
    @AddDataModule, TypeInfo(TUnnamed_1));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_2));
  TSepiVariable.Create(Result, 'RemoveDataModule',
    @RemoveDataModule, TypeInfo(TUnnamed_2));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_3));
  TSepiVariable.Create(Result, 'ApplicationHandleException',
    @ApplicationHandleException, TypeInfo(TUnnamed_3));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_4));
  TSepiVariable.Create(Result, 'ApplicationShowException',
    @ApplicationShowException, TypeInfo(TUnnamed_4));
  TSepiMethodRefType.Create(Result, '$5',
    'procedure(const Page: string; const ComponentClasses: array of TComponentClass )');
  TSepiVariable.Create(Result, 'RegisterComponentsProc',
    @RegisterComponentsProc, '$5');
  TSepiMethodRefType.Create(Result, '$6',
    'procedure(const ComponentClasses: array of TComponentClass)');
  TSepiVariable.Create(Result, 'RegisterNoIconProc',
    @RegisterNoIconProc, '$6');
  TSepiVariable.Create(Result, 'CurrentGroup',
    CurrentGroup, TypeInfo(Integer));

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TActiveXRegType));

  // Global variables
  TSepiMethodRefType.Create(Result, '$7',
    'procedure(const ComponentClasses: array of TComponentClass; AxRegType: TActiveXRegType )');
  TSepiVariable.Create(Result, 'RegisterNonActiveXProc',
    @RegisterNonActiveXProc, '$7');
  TSepiMethodRefType.Create(Result, '$8',
    'procedure(Component: TComponent)');
  TSepiVariable.Create(Result, 'CreateVCLComObjectProc',
    @CreateVCLComObjectProc, '$8');

  // Routines
  TSepiMetaMethod.Create(Result, 'Point', @Point,
    'function(AX, AY: Integer): TPoint');
  TSepiMetaMethod.Create(Result, 'SmallPoint', @SmallPoint,
    'function(AX, AY: SmallInt): TSmallPoint');
  TSepiMetaMethod.CreateOverloaded(Result, 'PointsEqual', @PointsEqual_0,
    'function(const P1, P2: TPoint): Boolean');
  TSepiMetaMethod.CreateOverloaded(Result, 'PointsEqual', @PointsEqual_1,
    'function(const P1, P2: TSmallPoint): Boolean');
  TSepiMetaMethod.CreateOverloaded(Result, 'InvalidPoint', @InvalidPoint_0,
    'function(X, Y: Integer): Boolean');
  TSepiMetaMethod.CreateOverloaded(Result, 'InvalidPoint', @InvalidPoint_1,
    'function(const At: TPoint): Boolean');
  TSepiMetaMethod.CreateOverloaded(Result, 'InvalidPoint', @InvalidPoint_2,
    'function(const At: TSmallPoint): Boolean');
  TSepiMetaMethod.CreateOverloaded(Result, 'Rect', @Rect_0,
    'function(ALeft, ATop, ARight, ABottom: Integer): TRect');
  TSepiMetaMethod.CreateOverloaded(Result, 'Rect', @Rect_1,
    'function(const ATopLeft, ABottomRight: TPoint): TRect');
  TSepiMetaMethod.Create(Result, 'Bounds', @Bounds,
    'function(ALeft, ATop, AWidth, AHeight: Integer): TRect');
  TSepiMetaMethod.Create(Result, 'RegisterClass', @RegisterClass,
    'procedure(AClass: TPersistentClass)');
  TSepiMetaMethod.Create(Result, 'RegisterClasses', @RegisterClasses,
    'procedure(AClasses: array of TPersistentClass)');
  TSepiMetaMethod.Create(Result, 'RegisterClassAlias', @RegisterClassAlias,
    'procedure(AClass: TPersistentClass; const Alias: string)');
  TSepiMetaMethod.Create(Result, 'UnRegisterClass', @UnRegisterClass,
    'procedure(AClass: TPersistentClass)');
  TSepiMetaMethod.Create(Result, 'UnRegisterClasses', @UnRegisterClasses,
    'procedure(AClasses: array of TPersistentClass)');
  TSepiMetaMethod.Create(Result, 'UnRegisterModuleClasses', @UnRegisterModuleClasses,
    'procedure(Module: HMODULE)');
  TSepiMetaMethod.Create(Result, 'FindClass', @FindClass,
    'function(const ClassName: string): TPersistentClass');
  TSepiMetaMethod.Create(Result, 'GetClass', @GetClass,
    'function(const AClassName: string): TPersistentClass');
  TSepiMetaMethod.Create(Result, 'StartClassGroup', @StartClassGroup,
    'procedure(AClass: TPersistentClass)');
  TSepiMetaMethod.Create(Result, 'GroupDescendentsWith', @GroupDescendentsWith,
    'procedure(AClass, AClassGroup: TPersistentClass)');
  TSepiMetaMethod.Create(Result, 'ActivateClassGroup', @ActivateClassGroup,
    'function(AClass: TPersistentClass): TPersistentClass');
  TSepiMetaMethod.Create(Result, 'ActiveClassGroup', @ActiveClassGroup,
    'function: TPersistentClass');
  TSepiMetaMethod.CreateOverloaded(Result, 'ClassGroupOf', @ClassGroupOf_0,
    'function(AClass: TPersistentClass): TPersistentClass');
  TSepiMetaMethod.CreateOverloaded(Result, 'ClassGroupOf', @ClassGroupOf_1,
    'function(Instance: TPersistent): TPersistentClass');
  TSepiMetaMethod.Create(Result, 'RegisterComponents', @RegisterComponents,
    'procedure(const Page: string; const ComponentClasses: array of TComponentClass )');
  TSepiMetaMethod.Create(Result, 'RegisterNoIcon', @RegisterNoIcon,
    'procedure(const ComponentClasses: array of TComponentClass)');
  TSepiMetaMethod.Create(Result, 'RegisterNonActiveX', @RegisterNonActiveX,
    'procedure(const ComponentClasses: array of TComponentClass; AxRegType: TActiveXRegType )');

  // Global variables
  TSepiVariable.Create(Result, 'GlobalNameSpace',
    GlobalNameSpace, TypeInfo(IReadWriteSync));

  // Types
  SepiImportTIdentMapEntry(Result);
  TSepiMethodRefType.Create(Result, 'TIdentToInt',
    'function(const Ident: string; var Int: Longint): Boolean');
  TSepiMethodRefType.Create(Result, 'TIntToIdent',
    'function(Int: Longint; var Ident: string): Boolean');
  TSepiMethodRefType.Create(Result, 'TFindGlobalComponent',
    'function(const Name: string): TComponent');
  TSepiMethodRefType.Create(Result, 'TIsUniqueGlobalComponentName',
    'function(const Name: string): Boolean');

  // Global variables
  TSepiVariable.Create(Result, 'IsUniqueGlobalComponentNameProc',
    @IsUniqueGlobalComponentNameProc, 'TIsUniqueGlobalComponentName');

  // Routines
  TSepiMetaMethod.Create(Result, 'RegisterIntegerConsts', @RegisterIntegerConsts,
    'procedure(AIntegerType: Pointer; AIdentToInt: TIdentToInt; AIntToIdent: TIntToIdent )');
  TSepiMetaMethod.Create(Result, 'UnregisterIntegerConsts', @UnregisterIntegerConsts,
    'procedure(AIntegerType: Pointer; AIdentToInt: TIdentToInt; AIntToIdent: TIntToIdent )');
  TSepiMetaMethod.Create(Result, 'RegisterFindGlobalComponentProc', @RegisterFindGlobalComponentProc,
    'procedure(AFindGlobalComponent: TFindGlobalComponent)');
  TSepiMetaMethod.Create(Result, 'UnregisterFindGlobalComponentProc', @UnregisterFindGlobalComponentProc,
    'procedure(AFindGlobalComponent: TFindGlobalComponent)');
  TSepiMetaMethod.Create(Result, 'FindGlobalComponent', @FindGlobalComponent,
    'function(const Name: string): TComponent');
  TSepiMetaMethod.Create(Result, 'IsUniqueGlobalComponentName', @IsUniqueGlobalComponentName,
    'function(const Name: string): Boolean');
  TSepiMetaMethod.Create(Result, 'IdentToInt', @IdentToInt,
    'function(const Ident: string; var Int: Longint; const Map: array of TIdentMapEntry): Boolean');
  TSepiMetaMethod.Create(Result, 'IntToIdent', @IntToIdent,
    'function(Int: Longint; var Ident: string; const Map: array of TIdentMapEntry): Boolean');
  TSepiMetaMethod.Create(Result, 'FindIntToIdent', @FindIntToIdent,
    'function(AIntegerType: Pointer): TIntToIdent');
  TSepiMetaMethod.Create(Result, 'FindIdentToInt', @FindIdentToInt,
    'function(AIntegerType: Pointer): TIdentToInt');
  TSepiMetaMethod.Create(Result, 'InitInheritedComponent', @InitInheritedComponent,
    'function(Instance: TComponent; RootAncestor: TClass): Boolean');
  TSepiMetaMethod.Create(Result, 'InitComponentRes', @InitComponentRes,
    'function(const ResName: string; Instance: TComponent): Boolean');
  TSepiMetaMethod.Create(Result, 'ReadComponentRes', @ReadComponentRes,
    'function(const ResName: string; Instance: TComponent): TComponent');
  TSepiMetaMethod.Create(Result, 'ReadComponentResEx', @ReadComponentResEx,
    'function(HInstance: THandle; const ResName: string): TComponent');
  TSepiMetaMethod.Create(Result, 'ReadComponentResFile', @ReadComponentResFile,
    'function(const FileName: string; Instance: TComponent): TComponent');
  TSepiMetaMethod.Create(Result, 'WriteComponentResFile', @WriteComponentResFile,
    'procedure(const FileName: string; Instance: TComponent)');
  TSepiMetaMethod.Create(Result, 'GlobalFixupReferences', @GlobalFixupReferences,
    'procedure');
  TSepiMetaMethod.Create(Result, 'GetFixupReferenceNames', @GetFixupReferenceNames,
    'procedure(Root: TComponent; Names: TStrings)');
  TSepiMetaMethod.Create(Result, 'GetFixupInstanceNames', @GetFixupInstanceNames,
    'procedure(Root: TComponent; const ReferenceRootName: string ; Names: TStrings )');
  TSepiMetaMethod.Create(Result, 'RedirectFixupReferences', @RedirectFixupReferences,
    'procedure(Root: TComponent; const OldRootName, NewRootName : string )');
  TSepiMetaMethod.Create(Result, 'RemoveFixupReferences', @RemoveFixupReferences,
    'procedure(Root: TComponent; const RootName: string)');
  TSepiMetaMethod.Create(Result, 'RemoveFixups', @RemoveFixups,
    'procedure(Instance: TPersistent)');
  TSepiMetaMethod.Create(Result, 'FindNestedComponent', @FindNestedComponent,
    'function(Root: TComponent; const NamePath: string): TComponent');
  TSepiMetaMethod.Create(Result, 'BeginGlobalLoading', @BeginGlobalLoading,
    'procedure');
  TSepiMetaMethod.Create(Result, 'NotifyGlobalLoading', @NotifyGlobalLoading,
    'procedure');
  TSepiMetaMethod.Create(Result, 'EndGlobalLoading', @EndGlobalLoading,
    'procedure');
  TSepiMetaMethod.Create(Result, 'CollectionsEqual', @CollectionsEqual,
    'function(C1, C2: TCollection; Owner1, Owner2: TComponent): Boolean');
  TSepiMetaMethod.CreateOverloaded(Result, 'GetUltimateOwner', @GetUltimateOwner_0,
    'function(ACollectionItem: TCollectionItem): TPersistent');
  TSepiMetaMethod.CreateOverloaded(Result, 'GetUltimateOwner', @GetUltimateOwner_1,
    'function(ACollection: TCollection): TPersistent');
  TSepiMetaMethod.CreateOverloaded(Result, 'GetUltimateOwner', @GetUltimateOwner_2,
    'function(APersistent: TPersistent): TPersistent');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStreamOriginalFormat));

  // Routines
  TSepiMetaMethod.CreateOverloaded(Result, 'ObjectBinaryToText', @ObjectBinaryToText_0,
    'procedure(Input, Output: TStream)');
  TSepiMetaMethod.CreateOverloaded(Result, 'ObjectBinaryToText', @ObjectBinaryToText_1,
    'procedure(Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat )');
  TSepiMetaMethod.CreateOverloaded(Result, 'ObjectTextToBinary', @ObjectTextToBinary_0,
    'procedure(Input, Output: TStream)');
  TSepiMetaMethod.CreateOverloaded(Result, 'ObjectTextToBinary', @ObjectTextToBinary_1,
    'procedure(Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat )');
  TSepiMetaMethod.CreateOverloaded(Result, 'ObjectResourceToText', @ObjectResourceToText_0,
    'procedure(Input, Output: TStream)');
  TSepiMetaMethod.CreateOverloaded(Result, 'ObjectResourceToText', @ObjectResourceToText_1,
    'procedure(Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat )');
  TSepiMetaMethod.CreateOverloaded(Result, 'ObjectTextToResource', @ObjectTextToResource_0,
    'procedure(Input, Output: TStream)');
  TSepiMetaMethod.CreateOverloaded(Result, 'ObjectTextToResource', @ObjectTextToResource_1,
    'procedure(Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat )');
  TSepiMetaMethod.Create(Result, 'TestStreamFormat', @TestStreamFormat,
    'function(Stream: TStream): TStreamOriginalFormat');
  TSepiMetaMethod.Create(Result, 'LineStart', @LineStart,
    'function(Buffer, BufPos: PChar): PChar');
  TSepiMetaMethod.Create(Result, 'ExtractStrings', @ExtractStrings,
    'function(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings ) : Integer');
  TSepiMetaMethod.Create(Result, 'BinToHex', @BinToHex,
    'procedure(Buffer, Text: PChar; BufSize: Integer)');
  TSepiMetaMethod.Create(Result, 'HexToBin', @HexToBin,
    'function(Text, Buffer: PChar; BufSize: Integer): Integer');
  TSepiMetaMethod.Create(Result, 'FindRootDesigner', @FindRootDesigner,
    'function(Obj: TPersistent): IDesignerNotify');
  TSepiMetaMethod.Create(Result, 'CountGenerations', @CountGenerations,
    'function(Ancestor, Descendent: TClass): Integer');
  TSepiMetaMethod.Create(Result, 'CheckSynchronize', @CheckSynchronize,
    'function(Timeout: Integer = 0): Boolean');

  // Global variables
  TSepiVariable.Create(Result, 'WakeMainThread',
    @WakeMainThread, TypeInfo(TNotifyEvent));
  TSepiVariable.Create(Result, 'SyncEvent',
    SyncEvent, TypeInfo(THandle));

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TWndMethod));

  // Routines
  TSepiMetaMethod.Create(Result, 'MakeObjectInstance', @MakeObjectInstance,
    'function(Method: TWndMethod): Pointer');
  TSepiMetaMethod.Create(Result, 'FreeObjectInstance', @FreeObjectInstance,
    'procedure(ObjectInstance: Pointer)');
  TSepiMetaMethod.Create(Result, 'AllocateHWnd', @AllocateHWnd,
    'function(Method: TWndMethod): HWND');
  TSepiMetaMethod.Create(Result, 'DeallocateHWnd', @DeallocateHWnd,
    'procedure(Wnd: HWND)');
  TSepiMetaMethod.Create(Result, 'AncestorIsValid', @AncestorIsValid,
    'function(Ancestor: TPersistent; Root, RootAncestor : TComponent ) : Boolean');
  TSepiMetaMethod.Create(Result, 'IsDefaultPropertyValue', @IsDefaultPropertyValue,
    'function(Instance: TObject; PropInfo: PPropInfo; OnGetLookupInfo: TGetLookupInfoEvent ; Writer: TWriter = nil ; OnFindMethodName: TFindMethodNameEvent = nil ) : Boolean');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Classes', ImportUnit);
end.

