{*
  Importe l'unité Contnrs dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsContnrs;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Contnrs, Classes;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTObjectList = class(TObjectList)
  private
    constructor Create_0;
    constructor Create_1(AOwnsObjects: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTComponentList = class(TComponentList)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTClassList = class(TClassList)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTOrderedList = class(TOrderedList)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTStack = class(TStack)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTObjectStack = class(TObjectStack)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTQueue = class(TQueue)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTObjectQueue = class(TObjectQueue)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomBucketList = class(TCustomBucketList)
  private
    function GetData(AItem: Pointer): Pointer;
    procedure SetData(AItem: Pointer; const AData: Pointer);
    procedure SetBucketCount(const Value: Integer);
    function ForEach_0(AProc: TBucketProc; AInfo: Pointer = nil): Boolean;
    function ForEach_1(AEvent: TBucketEvent): Boolean;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTBucketList = class(TBucketList)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTObjectBucketList = class(TObjectBucketList)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTIntegerBucketList = class(TIntegerBucketList)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{--------------------}
{ TObjectList import }
{--------------------}

constructor TSepiImportsTObjectList.Create_0;
begin
  Create;
end;

constructor TSepiImportsTObjectList.Create_1(AOwnsObjects: Boolean);
begin
  Create(AOwnsObjects);
end;

class function TSepiImportsTObjectList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TObjectList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOwnsObjects', System.TypeInfo(Boolean));

    CurrentVisibility := mvProtected;

    AddMethod('Notify', @TSepiImportsTObjectList.Notify,
      'procedure(Ptr: Pointer; Action: TListNotification)',
      mlkOverride);
    AddMethod('GetItem', @TSepiImportsTObjectList.GetItem,
      'function(Index: Integer): TObject');
    AddMethod('SetItem', @TSepiImportsTObjectList.SetItem,
      'procedure(Index: Integer; AObject: TObject)');

    CurrentVisibility := mvPublic;

    AddOverloadedMethod('Create', @TSepiImportsTObjectList.Create_0,
      'constructor');
    AddOverloadedMethod('Create', @TSepiImportsTObjectList.Create_1,
      'constructor(AOwnsObjects: Boolean)');
    AddMethod('Add', @TSepiImportsTObjectList.Add,
      'function(AObject: TObject): Integer');
    AddMethod('Extract', @TSepiImportsTObjectList.Extract,
      'function(Item: TObject): TObject');
    AddMethod('Remove', @TSepiImportsTObjectList.Remove,
      'function(AObject: TObject): Integer');
    AddMethod('IndexOf', @TSepiImportsTObjectList.IndexOf,
      'function(AObject: TObject): Integer');
    AddMethod('FindInstanceOf', @TSepiImportsTObjectList.FindInstanceOf,
      'function(AClass: TClass; AExact: Boolean = True; AStartAt: Integer = 0): Integer');
    AddMethod('Insert', @TSepiImportsTObjectList.Insert,
      'procedure(Index: Integer; AObject: TObject)');
    AddMethod('First', @TSepiImportsTObjectList.First,
      'function: TObject');
    AddMethod('Last', @TSepiImportsTObjectList.Last,
      'function: TObject');

    AddProperty('OwnsObjects', 'property: Boolean',
      'FOwnsObjects', 'FOwnsObjects');
    AddProperty('Items', 'property[Index: Integer]: TObject',
      'GetItem', 'SetItem', True);

    Complete;
  end;
end;

{-----------------------}
{ TComponentList import }
{-----------------------}

class function TSepiImportsTComponentList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComponentList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FNexus', System.TypeInfo(TComponent));

    CurrentVisibility := mvProtected;

    AddMethod('Notify', @TSepiImportsTComponentList.Notify,
      'procedure(Ptr: Pointer; Action: TListNotification)',
      mlkOverride);
    AddMethod('GetItems', @TSepiImportsTComponentList.GetItems,
      'function(Index: Integer): TComponent');
    AddMethod('SetItems', @TSepiImportsTComponentList.SetItems,
      'procedure(Index: Integer; AComponent: TComponent)');
    AddMethod('HandleFreeNotify', @TSepiImportsTComponentList.HandleFreeNotify,
      'procedure(Sender: TObject; AComponent: TComponent)');

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTComponentList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTComponentList.Add,
      'function(AComponent: TComponent): Integer');
    AddMethod('Extract', @TSepiImportsTComponentList.Extract,
      'function(Item: TComponent): TComponent');
    AddMethod('Remove', @TSepiImportsTComponentList.Remove,
      'function(AComponent: TComponent): Integer');
    AddMethod('IndexOf', @TSepiImportsTComponentList.IndexOf,
      'function(AComponent: TComponent): Integer');
    AddMethod('First', @TSepiImportsTComponentList.First,
      'function: TComponent');
    AddMethod('Last', @TSepiImportsTComponentList.Last,
      'function: TComponent');
    AddMethod('Insert', @TSepiImportsTComponentList.Insert,
      'procedure(Index: Integer; AComponent: TComponent)');

    AddProperty('Items', 'property[Index: Integer]: TComponent',
      'GetItems', 'SetItems', True);

    Complete;
  end;
end;

{-------------------}
{ TClassList import }
{-------------------}

class function TSepiImportsTClassList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TClassList));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('GetItems', @TSepiImportsTClassList.GetItems,
      'function(Index: Integer): TClass');
    AddMethod('SetItems', @TSepiImportsTClassList.SetItems,
      'procedure(Index: Integer; AClass: TClass)');

    CurrentVisibility := mvPublic;

    AddMethod('Add', @TSepiImportsTClassList.Add,
      'function(AClass: TClass): Integer');
    AddMethod('Extract', @TSepiImportsTClassList.Extract,
      'function(Item: TClass): TClass');
    AddMethod('Remove', @TSepiImportsTClassList.Remove,
      'function(AClass: TClass): Integer');
    AddMethod('IndexOf', @TSepiImportsTClassList.IndexOf,
      'function(AClass: TClass): Integer');
    AddMethod('First', @TSepiImportsTClassList.First,
      'function: TClass');
    AddMethod('Last', @TSepiImportsTClassList.Last,
      'function: TClass');
    AddMethod('Insert', @TSepiImportsTClassList.Insert,
      'procedure(Index: Integer; AClass: TClass)');

    AddProperty('Items', 'property[Index: Integer]: TClass',
      'GetItems', 'SetItems', True);

    Complete;
  end;
end;

{---------------------}
{ TOrderedList import }
{---------------------}

class function TSepiImportsTOrderedList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TOrderedList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FList', System.TypeInfo(TList));

    CurrentVisibility := mvProtected;

    AddMethod('PushItem', nil,
      'procedure(AItem: Pointer)',
      mlkVirtual, True);
    AddMethod('PopItem', @TSepiImportsTOrderedList.PopItem,
      'function: Pointer',
      mlkVirtual);
    AddMethod('PeekItem', @TSepiImportsTOrderedList.PeekItem,
      'function: Pointer',
      mlkVirtual);

    AddProperty('List', 'property: TList',
      'FList', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTOrderedList.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTOrderedList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Count', @TSepiImportsTOrderedList.Count,
      'function: Integer');
    AddMethod('AtLeast', @TSepiImportsTOrderedList.AtLeast,
      'function(ACount: Integer): Boolean');
    AddMethod('Push', @TSepiImportsTOrderedList.Push,
      'function(AItem: Pointer): Pointer');
    AddMethod('Pop', @TSepiImportsTOrderedList.Pop,
      'function: Pointer');
    AddMethod('Peek', @TSepiImportsTOrderedList.Peek,
      'function: Pointer');

    Complete;
  end;
end;

{---------------}
{ TStack import }
{---------------}

class function TSepiImportsTStack.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TStack));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('PushItem', @TSepiImportsTStack.PushItem,
      'procedure(AItem: Pointer)',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ TObjectStack import }
{---------------------}

class function TSepiImportsTObjectStack.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TObjectStack));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Push', @TSepiImportsTObjectStack.Push,
      'function(AObject: TObject): TObject');
    AddMethod('Pop', @TSepiImportsTObjectStack.Pop,
      'function: TObject');
    AddMethod('Peek', @TSepiImportsTObjectStack.Peek,
      'function: TObject');

    Complete;
  end;
end;

{---------------}
{ TQueue import }
{---------------}

class function TSepiImportsTQueue.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TQueue));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('PushItem', @TSepiImportsTQueue.PushItem,
      'procedure(AItem: Pointer)',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ TObjectQueue import }
{---------------------}

class function TSepiImportsTObjectQueue.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TObjectQueue));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Push', @TSepiImportsTObjectQueue.Push,
      'function(AObject: TObject): TObject');
    AddMethod('Pop', @TSepiImportsTObjectQueue.Pop,
      'function: TObject');
    AddMethod('Peek', @TSepiImportsTObjectQueue.Peek,
      'function: TObject');

    Complete;
  end;
end;

{--------------------}
{ TBucketItem import }
{--------------------}

function SepiImportTBucketItem(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TBucketItem', False, True);

  with Result do
  begin
    AddField('Item', 'Pointer');
    AddField('Data', 'Pointer', True);

    Complete;
  end;
end;

{----------------}
{ TBucket import }
{----------------}

function SepiImportTBucket(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TBucket', False, True,
    TypeInfo(TBucket));

  with Result do
  begin
    AddField('Count', System.TypeInfo(Integer));
    AddField('Items', System.TypeInfo(TBucketItemArray));

    Complete;
  end;
end;

{--------------------------}
{ TCustomBucketList import }
{--------------------------}

function TSepiImportsTCustomBucketList.GetData(AItem: Pointer): Pointer;
begin
  Result := Data[AItem];
end;

procedure TSepiImportsTCustomBucketList.SetData(AItem: Pointer;
  const AData: Pointer);
begin
  Data[AItem] := AData;
end;

procedure TSepiImportsTCustomBucketList.SetBucketCount(const Value: Integer);
begin
  BucketCount := Value;
end;

function TSepiImportsTCustomBucketList.ForEach_0(AProc: TBucketProc;
  AInfo: Pointer = nil): Boolean;
begin
  Result := ForEach(AProc, AInfo);
end;

function TSepiImportsTCustomBucketList.ForEach_1(
  AEvent: TBucketEvent): Boolean;
begin
  Result := ForEach(AEvent);
end;

class function TSepiImportsTCustomBucketList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomBucketList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomBucketList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FBuckets', System.TypeInfo(TBucketArray));
    AddField('FBucketCount', System.TypeInfo(Integer));
    AddField('FListLocked', System.TypeInfo(Boolean));
    AddField('FClearing', System.TypeInfo(Boolean));

    AddMethod('GetData', @TSepiImportsTCustomBucketList.GetData,
      'function(AItem: Pointer): Pointer');
    AddMethod('SetData', @TSepiImportsTCustomBucketList.SetData,
      'procedure(AItem: Pointer; const AData: Pointer)');
    AddMethod('SetBucketCount', @TSepiImportsTCustomBucketList.SetBucketCount,
      'procedure(const Value: Integer)');

    CurrentVisibility := mvProtected;

    AddProperty('Buckets', 'property: TBucketArray',
      'FBuckets', '');
    AddProperty('BucketCount', 'property: Integer',
      'FBucketCount', 'SetBucketCount');

    AddMethod('BucketFor', nil,
      'function(AItem: Pointer): Integer',
      mlkVirtual, True);
    AddMethod('FindItem', @TSepiImportsTCustomBucketList.FindItem,
      'function(AItem: Pointer; out ABucket, AIndex: Integer): Boolean',
      mlkVirtual);
    AddMethod('AddItem', @TSepiImportsTCustomBucketList.AddItem,
      'function(ABucket: Integer; AItem, AData: Pointer): Pointer',
      mlkVirtual);
    AddMethod('DeleteItem', @TSepiImportsTCustomBucketList.DeleteItem,
      'function(ABucket: Integer; AIndex: Integer): Pointer',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTCustomBucketList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTCustomBucketList.Clear,
      'procedure');
    AddMethod('Add', @TSepiImportsTCustomBucketList.Add,
      'function(AItem, AData: Pointer): Pointer');
    AddMethod('Remove', @TSepiImportsTCustomBucketList.Remove,
      'function(AItem: Pointer): Pointer');
    AddOverloadedMethod('ForEach', @TSepiImportsTCustomBucketList.ForEach_0,
      'function(AProc: TBucketProc; AInfo: Pointer = nil): Boolean');
    AddOverloadedMethod('ForEach', @TSepiImportsTCustomBucketList.ForEach_1,
      'function(AEvent: TBucketEvent): Boolean');
    AddMethod('Assign', @TSepiImportsTCustomBucketList.Assign,
      'procedure(AList: TCustomBucketList)');
    AddMethod('Exists', @TSepiImportsTCustomBucketList.Exists,
      'function(AItem: Pointer): Boolean');
    AddMethod('Find', @TSepiImportsTCustomBucketList.Find,
      'function(AItem: Pointer; out AData: Pointer): Boolean');

    AddProperty('Data', 'property[AItem: Pointer]: Pointer',
      'GetData', 'SetData', True);

    Complete;
  end;
end;

{--------------------}
{ TBucketList import }
{--------------------}

class function TSepiImportsTBucketList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBucketList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FBucketMask', System.TypeInfo(Byte));

    CurrentVisibility := mvProtected;

    AddMethod('BucketFor', @TSepiImportsTBucketList.BucketFor,
      'function(AItem: Pointer): Integer',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBucketList.Create,
      'constructor(ABuckets: TBucketListSizes = bl16)');

    Complete;
  end;
end;

{--------------------------}
{ TObjectBucketList import }
{--------------------------}

class function TSepiImportsTObjectBucketList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TObjectBucketList));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('GetData', @TSepiImportsTObjectBucketList.GetData,
      'function(AItem: TObject): TObject');
    AddMethod('SetData', @TSepiImportsTObjectBucketList.SetData,
      'procedure(AItem: TObject; const AData: TObject)');

    CurrentVisibility := mvPublic;

    AddMethod('Add', @TSepiImportsTObjectBucketList.Add,
      'function(AItem, AData: TObject): TObject');
    AddMethod('Remove', @TSepiImportsTObjectBucketList.Remove,
      'function(AItem: TObject): TObject');

    AddProperty('Data', 'property[AItem: TObject]: TObject',
      'GetData', 'SetData', True);

    Complete;
  end;
end;

{---------------------------}
{ TIntegerBucketList import }
{---------------------------}

class function TSepiImportsTIntegerBucketList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TIntegerBucketList));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('GetData', @TSepiImportsTIntegerBucketList.GetData,
      'function(AItem: Integer): Integer');
    AddMethod('SetData', @TSepiImportsTIntegerBucketList.SetData,
      'procedure(AItem: Integer; const AData: Integer)');

    CurrentVisibility := mvPublic;

    AddMethod('Add', @TSepiImportsTIntegerBucketList.Add,
      'function(AItem, AData: Integer): Integer');
    AddMethod('Remove', @TSepiImportsTIntegerBucketList.Remove,
      'function(AItem: Integer): Integer');

    AddProperty('Data', 'property[AItem: Integer]: Integer',
      'GetData', 'SetData', True);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'Contnrs',
    ['SysUtils', 'Classes']);

  // Types
  TSepiImportsTObjectList.SepiImport(Result);
  TSepiImportsTComponentList.SepiImport(Result);
  TSepiImportsTClassList.SepiImport(Result);
  TSepiImportsTOrderedList.SepiImport(Result);
  TSepiImportsTStack.SepiImport(Result);
  TSepiImportsTObjectStack.SepiImport(Result);
  TSepiImportsTQueue.SepiImport(Result);
  TSepiImportsTObjectQueue.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomBucketList));
  SepiImportTBucketItem(Result);
  TSepiDynArrayType(TSepiType.LoadFromTypeInfo(
    Result, TypeInfo(TBucketItemArray))).SetElementType('TBucketItem');
  SepiImportTBucket(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBucketArray));
  TSepiMethodRefType.Create(Result, 'TBucketProc',
    'procedure(AInfo, AItem, AData: Pointer; out AContinue: Boolean)');
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBucketEvent));
  TSepiImportsTCustomBucketList.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBucketListSizes));
  TSepiImportsTBucketList.SepiImport(Result);
  TSepiImportsTObjectBucketList.SepiImport(Result);
  TSepiImportsTIntegerBucketList.SepiImport(Result);

  // Routines
  TSepiMethod.Create(Result, 'RaiseListError', @RaiseListError,
    'procedure(const ATemplate: string; const AData: array of const)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Contnrs', ImportUnit);
end.

