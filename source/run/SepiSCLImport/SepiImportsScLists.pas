{*
  Importe l'unité ScLists dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsScLists;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ScLists, Classes;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEIntListError = class(EIntListError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCompareStrings = class(TCompareStrings)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsStringsOps = class(StringsOps)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScStrings = class(TScStrings)
  private
    function GetHasMoreString: boolean;
    procedure SetIndex(New : integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScList = class(TScList)
  private
    function GetCount: integer;
    procedure SetCount(New : integer);
    function GetHasMoreValue: boolean;
    function GetIndex: integer;
    procedure SetIndex(New : integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTIntegerList = class(TIntegerList)
  private
    function GetItems(Index : integer) : Int64;
    procedure SetItems(Index : integer; New : Int64);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTUnsignedIntList = class(TUnsignedIntList)
  private
    function GetItems(Index : integer) : LongWord;
    procedure SetItems(Index : integer; New : LongWord);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTExtendedList = class(TExtendedList)
  private
    function GetItems(Index : integer) : Extended;
    procedure SetItems(Index : integer; New : Extended);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{----------------------}
{ EIntListError import }
{----------------------}

class function TSepiImportsEIntListError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EIntListError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ TCompareStrings import }
{------------------------}

class function TSepiImportsTCompareStrings.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCompareStrings));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('CompareStrings', @TSepiImportsTCompareStrings.CompareStrings,
      'function(const S1, S2 : string) : integer',
      mlkOverride);

    Complete;
  end;
end;

{-------------------}
{ StringsOps import }
{-------------------}

class function TSepiImportsStringsOps.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(StringsOps));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('IndexOf', @TSepiImportsStringsOps.IndexOf,
      'class function(Strings : TStrings; const Str : string; BeginSearchAt : integer = 0 ; EndSearchAt : integer = -1 ) : integer');
    AddMethod('FindText', @TSepiImportsStringsOps.FindText,
      'class function(Strings : TStrings; const Str : string; BeginSearchAt : integer = 0 ; EndSearchAt : integer = -1 ) : integer');
    AddMethod('FindFirstWord', @TSepiImportsStringsOps.FindFirstWord,
      'class function(Strings : TStrings; const Word : string; BeginSearchAt : integer = 0 ; EndSearchAt : integer = -1 ) : integer');
    AddMethod('FindAtPos', @TSepiImportsStringsOps.FindAtPos,
      'class function(Strings : TStrings; const SubStr : string; Position : integer = 1 ; BeginSearchAt : integer = 0 ; EndSearchAt : integer = -1 ) : integer');
    AddMethod('CopyFrom', @TSepiImportsStringsOps.CopyFrom,
      'class procedure(Strings : TStrings; Source : TStrings; Index : integer = 0 ; Count : integer = -1 )');
    AddMethod('AddFrom', @TSepiImportsStringsOps.AddFrom,
      'class procedure(Strings : TStrings; Source : TStrings; Index : integer = 0 ; Count : integer = -1 )');
    AddMethod('FromString', @TSepiImportsStringsOps.FromString,
      'class procedure(Strings : TStrings; const Str, Delim : string; const NotIn : string = '''' )');
    AddMethod('AddFromString', @TSepiImportsStringsOps.AddFromString,
      'class procedure(Strings : TStrings; const Str, Delim : string; const NotIn : string = '''' )');

    Complete;
  end;
end;

{-------------------}
{ TScStrings import }
{-------------------}

function TSepiImportsTScStrings.GetHasMoreString: boolean;
begin
  Result := HasMoreString;
end;

procedure TSepiImportsTScStrings.SetIndex(New : integer);
begin
  Index := New;
end;

class function TSepiImportsTScStrings.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScStrings));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(integer));

    AddMethod('GetHasMoreString', @TSepiImportsTScStrings.GetHasMoreString,
      'function: boolean');
    AddMethod('SetIndex', @TSepiImportsTScStrings.SetIndex,
      'procedure(New : integer)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScStrings.Create,
      'constructor');
    AddMethod('CreateFromFile', @TSepiImportsTScStrings.CreateFromFile,
      'constructor(const FileName : TFileName)');
    AddMethod('CreateFromString', @TSepiImportsTScStrings.CreateFromString,
      'constructor(const Str, Delim : string; const NotIn : string = '''' )');
    AddMethod('CreateAssign', @TSepiImportsTScStrings.CreateAssign,
      'constructor(Source : TPersistent)');
    AddMethod('IndexOfEx', @TSepiImportsTScStrings.IndexOfEx,
      'function(const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1 ) : integer');
    AddMethod('FindText', @TSepiImportsTScStrings.FindText,
      'function(const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1 ) : integer');
    AddMethod('FindFirstWord', @TSepiImportsTScStrings.FindFirstWord,
      'function(const Word : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1 ) : integer');
    AddMethod('FindAtPos', @TSepiImportsTScStrings.FindAtPos,
      'function(const SubStr : string; Position : integer = 1; BeginSearchAt : integer = 0 ; EndSearchAt : integer = -1 ) : integer');
    AddMethod('CopyFrom', @TSepiImportsTScStrings.CopyFrom,
      'procedure(Source : TStrings; Index : integer = 0; Count : integer = -1 )');
    AddMethod('AddFrom', @TSepiImportsTScStrings.AddFrom,
      'procedure(Source : TStrings; Index : integer = 0; Count : integer = -1 )');
    AddMethod('FromString', @TSepiImportsTScStrings.FromString,
      'procedure(const Str, Delim : string; const NotIn : string = '''' )');
    AddMethod('AddFromString', @TSepiImportsTScStrings.AddFromString,
      'procedure(const Str, Delim : string; const NotIn : string = '''' )');
    AddMethod('Reset', @TSepiImportsTScStrings.Reset,
      'procedure');
    AddMethod('NextString', @TSepiImportsTScStrings.NextString,
      'function: string');

    AddProperty('HasMoreString', 'property: boolean',
      'GetHasMoreString', '');
    AddProperty('Index', 'property: integer',
      'FIndex', 'SetIndex');

    Complete;
  end;
end;

{----------------}
{ TScList import }
{----------------}

function TSepiImportsTScList.GetCount: integer;
begin
  Result := Count;
end;

procedure TSepiImportsTScList.SetCount(New : integer);
begin
  Count := New;
end;

function TSepiImportsTScList.GetHasMoreValue: boolean;
begin
  Result := HasMoreValue;
end;

function TSepiImportsTScList.GetIndex: integer;
begin
  Result := Index;
end;

procedure TSepiImportsTScList.SetIndex(New : integer);
begin
  Index := New;
end;

class function TSepiImportsTScList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TScList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TScList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStream', System.TypeInfo(TMemoryStream));
    AddField('FItemSize', System.TypeInfo(integer));

    AddMethod('GetCount', @TSepiImportsTScList.GetCount,
      'function: integer');
    AddMethod('SetCount', @TSepiImportsTScList.SetCount,
      'procedure(New : integer)');
    AddMethod('GetHasMoreValue', @TSepiImportsTScList.GetHasMoreValue,
      'function: boolean');
    AddMethod('GetIndex', @TSepiImportsTScList.GetIndex,
      'function: integer');
    AddMethod('SetIndex', @TSepiImportsTScList.SetIndex,
      'procedure(New : integer)');

    CurrentVisibility := mvProtected;

    AddMethod('DefineProperties', @TSepiImportsTScList.DefineProperties,
      'procedure(Filer : TFiler)',
      mlkOverride);
    AddMethod('AssignTo', @TSepiImportsTScList.AssignTo,
      'procedure(Dest : TPersistent)',
      mlkOverride);
    AddMethod('IsAssignClass', @TSepiImportsTScList.IsAssignClass,
      'function(ScListClass : TScListClass) : boolean',
      mlkVirtual);
    AddMethod('_Read', @TSepiImportsTScList._Read,
      'procedure(var Buffer)');
    AddMethod('_Write', @TSepiImportsTScList._Write,
      'procedure(var Buffer)');
    AddMethod('_GetItems', @TSepiImportsTScList._GetItems,
      'procedure(AIndex : integer; var Buffer)');
    AddMethod('_SetItems', @TSepiImportsTScList._SetItems,
      'procedure(AIndex : integer; var Buffer)');
    AddMethod('_Add', @TSepiImportsTScList._Add,
      'function(var Buffer) : integer');
    AddMethod('_Insert', @TSepiImportsTScList._Insert,
      'function(AIndex : integer; var Buffer) : integer');
    AddMethod('_Delete', @TSepiImportsTScList._Delete,
      'procedure(AIndex : integer; var Buffer)');

    AddProperty('ItemSize', 'property: integer',
      'FItemSize', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScList.Create,
      'constructor(ItemSize : integer)');
    AddMethod('Destroy', @TSepiImportsTScList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTScList.Assign,
      'procedure(Source : TPersistent)',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTScList.Clear,
      'procedure');
    AddMethod('Reset', @TSepiImportsTScList.Reset,
      'procedure');
    AddMethod('LoadFromStream', @TSepiImportsTScList.LoadFromStream,
      'procedure(Stream : TStream)');
    AddMethod('SaveToStream', @TSepiImportsTScList.SaveToStream,
      'procedure(Stream : TStream)');
    AddMethod('LoadFromFile', @TSepiImportsTScList.LoadFromFile,
      'procedure(const FileName : TFileName)');
    AddMethod('SaveToFile', @TSepiImportsTScList.SaveToFile,
      'procedure(const FileName : TFileName)');

    AddProperty('Count', 'property: integer',
      'GetCount', 'SetCount');
    AddProperty('HasMoreValue', 'property: boolean',
      'GetHasMoreValue', '');
    AddProperty('Index', 'property: integer',
      'GetIndex', 'SetIndex');

    Complete;
  end;
end;

{---------------------}
{ TIntegerList import }
{---------------------}

function TSepiImportsTIntegerList.GetItems(Index : integer) : Int64;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTIntegerList.SetItems(Index : integer; New : Int64);
begin
  Items[Index] := New;
end;

class function TSepiImportsTIntegerList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TIntegerList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetItems', @TSepiImportsTIntegerList.GetItems,
      'function(Index : integer) : Int64');
    AddMethod('SetItems', @TSepiImportsTIntegerList.SetItems,
      'procedure(Index : integer; New : Int64)');
    AddMethod('MakeItGood', nil,
      'procedure(var Value : Int64)');

    CurrentVisibility := mvProtected;

    AddMethod('AssignTo', @TSepiImportsTIntegerList.AssignTo,
      'procedure(Dest : TPersistent)',
      mlkOverride);
    AddMethod('IsAssignClass', @TSepiImportsTIntegerList.IsAssignClass,
      'function(ScListClass : TScListClass) : boolean',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTIntegerList.Create,
      'constructor(IntSize : integer = 4)');
    AddMethod('CreateAssign', @TSepiImportsTIntegerList.CreateAssign,
      'constructor(Source : TPersistent; IntSize : integer = 4)');
    AddMethod('Assign', @TSepiImportsTIntegerList.Assign,
      'procedure(Source : TPersistent)',
      mlkOverride);
    AddMethod('Read', @TSepiImportsTIntegerList.Read,
      'function: Int64');
    AddMethod('Write', @TSepiImportsTIntegerList.Write,
      'procedure(New : Int64)');
    AddMethod('Add', @TSepiImportsTIntegerList.Add,
      'function(New : Int64) : integer');
    AddMethod('Insert', @TSepiImportsTIntegerList.Insert,
      'function(Index : integer; New : Int64) : integer');
    AddMethod('Delete', @TSepiImportsTIntegerList.Delete,
      'function(Index : integer) : Int64');

    AddProperty('Items', 'property[index : integer] : Int64',
      'GetItems', 'SetItems', True);

    Complete;
  end;
end;

{-------------------------}
{ TUnsignedIntList import }
{-------------------------}

function TSepiImportsTUnsignedIntList.GetItems(Index : integer) : LongWord;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTUnsignedIntList.SetItems(Index : integer; New : LongWord);
begin
  Items[Index] := New;
end;

class function TSepiImportsTUnsignedIntList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TUnsignedIntList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetItems', @TSepiImportsTUnsignedIntList.GetItems,
      'function(Index : integer) : LongWord');
    AddMethod('SetItems', @TSepiImportsTUnsignedIntList.SetItems,
      'procedure(Index : integer; New : LongWord)');
    AddMethod('MakeItGood', nil,
      'procedure(var Value : LongWord)');

    CurrentVisibility := mvProtected;

    AddMethod('AssignTo', @TSepiImportsTUnsignedIntList.AssignTo,
      'procedure(Dest : TPersistent)',
      mlkOverride);
    AddMethod('IsAssignClass', @TSepiImportsTUnsignedIntList.IsAssignClass,
      'function(ScListClass : TScListClass) : boolean',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTUnsignedIntList.Create,
      'constructor(IntSize : integer = 4)');
    AddMethod('CreateAssign', @TSepiImportsTUnsignedIntList.CreateAssign,
      'constructor(Source : TPersistent; IntSize : integer = 4)');
    AddMethod('Assign', @TSepiImportsTUnsignedIntList.Assign,
      'procedure(Source : TPersistent)',
      mlkOverride);
    AddMethod('Read', @TSepiImportsTUnsignedIntList.Read,
      'function: LongWord');
    AddMethod('Write', @TSepiImportsTUnsignedIntList.Write,
      'procedure(New : LongWord)');
    AddMethod('Add', @TSepiImportsTUnsignedIntList.Add,
      'function(New : LongWord) : integer');
    AddMethod('Insert', @TSepiImportsTUnsignedIntList.Insert,
      'function(Index : integer; New : LongWord) : integer');
    AddMethod('Delete', @TSepiImportsTUnsignedIntList.Delete,
      'function(Index : integer) : LongWord');

    AddProperty('Items', 'property[index : integer] : LongWord',
      'GetItems', 'SetItems', True);

    Complete;
  end;
end;

{----------------------}
{ TExtendedList import }
{----------------------}

function TSepiImportsTExtendedList.GetItems(Index : integer) : Extended;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTExtendedList.SetItems(Index : integer; New : Extended);
begin
  Items[Index] := New;
end;

class function TSepiImportsTExtendedList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TExtendedList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetItems', @TSepiImportsTExtendedList.GetItems,
      'function(Index : integer) : Extended');
    AddMethod('SetItems', @TSepiImportsTExtendedList.SetItems,
      'procedure(Index : integer; New : Extended)');

    CurrentVisibility := mvProtected;

    AddMethod('AssignTo', @TSepiImportsTExtendedList.AssignTo,
      'procedure(Dest : TPersistent)',
      mlkOverride);
    AddMethod('IsAssignClass', @TSepiImportsTExtendedList.IsAssignClass,
      'function(ScListClass : TScListClass) : boolean',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTExtendedList.Create,
      'constructor');
    AddMethod('CreateAssign', @TSepiImportsTExtendedList.CreateAssign,
      'constructor(Source : TPersistent)');
    AddMethod('Assign', @TSepiImportsTExtendedList.Assign,
      'procedure(Source : TPersistent)',
      mlkOverride);
    AddMethod('Read', @TSepiImportsTExtendedList.Read,
      'function: Extended');
    AddMethod('Write', @TSepiImportsTExtendedList.Write,
      'procedure(New : Extended)');
    AddMethod('Add', @TSepiImportsTExtendedList.Add,
      'function(New : Extended) : integer');
    AddMethod('Insert', @TSepiImportsTExtendedList.Insert,
      'function(Index : integer; New : Extended) : integer');
    AddMethod('Delete', @TSepiImportsTExtendedList.Delete,
      'function(Index : integer) : Extended');

    AddProperty('Items', 'property[index : integer] : Extended',
      'GetItems', 'SetItems', True);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScLists',
    ['SysUtils', 'Classes']);

  // Types
  TSepiImportsEIntListError.SepiImport(Result);
  TSepiImportsTCompareStrings.SepiImport(Result);
  TSepiImportsStringsOps.SepiImport(Result);
  TSepiImportsTScStrings.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TScList));
  TSepiMetaClass.Create(Result, 'TScListClass', TypeInfo(TScList), True);
  TSepiImportsTScList.SepiImport(Result);
  TSepiImportsTIntegerList.SepiImport(Result);
  TSepiImportsTUnsignedIntList.SepiImport(Result);
  TSepiImportsTExtendedList.SepiImport(Result);

  // Global variables
  TSepiVariable.Create(Result, 'AppParams',
     AppParams, TypeInfo(TScStrings));

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScLists', ImportUnit);
end.

