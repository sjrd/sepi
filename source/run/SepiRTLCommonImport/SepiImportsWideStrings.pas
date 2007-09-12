{*
  Importe l'unité WideStrings dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsWideStrings;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, WideStrings, Classes;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTWideStrings = class(TWideStrings)
  private
    function GetCommaText: WideString;
    function GetDelimitedText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure SetCommaText(const Value: WideString);
    procedure SetDelimitedText(const Value: WideString);
    procedure SetStringsAdapter(const Value: IWideStringsAdapter);
    procedure SetValue(const Name, Value: WideString);
    function GetDelimiter: WideChar;
    procedure SetDelimiter(const Value: WideChar);
    function GetLineBreak: WideString;
    procedure SetLineBreak(const Value: WideString);
    function GetQuoteChar: WideChar;
    procedure SetQuoteChar(const Value: WideChar);
    function GetNameValueSeparator: WideChar;
    procedure SetNameValueSeparator(const Value: WideChar);
    function GetValueFromIndex(Index: Integer): WideString;
    procedure SetValueFromIndex(Index: Integer; const Value: WideString);
    procedure Error_0(const Msg: WideString; Data: Integer);
    procedure Error_1(Msg: PResStringRec; Data: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTWideStringList = class(TWideStringList)
  private
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{----------------------------}
{ IWideStringsAdapter import }
{----------------------------}

function SepiImportIWideStringsAdapter(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IWideStringsAdapter));

  with Result do
  begin
    AddMethod('ReferenceStrings',
      'procedure(S: TWideStrings)', ccRegister);
    AddMethod('ReleaseStrings',
      'procedure', ccRegister);

    Complete;
  end;
end;

{---------------------}
{ TWideStrings import }
{---------------------}

function TSepiImportsTWideStrings.GetCommaText: WideString;
begin
  Result := CommaText;
end;

function TSepiImportsTWideStrings.GetDelimitedText: WideString;
begin
  Result := DelimitedText;
end;

function TSepiImportsTWideStrings.GetName(Index: Integer): WideString;
begin
  Result := Names[Index];
end;

function TSepiImportsTWideStrings.GetValue(const Name: WideString): WideString;
begin
  Result := Values[Name];
end;

procedure TSepiImportsTWideStrings.SetCommaText(const Value: WideString);
begin
  CommaText := Value;
end;

procedure TSepiImportsTWideStrings.SetDelimitedText(const Value: WideString);
begin
  DelimitedText := Value;
end;

procedure TSepiImportsTWideStrings.SetStringsAdapter(
  const Value: IWideStringsAdapter);
begin
  StringsAdapter := Value;
end;

procedure TSepiImportsTWideStrings.SetValue(const Name, Value: WideString);
begin
  Values[Name] := Value;
end;

function TSepiImportsTWideStrings.GetDelimiter: WideChar;
begin
  Result := Delimiter;
end;

procedure TSepiImportsTWideStrings.SetDelimiter(const Value: WideChar);
begin
  Delimiter := Value;
end;

function TSepiImportsTWideStrings.GetLineBreak: WideString;
begin
  Result := LineBreak;
end;

procedure TSepiImportsTWideStrings.SetLineBreak(const Value: WideString);
begin
  LineBreak := Value;
end;

function TSepiImportsTWideStrings.GetQuoteChar: WideChar;
begin
  Result := QuoteChar;
end;

procedure TSepiImportsTWideStrings.SetQuoteChar(const Value: WideChar);
begin
  QuoteChar := Value;
end;

function TSepiImportsTWideStrings.GetNameValueSeparator: WideChar;
begin
  Result := NameValueSeparator;
end;

procedure TSepiImportsTWideStrings.SetNameValueSeparator(
  const Value: WideChar);
begin
  NameValueSeparator := Value;
end;

function TSepiImportsTWideStrings.GetValueFromIndex(Index: Integer):
WideString;
begin
  Result := ValueFromIndex[Index];
end;

procedure TSepiImportsTWideStrings.SetValueFromIndex(Index: Integer;
  const Value: WideString);
begin
  ValueFromIndex[Index] := Value;
end;

procedure TSepiImportsTWideStrings.Error_0(const Msg: WideString;
  Data: Integer);
begin
  Error(Msg, Data);
end;

procedure TSepiImportsTWideStrings.Error_1(Msg: PResStringRec; Data: Integer);
begin
  Error(Msg, Data);
end;

class function TSepiImportsTWideStrings.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TWideStrings'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TWideStrings));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDefined', System.TypeInfo(TStringsDefined));
    AddField('FDelimiter', System.TypeInfo(WideChar));
    AddField('FLineBreak', System.TypeInfo(WideString));
    AddField('FQuoteChar', System.TypeInfo(WideChar));
    AddField('FNameValueSeparator', System.TypeInfo(WideChar));
    AddField('FUpdateCount', System.TypeInfo(Integer));
    AddField('FAdapter', System.TypeInfo(IWideStringsAdapter));

    AddMethod('GetCommaText', @TSepiImportsTWideStrings.GetCommaText,
      'function: WideString');
    AddMethod('GetDelimitedText', @TSepiImportsTWideStrings.GetDelimitedText,
      'function: WideString');
    AddMethod('GetName', @TSepiImportsTWideStrings.GetName,
      'function(Index: Integer): WideString');
    AddMethod('GetValue', @TSepiImportsTWideStrings.GetValue,
      'function(const Name: WideString): WideString');
    AddMethod('ReadData', nil,
      'procedure(Reader: TReader)');
    AddMethod('SetCommaText', @TSepiImportsTWideStrings.SetCommaText,
      'procedure(const Value: WideString)');
    AddMethod('SetDelimitedText', @TSepiImportsTWideStrings.SetDelimitedText,
      'procedure(const Value: WideString)');
    AddMethod('SetStringsAdapter', @TSepiImportsTWideStrings.SetStringsAdapter,
      'procedure(const Value: IWideStringsAdapter)');
    AddMethod('SetValue', @TSepiImportsTWideStrings.SetValue,
      'procedure(const Name, Value: WideString)');
    AddMethod('WriteData', nil,
      'procedure(Writer: TWriter)');
    AddMethod('GetDelimiter', @TSepiImportsTWideStrings.GetDelimiter,
      'function: WideChar');
    AddMethod('SetDelimiter', @TSepiImportsTWideStrings.SetDelimiter,
      'procedure(const Value: WideChar)');
    AddMethod('GetLineBreak', @TSepiImportsTWideStrings.GetLineBreak,
      'function: WideString');
    AddMethod('SetLineBreak', @TSepiImportsTWideStrings.SetLineBreak,
      'procedure(const Value: WideString)');
    AddMethod('GetQuoteChar', @TSepiImportsTWideStrings.GetQuoteChar,
      'function: WideChar');
    AddMethod('SetQuoteChar', @TSepiImportsTWideStrings.SetQuoteChar,
      'procedure(const Value: WideChar)');
    AddMethod('GetNameValueSeparator',
      @TSepiImportsTWideStrings.GetNameValueSeparator,
      'function: WideChar');
    AddMethod('SetNameValueSeparator',
      @TSepiImportsTWideStrings.SetNameValueSeparator,
      'procedure(const Value: WideChar)');
    AddMethod('GetValueFromIndex', @TSepiImportsTWideStrings.GetValueFromIndex,
      'function(Index: Integer): WideString');
    AddMethod('SetValueFromIndex', @TSepiImportsTWideStrings.SetValueFromIndex,
      'procedure(Index: Integer; const Value: WideString)');

    CurrentVisibility := mvProtected;

    AddMethod('AssignTo', @TSepiImportsTWideStrings.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);
    AddMethod('DefineProperties', @TSepiImportsTWideStrings.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddOverloadedMethod('Error', @TSepiImportsTWideStrings.Error_0,
      'procedure(const Msg: WideString; Data: Integer)');
    AddOverloadedMethod('Error', @TSepiImportsTWideStrings.Error_1,
      'procedure(Msg: PResStringRec; Data: Integer)');
    AddMethod('ExtractName', @TSepiImportsTWideStrings.ExtractName,
      'function(const S: WideString): WideString');
    AddMethod('Get', nil,
      'function(Index: Integer): WideString',
      mlkVirtual, True);
    AddMethod('GetCapacity', @TSepiImportsTWideStrings.GetCapacity,
      'function: Integer',
      mlkVirtual);
    AddMethod('GetCount', nil,
      'function: Integer',
      mlkVirtual, True);
    AddMethod('GetObject', @TSepiImportsTWideStrings.GetObject,
      'function(Index: Integer): TObject',
      mlkVirtual);
    AddMethod('GetTextStr', @TSepiImportsTWideStrings.GetTextStr,
      'function: WideString',
      mlkVirtual);
    AddMethod('Put', @TSepiImportsTWideStrings.Put,
      'procedure(Index: Integer; const S: WideString)',
      mlkVirtual);
    AddMethod('PutObject', @TSepiImportsTWideStrings.PutObject,
      'procedure(Index: Integer; AObject: TObject)',
      mlkVirtual);
    AddMethod('SetCapacity', @TSepiImportsTWideStrings.SetCapacity,
      'procedure(NewCapacity: Integer)',
      mlkVirtual);
    AddMethod('SetTextStr', @TSepiImportsTWideStrings.SetTextStr,
      'procedure(const Value: WideString)',
      mlkVirtual);
    AddMethod('SetUpdateState', @TSepiImportsTWideStrings.SetUpdateState,
      'procedure(Updating: Boolean)',
      mlkVirtual);

    AddProperty('UpdateCount', 'property: Integer',
      'FUpdateCount', '');

    AddMethod('CompareStrings', @TSepiImportsTWideStrings.CompareStrings,
      'function(const S1, S2: WideString): Integer',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTWideStrings.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTWideStrings.Add,
      'function(const S: WideString): Integer',
      mlkVirtual);
    AddMethod('AddObject', @TSepiImportsTWideStrings.AddObject,
      'function(const S: WideString; AObject: TObject): Integer',
      mlkVirtual);
    AddMethod('Append', @TSepiImportsTWideStrings.Append,
      'procedure(const S: WideString)');
    AddOverloadedMethod('AddStrings', nil,
      'procedure(Strings: TStrings)',
      mlkVirtual);
    AddOverloadedMethod('AddStrings', nil,
      'procedure(Strings: TWideStrings)',
      mlkVirtual);
    AddMethod('Assign', @TSepiImportsTWideStrings.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('BeginUpdate', @TSepiImportsTWideStrings.BeginUpdate,
      'procedure');
    AddMethod('Clear', nil,
      'procedure',
      mlkVirtual, True);
    AddMethod('Delete', nil,
      'procedure(Index: Integer)',
      mlkVirtual, True);
    AddMethod('EndUpdate', @TSepiImportsTWideStrings.EndUpdate,
      'procedure');
    AddMethod('Equals', @TSepiImportsTWideStrings.Equals,
      'function(Strings: TWideStrings): Boolean');
    AddMethod('Exchange', @TSepiImportsTWideStrings.Exchange,
      'procedure(Index1, Index2: Integer)',
      mlkVirtual);
    AddMethod('GetText', @TSepiImportsTWideStrings.GetText,
      'function: PwideChar',
      mlkVirtual);
    AddMethod('IndexOf', @TSepiImportsTWideStrings.IndexOf,
      'function(const S: WideString): Integer',
      mlkVirtual);
    AddMethod('IndexOfName', @TSepiImportsTWideStrings.IndexOfName,
      'function(const Name: WideString): Integer',
      mlkVirtual);
    AddMethod('IndexOfObject', @TSepiImportsTWideStrings.IndexOfObject,
      'function(AObject: TObject): Integer',
      mlkVirtual);
    AddMethod('Insert', nil,
      'procedure(Index: Integer; const S: WideString)',
      mlkVirtual, True);
    AddMethod('InsertObject', @TSepiImportsTWideStrings.InsertObject,
      'procedure(Index: Integer; const S: WideString; AObject: TObject )',
      mlkVirtual);
    AddMethod('LoadFromFile', @TSepiImportsTWideStrings.LoadFromFile,
      'procedure(const FileName: WideString)',
      mlkVirtual);
    AddMethod('LoadFromStream', @TSepiImportsTWideStrings.LoadFromStream,
      'procedure(Stream: TStream)',
      mlkVirtual);
    AddMethod('Move', @TSepiImportsTWideStrings.Move,
      'procedure(CurIndex, NewIndex: Integer)',
      mlkVirtual);
    AddMethod('SaveToFile', @TSepiImportsTWideStrings.SaveToFile,
      'procedure(const FileName: WideString)',
      mlkVirtual);
    AddMethod('SaveToStream', @TSepiImportsTWideStrings.SaveToStream,
      'procedure(Stream: TStream)',
      mlkVirtual);
    AddMethod('SetText', @TSepiImportsTWideStrings.SetText,
      'procedure(Text: PwideChar)',
      mlkVirtual);

    AddProperty('Capacity', 'property: Integer',
      'GetCapacity', 'SetCapacity');
    AddProperty('CommaText', 'property: WideString',
      'GetCommaText', 'SetCommaText');
    AddProperty('Count', 'property: Integer',
      'GetCount', '');
    AddProperty('Delimiter', 'property: WideChar',
      'GetDelimiter', 'SetDelimiter');
    AddProperty('DelimitedText', 'property: WideString',
      'GetDelimitedText', 'SetDelimitedText');
    AddProperty('LineBreak', 'property: WideString',
      'GetLineBreak', 'SetLineBreak');
    AddProperty('Names', 'property[Index: Integer]: WideString',
      'GetName', '');
    AddProperty('Objects', 'property[Index: Integer]: TObject',
      'GetObject', 'PutObject');
    AddProperty('QuoteChar', 'property: WideChar',
      'GetQuoteChar', 'SetQuoteChar');
    AddProperty('Values', 'property[const Name: WideString]: WideString',
      'GetValue', 'SetValue');
    AddProperty('ValueFromIndex', 'property[Index: Integer]: WideString',
      'GetValueFromIndex', 'SetValueFromIndex');
    AddProperty('NameValueSeparator', 'property: WideChar',
      'GetNameValueSeparator', 'SetNameValueSeparator');
    AddProperty('Strings', 'property[Index: Integer]: WideString',
      'Get', 'Put', True);
    AddProperty('Text', 'property: WideString',
      'GetTextStr', 'SetTextStr');
    AddProperty('StringsAdapter', 'property: IWideStringsAdapter',
      'FAdapter', 'SetStringsAdapter');

    Complete;
  end;
end;

{------------------------}
{ TWideStringItem import }
{------------------------}

function SepiImportTWideStringItem(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWideStringItem', False, True,
    TypeInfo(TWideStringItem));

  with Result do
  begin
    AddField('FString', System.TypeInfo(WideString));
    AddField('FObject', System.TypeInfo(TObject));

    Complete;
  end;
end;

{------------------------}
{ TWideStringList import }
{------------------------}

procedure TSepiImportsTWideStringList.SetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TSepiImportsTWideStringList.SetCaseSensitive(const Value: Boolean);
begin
  CaseSensitive := Value;
end;

class function TSepiImportsTWideStringList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TWideStringList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TWideStringList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FList', 'PWideStringItemList');
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
      'procedure(L, R: Integer; SCompare: TWideStringListSortCompare)');
    AddMethod('SetSorted', @TSepiImportsTWideStringList.SetSorted,
      'procedure(Value: Boolean)');
    AddMethod('SetCaseSensitive',
      @TSepiImportsTWideStringList.SetCaseSensitive,
      'procedure(const Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTWideStringList.Changed,
      'procedure',
      mlkVirtual);
    AddMethod('Changing', @TSepiImportsTWideStringList.Changing,
      'procedure',
      mlkVirtual);
    AddMethod('Get', @TSepiImportsTWideStringList.Get,
      'function(Index: Integer): WideString',
      mlkOverride);
    AddMethod('GetCapacity', @TSepiImportsTWideStringList.GetCapacity,
      'function: Integer',
      mlkOverride);
    AddMethod('GetCount', @TSepiImportsTWideStringList.GetCount,
      'function: Integer',
      mlkOverride);
    AddMethod('GetObject', @TSepiImportsTWideStringList.GetObject,
      'function(Index: Integer): TObject',
      mlkOverride);
    AddMethod('Put', @TSepiImportsTWideStringList.Put,
      'procedure(Index: Integer; const S: WideString)',
      mlkOverride);
    AddMethod('PutObject', @TSepiImportsTWideStringList.PutObject,
      'procedure(Index: Integer; AObject: TObject)',
      mlkOverride);
    AddMethod('SetCapacity', @TSepiImportsTWideStringList.SetCapacity,
      'procedure(NewCapacity: Integer)',
      mlkOverride);
    AddMethod('SetUpdateState', @TSepiImportsTWideStringList.SetUpdateState,
      'procedure(Updating: Boolean)',
      mlkOverride);
    AddMethod('CompareStrings', @TSepiImportsTWideStringList.CompareStrings,
      'function(const S1, S2: WideString): Integer',
      mlkOverride);
    AddMethod('InsertItem', @TSepiImportsTWideStringList.InsertItem,
      'procedure(Index: Integer; const S: WideString; AObject: TObject)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTWideStringList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTWideStringList.Add,
      'function(const S: WideString): Integer',
      mlkOverride);
    AddMethod('AddObject', @TSepiImportsTWideStringList.AddObject,
      'function(const S: WideString; AObject: TObject): Integer',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTWideStringList.Clear,
      'procedure',
      mlkOverride);
    AddMethod('Delete', @TSepiImportsTWideStringList.Delete,
      'procedure(Index: Integer)',
      mlkOverride);
    AddMethod('Exchange', @TSepiImportsTWideStringList.Exchange,
      'procedure(Index1, Index2: Integer)',
      mlkOverride);
    AddMethod('Find', @TSepiImportsTWideStringList.Find,
      'function(const S: WideString; var Index: Integer): Boolean',
      mlkVirtual);
    AddMethod('IndexOf', @TSepiImportsTWideStringList.IndexOf,
      'function(const S: WideString): Integer',
      mlkOverride);
    AddMethod('Insert', @TSepiImportsTWideStringList.Insert,
      'procedure(Index: Integer; const S: WideString)',
      mlkOverride);
    AddMethod('InsertObject', @TSepiImportsTWideStringList.InsertObject,
      'procedure(Index: Integer; const S: WideString; AObject: TObject )',
      mlkOverride);
    AddMethod('Sort', @TSepiImportsTWideStringList.Sort,
      'procedure',
      mlkVirtual);
    AddMethod('CustomSort', @TSepiImportsTWideStringList.CustomSort,
      'procedure(Compare: TWideStringListSortCompare)',
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

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'WideStrings',
    ['Classes']);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TWideStrings));
  SepiImportIWideStringsAdapter(Result);
  TSepiImportsTWideStrings.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TWideStringList));
  TSepiPointerType.Create(Result, 'PWideStringItem',
    TypeInfo(TWideStringItem), True);
  SepiImportTWideStringItem(Result);
  TSepiPointerType.Create(Result, 'PWideStringItemList',
    TypeInfo(TWideStringItemList), True);
  TSepiArrayType.Create(Result, 'TWideStringItemList',
    [0, MaxListSize], TypeInfo(TWideStringItem), True,
    TypeInfo(TWideStringItemList));
  TSepiMethodRefType.Create(Result, 'TWideStringListSortCompare',
    'function(List: TWideStringList; Index1, Index2: Integer): Integer');
  TSepiImportsTWideStringList.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('WideStrings', ImportUnit);
end.

