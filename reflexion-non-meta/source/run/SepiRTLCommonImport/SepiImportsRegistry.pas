{*
  Importe l'unité Registry dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsRegistry;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Registry, Windows;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsERegistryException = class(ERegistryException)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTRegistry = class(TRegistry)
  private
    procedure SetRootKey(Value: HKEY);
    constructor Create_0;
    constructor Create_1(AAccess: LongWord);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTRegIniFile = class(TRegIniFile)
  private
    constructor Create_0(const FileName: string);
    constructor Create_1(const FileName: string; AAccess: LongWord);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTRegistryIniFile = class(TRegistryIniFile)
  private
    constructor Create_0(const FileName: string);
    constructor Create_1(const FileName: string; AAccess: LongWord);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{---------------------------}
{ ERegistryException import }
{---------------------------}

class function TSepiImportsERegistryException.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ERegistryException));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------}
{ TRegKeyInfo import }
{--------------------}

function SepiImportTRegKeyInfo(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TRegKeyInfo', False, True);

  with Result do
  begin
    AddField('NumSubKeys', System.TypeInfo(Integer));
    AddField('MaxSubKeyLen', System.TypeInfo(Integer));
    AddField('NumValues', System.TypeInfo(Integer));
    AddField('MaxValueLen', System.TypeInfo(Integer));
    AddField('MaxDataLen', System.TypeInfo(Integer));
    AddField('FileTime', 'TFileTime');

    Complete;
  end;
end;

{---------------------}
{ TRegDataInfo import }
{---------------------}

function SepiImportTRegDataInfo(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TRegDataInfo', False, True);

  with Result do
  begin
    AddField('RegData', System.TypeInfo(TRegDataType));
    AddField('DataSize', System.TypeInfo(Integer));

    Complete;
  end;
end;

{------------------}
{ TRegistry import }
{------------------}

procedure TSepiImportsTRegistry.SetRootKey(Value: HKEY);
begin
  RootKey := Value;
end;

constructor TSepiImportsTRegistry.Create_0;
begin
  Create;
end;

constructor TSepiImportsTRegistry.Create_1(AAccess: LongWord);
begin
  Create(AAccess);
end;

class function TSepiImportsTRegistry.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TRegistry));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCurrentKey', System.TypeInfo(HKEY));
    AddField('FRootKey', System.TypeInfo(HKEY));
    AddField('FLazyWrite', System.TypeInfo(Boolean));
    AddField('FCurrentPath', System.TypeInfo(string));
    AddField('FCloseRootKey', System.TypeInfo(Boolean));
    AddField('FAccess', System.TypeInfo(LongWord));

    AddMethod('SetRootKey', @TSepiImportsTRegistry.SetRootKey,
      'procedure(Value: HKEY)');

    CurrentVisibility := mvProtected;

    AddMethod('ChangeKey', @TSepiImportsTRegistry.ChangeKey,
      'procedure(Value: HKey; const Path: string)');
    AddMethod('GetBaseKey', @TSepiImportsTRegistry.GetBaseKey,
      'function(Relative: Boolean): HKey');
    AddMethod('GetData', @TSepiImportsTRegistry.GetData,
      'function(const Name: string; Buffer: Pointer; BufSize: Integer ; var RegData: TRegDataType ) : Integer');
    AddMethod('GetKey', @TSepiImportsTRegistry.GetKey,
      'function(const Key: string): HKEY');
    AddMethod('PutData', @TSepiImportsTRegistry.PutData,
      'procedure(const Name: string; Buffer: Pointer; BufSize: Integer; RegData: TRegDataType)');
    AddMethod('SetCurrentKey', @TSepiImportsTRegistry.SetCurrentKey,
      'procedure(Value: HKEY)');

    CurrentVisibility := mvPublic;

    AddOverloadedMethod('Create', @TSepiImportsTRegistry.Create_0,
      'constructor');
    AddOverloadedMethod('Create', @TSepiImportsTRegistry.Create_1,
      'constructor(AAccess: LongWord)');
    AddMethod('Destroy', @TSepiImportsTRegistry.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('CloseKey', @TSepiImportsTRegistry.CloseKey,
      'procedure');
    AddMethod('CreateKey', @TSepiImportsTRegistry.CreateKey,
      'function(const Key: string): Boolean');
    AddMethod('DeleteKey', @TSepiImportsTRegistry.DeleteKey,
      'function(const Key: string): Boolean');
    AddMethod('DeleteValue', @TSepiImportsTRegistry.DeleteValue,
      'function(const Name: string): Boolean');
    AddMethod('GetDataInfo', @TSepiImportsTRegistry.GetDataInfo,
      'function(const ValueName: string; var Value: TRegDataInfo): Boolean');
    AddMethod('GetDataSize', @TSepiImportsTRegistry.GetDataSize,
      'function(const ValueName: string): Integer');
    AddMethod('GetDataType', @TSepiImportsTRegistry.GetDataType,
      'function(const ValueName: string): TRegDataType');
    AddMethod('GetKeyInfo', @TSepiImportsTRegistry.GetKeyInfo,
      'function(var Value: TRegKeyInfo): Boolean');
    AddMethod('GetKeyNames', @TSepiImportsTRegistry.GetKeyNames,
      'procedure(Strings: TStrings)');
    AddMethod('GetValueNames', @TSepiImportsTRegistry.GetValueNames,
      'procedure(Strings: TStrings)');
    AddMethod('HasSubKeys', @TSepiImportsTRegistry.HasSubKeys,
      'function: Boolean');
    AddMethod('KeyExists', @TSepiImportsTRegistry.KeyExists,
      'function(const Key: string): Boolean');
    AddMethod('LoadKey', @TSepiImportsTRegistry.LoadKey,
      'function(const Key, FileName: string): Boolean');
    AddMethod('MoveKey', @TSepiImportsTRegistry.MoveKey,
      'procedure(const OldName, NewName: string; Delete: Boolean)');
    AddMethod('OpenKey', @TSepiImportsTRegistry.OpenKey,
      'function(const Key: string; CanCreate: Boolean): Boolean');
    AddMethod('OpenKeyReadOnly', @TSepiImportsTRegistry.OpenKeyReadOnly,
      'function(const Key: String): Boolean');
    AddMethod('ReadCurrency', @TSepiImportsTRegistry.ReadCurrency,
      'function(const Name: string): Currency');
    AddMethod('ReadBinaryData', @TSepiImportsTRegistry.ReadBinaryData,
      'function(const Name: string; var Buffer; BufSize: Integer): Integer');
    AddMethod('ReadBool', @TSepiImportsTRegistry.ReadBool,
      'function(const Name: string): Boolean');
    AddMethod('ReadDate', @TSepiImportsTRegistry.ReadDate,
      'function(const Name: string): TDateTime');
    AddMethod('ReadDateTime', @TSepiImportsTRegistry.ReadDateTime,
      'function(const Name: string): TDateTime');
    AddMethod('ReadFloat', @TSepiImportsTRegistry.ReadFloat,
      'function(const Name: string): Double');
    AddMethod('ReadInteger', @TSepiImportsTRegistry.ReadInteger,
      'function(const Name: string): Integer');
    AddMethod('ReadString', @TSepiImportsTRegistry.ReadString,
      'function(const Name: string): string');
    AddMethod('ReadTime', @TSepiImportsTRegistry.ReadTime,
      'function(const Name: string): TDateTime');
    AddMethod('RegistryConnect', @TSepiImportsTRegistry.RegistryConnect,
      'function(const UNCName: string): Boolean');
    AddMethod('RenameValue', @TSepiImportsTRegistry.RenameValue,
      'procedure(const OldName, NewName: string)');
    AddMethod('ReplaceKey', @TSepiImportsTRegistry.ReplaceKey,
      'function(const Key, FileName, BackUpFileName: string): Boolean');
    AddMethod('RestoreKey', @TSepiImportsTRegistry.RestoreKey,
      'function(const Key, FileName: string): Boolean');
    AddMethod('SaveKey', @TSepiImportsTRegistry.SaveKey,
      'function(const Key, FileName: string): Boolean');
    AddMethod('UnLoadKey', @TSepiImportsTRegistry.UnLoadKey,
      'function(const Key: string): Boolean');
    AddMethod('ValueExists', @TSepiImportsTRegistry.ValueExists,
      'function(const Name: string): Boolean');
    AddMethod('WriteCurrency', @TSepiImportsTRegistry.WriteCurrency,
      'procedure(const Name: string; Value: Currency)');
    AddMethod('WriteBinaryData', @TSepiImportsTRegistry.WriteBinaryData,
      'procedure(const Name: string; var Buffer; BufSize: Integer)');
    AddMethod('WriteBool', @TSepiImportsTRegistry.WriteBool,
      'procedure(const Name: string; Value: Boolean)');
    AddMethod('WriteDate', @TSepiImportsTRegistry.WriteDate,
      'procedure(const Name: string; Value: TDateTime)');
    AddMethod('WriteDateTime', @TSepiImportsTRegistry.WriteDateTime,
      'procedure(const Name: string; Value: TDateTime)');
    AddMethod('WriteFloat', @TSepiImportsTRegistry.WriteFloat,
      'procedure(const Name: string; Value: Double)');
    AddMethod('WriteInteger', @TSepiImportsTRegistry.WriteInteger,
      'procedure(const Name: string; Value: Integer)');
    AddMethod('WriteString', @TSepiImportsTRegistry.WriteString,
      'procedure(const Name, Value: string)');
    AddMethod('WriteExpandString', @TSepiImportsTRegistry.WriteExpandString,
      'procedure(const Name, Value: string)');
    AddMethod('WriteTime', @TSepiImportsTRegistry.WriteTime,
      'procedure(const Name: string; Value: TDateTime)');

    AddProperty('CurrentKey', 'property: HKEY',
      'FCurrentKey', '');
    AddProperty('CurrentPath', 'property: string',
      'FCurrentPath', '');
    AddProperty('LazyWrite', 'property: Boolean',
      'FLazyWrite', 'FLazyWrite');
    AddProperty('RootKey', 'property: HKEY',
      'FRootKey', 'SetRootKey');
    AddProperty('Access', 'property: LongWord',
      'FAccess', 'FAccess');

    Complete;
  end;
end;

{--------------------}
{ TRegIniFile import }
{--------------------}

constructor TSepiImportsTRegIniFile.Create_0(const FileName: string);
begin
  Create(FileName);
end;

constructor TSepiImportsTRegIniFile.Create_1(const FileName: string; AAccess: LongWord);
begin
  Create(FileName, AAccess);
end;

class function TSepiImportsTRegIniFile.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TRegIniFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFileName', System.TypeInfo(string));

    CurrentVisibility := mvPublic;

    AddOverloadedMethod('Create', @TSepiImportsTRegIniFile.Create_0,
      'constructor(const FileName: string)');
    AddOverloadedMethod('Create', @TSepiImportsTRegIniFile.Create_1,
      'constructor(const FileName: string; AAccess: LongWord)');
    AddMethod('ReadString', @TSepiImportsTRegIniFile.ReadString,
      'function(const Section, Ident, Default: string): string');
    AddMethod('ReadInteger', @TSepiImportsTRegIniFile.ReadInteger,
      'function(const Section, Ident: string; Default: Longint ) : Longint');
    AddMethod('WriteInteger', @TSepiImportsTRegIniFile.WriteInteger,
      'procedure(const Section, Ident: string; Value: Longint)');
    AddMethod('WriteString', @TSepiImportsTRegIniFile.WriteString,
      'procedure(const Section, Ident, Value: String)');
    AddMethod('ReadBool', @TSepiImportsTRegIniFile.ReadBool,
      'function(const Section, Ident: string; Default: Boolean): Boolean');
    AddMethod('WriteBool', @TSepiImportsTRegIniFile.WriteBool,
      'procedure(const Section, Ident: string; Value: Boolean)');
    AddMethod('ReadSection', @TSepiImportsTRegIniFile.ReadSection,
      'procedure(const Section: string; Strings: TStrings)');
    AddMethod('ReadSections', @TSepiImportsTRegIniFile.ReadSections,
      'procedure(Strings: TStrings)');
    AddMethod('ReadSectionValues', @TSepiImportsTRegIniFile.ReadSectionValues,
      'procedure(const Section: string; Strings: TStrings)');
    AddMethod('EraseSection', @TSepiImportsTRegIniFile.EraseSection,
      'procedure(const Section: string)');
    AddMethod('DeleteKey', @TSepiImportsTRegIniFile.DeleteKey,
      'procedure(const Section, Ident: String)');

    AddProperty('FileName', 'property: string',
      'FFileName', '');

    Complete;
  end;
end;

{-------------------------}
{ TRegistryIniFile import }
{-------------------------}

constructor TSepiImportsTRegistryIniFile.Create_0(const FileName: string);
begin
  Create(FileName);
end;

constructor TSepiImportsTRegistryIniFile.Create_1(const FileName: string; AAccess: LongWord);
begin
  Create(FileName, AAccess);
end;

class function TSepiImportsTRegistryIniFile.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TRegistryIniFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FRegIniFile', System.TypeInfo(TRegIniFile));

    CurrentVisibility := mvPublic;

    AddOverloadedMethod('Create', @TSepiImportsTRegistryIniFile.Create_0,
      'constructor(const FileName: string)');
    AddOverloadedMethod('Create', @TSepiImportsTRegistryIniFile.Create_1,
      'constructor(const FileName: string; AAccess: LongWord)');
    AddMethod('Destroy', @TSepiImportsTRegistryIniFile.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('ReadDate', @TSepiImportsTRegistryIniFile.ReadDate,
      'function(const Section, Name: string; Default: TDateTime): TDateTime',
      mlkOverride);
    AddMethod('ReadDateTime', @TSepiImportsTRegistryIniFile.ReadDateTime,
      'function(const Section, Name: string; Default: TDateTime): TDateTime',
      mlkOverride);
    AddMethod('ReadInteger', @TSepiImportsTRegistryIniFile.ReadInteger,
      'function(const Section, Ident: string; Default: Longint): Longint',
      mlkOverride);
    AddMethod('ReadFloat', @TSepiImportsTRegistryIniFile.ReadFloat,
      'function(const Section, Name: string; Default: Double): Double',
      mlkOverride);
    AddMethod('ReadString', @TSepiImportsTRegistryIniFile.ReadString,
      'function(const Section, Ident, Default: string): string',
      mlkOverride);
    AddMethod('ReadTime', @TSepiImportsTRegistryIniFile.ReadTime,
      'function(const Section, Name: string; Default: TDateTime): TDateTime',
      mlkOverride);
    AddMethod('ReadBinaryStream', @TSepiImportsTRegistryIniFile.ReadBinaryStream,
      'function(const Section, Name: string; Value: TStream): Integer',
      mlkOverride);
    AddMethod('WriteDate', @TSepiImportsTRegistryIniFile.WriteDate,
      'procedure(const Section, Name: string; Value: TDateTime)',
      mlkOverride);
    AddMethod('WriteDateTime', @TSepiImportsTRegistryIniFile.WriteDateTime,
      'procedure(const Section, Name: string; Value: TDateTime)',
      mlkOverride);
    AddMethod('WriteFloat', @TSepiImportsTRegistryIniFile.WriteFloat,
      'procedure(const Section, Name: string; Value: Double)',
      mlkOverride);
    AddMethod('WriteInteger', @TSepiImportsTRegistryIniFile.WriteInteger,
      'procedure(const Section, Ident: string; Value: Longint)',
      mlkOverride);
    AddMethod('WriteString', @TSepiImportsTRegistryIniFile.WriteString,
      'procedure(const Section, Ident, Value: String)',
      mlkOverride);
    AddMethod('WriteTime', @TSepiImportsTRegistryIniFile.WriteTime,
      'procedure(const Section, Name: string; Value: TDateTime)',
      mlkOverride);
    AddMethod('WriteBinaryStream', @TSepiImportsTRegistryIniFile.WriteBinaryStream,
      'procedure(const Section, Name: string; Value: TStream)',
      mlkOverride);
    AddMethod('ReadSection', @TSepiImportsTRegistryIniFile.ReadSection,
      'procedure(const Section: string; Strings: TStrings)',
      mlkOverride);
    AddOverloadedMethod('ReadSections', nil,
      'procedure(Strings: TStrings)',
      mlkOverride);
    AddOverloadedMethod('ReadSections', nil,
      'procedure(const Section: string; Strings: TStrings)',
      mlkOverride);
    AddMethod('ReadSectionValues', @TSepiImportsTRegistryIniFile.ReadSectionValues,
      'procedure(const Section: string; Strings: TStrings)',
      mlkOverride);
    AddMethod('EraseSection', @TSepiImportsTRegistryIniFile.EraseSection,
      'procedure(const Section: string)',
      mlkOverride);
    AddMethod('DeleteKey', @TSepiImportsTRegistryIniFile.DeleteKey,
      'procedure(const Section, Ident: String)',
      mlkOverride);
    AddMethod('UpdateFile', @TSepiImportsTRegistryIniFile.UpdateFile,
      'procedure',
      mlkOverride);

    AddProperty('RegIniFile', 'property: TRegIniFile',
      'FRegIniFile', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'Registry',
    ['Windows', 'Classes', 'SysUtils', 'IniFiles']);

  // Types
  TSepiImportsERegistryException.SepiImport(Result);
  SepiImportTRegKeyInfo(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TRegDataType));
  SepiImportTRegDataInfo(Result);
  TSepiImportsTRegistry.SepiImport(Result);
  TSepiImportsTRegIniFile.SepiImport(Result);
  TSepiImportsTRegistryIniFile.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Registry', ImportUnit);
end.

