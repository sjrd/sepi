{*
  Importe l'unité IniFiles dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsIniFiles;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, IniFiles, Classes;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEIniFileException = class(EIniFileException)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomIniFile = class(TCustomIniFile)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TUnnamed_1 = array of PHashItem;

  TSepiImportsTStringHash = class(TStringHash)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTHashedStringList = class(THashedStringList)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTMemIniFile = class(TMemIniFile)
  private
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTIniFile = class(TIniFile)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{--------------------------}
{ EIniFileException import }
{--------------------------}

class function TSepiImportsEIniFileException.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EIniFileException));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ TCustomIniFile import }
{-----------------------}

class function TSepiImportsTCustomIniFile.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomIniFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFileName', System.TypeInfo(string));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomIniFile.Create,
      'constructor(const FileName: string)');
    AddMethod('SectionExists', @TSepiImportsTCustomIniFile.SectionExists,
      'function(const Section: string): Boolean');
    AddMethod('ReadString', nil,
      'function(const Section, Ident, Default: string): string',
      mlkVirtual, True);
    AddMethod('WriteString', nil,
      'procedure(const Section, Ident, Value: String)',
      mlkVirtual, True);
    AddMethod('ReadInteger', @TSepiImportsTCustomIniFile.ReadInteger,
      'function(const Section, Ident: string; Default: Longint): Longint',
      mlkVirtual);
    AddMethod('WriteInteger', @TSepiImportsTCustomIniFile.WriteInteger,
      'procedure(const Section, Ident: string; Value: Longint)',
      mlkVirtual);
    AddMethod('ReadBool', @TSepiImportsTCustomIniFile.ReadBool,
      'function(const Section, Ident: string; Default: Boolean): Boolean',
      mlkVirtual);
    AddMethod('WriteBool', @TSepiImportsTCustomIniFile.WriteBool,
      'procedure(const Section, Ident: string; Value: Boolean)',
      mlkVirtual);
    AddMethod('ReadBinaryStream', @TSepiImportsTCustomIniFile.ReadBinaryStream,
      'function(const Section, Name: string; Value: TStream): Integer',
      mlkVirtual);
    AddMethod('ReadDate', @TSepiImportsTCustomIniFile.ReadDate,
      'function(const Section, Name: string; Default: TDateTime): TDateTime',
      mlkVirtual);
    AddMethod('ReadDateTime', @TSepiImportsTCustomIniFile.ReadDateTime,
      'function(const Section, Name: string; Default: TDateTime): TDateTime',
      mlkVirtual);
    AddMethod('ReadFloat', @TSepiImportsTCustomIniFile.ReadFloat,
      'function(const Section, Name: string; Default: Double): Double',
      mlkVirtual);
    AddMethod('ReadTime', @TSepiImportsTCustomIniFile.ReadTime,
      'function(const Section, Name: string; Default: TDateTime): TDateTime',
      mlkVirtual);
    AddMethod('WriteBinaryStream',
      @TSepiImportsTCustomIniFile.WriteBinaryStream,
      'procedure(const Section, Name: string; Value: TStream)',
      mlkVirtual);
    AddMethod('WriteDate', @TSepiImportsTCustomIniFile.WriteDate,
      'procedure(const Section, Name: string; Value: TDateTime)',
      mlkVirtual);
    AddMethod('WriteDateTime', @TSepiImportsTCustomIniFile.WriteDateTime,
      'procedure(const Section, Name: string; Value: TDateTime)',
      mlkVirtual);
    AddMethod('WriteFloat', @TSepiImportsTCustomIniFile.WriteFloat,
      'procedure(const Section, Name: string; Value: Double)',
      mlkVirtual);
    AddMethod('WriteTime', @TSepiImportsTCustomIniFile.WriteTime,
      'procedure(const Section, Name: string; Value: TDateTime)',
      mlkVirtual);
    AddMethod('ReadSection', nil,
      'procedure(const Section: string; Strings: TStrings)',
      mlkVirtual, True);
    AddOverloadedMethod('ReadSections', nil,
      'procedure(Strings: TStrings)',
      mlkVirtual, True);
    AddOverloadedMethod('ReadSections', nil,
      'procedure(const Section: string; Strings: TStrings)',
      mlkVirtual);
    AddMethod('ReadSectionValues', nil,
      'procedure(const Section: string; Strings: TStrings)',
      mlkVirtual, True);
    AddMethod('EraseSection', nil,
      'procedure(const Section: string)',
      mlkVirtual, True);
    AddMethod('DeleteKey', nil,
      'procedure(const Section, Ident: String)',
      mlkVirtual, True);
    AddMethod('UpdateFile', nil,
      'procedure',
      mlkVirtual, True);
    AddMethod('ValueExists', @TSepiImportsTCustomIniFile.ValueExists,
      'function(const Section, Ident: string): Boolean',
      mlkVirtual);

    AddProperty('FileName', 'property: string',
      'FFileName', '');

    Complete;
  end;
end;

{------------------}
{ THashItem import }
{------------------}

function SepiImportTHashItem(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'THashItem', False, True,
    TypeInfo(THashItem));

  with Result do
  begin
    AddField('Next', 'PHashItem');
    AddField('Key', System.TypeInfo(string));
    AddField('Value', System.TypeInfo(Integer));

    Complete;
  end;
end;

{--------------------}
{ TStringHash import }
{--------------------}

class function TSepiImportsTStringHash.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TStringHash));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('Buckets', System.TypeInfo(TUnnamed_1));

    CurrentVisibility := mvProtected;

    AddMethod('Find', @TSepiImportsTStringHash.Find,
      'function(const Key: string): PPHashItem');
    AddMethod('HashOf', @TSepiImportsTStringHash.HashOf,
      'function(const Key: string): Cardinal',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTStringHash.Create,
      'constructor(Size: Cardinal = 256)');
    AddMethod('Destroy', @TSepiImportsTStringHash.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTStringHash.Add,
      'procedure(const Key: string; Value: Integer)');
    AddMethod('Clear', @TSepiImportsTStringHash.Clear,
      'procedure');
    AddMethod('Remove', @TSepiImportsTStringHash.Remove,
      'procedure(const Key: string)');
    AddMethod('Modify', @TSepiImportsTStringHash.Modify,
      'function(const Key: string; Value: Integer): Boolean');
    AddMethod('ValueOf', @TSepiImportsTStringHash.ValueOf,
      'function(const Key: string): Integer');

    Complete;
  end;
end;

{--------------------------}
{ THashedStringList import }
{--------------------------}

class function TSepiImportsTHashedStringList.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(THashedStringList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FValueHash', System.TypeInfo(TStringHash));
    AddField('FNameHash', System.TypeInfo(TStringHash));
    AddField('FValueHashValid', System.TypeInfo(Boolean));
    AddField('FNameHashValid', System.TypeInfo(Boolean));

    AddMethod('UpdateValueHash', nil,
      'procedure');
    AddMethod('UpdateNameHash', nil,
      'procedure');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTHashedStringList.Changed,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTHashedStringList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('IndexOf', @TSepiImportsTHashedStringList.IndexOf,
      'function(const S: string): Integer',
      mlkOverride);
    AddMethod('IndexOfName', @TSepiImportsTHashedStringList.IndexOfName,
      'function(const Name: string): Integer',
      mlkOverride);

    Complete;
  end;
end;

{--------------------}
{ TMemIniFile import }
{--------------------}

function TSepiImportsTMemIniFile.GetCaseSensitive: Boolean;
begin
  Result := CaseSensitive;
end;

procedure TSepiImportsTMemIniFile.SetCaseSensitive(Value: Boolean);
begin
  CaseSensitive := Value;
end;

class function TSepiImportsTMemIniFile.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMemIniFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FSections', System.TypeInfo(TStringList));

    AddMethod('AddSection', nil,
      'function(const Section: string): TStrings');
    AddMethod('GetCaseSensitive', @TSepiImportsTMemIniFile.GetCaseSensitive,
      'function: Boolean');
    AddMethod('LoadValues', nil,
      'procedure');
    AddMethod('SetCaseSensitive', @TSepiImportsTMemIniFile.SetCaseSensitive,
      'procedure(Value: Boolean)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMemIniFile.Create,
      'constructor(const FileName: string)');
    AddMethod('Destroy', @TSepiImportsTMemIniFile.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTMemIniFile.Clear,
      'procedure');
    AddMethod('DeleteKey', @TSepiImportsTMemIniFile.DeleteKey,
      'procedure(const Section, Ident: String)',
      mlkOverride);
    AddMethod('EraseSection', @TSepiImportsTMemIniFile.EraseSection,
      'procedure(const Section: string)',
      mlkOverride);
    AddMethod('GetStrings', @TSepiImportsTMemIniFile.GetStrings,
      'procedure(List: TStrings)');
    AddMethod('ReadSection', @TSepiImportsTMemIniFile.ReadSection,
      'procedure(const Section: string; Strings: TStrings)',
      mlkOverride);
    AddMethod('ReadSections', @TSepiImportsTMemIniFile.ReadSections,
      'procedure(Strings: TStrings)',
      mlkOverride);
    AddMethod('ReadSectionValues', @TSepiImportsTMemIniFile.ReadSectionValues,
      'procedure(const Section: string; Strings: TStrings)',
      mlkOverride);
    AddMethod('ReadString', @TSepiImportsTMemIniFile.ReadString,
      'function(const Section, Ident, Default: string): string',
      mlkOverride);
    AddMethod('Rename', @TSepiImportsTMemIniFile.Rename,
      'procedure(const FileName: string; Reload: Boolean)');
    AddMethod('SetStrings', @TSepiImportsTMemIniFile.SetStrings,
      'procedure(List: TStrings)');
    AddMethod('UpdateFile', @TSepiImportsTMemIniFile.UpdateFile,
      'procedure',
      mlkOverride);
    AddMethod('WriteString', @TSepiImportsTMemIniFile.WriteString,
      'procedure(const Section, Ident, Value: String)',
      mlkOverride);

    AddProperty('CaseSensitive', 'property: Boolean',
      'GetCaseSensitive', 'SetCaseSensitive');

    Complete;
  end;
end;

{-----------------}
{ TIniFile import }
{-----------------}

class function TSepiImportsTIniFile.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TIniFile));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTIniFile.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('ReadString', @TSepiImportsTIniFile.ReadString,
      'function(const Section, Ident, Default: string): string',
      mlkOverride);
    AddMethod('WriteString', @TSepiImportsTIniFile.WriteString,
      'procedure(const Section, Ident, Value: String)',
      mlkOverride);
    AddMethod('ReadSection', @TSepiImportsTIniFile.ReadSection,
      'procedure(const Section: string; Strings: TStrings)',
      mlkOverride);
    AddMethod('ReadSections', @TSepiImportsTIniFile.ReadSections,
      'procedure(Strings: TStrings)',
      mlkOverride);
    AddMethod('ReadSectionValues', @TSepiImportsTIniFile.ReadSectionValues,
      'procedure(const Section: string; Strings: TStrings)',
      mlkOverride);
    AddMethod('EraseSection', @TSepiImportsTIniFile.EraseSection,
      'procedure(const Section: string)',
      mlkOverride);
    AddMethod('DeleteKey', @TSepiImportsTIniFile.DeleteKey,
      'procedure(const Section, Ident: String)',
      mlkOverride);
    AddMethod('UpdateFile', @TSepiImportsTIniFile.UpdateFile,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'IniFiles',
    ['SysUtils', 'Classes']);

  // Types
  TSepiImportsEIniFileException.SepiImport(Result);
  TSepiImportsTCustomIniFile.SepiImport(Result);
  TSepiPointerType.Create(Result, 'PPHashItem', 'PHashItem', True);
  TSepiPointerType.Create(Result, 'PHashItem', TypeInfo(THashItem), True);
  SepiImportTHashItem(Result);
  TSepiDynArrayType(TSepiType.LoadFromTypeInfo(
    Result, TypeInfo(TUnnamed_1))).SetElementType('PHashItem');
  TSepiImportsTStringHash.SepiImport(Result);
  TSepiImportsTHashedStringList.SepiImport(Result);
  TSepiImportsTMemIniFile.SepiImport(Result);
  TSepiImportsTIniFile.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('IniFiles', ImportUnit);
end.

