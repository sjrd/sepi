{*
  Importe l'unité ScTypInfo dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsScTypInfo;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScTypInfo;

implementation

{ You must not localize any of the strings this unit contains! }

{---------------------}
{ TRecordField import }
{---------------------}

function SepiImportTRecordField(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TRecordField', True, True);

  with Result do
  begin
    AddField('TypeInfo', 'PPTypeInfo');
    AddField('Offset', System.TypeInfo(integer));

    Complete;
  end;
end;

{------------------------}
{ TRecordTypeData import }
{------------------------}

function SepiImportTRecordTypeData(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TRecordTypeData', True, True);

  with Result do
  begin
    AddField('Size', System.TypeInfo(integer));
    AddField('FieldCount', System.TypeInfo(integer));
    AddField('Fields', '$1');

    Complete;
  end;
end;

{-----------------------}
{ TArrayTypeData import }
{-----------------------}

function SepiImportTArrayTypeData(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TArrayTypeData', True, True);

  with Result do
  begin
    AddField('Size', System.TypeInfo(integer));
    AddField('Count', System.TypeInfo(integer));
    AddField('ElType', 'PPTypeInfo');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

procedure CopyData_0(const Source; var Dest; Size : integer; TypeInfo : PTypeInfo );
begin
  CopyData(Source, Dest, Size, TypeInfo);
end;

procedure CopyData_1(const Source; var Dest; TypeInfo : PTypeInfo);
begin
  CopyData(Source, Dest, TypeInfo);
end;

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ScTypInfo',
    ['TypInfo']);

  // Constants
  //TSepiConstant.Create(Result, 'NeedInitTypeKinds', NeedInitTypeKinds);

  // Types
  TSepiPointerType.Create(Result, 'PRecordField', 'TRecordField', True);
  SepiImportTRecordField(Result);
  TSepiPointerType.Create(Result, 'PRecordTypeData', 'TRecordTypeData', True);
  TSepiArrayType.Create(Result, '$1',
    [0, 0], 'TRecordField', True);
  SepiImportTRecordTypeData(Result);
  TSepiPointerType.Create(Result, 'PArrayTypeData', 'TArrayTypeData', True);
  SepiImportTArrayTypeData(Result);

  // Routines
  TSepiMethod.Create(Result, 'TypeSize', @TypeSize,
    'function(TypeInfo : PTypeInfo) : integer');
  TSepiMethod.CreateOverloaded(Result, 'CopyData', @CopyData_0,
    'procedure(const Source; var Dest; Size : integer; TypeInfo : PTypeInfo )');
  TSepiMethod.CreateOverloaded(Result, 'CopyData', @CopyData_1,
    'procedure(const Source; var Dest; TypeInfo : PTypeInfo)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScTypInfo', ImportUnit);
end.

