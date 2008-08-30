{*
  Importe l'unité ScMD5 dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScMD5;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScMD5;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------------}
{ TMD5Digest import }
{-------------------}

function SepiImportTMD5Digest(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TMD5Digest', False, True);

  with Result do
  begin
    AddFieldAfter('A', System.TypeInfo(Longint), '');
    AddField('B', System.TypeInfo(Longint), True);
    AddField('C', System.TypeInfo(Longint), True);
    AddField('D', System.TypeInfo(Longint), True);
    AddFieldAfter('V', '$1', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ScMD5',
    ['Windows', 'SysUtils', 'Classes']);

  // Types
  TSepiPointerType.Create(Result, 'PMD5Digest', 'TMD5Digest', True);
  TSepiArrayType.Create(Result, '$1', TypeInfo(Integer),
    0, 15, TypeInfo(Byte), True);
  SepiImportTMD5Digest(Result);

  // Routines
  TSepiMethod.Create(Result, 'MD5String', @MD5String,
    'function(const S : string) : TMD5Digest');
  TSepiMethod.Create(Result, 'MD5File', @MD5File,
    'function(const FileName : TFileName) : TMD5Digest');
  TSepiMethod.Create(Result, 'MD5Stream', @MD5Stream,
    'function(const Stream : TStream) : TMD5Digest');
  TSepiMethod.Create(Result, 'MD5Buffer', @MD5Buffer,
    'function(const Buffer; Size : Integer) : TMD5Digest');
  TSepiMethod.Create(Result, 'MD5DigestToStr', @MD5DigestToStr,
    'function(const Digest : TMD5Digest) : string');
  TSepiMethod.Create(Result, 'StrToMD5Digest', @StrToMD5Digest,
    'function(Str : string) : TMD5Digest');
  TSepiMethod.Create(Result, 'MD5DigestCompare', @MD5DigestCompare,
    'function(const Digest1, Digest2 : TMD5Digest) : boolean');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScMD5', ImportUnit);
end.

