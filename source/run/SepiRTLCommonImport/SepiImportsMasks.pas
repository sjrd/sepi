{*
  Importe l'unité Masks dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsMasks;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Masks;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEMaskException = class(EMaskException)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMask = class(TMask)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{-----------------------}
{ EMaskException import }
{-----------------------}

class function TSepiImportsEMaskException.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EMaskException));

  with Result do
  begin

    Complete;
  end;
end;

{--------------}
{ TMask import }
{--------------}

class function TSepiImportsTMask.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMask));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMask', 'Pointer');
    AddField('FSize', System.TypeInfo(Integer));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMask.Create,
      'constructor(const MaskValue: string)');
    AddMethod('Destroy', @TSepiImportsTMask.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Matches', @TSepiImportsTMask.Matches,
      'function(const Filename: string): Boolean');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'Masks',
    ['SysUtils']);

  // Types
  TSepiImportsEMaskException.SepiImport(Result);
  TSepiImportsTMask.SepiImport(Result);

  // Routines
  TSepiMetaMethod.Create(Result, 'MatchesMask', @MatchesMask,
    'function(const Filename, Mask: string): Boolean');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Masks', ImportUnit);
end.

