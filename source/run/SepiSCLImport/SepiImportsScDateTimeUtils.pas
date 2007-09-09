{*
  Importe l'unité ScDateTimeUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScDateTimeUtils;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScDateTimeUtils;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ScDateTimeUtils', []);

  // Routines
  TSepiMethod.Create(Result, 'GetMonthName', @GetMonthName,
    'function(Month : integer; Maj : boolean = True) : string');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScDateTimeUtils', ImportUnit);
end.

