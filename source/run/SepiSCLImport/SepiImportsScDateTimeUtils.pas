{*
  Importe l'unité ScDateTimeUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScDateTimeUtils;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ScDateTimeUtils;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScDateTimeUtils', []);

  // Routines
  TSepiMetaMethod.Create(Result, 'GetMonthName', @GetMonthName,
    'function(Month : integer; Maj : boolean = True) : string');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScDateTimeUtils', ImportUnit);
end.

