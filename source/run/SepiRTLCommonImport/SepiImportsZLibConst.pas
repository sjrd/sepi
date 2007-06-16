{*
  Importe l'unité ZLibConst dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsZLibConst;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ZLibConst;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ZLibConst', []);

  // Constants
  TSepiConstant.Create(Result, 'sTargetBufferTooSmall', sTargetBufferTooSmall);
  TSepiConstant.Create(Result, 'sInvalidStreamOp', sInvalidStreamOp);
  TSepiConstant.Create(Result, 'sError', sError);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ZLibConst', ImportUnit);
end.

