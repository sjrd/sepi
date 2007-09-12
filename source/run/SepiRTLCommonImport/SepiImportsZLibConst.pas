{*
  Importe l'unité ZLibConst dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsZLibConst;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ZLibConst;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ZLibConst', []);

  // Constants
  TSepiConstant.Create(Result, 'sTargetBufferTooSmall', sTargetBufferTooSmall);
  TSepiConstant.Create(Result, 'sInvalidStreamOp', sInvalidStreamOp);
  TSepiConstant.Create(Result, 'sError', sError);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ZLibConst', ImportUnit);
end.

