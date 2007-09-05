{*
  Importe l'unité ComConst dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsComConst;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ComConst;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ComConst', []);

  // Constants
  TSepiConstant.Create(Result, 'SCreateRegKeyError', SCreateRegKeyError);
  TSepiConstant.Create(Result, 'SOleError', SOleError);
  TSepiConstant.Create(Result, 'SObjectFactoryMissing', SObjectFactoryMissing);
  TSepiConstant.Create(Result, 'STypeInfoMissing', STypeInfoMissing);
  TSepiConstant.Create(Result, 'SBadTypeInfo', SBadTypeInfo);
  TSepiConstant.Create(Result, 'SDispIntfMissing', SDispIntfMissing);
  TSepiConstant.Create(Result, 'SNoMethod', SNoMethod);
  TSepiConstant.Create(Result, 'SVarNotObject', SVarNotObject);
  TSepiConstant.Create(Result, 'STooManyParams', STooManyParams);
  TSepiConstant.Create(Result, 'SDCOMNotInstalled', SDCOMNotInstalled);
  TSepiConstant.Create(Result, 'SDAXError', SDAXError);
  TSepiConstant.Create(Result, 'SAutomationWarning', SAutomationWarning);
  TSepiConstant.Create(Result, 'SNoCloseActiveServer1', SNoCloseActiveServer1);
  TSepiConstant.Create(Result, 'SNoCloseActiveServer2', SNoCloseActiveServer2);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ComConst', ImportUnit);
end.

