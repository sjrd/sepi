{*
  Importe l'unité ScConsts dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScConsts;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScConsts;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ScConsts', []);

  // Constants
  TSepiConstant.Create(Result, 'sScLanguage', sScLanguage);
  TSepiConstant.Create(Result, 'sScYes', sScYes);
  TSepiConstant.Create(Result, 'sScNo', sScNo);
  TSepiConstant.Create(Result, 'sScColon', sScColon);
  TSepiConstant.Create(Result, 'sScSemiColon', sScSemiColon);
  TSepiConstant.Create(Result, 'sScAbout', sScAbout);
  TSepiConstant.Create(Result, 'sScVersion', sScVersion);
  TSepiConstant.Create(Result, 'sScAuthor', sScAuthor);
  TSepiConstant.Create(Result, 'sScWrongPassword', sScWrongPassword);
  TSepiConstant.Create(Result, 'sJanuary', sJanuary);
  TSepiConstant.Create(Result, 'sFebruary', sFebruary);
  TSepiConstant.Create(Result, 'sMarch', sMarch);
  TSepiConstant.Create(Result, 'sApril', sApril);
  TSepiConstant.Create(Result, 'sMay', sMay);
  TSepiConstant.Create(Result, 'sJune', sJune);
  TSepiConstant.Create(Result, 'sJuly', sJuly);
  TSepiConstant.Create(Result, 'sAugust', sAugust);
  TSepiConstant.Create(Result, 'sSeptember', sSeptember);
  TSepiConstant.Create(Result, 'sOctober', sOctober);
  TSepiConstant.Create(Result, 'sNovember', sNovember);
  TSepiConstant.Create(Result, 'sDecember', sDecember);
  TSepiConstant.Create(Result, 'sScNotInMustPairsOfChars', sScNotInMustPairsOfChars);
  TSepiConstant.Create(Result, 'sScDelimMustDifferentThanNotIn', sScDelimMustDifferentThanNotIn);
  TSepiConstant.Create(Result, 'sScIndexOutOfRange', sScIndexOutOfRange);
  TSepiConstant.Create(Result, 'sScWrongIntSize', sScWrongIntSize);
  TSepiConstant.Create(Result, 'sScWrongLongWord', sScWrongLongWord);
  TSepiConstant.Create(Result, 'sScListIsNotEmpty', sScListIsNotEmpty);
  TSepiConstant.Create(Result, 'sScWrongBase', sScWrongBase);
  TSepiConstant.Create(Result, 'sScWrongInteger', sScWrongInteger);
  TSepiConstant.Create(Result, 'sScWrongFloat', sScWrongFloat);
  TSepiConstant.Create(Result, 'sScWrongString', sScWrongString);
  TSepiConstant.Create(Result, 'sSjrdWrongChar', sSjrdWrongChar);
  TSepiConstant.Create(Result, 'sScWrongCharSet', sScWrongCharSet);
  TSepiConstant.Create(Result, 'sScSetOf', sScSetOf);
  TSepiConstant.Create(Result, 'sScWrongEnumSet', sScWrongEnumSet);
  TSepiConstant.Create(Result, 'sScErrorMaths', sScErrorMaths);
  TSepiConstant.Create(Result, 'sScErrorDegree', sScErrorDegree);
  TSepiConstant.Create(Result, 'sScErrorNotDegreeZero', sScErrorNotDegreeZero);
  TSepiConstant.Create(Result, 'sScErrorInteger', sScErrorInteger);
  TSepiConstant.Create(Result, 'sScErrorNotInteger', sScErrorNotInteger);
  TSepiConstant.Create(Result, 'sScErrorNatural', sScErrorNatural);
  TSepiConstant.Create(Result, 'sScErrorNotNatural', sScErrorNotNatural);
  TSepiConstant.Create(Result, 'sScErrorDivision', sScErrorDivision);
  TSepiConstant.Create(Result, 'sScErrorDivByZero', sScErrorDivByZero);
  TSepiConstant.Create(Result, 'sSjrdErrorAbortDivision', sSjrdErrorAbortDivision);
  TSepiConstant.Create(Result, 'sScErrorRoot', sScErrorRoot);
  TSepiConstant.Create(Result, 'sScErrorEval', sScErrorEval);
  TSepiConstant.Create(Result, 'sScErrorWrongExpression', sScErrorWrongExpression);
  TSepiConstant.Create(Result, 'sScErrorWrongCharacter', sScErrorWrongCharacter);
  TSepiConstant.Create(Result, 'sScErrorOperation', sScErrorOperation);
  TSepiConstant.Create(Result, 'sScErrorOpNotExists', sScErrorOpNotExists);
  TSepiConstant.Create(Result, 'sScErrorOpIsNotBinary', sScErrorOpIsNotBinary);
  TSepiConstant.Create(Result, 'sScErrorOpIsNotUnary', sScErrorOpIsNotUnary);
  TSepiConstant.Create(Result, 'sScErrorOpRequestsDegreeZero', sScErrorOpRequestsDegreeZero);
  TSepiConstant.Create(Result, 'sScErrorOpRequestsInteger', sScErrorOpRequestsInteger);
  TSepiConstant.Create(Result, 'sScErrorOpRequestsNatural', sScErrorOpRequestsNatural);
  TSepiConstant.Create(Result, 'sScErrorOpRequestsCorrectIndex', sScErrorOpRequestsCorrectIndex);
  TSepiConstant.Create(Result, 'sScErrorBrackets', sScErrorBrackets);
  TSepiConstant.Create(Result, 'sScErrorTooManyBrackets', sScErrorTooManyBrackets);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScConsts', ImportUnit);
end.

