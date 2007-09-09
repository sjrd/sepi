{*
  Importe l'unité SysConst dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsSysConst;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, SysConst;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'SysConst', []);

  // Constants
  TSepiConstant.Create(Result, 'SUnknown', SUnknown);
  TSepiConstant.Create(Result, 'SInvalidInteger', SInvalidInteger);
  TSepiConstant.Create(Result, 'SInvalidFloat', SInvalidFloat);
  TSepiConstant.Create(Result, 'SInvalidCurrency', SInvalidCurrency);
  TSepiConstant.Create(Result, 'SInvalidDate', SInvalidDate);
  TSepiConstant.Create(Result, 'SInvalidTime', SInvalidTime);
  TSepiConstant.Create(Result, 'SInvalidDateTime', SInvalidDateTime);
  TSepiConstant.Create(Result, 'SInvalidDateTimeFloat', SInvalidDateTimeFloat);
  TSepiConstant.Create(Result, 'SInvalidTimeStamp', SInvalidTimeStamp);
  TSepiConstant.Create(Result, 'SInvalidGUID', SInvalidGUID);
  TSepiConstant.Create(Result, 'SInvalidBoolean', SInvalidBoolean);
  TSepiConstant.Create(Result, 'STimeEncodeError', STimeEncodeError);
  TSepiConstant.Create(Result, 'SDateEncodeError', SDateEncodeError);
  TSepiConstant.Create(Result, 'SOutOfMemory', SOutOfMemory);
  TSepiConstant.Create(Result, 'SInOutError', SInOutError);
  TSepiConstant.Create(Result, 'SFileNotFound', SFileNotFound);
  TSepiConstant.Create(Result, 'SInvalidFilename', SInvalidFilename);
  TSepiConstant.Create(Result, 'STooManyOpenFiles', STooManyOpenFiles);
  TSepiConstant.Create(Result, 'SAccessDenied', SAccessDenied);
  TSepiConstant.Create(Result, 'SEndOfFile', SEndOfFile);
  TSepiConstant.Create(Result, 'SDiskFull', SDiskFull);
  TSepiConstant.Create(Result, 'SInvalidInput', SInvalidInput);
  TSepiConstant.Create(Result, 'SDivByZero', SDivByZero);
  TSepiConstant.Create(Result, 'SRangeError', SRangeError);
  TSepiConstant.Create(Result, 'SIntOverflow', SIntOverflow);
  TSepiConstant.Create(Result, 'SInvalidOp', SInvalidOp);
  TSepiConstant.Create(Result, 'SZeroDivide', SZeroDivide);
  TSepiConstant.Create(Result, 'SOverflow', SOverflow);
  TSepiConstant.Create(Result, 'SUnderflow', SUnderflow);
  TSepiConstant.Create(Result, 'SInvalidPointer', SInvalidPointer);
  TSepiConstant.Create(Result, 'SInvalidCast', SInvalidCast);
  TSepiConstant.Create(Result, 'SAccessViolationArg3', SAccessViolationArg3);
  TSepiConstant.Create(Result, 'SAccessViolationNoArg', SAccessViolationNoArg);
  TSepiConstant.Create(Result, 'SStackOverflow', SStackOverflow);
  TSepiConstant.Create(Result, 'SControlC', SControlC);
  TSepiConstant.Create(Result, 'SQuit', SQuit);
  TSepiConstant.Create(Result, 'SPrivilege', SPrivilege);
  TSepiConstant.Create(Result, 'SOperationAborted', SOperationAborted);
  TSepiConstant.Create(Result, 'SException', SException);
  TSepiConstant.Create(Result, 'SExceptTitle', SExceptTitle);
  TSepiConstant.Create(Result, 'SInvalidFormat', SInvalidFormat);
  TSepiConstant.Create(Result, 'SArgumentMissing', SArgumentMissing);
  TSepiConstant.Create(Result, 'SDispatchError', SDispatchError);
  TSepiConstant.Create(Result, 'SReadAccess', SReadAccess);
  TSepiConstant.Create(Result, 'SWriteAccess', SWriteAccess);
  TSepiConstant.Create(Result, 'SResultTooLong', SResultTooLong);
  TSepiConstant.Create(Result, 'SFormatTooLong', SFormatTooLong);
  TSepiConstant.Create(Result, 'SVarArrayCreate', SVarArrayCreate);
  TSepiConstant.Create(Result, 'SVarArrayBounds', SVarArrayBounds);
  TSepiConstant.Create(Result, 'SVarArrayLocked', SVarArrayLocked);
  TSepiConstant.Create(Result, 'SVarArrayWithHResult', SVarArrayWithHResult);
  TSepiConstant.Create(Result, 'SInvalidVarCast', SInvalidVarCast);
  TSepiConstant.Create(Result, 'SInvalidVarOp', SInvalidVarOp);
  TSepiConstant.Create(Result, 'SInvalidVarNullOp', SInvalidVarNullOp);
  TSepiConstant.Create(Result, 'SInvalidVarOpWithHResultWithPrefix', SInvalidVarOpWithHResultWithPrefix);
  TSepiConstant.Create(Result, 'SVarTypeRangeCheck1', SVarTypeRangeCheck1);
  TSepiConstant.Create(Result, 'SVarTypeRangeCheck2', SVarTypeRangeCheck2);
  TSepiConstant.Create(Result, 'SVarTypeOutOfRangeWithPrefix', SVarTypeOutOfRangeWithPrefix);
  TSepiConstant.Create(Result, 'SVarTypeAlreadyUsedWithPrefix', SVarTypeAlreadyUsedWithPrefix);
  TSepiConstant.Create(Result, 'SVarTypeNotUsableWithPrefix', SVarTypeNotUsableWithPrefix);
  TSepiConstant.Create(Result, 'SVarTypeTooManyCustom', SVarTypeTooManyCustom);
  TSepiConstant.Create(Result, 'SVarTypeCouldNotConvert', SVarTypeCouldNotConvert);
  TSepiConstant.Create(Result, 'SVarTypeConvertOverflow', SVarTypeConvertOverflow);
  TSepiConstant.Create(Result, 'SVarOverflow', SVarOverflow);
  TSepiConstant.Create(Result, 'SVarInvalid', SVarInvalid);
  TSepiConstant.Create(Result, 'SVarBadType', SVarBadType);
  TSepiConstant.Create(Result, 'SVarNotImplemented', SVarNotImplemented);
  TSepiConstant.Create(Result, 'SVarOutOfMemory', SVarOutOfMemory);
  TSepiConstant.Create(Result, 'SVarUnexpected', SVarUnexpected);
  TSepiConstant.Create(Result, 'SVarDataClearRecursing', SVarDataClearRecursing);
  TSepiConstant.Create(Result, 'SVarDataCopyRecursing', SVarDataCopyRecursing);
  TSepiConstant.Create(Result, 'SVarDataCopyNoIndRecursing', SVarDataCopyNoIndRecursing);
  TSepiConstant.Create(Result, 'SVarDataInitRecursing', SVarDataInitRecursing);
  TSepiConstant.Create(Result, 'SVarDataCastToRecursing', SVarDataCastToRecursing);
  TSepiConstant.Create(Result, 'SVarIsEmpty', SVarIsEmpty);
  TSepiConstant.Create(Result, 'sUnknownFromType', sUnknownFromType);
  TSepiConstant.Create(Result, 'sUnknownToType', sUnknownToType);
  TSepiConstant.Create(Result, 'SExternalException', SExternalException);
  TSepiConstant.Create(Result, 'SAssertionFailed', SAssertionFailed);
  TSepiConstant.Create(Result, 'SIntfCastError', SIntfCastError);
  TSepiConstant.Create(Result, 'SSafecallException', SSafecallException);
  TSepiConstant.Create(Result, 'SAssertError', SAssertError);
  TSepiConstant.Create(Result, 'SAbstractError', SAbstractError);
  TSepiConstant.Create(Result, 'SModuleAccessViolation', SModuleAccessViolation);
  TSepiConstant.Create(Result, 'SCannotReadPackageInfo', SCannotReadPackageInfo);
  TSepiConstant.Create(Result, 'sErrorLoadingPackage', sErrorLoadingPackage);
  TSepiConstant.Create(Result, 'SInvalidPackageFile', SInvalidPackageFile);
  TSepiConstant.Create(Result, 'SInvalidPackageHandle', SInvalidPackageHandle);
  TSepiConstant.Create(Result, 'SDuplicatePackageUnit', SDuplicatePackageUnit);
  TSepiConstant.Create(Result, 'SOSError', SOSError);
  TSepiConstant.Create(Result, 'SUnkOSError', SUnkOSError);
  TSepiConstant.Create(Result, 'SNL', SNL);
  TSepiConstant.Create(Result, 'SShortMonthNameJan', SShortMonthNameJan);
  TSepiConstant.Create(Result, 'SShortMonthNameFeb', SShortMonthNameFeb);
  TSepiConstant.Create(Result, 'SShortMonthNameMar', SShortMonthNameMar);
  TSepiConstant.Create(Result, 'SShortMonthNameApr', SShortMonthNameApr);
  TSepiConstant.Create(Result, 'SShortMonthNameMay', SShortMonthNameMay);
  TSepiConstant.Create(Result, 'SShortMonthNameJun', SShortMonthNameJun);
  TSepiConstant.Create(Result, 'SShortMonthNameJul', SShortMonthNameJul);
  TSepiConstant.Create(Result, 'SShortMonthNameAug', SShortMonthNameAug);
  TSepiConstant.Create(Result, 'SShortMonthNameSep', SShortMonthNameSep);
  TSepiConstant.Create(Result, 'SShortMonthNameOct', SShortMonthNameOct);
  TSepiConstant.Create(Result, 'SShortMonthNameNov', SShortMonthNameNov);
  TSepiConstant.Create(Result, 'SShortMonthNameDec', SShortMonthNameDec);
  TSepiConstant.Create(Result, 'SLongMonthNameJan', SLongMonthNameJan);
  TSepiConstant.Create(Result, 'SLongMonthNameFeb', SLongMonthNameFeb);
  TSepiConstant.Create(Result, 'SLongMonthNameMar', SLongMonthNameMar);
  TSepiConstant.Create(Result, 'SLongMonthNameApr', SLongMonthNameApr);
  TSepiConstant.Create(Result, 'SLongMonthNameMay', SLongMonthNameMay);
  TSepiConstant.Create(Result, 'SLongMonthNameJun', SLongMonthNameJun);
  TSepiConstant.Create(Result, 'SLongMonthNameJul', SLongMonthNameJul);
  TSepiConstant.Create(Result, 'SLongMonthNameAug', SLongMonthNameAug);
  TSepiConstant.Create(Result, 'SLongMonthNameSep', SLongMonthNameSep);
  TSepiConstant.Create(Result, 'SLongMonthNameOct', SLongMonthNameOct);
  TSepiConstant.Create(Result, 'SLongMonthNameNov', SLongMonthNameNov);
  TSepiConstant.Create(Result, 'SLongMonthNameDec', SLongMonthNameDec);
  TSepiConstant.Create(Result, 'SShortDayNameSun', SShortDayNameSun);
  TSepiConstant.Create(Result, 'SShortDayNameMon', SShortDayNameMon);
  TSepiConstant.Create(Result, 'SShortDayNameTue', SShortDayNameTue);
  TSepiConstant.Create(Result, 'SShortDayNameWed', SShortDayNameWed);
  TSepiConstant.Create(Result, 'SShortDayNameThu', SShortDayNameThu);
  TSepiConstant.Create(Result, 'SShortDayNameFri', SShortDayNameFri);
  TSepiConstant.Create(Result, 'SShortDayNameSat', SShortDayNameSat);
  TSepiConstant.Create(Result, 'SLongDayNameSun', SLongDayNameSun);
  TSepiConstant.Create(Result, 'SLongDayNameMon', SLongDayNameMon);
  TSepiConstant.Create(Result, 'SLongDayNameTue', SLongDayNameTue);
  TSepiConstant.Create(Result, 'SLongDayNameWed', SLongDayNameWed);
  TSepiConstant.Create(Result, 'SLongDayNameThu', SLongDayNameThu);
  TSepiConstant.Create(Result, 'SLongDayNameFri', SLongDayNameFri);
  TSepiConstant.Create(Result, 'SLongDayNameSat', SLongDayNameSat);
  TSepiConstant.Create(Result, 'SCannotCreateDir', SCannotCreateDir);
  TSepiConstant.Create(Result, 'SCodesetConversionError', SCodesetConversionError);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('SysConst', ImportUnit);
end.

