{*
  Importe l'unité RTLConsts dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsRTLConsts;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, RTLConsts;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'RTLConsts', []);

  // Constants
  TSepiConstant.Create(Result, 'SAncestorNotFound', SAncestorNotFound);
  TSepiConstant.Create(Result, 'SAssignError', SAssignError);
  TSepiConstant.Create(Result, 'SBitsIndexError', SBitsIndexError);
  TSepiConstant.Create(Result, 'SBucketListLocked', SBucketListLocked);
  TSepiConstant.Create(Result, 'SCantWriteResourceStreamError',
    SCantWriteResourceStreamError);
  TSepiConstant.Create(Result, 'SCharExpected', SCharExpected);
  TSepiConstant.Create(Result, 'SCheckSynchronizeError',
    SCheckSynchronizeError);
  TSepiConstant.Create(Result, 'SClassNotFound', SClassNotFound);
  TSepiConstant.Create(Result, 'SDelimiterQuoteCharError',
    SDelimiterQuoteCharError);
  TSepiConstant.Create(Result, 'SDuplicateClass', SDuplicateClass);
  TSepiConstant.Create(Result, 'SDuplicateItem', SDuplicateItem);
  TSepiConstant.Create(Result, 'SDuplicateName', SDuplicateName);
  TSepiConstant.Create(Result, 'SDuplicateString', SDuplicateString);
  TSepiConstant.Create(Result, 'SFCreateError', SFCreateError);
  TSepiConstant.Create(Result, 'SFCreateErrorEx', SFCreateErrorEx);
  TSepiConstant.Create(Result, 'SFixedColTooBig', SFixedColTooBig);
  TSepiConstant.Create(Result, 'SFixedRowTooBig', SFixedRowTooBig);
  TSepiConstant.Create(Result, 'SFOpenError', SFOpenError);
  TSepiConstant.Create(Result, 'SFOpenErrorEx', SFOpenErrorEx);
  TSepiConstant.Create(Result, 'SGridTooLarge', SGridTooLarge);
  TSepiConstant.Create(Result, 'SIdentifierExpected', SIdentifierExpected);
  TSepiConstant.Create(Result, 'SIndexOutOfRange', SIndexOutOfRange);
  TSepiConstant.Create(Result, 'SIniFileWriteError', SIniFileWriteError);
  TSepiConstant.Create(Result, 'SInvalidActionCreation',
    SInvalidActionCreation);
  TSepiConstant.Create(Result, 'SInvalidActionEnumeration',
    SInvalidActionEnumeration);
  TSepiConstant.Create(Result, 'SInvalidActionRegistration',
    SInvalidActionRegistration);
  TSepiConstant.Create(Result, 'SInvalidActionUnregistration',
    SInvalidActionUnregistration);
  TSepiConstant.Create(Result, 'SInvalidBinary', SInvalidBinary);
  TSepiConstant.Create(Result, 'SInvalidFileName', SInvalidFileName);
  TSepiConstant.Create(Result, 'SInvalidImage', SInvalidImage);
  TSepiConstant.Create(Result, 'SInvalidMask', SInvalidMask);
  TSepiConstant.Create(Result, 'SInvalidName', SInvalidName);
  TSepiConstant.Create(Result, 'SInvalidProperty', SInvalidProperty);
  TSepiConstant.Create(Result, 'SInvalidPropertyElement',
    SInvalidPropertyElement);
  TSepiConstant.Create(Result, 'SInvalidPropertyPath', SInvalidPropertyPath);
  TSepiConstant.Create(Result, 'SInvalidPropertyType', SInvalidPropertyType);
  TSepiConstant.Create(Result, 'SInvalidPropertyValue', SInvalidPropertyValue);
  TSepiConstant.Create(Result, 'SInvalidRegType', SInvalidRegType);
  TSepiConstant.Create(Result, 'SInvalidString', SInvalidString);
  TSepiConstant.Create(Result, 'SInvalidStringGridOp', SInvalidStringGridOp);
  TSepiConstant.Create(Result, 'SItemNotFound', SItemNotFound);
  TSepiConstant.Create(Result, 'SLineTooLong', SLineTooLong);
  TSepiConstant.Create(Result, 'SListCapacityError', SListCapacityError);
  TSepiConstant.Create(Result, 'SListCountError', SListCountError);
  TSepiConstant.Create(Result, 'SListIndexError', SListIndexError);
  TSepiConstant.Create(Result, 'SMaskErr', SMaskErr);
  TSepiConstant.Create(Result, 'SMaskEditErr', SMaskEditErr);
  TSepiConstant.Create(Result, 'SMemoryStreamError', SMemoryStreamError);
  TSepiConstant.Create(Result, 'SNoComSupport', SNoComSupport);
  TSepiConstant.Create(Result, 'SNotPrinting', SNotPrinting);
  TSepiConstant.Create(Result, 'SNumberExpected', SNumberExpected);
  TSepiConstant.Create(Result, 'SParseError', SParseError);
  TSepiConstant.Create(Result, 'SComponentNameTooLong', SComponentNameTooLong);
  TSepiConstant.Create(Result, 'SPropertyException', SPropertyException);
  TSepiConstant.Create(Result, 'SPrinting', SPrinting);
  TSepiConstant.Create(Result, 'SReadError', SReadError);
  TSepiConstant.Create(Result, 'SReadOnlyProperty', SReadOnlyProperty);
  TSepiConstant.Create(Result, 'SRegCreateFailed', SRegCreateFailed);
  TSepiConstant.Create(Result, 'SRegGetDataFailed', SRegGetDataFailed);
  TSepiConstant.Create(Result, 'SRegisterError', SRegisterError);
  TSepiConstant.Create(Result, 'SRegSetDataFailed', SRegSetDataFailed);
  TSepiConstant.Create(Result, 'SResNotFound', SResNotFound);
  TSepiConstant.Create(Result, 'SSeekNotImplemented', SSeekNotImplemented);
  TSepiConstant.Create(Result, 'SSortedListError', SSortedListError);
  TSepiConstant.Create(Result, 'SStringExpected', SStringExpected);
  TSepiConstant.Create(Result, 'SSymbolExpected', SSymbolExpected);
  TSepiConstant.Create(Result, 'STooManyDeleted', STooManyDeleted);
  TSepiConstant.Create(Result, 'SUnknownGroup', SUnknownGroup);
  TSepiConstant.Create(Result, 'SUnknownProperty', SUnknownProperty);
  TSepiConstant.Create(Result, 'SWriteError', SWriteError);
  TSepiConstant.Create(Result, 'SStreamSetSize', SStreamSetSize);
  TSepiConstant.Create(Result, 'SThreadCreateError', SThreadCreateError);
  TSepiConstant.Create(Result, 'SThreadError', SThreadError);
  TSepiConstant.Create(Result, 'SInvalidDateDay', SInvalidDateDay);
  TSepiConstant.Create(Result, 'SInvalidDateWeek', SInvalidDateWeek);
  TSepiConstant.Create(Result, 'SInvalidDateMonthWeek', SInvalidDateMonthWeek);
  TSepiConstant.Create(Result, 'SInvalidDayOfWeekInMonth',
    SInvalidDayOfWeekInMonth);
  TSepiConstant.Create(Result, 'SInvalidJulianDate', SInvalidJulianDate);
  TSepiConstant.Create(Result, 'SMissingDateTimeField', SMissingDateTimeField);
  TSepiConstant.Create(Result, 'SConvIncompatibleTypes2',
    SConvIncompatibleTypes2);
  TSepiConstant.Create(Result, 'SConvIncompatibleTypes3',
    SConvIncompatibleTypes3);
  TSepiConstant.Create(Result, 'SConvIncompatibleTypes4',
    SConvIncompatibleTypes4);
  TSepiConstant.Create(Result, 'SConvUnknownType', SConvUnknownType);
  TSepiConstant.Create(Result, 'SConvDuplicateType', SConvDuplicateType);
  TSepiConstant.Create(Result, 'SConvUnknownFamily', SConvUnknownFamily);
  TSepiConstant.Create(Result, 'SConvDuplicateFamily', SConvDuplicateFamily);
  TSepiConstant.Create(Result, 'SConvUnknownDescriptionWithPrefix',
    SConvUnknownDescriptionWithPrefix);
  TSepiConstant.Create(Result, 'SConvIllegalType', SConvIllegalType);
  TSepiConstant.Create(Result, 'SConvIllegalFamily', SConvIllegalFamily);
  TSepiConstant.Create(Result, 'SConvFactorZero', SConvFactorZero);
  TSepiConstant.Create(Result, 'SConvStrParseError', SConvStrParseError);
  TSepiConstant.Create(Result, 'SFailedToCallConstructor',
    SFailedToCallConstructor);
  TSepiConstant.Create(Result, 'sWindowsSocketError', sWindowsSocketError);
  TSepiConstant.Create(Result, 'sAsyncSocketError', sAsyncSocketError);
  TSepiConstant.Create(Result, 'sNoAddress', sNoAddress);
  TSepiConstant.Create(Result, 'sCannotListenOnOpen', sCannotListenOnOpen);
  TSepiConstant.Create(Result, 'sCannotCreateSocket', sCannotCreateSocket);
  TSepiConstant.Create(Result, 'sSocketAlreadyOpen', sSocketAlreadyOpen);
  TSepiConstant.Create(Result, 'sCantChangeWhileActive',
    sCantChangeWhileActive);
  TSepiConstant.Create(Result, 'sSocketMustBeBlocking', sSocketMustBeBlocking);
  TSepiConstant.Create(Result, 'sSocketIOError', sSocketIOError);
  TSepiConstant.Create(Result, 'sSocketRead', sSocketRead);
  TSepiConstant.Create(Result, 'sSocketWrite', sSocketWrite);
  TSepiConstant.Create(Result, 'SCmplxCouldNotParseImaginary',
    SCmplxCouldNotParseImaginary);
  TSepiConstant.Create(Result, 'SCmplxCouldNotParseSymbol',
    SCmplxCouldNotParseSymbol);
  TSepiConstant.Create(Result, 'SCmplxCouldNotParsePlus',
    SCmplxCouldNotParsePlus);
  TSepiConstant.Create(Result, 'SCmplxCouldNotParseReal',
    SCmplxCouldNotParseReal);
  TSepiConstant.Create(Result, 'SCmplxUnexpectedEOS', SCmplxUnexpectedEOS);
  TSepiConstant.Create(Result, 'SCmplxUnexpectedChars', SCmplxUnexpectedChars);
  TSepiConstant.Create(Result, 'SCmplxErrorSuffix', SCmplxErrorSuffix);
  TSepiConstant.Create(Result, 'hNoSystem', hNoSystem);
  TSepiConstant.Create(Result, 'hNoTopics', hNoTopics);
  TSepiConstant.Create(Result, 'hNoContext', hNoContext);
  TSepiConstant.Create(Result, 'hNothingFound', hNothingFound);
  TSepiConstant.Create(Result, 'hNoTableOfContents', hNoTableOfContents);
  TSepiConstant.Create(Result, 'SDistanceDescription', SDistanceDescription);
  TSepiConstant.Create(Result, 'SMicromicronsDescription',
    SMicromicronsDescription);
  TSepiConstant.Create(Result, 'SAngstromsDescription', SAngstromsDescription);
  TSepiConstant.Create(Result, 'SMillimicronsDescription',
    SMillimicronsDescription);
  TSepiConstant.Create(Result, 'SMicronsDescription', SMicronsDescription);
  TSepiConstant.Create(Result, 'SMillimetersDescription',
    SMillimetersDescription);
  TSepiConstant.Create(Result, 'SCentimetersDescription',
    SCentimetersDescription);
  TSepiConstant.Create(Result, 'SDecimetersDescription',
    SDecimetersDescription);
  TSepiConstant.Create(Result, 'SMetersDescription', SMetersDescription);
  TSepiConstant.Create(Result, 'SDecametersDescription',
    SDecametersDescription);
  TSepiConstant.Create(Result, 'SHectometersDescription',
    SHectometersDescription);
  TSepiConstant.Create(Result, 'SKilometersDescription',
    SKilometersDescription);
  TSepiConstant.Create(Result, 'SMegametersDescription',
    SMegametersDescription);
  TSepiConstant.Create(Result, 'SGigametersDescription',
    SGigametersDescription);
  TSepiConstant.Create(Result, 'SInchesDescription', SInchesDescription);
  TSepiConstant.Create(Result, 'SFeetDescription', SFeetDescription);
  TSepiConstant.Create(Result, 'SYardsDescription', SYardsDescription);
  TSepiConstant.Create(Result, 'SMilesDescription', SMilesDescription);
  TSepiConstant.Create(Result, 'SNauticalMilesDescription',
    SNauticalMilesDescription);
  TSepiConstant.Create(Result, 'SAstronomicalUnitsDescription',
    SAstronomicalUnitsDescription);
  TSepiConstant.Create(Result, 'SLightYearsDescription',
    SLightYearsDescription);
  TSepiConstant.Create(Result, 'SParsecsDescription', SParsecsDescription);
  TSepiConstant.Create(Result, 'SCubitsDescription', SCubitsDescription);
  TSepiConstant.Create(Result, 'SFathomsDescription', SFathomsDescription);
  TSepiConstant.Create(Result, 'SFurlongsDescription', SFurlongsDescription);
  TSepiConstant.Create(Result, 'SHandsDescription', SHandsDescription);
  TSepiConstant.Create(Result, 'SPacesDescription', SPacesDescription);
  TSepiConstant.Create(Result, 'SRodsDescription', SRodsDescription);
  TSepiConstant.Create(Result, 'SChainsDescription', SChainsDescription);
  TSepiConstant.Create(Result, 'SLinksDescription', SLinksDescription);
  TSepiConstant.Create(Result, 'SPicasDescription', SPicasDescription);
  TSepiConstant.Create(Result, 'SPointsDescription', SPointsDescription);
  TSepiConstant.Create(Result, 'SAreaDescription', SAreaDescription);
  TSepiConstant.Create(Result, 'SSquareMillimetersDescription',
    SSquareMillimetersDescription);
  TSepiConstant.Create(Result, 'SSquareCentimetersDescription',
    SSquareCentimetersDescription);
  TSepiConstant.Create(Result, 'SSquareDecimetersDescription',
    SSquareDecimetersDescription);
  TSepiConstant.Create(Result, 'SSquareMetersDescription',
    SSquareMetersDescription);
  TSepiConstant.Create(Result, 'SSquareDecametersDescription',
    SSquareDecametersDescription);
  TSepiConstant.Create(Result, 'SSquareHectometersDescription',
    SSquareHectometersDescription);
  TSepiConstant.Create(Result, 'SSquareKilometersDescription',
    SSquareKilometersDescription);
  TSepiConstant.Create(Result, 'SSquareInchesDescription',
    SSquareInchesDescription);
  TSepiConstant.Create(Result, 'SSquareFeetDescription',
    SSquareFeetDescription);
  TSepiConstant.Create(Result, 'SSquareYardsDescription',
    SSquareYardsDescription);
  TSepiConstant.Create(Result, 'SSquareMilesDescription',
    SSquareMilesDescription);
  TSepiConstant.Create(Result, 'SAcresDescription', SAcresDescription);
  TSepiConstant.Create(Result, 'SCentaresDescription', SCentaresDescription);
  TSepiConstant.Create(Result, 'SAresDescription', SAresDescription);
  TSepiConstant.Create(Result, 'SHectaresDescription', SHectaresDescription);
  TSepiConstant.Create(Result, 'SSquareRodsDescription',
    SSquareRodsDescription);
  TSepiConstant.Create(Result, 'SVolumeDescription', SVolumeDescription);
  TSepiConstant.Create(Result, 'SCubicMillimetersDescription',
    SCubicMillimetersDescription);
  TSepiConstant.Create(Result, 'SCubicCentimetersDescription',
    SCubicCentimetersDescription);
  TSepiConstant.Create(Result, 'SCubicDecimetersDescription',
    SCubicDecimetersDescription);
  TSepiConstant.Create(Result, 'SCubicMetersDescription',
    SCubicMetersDescription);
  TSepiConstant.Create(Result, 'SCubicDecametersDescription',
    SCubicDecametersDescription);
  TSepiConstant.Create(Result, 'SCubicHectometersDescription',
    SCubicHectometersDescription);
  TSepiConstant.Create(Result, 'SCubicKilometersDescription',
    SCubicKilometersDescription);
  TSepiConstant.Create(Result, 'SCubicInchesDescription',
    SCubicInchesDescription);
  TSepiConstant.Create(Result, 'SCubicFeetDescription', SCubicFeetDescription);
  TSepiConstant.Create(Result, 'SCubicYardsDescription',
    SCubicYardsDescription);
  TSepiConstant.Create(Result, 'SCubicMilesDescription',
    SCubicMilesDescription);
  TSepiConstant.Create(Result, 'SMilliLitersDescription',
    SMilliLitersDescription);
  TSepiConstant.Create(Result, 'SCentiLitersDescription',
    SCentiLitersDescription);
  TSepiConstant.Create(Result, 'SDeciLitersDescription',
    SDeciLitersDescription);
  TSepiConstant.Create(Result, 'SLitersDescription', SLitersDescription);
  TSepiConstant.Create(Result, 'SDecaLitersDescription',
    SDecaLitersDescription);
  TSepiConstant.Create(Result, 'SHectoLitersDescription',
    SHectoLitersDescription);
  TSepiConstant.Create(Result, 'SKiloLitersDescription',
    SKiloLitersDescription);
  TSepiConstant.Create(Result, 'SAcreFeetDescription', SAcreFeetDescription);
  TSepiConstant.Create(Result, 'SAcreInchesDescription',
    SAcreInchesDescription);
  TSepiConstant.Create(Result, 'SCordsDescription', SCordsDescription);
  TSepiConstant.Create(Result, 'SCordFeetDescription', SCordFeetDescription);
  TSepiConstant.Create(Result, 'SDecisteresDescription',
    SDecisteresDescription);
  TSepiConstant.Create(Result, 'SSteresDescription', SSteresDescription);
  TSepiConstant.Create(Result, 'SDecasteresDescription',
    SDecasteresDescription);
  TSepiConstant.Create(Result, 'SFluidGallonsDescription',
    SFluidGallonsDescription);
  TSepiConstant.Create(Result, 'SFluidQuartsDescription',
    SFluidQuartsDescription);
  TSepiConstant.Create(Result, 'SFluidPintsDescription',
    SFluidPintsDescription);
  TSepiConstant.Create(Result, 'SFluidCupsDescription', SFluidCupsDescription);
  TSepiConstant.Create(Result, 'SFluidGillsDescription',
    SFluidGillsDescription);
  TSepiConstant.Create(Result, 'SFluidOuncesDescription',
    SFluidOuncesDescription);
  TSepiConstant.Create(Result, 'SFluidTablespoonsDescription',
    SFluidTablespoonsDescription);
  TSepiConstant.Create(Result, 'SFluidTeaspoonsDescription',
    SFluidTeaspoonsDescription);
  TSepiConstant.Create(Result, 'SDryGallonsDescription',
    SDryGallonsDescription);
  TSepiConstant.Create(Result, 'SDryQuartsDescription', SDryQuartsDescription);
  TSepiConstant.Create(Result, 'SDryPintsDescription', SDryPintsDescription);
  TSepiConstant.Create(Result, 'SDryPecksDescription', SDryPecksDescription);
  TSepiConstant.Create(Result, 'SDryBucketsDescription',
    SDryBucketsDescription);
  TSepiConstant.Create(Result, 'SDryBushelsDescription',
    SDryBushelsDescription);
  TSepiConstant.Create(Result, 'SUKGallonsDescription', SUKGallonsDescription);
  TSepiConstant.Create(Result, 'SUKPottlesDescription', SUKPottlesDescription);
  TSepiConstant.Create(Result, 'SUKQuartsDescription', SUKQuartsDescription);
  TSepiConstant.Create(Result, 'SUKPintsDescription', SUKPintsDescription);
  TSepiConstant.Create(Result, 'SUKGillsDescription', SUKGillsDescription);
  TSepiConstant.Create(Result, 'SUKOuncesDescription', SUKOuncesDescription);
  TSepiConstant.Create(Result, 'SUKPecksDescription', SUKPecksDescription);
  TSepiConstant.Create(Result, 'SUKBucketsDescription', SUKBucketsDescription);
  TSepiConstant.Create(Result, 'SUKBushelsDescription', SUKBushelsDescription);
  TSepiConstant.Create(Result, 'SMassDescription', SMassDescription);
  TSepiConstant.Create(Result, 'SNanogramsDescription', SNanogramsDescription);
  TSepiConstant.Create(Result, 'SMicrogramsDescription',
    SMicrogramsDescription);
  TSepiConstant.Create(Result, 'SMilligramsDescription',
    SMilligramsDescription);
  TSepiConstant.Create(Result, 'SCentigramsDescription',
    SCentigramsDescription);
  TSepiConstant.Create(Result, 'SDecigramsDescription', SDecigramsDescription);
  TSepiConstant.Create(Result, 'SGramsDescription', SGramsDescription);
  TSepiConstant.Create(Result, 'SDecagramsDescription', SDecagramsDescription);
  TSepiConstant.Create(Result, 'SHectogramsDescription',
    SHectogramsDescription);
  TSepiConstant.Create(Result, 'SKilogramsDescription', SKilogramsDescription);
  TSepiConstant.Create(Result, 'SMetricTonsDescription',
    SMetricTonsDescription);
  TSepiConstant.Create(Result, 'SDramsDescription', SDramsDescription);
  TSepiConstant.Create(Result, 'SGrainsDescription', SGrainsDescription);
  TSepiConstant.Create(Result, 'STonsDescription', STonsDescription);
  TSepiConstant.Create(Result, 'SLongTonsDescription', SLongTonsDescription);
  TSepiConstant.Create(Result, 'SOuncesDescription', SOuncesDescription);
  TSepiConstant.Create(Result, 'SPoundsDescription', SPoundsDescription);
  TSepiConstant.Create(Result, 'SStonesDescription', SStonesDescription);
  TSepiConstant.Create(Result, 'STemperatureDescription',
    STemperatureDescription);
  TSepiConstant.Create(Result, 'SCelsiusDescription', SCelsiusDescription);
  TSepiConstant.Create(Result, 'SKelvinDescription', SKelvinDescription);
  TSepiConstant.Create(Result, 'SFahrenheitDescription',
    SFahrenheitDescription);
  TSepiConstant.Create(Result, 'SRankineDescription', SRankineDescription);
  TSepiConstant.Create(Result, 'SReaumurDescription', SReaumurDescription);
  TSepiConstant.Create(Result, 'STimeDescription', STimeDescription);
  TSepiConstant.Create(Result, 'SMilliSecondsDescription',
    SMilliSecondsDescription);
  TSepiConstant.Create(Result, 'SSecondsDescription', SSecondsDescription);
  TSepiConstant.Create(Result, 'SMinutesDescription', SMinutesDescription);
  TSepiConstant.Create(Result, 'SHoursDescription', SHoursDescription);
  TSepiConstant.Create(Result, 'SDaysDescription', SDaysDescription);
  TSepiConstant.Create(Result, 'SWeeksDescription', SWeeksDescription);
  TSepiConstant.Create(Result, 'SFortnightsDescription',
    SFortnightsDescription);
  TSepiConstant.Create(Result, 'SMonthsDescription', SMonthsDescription);
  TSepiConstant.Create(Result, 'SYearsDescription', SYearsDescription);
  TSepiConstant.Create(Result, 'SDecadesDescription', SDecadesDescription);
  TSepiConstant.Create(Result, 'SCenturiesDescription', SCenturiesDescription);
  TSepiConstant.Create(Result, 'SMillenniaDescription', SMillenniaDescription);
  TSepiConstant.Create(Result, 'SDateTimeDescription', SDateTimeDescription);
  TSepiConstant.Create(Result, 'SJulianDateDescription',
    SJulianDateDescription);
  TSepiConstant.Create(Result, 'SModifiedJulianDateDescription',
    SModifiedJulianDateDescription);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('RTLConsts', ImportUnit);
end.

