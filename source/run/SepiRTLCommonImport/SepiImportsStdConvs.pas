{*
  Importe l'unité StdConvs dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsStdConvs;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, StdConvs, ConvUtils;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'StdConvs',
    ['SysUtils', 'ConvUtils']);

  // Global variables
  TSepiVariable.Create(Result, 'cbDistance',
     cbDistance, TypeInfo(TConvFamily));
  TSepiVariable.Create(Result, 'duMicromicrons',
     duMicromicrons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duAngstroms',
     duAngstroms, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duMillimicrons',
     duMillimicrons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duMicrons',
     duMicrons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duMillimeters',
     duMillimeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duCentimeters',
     duCentimeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duDecimeters',
     duDecimeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duMeters',
     duMeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duDecameters',
     duDecameters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duHectometers',
     duHectometers, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duKilometers',
     duKilometers, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duMegameters',
     duMegameters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duGigameters',
     duGigameters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duInches',
     duInches, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duFeet',
     duFeet, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duYards',
     duYards, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duMiles',
     duMiles, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duNauticalMiles',
     duNauticalMiles, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duAstronomicalUnits',
     duAstronomicalUnits, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duLightYears',
     duLightYears, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duParsecs',
     duParsecs, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duCubits',
     duCubits, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duFathoms',
     duFathoms, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duFurlongs',
     duFurlongs, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duHands',
     duHands, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duPaces',
     duPaces, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duRods',
     duRods, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duChains',
     duChains, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duLinks',
     duLinks, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duPicas',
     duPicas, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'duPoints',
     duPoints, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'cbArea',
     cbArea, TypeInfo(TConvFamily));
  TSepiVariable.Create(Result, 'auSquareMillimeters',
     auSquareMillimeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareCentimeters',
     auSquareCentimeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareDecimeters',
     auSquareDecimeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareMeters',
     auSquareMeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareDecameters',
     auSquareDecameters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareHectometers',
     auSquareHectometers, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareKilometers',
     auSquareKilometers, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareInches',
     auSquareInches, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareFeet',
     auSquareFeet, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareYards',
     auSquareYards, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareMiles',
     auSquareMiles, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auAcres',
     auAcres, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auCentares',
     auCentares, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auAres',
     auAres, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auHectares',
     auHectares, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'auSquareRods',
     auSquareRods, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'cbVolume',
     cbVolume, TypeInfo(TConvFamily));
  TSepiVariable.Create(Result, 'vuCubicMillimeters',
     vuCubicMillimeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicCentimeters',
     vuCubicCentimeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicDecimeters',
     vuCubicDecimeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicMeters',
     vuCubicMeters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicDecameters',
     vuCubicDecameters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicHectometers',
     vuCubicHectometers, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicKilometers',
     vuCubicKilometers, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicInches',
     vuCubicInches, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicFeet',
     vuCubicFeet, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicYards',
     vuCubicYards, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCubicMiles',
     vuCubicMiles, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuMilliLiters',
     vuMilliLiters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCentiLiters',
     vuCentiLiters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDeciLiters',
     vuDeciLiters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuLiters',
     vuLiters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDecaLiters',
     vuDecaLiters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuHectoLiters',
     vuHectoLiters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuKiloLiters',
     vuKiloLiters, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuAcreFeet',
     vuAcreFeet, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuAcreInches',
     vuAcreInches, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCords',
     vuCords, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuCordFeet',
     vuCordFeet, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDecisteres',
     vuDecisteres, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuSteres',
     vuSteres, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDecasteres',
     vuDecasteres, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuFluidGallons',
     vuFluidGallons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuFluidQuarts',
     vuFluidQuarts, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuFluidPints',
     vuFluidPints, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuFluidCups',
     vuFluidCups, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuFluidGills',
     vuFluidGills, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuFluidOunces',
     vuFluidOunces, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuFluidTablespoons',
     vuFluidTablespoons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuFluidTeaspoons',
     vuFluidTeaspoons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDryGallons',
     vuDryGallons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDryQuarts',
     vuDryQuarts, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDryPints',
     vuDryPints, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDryPecks',
     vuDryPecks, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDryBuckets',
     vuDryBuckets, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuDryBushels',
     vuDryBushels, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuUKGallons',
     vuUKGallons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuUKPottles',
     vuUKPottles, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuUKQuarts',
     vuUKQuarts, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuUKPints',
     vuUKPints, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuUKGills',
     vuUKGills, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuUKOunces',
     vuUKOunces, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuUKPecks',
     vuUKPecks, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuUKBuckets',
     vuUKBuckets, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'vuUKBushels',
     vuUKBushels, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'cbMass',
     cbMass, TypeInfo(TConvFamily));
  TSepiVariable.Create(Result, 'muNanograms',
     muNanograms, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muMicrograms',
     muMicrograms, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muMilligrams',
     muMilligrams, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muCentigrams',
     muCentigrams, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muDecigrams',
     muDecigrams, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muGrams',
     muGrams, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muDecagrams',
     muDecagrams, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muHectograms',
     muHectograms, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muKilograms',
     muKilograms, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muMetricTons',
     muMetricTons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muDrams',
     muDrams, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muGrains',
     muGrains, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muLongTons',
     muLongTons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muTons',
     muTons, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muOunces',
     muOunces, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muPounds',
     muPounds, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'muStones',
     muStones, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'cbTemperature',
     cbTemperature, TypeInfo(TConvFamily));
  TSepiVariable.Create(Result, 'tuCelsius',
     tuCelsius, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuKelvin',
     tuKelvin, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuFahrenheit',
     tuFahrenheit, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuRankine',
     tuRankine, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuReaumur',
     tuReaumur, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'cbTime',
     cbTime, TypeInfo(TConvFamily));
  TSepiVariable.Create(Result, 'tuMilliSeconds',
     tuMilliSeconds, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuSeconds',
     tuSeconds, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuMinutes',
     tuMinutes, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuHours',
     tuHours, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuDays',
     tuDays, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuWeeks',
     tuWeeks, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuFortnights',
     tuFortnights, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuMonths',
     tuMonths, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuYears',
     tuYears, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuDecades',
     tuDecades, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuCenturies',
     tuCenturies, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuMillennia',
     tuMillennia, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuDateTime',
     tuDateTime, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuJulianDate',
     tuJulianDate, TypeInfo(TConvType));
  TSepiVariable.Create(Result, 'tuModifiedJulianDate',
     tuModifiedJulianDate, TypeInfo(TConvType));

  // Constants
  TSepiConstant.Create(Result, 'MetersPerInch', MetersPerInch);
  TSepiConstant.Create(Result, 'MetersPerFoot', MetersPerFoot);
  TSepiConstant.Create(Result, 'MetersPerYard', MetersPerYard);
  TSepiConstant.Create(Result, 'MetersPerMile', MetersPerMile);
  TSepiConstant.Create(Result, 'MetersPerNauticalMiles', MetersPerNauticalMiles);
  TSepiConstant.Create(Result, 'MetersPerAstronomicalUnit', MetersPerAstronomicalUnit);
  TSepiConstant.Create(Result, 'MetersPerLightSecond', MetersPerLightSecond);
  TSepiConstant.Create(Result, 'MetersPerLightYear', MetersPerLightYear);
  TSepiConstant.Create(Result, 'MetersPerParsec', MetersPerParsec);
  TSepiConstant.Create(Result, 'MetersPerCubit', MetersPerCubit);
  TSepiConstant.Create(Result, 'MetersPerFathom', MetersPerFathom);
  TSepiConstant.Create(Result, 'MetersPerFurlong', MetersPerFurlong);
  TSepiConstant.Create(Result, 'MetersPerHand', MetersPerHand);
  TSepiConstant.Create(Result, 'MetersPerPace', MetersPerPace);
  TSepiConstant.Create(Result, 'MetersPerRod', MetersPerRod);
  TSepiConstant.Create(Result, 'MetersPerChain', MetersPerChain);
  TSepiConstant.Create(Result, 'MetersPerLink', MetersPerLink);
  TSepiConstant.Create(Result, 'MetersPerPoint', MetersPerPoint);
  TSepiConstant.Create(Result, 'MetersPerPica', MetersPerPica);
  TSepiConstant.Create(Result, 'SquareMetersPerSquareInch', SquareMetersPerSquareInch);
  TSepiConstant.Create(Result, 'SquareMetersPerSquareFoot', SquareMetersPerSquareFoot);
  TSepiConstant.Create(Result, 'SquareMetersPerSquareYard', SquareMetersPerSquareYard);
  TSepiConstant.Create(Result, 'SquareMetersPerSquareMile', SquareMetersPerSquareMile);
  TSepiConstant.Create(Result, 'SquareMetersPerAcre', SquareMetersPerAcre);
  TSepiConstant.Create(Result, 'SquareMetersPerSquareRod', SquareMetersPerSquareRod);
  TSepiConstant.Create(Result, 'CubicMetersPerCubicInch', CubicMetersPerCubicInch);
  TSepiConstant.Create(Result, 'CubicMetersPerCubicFoot', CubicMetersPerCubicFoot);
  TSepiConstant.Create(Result, 'CubicMetersPerCubicYard', CubicMetersPerCubicYard);
  TSepiConstant.Create(Result, 'CubicMetersPerCubicMile', CubicMetersPerCubicMile);
  TSepiConstant.Create(Result, 'CubicMetersPerAcreFoot', CubicMetersPerAcreFoot);
  TSepiConstant.Create(Result, 'CubicMetersPerAcreInch', CubicMetersPerAcreInch);
  TSepiConstant.Create(Result, 'CubicMetersPerCord', CubicMetersPerCord);
  TSepiConstant.Create(Result, 'CubicMetersPerCordFoot', CubicMetersPerCordFoot);
  TSepiConstant.Create(Result, 'CubicMetersPerUSFluidGallon', CubicMetersPerUSFluidGallon);
  TSepiConstant.Create(Result, 'CubicMetersPerUSFluidQuart', CubicMetersPerUSFluidQuart);
  TSepiConstant.Create(Result, 'CubicMetersPerUSFluidPint', CubicMetersPerUSFluidPint);
  TSepiConstant.Create(Result, 'CubicMetersPerUSFluidCup', CubicMetersPerUSFluidCup);
  TSepiConstant.Create(Result, 'CubicMetersPerUSFluidGill', CubicMetersPerUSFluidGill);
  TSepiConstant.Create(Result, 'CubicMetersPerUSFluidOunce', CubicMetersPerUSFluidOunce);
  TSepiConstant.Create(Result, 'CubicMetersPerUSFluidTablespoon', CubicMetersPerUSFluidTablespoon);
  TSepiConstant.Create(Result, 'CubicMetersPerUSFluidTeaspoon', CubicMetersPerUSFluidTeaspoon);
  TSepiConstant.Create(Result, 'CubicMetersPerUSDryGallon', CubicMetersPerUSDryGallon);
  TSepiConstant.Create(Result, 'CubicMetersPerUSDryQuart', CubicMetersPerUSDryQuart);
  TSepiConstant.Create(Result, 'CubicMetersPerUSDryPint', CubicMetersPerUSDryPint);
  TSepiConstant.Create(Result, 'CubicMetersPerUSDryPeck', CubicMetersPerUSDryPeck);
  TSepiConstant.Create(Result, 'CubicMetersPerUSDryBucket', CubicMetersPerUSDryBucket);
  TSepiConstant.Create(Result, 'CubicMetersPerUSDryBushel', CubicMetersPerUSDryBushel);
  TSepiConstant.Create(Result, 'CubicMetersPerUKGallon', CubicMetersPerUKGallon);
  TSepiConstant.Create(Result, 'CubicMetersPerUKPottle', CubicMetersPerUKPottle);
  TSepiConstant.Create(Result, 'CubicMetersPerUKQuart', CubicMetersPerUKQuart);
  TSepiConstant.Create(Result, 'CubicMetersPerUKPint', CubicMetersPerUKPint);
  TSepiConstant.Create(Result, 'CubicMetersPerUKGill', CubicMetersPerUKGill);
  TSepiConstant.Create(Result, 'CubicMetersPerUKOunce', CubicMetersPerUKOunce);
  TSepiConstant.Create(Result, 'CubicMetersPerUKPeck', CubicMetersPerUKPeck);
  TSepiConstant.Create(Result, 'CubicMetersPerUKBucket', CubicMetersPerUKBucket);
  TSepiConstant.Create(Result, 'CubicMetersPerUKBushel', CubicMetersPerUKBushel);
  TSepiConstant.Create(Result, 'GramsPerPound', GramsPerPound);
  TSepiConstant.Create(Result, 'GramsPerDrams', GramsPerDrams);
  TSepiConstant.Create(Result, 'GramsPerGrains', GramsPerGrains);
  TSepiConstant.Create(Result, 'GramsPerTons', GramsPerTons);
  TSepiConstant.Create(Result, 'GramsPerLongTons', GramsPerLongTons);
  TSepiConstant.Create(Result, 'GramsPerOunces', GramsPerOunces);
  TSepiConstant.Create(Result, 'GramsPerStones', GramsPerStones);

  // Routines
  TSepiMetaMethod.Create(Result, 'FahrenheitToCelsius', @FahrenheitToCelsius,
    'function(const AValue: Double): Double');
  TSepiMetaMethod.Create(Result, 'CelsiusToFahrenheit', @CelsiusToFahrenheit,
    'function(const AValue: Double): Double');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('StdConvs', ImportUnit);
end.

