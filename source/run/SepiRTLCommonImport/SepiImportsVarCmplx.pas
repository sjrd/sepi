{*
  Importe l'unité VarCmplx dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsVarCmplx;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, VarCmplx;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function VarComplexCreate_0: Variant;
begin
  Result := VarComplexCreate;
end;

function VarComplexCreate_1(const AReal: Double): Variant;
begin
  Result := VarComplexCreate(AReal);
end;

function VarComplexCreate_2(const AReal, AImaginary: Double): Variant;
begin
  Result := VarComplexCreate(AReal, AImaginary);
end;

function VarComplexCreate_3(const AText: string): Variant;
begin
  Result := VarComplexCreate(AText);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'VarCmplx',
    ['Variants']);

  // Routines
  TSepiOverloadedMethod.Create(Result, 'VarComplexCreate');
  TSepiMethod.Create(Result, 'OL$VarComplexCreate$0', @VarComplexCreate_0,
    'function: Variant');
  TSepiMethod.Create(Result, 'OL$VarComplexCreate$1', @VarComplexCreate_1,
    'function(const AReal: Double): Variant');
  TSepiMethod.Create(Result, 'OL$VarComplexCreate$2', @VarComplexCreate_2,
    'function(const AReal, AImaginary: Double): Variant');
  TSepiMethod.Create(Result, 'OL$VarComplexCreate$3', @VarComplexCreate_3,
    'function(const AText: string): Variant');
  TSepiMethod.Create(Result, 'VarComplex', @VarComplex,
    'function: TVarType');
  TSepiMethod.Create(Result, 'VarIsComplex', @VarIsComplex,
    'function(const AValue: Variant): Boolean');
  TSepiMethod.Create(Result, 'VarAsComplex', @VarAsComplex,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexSimplify', @VarComplexSimplify,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexAbsSqr', @VarComplexAbsSqr,
    'function(const AValue: Variant): Double');
  TSepiMethod.Create(Result, 'VarComplexAbs', @VarComplexAbs,
    'function(const AValue: Variant): Double');
  TSepiMethod.Create(Result, 'VarComplexAngle', @VarComplexAngle,
    'function(const AValue: Variant): Double');
  TSepiMethod.Create(Result, 'VarComplexSign', @VarComplexSign,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexConjugate', @VarComplexConjugate,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexInverse', @VarComplexInverse,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexExp', @VarComplexExp,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexLn', @VarComplexLn,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexLog2', @VarComplexLog2,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexLog10', @VarComplexLog10,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexLogN', @VarComplexLogN,
    'function(const AValue: Variant; const X: Double): Variant');
  TSepiMethod.Create(Result, 'VarComplexSqr', @VarComplexSqr,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexSqrt', @VarComplexSqrt,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexPower', @VarComplexPower,
    'function(const AValue, APower: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexTimesPosI', @VarComplexTimesPosI,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexTimesNegI', @VarComplexTimesNegI,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexTimesImaginary',
    @VarComplexTimesImaginary,
    'function(const AValue: Variant; const AFactor: Double): Variant');
  TSepiMethod.Create(Result, 'VarComplexTimesReal', @VarComplexTimesReal,
    'function(const AValue: Variant; const AFactor: Double): Variant');
  TSepiMethod.Create(Result, 'VarComplexCos', @VarComplexCos,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexSin', @VarComplexSin,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexTan', @VarComplexTan,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexCot', @VarComplexCot,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexSec', @VarComplexSec,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexCsc', @VarComplexCsc,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcCos', @VarComplexArcCos,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcSin', @VarComplexArcSin,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcTan', @VarComplexArcTan,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcCot', @VarComplexArcCot,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcSec', @VarComplexArcSec,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcCsc', @VarComplexArcCsc,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexCosH', @VarComplexCosH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexSinH', @VarComplexSinH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexTanH', @VarComplexTanH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexCotH', @VarComplexCotH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexSecH', @VarComplexSecH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexCscH', @VarComplexCscH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcCosH', @VarComplexArcCosH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcSinH', @VarComplexArcSinH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcTanH', @VarComplexArcTanH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcCotH', @VarComplexArcCotH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcSecH', @VarComplexArcSecH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexArcCscH', @VarComplexArcCscH,
    'function(const AValue: Variant): Variant');
  TSepiMethod.Create(Result, 'VarComplexToPolar', @VarComplexToPolar,
    'procedure(const AValue: Variant; var ARadius, ATheta: Double; AFixTheta: Boolean = True )');
  TSepiMethod.Create(Result, 'VarComplexFromPolar', @VarComplexFromPolar,
    'function(const ARadius, ATheta: Double): Variant');

  // Global variables
  TSepiVariable.Create(Result, 'ComplexNumberSymbol',
    ComplexNumberSymbol, TypeInfo(string));
  TSepiVariable.Create(Result, 'ComplexNumberSymbolBeforeImaginary',
    ComplexNumberSymbolBeforeImaginary, TypeInfo(Boolean));
  TSepiVariable.Create(Result, 'ComplexNumberDefuzzAtZero',
    ComplexNumberDefuzzAtZero, TypeInfo(Boolean));

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('VarCmplx', ImportUnit);
end.

