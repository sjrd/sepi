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

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'VarCmplx',
    ['Variants']);

  // Routines
  TSepiMetaOverloadedMethod.Create(Result, 'VarComplexCreate');
  TSepiMetaMethod.Create(Result, 'OL$VarComplexCreate$0', @VarComplexCreate_0,
    'function: Variant');
  TSepiMetaMethod.Create(Result, 'OL$VarComplexCreate$1', @VarComplexCreate_1,
    'function(const AReal: Double): Variant');
  TSepiMetaMethod.Create(Result, 'OL$VarComplexCreate$2', @VarComplexCreate_2,
    'function(const AReal, AImaginary: Double): Variant');
  TSepiMetaMethod.Create(Result, 'OL$VarComplexCreate$3', @VarComplexCreate_3,
    'function(const AText: string): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplex', @VarComplex,
    'function: TVarType');
  TSepiMetaMethod.Create(Result, 'VarIsComplex', @VarIsComplex,
    'function(const AValue: Variant): Boolean');
  TSepiMetaMethod.Create(Result, 'VarAsComplex', @VarAsComplex,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexSimplify', @VarComplexSimplify,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexAbsSqr', @VarComplexAbsSqr,
    'function(const AValue: Variant): Double');
  TSepiMetaMethod.Create(Result, 'VarComplexAbs', @VarComplexAbs,
    'function(const AValue: Variant): Double');
  TSepiMetaMethod.Create(Result, 'VarComplexAngle', @VarComplexAngle,
    'function(const AValue: Variant): Double');
  TSepiMetaMethod.Create(Result, 'VarComplexSign', @VarComplexSign,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexConjugate', @VarComplexConjugate,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexInverse', @VarComplexInverse,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexExp', @VarComplexExp,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexLn', @VarComplexLn,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexLog2', @VarComplexLog2,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexLog10', @VarComplexLog10,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexLogN', @VarComplexLogN,
    'function(const AValue: Variant; const X: Double): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexSqr', @VarComplexSqr,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexSqrt', @VarComplexSqrt,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexPower', @VarComplexPower,
    'function(const AValue, APower: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexTimesPosI', @VarComplexTimesPosI,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexTimesNegI', @VarComplexTimesNegI,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexTimesImaginary', @VarComplexTimesImaginary,
    'function(const AValue: Variant; const AFactor: Double): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexTimesReal', @VarComplexTimesReal,
    'function(const AValue: Variant; const AFactor: Double): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexCos', @VarComplexCos,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexSin', @VarComplexSin,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexTan', @VarComplexTan,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexCot', @VarComplexCot,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexSec', @VarComplexSec,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexCsc', @VarComplexCsc,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcCos', @VarComplexArcCos,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcSin', @VarComplexArcSin,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcTan', @VarComplexArcTan,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcCot', @VarComplexArcCot,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcSec', @VarComplexArcSec,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcCsc', @VarComplexArcCsc,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexCosH', @VarComplexCosH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexSinH', @VarComplexSinH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexTanH', @VarComplexTanH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexCotH', @VarComplexCotH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexSecH', @VarComplexSecH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexCscH', @VarComplexCscH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcCosH', @VarComplexArcCosH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcSinH', @VarComplexArcSinH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcTanH', @VarComplexArcTanH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcCotH', @VarComplexArcCotH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcSecH', @VarComplexArcSecH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexArcCscH', @VarComplexArcCscH,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'VarComplexToPolar', @VarComplexToPolar,
    'procedure(const AValue: Variant; var ARadius, ATheta: Double; AFixTheta: Boolean = True )');
  TSepiMetaMethod.Create(Result, 'VarComplexFromPolar', @VarComplexFromPolar,
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

