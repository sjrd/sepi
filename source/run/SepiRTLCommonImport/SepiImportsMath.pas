{*
  Importe l'unité Math dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsMath;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Math, Types;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEInvalidArgument = class(EInvalidArgument)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{-------------------------}
{ EInvalidArgument import }
{-------------------------}

class function TSepiImportsEInvalidArgument.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidArgument));

  with Result do
  begin

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function Min_0(const A, B: Integer): Integer;
begin
  Result := Min(A, B);
end;

function Min_1(const A, B: Int64): Int64;
begin
  Result := Min(A, B);
end;

function Min_2(const A, B: Single): Single;
begin
  Result := Min(A, B);
end;

function Min_3(const A, B: Double): Double;
begin
  Result := Min(A, B);
end;

function Min_4(const A, B: Extended): Extended;
begin
  Result := Min(A, B);
end;

function Max_0(const A, B: Integer): Integer;
begin
  Result := Max(A, B);
end;

function Max_1(const A, B: Int64): Int64;
begin
  Result := Max(A, B);
end;

function Max_2(const A, B: Single): Single;
begin
  Result := Max(A, B);
end;

function Max_3(const A, B: Double): Double;
begin
  Result := Max(A, B);
end;

function Max_4(const A, B: Extended): Extended;
begin
  Result := Max(A, B);
end;

function IsNan_0(const AValue: Double): Boolean;
begin
  Result := IsNan(AValue);
end;

function IsNan_1(const AValue: Single): Boolean;
begin
  Result := IsNan(AValue);
end;

function IsNan_2(const AValue: Extended): Boolean;
begin
  Result := IsNan(AValue);
end;

function Sign_0(const AValue: Integer): TValueSign;
begin
  Result := Sign(AValue);
end;

function Sign_1(const AValue: Int64): TValueSign;
begin
  Result := Sign(AValue);
end;

function Sign_2(const AValue: Double): TValueSign;
begin
  Result := Sign(AValue);
end;

function CompareValue_0(const A, B: Extended;
  Epsilon: Extended = 0): TValueRelationship;
begin
  Result := CompareValue(A, B, Epsilon);
end;

function CompareValue_1(const A, B: Double;
  Epsilon: Double = 0): TValueRelationship;
begin
  Result := CompareValue(A, B, Epsilon);
end;

function CompareValue_2(const A, B: Single;
  Epsilon: Single = 0): TValueRelationship;
begin
  Result := CompareValue(A, B, Epsilon);
end;

function CompareValue_3(const A, B: Integer): TValueRelationship;
begin
  Result := CompareValue(A, B);
end;

function CompareValue_4(const A, B: Int64): TValueRelationship;
begin
  Result := CompareValue(A, B);
end;

function SameValue_0(const A, B: Extended; Epsilon: Extended = 0): Boolean;
begin
  Result := SameValue(A, B, Epsilon);
end;

function SameValue_1(const A, B: Double; Epsilon: Double = 0): Boolean;
begin
  Result := SameValue(A, B, Epsilon);
end;

function SameValue_2(const A, B: Single; Epsilon: Single = 0): Boolean;
begin
  Result := SameValue(A, B, Epsilon);
end;

function IsZero_0(const A: Extended; Epsilon: Extended = 0): Boolean;
begin
  Result := IsZero(A, Epsilon);
end;

function IsZero_1(const A: Double; Epsilon: Double = 0): Boolean;
begin
  Result := IsZero(A, Epsilon);
end;

function IsZero_2(const A: Single; Epsilon: Single = 0): Boolean;
begin
  Result := IsZero(A, Epsilon);
end;

function IfThen_0(AValue: Boolean; const ATrue: Integer;
  const AFalse: Integer = 0): Integer;
begin
  Result := IfThen(AValue, ATrue, AFalse);
end;

function IfThen_1(AValue: Boolean; const ATrue: Int64;
  const AFalse: Int64 = 0): Int64;
begin
  Result := IfThen(AValue, ATrue, AFalse);
end;

function IfThen_2(AValue: Boolean; const ATrue: Double;
  const AFalse: Double = 0.0): Double;
begin
  Result := IfThen(AValue, ATrue, AFalse);
end;

function RandomFrom_0(const AValues: array of Integer): Integer;
begin
  Result := RandomFrom(AValues);
end;

function RandomFrom_1(const AValues: array of Int64): Int64;
begin
  Result := RandomFrom(AValues);
end;

function RandomFrom_2(const AValues: array of Double): Double;
begin
  Result := RandomFrom(AValues);
end;

function InRange_0(const AValue, AMin, AMax: Integer): Boolean;
begin
  Result := InRange(AValue, AMin, AMax);
end;

function InRange_1(const AValue, AMin, AMax: Int64): Boolean;
begin
  Result := InRange(AValue, AMin, AMax);
end;

function InRange_2(const AValue, AMin, AMax: Double): Boolean;
begin
  Result := InRange(AValue, AMin, AMax);
end;

function EnsureRange_0(const AValue, AMin, AMax: Integer): Integer;
begin
  Result := EnsureRange(AValue, AMin, AMax);
end;

function EnsureRange_1(const AValue, AMin, AMax: Int64): Int64;
begin
  Result := EnsureRange(AValue, AMin, AMax);
end;

function EnsureRange_2(const AValue, AMin, AMax: Double): Double;
begin
  Result := EnsureRange(AValue, AMin, AMax);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'Math',
    ['SysUtils', 'Types']);

  // Constants
  TSepiConstant.Create(Result, 'MinSingle', MinSingle);
  TSepiConstant.Create(Result, 'MaxSingle', MaxSingle);
  TSepiConstant.Create(Result, 'MinDouble', MinDouble);
  TSepiConstant.Create(Result, 'MaxDouble', MaxDouble);
  TSepiConstant.Create(Result, 'MinExtended', MinExtended);
  //TSepiConstant.Create(Result, 'MaxExtended', MaxExtended);
  TSepiConstant.Create(Result, 'MinComp', MinComp);
  TSepiConstant.Create(Result, 'MaxComp', MaxComp);
  TSepiConstant.Create(Result, 'NaN', NaN);
  TSepiConstant.Create(Result, 'Infinity', Infinity);
  TSepiConstant.Create(Result, 'NegInfinity', NegInfinity);

  // Routines
  TSepiMethod.Create(Result, 'ArcCos', @ArcCos,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcSin', @ArcSin,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcTan2', @ArcTan2,
    'function(const Y, X: Extended): Extended');
  TSepiMethod.Create(Result, 'SinCos', @SinCos,
    'procedure(const Theta: Extended; var Sin, Cos: Extended)');
  TSepiMethod.Create(Result, 'Tan', @Tan,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Cotan', @Cotan,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Secant', @Secant,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Cosecant', @Cosecant,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Hypot', @Hypot,
    'function(const X, Y: Extended): Extended');
  TSepiMethod.Create(Result, 'RadToDeg', @RadToDeg,
    'function(const Radians: Extended): Extended');
  TSepiMethod.Create(Result, 'RadToGrad', @RadToGrad,
    'function(const Radians: Extended): Extended');
  TSepiMethod.Create(Result, 'RadToCycle', @RadToCycle,
    'function(const Radians: Extended): Extended');
  TSepiMethod.Create(Result, 'DegToRad', @DegToRad,
    'function(const Degrees: Extended): Extended');
  TSepiMethod.Create(Result, 'DegToGrad', @DegToGrad,
    'function(const Degrees: Extended): Extended');
  TSepiMethod.Create(Result, 'DegToCycle', @DegToCycle,
    'function(const Degrees: Extended): Extended');
  TSepiMethod.Create(Result, 'GradToRad', @GradToRad,
    'function(const Grads: Extended): Extended');
  TSepiMethod.Create(Result, 'GradToDeg', @GradToDeg,
    'function(const Grads: Extended): Extended');
  TSepiMethod.Create(Result, 'GradToCycle', @GradToCycle,
    'function(const Grads: Extended): Extended');
  TSepiMethod.Create(Result, 'CycleToRad', @CycleToRad,
    'function(const Cycles: Extended): Extended');
  TSepiMethod.Create(Result, 'CycleToDeg', @CycleToDeg,
    'function(const Cycles: Extended): Extended');
  TSepiMethod.Create(Result, 'CycleToGrad', @CycleToGrad,
    'function(const Cycles: Extended): Extended');
  TSepiMethod.Create(Result, 'Cot', @Cot,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Sec', @Sec,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Csc', @Csc,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Cosh', @Cosh,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Sinh', @Sinh,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Tanh', @Tanh,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'CotH', @CotH,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'SecH', @SecH,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'CscH', @CscH,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcCot', @ArcCot,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcSec', @ArcSec,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcCsc', @ArcCsc,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcCosh', @ArcCosh,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcSinh', @ArcSinh,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcTanh', @ArcTanh,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcCotH', @ArcCotH,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcSecH', @ArcSecH,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcCscH', @ArcCscH,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'LnXP1', @LnXP1,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Log10', @Log10,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Log2', @Log2,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'LogN', @LogN,
    'function(const Base, X: Extended): Extended');
  TSepiMethod.Create(Result, 'IntPower', @IntPower,
    'function(const Base: Extended; const Exponent: Integer): Extended');
  TSepiMethod.Create(Result, 'Power', @Power,
    'function(const Base, Exponent: Extended): Extended');
  TSepiMethod.Create(Result, 'Frexp', @Frexp,
    'procedure(const X: Extended; var Mantissa: Extended; var Exponent: Integer)');
  TSepiMethod.Create(Result, 'Ldexp', @Ldexp,
    'function(const X: Extended; const P: Integer): Extended');
  TSepiMethod.Create(Result, 'Ceil', @Ceil,
    'function(const X: Extended):Integer');
  TSepiMethod.Create(Result, 'Floor', @Floor,
    'function(const X: Extended): Integer');
  TSepiMethod.Create(Result, 'Poly', @Poly,
    'function(const X: Extended; const Coefficients: array of Double): Extended');
  TSepiMethod.Create(Result, 'Mean', @Mean,
    'function(const Data: array of Double): Extended');
  TSepiMethod.Create(Result, 'Sum', @Sum,
    'function(const Data: array of Double): Extended');
  TSepiMethod.Create(Result, 'SumInt', @SumInt,
    'function(const Data: array of Integer): Integer');
  TSepiMethod.Create(Result, 'SumOfSquares', @SumOfSquares,
    'function(const Data: array of Double): Extended');
  TSepiMethod.Create(Result, 'SumsAndSquares', @SumsAndSquares,
    'procedure(const Data: array of Double; var Sum, SumOfSquares: Extended )');
  TSepiMethod.Create(Result, 'MinValue', @MinValue,
    'function(const Data: array of Double): Double');
  TSepiMethod.Create(Result, 'MinIntValue', @MinIntValue,
    'function(const Data: array of Integer): Integer');
  TSepiOverloadedMethod.Create(Result, 'Min');
  TSepiMethod.Create(Result, 'OL$Min$0', @Min_0,
    'function(const A, B: Integer): Integer');
  TSepiMethod.Create(Result, 'OL$Min$1', @Min_1,
    'function(const A, B: Int64): Int64');
  TSepiMethod.Create(Result, 'OL$Min$2', @Min_2,
    'function(const A, B: Single): Single');
  TSepiMethod.Create(Result, 'OL$Min$3', @Min_3,
    'function(const A, B: Double): Double');
  TSepiMethod.Create(Result, 'OL$Min$4', @Min_4,
    'function(const A, B: Extended): Extended');
  TSepiMethod.Create(Result, 'MaxValue', @MaxValue,
    'function(const Data: array of Double): Double');
  TSepiMethod.Create(Result, 'MaxIntValue', @MaxIntValue,
    'function(const Data: array of Integer): Integer');
  TSepiOverloadedMethod.Create(Result, 'Max');
  TSepiMethod.Create(Result, 'OL$Max$0', @Max_0,
    'function(const A, B: Integer): Integer');
  TSepiMethod.Create(Result, 'OL$Max$1', @Max_1,
    'function(const A, B: Int64): Int64');
  TSepiMethod.Create(Result, 'OL$Max$2', @Max_2,
    'function(const A, B: Single): Single');
  TSepiMethod.Create(Result, 'OL$Max$3', @Max_3,
    'function(const A, B: Double): Double');
  TSepiMethod.Create(Result, 'OL$Max$4', @Max_4,
    'function(const A, B: Extended): Extended');
  TSepiMethod.Create(Result, 'StdDev', @StdDev,
    'function(const Data: array of Double): Extended');
  TSepiMethod.Create(Result, 'MeanAndStdDev', @MeanAndStdDev,
    'procedure(const Data: array of Double; var Mean, StdDev: Extended)');
  TSepiMethod.Create(Result, 'PopnStdDev', @PopnStdDev,
    'function(const Data: array of Double): Extended');
  TSepiMethod.Create(Result, 'Variance', @Variance,
    'function(const Data: array of Double): Extended');
  TSepiMethod.Create(Result, 'PopnVariance', @PopnVariance,
    'function(const Data: array of Double): Extended');
  TSepiMethod.Create(Result, 'TotalVariance', @TotalVariance,
    'function(const Data: array of Double): Extended');
  TSepiMethod.Create(Result, 'Norm', @Norm,
    'function(const Data: array of Double): Extended');
  TSepiMethod.Create(Result, 'MomentSkewKurtosis', @MomentSkewKurtosis,
    'procedure(const Data: array of Double; var M1, M2, M3, M4, Skew, Kurtosis: Extended )');
  TSepiMethod.Create(Result, 'RandG', @RandG,
    'function(Mean, StdDev: Extended): Extended');
  TSepiOverloadedMethod.Create(Result, 'IsNan');
  TSepiMethod.Create(Result, 'OL$IsNan$0', @IsNan_0,
    'function(const AValue: Double): Boolean');
  TSepiMethod.Create(Result, 'OL$IsNan$1', @IsNan_1,
    'function(const AValue: Single): Boolean');
  TSepiMethod.Create(Result, 'OL$IsNan$2', @IsNan_2,
    'function(const AValue: Extended): Boolean');
  TSepiMethod.Create(Result, 'IsInfinite', @IsInfinite,
    'function(const AValue: Double): Boolean');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TValueSign));

  // Constants
  TSepiConstant.Create(Result, 'NegativeValue', NegativeValue);
  TSepiConstant.Create(Result, 'ZeroValue', ZeroValue);
  TSepiConstant.Create(Result, 'PositiveValue', PositiveValue);

  // Routines
  TSepiOverloadedMethod.Create(Result, 'Sign');
  TSepiMethod.Create(Result, 'OL$Sign$0', @Sign_0,
    'function(const AValue: Integer): TValueSign');
  TSepiMethod.Create(Result, 'OL$Sign$1', @Sign_1,
    'function(const AValue: Int64): TValueSign');
  TSepiMethod.Create(Result, 'OL$Sign$2', @Sign_2,
    'function(const AValue: Double): TValueSign');
  TSepiOverloadedMethod.Create(Result, 'CompareValue');
  TSepiMethod.Create(Result, 'OL$CompareValue$0', @CompareValue_0,
    'function(const A, B: Extended; Epsilon: Extended = 0): TValueRelationship');
  TSepiMethod.Create(Result, 'OL$CompareValue$1', @CompareValue_1,
    'function(const A, B: Double; Epsilon: Double = 0): TValueRelationship');
  TSepiMethod.Create(Result, 'OL$CompareValue$2', @CompareValue_2,
    'function(const A, B: Single; Epsilon: Single = 0): TValueRelationship');
  TSepiMethod.Create(Result, 'OL$CompareValue$3', @CompareValue_3,
    'function(const A, B: Integer): TValueRelationship');
  TSepiMethod.Create(Result, 'OL$CompareValue$4', @CompareValue_4,
    'function(const A, B: Int64): TValueRelationship');
  TSepiOverloadedMethod.Create(Result, 'SameValue');
  TSepiMethod.Create(Result, 'OL$SameValue$0', @SameValue_0,
    'function(const A, B: Extended; Epsilon: Extended = 0): Boolean');
  TSepiMethod.Create(Result, 'OL$SameValue$1', @SameValue_1,
    'function(const A, B: Double; Epsilon: Double = 0): Boolean');
  TSepiMethod.Create(Result, 'OL$SameValue$2', @SameValue_2,
    'function(const A, B: Single; Epsilon: Single = 0): Boolean');
  TSepiOverloadedMethod.Create(Result, 'IsZero');
  TSepiMethod.Create(Result, 'OL$IsZero$0', @IsZero_0,
    'function(const A: Extended; Epsilon: Extended = 0): Boolean');
  TSepiMethod.Create(Result, 'OL$IsZero$1', @IsZero_1,
    'function(const A: Double; Epsilon: Double = 0): Boolean');
  TSepiMethod.Create(Result, 'OL$IsZero$2', @IsZero_2,
    'function(const A: Single; Epsilon: Single = 0): Boolean');
  TSepiOverloadedMethod.Create(Result, 'IfThen');
  TSepiMethod.Create(Result, 'OL$IfThen$0', @IfThen_0,
    'function(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer');
  TSepiMethod.Create(Result, 'OL$IfThen$1', @IfThen_1,
    'function(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64');
  TSepiMethod.Create(Result, 'OL$IfThen$2', @IfThen_2,
    'function(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0.0): Double');
  TSepiMethod.Create(Result, 'RandomRange', @RandomRange,
    'function(const AFrom, ATo: Integer): Integer');
  TSepiOverloadedMethod.Create(Result, 'RandomFrom');
  TSepiMethod.Create(Result, 'OL$RandomFrom$0', @RandomFrom_0,
    'function(const AValues: array of Integer): Integer');
  TSepiMethod.Create(Result, 'OL$RandomFrom$1', @RandomFrom_1,
    'function(const AValues: array of Int64): Int64');
  TSepiMethod.Create(Result, 'OL$RandomFrom$2', @RandomFrom_2,
    'function(const AValues: array of Double): Double');
  TSepiOverloadedMethod.Create(Result, 'InRange');
  TSepiMethod.Create(Result, 'OL$InRange$0', @InRange_0,
    'function(const AValue, AMin, AMax: Integer): Boolean');
  TSepiMethod.Create(Result, 'OL$InRange$1', @InRange_1,
    'function(const AValue, AMin, AMax: Int64): Boolean');
  TSepiMethod.Create(Result, 'OL$InRange$2', @InRange_2,
    'function(const AValue, AMin, AMax: Double): Boolean');
  TSepiOverloadedMethod.Create(Result, 'EnsureRange');
  TSepiMethod.Create(Result, 'OL$EnsureRange$0', @EnsureRange_0,
    'function(const AValue, AMin, AMax: Integer): Integer');
  TSepiMethod.Create(Result, 'OL$EnsureRange$1', @EnsureRange_1,
    'function(const AValue, AMin, AMax: Int64): Int64');
  TSepiMethod.Create(Result, 'OL$EnsureRange$2', @EnsureRange_2,
    'function(const AValue, AMin, AMax: Double): Double');
  TSepiMethod.Create(Result, 'DivMod', @DivMod,
    'procedure(Dividend: Cardinal; Divisor: Word; var Result, Remainder: Word )');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TRoundToRange));

  // Routines
  TSepiMethod.Create(Result, 'RoundTo', @RoundTo,
    'function(const AValue: Double; const ADigit: TRoundToRange): Double');
  TSepiMethod.Create(Result, 'SimpleRoundTo', @SimpleRoundTo,
    'function(const AValue: Double; const ADigit: TRoundToRange = -2): Double');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPaymentTime));

  // Routines
  TSepiMethod.Create(Result, 'DoubleDecliningBalance', @DoubleDecliningBalance,
    'function(const Cost, Salvage: Extended; Life, Period: Integer ) : Extended');
  TSepiMethod.Create(Result, 'FutureValue', @FutureValue,
    'function(const Rate: Extended; NPeriods: Integer; const Payment, PresentValue : Extended ; PaymentTime: TPaymentTime ) : Extended');
  TSepiMethod.Create(Result, 'InterestPayment', @InterestPayment,
    'function(const Rate: Extended; Period, NPeriods: Integer; const PresentValue, FutureValue: Extended ; PaymentTime: TPaymentTime ) : Extended');
  TSepiMethod.Create(Result, 'InterestRate', @InterestRate,
    'function(NPeriods: Integer; const Payment, PresentValue, FutureValue : Extended ; PaymentTime: TPaymentTime ) : Extended');
  TSepiMethod.Create(Result, 'InternalRateOfReturn', @InternalRateOfReturn,
    'function(const Guess: Extended; const CashFlows: array of Double ) : Extended');
  TSepiMethod.Create(Result, 'NumberOfPeriods', @NumberOfPeriods,
    'function(const Rate: Extended; Payment: Extended; const PresentValue, FutureValue: Extended ; PaymentTime: TPaymentTime ) : Extended');
  TSepiMethod.Create(Result, 'NetPresentValue', @NetPresentValue,
    'function(const Rate: Extended; const CashFlows: array of Double; PaymentTime: TPaymentTime ) : Extended');
  TSepiMethod.Create(Result, 'Payment', @Payment,
    'function(Rate: Extended; NPeriods: Integer; const PresentValue, FutureValue : Extended ; PaymentTime: TPaymentTime ) : Extended');
  TSepiMethod.Create(Result, 'PeriodPayment', @PeriodPayment,
    'function(const Rate: Extended; Period, NPeriods: Integer; const PresentValue, FutureValue: Extended ; PaymentTime: TPaymentTime ) : Extended');
  TSepiMethod.Create(Result, 'PresentValue', @PresentValue,
    'function(const Rate: Extended; NPeriods: Integer; const Payment, FutureValue: Extended ; PaymentTime: TPaymentTime ) : Extended');
  TSepiMethod.Create(Result, 'SLNDepreciation', @SLNDepreciation,
    'function(const Cost, Salvage: Extended; Life: Integer): Extended');
  TSepiMethod.Create(Result, 'SYDDepreciation', @SYDDepreciation,
    'function(const Cost, Salvage: Extended; Life, Period: Integer): Extended');

  // Types
  TSepiImportsEInvalidArgument.SepiImport(Result);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFPURoundingMode));

  // Routines
  TSepiMethod.Create(Result, 'GetRoundMode', @GetRoundMode,
    'function: TFPURoundingMode');
  TSepiMethod.Create(Result, 'SetRoundMode', @SetRoundMode,
    'function(const RoundMode: TFPURoundingMode): TFPURoundingMode');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFPUPrecisionMode));

  // Routines
  TSepiMethod.Create(Result, 'GetPrecisionMode', @GetPrecisionMode,
    'function: TFPUPrecisionMode');
  TSepiMethod.Create(Result, 'SetPrecisionMode', @SetPrecisionMode,
    'function(const Precision: TFPUPrecisionMode): TFPUPrecisionMode');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFPUException));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFPUExceptionMask));

  // Routines
  TSepiMethod.Create(Result, 'GetExceptionMask', @GetExceptionMask,
    'function: TFPUExceptionMask');
  TSepiMethod.Create(Result, 'SetExceptionMask', @SetExceptionMask,
    'function(const Mask: TFPUExceptionMask): TFPUExceptionMask');
  TSepiMethod.Create(Result, 'ClearExceptions', @ClearExceptions,
    'procedure(RaisePending: Boolean = True)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Math', ImportUnit);
end.

