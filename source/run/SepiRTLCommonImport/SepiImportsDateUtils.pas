{*
  Importe l'unité DateUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsDateUtils;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, DateUtils;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function StartOfADay_0(const AYear, AMonth, ADay: Word): TDateTime;
begin
  Result := StartOfADay(AYear, AMonth, ADay);
end;

function EndOfADay_0(const AYear, AMonth, ADay: Word): TDateTime;
begin
  Result := EndOfADay(AYear, AMonth, ADay);
end;

function StartOfADay_1(const AYear, ADayOfYear: Word): TDateTime;
begin
  Result := StartOfADay(AYear, ADayOfYear);
end;

function EndOfADay_1(const AYear, ADayOfYear: Word): TDateTime;
begin
  Result := EndOfADay(AYear, ADayOfYear);
end;

function WeekOfTheYear_0(const AValue: TDateTime): Word;
begin
  Result := WeekOfTheYear(AValue);
end;

function WeekOfTheYear_1(const AValue: TDateTime; var AYear: Word): Word;
begin
  Result := WeekOfTheYear(AValue, AYear);
end;

function WeekOfTheMonth_0(const AValue: TDateTime): Word;
begin
  Result := WeekOfTheMonth(AValue);
end;

function WeekOfTheMonth_1(const AValue: TDateTime;
  var
    AYear, AMonth: Word): Word;
begin
  Result := WeekOfTheMonth(AValue, AYear, AMonth);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'DateUtils',
    ['SysUtils', 'Math', 'Types']);

  // Routines
  TSepiMethod.Create(Result, 'DateOf', @DateOf,
    'function(const AValue: TDateTime): TDateTime');
  TSepiMethod.Create(Result, 'TimeOf', @TimeOf,
    'function(const AValue: TDateTime): TDateTime');
  TSepiMethod.Create(Result, 'IsInLeapYear', @IsInLeapYear,
    'function(const AValue: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'IsPM', @IsPM,
    'function(const AValue: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'IsValidDate', @IsValidDate,
    'function(const AYear, AMonth, ADay: Word): Boolean');
  TSepiMethod.Create(Result, 'IsValidTime', @IsValidTime,
    'function(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean');
  TSepiMethod.Create(Result, 'IsValidDateTime', @IsValidDateTime,
    'function(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond : Word ) : Boolean');
  TSepiMethod.Create(Result, 'IsValidDateDay', @IsValidDateDay,
    'function(const AYear, ADayOfYear: Word): Boolean');
  TSepiMethod.Create(Result, 'IsValidDateWeek', @IsValidDateWeek,
    'function(const AYear, AWeekOfYear, ADayOfWeek : Word ) : Boolean');
  TSepiMethod.Create(Result, 'IsValidDateMonthWeek', @IsValidDateMonthWeek,
    'function(const AYear, AMonth, AWeekOfMonth, ADayOfWeek : Word ) : Boolean');
  TSepiMethod.Create(Result, 'WeeksInYear', @WeeksInYear,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'WeeksInAYear', @WeeksInAYear,
    'function(const AYear: Word): Word');
  TSepiMethod.Create(Result, 'DaysInYear', @DaysInYear,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'DaysInAYear', @DaysInAYear,
    'function(const AYear: Word): Word');
  TSepiMethod.Create(Result, 'DaysInMonth', @DaysInMonth,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'DaysInAMonth', @DaysInAMonth,
    'function(const AYear, AMonth: Word): Word');
  TSepiMethod.Create(Result, 'Today', @Today,
    'function: TDateTime');
  TSepiMethod.Create(Result, 'Yesterday', @Yesterday,
    'function: TDateTime');
  TSepiMethod.Create(Result, 'Tomorrow', @Tomorrow,
    'function: TDateTime');
  TSepiMethod.Create(Result, 'IsToday', @IsToday,
    'function(const AValue: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'IsSameDay', @IsSameDay,
    'function(const AValue, ABasis: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'YearOf', @YearOf,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'MonthOf', @MonthOf,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'WeekOf', @WeekOf,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'DayOf', @DayOf,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'HourOf', @HourOf,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'MinuteOf', @MinuteOf,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'SecondOf', @SecondOf,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'MilliSecondOf', @MilliSecondOf,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'StartOfTheYear', @StartOfTheYear,
    'function(const AValue: TDateTime): TDateTime');
  TSepiMethod.Create(Result, 'EndOfTheYear', @EndOfTheYear,
    'function(const AValue: TDateTime): TDateTime');
  TSepiMethod.Create(Result, 'StartOfAYear', @StartOfAYear,
    'function(const AYear: Word): TDateTime');
  TSepiMethod.Create(Result, 'EndOfAYear', @EndOfAYear,
    'function(const AYear: Word): TDateTime');
  TSepiMethod.Create(Result, 'StartOfTheMonth', @StartOfTheMonth,
    'function(const AValue: TDateTime): TDateTime');
  TSepiMethod.Create(Result, 'EndOfTheMonth', @EndOfTheMonth,
    'function(const AValue: TDateTime): TDateTime');
  TSepiMethod.Create(Result, 'StartOfAMonth', @StartOfAMonth,
    'function(const AYear, AMonth: Word): TDateTime');
  TSepiMethod.Create(Result, 'EndOfAMonth', @EndOfAMonth,
    'function(const AYear, AMonth: Word): TDateTime');
  TSepiMethod.Create(Result, 'StartOfTheWeek', @StartOfTheWeek,
    'function(const AValue: TDateTime): TDateTime');
  TSepiMethod.Create(Result, 'EndOfTheWeek', @EndOfTheWeek,
    'function(const AValue: TDateTime): TDateTime');
  TSepiMethod.Create(Result, 'StartOfAWeek', @StartOfAWeek,
    'function(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word = 1 ) : TDateTime');
  TSepiMethod.Create(Result, 'EndOfAWeek', @EndOfAWeek,
    'function(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word = 7 ) : TDateTime');
  TSepiMethod.Create(Result, 'StartOfTheDay', @StartOfTheDay,
    'function(const AValue: TDateTime): TDateTime');
  TSepiMethod.Create(Result, 'EndOfTheDay', @EndOfTheDay,
    'function(const AValue: TDateTime): TDateTime');
  TSepiOverloadedMethod.Create(Result, 'StartOfADay');
  TSepiMethod.Create(Result, 'OL$StartOfADay$0', @StartOfADay_0,
    'function(const AYear, AMonth, ADay: Word): TDateTime');
  TSepiOverloadedMethod.Create(Result, 'EndOfADay');
  TSepiMethod.Create(Result, 'OL$EndOfADay$0', @EndOfADay_0,
    'function(const AYear, AMonth, ADay: Word): TDateTime');
  TSepiMethod.Create(Result, 'OL$StartOfADay$1', @StartOfADay_1,
    'function(const AYear, ADayOfYear: Word): TDateTime');
  TSepiMethod.Create(Result, 'OL$EndOfADay$1', @EndOfADay_1,
    'function(const AYear, ADayOfYear: Word): TDateTime');
  TSepiMethod.Create(Result, 'MonthOfTheYear', @MonthOfTheYear,
    'function(const AValue: TDateTime): Word');
  TSepiOverloadedMethod.Create(Result, 'WeekOfTheYear');
  TSepiMethod.Create(Result, 'OL$WeekOfTheYear$0', @WeekOfTheYear_0,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'OL$WeekOfTheYear$1', @WeekOfTheYear_1,
    'function(const AValue: TDateTime; var AYear: Word ) : Word');
  TSepiMethod.Create(Result, 'DayOfTheYear', @DayOfTheYear,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'HourOfTheYear', @HourOfTheYear,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'MinuteOfTheYear', @MinuteOfTheYear,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'SecondOfTheYear', @SecondOfTheYear,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'MilliSecondOfTheYear', @MilliSecondOfTheYear,
    'function(const AValue: TDateTime): Int64');
  TSepiOverloadedMethod.Create(Result, 'WeekOfTheMonth');
  TSepiMethod.Create(Result, 'OL$WeekOfTheMonth$0', @WeekOfTheMonth_0,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'OL$WeekOfTheMonth$1', @WeekOfTheMonth_1,
    'function(const AValue: TDateTime; var AYear, AMonth : Word ) : Word');
  TSepiMethod.Create(Result, 'DayOfTheMonth', @DayOfTheMonth,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'HourOfTheMonth', @HourOfTheMonth,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'MinuteOfTheMonth', @MinuteOfTheMonth,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'SecondOfTheMonth', @SecondOfTheMonth,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'MilliSecondOfTheMonth', @MilliSecondOfTheMonth,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'DayOfTheWeek', @DayOfTheWeek,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'HourOfTheWeek', @HourOfTheWeek,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'MinuteOfTheWeek', @MinuteOfTheWeek,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'SecondOfTheWeek', @SecondOfTheWeek,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'MilliSecondOfTheWeek', @MilliSecondOfTheWeek,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'HourOfTheDay', @HourOfTheDay,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'MinuteOfTheDay', @MinuteOfTheDay,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'SecondOfTheDay', @SecondOfTheDay,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'MilliSecondOfTheDay', @MilliSecondOfTheDay,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'MinuteOfTheHour', @MinuteOfTheHour,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'SecondOfTheHour', @SecondOfTheHour,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'MilliSecondOfTheHour', @MilliSecondOfTheHour,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'SecondOfTheMinute', @SecondOfTheMinute,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'MilliSecondOfTheMinute', @MilliSecondOfTheMinute,
    'function(const AValue: TDateTime): LongWord');
  TSepiMethod.Create(Result, 'MilliSecondOfTheSecond', @MilliSecondOfTheSecond,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'WithinPastYears', @WithinPastYears,
    'function(const ANow, AThen: TDateTime; const AYears: Integer ) : Boolean');
  TSepiMethod.Create(Result, 'WithinPastMonths', @WithinPastMonths,
    'function(const ANow, AThen: TDateTime; const AMonths: Integer ) : Boolean');
  TSepiMethod.Create(Result, 'WithinPastWeeks', @WithinPastWeeks,
    'function(const ANow, AThen: TDateTime; const AWeeks: Integer ) : Boolean');
  TSepiMethod.Create(Result, 'WithinPastDays', @WithinPastDays,
    'function(const ANow, AThen: TDateTime; const ADays: Integer ) : Boolean');
  TSepiMethod.Create(Result, 'WithinPastHours', @WithinPastHours,
    'function(const ANow, AThen: TDateTime; const AHours: Int64 ) : Boolean');
  TSepiMethod.Create(Result, 'WithinPastMinutes', @WithinPastMinutes,
    'function(const ANow, AThen: TDateTime; const AMinutes: Int64 ) : Boolean');
  TSepiMethod.Create(Result, 'WithinPastSeconds', @WithinPastSeconds,
    'function(const ANow, AThen: TDateTime; const ASeconds: Int64 ) : Boolean');
  TSepiMethod.Create(Result, 'WithinPastMilliSeconds', @WithinPastMilliSeconds,
    'function(const ANow, AThen: TDateTime; const AMilliSeconds: Int64 ) : Boolean');
  TSepiMethod.Create(Result, 'YearsBetween', @YearsBetween,
    'function(const ANow, AThen: TDateTime): Integer');
  TSepiMethod.Create(Result, 'MonthsBetween', @MonthsBetween,
    'function(const ANow, AThen: TDateTime): Integer');
  TSepiMethod.Create(Result, 'WeeksBetween', @WeeksBetween,
    'function(const ANow, AThen: TDateTime): Integer');
  TSepiMethod.Create(Result, 'DaysBetween', @DaysBetween,
    'function(const ANow, AThen: TDateTime): Integer');
  TSepiMethod.Create(Result, 'HoursBetween', @HoursBetween,
    'function(const ANow, AThen: TDateTime): Int64');
  TSepiMethod.Create(Result, 'MinutesBetween', @MinutesBetween,
    'function(const ANow, AThen: TDateTime): Int64');
  TSepiMethod.Create(Result, 'SecondsBetween', @SecondsBetween,
    'function(const ANow, AThen: TDateTime): Int64');
  TSepiMethod.Create(Result, 'MilliSecondsBetween', @MilliSecondsBetween,
    'function(const ANow, AThen: TDateTime): Int64');
  TSepiMethod.Create(Result, 'YearSpan', @YearSpan,
    'function(const ANow, AThen: TDateTime): Double');
  TSepiMethod.Create(Result, 'MonthSpan', @MonthSpan,
    'function(const ANow, AThen: TDateTime): Double');
  TSepiMethod.Create(Result, 'WeekSpan', @WeekSpan,
    'function(const ANow, AThen: TDateTime): Double');
  TSepiMethod.Create(Result, 'DaySpan', @DaySpan,
    'function(const ANow, AThen: TDateTime): Double');
  TSepiMethod.Create(Result, 'HourSpan', @HourSpan,
    'function(const ANow, AThen: TDateTime): Double');
  TSepiMethod.Create(Result, 'MinuteSpan', @MinuteSpan,
    'function(const ANow, AThen: TDateTime): Double');
  TSepiMethod.Create(Result, 'SecondSpan', @SecondSpan,
    'function(const ANow, AThen: TDateTime): Double');
  TSepiMethod.Create(Result, 'MilliSecondSpan', @MilliSecondSpan,
    'function(const ANow, AThen: TDateTime): Double');
  TSepiMethod.Create(Result, 'IncYear', @IncYear,
    'function(const AValue: TDateTime; const ANumberOfYears: Integer = 1 ) : TDateTime');
  TSepiMethod.Create(Result, 'IncWeek', @IncWeek,
    'function(const AValue: TDateTime; const ANumberOfWeeks: Integer = 1 ) : TDateTime');
  TSepiMethod.Create(Result, 'IncDay', @IncDay,
    'function(const AValue: TDateTime; const ANumberOfDays: Integer = 1 ) : TDateTime');
  TSepiMethod.Create(Result, 'IncHour', @IncHour,
    'function(const AValue: TDateTime; const ANumberOfHours: Int64 = 1 ) : TDateTime');
  TSepiMethod.Create(Result, 'IncMinute', @IncMinute,
    'function(const AValue: TDateTime; const ANumberOfMinutes: Int64 = 1 ) : TDateTime');
  TSepiMethod.Create(Result, 'IncSecond', @IncSecond,
    'function(const AValue: TDateTime; const ANumberOfSeconds: Int64 = 1 ) : TDateTime');
  TSepiMethod.Create(Result, 'IncMilliSecond', @IncMilliSecond,
    'function(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64 = 1 ) : TDateTime');
  TSepiMethod.Create(Result, 'EncodeDateTime', @EncodeDateTime,
    'function(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond : Word ) : TDateTime');
  TSepiMethod.Create(Result, 'DecodeDateTime', @DecodeDateTime,
    'procedure(const AValue: TDateTime; out AYear, AMonth, ADay, AHour , AMinute , ASecond , AMilliSecond : Word )');
  TSepiMethod.Create(Result, 'EncodeDateWeek', @EncodeDateWeek,
    'function(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word = 1 ) : TDateTime');
  TSepiMethod.Create(Result, 'DecodeDateWeek', @DecodeDateWeek,
    'procedure(const AValue: TDateTime; out AYear, AWeekOfYear , ADayOfWeek : Word )');
  TSepiMethod.Create(Result, 'EncodeDateDay', @EncodeDateDay,
    'function(const AYear, ADayOfYear: Word): TDateTime');
  TSepiMethod.Create(Result, 'DecodeDateDay', @DecodeDateDay,
    'procedure(const AValue: TDateTime; out AYear, ADayOfYear: Word)');
  TSepiMethod.Create(Result, 'EncodeDateMonthWeek', @EncodeDateMonthWeek,
    'function(const AYear, AMonth, AWeekOfMonth, ADayOfWeek : Word ) : TDateTime');
  TSepiMethod.Create(Result, 'DecodeDateMonthWeek', @DecodeDateMonthWeek,
    'procedure(const AValue: TDateTime; out AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word )');
  TSepiMethod.Create(Result, 'TryEncodeDateTime', @TryEncodeDateTime,
    'function(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond : Word ; out AValue: TDateTime ) : Boolean');
  TSepiMethod.Create(Result, 'TryEncodeDateWeek', @TryEncodeDateWeek,
    'function(const AYear, AWeekOfYear: Word; out AValue: TDateTime ; const ADayOfWeek: Word = 1 ) : Boolean');
  TSepiMethod.Create(Result, 'TryEncodeDateDay', @TryEncodeDateDay,
    'function(const AYear, ADayOfYear: Word; out AValue: TDateTime ) : Boolean');
  TSepiMethod.Create(Result, 'TryEncodeDateMonthWeek', @TryEncodeDateMonthWeek,
    'function(const AYear, AMonth, AWeekOfMonth, ADayOfWeek : Word ; var AValue: TDateTime ) : Boolean');
  TSepiMethod.Create(Result, 'RecodeYear', @RecodeYear,
    'function(const AValue: TDateTime; const AYear: Word): TDateTime');
  TSepiMethod.Create(Result, 'RecodeMonth', @RecodeMonth,
    'function(const AValue: TDateTime; const AMonth: Word): TDateTime');
  TSepiMethod.Create(Result, 'RecodeDay', @RecodeDay,
    'function(const AValue: TDateTime; const ADay: Word): TDateTime');
  TSepiMethod.Create(Result, 'RecodeHour', @RecodeHour,
    'function(const AValue: TDateTime; const AHour: Word): TDateTime');
  TSepiMethod.Create(Result, 'RecodeMinute', @RecodeMinute,
    'function(const AValue: TDateTime; const AMinute: Word): TDateTime');
  TSepiMethod.Create(Result, 'RecodeSecond', @RecodeSecond,
    'function(const AValue: TDateTime; const ASecond: Word): TDateTime');
  TSepiMethod.Create(Result, 'RecodeMilliSecond', @RecodeMilliSecond,
    'function(const AValue: TDateTime; const AMilliSecond: Word ) : TDateTime');
  TSepiMethod.Create(Result, 'RecodeDate', @RecodeDate,
    'function(const AValue: TDateTime; const AYear, AMonth, ADay : Word ) : TDateTime');
  TSepiMethod.Create(Result, 'RecodeTime', @RecodeTime,
    'function(const AValue: TDateTime; const AHour, AMinute, ASecond, AMilliSecond : Word ) : TDateTime');
  TSepiMethod.Create(Result, 'RecodeDateTime', @RecodeDateTime,
    'function(const AValue: TDateTime; const AYear, AMonth, ADay, AHour , AMinute , ASecond , AMilliSecond : Word ) : TDateTime');
  TSepiMethod.Create(Result, 'TryRecodeDateTime', @TryRecodeDateTime,
    'function(const AValue: TDateTime; const AYear, AMonth, ADay, AHour , AMinute , ASecond , AMilliSecond : Word ; out AResult: TDateTime ) : Boolean');
  TSepiMethod.Create(Result, 'CompareDateTime', @CompareDateTime,
    'function(const A, B: TDateTime): TValueRelationship');
  TSepiMethod.Create(Result, 'SameDateTime', @SameDateTime,
    'function(const A, B: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'CompareDate', @CompareDate,
    'function(const A, B: TDateTime): TValueRelationship');
  TSepiMethod.Create(Result, 'SameDate', @SameDate,
    'function(const A, B: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'CompareTime', @CompareTime,
    'function(const A, B: TDateTime): TValueRelationship');
  TSepiMethod.Create(Result, 'SameTime', @SameTime,
    'function(const A, B: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'NthDayOfWeek', @NthDayOfWeek,
    'function(const AValue: TDateTime): Word');
  TSepiMethod.Create(Result, 'DecodeDayOfWeekInMonth', @DecodeDayOfWeekInMonth,
    'procedure(const AValue: TDateTime; out AYear, AMonth, ANthDayOfWeek , ADayOfWeek : Word )');
  TSepiMethod.Create(Result, 'EncodeDayOfWeekInMonth', @EncodeDayOfWeekInMonth,
    'function(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek : Word ) : TDateTime');
  TSepiMethod.Create(Result, 'TryEncodeDayOfWeekInMonth',
    @TryEncodeDayOfWeekInMonth,
    'function(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek : Word ; out AValue: TDateTime ) : Boolean');
  TSepiMethod.Create(Result, 'InvalidDateTimeError', @InvalidDateTimeError,
    'procedure(const AYear, AMonth, ADay, AHour, AMinute, ASecond , AMilliSecond : Word ; const ABaseDate: TDateTime = 0 )');
  TSepiMethod.Create(Result, 'InvalidDateWeekError', @InvalidDateWeekError,
    'procedure(const AYear, AWeekOfYear, ADayOfWeek: Word)');
  TSepiMethod.Create(Result, 'InvalidDateDayError', @InvalidDateDayError,
    'procedure(const AYear, ADayOfYear: Word)');
  TSepiMethod.Create(Result, 'InvalidDateMonthWeekError',
    @InvalidDateMonthWeekError,
    'procedure(const AYear, AMonth, AWeekOfMonth, ADayOfWeek : Word )');
  TSepiMethod.Create(Result, 'InvalidDayOfWeekInMonthError',
    @InvalidDayOfWeekInMonthError,
    'procedure(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek : Word )');
  TSepiMethod.Create(Result, 'DateTimeToJulianDate', @DateTimeToJulianDate,
    'function(const AValue: TDateTime): Double');
  TSepiMethod.Create(Result, 'JulianDateToDateTime', @JulianDateToDateTime,
    'function(const AValue: Double): TDateTime');
  TSepiMethod.Create(Result, 'TryJulianDateToDateTime',
    @TryJulianDateToDateTime,
    'function(const AValue: Double; out ADateTime: TDateTime ) : Boolean');
  TSepiMethod.Create(Result, 'DateTimeToModifiedJulianDate',
    @DateTimeToModifiedJulianDate,
    'function(const AValue: TDateTime): Double');
  TSepiMethod.Create(Result, 'ModifiedJulianDateToDateTime',
    @ModifiedJulianDateToDateTime,
    'function(const AValue: Double): TDateTime');
  TSepiMethod.Create(Result, 'TryModifiedJulianDateToDateTime',
    @TryModifiedJulianDateToDateTime,
    'function(const AValue: Double; out ADateTime: TDateTime ) : Boolean');
  TSepiMethod.Create(Result, 'DateTimeToUnix', @DateTimeToUnix,
    'function(const AValue: TDateTime): Int64');
  TSepiMethod.Create(Result, 'UnixToDateTime', @UnixToDateTime,
    'function(const AValue: Int64): TDateTime');

  // Constants
  TSepiConstant.Create(Result, 'DaysPerWeek', DaysPerWeek);
  TSepiConstant.Create(Result, 'WeeksPerFortnight', WeeksPerFortnight);
  TSepiConstant.Create(Result, 'MonthsPerYear', MonthsPerYear);
  TSepiConstant.Create(Result, 'YearsPerDecade', YearsPerDecade);
  TSepiConstant.Create(Result, 'YearsPerCentury', YearsPerCentury);
  TSepiConstant.Create(Result, 'YearsPerMillennium', YearsPerMillennium);
  TSepiConstant.Create(Result, 'DayMonday', DayMonday);
  TSepiConstant.Create(Result, 'DayTuesday', DayTuesday);
  TSepiConstant.Create(Result, 'DayWednesday', DayWednesday);
  TSepiConstant.Create(Result, 'DayThursday', DayThursday);
  TSepiConstant.Create(Result, 'DayFriday', DayFriday);
  TSepiConstant.Create(Result, 'DaySaturday', DaySaturday);
  TSepiConstant.Create(Result, 'DaySunday', DaySunday);
  TSepiConstant.Create(Result, 'OneHour', OneHour);
  TSepiConstant.Create(Result, 'OneMinute', OneMinute);
  TSepiConstant.Create(Result, 'OneSecond', OneSecond);
  TSepiConstant.Create(Result, 'OneMillisecond', OneMillisecond);
  TSepiArrayType.Create(Result, '$1',
    [Integer(Low(Boolean)), Integer(High(Boolean))], TypeInfo(Word), True);
  TSepiVariable.Create(Result, 'DaysPerYear',
    DaysPerYear, '$1', True);
  TSepiConstant.Create(Result, 'RecodeLeaveFieldAsIs', RecodeLeaveFieldAsIs);

  // Global variables
  TSepiVariable.Create(Result, 'ApproxDaysPerMonth',
    ApproxDaysPerMonth, TypeInfo(Double));
  TSepiVariable.Create(Result, 'ApproxDaysPerYear',
    ApproxDaysPerYear, TypeInfo(Double));

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('DateUtils', ImportUnit);
end.

