{*
  Importe l'unité StrUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsStrUtils;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, StrUtils;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TUnnamed_1 = set of Char;

{-------------}
{ Unit import }
{-------------}

function RandomFrom_0(const AValues: array of string): string;
begin
  Result := RandomFrom(AValues);
end;

function IfThen_0(AValue: Boolean; const ATrue: string; AFalse: string = '' ) : string;
begin
  Result := IfThen(AValue, ATrue, AFalse);
end;

function LeftStr_0(const AText: AnsiString; const ACount: Integer): AnsiString;
begin
  Result := LeftStr(AText, ACount);
end;

function LeftStr_1(const AText: WideString; const ACount: Integer): WideString;
begin
  Result := LeftStr(AText, ACount);
end;

function RightStr_0(const AText: AnsiString; const ACount: Integer): AnsiString;
begin
  Result := RightStr(AText, ACount);
end;

function RightStr_1(const AText: WideString; const ACount: Integer): WideString;
begin
  Result := RightStr(AText, ACount);
end;

function MidStr_0(const AText: AnsiString; const AStart, ACount: Integer): AnsiString;
begin
  Result := MidStr(AText, AStart, ACount);
end;

function MidStr_1(const AText: WideString; const AStart, ACount: Integer): WideString;
begin
  Result := MidStr(AText, AStart, ACount);
end;

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'StrUtils',
    ['SysUtils', 'Types']);

  // Routines
  TSepiMethod.Create(Result, 'ResemblesText', @ResemblesText,
    'function(const AText, AOther: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiResemblesText', @AnsiResemblesText,
    'function(const AText, AOther: string): Boolean');
  TSepiMethod.Create(Result, 'ContainsText', @ContainsText,
    'function(const AText, ASubText: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiContainsText', @AnsiContainsText,
    'function(const AText, ASubText: string): Boolean');
  TSepiMethod.Create(Result, 'StartsText', @StartsText,
    'function(const ASubText, AText: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiStartsText', @AnsiStartsText,
    'function(const ASubText, AText: string): Boolean');
  TSepiMethod.Create(Result, 'EndsText', @EndsText,
    'function(const ASubText, AText: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiEndsText', @AnsiEndsText,
    'function(const ASubText, AText: string): Boolean');
  TSepiMethod.Create(Result, 'ReplaceText', @ReplaceText,
    'function(const AText, AFromText, AToText: string): string');
  TSepiMethod.Create(Result, 'AnsiReplaceText', @AnsiReplaceText,
    'function(const AText, AFromText, AToText: string): string');
  TSepiMethod.Create(Result, 'MatchText', @MatchText,
    'function(const AText: string; const AValues: array of string): Boolean');
  TSepiMethod.Create(Result, 'AnsiMatchText', @AnsiMatchText,
    'function(const AText: string; const AValues: array of string): Boolean');
  TSepiMethod.Create(Result, 'IndexText', @IndexText,
    'function(const AText: string; const AValues: array of string): Integer');
  TSepiMethod.Create(Result, 'AnsiIndexText', @AnsiIndexText,
    'function(const AText: string; const AValues: array of string): Integer');
  TSepiMethod.Create(Result, 'ContainsStr', @ContainsStr,
    'function(const AText, ASubText: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiContainsStr', @AnsiContainsStr,
    'function(const AText, ASubText: string): Boolean');
  TSepiMethod.Create(Result, 'StartsStr', @StartsStr,
    'function(const ASubText, AText: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiStartsStr', @AnsiStartsStr,
    'function(const ASubText, AText: string): Boolean');
  TSepiMethod.Create(Result, 'EndsStr', @EndsStr,
    'function(const ASubText, AText: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiEndsStr', @AnsiEndsStr,
    'function(const ASubText, AText: string): Boolean');
  TSepiMethod.Create(Result, 'ReplaceStr', @ReplaceStr,
    'function(const AText, AFromText, AToText: string): string');
  TSepiMethod.Create(Result, 'AnsiReplaceStr', @AnsiReplaceStr,
    'function(const AText, AFromText, AToText: string): string');
  TSepiMethod.Create(Result, 'MatchStr', @MatchStr,
    'function(const AText: string; const AValues: array of string): Boolean');
  TSepiMethod.Create(Result, 'AnsiMatchStr', @AnsiMatchStr,
    'function(const AText: string; const AValues: array of string): Boolean');
  TSepiMethod.Create(Result, 'IndexStr', @IndexStr,
    'function(const AText: string; const AValues: array of string): Integer');
  TSepiMethod.Create(Result, 'AnsiIndexStr', @AnsiIndexStr,
    'function(const AText: string; const AValues: array of string): Integer');
  TSepiMethod.Create(Result, 'DupeString', @DupeString,
    'function(const AText: string; ACount: Integer): string');
  TSepiMethod.Create(Result, 'ReverseString', @ReverseString,
    'function(const AText: string): string');
  TSepiMethod.Create(Result, 'AnsiReverseString', @AnsiReverseString,
    'function(const AText: AnsiString): AnsiString');
  TSepiMethod.Create(Result, 'StuffString', @StuffString,
    'function(const AText: string; AStart, ALength: Cardinal; const ASubText: string ) : string');
  TSepiOverloadedMethod.Create(Result, 'RandomFrom');
  TSepiMethod.Create(Result, 'OL$RandomFrom$0', @RandomFrom_0,
    'function(const AValues: array of string): string');
  TSepiOverloadedMethod.Create(Result, 'IfThen');
  TSepiMethod.Create(Result, 'OL$IfThen$0', @IfThen_0,
    'function(AValue: Boolean; const ATrue: string; AFalse: string = '''' ) : string');
  TSepiOverloadedMethod.Create(Result, 'LeftStr');
  TSepiMethod.Create(Result, 'OL$LeftStr$0', @LeftStr_0,
    'function(const AText: AnsiString; const ACount: Integer): AnsiString');
  TSepiMethod.Create(Result, 'OL$LeftStr$1', @LeftStr_1,
    'function(const AText: WideString; const ACount: Integer): WideString');
  TSepiOverloadedMethod.Create(Result, 'RightStr');
  TSepiMethod.Create(Result, 'OL$RightStr$0', @RightStr_0,
    'function(const AText: AnsiString; const ACount: Integer): AnsiString');
  TSepiMethod.Create(Result, 'OL$RightStr$1', @RightStr_1,
    'function(const AText: WideString; const ACount: Integer): WideString');
  TSepiOverloadedMethod.Create(Result, 'MidStr');
  TSepiMethod.Create(Result, 'OL$MidStr$0', @MidStr_0,
    'function(const AText: AnsiString; const AStart, ACount: Integer): AnsiString');
  TSepiMethod.Create(Result, 'OL$MidStr$1', @MidStr_1,
    'function(const AText: WideString; const AStart, ACount: Integer): WideString');
  TSepiMethod.Create(Result, 'LeftBStr', @LeftBStr,
    'function(const AText: AnsiString; const AByteCount: Integer): AnsiString');
  TSepiMethod.Create(Result, 'RightBStr', @RightBStr,
    'function(const AText: AnsiString; const AByteCount: Integer): AnsiString');
  TSepiMethod.Create(Result, 'MidBStr', @MidBStr,
    'function(const AText: AnsiString; const AByteStart, AByteCount: Integer): AnsiString');
  TSepiMethod.Create(Result, 'AnsiLeftStr', @AnsiLeftStr,
    'function(const AText: AnsiString; const ACount: Integer): AnsiString');
  TSepiMethod.Create(Result, 'AnsiRightStr', @AnsiRightStr,
    'function(const AText: AnsiString; const ACount: Integer): AnsiString');
  TSepiMethod.Create(Result, 'AnsiMidStr', @AnsiMidStr,
    'function(const AText: AnsiString; const AStart, ACount: Integer): AnsiString');

  // Constants
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_1));
  TSepiVariable.Create(Result, 'WordDelimiters',
     WordDelimiters, TypeInfo(TUnnamed_1), True);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStringSeachOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStringSearchOptions));

  // Routines
  TSepiMethod.Create(Result, 'SearchBuf', @SearchBuf,
    'function(Buf: PChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString: String ; Options: TStringSearchOptions = [soDown] ) : PChar');
  TSepiMethod.Create(Result, 'PosEx', @PosEx,
    'function(const SubStr, S: string; Offset: Integer = 1): Integer');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSoundexLength));

  // Routines
  TSepiMethod.Create(Result, 'Soundex', @Soundex,
    'function(const AText: string; ALength: TSoundexLength = 4): string');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSoundexIntLength));

  // Routines
  TSepiMethod.Create(Result, 'SoundexInt', @SoundexInt,
    'function(const AText: string; ALength: TSoundexIntLength = 4): Integer');
  TSepiMethod.Create(Result, 'DecodeSoundexInt', @DecodeSoundexInt,
    'function(AValue: Integer): string');
  TSepiMethod.Create(Result, 'SoundexWord', @SoundexWord,
    'function(const AText: string): Word');
  TSepiMethod.Create(Result, 'DecodeSoundexWord', @DecodeSoundexWord,
    'function(AValue: Word): string');
  TSepiMethod.Create(Result, 'SoundexSimilar', @SoundexSimilar,
    'function(const AText, AOther: string; ALength: TSoundexLength = 4 ) : Boolean');
  TSepiMethod.Create(Result, 'SoundexCompare', @SoundexCompare,
    'function(const AText, AOther: string; ALength: TSoundexLength = 4 ) : Integer');
  TSepiMethod.Create(Result, 'SoundexProc', @SoundexProc,
    'function(const AText, AOther: string): Boolean');

  // Types
  TSepiMethodRefType.Create(Result, 'TCompareTextProc',
    'function(const AText, AOther: string): Boolean');

  // Global variables
  TSepiVariable.Create(Result, 'ResemblesProc',
    @ResemblesProc, 'TCompareTextProc');
  TSepiVariable.Create(Result, 'AnsiResemblesProc',
    @AnsiResemblesProc, 'TCompareTextProc');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('StrUtils', ImportUnit);
end.

