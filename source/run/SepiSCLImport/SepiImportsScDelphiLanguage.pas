{*
  Importe l'unité ScDelphiLanguage dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsScDelphiLanguage;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ScDelphiLanguage;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScDelphiLanguage',
    ['SysUtils', 'TypInfo', 'ScUtils']);

  // Routines
  TSepiMetaMethod.Create(Result, 'CorrectIdentifier', @CorrectIdentifier,
    'function(const Ident : string) : boolean');
  TSepiMetaMethod.Create(Result, 'SetBit', @SetBit,
    'procedure(var Value : integer; const Bit : Byte)');
  TSepiMetaMethod.Create(Result, 'ClearBit', @ClearBit,
    'procedure(var Value : integer; const Bit : Byte)');
  TSepiMetaMethod.Create(Result, 'ToggleBit', @ToggleBit,
    'procedure(var Value : integer; const Bit : Byte)');
  TSepiMetaMethod.Create(Result, 'TestBit', @TestBit,
    'function(const Value : integer; const Bit : Byte) : boolean');
  TSepiMetaMethod.Create(Result, 'GetMethodFromName', @GetMethodFromName,
    'function(Obj : TObject; const MethodName : ShortString ) : TMethod');
  TSepiMetaMethod.Create(Result, 'StrToStrRepres', @StrToStrRepres,
    'function(const Str : string; ExcludedChars : TSysCharSet = [] ) : string');
  TSepiMetaMethod.Create(Result, 'StrRepresToStr', @StrRepresToStr,
    'function(Str : string) : string');
  TSepiMetaMethod.Create(Result, 'CharToCharRepres', @CharToCharRepres,
    'function(Chr : Char; ExcludedChars : TSysCharSet = [] ) : string');
  TSepiMetaMethod.Create(Result, 'CharRepresToChar', @CharRepresToChar,
    'function(Str : string) : Char');
  TSepiMetaMethod.Create(Result, 'CharSetToStr', @CharSetToStr,
    'function(const CharSet : TSysCharSet) : string');
  TSepiMetaMethod.Create(Result, 'StrToCharSet', @StrToCharSet,
    'function(Str : string) : TSysCharSet');
  TSepiMetaMethod.Create(Result, 'EnumSetToStr', @EnumSetToStr,
    'function(const EnumSet; TypeInfo : PTypeInfo) : string');
  TSepiMetaMethod.Create(Result, 'StrToEnumSet', @StrToEnumSet,
    'procedure(const Str : string; TypeInfo : PTypeInfo; out EnumSet)');
  TSepiMetaMethod.Create(Result, 'ExplicitInitialize', @ExplicitInitialize,
    'procedure(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1 )');
  TSepiMetaMethod.Create(Result, 'ExplicitFinalize', @ExplicitFinalize,
    'procedure(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1 )');
  TSepiMetaMethod.Create(Result, 'SkipPackedShortString', @SkipPackedShortString,
    'function(Value : PShortstring) : Pointer');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScDelphiLanguage', ImportUnit);
end.

