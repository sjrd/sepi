{*
  Importe l'unité ScStrUtils dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsScStrUtils;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ScStrUtils;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScStrUtils', []);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCompareStrOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCompareStrOptions));

  // Routines
  TSepiMetaMethod.Create(Result, 'RightPos', @RightPos,
    'function(C : Char; const Str : string) : integer');
  TSepiMetaMethod.Create(Result, 'NberSubStr', @NberSubStr,
    'function(const SubStr, Str : string) : integer');
  TSepiMetaMethod.Create(Result, 'NberCharInStr', @NberCharInStr,
    'function(C : Char; const Str : string) : integer');
  TSepiMetaMethod.Create(Result, 'CompareStringEx', @CompareStringEx,
    'function(const S1, S2 : string; CompareOptions : TCompareStrOptions = [] ) : integer');
  TSepiMetaMethod.Create(Result, 'GetFirstToken', @GetFirstToken,
    'function(const S : string; Token : Char) : string');
  TSepiMetaMethod.Create(Result, 'ExtractFirstToken', @ExtractFirstToken,
    'function(var S : string; Token : Char) : string');
  TSepiMetaMethod.Create(Result, 'GetLastToken', @GetLastToken,
    'function(const S : string; Token : Char) : string');
  TSepiMetaMethod.Create(Result, 'ExtractLastToken', @ExtractLastToken,
    'function(var S : string; Token : Char) : string');
  TSepiMetaMethod.Create(Result, 'SplitToken', @SplitToken,
    'function(const S : string; Token : Char; out LeftStr, RightStr : string ) : boolean');
  TSepiMetaMethod.Create(Result, 'GetXToken', @GetXToken,
    'function(const S : string; Token : Char; X : integer) : string');
  TSepiMetaMethod.Create(Result, 'GetXWord', @GetXWord,
    'function(const S : string; X : integer) : string');
  TSepiMetaMethod.Create(Result, 'PosWord', @PosWord,
    'function(const Wrd, Str : string; Index : integer = 1) : integer');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScStrUtils', ImportUnit);
end.

