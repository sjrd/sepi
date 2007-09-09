{*
  Importe l'unité ScStrUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScStrUtils;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScStrUtils;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ScStrUtils', []);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCompareStrOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCompareStrOptions));

  // Routines
  TSepiMethod.Create(Result, 'RightPos', @RightPos,
    'function(C : Char; const Str : string) : integer');
  TSepiMethod.Create(Result, 'NberSubStr', @NberSubStr,
    'function(const SubStr, Str : string) : integer');
  TSepiMethod.Create(Result, 'NberCharInStr', @NberCharInStr,
    'function(C : Char; const Str : string) : integer');
  TSepiMethod.Create(Result, 'CompareStringEx', @CompareStringEx,
    'function(const S1, S2 : string; CompareOptions : TCompareStrOptions = [] ) : integer');
  TSepiMethod.Create(Result, 'GetFirstToken', @GetFirstToken,
    'function(const S : string; Token : Char) : string');
  TSepiMethod.Create(Result, 'ExtractFirstToken', @ExtractFirstToken,
    'function(var S : string; Token : Char) : string');
  TSepiMethod.Create(Result, 'GetLastToken', @GetLastToken,
    'function(const S : string; Token : Char) : string');
  TSepiMethod.Create(Result, 'ExtractLastToken', @ExtractLastToken,
    'function(var S : string; Token : Char) : string');
  TSepiMethod.Create(Result, 'SplitToken', @SplitToken,
    'function(const S : string; Token : Char; out LeftStr, RightStr : string ) : boolean');
  TSepiMethod.Create(Result, 'GetXToken', @GetXToken,
    'function(const S : string; Token : Char; X : integer) : string');
  TSepiMethod.Create(Result, 'GetXWord', @GetXWord,
    'function(const S : string; X : integer) : string');
  TSepiMethod.Create(Result, 'PosWord', @PosWord,
    'function(const Wrd, Str : string; Index : integer = 1) : integer');
  TSepiMethod.Create(Result, 'HashOfStr', @HashOfStr,
    'function(const Str : string) : Cardinal');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScStrUtils', ImportUnit);
end.

