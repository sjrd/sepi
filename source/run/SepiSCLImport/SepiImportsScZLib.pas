{*
  Importe l'unité ScZLib dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScZLib;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScZLib;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ScZLib',
    ['Classes', 'ZLib']);

  // Constants
  TSepiConstant.Create(Result, 'clNoComp', clNoComp);
  TSepiConstant.Create(Result, 'clFastestComp', clFastestComp);
  TSepiConstant.Create(Result, 'clDefaultComp', clDefaultComp);
  TSepiConstant.Create(Result, 'clMaxComp', clMaxComp);

  // Routines
  TSepiMethod.Create(Result, 'CompressStream', @CompressStream,
    'procedure(Stream : TStream; Dest : TStream = nil; CompressionLevel : TCompressionLevel = clDefaultComp )');
  TSepiMethod.Create(Result, 'DecompressStream', @DecompressStream,
    'procedure(Stream : TStream; Dest : TStream = nil)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScZLib', ImportUnit);
end.

