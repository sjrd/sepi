{*
  Importe l'unité ScWindows dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsScWindows;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ScWindows;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScWindows',
    ['Windows', 'SysUtils', 'Classes']);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSoundType));

  // Routines
  TSepiMetaMethod.Create(Result, 'CreateShellLink', @CreateShellLink,
    'procedure(const Source, Dest : string; const Description : string = '''' ; const IconLocation : string = '''' ; IconIndex : integer = 0 ; const Arguments : string = '''' ; const WorkDir : string = '''' ; ShowCommand : integer = SW_SHOW )');
  TSepiMetaMethod.Create(Result, 'ExecuteSound', @ExecuteSound,
    'function(const Sound : string; SoundType : TSoundType = stFileName; Synchronous : boolean = False ; Module : HMODULE = 0 ; AddFlags : LongWord = 0 ) : boolean');
  TSepiMetaMethod.Create(Result, 'BeginUpdateRes', @BeginUpdateRes,
    'function(const FileName : string) : integer');
  TSepiMetaMethod.Create(Result, 'AddResource', @AddResource,
    'procedure(ResHandle : integer; const ResName : string; Resource : TStream ; const ResType : string = ''RCDATA'' )');
  TSepiMetaMethod.Create(Result, 'DelResource', @DelResource,
    'procedure(ResHandle : integer; const ResName : string)');
  TSepiMetaMethod.Create(Result, 'EndUpdateRes', @EndUpdateRes,
    'procedure(ResHandle : integer; Cancel : boolean = False)');
  TSepiMetaMethod.Create(Result, 'AddResToFile', @AddResToFile,
    'procedure(const FileName, ResName : string; Resource : TStream; const ResType : string = ''RCDATA'' )');
  TSepiMetaMethod.Create(Result, 'DelResInFile', @DelResInFile,
    'procedure(const FileName, ResName : string)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScWindows', ImportUnit);
end.

