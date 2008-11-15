{-------------------------------------------------------------------------------
SepiDelphiCompiler - Example program for Sepi
As an example program, SepiDelphiCompiler is free of any usage. It is released
in the public domain.
-------------------------------------------------------------------------------}


{*
  Options de compilation
  @author sjrd
  @version 1.0
*}
unit CompilerOptions;

interface

uses
  SysUtils, Classes, ScUtils, ScConsoleUtils, SDCConsts;

type
  {*
    Compiler coptions
    @author sjrd
    @version 1.0
  *}
  TOptions = class(TCommandLine)
  private
    FFileNames: TStrings;       /// Noms des fichiers à traiter
    FWaitWhenFinished: Boolean; /// Indique s'il faut attendre quand terminé

    FSepiBrowsingPath: string;   /// Chemin de recherche Sepi
    FSourceBrowsingPath: string; /// Chemin de recherche des sources

    FOutputDir: TFileName; /// Dossier de destination des unités compilées
  public
    constructor Create;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    property FileNames: TStrings read FFileNames;
    property WaitWhenFinished: Boolean read FWaitWhenFinished;

    property SepiBrowsingPath: string read FSepiBrowsingPath;
    property SourceBrowsingPath: string read FSourceBrowsingPath;

    property OutputDir: TFileName read FOutputDir;
  end;

implementation

{----------------}
{ TOptions class }
{----------------}

constructor TOptions.Create;
begin
  inherited Create(['-']);

  FFileNames := TStringList.Create;

  // Default values

  FSepiBrowsingPath := '.\';
  FSourceBrowsingPath := '.\';

  FOutputDir := Dir+DefaultOutputDir;

  // Create options

  AddOption(TCommandLineSwitch.Create('wait', ['w'], @FWaitWhenFinished));

  AddOption(TStringOption.Create('sepi-path', ['p'], @FSepiBrowsingPath,
    True, nil, True));
  AddOption(TStringOption.Create('source-path', ['s'], @FSourceBrowsingPath,
    True, nil, True));

  AddOption(TStringOption.Create('output', ['o'], @FOutputDir,
    True, nil, True));
end;

destructor TOptions.Destroy;
begin
  FFileNames.Free;
  inherited;
end;

procedure TOptions.AfterConstruction;
begin
  inherited;
  Parse(FileNames);

  if FileNames.Count = 0 then
    raise ECommandLineParsingException.Create(SOneOrMoreFileNamesRequired);
end;

end.

