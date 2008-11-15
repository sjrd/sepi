{-------------------------------------------------------------------------------
SepiRun - Example program for Sepi
As an example program, SepiRun is free of any usage. It is released in the
public domain.
-------------------------------------------------------------------------------}

{*
  Options d'exécution
  @author sjrd
  @version 1.0
*}
unit RunOptions;

interface

uses
  Classes, ScUtils, ScConsoleUtils;

resourcestring
  SOneUnitNameRequired = 'Un et un seul nom d''unité à exécuter requis';

type
  {*
    Options de lancement d'un programme Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiRunOptions = class(TCommandLine)
  private
    FUnitName: string;     /// Unité à exécuter
    FBrowsingPath: string; /// Chemin de recherche
    FUseDebugger: Boolean; /// Indique s'il faut utiliser un debugger

    FWaitWhenFinished: Boolean; /// Indique s'il faut attendre quand terminé
  public
    constructor Create;

    procedure AfterConstruction; override;

    property UnitName: string read FUnitName;
    property BrowsingPath: string read FBrowsingPath;
    property UseDebugger: Boolean read FUseDebugger;

    property WaitWhenFinished: Boolean read FWaitWhenFinished;
  end;

implementation

{-----------------------}
{ TSepiRunOptions class }
{-----------------------}

{*
  Crée les options de lancement et les charge depuis la ligne de commande
*}
constructor TSepiRunOptions.Create;
begin
  inherited Create(['-']);

  FBrowsingPath := Dir;

  AddOption(TStringOption.Create('path', ['p'], @FBrowsingPath,
    True, nil, True));
  AddOption(TCommandLineSwitch.Create('debug', ['d'], @FUseDebugger));

  AddOption(TCommandLineSwitch.Create('wait', ['w'], @FWaitWhenFinished));
end;

{*
  [@inheritDoc]
*}
procedure TSepiRunOptions.AfterConstruction;
var
  Arguments: TStrings;
begin
  inherited;

  Arguments := TStringList.Create;
  try
    Parse(Arguments);

    if Arguments.Count <> 1 then
      raise ECommandLineParsingException.Create(SOneUnitNameRequired);
    FUnitName := Arguments[0];
  finally
    Arguments.Free;
  end;
end;

end.

