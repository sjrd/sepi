{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  Sébastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.

Linking this library statically or dynamically with other modules is making a
combined work based on this library.  Thus, the terms and conditions of the GNU
General Public License cover the whole combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.
-------------------------------------------------------------------------------}

{*
  Options de l'importeur
  @author sjrd
  @version 1.0
*}
unit ImporterOptions;

interface

uses
  SysUtils, Classes, ScUtils, ScConsoleUtils, ImporterConsts;

type
  {*
    Options en ligne de commande de l'importeur
    @author sjrd
    @version 1.0
  *}
  TOptions = class(TCommandLine)
  private
    FFileNames: TStrings;       /// Noms des fichiers à traiter
    FWaitWhenFinished: Boolean; /// Indique s'il faut attendre quand terminé

    FBDSVersion: string; /// Version de BDS utilisée (défaut = '3.0')

    FCacheDir: TFileName;     /// Dossier de cache
    FOutputDir: TFileName;    /// Dossier de destination
    FResourcesDir: TFileName; /// Dossier de destination des ressources

    FProduceLazyLoad: Boolean; /// Indique s'il faut produire du code lazy-load
    FExcludeRoutines: Boolean; /// Indique s'il faut exclure les routines
  public
    constructor Create;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    property FileNames: TStrings read FFileNames;
    property WaitWhenFinished: Boolean read FWaitWhenFinished;

    property BDSVersion: string read FBDSVersion;

    property CacheDir: TFileName read FCacheDir;
    property OutputDir: TFileName read FOutputDir;
    property ResourcesDir: TFileName read FResourcesDir;

    property ProduceLazyLoad: Boolean read FProduceLazyLoad;
    property ExcludeRoutines: Boolean read FExcludeRoutines;
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

  FBDSVersion := '5.0';

  FCacheDir := Dir+DefaultCacheDir;
  FOutputDir := Dir+DefaultOutputDir;
  FResourcesDir := Dir+DefaultResourcesDir;

  FProduceLazyLoad := False;

  // Create options

  AddOption(TCommandLineSwitch.Create('wait', ['w'], @FWaitWhenFinished));

  AddOption(TStringOption.Create('bdsversion', ['v'], @FBDSVersion,
    True, nil, True));

  AddOption(TStringOption.Create('cache', [], @FCacheDir, True, nil, True));
  AddOption(TStringOption.Create('output', ['o'], @FOutputDir,
    True, nil, True));
  AddOption(TStringOption.Create('resource', ['r'], @FResourcesDir,
    True, nil, True));

  AddOption(TEnumOption.Create('lazy-load', [], @FProduceLazyLoad,
    TypeInfo(Boolean), '', True, nil, True));

  AddOption(TCommandLineSwitch.Create('exclude-routines', [],
    @FExcludeRoutines));
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

