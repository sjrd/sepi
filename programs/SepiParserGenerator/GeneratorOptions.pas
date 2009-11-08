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

unit GeneratorOptions;

interface

uses
  SysUtils, ScConsoleUtils;

type
  {*
    Options du générateur
    @author sjrd
    @version 1.0
  *}
  TGeneratorOptions = class(TCommandLine)
  private
    FSourceFileName: TFileName; /// Nom du fichier source
    FDestDir: TFileName;        /// Nom du dossier de destination
    FLanguageName: string;      /// Nom du langage à compiler
  public
    constructor Create;

    procedure AfterConstruction; override;

    property SourceFileName: TFileName read FSourceFileName;
    property DestDir: TFileName read FDestDir;
    property LanguageName: string read FLanguageName;
  end;

implementation

{*
  Crée les options du générateur
*}
constructor TGeneratorOptions.Create;
var
  Option: TCommandLineOption;
begin
  inherited Create(['-']);

  Option := TStringOption.Create('source', ['s'], @FSourceFileName);
  AddOption(Option);
  AddOption(TOptionAlias.Create('input', ['i'], Option));

  Option := TStringOption.Create('destination', ['d'], @FDestDir);
  AddOption(Option);
  AddOption(TOptionAlias.Create('output', ['o'], Option));
  AddOption(TOptionAlias.Create('dest', [], Option));

  Option := TStringOption.Create('language', ['l'], @FLanguageName);
  AddOption(Option);
end;

{*
  [@inheritDoc]
*}
procedure TGeneratorOptions.AfterConstruction;
begin
  inherited;

  Parse;

  FDestDir := IncludeTrailingPathDelimiter(FDestDir);
end;

end.

