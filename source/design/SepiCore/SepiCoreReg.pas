{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
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
-------------------------------------------------------------------------------}

{*
  Enregistre les composants de SepiCore dans la palette d'outils de Delphi
  @author sjrd
  @version 1.0
*}
unit SepiCoreReg;

interface

uses
  Classes, SepiAbout;

procedure Register;

implementation

{*
  Enregistre les composants de SepiCore dans la palette d'outils de Delphi
*}
procedure Register;
begin
  RegisterComponents('Sepi', [TSepiAboutDialog]);
end;

end.

