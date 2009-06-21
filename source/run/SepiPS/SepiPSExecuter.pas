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
  Exécuteur d'unité Sepi-PS
  @author sjrd
  @version 1.0
*}
unit SepiPSExecuter;

interface

procedure SepiPSExecute(const UnitName: string);

implementation

uses
  SepiReflectionCore, SepiMembers, SepiPSLoader;

{*
  Exécute un script Sepi-PS
  @param UnitName   Nom de l'unité à charger pour exécution
*}
procedure SepiPSExecute(const UnitName: string);
var
  Root: TSepiRoot;
  Method: TMethod;
  SepiUnit: TSepiUnit;
  SepiMainProc: TSepiMethod;
  MainProc: procedure;
begin
  Root := TSepiRoot.Create;
  try
    Method.Code := @SepiPSLoadUnit;
    Method.Data := nil;
    Root.OnLoadUnit := TSepiLoadUnitEvent(Method);

    SepiUnit := Root.LoadUnit(UnitName);
    SepiMainProc := SepiUnit.GetComponent('$MAIN') as TSepiMethod;

    if SepiMainProc = nil then
      WriteLn('There is no main proc in this unit')
    else
    begin
      @MainProc := SepiMainProc.Code;
      MainProc;
    end;
  finally
    Root.Free;
  end;
end;

end.

