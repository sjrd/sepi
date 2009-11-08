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
  Utilitaires pour les liaisons Sepi-PS
  @author sjrd
  @version 1.0
*}
unit SepiPSUtils;

interface

uses
  SepiMembers, uPSUtils;

function HandleOverloaded(const Name: string;
  out PSName, SecondName: string): Boolean;

const
  /// Tableau de conversion des conventions d'appel Sepi vers Pascal Script
  CallingConvSepiToPS: array[TCallingConvention] of TPSCallingConvention = (
    cdRegister, cdCdecl, cdPascal, cdStdCall, cdSafecall
  );

implementation

uses
  Windows, SysUtils, StrUtils, ScStrUtils;

{*
  Gère le cas des noms de méthodes surchargées
  @param Name         Nom Sepi de la méthode
  @param PSName       Nom Pascal Script de la méthode
  @param SecondName   Variante Pascal Script (seulement si renvoie True)
  @return True si SecondName doit être utilisé, False sinon
*}
function HandleOverloaded(const Name: string;
  out PSName, SecondName: string): Boolean;
var
  StrOverloadIndex: string;
begin
  if AnsiStartsText('OL$', Name) then
  begin
    PSName := Copy(Name, 4, MaxInt);
    SplitToken(PSName, '$', SecondName, StrOverloadIndex);
    PSName[Pos('$', PSName)] := '_';
    Result := StrToInt(StrOverloadIndex) = 0;
  end else
  begin
    PSName := Name;
    Result := False;
  end;
end;

end.

