{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of the SCL (Sepi Code Library), which is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.

Linking this library -the SCL- statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and conditions
of the GNU General Public License cover the whole combination.

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
  Classes d'énumérateurs fonctionnant en coroutine
  @author sjrd
  @version 1.0
*}
unit ScEnumerators;

interface

uses
  ScCoroutines;

type
  TIntegerEnumerator = class;

  /// Méthode d'énumérateur d'entier
  TIntegerEnumeratorMethod = procedure(
    Enumerator: TIntegerEnumerator) of object;

  {*
    Enumérateur d'entiers protégé
    @author sjrd
    @version 1.0
  *}
  TCustomIntegerEnumerator = class(TCustomEnumerator)
  private
    FCurrent: Integer; /// Valeur courante
  protected
    procedure Yield(Value: Integer);
  public
    function GetCurrent: Integer;

    property Current: Integer read GetCurrent;
  end;

  {*
    Enumérateur d'entiers
    @author sjrd
    @version 1.0
  *}
  TIntegerEnumerator = class(TCustomIntegerEnumerator)
  private
    FEnumerateMethod: TIntegerEnumeratorMethod; /// Méthode exécutante
  protected
    procedure Execute; override;
  public
    constructor Create(AEnumerateMethod: TIntegerEnumeratorMethod;
      ALoop: TCoroutineLoop = clNoLoop);

    procedure Yield(Value: Integer);

    property Terminating;
  end;

implementation

{--------------------------}
{ TCustomIntegerEnumerator }
{--------------------------}

{*
  Renvoie une valeur
  @param Value   Valeur à renvoyer
*}
procedure TCustomIntegerEnumerator.Yield(Value: Integer);
begin
  FCurrent := Value;
  inherited Yield;
end;

{*
  Valeur courante
  @return Valeur courante
*}
function TCustomIntegerEnumerator.GetCurrent: Integer;
begin
  Result := FCurrent;
end;

{--------------------}
{ TIntegerEnumerator }
{--------------------}

{*
  Crée une coroutine
  @param AEnumerateMethod   Méthode contenu de la coroutine
  @param ALoop              Type de boucle de la coroutine (défaut : clNoLoop)
*}
constructor TIntegerEnumerator.Create(
  AEnumerateMethod: TIntegerEnumeratorMethod;
  ALoop: TCoroutineLoop = clNoLoop);
begin
  inherited Create(ALoop);
  FEnumerateMethod := AEnumerateMethod;
end;

{*
  [@inheritDoc]
*}
procedure TIntegerEnumerator.Execute;
begin
  FEnumerateMethod(Self);
end;

{*
  [@inheritDoc]
*}
procedure TIntegerEnumerator.Yield(Value: Integer);
begin
  inherited;
end;

end.

