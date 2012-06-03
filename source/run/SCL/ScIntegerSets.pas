{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  S�bastien Doeraene
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
  Ensemble d'entiers
  @author sjrd
  @version 1.0
*}
unit ScIntegerSets;
{$i ..\..\source\Sepi.inc}
interface

uses
  Types, SysUtils, Classes;

type
  TScIntegerSet = class;

  {*
    Intervalle ferm� d'entiers
    @author sjrd
    @version 1.0
  *}
  TIntegerInterval = record
    Lower: Integer;  /// Borne inf�rieure
    Higher: Integer; /// Borne sup�rieure
  end;

  {*
    Tableau dynamique d'intervalles ferm�s d'entiers
  *}
  TIntegerIntervalDynArray = array of TIntegerInterval;

  {*
    �num�rateur des valeurs contenues dans un ensemble d'entiers
    @author sjrd
    @version 1.0
  *}
  TScIntegerSetEnumerator = class(TObject)
  private
    FIntegerSet: TScIntegerSet; /// Ensemble d'entiers � �num�rer
    FCurrentInterval: Integer;  /// Intervalle courante
    FCurrent: Integer;          /// Valeur courante
  public
    constructor Create(AIntegerSet: TScIntegerSet);

    function MoveNext: Boolean;
    function GetCurrent: Integer;

    property Current: Integer read FCurrent;
  end;

  {*
    Ensemble d'entiers
    L'impl�mentation favorise des intervalles de valeurs, plut�t que des valeurs
    isol�es. Mais celles-ci ne sont que faiblement p�nalis�es.
    @author sjrd
    @version 1.0
  *}
  TScIntegerSet = class(TObject)
  private
    FIntervals: TIntegerIntervalDynArray; /// Intervalles de valeurs contenues
    FIntervalCount: Integer;              /// Nombre d'intervalles

    FModified: Boolean; /// Indique si a �t� modifi�

    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetIntervals(Index: Integer): TIntegerInterval;
    function GetIsEmpty: Boolean;
  protected
    procedure Grow(MinCount: Integer = 0);
    function FindInterval(Value: Integer; out Index: Integer;
      First: Integer = 0; Last: Integer = -1): Boolean;

    procedure InsertIntervals(Index: Integer; Amount: Integer = 1);
    procedure DeleteIntervals(LowerIndex, UpperIndex: Integer);
  public
    constructor Create; overload;
    constructor Create(const Values: array of Integer); overload;
    constructor Clone(Source: TScIntegerSet);
    constructor CreateFromStream(Stream: TStream);

    procedure Assign(Source: TScIntegerSet);

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure Clear;
    procedure AddInterval(Lower, Higher: Integer);
    procedure RemoveInterval(Lower, Higher: Integer);
    procedure Include(Value: Integer);
    procedure Exclude(Value: Integer);

    function Exists(Value: Integer): Boolean;
    function GetEnumerator: TScIntegerSetEnumerator;

    procedure Union(Values: TScIntegerSet);
    procedure Subtract(Values: TScIntegerSet);
    procedure Intersect(Values: TScIntegerSet);

    function Equal(Right: TScIntegerSet): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property IntervalCount: Integer read FIntervalCount;
    property Intervals[Index: Integer]: TIntegerInterval read GetIntervals;
    property IsEmpty: Boolean read GetIsEmpty;

    property Modified: Boolean read FModified write FModified;
  end;

function IntInInterval(Value: Integer;
  const Interval: TIntegerInterval): Boolean; inline;

implementation

{-----------------}
{ Global routines }
{-----------------}

function IntInInterval(Value: Integer;
  const Interval: TIntegerInterval): Boolean; inline;
begin
  Result := (Value >= Interval.Lower) and (Value <= Interval.Higher);
end;

{-------------------------------}
{ TScIntegerSetEnumerator class }
{-------------------------------}

{*
  Cr�e l'�num�rateur
  @param AIntegerSet   Ensemble d'entiers � �num�rer
*}
constructor TScIntegerSetEnumerator.Create(AIntegerSet: TScIntegerSet);
begin
  inherited Create;

  FIntegerSet := AIntegerSet;
  FCurrentInterval := -1;
end;

{*
  Avance vers la valeur suivante
  @return True s'il y a une nouvelle valeur, False si l'�num�ration est termin�e
*}
function TScIntegerSetEnumerator.MoveNext: Boolean;
begin
  if FCurrentInterval >= FIntegerSet.FIntervalCount then
    Result := False
  else
  begin
    Inc(FCurrent);

    if (FCurrentInterval < 0) or
      (FCurrent > FIntegerSet.FIntervals[FCurrentInterval].Higher) then
    begin
      Inc(FCurrentInterval);
      Result := FCurrentInterval < FIntegerSet.FIntervalCount;

      if Result then
        FCurrent := FIntegerSet.FIntervals[FCurrentInterval].Lower;
    end else
    begin
      Result := True;
    end;
  end;
end;

{*
  Valeur courante
  @return Valeur courante
*}
function TScIntegerSetEnumerator.GetCurrent: Integer;
begin
  Result := FCurrent;
end;

{---------------------}
{ TScIntegerSet class }
{---------------------}

{*
  Cr�e un nouvel ensemble d'entiers vide
*}
constructor TScIntegerSet.Create;
begin
  inherited Create;

  FIntervalCount := 0;
end;

{*
  Cr�e un nouvel ensemble d'entiers � partir d'un tableau d'entiers
  Chaque valeur pr�sente au moins une fois dans le tableau fera partie de
  l'ensemble.
  @param Values   Tableau de valeurs de type entier
*}
constructor TScIntegerSet.Create(const Values: array of Integer);
var
  I: Integer;
begin
  Create;

  SetLength(FIntervals, Length(Values));
  for I := Low(Values) to High(Values) do
    Include(Values[I]);
  Modified := False;
end;

{*
  Cr�e une copie d'un ensemble d'entiers
  @param Source   Ensemble d'entiers � copier
*}
constructor TScIntegerSet.Clone(Source: TScIntegerSet);
begin
  Create;

  Assign(Source);
  Modified := False;
end;

{*
  Cr�e un nouvel ensemble d'entiers � partir d'un flux
  @param Stream   Flux source
*}
constructor TScIntegerSet.CreateFromStream(Stream: TStream);
begin
  inherited Create;

  LoadFromStream(Stream);
  Modified := False;
end;

{*
  Capacit� du tableau interne
  @return Capacit� du tableau interne
*}
function TScIntegerSet.GetCapacity: Integer;
begin
  Result := Length(FIntervals);
end;

{*
  Modifie la capacit� du tableau interne
  @param Value   Nouvelle capacit�
*}
procedure TScIntegerSet.SetCapacity(Value: Integer);
begin
  SetLength(FIntervals, Value);
  if FIntervalCount > Value then
    FIntervalCount := Value;
end;

{*
  Tableau zero-based des intervalles contenus dans l'ensemble
  @param Index   Index d'un intervalle
  @return Intervalle � l'index sp�cifi�
*}
function TScIntegerSet.GetIntervals(Index: Integer): TIntegerInterval;
begin
  Result := FIntervals[Index];
end;

{*
  Indique si l'ensemble est vide
  @return True si l'ensemble est vide, False sinon
*}
function TScIntegerSet.GetIsEmpty: Boolean;
begin
  Result := FIntervalCount = 0;
end;

{*
  Agrandit le tableau interne
  @param MinCount   Nombre minimum n�cessaire (d�faut = 0)
*}
procedure TScIntegerSet.Grow(MinCount: Integer = 0);
const
  AllocBy = 16;
  DoubleAt = 4*AllocBy;
var
  Len: Integer;
begin
  Len := Length(FIntervals);

  if Len < DoubleAt then
    Inc(Len, AllocBy)
  else
    Len := 2*Len;

  if Len < MinCount then
    SetLength(FIntervals, MinCount)
  else
    SetLength(FIntervals, Len);
end;

{*
  Cherche la place de l'intervalle contenant une valeur dans le tableau interne
  @param Value   Valeur � chercher
  @param Index   Index o� se trouve, ou o� devrait se trouver, l'intervalle
  @param First   Index du premier intervalle � consid�rer
  @param Last    Index du dernier intervalle � consid�rer (-1 = dernier)
  @return True si l'intervalle a �t� trouv�, False sinon
*}
function TScIntegerSet.FindInterval(Value: Integer; out Index: Integer;
  First: Integer = 0; Last: Integer = -1): Boolean;
begin
  if Last < 0 then
    Last := IntervalCount-1;

  while Last >= First do
  begin
    Index := (First+Last) div 2;

    if Value > FIntervals[Index].Higher then
      First := Index+1
    else if Value < FIntervals[Index].Lower then
      Last := Index-1
    else
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
  Index := First;
end;

{*
  Ins�re des intervalles dans le tableau interne
  Le nombre d'intervalles � ins�rer peut �tre n�gatif ; des intervalles sont
  alors retir�s depuis l'intervalle � Index-1 jusque celui � Index-Abs(Amount)
  compris.
  @param Index    Position o� ins�rer des intervalles
  @param Amount   Nombre d'intervalles � ins�rer (1 par d�faut)
*}
procedure TScIntegerSet.InsertIntervals(Index: Integer; Amount: Integer = 1);
var
  NewCount: Integer;
begin
  if Amount = 0 then
    Exit;

  NewCount := IntervalCount + Amount;
  if NewCount > Length(FIntervals) then
    Grow(NewCount);

  Move(FIntervals[Index], FIntervals[Index+Amount],
    (IntervalCount-Index) * SizeOf(TIntegerInterval));

  FIntervalCount := NewCount;
  if IntervalCount <= Length(FIntervals) div 4 then
    SetLength(FIntervals, IntervalCount*2);
end;

{*
  Retire une plage d'intervalles du tableau interne
  @param LowerIndex   Index bas de l'intervalle � retirer (compris)
  @param UpperIndex   Index sup�rieur de l'intervalle � retirer (non compris)
*}
procedure TScIntegerSet.DeleteIntervals(LowerIndex, UpperIndex: Integer);
begin
  InsertIntervals(UpperIndex, LowerIndex-UpperIndex);
end;

{*
  Remplace le contenu de cet ensemble par un autre
  @param Source   Ensemble � recopier
*}
procedure TScIntegerSet.Assign(Source: TScIntegerSet);
begin
  Capacity := Source.IntervalCount;
  FIntervalCount := Source.IntervalCount;
  Move(Source.FIntervals[0], FIntervals[0],
    IntervalCount * SizeOf(TIntegerInterval));

  Modified := True;
end;

{*
  Charge l'ensemble depuis un flux
  @param Stream   Flux source
*}
procedure TScIntegerSet.LoadFromStream(Stream: TStream);
begin
  Stream.WriteBuffer(FIntervalCount, 4);
  Stream.WriteBuffer(FIntervals[0], IntervalCount * SizeOf(TIntegerInterval));

  Modified := True;
end;

{*
  Sauvegarde l'ensemble dans un flux
  @param Stream   Flux destination
*}
procedure TScIntegerSet.SaveToStream(Stream: TStream);
begin
  Stream.ReadBuffer(FIntervalCount, 4);
  SetLength(FIntervals, IntervalCount);
  Stream.ReadBuffer(FIntervals[0], IntervalCount * SizeOf(TIntegerInterval));
end;

{*
  Vide l'ensemble
*}
procedure TScIntegerSet.Clear;
begin
  if FIntervalCount > 0 then
  begin
    SetLength(FIntervals, 0);
    FIntervalCount := 0;
    Modified := True;
  end;
end;

{*
  Inclut toutes les valeurs d'un intervalle (ferm�) dans l'ensemble
  @param Lower    Borne inf�rieure de l'intervalle
  @param Higher   Borne sup�rieure de l'intervalle
*}
procedure TScIntegerSet.AddInterval(Lower, Higher: Integer);
var
  LowerFound, HigherFound: Boolean;
  LowerIndex, HigherIndex, UpperIndex: Integer;
begin
  { What's the difference between HigherIndex and UpperIndex?
    UpperIndex = IIF(HigherFound, HigherIndex+1, HigherIndex)
    UpperIndex is more symetric to LowerIndex than HigherIndex, which
    simplifies greatly the code.  Meanwhile, HigherIndex is more useful when
    accessing the interval itself; it has no meaning when (not HigherFound). }

  // Empty interval - do nothing
  if Lower > Higher then
    Exit;

  // Find indexes for lower and higher intervals

  LowerFound := FindInterval(Lower, LowerIndex);

  if Higher = Lower then
  begin
    if LowerFound then
      Exit;

    HigherFound := False;
    HigherIndex := LowerIndex;
    UpperIndex := HigherIndex;
  end else
  begin
    HigherFound := FindInterval(Higher, HigherIndex);
    UpperIndex := HigherIndex;
    if HigherFound then
      Inc(UpperIndex);

    { If new interval is contained in a single existing interval, there is no
      change to be made. }
    if LowerFound and HigherFound and (LowerIndex = HigherIndex) then
      Exit;
  end;

  // If new interval hardly touches an adjacent one, merge them anyway

  if (not LowerFound) and (LowerIndex > 0) and
    (FIntervals[LowerIndex-1].Higher = Lower-1) then
  begin
    LowerFound := True;
    Dec(LowerIndex);
  end;

  if (not HigherFound) and (HigherIndex < IntervalCount) and
    (FIntervals[UpperIndex].Lower = Higher+1) then
  begin
    HigherFound := True;
    Inc(UpperIndex);
  end;

  // Update Lower and Higher if were found

  if LowerFound then
    Lower := FIntervals[LowerIndex].Lower;
  if HigherFound then
    Higher := FIntervals[HigherIndex].Higher;

  // Update internal array

  DeleteIntervals(LowerIndex+1, UpperIndex);
    // If LowerIndex = UpperIndex, it will insert an interval at UpperIndex
  FIntervals[LowerIndex].Lower := Lower;
  FIntervals[LowerIndex].Higher := Higher;

  Modified := True;
end;

{*
  Exclut toutes les valeurs d'un intervalle (ferm�) de l'ensemble
  @param Lower    Borne inf�rieure de l'intervalle
  @param Higher   Borne sup�rieure de l'intervalle
*}
procedure TScIntegerSet.RemoveInterval(Lower, Higher: Integer);
var
  LowerFound, HigherFound: Boolean;
  LowerIndex, HigherIndex: Integer;
begin
  // Empty interval - do nothing
  if Lower > Higher then
    Exit;

  // Find indexes for lower and higher intervals

  LowerFound := FindInterval(Lower, LowerIndex);

  if Higher = Lower then
  begin
    if not LowerFound then
      Exit;

    HigherFound := True;
    HigherIndex := LowerIndex;
  end else
  begin
    HigherFound := FindInterval(Higher, HigherIndex);

    { If the interval to be removed should be placed between two existing
      intervals, there is no change to be made. }
    if (not LowerFound) and (not HigherFound) and
      (LowerIndex = HigherIndex) then
      Exit;
  end;

  // If Lower equals (lower interval).Lower, treat it as not found
  // (same thing for Higher)

  if LowerFound and (Lower = FIntervals[LowerIndex].Lower) then
    LowerFound := False;

  if HigherFound and (Higher = FIntervals[HigherIndex].Higher) then
  begin
    HigherFound := False;
    Inc(HigherIndex);
  end;

  // Update internal array

  if LowerFound and HigherFound then
  begin
    DeleteIntervals(LowerIndex+1, HigherIndex);
    FIntervals[LowerIndex].Higher := Lower-1;
    FIntervals[LowerIndex+1].Lower := Higher+1;
  end else if LowerFound then // and not HigherFound
  begin
    DeleteIntervals(LowerIndex+1, HigherIndex);
    FIntervals[LowerIndex].Higher := Lower-1;
  end else if HigherFound then // and not LowerFound
  begin
    DeleteIntervals(LowerIndex, HigherIndex);
    FIntervals[LowerIndex].Lower := Higher+1;
  end else // (not LowerFound) and (not HigherFound)
  begin
    DeleteIntervals(LowerIndex, HigherIndex);
  end;

  Modified := True;
end;

{*
  Ajoute une valeur dans l'ensemble, si elle n'y �tait pas encore
  @param Value   Valeur � ajouter
*}
procedure TScIntegerSet.Include(Value: Integer);
begin
  AddInterval(Value, Value);
end;

{*
  Retire une valeur de l'ensemble, si elle y �tait
  @param Value   Valeur � retirer
*}
procedure TScIntegerSet.Exclude(Value: Integer);
begin
  RemoveInterval(Value, Value);
end;

{*
  D�termine si l'ensemble contient une valeur particuli�re
  @param Value   Valeur � tester
  @return True si l'ensemble contient Value, False sinon
*}
function TScIntegerSet.Exists(Value: Integer): Boolean;
var
  Temp: Integer;
begin
  Result := FindInterval(Value, Temp);
end;

{*
  Cr�e un �num�rateur pour les valeurs contenues dans cet ensemble
  @return �num�rateur des valeurs contenues dans cet ensemble
*}
function TScIntegerSet.GetEnumerator: TScIntegerSetEnumerator;
begin
  Result := TScIntegerSetEnumerator.Create(Self);
end;

{*
  Op�rateur + Calcule l'union de deux ensemble
  @param Left    Op�rande de gauche
  @param Right   Op�rande de droite
  @return Union de Left et Right
*}
procedure TScIntegerSet.Union(Values: TScIntegerSet);
var
  I: Integer;
begin
  for I := 0 to Values.IntervalCount-1 do
    AddInterval(Values.FIntervals[I].Lower, Values.FIntervals[I].Higher);
end;

{*
  Op�rateur - Calcule la soustraction de deux ensemble
  @param Left    Op�rande de gauche
  @param Right   Op�rande de droite
  @return Left moins Right, au sens ensembliste du terme
*}
procedure TScIntegerSet.Subtract(Values: TScIntegerSet);
var
  I: Integer;
begin
  for I := 0 to Values.IntervalCount-1 do
    RemoveInterval(Values.FIntervals[I].Lower, Values.FIntervals[I].Higher);
end;

{*
  Op�rateur * Calcule l'intersection entre deux ensemble
  @param Left    Op�rande de gauche
  @param Right   Op�rande de droite
  @return Intersection entre Left et Right
*}
procedure TScIntegerSet.Intersect(Values: TScIntegerSet);
var
  I, Lower, Higher: Integer;
begin
  for I := 0 to Values.IntervalCount do
  begin
    if I = 0 then
      Lower := -MaxInt-1
    else
      Lower := Values.FIntervals[I-1].Higher + 1;

    if I = Values.IntervalCount then
      Higher := MaxInt
    else
      Higher := Values.FIntervals[I].Lower - 1;

    RemoveInterval(Lower, Higher);
  end;
end;

{*
  D�termine si deux ensembles sont �gaux
  @param Right   Op�rande de droite
  @return True si les ensembles sont �gaux, False sinon
*}
function TScIntegerSet.Equal(Right: TScIntegerSet): Boolean;
var
  I: Integer;
begin
  Result := False;

  if IntervalCount <> Right.IntervalCount then
    Exit;

  for I := 0 to IntervalCount-1 do
    if Int64(FIntervals[I]) <> Int64(Right.FIntervals[I]) then
      Exit;

  Result := True;
end;

end.

