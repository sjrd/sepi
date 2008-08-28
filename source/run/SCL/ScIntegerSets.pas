unit ScIntegerSets;

interface

uses
  Types, SysUtils, Classes;

type
  {*
    Intervalle fermé d'entiers
    @author sjrd
    @version 1.0
  *}
  TIntegerInterval = record
    Lower: Integer;  /// Borne inférieure
    Higher: Integer; /// Borne supérieure
  end;

  {*
    Tableau dynamique d'intervalles fermés d'entiers
  *}
  TIntegerIntervalDynArray = array of TIntegerInterval;

  {*
    Ensemble d'entiers
    L'implémentation favorise des intervalles de valeurs, plutôt que des valeurs
    isolées. Mais celles-ci ne sont que faiblement pénalisées.
    @author sjrd
    @version 1.0
  *}
  TScIntegerSet = class(TObject)
  private
    FIntervals: TIntegerIntervalDynArray; /// Intervalles de valeurs contenues
    FIntervalCount: Integer;              /// Nombre d'intervalles

    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetIntervals(Index: Integer): TIntegerInterval;
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

    procedure Union(Values: TScIntegerSet);
    procedure Subtract(Values: TScIntegerSet);
    procedure Intersect(Values: TScIntegerSet);

    function Equal(Right: TScIntegerSet): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property IntervalCount: Integer read FIntervalCount;
    property Intervals[Index: Integer]: TIntegerInterval read GetIntervals;
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

{-------------------}
{ TScIntegerSet class }
{-------------------}

{*
  Crée un nouvel ensemble d'entiers vide
*}
constructor TScIntegerSet.Create;
begin
  inherited Create;

  FIntervalCount := 0;
end;

{*
  Crée un nouvel ensemble d'entiers à partir d'un tableau d'entiers
  Chaque valeur présente au moins une fois dans le tableau fera partie de
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
end;

{*
  Crée une copie d'un ensemble d'entiers
  @param Source   Ensemble d'entiers à copier
*}
constructor TScIntegerSet.Clone(Source: TScIntegerSet);
begin
  Create;

  Assign(Source);
end;

{*
  Crée un nouvel ensemble d'entiers à partir d'un flux
  @param Stream   Flux source
*}
constructor TScIntegerSet.CreateFromStream(Stream: TStream);
begin
  inherited Create;

  LoadFromStream(Stream);
end;

{*
  Capacité du tableau interne
  @return Capacité du tableau interne
*}
function TScIntegerSet.GetCapacity: Integer;
begin
  Result := Length(FIntervals);
end;

{*
  Modifie la capacité du tableau interne
  @param Value   Nouvelle capacité
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
  @return Intervalle à l'index spécifié
*}
function TScIntegerSet.GetIntervals(Index: Integer): TIntegerInterval;
begin
  Result := FIntervals[Index];
end;

{*
  Agrandit le tableau interne
  @param MinCount   Nombre minimum nécessaire (défaut = 0)
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
  @param Value   Valeur à chercher
  @param Index   Index où se trouve, ou où devrait se trouver, l'intervalle
  @param First   Index du premier intervalle à considérer
  @param Last    Index du dernier intervalle à considérer (-1 = dernier)
  @return True si l'intervalle a été trouvé, False sinon
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
  Insère des intervalles dans le tableau interne
  Le nombre d'intervalles à insérer peut être négatif ; des intervalles sont
  alors retirés depuis l'intervalle à Index-1 jusque celui à Index-Abs(Amount)
  compris.
  @param Index    Position où insérer des intervalles
  @param Amount   Nombre d'intervalles à insérer (1 par défaut)
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
  @param LowerIndex   Index bas de l'intervalle à retirer (compris)
  @param UpperIndex   Index supérieur de l'intervalle à retirer (non compris)
*}
procedure TScIntegerSet.DeleteIntervals(LowerIndex, UpperIndex: Integer);
begin
  InsertIntervals(UpperIndex, LowerIndex-UpperIndex);
end;

{*
  Remplace le contenu de cet ensemble par un autre
  @param Source   Ensemble à recopier
*}
procedure TScIntegerSet.Assign(Source: TScIntegerSet);
begin
  Capacity := Source.IntervalCount;
  FIntervalCount := Source.IntervalCount;
  Move(Source.FIntervals[0], FIntervals[0],
    IntervalCount * SizeOf(TIntegerInterval));
end;

{*
  Charge l'ensemble depuis un flux
  @param Stream   Flux source
*}
procedure TScIntegerSet.LoadFromStream(Stream: TStream);
begin
  Stream.WriteBuffer(FIntervalCount, 4);
  Stream.WriteBuffer(FIntervals[0], IntervalCount * SizeOf(TIntegerInterval));
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
  SetLength(FIntervals, 0);
  FIntervalCount := 0;
end;

{*
  Inclut toutes les valeurs d'un intervalle (fermé) dans l'ensemble
  @param Lower    Borne inférieure de l'intervalle
  @param Higher   Borne supérieure de l'intervalle
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
end;

{*
  Exclut toutes les valeurs d'un intervalle (fermé) de l'ensemble
  @param Lower    Borne inférieure de l'intervalle
  @param Higher   Borne supérieure de l'intervalle
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
    HigherFound := FindInterval(Higher, HigherIndex);

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
end;

{*
  Ajoute une valeur dans l'ensemble, si elle n'y était pas encore
  @param Value   Valeur à ajouter
*}
procedure TScIntegerSet.Include(Value: Integer);
begin
  AddInterval(Value, Value);
end;

{*
  Retire une valeur de l'ensemble, si elle y était
  @param Value   Valeur à retirer
*}
procedure TScIntegerSet.Exclude(Value: Integer);
begin
  RemoveInterval(Value, Value);
end;

{*
  Détermine si l'ensemble contient une valeur particulière
  @param Value   Valeur à tester
  @return True si l'ensemble contient Value, False sinon
*}
function TScIntegerSet.Exists(Value: Integer): Boolean;
var
  Temp: Integer;
begin
  Result := FindInterval(Value, Temp);
end;

{*
  Opérateur + Calcule l'union de deux ensemble
  @param Left    Opérande de gauche
  @param Right   Opérande de droite
  @return Union de Left et Right
*}
procedure TScIntegerSet.Union(Values: TScIntegerSet);
var
  Sources: array[0..1] of TIntegerIntervalDynArray;
  Counts, Indices: array[0..1] of Integer;
  Lower, Higher, Cur, Amount: Integer;
begin
  Sources[0] := FIntervals;
  Counts[0] := IntervalCount;
  Sources[1] := Values.FIntervals;
  Counts[1] := Values.IntervalCount;

  FIntervals := nil;
  SetLength(FIntervals, Counts[0] + Counts[1]);
  FIntervalCount := 0;

  Indices[0] := 0;
  Indices[1] := 0;

  { Main loop. It runs while there are intervals left in the two sources.
    At each iteration, it constructs a new interval to be stored in the
    internal array.  To do this, it runs frow the lowest of the lower bounds,
    and then from an interval from left to another from right, until it can't
    go further without going through a "no values land". }

  while (Indices[0] < Counts[0]) and (Indices[1] < Counts[1]) do
  begin
    if Sources[0][Indices[0]].Lower < Sources[1][Indices[1]].Lower then
      Cur := 0
    else
      Cur := 1;

    Lower := Sources[Cur][Indices[Cur]].Lower;

    repeat
      Higher := Sources[Cur][Indices[Cur]].Higher;
      Inc(Indices[Cur]);
      Cur := Cur xor 1; // 0 to 1 and 1 to 0
    until (Indices[Cur] >= Counts[Cur]) or
      (not IntInInterval(Higher+1, Sources[Cur][Indices[Cur]]));

    FIntervals[FIntervalCount].Lower := Lower;
    FIntervals[FIntervalCount].Higher := Higher;
    Inc(FIntervalCount);
  end;

  { After that, there may be intervals left in the left xor right sources.
    Just copy them. }

  if Indices[0] < Counts[0] then
    Cur := 0
  else
    Cur := 1;

  Amount := Counts[Cur] - Indices[Cur];
  Move(Sources[Cur][Indices[Cur]], FIntervals[FIntervalCount],
    Amount * SizeOf(TIntegerInterval));
  Inc(FIntervalCount, Amount);
end;

{*
  Opérateur - Calcule la soustraction de deux ensemble
  @param Left    Opérande de gauche
  @param Right   Opérande de droite
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
  Opérateur * Calcule l'intersection entre deux ensemble
  @param Left    Opérande de gauche
  @param Right   Opérande de droite
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
  Détermine si deux ensembles sont égaux
  @param Right   Opérande de droite
  @return True si les ensembles sont égaux, False sinon
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

