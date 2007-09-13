{*
  Définit quelques routines usuelles
  @author sjrd
  @version 1.0
*}
unit ScUtils;

interface

uses
  Types, SysUtils, Classes;

type
  {*
    Représente un point situé dans un espace en trois dimensions
  *}
  T3DPoint = record
    X: Integer; /// Coordonnée X du point
    Y: Integer; /// Coordonnée Y du point
    Z: Integer; /// Coordonnée Z du point
  end;

  {*
    Ensemble de Byte
  *}
  TSysByteSet = set of Byte;

  {*
    Ensemble de caractères
    Ce type est déprécié, utilisez TSysCharSet à la place.
  *}
  TSetOfChars = TSysCharSet {$IFNDEF DCTD} deprecated {$ENDIF};

  {*
    Ensemble de Byte
    Ce type est déprécié, utilisez TSysByteSet à la place.
  *}
  TSetOfBytes = TSysByteSet {$IFNDEF DCTD} deprecated {$ENDIF};

const
  /// Point nul
  NoPoint: TPoint = (X: MaxInt; Y: MaxInt);
  /// Point 3D nul
  No3DPoint: T3DPoint = (X: MaxInt; Y: MaxInt; Z: MaxInt);
  /// GUID nul
  NoGUID: TGUID = (D1: 0; D2: 0; D3: 0; D4: (0, 0, 0, 0, 0, 0, 0, 0));

function Dir: string;

// Fonctions de If Immédiat
function IIF(Cond: Boolean; Int1, Int2: Integer): Integer; overload;
function IIF(Cond: Boolean; Flo1, Flo2: Double): Double; overload;
function IIF(Cond: Boolean; Chr1, Chr2: Char): Char; overload;
function IIF(Cond: Boolean; const Str1, Str2: string): string; overload;
function IIF(Cond: Boolean; Obj1, Obj2: TObject): TObject; overload;
function IIF(Cond: Boolean; Ptr1, Ptr2: Pointer): Pointer; overload;
function IIF(Cond: Boolean; Var1, Var2: Variant): Variant; overload;

function MinMax(Value, Min, Max: Integer): Integer;

function IntDiv(Op1, Op2: Integer): Integer;
function IntMod(Op1, Op2: Integer): Integer;

function IntToBase(Value: Integer; Base: Byte = 10): string;
function BaseToInt(const Value: string; Base: Byte = 10): Integer;
function BaseToIntDef(const Value: string; Default: Integer = 0;
  Base: Byte = 10): Integer;

function ConvertDoubleToInt64(Value: Double): Int64;
function ConvertInt64ToDouble(Value: Int64): Double;

procedure Wait(Milliseconds: Integer); deprecated;
procedure WaitProcessMessages(Milliseconds: Integer);

function IntToStr0(Value, Digits: Integer): string;

function ReadStrFromStream(Stream: TStream): string;
procedure WriteStrToStream(Stream: TStream; const Str: string);

function CorrectFileName(const FileName: string;
  AcceptPathDelim: Boolean = False;
  AcceptDriveDelim: Boolean = False): Boolean;

function SamePoint(const Point1, Point2: TPoint): Boolean;
function Same3DPoint(const Point1, Point2: T3DPoint): Boolean;

function IsNoPoint(const Point: TPoint): Boolean;
function IsNo3DPoint(const Point3D: T3DPoint): Boolean;

function SameGUID(const GUID1, GUID2: TGUID): Boolean;
function IsNoGUID(const GUID: TGUID): Boolean;

function Point3D(X, Y, Z: Integer): T3DPoint;

function Point3DToString(const Point3D: T3DPoint;
  const Delim: string = ' '): string;

{$IFDEF MSWINDOWS}
procedure RunURL(const URL: string; const Verb: string = 'open');
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, ShellAPI,
{$ENDIF}
  Math, DateUtils, Forms, ScConsts;

const
  NumbersStr = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'; /// Chiffres des bases
  MaxBase = 36;                                        /// Base maximale

{----------------------------------}
{ Procédures et fonctions globales }
{----------------------------------}

{*
  Renvoie le chemin du dossier dans lequel se trouve l'application qui s'exécute
  @return Le chemin du dossier dans lequel se trouve l'application qui s'exécute
*}
function Dir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

{$REGION 'Fonctions de If Immédiat'}

{*
  Fonction de If Immédiat pour les entiers
  @param Cond   Condition à vérifier
  @param Int1   Valeur à renvoyer si la condition est vraie
  @param Int2   Valeur à renvoyer si la condition est fausse
  @return Int1 si Cond vaut True, Int2 sinon
*}
function IIF(Cond: Boolean; Int1, Int2: Integer): Integer;
begin
  if Cond then
    Result := Int1
  else
    Result := Int2;
end;

{*
  Fonction de If Immédiat pour les décimaux
  @param Cond   Condition à vérifier
  @param Flo1   Valeur à renvoyer si la condition est vraie
  @param Flo2   Valeur à renvoyer si la condition est fausse
  @return Flo1 si Cond vaut True, Flo2 sinon
*}
function IIF(Cond: Boolean; Flo1, Flo2: Double): Double;
begin
  if Cond then
    Result := Flo1
  else
    Result := Flo2;
end;

{*
  Fonction de If Immédiat pour les caractères
  @param Cond   Condition à vérifier
  @param Chr1   Valeur à renvoyer si la condition est vraie
  @param Chr2   Valeur à renvoyer si la condition est fausse
  @return Chr1 si Cond vaut True, Chr2 sinon
*}
function IIF(Cond: Boolean; Chr1, Chr2: Char): Char;
begin
  if Cond then
    Result := Chr1
  else
    Result := Chr2;
end;

{*
  Fonction de If Immédiat pour les chaînes de caractères
  @param Cond   Condition à vérifier
  @param Str1   Valeur à renvoyer si la condition est vraie
  @param Str2   Valeur à renvoyer si la condition est fausse
  @return Str1 si Cond vaut True, Str2 sinon
*}
function IIF(Cond: Boolean; const Str1, Str2: string): string;
begin
  if Cond then
    Result := Str1
  else
    Result := Str2;
end;

{*
  Fonction de If Immédiat pour les objets
  @param Cond   Condition à vérifier
  @param Obj1   Valeur à renvoyer si la condition est vraie
  @param Obj2   Valeur à renvoyer si la condition est fausse
  @return Obj1 si Cond vaut True, Obj2 sinon
*}
function IIF(Cond: Boolean; Obj1, Obj2: TObject): TObject;
begin
  if Cond then
    Result := Obj1
  else
    Result := Obj2;
end;

{*
  Fonction de If Immédiat pour les pointeurs
  @param Cond   Condition à vérifier
  @param Ptr1   Valeur à renvoyer si la condition est vraie
  @param Ptr2   Valeur à renvoyer si la condition est fausse
  @return Ptr1 si Cond vaut True, Ptr2 sinon
*}
function IIF(Cond: Boolean; Ptr1, Ptr2: Pointer): Pointer;
begin
  if Cond then
    Result := Ptr1
  else
    Result := Ptr2;
end;

{*
  Fonction de If Immédiat pour les variants
  @param Cond   Condition à vérifier
  @param Var1   Valeur à renvoyer si la condition est vraie
  @param Var2   Valeur à renvoyer si la condition est fausse
  @return Var1 si Cond vaut True, Var2 sinon
*}
function IIF(Cond: Boolean; Var1, Var2: Variant): Variant;
begin
  if Cond then
    Result := Var1
  else
    Result := Var2;
end;

{$ENDREGION}

{*
  S'assure qu'une valeur est bien dans un intervalle spécifié
  @param Value   Valeur de base
  @param Min     Valeur minimale
  @param Max     Valeur maximale
  @return Valeur la plus proche de Value dans l'intervalle [Min;Max]
*}
function MinMax(Value, Min, Max: Integer): Integer;
begin
  if Value > Max then
    Result := Max
  else if Value < Min then
    Result := Min
  else
    Result := Value;
end;

{*
  Division euclidienne
  @param Op1   Dividande
  @param Op2   Diviseur
  @return Résultat de la division euclidienne de Op1 par Op2
  @throws EDivByZero Division par 0
*}
function IntDiv(Op1, Op2: Integer): Integer;
begin
  Result := Floor(Op1 / Op2);
end;

{*
  Reste de la division euclidienne
  @param Op1   Dividande
  @param Op2   Diviseur
  @return Reste de la division euclidienne de Op1 par Op2
  @throws EDivByZero Division par 0
*}
function IntMod(Op1, Op2: Integer): Integer;
begin
  Result := Op1 - IntDiv(Op1, Op2) * Op2;
end;

{*
  Vérifie qu'une base est valide
  @param Base   Base à tester
  @throws EConvertError Base incorrecte
*}
procedure VerifyBase(Base: Byte);
begin
  if (Base < 2) or (Base > MaxBase) then
    raise EConvertError.CreateFmt(sScWrongBase, [Base]);
end;

{*
  Convertit un entier dans une base donnée
  @param Value   Entier à convertir
  @param Base    Base de destination
  @return Représentation en chaîne de Value exprimé dans la base Base
  @throws EConvertError Base incorrecte
*}
function IntToBase(Value: Integer; Base: Byte = 10): string;
var
  Negative: Boolean;
begin
  VerifyBase(Base);

  if Value = 0 then
    Result := NumbersStr[1]
  else
  begin
    Negative := Value < 0;
    if Negative then
      Value := -Value;
    Result := '';
    while Value > 0 do
    begin
      Result := NumbersStr[Value mod Base + 1] + Result;
      Value := Value div Base;
    end;
    if Negative then
      Result := '-'+Result;
  end;
end;

{*
  Convertit un nombre exprimé dans une base donnée en sa représentation décimale
  @param Value   Chaîne de caractère représentant le nombre
  @param Base    Base dans laquelle est exprimée le nombre
  @return Valeur décimale du nombre
  @throws EConvertError Base incorrecte
  @throws EConvertError Entier incorrect
*}
function BaseToInt(const Value: string; Base: Byte = 10): Integer;

  procedure RaiseUncorrectInteger;
  begin
    raise EConvertError.CreateFmt(sScWrongInteger, [Value]);
  end;

var
  Negative: Boolean;
  ResultCopy, Num: Integer;
  Val: string;
begin
  Val := Value;
  VerifyBase(Base);
  if (Val = '') or (Val = '-') then
    RaiseUncorrectInteger;
  Negative := Val[1] = '-';
  if Negative then
    Delete(Val, 1, 1);
  Result := 0;
  while Val <> '' do
  begin
    Num := Pos(Val[1], NumbersStr);
    if (Num = 0) or (Num > Base) then
      RaiseUncorrectInteger;
    Dec(Num);
    ResultCopy := Result;
    Result := Result * Base + Num;
    if Result < ResultCopy then
      RaiseUncorrectInteger;
    Delete(Val, 1, 1);
  end;
  if Negative then
    Result := -Result;
end;

{*
  Convertit un nombre exprimé dans une base donnée en sa représentation décimale
  Lorsque la chaîne n'est pas un entier valide, une valeur par défaut est
  renvoyée.
  @param Value     Chaîne de caractère représentant le nombre
  @param Default   Valeur par défaut
  @param Base      Base dans laquelle est exprimée le nombre
  @return Valeur décimale du nombre
*}
function BaseToIntDef(const Value: string; Default: Integer = 0;
  Base: Byte = 10): Integer;
begin
  try
    Result := BaseToInt(Value, Base);
  except
    on Error: EConvertError do
      Result := Default;
  end;
end;

{*
  Convertit une valeur Double en la valeur Int64 ayant les mêmes bits
  Attention ! Il n'y a aucune correspondance entre Value et Result ! Cette
  fonction est totalement empirique.
  @param Value   Valeur double à convertir
  @return Valeur entière dont les bits sont identiques à Value
*}
function ConvertDoubleToInt64(Value: Double): Int64;
var
  IntValue: Int64 absolute Value;
begin
  Result := IntValue;
end;

{*
  Convertit une valeur Int64 en la valeur Double ayant les mêmes bits
  Attention ! Il n'y a aucune correspondance entre Value et Result ! Cette
  fonction est totalement empirique.
  @param Value   Valeur entière à convertir
  @return Valeur double dont les bits sont identiques à Value
*}
function ConvertInt64ToDouble(Value: Int64): Double;
var
  DoubleValue: Double absolute Value;
begin
  Result := DoubleValue;
end;

{*
  Met en pause l'exécution pendant un temps défini
  Cette routine est dépréciée, utilisez Sleep à la place.
  @param Milliseconds   Nombre de milisecondes pendant lesquelles pauser
*}
procedure Wait(Milliseconds: Integer);
begin
  Sleep(Milliseconds);
end;

{*
  Met en pause l'exécution pendant un temps défini
  Pendant cette pause, les messages Windows de l'applications sont tout de
  même traités.
  @param Milliseconds   Nombre de milisecondes pendant lesquelles pauser
*}
procedure WaitProcessMessages(Milliseconds: Integer);
var
  BeginTime: TDateTime;
begin
  BeginTime := Now;
  while MilliSecondsBetween(Now, BeginTime) < Milliseconds do
    Application.ProcessMessages;
end;

{*
  Convertit un entier en chaîne, avec un nombre minimal de caractères spécifié
  Exemples : IntToStr(345, 4) = '0345' ; IntToStr0(1000, 3) = '1000'
  @param Value    Entier à convertir
  @param Digits   Nombre minimal de caractères de la chaîne convertie
  @return La représentation en chaîne de Value, avec Digits caractères minimum
*}
function IntToStr0(Value, Digits: Integer): string;
begin
  Result := Format('%.*d', [Digits, Value]);
end;

{*
  Lit une chaîne de caractères depuis un flux
  Cette chaîne doit avoir été écrite avec WriteStrToStream.
  @param Stream   Flux depuis lequel lire la chaîne
  @return La chaîne lue
*}
function ReadStrFromStream(Stream: TStream): string;
var
  Len: Integer;
begin
  Stream.ReadBuffer(Len, 4);
  SetLength(Result, Len);
  Stream.ReadBuffer(Result[1], Len);
end;

{*
  Écrit une chaîne de caractères dans un flux
  Cette chaine pourra être relue avec ReadStrFromStream.
  @param Stream   Flux dans lequel enregistrer la chaîne
  @param Str      Chaîne de caractères à écrire
*}
procedure WriteStrToStream(Stream: TStream; const Str: string);
var
  Len: Integer;
begin
  Len := Length(Str);
  Stream.WriteBuffer(Len, 4);
  Stream.WriteBuffer(Str[1], Len);
end;

{*
  Teste si une chaîne de caractères est un nom de fichier correct
  Ce test est effectué conformément aux règles de nommage des fichiers du
  système d'exploitation.
  @param FileName           Chaîne à tester
  @param AcceptPathDelim    Indique si le séparateur de chemin est accepté
  @param AcceptDriveDelim   Indique si le séparateur de disque est accepté
  @return True si FileName est un nom de fichier correct, False sinon
*}
function CorrectFileName(const FileName: string;
  AcceptPathDelim: Boolean = False;
  AcceptDriveDelim: Boolean = False): Boolean;
var
  I: Integer;
  BadChars: set of Char;
begin
  BadChars := ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
  Result := False;
  if FileName = '' then
    Exit;

  // Si le délimiteur de chemin est accepté, on l'exclut de BadChars
  if AcceptPathDelim then
    Exclude(BadChars, PathDelim);

  // Si le délimiteur de disque est accepté, on l'exclut de BadChars
  if AcceptDriveDelim then
    Exclude(BadChars, DriveDelim);

  // On teste tous les caractères de FileName
  for I := 1 to Length(FileName) do
    if FileName[I] in BadChars then
      Exit;
  Result := True;
end;

{*
  Compare deux points
  @param Point1   Premier point
  @param Point2   Second point
  @return True si Point1 et Point2 sont identiques, False sinon
*}
function SamePoint(const Point1, Point2: TPoint): Boolean;
begin
  Result := (Point1.X = Point2.X) and (Point1.Y = Point2.Y);
end;

{*
  Compare deux points 3D
  @param Point1   Premier point
  @param Point2   Second point
  @return True si Point1 et Point2 sont identiques, False sinon
*}
function Same3DPoint(const Point1, Point2: T3DPoint): Boolean;
begin
  Result := (Point1.X = Point2.X) and (Point1.Y = Point2.Y) and
    (Point1.Z = Point2.Z);
end;

{*
  Détermine si un point est nul
  @param Point   Point à tester
  @return True si le point Point est nul, False sinon
*}
function IsNoPoint(const Point: TPoint): Boolean;
begin
  Result := SamePoint(Point, NoPoint);
end;

{*
  Détermine si un point 3D est nul
  @param Point3D   Point à tester
  @return True si le point Point3D est nul, False sinon
*}
function IsNo3DPoint(const Point3D: T3DPoint): Boolean;
begin
  Result := Same3DPoint(Point3D, No3DPoint);
end;

{*
  Compare deux GUID
  @param GUID1   Premier GUID
  @param GUID2   Second GUID
  @return True si GUID1 et GUID2 sont identiques, False sinon
*}
function SameGUID(const GUID1, GUID2: TGUID): Boolean;
begin
  Result := (PInt64(@GUID1)^ = PInt64(@GUID2)^) and
    (PInt64(Integer(@GUID1)+8)^ = PInt64(Integer(@GUID2)+8)^);
end;

{*
  Détermine si un GUID est nul
  @param GUID   GUID à tester
  @return True si le GUID est nul, False sinon
*}
function IsNoGUID(const GUID: TGUID): Boolean;
begin
  Result := (PInt64(@GUID)^ = 0) and (PInt64(Integer(@GUID)+8)^ = 0);
end;

{*
  Crée un point 3D
  @param X   Coordonnée X du point
  @param Y   Coordonnée Y du point
  @param Z   Coordonnée Z du point
  @return Le point 3D (X, Y, Z)
*}
function Point3D(X, Y, Z: Integer): T3DPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

{*
  Convertit un point 3D en chaîne de caractères
  @param Point3D   Point 3D à convertir
  @param Delim     Délimiteur à placer entre les coordonnées
  @return Point3D convertit en chaîne de caractères
*}
function Point3DToString(const Point3D: T3DPoint;
  const Delim: string = ' '): string;
begin
  Result := IntToStr(Point3D.X) + Delim + IntToStr(Point3D.Y) + Delim +
    IntToStr(Point3D.Z);
end;

{$IFDEF MSWINDOWS}
{*
  Lance une URL
  @param URL    URL à lancer
  @param Verb   Verbe à utiliser pour lancer l'URL
*}
procedure RunURL(const URL: string; const Verb: string = 'open');
begin
  ShellExecute(GetDesktopWindow(), PChar(Verb), PChar(URL),
    nil, nil, SW_SHOWNORMAL);
end;
{$ENDIF}

end.

