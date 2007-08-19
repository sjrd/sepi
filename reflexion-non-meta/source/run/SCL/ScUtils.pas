{*
  D�finit quelques routines usuelles
  @author sjrd
  @version 1.0
*}
unit ScUtils;

interface

uses
  Types, SysUtils, Classes;

type
  {*
    Repr�sente un point situ� dans un espace en trois dimensions
  *}
  T3DPoint = record
    X : integer; /// Coordonn�e X du point
    Y : integer; /// Coordonn�e Y du point
    Z : integer; /// Coordonn�e Z du point
  end;

  {*
    Ensemble de Byte
  *}
  TSysByteSet = set of Byte;

  {*
    Ensemble de caract�res
    Ce type est d�pr�ci�, utilisez TSysCharSet � la place.
  *}
  TSetOfChars = TSysCharSet {$IFNDEF DCTD} deprecated {$ENDIF};

  {*
    Ensemble de Byte
    Ce type est d�pr�ci�, utilisez TSysByteSet � la place.
  *}
  TSetOfBytes = TSysByteSet {$IFNDEF DCTD} deprecated {$ENDIF};

const
  /// Point nul
  NoPoint : TPoint = (X : MaxInt; Y : MaxInt);
  /// Point 3D nul
  No3DPoint : T3DPoint = (X : MaxInt; Y : MaxInt; Z : MaxInt);
  /// GUID nul
  NoGUID : TGUID = (D1 : 0; D2 : 0; D3 : 0; D4 : (0, 0, 0, 0, 0, 0, 0, 0));

function Dir : string;

// Fonctions de If Imm�diat
function IIF(Cond : boolean; Int1, Int2 : integer) : integer; overload;
function IIF(Cond : boolean; Flo1, Flo2 : Double ) : Double ; overload;
function IIF(Cond : boolean; Chr1, Chr2 : Char   ) : Char   ; overload;
function IIF(Cond : boolean; const Str1, Str2 : string) : string; overload;
function IIF(Cond : boolean; Obj1, Obj2 : TObject) : TObject; overload;
function IIF(Cond : boolean; Ptr1, Ptr2 : Pointer) : Pointer; overload;
function IIF(Cond : boolean; Var1, Var2 : Variant) : Variant; overload;

function MinMax(Value, Min, Max : integer) : integer;

function IntDiv(Op1, Op2 : integer) : integer;
function IntMod(Op1, Op2 : integer) : integer;

function IntToBase(Value : integer; Base : Byte = 10) : string;
function BaseToInt(const Value : string; Base : Byte = 10) : integer;
function BaseToIntDef(const Value : string; Default : integer = 0;
  Base : Byte = 10) : integer;

function ConvertDoubleToInt64(Value : Double) : Int64;
function ConvertInt64ToDouble(Value : Int64) : Double;

procedure Wait(Milliseconds : integer); deprecated;
procedure WaitProcessMessages(Milliseconds : integer);

function IntToStr0(Value, Digits : integer) : string;

function ReadStrFromStream(Stream : TStream) : string;
procedure WriteStrToStream(Stream : TStream; const Str : string);

function CorrectFileName(const FileName : string;
  AcceptPathDelim : boolean = False;
  AcceptDriveDelim : boolean = False) : boolean;

function SamePoint(const Point1, Point2 : TPoint) : boolean;
function Same3DPoint(const Point1, Point2 : T3DPoint) : boolean;

function IsNoPoint(const Point : TPoint) : boolean;
function IsNo3DPoint(const Point3D : T3DPoint) : boolean;

function SameGUID(const GUID1, GUID2 : TGUID) : boolean;
function IsNoGUID(const GUID : TGUID) : boolean;

function Point3D(X, Y, Z : integer) : T3DPoint;

function Point3DToString(const Point3D : T3DPoint;
  const Delim : string = ' ') : string;

{$IFDEF MSWINDOWS}
procedure RunURL(const URL : string; const Verb : string = 'open');
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
{ Proc�dures et fonctions globales }
{----------------------------------}

{*
  Renvoie le chemin du dossier dans lequel se trouve l'application qui s'ex�cute
  @return Le chemin du dossier dans lequel se trouve l'application qui s'ex�cute
*}
function Dir : string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

{$REGION 'Fonctions de If Imm�diat'}

{*
  Fonction de If Imm�diat pour les entiers
  @param Cond   Condition � v�rifier
  @param Int1   Valeur � renvoyer si la condition est vraie
  @param Int2   Valeur � renvoyer si la condition est fausse
  @return Int1 si Cond vaut True, Int2 sinon
*}
function IIF(Cond : boolean; Int1, Int2 : integer) : integer;
begin
  if Cond then Result := Int1 else Result := Int2;
end;

{*
  Fonction de If Imm�diat pour les d�cimaux
  @param Cond   Condition � v�rifier
  @param Flo1   Valeur � renvoyer si la condition est vraie
  @param Flo2   Valeur � renvoyer si la condition est fausse
  @return Flo1 si Cond vaut True, Flo2 sinon
*}
function IIF(Cond : boolean; Flo1, Flo2 : Double) : Double;
begin
  if Cond then Result := Flo1 else Result := Flo2;
end;

{*
  Fonction de If Imm�diat pour les caract�res
  @param Cond   Condition � v�rifier
  @param Chr1   Valeur � renvoyer si la condition est vraie
  @param Chr2   Valeur � renvoyer si la condition est fausse
  @return Chr1 si Cond vaut True, Chr2 sinon
*}
function IIF(Cond : boolean; Chr1, Chr2 : Char) : Char;
begin
  if Cond then Result := Chr1 else Result := Chr2;
end;

{*
  Fonction de If Imm�diat pour les cha�nes de caract�res
  @param Cond   Condition � v�rifier
  @param Str1   Valeur � renvoyer si la condition est vraie
  @param Str2   Valeur � renvoyer si la condition est fausse
  @return Str1 si Cond vaut True, Str2 sinon
*}
function IIF(Cond : boolean; const Str1, Str2 : string) : string;
begin
  if Cond then Result := Str1 else Result := Str2;
end;

{*
  Fonction de If Imm�diat pour les objets
  @param Cond   Condition � v�rifier
  @param Obj1   Valeur � renvoyer si la condition est vraie
  @param Obj2   Valeur � renvoyer si la condition est fausse
  @return Obj1 si Cond vaut True, Obj2 sinon
*}
function IIF(Cond : boolean; Obj1, Obj2 : TObject) : TObject;
begin
  if Cond then Result := Obj1 else Result := Obj2;
end;

{*
  Fonction de If Imm�diat pour les pointeurs
  @param Cond   Condition � v�rifier
  @param Ptr1   Valeur � renvoyer si la condition est vraie
  @param Ptr2   Valeur � renvoyer si la condition est fausse
  @return Ptr1 si Cond vaut True, Ptr2 sinon
*}
function IIF(Cond : boolean; Ptr1, Ptr2 : Pointer) : Pointer;
begin
  if Cond then Result := Ptr1 else Result := Ptr2;
end;

{*
  Fonction de If Imm�diat pour les variants
  @param Cond   Condition � v�rifier
  @param Var1   Valeur � renvoyer si la condition est vraie
  @param Var2   Valeur � renvoyer si la condition est fausse
  @return Var1 si Cond vaut True, Var2 sinon
*}
function IIF(Cond : boolean; Var1, Var2 : Variant) : Variant;
begin
  if Cond then Result := Var1 else Result := Var2;
end;

{$ENDREGION}

{*
  S'assure qu'une valeur est bien dans un intervalle sp�cifi�
  @param Value   Valeur de base
  @param Min     Valeur minimale
  @param Max     Valeur maximale
  @return Valeur la plus proche de Value dans l'intervalle [Min;Max]
*}
function MinMax(Value, Min, Max : integer) : integer;
begin
  if Value > Max then Result := Max else
  if Value < Min then Result := Min else
  Result := Value;
end;

{*
  Division euclidienne
  @param Op1   Dividande
  @param Op2   Diviseur
  @return R�sultat de la division euclidienne de Op1 par Op2
  @throws EDivByZero Division par 0
*}
function IntDiv(Op1, Op2 : integer) : integer;
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
function IntMod(Op1, Op2 : integer) : integer;
begin
  Result := Op1 - IntDiv(Op1, Op2) * Op2;
end;

{*
  V�rifie qu'une base est valide
  @param Base   Base � tester
  @throws EConvertError Base incorrecte
*}
procedure VerifyBase(Base : Byte);
begin
  if (Base < 2) or (Base > MaxBase) then
    raise EConvertError.CreateFmt(sScWrongBase, [Base]);
end;

{*
  Convertit un entier dans une base donn�e
  @param Value   Entier � convertir
  @param Base    Base de destination
  @return Repr�sentation en cha�ne de Value exprim� dans la base Base
  @throws EConvertError Base incorrecte
*}
function IntToBase(Value : integer; Base : Byte = 10) : string;
var Negative : boolean;
begin
  VerifyBase(Base);

  if Value = 0 then Result := NumbersStr[1] else
  begin
    Negative := Value < 0;
    if Negative then Value := -Value;
    Result := '';
    while Value > 0 do
    begin
      Result := NumbersStr[Value mod Base + 1] + Result;
      Value := Value div Base;
    end;
    if Negative then Result := '-'+Result;
  end;
end;

{*
  Convertit un nombre exprim� dans une base donn�e en sa repr�sentation d�cimale
  @param Value   Cha�ne de caract�re repr�sentant le nombre
  @param Base    Base dans laquelle est exprim�e le nombre
  @return Valeur d�cimale du nombre
  @throws EConvertError Base incorrecte
  @throws EConvertError Entier incorrect
*}
function BaseToInt(const Value : string; Base : Byte = 10) : integer;
  procedure RaiseUncorrectInteger;
  begin
    raise EConvertError.CreateFmt(sScWrongInteger, [Value]);
  end;
var Negative : boolean;
    ResultCopy, Num : integer;
    Val : string;
begin
  Val := Value;
  VerifyBase(Base);
  if (Val = '') or (Val = '-') then
    RaiseUncorrectInteger;
  Negative := Val[1] = '-';
  if Negative then Delete(Val, 1, 1);
  Result := 0;
  while Val <> '' do
  begin
    Num := Pos(Val[1], NumbersStr);
    if (Num = 0) or (Num > Base) then
      RaiseUncorrectInteger;
    dec(Num);
    ResultCopy := Result;
    Result := Result * Base + Num;
    if Result < ResultCopy then
      RaiseUncorrectInteger;
    Delete(Val, 1, 1);
  end;
  if Negative then Result := -Result;
end;

{*
  Convertit un nombre exprim� dans une base donn�e en sa repr�sentation d�cimale
  Lorsque la cha�ne n'est pas un entier valide, une valeur par d�faut est
  renvoy�e.
  @param Value     Cha�ne de caract�re repr�sentant le nombre
  @param Default   Valeur par d�faut
  @param Base      Base dans laquelle est exprim�e le nombre
  @return Valeur d�cimale du nombre
*}
function BaseToIntDef(const Value : string; Default : integer = 0;
  Base : Byte = 10) : integer;
begin
  try
    Result := BaseToInt(Value, Base);
  except
    on Error : EConvertError do Result := Default;
  end;
end;

{*
  Convertit une valeur Double en la valeur Int64 ayant les m�mes bits
  Attention ! Il n'y a aucune correspondance entre Value et Result ! Cette
  fonction est totalement empirique.
  @param Value   Valeur double � convertir
  @return Valeur enti�re dont les bits sont identiques � Value
*}
function ConvertDoubleToInt64(Value : Double) : Int64;
var IntValue : Int64 absolute Value;
begin
  Result := IntValue;
end;

{*
  Convertit une valeur Int64 en la valeur Double ayant les m�mes bits
  Attention ! Il n'y a aucune correspondance entre Value et Result ! Cette
  fonction est totalement empirique.
  @param Value   Valeur enti�re � convertir
  @return Valeur double dont les bits sont identiques � Value
*}
function ConvertInt64ToDouble(Value : Int64) : Double;
var DoubleValue : Double absolute Value;
begin
  Result := DoubleValue;
end;

{*
  Met en pause l'ex�cution pendant un temps d�fini
  Cette routine est d�pr�ci�e, utilisez Sleep � la place.
  @param Milliseconds   Nombre de milisecondes pendant lesquelles pauser
*}
procedure Wait(Milliseconds : integer);
begin
  Sleep(Milliseconds);
end;

{*
  Met en pause l'ex�cution pendant un temps d�fini
  Pendant cette pause, les messages Windows de l'applications sont tout de
  m�me trait�s.
  @param Milliseconds   Nombre de milisecondes pendant lesquelles pauser
*}
procedure WaitProcessMessages(Milliseconds : integer);
var BeginTime : TDateTime;
begin
  BeginTime := Now;
  while MilliSecondsBetween(Now, BeginTime) < Milliseconds do
    Application.ProcessMessages;
end;

{*
  Convertit un entier en cha�ne, avec un nombre minimal de caract�res sp�cifi�
  Exemples : IntToStr(345, 4) = '0345' ; IntToStr0(1000, 3) = '1000'
  @param Value    Entier � convertir
  @param Digits   Nombre minimal de caract�res de la cha�ne convertie
  @return La repr�sentation en cha�ne de Value, avec Digits caract�res minimum
*}
function IntToStr0(Value, Digits : integer) : string;
begin
  Result := Format('%.*d', [Digits, Value]);
end;

{*
  Lit une cha�ne de caract�res depuis un flux
  Cette cha�ne doit avoir �t� �crite avec WriteStrToStream.
  @param Stream   Flux depuis lequel lire la cha�ne
  @return La cha�ne lue
*}
function ReadStrFromStream(Stream : TStream) : string;
var Len : integer;
begin
  Stream.ReadBuffer(Len, 4);
  SetLength(Result, Len);
  Stream.ReadBuffer(Result[1], Len);
end;

{*
  �crit une cha�ne de caract�res dans un flux
  Cette chaine pourra �tre relue avec ReadStrFromStream.
  @param Stream   Flux dans lequel enregistrer la cha�ne
  @param Str      Cha�ne de caract�res � �crire
*}
procedure WriteStrToStream(Stream : TStream; const Str : string);
var Len : integer;
begin
  Len := Length(Str);
  Stream.WriteBuffer(Len, 4);
  Stream.WriteBuffer(Str[1], Len);
end;

{*
  Teste si une cha�ne de caract�res est un nom de fichier correct
  Ce test est effectu� conform�ment aux r�gles de nommage des fichiers du
  syst�me d'exploitation.
  @param FileName           Cha�ne � tester
  @param AcceptPathDelim    Indique si le s�parateur de chemin est accept�
  @param AcceptDriveDelim   Indique si le s�parateur de disque est accept�
  @return True si FileName est un nom de fichier correct, False sinon
*}
function CorrectFileName(const FileName : string;
  AcceptPathDelim : boolean = False;
  AcceptDriveDelim : boolean = False) : boolean;
var I : integer;
    BadChars : set of Char;
begin
  BadChars := ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
  Result := False;
  if FileName = '' then exit;

  // Si le d�limiteur de chemin est accept�, on l'exclut de BadChars
  if AcceptPathDelim then
    Exclude(BadChars, PathDelim);

  // Si le d�limiteur de disque est accept�, on l'exclut de BadChars
  if AcceptDriveDelim then
    Exclude(BadChars, DriveDelim);

  // On teste tous les caract�res de FileName
  for I := 1 to Length(FileName) do if FileName[I] in BadChars then exit;
  Result := True;
end;

{*
  Compare deux points
  @param Point1   Premier point
  @param Point2   Second point
  @return True si Point1 et Point2 sont identiques, False sinon
*}
function SamePoint(const Point1, Point2 : TPoint) : boolean;
begin
  Result := (Point1.X = Point2.X) and (Point1.Y = Point2.Y);
end;

{*
  Compare deux points 3D
  @param Point1   Premier point
  @param Point2   Second point
  @return True si Point1 et Point2 sont identiques, False sinon
*}
function Same3DPoint(const Point1, Point2 : T3DPoint) : boolean;
begin
  Result := (Point1.X = Point2.X) and (Point1.Y = Point2.Y) and
    (Point1.Z = Point2.Z);
end;

{*
  D�termine si un point est nul
  @param Point   Point � tester
  @return True si le point Point est nul, False sinon
*}
function IsNoPoint(const Point : TPoint) : boolean;
begin
  Result := SamePoint(Point, NoPoint);
end;

{*
  D�termine si un point 3D est nul
  @param Point3D   Point � tester
  @return True si le point Point3D est nul, False sinon
*}
function IsNo3DPoint(const Point3D : T3DPoint) : boolean;
begin
  Result := Same3DPoint(Point3D, No3DPoint);
end;

{*
  Compare deux GUID
  @param GUID1   Premier GUID
  @param GUID2   Second GUID
  @return True si GUID1 et GUID2 sont identiques, False sinon
*}
function SameGUID(const GUID1, GUID2 : TGUID) : boolean;
begin
  Result := (PInt64(@GUID1)^ = PInt64(@GUID2)^) and
    (PInt64(Integer(@GUID1)+8)^ = PInt64(Integer(@GUID2)+8)^);
end;

{*
  D�termine si un GUID est nul
  @param GUID   GUID � tester
  @return True si le GUID est nul, False sinon
*}
function IsNoGUID(const GUID : TGUID) : boolean;
begin
  Result := (PInt64(@GUID)^ = 0) and (PInt64(Integer(@GUID)+8)^ = 0);
end;

{*
  Cr�e un point 3D
  @param X   Coordonn�e X du point
  @param Y   Coordonn�e Y du point
  @param Z   Coordonn�e Z du point
  @return Le point 3D (X, Y, Z)
*}
function Point3D(X, Y, Z : integer) : T3DPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

{*
  Convertit un point 3D en cha�ne de caract�res
  @param Point3D   Point 3D � convertir
  @param Delim     D�limiteur � placer entre les coordonn�es
  @return Point3D convertit en cha�ne de caract�res
*}
function Point3DToString(const Point3D : T3DPoint;
  const Delim : string = ' ') : string;
begin
  Result := IntToStr(Point3D.X) + Delim + IntToStr(Point3D.Y) + Delim +
    IntToStr(Point3D.Z);
end;

{$IFDEF MSWINDOWS}
{*
  Lance une URL
  @param URL    URL � lancer
  @param Verb   Verbe � utiliser pour lancer l'URL
*}
procedure RunURL(const URL : string; const Verb : string = 'open');
begin
  ShellExecute(GetDesktopWindow(), PChar(Verb), PChar(URL),
    nil, nil, SW_SHOWNORMAL);
end;
{$ENDIF}

end.

