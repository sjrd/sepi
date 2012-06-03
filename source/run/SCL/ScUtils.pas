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
  D�finit quelques routines usuelles
  @author sjrd
  @version 1.0
*}
unit ScUtils;
{$i ..\..\source\Sepi.inc}
interface

uses
 {$IFNDEF FPC}
 Types,
 {$ENDIF}
 SysUtils, Classes;

{$IF not Declared(CharInSet)}
  {$DEFINE NEED_CHARINSET}
{$IFEND}

type
  {*
    Repr�sente un point situ� dans un espace en trois dimensions
  *}
  T3DPoint = record
    X: Integer; /// Coordonn�e X du point
    Y: Integer; /// Coordonn�e Y du point
    Z: Integer; /// Coordonn�e Z du point
  end;

  {*
    Ensemble de Byte
  *}
  TSysByteSet = set of Byte;

const
  /// Point nul
  NoPoint: TPoint = (X: MaxInt; Y: MaxInt);
  /// Point 3D nul
  No3DPoint: T3DPoint = (X: MaxInt; Y: MaxInt; Z: MaxInt);
  /// GUID nul
  NoGUID: TGUID = (D1: 0; D2: 0; D3: 0; D4: (0, 0, 0, 0, 0, 0, 0, 0));

  /// Verbe par d�faut pour RunURL
  DefaultRunURLVerb = 'open';

function Dir: string;

// Fonctions de If Imm�diat
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

procedure WaitProcessMessages(Milliseconds: Integer);

function IntToStr0(Value, Digits: Integer): string;

{$IF not Declared(TEncoding)}
function ReadStrFromStream(Stream: TStream): string;
procedure WriteStrToStream(Stream: TStream; const Str: string);
{$ELSE}
function ReadStrFromStream(Stream: TStream): string; overload;
procedure WriteStrToStream(Stream: TStream; const Str: string); overload;

function ReadStrFromStream(Stream: TStream;
  Encoding: TEncoding): string; overload;
procedure WriteStrToStream(Stream: TStream; const Str: string;
  Encoding: TEncoding); overload;
{$IFEND}

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

function PointAdd(const Point1, Point2: TPoint): TPoint; overload;
function PointAdd(const Point: TPoint; X, Y: Integer): TPoint; overload;

function Point3DAdd(const Point1, Point2: T3DPoint): T3DPoint; overload;
function Point3DAdd(const Point1: T3DPoint;
  const Point2: TPoint): T3DPoint; overload;
function Point3DAdd(const Point: T3DPoint; X, Y: Integer;
  Z: Integer = 0): T3DPoint; overload;

function Point3DToString(const Point3D: T3DPoint;
  const Delim: string = ' '): string;

function GetMethodFromName(Obj: TObject;
  const MethodName: ShortString): TMethod;

function MakeMethod(Code: Pointer; Data: Pointer = nil): TMethod;

{$IFDEF MSWINDOWS}
procedure RunURL(const URL: string; const Verb: string = DefaultRunURLVerb;
  const Parameters: string = '');
{$ENDIF}

{$IFDEF NEED_CHARINSET}
function CharInSet(C: AnsiChar;
  const CharSet: TSysCharSet): Boolean; overload; inline;
function CharInSet(C: WideChar;
  const CharSet: TSysCharSet): Boolean; overload; inline;
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
function Dir: string;
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
function IIF(Cond: Boolean; Int1, Int2: Integer): Integer;
begin
  if Cond then
    Result := Int1
  else
    Result := Int2;
end;

{*
  Fonction de If Imm�diat pour les d�cimaux
  @param Cond   Condition � v�rifier
  @param Flo1   Valeur � renvoyer si la condition est vraie
  @param Flo2   Valeur � renvoyer si la condition est fausse
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
  Fonction de If Imm�diat pour les caract�res
  @param Cond   Condition � v�rifier
  @param Chr1   Valeur � renvoyer si la condition est vraie
  @param Chr2   Valeur � renvoyer si la condition est fausse
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
  Fonction de If Imm�diat pour les cha�nes de caract�res
  @param Cond   Condition � v�rifier
  @param Str1   Valeur � renvoyer si la condition est vraie
  @param Str2   Valeur � renvoyer si la condition est fausse
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
  Fonction de If Imm�diat pour les objets
  @param Cond   Condition � v�rifier
  @param Obj1   Valeur � renvoyer si la condition est vraie
  @param Obj2   Valeur � renvoyer si la condition est fausse
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
  Fonction de If Imm�diat pour les pointeurs
  @param Cond   Condition � v�rifier
  @param Ptr1   Valeur � renvoyer si la condition est vraie
  @param Ptr2   Valeur � renvoyer si la condition est fausse
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
  Fonction de If Imm�diat pour les variants
  @param Cond   Condition � v�rifier
  @param Var1   Valeur � renvoyer si la condition est vraie
  @param Var2   Valeur � renvoyer si la condition est fausse
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
  S'assure qu'une valeur est bien dans un intervalle sp�cifi�
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
  @return R�sultat de la division euclidienne de Op1 par Op2
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
  V�rifie qu'une base est valide
  @param Base   Base � tester
  @throws EConvertError Base incorrecte
*}
procedure VerifyBase(Base: Byte);
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
  Convertit un nombre exprim� dans une base donn�e en sa repr�sentation d�cimale
  @param Value   Cha�ne de caract�re repr�sentant le nombre
  @param Base    Base dans laquelle est exprim�e le nombre
  @return Valeur d�cimale du nombre
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
  Convertit un nombre exprim� dans une base donn�e en sa repr�sentation d�cimale
  Lorsque la cha�ne n'est pas un entier valide, une valeur par d�faut est
  renvoy�e.
  @param Value     Cha�ne de caract�re repr�sentant le nombre
  @param Default   Valeur par d�faut
  @param Base      Base dans laquelle est exprim�e le nombre
  @return Valeur d�cimale du nombre
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
  Met en pause l'ex�cution pendant un temps d�fini
  Pendant cette pause, les messages Windows de l'applications sont tout de
  m�me trait�s.
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
  Convertit un entier en cha�ne, avec un nombre minimal de caract�res sp�cifi�
  Exemples : IntToStr(345, 4) = '0345' ; IntToStr0(1000, 3) = '1000'
  @param Value    Entier � convertir
  @param Digits   Nombre minimal de caract�res de la cha�ne convertie
  @return La repr�sentation en cha�ne de Value, avec Digits caract�res minimum
*}
function IntToStr0(Value, Digits: Integer): string;
begin
  Result := Format('%.*d', [Digits, Value]);
end;

{*
  Lit une cha�ne de caract�res depuis un flux
  Cette cha�ne doit avoir �t� �crite avec WriteStrToStream.
  @param Stream   Flux depuis lequel lire la cha�ne
  @return La cha�ne lue
*}
function ReadStrFromStream(Stream: TStream): string;
var
  Len: Integer;
begin
  Stream.ReadBuffer(Len, 4);
  SetLength(Result, Len);
  Stream.ReadBuffer(Result[1], Len * SizeOf(Char));
end;

{*
  �crit une cha�ne de caract�res dans un flux
  Cette chaine pourra �tre relue avec ReadStrFromStream.
  @param Stream   Flux dans lequel enregistrer la cha�ne
  @param Str      Cha�ne de caract�res � �crire
*}
procedure WriteStrToStream(Stream: TStream; const Str: string);
var
  Len: Integer;
begin
  Len := Length(Str);
  Stream.WriteBuffer(Len, 4);
  Stream.WriteBuffer(Str[1], Len * SizeOf(Char));
end;

{$IF Declared(TEncoding)}
{*
  Lit une cha�ne de caract�res depuis un flux
  Cette cha�ne doit avoir �t� �crite avec WriteStrToStream.
  @param Stream     Flux depuis lequel lire la cha�ne
  @param Encoding   Encodage utilis� pour stocker la cha�ne
  @return La cha�ne lue
*}
function ReadStrFromStream(Stream: TStream; Encoding: TEncoding): string;
var
  Buffer: TBytes;
  Len: Integer;
begin
  Stream.ReadBuffer(Len, 4);
  SetLength(Buffer, Len);
  Stream.ReadBuffer(Buffer[0], Len);
  Result := Encoding.GetString(Buffer);
end;

{*
  �crit une cha�ne de caract�res dans un flux
  Cette chaine pourra �tre relue avec ReadStrFromStream.
  @param Stream     Flux dans lequel enregistrer la cha�ne
  @param Str        Cha�ne de caract�res � �crire
  @param Encoding   Encodage utilis� pour stocker la cha�ne
*}
procedure WriteStrToStream(Stream: TStream; const Str: string;
  Encoding: TEncoding);
var
  Buffer: TBytes;
  Len: Integer;
begin
  Buffer := Encoding.GetBytes(Str);
  Len := Length(Buffer);
  Stream.WriteBuffer(Len, 4);
  Stream.WriteBuffer(Buffer[0], Len);
end;
{$IFEND}

{*
  Teste si une cha�ne de caract�res est un nom de fichier correct
  Ce test est effectu� conform�ment aux r�gles de nommage des fichiers du
  syst�me d'exploitation.
  @param FileName           Cha�ne � tester
  @param AcceptPathDelim    Indique si le s�parateur de chemin est accept�
  @param AcceptDriveDelim   Indique si le s�parateur de disque est accept�
  @return True si FileName est un nom de fichier correct, False sinon
*}
function CorrectFileName(const FileName: string;
  AcceptPathDelim: Boolean = False;
  AcceptDriveDelim: Boolean = False): Boolean;
var
  I: Integer;
  BadChars: TSysCharSet;
begin
  BadChars := ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
  Result := False;
  if FileName = '' then
    Exit;

  // Si le d�limiteur de chemin est accept�, on l'exclut de BadChars
  if AcceptPathDelim then
    Exclude(BadChars, PathDelim);

  // Si le d�limiteur de disque est accept�, on l'exclut de BadChars
  if AcceptDriveDelim then
    Exclude(BadChars, DriveDelim);

  // On teste tous les caract�res de FileName
  for I := 1 to Length(FileName) do
    if CharInSet(FileName[I], BadChars) then
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
  D�termine si un point est nul
  @param Point   Point � tester
  @return True si le point Point est nul, False sinon
*}
function IsNoPoint(const Point: TPoint): Boolean;
begin
  Result := SamePoint(Point, NoPoint);
end;

{*
  D�termine si un point 3D est nul
  @param Point3D   Point � tester
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
  D�termine si un GUID est nul
  @param GUID   GUID � tester
  @return True si le GUID est nul, False sinon
*}
function IsNoGUID(const GUID: TGUID): Boolean;
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
function Point3D(X, Y, Z: Integer): T3DPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

{*
  Additionne deux points
  @param Point1   Premier point
  @param Point2   Second point
  @return Somme des deux points
*}
function PointAdd(const Point1, Point2: TPoint): TPoint;
begin
  Result.X := Point1.X + Point2.X;
  Result.Y := Point1.Y + Point2.Y;
end;

{*
  Ajoute des coordonn�es X et Y � un point
  @param Point   Point auquel ajouter les coordonn�es
  @param X       Ajout en abscisses
  @param Y       Ajout en ordonn�es
  @return Somme du point Point et du point Point(X, Y)
*}
function PointAdd(const Point: TPoint; X, Y: Integer): TPoint;
begin
  Result.X := Point.X + X;
  Result.Y := Point.Y + Y;
end;

{*
  Additionne deux points 3D
  @param Point1   Premier point
  @param Point2   Second point
  @return Somme des deux points
*}
function Point3DAdd(const Point1, Point2: T3DPoint): T3DPoint;
begin
  Result.X := Point1.X + Point2.X;
  Result.Y := Point1.Y + Point2.Y;
  Result.Z := Point1.Z + Point2.Z;
end;

{*
  Ajoute un point � un point 3D
  @param Point1   Premier point
  @param Point2   Second point
  @return Somme des deux points
*}
function Point3DAdd(const Point1: T3DPoint; const Point2: TPoint): T3DPoint;
begin
  Result.X := Point1.X + Point2.X;
  Result.Y := Point1.Y + Point2.Y;
  Result.Z := Point1.Z;
end;

{*
  Ajoute des coordonn�es X, Y et Z � un point 3D
  @param Point   Point auquel ajouter les coordonn�es
  @param X       Ajout en abscisses
  @param Y       Ajout en ordonn�es
  @param Z       Ajout en hauteur (d�faut = 0)
  @return Somme du point Point et du point Point3D(X, Y, Z)
*}
function Point3DAdd(const Point: T3DPoint; X, Y: Integer;
  Z: Integer = 0): T3DPoint;
begin
  Result.X := Point.X + X;
  Result.Y := Point.Y + Y;
  Result.Z := Point.Z + Z;
end;

{*
  Convertit un point 3D en cha�ne de caract�res
  @param Point3D   Point 3D � convertir
  @param Delim     D�limiteur � placer entre les coordonn�es
  @return Point3D convertit en cha�ne de caract�res
*}
function Point3DToString(const Point3D: T3DPoint;
  const Delim: string = ' '): string;
begin
  Result := IntToStr(Point3D.X) + Delim + IntToStr(Point3D.Y) + Delim +
    IntToStr(Point3D.Z);
end;

{*
  Recherche une m�thode d'un objet � partir de son nom
  La m�thode en question doit �tre publi�e pour pouvoir �tre trouv�e.
  @param Obj          L'objet d�finissant la m�thode
  @param MethodName   Nom de la m�thode
  @return Une r�f�rence � la m�thode recherch�e pour l'objet Obj
*}
function GetMethodFromName(Obj: TObject;
  const MethodName: ShortString): TMethod;
begin
  Result.Code := Obj.MethodAddress(MethodName);
  Result.Data := Obj;
end;

{*
  Construit un record TMethod
  @param Code   Valeur du champ Code
  @param Data   Valeur du champ Data
  @return Un enregistrement TMethod avec les champs indiqu�s
*}
function MakeMethod(Code: Pointer; Data: Pointer = nil): TMethod;
begin
  Result.Code := Code;
  Result.Data := Data;
end;

{$IFDEF MSWINDOWS}
{*
  Lance une URL
  @param URL    URL � lancer
  @param Verb   Verbe � utiliser pour lancer l'URL
*}
procedure RunURL(const URL: string; const Verb: string = DefaultRunURLVerb;
  const Parameters: string = '');
begin
  ShellExecute(GetDesktopWindow(), PChar(Verb), PChar(URL),
    PChar(Parameters), nil, SW_SHOWNORMAL);
end;
{$ENDIF}

{$IFDEF NEED_CHARINSET}
{*
  Teste si un caract�re est pr�sent dans un ensemble de caract�res
  @param C         Caract�re � tester
  @param CharSet   Ensemble de caract�res
  @return True ssi C appartient � l'ensemble CharSet
*}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;

{*
  Teste si un caract�re est pr�sent dans un ensemble de caract�res
  @param C         Caract�re � tester
  @param CharSet   Ensemble de caract�res
  @return True ssi C appartient � l'ensemble CharSet
*}
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := (C < #$0100) and (AnsiChar(C) in CharSet);
end;
{$ENDIF}

end.

