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
  Définit quelques routines de gestion de chaînes
  @author sjrd
  @version 1.0
*}
unit ScStrUtils;
{$i ..\..\source\Sepi.inc}
interface

uses
  Types
  {$IFDEF FPC}
  , Math // needed for TValueRelationship
  , StringHashList // needed to replace delphi TStringHash
  {$ENDIF};

type
  {*
    Option de comparaison de chaînes
    coIgnoreCase     : Ignore la casse
    coIgnoreNonSpace : Ignore les accents
    coIgnoreSymbols  : Ignore les symboles
  *}
  TCompareStrOption = (
    coIgnoreCase,
    coIgnoreNonSpace,
    coIgnoreSymbols
  );

  {*
    Ensemble d'options de comparaison de chaînes
  *}
  TCompareStrOptions = set of TCompareStrOption;

function RightPos(C: Char; const Str: string): Integer;

function NberSubStr(const SubStr, Str: string): Integer;
function NberCharInStr(C: Char; const Str: string): Integer;

{$IFDEF MSWINDOWS}
function CompareStringEx(const S1, S2: string;
  CompareOptions: TCompareStrOptions = []): Integer; platform;
{$ENDIF}

function GetFirstToken(const S: string; Token: Char): string;
function ExtractFirstToken(var S: string; Token: Char): string;
function GetLastToken(const S: string; Token: Char): string;
function ExtractLastToken(var S: string; Token: Char): string;
function SplitToken(const S: string; Token: Char;
  out LeftStr, RightStr: string): Boolean;
function SplitTokenAll(const S: string; Token: Char): TStringDynArray;

function GetXToken(const S: string; Token: Char; X: Integer): string;
function GetXWord(const S: string; X: Integer): string;

function PosWord(const Wrd, Str: string; Index: Integer = 1): Integer;

function HashOfStr(const Str: string): Cardinal;

function CompareVersion(const Left, Right: string): TValueRelationship;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, StrUtils, Classes, IniFiles, ScUtils;

{*
  Cherche un caractère dans une chaîne à partir de la fin de celle-ci
  @param C     Caractère à rechercher
  @param Str   Chaîne dans laquelle rechercher C
  @return Index de la dernière occurence de C dans Str, ou 0 si non trouvé
*}
function RightPos(C: Char; const Str: string): Integer;
begin
  Result := Length(Str);
  while Result > 0 do
    if Str[Result] = C then
      Exit
    else
      Dec(Result);
end;

{*
  Calcule le nombre d'occurences d'une sous-chaîne dans une chaîne
  @param SubStr   Sous-chaîne à chercher
  @param Str      Chaîne dans laquelle chercher SubStr
  @return Nombre d'occurences de SubStr dans Str
*}
function NberSubStr(const SubStr, Str: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Length(Str)-Length(SubStr)+1 downto 1 do
    if Copy(Str, I, Length(SubStr)) = SubStr then
      Inc(Result);
end;

{*
  Calcule le nombre d'occurences d'un caractère dans une chaîne
  @param C     Caractère à chercher
  @param Str   Chaîne dans laquelle chercher C
  @return Nombre d'occurences de C dans Str
*}
function NberCharInStr(C: Char; const Str: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Length(Str) downto 1 do
    if Str[I] = C then
      Inc(Result);
end;

{$IFDEF MSWINDOWS}
{*
  Compare deux chaînes de caractères avec des options avancées
  @param S1               Première chaîne
  @param S2               Seconde chaîne
  @param CompareOptions   Options de comparaison
  @return 0 si les chaînes sont semblables, un nombre positif si la première
          est supérieure à la seconde, un nombre négatif dans le cas inverse
*}
function CompareStringEx(const S1, S2: string;
  CompareOptions: TCompareStrOptions = []): Integer;
var
  Flags: DWord;
begin
  Flags := 0;

  // On ajoute les flags de comparaison
  if coIgnoreCase     in CompareOptions then
    Flags := Flags+NORM_IGNORECASE;
  if coIgnoreNonSpace in CompareOptions then
    Flags := Flags+NORM_IGNORENONSPACE;
  if coIgnoreSymbols  in CompareOptions then
    Flags := Flags+NORM_IGNORESYMBOLS;

  // Appel de Windows.CompareString
  Result := Windows.CompareString(LOCALE_USER_DEFAULT, Flags,
    PChar(S1), -1, PChar(S2), -1)-2;
end;
{$ENDIF}

{*
  Découpe une chaîne selon un délimiteur et récupère la première sous-chaîne
  @param S       Chaîne à découper
  @param Token   Délimiteur
  @return La première sous-chaîne de S délimitée par Token (ou S si non trouvé)
*}
function GetFirstToken(const S: string; Token: Char): string;
var
  I: Integer;
begin
  I := 1;
  // On parcourt la chaîne jusqu'à trouver un caractère Token
  while (I <= Length(S)) and (S[I] <> Token) do
    Inc(I);
  // On copie la chaîne depuis le début jusqu'au caractère avant Token
  Result := Copy(S, 1, I-1);
end;

{*
  Extrait la première sous-chaîne d'une chaîne et la supprime de l'originale
  @param S       Chaîne à découper
  @param Token   Délimiteur
  @return La première sous-chaîne de S délimitée par Token (ou S si non trouvé)
*}
function ExtractFirstToken(var S: string; Token: Char): string;
begin
  Result := GetFirstToken(S, Token);
  Delete(S, 1, Length(Result)+1);
end;

{*
  Découpe une chaîne selon un délimiteur et récupère la dernière sous-chaîne
  @param S       Chaîne à découper
  @param Token   Délimiteur
  @return La dernière sous-chaîne de S délimitée par Token (ou S si non trouvé)
*}
function GetLastToken(const S: string; Token: Char): string;
var
  I: Integer;
begin
  I := Length(S);
  // On parcourt la chaîne à l'envers jusqu'à trouver un caractère Token
  while (I > 0) and (S[I] <> Token) do
    Dec(I);
  // On copie la chaîne depuis le caractère après Token jusqu'à la fin
  Result := Copy(S, I+1, MaxInt);
end;

{*
  Extrait la dernière sous-chaîne d'une chaîne et la supprime de l'originale
  @param S       Chaîne à découper
  @param Token   Délimiteur
  @return La dernière sous-chaîne de S délimitée par Token (ou S si non trouvé)
*}
function ExtractLastToken(var S: string; Token: Char): string;
begin
  Result := GetLastToken(S, Token);
  Delete(S, Length(S)-Length(Result)-1, MaxInt);
end;

{*
  Découpe une chaîne en deux selon un délimiteur
  Si plusieurs délimiteurs consécutifs sont trouvés, ils sont considérés comme
  un seul. Mais s'ils sont séparés, seul le premier groupe prendra effet.
  @param S          Chaîne à découper
  @param Token      Délimiteur
  @param LeftStr    Sous-chaîne de gauche (ou S si non trouvé)
  @param RightStr   Sous-chaîne de droite (ou une chaîne vide si non trouvé)
  @return True si la chaîne a été séparée, False si Token n'est pas trouvé
*}
function SplitToken(const S: string; Token: Char;
  out LeftStr, RightStr: string): Boolean;
var
  Len, I: Integer;
begin
  Len := Length(S);

  // Recherche de la première occurence de Token
  I := 1;
  while (I <= Len) and (S[I] <> Token) do
    Inc(I);
  Result := I <= Len;

  if Result then
  begin
    // Trouvé : séparer LeftStr et RightStr
    LeftStr := Copy(S, 1, I-1);
    while (I <= Len) and (S[I] = Token) do
      Inc(I);
    RightStr := Copy(S, I, MaxInt);
  end else
  begin
    // Non trouvé : LeftStr = S et RightStr = ''
    LeftStr := S;
    RightStr := '';
  end;
end;

{*
  Découpe une chaîne en morceaux selon un délimiteur
  @param S          Chaîne à découper
  @param Token      Délimiteur
  @return Tableau des morceaux de la chaîne
*}
function SplitTokenAll(const S: string; Token: Char): TStringDynArray;
var
  Count, I: Integer;
  Pos, SavedPos: PChar;
begin
  Count := 1;

  for I := 1 to Length(S) do
    if S[I] = Token then
      Inc(Count);

  SetLength(Result, Count);
  Pos := PChar(S);

  for I := 0 to Count-2 do
  begin
    SavedPos := Pos;
    while Pos^ <> Token do
      Inc(Pos);

    SetString(Result[I], SavedPos, Pos-SavedPos);
    Inc(Pos);
  end;

  Result[Count-1] := string(Pos);
end;

{*
  Découpe une chaîne selon un délimiteur et récupère la Xème sous-chaîne
  @param S       Chaîne à découper
  @param Token   Délimiteur
  @param X       Index base sur 1 de la sous-chaîne à récupérer
  @return La Xème sous-chaîne de S délimitée par Token (ou '' si X trop grand)
*}
function GetXToken(const S: string; Token: Char; X: Integer): string;
var
  I, J: Integer;
begin
  Dec(X);
  I := 1;

  // On boucle jusqu'à trouver la bonne occurence de Token
  while (X > 0) and (I <= Length(S)) do
  begin
    if S[I] = Token then
      Dec(X);
    Inc(I);
  end;

  // Si X est encore plus grand que 0, c'est qu'il n'y a pas assez d'occurences
  if X > 0 then
    Result := ''
  else
  begin
    J := I;
    while (J <= Length(S)) and (S[J] <> Token) do
      Inc(J);
    Result := Copy(S, I, J-I);
  end;
end;

{*
  Découpe une chaîne en mots et récupère le Xème mot
  Les mots sont délimités par des espaces uniquement.
  @param S       Chaîne à découper
  @param X       Index base sur 1 du mot à récupérer
  @return Le Xème mot de S (ou '' si X trop grand)
*}
function GetXWord(const S: string; X: Integer): string;
begin
  Result := GetXToken(S, ' ', X);
end;

{*
  Cherche la première occurence d'un mot dans une chaîne
  Les mots sont délimités par des espaces uniquement.
  @param Wrd     Mot à chercher
  @param Str     Chaîne dans laquelle chercher le mot
  @param Index   Position dans la chaîne à partir de laquelle chercher le mot
  @return La position du premier caractère du mot dans la chaîne
*}
function PosWord(const Wrd, Str: string; Index: Integer = 1): Integer;
begin
  Result := PosEx(' '+Wrd+' ', ' '+Str+' ', Index);
end;

type
  {*
    Classe privée TProtectedStringHash
    @author sjrd
    @version 1.0
  *}
  {$IFDEF FPC}
  TProtectedStringHash = class(TStringHashList)
  {$ELSE}
  TProtectedStringHash = class(TStringHash)
  {$ENDIF}
  public
    function HashOf(const Str: string): Cardinal; reintroduce;
  end;

{*
  [@inheritDoc]
*}
function TProtectedStringHash.HashOf(const Str: string): Cardinal;
begin
  Result := inherited HashOf(Str);
end;

{*
  Calcule le hash d'une chaîne de caractères selon l'algorithme de TStringHash
  @param Str   Chaîne de caractères
  @return Hash de Str selon l'algorithme utilisé par IniFiles.TStringHash
*}
function HashOfStr(const Str: string): Cardinal;
begin
  Result := TProtectedStringHash(nil).HashOf(Str);
end;

{*
  Identifie le numéro d'ère d'un numéro de version
  @param Version   Numéro de version sur lequel avancer
  @return Numéro d'ère
*}
function GetEra(var Version: PChar): Integer;
var
  ColonPos: PChar;
  EraString: string;
begin
  ColonPos := Version;
  while (ColonPos^ >= '0') and (ColonPos^ <= '9') do
    Inc(ColonPos);

  if ColonPos^ = ':' then
  begin
    SetString(EraString, Version, ColonPos-Version);
    Result := StrToInt(EraString);
    Version := ColonPos + 1;
  end else
    Result := 0;
end;

{*
  Identifie l'élément suivant d'un numéro de version
  @param Version   Numéro de version sur lequel avancer
  @return Valeur de l'élément suivant
*}
function GetNextVersionItem(var Version: PChar): Integer;
const
  vikTilde = $1000000;
  vikEmpty = $2000000;
  vikNumber = $3000000;
  vikChar = $4000000;
var
  SavedPos: PChar;
  NumberString: string;
begin
  case Version^ of
    '~':
    begin
      Result := vikTilde;
      Inc(Version);
    end;

    #0:
    begin
      Result := vikEmpty;
    end;

    '0'..'9':
    begin
      SavedPos := Version;
      repeat
        Inc(Version);
      until (Version^ < '0') or (Version^ > '9');

      SetString(NumberString, SavedPos, Version-SavedPos);
      Result := vikNumber + StrToInt(NumberString);
    end;
  else
    Result := vikChar + Ord(Version^);
    Inc(Version);
  end;
end;

{*
  Compare deux numéros de version selon les règles de comparaison GNU
  @param Left    Numéro de version de gauche
  @param Right   Numéro de version de droite
  @return -1 si Left < Right, 1 si Left > Right, 0 si Left = Right
*}
function CompareVersion(const Left, Right: string): TValueRelationship;
var
  LeftPos, RightPos: PChar;
  Diff: Integer;
begin
  LeftPos := PChar(Left);
  RightPos := PChar(Right);

  Diff := GetEra(LeftPos) - GetEra(RightPos);

  while (Diff = 0) and ((LeftPos^ <> #0) or (RightPos^ <> #0)) do
    Diff := GetNextVersionItem(LeftPos) - GetNextVersionItem(RightPos);

  if Diff < 0 then
    Result := -1
  else if Diff > 0 then
    Result := 1
  else
    Result := 0;
end;

end.

