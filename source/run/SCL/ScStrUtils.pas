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
  D�finit quelques routines de gestion de cha�nes
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
    Option de comparaison de cha�nes
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
    Ensemble d'options de comparaison de cha�nes
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
  Cherche un caract�re dans une cha�ne � partir de la fin de celle-ci
  @param C     Caract�re � rechercher
  @param Str   Cha�ne dans laquelle rechercher C
  @return Index de la derni�re occurence de C dans Str, ou 0 si non trouv�
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
  Calcule le nombre d'occurences d'une sous-cha�ne dans une cha�ne
  @param SubStr   Sous-cha�ne � chercher
  @param Str      Cha�ne dans laquelle chercher SubStr
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
  Calcule le nombre d'occurences d'un caract�re dans une cha�ne
  @param C     Caract�re � chercher
  @param Str   Cha�ne dans laquelle chercher C
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
  Compare deux cha�nes de caract�res avec des options avanc�es
  @param S1               Premi�re cha�ne
  @param S2               Seconde cha�ne
  @param CompareOptions   Options de comparaison
  @return 0 si les cha�nes sont semblables, un nombre positif si la premi�re
          est sup�rieure � la seconde, un nombre n�gatif dans le cas inverse
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
  D�coupe une cha�ne selon un d�limiteur et r�cup�re la premi�re sous-cha�ne
  @param S       Cha�ne � d�couper
  @param Token   D�limiteur
  @return La premi�re sous-cha�ne de S d�limit�e par Token (ou S si non trouv�)
*}
function GetFirstToken(const S: string; Token: Char): string;
var
  I: Integer;
begin
  I := 1;
  // On parcourt la cha�ne jusqu'� trouver un caract�re Token
  while (I <= Length(S)) and (S[I] <> Token) do
    Inc(I);
  // On copie la cha�ne depuis le d�but jusqu'au caract�re avant Token
  Result := Copy(S, 1, I-1);
end;

{*
  Extrait la premi�re sous-cha�ne d'une cha�ne et la supprime de l'originale
  @param S       Cha�ne � d�couper
  @param Token   D�limiteur
  @return La premi�re sous-cha�ne de S d�limit�e par Token (ou S si non trouv�)
*}
function ExtractFirstToken(var S: string; Token: Char): string;
begin
  Result := GetFirstToken(S, Token);
  Delete(S, 1, Length(Result)+1);
end;

{*
  D�coupe une cha�ne selon un d�limiteur et r�cup�re la derni�re sous-cha�ne
  @param S       Cha�ne � d�couper
  @param Token   D�limiteur
  @return La derni�re sous-cha�ne de S d�limit�e par Token (ou S si non trouv�)
*}
function GetLastToken(const S: string; Token: Char): string;
var
  I: Integer;
begin
  I := Length(S);
  // On parcourt la cha�ne � l'envers jusqu'� trouver un caract�re Token
  while (I > 0) and (S[I] <> Token) do
    Dec(I);
  // On copie la cha�ne depuis le caract�re apr�s Token jusqu'� la fin
  Result := Copy(S, I+1, MaxInt);
end;

{*
  Extrait la derni�re sous-cha�ne d'une cha�ne et la supprime de l'originale
  @param S       Cha�ne � d�couper
  @param Token   D�limiteur
  @return La derni�re sous-cha�ne de S d�limit�e par Token (ou S si non trouv�)
*}
function ExtractLastToken(var S: string; Token: Char): string;
begin
  Result := GetLastToken(S, Token);
  Delete(S, Length(S)-Length(Result)-1, MaxInt);
end;

{*
  D�coupe une cha�ne en deux selon un d�limiteur
  Si plusieurs d�limiteurs cons�cutifs sont trouv�s, ils sont consid�r�s comme
  un seul. Mais s'ils sont s�par�s, seul le premier groupe prendra effet.
  @param S          Cha�ne � d�couper
  @param Token      D�limiteur
  @param LeftStr    Sous-cha�ne de gauche (ou S si non trouv�)
  @param RightStr   Sous-cha�ne de droite (ou une cha�ne vide si non trouv�)
  @return True si la cha�ne a �t� s�par�e, False si Token n'est pas trouv�
*}
function SplitToken(const S: string; Token: Char;
  out LeftStr, RightStr: string): Boolean;
var
  Len, I: Integer;
begin
  Len := Length(S);

  // Recherche de la premi�re occurence de Token
  I := 1;
  while (I <= Len) and (S[I] <> Token) do
    Inc(I);
  Result := I <= Len;

  if Result then
  begin
    // Trouv� : s�parer LeftStr et RightStr
    LeftStr := Copy(S, 1, I-1);
    while (I <= Len) and (S[I] = Token) do
      Inc(I);
    RightStr := Copy(S, I, MaxInt);
  end else
  begin
    // Non trouv� : LeftStr = S et RightStr = ''
    LeftStr := S;
    RightStr := '';
  end;
end;

{*
  D�coupe une cha�ne en morceaux selon un d�limiteur
  @param S          Cha�ne � d�couper
  @param Token      D�limiteur
  @return Tableau des morceaux de la cha�ne
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
  D�coupe une cha�ne selon un d�limiteur et r�cup�re la X�me sous-cha�ne
  @param S       Cha�ne � d�couper
  @param Token   D�limiteur
  @param X       Index base sur 1 de la sous-cha�ne � r�cup�rer
  @return La X�me sous-cha�ne de S d�limit�e par Token (ou '' si X trop grand)
*}
function GetXToken(const S: string; Token: Char; X: Integer): string;
var
  I, J: Integer;
begin
  Dec(X);
  I := 1;

  // On boucle jusqu'� trouver la bonne occurence de Token
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
  D�coupe une cha�ne en mots et r�cup�re le X�me mot
  Les mots sont d�limit�s par des espaces uniquement.
  @param S       Cha�ne � d�couper
  @param X       Index base sur 1 du mot � r�cup�rer
  @return Le X�me mot de S (ou '' si X trop grand)
*}
function GetXWord(const S: string; X: Integer): string;
begin
  Result := GetXToken(S, ' ', X);
end;

{*
  Cherche la premi�re occurence d'un mot dans une cha�ne
  Les mots sont d�limit�s par des espaces uniquement.
  @param Wrd     Mot � chercher
  @param Str     Cha�ne dans laquelle chercher le mot
  @param Index   Position dans la cha�ne � partir de laquelle chercher le mot
  @return La position du premier caract�re du mot dans la cha�ne
*}
function PosWord(const Wrd, Str: string; Index: Integer = 1): Integer;
begin
  Result := PosEx(' '+Wrd+' ', ' '+Str+' ', Index);
end;

type
  {*
    Classe priv�e TProtectedStringHash
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
  Calcule le hash d'une cha�ne de caract�res selon l'algorithme de TStringHash
  @param Str   Cha�ne de caract�res
  @return Hash de Str selon l'algorithme utilis� par IniFiles.TStringHash
*}
function HashOfStr(const Str: string): Cardinal;
begin
  Result := TProtectedStringHash(nil).HashOf(Str);
end;

{*
  Identifie le num�ro d'�re d'un num�ro de version
  @param Version   Num�ro de version sur lequel avancer
  @return Num�ro d'�re
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
  Identifie l'�l�ment suivant d'un num�ro de version
  @param Version   Num�ro de version sur lequel avancer
  @return Valeur de l'�l�ment suivant
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
  Compare deux num�ros de version selon les r�gles de comparaison GNU
  @param Left    Num�ro de version de gauche
  @param Right   Num�ro de version de droite
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

