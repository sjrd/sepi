{*
  Définit quelques routines de gestion de chaînes
  @author sjrd
  @version 1.0
*}
unit ScStrUtils;

interface

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

function GetXToken(const S: string; Token: Char; X: Integer): string;
function GetXWord(const S: string; X: Integer): string;

function PosWord(const Wrd, Str: string; Index: Integer = 1): Integer;

function HashOfStr(const Str: string): Cardinal;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  StrUtils, IniFiles;

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
    if Str[Result] = C then exit else Dec(Result);
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
    if Copy(Str, I, Length(SubStr)) = SubStr then Inc(Result);
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
    if Str[I] = C then Inc(Result);
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
  if coIgnoreCase     in CompareOptions then Flags := Flags+NORM_IGNORECASE;
  if coIgnoreNonSpace in CompareOptions then
    Flags := Flags+NORM_IGNORENONSPACE;
  if coIgnoreSymbols  in CompareOptions then Flags := Flags+NORM_IGNORESYMBOLS;

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
  while (I <= Length(S)) and (S[I] <> Token) do Inc(I);
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
  while (I > 0) and (S[I] <> Token) do Dec(I);
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
  @param RightStr   Sous-chaîne de droite (ou S si non trouvé)
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
  while (I <= Len) and (S[I] <> Token) do Inc(I);
  Result := I <= Len;

  if Result then
  begin
    // Trouvé : séparer LeftStr et RightStr
    LeftStr := Copy(S, 1, I-1);
    while (I <= Len) and (S[I] = Token) do Inc(I);
    RightStr := Copy(S, I, MaxInt);
  end else
  begin
    // Non trouvé : LeftStr = RightStr = S
    LeftStr := S;
    RightStr := S;
  end;
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
    if S[I] = Token then Dec(X);
    Inc(I);
  end;

  // Si X est encore plus grand que 0, c'est qu'il n'y a pas assez d'occurences
  if X > 0 then Result := '' else
  begin
    J := I;
    while (J <= Length(S)) and (S[J] <> Token) do Inc(J);
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
  TProtectedStringHash = class(TStringHash)
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

end.

