{*
  D�finit quelques routines de gestion de cha�nes
  @author sjrd
  @version 1.0
*}
unit ScStrUtils;

interface

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

function RightPos(C : Char; const Str : string) : integer;

function NberSubStr(const SubStr, Str : string) : integer;
function NberCharInStr(C : Char; const Str : string) : integer;

{$IFDEF MSWINDOWS}
function CompareStringEx(const S1, S2 : string;
  CompareOptions : TCompareStrOptions = []) : integer; platform;
{$ENDIF}

function GetFirstToken(const S : string; Token : Char) : string;
function ExtractFirstToken(var S : string; Token : Char) : string;
function GetLastToken(const S : string; Token : Char) : string;
function ExtractLastToken(var S : string; Token : Char) : string;
function SplitToken(const S : string; Token : Char;
  out LeftStr, RightStr : string) : boolean;

function GetXToken(const S : string; Token : Char; X : integer) : string;
function GetXWord(const S : string; X : integer) : string;

function PosWord(const Wrd, Str : string; Index : integer = 1) : integer;

function HashOfStr(const Str : string) : Cardinal;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  StrUtils, IniFiles;

{*
  Cherche un caract�re dans une cha�ne � partir de la fin de celle-ci
  @param C     Caract�re � rechercher
  @param Str   Cha�ne dans laquelle rechercher C
  @return Index de la derni�re occurence de C dans Str, ou 0 si non trouv�
*}
function RightPos(C : Char; const Str : string) : integer;
begin
  Result := Length(Str);
  while Result > 0 do
    if Str[Result] = C then exit else dec(Result);
end;

{*
  Calcule le nombre d'occurences d'une sous-cha�ne dans une cha�ne
  @param SubStr   Sous-cha�ne � chercher
  @param Str      Cha�ne dans laquelle chercher SubStr
  @return Nombre d'occurences de SubStr dans Str
*}
function NberSubStr(const SubStr, Str : string) : integer;
var I : integer;
begin
  Result := 0;
  for I := Length(Str)-Length(SubStr)+1 downto 1 do
    if Copy(Str, I, Length(SubStr)) = SubStr then inc(Result);
end;

{*
  Calcule le nombre d'occurences d'un caract�re dans une cha�ne
  @param C     Caract�re � chercher
  @param Str   Cha�ne dans laquelle chercher C
  @return Nombre d'occurences de C dans Str
*}
function NberCharInStr(C : Char; const Str : string) : integer;
var I : integer;
begin
  Result := 0;
  for I := Length(Str) downto 1 do
    if Str[I] = C then inc(Result);
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
function CompareStringEx(const S1, S2 : string;
  CompareOptions : TCompareStrOptions = []) : integer;
var Flags : DWord;
begin
  Flags := 0;

  // On ajoute les flags de comparaison
  if coIgnoreCase     in CompareOptions then Flags := Flags+NORM_IGNORECASE;
  if coIgnoreNonSpace in CompareOptions then Flags := Flags+NORM_IGNORENONSPACE;
  if coIgnoreSymbols  in CompareOptions then Flags := Flags+NORM_IGNORESYMBOLS;

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
function GetFirstToken(const S : string; Token : Char) : string;
var I : integer;
begin
  I := 1;
  // On parcourt la cha�ne jusqu'� trouver un caract�re Token
  while (I <= Length(S)) and (S[I] <> Token) do inc(I);
  // On copie la cha�ne depuis le d�but jusqu'au caract�re avant Token
  Result := Copy(S, 1, I-1);
end;

{*
  Extrait la premi�re sous-cha�ne d'une cha�ne et la supprime de l'originale
  @param S       Cha�ne � d�couper
  @param Token   D�limiteur
  @return La premi�re sous-cha�ne de S d�limit�e par Token (ou S si non trouv�)
*}
function ExtractFirstToken(var S : string; Token : Char) : string;
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
function GetLastToken(const S : string; Token : Char) : string;
var I : integer;
begin
  I := Length(S);
  // On parcourt la cha�ne � l'envers jusqu'� trouver un caract�re Token
  while (I > 0) and (S[I] <> Token) do dec(I);
  // On copie la cha�ne depuis le caract�re apr�s Token jusqu'� la fin
  Result := Copy(S, I+1, MaxInt);
end;

{*
  Extrait la derni�re sous-cha�ne d'une cha�ne et la supprime de l'originale
  @param S       Cha�ne � d�couper
  @param Token   D�limiteur
  @return La derni�re sous-cha�ne de S d�limit�e par Token (ou S si non trouv�)
*}
function ExtractLastToken(var S : string; Token : Char) : string;
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
  @param RightStr   Sous-cha�ne de droite (ou S si non trouv�)
  @return True si la cha�ne a �t� s�par�e, False si Token n'est pas trouv�
*}
function SplitToken(const S : string; Token : Char;
  out LeftStr, RightStr : string) : boolean;
var Len, I : integer;
begin
  Len := Length(S);

  // Recherche de la premi�re occurence de Token
  I := 1;
  while (I <= Len) and (S[I] <> Token) do inc(I);
  Result := I <= Len;

  if Result then
  begin
    // Trouv� : s�parer LeftStr et RightStr
    LeftStr := Copy(S, 1, I-1);
    while (I <= Len) and (S[I] = Token) do inc(I);
    RightStr := Copy(S, I, MaxInt);
  end else
  begin
    // Non trouv� : LeftStr = RightStr = S
    LeftStr := S;
    RightStr := S;
  end;
end;

{*
  D�coupe une cha�ne selon un d�limiteur et r�cup�re la X�me sous-cha�ne
  @param S       Cha�ne � d�couper
  @param Token   D�limiteur
  @param X       Index base sur 1 de la sous-cha�ne � r�cup�rer
  @return La X�me sous-cha�ne de S d�limit�e par Token (ou '' si X trop grand)
*}
function GetXToken(const S : string; Token : Char; X : integer) : string;
var I, J : integer;
begin
  dec(X);
  I := 1;

  // On boucle jusqu'� trouver la bonne occurence de Token
  while (X > 0) and (I <= Length(S)) do
  begin
    if S[I] = Token then dec(X);
    inc(I);
  end;

  // Si X est encore plus grand que 0, c'est qu'il n'y a pas assez d'occurences
  if X > 0 then Result := '' else
  begin
    J := I;
    while (J <= Length(S)) and (S[J] <> Token) do inc(J);
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
function GetXWord(const S : string; X : integer) : string;
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
function PosWord(const Wrd, Str : string; Index : integer = 1) : integer;
begin
  Result := PosEx(' '+Wrd+' ', ' '+Str+' ', Index);
end;

type
  {*
    Classe priv�e TProtectedStringHash
    @author sjrd
    @version 1.0
  *}
  TProtectedStringHash = class(TStringHash)
  public
    function HashOf(const Str : string) : Cardinal; reintroduce;
  end;

{*
  [@inheritDoc]
*}
function TProtectedStringHash.HashOf(const Str : string) : Cardinal;
begin
  Result := inherited HashOf(Str);
end;

{*
  Calcule le hash d'une cha�ne de caract�res selon l'algorithme de TStringHash
  @param Str   Cha�ne de caract�res
  @return Hash de Str selon l'algorithme utilis� par IniFiles.TStringHash
*}
function HashOfStr(const Str : string) : Cardinal;
begin
  Result := TProtectedStringHash(nil).HashOf(Str);
end;

end.

