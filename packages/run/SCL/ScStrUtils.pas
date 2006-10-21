{*
  Définit quelques routines de gestion de chaînes
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit ScStrUtils;

interface

type
  TCompareStrOption = (
    coIgnoreCase,        // Ignore la casse
    coIgnoreNonSpace,    // Ignore les accents
    coIgnoreSymbols);    // Ignore les symboles
  TCompareStrOptions = set of TCompareStrOption;

function NberSubStr(const SubStr, Str : string) : integer;
// Renvoie le nombre d'occurences de SubStr dans Str
// Attention ! Si vous recherchez '..' dans '...', vous en trouverez 2 !

function NberCharInStr(C : Char; const Str : string) : integer;
// Version optimisée de NberSubStr lorsqu'il ne faut chercher qu'un caractère

{$IFDEF MSWINDOWS}
function CompareStringEx(const S1, S2 : string;
  CompareOptions : TCompareStrOptions = []) : integer; platform;
// Compare deux chaînes suivant les options spécifiées par CompareOptions
{$ENDIF}

function GetFirstToken(const S : string; Token : Char) : string;
// Renvoie la première sous-chaîne de S délimitée par Token
// (si Token n'est pas dans S, renvoie S)
function GetLastToken(const S : string; Token : Char) : string;
// Renvoie la dernière sous-chaîne de S délimitée par Token
// (si Token n'est pas dans S, renvoie S)
function GetXToken(const S : string; Token : Char; X : integer) : string;
// Renvoie la Xème sous-chaîne de S délimitée par Token

function GetXWord(const S : string; X : integer) : string;
// Recherche le Xème mot dans S
// Attention ! L'appel GetXWord('Je m''appelle Sébastien', 2) renverra 'm''appelle' !

function PosWord(const Wrd, Str : string; Index : integer = 1) : integer;
// Renvoie la première position de Wrd dans Str à partir de la position Index
// Renvoie 0 si le mot n'est pas trouvé
// Attention ! L'appel PosWord('Chose', 'Chose-Machin Chouette Chose') renverra 23
//                                                             *
// De même que l'appel PosWord('revoir', 'Au revoir.') renverra 0 (non trouvé à cause du '.')

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  StrUtils;

function NberSubStr(const SubStr, Str : string) : integer;
var I : integer;
begin
  Result := 0;
  // On parcourt la chaîne et on incrémente Result si la correspondance est
  // établie
  for I := 1 to Length(Str)-Length(SubStr)+1 do
    if Copy(Str, I, Length(SubStr)) = SubStr then inc(Result);
end;

function NberCharInStr(C : Char; const Str : string) : integer;
var I : integer;
begin
  Result := 0;
  for I := 1 to Length(Str) do
    if Str[I] = C then inc(Result);
end;

{$IFDEF MSWINDOWS}
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

function GetFirstToken(const S : string; Token : Char) : string;
var I : integer;
begin
  I := 1;
  // On parcourt la chaîne jusqu'à trouver un caractère Token
  while (I <= Length(S)) and (S[I] <> Token) do inc(I);
  // On copie la chaîne depuis le début jusqu'au caractère avant Token
  Result := Copy(S, 1, I-1);
end;

function GetLastToken(const S : string; Token : Char) : string;
var I : integer;
begin
  I := Length(S);
  // On parcourt la chaîne à l'envers jusqu'à trouver un caractère Token
  while (I > 0) and (S[I] <> Token) do dec(I);
  // On copie la chaîne depuis le caractère après Token jusqu'à la fin
  Result := Copy(S, I+1, Length(S));
end;

function GetXToken(const S : string; Token : Char; X : integer) : string;
var I, J : integer;
begin
  dec(X);
  I := 1;
  // On boucle jusqu'à trouver la bonne occurence de Token
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

function GetXWord(const S : string; X : integer) : string;
begin
  Result := GetXToken(S, ' ', X);
end;

function PosWord(const Wrd, Str : string; Index : integer = 1) : integer;
begin
  Result := PosEx(' '+Wrd+' ', ' '+Str+' ', Index);
end;

end.
