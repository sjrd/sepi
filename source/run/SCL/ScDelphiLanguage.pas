{*
  Définit quelques routines d'utilisation plus rare ou avancée
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit ScDelphiLanguage;

interface

uses
  SysUtils, TypInfo, ScUtils;

function CorrectIdentifier(const Ident : string) : boolean;

procedure SetBit(var Value : integer; const Bit : Byte); register;
procedure ClearBit(var Value : integer; const Bit : Byte); register;
procedure ToggleBit(var Value : integer; const Bit : Byte); register;
function TestBit(const Value : integer; const Bit : Byte) : boolean; register;

function GetMethodFromName(Obj : TObject;
  const MethodName : ShortString) : TMethod;

function StrToStrRepres(const Str : string;
  ExcludedChars : TSysCharSet = []) : string;
function StrRepresToStr(Str : string) : string;

function CharToCharRepres(Chr : Char;
  ExcludedChars : TSysCharSet = []) : string;
function CharRepresToChar(Str : string) : Char;

function CharSetToStr(const CharSet : TSysCharSet) : string;
function StrToCharSet(Str : string) : TSysCharSet;

function EnumSetToStr(const EnumSet; TypeInfo : PTypeInfo) : string;
procedure StrToEnumSet(const Str : string; TypeInfo : PTypeInfo; out EnumSet);

procedure ExplicitInitialize(var Value; TypeInfo : PTypeInfo;
  Count : Cardinal = 1);
procedure ExplicitFinalize(var Value; TypeInfo : PTypeInfo;
  Count : Cardinal = 1);

function SkipPackedShortString(Value : PShortstring) : Pointer;

implementation

uses
  ScConsts;

{*
  Vérifie si une chaîne de caractères est un identificateur Pascal correct
  @param Ident   Chaîne de caractères à tester
  @return True si Ident est un identificateur Pascal correct, False sinon
*}
function CorrectIdentifier(const Ident : string) : boolean;
var I : integer;
begin
  Result := False;

  // Si Ident est vide, ce n'est un indentificateur correct
  if Ident = '' then exit;

  { Si le premier caractère n'est pas alphabétique,
    ce n'est pas un identificateur correct }
  if not (Ident[1] in ['A'..'Z', '_', 'a'..'z']) then exit;

  { Si l'un des caractères suivants n'est pas alphanumérique,
    ce n'est pas un identificateur correct }
  for I := 2 to Length(Ident) do
    if not (Ident[I] in ['0'..'9', 'A'..'Z', '_', 'a'..'z']) then exit;

  // Dans les autres cas, ça l'est
  Result := True;
end;

{*
  Positionne un bit à 1
  @param Value   Valeur à modifier
  @param Bit     Index du bit à modifier
  @author waskol
*}
procedure SetBit(var Value : integer; const Bit : Byte); register;
asm
        BTS     [EAX],EDX
end;

{*
  Positionne un bit à 0
  @param Value   Valeur à modifier
  @param Bit     Index du bit à modifier
  @author waskol
*}
procedure ClearBit(var Value : integer; const Bit : Byte); register;
asm
        BTR     [EAX],EDX
end;

{*
  Inverse un bit
  @param Value   Valeur à modifier
  @param Bit     Index du bit à modifier
  @author waskol
*}
procedure ToggleBit(var Value : integer; const Bit : Byte); register;
asm
        BTC     [EAX],EDX
end;

{*
  Teste la valeur d'un bit
  @param Value   Valeur à tester
  @param Bit     Index du bit à tester
  @return True si le bit est à 1, False sinon
  @author waskol
*}
function TestBit(const Value : integer; const Bit : Byte) : boolean; register;
asm
        BT      EAX,EDX
        SETB    AL
end;

{*
  Recherche une méthode d'un objet à partir de son nom
  La méthode en question doit être publiée pour pouvoir être trouvée.
  @param Obj          L'objet définissant la méthode
  @param MethodName   Nom de la méthode
  @return Une référence à la méthode recherchée pour l'objet Obj
*}
function GetMethodFromName(Obj : TObject;
  const MethodName : ShortString) : TMethod;
begin
  Result.Code := Obj.MethodAddress(MethodName);
  Result.Data := Obj;
end;

{*
  Détermine la représentation Pascal d'une chaîne de caractères
  Cette représentation est la chaîne encadrée de guillemets simples ('), dont
  ces caractères à l'intérieur de la chaîne sont doublés, et dont certains
  caractères spéciaux sont échappés au moyen de #.
  Cette chaîne peut alors être par exemple insérée dans un code Pascal.
  @param Str             Chaîne à traiter
  @param ExcludedChars   Ensemble des caractères qu'il faut échapper
  @return Représentation Pascal de Str
*}
function StrToStrRepres(const Str : string;
  ExcludedChars : TSysCharSet = []) : string;
var I : integer;
begin
  if Str = '' then Result := '''''' else
  begin
    I := 1;
    Result := '';
    ExcludedChars := ExcludedChars + [#0..#31];
    while I <= Length(Str) do
    begin
      if Str[I] in ExcludedChars then
      begin
        if I mod 256 = 0 then
          Result := Result+'+';

        Result := Result+'#'+IntToStr(Byte(Str[I]));
        inc(I);
      end else
      begin
        Result := Result+'''';
        while (I <= Length(Str)) and (not (Str[I] in ExcludedChars)) do
        begin
          if I mod 256 = 0 then
            Result := Result+'''+''';

          if Str[I] = '''' then Result := Result + '''''' else
            Result := Result + Str[I];
          inc(I);
        end;
        Result := Result+'''';
      end;
    end;
  end;
end;

{*
  Détermine une chaîne à partir de sa représentation Pascal
  Cette représentation est la chaîne encadrée de guillemets simples ('), dont
  ces caractères à l'intérieur de la chaîne sont doublés, et dont certains
  caractères spéciaux sont échappés au moyen de #.
  Cette chaîne peut par exemple être extraite d'un code Pascal.
  @param Str   Chaîne à traiter
  @return Chaîne représentée par Str en Pascal
  @raise EConvertError Chaîne de caractère incorrecte
*}
function StrRepresToStr(Str : string) : string;
var CharStr : string;
    I, IntChar : integer;
begin
  Result := '';
  Str := Trim(Str);
  I := 1;
  repeat
    if I > 1 then inc(I);
    
    while (I <= Length(Str)) and ((Str[I] = '''') or (Str[I] = '#')) do
    begin
      if Str[I] = '''' then
      begin
        inc(I);
        while True do
        begin
          if I > Length(Str) then
            raise EConvertError.CreateFmt(sScWrongString, [Str]);
          if Str[I] = '''' then
          begin
            inc(I);
            if (I <= Length(Str)) and (Str[I] = '''') then
            begin
              Result := Result+'''';
              inc(I);
            end else Break;
          end else
          begin
            Result := Result+Str[I];
            inc(I);
          end;
        end;
      end else
      begin
        inc(I);
        if I > Length(Str) then
          raise EConvertError.CreateFmt(sScWrongString, [Str]);
        CharStr := '';
        while (I <= Length(Str)) and (Str[I] in ['0'..'9']) do
        begin
          CharStr := CharStr+Str[I];
          inc(I);
        end;
        IntChar := StrToIntDef(CharStr, -1);
        if (IntChar >= 0) and (IntChar <= 255) then
          Result := Result+Char(IntChar)
        else
          raise EConvertError.CreateFmt(sScWrongString, [Str]);
      end;
    end;
  until (I > Length(Str)) or (Str[I] <> '+');
  if I <= Length(Str) then
    raise EConvertError.CreateFmt(sScWrongString, [Str]);
end;

{*
  Détermine la représentation Pascal d'un caractère
  @param Chr             Caractère à traiter
  @param ExcludedChars   Ensemble des caractères qu'il faut échapper
  @return Représentation Pascal de Chr
*}
function CharToCharRepres(Chr : Char;
  ExcludedChars : TSysCharSet = []) : string;
begin
  ExcludedChars := ExcludedChars + [#0..#31];
  if Chr in ExcludedChars then Result := '#'+IntToStr(Byte(Chr)) else
  if Chr = '''' then Result := '''''''''' else
  Result := ''''+Chr+'''';
end;

{*
  Détermine un caractère à partir de sa représentation Pascal
  @param Str   Chaîne à traiter
  @return Caractère représenté par Str en Pascal
  @raise EConvertError Caractère incorrect
*}
function CharRepresToChar(Str : string) : Char;
begin
  try
    Str := Trim(Str);
    if Str = '' then raise EConvertError.Create('');
    case Str[1] of
      '#' :
      begin
        // Le résultat est le caractère dont le code ASCII est l'entier
        // spécifié à la suite
        Delete(Str, 1, 1);
        Result := Chr(StrToInt(Str));
      end;
      '''' :
      begin
        case Length(Str) of
          // Si 3 caractères, le troisième doit être ' et le deuxième
          // est le caractère résultat
          3 : if Str[3] = '''' then Result := Str[2] else
            raise EConvertError.Create('');
          // Si 4 caractères, ce doit être '''', auquel cas le caractère
          // retour est '
          4 : if Str = '''''''''' then Result := '''' else
            raise EConvertError.Create('');
          // Sinon, ce n'est pas un caractère correct
          else raise EConvertError.Create('');
        end;
      end;
      else raise EConvertError.Create('');
    end;
  except
    on Error : EConvertError do
      raise EConvertError.CreateFmt(sSjrdWrongChar, [Str]);
  end;
end;

{*
  Détermine la représentation Pascal d'un ensemble de caractères (sans les [])
  @param CharSet   Ensemble de caractères à traiter
  @return Représentation Pascal de CharSet
*}
function CharSetToStr(const CharSet : TSysCharSet) : string;
var I, From : Word;
begin
  Result := '';
  I := 0;
  // On cherche d'abord le premier caractère inclus
  while (I <= 255) and (not (Chr(I) in CharSet)) do inc(I);
  while I <= 255 do
  begin
    // Chr(I) est inclus
    From := I;
    // On cherche le caractère suivant qui n'est pas inclus
    while (I <= 255) and (Chr(I) in CharSet) do inc(I);
    // On teste I-From, soit le nombre de caractère consécutifs
    case I-From of
      // 1 : on ajoute simplement ce caractère
      1 : Result := Result+', '+CharToCharRepres(Chr(From));
      // 2 : on ajoute ces deux caractères séparés par des virgules
      2 : Result := Result+', '+CharToCharRepres(Chr(From))+
        ', '+CharToCharRepres(Chr(I-1));
      // 3+ : on ajoute les deux extrèmes séparés par ..
      else Result := Result+', '+CharToCharRepres(Chr(From))+
        '..'+CharToCharRepres(Chr(I-1));
    end;
    // on cherche le caractère suivant inclus
    repeat inc(I) until (I > 255) or (Chr(I) in CharSet);
  end;
  // On supprime les deux premiers caractères, car ce sont ', '
  Delete(Result, 1, 2);
end;

{*
  Détermine un ensemble de caractères à partir de sa représentation Pascal
  @param Str   Chaîne à traiter
  @return Ensemble de caractères représenté par CharSet
  @raise EConvertError Ensemble de caractères incorrect
*}
function StrToCharSet(Str : string) : TSysCharSet;
var I : integer;
  function GetCharAt : Char;
  // Renvoie le caractère à la position courante et augmente I en conséquence
  // Fonctionne sur le même principe que CharRepresToChar
  var From : integer;
  begin
    case Str[I] of
      '#' :
      begin
        From := I+1;
        repeat inc(I) until (I > Length(Str)) or (not (Str[I] in ['0'..'9']));
        Result := Chr(StrToInt(Copy(Str, From, I-From)));
      end;
      '''' :
      begin
        inc(I);
        if I > Length(Str) then
          raise EConvertError.Create('');
        if Str[I] = '''' then
        begin
          if I+2 > Length(Str) then
            raise EConvertError.Create('');
          if (Str[I+1] <> '''') or (Str[I+2] <> '''') then
            raise EConvertError.Create('');
          Result := '''';
          inc(I, 3);
        end else
        begin
          if I+1 > Length(Str) then
            raise EConvertError.Create('');
          if Str[I+1] <> '''' then
            raise EConvertError.Create('');
          Result := Str[I];
          inc(I, 2);
        end;
      end;
      else raise EConvertError.Create('');
    end;
  end;
var C1, C2 : Char;
begin
  try
    Result := [];
    Str := Trim(Str);
    // Si Str est vide, il n'y a aucun caractère dans l'ensemble
    if Str = '' then exit;
    // Si il y des [] aux extrémités, on les supprime
    if (Str[1] = '[') and (Str[Length(Str)] = ']') then
      Str := Trim(Copy(Str, 2, Length(Str)-2));

    I := 1;
    while I <= Length(Str) do
    begin
      // On récupère le caractère à la position courante
      C1 := GetCharAt;
      // On passe tous les espaces
      while (I <= Length(Str)) and (Str[I] = ' ') do inc(I);

      // Si I > Length(Str), on ajoute le caractère et on arrête
      if I > Length(Str) then
      begin
        Include(Result, C1);
        Break;
      end;

      // Si Str[I] = ',', on ajoute le caractère et on passe la virgule
      if Str[I] = ',' then
      begin
        // On ajoute le caractère
        Include(Result, C1);
        // On passe la virgule et les espaces
        repeat inc(I) until (I > Length(Str)) or (Str[I] <> ' ');
        // Si on a atteint la fin de la chaîne, il y a une erreur
        // (on termine par une virgule)
        if I > Length(Str) then
          raise EConvertError.Create('');
        Continue;
      end;

      // Si Str[I] = '.', ce doit être une plage de caractères
      if Str[I] = '.' then
      begin
        // On teste si le caractère suivant est aussi un point
        inc(I);
        if (I > Length(Str)) or (Str[I] <> '.') then
          raise EConvertError.Create('');
        // On passe ce point et les espaces
        repeat inc(I) until (I > Length(Str)) or (Str[I] <> ' ');
        // On récupère le deuxième caractère
        C2 := GetCharAt;
        // On passe les espaces
        while (I <= Length(Str)) and (Str[I] = ' ') do inc(I);

        // Si I > Length(Str), on ajoute la plage de caractère et on termine
        if I > Length(Str) then
        begin
          Result := Result+[C1..C2];
          Break;
        end;

        // Si Str[I] = ',', on ajoute les caractères et on passe la virgule
        if Str[I] = ',' then
        begin
          // On ajoute la plage de caractères
          Result := Result+[C1..C2];
          // On passe la virgule et les espaces
          repeat inc(I) until (I > Length(Str)) or (Str[I] <> ' ');
          // Si on a atteint la fin de la chaîne, il y a une erreur
          // (on termine par une virgule)
          if I > Length(Str) then
            raise EConvertError.Create('');
          Continue;
        end;
        raise EConvertError.Create('');
      end;
      raise EConvertError.Create('');
    end;
  except
    on Error : EConvertError do
      raise EConvertError.CreateFmt(sScWrongCharSet, [Str]);
  end;
end;

{*
  Convertit un type ensemble d'éléments d'énumération en chaîne
  La représentation est celle du langage Pascal, sans les [].
  Cette routine fonctionne également pour les ensembles d'entiers.
  @param EnumSet    Ensemble à convertir
  @param TypeInfo   RTTI du type ensemble ou du type énumération
  @return Chaîne représentant l'ensemble EnumSet
*}
function EnumSetToStr(const EnumSet; TypeInfo : PTypeInfo) : string;
var TypeData : PTypeData;
    ByteValue : Byte;
begin
  if TypeInfo.Kind = tkSet then
    TypeInfo := GetTypeData(TypeInfo).CompType^;
  TypeData := GetTypeData(TypeInfo);

  Result := '';

  for ByteValue := TypeData.MinValue to TypeData.MaxValue do
  begin
    if ByteValue in TSysByteSet(EnumSet) then
      Result := Result + GetEnumName(TypeInfo, ByteValue) + ', ';
  end;

  if Result <> '' then
    SetLength(Result, Length(Result)-2);
end;

{*
  Convertit une chaîne en type ensemble d'éléments d'énumération
  La représentation est celle du langage Pascal, avec ou sans les [].
  Cette routine fonctionne également pour les ensembles d'entiers.
  @param Str        Chaîne à convertir
  @param TypeInfo   RTTI du type ensemble ou du type énumération
  @param EnumSet    Ensemble converti en sortie
  @return Chaîne représentant l'ensemble EnumSet
*}
procedure StrToEnumSet(const Str : string; TypeInfo : PTypeInfo; out EnumSet);
type
  TSetAsBytes = array[0..31] of Byte;
var SetName : string;
    TypeData : PTypeData;
    SetStr : string;
    Index, Len, BeginIndex, Value : integer;
begin
  if TypeInfo.Kind = tkSet then
  begin
    SetName := TypeInfo.Name;
    TypeInfo := GetTypeData(TypeInfo).CompType^;
  end else SetName := Format(sScSetOf, [TypeInfo.Name]);
  TypeData := GetTypeData(TypeInfo);

  Len := TypeData.MaxValue div 8 + 1;
  FillChar(EnumSet, Len, 0);

  try
    Index := 1;
    Len := Length(Str);
    if (Str <> '') and (Str[1] = '[') and (Str[Len] = ']') then
    begin
      SetStr := ',' + Copy(Str, 2, Len-2);
      dec(Len, 1);
    end else
    begin
      SetStr := ',' + Str;
      inc(Len, 1);
    end;

    while Index <= Len do
    begin
      if SetStr[Index] <> ',' then
        raise Exception.Create('');
      inc(Index);

      while (Index <= Len) and (SetStr[Index] in [' ', #13, #10]) do
        inc(Index);
      BeginIndex := Index;
      while (Index <= Len) and (not (SetStr[Index] in [',', ' ', #13, #10])) do
        inc(Index);

      Value := GetEnumValue(TypeInfo,
        Copy(SetStr, BeginIndex, Index-BeginIndex));
      if Value < 0 then
        raise Exception.Create('');
      Include(TSysByteSet(EnumSet), Value);

      while (Index <= Len) and (SetStr[Index] in [' ', #13, #10]) do inc(Index);
    end;
  except
    raise EConvertError.CreateFmt(sScWrongEnumSet, [Str, SetName]);
  end;
end;

{*
  Initialise une variable
  Cette routine n'est rien de plus qu'un moyen d'appeler explicitement la
  routine cachée _InitializeArray de l'unité System.
  @param Value      Variable à initialiser
  @param TypeInfo   RTTI du type de la variable
  @param Count      Nombre d'éléments dans la variable
*}
procedure ExplicitInitialize(var Value; TypeInfo : PTypeInfo;
  Count : Cardinal = 1);
asm
        JMP     System.@InitializeArray
end;

{*
  Finalise une variable
  Cette routine n'est rien de plus qu'un moyen d'appeler explicitement la
  routine cachée _FinalizeArray de l'unité System.
  @param Value      Variable à finaliser
  @param TypeInfo   RTTI du type de la variable
  @param Count      Nombre d'éléments dans la variable
*}
procedure ExplicitFinalize(var Value; TypeInfo : PTypeInfo;
  Count : Cardinal = 1);
asm
        JMP     System.@FinalizeArray
end;

{*
  Renvoie un pointeur vers le champ suivant un ShortString compactée (des RTTI)
  Cette routine peut être utilisée pour passer « au-dessus » d'une ShortString
  compactée, telle qu'on peut en trouver dans les record extra-compactés des
  RTTI.
  @param Value    Adresse de la ShortString compactée
  @return Adresse du champ suivant
*}
function SkipPackedShortString(Value : PShortstring) : Pointer;
asm
        { ->    EAX Pointer to type info }
        { <-    EAX Pointer to type data }
        XOR     EDX,EDX
        MOV     DL,[EAX]
        LEA     EAX,[EAX].Byte[EDX+1]
end;

end.

