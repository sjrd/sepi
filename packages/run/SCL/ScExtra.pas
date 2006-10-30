{*
  Définit quelques routines d'utilisation plus rare ou avancée
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit ScExtra;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, ComObj, ShlObj, ActiveX,
{$ENDIF}
  SysUtils, Classes, ZLib, ScUtils, ScConsts;

type
  {*
    Déclenchée lors d'une erreur de mise à jour des ressources d'un exécutable
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  EUpdateResError = class(EAPIError) end platform;

  {$IFDEF MSWINDOWS}
  {*
    Type de son
    stFileName : Nom de fichier
    stResource : Nom d'une ressource
    stSysSound : Nom d'un son système
  *}
  TSoundType = (stFileName, stResource, stSysSound);
  {$ENDIF}

function CorrectIdentifier(const Ident : string) : boolean;

function IntToBase(Value : integer; Base : Byte = 10) : string;
function BaseToInt(const Value : string; Base : Byte = 10) : integer;
function BaseToIntDef(const Value : string; Default : integer = 0;
  Base : Byte = 10) : integer;

function GetMethodFromName(Obj : TObject; MethodName : ShortString) : TMethod;

function ConvertDoubleToInt64(Value : Double) : Int64;
function ConvertInt64ToDouble(Value : Int64) : Double;

function StrToStrRepres(const Str : string;
  ExcludedChars : TSysCharSet = []) : string;
function StrRepresToStr(Str : string) : string;

function CharToCharRepres(Chr : Char;
  ExcludedChars : TSysCharSet = []) : string;
function CharRepresToChar(Str : string) : Char;

function CharSetToStr(CharSet : TSysCharSet) : string;
function StrToCharSet(Str : string) : TSysCharSet;

{$IFDEF MSWINDOWS}
procedure CreateShellLink(const Source, Dest : string;
                          const Description : string = '';
                          const IconLocation : string = '';
                          IconIndex : integer = 0;
                          const Arguments : string = '';
                          const WorkDir : string = '';
                          ShowCommand : integer = SW_SHOW); platform;
{$ENDIF}

{$IFDEF MSWINDOWS}
function ExecuteSound(const Sound : string; SoundType : TSoundType = stFileName;
                      Synchronous : boolean = False; Module : HMODULE = 0;
                      AddFlags : LongWord = 0) : boolean; platform;
{$ENDIF}

{$REGION 'Modification des ressources'}

{$IFDEF MSWINDOWS}
function BeginUpdateRes(const FileName : string) : integer; platform;
procedure AddResource(ResHandle : integer; const ResName : string;
  Resource : TStream; const ResType : string = 'RT_RCDATA'); platform;
procedure DelResource(ResHandle : integer; const ResName : string); platform;
procedure EndUpdateRes(ResHandle : integer; Cancel : boolean = False); platform;
procedure AddResToFile(const FileName, ResName : string; Resource : TStream;
  const ResType : string = 'RT_RCDATA'); platform;
procedure DelResInFile(const FileName, ResName : string); platform;
{$ENDIF}

{$ENDREGION}

{$REGION 'Compression/décompression'}

const
  clNoComp      = ZLib.clNone;    /// pas de compression
  clFastestComp = ZLib.clFastest; /// compression la plus rapide
  clDefaultComp = ZLib.clDefault; /// compression par défaut
  clMaxComp     = ZLib.clMax;     /// compression maximale

procedure CompressStream(Stream : TStream; Dest : TStream = nil;
  CompressionLevel : TCompressionLevel = clDefaultComp);
procedure DecompressStream(Stream : TStream; Dest : TStream = nil);

{$ENDREGION}

implementation

uses
{$IFDEF MSWINDOWS}
  MMSystem;
{$ENDIF}

const
  NumbersStr = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'; /// Chiffres des bases
  MaxBase = 36;                                        /// Base maximale

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

  { Si l'un des caractères suivant n'est pas alphanumérique,
    ce n'est pas un identificateur correct }
  for I := 2 to Length(Ident) do
    if not (Ident[I] in ['0'..'9', 'A'..'Z', '_', 'a'..'z']) then exit;

  // Dans les autres cas, ça l'est
  Result := True;
end;

{*
  Vérifie qu'une base est valide
  @param Base   Base à tester
  @raise EConvertError Base incorrecte
*}
procedure VerifyBase(Base : Byte);
begin
  if (Base < 2) or (Base > MaxBase) then
    raise EConvertError.CreateFmt(sScWrongBase, [Base]);
end;

{*
  Convertit un entier dans une base donnée
  @param Value   Entier à convertir
  @param Base    Base de destination
  @return Représentation en chaîne de Value exprimé dans la base Base
  @raise EConvertError Base incorrecte
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
  Convertit un nombre exprimé dans une base donnée en sa représentation décimale
  @param Value   Chaîne de caractère représentant le nombre
  @param Base    Base dans laquelle est exprimée le nombre
  @return Valeur décimale du nombre
  @raise EConvertError Base incorrecte
  @raise EConvertError Entier incorrect
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
  Convertit un nombre exprimé dans une base donnée en sa représentation décimale
  Lorsque la chaîne n'est pas un entier valide, une valeur par défaut est
  renvoyée.
  @param Value     Chaîne de caractère représentant le nombre
  @param Default   Valeur par défaut
  @param Base      Base dans laquelle est exprimée le nombre
  @return Valeur décimale du nombre
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
  Recherche une méthode d'un objet à partir de son nom
  La méthode en question doit être publiée pour pouvoir être trouvée.
  @param Obj          L'objet définissant la méthode
  @param MethodName   Nom de la méthode
  @return Une référence à la méthode recherchée pour l'objet Obj
*}
function GetMethodFromName(Obj : TObject; MethodName : ShortString) : TMethod;
begin
  Result.Code := Obj.MethodAddress(MethodName);
  Result.Data := Obj;
end;

{*
  Convertit une valeur Double en la valeur Int64 ayant les mêmes bits
  Attention ! Il n'y a aucune correspondance entre Value et Result ! Cette
  fonction est totalement empirique.
*}
function ConvertDoubleToInt64(Value : Double) : Int64;
type
  TypeDeTransition = packed record
    case integer of
    0 : (DblValue : Double);
    1 : (IntValue : Int64);
  end;
var VarDeTransition : TypeDeTransition;
begin
  { Ceci est totalement empirique, seuls les bits sont égaux (ce qui ne
    correspond absolument pas à la valeur) }
  VarDeTransition.DblValue := Value;
  Result := VarDeTransition.IntValue;
end;

{*
  Convertit une valeur Int64 en la valeur Double ayant les mêmes bits
  Attention ! Il n'y a aucune correspondance entre Value et Result ! Cette
  fonction est totalement empirique.
*}
function ConvertInt64ToDouble(Value : Int64) : Double;
type
  TypeDeTransition = packed record
    case integer of
    0 : (IntValue : Int64);
    1 : (DblValue : Double);
  end;
var VarDeTransition : TypeDeTransition;
begin
  { Ceci est totalement empirique, seuls les bits sont égaux (ce qui ne
    correspond absolument pas à la valeur) }
  VarDeTransition.IntValue := Value;
  Result := VarDeTransition.DblValue;
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
        Result := Result+'#'+IntToStr(Byte(Str[I]));
        inc(I);
      end else
      begin
        Result := Result+'''';
        while (I <= Length(Str)) and (not (Str[I] in ExcludedChars)) do
        begin
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
      if (IntChar >= 0) and (IntChar <= 255) then Result := Result+Char(IntChar) else
        raise EConvertError.CreateFmt(sScWrongString, [Str]);
    end;
  end;
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
function CharSetToStr(CharSet : TSysCharSet) : string;
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

{$IFDEF MSWINDOWS}
{*
  Crée un raccourci Windows
  Seuls les paramètres Source et Dest sont obligatoires.
  @param Source         Nom du fichier raccourci
  @param Dest           Destination du raccourci
  @param Description    Description
  @param IconLocation   Nom du fichier contenant l'icône du raccourci
  @param IconIndex      Index de l'icône dans le fichier
  @param Arguments      Arguments appliqués à la destination du raccourci
  @param WorkDir        Répertoire de travail pour l'exécution du raccourci
  @param ShowCommand    Commande d'affichage de la destination
*}
procedure CreateShellLink(const Source, Dest : string;
                          const Description : string = '';
                          const IconLocation : string = '';
                          IconIndex : integer = 0;
                          const Arguments : string = '';
                          const WorkDir : string = '';
                          ShowCommand : integer = SW_SHOW);
var Link : IShellLink;
begin
  // Création de l'objet ShellLink
  Link := CreateComObject(CLSID_ShellLink) as IShellLink;
  // Fichier source
  Link.SetPath(PChar(Source));
  // Description
  if Description <> '' then
    Link.SetDescription(PChar(Description));
  // Emplacement de l'icône
  if IconLocation <> '' then
    Link.SetIconLocation(PChar(IconLocation), IconIndex);
  // Arguments
  if Arguments <> '' then
    Link.SetArguments(PChar(Arguments));
  // Dossier de travail
  if WorkDir <> '' then
    Link.SetWorkingDirectory(PChar(WorkDir));
  // Type de lancement
  Link.SetShowCmd(ShowCommand);
  // Enregistrement
  (Link as IPersistFile).Save(StringToOleStr(Dest), True);
end;
{$ENDIF}

{*
  Exécute un son à partir d'un fichier, d'une ressource ou d'un son système
  @param Sound         Nom du son (selon le type de son)
  @param SoundType     Type de son
  @param Synchronous   True pour exécuter le son de façon synchrône
  @param Module        Dans le cas d'une ressource, le module la contenant
  @param AddFlags      Flags additionnels à passer à MMSystem.PlaySound
  @return True si le son a été correctement exécuté, False sinon
*}
function ExecuteSound(const Sound : string; SoundType : TSoundType = stFileName;
  Synchronous : boolean = False; Module : HMODULE = 0;
  AddFlags : LongWord = 0) : boolean;
var Flags : LongWord;
begin
  Flags := AddFlags;
  case SoundType of
    stFileName : Flags := Flags or SND_FILENAME; // Fichier son
    stResource : Flags := Flags or SND_RESOURCE; // Ressource son
    stSysSound : Flags := Flags or SND_ALIAS;    // Alias son système
  end;
  if not Synchronous then Flags := Flags or SND_ASYNC; // Asynchrône ?
  if SoundType <> stResource then Module := 0;
  // Appel de PlaySound et renvoi de la valeur renvoyée par celle-ci
  Result := PlaySound(PChar(Sound), Module, Flags);
end;

{$REGION 'Modification des ressources'}

{---------------------------------}
{ Ajout-suppression de ressources }
{---------------------------------}
{$IFDEF MSWINDOWS}

{*
  Débute la mise à jour des ressources d'un fichier module
  Tout appel à BeginUpdateRes doit être compensé par un appel à EndUpdateRes.
  @param FileName   Nom du fichier module
  @return Handle de ressources
*}
function BeginUpdateRes(const FileName : string) : integer;
begin
  // Appel de Windows.BeginUpdateResource
  Result := BeginUpdateResource(PChar(FileName), False);
  // Si Result = 0, il y a eu une erreur API
  if Result = 0 then
    raise EUpdateResError.Create;
end;

{*
  Ajoute une ressource
  @param ResHandle   Handle de ressources obtenu par BeginUpdateRes
  @param ResName     Nom de la ressource à ajouter
  @param Resource    Flux contenant la ressource
  @param ResType     Type de ressource
*}
procedure AddResource(ResHandle : integer; const ResName : string;
  Resource : TStream; const ResType : string = 'RT_RCDATA');
var MemRes : TMemoryStream;
    MustFreeRes, OK : boolean;
begin
  MustFreeRes := False;
  // On met dans MemRes un flux mémoire qui contient les données de la ressource
  if Resource is TMemoryStream then MemRes := Resource as TMemoryStream else
  begin
    MemRes := TMemoryStream.Create;
    MemRes.LoadFromStream(Resource);
    MustFreeRes := True;
  end;
  // Appel de Windows.UpdateResource
  OK := UpdateResource(ResHandle, PChar(ResType), PChar(ResName), 0,
                       MemRes.Memory, MemRes.Size);
  // On supprime le flux mémoire si on l'a créé
  if MustFreeRes then MemRes.Free;
  // Si UpdateResource a renvoyé False, il y a eu une erreur
  if not OK then raise EUpdateResError.Create;
end;

{*
  Supprime une ressource
  @param ResHandle   Handle de ressources obtenu par BeginUpdateRes
  @param ResName     Nom de la ressource à supprimer
*}
procedure DelResource(ResHandle : integer; const ResName : string);
begin
  // Appel de Windows.UpdateResource
  if not UpdateResource(ResHandle, '', PChar(ResName), 0, nil, 0) then
  // Si UpdateResource a renvoyé False, il y a eu une erreur
    raise EUpdateResError.Create;
end;

{*
  Termine la mise à jour des ressources d'un fichier module
  @param ResHandle   Handle de ressources obtenu par BeginUpdateRes
  @param Cancel      Indique s'il faut annuler les modifications faites
*}
procedure EndUpdateRes(ResHandle : integer; Cancel : boolean = False);
begin
  // Appel de Windows.EndUpdateResource
  if not EndUpdateResource(ResHandle, Cancel) then
  // Si EndUpdateResource a renvoyé False, il y a eu une erreur
    raise EUpdateResError.Create;
end;

{*
  Ajoute une ressources à un fichier module
  @param FileName   Nom du fichier module
  @param ResName    Nom de la ressource à ajouter
  @param Resource   Flux contenant la ressource
  @param ResType    Type de ressource
*}
procedure AddResToFile(const FileName, ResName : string; Resource : TStream;
  const ResType : string = 'RT_RCDATA');
var ResHandle : integer;
begin
  ResHandle := BeginUpdateRes(FileName);
  try
    AddResource(ResHandle, ResName, Resource, ResType);
    EndUpdateRes(ResHandle);
  except
    try EndUpdateRes(ResHandle, True) except end;
    raise;
  end;
end;

{*
  Supprime une ressources d'un fichier module
  @param FileName   Nom du fichier module
  @param ResName    Nom de la ressource à supprimer
*}
procedure DelResInFile(const FileName, ResName : string);
var ResHandle : integer;
begin
  ResHandle := BeginUpdateRes(FileName);
  try
    DelResource(ResHandle, ResName);
    EndUpdateRes(ResHandle);
  except
    try EndUpdateRes(ResHandle, True) except end;
    raise;
  end;
end;

{$ENDIF}

{$ENDREGION}

{$REGION 'Compression/décompression'}

{*
  Compresse un flux avec la bibliothèque ZLib
  @param Stream             Flux à compresser
  @param Dest               Flux de destination (ou nil pour Stream, plus lent)
  @param CompressionLevel   Niveau de compression
*}
procedure CompressStream(Stream : TStream; Dest : TStream = nil;
  CompressionLevel : TCompressionLevel = clDefaultComp);
var Compress : TCompressionStream;
    Destination : TStream;
begin
  // Si Dest vaut nil, on crée un flux temporaire de destination
  if Dest = nil then Destination := TMemoryStream.Create else
  begin
    Destination := Dest;
    Destination.Position := 0;
    Destination.Size := 0;
  end;
  // Création, utilisation et libération du flux de compression
  Compress := TCompressionStream.Create(CompressionLevel, Destination);
  Compress.CopyFrom(Stream, 0);
  Compress.Free;
  // Si Dest vaut nil, on recopie Destination dans Stream
  if Dest = nil then
  begin
    Stream.Position := 0;
    Stream.Size := 0;
    Stream.CopyFrom(Destination, 0);
    Destination.Free;
  end;
end;

{*
  Décompresse un flux avec la bibliothèque ZLib
  @param Stream   Flux à décompresser
  @param Dest     Flux de destination (ou nil pour Stream, plus lent)
*}
procedure DecompressStream(Stream : TStream; Dest : TStream = nil);
var Decompress : TDecompressionStream;
    Destination : TStream;
    Buffer : array [0..1023] of Byte;
    Copies : integer;
begin
  // Si Dest vaut nil, on crée un flux temporaire de destination
  if Dest = nil then Destination := TMemoryStream.Create else
  begin
    Destination := Dest;
    Destination.Position := 0;
    Destination.Size := 0;
  end;
  // Création, utilisation et libération du flux de décompression
  Decompress := TDecompressionStream.Create(Stream);
  Decompress.Position := 0;
  repeat
    Copies := Decompress.Read(Buffer, 1024);
    Destination.Write(Buffer, Copies);
  until Copies < 1024;
  Decompress.Free;
  // Si Dest vaut nil, on recopieDestination dans Stream
  if Dest = nil then
  begin
    Stream.Position := 0;
    Stream.Size := 0;
    Stream.CopyFrom(Destination, 0);
  end;
end;

{$ENDREGION}

end.

