{*
  Définit des routines en lien avec le langage Delphi lui-même
  @author sjrd
  @version 1.0
*}
unit ScDelphiLanguage;

interface

uses
  SysUtils, TypInfo, ScUtils;

type
  {*
    Instruction JMP ou CALL
    @author sjrd
    @version 1.0
  *}
  TJmpInstruction = packed record
    OpCode: Byte;      /// OpCode
    Argument: Integer; /// Destination
  end;

function CorrectIdentifier(const Ident: string): Boolean;

procedure SetBit(var Value: Integer; const Bit: Byte); register;
procedure ClearBit(var Value: Integer; const Bit: Byte); register;
procedure ToggleBit(var Value: Integer; const Bit: Byte); register;
function TestBit(const Value: Integer; const Bit: Byte): Boolean; register;

function GetMethodFromName(Obj: TObject;
  const MethodName: ShortString): TMethod;

function MakeMethod(Code: Pointer; Data: Pointer = nil): TMethod;

function GetClassVirtualCode(AClass: TClass; VMTOffset: Integer): Pointer;
function GetClassVirtualMethod(AClass: TClass; VMTOffset: Integer): TMethod;
function GetObjectVirtualCode(AObject: TObject;
  VMTOffset: Integer): Pointer;
function GetObjectVirtualMethod(AObject: TObject;
  VMTOffset: Integer): TMethod;

function GetClassDynamicCode(AClass: TClass; DMTIndex: Integer): Pointer;
function GetClassDynamicMethod(AClass: TClass; DMTIndex: Integer): TMethod;
function GetObjectDynamicCode(AObject: TObject; DMTIndex: Integer): Pointer;
function GetObjectDynamicMethod(AObject: TObject;
  DMTIndex: Integer): TMethod;

function StrToStrRepres(const Str: string;
  ExcludedChars: TSysCharSet = []): string;
function StrRepresToStr(Str: string): string;

function CharToCharRepres(Chr: Char;
  ExcludedChars: TSysCharSet = []): string;
function CharRepresToChar(Str: string): Char;

function CharSetToStr(const CharSet: TSysCharSet): string;
function StrToCharSet(Str: string): TSysCharSet;

function EnumSetToStr(const EnumSet; TypeInfo: PTypeInfo): string;
procedure StrToEnumSet(const Str: string; TypeInfo: PTypeInfo; out EnumSet);

function SkipPackedShortString(Value: PShortstring): Pointer;

function JmpArgument(JmpAddress, JmpDest: Pointer): Integer;
procedure MakeJmp(var Instruction; Dest: Pointer);
procedure MakeCall(var Instruction; Dest: Pointer);

function MakeProcOfRegisterMethod(const Method: TMethod;
  UsedRegCount: Byte; MoveStackCount: Word = 0): Pointer;
function MakeProcOfStdCallMethod(const Method: TMethod): Pointer;
function MakeProcOfPascalMethod(const Method: TMethod): Pointer;
function MakeProcOfCDeclMethod(const Method: TMethod): Pointer;
procedure ClearCDeclCallInfo;
procedure FreeProcOfMethod(Proc: Pointer);

implementation

uses
  Windows, ScConsts;

type
  /// Pointeur vers TCDeclCallInfo
  PCDeclCallInfo = ^TCDeclCallInfo;

  {*
    Informations de contexte sur l'appel d'une méthode cdecl
    @author sjrd
    @version 1.0
  *}
  TCDeclCallInfo = packed record
    Previous: PCDeclCallInfo; /// Pointeur vers le contexte précédent
    StackPointer: Pointer;    /// Valeur de ESP au moment de l'appel
    ReturnAddress: Pointer;   /// Adresse de retour de l'appel
  end;

threadvar
  /// Liste des infos sur les appels cdecl (spécifique à chaque thread)
  CDeclCallInfoList: PCDeclCallInfo;

{*
  Trouve la dernière info de routine cdecl valide
  @param StackPointer   Valeur du registre esp au moment de l'appel
  @param AllowSame      True pour permettre un StackPointer égal, False sinon
  @return Pointeur sur la dernière info de routine cdecl valide
*}
function GetLastValidCDeclCallInfo(StackPointer: Pointer;
  AllowSame: Boolean): PCDeclCallInfo;
var
  Previous: PCDeclCallInfo;
begin
  Result := CDeclCallInfoList;
  while (Result <> nil) and
    (Cardinal(Result.StackPointer) <= Cardinal(StackPointer)) do
  begin
    if AllowSame and (Result.StackPointer = StackPointer) then
      Break;
    Previous := Result.Previous;
    Dispose(Result);
    Result := Previous;
  end;
end;

{*
  S'assure que toutes les informations de routines cdecl sont supprimées
  Cette routine devrait être appelée à la fin de chaque thread susceptible
  d'utiliser des routines issues de méthodes cdecl.
  Si ce n'est pas possible, ce n'est pas dramatique, mais l'application
  s'expose à de légères fuites mémoire en cas d'exception à l'intérieur de
  telles méthodes.
  Pour le thread principal, ce n'est pas nécessaire, le code de finalisation de
  ScDelphiLanguage.pas s'en charge.
*}
procedure ClearCDeclCallInfo;
begin
  GetLastValidCDeclCallInfo(Pointer($FFFFFFFF), False);
  CDeclCallInfoList := nil;
end;

{*
  Ajoute une adresse de retour
  @param StackPointer    Valeur du registre ESP
  @param ReturnAddress   Adresse à ajouter
*}
procedure StoreCDeclReturnAddress(
  StackPointer, ReturnAddress: Pointer); stdcall;
var
  LastInfo, CurInfo: PCDeclCallInfo;
begin
  LastInfo := GetLastValidCDeclCallInfo(StackPointer, False);

  New(CurInfo);
  CurInfo.Previous := LastInfo;
  CurInfo.StackPointer := StackPointer;
  CurInfo.ReturnAddress := ReturnAddress;
  CDeclCallInfoList := CurInfo;
end;

{*
  Récupère une adresse de retour
  @param StackPointer   Valeur du registre ESP
  @return L'adresse de retour qui a été ajoutée en dernier
*}
function GetCDeclReturnAddress(StackPointer: Pointer): Pointer; register;
var
  LastInfo: PCDeclCallInfo;
begin
  LastInfo := GetLastValidCDeclCallInfo(StackPointer, True);

  if (LastInfo = nil) or (LastInfo.StackPointer <> StackPointer) then
  begin
    CDeclCallInfoList := LastInfo;
    Assert(False);
  end;

  CDeclCallInfoList := LastInfo.Previous;
  Result := LastInfo.ReturnAddress;
  Dispose(LastInfo);
end;

{*
  Vérifie si une chaîne de caractères est un identificateur Pascal correct
  @param Ident   Chaîne de caractères à tester
  @return True si Ident est un identificateur Pascal correct, False sinon
*}
function CorrectIdentifier(const Ident: string): Boolean;
var
  I: Integer;
begin
  Result := False;

  // Si Ident est vide, ce n'est un indentificateur correct
  if Ident = '' then
    Exit;

  { Si le premier caractère n'est pas alphabétique,
    ce n'est pas un identificateur correct }
  if not (Ident[1] in ['A'..'Z', '_', 'a'..'z']) then
    Exit;

  { Si l'un des caractères suivants n'est pas alphanumérique,
    ce n'est pas un identificateur correct }
  for I := 2 to Length(Ident) do
    if not (Ident[I] in ['0'..'9', 'A'..'Z', '_', 'a'..'z']) then
      Exit;

  // Dans les autres cas, ça l'est
  Result := True;
end;

{*
  Positionne un bit à 1
  @param Value   Valeur à modifier
  @param Bit     Index du bit à modifier
  @author waskol
*}
procedure SetBit(var Value: Integer; const Bit: Byte); register;
asm
        BTS     [EAX],EDX
end;

{*
  Positionne un bit à 0
  @param Value   Valeur à modifier
  @param Bit     Index du bit à modifier
  @author waskol
*}
procedure ClearBit(var Value: Integer; const Bit: Byte); register;
asm
        BTR     [EAX],EDX
end;

{*
  Inverse un bit
  @param Value   Valeur à modifier
  @param Bit     Index du bit à modifier
  @author waskol
*}
procedure ToggleBit(var Value: Integer; const Bit: Byte); register;
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
function TestBit(const Value: Integer; const Bit: Byte): Boolean; register;
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
  @return Un enregistrement TMethod avec les champs indiqués
*}
function MakeMethod(Code: Pointer; Data: Pointer = nil): TMethod;
begin
  Result.Code := Code;
  Result.Data := Data;
end;

{*
  Trouve le code d'une méthode virtuelle, pour une classe
  @param AClass      Classe concernée
  @param VMTOffset   VMT offset de la méthode
  @return Pointeur sur le code de la méthode
*}
function GetClassVirtualCode(AClass: TClass; VMTOffset: Integer): Pointer;
asm
        { ->    EAX     Pointer to class  }
        {       EDX     DMTIndex          }
        { <-    EAX     Pointer to method }
        MOV     EAX,[EAX+EDX]
end;

{*
  Trouve une méthode virtuelle, pour une classe
  @param AClass      Classe concernée
  @param VMTOffset   VMT offset de la méthode
  @return Méthode correspondante
*}
function GetClassVirtualMethod(AClass: TClass; VMTOffset: Integer): TMethod;
begin
  Result.Data := AClass;
  Result.Code := GetClassVirtualCode(AClass, VMTOffset);
end;

{*
  Trouve le code d'une méthode virtuelle, pour un objet
  @param AObject     Objet concerné
  @param VMTOffset   VMT offset de la méthode
  @return Pointeur sur le code de la méthode
*}
function GetObjectVirtualCode(AObject: TObject;
  VMTOffset: Integer): Pointer;
asm
        { ->    EAX     Pointer to object }
        {       EDX     DMTIndex          }
        { <-    EAX     Pointer to method }
        MOV     EAX,[EAX]
        MOV     EAX,[EAX+EDX]
end;

{*
  Trouve une virtuelle dynamique, pour un objet
  @param AObject     Objet concerné
  @param VMTOffset   VMT offset de la méthode
  @return Méthode correspondante
*}
function GetObjectVirtualMethod(AObject: TObject;
  VMTOffset: Integer): TMethod;
begin
  Result.Data := AObject;
  Result.Code := GetObjectVirtualCode(AObject, VMTOffset);
end;

{*
  Trouve le code d'une méthode dynamique, pour une classe
  @param AClass     Classe concernée
  @param DMTIndex   DMT index de la méthode
  @return Pointeur sur le code de la méthode
  @throws EAbstractError La classe n'implémente pas la méthode recherchée
*}
function GetClassDynamicCode(AClass: TClass; DMTIndex: Integer): Pointer;
asm
        { ->    EAX     Pointer to class  }
        {       EDX     DMTIndex          }
        { <-    EAX     Pointer to method }
        CALL    System.@FindDynaClass
end;

{*
  Trouve une méthode dynamique, pour une classe
  @param AClass     Classe concernée
  @param DMTIndex   DMT index de la méthode
  @return Méthode correspondante
  @throws EAbstractError La classe n'implémente pas la méthode recherchée
*}
function GetClassDynamicMethod(AClass: TClass; DMTIndex: Integer): TMethod;
begin
  Result.Data := AClass;
  Result.Code := GetClassDynamicCode(AClass, DMTIndex);
end;

{*
  Trouve le code d'une méthode dynamique, pour un objet
  @param AObject    Objet concerné
  @param DMTIndex   DMT index de la méthode
  @return Pointeur sur le code de la méthode
  @throws EAbstractError La classe n'implémente pas la méthode recherchée
*}
function GetObjectDynamicCode(AObject: TObject; DMTIndex: Integer): Pointer;
asm
        { ->    EAX     Pointer to object }
        {       EDX     DMTIndex          }
        { <-    EAX     Pointer to method }
        MOV     EAX,[EAX]
        CALL    System.@FindDynaClass
end;

{*
  Trouve une méthode dynamique, pour un objet
  @param AObject    Objet concerné
  @param DMTIndex   DMT index de la méthode
  @return Méthode correspondante
  @throws EAbstractError La classe n'implémente pas la méthode recherchée
*}
function GetObjectDynamicMethod(AObject: TObject;
  DMTIndex: Integer): TMethod;
begin
  Result.Data := AObject;
  Result.Code := GetObjectDynamicCode(AObject, DMTIndex);
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
function StrToStrRepres(const Str: string;
  ExcludedChars: TSysCharSet = []): string;
var
  I: Integer;
begin
  if Str = '' then
    Result := ''''''
  else
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
        Inc(I);
      end else
      begin
        Result := Result+'''';
        while (I <= Length(Str)) and (not (Str[I] in ExcludedChars)) do
        begin
          if I mod 256 = 0 then
            Result := Result+'''+''';

          if Str[I] = '''' then
            Result := Result + ''''''
          else
            Result := Result + Str[I];
          Inc(I);
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
  @throws EConvertError Chaîne de caractère incorrecte
*}
function StrRepresToStr(Str: string): string;
var
  CharStr: string;
  I, IntChar: Integer;
begin
  Result := '';
  Str := Trim(Str);
  I := 1;
  repeat
    if I > 1 then
      Inc(I);

    while (I <= Length(Str)) and ((Str[I] = '''') or (Str[I] = '#')) do
    begin
      if Str[I] = '''' then
      begin
        Inc(I);
        while True do
        begin
          if I > Length(Str) then
            raise EConvertError.CreateFmt(sScWrongString, [Str]);
          if Str[I] = '''' then
          begin
            Inc(I);
            if (I <= Length(Str)) and (Str[I] = '''') then
            begin
              Result := Result+'''';
              Inc(I);
            end else
              Break;
          end else
          begin
            Result := Result+Str[I];
            Inc(I);
          end;
        end;
      end else
      begin
        Inc(I);
        if I > Length(Str) then
          raise EConvertError.CreateFmt(sScWrongString, [Str]);
        CharStr := '';
        while (I <= Length(Str)) and (Str[I] in ['0'..'9']) do
        begin
          CharStr := CharStr+Str[I];
          Inc(I);
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
function CharToCharRepres(Chr: Char;
  ExcludedChars: TSysCharSet = []): string;
begin
  ExcludedChars := ExcludedChars + [#0..#31];
  if Chr in ExcludedChars then
    Result := '#'+IntToStr(Byte(Chr))
  else if Chr = '''' then
    Result := ''''''''''
  else
    Result := ''''+Chr+'''';
end;

{*
  Détermine un caractère à partir de sa représentation Pascal
  @param Str   Chaîne à traiter
  @return Caractère représenté par Str en Pascal
  @throws EConvertError Caractère incorrect
*}
function CharRepresToChar(Str: string): Char;
begin
  try
    Str := Trim(Str);
    if Str = '' then
      raise EConvertError.Create('');
    case Str[1] of
      '#':
      begin
        // Le résultat est le caractère dont le code ASCII est l'entier
        // spécifié à la suite
        Delete(Str, 1, 1);
        Result := Chr(StrToInt(Str));
      end;
      '''':
      begin
        case Length(Str) of
          // Si 3 caractères, le troisième doit être ' et le deuxième
          // est le caractère résultat
          3: if Str[3] = '''' then
              Result := Str[2]
            else
              raise EConvertError.Create('');
          // Si 4 caractères, ce doit être '''', auquel cas le caractère
          // retour est '
          4: if Str = '''''''''' then
              Result := ''''
            else
              raise EConvertError.Create('');
        else
          // Sinon, ce n'est pas un caractère correct
          raise EConvertError.Create('');
        end;
      end;
    else
      raise EConvertError.Create('');
    end;
  except
    on Error: EConvertError do
      raise EConvertError.CreateFmt(sSjrdWrongChar, [Str]);
  end;
end;

{*
  Détermine la représentation Pascal d'un ensemble de caractères (sans les [])
  @param CharSet   Ensemble de caractères à traiter
  @return Représentation Pascal de CharSet
*}
function CharSetToStr(const CharSet: TSysCharSet): string;
var
  I, From: Word;
begin
  Result := '';
  I := 0;
  // On cherche d'abord le premier caractère inclus
  while (I <= 255) and (not (Chr(I) in CharSet)) do
    Inc(I);
  while I <= 255 do
  begin
    // Chr(I) est inclus
    From := I;
    // On cherche le caractère suivant qui n'est pas inclus
    while (I <= 255) and (Chr(I) in CharSet) do
      Inc(I);
    // On teste I-From, soit le nombre de caractère consécutifs
    case I-From of
      // 1 : on ajoute simplement ce caractère
      1: Result := Result+', '+CharToCharRepres(Chr(From));
      // 2 : on ajoute ces deux caractères séparés par des virgules
      2: Result := Result+', '+CharToCharRepres(Chr(From))+
          ', '+CharToCharRepres(Chr(I-1));
    else
      // 3+ : on ajoute les deux extrèmes séparés par ..
      Result := Result+', '+CharToCharRepres(Chr(From))+
        '..'+CharToCharRepres(Chr(I-1));
    end;
    // on cherche le caractère suivant inclus
    repeat
      Inc(I);
    until (I > 255) or (Chr(I) in CharSet);
  end;
  // On supprime les deux premiers caractères, car ce sont ', '
  Delete(Result, 1, 2);
end;

{*
  Détermine un ensemble de caractères à partir de sa représentation Pascal
  @param Str   Chaîne à traiter
  @return Ensemble de caractères représenté par CharSet
  @throws EConvertError Ensemble de caractères incorrect
*}
function StrToCharSet(Str: string): TSysCharSet;
var
  I: Integer;

  // Renvoie le caractère à la position courante et augmente I en conséquence
  // Fonctionne sur le même principe que CharRepresToChar
  function GetCharAt: Char;
  var
    From: Integer;
  begin
    case Str[I] of
      '#':
      begin
        From := I+1;
        repeat
          Inc(I);
        until (I > Length(Str)) or (not (Str[I] in ['0'..'9']));
        Result := Chr(StrToInt(Copy(Str, From, I-From)));
      end;
      '''':
      begin
        Inc(I);
        if I > Length(Str) then
          raise EConvertError.Create('');
        if Str[I] = '''' then
        begin
          if I+2 > Length(Str) then
            raise EConvertError.Create('');
          if (Str[I+1] <> '''') or (Str[I+2] <> '''') then
            raise EConvertError.Create('');
          Result := '''';
          Inc(I, 3);
        end else
        begin
          if I+1 > Length(Str) then
            raise EConvertError.Create('');
          if Str[I+1] <> '''' then
            raise EConvertError.Create('');
          Result := Str[I];
          Inc(I, 2);
        end;
      end;
    else
      raise EConvertError.Create('');
    end;
  end;

var
  C1, C2: Char;
begin
  try
    Result := [];
    Str := Trim(Str);
    // Si Str est vide, il n'y a aucun caractère dans l'ensemble
    if Str = '' then
      Exit;
    // Si il y des [] aux extrémités, on les supprime
    if (Str[1] = '[') and (Str[Length(Str)] = ']') then
      Str := Trim(Copy(Str, 2, Length(Str)-2));

    I := 1;
    while I <= Length(Str) do
    begin
      // On récupère le caractère à la position courante
      C1 := GetCharAt;
      // On passe tous les espaces
      while (I <= Length(Str)) and (Str[I] = ' ') do
        Inc(I);

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
        repeat
          Inc(I);
        until (I > Length(Str)) or (Str[I] <> ' ');
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
        Inc(I);
        if (I > Length(Str)) or (Str[I] <> '.') then
          raise EConvertError.Create('');
        // On passe ce point et les espaces
        repeat
          Inc(I);
        until (I > Length(Str)) or (Str[I] <> ' ');
        // On récupère le deuxième caractère
        C2 := GetCharAt;
        // On passe les espaces
        while (I <= Length(Str)) and (Str[I] = ' ') do
          Inc(I);

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
          repeat
            Inc(I);
          until (I > Length(Str)) or (Str[I] <> ' ');
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
    on Error: EConvertError do
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
function EnumSetToStr(const EnumSet; TypeInfo: PTypeInfo): string;
var
  TypeData: PTypeData;
  ByteValue: Byte;
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
procedure StrToEnumSet(const Str: string; TypeInfo: PTypeInfo; out EnumSet);
type
  TSetAsBytes = array[0..31] of Byte;
var
  SetName: string;
  TypeData: PTypeData;
  SetStr: string;
  Index, Len, BeginIndex, Value: Integer;
begin
  if TypeInfo.Kind = tkSet then
  begin
    SetName := TypeInfo.Name;
    TypeInfo := GetTypeData(TypeInfo).CompType^;
  end else
    SetName := Format(sScSetOf, [TypeInfo.Name]);
  TypeData := GetTypeData(TypeInfo);

  Len := TypeData.MaxValue div 8 + 1;
  FillChar(EnumSet, Len, 0);

  try
    Index := 1;
    Len := Length(Str);
    if (Str <> '') and (Str[1] = '[') and (Str[Len] = ']') then
    begin
      SetStr := ',' + Copy(Str, 2, Len-2);
      Dec(Len, 1);
    end else
    begin
      SetStr := ',' + Str;
      Inc(Len, 1);
    end;

    while Index <= Len do
    begin
      if SetStr[Index] <> ',' then
        raise Exception.Create('');
      Inc(Index);

      while (Index <= Len) and (SetStr[Index] in [' ', #13, #10]) do
        Inc(Index);
      BeginIndex := Index;
      while (Index <= Len) and (not (SetStr[Index] in [',', ' ', #13, #10])) do
        Inc(Index);

      Value := GetEnumValue(TypeInfo,
        Copy(SetStr, BeginIndex, Index-BeginIndex));
      if Value < 0 then
        raise Exception.Create('');
      Include(TSysByteSet(EnumSet), Value);

      while (Index <= Len) and (SetStr[Index] in [' ', #13, #10]) do
        Inc(Index);
    end;
  except
    raise EConvertError.CreateFmt(sScWrongEnumSet, [Str, SetName]);
  end;
end;

{*
  Renvoie un pointeur vers le champ suivant un ShortString compactée (des RTTI)
  Cette routine peut être utilisée pour passer « au-dessus » d'une ShortString
  compactée, telle qu'on peut en trouver dans les record extra-compactés des
  RTTI.
  @param Value    Adresse de la ShortString compactée
  @return Adresse du champ suivant
*}
function SkipPackedShortString(Value: PShortstring): Pointer;
asm
        { ->    EAX Pointer to a packed ShortString                   }
        { <-    EAX Pointer to data following this packed ShortString }
        XOR     EDX,EDX
        MOV     DL,[EAX]
        LEA     EAX,[EAX].Byte[EDX+1]
end;

{*
  Calcule l'argument d'une instruction JMP ou CALL
  @param JmpAddress   Adresse de l'instruction JMP
  @param JmpDest      Destination du JMP
  @return Argument à donner au JMP
*}
function JmpArgument(JmpAddress, JmpDest: Pointer): Integer;
asm
        { -> EAX Address of the jump instruction }
        { -> EDX Pointer to destination          }
        { -> Return value = EDX - EAX - 5        }

        NEG     EAX
        ADD     EAX,EDX
        SUB     EAX,5
end;

{*
  Construit une instruction JMP
  @param Instruction   Instruction (minimum 5 octets)
  @param Dest          Destination du JMP
*}
procedure MakeJmp(var Instruction; Dest: Pointer);
asm
        { -> EAX Pointer to a TJmpInstruction record }
        { -> EDX Pointer to destination              }

        MOV     BYTE PTR [EAX],$E9
        SUB     EDX,EAX
        SUB     EDX,5
        MOV     [EAX+1],EDX
end;

{*
  Construit une instruction CALL
  @param Instruction   Instruction (minimum 5 octets)
  @param Dest          Destination du CALL
*}
procedure MakeCall(var Instruction; Dest: Pointer);
asm
        { -> EAX Pointer to a TJmpInstruction record }
        { -> EDX Pointer to destination              }

        MOV     BYTE PTR [EAX],$E8
        SUB     EDX,EAX
        SUB     EDX,5
        MOV     [EAX+1],EDX
end;

{*
  Construit une routine équivalente à une méthode register, version courte
  @param Method           Méthode à convertir
  @param UsedRegCount     Nombre de registres utilisés dans l'appel de procédure
  @param MoveStackCount   Nombre de cases de pile empilées après ECX
  @return Pointeur vers le code de la procédure créée
*}
function MakeShortProcOfRegisterMethod(const Method: TMethod;
  UsedRegCount: Byte; MoveStackCount: Word): Pointer;

const
  MoveStackItem: LongWord = $00244C87; // 874C24 xx   XCHG ECX,[ESP+xx]
  MoveRegisters: array[0..7] of Byte = (
    $87, $0C, $24, // XCHG    ECX,[ESP]
    $51,           // PUSH    ECX
    $8B, $CA,      // MOV     ECX,EDX
    $8B, $D0       // MOV     EDX,EAX
  );

type
  PRegisterRedirector = ^TRegisterRedirector;
  TRegisterRedirector = packed record
    MovEAXObj: Byte;
    ObjAddress: Pointer;
    Jump: TJmpInstruction;
  end;

var
  MoveStackSize: Integer;
  MoveRegSize: Integer;
  InstrPtr: Pointer;
  I: Cardinal;
begin
  if UsedRegCount >= 3 then
    UsedRegCount := 4;
  MoveRegSize := 2*UsedRegCount;
  MoveStackSize := 4*MoveStackCount;

  GetMem(Result, MoveStackSize + MoveRegSize + SizeOf(TRegisterRedirector));
  InstrPtr := Result;

  for I := MoveStackCount downto 1 do
  begin
    // I shl 26 => I*4 in the most significant byte (kind of $ I*4 00 00 00)
    PLongWord(InstrPtr)^ := (I shl 26) or MoveStackItem;
    Inc(Integer(InstrPtr), 4);
  end;

  Move(MoveRegisters[SizeOf(MoveRegisters) - MoveRegSize],
    InstrPtr^, MoveRegSize);
  Inc(Integer(InstrPtr), MoveRegSize);

  with PRegisterRedirector(InstrPtr)^ do
  begin
    MovEAXObj := $B8;
    ObjAddress := Method.Data;
    MakeJmp(Jump, Method.Code);
  end;
end;

{*
  Construit une routine équivalente à une méthode register, version longue
  @param Method           Méthode à convertir
  @param MoveStackCount   Nombre de cases de pile empilées après ECX
  @return Pointeur vers le code de la procédure créée
*}
function MakeLongProcOfRegisterMethod(const Method: TMethod;
  MoveStackCount: Word): Pointer;

type
  PRegisterRedirector = ^TRegisterRedirector;
  TRegisterRedirector = packed record
    Reserved1: array[0..8] of Byte;
    MoveStackCount4: LongWord;
    Reserved2: array[13..20] of Byte;
    FourMoveStackCount1: LongWord;
    Reserved3: array[25..29] of Byte;
    ObjAddress: Pointer;
    Jump: TJmpInstruction;
  end;

const
  Code: array[0..SizeOf(TRegisterRedirector)-1] of Byte = (
    $56,                     // PUSH    ESI
    $57,                     // PUSH    EDI
    $51,                     // PUSH    ECX

    $8B, $F4,                // MOV     ESI,ESP
    $51,                     // PUSH    ECX
    $8B, $FC,                // MOV     EDI,ESP
    $B9, $FF, $FF, $FF, $FF, // MOV     ECX,MoveStackCount+4

    $F3, $A5,                // REP     MOVSD

    $59,                     // POP     ECX
    $5F,                     // POP     EDI
    $5E,                     // POP     ESI

    $89, $8C, $24, $EE, $EE, $EE, $EE,
    // MOV     [ESP + 4*(MoveStackCount+1)],ECX
    $8B, $CA,                // MOV     ECX,EDX
    $8B, $D0,                // MOV     EDX,EAX
    $B8, $DD, $DD, $DD, $DD, // MOV     EAX,0
    $E9, $CC, $CC, $CC, $CC  // JMP     MethodAddress
  );

begin
  GetMem(Result, SizeOf(Code));
  Move(Code[0], Result^, SizeOf(Code));

  with PRegisterRedirector(Result)^ do
  begin
    MoveStackCount4 := MoveStackCount+4;
    FourMoveStackCount1 := 4 * (MoveStackCount+1);
    ObjAddress := Method.Data;
    MakeJmp(Jump, Method.Code);
  end;
end;

{*
  Construit une routine équivalente à une méthode register
  MakeProcOfMethod permet d'obtenir un pointeur sur une routine, construite
  dynamiquement, qui équivaut à une méthode. Ce qui signifie que la routine
  renvoyée commence par ajouter un paramètre supplémentaire, avant d'appeler
  la méthode initiale.
  La procédure devra être libérée avec FreeProcOfMethod une fois utilisée.
  Vous devez déterminer UsedRegCount et MoveStackCount d'après la délcaration de
  la *procédure*. UsedRegCount est le nombre de registres utilisés pour la
  transmission des paramètres (dans l'ordre EAX, EDX et ECX). Si les trois sont
  utilisés, le paramètre MoveStackCount doit renseigner le nombre de "cases" de
  pile (de doubles mots) utilisés par les paramètres déclarés *après* le
  paramètre transmis dans ECX.
  @param Method           Méthode à convertir
  @param UsedRegCount     Nombre de registres utilisés dans l'appel de procédure
  @param MoveStackCount   Nombre de cases de pile empilées après ECX
  @return Pointeur vers le code de la procédure créée
*}
function MakeProcOfRegisterMethod(const Method: TMethod;
  UsedRegCount: Byte; MoveStackCount: Word = 0): Pointer;
begin
  Assert((MoveStackCount = 0) or (UsedRegCount >= 3));

  if MoveStackCount <= 8 then
    Result := MakeShortProcOfRegisterMethod(
      Method, UsedRegCount, MoveStackCount)
  else
    Result := MakeLongProcOfRegisterMethod(Method, MoveStackCount);
end;

{*
  Construit une routine équivalente à une méthode stdcall
  MakeProcOfMethod permet d'obtenir un pointeur sur une routine, construite
  dynamiquement, qui équivaut à une méthode. Ce qui signifie que la routine
  renvoyée commence par ajouter un paramètre supplémentaire, avant d'appeler
  la méthode initiale.
  La procédure devra être libérée avec FreeProcOfMethod une fois utilisée.
  @param Method   Méthode à convertir
  @return Pointeur vers le code de la procédure créée
*}
function MakeProcOfStdCallMethod(const Method: TMethod): Pointer;
type
  PStdCallRedirector = ^TStdCallRedirector;
  TStdCallRedirector = packed record
    PopEAX: Byte;
    PushObj: Byte;
    ObjAddress: Pointer;
    PushEAX: Byte;
    Jump: TJmpInstruction;
  end;
begin
  GetMem(Result, SizeOf(TStdCallRedirector));
  with PStdCallRedirector(Result)^ do
  begin
    PopEAX := $58;
    PushObj := $68;
    ObjAddress := Method.Data;
    PushEAX := $50;
    MakeJmp(Jump, Method.Code);
  end;
end;

{*
  Construit une routine équivalente à une méthode pascal
  MakeProcOfMethod permet d'obtenir un pointeur sur une routine, construite
  dynamiquement, qui équivaut à une méthode. Ce qui signifie que la routine
  renvoyée commence par ajouter un paramètre supplémentaire, avant d'appeler
  la méthode initiale.
  La procédure devra être libérée avec FreeProcOfMethod une fois utilisée.
  @param Method   Méthode à convertir
  @return Pointeur vers le code de la procédure créée
*}
function MakeProcOfPascalMethod(const Method: TMethod): Pointer;
begin
  Result := MakeProcOfStdCallMethod(Method);
end;

{*
  Construit une routine équivalente à une méthode cdecl
  MakeProcOfMethod permet d'obtenir un pointeur sur une routine, construite
  dynamiquement, qui équivaut à une méthode. Ce qui signifie que la routine
  renvoyée commence par ajouter un paramètre supplémentaire, avant d'appeler
  la méthode initiale.
  La procédure devra être libérée avec FreeProcOfMethod une fois utilisée.
  @param Method   Méthode à convertir
  @return Pointeur vers le code de la procédure créée
*}
function MakeProcOfCDeclMethod(const Method: TMethod): Pointer;

type
  PCDeclRedirector = ^TCDeclRedirector;
  TCDeclRedirector = packed record
    PushESP: Byte;
    CallStoreAddress: TJmpInstruction;
    PushObj: Byte;
    ObjAddress: Pointer;
    CallMethod: TJmpInstruction;
    MovESPEAX: array[0..2] of Byte;
    MovEAXESP: array[0..1] of Byte;
    PushEDX: Byte;
    CallGetAddress: TJmpInstruction;
    PopEDX: Byte;
    Xchg: array[0..2] of Byte;
    Ret: Byte;
  end;

const
  Code: array[0..SizeOf(TCDeclRedirector)-1] of Byte = (
    $54,                     // PUSH    ESP
    $E8, $FF, $FF, $FF, $FF, // CALL    StoreCDeclReturnAddress
    $68, $EE, $EE, $EE, $EE, // PUSH    ObjAddress
    $E8, $DD, $DD, $DD, $DD, // CALL    MethodCode
    $89, $04, $24,           // MOV     [ESP],EAX
    $8B, $C4,                // MOV     EAX,ESP
    $52,                     // PUSH    EDX
    $E8, $CC, $CC, $CC, $CC, // CALL    GetCDeclReturnAddress
    $5A,                     // POP     EDX
    $87, $04, $24,           // XCHG    EAX,[ESP]
    $C3                      // RET
  );

begin
  GetMem(Result, SizeOf(Code));
  Move(Code[0], Result^, SizeOf(Code));

  with PCDeclRedirector(Result)^ do
  begin
    MakeCall(CallStoreAddress, @StoreCDeclReturnAddress);
    ObjAddress := Method.Data;
    MakeCall(CallMethod, Method.Code);
    MakeCall(CallGetAddress, @GetCDeclReturnAddress);
  end;
end;

{*
  Libère une routine construite avec une des MakeProcOfMethod
  @param Proc   Routine à libérer
*}
procedure FreeProcOfMethod(Proc: Pointer);
begin
  FreeMem(Proc);
end;

initialization
finalization
  ClearCDeclCallInfo;
end.

