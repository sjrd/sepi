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
  Étend les informations et les routines proposées par l'unité TypInfo
  @author sjrd
  @version 1.0
*}
unit ScTypInfo;
{$i ..\..\source\Sepi.inc}
interface

uses
  SysUtils, Classes, TypInfo;

const
  /// tkUString sous Delphi 2009+, tkUnknown sinon
{$IF Declared(tkUString)}
  tkUStringOrUnknown = tkUString;
{$ELSE}
  tkUStringOrUnknown = tkUnknown;
{$IFEND}

  /// tkClassRef sous Delphi 2010+, tkUnknown sinon
{$IF Declared(tkClassRef)}
  tkClassRefOrUnknown = tkClassRef;
{$ELSE}
  tkClassRefOrUnknown = tkUnknown;
{$IFEND}

  /// tkPointer sous Delphi 2010+, tkUnknown sinon
{$IF Declared(tkPointer)}
  tkPointerOrUnknown = tkPointer;
{$ELSE}
  tkPointerOrUnknown = tkUnknown;
{$IFEND}

  /// tkProcedure sous Delphi 2010+, tkUnknown sinon
{$IF Declared(tkProcedure)}
  tkProcedureOrUnknown = tkProcedure;
{$ELSE}
  tkProcedureOrUnknown = tkUnknown;
{$IFEND}

type
  /// Long string - ambivalent type for Delphi 2007- and Delphi 2009+
{$IF Declared(RawByteString)}
  LongString = RawByteString;
{$ELSE}
  LongString = AnsiString;
{$IFEND}

  /// Type chaîne utilisé dans les RTTI
  TypeInfoString = ShortString;

  /// Pointeur sur un type chaîne utilisé dans les RTTI
  PTypeInfoString = PShortString;



  {$IF Defined(FPC) or (CompilerVersion < 21)}

  /// Pointeur vers TManagedField
  PManagedField = ^TManagedField;

  {*
    Champ managé d'un record (qui requiert une initialisation)
    @author sjrd
    @version 1.0
  *}
  TManagedField = packed record
    TypeRef: PPTypeInfo; /// RTTI du type du champ
    FldOffset: Integer;  /// Offset du champ dans le record
  end;

 {$IFEND}

  /// Pointeur vers TRecordTypeData
  PRecordTypeData = ^TRecordTypeData;

  {*
    Données d'un type record
    @author sjrd
    @version 1.0
  *}
  TRecordTypeData = packed record
    Size: Integer;                               /// Taille du type
    ManagedCount: Integer;                       /// Nombre de champs managés
    ManagedFields: array[0..0] of TManagedField; /// Champs (0..ManagedCount-1)
  end;

  {$IF Defined(FPC) or (CompilerVersion < 21)}

  /// Pointeur vers TArrayTypeData
  PArrayTypeData = ^TArrayTypeData;

  {*
    Données d'un type tableau statique
    @author sjrd
    @version 1.0
  *}
  TArrayTypeData = packed record
    Size: Integer;      /// Taille du type
    ElCount: Integer;   /// Nombre d'éléments (linéarisés)
    ElType: PPTypeInfo; /// RTTI du type des éléments
  end;

  {$IFEND}



function IsTypeManaged(TypeInfo: PTypeInfo): Boolean;

function TypeSize(TypeInfo: PTypeInfo): Integer;

procedure CopyData(const Source; var Dest; Size: Integer;
  TypeInfo: PTypeInfo); overload;
procedure CopyData(const Source; var Dest; TypeInfo: PTypeInfo); overload;

function IsManagementCompatible(Left, Right: PTypeInfo): Boolean;

function TypeInfoEncode(const Str: string): TypeInfoString; inline;
function TypeInfoDecode(const Str: TypeInfoString): string; inline;

function ReadTypeInfoStringFromStream(Stream: TStream): string;
procedure WriteTypeInfoStringToStream(Stream: TStream; const Str: string);

function StrToStrRepres(const Str: string;
  ExcludedChars: TSysCharSet = []): string;
function StrRepresToStr(Str: string): string;

function CharToCharRepres(Chr: AnsiChar;
  ExcludedChars: TSysCharSet = []): string; overload;
function CharToCharRepres(Chr: WideChar;
  ExcludedChars: TSysCharSet = []): string; overload;
function CharRepresToChar(Str: string): WideChar;

function CharSetToStr(const CharSet: TSysCharSet): string;
function StrToCharSet(Str: string): TSysCharSet;

function EnumSetToStr(const EnumSet; TypeInfo: PTypeInfo): string;
procedure StrToEnumSet(const Str: string; TypeInfo: PTypeInfo; out EnumSet);

function SkipPackedShortString(Value: PShortString): Pointer;

implementation

uses
{$IFDEF UNICODE}
  Character,
{$ENDIF}
  ScCompilerMagic, ScUtils, ScConsts;

{*
  Teste si un type donné est un type managé par le compilateur
  @param TypeInfo   RTTI du type à tester
  @return True si le type est managé, False sinon
*}
function IsTypeManaged(TypeInfo: PTypeInfo): Boolean;
const
  ManagedTypeKinds = [
    tkLString, tkWString, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray
    {$IF Declared(tkUString)}, tkUString{$IFEND}
  ];
begin
  Result := (TypeInfo <> nil) and (TypeInfo.Kind in ManagedTypeKinds);

{$IFNDEF FPC}
    {$IF CompilerVersion >= 21}
{$ENDIF}
  if Result then
  begin
    case TypeInfo.Kind of
      tkArray: Result := IsTypeManaged(GetTypeData(TypeInfo).ArrayData.ElType^);
      tkRecord: Result := GetTypeData(TypeInfo).ManagedFldCount <> 0;
    end;
  end;
{$IFNDEF FPC}
  {$IFEND}
{$ENDIF}
end;

{*
  Détermine la taille d'un type à partir de ses RTTI
  Le seul cas dans lequel TypeSize est incapable de déterminer la taille du
  type (et donc renvoie -1), est si TypeInfo.Kind = tkUnknown, ce qui ne
  devrait jamais arriver.
  @param TypeInfo   RTTI du type
  @return Taille du type, ou -1 si c'est impossible à déterminer
*}
function TypeSize(TypeInfo: PTypeInfo): Integer;
const
// FPC Warning Error: Expected another 8 array elements
TypeKindToSize: array[TTypeKind] of Integer = (
    -1, 0, 1, 0, 0, 0, 0, 4, 8, 2, 4, 4, 16, 0, 0, 4, 8, 4
    {$IF Declared(tkUString)}, 4 {$IFEND}
    {$IF Declared(tkProcedure)}, 4, 4, 4 {$IFEND}
    { Added arbitrary size...to pass compilation with FPC... TODO LAZ}
    ,4,4,4,4,4,4,4,4);
  OrdTypeToSize: array[TOrdType] of Integer = (1, 1, 2, 2, 4, 4);
  FloatTypeToSize: array[TFloatType] of Integer = (4, 8, 10, 8, 8);
var
  TypeData: PTypeData;
begin
  Result := TypeKindToSize[TypeInfo.Kind];

  if Result = 0 then
  begin
    TypeData := GetTypeData(TypeInfo);
    case TypeInfo.Kind of
      tkInteger,
      tkEnumeration: Result := OrdTypeToSize[TypeData.OrdType];
      tkFloat: Result := FloatTypeToSize[TypeData.FloatType];
      tkString: Result := TypeData.MaxLength+1;
      tkArray: Result := PArrayTypeData(TypeData).Size;
      tkRecord: Result := PRecordTypeData(TypeData).Size;

      { Though tkSet has also the OrdType field, it isn't always reliable,
        since it can be out of range for large sets. }
      tkSet:
      begin
        {$IFDEF FPC}
        with GetTypeData(TypeData.CompType)^ do
        {$ELSE}
        with GetTypeData(TypeData.CompType^)^ do
        {$ENDIF}
          Result := (MaxValue - MinValue) div 8 + 1;
        if Result = 3 then
          Result := 4;
      end;
    end;
  end;
end;

{*
  Copie une variable
  Le paramètre TypeInfo n'est requis que si le type de données requiert une
  initialisation. Mais ce n'est pas une erreur de le renseigner même dans les
  autres cas.
  @param Source     Variable source
  @param Dest       Variable destination
  @param Size       Taille du type de la variable
  @param TypeInfo   RTTI du type de la variable (si requiert une initialisation)
*}
procedure CopyData(const Source; var Dest; Size: Integer;
  TypeInfo: PTypeInfo);
begin
  if IsTypeManaged(TypeInfo) then
    CopyData(Source, Dest, TypeInfo)
  else
    Move(Source, Dest, Size);
end;

{*
  Copie une variable dont le type possède des RTTI
  Utilisez cette variante de CopyData si vous ne connaissez pas la taille du
  type de données. En revanche, vous devez fournir des RTTI non-nulles du type.
  @param Source     Variable source
  @param Dest       Variable destination
  @param TypeInfo   RTTI du type de la variable
*}
procedure CopyData(const Source; var Dest; TypeInfo: PTypeInfo);
begin
  case TypeInfo.Kind of
    tkLString: LongString(Dest) := LongString(Source);
    tkWString: WideString(Dest) := WideString(Source);
    tkVariant: Variant(Dest) := Variant(Source);
    tkArray:
      with PArrayTypeData(GetTypeData(TypeInfo))^ do
        CopyArray(@Dest, @Source, ElType^, ElCount);
    tkRecord: CopyRecord(@Dest, @Source, TypeInfo);
    tkInterface: IInterface(Dest) := IInterface(Source);
    tkDynArray: DynArrayAsg(Pointer(Dest), Pointer(Source), TypeInfo);

    {$IF Declared(tkUString)}
    tkUString: UnicodeString(Dest) := UnicodeString(Source);
    {$IFEND}
  else
    Move(Source, Dest, TypeSize(TypeInfo));
  end;
end;

{*
  Teste si deux types tableau statique sont compatibles pour leur management
  @param Left    Premier type
  @param Right   Second type
  @return True s'ils sont compatibles, False sinon
*}
function IsArrayManagementCompatible(Left, Right: PArrayTypeData): Boolean;
begin
  Result := (Left.Size = Right.Size) and (Left.ElCount = Right.ElCount) and
    IsManagementCompatible(Left.ElType^, Right.ElType^);
end;

{*
  Teste si deux types record sont compatibles pour leur management
  @param Left    Premier type
  @param Right   Second type
  @return True s'ils sont compatibles, False sinon
*}
function IsRecordManagementCompatible(Left, Right: PRecordTypeData): Boolean;
var
  I: Integer;
begin
  Result := False;

  if Left.ManagedCount <> Right.ManagedCount then
    Exit;

  for I := 0 to Left.ManagedCount-1 do
  begin
    if Left.ManagedFields[I].FldOffset <> Right.ManagedFields[I].FldOffset then
      Exit;
    if not IsManagementCompatible(
      Left.ManagedFields[I].TypeRef^, Right.ManagedFields[I].TypeRef^) then
      Exit;
  end;

  Result := True;
end;

{*
  Teste si deux types tableau dynamique sont compatibles pour leur management
  @param Left    Premier type
  @param Right   Second type
  @return True s'ils sont compatibles, False sinon
*}
function IsDynArrayManagementCompatible(Left, Right: PTypeData): Boolean;
begin
  Result := (Left.elSize = Right.elSize) and
    IsManagementCompatible(Left.elType^, Right.elType^);
end;

{*
  Teste si deux types sont compatibles au niveau de leur management
  Deux types sont compatibles si et seulement si initialiser/finaliser une
  variable d'un des deux types avec les RTTI de l'autre type est valide.
  Notez que les tailles des types n'ont pas besoin d'être égales. En
  particulier, deux types de tailles quelconques et différentes sont compatibles
  s'ils ne sont pas managés.
  Un cas très particulier n'est pas traité parfaitement dans cette
  implémentation : celui ou un type record a la même structure qu'un tableau
  statique. Est alors renvoyé False au lieu de True.
  @param Left    Premier type
  @param Right   Second type
  @return True s'ils sont compatibles, False sinon
*}
function IsManagementCompatible(Left, Right: PTypeInfo): Boolean;
var
  LeftManaged, RightManaged: Boolean;
  LeftData, RightData: Pointer;
begin
  LeftManaged := IsTypeManaged(Left);
  RightManaged := IsTypeManaged(Right);

  if LeftManaged <> RightManaged then
    Result := False
  else if not LeftManaged then
    Result := True
  else if Left.Kind <> Right.Kind then
    Result := False
  else
  begin
    LeftData := GetTypeData(Left);
    RightData := GetTypeData(Right);

    case Left.Kind of
      tkArray:
        Result := IsArrayManagementCompatible(LeftData, RightData);
      tkRecord:
        Result := IsRecordManagementCompatible(LeftData, RightData);
      tkDynArray:
        Result := IsDynArrayManagementCompatible(LeftData, RightData);
    else
      Result := True;
    end;
  end;
end;

{*
  Encode a string for storing into RTTI
  @param Str   String to encode
  @return The string Str encoded in the proper form for storing into RTTI
*}
function TypeInfoEncode(const Str: string): TypeInfoString;
begin
{$IFDEF UNICODE}
  Result := UTF8EncodeToShortString(Str);
{$ELSE}
  Result := TypeInfoString(Str);
{$ENDIF}
end;

{*
  Decode a string coming from RTTI
  @param Str   String to decode
  @return The string Str decoded using the encoding of RTTI
*}
function TypeInfoDecode(const Str: TypeInfoString): string;
begin
{$IFDEF UNICODE}
  Result := UTF8ToString(Str);
{$ELSE}
  Result := string(Str);
{$ENDIF}
end;

{*
  Lit une chaîne de caractères de RTTI depuis un flux
  Cette chaîne doit avoir été écrite avec WriteTypeInfoStringToStream.
  @param Stream   Flux depuis lequel lire la chaîne
  @return La chaîne lue
*}
function ReadTypeInfoStringFromStream(Stream: TStream): string;
var
  TypeInfoStr: TypeInfoString;
begin
  Stream.ReadBuffer(TypeInfoStr, SizeOf(Byte));
  Stream.ReadBuffer(TypeInfoStr[1], Length(TypeInfoStr));
  Result := TypeInfoDecode(TypeInfoStr);
end;

{*
  Écrit une chaîne de caractères de RTTI dans un flux
  Cette chaine pourra être relue avec ReadTypeInfoStringFromStream.
  @param Stream   Flux dans lequel enregistrer la chaîne
  @param Str      Chaîne de caractères à écrire
*}
procedure WriteTypeInfoStringToStream(Stream: TStream; const Str: string);
var
  TypeInfoStr: TypeInfoString;
begin
  TypeInfoStr := TypeInfoEncode(Str);
  Stream.WriteBuffer(TypeInfoStr, Length(TypeInfoStr)+1);
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

  function IsExcluded(C: Char): Boolean;
  begin
    Result := CharInSet(C, ExcludedChars);
    {$IFDEF UNICODE}
    Result := Result or TCharacter.IsControl(C);
    {$ENDIF}
  end;

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
      if IsExcluded(Str[I]) then
      begin
        if I mod 256 = 0 then
          Result := Result+'+';

        Result := Result+'#'+IntToStr(Integer(Str[I]));
        Inc(I);
      end else
      begin
        Result := Result+'''';
        while (I <= Length(Str)) and (not IsExcluded(Str[I])) do
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
  NumChars: TSysCharSet;
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
        if Str[I] = '$' then
        begin
          CharStr := '$';
          Inc(I);
          NumChars := ['0'..'9', 'A'..'F', 'a'..'f'];
        end else
          NumChars := ['0'..'9'];
        while (I <= Length(Str)) and CharInSet(Str[I], NumChars) do
        begin
          CharStr := CharStr+Str[I];
          Inc(I);
        end;
        IntChar := StrToIntDef(CharStr, -1);
        if (IntChar >= 0) and (IntChar <= Integer(High(Char))) then
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
function CharToCharRepres(Chr: AnsiChar;
  ExcludedChars: TSysCharSet = []): string;
begin
  Result := CharToCharRepres(WideChar(Chr), ExcludedChars);
end;

{*
  Détermine la représentation Pascal d'un caractère
  @param Chr             Caractère à traiter
  @param ExcludedChars   Ensemble des caractères qu'il faut échapper
  @return Représentation Pascal de Chr
*}
function CharToCharRepres(Chr: WideChar;
  ExcludedChars: TSysCharSet = []): string;
var
  Excluded: Boolean;
begin
  Excluded := CharInSet(Chr, ExcludedChars + [#0..#31]);
  {$IFDEF UNICODE}
  Excluded := Excluded or TCharacter.IsControl(Chr);
  {$ENDIF}

  if Excluded then
    Result := '#'+IntToStr(Integer(Chr))
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
function CharRepresToChar(Str: string): WideChar;
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
        Result := WideChar(StrToInt(Copy(Str, 2, MaxInt)));
      end;
      '''':
      begin
        case Length(Str) of
          // Si 3 caractères, le troisième doit être ' et le deuxième
          // est le caractère résultat
          3: if Str[3] = '''' then
              Result := WideChar(Str[2])
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
      raise EConvertError.CreateFmt(sScWrongChar, [Str]);
  end;
end;

{*
  Détermine la représentation Pascal d'un ensemble de caractères (sans les [])
  @param CharSet   Ensemble de caractères à traiter
  @return Représentation Pascal de CharSet
*}
function CharSetToStr(const CharSet: TSysCharSet): string;
var
  I, From: Integer;
begin
  Result := '';
  I := 0;
  // On cherche d'abord le premier caractère inclus
  while (I <= 255) and (not CharInSet(Chr(I), CharSet)) do
    Inc(I);
  while I <= 255 do
  begin
    // Chr(I) est inclus
    From := I;
    // On cherche le caractère suivant qui n'est pas inclus
    while (I <= 255) and CharInSet(Chr(I), CharSet) do
      Inc(I);
    // On teste I-From, soit le nombre de caractère consécutifs
    case I-From of
      // 1 : on ajoute simplement ce caractère
      1: Result := Result+', '+CharToCharRepres(AnsiChar(From));
      // 2 : on ajoute ces deux caractères séparés par des virgules
      2: Result := Result+', '+CharToCharRepres(AnsiChar(From))+
          ', '+CharToCharRepres(AnsiChar(I-1));
    else
      // 3+ : on ajoute les deux extrèmes séparés par ..
      Result := Result+', '+CharToCharRepres(AnsiChar(From))+
        '..'+CharToCharRepres(AnsiChar(I-1));
    end;
    // on cherche le caractère suivant inclus
    repeat
      Inc(I);
    until (I > 255) or (AnsiChar(I) in CharSet);
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
  function GetCharAt: AnsiChar;
  var
    From: Integer;
  begin
    case Str[I] of
      '#':
      begin
        From := I+1;
        repeat
          Inc(I);
        until (I > Length(Str)) or (not CharInSet(Str[I], ['0'..'9']));
        Result := AnsiChar(StrToInt(Copy(Str, From, I-From)));
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
          Result := AnsiChar(Str[I]);
          Inc(I, 2);
        end;
      end;
    else
      raise EConvertError.Create('');
    end;
  end;

var
  C1, C2: AnsiChar;
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
     {$IFDEF FPC}
     TypeInfo := GetTypeData(TypeInfo).CompType;
     {$ELSE}
     TypeInfo := GetTypeData(TypeInfo).CompType^;
     {$ENDIF}

  TypeData := GetTypeData(TypeInfo);

  Result := '';

  for ByteValue := TypeData.MinValue to TypeData.MaxValue do
  begin
    if ByteValue-TypeData.MinValue in TSysByteSet(EnumSet) then
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
    {$IFDEF FPC}
    TypeInfo := GetTypeData(TypeInfo).CompType;
    {$ELSE}
    TypeInfo := GetTypeData(TypeInfo).CompType^;
    {$ENDIF}
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

      while (Index <= Len) and CharInSet(SetStr[Index], [' ', #13, #10]) do
        Inc(Index);
      BeginIndex := Index;
      while (Index <= Len) and (not CharInSet(SetStr[Index],
        [',', ' ', #13, #10])) do
        Inc(Index);

      Value := GetEnumValue(TypeInfo,
        Copy(SetStr, BeginIndex, Index-BeginIndex));
      if Value < 0 then
        raise Exception.Create('');
      Include(TSysByteSet(EnumSet), Value);

      while (Index <= Len) and CharInSet(SetStr[Index], [' ', #13, #10]) do
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

end.

