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
  D�finit des classes de listes diverses
  @author sjrd
  @version 1.0
*}
unit ScLists;
{$i ..\..\source\Sepi.inc}
interface

uses
  SysUtils, Classes, Contnrs, TypInfo;

type
  {*
    Exception d�clench�e lors d'une erreur de liste d'entiers
    @author sjrd
    @version 1.0
  *}
  EIntListError = class(EListError);

  {*
    Classe purement abstraite rendant publique la m�thode CompareStrings
    Rend publique la m�thode CompareStrings afin de pouvoir l'utiliser pour
    comparer deux cha�nes en respectant les r�gles de comparaison d'une liste.
    Cette classe ne doit pas �tre utilis�e dans une autre optique.
    @author sjrd
    @version 1.0
  *}

  { TCompareStrings }

  TCompareStrings = class(TStrings)
  public
    {$IFDEF FPC}
    FCaseSensitive: Boolean;
    {$ENDIF}
    function CompareStrings(const S1, S2: string): Integer; {$IFNDEF FPC}override;{$ENDIF}
    {$IFDEF FPC}
    Property CaseSensitive: Boolean Read FCaseSensitive Write FCaseSensitive;
    {$ENDIF}
  end;

  {*
    Classe utilitaire avec des m�thodes avanc�es de gestion de listes de cha�nes
    Propose une s�rie de m�thodes pouvant �tre appliqu�e � des instances de
    TStrings (toutes les m�thodes de recherche respectent les r�gles de
    comparaison de la liste concern�e)
    @author sjrd
    @version 1.0
  *}
  StringsOps = class
  public
    class function IndexOf(Strings: TStrings; const Str: string;
      BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;

    class function FindText(Strings: TStrings; const Str: string;
      BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
    class function FindFirstWord(Strings: TStrings; const Word: string;
      BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
    class function FindAtPos(Strings: TStrings; const SubStr: string;
      Position: Integer = 1; BeginSearchAt: Integer = 0;
      EndSearchAt: Integer = -1): Integer;

    class procedure CopyFrom(Strings: TStrings; Source: TStrings;
      Index: Integer = 0; Count: Integer = -1);
    class procedure AddFrom(Strings: TStrings; Source: TStrings;
      Index: Integer = 0; Count: Integer = -1);

    class procedure FromString(Strings: TStrings; const Str, Delim: string;
      const NotIn: string = '');
    class procedure AddFromString(Strings: TStrings;
      const Str, Delim: string;
      const NotIn: string = '');
  end;

  {*
    Extension de TStringList y ajoutant quelques fonctionnalit�s
    Am�lioration de TStringList pour lui ajouter les m�thodes de StringsOps
    ainsi qu'un index interne permettant de parcourir ais�ment toutes les
    cha�nes dans l'ordre
    @author sjrd
    @version 1.0
  *}
  TScStrings = class(TStringList)
  private
    FIndex: Integer; /// Index interne

    function GetHasMoreString: Boolean;
    procedure SetIndex(New: Integer);
  public
    constructor Create;
    constructor CreateFromFile(const FileName: TFileName);
    constructor CreateFromString(const Str, Delim: string;
      const NotIn: string = '');
    constructor CreateAssign(Source: TPersistent);

    function IndexOfEx(const Str: string; BeginSearchAt: Integer = 0;
      EndSearchAt: Integer = -1): Integer;

    function FindText(const Str: string; BeginSearchAt: Integer = 0;
      EndSearchAt: Integer = -1): Integer;
    function FindFirstWord(const Word: string; BeginSearchAt: Integer = 0;
      EndSearchAt: Integer = -1): Integer;
    function FindAtPos(const SubStr: string; Position: Integer = 1;
      BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;

    procedure CopyFrom(Source: TStrings; Index: Integer = 0;
      Count: Integer = -1);
    procedure AddFrom(Source: TStrings; Index: Integer = 0;
      Count: Integer = -1);

    procedure FromString(const Str, Delim: string;
      const NotIn: string = '');
    procedure AddFromString(const Str, Delim: string;
      const NotIn: string = '');

    procedure Reset;
    function NextString: string;

    property HasMoreString: Boolean read GetHasMoreString;
    property Index: Integer read FIndex write SetIndex;
  end;

  TScList = class;
  TScListClass = class of TScList;

  {*
    Classe abstraite de base pour la cr�ation de listes
    Classe de base pour les listes dont les �l�ments sont de taille homog�ne
    Ne cr�ez pas d'instance de TScList, mais cr�ez plut�t des instances de
    ses classes descendantes.
    Ne pas utiliser pour des listes de cha�nes, de pointeurs ou d'objets ;
    dans ces cas, utiliser respectivement TStringList (unit� Classes), TList
    (unit� Classes) et TObjectList (unit� Contnrs).
    @author sjrd
    @version 1.0
  *}
  TScList = class(TPersistent)
  private
    FStream: TMemoryStream; /// Flux m�moire interne contenant la liste
    FItemSize: Integer;     /// Taille en octets d'un �l�ment de la liste

    function GetCount: Integer;
    procedure SetCount(New: Integer);
    function GetHasMoreValue: Boolean;
    function GetIndex: Integer;
    procedure SetIndex(New: Integer);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    procedure AssignTo(Dest: TPersistent); override;

    function IsAssignClass(ScListClass: TScListClass): Boolean; virtual;

    { Les m�thodes suivantes doivent �tre appell�es par les m�thodes de m�me
      nom (sans le _) des descendants }
    procedure _Read(var Buffer);
    procedure _Write(var Buffer);
    procedure _GetItems(AIndex: Integer; var Buffer);
    procedure _SetItems(AIndex: Integer; var Buffer);
    function _Add(var Buffer): Integer;
    function _Insert(AIndex: Integer; var Buffer): Integer;
    procedure _Delete(AIndex: Integer; var Buffer);

    property ItemSize: Integer read FItemSize;
  public
    constructor Create(ItemSize: Integer);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Reset;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: TFileName);
    procedure SaveToFile(const FileName: TFileName);

    property Count: Integer read GetCount write SetCount;
    property HasMoreValue: Boolean read GetHasMoreValue;
    property Index: Integer read GetIndex write SetIndex;
  end;

  {*
    G�re une liste d'entiers sign�s
    @author sjrd
    @version 1.0
  *}
  TIntegerList = class(TScList)
  private
    function GetItems(Index: Integer): Int64;
    procedure SetItems(Index: Integer; New: Int64);
    procedure MakeItGood(var Value: Int64);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function IsAssignClass(ScListClass: TScListClass): Boolean; override;
  public
    constructor Create(IntSize: Integer = 4);
    constructor CreateAssign(Source: TPersistent; IntSize: Integer = 4);

    procedure Assign(Source: TPersistent); override;

    function Read: Int64;
    procedure Write(New: Int64);
    function Add(New: Int64): Integer;
    function Insert(Index: Integer; New: Int64): Integer;
    function Delete(Index: Integer): Int64;

    property Items[Index: Integer]: Int64
      read GetItems write SetItems; default;
  end;

  {*
    G�re une liste d'entiers non sign�s
    @author sjrd
    @version 1.0
  *}
  TUnsignedIntList = class(TScList)
  private
    function GetItems(Index: Integer): LongWord;
    procedure SetItems(Index: Integer; New: LongWord);
    procedure MakeItGood(var Value: LongWord);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function IsAssignClass(ScListClass: TScListClass): Boolean; override;
  public
    constructor Create(IntSize: Integer = 4);
    constructor CreateAssign(Source: TPersistent; IntSize: Integer = 4);

    procedure Assign(Source: TPersistent); override;

    function Read: LongWord;
    procedure Write(New: LongWord);
    function Add(New: LongWord): Integer;
    function Insert(Index: Integer; New: LongWord): Integer;
    function Delete(Index: Integer): LongWord;

    property Items[Index: Integer]: LongWord
      read GetItems write SetItems; default;
  end;

  {*
    G�re une liste de Extended
    @author sjrd
    @version 1.0
  *}
  TExtendedList = class(TScList)
  private
    function GetItems(Index: Integer): Extended;
    procedure SetItems(Index: Integer; New: Extended);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function IsAssignClass(ScListClass: TScListClass): Boolean; override;
  public
    constructor Create;
    constructor CreateAssign(Source: TPersistent);

    procedure Assign(Source: TPersistent); override;

    function Read: Extended;
    procedure Write(New: Extended);
    function Add(New: Extended): Integer;
    function Insert(Index: Integer; New: Extended): Integer;
    function Delete(Index: Integer): Extended;

    property Items[Index: Integer]: Extended
      read GetItems write SetItems; default;
  end;

  {*
    File d'attente
    TScWaitingQueue ajoute � TQueue une m�thode Cancel qui permet de supprimer
    un �l�ment de la liste, quelle que soit sa position.
    @author sjrd
    @version 1.0
  *}
  TScWaitingQueue = class(TQueue)
  public
    procedure Cancel(AItem: Pointer);
  end;

  {*
    File d'attente d'objets
    TScWaitingObjectQueue ajoute � TObjectQueue une m�thode Cancel qui permet
    de supprimer un objet de la liste, quel que soit sa position.
    @author sjrd
    @version 1.0
  *}
  TScWaitingObjectQueue = class(TObjectQueue)
  public
    procedure Cancel(AObject: TObject);
  end;

  {*
    Proc�dure de call-back pour la m�thode ForEach de TCustomValueBucketList
    @param Info       Retransmission du param�tre AInfo donn� � ForEach
    @param Key        Clef
    @param Data       Donn�es associ�es � la clef
    @param Continue   Positionner � False pour interrompre l'�num�ration
  *}
  TValueBucketProc = procedure(Info: Pointer; const Key, Data;
    var Continue: Boolean);

  {*
    M�thode de call-back pour la m�thode ForEach de TCustomValueBucketList
    @param Key        Clef
    @param Data       Donn�es associ�es � la clef
    @param Continue   Positionner � False pour interrompre l'�num�ration
  *}
  TValueBucketEvent = procedure(const Key, Data;
    var Continue: Boolean) of object;

  {$IFNDEF FPC} // FPC doesn't support "reference to" see http://www.lazarus.freepascal.org/index.php?topic=12250.0
  {$IF CompilerVersion >= 20}
{*
    R�f�rence de call-back pour la m�thode ForEach de TCustomValueBucketList
    @param Key        Clef
    @param Data       Donn�es associ�es � la clef
    @param Continue   Positionner � False pour interrompre l'�num�ration
  *}
  TValueBucketCallback = reference to procedure(const Key, Data;
    var Continue: Boolean);
  {$IFEND}
  {$ENDIF}

  {*
    Classe de base pour les tables associatives hash�es par valeur
    Au contraire de TCustomBucketList, et donc de TBucketList,
    TCustomValueBucketList recherche et fait correspondre les clefs et les
    donn�es par valeur, et non par pointeur.
    Pour cela, elle g�re elle-m�me l'allocation des donn�es en interne, et a
    donc besoin des RTTI des types de clef et de donn�es, ou � d�faut de leurs
    tailles (s'ils ne requi�rent pas d'initialisation).
    @author sjrd
    @version 1.0
  *}
  TCustomValueBucketList = class(TObject)
  private
    FBuckets: TBucketArray; /// Bo�tes de hashage
    FBucketCount: Integer;  /// Nombre de bo�tes de hashage (d�faut = 16)
    FListLocked: Boolean;   /// Indique si la liste est verrouill�e
    FClearing: Boolean;     /// Indique si la liste est en train d'�tre vid�e

    FKeySize: Integer;    /// Taille du type des clefs
    FKeyInfo: PTypeInfo;  /// RTTI du type des clefs (si need-init)
    FDataSize: Integer;   /// Taille du type des donn�es
    FDataInfo: PTypeInfo; /// RTTI du type des donn�es (si need-init)

    constructor Create(AKeySize: Integer; AKeyInfo: PTypeInfo;
      ADataSize: Integer; ADataInfo: PTypeInfo); overload;

    procedure AssignCallBack(const Key, Data; var Continue: Boolean);

    function GetIsEmpty: Boolean;
    procedure SetBucketCount(Value: Integer);
  protected
    function BucketFor(const Key): Cardinal; virtual;
    function KeyEquals(const Key1, Key2): Boolean; virtual;

    function FindItem(const Key;
      out Bucket, Index: Integer): Boolean; virtual;
    procedure AddItem(Bucket: Integer; const Key, Data); virtual;
    procedure DeleteItem(Bucket, Index: Integer); virtual;
    procedure ExtractItem(Bucket, Index: Integer; out Data); virtual;

    procedure GetData(const Key; out Data);
    procedure SetData(const Key, Data);
    procedure AddData(const Key, Data);
    procedure RemoveData(const Key);
    procedure ExtractData(const Key; out Data);
    procedure Clear;

    procedure Assign(Source: TCustomValueBucketList); virtual;

    property Buckets: TBucketArray read FBuckets;
    property BucketCount: Integer read FBucketCount write SetBucketCount;

    property KeySize: Integer read FKeySize;
    property KeyInfo: PTypeInfo read FKeyInfo;
    property DataSize: Integer read FDataSize;
    property DataInfo: PTypeInfo read FDataInfo;
  public
    constructor Create(AKeySize, ADataSize: Integer); overload;
    constructor Create(AKeySize: Integer; ADataInfo: PTypeInfo); overload;
    constructor Create(AKeyInfo: PTypeInfo; ADataSize: Integer); overload;
    constructor Create(AKeyInfo, ADataInfo: PTypeInfo); overload;
    destructor Destroy; override;

    function ForEach(Proc: TValueBucketProc;
      Info: Pointer = nil): Boolean; overload;
    function ForEach(Event: TValueBucketEvent): Boolean; overload;
    {$IFNDEF FPC}
      {$IF CompilerVersion >= 20}
    {$ENDIF}
    function ForEach(const Callback: TValueBucketCallback): Boolean; overload;
    {$IFNDEF FPC}
      {$IFEND}
    {$ENDIF}

    function Exists(const Key): Boolean;
    function Find(const Key; out Data): Boolean;

    property IsEmpty: Boolean read GetIsEmpty;
  end;

  {*
    Table associative hash�es par valeur g�n�rique
    Au contraire de TBucketList, TValueBucketList recherche et fait
    correspondre les clefs et les donn�es par valeur, et non par pointeur.
    Pour cela, elle g�re elle-m�me l'allocation des donn�es en interne, et a
    donc besoin des RTTI des types de clef et de donn�es, ou � d�faut de leurs
    tailles (s'ils ne requi�rent pas d'initialisation).
    @author sjrd
    @version 1.0
  *}
  TValueBucketList = class(TCustomValueBucketList)
  public
    procedure Get(const Key; out Data);
    procedure Put(const Key, Data);
    procedure Add(const Key, Data);
    procedure Remove(const Key);
    procedure Extract(const Key; out Data);
    procedure Clear;

    procedure Assign(Source: TCustomValueBucketList); override;

    /// [@inheritDoc]
    property BucketCount;
  end;

var
  AppParams: TScStrings; /// Liste des param�tres envoy�s � l'application

implementation

uses
  RTLConsts, ScUtils, ScStrUtils, ScCompilerMagic, ScTypInfo, ScConsts;

const
  /// Nombre par d�faut de bo�tes de hashage
  DefaultBucketCount = 16;

{------------------------}
{ Classe TCompareStrings }
{------------------------}

{*
  Compare deux cha�nes de caract�res
  @param S1   Premi�re cha�ne de caract�res
  @param S2   Seconde cha�ne de caract�res
  @return 0 si les cha�nes sont �quivalente, un nombre positif si la premi�re
          est sup�rieure � la seconde, un nombre n�gatif dans le cas inverse
*}
function TCompareStrings.CompareStrings(const S1, S2: string): Integer;
begin
  {$IFDEF FPC}
  if CaseSensitive
     then result:=AnsiCompareStr(s1,s2)
     else result:=AnsiCompareText(s1,s2);
  {$ELSE}
  Result := (inherited CompareStrings(S1, S2));
  {$ENDIF}
end;

{$REGION 'Classe StringsOps'}

{-------------------}
{ Classe StringsOps }
{-------------------}

{*
  Recherche une cha�ne dans une liste de cha�nes
  Recherche une cha�ne dans une liste de cha�nes, en d�butant et s'arr�tant �
  des index sp�cifi�s.
  @param Strings         Liste de cha�nes concern�e
  @param Str             Cha�ne � rechercher
  @param BeginSearchAt   Index � partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'� la fin si -1)
  @return Index de la premi�re cha�ne correspondant � Str, ou -1 si non trouv�e
*}
class function StringsOps.IndexOf(Strings: TStrings; const Str: string;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
begin
  with TCompareStrings(Strings) do
  begin
    // On s'assure que BeginSearchAt et EndSearchAt sont des entr�es correctes
    if BeginSearchAt < 0 then
      BeginSearchAt := 0;
    if (EndSearchAt < 0) or (EndSearchAt >= Count) then
      EndSearchAt := Count-1;

    Result := BeginSearchAt;
    while Result <= EndSearchAt do
    begin
      if CompareStrings(Str, Strings[Result]) = 0 then
        Exit
      else
        Inc(Result);
    end;
    Result := -1;
  end;
end;

{*
  Recherche un texte � l'int�rieur des cha�nes d'une liste
  Recherche un texte � l'int�rieur des cha�nes d'une liste, en d�butant et
  s'arr�tant � des index sp�cifi�s.
  @param Strings         Liste de cha�nes concern�e
  @param Str             Texte � rechercher
  @param BeginSearchAt   Index � partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'� la fin si -1)
  @return Index de la premi�re cha�ne contenant Str, ou -1 si non trouv�e
*}
class function StringsOps.FindText(Strings: TStrings; const Str: string;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
var
  I, Len: Integer;
begin
  with TCompareStrings(Strings) do
  begin
    // On s'assure que BeginSearchAt et EndSearchAt sont des entr�es correctes
    if BeginSearchAt < 0 then
      BeginSearchAt := 0;
    if (EndSearchAt < 0) or (EndSearchAt >= Count) then
      EndSearchAt := Count-1;

    Len := Length(Str);
    Result := BeginSearchAt;
    while Result <= EndSearchAt do
    begin
      for I := Length(Strings[Result])-Len+1 downto 1 do
        if CompareStrings(Str, Copy(Strings[Result], I, Len)) = 0 then
          Exit;
      Inc(Result);
    end;
    Result := -1;
  end;
end;

{*
  Recherche un mot en premi�re position d'une cha�ne d'une liste
  Recherche un mot plac� en premi�re position d'une cha�ne d'une liste (des
  espaces peuvent se trouver devant), en d�butant et s'arr�tant � des index
  sp�cifi�s.
  @param Strings         Liste de cha�nes concern�e
  @param Word            Mot � rechercher
  @param BeginSearchAt   Index � partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'� la fin si -1)
  @return Index de la premi�re cha�ne contenant Word, ou -1 si non trouv�e
*}
class function StringsOps.FindFirstWord(Strings: TStrings;
  const Word: string;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
begin
  with TCompareStrings(Strings) do
  begin
    // On s'assure que BeginSearchAt et EndSearchAt sont des entr�es correctes
    if BeginSearchAt < 0 then
      BeginSearchAt := 0;
    if (EndSearchAt < 0) or (EndSearchAt >= Count) then
      EndSearchAt := Count-1;

    Result := BeginSearchAt;
    while Result <= EndSearchAt do
    begin
      if CompareStrings(Word, GetXWord(Trim(Strings[Result]), 1)) = 0 then
        Exit
      else
        Inc(Result);
    end;
    Result := -1;
  end;
end;

{*
  Recherche une sous-cha�ne � une position sp�cifi�e les cha�nes d'une liste
  Recherche une sous-cha�ne d�butant � une position sp�cifi�e dans les cha�nes
  d'une liste, en d�butant et s'arr�tant � des index sp�cifi�s.
  @param Strings         Liste de cha�nes concern�e
  @param SubStr          Sous-cha�ne � rechercher
  @param Position        Position dans les cha�nes o� chercher la sous-cha�ne
  @param BeginSearchAt   Index � partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'� la fin si -1)
  @return Index de la premi�re cha�ne contenant Word, ou -1 si non trouv�e
*}
class function StringsOps.FindAtPos(Strings: TStrings; const SubStr: string;
  Position: Integer = 1; BeginSearchAt: Integer = 0;
  EndSearchAt: Integer = -1): Integer;
var
  Len: Integer;
begin
  with TCompareStrings(Strings) do
  begin
    // On s'assure que BeginSearchAt et EndSearchAt sont des entr�es correctes
    if BeginSearchAt < 0 then
      BeginSearchAt := 0;
    if (EndSearchAt < 0) or (EndSearchAt >= Count) then
      EndSearchAt := Count-1;

    Len := Length(SubStr);
    Result := BeginSearchAt;
    while Result <= EndSearchAt do
    begin
      if CompareStrings(SubStr, Copy(Strings[Result], Position, Len)) = 0 then
        Exit
      else
        Inc(Result);
    end;
    Result := -1;
  end;
end;

{*
  Remplit une liste de cha�nes avec des cha�nes d'une autre liste
  Remplit une liste de cha�nes avec les cha�nes d'une autre liste depuis un
  index et sur un nombre sp�cifi�s.
  @param Strings   Liste de cha�nes concern�e
  @param Source    Liste de cha�nes � partir de laquelle recopier les cha�nes
  @param Index     Index o� commencer la copie
  @param Count     Nombre de cha�nes � copier
*}
class procedure StringsOps.CopyFrom(Strings: TStrings; Source: TStrings;
  Index: Integer = 0; Count: Integer = -1);
begin
  Strings.Clear;
  AddFrom(Strings, Source, Index, Count);
end;

{*
  Ajoute � une liste de cha�nes des cha�nes d'une autre liste
  Ajoute � une liste de cha�nes les cha�nes d'une autre liste depuis un index et
  sur un nombre sp�cifi�s.
  @param Strings   Liste de cha�nes concern�e
  @param Source    Liste de cha�nes � partir de laquelle recopier les cha�nes
  @param Index     Index o� commencer la copie
  @param Count     Nombre de cha�nes � copier
*}
class procedure StringsOps.AddFrom(Strings: TStrings; Source: TStrings;
  Index: Integer = 0; Count: Integer = -1);
var
  I, EndAt: Integer;
begin
  // On s'assure que Index et Count sont des entr�es correctes
  if Index < 0 then
    Exit;
  if (Count < 0) or (Index+Count > Source.Count) then
    EndAt := Source.Count-1
  else
    EndAt := Index+Count-1;

  // On recopie les cha�nes
  for I := Index to EndAt do
    Strings.Append(Source[I]);
end;

{*
  D�coupe une cha�ne en sous-cha�nes et remplit une liste de ces sous-cha�nes
  D�coupe une cha�ne en sous-cha�nes d�limit�es par des caract�res sp�cifi�s, et
  remplit une liste de cha�nes avec ces sous-cha�nes.
  @param Strings   Liste de cha�nes dans laquelle copier les sous-cha�nes
  @param Str       Cha�ne � d�couper
  @param Delim     Caract�res qui d�limitent deux sous-cha�nes
  @param NotIn     Paires de caract�res �chappant les d�limiteurs
  @throws EListError NotIn contient un nombre impair de caract�res
  @throws EListError Delim et NotIn contiennent un m�me caract�re
*}
class procedure StringsOps.FromString(Strings: TStrings;
  const Str, Delim: string; const NotIn: string = '');
begin
  Strings.Clear;
  AddFromString(Strings, Str, Delim, NotIn);
end;

{*
  D�coupe une cha�ne en sous-cha�nes et ajoute ces sous-cha�nes � une liste
  D�coupe une cha�ne en sous-cha�nes d�limit�es par des caract�res sp�cifi�s, et
  ajoute ces sous-cha�nes � une liste de cha�nes.
  @param Strings   Liste de cha�nes � laquelle ajouter les sous-cha�nes
  @param Str       Cha�ne � d�couper
  @param Delim     Caract�res qui d�limitent deux sous-cha�nes
  @param NotIn     Paires de caract�res �chappant les d�limiteurs
  @throws EListError NotIn contient un nombre impair de caract�res
  @throws EListError Delim et NotIn contiennent un m�me caract�re
*}
class procedure StringsOps.AddFromString(Strings: TStrings;
  const Str, Delim: string; const NotIn: string = '');
var
  I, J, Len: Integer;
  NotIn1, NotIn2: string;
  C: Char;
begin
  with Strings do
  begin
    // On v�rifie que NotIn contient un nombre pair de caract�res
    if Odd(Length(NotIn)) then
      raise EListError.Create(sScNotInMustPairsOfChars);

    // On v�rifie qu'il n'y a pas d'interf�rence entre Delim et NotIn
    for I := 1 to Length(NotIn) do
      if Pos(NotIn[I], Delim) > 0 then
        raise EListError.Create(sScDelimMustDifferentThanNotIn);

    // S�paration de NotIn en NotIn1 et NotIn2
    NotIn1 := '';
    NotIn2 := '';
    for I := 1 to Length(NotIn) do
    begin
      if (I mod 2) = 1 then
        NotIn1 := NotIn1+NotIn[I]
      else
        NotIn2 := NotIn2+NotIn[I];
    end;

    Len := Length(Str);

    I := 1;
    while True do
    begin
      // On boucle jusqu'� trouver un caract�re qui n'est pas dans Delim
      // On ignore de ce fait plusieurs caract�res de Delim � la suite
      while (I <= Len) and (Pos(Str[I], Delim) <> 0) do
        Inc(I);
      if (I > Len) then
        Break;

      // On recherche le caract�re de Delim suivant
      J := I;
      while (J <= Len) and (Pos(Str[J], Delim) = 0) do
      begin
        // Si on trouve un caract�re de NotIn1, on boucle jusqu'� trouver le
        // caract�re correspondant de NotIn2
        if Pos(Str[J], NotIn1) > 0 then
        begin
          C := NotIn2[Pos(Str[J], NotIn1)];
          Inc(J);
          while (J <= Len) and (Str[J] <> C) do
            Inc(J);
        end;
        Inc(J);
      end;

      // On ajoute la sous-cha�ne rep�r�e par les caract�res de Delim
      Add(Copy(Str, I, J-I));
      I := J;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'Classe TScStrings'}

{-------------------}
{ Classe TScStrings }
{-------------------}

{*
  Cr�e une nouvelle instance de TScStrings
*}
constructor TScStrings.Create;
begin
  inherited Create;
  FIndex := 0;
end;

{*
  Cr�e une nouvelle instance de TScStrings charg�e � partir d'un fichier
  @param FileName   Nom du fichier � partir duquel charger la liste de cha�nes
*}
constructor TScStrings.CreateFromFile(const FileName: TFileName);
begin
  Create;
  LoadFromFile(FileName);
end;

{*
  Cr�e une nouvelle instance de TScStrings � partir d'une cha�ne d�coup�e
  @param Str       Cha�ne � d�couper
  @param Delim     Caract�res qui d�limitent deux sous-cha�nes
  @param NotIn     Paires de caract�res �chappant les d�limiteurs
  @throws EListError NotIn contient un nombre impair de caract�res
  @throws EListError Delim et NotIn contiennent un m�me caract�re
*}
constructor TScStrings.CreateFromString(const Str, Delim: string;
  const NotIn: string = '');
begin
  Create;
  FromString(Str, Delim, NotIn);
end;

{*
  Cr�e une nouvelle instance de TScStrings assign�e � partir d'une source
  @param Source   Objet source � copier dans la liste de cha�nes
*}
constructor TScStrings.CreateAssign(Source: TPersistent);
begin
  Create;
  Assign(Source);
end;

{*
  Indique s'il y a encore des cha�nes � lire
  @return True s'il y a encore des cha�nes � lire, False sinon
*}
function TScStrings.GetHasMoreString: Boolean;
begin
  Result := Index < Count;
end;

{*
  Modifie l'index interne
  @param New   Nouvelle valeur de l'index interne
*}
procedure TScStrings.SetIndex(New: Integer);
begin
  if New >= 0 then
    FIndex := New;
end;

{*
  Recherche une cha�ne dans la liste de cha�nes
  Recherche une cha�ne dans la liste de cha�nes, en d�butant et s'arr�tant �
  des index sp�cifi�s.
  @param Str             Cha�ne � rechercher
  @param BeginSearchAt   Index � partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'� la fin si -1)
  @return Index de la premi�re cha�ne correspondant � Str, ou -1 si non trouv�e
*}
function TScStrings.IndexOfEx(const Str: string; BeginSearchAt: Integer = 0;
  EndSearchAt: Integer = -1): Integer;
begin
  Result := StringsOps.IndexOf(Self, Str, BeginSearchAt, EndSearchAt);
end;

{*
  Recherche un texte � l'int�rieur des cha�nes de la liste
  Recherche un texte � l'int�rieur des cha�nes de la liste, en d�butant et
  s'arr�tant � des index sp�cifi�s.
  @param Str             Texte � rechercher
  @param BeginSearchAt   Index � partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'� la fin si -1)
  @return Index de la premi�re cha�ne contenant Str, ou -1 si non trouv�e
*}
function TScStrings.FindText(const Str: string; BeginSearchAt: Integer = 0;
  EndSearchAt: Integer = -1): Integer;
begin
  Result := StringsOps.FindText(Self, Text, BeginSearchAt, EndSearchAt);
end;

{*
  Recherche un mot en premi�re position d'une cha�ne de la liste
  Recherche un mot plac� en premi�re position d'une cha�ne de la liste (des
  espaces peuvent se trouver devant), en d�butant et s'arr�tant � des index
  sp�cifi�s.
  @param Word            Mot � rechercher
  @param BeginSearchAt   Index � partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'� la fin si -1)
  @return Index de la premi�re cha�ne contenant Word, ou -1 si non trouv�e
*}
function TScStrings.FindFirstWord(const Word: string;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
begin
  Result := StringsOps.FindFirstWord(Self, Word, BeginSearchAt, EndSearchAt);
end;

{*
  Recherche une sous-cha�ne � une position sp�cifi�e les cha�nes de la liste
  Recherche une sous-cha�ne d�butant � une position sp�cifi�e dans les cha�nes
  de la liste, en d�butant et s'arr�tant � des index sp�cifi�s.
  @param SubStr          Sous-cha�ne � rechercher
  @param Position        Position dans les cha�nes o� chercher la sous-cha�ne
  @param BeginSearchAt   Index � partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'� la fin si -1)
  @return Index de la premi�re cha�ne contenant Word, ou -1 si non trouv�e
*}
function TScStrings.FindAtPos(const SubStr: string; Position: Integer = 1;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
begin
  Result := StringsOps.FindAtPos(Self, SubStr, Position, BeginSearchAt,
    EndSearchAt);
end;

{*
  Remplit la liste de cha�nes avec des cha�nes d'une autre liste
  Remplit la liste de cha�nes avec les cha�nes d'une autre liste depuis un index
  et sur un nombre sp�cifi�s.
  @param Source    Liste de cha�nes � partir de laquelle recopier les cha�nes
  @param Index     Index o� commencer la copie
  @param Count     Nombre de cha�nes � copier
*}
procedure TScStrings.CopyFrom(Source: TStrings; Index: Integer = 0;
  Count: Integer = -1);
begin
  StringsOps.CopyFrom(Self, Source, Index, Count);
end;

{*
  Ajoute � la liste de cha�nes des cha�nes d'une autre liste
  Ajoute � la liste de cha�nes les cha�nes d'une autre liste depuis un index et
  sur un nombre sp�cifi�s.
  @param Source    Liste de cha�nes � partir de laquelle recopier les cha�nes
  @param Index     Index o� commencer la copie
  @param Count     Nombre de cha�nes � copier
*}
procedure TScStrings.AddFrom(Source: TStrings; Index: Integer = 0;
  Count: Integer = -1);
begin
  StringsOps.AddFrom(Self, Source, Index, Count);
end;

{*
  D�coupe une cha�ne en sous-cha�nes et remplit la liste de ces sous-cha�nes
  D�coupe une cha�ne en sous-cha�nes d�limit�es par des caract�res sp�cifi�s, et
  remplit la liste de cha�nes avec ces sous-cha�nes.
  @param Str       Cha�ne � d�couper
  @param Delim     Caract�res qui d�limitent deux sous-cha�nes
  @param NotIn     Paires de caract�res �chappant les d�limiteurs
  @throws EListError NotIn contient un nombre impair de caract�res
  @throws EListError Delim et NotIn contiennent un m�me caract�re
*}
procedure TScStrings.FromString(const Str, Delim: string;
  const NotIn: string = '');
begin
  StringsOps.FromString(Self, Str, Delim, NotIn);
end;

{*
  D�coupe une cha�ne en sous-cha�nes et ajoute ces sous-cha�nes � la liste
  D�coupe une cha�ne en sous-cha�nes d�limit�es par des caract�res sp�cifi�s, et
  ajoute ces sous-cha�nes � la liste de cha�nes.
  @param Str       Cha�ne � d�couper
  @param Delim     Caract�res qui d�limitent deux sous-cha�nes
  @param NotIn     Paires de caract�res �chappant les d�limiteurs
  @throws EListError NotIn contient un nombre impair de caract�res
  @throws EListError Delim et NotIn contiennent un m�me caract�re
*}
procedure TScStrings.AddFromString(const Str, Delim: string;
  const NotIn: string = '');
begin
  StringsOps.AddFromString(Self, Str, Delim, NotIn);
end;

{*
  Remet � 0 l'index interne
*}
procedure TScStrings.Reset;
begin
  FIndex := 0;
end;

{*
  Lit la cha�ne suivante dans la liste de cha�ne
  @return La cha�ne suivante
*}
function TScStrings.NextString: string;
begin
  if HasMoreString then
    Result := Strings[Index]
  else
    Result := '';
  Inc(FIndex);
end;

{$ENDREGION}

{$REGION 'Classe TScList'}

{----------------}
{ Classe TScList }
{----------------}

{*
  Cr�e une nouvelle instance de TScList
  @param ItemSize   Taille en octets d'un �l�ment de la liste
*}
constructor TScList.Create(ItemSize: Integer);
begin
  FStream := TMemoryStream.Create;
  FItemSize := ItemSize;
end;

{*
  D�truit l'instance
*}
destructor TScList.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

{*
  Nombre d'�l�ments de la liste
  @return Nombre d'�l�ments de la liste
*}
function TScList.GetCount: Integer;
begin
  Result := FStream.Size div FItemSize;
end;

{*
  Modifie le nombre d'�l�ments de la liste
  @param New   Nouveau nombre d'�l�ments de la liste
*}
procedure TScList.SetCount(New: Integer);
begin
  if New <= 0 then
    FStream.SetSize(0)
  else
    FStream.SetSize(New*FItemSize);
end;

{*
  Indique s'il y a encore des �l�ments � lire
  @return True s'il y a encore des �l�ments � lire, False sinon
*}
function TScList.GetHasMoreValue: Boolean;
begin
  Result := FStream.Position < FStream.Size;
end;

{*
  Index interne de la liste
  @return Index interne de la liste
*}
function TScList.GetIndex: Integer;
begin
  Result := FStream.Position div FItemSize;
end;

{*
  Modifie l'index interne de la liste
  @param New   Nouvelle valeur de l'index interne
  @throws EListError Index de liste hors bornes
*}
procedure TScList.SetIndex(New: Integer);
begin
  // On v�rifie que New est bien un index correct
  if (New < 0) or (New > Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [New]);

  FStream.Position := New*FItemSize;
end;

{*
  Propose une interface pour les m�thodes traitant des donn�es non publi�es
  @param Filer   Objet lecteur ou �crivain
*}
procedure TScList.DefineProperties(Filer: TFiler);
begin
  inherited;
  { La ligne suivante cr�e une propri�t� publi�e fictive qui permet
    d'enregistrer le contenu via WriteComponent et de le lire via
    ReadComponent. Don't localize. }
  Filer.DefineBinaryProperty('Items', LoadFromStream, SaveToStream, True);
end;

{*
  Copie les propri�t�s d'un objet dans l'objet destination
  @param Dest   Objet destination dans lequel copier la liste
*}
procedure TScList.AssignTo(Dest: TPersistent);
begin
  { On autorise l'assignation ssi Dest.ClassType fait partie de la liste des
    classes d'assignation et que les tailles d'�lements sont les m�mes }
  if (Dest is TScList) and
    (TScList(Dest).FItemSize = FItemSize) and
    IsAssignClass(TScListClass(Dest.ClassType)) then
  begin
    TScList(Dest).FStream.LoadFromStream(FStream);
  end else
    inherited;
end;

{*
  Indique si une classe est une classe d'assignation de la liste
  Les classes descendantes de TScList peuvent surcharger cette m�thode pour
  indiquer quelles sont les classes d'assignation de la liste.
  @param ScListClass   Classe � tester
  @return True si ScListClass est une classe d'assignation, False sinon
*}
function TScList.IsAssignClass(ScListClass: TScListClass): Boolean;
begin
  // Si on remonte jusqu'ici, c'est que ce n'est pas une classe d'assignation
  Result := False;
end;

{*
  Lit un �l�ment de la liste � la position courante
  @param Buffer   Buffer non typ� dans lequel enregistrer l'�l�ment lu
*}
procedure TScList._Read(var Buffer);
begin
  FStream.ReadBuffer(Buffer, FItemSize);
end;

{*
  �crit un �l�ment dans la liste � la position courante
  Si l'index interne est � la fin de la liste, celle-ci est agrandie pour
  acceuillir le nouvel �l�ment.
  @param Buffer   Buffer non typ� contenant l'�l�ment � �crire
*}
procedure TScList._Write(var Buffer);
begin
  FStream.WriteBuffer(Buffer, FItemSize);
end;

{*
  Lit un �l�ment identifi� par son index
  @param AIndex   Index de l'�l�ment � lire
  @param Buffer   Buffer non typ� dans lequel enregistr� l'�l�ment lu
  @throws EListError Index de liste hors bornes
*}
procedure TScList._GetItems(AIndex: Integer; var Buffer);
begin
  // On v�rifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);

  Index := AIndex;
  _Read(Buffer);
end;

{*
  Modifie un �l�ment identifi� par son index
  @param AIndex   Index de l'�l�ment � modifier
  @param Buffer   Buffer non typ� contenant l'�l�ment � �crire
  @throws EListError Index de liste hors bornes
*}
procedure TScList._SetItems(AIndex: Integer; var Buffer);
begin
  // On v�rifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);

  Index := AIndex;
  _Write(Buffer);
end;

{*
  Ajoute un �l�ment
  @param Buffer   Buffer non typ� contenant l'�l�ment � ajouter
  @return Index de l'�l�ment ajout�
*}
function TScList._Add(var Buffer): Integer;
begin
  Result := Count;
  Index := Result;
  _Write(Buffer);
end;

{*
  Insert un nouvel �l�ment � une position donn�e
  @param AIndex   Index o� ins�rer l'�l�ment
  @param Buffer   Buffer non typ� contenant l'�l�ment � ins�rer
  @return Index de l'�l�ment ins�r�
  @throws EListError Index de liste hors bornes
*}
function TScList._Insert(AIndex: Integer; var Buffer): Integer;
var
  Temp: TMemoryStream;
begin
  // On v�rifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex > Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);

  // Si AIndex vaut Count, on appelle _Add, sinon, on effectue le traitement
  if AIndex = Count then
    Result := _Add(Buffer)
  else
  begin
    Temp := TMemoryStream.Create;
    try
      Result := AIndex;

      // On copie tous les �l�ments � partir de AIndex dans Temp
      Index := AIndex;
      Temp.CopyFrom(FStream, FStream.Size-FStream.Position);

      // On agrandi la liste
      Count := Count+1;

      // On �crit le nouvel �l�ment et � sa suite le contenu de Temp
      Index := AIndex;
      _Write(Buffer);
      FStream.CopyFrom(Temp, 0);
    finally
      Temp.Free;
    end;
  end;
end;

{*
  Supprime un �l�ment de la liste
  @param AIndex   Index de l'�l�ment � supprimer
  @param Buffer   Buffer non typ� dans lequel enregistr� l'�l�ment supprim�
  @throws EListError Index de liste hors bornes
*}
procedure TScList._Delete(AIndex: Integer; var Buffer);
var
  Temp: TMemoryStream;
begin
  // On v�rifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);

  Temp := TMemoryStream.Create;
  try
    // On lit la valeur de retour � l'emplacement qui va �tre supprim�
    Index := AIndex;
    _Read(Buffer);

    // On copie tous les �l�ments apr�s celui-l� dans Temp
    if FStream.Position <> FStream.Size then // Pour �viter le CopyFrom(0)
      Temp.CopyFrom(FStream, FStream.Size-FStream.Position);

    // On r�duit la liste
    Count := Count-1;

    // On recopie le contenu de Temp � partir de l'emplacement qui a �t� supprim�
    Index := AIndex;
    FStream.CopyFrom(Temp, 0);
  finally
    Temp.Free;
  end;
end;

{*
  Copie le contenu d'un autre objet similaire
  Appelez Assign pour copier les propri�t�s ou d'autres attributs d'un objet sur
  un autre.
  @param Source   Objet source � copier
*}
procedure TScList.Assign(Source: TPersistent);
begin
  { On autorise l'assignation ssi Source.ClassType fait partie de la liste des
    classes d'assignation et que les tailles d'�lements sont les m�mes. }
  if (Source is TScList) and
    (TScList(Source).FItemSize = FItemSize) and
    IsAssignClass(TScListClass(Source.ClassType)) then
  begin
    FStream.LoadFromStream(TScList(Source).FStream);
  end else
    inherited;
end;

{*
  Vide la liste
*}
procedure TScList.Clear;
begin
  Count := 0;
end;

{*
  Remet � 0 l'index interne
*}
procedure TScList.Reset;
begin
  Index := 0;
end;

{*
  Charge la liste depuis un flux
  La liste doit avoir �t� enregistr�e au moyen de SaveToStream.
  @param Stream   Flux depuis lequel lire la liste
*}
procedure TScList.LoadFromStream(Stream: TStream);
var
  Size: Int64;
begin
  // On lit la taille sur 8 octets (Int64)
  Stream.Read(Size, 8);
  FStream.Size := Size;
  FStream.Position := 0;
  if Size > 0 then // Pour �viter le CopyFrom(0)
    FStream.CopyFrom(Stream, Size);
end;

{*
  Enregistre la liste dans un flux
  La liste pourra �tre relue avec LoadFromStream.
  @param Stream   Flux dans lequel enregistrer la liste
*}
procedure TScList.SaveToStream(Stream: TStream);
var
  Size: Int64;
begin
  // On �crit la taille sur 8 octets (Int64)
  Size := FStream.Size;
  Stream.Write(Size, 8);
  if Size > 0 then
    Stream.CopyFrom(FStream, 0);
end;

{*
  Charge la liste depuis un fichier
  La liste doit avoir �t� enregistr�e au moyen de SaveToFile.
  @param FileName   Nom du fichier depuis lequel lire la liste
*}
procedure TScList.LoadFromFile(const FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

{*
  Enregistre la liste dans un fichier
  La liste pourra �tre relue avec LoadFromFile.
  @param FileName   Nom du fichier dans lequel enregistrer la liste
*}
procedure TScList.SaveToFile(const FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

{$ENDREGION}

{$REGION 'Classe TIntegerList'}

{---------------------}
{ Classe TIntegerList }
{---------------------}

{*
  Cr�e une nouvelle instance de TIntegerList
  @param IntSize   Taille en octets des entiers (entre 1 et 8 inclus)
  @throws EIntListError Taille d'entier incorrecte
*}
constructor TIntegerList.Create(IntSize: Integer = 4);
begin
  // On v�rifie que IntSize est entre 1 et 8 inclus
  if (IntSize < 1) or (IntSize > 8) then
    raise EIntListError.CreateFmt(sScWrongIntSize, [IntSize]);

  inherited Create(IntSize);
end;

{*
  Cr�e une nouvelle instance de TIntegerList, copie d'une autre source
  @param Source    Objet source � copier
  @param IntSize   Taille en octets des entiers (entre 1 et 8 inclus)
  @throws EIntListError Taille d'entier incorrecte
*}
constructor TIntegerList.CreateAssign(Source: TPersistent;
  IntSize: Integer = 4);
begin
  Create(IntSize);
  Assign(Source);
end;

{*
  Tableau index� par leurs index des �l�ments de la liste
  @param Index   Index de l'�l�ment � lire
  @return Valeur de l'�l�ment � l'index Index
  @throws EListError Index de liste hors bornes
*}
function TIntegerList.GetItems(Index: Integer): Int64;
begin
  _GetItems(Index, Result);
  MakeItGood(Result);
end;

{*
  Modifie le tableau index� par leurs index des �l�ments de la liste
  @param Index   Index de l'�l�ment � modifier
  @param New     Nouvelle valeur de l'�l�ment � l'index Index
  @throws EListError Index de liste hors bornes
*}
procedure TIntegerList.SetItems(Index: Integer; New: Int64);
begin
  _SetItems(Index, New);
end;

{*
  S'assure que Value contient la valeur exacte selon la taille stock�e
  Cette proc�dure remplit les octets non stock�s de mani�re � transformer un
  entier stock� sur un nombre quelconque d'octets en Int64 (sur 8 octets)
  @param Value   Valeur � traiter
*}
procedure TIntegerList.MakeItGood(var Value: Int64);
type
  TRecVal = record
    case Integer of
      1: (Int: Int64);
      2: (Ints: array[1..8] of Shortint);
  end;
var
  RecVal: TRecVal;
  I: Integer;
  FillWith: Shortint;
begin
  // On s'�vite un travail inutile si Value est d�j� stock� sur 8 octets
  if ItemSize = 8 then
    Exit;

  // On initialise RecVal.Int (et donc aussi RecVal.Ints)
  RecVal.Int := Value;

  { Si le Shortint � la position ItemSize (c-�-d l'octet de poids le plus
    fort stock�) est n�gatif (c-�-d si le nombre complet est n�gatif),
    on remplit les suivant avec -1 (11111111b) et sinon avec 0 (00000000b) }
  if RecVal.Ints[ItemSize] < 0 then
    FillWith := -1
  else
    FillWith := 0;

  // On remplit les les octets non stock�s avec la valeur de Remplis
  for I := ItemSize+1 to 8 do
    RecVal.Ints[I] := FillWith;

  // On r�actualise Value
  Value := RecVal.Int;
end;

{*
  Copie les propri�t�s d'un objet dans l'objet destination
  @param Dest   Objet destination dans lequel copier la liste
*}
procedure TIntegerList.AssignTo(Dest: TPersistent);
var
  DestStrings: TStrings;
begin
  // Si Dest est un TStrings on le remplit avec les formes cha�nes des �l�ments
  if Dest is TStrings then
  begin
    DestStrings := TStrings(Dest);
    DestStrings.Clear;
    // Le principe "Reset-HasMoreValue-Read" �tant plus rapide que
    // "for 0 to Count-1" avec TScList, on l'utilise pour remplire le TStrings
    Reset;
    while HasMoreValue do
      DestStrings.Append(IntToStr(Read));
  end else
    inherited;
end;

{*
  Indique si une classe est une classe d'assignation de la liste
  Les classes descendantes de TScList peuvent surcharger cette m�thode pour
  indiquer quelles sont les classes d'assignation de la liste.
  @param ScListClass   Classe � tester
  @return True si ScListClass est une classe d'assignation, False sinon
*}
function TIntegerList.IsAssignClass(ScListClass: TScListClass): Boolean;
begin
  if ScListClass.InheritsFrom(TIntegerList) then
    Result := True
  else
    Result := inherited IsAssignClass(ScListClass);
end;

{*
  Copie le contenu d'un autre objet similaire
  Appelez Assign pour copier les propri�t�s ou d'autres attributs d'un objet sur
  un autre.
  @param Source   Objet source � copier
*}
procedure TIntegerList.Assign(Source: TPersistent);
var
  SourceStrings: TStrings;
  I: Integer;
begin
  // Si Source est un TStrings, on convertit les cha�nes en entiers
  if Source is TStrings then
  begin
    SourceStrings := TStrings(Source);
    Clear;
    for I := 0 to SourceStrings.Count-1 do
      Add(StrToInt64(SourceStrings[I]));
  end else
    inherited;
end;

{*
  Lit un �l�ment de la liste � la position courante
  @return L'�l�ment lu
*}
function TIntegerList.Read: Int64;
begin
  _Read(Result);
  MakeItGood(Result);
end;

{*
  �crit un �l�ment dans la liste � la position courante
  @param New   �l�m�nt � �crire
*}
procedure TIntegerList.Write(New: Int64);
begin
  _Write(New);
end;

{*
  Ajoute un �l�ment � la liste
  @param New   �l�m�nt � ajouter
  @return Index de l'�l�ment ajout�
*}
function TIntegerList.Add(New: Int64): Integer;
begin
  Result := _Add(New);
end;

{*
  Insert un �l�ment dans la liste � un index sp�cifi�
  @param Index   Index o� ins�rer l'�l�ment
  @param New     �l�m�nt � ins�rer
  @return Index de l'�l�ment ins�r�
*}
function TIntegerList.Insert(Index: Integer; New: Int64): Integer;
begin
  Result := _Insert(Index, New);
end;

{*
  Supprime un �l�ment de la liste
  @param Index   Index de l'�l�ment � supprimer
  @return L'�l�ment supprim�
*}
function TIntegerList.Delete(Index: Integer): Int64;
begin
  _Delete(Index, Result);
  MakeItGood(Result);
end;

{$ENDREGION}

{$REGION 'Classe TUnsignedIntList'}

{-------------------------}
{ Classe TUnsignedIntList }
{-------------------------}

{*
  Cr�e une nouvelle instance de TUnsignedIntList
  @param IntSize   Taille en octets des entiers (entre 1 et 4 inclus)
  @throws EIntListError Taille d'entier incorrecte
*}
constructor TUnsignedIntList.Create(IntSize: Integer = 4);
begin
  // On v�rifie que IntSize est entre 1 et 4 inclus
  if (IntSize < 1) or (IntSize > 4) then
    raise EIntListError.CreateFmt(sScWrongIntSize, [IntSize]);

  inherited Create(IntSize);
end;

{*
  Cr�e une nouvelle instance de TUnsignedIntList, copie d'une autre source
  @param Source    Objet source � copier
  @param IntSize   Taille en octets des entiers (entre 1 et 4 inclus)
  @throws EIntListError Taille d'entier incorrecte
*}
constructor TUnsignedIntList.CreateAssign(Source: TPersistent;
  IntSize: Integer = 4);
begin
  Create(IntSize);
  Assign(Source);
end;

{*
  Tableau index� par leurs index des �l�ments de la liste
  @param Index   Index de l'�l�ment � lire
  @return Valeur de l'�l�ment � l'index Index
  @throws EListError Index de liste hors bornes
*}
function TUnsignedIntList.GetItems(Index: Integer): LongWord;
begin
  _GetItems(Index, Result);
  MakeItGood(Result);
end;

{*
  Modifie le tableau index� par leurs index des �l�ments de la liste
  @param Index   Index de l'�l�ment � modifier
  @param New     Nouvelle valeur de l'�l�ment � l'index Index
  @throws EListError Index de liste hors bornes
*}
procedure TUnsignedIntList.SetItems(Index: Integer; New: LongWord);
begin
  _SetItems(Index, New);
end;

{*
  S'assure que Value contient la valeur exacte selon la taille stock�e
  Cette proc�dure remplit les octets non stock�s de mani�re � transformer un
  entier stock� sur un nombre quelconque d'octets en LongWord (sur 4 octets)
  @param Value   Valeur � traiter
*}
procedure TUnsignedIntList.MakeItGood(var Value: LongWord);
type
  TRecVal = record
    case Integer of
      1: (Int: LongWord);
      2: (Ints: array[1..4] of Byte);
  end;
var
  RecVal: TRecVal;
  I: Integer;
begin
  // On s'�vite un travail inutile si Value est d�j� stock� sur 4 octets
  if ItemSize = 4 then
    Exit;

  // On initialise RecVal.Int (et donc aussi RecVal.Ints)
  RecVal.Int := Value;

  // On remplit les octets non stock�s avec des 0
  for I := ItemSize+1 to 4 do
    RecVal.Ints[I] := 0;

  // On r�actualise Value
  Value := RecVal.Int;
end;

{*
  Copie les propri�t�s d'un objet dans l'objet destination
  @param Dest   Objet destination dans lequel copier la liste
*}
procedure TUnsignedIntList.AssignTo(Dest: TPersistent);
var
  DestStrings: TStrings;
begin
  // Si Dest est un TStrings on le remplit avec les formes cha�nes des �l�ments
  if Dest is TStrings then
  begin
    DestStrings := TStrings(Dest);
    DestStrings.Clear;
    // Le principe "Reset-HasMoreValue-Read" �tant plus rapide que
    // "for 0 to Count-1" avec TScList, on l'utilise pour remplire le TStrings
    Reset;
    while HasMoreValue do
      DestStrings.Append(IntToStr(Read));
  end else
    inherited;
end;

{*
  Indique si une classe est une classe d'assignation de la liste
  Les classes descendantes de TScList peuvent surcharger cette m�thode pour
  indiquer quelles sont les classes d'assignation de la liste.
  @param ScListClass   Classe � tester
  @return True si ScListClass est une classe d'assignation, False sinon
*}
function TUnsignedIntList.IsAssignClass(ScListClass: TScListClass): Boolean;
begin
  if ScListClass.InheritsFrom(TUnsignedIntList) then
    Result := True
  else
    Result := inherited IsAssignClass(ScListClass);
end;

{*
  Copie le contenu d'un autre objet similaire
  Appelez Assign pour copier les propri�t�s ou d'autres attributs d'un objet sur
  un autre.
  @param Source   Objet source � copier
*}
procedure TUnsignedIntList.Assign(Source: TPersistent);
var
  SourceStrings: TStrings;
  I: Integer;
  Val: Int64;
begin
  // Si Source est un TStrings, on convertit les cha�nes en entiers
  if Source is TStrings then
  begin
    SourceStrings := TStrings(Source);
    Clear;
    for I := 0 to SourceStrings.Count-1 do
    begin
      Val := StrToInt64(SourceStrings[I]);
      // On v�rifie que Val est bien un LongWord
      if (Val < 0) or (Val > High(LongWord)) then
        raise EConvertError.CreateFmt(sScWrongLongWord, [SourceStrings[I]]);
      Add(Val);
    end;
  end else
    inherited;
end;

{*
  Lit un �l�ment de la liste � la position courante
  @return L'�l�ment lu
*}
function TUnsignedIntList.Read: LongWord;
begin
  _Read(Result);
  MakeItGood(Result);
end;

{*
  �crit un �l�ment dans la liste � la position courante
  @param New   �l�m�nt � �crire
*}
procedure TUnsignedIntList.Write(New: LongWord);
begin
  _Write(New);
end;

{*
  Ajoute un �l�ment � la liste
  @param New   �l�m�nt � ajouter
  @return Index de l'�l�ment ajout�
*}
function TUnsignedIntList.Add(New: LongWord): Integer;
begin
  Result := _Add(New);
end;

{*
  Insert un �l�ment dans la liste � un index sp�cifi�
  @param Index   Index o� ins�rer l'�l�ment
  @param New     �l�m�nt � ins�rer
  @return Index de l'�l�ment ins�r�
*}
function TUnsignedIntList.Insert(Index: Integer; New: LongWord): Integer;
begin
  Result := _Insert(Index, New);
end;

{*
  Supprime un �l�ment de la liste
  @param Index   Index de l'�l�ment � supprimer
  @return L'�l�ment supprim�
*}
function TUnsignedIntList.Delete(Index: Integer): LongWord;
begin
  _Delete(Index, Result);
  MakeItGood(Result);
end;

{$ENDREGION}

{$REGION 'Classe TExtendedList'}

{----------------------}
{ Classe TExtendedList }
{----------------------}

{*
  Cr�e une nouvelle instance de TExtendedList
*}
constructor TExtendedList.Create;
begin
  inherited Create(SizeOf(Extended));
end;

{*
  Cr�e une nouvelle instance de TExtendedList, copie d'une autre source
  @param Source    Objet source � copier
*}
constructor TExtendedList.CreateAssign(Source: TPersistent);
begin
  Create;
  Assign(Source);
end;

{*
  Tableau index� par leurs index des �l�ments de la liste
  @param Index   Index de l'�l�ment � lire
  @return Valeur de l'�l�ment � l'index Index
  @throws EListError Index de liste hors bornes
*}
function TExtendedList.GetItems(Index: Integer): Extended;
begin
  _GetItems(Index, Result);
end;

{*
  Modifie le tableau index� par leurs index des �l�ments de la liste
  @param Index   Index de l'�l�ment � modifier
  @param New     Nouvelle valeur de l'�l�ment � l'index Index
  @throws EListError Index de liste hors bornes
*}
procedure TExtendedList.SetItems(Index: Integer; New: Extended);
begin
  _SetItems(Index, New);
end;

{*
  Copie les propri�t�s d'un objet dans l'objet destination
  @param Dest   Objet destination dans lequel copier la liste
*}
procedure TExtendedList.AssignTo(Dest: TPersistent);
var
  DestStrings: TStrings;
begin
  // Si Dest est un TStrings on le remplit avec les formes cha�nes des �l�ments
  if Dest is TStrings then
  begin
    DestStrings := TStrings(Dest);
    DestStrings.Clear;
    // Le principe "Reset-HasMoreValue-Read" �tant plus rapide que
    // "for 0 to Count-1" avec TScList, on l'utilise pour remplire le TStrings
    Reset;
    while HasMoreValue do
      DestStrings.Append(FloatToStr(Read));
  end else
    inherited;
end;

{*
  Indique si une classe est une classe d'assignation de la liste
  Les classes descendantes de TScList peuvent surcharger cette m�thode pour
  indiquer quelles sont les classes d'assignation de la liste.
  @param ScListClass   Classe � tester
  @return True si ScListClass est une classe d'assignation, False sinon
*}
function TExtendedList.IsAssignClass(ScListClass: TScListClass): Boolean;
begin
  if ScListClass.InheritsFrom(TExtendedList) then
    Result := True
  else
    Result := inherited IsAssignClass(ScListClass);
end;

{*
  Copie le contenu d'un autre objet similaire
  Appelez Assign pour copier les propri�t�s ou d'autres attributs d'un objet sur
  un autre.
  @param Source   Objet source � copier
*}
procedure TExtendedList.Assign(Source: TPersistent);
var
  SourceStrings: TStrings;
  I: Integer;
begin
  // Si Source est un TStrings, on convertit les cha�nes en nombres flottants
  if Source is TStrings then
  begin
    Clear;
    SourceStrings := TStrings(Source);
    for I := 0 to SourceStrings.Count-1 do
      Write(StrToFloat(SourceStrings[I]));
  end else
    inherited;
end;

{*
  Lit un �l�ment de la liste � la position courante
  @return L'�l�ment lu
*}
function TExtendedList.Read: Extended;
begin
  _Read(Result);
end;

{*
  �crit un �l�ment dans la liste � la position courante
  @param New   �l�m�nt � �crire
*}
procedure TExtendedList.Write(New: Extended);
begin
  _Write(New);
end;

{*
  Ajoute un �l�ment � la liste
  @param New   �l�m�nt � ajouter
  @return Index de l'�l�ment ajout�
*}
function TExtendedList.Add(New: Extended): Integer;
begin
  Result := _Add(New);
end;

{*
  Insert un �l�ment dans la liste � un index sp�cifi�
  @param Index   Index o� ins�rer l'�l�ment
  @param New     �l�m�nt � ins�rer
  @return Index de l'�l�ment ins�r�
*}
function TExtendedList.Insert(Index: Integer; New: Extended): Integer;
begin
  Result := _Insert(Index, New);
end;

{*
  Supprime un �l�ment de la liste
  @param Index   Index de l'�l�ment � supprimer
  @return L'�l�ment supprim�
*}
function TExtendedList.Delete(Index: Integer): Extended;
begin
  _Delete(Index, Result);
end;

{$ENDREGION}

{$REGION 'Classes TScWaitingQueue et TScWaitingObjectQueue'}
{-----------------------}
{ TScWaitingQueue class }
{-----------------------}

{*
  Annule un �l�ment de la file, en le supprimant
  @param AItem   �l�ment � supprimer
*}
procedure TScWaitingQueue.Cancel(AItem: Pointer);
begin
  List.Remove(AItem);
end;

{-----------------------------}
{ TScWaitingObjectQueue class }
{-----------------------------}

{*
  Annule un objet de la file, en le supprimant
  @param AObject   Objet � supprimer
*}
procedure TScWaitingObjectQueue.Cancel(AObject: TObject);
begin
  List.Remove(AObject);
end;

{$ENDREGION}

{$REGION 'Classe TCustomValueBucketList'}

{------------------------------}
{ TCustomValueBucketList class }
{------------------------------}

{*
  Cr�e une nouvelle instance de TCustomValueBucketList
  @param AKeySize    Taille du type des clefs
  @param AKeyInfo    RTTI du type des clefs
  @param ADataSize   Taille du type des donn�es
  @param ADataInfo   RTTI du type des donn�es
*}
constructor TCustomValueBucketList.Create(AKeySize: Integer;
  AKeyInfo: PTypeInfo; ADataSize: Integer; ADataInfo: PTypeInfo);
begin
  inherited Create;

  BucketCount := DefaultBucketCount;

  if Assigned(AKeyInfo) then
  begin
    FKeySize := TypeSize(AKeyInfo);
    if IsTypeManaged(AKeyInfo) then
      FKeyInfo := AKeyInfo
    else
      FKeyInfo := nil;
  end else
  begin
    FKeySize := AKeySize;
    FKeyInfo := nil;
  end;

  if Assigned(ADataInfo) then
  begin
    FDataSize := TypeSize(ADataInfo);
    if IsTypeManaged(ADataInfo) then
      FDataInfo := ADataInfo
    else
      FDataInfo := nil;
  end else
  begin
    FDataSize := ADataSize;
    FDataInfo := nil;
  end;
end;

{*
  Cr�e une nouvelle instance de TCustomValueBucketList
  @param AKeySize    Taille du type des clefs
  @param ADataSize   Taille du type des donn�es
*}
constructor TCustomValueBucketList.Create(AKeySize, ADataSize: Integer);
begin
  Create(AKeySize, nil, ADataSize, nil);
end;

{*
  Cr�e une nouvelle instance de TCustomValueBucketList
  @param AKeySize    Taille du type des clefs
  @param ADataInfo   RTTI du type des donn�es
*}
constructor TCustomValueBucketList.Create(AKeySize: Integer;
  ADataInfo: PTypeInfo);
begin
  Create(AKeySize, nil, 0, ADataInfo);
end;

{*
  Cr�e une nouvelle instance de TCustomValueBucketList
  @param AKeyInfo    RTTI du type des clefs
  @param ADataSize   Taille du type des donn�es
*}
constructor TCustomValueBucketList.Create(AKeyInfo: PTypeInfo;
  ADataSize: Integer);
begin
  Create(0, AKeyInfo, ADataSize, nil);
end;

{*
  Cr�e une nouvelle instance de TCustomValueBucketList
  @param AKeyInfo    RTTI du type des clefs
  @param ADataInfo   RTTI du type des donn�es
*}
constructor TCustomValueBucketList.Create(AKeyInfo, ADataInfo: PTypeInfo);
begin
  Create(0, AKeyInfo, 0, ADataInfo);
end;

{*
  [@inheritDoc]
*}
destructor TCustomValueBucketList.Destroy;
begin
  Clear;
  inherited;
end;

{*
  Call-back de ForEach utilis� par la m�thode Assign
  @param Key        Clef
  @param Data       Donn�es associ�es � la clef
  @param Continue   Positionner � False pour interrompre l'�num�ration
*}
procedure TCustomValueBucketList.AssignCallBack(const Key, Data;
  var Continue: Boolean);
begin
  AddData(Key, Data);
end;

{*
  True si la liste est vide, False sinon
  @return True si la liste est vide, False sinon
*}
function TCustomValueBucketList.GetIsEmpty: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to BucketCount-1 do
    if Buckets[I].Count > 0 then
      Exit;
  Result := True;
end;

{*
  Modifie le nombre de bo�tes de hashage
  BucketCount ne peut �tre modifi� que lorsque la liste est vide
  @param Value   Nouvelle valeur
  @throws EListError La liste n'est pas vide
*}
procedure TCustomValueBucketList.SetBucketCount(Value: Integer);
begin
  if not IsEmpty then
    raise EListError.Create(sScListIsNotEmpty);

  if Value > 0 then
  begin
    SetLength(FBuckets, Value);
    FBucketCount := Value;
  end;
end;

{*
  Identifie la bo�te dans laquelle ranger un �l�ment donn�
  Les descendants de TCustomValueBucketList surchargent en g�n�ral BucketFor
  pour utiliser au mieux les sp�cificit�s du type de donn�es trait�, afin
  d'avoir un remplissage optimal.
  L'impl�mentation par d�fauut de BucketFor est de prendre les 4 premiers
  octets de la clef, et de prendre le modulo par le nombre de bo�tes de
  hashage (BucketCount).
  @param Key   Clef
  @return Num�ro de la bo�te de hashage pour la clef Key
*}
function TCustomValueBucketList.BucketFor(const Key): Cardinal;
begin
  if KeySize >= 4 then
    Result := Cardinal(Key)
  else
  begin
    Result := 0;
    Move(Key, Result, KeySize);
  end;

  Result := Result mod BucketCount;
end;

{*
  D�termine si deux clefs sont identiques
  L'impl�mentation par d�faut de KeyEquals teste si le contenu brut point�
  par Key1 et Key2 est �quivalent sur KeySize octets.
  Ce comportement doit �tre surcharg� pour les types pour lesquels on ne peut
  pas se fier qu'au contenu brut (cha�nes, tableaux dynamiques, record non
  packed avec des trous, etc.)
  @param Key1   Premi�re clef
  @param Key2   Seconde clef
  @return True si les clefs sont identiques, False sinon
*}
function TCustomValueBucketList.KeyEquals(const Key1, Key2): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to KeySize-1 do
  begin
    if PByte(Integer(@Key1) + I)^ <> PByte(Integer(@Key2) + I)^ then
      Exit;
  end;

  Result := True;
end;

{*
  Localise la bo�te de hashage et l'index d'une paire par sa clef
  @param Key      Clef � rechercher
  @param Bucket   En sortie : bo�te de hashage (m�me si FindItem renvoie False)
  @param Index    En sortie : index dans la bo�te (ind�termin� si renvoie False)
  @return True si la paire a �t� trouv�e, False sinon
*}
function TCustomValueBucketList.FindItem(const Key;
  out Bucket, Index: Integer): Boolean;
begin
  Result := True;
  Bucket := BucketFor(Key);
  Index := 0;

  with Buckets[Bucket] do
  begin
    while Index < Count do
    begin
      if KeyEquals(Items[Index].Item^, Key) then
        Exit;
      Inc(Index);
    end;
  end;

  Result := False;
end;

{*
  Ajoute une paire clef/donn�es dans une bo�te donn�e
  AddItem ne v�rifie pas quue la clef donn�e doit effectivement se mettre dans
  la bo�te sp�cifi�e. Assurez-vous d'appeler AddItem avec des donn�es valides.
  @param Bucket   Index de la bo�te dans laquelle ajouter la paire
  @param Key      Clef
  @param Data     Donn�es
*}
procedure TCustomValueBucketList.AddItem(Bucket: Integer; const Key, Data);
var
  FKey, FData: Pointer;
  Delta, Size: Integer;
begin
  GetMem(FKey, KeySize);
  if Assigned(KeyInfo) then
    Initialize(FKey^, KeyInfo);
  CopyData(Key, FKey^, KeySize, KeyInfo);

  GetMem(FData, DataSize);
  if Assigned(DataInfo) then
    Initialize(FData^, DataInfo);
  CopyData(Data, FData^, DataSize, DataInfo);

  // Copied and adapted from Contnrs.pas: TCustomBucketList.AddItem
  with Buckets[Bucket] do
  begin
    Size := Length(Items);
    if Count = Size then
    begin
      if Size > 64 then
        Delta := Size div 4
      else if Size > 8 then
        Delta := 16
      else
        Delta := 4;
      SetLength(Items, Size + Delta);
    end;

    with Items[Count] do
    begin
      Item := FKey;
      Data := FData;
    end;
    Inc(Count);
  end;
end;

{*
  Supprime une paire clef/donn�es, identifi�e par sa position
  @param Bucket   Index de la bo�te
  @param Index    Index dans la bo�te
*}
procedure TCustomValueBucketList.DeleteItem(Bucket, Index: Integer);
var
  Ptr: Pointer;
begin
  with Buckets[Bucket] do
  begin
    Ptr := Items[Index].Item;
    if Assigned(KeyInfo) then
      Finalize(Ptr^, KeyInfo);
    FreeMem(Ptr);

    Ptr := Items[Index].Data;
    if Assigned(DataInfo) then
      Finalize(Ptr^, DataInfo);
    FreeMem(Ptr);

    // Copied and adapted from Contnrs.pas: TCustomBucketList.DeleteItem
    if not FClearing then
    begin
      if Count = 1 then
        SetLength(Items, 0)
      else
        Move(Items[Index+1], Items[Index],
          (Count-Index) * SizeOf(TBucketItem));
      Dec(Count);
    end;
  end;
end;

{*
  Supprime une paire clef/donn�es, identifi�e par sa position
  @param Bucket   Index de la bo�te
  @param Index    Index dans la bo�te
  @param Data     En sortie : les donn�es de la paire qui est supprim�e
*}
procedure TCustomValueBucketList.ExtractItem(Bucket, Index: Integer;
  out Data);
begin
  CopyData(Buckets[Bucket].Items[Index].Data^, Data, DataSize, DataInfo);
  DeleteItem(Bucket, Index);
end;

{*
  R�cup�re les donn�es li�es � une clef
  @param Key    Clef
  @param Data   En sortie : donn�es associ�es � la clef Key
  @throws EListError �l�ment non trouv�
*}
procedure TCustomValueBucketList.GetData(const Key; out Data);
var
  Bucket, Index: Integer;
begin
  if not FindItem(Key, Bucket, Index) then
    raise EListError.CreateFmt(SItemNotFound, [Integer(@Key)]);
  CopyData(Buckets[Bucket].Items[Index].Data^, Data, DataSize, DataInfo);
end;

{*
  Modifie les donn�es li�es � une clef
  Il doit d�j� exister une paire clef/donn�es avec cette clef.
  @param Key    Clef
  @param Data   Donn�es associ�es � la clef Key
  @throws EListError �l�ment non trouv�
*}
procedure TCustomValueBucketList.SetData(const Key, Data);
var
  Bucket, Index: Integer;
begin
  if not FindItem(Key, Bucket, Index) then
    raise EListError.CreateFmt(SItemNotFound, [Integer(@Key)]);
  CopyData(Data, Buckets[Bucket].Items[Index].Data^, DataSize, DataInfo);
end;

{*
  Ajoute une paire clef/donn�es
  Il ne peut pas encore exister de paire clef/donn�es avec cette clef.
  @param Key    Clef
  @param Data   Donn�es associ�es � la clef Key
  @throws EListError Liste verrouill�e lors d'une op�ration ForEach active
  @throws EListError �l�ment dupliqu�
*}
procedure TCustomValueBucketList.AddData(const Key, Data);
var
  Bucket, Index: Integer;
begin
  if FListLocked then
    raise EListError.Create(SBucketListLocked);
  if FindItem(Key, Bucket, Index) then
    raise EListError.CreateFmt(SDuplicateItem, [Integer(@Key)]);
  AddItem(Bucket, Key, Data);
end;

{*
  Retire une paire clef/donn�es
  La paire ne doit pas n�cessairement �tre pr�sente dans la liste.
  @param Key   Clef
  @throws EListError Liste verrouill�e lors d'une op�ration ForEach active
*}
procedure TCustomValueBucketList.RemoveData(const Key);
var
  Bucket, Index: Integer;
begin
  if FListLocked then
    raise EListError.Create(SBucketListLocked);
  if FindItem(Key, Bucket, Index) then
    DeleteItem(Bucket, Index);
end;

{*
  Retire une paire clef/donn�es
  La paire ne doit pas n�cessairement �tre pr�sente dans la liste.
  @param Key    Clef
  @param Data   En sortie : les donn�es de la paire qui est supprim�e
  @throws EListError Liste verrouill�e lors d'une op�ration ForEach active
*}
procedure TCustomValueBucketList.ExtractData(const Key; out Data);
var
  Bucket, Index: Integer;
begin
  if FListLocked then
    raise EListError.Create(SBucketListLocked);
  if FindItem(Key, Bucket, Index) then
    DeleteItem(Bucket, Index);
end;

{*
  Vide la liste
*}
procedure TCustomValueBucketList.Clear;
var
  Bucket, Index: Integer;
begin
  // Copied and adapted from Contnrs.pas: TCustomBucketList.Clear
  if FListLocked then
    raise EListError.Create(SBucketListLocked);

  FClearing := True;
  try
    for Bucket := 0 to BucketCount-1 do
    begin
      for Index := Buckets[Bucket].Count-1 downto 0 do
        DeleteItem(Bucket, Index);

      SetLength(Buckets[Bucket].Items, 0);
      Buckets[Bucket].Count := 0;
    end;
  finally
    FClearing := False;
  end;
end;

{*
  Appelle une routine de call-back pour chaque paire clef/valeur de la liste
  @param Proc   Routine de call-back
  @param Info   Pointeur libre retransmis dans le param�tre Info de Proc
  @return False si ForEach a �t� interrompu par le param�tre Continue,
          True sinon
*}
function TCustomValueBucketList.ForEach(Proc: TValueBucketProc;
  Info: Pointer = nil): Boolean;
var
  Bucket, Index: Integer;
  OldListLocked: Boolean;
begin
  // Copied and adapted from Contnrs.pas: TCustomBucketList.ForEach
  Result := True;
  OldListLocked := FListLocked;
  FListLocked := True;
  try
    for Bucket := 0 to BucketCount-1 do
    begin
      with Buckets[Bucket] do
      begin
        for Index := Count-1 downto 0 do
        begin
          with Items[Index] do
            Proc(Info, Item^, Data^, Result);
          if not Result then
            Exit;
        end;
      end;
    end;
  finally
    FListLocked := OldListLocked;
  end;
end;

{*
  Appelle une m�thode de call-back pour chaque paire clef/valeur de la liste
  @param Event   M�thode de call-back
  @return False si ForEach a �t� interrompu par le param�tre Continue,
          True sinon
*}
function TCustomValueBucketList.ForEach(Event: TValueBucketEvent): Boolean;
begin
  with TMethod(Event) do
    Result := ForEach(TValueBucketProc(Code), Data);
end;

{$IFNDEF FPC}  // FPC doesn't support "reference to" see http://www.lazarus.freepascal.org/index.php?topic=12250.0
{$IF CompilerVersion >= 20}
{*
  Appelle un call-back pour chaque paire clef/valeur de la liste
  @param Callback   Call-back
  @return False si ForEach a �t� interrompu par le param�tre Continue,
          True sinon
*}
function TCustomValueBucketList.ForEach(
  const Callback: TValueBucketCallback): Boolean;
var
  Bucket, Index: Integer;
  OldListLocked: Boolean;
begin
  // Copied and adapted from Contnrs.pas: TCustomBucketList.ForEach
  Result := True;
  OldListLocked := FListLocked;
  FListLocked := True;
  try
    for Bucket := 0 to BucketCount-1 do
    begin
      with Buckets[Bucket] do
      begin
        for Index := Count-1 downto 0 do
        begin
          with Items[Index] do
            Callback(Item^, Data^, Result);
          if not Result then
            Exit;
        end;
      end;
    end;
  finally
    FListLocked := OldListLocked;
  end;
end;
{$IFEND}
{$ENDIF}

{*
  Copie le contenu d'une liste
  La liste source doit avoir le m�me type d'�l�ments, sinon une exception
  EConvertError sera d�clench�e.
  @param Source   Liste source
  @throws EConvertError La liste source n'a pas le m�me type d'�l�ment
*}
procedure TCustomValueBucketList.Assign(Source: TCustomValueBucketList);
begin
  if (Source.KeySize <> KeySize) or (Source.KeyInfo <> KeyInfo) or
    (Source.DataSize <> DataSize) or (Source.DataInfo <> DataInfo) then
    raise EConvertError.CreateResFmt(@SAssignError,
      [Source.ClassName, ClassName]);

  Clear;
  Source.ForEach(AssignCallBack);
end;

{*
  D�termine si un clef est r�f�renc�e dans la liste
  @param Key   Clef � rechercher
  @return True si la clef est r�f�renc�e, False sinon
*}
function TCustomValueBucketList.Exists(const Key): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindItem(Key, Bucket, Index);
end;

{*
  Recherche une clef dans la liste
  @param Key    Clef � rechercher
  @param Data   En sortie : donn�es associ�es � la clef, si elle existe
  @return True si la clef a �t� trouv�e, False sinon
*}
function TCustomValueBucketList.Find(const Key; out Data): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindItem(Key, Bucket, Index);
  if Result then
    CopyData(Buckets[Bucket].Items[Index].Data^, Data, DataSize, DataInfo);
end;

{$ENDREGION}

{$REGION 'Classe TValueBucketList'}

{------------------------}
{ TValueBucketList class }
{------------------------}

{*
  R�cup�re les donn�es li�es � une clef
  @param Key    Clef
  @param Data   En sortie : donn�es associ�es � la clef Key
  @throws EListError �l�ment non trouv�
*}
procedure TValueBucketList.Get(const Key; out Data);
begin
  GetData(Key, Data);
end;

{*
  Modifie les donn�es li�es � une clef
  Il doit d�j� exister une paire clef/donn�es avec cette clef.
  @param Key    Clef
  @param Data   Donn�es associ�es � la clef Key
  @throws EListError �l�ment non trouv�
*}
procedure TValueBucketList.Put(const Key, Data);
begin
  SetData(Key, Data);
end;

{*
  Ajoute une paire clef/donn�es
  Il ne peut pas encore exister de paire clef/donn�es avec cette clef.
  @param Key    Clef
  @param Data   Donn�es associ�es � la clef Key
  @throws EListError Liste verrouill�e lors d'une op�ration ForEach active
  @throws EListError �l�ment dupliqu�
*}
procedure TValueBucketList.Add(const Key, Data);
begin
  AddData(Key, Data);
end;

{*
  Retire une paire clef/donn�es
  La paire ne doit pas n�cessairement �tre pr�sente dans la liste.
  @param Key   Clef
  @throws EListError Liste verrouill�e lors d'une op�ration ForEach active
*}
procedure TValueBucketList.Remove(const Key);
begin
  RemoveData(Key);
end;

{*
  Retire une paire clef/donn�es
  La paire ne doit pas n�cessairement �tre pr�sente dans la liste.
  @param Key    Clef
  @param Data   En sortie : les donn�es de la paire qui est supprim�e
  @throws EListError Liste verrouill�e lors d'une op�ration ForEach active
*}
procedure TValueBucketList.Extract(const Key; out Data);
begin
  ExtractData(Key, Data);
end;

{*
  Vide la liste
*}
procedure TValueBucketList.Clear;
begin
  inherited Clear;
end;

{*
  [@inheritDoc]
*}
procedure TValueBucketList.Assign(Source: TCustomValueBucketList);
begin
  inherited;
end;

{$ENDREGION}

{--------------------------------}
{ Initialization et Finalization }
{--------------------------------}

var
  I: Integer; /// Variable de contr�le de boucle interne

initialization
  AppParams := TScStrings.Create;
  // On remplit AppParams avec les param�tres envoy�s � l'application
  for I := 1 to ParamCount do
    AppParams.Append(ParamStr(I));

finalization
  AppParams.Free;
end.

