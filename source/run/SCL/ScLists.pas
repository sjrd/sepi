{*
  Définit des classes de listes diverses
  @author sjrd
  @version 1.0
*}
unit ScLists;

interface

uses
  SysUtils, Classes, Contnrs, TypInfo;

type
  {*
    Exception déclenchée lors d'une erreur de liste d'entiers
    @author sjrd
    @version 1.0
  *}
  EIntListError = class(EListError);

  {*
    Classe purement abstraite rendant publique la méthode CompareStrings
    Rend publique la méthode CompareStrings afin de pouvoir l'utiliser pour
    comparer deux chaînes en respectant les règles de comparaison d'une liste.
    Cette classe ne doit pas être utilisée dans une autre optique.
    @author sjrd
    @version 1.0
  *}
  TCompareStrings = class(TStrings)
  public
    function CompareStrings(const S1, S2: string): Integer; override;
  end;

  {*
    Classe utilitaire avec des méthodes avancées de gestion de listes de chaînes
    Propose une série de méthodes pouvant être appliquée à des instances de
    TStrings (toutes les méthodes de recherche respectent les règles de
    comparaison de la liste concernée)
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
    Extension de TStringList y ajoutant quelques fonctionnalités
    Amélioration de TStringList pour lui ajouter les méthodes de StringsOps
    ainsi qu'un index interne permettant de parcourir aisément toutes les
    chaînes dans l'ordre
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
    Classe abstraite de base pour la création de listes
    Classe de base pour les listes dont les éléments sont de taille homogène
    Ne créez pas d'instance de TScList, mais créez plutôt des instances de
    ses classes descendantes.
    Ne pas utiliser pour des listes de chaînes, de pointeurs ou d'objets ;
    dans ces cas, utiliser respectivement TStringList (unité Classes), TList
    (unité Classes) et TObjectList (unité Contnrs).
    @author sjrd
    @version 1.0
  *}
  TScList = class(TPersistent)
  private
    FStream: TMemoryStream; /// Flux mémoire interne contenant la liste
    FItemSize: Integer;     /// Taille en octets d'un élément de la liste

    function GetCount: Integer;
    procedure SetCount(New: Integer);
    function GetHasMoreValue: Boolean;
    function GetIndex: Integer;
    procedure SetIndex(New: Integer);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    procedure AssignTo(Dest: TPersistent); override;

    function IsAssignClass(ScListClass: TScListClass): Boolean; virtual;

    { Les méthodes suivantes doivent être appellées par les méthodes de même
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
    Gère une liste d'entiers signés
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
    Gère une liste d'entiers non signés
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
    Gère une liste de Extended
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
    TScWaitingQueue ajoute à TQueue une méthode Cancel qui permet de supprimer
    un élément de la liste, quelle que soit sa position.
    @author sjrd
    @version 1.0
  *}
  TScWaitingQueue = class(TQueue)
  public
    procedure Cancel(AItem: Pointer);
  end;

  {*
    File d'attente d'objets
    TScWaitingObjectQueue ajoute à TObjectQueue une méthode Cancel qui permet
    de supprimer un objet de la liste, quel que soit sa position.
    @author sjrd
    @version 1.0
  *}
  TScWaitingObjectQueue = class(TObjectQueue)
  public
    procedure Cancel(AObject: TObject);
  end;

  {*
    Procédure de call-back pour la méthode ForEach de TCustomValueBucketList
    @param Info       Retransmission du paramètre AInfo donné à ForEach
    @param Key        Clef
    @param Data       Données associées à la clef
    @param Continue   Positionner à False pour interrompre l'énumération
  *}
  TValueBucketProc = procedure(Info: Pointer; const Key, Data;
    var
    Continue: Boolean);

  {*
    Méthode de call-back pour la méthode ForEach de TCustomValueBucketList
    @param Key        Clef
    @param Data       Données associées à la clef
    @param Continue   Positionner à False pour interrompre l'énumération
  *}
  TValueBucketEvent = procedure(const Key, Data;
    var
    Continue: Boolean) of object;

  {*
    Classe de base pour les tables associatives hashées par valeur
    Au contraire de TCustomBucketList, et donc de TBucketList,
    TCustomValueBucketList recherche et fait correspondre les clefs et les
    données par valeur, et non par pointeur.
    Pour cela, elle gère elle-même l'allocation des données en interne, et a
    donc besoin des RTTI des types de clef et de données, ou à défaut de leurs
    tailles (s'ils ne requièrent pas d'initialisation).
    @author sjrd
    @version 1.0
  *}
  TCustomValueBucketList = class(TObject)
  private
    FBuckets: TBucketArray; /// Boîtes de hashage
    FBucketCount: Integer;  /// Nombre de boîtes de hashage (défaut = 16)
    FListLocked: Boolean;   /// Indique si la liste est verrouillée
    FClearing: Boolean;     /// Indique si la liste est en train d'être vidée

    FKeySize: Integer;    /// Taille du type des clefs
    FKeyInfo: PTypeInfo;  /// RTTI du type des clefs (si need-init)
    FDataSize: Integer;   /// Taille du type des données
    FDataInfo: PTypeInfo; /// RTTI du type des données (si need-init)

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

    function Exists(const Key): Boolean;
    function Find(const Key; out Data): Boolean;

    property IsEmpty: Boolean read GetIsEmpty;
  end;

  {*
    Table associative hashées par valeur générique
    Au contraire de TBucketList, TValueBucketList recherche et fait
    correspondre les clefs et les données par valeur, et non par pointeur.
    Pour cela, elle gère elle-même l'allocation des données en interne, et a
    donc besoin des RTTI des types de clef et de données, ou à défaut de leurs
    tailles (s'ils ne requièrent pas d'initialisation).
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
  AppParams: TScStrings; /// Liste des paramètres envoyés à l'application

implementation

uses
  RTLConsts, ScUtils, ScStrUtils, ScCompilerMagic, ScTypInfo, ScConsts;

const
  /// Nombre par défaut de boîtes de hashage
  DefaultBucketCount = 16;

{------------------------}
{ Classe TCompareStrings }
{------------------------}

{*
  Compare deux chaînes de caractères
  @param S1   Première chaîne de caractères
  @param S2   Seconde chaîne de caractères
  @return 0 si les chaînes sont équivalente, un nombre positif si la première
          est supérieure à la seconde, un nombre négatif dans le cas inverse
*}
function TCompareStrings.CompareStrings(const S1, S2: string): Integer;
begin
  Result := (inherited CompareStrings(S1, S2));
end;

{$REGION 'Classe StringsOps'}

{-------------------}
{ Classe StringsOps }
{-------------------}

{*
  Recherche une chaîne dans une liste de chaînes
  Recherche une chaîne dans une liste de chaînes, en débutant et s'arrêtant à
  des index spécifiés.
  @param Strings         Liste de chaînes concernée
  @param Str             Chaîne à rechercher
  @param BeginSearchAt   Index à partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'à la fin si -1)
  @return Index de la première chaîne correspondant à Str, ou -1 si non trouvée
*}
class function StringsOps.IndexOf(Strings: TStrings; const Str: string;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
begin
  with TCompareStrings(Strings) do
  begin
    // On s'assure que BeginSearchAt et EndSearchAt sont des entrées correctes
    if BeginSearchAt < 0 then
      BeginSearchAt := 0;
    if (EndSearchAt < 0) or (EndSearchAt >= Count) then
      EndSearchAt := Count-1;

    Result := BeginSearchAt;
    while Result <= EndSearchAt do
    begin
      if CompareStrings(Str, Strings[BeginSearchAt]) = 0 then
        Exit
      else
        Inc(BeginSearchAt);
    end;
    Result := -1;
  end;
end;

{*
  Recherche un texte à l'intérieur des chaînes d'une liste
  Recherche un texte à l'intérieur des chaînes d'une liste, en débutant et
  s'arrêtant à des index spécifiés.
  @param Strings         Liste de chaînes concernée
  @param Str             Texte à rechercher
  @param BeginSearchAt   Index à partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'à la fin si -1)
  @return Index de la première chaîne contenant Str, ou -1 si non trouvée
*}
class function StringsOps.FindText(Strings: TStrings; const Str: string;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
var
  I, Len: Integer;
begin
  with TCompareStrings(Strings) do
  begin
    // On s'assure que BeginSearchAt et EndSearchAt sont des entrées correctes
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
  Recherche un mot en première position d'une chaîne d'une liste
  Recherche un mot placé en première position d'une chaîne d'une liste (des
  espaces peuvent se trouver devant), en débutant et s'arrêtant à des index
  spécifiés.
  @param Strings         Liste de chaînes concernée
  @param Word            Mot à rechercher
  @param BeginSearchAt   Index à partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'à la fin si -1)
  @return Index de la première chaîne contenant Word, ou -1 si non trouvée
*}
class function StringsOps.FindFirstWord(Strings: TStrings;
  const Word: string;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
begin
  with TCompareStrings(Strings) do
  begin
    // On s'assure que BeginSearchAt et EndSearchAt sont des entrées correctes
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
  Recherche une sous-chaîne à une position spécifiée les chaînes d'une liste
  Recherche une sous-chaîne débutant à une position spécifiée dans les chaînes
  d'une liste, en débutant et s'arrêtant à des index spécifiés.
  @param Strings         Liste de chaînes concernée
  @param SubStr          Sous-chaîne à rechercher
  @param Position        Position dans les chaînes où chercher la sous-chaîne
  @param BeginSearchAt   Index à partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'à la fin si -1)
  @return Index de la première chaîne contenant Word, ou -1 si non trouvée
*}
class function StringsOps.FindAtPos(Strings: TStrings; const SubStr: string;
  Position: Integer = 1; BeginSearchAt: Integer = 0;
  EndSearchAt: Integer = -1): Integer;
var
  Len: Integer;
begin
  with TCompareStrings(Strings) do
  begin
    // On s'assure que BeginSearchAt et EndSearchAt sont des entrées correctes
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
  Remplit une liste de chaînes avec des chaînes d'une autre liste
  Remplit une liste de chaînes avec les chaînes d'une autre liste depuis un
  index et sur un nombre spécifiés.
  @param Strings   Liste de chaînes concernée
  @param Source    Liste de chaînes à partir de laquelle recopier les chaînes
  @param Index     Index où commencer la copie
  @param Count     Nombre de chaînes à copier
*}
class procedure StringsOps.CopyFrom(Strings: TStrings; Source: TStrings;
  Index: Integer = 0; Count: Integer = -1);
begin
  Strings.Clear;
  AddFrom(Strings, Source, Index, Count);
end;

{*
  Ajoute à une liste de chaînes des chaînes d'une autre liste
  Ajoute à une liste de chaînes les chaînes d'une autre liste depuis un index et
  sur un nombre spécifiés.
  @param Strings   Liste de chaînes concernée
  @param Source    Liste de chaînes à partir de laquelle recopier les chaînes
  @param Index     Index où commencer la copie
  @param Count     Nombre de chaînes à copier
*}
class procedure StringsOps.AddFrom(Strings: TStrings; Source: TStrings;
  Index: Integer = 0; Count: Integer = -1);
var
  I, EndAt: Integer;
begin
  // On s'assure que Index et Count sont des entrées correctes
  if Index < 0 then
    Exit;
  if (Count < 0) or (Index+Count > Source.Count) then
    EndAt := Source.Count-1
  else
    EndAt := Index+Count-1;

  // On recopie les chaînes
  for I := Index to EndAt do
    Strings.Append(Source[I]);
end;

{*
  Découpe une chaîne en sous-chaînes et remplit une liste de ces sous-chaînes
  Découpe une chaîne en sous-chaînes délimitées par des caractères spécifiés, et
  remplit une liste de chaînes avec ces sous-chaînes.
  @param Strings   Liste de chaînes dans laquelle copier les sous-chaînes
  @param Str       Chaîne à découper
  @param Delim     Caractères qui délimitent deux sous-chaînes
  @param NotIn     Paires de caractères échappant les délimiteurs
  @throws EListError NotIn contient un nombre impair de caractères
  @throws EListError Delim et NotIn contiennent un même caractère
*}
class procedure StringsOps.FromString(Strings: TStrings;
  const Str, Delim: string; const NotIn: string = '');
begin
  Strings.Clear;
  AddFromString(Strings, Str, Delim, NotIn);
end;

{*
  Découpe une chaîne en sous-chaînes et ajoute ces sous-chaînes à une liste
  Découpe une chaîne en sous-chaînes délimitées par des caractères spécifiés, et
  ajoute ces sous-chaînes à une liste de chaînes.
  @param Strings   Liste de chaînes à laquelle ajouter les sous-chaînes
  @param Str       Chaîne à découper
  @param Delim     Caractères qui délimitent deux sous-chaînes
  @param NotIn     Paires de caractères échappant les délimiteurs
  @throws EListError NotIn contient un nombre impair de caractères
  @throws EListError Delim et NotIn contiennent un même caractère
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
    // On vérifie que NotIn contient un nombre pair de caractères
    if Odd(Length(NotIn)) then
      raise EListError.Create(sScNotInMustPairsOfChars);

    // On vérifie qu'il n'y a pas d'interférence entre Delim et NotIn
    for I := 1 to Length(NotIn) do
      if Pos(NotIn[I], Delim) > 0 then
        raise EListError.Create(sScDelimMustDifferentThanNotIn);

    // Séparation de NotIn en NotIn1 et NotIn2
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
      // On boucle jusqu'à trouver un caractère qui n'est pas dans Delim
      // On ignore de ce fait plusieurs caractères de Delim à la suite
      while (I <= Len) and (Pos(Str[I], Delim) <> 0) do
        Inc(I);
      if (I > Len) then
        Break;

      // On recherche le caractère de Delim suivant
      J := I;
      while (J <= Len) and (Pos(Str[J], Delim) = 0) do
      begin
        // Si on trouve un caractère de NotIn1, on boucle jusqu'à trouver le
        // caractère correspondant de NotIn2
        if Pos(Str[J], NotIn1) > 0 then
        begin
          C := NotIn2[Pos(Str[J], NotIn1)];
          Inc(J);
          while (J <= Len) and (Str[J] <> C) do
            Inc(J);
        end;
        Inc(J);
      end;

      // On ajoute la sous-chaîne repérée par les caractères de Delim
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
  Crée une nouvelle instance de TScStrings
*}
constructor TScStrings.Create;
begin
  inherited Create;
  FIndex := 0;
end;

{*
  Crée une nouvelle instance de TScStrings chargée à partir d'un fichier
  @param FileName   Nom du fichier à partir duquel charger la liste de chaînes
*}
constructor TScStrings.CreateFromFile(const FileName: TFileName);
begin
  Create;
  LoadFromFile(FileName);
end;

{*
  Crée une nouvelle instance de TScStrings à partir d'une chaîne découpée
  @param Str       Chaîne à découper
  @param Delim     Caractères qui délimitent deux sous-chaînes
  @param NotIn     Paires de caractères échappant les délimiteurs
  @throws EListError NotIn contient un nombre impair de caractères
  @throws EListError Delim et NotIn contiennent un même caractère
*}
constructor TScStrings.CreateFromString(const Str, Delim: string;
  const NotIn: string = '');
begin
  Create;
  FromString(Str, Delim, NotIn);
end;

{*
  Crée une nouvelle instance de TScStrings assignée à partir d'une source
  @param Source   Objet source à copier dans la liste de chaînes
*}
constructor TScStrings.CreateAssign(Source: TPersistent);
begin
  Create;
  Assign(Source);
end;

{*
  Indique s'il y a encore des chaînes à lire
  @return True s'il y a encore des chaînes à lire, False sinon
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
  Recherche une chaîne dans la liste de chaînes
  Recherche une chaîne dans la liste de chaînes, en débutant et s'arrêtant à
  des index spécifiés.
  @param Str             Chaîne à rechercher
  @param BeginSearchAt   Index à partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'à la fin si -1)
  @return Index de la première chaîne correspondant à Str, ou -1 si non trouvée
*}
function TScStrings.IndexOfEx(const Str: string; BeginSearchAt: Integer = 0;
  EndSearchAt: Integer = -1): Integer;
begin
  Result := StringsOps.IndexOf(Self, Str, BeginSearchAt, EndSearchAt);
end;

{*
  Recherche un texte à l'intérieur des chaînes de la liste
  Recherche un texte à l'intérieur des chaînes de la liste, en débutant et
  s'arrêtant à des index spécifiés.
  @param Str             Texte à rechercher
  @param BeginSearchAt   Index à partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'à la fin si -1)
  @return Index de la première chaîne contenant Str, ou -1 si non trouvée
*}
function TScStrings.FindText(const Str: string; BeginSearchAt: Integer = 0;
  EndSearchAt: Integer = -1): Integer;
begin
  Result := StringsOps.FindText(Self, Text, BeginSearchAt, EndSearchAt);
end;

{*
  Recherche un mot en première position d'une chaîne de la liste
  Recherche un mot placé en première position d'une chaîne de la liste (des
  espaces peuvent se trouver devant), en débutant et s'arrêtant à des index
  spécifiés.
  @param Word            Mot à rechercher
  @param BeginSearchAt   Index à partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'à la fin si -1)
  @return Index de la première chaîne contenant Word, ou -1 si non trouvée
*}
function TScStrings.FindFirstWord(const Word: string;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
begin
  Result := StringsOps.FindFirstWord(Self, Word, BeginSearchAt, EndSearchAt);
end;

{*
  Recherche une sous-chaîne à une position spécifiée les chaînes de la liste
  Recherche une sous-chaîne débutant à une position spécifiée dans les chaînes
  de la liste, en débutant et s'arrêtant à des index spécifiés.
  @param SubStr          Sous-chaîne à rechercher
  @param Position        Position dans les chaînes où chercher la sous-chaîne
  @param BeginSearchAt   Index à partir duquel chercher
  @param EndSearchAt     Index jusqu'auquel chercher (jusqu'à la fin si -1)
  @return Index de la première chaîne contenant Word, ou -1 si non trouvée
*}
function TScStrings.FindAtPos(const SubStr: string; Position: Integer = 1;
  BeginSearchAt: Integer = 0; EndSearchAt: Integer = -1): Integer;
begin
  Result := StringsOps.FindAtPos(Self, SubStr, Position, BeginSearchAt,
    EndSearchAt);
end;

{*
  Remplit la liste de chaînes avec des chaînes d'une autre liste
  Remplit la liste de chaînes avec les chaînes d'une autre liste depuis un index
  et sur un nombre spécifiés.
  @param Source    Liste de chaînes à partir de laquelle recopier les chaînes
  @param Index     Index où commencer la copie
  @param Count     Nombre de chaînes à copier
*}
procedure TScStrings.CopyFrom(Source: TStrings; Index: Integer = 0;
  Count: Integer = -1);
begin
  StringsOps.CopyFrom(Self, Source, Index, Count);
end;

{*
  Ajoute à la liste de chaînes des chaînes d'une autre liste
  Ajoute à la liste de chaînes les chaînes d'une autre liste depuis un index et
  sur un nombre spécifiés.
  @param Source    Liste de chaînes à partir de laquelle recopier les chaînes
  @param Index     Index où commencer la copie
  @param Count     Nombre de chaînes à copier
*}
procedure TScStrings.AddFrom(Source: TStrings; Index: Integer = 0;
  Count: Integer = -1);
begin
  StringsOps.AddFrom(Self, Source, Index, Count);
end;

{*
  Découpe une chaîne en sous-chaînes et remplit la liste de ces sous-chaînes
  Découpe une chaîne en sous-chaînes délimitées par des caractères spécifiés, et
  remplit la liste de chaînes avec ces sous-chaînes.
  @param Str       Chaîne à découper
  @param Delim     Caractères qui délimitent deux sous-chaînes
  @param NotIn     Paires de caractères échappant les délimiteurs
  @throws EListError NotIn contient un nombre impair de caractères
  @throws EListError Delim et NotIn contiennent un même caractère
*}
procedure TScStrings.FromString(const Str, Delim: string;
  const NotIn: string = '');
begin
  StringsOps.FromString(Self, Str, Delim, NotIn);
end;

{*
  Découpe une chaîne en sous-chaînes et ajoute ces sous-chaînes à la liste
  Découpe une chaîne en sous-chaînes délimitées par des caractères spécifiés, et
  ajoute ces sous-chaînes à la liste de chaînes.
  @param Str       Chaîne à découper
  @param Delim     Caractères qui délimitent deux sous-chaînes
  @param NotIn     Paires de caractères échappant les délimiteurs
  @throws EListError NotIn contient un nombre impair de caractères
  @throws EListError Delim et NotIn contiennent un même caractère
*}
procedure TScStrings.AddFromString(const Str, Delim: string;
  const NotIn: string = '');
begin
  StringsOps.AddFromString(Self, Str, Delim, NotIn);
end;

{*
  Remet à 0 l'index interne
*}
procedure TScStrings.Reset;
begin
  FIndex := 0;
end;

{*
  Lit la chaîne suivante dans la liste de chaîne
  @return La chaîne suivante
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
  Crée une nouvelle instance de TScList
  @param ItemSize   Taille en octets d'un élément de la liste
*}
constructor TScList.Create(ItemSize: Integer);
begin
  FStream := TMemoryStream.Create;
  FItemSize := ItemSize;
end;

{*
  Détruit l'instance
*}
destructor TScList.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

{*
  Nombre d'éléments de la liste
  @return Nombre d'éléments de la liste
*}
function TScList.GetCount: Integer;
begin
  Result := FStream.Size div FItemSize;
end;

{*
  Modifie le nombre d'éléments de la liste
  @param New   Nouveau nombre d'éléments de la liste
*}
procedure TScList.SetCount(New: Integer);
begin
  if New <= 0 then
    FStream.SetSize(0)
  else
    FStream.SetSize(New*FItemSize);
end;

{*
  Indique s'il y a encore des éléments à lire
  @return True s'il y a encore des éléments à lire, False sinon
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
  // On vérifie que New est bien un index correct
  if (New < 0) or (New > Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [New]);

  FStream.Position := New*FItemSize;
end;

{*
  Propose une interface pour les méthodes traitant des données non publiées
  @param Filer   Objet lecteur ou écrivain
*}
procedure TScList.DefineProperties(Filer: TFiler);
begin
  inherited;
  { La ligne suivante crée une propriété publiée fictive qui permet
    d'enregistrer le contenu via WriteComponent et de le lire via
    ReadComponent. Don't localize. }
  Filer.DefineBinaryProperty('Items', LoadFromStream, SaveToStream, True);
end;

{*
  Copie les propriétés d'un objet dans l'objet destination
  @param Dest   Objet destination dans lequel copier la liste
*}
procedure TScList.AssignTo(Dest: TPersistent);
begin
  { On autorise l'assignation ssi Dest.ClassType fait partie de la liste des
    classes d'assignation et que les tailles d'élements sont les mêmes }
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
  Les classes descendantes de TScList peuvent surcharger cette méthode pour
  indiquer quelles sont les classes d'assignation de la liste.
  @param ScListClass   Classe à tester
  @return True si ScListClass est une classe d'assignation, False sinon
*}
function TScList.IsAssignClass(ScListClass: TScListClass): Boolean;
begin
  // Si on remonte jusqu'ici, c'est que ce n'est pas une classe d'assignation
  Result := False;
end;

{*
  Lit un élément de la liste à la position courante
  @param Buffer   Buffer non typé dans lequel enregistrer l'élément lu
*}
procedure TScList._Read(var Buffer);
begin
  FStream.ReadBuffer(Buffer, FItemSize);
end;

{*
  Écrit un élément dans la liste à la position courante
  Si l'index interne est à la fin de la liste, celle-ci est agrandie pour
  acceuillir le nouvel élément.
  @param Buffer   Buffer non typé contenant l'élément à écrire
*}
procedure TScList._Write(var Buffer);
begin
  FStream.WriteBuffer(Buffer, FItemSize);
end;

{*
  Lit un élément identifié par son index
  @param AIndex   Index de l'élément à lire
  @param Buffer   Buffer non typé dans lequel enregistré l'élément lu
  @throws EListError Index de liste hors bornes
*}
procedure TScList._GetItems(AIndex: Integer; var Buffer);
begin
  // On vérifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);

  Index := AIndex;
  _Read(Buffer);
end;

{*
  Modifie un élément identifié par son index
  @param AIndex   Index de l'élément à modifier
  @param Buffer   Buffer non typé contenant l'élément à écrire
  @throws EListError Index de liste hors bornes
*}
procedure TScList._SetItems(AIndex: Integer; var Buffer);
begin
  // On vérifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);

  Index := AIndex;
  _Write(Buffer);
end;

{*
  Ajoute un élément
  @param Buffer   Buffer non typé contenant l'élément à ajouter
  @return Index de l'élément ajouté
*}
function TScList._Add(var Buffer): Integer;
begin
  Result := Count;
  Index := Result;
  _Write(Buffer);
end;

{*
  Insert un nouvel élément à une position donnée
  @param AIndex   Index où insérer l'élément
  @param Buffer   Buffer non typé contenant l'élément à insérer
  @return Index de l'élément inséré
  @throws EListError Index de liste hors bornes
*}
function TScList._Insert(AIndex: Integer; var Buffer): Integer;
var
  Temp: TMemoryStream;
begin
  // On vérifie que AIndex est un index correct
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

      // On copie tous les éléments à partir de AIndex dans Temp
      Index := AIndex;
      Temp.CopyFrom(FStream, FStream.Size-FStream.Position);

      // On agrandi la liste
      Count := Count+1;

      // On écrit le nouvel élément et à sa suite le contenu de Temp
      Index := AIndex;
      _Write(Buffer);
      FStream.CopyFrom(Temp, 0);
    finally
      Temp.Free;
    end;
  end;
end;

{*
  Supprime un élément de la liste
  @param AIndex   Index de l'élément à supprimer
  @param Buffer   Buffer non typé dans lequel enregistré l'élément supprimé
  @throws EListError Index de liste hors bornes
*}
procedure TScList._Delete(AIndex: Integer; var Buffer);
var
  Temp: TMemoryStream;
begin
  // On vérifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);

  Temp := TMemoryStream.Create;
  try
    // On lit la valeur de retour à l'emplacement qui va être supprimé
    Index := AIndex;
    _Read(Buffer);

    // On copie tous les éléments après celui-là dans Temp
    if FStream.Position <> FStream.Size then // Pour éviter le CopyFrom(0)
      Temp.CopyFrom(FStream, FStream.Size-FStream.Position);

    // On réduit la liste
    Count := Count-1;

    // On recopie le contenu de Temp à partir de l'emplacement qui a été supprimé
    Index := AIndex;
    FStream.CopyFrom(Temp, 0);
  finally
    Temp.Free;
  end;
end;

{*
  Copie le contenu d'un autre objet similaire
  Appelez Assign pour copier les propriétés ou d'autres attributs d'un objet sur
  un autre.
  @param Source   Objet source à copier
*}
procedure TScList.Assign(Source: TPersistent);
begin
  { On autorise l'assignation ssi Source.ClassType fait partie de la liste des
    classes d'assignation et que les tailles d'élements sont les mêmes. }
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
  Remet à 0 l'index interne
*}
procedure TScList.Reset;
begin
  Index := 0;
end;

{*
  Charge la liste depuis un flux
  La liste doit avoir été enregistrée au moyen de SaveToStream.
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
  if Size > 0 then // Pour éviter le CopyFrom(0)
    FStream.CopyFrom(Stream, Size);
end;

{*
  Enregistre la liste dans un flux
  La liste pourra être relue avec LoadFromStream.
  @param Stream   Flux dans lequel enregistrer la liste
*}
procedure TScList.SaveToStream(Stream: TStream);
var
  Size: Int64;
begin
  // On écrit la taille sur 8 octets (Int64)
  Size := FStream.Size;
  Stream.Write(Size, 8);
  if Size > 0 then
    Stream.CopyFrom(FStream, 0);
end;

{*
  Charge la liste depuis un fichier
  La liste doit avoir été enregistrée au moyen de SaveToFile.
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
  La liste pourra être relue avec LoadFromFile.
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
  Crée une nouvelle instance de TIntegerList
  @param IntSize   Taille en octets des entiers (entre 1 et 8 inclus)
  @throws EIntListError Taille d'entier incorrecte
*}
constructor TIntegerList.Create(IntSize: Integer = 4);
begin
  // On vérifie que IntSize est entre 1 et 8 inclus
  if (IntSize < 1) or (IntSize > 8) then
    raise EIntListError.CreateFmt(sScWrongIntSize, [IntSize]);

  inherited Create(IntSize);
end;

{*
  Crée une nouvelle instance de TIntegerList, copie d'une autre source
  @param Source    Objet source à copier
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
  Tableau indexé par leurs index des éléments de la liste
  @param Index   Index de l'élément à lire
  @return Valeur de l'élément à l'index Index
  @throws EListError Index de liste hors bornes
*}
function TIntegerList.GetItems(Index: Integer): Int64;
begin
  _GetItems(Index, Result);
  MakeItGood(Result);
end;

{*
  Modifie le tableau indexé par leurs index des éléments de la liste
  @param Index   Index de l'élément à modifier
  @param New     Nouvelle valeur de l'élément à l'index Index
  @throws EListError Index de liste hors bornes
*}
procedure TIntegerList.SetItems(Index: Integer; New: Int64);
begin
  _SetItems(Index, New);
end;

{*
  S'assure que Value contient la valeur exacte selon la taille stockée
  Cette procédure remplit les octets non stockés de manière à transformer un
  entier stocké sur un nombre quelconque d'octets en Int64 (sur 8 octets)
  @param Value   Valeur à traiter
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
  // On s'évite un travail inutile si Value est déjà stocké sur 8 octets
  if ItemSize = 8 then
    Exit;

  // On initialise RecVal.Int (et donc aussi RecVal.Ints)
  RecVal.Int := Value;

  { Si le Shortint à la position ItemSize (c-à-d l'octet de poids le plus
    fort stocké) est négatif (c-à-d si le nombre complet est négatif),
    on remplit les suivant avec -1 (11111111b) et sinon avec 0 (00000000b) }
  if RecVal.Ints[ItemSize] < 0 then
    FillWith := -1
  else
    FillWith := 0;

  // On remplit les les octets non stockés avec la valeur de Remplis
  for I := ItemSize+1 to 8 do
    RecVal.Ints[I] := FillWith;

  // On réactualise Value
  Value := RecVal.Int;
end;

{*
  Copie les propriétés d'un objet dans l'objet destination
  @param Dest   Objet destination dans lequel copier la liste
*}
procedure TIntegerList.AssignTo(Dest: TPersistent);
var
  DestStrings: TStrings;
begin
  // Si Dest est un TStrings on le remplit avec les formes chaînes des éléments
  if Dest is TStrings then
  begin
    DestStrings := TStrings(Dest);
    DestStrings.Clear;
    // Le principe "Reset-HasMoreValue-Read" étant plus rapide que
    // "for 0 to Count-1" avec TScList, on l'utilise pour remplire le TStrings
    Reset;
    while HasMoreValue do
      DestStrings.Append(IntToStr(Read));
  end else
    inherited;
end;

{*
  Indique si une classe est une classe d'assignation de la liste
  Les classes descendantes de TScList peuvent surcharger cette méthode pour
  indiquer quelles sont les classes d'assignation de la liste.
  @param ScListClass   Classe à tester
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
  Appelez Assign pour copier les propriétés ou d'autres attributs d'un objet sur
  un autre.
  @param Source   Objet source à copier
*}
procedure TIntegerList.Assign(Source: TPersistent);
var
  SourceStrings: TStrings;
  I: Integer;
begin
  // Si Source est un TStrings, on convertit les chaînes en entiers
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
  Lit un élément de la liste à la position courante
  @return L'élément lu
*}
function TIntegerList.Read: Int64;
begin
  _Read(Result);
  MakeItGood(Result);
end;

{*
  Écrit un élément dans la liste à la position courante
  @param New   Élémént à écrire
*}
procedure TIntegerList.Write(New: Int64);
begin
  _Write(New);
end;

{*
  Ajoute un élément à la liste
  @param New   Élémént à ajouter
  @return Index de l'élément ajouté
*}
function TIntegerList.Add(New: Int64): Integer;
begin
  Result := _Add(New);
end;

{*
  Insert un élément dans la liste à un index spécifié
  @param Index   Index où insérer l'élément
  @param New     Élémént à insérer
  @return Index de l'élément inséré
*}
function TIntegerList.Insert(Index: Integer; New: Int64): Integer;
begin
  Result := _Insert(Index, New);
end;

{*
  Supprime un élément de la liste
  @param Index   Index de l'élément à supprimer
  @return L'élément supprimé
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
  Crée une nouvelle instance de TUnsignedIntList
  @param IntSize   Taille en octets des entiers (entre 1 et 4 inclus)
  @throws EIntListError Taille d'entier incorrecte
*}
constructor TUnsignedIntList.Create(IntSize: Integer = 4);
begin
  // On vérifie que IntSize est entre 1 et 4 inclus
  if (IntSize < 1) or (IntSize > 4) then
    raise EIntListError.CreateFmt(sScWrongIntSize, [IntSize]);

  inherited Create(IntSize);
end;

{*
  Crée une nouvelle instance de TUnsignedIntList, copie d'une autre source
  @param Source    Objet source à copier
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
  Tableau indexé par leurs index des éléments de la liste
  @param Index   Index de l'élément à lire
  @return Valeur de l'élément à l'index Index
  @throws EListError Index de liste hors bornes
*}
function TUnsignedIntList.GetItems(Index: Integer): LongWord;
begin
  _GetItems(Index, Result);
  MakeItGood(Result);
end;

{*
  Modifie le tableau indexé par leurs index des éléments de la liste
  @param Index   Index de l'élément à modifier
  @param New     Nouvelle valeur de l'élément à l'index Index
  @throws EListError Index de liste hors bornes
*}
procedure TUnsignedIntList.SetItems(Index: Integer; New: LongWord);
begin
  _SetItems(Index, New);
end;

{*
  S'assure que Value contient la valeur exacte selon la taille stockée
  Cette procédure remplit les octets non stockés de manière à transformer un
  entier stocké sur un nombre quelconque d'octets en LongWord (sur 4 octets)
  @param Value   Valeur à traiter
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
  // On s'évite un travail inutile si Value est déjà stocké sur 4 octets
  if ItemSize = 4 then
    Exit;

  // On initialise RecVal.Int (et donc aussi RecVal.Ints)
  RecVal.Int := Value;

  // On remplit les octets non stockés avec des 0
  for I := ItemSize+1 to 4 do
    RecVal.Ints[I] := 0;

  // On réactualise Value
  Value := RecVal.Int;
end;

{*
  Copie les propriétés d'un objet dans l'objet destination
  @param Dest   Objet destination dans lequel copier la liste
*}
procedure TUnsignedIntList.AssignTo(Dest: TPersistent);
var
  DestStrings: TStrings;
begin
  // Si Dest est un TStrings on le remplit avec les formes chaînes des éléments
  if Dest is TStrings then
  begin
    DestStrings := TStrings(Dest);
    DestStrings.Clear;
    // Le principe "Reset-HasMoreValue-Read" étant plus rapide que
    // "for 0 to Count-1" avec TScList, on l'utilise pour remplire le TStrings
    Reset;
    while HasMoreValue do
      DestStrings.Append(IntToStr(Read));
  end else
    inherited;
end;

{*
  Indique si une classe est une classe d'assignation de la liste
  Les classes descendantes de TScList peuvent surcharger cette méthode pour
  indiquer quelles sont les classes d'assignation de la liste.
  @param ScListClass   Classe à tester
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
  Appelez Assign pour copier les propriétés ou d'autres attributs d'un objet sur
  un autre.
  @param Source   Objet source à copier
*}
procedure TUnsignedIntList.Assign(Source: TPersistent);
var
  SourceStrings: TStrings;
  I: Integer;
  Val: Int64;
begin
  // Si Source est un TStrings, on convertit les chaînes en entiers
  if Source is TStrings then
  begin
    SourceStrings := TStrings(Source);
    Clear;
    for I := 0 to SourceStrings.Count-1 do
    begin
      Val := StrToInt64(SourceStrings[I]);
      // On vérifie que Val est bien un LongWord
      if (Val < 0) or (Val > High(LongWord)) then
        raise EConvertError.CreateFmt(sScWrongLongWord, [SourceStrings[I]]);
      Add(Val);
    end;
  end else
    inherited;
end;

{*
  Lit un élément de la liste à la position courante
  @return L'élément lu
*}
function TUnsignedIntList.Read: LongWord;
begin
  _Read(Result);
  MakeItGood(Result);
end;

{*
  Écrit un élément dans la liste à la position courante
  @param New   Élémént à écrire
*}
procedure TUnsignedIntList.Write(New: LongWord);
begin
  _Write(New);
end;

{*
  Ajoute un élément à la liste
  @param New   Élémént à ajouter
  @return Index de l'élément ajouté
*}
function TUnsignedIntList.Add(New: LongWord): Integer;
begin
  Result := _Add(New);
end;

{*
  Insert un élément dans la liste à un index spécifié
  @param Index   Index où insérer l'élément
  @param New     Élémént à insérer
  @return Index de l'élément inséré
*}
function TUnsignedIntList.Insert(Index: Integer; New: LongWord): Integer;
begin
  Result := _Insert(Index, New);
end;

{*
  Supprime un élément de la liste
  @param Index   Index de l'élément à supprimer
  @return L'élément supprimé
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
  Crée une nouvelle instance de TExtendedList
*}
constructor TExtendedList.Create;
begin
  inherited Create(SizeOf(Extended));
end;

{*
  Crée une nouvelle instance de TExtendedList, copie d'une autre source
  @param Source    Objet source à copier
*}
constructor TExtendedList.CreateAssign(Source: TPersistent);
begin
  Create;
  Assign(Source);
end;

{*
  Tableau indexé par leurs index des éléments de la liste
  @param Index   Index de l'élément à lire
  @return Valeur de l'élément à l'index Index
  @throws EListError Index de liste hors bornes
*}
function TExtendedList.GetItems(Index: Integer): Extended;
begin
  _GetItems(Index, Result);
end;

{*
  Modifie le tableau indexé par leurs index des éléments de la liste
  @param Index   Index de l'élément à modifier
  @param New     Nouvelle valeur de l'élément à l'index Index
  @throws EListError Index de liste hors bornes
*}
procedure TExtendedList.SetItems(Index: Integer; New: Extended);
begin
  _SetItems(Index, New);
end;

{*
  Copie les propriétés d'un objet dans l'objet destination
  @param Dest   Objet destination dans lequel copier la liste
*}
procedure TExtendedList.AssignTo(Dest: TPersistent);
var
  DestStrings: TStrings;
begin
  // Si Dest est un TStrings on le remplit avec les formes chaînes des éléments
  if Dest is TStrings then
  begin
    DestStrings := TStrings(Dest);
    DestStrings.Clear;
    // Le principe "Reset-HasMoreValue-Read" étant plus rapide que
    // "for 0 to Count-1" avec TScList, on l'utilise pour remplire le TStrings
    Reset;
    while HasMoreValue do
      DestStrings.Append(FloatToStr(Read));
  end else
    inherited;
end;

{*
  Indique si une classe est une classe d'assignation de la liste
  Les classes descendantes de TScList peuvent surcharger cette méthode pour
  indiquer quelles sont les classes d'assignation de la liste.
  @param ScListClass   Classe à tester
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
  Appelez Assign pour copier les propriétés ou d'autres attributs d'un objet sur
  un autre.
  @param Source   Objet source à copier
*}
procedure TExtendedList.Assign(Source: TPersistent);
var
  SourceStrings: TStrings;
  I: Integer;
begin
  // Si Source est un TStrings, on convertit les chaînes en nombres flottants
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
  Lit un élément de la liste à la position courante
  @return L'élément lu
*}
function TExtendedList.Read: Extended;
begin
  _Read(Result);
end;

{*
  Écrit un élément dans la liste à la position courante
  @param New   Élémént à écrire
*}
procedure TExtendedList.Write(New: Extended);
begin
  _Write(New);
end;

{*
  Ajoute un élément à la liste
  @param New   Élémént à ajouter
  @return Index de l'élément ajouté
*}
function TExtendedList.Add(New: Extended): Integer;
begin
  Result := _Add(New);
end;

{*
  Insert un élément dans la liste à un index spécifié
  @param Index   Index où insérer l'élément
  @param New     Élémént à insérer
  @return Index de l'élément inséré
*}
function TExtendedList.Insert(Index: Integer; New: Extended): Integer;
begin
  Result := _Insert(Index, New);
end;

{*
  Supprime un élément de la liste
  @param Index   Index de l'élément à supprimer
  @return L'élément supprimé
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
  Annule un élément de la file, en le supprimant
  @param AItem   Élément à supprimer
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
  @param AObject   Objet à supprimer
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
  Crée une nouvelle instance de TCustomValueBucketList
  @param AKeySize    Taille du type des clefs
  @param AKeyInfo    RTTI du type des clefs
  @param ADataSize   Taille du type des données
  @param ADataInfo   RTTI du type des données
*}
constructor TCustomValueBucketList.Create(AKeySize: Integer;
  AKeyInfo: PTypeInfo; ADataSize: Integer; ADataInfo: PTypeInfo);
begin
  inherited Create;

  BucketCount := DefaultBucketCount;

  if Assigned(AKeyInfo) then
  begin
    FKeySize := TypeSize(AKeyInfo);
    if AKeyInfo.Kind in NeedInitTypeKinds then
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
    if ADataInfo.Kind in NeedInitTypeKinds then
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
  Crée une nouvelle instance de TCustomValueBucketList
  @param AKeySize    Taille du type des clefs
  @param ADataSize   Taille du type des données
*}
constructor TCustomValueBucketList.Create(AKeySize, ADataSize: Integer);
begin
  Create(AKeySize, nil, ADataSize, nil);
end;

{*
  Crée une nouvelle instance de TCustomValueBucketList
  @param AKeySize    Taille du type des clefs
  @param ADataInfo   RTTI du type des données
*}
constructor TCustomValueBucketList.Create(AKeySize: Integer;
  ADataInfo: PTypeInfo);
begin
  Create(AKeySize, nil, 0, ADataInfo);
end;

{*
  Crée une nouvelle instance de TCustomValueBucketList
  @param AKeyInfo    RTTI du type des clefs
  @param ADataSize   Taille du type des données
*}
constructor TCustomValueBucketList.Create(AKeyInfo: PTypeInfo;
  ADataSize: Integer);
begin
  Create(0, AKeyInfo, ADataSize, nil);
end;

{*
  Crée une nouvelle instance de TCustomValueBucketList
  @param AKeyInfo    RTTI du type des clefs
  @param ADataInfo   RTTI du type des données
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
  Call-back de ForEach utilisé par la méthode Assign
  @param Key        Clef
  @param Data       Données associées à la clef
  @param Continue   Positionner à False pour interrompre l'énumération
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
  Modifie le nombre de boîtes de hashage
  BucketCount ne peut être modifié que lorsque la liste est vide
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
  Identifie la boîte dans laquelle ranger un élément donné
  Les descendants de TCustomValueBucketList surchargent en général BucketFor
  pour utiliser au mieux les spécificités du type de données traité, afin
  d'avoir un remplissage optimal.
  L'implémentation par défauut de BucketFor est de prendre les 4 premiers
  octets de la clef, et de prendre le modulo par le nombre de boîtes de
  hashage (BucketCount).
  @param Key   Clef
  @return Numéro de la boîte de hashage pour la clef Key
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
  Détermine si deux clefs sont identiques
  L'implémentation par défaut de KeyEquals teste si le contenu brut pointé
  par Key1 et Key2 est équivalent sur KeySize octets.
  Ce comportement doit être surchargé pour les types pour lesquels on ne peut
  pas se fier qu'au contenu brut (chaînes, tableaux dynamiques, record non
  packed avec des trous, etc.)
  @param Key1   Première clef
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
  Localise la boîte de hashage et l'index d'une paire par sa clef
  @param Key      Clef à rechercher
  @param Bucket   En sortie : boîte de hashage (même si FindItem renvoie False)
  @param Index    En sortie : index dans la boîte (indéterminé si renvoie False)
  @return True si la paire a été trouvée, False sinon
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
  Ajoute une paire clef/données dans une boîte donnée
  AddItem ne vérifie pas quue la clef donnée doit effectivement se mettre dans
  la boîte spécifiée. Assurez-vous d'appeler AddItem avec des données valides.
  @param Bucket   Index de la boîte dans laquelle ajouter la paire
  @param Key      Clef
  @param Data     Données
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
  Supprime une paire clef/données, identifiée par sa position
  @param Bucket   Index de la boîte
  @param Index    Index dans la boîte
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
  Supprime une paire clef/données, identifiée par sa position
  @param Bucket   Index de la boîte
  @param Index    Index dans la boîte
  @param Data     En sortie : les données de la paire qui est supprimée
*}
procedure TCustomValueBucketList.ExtractItem(Bucket, Index: Integer;
  out Data);
begin
  CopyData(Buckets[Bucket].Items[Index].Data^, Data, DataSize, DataInfo);
  DeleteItem(Bucket, Index);
end;

{*
  Récupère les données liées à une clef
  @param Key    Clef
  @param Data   En sortie : données associées à la clef Key
  @throws EListError Élément non trouvé
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
  Modifie les données liées à une clef
  Il doit déjà exister une paire clef/données avec cette clef.
  @param Key    Clef
  @param Data   Données associées à la clef Key
  @throws EListError Élément non trouvé
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
  Ajoute une paire clef/données
  Il ne peut pas encore exister de paire clef/données avec cette clef.
  @param Key    Clef
  @param Data   Données associées à la clef Key
  @throws EListError Liste verrouillée lors d'une opération ForEach active
  @throws EListError Élément dupliqué
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
  Retire une paire clef/données
  La paire ne doit pas nécessairement être présente dans la liste.
  @param Key   Clef
  @throws EListError Liste verrouillée lors d'une opération ForEach active
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
  Retire une paire clef/données
  La paire ne doit pas nécessairement être présente dans la liste.
  @param Key    Clef
  @param Data   En sortie : les données de la paire qui est supprimée
  @throws EListError Liste verrouillée lors d'une opération ForEach active
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
  @param Info   Pointeur libre retransmis dans le paramètre Info de Proc
  @return False si ForEach a été interrompu par le paramètre Continue,
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
  Appelle une méthode de call-back pour chaque paire clef/valeur de la liste
  @param Event   Méthode de call-back
  @return False si ForEach a été interrompu par le paramètre Continue,
          True sinon
*}
function TCustomValueBucketList.ForEach(Event: TValueBucketEvent): Boolean;
begin
  with TMethod(Event) do
    Result := ForEach(TValueBucketProc(Code), Data);
end;

{*
  Copie le contenu d'une liste
  La liste source doit avoir le même type d'éléments, sinon une exception
  EConvertError sera déclenchée.
  @param Source   Liste source
  @throws EConvertError La liste source n'a pas le même type d'élément
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
  Détermine si un clef est référencée dans la liste
  @param Key   Clef à rechercher
  @return True si la clef est référencée, False sinon
*}
function TCustomValueBucketList.Exists(const Key): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindItem(Key, Bucket, Index);
end;

{*
  Recherche une clef dans la liste
  @param Key    Clef à rechercher
  @param Data   En sortie : données associées à la clef, si elle existe
  @return True si la clef a été trouvée, False sinon
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
  Récupère les données liées à une clef
  @param Key    Clef
  @param Data   En sortie : données associées à la clef Key
  @throws EListError Élément non trouvé
*}
procedure TValueBucketList.Get(const Key; out Data);
begin
  GetData(Key, Data);
end;

{*
  Modifie les données liées à une clef
  Il doit déjà exister une paire clef/données avec cette clef.
  @param Key    Clef
  @param Data   Données associées à la clef Key
  @throws EListError Élément non trouvé
*}
procedure TValueBucketList.Put(const Key, Data);
begin
  SetData(Key, Data);
end;

{*
  Ajoute une paire clef/données
  Il ne peut pas encore exister de paire clef/données avec cette clef.
  @param Key    Clef
  @param Data   Données associées à la clef Key
  @throws EListError Liste verrouillée lors d'une opération ForEach active
  @throws EListError Élément dupliqué
*}
procedure TValueBucketList.Add(const Key, Data);
begin
  AddData(Key, Data);
end;

{*
  Retire une paire clef/données
  La paire ne doit pas nécessairement être présente dans la liste.
  @param Key   Clef
  @throws EListError Liste verrouillée lors d'une opération ForEach active
*}
procedure TValueBucketList.Remove(const Key);
begin
  RemoveData(Key);
end;

{*
  Retire une paire clef/données
  La paire ne doit pas nécessairement être présente dans la liste.
  @param Key    Clef
  @param Data   En sortie : les données de la paire qui est supprimée
  @throws EListError Liste verrouillée lors d'une opération ForEach active
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
  I: Integer; /// Variable de contrôle de boucle interne

initialization
  AppParams := TScStrings.Create;
  // On remplit AppParams avec les paramètres envoyés à l'application
  for I := 1 to ParamCount do
    AppParams.Append(ParamStr(I));

finalization
  AppParams.Free;
end.

