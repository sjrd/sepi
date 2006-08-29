unit ScLists;

interface

uses
  SysUtils, Classes;

type
  StringsOps = class
  // Propose une série de méthodes pouvant être appliquée à des instances de
  // TStrings (toutes les méthodes de recherche ignorent la différence
  // MAJ/min dans le cas d'un TStringList avec CaseSensitive à False)
  private
    class function CompareStrings(Strings : TStrings; const Str1, Str2 : string) : boolean;
  public
    class function  IndexOf      (Strings : TStrings; const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
      // Recherche la chaîne Str entre BeginSearchAt et EndSearchAt inclus
      // Si EndSearchAt = -1, la recherche s'effectue jusqu'au bout de la liste
    class function  FindText     (Strings : TStrings; const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
      // Recherche le texte Text entre BeginSearchAt et EndSearchAt inclus
      // Si EndSearchAt = -1, la recherche s'effectue jusqu'au bout de la liste
    class function  FindFirstWord(Strings : TStrings; const Word : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
      // Recherche le mot Word entre BeginSearchAt et EndSearchAt inclus
      // mais uniquement en première position (il peut y avoir des espaces devant)
      // Si EndSearchAt = -1, la recherche s'effectue jusqu'au bout de la liste
    class function  FindAtPos    (Strings : TStrings; const SubStr : string; Position : integer = 1; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
      // Recherche la sous-chaîne SubStr à la position Position de chaque
      // chaîne de la liste entre BeginSearchAt et EndSearchAt inclus
      // Si EndSearchAt = -1, la recherche s'effectue jusqu'au bout de la liste
    class procedure CopyFrom     (Strings : TStrings; Source : TStrings; Index : integer = 0; Count : integer = -1);
      // Clear + AddFrom avec les mêmes paramètres
    class procedure AddFrom      (Strings : TStrings; Source : TStrings; Index : integer = 0; Count : integer = -1);
      // Ajoute les Count chaînes à partir de Index depuis Source à Strings
    class procedure FromString   (Strings : TStrings; const Str, Delim : string; const NotIn : string = '');
      // Clear + AddFromString avec les mêmes paramètres
    class procedure AddFromString(Strings : TStrings; const Str, Delim : string; const NotIn : string = '');
      // Découpe Str en sous-chaînes délimitées par Delim et les ajoute
      // Les chaînes Delim se trouvant entre un caractère impair de NotIn et le
      // caractère pair lui correspondant ne sont pas pris en compte
  end;

  TScStrings = class(TStringList)
  // Amélioration de TStringList pour lui ajouter les méthodes de StringsOps
  // ainsi qu'un index interne permettant de parcourir aisément toutes les
  // chaînes dans l'ordre
  private
    FIndex : integer;
    function GetHasMoreString : boolean;
    procedure SetIndex(New : integer);
  public
    constructor Create;
    constructor CreateFromFile(const FileName : TFileName);
    constructor CreateFromString(const Str, Delim : string; const NotIn : string = '');
    constructor CreateAssign(Source : TPersistent);

    // Les méthodes suivantes sont des appels aux méthodes correspondantes
    // de la classe StringsOps
    function  IndexOfEx    (const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
    function  FindText     (const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
    function  FindFirstWord(const Word : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
    function  FindAtPos    (const SubStr : string; Position : integer = 1; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
    procedure CopyFrom     (Source : TStrings; Index : integer = 0; Count : integer = -1);
    procedure AddFrom      (Source : TStrings; Index : integer = 0; Count : integer = -1);
    procedure FromString   (const Str, Delim : string; const NotIn : string = '');
    procedure AddFromString(const Str, Delim : string; const NotIn : string = '');

    // Gestion de l'index
    procedure Reset;
      // Remet l'index à 0
    function NextString : string;
      // Renvoie la chaîne suivante (incrémente aussi l'index interne)
    property HasMoreString : boolean read GetHasMoreString;
      // True s'il y a encore une ou plusieurs chaîne(s), False sinon
    property Index : integer read FIndex write SetIndex;
      // Index
  end;

  TScList = class;
  TScListClass = class of TScList;

  TScList = class(TPersistent)
  // Classe de base pour les listes dont les éléments sont de taille homogène
  // Ne créez pas d'instance de TScList, mais créez plutôt des instances de
  // ses classes descendantes
    // Ne pas utiliser pour des listes de chaînes, de pointeurs ou d'objets ;
    // dans ces cas, utiliser respectivement
    // TStringList (Classes), TList (Classes) et TObjectList (Contnrs)
  private
    FStream : TMemoryStream;
    FItemSize : integer;

    function GetCount : integer;
    procedure SetCount(New : integer);
    function GetHasMoreValue : boolean;
    function GetIndex : integer;
    procedure SetIndex(New : integer);
  protected
    procedure DefineProperties(Filer : TFiler); override;

    procedure AssignTo(Dest : TPersistent); override;

    // Renvoie True si ScListClass est une classe d'assignation
    function IsAssignClass(ScListClass : TScListClass) : boolean; virtual;

    // Les méthodes suivantes doivent être appellées par les méthodes de même
    // nom (sans le _) des descendants
    procedure _Read(var Buffer);
    procedure _Write(var Buffer);
    procedure _GetItems(AIndex : integer; var Buffer);
    procedure _SetItems(AIndex : integer; var Buffer);
    function _Add(var Buffer) : integer;
    function _Insert(AIndex : integer; var Buffer) : integer;
    procedure _Delete(AIndex : integer; var Buffer);

    property ItemSize : integer read FItemSize;
  public
    constructor Create(ItemSize : integer);
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;
    procedure Clear;
    procedure Reset;

    procedure LoadFromStream(Stream : TStream);
    procedure SaveToStream(Stream : TStream);
    procedure LoadFromFile(const FileName : TFileName);
    procedure SaveToFile(const FileName : TFileName);

    property Count : integer read GetCount write SetCount;
    property HasMoreValue : boolean read GetHasMoreValue;
    property Index : integer read GetIndex write SetIndex;
  end;

  TIntegerList = class(TScList)
  // Gère une liste d'entiers
  private
    function GetItems(Index : integer) : Int64;
    procedure SetItems(Index : integer; New : Int64);
    procedure MakeItGood(var Value : Int64);
  protected
    procedure AssignTo(Dest : TPersistent); override;

    function IsAssignClass(ScListClass : TScListClass) : boolean; override;
  public
    constructor Create(IntSize : integer = 4);
    constructor CreateAssign(Source : TPersistent; IntSize : integer = 4);

    procedure Assign(Source : TPersistent); override;

    function Read : Int64;
    procedure Write(New : Int64);
    function Add(New : Int64) : integer;
    function Insert(Index : integer; New : Int64) : integer;
    function Delete(Index : integer) : Int64;

    property Items[index : integer] : Int64 read GetItems write SetItems; default;
  end;

  TUnsignedIntList = class(TScList)
  // Gère une liste d'entiers non signés
  private
    function GetItems(Index : integer) : LongWord;
    procedure SetItems(Index : integer; New : LongWord);
    procedure MakeItGood(var Value : LongWord);
  protected
    procedure AssignTo(Dest : TPersistent); override;

    function IsAssignClass(ScListClass : TScListClass) : boolean; override;
  public
    constructor Create(IntSize : integer = 4);
    constructor CreateAssign(Source : TPersistent; IntSize : integer = 4);

    procedure Assign(Source : TPersistent); override;

    function Read : LongWord;
    procedure Write(New : LongWord);
    function Add(New : LongWord) : integer;
    function Insert(Index : integer; New : LongWord) : integer;
    function Delete(Index : integer) : LongWord;

    property Items[index : integer] : LongWord read GetItems write SetItems; default;
  end;

  TExtendedList = class(TScList)
  // Gère une liste de Extended
  private
    function GetItems(Index : integer) : Extended;
    procedure SetItems(Index : integer; New : Extended);
  protected
    procedure AssignTo(Dest : TPersistent); override;

    function IsAssignClass(ScListClass : TScListClass) : boolean; override;
  public
    constructor Create;
    constructor CreateAssign(Source : TPersistent);

    procedure Assign(Source : TPersistent); override;

    function Read : Extended;
    procedure Write(New : Extended);
    function Add(New : Extended) : integer;
    function Insert(Index : integer; New : Extended) : integer;
    function Delete(Index : integer) : Extended;

    property Items[index : integer] : Extended read GetItems write SetItems; default;
  end;

var AppParams : TScStrings;
// Contient la liste des paramètres de l'application sous forme de TScStrings
// Vous pouvez y supprimer ou rajouter des éléments : AppParams n'est utilisé
// par aucune routine dans la SCL

implementation

uses
  ScUtils, ScStrUtils, ScConsts;

{$REGION 'Classe StringsOps'}

/////////////////////////
/// Classe StringsOps ///
/////////////////////////

class function StringsOps.CompareStrings(Strings : TStrings; const Str1, Str2 : string) : boolean;
begin
  // Si Strings est une instance de TStringList et que sa propriété
  // CaseSensitive est à True, la comparaison se fait en respectant la casse.
  if (Strings is TStringList) and TStringList(Strings).CaseSensitive then
    Result := CompareStr(Str1, Str2) = 0
  // Dans tous les autres cas, la comparaison se fait sans respecter la casse.
  else
    Result := CompareText(Str1, Str2) = 0;
end;

class function StringsOps.IndexOf(Strings : TStrings; const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
var Strs : TStrings;
begin
  Strs := Strings;
  with Strings do
  begin
    // On vérifie que BeginSearchAt et EndSearchAt sont des entrées correctes
    if BeginSearchAt < 0 then BeginSearchAt := 0;
    if (EndSearchAt < 0) or (EndSearchAt >= Count) then EndSearchAt := Count-1;
    Result := BeginSearchAt; // On commence la recherche à BeginSearchAt
    while Result <= EndSearchAt do // On termine à EndSearchAt
      if CompareStrings(Strs, Str, Strings[BeginSearchAt]) then exit else
        inc(BeginSearchAt);
    Result := -1;
  end;
end;

class function StringsOps.FindText(Strings : TStrings; const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
var I, Len : integer;
    Strs : TStrings;
begin
  Strs := Strings;
  with Strings do
  begin
    // On vérifie que BeginSearchAt et EndSearchAt sont des entrées correctes
    if BeginSearchAt < 0 then BeginSearchAt := 0;
    if (EndSearchAt < 0) or (EndSearchAt >= Count) then EndSearchAt := Count-1;
    Len := Length(Str);
    Result := BeginSearchAt; // On commence la recherche à BeginSearchAt
    while Result <= EndSearchAt do // On termine à EndSearchAt
    begin
      for I := 1 to Length(Strings[Result])-Len+1 do
        if CompareStrings(Strs, Str, Copy(Strings[Result], I, Len)) then exit;
      inc(Result);
    end;
    Result := -1;
  end;
end;

class function StringsOps.FindFirstWord(Strings : TStrings; const Word : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
var Strs : TStrings;
begin
  Strs := Strings;
  with Strings do
  begin
    // On vérifie que BeginSearchAt et EndSearchAt sont des entrées correctes
    if BeginSearchAt < 0 then BeginSearchAt := 0;
    if (EndSearchAt < 0) or (EndSearchAt >= Count) then EndSearchAt := Count-1;
    Result := BeginSearchAt; // On commence la recherche à BeginSearchAt
    while Result <= EndSearchAt do // On termine à EndSearchAt
      if CompareStrings(Strs, Word, GetXWord(Trim(Strings[Result]), 1)) then exit else
        inc(Result);
    Result := -1;
  end;
end;

class function StringsOps.FindAtPos(Strings : TStrings; const SubStr : string; Position : integer = 1; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
var Len : integer;
    Strs : TStrings;
begin
  Strs := Strings;
  with Strings do
  begin
    // On vérifie que BeginSearchAt et EndSearchAt sont des entrées correctes
    if BeginSearchAt < 0 then BeginSearchAt := 0;
    if (EndSearchAt < 0) or (EndSearchAt >= Count) then EndSearchAt := Count-1;
    Len := Length(SubStr);
    Result := BeginSearchAt; // On commence la recherche à BeginSearchAt
    while Result <= EndSearchAt do // On termine à EndSearchAt
      if CompareStrings(Strs, SubStr, Copy(Strings[Result], Position, Len)) then exit else
        inc(Result);
    Result := -1;
  end;
end;

class procedure StringsOps.CopyFrom(Strings : TStrings; Source : TStrings; Index : integer = 0; Count : integer = -1);
begin
  Strings.Clear;
  AddFrom(Strings, Source, Index, Count);
end;

class procedure StringsOps.AddFrom(Strings : TStrings; Source : TStrings; Index : integer = 0; Count : integer = -1);
var I, EndAt : integer;
begin
  // On vérifie que Index et Count sont des entrées correctes
  if Index < 0 then exit;
  if (Count < 0) or (Index+Count > Source.Count) then EndAt := Source.Count-1 else
    EndAt := Index+Count-1;
  // On recopie les chaînes
  for I := Index to EndAt do
    Strings.Append(Source[I]);
end;

class procedure StringsOps.FromString(Strings : TStrings; const Str, Delim : string; const NotIn : string = '');
begin
  Strings.Clear;
  AddFromString(Strings, Str, Delim, NotIn);
end;

class procedure StringsOps.AddFromString(Strings : TStrings; const Str, Delim : string; const NotIn : string = '');
var I, J, Len : integer;
    NotIn1, NotIn2 : string;
    C : Char;
begin
  with Strings do
  begin
    // On vérifie que NotIn contient un nombre pair de caractères
    if (Length(NotIn) mod 2) = 1 then
      raise Exception.Create(sScNotInMustPairsOfChars);
    // On vérifie qu'il n'y a pas d'interférence entre Delim et NotIn
    for I := 1 to Length(NotIn) do if Pos(NotIn[I], Delim) > 0 then
      raise Exception.Create(sScDelimMustDifferentThanNotIn);

    Len := Length(Str);
    // Séparation de NotIn en NotIn1 et NotIn2
    NotIn1 := '';
    NotIn2 := '';
    for I := 1 to Length(NotIn) do if (I mod 2) = 1 then
      NotIn1 := NotIn1+NotIn[I] else NotIn2 := NotIn2+NotIn[I];

    I := 1;
    while True do
    begin
      // On boucle jusqu'à trouver un caractère qui n'est pas dans Delim
      // On ignore de ce fait plusieurs caractères de Delim à la suite
      while (I <= Len) and (Pos(Str[I], Delim) <> 0) do inc(I);
      if (I > Len) then Break;
      // On recherche le caractère de Delim suivant
      J := I;
      while (J <= Len) and (Pos(Str[J], Delim) = 0) do
      begin
        // Si on trouve un caractère de NotIn1, on boucle jusqu'à trouver le
        // caractère correspondant de NotIn2
        if Pos(Str[J], NotIn1) > 0 then
        begin
          C := NotIn2[Pos(Str[J], NotIn1)];
          inc(J);
          while (J <= Len) and (Str[J] <> C) do inc(J);
        end;
        inc(J);
      end;
      // On ajoute la chaîne repérée par les caractères de Delim
      Add(Copy(Str, I, J-I));
      I := J;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'Classe TScStrings'}

/////////////////////////
/// Classe TScStrings ///
/////////////////////////

constructor TScStrings.Create;
begin
  inherited Create;
  FIndex := 0;
end;

constructor TScStrings.CreateFromFile(const FileName : TFileName);
begin
  Create;
  LoadFromFile(FileName);
end;

constructor TScStrings.CreateFromString(const Str, Delim : string; const NotIn : string = '');
begin
  Create;
  FromString(Str, Delim, NotIn);
end;

constructor TScStrings.CreateAssign(Source : TPersistent);
begin
  Create;
  Assign(Source);
end;

function TScStrings.GetHasMoreString : boolean;
begin
  Result := Index < Count;
end;

procedure TScStrings.SetIndex(New : integer);
begin
  if New >= 0 then FIndex := New;
end;

function TScStrings.IndexOfEx(const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
begin
  Result := StringsOps.IndexOf(Self, Str, BeginSearchAt, EndSearchAt);
end;

function TScStrings.FindText(const Str : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
begin
  Result := StringsOps.FindText(Self, Text, BeginSearchAt, EndSearchAt);
end;

function TScStrings.FindFirstWord(const Word : string; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
begin
  Result := StringsOps.FindFirstWord(Self, Word, BeginSearchAt, EndSearchAt);
end;

function TScStrings.FindAtPos(const SubStr : string; Position : integer = 1; BeginSearchAt : integer = 0; EndSearchAt : integer = -1) : integer;
begin
  Result := StringsOps.FindAtPos(Self, SubStr, Position, BeginSearchAt, EndSearchAt);
end;

procedure TScStrings.CopyFrom(Source : TStrings; Index : integer = 0; Count : integer = -1);
begin
  StringsOps.CopyFrom(Self, Source, Index, Count);
end;

procedure TScStrings.AddFrom(Source : TStrings; Index : integer = 0; Count : integer = -1);
begin
  StringsOps.AddFrom(Self, Source, Index, Count);
end;

procedure TScStrings.FromString(const Str, Delim : string; const NotIn : string = '');
begin
  StringsOps.FromString(Self, Str, Delim, NotIn);
end;

procedure TScStrings.AddFromString(const Str, Delim : string; const NotIn : string = '');
begin
  StringsOps.AddFromString(Self, Str, Delim, NotIn);
end;

procedure TScStrings.Reset;
begin
  FIndex := 0;
end;

function TScStrings.NextString : string;
begin
  if HasMoreString then Result := Strings[Index] else Result := '';
  inc(FIndex);
end;

{$ENDREGION}

{$REGION 'Classe TScList'}

//////////////////////
/// Classe TScList ///
//////////////////////

constructor TScList.Create(ItemSize : integer);
begin
  FStream := TMemoryStream.Create;
  FItemSize := ItemSize;
end;

destructor TScList.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TScList.GetCount : integer;
begin
  // Le nombre d'éléments est la taille du flux divisées la taille d'un élément
  Result := FStream.Size div FItemSize;
end;

procedure TScList.SetCount(New : integer);
begin
  // La taille du flux est le nombre d'éléments multiplié par leur taille
  if New <= 0 then FStream.SetSize(0) else
    FStream.SetSize(New*FItemSize);
end;

function TScList.GetHasMoreValue : boolean;
begin
  // Il y a encore une ou plusieurs valeurs à lire ssi la position du flux
  // est inférieure à sa taille
  Result := FStream.Position < FStream.Size;
end;

function TScList.GetIndex : integer;
begin
  // L'index est la position du flux divisé par la taille des éléments
  Result := FStream.Position div FItemSize;
end;

procedure TScList.SetIndex(New : integer);
begin
  // On vérifie que New est bien un index correct
  if (New < 0) or (New > Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [New]);
  // La position du flux est l'index multiplié par la taille des éléments
  FStream.Position := New*FItemSize;
end;

procedure TScList.DefineProperties(Filer : TFiler);
begin
  inherited;
  // La ligne suivante crée une propriété publiée inexistante qui permet
  // d'enregistrer le contenu via WriteComponent et de le lire via
  // ReadComponent
  Filer.DefineBinaryProperty('Items', LoadFromStream, SaveToStream, True);
end;

procedure TScList.AssignTo(Dest : TPersistent);
begin
  // On autorise l'assignation ssi Dest.ClassType fait partie de la liste des
  // Classes d'assignation et que les tailles d'élements sont les mêmes
  if (Dest is TScList) and
     (TScList(Dest).FItemSize = FItemSize) and
     IsAssignClass(TScListClass(Dest.ClassType)) then
  begin
    TScList(Dest).FStream.LoadFromStream(FStream);
  end else
  inherited;
end;

function TScList.IsAssignClass(ScListClass : TScListClass) : boolean;
begin
  // Si on remonte jusqu'ici, c'est que ce n'est une classe d'assignation
  Result := False;
end;

procedure TScList._Read(var Buffer);
begin
  // Lit un élément à la position courante
  FStream.ReadBuffer(Buffer, FItemSize);
end;

procedure TScList._Write(var Buffer);
begin
  // Écrit l'élément à la position courante
  FStream.WriteBuffer(Buffer, FItemSize);
end;

procedure TScList._GetItems(AIndex : integer; var Buffer);
begin
  // On vérifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);
  // On se place au bon endroit pour lire l'élément
  Index := AIndex;
  // On lit l'élément
  _Read(Buffer);
end;

procedure TScList._SetItems(AIndex : integer; var Buffer);
begin
  // On vérifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);
  // On se place au bon endroit pour écrire l'élément
  Index := AIndex;
  // On écrit l'élément
  _Write(Buffer);
end;

function TScList._Add(var Buffer) : integer;
begin
  // Le résultat est l'index du nouvel élément, soit le nombre d'éléments actuel
  Result := Count;
  // On se place à la fin du flux
  Index := Result;
  // On écrit l'élément ; comme c'est un flux mémoire, cela agrandira
  // automatiquemnt le flux de façon à accepter les nouvelles données
  _Write(Buffer);
end;

function TScList._Insert(AIndex : integer; var Buffer) : integer;
var Temp : TMemoryStream;
begin
  // On vérifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex > Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);

  // Si AIndex vaut Count, on appelle _Add, sinon, on effectue le traitement
  if AIndex = Count then Result := _Add(Buffer) else
  begin
    Temp := TMemoryStream.Create;
    Result := AIndex; // La valeur de retour est l'index du nouvel élément

    // On copie tous les éléments à partir de AIndex dans Temp
    Index := AIndex;
    Temp.CopyFrom(FStream, FStream.Size-FStream.Position);

    // On agrandi la liste
    Count := Count+1;

    // On écrit le nouvel élément et à sa suite le contenu de Temp
    Index := AIndex;
    _Write(Buffer);
    FStream.CopyFrom(Temp, 0);

    Temp.Free;
  end;
end;

procedure TScList._Delete(AIndex : integer; var Buffer);
var Temp : TMemoryStream;
begin
  // On vérifie que AIndex est un index correct
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.CreateFmt(sScIndexOutOfRange, [AIndex]);

  Temp := TMemoryStream.Create;

  // On lit la valeur de retour à l'emplacement qui va être supprimé
  Index := AIndex;
  _Read(Buffer);

  // On copie tous les éléments après celui-là dans Temp
  if FStream.Position <> FStream.Size then // Pour éviter le CopyFrom(..., 0)
    Temp.CopyFrom(FStream, FStream.Size-FStream.Position);

  // On réduit la liste
  Count := Count-1;

  // On recopie le contenu de Temp à partir de l'emplacement qui a été supprimé
  Index := AIndex;
  FStream.CopyFrom(Temp, 0);

  Temp.Free;
end;

procedure TScList.Assign(Source : TPersistent);
begin
  // On autorise l'assignation ssi Source.ClassType fait partie de la liste des
  // classes d'assignation et que les tailles d'élements sont les mêmes
  if (Source is TScList) and
     (TScList(Source).FItemSize = FItemSize) and
     IsAssignClass(TScListClass(Source.ClassType)) then
  begin
    FStream.LoadFromStream(TScList(Source).FStream);
  end else
  inherited;
end;

procedure TScList.Clear;
begin
  Count := 0;
end;

procedure TScList.Reset;
begin
  Index := 0;
end;

procedure TScList.LoadFromStream(Stream : TStream);
var Size : Int64;
begin
  // On lit la taille sur 8 octets (Int64)
  Stream.Read(Size, 8);
  FStream.Size := 0;
  // Si la taille est plus grande que 0, on fait un CopyFrom
  if Size > 0 then
    FStream.CopyFrom(Stream, Size);
end;

procedure TScList.SaveToStream(Stream : TStream);
var Size : Int64;
begin
  // On écrit la taille sur 8 octets (Int64)
  Size := FStream.Size;
  Stream.Write(Size, 8);
  // Si la taille est plus grande que 0, on fait un CopyFrom
  if Size > 0 then
    Stream.CopyFrom(FStream, 0);
end;

procedure TScList.LoadFromFile(const FileName : TFileName);
var FileStream : TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TScList.SaveToFile(const FileName : TFileName);
var FileStream : TFileStream;
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

///////////////////////////
/// Classe TIntegerList ///
///////////////////////////

constructor TIntegerList.Create(IntSize : integer = 4);
begin
  // On vérifie que IntSize est entre 1 et 8 inclus
  if (IntSize < 1) or (IntSize > 8) then
    raise ECreateError.CreateFmt(sScWrongIntSize, [IntSize]);
  inherited Create(IntSize);
end;

constructor TIntegerList.CreateAssign(Source : TPersistent; IntSize : integer = 4);
begin
  Create(IntSize);
  Assign(Source);
end;

function TIntegerList.GetItems(Index : integer) : Int64;
begin
  _GetItems(Index, Result);
  MakeItGood(Result);
end;

procedure TIntegerList.SetItems(Index : integer; New : Int64);
begin
  _SetItems(Index, New);
end;

procedure TIntegerList.MakeItGood(var Value : Int64);
// Cette procédure remplis les octets non stockés de manière à transformer
// un entier sur un nombre quelconque d'octets en Int64 (sur 8 octets)
type
  TRecVal = record
    case integer of
      1 : (Int : Int64);
      2 : (Ints : array[1..8] of Shortint);
  end;
var RecVal : TRecVal;
    I : integer;
    Remplis : Shortint;
begin
  RecVal.Int := Value;
    // On initialise RecVal.Int (et donc aussi RecVal.Ints)
  if RecVal.Ints[ItemSize] < 0 then Remplis := -1 else Remplis := 0;
    // Si le Shortint à la position ItemSize (c-à-d l'octet de poids le plus
    // fort stocké) est négatif (c-à-d si le nombre complet est négatif),
    // on remplit les suivant avec -1 (11111111b) et sinon avec 0 (00000000b)
  for I := ItemSize+1 to 8 do RecVal.Ints[I] := Remplis;
    // On remplit les les octets non stockés avec la valeur de Remplis
  Value := RecVal.Int;
    // On réactualise Value
end;

procedure TIntegerList.AssignTo(Dest : TPersistent);
var DestStrings : TStrings;
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

function TIntegerList.IsAssignClass(ScListClass : TScListClass) : boolean;
begin
  if ScListClass.InheritsFrom(TIntegerList) then Result := True else
  Result := inherited IsAssignClass(ScListClass);
end;

procedure TIntegerList.Assign(Source : TPersistent);
var SourceStrings : TStrings;
    I : integer;
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

function TIntegerList.Read : Int64;
begin
  _Read(Result);
  MakeItGood(Result);
end;

procedure TIntegerList.Write(New : Int64);
begin
  _Write(New);
end;

function TIntegerList.Add(New : Int64) : integer;
begin
  Result := _Add(New);
end;

function TIntegerList.Insert(Index : integer; New : Int64) : integer;
begin
  Result := _Insert(Index, New);
end;

function TIntegerList.Delete(Index : integer) : Int64;
begin
  _Delete(Index, Result);
  MakeItGood(Result);
end;

{$ENDREGION}

{$REGION 'Classe TUnsignedIntList'}

///////////////////////////////
/// Classe TUnsignedIntList ///
///////////////////////////////

constructor TUnsignedIntList.Create(IntSize : integer = 4);
begin
  // On vérifie que IntSize est entre 1 et 4 inclus
  if (IntSize < 1) or (IntSize > 4) then
    raise ECreateError.CreateFmt(sScWrongIntSize, [IntSize]);
  inherited Create(IntSize);
end;

constructor TUnsignedIntList.CreateAssign(Source : TPersistent; IntSize : integer = 4);
begin
  Create(IntSize);
  Assign(Source);
end;

function TUnsignedIntList.GetItems(Index : integer) : LongWord;
begin
  _GetItems(Index, Result);
  MakeItGood(Result);
end;

procedure TUnsignedIntList.SetItems(Index : integer; New : LongWord);
begin
  _SetItems(Index, New);
end;

procedure TUnsignedIntList.MakeItGood(var Value : LongWord);
// Cette procédure remplis les octets non stockés de manière à transformer
// un entier sur un nombre quelconque d'octets en LongWord (sur 4 octets)
type
  TRecVal = record
    case integer of
      1 : (Int : LongWord);
      2 : (Ints : array[1..4] of Byte);
  end;
var RecVal : TRecVal;
    I : integer;
begin
  RecVal.Int := Value;
    // On initialise RecVal.Int (et donc aussi RecVal.Ints)
  for I := ItemSize+1 to 4 do RecVal.Ints[I] := 0;
    // On remplit les octets non stockés avec des 0
  Value := RecVal.Int;
    // On réactualise Value
end;

procedure TUnsignedIntList.AssignTo(Dest : TPersistent);
var DestStrings : TStrings;
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

function TUnsignedIntList.IsAssignClass(ScListClass : TScListClass) : boolean;
begin
  if ScListClass.InheritsFrom(TUnsignedIntList) then Result := True else
  Result := inherited IsAssignClass(ScListClass);
end;

procedure TUnsignedIntList.Assign(Source : TPersistent);
var SourceStrings : TStrings;
    I : integer;
    Val : Int64;
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

function TUnsignedIntList.Read : LongWord;
begin
  _Read(Result);
  MakeItGood(Result);
end;

procedure TUnsignedIntList.Write(New : LongWord);
begin
  _Write(New);
end;

function TUnsignedIntList.Add(New : LongWord) : integer;
begin
  Result := _Add(New);
end;

function TUnsignedIntList.Insert(Index : integer; New : LongWord) : integer;
begin
  Result := _Insert(Index, New);
end;

function TUnsignedIntList.Delete(Index : integer) : LongWord;
begin
  _Delete(Index, Result);
  MakeItGood(Result);
end;

{$ENDREGION}

{$REGION 'Classe TExtendedList'}

////////////////////////////
/// Classe TExtendedList ///
////////////////////////////

constructor TExtendedList.Create;
begin
  inherited Create(sizeof(Extended));
end;

constructor TExtendedList.CreateAssign(Source : TPersistent);
begin
  Create;
  Assign(Source);
end;

function TExtendedList.GetItems(Index : integer) : Extended;
begin
  _GetItems(Index, Result);
end;

procedure TExtendedList.SetItems(Index : integer; New : Extended);
begin
  _SetItems(Index, New);
end;

procedure TExtendedList.AssignTo(Dest : TPersistent);
var DestStrings : TStrings;
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

function TExtendedList.IsAssignClass(ScListClass : TScListClass) : boolean;
begin
  if ScListClass.InheritsFrom(TExtendedList) then Result := True else
  Result := inherited IsAssignClass(ScListClass);
end;

procedure TExtendedList.Assign(Source : TPersistent);
var SourceStrings : TStrings;
    I : integer;
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

function TExtendedList.Read : Extended;
begin
  _Read(Result);
end;

procedure TExtendedList.Write(New : Extended);
begin
  _Write(New);
end;

function TExtendedList.Add(New : Extended) : integer;
begin
  Result := _Add(New);
end;

function TExtendedList.Insert(Index : integer; New : Extended) : integer;
begin
  Result := _Insert(Index, New);
end;

function TExtendedList.Delete(Index : integer) : Extended;
begin
  _Delete(Index, Result);
end;

{$ENDREGION}

//////////////////////////////////////
/// Initialization et Finalization ///
//////////////////////////////////////

var I : integer;

initialization
  AppParams := TScStrings.Create;
  // On remplit AppParams avec les paramètres envoyés à l'application
  for I := 1 to ParamCount do AppParams.Append(ParamStr(I));
finalization
  AppParams.Free;
end.
