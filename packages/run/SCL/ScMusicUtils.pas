{*
  Définit des classes travaillant sur des éléments de musique
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit ScMusicUtils;

interface

uses
  SysUtils;

type
  {*
    Générée lors d'une erreur de musique
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  EMusicError = class(Exception);

  {*
    Représentation interne à l'unité d'une note de musique
    Les informations sont stockées dans les bits selon oonnnaaa :
    oo  : deux bits pour l'octave (0 à 4)
    nnn : trois bits pour la note (0, do, à 6, si)
    aaa : trois bits pour l'altération (0, bb, à 4, x)
    Rem : 255 (11111111) est le do naturel de la 5ème octave
  *}
  TMusicNoteInfo = type Byte;

  {*
    Représentation interne à l'unité d'un intervalle de musique
  *}
  TMusicIntervalInfo = record
    Size : Shortint;                 /// Taille de l'intervalle
    Tones : Shortint;                /// Nombre de tons entiers
    DiatonalHalfTones : Shortint;    /// Nombre de demi-tons diatoniques
    ChromatonalHalfTones : Shortint; /// Nombre de demi-tons chromatiques
  end;

  {*
    Classe enregistrant les informations concernant une note
    Les notes enregistrable vont du do classique de la clé de Fa au
    do quatre octaves plus haut.
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TMusicNote = class
  private
    FNote : TMusicNoteInfo; /// Représentation interne de la note

    function GetSubProp(Index : integer) : Shortint;
    procedure SetSubProp(Index : integer; New : Shortint);
    function GetVibrations : Word;
  public
    constructor Create(ANote, AChange, AOctave : Shortint); overload;
    constructor Create(AOctave : Shortint = 1); overload;

    /// Composante Note de la note (de 0 pour do à 6 pour si)
    property Note : Shortint index 1 read GetSubProp write SetSubProp;
    /// Composante Altération de la note (en demi-tons chromatiques)
    property Change : Shortint index 2 read GetSubProp write SetSubProp;
    /// Composante Octave de la note (de 1 à 5)
    property Octave : Shortint index 3 read GetSubProp write SetSubProp;

    property Vibrations : Word read GetVibrations;
  end;

  {*
    Classe enregistrant les informations concernant un intervalle
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TMusicInterval = class
  private
    FInterval : TMusicIntervalInfo; /// Représentation interne de l'intervalle

    function GetSubProp(Index : integer) : Shortint;
  public
    constructor Create(Note : TMusicNote); overload;
    constructor Create(Note1, Note2 : TMusicNote); overload;

    /// Composante Taille de l'interval
    property Size : Shortint index 1 read GetSubProp;
    /// Composante Tons de l'interval
    property Tones : Shortint index 2 read GetSubProp;
    /// Composante Demi-tons diatoniques de l'interval
    property DiatonalHalfTones : Shortint index 3 read GetSubProp;
    /// Composante Demi-tons chromatiques de l'interval
    property ChromatonalHalfTones : Shortint index 4 read GetSubProp;
  end;

const
  {*
    Tableau des harmoniques de do
    Ces constantes ont été calculées manuellement. Elles représentent les
    chiffres correspondant aux harmoniques de do. Elles sont utilisées par le
    calcul du nombre de vibrations d'une note.
  *}
  HarmonyOfDo : array[1..16] of TMusicNoteInfo =
  (2, 66, 98, 130, 146, 162, 177, 194, 202, 210, 219, 226, 234, 241, 242, 255);

implementation

var
  EmptyInterval : TMusicIntervalInfo; /// Intervalle vide

{*
  Extrait la composante Note d'une représentation de note
  @param NoteInfo   Représentation interne d'une note
  @return La composante Note de la note
*}
function ExtractNote(NoteInfo : TMusicNoteInfo) : Shortint;
begin
  // Si la note est 255 c'est un do sinon on prend les bits 3 à 5
  if NoteInfo = 255 then Result := 0 else
    Result := (NoteInfo and 56) shr 3;
end;

{*
  Extrait la composante Altération d'une représentation de note
  @param NoteInfo   Représentation interne d'une note
  @return La composante Altération de la note
*}
function ExtractChange(NoteInfo : TMusicNoteInfo) : Shortint;
begin
  // Si la note est 255 c'est un do naturel sinon on prend les bits 0 à 2 -2
  if NoteInfo = 255 then Result := 0 else
    Result := (NoteInfo and 7) - 2;
end;

{*
  Extrait la composante Octave d'une représentation de note
  @param NoteInfo   Représentation interne d'une note
  @return La composante Octave de la note
*}
function ExtractOctave(NoteInfo : TMusicNoteInfo) : Shortint;
begin
  // Si la note est 255 c'est la 5ème octave sinon on prend les bits 6 à 7 +1
  if NoteInfo = 255 then Result := 5 else
    Result := (NoteInfo and 192) shr 6 + 1;
end;

{*
  Calcule le nombre de commas constituant un intervalle
  @param Interval   Représentation d'un intervalle
  @return Nombre de commas constituant l'intervalle Interval
*}
function CommasOfInterval(Interval : TMusicIntervalInfo) : integer;
begin
  { Le commas est une unité qui divise le ton en 9, le demi-ton chromatique
    en 4 et le demi-ton chromatique en 5
    D'où -> 1 ton                  = 9 commas
         -> 1 demi-ton diatonique  = 4 commas
         -> 1 demi-ton chromatique = 5 commas }
  Result := Interval.Tones*9 +
    Interval.DiatonalHalfTones*4 +
    Interval.ChromatonalHalfTones*5;
end;

{*
  Simplifie un intervalle en additionnant les demi-tons qui peuvent l'être
  @param Interval   Intervalle à simplifier
*}
procedure SimplifyInterval(var Interval : TMusicIntervalInfo);
begin
  // 1/2 ton diatonique + 1/2 ton chromatique = 1 ton
  while (Interval.DiatonalHalfTones > 0) and
    (Interval.ChromatonalHalfTones > 0) do
  begin
    dec(Interval.DiatonalHalfTones);
    dec(Interval.ChromatonalHalfTones);
    inc(Interval.Tones);
  end;

  // -1/2 ton chromatique + -1/2 ton chromatique = -1 ton
  while (Interval.DiatonalHalfTones < 0) and
    (Interval.ChromatonalHalfTones < 0) do
  begin
    inc(Interval.DiatonalHalfTones);
    inc(Interval.ChromatonalHalfTones);
    dec(Interval.Tones);
  end;
end;

{*
  Additionne deux intervalles
  @param Inter1   Premier intervalle
  @param Inter2   Second intervalle
  @return Somme des intervalles Inter1 et Inter2
*}
function AddIntervals(Inter1, Inter2 : TMusicIntervalInfo) : TMusicIntervalInfo;
begin
  // Révisez votre cours de solfège pour comprendre ça
  Result := Inter1;
  inc(Result.Size, Inter2.Size-1);
  inc(Result.Tones, Inter2.Tones);
  inc(Result.DiatonalHalfTones, Inter2.DiatonalHalfTones);
  inc(Result.ChromatonalHalfTones, Inter2.ChromatonalHalfTones);
end;

{*
  Soustrait deux intervalles
  @param Inter1   Premier intervalle
  @param Inter2   Second intervalle
  @return Différence des intervalles Inter1 et Inter2
*}
function SubIntervals(Inter1, Inter2 : TMusicIntervalInfo) : TMusicIntervalInfo;
begin
  // Révisez votre cours de solfège pour comprendre ça
  Result := Inter1;
  dec(Result.Size, Inter2.Size+1);
  dec(Result.Tones, Inter2.Tones);
  dec(Result.DiatonalHalfTones, Inter2.DiatonalHalfTones);
  dec(Result.ChromatonalHalfTones, Inter2.ChromatonalHalfTones);
end;

{*
  Oppose un intervalle
  @param Interval   Intervalle à opposer
*}
procedure NegInterval(var Interval : TMusicIntervalInfo);
begin
  with Interval do
  begin
    Size := -Size;
    Tones := -Tones;
    DiatonalHalfTones := -DiatonalHalfTones;
    ChromatonalHalfTones := -ChromatonalHalfTones;
  end;
end;

{*
  S'assure qu'un intervalle est positif
  @param Interval   Intervalle à traiter
*}
procedure AbsInterval(var Interval : TMusicIntervalInfo);
begin
  if CommasOfInterval(Interval) < 0 then NegInterval(Interval);
end;

{*
  Compare deux intervalles
  @param Inter1   Premier intervalle
  @param Inter2   Second intervalle
  @return 0 si les intervalles sont égaux, un nombre positif si le premier
          est supérieur au second, et un nombre négatif dans le cas inverse
*}
function CompareIntervals(Inter1, Inter2 : TMusicIntervalInfo) : integer;
begin
  Result := CommasOfInterval(SubIntervals(Inter1, Inter2));
end;

{*
  Compare une note au la naturel de la même octave
  @param NoteInfo   Représentation de la note
  @return Intervalle entre la note et le la naturel de la même octave
*}
function CompareWithLa(NoteInfo : TMusicNoteInfo) : TMusicIntervalInfo;
var Note, Change : Shortint;
begin
  // Révisez votre cours de solfège pour comprendre ça

  Note := ExtractNote(NoteInfo);

  Result.Size := Note-4;
  Result.Tones := 0;
  Result.DiatonalHalfTones := 0;
  Result.ChromatonalHalfTones := 0;

  if Note > 2 then Result.Tones := Note-5 else
  begin
    Result.Tones := Note-4;
    Result.DiatonalHalfTones := -1;
  end;

  Change := ExtractChange(NoteInfo);
  if Note > 5 then inc(Result.ChromatonalHalfTones, Change) else
  if Note < 5 then dec(Result.ChromatonalHalfTones, Change) else
  begin
    if Change < 0 then dec(Result.ChromatonalHalfTones, Change) else
      inc(Result.ChromatonalHalfTones, Change);
  end;

  SimplifyInterval(Result);
end;

{*
  Compare une note au la naturel de la première octave
  @param NoteInfo   Représentation de la note
  @return Intervalle entre la note et le la naturel de la première octave
*}
function CompareWithFirstLa(NoteInfo : TMusicNoteInfo) : TMusicIntervalInfo;
var Octave : Byte;
begin
  // On calcule d'abord la différence avec le la de l'octave
  Result := CompareWithLa(NoteInfo);
  // On récupère l'octave et on retire 1 (puisque que les octaves vont de 1 à 5)
  Octave := ExtractOctave(NoteInfo)-1;
  // On ajoute à la taille le nombre d'octaves multiplié par 7
  inc(Result.Size, 7*Octave);
  // On ajoute au nombre de tons le nombre d'octaves multiplié par 5
  inc(Result.Tones, 5*Octave);
  // On ajoute au nombre de tons diatoniques le nombre d'octaves multiplié par 2
  inc(Result.DiatonalHalfTones, 2*Octave);
  // On simplifie l'interval résultant
  SimplifyInterval(Result);
end;

{*
  Décode une représentation interne d'une note
  @param NoteInfo   Représentation interne d'une note
  @param Note       Composante Note de la note
  @param Change     Composante Altération de la note
  @param Octave     Composante Octave de la note
*}
procedure DecodeNote(NoteInfo : TMusicNoteInfo;
  var Note, Change, Octave : Shortint);
begin
  Note := ExtractNote(NoteInfo);
  Change := ExtractChange(NoteInfo);
  Octave := ExtractOctave(NoteInfo);
end;

{*
  Encode une note en représentation interne
  @param Note       Composante Note de la note
  @param Change     Composante Altération de la note
  @param Octave     Composante Octave de la note
  @return Représentation interne de la note
*}
function EncodeNote(Note : Shortint; Change : Shortint = 0;
                    Octave : Shortint = 3) : TMusicNoteInfo;
begin
  if (Note < 0) or (Note > 6) or (Change < -2) or (Change > 2) or
     (Octave < 1) or (Octave > 4) then
    raise EMusicError.Create('Informations de note incorrectes');
  Result := ((Octave-1) shl 6) + (Change+2) + (Note shl 3);
end;

{*
  Compare deux notes
  @param Note1   Première note
  @param Note2   Seconde note
  @return 0 si les notes sont égales, un nombre positif si la première est
          supérieure à la seconde, et un nombre négatif dans le cas inverse
*}
function CompareNotes(Note1, Note2 : TMusicNoteInfo) : integer;
begin
  Result := CommasOfInterval(SubIntervals(CompareWithFirstLa(Note1),
                                          CompareWithFirstLa(Note2)));
end;

{*
  Calcule l'intervalle entre deux notes
  @param Note1   Première note
  @param Note2   Seconde note
  @return Intervalle entre les notes Note1 et Note2
*}
function CalcInterval(Note1, Note2 : TMusicNoteInfo) : TMusicIntervalInfo;
begin
  Result := SubIntervals(CompareWithFirstLa(Note1), CompareWithFirstLa(Note2));
  AbsInterval(Result);
end;

{*
  Calcule le nombre de vibrations par secondes d'une note
  @param NoteInfo   Représentation de la note
  @return Nombre de vibrations par secondes de la note
*}
function CalcVibrations(NoteInfo : TMusicNoteInfo) : Word;
var Octave : Shortint;
    Interval : TMusicIntervalInfo;
    LowPos, HighPos, Temp : Word;
    InverseLowHigh : boolean;
begin
  // On extrait l'octave
  Octave := ExtractOctave(NoteInfo);
  // Le La de l'octave 1 fait 55 vibrations
  Result := 55;
  // Pour chaque octave supérieure, on multiplie par 2
  while Octave > 1 do
  begin
    Result := Result*2;
    dec(Octave);
  end;

  // On compare avec le La de l'octave
  Interval := CompareWithLa(NoteInfo);

  // Si la note est le la, on a fini
  if CompareIntervals(Interval, EmptyInterval) = 0 then exit;

  // Si l'interval est "négatif", c'est que la note est en-dessous du la,
  // auquel cas il faudra inverser Low et High (voir plus loin)
  InverseLowHigh := CommasOfInterval(Interval) < 0;
  // On prend l'absolu de l'interval
  AbsInterval(Interval);

  begin // ici on commence la recherche dans les harmoniques de do
    // Le principe est de trouver un interval dans les harmonique de do
    // qui soit équivalent à l'interval calculé plus haut
    // On en obtient deux positions dans ces harmoniques (LowPos et HighPos)
    LowPos := 1;
    HighPos := 1;
    while LowPos < 16 do
    begin
      HighPos := LowPos+1;
      while HighPos <= 16 do
      begin
        if CompareIntervals(Interval, CalcInterval(HarmonyOfDo[LowPos],
                            HarmonyOfDo[HighPos])) = 0 then
          Break;
        inc(HighPos);
      end;
      if HighPos <= 16 then Break;
      inc(LowPos);
    end;
    if LowPos >= 16 then
      raise EMusicError.Create('Aucun interval dans les harmoniques de do ne correspond');
  end; // recherche dans les harmoniques de do terminée

  // On inverse LowPos et HighPos si on doit (voir plus haut)
  if InverseLowHigh then
  begin
    Temp := LowPos;
    LowPos := HighPos;
    HighPos := Temp;
  end;

  // On calcule le résultat selon le principe appris en classe de solfège
  Result := (Result*HighPos) div LowPos;
end;

/////////////////////////
/// Classe TMusicNote ///
/////////////////////////

{*
  Crée une nouvelle instance de TMusicNote
  @param ANote     Composante Note de la note
  @param AChange   Composante Altération de la note
  @param AOctave   Composante Octave de la note
*}
constructor TMusicNote.Create(ANote, AChange, AOctave : Shortint);
begin
  inherited Create;
  FNote := EncodeNote(ANote, AChange, AOctave);
end;

{*
  Crée une nouvelle instance de TMusicNote
  La note créée est un la naturel.
  @param AOctave   Composante Octave de la note
*}
constructor TMusicNote.Create(AOctave : Shortint = 1);
begin
  inherited Create;
  FNote := EncodeNote(5, 0, AOctave);
end;

{*
  Lit une des sous-propriétés de la note
  @param Index   Index de la sous-propriété
  @return Valeur de la sous-propriété
*}
function TMusicNote.GetSubProp(Index : integer) : Shortint;
begin
  case Index of
    1 : Result := ExtractNote(FNote);
    2 : Result := ExtractChange(FNote);
    3 : Result := ExtractOctave(FNote);
    else Result := 0;
  end;
end;

{*
  Modifie une des sous-propriétés de la note
  @param Index   Index de la sous-propriété
  @param New     Nouvelle valeur de la sous-propriété
*}
procedure TMusicNote.SetSubProp(Index : integer; New : Shortint);
begin
  case Index of
    1 : FNote := EncodeNote(New, Change, Octave);
    2 : FNote := EncodeNote(Note, New, Octave);
    3 : FNote := EncodeNote(Note, Change, New);
  end;
end;

{*
  Nombre de vibrations par seconde de la note
  @return Nombre de vibrations par seconde de la note
*}
function TMusicNote.GetVibrations : Word;
begin
  Result := CalcVibrations(FNote);
end;

/////////////////////////////
/// Classe TMusicInterval ///
/////////////////////////////

{*
  Crée une nouvelle instance de TMusicInterval
  @param Note   Note déterminant, avec le premier la naturel, l'intervalle
*}
constructor TMusicInterval.Create(Note : TMusicNote);
begin
  inherited Create;
  FInterval := CompareWithFirstLa(Note.FNote);
end;

{*
  Crée une nouvelle instance de TMusicInterval
  @param Note1   Note déterminant, avec Note2, l'intervalle
  @param Note2   Note déterminant, avec Note1, l'intervalle
*}
constructor TMusicInterval.Create(Note1, Note2 : TMusicNote);
begin
  inherited Create;
  FInterval := CalcInterval(Note1.FNote, Note2.FNote);
end;

{*
  Lit une des sous-propriétés de l'intervalle
  @param Index   Index de la sous-propriété
  @return Valeur de la sous-propriété
*}
function TMusicInterval.GetSubProp(Index : integer) : Shortint;
begin
  with FInterval do case Index of
    1 : Result := Size;
    2 : Result := Tones;
    3 : Result := DiatonalHalfTones;
    4 : Result := ChromatonalHalfTones;
    else Result := 0;
  end;
end;

initialization
  with EmptyInterval do
  begin
    Size := 1;
    Tones := 0;
    DiatonalHalfTones := 0;
    ChromatonalHalfTones := 0;
  end;
end.

