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
  EMusicError = class(Exception);
  // Classe d'exeption de base pour les erreurs de musique

  // Les deux types suivant sont utilisés en interne,
  // vous ne devez pas vous en servir
  TMusicNoteInfo = type Byte;
    // oonnnaaa :
    // oo  : deux bits pour l'octave (0 à 4)
    // nnn : trois bits pour la note (0, do, à 6, si)
    // aaa : trois bits pour l'altération (0, bb, à 4, x)
    // Rem : 255 (11111111) est le do naturel de la 5ème octave
  TMusicIntervalInfo = record
    Size : Shortint;                 // Taille de l'intervalle
    Tones : Shortint;                // Nombre de tons entiers
    DiatonalHalfTones : Shortint;    // Nombre de demi-tons diatoniques
    ChromatonalHalfTones : Shortint; // Nombre de demi-tons chromatiques
  end;

  TMusicNote = class
  // Classe enregistrant les informations concernant une note
  // Les notes enregistrable vont du do classique de la clé de Fa au
  // do quatre octaves plus haut
  private
    FNote : TMusicNoteInfo;
    function GetSubProp(Index : integer) : Shortint;
    procedure SetSubProp(Index : integer; New : Shortint);
    function GetVibrations : Word;
  public
    constructor Create(ANote, AChange, AOctave : Shortint); overload;
      // Crée une nouvelle instance avec les informations indiquées
    constructor Create(AOctave : Shortint = 1); overload;
      // Crée une nouvelle instance avec le do naturel de l'octave spécifiée

    property Note   : Shortint index 1 read GetSubProp write SetSubProp;
      // Composante Note de la note (de 0 pour do à 6 pour si)
    property Change : Shortint index 2 read GetSubProp write SetSubProp;
      // Composante Altération de la note (de -2 pour double-bémol à 2 pour double-dièse)
    property Octave : Shortint index 3 read GetSubProp write SetSubProp;
      // Composante Octave de la note (de 1 à 5)
    property Vibrations : Word read GetVibrations;
      // Nombre de vibrations de la note
  end;

  TMusicInterval = class
  // Classe enregistrant les informations concernant un interval
  private
    FInterval : TMusicIntervalInfo;
    function GetSubProp(Index : integer) : Shortint;
  public
    constructor Create(Note : TMusicNote); overload;
      // Crée une nouvelle instance avec l'interval entre Note et le premier la
    constructor Create(Note1, Note2 : TMusicNote); overload;
      // Crée une nouvelle instance avec l'interval entre Note1 et Note2

    property Size : Shortint index 1 read GetSubProp;
      // Composante Taille de l'interval
    property Tones : Shortint index 2 read GetSubProp;
      // Composante Tons de l'interval
    property DiatonalHalfTones : Shortint index 3 read GetSubProp;
      // Composante Demi-tons diatoniques de l'interval
    property ChromatonalHalfTones : Shortint index 4 read GetSubProp;
      // Composante Demi-tons chromatiques de l'interval
  end;

const
  HarmonyOfDo : array[1..16] of TMusicNoteInfo =
  (2, 66, 98, 130, 146, 162, 177, 194, 202, 210, 219, 226, 234, 241, 242, 255); {
  Les constantes ci-dessus ont été calculées manuellement. Elles représentent
  les chiffres correspondant aux harmoniques de do. Elles sont utilisées par le
  calcul du nombre de vibrations d'une note.                                    }

implementation

var
  EmptyInterval : TMusicIntervalInfo;

function ExtractNote(NoteInfo : TMusicNoteInfo) : Shortint;
begin
  // Si la note est 255 c'est un do sinon on prend les bits 3 à 5
  if NoteInfo = 255 then Result := 0 else
    Result := (NoteInfo and 56) shr 3;
end;

function ExtractChange(NoteInfo : TMusicNoteInfo) : Shortint;
begin
  // Si la note est 255 c'est un do naturel sinon on prend les bits 0 à 2 -2
  if NoteInfo = 255 then Result := 0 else
    Result := (NoteInfo and 7) - 2;
end;

function ExtractOctave(NoteInfo : TMusicNoteInfo) : Shortint;
begin
  // Si la note est 255 c'est la 5ème octave sinon on prend les bits 6 à 7 +1
  if NoteInfo = 255 then Result := 5 else
    Result := (NoteInfo and 192) shr 6 + 1;
end;

function CommasOfInterval(Interval : TMusicIntervalInfo) : integer;
begin
  // Le commas est une unité qui divise le ton en 9, le demi-ton chromatique
  // en 4 et le demi-ton chromatique en 5
  // D'où -> 1 ton                  = 9 commas
  //      -> 1 demi-ton diatonique  = 4 commas
  //      -> 1 demi-ton chromatique = 5 commas
  Result := Interval.Tones*9 +
            Interval.DiatonalHalfTones*4 +
            Interval.ChromatonalHalfTones*5;
end;

procedure SimplifyInterval(var Interval : TMusicIntervalInfo);
begin
  // 1/2 ton diatonique + 1/2 ton chromatique = 1 ton
  while (Interval.DiatonalHalfTones > 0) and (Interval.ChromatonalHalfTones > 0) do
  begin
    dec(Interval.DiatonalHalfTones);
    dec(Interval.ChromatonalHalfTones);
    inc(Interval.Tones);
  end;

  // -1/2 ton chromatique + -1/2 ton chromatique = -1 ton
  while (Interval.DiatonalHalfTones < 0) and (Interval.ChromatonalHalfTones < 0) do
  begin
    inc(Interval.DiatonalHalfTones);
    inc(Interval.ChromatonalHalfTones);
    dec(Interval.Tones);
  end;
end;

function AddIntervals(Inter1, Inter2 : TMusicIntervalInfo) : TMusicIntervalInfo;
begin
  // Révisez votre cours de solfège pour comprendre ça
  Result := Inter1;
  inc(Result.Size, Inter2.Size-1);
  inc(Result.Tones, Inter2.Tones);
  inc(Result.DiatonalHalfTones, Inter2.DiatonalHalfTones);
  inc(Result.ChromatonalHalfTones, Inter2.ChromatonalHalfTones);
end;

function SubIntervals(Inter1, Inter2 : TMusicIntervalInfo) : TMusicIntervalInfo;
begin
  // Révisez votre cours de solfège pour comprendre ça
  Result := Inter1;
  dec(Result.Size, Inter2.Size+1);
  dec(Result.Tones, Inter2.Tones);
  dec(Result.DiatonalHalfTones, Inter2.DiatonalHalfTones);
  dec(Result.ChromatonalHalfTones, Inter2.ChromatonalHalfTones);
end;

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

procedure AbsInterval(var Interval : TMusicIntervalInfo);
begin
  if CommasOfInterval(Interval) < 0 then NegInterval(Interval);
end;

function CompareIntervals(Inter1, Inter2 : TMusicIntervalInfo) : integer;
begin
  Result := CommasOfInterval(SubIntervals(Inter1, Inter2));
end;

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

procedure DecodeNote(NoteInfo : TMusicNoteInfo; var Note, Change, Octave : Shortint);
begin
  Note := ExtractNote(NoteInfo);
  Change := ExtractChange(NoteInfo);
  Octave := ExtractOctave(NoteInfo);
end;

function EncodeNote(Note : Shortint; Change : Shortint = 0;
                    Octave : Shortint = 3) : TMusicNoteInfo;
begin
  if (Note < 0) or (Note > 6) or (Change < -2) or (Change > 2) or
     (Octave < 1) or (Octave > 4) then
    raise EMusicError.Create('Informations de note incorrectes');
  Result := ((Octave-1) shl 6) + (Change+2) + (Note shl 3);
end;

function CompareNotes(Note1, Note2 : TMusicNoteInfo) : integer;
begin
  Result := CommasOfInterval(SubIntervals(CompareWithFirstLa(Note1),
                                          CompareWithFirstLa(Note2)));
end;

function CalcInterval(Note1, Note2 : TMusicNoteInfo) : TMusicIntervalInfo;
begin
  Result := SubIntervals(CompareWithFirstLa(Note1), CompareWithFirstLa(Note2));
  AbsInterval(Result);
end;

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

constructor TMusicNote.Create(ANote, AChange, AOctave : Shortint);
begin
  inherited Create;
  FNote := EncodeNote(ANote, AChange, AOctave);
end;

constructor TMusicNote.Create(AOctave : Shortint = 1);
begin
  inherited Create;
  FNote := EncodeNote(5, 0, AOctave);
end;

function TMusicNote.GetSubProp(Index : integer) : Shortint;
begin
  case Index of
    1 : Result := ExtractNote(FNote);
    2 : Result := ExtractChange(FNote);
    3 : Result := ExtractOctave(FNote);
    else Result := 0;
  end;
end;

procedure TMusicNote.SetSubProp(Index : integer; New : Shortint);
begin
  case Index of
    1 : FNote := EncodeNote(New, Change, Octave);
    2 : FNote := EncodeNote(Note, New, Octave);
    3 : FNote := EncodeNote(Note, Change, New);
  end;
end;

function TMusicNote.GetVibrations : Word;
begin
  Result := CalcVibrations(FNote);
end;

/////////////////////////////
/// Classe TMusicInterval ///
/////////////////////////////

constructor TMusicInterval.Create(Note : TMusicNote);
begin
  inherited Create;
  FInterval := CompareWithFirstLa(Note.FNote);
end;

constructor TMusicInterval.Create(Note1, Note2 : TMusicNote);
begin
  inherited Create;
  FInterval := CalcInterval(Note1.FNote, Note2.FNote);
end;

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

