{*
  Définit quelques routines usuelles
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit ScUtils;

interface

uses
{$IFDEF LINUX}
  QDialogs, QGraphics,
{$ENDIF}
  SysUtils, Classes, Math;

{$IFDEF LINUX}
const
  dtCustom = mtCustom;
  dtInformation = mtInformation;
  dtWarning = mtWarning;
  dtError = mtError;
  dtConfirmation = mtConfirmation;

  dbOK = [mbOK];
  dbOKCancel = mbOKCancel;
  dbYesNo = [mbYes, mbNo];
  dbYesNoCancel = mbYesNoCancel;
  dbRetryCancel = [mbRetry, mbCancel];
  dbAbortRetryIgnore = mbAbortRetryIgnore;

  drOK = mrOK;
  drYes = mrYes;
  drNo = mrNo;
  drCancel = mrCancel;
  drAbort = mrAbort;
  drRetry = mrRetry;
  drIgnore = mrIgnore;
{$ENDIF}

type

{$REGION 'Exceptions'}

  {*
    Déclenchée lorsqu'une erreur a été reportée par une API Windows
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  EAPIError = class(Exception)
  private
    FErrorCode : integer; /// Code d'erreur HRESULT
  public
    constructor Create(Error : integer); overload;
    constructor Create; overload;

    property ErrorCode : integer read FErrorCode;
  end platform;

{$ENDREGION}

  {*
    Représente un point situé dans un espace en trois dimensions
  *}
  T3DPoint = record
    X : integer; /// Coordonnée X du point
    Y : integer; /// Coordonnée Y du point
    Z : integer; /// Coordonnée Z du point
  end;

  {*
    Type d'une boîte de dialogue
    dtCustom       : Type générique
    dtInformation  : Information
    dtWarning      : Avertissement
    dtError        : Erreur
    dtConfirmation : Confirmation
  *}
  {$IFDEF MSWINDOWS}
    TDialogType = (dtCustom, dtInformation, dtWarning, dtError, dtConfirmation);
  {$ELSE}
    TDialogType = type TMsgDlgType;
  {$ENDIF}

  {*
    Boutons présents dans une boîte de dialogue
    dbOK               : OK
    dbOKCancel         : OK et Annuler
    dbYesNo            : Oui et Non
    dbYesNoCancel      : Oui, Non et Annuler
    dbRetryCancel      : Réessayer et Annuler
    dbAbortRetryIgnore : Abandonner, Réessayer et Ignorer
  *}
  {$IFDEF MSWINDOWS}
    TDialogButtons = (dbOK, dbOKCancel, dbYesNo, dbYesNoCancel, dbRetryCancel,
                      dbAbortRetryIgnore);
  {$ELSE}
    TDialogButtons = type TMsgDlgButtons;
  {$ENDIF}

  {*
    Résultat de l'affichage d'une boîte de dialogue
    drOK     : OK
    drYes    : Oui
    drNo     : Non
    drCancel : Annuler
    drAbort  : Abandonner
    drRetry  : Réessayer
    drIgnore : Ignorer
  *}
  {$IFDEF MSWINDOWS}
    TDialogResult = (drOK, drYes, drNo, drCancel, drAbort, drRetry, drIgnore);
  {$ELSE}
    TDialogResult = type Word;
  {$ENDIF}

  {*
    Ensemble de Byte
  *}
  TSysByteSet = set of Byte;

  {*
    Ensemble de caractères
    Ce type est déprécié, utilisez TSysCharSet à la place.
  *}
  TSetOfChars = TSysCharSet {$IFNDEF DCTD} deprecated {$ENDIF};

  {*
    Ensemble de Byte
    Ce type est déprécié, utilisez TSysByteSet à la place.
  *}
  TSetOfBytes = TSysByteSet {$IFNDEF DCTD} deprecated {$ENDIF};

function Dir : string;

// Fonctions de If Immédiat
function IIF(Cond : boolean; Int1, Int2 : integer) : integer; overload;
function IIF(Cond : boolean; Flo1, Flo2 : Double ) : Double ; overload;
function IIF(Cond : boolean; Chr1, Chr2 : Char   ) : Char   ; overload;
function IIF(Cond : boolean; const Str1, Str2 : string) : string; overload;
function IIF(Cond : boolean; Obj1, Obj2 : TObject) : TObject; overload;
function IIF(Cond : boolean; Ptr1, Ptr2 : Pointer) : Pointer; overload;
function IIF(Cond : boolean; Var1, Var2 : Variant) : Variant; overload;

function Point3DToString(Point3D : T3DPoint;
  const Delim : string = ' ') : string;

function MinMax(Value, Min, Max : integer) : integer;

function IntDiv(Op1, Op2 : integer) : integer;
function IntMod(Op1, Op2 : integer) : integer;

{$IFDEF MSWINDOWS}
function ShowMes(const Title, Text : string;
  Flags : LongWord) : integer; platform;
{$ENDIF}

function ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;

procedure Wait(Milliseconds : integer); deprecated;
procedure WaitProcessMessages(Milliseconds : integer);

function IntToStr0(Value, Digits : integer) : string;

function ReadStrFromStream(Stream : TStream) : string;
procedure WriteStrToStream(Stream : TStream; const Str : string);

function CorrectFileName(const FileName : string;
  AcceptPathDelim : boolean = False; AcceptDriveDelim : boolean = False) : boolean;

function Point3D(X, Y, Z : integer) : T3DPoint;

function Same3DPoint(Point1, Point2 : T3DPoint) : boolean;

{$IFDEF MSWINDOWS}
procedure RunURL(const URL : string; const Verb : string = 'open');
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, ShellAPI,
{$ENDIF}
  DateUtils, Forms;

{$REGION 'Classe EAPIError'}

{------------------}
{ Classe EAPIError }
{------------------}

{*
  Crée une nouvelle instance de EAPIError
  @param Error   Code d'erreur HRESULT
*}
constructor EAPIError.Create(Error : integer);
begin
  // Le message est récupéré via SysErrorMessage
  inherited Create(SysErrorMessage(Error));
  FErrorCode := Error;
end;

{*
  Crée une nouvelle instance de EAPIError
  Le code d'erreur est récupéré avec la routien GetLastError
*}
constructor EAPIError.Create;
begin
  Create(GetLastError);
end;

{$ENDREGION}

{----------------------------------}
{ Procédures et fonctions globales }
{----------------------------------}

{*
  Renvoie le chemin du dossier dans lequel se trouve l'application qui s'exécute
  @return Le chemin du dossier dans lequel se trouve l'application qui s'exécute
*}
function Dir : string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

{$REGION 'Fonctions de If Immédiat'}

{*
  Fonction de If Immédiat pour les entiers
  @param Cond   Condition à vérifier
  @param Int1   Valeur à renvoyer si la condition est vraie
  @param Int2   Valeur à renvoyer si la condition est fausse
  @return Int1 si Cond vaut True, Int2 sinon
*}
function IIF(Cond : boolean; Int1, Int2 : integer) : integer;
begin
  if Cond then Result := Int1 else Result := Int2;
end;

{*
  Fonction de If Immédiat pour les décimaux
  @param Cond   Condition à vérifier
  @param Flo1   Valeur à renvoyer si la condition est vraie
  @param Flo2   Valeur à renvoyer si la condition est fausse
  @return Flo1 si Cond vaut True, Flo2 sinon
*}
function IIF(Cond : boolean; Flo1, Flo2 : Double) : Double;
begin
  if Cond then Result := Flo1 else Result := Flo2;
end;

{*
  Fonction de If Immédiat pour les caractères
  @param Cond   Condition à vérifier
  @param Chr1   Valeur à renvoyer si la condition est vraie
  @param Chr2   Valeur à renvoyer si la condition est fausse
  @return Chr1 si Cond vaut True, Chr2 sinon
*}
function IIF(Cond : boolean; Chr1, Chr2 : Char) : Char;
begin
  if Cond then Result := Chr1 else Result := Chr2;
end;

{*
  Fonction de If Immédiat pour les chaînes de caractères
  @param Cond   Condition à vérifier
  @param Str1   Valeur à renvoyer si la condition est vraie
  @param Str2   Valeur à renvoyer si la condition est fausse
  @return Str1 si Cond vaut True, Str2 sinon
*}
function IIF(Cond : boolean; const Str1, Str2 : string) : string;
begin
  if Cond then Result := Str1 else Result := Str2;
end;

{*
  Fonction de If Immédiat pour les objets
  @param Cond   Condition à vérifier
  @param Obj1   Valeur à renvoyer si la condition est vraie
  @param Obj2   Valeur à renvoyer si la condition est fausse
  @return Obj1 si Cond vaut True, Obj2 sinon
*}
function IIF(Cond : boolean; Obj1, Obj2 : TObject) : TObject;
begin
  if Cond then Result := Obj1 else Result := Obj2;
end;

{*
  Fonction de If Immédiat pour les pointeurs
  @param Cond   Condition à vérifier
  @param Ptr1   Valeur à renvoyer si la condition est vraie
  @param Ptr2   Valeur à renvoyer si la condition est fausse
  @return Ptr1 si Cond vaut True, Ptr2 sinon
*}
function IIF(Cond : boolean; Ptr1, Ptr2 : Pointer) : Pointer;
begin
  if Cond then Result := Ptr1 else Result := Ptr2;
end;

{*
  Fonction de If Immédiat pour les variants
  @param Cond   Condition à vérifier
  @param Var1   Valeur à renvoyer si la condition est vraie
  @param Var2   Valeur à renvoyer si la condition est fausse
  @return Var1 si Cond vaut True, Var2 sinon
*}
function IIF(Cond : boolean; Var1, Var2 : Variant) : Variant;
begin
  if Cond then Result := Var1 else Result := Var2;
end;

{$ENDREGION}

{*
  Convertit un point 3D en chaîne de caractères
  @param Point3D   Point 3D à convertir
  @param Delim     Délimiteur à placer entre les coordonnées
  @return Point3D convertit en chaîne de caractères
*}
function Point3DToString(Point3D : T3DPoint;
  const Delim : string = ' ') : string;
begin
  Result := IntToStr(Point3D.X) + Delim +
            IntToStr(Point3D.Y) + Delim +
            IntToStr(Point3D.Z);
end;

{*
  S'assure qu'une valeur est bien dans un intervalle spécifié
  @param Value   Valeur de base
  @param Min     Valeur minimale
  @param Max     Valeur maximale
  @return Valeur la plus proche de Value dans l'intervalle [Min;Max]
*}
function MinMax(Value, Min, Max : integer) : integer;
begin
  if Value > Max then Result := Max else
  if Value < Min then Result := Min else
  Result := Value;
end;

{*
  Division euclidienne
  @param Op1   Dividande
  @param Op2   Diviseur
  @return Résultat de la division euclidienne de Op1 par Op2
  @raise EDivByZero Division par 0
*}
function IntDiv(Op1, Op2 : integer) : integer;
begin
  Result := Floor(Op1 / Op2);
end;

{*
  Reste de la division euclidienne
  @param Op1   Dividande
  @param Op2   Diviseur
  @return Reste de la division euclidienne de Op1 par Op2
  @raise EDivByZero Division par 0
*}
function IntMod(Op1, Op2 : integer) : integer;
begin
  Result := Op1 - IntDiv(Op1, Op2) * Op2;
end;

{$REGION 'Fonctions de boîtes de dialogue'}

{$IFDEF MSWINDOWS}
{*
  Affiche une boîte de dialogue modale Windows
  @param Title   Titre de la boîte de dialogue
  @param Text    Texte de la boîte de dialogue
  @param Flags   Flags contrôlant le style de boîte de dialogue
  @return Code de résultat du bouton sur lequel a cliqué l'utilisateur
*}
function ShowMes(const Title, Text : string; Flags : LongWord) : integer;
var AText, ATitle : PChar;
    AFlags : LongWord;
begin
  AText  := PChar(Text);
  ATitle := PChar(Title);
  AFlags := Flags or MB_APPLMODAL; // Ajout du style Modal
  Result := MessageBox(Application.Handle, AText, ATitle, AFlags);
end;
{$ENDIF}

{*
  Affiche une boîte de dialogue modale Windows ou QT (selon la VCL utilisée)
  @param Title        Titre de la boîte de dialogue
  @param Text         Texte de la boîte de dialogue
  @param DlgType      Type de boîte de dialogue
  @param DlgButtons   Boutons à placer dans la boîte de dialogue
  @param DefButton    Numéro du bouton sélectionné par défaut (à partir de 1)
  @param AddFlags     Flags additionnels contrôlant de style de boîte (Win32)
  @return Bouton sur lequel a cliqué l'utilisateur
*}

{$IFDEF MSWINDOWS}
function ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
var TypeFlags, BtnsFlags, DefBtnFlags, Flags : LongWord;
begin
  // Transformation du paramètre DlgType en flag de type
  case DlgType of
    dtCustom       : TypeFlags := 0;
    dtInformation  : TypeFlags := MB_ICONINFORMATION;
    dtWarning      : TypeFlags := MB_ICONEXCLAMATION;
    dtError        : TypeFlags := MB_ICONERROR;
    dtConfirmation : TypeFlags := MB_ICONQUESTION;
    else TypeFlags := 0;
  end;

  // Transformation du paramètre DlgButtons en flag de boutons
  case DlgButtons of
    dbOK               : BtnsFlags := MB_OK;
    dbOKCancel         : BtnsFlags := MB_OKCANCEL;
    dbYesNo            : BtnsFlags := MB_YESNO;
    dbYesNoCancel      : BtnsFlags := MB_YESNOCANCEL;
    dbRetryCancel      : BtnsFlags := MB_RETRYCANCEL;
    dbAbortRetryIgnore : BtnsFlags := MB_AbortRetryIgnore;
    else BtnsFlags := MB_OK;
  end;

  // Transformation du paramètre DefButton en flag de bouton par défaut
  case DefButton of
    1 : DefBtnFlags := MB_DEFBUTTON1;
    2 : DefBtnFlags := MB_DEFBUTTON2;
    3 : DefBtnFlags := MB_DEFBUTTON3;
    else DefBtnFlags := 0;
  end;

  // Appel de ShowMes et transformation du retour Word en TDialogResult
  Flags := TypeFlags or BtnsFlags or DefBtnFlags or AddFlags;
  case ShowMes(Title, Text, Flags) of
    IDOK     : Result := drOK;
    IDYES    : Result := drYes;
    IDNO     : Result := drNo;
    IDCANCEL : Result := drCancel;
    IDABORT  : Result := drAbort;
    IDRETRY  : Result := drRetry;
    IDIGNORE : Result := drIgnore;
    else Result := drCancel;
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
function ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
var NbBtns : integer;
    MsgDefButton : TMsgDlgBtn;
begin
  // Détermination du nombre de boutons
  case DlgButtons of
    dbOK               : NbBtns := 1;
    dbOKCancel         : NbBtns := 2;
    dbYesNo            : NbBtns := 2;
    dbYesNoCancel      : NbBtns := 3;
    dbRetryCancel      : NbBtns := 2;
    dbAbortRetryIgnore : NbBtns := 3;
    else begin NbBtns := 1 end;
  end;

  MsgDefButton := mbNone;
  if (DefButton < 1) or (DefButton > NbBtns) then DefButton := 1;

  // Détermination du bouton par défaut
  case DefButton of
    1 :
    begin
      if DlgButtons = [dbOK, dbOKCancel] then
        MsgDefButton := mbOK else
      if DlgButtons = [dbYesNo, dbYesNoCancel] then
        MsgDefButton := mbYes else
      if DlgButtons = dbRetryCancel then
        MsgDefButton := mbRetry else
      if DlgButtons = dbAbortRetryIgnore then
        MsgDefButton := mbAbort;
    end;
    2 :
    begin
      if DlgButtons = [dbOKCancel, dbRetryCancel] then
        MsgDefButton := mbCancel else
      if DlgButtons = [dbYesNo, dbYesNoCancel]
        then MsgDefButton := mbNo else
      if DlgButtons = dbAbortRetryIgnore then
        MsgDefButton := mbRetry;
    end;
    3 :
    begin
      if DlgButtons = dbYesNoCancel then
        MsgDefButton := mbCancel else
      if DlgButtons = dbAbortRetryIgnore then
        MsgDefButton := mbIgnore;
    end;
  end;

  // Appel de MessageDlg et renvoi de la valeur renvoyée par celle-ci
  Result := MessageDlg(Title, Text, DlgType, DlgButtons, 0, MsgDefBtn);
end;
{$ENDIF}

{$ENDREGION}

{*
  Met en pause l'exécution pendant un temps défini
  Cette routine est dépréciée, utilisez Sleep à la place.
  @param Milliseconds   Nombre de milisecondes pendant lesquelles pauser
*}
procedure Wait(Milliseconds : integer);
begin
  Sleep(Milliseconds);
end;

{*
  Met en pause l'exécution pendant un temps défini
  Pendant cette pause, les messages Windows de l'applications sont tout de
  même traités.
  @param Milliseconds   Nombre de milisecondes pendant lesquelles pauser
*}
procedure WaitProcessMessages(Milliseconds : integer);
var BeginTime : TDateTime;
begin
  BeginTime := Now;
  while MilliSecondsBetween(Now, BeginTime) < Milliseconds do
    Application.ProcessMessages;
end;

{*
  Convertit un entier en chaîne, avec un nombre minimal de caractères spécifié
  Exemples : IntToStr(345, 4) = '0345' ; IntToStr0(1000, 3) = '1000'
  @param Value    Entier à convertir
  @param Digits   Nombre minimal de caractères de la chaîne convertie
  @return La représentation en chaîne de Value, avec Digits caractères minimum
*}
function IntToStr0(Value, Digits : integer) : string;
begin
  Result := Format('%.*d', [Digits, Value]);
end;

{*
  Lit une chaîne de caractères depuis un flux
  Cette chaîne doit avoir été écrite avec WriteStrToStream.
  @param Stream   Flux depuis lequel lire la chaîne
  @return La chaîne lue
*}
function ReadStrFromStream(Stream : TStream) : string;
var Len : integer;
begin
  Stream.ReadBuffer(Len, 4);
  SetLength(Result, Len);
  Stream.ReadBuffer(Result[1], Len);
end;

{*
  Écrit une chaîne de caractères dans un flux
  Cette chaine pourra être relue avec ReadStrFromStream.
  @param Stream   Flux dans lequel enregistrer la chaîne
  @param Str      Chaîne de caractères à écrire
*}
procedure WriteStrToStream(Stream : TStream; const Str : string);
var Len : integer;
begin
  Len := Length(Str);
  Stream.WriteBuffer(Len, 4);
  Stream.WriteBuffer(Str[1], Len);
end;

{*
  Teste si une chaîne de caractères est un nom de fichier correct
  Ce test est effectué conformément aux règles de nommage des fichiers du
  système d'exploitation.
  @param FileName           Chaîne à tester
  @param AcceptPathDelim    Indique si le séparateur de chemin est accepté
  @param AcceptDriveDelim   Indique si le séparateur de disque est accepté
  @return True si FileName est un nom de fichier correct, False sinon
*}
function CorrectFileName(const FileName : string;
  AcceptPathDelim : boolean = False;
  AcceptDriveDelim : boolean = False) : boolean;
var I : integer;
    BadChars : set of Char;
begin
  BadChars := ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
  Result := False;
  if FileName = '' then exit;

  // Si le délimiteur de chemin est accepté, on l'exclut de BadChars
  if AcceptPathDelim then
    Exclude(BadChars, PathDelim);

  // Si le délimiteur de disque est accepté, on l'exclut de BadChars
  if AcceptDriveDelim then
    Exclude(BadChars, DriveDelim);

  // On teste tous les caractères de FileName
  for I := 1 to Length(FileName) do if FileName[I] in BadChars then exit;
  Result := True;
end;

{*
  Crée un point 3D
  @param X   Coordonnée X du point
  @param Y   Coordonnée Y du point
  @param Z   Coordonnée Z du point
  @return Le point 3D (X, Y, Z)
*}
function Point3D(X, Y, Z : integer) : T3DPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

{*
  Compare deux points 3D
  @param Point1   Premier point
  @param Point2   Second point
  @return True si Point1 et Point2 sont identiques, False sinon
*}
function Same3DPoint(Point1, Point2 : T3DPoint) : boolean;
begin
  Result := (Point1.X = Point2.X) and
    (Point1.Y = Point2.Y) and (Point1.Z = Point2.Z);
end;

{$IFDEF MSWINDOWS}
{*
  Lance une URL
  @param URL    URL à lancer
  @param Verb   Verbe à utiliser pour lancer l'URL
*}
procedure RunURL(const URL : string; const Verb : string = 'open');
begin
  ShellExecute(GetDesktopWindow(), PChar(Verb), PChar(URL),
    nil, nil, SW_SHOWNORMAL);
end;
{$ENDIF}

end.

