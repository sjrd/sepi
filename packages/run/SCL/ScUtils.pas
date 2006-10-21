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

  EAPIError = class(Exception)
  private
    FErrorCode : integer;
  public
    constructor Create(Error : integer); overload;
    constructor Create; overload;
    property ErrorCode : integer read FErrorCode;
  end platform;

{$ENDREGION}

  T3DPoint = record
    X, Y, Z : integer;
  end;

  // Dans le cas de Windows, les types de paramètres envoyés à ShowDialog sont
  // de nouveaux types
  {$IFDEF MSWINDOWS}
    TDialogType = (dtCustom, dtInformation, dtWarning, dtError, dtConfirmation);
    TDialogButtons = (dbOK, dbOKCancel, dbYesNo, dbYesNoCancel, dbRetryCancel,
                      dbAbortRetryIgnore);
    TDialogResult = (drOK, drYes, drNo, drCancel, drAbort, drRetry, drIgnore);
  {$ENDIF}

  // Dans le cas de Linux, ce sont des types équivalents aux types respectifs de
  // MessageDlg
  {$IFDEF LINUX}
    TDialogType = type TMsgDlgType;
    TDialogButtons = type TMsgDlgButtons;
    TDialogResult = type Word;
  {$ENDIF}

  TSysByteSet = set of Byte;
  TSetOfChars = TSysCharSet deprecated; // use TSysCharSet instead
  TSetOfBytes = TSysByteSet deprecated; // use TSysByteSet instead

function Dir : string;
// Renvoie le dossier dans lequel se trouve le programme
// (avec le \ ou / final)

// Fonctions de If immédiat
function IIF(Cond : boolean; Int1, Int2 : integer) : integer; overload;
function IIF(Cond : boolean; Flo1, Flo2 : Double ) : Double ; overload;
function IIF(Cond : boolean; Chr1, Chr2 : Char   ) : Char   ; overload;
function IIF(Cond : boolean; const Str1, Str2 : string) : string; overload;
function IIF(Cond : boolean; Obj1, Obj2 : TObject) : TObject; overload;
function IIF(Cond : boolean; Ptr1, Ptr2 : Pointer) : Pointer; overload;
function IIF(Cond : boolean; Var1, Var2 : Variant) : Variant; overload;

function Point3DToString(Point3D : T3DPoint;
  const Delim : string = ' ') : string;
// Renvoie la chaîne X+Delim+Y+Delim+Z

function MinMax(Value, Min, Max : integer) : integer;
// Renvoie la valeur la plus proche de Value dans l'intervalle [Min;Max]

function IntDiv(Op1, Op2 : integer) : integer;
// Renvoie la division entière de Op1 par Op2 (correct en négatif)

function IntMod(Op1, Op2 : integer) : integer;
// Renvoie le rest de la division entière de Op1 par Op2 (correct en négatif)

{$IFDEF MSWINDOWS}
function ShowMes(const Title, Text : string;
  Flags : LongWord) : integer; platform;
// Affiche un dialogue Windows via Windows.MessageBox
{$ENDIF}

function ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
// Dans le cas de Windows, améliore ShowMes pour accepter des paramètres comme
// ceux de MessageDlg ;
// Dans le cas de Linux, identique à MessageDlg mais avec moins de possibilités,
// plus lent, etc.

// L'avantage d'utiliser ShowMes et ShowDialog est que les boîtes de dialogue
// sont celles de Windows et émettent donc le son correspondant. De plus, cela
// permet, même en VCL, de choisir le titre de la boîte de dialogue.
// L'inconvénient est que toutes les combinaisons de boutons ne sont pas
// acceptées, seules celles de Windows.MessageBox le sont (voir l'aide en ligne
// pour savoir quelles sont-elles).
// Si votre projet ne doit tourner que sous Linux ou que le fait d'utiliser les
// boîtes de dialogue Windows vous importe peu, vous aurez tout intérêt à
// utiliser MessageDlg.

procedure Wait(Milliseconds : integer); deprecated; // use Sleep instead
// Boucle pendant Miliseconds milisecondes
procedure WaitProcessMessages(Milliseconds : integer);
// Boucle pendant Miliseconds milisecondes tout en effectuant des Application.ProcessMessages

function IntToStr0(Value, Digits : integer) : string;
// Semblable à IntToStr mais place des 0 devant pour atteindre NbreChiffres chiffres
// Attention ! IntToStr0(1000, 3) = '1000'

function ReadStrFromStream(Stream : TStream) : string;
// Lit une chaîne de caractères depuis Stream à la position courante

procedure WriteStrToStream(Stream : TStream; const Str : string);
// Ecrit une chaîne de caractères dans Stream à la position courante

function CorrectFileName(const FileName : string;
  AcceptPathDelim : boolean = False; AcceptDriveDelim : boolean = False) : boolean;
// Vérifie si FileName est un nom de fichier correct
// AcceptPathDelim indique si les \ (Windows) ou / (Linux) sont autorisés
// AcceptDriveDelim indique si les : sont acceptés (Windows seulement)

function Point3D(X, Y, Z : integer) : T3DPoint;
// Renvoie un T3DPoint avec les coordonnées X, Y et Z

function Same3DPoint(Point1, Point2 : T3DPoint) : boolean;
// Renvoie True si Point1 et Point2 sont identiques

{$IFDEF MSWINDOWS}
procedure RunURL(const URL : string; const Verb : string = 'open');
// Exécute URL avec pour verbe Verb
// Une URL peut être
// -> un nom de fichier : le verbe Verb est exécuté pour ce fichier (un fichier .txt sera ouvert dans le bloc-notes)
// -> un nom de programme : le programme est exécuté
// -> un adresse Internet : votre navigateur Web s'ouvre sur cette adresse
// -> 'mailto:' + une adresse E-mail : votre programme de messagerie est ouvert à l'adresse indiquée
// -> ...
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, ShellAPI,
{$ENDIF}
  DateUtils, Forms;

////////////////////////////////////////
/// Procédures et fonctions globales ///
////////////////////////////////////////

function Dir : string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

{$REGION 'Fonctions de If Immédiat'}

function IIF(Cond : boolean; Int1, Int2 : integer) : integer;
begin
  if Cond then Result := Int1 else Result := Int2;
end;

function IIF(Cond : boolean; Flo1, Flo2 : Double) : Double;
begin
  if Cond then Result := Flo1 else Result := Flo2;
end;

function IIF(Cond : boolean; Chr1, Chr2 : Char) : Char;
begin
  if Cond then Result := Chr1 else Result := Chr2;
end;

function IIF(Cond : boolean; const Str1, Str2 : string) : string;
begin
  if Cond then Result := Str1 else Result := Str2;
end;

function IIF(Cond : boolean; Obj1, Obj2 : TObject) : TObject;
begin
  if Cond then Result := Obj1 else Result := Obj2;
end;

function IIF(Cond : boolean; Ptr1, Ptr2 : Pointer) : Pointer;
begin
  if Cond then Result := Ptr1 else Result := Ptr2;
end;

function IIF(Cond : boolean; Var1, Var2 : Variant) : Variant;
begin
  if Cond then Result := Var1 else Result := Var2;
end;

{$ENDREGION}

function Point3DToString(Point3D : T3DPoint;
  const Delim : string = ' ') : string;
begin
  Result := IntToStr(Point3D.X) + Delim +
            IntToStr(Point3D.Y) + Delim +
            IntToStr(Point3D.Z);
end;

function MinMax(Value, Min, Max : integer) : integer;
begin
  if Value > Max then Result := Max else
  if Value < Min then Result := Min else
  Result := Value;
end;

function IntDiv(Op1, Op2 : integer) : integer;
begin
  Result := Floor(Op1 / Op2);
end;

function IntMod(Op1, Op2 : integer) : integer;
begin
  Result := Op1 - IntDiv(Op1, Op2) * Op2;
end;

{$REGION 'Fonction de boîtes de dialogue'}

{$IFDEF MSWINDOWS}
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

{$IFDEF MSWINDOWS}
function ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
var TypeFlags, BtnsFlags, DefBtnFlags : LongWord;
begin
  case DlgType of // Transformation du paramètre DlgType en flag de type
    dtCustom       : TypeFlags := 0;
    dtInformation  : TypeFlags := MB_ICONINFORMATION;
    dtWarning      : TypeFlags := MB_ICONEXCLAMATION;
    dtError        : TypeFlags := MB_ICONERROR;
    dtConfirmation : TypeFlags := MB_ICONQUESTION;
    else TypeFlags := 0;
  end;
  case DlgButtons of // Transformation du paramètre DlgButtons en flag de boutons
    dbOK               : BtnsFlags := MB_OK;
    dbOKCancel         : BtnsFlags := MB_OKCANCEL;
    dbYesNo            : BtnsFlags := MB_YESNO;
    dbYesNoCancel      : BtnsFlags := MB_YESNOCANCEL;
    dbRetryCancel      : BtnsFlags := MB_RETRYCANCEL;
    dbAbortRetryIgnore : BtnsFlags := MB_AbortRetryIgnore;
    else BtnsFlags := MB_OK;
  end;
  case DefButton of // Transformation du paramètre DefButton en flag de bouton par défaut
    1 : DefBtnFlags := MB_DEFBUTTON1;
    2 : DefBtnFlags := MB_DEFBUTTON2;
    3 : DefBtnFlags := MB_DEFBUTTON3;
    else DefBtnFlags := 0;
  end;
  // Appel de ShowMes et transformation du retour Word en TDialogResult
  case ShowMes(Title, Text, TypeFlags or BtnsFlags or DefBtnFlags or AddFlags) of
    IDOK     : Result := drOK    ;
    IDYES    : Result := drYes   ;
    IDNO     : Result := drNo    ;
    IDCANCEL : Result := drCancel;
    IDABORT  : Result := drAbort ;
    IDRETRY  : Result := drRetry ;
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
  case DlgButtons of // Calcul du nombre de boutons
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
  case DefButton of // On cherche quel est le bouton par défaut
    1 : if DlgType in [dbOK, dbOKCancel]          then MsgDefButton := mbOK     else
        if DlgType in [dbYesNo, dbYesNoCancel]    then MsgDefButton := mbYes    else
        if DlgType = dbRetryCancel                then MsgDefButton := mbRetry  else
        if DlgType = dbAbortRetryIgnore           then MsgDefButton := mbAbort;
    2 : if DlgType in [dbOKCancel, dbRetryCancel] then MsgDefButton := mbCancel else
        if DlgType in [dbYesNo, dbYesNoCancel]    then MsgDefButton := mbNo     else
        if DlgType = dbAbortRetryIgnore           then MsgDefButton := mbRetry;
    3 : if DlgType = dbYesNoCancel                then MsgDefButton := mbCancel else
        if DlgType = dbAbortRetryIgnore           then MsgDefButton := mbIgnore;
  end;
  // Appel de MessageDlg et renvoi de la valeur renvoyée par celle-ci
  Result := MessageDlg(Title, Text, DlgType, DlgButtons, 0, MsgDefBtn);
end;
{$ENDIF}

{$ENDREGION}

procedure Wait(Milliseconds : integer);
begin
  Sleep(Milliseconds);
end;

procedure WaitProcessMessages(Milliseconds : integer);
var BeginTime : TDateTime;
begin
  BeginTime := Now;
  while MilliSecondsBetween(Now, BeginTime) < Milliseconds do
    Application.ProcessMessages;
end;

function IntToStr0(Value, Digits : integer) : string;
begin
  Result := Format('%.*d', [Digits, Value]);
end;

function ReadStrFromStream(Stream : TStream) : string;
var Len : integer;
begin
  Stream.ReadBuffer(Len, 4);
  SetLength(Result, Len);
  Stream.ReadBuffer(Result[1], Len);
end;

procedure WriteStrToStream(Stream : TStream; const Str : string);
var Len : integer;
begin
  Len := Length(Str);
  Stream.WriteBuffer(Len, 4);
  Stream.WriteBuffer(Str[1], Len);
end;

function CorrectFileName(const FileName : string;
  AcceptPathDelim : boolean = False; AcceptDriveDelim : boolean = False) : boolean;
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
  for I := 1 to Length(FileName) do if FileName[I] in BadChars then exit;
  Result := True;
end;

function Point3D(X, Y, Z : integer) : T3DPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Same3DPoint(Point1, Point2 : T3DPoint) : boolean; register;
begin
  Result := (Point1.X = Point2.X) and (Point1.Y = Point2.Y) and (Point1.Z = Point2.Z);
end;

{$IFDEF MSWINDOWS}
procedure RunURL(const URL : string; const Verb : string = 'open');
begin
  ShellExecute(GetDesktopWindow(), PChar(Verb), PChar(URL), nil, nil, SW_SHOWNORMAL);
end;
{$ENDIF}

{$REGION 'Classe EAPIError'}

////////////////////////
/// Classe EAPIError ///
////////////////////////

constructor EAPIError.Create(Error : integer);
begin
  // Le message est récupéré via SysErrorMessage
  inherited Create(SysErrorMessage(Error));
  FErrorCode := Error;
end;

constructor EAPIError.Create;
begin
  // Le code erreur est récupéré via GetLastError
  Create(GetLastError);
end;

{$ENDREGION}

end.

