{*
  Définit des routines et composants utilisant des boîtes de dialogue
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SdDialogs;

interface

uses
  Classes, Dialogs, Controls, Graphics;

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
    Gère une boîte de dialogue d'introduction de mot de passe
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSdPasswordDialog = class(TComponent)
  private
    FPassword : string;      /// Mot de passe correct
    FShowErrorMes : boolean; /// Indique s'il faut notifier sur erreur
  public
    constructor Create(AOwner : TComponent); override;

    function Execute : boolean; overload;
    function Execute(Password : string;
      ShowErrorMes : boolean = True) : boolean; overload;
    function Execute(ShowErrorMes : boolean) : boolean; overload;
  published
    property Password : string read FPassword write FPassword;
    property ShowErrorMes : boolean read FShowErrorMes write FShowErrorMes
      default True;
  end;

  {*
    Gère une boîte de dialogue À propos
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSdAboutDialog = class(TComponent)
  private
    FTitle : string;          /// Titre de la boîte de dialogue
    FProgramIcon : TIcon;     /// Icône du programme
    FProgramName : string;    /// Nom du programme
    FVersion : string;        /// Intitulé de version
    FProgramVersion : string; /// Version du programme
    FAuthor : string;         /// Intitulé d'auteur
    FAuthorName : string;     /// Nom de l'auteur du programme
    FAuthorEMail : string;    /// Adresse e-mail de l'auteur (optionnel)
    FWebSite : string;        /// Site Web du programme (optionnel)

    procedure SetProgramIcon(New : TIcon);

    function IsTitleStored : boolean;
    function IsVersionStored : boolean;
    function IsProgramVersionStored : boolean;
    function IsAuthorStored : boolean;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Execute;
  published
    property Title : string read FTitle write FTitle stored IsTitleStored;
    property ProgramIcon : TIcon read FProgramIcon write SetProgramIcon;
    property ProgramName : string read FProgramName write FProgramName;
    property Version : string read FVersion write FVersion
      stored IsVersionStored;
    property ProgramVersion : string read FProgramVersion write FProgramVersion
      stored IsProgramVersionStored;
    property Author : string read FAuthor write FAuthor stored IsAuthorStored;
    property AuthorName : string read FAuthorName write FAuthorName;
    property AuthorEMail : string read FAuthorEMail write FAuthorEMail;
    property WebSite : string read FWebSite write FWebSite;
  end platform;

  {*
    Gère une boîte de dialogue demandant un nombre à l'utilisateur
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSdNumberDialog = class(TComponent)
  private
    FTitle : string;  /// Titre de la boîte de dialogue
    FPrompt : string; /// Invite de la boîte de dialogue
    FValue : integer; /// Valeur par défaut, puis celle saisie par l'utilisateur
    FMin : integer;   /// Valeur minimale que peut choisir l'utilisateur
    FMax : integer;   /// Valeur maximale que peut choisir l'utilisateur
  public
    constructor Create(AOwner : TComponent); override;

    function Execute(const ATitle, APrompt : string;
      ADefault, AMin, AMax : integer) : integer; overload;
    function Execute(ADefault, AMin, AMax : integer) : integer; overload;
    function Execute(const ATitle, APrompt : string;
      AMin, AMax : integer) : integer; overload;
    function Execute(AMin, AMax : integer) : integer; overload;
    function Execute : integer; overload;
  published
    property Title : string read FTitle write FTitle;
    property Prompt : string read FPrompt write FPrompt;
    property Value : integer read FValue write FValue;
    property Min : integer read FMin write FMin;
    property Max : integer read FMax write FMax;
  end;

{$IFDEF MSWINDOWS}
function ShowMes(const Title, Text : string;
  Flags : LongWord) : integer; platform;
{$ENDIF}

function ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;

function ShowDialogRadio(const Title, Text : string; DlgType : TMsgDlgType;
  DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
  const RadioTitles : array of string; var Selected : integer;
  OverButtons : boolean = False) : Word;

function QueryPassword : string; overload;

function QueryPassWord(Password : string;
  ShowErrorMes : boolean = True) : boolean; overload;

procedure ShowAbout(Title : string; ProgramIcon : TIcon; ProgramName : string;
  ProgramVersion : string; Author : string; AuthorEMail : string = '';
  WebSite : string = ''); platform;

function QueryNumber(const Title, Prompt : string;
  Default, Min, Max : integer) : integer;

implementation

uses
  Windows, Forms, StdCtrls, Math, SdPassword, SdAbout, SdNumber, ScConsts;

{-------------------}
{ Routines globales }
{-------------------}

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

{*
  Affiche une boîte de dialogue avec des boutons radio
  ShowDialogRadio est une variante de ShowDialog qui affiche des boutons radio
  pour chaque choix possible.
  @param Title         Titre de la boîte de dialogue
  @param Text          Texte de la boîte de dialogue
  @param DlgType       Type de boîte de dialogue
  @param DlgButtons    Boutons présents dans la boîte de dialogue
  @param DefButton     Bouton sélectionné par défaut
  @param RadioTitles   Libellés des différents boutons radio
  @param Selected      Bouton radio sélectionné
  @param OverButtons   Boutons radio placés au-dessus des boutons si True
  @return Bouton sur lequel a cliqué l'utilisateur
*}
function ShowDialogRadio(const Title, Text : string; DlgType : TMsgDlgType;
  DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
  const RadioTitles : array of string; var Selected : integer;
  OverButtons : boolean = False) : Word;
var Form : TForm;
    I, MaxWidth, OldWidth : integer;
    Button : TButton;
begin
  // Création de la boîte de dialogue
  Form := CreateMessageDialog(Text, DlgType, DlgButtons);

  with Form do
  try
    Caption := Title;
    // On augmente la taille de la boîte de dialogue
    Height := Height + Length(RadioTitles) * 25;

    // Création des boutons radio et détermination de la largeur minimale
    MaxWidth := 0;
    for I := High(RadioTitles) downto Low(RadioTitles) do
    with TRadioButton.Create(Form) do
    begin
      FreeNotification(Form);
      Parent := Form;
      Width := Canvas.TextWidth(RadioTitles[I]) + 20;
      MaxWidth := Max(MaxWidth, Width-20);
      Caption := RadioTitles[I];
      Checked := I = Selected;
      Tag := I;
      Left := 8;

      // OverButtons indique si les RadioBox sont au-dessus ou en-dessous des
      // boutons
      if OverButtons then
        Top := Form.Height - 90 - (High(RadioTitles) - I) * 25
      else
        Top := Form.Height - 50 - (High(RadioTitles) - I) * 25;
    end;

    // Il faut aussi vérifier que la fiche peut afficher les textes des RadioBox
    // en entier
    OldWidth := 0;
    if (MaxWidth + 40) > Width then
    begin
      OldWidth := Width;
      Width := MaxWidth +40;
    end;

    for I := 0 to ComponentCount-1 do
    begin
      // On récupère chaque bouton
      if Components[I] is TButton then
      begin
        Button := TButton(Components[I]);

        // On met le bon bouton par défaut et on le sélectionne
        Button.Default := Button.ModalResult = DefButton;
        if Button.Default then ActiveControl := Button;

        // S'il le faut, décaler tous les boutons vers le bas
        if OverButtons then
          Button.Top := Button.Top + Length(RadioTitles) * 25;

        // S'il le faut, décaler tous les boutons vers la droite
        if OldWidth > 0 then
          Button.Left := Button.Left + (Width - OldWidth) div 2;
      end;
    end;

    // On centre la boîte de dialogue
    Position := poScreenCenter;

    // Affichage de la boîte de dialogue
    Result := ShowModal;

    // Récupération du choix de l'utilisateur
    Selected := -1;
    for I := 0 to ControlCount-1 do
    begin
      if (Controls[I] is TRadioButton) and
         TRadioButton(Controls[I]).Checked then
        Selected := Controls[I].Tag;
    end;
  finally
    Free;
  end;
end;

{*
  Demande un mot de passe à l'utilisateur
  @return Le mot de passe qu'a saisi l'utilisateur
*}
function QueryPassword : string;
begin
  Result := TSdPasswordForm.QueryPassword;
end;

{*
  Demande un mot de passe à l'utilisateur
  @param Password       Mot de passe correct
  @param ShowErrorMes   Indique s'il faut notifier sur erreur
  @return True si l'utilisateur a saisi le bon mot de passe, False sinon
*}
function QueryPassWord(Password : string;
  ShowErrorMes : boolean = True) : boolean;
begin
  Result := TSdPasswordForm.QueryPassword(Password, ShowErrorMes);
end;

{*
  Affiche une boîte de dialogue À propos
  @param Title            Titre de la boîte de dialogue
  @param ProgramIcon      Icône du programme
  @param ProgramName      Nom du programme
  @param ProgramVersion   Version du programme
  @param Author           Auteur du programme
  @param AuthorEMail      Adresse e-mail de l'auteur (optionnel)
  @param WebSite          Site Web du programme (optionnel)
*}
procedure ShowAbout(Title : string; ProgramIcon : TIcon; ProgramName : string;
  ProgramVersion : string; Author : string; AuthorEMail : string = '';
  WebSite : string = '');
begin
  TSdAboutForm.ShowAbout(Title, ProgramIcon, ProgramName, ProgramVersion,
    Author, AuthorEMail, WebSite);
end;

{*
  Demande un nombre à l'utilisateur
  @param Title     Titre de la boîte de dialogue
  @param Prompt    Invite de la boîte de dialogue
  @param Default   Valeur par défaut
  @param Min       Valeur minimum que peut choisir l'utilisateur
  @param Max       Valeur maximum que peut choisir l'utilisateur
  @return Nombre qu'a choisi l'utilisateur
*}
function QueryNumber(const Title, Prompt : string;
  Default, Min, Max : integer) : integer;
begin
  Result := TSdNumberForm.QueryNumber(Title, Prompt, Default, Min, Max);
end;

{--------------------------}
{ Classe TSdPasswordDialog }
{--------------------------}

{*
  Crée une instance de TSdPasswordDialog
  @param AOwner   Propriétaire
*}
constructor TSdPasswordDialog.Create(AOwner : TComponent);
begin
  inherited;
  FPassword := '';
  FShowErrorMes := True;
end;

{*
  Demande un mot de passe à l'utilisateur
  @return True si l'utilisateur a saisi le bon mot de passe, False sinon
*}
function TSdPasswordDialog.Execute : boolean;
begin
  Result := Execute(Password, ShowErrorMes);
end;

{*
  Demande un mot de passe à l'utilisateur
  @param Password       Mot de passe correct
  @param ShowErrorMes   Indique s'il faut notifier sur erreur
  @return True si l'utilisateur a saisi le bon mot de passe, False sinon
*}
function TSdPasswordDialog.Execute(Password : string;
  ShowErrorMes : boolean = True) : boolean;
begin
  Result := TSdPassWordForm.QueryPassword(Password, ShowErrorMes);
end;

{*
  Demande un mot de passe à l'utilisateur
  @param ShowErrorMes   Indique s'il faut notifier sur erreur
  @return True si l'utilisateur a saisi le bon mot de passe, False sinon
*}
function TSdPasswordDialog.Execute(ShowErrorMes : boolean) : boolean;
begin
  Result := Execute(Password, ShowErrorMes);
end;

{-----------------------}
{ Classe TSdAboutDialog }
{-----------------------}

{*
  Crée une instance de TSdAboutDialog
  @param AOwner   Propriétaire
*}
constructor TSdAboutDialog.Create(AOwner : TComponent);
begin
  inherited;
  FTitle := sScAbout;
  FProgramIcon := TIcon.Create;
  FProgramName := '';
  FVersion := sScVersion+sScColon;
  FProgramVersion := '1.0'; {don't localize}
  FAuthor := sScAuthor+sScColon;
  FAuthorName := '';
  FAuthorEMail := '';
  FWebSite := '';
end;

{*
  Détruit l'instance
*}
destructor TSdAboutDialog.Destroy;
begin
  FProgramIcon.Free;
  inherited;
end;

{*
  Modifie l'icône du programme
  @param New   Nouvelle icône
*}
procedure TSdAboutDialog.SetProgramIcon(New : TIcon);
begin
  FProgramIcon.Assign(New);
end;

{*
  Indique si le titre doit être stocké dans un flux dfm
  @return True si le titre est différent de celui par défaut, False sinon
*}
function TSdAboutDialog.IsTitleStored : boolean;
begin
  Result := FTitle <> sScAbout;
end;

{*
  Indique si l'intitulé de version doit être stocké dans un flux dfm
  @return True si l'intitulé est différent de celui par défaut, False sinon
*}
function TSdAboutDialog.IsVersionStored : boolean;
begin
  Result := FVersion <> sScVersion+sScColon;
end;

{*
  Indique si la version du programme doit être stockée dans un flux dfm
  @return True si la version est '1.0', False sinon
*}
function TSdAboutDialog.IsProgramVersionStored : boolean;
begin
  Result := FProgramVersion <> '1.0'; {don't localize}
end;

{*
  Indique si l'intitulé d'auteur doit être stocké dans un flux dfm
  @return True si l'intitulé est différent de celui par défaut, False sinon
*}
function TSdAboutDialog.IsAuthorStored : boolean;
begin
  Result := FAuthor <> sScAuthor+sScColon;
end;

{*
  Affiche la boîte de dialogue
*}
procedure TSdAboutDialog.Execute;
begin
  TSdAboutForm.ShowAbout(Title, ProgramIcon, ProgramName,
    Version+' '+ProgramVersion, Author+' '+AuthorName, AuthorEMail, WebSite);
end;

{------------------------}
{ Classe TSdNumberDialog }
{------------------------}

{*
  Crée une instance de TSdNumberDialog
  @param AOwner   Propriétaire
*}
constructor TSdNumberDialog.Create(AOwner : TComponent);
begin
  inherited;

  FTitle := '';
  FPrompt := '';
  FValue := 0;
  FMin := 0;
  FMax := 0;
end;

function TSdNumberDialog.Execute(const ATitle, APrompt : string;
  ADefault, AMin, AMax : integer) : integer;
begin
  FValue := QueryNumber(ATitle, APrompt, ADefault, AMin, AMax);
  Result := FValue;
end;

function TSdNumberDialog.Execute(
  ADefault, AMin, AMax : integer) : integer;
begin
  Result := Execute(Title, Prompt, ADefault, AMin, AMax);
end;

function TSdNumberDialog.Execute(const ATitle, APrompt : string;
  AMin, AMax : integer) : integer;
begin
  Result := Execute(Title, Prompt, Value, AMin, AMax);
end;

function TSdNumberDialog.Execute(AMin, AMax : integer) : integer;
begin
  Result := Execute(Title, Prompt, Value, AMin, AMax);
end;

function TSdNumberDialog.Execute : integer;
begin
  Result := Execute(Title, Prompt, Value, Min, Max);
end;

end.

