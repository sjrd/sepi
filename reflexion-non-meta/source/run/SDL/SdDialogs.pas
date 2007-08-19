{*
  D�finit des routines et composants utilisant des bo�tes de dialogue
  @author sjrd
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
    Type d'une bo�te de dialogue
    dtCustom       : Type g�n�rique
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
    Boutons pr�sents dans une bo�te de dialogue
    dbOK               : OK
    dbOKCancel         : OK et Annuler
    dbYesNo            : Oui et Non
    dbYesNoCancel      : Oui, Non et Annuler
    dbRetryCancel      : R�essayer et Annuler
    dbAbortRetryIgnore : Abandonner, R�essayer et Ignorer
  *}
  {$IFDEF MSWINDOWS}
    TDialogButtons = (dbOK, dbOKCancel, dbYesNo, dbYesNoCancel, dbRetryCancel,
                      dbAbortRetryIgnore);
  {$ELSE}
    TDialogButtons = type TMsgDlgButtons;
  {$ENDIF}

  {*
    R�sultat de l'affichage d'une bo�te de dialogue
    drOK     : OK
    drYes    : Oui
    drNo     : Non
    drCancel : Annuler
    drAbort  : Abandonner
    drRetry  : R�essayer
    drIgnore : Ignorer
  *}
  {$IFDEF MSWINDOWS}
    TDialogResult = (drOK, drYes, drNo, drCancel, drAbort, drRetry, drIgnore);
  {$ELSE}
    TDialogResult = type Word;
  {$ENDIF}

  {*
    G�re une bo�te de dialogue d'introduction de mot de passe
    @author sjrd
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
    G�re une bo�te de dialogue � propos
    @author sjrd
    @version 1.0
  *}
  TSdAboutDialog = class(TComponent)
  private
    FTitle : string;          /// Titre de la bo�te de dialogue
    FProgramIcon : TIcon;     /// Ic�ne du programme
    FProgramName : string;    /// Nom du programme
    FVersion : string;        /// Intitul� de version
    FProgramVersion : string; /// Version du programme
    FAuthor : string;         /// Intitul� d'auteur
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
    G�re une bo�te de dialogue demandant un nombre � l'utilisateur
    @author sjrd
    @version 1.0
  *}
  TSdNumberDialog = class(TComponent)
  private
    FTitle : string;  /// Titre de la bo�te de dialogue
    FPrompt : string; /// Invite de la bo�te de dialogue
    FValue : integer; /// Valeur par d�faut, puis celle saisie par l'utilisateur
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
  Affiche une bo�te de dialogue modale Windows
  @param Title   Titre de la bo�te de dialogue
  @param Text    Texte de la bo�te de dialogue
  @param Flags   Flags contr�lant le style de bo�te de dialogue
  @return Code de r�sultat du bouton sur lequel a cliqu� l'utilisateur
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
  Affiche une bo�te de dialogue modale Windows ou QT (selon la VCL utilis�e)
  @param Title        Titre de la bo�te de dialogue
  @param Text         Texte de la bo�te de dialogue
  @param DlgType      Type de bo�te de dialogue
  @param DlgButtons   Boutons � placer dans la bo�te de dialogue
  @param DefButton    Num�ro du bouton s�lectionn� par d�faut (� partir de 1)
  @param AddFlags     Flags additionnels contr�lant de style de bo�te (Win32)
  @return Bouton sur lequel a cliqu� l'utilisateur
*}

{$IFDEF MSWINDOWS}
function ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
var TypeFlags, BtnsFlags, DefBtnFlags, Flags : LongWord;
begin
  // Transformation du param�tre DlgType en flag de type
  case DlgType of
    dtCustom       : TypeFlags := 0;
    dtInformation  : TypeFlags := MB_ICONINFORMATION;
    dtWarning      : TypeFlags := MB_ICONEXCLAMATION;
    dtError        : TypeFlags := MB_ICONERROR;
    dtConfirmation : TypeFlags := MB_ICONQUESTION;
    else TypeFlags := 0;
  end;

  // Transformation du param�tre DlgButtons en flag de boutons
  case DlgButtons of
    dbOK               : BtnsFlags := MB_OK;
    dbOKCancel         : BtnsFlags := MB_OKCANCEL;
    dbYesNo            : BtnsFlags := MB_YESNO;
    dbYesNoCancel      : BtnsFlags := MB_YESNOCANCEL;
    dbRetryCancel      : BtnsFlags := MB_RETRYCANCEL;
    dbAbortRetryIgnore : BtnsFlags := MB_AbortRetryIgnore;
    else BtnsFlags := MB_OK;
  end;

  // Transformation du param�tre DefButton en flag de bouton par d�faut
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
  // D�termination du nombre de boutons
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

  // D�termination du bouton par d�faut
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

  // Appel de MessageDlg et renvoi de la valeur renvoy�e par celle-ci
  Result := MessageDlg(Title, Text, DlgType, DlgButtons, 0, MsgDefBtn);
end;
{$ENDIF}

{*
  Affiche une bo�te de dialogue avec des boutons radio
  ShowDialogRadio est une variante de ShowDialog qui affiche des boutons radio
  pour chaque choix possible.
  @param Title         Titre de la bo�te de dialogue
  @param Text          Texte de la bo�te de dialogue
  @param DlgType       Type de bo�te de dialogue
  @param DlgButtons    Boutons pr�sents dans la bo�te de dialogue
  @param DefButton     Bouton s�lectionn� par d�faut
  @param RadioTitles   Libell�s des diff�rents boutons radio
  @param Selected      Bouton radio s�lectionn�
  @param OverButtons   Boutons radio plac�s au-dessus des boutons si True
  @return Bouton sur lequel a cliqu� l'utilisateur
*}
function ShowDialogRadio(const Title, Text : string; DlgType : TMsgDlgType;
  DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
  const RadioTitles : array of string; var Selected : integer;
  OverButtons : boolean = False) : Word;
var Form : TForm;
    I, MaxWidth, OldWidth : integer;
    Button : TButton;
begin
  // Cr�ation de la bo�te de dialogue
  Form := CreateMessageDialog(Text, DlgType, DlgButtons);

  with Form do
  try
    Caption := Title;
    // On augmente la taille de la bo�te de dialogue
    Height := Height + Length(RadioTitles) * 25;

    // Cr�ation des boutons radio et d�termination de la largeur minimale
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

    // Il faut aussi v�rifier que la fiche peut afficher les textes des RadioBox
    // en entier
    OldWidth := 0;
    if (MaxWidth + 40) > Width then
    begin
      OldWidth := Width;
      Width := MaxWidth +40;
    end;

    for I := 0 to ComponentCount-1 do
    begin
      // On r�cup�re chaque bouton
      if Components[I] is TButton then
      begin
        Button := TButton(Components[I]);

        // On met le bon bouton par d�faut et on le s�lectionne
        Button.Default := Button.ModalResult = DefButton;
        if Button.Default then ActiveControl := Button;

        // S'il le faut, d�caler tous les boutons vers le bas
        if OverButtons then
          Button.Top := Button.Top + Length(RadioTitles) * 25;

        // S'il le faut, d�caler tous les boutons vers la droite
        if OldWidth > 0 then
          Button.Left := Button.Left + (Width - OldWidth) div 2;
      end;
    end;

    // On centre la bo�te de dialogue
    Position := poScreenCenter;

    // Affichage de la bo�te de dialogue
    Result := ShowModal;

    // R�cup�ration du choix de l'utilisateur
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
  Demande un mot de passe � l'utilisateur
  @return Le mot de passe qu'a saisi l'utilisateur
*}
function QueryPassword : string;
begin
  Result := TSdPasswordForm.QueryPassword;
end;

{*
  Demande un mot de passe � l'utilisateur
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
  Affiche une bo�te de dialogue � propos
  @param Title            Titre de la bo�te de dialogue
  @param ProgramIcon      Ic�ne du programme
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
  Demande un nombre � l'utilisateur
  @param Title     Titre de la bo�te de dialogue
  @param Prompt    Invite de la bo�te de dialogue
  @param Default   Valeur par d�faut
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
  Cr�e une instance de TSdPasswordDialog
  @param AOwner   Propri�taire
*}
constructor TSdPasswordDialog.Create(AOwner : TComponent);
begin
  inherited;
  FPassword := '';
  FShowErrorMes := True;
end;

{*
  Demande un mot de passe � l'utilisateur
  @return True si l'utilisateur a saisi le bon mot de passe, False sinon
*}
function TSdPasswordDialog.Execute : boolean;
begin
  Result := Execute(Password, ShowErrorMes);
end;

{*
  Demande un mot de passe � l'utilisateur
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
  Demande un mot de passe � l'utilisateur
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
  Cr�e une instance de TSdAboutDialog
  @param AOwner   Propri�taire
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
  D�truit l'instance
*}
destructor TSdAboutDialog.Destroy;
begin
  FProgramIcon.Free;
  inherited;
end;

{*
  Modifie l'ic�ne du programme
  @param New   Nouvelle ic�ne
*}
procedure TSdAboutDialog.SetProgramIcon(New : TIcon);
begin
  FProgramIcon.Assign(New);
end;

{*
  Indique si le titre doit �tre stock� dans un flux dfm
  @return True si le titre est diff�rent de celui par d�faut, False sinon
*}
function TSdAboutDialog.IsTitleStored : boolean;
begin
  Result := FTitle <> sScAbout;
end;

{*
  Indique si l'intitul� de version doit �tre stock� dans un flux dfm
  @return True si l'intitul� est diff�rent de celui par d�faut, False sinon
*}
function TSdAboutDialog.IsVersionStored : boolean;
begin
  Result := FVersion <> sScVersion+sScColon;
end;

{*
  Indique si la version du programme doit �tre stock�e dans un flux dfm
  @return True si la version est '1.0', False sinon
*}
function TSdAboutDialog.IsProgramVersionStored : boolean;
begin
  Result := FProgramVersion <> '1.0'; {don't localize}
end;

{*
  Indique si l'intitul� d'auteur doit �tre stock� dans un flux dfm
  @return True si l'intitul� est diff�rent de celui par d�faut, False sinon
*}
function TSdAboutDialog.IsAuthorStored : boolean;
begin
  Result := FAuthor <> sScAuthor+sScColon;
end;

{*
  Affiche la bo�te de dialogue
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
  Cr�e une instance de TSdNumberDialog
  @param AOwner   Propri�taire
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

{*
  Affiche la bo�te de dialogue
  @param ATitle     Titre de la bo�te de dialogue
  @param APrompt    Invite
  @param ADefault   Valeur par d�faut
  @param AMin       Valeur minimale
  @param AMax       Valeur maximale
  @return Valeur s�lectionn�e par l'utilisateur
*}
function TSdNumberDialog.Execute(const ATitle, APrompt : string;
  ADefault, AMin, AMax : integer) : integer;
begin
  FValue := QueryNumber(ATitle, APrompt, ADefault, AMin, AMax);
  Result := FValue;
end;

{*
  Affiche la bo�te de dialogue
  @param ADefault   Valeur par d�faut
  @param AMin       Valeur minimale
  @param AMax       Valeur maximale
  @return Valeur s�lectionn�e par l'utilisateur
*}
function TSdNumberDialog.Execute(
  ADefault, AMin, AMax : integer) : integer;
begin
  Result := Execute(Title, Prompt, ADefault, AMin, AMax);
end;

{*
  Affiche la bo�te de dialogue
  Cette variante utilise comme valeur par d�faut la valeur s�lectionn�e par
  l'utilisateur lors de la pr�c�dent invocation.
  @param ATitle    Titre de la bo�te de dialogue
  @param APrompt   Invite
  @param AMin      Valeur minimale
  @param AMax      Valeur maximale
  @return Valeur s�lectionn�e par l'utilisateur
*}
function TSdNumberDialog.Execute(const ATitle, APrompt : string;
  AMin, AMax : integer) : integer;
begin
  Result := Execute(Title, Prompt, Value, AMin, AMax);
end;

{*
  Affiche la bo�te de dialogue
  Cette variante utilise comme valeur par d�faut la valeur s�lectionn�e par
  l'utilisateur lors de la pr�c�dent invocation.
  @param AMin   Valeur minimale
  @param AMax   Valeur maximale
  @return Valeur s�lectionn�e par l'utilisateur
*}
function TSdNumberDialog.Execute(AMin, AMax : integer) : integer;
begin
  Result := Execute(Title, Prompt, Value, AMin, AMax);
end;

{*
  Affiche la bo�te de dialogue
  Cette variante utilise comme valeur par d�faut la valeur s�lectionn�e par
  l'utilisateur lors de la pr�c�dent invocation.
  @return Valeur s�lectionn�e par l'utilisateur
*}
function TSdNumberDialog.Execute : integer;
begin
  Result := Execute(Title, Prompt, Value, Min, Max);
end;

end.

