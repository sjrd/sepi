{*
  Définit des routines et composants utilisant des boîtes de dialogue
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SdDialogs;

interface

uses
  Classes, Graphics, SdPassword, SdAbout, ScConsts;

type
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

function QueryPassword : string; overload;

function QueryPassWord(Password : string;
  ShowErrorMes : boolean = True) : boolean; overload;

procedure ShowAbout(Title : string; ProgramIcon : TIcon; ProgramName : string;
  ProgramVersion : string; Author : string; AuthorEMail : string = '';
  WebSite : string = ''); platform;

implementation

{-------------------}
{ Routines globales }
{-------------------}

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

end.

