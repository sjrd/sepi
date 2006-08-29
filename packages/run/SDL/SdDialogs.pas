unit SdDialogs;

interface

uses
  Classes, Graphics, SdPassword, SdAbout, ScConsts;

type
  TSdPasswordDialog = class(TComponent)
  private
    FPassword : string;
    FShowErrorMes : boolean;
  public
    constructor Create(AOwner : TComponent); override;
    function Execute : boolean; overload;
    function Execute(Password : string; ShowErrorMes : boolean = True) : boolean; overload;
    function Execute(ShowErrorMes : boolean) : boolean; overload;
  published
    property Password : string read FPassword write FPassword;
    property ShowErrorMes : boolean read FShowErrorMes write FShowErrorMes default True;
  end;

  TSdAboutDialog = class(TComponent)
  private
    FTitle : string;
    FProgramIcon : TIcon;
    FProgramName : string;
    FVersion : string;
    FProgramVersion : string;
    FAuthor : string;
    FAuthorName : string;
    FAuthorEMail : string;
    FWebSite : string;
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
    property Version : string read FVersion write FVersion stored IsVersionStored;
    property ProgramVersion : string read FProgramVersion write FProgramVersion stored IsProgramVersionStored;
    property Author : string read FAuthor write FAuthor stored IsAuthorStored;
    property AuthorName : string read FAuthorName write FAuthorName;
    property AuthorEMail : string read FAuthorEMail write FAuthorEMail;
    property WebSite : string read FWebSite write FWebSite;
  end platform;

function QueryPassword : string; overload;
function QueryPassWord(Password : string; ShowErrorMes : boolean = True) : boolean; overload;

procedure ShowAbout(Title : string; Icon : TIcon; ProgramName : string;
  ProgramVersion : string; Author : string; AuthorEMail : string = '';
  WebSite : string = ''); platform;

implementation

/////////////////////////
/// Routines globales ///
/////////////////////////

function QueryPassword : string;
begin
  Result := TSdPasswordForm.QueryPassword;
end;

function QueryPassWord(Password : string; ShowErrorMes : boolean = True) : boolean;
begin
  Result := TSdPasswordForm.QueryPassword(Password, ShowErrorMes);
end;

procedure ShowAbout(Title : string; Icon : TIcon; ProgramName : string;
  ProgramVersion : string; Author : string; AuthorEMail : string = '';
  WebSite : string = '');
begin
  TSdAboutForm.ShowAbout(Title, Icon, ProgramName, ProgramVersion, Author,
    AuthorEMail, WebSite);
end;

////////////////////////////////
/// Classe TSdPasswordDialog ///
////////////////////////////////

constructor TSdPasswordDialog.Create(AOwner : TComponent);
begin
  inherited;
  FPassword := '';
  FShowErrorMes := True;
end;

function TSdPasswordDialog.Execute : boolean;
begin
  Result := Execute(Password, ShowErrorMes);
end;

function TSdPasswordDialog.Execute(Password : string; ShowErrorMes : boolean = True) : boolean;
begin
  Result := TSdPassWordForm.QueryPassword(Password, ShowErrorMes);
end;

function TSdPasswordDialog.Execute(ShowErrorMes : boolean) : boolean;
begin
  Result := Execute(Password, ShowErrorMes);
end;

/////////////////////////////
/// Classe TSdAboutDialog ///
/////////////////////////////

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

destructor TSdAboutDialog.Destroy;
begin
  FProgramIcon.Free;
  inherited;
end;

procedure TSdAboutDialog.SetProgramIcon(New : TIcon);
begin
  FProgramIcon.Assign(New);
end;

function TSdAboutDialog.IsTitleStored : boolean;
begin
  Result := FTitle <> sScAbout;
end;

function TSdAboutDialog.IsVersionStored : boolean;
begin
  Result := FVersion <> sScVersion+sScColon;
end;

function TSdAboutDialog.IsProgramVersionStored : boolean;
begin
  Result := FProgramVersion <> '1.0'; {don't localize}
end;

function TSdAboutDialog.IsAuthorStored : boolean;
begin
  Result := FAuthor <> sScAuthor+sScColon;
end;

procedure TSdAboutDialog.Execute;
begin
  TSdAboutForm.ShowAbout(Title, ProgramIcon, ProgramName,
    Version+' '+ProgramVersion, Author+' '+AuthorName, AuthorEMail, WebSite);
end;

end.
