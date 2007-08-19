{*
  D�finit le composant � � propos � de Sepi
  @author sjrd
  @version 1.0
*}
unit SepiAbout;

interface

uses
  Classes, SysUtils, SepiCore, SepiConsts, SdDialogs;

type
  {*
    Bo�te de dialogue � � propos � de Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiAboutDialog = class(TComponent)
  private
    FDialog : TSdAboutDialog; /// Bo�te de dialogue g�n�rique
  public
    constructor Create(AOwner : TComponent); override;

    procedure Execute;
  end;

implementation

{*
  Cr�e une instance de TSepiAboutDialog
  @param AOwner   Propri�taire
*}
constructor TSepiAboutDialog.Create(AOwner : TComponent);
begin
  inherited;
  FDialog := TSdAboutDialog.Create(Self);
  FDialog.Title := sSepiAbout;
  try
    FDialog.ProgramIcon.LoadFromFile(Sepi.Path+'Sepi.ico'); {don't localize}
  except
  end;
  FDialog.ProgramName := Sepi.Name;
  FDialog.ProgramVersion := Format('%d.%d', {don't localize}
    [Sepi.Version.MajVersion, Sepi.Version.MinVersion]);
  FDialog.AuthorName := Sepi.Author;
  FDialog.AuthorEMail := Sepi.AuthorEMail;
  FDialog.WebSite := Sepi.WebSite;
end;

{*
  Affiche la bo�te de dialogue
*}
procedure TSepiAboutDialog.Execute;
begin
  FDialog.Execute;
end;

end.

