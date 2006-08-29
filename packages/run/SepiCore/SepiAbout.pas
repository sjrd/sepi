unit SepiAbout;

interface

uses
  Classes, SysUtils, SepiCore, SepiConsts, SdDialogs;

type
  TSepiAboutDialog = class(TComponent)
  private
    FDialog : TSdAboutDialog;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Execute;
  end;

implementation

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
  FDialog.ProgramVersion := Format('%d.%d', [Sepi.Version.MajVersion, Sepi.Version.MinVersion]);
  FDialog.AuthorName := Sepi.Author;
  FDialog.AuthorEMail := Sepi.AuthorEMail;
  FDialog.WebSite := Sepi.WebSite;
end;

procedure TSepiAboutDialog.Execute;
begin
  FDialog.Execute;
end;

end.
