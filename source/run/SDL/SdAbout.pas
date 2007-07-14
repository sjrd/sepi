{*
  Boîte de dialogue À propos
  @author sjrd
  @version 1.0
*}
unit SdAbout platform;

interface

uses
{$IFDEF MSWINDOWS}
  Forms, Graphics, StdCtrls, ExtCtrls, Controls,
{$ENDIF}
{$IFDEF LINUX}
  QForms, QGraphics, QStdCtrls, QExtCtrls, QControls,
{$ENDIF}
  ScStrUtils, SvLabels, Classes;

type
  {*
    Boîte de dialogue À propos
    @author sjrd
    @version 1.0
  *}
  TSdAboutForm = class(TForm)
    ImageProgramIcon: TImage;
    LabelProgramName: TLabel;
    LabelProgramVersion: TLabel;
    LabelAuthor: TLabel;
    ButtonOK: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Composants non disponibles dans Turbo Explorer }
    URLLabelEMail : TSvURLLabel;
    URLLabelWebSite : TSvURLLabel;

    { Déclarations publiques }
    class procedure ShowAbout(Title : string; ProgramIcon : TGraphic;
      ProgramName : string; ProgramVersion : string; Author : string;
      AuthorEMail : string = ''; WebSite : string = '');
  end;

implementation

{$R *.dfm}

{---------------------}
{ Classe TSdAboutForm }
{---------------------}

{*
  Gestionnaire d'événement OnCreate
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TSdAboutForm.FormCreate(Sender: TObject);
begin
  // Création des composants non disponibles dans Turbo Explorer

  URLLabelEMail := TSvURLLabel.Create(Self);
  with URLLabelEMail do
  begin
    Name := 'URLLabelEMail'; {don't localize}
    Parent := Self;
    Left := 16;
    Top := 112;
  end;

  URLLabelWebSite := TSvURLLabel.Create(Self);
  with URLLabelWebSite do
  begin
    Name := 'URLLabelWebSite'; {don't localize}
    Parent := Self;
    Left := 16;
    Top := 136;
  end;
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
class procedure TSdAboutForm.ShowAbout(Title : string; ProgramIcon : TGraphic;
  ProgramName : string; ProgramVersion : string; Author : string;
  AuthorEMail : string; WebSite : string);
begin
  with Create(nil) do
  try
    Caption := Title;
    ImageProgramIcon.Picture.Assign(ProgramIcon);
    LabelProgramName.Caption := ProgramName;
    LabelProgramVersion.Caption := ProgramVersion;
    LabelAuthor.Caption := Author;
    if AuthorEMail = '' then URLLabelEMail.Visible := False else
    begin
      URLLabelEMail.Caption := GetFirstToken(AuthorEMail, '?'); {don't localize}
      URLLabelEMail.URL := 'mailto:'+AuthorEMail; {don't localize}
    end;
    if WebSite = '' then URLLabelWebSite.Visible := False else
      URLLabelWebSite.Caption := WebSite;
    ShowModal;
  finally
    Free;
  end;
end;

end.

