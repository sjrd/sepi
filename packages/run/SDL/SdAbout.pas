{*
  Boîte de dialogue À propos
  @author Sébastien Jean Robert Doeraene
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
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSdAboutForm = class(TForm)
    ImageProgramIcon: TImage;
    LabelProgramName: TLabel;
    LabelProgramVersion: TLabel;
    LabelAuthor: TLabel;
    ButtonOK: TButton;
    URLLabelEMail: TSvURLLabel;
    URLLabelWebSite: TSvURLLabel;
  private
    { Déclarations privées }
  public
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

