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

///////////////////////////
/// Classe TSdAboutForm ///
///////////////////////////

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
