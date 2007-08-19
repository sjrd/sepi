{*
  Fiche principale de SepiOptions
  @author S�bastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiOptionsMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SepiCore, SepiAbout;

type
  {*
    Classe fiche principale de SepiOptions
    @author S�bastien Jean Robert Doeraene
    @version 1.0
  *}
  TFormMain = class(TForm)
    SepiAboutDialog: TSepiAboutDialog;
    LabelHighlight: TLabel;
    ButtonHighlight: TBitBtn;
    LabelAbout: TLabel;
    ButtonAbout: TBitBtn;
    LabelAccept: TLabel;
    ButtonAccept: TBitBtn;
    procedure ButtonHighlightClick(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonAcceptClick(Sender: TObject);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{*
  Gestionnaire de l'�v�nement OnClick de ButtonHighlight
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ButtonHighlightClick(Sender: TObject);
begin
//
end;

{*
  Gestionnaire de l'�v�nement OnClick de ButtonAbout
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ButtonAboutClick(Sender: TObject);
begin
  SepiAboutDialog.Execute;
end;

{*
  Gestionnaire de l'�v�nement OnClick de ButtonAccept
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ButtonAcceptClick(Sender: TObject);
begin
  Close;
end;

end.

