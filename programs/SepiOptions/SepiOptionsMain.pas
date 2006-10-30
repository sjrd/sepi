{*
  Fiche principale de SepiOptions
  @author Sébastien Jean Robert Doeraene
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
    @author Sébastien Jean Robert Doeraene
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
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{*
  Gestionnaire de l'événement OnClick de ButtonHighlight
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ButtonHighlightClick(Sender: TObject);
begin
//
end;

{*
  Gestionnaire de l'événement OnClick de ButtonAbout
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ButtonAboutClick(Sender: TObject);
begin
  SepiAboutDialog.Execute;
end;

{*
  Gestionnaire de l'événement OnClick de ButtonAccept
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ButtonAcceptClick(Sender: TObject);
begin
  Close;
end;

end.

