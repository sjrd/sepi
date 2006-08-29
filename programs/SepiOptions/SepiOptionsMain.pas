unit SepiOptionsMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SepiCore, SepiAbout;

type
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

procedure TFormMain.ButtonHighlightClick(Sender: TObject);
begin
//
end;

procedure TFormMain.ButtonAboutClick(Sender: TObject);
begin
  SepiAboutDialog.Execute;
end;

procedure TFormMain.ButtonAcceptClick(Sender: TObject);
begin
  Close;
end;

end.
