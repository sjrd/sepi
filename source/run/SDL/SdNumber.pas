{*
  Boîte de dialogue demandant un nombre à l'utilisateur
  @author sjrd
  @version 5.0
*}
unit SdNumber;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, Spin, ScUtils;

type
  {*
    Boîte de dialogue demandant un nombre à l'utilisateur
    @author sjrd
    @version 1.0
  *}
  TSdNumberForm = class(TForm)
    LabelPrompt: TLabel;
    ButtonOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Composants non disponibles dans Turbo Explorer }
    EditValue : TSpinEdit;

    { Déclarations privées }
  public
    { Déclarations publiques }
    class function QueryNumber(const Title, Prompt : string;
      Default, Min, Max : integer) : integer;
  end;

implementation

{$R *.DFM}

{*
  Affiche une invite à l'utilisateur lui demandant de choisir un nombre
  @param Title     Titre de la boîte de dialogue
  @param Prompt    Invite
  @param Default   Valeur par défaut affichée
  @param Min       Valeur minimale que peut choisir l'utilisateur
  @param Max       Valeur maximale que peut choisir l'utilisateur
  @return La valeur qu'a choisie l'utilisateur
*}
class function TSdNumberForm.QueryNumber(const Title, Prompt : string;
  Default, Min, Max : integer) : integer;
begin
  with Create(Application) do
  try
    Default := MinMax(Default, Min, Max);

    Caption := Title;
    LabelPrompt.Caption := Prompt;

    EditValue.Value := Default;
    EditValue.MinValue := Min;
    EditValue.MaxValue := Max;

    EditValue.Enabled := Min <> Max;
    if EditValue.Enabled then
      ActiveControl := EditValue
    else
      ActiveControl := ButtonOK;

    ShowModal;

    Result := EditValue.Value;
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'événement OnCreate
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TSdNumberForm.FormCreate(Sender: TObject);
begin
  // Création dynamique des composants non disponibles dans Turbo Explorer
  EditValue := TSpinEdit.Create(Self);
  with EditValue do
  begin
    Name := 'EditValue'; {don't localize}
    Parent := Self;
    Left := 176;
    Top := 16;
    Width := 65;
    AutoSelect := False;
    EditorEnabled := False;
    TabOrder := 0;
  end;
end;

{*
  Gestionnaire d'événement OnKeyDown
  @param Sender   Objet qui a déclenché l'événement
  @param Key      Touche enfoncée
  @param Shift    État des touches système
*}
procedure TSdNumberForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then ModalResult := mrOK;
end;

{*
  Gestionnaire d'événement OnKeyPress
  @param Sender   Objet qui a déclenché l'événement
  @param Key      Caractère frappé
*}
procedure TSdNumberForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Key := #0;
end;

end.

