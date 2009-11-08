{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  Sébastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.

Linking this library statically or dynamically with other modules is making a
combined work based on this library.  Thus, the terms and conditions of the GNU
General Public License cover the whole combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.
-------------------------------------------------------------------------------}

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
    LabelPrompt: TLabel; /// Label pour l'invite
    ButtonOK: TButton;   /// Bouton OK
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Composants non disponibles dans Turbo Explorer }
    EditValue: TSpinEdit; /// Éditeur pour la valeur sélectionnée

    { Déclarations privées }
  public
    { Déclarations publiques }
    class function QueryNumber(const Title, Prompt: string;
      Default, Min, Max: Integer): Integer;
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
class function TSdNumberForm.QueryNumber(const Title, Prompt: string;
  Default, Min, Max: Integer): Integer;
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
  if Key = VK_RETURN then
    ModalResult := mrOk;
end;

{*
  Gestionnaire d'événement OnKeyPress
  @param Sender   Objet qui a déclenché l'événement
  @param Key      Caractère frappé
*}
procedure TSdNumberForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    Key := #0;
end;

end.

