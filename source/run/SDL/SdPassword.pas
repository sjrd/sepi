{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
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
-------------------------------------------------------------------------------}

{*
  Boîte de dialogue de saisie de mot de passe
  @author sjrd
  @version 1.0
*}
unit SdPassword;

interface

uses
{$IFDEF MSWINDOWS}
  Forms, StdCtrls, Buttons, Controls,
{$ENDIF}
{$IFDEF LINUX}
  QForms, QStdCtrls, QButtons, QControls,
{$ENDIF}
  Classes;

type
  {*
    Boîte de dialogue de saisie de mot de passe
    @author sjrd
    @version 1.0
  *}
  TSdPasswordForm = class(TForm)
    LabelPrompt: TLabel;    /// Label pour l'invite
    EditPassword: TEdit;    /// Edit pour le mot de passe
    BoutonOK: TBitBtn;      /// Bouton OK
    BoutonAnnuler: TBitBtn; /// Bouton Annuler
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    class function QueryPassword: string; overload;
    class function QueryPassword(Password: string;
      ShowErrorMes: Boolean = True): Boolean; overload;
  end;

implementation

{$R *.dfm}

uses
  SdDialogs, ScConsts;

{*
  Demande un mot de passe à l'utilisateur
  @return Le mot de passe qu'a saisi l'utilisateur
*}
class function TSdPasswordForm.QueryPassword: string;
begin
  with Create(Application) do
    try
      ActiveControl := EditPassWord;
      if ShowModal <> mrOk then
        Result := ''
      else
        Result := EditPassWord.Text;
    finally
      Release;
    end;
end;

{*
  Demande un mot de passe à l'utilisateur
  @param Password       Mot de passe correct
  @param ShowErrorMes   Indique s'il faut notifier sur erreur
  @return True si l'utilisateur a saisi le bon mot de passe, False sinon
*}
class function TSdPasswordForm.QueryPassword(Password: string;
  ShowErrorMes: Boolean = True): Boolean;
var
  Passwd: string;
begin
  if Password = '' then
    Passwd := ''
  else
    Passwd := QueryPassword;
  Result := Passwd = Password;
  if (not Result) and ShowErrorMes and (Passwd <> '') then
    ShowDialog(sScWrongPassword, sScWrongPassword, dtError, dbOK);
end;

end.

