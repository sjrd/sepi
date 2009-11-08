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
  Boîte de dialogue À propos
  @author sjrd
  @version 1.0
*}
unit SdAbout;

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
    ImageProgramIcon: TImage;    /// Image pour l'icône
    LabelProgramName: TLabel;    /// Label pour le nom du programme
    LabelProgramVersion: TLabel; /// Label pour la version
    LabelAuthor: TLabel;         /// Label pour l'auteur
    ButtonOK: TButton;           /// Bouton OK
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Composants non disponibles dans Turbo Explorer }
    URLLabelEMail: TSvURLLabel;   /// Label pour l'e-mail de l'auteur
    URLLabelWebSite: TSvURLLabel; /// Label pour le site Web

    { Déclarations publiques }
    class procedure ShowAbout(Title: string; ProgramIcon: TGraphic;
      ProgramName: string; ProgramVersion: string; Author: string;
      AuthorEMail: string = ''; WebSite: string = '');
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
class procedure TSdAboutForm.ShowAbout(Title: string; ProgramIcon: TGraphic;
  ProgramName: string; ProgramVersion: string; Author: string;
  AuthorEMail: string; WebSite: string);
begin
  with Create(nil) do
    try
      Caption := Title;
      ImageProgramIcon.Picture.Assign(ProgramIcon);
      LabelProgramName.Caption := ProgramName;
      LabelProgramVersion.Caption := ProgramVersion;
      LabelAuthor.Caption := Author;
      if AuthorEMail = '' then
        URLLabelEMail.Visible := False
      else
      begin
        URLLabelEMail.Caption := GetFirstToken(AuthorEMail, '?');
        {don't localize}
        URLLabelEMail.URL := 'mailto:'+AuthorEMail; {don't localize}
      end;
      if WebSite = '' then
        URLLabelWebSite.Visible := False
      else
        URLLabelWebSite.Caption := WebSite;
      ShowModal;
    finally
      Free;
    end;
end;

end.

