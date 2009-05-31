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
  Composants de type Image
  @author sjrd
  @version 1.0
*}
unit SvImages;

interface

uses
  Windows, Forms, Classes, Controls, ExtCtrls;

type
  {*
    Type de l'événement TSvDropImage.OnDrop
    @param Sender   Objet qui a déclenché l'événement
    @param X        Coordonnée X de dépôt de l'image
    @param Y        Coordonnée Y de dépôt de l'image
  *}
  TDropImageEvent = procedure(Sender: TObject; X, Y: Integer) of object;

  {*
    Image pouvant être « prise » et déposée
    TSvDropImage est une image qui peut être « prise » au moyen d'un clic de
    souris et déposée par glisser-déposer soit sur le parent, soit sur un autre
    contrôle. Ce composant permet de constituer de façon très simple une palette
    de glisser-déposer.
    @author sjrd
    @version 1.0
  *}
  TSvDropImage = class(TImage)
  private
    ImageBis: TImage;         /// Copie de l'image en train d'être glissée
    FDropControl: TControl;   /// Contrôle sur lequel il faut déposer l'image
    FOnDrop: TDropImageEvent; /// Exécuté lorsque l'image a été déposée

    procedure MoveBisAt(X, Y: Integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DropControl: TControl
      read FDropControl write FDropControl;
    property OnDrop: TDropImageEvent read FOnDrop write FOnDrop;
  end;

implementation

{---------------------}
{ Classe TScDropImage }
{---------------------}

{*
  Crée une instance de TSvDropImage
  @param AOwner   Propriétaire
*}
constructor TSvDropImage.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse];
  ImageBis := nil;
  FDropControl := nil;
  FOnDrop := nil;
end;

{*
  Déplace la copie en déplacement de l'image à des coordonnées spécifiées
  @param X   Coordonnée X du point sur lequel centrer la copie de l'image
  @param Y   Coordonnée Y du point sur lequel centrer la copie de l'image
*}
procedure TSvDropImage.MoveBisAt(X, Y: Integer);
var
  L, T, ParentWidth, ParentHeight: Integer;
begin
  if Parent is TForm then
  begin
    ParentWidth := (Parent as TForm).ClientWidth;
    ParentHeight := (Parent as TForm).ClientHeight;
  end else
  begin
    ParentWidth := Parent.Width;
    ParentHeight := Parent.Height;
  end;

  L := Left + X - Width  div 2;
  T := Top  + Y - Height div 2;

  if L > (ParentWidth -Width) then
    L := ParentWidth -Width;
  if L < 0 then
    L := 0;
  if T > (ParentHeight-Height) then
    T := ParentHeight-Height;
  if T < 0 then
    T := 0;

  ImageBis.Left := L;
  ImageBis.Top := T;
end;

{*
  Exécuté lorsque l'utilisateur enfonce le bouton de la souris
  @param Button   Bouton de souris enfoncé
  @param Shift    État des touches système et des boutons de souris
  @param X        Coordonnée X du point de clic
  @param Y        Coordonnée Y du point de clic
*}
procedure TSvDropImage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if not Enabled then
    Exit;
  if (Button <> mbLeft) or Assigned(ImageBis) then
    Exit;

  ImageBis := TImage.Create(Self);
  ImageBis.Parent := Parent;
  ImageBis.Width := Width;
  ImageBis.Height := Height;
  ImageBis.Stretch := Stretch;
  ImageBis.Picture.Assign(Picture);
  MoveBisAt(X, Y);
end;

{*
  Exécuté lorsque l'utilisateur déplace la souris
  @param Shift    État des touches système et des boutons de souris
  @param X        Coordonnée X de la souris
  @param Y        Coordonnée Y de la souris
*}
procedure TSvDropImage.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(ImageBis) then
    MoveBisAt(X, Y);
end;

{*
  Exécuté lorsque l'utilisateur relâche le bouton de la souris
  @param Button   Bouton de souris relâché
  @param Shift    État des touches système et des boutons de souris
  @param X        Coordonnée X du point de relâche
  @param Y        Coordonnée Y du point de relâche
*}
procedure TSvDropImage.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  PutPoint: TPoint;
begin
  inherited;
  if (Button <> mbLeft) or (not Assigned(ImageBis)) then
    Exit;

  ImageBis.Free;
  ImageBis := nil;

  if not Assigned(FOnDrop) then
    Exit;
  if Assigned(FDropControl) then
    PutPoint := FDropControl.ScreenToClient(ClientToScreen(Point(X, Y)))
  else
    PutPoint := ClientToParent(Point(X, Y));
  FOnDrop(Self, PutPoint.X, PutPoint.Y);
end;

end.

