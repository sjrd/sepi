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
  Composants de type Edit
  @author sjrd
  @version 1.0
*}
unit SvEdits;

interface

uses
  Windows, Messages, SysUtils, Classes, ScUtils, StdCtrls;

type
  {*
    Classe de base pour les éditeurs de nombres
    TSvCustomNumberEdit peut bloquer la saisie de nombres flottants ou de
    nombres négatifs selon des propriétés. Il peut forcer le caractère de
    séparations des décimales, une valeur minimum et une valeur maximum. Il peut
    aussi assurer un préfixe et/ou un suffixe textuels sur le nombre.
    @author sjrd
    @version 1.0
  *}
  TSvCustomNumberEdit = class(TCustomEdit)
  private
    FReadingValue: Double;   /// Valeur lue durant un chargement depuis dfm
    FPrefix: string;         /// Préfixe textuel
    FSuffix: string;         /// Suffixe textuel
    FAllowNegative: Boolean; /// Indique si les négatifs sont autorisés
    FAllowFrac: Boolean;     /// Indique si les décimaux sont autorisés
    FSeparator: Char;        /// Caractère des séparation des décimales
    FHasMinValue: Boolean;   /// Indique si une valeur minimale est imposée
    FHasMaxValue: Boolean;   /// Indique si une valeur maximale est imposée
    FMinValue: Double;       /// Valeur minimale
    FMaxValue: Double;       /// Valeur maximale

    function GetValue: Double;
    procedure SetValue(New: Double);
    function GetInt: Integer;
    procedure SetInt(New: Integer);

    procedure SetPrefix(New: string);
    procedure SetSuffix(New: string);
    procedure SetAllowNegative(New: Boolean);
    procedure SetAllowFrac(New: Boolean);
    procedure SetSeparator(New: Char);
    procedure SetHasMinValue(New: Boolean);
    procedure SetHasMaxValue(New: Boolean);
    procedure SetMinValue(New: Double);
    procedure SetMaxValue(New: Double);

    function SelEnd: Integer;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;

    property Int: Integer read GetInt write SetInt;

    /// [@inheritDoc]
    property AutoSelect default False;

    property Value: Double read GetValue write SetValue;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
    property AllowNegative: Boolean read FAllowNegative write SetAllowNegative
      default False;
    property AllowFrac: Boolean read FAllowFrac write SetAllowFrac
      default False;
    property Separator: Char read FSeparator write SetSeparator default ',';
    property HasMinValue: Boolean read FHasMinValue write SetHasMinValue
      default False;
    property HasMaxValue: Boolean read FHasMaxValue write SetHasMaxValue
      default False;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {*
    Edit pour les nombres
    TSvCustomNumberEdit peut bloquer la saisie de nombres flottants ou de
    nombres négatifs selon des propriétés. Il peut forcer le caractère de
    séparations des décimales, une valeur minimum et une valeur maximum. Il peut
    aussi assurer un préfixe et/ou un suffixe textuels sur le nombre.
    @author sjrd
    @version 1.0
  *}
  TSvNumberEdit = class(TSvCustomNumberEdit)
  public
    property Int;
  published
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property HideSelection;
    property ReadOnly;
    property TabOrder;
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockOrientation;
    property Enabled;
    property FloatingDockSiteClass;
    property ParentColor;
    property ShowHint;
    property Visible;

    property Value;
    property Prefix;
    property Suffix;
    property AllowNegative;
    property AllowFrac;
    property Separator;
    property HasMinValue;
    property HasMaxValue;
    property MinValue;
    property MaxValue;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEnter;
    property OnExit;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
    property OnCanResize;
    property OnConstrainedResize;
    property OnResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDock;
    property OnEndDock;
    property OnStartDrag;
    property OnEndDrag;
  end;

implementation

{----------------------------}
{ Classe TScCustomNumberEdit }
{----------------------------}

{*
  Crée une instance de TSvCustomNumberEdit
  @param AOwner   Propriétaire
*}
constructor TSvCustomNumberEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSelect := False;
  FReadingValue := 0.0;
  FPrefix := '';
  FSuffix := '';
  FAllowNegative := False;
  FAllowFrac := False;
  FSeparator := ',';
  FHasMinValue := False;
  FHasMaxValue := False;
  FMinValue := 0.0;
  FMaxValue := 0.0;
  Int := 0;
end;

{*
  Valeur courante
  @return Valeur courante
*}
function TSvCustomNumberEdit.GetValue: Double;
var
  Str: string;
begin
  if csReading in ComponentState then
  begin
    Result := FReadingValue;
    Exit;
  end;

  Str := Text; // To save a bit of time
  Result := 0.0;
  if Length(Str) > (Length(FPrefix) + Length(FSuffix)) then
    if Copy(Str, 1, Length(FPrefix)) = FPrefix then
      if Copy(Str, Length(Str)-Length(FSuffix)+1, Length(FSuffix)) =
        FSuffix then
      begin
        Str := Copy(Str, Length(FPrefix)+1,
          Length(Str)-Length(FPrefix)-Length(FSuffix));

        if Pos(FSeparator, Str) > 0 then
          Str[Pos(FSeparator, Str)] := DecimalSeparator;

        try
          Result := StrToFloat(Str);
        except
          on E: EConvertError do
            Result := 0.0;
        end;

        if FHasMinValue and (Result < FMinValue) then
          Result := FMinValue;
        if FHasMaxValue and (Result > FMaxValue) then
          Result := FMaxValue;
      end;
end;

{*
  Modifie la valeur courante
  @param New   Nouvelle valeur
*}
procedure TSvCustomNumberEdit.SetValue(New: Double);
var
  Str: string;
begin
  if csReading in ComponentState then
  begin
    FReadingValue := New;
    Exit;
  end;

  if not AllowNegative then
    New := Abs(New);
  if not AllowFrac then
    New := Round(New);
  if FHasMinValue and (New < FMinValue) then
    New := FMinValue;
  if FHasMaxValue and (New > FMaxValue) then
    New := FMaxValue;

  Str := FloatToStr(New);
  if Pos(DecimalSeparator, Str) > 0 then
    Str[Pos(DecimalSeparator, Str)] := FSeparator;
  Text := Prefix + Str + Suffix;
end;

{*
  Valeur courante sous forme entière
  @return Valeur courante
*}
function TSvCustomNumberEdit.GetInt: Integer;
begin
  Result := Round(Value);
end;

{*
  Modifie la valeur courante
  @param New   Nouvelle valeur
*}
procedure TSvCustomNumberEdit.SetInt(New: Integer);
begin
  Value := New;
end;

{*
  Modifie le préfixe textuel
  @param New   Nouveau préfixe textuel
*}
procedure TSvCustomNumberEdit.SetPrefix(New: string);
var
  Anc: Double;
begin
  Anc := Value;
  FPrefix := New;
  if not (csReading in ComponentState) then
    Value := Anc;
end;

{*
  Modifie le suffixe textuel
  @param New   Nouveau suffixe textuel
*}
procedure TSvCustomNumberEdit.SetSuffix(New: string);
var
  Anc: Double;
begin
  Anc := Value;
  FSuffix := New;
  if not (csReading in ComponentState) then
    Value := Anc;
end;

{*
  Modifie l'autorisation des nombres négatifs
  @param New   True pour autoriser les nombres négatifs, False pour interdire
*}
procedure TSvCustomNumberEdit.SetAllowNegative(New: Boolean);
var
  Anc: Double;
begin
  Anc := Value;
  FAllowNegative := New;
  if not (csReading in ComponentState) then
    if New then
      Value := Anc
    else
      Value := Abs(Anc);
end;

{*
  Modifie l'autorisation des nombres décimaux
  @param New   True pour autoriser les nombres décimaux, False pour interdire
*}
procedure TSvCustomNumberEdit.SetAllowFrac(New: Boolean);
var
  Anc: Double;
begin
  Anc := Value;
  FAllowFrac := New;
  if not (csReading in ComponentState) then
    if New then
      Value := Anc
    else
      Value := Round(Anc);
end;

{*
  Modifie le caractère de séparation des décimales
  @param New   Nouveau caractère de séparation
*}
procedure TSvCustomNumberEdit.SetSeparator(New: Char);
var
  Anc: Double;
begin
  Anc := Value;
  FSeparator := New;
  if not (csReading in ComponentState) then
    Value := Anc;
end;

{*
  Modifie la prise en compte de la valeur minimale
  @param New   True pour la prendre en compte, False pour ne pas le faire
*}
procedure TSvCustomNumberEdit.SetHasMinValue(New: Boolean);
begin
  if New = FHasMinValue then
    Exit;
  FHasMinValue := New;
  MaxValue := MaxValue;
  Value := Value;
end;

{*
  Modifie la prise en compte de la valeur maximale
  @param New   True pour la prendre en compte, False pour ne pas le faire
*}
procedure TSvCustomNumberEdit.SetHasMaxValue(New: Boolean);
begin
  if New = FHasMaxValue then
    Exit;
  FHasMaxValue := New;
  MinValue := MinValue;
  Value := Value;
end;

{*
  Modifie la valeur minimale
  @param New   Nouvelle valeur minimale
*}
procedure TSvCustomNumberEdit.SetMinValue(New: Double);
begin
  if FHasMaxValue and (New > FMaxValue) then
    FMinValue := FMaxValue
  else
    FMinValue := New;
  Value := Value;
end;

{*
  Modifie la valeur maximale
  @param New   Nouvelle valeur maximale
*}
procedure TSvCustomNumberEdit.SetMaxValue(New: Double);
begin
  if FHasMinValue and (New < FMinValue) then
    FMaxValue := FMinValue
  else
    FMaxValue := New;
  Value := Value;
end;

{*
  Détermine la position de la fin de la sélection
  @return Position de la fin de la sélection
*}
function TSvCustomNumberEdit.SelEnd: Integer;
begin
  Result := SelStart + SelLength;
end;

{*
  Exécuté lorsque l'utilisateur enfonce une touche
  @param Key     Code de la touche enfoncée
  @param Shift   État des touches système et des boutons de souris
*}
procedure TSvCustomNumberEdit.KeyDown(var Key: Word; Shift: TShiftState);
const
  IgnoredKeys =
    [VK_Tab, VK_Return, VK_Shift, VK_Control, VK_Menu, VK_Pause, VK_Capital,
    VK_Space, VK_Prior, VK_Next, VK_End, VK_Home, VK_Left, VK_Up, VK_Right,
    VK_Down];
var
  Debut, Fin: Integer;
begin
  if ReadOnly then
    Exit;
  if Key in IgnoredKeys then
    Exit;

  Debut := Length(FPrefix);
  Fin := Length(Text)-Length(FSuffix);

  if (SelStart < Debut) or (SelEnd > Fin) then
    Key := 0
  else if (SelStart = Fin) and (SelEnd = Fin) and (Key = VK_Delete) then
    Key := 0;

  if Key = 0 then
    MessageBeep(0);
end;

{*
  Exécuté lorsque l'utilisateur presse une touche
  @param Key   Code ASCII de la touche pressée
*}
procedure TSvCustomNumberEdit.KeyPress(var Key: Char);
var
  Debut, Fin: Integer;
  CurText: string;
begin
  if ReadOnly then
    Exit;

  Debut := Length(FPrefix);
  Fin := Length(Text)-Length(FSuffix);
  CurText := Copy(Text, Debut+1, Fin-Debut);

  if (SelStart < Debut) or (SelEnd > Fin) then
    Key := #0
  else if (SelStart = Debut) and (SelEnd = Debut) and (Key = Char(VK_Back)) then
    Key := #0
  else if (Key = '-') and ((not FAllowNegative) or (SelStart > Debut) or
    ((CurText <> '') and (CurText[1] = '-'))) then
    Key := #0
  else if ((Key = '.') or (Key = ',')) and ((not AllowFrac) or
    (Pos(FSeparator, CurText) > 0)) then
    Key := #0
  else if ((Key < '0') or (Key > '9')) and
    (Key <> '.') and (Key <> ',') and (Key <> '-') and
    (Key <> Char(VK_Back)) then
    Key := #0
  else if (Key = '.') or (Key = ',') then
    Key := FSeparator;
end;

{*
  Exécuté lorsque le composant a été chargé depuis un flux dfm
*}
procedure TSvCustomNumberEdit.Loaded;
begin
  inherited;
  Value := FReadingValue;
end;

end.

