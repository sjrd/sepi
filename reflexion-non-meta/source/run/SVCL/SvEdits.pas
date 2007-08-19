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
    Classe de base pour les �diteurs de nombres
    TSvCustomNumberEdit peut bloquer la saisie de nombres flottants ou de
    nombres n�gatifs selon des propri�t�s. Il peut forcer le caract�re de
    s�parations des d�cimales, une valeur minimum et une valeur maximum. Il peut
    aussi assurer un pr�fixe et/ou un suffixe textuels sur le nombre.
    @author sjrd
    @version 1.0
  *}
  TSvCustomNumberEdit = class(TCustomEdit)
  private
    FReadingValue : Double;   /// Valeur lue durant un chargement depuis dfm
    FPrefix : string;         /// Pr�fixe textuel
    FSuffix : string;         /// Suffixe textuel
    FAllowNegative : boolean; /// Indique si les n�gatifs sont autoris�s
    FAllowFrac : boolean;     /// Indique si les d�cimaux sont autoris�s
    FSeparator : Char;        /// Caract�re des s�paration des d�cimales
    FHasMinValue : boolean;   /// Indique si une valeur minimale est impos�e
    FHasMaxValue : boolean;   /// Indique si une valeur maximale est impos�e
    FMinValue : Double;       /// Valeur minimale
    FMaxValue : Double;       /// Valeur maximale

    function GetValue : Double;
    procedure SetValue(New : Double);
    function GetInt : integer;
    procedure SetInt(New : integer);

    procedure SetPrefix(New : string);
    procedure SetSuffix(New : string);
    procedure SetAllowNegative(New : boolean);
    procedure SetAllowFrac(New : boolean);
    procedure SetSeparator(New : Char);
    procedure SetHasMinValue(New : boolean);
    procedure SetHasMaxValue(New : boolean);
    procedure SetMinValue(New : Double);
    procedure SetMaxValue(New : Double);

    function SelEnd : integer;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;

    property Int : integer read GetInt write SetInt;

    /// [@inheritDoc]
    property AutoSelect default False;

    property Value : Double read GetValue write SetValue;
    property Prefix : string read FPrefix write SetPrefix;
    property Suffix  : string read FSuffix  write SetSuffix;
    property AllowNegative : boolean read FAllowNegative write SetAllowNegative
      default False;
    property AllowFrac : boolean read FAllowFrac write SetAllowFrac
      default False;
    property Separator : Char read FSeparator write SetSeparator default ',';
    property HasMinValue : boolean read FHasMinValue write SetHasMinValue
      default False;
    property HasMaxValue : boolean read FHasMaxValue write SetHasMaxValue
      default False;
    property MinValue : Double read FMinValue write SetMinValue;
    property MaxValue : Double read FMaxValue write SetMaxValue;
  public
    constructor Create(AOwner : TComponent); override;
  end;

  {*
    Edit pour les nombres
    TSvCustomNumberEdit peut bloquer la saisie de nombres flottants ou de
    nombres n�gatifs selon des propri�t�s. Il peut forcer le caract�re de
    s�parations des d�cimales, une valeur minimum et une valeur maximum. Il peut
    aussi assurer un pr�fixe et/ou un suffixe textuels sur le nombre.
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
  Cr�e une instance de TSvCustomNumberEdit
  @param AOwner   Propri�taire
*}
constructor TSvCustomNumberEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  AutoSelect := False;
  FReadingValue := 0.0;
  FPrefix := '';
  FSuffix  := '';
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
function TSvCustomNumberEdit.GetValue : Double;
var Str : string;
begin
  if csReading in ComponentState then
  begin
    Result := FReadingValue;
    exit;
  end;

  Str := Text; // To save a bit of time
  Result := 0.0;
  if Length(Str) > ( Length(FPrefix) + Length(FSuffix) ) then
  if Copy(Str, 1, Length(FPrefix)) = FPrefix then
  if Copy(Str, Length(Str)-Length(FSuffix)+1, Length(FSuffix)) = FSuffix then
  begin
    Str := Copy(Str, Length(FPrefix)+1,
      Length(Str)-Length(FPrefix)-Length(FSuffix));

    if Pos(FSeparator, Str) > 0 then
      Str[Pos(FSeparator, Str)] := DecimalSeparator;

    try
      Result := StrToFloat(Str);
    except
      on E : EConvertError do Result := 0.0;
    end;

    if FHasMinValue and (Result < FMinValue) then Result := FMinValue;
    if FHasMaxValue and (Result > FMaxValue) then Result := FMaxValue;
  end;
end;

{*
  Modifie la valeur courante
  @param New   Nouvelle valeur
*}
procedure TSvCustomNumberEdit.SetValue(New : Double);
var Str : string;
begin
  if csReading in ComponentState then
  begin
    FReadingValue := New;
    exit;
  end;

  if not AllowNegative then New := Abs(New);
  if not AllowFrac then New := Round(New);
  if FHasMinValue and (New < FMinValue) then New := FMinValue;
  if FHasMaxValue and (New > FMaxValue) then New := FMaxValue;

  Str := FloatToStr(New);
  if Pos(DecimalSeparator, Str) > 0 then
    Str[Pos(DecimalSeparator, Str)] := FSeparator;
  Text := Prefix + Str + Suffix;
end;

{*
  Valeur courante sous forme enti�re
  @return Valeur courante
*}
function TSvCustomNumberEdit.GetInt : integer;
begin
  Result := Round(Value);
end;

{*
  Modifie la valeur courante
  @param New   Nouvelle valeur
*}
procedure TSvCustomNumberEdit.SetInt(New : integer);
begin
  Value := New;
end;

{*
  Modifie le pr�fixe textuel
  @param New   Nouveau pr�fixe textuel
*}
procedure TSvCustomNumberEdit.SetPrefix(New : string);
var Anc : Double;
begin
  Anc := Value;
  FPrefix := New;
  if not (csReading in ComponentState) then Value := Anc;
end;

{*
  Modifie le suffixe textuel
  @param New   Nouveau suffixe textuel
*}
procedure TSvCustomNumberEdit.SetSuffix(New : string);
var Anc : Double;
begin
  Anc := Value;
  FSuffix := New;
  if not (csReading in ComponentState) then Value := Anc;
end;

{*
  Modifie l'autorisation des nombres n�gatifs
  @param New   True pour autoriser les nombres n�gatifs, False pour interdire
*}
procedure TSvCustomNumberEdit.SetAllowNegative(New : boolean);
var Anc : Double;
begin
  Anc := Value;
  FAllowNegative := New;
  if not (csReading in ComponentState) then
    if New then Value := Anc else Value := Abs(Anc);
end;

{*
  Modifie l'autorisation des nombres d�cimaux
  @param New   True pour autoriser les nombres d�cimaux, False pour interdire
*}
procedure TSvCustomNumberEdit.SetAllowFrac(New : boolean);
var Anc : Double;
begin
  Anc := Value;
  FAllowFrac := New;
  if not (csReading in ComponentState) then
    if New then Value := Anc else Value := Round(Anc);
end;

{*
  Modifie le caract�re de s�paration des d�cimales
  @param New   Nouveau caract�re de s�paration
*}
procedure TSvCustomNumberEdit.SetSeparator(New : Char);
var Anc : Double;
begin
  Anc := Value;
  FSeparator := New;
  if not (csReading in ComponentState) then Value := Anc;
end;

{*
  Modifie la prise en compte de la valeur minimale
  @param New   True pour la prendre en compte, False pour ne pas le faire
*}
procedure TSvCustomNumberEdit.SetHasMinValue(New : boolean);
begin
  if New = FHasMinValue then exit;
  FHasMinValue := New;
  MaxValue := MaxValue;
  Value := Value;
end;

{*
  Modifie la prise en compte de la valeur maximale
  @param New   True pour la prendre en compte, False pour ne pas le faire
*}
procedure TSvCustomNumberEdit.SetHasMaxValue(New : boolean);
begin
  if New = FHasMaxValue then exit;
  FHasMaxValue := New;
  MinValue := MinValue;
  Value := Value;
end;

{*
  Modifie la valeur minimale
  @param New   Nouvelle valeur minimale
*}
procedure TSvCustomNumberEdit.SetMinValue(New : Double);
begin
  if FHasMaxValue and (New > FMaxValue) then
    FMinValue := FMaxValue else FMinValue := New;
  Value := Value;
end;

{*
  Modifie la valeur maximale
  @param New   Nouvelle valeur maximale
*}
procedure TSvCustomNumberEdit.SetMaxValue(New : Double);
begin
  if FHasMinValue and (New < FMinValue) then
    FMaxValue := FMinValue else FMaxValue := New;
  Value := Value;
end;

{*
  D�termine la position de la fin de la s�lection
  @return Position de la fin de la s�lection
*}
function TSvCustomNumberEdit.SelEnd : integer;
begin
  Result := SelStart + SelLength;
end;

{*
  Ex�cut� lorsque l'utilisateur enfonce une touche
  @param Key     Code de la touche enfonc�e
  @param Shift   �tat des touches syst�me et des boutons de souris
*}
procedure TSvCustomNumberEdit.KeyDown(var Key : Word; Shift : TShiftState);
const IgnoredKeys =
  [VK_Tab, VK_Return, VK_Shift, VK_Control, VK_Menu, VK_Pause, VK_Capital,
  VK_Space, VK_Prior, VK_Next, VK_End, VK_Home, VK_Left, VK_Up, VK_Right,
  VK_Down];
var Debut, Fin : integer;
begin
  if ReadOnly then exit;
  if Key in IgnoredKeys then exit;

  Debut := Length(FPrefix);
  Fin := Length(Text)-Length(FSuffix);

  if (SelStart < Debut) or (SelEnd > Fin) then Key := 0 else
  if (SelStart = Fin) and (SelEnd = Fin) and (Key = VK_Delete) then Key := 0;

  if Key = 0 then MessageBeep(0);
end;

{*
  Ex�cut� lorsque l'utilisateur presse une touche
  @param Key   Code ASCII de la touche press�e
*}
procedure TSvCustomNumberEdit.KeyPress(var Key: Char);
var Debut, Fin : integer;
    CurText : string;
begin
  if ReadOnly then exit;

  Debut := Length(FPrefix);
  Fin := Length(Text)-Length(FSuffix);
  CurText := Copy(Text, Debut+1, Fin-Debut);

  if (SelStart < Debut) or (SelEnd > Fin) then
    Key := #0 else

  if (SelStart = Debut) and (SelEnd = Debut) and (Key = Char(VK_Back)) then
    Key := #0 else

  if (Key = '-') and ( (not FAllowNegative) or (SelStart > Debut) or
     ((CurText <> '') and (CurText[1] = '-')) ) then
    Key := #0 else

  if ((Key = '.') or (Key = ',')) and ((not AllowFrac) or
     (Pos(FSeparator, CurText) > 0)) then
    Key := #0 else

  if ((Key < '0') or (Key > '9')) and
     (Key <> '.') and (Key <> ',') and (Key <> '-') and
     (Key <> Char(VK_Back)) then
    Key := #0 else

  if (Key = '.') or (Key = ',') then
    Key := FSeparator;
end;

{*
  Ex�cut� lorsque le composant a �t� charg� depuis un flux dfm
*}
procedure TSvCustomNumberEdit.Loaded;
begin
  inherited;
  Value := FReadingValue;
end;

end.

