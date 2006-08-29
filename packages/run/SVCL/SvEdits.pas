unit SvEdits;

interface

uses
  Windows, Messages, SysUtils, Classes, ScUtils, StdCtrls;

type
  TSvCustomNumberEdit = class(TCustomEdit)
  private
    FReadingValue : Double;
    FPrefix, FSuffix : string;
    FAllowNegative, FAllowFrac : boolean;
    FSeparator : Char;
    FHasMinValue, FHasMaxValue : boolean;
    FMinValue, FMaxValue : Double;
    function GetValue : Double;
    function GetInt : integer;
    procedure SetValue(New : Double);
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

    property AutoSelect default False;

    property Value : Double read GetValue write SetValue;
    property Prefix : string read FPrefix write SetPrefix;
    property Suffix  : string read FSuffix  write SetSuffix;
    property AllowNegative : boolean read FAllowNegative write SetAllowNegative default False;
    property AllowFrac : boolean read FAllowFrac write SetAllowFrac default False;
    property Separator : Char read FSeparator write SetSeparator default ',';
    property HasMinValue : boolean read FHasMinValue write SetHasMinValue default False;
    property HasMaxValue : boolean read FHasMaxValue write SetHasMaxValue default False;
    property MinValue : Double read FMinValue write SetMinValue;
    property MaxValue : Double read FMaxValue write SetMaxValue;
  public
    constructor Create(AOwner : TComponent); override;
  end;

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

//////////////////////////////////
/// Classe TScCustomNumberEdit ///
//////////////////////////////////

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

function TSvCustomNumberEdit.GetValue : Double;
var Str : string;
begin
  if csReading in ComponentState then
  begin
    Result := FReadingValue;
    exit;
  end;
  Str := Text; // To save a bit of time
  if Length(Str) <= ( Length(FPrefix) + Length(FSuffix) ) then Result := 0.0 else
  if Copy(Str, 1, Length(FPrefix)) <> FPrefix then Result := 0.0 else
  if Copy(Str, Length(Str)-Length(FSuffix)+1, Length(FSuffix)) <> FSuffix then Result := 0.0 else
  begin
    Str := Copy(Str, Length(FPrefix)+1, Length(Str)-Length(FPrefix)-Length(FSuffix));
    if Pos(FSeparator, Str) > 0 then Str[Pos(FSeparator, Str)] := DecimalSeparator;
    try
      Result := StrToFloat(Str);
    except
      on E : EConvertError do Result := 0.0;
    end;
    if FHasMinValue and (Result < FMinValue) then Result := FMinValue;
    if FHasMaxValue and (Result > FMaxValue) then Result := FMaxValue;
  end;
end;

function TSvCustomNumberEdit.GetInt : integer;
begin
  Result := Round(Value);
end;

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

procedure TSvCustomNumberEdit.SetInt(New : integer);
begin
  Value := New;
end;

procedure TSvCustomNumberEdit.SetPrefix(New : string);
var Anc : Double;
begin
  Anc := Value;
  FPrefix := New;
  if not (csReading in ComponentState) then Value := Anc;
end;

procedure TSvCustomNumberEdit.SetSuffix(New : string);
var Anc : Double;
begin
  Anc := Value;
  FSuffix := New;
  if not (csReading in ComponentState) then Value := Anc;
end;

procedure TSvCustomNumberEdit.SetAllowNegative(New : boolean);
var Anc : Double;
begin
  Anc := Value;
  FAllowNegative := New;
  if not (csReading in ComponentState) then
    if New then Value := Anc else Value := Abs(Anc);
end;

procedure TSvCustomNumberEdit.SetAllowFrac(New : boolean);
var Anc : Double;
begin
  Anc := Value;
  FAllowFrac := New;
  if not (csReading in ComponentState) then
    if New then Value := Anc else Value := Round(Anc);
end;

procedure TSvCustomNumberEdit.SetSeparator(New : Char);
var Anc : Double;
begin
  Anc := Value;
  FSeparator := New;
  if not (csReading in ComponentState) then Value := Anc;
end;

procedure TSvCustomNumberEdit.SetHasMinValue(New : boolean);
begin
  if New = FHasMinValue then exit;
  FHasMinValue := New;
  MaxValue := MaxValue;
  Value := Value;
end;

procedure TSvCustomNumberEdit.SetHasMaxValue(New : boolean);
begin
  if New = FHasMaxValue then exit;
  FHasMaxValue := New;
  MinValue := MinValue;
  Value := Value;
end;

procedure TSvCustomNumberEdit.SetMinValue(New : Double);
begin
  if FHasMaxValue and (New > FMaxValue) then
    FMinValue := FMaxValue else FMinValue := New;
  Value := Value;
end;

procedure TSvCustomNumberEdit.SetMaxValue(New : Double);
begin
  if FHasMinValue and (New < FMinValue) then
    FMaxValue := FMinValue else FMaxValue := New;
  Value := Value;
end;

function TSvCustomNumberEdit.SelEnd : integer;
begin
  Result := SelStart + SelLength;
end;

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

procedure TSvCustomNumberEdit.KeyPress(var Key: Char);
var Debut, Fin : integer;
begin
  if ReadOnly then exit;
  Debut := Length(FPrefix);
  Fin := Length(Text)-Length(FSuffix);
  if (SelStart < Debut) or (SelEnd > Fin) then Key := #0 else
  if (SelStart = Debut) and (SelEnd = Debut) and (Key = Char(VK_Back)) then Key := #0 else
  if (Key = '-') and ((not FAllowNegative) or (SelStart > Debut) or (Text[SelStart+1] = '-')) then Key := #0 else
  if ((Key = '.') or (Key = ',')) and ((not AllowFrac) or (Pos(FSeparator, Copy(Text, Length(FPrefix)+1, Length(Text)-Length(FPrefix)-Length(FSuffix))) > 0)) then Key := #0 else
  if ((Key < '0') or (Key > '9')) and
     (Key <> '.') and (Key <> ',') and (Key <> '-') and
     (Key <> Char(VK_Back)) then Key := #0 else
  if (Key = '.') or (Key = ',') then Key := FSeparator;
end;

procedure TSvCustomNumberEdit.Loaded;
begin
  inherited;
  Value := FReadingValue;
end;

end.

