{*
  Importe l'unité Forms dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsForms;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Windows, Messages, Classes, Graphics, Controls, Menus,
  HelpIntfs, ActnList, MultiMon, Forms;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTControlScrollBar = class(TControlScrollBar)
  private
    function GetScrollPos: Integer;
    procedure SetButtonSize(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetParentColor(Value: Boolean);
    procedure SetPosition(Value: Integer);
    procedure SetRange(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TScrollBarStyle);
    procedure SetThumbSize(Value: Integer);
    procedure SetVisible(Value: Boolean);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScrollingWinControl = class(TScrollingWinControl)
  private
    procedure SetAutoScroll(Value: Boolean);
    procedure SetHorzScrollBar(Value: TControlScrollBar);
    procedure SetVertScrollBar(Value: TControlScrollBar);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScrollBox = class(TScrollBox)
  private
    procedure SetBorderStyle(Value: TBorderStyle);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomFrame = class(TCustomFrame)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTFrame = class(TFrame)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomForm = class(TCustomForm)
  private
    function GetActiveMDIChild: TForm;
    function GetCanvas: TCanvas;
    function GetLeft: Integer;
    function GetMDIChildCount: Integer;
    function GetMDIChildren(I: Integer): TForm;
    function GetMonitor: TMonitor;
    function GetPixelsPerInch: Integer;
    function GetPopupChildren: TList;
    function GetScaled: Boolean;
    function GetTop: Integer;
    procedure SetActiveControl(Control: TWinControl);
    procedure SetActiveOleControl(Control: TWinControl);
    procedure SetBorderIcons(Value: TBorderIcons);
    procedure SetBorderStyle(Value: TFormBorderStyle);
    procedure SetClientHeight(Value: Integer);
    procedure SetClientWidth(Value: Integer);
    procedure SetDesigner(ADesigner: IDesignerHook);
    procedure SetFormStyle(Value: TFormStyle);
    procedure SetIcon(Value: TIcon);
    procedure SetLeft(Value: Integer);
    procedure SetMenu(Value: TMainMenu);
    procedure SetPixelsPerInch(Value: Integer);
    procedure SetPosition(Value: TPosition);
    procedure SetPopupMode(Value: TPopupMode);
    procedure SetPopupParent(Value: TCustomForm);
    procedure SetScaled(Value: Boolean);
    procedure SetTop(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetWindowMenu(Value: TMenuItem);
    procedure SetObjectMenuItem(Value: TMenuItem);
    procedure SetWindowState(Value: TWindowState);
    procedure SetAlphaBlend(const Value: Boolean);
    procedure SetAlphaBlendValue(const Value: Byte);
    procedure SetTransparentColor(const Value: Boolean);
    procedure SetTransparentColorValue(const Value: TColor);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomActiveForm = class(TCustomActiveForm)
  private
    procedure SetAxBorderStyle(Value: TActiveFormBorderStyle);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTForm = class(TForm)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomDockForm = class(TCustomDockForm)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMonitor = class(TMonitor)
  private
    function GetLeft: Integer;
    function GetHeight: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    function GetBoundsRect: TRect;
    function GetWorkareaRect: TRect;
    function GetPrimary: Boolean;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScreen = class(TScreen)
  private
    function GetCustomFormCount: Integer;
    function GetCustomForms(Index: Integer): TCustomForm;
    function GetCursors(Index: Integer): HCURSOR;
    function GetDataModule(Index: Integer): TDataModule;
    function GetDataModuleCount: Integer;
    function GetDefaultIME: String;
    function GetDesktopTop: Integer;
    function GetDesktopLeft: Integer;
    function GetDesktopHeight: Integer;
    function GetDesktopWidth: Integer;
    function GetDesktopRect: TRect;
    function GetWorkAreaRect: TRect;
    function GetWorkAreaHeight: Integer;
    function GetWorkAreaLeft: Integer;
    function GetWorkAreaTop: Integer;
    function GetWorkAreaWidth: Integer;
    function GetImes: TStrings;
    function GetHeight: Integer;
    function GetMonitor(Index: Integer): TMonitor;
    function GetMonitorCount: Integer;
    function GetFonts: TStrings;
    function GetForm(Index: Integer): TForm;
    function GetFormCount: Integer;
    function GetWidth: Integer;
    procedure SetCursors(Index: Integer; Handle: HCURSOR);
    procedure SetCursor(Value: TCursor);
    procedure SetHintFont(Value: TFont);
    procedure SetIconFont(Value: TFont);
    procedure SetMenuFont(Value: TFont);
    function GetPrimaryMonitor: TMonitor;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTApplication = class(TApplication)
  private
    function GetCurrentHelpFile: string;
    function GetDialogHandle: HWND;
    function GetActiveFormHandle: HWND;
    function GetMainFormHandle: HWND;
    function GetExeName: string;
    function GetTitle: string;
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetDialogHandle(Value: HWnd);
    procedure SetHandle(Value: HWnd);
    procedure SetHint(const Value: string);
    procedure SetHintColor(Value: TColor);
    procedure SetIcon(Value: TIcon);
    procedure SetShowHint(Value: Boolean);
    procedure SetTitle(const Value: string);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{--------------------------}
{ TControlScrollBar import }
{--------------------------}

function TSepiImportsTControlScrollBar.GetScrollPos: Integer;
begin
  Result := ScrollPos;
end;

procedure TSepiImportsTControlScrollBar.SetButtonSize(Value: Integer);
begin
  ButtonSize := Value;
end;

procedure TSepiImportsTControlScrollBar.SetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TSepiImportsTControlScrollBar.SetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TSepiImportsTControlScrollBar.SetPosition(Value: Integer);
begin
  Position := Value;
end;

procedure TSepiImportsTControlScrollBar.SetRange(Value: Integer);
begin
  Range := Value;
end;

procedure TSepiImportsTControlScrollBar.SetSize(Value: Integer);
begin
  Size := Value;
end;

procedure TSepiImportsTControlScrollBar.SetStyle(Value: TScrollBarStyle);
begin
  Style := Value;
end;

procedure TSepiImportsTControlScrollBar.SetThumbSize(Value: Integer);
begin
  ThumbSize := Value;
end;

procedure TSepiImportsTControlScrollBar.SetVisible(Value: Boolean);
begin
  Visible := Value;
end;

class function TSepiImportsTControlScrollBar.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TControlScrollBar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FControl', System.TypeInfo(TScrollingWinControl));
    AddField('FIncrement', System.TypeInfo(TScrollBarInc));
    AddField('FPageIncrement', System.TypeInfo(TScrollbarInc));
    AddField('FPosition', System.TypeInfo(Integer));
    AddField('FRange', System.TypeInfo(Integer));
    AddField('FCalcRange', System.TypeInfo(Integer));
    AddField('FKind', System.TypeInfo(TScrollBarKind));
    AddField('FMargin', System.TypeInfo(Word));
    AddField('FVisible', System.TypeInfo(Boolean));
    AddField('FTracking', System.TypeInfo(Boolean));
    AddField('FScaled', System.TypeInfo(Boolean));
    AddField('FSmooth', System.TypeInfo(Boolean));
    AddField('FDelay', System.TypeInfo(Integer));
    AddField('FButtonSize', System.TypeInfo(Integer));
    AddField('FColor', System.TypeInfo(TColor));
    AddField('FParentColor', System.TypeInfo(Boolean));
    AddField('FSize', System.TypeInfo(Integer));
    AddField('FStyle', System.TypeInfo(TScrollBarStyle));
    AddField('FThumbSize', System.TypeInfo(Integer));
    AddField('FPageDiv', System.TypeInfo(Integer));
    AddField('FLineDiv', System.TypeInfo(Integer));
    AddField('FUpdateNeeded', System.TypeInfo(Boolean));

    AddMethod('Create', nil,
      'constructor(AControl: TScrollingWinControl; AKind: TScrollBarKind)');
    AddMethod('CalcAutoRange', nil,
      'procedure');
    AddMethod('ControlSize', nil,
      'function(ControlSB, AssumeSB: Boolean): Integer');
    AddMethod('DoSetRange', nil,
      'procedure(Value: Integer)');
    AddMethod('GetScrollPos', @TSepiImportsTControlScrollBar.GetScrollPos,
      'function: Integer');
    AddMethod('NeedsScrollBarVisible', nil,
      'function: Boolean');
    AddMethod('IsIncrementStored', nil,
      'function: Boolean');
    AddMethod('ScrollMessage', nil,
      'procedure(var Msg: TWMScroll)');
    AddMethod('SetButtonSize', @TSepiImportsTControlScrollBar.SetButtonSize,
      'procedure(Value: Integer)');
    AddMethod('SetColor', @TSepiImportsTControlScrollBar.SetColor,
      'procedure(Value: TColor)');
    AddMethod('SetParentColor', @TSepiImportsTControlScrollBar.SetParentColor,
      'procedure(Value: Boolean)');
    AddMethod('SetPosition', @TSepiImportsTControlScrollBar.SetPosition,
      'procedure(Value: Integer)');
    AddMethod('SetRange', @TSepiImportsTControlScrollBar.SetRange,
      'procedure(Value: Integer)');
    AddMethod('SetSize', @TSepiImportsTControlScrollBar.SetSize,
      'procedure(Value: Integer)');
    AddMethod('SetStyle', @TSepiImportsTControlScrollBar.SetStyle,
      'procedure(Value: TScrollBarStyle)');
    AddMethod('SetThumbSize', @TSepiImportsTControlScrollBar.SetThumbSize,
      'procedure(Value: Integer)');
    AddMethod('SetVisible', @TSepiImportsTControlScrollBar.SetVisible,
      'procedure(Value: Boolean)');
    AddMethod('IsRangeStored', nil,
      'function: Boolean');
    AddMethod('Update', nil,
      'procedure(ControlSB, AssumeSB: Boolean)');

    CurrentVisibility := mvPublic;

    AddMethod('Assign', @TSepiImportsTControlScrollBar.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('ChangeBiDiPosition', @TSepiImportsTControlScrollBar.ChangeBiDiPosition,
      'procedure');

    AddProperty('Kind', 'property: TScrollBarKind',
      'FKind', '');

    AddMethod('IsScrollBarVisible', @TSepiImportsTControlScrollBar.IsScrollBarVisible,
      'function: Boolean');

    AddProperty('ScrollPos', 'property: Integer',
      'GetScrollPos', '');

    CurrentVisibility := mvPublished;

    AddProperty('ButtonSize', 'property: Integer',
      'FButtonSize', 'SetButtonSize',
      NoIndex, 0);
    AddProperty('Color', 'property: TColor',
      'FColor', 'SetColor',
      NoIndex, clBtnHighlight);
    AddProperty('Increment', 'property: TScrollBarInc',
      'FIncrement', 'FIncrement',
      NoIndex, 8, 'IsIncrementStored');
    AddProperty('Margin', 'property: Word',
      'FMargin', 'FMargin',
      NoIndex, 0);
    AddProperty('ParentColor', 'property: Boolean',
      'FParentColor', 'SetParentColor',
      NoIndex, Integer(True));
    AddProperty('Position', 'property: Integer',
      'FPosition', 'SetPosition',
      NoIndex, 0);
    AddProperty('Range', 'property: Integer',
      'FRange', 'SetRange',
      NoIndex, 0, 'IsRangeStored');
    AddProperty('Smooth', 'property: Boolean',
      'FSmooth', 'FSmooth',
      NoIndex, Integer(False));
    AddProperty('Size', 'property: Integer',
      'FSize', 'SetSize',
      NoIndex, 0);
    AddProperty('Style', 'property: TScrollBarStyle',
      'FStyle', 'SetStyle',
      NoIndex, Integer(ssRegular));
    AddProperty('ThumbSize', 'property: Integer',
      'FThumbSize', 'SetThumbSize',
      NoIndex, 0);
    AddProperty('Tracking', 'property: Boolean',
      'FTracking', 'FTracking',
      NoIndex, Integer(False));
    AddProperty('Visible', 'property: Boolean',
      'FVisible', 'SetVisible',
      NoIndex, Integer(True));

    Complete;
  end;
end;

{-----------------------------}
{ TScrollingWinControl import }
{-----------------------------}

procedure TSepiImportsTScrollingWinControl.SetAutoScroll(Value: Boolean);
begin
  AutoScroll := Value;
end;

procedure TSepiImportsTScrollingWinControl.SetHorzScrollBar(Value: TControlScrollBar);
begin
  HorzScrollBar := Value;
end;

procedure TSepiImportsTScrollingWinControl.SetVertScrollBar(Value: TControlScrollBar);
begin
  VertScrollBar := Value;
end;

class function TSepiImportsTScrollingWinControl.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TScrollingWinControl'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TScrollingWinControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHorzScrollBar', System.TypeInfo(TControlScrollBar));
    AddField('FVertScrollBar', System.TypeInfo(TControlScrollBar));
    AddField('FAutoScroll', System.TypeInfo(Boolean));
    AddField('FAutoRangeCount', System.TypeInfo(Integer));
    AddField('FUpdatingScrollBars', System.TypeInfo(Boolean));

    AddMethod('CalcAutoRange', nil,
      'procedure');
    AddMethod('ScaleScrollBars', nil,
      'procedure(M, D: Integer)');
    AddMethod('SetAutoScroll', @TSepiImportsTScrollingWinControl.SetAutoScroll,
      'procedure(Value: Boolean)');
    AddMethod('SetHorzScrollBar', @TSepiImportsTScrollingWinControl.SetHorzScrollBar,
      'procedure(Value: TControlScrollBar)');
    AddMethod('SetVertScrollBar', @TSepiImportsTScrollingWinControl.SetVertScrollBar,
      'procedure(Value: TControlScrollBar)');
    AddMethod('UpdateScrollBars', nil,
      'procedure');
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('WMHScroll', nil,
      'procedure(var Message: TWMHScroll)',
      mlkMessage, False, WM_HSCROLL);
    AddMethod('WMVScroll', nil,
      'procedure(var Message: TWMVScroll)',
      mlkMessage, False, WM_VSCROLL);
    AddMethod('CMBiDiModeChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BIDIMODECHANGED);

    CurrentVisibility := mvProtected;

    AddMethod('AdjustClientRect', @TSepiImportsTScrollingWinControl.AdjustClientRect,
      'procedure(var Rect: TRect)',
      mlkOverride);
    AddMethod('AlignControls', @TSepiImportsTScrollingWinControl.AlignControls,
      'procedure(AControl: TControl; var ARect: TRect)',
      mlkOverride);
    AddMethod('AutoScrollEnabled', @TSepiImportsTScrollingWinControl.AutoScrollEnabled,
      'function: Boolean',
      mlkVirtual);
    AddMethod('AutoScrollInView', @TSepiImportsTScrollingWinControl.AutoScrollInView,
      'procedure(AControl: TControl)',
      mlkVirtual);
    AddMethod('ChangeScale', @TSepiImportsTScrollingWinControl.ChangeScale,
      'procedure(M, D: Integer)',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTScrollingWinControl.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTScrollingWinControl.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DoFlipChildren', @TSepiImportsTScrollingWinControl.DoFlipChildren,
      'procedure',
      mlkOverride);

    AddProperty('AutoScroll', 'property: Boolean',
      'FAutoScroll', 'SetAutoScroll',
      NoIndex, Integer(True));

    AddMethod('Resizing', @TSepiImportsTScrollingWinControl.Resizing,
      'procedure(State: TWindowState)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScrollingWinControl.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTScrollingWinControl.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('DisableAutoRange', @TSepiImportsTScrollingWinControl.DisableAutoRange,
      'procedure');
    AddMethod('EnableAutoRange', @TSepiImportsTScrollingWinControl.EnableAutoRange,
      'procedure');
    AddMethod('ScrollInView', @TSepiImportsTScrollingWinControl.ScrollInView,
      'procedure(AControl: TControl)');

    CurrentVisibility := mvPublished;

    RedefineProperty('OnAlignInsertBefore');
    RedefineProperty('OnAlignPosition');
    AddProperty('HorzScrollBar', 'property: TControlScrollBar',
      'FHorzScrollBar', 'SetHorzScrollBar');
    AddProperty('VertScrollBar', 'property: TControlScrollBar',
      'FVertScrollBar', 'SetVertScrollBar');

    Complete;
  end;
end;

{-------------------}
{ TScrollBox import }
{-------------------}

procedure TSepiImportsTScrollBox.SetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle := Value;
end;

class function TSepiImportsTScrollBox.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScrollBox));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FBorderStyle', System.TypeInfo(TBorderStyle));

    AddMethod('SetBorderStyle', @TSepiImportsTScrollBox.SetBorderStyle,
      'procedure(Value: TBorderStyle)');
    AddMethod('WMNCHitTest', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_NCHITTEST);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTScrollBox.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('PaintWindow', @TSepiImportsTScrollBox.PaintWindow,
      'procedure(DC: HDC)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScrollBox.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('AutoScroll');
    RedefineProperty('AutoSize');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelOuter');
    RedefineProperty('BevelKind');
    RedefineProperty('BevelWidth');
    RedefineProperty('BiDiMode');
    AddProperty('BorderStyle', 'property: TBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(bsSingle));
    RedefineProperty('Constraints');
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Color',
      '', '', NoDefaultValue);
    RedefineProperty('Ctl3D');
    RedefineProperty('Font');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentBackground',
      '', '', Integer(False));
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('OnCanResize');
    RedefineProperty('OnClick');
    RedefineProperty('OnConstrainedResize');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnGetSiteInfo');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnMouseWheel');
    RedefineProperty('OnMouseWheelDown');
    RedefineProperty('OnMouseWheelUp');
    RedefineProperty('OnResize');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{---------------------}
{ TCustomFrame import }
{---------------------}

class function TSepiImportsTCustomFrame.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomFrame));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTCustomFrame.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('GetChildren', @TSepiImportsTCustomFrame.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('SetParent', @TSepiImportsTCustomFrame.SetParent,
      'procedure(AParent: TWinControl)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomFrame.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    Complete;
  end;
end;

{---------------}
{ TFrame import }
{---------------}

class function TSepiImportsTFrame.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TFrame));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('AutoScroll');
    RedefineProperty('AutoSize');
    RedefineProperty('BiDiMode');
    RedefineProperty('Constraints');
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Color',
      '', '', NoDefaultValue);
    RedefineProperty('Ctl3D');
    RedefineProperty('Font');
    RedefineProperty('ParentBackground',
      '', '', Integer(True));
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('OnAlignInsertBefore');
    RedefineProperty('OnAlignPosition');
    RedefineProperty('OnCanResize');
    RedefineProperty('OnClick');
    RedefineProperty('OnConstrainedResize');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnGetSiteInfo');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnMouseWheel');
    RedefineProperty('OnMouseWheelDown');
    RedefineProperty('OnMouseWheelUp');
    RedefineProperty('OnResize');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{----------------------}
{ IDesignerHook import }
{----------------------}

function SepiImportIDesignerHook(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDesignerHook));

  with Result do
  begin
    AddMethod('GetCustomForm',
      'function: TCustomForm', ccRegister);
    AddMethod('SetCustomForm',
      'procedure(Value: TCustomForm)', ccRegister);
    AddMethod('GetIsControl',
      'function: Boolean', ccRegister);
    AddMethod('SetIsControl',
      'procedure(Value: Boolean)', ccRegister);
    AddMethod('IsDesignMsg',
      'function(Sender: TControl; var Message: TMessage): Boolean', ccRegister);
    AddMethod('PaintGrid',
      'procedure', ccRegister);
    AddMethod('PaintMenu',
      'procedure', ccRegister);
    AddMethod('ValidateRename',
      'procedure(AComponent: TComponent; const CurName, NewName: string )', ccRegister);
    AddMethod('UniqueName',
      'function(const BaseName: string): string', ccRegister);
    AddMethod('GetRoot',
      'function: TComponent', ccRegister);
    AddProperty('IsControl', 'property: Boolean',
      'GetIsControl', 'SetIsControl');
    AddProperty('Form', 'property: TCustomForm',
      'GetCustomForm', 'SetCustomForm');

    Complete;
  end;
end;

{-----------------}
{ IOleForm import }
{-----------------}

function SepiImportIOleForm(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IOleForm));

  with Result do
  begin
    AddMethod('OnDestroy',
      'procedure', ccRegister);
    AddMethod('OnResize',
      'procedure', ccRegister);

    Complete;
  end;
end;

{------------------}
{ TPopupWnd import }
{------------------}

function SepiImportTPopupWnd(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TPopupWnd', False, True);

  with Result do
  begin
    AddField('ID', System.TypeInfo(Integer));
    AddField('ControlWnd', System.TypeInfo(HWND));

    Complete;
  end;
end;

{--------------------}
{ TCustomForm import }
{--------------------}

function TSepiImportsTCustomForm.GetActiveMDIChild: TForm;
begin
  Result := ActiveMDIChild;
end;

function TSepiImportsTCustomForm.GetCanvas: TCanvas;
begin
  Result := Canvas;
end;

function TSepiImportsTCustomForm.GetLeft: Integer;
begin
  Result := Left;
end;

function TSepiImportsTCustomForm.GetMDIChildCount: Integer;
begin
  Result := MDIChildCount;
end;

function TSepiImportsTCustomForm.GetMDIChildren(I: Integer): TForm;
begin
  Result := MDIChildren[I];
end;

function TSepiImportsTCustomForm.GetMonitor: TMonitor;
begin
  Result := Monitor;
end;

function TSepiImportsTCustomForm.GetPixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TSepiImportsTCustomForm.GetPopupChildren: TList;
begin
  Result := PopupChildren;
end;

function TSepiImportsTCustomForm.GetScaled: Boolean;
begin
  Result := Scaled;
end;

function TSepiImportsTCustomForm.GetTop: Integer;
begin
  Result := Top;
end;

procedure TSepiImportsTCustomForm.SetActiveControl(Control: TWinControl);
begin
  ActiveControl := Control;
end;

procedure TSepiImportsTCustomForm.SetActiveOleControl(Control: TWinControl);
begin
  ActiveOleControl := Control;
end;

procedure TSepiImportsTCustomForm.SetBorderIcons(Value: TBorderIcons);
begin
  BorderIcons := Value;
end;

procedure TSepiImportsTCustomForm.SetBorderStyle(Value: TFormBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TSepiImportsTCustomForm.SetClientHeight(Value: Integer);
begin
  ClientHeight := Value;
end;

procedure TSepiImportsTCustomForm.SetClientWidth(Value: Integer);
begin
  ClientWidth := Value;
end;

procedure TSepiImportsTCustomForm.SetDesigner(ADesigner: IDesignerHook);
begin
  Designer := ADesigner;
end;

procedure TSepiImportsTCustomForm.SetFormStyle(Value: TFormStyle);
begin
  FormStyle := Value;
end;

procedure TSepiImportsTCustomForm.SetIcon(Value: TIcon);
begin
  Icon := Value;
end;

procedure TSepiImportsTCustomForm.SetLeft(Value: Integer);
begin
  Left := Value;
end;

procedure TSepiImportsTCustomForm.SetMenu(Value: TMainMenu);
begin
  Menu := Value;
end;

procedure TSepiImportsTCustomForm.SetPixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TSepiImportsTCustomForm.SetPosition(Value: TPosition);
begin
  Position := Value;
end;

procedure TSepiImportsTCustomForm.SetPopupMode(Value: TPopupMode);
begin
  PopupMode := Value;
end;

procedure TSepiImportsTCustomForm.SetPopupParent(Value: TCustomForm);
begin
  PopupParent := Value;
end;

procedure TSepiImportsTCustomForm.SetScaled(Value: Boolean);
begin
  Scaled := Value;
end;

procedure TSepiImportsTCustomForm.SetTop(Value: Integer);
begin
  Top := Value;
end;

procedure TSepiImportsTCustomForm.SetVisible(Value: Boolean);
begin
  Visible := Value;
end;

procedure TSepiImportsTCustomForm.SetWindowMenu(Value: TMenuItem);
begin
  WindowMenu := Value;
end;

procedure TSepiImportsTCustomForm.SetObjectMenuItem(Value: TMenuItem);
begin
  ObjectMenuItem := Value;
end;

procedure TSepiImportsTCustomForm.SetWindowState(Value: TWindowState);
begin
  WindowState := Value;
end;

procedure TSepiImportsTCustomForm.SetAlphaBlend(const Value: Boolean);
begin
  AlphaBlend := Value;
end;

procedure TSepiImportsTCustomForm.SetAlphaBlendValue(const Value: Byte);
begin
  AlphaBlendValue := Value;
end;

procedure TSepiImportsTCustomForm.SetTransparentColor(const Value: Boolean);
begin
  TransparentColor := Value;
end;

procedure TSepiImportsTCustomForm.SetTransparentColorValue(const Value: TColor);
begin
  TransparentColorValue := Value;
end;

class function TSepiImportsTCustomForm.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
const
  DefaultBorderIcons : TBorderIcons = [biSystemMenu, biMinimize, biMaximize];
begin
  Result := TSepiClass(Owner.FindMeta('TCustomForm'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomForm));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FActiveControl', System.TypeInfo(TWinControl));
    AddField('FFocusedControl', System.TypeInfo(TWinControl));
    AddField('FBorderIcons', System.TypeInfo(TBorderIcons));
    AddField('FBorderStyle', System.TypeInfo(TFormBorderStyle));
    AddField('FSizeChanging', System.TypeInfo(Boolean));
    AddField('FWindowState', System.TypeInfo(TWindowState));
    AddField('FShowAction', System.TypeInfo(TShowAction));
    AddField('FKeyPreview', System.TypeInfo(Boolean));
    AddField('FActive', System.TypeInfo(Boolean));
    AddField('FFormStyle', System.TypeInfo(TFormStyle));
    AddField('FPosition', System.TypeInfo(TPosition));
    AddField('FDefaultMonitor', System.TypeInfo(TDefaultMonitor));
    AddField('FTileMode', System.TypeInfo(TTileMode));
    AddField('FDropTarget', System.TypeInfo(Boolean));
    AddField('FOldCreateOrder', System.TypeInfo(Boolean));
    AddField('FPrintScale', System.TypeInfo(TPrintScale));
    AddField('FCanvas', System.TypeInfo(TControlCanvas));
    AddField('FHelpFile', System.TypeInfo(string));
    AddField('FIcon', System.TypeInfo(TIcon));
    AddField('FInCMParentBiDiModeChanged', System.TypeInfo(Boolean));
    AddField('FMenu', System.TypeInfo(TMainMenu));
    AddField('FModalResult', System.TypeInfo(TModalResult));
    AddField('FDesigner', System.TypeInfo(IDesignerHook));
    AddField('FClientHandle', System.TypeInfo(HWND));
    AddField('FWindowMenu', System.TypeInfo(TMenuItem));
    AddField('FPixelsPerInch', System.TypeInfo(Integer));
    AddField('FObjectMenuItem', System.TypeInfo(TMenuItem));
    AddField('FOleForm', System.TypeInfo(IOleForm));
    AddField('FClientWidth', System.TypeInfo(Integer));
    AddField('FClientHeight', System.TypeInfo(Integer));
    AddField('FTextHeight', System.TypeInfo(Integer));
    AddField('FDefClientProc', 'TFarProc');
    AddField('FClientInstance', 'TFarProc');
    AddField('FActiveOleControl', System.TypeInfo(TWinControl));
    AddField('FSavedBorderStyle', System.TypeInfo(TFormBorderStyle));
    AddField('FOnActivate', System.TypeInfo(TNotifyEvent));
    AddField('FOnClose', System.TypeInfo(TCloseEvent));
    AddField('FOnCloseQuery', System.TypeInfo(TCloseQueryEvent));
    AddField('FOnDeactivate', System.TypeInfo(TNotifyEvent));
    AddField('FOnHelp', System.TypeInfo(THelpEvent));
    AddField('FOnHide', System.TypeInfo(TNotifyEvent));
    AddField('FOnPaint', System.TypeInfo(TNotifyEvent));
    AddField('FOnShortCut', System.TypeInfo(TShortCutEvent));
    AddField('FOnShow', System.TypeInfo(TNotifyEvent));
    AddField('FOnCreate', System.TypeInfo(TNotifyEvent));
    AddField('FOnDestroy', System.TypeInfo(TNotifyEvent));
    AddField('FAlphaBlend', System.TypeInfo(Boolean));
    AddField('FAlphaBlendValue', System.TypeInfo(Byte));
    AddField('FPopupChildren', System.TypeInfo(TList));
    AddField('FPopupMode', System.TypeInfo(TPopupMode));
    AddField('FPopupParent', System.TypeInfo(TCustomForm));
    AddField('FRecreateChildren', System.TypeInfo(TList));
    AddField('FPopupWnds', System.TypeInfo(TPopupWndArray));
    AddField('FInternalPopupParent', System.TypeInfo(TCustomForm));
    AddField('FInternalPopupParentWnd', System.TypeInfo(HWND));
    AddField('FScreenSnap', System.TypeInfo(Boolean));
    AddField('FSnapBuffer', System.TypeInfo(Integer));
    AddField('FTransparentColor', System.TypeInfo(Boolean));
    AddField('FTransparentColorValue', System.TypeInfo(TColor));

    AddMethod('RefreshMDIMenu', nil,
      'procedure');
    AddMethod('ClientWndProc', nil,
      'procedure(var Message: TMessage)');
    AddMethod('GetActiveMDIChild', @TSepiImportsTCustomForm.GetActiveMDIChild,
      'function: TForm');
    AddMethod('GetCanvas', @TSepiImportsTCustomForm.GetCanvas,
      'function: TCanvas');
    AddMethod('GetIconHandle', nil,
      'function: HICON');
    AddMethod('GetLeft', @TSepiImportsTCustomForm.GetLeft,
      'function: Integer');
    AddMethod('GetMDIChildCount', @TSepiImportsTCustomForm.GetMDIChildCount,
      'function: Integer');
    AddMethod('GetMDIChildren', @TSepiImportsTCustomForm.GetMDIChildren,
      'function(I: Integer): TForm');
    AddMethod('GetMonitor', @TSepiImportsTCustomForm.GetMonitor,
      'function: TMonitor');
    AddMethod('GetPixelsPerInch', @TSepiImportsTCustomForm.GetPixelsPerInch,
      'function: Integer');
    AddMethod('GetPopupChildren', @TSepiImportsTCustomForm.GetPopupChildren,
      'function: TList');
    AddMethod('GetRecreateChildren', nil,
      'function: TList');
    AddMethod('GetScaled', @TSepiImportsTCustomForm.GetScaled,
      'function: Boolean');
    AddMethod('GetTextHeight', nil,
      'function: Integer');
    AddMethod('GetTop', @TSepiImportsTCustomForm.GetTop,
      'function: Integer');
    AddMethod('IconChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('IsAutoScrollStored', nil,
      'function: Boolean');
    AddMethod('IsClientSizeStored', nil,
      'function: Boolean');
    AddMethod('IsForm', nil,
      'function: Boolean');
    AddMethod('IsFormSizeStored', nil,
      'function: Boolean');
    AddMethod('IsIconStored', nil,
      'function: Boolean');
    AddMethod('MergeMenu', nil,
      'procedure(MergeState: Boolean)');
    AddMethod('ReadIgnoreFontProperty', nil,
      'procedure(Reader: TReader)');
    AddMethod('ReadTextHeight', nil,
      'procedure(Reader: TReader)');
    AddMethod('SetActive', nil,
      'procedure(Value: Boolean)');
    AddMethod('SetActiveControl', @TSepiImportsTCustomForm.SetActiveControl,
      'procedure(Control: TWinControl)');
    AddMethod('SetActiveOleControl', @TSepiImportsTCustomForm.SetActiveOleControl,
      'procedure(Control: TWinControl)');
    AddMethod('SetBorderIcons', @TSepiImportsTCustomForm.SetBorderIcons,
      'procedure(Value: TBorderIcons)');
    AddMethod('SetBorderStyle', @TSepiImportsTCustomForm.SetBorderStyle,
      'procedure(Value: TFormBorderStyle)');
    AddMethod('SetClientHeight', @TSepiImportsTCustomForm.SetClientHeight,
      'procedure(Value: Integer)');
    AddMethod('SetClientWidth', @TSepiImportsTCustomForm.SetClientWidth,
      'procedure(Value: Integer)');
    AddMethod('SetDesigner', @TSepiImportsTCustomForm.SetDesigner,
      'procedure(ADesigner: IDesignerHook)');
    AddMethod('SetFormStyle', @TSepiImportsTCustomForm.SetFormStyle,
      'procedure(Value: TFormStyle)');
    AddMethod('SetIcon', @TSepiImportsTCustomForm.SetIcon,
      'procedure(Value: TIcon)');
    AddMethod('SetLeft', @TSepiImportsTCustomForm.SetLeft,
      'procedure(Value: Integer)');
    AddMethod('SetMenu', @TSepiImportsTCustomForm.SetMenu,
      'procedure(Value: TMainMenu)');
    AddMethod('SetPixelsPerInch', @TSepiImportsTCustomForm.SetPixelsPerInch,
      'procedure(Value: Integer)');
    AddMethod('SetPosition', @TSepiImportsTCustomForm.SetPosition,
      'procedure(Value: TPosition)');
    AddMethod('SetPopupMode', @TSepiImportsTCustomForm.SetPopupMode,
      'procedure(Value: TPopupMode)');
    AddMethod('SetPopupParent', @TSepiImportsTCustomForm.SetPopupParent,
      'procedure(Value: TCustomForm)');
    AddMethod('SetScaled', @TSepiImportsTCustomForm.SetScaled,
      'procedure(Value: Boolean)');
    AddMethod('SetTop', @TSepiImportsTCustomForm.SetTop,
      'procedure(Value: Integer)');
    AddMethod('SetVisible', @TSepiImportsTCustomForm.SetVisible,
      'procedure(Value: Boolean)');
    AddMethod('SetWindowFocus', nil,
      'procedure');
    AddMethod('SetWindowMenu', @TSepiImportsTCustomForm.SetWindowMenu,
      'procedure(Value: TMenuItem)');
    AddMethod('SetObjectMenuItem', @TSepiImportsTCustomForm.SetObjectMenuItem,
      'procedure(Value: TMenuItem)');
    AddMethod('SetWindowState', @TSepiImportsTCustomForm.SetWindowState,
      'procedure(Value: TWindowState)');
    AddMethod('SetWindowToMonitor', nil,
      'procedure');
    AddMethod('WritePixelsPerInch', nil,
      'procedure(Writer: TWriter)');
    AddMethod('WriteTextHeight', nil,
      'procedure(Writer: TWriter)');
    AddMethod('NormalColor', nil,
      'function: TColor');
    AddMethod('WMPaint', nil,
      'procedure(var Message: TWMPaint)',
      mlkMessage, False, WM_PAINT);
    AddMethod('WMNCPaint', nil,
      'procedure(var Message: TWMNCPaint)',
      mlkMessage, False, WM_NCPAINT);
    AddMethod('WMEraseBkgnd', nil,
      'procedure(var Message: TWMEraseBkgnd)',
      mlkMessage, False, WM_ERASEBKGND);
    AddMethod('WMIconEraseBkgnd', nil,
      'procedure(var Message: TWMEraseBkgnd)',
      mlkMessage, False, WM_ICONERASEBKGND);
    AddMethod('WMQueryDragIcon', nil,
      'procedure(var Message: TWMQueryDragIcon)',
      mlkMessage, False, WM_QUERYDRAGICON);
    AddMethod('WMNCCreate', nil,
      'procedure(var Message: TWMNCCreate)',
      mlkMessage, False, WM_NCCREATE);
    AddMethod('WMNCHitTest', nil,
      'procedure(var Message: TWMNCHitTest)',
      mlkMessage, False, WM_NCHITTEST);
    AddMethod('WMNCLButtonDown', nil,
      'procedure(var Message: TWMNCLButtonDown)',
      mlkMessage, False, WM_NCLBUTTONDOWN);
    AddMethod('WMDestroy', nil,
      'procedure(var Message: TWMDestroy)',
      mlkMessage, False, WM_DESTROY);
    AddMethod('WMCommand', nil,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, WM_COMMAND);
    AddMethod('WMInitMenuPopup', nil,
      'procedure(var Message: TWMInitMenuPopup)',
      mlkMessage, False, WM_INITMENUPOPUP);
    AddMethod('WMMenuChar', nil,
      'procedure(var Message: TWMMenuChar)',
      mlkMessage, False, WM_MENUCHAR);
    AddMethod('WMMenuSelect', nil,
      'procedure(var Message: TWMMenuSelect)',
      mlkMessage, False, WM_MENUSELECT);
    AddMethod('WMActivate', nil,
      'procedure(var Message: TWMActivate)',
      mlkMessage, False, WM_ACTIVATE);
    AddMethod('WMClose', nil,
      'procedure(var Message: TWMClose)',
      mlkMessage, False, WM_CLOSE);
    AddMethod('WMQueryEndSession', nil,
      'procedure(var Message: TWMQueryEndSession)',
      mlkMessage, False, WM_QUERYENDSESSION);
    AddMethod('WMSysCommand', nil,
      'procedure(var Message: TWMSysCommand)',
      mlkMessage, False, WM_SYSCOMMAND);
    AddMethod('WMShowWindow', nil,
      'procedure(var Message: TWMShowWindow)',
      mlkMessage, False, WM_SHOWWINDOW);
    AddMethod('WMMDIActivate', nil,
      'procedure(var Message: TWMMDIActivate)',
      mlkMessage, False, WM_MDIACTIVATE);
    AddMethod('WMNextDlgCtl', nil,
      'procedure(var Message: TWMNextDlgCtl)',
      mlkMessage, False, WM_NEXTDLGCTL);
    AddMethod('WMEnterMenuLoop', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_ENTERMENULOOP);
    AddMethod('WMHelp', nil,
      'procedure(var Message: TWMHelp)',
      mlkMessage, False, WM_HELP);
    AddMethod('WMGetMinMaxInfo', nil,
      'procedure(var Message: TWMGetMinMaxInfo)',
      mlkMessage, False, WM_GETMINMAXINFO);
    AddMethod('WMSettingChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_SETTINGCHANGE);
    AddMethod('WMWindowPosChanging', nil,
      'procedure(var Message: TWMWindowPosChanging)',
      mlkMessage, False, WM_WINDOWPOSCHANGING);
    AddMethod('WMNCCalcSize', nil,
      'procedure(var Message: TWMNCCalcSize)',
      mlkMessage, False, WM_NCCALCSIZE);
    AddMethod('CMActionExecute', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ACTIONEXECUTE);
    AddMethod('CMActionUpdate', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ACTIONUPDATE);
    AddMethod('CMActivate', nil,
      'procedure(var Message: TCMActivate)',
      mlkMessage, False, CM_ACTIVATE);
    AddMethod('CMAppSysCommand', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_APPSYSCOMMAND);
    AddMethod('CMBiDiModeChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BIDIMODECHANGED);
    AddMethod('CMDeactivate', nil,
      'procedure(var Message: TCMDeactivate)',
      mlkMessage, False, CM_DEACTIVATE);
    AddMethod('CMDialogKey', nil,
      'procedure(var Message: TCMDialogKey)',
      mlkMessage, False, CM_DIALOGKEY);
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CMMenuChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_MENUCHANGED);
    AddMethod('CMShowingChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SHOWINGCHANGED);
    AddMethod('CMIconChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ICONCHANGED);
    AddMethod('CMRelease', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_RELEASE);
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);
    AddMethod('CMUIActivate', nil,
      'procedure(var Message)',
      mlkMessage, False, CM_UIACTIVATE);
    AddMethod('CMParentBiDiModeChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTBIDIMODECHANGED);
    AddMethod('CMParentFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTFONTCHANGED);
    AddMethod('CMPopupHwndDestroy', nil,
      'procedure(var Message: TCMPopupHWndDestroy)',
      mlkMessage, False, CM_POPUPHWNDDESTROY);
    AddMethod('CMIsShortCut', nil,
      'procedure(var Message: TWMKey)',
      mlkMessage, False, CM_ISSHORTCUT);
    AddMethod('SetLayeredAttribs', nil,
      'procedure');
    AddMethod('SetAlphaBlend', @TSepiImportsTCustomForm.SetAlphaBlend,
      'procedure(const Value: Boolean)');
    AddMethod('SetAlphaBlendValue', @TSepiImportsTCustomForm.SetAlphaBlendValue,
      'procedure(const Value: Byte)');
    AddMethod('SetTransparentColor', @TSepiImportsTCustomForm.SetTransparentColor,
      'procedure(const Value: Boolean)');
    AddMethod('SetTransparentColorValue', @TSepiImportsTCustomForm.SetTransparentColorValue,
      'procedure(const Value: TColor)');
    AddMethod('InitAlphaBlending', nil,
      'procedure(var Params: TCreateParams)');

    CurrentVisibility := mvProtected;

    AddField('FFormState', System.TypeInfo(TFormState));

    AddMethod('Activate', @TSepiImportsTCustomForm.Activate,
      'procedure',
      mlkDynamic);
    AddMethod('ActiveChanged', @TSepiImportsTCustomForm.ActiveChanged,
      'procedure',
      mlkDynamic);
    AddMethod('AlignControls', @TSepiImportsTCustomForm.AlignControls,
      'procedure(AControl: TControl; var Rect: TRect)',
      mlkOverride);
    AddMethod('BeginAutoDrag', @TSepiImportsTCustomForm.BeginAutoDrag,
      'procedure',
      mlkOverride);
    AddMethod('ChangeScale', @TSepiImportsTCustomForm.ChangeScale,
      'procedure(M, D: Integer)',
      mlkOverride);
    AddMethod('CloseModal', @TSepiImportsTCustomForm.CloseModal,
      'procedure');
    AddMethod('CreateParams', @TSepiImportsTCustomForm.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWindowHandle', @TSepiImportsTCustomForm.CreateWindowHandle,
      'procedure(const Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomForm.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('Deactivate', @TSepiImportsTCustomForm.Deactivate,
      'procedure',
      mlkDynamic);
    AddMethod('DefineProperties', @TSepiImportsTCustomForm.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('DestroyHandle', @TSepiImportsTCustomForm.DestroyHandle,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWindowHandle', @TSepiImportsTCustomForm.DestroyWindowHandle,
      'procedure',
      mlkOverride);
    AddMethod('DoClose', @TSepiImportsTCustomForm.DoClose,
      'procedure(var Action: TCloseAction)',
      mlkDynamic);
    AddMethod('DoCreate', @TSepiImportsTCustomForm.DoCreate,
      'procedure',
      mlkVirtual);
    AddMethod('DoDestroy', @TSepiImportsTCustomForm.DoDestroy,
      'procedure',
      mlkVirtual);
    AddMethod('DoHide', @TSepiImportsTCustomForm.DoHide,
      'procedure',
      mlkDynamic);
    AddMethod('DoShow', @TSepiImportsTCustomForm.DoShow,
      'procedure',
      mlkDynamic);
    AddMethod('GetClientRect', @TSepiImportsTCustomForm.GetClientRect,
      'function: TRect',
      mlkOverride);
    AddMethod('GetChildren', @TSepiImportsTCustomForm.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('GetFloating', @TSepiImportsTCustomForm.GetFloating,
      'function: Boolean',
      mlkOverride);
    AddMethod('GetOwnerWindow', @TSepiImportsTCustomForm.GetOwnerWindow,
      'function: HWND',
      mlkDynamic);
    AddMethod('HandleCreateException', @TSepiImportsTCustomForm.HandleCreateException,
      'function: Boolean',
      mlkDynamic);
    AddMethod('Loaded', @TSepiImportsTCustomForm.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTCustomForm.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation )',
      mlkOverride);
    AddMethod('Paint', @TSepiImportsTCustomForm.Paint,
      'procedure',
      mlkDynamic);
    AddMethod('PaintWindow', @TSepiImportsTCustomForm.PaintWindow,
      'procedure(DC: HDC)',
      mlkOverride);
    AddMethod('PaletteChanged', @TSepiImportsTCustomForm.PaletteChanged,
      'function(Foreground: Boolean): Boolean',
      mlkOverride);
    AddMethod('QueryInterface', @TSepiImportsTCustomForm.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      mlkOverride);
    AddMethod('ReadState', @TSepiImportsTCustomForm.ReadState,
      'procedure(Reader: TReader)',
      mlkOverride);
    AddMethod('RequestAlign', @TSepiImportsTCustomForm.RequestAlign,
      'procedure',
      mlkOverride);
    AddMethod('SetChildOrder', @TSepiImportsTCustomForm.SetChildOrder,
      'procedure(Child: TComponent; Order: Integer)',
      mlkOverride);
    AddMethod('SetParentBiDiMode', @TSepiImportsTCustomForm.SetParentBiDiMode,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('DoDock', @TSepiImportsTCustomForm.DoDock,
      'procedure(NewDockSite: TWinControl; var ARect: TRect)',
      mlkOverride);
    AddMethod('SetParent', @TSepiImportsTCustomForm.SetParent,
      'procedure(AParent: TWinControl)',
      mlkOverride);
    AddMethod('UpdateActions', @TSepiImportsTCustomForm.UpdateActions,
      'procedure',
      mlkVirtual);
    AddMethod('UpdateWindowState', @TSepiImportsTCustomForm.UpdateWindowState,
      'procedure');
    AddMethod('ValidateRename', @TSepiImportsTCustomForm.ValidateRename,
      'procedure(AComponent: TComponent; const CurName, NewName: string )',
      mlkOverride);
    AddMethod('VisibleChanging', @TSepiImportsTCustomForm.VisibleChanging,
      'procedure',
      mlkOverride);
    AddMethod('WndProc', @TSepiImportsTCustomForm.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);
    AddMethod('Resizing', @TSepiImportsTCustomForm.Resizing,
      'procedure(State: TWindowState)',
      mlkOverride);

    AddProperty('ActiveMDIChild', 'property: TForm',
      'GetActiveMDIChild', '');
    AddProperty('AlphaBlend', 'property: Boolean',
      'FAlphaBlend', 'SetAlphaBlend');
    AddProperty('AlphaBlendValue', 'property: Byte',
      'FAlphaBlendValue', 'SetAlphaBlendValue');
    AddProperty('BorderIcons', 'property: TBorderIcons',
      'FBorderIcons', 'SetBorderIcons',
      NoIndex, Byte(DefaultBorderIcons), 'IsForm');
    RedefineProperty('AutoScroll',
      '', '', 'IsAutoScrollStored');
    AddProperty('ClientHandle', 'property: HWND',
      'FClientHandle', '');
    RedefineProperty('ClientHeight',
      '', 'SetClientHeight', 'IsClientSizeStored');
    RedefineProperty('ClientWidth',
      '', 'SetClientWidth', 'IsClientSizeStored');
    AddProperty('TransparentColor', 'property: Boolean',
      'FTransparentColor', 'SetTransparentColor');
    AddProperty('TransparentColorValue', 'property: TColor',
      'FTransparentColorValue', 'SetTransparentColorValue');
    RedefineProperty('Ctl3D',
      '', '', Integer(True));
    AddProperty('DefaultMonitor', 'property: TDefaultMonitor',
      'FDefaultMonitor', 'FDefaultMonitor',
      NoIndex, Integer(dmActiveForm), 'IsForm');
    AddProperty('FormStyle', 'property: TFormStyle',
      'FFormStyle', 'SetFormStyle',
      NoIndex, Integer(fsNormal), 'IsForm');
    RedefineProperty('Height',
      '', '', 'IsFormSizeStored');
    RedefineProperty('HorzScrollBar',
      '', '', 'IsForm');
    AddProperty('Icon', 'property: TIcon',
      'FIcon', 'SetIcon',
      NoIndex, NoDefaultValue, 'IsIconStored');
    AddProperty('MDIChildCount', 'property: Integer',
      'GetMDIChildCount', '');
    AddProperty('MDIChildren', 'property[I: Integer]: TForm',
      'GetMDIChildren', '');
    AddProperty('OldCreateOrder', 'property: Boolean',
      'FOldCreateOrder', 'FOldCreateOrder');
    AddProperty('ObjectMenuItem', 'property: TMenuItem',
      'FObjectMenuItem', 'SetObjectMenuItem',
      NoIndex, NoDefaultValue, 'IsForm');
    AddProperty('PixelsPerInch', 'property: Integer',
      'GetPixelsPerInch', 'SetPixelsPerInch',
      NoIndex, NoDefaultValue, 'False');
    RedefineProperty('ParentFont',
      '', '', Integer(False));
    RedefineProperty('PopupMenu',
      '', '', 'IsForm');
    AddProperty('PopupChildren', 'property: TList',
      'GetPopupChildren', '');
    AddProperty('Position', 'property: TPosition',
      'FPosition', 'SetPosition',
      NoIndex, Integer(poDefaultPosOnly), 'IsForm');
    AddProperty('PrintScale', 'property: TPrintScale',
      'FPrintScale', 'FPrintScale',
      NoIndex, Integer(poProportional), 'IsForm');
    AddProperty('Scaled', 'property: Boolean',
      'GetScaled', 'SetScaled',
      NoIndex, Integer(True), 'IsForm');
    AddProperty('TileMode', 'property: TTileMode',
      'FTileMode', 'FTileMode',
      NoIndex, Integer(tbHorizontal));
    RedefineProperty('VertScrollBar',
      '', '', 'IsForm');
    RedefineProperty('Visible',
      '', 'SetVisible', Integer(False));
    RedefineProperty('Width',
      '', '', 'IsFormSizeStored');
    AddProperty('WindowMenu', 'property: TMenuItem',
      'FWindowMenu', 'SetWindowMenu',
      NoIndex, NoDefaultValue, 'IsForm');
    AddProperty('OnActivate', 'property: TNotifyEvent',
      'FOnActivate', 'FOnActivate',
      NoIndex, NoDefaultValue, 'IsForm');
    RedefineProperty('OnCanResize',
      '', '', 'IsForm');
    RedefineProperty('OnClick',
      '', '', 'IsForm');
    AddProperty('OnClose', 'property: TCloseEvent',
      'FOnClose', 'FOnClose',
      NoIndex, NoDefaultValue, 'IsForm');
    AddProperty('OnCloseQuery', 'property: TCloseQueryEvent',
      'FOnCloseQuery', 'FOnCloseQuery',
      NoIndex, NoDefaultValue, 'IsForm');
    AddProperty('OnCreate', 'property: TNotifyEvent',
      'FOnCreate', 'FOnCreate',
      NoIndex, NoDefaultValue, 'IsForm');
    RedefineProperty('OnDblClick',
      '', '', 'IsForm');
    AddProperty('OnDestroy', 'property: TNotifyEvent',
      'FOnDestroy', 'FOnDestroy',
      NoIndex, NoDefaultValue, 'IsForm');
    AddProperty('OnDeactivate', 'property: TNotifyEvent',
      'FOnDeactivate', 'FOnDeactivate',
      NoIndex, NoDefaultValue, 'IsForm');
    RedefineProperty('OnDragDrop',
      '', '', 'IsForm');
    RedefineProperty('OnDragOver',
      '', '', 'IsForm');
    AddProperty('OnHelp', 'property: THelpEvent',
      'FOnHelp', 'FOnHelp');
    AddProperty('OnHide', 'property: TNotifyEvent',
      'FOnHide', 'FOnHide',
      NoIndex, NoDefaultValue, 'IsForm');
    RedefineProperty('OnKeyDown',
      '', '', 'IsForm');
    RedefineProperty('OnKeyPress',
      '', '', 'IsForm');
    RedefineProperty('OnKeyUp',
      '', '', 'IsForm');
    RedefineProperty('OnMouseActivate',
      '', '', 'IsForm');
    RedefineProperty('OnMouseDown',
      '', '', 'IsForm');
    RedefineProperty('OnMouseMove',
      '', '', 'IsForm');
    RedefineProperty('OnMouseUp',
      '', '', 'IsForm');
    AddProperty('OnPaint', 'property: TNotifyEvent',
      'FOnPaint', 'FOnPaint',
      NoIndex, NoDefaultValue, 'IsForm');
    RedefineProperty('OnResize',
      '', '', 'IsForm');
    AddProperty('OnShortCut', 'property: TShortCutEvent',
      'FOnShortCut', 'FOnShortCut');
    AddProperty('OnShow', 'property: TNotifyEvent',
      'FOnShow', 'FOnShow',
      NoIndex, NoDefaultValue, 'IsForm');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomForm.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('CreateNew', @TSepiImportsTCustomForm.CreateNew,
      'constructor(AOwner: TComponent; Dummy: Integer = 0)',
      mlkVirtual);
    AddMethod('Destroy', @TSepiImportsTCustomForm.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AfterConstruction', @TSepiImportsTCustomForm.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('BeforeDestruction', @TSepiImportsTCustomForm.BeforeDestruction,
      'procedure',
      mlkOverride);
    AddMethod('Close', @TSepiImportsTCustomForm.Close,
      'procedure');
    AddMethod('CloseQuery', @TSepiImportsTCustomForm.CloseQuery,
      'function: Boolean',
      mlkVirtual);
    AddMethod('DefaultHandler', @TSepiImportsTCustomForm.DefaultHandler,
      'procedure(var Message)',
      mlkOverride);
    AddMethod('DefocusControl', @TSepiImportsTCustomForm.DefocusControl,
      'procedure(Control: TWinControl; Removing: Boolean)');
    AddMethod('Dock', @TSepiImportsTCustomForm.Dock,
      'procedure(NewDockSite: TWinControl; ARect: TRect)',
      mlkOverride);
    AddMethod('FocusControl', @TSepiImportsTCustomForm.FocusControl,
      'procedure(Control: TWinControl)');
    AddMethod('GetFormImage', @TSepiImportsTCustomForm.GetFormImage,
      'function: TBitmap');
    AddMethod('Hide', @TSepiImportsTCustomForm.Hide,
      'procedure');
    AddMethod('IsShortCut', @TSepiImportsTCustomForm.IsShortCut,
      'function(var Message: TWMKey): Boolean',
      mlkDynamic);
    AddMethod('MakeFullyVisible', @TSepiImportsTCustomForm.MakeFullyVisible,
      'procedure(AMonitor: TMonitor = nil)');
    AddMethod('MouseWheelHandler', @TSepiImportsTCustomForm.MouseWheelHandler,
      'procedure(var Message: TMessage)',
      mlkOverride);
    AddMethod('Print', @TSepiImportsTCustomForm.Print,
      'procedure');
    AddMethod('RecreateAsPopup', @TSepiImportsTCustomForm.RecreateAsPopup,
      'procedure(AWindowHandle: HWND)');
    AddMethod('Release', @TSepiImportsTCustomForm.Release,
      'procedure');
    AddMethod('SendCancelMode', @TSepiImportsTCustomForm.SendCancelMode,
      'procedure(Sender: TControl)');
    AddMethod('SetFocus', @TSepiImportsTCustomForm.SetFocus,
      'procedure',
      mlkOverride);
    AddMethod('SetFocusedControl', @TSepiImportsTCustomForm.SetFocusedControl,
      'function(Control: TWinControl): Boolean',
      mlkVirtual);
    AddMethod('Show', @TSepiImportsTCustomForm.Show,
      'procedure');
    AddMethod('ShowModal', @TSepiImportsTCustomForm.ShowModal,
      'function: Integer',
      mlkVirtual);
    AddMethod('WantChildKey', @TSepiImportsTCustomForm.WantChildKey,
      'function(Child: TControl; var Message: TMessage): Boolean',
      mlkVirtual);

    AddProperty('Active', 'property: Boolean',
      'FActive', '');
    AddProperty('ActiveControl', 'property: TWinControl',
      'FActiveControl', 'SetActiveControl',
      NoIndex, NoDefaultValue, 'IsForm');
    RedefineProperty('Action');
    AddProperty('ActiveOleControl', 'property: TWinControl',
      'FActiveOleControl', 'SetActiveOleControl');
    AddProperty('BorderStyle', 'property: TFormBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(bsSizeable), 'IsForm');
    AddProperty('Canvas', 'property: TCanvas',
      'GetCanvas', '');
    RedefineProperty('Caption',
      '', '', 'IsForm');
    RedefineProperty('Color',
      '', '', NoDefaultValue);
    AddProperty('Designer', 'property: IDesignerHook',
      'FDesigner', 'SetDesigner');
    AddProperty('DropTarget', 'property: Boolean',
      'FDropTarget', 'FDropTarget');
    RedefineProperty('Font');
    AddProperty('FormState', 'property: TFormState',
      'FFormState', '');
    AddProperty('HelpFile', 'property: string',
      'FHelpFile', 'FHelpFile');
    AddProperty('KeyPreview', 'property: Boolean',
      'FKeyPreview', 'FKeyPreview',
      NoIndex, Integer(False), 'IsForm');
    AddProperty('Menu', 'property: TMainMenu',
      'FMenu', 'SetMenu',
      NoIndex, NoDefaultValue, 'IsForm');
    AddProperty('ModalResult', 'property: TModalResult',
      'FModalResult', 'FModalResult');
    AddProperty('Monitor', 'property: TMonitor',
      'GetMonitor', '');
    AddProperty('OleFormObject', 'property: IOleForm',
      'FOleForm', 'FOleForm');
    AddProperty('PopupMode', 'property: TPopupMode',
      'FPopupMode', 'SetPopupMode',
      NoIndex, Integer(pmNone));
    AddProperty('PopupParent', 'property: TCustomForm',
      'FPopupParent', 'SetPopupParent');
    AddProperty('ScreenSnap', 'property: Boolean',
      'FScreenSnap', 'FScreenSnap',
      NoIndex, Integer(False));
    AddProperty('SnapBuffer', 'property: Integer',
      'FSnapBuffer', 'FSnapBuffer');
    AddProperty('WindowState', 'property: TWindowState',
      'FWindowState', 'SetWindowState',
      NoIndex, Integer(wsNormal), 'IsForm');

    CurrentVisibility := mvPublished;

    AddProperty('Left', 'property: Integer',
      'GetLeft', 'SetLeft');
    AddProperty('Top', 'property: Integer',
      'GetTop', 'SetTop');

    Complete;
  end;
end;

{--------------------------}
{ TCustomActiveForm import }
{--------------------------}

procedure TSepiImportsTCustomActiveForm.SetAxBorderStyle(Value: TActiveFormBorderStyle);
begin
  AxBorderStyle := Value;
end;

class function TSepiImportsTCustomActiveForm.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomActiveForm));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAxBorderStyle', System.TypeInfo(TActiveFormBorderStyle));

    AddMethod('SetAxBorderStyle', @TSepiImportsTCustomActiveForm.SetAxBorderStyle,
      'procedure(Value: TActiveFormBorderStyle)');

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTCustomActiveForm.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomActiveForm.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('WantChildKey', @TSepiImportsTCustomActiveForm.WantChildKey,
      'function(Child: TControl; var Message: TMessage): Boolean',
      mlkOverride);

    RedefineProperty('Visible');

    CurrentVisibility := mvPublished;

    RedefineProperty('ActiveControl');
    RedefineProperty('Anchors');
    RedefineProperty('AutoScroll');
    RedefineProperty('AutoSize');
    AddProperty('AxBorderStyle', 'property: TActiveFormBorderStyle',
      'FAxBorderStyle', 'SetAxBorderStyle',
      NoIndex, Integer(afbSingle));
    RedefineProperty('BorderWidth');
    RedefineProperty('Caption',
      '', '', 'True');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Font');
    RedefineProperty('Height',
      '', '', 'True');
    RedefineProperty('HorzScrollBar');
    RedefineProperty('KeyPreview');
    RedefineProperty('OldCreateOrder');
    RedefineProperty('PixelsPerInch');
    RedefineProperty('PopupMenu');
    RedefineProperty('PrintScale');
    RedefineProperty('Scaled');
    RedefineProperty('ShowHint');
    RedefineProperty('VertScrollBar');
    RedefineProperty('Width',
      '', '', 'True');
    RedefineProperty('OnActivate');
    RedefineProperty('OnClick');
    RedefineProperty('OnCreate');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDestroy');
    RedefineProperty('OnDeactivate');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnPaint');

    Complete;
  end;
end;

{--------------}
{ TForm import }
{--------------}

class function TSepiImportsTForm.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TForm'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TForm));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('ArrangeIcons', @TSepiImportsTForm.ArrangeIcons,
      'procedure');
    AddMethod('Cascade', @TSepiImportsTForm.Cascade,
      'procedure');
    AddMethod('Next', @TSepiImportsTForm.Next,
      'procedure');
    AddMethod('Previous', @TSepiImportsTForm.Previous,
      'procedure');
    AddMethod('Tile', @TSepiImportsTForm.Tile,
      'procedure');

    RedefineProperty('ActiveMDIChild');
    RedefineProperty('ClientHandle');
    RedefineProperty('DockManager');
    RedefineProperty('MDIChildCount');
    RedefineProperty('MDIChildren');
    RedefineProperty('TileMode');

    CurrentVisibility := mvPublished;

    RedefineProperty('Action');
    RedefineProperty('ActiveControl');
    RedefineProperty('Align');
    RedefineProperty('AlphaBlend',
      '', '', Integer(False));
    RedefineProperty('AlphaBlendValue',
      '', '', 255);
    RedefineProperty('Anchors');
    RedefineProperty('AutoScroll');
    RedefineProperty('AutoSize');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderIcons');
    RedefineProperty('BorderStyle');
    RedefineProperty('BorderWidth');
    RedefineProperty('Caption');
    RedefineProperty('ClientHeight');
    RedefineProperty('ClientWidth');
    RedefineProperty('Color');
    RedefineProperty('TransparentColor',
      '', '', Integer(False));
    RedefineProperty('TransparentColorValue',
      '', '', 0);
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('UseDockManager');
    RedefineProperty('DefaultMonitor');
    RedefineProperty('DockSite');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('ParentFont',
      '', '', Integer(False));
    RedefineProperty('Font');
    RedefineProperty('FormStyle');
    RedefineProperty('Height');
    RedefineProperty('HelpFile');
    RedefineProperty('HorzScrollBar');
    RedefineProperty('Icon');
    RedefineProperty('KeyPreview');
    RedefineProperty('Menu');
    RedefineProperty('OldCreateOrder');
    RedefineProperty('ObjectMenuItem');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('PixelsPerInch');
    RedefineProperty('PopupMenu');
    RedefineProperty('PopupMode');
    RedefineProperty('PopupParent');
    RedefineProperty('Position');
    RedefineProperty('PrintScale');
    RedefineProperty('Scaled');
    RedefineProperty('ScreenSnap',
      '', '', Integer(False));
    RedefineProperty('ShowHint');
    RedefineProperty('SnapBuffer',
      '', '', 10);
    RedefineProperty('VertScrollBar');
    RedefineProperty('Visible');
    RedefineProperty('Width');
    RedefineProperty('WindowState');
    RedefineProperty('WindowMenu');
    RedefineProperty('OnActivate');
    RedefineProperty('OnAlignInsertBefore');
    RedefineProperty('OnAlignPosition');
    RedefineProperty('OnCanResize');
    RedefineProperty('OnClick');
    RedefineProperty('OnClose');
    RedefineProperty('OnCloseQuery');
    RedefineProperty('OnConstrainedResize');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnCreate');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDestroy');
    RedefineProperty('OnDeactivate');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnGetSiteInfo');
    RedefineProperty('OnHide');
    RedefineProperty('OnHelp');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnMouseWheel');
    RedefineProperty('OnMouseWheelDown');
    RedefineProperty('OnMouseWheelUp');
    RedefineProperty('OnPaint');
    RedefineProperty('OnResize');
    RedefineProperty('OnShortCut');
    RedefineProperty('OnShow');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{------------------------}
{ TCustomDockForm import }
{------------------------}

class function TSepiImportsTCustomDockForm.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomDockForm));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('WMNCHitTest', nil,
      'procedure(var Message: TWMNCHitTest)',
      mlkMessage, False, WM_NCHITTEST);
    AddMethod('WMNCLButtonDown', nil,
      'procedure(var Message: TWMNCLButtonDown)',
      mlkMessage, False, WM_NCLBUTTONDOWN);
    AddMethod('CMControlListChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CONTROLLISTCHANGE);
    AddMethod('CMDockNotification', nil,
      'procedure(var Message: TCMDockNotification)',
      mlkMessage, False, CM_DOCKNOTIFICATION);
    AddMethod('CMUnDockClient', nil,
      'procedure(var Message: TCMUnDockClient)',
      mlkMessage, False, CM_UNDOCKCLIENT);
    AddMethod('CMVisibleChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_VISIBLECHANGED);

    CurrentVisibility := mvProtected;

    AddMethod('DoAddDockClient', @TSepiImportsTCustomDockForm.DoAddDockClient,
      'procedure(Client: TControl; const ARect: TRect)',
      mlkOverride);
    AddMethod('DoRemoveDockClient', @TSepiImportsTCustomDockForm.DoRemoveDockClient,
      'procedure(Client: TControl)',
      mlkOverride);
    AddMethod('GetSiteInfo', @TSepiImportsTCustomDockForm.GetSiteInfo,
      'procedure(Client: TControl; var InfluenceRect: TRect; MousePos: TPoint ; var CanDock: Boolean )',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTCustomDockForm.Loaded,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomDockForm.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    RedefineProperty('AutoScroll',
      '', '', Integer(False));
    RedefineProperty('BorderStyle',
      '', '', Integer(bsSizeToolWin));
    RedefineProperty('FormStyle',
      '', '', Integer(fsStayOnTop));

    CurrentVisibility := mvPublished;

    RedefineProperty('PixelsPerInch');

    Complete;
  end;
end;

{-----------------}
{ TMonitor import }
{-----------------}

function TSepiImportsTMonitor.GetLeft: Integer;
begin
  Result := Left;
end;

function TSepiImportsTMonitor.GetHeight: Integer;
begin
  Result := Height;
end;

function TSepiImportsTMonitor.GetTop: Integer;
begin
  Result := Top;
end;

function TSepiImportsTMonitor.GetWidth: Integer;
begin
  Result := Width;
end;

function TSepiImportsTMonitor.GetBoundsRect: TRect;
begin
  Result := BoundsRect;
end;

function TSepiImportsTMonitor.GetWorkareaRect: TRect;
begin
  Result := WorkareaRect;
end;

function TSepiImportsTMonitor.GetPrimary: Boolean;
begin
  Result := Primary;
end;

class function TSepiImportsTMonitor.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TMonitor'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TMonitor));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHandle', System.TypeInfo(HMONITOR));
    AddField('FMonitorNum', System.TypeInfo(Integer));

    AddMethod('GetLeft', @TSepiImportsTMonitor.GetLeft,
      'function: Integer');
    AddMethod('GetHeight', @TSepiImportsTMonitor.GetHeight,
      'function: Integer');
    AddMethod('GetTop', @TSepiImportsTMonitor.GetTop,
      'function: Integer');
    AddMethod('GetWidth', @TSepiImportsTMonitor.GetWidth,
      'function: Integer');
    AddMethod('GetBoundsRect', @TSepiImportsTMonitor.GetBoundsRect,
      'function: TRect');
    AddMethod('GetWorkareaRect', @TSepiImportsTMonitor.GetWorkareaRect,
      'function: TRect');
    AddMethod('GetPrimary', @TSepiImportsTMonitor.GetPrimary,
      'function: Boolean');

    CurrentVisibility := mvPublic;

    AddProperty('Handle', 'property: HMONITOR',
      'FHandle', '');
    AddProperty('MonitorNum', 'property: Integer',
      'FMonitorNum', '');
    AddProperty('Left', 'property: Integer',
      'GetLeft', '');
    AddProperty('Height', 'property: Integer',
      'GetHeight', '');
    AddProperty('Top', 'property: Integer',
      'GetTop', '');
    AddProperty('Width', 'property: Integer',
      'GetWidth', '');
    AddProperty('BoundsRect', 'property: TRect',
      'GetBoundsRect', '');
    AddProperty('WorkareaRect', 'property: TRect',
      'GetWorkareaRect', '');
    AddProperty('Primary', 'property: Boolean',
      'GetPrimary', '');

    Complete;
  end;
end;

{-------------------}
{ TCursorRec import }
{-------------------}

function SepiImportTCursorRec(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCursorRec', False, True);

  with Result do
  begin
    AddField('Next', 'PCursorRec');
    AddField('Index', System.TypeInfo(Integer));
    AddField('Handle', System.TypeInfo(HCURSOR));

    Complete;
  end;
end;

{----------------}
{ TScreen import }
{----------------}

function TSepiImportsTScreen.GetCustomFormCount: Integer;
begin
  Result := CustomFormCount;
end;

function TSepiImportsTScreen.GetCustomForms(Index: Integer): TCustomForm;
begin
  Result := CustomForms[Index];
end;

function TSepiImportsTScreen.GetCursors(Index: Integer): HCURSOR;
begin
  Result := Cursors[Index];
end;

function TSepiImportsTScreen.GetDataModule(Index: Integer): TDataModule;
begin
  Result := DataModules[Index];
end;

function TSepiImportsTScreen.GetDataModuleCount: Integer;
begin
  Result := DataModuleCount;
end;

function TSepiImportsTScreen.GetDefaultIME: String;
begin
  Result := DefaultIme;
end;

function TSepiImportsTScreen.GetDesktopTop: Integer;
begin
  Result := DesktopTop;
end;

function TSepiImportsTScreen.GetDesktopLeft: Integer;
begin
  Result := DesktopLeft;
end;

function TSepiImportsTScreen.GetDesktopHeight: Integer;
begin
  Result := DesktopHeight;
end;

function TSepiImportsTScreen.GetDesktopWidth: Integer;
begin
  Result := DesktopWidth;
end;

function TSepiImportsTScreen.GetDesktopRect: TRect;
begin
  Result := DesktopRect;
end;

function TSepiImportsTScreen.GetWorkAreaRect: TRect;
begin
  Result := WorkAreaRect;
end;

function TSepiImportsTScreen.GetWorkAreaHeight: Integer;
begin
  Result := WorkAreaHeight;
end;

function TSepiImportsTScreen.GetWorkAreaLeft: Integer;
begin
  Result := WorkAreaLeft;
end;

function TSepiImportsTScreen.GetWorkAreaTop: Integer;
begin
  Result := WorkAreaTop;
end;

function TSepiImportsTScreen.GetWorkAreaWidth: Integer;
begin
  Result := WorkAreaWidth;
end;

function TSepiImportsTScreen.GetImes: TStrings;
begin
  Result := Imes;
end;

function TSepiImportsTScreen.GetHeight: Integer;
begin
  Result := Height;
end;

function TSepiImportsTScreen.GetMonitor(Index: Integer): TMonitor;
begin
  Result := Monitors[Index];
end;

function TSepiImportsTScreen.GetMonitorCount: Integer;
begin
  Result := MonitorCount;
end;

function TSepiImportsTScreen.GetFonts: TStrings;
begin
  Result := Fonts;
end;

function TSepiImportsTScreen.GetForm(Index: Integer): TForm;
begin
  Result := Forms[Index];
end;

function TSepiImportsTScreen.GetFormCount: Integer;
begin
  Result := FormCount;
end;

function TSepiImportsTScreen.GetWidth: Integer;
begin
  Result := Width;
end;

procedure TSepiImportsTScreen.SetCursors(Index: Integer; Handle: HCURSOR);
begin
  Cursors[Index] := Handle;
end;

procedure TSepiImportsTScreen.SetCursor(Value: TCursor);
begin
  Cursor := Value;
end;

procedure TSepiImportsTScreen.SetHintFont(Value: TFont);
begin
  HintFont := Value;
end;

procedure TSepiImportsTScreen.SetIconFont(Value: TFont);
begin
  IconFont := Value;
end;

procedure TSepiImportsTScreen.SetMenuFont(Value: TFont);
begin
  MenuFont := Value;
end;

function TSepiImportsTScreen.GetPrimaryMonitor: TMonitor;
begin
  Result := PrimaryMonitor;
end;

class function TSepiImportsTScreen.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScreen));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFonts', System.TypeInfo(TStrings));
    AddField('FImes', System.TypeInfo(TStrings));
    AddField('FDefaultIme', System.TypeInfo(string));
    AddField('FDefaultKbLayout', System.TypeInfo(HKL));
    AddField('FPixelsPerInch', System.TypeInfo(Integer));
    AddField('FCursor', System.TypeInfo(TCursor));
    AddField('FCursorCount', System.TypeInfo(Integer));
    AddField('FForms', System.TypeInfo(TList));
    AddField('FCustomForms', System.TypeInfo(TList));
    AddField('FDataModules', System.TypeInfo(TList));
    AddField('FMonitors', System.TypeInfo(TList));
    AddField('FCursorList', 'PCursorRec');
    AddField('FDefaultCursor', System.TypeInfo(HCURSOR));
    AddField('FActiveControl', System.TypeInfo(TWinControl));
    AddField('FActiveCustomForm', System.TypeInfo(TCustomForm));
    AddField('FActiveForm', System.TypeInfo(TForm));
    AddField('FLastActiveControl', System.TypeInfo(TWinControl));
    AddField('FLastActiveCustomForm', System.TypeInfo(TCustomForm));
    AddField('FFocusedForm', System.TypeInfo(TCustomForm));
    AddField('FSaveFocusedList', System.TypeInfo(TList));
    AddField('FHintFont', System.TypeInfo(TFont));
    AddField('FIconFont', System.TypeInfo(TFont));
    AddField('FMenuFont', System.TypeInfo(TFont));
    AddField('FAlignLevel', System.TypeInfo(Word));
    AddField('FControlState', System.TypeInfo(TControlState));
    AddField('FOnActiveControlChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnActiveFormChange', System.TypeInfo(TNotifyEvent));

    AddMethod('AlignForm', nil,
      'procedure(AForm: TCustomForm)');
    AddMethod('AlignForms', nil,
      'procedure(AForm: TCustomForm; var Rect: TRect)');
    AddMethod('AddDataModule', nil,
      'procedure(DataModule: TDataModule)');
    AddMethod('AddForm', nil,
      'procedure(AForm: TCustomForm)');
    AddMethod('ClearMonitors', nil,
      'procedure');
    AddMethod('CreateCursors', nil,
      'procedure');
    AddMethod('DeleteCursor', nil,
      'procedure(Index: Integer)');
    AddMethod('DestroyCursors', nil,
      'procedure');
    AddMethod('FindMonitor', nil,
      'function(Handle: HMONITOR): TMonitor');
    AddMethod('IconFontChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('GetCustomFormCount', @TSepiImportsTScreen.GetCustomFormCount,
      'function: Integer');
    AddMethod('GetCustomForms', @TSepiImportsTScreen.GetCustomForms,
      'function(Index: Integer): TCustomForm');
    AddMethod('GetCursors', @TSepiImportsTScreen.GetCursors,
      'function(Index: Integer): HCURSOR');
    AddMethod('GetDataModule', @TSepiImportsTScreen.GetDataModule,
      'function(Index: Integer): TDataModule');
    AddMethod('GetDataModuleCount', @TSepiImportsTScreen.GetDataModuleCount,
      'function: Integer');
    AddMethod('GetDefaultIME', @TSepiImportsTScreen.GetDefaultIME,
      'function: String');
    AddMethod('GetDesktopTop', @TSepiImportsTScreen.GetDesktopTop,
      'function: Integer');
    AddMethod('GetDesktopLeft', @TSepiImportsTScreen.GetDesktopLeft,
      'function: Integer');
    AddMethod('GetDesktopHeight', @TSepiImportsTScreen.GetDesktopHeight,
      'function: Integer');
    AddMethod('GetDesktopWidth', @TSepiImportsTScreen.GetDesktopWidth,
      'function: Integer');
    AddMethod('GetDesktopRect', @TSepiImportsTScreen.GetDesktopRect,
      'function: TRect');
    AddMethod('GetWorkAreaRect', @TSepiImportsTScreen.GetWorkAreaRect,
      'function: TRect');
    AddMethod('GetWorkAreaHeight', @TSepiImportsTScreen.GetWorkAreaHeight,
      'function: Integer');
    AddMethod('GetWorkAreaLeft', @TSepiImportsTScreen.GetWorkAreaLeft,
      'function: Integer');
    AddMethod('GetWorkAreaTop', @TSepiImportsTScreen.GetWorkAreaTop,
      'function: Integer');
    AddMethod('GetWorkAreaWidth', @TSepiImportsTScreen.GetWorkAreaWidth,
      'function: Integer');
    AddMethod('GetImes', @TSepiImportsTScreen.GetImes,
      'function: TStrings');
    AddMethod('GetHeight', @TSepiImportsTScreen.GetHeight,
      'function: Integer');
    AddMethod('GetMonitor', @TSepiImportsTScreen.GetMonitor,
      'function(Index: Integer): TMonitor');
    AddMethod('GetMonitorCount', @TSepiImportsTScreen.GetMonitorCount,
      'function: Integer');
    AddMethod('GetMonitors', nil,
      'procedure');
    AddMethod('GetFonts', @TSepiImportsTScreen.GetFonts,
      'function: TStrings');
    AddMethod('GetForm', @TSepiImportsTScreen.GetForm,
      'function(Index: Integer): TForm');
    AddMethod('GetFormCount', @TSepiImportsTScreen.GetFormCount,
      'function: Integer');
    AddMethod('GetMetricSettings', nil,
      'procedure');
    AddMethod('GetWidth', @TSepiImportsTScreen.GetWidth,
      'function: Integer');
    AddMethod('InsertCursor', nil,
      'procedure(Index: Integer; Handle: HCURSOR)');
    AddMethod('RemoveDataModule', nil,
      'procedure(DataModule: TDataModule)');
    AddMethod('RemoveForm', nil,
      'procedure(AForm: TCustomForm)');
    AddMethod('SetCursors', @TSepiImportsTScreen.SetCursors,
      'procedure(Index: Integer; Handle: HCURSOR)');
    AddMethod('SetCursor', @TSepiImportsTScreen.SetCursor,
      'procedure(Value: TCursor)');
    AddMethod('SetHintFont', @TSepiImportsTScreen.SetHintFont,
      'procedure(Value: TFont)');
    AddMethod('SetIconFont', @TSepiImportsTScreen.SetIconFont,
      'procedure(Value: TFont)');
    AddMethod('SetMenuFont', @TSepiImportsTScreen.SetMenuFont,
      'procedure(Value: TFont)');
    AddMethod('UpdateLastActive', nil,
      'procedure');
    AddMethod('GetPrimaryMonitor', @TSepiImportsTScreen.GetPrimaryMonitor,
      'function: TMonitor');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScreen.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTScreen.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('DisableAlign', @TSepiImportsTScreen.DisableAlign,
      'procedure');
    AddMethod('EnableAlign', @TSepiImportsTScreen.EnableAlign,
      'procedure');
    AddMethod('MonitorFromPoint', @TSepiImportsTScreen.MonitorFromPoint,
      'function(const Point: TPoint; MonitorDefault: TMonitorDefaultTo = mdNearest ) : TMonitor');
    AddMethod('MonitorFromRect', @TSepiImportsTScreen.MonitorFromRect,
      'function(const Rect: TRect; MonitorDefault: TMonitorDefaultTo = mdNearest ) : TMonitor');
    AddMethod('MonitorFromWindow', @TSepiImportsTScreen.MonitorFromWindow,
      'function(const Handle: THandle; MonitorDefault: TMonitorDefaultTo = mdNearest ) : TMonitor');
    AddMethod('Realign', @TSepiImportsTScreen.Realign,
      'procedure');
    AddMethod('ResetFonts', @TSepiImportsTScreen.ResetFonts,
      'procedure');

    AddProperty('ActiveControl', 'property: TWinControl',
      'FActiveControl', '');
    AddProperty('ActiveCustomForm', 'property: TCustomForm',
      'FActiveCustomForm', '');
    AddProperty('ActiveForm', 'property: TForm',
      'FActiveForm', '');
    AddProperty('CustomFormCount', 'property: Integer',
      'GetCustomFormCount', '');
    AddProperty('CustomForms', 'property[Index: Integer]: TCustomForm',
      'GetCustomForms', '');
    AddProperty('CursorCount', 'property: Integer',
      'FCursorCount', '');
    AddProperty('Cursor', 'property: TCursor',
      'FCursor', 'SetCursor');
    AddProperty('Cursors', 'property[Index: Integer]: HCURSOR',
      'GetCursors', 'SetCursors');
    AddProperty('DataModules', 'property[Index: Integer]: TDataModule',
      'GetDataModule', '');
    AddProperty('DataModuleCount', 'property: Integer',
      'GetDataModuleCount', '');
    AddProperty('FocusedForm', 'property: TCustomForm',
      'FFocusedForm', 'FFocusedForm');
    AddProperty('SaveFocusedList', 'property: TList',
      'FSaveFocusedList', '');
    AddProperty('MonitorCount', 'property: Integer',
      'GetMonitorCount', '');
    AddProperty('Monitors', 'property[Index: Integer]: TMonitor',
      'GetMonitor', '');
    AddProperty('DesktopRect', 'property: TRect',
      'GetDesktopRect', '');
    AddProperty('DesktopHeight', 'property: Integer',
      'GetDesktopHeight', '');
    AddProperty('DesktopLeft', 'property: Integer',
      'GetDesktopLeft', '');
    AddProperty('DesktopTop', 'property: Integer',
      'GetDesktopTop', '');
    AddProperty('DesktopWidth', 'property: Integer',
      'GetDesktopWidth', '');
    AddProperty('WorkAreaRect', 'property: TRect',
      'GetWorkAreaRect', '');
    AddProperty('WorkAreaHeight', 'property: Integer',
      'GetWorkAreaHeight', '');
    AddProperty('WorkAreaLeft', 'property: Integer',
      'GetWorkAreaLeft', '');
    AddProperty('WorkAreaTop', 'property: Integer',
      'GetWorkAreaTop', '');
    AddProperty('WorkAreaWidth', 'property: Integer',
      'GetWorkAreaWidth', '');
    AddProperty('HintFont', 'property: TFont',
      'FHintFont', 'SetHintFont');
    AddProperty('IconFont', 'property: TFont',
      'FIconFont', 'SetIconFont');
    AddProperty('MenuFont', 'property: TFont',
      'FMenuFont', 'SetMenuFont');
    AddProperty('Fonts', 'property: TStrings',
      'GetFonts', '');
    AddProperty('FormCount', 'property: Integer',
      'GetFormCount', '');
    AddProperty('Forms', 'property[Index: Integer]: TForm',
      'GetForm', '');
    AddProperty('Imes', 'property: TStrings',
      'GetImes', '');
    AddProperty('DefaultIme', 'property: string',
      'GetDefaultIme', '');
    AddProperty('DefaultKbLayout', 'property: HKL',
      'FDefaultKbLayout', '');
    AddProperty('Height', 'property: Integer',
      'GetHeight', '');
    AddProperty('PixelsPerInch', 'property: Integer',
      'FPixelsPerInch', '');
    AddProperty('PrimaryMonitor', 'property: TMonitor',
      'GetPrimaryMonitor', '');
    AddProperty('Width', 'property: Integer',
      'GetWidth', '');
    AddProperty('OnActiveControlChange', 'property: TNotifyEvent',
      'FOnActiveControlChange', 'FOnActiveControlChange');
    AddProperty('OnActiveFormChange', 'property: TNotifyEvent',
      'FOnActiveFormChange', 'FOnActiveFormChange');

    Complete;
  end;
end;

{------------------}
{ THintInfo import }
{------------------}

function SepiImportTHintInfo(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'THintInfo', False, True,
    TypeInfo(THintInfo));

  with Result do
  begin
    AddField('HintControl', System.TypeInfo(TControl));
    AddField('HintWindowClass', 'THintWindowClass');
    AddField('HintPos', 'TPoint');
    AddField('HintMaxWidth', System.TypeInfo(Integer));
    AddField('HintColor', System.TypeInfo(TColor));
    AddField('CursorRect', 'TRect');
    AddField('CursorPos', 'TPoint');
    AddField('ReshowTimeout', System.TypeInfo(Integer));
    AddField('HideTimeout', System.TypeInfo(Integer));
    AddField('HintStr', System.TypeInfo(string));
    AddField('HintData', 'Pointer');

    Complete;
  end;
end;

{--------------------}
{ TCMHintShow import }
{--------------------}

function SepiImportTCMHintShow(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMHintShow', False, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Reserved', System.TypeInfo(Integer));
    AddField('HintInfo', 'PHintInfo');
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------------}
{ TCMHintShowPause import }
{-------------------------}

function SepiImportTCMHintShowPause(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMHintShowPause', False, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('WasActive', System.TypeInfo(Integer));
    AddField('Pause', 'PInteger');
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------}
{ TPopupForm import }
{-------------------}

function SepiImportTPopupForm(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TPopupForm', False, True);

  with Result do
  begin
    AddField('FormID', System.TypeInfo(Integer));
    AddField('Form', System.TypeInfo(TCustomForm));
    AddField('WasPopup', System.TypeInfo(Boolean));

    Complete;
  end;
end;

{---------------------}
{ TApplication import }
{---------------------}

function TSepiImportsTApplication.GetCurrentHelpFile: string;
begin
  Result := CurrentHelpFile;
end;

function TSepiImportsTApplication.GetDialogHandle: HWND;
begin
  Result := DialogHandle;
end;

function TSepiImportsTApplication.GetActiveFormHandle: HWND;
begin
  Result := ActiveFormHandle;
end;

function TSepiImportsTApplication.GetMainFormHandle: HWND;
begin
  Result := MainFormHandle;
end;

function TSepiImportsTApplication.GetExeName: string;
begin
  Result := ExeName;
end;

function TSepiImportsTApplication.GetTitle: string;
begin
  Result := Title;
end;

procedure TSepiImportsTApplication.SetBiDiMode(Value: TBiDiMode);
begin
  BiDiMode := Value;
end;

procedure TSepiImportsTApplication.SetDialogHandle(Value: HWnd);
begin
  DialogHandle := Value;
end;

procedure TSepiImportsTApplication.SetHandle(Value: HWnd);
begin
  Handle := Value;
end;

procedure TSepiImportsTApplication.SetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TSepiImportsTApplication.SetHintColor(Value: TColor);
begin
  HintColor := Value;
end;

procedure TSepiImportsTApplication.SetIcon(Value: TIcon);
begin
  Icon := Value;
end;

procedure TSepiImportsTApplication.SetShowHint(Value: Boolean);
begin
  ShowHint := Value;
end;

procedure TSepiImportsTApplication.SetTitle(const Value: string);
begin
  Title := Value;
end;

class function TSepiImportsTApplication.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TApplication));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHandle', System.TypeInfo(HWnd));
    AddField('FBiDiMode', System.TypeInfo(TBiDiMode));
    AddField('FBiDiKeyboard', System.TypeInfo(string));
    AddField('FNonBiDiKeyboard', System.TypeInfo(string));
    AddField('FObjectInstance', 'Pointer');
    AddField('FMainForm', System.TypeInfo(TForm));
    AddField('FMouseControl', System.TypeInfo(TControl));
    AddField('FHelpSystem', System.TypeInfo(IHelpSystem));
    AddField('FHelpFile', System.TypeInfo(string));
    AddField('FHint', System.TypeInfo(string));
    AddField('FHintActive', System.TypeInfo(Boolean));
    AddField('FUpdateFormatSettings', System.TypeInfo(Boolean));
    AddField('FUpdateMetricSettings', System.TypeInfo(Boolean));
    AddField('FShowMainForm', System.TypeInfo(Boolean));
    AddField('FHintColor', System.TypeInfo(TColor));
    AddField('FHintControl', System.TypeInfo(TControl));
    AddField('FHintCursorRect', 'TRect');
    AddField('FHintHidePause', System.TypeInfo(Integer));
    AddField('FHintPause', System.TypeInfo(Integer));
    AddField('FHintShortCuts', System.TypeInfo(Boolean));
    AddField('FHintShortPause', System.TypeInfo(Integer));
    AddField('FHintWindow', System.TypeInfo(THintWindow));
    AddField('FShowHint', System.TypeInfo(Boolean));
    AddField('FTimerMode', System.TypeInfo(TTimerMode));
    AddField('FTimerHandle', System.TypeInfo(Word));
    AddField('FTitle', System.TypeInfo(string));
    AddField('FTopMostList', System.TypeInfo(TList));
    AddField('FTopMostLevel', System.TypeInfo(Integer));
    AddField('FPopupOwners', System.TypeInfo(TList));
    AddField('FPopupLevel', System.TypeInfo(Integer));
    AddField('FIcon', System.TypeInfo(TIcon));
    AddField('FTerminate', System.TypeInfo(Boolean));
    AddField('FActive', System.TypeInfo(Boolean));
    AddField('FAllowTesting', System.TypeInfo(Boolean));
    AddField('FTestLib', System.TypeInfo(THandle));
    AddField('FHandleCreated', System.TypeInfo(Boolean));
    AddField('FRunning', System.TypeInfo(Boolean));
    AddField('FWindowHooks', System.TypeInfo(TList));
    AddField('FWindowList', 'Pointer');
    AddField('FDialogHandle', System.TypeInfo(HWnd));
    AddField('FAutoDragDocking', System.TypeInfo(Boolean));
    AddField('FActionUpdateDelay', System.TypeInfo(Integer));
    AddField('FModalLevel', System.TypeInfo(Integer));
    AddField('FPopupControlWnd', System.TypeInfo(HWnd));
    AddField('FCurrentPopupID', System.TypeInfo(Integer));
    AddField('FPopupForms', System.TypeInfo(TPopupFormArray));
    AddField('FOnActionExecute', System.TypeInfo(TActionEvent));
    AddField('FOnActionUpdate', System.TypeInfo(TActionEvent));
    AddField('FOnException', System.TypeInfo(TExceptionEvent));
    AddField('FOnGetActiveFormHandle', System.TypeInfo(TGetHandleEvent));
    AddField('FOnGetMainFormHandle', System.TypeInfo(TGetHandleEvent));
    AddField('FOnMessage', System.TypeInfo(TMessageEvent));
    AddField('FOnModalBegin', System.TypeInfo(TNotifyEvent));
    AddField('FOnModalEnd', System.TypeInfo(TNotifyEvent));
    AddField('FOnHelp', System.TypeInfo(THelpEvent));
    AddField('FOnHint', System.TypeInfo(TNotifyEvent));
    AddField('FOnIdle', System.TypeInfo(TIdleEvent));
    AddField('FOnDeactivate', System.TypeInfo(TNotifyEvent));
    AddField('FOnActivate', System.TypeInfo(TNotifyEvent));
    AddField('FOnMinimize', System.TypeInfo(TNotifyEvent));
    AddField('FOnRestore', System.TypeInfo(TNotifyEvent));
    AddField('FOnShortCut', System.TypeInfo(TShortCutEvent));
    AddField('FOnShowHint', System.TypeInfo(TShowHintEvent));
    AddField('FOnSettingChange', System.TypeInfo(TSettingChangeEvent));

    AddMethod('CheckIniChange', nil,
      'function(var Message: TMessage): Boolean');
    AddMethod('DispatchAction', nil,
      'function(Msg: Longint; Action: TBasicAction): Boolean');
    AddMethod('DoActionIdle', nil,
      'procedure');
    AddMethod('DoMouseIdle', nil,
      'function: TControl');
    AddMethod('DoNormalizeTopMosts', nil,
      'procedure(IncludeMain: Boolean)');
    AddMethod('DoOnHelp', nil,
      'function(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean');
    AddMethod('DoShowOwnedPopups', nil,
      'procedure(Show: Boolean)');
    AddMethod('GetCurrentHelpFile', @TSepiImportsTApplication.GetCurrentHelpFile,
      'function: string');
    AddMethod('GetDialogHandle', @TSepiImportsTApplication.GetDialogHandle,
      'function: HWND');
    AddMethod('GetActiveFormHandle', @TSepiImportsTApplication.GetActiveFormHandle,
      'function: HWND');
    AddMethod('GetMainFormHandle', @TSepiImportsTApplication.GetMainFormHandle,
      'function: HWND');
    AddMethod('GetExeName', @TSepiImportsTApplication.GetExeName,
      'function: string');
    AddMethod('GetIconHandle', nil,
      'function: HICON');
    AddMethod('GetTitle', @TSepiImportsTApplication.GetTitle,
      'function: string');
    AddMethod('HintTimerExpired', nil,
      'procedure');
    AddMethod('IconChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('InvokeHelp', nil,
      'function(Command: Word; Data: Longint): Boolean');
    AddMethod('NotifyForms', nil,
      'procedure(Msg: Word)');
    AddMethod('PopupControlProc', nil,
      'procedure(var Message: TMessage)');
    AddMethod('ProcessMessage', nil,
      'function(var Msg: TMsg): Boolean');
    AddMethod('SetBiDiMode', @TSepiImportsTApplication.SetBiDiMode,
      'procedure(Value: TBiDiMode)');
    AddMethod('SetDialogHandle', @TSepiImportsTApplication.SetDialogHandle,
      'procedure(Value: HWnd)');
    AddMethod('SetHandle', @TSepiImportsTApplication.SetHandle,
      'procedure(Value: HWnd)');
    AddMethod('SetHint', @TSepiImportsTApplication.SetHint,
      'procedure(const Value: string)');
    AddMethod('SetHintColor', @TSepiImportsTApplication.SetHintColor,
      'procedure(Value: TColor)');
    AddMethod('SetIcon', @TSepiImportsTApplication.SetIcon,
      'procedure(Value: TIcon)');
    AddMethod('SetShowHint', @TSepiImportsTApplication.SetShowHint,
      'procedure(Value: Boolean)');
    AddMethod('SetTitle', @TSepiImportsTApplication.SetTitle,
      'procedure(const Value: string)');
    AddMethod('SettingChange', nil,
      'procedure(var Message: TWMSettingChange)');
    AddMethod('StartHintTimer', nil,
      'procedure(Value: Integer; TimerMode: TTimerMode)');
    AddMethod('StopHintTimer', nil,
      'procedure');
    AddMethod('WndProc', nil,
      'procedure(var Message: TMessage)');
    AddMethod('UpdateVisible', nil,
      'procedure');
    AddMethod('ValidateHelpSystem', nil,
      'function: Boolean');
    AddMethod('WakeMainThread', nil,
      'procedure(Sender: TObject)');

    CurrentVisibility := mvProtected;

    AddMethod('Idle', @TSepiImportsTApplication.Idle,
      'procedure(const Msg: TMsg)');
    AddMethod('IsDlgMsg', @TSepiImportsTApplication.IsDlgMsg,
      'function(var Msg: TMsg): Boolean');
    AddMethod('IsHintMsg', @TSepiImportsTApplication.IsHintMsg,
      'function(var Msg: TMsg): Boolean');
    AddMethod('IsKeyMsg', @TSepiImportsTApplication.IsKeyMsg,
      'function(var Msg: TMsg): Boolean');
    AddMethod('IsMDIMsg', @TSepiImportsTApplication.IsMDIMsg,
      'function(var Msg: TMsg): Boolean');
    AddMethod('IsShortCut', @TSepiImportsTApplication.IsShortCut,
      'function(var Message: TWMKey): Boolean');
    AddMethod('IsPreProcessMessage', @TSepiImportsTApplication.IsPreProcessMessage,
      'function(var Msg: TMsg): Boolean');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTApplication.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTApplication.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('ActivateHint', @TSepiImportsTApplication.ActivateHint,
      'procedure(CursorPos: TPoint)');
    AddMethod('AddPopupForm', @TSepiImportsTApplication.AddPopupForm,
      'function(APopupForm: TCustomForm): Integer');
    AddMethod('BringToFront', @TSepiImportsTApplication.BringToFront,
      'procedure');
    AddMethod('ControlDestroyed', @TSepiImportsTApplication.ControlDestroyed,
      'procedure(Control: TControl)');
    AddMethod('CancelHint', @TSepiImportsTApplication.CancelHint,
      'procedure');
    AddMethod('CreateForm', @TSepiImportsTApplication.CreateForm,
      'procedure(InstanceClass: TComponentClass; var Reference)');
    AddMethod('CreateHandle', @TSepiImportsTApplication.CreateHandle,
      'procedure');
    AddMethod('DoApplicationIdle', @TSepiImportsTApplication.DoApplicationIdle,
      'procedure');
    AddMethod('ExecuteAction', @TSepiImportsTApplication.ExecuteAction,
      'function(Action: TBasicAction): Boolean');
    AddMethod('HandleException', @TSepiImportsTApplication.HandleException,
      'procedure(Sender: TObject)');
    AddMethod('HandleMessage', @TSepiImportsTApplication.HandleMessage,
      'procedure');
    AddMethod('HelpCommand', @TSepiImportsTApplication.HelpCommand,
      'function(Command: Integer; Data: Longint): Boolean');
    AddMethod('HelpContext', @TSepiImportsTApplication.HelpContext,
      'function(Context: THelpContext): Boolean');
    AddMethod('HelpJump', @TSepiImportsTApplication.HelpJump,
      'function(const JumpID: string): Boolean');
    AddMethod('HelpKeyword', @TSepiImportsTApplication.HelpKeyword,
      'function(const Keyword: string): Boolean');
    AddMethod('HideHint', @TSepiImportsTApplication.HideHint,
      'procedure');
    AddMethod('HintMouseMessage', @TSepiImportsTApplication.HintMouseMessage,
      'procedure(Control: TControl; var Message: TMessage)');
    AddMethod('HookMainWindow', @TSepiImportsTApplication.HookMainWindow,
      'procedure(Hook: TWindowHook)');
    AddMethod('HookSynchronizeWakeup', @TSepiImportsTApplication.HookSynchronizeWakeup,
      'procedure');
    AddMethod('Initialize', @TSepiImportsTApplication.Initialize,
      'procedure');
    AddMethod('IsRightToLeft', @TSepiImportsTApplication.IsRightToLeft,
      'function: Boolean');
    AddMethod('MessageBox', @TSepiImportsTApplication.MessageBox,
      'function(const Text, Caption: PChar; Flags: Longint = MB_OK): Integer');
    AddMethod('Minimize', @TSepiImportsTApplication.Minimize,
      'procedure');
    AddMethod('ModalStarted', @TSepiImportsTApplication.ModalStarted,
      'procedure');
    AddMethod('ModalFinished', @TSepiImportsTApplication.ModalFinished,
      'procedure');
    AddMethod('NormalizeAllTopMosts', @TSepiImportsTApplication.NormalizeAllTopMosts,
      'procedure');
    AddMethod('NormalizeTopMosts', @TSepiImportsTApplication.NormalizeTopMosts,
      'procedure');
    AddMethod('ProcessMessages', @TSepiImportsTApplication.ProcessMessages,
      'procedure');
    AddMethod('RemovePopupForm', @TSepiImportsTApplication.RemovePopupForm,
      'procedure(APopupForm: TCustomForm)');
    AddMethod('Restore', @TSepiImportsTApplication.Restore,
      'procedure');
    AddMethod('RestoreTopMosts', @TSepiImportsTApplication.RestoreTopMosts,
      'procedure');
    AddMethod('Run', @TSepiImportsTApplication.Run,
      'procedure');
    AddMethod('ShowException', @TSepiImportsTApplication.ShowException,
      'procedure(E: Exception)');
    AddMethod('Terminate', @TSepiImportsTApplication.Terminate,
      'procedure');
    AddMethod('UnhookMainWindow', @TSepiImportsTApplication.UnhookMainWindow,
      'procedure(Hook: TWindowHook)');
    AddMethod('UnhookSynchronizeWakeup', @TSepiImportsTApplication.UnhookSynchronizeWakeup,
      'procedure');
    AddMethod('UpdateAction', @TSepiImportsTApplication.UpdateAction,
      'function(Action: TBasicAction): Boolean');
    AddMethod('UseRightToLeftAlignment', @TSepiImportsTApplication.UseRightToLeftAlignment,
      'function: Boolean');
    AddMethod('UseRightToLeftReading', @TSepiImportsTApplication.UseRightToLeftReading,
      'function: Boolean');
    AddMethod('UseRightToLeftScrollBar', @TSepiImportsTApplication.UseRightToLeftScrollBar,
      'function: Boolean');

    AddProperty('Active', 'property: Boolean',
      'FActive', '');
    AddProperty('ActionUpdateDelay', 'property: Integer',
      'FActionUpdateDelay', 'FActionUpdateDelay',
      NoIndex, 0);
    AddProperty('ActiveFormHandle', 'property: HWND',
      'GetActiveFormHandle', '');
    AddProperty('AllowTesting', 'property: Boolean',
      'FAllowTesting', 'FAllowTesting');
    AddProperty('AutoDragDocking', 'property: Boolean',
      'FAutoDragDocking', 'FAutoDragDocking',
      NoIndex, Integer(True));
    AddProperty('HelpSystem', 'property: IHelpSystem',
      'FHelpSystem', '');
    AddProperty('CurrentHelpFile', 'property: string',
      'GetCurrentHelpFile', '');
    AddProperty('DialogHandle', 'property: HWnd',
      'GetDialogHandle', 'SetDialogHandle');
    AddProperty('ExeName', 'property: string',
      'GetExeName', '');
    AddProperty('Handle', 'property: HWnd',
      'FHandle', 'SetHandle');
    AddProperty('HelpFile', 'property: string',
      'FHelpFile', 'FHelpFile');
    AddProperty('Hint', 'property: string',
      'FHint', 'SetHint');
    AddProperty('HintColor', 'property: TColor',
      'FHintColor', 'SetHintColor');
    AddProperty('HintHidePause', 'property: Integer',
      'FHintHidePause', 'FHintHidePause');
    AddProperty('HintPause', 'property: Integer',
      'FHintPause', 'FHintPause');
    AddProperty('HintShortCuts', 'property: Boolean',
      'FHintShortCuts', 'FHintShortCuts');
    AddProperty('HintShortPause', 'property: Integer',
      'FHintShortPause', 'FHintShortPause');
    AddProperty('Icon', 'property: TIcon',
      'FIcon', 'SetIcon');
    AddProperty('MainForm', 'property: TForm',
      'FMainForm', '');
    AddProperty('MainFormHandle', 'property: HWND',
      'GetMainFormHandle', '');
    AddProperty('ModalLevel', 'property: Integer',
      'FModalLevel', '');
    AddProperty('BiDiMode', 'property: TBiDiMode',
      'FBiDiMode', 'SetBiDiMode',
      NoIndex, Integer(bdLeftToRight));
    AddProperty('BiDiKeyboard', 'property: string',
      'FBiDiKeyboard', 'FBiDiKeyboard');
    AddProperty('NonBiDiKeyboard', 'property: string',
      'FNonBiDiKeyboard', 'FNonBiDiKeyboard');
    AddProperty('PopupControlWnd', 'property: HWND',
      'FPopupControlWnd', '');
    AddProperty('ShowHint', 'property: Boolean',
      'FShowHint', 'SetShowHint');
    AddProperty('ShowMainForm', 'property: Boolean',
      'FShowMainForm', 'FShowMainForm');
    AddProperty('Terminated', 'property: Boolean',
      'FTerminate', '');
    AddProperty('Title', 'property: string',
      'GetTitle', 'SetTitle');
    AddProperty('UpdateFormatSettings', 'property: Boolean',
      'FUpdateFormatSettings', 'FUpdateFormatSettings');
    AddProperty('UpdateMetricSettings', 'property: Boolean',
      'FUpdateMetricSettings', 'FUpdateMetricSettings');
    AddProperty('OnActionExecute', 'property: TActionEvent',
      'FOnActionExecute', 'FOnActionExecute');
    AddProperty('OnActionUpdate', 'property: TActionEvent',
      'FOnActionUpdate', 'FOnActionUpdate');
    AddProperty('OnActivate', 'property: TNotifyEvent',
      'FOnActivate', 'FOnActivate');
    AddProperty('OnDeactivate', 'property: TNotifyEvent',
      'FOnDeactivate', 'FOnDeactivate');
    AddProperty('OnException', 'property: TExceptionEvent',
      'FOnException', 'FOnException');
    AddProperty('OnGetActiveFormHandle', 'property: TGetHandleEvent',
      'FOnGetActiveFormHandle', 'FOnGetActiveFormHandle');
    AddProperty('OnGetMainFormHandle', 'property: TGetHandleEvent',
      'FOnGetMainFormHandle', 'FOnGetMainFormHandle');
    AddProperty('OnIdle', 'property: TIdleEvent',
      'FOnIdle', 'FOnIdle');
    AddProperty('OnHelp', 'property: THelpEvent',
      'FOnHelp', 'FOnHelp');
    AddProperty('OnHint', 'property: TNotifyEvent',
      'FOnHint', 'FOnHint');
    AddProperty('OnMessage', 'property: TMessageEvent',
      'FOnMessage', 'FOnMessage');
    AddProperty('OnMinimize', 'property: TNotifyEvent',
      'FOnMinimize', 'FOnMinimize');
    AddProperty('OnModalBegin', 'property: TNotifyEvent',
      'FOnModalBegin', 'FOnModalBegin');
    AddProperty('OnModalEnd', 'property: TNotifyEvent',
      'FOnModalEnd', 'FOnModalEnd');
    AddProperty('OnRestore', 'property: TNotifyEvent',
      'FOnRestore', 'FOnRestore');
    AddProperty('OnShowHint', 'property: TShowHintEvent',
      'FOnShowHint', 'FOnShowHint');
    AddProperty('OnShortCut', 'property: TShortCutEvent',
      'FOnShortCut', 'FOnShortCut');
    AddProperty('OnSettingChange', 'property: TSettingChangeEvent',
      'FOnSettingChange', 'FOnSettingChange');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function KeyboardStateToShiftState_0(const KeyboardState: TKeyboardState): TShiftState;
begin
  Result := KeyboardStateToShiftState(KeyboardState);
end;

function KeyboardStateToShiftState_1: TShiftState;
begin
  Result := KeyboardStateToShiftState;
end;

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'Forms',
    ['Messages', 'Windows', 'SysUtils', 'Classes', 'Graphics', 'Menus',
    'Controls', 'ImmTypes', 'ActnList', 'MultiMonTypes', 'HelpIntfs']);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TScrollingWinControl));
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomForm));
  TSepiClass.ForwardDecl(Result, TypeInfo(TForm));
  TSepiClass.ForwardDecl(Result, TypeInfo(TMonitor));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TScrollBarKind));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TScrollBarInc));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TScrollBarStyle));
  TSepiImportsTControlScrollBar.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TWindowState));
  TSepiImportsTScrollingWinControl.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFormBorderStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBorderStyle));
  TSepiImportsTScrollBox.SepiImport(Result);
  TSepiImportsTCustomFrame.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TCustomFrameClass', TypeInfo(TCustomFrame), True);
  TSepiImportsTFrame.SepiImport(Result);
  SepiImportIDesignerHook(Result);
  SepiImportIOleForm(Result);
  SepiImportTPopupWnd(Result);
  TSepiDynArrayType(TSepiType.LoadFromTypeInfo(
    Result, TypeInfo(TPopupWndArray))).SetElementType('TPopupWnd');
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFormStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBorderIcon));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBorderIcons));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPosition));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDefaultMonitor));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPrintScale));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TShowAction));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTileMode));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCloseAction));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCloseEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCloseQueryEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFormState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TShortCutEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THelpEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPopupMode));
  TSepiImportsTCustomForm.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TCustomFormClass', TypeInfo(TCustomForm), True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TActiveFormBorderStyle));
  TSepiImportsTCustomActiveForm.SepiImport(Result);
  TSepiImportsTForm.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TFormClass', TypeInfo(TForm), True);
  TSepiImportsTCustomDockForm.SepiImport(Result);
  TSepiImportsTMonitor.SepiImport(Result);
  TSepiPointerType.Create(Result, 'PCursorRec', 'TCursorRec', True);
  SepiImportTCursorRec(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMonitorDefaultTo));
  TSepiImportsTScreen.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTimerMode));
  TSepiPointerType.Create(Result, 'PHintInfo', TypeInfo(THintInfo), True);
  SepiImportTHintInfo(Result);
  SepiImportTCMHintShow(Result);
  SepiImportTCMHintShowPause(Result);
  SepiImportTPopupForm(Result);
  TSepiDynArrayType(TSepiType.LoadFromTypeInfo(
    Result, TypeInfo(TPopupFormArray))).SetElementType('TPopupForm');
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMessageEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TExceptionEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetHandleEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TIdleEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TShowHintEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TWindowHook));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSettingChangeEvent));
  TSepiImportsTApplication.SepiImport(Result);

  // Global variables
  TSepiVariable.Create(Result, 'Application',
     Application, TypeInfo(TApplication));
  TSepiVariable.Create(Result, 'Screen',
     Screen, TypeInfo(TScreen));
  TSepiVariable.Create(Result, 'Ctl3DBtnWndProc',
     Ctl3DBtnWndProc, 'Pointer');
  TSepiMethodRefType.Create(Result, '$1',
    'function(Window: HWnd; Msg, wParam, lParam: Longint): Longint', False, ccStdCall);
  TSepiVariable.Create(Result, 'Ctl3DDlgFramePaint',
     @Ctl3DDlgFramePaint, '$1');
  TSepiMethodRefType.Create(Result, '$2',
    'function(Window: HWnd; Msg, wParam, lParam: Longint): Longint', False, ccStdCall);
  TSepiVariable.Create(Result, 'Ctl3DCtlColorEx',
     @Ctl3DCtlColorEx, '$2');
  TSepiVariable.Create(Result, 'HintWindowClass',
     HintWindowClass, 'THintWindowClass');

  // Routines
  TSepiMetaMethod.Create(Result, 'GetParentForm', @GetParentForm,
    'function(Control: TControl; TopForm: Boolean = True): TCustomForm');
  TSepiMetaMethod.Create(Result, 'ValidParentForm', @ValidParentForm,
    'function(Control: TControl; TopForm: Boolean = True): TCustomForm');
  TSepiMetaMethod.Create(Result, 'DisableTaskWindows', @DisableTaskWindows,
    'function(ActiveWindow: HWnd): Pointer');
  TSepiMetaMethod.Create(Result, 'EnableTaskWindows', @EnableTaskWindows,
    'procedure(WindowList: Pointer)');
  TSepiMetaMethod.Create(Result, 'IsAccel', @IsAccel,
    'function(VK: Word; const Str: string): Boolean');
  TSepiMetaMethod.Create(Result, 'KeysToShiftState', @KeysToShiftState,
    'function(Keys: Word): TShiftState');
  TSepiMetaMethod.Create(Result, 'KeyDataToShiftState', @KeyDataToShiftState,
    'function(KeyData: Longint): TShiftState');
  TSepiMetaOverloadedMethod.Create(Result, 'KeyboardStateToShiftState');
  TSepiMetaMethod.Create(Result, 'OL$KeyboardStateToShiftState$0', @KeyboardStateToShiftState_0,
    'function(const KeyboardState: TKeyboardState): TShiftState');
  TSepiMetaMethod.Create(Result, 'OL$KeyboardStateToShiftState$1', @KeyboardStateToShiftState_1,
    'function: TShiftState');
  TSepiMetaMethod.Create(Result, 'ForegroundTask', @ForegroundTask,
    'function: Boolean');

  // Types
  TSepiPointerType.Create(Result, 'TFocusState', TSepiType(nil), True);

  // Routines
  TSepiMetaMethod.Create(Result, 'SaveFocusState', @SaveFocusState,
    'function: TFocusState');
  TSepiMetaMethod.Create(Result, 'RestoreFocusState', @RestoreFocusState,
    'procedure(FocusState: TFocusState)');

  // Types
  TSepiMethodRefType.Create(Result, 'TSetLayeredWindowAttributes',
    'function(Hwnd: THandle; crKey: COLORREF; bAlpha: Byte; dwFlags: DWORD): Boolean', False, ccStdCall);

  // Global variables
  TSepiVariable.Create(Result, 'SetLayeredWindowAttributes',
     @SetLayeredWindowAttributes, 'TSetLayeredWindowAttributes');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Forms', ImportUnit);
end.

