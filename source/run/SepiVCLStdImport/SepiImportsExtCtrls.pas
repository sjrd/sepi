{*
  Importe l'unité ExtCtrls dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsExtCtrls;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, Messages, Classes, Controls, Graphics, StdCtrls,
  Forms, ExtCtrls;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTShape = class(TShape)
  private
    procedure SetBrush(Value: TBrush);
    procedure SetPen(Value: TPen);
    procedure SetShape(Value: TShapeType);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTPaintBox = class(TPaintBox)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTImage = class(TImage)
  private
    function GetCanvas: TCanvas;
    procedure SetCenter(Value: Boolean);
    procedure SetPicture(Value: TPicture);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTBevel = class(TBevel)
  private
    procedure SetStyle(Value: TBevelStyle);
    procedure SetShape(Value: TBevelShape);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTTimer = class(TTimer)
  private
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTCustomPanel = class(TCustomPanel)
  private
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelWidth(Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTPanel = class(TPanel)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTPage = class(TPage)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTNotebook = class(TNotebook)
  private
    procedure SetPages(Value: TStrings);
    procedure SetActivePage(const Value: string);
    function GetActivePage: string;
    procedure SetPageIndex(Value: Integer);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTHeader = class(THeader)
  private
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetSections(Strings: TStrings);
    function GetWidth(X: Integer): Integer;
    procedure SetWidth(X: Integer; Value: Integer);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTCustomRadioGroup = class(TCustomRadioGroup)
  private
    function GetButtons(Index: Integer): TRadioButton;
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTRadioGroup = class(TRadioGroup)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTSplitter = class(TSplitter)
  private
    procedure SetBeveled(Value: Boolean);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTCustomControlBar = class(TCustomControlBar)
  private
    procedure SetPicture(const Value: TPicture);
    procedure SetRowSize(Value: TRowSize);
    procedure SetRowSnap(Value: Boolean);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTControlBar = class(TControlBar)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTBoundLabel = class(TBoundLabel)
  private
    function GetTop: Integer;
    function GetLeft: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTCustomLabeledEdit = class(TCustomLabeledEdit)
  private
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTLabeledEdit = class(TLabeledEdit)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTCustomColorBox = class(TCustomColorBox)
  private
    function GetColor(Index: Integer): TColor;
    function GetColorName(Index: Integer): string;
    function GetSelected: TColor;
    procedure SetSelected(const AColor: TColor);
    procedure SetDefaultColorColor(const Value: TColor);
    procedure SetNoneColorColor(const Value: TColor);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTColorBox = class(TColorBox)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTCustomColorListBox = class(TCustomColorListBox)
  private
    function GetColor(Index: Integer): TColor;
    function GetColorName(Index: Integer): string;
    function GetSelected: TColor;
    procedure SetSelected(const AColor: TColor);
    procedure SetDefaultColorColor(const Value: TColor);
    procedure SetNoneColorColor(const Value: TColor);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

  TSepiImportsTColorListBox = class(TColorListBox)
  private
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

{---------------}
{ TShape import }
{---------------}

procedure TSepiImportsTShape.SetBrush(Value: TBrush);
begin
  Brush := Value;
end;

procedure TSepiImportsTShape.SetPen(Value: TPen);
begin
  Pen := Value;
end;

procedure TSepiImportsTShape.SetShape(Value: TShapeType);
begin
  Shape := Value;
end;

class function TSepiImportsTShape.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TShape));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPen', System.TypeInfo(TPen));
    AddField('FBrush', System.TypeInfo(TBrush));
    AddField('FShape', System.TypeInfo(TShapeType));

    AddMethod('SetBrush', @TSepiImportsTShape.SetBrush,
      'procedure(Value: TBrush)');
    AddMethod('SetPen', @TSepiImportsTShape.SetPen,
      'procedure(Value: TPen)');
    AddMethod('SetShape', @TSepiImportsTShape.SetShape,
      'procedure(Value: TShapeType)');

    CurrentVisibility := mvProtected;

    AddMethod('Paint', @TSepiImportsTShape.Paint,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTShape.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTShape.Destroy,
      'destructor',
      mlkOverride);

    CurrentVisibility := mvPublished;

    AddMethod('StyleChanged', @TSepiImportsTShape.StyleChanged,
      'procedure(Sender: TObject)');

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    AddProperty('Brush', 'property: TBrush',
      'FBrush', 'SetBrush');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Constraints');
    RedefineProperty('ParentShowHint');
    AddProperty('Pen', 'property: TPen',
      'FPen', 'SetPen');
    AddProperty('Shape', 'property: TShapeType',
      'FShape', 'SetShape',
      NoIndex, Integer(stRectangle));
    RedefineProperty('ShowHint');
    RedefineProperty('Visible');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{------------------}
{ TPaintBox import }
{------------------}

class function TSepiImportsTPaintBox.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPaintBox));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOnPaint', System.TypeInfo(TNotifyEvent));

    CurrentVisibility := mvProtected;

    AddMethod('Paint', @TSepiImportsTPaintBox.Paint,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPaintBox.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    RedefineProperty('Canvas');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('Visible');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    AddProperty('OnPaint', 'property: TNotifyEvent',
      'FOnPaint', 'FOnPaint');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{---------------}
{ TImage import }
{---------------}

function TSepiImportsTImage.GetCanvas: TCanvas;
begin
  Result := Canvas;
end;

procedure TSepiImportsTImage.SetCenter(Value: Boolean);
begin
  Center := Value;
end;

procedure TSepiImportsTImage.SetPicture(Value: TPicture);
begin
  Picture := Value;
end;

procedure TSepiImportsTImage.SetStretch(Value: Boolean);
begin
  Stretch := Value;
end;

procedure TSepiImportsTImage.SetTransparent(Value: Boolean);
begin
  Transparent := Value;
end;

procedure TSepiImportsTImage.SetProportional(Value: Boolean);
begin
  Proportional := Value;
end;

class function TSepiImportsTImage.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TImage));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPicture', System.TypeInfo(TPicture));
    AddField('FOnProgress', System.TypeInfo(TProgressEvent));
    AddField('FStretch', System.TypeInfo(Boolean));
    AddField('FCenter', System.TypeInfo(Boolean));
    AddField('FIncrementalDisplay', System.TypeInfo(Boolean));
    AddField('FTransparent', System.TypeInfo(Boolean));
    AddField('FDrawing', System.TypeInfo(Boolean));
    AddField('FProportional', System.TypeInfo(Boolean));

    AddMethod('GetCanvas', @TSepiImportsTImage.GetCanvas,
      'function: TCanvas');
    AddMethod('PictureChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('SetCenter', @TSepiImportsTImage.SetCenter,
      'procedure(Value: Boolean)');
    AddMethod('SetPicture', @TSepiImportsTImage.SetPicture,
      'procedure(Value: TPicture)');
    AddMethod('SetStretch', @TSepiImportsTImage.SetStretch,
      'procedure(Value: Boolean)');
    AddMethod('SetTransparent', @TSepiImportsTImage.SetTransparent,
      'procedure(Value: Boolean)');
    AddMethod('SetProportional', @TSepiImportsTImage.SetProportional,
      'procedure(Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('CanAutoSize', @TSepiImportsTImage.CanAutoSize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('DestRect', @TSepiImportsTImage.DestRect,
      'function: TRect');
    AddMethod('DoPaletteChange', @TSepiImportsTImage.DoPaletteChange,
      'function: Boolean');
    AddMethod('GetPalette', @TSepiImportsTImage.GetPalette,
      'function: HPALETTE',
      mlkOverride);
    AddMethod('Paint', @TSepiImportsTImage.Paint,
      'procedure',
      mlkOverride);
    AddMethod('Progress', @TSepiImportsTImage.Progress,
      'procedure(Sender: TObject; Stage: TProgressStage; PercentDone: Byte ; RedrawNow: Boolean ; const R: TRect ; const Msg: string )',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTImage.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTImage.Destroy,
      'destructor',
      mlkOverride);

    AddProperty('Canvas', 'property: TCanvas',
      'GetCanvas', '');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('AutoSize');
    AddProperty('Center', 'property: Boolean',
      'FCenter', 'SetCenter',
      NoIndex, Integer(False));
    RedefineProperty('Constraints');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    AddProperty('IncrementalDisplay', 'property: Boolean',
      'FIncrementalDisplay', 'FIncrementalDisplay',
      NoIndex, Integer(False));
    RedefineProperty('ParentShowHint');
    AddProperty('Picture', 'property: TPicture',
      'FPicture', 'SetPicture');
    RedefineProperty('PopupMenu');
    AddProperty('Proportional', 'property: Boolean',
      'FProportional', 'SetProportional',
      NoIndex, Integer(false));
    RedefineProperty('ShowHint');
    AddProperty('Stretch', 'property: Boolean',
      'FStretch', 'SetStretch',
      NoIndex, Integer(False));
    AddProperty('Transparent', 'property: Boolean',
      'FTransparent', 'SetTransparent',
      NoIndex, Integer(False));
    RedefineProperty('Visible');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    AddProperty('OnProgress', 'property: TProgressEvent',
      'FOnProgress', 'FOnProgress');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{---------------}
{ TBevel import }
{---------------}

procedure TSepiImportsTBevel.SetStyle(Value: TBevelStyle);
begin
  Style := Value;
end;

procedure TSepiImportsTBevel.SetShape(Value: TBevelShape);
begin
  Shape := Value;
end;

class function TSepiImportsTBevel.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBevel));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStyle', System.TypeInfo(TBevelStyle));
    AddField('FShape', System.TypeInfo(TBevelShape));

    AddMethod('SetStyle', @TSepiImportsTBevel.SetStyle,
      'procedure(Value: TBevelStyle)');
    AddMethod('SetShape', @TSepiImportsTBevel.SetShape,
      'procedure(Value: TBevelShape)');

    CurrentVisibility := mvProtected;

    AddMethod('Paint', @TSepiImportsTBevel.Paint,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBevel.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('Constraints');
    RedefineProperty('ParentShowHint');
    AddProperty('Shape', 'property: TBevelShape',
      'FShape', 'SetShape',
      NoIndex, Integer(bsBox));
    RedefineProperty('ShowHint');
    AddProperty('Style', 'property: TBevelStyle',
      'FStyle', 'SetStyle',
      NoIndex, Integer(bsLowered));
    RedefineProperty('Visible');

    Complete;
  end;
end;

{---------------}
{ TTimer import }
{---------------}

procedure TSepiImportsTTimer.SetEnabled(Value: Boolean);
begin
  Enabled := Value;
end;

procedure TSepiImportsTTimer.SetInterval(Value: Cardinal);
begin
  Interval := Value;
end;

procedure TSepiImportsTTimer.SetOnTimer(Value: TNotifyEvent);
begin
  OnTimer := Value;
end;

class function TSepiImportsTTimer.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTimer));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FInterval', System.TypeInfo(Cardinal));
    AddField('FWindowHandle', System.TypeInfo(HWND));
    AddField('FOnTimer', System.TypeInfo(TNotifyEvent));
    AddField('FEnabled', System.TypeInfo(Boolean));

    AddMethod('UpdateTimer', nil,
      'procedure');
    AddMethod('SetEnabled', @TSepiImportsTTimer.SetEnabled,
      'procedure(Value: Boolean)');
    AddMethod('SetInterval', @TSepiImportsTTimer.SetInterval,
      'procedure(Value: Cardinal)');
    AddMethod('SetOnTimer', @TSepiImportsTTimer.SetOnTimer,
      'procedure(Value: TNotifyEvent)');
    AddMethod('WndProc', nil,
      'procedure(var Msg: TMessage)');

    CurrentVisibility := mvProtected;

    AddMethod('Timer', @TSepiImportsTTimer.Timer,
      'procedure',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTTimer.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTTimer.Destroy,
      'destructor',
      mlkOverride);

    CurrentVisibility := mvPublished;

    AddProperty('Enabled', 'property: Boolean',
      'FEnabled', 'SetEnabled',
      NoIndex, Integer(True));
    AddProperty('Interval', 'property: Cardinal',
      'FInterval', 'SetInterval',
      NoIndex, 1000);
    AddProperty('OnTimer', 'property: TNotifyEvent',
      'FOnTimer', 'SetOnTimer');

    Complete;
  end;
end;

{---------------------}
{ TCustomPanel import }
{---------------------}

procedure TSepiImportsTCustomPanel.SetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TSepiImportsTCustomPanel.SetBevelInner(Value: TPanelBevel);
begin
  BevelInner := Value;
end;

procedure TSepiImportsTCustomPanel.SetBevelOuter(Value: TPanelBevel);
begin
  BevelOuter := Value;
end;

procedure TSepiImportsTCustomPanel.SetBevelWidth(Value: TBevelWidth);
begin
  BevelWidth := Value;
end;

procedure TSepiImportsTCustomPanel.SetBorderWidth(Value: TBorderWidth);
begin
  BorderWidth := Value;
end;

procedure TSepiImportsTCustomPanel.SetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TSepiImportsTCustomPanel.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
  VerticalAlignment := Value;
end;

class function TSepiImportsTCustomPanel.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomPanel));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAutoSizeDocking', System.TypeInfo(Boolean));
    AddField('FBevelInner', System.TypeInfo(TPanelBevel));
    AddField('FBevelOuter', System.TypeInfo(TPanelBevel));
    AddField('FBevelWidth', System.TypeInfo(TBevelWidth));
    AddField('FBorderWidth', System.TypeInfo(TBorderWidth));
    AddField('FBorderStyle', System.TypeInfo(TBorderStyle));
    AddField('FFullRepaint', System.TypeInfo(Boolean));
    AddField('FLocked', System.TypeInfo(Boolean));
    AddField('FParentBackgroundSet', System.TypeInfo(Boolean));
    AddField('FAlignment', System.TypeInfo(TAlignment));
    AddField('FVerticalAlignment', System.TypeInfo(TVerticalAlignment));

    AddMethod('CMBorderChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BORDERCHANGED);
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('CMIsToolControl', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ISTOOLCONTROL);
    AddMethod('WMWindowPosChanged', nil,
      'procedure(var Message: TWMWindowPosChanged)',
      mlkMessage, False, WM_WINDOWPOSCHANGED);
    AddMethod('SetAlignment', @TSepiImportsTCustomPanel.SetAlignment,
      'procedure(Value: TAlignment)');
    AddMethod('SetBevelInner', @TSepiImportsTCustomPanel.SetBevelInner,
      'procedure(Value: TPanelBevel)');
    AddMethod('SetBevelOuter', @TSepiImportsTCustomPanel.SetBevelOuter,
      'procedure(Value: TPanelBevel)');
    AddMethod('SetBevelWidth', @TSepiImportsTCustomPanel.SetBevelWidth,
      'procedure(Value: TBevelWidth)');
    AddMethod('SetBorderWidth', @TSepiImportsTCustomPanel.SetBorderWidth,
      'procedure(Value: TBorderWidth)');
    AddMethod('SetBorderStyle', @TSepiImportsTCustomPanel.SetBorderStyle,
      'procedure(Value: TBorderStyle)');
    AddMethod('CMDockClient', nil,
      'procedure(var Message: TCMDockClient)',
      mlkMessage, False, CM_DOCKCLIENT);
    AddMethod('SetVerticalAlignment', @TSepiImportsTCustomPanel.SetVerticalAlignment,
      'procedure(const Value: TVerticalAlignment)');

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTCustomPanel.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('AdjustClientRect', @TSepiImportsTCustomPanel.AdjustClientRect,
      'procedure(var Rect: TRect)',
      mlkOverride);
    AddMethod('CanAutoSize', @TSepiImportsTCustomPanel.CanAutoSize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('Paint', @TSepiImportsTCustomPanel.Paint,
      'procedure',
      mlkOverride);

    AddProperty('Alignment', 'property: TAlignment',
      'FAlignment', 'SetAlignment',
      NoIndex, Integer(taCenter));
    AddProperty('BevelInner', 'property: TPanelBevel',
      'FBevelInner', 'SetBevelInner',
      NoIndex, Integer(bvNone));
    AddProperty('BevelOuter', 'property: TPanelBevel',
      'FBevelOuter', 'SetBevelOuter',
      NoIndex, Integer(bvRaised));
    AddProperty('BevelWidth', 'property: TBevelWidth',
      'FBevelWidth', 'SetBevelWidth',
      NoIndex, 1);
    AddProperty('BorderWidth', 'property: TBorderWidth',
      'FBorderWidth', 'SetBorderWidth',
      NoIndex, 0);
    AddProperty('BorderStyle', 'property: TBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(bsNone));
    RedefineProperty('Color',
      '', '', Integer(clBtnFace));
    AddProperty('FullRepaint', 'property: Boolean',
      'FFullRepaint', 'FFullRepaint',
      NoIndex, Integer(True));
    AddProperty('Locked', 'property: Boolean',
      'FLocked', 'FLocked',
      NoIndex, Integer(False));
    RedefineProperty('ParentColor',
      '', '', Integer(False));
    AddProperty('VerticalAlignment', 'property: TVerticalAlignment',
      'FVerticalAlignment', 'SetVerticalAlignment',
      NoIndex, Integer(taVerticalCenter));

    AddMethod('SetParentBackground', @TSepiImportsTCustomPanel.SetParentBackground,
      'procedure(Value: Boolean)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    RedefineProperty('ParentBackground',
      '', '', Integer(True), 'FParentBackgroundSet');

    AddMethod('Create', @TSepiImportsTCustomPanel.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('GetControlsAlignment', @TSepiImportsTCustomPanel.GetControlsAlignment,
      'function: TAlignment',
      mlkOverride);

    Complete;
  end;
end;

{---------------}
{ TPanel import }
{---------------}

class function TSepiImportsTPanel.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPanel));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    RedefineProperty('DockManager');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Alignment');
    RedefineProperty('Anchors');
    RedefineProperty('AutoSize');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelKind');
    RedefineProperty('BevelOuter');
    RedefineProperty('BevelWidth');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderWidth');
    RedefineProperty('BorderStyle');
    RedefineProperty('Caption');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('UseDockManager',
      '', '', Integer(True));
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('FullRepaint');
    RedefineProperty('Font');
    RedefineProperty('Locked');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentBackground');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('VerticalAlignment');
    RedefineProperty('Visible');
    RedefineProperty('OnAlignInsertBefore');
    RedefineProperty('OnAlignPosition');
    RedefineProperty('OnCanResize');
    RedefineProperty('OnClick');
    RedefineProperty('OnConstrainedResize');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDblClick');
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
    RedefineProperty('OnResize');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{--------------}
{ TPage import }
{--------------}

class function TSepiImportsTPage.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPage));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('WMNCHitTest', nil,
      'procedure(var Message: TWMNCHitTest)',
      mlkMessage, False, WM_NCHITTEST);

    CurrentVisibility := mvProtected;

    AddMethod('ReadState', @TSepiImportsTPage.ReadState,
      'procedure(Reader: TReader)',
      mlkOverride);
    AddMethod('Paint', @TSepiImportsTPage.Paint,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPage.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('Caption');
    RedefineProperty('Height',
      '', '', 'False');
    RedefineProperty('TabOrder',
      '', '', 'False');
    RedefineProperty('Visible',
      '', '', 'False');
    RedefineProperty('Width',
      '', '', 'False');
    RedefineProperty('OnAlignInsertBefore');
    RedefineProperty('OnAlignPosition');

    Complete;
  end;
end;

{------------------}
{ TNotebook import }
{------------------}

procedure TSepiImportsTNotebook.SetPages(Value: TStrings);
begin
  Pages := Value;
end;

procedure TSepiImportsTNotebook.SetActivePage(const Value: string);
begin
  ActivePage := Value;
end;

function TSepiImportsTNotebook.GetActivePage: string;
begin
  Result := ActivePage;
end;

procedure TSepiImportsTNotebook.SetPageIndex(Value: Integer);
begin
  PageIndex := Value;
end;

class function TSepiImportsTNotebook.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TNotebook));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPageList', System.TypeInfo(TList));
    AddField('FAccess', System.TypeInfo(TStrings));
    AddField('FPageIndex', System.TypeInfo(Integer));
    AddField('FOnPageChanged', System.TypeInfo(TNotifyEvent));

    AddMethod('SetPages', @TSepiImportsTNotebook.SetPages,
      'procedure(Value: TStrings)');
    AddMethod('SetActivePage', @TSepiImportsTNotebook.SetActivePage,
      'procedure(const Value: string)');
    AddMethod('GetActivePage', @TSepiImportsTNotebook.GetActivePage,
      'function: string');
    AddMethod('SetPageIndex', @TSepiImportsTNotebook.SetPageIndex,
      'procedure(Value: Integer)');

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTNotebook.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('GetChildOwner', @TSepiImportsTNotebook.GetChildOwner,
      'function: TComponent',
      mlkOverride);
    AddMethod('GetChildren', @TSepiImportsTNotebook.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('ReadState', @TSepiImportsTNotebook.ReadState,
      'procedure(Reader: TReader)',
      mlkOverride);
    AddMethod('ShowControl', @TSepiImportsTNotebook.ShowControl,
      'procedure(AControl: TControl)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTNotebook.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTNotebook.Destroy,
      'destructor',
      mlkOverride);

    CurrentVisibility := mvPublished;

    AddProperty('ActivePage', 'property: string',
      'GetActivePage', 'SetActivePage',
      NoIndex, NoDefaultValue, 'False');
    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('Color');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Font');
    RedefineProperty('Enabled');
    RedefineProperty('Constraints');
    AddProperty('PageIndex', 'property: Integer',
      'FPageIndex', 'SetPageIndex',
      NoIndex, 0);
    AddProperty('Pages', 'property: TStrings',
      'FAccess', 'SetPages',
      NoIndex, NoDefaultValue, 'False');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    AddProperty('OnPageChanged', 'property: TNotifyEvent',
      'FOnPageChanged', 'FOnPageChanged');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{----------------}
{ THeader import }
{----------------}

procedure TSepiImportsTHeader.SetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TSepiImportsTHeader.SetSections(Strings: TStrings);
begin
  Sections := Strings;
end;

function TSepiImportsTHeader.GetWidth(X: Integer): Integer;
begin
  Result := SectionWidth[X];
end;

procedure TSepiImportsTHeader.SetWidth(X: Integer; Value: Integer);
begin
  SectionWidth[X] := Value;
end;

class function TSepiImportsTHeader.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(THeader));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FSections', System.TypeInfo(TStrings));
    AddField('FHitTest', 'TPoint');
    AddField('FCanResize', System.TypeInfo(Boolean));
    AddField('FAllowResize', System.TypeInfo(Boolean));
    AddField('FBorderStyle', System.TypeInfo(TBorderStyle));
    AddField('FResizeSection', System.TypeInfo(Integer));
    AddField('FMouseOffset', System.TypeInfo(Integer));
    AddField('FOnSizing', System.TypeInfo(TSectionEvent));
    AddField('FOnSized', System.TypeInfo(TSectionEvent));

    AddMethod('SetBorderStyle', @TSepiImportsTHeader.SetBorderStyle,
      'procedure(Value: TBorderStyle)');
    AddMethod('FreeSections', nil,
      'procedure');
    AddMethod('SetSections', @TSepiImportsTHeader.SetSections,
      'procedure(Strings: TStrings)');
    AddMethod('GetWidth', @TSepiImportsTHeader.GetWidth,
      'function(X: Integer): Integer');
    AddMethod('SetWidth', @TSepiImportsTHeader.SetWidth,
      'procedure(X: Integer; Value: Integer)');
    AddMethod('WMSetCursor', nil,
      'procedure(var Msg: TWMSetCursor)',
      mlkMessage, False, WM_SETCURSOR);
    AddMethod('WMNCHitTest', nil,
      'procedure(var Msg: TWMNCHitTest)',
      mlkMessage, False, WM_NCHITTEST);
    AddMethod('WMSize', nil,
      'procedure(var Msg: TWMSize)',
      mlkMessage, False, WM_SIZE);

    CurrentVisibility := mvProtected;

    AddMethod('Paint', @TSepiImportsTHeader.Paint,
      'procedure',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTHeader.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('MouseDown', @TSepiImportsTHeader.MouseDown,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer )',
      mlkOverride);
    AddMethod('MouseMove', @TSepiImportsTHeader.MouseMove,
      'procedure(Shift: TShiftState; X, Y: Integer)',
      mlkOverride);
    AddMethod('MouseUp', @TSepiImportsTHeader.MouseUp,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer )',
      mlkOverride);
    AddMethod('Sizing', @TSepiImportsTHeader.Sizing,
      'procedure(ASection, AWidth: Integer)',
      mlkDynamic);
    AddMethod('Sized', @TSepiImportsTHeader.Sized,
      'procedure(ASection, AWidth: Integer)',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTHeader.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTHeader.Destroy,
      'destructor',
      mlkOverride);

    AddProperty('SectionWidth', 'property[X: Integer]: Integer',
      'GetWidth', 'SetWidth');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    AddProperty('AllowResize', 'property: Boolean',
      'FAllowResize', 'FAllowResize',
      NoIndex, Integer(True));
    RedefineProperty('Anchors');
    AddProperty('BorderStyle', 'property: TBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(bsSingle));
    RedefineProperty('Constraints');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    AddProperty('Sections', 'property: TStrings',
      'FSections', 'SetSections');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('OnContextPopup');
    AddProperty('OnSizing', 'property: TSectionEvent',
      'FOnSizing', 'FOnSizing');
    AddProperty('OnSized', 'property: TSectionEvent',
      'FOnSized', 'FOnSized');

    Complete;
  end;
end;

{--------------------------}
{ TCustomRadioGroup import }
{--------------------------}

function TSepiImportsTCustomRadioGroup.GetButtons(Index: Integer): TRadioButton;
begin
  Result := Buttons[Index];
end;

procedure TSepiImportsTCustomRadioGroup.SetColumns(Value: Integer);
begin
  Columns := Value;
end;

procedure TSepiImportsTCustomRadioGroup.SetItemIndex(Value: Integer);
begin
  ItemIndex := Value;
end;

procedure TSepiImportsTCustomRadioGroup.SetItems(Value: TStrings);
begin
  Items := Value;
end;

class function TSepiImportsTCustomRadioGroup.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomRadioGroup));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FButtons', System.TypeInfo(TList));
    AddField('FItems', System.TypeInfo(TStrings));
    AddField('FItemIndex', System.TypeInfo(Integer));
    AddField('FColumns', System.TypeInfo(Integer));
    AddField('FReading', System.TypeInfo(Boolean));
    AddField('FUpdating', System.TypeInfo(Boolean));

    AddMethod('GetButtons', @TSepiImportsTCustomRadioGroup.GetButtons,
      'function(Index: Integer): TRadioButton');
    AddMethod('ArrangeButtons', nil,
      'procedure');
    AddMethod('ButtonClick', nil,
      'procedure(Sender: TObject)');
    AddMethod('ItemsChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('SetButtonCount', nil,
      'procedure(Value: Integer)');
    AddMethod('SetColumns', @TSepiImportsTCustomRadioGroup.SetColumns,
      'procedure(Value: Integer)');
    AddMethod('SetItemIndex', @TSepiImportsTCustomRadioGroup.SetItemIndex,
      'procedure(Value: Integer)');
    AddMethod('SetItems', @TSepiImportsTCustomRadioGroup.SetItems,
      'procedure(Value: TStrings)');
    AddMethod('UpdateButtons', nil,
      'procedure');
    AddMethod('CMEnabledChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ENABLEDCHANGED);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);

    CurrentVisibility := mvProtected;

    AddMethod('Loaded', @TSepiImportsTCustomRadioGroup.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('ReadState', @TSepiImportsTCustomRadioGroup.ReadState,
      'procedure(Reader: TReader)',
      mlkOverride);
    AddMethod('CanModify', @TSepiImportsTCustomRadioGroup.CanModify,
      'function: Boolean',
      mlkVirtual);
    AddMethod('GetChildren', @TSepiImportsTCustomRadioGroup.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);

    AddProperty('Columns', 'property: Integer',
      'FColumns', 'SetColumns',
      NoIndex, 1);
    AddProperty('ItemIndex', 'property: Integer',
      'FItemIndex', 'SetItemIndex',
      NoIndex, -1);
    AddProperty('Items', 'property: TStrings',
      'FItems', 'SetItems');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomRadioGroup.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomRadioGroup.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('FlipChildren', @TSepiImportsTCustomRadioGroup.FlipChildren,
      'procedure(AllLevels: Boolean)',
      mlkOverride);

    AddProperty('Buttons', 'property[Index: Integer]: TRadioButton',
      'GetButtons', '');

    Complete;
  end;
end;

{--------------------}
{ TRadioGroup import }
{--------------------}

class function TSepiImportsTRadioGroup.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TRadioGroup));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('Caption');
    RedefineProperty('Color');
    RedefineProperty('Columns');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ItemIndex');
    RedefineProperty('Items');
    RedefineProperty('Constraints');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentBackground',
      '', '', Integer(True));
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{------------------}
{ TSplitter import }
{------------------}

procedure TSepiImportsTSplitter.SetBeveled(Value: Boolean);
begin
  Beveled := Value;
end;

class function TSepiImportsTSplitter.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSplitter));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FActiveControl', System.TypeInfo(TWinControl));
    AddField('FAutoSnap', System.TypeInfo(Boolean));
    AddField('FBeveled', System.TypeInfo(Boolean));
    AddField('FBrush', System.TypeInfo(TBrush));
    AddField('FControl', System.TypeInfo(TControl));
    AddField('FDownPos', 'TPoint');
    AddField('FLineDC', System.TypeInfo(HDC));
    AddField('FLineVisible', System.TypeInfo(Boolean));
    AddField('FMinSize', System.TypeInfo(NaturalNumber));
    AddField('FMaxSize', System.TypeInfo(Integer));
    AddField('FNewSize', System.TypeInfo(Integer));
    AddField('FOldKeyDown', System.TypeInfo(TKeyEvent));
    AddField('FOldSize', System.TypeInfo(Integer));
    AddField('FPrevBrush', System.TypeInfo(HBrush));
    AddField('FResizeStyle', System.TypeInfo(TResizeStyle));
    AddField('FSplit', System.TypeInfo(Integer));
    AddField('FOnCanResize', System.TypeInfo(TCanResizeEvent));
    AddField('FOnMoved', System.TypeInfo(TNotifyEvent));
    AddField('FOnPaint', System.TypeInfo(TNotifyEvent));

    AddMethod('AllocateLineDC', nil,
      'procedure');
    AddMethod('CalcSplitSize', nil,
      'procedure(X, Y: Integer; var NewSize, Split: Integer)');
    AddMethod('DrawLine', nil,
      'procedure');
    AddMethod('FindControl', nil,
      'function: TControl');
    AddMethod('FocusKeyDown', nil,
      'procedure(Sender: TObject; var Key: Word; Shift: TShiftState)');
    AddMethod('ReleaseLineDC', nil,
      'procedure');
    AddMethod('SetBeveled', @TSepiImportsTSplitter.SetBeveled,
      'procedure(Value: Boolean)');
    AddMethod('UpdateControlSize', nil,
      'procedure');
    AddMethod('UpdateSize', nil,
      'procedure(X, Y: Integer)');

    CurrentVisibility := mvProtected;

    AddMethod('CanResize', @TSepiImportsTSplitter.CanResize,
      'function(var NewSize: Integer): Boolean',
      mlkVirtual);
    AddMethod('DoCanResize', @TSepiImportsTSplitter.DoCanResize,
      'function(var NewSize: Integer): Boolean',
      mlkVirtual);
    AddMethod('MouseDown', @TSepiImportsTSplitter.MouseDown,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer )',
      mlkOverride);
    AddMethod('MouseMove', @TSepiImportsTSplitter.MouseMove,
      'procedure(Shift: TShiftState; X, Y: Integer)',
      mlkOverride);
    AddMethod('MouseUp', @TSepiImportsTSplitter.MouseUp,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer )',
      mlkOverride);
    AddMethod('Paint', @TSepiImportsTSplitter.Paint,
      'procedure',
      mlkOverride);
    AddMethod('RequestAlign', @TSepiImportsTSplitter.RequestAlign,
      'procedure',
      mlkOverride);
    AddMethod('StopSizing', @TSepiImportsTSplitter.StopSizing,
      'procedure',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTSplitter.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTSplitter.Destroy,
      'destructor',
      mlkOverride);

    RedefineProperty('Canvas');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align',
      '', '', Integer(alLeft));
    AddProperty('AutoSnap', 'property: Boolean',
      'FAutoSnap', 'FAutoSnap',
      NoIndex, Integer(True));
    AddProperty('Beveled', 'property: Boolean',
      'FBeveled', 'SetBeveled',
      NoIndex, Integer(False));
    RedefineProperty('Color');
    RedefineProperty('Cursor',
      '', '', Integer(crHSplit));
    RedefineProperty('Constraints');
    AddProperty('MinSize', 'property: NaturalNumber',
      'FMinSize', 'FMinSize',
      NoIndex, 30);
    RedefineProperty('ParentColor');
    AddProperty('ResizeStyle', 'property: TResizeStyle',
      'FResizeStyle', 'FResizeStyle',
      NoIndex, Integer(rsPattern));
    RedefineProperty('Visible');
    RedefineProperty('Width',
      '', '', 3);
    AddProperty('OnCanResize', 'property: TCanResizeEvent',
      'FOnCanResize', 'FOnCanResize');
    AddProperty('OnMoved', 'property: TNotifyEvent',
      'FOnMoved', 'FOnMoved');
    AddProperty('OnPaint', 'property: TNotifyEvent',
      'FOnPaint', 'FOnPaint');

    Complete;
  end;
end;

{--------------------------}
{ TCustomControlBar import }
{--------------------------}

procedure TSepiImportsTCustomControlBar.SetPicture(const Value: TPicture);
begin
  Picture := Value;
end;

procedure TSepiImportsTCustomControlBar.SetRowSize(Value: TRowSize);
begin
  RowSize := Value;
end;

procedure TSepiImportsTCustomControlBar.SetRowSnap(Value: Boolean);
begin
  RowSnap := Value;
end;

class function TSepiImportsTCustomControlBar.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomControlBar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAligning', System.TypeInfo(Boolean));
    AddField('FAutoDrag', System.TypeInfo(Boolean));
    AddField('FAutoDock', System.TypeInfo(Boolean));
    AddField('FDockingControl', System.TypeInfo(TControl));
    AddField('FDragControl', System.TypeInfo(TControl));
    AddField('FDragOffset', 'TPoint');
    AddField('FDrawing', System.TypeInfo(Boolean));
    AddField('FFloating', System.TypeInfo(Boolean));
    AddField('FItems', System.TypeInfo(TList));
    AddField('FPicture', System.TypeInfo(TPicture));
    AddField('FRowSize', System.TypeInfo(TRowSize));
    AddField('FRowSnap', System.TypeInfo(Boolean));
    AddField('FOnBandDrag', System.TypeInfo(TBandDragEvent));
    AddField('FOnBandInfo', System.TypeInfo(TBandInfoEvent));
    AddField('FOnBandMove', System.TypeInfo(TBandMoveEvent));
    AddField('FOnBandPaint', System.TypeInfo(TBandPaintEvent));
    AddField('FOnBeginBandMove', System.TypeInfo(TBeginBandMoveEvent));
    AddField('FOnEndBandMove', System.TypeInfo(TEndBandMoveEvent));
    AddField('FOnPaint', System.TypeInfo(TNotifyEvent));

    AddMethod('DoAlignControl', nil,
      'procedure(AControl: TControl)');
    AddMethod('FindPos', nil,
      'function(AControl: TControl): Pointer');
    AddMethod('HitTest2', nil,
      'function(X, Y: Integer): Pointer');
    AddMethod('DockControl', nil,
      'procedure(AControl: TControl; const ARect: TRect; BreakList, IndexList, SizeList: TList ; Parent: Pointer ; ChangedPriorBreak: Boolean ; Insets: TRect ; PreferredSize, RowCount : Integer ; Existing: Boolean )');
    AddMethod('PictureChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('SetPicture', @TSepiImportsTCustomControlBar.SetPicture,
      'procedure(const Value: TPicture)');
    AddMethod('SetRowSize', @TSepiImportsTCustomControlBar.SetRowSize,
      'procedure(Value: TRowSize)');
    AddMethod('SetRowSnap', @TSepiImportsTCustomControlBar.SetRowSnap,
      'procedure(Value: Boolean)');
    AddMethod('UnDockControl', nil,
      'procedure(AControl: TControl)');
    AddMethod('UpdateItems', nil,
      'function(AControl: TControl): Boolean');
    AddMethod('CMControlListChange', nil,
      'procedure(var Message: TCMControlListChange)',
      mlkMessage, False, CM_CONTROLLISTCHANGE);
    AddMethod('CMDesignHitTest', nil,
      'procedure(var Message: TCMDesignHitTest)',
      mlkMessage, False, CM_DESIGNHITTEST);
    AddMethod('CNKeyDown', nil,
      'procedure(var Message: TWMKeyDown)',
      mlkMessage, False, CN_KEYDOWN);
    AddMethod('WMEraseBkgnd', nil,
      'procedure(var Message: TWmEraseBkgnd)',
      mlkMessage, False, WM_ERASEBKGND);

    CurrentVisibility := mvProtected;

    AddMethod('AlignControls', @TSepiImportsTCustomControlBar.AlignControls,
      'procedure(AControl: TControl; var ARect: TRect)',
      mlkOverride);
    AddMethod('CanAutoSize', @TSepiImportsTCustomControlBar.CanAutoSize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTCustomControlBar.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('DoBandMove', @TSepiImportsTCustomControlBar.DoBandMove,
      'procedure(Control: TControl; var ARect: TRect)',
      mlkVirtual);
    AddMethod('DoBandPaint', @TSepiImportsTCustomControlBar.DoBandPaint,
      'procedure(Control: TControl; Canvas: TCanvas; var ARect: TRect; var Options: TBandPaintOptions )',
      mlkVirtual);
    AddMethod('DoBeginBandMove', @TSepiImportsTCustomControlBar.DoBeginBandMove,
      'function(Control: TControl): Boolean',
      mlkDynamic);
    AddMethod('DoEndBandMove', @TSepiImportsTCustomControlBar.DoEndBandMove,
      'procedure(Control: TControl)',
      mlkDynamic);
    AddMethod('DockOver', @TSepiImportsTCustomControlBar.DockOver,
      'procedure(Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean )',
      mlkOverride);
    AddMethod('DoPaletteChange', @TSepiImportsTCustomControlBar.DoPaletteChange,
      'function: Boolean');
    AddMethod('DragControl', @TSepiImportsTCustomControlBar.DragControl,
      'function(AControl: TControl; X, Y: Integer; KeepCapture: Boolean = False ) : Boolean',
      mlkVirtual);
    AddMethod('GetControlInfo', @TSepiImportsTCustomControlBar.GetControlInfo,
      'procedure(AControl: TControl; var Insets: TRect; var PreferredSize, RowCount: Integer )',
      mlkVirtual);
    AddMethod('GetPalette', @TSepiImportsTCustomControlBar.GetPalette,
      'function: HPALETTE',
      mlkOverride);
    AddMethod('GetSiteInfo', @TSepiImportsTCustomControlBar.GetSiteInfo,
      'procedure(Client: TControl; var InfluenceRect: TRect; MousePos: TPoint ; var CanDock: Boolean )',
      mlkOverride);
    AddMethod('HitTest', @TSepiImportsTCustomControlBar.HitTest,
      'function(X, Y: Integer): TControl');
    AddMethod('MouseDown', @TSepiImportsTCustomControlBar.MouseDown,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer )',
      mlkOverride);
    AddMethod('MouseMove', @TSepiImportsTCustomControlBar.MouseMove,
      'procedure(Shift: TShiftState; X, Y: Integer)',
      mlkOverride);
    AddMethod('MouseUp', @TSepiImportsTCustomControlBar.MouseUp,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer )',
      mlkOverride);
    AddMethod('Paint', @TSepiImportsTCustomControlBar.Paint,
      'procedure',
      mlkOverride);
    AddMethod('PaintControlFrame', @TSepiImportsTCustomControlBar.PaintControlFrame,
      'procedure(Canvas: TCanvas; AControl: TControl; var ARect: TRect )',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomControlBar.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomControlBar.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('FlipChildren', @TSepiImportsTCustomControlBar.FlipChildren,
      'procedure(AllLevels: Boolean)',
      mlkOverride);
    AddMethod('StickControls', @TSepiImportsTCustomControlBar.StickControls,
      'procedure',
      mlkVirtual);

    AddProperty('Picture', 'property: TPicture',
      'FPicture', 'SetPicture');

    CurrentVisibility := mvProtected;

    AddProperty('AutoDock', 'property: Boolean',
      'FAutoDock', 'FAutoDock',
      NoIndex, Integer(True));
    AddProperty('AutoDrag', 'property: Boolean',
      'FAutoDrag', 'FAutoDrag',
      NoIndex, Integer(True));
    RedefineProperty('AutoSize');
    RedefineProperty('BevelKind',
      '', '', Integer(bkTile));
    RedefineProperty('DockSite',
      '', '', Integer(True));
    AddProperty('RowSize', 'property: TRowSize',
      'FRowSize', 'SetRowSize',
      NoIndex, 26);
    AddProperty('RowSnap', 'property: Boolean',
      'FRowSnap', 'SetRowSnap',
      NoIndex, Integer(True));
    AddProperty('OnBandDrag', 'property: TBandDragEvent',
      'FOnBandDrag', 'FOnBandDrag');
    AddProperty('OnBandInfo', 'property: TBandInfoEvent',
      'FOnBandInfo', 'FOnBandInfo');
    AddProperty('OnBandMove', 'property: TBandMoveEvent',
      'FOnBandMove', 'FOnBandMove');
    AddProperty('OnBandPaint', 'property: TBandPaintEvent',
      'FOnBandPaint', 'FOnBandPaint');
    AddProperty('OnBeginBandMove', 'property: TBeginBandMoveEvent',
      'FOnBeginBandMove', 'FOnBeginBandMove');
    AddProperty('OnEndBandMove', 'property: TEndBandMoveEvent',
      'FOnEndBandMove', 'FOnEndBandMove');
    AddProperty('OnPaint', 'property: TNotifyEvent',
      'FOnPaint', 'FOnPaint');

    Complete;
  end;
end;

{--------------------}
{ TControlBar import }
{--------------------}

class function TSepiImportsTControlBar.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TControlBar));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    RedefineProperty('Canvas');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('AutoDock');
    RedefineProperty('AutoDrag');
    RedefineProperty('AutoSize');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelOuter');
    RedefineProperty('BevelKind');
    RedefineProperty('BevelWidth');
    RedefineProperty('BorderWidth');
    RedefineProperty('Color',
      '', '', NoDefaultValue);
    RedefineProperty('Constraints');
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('ParentBackground',
      '', '', Integer(True));
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('Picture');
    RedefineProperty('PopupMenu');
    RedefineProperty('RowSize');
    RedefineProperty('RowSnap');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('OnAlignInsertBefore');
    RedefineProperty('OnAlignPosition');
    RedefineProperty('OnBandDrag');
    RedefineProperty('OnBandInfo');
    RedefineProperty('OnBandMove');
    RedefineProperty('OnBandPaint');
    RedefineProperty('OnBeginBandMove');
    RedefineProperty('OnEndBandMove');
    RedefineProperty('OnCanResize');
    RedefineProperty('OnClick');
    RedefineProperty('OnConstrainedResize');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDblClick');
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
    RedefineProperty('OnPaint');
    RedefineProperty('OnResize');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{--------------------}
{ TBoundLabel import }
{--------------------}

function TSepiImportsTBoundLabel.GetTop: Integer;
begin
  Result := Top;
end;

function TSepiImportsTBoundLabel.GetLeft: Integer;
begin
  Result := Left;
end;

function TSepiImportsTBoundLabel.GetWidth: Integer;
begin
  Result := Width;
end;

function TSepiImportsTBoundLabel.GetHeight: Integer;
begin
  Result := Height;
end;

procedure TSepiImportsTBoundLabel.SetHeight(const Value: Integer);
begin
  Height := Value;
end;

procedure TSepiImportsTBoundLabel.SetWidth(const Value: Integer);
begin
  Width := Value;
end;

class function TSepiImportsTBoundLabel.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBoundLabel));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetTop', @TSepiImportsTBoundLabel.GetTop,
      'function: Integer');
    AddMethod('GetLeft', @TSepiImportsTBoundLabel.GetLeft,
      'function: Integer');
    AddMethod('GetWidth', @TSepiImportsTBoundLabel.GetWidth,
      'function: Integer');
    AddMethod('GetHeight', @TSepiImportsTBoundLabel.GetHeight,
      'function: Integer');
    AddMethod('SetHeight', @TSepiImportsTBoundLabel.SetHeight,
      'procedure(const Value: Integer)');
    AddMethod('SetWidth', @TSepiImportsTBoundLabel.SetWidth,
      'procedure(const Value: Integer)');

    CurrentVisibility := mvProtected;

    AddMethod('AdjustBounds', @TSepiImportsTBoundLabel.AdjustBounds,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBoundLabel.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('BiDiMode');
    RedefineProperty('Caption');
    RedefineProperty('Color');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Font');
    AddProperty('Height', 'property: Integer',
      'GetHeight', 'SetHeight');
    AddProperty('Left', 'property: Integer',
      'GetLeft', '');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowAccelChar');
    RedefineProperty('ShowHint');
    AddProperty('Top', 'property: Integer',
      'GetTop', '');
    RedefineProperty('Transparent');
    RedefineProperty('Layout');
    RedefineProperty('WordWrap');
    AddProperty('Width', 'property: Integer',
      'GetWidth', 'SetWidth');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{---------------------------}
{ TCustomLabeledEdit import }
{---------------------------}

procedure TSepiImportsTCustomLabeledEdit.SetLabelPosition(const Value: TLabelPosition);
begin
  LabelPosition := Value;
end;

procedure TSepiImportsTCustomLabeledEdit.SetLabelSpacing(const Value: Integer);
begin
  LabelSpacing := Value;
end;

class function TSepiImportsTCustomLabeledEdit.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomLabeledEdit));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FEditLabel', System.TypeInfo(TBoundLabel));
    AddField('FLabelPosition', System.TypeInfo(TLabelPosition));
    AddField('FLabelSpacing', System.TypeInfo(Integer));

    AddMethod('SetLabelPosition', @TSepiImportsTCustomLabeledEdit.SetLabelPosition,
      'procedure(const Value: TLabelPosition)');
    AddMethod('SetLabelSpacing', @TSepiImportsTCustomLabeledEdit.SetLabelSpacing,
      'procedure(const Value: Integer)');

    CurrentVisibility := mvProtected;

    AddMethod('SetParent', @TSepiImportsTCustomLabeledEdit.SetParent,
      'procedure(AParent: TWinControl)',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTCustomLabeledEdit.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('SetName', @TSepiImportsTCustomLabeledEdit.SetName,
      'procedure(const Value: TComponentName)',
      mlkOverride);
    AddMethod('CMVisiblechanged', @TSepiImportsTCustomLabeledEdit.CMVisiblechanged,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_VISIBLECHANGED);
    AddMethod('CMEnabledchanged', @TSepiImportsTCustomLabeledEdit.CMEnabledchanged,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ENABLEDCHANGED);
    AddMethod('CMBidimodechanged', @TSepiImportsTCustomLabeledEdit.CMBidimodechanged,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BIDIMODECHANGED);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomLabeledEdit.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('SetBounds', @TSepiImportsTCustomLabeledEdit.SetBounds,
      'procedure(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer)',
      mlkOverride);
    AddMethod('SetupInternalLabel', @TSepiImportsTCustomLabeledEdit.SetupInternalLabel,
      'procedure');

    AddProperty('EditLabel', 'property: TBoundLabel',
      'FEditLabel', '');
    AddProperty('LabelPosition', 'property: TLabelPosition',
      'FLabelPosition', 'SetLabelPosition');
    AddProperty('LabelSpacing', 'property: Integer',
      'FLabelSpacing', 'SetLabelSpacing');

    Complete;
  end;
end;

{---------------------}
{ TLabeledEdit import }
{---------------------}

class function TSepiImportsTLabeledEdit.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TLabeledEdit));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Anchors');
    RedefineProperty('AutoSelect');
    RedefineProperty('AutoSize');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelKind');
    RedefineProperty('BevelOuter');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderStyle');
    RedefineProperty('CharCase');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('EditLabel');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('HideSelection');
    RedefineProperty('ImeMode');
    RedefineProperty('ImeName');
    RedefineProperty('LabelPosition',
      '', '', Integer(lpAbove));
    RedefineProperty('LabelSpacing',
      '', '', 3);
    RedefineProperty('MaxLength');
    RedefineProperty('OEMConvert');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PasswordChar');
    RedefineProperty('PopupMenu');
    RedefineProperty('ReadOnly');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Text');
    RedefineProperty('Visible');
    RedefineProperty('OnChange');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{------------------------}
{ TCustomColorBox import }
{------------------------}

function TSepiImportsTCustomColorBox.GetColor(Index: Integer): TColor;
begin
  Result := Colors[Index];
end;

function TSepiImportsTCustomColorBox.GetColorName(Index: Integer): string;
begin
  Result := ColorNames[Index];
end;

function TSepiImportsTCustomColorBox.GetSelected: TColor;
begin
  Result := Selected;
end;

procedure TSepiImportsTCustomColorBox.SetSelected(const AColor: TColor);
begin
  Selected := AColor;
end;

procedure TSepiImportsTCustomColorBox.SetDefaultColorColor(const Value: TColor);
begin
  DefaultColorColor := Value;
end;

procedure TSepiImportsTCustomColorBox.SetNoneColorColor(const Value: TColor);
begin
  NoneColorColor := Value;
end;

class function TSepiImportsTCustomColorBox.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
const
  DefaultStyle : TColorBoxStyle =
    [cbStandardColors, cbExtendedColors, cbSystemColors];
begin
  Result := TSepiClass(Owner.FindMeta('TCustomColorBox'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomColorBox));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStyle', System.TypeInfo(TColorBoxStyle));
    AddField('FNeedToPopulate', System.TypeInfo(Boolean));
    AddField('FListSelected', System.TypeInfo(Boolean));
    AddField('FDefaultColorColor', System.TypeInfo(TColor));
    AddField('FNoneColorColor', System.TypeInfo(TColor));
    AddField('FSelectedColor', System.TypeInfo(TColor));
    AddField('FOnGetColors', System.TypeInfo(TGetColorsEvent));

    AddMethod('GetColor', @TSepiImportsTCustomColorBox.GetColor,
      'function(Index: Integer): TColor');
    AddMethod('GetColorName', @TSepiImportsTCustomColorBox.GetColorName,
      'function(Index: Integer): string');
    AddMethod('GetSelected', @TSepiImportsTCustomColorBox.GetSelected,
      'function: TColor');
    AddMethod('SetSelected', @TSepiImportsTCustomColorBox.SetSelected,
      'procedure(const AColor: TColor)');
    AddMethod('ColorCallBack', nil,
      'procedure(const AName: string)');
    AddMethod('SetDefaultColorColor', @TSepiImportsTCustomColorBox.SetDefaultColorColor,
      'procedure(const Value: TColor)');
    AddMethod('SetNoneColorColor', @TSepiImportsTCustomColorBox.SetNoneColorColor,
      'procedure(const Value: TColor)');

    CurrentVisibility := mvProtected;

    AddMethod('CloseUp', @TSepiImportsTCustomColorBox.CloseUp,
      'procedure',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomColorBox.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DrawItem', @TSepiImportsTCustomColorBox.DrawItem,
      'procedure(Index: Integer; Rect: TRect; State: TOwnerDrawState)',
      mlkOverride);
    AddMethod('KeyDown', @TSepiImportsTCustomColorBox.KeyDown,
      'procedure(var Key: Word; Shift: TShiftState)',
      mlkOverride);
    AddMethod('KeyPress', @TSepiImportsTCustomColorBox.KeyPress,
      'procedure(var Key: Char)',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTCustomColorBox.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('PickCustomColor', @TSepiImportsTCustomColorBox.PickCustomColor,
      'function: Boolean',
      mlkVirtual);
    AddMethod('PopulateList', @TSepiImportsTCustomColorBox.PopulateList,
      'procedure');
    AddMethod('Select', @TSepiImportsTCustomColorBox.Select,
      'procedure',
      mlkOverride);
    AddMethod('SetStyle', @TSepiImportsTCustomColorBox.SetStyle,
      'procedure(AStyle: TColorBoxStyle)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomColorBox.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    AddProperty('Style', 'property: TColorBoxStyle',
      'FStyle', 'SetStyle',
      NoIndex, Byte(DefaultStyle));
    AddProperty('Colors', 'property[Index: Integer]: TColor',
      'GetColor', '');
    AddProperty('ColorNames', 'property[Index: Integer]: string',
      'GetColorName', '');
    AddProperty('Selected', 'property: TColor',
      'GetSelected', 'SetSelected',
      NoIndex, Integer(clBlack));
    AddProperty('DefaultColorColor', 'property: TColor',
      'FDefaultColorColor', 'SetDefaultColorColor',
      NoIndex, Integer(clBlack));
    AddProperty('NoneColorColor', 'property: TColor',
      'FNoneColorColor', 'SetNoneColorColor',
      NoIndex, Integer(clBlack));
    AddProperty('OnGetColors', 'property: TGetColorsEvent',
      'FOnGetColors', 'FOnGetColors');

    Complete;
  end;
end;

{------------------}
{ TColorBox import }
{------------------}

class function TSepiImportsTColorBox.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TColorBox));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('AutoComplete');
    RedefineProperty('AutoDropDown');
    RedefineProperty('DefaultColorColor');
    RedefineProperty('NoneColorColor');
    RedefineProperty('Selected');
    RedefineProperty('Style');
    RedefineProperty('Anchors');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelKind');
    RedefineProperty('BevelOuter');
    RedefineProperty('BiDiMode');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DropDownCount');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ItemHeight');
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
    RedefineProperty('OnChange');
    RedefineProperty('OnCloseUp');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnDropDown');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnGetColors');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnSelect');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{----------------------------}
{ TCustomColorListBox import }
{----------------------------}

function TSepiImportsTCustomColorListBox.GetColor(Index: Integer): TColor;
begin
  Result := Colors[Index];
end;

function TSepiImportsTCustomColorListBox.GetColorName(Index: Integer): string;
begin
  Result := ColorNames[Index];
end;

function TSepiImportsTCustomColorListBox.GetSelected: TColor;
begin
  Result := Selected;
end;

procedure TSepiImportsTCustomColorListBox.SetSelected(const AColor: TColor);
begin
  Selected := AColor;
end;

procedure TSepiImportsTCustomColorListBox.SetDefaultColorColor(const Value: TColor);
begin
  DefaultColorColor := Value;
end;

procedure TSepiImportsTCustomColorListBox.SetNoneColorColor(const Value: TColor);
begin
  NoneColorColor := Value;
end;

class function TSepiImportsTCustomColorListBox.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
const
  DefaultStyle : TColorBoxStyle =
    [cbStandardColors, cbExtendedColors, cbSystemColors];
begin
  Result := TSepiClass(Owner.FindMeta('TCustomColorListBox'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomColorListBox));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStyle', System.TypeInfo(TColorBoxStyle));
    AddField('FNeedToPopulate', System.TypeInfo(Boolean));
    AddField('FListSelected', System.TypeInfo(Boolean));
    AddField('FDefaultColorColor', System.TypeInfo(TColor));
    AddField('FNoneColorColor', System.TypeInfo(TColor));
    AddField('FSelectedColor', System.TypeInfo(TColor));
    AddField('FOnGetColors', System.TypeInfo(TLBGetColorsEvent));

    AddMethod('GetColor', @TSepiImportsTCustomColorListBox.GetColor,
      'function(Index: Integer): TColor');
    AddMethod('GetColorName', @TSepiImportsTCustomColorListBox.GetColorName,
      'function(Index: Integer): string');
    AddMethod('GetSelected', @TSepiImportsTCustomColorListBox.GetSelected,
      'function: TColor');
    AddMethod('SetSelected', @TSepiImportsTCustomColorListBox.SetSelected,
      'procedure(const AColor: TColor)');
    AddMethod('ColorCallBack', nil,
      'procedure(const AName: string)');
    AddMethod('SetDefaultColorColor', @TSepiImportsTCustomColorListBox.SetDefaultColorColor,
      'procedure(const Value: TColor)');
    AddMethod('SetNoneColorColor', @TSepiImportsTCustomColorListBox.SetNoneColorColor,
      'procedure(const Value: TColor)');

    CurrentVisibility := mvProtected;

    AddMethod('CreateWnd', @TSepiImportsTCustomColorListBox.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DrawItem', @TSepiImportsTCustomColorListBox.DrawItem,
      'procedure(Index: Integer; Rect: TRect; State: TOwnerDrawState)',
      mlkOverride);
    AddMethod('KeyDown', @TSepiImportsTCustomColorListBox.KeyDown,
      'procedure(var Key: Word; Shift: TShiftState)',
      mlkOverride);
    AddMethod('KeyPress', @TSepiImportsTCustomColorListBox.KeyPress,
      'procedure(var Key: Char)',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTCustomColorListBox.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('PickCustomColor', @TSepiImportsTCustomColorListBox.PickCustomColor,
      'function: Boolean',
      mlkVirtual);
    AddMethod('PopulateList', @TSepiImportsTCustomColorListBox.PopulateList,
      'procedure');
    AddMethod('SetStyle', @TSepiImportsTCustomColorListBox.SetStyle,
      'procedure(AStyle: TColorBoxStyle)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomColorListBox.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    AddProperty('Style', 'property: TColorBoxStyle',
      'FStyle', 'SetStyle',
      NoIndex, Byte(DefaultStyle));
    AddProperty('Colors', 'property[Index: Integer]: TColor',
      'GetColor', '');
    AddProperty('ColorNames', 'property[Index: Integer]: string',
      'GetColorName', '');
    AddProperty('Selected', 'property: TColor',
      'GetSelected', 'SetSelected',
      NoIndex, Integer(clBlack));
    AddProperty('DefaultColorColor', 'property: TColor',
      'FDefaultColorColor', 'SetDefaultColorColor',
      NoIndex, Integer(clBlack));
    AddProperty('NoneColorColor', 'property: TColor',
      'FNoneColorColor', 'SetNoneColorColor',
      NoIndex, Integer(clBlack));
    AddProperty('OnGetColors', 'property: TLBGetColorsEvent',
      'FOnGetColors', 'FOnGetColors');

    Complete;
  end;
end;

{----------------------}
{ TColorListBox import }
{----------------------}

class function TSepiImportsTColorListBox.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TColorListBox));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('AutoComplete');
    RedefineProperty('DefaultColorColor');
    RedefineProperty('NoneColorColor');
    RedefineProperty('Selected');
    RedefineProperty('Style');
    RedefineProperty('Anchors');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelKind');
    RedefineProperty('BevelOuter');
    RedefineProperty('BiDiMode');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ItemHeight');
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
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnGetColors');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ExtCtrls',
    ['Messages', 'Windows', 'SysUtils', 'Classes', 'Controls', 'Forms', 'Menus', 'Graphics', 'StdCtrls']);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TShapeType));
  TSepiImportsTShape.SepiImport(Result);
  TSepiImportsTPaintBox.SepiImport(Result);
  TSepiImportsTImage.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBevelStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBevelShape));
  TSepiImportsTBevel.SepiImport(Result);
  TSepiImportsTTimer.SepiImport(Result);
  TSepiTypeAlias.Create(Result, 'TPanelBevel', TypeInfo(TBevelCut));
  TSepiImportsTCustomPanel.SepiImport(Result);
  TSepiImportsTPanel.SepiImport(Result);
  TSepiImportsTPage.SepiImport(Result);
  TSepiImportsTNotebook.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSectionEvent));
  TSepiImportsTHeader.SepiImport(Result);
  TSepiImportsTCustomRadioGroup.SepiImport(Result);
  TSepiImportsTRadioGroup.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(NaturalNumber));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSplitterCanResizeEvent));
  TSepiTypeAlias.Create(Result, 'TCanResizeEvent', TypeInfo(TSplitterCanResizeEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TResizeStyle));
  TSepiImportsTSplitter.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBandPaintOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBandPaintOptions));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBandDragEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBandInfoEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBandMoveEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBeginBandMoveEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEndBandMoveEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBandPaintEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TRowSize));
  TSepiImportsTCustomControlBar.SepiImport(Result);
  TSepiImportsTControlBar.SepiImport(Result);
  TSepiImportsTBoundLabel.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLabelPosition));
  TSepiImportsTCustomLabeledEdit.SepiImport(Result);
  TSepiImportsTLabeledEdit.SepiImport(Result);

  // Constants
  TSepiConstant.Create(Result, 'NoColorSelected', NoColorSelected);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomColorBox));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TColorBoxStyles));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TColorBoxStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetColorsEvent));
  TSepiImportsTCustomColorBox.SepiImport(Result);
  TSepiImportsTColorBox.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomColorListBox));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLBGetColorsEvent));
  TSepiImportsTCustomColorListBox.SepiImport(Result);
  TSepiImportsTColorListBox.SepiImport(Result);

  // Routines
  TSepiMethod.Create(Result, 'Frame3D', @Frame3D,
    'procedure(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor ; Width: Integer )');
  TSepiMethod.Create(Result, 'NotebookHandlesNeeded', @NotebookHandlesNeeded,
    'procedure(Notebook: TNotebook)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ExtCtrls', ImportUnit);
end.

