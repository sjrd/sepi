{*
  Importe l'unité Controls dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsControls;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Windows, Messages, Graphics, Classes, Menus, Controls;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTDragObject = class(TDragObject)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDragObjectEx = class(TDragObjectEx)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTBaseDragControlObject = class(TBaseDragControlObject)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDragControlObject = class(TDragControlObject)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDragControlObjectEx = class(TDragControlObjectEx)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDragDockObject = class(TDragDockObject)
  private
    procedure SetBrush(Value: TBrush);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDragDockObjectEx = class(TDragDockObjectEx)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTControlCanvas = class(TControlCanvas)
  private
    procedure SetControl(AControl: TControl);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomControlAction = class(TCustomControlAction)
  private
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetEnableDropdown(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTControlAction = class(TControlAction)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTControlActionLink = class(TControlActionLink)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTSizeConstraints = class(TSizeConstraints)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTControl = class(TControl)
  private
    function GetBoundsRect: TRect;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetLRDockWidth: Integer;
    function GetMouseCapture: Boolean;
    function GetText: TCaption;
    function GetTBDockHeight: Integer;
    function GetUndockWidth: Integer;
    function GetUndockHeight: Integer;
    procedure SetAnchors(Value: TAnchors);
    procedure SetAction(Value: TBasicAction);
    procedure SetAlign(Value: TAlign);
    procedure SetBoundsRect(const Rect: TRect);
    procedure SetClientHeight(Value: Integer);
    procedure SetClientWidth(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetCursor(Value: TCursor);
    procedure SetDesktopFont(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetHeight(Value: Integer);
    procedure SetHelpContext(const Value: THelpContext);
    procedure SetHelpKeyword(const Value: String);
    procedure SetHostDockSite(Value: TWinControl);
    procedure SetLeft(Value: Integer);
    procedure SetMouseCapture(Value: Boolean);
    procedure SetParentColor(Value: Boolean);
    procedure SetParentFont(Value: Boolean);
    procedure SetShowHint(Value: Boolean);
    procedure SetParentShowHint(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetText(const Value: TCaption);
    procedure SetTop(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
    procedure SetConstraints(const Value: TSizeConstraints);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTWinControlActionLink = class(TWinControlActionLink)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTWinControl = class(TWinControl)
  private
    function GetAlignDisabled: Boolean;
    function GetControl(Index: Integer): TControl;
    function GetControlCount: Integer;
    function GetDockClientCount: Integer;
    function GetDockClients(Index: Integer): TControl;
    function GetHandle: HWnd;
    function GetParentBackground: Boolean;
    function GetTabOrder: TTabOrder;
    function GetVisibleDockClientCount: Integer;
    procedure SetBevelEdges(const Value: TBevelEdges);
    procedure SetBevelKind(const Value: TBevelKind);
    procedure SetBevelWidth(const Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetCtl3D(Value: Boolean);
    procedure SetDockSite(Value: Boolean);
    procedure SetParentCtl3D(Value: Boolean);
    procedure SetParentWindow(Value: HWnd);
    procedure SetTabOrder(Value: TTabOrder);
    procedure SetTabStop(Value: Boolean);
    procedure SetUseDockManager(Value: Boolean);
    procedure PaintTo_0(DC: HDC; X, Y: Integer);
    procedure PaintTo_1(Canvas: TCanvas; X, Y: Integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTGraphicControl = class(TGraphicControl)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomControl = class(TCustomControl)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTHintWindow = class(THintWindow)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDragImageList = class(TDragImageList)
  private
    procedure SetDragCursor(Value: TCursor);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTImageList = class(TImageList)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDockZone = class(TDockZone)
  private
    function GetChildCount: Integer;
    function GetLimitBegin: Integer;
    function GetLimitSize: Integer;
    function GetVisible: Boolean;
    function GetVisibleChildCount: Integer;
    function GetZoneLimit: Integer;
    procedure SetZoneLimit(const Value: Integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDockTree = class(TDockTree)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMouse = class(TMouse)
  private
    function GetCursorPos: TPoint;
    procedure SetCursorPos(const Value: TPoint);
    function GetCapture: HWND;
    procedure SetCapture(const Value: HWND);
    function GetIsDragging: Boolean;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomListControl = class(TCustomListControl)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomMultiSelectListControl = class(TCustomMultiSelectListControl)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{----------------------}
{ TCMMouseWheel import }
{----------------------}

function SepiImportTCMMouseWheel(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMMouseWheel', False, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('ShiftState', System.TypeInfo(TShiftState));
    AddField('Unused', System.TypeInfo(Byte));
    AddField('WheelDelta', System.TypeInfo(SmallInt));
    AddFieldAfter('XPos', System.TypeInfo(Smallint), 'WheelDelta');
    AddFieldAfter('YPos', System.TypeInfo(Smallint), 'XPos');
    AddFieldAfter('Pos', 'TSmallPoint', 'WheelDelta');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'Pos');

    Complete;
  end;
end;

{----------------------}
{ TCMCancelMode import }
{----------------------}

function SepiImportTCMCancelMode(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMCancelMode', False, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('Sender', System.TypeInfo(TControl));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------}
{ TCMFocusChanged import }
{------------------------}

function SepiImportTCMFocusChanged(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMFocusChanged', False, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('Sender', System.TypeInfo(TWinControl));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------------}
{ TCMControlListChange import }
{-----------------------------}

function SepiImportTCMControlListChange(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMControlListChange', False, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Control', System.TypeInfo(TControl));
    AddField('Inserting', System.TypeInfo(LongBool));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TCMChildKey import }
{--------------------}

function SepiImportTCMChildKey(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMChildKey', False, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('CharCode', System.TypeInfo(Word));
    AddField('Unused', System.TypeInfo(Word));
    AddField('Sender', System.TypeInfo(TWinControl));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TCMControlChange import }
{-------------------------}

function SepiImportTCMControlChange(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMControlChange', False, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Control', System.TypeInfo(TControl));
    AddField('Inserting', System.TypeInfo(LongBool));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TCMChanged import }
{-------------------}

function SepiImportTCMChanged(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMChanged', False, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Child', System.TypeInfo(TControl));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------}
{ TDragRec import }
{-----------------}

function SepiImportTDragRec(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TDragRec', False, True);

  with Result do
  begin
    AddField('Pos', 'TPoint');
    AddField('Source', System.TypeInfo(TDragObject));
    AddField('Target', 'Pointer');
    AddField('Docking', System.TypeInfo(Boolean));

    Complete;
  end;
end;

{----------------}
{ TCMDrag import }
{----------------}

function SepiImportTCMDrag(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMDrag', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('DragMessage', System.TypeInfo(TDragMessage));
    AddField('Reserved1', System.TypeInfo(Byte));
    AddField('Reserved2', System.TypeInfo(Word));
    AddField('DragRec', 'PDragRec');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TCMDockClient import }
{----------------------}

function SepiImportTCMDockClient(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMDockClient', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('DockSource', System.TypeInfo(TDragDockObject));
    AddField('MousePos', 'TSmallPoint');
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{------------------------}
{ TCMUnDockClient import }
{------------------------}

function SepiImportTCMUnDockClient(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMUnDockClient', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('NewTarget', System.TypeInfo(TControl));
    AddField('Client', System.TypeInfo(TControl));
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-----------------}
{ TCMFloat import }
{-----------------}

function SepiImportTCMFloat(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMFloat', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Reserved', System.TypeInfo(Integer));
    AddField('DockSource', System.TypeInfo(TDragDockObject));
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-----------------------}
{ TDockNotifyRec import }
{-----------------------}

function SepiImportTDockNotifyRec(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TDockNotifyRec', False, True);

  with Result do
  begin
    AddField('ClientMsg', System.TypeInfo(Cardinal));
    AddField('MsgWParam', System.TypeInfo(Integer));
    AddField('MsgLParam', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------------}
{ TCMDockNotification import }
{----------------------------}

function SepiImportTCMDockNotification(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMDockNotification', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Client', System.TypeInfo(TControl));
    AddField('NotifyRec', 'PDockNotifyRec');
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-----------------------}
{ TPopupFormInfo import }
{-----------------------}

function SepiImportTPopupFormInfo(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TPopupFormInfo', False, True);

  with Result do
  begin
    AddField('PopupID', System.TypeInfo(Integer));
    AddField('PopupWnd', System.TypeInfo(HWND));
    AddField('IsPopup', System.TypeInfo(Boolean));

    Complete;
  end;
end;

{----------------------------}
{ TCMPopupHWndDestroy import }
{----------------------------}

function SepiImportTCMPopupHWndDestroy(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMPopupHWndDestroy', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PopupFormInfo', 'PPopupFormInfo');
    AddField('PopupControlWnd', System.TypeInfo(HWND));
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-----------------------}
{ TCMCreatePopup import }
{-----------------------}

function SepiImportTCMCreatePopup(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMCreatePopup', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PopupID', System.TypeInfo(Integer));
    AddField('OwnerWnd', System.TypeInfo(HWND));
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{--------------------}
{ TDragObject import }
{--------------------}

class function TSepiImportsTDragObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TDragObject'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TDragObject));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAlwaysShowDragImages', System.TypeInfo(Boolean));
    AddField('FCancelling', System.TypeInfo(Boolean));
    AddField('FDragTarget', 'Pointer');
    AddField('FDragHandle', System.TypeInfo(HWND));
    AddField('FDragPos', 'TPoint');
    AddField('FDragTargetPos', 'TPoint');
    AddField('FDropped', System.TypeInfo(Boolean));
    AddField('FMouseDeltaX', System.TypeInfo(Double));
    AddField('FMouseDeltaY', System.TypeInfo(Double));
    AddField('FRightClickCancels', System.TypeInfo(Boolean));

    AddMethod('Capture', nil,
      'function: HWND');
    AddMethod('ReleaseCapture', nil,
      'procedure(Handle: HWND)');

    CurrentVisibility := mvProtected;

    AddMethod('Finished', @TSepiImportsTDragObject.Finished,
      'procedure(Target: TObject; X, Y: Integer; Accepted: Boolean)',
      mlkVirtual);
    AddMethod('GetDragCursor', @TSepiImportsTDragObject.GetDragCursor,
      'function(Accepted: Boolean; X, Y: Integer): TCursor',
      mlkVirtual);
    AddMethod('GetDragImages', @TSepiImportsTDragObject.GetDragImages,
      'function: TDragImageList',
      mlkVirtual);
    AddMethod('WndProc', @TSepiImportsTDragObject.WndProc,
      'procedure(var Msg: TMessage)',
      mlkVirtual);
    AddMethod('MainWndProc', @TSepiImportsTDragObject.MainWndProc,
      'procedure(var Message: TMessage)');

    CurrentVisibility := mvPublic;

    AddMethod('AfterConstruction', @TSepiImportsTDragObject.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTDragObject.Assign,
      'procedure(Source: TDragObject)',
      mlkVirtual);
    AddMethod('BeforeDestruction', @TSepiImportsTDragObject.BeforeDestruction,
      'procedure',
      mlkOverride);
    AddMethod('GetName', @TSepiImportsTDragObject.GetName,
      'function: string',
      mlkVirtual);
    AddMethod('HideDragImage', @TSepiImportsTDragObject.HideDragImage,
      'procedure',
      mlkVirtual);
    AddMethod('Instance', @TSepiImportsTDragObject.Instance,
      'function: THandle',
      mlkVirtual);
    AddMethod('ShowDragImage', @TSepiImportsTDragObject.ShowDragImage,
      'procedure',
      mlkVirtual);

    AddProperty('AlwaysShowDragImages', 'property: Boolean',
      'FAlwaysShowDragImages', 'FAlwaysShowDragImages');
    AddProperty('Cancelling', 'property: Boolean',
      'FCancelling', 'FCancelling');
    AddProperty('DragHandle', 'property: HWND',
      'FDragHandle', 'FDragHandle');
    AddProperty('DragPos', 'property: TPoint',
      'FDragPos', 'FDragPos');
    AddProperty('DragTargetPos', 'property: TPoint',
      'FDragTargetPos', 'FDragTargetPos');
    AddProperty('DragTarget', 'property: Pointer',
      'FDragTarget', 'FDragTarget');
    AddProperty('Dropped', 'property: Boolean',
      'FDropped', '');
    AddProperty('MouseDeltaX', 'property: Double',
      'FMouseDeltaX', '');
    AddProperty('MouseDeltaY', 'property: Double',
      'FMouseDeltaY', '');
    AddProperty('RightClickCancels', 'property: Boolean',
      'FRightClickCancels', 'FRightClickCancels');

    Complete;
  end;
end;

{----------------------}
{ TDragObjectEx import }
{----------------------}

class function TSepiImportsTDragObjectEx.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDragObjectEx));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('BeforeDestruction', @TSepiImportsTDragObjectEx.BeforeDestruction,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{-------------------------------}
{ TBaseDragControlObject import }
{-------------------------------}

class function TSepiImportsTBaseDragControlObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBaseDragControlObject));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FControl', System.TypeInfo(TControl));

    CurrentVisibility := mvProtected;

    AddMethod('EndDrag', @TSepiImportsTBaseDragControlObject.EndDrag,
      'procedure(Target: TObject; X, Y: Integer)',
      mlkVirtual);
    AddMethod('Finished', @TSepiImportsTBaseDragControlObject.Finished,
      'procedure(Target: TObject; X, Y: Integer; Accepted: Boolean)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBaseDragControlObject.Create,
      'constructor(AControl: TControl)',
      mlkVirtual);
    AddMethod('Assign', @TSepiImportsTBaseDragControlObject.Assign,
      'procedure(Source: TDragObject)',
      mlkOverride);

    AddProperty('Control', 'property: TControl',
      'FControl', 'FControl');

    Complete;
  end;
end;

{---------------------------}
{ TDragControlObject import }
{---------------------------}

class function TSepiImportsTDragControlObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDragControlObject));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('GetDragCursor', @TSepiImportsTDragControlObject.GetDragCursor,
      'function(Accepted: Boolean; X, Y: Integer): TCursor',
      mlkOverride);
    AddMethod('GetDragImages', @TSepiImportsTDragControlObject.GetDragImages,
      'function: TDragImageList',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('HideDragImage', @TSepiImportsTDragControlObject.HideDragImage,
      'procedure',
      mlkOverride);
    AddMethod('ShowDragImage', @TSepiImportsTDragControlObject.ShowDragImage,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{-----------------------------}
{ TDragControlObjectEx import }
{-----------------------------}

class function TSepiImportsTDragControlObjectEx.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDragControlObjectEx));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('BeforeDestruction', @TSepiImportsTDragControlObjectEx.BeforeDestruction,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{------------------------}
{ TDragDockObject import }
{------------------------}

procedure TSepiImportsTDragDockObject.SetBrush(Value: TBrush);
begin
  Brush := Value;
end;

class function TSepiImportsTDragDockObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TDragDockObject'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TDragDockObject));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FBrush', System.TypeInfo(TBrush));
    AddField('FDockRect', 'TRect');
    AddField('FDropAlign', System.TypeInfo(TAlign));
    AddField('FDropOnControl', System.TypeInfo(TControl));
    AddField('FEraseDockRect', 'TRect');
    AddField('FFloating', System.TypeInfo(Boolean));

    AddMethod('SetBrush', @TSepiImportsTDragDockObject.SetBrush,
      'procedure(Value: TBrush)');

    CurrentVisibility := mvProtected;

    AddMethod('AdjustDockRect', @TSepiImportsTDragDockObject.AdjustDockRect,
      'procedure(ARect: TRect)',
      mlkVirtual);
    AddMethod('DrawDragDockImage', @TSepiImportsTDragDockObject.DrawDragDockImage,
      'procedure',
      mlkVirtual);
    AddMethod('EndDrag', @TSepiImportsTDragDockObject.EndDrag,
      'procedure(Target: TObject; X, Y: Integer)',
      mlkOverride);
    AddMethod('EraseDragDockImage', @TSepiImportsTDragDockObject.EraseDragDockImage,
      'procedure',
      mlkVirtual);
    AddMethod('GetDragCursor', @TSepiImportsTDragDockObject.GetDragCursor,
      'function(Accepted: Boolean; X, Y: Integer): TCursor',
      mlkOverride);
    AddMethod('GetFrameWidth', @TSepiImportsTDragDockObject.GetFrameWidth,
      'function: Integer',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTDragDockObject.Create,
      'constructor(AControl: TControl)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTDragDockObject.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTDragDockObject.Assign,
      'procedure(Source: TDragObject)',
      mlkOverride);

    AddProperty('Brush', 'property: TBrush',
      'FBrush', 'SetBrush');
    AddProperty('DockRect', 'property: TRect',
      'FDockRect', 'FDockRect');
    AddProperty('DropAlign', 'property: TAlign',
      'FDropAlign', '');
    AddProperty('DropOnControl', 'property: TControl',
      'FDropOnControl', '');
    AddProperty('Floating', 'property: Boolean',
      'FFloating', 'FFloating');
    AddProperty('FrameWidth', 'property: Integer',
      'GetFrameWidth', '');

    Complete;
  end;
end;

{--------------------------}
{ TDragDockObjectEx import }
{--------------------------}

class function TSepiImportsTDragDockObjectEx.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDragDockObjectEx));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('BeforeDestruction', @TSepiImportsTDragDockObjectEx.BeforeDestruction,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{-----------------------}
{ TControlCanvas import }
{-----------------------}

procedure TSepiImportsTControlCanvas.SetControl(AControl: TControl);
begin
  Control := AControl;
end;

class function TSepiImportsTControlCanvas.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TControlCanvas));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FControl', System.TypeInfo(TControl));
    AddField('FDeviceContext', System.TypeInfo(HDC));
    AddField('FWindowHandle', System.TypeInfo(HWnd));

    AddMethod('SetControl', @TSepiImportsTControlCanvas.SetControl,
      'procedure(AControl: TControl)');

    CurrentVisibility := mvProtected;

    AddMethod('CreateHandle', @TSepiImportsTControlCanvas.CreateHandle,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTControlCanvas.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('FreeHandle', @TSepiImportsTControlCanvas.FreeHandle,
      'procedure');
    AddMethod('UpdateTextFlags', @TSepiImportsTControlCanvas.UpdateTextFlags,
      'procedure');

    AddProperty('Control', 'property: TControl',
      'FControl', 'SetControl');

    Complete;
  end;
end;

{-----------------------------}
{ TCustomControlAction import }
{-----------------------------}

procedure TSepiImportsTCustomControlAction.SetDropdownMenu(Value: TPopupMenu);
begin
  DropdownMenu := Value;
end;

procedure TSepiImportsTCustomControlAction.SetEnableDropdown(Value: Boolean);
begin
  EnableDropdown := Value;
end;

procedure TSepiImportsTCustomControlAction.SetPopupMenu(Value: TPopupMenu);
begin
  PopupMenu := Value;
end;

class function TSepiImportsTCustomControlAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomControlAction));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDropdownMenu', System.TypeInfo(TPopupMenu));
    AddField('FPopupMenu', System.TypeInfo(TPopupMenu));
    AddField('FEnableDropdown', System.TypeInfo(Boolean));

    AddMethod('SetDropdownMenu', @TSepiImportsTCustomControlAction.SetDropdownMenu,
      'procedure(Value: TPopupMenu)');
    AddMethod('SetEnableDropdown', @TSepiImportsTCustomControlAction.SetEnableDropdown,
      'procedure(Value: Boolean)');
    AddMethod('SetPopupMenu', @TSepiImportsTCustomControlAction.SetPopupMenu,
      'procedure(Value: TPopupMenu)');

    CurrentVisibility := mvPublic;

    AddProperty('DropdownMenu', 'property: TPopupMenu',
      'FDropdownMenu', 'SetDropdownMenu');
    AddProperty('EnableDropdown', 'property: Boolean',
      'FEnableDropdown', 'SetEnableDropdown',
      NoIndex, Integer(False));
    AddProperty('PopupMenu', 'property: TPopupMenu',
      'FPopupMenu', 'SetPopupMenu');

    Complete;
  end;
end;

{-----------------------}
{ TControlAction import }
{-----------------------}

class function TSepiImportsTControlAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TControlAction));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('AutoCheck');
    RedefineProperty('Caption');
    RedefineProperty('Checked');
    RedefineProperty('DropdownMenu');
    RedefineProperty('Enabled');
    RedefineProperty('EnableDropdown');
    RedefineProperty('GroupIndex');
    RedefineProperty('HelpContext');
    RedefineProperty('HelpKeyword');
    RedefineProperty('HelpType');
    RedefineProperty('Hint');
    RedefineProperty('ImageIndex');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShortCut');
    RedefineProperty('SecondaryShortCuts');
    RedefineProperty('Visible');
    RedefineProperty('OnExecute');
    RedefineProperty('OnHint');
    RedefineProperty('OnUpdate');

    Complete;
  end;
end;

{---------------------------}
{ TControlActionLink import }
{---------------------------}

class function TSepiImportsTControlActionLink.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TControlActionLink));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FClient', System.TypeInfo(TControl));

    AddMethod('AssignClient', @TSepiImportsTControlActionLink.AssignClient,
      'procedure(AClient: TObject)',
      mlkOverride);
    AddMethod('IsCaptionLinked', @TSepiImportsTControlActionLink.IsCaptionLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsDropdownMenuLinked', @TSepiImportsTControlActionLink.IsDropdownMenuLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsEnabledLinked', @TSepiImportsTControlActionLink.IsEnabledLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsEnableDropdownLinked', @TSepiImportsTControlActionLink.IsEnableDropdownLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsHelpLinked', @TSepiImportsTControlActionLink.IsHelpLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsHintLinked', @TSepiImportsTControlActionLink.IsHintLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsVisibleLinked', @TSepiImportsTControlActionLink.IsVisibleLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsOnExecuteLinked', @TSepiImportsTControlActionLink.IsOnExecuteLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsPopupMenuLinked', @TSepiImportsTControlActionLink.IsPopupMenuLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('DoShowHint', @TSepiImportsTControlActionLink.DoShowHint,
      'function(var HintStr: string): Boolean',
      mlkVirtual);
    AddMethod('SetCaption', @TSepiImportsTControlActionLink.SetCaption,
      'procedure(const Value: string)',
      mlkOverride);
    AddMethod('SetDropdownMenu', @TSepiImportsTControlActionLink.SetDropdownMenu,
      'procedure(Value: TPopupMenu)',
      mlkVirtual);
    AddMethod('SetEnabled', @TSepiImportsTControlActionLink.SetEnabled,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetEnableDropdown', @TSepiImportsTControlActionLink.SetEnableDropdown,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('SetHint', @TSepiImportsTControlActionLink.SetHint,
      'procedure(const Value: string)',
      mlkOverride);
    AddMethod('SetHelpContext', @TSepiImportsTControlActionLink.SetHelpContext,
      'procedure(Value: THelpContext)',
      mlkOverride);
    AddMethod('SetHelpKeyword', @TSepiImportsTControlActionLink.SetHelpKeyword,
      'procedure(const Value: string)',
      mlkOverride);
    AddMethod('SetHelpType', @TSepiImportsTControlActionLink.SetHelpType,
      'procedure(Value: THelpType)',
      mlkOverride);
    AddMethod('SetVisible', @TSepiImportsTControlActionLink.SetVisible,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetOnExecute', @TSepiImportsTControlActionLink.SetOnExecute,
      'procedure(Value: TNotifyEvent)',
      mlkOverride);
    AddMethod('SetPopupMenu', @TSepiImportsTControlActionLink.SetPopupMenu,
      'procedure(Value: TPopupMenu)',
      mlkVirtual);

    Complete;
  end;
end;

{--------------------------}
{ TMouseActivateRec import }
{--------------------------}

function SepiImportTMouseActivateRec(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TMouseActivateRec', False, True);

  with Result do
  begin
    AddField('MousePos', 'TPoint');
    AddField('HitTest', System.TypeInfo(Integer));
    AddField('Button', System.TypeInfo(TMouseButton));
    AddField('ShiftState', System.TypeInfo(TShiftState));
    AddField('TopLevel', System.TypeInfo(HWND));

    Complete;
  end;
end;

{-------------------------}
{ TCMMouseActivate import }
{-------------------------}

function SepiImportTCMMouseActivate(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCMMouseActivate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Reserved', System.TypeInfo(Integer));
    AddField('MouseActivateRec', 'PMouseActivateRec');
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------------}
{ TSizeConstraints import }
{-------------------------}

class function TSepiImportsTSizeConstraints.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSizeConstraints));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FControl', System.TypeInfo(TControl));
    AddField('FMaxHeight', System.TypeInfo(TConstraintSize));
    AddField('FMaxWidth', System.TypeInfo(TConstraintSize));
    AddField('FMinHeight', System.TypeInfo(TConstraintSize));
    AddField('FMinWidth', System.TypeInfo(TConstraintSize));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));

    AddMethod('SetConstraints', nil,
      'procedure(Index: Integer; Value: TConstraintSize)');

    CurrentVisibility := mvProtected;

    AddMethod('Change', @TSepiImportsTSizeConstraints.Change,
      'procedure',
      mlkVirtual);
    AddMethod('AssignTo', @TSepiImportsTSizeConstraints.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);

    AddProperty('Control', 'property: TControl',
      'FControl', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTSizeConstraints.Create,
      'constructor(Control: TControl)',
      mlkVirtual);

    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');

    CurrentVisibility := mvPublished;

    AddProperty('MaxHeight', 'property: TConstraintSize',
      'FMaxHeight', 'SetConstraints',
      0, 0);
    AddProperty('MaxWidth', 'property: TConstraintSize',
      'FMaxWidth', 'SetConstraints',
      1, 0);
    AddProperty('MinHeight', 'property: TConstraintSize',
      'FMinHeight', 'SetConstraints',
      2, 0);
    AddProperty('MinWidth', 'property: TConstraintSize',
      'FMinWidth', 'SetConstraints',
      3, 0);

    Complete;
  end;
end;

{-----------------}
{ TControl import }
{-----------------}

function TSepiImportsTControl.GetBoundsRect: TRect;
begin
  Result := BoundsRect;
end;

function TSepiImportsTControl.GetClientHeight: Integer;
begin
  Result := ClientHeight;
end;

function TSepiImportsTControl.GetClientWidth: Integer;
begin
  Result := ClientWidth;
end;

function TSepiImportsTControl.GetLRDockWidth: Integer;
begin
  Result := LRDockWidth;
end;

function TSepiImportsTControl.GetMouseCapture: Boolean;
begin
  Result := MouseCapture;
end;

function TSepiImportsTControl.GetText: TCaption;
begin
  Result := Caption;
end;

function TSepiImportsTControl.GetTBDockHeight: Integer;
begin
  Result := TBDockHeight;
end;

function TSepiImportsTControl.GetUndockWidth: Integer;
begin
  Result := UndockWidth;
end;

function TSepiImportsTControl.GetUndockHeight: Integer;
begin
  Result := UndockHeight;
end;

procedure TSepiImportsTControl.SetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TSepiImportsTControl.SetAction(Value: TBasicAction);
begin
  Action := Value;
end;

procedure TSepiImportsTControl.SetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TSepiImportsTControl.SetBoundsRect(const Rect: TRect);
begin
  BoundsRect := Rect;
end;

procedure TSepiImportsTControl.SetClientHeight(Value: Integer);
begin
  ClientHeight := Value;
end;

procedure TSepiImportsTControl.SetClientWidth(Value: Integer);
begin
  ClientWidth := Value;
end;

procedure TSepiImportsTControl.SetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TSepiImportsTControl.SetCursor(Value: TCursor);
begin
  Cursor := Value;
end;

procedure TSepiImportsTControl.SetDesktopFont(Value: Boolean);
begin
  DesktopFont := Value;
end;

procedure TSepiImportsTControl.SetFont(Value: TFont);
begin
  Font := Value;
end;

procedure TSepiImportsTControl.SetHeight(Value: Integer);
begin
  Height := Value;
end;

procedure TSepiImportsTControl.SetHelpContext(const Value: THelpContext);
begin
  HelpContext := Value;
end;

procedure TSepiImportsTControl.SetHelpKeyword(const Value: String);
begin
  HelpKeyword := Value;
end;

procedure TSepiImportsTControl.SetHostDockSite(Value: TWinControl);
begin
  HostDockSite := Value;
end;

procedure TSepiImportsTControl.SetLeft(Value: Integer);
begin
  Left := Value;
end;

procedure TSepiImportsTControl.SetMouseCapture(Value: Boolean);
begin
  MouseCapture := Value;
end;

procedure TSepiImportsTControl.SetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TSepiImportsTControl.SetParentFont(Value: Boolean);
begin
  ParentFont := Value;
end;

procedure TSepiImportsTControl.SetShowHint(Value: Boolean);
begin
  ShowHint := Value;
end;

procedure TSepiImportsTControl.SetParentShowHint(Value: Boolean);
begin
  ParentShowHint := Value;
end;

procedure TSepiImportsTControl.SetPopupMenu(Value: TPopupMenu);
begin
  PopupMenu := Value;
end;

procedure TSepiImportsTControl.SetText(const Value: TCaption);
begin
  Caption := Value;
end;

procedure TSepiImportsTControl.SetTop(Value: Integer);
begin
  Top := Value;
end;

procedure TSepiImportsTControl.SetVisible(Value: Boolean);
begin
  Visible := Value;
end;

procedure TSepiImportsTControl.SetWidth(Value: Integer);
begin
  Width := Value;
end;

procedure TSepiImportsTControl.SetConstraints(const Value: TSizeConstraints);
begin
  Constraints := Value;
end;

class function TSepiImportsTControl.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
const
  DefaultAnchors : TAnchors = [akLeft, akTop];
begin
  Result := TSepiClass(Owner.FindMeta('TControl'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FParent', System.TypeInfo(TWinControl));
    AddField('FWindowProc', System.TypeInfo(TWndMethod));
    AddField('FLeft', System.TypeInfo(Integer));
    AddField('FTop', System.TypeInfo(Integer));
    AddField('FWidth', System.TypeInfo(Integer));
    AddField('FHeight', System.TypeInfo(Integer));
    AddField('FControlStyle', System.TypeInfo(TControlStyle));
    AddField('FControlState', System.TypeInfo(TControlState));
    AddField('FDesktopFont', System.TypeInfo(Boolean));
    AddField('FVisible', System.TypeInfo(Boolean));
    AddField('FEnabled', System.TypeInfo(Boolean));
    AddField('FParentFont', System.TypeInfo(Boolean));
    AddField('FParentColor', System.TypeInfo(Boolean));
    AddField('FAlign', System.TypeInfo(TAlign));
    AddField('FAutoSize', System.TypeInfo(Boolean));
    AddField('FDragMode', System.TypeInfo(TDragMode));
    AddField('FIsControl', System.TypeInfo(Boolean));
    AddField('FBiDiMode', System.TypeInfo(TBiDiMode));
    AddField('FParentBiDiMode', System.TypeInfo(Boolean));
    AddField('FAnchors', System.TypeInfo(TAnchors));
    AddField('FAnchorMove', System.TypeInfo(Boolean));
    AddField('FText', 'PChar');
    AddField('FFont', System.TypeInfo(TFont));
    AddField('FActionLink', System.TypeInfo(TControlActionLink));
    AddField('FColor', System.TypeInfo(TColor));
    AddField('FConstraints', System.TypeInfo(TSizeConstraints));
    AddField('FCursor', System.TypeInfo(TCursor));
    AddField('FDragCursor', System.TypeInfo(TCursor));
    AddField('FPopupMenu', System.TypeInfo(TPopupMenu));
    AddField('FHint', System.TypeInfo(string));
    AddField('FFontHeight', System.TypeInfo(Integer));
    AddField('FAnchorRules', 'TPoint');
    AddField('FOriginalParentSize', 'TPoint');
    AddField('FScalingFlags', System.TypeInfo(TScalingFlags));
    AddField('FShowHint', System.TypeInfo(Boolean));
    AddField('FParentShowHint', System.TypeInfo(Boolean));
    AddField('FDragKind', System.TypeInfo(TDragKind));
    AddField('FDockOrientation', System.TypeInfo(TDockOrientation));
    AddField('FHostDockSite', System.TypeInfo(TWinControl));
    AddField('FWheelAccumulator', System.TypeInfo(Integer));
    AddField('FUndockWidth', System.TypeInfo(Integer));
    AddField('FUndockHeight', System.TypeInfo(Integer));
    AddField('FLRDockWidth', System.TypeInfo(Integer));
    AddField('FTBDockHeight', System.TypeInfo(Integer));
    AddField('FFloatingDockSiteClass', 'TWinControlClass');
    AddField('FOnCanResize', System.TypeInfo(TCanResizeEvent));
    AddField('FOnConstrainedResize', System.TypeInfo(TConstrainedResizeEvent));
    AddField('FOnMouseDown', System.TypeInfo(TMouseEvent));
    AddField('FOnMouseMove', System.TypeInfo(TMouseMoveEvent));
    AddField('FOnMouseUp', System.TypeInfo(TMouseEvent));
    AddField('FOnDragDrop', System.TypeInfo(TDragDropEvent));
    AddField('FOnDragOver', System.TypeInfo(TDragOverEvent));
    AddField('FOnResize', System.TypeInfo(TNotifyEvent));
    AddField('FOnStartDock', System.TypeInfo(TStartDockEvent));
    AddField('FOnEndDock', System.TypeInfo(TEndDragEvent));
    AddField('FOnStartDrag', System.TypeInfo(TStartDragEvent));
    AddField('FOnEndDrag', System.TypeInfo(TEndDragEvent));
    AddField('FOnClick', System.TypeInfo(TNotifyEvent));
    AddField('FOnDblClick', System.TypeInfo(TNotifyEvent));
    AddField('FOnContextPopup', System.TypeInfo(TContextPopupEvent));
    AddField('FOnMouseActivate', System.TypeInfo(TMouseActivateEvent));
    AddField('FOnMouseWheel', System.TypeInfo(TMouseWheelEvent));
    AddField('FOnMouseWheelDown', System.TypeInfo(TMouseWheelUpDownEvent));
    AddField('FOnMouseWheelUp', System.TypeInfo(TMouseWheelUpDownEvent));
    AddField('FHelpType', System.TypeInfo(THelpType));
    AddField('FHelpKeyword', System.TypeInfo(String));
    AddField('FHelpContext', System.TypeInfo(THelpContext));

    AddMethod('CalcDockSizes', nil,
      'procedure');
    AddMethod('CheckNewSize', nil,
      'function(var NewWidth, NewHeight: Integer): Boolean');
    AddMethod('CreateFloatingDockSite', nil,
      'function(Bounds: TRect): TWinControl');
    AddMethod('DoActionChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('DoCanAutoSize', nil,
      'function(var NewWidth, NewHeight: Integer): Boolean');
    AddMethod('DoCanResize', nil,
      'function(var NewWidth, NewHeight: Integer): Boolean');
    AddMethod('DoConstraintsChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('DoConstrainedResize', nil,
      'procedure(var NewWidth, NewHeight: Integer)');
    AddMethod('DoDragMsg', nil,
      'procedure(var DragMsg: TCMDrag)');
    AddMethod('DoMouseActivate', nil,
      'procedure(var Message: TCMMouseActivate)');
    AddMethod('DoMouseDown', nil,
      'procedure(var Message: TWMMouse; Button: TMouseButton; Shift: TShiftState )');
    AddMethod('DoMouseUp', nil,
      'procedure(var Message: TWMMouse; Button: TMouseButton)');
    AddMethod('FontChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('GetBoundsRect', @TSepiImportsTControl.GetBoundsRect,
      'function: TRect');
    AddMethod('GetClientHeight', @TSepiImportsTControl.GetClientHeight,
      'function: Integer');
    AddMethod('GetClientWidth', @TSepiImportsTControl.GetClientWidth,
      'function: Integer');
    AddMethod('GetLRDockWidth', @TSepiImportsTControl.GetLRDockWidth,
      'function: Integer');
    AddMethod('GetMouseCapture', @TSepiImportsTControl.GetMouseCapture,
      'function: Boolean');
    AddMethod('GetText', @TSepiImportsTControl.GetText,
      'function: TCaption');
    AddMethod('GetTBDockHeight', @TSepiImportsTControl.GetTBDockHeight,
      'function: Integer');
    AddMethod('GetUndockWidth', @TSepiImportsTControl.GetUndockWidth,
      'function: Integer');
    AddMethod('GetUndockHeight', @TSepiImportsTControl.GetUndockHeight,
      'function: Integer');
    AddMethod('InvalidateControl', nil,
      'procedure(IsVisible, IsOpaque: Boolean)');
    AddMethod('IsAnchorsStored', nil,
      'function: Boolean');
    AddMethod('IsBiDiModeStored', nil,
      'function: Boolean');
    AddMethod('IsCaptionStored', nil,
      'function: Boolean');
    AddMethod('IsColorStored', nil,
      'function: Boolean');
    AddMethod('IsEnabledStored', nil,
      'function: Boolean');
    AddMethod('IsFontStored', nil,
      'function: Boolean');
    AddMethod('IsHintStored', nil,
      'function: Boolean');
    AddMethod('IsHelpContextStored', nil,
      'function: Boolean');
    AddMethod('IsOnClickStored', nil,
      'function: Boolean');
    AddMethod('IsShowHintStored', nil,
      'function: Boolean');
    AddMethod('IsVisibleStored', nil,
      'function: Boolean');
    AddMethod('ReadIsControl', nil,
      'procedure(Reader: TReader)');
    AddMethod('SetAnchors', @TSepiImportsTControl.SetAnchors,
      'procedure(Value: TAnchors)');
    AddMethod('SetAction', @TSepiImportsTControl.SetAction,
      'procedure(Value: TBasicAction)');
    AddMethod('SetAlign', @TSepiImportsTControl.SetAlign,
      'procedure(Value: TAlign)');
    AddMethod('SetBoundsRect', @TSepiImportsTControl.SetBoundsRect,
      'procedure(const Rect: TRect)');
    AddMethod('SetClientHeight', @TSepiImportsTControl.SetClientHeight,
      'procedure(Value: Integer)');
    AddMethod('SetClientSize', nil,
      'procedure(Value: TPoint)');
    AddMethod('SetClientWidth', @TSepiImportsTControl.SetClientWidth,
      'procedure(Value: Integer)');
    AddMethod('SetColor', @TSepiImportsTControl.SetColor,
      'procedure(Value: TColor)');
    AddMethod('SetCursor', @TSepiImportsTControl.SetCursor,
      'procedure(Value: TCursor)');
    AddMethod('SetDesktopFont', @TSepiImportsTControl.SetDesktopFont,
      'procedure(Value: Boolean)');
    AddMethod('SetFont', @TSepiImportsTControl.SetFont,
      'procedure(Value: TFont)');
    AddMethod('SetHeight', @TSepiImportsTControl.SetHeight,
      'procedure(Value: Integer)');
    AddMethod('SetHelpContext', @TSepiImportsTControl.SetHelpContext,
      'procedure(const Value: THelpContext)');
    AddMethod('SetHelpKeyword', @TSepiImportsTControl.SetHelpKeyword,
      'procedure(const Value: String)');
    AddMethod('SetHostDockSite', @TSepiImportsTControl.SetHostDockSite,
      'procedure(Value: TWinControl)');
    AddMethod('SetLeft', @TSepiImportsTControl.SetLeft,
      'procedure(Value: Integer)');
    AddMethod('SetMouseCapture', @TSepiImportsTControl.SetMouseCapture,
      'procedure(Value: Boolean)');
    AddMethod('SetParentColor', @TSepiImportsTControl.SetParentColor,
      'procedure(Value: Boolean)');
    AddMethod('SetParentFont', @TSepiImportsTControl.SetParentFont,
      'procedure(Value: Boolean)');
    AddMethod('SetShowHint', @TSepiImportsTControl.SetShowHint,
      'procedure(Value: Boolean)');
    AddMethod('SetParentShowHint', @TSepiImportsTControl.SetParentShowHint,
      'procedure(Value: Boolean)');
    AddMethod('SetPopupMenu', @TSepiImportsTControl.SetPopupMenu,
      'procedure(Value: TPopupMenu)');
    AddMethod('SetText', @TSepiImportsTControl.SetText,
      'procedure(const Value: TCaption)');
    AddMethod('SetTop', @TSepiImportsTControl.SetTop,
      'procedure(Value: Integer)');
    AddMethod('SetVisible', @TSepiImportsTControl.SetVisible,
      'procedure(Value: Boolean)');
    AddMethod('SetWidth', @TSepiImportsTControl.SetWidth,
      'procedure(Value: Integer)');
    AddMethod('SetZOrderPosition', nil,
      'procedure(Position: Integer)');
    AddMethod('UpdateAnchorRules', nil,
      'procedure');
    AddMethod('WriteIsControl', nil,
      'procedure(Writer: TWriter)');
    AddMethod('WMLButtonDown', nil,
      'procedure(var Message: TWMLButtonDown)',
      mlkMessage, False, WM_LBUTTONDOWN);
    AddMethod('WMNCLButtonDown', nil,
      'procedure(var Message: TWMNCLButtonDown)',
      mlkMessage, False, WM_NCLBUTTONDOWN);
    AddMethod('WMRButtonDown', nil,
      'procedure(var Message: TWMRButtonDown)',
      mlkMessage, False, WM_RBUTTONDOWN);
    AddMethod('WMMButtonDown', nil,
      'procedure(var Message: TWMMButtonDown)',
      mlkMessage, False, WM_MBUTTONDOWN);
    AddMethod('WMLButtonDblClk', nil,
      'procedure(var Message: TWMLButtonDblClk)',
      mlkMessage, False, WM_LBUTTONDBLCLK);
    AddMethod('WMRButtonDblClk', nil,
      'procedure(var Message: TWMRButtonDblClk)',
      mlkMessage, False, WM_RBUTTONDBLCLK);
    AddMethod('WMMButtonDblClk', nil,
      'procedure(var Message: TWMMButtonDblClk)',
      mlkMessage, False, WM_MBUTTONDBLCLK);
    AddMethod('WMMouseMove', nil,
      'procedure(var Message: TWMMouseMove)',
      mlkMessage, False, WM_MOUSEMOVE);
    AddMethod('WMLButtonUp', nil,
      'procedure(var Message: TWMLButtonUp)',
      mlkMessage, False, WM_LBUTTONUP);
    AddMethod('WMRButtonUp', nil,
      'procedure(var Message: TWMRButtonUp)',
      mlkMessage, False, WM_RBUTTONUP);
    AddMethod('WMMButtonUp', nil,
      'procedure(var Message: TWMMButtonUp)',
      mlkMessage, False, WM_MBUTTONUP);
    AddMethod('WMMouseWheel', nil,
      'procedure(var Message: TWMMouseWheel)',
      mlkMessage, False, WM_MOUSEWHEEL);
    AddMethod('WMCancelMode', nil,
      'procedure(var Message: TWMCancelMode)',
      mlkMessage, False, WM_CANCELMODE);
    AddMethod('WMWindowPosChanged', nil,
      'procedure(var Message: TWMWindowPosChanged)',
      mlkMessage, False, WM_WINDOWPOSCHANGED);
    AddMethod('CMVisibleChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_VISIBLECHANGED);
    AddMethod('CMEnabledChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ENABLEDCHANGED);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMMouseActivate', nil,
      'procedure(var Message: TCMMouseActivate)',
      mlkMessage, False, CM_MOUSEACTIVATE);
    AddMethod('CMParentFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTFONTCHANGED);
    AddMethod('CMSysFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSFONTCHANGED);
    AddMethod('CMParentColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTCOLORCHANGED);
    AddMethod('CMParentShowHintChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTSHOWHINTCHANGED);
    AddMethod('CMHintShow', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_HINTSHOW);
    AddMethod('CMHitTest', nil,
      'procedure(var Message: TCMHitTest)',
      mlkMessage, False, CM_HITTEST);
    AddMethod('CMMouseEnter', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_MOUSEENTER);
    AddMethod('CMMouseLeave', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_MOUSELEAVE);
    AddMethod('CMDesignHitTest', nil,
      'procedure(var Message: TCMDesignHitTest)',
      mlkMessage, False, CM_DESIGNHITTEST);
    AddMethod('CMFloat', nil,
      'procedure(var Message: TCMFloat)',
      mlkMessage, False, CM_FLOAT);
    AddMethod('CMBiDiModeChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BIDIMODECHANGED);
    AddMethod('CMParentBiDiModeChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTBIDIMODECHANGED);
    AddMethod('CMMouseWheel', nil,
      'procedure(var Message: TCMMouseWheel)',
      mlkMessage, False, CM_MOUSEWHEEL);
    AddMethod('WMContextMenu', nil,
      'procedure(var Message: TWMContextMenu)',
      mlkMessage, False, WM_CONTEXTMENU);
    AddMethod('SetConstraints', @TSepiImportsTControl.SetConstraints,
      'procedure(const Value: TSizeConstraints)');

    CurrentVisibility := mvProtected;

    AddMethod('ActionChange', @TSepiImportsTControl.ActionChange,
      'procedure(Sender: TObject; CheckDefaults: Boolean)',
      mlkDynamic);
    AddMethod('AdjustSize', @TSepiImportsTControl.AdjustSize,
      'procedure',
      mlkDynamic);
    AddMethod('AssignTo', @TSepiImportsTControl.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);
    AddMethod('BeginAutoDrag', @TSepiImportsTControl.BeginAutoDrag,
      'procedure',
      mlkDynamic);
    AddMethod('CanResize', @TSepiImportsTControl.CanResize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkVirtual);
    AddMethod('CanAutoSize', @TSepiImportsTControl.CanAutoSize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkVirtual);
    AddMethod('Changed', @TSepiImportsTControl.Changed,
      'procedure');
    AddMethod('ChangeScale', @TSepiImportsTControl.ChangeScale,
      'procedure(M, D: Integer)',
      mlkDynamic);
    AddMethod('Click', @TSepiImportsTControl.Click,
      'procedure',
      mlkDynamic);
    AddMethod('ConstrainedResize', @TSepiImportsTControl.ConstrainedResize,
      'procedure(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer)',
      mlkVirtual);
    AddMethod('CalcCursorPos', @TSepiImportsTControl.CalcCursorPos,
      'function: TPoint');
    AddMethod('DesignWndProc', @TSepiImportsTControl.DesignWndProc,
      'function(var Message: TMessage): Boolean',
      mlkDynamic);
    AddMethod('DblClick', @TSepiImportsTControl.DblClick,
      'procedure',
      mlkDynamic);
    AddMethod('DefaultDockImage', @TSepiImportsTControl.DefaultDockImage,
      'procedure(DragDockObject: TDragDockObject; Erase: Boolean)',
      mlkDynamic);
    AddMethod('DefineProperties', @TSepiImportsTControl.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('DockTrackNoTarget', @TSepiImportsTControl.DockTrackNoTarget,
      'procedure(Source: TDragDockObject; X, Y: Integer)',
      mlkDynamic);
    AddMethod('DoContextPopup', @TSepiImportsTControl.DoContextPopup,
      'procedure(MousePos: TPoint; var Handled: Boolean)',
      mlkDynamic);
    AddMethod('DoEndDock', @TSepiImportsTControl.DoEndDock,
      'procedure(Target: TObject; X, Y: Integer)',
      mlkDynamic);
    AddMethod('DoDock', @TSepiImportsTControl.DoDock,
      'procedure(NewDockSite: TWinControl; var ARect: TRect)',
      mlkDynamic);
    AddMethod('DoStartDock', @TSepiImportsTControl.DoStartDock,
      'procedure(var DragObject: TDragObject)',
      mlkDynamic);
    AddMethod('DoMouseWheel', @TSepiImportsTControl.DoMouseWheel,
      'function(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint ) : Boolean',
      mlkDynamic);
    AddMethod('DoMouseWheelDown', @TSepiImportsTControl.DoMouseWheelDown,
      'function(Shift: TShiftState; MousePos: TPoint): Boolean',
      mlkDynamic);
    AddMethod('DoMouseWheelUp', @TSepiImportsTControl.DoMouseWheelUp,
      'function(Shift: TShiftState; MousePos: TPoint): Boolean',
      mlkDynamic);
    AddMethod('DragCanceled', @TSepiImportsTControl.DragCanceled,
      'procedure',
      mlkDynamic);
    AddMethod('DragOver', @TSepiImportsTControl.DragOver,
      'procedure(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean )',
      mlkDynamic);
    AddMethod('DoEndDrag', @TSepiImportsTControl.DoEndDrag,
      'procedure(Target: TObject; X, Y: Integer)',
      mlkDynamic);
    AddMethod('DoStartDrag', @TSepiImportsTControl.DoStartDrag,
      'procedure(var DragObject: TDragObject)',
      mlkDynamic);
    AddMethod('DrawDragDockImage', @TSepiImportsTControl.DrawDragDockImage,
      'procedure(DragDockObject: TDragDockObject)',
      mlkDynamic);
    AddMethod('EraseDragDockImage', @TSepiImportsTControl.EraseDragDockImage,
      'procedure(DragDockObject: TDragDockObject)',
      mlkDynamic);
    AddMethod('GetAction', @TSepiImportsTControl.GetAction,
      'function: TBasicAction',
      mlkVirtual);
    AddMethod('GetActionLinkClass', @TSepiImportsTControl.GetActionLinkClass,
      'function: TControlActionLinkClass',
      mlkDynamic);
    AddMethod('GetClientOrigin', @TSepiImportsTControl.GetClientOrigin,
      'function: TPoint',
      mlkVirtual);
    AddMethod('GetClientRect', @TSepiImportsTControl.GetClientRect,
      'function: TRect',
      mlkVirtual);
    AddMethod('GetDeviceContext', @TSepiImportsTControl.GetDeviceContext,
      'function(var WindowHandle: HWnd): HDC',
      mlkVirtual);
    AddMethod('GetDockEdge', @TSepiImportsTControl.GetDockEdge,
      'function(MousePos: TPoint): TAlign',
      mlkDynamic);
    AddMethod('GetDragImages', @TSepiImportsTControl.GetDragImages,
      'function: TDragImageList',
      mlkVirtual);
    AddMethod('GetEnabled', @TSepiImportsTControl.GetEnabled,
      'function: Boolean',
      mlkVirtual);
    AddMethod('GetFloating', @TSepiImportsTControl.GetFloating,
      'function: Boolean',
      mlkVirtual);
    AddMethod('GetFloatingDockSiteClass', @TSepiImportsTControl.GetFloatingDockSiteClass,
      'function: TWinControlClass',
      mlkVirtual);
    AddMethod('GetPalette', @TSepiImportsTControl.GetPalette,
      'function: HPALETTE',
      mlkDynamic);
    AddMethod('GetPopupMenu', @TSepiImportsTControl.GetPopupMenu,
      'function: TPopupMenu',
      mlkDynamic);
    AddMethod('Loaded', @TSepiImportsTControl.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('MouseActivate', @TSepiImportsTControl.MouseActivate,
      'function(Button: TMouseButton; Shift: TShiftState; X, Y: Integer ; HitTest: Integer ) : TMouseActivate',
      mlkDynamic);
    AddMethod('MouseDown', @TSepiImportsTControl.MouseDown,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer )',
      mlkDynamic);
    AddMethod('MouseMove', @TSepiImportsTControl.MouseMove,
      'procedure(Shift: TShiftState; X, Y: Integer)',
      mlkDynamic);
    AddMethod('MouseUp', @TSepiImportsTControl.MouseUp,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer )',
      mlkDynamic);
    AddMethod('Notification', @TSepiImportsTControl.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation )',
      mlkOverride);
    AddMethod('PositionDockRect', @TSepiImportsTControl.PositionDockRect,
      'procedure(DragDockObject: TDragDockObject)',
      mlkDynamic);
    AddMethod('PaletteChanged', @TSepiImportsTControl.PaletteChanged,
      'function(Foreground: Boolean): Boolean',
      mlkDynamic);
    AddMethod('ReadState', @TSepiImportsTControl.ReadState,
      'procedure(Reader: TReader)',
      mlkOverride);
    AddMethod('RequestAlign', @TSepiImportsTControl.RequestAlign,
      'procedure',
      mlkVirtual);
    AddMethod('Resize', @TSepiImportsTControl.Resize,
      'procedure',
      mlkDynamic);
    AddMethod('SendCancelMode', @TSepiImportsTControl.SendCancelMode,
      'procedure(Sender: TControl)');
    AddMethod('SendDockNotification', @TSepiImportsTControl.SendDockNotification,
      'procedure(Msg: Cardinal; WParam, LParam: Integer)');
    AddMethod('SetAutoSize', @TSepiImportsTControl.SetAutoSize,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('SetDragMode', @TSepiImportsTControl.SetDragMode,
      'procedure(Value: TDragMode)',
      mlkVirtual);
    AddMethod('SetEnabled', @TSepiImportsTControl.SetEnabled,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('SetName', @TSepiImportsTControl.SetName,
      'procedure(const Value: TComponentName)',
      mlkOverride);
    AddMethod('SetParent', @TSepiImportsTControl.SetParent,
      'procedure(AParent: TWinControl)',
      mlkVirtual);
    AddMethod('SetParentComponent', @TSepiImportsTControl.SetParentComponent,
      'procedure(Value: TComponent)',
      mlkOverride);
    AddMethod('SetParentBiDiMode', @TSepiImportsTControl.SetParentBiDiMode,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('SetBiDiMode', @TSepiImportsTControl.SetBiDiMode,
      'procedure(Value: TBiDiMode)',
      mlkVirtual);
    AddMethod('SetZOrder', @TSepiImportsTControl.SetZOrder,
      'procedure(TopMost: Boolean)',
      mlkDynamic);
    AddMethod('UpdateBoundsRect', @TSepiImportsTControl.UpdateBoundsRect,
      'procedure(const R: TRect)');
    AddMethod('VisibleChanging', @TSepiImportsTControl.VisibleChanging,
      'procedure',
      mlkDynamic);
    AddMethod('WndProc', @TSepiImportsTControl.WndProc,
      'procedure(var Message: TMessage)',
      mlkVirtual);

    AddProperty('ActionLink', 'property: TControlActionLink',
      'FActionLink', 'FActionLink');
    AddProperty('AutoSize', 'property: Boolean',
      'FAutoSize', 'SetAutoSize',
      NoIndex, Integer(False));
    AddProperty('Caption', 'property: TCaption',
      'GetText', 'SetText',
      NoIndex, NoDefaultValue, 'IsCaptionStored');
    AddProperty('Color', 'property: TColor',
      'FColor', 'SetColor',
      NoIndex, clWindow, 'IsColorStored');
    AddProperty('DesktopFont', 'property: Boolean',
      'FDesktopFont', 'SetDesktopFont',
      NoIndex, Integer(False));
    AddProperty('DragKind', 'property: TDragKind',
      'FDragKind', 'FDragKind',
      NoIndex, Integer(dkDrag));
    AddProperty('DragCursor', 'property: TCursor',
      'FDragCursor', 'FDragCursor',
      NoIndex, crDrag);
    AddProperty('DragMode', 'property: TDragMode',
      'FDragMode', 'SetDragMode',
      NoIndex, Integer(dmManual));
    AddProperty('Font', 'property: TFont',
      'FFont', 'SetFont',
      NoIndex, NoDefaultValue, 'IsFontStored');
    AddProperty('IsControl', 'property: Boolean',
      'FIsControl', 'FIsControl');
    AddProperty('MouseCapture', 'property: Boolean',
      'GetMouseCapture', 'SetMouseCapture');
    AddProperty('ParentBiDiMode', 'property: Boolean',
      'FParentBiDiMode', 'SetParentBiDiMode',
      NoIndex, Integer(True));
    AddProperty('ParentColor', 'property: Boolean',
      'FParentColor', 'SetParentColor',
      NoIndex, Integer(True));
    AddProperty('ParentFont', 'property: Boolean',
      'FParentFont', 'SetParentFont',
      NoIndex, Integer(True));
    AddProperty('ParentShowHint', 'property: Boolean',
      'FParentShowHint', 'SetParentShowHint',
      NoIndex, Integer(True));
    AddProperty('PopupMenu', 'property: TPopupMenu',
      'FPopupMenu', 'SetPopupMenu');
    AddProperty('ScalingFlags', 'property: TScalingFlags',
      'FScalingFlags', 'FScalingFlags');
    AddProperty('Text', 'property: TCaption',
      'GetText', 'SetText');
    AddProperty('WheelAccumulator', 'property: Integer',
      'FWheelAccumulator', 'FWheelAccumulator');
    AddProperty('WindowText', 'property: PChar',
      'FText', 'FText');
    AddProperty('OnCanResize', 'property: TCanResizeEvent',
      'FOnCanResize', 'FOnCanResize');
    AddProperty('OnClick', 'property: TNotifyEvent',
      'FOnClick', 'FOnClick',
      NoIndex, NoDefaultValue, 'IsOnClickStored');
    AddProperty('OnConstrainedResize', 'property: TConstrainedResizeEvent',
      'FOnConstrainedResize', 'FOnConstrainedResize');
    AddProperty('OnContextPopup', 'property: TContextPopupEvent',
      'FOnContextPopup', 'FOnContextPopup');
    AddProperty('OnDblClick', 'property: TNotifyEvent',
      'FOnDblClick', 'FOnDblClick');
    AddProperty('OnDragDrop', 'property: TDragDropEvent',
      'FOnDragDrop', 'FOnDragDrop');
    AddProperty('OnDragOver', 'property: TDragOverEvent',
      'FOnDragOver', 'FOnDragOver');
    AddProperty('OnEndDock', 'property: TEndDragEvent',
      'FOnEndDock', 'FOnEndDock');
    AddProperty('OnEndDrag', 'property: TEndDragEvent',
      'FOnEndDrag', 'FOnEndDrag');
    AddProperty('OnMouseActivate', 'property: TMouseActivateEvent',
      'FOnMouseActivate', 'FOnMouseActivate');
    AddProperty('OnMouseDown', 'property: TMouseEvent',
      'FOnMouseDown', 'FOnMouseDown');
    AddProperty('OnMouseMove', 'property: TMouseMoveEvent',
      'FOnMouseMove', 'FOnMouseMove');
    AddProperty('OnMouseUp', 'property: TMouseEvent',
      'FOnMouseUp', 'FOnMouseUp');
    AddProperty('OnMouseWheel', 'property: TMouseWheelEvent',
      'FOnMouseWheel', 'FOnMouseWheel');
    AddProperty('OnMouseWheelDown', 'property: TMouseWheelUpDownEvent',
      'FOnMouseWheelDown', 'FOnMouseWheelDown');
    AddProperty('OnMouseWheelUp', 'property: TMouseWheelUpDownEvent',
      'FOnMouseWheelUp', 'FOnMouseWheelUp');
    AddProperty('OnResize', 'property: TNotifyEvent',
      'FOnResize', 'FOnResize');
    AddProperty('OnStartDock', 'property: TStartDockEvent',
      'FOnStartDock', 'FOnStartDock');
    AddProperty('OnStartDrag', 'property: TStartDragEvent',
      'FOnStartDrag', 'FOnStartDrag');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTControl.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTControl.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('BeginDrag', @TSepiImportsTControl.BeginDrag,
      'procedure(Immediate: Boolean; Threshold: Integer = -1)');
    AddMethod('BringToFront', @TSepiImportsTControl.BringToFront,
      'procedure');
    AddMethod('ClientToScreen', @TSepiImportsTControl.ClientToScreen,
      'function(const Point: TPoint): TPoint');
    AddMethod('ClientToParent', @TSepiImportsTControl.ClientToParent,
      'function(const Point: TPoint; AParent: TWinControl = nil): TPoint');
    AddMethod('Dock', @TSepiImportsTControl.Dock,
      'procedure(NewDockSite: TWinControl; ARect: TRect)',
      mlkDynamic);
    AddMethod('DefaultHandler', @TSepiImportsTControl.DefaultHandler,
      'procedure(var Message)',
      mlkOverride);
    AddMethod('Dragging', @TSepiImportsTControl.Dragging,
      'function: Boolean');
    AddMethod('DragDrop', @TSepiImportsTControl.DragDrop,
      'procedure(Source: TObject; X, Y: Integer)',
      mlkDynamic);
    AddMethod('DrawTextBiDiModeFlags', @TSepiImportsTControl.DrawTextBiDiModeFlags,
      'function(Flags: Longint): Longint');
    AddMethod('DrawTextBiDiModeFlagsReadingOnly', @TSepiImportsTControl.DrawTextBiDiModeFlagsReadingOnly,
      'function: Longint');

    AddProperty('Enabled', 'property: Boolean',
      'GetEnabled', 'SetEnabled',
      NoIndex, Integer(True), 'IsEnabledStored');

    AddMethod('EndDrag', @TSepiImportsTControl.EndDrag,
      'procedure(Drop: Boolean)');
    AddMethod('GetControlsAlignment', @TSepiImportsTControl.GetControlsAlignment,
      'function: TAlignment',
      mlkDynamic);
    AddMethod('GetParentComponent', @TSepiImportsTControl.GetParentComponent,
      'function: TComponent',
      mlkOverride);
    AddMethod('GetTextBuf', @TSepiImportsTControl.GetTextBuf,
      'function(Buffer: PChar; BufSize: Integer): Integer');
    AddMethod('GetTextLen', @TSepiImportsTControl.GetTextLen,
      'function: Integer');
    AddMethod('HasParent', @TSepiImportsTControl.HasParent,
      'function: Boolean',
      mlkOverride);
    AddMethod('Hide', @TSepiImportsTControl.Hide,
      'procedure');
    AddMethod('InitiateAction', @TSepiImportsTControl.InitiateAction,
      'procedure',
      mlkVirtual);
    AddMethod('Invalidate', @TSepiImportsTControl.Invalidate,
      'procedure',
      mlkVirtual);
    AddMethod('MouseWheelHandler', @TSepiImportsTControl.MouseWheelHandler,
      'procedure(var Message: TMessage)',
      mlkDynamic);
    AddMethod('IsRightToLeft', @TSepiImportsTControl.IsRightToLeft,
      'function: Boolean');
    AddMethod('ManualDock', @TSepiImportsTControl.ManualDock,
      'function(NewDockSite: TWinControl; DropControl: TControl = nil; ControlSide: TAlign = alNone ) : Boolean');
    AddMethod('ManualFloat', @TSepiImportsTControl.ManualFloat,
      'function(ScreenPos: TRect): Boolean');
    AddMethod('Perform', @TSepiImportsTControl.Perform,
      'function(Msg: Cardinal; WParam, LParam: Longint): Longint');
    AddMethod('Refresh', @TSepiImportsTControl.Refresh,
      'procedure');
    AddMethod('Repaint', @TSepiImportsTControl.Repaint,
      'procedure',
      mlkVirtual);
    AddMethod('ReplaceDockedControl', @TSepiImportsTControl.ReplaceDockedControl,
      'function(Control: TControl; NewDockSite: TWinControl; DropControl: TControl ; ControlSide: TAlign ) : Boolean');
    AddMethod('ScreenToClient', @TSepiImportsTControl.ScreenToClient,
      'function(const Point: TPoint): TPoint');
    AddMethod('ParentToClient', @TSepiImportsTControl.ParentToClient,
      'function(const Point: TPoint; AParent: TWinControl = nil): TPoint');
    AddMethod('SendToBack', @TSepiImportsTControl.SendToBack,
      'procedure');
    AddMethod('SetBounds', @TSepiImportsTControl.SetBounds,
      'procedure(ALeft, ATop, AWidth, AHeight: Integer)',
      mlkVirtual);
    AddMethod('SetTextBuf', @TSepiImportsTControl.SetTextBuf,
      'procedure(Buffer: PChar)');
    AddMethod('Show', @TSepiImportsTControl.Show,
      'procedure');
    AddMethod('Update', @TSepiImportsTControl.Update,
      'procedure',
      mlkVirtual);
    AddMethod('UseRightToLeftAlignment', @TSepiImportsTControl.UseRightToLeftAlignment,
      'function: Boolean',
      mlkDynamic);
    AddMethod('UseRightToLeftReading', @TSepiImportsTControl.UseRightToLeftReading,
      'function: Boolean');
    AddMethod('UseRightToLeftScrollBar', @TSepiImportsTControl.UseRightToLeftScrollBar,
      'function: Boolean');

    AddProperty('Action', 'property: TBasicAction',
      'GetAction', 'SetAction');
    AddProperty('Align', 'property: TAlign',
      'FAlign', 'SetAlign',
      NoIndex, Integer(alNone));
    AddProperty('Anchors', 'property: TAnchors',
      'FAnchors', 'SetAnchors',
      NoIndex, Byte(DefaultAnchors), 'IsAnchorsStored');
    AddProperty('BiDiMode', 'property: TBiDiMode',
      'FBiDiMode', 'SetBiDiMode',
      NoIndex, NoDefaultValue, 'IsBiDiModeStored');
    AddProperty('BoundsRect', 'property: TRect',
      'GetBoundsRect', 'SetBoundsRect');
    AddProperty('ClientHeight', 'property: Integer',
      'GetClientHeight', 'SetClientHeight',
      NoIndex, NoDefaultValue, 'False');
    AddProperty('ClientOrigin', 'property: TPoint',
      'GetClientOrigin', '');
    AddProperty('ClientRect', 'property: TRect',
      'GetClientRect', '');
    AddProperty('ClientWidth', 'property: Integer',
      'GetClientWidth', 'SetClientWidth',
      NoIndex, NoDefaultValue, 'False');
    AddProperty('Constraints', 'property: TSizeConstraints',
      'FConstraints', 'SetConstraints');
    AddProperty('ControlState', 'property: TControlState',
      'FControlState', 'FControlState');
    AddProperty('ControlStyle', 'property: TControlStyle',
      'FControlStyle', 'FControlStyle');
    AddProperty('DockOrientation', 'property: TDockOrientation',
      'FDockOrientation', 'FDockOrientation');
    AddProperty('Floating', 'property: Boolean',
      'GetFloating', '');
    AddProperty('FloatingDockSiteClass', 'property: TWinControlClass',
      'GetFloatingDockSiteClass', 'FFloatingDockSiteClass');
    AddProperty('HostDockSite', 'property: TWinControl',
      'FHostDockSite', 'SetHostDockSite');
    AddProperty('LRDockWidth', 'property: Integer',
      'GetLRDockWidth', 'FLRDockWidth');
    AddProperty('Parent', 'property: TWinControl',
      'FParent', 'SetParent');
    AddProperty('ShowHint', 'property: Boolean',
      'FShowHint', 'SetShowHint',
      NoIndex, NoDefaultValue, 'IsShowHintStored');
    AddProperty('TBDockHeight', 'property: Integer',
      'GetTBDockHeight', 'FTBDockHeight');
    AddProperty('UndockHeight', 'property: Integer',
      'GetUndockHeight', 'FUndockHeight');
    AddProperty('UndockWidth', 'property: Integer',
      'GetUndockWidth', 'FUndockWidth');
    AddProperty('Visible', 'property: Boolean',
      'FVisible', 'SetVisible',
      NoIndex, Integer(True), 'IsVisibleStored');
    AddProperty('WindowProc', 'property: TWndMethod',
      'FWindowProc', 'FWindowProc');

    CurrentVisibility := mvPublished;

    AddProperty('Left', 'property: Integer',
      'FLeft', 'SetLeft');
    AddProperty('Top', 'property: Integer',
      'FTop', 'SetTop');
    AddProperty('Width', 'property: Integer',
      'FWidth', 'SetWidth');
    AddProperty('Height', 'property: Integer',
      'FHeight', 'SetHeight');
    AddProperty('Cursor', 'property: TCursor',
      'FCursor', 'SetCursor',
      NoIndex, crDefault);
    AddProperty('Hint', 'property: string',
      'FHint', 'FHint',
      NoIndex, NoDefaultValue, 'IsHintStored');
    AddProperty('HelpType', 'property: THelpType',
      'FHelpType', 'FHelpType',
      NoIndex, Integer(htContext));
    AddProperty('HelpKeyword', 'property: String',
      'FHelpKeyword', 'SetHelpKeyword',
      NoIndex, NoDefaultValue, 'IsHelpContextStored');
    AddProperty('HelpContext', 'property: THelpContext',
      'FHelpContext', 'SetHelpContext',
      NoIndex, 0, 'IsHelpContextStored');

    Complete;
  end;
end;

{----------------------}
{ TCreateParams import }
{----------------------}

function SepiImportTCreateParams(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCreateParams', False, True);

  with Result do
  begin
    AddField('Caption', 'PChar');
    AddField('Style', System.TypeInfo(DWORD));
    AddField('ExStyle', System.TypeInfo(DWORD));
    AddField('X', System.TypeInfo(Integer));
    AddField('Y', System.TypeInfo(Integer), True);
    AddField('Width', System.TypeInfo(Integer));
    AddField('Height', System.TypeInfo(Integer), True);
    AddField('WndParent', System.TypeInfo(HWnd));
    AddField('Param', 'Pointer');
    AddField('WindowClass', 'TWndClass');
    AddField('WinClassName', '$1');

    Complete;
  end;
end;

{------------------------------}
{ TWinControlActionLink import }
{------------------------------}

class function TSepiImportsTWinControlActionLink.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TWinControlActionLink));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FClient', System.TypeInfo(TWinControl));

    AddMethod('AssignClient', @TSepiImportsTWinControlActionLink.AssignClient,
      'procedure(AClient: TObject)',
      mlkOverride);
    AddMethod('IsHelpContextLinked', @TSepiImportsTWinControlActionLink.IsHelpContextLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('SetHelpContext', @TSepiImportsTWinControlActionLink.SetHelpContext,
      'procedure(Value: THelpContext)',
      mlkOverride);

    Complete;
  end;
end;

{-------------------}
{ TAlignInfo import }
{-------------------}

function SepiImportTAlignInfo(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TAlignInfo', False, True);

  with Result do
  begin
    AddField('AlignList', System.TypeInfo(TList));
    AddField('ControlIndex', System.TypeInfo(Integer));
    AddField('Align', System.TypeInfo(TAlign));
    AddField('Scratch', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------}
{ IDockManager import }
{---------------------}

function SepiImportIDockManager(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDockManager));

  with Result do
  begin
    AddMethod('BeginUpdate',
      'procedure', ccRegister);
    AddMethod('EndUpdate',
      'procedure', ccRegister);
    AddMethod('GetControlBounds',
      'procedure(Control: TControl; out CtlBounds: TRect)', ccRegister);
    AddMethod('InsertControl',
      'procedure(Control: TControl; InsertAt: TAlign; DropCtl: TControl )', ccRegister);
    AddMethod('LoadFromStream',
      'procedure(Stream: TStream)', ccRegister);
    AddMethod('PaintSite',
      'procedure(DC: HDC)', ccRegister);
    AddMethod('PositionDockRect',
      'procedure(Client, DropCtl: TControl; DropAlign: TAlign; var DockRect: TRect )', ccRegister);
    AddMethod('RemoveControl',
      'procedure(Control: TControl)', ccRegister);
    AddMethod('ResetBounds',
      'procedure(Force: Boolean)', ccRegister);
    AddMethod('SaveToStream',
      'procedure(Stream: TStream)', ccRegister);
    AddMethod('SetReplacingControl',
      'procedure(Control: TControl)', ccRegister);

    Complete;
  end;
end;

{--------------------}
{ TWinControl import }
{--------------------}

function TSepiImportsTWinControl.GetAlignDisabled: Boolean;
begin
  Result := AlignDisabled;
end;

function TSepiImportsTWinControl.GetControl(Index: Integer): TControl;
begin
  Result := Controls[Index];
end;

function TSepiImportsTWinControl.GetControlCount: Integer;
begin
  Result := ControlCount;
end;

function TSepiImportsTWinControl.GetDockClientCount: Integer;
begin
  Result := DockClientCount;
end;

function TSepiImportsTWinControl.GetDockClients(Index: Integer): TControl;
begin
  Result := DockClients[Index];
end;

function TSepiImportsTWinControl.GetHandle: HWnd;
begin
  Result := Handle;
end;

function TSepiImportsTWinControl.GetParentBackground: Boolean;
begin
  Result := ParentBackground;
end;

function TSepiImportsTWinControl.GetTabOrder: TTabOrder;
begin
  Result := TabOrder;
end;

function TSepiImportsTWinControl.GetVisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TSepiImportsTWinControl.SetBevelEdges(const Value: TBevelEdges);
begin
  BevelEdges := Value;
end;

procedure TSepiImportsTWinControl.SetBevelKind(const Value: TBevelKind);
begin
  BevelKind := Value;
end;

procedure TSepiImportsTWinControl.SetBevelWidth(const Value: TBevelWidth);
begin
  BevelWidth := Value;
end;

procedure TSepiImportsTWinControl.SetBorderWidth(Value: TBorderWidth);
begin
  BorderWidth := Value;
end;

procedure TSepiImportsTWinControl.SetCtl3D(Value: Boolean);
begin
  Ctl3D := Value;
end;

procedure TSepiImportsTWinControl.SetDockSite(Value: Boolean);
begin
  DockSite := Value;
end;

procedure TSepiImportsTWinControl.SetParentCtl3D(Value: Boolean);
begin
  ParentCtl3D := Value;
end;

procedure TSepiImportsTWinControl.SetParentWindow(Value: HWnd);
begin
  ParentWindow := Value;
end;

procedure TSepiImportsTWinControl.SetTabOrder(Value: TTabOrder);
begin
  TabOrder := Value;
end;

procedure TSepiImportsTWinControl.SetTabStop(Value: Boolean);
begin
  TabStop := Value;
end;

procedure TSepiImportsTWinControl.SetUseDockManager(Value: Boolean);
begin
  UseDockManager := Value;
end;

procedure TSepiImportsTWinControl.PaintTo_0(DC: HDC; X, Y: Integer);
begin
  PaintTo(DC, X, Y);
end;

procedure TSepiImportsTWinControl.PaintTo_1(Canvas: TCanvas; X, Y: Integer);
begin
  PaintTo(Canvas, X, Y);
end;

class function TSepiImportsTWinControl.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
const
  DefaultBevelEdges : TBevelEdges = [beLeft, beTop, beRight, beBottom];
begin
  Result := TSepiClass(Owner.FindMeta('TWinControl'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TWinControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAlignLevel', System.TypeInfo(Word));
    AddField('FBevelEdges', System.TypeInfo(TBevelEdges));
    AddField('FBevelInner', System.TypeInfo(TBevelCut));
    AddField('FBevelOuter', System.TypeInfo(TBevelCut));
    AddField('FBevelKind', System.TypeInfo(TBevelKind));
    AddField('FBevelWidth', System.TypeInfo(TBevelWidth));
    AddField('FBorderWidth', System.TypeInfo(TBorderWidth));
    AddField('FBrush', System.TypeInfo(TBrush));
    AddField('FDefWndProc', 'Pointer');
    AddField('FDockClients', System.TypeInfo(TList));
    AddField('FDockManager', System.TypeInfo(IDockManager));
    AddField('FHandle', System.TypeInfo(HWnd));
    AddField('FImeMode', System.TypeInfo(TImeMode));
    AddField('FImeName', System.TypeInfo(TImeName));
    AddField('FObjectInstance', 'Pointer');
    AddField('FParentWindow', System.TypeInfo(HWnd));
    AddField('FTabList', System.TypeInfo(TList));
    AddField('FControls', System.TypeInfo(TList));
    AddField('FWinControls', System.TypeInfo(TList));
    AddField('FTabOrder', System.TypeInfo(Integer));
    AddField('FTabStop', System.TypeInfo(Boolean));
    AddField('FCtl3D', System.TypeInfo(Boolean));
    AddField('FShowing', System.TypeInfo(Boolean));
    AddField('FUseDockManager', System.TypeInfo(Boolean));
    AddField('FDockSite', System.TypeInfo(Boolean));
    AddField('FParentCtl3D', System.TypeInfo(Boolean));
    AddField('FOnDockDrop', System.TypeInfo(TDockDropEvent));
    AddField('FOnDockOver', System.TypeInfo(TDockOverEvent));
    AddField('FOnEnter', System.TypeInfo(TNotifyEvent));
    AddField('FOnExit', System.TypeInfo(TNotifyEvent));
    AddField('FOnGetSiteInfo', System.TypeInfo(TGetSiteInfoEvent));
    AddField('FOnKeyDown', System.TypeInfo(TKeyEvent));
    AddField('FOnKeyPress', System.TypeInfo(TKeyPressEvent));
    AddField('FOnKeyUp', System.TypeInfo(TKeyEvent));
    AddField('FOnUnDock', System.TypeInfo(TUnDockEvent));
    AddField('FOnAlignInsertBefore', System.TypeInfo(TAlignInsertBeforeEvent));
    AddField('FOnAlignPosition', System.TypeInfo(TAlignPositionEvent));

    AddMethod('AlignControl', nil,
      'procedure(AControl: TControl)');
    AddMethod('CalcConstraints', nil,
      'procedure(var MinWidth, MinHeight, MaxWidth, MaxHeight : Integer )');
    AddMethod('GetAlignDisabled', @TSepiImportsTWinControl.GetAlignDisabled,
      'function: Boolean');
    AddMethod('GetControl', @TSepiImportsTWinControl.GetControl,
      'function(Index: Integer): TControl');
    AddMethod('GetControlCount', @TSepiImportsTWinControl.GetControlCount,
      'function: Integer');
    AddMethod('GetDockClientCount', @TSepiImportsTWinControl.GetDockClientCount,
      'function: Integer');
    AddMethod('GetDockClients', @TSepiImportsTWinControl.GetDockClients,
      'function(Index: Integer): TControl');
    AddMethod('GetHandle', @TSepiImportsTWinControl.GetHandle,
      'function: HWnd');
    AddMethod('GetParentBackground', @TSepiImportsTWinControl.GetParentBackground,
      'function: Boolean');
    AddMethod('GetTabOrder', @TSepiImportsTWinControl.GetTabOrder,
      'function: TTabOrder');
    AddMethod('GetVisibleDockClientCount', @TSepiImportsTWinControl.GetVisibleDockClientCount,
      'function: Integer');
    AddMethod('Insert', nil,
      'procedure(AControl: TControl)');
    AddMethod('InvalidateFrame', nil,
      'procedure');
    AddMethod('InvokeHelp', nil,
      'procedure');
    AddMethod('IsCtl3DStored', nil,
      'function: Boolean');
    AddMethod('PrecedingWindow', nil,
      'function(Control: TWinControl): HWnd');
    AddMethod('ReadDesignSize', nil,
      'procedure(Reader: TReader)');
    AddMethod('Remove', nil,
      'procedure(AControl: TControl)');
    AddMethod('RemoveFocus', nil,
      'procedure(Removing: Boolean)');
    AddMethod('SetBevelCut', nil,
      'procedure(Index: Integer; const Value: TBevelCut)');
    AddMethod('SetBevelEdges', @TSepiImportsTWinControl.SetBevelEdges,
      'procedure(const Value: TBevelEdges)');
    AddMethod('SetBevelKind', @TSepiImportsTWinControl.SetBevelKind,
      'procedure(const Value: TBevelKind)');
    AddMethod('SetBevelWidth', @TSepiImportsTWinControl.SetBevelWidth,
      'procedure(const Value: TBevelWidth)');
    AddMethod('SetBorderWidth', @TSepiImportsTWinControl.SetBorderWidth,
      'procedure(Value: TBorderWidth)');
    AddMethod('SetCtl3D', @TSepiImportsTWinControl.SetCtl3D,
      'procedure(Value: Boolean)');
    AddMethod('SetDockSite', @TSepiImportsTWinControl.SetDockSite,
      'procedure(Value: Boolean)');
    AddMethod('SetParentCtl3D', @TSepiImportsTWinControl.SetParentCtl3D,
      'procedure(Value: Boolean)');
    AddMethod('SetParentWindow', @TSepiImportsTWinControl.SetParentWindow,
      'procedure(Value: HWnd)');
    AddMethod('SetTabOrder', @TSepiImportsTWinControl.SetTabOrder,
      'procedure(Value: TTabOrder)');
    AddMethod('SetTabStop', @TSepiImportsTWinControl.SetTabStop,
      'procedure(Value: Boolean)');
    AddMethod('SetUseDockManager', @TSepiImportsTWinControl.SetUseDockManager,
      'procedure(Value: Boolean)');
    AddMethod('SetZOrderPosition', nil,
      'procedure(Position: Integer)');
    AddMethod('UpdateTabOrder', nil,
      'procedure(Value: TTabOrder)');
    AddMethod('UpdateShowing', nil,
      'procedure');
    AddMethod('WriteDesignSize', nil,
      'procedure(Writer: TWriter)');
    AddMethod('IsMenuKey', nil,
      'function(var Message: TWMKey): Boolean');
    AddMethod('WMPaint', nil,
      'procedure(var Message: TWMPaint)',
      mlkMessage, False, WM_PAINT);
    AddMethod('WMCommand', nil,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, WM_COMMAND);
    AddMethod('WMNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, WM_NOTIFY);
    AddMethod('WMSysColorChange', nil,
      'procedure(var Message: TWMSysColorChange)',
      mlkMessage, False, WM_SYSCOLORCHANGE);
    AddMethod('WMHScroll', nil,
      'procedure(var Message: TWMHScroll)',
      mlkMessage, False, WM_HSCROLL);
    AddMethod('WMVScroll', nil,
      'procedure(var Message: TWMVScroll)',
      mlkMessage, False, WM_VSCROLL);
    AddMethod('WMCompareItem', nil,
      'procedure(var Message: TWMCompareItem)',
      mlkMessage, False, WM_COMPAREITEM);
    AddMethod('WMDeleteItem', nil,
      'procedure(var Message: TWMDeleteItem)',
      mlkMessage, False, WM_DELETEITEM);
    AddMethod('WMDrawItem', nil,
      'procedure(var Message: TWMDrawItem)',
      mlkMessage, False, WM_DRAWITEM);
    AddMethod('WMMeasureItem', nil,
      'procedure(var Message: TWMMeasureItem)',
      mlkMessage, False, WM_MEASUREITEM);
    AddMethod('WMEraseBkgnd', nil,
      'procedure(var Message: TWmEraseBkgnd)',
      mlkMessage, False, WM_ERASEBKGND);
    AddMethod('WMWindowPosChanged', nil,
      'procedure(var Message: TWMWindowPosChanged)',
      mlkMessage, False, WM_WINDOWPOSCHANGED);
    AddMethod('WMWindowPosChanging', nil,
      'procedure(var Message: TWMWindowPosChanging)',
      mlkMessage, False, WM_WINDOWPOSCHANGING);
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('WMMove', nil,
      'procedure(var Message: TWMMove)',
      mlkMessage, False, WM_MOVE);
    AddMethod('WMSetCursor', nil,
      'procedure(var Message: TWMSetCursor)',
      mlkMessage, False, WM_SETCURSOR);
    AddMethod('WMKeyDown', nil,
      'procedure(var Message: TWMKeyDown)',
      mlkMessage, False, WM_KEYDOWN);
    AddMethod('WMSysKeyDown', nil,
      'procedure(var Message: TWMKeyDown)',
      mlkMessage, False, WM_SYSKEYDOWN);
    AddMethod('WMKeyUp', nil,
      'procedure(var Message: TWMKeyUp)',
      mlkMessage, False, WM_KEYUP);
    AddMethod('WMSysKeyUp', nil,
      'procedure(var Message: TWMKeyUp)',
      mlkMessage, False, WM_SYSKEYUP);
    AddMethod('WMChar', nil,
      'procedure(var Message: TWMChar)',
      mlkMessage, False, WM_CHAR);
    AddMethod('WMSysCommand', nil,
      'procedure(var Message: TWMSysCommand)',
      mlkMessage, False, WM_SYSCOMMAND);
    AddMethod('WMCharToItem', nil,
      'procedure(var Message: TWMCharToItem)',
      mlkMessage, False, WM_CHARTOITEM);
    AddMethod('WMParentNotify', nil,
      'procedure(var Message: TWMParentNotify)',
      mlkMessage, False, WM_PARENTNOTIFY);
    AddMethod('WMVKeyToItem', nil,
      'procedure(var Message: TWMVKeyToItem)',
      mlkMessage, False, WM_VKEYTOITEM);
    AddMethod('WMDestroy', nil,
      'procedure(var Message: TWMDestroy)',
      mlkMessage, False, WM_DESTROY);
    AddMethod('WMMouseActivate', nil,
      'procedure(var Message: TWMMouseActivate)',
      mlkMessage, False, WM_MOUSEACTIVATE);
    AddMethod('WMNCCalcSize', nil,
      'procedure(var Message: TWMNCCalcSize)',
      mlkMessage, False, WM_NCCALCSIZE);
    AddMethod('WMNCDestroy', nil,
      'procedure(var Message: TWMNCDestroy)',
      mlkMessage, False, WM_NCDESTROY);
    AddMethod('WMNCHitTest', nil,
      'procedure(var Message: TWMNCHitTest)',
      mlkMessage, False, WM_NCHITTEST);
    AddMethod('WMNCPaint', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_NCPAINT);
    AddMethod('WMQueryNewPalette', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_QUERYNEWPALETTE);
    AddMethod('WMPaletteChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_PALETTECHANGED);
    AddMethod('WMWinIniChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_WININICHANGE);
    AddMethod('WMFontChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_FONTCHANGE);
    AddMethod('WMTimeChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_TIMECHANGE);
    AddMethod('WMSetFocus', nil,
      'procedure(var Message: TWMSetFocus)',
      mlkMessage, False, WM_SETFOCUS);
    AddMethod('WMKillFocus', nil,
      'procedure(var Message: TWMSetFocus)',
      mlkMessage, False, WM_KILLFOCUS);
    AddMethod('WMIMEStartComp', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_IME_STARTCOMPOSITION);
    AddMethod('WMIMEEndComp', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_IME_ENDCOMPOSITION);
    AddMethod('WMContextMenu', nil,
      'procedure(var Message: TWMContextMenu)',
      mlkMessage, False, WM_CONTEXTMENU);
    AddMethod('CMChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CHANGED);
    AddMethod('CMChildKey', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CHILDKEY);
    AddMethod('CMDialogKey', nil,
      'procedure(var Message: TCMDialogKey)',
      mlkMessage, False, CM_DIALOGKEY);
    AddMethod('CMDialogChar', nil,
      'procedure(var Message: TCMDialogChar)',
      mlkMessage, False, CM_DIALOGCHAR);
    AddMethod('CMFocusChanged', nil,
      'procedure(var Message: TCMFocusChanged)',
      mlkMessage, False, CM_FOCUSCHANGED);
    AddMethod('CMVisibleChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_VISIBLECHANGED);
    AddMethod('CMEnabledChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ENABLEDCHANGED);
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CMBorderChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BORDERCHANGED);
    AddMethod('CMCursorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CURSORCHANGED);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('CMParentCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTCTL3DCHANGED);
    AddMethod('CMShowingChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SHOWINGCHANGED);
    AddMethod('CMShowHintChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SHOWHINTCHANGED);
    AddMethod('CMEnter', nil,
      'procedure(var Message: TCMEnter)',
      mlkMessage, False, CM_ENTER);
    AddMethod('CMExit', nil,
      'procedure(var Message: TCMExit)',
      mlkMessage, False, CM_EXIT);
    AddMethod('CMDesignHitTest', nil,
      'procedure(var Message: TCMDesignHitTest)',
      mlkMessage, False, CM_DESIGNHITTEST);
    AddMethod('CMSysColorChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSCOLORCHANGE);
    AddMethod('CMSysFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSFONTCHANGED);
    AddMethod('CMWinIniChange', nil,
      'procedure(var Message: TWMWinIniChange)',
      mlkMessage, False, CM_WININICHANGE);
    AddMethod('CMFontChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGE);
    AddMethod('CMTimeChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TIMECHANGE);
    AddMethod('CMDrag', nil,
      'procedure(var Message: TCMDrag)',
      mlkMessage, False, CM_DRAG);
    AddMethod('CNKeyDown', nil,
      'procedure(var Message: TWMKeyDown)',
      mlkMessage, False, CN_KEYDOWN);
    AddMethod('CNKeyUp', nil,
      'procedure(var Message: TWMKeyUp)',
      mlkMessage, False, CN_KEYUP);
    AddMethod('CNChar', nil,
      'procedure(var Message: TWMChar)',
      mlkMessage, False, CN_CHAR);
    AddMethod('CNSysKeyDown', nil,
      'procedure(var Message: TWMKeyDown)',
      mlkMessage, False, CN_SYSKEYDOWN);
    AddMethod('CNSysChar', nil,
      'procedure(var Message: TWMChar)',
      mlkMessage, False, CN_SYSCHAR);
    AddMethod('CMControlListChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CONTROLLISTCHANGE);
    AddMethod('CMRecreateWnd', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_RECREATEWND);
    AddMethod('CMInvalidate', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_INVALIDATE);
    AddMethod('CMDockClient', nil,
      'procedure(var Message: TCMDockClient)',
      mlkMessage, False, CM_DOCKCLIENT);
    AddMethod('CMUnDockClient', nil,
      'procedure(var Message: TCMUnDockClient)',
      mlkMessage, False, CM_UNDOCKCLIENT);
    AddMethod('CMFloat', nil,
      'procedure(var Message: TCMFloat)',
      mlkMessage, False, CM_FLOAT);
    AddMethod('CMBiDiModeChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BIDIMODECHANGED);
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);
    AddMethod('WMPrintClient', nil,
      'procedure(var Message: TWMPrintClient)',
      mlkMessage, False, WM_PRINTCLIENT);

    CurrentVisibility := mvProtected;

    AddField('FDoubleBuffered', System.TypeInfo(Boolean));
    AddField('FInImeComposition', System.TypeInfo(Boolean));
    AddField('FDesignSize', 'TPoint');

    AddMethod('ActionChange', @TSepiImportsTWinControl.ActionChange,
      'procedure(Sender: TObject; CheckDefaults: Boolean)',
      mlkOverride);
    AddMethod('AddBiDiModeExStyle', @TSepiImportsTWinControl.AddBiDiModeExStyle,
      'procedure(var ExStyle: DWORD)');
    AddMethod('AssignTo', @TSepiImportsTWinControl.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);
    AddMethod('AdjustClientRect', @TSepiImportsTWinControl.AdjustClientRect,
      'procedure(var Rect: TRect)',
      mlkVirtual);
    AddMethod('AdjustSize', @TSepiImportsTWinControl.AdjustSize,
      'procedure',
      mlkOverride);
    AddMethod('AlignControls', @TSepiImportsTWinControl.AlignControls,
      'procedure(AControl: TControl; var Rect: TRect)',
      mlkVirtual);
    AddMethod('CanAutoSize', @TSepiImportsTWinControl.CanAutoSize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('CanResize', @TSepiImportsTWinControl.CanResize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('ChangeScale', @TSepiImportsTWinControl.ChangeScale,
      'procedure(M, D: Integer)',
      mlkOverride);
    AddMethod('ConstrainedResize', @TSepiImportsTWinControl.ConstrainedResize,
      'procedure(var MinWidth, MinHeight, MaxWidth, MaxHeight : Integer )',
      mlkOverride);
    AddMethod('ControlsAligned', @TSepiImportsTWinControl.ControlsAligned,
      'procedure',
      mlkDynamic);
    AddMethod('CreateDockManager', @TSepiImportsTWinControl.CreateDockManager,
      'function: IDockManager',
      mlkDynamic);
    AddMethod('CreateHandle', @TSepiImportsTWinControl.CreateHandle,
      'procedure',
      mlkVirtual);
    AddMethod('CreateParams', @TSepiImportsTWinControl.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkVirtual);
    AddMethod('CreateSubClass', @TSepiImportsTWinControl.CreateSubClass,
      'procedure(var Params: TCreateParams; ControlClassName: PChar )');
    AddMethod('CreateWindowHandle', @TSepiImportsTWinControl.CreateWindowHandle,
      'procedure(const Params: TCreateParams)',
      mlkVirtual);
    AddMethod('CreateWnd', @TSepiImportsTWinControl.CreateWnd,
      'procedure',
      mlkVirtual);
    AddMethod('CustomAlignInsertBefore', @TSepiImportsTWinControl.CustomAlignInsertBefore,
      'function(C1, C2: TControl): Boolean',
      mlkVirtual);
    AddMethod('CustomAlignPosition', @TSepiImportsTWinControl.CustomAlignPosition,
      'procedure(Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight : Integer ; var AlignRect: TRect ; AlignInfo: TAlignInfo )',
      mlkVirtual);
    AddMethod('DefineProperties', @TSepiImportsTWinControl.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('DestroyHandle', @TSepiImportsTWinControl.DestroyHandle,
      'procedure',
      mlkVirtual);
    AddMethod('DestroyWindowHandle', @TSepiImportsTWinControl.DestroyWindowHandle,
      'procedure',
      mlkVirtual);
    AddMethod('DestroyWnd', @TSepiImportsTWinControl.DestroyWnd,
      'procedure',
      mlkVirtual);
    AddMethod('DoAddDockClient', @TSepiImportsTWinControl.DoAddDockClient,
      'procedure(Client: TControl; const ARect: TRect)',
      mlkDynamic);
    AddMethod('DockOver', @TSepiImportsTWinControl.DockOver,
      'procedure(Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean )',
      mlkDynamic);
    AddMethod('DockReplaceDockClient', @TSepiImportsTWinControl.DockReplaceDockClient,
      'function(Client: TControl; NewDockSite: TWinControl ; DropControl: TControl ; ControlSide: TAlign ; ReplacementClient: TControl ) : Boolean',
      mlkVirtual);
    AddMethod('DoDockOver', @TSepiImportsTWinControl.DoDockOver,
      'procedure(Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean )',
      mlkDynamic);
    AddMethod('DoEnter', @TSepiImportsTWinControl.DoEnter,
      'procedure',
      mlkDynamic);
    AddMethod('DoExit', @TSepiImportsTWinControl.DoExit,
      'procedure',
      mlkDynamic);
    AddMethod('DoFlipChildren', @TSepiImportsTWinControl.DoFlipChildren,
      'procedure',
      mlkDynamic);
    AddMethod('DoKeyDown', @TSepiImportsTWinControl.DoKeyDown,
      'function(var Message: TWMKey): Boolean');
    AddMethod('DoKeyPress', @TSepiImportsTWinControl.DoKeyPress,
      'function(var Message: TWMKey): Boolean');
    AddMethod('DoKeyUp', @TSepiImportsTWinControl.DoKeyUp,
      'function(var Message: TWMKey): Boolean');
    AddMethod('DoRemoveDockClient', @TSepiImportsTWinControl.DoRemoveDockClient,
      'procedure(Client: TControl)',
      mlkDynamic);
    AddMethod('DoUnDock', @TSepiImportsTWinControl.DoUnDock,
      'function(NewTarget: TWinControl; Client: TControl): Boolean',
      mlkDynamic);
    AddMethod('FindNextControl', @TSepiImportsTWinControl.FindNextControl,
      'function(CurControl: TWinControl; GoForward, CheckTabStop, CheckParent: Boolean ) : TWinControl');
    AddMethod('FixupTabList', @TSepiImportsTWinControl.FixupTabList,
      'procedure');
    AddMethod('GetActionLinkClass', @TSepiImportsTWinControl.GetActionLinkClass,
      'function: TControlActionLinkClass',
      mlkOverride);
    AddMethod('GetChildren', @TSepiImportsTWinControl.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('GetClientOrigin', @TSepiImportsTWinControl.GetClientOrigin,
      'function: TPoint',
      mlkOverride);
    AddMethod('GetClientRect', @TSepiImportsTWinControl.GetClientRect,
      'function: TRect',
      mlkOverride);
    AddMethod('GetControlExtents', @TSepiImportsTWinControl.GetControlExtents,
      'function: TRect',
      mlkVirtual);
    AddMethod('GetDeviceContext', @TSepiImportsTWinControl.GetDeviceContext,
      'function(var WindowHandle: HWnd): HDC',
      mlkOverride);
    AddMethod('GetParentHandle', @TSepiImportsTWinControl.GetParentHandle,
      'function: HWnd');
    AddMethod('GetSiteInfo', @TSepiImportsTWinControl.GetSiteInfo,
      'procedure(Client: TControl; var InfluenceRect: TRect; MousePos: TPoint ; var CanDock: Boolean )',
      mlkDynamic);
    AddMethod('GetTopParentHandle', @TSepiImportsTWinControl.GetTopParentHandle,
      'function: HWnd');
    AddMethod('InvalidateDockHostSite', @TSepiImportsTWinControl.InvalidateDockHostSite,
      'procedure(FocusLost: Boolean)');
    AddMethod('IsControlMouseMsg', @TSepiImportsTWinControl.IsControlMouseMsg,
      'function(var Message: TWMMouse): Boolean');
    AddMethod('IsControlActivateMsg', @TSepiImportsTWinControl.IsControlActivateMsg,
      'function(var Message: TWMMouseActivate; Control: TControl = nil): Boolean');
    AddMethod('IsQualifyingSite', @TSepiImportsTWinControl.IsQualifyingSite,
      'function(const Client: TControl): Boolean',
      mlkDynamic);
    AddMethod('KeyDown', @TSepiImportsTWinControl.KeyDown,
      'procedure(var Key: Word; Shift: TShiftState)',
      mlkDynamic);
    AddMethod('KeyUp', @TSepiImportsTWinControl.KeyUp,
      'procedure(var Key: Word; Shift: TShiftState)',
      mlkDynamic);
    AddMethod('KeyPress', @TSepiImportsTWinControl.KeyPress,
      'procedure(var Key: Char)',
      mlkDynamic);
    AddMethod('MainWndProc', @TSepiImportsTWinControl.MainWndProc,
      'procedure(var Message: TMessage)');
    AddMethod('NotifyControls', @TSepiImportsTWinControl.NotifyControls,
      'procedure(Msg: Word)');
    AddMethod('PaintControls', @TSepiImportsTWinControl.PaintControls,
      'procedure(DC: HDC; First: TControl)');
    AddMethod('PaintHandler', @TSepiImportsTWinControl.PaintHandler,
      'procedure(var Message: TWMPaint)');
    AddMethod('PaintWindow', @TSepiImportsTWinControl.PaintWindow,
      'procedure(DC: HDC)',
      mlkVirtual);
    AddMethod('PaletteChanged', @TSepiImportsTWinControl.PaletteChanged,
      'function(Foreground: Boolean): Boolean',
      mlkOverride);
    AddMethod('ReadState', @TSepiImportsTWinControl.ReadState,
      'procedure(Reader: TReader)',
      mlkOverride);
    AddMethod('RecreateWnd', @TSepiImportsTWinControl.RecreateWnd,
      'procedure');
    AddMethod('ReloadDockedControl', @TSepiImportsTWinControl.ReloadDockedControl,
      'procedure(const AControlName: string; var AControl: TControl )',
      mlkDynamic);
    AddMethod('ResetIme', @TSepiImportsTWinControl.ResetIme,
      'procedure');
    AddMethod('ResetImeComposition', @TSepiImportsTWinControl.ResetImeComposition,
      'function(Action: DWORD): Boolean');
    AddMethod('RemoveWindowProps', @TSepiImportsTWinControl.RemoveWindowProps,
      'procedure');
    AddMethod('ScaleControls', @TSepiImportsTWinControl.ScaleControls,
      'procedure(M, D: Integer)');
    AddMethod('SelectFirst', @TSepiImportsTWinControl.SelectFirst,
      'procedure');
    AddMethod('SelectNext', @TSepiImportsTWinControl.SelectNext,
      'procedure(CurControl: TWinControl; GoForward, CheckTabStop: Boolean )');
    AddMethod('SetChildOrder', @TSepiImportsTWinControl.SetChildOrder,
      'procedure(Child: TComponent; Order: Integer)',
      mlkOverride);
    AddMethod('SetIme', @TSepiImportsTWinControl.SetIme,
      'procedure');
    AddMethod('SetImeCompositionWindow', @TSepiImportsTWinControl.SetImeCompositionWindow,
      'function(Font: TFont; XPos, YPos: Integer): Boolean');
    AddMethod('SetParentBackground', @TSepiImportsTWinControl.SetParentBackground,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('SetZOrder', @TSepiImportsTWinControl.SetZOrder,
      'procedure(TopMost: Boolean)',
      mlkOverride);
    AddMethod('ShowControl', @TSepiImportsTWinControl.ShowControl,
      'procedure(AControl: TControl)',
      mlkVirtual);
    AddMethod('UpdateBounds', @TSepiImportsTWinControl.UpdateBounds,
      'procedure');
    AddMethod('UpdateUIState', @TSepiImportsTWinControl.UpdateUIState,
      'procedure(CharCode: Word)');
    AddMethod('WndProc', @TSepiImportsTWinControl.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);

    AddProperty('BevelEdges', 'property: TBevelEdges',
      'FBevelEdges', 'SetBevelEdges',
      NoIndex, Byte(DefaultBevelEdges));
    AddProperty('BevelInner', 'property: TBevelCut',
      'FBevelInner', 'SetBevelCut',
      0, Integer(bvRaised));
    AddProperty('BevelOuter', 'property: TBevelCut',
      'FBevelOuter', 'SetBevelCut',
      1, Integer(bvLowered));
    AddProperty('BevelKind', 'property: TBevelKind',
      'FBevelKind', 'SetBevelKind',
      NoIndex, Integer(bkNone));
    AddProperty('BevelWidth', 'property: TBevelWidth',
      'FBevelWidth', 'SetBevelWidth',
      NoIndex, 1);
    AddProperty('BorderWidth', 'property: TBorderWidth',
      'FBorderWidth', 'SetBorderWidth',
      NoIndex, 0);
    AddProperty('Ctl3D', 'property: Boolean',
      'FCtl3D', 'SetCtl3D',
      NoIndex, NoDefaultValue, 'IsCtl3DStored');
    AddProperty('DefWndProc', 'property: Pointer',
      'FDefWndProc', 'FDefWndProc');
    AddProperty('ImeMode', 'property: TImeMode',
      'FImeMode', 'FImeMode',
      NoIndex, Integer(imDontCare));
    AddProperty('ImeName', 'property: TImeName',
      'FImeName', 'FImeName');
    AddProperty('ParentBackground', 'property: Boolean',
      'GetParentBackground', 'SetParentBackground');
    AddProperty('ParentCtl3D', 'property: Boolean',
      'FParentCtl3D', 'SetParentCtl3D',
      NoIndex, Integer(True));
    AddProperty('WindowHandle', 'property: HWnd',
      'FHandle', 'FHandle');
    AddProperty('OnAlignInsertBefore', 'property: TAlignInsertBeforeEvent',
      'FOnAlignInsertBefore', 'FOnAlignInsertBefore');
    AddProperty('OnAlignPosition', 'property: TAlignPositionEvent',
      'FOnAlignPosition', 'FOnAlignPosition');
    AddProperty('OnDockDrop', 'property: TDockDropEvent',
      'FOnDockDrop', 'FOnDockDrop');
    AddProperty('OnDockOver', 'property: TDockOverEvent',
      'FOnDockOver', 'FOnDockOver');
    AddProperty('OnEnter', 'property: TNotifyEvent',
      'FOnEnter', 'FOnEnter');
    AddProperty('OnExit', 'property: TNotifyEvent',
      'FOnExit', 'FOnExit');
    AddProperty('OnGetSiteInfo', 'property: TGetSiteInfoEvent',
      'FOnGetSiteInfo', 'FOnGetSiteInfo');
    AddProperty('OnKeyDown', 'property: TKeyEvent',
      'FOnKeyDown', 'FOnKeyDown');
    AddProperty('OnKeyPress', 'property: TKeyPressEvent',
      'FOnKeyPress', 'FOnKeyPress');
    AddProperty('OnKeyUp', 'property: TKeyEvent',
      'FOnKeyUp', 'FOnKeyUp');
    AddProperty('OnUnDock', 'property: TUnDockEvent',
      'FOnUnDock', 'FOnUnDock');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTWinControl.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('CreateParented', @TSepiImportsTWinControl.CreateParented,
      'constructor(ParentWindow: HWnd)');
    AddMethod('CreateParentedControl', @TSepiImportsTWinControl.CreateParentedControl,
      'class function(ParentWindow: HWnd): TWinControl');
    AddMethod('Destroy', @TSepiImportsTWinControl.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Broadcast', @TSepiImportsTWinControl.Broadcast,
      'procedure(var Message)');
    AddMethod('CanFocus', @TSepiImportsTWinControl.CanFocus,
      'function: Boolean',
      mlkDynamic);
    AddMethod('ContainsControl', @TSepiImportsTWinControl.ContainsControl,
      'function(Control: TControl): Boolean');
    AddMethod('ControlAtPos', @TSepiImportsTWinControl.ControlAtPos,
      'function(const Pos: TPoint; AllowDisabled: Boolean; AllowWinControls: Boolean = False ; AllLevels: Boolean = False ) : TControl');
    AddMethod('DefaultHandler', @TSepiImportsTWinControl.DefaultHandler,
      'procedure(var Message)',
      mlkOverride);
    AddMethod('DisableAlign', @TSepiImportsTWinControl.DisableAlign,
      'procedure');

    AddProperty('DockClientCount', 'property: Integer',
      'GetDockClientCount', '');
    AddProperty('DockClients', 'property[Index: Integer]: TControl',
      'GetDockClients', '');

    AddMethod('DockDrop', @TSepiImportsTWinControl.DockDrop,
      'procedure(Source: TDragDockObject; X, Y: Integer)',
      mlkDynamic);

    AddProperty('DockSite', 'property: Boolean',
      'FDockSite', 'SetDockSite',
      NoIndex, Integer(False));
    AddProperty('DockManager', 'property: IDockManager',
      'FDockManager', 'FDockManager');
    AddProperty('DoubleBuffered', 'property: Boolean',
      'FDoubleBuffered', 'FDoubleBuffered');

    AddMethod('EnableAlign', @TSepiImportsTWinControl.EnableAlign,
      'procedure');
    AddMethod('FindChildControl', @TSepiImportsTWinControl.FindChildControl,
      'function(const ControlName: string): TControl');
    AddMethod('FlipChildren', @TSepiImportsTWinControl.FlipChildren,
      'procedure(AllLevels: Boolean)',
      mlkDynamic);
    AddMethod('Focused', @TSepiImportsTWinControl.Focused,
      'function: Boolean',
      mlkDynamic);
    AddMethod('GetTabOrderList', @TSepiImportsTWinControl.GetTabOrderList,
      'procedure(List: TList)',
      mlkDynamic);
    AddMethod('HandleAllocated', @TSepiImportsTWinControl.HandleAllocated,
      'function: Boolean');
    AddMethod('HandleNeeded', @TSepiImportsTWinControl.HandleNeeded,
      'procedure');
    AddMethod('InsertControl', @TSepiImportsTWinControl.InsertControl,
      'procedure(AControl: TControl)');
    AddMethod('Invalidate', @TSepiImportsTWinControl.Invalidate,
      'procedure',
      mlkOverride);
    AddOverloadedMethod('PaintTo', @TSepiImportsTWinControl.PaintTo_0,
      'procedure(DC: HDC; X, Y: Integer)');
    AddOverloadedMethod('PaintTo', @TSepiImportsTWinControl.PaintTo_1,
      'procedure(Canvas: TCanvas; X, Y: Integer)');
    AddMethod('PreProcessMessage', @TSepiImportsTWinControl.PreProcessMessage,
      'function(var Msg: TMsg): Boolean',
      mlkDynamic);
    AddMethod('RemoveControl', @TSepiImportsTWinControl.RemoveControl,
      'procedure(AControl: TControl)');
    AddMethod('Realign', @TSepiImportsTWinControl.Realign,
      'procedure');
    AddMethod('Repaint', @TSepiImportsTWinControl.Repaint,
      'procedure',
      mlkOverride);
    AddMethod('ScaleBy', @TSepiImportsTWinControl.ScaleBy,
      'procedure(M, D: Integer)');
    AddMethod('ScrollBy', @TSepiImportsTWinControl.ScrollBy,
      'procedure(DeltaX, DeltaY: Integer)');
    AddMethod('SetBounds', @TSepiImportsTWinControl.SetBounds,
      'procedure(ALeft, ATop, AWidth, AHeight: Integer)',
      mlkOverride);
    AddMethod('SetFocus', @TSepiImportsTWinControl.SetFocus,
      'procedure',
      mlkVirtual);
    AddMethod('Update', @TSepiImportsTWinControl.Update,
      'procedure',
      mlkOverride);
    AddMethod('UpdateControlState', @TSepiImportsTWinControl.UpdateControlState,
      'procedure');

    AddProperty('AlignDisabled', 'property: Boolean',
      'GetAlignDisabled', '');
    AddProperty('VisibleDockClientCount', 'property: Integer',
      'GetVisibleDockClientCount', '');
    AddProperty('Brush', 'property: TBrush',
      'FBrush', '');
    AddProperty('Controls', 'property[Index: Integer]: TControl',
      'GetControl', '');
    AddProperty('ControlCount', 'property: Integer',
      'GetControlCount', '');
    AddProperty('Handle', 'property: HWnd',
      'GetHandle', '');
    AddProperty('ParentWindow', 'property: HWnd',
      'FParentWindow', 'SetParentWindow');
    AddProperty('Showing', 'property: Boolean',
      'FShowing', '');
    AddProperty('TabOrder', 'property: TTabOrder',
      'GetTabOrder', 'SetTabOrder',
      NoIndex, -1);
    AddProperty('TabStop', 'property: Boolean',
      'FTabStop', 'SetTabStop',
      NoIndex, Integer(False));
    AddProperty('UseDockManager', 'property: Boolean',
      'FUseDockManager', 'SetUseDockManager',
      NoIndex, Integer(False));

    CurrentVisibility := mvPublished;

    Complete;
  end;
end;

{------------------------}
{ TGraphicControl import }
{------------------------}

class function TSepiImportsTGraphicControl.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TGraphicControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCanvas', System.TypeInfo(TCanvas));

    AddMethod('WMPaint', nil,
      'procedure(var Message: TWMPaint)',
      mlkMessage, False, WM_PAINT);

    CurrentVisibility := mvProtected;

    AddMethod('Paint', @TSepiImportsTGraphicControl.Paint,
      'procedure',
      mlkVirtual);

    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTGraphicControl.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTGraphicControl.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{-----------------------}
{ TCustomControl import }
{-----------------------}

class function TSepiImportsTCustomControl.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCanvas', System.TypeInfo(TCanvas));

    AddMethod('WMPaint', nil,
      'procedure(var Message: TWMPaint)',
      mlkMessage, False, WM_PAINT);

    CurrentVisibility := mvProtected;

    AddMethod('Paint', @TSepiImportsTCustomControl.Paint,
      'procedure',
      mlkVirtual);
    AddMethod('PaintWindow', @TSepiImportsTCustomControl.PaintWindow,
      'procedure(DC: HDC)',
      mlkOverride);

    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomControl.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomControl.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{--------------------}
{ THintWindow import }
{--------------------}

class function TSepiImportsTHintWindow.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(THintWindow));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FActivating', System.TypeInfo(Boolean));
    AddField('FLastActive', System.TypeInfo(Cardinal));

    AddMethod('WMNCHitTest', nil,
      'procedure(var Message: TWMNCHitTest)',
      mlkMessage, False, WM_NCHITTEST);
    AddMethod('WMNCPaint', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_NCPAINT);
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTHintWindow.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('NCPaint', @TSepiImportsTHintWindow.NCPaint,
      'procedure(DC: HDC)',
      mlkVirtual);
    AddMethod('Paint', @TSepiImportsTHintWindow.Paint,
      'procedure',
      mlkOverride);
    AddMethod('WMPrint', @TSepiImportsTHintWindow.WMPrint,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_PRINT);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTHintWindow.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('ActivateHint', @TSepiImportsTHintWindow.ActivateHint,
      'procedure(Rect: TRect; const AHint: string)',
      mlkVirtual);
    AddMethod('ActivateHintData', @TSepiImportsTHintWindow.ActivateHintData,
      'procedure(Rect: TRect; const AHint: string; AData: Pointer)',
      mlkVirtual);
    AddMethod('CalcHintRect', @TSepiImportsTHintWindow.CalcHintRect,
      'function(MaxWidth: Integer; const AHint: string; AData: Pointer ) : TRect',
      mlkVirtual);
    AddMethod('IsHintMsg', @TSepiImportsTHintWindow.IsHintMsg,
      'function(var Msg: TMsg): Boolean',
      mlkVirtual);
    AddMethod('ShouldHideHint', @TSepiImportsTHintWindow.ShouldHideHint,
      'function: Boolean',
      mlkVirtual);
    AddMethod('ReleaseHandle', @TSepiImportsTHintWindow.ReleaseHandle,
      'procedure');

    RedefineProperty('BiDiMode');
    RedefineProperty('Caption');
    RedefineProperty('Color');
    RedefineProperty('Canvas');
    RedefineProperty('Font');

    Complete;
  end;
end;

{-----------------------}
{ TDragImageList import }
{-----------------------}

procedure TSepiImportsTDragImageList.SetDragCursor(Value: TCursor);
begin
  DragCursor := Value;
end;

class function TSepiImportsTDragImageList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TDragImageList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TDragImageList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDragCursor', System.TypeInfo(TCursor));
    AddField('FDragging', System.TypeInfo(Boolean));
    AddField('FDragHandle', System.TypeInfo(HWND));
    AddField('FDragHotspot', 'TPoint');
    AddField('FDragIndex', System.TypeInfo(Integer));
    AddField('FOldCursor', System.TypeInfo(TCursor));

    AddMethod('SetDragCursor', @TSepiImportsTDragImageList.SetDragCursor,
      'procedure(Value: TCursor)');

    CurrentVisibility := mvProtected;

    AddMethod('Initialize', @TSepiImportsTDragImageList.Initialize,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('BeginDrag', @TSepiImportsTDragImageList.BeginDrag,
      'function(Window: HWND; X, Y: Integer): Boolean');
    AddMethod('DragLock', @TSepiImportsTDragImageList.DragLock,
      'function(Window: HWND; XPos, YPos: Integer): Boolean');
    AddMethod('DragMove', @TSepiImportsTDragImageList.DragMove,
      'function(X, Y: Integer): Boolean');
    AddMethod('DragUnlock', @TSepiImportsTDragImageList.DragUnlock,
      'procedure');
    AddMethod('EndDrag', @TSepiImportsTDragImageList.EndDrag,
      'function: Boolean');
    AddMethod('GetHotSpot', @TSepiImportsTDragImageList.GetHotSpot,
      'function: TPoint',
      mlkOverride);
    AddMethod('HideDragImage', @TSepiImportsTDragImageList.HideDragImage,
      'procedure');
    AddMethod('SetDragImage', @TSepiImportsTDragImageList.SetDragImage,
      'function(Index, HotSpotX, HotSpotY: Integer): Boolean');
    AddMethod('ShowDragImage', @TSepiImportsTDragImageList.ShowDragImage,
      'procedure');

    AddProperty('DragCursor', 'property: TCursor',
      'FDragCursor', 'SetDragCursor');
    AddProperty('DragHotspot', 'property: TPoint',
      'FDragHotspot', 'FDragHotspot');
    AddProperty('Dragging', 'property: Boolean',
      'FDragging', '');

    Complete;
  end;
end;

{-------------------}
{ TImageList import }
{-------------------}

class function TSepiImportsTImageList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TImageList));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('BlendColor');
    RedefineProperty('BkColor');
    RedefineProperty('AllocBy');
    RedefineProperty('DrawingStyle');
    RedefineProperty('Height');
    RedefineProperty('ImageType');
    RedefineProperty('Masked');
    RedefineProperty('OnChange');
    RedefineProperty('ShareImages');
    RedefineProperty('Width');

    Complete;
  end;
end;

{------------------}
{ TDockZone import }
{------------------}

function TSepiImportsTDockZone.GetChildCount: Integer;
begin
  Result := ChildCount;
end;

function TSepiImportsTDockZone.GetLimitBegin: Integer;
begin
  Result := LimitBegin;
end;

function TSepiImportsTDockZone.GetLimitSize: Integer;
begin
  Result := LimitSize;
end;

function TSepiImportsTDockZone.GetVisible: Boolean;
begin
  Result := Visible;
end;

function TSepiImportsTDockZone.GetVisibleChildCount: Integer;
begin
  Result := VisibleChildCount;
end;

function TSepiImportsTDockZone.GetZoneLimit: Integer;
begin
  Result := ZoneLimit;
end;

procedure TSepiImportsTDockZone.SetZoneLimit(const Value: Integer);
begin
  ZoneLimit := Value;
end;

class function TSepiImportsTDockZone.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDockZone));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FChildControl', System.TypeInfo(TControl));
    AddField('FChildZones', System.TypeInfo(TDockZone));
    AddField('FNextSibling', System.TypeInfo(TDockZone));
    AddField('FOrientation', System.TypeInfo(TDockOrientation));
    AddField('FParentZone', System.TypeInfo(TDockZone));
    AddField('FPrevSibling', System.TypeInfo(TDockZone));
    AddField('FTree', System.TypeInfo(TDockTree));
    AddField('FZoneLimit', System.TypeInfo(Integer));
    AddField('FOldSize', System.TypeInfo(Integer));

    AddMethod('GetChildCount', @TSepiImportsTDockZone.GetChildCount,
      'function: Integer');
    AddMethod('GetControlName', nil,
      'function: string');
    AddMethod('GetLimitBegin', @TSepiImportsTDockZone.GetLimitBegin,
      'function: Integer');
    AddMethod('GetLimitSize', @TSepiImportsTDockZone.GetLimitSize,
      'function: Integer');
    AddMethod('GetTopLeft', nil,
      'function(Orient: Integer  ) : Integer');
    AddMethod('GetHeightWidth', nil,
      'function(Orient: Integer  ) : Integer');
    AddMethod('GetVisible', @TSepiImportsTDockZone.GetVisible,
      'function: Boolean');
    AddMethod('GetVisibleChildCount', @TSepiImportsTDockZone.GetVisibleChildCount,
      'function: Integer');
    AddMethod('GetZoneLimit', @TSepiImportsTDockZone.GetZoneLimit,
      'function: Integer');
    AddMethod('SetControlName', nil,
      'function(const Value: string): Boolean');
    AddMethod('SetZoneLimit', @TSepiImportsTDockZone.SetZoneLimit,
      'procedure(const Value: Integer)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTDockZone.Create,
      'constructor(Tree: TDockTree)');
    AddMethod('ExpandZoneLimit', @TSepiImportsTDockZone.ExpandZoneLimit,
      'procedure(NewLimit: Integer)');
    AddMethod('FirstVisibleChild', @TSepiImportsTDockZone.FirstVisibleChild,
      'function: TDockZone');
    AddMethod('NextVisible', @TSepiImportsTDockZone.NextVisible,
      'function: TDockZone');
    AddMethod('PrevVisible', @TSepiImportsTDockZone.PrevVisible,
      'function: TDockZone');
    AddMethod('ResetChildren', @TSepiImportsTDockZone.ResetChildren,
      'procedure');
    AddMethod('ResetZoneLimits', @TSepiImportsTDockZone.ResetZoneLimits,
      'procedure');
    AddMethod('Update', @TSepiImportsTDockZone.Update,
      'procedure');

    AddProperty('ChildCount', 'property: Integer',
      'GetChildCount', '');
    AddProperty('ChildControl', 'property: TControl',
      'FChildControl', '');
    AddProperty('Height', 'property: Integer',
      'GetHeightWidth', '',
      Ord(doHorizontal));
    AddProperty('Left', 'property: Integer',
      'GetTopLeft', '',
      Ord(doVertical));
    AddProperty('LimitBegin', 'property: Integer',
      'GetLimitBegin', '');
    AddProperty('LimitSize', 'property: Integer',
      'GetLimitSize', '');
    AddProperty('Top', 'property: Integer',
      'GetTopLeft', '',
      Ord(doHorizontal));
    AddProperty('Visible', 'property: Boolean',
      'GetVisible', '');
    AddProperty('VisibleChildCount', 'property: Integer',
      'GetVisibleChildCount', '');
    AddProperty('Width', 'property: Integer',
      'GetHeightWidth', '',
      Ord(doVertical));
    AddProperty('ZoneLimit', 'property: Integer',
      'GetZoneLimit', 'SetZoneLimit');

    Complete;
  end;
end;

{------------------}
{ TDockTree import }
{------------------}

class function TSepiImportsTDockTree.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TDockTree'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TDockTree));

  with Result do
  begin
    AddInterface(System.TypeInfo(IDockManager));

    CurrentVisibility := mvPrivate;

    AddField('FBorderWidth', System.TypeInfo(Integer));
    AddField('FBrush', System.TypeInfo(TBrush));
    AddField('FDockSite', System.TypeInfo(TWinControl));
    AddField('FGrabberSize', System.TypeInfo(Integer));
    AddField('FGrabbersOnTop', System.TypeInfo(Boolean));
    AddField('FOldRect', 'TRect');
    AddField('FOldWndProc', System.TypeInfo(TWndMethod));
    AddField('FReplacementZone', System.TypeInfo(TDockZone));
    AddField('FScaleBy', System.TypeInfo(Double));
    AddField('FShiftScaleOrient', System.TypeInfo(TDockOrientation));
    AddField('FShiftBy', System.TypeInfo(Integer));
    AddField('FSizePos', 'TPoint');
    AddField('FSizingDC', System.TypeInfo(HDC));
    AddField('FSizingWnd', System.TypeInfo(HWND));
    AddField('FSizingZone', System.TypeInfo(TDockZone));
    AddField('FTopZone', System.TypeInfo(TDockZone));
    AddField('FTopXYLimit', System.TypeInfo(Integer));
    AddField('FUpdateCount', System.TypeInfo(Integer));
    AddField('FVersion', System.TypeInfo(Integer));
    AddField('FRelativeSizes', System.TypeInfo(Boolean));

    AddMethod('ControlVisibilityChanged', nil,
      'procedure(Control: TControl; Visible: Boolean)');
    AddMethod('ActualSize', nil,
      'function(const RelativeSize, Reference: Integer): Integer');
    AddMethod('RelativeSize', nil,
      'function(const ActualSize, Reference: Integer): Integer');
    AddMethod('DrawSizeSplitter', nil,
      'procedure');
    AddMethod('FindControlZone', nil,
      'function(Control: TControl): TDockZone');
    AddMethod('ForEachAt', nil,
      'procedure(Zone: TDockZone; Proc: TForEachZoneProc)');
    AddMethod('GetNextLimit', nil,
      'function(AZone: TDockZone): Integer');
    AddMethod('InsertNewParent', nil,
      'procedure(NewZone, SiblingZone: TDockZone; ParentOrientation: TDockOrientation ; InsertLast: Boolean )');
    AddMethod('InsertSibling', nil,
      'procedure(NewZone, SiblingZone: TDockZone; InsertLast: Boolean)');
    AddMethod('InternalHitTest', nil,
      'function(const MousePos: TPoint; out HTFlag: Integer ) : TDockZone');
    AddMethod('PruneZone', nil,
      'procedure(Zone: TDockZone)');
    AddMethod('RemoveZone', nil,
      'procedure(Zone: TDockZone)');
    AddMethod('ScaleZone', nil,
      'procedure(Zone: TDockZone)');
    AddMethod('SetNewBounds', nil,
      'procedure(Zone: TDockZone)');
    AddMethod('ShiftZone', nil,
      'procedure(Zone: TDockZone)');
    AddMethod('SplitterMouseDown', nil,
      'procedure(OnZone: TDockZone; MousePos: TPoint)');
    AddMethod('SplitterMouseUp', nil,
      'procedure');
    AddMethod('UpdateZone', nil,
      'procedure(Zone: TDockZone)');
    AddMethod('WindowProc', nil,
      'procedure(var Message: TMessage)');

    CurrentVisibility := mvProtected;

    AddMethod('AdjustDockRect', @TSepiImportsTDockTree.AdjustDockRect,
      'procedure(Control: TControl; var ARect: TRect)',
      mlkVirtual);
    AddMethod('AdjustFrameRect', @TSepiImportsTDockTree.AdjustFrameRect,
      'procedure(Control: TControl; var ARect: TRect)',
      mlkVirtual);
    AddMethod('BeginUpdate', @TSepiImportsTDockTree.BeginUpdate,
      'procedure');
    AddMethod('EndUpdate', @TSepiImportsTDockTree.EndUpdate,
      'procedure');
    AddMethod('FindControlAtPos', @TSepiImportsTDockTree.FindControlAtPos,
      'function(const Pos: TPoint): TControl');
    AddMethod('GetControlBounds', @TSepiImportsTDockTree.GetControlBounds,
      'procedure(Control: TControl; out CtlBounds: TRect)');
    AddMethod('HitTest', @TSepiImportsTDockTree.HitTest,
      'function(const MousePos: TPoint; out HTFlag: Integer): TControl',
      mlkVirtual);
    AddMethod('InsertControl', @TSepiImportsTDockTree.InsertControl,
      'procedure(Control: TControl; InsertAt: TAlign; DropCtl: TControl )',
      mlkVirtual);
    AddMethod('LoadFromStream', @TSepiImportsTDockTree.LoadFromStream,
      'procedure(Stream: TStream)',
      mlkVirtual);
    AddMethod('MouseDown', @TSepiImportsTDockTree.MouseDown,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer ; var Handled: Boolean )',
      mlkVirtual);
    AddMethod('MouseMove', @TSepiImportsTDockTree.MouseMove,
      'procedure(Shift: TShiftState; X, Y: Integer; var Handled: Boolean )',
      mlkVirtual);
    AddMethod('MouseUp', @TSepiImportsTDockTree.MouseUp,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer ; var Handled: Boolean )',
      mlkVirtual);
    AddMethod('PaintDockFrame', @TSepiImportsTDockTree.PaintDockFrame,
      'procedure(Canvas: TCanvas; Control: TControl; const ARect: TRect )',
      mlkVirtual);
    AddMethod('PositionDockRect', @TSepiImportsTDockTree.PositionDockRect,
      'procedure(Client, DropCtl: TControl; DropAlign: TAlign; var DockRect: TRect )',
      mlkVirtual);
    AddMethod('ReferenceFromOrient', @TSepiImportsTDockTree.ReferenceFromOrient,
      'function(const Orient: TDockOrientation): Integer',
      mlkVirtual);
    AddMethod('RemoveControl', @TSepiImportsTDockTree.RemoveControl,
      'procedure(Control: TControl)',
      mlkVirtual);
    AddMethod('SaveToStream', @TSepiImportsTDockTree.SaveToStream,
      'procedure(Stream: TStream)',
      mlkVirtual);
    AddMethod('SetReplacingControl', @TSepiImportsTDockTree.SetReplacingControl,
      'procedure(Control: TControl)');
    AddMethod('ShowHint', @TSepiImportsTDockTree.ShowHint,
      'procedure(CursorPos: TPoint; var CursorRect: TRect; var HintStr: string )',
      mlkVirtual);
    AddMethod('ResetBounds', @TSepiImportsTDockTree.ResetBounds,
      'procedure(Force: Boolean)',
      mlkVirtual);
    AddMethod('UpdateAll', @TSepiImportsTDockTree.UpdateAll,
      'procedure');
    AddMethod('WndProc', @TSepiImportsTDockTree.WndProc,
      'procedure(var Message: TMessage)',
      mlkVirtual);
    AddMethod('ZoneCaptionHitTest', @TSepiImportsTDockTree.ZoneCaptionHitTest,
      'function(const Zone: TDockZone; const MousePos: TPoint; var HTFlag: Integer ) : Boolean',
      mlkVirtual);

    AddProperty('DockSite', 'property: TWinControl',
      'FDockSite', 'FDockSite');
    AddProperty('RelativeSizes', 'property: Boolean',
      'FRelativeSizes', 'FRelativeSizes');
    AddProperty('TopZone', 'property: TDockZone',
      'FTopZone', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTDockTree.Create,
      'constructor(DockSite: TWinControl)',
      mlkVirtual);
    AddMethod('Destroy', @TSepiImportsTDockTree.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('PaintSite', @TSepiImportsTDockTree.PaintSite,
      'procedure(DC: HDC)',
      mlkVirtual);

    Complete;
  end;
end;

{---------------}
{ TMouse import }
{---------------}

function TSepiImportsTMouse.GetCursorPos: TPoint;
begin
  Result := CursorPos;
end;

procedure TSepiImportsTMouse.SetCursorPos(const Value: TPoint);
begin
  CursorPos := Value;
end;

function TSepiImportsTMouse.GetCapture: HWND;
begin
  Result := Capture;
end;

procedure TSepiImportsTMouse.SetCapture(const Value: HWND);
begin
  Capture := Value;
end;

function TSepiImportsTMouse.GetIsDragging: Boolean;
begin
  Result := IsDragging;
end;

class function TSepiImportsTMouse.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMouse));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDragImmediate', System.TypeInfo(Boolean));
    AddField('FDragThreshold', System.TypeInfo(Integer));
    AddField('FMousePresent', System.TypeInfo(Boolean));
    AddField('FNativeWheelSupport', System.TypeInfo(Boolean));
    AddField('FScrollLines', System.TypeInfo(Integer));
    AddField('FScrollLinesMessage', System.TypeInfo(UINT));
    AddField('FWheelHwnd', System.TypeInfo(HWND));
    AddField('FWheelMessage', System.TypeInfo(UINT));
    AddField('FWheelPresent', System.TypeInfo(Boolean));
    AddField('FWheelSupportMessage', System.TypeInfo(UINT));

    AddMethod('GetMouseData', nil,
      'procedure');
    AddMethod('GetNativeData', nil,
      'procedure');
    AddMethod('GetRegisteredData', nil,
      'procedure');
    AddMethod('GetCursorPos', @TSepiImportsTMouse.GetCursorPos,
      'function: TPoint');
    AddMethod('SetCursorPos', @TSepiImportsTMouse.SetCursorPos,
      'procedure(const Value: TPoint)');
    AddMethod('GetCapture', @TSepiImportsTMouse.GetCapture,
      'function: HWND');
    AddMethod('SetCapture', @TSepiImportsTMouse.SetCapture,
      'procedure(const Value: HWND)');
    AddMethod('GetIsDragging', @TSepiImportsTMouse.GetIsDragging,
      'function: Boolean');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMouse.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTMouse.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('SettingChanged', @TSepiImportsTMouse.SettingChanged,
      'procedure(Setting: Integer)');

    AddProperty('Capture', 'property: HWND',
      'GetCapture', 'SetCapture');
    AddProperty('CursorPos', 'property: TPoint',
      'GetCursorPos', 'SetCursorPos');
    AddProperty('DragImmediate', 'property: Boolean',
      'FDragImmediate', 'FDragImmediate',
      NoIndex, Integer(True));
    AddProperty('DragThreshold', 'property: Integer',
      'FDragThreshold', 'FDragThreshold',
      NoIndex, 5);
    AddProperty('MousePresent', 'property: Boolean',
      'FMousePresent', '');
    AddProperty('IsDragging', 'property: Boolean',
      'GetIsDragging', '');
    AddProperty('RegWheelMessage', 'property: UINT',
      'FWheelMessage', '');
    AddProperty('WheelPresent', 'property: Boolean',
      'FWheelPresent', '');
    AddProperty('WheelScrollLines', 'property: Integer',
      'FScrollLines', '');

    Complete;
  end;
end;

{---------------------------}
{ TCustomListControl import }
{---------------------------}

class function TSepiImportsTCustomListControl.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomListControl));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('GetCount', nil,
      'function: Integer',
      mlkVirtual, True);
    AddMethod('GetItemIndex', nil,
      'function: Integer',
      mlkVirtual, True);
    AddMethod('SetItemIndex', nil,
      'procedure(const Value: Integer)',
      mlkVirtual, True);

    CurrentVisibility := mvPublic;

    AddMethod('AddItem', nil,
      'procedure(Item: String; AObject: TObject)',
      mlkVirtual, True);
    AddMethod('Clear', nil,
      'procedure',
      mlkVirtual, True);
    AddMethod('ClearSelection', nil,
      'procedure',
      mlkVirtual, True);
    AddMethod('CopySelection', nil,
      'procedure(Destination: TCustomListControl)',
      mlkVirtual, True);
    AddMethod('DeleteSelected', nil,
      'procedure',
      mlkVirtual, True);
    AddMethod('MoveSelection', @TSepiImportsTCustomListControl.MoveSelection,
      'procedure(Destination: TCustomListControl)',
      mlkVirtual);
    AddMethod('SelectAll', nil,
      'procedure',
      mlkVirtual, True);

    AddProperty('ItemIndex', 'property: Integer',
      'GetItemIndex', 'SetItemIndex');

    Complete;
  end;
end;

{--------------------------------------}
{ TCustomMultiSelectListControl import }
{--------------------------------------}

class function TSepiImportsTCustomMultiSelectListControl.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomMultiSelectListControl));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FMultiSelect', System.TypeInfo(Boolean));

    AddMethod('GetSelCount', nil,
      'function: Integer',
      mlkVirtual, True);
    AddMethod('SetMultiSelect', nil,
      'procedure(Value: Boolean)',
      mlkVirtual, True);

    CurrentVisibility := mvPublic;

    AddProperty('MultiSelect', 'property: Boolean',
      'FMultiSelect', 'SetMultiSelect',
      NoIndex, Integer(False));
    AddProperty('SelCount', 'property: Integer',
      'GetSelCount', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'Controls',
    ['Messages', 'Types', 'Windows', 'Classes', 'SysUtils', 'Graphics', 'Menus',
     'ImmTypes', 'ImgList', 'ActnList']);

  // Constants
  TSepiConstant.Create(Result, 'CM_BASE', CM_BASE);
  TSepiConstant.Create(Result, 'CM_ACTIVATE', CM_ACTIVATE);
  TSepiConstant.Create(Result, 'CM_DEACTIVATE', CM_DEACTIVATE);
  TSepiConstant.Create(Result, 'CM_GOTFOCUS', CM_GOTFOCUS);
  TSepiConstant.Create(Result, 'CM_LOSTFOCUS', CM_LOSTFOCUS);
  TSepiConstant.Create(Result, 'CM_CANCELMODE', CM_CANCELMODE);
  TSepiConstant.Create(Result, 'CM_DIALOGKEY', CM_DIALOGKEY);
  TSepiConstant.Create(Result, 'CM_DIALOGCHAR', CM_DIALOGCHAR);
  TSepiConstant.Create(Result, 'CM_FOCUSCHANGED', CM_FOCUSCHANGED);
  TSepiConstant.Create(Result, 'CM_PARENTFONTCHANGED', CM_PARENTFONTCHANGED);
  TSepiConstant.Create(Result, 'CM_PARENTCOLORCHANGED', CM_PARENTCOLORCHANGED);
  TSepiConstant.Create(Result, 'CM_HITTEST', CM_HITTEST);
  TSepiConstant.Create(Result, 'CM_VISIBLECHANGED', CM_VISIBLECHANGED);
  TSepiConstant.Create(Result, 'CM_ENABLEDCHANGED', CM_ENABLEDCHANGED);
  TSepiConstant.Create(Result, 'CM_COLORCHANGED', CM_COLORCHANGED);
  TSepiConstant.Create(Result, 'CM_FONTCHANGED', CM_FONTCHANGED);
  TSepiConstant.Create(Result, 'CM_CURSORCHANGED', CM_CURSORCHANGED);
  TSepiConstant.Create(Result, 'CM_CTL3DCHANGED', CM_CTL3DCHANGED);
  TSepiConstant.Create(Result, 'CM_PARENTCTL3DCHANGED', CM_PARENTCTL3DCHANGED);
  TSepiConstant.Create(Result, 'CM_TEXTCHANGED', CM_TEXTCHANGED);
  TSepiConstant.Create(Result, 'CM_MOUSEENTER', CM_MOUSEENTER);
  TSepiConstant.Create(Result, 'CM_MOUSELEAVE', CM_MOUSELEAVE);
  TSepiConstant.Create(Result, 'CM_MENUCHANGED', CM_MENUCHANGED);
  TSepiConstant.Create(Result, 'CM_APPKEYDOWN', CM_APPKEYDOWN);
  TSepiConstant.Create(Result, 'CM_APPSYSCOMMAND', CM_APPSYSCOMMAND);
  TSepiConstant.Create(Result, 'CM_BUTTONPRESSED', CM_BUTTONPRESSED);
  TSepiConstant.Create(Result, 'CM_SHOWINGCHANGED', CM_SHOWINGCHANGED);
  TSepiConstant.Create(Result, 'CM_ENTER', CM_ENTER);
  TSepiConstant.Create(Result, 'CM_EXIT', CM_EXIT);
  TSepiConstant.Create(Result, 'CM_DESIGNHITTEST', CM_DESIGNHITTEST);
  TSepiConstant.Create(Result, 'CM_ICONCHANGED', CM_ICONCHANGED);
  TSepiConstant.Create(Result, 'CM_WANTSPECIALKEY', CM_WANTSPECIALKEY);
  TSepiConstant.Create(Result, 'CM_INVOKEHELP', CM_INVOKEHELP);
  TSepiConstant.Create(Result, 'CM_WINDOWHOOK', CM_WINDOWHOOK);
  TSepiConstant.Create(Result, 'CM_RELEASE', CM_RELEASE);
  TSepiConstant.Create(Result, 'CM_SHOWHINTCHANGED', CM_SHOWHINTCHANGED);
  TSepiConstant.Create(Result, 'CM_PARENTSHOWHINTCHANGED', CM_PARENTSHOWHINTCHANGED);
  TSepiConstant.Create(Result, 'CM_SYSCOLORCHANGE', CM_SYSCOLORCHANGE);
  TSepiConstant.Create(Result, 'CM_WININICHANGE', CM_WININICHANGE);
  TSepiConstant.Create(Result, 'CM_FONTCHANGE', CM_FONTCHANGE);
  TSepiConstant.Create(Result, 'CM_TIMECHANGE', CM_TIMECHANGE);
  TSepiConstant.Create(Result, 'CM_TABSTOPCHANGED', CM_TABSTOPCHANGED);
  TSepiConstant.Create(Result, 'CM_UIACTIVATE', CM_UIACTIVATE);
  TSepiConstant.Create(Result, 'CM_UIDEACTIVATE', CM_UIDEACTIVATE);
  TSepiConstant.Create(Result, 'CM_DOCWINDOWACTIVATE', CM_DOCWINDOWACTIVATE);
  TSepiConstant.Create(Result, 'CM_CONTROLLISTCHANGE', CM_CONTROLLISTCHANGE);
  TSepiConstant.Create(Result, 'CM_GETDATALINK', CM_GETDATALINK);
  TSepiConstant.Create(Result, 'CM_CHILDKEY', CM_CHILDKEY);
  TSepiConstant.Create(Result, 'CM_DRAG', CM_DRAG);
  TSepiConstant.Create(Result, 'CM_HINTSHOW', CM_HINTSHOW);
  TSepiConstant.Create(Result, 'CM_DIALOGHANDLE', CM_DIALOGHANDLE);
  TSepiConstant.Create(Result, 'CM_ISTOOLCONTROL', CM_ISTOOLCONTROL);
  TSepiConstant.Create(Result, 'CM_RECREATEWND', CM_RECREATEWND);
  TSepiConstant.Create(Result, 'CM_INVALIDATE', CM_INVALIDATE);
  TSepiConstant.Create(Result, 'CM_SYSFONTCHANGED', CM_SYSFONTCHANGED);
  TSepiConstant.Create(Result, 'CM_CONTROLCHANGE', CM_CONTROLCHANGE);
  TSepiConstant.Create(Result, 'CM_CHANGED', CM_CHANGED);
  TSepiConstant.Create(Result, 'CM_DOCKCLIENT', CM_DOCKCLIENT);
  TSepiConstant.Create(Result, 'CM_UNDOCKCLIENT', CM_UNDOCKCLIENT);
  TSepiConstant.Create(Result, 'CM_FLOAT', CM_FLOAT);
  TSepiConstant.Create(Result, 'CM_BORDERCHANGED', CM_BORDERCHANGED);
  TSepiConstant.Create(Result, 'CM_BIDIMODECHANGED', CM_BIDIMODECHANGED);
  TSepiConstant.Create(Result, 'CM_PARENTBIDIMODECHANGED', CM_PARENTBIDIMODECHANGED);
  TSepiConstant.Create(Result, 'CM_ALLCHILDRENFLIPPED', CM_ALLCHILDRENFLIPPED);
  TSepiConstant.Create(Result, 'CM_ACTIONUPDATE', CM_ACTIONUPDATE);
  TSepiConstant.Create(Result, 'CM_ACTIONEXECUTE', CM_ACTIONEXECUTE);
  TSepiConstant.Create(Result, 'CM_HINTSHOWPAUSE', CM_HINTSHOWPAUSE);
  TSepiConstant.Create(Result, 'CM_DOCKNOTIFICATION', CM_DOCKNOTIFICATION);
  TSepiConstant.Create(Result, 'CM_MOUSEWHEEL', CM_MOUSEWHEEL);
  TSepiConstant.Create(Result, 'CM_ISSHORTCUT', CM_ISSHORTCUT);
  TSepiConstant.Create(Result, 'CM_INVALIDATEDOCKHOST', CM_INVALIDATEDOCKHOST);
  TSepiConstant.Create(Result, 'CM_SETACTIVECONTROL', CM_SETACTIVECONTROL);
  TSepiConstant.Create(Result, 'CM_POPUPHWNDDESTROY', CM_POPUPHWNDDESTROY);
  TSepiConstant.Create(Result, 'CM_CREATEPOPUP', CM_CREATEPOPUP);
  TSepiConstant.Create(Result, 'CM_DESTROYHANDLE', CM_DESTROYHANDLE);
  TSepiConstant.Create(Result, 'CM_MOUSEACTIVATE', CM_MOUSEACTIVATE);

  // Constants
  TSepiConstant.Create(Result, 'CN_BASE', CN_BASE);
  TSepiConstant.Create(Result, 'CN_CHARTOITEM', CN_CHARTOITEM);
  TSepiConstant.Create(Result, 'CN_COMMAND', CN_COMMAND);
  TSepiConstant.Create(Result, 'CN_COMPAREITEM', CN_COMPAREITEM);
  TSepiConstant.Create(Result, 'CN_CTLCOLORBTN', CN_CTLCOLORBTN);
  TSepiConstant.Create(Result, 'CN_CTLCOLORDLG', CN_CTLCOLORDLG);
  TSepiConstant.Create(Result, 'CN_CTLCOLOREDIT', CN_CTLCOLOREDIT);
  TSepiConstant.Create(Result, 'CN_CTLCOLORLISTBOX', CN_CTLCOLORLISTBOX);
  TSepiConstant.Create(Result, 'CN_CTLCOLORMSGBOX', CN_CTLCOLORMSGBOX);
  TSepiConstant.Create(Result, 'CN_CTLCOLORSCROLLBAR', CN_CTLCOLORSCROLLBAR);
  TSepiConstant.Create(Result, 'CN_CTLCOLORSTATIC', CN_CTLCOLORSTATIC);
  TSepiConstant.Create(Result, 'CN_DELETEITEM', CN_DELETEITEM);
  TSepiConstant.Create(Result, 'CN_DRAWITEM', CN_DRAWITEM);
  TSepiConstant.Create(Result, 'CN_HSCROLL', CN_HSCROLL);
  TSepiConstant.Create(Result, 'CN_MEASUREITEM', CN_MEASUREITEM);
  TSepiConstant.Create(Result, 'CN_PARENTNOTIFY', CN_PARENTNOTIFY);
  TSepiConstant.Create(Result, 'CN_VKEYTOITEM', CN_VKEYTOITEM);
  TSepiConstant.Create(Result, 'CN_VSCROLL', CN_VSCROLL);
  TSepiConstant.Create(Result, 'CN_KEYDOWN', CN_KEYDOWN);
  TSepiConstant.Create(Result, 'CN_KEYUP', CN_KEYUP);
  TSepiConstant.Create(Result, 'CN_CHAR', CN_CHAR);
  TSepiConstant.Create(Result, 'CN_SYSKEYDOWN', CN_SYSKEYDOWN);
  TSepiConstant.Create(Result, 'CN_SYSCHAR', CN_SYSCHAR);
  TSepiConstant.Create(Result, 'CN_NOTIFY', CN_NOTIFY);

  // Constants
  TSepiConstant.Create(Result, 'mrNone', mrNone);
  TSepiConstant.Create(Result, 'mrOk', mrOk);
  TSepiConstant.Create(Result, 'mrCancel', mrCancel);
  TSepiConstant.Create(Result, 'mrAbort', mrAbort);
  TSepiConstant.Create(Result, 'mrRetry', mrRetry);
  TSepiConstant.Create(Result, 'mrIgnore', mrIgnore);
  TSepiConstant.Create(Result, 'mrYes', mrYes);
  TSepiConstant.Create(Result, 'mrNo', mrNo);
  TSepiConstant.Create(Result, 'mrAll', mrAll);
  TSepiConstant.Create(Result, 'mrNoToAll', mrNoToAll);
  TSepiConstant.Create(Result, 'mrYesToAll', mrYesToAll);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TModalResult));

  // Routines
  TSepiMetaMethod.Create(Result, 'IsPositiveResult', @IsPositiveResult,
    'function(const AModalResult: TModalResult): Boolean');
  TSepiMetaMethod.Create(Result, 'IsNegativeResult', @IsNegativeResult,
    'function(const AModalResult: TModalResult): Boolean');
  TSepiMetaMethod.Create(Result, 'IsAbortResult', @IsAbortResult,
    'function(const AModalResult: TModalResult): Boolean');
  TSepiMetaMethod.Create(Result, 'IsAnAllResult', @IsAnAllResult,
    'function(const AModalResult: TModalResult): Boolean');
  TSepiMetaMethod.Create(Result, 'StripAllFromResult', @StripAllFromResult,
    'function(const AModalResult: TModalResult): TModalResult');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCursor));

  // Constants
  TSepiConstant.Create(Result, 'crDefault', crDefault);
  TSepiConstant.Create(Result, 'crNone', crNone);
  TSepiConstant.Create(Result, 'crArrow', crArrow);
  TSepiConstant.Create(Result, 'crCross', crCross);
  TSepiConstant.Create(Result, 'crIBeam', crIBeam);
  TSepiConstant.Create(Result, 'crSize', crSize);
  TSepiConstant.Create(Result, 'crSizeNESW', crSizeNESW);
  TSepiConstant.Create(Result, 'crSizeNS', crSizeNS);
  TSepiConstant.Create(Result, 'crSizeNWSE', crSizeNWSE);
  TSepiConstant.Create(Result, 'crSizeWE', crSizeWE);
  TSepiConstant.Create(Result, 'crUpArrow', crUpArrow);
  TSepiConstant.Create(Result, 'crHourGlass', crHourGlass);
  TSepiConstant.Create(Result, 'crDrag', crDrag);
  TSepiConstant.Create(Result, 'crNoDrop', crNoDrop);
  TSepiConstant.Create(Result, 'crHSplit', crHSplit);
  TSepiConstant.Create(Result, 'crVSplit', crVSplit);
  TSepiConstant.Create(Result, 'crMultiDrag', crMultiDrag);
  TSepiConstant.Create(Result, 'crSQLWait', crSQLWait);
  TSepiConstant.Create(Result, 'crNo', crNo);
  TSepiConstant.Create(Result, 'crAppStart', crAppStart);
  TSepiConstant.Create(Result, 'crHelp', crHelp);
  TSepiConstant.Create(Result, 'crHandPoint', crHandPoint);
  TSepiConstant.Create(Result, 'crSizeAll', crSizeAll);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TDragObject));
  TSepiClass.ForwardDecl(Result, TypeInfo(TControl));
  TSepiClass.ForwardDecl(Result, TypeInfo(TWinControl));
  TSepiClass.ForwardDecl(Result, TypeInfo(TDragImageList));
  TSepiMetaClass.Create(Result, 'TWinControlClass', TypeInfo(TWinControl), True);
  TSepiTypeAlias.Create(Result, 'TCMActivate', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TCMDeactivate', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TCMGotFocus', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TCMLostFocus', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TCMDialogKey', 'TWMKey');
  TSepiTypeAlias.Create(Result, 'TCMDialogChar', 'TWMKey');
  TSepiTypeAlias.Create(Result, 'TCMHitTest', 'TWMNCHitTest');
  TSepiTypeAlias.Create(Result, 'TCMEnter', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TCMExit', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TCMDesignHitTest', 'TWMMouse');
  TSepiTypeAlias.Create(Result, 'TCMWantSpecialKey', 'TWMKey');
  SepiImportTCMMouseWheel(Result);
  SepiImportTCMCancelMode(Result);
  SepiImportTCMFocusChanged(Result);
  SepiImportTCMControlListChange(Result);
  SepiImportTCMChildKey(Result);
  SepiImportTCMControlChange(Result);
  SepiImportTCMChanged(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDragMessage));
  TSepiPointerType.Create(Result, 'PDragRec', 'TDragRec', True);
  SepiImportTDragRec(Result);
  SepiImportTCMDrag(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TDragDockObject));
  SepiImportTCMDockClient(Result);
  SepiImportTCMUnDockClient(Result);
  SepiImportTCMFloat(Result);
  TSepiPointerType.Create(Result, 'PDockNotifyRec', 'TDockNotifyRec', True);
  SepiImportTDockNotifyRec(Result);
  SepiImportTCMDockNotification(Result);
  TSepiPointerType.Create(Result, 'PPopupFormInfo', 'TPopupFormInfo', True);
  SepiImportTPopupFormInfo(Result);
  SepiImportTCMPopupHWndDestroy(Result);
  SepiImportTCMCreatePopup(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAlign));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAlignSet));
  TSepiImportsTDragObject.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TDragObjectClass', TypeInfo(TDragObject), True);
  TSepiImportsTDragObjectEx.SepiImport(Result);
  TSepiImportsTBaseDragControlObject.SepiImport(Result);
  TSepiImportsTDragControlObject.SepiImport(Result);
  TSepiImportsTDragControlObjectEx.SepiImport(Result);
  TSepiImportsTDragDockObject.SepiImport(Result);
  TSepiImportsTDragDockObjectEx.SepiImport(Result);
  TSepiImportsTControlCanvas.SepiImport(Result);
  TSepiImportsTCustomControlAction.SepiImport(Result);
  TSepiImportsTControlAction.SepiImport(Result);
  TSepiImportsTControlActionLink.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TControlActionLinkClass', TypeInfo(TControlActionLink), True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TControlState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TControlStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMouseButton));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMouseActivate));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDragMode));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDragState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDragKind));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTabOrder));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCaption));
  TSepiPointerType.Create(Result, 'PMouseActivateRec', 'TMouseActivateRec', True);
  SepiImportTMouseActivateRec(Result);
  SepiImportTCMMouseActivate(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDate));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTime));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TScalingFlags));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAnchorKind));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAnchors));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConstraintSize));
  TSepiImportsTSizeConstraints.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMouseEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMouseMoveEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMouseActivateEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TKeyEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TKeyPressEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDragOverEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDragDropEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStartDragEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEndDragEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDockDropEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDockOverEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnDockEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStartDockEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetSiteInfoEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCanResizeEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConstrainedResizeEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMouseWheelEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMouseWheelUpDownEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TContextPopupEvent));
  TSepiTypeAlias.Create(Result, 'TWndMethod', TypeInfo(TWndMethod));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDockOrientation));
  TSepiImportsTControl.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TControlClass', TypeInfo(TControl), True);
  TSepiArrayType.Create(Result, '$1',
    [0, 63], TypeInfo(Char), True);
  SepiImportTCreateParams(Result);
  TSepiImportsTWinControlActionLink.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TWinControlActionLinkClass', TypeInfo(TWinControlActionLink), True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TImeMode));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TImeName));
  SepiImportTAlignInfo(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBorderWidth));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBevelCut));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBevelEdge));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBevelEdges));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBevelKind));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBevelWidth));
  SepiImportIDockManager(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAlignInsertBeforeEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAlignPositionEvent));
  TSepiImportsTWinControl.SepiImport(Result);
  TSepiImportsTGraphicControl.SepiImport(Result);
  TSepiImportsTCustomControl.SepiImport(Result);
  TSepiImportsTHintWindow.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'THintWindowClass', TypeInfo(THintWindow), True);
  TSepiImportsTDragImageList.SepiImport(Result);
  TSepiImportsTImageList.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TDockTree));
  TSepiImportsTDockZone.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TForEachZoneProc));
  TSepiMetaClass.Create(Result, 'TDockTreeClass', TypeInfo(TDockTree), True);
  TSepiImportsTDockTree.SepiImport(Result);
  TSepiImportsTMouse.SepiImport(Result);
  TSepiImportsTCustomListControl.SepiImport(Result);
  TSepiImportsTCustomMultiSelectListControl.SepiImport(Result);
  TSepiMethodRefType.Create(Result, 'TAnimateWindowProc',
    'function(hWnd: HWND; dwTime: DWORD; dwFlags: DWORD): BOOL', False, ccStdCall);

  // Global variables
  TSepiVariable.Create(Result, 'Mouse',
    Mouse, TypeInfo(TMouse));
  TSepiVariable.Create(Result, 'AnimateWindowProc',
    @AnimateWindowProc, 'TAnimateWindowProc');

  // Routines
  TSepiMetaMethod.Create(Result, 'IsDragObject', @IsDragObject,
    'function(Sender: TObject): Boolean');
  TSepiMetaMethod.Create(Result, 'FindControl', @FindControl,
    'function(Handle: HWnd): TWinControl');
  TSepiMetaMethod.Create(Result, 'FindVCLWindow', @FindVCLWindow,
    'function(const Pos: TPoint): TWinControl');
  TSepiMetaMethod.Create(Result, 'FindDragTarget', @FindDragTarget,
    'function(const Pos: TPoint; AllowDisabled: Boolean): TControl');
  TSepiMetaMethod.Create(Result, 'GetCaptureControl', @GetCaptureControl,
    'function: TControl');
  TSepiMetaMethod.Create(Result, 'SetCaptureControl', @SetCaptureControl,
    'procedure(Control: TControl)');
  TSepiMetaMethod.Create(Result, 'CancelDrag', @CancelDrag,
    'procedure');
  TSepiMetaMethod.Create(Result, 'CursorToString', @CursorToString,
    'function(Cursor: TCursor): string');
  TSepiMetaMethod.Create(Result, 'StringToCursor', @StringToCursor,
    'function(const S: string): TCursor');
  TSepiMetaMethod.Create(Result, 'GetCursorValues', @GetCursorValues,
    'procedure(Proc: TGetStrProc)');
  TSepiMetaMethod.Create(Result, 'CursorToIdent', @CursorToIdent,
    'function(Cursor: Longint; var Ident: string): Boolean');
  TSepiMetaMethod.Create(Result, 'IdentToCursor', @IdentToCursor,
    'function(const Ident: string; var Cursor: Longint): Boolean');
  TSepiMetaMethod.Create(Result, 'GetShortHint', @GetShortHint,
    'function(const Hint: string): string');
  TSepiMetaMethod.Create(Result, 'GetLongHint', @GetLongHint,
    'function(const Hint: string): string');
  TSepiMetaMethod.Create(Result, 'PerformEraseBackground', @PerformEraseBackground,
    'procedure(Control: TControl; DC: HDC)');

  // Global variables
  TSepiVariable.Create(Result, 'CreationControl',
    CreationControl, TypeInfo(TWinControl));
  TSepiVariable.Create(Result, 'DefaultDockTreeClass',
    DefaultDockTreeClass, 'TDockTreeClass');

  // Routines
  TSepiMetaMethod.Create(Result, 'InitWndProc', @InitWndProc,
    'function(HWindow: HWnd; Message, WParam: Longint; LParam: Longint ) : Longint');

  // Constants
  TSepiConstant.Create(Result, 'CTL3D_ALL', CTL3D_ALL);
  TSepiConstant.Create(Result, 'NullDockSite', LongWord(NullDockSite), TypeInfo(TWinControl));
  TSepiArrayType.Create(Result, '$2',
    [Integer(Low(TAlign)), Integer(High(TAlign))], TypeInfo(TAnchors), True);
  TSepiVariable.Create(Result, 'AnchorAlign',
    AnchorAlign, '$2', True);

  // Global variables
  TSepiVariable.Create(Result, 'NewStyleControls',
    NewStyleControls, TypeInfo(Boolean));

  // Routines
  TSepiMetaMethod.Create(Result, 'ChangeBiDiModeAlignment', @ChangeBiDiModeAlignment,
    'procedure(var Alignment: TAlignment)');
  TSepiMetaMethod.Create(Result, 'SendAppMessage', @SendAppMessage,
    'function(Msg: Cardinal; WParam, LParam: Longint): Longint');
  TSepiMetaMethod.Create(Result, 'MoveWindowOrg', @MoveWindowOrg,
    'procedure(DC: HDC; DX, DY: Integer)');
  TSepiMetaMethod.Create(Result, 'SetImeMode', @SetImeMode,
    'procedure(hWnd: HWND; Mode: TImeMode)');
  TSepiMetaMethod.Create(Result, 'SetImeName', @SetImeName,
    'procedure(Name: TImeName)');
  TSepiMetaMethod.Create(Result, 'Win32NLSEnableIME', @Win32NLSEnableIME,
    'function(hWnd: HWND; Enable: Boolean): Boolean');
  TSepiMetaMethod.Create(Result, 'Imm32GetContext', @Imm32GetContext,
    'function(hWnd: HWND): HIMC');
  TSepiMetaMethod.Create(Result, 'Imm32ReleaseContext', @Imm32ReleaseContext,
    'function(hWnd: HWND; hImc: HIMC): Boolean');
  TSepiMetaMethod.Create(Result, 'Imm32GetConversionStatus', @Imm32GetConversionStatus,
    'function(hImc: HIMC; var Conversion, Sentence: DWORD): Boolean');
  TSepiMetaMethod.Create(Result, 'Imm32SetConversionStatus', @Imm32SetConversionStatus,
    'function(hImc: HIMC; Conversion, Sentence: DWORD): Boolean');
  TSepiMetaMethod.Create(Result, 'Imm32SetOpenStatus', @Imm32SetOpenStatus,
    'function(hImc: HIMC; fOpen: Boolean): Boolean');
  TSepiMetaMethod.Create(Result, 'Imm32SetCompositionWindow', @Imm32SetCompositionWindow,
    'function(hImc: HIMC; lpCompForm: PCOMPOSITIONFORM): Boolean');
  TSepiMetaMethod.Create(Result, 'Imm32SetCompositionFont', @Imm32SetCompositionFont,
    'function(hImc: HIMC; lpLogfont: PLOGFONTA): Boolean');
  TSepiMetaMethod.Create(Result, 'Imm32GetCompositionString', @Imm32GetCompositionString,
    'function(hImc: HIMC; dWord1: DWORD; lpBuf: pointer; dwBufLen: DWORD): Longint');
  TSepiMetaMethod.Create(Result, 'Imm32IsIME', @Imm32IsIME,
    'function(hKl: HKL): Boolean');
  TSepiMetaMethod.Create(Result, 'Imm32NotifyIME', @Imm32NotifyIME,
    'function(hImc: HIMC; dwAction, dwIndex, dwValue: DWORD): Boolean');
  TSepiMetaMethod.Create(Result, 'DragDone', @DragDone,
    'procedure(Drop: Boolean)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Controls', ImportUnit);
end.

