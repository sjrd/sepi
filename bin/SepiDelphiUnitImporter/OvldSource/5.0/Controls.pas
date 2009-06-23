{*******************************************************}
{                                                       }
{       CodeGear Delphi Visual Component Library        }
{                                                       }
{           Copyright (c) 1995-2007 CodeGear            }
{                                                       }
{*******************************************************}

unit Controls;

{$P+,S-,W-,R-,T-,H+,X+}
{ WARN SYMBOL_PLATFORM OFF}
{$C PRELOAD}

interface

{$R Controls.res}

{ CommCtrl.hpp is not required in Controls.hpp }
(*$NOINCLUDE CommCtrl *)
{$IFDEF LINUX}
uses
  Messages, WinUtils, Windows, Classes, Sysutils,
  Graphics, MultiMon, Menus, CommCtrl, Imm, ImgList, ActnList;
{$ENDIF}
{$IFDEF MSWINDOWS}
uses Messages, Types, Windows, MultiMon, Classes, SysUtils, Graphics, Menus,
  CommCtrl, Imm, ImgList, ActnList;
{$ENDIF}

{ VCL control message IDs }

const
  CM_BASE                   = $B000;
  CM_ACTIVATE               = CM_BASE + 0;
  CM_DEACTIVATE             = CM_BASE + 1;
  CM_GOTFOCUS               = CM_BASE + 2;
  CM_LOSTFOCUS              = CM_BASE + 3;
  CM_CANCELMODE             = CM_BASE + 4;
  CM_DIALOGKEY              = CM_BASE + 5;
  CM_DIALOGCHAR             = CM_BASE + 6;
  CM_FOCUSCHANGED           = CM_BASE + 7;
  CM_PARENTFONTCHANGED      = CM_BASE + 8;
  CM_PARENTCOLORCHANGED     = CM_BASE + 9;
  CM_HITTEST                = CM_BASE + 10;
  CM_VISIBLECHANGED         = CM_BASE + 11;
  CM_ENABLEDCHANGED         = CM_BASE + 12;
  CM_COLORCHANGED           = CM_BASE + 13;
  CM_FONTCHANGED            = CM_BASE + 14;
  CM_CURSORCHANGED          = CM_BASE + 15;
  CM_CTL3DCHANGED           = CM_BASE + 16;
  CM_PARENTCTL3DCHANGED     = CM_BASE + 17;
  CM_TEXTCHANGED            = CM_BASE + 18;
  CM_MOUSEENTER             = CM_BASE + 19;
  CM_MOUSELEAVE             = CM_BASE + 20;
  CM_MENUCHANGED            = CM_BASE + 21;
  CM_APPKEYDOWN             = CM_BASE + 22;
  CM_APPSYSCOMMAND          = CM_BASE + 23;
  CM_BUTTONPRESSED          = CM_BASE + 24;
  CM_SHOWINGCHANGED         = CM_BASE + 25;
  CM_ENTER                  = CM_BASE + 26;
  CM_EXIT                   = CM_BASE + 27;
  CM_DESIGNHITTEST          = CM_BASE + 28;
  CM_ICONCHANGED            = CM_BASE + 29;
  CM_WANTSPECIALKEY         = CM_BASE + 30;
  CM_INVOKEHELP             = CM_BASE + 31;
  CM_WINDOWHOOK             = CM_BASE + 32;
  CM_RELEASE                = CM_BASE + 33;
  CM_SHOWHINTCHANGED        = CM_BASE + 34;
  CM_PARENTSHOWHINTCHANGED  = CM_BASE + 35;
  CM_SYSCOLORCHANGE         = CM_BASE + 36;
  CM_WININICHANGE           = CM_BASE + 37;
  CM_FONTCHANGE             = CM_BASE + 38;
  CM_TIMECHANGE             = CM_BASE + 39;
  CM_TABSTOPCHANGED         = CM_BASE + 40;
  CM_UIACTIVATE             = CM_BASE + 41;
  CM_UIDEACTIVATE           = CM_BASE + 42;
  CM_DOCWINDOWACTIVATE      = CM_BASE + 43;
  CM_CONTROLLISTCHANGE      = CM_BASE + 44;
  CM_GETDATALINK            = CM_BASE + 45;
  CM_CHILDKEY               = CM_BASE + 46;
  CM_DRAG                   = CM_BASE + 47;
  CM_HINTSHOW               = CM_BASE + 48;
  CM_DIALOGHANDLE           = CM_BASE + 49;
  CM_ISTOOLCONTROL          = CM_BASE + 50;
  CM_RECREATEWND            = CM_BASE + 51;
  CM_INVALIDATE             = CM_BASE + 52;
  CM_SYSFONTCHANGED         = CM_BASE + 53;
  CM_CONTROLCHANGE          = CM_BASE + 54;
  CM_CHANGED                = CM_BASE + 55;
  CM_DOCKCLIENT             = CM_BASE + 56;
  CM_UNDOCKCLIENT           = CM_BASE + 57;
  CM_FLOAT                  = CM_BASE + 58;
  CM_BORDERCHANGED          = CM_BASE + 59;
  CM_BIDIMODECHANGED        = CM_BASE + 60;
  CM_PARENTBIDIMODECHANGED  = CM_BASE + 61;
  CM_ALLCHILDRENFLIPPED     = CM_BASE + 62;
  CM_ACTIONUPDATE           = CM_BASE + 63;
  CM_ACTIONEXECUTE          = CM_BASE + 64;
  CM_HINTSHOWPAUSE          = CM_BASE + 65;
  CM_DOCKNOTIFICATION       = CM_BASE + 66;
  CM_MOUSEWHEEL             = CM_BASE + 67;
  CM_ISSHORTCUT             = CM_BASE + 68;
{$IFDEF LINUX}
  CM_RAWX11EVENT            = CM_BASE + 69;
{$ENDIF}
  CM_INVALIDATEDOCKHOST     = CM_BASE + 70;
  CM_SETACTIVECONTROL       = CM_BASE + 71;
  CM_POPUPHWNDDESTROY       = CM_BASE + 72;
  CM_CREATEPOPUP            = CM_BASE + 73;
  CM_DESTROYHANDLE          = CM_BASE + 74;
  CM_MOUSEACTIVATE          = CM_BASE + 75;
  CM_CONTROLLISTCHANGING    = CM_BASE + 76;
  CM_BUFFEREDPRINTCLIENT    = CM_BASE + 77;
  CM_UNTHEMECONTROL         = CM_BASE + 78;

{ VCL control notification IDs }

const
  CN_BASE              = $BC00;
  CN_CHARTOITEM        = CN_BASE + WM_CHARTOITEM;
  CN_COMMAND           = CN_BASE + WM_COMMAND;
  CN_COMPAREITEM       = CN_BASE + WM_COMPAREITEM;
  CN_CTLCOLORBTN       = CN_BASE + WM_CTLCOLORBTN;
  CN_CTLCOLORDLG       = CN_BASE + WM_CTLCOLORDLG;
  CN_CTLCOLOREDIT      = CN_BASE + WM_CTLCOLOREDIT;
  CN_CTLCOLORLISTBOX   = CN_BASE + WM_CTLCOLORLISTBOX;
  CN_CTLCOLORMSGBOX    = CN_BASE + WM_CTLCOLORMSGBOX;
  CN_CTLCOLORSCROLLBAR = CN_BASE + WM_CTLCOLORSCROLLBAR;
  CN_CTLCOLORSTATIC    = CN_BASE + WM_CTLCOLORSTATIC;
  CN_DELETEITEM        = CN_BASE + WM_DELETEITEM;
  CN_DRAWITEM          = CN_BASE + WM_DRAWITEM;
  CN_HSCROLL           = CN_BASE + WM_HSCROLL;
  CN_MEASUREITEM       = CN_BASE + WM_MEASUREITEM;
  CN_PARENTNOTIFY      = CN_BASE + WM_PARENTNOTIFY;
  CN_VKEYTOITEM        = CN_BASE + WM_VKEYTOITEM;
  CN_VSCROLL           = CN_BASE + WM_VSCROLL;
  CN_KEYDOWN           = CN_BASE + WM_KEYDOWN;
  CN_KEYUP             = CN_BASE + WM_KEYUP;
  CN_CHAR              = CN_BASE + WM_CHAR;
  CN_SYSKEYDOWN        = CN_BASE + WM_SYSKEYDOWN;
  CN_SYSCHAR           = CN_BASE + WM_SYSCHAR;
  CN_NOTIFY            = CN_BASE + WM_NOTIFY;

{ TModalResult values }

const
  mrNone     = 0;
  mrOk       = idOk;
  mrCancel   = idCancel;
  mrAbort    = idAbort;
  mrRetry    = idRetry;
  mrIgnore   = idIgnore;
  mrYes      = idYes;
  mrNo       = idNo;
  mrAll      = mrNo + 1;
  mrNoToAll  = mrAll + 1;
  mrYesToAll = mrNoToAll + 1;

type
  TModalResult = Low(Integer)..High(Integer);

function IsPositiveResult(const AModalResult: TModalResult): Boolean;
function IsNegativeResult(const AModalResult: TModalResult): Boolean;
function IsAbortResult(const AModalResult: TModalResult): Boolean;
function IsAnAllResult(const AModalResult: TModalResult): Boolean;
function StripAllFromResult(const AModalResult: TModalResult): TModalResult;

{ Cursor identifiers }

type
  TCursor = -32768..32767;
  {$NODEFINE TCursor}

  (*$HPPEMIT 'namespace Controls'}*)
  (*$HPPEMIT '{'}*)
  (*$HPPEMIT '#pragma option -b-'*)
  (*$HPPEMIT '  enum TCursor {crMin=-32768, crMax=32767};'}*)
  (*$HPPEMIT '#pragma option -b.'*)
  (*$HPPEMIT '}'*)

const
  crDefault     = TCursor(0);
  crNone        = TCursor(-1);
  crArrow       = TCursor(-2);
  crCross       = TCursor(-3);
  crIBeam       = TCursor(-4);
  crSize        = TCursor(-22);
  crSizeNESW    = TCursor(-6);
  crSizeNS      = TCursor(-7);
  crSizeNWSE    = TCursor(-8);
  crSizeWE      = TCursor(-9);
  crUpArrow     = TCursor(-10);
  crHourGlass   = TCursor(-11);
  crDrag        = TCursor(-12);
  crNoDrop      = TCursor(-13);
  crHSplit      = TCursor(-14);
  crVSplit      = TCursor(-15);
  crMultiDrag   = TCursor(-16);
  crSQLWait     = TCursor(-17);
  crNo          = TCursor(-18);
  crAppStart    = TCursor(-19);
  crHelp        = TCursor(-20);
  crHandPoint   = TCursor(-21);
  crSizeAll     = TCursor(-22);

type

{ Forward declarations }

  TDragObject = class;
  TControl = class;
  TWinControl = class;
  TDragImageList = class;

  TWinControlClass = class of TWinControl;

{ VCL control message records }

  TCMActivate = TWMNoParams;
  TCMDeactivate = TWMNoParams;
  TCMGotFocus = TWMNoParams;
  TCMLostFocus = TWMNoParams;
  TCMDialogKey = TWMKey;
  TCMDialogChar = TWMKey;
  TCMHitTest = TWMNCHitTest;
  TCMEnter = TWMNoParams;
  TCMExit = TWMNoParams;
  TCMDesignHitTest = TWMMouse;
  TCMWantSpecialKey = TWMKey;

  TCMMouseWheel = record
    Msg: Cardinal;
    ShiftState: TShiftState;
    Unused: Byte;
    WheelDelta: SmallInt;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TCMCancelMode = record
    Msg: Cardinal;
    Unused: Integer;
    Sender: TControl;
    Result: Longint;
  end;

  TCMFocusChanged = record
    Msg: Cardinal;
    Unused: Integer;
    Sender: TWinControl;
    Result: Longint;
  end;

  TCMControlListChange = record
    Msg: Cardinal;
    Control: TControl;
    Inserting: LongBool;
    Result: Longint;
  end;

  PControlListItem = ^TControlListItem;
  TControlListItem = record
    Control: TControl;
    Parent: TWinControl;
  end;

  TCMControlListChanging = record
    Msg: Cardinal;
    ControlListItem: PControlListItem;
    Inserting: LongBool;
    Result: Longint;
  end;

  TCMChildKey = record
    Msg: Cardinal;
    CharCode: Word;
    Unused: Word;
    Sender: TWinControl;
    Result: Longint;
  end;

  TCMControlChange = record
    Msg: Cardinal;
    Control: TControl;
    Inserting: LongBool;
    Result: Longint;
  end;

  TCMChanged = record
    Msg: Cardinal;
    Unused: Longint;
    Child: TControl;
    Result: Longint;
  end;

  TDragMessage = (dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop, dmDragCancel,
    dmFindTarget);

  PDragRec = ^TDragRec;
  TDragRec = record
    Pos: TPoint;
    Source: TDragObject;
    Target: Pointer;
    Docking: Boolean;
  end;

  TCMDrag = packed record
    Msg: Cardinal;
    DragMessage: TDragMessage;
    Reserved1: Byte;
    Reserved2: Word;
    DragRec: PDragRec;
    Result: Longint;
  end;

  TDragDockObject = class;

  TCMDockClient = packed record
    Msg: Cardinal;
    DockSource: TDragDockObject;
    MousePos: TSmallPoint;
    Result: Integer;
  end;

  TCMUnDockClient = packed record
    Msg: Cardinal;
    NewTarget: TControl;
    Client: TControl;
    Result: Integer;
  end;

  TCMFloat = packed record
    Msg: Cardinal;
    Reserved: Integer;
    DockSource: TDragDockObject;
    Result: Integer;
  end;

  PDockNotifyRec = ^TDockNotifyRec;
  TDockNotifyRec = record
    ClientMsg: Cardinal;
    MsgWParam: Integer;
    MsgLParam: Integer;
  end;

  TCMDockNotification = packed record
    Msg: Cardinal;
    Client: TControl;
    NotifyRec: PDockNotifyRec;
    Result: Integer;
  end;

  PPopupFormInfo = ^TPopupFormInfo;
  TPopupFormInfo = record
    PopupID: Integer;
    PopupWnd: HWND;
    IsPopup: Boolean;
  end;

  TCMPopupHWndDestroy = packed record
    Msg: Cardinal;
    PopupFormInfo: PPopupFormInfo;
    PopupControlWnd: HWND;
    Result: Integer;
  end;

  TCMCreatePopup = packed record
    Msg: Cardinal;
    PopupID: Integer;
    OwnerWnd: HWND;
    Result: Integer;
  end;

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);

  TAlignSet = set of TAlign;

{ Dragging objects }

  TDragObject = class(TObject)
  private
    FAlwaysShowDragImages: Boolean;
    FCancelling: Boolean;
    FDragTarget: Pointer;
    FDragHandle: HWND;
    FDragPos: TPoint;
    FDragTargetPos: TPoint;
    FDropped: Boolean;
    FMouseDeltaX: Double;
    FMouseDeltaY: Double;
    FRightClickCancels: Boolean;
    function Capture: HWND;
    procedure ReleaseCapture(Handle: HWND);
  protected
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; virtual;
    function GetDragImages: TDragImageList; virtual;
    procedure WndProc(var Msg: TMessage); virtual;
    procedure MainWndProc(var Message: TMessage);
  public
    procedure AfterConstruction; override;
    procedure Assign(Source: TDragObject); virtual;
    procedure BeforeDestruction; override;
    function GetName: string; virtual;
    procedure HideDragImage; virtual;
    function Instance: THandle; virtual;
    procedure ShowDragImage; virtual;
    property AlwaysShowDragImages: Boolean read FAlwaysShowDragImages write FAlwaysShowDragImages;
    property Cancelling: Boolean read FCancelling write FCancelling;
    property DragHandle: HWND read FDragHandle write FDragHandle;
    property DragPos: TPoint read FDragPos write FDragPos;
    property DragTargetPos: TPoint read FDragTargetPos write FDragTargetPos;
    property DragTarget: Pointer read FDragTarget write FDragTarget;
    property Dropped: Boolean read FDropped;
    property MouseDeltaX: Double read FMouseDeltaX;
    property MouseDeltaY: Double read FMouseDeltaY;
    property RightClickCancels: Boolean read FRightClickCancels write FRightClickCancels;
  end;

  TDragObjectClass = class of TDragObject;
  
  TDragObjectEx = class(TDragObject)
  public
    procedure BeforeDestruction; override;
  end;

  TBaseDragControlObject = class(TDragObject)
  private
    FControl: TControl;
  protected
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
  public
    constructor Create(AControl: TControl); virtual;
    procedure Assign(Source: TDragObject); override;
    property Control: TControl read FControl write FControl;
  end;

  TDragControlObject = class(TBaseDragControlObject)
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetDragImages: TDragImageList; override;
  public
    procedure HideDragImage; override;
    procedure ShowDragImage; override;
  end;

  TDragControlObjectEx = class(TDragControlObject)
  public
    procedure BeforeDestruction; override;
  end;

  TDragDockObject = class(TBaseDragControlObject)
  private
    FBrush: TBrush;
    FDockRect: TRect;
    FDropAlign: TAlign;
    FDropOnControl: TControl;
    FEraseDockRect: TRect;
    FFloating: Boolean;
    procedure SetBrush(Value: TBrush);
  protected
    procedure AdjustDockRect(ARect: TRect); virtual;
    procedure DrawDragDockImage; virtual;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    procedure EraseDragDockImage; virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetFrameWidth: Integer; virtual;
    function GetEraseWhenMoving: Boolean; virtual;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    procedure Assign(Source: TDragObject); override;
    property Brush: TBrush read FBrush write SetBrush;
    property DockRect: TRect read FDockRect write FDockRect;
    property DropAlign: TAlign read FDropAlign;
    property DropOnControl: TControl read FDropOnControl;
    property EraseDockRect: TRect read FEraseDockRect write FEraseDockRect;
    property EraseWhenMoving: Boolean read GetEraseWhenMoving;
    property Floating: Boolean read FFloating write FFloating;
    property FrameWidth: Integer read GetFrameWidth;
  end;

  TDragDockObjectEx = class(TDragDockObject)
  public
    procedure BeforeDestruction; override;
  end;

{ Controls }

  TControlCanvas = class(TCanvas)
  private
    FControl: TControl;
    FDeviceContext: HDC;
    FWindowHandle: HWnd;
    procedure SetControl(AControl: TControl);
  protected
    procedure CreateHandle; override;
  public
    destructor Destroy; override;
    procedure FreeHandle;
    procedure UpdateTextFlags;
    property Control: TControl read FControl write SetControl;
  end;

{ TControlAction }

  TCustomControlAction = class(TCustomAction)
  private
    FDropdownMenu: TPopupMenu;
    FPopupMenu: TPopupMenu;
    FEnableDropdown: Boolean;
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetEnableDropdown(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
  public
    property DropdownMenu: TPopupMenu read FDropdownMenu write SetDropdownMenu;
    property EnableDropdown: Boolean read FEnableDropdown write SetEnableDropdown default False;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

{ TControlAction }

  TControlAction = class(TCustomControlAction)
  published
    property AutoCheck;
    property Caption;
    property Checked;
    property DropdownMenu;
    property Enabled;
    property EnableDropdown;
    property GroupIndex;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property PopupMenu;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnExecute;
    property OnHint;
    property OnUpdate;
  end;

{ TControlActionLink }

  TControlActionLink = class(TActionLink)
  protected
    FClient: TControl;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsDropdownMenuLinked: Boolean; virtual;
    function IsEnabledLinked: Boolean; override;
    function IsEnableDropdownLinked: Boolean; virtual;
    function IsHelpLinked: Boolean;  override;
    function IsHintLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    function IsPopupMenuLinked: Boolean; virtual;
    function DoShowHint(var HintStr: string): Boolean; virtual;
    procedure SetCaption(const Value: string); override;
    procedure SetDropdownMenu(Value: TPopupMenu); virtual;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetEnableDropdown(Value: Boolean); virtual;
    procedure SetHint(const Value: string); override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHelpKeyword(const Value: string); override;
    procedure SetHelpType(Value: THelpType); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
    procedure SetPopupMenu(Value: TPopupMenu); virtual;
  end;

  TControlActionLinkClass = class of TControlActionLink;

{ TControl }

  TControlState = set of (csLButtonDown, csClicked, csPalette,
    csReadingState, csAlignmentNeeded, csFocusing, csCreating,
    csPaintCopy, csCustomPaint, csDestroyingHandle, csDocking,
    csDesignerHide, csPanning, csRecreating, csAligning);


  { New TControlStyles: csNeedsBorderPaint and csParentBackground.

    These two ControlStyles are only applicable when Themes are Enabled
    in applications on Windows XP. csNeedsBorderPaint causes the
    ThemeServices to paint the border of a control with the current theme.
    csParentBackground causes the parent to draw its background into the
    Control's background; this is useful for controls which need to show their
    parent's theme elements, such as a TPanel or TFrame that appear on a
    TPageControl. TWinControl introduces a protected ParentBackground
    property which includes/excludes the csParentBackground control style.
  }
  TControlStyle = set of (csAcceptsControls, csCaptureMouse,
    csDesignInteractive, csClickEvents, csFramed, csSetCaption, csOpaque,
    csDoubleClicks, csFixedWidth, csFixedHeight, csNoDesignVisible,
    csReplicatable, csNoStdEvents, csDisplayDragImage, csReflector,
    csActionClient, csMenuEvents, csNeedsBorderPaint, csParentBackground,
    csPannable, csAlignWithMargins);

  TMouseButton = (mbLeft, mbRight, mbMiddle);

  TMouseActivate = (maDefault, maActivate, maActivateAndEat, maNoActivate, maNoActivateAndEat);

  TDragMode = (dmManual, dmAutomatic);

  TDragState = (dsDragEnter, dsDragLeave, dsDragMove);

  TDragKind = (dkDrag, dkDock);

  TTabOrder = -1..32767;

  TCaption = type string;

  PMouseActivateRec = ^TMouseActivateRec;
  TMouseActivateRec = record
    MousePos: TPoint;
    HitTest: Integer;
    Button: TMouseButton;
    ShiftState: TShiftState;
    TopLevel: HWND;
  end;

  TCMMouseActivate = packed record
    Msg: Cardinal;
    Reserved: Integer;
    MouseActivateRec: PMouseActivateRec;
    Result: Integer;
  end;

  TDate = type TDateTime;

  TTime = type TDateTime;
  {$EXTERNALSYM TDate}
  {$EXTERNALSYM TTime}
  (*$HPPEMIT 'namespace Controls'*)
  (*$HPPEMIT '{'*)
  (*$HPPEMIT '    typedef System::TDateTime TDate;'*)
  (*$HPPEMIT '    typedef System::TDateTime TTime;'*)
  (*$HPPEMIT '}'*)


  TScalingFlags = set of (sfLeft, sfTop, sfWidth, sfHeight, sfFont,
    sfDesignSize);

  TAnchorKind = (akLeft, akTop, akRight, akBottom);
  TAnchors = set of TAnchorKind;

  TConstraintSize = 0..MaxInt;

  TSizeConstraints = class(TPersistent)
  private
    FControl: TControl;
    FMaxHeight: TConstraintSize;
    FMaxWidth: TConstraintSize;
    FMinHeight: TConstraintSize;
    FMinWidth: TConstraintSize;
    FOnChange: TNotifyEvent;
    procedure SetConstraints(Index: Integer; Value: TConstraintSize);
  protected
    procedure Change; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    property Control: TControl read FControl;
  public
    constructor Create(Control: TControl); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property MaxHeight: TConstraintSize index 0 read FMaxHeight write SetConstraints default 0;
    property MaxWidth: TConstraintSize index 1 read FMaxWidth write SetConstraints default 0;
    property MinHeight: TConstraintSize index 2 read FMinHeight write SetConstraints default 0;
    property MinWidth: TConstraintSize index 3 read FMinWidth write SetConstraints default 0;
  end;

  TMarginSize = 0..MaxInt;

  TMargins = class(TPersistent)
  private
    FControl: TControl;
    FLeft, FTop, FRight, FBottom: TMarginSize;
    FOnChange: TNotifyEvent;
    procedure SetMargin(Index: Integer; Value: TMarginSize);
  protected
    procedure Change; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    function GetControlBound(Index: Integer): Integer; virtual;
    class procedure InitDefaults(Margins: TMargins); virtual;
    property Control: TControl read FControl;
  public
    constructor Create(Control: TControl); virtual;
    procedure SetControlBounds(ALeft, ATop, AWidth, AHeight: Integer; Aligning: Boolean = False); overload;
    procedure SetControlBounds(const ARect: TRect; Aligning: Boolean = False); overload;
    procedure SetBounds(ALeft, ATop, ARight, ABottom: Integer);
    property ControlLeft: Integer index 0 read GetControlBound;
    property ControlTop: Integer index 1 read GetControlBound;
    property ControlWidth: Integer index 2 read GetControlBound;
    property ControlHeight: Integer index 3 read GetControlBound;
    property ExplicitLeft: Integer index 4 read GetControlBound;
    property ExplicitTop: Integer index 5 read GetControlBound;
    property ExplicitWidth: Integer index 6 read GetControlBound;
    property ExplicitHeight: Integer index 7 read GetControlBound;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left: TMarginSize index 0 read FLeft write SetMargin default 3;
    property Top: TMarginSize index 1 read FTop write SetMargin default 3;
    property Right: TMarginSize index 2 read FRight write SetMargin default 3;
    property Bottom: TMarginSize index 3 read FBottom write SetMargin default 3;
  end;

  TPadding = class(TMargins)
  protected
    class procedure InitDefaults(Margins: TMargins); override;
  published
    property Left default 0;
    property Top default 0;
    property Right default 0;
    property Bottom default 0;
  end;

  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;
  TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y: Integer) of object;
  TMouseActivateEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer; HitTest: Integer; var MouseActivate: TMouseActivate) of object;
  TKeyEvent = procedure(Sender: TObject; var Key: Word;
    Shift: TShiftState) of object;
  TKeyPressEvent = procedure(Sender: TObject; var Key: Char) of object;
  TDragOverEvent = procedure(Sender, Source: TObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean) of object;
  TDragDropEvent = procedure(Sender, Source: TObject;
    X, Y: Integer) of object;
  TStartDragEvent = procedure(Sender: TObject;
    var DragObject: TDragObject) of object;
  TEndDragEvent = procedure(Sender, Target: TObject;
    X, Y: Integer) of object;
  TDockDropEvent = procedure(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer) of object;
  TDockOverEvent = procedure(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer; State: TDragState; var Accept: Boolean) of object;
  TUnDockEvent = procedure(Sender: TObject; Client: TControl;
    NewTarget: TWinControl; var Allow: Boolean) of object;
  TStartDockEvent = procedure(Sender: TObject;
    var DragObject: TDragDockObject) of object;
  TGetSiteInfoEvent = procedure(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean) of object;
  TCanResizeEvent = procedure(Sender: TObject; var NewWidth, NewHeight: Integer;
    var Resize: Boolean) of object;
  TConstrainedResizeEvent = procedure(Sender: TObject; var MinWidth, MinHeight,
    MaxWidth, MaxHeight: Integer) of object;
  TMouseWheelEvent = procedure(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object;
  TMouseWheelUpDownEvent = procedure(Sender: TObject; Shift: TShiftState;
    MousePos: TPoint; var Handled: Boolean) of object;
  TContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint; var Handled: Boolean) of object;

{$IFDEF LINUX}
  TWndMethod = WinUtils.TWndMethod;
{$ENDIF}
{$IFDEF MSWINDOWS}
  TWndMethod = Classes.TWndMethod;
{$ENDIF}

  {$EXTERNALSYM TWndMethod}

  // TDockOrientation indicates how a zone's child zones are arranged.
  // doNoOrient means a zone contains a TControl and not child zones.
  // doHorizontal means a zone's children are stacked top-to-bottom.
  // doVertical means a zone's children are arranged left-to-right.
  TDockOrientation = (doNoOrient, doHorizontal, doVertical);

  TControl = class(TComponent)
  private
    FParent: TWinControl;
    FWindowProc: TWndMethod;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FControlStyle: TControlStyle;
    FControlState: TControlState;
    FDesktopFont: Boolean;
    FVisible: Boolean;
    FEnabled: Boolean;
    FParentFont: Boolean;
    FParentColor: Boolean;
    FAlign: TAlign;
    FAutoSize: Boolean;
    FDragMode: TDragMode;
    FIsControl: Boolean;
    FBiDiMode: TBiDiMode;
    FParentBiDiMode: Boolean;
    FAnchors: TAnchors;
    FText: PChar;
    FFont: TFont;
    FActionLink: TControlActionLink;
    FColor: TColor;
    FConstraints: TSizeConstraints;
    FMargins: TMargins;
    FCursor: TCursor;
    FDragCursor: TCursor;
    FPopupMenu: TPopupMenu;
    FHint: string;
    FFontHeight: Integer;
    FScalingFlags: TScalingFlags;
    FShowHint: Boolean;
    FParentShowHint: Boolean;
    FDragKind: TDragKind;
    FDockOrientation: TDockOrientation;
    FHostDockSite: TWinControl;
    FWheelAccumulator: Integer;
    FUndockWidth: Integer;
    FUndockHeight: Integer;
    FLRDockWidth: Integer;
    FTBDockHeight: Integer;
    FFloatingDockSiteClass: TWinControlClass;
    FOnCanResize: TCanResizeEvent;
    FOnConstrainedResize: TConstrainedResizeEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnResize: TNotifyEvent;
    FOnStartDock: TStartDockEvent;
    FOnEndDock: TEndDragEvent;
    FOnStartDrag: TStartDragEvent;
    FOnEndDrag: TEndDragEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnContextPopup: TContextPopupEvent;
    FOnMouseActivate: TMouseActivateEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FHelpType: THelpType;
    FHelpKeyword: String;
    FHelpContext: THelpContext;
    procedure CalcDockSizes;
    function CheckNewSize(var NewWidth, NewHeight: Integer): Boolean;
    function CreateFloatingDockSite(Bounds: TRect): TWinControl;
    procedure DoActionChange(Sender: TObject);
    function DoCanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
    function DoCanResize(var NewWidth, NewHeight: Integer): Boolean;
    procedure DoConstraintsChange(Sender: TObject);
    procedure DoConstrainedResize(var NewWidth, NewHeight: Integer);
    procedure DoDragMsg(var DragMsg: TCMDrag);
    procedure DoMouseActivate(var Message: TCMMouseActivate);
    procedure DoMouseDown(var Message: TWMMouse; Button: TMouseButton;
      Shift: TShiftState);
    procedure DoMouseUp(var Message: TWMMouse; Button: TMouseButton);
    procedure DoMarginChange(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    function GetAlignWithMargins: Boolean;
    function GetBoundsRect: TRect;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetLRDockWidth: Integer;
    function GetMouseCapture: Boolean;
    function GetText: TCaption;
    function GetTBDockHeight: Integer;
    function GetUndockWidth: Integer;
    function GetUndockHeight: Integer;
    procedure InvalidateControl(IsVisible, IsOpaque: Boolean);
    function IsAnchorsStored: Boolean;
    function IsBiDiModeStored: Boolean;
    function IsCaptionStored: Boolean;
    function IsColorStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsFontStored: Boolean;
    function IsHintStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsOnClickStored: Boolean;
    function IsShowHintStored: Boolean;
    function IsVisibleStored: Boolean;
    procedure ReadIsControl(Reader: TReader);
    procedure ReadExplicitLeft(Reader: TReader);
    procedure ReadExplicitTop(Reader: TReader);
    procedure ReadExplicitWidth(Reader: TReader);
    procedure ReadExplicitHeight(Reader: TReader);
    procedure SetAlignWithMargins(Value: Boolean);
    procedure SetAnchors(Value: TAnchors);
    procedure SetAction(Value: TBasicAction);
    procedure SetAlign(Value: TAlign);
    procedure SetBoundsRect(const Rect: TRect);
    procedure SetClientHeight(Value: Integer);
    procedure SetClientSize(Value: TPoint);
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
    procedure SetZOrderPosition(Position: Integer);
    procedure UpdateAnchorRules;
    procedure WriteIsControl(Writer: TWriter);
    procedure WriteExplicitLeft(Writer: TWriter);
    procedure WriteExplicitTop(Writer: TWriter);
    procedure WriteExplicitWidth(Writer: TWriter);
    procedure WriteExplicitHeight(Writer: TWriter);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk); message WM_MBUTTONDBLCLK;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TWMMButtonUp); message WM_MBUTTONUP;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMMouseActivate(var Message: TCMMouseActivate); message CM_MOUSEACTIVATE;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentShowHintChanged(var Message: TMessage); message CM_PARENTSHOWHINTCHANGED;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMFloat(var Message: TCMFloat); message CM_FLOAT;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMParentBiDiModeChanged(var Message: TMessage); message CM_PARENTBIDIMODECHANGED;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure SetConstraints(const Value: TSizeConstraints);
    procedure SetMargins(const Value: TMargins);
  protected
    FAnchorMove: Boolean;
    FAnchorRules: TPoint;
    FAnchorOrigin: TPoint;
    FOriginalParentSize: TPoint;
    FExplicitLeft: Integer;
    FExplicitTop: Integer;
    FExplicitWidth: Integer;
    FExplicitHeight: Integer;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    procedure AdjustSize; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginAutoDrag; dynamic;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; virtual;
    procedure Changed;
    procedure ChangeScale(M, D: Integer); dynamic;
    procedure Click; dynamic;
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer); virtual;
    function CalcCursorPos: TPoint;
    function DesignWndProc(var Message: TMessage): Boolean; dynamic;
    procedure DblClick; dynamic;
    procedure DefaultDockImage(DragDockObject: TDragDockObject; Erase: Boolean); dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DockTrackNoTarget(Source: TDragDockObject; X, Y: Integer); dynamic;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); dynamic;
    procedure DoEndDock(Target: TObject; X, Y: Integer); dynamic;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); dynamic;
    procedure DoStartDock(var DragObject: TDragObject); dynamic;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; dynamic;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; dynamic;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; dynamic;
    procedure DragCanceled; dynamic;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); dynamic;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); dynamic;
    procedure DoStartDrag(var DragObject: TDragObject); dynamic;
    procedure DrawDragDockImage(DragDockObject: TDragDockObject); dynamic;
    procedure EraseDragDockImage(DragDockObject: TDragDockObject); dynamic;
    function GetAction: TBasicAction; virtual;    
    function GetActionLinkClass: TControlActionLinkClass; dynamic;
    function GetClientOrigin: TPoint; virtual;
    function GetClientRect: TRect; virtual;
    function GetDeviceContext(var WindowHandle: HWnd): HDC; virtual;
    function GetDockEdge(MousePos: TPoint): TAlign; dynamic;
    function GetDragImages: TDragImageList; virtual;
    function GetEnabled: Boolean; virtual;
    function GetFloating: Boolean; virtual;
    function GetFloatingDockSiteClass: TWinControlClass; virtual;
    function GetPalette: HPALETTE; dynamic;
    function GetPopupMenu: TPopupMenu; dynamic;
    procedure Loaded; override;
    function MouseActivate(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; HitTest: Integer): TMouseActivate; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); dynamic;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PositionDockRect(DragDockObject: TDragDockObject); dynamic;
    function PaletteChanged(Foreground: Boolean): Boolean; dynamic;
    procedure ReadState(Reader: TReader); override;
    procedure RequestAlign; virtual;
    procedure Resize; dynamic;
    procedure ScaleConstraints(M, D: Integer);
    procedure SendCancelMode(Sender: TControl);
    procedure SendDockNotification(Msg: Cardinal; WParam, LParam: Integer);
    procedure SetAutoSize(Value: Boolean); virtual;
    procedure SetDragMode(Value: TDragMode); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); virtual;
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetParentBiDiMode(Value: Boolean); virtual;
    procedure SetBiDiMode(Value: TBiDiMode); virtual;
    procedure SetZOrder(TopMost: Boolean); dynamic;
    procedure UpdateExplicitBounds;
    procedure UpdateBoundsRect(const R: TRect);
    procedure VisibleChanging; dynamic;
    procedure WndProc(var Message: TMessage); virtual;
    property ActionLink: TControlActionLink read FActionLink write FActionLink;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property Color: TColor read FColor write SetColor stored IsColorStored default clWindow;
    property DesktopFont: Boolean read FDesktopFont write SetDesktopFont default False;
    property DragKind: TDragKind read FDragKind write FDragKind default dkDrag;
    property DragCursor: TCursor read FDragCursor write FDragCursor default crDrag;
    property DragMode: TDragMode read FDragMode write SetDragMode default dmManual;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property IsControl: Boolean read FIsControl write FIsControl;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property ParentShowHint: Boolean read FParentShowHint write SetParentShowHint default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ScalingFlags: TScalingFlags read FScalingFlags write FScalingFlags;
    property Text: TCaption read GetText write SetText;
    property WheelAccumulator: Integer read FWheelAccumulator write FWheelAccumulator;
    property WindowText: PChar read FText write FText;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnClick: TNotifyEvent read FOnClick write FOnClick stored IsOnClickStored;
    property OnConstrainedResize: TConstrainedResizeEvent read FOnConstrainedResize write FOnConstrainedResize;
    property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnEndDock: TEndDragEvent read FOnEndDock write FOnEndDock;
    property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnMouseActivate: TMouseActivateEvent read FOnMouseActivate write FOnMouseActivate;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown
      write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write
      FOnMouseWheelUp;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnStartDock: TStartDockEvent read FOnStartDock write FOnStartDock;
    property OnStartDrag: TStartDragEvent read FOnStartDrag write FOnStartDrag;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginDrag(Immediate: Boolean; Threshold: Integer = -1);
    procedure BringToFront;
    function ClientToScreen(const Point: TPoint): TPoint;
    function ClientToParent(const Point: TPoint; AParent: TWinControl = nil): TPoint;
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); dynamic;
    procedure DefaultHandler(var Message); override;
    function Dragging: Boolean;
    procedure DragDrop(Source: TObject; X, Y: Integer); dynamic;
    function DrawTextBiDiModeFlags(Flags: Longint): Longint;
    function DrawTextBiDiModeFlagsReadingOnly: Longint;
    property Enabled: Boolean read GetEnabled write SetEnabled stored IsEnabledStored default True;
    procedure EndDrag(Drop: Boolean);
    function GetControlsAlignment: TAlignment; dynamic;
    function GetParentComponent: TComponent; override;
    function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    function GetTextLen: Integer;
    function HasParent: Boolean; override;
    procedure Hide;
    procedure InitiateAction; virtual;
    procedure Invalidate; virtual;
    procedure MouseWheelHandler(var Message: TMessage); dynamic;
    function IsRightToLeft: Boolean;
    function ManualDock(NewDockSite: TWinControl; DropControl: TControl = nil;
      ControlSide: TAlign = alNone): Boolean;
    function ManualFloat(ScreenPos: TRect): Boolean;
    function Perform(Msg: Cardinal; WParam, LParam: Longint): Longint;
    procedure Refresh;
    procedure Repaint; virtual;
    function ReplaceDockedControl(Control: TControl; NewDockSite: TWinControl;
      DropControl: TControl; ControlSide: TAlign): Boolean;
    function ScreenToClient(const Point: TPoint): TPoint;
    function ParentToClient(const Point: TPoint; AParent: TWinControl = nil): TPoint;
    procedure SendToBack;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure SetDesignVisible(Value: Boolean); dynamic;
    procedure SetTextBuf(Buffer: PChar);
    procedure Show;
    procedure Update; virtual;
    function UseRightToLeftAlignment: Boolean; dynamic;
    function UseRightToLeftReading: Boolean;
    function UseRightToLeftScrollBar: Boolean;
    property Action: TBasicAction read GetAction write SetAction;
    property Align: TAlign read FAlign write SetAlign default alNone;
    property Anchors: TAnchors read FAnchors write SetAnchors stored IsAnchorsStored default [akLeft, akTop];
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight stored False;
    property ClientOrigin: TPoint read GetClientOrigin;
    property ClientRect: TRect read GetClientRect;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth stored False;
    property Constraints: TSizeConstraints read FConstraints write SetConstraints;
    property ControlState: TControlState read FControlState write FControlState;
    property ControlStyle: TControlStyle read FControlStyle write FControlStyle;
    property DockOrientation: TDockOrientation read FDockOrientation write FDockOrientation;
    property ExplicitLeft: Integer read FExplicitLeft;
    property ExplicitTop: Integer read FExplicitTop;
    property ExplicitWidth: Integer read FExplicitWidth;
    property ExplicitHeight: Integer read FExplicitHeight;
    property Floating: Boolean read GetFloating;
    property FloatingDockSiteClass: TWinControlClass read GetFloatingDockSiteClass write FFloatingDockSiteClass;
    property HostDockSite: TWinControl read FHostDockSite write SetHostDockSite;
    property LRDockWidth: Integer read GetLRDockWidth write FLRDockWidth;
    property Parent: TWinControl read FParent write SetParent;
    property ShowHint: Boolean read FShowHint write SetShowHint stored IsShowHintStored;
    property TBDockHeight: Integer read GetTBDockHeight write FTBDockHeight;
    property UndockHeight: Integer read GetUndockHeight write FUndockHeight;
    property UndockWidth: Integer read GetUndockWidth write FUndockWidth;
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored default True;
    property WindowProc: TWndMethod read FWindowProc write FWindowProc;
  published
    property AlignWithMargins: Boolean read GetAlignWithMargins write SetAlignWithMargins default False;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property Hint: string read FHint write FHint stored IsHintStored;
    property HelpType: THelpType read FHelpType write FHelpType default htContext;
    property HelpKeyword: String read FHelpKeyword write SetHelpKeyword stored IsHelpContextStored;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext stored IsHelpContextStored default 0;
    property Margins: TMargins read FMargins write SetMargins;
end;

  TControlClass = class of TControl;

  TCreateParams = record
    Caption: PChar;
    Style: DWORD;
    ExStyle: DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWnd;
    Param: Pointer;
    WindowClass: TWndClass;
    WinClassName: array[0..63] of Char;
  end;

{ TWinControlActionLink }

  TWinControlActionLink = class(TControlActionLink)
  protected
    FClient: TWinControl;
    procedure AssignClient(AClient: TObject); override;
    function IsHelpContextLinked: Boolean; override;
    procedure SetHelpContext(Value: THelpContext); override;
  end;

  TWinControlActionLinkClass = class of TWinControlActionLink;

{ TWinControl }

  TImeMode = (imDisable, imClose, imOpen, imDontCare,
              imSAlpha, imAlpha, imHira, imSKata, imKata,
              imChinese, imSHanguel, imHanguel);
  TImeName = type string;

  TAlignInfo = record
    AlignList: TList;
    ControlIndex: Integer;
    Align: TAlign;
    Scratch: Integer;
  end;

  TBorderWidth = 0..MaxInt;

  TBevelCut = (bvNone, bvLowered, bvRaised, bvSpace);
  TBevelEdge = (beLeft, beTop, beRight, beBottom);
  TBevelEdges = set of TBevelEdge;
  TBevelKind = (bkNone, bkTile, bkSoft, bkFlat);
  TBevelWidth = 1..MaxInt;

  // IDockManager defines an interface for managing a dock site's docked
  // controls. The default VCL implementation of IDockManager is TDockTree.
  IDockManager = interface
    ['{8619FD79-C281-11D1-AA60-00C04FA370E8}']
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect);
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl);
    procedure LoadFromStream(Stream: TStream);
    procedure PaintSite(DC: HDC);
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect);
    procedure RemoveControl(Control: TControl);
    procedure ResetBounds(Force: Boolean);
    procedure SaveToStream(Stream: TStream);
    procedure SetReplacingControl(Control: TControl);
  end;

  TAlignInsertBeforeEvent = function(Sender: TWinControl; C1, C2: TControl): Boolean of object;
  TAlignPositionEvent = procedure(Sender: TWinControl; Control: TControl;
    var NewLeft, NewTop, NewWidth, NewHeight: Integer;
    var AlignRect: TRect; AlignInfo: TAlignInfo) of Object;

  TWinControl = class(TControl)
  private
    FAlignLevel: Word;
    FBevelEdges: TBevelEdges;
    FBevelInner: TBevelCut;
    FBevelOuter: TBevelCut;
    FBevelKind: TBevelKind;
    FBevelWidth: TBevelWidth;
    FBorderWidth: TBorderWidth;
    FPadding: TPadding;
    FBrush: TBrush;
    FDefWndProc: Pointer;
    FDockClients: TList;
    FDockManager: IDockManager;
    FHandle: HWnd;
    FImeMode: TImeMode;
    FImeName: TImeName;
    FObjectInstance: Pointer;
    FParentWindow: HWnd;
    FTabList: TList;
    FControls: TList;
    FWinControls: TList;
    FTabOrder: Integer;
    FTabStop: Boolean;
    FCtl3D: Boolean;
    FShowing: Boolean;
    FUseDockManager: Boolean;
    FDockSite: Boolean;
    FParentCtl3D: Boolean;
    FOnDockDrop: TDockDropEvent;
    FOnDockOver: TDockOverEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnGetSiteInfo: TGetSiteInfoEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnUnDock: TUnDockEvent;
    FOnAlignInsertBefore: TAlignInsertBeforeEvent;
    FOnAlignPosition: TAlignPositionEvent;
    FMouseInClient: Boolean;
    FMouseControl: TControl;
    procedure AlignControl(AControl: TControl);
    procedure CalcConstraints(var MinWidth, MinHeight, MaxWidth,
      MaxHeight: Integer);
    procedure DoPaddingChange(Sender: TObject);
    function GetAlignDisabled: Boolean;
    function GetControl(Index: Integer): TControl;
    function GetControlCount: Integer;
    function GetDockClientCount: Integer;
    function GetDockClients(Index: Integer): TControl;
    function GetHandle: HWnd;
    function GetParentBackground: Boolean;
    function GetTabOrder: TTabOrder;
    function GetVisibleDockClientCount: Integer;
    procedure Insert(AControl: TControl);
    procedure InvalidateFrame;
    procedure InvokeHelp;
    function IsCtl3DStored: Boolean;
    function PrecedingWindow(Control: TWinControl): HWnd;
    procedure ReadDesignSize(Reader: TReader);
    procedure Remove(AControl: TControl);
    procedure RemoveFocus(Removing: Boolean);
    procedure SetBevelCut(Index: Integer; const Value: TBevelCut);
    procedure SetBevelEdges(const Value: TBevelEdges);
    procedure SetBevelKind(const Value: TBevelKind);
    procedure SetBevelWidth(const Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetCtl3D(Value: Boolean);
    procedure SetDockSite(Value: Boolean);
    procedure SetPadding(const Value: TPadding);
    procedure SetParentCtl3D(Value: Boolean);
    procedure SetParentWindow(Value: HWnd);
    procedure SetTabOrder(Value: TTabOrder);
    procedure SetTabStop(Value: Boolean);
    procedure SetUseDockManager(Value: Boolean);
    procedure SetZOrderPosition(Position: Integer);
    procedure UpdateTabOrder(Value: TTabOrder);
    procedure UpdateShowing;
    procedure WriteDesignSize(Writer: TWriter);
    function IsMenuKey(var Message: TWMKey): Boolean;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMSysColorChange(var Message: TWMSysColorChange); message WM_SYSCOLORCHANGE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMCompareItem(var Message: TWMCompareItem); message WM_COMPAREITEM;
    procedure WMDeleteItem(var Message: TWMDeleteItem); message WM_DELETEITEM;
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMMeasureItem(var Message: TWMMeasureItem); message WM_MEASUREITEM;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMSysKeyDown(var Message: TWMKeyDown); message WM_SYSKEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMSysKeyUp(var Message: TWMKeyUp); message WM_SYSKEYUP;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMCharToItem(var Message: TWMCharToItem); message WM_CHARTOITEM;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMVKeyToItem(var Message: TWMVKeyToItem); message WM_VKEYTOITEM;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMQueryNewPalette(var Message: TMessage); message WM_QUERYNEWPALETTE;
    procedure WMPaletteChanged(var Message: TMessage); message WM_PALETTECHANGED;
    procedure WMWinIniChange(var Message: TMessage); message WM_WININICHANGE;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    procedure WMTimeChange(var Message: TMessage); message WM_TIMECHANGE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMIMEStartComp(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMIMEEndComp(var Message: TMessage); message WM_IME_ENDCOMPOSITION;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure CMChanged(var Message: TMessage); message CM_CHANGED;
    procedure CMChildKey(var Message: TMessage); message CM_CHILDKEY;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMCursorChanged(var Message: TMessage); message CM_CURSORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentCtl3DChanged(var Message: TMessage); message CM_PARENTCTL3DCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMShowHintChanged(var Message: TMessage); message CM_SHOWHINTCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
    procedure CMFontChange(var Message: TMessage); message CM_FONTCHANGE;
    procedure CMTimeChange(var Message: TMessage); message CM_TIMECHANGE;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNKeyUp(var Message: TWMKeyUp); message CN_KEYUP;
    procedure CNChar(var Message: TWMChar); message CN_CHAR;
    procedure CNSysKeyDown(var Message: TWMKeyDown); message CN_SYSKEYDOWN;
    procedure CNSysChar(var Message: TWMChar); message CN_SYSCHAR;
    procedure CMControlListChange(var Message: TMessage); message CM_CONTROLLISTCHANGE;
    procedure CMControlListChanging(var Message: TMessage); message CM_CONTROLLISTCHANGING;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
    procedure CMInvalidate(var Message: TMessage); message CM_INVALIDATE;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure CMFloat(var Message: TCMFloat); message CM_FLOAT;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMPrintClient(var Message: TWMPrintClient); message WM_PRINTCLIENT;
  protected
    FDoubleBuffered: Boolean;
    FInImeComposition: Boolean;
    FDesignSize: TPoint;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure AddBiDiModeExStyle(var ExStyle: DWORD);
    procedure AssignTo(Dest: TPersistent); override;
    procedure AdjustClientRect(var Rect: TRect); virtual;
    procedure AdjustSize; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); virtual;
    procedure ArrangeControl(AControl: TControl; const ParentSize: TPoint; AAlign: TAlign;
      AAlignInfo: TAlignInfo; var Rect: TRect; UpdateAnchorOrigin: Boolean = False);
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
      MaxHeight: Integer); override;
    procedure ControlsAligned; dynamic;
    function CreateDockManager: IDockManager; dynamic;
    procedure CreateHandle; virtual;
    procedure CreateParams(var Params: TCreateParams); virtual;
    procedure CreateSubClass(var Params: TCreateParams;
      ControlClassName: PChar);
    procedure CreateWindowHandle(const Params: TCreateParams); virtual;
    procedure CreateWnd; virtual;
    function CustomAlignInsertBefore(C1, C2: TControl): Boolean; virtual;
    procedure CustomAlignPosition(Control: TControl; var NewLeft, NewTop, NewWidth,
      NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroyHandle; virtual;
    procedure DestroyWindowHandle; virtual;
    procedure DestroyWnd; virtual;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); dynamic;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); dynamic;
    function DockReplaceDockClient(Client: TControl;
      NewDockSite: TWinControl; DropControl: TControl;
      ControlSide: TAlign; ReplacementClient: TControl): Boolean; virtual;
    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); dynamic;
    procedure DoEnter; dynamic;
    procedure DoExit; dynamic;
    procedure DoFlipChildren; dynamic;
    function DoKeyDown(var Message: TWMKey): Boolean;
    function DoKeyPress(var Message: TWMKey): Boolean;
    function DoKeyUp(var Message: TWMKey): Boolean;
    procedure DoRemoveDockClient(Client: TControl); dynamic;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; dynamic;
    function FindNextControl(CurControl: TWinControl;
      GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;
    procedure FixupTabList;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetClientOrigin: TPoint; override;
    function GetClientRect: TRect; override;
    function GetControlExtents: TRect; virtual;
    function GetDeviceContext(var WindowHandle: HWnd): HDC; override;
    function GetParentHandle: HWnd;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); dynamic;
    function GetTopParentHandle: HWnd;
    procedure InvalidateDockHostSite(FocusLost: Boolean);
    function IsControlMouseMsg(var Message: TWMMouse): Boolean;
    function IsControlActivateMsg(var Message: TWMMouseActivate; Control: TControl = nil): Boolean;
    function IsQualifyingSite(const Client: TControl): Boolean; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyUp(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyPress(var Key: Char); dynamic;
    procedure MainWndProc(var Message: TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyControls(Msg: Word);
    procedure PaintControls(DC: HDC; First: TControl);
    procedure PaintHandler(var Message: TWMPaint);
    procedure PaintWindow(DC: HDC); virtual;
    function PaletteChanged(Foreground: Boolean): Boolean; override;
    procedure ReadState(Reader: TReader); override;
    procedure RecreateWnd;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); dynamic;
    procedure ResetIme;
    function ResetImeComposition(Action: DWORD): Boolean;
    procedure RemoveWindowProps;
    procedure ScaleControls(M, D: Integer);
    procedure SelectFirst;
    procedure SelectNext(CurControl: TWinControl;
      GoForward, CheckTabStop: Boolean);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetIme;
    function SetImeCompositionWindow(Font: TFont; XPos, YPos: Integer): Boolean;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetParentBackground(Value: Boolean); virtual;
    procedure SetZOrder(TopMost: Boolean); override;
    procedure ShowControl(AControl: TControl); virtual;
    procedure UpdateBounds;
    procedure UpdateControlOriginalParentSize(AControl: TControl; var AOriginalParentSize: TPoint); virtual;
    procedure UpdateRecreatingFlag(Recreating: Boolean);
    procedure UpdateUIState(CharCode: Word);
    procedure WndProc(var Message: TMessage); override;
    property BevelEdges: TBevelEdges read FBevelEdges write SetBevelEdges default [beLeft, beTop, beRight, beBottom];
    property BevelInner: TBevelCut index 0 read FBevelInner write SetBevelCut default bvRaised;
    property BevelOuter: TBevelCut index 1 read FBevelOuter write SetBevelCut default bvLowered;
    property BevelKind: TBevelKind read FBevelKind write SetBevelKind default bkNone;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property Ctl3D: Boolean read FCtl3D write SetCtl3D stored IsCtl3DStored;
    property DefWndProc: Pointer read FDefWndProc write FDefWndProc;
    property ImeMode: TImeMode read FImeMode write FImeMode default imDontCare;
    property ImeName: TImeName read FImeName write FImeName;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
    property ParentCtl3D: Boolean read FParentCtl3D write SetParentCtl3D default True;
    property WindowHandle: HWnd read FHandle write FHandle;
    property OnAlignInsertBefore: TAlignInsertBeforeEvent read FOnAlignInsertBefore
      write FOnAlignInsertBefore;
    property OnAlignPosition: TAlignPositionEvent read FOnAlignPosition write FOnAlignPosition;
    property OnDockDrop: TDockDropEvent read FOnDockDrop write FOnDockDrop;
    property OnDockOver: TDockOverEvent read FOnDockOver write FOnDockOver;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnGetSiteInfo: TGetSiteInfoEvent read FOnGetSiteInfo write FOnGetSiteInfo;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnUnDock: TUnDockEvent read FOnUnDock write FOnUnDock;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateParented(ParentWindow: HWnd);
    class function CreateParentedControl(ParentWindow: HWnd): TWinControl;
    destructor Destroy; override;
    procedure Broadcast(var Message);
    function CanFocus: Boolean; dynamic;
    function ContainsControl(Control: TControl): Boolean;
    function ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean;
      AllowWinControls: Boolean = False; AllLevels: Boolean = False): TControl;
    procedure DefaultHandler(var Message); override;
    procedure DisableAlign;
    property DockClientCount: Integer read GetDockClientCount;
    property DockClients[Index: Integer]: TControl read GetDockClients;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); dynamic;
    property DockSite: Boolean read FDockSite write SetDockSite default False;
    property DockManager: IDockManager read FDockManager write FDockManager;
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    procedure EnableAlign;
    function FindChildControl(const ControlName: string): TControl;
    procedure FlipChildren(AllLevels: Boolean); dynamic;
    function Focused: Boolean; dynamic;
    procedure GetTabOrderList(List: TList); dynamic;
    function HandleAllocated: Boolean;
    procedure HandleNeeded;
    procedure InsertControl(AControl: TControl);
    procedure Invalidate; override;
    procedure PaintTo(DC: HDC; X, Y: Integer); overload;
    procedure PaintTo(Canvas: TCanvas; X, Y: Integer); overload;
    function PreProcessMessage(var Msg: TMsg): Boolean; dynamic;
    procedure RemoveControl(AControl: TControl);
    procedure Realign;
    procedure Repaint; override;
    procedure ScaleBy(M, D: Integer);
    procedure ScrollBy(DeltaX, DeltaY: Integer);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetDesignVisible(Value: Boolean); override;
    procedure SetFocus; virtual;
    procedure Update; override;
    procedure UpdateControlState;
    property AlignDisabled: Boolean read GetAlignDisabled;
    property MouseInClient: Boolean read FMouseInClient;
    property VisibleDockClientCount: Integer read GetVisibleDockClientCount;
    property Brush: TBrush read FBrush;
    property Controls[Index: Integer]: TControl read GetControl;
    property ControlCount: Integer read GetControlCount;
    property Handle: HWnd read GetHandle;
    property Padding: TPadding read FPadding write SetPadding;
    property ParentWindow: HWnd read FParentWindow write SetParentWindow;
    property Showing: Boolean read FShowing;
    property TabOrder: TTabOrder read GetTabOrder write SetTabOrder default -1;
    property TabStop: Boolean read FTabStop write SetTabStop default False;
    property UseDockManager: Boolean read FUseDockManager write SetUseDockManager default False;
  end;

  TGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCustomControl = class(TWinControl)
  private
    FCanvas: TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCustomTransparentControl = class(TCustomControl)
  private
    FInterceptMouse: Boolean;
  protected
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure InvalidateControlsUnderneath;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Invalidate; override;
    property InterceptMouse: Boolean read FInterceptMouse write FInterceptMouse default False;
  end;

  THintWindow = class(TCustomControl)
  private
    FActivating: Boolean;
    FLastActive: Cardinal;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure NCPaint(DC: HDC); virtual;
    procedure Paint; override;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); virtual;
    procedure ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer); virtual;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; virtual;
    function IsHintMsg(var Msg: TMsg): Boolean; virtual;
    function ShouldHideHint: Boolean; virtual;
    procedure ReleaseHandle;
    property BiDiMode;
    property Caption;
    property Color;
    property Canvas;
    property Font;
  end;

  THintWindowClass = class of THintWindow;

{ TDragImageList }

  TDragImageList = class(TCustomImageList)
  private
    FDragCursor: TCursor;
    FDragging: Boolean;
    FDragHandle: HWND;
    FDragHotspot: TPoint;
    FDragIndex: Integer;
    FOldCursor: TCursor;
    procedure SetDragCursor(Value: TCursor);
  protected
    procedure Initialize; override;
  public
    function BeginDrag(Window: HWND; X, Y: Integer): Boolean;
    function DragLock(Window: HWND; XPos, YPos: Integer): Boolean;
    function DragMove(X, Y: Integer): Boolean;
    procedure DragUnlock;
    function EndDrag: Boolean;
    function GetHotSpot: TPoint; override;
    procedure HideDragImage;
    function SetDragImage(Index, HotSpotX, HotSpotY: Integer): Boolean;
    procedure ShowDragImage;
    property DragCursor: TCursor read FDragCursor write SetDragCursor;
    property DragHotspot: TPoint read FDragHotspot write FDragHotspot;
    property Dragging: Boolean read FDragging;
  end;

{ TImageList }

  TImageList = class(TDragImageList)
  published
    property BlendColor;
    property BkColor;
    property AllocBy;
    property DrawingStyle;
    property Height;
    property ImageType;
    property Masked;
    property OnChange;
    property ShareImages;
    property Width;
  end;

{ TDockZone }

  TDockTree = class;

  // TDockZone encapsulates a region into which other zones are contained.
  // A TDockZone can be a parent to other zones (when FChildZones <> nil) or
  // can contain only a control (when FChildControl <> nil).  A TDockZone also
  // stores pointers to previous and next siblings and its parent.  Parents
  // store a pointer to only the first child in a doubly-linked list of child
  // zones, though each child maintains a pointer to its parent.  Thus, the
  // data structure of relating TDockZones works out to a kind of a
  // doubly-linked list tree.  The FZoneLimit field of TDockZone represents
  // the coordinate of either the left or bottom of the zone, depending on
  // whether its parent zone's orientation is doVertical or doHorizontal.

  TDockZone = class
  private
    FChildControl: TControl;
    FChildZones: TDockZone;
    FNextSibling: TDockZone;
    FOrientation: TDockOrientation;
    FParentZone: TDockZone;
    FPrevSibling: TDockZone;
    FTree: TDockTree;
    FZoneLimit: Integer;
    FOldSize: Integer;
    function GetChildCount: Integer;
    function GetControlName: string;
    function GetLimitBegin: Integer;
    function GetLimitSize: Integer;
    function GetTopLeft(Orient: Integer{TDockOrientation}): Integer;
    function GetHeightWidth(Orient: Integer{TDockOrientation}): Integer;
    function GetVisible: Boolean;
    function GetVisibleChildCount: Integer;
    function GetZoneLimit: Integer;
    function SetControlName(const Value: string): Boolean;
    procedure SetZoneLimit(const Value: Integer);
  public
    constructor Create(Tree: TDockTree);
    procedure ExpandZoneLimit(NewLimit: Integer);
    function FirstVisibleChild: TDockZone;
    function NextVisible: TDockZone;
    function PrevVisible: TDockZone;
    procedure ResetChildren;
    procedure ResetZoneLimits;
    procedure Update;
    property ChildCount: Integer read GetChildCount;
    property ChildControl: TControl read FChildControl;
    property Height: Integer index Ord(doHorizontal) read GetHeightWidth;
    property Left: Integer index Ord(doVertical) read GetTopLeft;
    property LimitBegin: Integer read GetLimitBegin;
    property LimitSize: Integer read GetLimitSize;
    property Top: Integer index Ord(doHorizontal) read GetTopLeft;
    property Visible: Boolean read GetVisible;
    property VisibleChildCount: Integer read GetVisibleChildCount;
    property Width: Integer index Ord(doVertical) read GetHeightWidth;
    property ZoneLimit: Integer read GetZoneLimit write SetZoneLimit;
  end;

{ TDockTree }

  TForEachZoneProc = procedure(Zone: TDockZone) of object;

  TDockTreeClass = class of TDockTree;

  // TDockTree serves as a manager for a tree of TDockZones.  It is responsible
  // for inserting and removing controls (and thus zones) from the tree and
  // associated housekeeping, such as orientation, zone limits, parent zone
  // creation, and painting of controls into zone bounds.
  TDockTree = class(TInterfacedObject, IDockManager)
  private
    FBorderWidth: Integer;
    FBrush: TBrush;
    FDockSite: TWinControl;
    FGrabberSize: Integer;
    FGrabbersOnTop: Boolean;
    FOldRect: TRect;
    FOldWndProc: TWndMethod;
    FReplacementZone: TDockZone;
    FScaleBy: Double;
    FShiftScaleOrient: TDockOrientation;
    FShiftBy: Integer;
    FSizePos: TPoint;
    FSizingDC: HDC;
    FSizingWnd: HWND;
    FSizingZone: TDockZone;
    FTopZone: TDockZone;
    FTopXYLimit: Integer;
    FUpdateCount: Integer;
    FVersion: Integer;
    FRelativeSizes: Boolean;
    procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean);
    function ActualSize(const RelativeSize, Reference: Integer): Integer;
    function RelativeSize(const ActualSize, Reference: Integer): Integer;
    procedure DrawSizeSplitter;
    function FindControlZone(Control: TControl): TDockZone;
    procedure ForEachAt(Zone: TDockZone; Proc: TForEachZoneProc);
    function GetNextLimit(AZone: TDockZone): Integer;
    procedure InsertNewParent(NewZone, SiblingZone: TDockZone;
      ParentOrientation: TDockOrientation; InsertLast: Boolean);
    procedure InsertSibling(NewZone, SiblingZone: TDockZone; InsertLast: Boolean);
    function InternalHitTest(const MousePos: TPoint;
      out HTFlag: Integer): TDockZone;
    procedure PruneZone(Zone: TDockZone);
    procedure RemoveZone(Zone: TDockZone);
    procedure ScaleZone(Zone: TDockZone);
    procedure SetNewBounds(Zone: TDockZone);
    procedure ShiftZone(Zone: TDockZone);
    procedure SplitterMouseDown(OnZone: TDockZone; MousePos: TPoint);
    procedure SplitterMouseUp;
    procedure UpdateZone(Zone: TDockZone);
    procedure WindowProc(var Message: TMessage);
  protected
    procedure AdjustDockRect(Control: TControl; var ARect: TRect); virtual;
    procedure AdjustFrameRect(Control: TControl; var ARect: TRect); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function FindControlAtPos(const Pos: TPoint): TControl;
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect);
    function HitTest(const MousePos: TPoint; out HTFlag: Integer): TControl; virtual;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer;
      var Handled: Boolean); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
    procedure PaintDockFrame(Canvas: TCanvas; Control: TControl;
      const ARect: TRect); virtual;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect); virtual;
    function ReferenceFromOrient(const Orient: TDockOrientation): Integer; virtual;
    procedure RemoveControl(Control: TControl); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetReplacingControl(Control: TControl);
    procedure ShowHint(CursorPos: TPoint; var CursorRect: TRect;
      var HintStr: string); virtual;
    procedure ResetBounds(Force: Boolean); virtual;
    procedure UpdateAll;
    procedure WndProc(var Message: TMessage); virtual;
    function ZoneCaptionHitTest(const Zone: TDockZone; const MousePos: TPoint;
      var HTFlag: Integer): Boolean; virtual;
    property DockSite: TWinControl read FDockSite write FDockSite;
    property RelativeSizes: Boolean read FRelativeSizes write FRelativeSizes;
    property TopZone: TDockZone read FTopZone;
  public
    constructor Create(DockSite: TWinControl); virtual;
    destructor Destroy; override;
    procedure PaintSite(DC: HDC); virtual;
  end;

{ Mouse support }

  TCustomPanningWindow = class; TPanningWindowClass = class of TCustomPanningWindow;

  { TCustomPanningWindow }
  TCustomPanningWindow = class(TCustomControl)
    function GetIsPanning: Boolean; virtual; abstract;
    function StartPanning(AHandle: THandle; AControl: TControl): Boolean; virtual; abstract;
    procedure StopPanning; virtual; abstract;
  end;

  TMouse = class
  private
    FDragImmediate: Boolean;
    FDragThreshold: Integer;
    FMousePresent: Boolean;
    FNativeWheelSupport: Boolean;
    FScrollLines: Integer;
    FScrollLinesMessage: UINT;
    FWheelHwnd: HWND;
    FWheelMessage: UINT;
    FWheelPresent: Boolean;
    FWheelSupportMessage: UINT;
    FPanningWindow: TCustomPanningWindow;
    FPanningWindowClass: TPanningWindowClass;
    procedure GetMouseData;
    procedure GetNativeData;
    procedure GetRegisteredData;
    function GetCursorPos: TPoint;
    procedure SetCursorPos(const Value: TPoint);
    function GetCapture: HWND;
    procedure SetCapture(const Value: HWND);
    function GetIsDragging: Boolean;
    procedure SetPanningWindow(const Value: TCustomPanningWindow);
    function GetIsPanning: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettingChanged(Setting: Integer);
    function CreatePanningWindow: TCustomPanningWindow;
    property Capture: HWND read GetCapture write SetCapture;
    property CursorPos: TPoint read GetCursorPos write SetCursorPos;
    property DragImmediate: Boolean read FDragImmediate write FDragImmediate default True;
    property DragThreshold: Integer read FDragThreshold write FDragThreshold default 5;
    property MousePresent: Boolean read FMousePresent;
    property IsDragging: Boolean read GetIsDragging;
    property IsPanning: Boolean read GetIsPanning;
    property PanningWindow: TCustomPanningWindow read FPanningWindow write SetPanningWindow;
    property PanningWindowClass: TPanningWindowClass read FPanningWindowClass write
      FPanningWindowClass;
    property RegWheelMessage: UINT read FWheelMessage;
    property WheelPresent: Boolean read FWheelPresent;
    property WheelScrollLines: Integer read FScrollLines;
  end;

  TCustomListControl = class(TWinControl)
  protected
    function GetCount: Integer; virtual; abstract;
    function GetItemIndex: Integer; virtual; abstract;
    procedure SetItemIndex(const Value: Integer); overload; virtual; abstract;
  public
    procedure AddItem(Item: String; AObject: TObject); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    procedure CopySelection(Destination: TCustomListControl); virtual; abstract;
    procedure DeleteSelected; virtual; abstract;
    procedure MoveSelection(Destination: TCustomListControl); virtual;
    procedure SelectAll; virtual; abstract;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  end;

  TCustomMultiSelectListControl = class(TCustomListControl)
  protected
    FMultiSelect: Boolean;
    function GetSelCount: Integer; virtual; abstract;
    procedure SetMultiSelect(Value: Boolean); virtual; abstract;
  public
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property SelCount: Integer read GetSelCount;
  end;

  TAnimateWindowProc = function(hWnd: HWND; dwTime: DWORD; dwFlags: DWORD): BOOL; stdcall;

var
  Mouse: TMouse;
  AnimateWindowProc: TAnimateWindowProc = nil;

{ Drag stuff }

function IsDragObject(Sender: TObject): Boolean;
function IsVCLControl(Handle: HWnd): Boolean;
function FindControl(Handle: HWnd): TWinControl;
function FindVCLWindow(const Pos: TPoint): TWinControl;
function FindDragTarget(const Pos: TPoint; AllowDisabled: Boolean): TControl;
function GetCaptureControl: TControl;
procedure SetCaptureControl(Control: TControl);
procedure CancelDrag;

{ Misc }

function CursorToString(Cursor: TCursor): string;
function StringToCursor(const S: string): TCursor;
procedure GetCursorValues(Proc: TGetStrProc);
function CursorToIdent(Cursor: Longint; var Ident: string): Boolean;
function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean;

function GetShortHint(const Hint: string): string;
function GetLongHint(const Hint: string): string;

procedure PerformEraseBackground(Control: TControl; DC: HDC);

var
  CreationControl: TWinControl = nil;
  DefaultDockTreeClass: TDockTreeClass = TDockTree;
  IsVCLControlHook: function (Handle: HWnd): Boolean of object;

function InitWndProc(HWindow: HWnd; Message, WParam: Longint;
  LParam: Longint): Longint; stdcall;

const
  CTL3D_ALL = $FFFF;
  NullDockSite = TWinControl($FFFFFFFF);
  AnchorAlign: array[TAlign] of TAnchors = (
    { alNone }
    [akLeft, akTop],
    { alTop }
    [akLeft, akTop, akRight],
    { alBottom }
    [akLeft, akRight, akBottom],
    { alLeft }
    [akLeft, akTop, akBottom],
    { alRight }
    [akRight, akTop, akBottom],
    { alClient }
    [akLeft, akTop, akRight, akBottom],
    { alCustom }
    [akLeft, akTop]
    );

var
  NewStyleControls: Boolean;

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);

function SendAppMessage(Msg: Cardinal; WParam, LParam: Longint): Longint;
procedure MoveWindowOrg(DC: HDC; DX, DY: Integer);

procedure SetImeMode(hWnd: HWND; Mode: TImeMode);
procedure SetImeName(Name: TImeName);
function Win32NLSEnableIME(hWnd: HWND; Enable: Boolean): Boolean;
function Imm32GetContext(hWnd: HWND): HIMC;
function Imm32ReleaseContext(hWnd: HWND; hImc: HIMC): Boolean;
function Imm32GetConversionStatus(hImc: HIMC; var Conversion, Sentence: DWORD): Boolean;
function Imm32SetConversionStatus(hImc: HIMC; Conversion, Sentence: DWORD): Boolean;
function Imm32SetOpenStatus(hImc: HIMC; fOpen: Boolean): Boolean;
function Imm32SetCompositionWindow(hImc: HIMC; lpCompForm: PCOMPOSITIONFORM): Boolean;
function Imm32SetCompositionFont(hImc: HIMC; lpLogfont: PLOGFONTA): Boolean;
function Imm32GetCompositionString(hImc: HIMC; dWord1: DWORD; lpBuf: pointer; dwBufLen: DWORD): Longint;
function Imm32IsIME(hKl: HKL): Boolean;
function Imm32NotifyIME(hImc: HIMC; dwAction, dwIndex, dwValue: DWORD): Boolean;
procedure DragDone(Drop: Boolean);

implementation
