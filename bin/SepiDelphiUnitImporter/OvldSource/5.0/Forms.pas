{*******************************************************}
{                                                       }
{       CodeGear Delphi Visual Component Library        }
{                                                       }
{           Copyright (c) 1995-2007 CodeGear            }
{                                                       }
{*******************************************************}

unit Forms;

{$P+,S-,W-,R-,T-,H+,X+}
{$C PRELOAD}
{$WARN SYMBOL_PLATFORM OFF}

interface

{$IFDEF LINUX}
uses WinUtils, Messages, Libc, Windows, SysUtils, Classes, Graphics, Menus,
  Controls, Imm, ActnList, MultiMon, HelpIntfs;
{$ENDIF}
{$IFDEF MSWINDOWS}
uses Messages, Windows, SysUtils, Classes, Graphics, Menus, Controls, Imm,
  ActnList, MultiMon, HelpIntfs;
{$ENDIF}

type

{ Forward declarations }

  TScrollingWinControl = class;
  TCustomForm = class;
  TForm = class;
  TMonitor = class;

{ TControlScrollBar }

  TScrollBarKind = (sbHorizontal, sbVertical);
  TScrollBarInc = 1..32767;
  TScrollBarStyle = (ssRegular, ssFlat, ssHotTrack);

  TControlScrollBar = class(TPersistent)
  private
    FControl: TScrollingWinControl;
    FIncrement: TScrollBarInc;
    FPageIncrement: TScrollbarInc;
    FPosition: Integer;
    FRange: Integer;
    FCalcRange: Integer;
    FKind: TScrollBarKind;
    FMargin: Word;
    FVisible: Boolean;
    FTracking: Boolean;
    FScaled: Boolean;
    FSmooth: Boolean;
    FDelay: Integer;
    FButtonSize: Integer;
    FColor: TColor;
    FParentColor: Boolean;
    FSize: Integer;
    FStyle: TScrollBarStyle;
    FThumbSize: Integer;
    FPageDiv: Integer;
    FLineDiv: Integer;
    FUpdateNeeded: Boolean;
    constructor Create(AControl: TScrollingWinControl; AKind: TScrollBarKind);
    procedure CalcAutoRange;
    function ControlSize(ControlSB, AssumeSB: Boolean): Integer;
    procedure DoSetRange(Value: Integer);
    function GetScrollPos: Integer;
    function NeedsScrollBarVisible: Boolean;
    function IsIncrementStored: Boolean;
    procedure ScrollMessage(var Msg: TWMScroll);
    procedure SetButtonSize(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetParentColor(Value: Boolean);
    procedure SetPosition(Value: Integer);
    procedure SetRange(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TScrollBarStyle);
    procedure SetThumbSize(Value: Integer);
    procedure SetVisible(Value: Boolean);
    function IsRangeStored: Boolean;
    procedure Update(ControlSB, AssumeSB: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure ChangeBiDiPosition;
    property Kind: TScrollBarKind read FKind;
    function IsScrollBarVisible: Boolean;
    property ScrollPos: Integer read GetScrollPos;
  published
    property ButtonSize: Integer read FButtonSize write SetButtonSize default 0;
    property Color: TColor read FColor write SetColor default clBtnHighlight;
    property Increment: TScrollBarInc read FIncrement write FIncrement stored IsIncrementStored default 8;
    property Margin: Word read FMargin write FMargin default 0;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property Position: Integer read FPosition write SetPosition default 0;
    property Range: Integer read FRange write SetRange stored IsRangeStored default 0;
    property Smooth: Boolean read FSmooth write FSmooth default False;
    property Size: Integer read FSize write SetSize default 0;
    property Style: TScrollBarStyle read FStyle write SetStyle default ssRegular;
    property ThumbSize: Integer read FThumbSize write SetThumbSize default 0;
    property Tracking: Boolean read FTracking write FTracking default False;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

{ TScrollingWinControl }

  TWindowState = (wsNormal, wsMinimized, wsMaximized);

  TScrollingWinControl = class(TWinControl)
  private
    FHorzScrollBar: TControlScrollBar;
    FVertScrollBar: TControlScrollBar;
    FAutoScroll: Boolean;
    FAutoRangeCount: Integer;
    FUpdatingScrollBars: Boolean;
    procedure CalcAutoRange;
    procedure ScaleScrollBars(M, D: Integer);
    procedure SetAutoScroll(Value: Boolean);
    procedure SetHorzScrollBar(Value: TControlScrollBar);
    procedure SetVertScrollBar(Value: TControlScrollBar);
    procedure UpdateScrollBars;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    function AutoScrollEnabled: Boolean; virtual;
    procedure AutoScrollInView(AControl: TControl); virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoFlipChildren; override;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default False;
    procedure Resizing(State: TWindowState); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisableAutoRange;
    procedure EnableAutoRange;
    procedure ScrollInView(AControl: TControl);
  published
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property HorzScrollBar: TControlScrollBar read FHorzScrollBar write SetHorzScrollBar;
    property VertScrollBar: TControlScrollBar read FVertScrollBar write SetVertScrollBar;
  end;

{ TScrollBox }

  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow,
    bsSizeToolWin);
  TBorderStyle = bsNone..bsSingle;

  TScrollBox = class(TScrollingWinControl)
  private
    FBorderStyle: TBorderStyle;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoScroll default True;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
    property Ctl3D;
    property Font;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground default False;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ TCustomFrame }

  TCustomFrame = class(TScrollingWinControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure PaintWindow(DC: HDC); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCustomFrameClass = class of TCustomFrame;

{ TFrame }

  TFrame = class(TCustomFrame)
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
    property Ctl3D;
    property Font;
    property Padding;
    property ParentBackground default True;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ IDesignerHook }

  IDesignerHook = interface(IDesignerNotify)
    ['{1E431DA5-2BEA-4DE7-A330-CC45FD2FB1EC}']
    function GetCustomForm: TCustomForm;
    procedure SetCustomForm(Value: TCustomForm);
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
    function IsDesignMsg(Sender: TControl; var Message: TMessage): Boolean;
    procedure PaintGrid;
    procedure PaintMenu;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string);
    function UniqueName(const BaseName: string): string;
    function GetRoot: TComponent;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read GetCustomForm write SetCustomForm;
  end;

{ IOleForm }

  IOleForm = interface
    ['{CD02E1C1-52DA-11D0-9EA6-0020AF3D82DA}']
    procedure OnDestroy;
    procedure OnResize;
  end;

{ TCustomForm }

  TPopupWnd = record
    ID: Integer;
    ControlWnd: HWND;
  end;
  TPopupWndArray = array of TPopupWnd;

  TFormStyle = (fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop);
  TBorderIcon = (biSystemMenu, biMinimize, biMaximize, biHelp);
  TBorderIcons = set of TBorderIcon;
  TPosition = (poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly,
    poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter);
  TDefaultMonitor = (dmDesktop, dmPrimary, dmMainForm, dmActiveForm);
  TPrintScale = (poNone, poProportional, poPrintToFit);
  TShowAction = (saIgnore, saRestore, saMinimize, saMaximize);
  TTileMode = (tbHorizontal, tbVertical);
  TCloseAction = (caNone, caHide, caFree, caMinimize);
  TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender: TObject;
    var CanClose: Boolean) of object;
  TFormState = set of (fsCreating, fsVisible, fsShowing, fsModal,
    fsCreatedMDIChild, fsActivated);
  TShortCutEvent = procedure (var Msg: TWMKey; var Handled: Boolean) of object;
  THelpEvent = function(Command: Word; Data: Longint;
    var CallHelp: Boolean): Boolean of object;
  TPopupMode = (pmNone, pmAuto, pmExplicit);

  TCustomForm = class(TScrollingWinControl)
  private
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    FSizeChanging: Boolean;
    FWindowState: TWindowState;
    FShowAction: TShowAction;
    FKeyPreview: Boolean;
    FActive: Boolean;
    FFormStyle: TFormStyle;
    FPosition: TPosition;
    FDefaultMonitor: TDefaultMonitor;
    FTileMode: TTileMode;
    FDropTarget: Boolean;
    FOldCreateOrder: Boolean;
    FPrintScale: TPrintScale;
    FCanvas: TControlCanvas;
    FHelpFile: string;
    FIcon: TIcon;
    FInCMParentBiDiModeChanged: Boolean;
    FMenu: TMainMenu;
    FModalResult: TModalResult;
    FDesigner: IDesignerHook;
    FClientHandle: HWND;
    FWindowMenu: TMenuItem;
    FPixelsPerInch: Integer;
    FObjectMenuItem: TMenuItem;
    FOleForm: IOleForm;
    FClientWidth: Integer;
    FClientHeight: Integer;
    FTextHeight: Integer;
    FDefClientProc: TFarProc;
    FClientInstance: TFarProc;
    FActiveOleControl: TWinControl;
    FSavedBorderStyle: TFormBorderStyle;
    FOnActivate: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnDeactivate: TNotifyEvent;
    FOnHelp: THelpEvent;
    FOnHide: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FOnShortCut: TShortCutEvent;
    FOnShow: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FPopupChildren: TList;
    FPopupMode: TPopupMode;
    FPopupParent: TCustomForm;
    FRecreateChildren: TList;
    FPopupWnds: TPopupWndArray;
    FInternalPopupParent: TCustomForm;
    FInternalPopupParentWnd: HWND;
    FScreenSnap: Boolean;
    FSnapBuffer: Integer;
    FTransparentColor: Boolean;
    FTransparentColorValue: TColor;
    procedure RefreshMDIMenu;
    procedure ClientWndProc(var Message: TMessage);
    function GetActiveMDIChild: TForm;
    function GetCanvas: TCanvas;
    function GetIconHandle: HICON;
    function GetLeft: Integer;
    function GetMDIChildCount: Integer;
    function GetMDIChildren(I: Integer): TForm;
    function GetMonitor: TMonitor;
    function GetPixelsPerInch: Integer;
    function GetPopupChildren: TList;
    function GetRecreateChildren: TList;
    function GetScaled: Boolean;
    function GetTextHeight: Integer;
    function GetTop: Integer;
    procedure IconChanged(Sender: TObject);
    function IsAutoScrollStored: Boolean;
    function IsClientSizeStored: Boolean;
    function IsForm: Boolean;
    function IsFormSizeStored: Boolean;
    function IsIconStored: Boolean;
    procedure MergeMenu(MergeState: Boolean);
    procedure ReadIgnoreFontProperty(Reader: TReader);
    procedure ReadTextHeight(Reader: TReader);
    procedure SetActive(Value: Boolean);
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
    procedure SetWindowFocus;
    procedure SetWindowMenu(Value: TMenuItem);
    procedure SetObjectMenuItem(Value: TMenuItem);
    procedure SetWindowState(Value: TWindowState);
    procedure SetWindowToMonitor;
    procedure WritePixelsPerInch(Writer: TWriter);
    procedure WriteTextHeight(Writer: TWriter);
    function NormalColor: TColor;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMIconEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ICONERASEBKGND;
    procedure WMQueryDragIcon(var Message: TWMQueryDragIcon); message WM_QUERYDRAGICON;
    procedure WMNCCreate(var Message: TWMNCCreate); message WM_NCCREATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMInitMenuPopup(var Message: TWMInitMenuPopup); message WM_INITMENUPOPUP;
    procedure WMMenuChar(var Message: TWMMenuChar); message WM_MENUCHAR;
    procedure WMMenuSelect(var Message: TWMMenuSelect); message WM_MENUSELECT;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;
    procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMMDIActivate(var Message: TWMMDIActivate); message WM_MDIACTIVATE;
    procedure WMNextDlgCtl(var Message: TWMNextDlgCtl); message WM_NEXTDLGCTL;
    procedure WMEnterMenuLoop(var Message: TMessage); message WM_ENTERMENULOOP;
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMSettingChange(var Message: TMessage); message WM_SETTINGCHANGE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure CMActionExecute(var Message: TMessage); message CM_ACTIONEXECUTE;
    procedure CMActionUpdate(var Message: TMessage); message CM_ACTIONUPDATE;
    procedure CMActivate(var Message: TCMActivate); message CM_ACTIVATE;
    procedure CMAppSysCommand(var Message: TMessage); message CM_APPSYSCOMMAND;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMDeactivate(var Message: TCMDeactivate); message CM_DEACTIVATE;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMenuChanged(var Message: TMessage); message CM_MENUCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMIconChanged(var Message: TMessage); message CM_ICONCHANGED;
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMUIActivate(var Message); message CM_UIACTIVATE;
    procedure CMParentBiDiModeChanged(var Message: TMessage); message CM_PARENTBIDIMODECHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMPopupHwndDestroy(var Message: TCMPopupHWndDestroy); message CM_POPUPHWNDDESTROY;
    procedure CMIsShortCut(var Message: TWMKey); message CM_ISSHORTCUT;
    procedure SetLayeredAttribs;
    procedure SetAlphaBlend(const Value: Boolean);
    procedure SetAlphaBlendValue(const Value: Byte);
    procedure SetTransparentColor(const Value: Boolean);
    procedure SetTransparentColorValue(const Value: TColor);
    procedure InitAlphaBlending(var Params: TCreateParams);
  protected
    FFormState: TFormState;
    procedure Activate; dynamic;
    procedure ActiveChanged; dynamic;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure BeginAutoDrag; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure CloseModal;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Deactivate; dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroyHandle; override;
    procedure DestroyWindowHandle; override;
    procedure DoClose(var Action: TCloseAction); dynamic;
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure GetBorderIconStyles(var Style, ExStyle: Cardinal); dynamic;
    procedure GetBorderStyles(var Style, ExStyle, ClassStyle: Cardinal); dynamic;
    function GetClientRect: TRect; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetFloating: Boolean; override;
    function GetOwnerWindow: HWND; dynamic;
    function HandleCreateException: Boolean; dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; dynamic;
    procedure PaintWindow(DC: HDC); override;
    function PaletteChanged(Foreground: Boolean): Boolean; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure ReadState(Reader: TReader); override;
    procedure RequestAlign; override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetParentBiDiMode(Value: Boolean); override;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateActions; virtual;
    procedure UpdateWindowState;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); override;
    procedure VisibleChanging; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Resizing(State: TWindowState); override;
    property ActiveMDIChild: TForm read GetActiveMDIChild;
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend;
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue;
    property BorderIcons: TBorderIcons read FBorderIcons write SetBorderIcons stored IsForm
      default [biSystemMenu, biMinimize, biMaximize];
    property AutoScroll stored IsAutoScrollStored;
    property ClientHandle: HWND read FClientHandle;
    property ClientHeight write SetClientHeight stored IsClientSizeStored;
    property ClientWidth write SetClientWidth stored IsClientSizeStored;
    property TransparentColor: Boolean read FTransparentColor write SetTransparentColor;
    property TransparentColorValue: TColor read FTransparentColorValue write SetTransparentColorValue;
    property Ctl3D default True;
    property DefaultMonitor: TDefaultMonitor read FDefaultMonitor write FDefaultMonitor
      stored IsForm default dmActiveForm;
    property FormStyle: TFormStyle read FFormStyle write SetFormStyle
      stored IsForm default fsNormal;
    property Height stored IsFormSizeStored;
    property HorzScrollBar stored IsForm;
    property Icon: TIcon read FIcon write SetIcon stored IsIconStored;
    property MDIChildCount: Integer read GetMDIChildCount;
    property MDIChildren[I: Integer]: TForm read GetMDIChildren;
    property OldCreateOrder: Boolean read FOldCreateOrder write FOldCreateOrder;
    property ObjectMenuItem: TMenuItem read FObjectMenuItem write SetObjectMenuItem
      stored IsForm;
    property PixelsPerInch: Integer read GetPixelsPerInch write SetPixelsPerInch
      stored False;
    property ParentFont default False;
    property PopupMenu stored IsForm;
    property PopupChildren: TList read GetPopupChildren;
    property Position: TPosition read FPosition write SetPosition stored IsForm
      default poDefaultPosOnly;
    property PrintScale: TPrintScale read FPrintScale write FPrintScale stored IsForm
      default poProportional;
    property Scaled: Boolean read GetScaled write SetScaled stored IsForm default True;
    property TileMode: TTileMode read FTileMode write FTileMode default tbHorizontal;
    property VertScrollBar stored IsForm;
    property Visible write SetVisible default False;
    property Width stored IsFormSizeStored;
    property WindowMenu: TMenuItem read FWindowMenu write SetWindowMenu stored IsForm;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate stored IsForm;
    property OnCanResize stored IsForm;
    property OnClick stored IsForm;
    property OnClose: TCloseEvent read FOnClose write FOnClose stored IsForm;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery
      stored IsForm;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate stored IsForm;
    property OnDblClick stored IsForm;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy stored IsForm;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate stored IsForm;
    property OnDragDrop stored IsForm;
    property OnDragOver stored IsForm;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHide: TNotifyEvent read FOnHide write FOnHide stored IsForm;
    property OnKeyDown stored IsForm;
    property OnKeyPress stored IsForm;
    property OnKeyUp stored IsForm;
    property OnMouseActivate stored IsForm;
    property OnMouseDown stored IsForm;
    property OnMouseMove stored IsForm;
    property OnMouseUp stored IsForm;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint stored IsForm;
    property OnResize stored IsForm;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
    property OnShow: TNotifyEvent read FOnShow write FOnShow stored IsForm;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Close;
    function CloseQuery: Boolean; virtual;
    procedure DefaultHandler(var Message); override;
    procedure DefocusControl(Control: TWinControl; Removing: Boolean);
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); override;
    procedure FocusControl(Control: TWinControl);
    function GetFormImage: TBitmap;
    procedure Hide;
    function IsShortCut(var Message: TWMKey): Boolean; dynamic;
    procedure MakeFullyVisible(AMonitor: TMonitor = nil);
    procedure MouseWheelHandler(var Message: TMessage); override;
    procedure Print;
    procedure RecreateAsPopup(AWindowHandle: HWND);
    procedure Release;
    procedure SendCancelMode(Sender: TControl);
    procedure SetFocus; override;
    function SetFocusedControl(Control: TWinControl): Boolean; virtual;
    procedure Show;
    function ShowModal: Integer; virtual;
    function WantChildKey(Child: TControl; var Message: TMessage): Boolean; virtual;
    property Active: Boolean read FActive;
    property ActiveControl: TWinControl read FActiveControl write SetActiveControl
      stored IsForm;
    property Action;
    property ActiveOleControl: TWinControl read FActiveOleControl write SetActiveOleControl;
    property BorderStyle: TFormBorderStyle read FBorderStyle write SetBorderStyle
      stored IsForm default bsSizeable;
    property Canvas: TCanvas read GetCanvas;
    property Caption stored IsForm;
    property Color nodefault;
    property Designer: IDesignerHook read FDesigner write SetDesigner;
    property DropTarget: Boolean read FDropTarget write FDropTarget;
    property Font;
    property FormState: TFormState read FFormState;
    property HelpFile: string read FHelpFile write FHelpFile;
    property KeyPreview: Boolean read FKeyPreview write FKeyPreview
      stored IsForm default False;
    property Menu: TMainMenu read FMenu write SetMenu stored IsForm;
    property ModalResult: TModalResult read FModalResult write FModalResult;
    property Monitor: TMonitor read GetMonitor;
    property OleFormObject: IOleForm read FOleForm write FOleForm;
    property PopupMode: TPopupMode read FPopupMode write SetPopupMode default pmNone;
    property PopupParent: TCustomForm read FPopupParent write SetPopupParent;
    property ScreenSnap: Boolean read FScreenSnap write FScreenSnap default False;
    property SnapBuffer: Integer read FSnapBuffer write FSnapBuffer;
    property WindowState: TWindowState read FWindowState write SetWindowState
      stored IsForm default wsNormal;
  published
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
  end;

  TCustomFormClass = class of TCustomForm;

  { TCustomActiveForm }

  TActiveFormBorderStyle = (afbNone, afbSingle, afbSunken, afbRaised);

  TCustomActiveForm = class(TCustomForm)
  private
    FAxBorderStyle: TActiveFormBorderStyle;
    procedure SetAxBorderStyle(Value: TActiveFormBorderStyle);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    function WantChildKey(Child: TControl; var Message: TMessage): Boolean; override;
    property Visible;
  published
    property ActiveControl;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property AxBorderStyle: TActiveFormBorderStyle read FAxBorderStyle
      write SetAxBorderStyle default afbSingle;
    property BorderWidth;
    property Caption stored True;
    property Color;
    property Constraints;
    property Font;
    property Height stored True;
    property HorzScrollBar;
    property KeyPreview;
    property Padding;
    property OldCreateOrder;
    property PixelsPerInch;
    property PopupMenu;
    property PrintScale;
    property Scaled;
    property ShowHint;
    property VertScrollBar;
    property Width stored True;
    property OnActivate;
    property OnClick;
    property OnCreate;
    property OnContextPopup;
    property OnDblClick;
    property OnDestroy;
    property OnDeactivate;
    property OnDragDrop;
    property OnDragOver;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
  end;

{ TForm }

  TForm = class(TCustomForm)
  public
    procedure ArrangeIcons;
    procedure Cascade;
    procedure Next;
    procedure Previous;
    procedure Tile;
    property ActiveMDIChild;
    property ClientHandle;
    property DockManager;
    property MDIChildCount;
    property MDIChildren;
    property TileMode;
  published
    property Action;
    property ActiveControl;
    property Align;
    property AlphaBlend default False;
    property AlphaBlendValue default 255;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderIcons;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property TransparentColor default False;
    property TransparentColorValue default 0;
    property Constraints;
    property Ctl3D;
    property UseDockManager;
    property DefaultMonitor;
    property DockSite;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentFont default False;
    property Font;
    property FormStyle;
    property Height;
    property HelpFile;
    property HorzScrollBar;
    property Icon;
    property KeyPreview;
    property Padding;
    property Menu;
    property OldCreateOrder;
    property ObjectMenuItem;
    property ParentBiDiMode;
    property PixelsPerInch;
    property PopupMenu;
    property PopupMode;
    property PopupParent;
    property Position;
    property PrintScale;
    property Scaled;
    property ScreenSnap default False;
    property ShowHint;
    property SnapBuffer default 10;
    property VertScrollBar;
    property Visible;
    property Width;
    property WindowState;
    property WindowMenu;
    property OnActivate;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDestroy;
    property OnDeactivate;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnHide;
    property OnHelp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnStartDock;
    property OnUnDock;
  end;

  TFormClass = class of TForm;

{ TCustomDockForm }

  TCustomDockForm = class(TCustomForm)
  private
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure CMControlListChange(var Message: TMessage); message CM_CONTROLLISTCHANGE;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    property AutoScroll default False;
    property BorderStyle default bsSizeToolWin;
    property FormStyle default fsStayOnTop;
  published
    property PixelsPerInch;
  end;

{ TMonitor }

  TMonitor = class(TObject)
  private
    FHandle: HMONITOR;
    FMonitorNum: Integer;
    function GetLeft: Integer;
    function GetHeight: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    function GetBoundsRect: TRect;
    function GetWorkareaRect: TRect;
    function GetPrimary: Boolean;
  public
    property Handle: HMONITOR read FHandle;
    property MonitorNum: Integer read FMonitorNum;
    property Left: Integer read GetLeft;
    property Height: Integer read GetHeight;
    property Top: Integer read GetTop;
    property Width: Integer read GetWidth;
    property BoundsRect: TRect read GetBoundsRect;
    property WorkareaRect: TRect read GetWorkareaRect;
    property Primary: Boolean read GetPrimary;
  end;

{ TScreen }

  PCursorRec = ^TCursorRec;
  TCursorRec = record
    Next: PCursorRec;
    Index: Integer;
    Handle: HCURSOR;
  end;

  TMonitorDefaultTo = (mdNearest, mdNull, mdPrimary);

  TScreen = class(TComponent)
  private
    FFonts: TStrings;
    FImes: TStrings;
    FDefaultIme: string;
    FDefaultKbLayout: HKL;
    FPixelsPerInch: Integer;
    FCursor: TCursor;
    FCursorCount: Integer;
    FForms: TList;
    FCustomForms: TList;
    FDataModules: TList;
    FMonitors: TList;
    FCursorList: PCursorRec;
    FDefaultCursor: HCURSOR;
    FActiveControl: TWinControl;
    FActiveCustomForm: TCustomForm;
    FActiveForm: TForm;
    FLastActiveControl: TWinControl;
    FLastActiveCustomForm: TCustomForm;
    FFocusedForm: TCustomForm;
    FSaveFocusedList: TList;
    FHintFont: TFont;
    FIconFont: TFont;
    FMenuFont: TFont;
    FAlignLevel: Word;
    FControlState: TControlState;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    procedure AlignForm(AForm: TCustomForm);
    procedure AlignForms(AForm: TCustomForm; var Rect: TRect);
    procedure AddDataModule(DataModule: TDataModule);
    procedure AddForm(AForm: TCustomForm);
    procedure ClearMonitors;
    procedure CreateCursors;
    procedure DeleteCursor(Index: Integer);
    procedure DestroyCursors;
    function FindMonitor(Handle: HMONITOR): TMonitor;
    procedure IconFontChanged(Sender: TObject);
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
    procedure GetMonitors;
    function GetFonts: TStrings;
    function GetForm(Index: Integer): TForm;
    function GetFormCount: Integer;
    procedure GetMetricSettings;
    function GetWidth: Integer;
    procedure InsertCursor(Index: Integer; Handle: HCURSOR);
    procedure RemoveDataModule(DataModule: TDataModule);
    procedure RemoveForm(AForm: TCustomForm);
    procedure SetCursors(Index: Integer; Handle: HCURSOR);
    procedure SetCursor(Value: TCursor);
    procedure SetHintFont(Value: TFont);
    procedure SetIconFont(Value: TFont);
    procedure SetMenuFont(Value: TFont);
    procedure UpdateLastActive;
    function GetPrimaryMonitor: TMonitor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisableAlign;
    procedure EnableAlign;
    function MonitorFromPoint(const Point: TPoint;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    function MonitorFromRect(const Rect: TRect;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    function MonitorFromWindow(const Handle: THandle;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    procedure Realign;
    procedure ResetFonts;
    property ActiveControl: TWinControl read FActiveControl;
    property ActiveCustomForm: TCustomForm read FActiveCustomForm;
    property ActiveForm: TForm read FActiveForm;
    property CustomFormCount: Integer read GetCustomFormCount;
    property CustomForms[Index: Integer]: TCustomForm read GetCustomForms;
    property CursorCount: Integer read FCursorCount;
    property Cursor: TCursor read FCursor write SetCursor;
    property Cursors[Index: Integer]: HCURSOR read GetCursors write SetCursors;
    property DataModules[Index: Integer]: TDataModule read GetDataModule;
    property DataModuleCount: Integer read GetDataModuleCount;
    property FocusedForm: TCustomForm read FFocusedForm write FFocusedForm;
    property SaveFocusedList: TList read FSaveFocusedList;
    property MonitorCount: Integer read GetMonitorCount;
    property Monitors[Index: Integer]: TMonitor read GetMonitor;
    property DesktopRect: TRect read GetDesktopRect;
    property DesktopHeight: Integer read GetDesktopHeight;
    property DesktopLeft: Integer read GetDesktopLeft;
    property DesktopTop: Integer read GetDesktopTop;
    property DesktopWidth: Integer read GetDesktopWidth;
    property WorkAreaRect: TRect read GetWorkAreaRect;
    property WorkAreaHeight: Integer read GetWorkAreaHeight;
    property WorkAreaLeft: Integer read GetWorkAreaLeft;
    property WorkAreaTop: Integer read GetWorkAreaTop;
    property WorkAreaWidth: Integer read GetWorkAreaWidth;
    property HintFont: TFont read FHintFont write SetHintFont;
    property IconFont: TFont read FIconFont write SetIconFont;
    property MenuFont: TFont read FMenuFont write SetMenuFont;
    property Fonts: TStrings read GetFonts;
    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TForm read GetForm;
    property Imes: TStrings read GetImes;
    property DefaultIme: string read GetDefaultIme;
    property DefaultKbLayout: HKL read FDefaultKbLayout;
    property Height: Integer read GetHeight;
    property PixelsPerInch: Integer read FPixelsPerInch;
    property PrimaryMonitor: TMonitor read GetPrimaryMonitor;
    property Width: Integer read GetWidth;
    property OnActiveControlChange: TNotifyEvent
      read FOnActiveControlChange write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent
      read FOnActiveFormChange write FOnActiveFormChange;
  end;

{ TApplication }

  TTimerMode = (tmShow, tmHide);

  PHintInfo = ^THintInfo;
  THintInfo = record
    HintControl: TControl;
    HintWindowClass: THintWindowClass;
    HintPos: TPoint;
    HintMaxWidth: Integer;
    HintColor: TColor;
    CursorRect: TRect;
    CursorPos: TPoint;
    ReshowTimeout: Integer;
    HideTimeout: Integer;
    HintStr: string;
    HintData: Pointer;
  end;

  TCMHintShow = record
    Msg: Cardinal;
    Reserved: Integer;
    HintInfo: PHintInfo;
    Result: Integer;
  end;

  TCMHintShowPause = record
    Msg: Cardinal;
    WasActive: Integer;
    Pause: PInteger;
    Result: Integer;
  end;

  TPopupForm = record
    FormID: Integer;
    Form: TCustomForm;
    WasPopup: Boolean;
  end;
  TPopupFormArray = array of TPopupForm;

  TMessageEvent = procedure (var Msg: TMsg; var Handled: Boolean) of object;
  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
  TGetHandleEvent = procedure(var Handle: HWND) of object;
  TIdleEvent = procedure (Sender: TObject; var Done: Boolean) of object;
  TShowHintEvent = procedure (var HintStr: string; var CanShow: Boolean;
    var HintInfo: THintInfo) of object;
  TWindowHook = function (var Message: TMessage): Boolean of object;
  TSettingChangeEvent = procedure (Sender: TObject; Flag: Integer; const Section: string; var Result: Longint) of object;

  TApplication = class(TComponent)
  private
    FHandle: HWnd;
    FBiDiMode: TBiDiMode;
    FBiDiKeyboard: string;
    FNonBiDiKeyboard: string;
    FObjectInstance: Pointer;
    FMainForm: TForm;
    FMouseControl: TControl;
    FHelpSystem : IHelpSystem;
    FHelpFile: string;
    FHint: string;
    FHintActive: Boolean;
    FUpdateFormatSettings: Boolean;
    FUpdateMetricSettings: Boolean;
    FShowMainForm: Boolean;
    FHintColor: TColor;
    FHintControl: TControl;
    FHintCursorRect: TRect;
    FHintHidePause: Integer;
    FHintPause: Integer;
    FHintShortCuts: Boolean;
    FHintShortPause: Integer;
    FHintWindow: THintWindow;
    FShowHint: Boolean;
    FTimerMode: TTimerMode;
    FTimerHandle: Word;
    FTitle: string;
    FTopMostList: TList;
    FTopMostLevel: Integer;
    FPopupOwners: TList;
    FPopupLevel: Integer;
    FIcon: TIcon;
    FTerminate: Boolean;
    FActive: Boolean;
    FAllowTesting: Boolean;
    FTestLib: THandle;
    FHandleCreated: Boolean;
    FRunning: Boolean;
    FWindowHooks: TList;
    FWindowList: Pointer;
    FDialogHandle: HWnd;
    FAutoDragDocking: Boolean;
    FActionUpdateDelay: Integer;
    FModalLevel: Integer;
    FPopupControlWnd: HWnd;
    FCurrentPopupID: Integer;
    FPopupForms: TPopupFormArray;
    FModalPopupMode: TPopupMode;
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnException: TExceptionEvent;
    FOnGetActiveFormHandle: TGetHandleEvent;
    FOnGetMainFormHandle: TGetHandleEvent;
    FOnMessage: TMessageEvent;
    FOnModalBegin: TNotifyEvent;
    FOnModalEnd: TNotifyEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnIdle: TIdleEvent;
    FOnDeactivate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShortCut: TShortCutEvent;
    FOnShowHint: TShowHintEvent;
    FOnSettingChange: TSettingChangeEvent;
    function CheckIniChange(var Message: TMessage): Boolean;
    function DispatchAction(Msg: Longint; Action: TBasicAction): Boolean;
    procedure DoActionIdle;
    function DoMouseIdle: TControl;
    procedure DoNormalizeTopMosts(IncludeMain: Boolean);
    function DoOnHelp(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean;
    procedure DoShowOwnedPopups(Show: Boolean);
    function GetCurrentHelpFile: string;
    function GetDialogHandle: HWND;
    function GetActiveFormHandle: HWND;
    function GetMainFormHandle: HWND;
    function GetExeName: string;
    function GetIconHandle: HICON;
    function GetTitle: string;
    procedure HintTimerExpired;
    procedure IconChanged(Sender: TObject);
    function InvokeHelp(Command: Word; Data: Longint): Boolean;
    procedure NotifyForms(Msg: Word);
    procedure PopupControlProc(var Message: TMessage);
    function ProcessMessage(var Msg: TMsg): Boolean;
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetDialogHandle(Value: HWnd);
    procedure SetHandle(Value: HWnd);
    procedure SetHint(const Value: string);
    procedure SetHintColor(Value: TColor);
    procedure SetIcon(Value: TIcon);
    procedure SetShowHint(Value: Boolean);
    procedure SetTitle(const Value: string);
    procedure SettingChange(var Message: TWMSettingChange);
    procedure StartHintTimer(Value: Integer; TimerMode: TTimerMode);
    procedure StopHintTimer;
    procedure WndProc(var Message: TMessage);
    procedure UpdateVisible;
    function  ValidateHelpSystem: Boolean;
    procedure WakeMainThread(Sender: TObject);
  protected
    procedure Idle(const Msg: TMsg);
    function IsDlgMsg(var Msg: TMsg): Boolean;
    function IsHintMsg(var Msg: TMsg): Boolean;
    function IsKeyMsg(var Msg: TMsg): Boolean;
    function IsMDIMsg(var Msg: TMsg): Boolean;
    function IsShortCut(var Message: TWMKey): Boolean;
    function IsPreProcessMessage(var Msg: TMsg): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(CursorPos: TPoint);
    function AddPopupForm(APopupForm: TCustomForm): Integer;
    procedure BringToFront;
    procedure ControlDestroyed(Control: TControl);
    procedure CancelHint;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure CreateHandle;
    procedure DoApplicationIdle;
    function ExecuteAction(Action: TBasicAction): Boolean; reintroduce;
    procedure HandleException(Sender: TObject);
    procedure HandleMessage;
    function HelpCommand(Command: Integer; Data: Longint): Boolean;
    function HelpContext(Context: THelpContext): Boolean;
    function HelpJump(const JumpID: string): Boolean;
    function HelpKeyword(const Keyword: string): Boolean;
    function HelpShowTableOfContents: Boolean;
    procedure HideHint;
    procedure HintMouseMessage(Control: TControl; var Message: TMessage);
    procedure HookMainWindow(Hook: TWindowHook);
    procedure HookSynchronizeWakeup;
    procedure Initialize;
    function IsRightToLeft: Boolean;
    function MessageBox(const Text, Caption: PChar; Flags: Longint = MB_OK): Integer;
    procedure Minimize;
    procedure ModalStarted;
    procedure ModalFinished;
    procedure NormalizeAllTopMosts;
    procedure NormalizeTopMosts;
    procedure ProcessMessages;
    procedure RemovePopupForm(APopupForm: TCustomForm);
    procedure Restore;
    procedure RestoreTopMosts;
    procedure Run;
    procedure ShowException(E: Exception);
    procedure Terminate;
    procedure UnhookMainWindow(Hook: TWindowHook);
    procedure UnhookSynchronizeWakeup;
    function UpdateAction(Action: TBasicAction): Boolean; reintroduce;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    function UseRightToLeftScrollBar: Boolean;
    property Active: Boolean read FActive;
    property ActionUpdateDelay: Integer read FActionUpdateDelay write FActionUpdateDelay default 0;
    property ActiveFormHandle: HWND read GetActiveFormHandle;
    property AllowTesting: Boolean read FAllowTesting write FAllowTesting;
    property AutoDragDocking: Boolean read FAutoDragDocking write FAutoDragDocking default True;
    property HelpSystem: IHelpSystem read FHelpSystem;
    property CurrentHelpFile: string read GetCurrentHelpFile;
    property DialogHandle: HWnd read GetDialogHandle write SetDialogHandle;
    property ExeName: string read GetExeName;
    property Handle: HWnd read FHandle write SetHandle;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Hint: string read FHint write SetHint;
    property HintColor: TColor read FHintColor write SetHintColor;
    property HintHidePause: Integer read FHintHidePause write FHintHidePause;
    property HintPause: Integer read FHintPause write FHintPause;
    property HintShortCuts: Boolean read FHintShortCuts write FHintShortCuts;
    property HintShortPause: Integer read FHintShortPause write FHintShortPause;
    property Icon: TIcon read FIcon write SetIcon;
    property MainForm: TForm read FMainForm;
    property MainFormHandle: HWND read GetMainFormHandle;
    property ModalLevel: Integer read FModalLevel;
    property ModalPopupMode: TPopupMode read FModalPopupMode write FModalPopupMode default pmNone;
    property BiDiMode: TBiDiMode read FBiDiMode
      write SetBiDiMode default bdLeftToRight;
    property BiDiKeyboard: string read FBiDiKeyboard write FBiDiKeyboard;
    property NonBiDiKeyboard: string read FNonBiDiKeyboard write FNonBiDiKeyboard;
    property PopupControlWnd: HWND read FPopupControlWnd;
    property ShowHint: Boolean read FShowHint write SetShowHint;
    property ShowMainForm: Boolean read FShowMainForm write FShowMainForm;
    property Terminated: Boolean read FTerminate;
    property Title: string read GetTitle write SetTitle;
    property UpdateFormatSettings: Boolean read FUpdateFormatSettings
      write FUpdateFormatSettings;
    property UpdateMetricSettings: Boolean read FUpdateMetricSettings
      write FUpdateMetricSettings;
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnGetActiveFormHandle: TGetHandleEvent read FOnGetActiveFormHandle
      write FOnGetActiveFormHandle;
    property OnGetMainFormHandle: TGetHandleEvent read FOnGetMainFormHandle
      write FOnGetMainFormHandle;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnModalBegin: TNotifyEvent read FOnModalBegin write FOnModalBegin;
    property OnModalEnd: TNotifyEvent read FOnModalEnd write FOnModalEnd;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
    property OnSettingChange: TSettingChangeEvent read FOnSettingChange write FOnSettingChange;
  end;

{ Global objects }

var
  Application: TApplication;
  Screen: TScreen;
  Ctl3DBtnWndProc: Pointer = nil;  { obsolete }
  Ctl3DDlgFramePaint: function(Window: HWnd; Msg, wParam, lParam: Longint): Longint stdcall = nil; { obsolete }
  Ctl3DCtlColorEx: function(Window: HWnd; Msg, wParam, lParam: Longint): Longint stdcall = nil; { obsolete }
  HintWindowClass: THintWindowClass = THintWindow;

function GetParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;
function ValidParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;

function DisableTaskWindows(ActiveWindow: HWnd): Pointer;
procedure EnableTaskWindows(WindowList: Pointer);

function MakeObjectInstance(Method: TWndMethod): Pointer; deprecated; { moved to Classes.pas }
{$EXTERNALSYM MakeObjectInstance}
procedure FreeObjectInstance(ObjectInstance: Pointer);    deprecated; { moved to Classes.pas }
{$EXTERNALSYM FreeObjectInstance}

function IsAccel(VK: Word; const Str: string): Boolean;

function  Subclass3DWnd(Wnd: HWnd): Boolean;     deprecated;  { obsolete }
procedure Subclass3DDlg(Wnd: HWnd; Flags: Word); deprecated;  { obsolete }
procedure SetAutoSubClass(Enable: Boolean);      deprecated;  { obsolete }
function AllocateHWnd(Method: TWndMethod): HWND; deprecated;  { moved to Classes.pas }
{$EXTERNALSYM AllocateHWnd}
procedure DeallocateHWnd(Wnd: HWND);             deprecated;  { moved to Classes.pas }
{$EXTERNALSYM DeallocateHWnd}
procedure DoneCtl3D;                             deprecated;  { obsolete }
procedure InitCtl3D;                             deprecated;  { obsolete }

function KeysToShiftState(Keys: Word): TShiftState;
function KeyDataToShiftState(KeyData: Longint): TShiftState;
function KeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState; overload;
function KeyboardStateToShiftState: TShiftState; overload;

function ForegroundTask: Boolean;

type
  TFocusState = type Pointer;

function SaveFocusState: TFocusState;
procedure RestoreFocusState(FocusState: TFocusState);

type
  TSetLayeredWindowAttributes = function (Hwnd: THandle; crKey: COLORREF; bAlpha: Byte; dwFlags: DWORD): Boolean; stdcall;

var
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;

type

(*$HPPEMIT 'namespace Forms { '*)
(*$HPPEMIT 'class DELPHICLASS TGlassFrame;'*)
(*$HPPEMIT 'class DELPHICLASS TCustomForm;'*)
(*$HPPEMIT 'class DELPHICLASS TApplication;'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall SetCustomFormGlassFrame(const TCustomForm* CustomForm, const TGlassFrame* GlassFrame);'*)
(*$HPPEMIT 'extern PACKAGE TGlassFrame* __fastcall GetCustomFormGlassFrame(const TCustomForm* CustomForm);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall SetApplicationMainFormOnTaskBar(const TApplication* Application, bool Value);'*)
(*$HPPEMIT 'extern PACKAGE bool __fastcall GetApplicationMainFormOnTaskBar(const TApplication* Application);'*)
(*$HPPEMIT '}'*)

  TGlassFrame = class(TPersistent)
  private
    FClient: TCustomForm;
    FEnabled: Boolean;
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    FOnChange: TNotifyEvent;
    FSheetOfGlass: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetExtendedFrame(Index: Integer; Value: Integer);
    procedure SetSheetOfGlass(Value: Boolean);
  protected
    procedure Change; virtual;
  public
    constructor Create(Client: TCustomForm);
    procedure Assign(Source: TPersistent); override;
    function FrameExtended: Boolean;
    function IntersectsControl(Control: TControl): Boolean;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Left: Integer index 0 read FLeft write SetExtendedFrame default 0;
    property Top: Integer index 1 read FTop write SetExtendedFrame default 0;
    property Right: Integer index 2 read FRight write SetExtendedFrame default 0;
    property Bottom: Integer index 3 read FBottom write SetExtendedFrame default 0;
    property SheetOfGlass: Boolean read FSheetOfGlass write SetSheetOfGlass default False;
  end;

implementation
