{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{  Copyright (c) 1996-2004 Borland Software Corporation }
{                                                       }
{*******************************************************}

unit ComCtrls;

{$R-,T-,H+,X+}

interface

uses
  {$IFDEF LINUX}
  WinUtils,
  {$ENDIF}
  Messages, Windows, SysUtils, CommCtrl, Classes, Controls, Forms, Menus,
  Graphics, StdCtrls, RichEdit, ToolWin, ImgList, ExtCtrls, ListActns,
  ShlObj;

type
  THitTest = (htAbove, htBelow, htNowhere, htOnItem, htOnButton, htOnIcon,
    htOnIndent, htOnLabel, htOnRight, htOnStateIcon, htToLeft, htToRight);
  THitTests = set of THitTest;

  TCustomTabControl = class;

  TTabChangingEvent = procedure(Sender: TObject;
    var AllowChange: Boolean) of object;

  TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);

  TTabStyle = (tsTabs, tsButtons, tsFlatButtons);

  TDrawTabEvent = procedure(Control: TCustomTabControl; TabIndex: Integer;
    const Rect: TRect; Active: Boolean) of object;
  TTabGetImageEvent = procedure(Sender: TObject; TabIndex: Integer;
    var ImageIndex: Integer) of object;

  TCustomTabControl = class(TWinControl)
  private
    FCanvas: TCanvas;
    FHotTrack: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FMultiLine: Boolean;
    FMultiSelect: Boolean;
    FOwnerDraw: Boolean;
    FRaggedRight: Boolean;
    FSaveTabIndex: Integer;
    FSaveTabs: TStringList;
    FScrollOpposite: Boolean;
    FStyle: TTabStyle;
    FTabPosition: TTabPosition;
    FTabs: TStrings;
    FTabSize: TSmallPoint;
    FUpdating: Boolean;
    FSavedAdjustRect: TRect;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOnDrawTab: TDrawTabEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    function GetDisplayRect: TRect;
    function GetTabIndex: Integer;
    procedure ImageListChange(Sender: TObject);
    function InternalSetMultiLine(Value: Boolean): Boolean;
    procedure SetHotTrack(Value: Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetMultiLine(Value: Boolean);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetRaggedRight(Value: Boolean);
    procedure SetScrollOpposite(Value: Boolean);
    procedure SetStyle(Value: TTabStyle);
    procedure SetTabHeight(Value: Smallint);
    procedure SetTabPosition(Value: TTabPosition);
    procedure SetTabs(Value: TStrings);
    procedure SetTabWidth(Value: Smallint);
    procedure TabsChanged;
    procedure UpdateTabSize;
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTabStopChanged(var Message: TMessage); message CM_TABSTOPCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure TCMAdjustRect(var Message: TMessage); message TCM_ADJUSTRECT;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMNotifyFormat(var Message: TMessage); message WM_NOTIFYFORMAT;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    function CanChange: Boolean; dynamic;
    function CanShowTab(TabIndex: Integer): Boolean; virtual;
    procedure Change; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); virtual;
    function GetImageIndex(TabIndex: Integer): Integer; virtual;
    procedure Loaded; override;
    procedure UpdateTabImages;
    property DisplayRect: TRect read GetDisplayRect;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property Images: TCustomImageList read FImages write SetImages;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetTabIndex(Value: Integer); virtual;    
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property RaggedRight: Boolean read FRaggedRight write SetRaggedRight default False;
    property ScrollOpposite: Boolean read FScrollOpposite
      write SetScrollOpposite default False;
    property Style: TTabStyle read FStyle write SetStyle default tsTabs;
    property TabHeight: Smallint read FTabSize.Y write SetTabHeight default 0;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition
      default tpTop;
    property Tabs: TStrings read FTabs write SetTabs;
    property TabWidth: Smallint read FTabSize.X write SetTabWidth default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex write FOnGetImageIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOfTabAt(X, Y: Integer): Integer;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function TabRect(Index: Integer): TRect;
    function RowCount: Integer;
    procedure ScrollTabs(Delta: Integer);
    property Canvas: TCanvas read FCanvas;
    property TabStop default True;
  end;

  TTabControl = class(TCustomTabControl)
  public
    property DisplayRect;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
    property MultiLine;
    property MultiSelect;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property Style;
    property TabHeight;
    property TabOrder;
    property TabPosition;
    property Tabs;
    property TabIndex;  // must be after Tabs
    property TabStop;
    property TabWidth;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TPageControl = class;

  TTabSheet = class(TWinControl)
  private
    FImageIndex: TImageIndex;
    FPageControl: TPageControl;
    FTabVisible: Boolean;
    FTabShowing: Boolean;
    FHighlighted: Boolean;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetPageIndex: Integer;
    function GetTabIndex: Integer;
    procedure SetHighlighted(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetPageControl(APageControl: TPageControl);
    procedure SetPageIndex(Value: Integer);
    procedure SetTabShowing(Value: Boolean);
    procedure SetTabVisible(Value: Boolean);
    procedure UpdateTabShowing;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMPrintClient(var Message: TWMPrintClient); message WM_PRINTCLIENT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageControl: TPageControl read FPageControl write SetPageControl;
    property TabIndex: Integer read GetTabIndex;
  published
    property BorderWidth;
    property Caption;
    property DragMode;
    property Enabled;
    property Font;
    property Height stored False;
    property Highlighted: Boolean read FHighlighted write SetHighlighted default False;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default 0;
    property Left stored False;
    property Constraints;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnStartDrag;
  end;

  TPageControl = class(TCustomTabControl)
  private
    FPages: TList;
    FActivePage: TTabSheet;
    FNewDockSheet: TTabSheet;
    FUndockingPage: TTabSheet;
    FInSetActivePage: Boolean;
    procedure ChangeActivePage(Page: TTabSheet);
    procedure DeleteTab(Page: TTabSheet; Index: Integer);
    function GetActivePageIndex: Integer;
    function GetDockClientFromMousePos(MousePos: TPoint): TControl;
    function GetPage(Index: Integer): TTabSheet;
    function GetPageCount: Integer;
    procedure InsertPage(Page: TTabSheet);
    procedure InsertTab(Page: TTabSheet);
    procedure MoveTab(CurIndex, NewIndex: Integer);
    procedure RemovePage(Page: TTabSheet);
    procedure SetActivePageIndex(const Value: Integer);
    procedure UpdateTab(Page: TTabSheet);
    procedure UpdateTabHighlights;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
  protected
    function CanShowTab(TabIndex: Integer): Boolean; override;
    procedure Change; override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetImageIndex(TabIndex: Integer): Integer; override;
    function GetPageFromDockClient(Client: TControl): TTabSheet;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure Loaded; override;
    procedure SetActivePage(Page: TTabSheet);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetTabIndex(Value: Integer); override;
    procedure ShowControl(AControl: TControl); override;
    procedure UpdateActivePage; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindNextPage(CurPage: TTabSheet;
      GoForward, CheckTabVisible: Boolean): TTabSheet;
    procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
    property ActivePageIndex: Integer read GetActivePageIndex
      write SetActivePageIndex;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TTabSheet read GetPage;
  published
    property ActivePage: TTabSheet read FActivePage write SetActivePage;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
    property MultiLine;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property Style;
    property TabHeight;
    property TabIndex stored False;
    property TabOrder;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ TCustomStatusBar }

  TCustomStatusBar = class;
  TStatusPanel = class;
  TStatusPanels = class;
  TStatusPanelStyle = (psText, psOwnerDraw);
  TStatusPanelBevel = (pbNone, pbLowered, pbRaised);
  TStatusPanelClass = class of TStatusPanel;

  TStatusPanel = class(TCollectionItem)
  private
    FText: string;
    FWidth: Integer;
    FAlignment: TAlignment;
    FBevel: TStatusPanelBevel;
    FBiDiMode: TBiDiMode;
    FParentBiDiMode: Boolean;
    FStyle: TStatusPanelStyle;
    FUpdateNeeded: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevel(Value: TStatusPanelBevel);
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure SetStyle(Value: TStatusPanelStyle);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    function IsBiDiModeStored: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure ParentBiDiModeChanged;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Bevel: TStatusPanelBevel read FBevel write SetBevel default pbLowered;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property Style: TStatusPanelStyle read FStyle write SetStyle default psText;
    property Text: string read FText write SetText;
    property Width: Integer read FWidth write SetWidth;
  end;

  TStatusPanels = class(TCollection)
  private
    FStatusBar: TCustomStatusBar;
    function GetItem(Index: Integer): TStatusPanel;
    procedure SetItem(Index: Integer; Value: TStatusPanel);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(StatusBar: TCustomStatusBar);
    function Add: TStatusPanel;
    function AddItem(Item: TStatusPanel; Index: Integer): TStatusPanel;
    function Insert(Index: Integer): TStatusPanel;
    property Items[Index: Integer]: TStatusPanel read GetItem write SetItem; default;
  end;

  TCustomDrawPanelEvent = procedure(StatusBar: TCustomStatusBar; Panel: TStatusPanel;
    const Rect: TRect) of object;
  TSBCreatePanelClassEvent = procedure(Sender: TCustomStatusBar;
    var PanelClass: TStatusPanelClass) of object;

  TCustomStatusBar = class(TWinControl)
  private
    FPanels: TStatusPanels;
    FCanvas: TCanvas;
    FSimpleText: string;
    FSimplePanel: Boolean;
    FSizeGrip, FSizeGripValid: Boolean;
    FUseSystemFont: Boolean;
    FAutoHint: Boolean;
    FOnDrawPanel: TCustomDrawPanelEvent;
    FOnHint: TNotifyEvent;
    FOnCreatePanelClass: TSBCreatePanelClassEvent;
    procedure DoRightToLeftAlignment(var Str: string; AAlignment: TAlignment;
      ARTLAlignment: Boolean);
    procedure SetPanels(Value: TStatusPanels);
    procedure SetSimplePanel(Value: Boolean);
    procedure UpdateSimpleText;
    procedure SetSimpleText(const Value: string);
    procedure SetSizeGrip(Value: Boolean);
    procedure SyncToSystemFont;
    procedure UpdatePanel(Index: Integer; Repaint: Boolean);
    procedure UpdatePanels(UpdateRects, UpdateText: Boolean);
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMWinIniChange(var Message: TMessage); message CM_WININICHANGE;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetUseSystemFont(const Value: Boolean);
    procedure ValidateSizeGrip(ARecreate: Boolean);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreatePanel: TStatusPanel; virtual;
    function CreatePanels: TStatusPanels; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function DoHint: Boolean; virtual;
    procedure DrawPanel(Panel: TStatusPanel; const Rect: TRect); dynamic;
    function GetPanelClass: TStatusPanelClass; virtual;
    function IsFontStored: Boolean;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Canvas: TCanvas read FCanvas;
    property AutoHint: Boolean read FAutoHint write FAutoHint;
    property Panels: TStatusPanels read FPanels write SetPanels;
    property SimplePanel: Boolean read FSimplePanel write SetSimplePanel;
    property SimpleText: string read FSimpleText write SetSimpleText;
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip;
    property UseSystemFont: Boolean read FUseSystemFont write SetUseSystemFont;
    property OnCreatePanelClass: TSBCreatePanelClassEvent read FOnCreatePanelClass
      write FOnCreatePanelClass;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnDrawPanel: TCustomDrawPanelEvent read FOnDrawPanel write FOnDrawPanel;
  end;

{ TStatusBar }

  TStatusBar = class;

  TDrawPanelEvent = procedure(StatusBar: TStatusBar; Panel: TStatusPanel;
    const Rect: TRect) of object;

  TStatusBar = class(TCustomStatusBar)
  private
    function GetOnDrawPanel: TDrawPanelEvent;
    procedure SetOnDrawPanel(const Value: TDrawPanelEvent);
  published
    property Action;
    property AutoHint default False;
    property Align default alBottom;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property Color default clBtnFace;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font stored IsFontStored;
    property Constraints;
    property Panels;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SimplePanel default False;
    property SimpleText;
    property SizeGrip default True;
    property UseSystemFont default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnCreatePanelClass;    
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnHint;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // Required for backwards compatibility with the old event signature
    property OnDrawPanel: TDrawPanelEvent read GetOnDrawPanel write SetOnDrawPanel;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ Custom draw }

  TCustomDrawTarget = (dtControl, dtItem, dtSubItem);
  TCustomDrawStage = (cdPrePaint, cdPostPaint, cdPreErase, cdPostErase);
  TCustomDrawState = set of (cdsSelected, cdsGrayed, cdsDisabled, cdsChecked,
    cdsFocused, cdsDefault, cdsHot, cdsMarked, cdsIndeterminate);

{ TCustomHeaderControl }

  TCustomHeaderControl = class;
  
{ THeaderControl }

  THeaderControl = class;
  THeaderSection = class;

  THeaderSectionStyle = (hsText, hsOwnerDraw);
  THeaderSectionClass = class of THeaderSection;

  THeaderSection = class(TCollectionItem)
  private
    FText: string;
    FWidth: Integer;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FAlignment: TAlignment;
    FStyle: THeaderSectionStyle;
    FAllowClick: Boolean;
    FAutoSize: Boolean;
    FImageIndex: TImageIndex;
    FBiDiMode: TBiDiMode;
    FParentBiDiMode: Boolean;
    function GetLeft: Integer;
    function GetRight: Integer;
    function IsBiDiModeStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSize(Value: Boolean);
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure SetStyle(Value: THeaderSectionStyle);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetImageIndex(const Value: TImageIndex);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure ParentBiDiModeChanged;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    property Left: Integer read GetLeft;
    property Right: Integer read GetRight;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AllowClick: Boolean read FAllowClick write FAllowClick default True;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 10000;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property Style: THeaderSectionStyle read FStyle write SetStyle default hsText;
    property Text: string read FText write SetText;
    property Width: Integer read FWidth write SetWidth;
  end;

  THeaderSections = class(TCollection)
  private
    FHeaderControl: TCustomHeaderControl;
    function GetItem(Index: Integer): THeaderSection;
    procedure SetItem(Index: Integer; Value: THeaderSection);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(HeaderControl: TCustomHeaderControl);
    function Add: THeaderSection;
    function AddItem(Item: THeaderSection; Index: Integer): THeaderSection;
    function Insert(Index: Integer): THeaderSection;
    property Items[Index: Integer]: THeaderSection read GetItem write SetItem; default;
  end;

  TSectionTrackState = (tsTrackBegin, tsTrackMove, tsTrackEnd);

  TCustomDrawSectionEvent = procedure(HeaderControl: TCustomHeaderControl;
    Section: THeaderSection; const Rect: TRect; Pressed: Boolean) of object;
  TCustomSectionNotifyEvent = procedure(HeaderControl: TCustomHeaderControl;
    Section: THeaderSection) of object;
  TCustomSectionTrackEvent = procedure(HeaderControl: TCustomHeaderControl;
    Section: THeaderSection; Width: Integer;
    State: TSectionTrackState) of object;
  TSectionDragEvent = procedure (Sender: TObject; FromSection, ToSection: THeaderSection;
    var AllowDrag: Boolean) of object;
  TCustomHCCreateSectionClassEvent = procedure(Sender: TCustomHeaderControl;
    var SectionClass: THeaderSectionClass) of object;

  THeaderStyle = (hsButtons, hsFlat);

  TCustomHeaderControl = class(TWinControl)
  private
    FSections: THeaderSections;
    FSectionStream: TMemoryStream;
    FUpdatingSectionOrder,
    FSectionDragged: Boolean;
    FCanvas: TCanvas;
    FFromIndex,
    FToIndex: Integer;
    FFullDrag: Boolean;
    FHotTrack: Boolean;
    FDragReorder: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FStyle: THeaderStyle;
    FTrackSection: THeaderSection;
    FTrackWidth: Integer;
    FTrackPos: TPoint;
    FOnDrawSection: TCustomDrawSectionEvent;
    FOnSectionClick: TCustomSectionNotifyEvent;
    FOnSectionResize: TCustomSectionNotifyEvent;
    FOnSectionTrack: TCustomSectionTrackEvent;
    FOnSectionDrag: TSectionDragEvent;
    FOnSectionEndDrag: TNotifyEvent;
    FOnCreateSectionClass: TCustomHCCreateSectionClassEvent;
    function  DoSectionDrag(FromSection, ToSection: THeaderSection): Boolean;
    procedure DoSectionEndDrag;
    procedure ImageListChange(Sender: TObject);
    procedure SetDragReorder(const Value: Boolean);
    procedure SetFullDrag(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetSections(Value: THeaderSections);
    procedure SetStyle(Value: THeaderStyle);
    procedure UpdateItem(Message, Index: Integer);
    procedure UpdateSection(Index: Integer);
    procedure UpdateSections;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    function CreateSection: THeaderSection; virtual;
    function CreateSections: THeaderSections; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DrawSection(Section: THeaderSection; const Rect: TRect;
      Pressed: Boolean); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SectionClick(Section: THeaderSection); dynamic;
    procedure SectionDrag(FromSection, ToSection: THeaderSection; var AllowDrag: Boolean); dynamic;
    procedure SectionEndDrag; dynamic;
    procedure SectionResize(Section: THeaderSection); dynamic;
    procedure SectionTrack(Section: THeaderSection; Width: Integer;
      State: TSectionTrackState); dynamic;
    procedure SetImages(Value: TCustomImageList); virtual;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
    procedure FlipChildren(AllLevels: Boolean); override;
  published
    property Align default alTop;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DragReorder: Boolean read FDragReorder write SetDragReorder default False;
    property FullDrag: Boolean read FFullDrag write SetFullDrag;
    property HotTrack: Boolean read FHotTrack write SetHotTrack;
    property Enabled;
    property Font;
    property Images: TCustomImageList read FImages write SetImages;
    property Constraints;
    property Sections: THeaderSections read FSections write SetSections;
    property Style: THeaderStyle read FStyle write SetStyle;
    property OnCreateSectionClass: TCustomHCCreateSectionClassEvent read FOnCreateSectionClass
      write FOnCreateSectionClass;
    property OnDrawSection: TCustomDrawSectionEvent read FOnDrawSection write FOnDrawSection;
    property OnSectionClick: TCustomSectionNotifyEvent read FOnSectionClick
      write FOnSectionClick;
    property OnSectionDrag: TSectionDragEvent read FOnSectionDrag
      write FOnSectionDrag;
    property OnSectionEndDrag: TNotifyEvent read FOnSectionEndDrag
      write FOnSectionEndDrag;
    property OnSectionResize: TCustomSectionNotifyEvent read FOnSectionResize
      write FOnSectionResize;
    property OnSectionTrack: TCustomSectionTrackEvent read FOnSectionTrack
      write FOnSectionTrack;
  end;

{ THeaderControl }

  TDrawSectionEvent = procedure(HeaderControl: THeaderControl;
    Section: THeaderSection; const Rect: TRect; Pressed: Boolean) of object;
  TSectionNotifyEvent = procedure(HeaderControl: THeaderControl;
    Section: THeaderSection) of object;
  TSectionTrackEvent = procedure(HeaderControl: THeaderControl;
    Section: THeaderSection; Width: Integer;
    State: TSectionTrackState) of object;
  THCCreateSectionClassEvent = procedure(Sender: THeaderControl;
    var SectionClass: THeaderSectionClass) of object;

  THeaderControl = class(TCustomHeaderControl)
  private
    function GetOnDrawSection: TDrawSectionEvent;
    function GetOnSectionClick: TSectionNotifyEvent;
    function GetOnSectionResize: TSectionNotifyEvent;
    function GetOnSectionTrack: TSectionTrackEvent;
    procedure SetOnDrawSection(const Value: TDrawSectionEvent);
    procedure SetOnSectionClick(const Value: TSectionNotifyEvent);
    procedure SetOnSectionResize(const Value: TSectionNotifyEvent);
    procedure SetOnSectionTrack(const Value: TSectionTrackEvent);
  published
    property Align default alTop;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DragReorder;
    property Enabled;
    property Font;
    property FullDrag default True;
    property HotTrack default False;
    property Images;
    property Constraints;
    property Sections;
    property ShowHint;
    property Style default hsButtons;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property OnContextPopup;
    property OnCreateSectionClass;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    // Required for backwards compatibility with the old events
    property OnDrawSection: TDrawSectionEvent read GetOnDrawSection write SetOnDrawSection;
    property OnSectionClick: TSectionNotifyEvent read GetOnSectionClick
      write SetOnSectionClick;
    property OnSectionResize: TSectionNotifyEvent read GetOnSectionResize
      write SetOnSectionResize;
    property OnSectionTrack: TSectionTrackEvent read GetOnSectionTrack
      write SetOnSectionTrack;
    property OnSectionDrag;
    property OnSectionEndDrag;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TTreeNode }

  TCustomTreeView = class;
  TTreeNode = class;
  TTreeNodes = class;

  TNodeState = (nsCut, nsDropHilited, nsFocused, nsSelected, nsExpanded);
  TNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert);
  TAddMode = (taAddFirst, taAdd, taInsert);

  PNodeInfo = ^TNodeInfo;
  TNodeInfo = packed record
    ImageIndex: Integer;
    SelectedIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    Data: Pointer;
    Count: Integer;
    Text: string[255];
  end;

  TNodeDataInfo = packed record
    ImageIndex: Integer;
    SelectedIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    Data: Pointer;
    Count: Integer;
    TextLen: Byte;
    // WideString Text of TextLen chars follows
  end;

  TTreeNodeClass = class of TTreeNode;
  TTreeNode = class(TPersistent)
  private
    FOwner: TTreeNodes;
    FText: string;
    FData: Pointer;
    FItemId: HTreeItem;
    FImageIndex: TImageIndex;
    FSelectedIndex: Integer;
    FOverlayIndex: Integer;
    FStateIndex: Integer;
    FDeleting: Boolean;
    FInTree: Boolean;
    function CompareCount(CompareMe: Integer): Boolean;
    function DoCanExpand(Expand: Boolean): Boolean;
    procedure DoExpand(Expand: Boolean);
    procedure ExpandItem(Expand: Boolean; Recurse: Boolean);
    function GetAbsoluteIndex: Integer;
    function GetExpanded: Boolean;
    function GetLevel: Integer;
    function GetParent: TTreeNode;
    function GetChildren: Boolean;
    function GetCut: Boolean;
    function GetDropTarget: Boolean;
    function GetFocused: Boolean;
    function GetIndex: Integer;
    function GetItem(Index: Integer): TTreeNode;
    function GetSelected: Boolean;
    function GetCount: Integer;
    function GetTreeView: TCustomTreeView;
    procedure InternalMove(ParentNode, Node: TTreeNode; HItem: HTreeItem;
      AddMode: TAddMode);
    function IsEqual(Node: TTreeNode): Boolean;
    function IsNodeVisible: Boolean;
    procedure ReadData(Stream: TStream; Info: PNodeInfo);
    procedure ReadNodeData(Stream: TStream; var Info: TNodeDataInfo);
    procedure SetChildren(Value: Boolean);
    procedure SetCut(Value: Boolean);
    procedure SetData(Value: Pointer);
    procedure SetDropTarget(Value: Boolean);
    procedure SetItem(Index: Integer; Value: TTreeNode);
    procedure SetExpanded(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetOverlayIndex(Value: Integer);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetSelected(Value: Boolean);
    procedure SetStateIndex(Value: Integer);
    procedure SetText(const S: string);
    procedure WriteNodeData(Stream: TStream; var Info: TNodeDataInfo);
  protected
    function GetState(NodeState: TNodeState): Boolean;
    procedure SetState(NodeState: TNodeState; Value: Boolean);
    procedure SetSelectedBit(Value: Boolean);
  public
    constructor Create(AOwner: TTreeNodes);
    destructor Destroy; override;
    function AlphaSort(ARecurse: Boolean = False): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure Collapse(Recurse: Boolean);
    function CustomSort(SortProc: TTVCompare; Data: Longint; ARecurse: Boolean = False): Boolean;
    procedure Delete;
    procedure DeleteChildren;
    function DisplayRect(TextOnly: Boolean): TRect;
    function EditText: Boolean;
    procedure EndEdit(Cancel: Boolean);
    procedure Expand(Recurse: Boolean);
    function getFirstChild: TTreeNode; {GetFirstChild conflicts with C++ macro}
    function GetHandle: HWND;
    function GetLastChild: TTreeNode;
    function GetNext: TTreeNode;
    function GetNextChild(Value: TTreeNode): TTreeNode;
    function getNextSibling: TTreeNode; {GetNextSibling conflicts with C++ macro}
    function GetNextVisible: TTreeNode;
    function GetPrev: TTreeNode;
    function GetPrevChild(Value: TTreeNode): TTreeNode;
    function getPrevSibling: TTreeNode; {GetPrevSibling conflicts with a C++ macro}
    function GetPrevVisible: TTreeNode;
    function HasAsParent(Value: TTreeNode): Boolean;
    function IndexOf(Value: TTreeNode): Integer;
    procedure MakeVisible;
    procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); virtual;
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    function IsFirstNode: Boolean;
    property Count: Integer read GetCount;
    property Cut: Boolean read GetCut write SetCut;
    property Data: Pointer read FData write SetData;
    property Deleting: Boolean read FDeleting;
    property Focused: Boolean read GetFocused write SetFocused;
    property DropTarget: Boolean read GetDropTarget write SetDropTarget;
    property Selected: Boolean read GetSelected write SetSelected;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Handle: HWND read GetHandle;
    property HasChildren: Boolean read GetChildren write SetChildren;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Index: Integer read GetIndex;
    property IsVisible: Boolean read IsNodeVisible;
    property Item[Index: Integer]: TTreeNode read GetItem write SetItem; default;
    property ItemId: HTreeItem read FItemId;
    property Level: Integer read GetLevel;
    property OverlayIndex: Integer read FOverlayIndex write SetOverlayIndex;
    property Owner: TTreeNodes read FOwner;
    property Parent: TTreeNode read GetParent;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property StateIndex: Integer read FStateIndex write SetStateIndex;
    property Text: string read FText write SetText;
    property TreeView: TCustomTreeView read GetTreeView;
  end;

{ TTreeNodesEnumerator }

  TTreeNodesEnumerator = class
  private
    FIndex: Integer;
    FTreeNodes: TTreeNodes;
  public
    constructor Create(ATreeNodes: TTreeNodes);
    function GetCurrent: TTreeNode;
    function MoveNext: Boolean;
    property Current: TTreeNode read GetCurrent;
  end;

{ TTreeNodes }

  PNodeCache = ^TNodeCache;
  TNodeCache = record
    CacheNode: TTreeNode;
    CacheIndex: Integer;
  end;

  TTreeNodes = class(TPersistent)
  private
    FOwner: TCustomTreeView;
    FUpdateCount: Integer;
    FNodeCache: TNodeCache;
    FReading: Boolean;
    procedure AddedNode(Value: TTreeNode);
    function GetHandle: HWND;
    function GetNodeFromIndex(Index: Integer): TTreeNode;
    procedure ReadData(Stream: TStream);
    procedure ReadNodeData(Stream: TStream);
    procedure Repaint(Node: TTreeNode);
    procedure WriteNodeData(Stream: TStream);
    procedure ClearCache;
    procedure WriteExpandedState(Stream: TStream);
    procedure ReadExpandedState(Stream: TStream);
  protected
    function AddItem(Parent, Target: HTreeItem; const Item: TTVItem;
      AddMode: TAddMode): HTreeItem;
    procedure DefineProperties(Filer: TFiler); override;
    function CreateItem(Node: TTreeNode): TTVItem;
    function GetCount: Integer;
    procedure SetItem(Index: Integer; Value: TTreeNode);
    procedure SetUpdateState(Updating: Boolean);
    property Reading: Boolean read FReading;
  public
    constructor Create(AOwner: TCustomTreeView);
    destructor Destroy; override;
    function AddChildFirst(Parent: TTreeNode; const S: string): TTreeNode;
    function AddChild(Parent: TTreeNode; const S: string): TTreeNode;
    function AddChildObjectFirst(Parent: TTreeNode; const S: string;
      Ptr: Pointer): TTreeNode;
    function AddChildObject(Parent: TTreeNode; const S: string;
      Ptr: Pointer): TTreeNode;
    function AddFirst(Sibling: TTreeNode; const S: string): TTreeNode;
    function Add(Sibling: TTreeNode; const S: string): TTreeNode;
    function AddObjectFirst(Sibling: TTreeNode; const S: string;
      Ptr: Pointer): TTreeNode;
    function AddObject(Sibling: TTreeNode; const S: string;
      Ptr: Pointer): TTreeNode;
    function AddNode(Node, Relative: TTreeNode; const S: string;
      Ptr: Pointer; Method: TNodeAttachMode): TTreeNode;
    function AlphaSort(ARecurse: Boolean = False): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    function CustomSort(SortProc: TTVCompare; Data: Longint; ARecurse: Boolean = False): Boolean;
    procedure Delete(Node: TTreeNode);
    procedure EndUpdate;
    function GetFirstNode: TTreeNode;
    function GetEnumerator: TTreeNodesEnumerator;
    function GetNode(ItemId: HTreeItem): TTreeNode;
    function Insert(Sibling: TTreeNode; const S: string): TTreeNode;
    function InsertObject(Sibling: TTreeNode; const S: string;
      Ptr: Pointer): TTreeNode;
    function InsertNode(Node, Sibling: TTreeNode; const S: string;
      Ptr: Pointer): TTreeNode;
    property Count: Integer read GetCount;
    property Handle: HWND read GetHandle;
    property Item[Index: Integer]: TTreeNode read GetNodeFromIndex; default;
    property Owner: TCustomTreeView read FOwner;
  end;

{ TCustomTreeView }

  TSortType = (stNone, stData, stText, stBoth);
  TMultiSelectStyles = (msControlSelect, msShiftSelect,
                        msVisibleOnly, msSiblingOnly);
  TMultiSelectStyle = set of TMultiSelectStyles;
  ETreeViewError = class(Exception);

  TTVChangingEvent = procedure(Sender: TObject; Node: TTreeNode;
    var AllowChange: Boolean) of object;
  TTVChangedEvent = procedure(Sender: TObject; Node: TTreeNode) of object;
  TTVEditingEvent = procedure(Sender: TObject; Node: TTreeNode;
    var AllowEdit: Boolean) of object;
  TTVEditedEvent = procedure(Sender: TObject; Node: TTreeNode; var S: string) of object;
  TTVExpandingEvent = procedure(Sender: TObject; Node: TTreeNode;
    var AllowExpansion: Boolean) of object;
  TTVCollapsingEvent = procedure(Sender: TObject; Node: TTreeNode;
    var AllowCollapse: Boolean) of object;
  TTVExpandedEvent = procedure(Sender: TObject; Node: TTreeNode) of object;
  TTVCompareEvent = procedure(Sender: TObject; Node1, Node2: TTreeNode;
    Data: Integer; var Compare: Integer) of object;
  TTVCustomDrawEvent = procedure(Sender: TCustomTreeView; const ARect: TRect;
    var DefaultDraw: Boolean) of object;
  TTVCustomDrawItemEvent = procedure(Sender: TCustomTreeView; Node: TTreeNode;
    State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  TTVAdvancedCustomDrawEvent = procedure(Sender: TCustomTreeView; const ARect: TRect;
    Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  TTVAdvancedCustomDrawItemEvent = procedure(Sender: TCustomTreeView; Node: TTreeNode;
    State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
    DefaultDraw: Boolean) of object;
  TTVCreateNodeClassEvent = procedure(Sender: TCustomTreeView;
    var NodeClass: TTreeNodeClass) of object;

  TCustomTreeView = class(TWinControl)
  private
    FAutoExpand: Boolean;
    FBorderStyle: TBorderStyle;
    FCanvas: TCanvas;
    FCanvasChanged: Boolean;
    FDefEditProc: Pointer;
    FDragged: Boolean;
    FDragImage: TDragImageList;
    FDragNode: TTreeNode;
    FEditHandle: HWND;
    FEditInstance: Pointer;
    FHideSelection: Boolean;
    FHotTrack: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FLastDropTarget: TTreeNode;
    FMemStream: TMemoryStream;
    FRClickNode: TTreeNode;
    FRightClickSelect: Boolean;
    FManualNotify: Boolean;
    FReadOnly: Boolean;
    FRowSelect: Boolean;
    FSaveIndex: Integer;
    FSaveIndent: Integer;
    FSaveItems: TStringList;
    FSaveTopIndex: Integer;
    FShowButtons: Boolean;
    FShowLines: Boolean;
    FShowRoot: Boolean;
    FSortType: TSortType;
    FStateChanging: Boolean;
    FStateImages: TCustomImageList;
    FStateChangeLink: TChangeLink;
    FToolTips: Boolean;
    FTreeNodes: TTreeNodes;
    FWideText: WideString;
    FMultiSelect: Boolean;
    FMultiSelectStyle: TMultiSelectStyle;
    FSelections: TList;
    FSaveIndexes: TList;
    FShiftAnchor: TTreeNode;
    FSelecting, FSelectChanged: Boolean;
    FOurFont: Integer;
    FStockFont: Integer;
    FCreateWndRestores: Boolean;
    FOnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent;
    FOnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent;
    FOnCancelEdit: TTVChangedEvent;
    FOnChange: TTVChangedEvent;
    FOnChanging: TTVChangingEvent;
    FOnCollapsed: TTVExpandedEvent;
    FOnCollapsing: TTVCollapsingEvent;
    FOnCompare: TTVCompareEvent;
    FOnCustomDraw: TTVCustomDrawEvent;
    FOnCustomDrawItem: TTVCustomDrawItemEvent;
    FOnDeletion: TTVExpandedEvent;
    FOnAddition: TTVExpandedEvent;
    FOnEditing: TTVEditingEvent;
    FOnEdited: TTVEditedEvent;
    FOnExpanded: TTVExpandedEvent;
    FOnExpanding: TTVExpandingEvent;
    FOnGetImageIndex: TTVExpandedEvent;
    FOnGetSelectedIndex: TTVExpandedEvent;
    FOnCreateNodeClass: TTVCreateNodeClassEvent;
    procedure CanvasChanged(Sender: TObject);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure EditWndProc(var Message: TMessage);
    procedure DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
    procedure NodeDeselect(Index: Integer);
    procedure NodeSelect(Node: TTreeNode; At: Integer = 0);
    procedure FinishSelection(Node: TTreeNode; ShiftState: TShiftState);
    procedure ControlSelectNode(Node: TTreeNode);
    procedure ShiftSelectNode(Node: TTreeNode; Backward: Boolean; Deselect: Boolean = True);
    procedure ControlShiftSelectNode(Node: TTreeNode; Backward: Boolean);
    procedure SelectNode(Node: TTreeNode);
    function GetChangeDelay: Integer;
    function GetDropTarget: TTreeNode;
    function GetIndent: Integer;
    function GetNodeFromItem(const Item: TTVItem): TTreeNode;
    function GetSelected: TTreeNode;
    function GetSelectionCount: Cardinal;
    function GetSelection(Index: Integer): TTreeNode;
    function GetTopItem: TTreeNode;
    procedure ImageListChange(Sender: TObject);
    procedure SetAutoExpand(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetButtonStyle(Value: Boolean);
    procedure SetChangeDelay(Value: Integer);
    procedure SetDropTarget(Value: TTreeNode);
    procedure SetHideSelection(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetImageList(Value: HImageList; Flags: Integer);
    procedure SetIndent(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLineStyle(Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetMultiSelectStyle(const Value: TMultiSelectStyle);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRootStyle(Value: Boolean);
    procedure SetRowSelect(Value: Boolean);
    procedure SetSelected(Value: TTreeNode);
    procedure SetSortType(Value: TSortType);
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetToolTips(Value: Boolean);
    procedure SetTreeNodes(Value: TTreeNodes);
    procedure SetTopItem(Value: TTreeNode);
    procedure OnChangeTimer(Sender: TObject);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMCtlColorEdit(var Message: TMessage); message WM_CTLCOLOREDIT;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
  protected
    FChangeTimer: TTimer;
    function CanEdit(Node: TTreeNode): Boolean; dynamic;
    function CanChange(Node: TTreeNode): Boolean; dynamic;
    function CanCollapse(Node: TTreeNode): Boolean; dynamic;
    function CanExpand(Node: TTreeNode): Boolean; dynamic;
    procedure Change(Node: TTreeNode); dynamic;
    procedure Collapse(Node: TTreeNode); dynamic;
    function CreateNode: TTreeNode; virtual;
    function CreateNodes: TTreeNodes; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; virtual;
    procedure Delete(Node: TTreeNode); dynamic;
    procedure Added(Node: TTreeNode); dynamic;
    procedure DestroyWnd; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure Edit(const Item: TTVItem); dynamic;
    procedure Expand(Node: TTreeNode); dynamic;
    function GetDragImages: TDragImageList; override;
    procedure GetImageIndex(Node: TTreeNode); virtual;
    procedure GetSelectedIndex(Node: TTreeNode); virtual;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetDragMode(Value: TDragMode); override;
    procedure WndProc(var Message: TMessage); override;
    procedure ValidateSelection;
    procedure InvalidateSelectionsRects;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    property AutoExpand: Boolean read FAutoExpand write SetAutoExpand default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay default 0;
    property CreateWndRestores: Boolean read FCreateWndRestores write FCreateWndRestores default True;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read GetIndent write SetIndent;
    property Items: TTreeNodes read FTreeNodes write SetTreeNodes;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property MultiSelectStyle: TMultiSelectStyle read FMultiSelectStyle write SetMultiSelectStyle default [msControlSelect];
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default False;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
    property ShowButtons: Boolean read FShowButtons write SetButtonStyle default True;
    property ShowLines: Boolean read FShowLines write SetLineStyle default True;
    property ShowRoot: Boolean read FShowRoot write SetRootStyle default True;
    property SortType: TSortType read FSortType write SetSortType default stNone;
    property StateImages: TCustomImageList read FStateImages write SetStateImages;
    property ToolTips: Boolean read FToolTips write SetToolTips default True;
    property OnAddition: TTVExpandedEvent read FOnAddition write FOnAddition;
    property OnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent read FOnAdvancedCustomDraw write FOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent read FOnAdvancedCustomDrawItem write FOnAdvancedCustomDrawItem;
    property OnCancelEdit: TTVChangedEvent read FOnCancelEdit write FOnCancelEdit;
    property OnChange: TTVChangedEvent read FOnChange write FOnChange;
    property OnChanging: TTVChangingEvent read FOnChanging write FOnChanging;
    property OnCollapsed: TTVExpandedEvent read FOnCollapsed write FOnCollapsed;
    property OnCollapsing: TTVCollapsingEvent read FOnCollapsing write FOnCollapsing;
    property OnCompare: TTVCompareEvent read FOnCompare write FOnCompare;
    property OnCustomDraw: TTVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawItem: TTVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnDeletion: TTVExpandedEvent read FOnDeletion write FOnDeletion;
    property OnEditing: TTVEditingEvent read FOnEditing write FOnEditing;
    property OnEdited: TTVEditedEvent read FOnEdited write FOnEdited;
    property OnExpanding: TTVExpandingEvent read FOnExpanding write FOnExpanding;
    property OnExpanded: TTVExpandedEvent read FOnExpanded write FOnExpanded;
    property OnGetImageIndex: TTVExpandedEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSelectedIndex: TTVExpandedEvent read FOnGetSelectedIndex write FOnGetSelectedIndex;
    property OnCreateNodeClass: TTVCreateNodeClassEvent read FOnCreateNodeClass write FOnCreateNodeClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AlphaSort(ARecurse: Boolean = True): Boolean;
    function CustomSort(SortProc: TTVCompare; Data: Longint; ARecurse: Boolean = True): Boolean;
    procedure FullCollapse;
    procedure FullExpand;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function GetNodeAt(X, Y: Integer): TTreeNode;
    function IsEditing: Boolean;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property Canvas: TCanvas read FCanvas;
    property DropTarget: TTreeNode read GetDropTarget write SetDropTarget;
    property Selected: TTreeNode read GetSelected write SetSelected;
    property TopItem: TTreeNode read GetTopItem write SetTopItem;

    procedure Select(Node: TTreeNode; ShiftState: TShiftState = []); overload; virtual;
    procedure Select(const Nodes: array of TTreeNode); overload; virtual;
    procedure Select(Nodes: TList); overload; virtual;
    procedure Deselect(Node: TTreeNode); virtual;
    procedure Subselect(Node: TTreeNode; Validate: Boolean = False); virtual;
    property SelectionCount: Cardinal read GetSelectionCount;
    property Selections[Index: Integer]: TTreeNode read GetSelection;
    procedure ClearSelection(KeepPrimary: Boolean = False); virtual;
    function GetSelections(AList: TList): TTreeNode;
    function FindNextToSelect: TTreeNode; virtual;
  end;

  TTreeView = class(TCustomTreeView)
  published
    property Align;
    property Anchors;
    property AutoExpand;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    property MultiSelect;
    property MultiSelectStyle;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    property OnAddition;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    { Items must be published after OnGetImageIndex and OnGetSelectedIndex }
    property Items;
  end;

{ TTrackBar }

  TTrackBarOrientation = (trHorizontal, trVertical);
  TTickMark = (tmBottomRight, tmTopLeft, tmBoth);
  TTickStyle = (tsNone, tsAuto, tsManual);

  TTrackBar = class(TWinControl)
  private
    FOrientation: TTrackBarOrientation;
    FTickMarks: TTickMark;
    FTickStyle: TTickStyle;
    FLineSize: Integer;
    FPageSize: Integer;
    FThumbLength: Integer;
    FSliderVisible: Boolean;
    FMin: Integer;
    FMax: Integer;
    FFrequency: Integer;
    FPosition: Integer;
    FSelStart: Integer;
    FSelEnd: Integer;
    FOnChange: TNotifyEvent;
    function GetThumbLength: Integer;
    procedure SetOrientation(Value: TTrackBarOrientation);
    procedure SetParams(APosition, AMin, AMax: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetFrequency(Value: Integer);
    procedure SetTickStyle(Value: TTickStyle);
    procedure SetTickMarks(Value: TTickMark);
    procedure SetLineSize(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetThumbLength(Value: Integer);
    procedure SetSliderVisible(Value: Boolean);
    procedure SetSelStart(Value: Integer);
    procedure SetSelEnd(Value: Integer);
    procedure UpdateSelection;
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Changed; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetTick(Value: Integer);
  published
    property Align;
    property Anchors;
    property BorderWidth;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property LineSize: Integer read FLineSize write SetLineSize default 1;
    property Max: Integer read FMax write SetMax default 10;
    property Min: Integer read FMin write SetMin default 0;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
    property ParentCtl3D;
    property ParentShowHint;
    property PageSize: Integer read FPageSize write SetPageSize default 2;
    property PopupMenu;
    property Frequency: Integer read FFrequency write SetFrequency default 1;
    property Position: Integer read FPosition write SetPosition default 0;
    property SliderVisible: Boolean read FSliderVisible write SetSliderVisible default True;
    property SelEnd: Integer read FSelEnd write SetSelEnd default 0;
    property SelStart: Integer read FSelStart write SetSelStart default 0;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property ThumbLength: Integer read GetThumbLength write SetThumbLength default 20;
    property TickMarks: TTickMark read FTickMarks write SetTickMarks default tmBottomRight;
    property TickStyle: TTickStyle read FTickStyle write SetTickStyle default tsAuto;
    property Visible;
    property OnContextPopup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TProgressBar }

  TProgressRange = Integer; // for backward compatibility

  TProgressBarOrientation = (pbHorizontal, pbVertical);

  TProgressBar = class(TWinControl)
  private
    F32BitMode: Boolean;
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FStep: Integer;
    FOrientation: TProgressBarOrientation;
    FSmooth: Boolean;
    function GetMin: Integer;
    function GetMax: Integer;
    function GetPosition: Integer;
    procedure SetParams(AMin, AMax: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetStep(Value: Integer);
    procedure SetOrientation(Value: TProgressBarOrientation);
    procedure SetSmooth(Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure StepIt;
    procedure StepBy(Delta: Integer);
  published
    property Align;
    property Anchors;
    property BorderWidth;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Constraints;
    property Min: Integer read GetMin write SetMin default 0;
    property Max: Integer read GetMax write SetMax default 100;
    property Orientation: TProgressBarOrientation read FOrientation
      write SetOrientation default pbHorizontal;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read GetPosition write SetPosition default 0;
    property Smooth: Boolean read FSmooth write SetSmooth default False;
    property Step: Integer read FStep write SetStep default 10;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TTextAttributes }

  TCustomRichEdit = class;

  TAttributeType = (atSelected, atDefaultText);
  TConsistentAttribute = (caBold, caColor, caFace, caItalic,
    caSize, caStrikeOut, caUnderline, caProtected);
  TConsistentAttributes = set of TConsistentAttribute;

  TTextAttributes = class(TPersistent)
  private
    RichEdit: TCustomRichEdit;
    FType: TAttributeType;
    procedure GetAttributes(var Format: TCharFormat);
    function GetCharset: TFontCharset;
    function GetColor: TColor;
    function GetConsistentAttributes: TConsistentAttributes;
    function GetHeight: Integer;
    function GetName: TFontName;
    function GetPitch: TFontPitch;
    function GetProtected: Boolean;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    procedure SetAttributes(var Format: TCharFormat);
    procedure SetCharset(Value: TFontCharset);
    procedure SetColor(Value: TColor);
    procedure SetHeight(Value: Integer);
    procedure SetName(Value: TFontName);
    procedure SetPitch(Value: TFontPitch);
    procedure SetProtected(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TFontStyles);
  protected
    procedure InitFormat(var Format: TCharFormat);
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TCustomRichEdit; AttributeType: TAttributeType);
    procedure Assign(Source: TPersistent); override;
    property Charset: TFontCharset read GetCharset write SetCharset;
    property Color: TColor read GetColor write SetColor;
    property ConsistentAttributes: TConsistentAttributes read GetConsistentAttributes;
    property Name: TFontName read GetName write SetName;
    property Pitch: TFontPitch read GetPitch write SetPitch;
    property &Protected: Boolean read GetProtected write SetProtected;
    property Size: Integer read GetSize write SetSize;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Height: Integer read GetHeight write SetHeight;
  end;

{ TParaAttributes }

  TNumberingStyle = (nsNone, nsBullet);

  TParaAttributes = class(TPersistent)
  private
    RichEdit: TCustomRichEdit;
    procedure GetAttributes(var Paragraph: TParaFormat);
    function GetAlignment: TAlignment;
    function GetFirstIndent: Longint;
    function GetLeftIndent: Longint;
    function GetRightIndent: Longint;
    function GetNumbering: TNumberingStyle;
    function GetTab(Index: Byte): Longint;
    function GetTabCount: Integer;
    procedure InitPara(var Paragraph: TParaFormat);
    procedure SetAlignment(Value: TAlignment);
    procedure SetAttributes(var Paragraph: TParaFormat);
    procedure SetFirstIndent(Value: Longint);
    procedure SetLeftIndent(Value: Longint);
    procedure SetRightIndent(Value: Longint);
    procedure SetNumbering(Value: TNumberingStyle);
    procedure SetTab(Index: Byte; Value: Longint);
    procedure SetTabCount(Value: Integer);
  public
    constructor Create(AOwner: TCustomRichEdit);
    procedure Assign(Source: TPersistent); override;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property FirstIndent: Longint read GetFirstIndent write SetFirstIndent;
    property LeftIndent: Longint read GetLeftIndent write SetLeftIndent;
    property Numbering: TNumberingStyle read GetNumbering write SetNumbering;
    property RightIndent: Longint read GetRightIndent write SetRightIndent;
    property Tab[Index: Byte]: Longint read GetTab write SetTab;
    property TabCount: Integer read GetTabCount write SetTabCount;
  end;

{ TCustomRichEdit }

  TRichEditResizeEvent = procedure(Sender: TObject; Rect: TRect) of object;
  TRichEditProtectChange = procedure(Sender: TObject;
    StartPos, EndPos: Integer; var AllowChange: Boolean) of object;
  TRichEditSaveClipboard = procedure(Sender: TObject;
    NumObjects, NumChars: Integer; var SaveClipboard: Boolean) of object;
  TSearchType = (stWholeWord, stMatchCase);
  TSearchTypes = set of TSearchType;

  TConversion = class(TObject)
  public
    function ConvertReadStream(Stream: TStream; Buffer: PChar; BufSize: Integer): Integer; virtual;
    function ConvertWriteStream(Stream: TStream; Buffer: PChar; BufSize: Integer): Integer; virtual;
  end;

  TConversionClass = class of TConversion;

  PConversionFormat = ^TConversionFormat;
  TConversionFormat = record
    ConversionClass: TConversionClass;
    Extension: string;
    Next: PConversionFormat;
  end;

  PRichEditStreamInfo = ^TRichEditStreamInfo;
  TRichEditStreamInfo = record
    Converter: TConversion;
    Stream: TStream;
  end;

  TCustomRichEdit = class(TCustomMemo)
  private
    FHideScrollBars: Boolean;
    FSelAttributes: TTextAttributes;
    FDefAttributes: TTextAttributes;
    FParagraph: TParaAttributes;
    FOldParaAlignment: TAlignment;
    FScreenLogPixels: Integer;
    FRichEditStrings: TStrings;
    FMemStream: TMemoryStream;
    FOnSelChange: TNotifyEvent;
    FHideSelection: Boolean;
    FModified: Boolean;
    FDefaultConverter: TConversionClass;
    FOnResizeRequest: TRichEditResizeEvent;
    FOnProtectChange: TRichEditProtectChange;
    FOnSaveClipboard: TRichEditSaveClipboard;
    FPageRect: TRect;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    function GetPlainText: Boolean;
    function ProtectChange(StartPos, EndPos: Integer): Boolean;
    function SaveClipboard(NumObj, NumChars: Integer): Boolean;
    procedure SetHideScrollBars(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetPlainText(Value: Boolean);
    procedure SetRichEditStrings(Value: TStrings);
    procedure SetDefAttributes(Value: TTextAttributes);
    procedure SetSelAttributes(Value: TTextAttributes);
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFont(var Message: TWMSetFont); message WM_SETFONT;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure RequestSize(const Rect: TRect); virtual;
    procedure SelectionChange; dynamic;
    procedure DoSetMaxLength(Value: Integer); override;
    function GetCaretPos: TPoint; override;
    procedure SetCaretPos(const Value: TPoint); override;    
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
    function GetSelText: string; override;
    procedure SetSelLength(Value: Integer); override;
    procedure SetSelStart(Value: Integer); override;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property HideScrollBars: Boolean read FHideScrollBars
      write SetHideScrollBars default True;
    property Lines: TStrings read FRichEditStrings write SetRichEditStrings;
    property OnSaveClipboard: TRichEditSaveClipboard read FOnSaveClipboard
      write FOnSaveClipboard;
    property OnSelectionChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    property OnProtectChange: TRichEditProtectChange read FOnProtectChange
      write FOnProtectChange;
    property OnResizeRequest: TRichEditResizeEvent read FOnResizeRequest
      write FOnResizeRequest;
    property PlainText: Boolean read GetPlainText write SetPlainText default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function FindText(const SearchStr: string;
      StartPos, Length: Integer; Options: TSearchTypes): Integer;
    function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer; override;
    procedure Print(const Caption: string); virtual;
    class procedure RegisterConversionFormat(const AExtension: string;
      AConversionClass: TConversionClass);
    property DefaultConverter: TConversionClass
      read FDefaultConverter write FDefaultConverter;
    property DefAttributes: TTextAttributes read FDefAttributes write SetDefAttributes;
    property SelAttributes: TTextAttributes read FSelAttributes write SetSelAttributes;
    property PageRect: TRect read FPageRect write FPageRect;
    property Paragraph: TParaAttributes read FParagraph;
  end;

  TRichEdit = class(TCustomRichEdit)
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property ImeMode;
    property ImeName;
    property Constraints;
    property Lines;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;
    property OnChange;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnProtectChange;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TUpDown }

  TUDAlignButton = (udLeft, udRight);
  TUDOrientation = (udHorizontal, udVertical);
  TUDBtnType = (btNext, btPrev);
  TUpDownDirection = (updNone, updUp, updDown);
  TUDClickEvent = procedure (Sender: TObject; Button: TUDBtnType) of object;
  TUDChangingEvent = procedure (Sender: TObject; var AllowChange: Boolean) of object;
  TUDChangingEventEx = procedure (Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection) of object;

  TCustomUpDown = class(TWinControl)
  private
    FArrowKeys: Boolean;
    FAssociate: TWinControl;
    FMin: SmallInt;
    FMax: SmallInt;
    FIncrement: Integer;
    FNewValue: SmallInt;
    FNewValueDelta: SmallInt;
    FPosition: SmallInt;
    FThousands: Boolean;
    FWrap: Boolean;
    FOnClick: TUDClickEvent;
    FAlignButton: TUDAlignButton;
    FOrientation: TUDOrientation;
    FOnChanging: TUDChangingEvent;
    FOnChangingEx: TUDChangingEventEx;
    procedure UndoAutoResizing(Value: TWinControl);
    procedure SetAssociate(Value: TWinControl);
    function GetPosition: SmallInt;
    procedure SetMin(Value: SmallInt);
    procedure SetMax(Value: SmallInt);
    procedure SetIncrement(Value: Integer);
    procedure SetPosition(Value: SmallInt);
    procedure SetAlignButton(Value: TUDAlignButton);
    procedure SetOrientation(Value: TUDOrientation);
    procedure SetArrowKeys(Value: Boolean);
    procedure SetThousands(Value: Boolean);
    procedure SetWrap(Value: Boolean);
    procedure CMAllChildrenFlipped(var Message: TMessage); message CM_ALLCHILDRENFLIPPED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message CN_VSCROLL;
  protected
    function DoCanChange(NewVal: SmallInt; Delta: SmallInt): Boolean;
    function CanChange: Boolean; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click(Button: TUDBtnType); reintroduce; dynamic;
    property AlignButton: TUDAlignButton read FAlignButton write SetAlignButton default udRight;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
    property Associate: TWinControl read FAssociate write SetAssociate;
    property Min: SmallInt read FMin write SetMin default 0;
    property Max: SmallInt read FMax write SetMax default 100;
    property Increment: Integer read FIncrement write SetIncrement default 1;
    property Orientation: TUDOrientation read FOrientation write SetOrientation default udVertical;
    property Position: SmallInt read GetPosition write SetPosition default 0;
    property Thousands: Boolean read FThousands write SetThousands default True;
    property Wrap: Boolean read FWrap write SetWrap default False;
    property OnChanging: TUDChangingEvent read FOnChanging write FOnChanging;
    property OnChangingEx: TUDChangingEventEx read FOnChangingEx write FOnChangingEx;    
    property OnClick: TUDClickEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TUpDown = class(TCustomUpDown)
  published
    property AlignButton;
    property Anchors;
    property Associate;
    property ArrowKeys;
    property Enabled;
    property Hint;
    property Min;
    property Max;
    property Increment;
    property Constraints;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Thousands;
    property Visible;
    property Wrap;
    property OnChanging;
    property OnChangingEx;
    property OnContextPopup;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ THotKey }

  THKModifier = (hkShift, hkCtrl, hkAlt, hkExt);
  THKModifiers = set of THKModifier;
  THKInvalidKey = (hcNone, hcShift, hcCtrl, hcAlt, hcShiftCtrl,
    hcShiftAlt, hcCtrlAlt, hcShiftCtrlAlt);
  THKInvalidKeys = set of THKInvalidKey;

  TCustomHotKey = class(TWinControl)
  private
    FAutoSize: Boolean;
    FModifiers: THKModifiers;
    FInvalidKeys: THKInvalidKeys;
    FHotKey: Word;
    FOnChange: TNotifyEvent;
    procedure AdjustHeight;
    procedure SetInvalidKeys(Value: THKInvalidKeys);
    procedure SetModifiers(Value: THKModifiers);
    procedure UpdateHeight;
    function GetHotKey: TShortCut;
    procedure SetHotKey(Value: TShortCut);
    procedure ShortCutToHotKey(Value: TShortCut);
    function HotKeyToShortCut(Value: Longint): TShortCut;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure SetAutoSize(Value: Boolean); override;    
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property InvalidKeys: THKInvalidKeys read FInvalidKeys write SetInvalidKeys default [hcNone, hcShift];
    property Modifiers: THKModifiers read FModifiers write SetModifiers default [hkAlt];
    property HotKey: TShortCut read GetHotKey write SetHotKey default $0041; { Alt - A }
    property TabStop default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  THotKey = class(TCustomHotKey)
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property Enabled;
    property Hint;
    property HotKey;
    property InvalidKeys;
    property Modifiers;
    property ParentBiDiMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

const
  ColumnHeaderWidth = LVSCW_AUTOSIZE_USEHEADER;
  ColumnTextWidth = LVSCW_AUTOSIZE;

type
  TListColumns = class;
  TListItem = class;
  TListItems = class;
  TCustomListView = class;
  TWidth = ColumnHeaderWidth..MaxInt;

  TListColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FCaption: string;
    FMaxWidth: TWidth;
    FMinWidth: TWidth;
    FImageIndex: TImageIndex;
    FPrivateWidth: TWidth;
    FWidth: TWidth;
    FOrderTag,
    FTag: Integer;
    procedure DoChange;
    function GetWidth: TWidth;
    function IsWidthStored: Boolean;
    procedure ReadData(Reader: TReader);
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSize(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetMaxWidth(Value: TWidth);
    procedure SetMinWidth(Value: TWidth);
    procedure SetWidth(Value: TWidth);
    procedure WriteData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property WidthType: TWidth read FWidth;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property MaxWidth: TWidth read FMaxWidth write SetMaxWidth default 0;
    property MinWidth: TWidth read FMinWidth write SetMinWidth default 0;
    property Tag: Integer read FTag write FTag default 0;
    property Width: TWidth read GetWidth write SetWidth stored IsWidthStored default 50;
  end;

  TListColumns = class(TCollection)
  private
    FOwner: TCustomListView;
    function GetItem(Index: Integer): TListColumn;
    procedure SetItem(Index: Integer; Value: TListColumn);
    procedure UpdateCols;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TCustomListView);
    function Add: TListColumn;
    function Owner: TCustomListView;
    property Items[Index: Integer]: TListColumn read GetItem write SetItem; default;
  end;

  TDisplayCode = (drBounds, drIcon, drLabel, drSelectBounds);

  { TListItem }

  TListItemClass = class of TListItem;
  TListItem = class(TPersistent)
  private
    FOwner: TListItems;
    FSubItems: TStrings;
    FData: Pointer;
    FImageIndex: TImageIndex;
    FIndent: Integer;
    FIndex: Integer;
    FOverlayIndex: TImageIndex;
    FStateIndex: TImageIndex;
    FCaption: string;
    FDeleting: Boolean;
    FProcessedDeleting: Boolean;
    FChecked: Boolean;
    function GetChecked: Boolean;
    function GetHandle: HWND;
    function GetIndex: Integer;
    function GetListView: TCustomListView;
    function GetLeft: Integer;
    function GetState(Index: Integer): Boolean;
    function GetTop: Integer;
    function IsEqual(Item: TListItem): Boolean;
    procedure SetChecked(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetData(Value: Pointer);
    procedure SetImage(Index: Integer; Value: TImageIndex);
    procedure SetIndent(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetState(Index: Integer; State: Boolean);
    procedure SetSubItems(Value: TStrings);
    procedure SetTop(Value: Integer);
    function GetSubItemImage(Index: Integer): Integer;
    procedure SetSubItemImage(Index: Integer; const Value: Integer);
  public
    constructor Create(AOwner: TListItems);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CancelEdit;
    procedure Delete;
    function DisplayRect(Code: TDisplayCode): TRect;
    function EditCaption: Boolean;
    function GetPosition: TPoint;
    procedure MakeVisible(PartialOK: Boolean);
    procedure Update;
    procedure SetPosition(const Value: TPoint);
    function WorkArea: Integer;
    property Caption: string read FCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Cut: Boolean index 0 read GetState write SetState;
    property Data: Pointer read FData write SetData;
    property Deleting: Boolean read FDeleting;
    property DropTarget: Boolean index 1 read GetState write SetState;
    property Focused: Boolean index 2 read GetState write SetState;
    property Handle: HWND read GetHandle;
    property ImageIndex: TImageIndex index 0 read FImageIndex write SetImage;
    property Indent: Integer read FIndent write SetIndent default 0;
    property Index: Integer read GetIndex;
    property Left: Integer read GetLeft write SetLeft;
    property ListView: TCustomListView read GetListView;
    property Owner: TListItems read FOwner;
    property OverlayIndex: TImageIndex index 1 read FOverlayIndex write SetImage;
    property Position: TPoint read GetPosition write SetPosition;
    property Selected: Boolean index 3 read GetState write SetState;
    property StateIndex: TImageIndex index 2 read FStateIndex write SetImage;
    property SubItems: TStrings read FSubItems write SetSubItems;
    property SubItemImages[Index: Integer]: Integer read GetSubItemImage write SetSubItemImage;
    property Top: Integer read GetTop write SetTop;
  end;

{ TListItemsEnumerator }

  TListItemsEnumerator = class
  private
    FIndex: Integer;
    FListItems: TListItems;
  public
    constructor Create(AListItems: TListItems);
    function GetCurrent: TListItem;
    function MoveNext: Boolean;
    property Current: TListItem read GetCurrent;
  end;

{ TListItems }

  TListItems = class(TPersistent)
  private
    FOwner: TCustomListView;
    FUpdateCount: Integer;
    FNoRedraw: Boolean;
    procedure ReadData(Stream: TStream);
    procedure ReadItemData(Stream: TStream);
    procedure WriteItemData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function CreateItem(Index: Integer; ListItem: TListItem): TLVItem;
    function GetCount: Integer;
    function GetHandle: HWND;
    function GetItem(Index: Integer): TListItem;
    procedure SetCount(Value: Integer);
    procedure SetItem(Index: Integer; Value: TListItem);
    procedure SetUpdateState(Updating: Boolean);
  public
    constructor Create(AOwner: TCustomListView);
    destructor Destroy; override;
    function Add: TListItem;
    function AddItem(Item: TListItem; Index: Integer = -1): TListItem;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EndUpdate;
    function GetEnumerator: TListItemsEnumerator;
    function IndexOf(Value: TListItem): Integer;
    function Insert(Index: Integer): TListItem;
    property Count: Integer read GetCount write SetCount;
    property Handle: HWND read GetHandle;
    property Item[Index: Integer]: TListItem read GetItem write SetItem; default;
    property Owner: TCustomListView read FOwner;
  end;

{ TWorkArea }

  TWorkArea = class(TCollectionItem)
  private
    FRect: TRect;
    FDisplayName: string;
    FColor: TColor;
    procedure SetRect(const Value: TRect);
    procedure SetColor(const Value: TColor);
  public
    constructor Create(Collection: TCollection); override;
    procedure SetDisplayName(const Value: string); override;
    function  GetDisplayName: string; override;
    property Rect: TRect read FRect write SetRect;
    property Color: TColor read FColor write SetColor;
  end;

{ TWorkAreas }

  TWorkAreas = class(TOwnedCollection)
  private
    function  GetItem(Index: Integer): TWorkArea;
    procedure SetItem(Index: Integer; const Value: TWorkArea);
  protected
    procedure Changed;
    procedure Update(Item: TCollectionItem); override;
  public
    function  Add: TWorkArea;
    procedure Delete(Index: Integer);
    function  Insert(Index: Integer): TWorkArea;
    property  Items[Index: Integer]: TWorkArea read GetItem write SetItem; default;
  end;

{ TIconOptions }

  TIconArrangement = (iaTop, iaLeft);

  TIconOptions = class(TPersistent)
  private
    FListView: TCustomListView;
    FArrangement: TIconArrangement;
    FAutoArrange: Boolean;
    FWrapText: Boolean;
    procedure SetArrangement(Value: TIconArrangement);
    procedure SetAutoArrange(Value: Boolean);
    procedure SetWrapText(Value: Boolean);
  public
    constructor Create(AOwner: TCustomListView);
  published
    property Arrangement: TIconArrangement read FArrangement write SetArrangement default iaTop;
    property AutoArrange: Boolean read FAutoArrange write SetAutoArrange default False;
    property WrapText: Boolean read FWrapText write SetWrapText default True;
  end;

  TOwnerDrawState = Windows.TOwnerDrawState;

  (*$NODEFINE TOwnerDrawState*)

  TListArrangement = (arAlignBottom, arAlignLeft, arAlignRight,
    arAlignTop, arDefault, arSnapToGrid);
  TViewStyle = (vsIcon, vsSmallIcon, vsList, vsReport);
  TItemState = (isNone, isCut, isDropHilited, isFocused, isSelected, isActivating);
  TItemStates = set of TItemState;
  TItemChange = (ctText, ctImage, ctState);
  TItemFind = (ifData, ifPartialString, ifExactString, ifNearest);
  TSearchDirection = (sdLeft, sdRight, sdAbove, sdBelow, sdAll);
  TListHotTrackStyle = (htHandPoint, htUnderlineCold, htUnderlineHot);
  TListHotTrackStyles = set of TListHotTrackStyle;
  TItemRequests = (irText, irImage, irParam, irState, irIndent);
  TItemRequest = set of TItemRequests;

  TLVDeletedEvent = procedure(Sender: TObject; Item: TListItem) of object;
  TLVEditingEvent = procedure(Sender: TObject; Item: TListItem;
    var AllowEdit: Boolean) of object;
  TLVEditedEvent = procedure(Sender: TObject; Item: TListItem; var S: string) of object;
  TLVChangeEvent = procedure(Sender: TObject; Item: TListItem;
    Change: TItemChange) of object;
  TLVChangingEvent = procedure(Sender: TObject; Item: TListItem;
    Change: TItemChange; var AllowChange: Boolean) of object;
  TLVColumnClickEvent = procedure(Sender: TObject; Column: TListColumn) of object;
  TLVColumnRClickEvent = procedure(Sender: TObject; Column: TListColumn;
    Point: TPoint) of object;
  TLVCompareEvent = procedure(Sender: TObject; Item1, Item2: TListItem;
    Data: Integer; var Compare: Integer) of object;
  TLVNotifyEvent = procedure(Sender: TObject; Item: TListItem) of object;
  TLVSelectItemEvent = procedure(Sender: TObject; Item: TListItem;
    Selected: Boolean) of object;
  TLVDrawItemEvent = procedure(Sender: TCustomListView; Item: TListItem;
    Rect: TRect; State: TOwnerDrawState) of object;
  TLVCustomDrawEvent = procedure(Sender: TCustomListView; const ARect: TRect;
    var DefaultDraw: Boolean) of object;
  TLVCustomDrawItemEvent = procedure(Sender: TCustomListView; Item: TListItem;
    State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  TLVCustomDrawSubItemEvent = procedure(Sender: TCustomListView; Item: TListItem;
    SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  TLVAdvancedCustomDrawEvent = procedure(Sender: TCustomListView; const ARect: TRect;
    Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  TLVAdvancedCustomDrawItemEvent = procedure(Sender: TCustomListView; Item: TListItem;
    State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  TLVAdvancedCustomDrawSubItemEvent = procedure(Sender: TCustomListView; Item: TListItem;
    SubItem: Integer; State: TCustomDrawState; Stage: TCustomDrawStage;
    var DefaultDraw: Boolean) of object;
  TLVOwnerDataEvent = procedure(Sender: TObject; Item: TListItem) of object;
  TLVOwnerDataFindEvent = procedure(Sender: TObject; Find: TItemFind;
    const FindString: string; const FindPosition: TPoint; FindData: Pointer;
    StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
    var Index: Integer) of object;
  TLVOwnerDataHintEvent = procedure(Sender: TObject; StartIndex, EndIndex: Integer) of object;
  TLVOwnerDataStateChangeEvent = procedure(Sender: TObject; StartIndex,
    EndIndex: Integer; OldState, NewState: TItemStates) of object;
  TLVSubItemImageEvent = procedure(Sender: TObject; Item: TListItem; SubItem: Integer;
    var ImageIndex: Integer) of object;
  TLVInfoTipEvent = procedure(Sender: TObject; Item: TListItem; var InfoTip: string) of object;
  TLVCreateItemClassEvent = procedure(Sender: TCustomListView; var ItemClass: TListItemClass) of object;

{ TCustomListView }

  TCustomListView = class(TCustomMultiSelectListControl)
  private
    FCanvas: TCanvas;
    FBorderStyle: TBorderStyle;
    FViewStyle: TViewStyle;
    FReadOnly: Boolean;
    FLargeImages: TCustomImageList;
    FSmallImages: TCustomImageList;
    FStateImages: TCustomImageList;
    FDragImage: TDragImageList;
    FMultiSelect: Boolean;
    FSortType: TSortType;
    FColumnClick: Boolean;
    FShowColumnHeaders: Boolean;
    FListItems: TListItems;
    FClicked: Boolean;
    FRClicked: Boolean;
    FIconOptions: TIconOptions;
    FHideSelection: Boolean;
    FListColumns: TListColumns;
    FMemStream: TMemoryStream;
    FOwnerData: Boolean;
    FOwnerDraw: Boolean;
    FColStream: TMemoryStream;
    FCheckStream: TMemoryStream;
    FEditInstance: Pointer;
    FDefEditProc: Pointer;
    FEditHandle: HWND;
    FHeaderInstance: Pointer;
    FDefHeaderProc: Pointer;
    FHeaderHandle: HWND;
    FAllocBy: Integer;
    FDragIndex: Integer;
    FLastDropTarget: TListItem;
    FCheckboxes: Boolean;
    FFlatScrollBars: Boolean;
    FFullDrag: Boolean;
    FGridLines: Boolean;
    FHotTrack: Boolean;
    FHotTrackStyles: TListHotTrackStyles;
    FRowSelect: Boolean;
    FHoverTime: Integer;
    FLargeChangeLink: TChangeLink;
    FSmallChangeLink: TChangeLink;
    FStateChangeLink: TChangeLink;
    FSavedSort: TSortType;
    FReading: Boolean;
    FCanvasChanged: Boolean;
    FTempItem: TListItem;
    FWorkAreas: TWorkAreas;
    FShowWorkAreas: Boolean;
    FUpdatingColumnOrder: Boolean;
    FOurFont: Integer;
    FStockFont: Integer;
    FOwnerDataCount: Integer;
    FOnAdvancedCustomDraw: TLVAdvancedCustomDrawEvent;
    FOnAdvancedCustomDrawItem: TLVAdvancedCustomDrawItemEvent;
    FOnAdvancedCustomDrawSubItem: TLVAdvancedCustomDrawSubItemEvent;
    FOnChange: TLVChangeEvent;
    FOnChanging: TLVChangingEvent;
    FOnColumnClick: TLVColumnClickEvent;
    FOnColumnDragged: TNotifyEvent;
    FOnColumnRightClick: TLVColumnRClickEvent;
    FOnCompare: TLVCompareEvent;
    FOnCustomDraw: TLVCustomDrawEvent;
    FOnCustomDrawItem: TLVCustomDrawItemEvent;
    FOnCustomDrawSubItem: TLVCustomDrawSubItemEvent;
    FOnData: TLVOwnerDataEvent;
    FOnDataFind: TLVOwnerDataFindEvent;
    FOnDataHint: TLVOwnerDataHintEvent;
    FOnDataStateChange: TLVOwnerDataStateChangeEvent;
    FOnDeletion: TLVDeletedEvent;
    FOnDrawItem: TLVDrawItemEvent;
    FOnEdited: TLVEditedEvent;
    FOnEditing: TLVEditingEvent;
    FOnGetImageIndex: TLVNotifyEvent;
    FOnGetSubItemImage: TLVSubItemImageEvent;
    FOnInfoTip: TLVInfoTipEvent;
    FOnInsert: TLVDeletedEvent;
    FOnSelectItem: TLVSelectItemEvent;
    FOnCreateItemClass: TLVCreateItemClassEvent;
    function AreItemsStored: Boolean;
    procedure CanvasChanged(Sender: TObject);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure DoAutoSize;
    procedure DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
    procedure DrawWorkAreas;
    procedure EditWndProc(var Message: TMessage);
    function GetBoundingRect: TRect;
    function GetColumnFromIndex(Index: Integer): TListColumn;
    function GetColumnFromTag(Tag: Integer): TListColumn;
    function GetDropTarget: TListItem;
    function GetFocused: TListItem;
    procedure GetImageIndex(Item: TListItem);
    procedure GetSubItemImage(Item: TListItem; SubItem: Integer; var ImageIndex: Integer);
    function GetItem(Value: TLVItem): TListItem;
    function GetSelected: TListItem;
    function GetTopItem: TListItem;
    function GetViewOrigin: TPoint;
    function GetVisibleRowCount: Integer;
    function GetHoverTime: Integer;
    procedure HeaderWndProc(var Message: TMessage);
    procedure ImageListChange(Sender: TObject);
    procedure RestoreChecks;
    procedure SaveChecks;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColumnClick(Value: Boolean);
    procedure SetColumnHeaders(Value: Boolean);
    procedure SetDropTarget(Value: TListItem);
    procedure SetFocused(Value: TListItem);
    procedure SetHideSelection(Value: Boolean);
    procedure SetIconOptions(Value: TIconOptions);
    procedure SetImageList(Value: HImageList; Flags: Integer);
    procedure SetLargeImages(Value: TCustomImageList);
    procedure SetAllocBy(Value: Integer);
    procedure SetItems(Value: TListItems);
    procedure SetListColumns(Value: TListColumns);
    procedure SetOwnerData(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetShowWorkAreas(const Value: Boolean);
    procedure SetSmallImages(Value: TCustomImageList);
    procedure SetSortType(Value: TSortType);
    procedure SetSelected(Value: TListItem);
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetTextBkColor(Value: TColor);
    procedure SetTextColor(Value: TColor);
    procedure SetCheckboxes(Value: Boolean);
    procedure SetFlatScrollBars(Value: Boolean);
    procedure SetFullDrag(Value: Boolean);
    procedure SetGridLines(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackStyles(Value: TListHotTrackStyles);
    procedure SetRowSelect(Value: Boolean);
    procedure SetHoverTime(Value: Integer);
    procedure ResetExStyles;
    function ValidHeaderHandle: Boolean;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMCtlColorEdit(var Message: TMessage); message WM_CTLCOLOREDIT;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function CanChange(Item: TListItem; Change: Integer): Boolean; dynamic;
    function CanEdit(Item: TListItem): Boolean; dynamic;
    procedure Change(Item: TListItem; Change: Integer); dynamic;
    procedure ChangeScale(M, D: Integer); override;
    procedure ColClick(Column: TListColumn); dynamic;
    procedure ColRightClick(Column: TListColumn; Point: TPoint); dynamic;
    function ColumnsShowing: Boolean;
    function CreateListItem: TListItem; virtual;
    function CreateListItems: TListItems; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawItem(Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawSubItem(Item: TListItem; SubItem: Integer;
      State: TCustomDrawState; Stage: TCustomDrawStage): Boolean; virtual;
    procedure Delete(Item: TListItem); dynamic;
    procedure DestroyWnd; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoInfoTip(Item: TListItem; var InfoTip: string); virtual;
    procedure DrawItem(Item: TListItem; Rect: TRect; State: TOwnerDrawState); virtual;
    procedure Edit(const Item: TLVItem); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean; virtual;
    function OwnerDataFind(Find: TItemFind; const FindString: string;
      const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer;
      Direction: TSearchDirection; Wrap: Boolean): Integer; virtual;
    function OwnerDataHint(StartIndex, EndIndex: Integer): Boolean; virtual;
    function OwnerDataStateChange(StartIndex, EndIndex: Integer; OldState,
      NewState: TItemStates): Boolean; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetCount: Integer; override;
    function GetDragImages: TDragImageList; override;
    function GetItemIndex(Value: TListItem): Integer; reintroduce; overload;
    function GetItemIndex: Integer; reintroduce; overload; override;
    function GetSelCount: Integer; override;
    procedure InsertItem(Item: TListItem); dynamic;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure SetMultiSelect(Value: Boolean); override;
    procedure SetViewStyle(Value: TViewStyle); virtual;
    procedure UpdateColumn(AnIndex: Integer);
    procedure UpdateColumns;
    procedure WndProc(var Message: TMessage); override;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Columns: TListColumns read FListColumns write SetListColumns;
    property ColumnClick: Boolean read FColumnClick write SetColumnClick default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property IconOptions: TIconOptions read FIconOptions write SetIconOptions;
    property Items: TListItems read FListItems write SetItems stored AreItemsStored;
    property AllocBy: Integer read FAllocBy write SetAllocBy default 0;
    property HoverTime: Integer read GetHoverTime write SetHoverTime default -1;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property OwnerData: Boolean read FOwnerData write SetOwnerData default False;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property OnAdvancedCustomDraw: TLVAdvancedCustomDrawEvent read FOnAdvancedCustomDraw write FOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem: TLVAdvancedCustomDrawItemEvent read FOnAdvancedCustomDrawItem write FOnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem: TLVAdvancedCustomDrawSubItemEvent read FOnAdvancedCustomDrawSubItem write FOnAdvancedCustomDrawSubItem;
    property OnChange: TLVChangeEvent read FOnChange write FOnChange;
    property OnChanging: TLVChangingEvent read FOnChanging write FOnChanging;
    property OnColumnClick: TLVColumnClickEvent read FOnColumnClick
      write FOnColumnClick;
    property OnColumnDragged: TNotifyEvent read FOnColumnDragged write FOnColumnDragged;
    property OnColumnRightClick: TLVColumnRClickEvent read FOnColumnRightClick
      write FOnColumnRightClick;
    property OnCompare: TLVCompareEvent read FOnCompare write FOnCompare;
    property OnCustomDraw: TLVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawItem: TLVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnCustomDrawSubItem: TLVCustomDrawSubItemEvent read FOnCustomDrawSubItem write FOnCustomDrawSubItem;
    property OnData: TLVOwnerDataEvent read FOnData write FOnData;
    property OnDataFind: TLVOwnerDataFindEvent read FOnDataFind write FOnDataFind;
    property OnDataHint: TLVOwnerDataHintEvent read FOnDataHint write FOnDataHint;
    property OnDataStateChange: TLVOwnerDataStateChangeEvent read FOnDataStateChange write FOnDataStateChange;
    property OnDeletion: TLVDeletedEvent read FOnDeletion write FOnDeletion;
    property OnDrawItem: TLVDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnEdited: TLVEditedEvent read FOnEdited write FOnEdited;
    property OnEditing: TLVEditingEvent read FOnEditing write FOnEditing;
    property OnInfoTip: TLVInfoTipEvent read FOnInfoTip write FOnInfoTip;
    property OnInsert: TLVDeletedEvent read FOnInsert write FOnInsert;
    property OnGetImageIndex: TLVNotifyEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSubItemImage: TLVSubItemImageEvent read FOnGetSubItemImage write FOnGetSubItemImage;
    property OnSelectItem: TLVSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property ShowColumnHeaders: Boolean read FShowColumnHeaders write
      SetColumnHeaders default True;
    property ShowWorkAreas: Boolean read FShowWorkAreas write SetShowWorkAreas default False;
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    property SortType: TSortType read FSortType write SetSortType default stNone;
    property StateImages: TCustomImageList read FStateImages write SetStateImages;
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle default vsIcon;
    property OnCreateItemClass: TLVCreateItemClassEvent read FOnCreateItemClass write FOnCreateItemClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(Item: String; AObject: TObject); override;
    function AlphaSort: Boolean;
    procedure Arrange(Code: TListArrangement);
    procedure Clear; override;
    procedure ClearSelection; override;
    procedure CopySelection(Destination: TCustomListControl); override;
    procedure DeleteSelected; override;
    function FindCaption(StartIndex: Integer; Value: string;
      Partial, Inclusive, Wrap: Boolean): TListItem;
    function FindData(StartIndex: Integer; Value: Pointer;
      Inclusive, Wrap: Boolean): TListItem;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function GetItemAt(X, Y: Integer): TListItem;
    function GetNearestItem(Point: TPoint;
      Direction: TSearchDirection): TListItem;
    function GetNextItem(StartItem: TListItem;
      Direction: TSearchDirection; States: TItemStates): TListItem;
    function GetSearchString: string;
    function IsEditing: Boolean;
    procedure SelectAll; override;
    procedure Scroll(DX, DY: Integer);
    property Canvas: TCanvas read FCanvas;
    property Checkboxes: Boolean read FCheckboxes write SetCheckboxes default False;
    property Column[Index: Integer]: TListColumn read GetColumnFromIndex;
    property DropTarget: TListItem read GetDropTarget write SetDropTarget;
    property FlatScrollBars: Boolean read FFlatScrollBars write SetFlatScrollBars default False;
    property FullDrag: Boolean read FFullDrag write SetFullDrag default False;
    property GridLines: Boolean read FGridLines write SetGridLines default False;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HotTrackStyles: TListHotTrackStyles read FHotTrackStyles write SetHotTrackStyles default [];
    property ItemFocused: TListItem read GetFocused write SetFocused;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
    property SelCount: Integer read GetSelCount;
    property Selected: TListItem read GetSelected write SetSelected;
    function CustomSort(SortProc: TLVCompare; lParam: Longint): Boolean;
    function StringWidth(S: string): Integer;
    procedure UpdateItems(FirstIndex, LastIndex: Integer);
    property TopItem: TListItem read GetTopItem;
    property ViewOrigin: TPoint read GetViewOrigin;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property BoundingRect: TRect read GetBoundingRect;
    property WorkAreas: TWorkAreas read FWorkAreas;
  end;

{ TListView }

  TListView = class(TCustomListView)
  published
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TListViewActionLink }

  TListViewActionLink = class(TListActionLink)
  protected
    procedure AddItem(AnItem: TListControlItem); override;
    procedure AddItem(ACaption: String; AImageIndex: Integer;
      DataPtr: Pointer); override;
    procedure SetImages(Value: TCustomImageList); override;
  end;

{ TAnimate }

  TCommonAVI = (aviNone, aviFindFolder, aviFindFile, aviFindComputer, aviCopyFiles,
    aviCopyFile, aviRecycleFile, aviEmptyRecycle, aviDeleteFile);

  TAnimate = class(TWinControl)
  private
    FActive: Boolean;
    FFileName: string;
    FCenter: Boolean;
    FCommonAVI: TCommonAVI;
    FFrameCount: Integer;
    FFrameHeight: Integer;
    FFrameWidth: Integer;
    FOpen: Boolean;
    FRecreateNeeded: Boolean;
    FRepetitions: Integer;
    FResHandle: THandle;
    FResId: Integer;
    FResName: string;
    FStreamedActive: Boolean;
    FTimers: Boolean;
    FTransparent: Boolean;
    FStartFrame: Smallint;
    FStopFrame: Smallint;
    FStopCount: Integer;
    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    procedure CheckOpen;
    function InternalClose: Boolean;
    function InternalOpen: Boolean;
    procedure GetAnimateParams(var Params);
    function GetActualResHandle: THandle;
    function GetActualResId: Integer;
    procedure GetFrameInfo;
    procedure SetAnimateParams(const Params);
    procedure SetActive(Value: Boolean);
    procedure SetFileName(Value: string);
    procedure SetCenter(Value: Boolean);
    procedure SetCommonAVI(Value: TCommonAVI);
    procedure SetOpen(Value: Boolean);
    procedure SetRepetitions(Value: Integer);
    procedure SetResHandle(Value: THandle);
    procedure SetResId(Value: Integer);
    procedure SetResName(Value: string);
    procedure SetTimers(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetStartFrame(Value: Smallint);
    procedure SetStopFrame(Value: Smallint);
    procedure UpdateActiveState;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    property FrameCount: Integer read FFrameCount;
    property FrameHeight: Integer read FFrameHeight;
    property FrameWidth: Integer read FFrameWidth;
    property Open: Boolean read FOpen write SetOpen;
    procedure Play(FromFrame, ToFrame: Word; Count: Integer);
    procedure Reset;
    procedure Seek(Frame: Smallint);
    procedure Stop;
    property ResHandle: THandle read FResHandle write SetResHandle;
    property ResId: Integer read FResId write SetResId;
    property ResName: string read FResName write SetResName;
  published
    property Align;
    property Active: Boolean read FActive write SetActive default False;
    property Anchors;
    property AutoSize default True;
    property BorderWidth;
    property Center: Boolean read FCenter write SetCenter default True;
    property Color;
    property CommonAVI: TCommonAVI read FCommonAVI write SetCommonAVI default aviNone;
    property Constraints;
    property FileName: string read FFileName write SetFileName;
    property ParentColor;
    property ParentShowHint;
    property Repetitions: Integer read FRepetitions write SetRepetitions default 0;
    property ShowHint;
    property StartFrame: Smallint read FStartFrame write SetStartFrame default 1;
    property StopFrame: Smallint read FStopFrame write SetStopFrame default 0;
    property Timers: Boolean read FTimers write SetTimers default False;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Visible;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

{ TToolBar }

const
  CN_DROPDOWNCLOSED = WM_USER + $1000;

type
  TToolButtonStyle = (tbsButton, tbsCheck, tbsDropDown, tbsSeparator, tbsDivider);

  TToolButtonState = (tbsChecked, tbsPressed, tbsEnabled, tbsHidden,
    tbsIndeterminate, tbsWrap, tbsEllipses, tbsMarked);

  TToolBar = class;
  TToolButton = class;

{ TToolButtonActionLink }

  TToolButtonActionLink = class(TControlActionLink)
  protected
    FClient: TToolButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsDropdownMenuLinked: Boolean; override;
    function IsEnableDropdownLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetDropdownMenu(Value: TPopupMenu); override;
    procedure SetEnableDropdown(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  end;

  TToolButtonActionLinkClass = class of TToolButtonActionLink;

  TToolButton = class(TGraphicControl)
  private
    FAllowAllUp: Boolean;
    FAutoSize: Boolean;
    FDown: Boolean;
    FGrouped: Boolean;
    FImageIndex: TImageIndex;
    FIndeterminate: Boolean;
    FMarked: Boolean;
    FMenuItem: TMenuItem;
    FDropdownMenu: TPopupMenu;
    FEnableDropdown: Boolean;
    FWrap: Boolean;
    FStyle: TToolButtonStyle;
    FUpdateCount: Integer;
    function GetButtonState: Byte;
    function GetIndex: Integer;
    function IsCheckedStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetButtonState(State: Byte);
    procedure SetDown(Value: Boolean);
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetEnableDropdown(Value: Boolean);
    procedure SetGrouped(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetIndeterminate(Value: Boolean);
    procedure SetMarked(Value: Boolean);
    procedure SetMenuItem(Value: TMenuItem);
    procedure SetStyle(Value: TToolButtonStyle);
    procedure SetWrap(Value: Boolean);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    FToolBar: TToolBar;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure RefreshControl; virtual;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetToolBar(AToolBar: TToolBar);
    procedure UpdateControl; virtual;
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function CheckMenuDropdown: Boolean; dynamic;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Index: Integer read GetIndex;
  published
    property Action;
    property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp default False;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption;
    property Down: Boolean read FDown write SetDown stored IsCheckedStored default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropdownMenu: TPopupMenu read FDropdownMenu write SetDropdownMenu;
    property Enabled;
    property EnableDropdown: Boolean read FEnableDropdown write SetEnableDropdown default False;
    property Grouped: Boolean read FGrouped write SetGrouped default False;
    property Height stored False;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property Indeterminate: Boolean read FIndeterminate write SetIndeterminate default False;
    property Marked: Boolean read FMarked write SetMarked default False;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property ParentShowHint;
    property PopupMenu;
    property Wrap: Boolean read FWrap write SetWrap default False;
    property ShowHint;
    property Style: TToolButtonStyle read FStyle write SetStyle default tbsButton;
    property Visible;
    property Width stored IsWidthStored;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TTBCustomDrawFlags = set of (tbNoEdges, tbHiliteHotTrack, tbNoOffset,
    tbNoMark, tbNoEtchedEffect);

  TTBCustomDrawEvent = procedure(Sender: TToolBar; const ARect: TRect;
    var DefaultDraw: Boolean) of object;
  TTBCustomDrawBtnEvent = procedure(Sender: TToolBar; Button: TToolButton;
    State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  TTBAdvancedCustomDrawEvent = procedure(Sender: TToolBar; const ARect: TRect;
    Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  TTBAdvancedCustomDrawBtnEvent = procedure(Sender: TToolBar; Button: TToolButton;
    State: TCustomDrawState; Stage: TCustomDrawStage;
    var Flags: TTBCustomDrawFlags; var DefaultDraw: Boolean) of object;
  TTBCustomizeQueryEvent = procedure(Sender: TToolbar; Index: Integer;
    var Allow: Boolean) of object;
  TTBNewButtonEvent = procedure(Sender: TToolbar; Index: Integer;
    var Button: TToolButton) of object;
  TTBButtonEvent = procedure(Sender: TToolbar; Button: TToolButton) of object;

  TToolBarEnumerator = class
  private
    FIndex: Integer;
    FToolBar: TToolBar;
  public
    constructor Create(AToolBar: TToolBar);
    function GetCurrent: TToolButton;
    function MoveNext: Boolean;
    property Current: TToolButton read GetCurrent;
  end;

  TToolBar = class(TToolWindow)
  private
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FButtons: TList;
    FCaption: string;
    FCanvas: TCanvas;
    FCanvasChanged: Boolean;
    FCustomizable: Boolean;
    FCustomizing: Boolean;
    FShowCaptions: Boolean;
    FList: Boolean;
    FFlat: Boolean;
    FTransparent: Boolean;
    FTransparentSet: Boolean;
    FWrapable: Boolean;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FDisabledImages: TCustomImageList;
    FDisabledImageChangeLink: TChangeLink;
    FHotImages: TCustomImageList;
    FHotImageChangeLink: TChangeLink;
    FIndent: Integer;
    FNewStyle: Boolean;
    FNullBitmap: TBitmap;
    FOldHandle: HBitmap;
    FRestoring: Boolean;
    FUpdateCount: Integer;
    FHeightMargin: Integer;
    FSeparators: Integer;
    FOnAdvancedCustomDraw: TTBAdvancedCustomDrawEvent;
    FOnAdvancedCustomDrawButton: TTBAdvancedCustomDrawBtnEvent;
    FOnCustomDraw: TTBCustomDrawEvent;
    FOnCustomDrawButton: TTBCustomDrawBtnEvent;
    FOnCustomizeCanDelete: TTBCustomizeQueryEvent;
    FOnCustomizeCanInsert: TTBCustomizeQueryEvent;
    FOnCustomizeNewButton: TTBNewButtonEvent;
    FOnCustomized: TNotifyEvent;
    FOnCustomizeDelete: TTBButtonEvent;
    FOnCustomizeAdded: TTBButtonEvent;
    FOnCustomizing: TNotifyEvent;
    FOnCustomizeReset: TNotifyEvent;
    { Toolbar menu support }
    FCaptureChangeCancels: Boolean;
    FInMenuLoop: Boolean;
    FTempMenu: TPopupMenu;
    FButtonMenu: TMenuItem;
    FMenuButton: TToolButton;
    FMenuResult: Boolean;
    FMenuDropped: Boolean;
    FMenu: TMainMenu;
    FCustomizeKeyName: string;
    FCustomizeValueName: string;
    FOurFont: Integer;
    FStockFont: Integer;
    FHideClippedButtons: Boolean;
    function ButtonIndex(OldIndex, ALeft, ATop: Integer): Integer;
    procedure CanvasChanged(Sender: TObject);
    function DoGetButton(NMToolbar: PNMToolbar): Boolean;
    procedure LoadImages(AImages: TCustomImageList);
    function GetButton(Index: Integer): TToolButton;
    function GetButtonCount: Integer;
    procedure GetButtonSize(var AWidth, AHeight: Integer);
    function GetRowCount: Integer;
    procedure SetList(Value: Boolean);
    procedure SetShowCaptions(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetWrapable(Value: Boolean);
    procedure InsertButton(Control: TControl);
    procedure RemoveButton(Control: TControl);
    function RefreshButton(Index: Integer): Boolean;
    procedure UpdateButton(Index: Integer);
    procedure UpdateButtons;
    procedure UpdateButtonState(Index: Integer);
    procedure UpdateButtonStates;
    function UpdateItem(Message, FromIndex, ToIndex: Integer): Boolean;
    function UpdateItem2(Message, FromIndex, ToIndex: Integer): Boolean;
    procedure ClearTempMenu;
    procedure CreateButtons(NewWidth, NewHeight: Integer);
    procedure SetButtonWidth(Value: Integer);
    procedure SetButtonHeight(Value: Integer);
    procedure UpdateImages;
    procedure ImageListChange(Sender: TObject);
    procedure SetImageList(Value: HImageList);
    procedure SetImages(Value: TCustomImageList);
    procedure DisabledImageListChange(Sender: TObject);
    procedure SetDisabledImageList(Value: HImageList);
    procedure SetDisabledImages(Value: TCustomImageList);
    procedure HotImageListChange(Sender: TObject);
    procedure SetHotImageList(Value: HImageList);
    procedure SetHotImages(Value: TCustomImageList);
    procedure SetIndent(Value: Integer);
    procedure SetMenu(const Value: TMainMenu);
    procedure AdjustControl(Control: TControl);
    procedure RecreateButtons;
    procedure RecreateButtonsFromToolbar;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ResizeButtons;
    procedure SaveButtons(Save: Boolean);
    function InternalButtonCount: Integer;
    function ReorderButton(OldIndex, ALeft, ATop: Integer): Integer;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMGetText(var Message: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMNotifyFormat(var Message: TMessage); message WM_NOTIFYFORMAT;
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSysChar(var Message: TWMSysChar); message WM_SYSCHAR;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNChar(var Message: TWMChar); message CN_CHAR;
    procedure CNSysKeyDown(var Message: TWMSysKeyDown); message CN_SYSKEYDOWN;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CNDropDownClosed(var Message: TMessage); message CN_DROPDOWNCLOSED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure SetCustomizable(const Value: Boolean);
    procedure SetHideClippedButtons(const Value: Boolean);
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CancelMenu; dynamic;
    procedure ChangeScale(M, D: Integer); override;
    function CheckMenuDropdown(Button: TToolButton): Boolean; dynamic;
    procedure ClickButton(Button: TToolButton); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawButton(Button: TToolButton; State: TCustomDrawState;
      Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags): Boolean; virtual;
    function DoQueryInsert(Index: Integer): Boolean; virtual;
    function DoQueryDelete(Index: Integer): Boolean; virtual;
    function FindButtonFromAccel(Accel: Word): TToolButton;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure InitMenu(Button: TToolButton); dynamic;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure RepositionButton(Index: Integer);
    procedure RepositionButtons(Index: Integer);
    procedure WndProc(var Message: TMessage); override;
    function WrapButtons(var NewWidth, NewHeight: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    function GetEnumerator: TToolBarEnumerator;
    function TrackMenu(Button: TToolButton): Boolean; dynamic;
    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TToolButton read GetButton;
    property Canvas: TCanvas read FCanvas;
    property CustomizeKeyName: string read FCustomizeKeyName write FCustomizeKeyName;
    property CustomizeValueName: string read FCustomizeValueName write FCustomizeValueName;
    property RowCount: Integer read GetRowCount;
  published
    property Align default alTop;
    property Anchors;
    property AutoSize;
    property BorderWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 22;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 23;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property Customizable: Boolean read FCustomizable write SetCustomizable default False;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EdgeBorders default [ebTop];
    property EdgeInner;
    property EdgeOuter;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property Height default 32;
    property HideClippedButtons: Boolean read FHideClippedButtons write SetHideClippedButtons default False;
    property HotImages: TCustomImageList read FHotImages write SetHotImages;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read FIndent write SetIndent default 0;
    property List: Boolean read FList write SetList default False;
    property Menu: TMainMenu read FMenu write SetMenu;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent: Boolean read FTransparent write SetTransparent stored FTransparentSet;
    property Visible;
    property Wrapable: Boolean read FWrapable write SetWrapable default True;
    property OnAdvancedCustomDraw: TTBAdvancedCustomDrawEvent
      read FOnAdvancedCustomDraw write FOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawButton: TTBAdvancedCustomDrawBtnEvent
      read FOnAdvancedCustomDrawButton write FOnAdvancedCustomDrawButton;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw: TTBCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawButton: TTBCustomDrawBtnEvent read FOnCustomDrawButton
      write FOnCustomDrawButton;
    property OnCustomizeAdded: TTBButtonEvent read FOnCustomizeAdded write FOnCustomizeAdded;
    property OnCustomizeCanInsert: TTBCustomizeQueryEvent read FOnCustomizeCanInsert
      write FOnCustomizeCanInsert;
    property OnCustomizeCanDelete: TTBCustomizeQueryEvent read FOnCustomizeCanDelete
      write FOnCustomizeCanDelete;
    property OnCustomized: TNotifyEvent read FOnCustomized write FOnCustomized;
    property OnCustomizeDelete: TTBButtonEvent read FOnCustomizeDelete write FOnCustomizeDelete;
    property OnCustomizing: TNotifyEvent read FOnCustomizing write FOnCustomizing;
    property OnCustomizeNewButton: TTBNewButtonEvent read FOnCustomizeNewButton
      write FOnCustomizeNewButton;
    property OnCustomizeReset: TNotifyEvent read FOnCustomizeReset write FOnCustomizeReset;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TToolBarDockObject = class(TDragDockObject)
  private
    FEraseDockRect: TRect;
    FErase: Boolean;
  protected
    procedure AdjustDockRect(ARect: TRect); override;
    procedure DrawDragDockImage; override;
    procedure EraseDragDockImage; override;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function ToolDockImage(Erase: Boolean): Boolean; virtual;
  end;

{ TCoolBar }

const
  CN_BANDCHANGE = WM_USER + $1000;

type
  TCoolBar = class;

  TCoolBand = class(TCollectionItem)
  private
    FBorderStyle: TBorderStyle;
    FBreak: Boolean;
    FFixedSize: Boolean;
    FVisible: Boolean;
    FHorizontalOnly: Boolean;
    FImageIndex: TImageIndex;
    FFixedBackground: Boolean;
    FMinHeight: Integer;
    FMinWidth: Integer;
    FColor: TColor;
    FControl: TWinControl;
    FParentColor: Boolean;
    FParentBitmap: Boolean;
    FBitmap: TBitmap;
    FText: string;
    FWidth: Integer;
    FDDB: TBitmap;
    FID: Integer;
    function CoolBar: TCoolBar;
    function IsColorStored: Boolean;
    function IsBitmapStored: Boolean;
    procedure BitmapChanged(Sender: TObject);
    function GetHeight: Integer;
    function GetVisible: Boolean;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetBreak(Value: Boolean);
    procedure SetFixedSize(Value: Boolean);
    procedure SetMinHeight(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetHorizontalOnly(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetFixedBackground(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetControl(Value: TWinControl);
    procedure SetParentColor(Value: Boolean);
    procedure SetParentBitmap(Value: Boolean);
    procedure SetBitmap(Value: TBitmap);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
  protected
    function GetDisplayName: string; override;
    procedure ParentColorChanged; dynamic;
    procedure ParentBitmapChanged; dynamic;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Height: Integer read GetHeight;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap stored IsBitmapStored;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property Break: Boolean read FBreak write SetBreak default True;
    property Color: TColor read FColor write SetColor stored IsColorStored default clBtnFace;
    property Control: TWinControl read FControl write SetControl;
    property FixedBackground: Boolean read FFixedBackground write SetFixedBackground default True;
    property FixedSize: Boolean read FFixedSize write SetFixedSize default False;
    property HorizontalOnly: Boolean read FHorizontalOnly write SetHorizontalOnly default False;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property MinHeight: Integer read FMinHeight write SetMinHeight default 25;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ParentBitmap: Boolean read FParentBitmap write SetParentBitmap default True;
    property Text: string read FText write SetText;
    property Visible: Boolean read GetVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth;
  end;

  TCoolBands = class(TCollection)
  private
    FCoolBar: TCoolBar;
    FVisibleCount: Longword;
    function GetItem(Index: Integer): TCoolBand;
    procedure SetItem(Index: Integer; Value: TCoolBand);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    function HaveGraphic: Boolean;
  public
    constructor Create(CoolBar: TCoolBar);
    function Add: TCoolBand;
    function FindBand(AControl: TControl): TCoolBand;
    property CoolBar: TCoolBar read FCoolBar;
    property Items[Index: Integer]: TCoolBand read GetItem write SetItem; default;
  end;

  TCoolBandMaximize = (bmNone, bmClick, bmDblClick);

  TCoolBar = class(TToolWindow)
  private
    FBands: TCoolBands;
    FBandBorderStyle: TBorderStyle;
    FBandMaximize: TCoolBandMaximize;
    FBitmap: TBitmap;
    FCaptionFont: TFont;
    FCaptionFontHeight: Integer;
    FDDB: TBitmap;
    FFixedSize: Boolean;
    FFixedOrder: Boolean;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FShowText: Boolean;
    FVertical: Boolean;
    FTrackDrag: TSmallPoint;
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
    procedure BeginUpdate;
    procedure BitmapChanged(Sender: TObject);
    procedure EndUpdate;
    function IsAutoSized: Boolean;
    function IsBackgroundDirty: Boolean;
    function GetAlign: TAlign;
    function GetCaptionFont: HFONT;
    function GetCaptionFontHeight: Integer;
    function GetCaptionSize(Band: TCoolBand): Integer;
    function GetRowHeight(Index: Integer): Integer;
    procedure RefreshControl(Band: TCoolBand);
    procedure SetAlign(Value: TAlign);
    procedure SetBands(Value: TCoolBands);
    procedure SetBandBorderStyle(Value: TBorderStyle);
    procedure SetBandMaximize(Value: TCoolBandMaximize);
    procedure SetBitmap(Value: TBitmap);
    procedure SetFixedSize(Value: Boolean);
    procedure SetFixedOrder(Value: Boolean);
    procedure SetImageList(Value: HImageList);
    procedure SetImages(Value: TCustomImageList);
    procedure SetShowText(Value: Boolean);
    procedure SetVertical(Value: Boolean);
    procedure ImageListChange(Sender: TObject);
    function PtInGripRect(const Pos: TPoint; var Band: TCoolBand): Integer;
    function ReadBands: Boolean;
    function UpdateItem(Message, FromIndex, ToIndex: Integer): Boolean;
    procedure UpdateBand(Index: Integer);
    procedure UpdateBands;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMNotifyFormat(var Message: TMessage); message WM_NOTIFYFORMAT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CNBandChange(var Message: TMessage); message CN_BANDCHANGE;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Change; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetPalette: HPALETTE; override;
    function HitTest(const Pos: TPoint): TCoolBand;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
  published
    property Align read GetAlign write SetAlign default alTop;
    property Anchors;
    property AutoSize;
    property BandBorderStyle: TBorderStyle read FBandBorderStyle write SetBandBorderStyle default bsSingle;
    property BandMaximize: TCoolBandMaximize read FBandMaximize write SetBandMaximize default bmClick;
    property Bands: TCoolBands read FBands write SetBands;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EdgeBorders;
    property EdgeInner;
    property EdgeOuter;
    property Enabled;
    property FixedSize: Boolean read FFixedSize write SetFixedSize default False;
    property FixedOrder: Boolean read FFixedOrder write SetFixedOrder default False;
    property Font;
    property Images: TCustomImageList read FImages write SetImages;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property PopupMenu;
    property ShowHint;
    property ShowText: Boolean read FShowText write SetShowText default True;
    property Vertical: Boolean read FVertical write SetVertical default False;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ Calendar common control support }

  TCommonCalendar = class;

  ECommonCalendarError = class(Exception);

  TMonthCalColors = class(TPersistent)
  private
    Owner: TCommonCalendar;
    FBackColor: TColor;
    FTextColor: TColor;
    FTitleBackColor: TColor;
    FTitleTextColor: TColor;
    FMonthBackColor: TColor;
    FTrailingTextColor: TColor;
    procedure SetColor(Index: Integer; Value: TColor);
    procedure SetAllColors;
  public
    constructor Create(AOwner: TCommonCalendar);
    procedure Assign(Source: TPersistent); override;
  published
    property BackColor: TColor index 0 read FBackColor write SetColor default clWindow;
    property TextColor: TColor index 1 read FTextColor write SetColor default clWindowText;
    property TitleBackColor: TColor index 2 read FTitleBackColor write SetColor default clActiveCaption;
    property TitleTextColor: TColor index 3 read FTitleTextColor write SetColor default clWhite;
    property MonthBackColor: TColor index 4 read FMonthBackColor write SetColor default clWhite;
    property TrailingTextColor: TColor index 5 read FTrailingTextColor
      write SetColor default clInactiveCaptionText;
  end;

  TCalDayOfWeek = (dowMonday, dowTuesday, dowWednesday, dowThursday,
    dowFriday, dowSaturday, dowSunday, dowLocaleDefault);

  TOnGetMonthInfoEvent = procedure(Sender: TObject; Month: LongWord;
    var MonthBoldInfo: LongWord) of object;

  TCommonCalendar = class(TWinControl)
  private
    FCalColors: TMonthCalColors;
    FCalExceptionClass: ExceptClass;
    FDateTime: TDateTime;
    FEndDate: TDate;
    FFirstDayOfWeek: TCalDayOfWeek;
    FMaxDate: TDate;
    FMaxSelectRange: Integer;
    FMinDate: TDate;
    FMonthDelta: Integer;
    FMultiSelect: Boolean;
    FShowToday: Boolean;
    FShowTodayCircle: Boolean;
    FWeekNumbers: Boolean;
    FOnGetMonthInfo: TOnGetMonthInfoEvent;
    function DoStoreEndDate: Boolean;
    function DoStoreMaxDate: Boolean;
    function DoStoreMinDate: Boolean;
    function GetDate: TDate;
    procedure SetCalColors(Value: TMonthCalColors);
    procedure SetDate(Value: TDate);
    procedure SetDateTime(Value: TDateTime);
    procedure SetEndDate(Value: TDate);
    procedure SetFirstDayOfWeek(Value: TCalDayOfWeek);
    procedure SetMaxDate(Value: TDate);
    procedure SetMaxSelectRange(Value: Integer);
    procedure SetMinDate(Value: TDate);
    procedure SetMonthDelta(Value: Integer);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetRange(MinVal, MaxVal: TDate);
    procedure SetSelectedRange(Date, EndDate: TDate);
    procedure SetShowToday(Value: Boolean);
    procedure SetShowTodayCircle(Value: Boolean);
    procedure SetWeekNumbers(Value: Boolean);
  protected
    procedure CheckEmptyDate; virtual;
    procedure CheckValidDate(Value: TDate); virtual;
    procedure CreateWnd; override;
    function GetCalendarHandle: HWND; virtual; abstract;
    function GetCalStyles: DWORD; virtual;
    function MsgSetCalColors(ColorIndex: Integer; ColorValue: TColor): Boolean; virtual; abstract;
    function MsgSetDateTime(Value: TSystemTime): Boolean; virtual; abstract;
    function MsgSetRange(Flags: Integer; SysTime: PSystemTime): Boolean; virtual; abstract;
    property CalColors: TMonthCalColors read FCalColors write SetCalColors;
    property CalendarHandle: HWND read GetCalendarHandle;
    property CalExceptionClass: ExceptClass read FCalExceptionClass write FCalExceptionClass;
    property Date: TDate read GetDate write SetDate;
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property EndDate: TDate read FEndDate write SetEndDate stored DoStoreEndDate;
    property FirstDayOfWeek: TCalDayOfWeek read FFirstDayOfWeek write SetFirstDayOfWeek
      default dowLocaleDefault;
    property MaxDate: TDate read FMaxDate write SetMaxDate stored DoStoreMaxDate;
    property MaxSelectRange: Integer read FMaxSelectRange write SetMaxSelectRange default 31;
    property MinDate: TDate read FMinDate write SetMinDate stored DoStoreMinDate;
    property MonthDelta: Integer read FMonthDelta write SetMonthDelta default 1;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ShowToday: Boolean read FShowToday write SetShowToday default True;
    property ShowTodayCircle: Boolean read FShowTodayCircle write
      SetShowTodayCircle default True;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers default False;
    property OnGetMonthInfo: TOnGetMonthInfoEvent read FOnGetMonthInfo write FOnGetMonthInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BoldDays(Days: array of LongWord; var MonthBoldInfo: LongWord);
  end;

{ TMonthCalendar }

  EMonthCalError = class(ECommonCalendarError);

  TMonthCalendar = class(TCommonCalendar)
  private
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
      MaxHeight: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetCalendarHandle: HWND; override;
    function MsgSetCalColors(ColorIndex: Integer; ColorValue: TColor): Boolean; override;
    function MsgSetDateTime(Value: TSystemTime): Boolean; override;
    function MsgSetRange(Flags: Integer; SysTime: PSystemTime): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderWidth;
    property BiDiMode;
    property CalColors;
    property Constraints;
    property MultiSelect;  // must be before date stuff
    property Date;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EndDate;
    property FirstDayOfWeek;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxDate;
    property MaxSelectRange;
    property MinDate;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowToday;
    property ShowTodayCircle;
    property TabOrder;
    property TabStop;
    property Visible;
    property WeekNumbers;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetMonthInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TDateTimePicker }

  EDateTimeError = class(ECommonCalendarError);

  TDateTimeKind = (dtkDate, dtkTime);
  TDTDateMode = (dmComboBox, dmUpDown);
  TDTDateFormat = (dfShort, dfLong);
  TDTCalAlignment = (dtaLeft, dtaRight);

  TDTParseInputEvent = procedure(Sender: TObject; const UserString: string;
    var DateAndTime: TDateTime; var AllowChange: Boolean) of object;

  TDateTimeColors = TMonthCalColors;  // for backward compatibility

  TDateTimePicker = class(TCommonCalendar)
  private
    FCalAlignment: TDTCalAlignment;
    FChanging: Boolean;
    FChecked: Boolean;
    FDateFormat: TDTDateFormat;
    FDateMode: TDTDateMode;
    FDroppedDown: Boolean;
    FKind: TDateTimeKind;
    FLastChange: TSystemTime;
    FParseInput: Boolean;
    FShowCheckbox: Boolean;
    FOnUserInput: TDTParseInputEvent;
    FOnCloseUp: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FFormat: String;
    procedure AdjustHeight;
    function GetTime: TTime;
    procedure SetCalAlignment(Value: TDTCalAlignment);
    procedure SetChecked(Value: Boolean);
    procedure SetDateMode(Value: TDTDateMode);
    procedure SetDateFormat(Value: TDTDateFormat);
    procedure SetKind(Value: TDateTimeKind);
    procedure SetParseInput(Value: Boolean);
    procedure SetShowCheckbox(Value: Boolean);
    procedure SetTime(Value: TTime);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure SetFormat(const Value: String);
  protected
    procedure CheckEmptyDate; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Change; dynamic;
    function GetCalendarHandle: HWND; override;
    function MsgSetCalColors(ColorIndex: Integer; ColorValue: TColor): Boolean; override;
    function MsgSetDateTime(Value: TSystemTime): Boolean; override;
    function MsgSetRange(Flags: Integer; SysTime: PSystemTime): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property DateTime;
    property DroppedDown: Boolean read FDroppedDown;
  published
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property CalAlignment: TDTCalAlignment read FCalAlignment write SetCalAlignment default dtaLeft;
    property CalColors;
    property Constraints;
    // The Date, Time, ShowCheckbox, and Checked properties must be in this order:
    property Date;
    property Format: String read FFormat write SetFormat;
    property Time: TTime read GetTime write SetTime;
    property ShowCheckbox: Boolean read FShowCheckbox write SetShowCheckbox default False;
    property Checked: Boolean read FChecked write SetChecked default True;
    property Color stored True default clWindow;
    property DateFormat: TDTDateFormat read FDateFormat write SetDateFormat default dfShort;
    property DateMode: TDTDateMode read FDateMode write SetDateMode default dmComboBox;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property Kind: TDateTimeKind read FKind write SetKind default dtkDate;
    property MaxDate;
    property MinDate;
    property ParseInput: Boolean read FParseInput write SetParseInput default False;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnContextPopup;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUserInput: TDTParseInputEvent read FOnUserInput write FOnUserInput;
  end;

{ TPageScroller }

  TPageScrollerOrientation = (soHorizontal, soVertical);
  TPageScrollerButton = (sbFirst, sbLast);
  TPageScrollerButtonState = (bsNormal, bsInvisible, bsGrayed, bsDepressed, bsHot);

  TPageScrollEvent = procedure (Sender: TObject; Shift: TShiftState; X, Y: Integer;
    Orientation: TPageScrollerOrientation; var Delta: Integer) of object;

  TPageScroller = class(TWinControl)
  private
    FAutoScroll: Boolean;
    FButtonSize: Integer;
    FControl: TWinControl;
    FDragScroll: Boolean;
    FMargin: Integer;
    FOrientation: TPageScrollerOrientation;
    FPosition: Integer;
    FPreferredSize: Integer;
    FOnScroll: TPageScrollEvent;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure DoSetControl(Value: TWinControl);
    procedure SetAutoScroll(Value: Boolean);
    procedure SetButtonSize(Value: Integer);
    procedure SetControl(Value: TWinControl);
    procedure SetDragScroll(Value: Boolean);
    procedure SetMargin(Value: Integer);
    procedure SetOrientation(Value: TPageScrollerOrientation);
    procedure SetPosition(Value: Integer);
    procedure UpdatePreferredSize;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Scroll(Shift: TShiftState; X, Y: Integer;
      Orientation: TPageScrollerOrientation; var Delta: Integer); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    function GetButtonState(Button: TPageScrollerButton): TPageScrollerButtonState;
  published
    property Align;
    property Anchors;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default False;
    property BorderWidth;
    property ButtonSize: Integer read FButtonSize write SetButtonSize default 12;
    property Color;
    property Constraints;
    property Control: TWinControl read FControl write SetControl;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DragScroll: Boolean read FDragScroll write SetDragScroll default True;
    property Enabled;
    property Font;
    property Margin: Integer read FMargin write SetMargin default 0;
    property Orientation: TPageScrollerOrientation read FOrientation write SetOrientation default soHorizontal;
    property ParentBackground default True;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read FPosition write SetPosition default 0;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnResize;
    property OnScroll: TPageScrollEvent read FOnScroll write FOnScroll;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TCustomComboBoxEx }

  TComboExItem = class(TListControlItem)
  private
    FSelectedImageIndex: TImageIndex;
    FOverlayImageIndex: TImageIndex;
    FIndent: Integer;
  protected
    procedure SetOverlayImageIndex(const Value: TImageIndex); virtual;
    procedure SetSelectedImageIndex(const Value: TImageIndex); virtual;
    procedure SetCaption(const Value: String); override;
    procedure SetData(const Value: Pointer); override;
    procedure SetDisplayName(const Value: String); override;
    procedure SetImageIndex(const Value: TImageIndex); override;
    procedure SetIndex(Value: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Indent: Integer read FIndent write FIndent default -1;
    property OverlayImageIndex: TImageIndex read FOverlayImageIndex
      write SetOverlayImageIndex default -1;
    property SelectedImageIndex: TImageIndex read FSelectedImageIndex
      write SetSelectedImageIndex default -1;
  end;

  TComboExItems = class(TListControlItems)
  private
    function GetComboItem(const Index: Integer): TComboExItem;
  protected
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
    procedure SetItem(const Index: Integer); virtual;
  public
    function Add: TComboExItem;
    function AddItem(const Caption: String; const ImageIndex, SelectedImageIndex,
      OverlayImageIndex, Indent: Integer; Data: Pointer): TComboExItem;
    function Insert(Index: Integer): TComboExItem;
    property ComboItems[const Index: Integer]: TComboExItem read GetComboItem;
  end;

  TComboExItemsClass = class of TComboExItems;
  TComboExItemClass = class of TComboExItem;

  TCustomComboBoxEx = class;

  TComboBoxExStrings = class(TCustomComboBoxStrings)
  private
    FItems: TComboExItems;
    function GetSortType: TListItemsSortType;
    procedure SetItems(const Value: TComboExItems);
    procedure SetSortType(const Value: TListItemsSortType);
  protected
    function GetItemsClass: TComboExItemsClass; virtual;
    function GetItemClass: TComboExItemClass; virtual;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(Owner: TCustomComboBoxEx);
    destructor Destroy; override;
    function Add(const S: String): Integer; override;
    function AddItem(const Caption: String; const ImageIndex, SelectedImageIndex,
      OverlayImageIndex, Indent: Integer; Data: Pointer): TComboExItem;
    function AddObject(const S: String; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1: Integer; Index2: Integer); override;
    function Get(Index: Integer): String; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function IndexOf(const S: String): Integer; override;
    function IndexOfName(const Name: String): Integer; override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure Move(CurIndex: Integer; NewIndex: Integer); override;
    property SortType: TListItemsSortType read GetSortType write SetSortType;
    property ItemsEx: TComboExItems read FItems write SetItems;
  end;

{ TCustomComboBoxEx }

  TComboBoxExStyle = (csExDropDown, csExSimple, csExDropDownList);
  TComboBoxExStyleEx = (csExCaseSensitive, csExNoEditImage, csExNoEditImageIndent,
                        csExNoSizeLimit, csExPathWordBreak);
  TComboBoxExStyles = set of TComboBoxExStyleEx;

  TAutoCompleteOption = (acoAutoSuggest, acoAutoAppend, acoSearch,
    acoFilterPrefixes, acoUseTab, acoUpDownKeyDropsList, acoRtlReading);
  TAutoCompleteOptions = set of TAutoCompleteOption;

  TCustomComboBoxEx = class(TCustomCombo)
  private
    FAutoCompleteIntf: IAutoComplete;
    FAutoCompleteOptions: TAutoCompleteOptions;
    FComboBoxExHandle: HWND;
    FComboBoxExDefProc: Pointer;
    FComboBoxExInstance: Pointer;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FMemStream: TCollection;
    FReading: Boolean;
    FStyle: TComboBoxExStyle;
    FStyleEx: TComboBoxExStyles;
    FItemsEx: TComboExItems;
    FOnBeginEdit: TNotifyEvent;
    FOnEndEdit: TNotifyEvent;
    function GetSelText: String;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetSelText(const Value: String);
    procedure SetStyle(Value: TComboBoxExStyle);
    procedure SetItemsEx(const Value: TComboExItems);
    procedure SetStyleEx(const Value: TComboBoxExStyles);
    function IsItemsExStored: Boolean;
    function GetDropDownCount: Integer;
    procedure UpdateAutoComplete;
    procedure SetAutoCompleteOptions(const Value: TAutoCompleteOptions);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure ComboExWndProc(var Message: TMessage);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetItemsClass: TCustomComboBoxStringsClass; override;
    function GetItemCount: Integer; override;
    function GetItemHt: Integer; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetDropDownCount(const Value: Integer); override;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Focused: Boolean; override;
    property AutoCompleteOptions: TAutoCompleteOptions read FAutoCompleteOptions
      write SetAutoCompleteOptions default [acoAutoAppend];
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount;
    property Images: TCustomImageList read FImages write SetImages;
    property ItemsEx: TComboExItems read FItemsEx write SetItemsEx stored IsItemsExStored;
    property SelText: string read GetSelText write SetSelText;
    property Style: TComboBoxExStyle read FStyle write SetStyle default csExDropDown;
    property StyleEx: TComboBoxExStyles read FStyleEx write SetStyleEx default [];
    property OnBeginEdit: TNotifyEvent read FOnBeginEdit write FOnBeginEdit;
    property OnEndEdit: TNotifyEvent read FOnEndEdit write FOnEndEdit;
  end;

{ TComboBoxEx }

  TComboBoxEx = class(TCustomComboBoxEx)
  published
    property AutoCompleteOptions default [acoAutoAppend];
    property ItemsEx;
    property Style; {Must be published before Items}
    property StyleEx;
    property Action;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnBeginEdit;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndEdit;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
//    property Items;
    property Images;
    property DropDownCount;
  end;

{ TComboBoxExActionLink }

  TComboBoxExActionLink = class(TListActionLink)
  protected
    procedure AddItem(AnItem: TListControlItem); override;
    procedure AddItem(ACaption: String; AImageIndex: Integer;
      DataPtr: Pointer); override;
  end;

function InitCommonControl(CC: Integer): Boolean;
procedure CheckCommonControl(CC: Integer);

const
  ComCtlVersionIE3 = $00040046;
  ComCtlVersionIE4 = $00040047;
  ComCtlVersionIE401 = $00040048;
  ComCtlVersionIE5 = $00050050;
  ComCtlVersionIE501 = $00050051;
  ComCtlVersionIE6 = $00060000;

function GetComCtlVersion: Integer;
procedure CheckToolMenuDropdown(ToolButton: TToolButton);

implementation

