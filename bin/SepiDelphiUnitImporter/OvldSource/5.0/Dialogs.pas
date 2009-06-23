{*******************************************************}
{                                                       }
{       CodeGear Delphi Visual Component Library        }
{                                                       }
{           Copyright (c) 1995-2007 CodeGear            }
{                                                       }
{*******************************************************}

unit Dialogs;

{$R-,T-,H+,X+}

interface

{$IFDEF LINUX}
uses WinUtils, Windows, Messages, SysUtils, CommDlg,
  Printers, Classes, Graphics, Controls, Forms, StdCtrls;
{$ENDIF}
{$IFDEF MSWINDOWS}
uses Windows, Messages, SysUtils, CommDlg,
  Printers, Classes, Graphics, Controls, Forms, StdCtrls, ShlObj, CommCtrl;
{$ENDIF}
const


(*$HPPEMIT '#include <objbase.h>' *)
(*$HPPEMIT '#include <ShObjIdl.h>' *)

{ Maximum number of custom colors in color dialog }

  MaxCustomColors = 16;

type

{ TCommonDialog }

  TCommonDialog = class(TComponent)
  private
    FCtl3D: Boolean;
    FDefWndProc: Pointer;
    FHelpContext: THelpContext;
    FHandle: HWnd;
    FRedirector: TWinControl;
    FObjectInstance: Pointer;
    FTemplate: PChar;
    FTemplateModule: HINST;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMInitDialog(var Message: TWMInitDialog); message WM_INITDIALOG;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure MainWndProc(var Message: TMessage);
  protected
    procedure DoClose; dynamic;
    procedure DoShow; dynamic;
    procedure WndProc(var Message: TMessage); virtual;
    function MessageHook(var Msg: TMessage): Boolean; virtual;
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; virtual;
    property Template: PChar read FTemplate write FTemplate;
    property TemplateModule: HINST read FTemplateModule write FTemplateModule;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; overload; virtual; 
    function Execute(ParentWnd: HWND): Boolean; overload; virtual; abstract; 
    procedure DefaultHandler(var Message); override;
    property Handle: HWnd read FHandle;
  published
    property Ctl3D: Boolean read FCtl3D write FCtl3D default True;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

{ TOpenDialog }

  TOpenOption = (ofReadOnly, ofOverwritePrompt, ofHideReadOnly,
    ofNoChangeDir, ofShowHelp, ofNoValidate, ofAllowMultiSelect,
    ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofCreatePrompt,
    ofShareAware, ofNoReadOnlyReturn, ofNoTestFileCreate, ofNoNetworkButton,
    ofNoLongNames, ofOldStyleDialog, ofNoDereferenceLinks, ofEnableIncludeNotify,
    ofEnableSizing, ofDontAddToRecent, ofForceShowHidden);
  TOpenOptions = set of TOpenOption;

  TOpenOptionEx = (ofExNoPlacesBar);
  TOpenOptionsEx = set of TOpenOptionEx;

  TFileEditStyle = (fsEdit, fsComboBox);
  TOFNotifyEx = type CommDlg.TOFNotifyEx;
  {$NODEFINE TOFNotifyEx}
  TIncludeItemEvent = procedure (const OFN: TOFNotifyEx; var Include: Boolean) of object;

  TOpenDialog = class(TCommonDialog)
  private
    FHistoryList: TStrings;
    FOptions: TOpenOptions;
    FFilter: string;
    FFilterIndex: Integer;
    FCurrentFilterIndex: Integer;
    FInitialDir: string;
    FTitle: string;
    FDefaultExt: string;
    FFileName: TFileName;
    FFiles: TStrings;
    FFileEditStyle: TFileEditStyle;
    FOnSelectionChange: TNotifyEvent;
    FOnFolderChange: TNotifyEvent;
    FOnTypeChange: TNotifyEvent;
    FOnCanClose: TCloseQueryEvent;
    FOnIncludeItem: TIncludeItemEvent;
    FOptionsEx: TOpenOptionsEx;
    function GetFileName: TFileName;
    function GetFilterIndex: Integer;
    procedure ReadFileEditStyle(Reader: TReader);
    procedure SetHistoryList(Value: TStrings);
    procedure SetInitialDir(const Value: string);
  protected
    function CanClose(var OpenFileName: TOpenFileName): Boolean;
    function DoCanClose: Boolean; dynamic;
    function DoExecute(Func: Pointer): Bool; overload;
    function DoExecute(Func: Pointer; ParentWnd: HWND): Bool; overload;
    procedure DoSelectionChange; dynamic;
    procedure DoFolderChange; dynamic;
    procedure DoTypeChange; dynamic;
    procedure DoIncludeItem(const OFN: TOFNotifyEx; var Include: Boolean); dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetFileNames(var OpenFileName: TOpenFileName);
    function GetStaticRect: TRect; virtual;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(ParentWnd: HWND): Boolean; override;
    property FileEditStyle: TFileEditStyle read FFileEditStyle write FFileEditStyle;
    property Files: TStrings read FFiles;
    property HistoryList: TStrings read FHistoryList write SetHistoryList;
  published
    property DefaultExt: string read FDefaultExt write FDefaultExt;
    property FileName: TFileName read GetFileName write FFileName;
    property Filter: string read FFilter write FFilter;
    property FilterIndex: Integer read GetFilterIndex write FFilterIndex default 1;
    property InitialDir: string read FInitialDir write SetInitialDir;
    property Options: TOpenOptions read FOptions write FOptions default [ofHideReadOnly, ofEnableSizing];
    property OptionsEx: TOpenOptionsEx read FOptionsEx write FOptionsEx default [];
    property Title: string read FTitle write FTitle;
    property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
    property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
    property OnIncludeItem: TIncludeItemEvent read FOnIncludeItem write FOnIncludeItem;
  end;

{ TSaveDialog }

  TSaveDialog = class(TOpenDialog)
    function Execute(ParentWnd: HWND): Boolean; override;
  end;

{ TColorDialog }

  TColorDialogOption = (cdFullOpen, cdPreventFullOpen, cdShowHelp,
    cdSolidColor, cdAnyColor);
  TColorDialogOptions = set of TColorDialogOption;

  TCustomColors = array[0..MaxCustomColors - 1] of Longint;

  TColorDialog = class(TCommonDialog)
  private
    FColor: TColor;
    FOptions: TColorDialogOptions;
    FCustomColors: TStrings;
    procedure SetCustomColors(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(ParentWnd: HWND): Boolean; override;
  published
    property Color: TColor read FColor write FColor default clBlack;
    property Ctl3D default True;
    property CustomColors: TStrings read FCustomColors write SetCustomColors;
    property Options: TColorDialogOptions read FOptions write FOptions default [];
  end;

{ TFontDialog }

  TFontDialogOption = (fdAnsiOnly, fdTrueTypeOnly, fdEffects,
    fdFixedPitchOnly, fdForceFontExist, fdNoFaceSel, fdNoOEMFonts,
    fdNoSimulations, fdNoSizeSel, fdNoStyleSel,  fdNoVectorFonts,
    fdShowHelp, fdWysiwyg, fdLimitSize, fdScalableOnly, fdApplyButton);
  TFontDialogOptions = set of TFontDialogOption;

  TFontDialogDevice = (fdScreen, fdPrinter, fdBoth);

  TFDApplyEvent = procedure(Sender: TObject; Wnd: HWND) of object;

  TFontDialog = class(TCommonDialog)
  private
    FFont: TFont;
    FDevice: TFontDialogDevice;
    FOptions: TFontDialogOptions;
    FOnApply: TFDApplyEvent;
    FMinFontSize: Integer;
    FMaxFontSize: Integer;
    FFontCharsetModified: Boolean;
    FFontColorModified: Boolean;
    procedure DoApply(Wnd: HWND);
    procedure SetFont(Value: TFont);
    procedure UpdateFromLogFont(const LogFont: TLogFont);
  protected
    procedure Apply(Wnd: HWND); dynamic;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(ParentWnd: HWND): Boolean; override;
  published
    property Font: TFont read FFont write SetFont;
    property Device: TFontDialogDevice read FDevice write FDevice default fdScreen;
    property MinFontSize: Integer read FMinFontSize write FMinFontSize default 0;
    property MaxFontSize: Integer read FMaxFontSize write FMaxFontSize default 0;
    property Options: TFontDialogOptions read FOptions write FOptions default [fdEffects];
    property OnApply: TFDApplyEvent read FOnApply write FOnApply;
  end;

{ TPrinterSetupDialog }

  TPrinterSetupDialog = class(TCommonDialog)
  public
    function Execute(ParentWnd: HWND): Boolean; override;
  end;

{ TPrintDialog }

  TPrintRange = (prAllPages, prSelection, prPageNums);
  TPrintDialogOption = (poPrintToFile, poPageNums, poSelection, poWarning,
    poHelp, poDisablePrintToFile);
  TPrintDialogOptions = set of TPrintDialogOption;

  TPrintDialog = class(TCommonDialog)
  private
    FFromPage: Integer;
    FToPage: Integer;
    FCollate: Boolean;
    FOptions: TPrintDialogOptions;
    FPrintToFile: Boolean;
    FPrintRange: TPrintRange;
    FMinPage: Integer;
    FMaxPage: Integer;
    FCopies: Integer;
    procedure SetNumCopies(Value: Integer);
  public
    function Execute(ParentWnd: HWND): Boolean; override;
  published
    property Collate: Boolean read FCollate write FCollate default False;
    property Copies: Integer read FCopies write SetNumCopies default 0;
    property FromPage: Integer read FFromPage write FFromPage default 0;
    property MinPage: Integer read FMinPage write FMinPage default 0;
    property MaxPage: Integer read FMaxPage write FMaxPage default 0;
    property Options: TPrintDialogOptions read FOptions write FOptions default [];
    property PrintToFile: Boolean read FPrintToFile write FPrintToFile default False;
    property PrintRange: TPrintRange read FPrintRange write FPrintRange default prAllPages;
    property ToPage: Integer read FToPage write FToPage default 0;
  end;

  TPrinterOrientation = Printers.TPrinterOrientation;  // required for Form Designer
  TPageSetupDialogOption = (psoDefaultMinMargins, psoDisableMargins,
    psoDisableOrientation, psoDisablePagePainting, psoDisablePaper, psoDisablePrinter,
    psoMargins, psoMinMargins, psoShowHelp, psoWarning, psoNoNetworkButton);
  TPageSetupDialogOptions = set of TPageSetupDialogOption;
  TPrinterKind = (pkDotMatrix, pkHPPCL);
  TPageType = (ptEnvelope, ptPaper);
  TPageSetupBeforePaintEvent = procedure (Sender: TObject; const PaperSize: SmallInt;
    const Orientation: TPrinterOrientation; const PageType: TPageType;
    var DoneDrawing: Boolean) of object;
  TPageMeasureUnits = (pmDefault, pmMillimeters, pmInches);
  TPaintPageEvent = procedure(Sender: TObject; Canvas: TCanvas; PageRect: TRect;
    var DoneDrawing: Boolean) of object;

{ TPageSetupDialog }

  TPageSetupDialog = class(TCommonDialog)
  private
    FOptions: TPageSetupDialogOptions;
    FMinMarginLeft: Integer;
    FMinMarginTop: Integer;
    FMinMarginRight: Integer;
    FMinMarginBottom: Integer;
    FMarginLeft: Integer;
    FMarginTop: Integer;
    FMarginRight: Integer;
    FMarginBottom: Integer;
    FPageWidth: Integer;
    FPageHeight: Integer;
    FPageSetupDlgRec: TPageSetupDlg;
    FBeforePaint: TPageSetupBeforePaintEvent;
    FUnits: TPageMeasureUnits;
    FOnDrawRetAddress: TPaintPageEvent;
    FOnDrawMinMargin: TPaintPageEvent;
    FOnDrawEnvStamp: TPaintPageEvent;
    FOnDrawFullPage: TPaintPageEvent;
    FOnDrawGreekText: TPaintPageEvent;
    FOnDrawMargin: TPaintPageEvent;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(ParentWnd: HWND): Boolean; override;
    function GetDefaults: Boolean;
    property PageSetupDlgRec: TPageSetupDlg read FPageSetupDlgRec;
  published
    property MinMarginLeft: Integer read FMinMarginLeft write FMinMarginLeft;
    property MinMarginTop: Integer read FMinMarginTop write FMinMarginTop;
    property MinMarginRight: Integer read FMinMarginRight write FMinMarginRight;
    property MinMarginBottom: Integer read FMinMarginBottom write FMinMarginBottom;
    property MarginLeft: Integer read FMarginLeft write FMarginLeft;
    property MarginTop: Integer read FMarginTop write FMarginTop;
    property MarginRight: Integer read FMarginRight write FMarginRight;
    property MarginBottom: Integer read FMarginBottom write FMarginBottom;
    property Options: TPageSetupDialogOptions read FOptions write FOptions
      default [psoDefaultMinMargins];
    property PageWidth: Integer read FPageWidth write FPageWidth;
    property PageHeight: Integer read FPageHeight write FPageHeight;
    property Units: TPageMeasureUnits read FUnits write FUnits default pmDefault;
    property BeforePaint: TPageSetupBeforePaintEvent read FBeforePaint
      write FBeforePaint;
    property OnDrawFullPage: TPaintPageEvent read FOnDrawFullPage write FOnDrawFullPage;
    property OnDrawMinMargin: TPaintPageEvent read FOnDrawMinMargin write FOnDrawMinMargin;
    property OnDrawMargin: TPaintPageEvent read FOnDrawMargin write FOnDrawMargin;
    property OnDrawGreekText: TPaintPageEvent read FOnDrawGreekText write FOnDrawGreekText;
    property OnDrawEnvStamp: TPaintPageEvent read FOnDrawEnvStamp write FOnDrawEnvStamp;
    property OnDrawRetAddress: TPaintPageEvent read FOnDrawRetAddress write FOnDrawRetAddress;                
  end;

{ TFindDialog }

  TFindOption = (frDown, frFindNext, frHideMatchCase, frHideWholeWord,
    frHideUpDown, frMatchCase, frDisableMatchCase, frDisableUpDown,
    frDisableWholeWord, frReplace, frReplaceAll, frWholeWord, frShowHelp);
  TFindOptions = set of TFindOption;

  TFindReplaceFunc = function(var FindReplace: TFindReplace): HWnd stdcall;

  TFindDialog = class(TCommonDialog)
  private
    FOptions: TFindOptions;
    FPosition: TPoint;
    FFindReplaceFunc: TFindReplaceFunc;
    FOnFind: TNotifyEvent;
    FOnReplace: TNotifyEvent;
    FFindHandle: HWnd;
    FFindReplace: TFindReplace;
    FFindText: array[0..255] of Char;
    FReplaceText: array[0..255] of Char;
    function GetFindText: string;
    function GetLeft: Integer;
    function GetPosition: TPoint;
    function GetReplaceText: string;
    function GetTop: Integer;
    procedure SetFindText(const Value: string);
    procedure SetLeft(Value: Integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetReplaceText(const Value: string);
    procedure SetTop(Value: Integer);
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
  protected
    function MessageHook(var Msg: TMessage): Boolean; override;
    procedure Find; dynamic;
    procedure Replace; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseDialog;
    function Execute(ParentWnd: HWND): Boolean; override;
    property Left: Integer read GetLeft write SetLeft;
    property Position: TPoint read GetPosition write SetPosition;
    property Top: Integer read GetTop write SetTop;
  published
    property FindText: string read GetFindText write SetFindText;
    property Options: TFindOptions read FOptions write FOptions default [frDown];
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
  end;

{ TReplaceDialog }

  TReplaceDialog = class(TFindDialog)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ReplaceText;
    property OnReplace;
  end;

{ TCustomFileDialog }

  EPlatformVersionException = class(Exception);

  TFileDialogOption = (fdoOverWritePrompt, fdoStrictFileTypes,
    fdoNoChangeDir, fdoPickFolders, fdoForceFileSystem,
    fdoAllNonStorageItems, fdoNoValidate, fdoAllowMultiSelect,
    fdoPathMustExist, fdoFileMustExist, fdoCreatePrompt,
    fdoShareAware, fdoNoReadOnlyReturn, fdoNoTestFileCreate,
    fdoHideMRUPlaces, fdoHidePinnedPlaces, fdoNoDereferenceLinks,
    fdoDontAddToRecent, fdoForceShowHidden, fdoDefaultNoMiniMode,
    fdoForcePreviewPaneOn);
  TFileDialogOptions = set of TFileDialogOption;

  TFileDialogOverwriteResponse = array[0..0] of Byte; const forDefault = TFileDialogOverwriteResponse(FDEOR_DEFAULT);
    forAccept = TFileDialogOverwriteResponse(FDEOR_ACCEPT); forRefuse = TFileDialogOverwriteResponse(FDEOR_REFUSE); type
  TFileDialogShareViolationResponse = array[0..0] of Byte; const fsrDefault = TFileDialogShareViolationResponse(FDESVR_DEFAULT);
    fsrAccept = TFileDialogShareViolationResponse(FDESVR_ACCEPT); fsrRefuse = TFileDialogShareViolationResponse(FDESVR_REFUSE); type

  TFileDialogCloseEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
  TFileDialogFolderChangingEvent = procedure(Sender: TObject; var CanChange: Boolean) of object;
  TFileDialogOverwriteEvent = procedure(Sender: TObject;
    var Response: TFileDialogOverwriteResponse) of object;
  TFileDialogShareViolationEvent = procedure(Sender: TObject;
    var Response: TFileDialogShareViolationResponse) of object;

  TFileTypeItem = class(TCollectionItem)
  private
    FDisplayName: string;
    FDisplayNameWStr: LPCWSTR;
    FFileMask: string;
    FFileMaskWStr: LPCWSTR;
    function GetDisplayNameWStr: LPCWSTR;
    function GetFileMaskWStr: LPCWSTR;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property DisplayNameWStr: LPCWSTR read GetDisplayNameWStr;
    property FileMaskWStr: LPCWSTR read GetFileMaskWStr;
  published
    property DisplayName: string read FDisplayName write FDisplayName;
    property FileMask: string read FFileMask write FFileMask;
  end;

  TFileTypeItems = class(TCollection)
  private
    function GetItem(Index: Integer): TFileTypeItem;
    procedure SetItem(Index: Integer; const Value: TFileTypeItem);
  public
    function Add: TFileTypeItem;
    function FilterSpecArray: TComdlgFilterSpecArray;
    property Items[Index: Integer]: TFileTypeItem read GetItem write SetItem; default;
  end;

  TFavoriteLinkItem = class(TCollectionItem)
  private
    FLocation: string;
  published
  protected
    function GetDisplayName: string; override;
  published
    property Location: string read FLocation write FLocation;
  end;

  TFavoriteLinkItems = class;

  TFavoriteLinkItemsEnumerator = class
  private
    FIndex: Integer;
    FCollection: TFavoriteLinkItems;
  public
    constructor Create(ACollection: TFavoriteLinkItems);
    function GetCurrent: TFavoriteLinkItem;
    function MoveNext: Boolean;
    property Current: TFavoriteLinkItem read GetCurrent;
  end;

  TFavoriteLinkItems = class(TCollection)
  private
    function GetItem(Index: Integer): TFavoriteLinkItem;
    procedure SetItem(Index: Integer; const Value: TFavoriteLinkItem);
  public
    function Add: TFavoriteLinkItem;
    function GetEnumerator: TFavoriteLinkItemsEnumerator;
    property Items[Index: Integer]: TFavoriteLinkItem read GetItem write SetItem; default;
  end;

  TCustomFileDialog = class(TComponent)
  private
    FClientGuid: string;
    FDefaultExtension: string;
    FDefaultFolder: string;
    FDialog: IFileDialog;
    FFavoriteLinks: TFavoriteLinkItems;
    FFileName: TFileName;
    FFileNameLabel: string;
    FFiles: TStrings;
    FFileTypeIndex: Cardinal;
    FFileTypes: TFileTypeItems;
    FHandle: HWnd;
    FOkButtonLabel: string;
    FOptions: TFileDialogOptions;
    FShellItem: IShellItem;
    FShellItems: IShellItemArray;
    FTitle: string;
    FOnExecute: TNotifyEvent;
    FOnFileOkClick: TFileDialogCloseEvent;
    FOnFolderChange: TNotifyEvent;
    FOnFolderChanging: TFileDialogFolderChangingEvent;
    FOnOverwrite: TFileDialogOverwriteEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnShareViolation: TFileDialogShareViolationEvent;
    FOnTypeChange: TNotifyEvent;
    function GetDefaultFolder: string;
    function GetFileName: TFileName;
    function GetFiles: TStrings;
    procedure GetWindowHandle;
    procedure SetClientGuid(const Value: string);
    procedure SetDefaultFolder(const Value: string);
    procedure SetFavoriteLinks(const Value: TFavoriteLinkItems);
    procedure SetFileName(const Value: TFileName);
    procedure SetFileTypes(const Value: TFileTypeItems);
  strict protected
    function CreateFileDialog: IFileDialog; virtual; abstract;
    procedure DoOnExecute; dynamic;
    function DoOnFileOkClick: Boolean; dynamic;
    procedure DoOnFolderChange; dynamic;
    function DoOnFolderChanging: Boolean; dynamic;
    procedure DoOnOverwrite(var Response: TFileDialogOverwriteResponse); dynamic;
    procedure DoOnSelectionChange; dynamic;
    procedure DoOnShareViolation(var Response: TFileDialogShareViolationResponse); dynamic;
    procedure DoOnTypeChange; dynamic;
    function GetFileNames(Items: IShellItemArray): HResult; dynamic;
    function GetItemName(Item: IShellItem; var ItemName: TFileName): HResult; dynamic;
    function GetResults: HResult; virtual;
  protected
    function FileOkClick: HResult; dynamic;
    function FolderChange: HResult; dynamic;
    function FolderChanging(psiFolder: IShellItem): HResult; dynamic;
    function Overwrite(psiFile: IShellItem; var Response: Cardinal): HResult; dynamic;
    function SelectionChange: HResult; dynamic;
    function ShareViolation(psiFile: IShellItem; var Response: Cardinal): HResult; dynamic;
    function TypeChange: HResult; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; overload; virtual;
    function Execute(ParentWnd: HWND): Boolean; overload; virtual;
    property ClientGuid: string read FClientGuid write SetClientGuid;
    property DefaultExtension: string read FDefaultExtension write FDefaultExtension;
    property DefaultFolder: string read GetDefaultFolder write SetDefaultFolder;
    property Dialog: IFileDialog read FDialog;
    property FavoriteLinks: TFavoriteLinkItems read FFavoriteLinks write SetFavoriteLinks;
    property FileName: TFileName read GetFileName write SetFileName;
    property FileNameLabel: string read FFileNameLabel write FFileNameLabel;
    property Files: TStrings read GetFiles;
    property FileTypes: TFileTypeItems read FFileTypes write SetFileTypes;
    property FileTypeIndex: Cardinal read FFileTypeIndex write FFileTypeIndex default 1;
    property Handle: HWnd read FHandle;
    property OkButtonLabel: string read FOkButtonLabel write FOkButtonLabel;
    property Options: TFileDialogOptions read FOptions write FOptions;
    property ShellItem: IShellItem read FShellItem;
    property ShellItems: IShellItemArray read FShellItems;
    property Title: string read FTitle write FTitle;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnFileOkClick: TFileDialogCloseEvent read FOnFileOkClick write FOnFileOkClick;
    property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
    property OnFolderChanging: TFileDialogFolderChangingEvent read FOnFolderChanging write FOnFolderChanging;
    property OnOverwrite: TFileDialogOverwriteEvent read FOnOverwrite write FOnOverwrite;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnShareViolation: TFileDialogShareViolationEvent read FOnShareViolation write FOnShareViolation;
    property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
  end;

{ TFileOpenDialog }

  TCustomFileOpenDialog = class(TCustomFileDialog)
  protected
    function CreateFileDialog: IFileDialog; override;
    function GetResults: HResult; override;
    function SelectionChange: HResult; override;
  end;

  TFileOpenDialog = class(TCustomFileOpenDialog)
  published
    property ClientGuid;
    property DefaultExtension;
    property DefaultFolder;
    property FavoriteLinks;
    property FileName;
    property FileNameLabel;
    property FileTypes;
    property FileTypeIndex;
    property OkButtonLabel;
    property Options;
    property Title;
    property OnExecute;
    property OnFileOkClick;
    property OnFolderChange;
    property OnFolderChanging;
    property OnSelectionChange;
    property OnShareViolation;
    property OnTypeChange;
  end platform;

{ TFileSaveDialog }

  TCustomFileSaveDialog = class(TCustomFileDialog)
  protected
    function CreateFileDialog: IFileDialog; override;
  end;

  TFileSaveDialog = class(TCustomFileSaveDialog)
  published
    property ClientGuid;
    property DefaultExtension;
    property DefaultFolder;
    property FavoriteLinks;
    property FileName;
    property FileNameLabel;
    property FileTypes;
    property FileTypeIndex;
    property OkButtonLabel;
    property Options;
    property Title;
    property OnExecute;
    property OnFileOkClick;
    property OnFolderChange;
    property OnFolderChanging;
    property OnOverwrite;
    property OnSelectionChange;
    property OnShareViolation;
    property OnTypeChange;
  end platform;

{ TTaskDialog }

const
  tdiNone = 0;
  tdiWarning = 1;
  tdiError = 2;
  tdiInformation = 3;
  tdiShield = 4;

type
  TCustomTaskDialog = class;

  TTaskDialogFlag = (tfEnableHyperlinks, tfUseHiconMain,
    tfUseHiconFooter, tfAllowDialogCancellation,
    tfUseCommandLinks, tfUseCommandLinksNoIcon,
    tfExpandFooterArea, tfExpandedByDefault,
    tfVerificationFlagChecked, tfShowProgressBar,
    tfShowMarqueeProgressBar, tfCallbackTimer,
    tfPositionRelativeToWindow, tfRtlLayout,
    tfNoDefaultRadioButton, tfCanBeMinimized);
  TTaskDialogFlags = set of TTaskDialogFlag;

  TTaskDialogCommonButton = (tcbOk, tcbYes, tcbNo, tcbCancel, tcbRetry, tcbClose);
  TTaskDialogCommonButtons = set of TTaskDialogCommonButton;

  TTaskDialogIcon = Low(Integer)..High(Integer);

  TProgressBarState = (pbsNormal, pbsError, pbsPaused);

  TTaskDialogProgressBar = class(TPersistent)
  private
    FClient: TCustomTaskDialog;
    FMarqueeSpeed: Cardinal;
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    FState: TProgressBarState;
    procedure SetMarqueeSpeed(const Value: Cardinal);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetState(const Value: TProgressBarState);
  public
    constructor Create(AClient: TCustomTaskDialog);
    procedure Initialize;
  published
    property MarqueeSpeed: Cardinal read FMarqueeSpeed write SetMarqueeSpeed default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Position: Integer read FPosition write SetPosition default 0;
    property State: TProgressBarState read FState write SetState default pbsNormal;
  end;

  TTaskDialogBaseButtonItem = class(TCollectionItem)
  private
    FCaption: string;
    FClient: TCustomTaskDialog;
    FEnabled: Boolean;
    FModalResult: TModalResult;
    FTextWStr: LPCWSTR;
    function GetDefault: Boolean;
    function GetTextWStr: LPCWSTR;
    procedure SetCaption(const Value: string);
    procedure SetDefault(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
  strict protected
    procedure DoButtonClick; virtual;
    procedure DoSetEnabled; virtual;
    function GetButtonText: string; virtual;
    function GetDisplayName: string; override;
    property Client: TCustomTaskDialog read FClient;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Click;
    procedure SetInitialState; virtual;
    property ModalResult: TModalResult read FModalResult write FModalResult;
    property TextWStr: LPCWSTR read GetTextWStr;
  published
    property Caption: string read FCaption write SetCaption;
    property Default: Boolean read GetDefault write SetDefault default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TTaskDialogButtonItem = class(TTaskDialogBaseButtonItem)
  private
    FCommandLinkHint: string;
    FElevationRequired: Boolean;
    procedure DoSetElevationRequired;
    procedure SetElevationRequired(const Value: Boolean);
  strict protected
    function GetButtonText: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure SetInitialState; override;
  published
    property CommandLinkHint: string read FCommandLinkHint write FCommandLinkHint;
    property ElevationRequired: Boolean read FElevationRequired write SetElevationRequired default False;
    property ModalResult;
  end;

  TTaskDialogRadioButtonItem = class(TTaskDialogBaseButtonItem)
  strict protected
    procedure DoButtonClick; override;
    procedure DoSetEnabled; override;
  public
    constructor Create(Collection: TCollection); override;
  end;

  TTaskDialogButtonList = array of TTaskDialogButton;

  TTaskDialogButtons = class;

  TTaskDialogButtonsEnumerator = class
  private
    FIndex: Integer;
    FCollection: TTaskDialogButtons;
  public
    constructor Create(ACollection: TTaskDialogButtons);
    function GetCurrent: TTaskDialogBaseButtonItem;
    function MoveNext: Boolean;
    property Current: TTaskDialogBaseButtonItem read GetCurrent;
  end;

  TTaskDialogButtons = class(TOwnedCollection)
  private
    FButtonList:  TTaskDialogButtonList;
    FDefaultButton: TTaskDialogBaseButtonItem;
    function GetItem(Index: Integer): TTaskDialogBaseButtonItem;
    procedure SetDefaultButton(const Value: TTaskDialogBaseButtonItem);
    procedure SetItem(Index: Integer; const Value: TTaskDialogBaseButtonItem);
  public
    function Add: TTaskDialogBaseButtonItem;
    function Buttons: PTaskDialogButton;
    function FindButton(AModalResult: TModalResult): TTaskDialogBaseButtonItem;
    function GetEnumerator: TTaskDialogButtonsEnumerator;
    procedure SetInitialState; dynamic;
    property DefaultButton: TTaskDialogBaseButtonItem read FDefaultButton write SetDefaultButton;
    property Items[Index: Integer]: TTaskDialogBaseButtonItem read GetItem write SetItem; default;
  end;

  TTaskDlgClickEvent = procedure(Sender: TObject; ModalResult: TModalResult; var CanClose: Boolean) of object;
  TTaskDlgTimerEvent = procedure(Sender: TObject; TickCount: Cardinal; var Reset: Boolean) of object;

  TCustomTaskDialog = class(TComponent)
  private
    FButton: TTaskDialogButtonItem;
    FButtons: TTaskDialogButtons;
    FCaption: string;
    FCommonButtons: TTaskDialogCommonButtons;
    FCustomFooterIcon: TIcon;
    FCustomMainIcon: TIcon;
    FDefaultButton: TTaskDialogCommonButton;
    FExpandButtonCaption: string;
    FExpanded: Boolean;
    FExpandedText: string;
    FFlags: TTaskDialogFlags;
    FFooterIcon: TTaskDialogIcon;
    FFooterText: string;
    FHandle: HWND;
    FHelpContext: Integer;
    FMainIcon: TTaskDialogIcon;
    FModalResult: TModalResult;
    FProgressBar: TTaskDialogProgressBar;
    FRadioButton: TTaskDialogRadioButtonItem;
    FRadioButtons: TTaskDialogButtons;
    FText: string;
    FTitle: string;
    FURL: string;
    FVerificationText: string;
    FOnButtonClicked: TTaskDlgClickEvent;
    FOnDialogConstructed: TNotifyEvent;
    FOnDialogCreated: TNotifyEvent;
    FOnDialogDestroyed: TNotifyEvent;
    FOnExpanded: TNotifyEvent;
    FOnHyperlinkClicked: TNotifyEvent;
    FOnNavigated: TNotifyEvent;
    FOnRadioButtonClicked: TNotifyEvent;
    FOnTimer: TTaskDlgTimerEvent;
    FOnVerificationClicked: TNotifyEvent;
    procedure SetButtons(const Value: TTaskDialogButtons);
    procedure SetExpandedText(const Value: string);
    procedure SetFooterIcon(const Value: TTaskDialogIcon);
    procedure SetFooterText(const Value: string);
    procedure SetFlags(const Value: TTaskDialogFlags);
    procedure SetMainIcon(const Value: TTaskDialogIcon);
    procedure SetRadioButtons(const Value: TTaskDialogButtons);
    procedure SetText(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetCustomFooterIcon(const Value: TIcon);
    procedure SetCustomMainIcon(const Value: TIcon);
  strict protected
    function DoExecute(ParentWnd: HWND): Boolean; dynamic;
    procedure DoOnButtonClicked(AModalResult: Integer; var CanClose: Boolean); dynamic;
    procedure DoOnDialogContructed; dynamic;
    procedure DoOnDialogCreated; dynamic;
    procedure DoOnDialogDestroyed; dynamic;
    procedure DoOnExpandButtonClicked(Expanded: Boolean); dynamic;
    procedure DoOnHelp; dynamic;
    procedure DoOnHyperlinkClicked(const AURL: string); dynamic;
    procedure DoOnNavigated; dynamic;
    procedure DoOnRadioButtonClicked(ButtonID: Integer); dynamic;
    procedure DoOnTimer(TickCount: Cardinal; var Reset: Boolean); dynamic;
    procedure DoOnVerificationClicked(Checked: Boolean); dynamic;
  protected
    function CallbackProc(hwnd: HWND; msg: UINT; wParam: WPARAM;
      lParam: LPARAM; lpRefData: LONG_PTR): HResult; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; overload;
    function Execute(ParentWnd: HWND): Boolean; overload;
    property Button: TTaskDialogButtonItem read FButton write FButton;
    property Buttons: TTaskDialogButtons read FButtons write SetButtons;
    property Caption: string read FCaption write FCaption;
    property CommonButtons: TTaskDialogCommonButtons read FCommonButtons write FCommonButtons default [tcbOk, tcbCancel];
    property CustomFooterIcon: TIcon read FCustomFooterIcon write SetCustomFooterIcon;
    property CustomMainIcon: TIcon read FCustomMainIcon write SetCustomMainIcon;
    property DefaultButton: TTaskDialogCommonButton read FDefaultButton write FDefaultButton default tcbOk;
    property ExpandButtonCaption: string read FExpandButtonCaption write FExpandButtonCaption;
    property Expanded: Boolean read FExpanded;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property Flags: TTaskDialogFlags read FFlags write SetFlags default [tfAllowDialogCancellation];
    property FooterIcon: TTaskDialogIcon read FFooterIcon write SetFooterIcon default tdiNone;
    property FooterText: string read FFooterText write SetFooterText;
    property Handle: HWND read FHandle;
    property HelpContext: Integer read FHelpContext write FHelpContext default 0;
    property MainIcon: TTaskDialogIcon read FMainIcon write SetMainIcon default tdiInformation;
    property ModalResult: TModalResult read FModalResult write FModalResult;
    property ProgressBar: TTaskDialogProgressBar read FProgressBar write FProgressBar;
    property RadioButton: TTaskDialogRadioButtonItem read FRadioButton;
    property RadioButtons: TTaskDialogButtons read FRadioButtons write SetRadioButtons;
    property Text: string read FText write SetText;
    property Title: string read FTitle write SetTitle;
    property URL: string read FURL;
    property VerificationText: string read FVerificationText write FVerificationText;
    property OnButtonClicked: TTaskDlgClickEvent read FOnButtonClicked write FOnButtonClicked;
    property OnDialogConstructed: TNotifyEvent read FOnDialogConstructed write FOnDialogConstructed;
    property OnDialogCreated: TNotifyEvent read FOnDialogCreated write FOnDialogCreated;
    property OnDialogDestroyed: TNotifyEvent read FOnDialogDestroyed write FOnDialogDestroyed;
    property OnExpanded: TNotifyEvent read FOnExpanded write FOnExpanded;
    property OnHyperlinkClicked: TNotifyEvent read FOnHyperlinkClicked write FOnHyperlinkClicked;
    property OnNavigated: TNotifyEvent read FOnNavigated write FOnNavigated;
    property OnRadioButtonClicked: TNotifyEvent read FOnRadioButtonClicked write FOnRadioButtonClicked;
    property OnTimer: TTaskDlgTimerEvent read FOnTimer write FOnTimer;
    property OnVerificationClicked: TNotifyEvent read FOnVerificationClicked write FOnVerificationClicked;
  end;

  TTaskDialog = class(TCustomTaskDialog)
  published
    property Buttons;
    property Caption;
    property CommonButtons;
    property CustomFooterIcon;
    property CustomMainIcon;
    property DefaultButton;
    property ExpandButtonCaption;
    property ExpandedText;
    property Flags;
    property FooterIcon;
    property FooterText;
    property HelpContext;
    property MainIcon;
    property ProgressBar;
    property RadioButtons;
    property Text;
    property Title;
    property VerificationText;
    property OnButtonClicked;
    property OnDialogConstructed;
    property OnDialogCreated;
    property OnDialogDestroyed;
    property OnExpanded;
    property OnHyperlinkClicked;
    property OnNavigated;
    property OnRadioButtonClicked;
    property OnTimer;
    property OnVerificationClicked;
  end platform;

{ Message dialog }

type
  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;

const
  mbYesNo = [mbYes, mbNo];
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbYesAllNoAllCancel = [mbYes, mbYesToAll, mbNo, mbNoToAll, mbCancel];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];
  mbAbortIgnore = [mbAbort, mbIgnore];

function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TForm; overload;
function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn): TForm; overload;

function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;

function MessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; overload;
function MessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn): Integer; overload;

function MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer; overload;
function MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; DefaultButton: TMsgDlgBtn): Integer; overload;

{ TaskDialog based Message dialogs }

function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;

function TaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; overload;
function TaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn): Integer; overload;

function TaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer; overload;
function TaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; DefaultButton: TMsgDlgBtn): Integer; overload;

procedure ShowMessage(const Msg: string);
procedure ShowMessageFmt(const Msg: string; Params: array of const);
procedure ShowMessagePos(const Msg: string; X, Y: Integer);

{ Input dialog }

function InputBox(const ACaption, APrompt, ADefault: string): string;
function InputQuery(const ACaption, APrompt: string;
  var Value: string): Boolean;

function PromptForFileName(var AFileName: string; const AFilter: string = '';
  const ADefaultExt: string = ''; const ATitle: string = '';
  const AInitialDir: string = ''; SaveDialog: Boolean = False): Boolean;

{ Win98 and Win2k will default to the "My Documents" folder if the InitialDir
  property is empty and no files of the filtered type are contained in the
  current directory. Set this flag to True to force TOpenDialog and descendents
  to always open in the current directory when InitialDir is empty. (Same
  behavior as setting InitialDir to '.') }
var
  ForceCurrentDirectory: Boolean = False;
  UseLatestCommonDialogs: Boolean = True;

implementation
