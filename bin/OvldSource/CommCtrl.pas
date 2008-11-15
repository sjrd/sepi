
{*******************************************************}
{                                                       }
{       Borland Delphi Run-time Library                 }
{       Win32 common controls interface unit            }
{                                                       }
{       Copyright (c) 1985-1999, Microsoft Corporation  }
{                                                       }
{       Translator: Borland Software Corporation        }
{                                                       }
{*******************************************************}

unit CommCtrl;

{$WEAKPACKAGEUNIT}

interface

(*$HPPEMIT '' *)
(*$HPPEMIT '#include <CommCtrl.h>' *)
(*$HPPEMIT '' *)

{ Although the COMMCTRL unit requires the ActiveX unit, COMMCTRL.HPP does }
{ not require ActiveX.hpp to compile. Hence the $NOINCLUDE directive.     }
(*$NOINCLUDE ActiveX *)

uses Messages, Windows, ActiveX;

{ From prsht.h -- Interface for the Windows Property Sheet Pages }

const
  {$EXTERNALSYM MAXPROPPAGES}
  MAXPROPPAGES = 100;

  {$EXTERNALSYM PSP_DEFAULT}
  PSP_DEFAULT             = $00000000;
  {$EXTERNALSYM PSP_DLGINDIRECT}
  PSP_DLGINDIRECT         = $00000001;
  {$EXTERNALSYM PSP_USEHICON}
  PSP_USEHICON            = $00000002;
  {$EXTERNALSYM PSP_USEICONID}
  PSP_USEICONID           = $00000004;
  {$EXTERNALSYM PSP_USETITLE}
  PSP_USETITLE            = $00000008;
  {$EXTERNALSYM PSP_RTLREADING}
  PSP_RTLREADING          = $00000010;
  {$EXTERNALSYM PSP_HASHELP}
  PSP_HASHELP             = $00000020;
  {$EXTERNALSYM PSP_USEREFPARENT}
  PSP_USEREFPARENT        = $00000040;
  {$EXTERNALSYM PSP_USECALLBACK}
  PSP_USECALLBACK         = $00000080;
  {$EXTERNALSYM PSP_PREMATURE}
  PSP_PREMATURE           = $00000400;
  {$EXTERNALSYM PSP_HIDEHEADER}
  PSP_HIDEHEADER          = $00000800;
  {$EXTERNALSYM PSP_USEHEADERTITLE}
  PSP_USEHEADERTITLE      = $00001000;
  {$EXTERNALSYM PSP_USEHEADERSUBTITLE}
  PSP_USEHEADERSUBTITLE   = $00002000;

  {$EXTERNALSYM PSPCB_RELEASE}
  PSPCB_RELEASE           = 1;
  {$EXTERNALSYM PSPCB_CREATE}
  PSPCB_CREATE            = 2;

  {$EXTERNALSYM PSH_DEFAULT}
  PSH_DEFAULT             = $00000000;
  {$EXTERNALSYM PSH_PROPTITLE}
  PSH_PROPTITLE           = $00000001;
  {$EXTERNALSYM PSH_USEHICON}
  PSH_USEHICON            = $00000002;
  {$EXTERNALSYM PSH_USEICONID}
  PSH_USEICONID           = $00000004;
  {$EXTERNALSYM PSH_PROPSHEETPAGE}
  PSH_PROPSHEETPAGE       = $00000008;
  {$EXTERNALSYM PSH_WIZARDHASFINISH}
  PSH_WIZARDHASFINISH     = $00000010;
  PSH_MULTILINETABS       = $00000010;
  {$EXTERNALSYM PSH_WIZARD}
  PSH_WIZARD              = $00000020;
  {$EXTERNALSYM PSH_USEPSTARTPAGE}
  PSH_USEPSTARTPAGE       = $00000040;
  {$EXTERNALSYM PSH_NOAPPLYNOW}
  PSH_NOAPPLYNOW          = $00000080;
  {$EXTERNALSYM PSH_USECALLBACK}
  PSH_USECALLBACK         = $00000100;
  {$EXTERNALSYM PSH_HASHELP}
  PSH_HASHELP             = $00000200;
  {$EXTERNALSYM PSH_MODELESS}
  PSH_MODELESS            = $00000400;
  {$EXTERNALSYM PSH_RTLREADING}
  PSH_RTLREADING          = $00000800;
  {$EXTERNALSYM PSH_WIZARDCONTEXTHELP}
  PSH_WIZARDCONTEXTHELP   = $00001000;
  {$EXTERNALSYM PSH_WIZARD97}
  PSH_WIZARD97            = $00002000;
  {$EXTERNALSYM PSH_WATERMARK}
  PSH_WATERMARK           = $00008000;
  {$EXTERNALSYM PSH_USEHBMWATERMARK}
  PSH_USEHBMWATERMARK     = $00010000;  // user pass in a hbmWatermark instead of pszbmWatermark
  {$EXTERNALSYM PSH_USEHPLWATERMARK}
  PSH_USEHPLWATERMARK     = $00020000;  //
  {$EXTERNALSYM PSH_STRETCHWATERMARK}
  PSH_STRETCHWATERMARK    = $00040000;  // stretchwatermark also applies for the header
  {$EXTERNALSYM PSH_HEADER}
  PSH_HEADER              = $00080000;
  {$EXTERNALSYM PSH_USEHBMHEADER}
  PSH_USEHBMHEADER        = $00100000;
  {$EXTERNALSYM PSH_USEPAGELANG}
  PSH_USEPAGELANG         = $00200000;  // use frame dialog template matched to page

  {$EXTERNALSYM PSCB_INITIALIZED}
  PSCB_INITIALIZED  = 1;
  {$EXTERNALSYM PSCB_PRECREATE}
  PSCB_PRECREATE    = 2;

  {$EXTERNALSYM PSN_FIRST}
  PSN_FIRST               = -200;
  {$EXTERNALSYM PSN_LAST}
  PSN_LAST                = -299;

  {$EXTERNALSYM PSN_SETACTIVE}
  PSN_SETACTIVE           = PSN_FIRST - 0;
  {$EXTERNALSYM PSN_KILLACTIVE}
  PSN_KILLACTIVE          = PSN_FIRST - 1;
  {$EXTERNALSYM PSN_APPLY}
  PSN_APPLY               = PSN_FIRST - 2;
  {$EXTERNALSYM PSN_RESET}
  PSN_RESET               = PSN_FIRST - 3;
  {$EXTERNALSYM PSN_HELP}
  PSN_HELP                = PSN_FIRST - 5;
  {$EXTERNALSYM PSN_WIZBACK}
  PSN_WIZBACK             = PSN_FIRST - 6;
  {$EXTERNALSYM PSN_WIZNEXT}
  PSN_WIZNEXT             = PSN_FIRST - 7;
  {$EXTERNALSYM PSN_WIZFINISH}
  PSN_WIZFINISH           = PSN_FIRST - 8;
  {$EXTERNALSYM PSN_QUERYCANCEL}
  PSN_QUERYCANCEL         = PSN_FIRST - 9;
  {$EXTERNALSYM PSN_GETOBJECT}
  PSN_GETOBJECT           = PSN_FIRST - 10;

  {$EXTERNALSYM PSNRET_NOERROR}
  PSNRET_NOERROR              = 0;
  {$EXTERNALSYM PSNRET_INVALID}
  PSNRET_INVALID              = 1;
  {$EXTERNALSYM PSNRET_INVALID_NOCHANGEPAGE}
  PSNRET_INVALID_NOCHANGEPAGE = 2;

  {$EXTERNALSYM PSM_SETCURSEL}
  PSM_SETCURSEL           = WM_USER + 101;
  {$EXTERNALSYM PSM_REMOVEPAGE}
  PSM_REMOVEPAGE          = WM_USER + 102;
  {$EXTERNALSYM PSM_ADDPAGE}
  PSM_ADDPAGE             = WM_USER + 103;
  {$EXTERNALSYM PSM_CHANGED}
  PSM_CHANGED             = WM_USER + 104;
  {$EXTERNALSYM PSM_RESTARTWINDOWS}
  PSM_RESTARTWINDOWS      = WM_USER + 105;
  {$EXTERNALSYM PSM_REBOOTSYSTEM}
  PSM_REBOOTSYSTEM        = WM_USER + 106;
  {$EXTERNALSYM PSM_CANCELTOCLOSE}
  PSM_CANCELTOCLOSE       = WM_USER + 107;
  {$EXTERNALSYM PSM_QUERYSIBLINGS}
  PSM_QUERYSIBLINGS       = WM_USER + 108;
  {$EXTERNALSYM PSM_UNCHANGED}
  PSM_UNCHANGED           = WM_USER + 109;
  {$EXTERNALSYM PSM_APPLY}
  PSM_APPLY               = WM_USER + 110;
  {$EXTERNALSYM PSM_SETTITLE}
  PSM_SETTITLE            = WM_USER + 111;
  {$EXTERNALSYM PSM_SETTITLEW}
  PSM_SETTITLEW           = WM_USER + 120;
  {$EXTERNALSYM PSM_SETWIZBUTTONS}
  PSM_SETWIZBUTTONS       = WM_USER + 112;
  {$EXTERNALSYM PSM_PRESSBUTTON}
  PSM_PRESSBUTTON         = WM_USER + 113;
  {$EXTERNALSYM PSM_SETCURSELID}
  PSM_SETCURSELID         = WM_USER + 114;
  {$EXTERNALSYM PSM_SETFINISHTEXT}
  PSM_SETFINISHTEXT       = WM_USER + 115;
  {$EXTERNALSYM PSM_SETFINISHTEXTW}
  PSM_SETFINISHTEXTW      = WM_USER + 121;
  {$EXTERNALSYM PSM_GETTABCONTROL}
  PSM_GETTABCONTROL       = WM_USER + 116;
  {$EXTERNALSYM PSM_ISDIALOGMESSAGE}
  PSM_ISDIALOGMESSAGE     = WM_USER + 117;

  {$EXTERNALSYM PSWIZB_BACK}
  PSWIZB_BACK             = $00000001;
  {$EXTERNALSYM PSWIZB_NEXT}
  PSWIZB_NEXT             = $00000002;
  {$EXTERNALSYM PSWIZB_FINISH}
  PSWIZB_FINISH           = $00000004;
  {$EXTERNALSYM PSWIZB_DISABLEDFINISH}
  PSWIZB_DISABLEDFINISH   = $00000008;

  {$EXTERNALSYM PSBTN_BACK}
  PSBTN_BACK              = 0;
  {$EXTERNALSYM PSBTN_NEXT}
  PSBTN_NEXT              = 1;
  {$EXTERNALSYM PSBTN_FINISH}
  PSBTN_FINISH            = 2;
  {$EXTERNALSYM PSBTN_OK}
  PSBTN_OK                = 3;
  {$EXTERNALSYM PSBTN_APPLYNOW}
  PSBTN_APPLYNOW          = 4;
  {$EXTERNALSYM PSBTN_CANCEL}
  PSBTN_CANCEL            = 5;
  {$EXTERNALSYM PSBTN_HELP}
  PSBTN_HELP              = 6;
  {$EXTERNALSYM PSBTN_MAX}
  PSBTN_MAX               = 6;

  {$EXTERNALSYM ID_PSRESTARTWINDOWS}
  ID_PSRESTARTWINDOWS     = 2;
  {$EXTERNALSYM ID_PSREBOOTSYSTEM}
  ID_PSREBOOTSYSTEM       = ID_PSRESTARTWINDOWS or 1;

  {$EXTERNALSYM WIZ_CXDLG}
  WIZ_CXDLG               = 276;
  {$EXTERNALSYM WIZ_CYDLG}
  WIZ_CYDLG               = 140;

  {$EXTERNALSYM WIZ_CXBMP}
  WIZ_CXBMP               = 80;

  {$EXTERNALSYM WIZ_BODYX}
  WIZ_BODYX               = 92;
  {$EXTERNALSYM WIZ_BODYCX}
  WIZ_BODYCX              = 184;

  {$EXTERNALSYM PROP_SM_CXDLG}
  PROP_SM_CXDLG           = 212;
  {$EXTERNALSYM PROP_SM_CYDLG}
  PROP_SM_CYDLG           = 188;

  {$EXTERNALSYM PROP_MED_CXDLG}
  PROP_MED_CXDLG          = 227;
  {$EXTERNALSYM PROP_MED_CYDLG}
  PROP_MED_CYDLG          = 215;

  {$EXTERNALSYM PROP_LG_CXDLG}
  PROP_LG_CXDLG           = 252;
  {$EXTERNALSYM PROP_LG_CYDLG}
  PROP_LG_CYDLG           = 218;

type
  HPropSheetPage = Pointer;

  PPropSheetPageA = ^TPropSheetPageA;
  PPropSheetPageW = ^TPropSheetPageW;
  PPropSheetPage = PPropSheetPageA;

  {$EXTERNALSYM LPFNPSPCALLBACKA}
  LPFNPSPCALLBACKA = function(Wnd: HWnd; Msg: Integer;
    PPSP: PPropSheetPageA): Integer stdcall;
  {$EXTERNALSYM LPFNPSPCALLBACKW}
  LPFNPSPCALLBACKW = function(Wnd: HWnd; Msg: Integer;
    PPSP: PPropSheetPageW): Integer stdcall;
  {$EXTERNALSYM LPFNPSPCALLBACK}
  LPFNPSPCALLBACK = LPFNPSPCALLBACKA;
  TFNPSPCallbackA = LPFNPSPCALLBACKA;
  TFNPSPCallbackW = LPFNPSPCALLBACKW;
  TFNPSPCallback = TFNPSPCallbackA;

  {$EXTERNALSYM _PROPSHEETPAGEA}
  _PROPSHEETPAGEA = record
    dwSize: Longint;
    dwFlags: Longint;
    hInstance: THandle;
    case Integer of
      0: (
        pszTemplate: PAnsiChar);
      1: (
        pResource: Pointer;
        case Integer of
          0: (
            hIcon: THandle);
          1: (
            pszIcon: PAnsiChar;
            pszTitle: PAnsiChar;
            pfnDlgProc: Pointer;
            lParam: Longint;
            pfnCallback: TFNPSPCallbackA;
            pcRefParent: PInteger;
            pszHeaderTitle: PAnsiChar;      // this is displayed in the header
            pszHeaderSubTitle: PAnsiChar)); //
  end;
  {$EXTERNALSYM _PROPSHEETPAGEW}
  _PROPSHEETPAGEW = record
    dwSize: Longint;
    dwFlags: Longint;
    hInstance: THandle;
    case Integer of
      0: (
        pszTemplate: PWideChar);
      1: (
        pResource: Pointer;
        case Integer of
          0: (
            hIcon: THandle);
          1: (
            pszIcon: PWideChar;
            pszTitle: PWideChar;
            pfnDlgProc: Pointer;
            lParam: Longint;
            pfnCallback: TFNPSPCallbackW;
            pcRefParent: PInteger;
            pszHeaderTitle: PWideChar;      // this is displayed in the header
            pszHeaderSubTitle: PWideChar)); //
  end;
  {$EXTERNALSYM _PROPSHEETPAGE}
  _PROPSHEETPAGE = _PROPSHEETPAGEA;
  TPropSheetPageA = _PROPSHEETPAGEA;
  TPropSheetPageW = _PROPSHEETPAGEW;
  TPropSheetPage = TPropSheetPageA;
  {$EXTERNALSYM PROPSHEETPAGEA}
  PROPSHEETPAGEA = _PROPSHEETPAGEA;
  {$EXTERNALSYM PROPSHEETPAGEW}
  PROPSHEETPAGEW = _PROPSHEETPAGEW;
  {$EXTERNALSYM PROPSHEETPAGE}
  PROPSHEETPAGE = PROPSHEETPAGEA;


  {$EXTERNALSYM PFNPROPSHEETCALLBACK}
  PFNPROPSHEETCALLBACK = function(Wnd: HWnd; Msg: Integer;
    LParam: Integer): Integer stdcall;
  TFNPropSheetCallback = PFNPROPSHEETCALLBACK;

  PPropSheetHeaderA = ^TPropSheetHeaderA;
  PPropSheetHeaderW = ^TPropSheetHeaderW;
  PPropSheetHeader = PPropSheetHeaderA;
  {$EXTERNALSYM _PROPSHEETHEADERA}
  _PROPSHEETHEADERA = record
    dwSize: Longint;
    dwFlags: Longint;
    hwndParent: HWnd;
    hInstance: THandle;
    case Integer of
      0: (
	hIcon: THandle);
      1: (
	pszIcon: PAnsiChar;
	pszCaption: PAnsiChar;
	nPages: Integer;
	case Integer of
	  0: (
	    nStartPage: Integer);
	  1: (
	    pStartPage: PAnsiChar;
	    case Integer of
	      0: (
		ppsp: PPropSheetPageA);
	      1: (
		phpage: Pointer;
		pfnCallback: TFNPropSheetCallback;
                case Integer of
                  0: (
                    hbmWatermark: HBITMAP);
                  1: (
                    pszbmWatermark: PAnsiChar;
                    hplWatermark: HPALETTE;
                    // Header bitmap shares the palette with watermark
                    case Integer of
                      0: (
                        hbmHeader: HBITMAP);
                      1: (
                        pszbmHeader: PAnsiChar)))));
  end;
  {$EXTERNALSYM _PROPSHEETHEADERW}
  _PROPSHEETHEADERW = record
    dwSize: Longint;
    dwFlags: Longint;
    hwndParent: HWnd;
    hInstance: THandle;
    case Integer of
      0: (
	hIcon: THandle);
      1: (
	pszIcon: PWideChar;
	pszCaption: PWideChar;
	nPages: Integer;
	case Integer of
	  0: (
	    nStartPage: Integer);
	  1: (
	    pStartPage: PWideChar;
	    case Integer of
	      0: (
		ppsp: PPropSheetPageW);
	      1: (
		phpage: Pointer;
		pfnCallback: TFNPropSheetCallback;
                case Integer of
                  0: (
                    hbmWatermark: HBITMAP);
                  1: (
                    pszbmWatermark: PWideChar;
                    hplWatermark: HPALETTE;
                    // Header bitmap shares the palette with watermark
                    case Integer of
                      0: (
                        hbmHeader: HBITMAP);
                      1: (
                        pszbmHeader: PWideChar)))));
  end;
  {$EXTERNALSYM _PROPSHEETHEADER}
  _PROPSHEETHEADER = _PROPSHEETHEADERA;
  TPropSheetHeaderA = _PROPSHEETHEADERA;
  TPropSheetHeaderW = _PROPSHEETHEADERW;
  TPropSheetHeader = TPropSheetHeaderA;

  {$EXTERNALSYM LPFNADDPROPSHEETPAGE}
  LPFNADDPROPSHEETPAGE = function(hPSP: HPropSheetPage;
    lParam: Longint): BOOL stdcall;
  TFNAddPropSheetPage = LPFNADDPROPSHEETPAGE;

  {$EXTERNALSYM LPFNADDPROPSHEETPAGES}
  LPFNADDPROPSHEETPAGES = function(lpvoid: Pointer; pfn: TFNAddPropSheetPage;
    lParam: Longint): BOOL stdcall;
  TFNAddPropSheetPages = LPFNADDPROPSHEETPAGES;

{$EXTERNALSYM CreatePropertySheetPage}
function CreatePropertySheetPage(var PSP: TPropSheetPage): HPropSheetPage; stdcall;
{$EXTERNALSYM CreatePropertySheetPageA}
function CreatePropertySheetPageA(var PSP: TPropSheetPageA): HPropSheetPage; stdcall;
{$EXTERNALSYM CreatePropertySheetPageW}
function CreatePropertySheetPageW(var PSP: TPropSheetPageW): HPropSheetPage; stdcall;
{$EXTERNALSYM DestroyPropertySheetPage}
function DestroyPropertySheetPage(hPSP: HPropSheetPage): BOOL; stdcall;
{$EXTERNALSYM PropertySheet}
function PropertySheet(var PSH: TPropSheetHeader): Integer; stdcall;
{$EXTERNALSYM PropertySheetA}
function PropertySheetA(var PSH: TPropSheetHeaderA): Integer; stdcall;
{$EXTERNALSYM PropertySheetW}
function PropertySheetW(var PSH: TPropSheetHeaderW): Integer; stdcall;

{ From commctrl.h }

type
  {$EXTERNALSYM tagINITCOMMONCONTROLSEX}
  tagINITCOMMONCONTROLSEX = packed record
    dwSize: DWORD;             // size of this structure
    dwICC: DWORD;              // flags indicating which classes to be initialized
  end;
  PInitCommonControlsEx = ^TInitCommonControlsEx;
  TInitCommonControlsEx = tagINITCOMMONCONTROLSEX;
  
const
  {$EXTERNALSYM ICC_LISTVIEW_CLASSES}
  ICC_LISTVIEW_CLASSES   = $00000001; // listview, header
  {$EXTERNALSYM ICC_TREEVIEW_CLASSES}
  ICC_TREEVIEW_CLASSES   = $00000002; // treeview, tooltips
  {$EXTERNALSYM ICC_BAR_CLASSES}
  ICC_BAR_CLASSES        = $00000004; // toolbar, statusbar, trackbar, tooltips
  {$EXTERNALSYM ICC_TAB_CLASSES}
  ICC_TAB_CLASSES        = $00000008; // tab, tooltips
  {$EXTERNALSYM ICC_UPDOWN_CLASS}
  ICC_UPDOWN_CLASS       = $00000010; // updown
  {$EXTERNALSYM ICC_PROGRESS_CLASS}
  ICC_PROGRESS_CLASS     = $00000020; // progress
  {$EXTERNALSYM ICC_HOTKEY_CLASS}
  ICC_HOTKEY_CLASS       = $00000040; // hotkey
  {$EXTERNALSYM ICC_ANIMATE_CLASS}
  ICC_ANIMATE_CLASS      = $00000080; // animate
  {$EXTERNALSYM ICC_WIN95_CLASSES}
  ICC_WIN95_CLASSES      = $000000FF;
  {$EXTERNALSYM ICC_DATE_CLASSES}
  ICC_DATE_CLASSES       = $00000100; // month picker, date picker, time picker, updown
  {$EXTERNALSYM ICC_USEREX_CLASSES}
  ICC_USEREX_CLASSES     = $00000200; // comboex
  {$EXTERNALSYM ICC_COOL_CLASSES}
  ICC_COOL_CLASSES       = $00000400; // rebar (coolbar) control
  {$EXTERNALSYM ICC_INTERNET_CLASSES}
  ICC_INTERNET_CLASSES   = $00000800;
  {$EXTERNALSYM ICC_PAGESCROLLER_CLASS}
  ICC_PAGESCROLLER_CLASS = $00001000; // page scroller
  {$EXTERNALSYM ICC_NATIVEFNTCTL_CLASS}
  ICC_NATIVEFNTCTL_CLASS = $00002000; // native font control

{$EXTERNALSYM InitCommonControls}
procedure InitCommonControls; stdcall;
{$EXTERNALSYM InitCommonControlsEx}
function InitCommonControlsEx(var ICC: TInitCommonControlsEx): Bool; { Re-defined below }

const
  {$EXTERNALSYM IMAGE_BITMAP}
  IMAGE_BITMAP = 0;

const
  {$EXTERNALSYM ODT_HEADER}
  ODT_HEADER              = 100;
  {$EXTERNALSYM ODT_TAB}
  ODT_TAB                 = 101;
  {$EXTERNALSYM ODT_LISTVIEW}
  ODT_LISTVIEW            = 102;


{ ====== Ranges for control message IDs ======================= }

const
  {$EXTERNALSYM LVM_FIRST}
  LVM_FIRST               = $1000;      { ListView messages }
  {$EXTERNALSYM TV_FIRST}
  TV_FIRST                = $1100;      { TreeView messages }
  {$EXTERNALSYM HDM_FIRST}
  HDM_FIRST               = $1200;      { Header messages }
  {$EXTERNALSYM TCM_FIRST}
  TCM_FIRST               = $1300;      { Tab control messages }
  {$EXTERNALSYM PGM_FIRST}
  PGM_FIRST               = $1400;      { Pager control messages }
  {$EXTERNALSYM CCM_FIRST}
  CCM_FIRST               = $2000;      { Common control shared messages }

  {$EXTERNALSYM CCM_SETBKCOLOR}
  CCM_SETBKCOLOR          = CCM_FIRST + 1; // lParam is bkColor

type
  {$EXTERNALSYM tagCOLORSCHEME}
  tagCOLORSCHEME = packed record
    dwSize: DWORD;
    clrBtnHighlight: COLORREF;    // highlight color
    clrBtnShadow: COLORREF;       // shadow color
  end;
  PColorScheme = ^TColorScheme;
  TColorScheme = tagCOLORSCHEME;

const
  {$EXTERNALSYM CCM_SETCOLORSCHEME}
  CCM_SETCOLORSCHEME      = CCM_FIRST + 2; // lParam is color scheme
  {$EXTERNALSYM CCM_GETCOLORSCHEME}
  CCM_GETCOLORSCHEME      = CCM_FIRST + 3; // fills in COLORSCHEME pointed to by lParam
  {$EXTERNALSYM CCM_GETDROPTARGET}
  CCM_GETDROPTARGET       = CCM_FIRST + 4;
  {$EXTERNALSYM CCM_SETUNICODEFORMAT}
  CCM_SETUNICODEFORMAT    = CCM_FIRST + 5;
  {$EXTERNALSYM CCM_GETUNICODEFORMAT}
  CCM_GETUNICODEFORMAT    = CCM_FIRST + 6;

  {$EXTERNALSYM INFOTIPSIZE}
  INFOTIPSIZE = 1024;  // for tooltips

{ ====== WM_NOTIFY codes (NMHDR.code values) ================== }

const
  {$EXTERNALSYM NM_FIRST}
  NM_FIRST                 = 0-  0;       { generic to all controls }
  {$EXTERNALSYM NM_LAST}
  NM_LAST                  = 0- 99;

  {$EXTERNALSYM LVN_FIRST}
  LVN_FIRST                = 0-100;       { listview }
  {$EXTERNALSYM LVN_LAST}
  LVN_LAST                 = 0-199;

  {$EXTERNALSYM HDN_FIRST}
  HDN_FIRST                = 0-300;       { header }
  {$EXTERNALSYM HDN_LAST}
  HDN_LAST                 = 0-399;

  {$EXTERNALSYM TVN_FIRST}
  TVN_FIRST                = 0-400;       { treeview }
  {$EXTERNALSYM TVN_LAST}
  TVN_LAST                 = 0-499;

  {$EXTERNALSYM TTN_FIRST}
  TTN_FIRST                = 0-520;       { tooltips }
  {$EXTERNALSYM TTN_LAST}
  TTN_LAST                 = 0-549;

  {$EXTERNALSYM TCN_FIRST}
  TCN_FIRST                = 0-550;       { tab control }
  {$EXTERNALSYM TCN_LAST}
  TCN_LAST                 = 0-580;

{ Shell reserved           (0-580) -  (0-589) }

  {$EXTERNALSYM CDN_FIRST}
  CDN_FIRST                = 0-601;       { common dialog (new) }
  {$EXTERNALSYM CDN_LAST}
  CDN_LAST                 = 0-699;

  {$EXTERNALSYM TBN_FIRST}
  TBN_FIRST                = 0-700;       { toolbar }
  {$EXTERNALSYM TBN_LAST}
  TBN_LAST                 = 0-720;

  {$EXTERNALSYM UDN_FIRST}
  UDN_FIRST                = 0-721;       { updown }
  {$EXTERNALSYM UDN_LAST}
  UDN_LAST                 = 0-740;

  {$EXTERNALSYM MCN_FIRST}
  MCN_FIRST                = 0-750;       { monthcal }
  {$EXTERNALSYM MCN_LAST}
  MCN_LAST                 = 0-759;

  {$EXTERNALSYM DTN_FIRST}
  DTN_FIRST                = 0-760;       { datetimepick }
  {$EXTERNALSYM DTN_LAST}
  DTN_LAST                 = 0-799;

  {$EXTERNALSYM CBEN_FIRST}
  CBEN_FIRST               = 0-800;       { combo box ex }
  {$EXTERNALSYM CBEN_LAST}
  CBEN_LAST                = 0-830;

  {$EXTERNALSYM RBN_FIRST}
  RBN_FIRST                = 0-831;       { coolbar }
  {$EXTERNALSYM RBN_LAST}
  RBN_LAST                 = 0-859;

  {$EXTERNALSYM IPN_FIRST}
  IPN_FIRST               = 0-860;       { internet address }
  {$EXTERNALSYM IPN_LAST}
  IPN_LAST                = 0-879;       { internet address }

  {$EXTERNALSYM SBN_FIRST}
  SBN_FIRST               = 0-880;       { status bar }
  {$EXTERNALSYM SBN_LAST}
  SBN_LAST                = 0-899;

  {$EXTERNALSYM PGN_FIRST}
  PGN_FIRST               = 0-900;       { Pager Control }
  {$EXTERNALSYM PGN_LAST}
  PGN_LAST                = 0-950;

  {$EXTERNALSYM MSGF_COMMCTRL_BEGINDRAG}
  MSGF_COMMCTRL_BEGINDRAG     = $4200;
  {$EXTERNALSYM MSGF_COMMCTRL_SIZEHEADER}
  MSGF_COMMCTRL_SIZEHEADER    = $4201;
  {$EXTERNALSYM MSGF_COMMCTRL_DRAGSELECT}
  MSGF_COMMCTRL_DRAGSELECT    = $4202;
  {$EXTERNALSYM MSGF_COMMCTRL_TOOLBARCUST}
  MSGF_COMMCTRL_TOOLBARCUST   = $4203;


{ ====== Generic WM_NOTIFY notification codes ================= }

const
  {$EXTERNALSYM NM_OUTOFMEMORY}
  NM_OUTOFMEMORY           = NM_FIRST-1;
  {$EXTERNALSYM NM_CLICK}
  NM_CLICK                 = NM_FIRST-2;
  {$EXTERNALSYM NM_DBLCLK}
  NM_DBLCLK                = NM_FIRST-3;
  {$EXTERNALSYM NM_RETURN}
  NM_RETURN                = NM_FIRST-4;
  {$EXTERNALSYM NM_RCLICK}
  NM_RCLICK                = NM_FIRST-5;
  {$EXTERNALSYM NM_RDBLCLK}
  NM_RDBLCLK               = NM_FIRST-6;
  {$EXTERNALSYM NM_SETFOCUS}
  NM_SETFOCUS              = NM_FIRST-7;
  {$EXTERNALSYM NM_KILLFOCUS}
  NM_KILLFOCUS             = NM_FIRST-8;
  {$EXTERNALSYM NM_CUSTOMDRAW}
  NM_CUSTOMDRAW            = NM_FIRST-12;
  {$EXTERNALSYM NM_HOVER}
  NM_HOVER                 = NM_FIRST-13;
  {$EXTERNALSYM NM_NCHITTEST}
  NM_NCHITTEST             = NM_FIRST-14;   // uses NMMOUSE struct
  {$EXTERNALSYM NM_KEYDOWN}
  NM_KEYDOWN               = NM_FIRST-15;   // uses NMKEY struct
  {$EXTERNALSYM NM_RELEASEDCAPTURE}
  NM_RELEASEDCAPTURE       = NM_FIRST-16;
  {$EXTERNALSYM NM_SETCURSOR}
  NM_SETCURSOR             = NM_FIRST-17;   // uses NMMOUSE struct
  {$EXTERNALSYM NM_CHAR}
  NM_CHAR                  = NM_FIRST-18;   // uses NMCHAR struct

type
  {$EXTERNALSYM tagNMMOUSE}
  tagNMMOUSE = packed record
    hdr: TNMHdr;
    dwItemSpec: DWORD;
    dwItemData: DWORD;
    pt: TPoint;
    dwHitInfo: DWORD; // any specifics about where on the item or control the mouse is
  end;
  PNMMouse = ^TNMMouse;
  TNMMouse = tagNMMOUSE;

  PNMClick = ^TNMClick;
  TNMClick = tagNMMOUSE;

  // Generic structure to request an object of a specific type.
  {$EXTERNALSYM tagNMOBJECTNOTIFY}
  tagNMOBJECTNOTIFY = packed record
    hdr: TNMHdr;
    iItem: Integer;
    piid: PGUID;
    pObject: Pointer;
    hResult: HRESULT;
    dwFlags: DWORD;    // control specific flags (hints as to where in iItem it hit)
  end;
  PNMObjectNotify = ^TNMObjectNotify;
  TNMObjectNotify = tagNMOBJECTNOTIFY;

  // Generic structure for a key
  {$EXTERNALSYM tagNMKEY}
  tagNMKEY = packed record
    hdr: TNMHdr;
    nVKey: UINT;
    uFlags: UINT;
  end;
  PNMKey = ^TNMKey;
  TNMKey = tagNMKEY;

  // Generic structure for a character
  {$EXTERNALSYM tagNMCHAR}
  tagNMCHAR = packed record
    hdr: TNMHdr;
    ch: UINT;
    dwItemPrev: DWORD;     // Item previously selected
    dwItemNext: DWORD;     // Item to be selected
  end;
  PNMChar = ^TNMChar;
  TNMChar = tagNMCHAR;

{ ==================== CUSTOM DRAW ========================================== }

const
  // custom draw return flags
  // values under 0x00010000 are reserved for global custom draw values.
  // above that are for specific controls
  {$EXTERNALSYM CDRF_DODEFAULT}
  CDRF_DODEFAULT          = $00000000;
  {$EXTERNALSYM CDRF_NEWFONT}
  CDRF_NEWFONT            = $00000002;
  {$EXTERNALSYM CDRF_SKIPDEFAULT}
  CDRF_SKIPDEFAULT        = $00000004;

  {$EXTERNALSYM CDRF_NOTIFYPOSTPAINT}
  CDRF_NOTIFYPOSTPAINT    = $00000010;
  {$EXTERNALSYM CDRF_NOTIFYITEMDRAW}
  CDRF_NOTIFYITEMDRAW     = $00000020;
  {$EXTERNALSYM CDRF_NOTIFYSUBITEMDRAW}
  CDRF_NOTIFYSUBITEMDRAW  = $00000020;  // flags are the same, we can distinguish by context
  {$EXTERNALSYM CDRF_NOTIFYPOSTERASE}
  CDRF_NOTIFYPOSTERASE    = $00000040;

  // drawstage flags
  // values under = $00010000 are reserved for global custom draw values.
  // above that are for specific controls
  {$EXTERNALSYM CDDS_PREPAINT}
  CDDS_PREPAINT           = $00000001;
  {$EXTERNALSYM CDDS_POSTPAINT}
  CDDS_POSTPAINT          = $00000002;
  {$EXTERNALSYM CDDS_PREERASE}
  CDDS_PREERASE           = $00000003;
  {$EXTERNALSYM CDDS_POSTERASE}
  CDDS_POSTERASE          = $00000004;
  // the = $000010000 bit means it's individual item specific
  {$EXTERNALSYM CDDS_ITEM}
  CDDS_ITEM               = $00010000;
  {$EXTERNALSYM CDDS_ITEMPREPAINT}
  CDDS_ITEMPREPAINT       = CDDS_ITEM or CDDS_PREPAINT;
  {$EXTERNALSYM CDDS_ITEMPOSTPAINT}
  CDDS_ITEMPOSTPAINT      = CDDS_ITEM or CDDS_POSTPAINT;
  {$EXTERNALSYM CDDS_ITEMPREERASE}
  CDDS_ITEMPREERASE       = CDDS_ITEM or CDDS_PREERASE;
  {$EXTERNALSYM CDDS_ITEMPOSTERASE}
  CDDS_ITEMPOSTERASE      = CDDS_ITEM or CDDS_POSTERASE;
  {$EXTERNALSYM CDDS_SUBITEM}
  CDDS_SUBITEM            = $00020000;

  // itemState flags
  {$EXTERNALSYM CDIS_SELECTED}
  CDIS_SELECTED       = $0001;
  {$EXTERNALSYM CDIS_GRAYED}
  CDIS_GRAYED         = $0002;
  {$EXTERNALSYM CDIS_DISABLED}
  CDIS_DISABLED       = $0004;
  {$EXTERNALSYM CDIS_CHECKED}
  CDIS_CHECKED        = $0008;
  {$EXTERNALSYM CDIS_FOCUS}
  CDIS_FOCUS          = $0010;
  {$EXTERNALSYM CDIS_DEFAULT}
  CDIS_DEFAULT        = $0020;
  {$EXTERNALSYM CDIS_HOT}
  CDIS_HOT            = $0040;
  {$EXTERNALSYM CDIS_MARKED}
  CDIS_MARKED         = $0080;
  {$EXTERNALSYM CDIS_INDETERMINATE}
  CDIS_INDETERMINATE  = $0100;
  {$EXTERNALSYM CDIS_SHOWKEYBOARDCUES}
  CDIS_SHOWKEYBOARDCUES = $0200;

type
  {$EXTERNALSYM tagNMCUSTOMDRAWINFO}
  tagNMCUSTOMDRAWINFO = packed record
    hdr: TNMHdr;
    dwDrawStage: DWORD;
    hdc: HDC;
    rc: TRect;
    dwItemSpec: DWORD;  // this is control specific, but it's how to specify an item.  valid only with CDDS_ITEM bit set
    uItemState: UINT;
    lItemlParam: LPARAM;
  end;
  PNMCustomDraw = ^TNMCustomDraw;
  TNMCustomDraw = tagNMCUSTOMDRAWINFO;

  {$EXTERNALSYM tagNMTTCUSTOMDRAW}
  tagNMTTCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    uDrawFlags: UINT;
  end;
  PNMTTCustomDraw = ^TNMTTCustomDraw;
  TNMTTCustomDraw = tagNMTTCUSTOMDRAW;

{ ====== IMAGE LIST =========================================== }

const
  {$EXTERNALSYM CLR_NONE}
  CLR_NONE                = $FFFFFFFF;
  {$EXTERNALSYM CLR_DEFAULT}
  CLR_DEFAULT             = $FF000000;

type
  {$EXTERNALSYM HIMAGELIST}
  HIMAGELIST = THandle;

  {$EXTERNALSYM _IMAGELISTDRAWPARAMS}
  _IMAGELISTDRAWPARAMS = packed record
    cbSize: DWORD;
    himl: HIMAGELIST;
    i: Integer;
    hdcDst: HDC;
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    xBitmap: Integer;        // x offest from the upperleft of bitmap
    yBitmap: Integer;        // y offset from the upperleft of bitmap
    rgbBk: COLORREF;
    rgbFg: COLORREF;
    fStyle: UINT;
    dwRop: DWORD;
  end;
  PImageListDrawParams = ^TImageListDrawParams;
  TImageListDrawParams = _IMAGELISTDRAWPARAMS;

const
  {$EXTERNALSYM ILC_MASK}
  ILC_MASK                = $0001;
  {$EXTERNALSYM ILC_COLOR}
  ILC_COLOR               = $0000;
  {$EXTERNALSYM ILC_COLORDDB}
  ILC_COLORDDB            = $00FE;
  {$EXTERNALSYM ILC_COLOR4}
  ILC_COLOR4              = $0004;
  {$EXTERNALSYM ILC_COLOR8}
  ILC_COLOR8              = $0008;
  {$EXTERNALSYM ILC_COLOR16}
  ILC_COLOR16             = $0010;
  {$EXTERNALSYM ILC_COLOR24}
  ILC_COLOR24             = $0018;
  {$EXTERNALSYM ILC_COLOR32}
  ILC_COLOR32             = $0020;
  {$EXTERNALSYM ILC_PALETTE}
  ILC_PALETTE             = $0800;

{$EXTERNALSYM ImageList_Create}
function ImageList_Create(CX, CY: Integer; Flags: UINT;
  Initial, Grow: Integer): HIMAGELIST; stdcall;
{$EXTERNALSYM ImageList_Destroy}
function ImageList_Destroy(ImageList: HIMAGELIST): Bool; stdcall;
{$EXTERNALSYM ImageList_GetImageCount}
function ImageList_GetImageCount(ImageList: HIMAGELIST): Integer; stdcall;
{$EXTERNALSYM ImageList_SetImageCount}
function ImageList_SetImageCount(himl: HIMAGELIST; uNewCount: UINT): Integer; stdcall;
{$EXTERNALSYM ImageList_Add}
function ImageList_Add(ImageList: HIMAGELIST; Image, Mask: HBitmap): Integer; stdcall;
{$EXTERNALSYM ImageList_ReplaceIcon}
function ImageList_ReplaceIcon(ImageList: HIMAGELIST; Index: Integer;
  Icon: HIcon): Integer; stdcall;
{$EXTERNALSYM ImageList_SetBkColor}
function ImageList_SetBkColor(ImageList: HIMAGELIST; ClrBk: TColorRef): TColorRef; stdcall;
{$EXTERNALSYM ImageList_GetBkColor}
function ImageList_GetBkColor(ImageList: HIMAGELIST): TColorRef; stdcall;
{$EXTERNALSYM ImageList_SetOverlayImage}
function ImageList_SetOverlayImage(ImageList: HIMAGELIST; Image: Integer;
  Overlay: Integer): Bool; stdcall;

{$EXTERNALSYM ImageList_AddIcon}
function ImageList_AddIcon(ImageList: HIMAGELIST; Icon: HIcon): Integer; inline;

const
  {$EXTERNALSYM ILD_NORMAL}
  ILD_NORMAL              = $0000;
  {$EXTERNALSYM ILD_TRANSPARENT}
  ILD_TRANSPARENT         = $0001;
  {$EXTERNALSYM ILD_MASK}
  ILD_MASK                = $0010;
  {$EXTERNALSYM ILD_IMAGE}
  ILD_IMAGE               = $0020;
  {$EXTERNALSYM ILD_ROP}
  ILD_ROP                 = $0040;
  {$EXTERNALSYM ILD_BLEND25}
  ILD_BLEND25             = $0002;
  {$EXTERNALSYM ILD_BLEND50}
  ILD_BLEND50             = $0004;
  {$EXTERNALSYM ILD_OVERLAYMASK}
  ILD_OVERLAYMASK         = $0F00;

{$EXTERNALSYM IndexToOverlayMask}
function IndexToOverlayMask(Index: Integer): Integer; inline;

const
  {$EXTERNALSYM ILD_SELECTED}
  ILD_SELECTED            = ILD_BLEND50;
  {$EXTERNALSYM ILD_FOCUS}
  ILD_FOCUS               = ILD_BLEND25;
  {$EXTERNALSYM ILD_BLEND}
  ILD_BLEND               = ILD_BLEND50;
  {$EXTERNALSYM CLR_HILIGHT}
  CLR_HILIGHT             = CLR_DEFAULT;

{$EXTERNALSYM ImageList_Draw}
function ImageList_Draw(ImageList: HIMAGELIST; Index: Integer;
  Dest: HDC; X, Y: Integer; Style: UINT): Bool; stdcall;

{$EXTERNALSYM ImageList_Replace}
function ImageList_Replace(ImageList: HIMAGELIST; Index: Integer;
  Image, Mask: HBitmap): Bool; stdcall;
{$EXTERNALSYM ImageList_AddMasked}
function ImageList_AddMasked(ImageList: HIMAGELIST; Image: HBitmap;
  Mask: TColorRef): Integer; stdcall;
{$EXTERNALSYM ImageList_DrawEx}
function ImageList_DrawEx(ImageList: HIMAGELIST; Index: Integer;
  Dest: HDC; X, Y, DX, DY: Integer; Bk, Fg: TColorRef; Style: Cardinal): Bool; stdcall;
{$EXTERNALSYM ImageList_DrawIndirect}
function ImageList_DrawIndirect(pimldp: PImageListDrawParams): Integer; stdcall;
{$EXTERNALSYM ImageList_Remove}
function ImageList_Remove(ImageList: HIMAGELIST; Index: Integer): Bool; stdcall;
{$EXTERNALSYM ImageList_GetIcon}
function ImageList_GetIcon(ImageList: HIMAGELIST; Index: Integer;
  Flags: Cardinal): HIcon; stdcall;
{$EXTERNALSYM ImageList_LoadImage}
function ImageList_LoadImage(Instance: THandle; Bmp: PChar; CX, Grow: Integer;
  Mask: TColorRef; pType, Flags: Cardinal): HIMAGELIST; stdcall;
{$EXTERNALSYM ImageList_LoadImageA}
function ImageList_LoadImageA(Instance: THandle; Bmp: PAnsiChar; CX, Grow: Integer;
  Mask: TColorRef; pType, Flags: Cardinal): HIMAGELIST; stdcall;
{$EXTERNALSYM ImageList_LoadImageW}
function ImageList_LoadImageW(Instance: THandle; Bmp: PWideChar; CX, Grow: Integer;
  Mask: TColorRef; pType, Flags: Cardinal): HIMAGELIST; stdcall;

const
  {$EXTERNALSYM ILCF_MOVE}
  ILCF_MOVE   = $00000000;
  {$EXTERNALSYM ILCF_SWAP}
  ILCF_SWAP   = $00000001;

{$EXTERNALSYM ImageList_Copy}
function ImageList_Copy(himlDst: HIMAGELIST; iDst: Integer; himlSrc: HIMAGELIST;
  Src: Integer; uFlags: UINT): Integer; stdcall;

{$EXTERNALSYM ImageList_BeginDrag}
function ImageList_BeginDrag(ImageList: HIMAGELIST; Track: Integer;
  XHotSpot, YHotSpot: Integer): Bool; stdcall;
{$EXTERNALSYM ImageList_EndDrag}
function ImageList_EndDrag: Bool; stdcall;
{$EXTERNALSYM ImageList_DragEnter}
function ImageList_DragEnter(LockWnd: HWnd; X, Y: Integer): Bool; stdcall;
{$EXTERNALSYM ImageList_DragLeave}
function ImageList_DragLeave(LockWnd: HWnd): Bool; stdcall;
{$EXTERNALSYM ImageList_DragMove}
function ImageList_DragMove(X, Y: Integer): Bool; stdcall;
{$EXTERNALSYM ImageList_SetDragCursorImage}
function ImageList_SetDragCursorImage(ImageList: HIMAGELIST; Drag: Integer;
  XHotSpot, YHotSpot: Integer): Bool; stdcall;
{$EXTERNALSYM ImageList_DragShowNolock}
function ImageList_DragShowNolock(Show: Bool): Bool; stdcall;
{$EXTERNALSYM ImageList_GetDragImage}
function ImageList_GetDragImage(Point, HotSpot: PPoint): HIMAGELIST; stdcall;

{ macros }
{$EXTERNALSYM ImageList_RemoveAll}
procedure ImageList_RemoveAll(ImageList: HIMAGELIST); inline;
{$EXTERNALSYM ImageList_ExtractIcon}
function ImageList_ExtractIcon(Instance: THandle; ImageList: HIMAGELIST;
  Image: Integer): HIcon; inline;
{$EXTERNALSYM ImageList_LoadBitmap}
function ImageList_LoadBitmap(Instance: THandle; Bmp: PChar;
  CX, Grow: Integer; MasK: TColorRef): HIMAGELIST;
{$EXTERNALSYM ImageList_LoadBitmapA}
function ImageList_LoadBitmapA(Instance: THandle; Bmp: PAnsiChar;
  CX, Grow: Integer; MasK: TColorRef): HIMAGELIST;
{$EXTERNALSYM ImageList_LoadBitmapW}
function ImageList_LoadBitmapW(Instance: THandle; Bmp: PWideChar;
  CX, Grow: Integer; MasK: TColorRef): HIMAGELIST;

{$EXTERNALSYM ImageList_Read}
function ImageList_Read(Stream: IStream): HIMAGELIST; stdcall;
{$EXTERNALSYM ImageList_Write}
function ImageList_Write(ImageList: HIMAGELIST; Stream: IStream): BOOL; stdcall;

type
  PImageInfo = ^TImageInfo;
  {$EXTERNALSYM _IMAGEINFO}
  _IMAGEINFO = packed record
    hbmImage: HBitmap;
    hbmMask: HBitmap;
    Unused1: Integer;
    Unused2: Integer;
    rcImage: TRect;
  end;
  TImageInfo = _IMAGEINFO;
  {$EXTERNALSYM IMAGEINFO}
  IMAGEINFO = _IMAGEINFO;

{$EXTERNALSYM ImageList_GetIconSize}
function ImageList_GetIconSize(ImageList: HIMAGELIST; var CX, CY: Integer): Bool; stdcall;
{$EXTERNALSYM ImageList_SetIconSize}
function ImageList_SetIconSize(ImageList: HIMAGELIST; CX, CY: Integer): Bool; stdcall;
{$EXTERNALSYM ImageList_GetImageInfo}
function ImageList_GetImageInfo(ImageList: HIMAGELIST; Index: Integer;
  var ImageInfo: TImageInfo): Bool; stdcall;
{$EXTERNALSYM ImageList_Merge}
function ImageList_Merge(ImageList1: HIMAGELIST; Index1: Integer;
  ImageList2: HIMAGELIST; Index2: Integer; DX, DY: Integer): HIMAGELIST; stdcall;
{$EXTERNALSYM ImageList_Duplicate}
function ImageList_Duplicate(himl: HIMAGELIST): HIMAGELIST; stdcall;

{ ====== HEADER CONTROL ========================== }

const
  {$EXTERNALSYM WC_HEADER}
  WC_HEADER = 'SysHeader32';

  {$EXTERNALSYM HDS_HORZ}
  HDS_HORZ                = $00000000;
  {$EXTERNALSYM HDS_BUTTONS}
  HDS_BUTTONS             = $00000002;
  {$EXTERNALSYM HDS_HOTTRACK}
  HDS_HOTTRACK            = $00000004;
  {$EXTERNALSYM HDS_HIDDEN}
  HDS_HIDDEN              = $00000008;
  {$EXTERNALSYM HDS_DRAGDROP}
  HDS_DRAGDROP            = $00000040;
  {$EXTERNALSYM HDS_FULLDRAG}
  HDS_FULLDRAG            = $00000080;

type
  PHDItemA = ^THDItemA;
  PHDItemW = ^THDItemW;
  PHDItem = PHDItemA;
  {$EXTERNALSYM _HD_ITEMA}
  _HD_ITEMA = packed record
    Mask: Cardinal;
    cxy: Integer;
    pszText: PAnsiChar;
    hbm: HBITMAP;
    cchTextMax: Integer;
    fmt: Integer;
    lParam: LPARAM;
    iImage: Integer;        // index of bitmap in ImageList
    iOrder: Integer;        // where to draw this item
  end;
  {$EXTERNALSYM _HD_ITEMW}
  _HD_ITEMW = packed record
    Mask: Cardinal;
    cxy: Integer;
    pszText: PWideChar;
    hbm: HBITMAP;
    cchTextMax: Integer;
    fmt: Integer;
    lParam: LPARAM;
    iImage: Integer;        // index of bitmap in ImageList
    iOrder: Integer;        // where to draw this item
  end;
  {$EXTERNALSYM _HD_ITEM}
  _HD_ITEM = _HD_ITEMA;
  THDItemA = _HD_ITEMA;
  THDItemW = _HD_ITEMW;
  THDItem = THDItemA;
  {$EXTERNALSYM HD_ITEMA}
  HD_ITEMA = _HD_ITEMA;
  {$EXTERNALSYM HD_ITEMW}
  HD_ITEMW = _HD_ITEMW;
  {$EXTERNALSYM HD_ITEM}
  HD_ITEM = HD_ITEMA;

const
  {$EXTERNALSYM HDI_WIDTH}
  HDI_WIDTH               = $0001;
  {$EXTERNALSYM HDI_HEIGHT}
  HDI_HEIGHT              = HDI_WIDTH;
  {$EXTERNALSYM HDI_TEXT}
  HDI_TEXT                = $0002;
  {$EXTERNALSYM HDI_FORMAT}
  HDI_FORMAT              = $0004;
  {$EXTERNALSYM HDI_LPARAM}
  HDI_LPARAM              = $0008;
  {$EXTERNALSYM HDI_BITMAP}
  HDI_BITMAP              = $0010;
  {$EXTERNALSYM HDI_IMAGE}
  HDI_IMAGE               = $0020;
  {$EXTERNALSYM HDI_DI_SETITEM}
  HDI_DI_SETITEM          = $0040;
  {$EXTERNALSYM HDI_ORDER}
  HDI_ORDER               = $0080;

  {$EXTERNALSYM HDF_LEFT}
  HDF_LEFT                = 0;
  {$EXTERNALSYM HDF_RIGHT}
  HDF_RIGHT               = 1;
  {$EXTERNALSYM HDF_CENTER}
  HDF_CENTER              = 2;
  {$EXTERNALSYM HDF_JUSTIFYMASK}
  HDF_JUSTIFYMASK         = $0003;
  {$EXTERNALSYM HDF_RTLREADING}
  HDF_RTLREADING          = 4; 

  {$EXTERNALSYM HDF_OWNERDRAW}
  HDF_OWNERDRAW           = $8000;
  {$EXTERNALSYM HDF_STRING}
  HDF_STRING              = $4000;
  {$EXTERNALSYM HDF_BITMAP}
  HDF_BITMAP              = $2000;
  {$EXTERNALSYM HDF_BITMAP_ON_RIGHT}
  HDF_BITMAP_ON_RIGHT     = $1000;
  {$EXTERNALSYM HDF_IMAGE}
  HDF_IMAGE               = $0800;

  {$EXTERNALSYM HDM_GETITEMCOUNT}
  HDM_GETITEMCOUNT        = HDM_FIRST + 0;

{$EXTERNALSYM Header_GetItemCount}
function Header_GetItemCount(Header: HWnd): Integer; inline;

const
  {$EXTERNALSYM HDM_INSERTITEMW}
  HDM_INSERTITEMW          = HDM_FIRST + 10;
  {$EXTERNALSYM HDM_INSERTITEMA}
  HDM_INSERTITEMA          = HDM_FIRST + 1;




  {$EXTERNALSYM HDM_INSERTITEM}
  HDM_INSERTITEM           = HDM_INSERTITEMA;


{$EXTERNALSYM Header_InsertItem}
function Header_InsertItem(Header: HWnd; Index: Integer;
  const Item: THDItem): Integer; inline;
{$EXTERNALSYM Header_InsertItemA}
function Header_InsertItemA(Header: HWnd; Index: Integer;
  const Item: THDItemA): Integer; inline;
{$EXTERNALSYM Header_InsertItemW}
function Header_InsertItemW(Header: HWnd; Index: Integer;
  const Item: THDItemW): Integer; inline;

const
  {$EXTERNALSYM HDM_DELETEITEM}
  HDM_DELETEITEM          = HDM_FIRST + 2;

{$EXTERNALSYM Header_DeleteItem}
function Header_DeleteItem(Header: HWnd; Index: Integer): Bool; inline;

const
  {$EXTERNALSYM HDM_GETITEMW}
  HDM_GETITEMW             = HDM_FIRST + 11;
  {$EXTERNALSYM HDM_GETITEMA}
  HDM_GETITEMA             = HDM_FIRST + 3;




  {$EXTERNALSYM HDM_GETITEM}
  HDM_GETITEM              = HDM_GETITEMA;


{$EXTERNALSYM Header_GetItem}
function Header_GetItem(Header: HWnd; Index: Integer;
  var Item: THDItem): Bool; inline;
{$EXTERNALSYM Header_GetItemA}
function Header_GetItemA(Header: HWnd; Index: Integer;
  var Item: THDItemA): Bool; inline;
{$EXTERNALSYM Header_GetItemW}
function Header_GetItemW(Header: HWnd; Index: Integer;
  var Item: THDItemW): Bool; inline;

const
  {$EXTERNALSYM HDM_SETITEMA}
  HDM_SETITEMA            = HDM_FIRST + 4;
  {$EXTERNALSYM HDM_SETITEMW}
  HDM_SETITEMW            = HDM_FIRST + 12;




  {$EXTERNALSYM HDM_SETITEM}
  HDM_SETITEM             = HDM_SETITEMA;


{$EXTERNALSYM Header_SetItem}
function Header_SetItem(Header: HWnd; Index: Integer; const Item: THDItem): Bool; inline;
{$EXTERNALSYM Header_SetItemA}
function Header_SetItemA(Header: HWnd; Index: Integer; const Item: THDItemA): Bool; inline;
{$EXTERNALSYM Header_SetItemW}
function Header_SetItemW(Header: HWnd; Index: Integer; const Item: THDItemW): Bool; inline;

type
  PHDLayout = ^THDLayout;
  {$EXTERNALSYM _HD_LAYOUT}
  _HD_LAYOUT = packed record
    Rect: ^TRect;
    WindowPos: PWindowPos;
  end;
  THDLayout = _HD_LAYOUT;
  {$EXTERNALSYM HD_LAYOUT}
  HD_LAYOUT = _HD_LAYOUT;

const
  {$EXTERNALSYM HDM_LAYOUT}
  HDM_LAYOUT              = HDM_FIRST + 5;

{$EXTERNALSYM Header_Layout}
function Header_Layout(Header: HWnd; Layout: PHDLayout): Bool; inline;

const
  {$EXTERNALSYM HHT_NOWHERE}
  HHT_NOWHERE             = $0001;
  {$EXTERNALSYM HHT_ONHEADER}
  HHT_ONHEADER            = $0002;
  {$EXTERNALSYM HHT_ONDIVIDER}
  HHT_ONDIVIDER           = $0004;
  {$EXTERNALSYM HHT_ONDIVOPEN}
  HHT_ONDIVOPEN           = $0008;
  {$EXTERNALSYM HHT_ABOVE}
  HHT_ABOVE               = $0100;
  {$EXTERNALSYM HHT_BELOW}
  HHT_BELOW               = $0200;
  {$EXTERNALSYM HHT_TORIGHT}
  HHT_TORIGHT             = $0400;
  {$EXTERNALSYM HHT_TOLEFT}
  HHT_TOLEFT              = $0800;

type
  PHDHitTestInfo = ^THDHitTestInfo;
  {$EXTERNALSYM _HD_HITTESTINFO}
  _HD_HITTESTINFO = packed record
    Point: TPoint;
    Flags: Cardinal;
    Item: Integer;
  end;
  THDHitTestInfo = _HD_HITTESTINFO;
  {$EXTERNALSYM HD_HITTESTINFO}
  HD_HITTESTINFO = _HD_HITTESTINFO;

const
  {$EXTERNALSYM HDM_HITTEST}
  HDM_HITTEST             = HDM_FIRST + 6;
  {$EXTERNALSYM HDM_GETITEMRECT}
  HDM_GETITEMRECT         = HDM_FIRST + 7;
  {$EXTERNALSYM HDM_SETIMAGELIST}
  HDM_SETIMAGELIST        = HDM_FIRST + 8;
  {$EXTERNALSYM HDM_GETIMAGELIST}
  HDM_GETIMAGELIST        = HDM_FIRST + 9;
  {$EXTERNALSYM HDM_ORDERTOINDEX}
  HDM_ORDERTOINDEX        = HDM_FIRST + 15;
  {$EXTERNALSYM HDM_CREATEDRAGIMAGE}
  HDM_CREATEDRAGIMAGE     = HDM_FIRST + 16;  // wparam = which item = by index;
  {$EXTERNALSYM HDM_GETORDERARRAY}
  HDM_GETORDERARRAY       = HDM_FIRST + 17;
  {$EXTERNALSYM HDM_SETORDERARRAY}
  HDM_SETORDERARRAY       = HDM_FIRST + 18;
  {$EXTERNALSYM HDM_SETHOTDIVIDER}
  HDM_SETHOTDIVIDER       = HDM_FIRST + 19;
  {$EXTERNALSYM HDM_SETUNICODEFORMAT}
  HDM_SETUNICODEFORMAT    = CCM_SETUNICODEFORMAT;
  {$EXTERNALSYM HDM_GETUNICODEFORMAT}
  HDM_GETUNICODEFORMAT    = CCM_GETUNICODEFORMAT;

{$EXTERNALSYM Header_GetItemRect}
function Header_GetItemRect(hwnd: HWND; iItem: Integer; lprc: PRect): Integer; inline;
{$EXTERNALSYM Header_SetImageList}
function Header_SetImageList(hwnd: HWND; himl: HIMAGELIST): HIMAGELIST; inline;
{$EXTERNALSYM Header_GetImageList}
function Header_GetImageList(hwnd: HWND): HIMAGELIST; inline;
{$EXTERNALSYM Header_OrderToIndex}
function Header_OrderToIndex(hwnd: HWND; i: Integer): Integer; inline;
{$EXTERNALSYM Header_CreateDragImage}
function Header_CreateDragImage(hwnd: HWND; i: Integer): HIMAGELIST; inline;
{$EXTERNALSYM Header_GetOrderArray}
function Header_GetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer; inline;
{$EXTERNALSYM Header_SetOrderArray}
function Header_SetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer; inline;

// lparam = int array of size HDM_GETITEMCOUNT
// the array specifies the order that all items should be displayed.
// e.g.  { 2, 0, 1}
// says the index 2 item should be shown in the 0ths position
//      index 0 should be shown in the 1st position
//      index 1 should be shown in the 2nd position

{$EXTERNALSYM Header_SetHotDivider}
function Header_SetHotDivider(hwnd: HWND; fPos: BOOL; dw: DWORD): Integer; inline;

// convenience message for external dragdrop
// wParam = BOOL  specifying whether the lParam is a dwPos of the cursor
//              position or the index of which divider to hotlight
// lParam = depends on wParam  (-1 and wParm = FALSE turns off hotlight)

{$EXTERNALSYM Header_SetUnicodeFormat}
function Header_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): Integer; inline;
{$EXTERNALSYM Header_GetUnicodeFormat}
function Header_GetUnicodeFormat(hwnd: HWND): Integer; inline;

const
  {$EXTERNALSYM HDN_ITEMCHANGINGA}
  HDN_ITEMCHANGINGA        = HDN_FIRST-0;
  {$EXTERNALSYM HDN_ITEMCHANGEDA}
  HDN_ITEMCHANGEDA         = HDN_FIRST-1;
  {$EXTERNALSYM HDN_ITEMCLICKA}
  HDN_ITEMCLICKA           = HDN_FIRST-2;
  {$EXTERNALSYM HDN_ITEMDBLCLICKA}
  HDN_ITEMDBLCLICKA        = HDN_FIRST-3;
  {$EXTERNALSYM HDN_DIVIDERDBLCLICKA}
  HDN_DIVIDERDBLCLICKA     = HDN_FIRST-5;
  {$EXTERNALSYM HDN_BEGINTRACKA}
  HDN_BEGINTRACKA          = HDN_FIRST-6;
  {$EXTERNALSYM HDN_ENDTRACKA}
  HDN_ENDTRACKA            = HDN_FIRST-7;
  {$EXTERNALSYM HDN_TRACKA}
  HDN_TRACKA               = HDN_FIRST-8;
  {$EXTERNALSYM HDN_GETDISPINFOA}
  HDN_GETDISPINFOA         = HDN_FIRST-9;
  {$EXTERNALSYM HDN_BEGINDRAG}
  HDN_BEGINDRAG            = HDN_FIRST-10;
  {$EXTERNALSYM HDN_ENDDRAG}
  HDN_ENDDRAG              = HDN_FIRST-11;

  {$EXTERNALSYM HDN_ITEMCHANGINGW}
  HDN_ITEMCHANGINGW        = HDN_FIRST-20;
  {$EXTERNALSYM HDN_ITEMCHANGEDW}
  HDN_ITEMCHANGEDW         = HDN_FIRST-21;
  {$EXTERNALSYM HDN_ITEMCLICKW}
  HDN_ITEMCLICKW           = HDN_FIRST-22;
  {$EXTERNALSYM HDN_ITEMDBLCLICKW}
  HDN_ITEMDBLCLICKW        = HDN_FIRST-23;
  {$EXTERNALSYM HDN_DIVIDERDBLCLICKW}
  HDN_DIVIDERDBLCLICKW     = HDN_FIRST-25;
  {$EXTERNALSYM HDN_BEGINTRACKW}
  HDN_BEGINTRACKW          = HDN_FIRST-26;
  {$EXTERNALSYM HDN_ENDTRACKW}
  HDN_ENDTRACKW            = HDN_FIRST-27;
  {$EXTERNALSYM HDN_TRACKW}
  HDN_TRACKW               = HDN_FIRST-28;
  {$EXTERNALSYM HDN_GETDISPINFOW}
  HDN_GETDISPINFOW         = HDN_FIRST-29;





















  {$EXTERNALSYM HDN_ITEMCHANGING}
  HDN_ITEMCHANGING        = HDN_ITEMCHANGINGA;
  {$EXTERNALSYM HDN_ITEMCHANGED}
  HDN_ITEMCHANGED         = HDN_ITEMCHANGEDA;
  {$EXTERNALSYM HDN_ITEMCLICK}
  HDN_ITEMCLICK           = HDN_ITEMCLICKA;
  {$EXTERNALSYM HDN_ITEMDBLCLICK}
  HDN_ITEMDBLCLICK        = HDN_ITEMDBLCLICKA;
  {$EXTERNALSYM HDN_DIVIDERDBLCLICK}
  HDN_DIVIDERDBLCLICK     = HDN_DIVIDERDBLCLICKA;
  {$EXTERNALSYM HDN_BEGINTRACK}
  HDN_BEGINTRACK          = HDN_BEGINTRACKA;
  {$EXTERNALSYM HDN_ENDTRACK}
  HDN_ENDTRACK            = HDN_ENDTRACKA;
  {$EXTERNALSYM HDN_TRACK}
  HDN_TRACK               = HDN_TRACKA;
  {$EXTERNALSYM HDN_GETDISPINFO}
  HDN_GETDISPINFO         = HDN_GETDISPINFOA;


type
  {$EXTERNALSYM tagNMHEADERA}
  tagNMHEADERA = packed record
    Hdr: TNMHdr;
    Item: Integer;
    Button: Integer;
    PItem: PHDItemA;
  end;
  {$EXTERNALSYM tagNMHEADERW}
  tagNMHEADERW = packed record
    Hdr: TNMHdr;
    Item: Integer;
    Button: Integer;
    PItem: PHDItemW;
  end;
  {$EXTERNALSYM tagNMHEADER}
  tagNMHEADER = tagNMHEADERA;
  {$EXTERNALSYM HD_NOTIFYA}
  HD_NOTIFYA = tagNMHEADERA;
  {$EXTERNALSYM HD_NOTIFYW}
  HD_NOTIFYW = tagNMHEADERW;
  {$EXTERNALSYM HD_NOTIFY}
  HD_NOTIFY = HD_NOTIFYA;
  PHDNotifyA = ^THDNotifyA;
  PHDNotifyW = ^THDNotifyW;
  PHDNotify = PHDNotifyA;
  THDNotifyA = tagNMHEADERA;
  THDNotifyW = tagNMHEADERW;
  THDNotify = THDNotifyA;

  {$EXTERNALSYM tagNMHDDISPINFOA}
  tagNMHDDISPINFOA = packed record
    hdr: TNMHdr;
    iItem: Integer;
    mask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagNMHDDISPINFOW}
  tagNMHDDISPINFOW = packed record
    hdr: TNMHdr;
    iItem: Integer;
    mask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagNMHDDISPINFO}
  tagNMHDDISPINFO = tagNMHDDISPINFOA;
  PNMHDispInfoA = ^TNMHDispInfoA;
  PNMHDispInfoW = ^TNMHDispInfoW;
  PNMHDispInfo = PNMHDispInfoA;
  TNMHDispInfoA = tagNMHDDISPINFOA;
  TNMHDispInfoW = tagNMHDDISPINFOW;
  TNMHDispInfo = TNMHDispInfoA;


{ ====== TOOLBAR CONTROL =================== }

const
  {$EXTERNALSYM TOOLBARCLASSNAME}
  TOOLBARCLASSNAME = 'ToolbarWindow32';

type
  PTBButton = ^TTBButton;
  {$EXTERNALSYM _TBBUTTON}
  _TBBUTTON = packed record
    iBitmap: Integer;
    idCommand: Integer;
    fsState: Byte;
    fsStyle: Byte;
    bReserved: array[1..2] of Byte;
    dwData: Longint;
    iString: Integer;
  end;
  TTBButton = _TBBUTTON;

  PColorMap = ^TColorMap;
  {$EXTERNALSYM _COLORMAP}
  _COLORMAP = packed record
    cFrom: TColorRef;
    cTo: TColorRef;
  end;
  TColorMap = _COLORMAP;
  {$EXTERNALSYM COLORMAP}
  COLORMAP = _COLORMAP;

{$EXTERNALSYM CreateToolBarEx}
function CreateToolBarEx(Wnd: HWnd; ws: Longint; ID: UINT;
  Bitmaps: Integer; BMInst: THandle; BMID: Cardinal; Buttons: PTBButton;
  NumButtons: Integer; dxButton, dyButton: Integer;
  dxBitmap, dyBitmap: Integer; StructSize: UINT): HWnd; stdcall;

{$EXTERNALSYM CreateMappedBitmap}
function CreateMappedBitmap(Instance: THandle; Bitmap: Integer;
  Flags: UINT; ColorMap: PColorMap; NumMaps: Integer): HBitmap; stdcall;

const

  {$EXTERNALSYM CMB_MASKED}
  CMB_MASKED              = $02;

  {$EXTERNALSYM TBSTATE_CHECKED}
  TBSTATE_CHECKED         = $01;
  {$EXTERNALSYM TBSTATE_PRESSED}
  TBSTATE_PRESSED         = $02;
  {$EXTERNALSYM TBSTATE_ENABLED}
  TBSTATE_ENABLED         = $04;
  {$EXTERNALSYM TBSTATE_HIDDEN}
  TBSTATE_HIDDEN          = $08;
  {$EXTERNALSYM TBSTATE_INDETERMINATE}
  TBSTATE_INDETERMINATE   = $10;
  {$EXTERNALSYM TBSTATE_WRAP}
  TBSTATE_WRAP            = $20;
  {$EXTERNALSYM TBSTATE_ELLIPSES}
  TBSTATE_ELLIPSES        = $40;
  {$EXTERNALSYM TBSTATE_MARKED}
  TBSTATE_MARKED          = $80;

  {$EXTERNALSYM TBSTYLE_BUTTON}
  TBSTYLE_BUTTON          = $00;
  {$EXTERNALSYM TBSTYLE_SEP}
  TBSTYLE_SEP             = $01;
  {$EXTERNALSYM TBSTYLE_CHECK}
  TBSTYLE_CHECK           = $02;
  {$EXTERNALSYM TBSTYLE_GROUP}
  TBSTYLE_GROUP           = $04;
  {$EXTERNALSYM TBSTYLE_CHECKGROUP}
  TBSTYLE_CHECKGROUP      = TBSTYLE_GROUP or TBSTYLE_CHECK;
  {$EXTERNALSYM TBSTYLE_DROPDOWN}
  TBSTYLE_DROPDOWN        = $08;
  {$EXTERNALSYM TBSTYLE_AUTOSIZE}
  TBSTYLE_AUTOSIZE        = $0010; // automatically calculate the cx of the button
  {$EXTERNALSYM TBSTYLE_NOPREFIX}
  TBSTYLE_NOPREFIX        = $0020; // if this button should not have accel prefix

  {$EXTERNALSYM TBSTYLE_TOOLTIPS}
  TBSTYLE_TOOLTIPS        = $0100;
  {$EXTERNALSYM TBSTYLE_WRAPABLE}
  TBSTYLE_WRAPABLE        = $0200;
  {$EXTERNALSYM TBSTYLE_ALTDRAG}
  TBSTYLE_ALTDRAG         = $0400;
  {$EXTERNALSYM TBSTYLE_FLAT}
  TBSTYLE_FLAT            = $0800;
  {$EXTERNALSYM TBSTYLE_LIST}
  TBSTYLE_LIST            = $1000;
  {$EXTERNALSYM TBSTYLE_CUSTOMERASE}
  TBSTYLE_CUSTOMERASE     = $2000;
  {$EXTERNALSYM TBSTYLE_REGISTERDROP}
  TBSTYLE_REGISTERDROP    = $4000;
  {$EXTERNALSYM TBSTYLE_TRANSPARENT}
  TBSTYLE_TRANSPARENT     = $8000;
  {$EXTERNALSYM TBSTYLE_EX_DRAWDDARROWS}
  TBSTYLE_EX_DRAWDDARROWS = $00000001;

  { For IE >= 0x0500 }
  {$EXTERNALSYM BTNS_BUTTON}
  BTNS_BUTTON             = TBSTYLE_BUTTON;
  {$EXTERNALSYM BTNS_SEP}
  BTNS_SEP                = TBSTYLE_SEP;
  {$EXTERNALSYM BTNS_CHECK}
  BTNS_CHECK              = TBSTYLE_CHECK;
  {$EXTERNALSYM BTNS_GROUP}
  BTNS_GROUP              = TBSTYLE_GROUP;
  {$EXTERNALSYM BTNS_CHECKGROUP}
  BTNS_CHECKGROUP         = TBSTYLE_CHECKGROUP;
  {$EXTERNALSYM BTNS_DROPDOWN}
  BTNS_DROPDOWN           = TBSTYLE_DROPDOWN;
  {$EXTERNALSYM BTNS_AUTOSIZE}
  BTNS_AUTOSIZE           = TBSTYLE_AUTOSIZE;
  {$EXTERNALSYM BTNS_NOPREFIX}
  BTNS_NOPREFIX           = TBSTYLE_NOPREFIX;
  { For IE >= 0x0501 }
  {$EXTERNALSYM BTNS_SHOWTEXT}
  BTNS_SHOWTEXT           = $0040;  // ignored unless TBSTYLE_EX_MIXEDBUTTONS is set

  { For IE >= 0x0500 }
  {$EXTERNALSYM BTNS_WHOLEDROPDOWN}
  BTNS_WHOLEDROPDOWN      = $0080;  // draw drop-down arrow, but without split arrow section

  { For IE >= 0x0501 }
  {$EXTERNALSYM TBSTYLE_EX_MIXEDBUTTONS}
  TBSTYLE_EX_MIXEDBUTTONS = $00000008;
  {$EXTERNALSYM TBSTYLE_EX_HIDECLIPPEDBUTTONS}
  TBSTYLE_EX_HIDECLIPPEDBUTTONS = $00000010;  // don't show partially obscured buttons

  { For Windows >= XP }
  {$EXTERNALSYM TBSTYLE_EX_DOUBLEBUFFER}
  TBSTYLE_EX_DOUBLEBUFFER = $00000080; // Double Buffer the toolbar

type
  // Custom Draw Structure
  {$EXTERNALSYM _NMTBCUSTOMDRAW}
  _NMTBCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    hbrMonoDither: HBRUSH;
    hbrLines: HBRUSH;                // For drawing lines on buttons
    hpenLines: HPEN;                 // For drawing lines on buttons
    clrText: COLORREF;               // Color of text
    clrMark: COLORREF;               // Color of text bk when marked. (only if TBSTATE_MARKED)
    clrTextHighlight: COLORREF;      // Color of text when highlighted
    clrBtnFace: COLORREF;            // Background of the button
    clrBtnHighlight: COLORREF;       // 3D highlight
    clrHighlightHotTrack: COLORREF;  // In conjunction with fHighlightHotTrack
                                     // will cause button to highlight like a menu
    rcText: TRect;                   // Rect for text
    nStringBkMode: Integer;
    nHLStringBkMode: Integer;
  end;
  PNMTBCustomDraw = ^TNMTBCustomDraw;
  TNMTBCustomDraw = _NMTBCUSTOMDRAW;

const
  // Toolbar custom draw return flags
  {$EXTERNALSYM TBCDRF_NOEDGES}
  TBCDRF_NOEDGES              = $00010000;  // Don't draw button edges
  {$EXTERNALSYM TBCDRF_HILITEHOTTRACK}
  TBCDRF_HILITEHOTTRACK       = $00020000;  // Use color of the button bk when hottracked
  {$EXTERNALSYM TBCDRF_NOOFFSET}
  TBCDRF_NOOFFSET             = $00040000;  // Don't offset button if pressed
  {$EXTERNALSYM TBCDRF_NOMARK}
  TBCDRF_NOMARK               = $00080000;  // Don't draw default highlight of image/text for TBSTATE_MARKED
  {$EXTERNALSYM TBCDRF_NOETCHEDEFFECT}
  TBCDRF_NOETCHEDEFFECT       = $00100000;  // Don't draw etched effect for disabled items

  {$EXTERNALSYM TB_ENABLEBUTTON}
  TB_ENABLEBUTTON         = WM_USER + 1;
  {$EXTERNALSYM TB_CHECKBUTTON}
  TB_CHECKBUTTON          = WM_USER + 2;
  {$EXTERNALSYM TB_PRESSBUTTON}
  TB_PRESSBUTTON          = WM_USER + 3;
  {$EXTERNALSYM TB_HIDEBUTTON}
  TB_HIDEBUTTON           = WM_USER + 4;
  {$EXTERNALSYM TB_INDETERMINATE}
  TB_INDETERMINATE        = WM_USER + 5;
  {$EXTERNALSYM TB_MARKBUTTON}
  TB_MARKBUTTON           = WM_USER + 6;
  {$EXTERNALSYM TB_ISBUTTONENABLED}
  TB_ISBUTTONENABLED      = WM_USER + 9;
  {$EXTERNALSYM TB_ISBUTTONCHECKED}
  TB_ISBUTTONCHECKED      = WM_USER + 10;
  {$EXTERNALSYM TB_ISBUTTONPRESSED}
  TB_ISBUTTONPRESSED      = WM_USER + 11;
  {$EXTERNALSYM TB_ISBUTTONHIDDEN}
  TB_ISBUTTONHIDDEN       = WM_USER + 12;
  {$EXTERNALSYM TB_ISBUTTONINDETERMINATE}
  TB_ISBUTTONINDETERMINATE = WM_USER + 13;
  {$EXTERNALSYM TB_ISBUTTONHIGHLIGHTED}
  TB_ISBUTTONHIGHLIGHTED   = WM_USER + 14;
  {$EXTERNALSYM TB_SETSTATE}
  TB_SETSTATE             = WM_USER + 17;
  {$EXTERNALSYM TB_GETSTATE}
  TB_GETSTATE             = WM_USER + 18;
  {$EXTERNALSYM TB_ADDBITMAP}
  TB_ADDBITMAP            = WM_USER + 19;

type
  PTBAddBitmap = ^TTBAddBitmap;
  {$EXTERNALSYM tagTBADDBITMAP}
  tagTBADDBITMAP = packed record
    hInst: THandle;
    nID: UINT;
  end;
  TTBAddBitmap = tagTBADDBITMAP;
  {$EXTERNALSYM TBADDBITMAP}
  TBADDBITMAP = tagTBADDBITMAP;

const
  {$EXTERNALSYM HINST_COMMCTRL}
  HINST_COMMCTRL = THandle(-1);

  {$EXTERNALSYM IDB_STD_SMALL_COLOR}
  IDB_STD_SMALL_COLOR     = 0;
  {$EXTERNALSYM IDB_STD_LARGE_COLOR}
  IDB_STD_LARGE_COLOR     = 1;
  {$EXTERNALSYM IDB_VIEW_SMALL_COLOR}
  IDB_VIEW_SMALL_COLOR    = 4;
  {$EXTERNALSYM IDB_VIEW_LARGE_COLOR}
  IDB_VIEW_LARGE_COLOR    = 5;
  {$EXTERNALSYM IDB_HIST_SMALL_COLOR}
  IDB_HIST_SMALL_COLOR    = 8;
  {$EXTERNALSYM IDB_HIST_LARGE_COLOR}
  IDB_HIST_LARGE_COLOR    = 9;

{ icon indexes for standard bitmap }
  {$EXTERNALSYM STD_CUT}
  STD_CUT                 = 0;
  {$EXTERNALSYM STD_COPY}
  STD_COPY                = 1;
  {$EXTERNALSYM STD_PASTE}
  STD_PASTE               = 2;
  {$EXTERNALSYM STD_UNDO}
  STD_UNDO                = 3;
  {$EXTERNALSYM STD_REDOW}
  STD_REDOW               = 4;
  {$EXTERNALSYM STD_DELETE}
  STD_DELETE              = 5;
  {$EXTERNALSYM STD_FILENEW}
  STD_FILENEW             = 6;
  {$EXTERNALSYM STD_FILEOPEN}
  STD_FILEOPEN            = 7;
  {$EXTERNALSYM STD_FILESAVE}
  STD_FILESAVE            = 8;
  {$EXTERNALSYM STD_PRINTPRE}
  STD_PRINTPRE            = 9;
  {$EXTERNALSYM STD_PROPERTIES}
  STD_PROPERTIES          = 10;
  {$EXTERNALSYM STD_HELP}
  STD_HELP                = 11;
  {$EXTERNALSYM STD_FIND}
  STD_FIND                = 12;
  {$EXTERNALSYM STD_REPLACE}
  STD_REPLACE             = 13;
  {$EXTERNALSYM STD_PRINT}
  STD_PRINT               = 14;

{ icon indexes for standard view bitmap }

  {$EXTERNALSYM VIEW_LARGEICONS}
  VIEW_LARGEICONS         = 0;
  {$EXTERNALSYM VIEW_SMALLICONS}
  VIEW_SMALLICONS         = 1;
  {$EXTERNALSYM VIEW_LIST}
  VIEW_LIST               = 2;
  {$EXTERNALSYM VIEW_DETAILS}
  VIEW_DETAILS            = 3;
  {$EXTERNALSYM VIEW_SORTNAME}
  VIEW_SORTNAME           = 4;
  {$EXTERNALSYM VIEW_SORTSIZE}
  VIEW_SORTSIZE           = 5;
  {$EXTERNALSYM VIEW_SORTDATE}
  VIEW_SORTDATE           = 6;
  {$EXTERNALSYM VIEW_SORTTYPE}
  VIEW_SORTTYPE           = 7;
  {$EXTERNALSYM VIEW_PARENTFOLDER}
  VIEW_PARENTFOLDER       = 8;
  {$EXTERNALSYM VIEW_NETCONNECT}
  VIEW_NETCONNECT         = 9;
  {$EXTERNALSYM VIEW_NETDISCONNECT}
  VIEW_NETDISCONNECT      = 10;
  {$EXTERNALSYM VIEW_NEWFOLDER}
  VIEW_NEWFOLDER          = 11;
  {$EXTERNALSYM VIEW_VIEWMENU}
  VIEW_VIEWMENU           = 12;

{ icon indexes for history bitmap }

  {$EXTERNALSYM HIST_BACK}
  HIST_BACK               = 0;
  {$EXTERNALSYM HIST_FORWARD}
  HIST_FORWARD            = 1;
  {$EXTERNALSYM HIST_FAVORITES}
  HIST_FAVORITES          = 2;
  {$EXTERNALSYM HIST_ADDTOFAVORITES}
  HIST_ADDTOFAVORITES     = 3;
  {$EXTERNALSYM HIST_VIEWTREE}
  HIST_VIEWTREE           = 4;

  {$EXTERNALSYM TB_ADDBUTTONSA}
  TB_ADDBUTTONSA          = WM_USER + 20;
  {$EXTERNALSYM TB_INSERTBUTTONA}
  TB_INSERTBUTTONA        = WM_USER + 21;
  {$EXTERNALSYM TB_DELETEBUTTON}
  TB_DELETEBUTTON         = WM_USER + 22;
  {$EXTERNALSYM TB_GETBUTTON}
  TB_GETBUTTON            = WM_USER + 23;
  {$EXTERNALSYM TB_BUTTONCOUNT}
  TB_BUTTONCOUNT          = WM_USER + 24;
  {$EXTERNALSYM TB_COMMANDTOINDEX}
  TB_COMMANDTOINDEX       = WM_USER + 25;

type
  PTBSaveParamsA = ^TTBSaveParamsA;
  PTBSaveParamsW = ^TTBSaveParamsW;
  PTBSaveParams = PTBSaveParamsA;
  {$EXTERNALSYM tagTBSAVEPARAMSA}
  tagTBSAVEPARAMSA = packed record
    hkr: THandle;
    pszSubKey: PAnsiChar;
    pszValueName: PAnsiChar;
  end;
  {$EXTERNALSYM tagTBSAVEPARAMSW}
  tagTBSAVEPARAMSW = packed record
    hkr: THandle;
    pszSubKey: PWideChar;
    pszValueName: PWideChar;
  end;
  {$EXTERNALSYM tagTBSAVEPARAMS}
  tagTBSAVEPARAMS = tagTBSAVEPARAMSA;
  TTBSaveParamsA = tagTBSAVEPARAMSA;
  TTBSaveParamsW = tagTBSAVEPARAMSW;
  TTBSaveParams = TTBSaveParamsA;
  {$EXTERNALSYM TBSAVEPARAMSA}
  TBSAVEPARAMSA = tagTBSAVEPARAMSA;
  {$EXTERNALSYM TBSAVEPARAMSW}
  TBSAVEPARAMSW = tagTBSAVEPARAMSW;
  {$EXTERNALSYM TBSAVEPARAMS}
  TBSAVEPARAMS = TBSAVEPARAMSA;

const
  {$EXTERNALSYM TB_SAVERESTOREA}
  TB_SAVERESTOREA          = WM_USER + 26;
  {$EXTERNALSYM TB_ADDSTRINGA}
  TB_ADDSTRINGA            = WM_USER + 28;
  {$EXTERNALSYM TB_GETBUTTONTEXTA}
  TB_GETBUTTONTEXTA        = WM_USER + 45;
  {$EXTERNALSYM TBN_GETBUTTONINFOA}
  TBN_GETBUTTONINFOA       = TBN_FIRST-0;

  {$EXTERNALSYM TB_SAVERESTOREW}
  TB_SAVERESTOREW          = WM_USER + 76;
  {$EXTERNALSYM TB_ADDSTRINGW}
  TB_ADDSTRINGW            = WM_USER + 77;
  {$EXTERNALSYM TB_GETBUTTONTEXTW}
  TB_GETBUTTONTEXTW        = WM_USER + 75;
  {$EXTERNALSYM TBN_GETBUTTONINFOW}
  TBN_GETBUTTONINFOW       = TBN_FIRST-20;











  {$EXTERNALSYM TB_SAVERESTORE}
  TB_SAVERESTORE          = TB_SAVERESTOREA;
  {$EXTERNALSYM TB_ADDSTRING}
  TB_ADDSTRING            = TB_ADDSTRINGA;
  {$EXTERNALSYM TB_GETBUTTONTEXT}
  TB_GETBUTTONTEXT        = TB_GETBUTTONTEXTA;
  {$EXTERNALSYM TBN_GETBUTTONINFO}
  TBN_GETBUTTONINFO       = TBN_GETBUTTONINFOA;


  {$EXTERNALSYM TB_CUSTOMIZE}
  TB_CUSTOMIZE            = WM_USER + 27;
  {$EXTERNALSYM TB_GETITEMRECT}
  TB_GETITEMRECT          = WM_USER + 29;
  {$EXTERNALSYM TB_BUTTONSTRUCTSIZE}
  TB_BUTTONSTRUCTSIZE     = WM_USER + 30;
  {$EXTERNALSYM TB_SETBUTTONSIZE}
  TB_SETBUTTONSIZE        = WM_USER + 31;
  {$EXTERNALSYM TB_SETBITMAPSIZE}
  TB_SETBITMAPSIZE        = WM_USER + 32;
  {$EXTERNALSYM TB_AUTOSIZE}
  TB_AUTOSIZE             = WM_USER + 33;
  {$EXTERNALSYM TB_GETTOOLTIPS}
  TB_GETTOOLTIPS          = WM_USER + 35;
  {$EXTERNALSYM TB_SETTOOLTIPS}
  TB_SETTOOLTIPS          = WM_USER + 36;
  {$EXTERNALSYM TB_SETPARENT}
  TB_SETPARENT            = WM_USER + 37;
  {$EXTERNALSYM TB_SETROWS}
  TB_SETROWS              = WM_USER + 39;
  {$EXTERNALSYM TB_GETROWS}
  TB_GETROWS              = WM_USER + 40;
  {$EXTERNALSYM TB_SETCMDID}
  TB_SETCMDID             = WM_USER + 42;
  {$EXTERNALSYM TB_CHANGEBITMAP}
  TB_CHANGEBITMAP         = WM_USER + 43;
  {$EXTERNALSYM TB_GETBITMAP}
  TB_GETBITMAP            = WM_USER + 44;
  {$EXTERNALSYM TB_REPLACEBITMAP}
  TB_REPLACEBITMAP        = WM_USER + 46;
  {$EXTERNALSYM TB_SETINDENT}
  TB_SETINDENT            = WM_USER + 47;
  {$EXTERNALSYM TB_SETIMAGELIST}
  TB_SETIMAGELIST         = WM_USER + 48;
  {$EXTERNALSYM TB_GETIMAGELIST}
  TB_GETIMAGELIST         = WM_USER + 49;
  {$EXTERNALSYM TB_LOADIMAGES}
  TB_LOADIMAGES           = WM_USER + 50;
  {$EXTERNALSYM TB_GETRECT}
  TB_GETRECT              = WM_USER + 51; { wParam is the Cmd instead of index }
  {$EXTERNALSYM TB_SETHOTIMAGELIST}
  TB_SETHOTIMAGELIST      = WM_USER + 52;
  {$EXTERNALSYM TB_GETHOTIMAGELIST}
  TB_GETHOTIMAGELIST      = WM_USER + 53;
  {$EXTERNALSYM TB_SETDISABLEDIMAGELIST}
  TB_SETDISABLEDIMAGELIST = WM_USER + 54;
  {$EXTERNALSYM TB_GETDISABLEDIMAGELIST}
  TB_GETDISABLEDIMAGELIST = WM_USER + 55;
  {$EXTERNALSYM TB_SETSTYLE}
  TB_SETSTYLE             = WM_USER + 56;
  {$EXTERNALSYM TB_GETSTYLE}
  TB_GETSTYLE             = WM_USER + 57;
  {$EXTERNALSYM TB_GETBUTTONSIZE}
  TB_GETBUTTONSIZE        = WM_USER + 58;
  {$EXTERNALSYM TB_SETBUTTONWIDTH}
  TB_SETBUTTONWIDTH       = WM_USER + 59;
  {$EXTERNALSYM TB_SETMAXTEXTROWS}
  TB_SETMAXTEXTROWS       = WM_USER + 60;
  {$EXTERNALSYM TB_GETTEXTROWS}
  TB_GETTEXTROWS          = WM_USER + 61;

  {$EXTERNALSYM TB_GETOBJECT}
  TB_GETOBJECT            = WM_USER + 62;  // wParam == IID, lParam void **ppv
  {$EXTERNALSYM TB_GETHOTITEM}
  TB_GETHOTITEM           = WM_USER + 71;
  {$EXTERNALSYM TB_SETHOTITEM}
  TB_SETHOTITEM           = WM_USER + 72;  // wParam == iHotItem
  {$EXTERNALSYM TB_SETANCHORHIGHLIGHT}
  TB_SETANCHORHIGHLIGHT   = WM_USER + 73;  // wParam == TRUE/FALSE
  {$EXTERNALSYM TB_GETANCHORHIGHLIGHT}
  TB_GETANCHORHIGHLIGHT   = WM_USER + 74;
  {$EXTERNALSYM TB_MAPACCELERATORA}
  TB_MAPACCELERATORA      = WM_USER + 78;  // wParam == ch, lParam int * pidBtn

type
  {$EXTERNALSYM TBINSERTMARK}
  TBINSERTMARK = packed record
    iButton: Integer;
    dwFlags: DWORD;
  end;
  PTBInsertMark = ^TTBInsertMark;
  TTBInsertMark = TBINSERTMARK;

const
  {$EXTERNALSYM TBIMHT_AFTER}
  TBIMHT_AFTER      = $00000001; // TRUE = insert After iButton, otherwise before
  {$EXTERNALSYM TBIMHT_BACKGROUND}
  TBIMHT_BACKGROUND = $00000002; // TRUE iff missed buttons completely

  {$EXTERNALSYM TB_GETINSERTMARK}
  TB_GETINSERTMARK        = WM_USER + 79;  // lParam == LPTBINSERTMARK
  {$EXTERNALSYM TB_SETINSERTMARK}
  TB_SETINSERTMARK        = WM_USER + 80;  // lParam == LPTBINSERTMARK
  {$EXTERNALSYM TB_INSERTMARKHITTEST}
  TB_INSERTMARKHITTEST    = WM_USER + 81;  // wParam == LPPOINT lParam == LPTBINSERTMARK
  {$EXTERNALSYM TB_MOVEBUTTON}
  TB_MOVEBUTTON           = WM_USER + 82;
  {$EXTERNALSYM TB_GETMAXSIZE}
  TB_GETMAXSIZE           = WM_USER + 83;  // lParam == LPSIZE
  {$EXTERNALSYM TB_SETEXTENDEDSTYLE}
  TB_SETEXTENDEDSTYLE     = WM_USER + 84;  // For TBSTYLE_EX_*
  {$EXTERNALSYM TB_GETEXTENDEDSTYLE}
  TB_GETEXTENDEDSTYLE     = WM_USER + 85;  // For TBSTYLE_EX_*
  {$EXTERNALSYM TB_GETPADDING}
  TB_GETPADDING           = WM_USER + 86;
  {$EXTERNALSYM TB_SETPADDING}
  TB_SETPADDING           = WM_USER + 87;
  {$EXTERNALSYM TB_SETINSERTMARKCOLOR}
  TB_SETINSERTMARKCOLOR   = WM_USER + 88;
  {$EXTERNALSYM TB_GETINSERTMARKCOLOR}
  TB_GETINSERTMARKCOLOR   = WM_USER + 89;

  {$EXTERNALSYM TB_SETCOLORSCHEME}
  TB_SETCOLORSCHEME       = CCM_SETCOLORSCHEME;  // lParam is color scheme
  {$EXTERNALSYM TB_GETCOLORSCHEME}
  TB_GETCOLORSCHEME       = CCM_GETCOLORSCHEME;	// fills in COLORSCHEME pointed to by lParam

  {$EXTERNALSYM TB_SETUNICODEFORMAT}
  TB_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;
  {$EXTERNALSYM TB_GETUNICODEFORMAT}
  TB_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;

  {$EXTERNALSYM TB_MAPACCELERATORW}
  TB_MAPACCELERATORW      = WM_USER + 90;  // wParam == ch, lParam int * pidBtn




  {$EXTERNALSYM TB_MAPACCELERATOR}
  TB_MAPACCELERATOR       = TB_MAPACCELERATORA;


type
  {$EXTERNALSYM TBREPLACEBITMAP}
  TBREPLACEBITMAP = packed record
    hInstOld: THandle;
    nIDOld: Cardinal;
    hInstNew: THandle;
    nIDNew: Cardinal;
    nButtons: Integer;
  end;
  PTBReplaceBitmap = ^TTBReplaceBitmap;
  TTBReplaceBitmap = TBREPLACEBITMAP;

const
  {$EXTERNALSYM TBBF_LARGE}
  TBBF_LARGE              = $0001;

  {$EXTERNALSYM TB_GETBITMAPFLAGS}
  TB_GETBITMAPFLAGS       = WM_USER + 41;

  {$EXTERNALSYM TBIF_IMAGE}
  TBIF_IMAGE              = $00000001;
  {$EXTERNALSYM TBIF_TEXT}
  TBIF_TEXT               = $00000002;
  {$EXTERNALSYM TBIF_STATE}
  TBIF_STATE              = $00000004;
  {$EXTERNALSYM TBIF_STYLE}
  TBIF_STYLE              = $00000008;
  {$EXTERNALSYM TBIF_LPARAM}
  TBIF_LPARAM             = $00000010;
  {$EXTERNALSYM TBIF_COMMAND}
  TBIF_COMMAND            = $00000020;
  {$EXTERNALSYM TBIF_SIZE}
  TBIF_SIZE               = $00000040;
  {$EXTERNALSYM TBIF_BYINDEX}
  TBIF_BYINDEX            = $80000000;

type
  {$EXTERNALSYM TBBUTTONINFOA}
  TBBUTTONINFOA = packed record
    cbSize: UINT;
    dwMask: DWORD;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: DWORD;
    pszText: PAnsiChar;
    cchText: Integer;
  end;
  {$EXTERNALSYM TBBUTTONINFOW}
  TBBUTTONINFOW = packed record
    cbSize: UINT;
    dwMask: DWORD;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: DWORD;
    pszText: PWideChar;
    cchText: Integer;
  end;
  {$EXTERNALSYM TBBUTTONINFO}
  TBBUTTONINFO = TBBUTTONINFOA;
  PTBButtonInfoA = ^TTBButtonInfoA;
  PTBButtonInfoW = ^TTBButtonInfoW;
  PTBButtonInfo = PTBButtonInfoA;
  TTBButtonInfoA = TBBUTTONINFOA;
  TTBButtonInfoW = TBBUTTONINFOW;
  TTBButtonInfo = TTBButtonInfoA;

const
  // BUTTONINFO APIs do NOT support the string pool.
  {$EXTERNALSYM TB_GETBUTTONINFOW}
  TB_GETBUTTONINFOW        = WM_USER + 63;
  {$EXTERNALSYM TB_SETBUTTONINFOW}
  TB_SETBUTTONINFOW        = WM_USER + 64;
  {$EXTERNALSYM TB_GETBUTTONINFOA}
  TB_GETBUTTONINFOA        = WM_USER + 65;
  {$EXTERNALSYM TB_SETBUTTONINFOA}
  TB_SETBUTTONINFOA        = WM_USER + 66;






  {$EXTERNALSYM TB_GETBUTTONINFO}
  TB_GETBUTTONINFO         = TB_GETBUTTONINFOA;
  {$EXTERNALSYM TB_SETBUTTONINFO}
  TB_SETBUTTONINFO         = TB_SETBUTTONINFOA;


  {$EXTERNALSYM TB_INSERTBUTTONW}
  TB_INSERTBUTTONW        = WM_USER + 67;
  {$EXTERNALSYM TB_ADDBUTTONSW}
  TB_ADDBUTTONSW          = WM_USER + 68;

  {$EXTERNALSYM TB_HITTEST}
  TB_HITTEST              = WM_USER + 69;

  // New post Win95/NT4 for InsertButton and AddButton.  if iString member
  // is a pointer to a string, it will be handled as a string like listview
  // = although LPSTR_TEXTCALLBACK is not supported;.






  {$EXTERNALSYM TB_INSERTBUTTON}
  TB_INSERTBUTTON         = TB_INSERTBUTTONA;
  {$EXTERNALSYM TB_ADDBUTTONS}
  TB_ADDBUTTONS           = TB_ADDBUTTONSA;


  {$EXTERNALSYM TB_SETDRAWTEXTFLAGS}
  TB_SETDRAWTEXTFLAGS     = WM_USER + 70;  // wParam == mask lParam == bit values

  {$EXTERNALSYM TB_GETSTRING}
  TB_GETSTRING            = WM_USER + 92;

  {$EXTERNALSYM TBN_BEGINDRAG}
  TBN_BEGINDRAG           = TBN_FIRST-1;
  {$EXTERNALSYM TBN_ENDDRAG}
  TBN_ENDDRAG             = TBN_FIRST-2;
  {$EXTERNALSYM TBN_BEGINADJUST}
  TBN_BEGINADJUST         = TBN_FIRST-3;
  {$EXTERNALSYM TBN_ENDADJUST}
  TBN_ENDADJUST           = TBN_FIRST-4;
  {$EXTERNALSYM TBN_RESET}
  TBN_RESET               = TBN_FIRST-5;
  {$EXTERNALSYM TBN_QUERYINSERT}
  TBN_QUERYINSERT         = TBN_FIRST-6;
  {$EXTERNALSYM TBN_QUERYDELETE}
  TBN_QUERYDELETE         = TBN_FIRST-7;
  {$EXTERNALSYM TBN_TOOLBARCHANGE}
  TBN_TOOLBARCHANGE       = TBN_FIRST-8;
  {$EXTERNALSYM TBN_CUSTHELP}
  TBN_CUSTHELP            = TBN_FIRST-9;
  {$EXTERNALSYM TBN_DROPDOWN}
  TBN_DROPDOWN            = TBN_FIRST-10;
  {$EXTERNALSYM TBN_CLOSEUP}
  TBN_CLOSEUP             = TBN_FIRST-11;
  {$EXTERNALSYM TBN_GETOBJECT}
  TBN_GETOBJECT           = TBN_FIRST-12;
  {$EXTERNALSYM TBN_RESTORE}
  TBN_RESTORE             = TBN_FIRST-21;
  {$EXTERNALSYM TBN_SAVE}
  TBN_SAVE                = TBN_FIRST-22;


type
  // Structure for TBN_HOTITEMCHANGE notification
  {$EXTERNALSYM tagNMTBHOTITEM}
  tagNMTBHOTITEM = packed record
    hdr: TNMHdr;
    idOld: Integer;
    idNew: Integer;
    dwFlags: DWORD;           // HICF_*
  end;
  PNMTBHotItem = ^TNMTBHotItem;
  TNMTBHotItem = tagNMTBHOTITEM;

const
  // Hot item change flags
  {$EXTERNALSYM HICF_OTHER}
  HICF_OTHER          = $00000000;
  {$EXTERNALSYM HICF_MOUSE}
  HICF_MOUSE          = $00000001;          // Triggered by mouse
  {$EXTERNALSYM HICF_ARROWKEYS}
  HICF_ARROWKEYS      = $00000002;          // Triggered by arrow keys
  {$EXTERNALSYM HICF_ACCELERATOR}
  HICF_ACCELERATOR    = $00000004;          // Triggered by accelerator
  {$EXTERNALSYM HICF_DUPACCEL}
  HICF_DUPACCEL       = $00000008;          // This accelerator is not unique
  {$EXTERNALSYM HICF_ENTERING}
  HICF_ENTERING       = $00000010;          // idOld is invalid
  {$EXTERNALSYM HICF_LEAVING}
  HICF_LEAVING        = $00000020;          // idNew is invalid
  {$EXTERNALSYM HICF_RESELECT}
  HICF_RESELECT       = $00000040;          // hot item reselected

  {$EXTERNALSYM TBN_HOTITEMCHANGE}
  TBN_HOTITEMCHANGE       = TBN_FIRST - 13;
  {$EXTERNALSYM TBN_DRAGOUT}
  TBN_DRAGOUT             = TBN_FIRST - 14; // this is sent when the user clicks down on a button then drags off the button
  {$EXTERNALSYM TBN_DELETINGBUTTON}
  TBN_DELETINGBUTTON      = TBN_FIRST - 15; // uses TBNOTIFY
  {$EXTERNALSYM TBN_GETDISPINFOA}
  TBN_GETDISPINFOA        = TBN_FIRST - 16; // This is sent when the  toolbar needs  some display information
  {$EXTERNALSYM TBN_GETDISPINFOW}
  TBN_GETDISPINFOW        = TBN_FIRST - 17; // This is sent when the  toolbar needs  some display information
  {$EXTERNALSYM TBN_GETINFOTIPA}
  TBN_GETINFOTIPA         = TBN_FIRST - 18;
  {$EXTERNALSYM TBN_GETINFOTIPW}
  TBN_GETINFOTIPW         = TBN_FIRST - 19;

type
  {$EXTERNALSYM tagNMTBGETINFOTIPA}
  tagNMTBGETINFOTIPA = packed record
    hdr: TNMHdr;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iItem: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagNMTBGETINFOTIPW}
  tagNMTBGETINFOTIPW = packed record
    hdr: TNMHdr;
    pszText: PWideChar;
    cchTextMax: Integer;
    iItem: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagNMTBGETINFOTIP}
  tagNMTBGETINFOTIP = tagNMTBGETINFOTIPA;
  PNMTBGetInfoTipA = ^TNMTBGetInfoTipA;
  PNMTBGetInfoTipW = ^TNMTBGetInfoTipW;
  PNMTBGetInfoTip = PNMTBGetInfoTipA;
  TNMTBGetInfoTipA = tagNMTBGETINFOTIPA;
  TNMTBGetInfoTipW = tagNMTBGETINFOTIPW;
  TNMTBGetInfoTip = TNMTBGetInfoTipA;

const
  {$EXTERNALSYM TBNF_IMAGE}
  TBNF_IMAGE              = $00000001;
  {$EXTERNALSYM TBNF_TEXT}
  TBNF_TEXT               = $00000002;
  {$EXTERNALSYM TBNF_DI_SETITEM}
  TBNF_DI_SETITEM         = $10000000;

type
  {$EXTERNALSYM NMTBDISPINFOA}
  NMTBDISPINFOA = packed record
    hdr: TNMHdr;
    dwMask: DWORD;      // [in] Specifies the values requested .[out] Client ask the data to be set for future use
    idCommand: Integer; // [in] id of button we're requesting info for
    lParam: DWORD;      // [in] lParam of button
    iImage: Integer;    // [out] image index
    pszText: PAnsiChar;    // [out] new text for item
    cchText: Integer;   // [in] size of buffer pointed to by pszText
  end;
  {$EXTERNALSYM NMTBDISPINFOW}
  NMTBDISPINFOW = packed record
    hdr: TNMHdr;
    dwMask: DWORD;      // [in] Specifies the values requested .[out] Client ask the data to be set for future use
    idCommand: Integer; // [in] id of button we're requesting info for
    lParam: DWORD;      // [in] lParam of button
    iImage: Integer;    // [out] image index
    pszText: PWideChar;    // [out] new text for item
    cchText: Integer;   // [in] size of buffer pointed to by pszText
  end;
  {$EXTERNALSYM NMTBDISPINFO}
  NMTBDISPINFO = NMTBDISPINFOA;
  PNMTBDispInfoA = ^TNMTBDispInfoA;
  PNMTBDispInfoW = ^TNMTBDispInfoW;
  PNMTBDispInfo = PNMTBDispInfoA;
  TNMTBDispInfoA = NMTBDISPINFOA;
  TNMTBDispInfoW = NMTBDISPINFOW;
  TNMTBDispInfo = TNMTBDispInfoA;

const
  // Return codes for TBN_DROPDOWN
  {$EXTERNALSYM TBDDRET_DEFAULT}
  TBDDRET_DEFAULT         = 0;
  {$EXTERNALSYM TBDDRET_NODEFAULT}
  TBDDRET_NODEFAULT       = 1;
  {$EXTERNALSYM TBDDRET_TREATPRESSED}
  TBDDRET_TREATPRESSED    = 2;       // Treat as a standard press button

type
  {$EXTERNALSYM tagNMTOOLBARA}
  tagNMTOOLBARA = packed record
    hdr: TNMHdr;
    iItem: Integer;
    tbButton: TTBButton;
    cchText: Integer;
    pszText: PAnsiChar;
  end;
  {$EXTERNALSYM tagNMTOOLBARW}
  tagNMTOOLBARW = packed record
    hdr: TNMHdr;
    iItem: Integer;
    tbButton: TTBButton;
    cchText: Integer;
    pszText: PWideChar;
  end;
  {$EXTERNALSYM tagNMTOOLBAR}
  tagNMTOOLBAR = tagNMTOOLBARA;
  PNMToolBarA = ^TNMToolBarA;
  PNMToolBarW = ^TNMToolBarW;
  PNMToolBar = PNMToolBarA;
  TNMToolBarA = tagNMTOOLBARA;
  TNMToolBarW = tagNMTOOLBARW;
  TNMToolBar = TNMToolBarA;

{ ====== REBAR CONTROL =================== }

const
  {$EXTERNALSYM REBARCLASSNAME}
  REBARCLASSNAME = 'ReBarWindow32';

type
  {$EXTERNALSYM tagREBARINFO}
  tagREBARINFO = packed record
    cbSize: UINT;
    fMask: UINT;
    himl: HIMAGELIST;
  end;
  PReBarInfo = ^TReBarInfo;
  TReBarInfo = tagREBARINFO;

const
  {$EXTERNALSYM RBIM_IMAGELIST}
  RBIM_IMAGELIST    = $00000001;

  {$EXTERNALSYM RBS_TOOLTIPS}
  RBS_TOOLTIPS      = $00000100;
  {$EXTERNALSYM RBS_VARHEIGHT}
  RBS_VARHEIGHT     = $00000200;
  {$EXTERNALSYM RBS_BANDBORDERS}
  RBS_BANDBORDERS   = $00000400;
  {$EXTERNALSYM RBS_FIXEDORDER}
  RBS_FIXEDORDER    = $00000800;

  {$EXTERNALSYM RBS_REGISTERDROP}
  RBS_REGISTERDROP  = $00001000;
  {$EXTERNALSYM RBS_AUTOSIZE}
  RBS_AUTOSIZE      = $00002000;
  {$EXTERNALSYM RBS_VERTICALGRIPPER}
  RBS_VERTICALGRIPPER = $00004000;  // this always has the vertical gripper (default for horizontal mode)
  {$EXTERNALSYM RBS_DBLCLKTOGGLE}
  RBS_DBLCLKTOGGLE  = $00008000;

  {$EXTERNALSYM RBBS_BREAK}
  RBBS_BREAK        = $00000001;  // break to new line
  {$EXTERNALSYM RBBS_FIXEDSIZE}
  RBBS_FIXEDSIZE    = $00000002;  // band can't be sized
  {$EXTERNALSYM RBBS_CHILDEDGE}
  RBBS_CHILDEDGE    = $00000004;  // edge around top and bottom of child window
  {$EXTERNALSYM RBBS_HIDDEN}
  RBBS_HIDDEN       = $00000008;  // don't show
  {$EXTERNALSYM RBBS_NOVERT}
  RBBS_NOVERT       = $00000010;  // don't show when vertical
  {$EXTERNALSYM RBBS_FIXEDBMP}
  RBBS_FIXEDBMP     = $00000020;  // bitmap doesn't move during band resize
  {$EXTERNALSYM RBBS_VARIABLEHEIGHT}
  RBBS_VARIABLEHEIGHT = $00000040;  // allow autosizing of this child vertically
  {$EXTERNALSYM RBBS_GRIPPERALWAYS}
  RBBS_GRIPPERALWAYS  = $00000080;  // always show the gripper
  {$EXTERNALSYM RBBS_NOGRIPPER}
  RBBS_NOGRIPPER      = $00000100;  // never show the gripper

  {$EXTERNALSYM RBBIM_STYLE}
  RBBIM_STYLE       = $00000001;
  {$EXTERNALSYM RBBIM_COLORS}
  RBBIM_COLORS      = $00000002;
  {$EXTERNALSYM RBBIM_TEXT}
  RBBIM_TEXT        = $00000004;
  {$EXTERNALSYM RBBIM_IMAGE}
  RBBIM_IMAGE       = $00000008;
  {$EXTERNALSYM RBBIM_CHILD}
  RBBIM_CHILD       = $00000010;
  {$EXTERNALSYM RBBIM_CHILDSIZE}
  RBBIM_CHILDSIZE   = $00000020;
  {$EXTERNALSYM RBBIM_SIZE}
  RBBIM_SIZE        = $00000040;
  {$EXTERNALSYM RBBIM_BACKGROUND}
  RBBIM_BACKGROUND  = $00000080;
  {$EXTERNALSYM RBBIM_ID}
  RBBIM_ID          = $00000100;
  {$EXTERNALSYM RBBIM_IDEALSIZE}
  RBBIM_IDEALSIZE     = $00000200;
  {$EXTERNALSYM RBBIM_LPARAM}
  RBBIM_LPARAM        = $00000400;
  {$EXTERNALSYM RBBIM_HEADERSIZE}
  RBBIM_HEADERSIZE    = $00000800;  // control the size of the header

type
  {$EXTERNALSYM tagREBARBANDINFOA}
  tagREBARBANDINFOA = packed record
    cbSize: UINT;
    fMask: UINT;
    fStyle: UINT;
    clrFore: TColorRef;
    clrBack: TColorRef;
    lpText: PAnsiChar;
    cch: UINT;
    iImage: Integer;
    hwndChild: HWnd;
    cxMinChild: UINT;
    cyMinChild: UINT;
    cx: UINT;
    hbmBack: HBitmap;
    wID: UINT;
    cyChild: UINT;
    cyMaxChild: UINT;
    cyIntegral: UINT;
    cxIdeal: UINT;
    lParam: LPARAM;
    cxHeader: UINT;
  end;
  {$EXTERNALSYM tagREBARBANDINFOW}
  tagREBARBANDINFOW = packed record
    cbSize: UINT;
    fMask: UINT;
    fStyle: UINT;
    clrFore: TColorRef;
    clrBack: TColorRef;
    lpText: PWideChar;
    cch: UINT;
    iImage: Integer;
    hwndChild: HWnd;
    cxMinChild: UINT;
    cyMinChild: UINT;
    cx: UINT;
    hbmBack: HBitmap;
    wID: UINT;
    cyChild: UINT;
    cyMaxChild: UINT;
    cyIntegral: UINT;
    cxIdeal: UINT;
    lParam: LPARAM;
    cxHeader: UINT;
  end;
  {$EXTERNALSYM tagREBARBANDINFO}
  tagREBARBANDINFO = tagREBARBANDINFOA;
  PReBarBandInfoA = ^TReBarBandInfoA;
  PReBarBandInfoW = ^TReBarBandInfoW;
  PReBarBandInfo = PReBarBandInfoA;
  TReBarBandInfoA = tagREBARBANDINFOA;
  TReBarBandInfoW = tagREBARBANDINFOW;
  TReBarBandInfo = TReBarBandInfoA;

const
  {$EXTERNALSYM RB_INSERTBANDA}
  RB_INSERTBANDA     = WM_USER +  1;
  {$EXTERNALSYM RB_DELETEBAND}
  RB_DELETEBAND      = WM_USER +  2;
  {$EXTERNALSYM RB_GETBARINFO}
  RB_GETBARINFO      = WM_USER +  3;
  {$EXTERNALSYM RB_SETBARINFO}
  RB_SETBARINFO      = WM_USER +  4;
  RB_GETBANDINFO_PRE_IE4     = WM_USER +  5;
  {$EXTERNALSYM RB_SETBANDINFOA}
  RB_SETBANDINFOA    = WM_USER +  6;
  {$EXTERNALSYM RB_SETPARENT}
  RB_SETPARENT       = WM_USER +  7;
  {$EXTERNALSYM RB_HITTEST}
  RB_HITTEST         = WM_USER +  8;
  {$EXTERNALSYM RB_GETRECT}
  RB_GETRECT         = WM_USER +  9;
  {$EXTERNALSYM RB_INSERTBANDW}
  RB_INSERTBANDW     = WM_USER +  10;
  {$EXTERNALSYM RB_SETBANDINFOW}
  RB_SETBANDINFOW    = WM_USER +  11;
  {$EXTERNALSYM RB_GETBANDCOUNT}
  RB_GETBANDCOUNT    = WM_USER +  12;
  {$EXTERNALSYM RB_GETROWCOUNT}
  RB_GETROWCOUNT     = WM_USER +  13;
  {$EXTERNALSYM RB_GETROWHEIGHT}
  RB_GETROWHEIGHT    = WM_USER +  14;
  {$EXTERNALSYM RB_IDTOINDEX}
  RB_IDTOINDEX       = WM_USER +  16; // wParam == id
  {$EXTERNALSYM RB_GETTOOLTIPS}
  RB_GETTOOLTIPS     = WM_USER +  17;
  {$EXTERNALSYM RB_SETTOOLTIPS}
  RB_SETTOOLTIPS     = WM_USER +  18;
  {$EXTERNALSYM RB_SETBKCOLOR}
  RB_SETBKCOLOR      = WM_USER +  19; // sets the default BK color
  {$EXTERNALSYM RB_GETBKCOLOR}
  RB_GETBKCOLOR      = WM_USER +  20; // defaults to CLR_NONE
  {$EXTERNALSYM RB_SETTEXTCOLOR}
  RB_SETTEXTCOLOR    = WM_USER +  21;
  {$EXTERNALSYM RB_GETTEXTCOLOR}
  RB_GETTEXTCOLOR    = WM_USER +  22; // defaults to 0x00000000
  {$EXTERNALSYM RB_SIZETORECT}
  RB_SIZETORECT      = WM_USER +  23; // resize the rebar/break bands and such to this rect (lparam;

  // for manual drag control
  // lparam == cursor pos
        // -1 means do it yourself.
        // -2 means use what you had saved before
  {$EXTERNALSYM RB_BEGINDRAG}
  RB_BEGINDRAG    = WM_USER + 24;
  {$EXTERNALSYM RB_ENDDRAG}
  RB_ENDDRAG      = WM_USER + 25;
  {$EXTERNALSYM RB_DRAGMOVE}
  RB_DRAGMOVE     = WM_USER + 26;
  {$EXTERNALSYM RB_GETBARHEIGHT}
  RB_GETBARHEIGHT = WM_USER + 27;
  {$EXTERNALSYM RB_GETBANDINFOW}
  RB_GETBANDINFOW = WM_USER + 28;
  {$EXTERNALSYM RB_GETBANDINFOA}
  RB_GETBANDINFOA = WM_USER + 29;

  {$EXTERNALSYM RB_MINIMIZEBAND}
  RB_MINIMIZEBAND = WM_USER + 30;
  {$EXTERNALSYM RB_MAXIMIZEBAND}
  RB_MAXIMIZEBAND = WM_USER + 31;

  {$EXTERNALSYM RB_GETDROPTARGET}
  RB_GETDROPTARGET = CCM_GETDROPTARGET;

  {$EXTERNALSYM RB_GETBANDBORDERS}
  RB_GETBANDBORDERS = WM_USER + 34;  // returns in lparam = lprc the amount of edges added to band wparam

  {$EXTERNALSYM RB_SHOWBAND}
  RB_SHOWBAND     = WM_USER + 35;      // show/hide band
  {$EXTERNALSYM RB_SETPALETTE}
  RB_SETPALETTE   = WM_USER + 37;
  {$EXTERNALSYM RB_GETPALETTE}
  RB_GETPALETTE   = WM_USER + 38;
  {$EXTERNALSYM RB_MOVEBAND}
  RB_MOVEBAND     = WM_USER + 39;

  {$EXTERNALSYM RB_SETUNICODEFORMAT}
  RB_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;
  {$EXTERNALSYM RB_GETUNICODEFORMAT}
  RB_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;









  {$EXTERNALSYM RB_INSERTBAND}
  RB_INSERTBAND      = RB_INSERTBANDA;
  {$EXTERNALSYM RB_SETBANDINFO}
  RB_SETBANDINFO     = RB_SETBANDINFOA;
  {$EXTERNALSYM RB_GETBANDINFO}
  RB_GETBANDINFO     = RB_GETBANDINFOA;


  {$EXTERNALSYM RBN_HEIGHTCHANGE}
  RBN_HEIGHTCHANGE   = RBN_FIRST - 0;

  {$EXTERNALSYM RBN_GETOBJECT}
  RBN_GETOBJECT       = RBN_FIRST - 1;
  {$EXTERNALSYM RBN_LAYOUTCHANGED}
  RBN_LAYOUTCHANGED   = RBN_FIRST - 2;
  {$EXTERNALSYM RBN_AUTOSIZE}
  RBN_AUTOSIZE        = RBN_FIRST - 3;
  {$EXTERNALSYM RBN_BEGINDRAG}
  RBN_BEGINDRAG       = RBN_FIRST - 4;
  {$EXTERNALSYM RBN_ENDDRAG}
  RBN_ENDDRAG         = RBN_FIRST - 5;
  {$EXTERNALSYM RBN_DELETINGBAND}
  RBN_DELETINGBAND    = RBN_FIRST - 6;     // Uses NMREBAR
  {$EXTERNALSYM RBN_DELETEDBAND}
  RBN_DELETEDBAND     = RBN_FIRST - 7;     // Uses NMREBAR
  {$EXTERNALSYM RBN_CHILDSIZE}
  RBN_CHILDSIZE       = RBN_FIRST - 8;

type
  {$EXTERNALSYM tagNMREBARCHILDSIZE}
  tagNMREBARCHILDSIZE = packed record
    hdr: TNMHdr;
    uBand: UINT;
    wID: UINT;
    rcChild: TRect;
    rcBand: TRect;
  end;
  PNMReBarChildSize = ^TNMReBarChildSize;
  TNMReBarChildSize = tagNMREBARCHILDSIZE;

  {$EXTERNALSYM tagNMREBAR}
  tagNMREBAR = packed record
    hdr: TNMHdr;
    dwMask: DWORD;           // RBNM_*
    uBand: UINT;
    fStyle: UINT;
    wID: UINT;
    lParam: LPARAM;
  end;
  PNMReBar = ^TNMReBar;
  TNMReBar = tagNMREBAR;

const
  // Mask flags for NMREBAR
  {$EXTERNALSYM RBNM_ID}
  RBNM_ID         = $00000001;
  {$EXTERNALSYM RBNM_STYLE}
  RBNM_STYLE      = $00000002;
  {$EXTERNALSYM RBNM_LPARAM}
  RBNM_LPARAM     = $00000004;

type
  {$EXTERNALSYM tagNMRBAUTOSIZE}
  tagNMRBAUTOSIZE = packed record
    hdr: TNMHdr;
    fChanged: BOOL;
    rcTarget: TRect;
    rcActual: TRect;
  end;
  PNMRBAutoSize = ^TNMRBAutoSize;
  TNMRBAutoSize = tagNMRBAUTOSIZE;

const
  {$EXTERNALSYM RBHT_NOWHERE}
  RBHT_NOWHERE    = $0001;
  {$EXTERNALSYM RBHT_CAPTION}
  RBHT_CAPTION    = $0002;
  {$EXTERNALSYM RBHT_CLIENT}
  RBHT_CLIENT     = $0003;
  {$EXTERNALSYM RBHT_GRABBER}
  RBHT_GRABBER    = $0004;

type
  {$EXTERNALSYM _RB_HITTESTINFO}
  _RB_HITTESTINFO = packed record
    pt: TPoint;
    flags: UINT;
    iBand: Integer;
  end;
  PRBHitTestInfo = ^TRBHitTestInfo;
  TRBHitTestInfo = _RB_HITTESTINFO;

{ ====== TOOLTIPS CONTROL ========================== }

const
  {$EXTERNALSYM TOOLTIPS_CLASS}
  TOOLTIPS_CLASS = 'tooltips_class32';

type
  PToolInfoA = ^TToolInfoA;
  PToolInfoW = ^TToolInfoW;
  PToolInfo = PToolInfoA;
  {$EXTERNALSYM tagTOOLINFOA}
  tagTOOLINFOA = packed record
    cbSize: UINT;
    uFlags: UINT;
    hwnd: HWND;
    uId: UINT;
    Rect: TRect;
    hInst: THandle;
    lpszText: PAnsiChar;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagTOOLINFOW}
  tagTOOLINFOW = packed record
    cbSize: UINT;
    uFlags: UINT;
    hwnd: HWND;
    uId: UINT;
    Rect: TRect;
    hInst: THandle;
    lpszText: PWideChar;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagTOOLINFO}
  tagTOOLINFO = tagTOOLINFOA;
  TToolInfoA = tagTOOLINFOA;
  TToolInfoW = tagTOOLINFOW;
  TToolInfo = TToolInfoA;
  {$EXTERNALSYM TOOLINFOA}
  TOOLINFOA = tagTOOLINFOA;
  {$EXTERNALSYM TOOLINFOW}
  TOOLINFOW = tagTOOLINFOW;
  {$EXTERNALSYM TOOLINFO}
  TOOLINFO = TOOLINFOA;

const
  {$EXTERNALSYM TTS_ALWAYSTIP}
  TTS_ALWAYSTIP           = $01;
  {$EXTERNALSYM TTS_NOPREFIX}
  TTS_NOPREFIX            = $02;

  {$EXTERNALSYM TTF_IDISHWND}
  TTF_IDISHWND            = $0001;

  // Use this to center around trackpoint in trackmode
  // -OR- to center around tool in normal mode.
  // Use TTF_ABSOLUTE to place the tip exactly at the track coords when
  // in tracking mode.  TTF_ABSOLUTE can be used in conjunction with TTF_CENTERTIP
  // to center the tip absolutely about the track point.

  {$EXTERNALSYM TTF_CENTERTIP}
  TTF_CENTERTIP           = $0002;
  {$EXTERNALSYM TTF_RTLREADING}
  TTF_RTLREADING          = $0004;
  {$EXTERNALSYM TTF_SUBCLASS}
  TTF_SUBCLASS            = $0010;
  {$EXTERNALSYM TTF_TRACK}
  TTF_TRACK               = $0020;
  {$EXTERNALSYM TTF_ABSOLUTE}
  TTF_ABSOLUTE            = $0080;
  {$EXTERNALSYM TTF_TRANSPARENT}
  TTF_TRANSPARENT         = $0100;
  {$EXTERNALSYM TTF_DI_SETITEM}
  TTF_DI_SETITEM          = $8000;       // valid only on the TTN_NEEDTEXT callback

  {$EXTERNALSYM TTDT_AUTOMATIC}
  TTDT_AUTOMATIC          = 0;
  {$EXTERNALSYM TTDT_RESHOW}
  TTDT_RESHOW             = 1;
  {$EXTERNALSYM TTDT_AUTOPOP}
  TTDT_AUTOPOP            = 2;
  {$EXTERNALSYM TTDT_INITIAL}
  TTDT_INITIAL            = 3;

  {$EXTERNALSYM TTM_ACTIVATE}
  TTM_ACTIVATE            = WM_USER + 1;
  {$EXTERNALSYM TTM_SETDELAYTIME}
  TTM_SETDELAYTIME        = WM_USER + 3;

  {$EXTERNALSYM TTM_ADDTOOLA}
  TTM_ADDTOOLA             = WM_USER + 4;
  {$EXTERNALSYM TTM_DELTOOLA}
  TTM_DELTOOLA             = WM_USER + 5;
  {$EXTERNALSYM TTM_NEWTOOLRECTA}
  TTM_NEWTOOLRECTA         = WM_USER + 6;
  {$EXTERNALSYM TTM_GETTOOLINFOA}
  TTM_GETTOOLINFOA         = WM_USER + 8;
  {$EXTERNALSYM TTM_SETTOOLINFOA}
  TTM_SETTOOLINFOA         = WM_USER + 9;
  {$EXTERNALSYM TTM_HITTESTA}
  TTM_HITTESTA             = WM_USER + 10;
  {$EXTERNALSYM TTM_GETTEXTA}
  TTM_GETTEXTA             = WM_USER + 11;
  {$EXTERNALSYM TTM_UPDATETIPTEXTA}
  TTM_UPDATETIPTEXTA       = WM_USER + 12;
  {$EXTERNALSYM TTM_ENUMTOOLSA}
  TTM_ENUMTOOLSA           = WM_USER + 14;
  {$EXTERNALSYM TTM_GETCURRENTTOOLA}
  TTM_GETCURRENTTOOLA      = WM_USER + 15;

  {$EXTERNALSYM TTM_ADDTOOLW}
  TTM_ADDTOOLW             = WM_USER + 50;
  {$EXTERNALSYM TTM_DELTOOLW}
  TTM_DELTOOLW             = WM_USER + 51;
  {$EXTERNALSYM TTM_NEWTOOLRECTW}
  TTM_NEWTOOLRECTW         = WM_USER + 52;
  {$EXTERNALSYM TTM_GETTOOLINFOW}
  TTM_GETTOOLINFOW         = WM_USER + 53;
  {$EXTERNALSYM TTM_SETTOOLINFOW}
  TTM_SETTOOLINFOW         = WM_USER + 54;
  {$EXTERNALSYM TTM_HITTESTW}
  TTM_HITTESTW             = WM_USER + 55;
  {$EXTERNALSYM TTM_GETTEXTW}
  TTM_GETTEXTW             = WM_USER + 56;
  {$EXTERNALSYM TTM_UPDATETIPTEXTW}
  TTM_UPDATETIPTEXTW       = WM_USER + 57;
  {$EXTERNALSYM TTM_ENUMTOOLSW}
  TTM_ENUMTOOLSW           = WM_USER + 58;
  {$EXTERNALSYM TTM_GETCURRENTTOOLW}
  TTM_GETCURRENTTOOLW      = WM_USER + 59;
  {$EXTERNALSYM TTM_WINDOWFROMPOINT}
  TTM_WINDOWFROMPOINT      = WM_USER + 16;
  {$EXTERNALSYM TTM_TRACKACTIVATE}
  TTM_TRACKACTIVATE        = WM_USER + 17;  // wParam = TRUE/FALSE start end  lparam = LPTOOLINFO
  {$EXTERNALSYM TTM_TRACKPOSITION}
  TTM_TRACKPOSITION        = WM_USER + 18;  // lParam = dwPos
  {$EXTERNALSYM TTM_SETTIPBKCOLOR}
  TTM_SETTIPBKCOLOR        = WM_USER + 19;
  {$EXTERNALSYM TTM_SETTIPTEXTCOLOR}
  TTM_SETTIPTEXTCOLOR      = WM_USER + 20;
  {$EXTERNALSYM TTM_GETDELAYTIME}
  TTM_GETDELAYTIME         = WM_USER + 21;
  {$EXTERNALSYM TTM_GETTIPBKCOLOR}
  TTM_GETTIPBKCOLOR        = WM_USER + 22;
  {$EXTERNALSYM TTM_GETTIPTEXTCOLOR}
  TTM_GETTIPTEXTCOLOR      = WM_USER + 23;
  {$EXTERNALSYM TTM_SETMAXTIPWIDTH}
  TTM_SETMAXTIPWIDTH       = WM_USER + 24;
  {$EXTERNALSYM TTM_GETMAXTIPWIDTH}
  TTM_GETMAXTIPWIDTH       = WM_USER + 25;
  {$EXTERNALSYM TTM_SETMARGIN}
  TTM_SETMARGIN            = WM_USER + 26;  // lParam = lprc
  {$EXTERNALSYM TTM_GETMARGIN}
  TTM_GETMARGIN            = WM_USER + 27;  // lParam = lprc
  {$EXTERNALSYM TTM_POP}
  TTM_POP                  = WM_USER + 28;
  {$EXTERNALSYM TTM_UPDATE}
  TTM_UPDATE               = WM_USER + 29;























  {$EXTERNALSYM TTM_ADDTOOL}
  TTM_ADDTOOL             = TTM_ADDTOOLA;
  {$EXTERNALSYM TTM_DELTOOL}
  TTM_DELTOOL             = TTM_DELTOOLA;
  {$EXTERNALSYM TTM_NEWTOOLRECT}
  TTM_NEWTOOLRECT         = TTM_NEWTOOLRECTA;
  {$EXTERNALSYM TTM_GETTOOLINFO}
  TTM_GETTOOLINFO         = TTM_GETTOOLINFOA;
  {$EXTERNALSYM TTM_SETTOOLINFO}
  TTM_SETTOOLINFO         = TTM_SETTOOLINFOA;
  {$EXTERNALSYM TTM_HITTEST}
  TTM_HITTEST             = TTM_HITTESTA;
  {$EXTERNALSYM TTM_GETTEXT}
  TTM_GETTEXT             = TTM_GETTEXTA;
  {$EXTERNALSYM TTM_UPDATETIPTEXT}
  TTM_UPDATETIPTEXT       = TTM_UPDATETIPTEXTA;
  {$EXTERNALSYM TTM_ENUMTOOLS}
  TTM_ENUMTOOLS           = TTM_ENUMTOOLSA;
  {$EXTERNALSYM TTM_GETCURRENTTOOL}
  TTM_GETCURRENTTOOL      = TTM_GETCURRENTTOOLA;


  {$EXTERNALSYM TTM_RELAYEVENT}
  TTM_RELAYEVENT          = WM_USER + 7;
  {$EXTERNALSYM TTM_GETTOOLCOUNT}
  TTM_GETTOOLCOUNT        = WM_USER +13;


type
  PTTHitTestInfoA = ^TTTHitTestInfoA;
  PTTHitTestInfoW = ^TTTHitTestInfoW;
  PTTHitTestInfo = PTTHitTestInfoA;
  {$EXTERNALSYM _TT_HITTESTINFOA}
  _TT_HITTESTINFOA = packed record
    hwnd: HWND;
    pt: TPoint;
    ti: TToolInfoA;
  end;
  {$EXTERNALSYM _TT_HITTESTINFOW}
  _TT_HITTESTINFOW = packed record
    hwnd: HWND;
    pt: TPoint;
    ti: TToolInfoW;
  end;
  {$EXTERNALSYM _TT_HITTESTINFO}
  _TT_HITTESTINFO = _TT_HITTESTINFOA;
  TTTHitTestInfoA = _TT_HITTESTINFOA;
  TTTHitTestInfoW = _TT_HITTESTINFOW;
  TTTHitTestInfo = TTTHitTestInfoA;
  {$EXTERNALSYM TTHITTESTINFOA}
  TTHITTESTINFOA = _TT_HITTESTINFOA;
  {$EXTERNALSYM TTHITTESTINFOW}
  TTHITTESTINFOW = _TT_HITTESTINFOW;
  {$EXTERNALSYM TTHITTESTINFO}
  TTHITTESTINFO = TTHITTESTINFOA;


const
  {$EXTERNALSYM TTN_NEEDTEXTA}
  TTN_NEEDTEXTA            = TTN_FIRST - 0;
  {$EXTERNALSYM TTN_NEEDTEXTW}
  TTN_NEEDTEXTW            = TTN_FIRST - 10;





  {$EXTERNALSYM TTN_NEEDTEXT}
  TTN_NEEDTEXT            = TTN_NEEDTEXTA;


  {$EXTERNALSYM TTN_SHOW}
  TTN_SHOW                = TTN_FIRST - 1;
  {$EXTERNALSYM TTN_POP}
  TTN_POP                 = TTN_FIRST - 2;

type
  tagNMTTDISPINFOA = packed record
    hdr: TNMHdr;
    lpszText: PAnsiChar;
    szText: array[0..79] of AnsiChar;
    hinst: HINST;
    uFlags: UINT;
    lParam: LPARAM;
  end;
//  {$EXTERNALSYM tagNMTTDISPINFOA}
  tagNMTTDISPINFOW = packed record
    hdr: TNMHdr;
    lpszText: PWideChar;
    szText: array[0..79] of WideChar;
    hinst: HINST;
    uFlags: UINT;
    lParam: LPARAM;
  end;
//  {$EXTERNALSYM tagNMTTDISPINFOW}
  tagNMTTDISPINFO = tagNMTTDISPINFOA;
  PNMTTDispInfoA = ^TNMTTDispInfoA;
  PNMTTDispInfoW = ^TNMTTDispInfoW;
  PNMTTDispInfo = PNMTTDispInfoA;
  TNMTTDispInfoA = tagNMTTDISPINFOA;
  TNMTTDispInfoW = tagNMTTDISPINFOW;
  TNMTTDispInfo = TNMTTDispInfoA;

  {$EXTERNALSYM tagTOOLTIPTEXTA}
  tagTOOLTIPTEXTA = tagNMTTDISPINFOA;
  {$EXTERNALSYM tagTOOLTIPTEXTW}
  tagTOOLTIPTEXTW = tagNMTTDISPINFOW;
  {$EXTERNALSYM tagTOOLTIPTEXT}
  tagTOOLTIPTEXT = tagTOOLTIPTEXTA;
  {$EXTERNALSYM TOOLTIPTEXTA}
  TOOLTIPTEXTA = tagNMTTDISPINFOA;
  {$EXTERNALSYM TOOLTIPTEXTW}
  TOOLTIPTEXTW = tagNMTTDISPINFOW;
  {$EXTERNALSYM TOOLTIPTEXT}
  TOOLTIPTEXT = TOOLTIPTEXTA;
  TToolTipTextA = tagNMTTDISPINFOA;
  TToolTipTextW = tagNMTTDISPINFOW;
  TToolTipText = TToolTipTextA;
  PToolTipTextA = ^TToolTipTextA;
  PToolTipTextW = ^TToolTipTextW;
  PToolTipText = PToolTipTextA;
{ ====== STATUS BAR CONTROL ================= }

const
  {$EXTERNALSYM SBARS_SIZEGRIP}
  SBARS_SIZEGRIP          = $0100;

{$EXTERNALSYM DrawStatusText}
procedure DrawStatusText(hDC: HDC; lprc: PRect; pzsText: PChar;
  uFlags: UINT); stdcall;
{$EXTERNALSYM DrawStatusTextA}
procedure DrawStatusTextA(hDC: HDC; lprc: PRect; pzsText: PAnsiChar;
  uFlags: UINT); stdcall;
{$EXTERNALSYM DrawStatusTextW}
procedure DrawStatusTextW(hDC: HDC; lprc: PRect; pzsText: PWideChar;
  uFlags: UINT); stdcall;
{$EXTERNALSYM CreateStatusWindow}
function CreateStatusWindow(Style: Longint; lpszText: PChar;
  hwndParent: HWND; wID: UINT): HWND; stdcall;
{$EXTERNALSYM CreateStatusWindowA}
function CreateStatusWindowA(Style: Longint; lpszText: PAnsiChar;
  hwndParent: HWND; wID: UINT): HWND; stdcall;
{$EXTERNALSYM CreateStatusWindowW}
function CreateStatusWindowW(Style: Longint; lpszText: PWideChar;
  hwndParent: HWND; wID: UINT): HWND; stdcall;

const
  {$EXTERNALSYM STATUSCLASSNAME}
  STATUSCLASSNAME = 'msctls_statusbar32';

const
  {$EXTERNALSYM SB_SETTEXTA}
  SB_SETTEXTA             = WM_USER+1;
  {$EXTERNALSYM SB_GETTEXTA}
  SB_GETTEXTA             = WM_USER+2;
  {$EXTERNALSYM SB_GETTEXTLENGTHA}
  SB_GETTEXTLENGTHA       = WM_USER+3;
  {$EXTERNALSYM SB_SETTIPTEXTA}
  SB_SETTIPTEXTA          = WM_USER+16;
  {$EXTERNALSYM SB_GETTIPTEXTA}
  SB_GETTIPTEXTA          = WM_USER+18;

  {$EXTERNALSYM SB_SETTEXTW}
  SB_SETTEXTW             = WM_USER+11;
  {$EXTERNALSYM SB_GETTEXTW}
  SB_GETTEXTW             = WM_USER+13;
  {$EXTERNALSYM SB_GETTEXTLENGTHW}
  SB_GETTEXTLENGTHW       = WM_USER+12;
  {$EXTERNALSYM SB_SETTIPTEXTW}
  SB_SETTIPTEXTW          = WM_USER+17;
  {$EXTERNALSYM SB_GETTIPTEXTW}
  SB_GETTIPTEXTW          = WM_USER+19;













  {$EXTERNALSYM SB_SETTEXT}
  SB_SETTEXT             = SB_SETTEXTA;
  {$EXTERNALSYM SB_GETTEXT}
  SB_GETTEXT             = SB_GETTEXTA;
  {$EXTERNALSYM SB_GETTEXTLENGTH}
  SB_GETTEXTLENGTH       = SB_GETTEXTLENGTHA;
  {$EXTERNALSYM SB_SETTIPTEXT}
  SB_SETTIPTEXT          = SB_SETTIPTEXTA;
  {$EXTERNALSYM SB_GETTIPTEXT}
  SB_GETTIPTEXT          = SB_GETTIPTEXTA;


  {$EXTERNALSYM SB_SETPARTS}
  SB_SETPARTS             = WM_USER+4;
  {$EXTERNALSYM SB_GETPARTS}
  SB_GETPARTS             = WM_USER+6;
  {$EXTERNALSYM SB_GETBORDERS}
  SB_GETBORDERS           = WM_USER+7;
  {$EXTERNALSYM SB_SETMINHEIGHT}
  SB_SETMINHEIGHT         = WM_USER+8;
  {$EXTERNALSYM SB_SIMPLE}
  SB_SIMPLE               = WM_USER+9;
  {$EXTERNALSYM SB_GETRECT}
  SB_GETRECT              = WM_USER + 10;
  {$EXTERNALSYM SB_ISSIMPLE}
  SB_ISSIMPLE             = WM_USER+14;
  {$EXTERNALSYM SB_SETICON}
  SB_SETICON              = WM_USER+15;
  {$EXTERNALSYM SB_GETICON}
  SB_GETICON              = WM_USER+20;
  {$EXTERNALSYM SB_SETUNICODEFORMAT}
  SB_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;
  {$EXTERNALSYM SB_GETUNICODEFORMAT}
  SB_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;

  {$EXTERNALSYM SBT_OWNERDRAW}
  SBT_OWNERDRAW            = $1000;
  {$EXTERNALSYM SBT_NOBORDERS}
  SBT_NOBORDERS            = $0100;
  {$EXTERNALSYM SBT_POPOUT}
  SBT_POPOUT               = $0200;
  {$EXTERNALSYM SBT_RTLREADING}
  SBT_RTLREADING           = $0400;
  {$EXTERNALSYM SBT_TOOLTIPS}
  SBT_TOOLTIPS             = $0800;

  {$EXTERNALSYM SB_SETBKCOLOR}
  SB_SETBKCOLOR            = CCM_SETBKCOLOR;      // lParam = bkColor

  // status bar notifications
  {$EXTERNALSYM SBN_SIMPLEMODECHANGE}
  SBN_SIMPLEMODECHANGE     = SBN_FIRST - 0;

{ ====== MENU HELP ========================== }

{$EXTERNALSYM MenuHelp}
procedure MenuHelp(Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  hMainMenu: HMENU; hInst: THandle; hwndStatus: HWND; lpwIDs: PUINT); stdcall;
{$EXTERNALSYM ShowHideMenuCtl}
function ShowHideMenuCtl(hWnd: HWND; uFlags: UINT; lpInfo: PINT): Bool; stdcall;
{$EXTERNALSYM GetEffectiveClientRect}
procedure GetEffectiveClientRect(hWnd: HWND; lprc: PRect; lpInfo: PINT); stdcall;

const
  {$EXTERNALSYM MINSYSCOMMAND}
  MINSYSCOMMAND   = SC_SIZE;


{ ====== TRACKBAR CONTROL =================== }

  {$EXTERNALSYM TRACKBAR_CLASS}
  TRACKBAR_CLASS = 'msctls_trackbar32';

const
  {$EXTERNALSYM TBS_AUTOTICKS}
  TBS_AUTOTICKS           = $0001;
  {$EXTERNALSYM TBS_VERT}
  TBS_VERT                = $0002;
  {$EXTERNALSYM TBS_HORZ}
  TBS_HORZ                = $0000;
  {$EXTERNALSYM TBS_TOP}
  TBS_TOP                 = $0004;
  {$EXTERNALSYM TBS_BOTTOM}
  TBS_BOTTOM              = $0000;
  {$EXTERNALSYM TBS_LEFT}
  TBS_LEFT                = $0004;
  {$EXTERNALSYM TBS_RIGHT}
  TBS_RIGHT               = $0000;
  {$EXTERNALSYM TBS_BOTH}
  TBS_BOTH                = $0008;
  {$EXTERNALSYM TBS_NOTICKS}
  TBS_NOTICKS             = $0010;
  {$EXTERNALSYM TBS_ENABLESELRANGE}
  TBS_ENABLESELRANGE      = $0020;
  {$EXTERNALSYM TBS_FIXEDLENGTH}
  TBS_FIXEDLENGTH         = $0040;
  {$EXTERNALSYM TBS_NOTHUMB}
  TBS_NOTHUMB             = $0080;
  {$EXTERNALSYM TBS_TOOLTIPS}
  TBS_TOOLTIPS            = $0100;

  {$EXTERNALSYM TBM_GETPOS}
  TBM_GETPOS              = WM_USER;
  {$EXTERNALSYM TBM_GETRANGEMIN}
  TBM_GETRANGEMIN         = WM_USER+1;
  {$EXTERNALSYM TBM_GETRANGEMAX}
  TBM_GETRANGEMAX         = WM_USER+2;
  {$EXTERNALSYM TBM_GETTIC}
  TBM_GETTIC              = WM_USER+3;
  {$EXTERNALSYM TBM_SETTIC}
  TBM_SETTIC              = WM_USER+4;
  {$EXTERNALSYM TBM_SETPOS}
  TBM_SETPOS              = WM_USER+5;
  {$EXTERNALSYM TBM_SETRANGE}
  TBM_SETRANGE            = WM_USER+6;
  {$EXTERNALSYM TBM_SETRANGEMIN}
  TBM_SETRANGEMIN         = WM_USER+7;
  {$EXTERNALSYM TBM_SETRANGEMAX}
  TBM_SETRANGEMAX         = WM_USER+8;
  {$EXTERNALSYM TBM_CLEARTICS}
  TBM_CLEARTICS           = WM_USER+9;
  {$EXTERNALSYM TBM_SETSEL}
  TBM_SETSEL              = WM_USER+10;
  {$EXTERNALSYM TBM_SETSELSTART}
  TBM_SETSELSTART         = WM_USER+11;
  {$EXTERNALSYM TBM_SETSELEND}
  TBM_SETSELEND           = WM_USER+12;
  {$EXTERNALSYM TBM_GETPTICS}
  TBM_GETPTICS            = WM_USER+14;
  {$EXTERNALSYM TBM_GETTICPOS}
  TBM_GETTICPOS           = WM_USER+15;
  {$EXTERNALSYM TBM_GETNUMTICS}
  TBM_GETNUMTICS          = WM_USER+16;
  {$EXTERNALSYM TBM_GETSELSTART}
  TBM_GETSELSTART         = WM_USER+17;
  {$EXTERNALSYM TBM_GETSELEND}
  TBM_GETSELEND           = WM_USER+18;
  {$EXTERNALSYM TBM_CLEARSEL}
  TBM_CLEARSEL            = WM_USER+19;
  {$EXTERNALSYM TBM_SETTICFREQ}
  TBM_SETTICFREQ          = WM_USER+20;
  {$EXTERNALSYM TBM_SETPAGESIZE}
  TBM_SETPAGESIZE         = WM_USER+21;
  {$EXTERNALSYM TBM_GETPAGESIZE}
  TBM_GETPAGESIZE         = WM_USER+22;
  {$EXTERNALSYM TBM_SETLINESIZE}
  TBM_SETLINESIZE         = WM_USER+23;
  {$EXTERNALSYM TBM_GETLINESIZE}
  TBM_GETLINESIZE         = WM_USER+24;
  {$EXTERNALSYM TBM_GETTHUMBRECT}
  TBM_GETTHUMBRECT        = WM_USER+25;
  {$EXTERNALSYM TBM_GETCHANNELRECT}
  TBM_GETCHANNELRECT      = WM_USER+26;
  {$EXTERNALSYM TBM_SETTHUMBLENGTH}
  TBM_SETTHUMBLENGTH      = WM_USER+27;
  {$EXTERNALSYM TBM_GETTHUMBLENGTH}
  TBM_GETTHUMBLENGTH      = WM_USER+28;
  {$EXTERNALSYM TBM_SETTOOLTIPS}
  TBM_SETTOOLTIPS         = WM_USER+29;
  {$EXTERNALSYM TBM_GETTOOLTIPS}
  TBM_GETTOOLTIPS         = WM_USER+30;
  {$EXTERNALSYM TBM_SETTIPSIDE}
  TBM_SETTIPSIDE          = WM_USER+31;

  // TrackBar Tip Side flags
  {$EXTERNALSYM TBTS_TOP}
  TBTS_TOP                = 0;
  {$EXTERNALSYM TBTS_LEFT}
  TBTS_LEFT               = 1;
  {$EXTERNALSYM TBTS_BOTTOM}
  TBTS_BOTTOM             = 2;
  {$EXTERNALSYM TBTS_RIGHT}
  TBTS_RIGHT              = 3;

  {$EXTERNALSYM TBM_SETBUDDY}
  TBM_SETBUDDY            = WM_USER+32; // wparam = BOOL fLeft; (or right)
  {$EXTERNALSYM TBM_GETBUDDY}
  TBM_GETBUDDY            = WM_USER+33; // wparam = BOOL fLeft; (or right)
  {$EXTERNALSYM TBM_SETUNICODEFORMAT}
  TBM_SETUNICODEFORMAT    = CCM_SETUNICODEFORMAT;
  {$EXTERNALSYM TBM_GETUNICODEFORMAT}
  TBM_GETUNICODEFORMAT    = CCM_GETUNICODEFORMAT;

  {$EXTERNALSYM TB_LINEUP}
  TB_LINEUP               = 0;
  {$EXTERNALSYM TB_LINEDOWN}
  TB_LINEDOWN             = 1;
  {$EXTERNALSYM TB_PAGEUP}
  TB_PAGEUP               = 2;
  {$EXTERNALSYM TB_PAGEDOWN}
  TB_PAGEDOWN             = 3;
  {$EXTERNALSYM TB_THUMBPOSITION}
  TB_THUMBPOSITION        = 4;
  {$EXTERNALSYM TB_THUMBTRACK}
  TB_THUMBTRACK           = 5;
  {$EXTERNALSYM TB_TOP}
  TB_TOP                  = 6;
  {$EXTERNALSYM TB_BOTTOM}
  TB_BOTTOM               = 7;
  {$EXTERNALSYM TB_ENDTRACK}
  TB_ENDTRACK             = 8;

  // custom draw item specs
  {$EXTERNALSYM TBCD_TICS}
  TBCD_TICS    = $0001;
  {$EXTERNALSYM TBCD_THUMB}
  TBCD_THUMB   = $0002;
  {$EXTERNALSYM TBCD_CHANNEL}
  TBCD_CHANNEL = $0003;

{ ====== DRAG LIST CONTROL ================== }

type
  PDragListInfo = ^TDragListInfo;
  {$EXTERNALSYM tagDRAGLISTINFO}
  tagDRAGLISTINFO = packed record
    uNotification: UINT;
    hWnd: HWND;
    ptCursor: TPoint;
  end;
  TDragListInfo = tagDRAGLISTINFO;
  {$EXTERNALSYM DRAGLISTINFO}
  DRAGLISTINFO = tagDRAGLISTINFO;

const
  {$EXTERNALSYM DL_BEGINDRAG}
  DL_BEGINDRAG            = WM_USER+133;
  {$EXTERNALSYM DL_DRAGGING}
  DL_DRAGGING             = WM_USER+134;
  {$EXTERNALSYM DL_DROPPED}
  DL_DROPPED              = WM_USER+135;
  {$EXTERNALSYM DL_CANCELDRAG}
  DL_CANCELDRAG           = WM_USER+136;

  {$EXTERNALSYM DL_CURSORSET}
  DL_CURSORSET            = 0;
  {$EXTERNALSYM DL_STOPCURSOR}
  DL_STOPCURSOR           = 1;
  {$EXTERNALSYM DL_COPYCURSOR}
  DL_COPYCURSOR           = 2;
  {$EXTERNALSYM DL_MOVECURSOR}
  DL_MOVECURSOR           = 3;

const
  {$EXTERNALSYM DRAGLISTMSGSTRING}
  DRAGLISTMSGSTRING = 'commctrl_DragListMsg';

{$EXTERNALSYM MakeDragList}
procedure MakeDragList(hLB: HWND); stdcall;
{$EXTERNALSYM DrawInsert}
procedure DrawInsert(hwndParent: HWND; hLB: HWND; nItem: Integer); stdcall;
{$EXTERNALSYM LBItemFromPt}
function LBItemFromPt(hLB: HWND; pt: TPoint; bAutoScroll: Bool): Integer; stdcall;


{ ====== UPDOWN CONTROL ========================== }

const
  {$EXTERNALSYM UPDOWN_CLASS}
  UPDOWN_CLASS = 'msctls_updown32';

type
  PUDAccel = ^TUDAccel;
  {$EXTERNALSYM _UDACCEL}
  _UDACCEL = packed record
    nSec: UINT;
    nInc: UINT;
  end;
  TUDAccel = _UDACCEL;
  {$EXTERNALSYM UDACCEL}
  UDACCEL = _UDACCEL;

const
  {$EXTERNALSYM UD_MAXVAL}
  UD_MAXVAL               = $7fff;
  {$EXTERNALSYM UD_MINVAL}
  UD_MINVAL               = -UD_MAXVAL;

  {$EXTERNALSYM UDS_WRAP}
  UDS_WRAP                = $0001;
  {$EXTERNALSYM UDS_SETBUDDYINT}
  UDS_SETBUDDYINT         = $0002;
  {$EXTERNALSYM UDS_ALIGNRIGHT}
  UDS_ALIGNRIGHT          = $0004;
  {$EXTERNALSYM UDS_ALIGNLEFT}
  UDS_ALIGNLEFT           = $0008;
  {$EXTERNALSYM UDS_AUTOBUDDY}
  UDS_AUTOBUDDY           = $0010;
  {$EXTERNALSYM UDS_ARROWKEYS}
  UDS_ARROWKEYS           = $0020;
  {$EXTERNALSYM UDS_HORZ}
  UDS_HORZ                = $0040;
  {$EXTERNALSYM UDS_NOTHOUSANDS}
  UDS_NOTHOUSANDS         = $0080;
  {$EXTERNALSYM UDS_HOTTRACK}
  UDS_HOTTRACK            = $0100;


  {$EXTERNALSYM UDM_SETRANGE}
  UDM_SETRANGE            = WM_USER+101;
  {$EXTERNALSYM UDM_GETRANGE}
  UDM_GETRANGE            = WM_USER+102;
  {$EXTERNALSYM UDM_SETPOS}
  UDM_SETPOS              = WM_USER+103;
  {$EXTERNALSYM UDM_GETPOS}
  UDM_GETPOS              = WM_USER+104;
  {$EXTERNALSYM UDM_SETBUDDY}
  UDM_SETBUDDY            = WM_USER+105;
  {$EXTERNALSYM UDM_GETBUDDY}
  UDM_GETBUDDY            = WM_USER+106;
  {$EXTERNALSYM UDM_SETACCEL}
  UDM_SETACCEL            = WM_USER+107;
  {$EXTERNALSYM UDM_GETACCEL}
  UDM_GETACCEL            = WM_USER+108;
  {$EXTERNALSYM UDM_SETBASE}
  UDM_SETBASE             = WM_USER+109;
  {$EXTERNALSYM UDM_GETBASE}
  UDM_GETBASE             = WM_USER+110;
  {$EXTERNALSYM UDM_SETRANGE32}
  UDM_SETRANGE32          = WM_USER+111;
  {$EXTERNALSYM UDM_GETRANGE32}
  UDM_GETRANGE32          = WM_USER+112; // wParam & lParam are LPINT
  {$EXTERNALSYM UDM_SETUNICODEFORMAT}
  UDM_SETUNICODEFORMAT    = CCM_SETUNICODEFORMAT;
  {$EXTERNALSYM UDM_GETUNICODEFORMAT}
  UDM_GETUNICODEFORMAT    = CCM_GETUNICODEFORMAT;

{$EXTERNALSYM CreateUpDownControl}
function CreateUpDownControl(dwStyle: Longint; X, Y, CX, CY: Integer;
  hParent: HWND;  nID: Integer; hInst: THandle; hBuddy: HWND;
  nUpper, nLower, nPos: Integer): HWND; stdcall;

type
  PNMUpDown = ^TNMUpDown;
  {$EXTERNALSYM _NM_UPDOWN}
  _NM_UPDOWN = packed record
    hdr: TNMHDR;
    iPos: Integer;
    iDelta: Integer;
  end;
  TNMUpDown = _NM_UPDOWN;
  {$EXTERNALSYM NM_UPDOWN}
  NM_UPDOWN = _NM_UPDOWN;

const
  {$EXTERNALSYM UDN_DELTAPOS}
  UDN_DELTAPOS = UDN_FIRST - 1;


{ ====== PROGRESS CONTROL ========================= }

const
  {$EXTERNALSYM PROGRESS_CLASS}
  PROGRESS_CLASS = 'msctls_progress32';

type
  {$EXTERNALSYM PBRANGE}
  PBRANGE = record
    iLow: Integer;
    iHigh: Integer;
  end;
  PPBRange = ^TPBRange;
  TPBRange = PBRANGE;

const
  {$EXTERNALSYM PBS_SMOOTH}
  PBS_SMOOTH              = 01;
  {$EXTERNALSYM PBS_VERTICAL}
  PBS_VERTICAL            = 04;
  
  {$EXTERNALSYM PBM_SETRANGE}
  PBM_SETRANGE            = WM_USER+1;
  {$EXTERNALSYM PBM_SETPOS}
  PBM_SETPOS              = WM_USER+2;
  {$EXTERNALSYM PBM_DELTAPOS}
  PBM_DELTAPOS            = WM_USER+3;
  {$EXTERNALSYM PBM_SETSTEP}
  PBM_SETSTEP             = WM_USER+4;
  {$EXTERNALSYM PBM_STEPIT}
  PBM_STEPIT              = WM_USER+5;
  {$EXTERNALSYM PBM_SETRANGE32}
  PBM_SETRANGE32          = WM_USER+6;   // lParam = high, wParam = low
  {$EXTERNALSYM PBM_GETRANGE}
  PBM_GETRANGE            = WM_USER+7;   // lParam = PPBRange or Nil
					 // wParam = False: Result = high
					 // wParam = True: Result = low
  {$EXTERNALSYM PBM_GETPOS}
  PBM_GETPOS              = WM_USER+8;
  {$EXTERNALSYM PBM_SETBARCOLOR}
  PBM_SETBARCOLOR         = WM_USER+9;		// lParam = bar color
  {$EXTERNALSYM PBM_SETBKCOLOR}
  PBM_SETBKCOLOR          = CCM_SETBKCOLOR;  // lParam = bkColor


{  ====== HOTKEY CONTROL ========================== }

const
  {$EXTERNALSYM HOTKEYF_SHIFT}
  HOTKEYF_SHIFT           = $01;
  {$EXTERNALSYM HOTKEYF_CONTROL}
  HOTKEYF_CONTROL         = $02;
  {$EXTERNALSYM HOTKEYF_ALT}
  HOTKEYF_ALT             = $04;
  {$EXTERNALSYM HOTKEYF_EXT}
  HOTKEYF_EXT             = $08;

  {$EXTERNALSYM HKCOMB_NONE}
  HKCOMB_NONE             = $0001;
  {$EXTERNALSYM HKCOMB_S}
  HKCOMB_S                = $0002;
  {$EXTERNALSYM HKCOMB_C}
  HKCOMB_C                = $0004;
  {$EXTERNALSYM HKCOMB_A}
  HKCOMB_A                = $0008;
  {$EXTERNALSYM HKCOMB_SC}
  HKCOMB_SC               = $0010;
  {$EXTERNALSYM HKCOMB_SA}
  HKCOMB_SA               = $0020;
  {$EXTERNALSYM HKCOMB_CA}
  HKCOMB_CA               = $0040;
  {$EXTERNALSYM HKCOMB_SCA}
  HKCOMB_SCA              = $0080;


  {$EXTERNALSYM HKM_SETHOTKEY}
  HKM_SETHOTKEY           = WM_USER+1;
  {$EXTERNALSYM HKM_GETHOTKEY}
  HKM_GETHOTKEY           = WM_USER+2;
  {$EXTERNALSYM HKM_SETRULES}
  HKM_SETRULES            = WM_USER+3;

const
  HOTKEYCLASS = 'msctls_hotkey32';


{ ====== COMMON CONTROL STYLES ================ }

const
  {$EXTERNALSYM CCS_TOP}
  CCS_TOP                 = $00000001;
  {$EXTERNALSYM CCS_NOMOVEY}
  CCS_NOMOVEY             = $00000002;
  {$EXTERNALSYM CCS_BOTTOM}
  CCS_BOTTOM              = $00000003;
  {$EXTERNALSYM CCS_NORESIZE}
  CCS_NORESIZE            = $00000004;
  {$EXTERNALSYM CCS_NOPARENTALIGN}
  CCS_NOPARENTALIGN       = $00000008;
  {$EXTERNALSYM CCS_ADJUSTABLE}
  CCS_ADJUSTABLE          = $00000020;
  {$EXTERNALSYM CCS_NODIVIDER}
  CCS_NODIVIDER           = $00000040;
  {$EXTERNALSYM CCS_VERT}
  CCS_VERT                = $00000080;
  {$EXTERNALSYM CCS_LEFT}
  CCS_LEFT                = (CCS_VERT or CCS_TOP);
  {$EXTERNALSYM CCS_RIGHT}
  CCS_RIGHT               = (CCS_VERT or CCS_BOTTOM);
  {$EXTERNALSYM CCS_NOMOVEX}
  CCS_NOMOVEX             = (CCS_VERT or CCS_NOMOVEY);


{ ====== LISTVIEW CONTROL ====================== }

const
  {$EXTERNALSYM WC_LISTVIEW}
  WC_LISTVIEW = 'SysListView32';

const

  { List View Styles }
  {$EXTERNALSYM LVS_ICON}
  LVS_ICON                = $0000;
  {$EXTERNALSYM LVS_REPORT}
  LVS_REPORT              = $0001;
  {$EXTERNALSYM LVS_SMALLICON}
  LVS_SMALLICON           = $0002;
  {$EXTERNALSYM LVS_LIST}
  LVS_LIST                = $0003;
  {$EXTERNALSYM LVS_TYPEMASK}
  LVS_TYPEMASK            = $0003;
  {$EXTERNALSYM LVS_SINGLESEL}
  LVS_SINGLESEL           = $0004;
  {$EXTERNALSYM LVS_SHOWSELALWAYS}
  LVS_SHOWSELALWAYS       = $0008;
  {$EXTERNALSYM LVS_SORTASCENDING}
  LVS_SORTASCENDING       = $0010;
  {$EXTERNALSYM LVS_SORTDESCENDING}
  LVS_SORTDESCENDING      = $0020;
  {$EXTERNALSYM LVS_SHAREIMAGELISTS}
  LVS_SHAREIMAGELISTS     = $0040;
  {$EXTERNALSYM LVS_NOLABELWRAP}
  LVS_NOLABELWRAP         = $0080;
  {$EXTERNALSYM LVS_AUTOARRANGE}
  LVS_AUTOARRANGE         = $0100;
  {$EXTERNALSYM LVS_EDITLABELS}
  LVS_EDITLABELS          = $0200;
  {$EXTERNALSYM LVS_OWNERDATA}
  LVS_OWNERDATA           = $1000; 
  {$EXTERNALSYM LVS_NOSCROLL}
  LVS_NOSCROLL            = $2000;

  {$EXTERNALSYM LVS_TYPESTYLEMASK}
  LVS_TYPESTYLEMASK       = $FC00;

  {$EXTERNALSYM LVS_ALIGNTOP}
  LVS_ALIGNTOP            = $0000;
  {$EXTERNALSYM LVS_ALIGNLEFT}
  LVS_ALIGNLEFT           = $0800;
  {$EXTERNALSYM LVS_ALIGNMASK}
  LVS_ALIGNMASK           = $0c00;

  {$EXTERNALSYM LVS_OWNERDRAWFIXED}
  LVS_OWNERDRAWFIXED      = $0400;
  {$EXTERNALSYM LVS_NOCOLUMNHEADER}
  LVS_NOCOLUMNHEADER      = $4000;
  {$EXTERNALSYM LVS_NOSORTHEADER}
  LVS_NOSORTHEADER        = $8000;

  { List View Extended Styles }
  {$EXTERNALSYM LVS_EX_GRIDLINES}
  LVS_EX_GRIDLINES        = $00000001;
  {$EXTERNALSYM LVS_EX_SUBITEMIMAGES}
  LVS_EX_SUBITEMIMAGES    = $00000002;
  {$EXTERNALSYM LVS_EX_CHECKBOXES}
  LVS_EX_CHECKBOXES       = $00000004;
  {$EXTERNALSYM LVS_EX_TRACKSELECT}
  LVS_EX_TRACKSELECT      = $00000008;
  {$EXTERNALSYM LVS_EX_HEADERDRAGDROP}
  LVS_EX_HEADERDRAGDROP   = $00000010;
  {$EXTERNALSYM LVS_EX_FULLROWSELECT}
  LVS_EX_FULLROWSELECT    = $00000020; // applies to report mode only
  {$EXTERNALSYM LVS_EX_ONECLICKACTIVATE}
  LVS_EX_ONECLICKACTIVATE = $00000040;
  {$EXTERNALSYM LVS_EX_TWOCLICKACTIVATE}
  LVS_EX_TWOCLICKACTIVATE = $00000080;
  {$EXTERNALSYM LVS_EX_FLATSB}
  LVS_EX_FLATSB           = $00000100;
  {$EXTERNALSYM LVS_EX_REGIONAL}
  LVS_EX_REGIONAL         = $00000200;
  {$EXTERNALSYM LVS_EX_INFOTIP}
  LVS_EX_INFOTIP          = $00000400; // listview does InfoTips for you
  {$EXTERNALSYM LVS_EX_UNDERLINEHOT}
  LVS_EX_UNDERLINEHOT     = $00000800;
  {$EXTERNALSYM LVS_EX_UNDERLINECOLD}
  LVS_EX_UNDERLINECOLD    = $00001000;
  {$EXTERNALSYM LVS_EX_MULTIWORKAREAS}
  LVS_EX_MULTIWORKAREAS   = $00002000;

const
  {$EXTERNALSYM LVM_SETUNICODEFORMAT}
  LVM_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;

{$EXTERNALSYM ListView_SetUnicodeFormat}
function ListView_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): Integer; inline;

const
  {$EXTERNALSYM LVM_GETUNICODEFORMAT}
  LVM_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;

{$EXTERNALSYM ListView_GetUnicodeFormat}
function ListView_GetUnicodeFormat(hwnd: HWND): BOOL; inline;

const
  {$EXTERNALSYM LVM_GETBKCOLOR}
  LVM_GETBKCOLOR          = LVM_FIRST + 0;

{$EXTERNALSYM ListView_GetBkColor}
function ListView_GetBkColor(hWnd: HWND): TColorRef; inline;

const
  {$EXTERNALSYM LVM_SETBKCOLOR}
  LVM_SETBKCOLOR          = LVM_FIRST + 1;

{$EXTERNALSYM ListView_SetBkColor}
function ListView_SetBkColor(hWnd: HWND; clrBk: TColorRef): Bool; inline;

const
  {$EXTERNALSYM LVM_GETIMAGELIST}
  LVM_GETIMAGELIST        = LVM_FIRST + 2;

{$EXTERNALSYM ListView_GetImageList}
function ListView_GetImageList(hWnd: HWND; iImageList: Integer): HIMAGELIST; inline;

const
  {$EXTERNALSYM LVSIL_NORMAL}
  LVSIL_NORMAL            = 0;
  {$EXTERNALSYM LVSIL_SMALL}
  LVSIL_SMALL             = 1;
  {$EXTERNALSYM LVSIL_STATE}
  LVSIL_STATE             = 2;

const
  {$EXTERNALSYM LVM_SETIMAGELIST}
  LVM_SETIMAGELIST        = LVM_FIRST + 3;

{$EXTERNALSYM ListView_SetImageList}
function ListView_SetImageList(hWnd: HWND; himl: HIMAGELIST;
  iImageList: Integer): HIMAGELIST; inline;

const
  {$EXTERNALSYM LVM_GETITEMCOUNT}
  LVM_GETITEMCOUNT        = LVM_FIRST + 4;

{$EXTERNALSYM ListView_GetItemCount}
function ListView_GetItemCount(hWnd: HWND): Integer; inline;

const
  {$EXTERNALSYM LVIF_TEXT}
  LVIF_TEXT               = $0001;
  {$EXTERNALSYM LVIF_IMAGE}
  LVIF_IMAGE              = $0002;
  {$EXTERNALSYM LVIF_PARAM}
  LVIF_PARAM              = $0004;
  {$EXTERNALSYM LVIF_STATE}
  LVIF_STATE              = $0008;
  {$EXTERNALSYM LVIF_INDENT}
  LVIF_INDENT             = $0010;
  {$EXTERNALSYM LVIF_NORECOMPUTE}
  LVIF_NORECOMPUTE        = $0800;

  {$EXTERNALSYM LVIS_FOCUSED}
  LVIS_FOCUSED            = $0001;
  {$EXTERNALSYM LVIS_SELECTED}
  LVIS_SELECTED           = $0002;
  {$EXTERNALSYM LVIS_CUT}
  LVIS_CUT                = $0004;
  {$EXTERNALSYM LVIS_DROPHILITED}
  LVIS_DROPHILITED        = $0008;
  {$EXTERNALSYM LVIS_ACTIVATING}
  LVIS_ACTIVATING         = $0020;

  {$EXTERNALSYM LVIS_OVERLAYMASK}
  LVIS_OVERLAYMASK        = $0F00;
  {$EXTERNALSYM LVIS_STATEIMAGEMASK}
  LVIS_STATEIMAGEMASK     = $F000;

{$EXTERNALSYM IndexToStateImageMask}
function IndexToStateImageMask(I: Longint): Longint; inline;

const
  {$EXTERNALSYM I_INDENTCALLBACK}
  I_INDENTCALLBACK        = -1;
  {$EXTERNALSYM I_IMAGENONE}
  I_IMAGENONE             = -2;
  {$EXTERNALSYM I_COLUMNSCALLBACK}
  I_COLUMNSCALLBACK       = -1;

type
  PLVItemA = ^TLVItemA;
  PLVItemW = ^TLVItemW;
  PLVItem = PLVItemA;
  {$EXTERNALSYM tagLVITEMA}
  tagLVITEMA = packed record
    mask: UINT;
    iItem: Integer;
    iSubItem: Integer;
    state: UINT;
    stateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
    iIndent: Integer;
  end;
  {$EXTERNALSYM tagLVITEMW}
  tagLVITEMW = packed record
    mask: UINT;
    iItem: Integer;
    iSubItem: Integer;
    state: UINT;
    stateMask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
    iIndent: Integer;
  end;
  {$EXTERNALSYM tagLVITEM}
  tagLVITEM = tagLVITEMA;
  {$EXTERNALSYM _LV_ITEMA}
  _LV_ITEMA = tagLVITEMA;
  {$EXTERNALSYM _LV_ITEMW}
  _LV_ITEMW = tagLVITEMW;
  {$EXTERNALSYM _LV_ITEM}
  _LV_ITEM = _LV_ITEMA;
  TLVItemA = tagLVITEMA;
  TLVItemW = tagLVITEMW;
  TLVItem = TLVItemA;
  {$EXTERNALSYM LV_ITEMA}
  LV_ITEMA = tagLVITEMA;
  {$EXTERNALSYM LV_ITEMW}
  LV_ITEMW = tagLVITEMW;
  {$EXTERNALSYM LV_ITEM}
  LV_ITEM = LV_ITEMA;

const
  {$EXTERNALSYM LPSTR_TEXTCALLBACKA}
  LPSTR_TEXTCALLBACKA = LPSTR(-1);
  {$EXTERNALSYM LPSTR_TEXTCALLBACKW}
  LPSTR_TEXTCALLBACKW = LPWSTR(-1);





  {$EXTERNALSYM LPSTR_TEXTCALLBACK}
  LPSTR_TEXTCALLBACK = LPSTR_TEXTCALLBACKA;


  {$EXTERNALSYM I_IMAGECALLBACK}
  I_IMAGECALLBACK         = -1;

const
  {$EXTERNALSYM LVM_GETITEMA}
  LVM_GETITEMA            = LVM_FIRST + 5;
  {$EXTERNALSYM LVM_SETITEMA}
  LVM_SETITEMA            = LVM_FIRST + 6;
  {$EXTERNALSYM LVM_INSERTITEMA}
  LVM_INSERTITEMA         = LVM_FIRST + 7;

  {$EXTERNALSYM LVM_GETITEMW}
  LVM_GETITEMW            = LVM_FIRST + 75;
  {$EXTERNALSYM LVM_SETITEMW}
  LVM_SETITEMW            = LVM_FIRST + 76;
  {$EXTERNALSYM LVM_INSERTITEMW}
  LVM_INSERTITEMW         = LVM_FIRST + 77;









  {$EXTERNALSYM LVM_GETITEM}
  LVM_GETITEM            = LVM_GETITEMA;
  {$EXTERNALSYM LVM_SETITEM}
  LVM_SETITEM            = LVM_SETITEMA;
  {$EXTERNALSYM LVM_INSERTITEM}
  LVM_INSERTITEM         = LVM_INSERTITEMA;


  {$EXTERNALSYM LVM_DELETEITEM}
  LVM_DELETEITEM          = LVM_FIRST + 8;
  {$EXTERNALSYM LVM_DELETEALLITEMS}
  LVM_DELETEALLITEMS      = LVM_FIRST + 9;
  {$EXTERNALSYM LVM_GETCALLBACKMASK}
  LVM_GETCALLBACKMASK     = LVM_FIRST + 10;
  {$EXTERNALSYM LVM_SETCALLBACKMASK}
  LVM_SETCALLBACKMASK     = LVM_FIRST + 11;

{$EXTERNALSYM ListView_GetItem}
function ListView_GetItem(hWnd: HWND; var pItem: TLVItem): Bool; inline;
{$EXTERNALSYM ListView_GetItemA}
function ListView_GetItemA(hWnd: HWND; var pItem: TLVItemA): Bool; inline;
{$EXTERNALSYM ListView_GetItemW}
function ListView_GetItemW(hWnd: HWND; var pItem: TLVItemW): Bool; inline;
{$EXTERNALSYM ListView_SetItem}
function ListView_SetItem(hWnd: HWND; const pItem: TLVItem): Bool; inline;
{$EXTERNALSYM ListView_SetItemA}
function ListView_SetItemA(hWnd: HWND; const pItem: TLVItemA): Bool; inline;
{$EXTERNALSYM ListView_SetItemW}
function ListView_SetItemW(hWnd: HWND; const pItem: TLVItemW): Bool; inline;
{$EXTERNALSYM ListView_InsertItem}
function ListView_InsertItem(hWnd: HWND; const pItem: TLVItem): Integer; inline;
{$EXTERNALSYM ListView_InsertItemA}
function ListView_InsertItemA(hWnd: HWND; const pItem: TLVItemA): Integer; inline;
{$EXTERNALSYM ListView_InsertItemW}
function ListView_InsertItemW(hWnd: HWND; const pItem: TLVItemW): Integer; inline;
{$EXTERNALSYM ListView_DeleteItem}
function ListView_DeleteItem(hWnd: HWND; i: Integer): Bool; inline;
{$EXTERNALSYM ListView_DeleteAllItems}
function ListView_DeleteAllItems(hWnd: HWND): Bool; inline;
{$EXTERNALSYM ListView_GetCallbackMask}
function ListView_GetCallbackMask(hWnd: HWND): UINT; inline;
{$EXTERNALSYM ListView_SetCallbackMask}
function ListView_SetCallbackMask(hWnd: HWND; mask: UINT): Bool; inline;

const
  {$EXTERNALSYM LVNI_ALL}
  LVNI_ALL                = $0000;
  {$EXTERNALSYM LVNI_FOCUSED}
  LVNI_FOCUSED            = $0001;
  {$EXTERNALSYM LVNI_SELECTED}
  LVNI_SELECTED           = $0002;
  {$EXTERNALSYM LVNI_CUT}
  LVNI_CUT                = $0004;
  {$EXTERNALSYM LVNI_DROPHILITED}
  LVNI_DROPHILITED        = $0008;

  {$EXTERNALSYM LVNI_ABOVE}
  LVNI_ABOVE              = $0100;
  {$EXTERNALSYM LVNI_BELOW}
  LVNI_BELOW              = $0200;
  {$EXTERNALSYM LVNI_TOLEFT}
  LVNI_TOLEFT             = $0400;
  {$EXTERNALSYM LVNI_TORIGHT}
  LVNI_TORIGHT            = $0800;


const
  {$EXTERNALSYM LVM_GETNEXTITEM}
  LVM_GETNEXTITEM         = LVM_FIRST + 12;

{$EXTERNALSYM ListView_GetNextItem}
function ListView_GetNextItem(hWnd: HWND; iStart: Integer; Flags: UINT): Integer;

const
  {$EXTERNALSYM LVFI_PARAM}
  LVFI_PARAM              = $0001;
  {$EXTERNALSYM LVFI_STRING}
  LVFI_STRING             = $0002;
  {$EXTERNALSYM LVFI_PARTIAL}
  LVFI_PARTIAL            = $0008;
  {$EXTERNALSYM LVFI_WRAP}
  LVFI_WRAP               = $0020;
  {$EXTERNALSYM LVFI_NEARESTXY}
  LVFI_NEARESTXY          = $0040;


type
  PLVFindInfoA = ^TLVFindInfoA;
  PLVFindInfoW = ^TLVFindInfoW;
  PLVFindInfo = PLVFindInfoA;
  {$EXTERNALSYM tagLVFINDINFOA}
  tagLVFINDINFOA = packed record
    flags: UINT;
    psz: PAnsiChar;
    lParam: LPARAM;
    pt: TPoint;
    vkDirection: UINT;
  end;
  {$EXTERNALSYM tagLVFINDINFOW}
  tagLVFINDINFOW = packed record
    flags: UINT;
    psz: PWideChar;
    lParam: LPARAM;
    pt: TPoint;
    vkDirection: UINT;
  end;
  {$EXTERNALSYM tagLVFINDINFO}
  tagLVFINDINFO = tagLVFINDINFOA;
  {$EXTERNALSYM _LV_FINDINFOA}
  _LV_FINDINFOA = tagLVFINDINFOA;
  {$EXTERNALSYM _LV_FINDINFOW}
  _LV_FINDINFOW = tagLVFINDINFOW;
  {$EXTERNALSYM _LV_FINDINFO}
  _LV_FINDINFO = _LV_FINDINFOA;
  TLVFindInfoA = tagLVFINDINFOA;
  TLVFindInfoW = tagLVFINDINFOW;
  TLVFindInfo = TLVFindInfoA;
  {$EXTERNALSYM LV_FINDINFOA}
  LV_FINDINFOA = tagLVFINDINFOA;
  {$EXTERNALSYM LV_FINDINFOW}
  LV_FINDINFOW = tagLVFINDINFOW;
  {$EXTERNALSYM LV_FINDINFO}
  LV_FINDINFO = LV_FINDINFOA;

const
  {$EXTERNALSYM LVM_FINDITEMA}
  LVM_FINDITEMA            = LVM_FIRST + 13;
  {$EXTERNALSYM LVM_FINDITEMW}
  LVM_FINDITEMW            = LVM_FIRST + 83;




  {$EXTERNALSYM LVM_FINDITEM}
  LVM_FINDITEM            = LVM_FINDITEMA;


{$EXTERNALSYM ListView_FindItem}
function ListView_FindItem(hWnd: HWND; iStart: Integer;
  const plvfi: TLVFindInfo): Integer; inline;
{$EXTERNALSYM ListView_FindItemA}
function ListView_FindItemA(hWnd: HWND; iStart: Integer;
  const plvfi: TLVFindInfoA): Integer; inline;
{$EXTERNALSYM ListView_FindItemW}
function ListView_FindItemW(hWnd: HWND; iStart: Integer;
  const plvfi: TLVFindInfoW): Integer; inline;

const
  {$EXTERNALSYM LVIR_BOUNDS}
  LVIR_BOUNDS             = 0;
  {$EXTERNALSYM LVIR_ICON}
  LVIR_ICON               = 1;
  {$EXTERNALSYM LVIR_LABEL}
  LVIR_LABEL              = 2;
  {$EXTERNALSYM LVIR_SELECTBOUNDS}
  LVIR_SELECTBOUNDS       = 3;


const
  {$EXTERNALSYM LVM_GETITEMRECT}
  LVM_GETITEMRECT         = LVM_FIRST + 14;

{$EXTERNALSYM ListView_GetItemRect}
function ListView_GetItemRect(hWnd: HWND; i: Integer; var prc: TRect;
  Code: Integer): Bool;

const
  {$EXTERNALSYM LVM_SETITEMPOSITION}
  LVM_SETITEMPOSITION     = LVM_FIRST + 15;

{$EXTERNALSYM ListView_SetItemPosition}
function ListView_SetItemPosition(hWnd: HWND; i, x, y: Integer): Bool;

const
  {$EXTERNALSYM LVM_GETITEMPOSITION}
  LVM_GETITEMPOSITION     = LVM_FIRST + 16;

{$EXTERNALSYM ListView_GetItemPosition}
function ListView_GetItemPosition(hwndLV: HWND; i: Integer; var ppt: TPoint): Bool; inline;

const
  {$EXTERNALSYM LVM_GETSTRINGWIDTHA}
  LVM_GETSTRINGWIDTHA      = LVM_FIRST + 17;
  {$EXTERNALSYM LVM_GETSTRINGWIDTHW}
  LVM_GETSTRINGWIDTHW      = LVM_FIRST + 87;




  {$EXTERNALSYM LVM_GETSTRINGWIDTH}
  LVM_GETSTRINGWIDTH      = LVM_GETSTRINGWIDTHA;


{$EXTERNALSYM ListView_GetStringWidth}
function ListView_GetStringWidth(hwndLV: HWND; psz: PChar): Integer; inline;
{$EXTERNALSYM ListView_GetStringWidthA}
function ListView_GetStringWidthA(hwndLV: HWND; psz: PAnsiChar): Integer; inline;
{$EXTERNALSYM ListView_GetStringWidthW}
function ListView_GetStringWidthW(hwndLV: HWND; psz: PWideChar): Integer; inline;

const
  {$EXTERNALSYM LVHT_NOWHERE}
  LVHT_NOWHERE            = $0001;
  {$EXTERNALSYM LVHT_ONITEMICON}
  LVHT_ONITEMICON         = $0002;
  {$EXTERNALSYM LVHT_ONITEMLABEL}
  LVHT_ONITEMLABEL        = $0004;
  {$EXTERNALSYM LVHT_ONITEMSTATEICON}
  LVHT_ONITEMSTATEICON    = $0008;
  {$EXTERNALSYM LVHT_ONITEM}
  LVHT_ONITEM             = LVHT_ONITEMICON or LVHT_ONITEMLABEL or
			    LVHT_ONITEMSTATEICON;
  {$EXTERNALSYM LVHT_ABOVE}
  LVHT_ABOVE              = $0008;
  {$EXTERNALSYM LVHT_BELOW}
  LVHT_BELOW              = $0010;
  {$EXTERNALSYM LVHT_TORIGHT}
  LVHT_TORIGHT            = $0020;
  {$EXTERNALSYM LVHT_TOLEFT}
  LVHT_TOLEFT             = $0040;

type
  PLVHitTestInfo = ^TLVHitTestInfo;
  {$EXTERNALSYM tagLVHITTESTINFO}
  tagLVHITTESTINFO = packed record
    pt: TPoint;
    flags: UINT;
    iItem: Integer;
    iSubItem: Integer;    // this is was NOT in win95.  valid only for LVM_SUBITEMHITTEST
  end;
  TLVHitTestInfo = tagLVHITTESTINFO;
  {$EXTERNALSYM LV_HITTESTINFO}
  LV_HITTESTINFO = tagLVHITTESTINFO;
  {$EXTERNALSYM _LV_HITTESTINFO}
  _LV_HITTESTINFO = tagLVHITTESTINFO;

const
  {$EXTERNALSYM LVM_HITTEST}
  LVM_HITTEST             = LVM_FIRST + 18;

{$EXTERNALSYM ListView_HitTest}
function ListView_HitTest(hwndLV: HWND; var pinfo: TLVHitTestInfo): Integer; inline;

const
  {$EXTERNALSYM LVM_ENSUREVISIBLE}
  LVM_ENSUREVISIBLE       = LVM_FIRST + 19;

{$EXTERNALSYM ListView_EnsureVisible}
function ListView_EnsureVisible(hwndLV: HWND; i: Integer; fPartialOK: Bool): Bool;

const
  {$EXTERNALSYM LVM_SCROLL}
  LVM_SCROLL              = LVM_FIRST + 20;

{$EXTERNALSYM ListView_Scroll}
function ListView_Scroll(hwndLV: HWnd; DX, DY: Integer): Bool; inline;

const
  {$EXTERNALSYM LVM_REDRAWITEMS}
  LVM_REDRAWITEMS         = LVM_FIRST + 21;

{$EXTERNALSYM ListView_RedrawItems}
function ListView_RedrawItems(hwndLV: HWND; iFirst, iLast: Integer): Bool; inline;

const
  {$EXTERNALSYM LVA_DEFAULT}
  LVA_DEFAULT             = $0000;
  {$EXTERNALSYM LVA_ALIGNLEFT}
  LVA_ALIGNLEFT           = $0001;
  {$EXTERNALSYM LVA_ALIGNTOP}
  LVA_ALIGNTOP            = $0002;
  LVA_ALIGNRIGHT          = $0003;
  LVA_ALIGNBOTTOM         = $0004;
  {$EXTERNALSYM LVA_SNAPTOGRID}
  LVA_SNAPTOGRID          = $0005;

  LVA_SORTASCENDING       = $0100;
  LVA_SORTDESCENDING      = $0200;

  {$EXTERNALSYM LVM_ARRANGE}
  LVM_ARRANGE             = LVM_FIRST + 22;

{$EXTERNALSYM ListView_Arrange}
function ListView_Arrange(hwndLV: HWND; Code: UINT): Bool; inline;


const
  {$EXTERNALSYM LVM_EDITLABELA}
  LVM_EDITLABELA           = LVM_FIRST + 23;
  {$EXTERNALSYM LVM_EDITLABELW}
  LVM_EDITLABELW           = LVM_FIRST + 118;




  {$EXTERNALSYM LVM_EDITLABEL}
  LVM_EDITLABEL           = LVM_EDITLABELA;


{$EXTERNALSYM ListView_EditLabel}
function ListView_EditLabel(hwndLV: HWND; i: Integer): HWND; inline;
{$EXTERNALSYM ListView_EditLabelA}
function ListView_EditLabelA(hwndLV: HWND; i: Integer): HWND; inline;
{$EXTERNALSYM ListView_EditLabelW}
function ListView_EditLabelW(hwndLV: HWND; i: Integer): HWND; inline;

const
  {$EXTERNALSYM LVM_GETEDITCONTROL}
  LVM_GETEDITCONTROL      = LVM_FIRST + 24;

{$EXTERNALSYM ListView_GetEditControl}
function ListView_GetEditControl(hwndLV: HWND): HWND; inline;

type
  PLVColumnA = ^TLVColumnA;
  PLVColumnW = ^TLVColumnW;
  PLVColumn = PLVColumnA;
  {$EXTERNALSYM tagLVCOLUMNA}
  tagLVCOLUMNA = packed record
    mask: UINT;
    fmt: Integer;
    cx: Integer;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iSubItem: Integer;
    iImage: Integer;
    iOrder: Integer;
  end;
  {$EXTERNALSYM tagLVCOLUMNW}
  tagLVCOLUMNW = packed record
    mask: UINT;
    fmt: Integer;
    cx: Integer;
    pszText: PWideChar;
    cchTextMax: Integer;
    iSubItem: Integer;
    iImage: Integer;
    iOrder: Integer;
  end;
  {$EXTERNALSYM tagLVCOLUMN}
  tagLVCOLUMN = tagLVCOLUMNA;
  {$EXTERNALSYM _LV_COLUMNA}
  _LV_COLUMNA = tagLVCOLUMNA;
  {$EXTERNALSYM _LV_COLUMNW}
  _LV_COLUMNW = tagLVCOLUMNW;
  {$EXTERNALSYM _LV_COLUMN}
  _LV_COLUMN = _LV_COLUMNA;
  TLVColumnA = tagLVCOLUMNA;
  TLVColumnW = tagLVCOLUMNW;
  TLVColumn = TLVColumnA;
  {$EXTERNALSYM LV_COLUMNA}
  LV_COLUMNA = tagLVCOLUMNA;
  {$EXTERNALSYM LV_COLUMNW}
  LV_COLUMNW = tagLVCOLUMNW;
  {$EXTERNALSYM LV_COLUMN}
  LV_COLUMN = LV_COLUMNA;

const
  {$EXTERNALSYM LVCF_FMT}
  LVCF_FMT                = $0001;
  {$EXTERNALSYM LVCF_WIDTH}
  LVCF_WIDTH              = $0002;
  {$EXTERNALSYM LVCF_TEXT}
  LVCF_TEXT               = $0004;
  {$EXTERNALSYM LVCF_SUBITEM}
  LVCF_SUBITEM            = $0008;
  {$EXTERNALSYM LVCF_IMAGE}
  LVCF_IMAGE              = $0010;
  {$EXTERNALSYM LVCF_ORDER}
  LVCF_ORDER              = $0020;

  {$EXTERNALSYM LVCFMT_LEFT}
  LVCFMT_LEFT             = $0000; 
  {$EXTERNALSYM LVCFMT_RIGHT}
  LVCFMT_RIGHT            = $0001;
  {$EXTERNALSYM LVCFMT_CENTER}
  LVCFMT_CENTER           = $0002; 
  {$EXTERNALSYM LVCFMT_JUSTIFYMASK}
  LVCFMT_JUSTIFYMASK      = $0003;
  {$EXTERNALSYM LVCFMT_IMAGE}
  LVCFMT_IMAGE            = $0800;
  {$EXTERNALSYM LVCFMT_BITMAP_ON_RIGHT}
  LVCFMT_BITMAP_ON_RIGHT  = $1000;
  {$EXTERNALSYM LVCFMT_COL_HAS_IMAGES}
  LVCFMT_COL_HAS_IMAGES   = $8000;

  {$EXTERNALSYM LVM_GETCOLUMNA}
  LVM_GETCOLUMNA          = LVM_FIRST + 25;
  {$EXTERNALSYM LVM_GETCOLUMNW}
  LVM_GETCOLUMNW          = LVM_FIRST + 95;




  {$EXTERNALSYM LVM_GETCOLUMN}
  LVM_GETCOLUMN           = LVM_GETCOLUMNA;


{$EXTERNALSYM ListView_GetColumn}
function ListView_GetColumn(hwnd: HWND; iCol: Integer;
  var pcol: TLVColumn): Bool; inline;
{$EXTERNALSYM ListView_GetColumnA}
function ListView_GetColumnA(hwnd: HWND; iCol: Integer;
  var pcol: TLVColumnA): Bool; inline;
{$EXTERNALSYM ListView_GetColumnW}
function ListView_GetColumnW(hwnd: HWND; iCol: Integer;
  var pcol: TLVColumnW): Bool; inline;

const
  {$EXTERNALSYM LVM_SETCOLUMNA}
  LVM_SETCOLUMNA           = LVM_FIRST + 26;
  {$EXTERNALSYM LVM_SETCOLUMNW}
  LVM_SETCOLUMNW           = LVM_FIRST + 96;




  {$EXTERNALSYM LVM_SETCOLUMN}
  LVM_SETCOLUMN           = LVM_SETCOLUMNA;


{$EXTERNALSYM ListView_SetColumn}
function ListView_SetColumn(hwnd: HWnd; iCol: Integer; const pcol: TLVColumn): Bool; inline;
{$EXTERNALSYM ListView_SetColumnA}
function ListView_SetColumnA(hwnd: HWnd; iCol: Integer; const pcol: TLVColumnA): Bool; inline;
{$EXTERNALSYM ListView_SetColumnW}
function ListView_SetColumnW(hwnd: HWnd; iCol: Integer; const pcol: TLVColumnW): Bool; inline;

const
  {$EXTERNALSYM LVM_INSERTCOLUMNA}
  LVM_INSERTCOLUMNA        = LVM_FIRST + 27;
  {$EXTERNALSYM LVM_INSERTCOLUMNW}
  LVM_INSERTCOLUMNW        = LVM_FIRST + 97;




  {$EXTERNALSYM LVM_INSERTCOLUMN}
  LVM_INSERTCOLUMN        = LVM_INSERTCOLUMNA;


{$EXTERNALSYM ListView_InsertColumn}
function ListView_InsertColumn(hwnd: HWND; iCol: Integer;
  const pcol: TLVColumn): Integer; inline;
{$EXTERNALSYM ListView_InsertColumnA}
function ListView_InsertColumnA(hwnd: HWND; iCol: Integer;
  const pcol: TLVColumnA): Integer; inline;
{$EXTERNALSYM ListView_InsertColumnW}
function ListView_InsertColumnW(hwnd: HWND; iCol: Integer;
  const pcol: TLVColumnW): Integer; inline;

const
  {$EXTERNALSYM LVM_DELETECOLUMN}
  LVM_DELETECOLUMN        = LVM_FIRST + 28;

{$EXTERNALSYM ListView_DeleteColumn}
function ListView_DeleteColumn(hwnd: HWND; iCol: Integer): Bool; inline;

const
  {$EXTERNALSYM LVM_GETCOLUMNWIDTH}
  LVM_GETCOLUMNWIDTH      = LVM_FIRST + 29;

{$EXTERNALSYM ListView_GetColumnWidth}
function ListView_GetColumnWidth(hwnd: HWND; iCol: Integer): Integer; inline;

const
  {$EXTERNALSYM LVSCW_AUTOSIZE}
  LVSCW_AUTOSIZE              = -1;
  {$EXTERNALSYM LVSCW_AUTOSIZE_USEHEADER}
  LVSCW_AUTOSIZE_USEHEADER    = -2;
  {$EXTERNALSYM LVM_SETCOLUMNWIDTH}
  LVM_SETCOLUMNWIDTH          = LVM_FIRST + 30;

{$EXTERNALSYM ListView_SetColumnWidth}
function ListView_SetColumnWidth(hwnd: HWnd; iCol: Integer; cx: Integer): Bool;

const
  {$EXTERNALSYM LVM_GETHEADER}
  LVM_GETHEADER               = LVM_FIRST + 31;

{$EXTERNALSYM ListView_GetHeader}
function ListView_GetHeader(hwnd: HWND): HWND;

const
  {$EXTERNALSYM LVM_CREATEDRAGIMAGE}
  LVM_CREATEDRAGIMAGE     = LVM_FIRST + 33;

{$EXTERNALSYM ListView_CreateDragImage}
function ListView_CreateDragImage(hwnd: HWND; i: Integer;
  const lpptUpLeft: TPoint): HIMAGELIST; inline;

const
  {$EXTERNALSYM LVM_GETVIEWRECT}
  LVM_GETVIEWRECT         = LVM_FIRST + 34;

{$EXTERNALSYM ListView_GetViewRect}
function ListView_GetViewRect(hwnd: HWND; var prc: TRect): Bool; inline;

const
  {$EXTERNALSYM LVM_GETTEXTCOLOR}
  LVM_GETTEXTCOLOR        = LVM_FIRST + 35;

{$EXTERNALSYM ListView_GetTextColor}
function ListView_GetTextColor(hwnd: HWND): TColorRef; inline;

const
  {$EXTERNALSYM LVM_SETTEXTCOLOR}
  LVM_SETTEXTCOLOR        = LVM_FIRST + 36;

{$EXTERNALSYM ListView_SetTextColor}
function ListView_SetTextColor(hwnd: HWND; clrText: TColorRef): Bool; inline;

const
  {$EXTERNALSYM LVM_GETTEXTBKCOLOR}
  LVM_GETTEXTBKCOLOR      = LVM_FIRST + 37;

{$EXTERNALSYM ListView_GetTextBkColor}
function ListView_GetTextBkColor(hwnd: HWND): TColorRef; inline;

const
  {$EXTERNALSYM LVM_SETTEXTBKCOLOR}
  LVM_SETTEXTBKCOLOR      = LVM_FIRST + 38;

{$EXTERNALSYM ListView_SetTextBkColor}
function ListView_SetTextBkColor(hwnd: HWND; clrTextBk: TColorRef): Bool; inline;

const
  {$EXTERNALSYM LVM_GETTOPINDEX}
  LVM_GETTOPINDEX         = LVM_FIRST + 39;

{$EXTERNALSYM ListView_GetTopIndex}
function ListView_GetTopIndex(hwndLV: HWND): Integer; inline;

const
  {$EXTERNALSYM LVM_GETCOUNTPERPAGE}
  LVM_GETCOUNTPERPAGE     = LVM_FIRST + 40;

{$EXTERNALSYM ListView_GetCountPerPage}
function ListView_GetCountPerPage(hwndLV: HWND): Integer; inline;

const
  {$EXTERNALSYM LVM_GETORIGIN}
  LVM_GETORIGIN           = LVM_FIRST + 41;

{$EXTERNALSYM ListView_GetOrigin}
function ListView_GetOrigin(hwndLV: HWND; var ppt: TPoint): Bool; inline;

const
  {$EXTERNALSYM LVM_UPDATE}
  LVM_UPDATE              = LVM_FIRST + 42;

{$EXTERNALSYM ListView_Update}
function ListView_Update(hwndLV: HWND; i: Integer): Bool; inline;

const
  {$EXTERNALSYM LVM_SETITEMSTATE}
  LVM_SETITEMSTATE        = LVM_FIRST + 43;

{$EXTERNALSYM ListView_SetItemState}
function ListView_SetItemState(hwndLV: HWND; i: Integer; data, mask: UINT): Bool;

const
  {$EXTERNALSYM LVM_GETITEMSTATE}
  LVM_GETITEMSTATE        = LVM_FIRST + 44;

{$EXTERNALSYM ListView_GetItemState}
function ListView_GetItemState(hwndLV: HWND; i, mask: Integer): Integer; inline;

{$EXTERNALSYM ListView_GetCheckState}
function ListView_GetCheckState(hwndLV: HWND; i: Integer): UINT; inline;
{$EXTERNALSYM ListView_SetCheckState}
procedure ListView_SetCheckState(hwndLV: HWND; i: Integer; Checked: Boolean);

const
  {$EXTERNALSYM LVM_GETITEMTEXTA}
  LVM_GETITEMTEXTA         = LVM_FIRST + 45;
  {$EXTERNALSYM LVM_GETITEMTEXTW}
  LVM_GETITEMTEXTW         = LVM_FIRST + 115;




  {$EXTERNALSYM LVM_GETITEMTEXT}
  LVM_GETITEMTEXT         = LVM_GETITEMTEXTA;


{$EXTERNALSYM ListView_GetItemText}
function ListView_GetItemText(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PChar; cchTextMax: Integer): Integer;
{$EXTERNALSYM ListView_GetItemTextA}
function ListView_GetItemTextA(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PAnsiChar; cchTextMax: Integer): Integer;
{$EXTERNALSYM ListView_GetItemTextW}
function ListView_GetItemTextW(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PWideChar; cchTextMax: Integer): Integer;

const
  {$EXTERNALSYM LVM_SETITEMTEXTA}
  LVM_SETITEMTEXTA         = LVM_FIRST + 46;
  {$EXTERNALSYM LVM_SETITEMTEXTW}
  LVM_SETITEMTEXTW         = LVM_FIRST + 116;




  {$EXTERNALSYM LVM_SETITEMTEXT}
  LVM_SETITEMTEXT         = LVM_SETITEMTEXTA;


{$EXTERNALSYM ListView_SetItemText}
function ListView_SetItemText(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PChar): Bool;
{$EXTERNALSYM ListView_SetItemTextA}
function ListView_SetItemTextA(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PAnsiChar): Bool;
{$EXTERNALSYM ListView_SetItemTextW}
function ListView_SetItemTextW(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PWideChar): Bool;

const
  // these flags only apply to LVS_OWNERDATA listviews in report or list mode
  {$EXTERNALSYM LVSICF_NOINVALIDATEALL}
  LVSICF_NOINVALIDATEALL  = $00000001;
  {$EXTERNALSYM LVSICF_NOSCROLL}
  LVSICF_NOSCROLL         = $00000002;

  {$EXTERNALSYM LVM_SETITEMCOUNT}
  LVM_SETITEMCOUNT        = LVM_FIRST + 47;

{$EXTERNALSYM ListView_SetItemCount}
procedure ListView_SetItemCount(hwndLV: HWND; cItems: Integer); inline;

{$EXTERNALSYM ListView_SetItemCountEx}
procedure ListView_SetItemCountEx(hwndLV: HWND; cItems: Integer; dwFlags: DWORD); inline;

type
  {$EXTERNALSYM PFNLVCOMPARE}
  PFNLVCOMPARE = function(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
  TLVCompare = PFNLVCOMPARE;

const
  {$EXTERNALSYM LVM_SORTITEMS}
  LVM_SORTITEMS           = LVM_FIRST + 48;

{$EXTERNALSYM ListView_SortItems}
function ListView_SortItems(hwndLV: HWND; pfnCompare: TLVCompare;
  lPrm: Longint): Bool; inline;

const
  {$EXTERNALSYM LVM_SETITEMPOSITION32}
  LVM_SETITEMPOSITION32   = LVM_FIRST + 49;

{$EXTERNALSYM ListView_SetItemPosition32}
procedure ListView_SetItemPosition32(hwndLV: HWND; i, x, y: Integer);

const
  {$EXTERNALSYM LVM_GETSELECTEDCOUNT}
  LVM_GETSELECTEDCOUNT    = LVM_FIRST + 50;

{$EXTERNALSYM ListView_GetSelectedCount}
function ListView_GetSelectedCount(hwndLV: HWND): UINT; inline;

const
  {$EXTERNALSYM LVM_GETITEMSPACING}
  LVM_GETITEMSPACING      = LVM_FIRST + 51;

{$EXTERNALSYM ListView_GetItemSpacing}
function ListView_GetItemSpacing(hwndLV: HWND; fSmall: Integer): Longint; inline;

const
  {$EXTERNALSYM LVM_GETISEARCHSTRINGA}
  LVM_GETISEARCHSTRINGA    = LVM_FIRST + 52;
  {$EXTERNALSYM LVM_GETISEARCHSTRINGW}
  LVM_GETISEARCHSTRINGW    = LVM_FIRST + 117;




  {$EXTERNALSYM LVM_GETISEARCHSTRING}
  LVM_GETISEARCHSTRING    = LVM_GETISEARCHSTRINGA;


{$EXTERNALSYM ListView_GetISearchString}
function ListView_GetISearchString(hwndLV: HWND; lpsz: PChar): Bool; inline;
{$EXTERNALSYM ListView_GetISearchStringA}
function ListView_GetISearchStringA(hwndLV: HWND; lpsz: PAnsiChar): Bool; inline;
{$EXTERNALSYM ListView_GetISearchStringW}
function ListView_GetISearchStringW(hwndLV: HWND; lpsz: PWideChar): Bool; inline;

const
  {$EXTERNALSYM LVM_SETICONSPACING}
  LVM_SETICONSPACING      = LVM_FIRST + 53;

// -1 for cx and cy means we'll use the default (system settings)
// 0 for cx or cy means use the current setting (allows you to change just one param)
{$EXTERNALSYM ListView_SetIconSpacing}
function ListView_SetIconSpacing(hwndLV: HWND; cx, cy: Word): DWORD;

const
  {$EXTERNALSYM LVM_SETEXTENDEDLISTVIEWSTYLE}
  LVM_SETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 54;

{$EXTERNALSYM ListView_SetExtendedListViewStyle}
function ListView_SetExtendedListViewStyle(hwndLV: HWND; dw: DWORD): BOOL; inline;

const
  {$EXTERNALSYM LVM_GETEXTENDEDLISTVIEWSTYLE}
  LVM_GETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 55;

{$EXTERNALSYM ListView_GetExtendedListViewStyle}
function ListView_GetExtendedListViewStyle(hwndLV: HWND): DWORD; inline;

const
  {$EXTERNALSYM LVM_GETSUBITEMRECT}
  LVM_GETSUBITEMRECT      = LVM_FIRST + 56;

{$EXTERNALSYM ListView_GetSubItemRect}
function ListView_GetSubItemRect(hwndLV: HWND; iItem, iSubItem: Integer;
  code: DWORD; prc: PRect): BOOL;

const
  {$EXTERNALSYM LVM_SUBITEMHITTEST}
  LVM_SUBITEMHITTEST      = LVM_FIRST + 57;

{$EXTERNALSYM ListView_SubItemHitTest}
function ListView_SubItemHitTest(hwndLV: HWND; plvhti: PLVHitTestInfo): Integer; inline;

const
  {$EXTERNALSYM LVM_SETCOLUMNORDERARRAY}
  LVM_SETCOLUMNORDERARRAY = LVM_FIRST + 58;

{$EXTERNALSYM ListView_SetColumnOrderArray}
function ListView_SetColumnOrderArray(hwndLV: HWND; iCount: Integer;
  pi: PInteger): BOOL; inline;

const
  {$EXTERNALSYM LVM_GETCOLUMNORDERARRAY}
  LVM_GETCOLUMNORDERARRAY = LVM_FIRST + 59;

{$EXTERNALSYM ListView_GetColumnOrderArray}
function ListView_GetColumnOrderArray(hwndLV: HWND; iCount: Integer;
  pi: PInteger): BOOL; inline;

const
  {$EXTERNALSYM LVM_SETHOTITEM}
  LVM_SETHOTITEM  = LVM_FIRST + 60;

{$EXTERNALSYM ListView_SetHotItem}
function ListView_SetHotItem(hwndLV: HWND; i: Integer): Integer; inline;

const
  {$EXTERNALSYM LVM_GETHOTITEM}
  LVM_GETHOTITEM  = LVM_FIRST + 61;

{$EXTERNALSYM ListView_GetHotItem}
function ListView_GetHotItem(hwndLV: HWND): Integer; inline;

const
  {$EXTERNALSYM LVM_SETHOTCURSOR}
  LVM_SETHOTCURSOR  = LVM_FIRST + 62;

{$EXTERNALSYM ListView_SetHotCursor}
function ListView_SetHotCursor(hwndLV: HWND; hcur: HCURSOR): HCURSOR; inline;

const
  {$EXTERNALSYM LVM_GETHOTCURSOR}
  LVM_GETHOTCURSOR  = LVM_FIRST + 63;

{$EXTERNALSYM ListView_GetHotCursor}
function ListView_GetHotCursor(hwndLV: HWND): HCURSOR; inline;

const
  {$EXTERNALSYM LVM_APPROXIMATEVIEWRECT}
  LVM_APPROXIMATEVIEWRECT = LVM_FIRST + 64;

{$EXTERNALSYM ListView_ApproximateViewRect}
function ListView_ApproximateViewRect(hwndLV: HWND; iWidth, iHeight: Word;
  iCount: Integer): DWORD;

const
  {$EXTERNALSYM LV_MAX_WORKAREAS}
  LV_MAX_WORKAREAS        = 16;
  {$EXTERNALSYM LVM_SETWORKAREA}
  LVM_SETWORKAREA         = LVM_FIRST + 65;

{$EXTERNALSYM ListView_SetWorkAreas}
function ListView_SetWorkAreas(hwndLV: HWND; nWorkAreas: Integer; prc: PRect): BOOL; inline;

const
  {$EXTERNALSYM LVM_GETSELECTIONMARK}
  LVM_GETSELECTIONMARK    = LVM_FIRST + 66;

{$EXTERNALSYM ListView_GetSelectionMark}
function ListView_GetSelectionMark(hwnd: HWND): Integer; inline;

const
  {$EXTERNALSYM LVM_SETSELECTIONMARK}
  LVM_SETSELECTIONMARK    = LVM_FIRST + 67;

{$EXTERNALSYM ListView_SetSelectionMark}
function ListView_SetSelectionMark(hwnd: HWND; i: Integer): Integer; inline;

const
  {$EXTERNALSYM LVM_GETWORKAREAS}
  LVM_GETWORKAREAS        = LVM_FIRST + 70;

{$EXTERNALSYM ListView_GetWorkAreas}
function ListView_GetWorkAreas(hwnd: HWND; nWorkAreas: Integer; prc: PRect): BOOL; inline;

const
  {$EXTERNALSYM LVM_SETHOVERTIME}
  LVM_SETHOVERTIME        = LVM_FIRST + 71;

{$EXTERNALSYM ListView_SetHoverTime}
function ListView_SetHoverTime(hwndLV: HWND; dwHoverTimeMs: DWORD): DWORD; inline;

const
  {$EXTERNALSYM LVM_GETHOVERTIME}
  LVM_GETHOVERTIME        = LVM_FIRST + 72;

{$EXTERNALSYM ListView_GetHoverTime}
function ListView_GetHoverTime(hwndLV: HWND): Integer; inline;

const
  {$EXTERNALSYM LVM_GETNUMBEROFWORKAREAS}
  LVM_GETNUMBEROFWORKAREAS  = LVM_FIRST + 73;

{$EXTERNALSYM ListView_GetNumberOfWorkAreas}
function ListView_GetNumberOfWorkAreas(hwnd: HWND; pnWorkAreas: PInteger): Integer; inline;

const
  {$EXTERNALSYM LVM_SETTOOLTIPS}
  LVM_SETTOOLTIPS       = LVM_FIRST + 74;

{$EXTERNALSYM ListView_SetToolTips}
function ListView_SetToolTips(hwndLV: HWND; hwndNewHwnd: HWND): HWND; inline;

const
  {$EXTERNALSYM LVM_GETTOOLTIPS}
  LVM_GETTOOLTIPS       = LVM_FIRST + 78;

{$EXTERNALSYM ListView_GetToolTips}
function ListView_GetToolTips(hwndLV: HWND): HWND; inline;

type
{$EXTERNALSYM tagLVBKIMAGEA}
  tagLVBKIMAGEA = packed record
    ulFlags: ULONG;              // LVBKIF_*
    hbm: HBITMAP;
    pszImage: PAnsiChar;
    cchImageMax: UINT;
    xOffsetPercent: Integer;
    yOffsetPercent: Integer;
  end;
{$EXTERNALSYM tagLVBKIMAGEW}
  tagLVBKIMAGEW = packed record
    ulFlags: ULONG;              // LVBKIF_*
    hbm: HBITMAP;
    pszImage: PWideChar;
    cchImageMax: UINT;
    xOffsetPercent: Integer;
    yOffsetPercent: Integer;
  end;
  {$EXTERNALSYM tagLVBKIMAGE}
  tagLVBKIMAGE = tagLVBKIMAGEA;
  PLVBKImageA = ^TLVBKImageA;
  PLVBKImageW = ^TLVBKImageW;
  PLVBKImage = PLVBKImageA;
  TLVBKImageA = tagLVBKIMAGEA;
  TLVBKImageW = tagLVBKIMAGEW;
  TLVBKImage = TLVBKImageA;

const
  {$EXTERNALSYM LVBKIF_SOURCE_NONE}
  LVBKIF_SOURCE_NONE      = $00000000;
  {$EXTERNALSYM LVBKIF_SOURCE_HBITMAP}
  LVBKIF_SOURCE_HBITMAP   = $00000001;
  {$EXTERNALSYM LVBKIF_SOURCE_URL}
  LVBKIF_SOURCE_URL       = $00000002;
  {$EXTERNALSYM LVBKIF_SOURCE_MASK}
  LVBKIF_SOURCE_MASK      = $00000003;
  {$EXTERNALSYM LVBKIF_STYLE_NORMAL}
  LVBKIF_STYLE_NORMAL     = $00000000;
  {$EXTERNALSYM LVBKIF_STYLE_TILE}
  LVBKIF_STYLE_TILE       = $00000010;
  {$EXTERNALSYM LVBKIF_STYLE_MASK}
  LVBKIF_STYLE_MASK       = $00000010;

  {$EXTERNALSYM LVM_SETBKIMAGEA}
  LVM_SETBKIMAGEA         = LVM_FIRST + 68;
  {$EXTERNALSYM LVM_SETBKIMAGEW}
  LVM_SETBKIMAGEW         = LVM_FIRST + 138;
  {$EXTERNALSYM LVM_GETBKIMAGEA}
  LVM_GETBKIMAGEA         = LVM_FIRST + 69;
  {$EXTERNALSYM LVM_GETBKIMAGEW}
  LVM_GETBKIMAGEW         = LVM_FIRST + 139;







  {$EXTERNALSYM LVM_SETBKIMAGE}
  LVM_SETBKIMAGE = LVM_SETBKIMAGEA;
  {$EXTERNALSYM LVM_GETBKIMAGE}
  LVM_GETBKIMAGE = LVM_GETBKIMAGEA;


{$EXTERNALSYM ListView_SetBkImage}
function ListView_SetBkImage(hwnd: HWND; plvbki: PLVBKImage): BOOL; inline;
{$EXTERNALSYM ListView_SetBkImage}
function ListView_SetBkImageA(hwnd: HWND; plvbki: PLVBKImageA): BOOL; inline;
{$EXTERNALSYM ListView_SetBkImage}
function ListView_SetBkImageW(hwnd: HWND; plvbki: PLVBKImageW): BOOL; inline;

{$EXTERNALSYM ListView_GetBkImage}
function ListView_GetBkImage(hwnd: HWND; plvbki: PLVBKImage): BOOL; inline;
{$EXTERNALSYM ListView_GetBkImage}
function ListView_GetBkImageA(hwnd: HWND; plvbki: PLVBKImageA): BOOL; inline;
{$EXTERNALSYM ListView_GetBkImage}
function ListView_GetBkImageW(hwnd: HWND; plvbki: PLVBKImageW): BOOL; inline;

type
  {$EXTERNALSYM tagNMLISTVIEW}
  tagNMLISTVIEW = packed record
    hdr: TNMHDR;
    iItem: Integer;
    iSubItem: Integer;
    uNewState: UINT;
    uOldState: UINT;
    uChanged: UINT;
    ptAction: TPoint;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM _NM_LISTVIEW}
  _NM_LISTVIEW = tagNMLISTVIEW;
  {$EXTERNALSYM NM_LISTVIEW}
  NM_LISTVIEW = tagNMLISTVIEW;
  PNMListView = ^TNMListView;
  TNMListView = tagNMLISTVIEW;

  // NMITEMACTIVATE is used instead of NMLISTVIEW in IE >= 0x400
  // therefore all the fields are the same except for extra uKeyFlags
  // they are used to store key flags at the time of the single click with
  // delayed activation - because by the time the timer goes off a user may
  // not hold the keys (shift, ctrl) any more
  {$EXTERNALSYM tagNMITEMACTIVATE}
  tagNMITEMACTIVATE = packed record
    hdr: TNMHdr;
    iItem: Integer;
    iSubItem: Integer;
    uNewState: UINT;
    uOldState: UINT;
    uChanged: UINT;
    ptAction: TPoint;
    lParam: LPARAM;
    uKeyFlags: UINT;
  end;
  PNMItemActivate = ^TNMItemActivate;
  TNMItemActivate = tagNMITEMACTIVATE;

const
  // key flags stored in uKeyFlags
  {$EXTERNALSYM LVKF_ALT}
  LVKF_ALT       = $0001;
  {$EXTERNALSYM LVKF_CONTROL}
  LVKF_CONTROL   = $0002;
  {$EXTERNALSYM LVKF_SHIFT}
  LVKF_SHIFT     = $0004;

type
  {$EXTERNALSYM tagNMLVCUSTOMDRAW}
  tagNMLVCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    clrText: COLORREF;
    clrTextBk: COLORREF;
    iSubItem: Integer;
  end;
  PNMLVCustomDraw = ^TNMLVCustomDraw;
  TNMLVCustomDraw = tagNMLVCUSTOMDRAW;

  {$EXTERNALSYM tagNMLVCACHEHINT}
  tagNMLVCACHEHINT = packed record
    hdr: TNMHDR;
    iFrom: Integer;
    iTo: Integer;
  end;
  PNMLVCacheHint = ^TNMLVCacheHint;
  TNMLVCacheHint = tagNMLVCACHEHINT;
  PNMCacheHint = ^TNMCacheHint;
  TNMCacheHint = tagNMLVCACHEHINT;

  {$EXTERNALSYM tagNMLVFINDITEMA}
  tagNMLVFINDITEMA = packed record // WIN2K
    hdr: TNMHdr;
    iStart: Integer;
    lvfi: TLVFindInfoA;
  end;
  {$EXTERNALSYM tagNMLVFINDITEMW}
  tagNMLVFINDITEMW = packed record // WIN2K
    hdr: TNMHdr;
    iStart: Integer;
    lvfi: TLVFindInfoW;
  end;
  {$EXTERNALSYM tagNMLVFINDITEM}
  tagNMLVFINDITEM = tagNMLVFINDITEMA;
  PNMLVFinditemA = ^TNMLVFinditemA;
  TNMLVFinditemA = tagNMLVFINDITEMA; // WIN2K
  PNMLVFinditemW = ^TNMLVFinditemW;
  TNMLVFinditemW = tagNMLVFINDITEMW; // WIN2K
  PNMLVFinditem = PNMLVFinditemA;
  PNMFinditemA = ^TNMFinditemA;
  TNMFinditemA = tagNMLVFINDITEMA; // WIN2K
  PNMFinditemW = ^TNMFinditemW;
  TNMFinditemW = tagNMLVFINDITEMW; // WIN2K
  PNMFinditem = PNMFinditemA;

  {$EXTERNALSYM tagNMLVODSTATECHANGE}
  tagNMLVODSTATECHANGE = packed record
    hdr: TNMHdr;
    iFrom: Integer;
    iTo: Integer;
    uNewState: UINT;
    uOldState: UINT;
  end;
  PNMLVODStateChange = ^TNMLVODStateChange;
  TNMLVODStateChange = tagNMLVODSTATECHANGE;

const
  {$EXTERNALSYM LVN_ITEMCHANGING}
  LVN_ITEMCHANGING        = LVN_FIRST-0;
  {$EXTERNALSYM LVN_ITEMCHANGED}
  LVN_ITEMCHANGED         = LVN_FIRST-1;
  {$EXTERNALSYM LVN_INSERTITEM}
  LVN_INSERTITEM          = LVN_FIRST-2;
  {$EXTERNALSYM LVN_DELETEITEM}
  LVN_DELETEITEM          = LVN_FIRST-3;
  {$EXTERNALSYM LVN_DELETEALLITEMS}
  LVN_DELETEALLITEMS      = LVN_FIRST-4;
  {$EXTERNALSYM LVN_COLUMNCLICK}
  LVN_COLUMNCLICK         = LVN_FIRST-8;
  {$EXTERNALSYM LVN_BEGINDRAG}
  LVN_BEGINDRAG           = LVN_FIRST-9;
  {$EXTERNALSYM LVN_BEGINRDRAG}
  LVN_BEGINRDRAG          = LVN_FIRST-11;

  {$EXTERNALSYM LVN_ODCACHEHINT}
  LVN_ODCACHEHINT         = LVN_FIRST-13;
  {$EXTERNALSYM LVN_ODFINDITEMA}
  LVN_ODFINDITEMA         = LVN_FIRST-52;
  {$EXTERNALSYM LVN_ODFINDITEMW}
  LVN_ODFINDITEMW         = LVN_FIRST-79;

  {$EXTERNALSYM LVN_ITEMACTIVATE}
  LVN_ITEMACTIVATE        = LVN_FIRST-14;
  {$EXTERNALSYM LVN_ODSTATECHANGED}
  LVN_ODSTATECHANGED      = LVN_FIRST-15;





  {$EXTERNALSYM LVN_ODFINDITEM}
  LVN_ODFINDITEM          = LVN_ODFINDITEMA; 


  {$EXTERNALSYM LVN_BEGINLABELEDITA}
  LVN_BEGINLABELEDITA      = LVN_FIRST-5;
  {$EXTERNALSYM LVN_ENDLABELEDITA}
  LVN_ENDLABELEDITA        = LVN_FIRST-6;
  {$EXTERNALSYM LVN_BEGINLABELEDITW}
  LVN_BEGINLABELEDITW      = LVN_FIRST-75;
  {$EXTERNALSYM LVN_ENDLABELEDITW}
  LVN_ENDLABELEDITW        = LVN_FIRST-76;






  {$EXTERNALSYM LVN_BEGINLABELEDIT}
  LVN_BEGINLABELEDIT      = LVN_BEGINLABELEDITA;
  {$EXTERNALSYM LVN_ENDLABELEDIT}
  LVN_ENDLABELEDIT        = LVN_ENDLABELEDITA;


  {$EXTERNALSYM LVN_HOTTRACK}
  LVN_HOTTRACK            = LVN_FIRST-21;
  
  {$EXTERNALSYM LVN_GETDISPINFOA}
  LVN_GETDISPINFOA        = LVN_FIRST-50;
  {$EXTERNALSYM LVN_SETDISPINFOA}
  LVN_SETDISPINFOA        = LVN_FIRST-51;
  {$EXTERNALSYM LVN_GETDISPINFOW}
  LVN_GETDISPINFOW        = LVN_FIRST-77;
  {$EXTERNALSYM LVN_SETDISPINFOW}
  LVN_SETDISPINFOW        = LVN_FIRST-78;






  {$EXTERNALSYM LVN_GETDISPINFO}
  LVN_GETDISPINFO        = LVN_GETDISPINFOA;
  {$EXTERNALSYM LVN_SETDISPINFO}
  LVN_SETDISPINFO        = LVN_SETDISPINFOA;


  {$EXTERNALSYM LVIF_DI_SETITEM}
  LVIF_DI_SETITEM         = $1000;

type
  PLVDispInfoA = ^TLVDispInfoA;
  PLVDispInfoW = ^TLVDispInfoW;
  PLVDispInfo = PLVDispInfoA;
  {$EXTERNALSYM tagLVDISPINFO}
  tagLVDISPINFO = packed record
    hdr: TNMHDR;
    item: TLVItemA;
  end;
  {$EXTERNALSYM _LV_DISPINFO}
  _LV_DISPINFO = tagLVDISPINFO;
  {$EXTERNALSYM tagLVDISPINFOW}
  tagLVDISPINFOW = packed record
    hdr: TNMHDR;
    item: TLVItemW;
  end;
  {$EXTERNALSYM _LV_DISPINFOW}
  _LV_DISPINFOW = tagLVDISPINFOW;
  TLVDispInfoA = tagLVDISPINFO;
  TLVDispInfoW = tagLVDISPINFOW;
  TLVDispInfo = TLVDispInfoA;
  {$EXTERNALSYM LV_DISPINFOA}
  LV_DISPINFOA = tagLVDISPINFO;
  {$EXTERNALSYM LV_DISPINFOW}
  LV_DISPINFOW = tagLVDISPINFOW;
  {$EXTERNALSYM LV_DISPINFO}
  LV_DISPINFO = LV_DISPINFOA;

const
  {$EXTERNALSYM LVN_KEYDOWN}
  LVN_KEYDOWN             = LVN_FIRST-55;

type
  PLVKeyDown = ^TLVKeyDown;
  {$EXTERNALSYM tagLVKEYDOWN}
  tagLVKEYDOWN = packed record
    hdr: TNMHDR;
    wVKey: Word;
    flags: UINT;
  end;
  {$EXTERNALSYM _LV_KEYDOWN}
  _LV_KEYDOWN = tagLVKEYDOWN;
  TLVKeyDown = tagLVKEYDOWN;
  {$EXTERNALSYM LV_KEYDOWN}
  LV_KEYDOWN = tagLVKEYDOWN;

const
  {$EXTERNALSYM LVN_MARQUEEBEGIN}
  LVN_MARQUEEBEGIN        = LVN_FIRST-56;

type
  {$EXTERNALSYM tagNMLVGETINFOTIPA}
  tagNMLVGETINFOTIPA = packed record
    hdr: TNMHdr;
    dwFlags: DWORD;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iItem: Integer;
    iSubItem: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagNMLVGETINFOTIPW}
  tagNMLVGETINFOTIPW = packed record
    hdr: TNMHdr;
    dwFlags: DWORD;
    pszText: PWideChar;
    cchTextMax: Integer;
    iItem: Integer;
    iSubItem: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagNMLVGETINFOTIP}
  tagNMLVGETINFOTIP = tagNMLVGETINFOTIPA;
  PNMLVGetInfoTipA = ^TNMLVGetInfoTipA;
  PNMLVGetInfoTipW = ^TNMLVGetInfoTipW;
  PNMLVGetInfoTip = PNMLVGetInfoTipA;
  TNMLVGetInfoTipA = tagNMLVGETINFOTIPA;
  TNMLVGetInfoTipW = tagNMLVGETINFOTIPW;
  TNMLVGetInfoTip = TNMLVGetInfoTipA;

const
  // NMLVGETINFOTIPA.dwFlag values
  {$EXTERNALSYM LVGIT_UNFOLDED}
  LVGIT_UNFOLDED  = $0001;

  {$EXTERNALSYM LVN_GETINFOTIPA}
  LVN_GETINFOTIPA          = LVN_FIRST-57;
  {$EXTERNALSYM LVN_GETINFOTIPW}
  LVN_GETINFOTIPW          = LVN_FIRST-58;





  {$EXTERNALSYM LVN_GETINFOTIP}
  LVN_GETINFOTIP          = LVN_GETINFOTIPA;


{ ====== TREEVIEW CONTROL =================== }

const
  {$EXTERNALSYM WC_TREEVIEW}
  WC_TREEVIEW = 'SysTreeView32';

const
  {$EXTERNALSYM TVS_HASBUTTONS}
  TVS_HASBUTTONS          = $0001;
  {$EXTERNALSYM TVS_HASLINES}
  TVS_HASLINES            = $0002;
  {$EXTERNALSYM TVS_LINESATROOT}
  TVS_LINESATROOT         = $0004;
  {$EXTERNALSYM TVS_EDITLABELS}
  TVS_EDITLABELS          = $0008;
  {$EXTERNALSYM TVS_DISABLEDRAGDROP}
  TVS_DISABLEDRAGDROP     = $0010;
  {$EXTERNALSYM TVS_SHOWSELALWAYS}
  TVS_SHOWSELALWAYS       = $0020;
  {$EXTERNALSYM TVS_RTLREADING}
  TVS_RTLREADING          = $0040;
  {$EXTERNALSYM TVS_NOTOOLTIPS}
  TVS_NOTOOLTIPS          = $0080;
  {$EXTERNALSYM TVS_CHECKBOXES}
  TVS_CHECKBOXES          = $0100;
  {$EXTERNALSYM TVS_TRACKSELECT}
  TVS_TRACKSELECT         = $0200;
  {$EXTERNALSYM TVS_SINGLEEXPAND}
  TVS_SINGLEEXPAND        = $0400;
  {$EXTERNALSYM TVS_INFOTIP}
  TVS_INFOTIP             = $0800;
  {$EXTERNALSYM TVS_FULLROWSELECT}
  TVS_FULLROWSELECT       = $1000;
  {$EXTERNALSYM TVS_NOSCROLL}
  TVS_NOSCROLL            = $2000;
  {$EXTERNALSYM TVS_NONEVENHEIGHT}
  TVS_NONEVENHEIGHT       = $4000;

type
  {$EXTERNALSYM HTREEITEM}
  HTREEITEM = ^_TREEITEM;
  {$EXTERNALSYM _TREEITEM}
  _TREEITEM = packed record
  end;

const
  {$EXTERNALSYM TVIF_TEXT}
  TVIF_TEXT               = $0001;
  {$EXTERNALSYM TVIF_IMAGE}
  TVIF_IMAGE              = $0002;
  {$EXTERNALSYM TVIF_PARAM}
  TVIF_PARAM              = $0004;
  {$EXTERNALSYM TVIF_STATE}
  TVIF_STATE              = $0008;
  {$EXTERNALSYM TVIF_HANDLE}
  TVIF_HANDLE             = $0010;
  {$EXTERNALSYM TVIF_SELECTEDIMAGE}
  TVIF_SELECTEDIMAGE      = $0020;
  {$EXTERNALSYM TVIF_CHILDREN}
  TVIF_CHILDREN           = $0040;
  {$EXTERNALSYM TVIF_INTEGRAL}
  TVIF_INTEGRAL           = $0080;

  {$EXTERNALSYM TVIS_FOCUSED}
  TVIS_FOCUSED            = $0001;
  {$EXTERNALSYM TVIS_SELECTED}
  TVIS_SELECTED           = $0002;
  {$EXTERNALSYM TVIS_CUT}
  TVIS_CUT                = $0004;
  {$EXTERNALSYM TVIS_DROPHILITED}
  TVIS_DROPHILITED        = $0008;
  {$EXTERNALSYM TVIS_BOLD}
  TVIS_BOLD               = $0010;
  {$EXTERNALSYM TVIS_EXPANDED}
  TVIS_EXPANDED           = $0020;
  {$EXTERNALSYM TVIS_EXPANDEDONCE}
  TVIS_EXPANDEDONCE       = $0040;
  {$EXTERNALSYM TVIS_EXPANDPARTIAL}
  TVIS_EXPANDPARTIAL      = $0080;

  {$EXTERNALSYM TVIS_OVERLAYMASK}
  TVIS_OVERLAYMASK        = $0F00;
  {$EXTERNALSYM TVIS_STATEIMAGEMASK}
  TVIS_STATEIMAGEMASK     = $F000;
  {$EXTERNALSYM TVIS_USERMASK}
  TVIS_USERMASK           = $F000;


const
  {$EXTERNALSYM I_CHILDRENCALLBACK}
  I_CHILDRENCALLBACK  = -1;

type
  PTVItemA = ^TTVItemA;
  PTVItemW = ^TTVItemW;
  PTVItem = PTVItemA;
  {$EXTERNALSYM tagTVITEMA}
  tagTVITEMA = packed record
    mask: UINT;
    hItem: HTreeItem;
    state: UINT;
    stateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    cChildren: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagTVITEMW}
  tagTVITEMW = packed record
    mask: UINT;
    hItem: HTreeItem;
    state: UINT;
    stateMask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    cChildren: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagTVITEM}
  tagTVITEM = tagTVITEMA;
  {$EXTERNALSYM _TV_ITEMA}
  _TV_ITEMA = tagTVITEMA;
  {$EXTERNALSYM _TV_ITEMW}
  _TV_ITEMW = tagTVITEMW;
  {$EXTERNALSYM _TV_ITEM}
  _TV_ITEM = _TV_ITEMA;
  TTVItemA = tagTVITEMA;
  TTVItemW = tagTVITEMW;
  TTVItem = TTVItemA;
  {$EXTERNALSYM TV_ITEMA}
  TV_ITEMA = tagTVITEMA;
  {$EXTERNALSYM TV_ITEMW}
  TV_ITEMW = tagTVITEMW;
  {$EXTERNALSYM TV_ITEM}
  TV_ITEM = TV_ITEMA;

  // only used for Get and Set messages.  no notifies
  {$EXTERNALSYM tagTVITEMEXA}
  tagTVITEMEXA = packed record
    mask: UINT;
    hItem: HTREEITEM;
    state: UINT;
    stateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    cChildren: Integer;
    lParam: LPARAM;
    iIntegral: Integer;
  end;
  {$EXTERNALSYM tagTVITEMEXW}
  tagTVITEMEXW = packed record
    mask: UINT;
    hItem: HTREEITEM;
    state: UINT;
    stateMask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    cChildren: Integer;
    lParam: LPARAM;
    iIntegral: Integer;
  end;
  {$EXTERNALSYM tagTVITEMEX}
  tagTVITEMEX = tagTVITEMEXA;
  PTVItemExA = ^TTVItemExA;
  PTVItemExW = ^TTVItemExW;
  PTVItemEx = PTVItemExA;
  TTVItemExA = tagTVITEMEXA;
  TTVItemExW = tagTVITEMEXW;
  TTVItemEx = TTVItemExA;

const
  {$EXTERNALSYM TVI_ROOT}
  TVI_ROOT                = HTreeItem($FFFF0000);
  {$EXTERNALSYM TVI_FIRST}
  TVI_FIRST               = HTreeItem($FFFF0001);
  {$EXTERNALSYM TVI_LAST}
  TVI_LAST                = HTreeItem($FFFF0002);
  {$EXTERNALSYM TVI_SORT}
  TVI_SORT                = HTreeItem($FFFF0003);

type
  PTVInsertStructA = ^TTVInsertStructA;
  PTVInsertStructW = ^TTVInsertStructW;
  PTVInsertStruct = PTVInsertStructA;
  {$EXTERNALSYM tagTVINSERTSTRUCTA}
  tagTVINSERTSTRUCTA = packed record
    hParent: HTreeItem;
    hInsertAfter: HTreeItem;
    case Integer of
      0: (itemex: TTVItemExA);
      1: (item: TTVItemA);
  end;
  {$EXTERNALSYM tagTVINSERTSTRUCTW}
  tagTVINSERTSTRUCTW = packed record
    hParent: HTreeItem;
    hInsertAfter: HTreeItem;
    case Integer of
      0: (itemex: TTVItemExW);
      1: (item: TTVItemW);
  end;
  {$EXTERNALSYM tagTVINSERTSTRUCT}
  tagTVINSERTSTRUCT = tagTVINSERTSTRUCTA;
  {$EXTERNALSYM _TV_INSERTSTRUCTA}
  _TV_INSERTSTRUCTA = tagTVINSERTSTRUCTA;
  {$EXTERNALSYM _TV_INSERTSTRUCTW}
  _TV_INSERTSTRUCTW = tagTVINSERTSTRUCTW;
  {$EXTERNALSYM _TV_INSERTSTRUCT}
  _TV_INSERTSTRUCT = _TV_INSERTSTRUCTA;
  TTVInsertStructA = tagTVINSERTSTRUCTA;
  TTVInsertStructW = tagTVINSERTSTRUCTW;
  TTVInsertStruct = TTVInsertStructA;
  {$EXTERNALSYM TV_INSERTSTRUCTA}
  TV_INSERTSTRUCTA = tagTVINSERTSTRUCTA;
  {$EXTERNALSYM TV_INSERTSTRUCTW}
  TV_INSERTSTRUCTW = tagTVINSERTSTRUCTW;
  {$EXTERNALSYM TV_INSERTSTRUCT}
  TV_INSERTSTRUCT = TV_INSERTSTRUCTA;

const
  {$EXTERNALSYM TVM_INSERTITEMA}
  TVM_INSERTITEMA          = TV_FIRST + 0;
  {$EXTERNALSYM TVM_INSERTITEMW}
  TVM_INSERTITEMW          = TV_FIRST + 50;




  {$EXTERNALSYM TVM_INSERTITEM}
  TVM_INSERTITEM          = TVM_INSERTITEMA;


{$EXTERNALSYM TreeView_InsertItem}
function TreeView_InsertItem(hwnd: HWND; const lpis: TTVInsertStruct): HTreeItem; inline;
function TreeView_InsertItemA(hwnd: HWND; const lpis: TTVInsertStructA): HTreeItem; inline;
function TreeView_InsertItemW(hwnd: HWND; const lpis: TTVInsertStructW): HTreeItem; inline;

const
  {$EXTERNALSYM TVM_DELETEITEM}
  TVM_DELETEITEM          = TV_FIRST + 1;

{$EXTERNALSYM TreeView_DeleteItem}
function TreeView_DeleteItem(hwnd: HWND; hitem: HTreeItem): Bool; inline;

{$EXTERNALSYM TreeView_DeleteAllItems}
function TreeView_DeleteAllItems(hwnd: HWND): Bool; inline;

const
  {$EXTERNALSYM TVM_EXPAND}
  TVM_EXPAND              = TV_FIRST + 2;

{$EXTERNALSYM TreeView_Expand}
function TreeView_Expand(hwnd: HWND; hitem: HTreeItem; code: Integer): Bool; inline;

const
  {$EXTERNALSYM TVE_COLLAPSE}
  TVE_COLLAPSE            = $0001;
  {$EXTERNALSYM TVE_EXPAND}
  TVE_EXPAND              = $0002;
  {$EXTERNALSYM TVE_TOGGLE}
  TVE_TOGGLE              = $0003;
  {$EXTERNALSYM TVE_EXPANDPARTIAL}
  TVE_EXPANDPARTIAL       = $4000;
  {$EXTERNALSYM TVE_COLLAPSERESET}
  TVE_COLLAPSERESET       = $8000;

const
  {$EXTERNALSYM TVM_GETITEMRECT}
  TVM_GETITEMRECT         = TV_FIRST + 4;

{$EXTERNALSYM TreeView_GetItemRect}
function TreeView_GetItemRect(hwnd: HWND; hitem: HTreeItem;
  var prc: TRect; code: Bool): Bool;

const
  {$EXTERNALSYM TVM_GETCOUNT}
  TVM_GETCOUNT            = TV_FIRST + 5;

{$EXTERNALSYM TreeView_GetCount}
function TreeView_GetCount(hwnd: HWND): UINT; inline;

const
  {$EXTERNALSYM TVM_GETINDENT}
  TVM_GETINDENT           = TV_FIRST + 6;

{$EXTERNALSYM TreeView_GetIndent}
function TreeView_GetIndent(hwnd: HWND): UINT; inline;

const
  {$EXTERNALSYM TVM_SETINDENT}
  TVM_SETINDENT           = TV_FIRST + 7;

{$EXTERNALSYM TreeView_SetIndent}
function TreeView_SetIndent(hwnd: HWND; indent: Integer): Bool; inline;

const
  {$EXTERNALSYM TVM_GETIMAGELIST}
  TVM_GETIMAGELIST        = TV_FIRST + 8;

{$EXTERNALSYM TreeView_GetImageList}
function TreeView_GetImageList(hwnd: HWND; iImage: Integer): HIMAGELIST; inline;

const
  {$EXTERNALSYM TVSIL_NORMAL}
  TVSIL_NORMAL            = 0;
  {$EXTERNALSYM TVSIL_STATE}
  TVSIL_STATE             = 2;


const
  {$EXTERNALSYM TVM_SETIMAGELIST}
  TVM_SETIMAGELIST        = TV_FIRST + 9;

{$EXTERNALSYM TreeView_SetImageList}
function TreeView_SetImageList(hwnd: HWND; himl: HIMAGELIST;
  iImage: Integer): HIMAGELIST; inline;

const
  {$EXTERNALSYM TVM_GETNEXTITEM}
  TVM_GETNEXTITEM         = TV_FIRST + 10;

{$EXTERNALSYM TreeView_GetNextItem}
function TreeView_GetNextItem(hwnd: HWND; hitem: HTreeItem;
  code: Integer): HTreeItem; inline;

const
  {$EXTERNALSYM TVGN_ROOT}
  TVGN_ROOT               = $0000;
  {$EXTERNALSYM TVGN_NEXT}
  TVGN_NEXT               = $0001;
  {$EXTERNALSYM TVGN_PREVIOUS}
  TVGN_PREVIOUS           = $0002;
  {$EXTERNALSYM TVGN_PARENT}
  TVGN_PARENT             = $0003;
  {$EXTERNALSYM TVGN_CHILD}
  TVGN_CHILD              = $0004;
  {$EXTERNALSYM TVGN_FIRSTVISIBLE}
  TVGN_FIRSTVISIBLE       = $0005;
  {$EXTERNALSYM TVGN_NEXTVISIBLE}
  TVGN_NEXTVISIBLE        = $0006;
  {$EXTERNALSYM TVGN_PREVIOUSVISIBLE}
  TVGN_PREVIOUSVISIBLE    = $0007;
  {$EXTERNALSYM TVGN_DROPHILITE}
  TVGN_DROPHILITE         = $0008;
  {$EXTERNALSYM TVGN_CARET}
  TVGN_CARET              = $0009;
  {$EXTERNALSYM TVGN_LASTVISIBLE}
  TVGN_LASTVISIBLE        = $000A;

{$EXTERNALSYM TreeView_GetChild}
function TreeView_GetChild(hwnd: HWND; hitem: HTreeItem): HTreeItem;
{$EXTERNALSYM TreeView_GetNextSibling}
function TreeView_GetNextSibling(hwnd: HWND; hitem: HTreeItem): HTreeItem;
{$EXTERNALSYM TreeView_GetPrevSibling}
function TreeView_GetPrevSibling(hwnd: HWND; hitem: HTreeItem): HTreeItem;
{$EXTERNALSYM TreeView_GetParent}
function TreeView_GetParent(hwnd: HWND; hitem: HTreeItem): HTreeItem;
{$EXTERNALSYM TreeView_GetFirstVisible}
function TreeView_GetFirstVisible(hwnd: HWND): HTreeItem;
{$EXTERNALSYM TreeView_GetNextVisible}
function TreeView_GetNextVisible(hwnd: HWND; hitem: HTreeItem): HTreeItem;
{$EXTERNALSYM TreeView_GetPrevVisible}
function TreeView_GetPrevVisible(hwnd: HWND; hitem: HTreeItem): HTreeItem;
{$EXTERNALSYM TreeView_GetSelection}
function TreeView_GetSelection(hwnd: HWND): HTreeItem;
{$EXTERNALSYM TreeView_GetDropHilite}
function TreeView_GetDropHilite(hwnd: HWND): HTreeItem;
{$EXTERNALSYM TreeView_GetRoot}
function TreeView_GetRoot(hwnd: HWND): HTreeItem;
{$EXTERNALSYM TreeView_GetLastVisible}
function TreeView_GetLastVisible(hwnd: HWND): HTreeItem;

const
  {$EXTERNALSYM TVM_SELECTITEM}
  TVM_SELECTITEM          = TV_FIRST + 11;

{$EXTERNALSYM TreeView_Select}
function TreeView_Select(hwnd: HWND; hitem: HTreeItem;
  code: Integer): HTreeItem; inline;

{$EXTERNALSYM TreeView_SelectItem}
function TreeView_SelectItem(hwnd: HWND; hitem: HTreeItem): HTreeItem;
{$EXTERNALSYM TreeView_SelectDropTarget}
function TreeView_SelectDropTarget(hwnd: HWND; hitem: HTreeItem): HTreeItem;
{$EXTERNALSYM TreeView_SelectSetFirstVisible}
function TreeView_SelectSetFirstVisible(hwnd: HWND; hitem: HTreeItem): HTreeItem;

const
  {$EXTERNALSYM TVM_GETITEMA}
  TVM_GETITEMA             = TV_FIRST + 12;
  {$EXTERNALSYM TVM_GETITEMW}
  TVM_GETITEMW             = TV_FIRST + 62;




  {$EXTERNALSYM TVM_GETITEM}
  TVM_GETITEM             = TVM_GETITEMA;


{$EXTERNALSYM TreeView_GetItem}
function TreeView_GetItem(hwnd: HWND; var pitem: TTVItem): Bool; inline;
{$EXTERNALSYM TreeView_GetItemA}
function TreeView_GetItemA(hwnd: HWND; var pitem: TTVItemA): Bool; inline;
{$EXTERNALSYM TreeView_GetItemW}
function TreeView_GetItemW(hwnd: HWND; var pitem: TTVItemW): Bool; inline;

const
  {$EXTERNALSYM TVM_SETITEMA}
  TVM_SETITEMA             = TV_FIRST + 13;
  {$EXTERNALSYM TVM_SETITEMW}
  TVM_SETITEMW             = TV_FIRST + 63;




  {$EXTERNALSYM TVM_SETITEM}
  TVM_SETITEM             = TVM_SETITEMA;


{$EXTERNALSYM TreeView_SetItem}
function TreeView_SetItem(hwnd: HWND; const pitem: TTVItem): Bool; inline;
{$EXTERNALSYM TreeView_SetItemA}
function TreeView_SetItemA(hwnd: HWND; const pitem: TTVItemA): Bool; inline;
{$EXTERNALSYM TreeView_SetItemW}
function TreeView_SetItemW(hwnd: HWND; const pitem: TTVItemW): Bool; inline;

const
  {$EXTERNALSYM TVM_EDITLABELA}
  TVM_EDITLABELA           = TV_FIRST + 14;
  {$EXTERNALSYM TVM_EDITLABELW}
  TVM_EDITLABELW           = TV_FIRST + 65;




  {$EXTERNALSYM TVM_EDITLABEL}
  TVM_EDITLABEL           = TVM_EDITLABELA;


{$EXTERNALSYM TreeView_EditLabel}
function TreeView_EditLabel(hwnd: HWND; hitem: HTreeItem): HWND; inline;
{$EXTERNALSYM TreeView_EditLabelA}
function TreeView_EditLabelA(hwnd: HWND; hitem: HTreeItem): HWND; inline;
{$EXTERNALSYM TreeView_EditLabelW}
function TreeView_EditLabelW(hwnd: HWND; hitem: HTreeItem): HWND; inline;

const
  {$EXTERNALSYM TVM_GETEDITCONTROL}
  TVM_GETEDITCONTROL      = TV_FIRST + 15;

{$EXTERNALSYM TreeView_GetEditControl}
function TreeView_GetEditControl(hwnd: HWND): HWND; inline;


const
  {$EXTERNALSYM TVM_GETVISIBLECOUNT}
  TVM_GETVISIBLECOUNT     = TV_FIRST + 16;

{$EXTERNALSYM TreeView_GetVisibleCount}
function TreeView_GetVisibleCount(hwnd: HWND): UINT; inline;

const
  {$EXTERNALSYM TVM_HITTEST}
  TVM_HITTEST             = TV_FIRST + 17;

type
  PTVHitTestInfo = ^TTVHitTestInfo;
  {$EXTERNALSYM tagTVHITTESTINFO}
  tagTVHITTESTINFO = packed record
    pt: TPoint;
    flags: UINT;
    hItem: HTreeItem;
  end;
  {$EXTERNALSYM _TV_HITTESTINFO}
  _TV_HITTESTINFO = tagTVHITTESTINFO;
  TTVHitTestInfo = tagTVHITTESTINFO;
  {$EXTERNALSYM TV_HITTESTINFO}
  TV_HITTESTINFO = tagTVHITTESTINFO;

{$EXTERNALSYM TreeView_HitTest}
function TreeView_HitTest(hwnd: HWND; var lpht: TTVHitTestInfo): HTreeItem; inline;

const
  {$EXTERNALSYM TVHT_NOWHERE}
  TVHT_NOWHERE            = $0001;
  {$EXTERNALSYM TVHT_ONITEMICON}
  TVHT_ONITEMICON         = $0002;
  {$EXTERNALSYM TVHT_ONITEMLABEL}
  TVHT_ONITEMLABEL        = $0004;
  {$EXTERNALSYM TVHT_ONITEMINDENT}
  TVHT_ONITEMINDENT       = $0008;
  {$EXTERNALSYM TVHT_ONITEMBUTTON}
  TVHT_ONITEMBUTTON       = $0010;
  {$EXTERNALSYM TVHT_ONITEMRIGHT}
  TVHT_ONITEMRIGHT        = $0020;
  {$EXTERNALSYM TVHT_ONITEMSTATEICON}
  TVHT_ONITEMSTATEICON    = $0040;

  {$EXTERNALSYM TVHT_ONITEM}
  TVHT_ONITEM             = TVHT_ONITEMICON or TVHT_ONITEMLABEL or
			      TVHT_ONITEMSTATEICON;

  {$EXTERNALSYM TVHT_ABOVE}
  TVHT_ABOVE              = $0100;
  {$EXTERNALSYM TVHT_BELOW}
  TVHT_BELOW              = $0200;
  {$EXTERNALSYM TVHT_TORIGHT}
  TVHT_TORIGHT            = $0400;
  {$EXTERNALSYM TVHT_TOLEFT}
  TVHT_TOLEFT             = $0800;

const
  {$EXTERNALSYM TVM_CREATEDRAGIMAGE}
  TVM_CREATEDRAGIMAGE     = TV_FIRST + 18;

{$EXTERNALSYM TreeView_CreateDragImage}
function TreeView_CreateDragImage(hwnd: HWND; hitem: HTreeItem): HIMAGELIST; inline;

const
  {$EXTERNALSYM TVM_SORTCHILDREN}
  TVM_SORTCHILDREN        = TV_FIRST + 19;

{$EXTERNALSYM TreeView_SortChildren}
function TreeView_SortChildren(hwnd: HWND; hitem: HTreeItem;
  recurse: Integer): Bool; inline;

const
  {$EXTERNALSYM TVM_ENSUREVISIBLE}
  TVM_ENSUREVISIBLE       = TV_FIRST + 20;

{$EXTERNALSYM TreeView_EnsureVisible}
function TreeView_EnsureVisible(hwnd: HWND; hitem: HTreeItem): Bool; inline;

const
  {$EXTERNALSYM TVM_SORTCHILDRENCB}
  TVM_SORTCHILDRENCB      = TV_FIRST + 21;

type
  {$EXTERNALSYM PFNTVCOMPARE}
  PFNTVCOMPARE = function(lParam1, lParam2, lParamSort: Longint): Integer stdcall;
  TTVCompare = PFNTVCOMPARE;

type
  {$EXTERNALSYM tagTVSORTCB}
  tagTVSORTCB = packed record
    hParent: HTreeItem;
    lpfnCompare: TTVCompare;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM _TV_SORTCB}
  _TV_SORTCB = tagTVSORTCB;
  TTVSortCB = tagTVSORTCB;
  {$EXTERNALSYM TV_SORTCB}
  TV_SORTCB = tagTVSORTCB;

{$EXTERNALSYM TreeView_SortChildrenCB}
function TreeView_SortChildrenCB(hwnd: HWND; const psort: TTVSortCB;
  recurse: Integer): Bool; inline;

const
  {$EXTERNALSYM TVM_ENDEDITLABELNOW}
  TVM_ENDEDITLABELNOW     = TV_FIRST + 22;

{$EXTERNALSYM TreeView_EndEditLabelNow}
function TreeView_EndEditLabelNow(hwnd: HWND; fCancel: Bool): Bool; inline;

const
  {$EXTERNALSYM TVM_GETISEARCHSTRINGA}
  TVM_GETISEARCHSTRINGA    = TV_FIRST + 23;
  {$EXTERNALSYM TVM_GETISEARCHSTRINGW}
  TVM_GETISEARCHSTRINGW    = TV_FIRST + 64;




  {$EXTERNALSYM TVM_GETISEARCHSTRING}
  TVM_GETISEARCHSTRING    = TVM_GETISEARCHSTRINGA;


{$EXTERNALSYM TreeView_GetISearchString}
function TreeView_GetISearchString(hwndTV: HWND; lpsz: PChar): Bool; inline;
{$EXTERNALSYM TreeView_GetISearchStringA}
function TreeView_GetISearchStringA(hwndTV: HWND; lpsz: PAnsiChar): Bool; inline;
{$EXTERNALSYM TreeView_GetISearchStringW}
function TreeView_GetISearchStringW(hwndTV: HWND; lpsz: PWideChar): Bool; inline;

const
  {$EXTERNALSYM TVM_SETTOOLTIPS}
  TVM_SETTOOLTIPS         = TV_FIRST + 24;

{$EXTERNALSYM TreeView_SetToolTips}
function TreeView_SetToolTips(wnd: HWND; hwndTT: HWND): HWND; inline;

const
  {$EXTERNALSYM TVM_GETTOOLTIPS}
  TVM_GETTOOLTIPS         = TV_FIRST + 25;

{$EXTERNALSYM TreeView_GetToolTips}
function TreeView_GetToolTips(wnd: HWND): HWND; inline;

const
  {$EXTERNALSYM TVM_SETINSERTMARK}
  TVM_SETINSERTMARK       = TV_FIRST + 26;

{$EXTERNALSYM TreeView_SetInsertMark}
function TreeView_SetInsertMark(hwnd: HWND; hItem: Integer; fAfter: BOOL): BOOL; inline;

const
  {$EXTERNALSYM TVM_SETUNICODEFORMAT}
  TVM_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;

{$EXTERNALSYM TreeView_SetUnicodeFormat}
function TreeView_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): BOOL; inline;

const
  {$EXTERNALSYM TVM_GETUNICODEFORMAT}
  TVM_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;

{$EXTERNALSYM TreeView_GetUnicodeFormat}
function TreeView_GetUnicodeFormat(hwnd: HWND): BOOL; inline;

const
  {$EXTERNALSYM TVM_SETITEMHEIGHT}
  TVM_SETITEMHEIGHT         = TV_FIRST + 27;

{$EXTERNALSYM TreeView_SetItemHeight}
function TreeView_SetItemHeight(hwnd: HWND; iHeight: Integer): Integer; inline;

const
  {$EXTERNALSYM TVM_GETITEMHEIGHT}
  TVM_GETITEMHEIGHT         = TV_FIRST + 28;

{$EXTERNALSYM TreeView_GetItemHeight}
function TreeView_GetItemHeight(hwnd: HWND): Integer; inline;

const
  {$EXTERNALSYM TVM_SETBKCOLOR}
  TVM_SETBKCOLOR              = TV_FIRST + 29;

{$EXTERNALSYM TreeView_SetBkColor}
function TreeView_SetBkColor(hwnd: HWND; clr: COLORREF): COLORREF; inline;

const
  {$EXTERNALSYM TVM_SETTEXTCOLOR}
  TVM_SETTEXTCOLOR              = TV_FIRST + 30;

{$EXTERNALSYM TreeView_SetTextColor}
function TreeView_SetTextColor(hwnd: HWND; clr: COLORREF): COLORREF; inline;

const
  {$EXTERNALSYM TVM_GETBKCOLOR}
  TVM_GETBKCOLOR              = TV_FIRST + 31;

{$EXTERNALSYM TreeView_GetBkColor}
function TreeView_GetBkColor(hwnd: HWND): COLORREF; inline;

const
  {$EXTERNALSYM TVM_GETTEXTCOLOR}
  TVM_GETTEXTCOLOR              = TV_FIRST + 32;

{$EXTERNALSYM TreeView_GetTextColor}
function TreeView_GetTextColor(hwnd: HWND): COLORREF; inline;

const
  {$EXTERNALSYM TVM_SETSCROLLTIME}
  TVM_SETSCROLLTIME              = TV_FIRST + 33;

{$EXTERNALSYM TreeView_SetScrollTime}
function TreeView_SetScrollTime(hwnd: HWND; uTime: UINT): UINT; inline;

const
  {$EXTERNALSYM TVM_GETSCROLLTIME}
  TVM_GETSCROLLTIME              = TV_FIRST + 34;

{$EXTERNALSYM TreeView_GetScrollTime}
function TreeView_GetScrollTime(hwnd: HWND): UINT; inline;

const
  {$EXTERNALSYM TVM_SETINSERTMARKCOLOR}
  TVM_SETINSERTMARKCOLOR         = TV_FIRST + 37;

{$EXTERNALSYM TreeView_SetInsertMarkColor}
function TreeView_SetInsertMarkColor(hwnd: HWND; clr: COLORREF): COLORREF; inline;

const
  {$EXTERNALSYM TVM_GETINSERTMARKCOLOR}
  TVM_GETINSERTMARKCOLOR         = TV_FIRST + 38;

{$EXTERNALSYM TreeView_GetInsertMarkColor}
function TreeView_GetInsertMarkColor(hwnd: HWND): COLORREF; inline;

type
  PNMTreeViewA = ^TNMTreeViewA;
  PNMTreeViewW = ^TNMTreeViewW;
  PNMTreeView = PNMTreeViewA;
  {$EXTERNALSYM tagNMTREEVIEWA}
  tagNMTREEVIEWA = packed record
    hdr: TNMHDR;
    action: UINT;
    itemOld: TTVItemA;
    itemNew: TTVItemA;
    ptDrag: TPoint;
  end;
  {$EXTERNALSYM tagNMTREEVIEWW}
  tagNMTREEVIEWW = packed record
    hdr: TNMHDR;
    action: UINT;
    itemOld: TTVItemW;
    itemNew: TTVItemW;
    ptDrag: TPoint;
  end;
  {$EXTERNALSYM tagNMTREEVIEW}
  tagNMTREEVIEW = tagNMTREEVIEWA;
  {$EXTERNALSYM _NM_TREEVIEWA}
  _NM_TREEVIEWA = tagNMTREEVIEWA;
  {$EXTERNALSYM _NM_TREEVIEWW}
  _NM_TREEVIEWW = tagNMTREEVIEWW;
  {$EXTERNALSYM _NM_TREEVIEW}
  _NM_TREEVIEW = _NM_TREEVIEWA;
  TNMTreeViewA  = tagNMTREEVIEWA;
  TNMTreeViewW  = tagNMTREEVIEWW;
  TNMTreeView = TNMTreeViewA;
  {$EXTERNALSYM NM_TREEVIEWA}
  NM_TREEVIEWA  = tagNMTREEVIEWA;
  {$EXTERNALSYM NM_TREEVIEWW}
  NM_TREEVIEWW  = tagNMTREEVIEWW;
  {$EXTERNALSYM NM_TREEVIEW}
  NM_TREEVIEW = NM_TREEVIEWA;

const
  {$EXTERNALSYM TVN_SELCHANGINGA}
  TVN_SELCHANGINGA         = TVN_FIRST-1;
  {$EXTERNALSYM TVN_SELCHANGEDA}
  TVN_SELCHANGEDA          = TVN_FIRST-2;
  {$EXTERNALSYM TVN_SELCHANGINGW}
  TVN_SELCHANGINGW         = TVN_FIRST-50;
  {$EXTERNALSYM TVN_SELCHANGEDW}
  TVN_SELCHANGEDW          = TVN_FIRST-51;






  {$EXTERNALSYM TVN_SELCHANGING}
  TVN_SELCHANGING         = TVN_SELCHANGINGA;
  {$EXTERNALSYM TVN_SELCHANGED}
  TVN_SELCHANGED          = TVN_SELCHANGEDA;


const
  {$EXTERNALSYM TVC_UNKNOWN}
  TVC_UNKNOWN             = $0000;
  {$EXTERNALSYM TVC_BYMOUSE}
  TVC_BYMOUSE             = $0001;
  {$EXTERNALSYM TVC_BYKEYBOARD}
  TVC_BYKEYBOARD          = $0002;

const
  {$EXTERNALSYM TVN_GETDISPINFOA}
  TVN_GETDISPINFOA         = TVN_FIRST-3;
  {$EXTERNALSYM TVN_SETDISPINFOA}
  TVN_SETDISPINFOA         = TVN_FIRST-4;
  {$EXTERNALSYM TVN_GETDISPINFOW}
  TVN_GETDISPINFOW         = TVN_FIRST-52;
  {$EXTERNALSYM TVN_SETDISPINFOW}
  TVN_SETDISPINFOW         = TVN_FIRST-53;






  {$EXTERNALSYM TVN_GETDISPINFO}
  TVN_GETDISPINFO         = TVN_GETDISPINFOA;
  {$EXTERNALSYM TVN_SETDISPINFO}
  TVN_SETDISPINFO         = TVN_SETDISPINFOA;


  {$EXTERNALSYM TVIF_DI_SETITEM}
  TVIF_DI_SETITEM         = $1000;

type
  PTVDispInfoA = ^TTVDispInfoA;
  PTVDispInfoW = ^TTVDispInfoW;
  PTVDispInfo = PTVDispInfoA;
  {$EXTERNALSYM tagTVDISPINFOA}
  tagTVDISPINFOA = packed record
    hdr: TNMHDR;
    item: TTVItemA;
  end;
  {$EXTERNALSYM tagTVDISPINFOW}
  tagTVDISPINFOW = packed record
    hdr: TNMHDR;
    item: TTVItemW;
  end;
  {$EXTERNALSYM tagTVDISPINFO}
  tagTVDISPINFO = tagTVDISPINFOA;
  {$EXTERNALSYM _TV_DISPINFOA}
  _TV_DISPINFOA = tagTVDISPINFOA;
  {$EXTERNALSYM _TV_DISPINFOW}
  _TV_DISPINFOW = tagTVDISPINFOW;
  {$EXTERNALSYM _TV_DISPINFO}
  _TV_DISPINFO = _TV_DISPINFOA;
  TTVDispInfoA = tagTVDISPINFOA;
  TTVDispInfoW = tagTVDISPINFOW;
  TTVDispInfo = TTVDispInfoA;
  {$EXTERNALSYM TV_DISPINFOA}
  TV_DISPINFOA = tagTVDISPINFOA;
  {$EXTERNALSYM TV_DISPINFOW}
  TV_DISPINFOW = tagTVDISPINFOW;
  {$EXTERNALSYM TV_DISPINFO}
  TV_DISPINFO = TV_DISPINFOA;

const
  {$EXTERNALSYM TVN_ITEMEXPANDINGA}
  TVN_ITEMEXPANDINGA       = TVN_FIRST-5;
  {$EXTERNALSYM TVN_ITEMEXPANDEDA}
  TVN_ITEMEXPANDEDA        = TVN_FIRST-6;
  {$EXTERNALSYM TVN_BEGINDRAGA}
  TVN_BEGINDRAGA           = TVN_FIRST-7;
  {$EXTERNALSYM TVN_BEGINRDRAGA}
  TVN_BEGINRDRAGA          = TVN_FIRST-8;
  {$EXTERNALSYM TVN_DELETEITEMA}
  TVN_DELETEITEMA          = TVN_FIRST-9;
  {$EXTERNALSYM TVN_BEGINLABELEDITA}
  TVN_BEGINLABELEDITA      = TVN_FIRST-10;
  {$EXTERNALSYM TVN_ENDLABELEDITA}
  TVN_ENDLABELEDITA        = TVN_FIRST-11;
  {$EXTERNALSYM TVN_GETINFOTIPA}
  TVN_GETINFOTIPA          = TVN_FIRST-13;
  {$EXTERNALSYM TVN_ITEMEXPANDINGW}
  TVN_ITEMEXPANDINGW       = TVN_FIRST-54;
  {$EXTERNALSYM TVN_ITEMEXPANDEDW}
  TVN_ITEMEXPANDEDW        = TVN_FIRST-55;
  {$EXTERNALSYM TVN_BEGINDRAGW}
  TVN_BEGINDRAGW           = TVN_FIRST-56;
  {$EXTERNALSYM TVN_BEGINRDRAGW}
  TVN_BEGINRDRAGW          = TVN_FIRST-57;
  {$EXTERNALSYM TVN_DELETEITEMW}
  TVN_DELETEITEMW          = TVN_FIRST-58;
  {$EXTERNALSYM TVN_BEGINLABELEDITW}
  TVN_BEGINLABELEDITW      = TVN_FIRST-59;
  {$EXTERNALSYM TVN_ENDLABELEDITW}
  TVN_ENDLABELEDITW        = TVN_FIRST-60;
  {$EXTERNALSYM TVN_GETINFOTIPW}
  TVN_GETINFOTIPW          = TVN_FIRST-14;


















  {$EXTERNALSYM TVN_ITEMEXPANDING}
  TVN_ITEMEXPANDING       = TVN_ITEMEXPANDINGA;
  {$EXTERNALSYM TVN_ITEMEXPANDED}
  TVN_ITEMEXPANDED        = TVN_ITEMEXPANDEDA;
  {$EXTERNALSYM TVN_BEGINDRAG}
  TVN_BEGINDRAG           = TVN_BEGINDRAGA;
  {$EXTERNALSYM TVN_BEGINRDRAG}
  TVN_BEGINRDRAG          = TVN_BEGINRDRAGA;
  {$EXTERNALSYM TVN_DELETEITEM}
  TVN_DELETEITEM          = TVN_DELETEITEMA;
  {$EXTERNALSYM TVN_BEGINLABELEDIT}
  TVN_BEGINLABELEDIT      = TVN_BEGINLABELEDITA;
  {$EXTERNALSYM TVN_ENDLABELEDIT}
  TVN_ENDLABELEDIT        = TVN_ENDLABELEDITA;
  {$EXTERNALSYM TVN_GETINFOTIP}
  TVN_GETINFOTIP         = TVN_GETINFOTIPA;


const
  {$EXTERNALSYM TVN_KEYDOWN}
  TVN_KEYDOWN             = TVN_FIRST-12;
  {$EXTERNALSYM TVN_SINGLEEXPAND}
  TVN_SINGLEEXPAND        = TVN_FIRST-15;

type
  {$EXTERNALSYM tagTVKEYDOWN}
  tagTVKEYDOWN = packed record
    hdr: TNMHDR;
    wVKey: Word;
    flags: UINT;
  end;
  {$EXTERNALSYM _TV_KEYDOWN}
  _TV_KEYDOWN = tagTVKEYDOWN;
  TTVKeyDown = tagTVKEYDOWN;
  {$EXTERNALSYM TV_KEYDOWN}
  TV_KEYDOWN = tagTVKEYDOWN;

  {$EXTERNALSYM tagNMTVCUSTOMDRAW}
  tagNMTVCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    clrText: COLORREF;
    clrTextBk: COLORREF;
    iLevel: Integer;
  end;
  PNMTVCustomDraw = ^TNMTVCustomDraw;
  TNMTVCustomDraw = tagNMTVCUSTOMDRAW;

  // for tooltips
  {$EXTERNALSYM tagNMTVGETINFOTIPA}
  tagNMTVGETINFOTIPA = packed record
    hdr: TNMHdr;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    hItem: HTREEITEM;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagNMTVGETINFOTIPW}
  tagNMTVGETINFOTIPW = packed record
    hdr: TNMHdr;
    pszText: PWideChar;
    cchTextMax: Integer;
    hItem: HTREEITEM;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagNMTVGETINFOTIP}
  tagNMTVGETINFOTIP = tagNMTVGETINFOTIPA;
  PNMTVGetInfoTipA = ^TNMTVGetInfoTipA;
  PNMTVGetInfoTipW = ^TNMTVGetInfoTipW;
  PNMTVGetInfoTip = PNMTVGetInfoTipA;
  TNMTVGetInfoTipA = tagNMTVGETINFOTIPA;
  TNMTVGetInfoTipW = tagNMTVGETINFOTIPW;
  TNMTVGetInfoTip = TNMTVGetInfoTipA;

const
  // treeview's customdraw return meaning don't draw images.  valid on CDRF_NOTIFYITEMPREPAINT
  {$EXTERNALSYM TVCDRF_NOIMAGES}
  TVCDRF_NOIMAGES         = $00010000;

{ ====== ComboBoxEx ======================== }

const
  {$EXTERNALSYM WC_COMBOBOXEX}
  WC_COMBOBOXEX = 'ComboBoxEx32';

  {$EXTERNALSYM CBEIF_TEXT}
  CBEIF_TEXT              = $00000001;
  {$EXTERNALSYM CBEIF_IMAGE}
  CBEIF_IMAGE             = $00000002;
  {$EXTERNALSYM CBEIF_SELECTEDIMAGE}
  CBEIF_SELECTEDIMAGE     = $00000004;
  {$EXTERNALSYM CBEIF_OVERLAY}
  CBEIF_OVERLAY           = $00000008;
  {$EXTERNALSYM CBEIF_INDENT}
  CBEIF_INDENT            = $00000010;
  {$EXTERNALSYM CBEIF_LPARAM}
  CBEIF_LPARAM            = $00000020;

  {$EXTERNALSYM CBEIF_DI_SETITEM}
  CBEIF_DI_SETITEM        = $10000000;

type
  {$EXTERNALSYM tagCOMBOBOXEXITEMA}
  tagCOMBOBOXEXITEMA = packed record
    mask: UINT;
    iItem: Integer;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    iOverlay: Integer;
    iIndent: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagCOMBOBOXEXITEMW}
  tagCOMBOBOXEXITEMW = packed record
    mask: UINT;
    iItem: Integer;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    iOverlay: Integer;
    iIndent: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagCOMBOBOXEXITEM}
  tagCOMBOBOXEXITEM = tagCOMBOBOXEXITEMA;
  PComboBoxExItemA = ^TComboBoxExItemA;
  PComboBoxExItemW = ^TComboBoxExItemW;
  PComboBoxExItem = PComboBoxExItemA;
  TComboBoxExItemA = tagCOMBOBOXEXITEMA;
  TComboBoxExItemW = tagCOMBOBOXEXITEMW;
  TComboBoxExItem = TComboBoxExItemA;

const
  {$EXTERNALSYM CBEM_INSERTITEMA}
  CBEM_INSERTITEMA        = WM_USER + 1;
  {$EXTERNALSYM CBEM_SETIMAGELIST}
  CBEM_SETIMAGELIST       = WM_USER + 2;
  {$EXTERNALSYM CBEM_GETIMAGELIST}
  CBEM_GETIMAGELIST       = WM_USER + 3;
  {$EXTERNALSYM CBEM_GETITEMA}
  CBEM_GETITEMA           = WM_USER + 4;
  {$EXTERNALSYM CBEM_SETITEMA}
  CBEM_SETITEMA           = WM_USER + 5;
  {$EXTERNALSYM CBEM_DELETEITEM}
  CBEM_DELETEITEM         = CB_DELETESTRING;
  {$EXTERNALSYM CBEM_GETCOMBOCONTROL}
  CBEM_GETCOMBOCONTROL    = WM_USER + 6;
  {$EXTERNALSYM CBEM_GETEDITCONTROL}
  CBEM_GETEDITCONTROL     = WM_USER + 7;
  {$EXTERNALSYM CBEM_SETEXSTYLE}
  CBEM_SETEXSTYLE         = WM_USER + 8;  // use SETEXTENDEDSTYLE instead
  {$EXTERNALSYM CBEM_GETEXSTYLE}
  CBEM_GETEXSTYLE         = WM_USER + 9;  // use GETEXTENDEDSTYLE instead
  {$EXTERNALSYM CBEM_GETEXTENDEDSTYLE}
  CBEM_GETEXTENDEDSTYLE   = WM_USER + 9;
  {$EXTERNALSYM CBEM_HASEDITCHANGED}
  CBEM_HASEDITCHANGED     = WM_USER + 10;
  {$EXTERNALSYM CBEM_INSERTITEMW}
  CBEM_INSERTITEMW        = WM_USER + 11;
  {$EXTERNALSYM CBEM_SETITEMW}
  CBEM_SETITEMW           = WM_USER + 12;
  {$EXTERNALSYM CBEM_GETITEMW}
  CBEM_GETITEMW           = WM_USER + 13;
  {$EXTERNALSYM CBEM_SETEXTENDEDSTYLE}
  CBEM_SETEXTENDEDSTYLE   = WM_USER + 14; // lparam == new style, wParam (optional) == mask
  {$EXTERNALSYM CBEM_SETUNICODEFORMAT}
  CBEM_SETUNICODEFORMAT   = CCM_SETUNICODEFORMAT;
  {$EXTERNALSYM CBEM_GETUNICODEFORMAT}
  CBEM_GETUNICODEFORMAT   = CCM_GETUNICODEFORMAT;









  {$EXTERNALSYM CBEM_INSERTITEM}
  CBEM_INSERTITEM         = CBEM_INSERTITEMA;
  {$EXTERNALSYM CBEM_SETITEM}
  CBEM_SETITEM            = CBEM_SETITEMA;
  {$EXTERNALSYM CBEM_GETITEM}
  CBEM_GETITEM            = CBEM_GETITEMA;


  {$EXTERNALSYM CBES_EX_NOEDITIMAGE}
  CBES_EX_NOEDITIMAGE          = $00000001;
  {$EXTERNALSYM CBES_EX_NOEDITIMAGEINDENT}
  CBES_EX_NOEDITIMAGEINDENT    = $00000002;
  {$EXTERNALSYM CBES_EX_PATHWORDBREAKPROC}
  CBES_EX_PATHWORDBREAKPROC    = $00000004;
  {$EXTERNALSYM CBES_EX_NOSIZELIMIT}
  CBES_EX_NOSIZELIMIT          = $00000008;
  {$EXTERNALSYM CBES_EX_CASESENSITIVE}
  CBES_EX_CASESENSITIVE        = $00000010;

type
  {$EXTERNALSYM NMCOMBOBOXEXA}
  NMCOMBOBOXEXA = packed record
    hdr: TNMHdr;
    ceItem: TComboBoxExItemA;
  end;
  {$EXTERNALSYM NMCOMBOBOXEXW}
  NMCOMBOBOXEXW = packed record
    hdr: TNMHdr;
    ceItem: TComboBoxExItemW;
  end;
  {$EXTERNALSYM NMCOMBOBOXEX}
  NMCOMBOBOXEX = NMCOMBOBOXEXA;
  PNMComboBoxExA = ^TNMComboBoxExA;
  PNMComboBoxExW = ^TNMComboBoxExW;
  PNMComboBoxEx = PNMComboBoxExA;
  TNMComboBoxExA = NMCOMBOBOXEXA;
  TNMComboBoxExW = NMCOMBOBOXEXW;
  TNMComboBoxEx = TNMComboBoxExA;

const
  {$EXTERNALSYM CBEN_GETDISPINFOA}
  CBEN_GETDISPINFOA       = CBEN_FIRST - 0;
  {$EXTERNALSYM CBEN_INSERTITEM}
  CBEN_INSERTITEM         = CBEN_FIRST - 1;
  {$EXTERNALSYM CBEN_DELETEITEM}
  CBEN_DELETEITEM         = CBEN_FIRST - 2;
  {$EXTERNALSYM CBEN_BEGINEDIT}
  CBEN_BEGINEDIT          = CBEN_FIRST - 4;
  {$EXTERNALSYM CBEN_ENDEDITA}
  CBEN_ENDEDITA           = CBEN_FIRST - 5; // lParam specifies why the endedit is happening
  {$EXTERNALSYM CBEN_ENDEDITW}
  CBEN_ENDEDITW           = CBEN_FIRST - 6;
  {$EXTERNALSYM CBEN_GETDISPINFOW}
  CBEN_GETDISPINFOW       = CBEN_FIRST - 7;
  {$EXTERNALSYM CBEN_DRAGBEGINA}
  CBEN_DRAGBEGINA			    = CBEN_FIRST - 8;
  {$EXTERNALSYM CBEN_DRAGBEGINW}
  CBEN_DRAGBEGINW			    = CBEN_FIRST - 9;









  {$EXTERNALSYM CBEN_ENDEDIT}
  CBEN_ENDEDIT            = CBEN_ENDEDITA;
  {$EXTERNALSYM CBEN_GETDISPINFO}
  CBEN_GETDISPINFO        = CBEN_GETDISPINFOA;
  {$EXTERNALSYM CBEN_DRAGBEGIN}
  CBEN_DRAGBEGIN          = CBEN_DRAGBEGINA;


  {$EXTERNALSYM CBENF_KILLFOCUS}
  CBENF_KILLFOCUS         = 1;
  {$EXTERNALSYM CBENF_RETURN}
  CBENF_RETURN            = 2;
  {$EXTERNALSYM CBENF_ESCAPE}
  CBENF_ESCAPE            = 3;
  {$EXTERNALSYM CBENF_DROPDOWN}
  CBENF_DROPDOWN          = 4;

  {$EXTERNALSYM CBEMAXSTRLEN}
  CBEMAXSTRLEN = 260;

type
  // CBEN_DRAGBEGIN sends this information ...
  {$EXTERNALSYM NMCBEDRAGBEGINA}
  NMCBEDRAGBEGINA = packed record
    hdr: TNMHdr;
    iItemid: Integer;
    szText: array[0..CBEMAXSTRLEN - 1] of AnsiChar;
  end;
  {$EXTERNALSYM NMCBEDRAGBEGINW}
  NMCBEDRAGBEGINW = packed record
    hdr: TNMHdr;
    iItemid: Integer;
    szText: array[0..CBEMAXSTRLEN - 1] of WideChar;
  end;
  {$EXTERNALSYM NMCBEDRAGBEGIN}
  NMCBEDRAGBEGIN = NMCBEDRAGBEGINA;
  PNMCBEDragBeginA = ^TNMCBEDragBeginA;
  PNMCBEDragBeginW = ^TNMCBEDragBeginW;
  PNMCBEDragBegin = PNMCBEDragBeginA;
  TNMCBEDragBeginA = NMCBEDRAGBEGINA;
  TNMCBEDragBeginW = NMCBEDRAGBEGINW;
  TNMCBEDragBegin = TNMCBEDragBeginA;

  // CBEN_ENDEDIT sends this information...
  // fChanged if the user actually did anything
  // iNewSelection gives what would be the new selection unless the notify is failed
  //                      iNewSelection may be CB_ERR if there's no match
  {$EXTERNALSYM NMCBEENDEDITA}
  NMCBEENDEDITA = packed record
    hdr: TNMHdr;
    fChanged: BOOL;
    iNewSelection: Integer;
    szText: array[0..CBEMAXSTRLEN - 1] of AnsiChar;
    iWhy: Integer;
  end;
  {$EXTERNALSYM NMCBEENDEDITW}
  NMCBEENDEDITW = packed record
    hdr: TNMHdr;
    fChanged: BOOL;
    iNewSelection: Integer;
    szText: array[0..CBEMAXSTRLEN - 1] of WideChar;
    iWhy: Integer;
  end;
  {$EXTERNALSYM NMCBEENDEDIT}
  NMCBEENDEDIT = NMCBEENDEDITA;
  PNMCBEEndEditA = ^TNMCBEEndEditA;
  PNMCBEEndEditW = ^TNMCBEEndEditW;
  PNMCBEEndEdit = PNMCBEEndEditA;
  TNMCBEEndEditA = NMCBEENDEDITA;
  TNMCBEEndEditW = NMCBEENDEDITW;
  TNMCBEEndEdit = TNMCBEEndEditA;

{ ====== TAB CONTROL ======================== }

const
  {$EXTERNALSYM WC_TABCONTROL}
  WC_TABCONTROL = 'SysTabControl32';

const
  {$EXTERNALSYM TCS_SCROLLOPPOSITE}
  TCS_SCROLLOPPOSITE    = $0001;  // assumes multiline tab
  {$EXTERNALSYM TCS_BOTTOM}
  TCS_BOTTOM            = $0002;
  {$EXTERNALSYM TCS_RIGHT}
  TCS_RIGHT             = $0002;
  {$EXTERNALSYM TCS_MULTISELECT}
  TCS_MULTISELECT       = $0004;  // allow multi-select in button mode
  {$EXTERNALSYM TCS_FLATBUTTONS}
  TCS_FLATBUTTONS       = $0008;
  {$EXTERNALSYM TCS_FORCEICONLEFT}
  TCS_FORCEICONLEFT     = $0010;
  {$EXTERNALSYM TCS_FORCELABELLEFT}
  TCS_FORCELABELLEFT    = $0020;
  {$EXTERNALSYM TCS_HOTTRACK}
  TCS_HOTTRACK          = $0040;
  {$EXTERNALSYM TCS_VERTICAL}
  TCS_VERTICAL          = $0080;
  {$EXTERNALSYM TCS_TABS}
  TCS_TABS              = $0000;
  {$EXTERNALSYM TCS_BUTTONS}
  TCS_BUTTONS           = $0100;
  {$EXTERNALSYM TCS_SINGLELINE}
  TCS_SINGLELINE        = $0000;
  {$EXTERNALSYM TCS_MULTILINE}
  TCS_MULTILINE         = $0200;
  {$EXTERNALSYM TCS_RIGHTJUSTIFY}
  TCS_RIGHTJUSTIFY      = $0000;
  {$EXTERNALSYM TCS_FIXEDWIDTH}
  TCS_FIXEDWIDTH        = $0400;
  {$EXTERNALSYM TCS_RAGGEDRIGHT}
  TCS_RAGGEDRIGHT       = $0800;
  {$EXTERNALSYM TCS_FOCUSONBUTTONDOWN}
  TCS_FOCUSONBUTTONDOWN = $1000;
  {$EXTERNALSYM TCS_OWNERDRAWFIXED}
  TCS_OWNERDRAWFIXED    = $2000;
  {$EXTERNALSYM TCS_TOOLTIPS}
  TCS_TOOLTIPS          = $4000;
  {$EXTERNALSYM TCS_FOCUSNEVER}
  TCS_FOCUSNEVER        = $8000;

  {$EXTERNALSYM TCS_EX_FLATSEPARATORS}
  TCS_EX_FLATSEPARATORS = $00000001;
  {$EXTERNALSYM TCS_EX_REGISTERDROP}
  TCS_EX_REGISTERDROP   = $00000002;

  {$EXTERNALSYM TCM_GETIMAGELIST}
  TCM_GETIMAGELIST       = TCM_FIRST + 2;
  {$EXTERNALSYM TCM_SETIMAGELIST}
  TCM_SETIMAGELIST       = TCM_FIRST + 3;
  {$EXTERNALSYM TCM_GETITEMCOUNT}
  TCM_GETITEMCOUNT       = TCM_FIRST + 4;
  {$EXTERNALSYM TCM_DELETEITEM}
  TCM_DELETEITEM         = TCM_FIRST + 8;
  {$EXTERNALSYM TCM_DELETEALLITEMS}
  TCM_DELETEALLITEMS     = TCM_FIRST + 9;
  {$EXTERNALSYM TCM_GETITEMRECT}
  TCM_GETITEMRECT        = TCM_FIRST + 10;
  {$EXTERNALSYM TCM_GETCURSEL}
  TCM_GETCURSEL          = TCM_FIRST + 11;
  {$EXTERNALSYM TCM_SETCURSEL}
  TCM_SETCURSEL          = TCM_FIRST + 12;
  {$EXTERNALSYM TCM_HITTEST}
  TCM_HITTEST            = TCM_FIRST + 13;
  {$EXTERNALSYM TCM_SETITEMEXTRA}
  TCM_SETITEMEXTRA       = TCM_FIRST + 14;
  {$EXTERNALSYM TCM_ADJUSTRECT}
  TCM_ADJUSTRECT         = TCM_FIRST + 40;
  {$EXTERNALSYM TCM_SETITEMSIZE}
  TCM_SETITEMSIZE        = TCM_FIRST + 41;
  {$EXTERNALSYM TCM_REMOVEIMAGE}
  TCM_REMOVEIMAGE        = TCM_FIRST + 42;
  {$EXTERNALSYM TCM_SETPADDING}
  TCM_SETPADDING         = TCM_FIRST + 43;
  {$EXTERNALSYM TCM_GETROWCOUNT}
  TCM_GETROWCOUNT        = TCM_FIRST + 44;
  {$EXTERNALSYM TCM_GETTOOLTIPS}
  TCM_GETTOOLTIPS        = TCM_FIRST + 45;
  {$EXTERNALSYM TCM_SETTOOLTIPS}
  TCM_SETTOOLTIPS        = TCM_FIRST + 46;
  {$EXTERNALSYM TCM_GETCURFOCUS}
  TCM_GETCURFOCUS        = TCM_FIRST + 47;
  {$EXTERNALSYM TCM_SETCURFOCUS}
  TCM_SETCURFOCUS        = TCM_FIRST + 48;
  {$EXTERNALSYM TCM_SETMINTABWIDTH}
  TCM_SETMINTABWIDTH     = TCM_FIRST + 49;
  {$EXTERNALSYM TCM_DESELECTALL}
  TCM_DESELECTALL        = TCM_FIRST + 50;
  {$EXTERNALSYM TCM_HIGHLIGHTITEM}
  TCM_HIGHLIGHTITEM      = TCM_FIRST + 51;
  {$EXTERNALSYM TCM_SETEXTENDEDSTYLE}
  TCM_SETEXTENDEDSTYLE   = TCM_FIRST + 52;  // optional wParam == mask
  {$EXTERNALSYM TCM_GETEXTENDEDSTYLE}
  TCM_GETEXTENDEDSTYLE   = TCM_FIRST + 53;
  {$EXTERNALSYM TCM_SETUNICODEFORMAT}
  TCM_SETUNICODEFORMAT   = CCM_SETUNICODEFORMAT;
  {$EXTERNALSYM TCM_GETUNICODEFORMAT}
  TCM_GETUNICODEFORMAT   = CCM_GETUNICODEFORMAT;

  {$EXTERNALSYM TCIF_TEXT}
  TCIF_TEXT       = $0001;
  {$EXTERNALSYM TCIF_IMAGE}
  TCIF_IMAGE      = $0002;
  {$EXTERNALSYM TCIF_RTLREADING}
  TCIF_RTLREADING = $0004;
  {$EXTERNALSYM TCIF_PARAM}
  TCIF_PARAM      = $0008;
  {$EXTERNALSYM TCIF_STATE}
  TCIF_STATE      = $0010;

  {$EXTERNALSYM TCIS_BUTTONPRESSED}
  TCIS_BUTTONPRESSED      = $0001;
  {$EXTERNALSYM TCIS_HIGHLIGHTED}
  TCIS_HIGHLIGHTED        = $0002;

type
  PTCItemHeaderA = ^TTCItemHeaderA;
  PTCItemHeaderW = ^TTCItemHeaderW;
  PTCItemHeader = PTCItemHeaderA;
  {$EXTERNALSYM tagTCITEMHEADERA}
  tagTCITEMHEADERA = packed record
    mask: UINT;
    lpReserved1: UINT;
    lpReserved2: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
  end;
  {$EXTERNALSYM tagTCITEMHEADERW}
  tagTCITEMHEADERW = packed record
    mask: UINT;
    lpReserved1: UINT;
    lpReserved2: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
  end;
  {$EXTERNALSYM tagTCITEMHEADER}
  tagTCITEMHEADER = tagTCITEMHEADERA;
  {$EXTERNALSYM _TC_ITEMHEADERA}
  _TC_ITEMHEADERA = tagTCITEMHEADERA;
  {$EXTERNALSYM _TC_ITEMHEADERW}
  _TC_ITEMHEADERW = tagTCITEMHEADERW;
  {$EXTERNALSYM _TC_ITEMHEADER}
  _TC_ITEMHEADER = _TC_ITEMHEADERA;
  TTCItemHeaderA = tagTCITEMHEADERA;
  TTCItemHeaderW = tagTCITEMHEADERW;
  TTCItemHeader = TTCItemHeaderA;
  {$EXTERNALSYM TC_ITEMHEADERA}
  TC_ITEMHEADERA = tagTCITEMHEADERA;
  {$EXTERNALSYM TC_ITEMHEADERW}
  TC_ITEMHEADERW = tagTCITEMHEADERW;
  {$EXTERNALSYM TC_ITEMHEADER}
  TC_ITEMHEADER = TC_ITEMHEADERA;

  PTCItemA = ^TTCItemA;
  PTCItemW = ^TTCItemW;
  PTCItem = PTCItemA;
  {$EXTERNALSYM tagTCITEMA}
  tagTCITEMA = packed record
    mask: UINT;
    dwState: UINT;
    dwStateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagTCITEMW}
  tagTCITEMW = packed record
    mask: UINT;
    dwState: UINT;
    dwStateMask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM tagTCITEM}
  tagTCITEM = tagTCITEMA;
  {$EXTERNALSYM _TC_ITEMA}
  _TC_ITEMA = tagTCITEMA;
  {$EXTERNALSYM _TC_ITEMW}
  _TC_ITEMW = tagTCITEMW;
  {$EXTERNALSYM _TC_ITEM}
  _TC_ITEM = _TC_ITEMA;
  TTCItemA = tagTCITEMA;
  TTCItemW = tagTCITEMW;
  TTCItem = TTCItemA;
  {$EXTERNALSYM TC_ITEMA}
  TC_ITEMA = tagTCITEMA;
  {$EXTERNALSYM TC_ITEMW}
  TC_ITEMW = tagTCITEMW;
  {$EXTERNALSYM TC_ITEM}
  TC_ITEM = TC_ITEMA;

const
  {$EXTERNALSYM TCM_GETITEMA}
  TCM_GETITEMA             = TCM_FIRST + 5;
  {$EXTERNALSYM TCM_SETITEMA}
  TCM_SETITEMA             = TCM_FIRST + 6;
  {$EXTERNALSYM TCM_INSERTITEMA}
  TCM_INSERTITEMA          = TCM_FIRST + 7;
  {$EXTERNALSYM TCM_GETITEMW}
  TCM_GETITEMW             = TCM_FIRST + 60;
  {$EXTERNALSYM TCM_SETITEMW}
  TCM_SETITEMW             = TCM_FIRST + 61;
  {$EXTERNALSYM TCM_INSERTITEMW}
  TCM_INSERTITEMW          = TCM_FIRST + 62;








  {$EXTERNALSYM TCM_GETITEM}
  TCM_GETITEM             = TCM_GETITEMA;
  {$EXTERNALSYM TCM_SETITEM}
  TCM_SETITEM             = TCM_SETITEMA;
  {$EXTERNALSYM TCM_INSERTITEM}
  TCM_INSERTITEM          = TCM_INSERTITEMA;


const
  {$EXTERNALSYM TCHT_NOWHERE}
  TCHT_NOWHERE     = $0001;
  {$EXTERNALSYM TCHT_ONITEMICON}
  TCHT_ONITEMICON  = $0002;
  {$EXTERNALSYM TCHT_ONITEMLABEL}
  TCHT_ONITEMLABEL = $0004;
  {$EXTERNALSYM TCHT_ONITEM}
  TCHT_ONITEM      = TCHT_ONITEMICON or TCHT_ONITEMLABEL;

type
  PTCHitTestInfo = ^TTCHitTestInfo;
  {$EXTERNALSYM tagTCHITTESTINFO}
  tagTCHITTESTINFO = packed record
    pt: TPoint;
    flags: UINT;
  end;
  {$EXTERNALSYM _TC_HITTESTINFO}
  _TC_HITTESTINFO = tagTCHITTESTINFO;
  TTCHitTestInfo = tagTCHITTESTINFO;
  {$EXTERNALSYM TC_HITTESTINFO}
  TC_HITTESTINFO = tagTCHITTESTINFO;

  {$EXTERNALSYM tagTCKEYDOWN}
  tagTCKEYDOWN = packed record
    hdr: TNMHDR;
    wVKey: Word;
    flags: UINT;
  end;
  {$EXTERNALSYM _TC_KEYDOWN}
  _TC_KEYDOWN = tagTCKEYDOWN;
  TTCKeyDown = tagTCKEYDOWN;
  {$EXTERNALSYM TC_KEYDOWN}
  TC_KEYDOWN = tagTCKEYDOWN;

const
  {$EXTERNALSYM TCN_KEYDOWN}
  TCN_KEYDOWN             = TCN_FIRST - 0;
  {$EXTERNALSYM TCN_SELCHANGE}
  TCN_SELCHANGE           = TCN_FIRST - 1;
  {$EXTERNALSYM TCN_SELCHANGING}
  TCN_SELCHANGING         = TCN_FIRST - 2;
  {$EXTERNALSYM TCN_GETOBJECT}
  TCN_GETOBJECT           = TCN_FIRST - 3;

{$EXTERNALSYM TabCtrl_HitTest}
function TabCtrl_HitTest(hwndTC: HWND; pinfo: PTCHitTestInfo): Integer; inline;
{$EXTERNALSYM TabCtrl_SetItemExtra}
function TabCtrl_SetItemExtra(hwndTC: HWND; cb: Integer): BOOL; inline;
{$EXTERNALSYM TabCtrl_AdjustRect}
function TabCtrl_AdjustRect(hwnd: HWND; bLarger: BOOL; prc: PRect): Integer; inline;
{$EXTERNALSYM TabCtrl_SetItemSize}
function TabCtrl_SetItemSize(hwnd: HWND; x, y: Integer): DWORD;
{$EXTERNALSYM TabCtrl_RemoveImage}
procedure TabCtrl_RemoveImage(hwnd: HWND; i: Integer);
{$EXTERNALSYM TabCtrl_SetPadding}
procedure TabCtrl_SetPadding(hwnd: HWND; cx, cy: Integer);
{$EXTERNALSYM TabCtrl_GetRowCount}
function TabCtrl_GetRowCount(hwnd: HWND): Integer; inline;
{$EXTERNALSYM TabCtrl_GetToolTips}
function TabCtrl_GetToolTips(wnd: HWND): HWND; inline;
{$EXTERNALSYM TabCtrl_SetToolTips}
procedure TabCtrl_SetToolTips(hwnd: HWND; hwndTT: HWND);
{$EXTERNALSYM TabCtrl_GetCurFocus}
function TabCtrl_GetCurFocus(hwnd: HWND): Integer; inline;
{$EXTERNALSYM TabCtrl_SetCurFocus}
procedure TabCtrl_SetCurFocus(hwnd: HWND; i: Integer);
{$EXTERNALSYM TabCtrl_SetMinTabWidth}
function TabCtrl_SetMinTabWidth(hwnd: HWND; x: Integer): Integer; inline;
{$EXTERNALSYM TabCtrl_DeselectAll}
procedure TabCtrl_DeselectAll(hwnd: HWND; fExcludeFocus: BOOL);
{$EXTERNALSYM TabCtrl_HighlightItem}
function TabCtrl_HighlightItem(hwnd: HWND; i: Integer; fHighlight: WordBool): BOOL;
{$EXTERNALSYM TabCtrl_SetExtendedStyle}
function TabCtrl_SetExtendedStyle(hwnd: HWND; dw: DWORD): DWORD; inline;
{$EXTERNALSYM TabCtrl_GetExtendedStyle}
function TabCtrl_GetExtendedStyle(hwnd: HWND): DWORD; inline;
{$EXTERNALSYM TabCtrl_SetUnicodeFormat}
function TabCtrl_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): BOOL; inline;
{$EXTERNALSYM TabCtrl_GetUnicodeFormat}
function TabCtrl_GetUnicodeFormat(hwnd: HWND): BOOL; inline;
{$EXTERNALSYM TabCtrl_GetItemRect}
function TabCtrl_GetItemRect(hwnd: HWND; i: Integer; var prc: TRect): BOOL; inline;

{ ====== ANIMATE CONTROL ================= }

const
  {$EXTERNALSYM ANIMATE_CLASS}
  ANIMATE_CLASS = 'SysAnimate32';

const
  {$EXTERNALSYM ACS_CENTER}
  ACS_CENTER              = $0001;
  {$EXTERNALSYM ACS_TRANSPARENT}
  ACS_TRANSPARENT         = $0002;
  {$EXTERNALSYM ACS_AUTOPLAY}
  ACS_AUTOPLAY            = $0004;
  {$EXTERNALSYM ACS_TIMER}
  ACS_TIMER               = $0008;  { don't use threads... use timers }

  {$EXTERNALSYM ACM_OPENA}
  ACM_OPENA                = WM_USER + 100;
  {$EXTERNALSYM ACM_OPENW}
  ACM_OPENW                = WM_USER + 103;




  {$EXTERNALSYM ACM_OPEN}
  ACM_OPEN                = ACM_OPENA;


  {$EXTERNALSYM ACM_PLAY}
  ACM_PLAY                = WM_USER + 101;
  {$EXTERNALSYM ACM_STOP}
  ACM_STOP                = WM_USER + 102;

  {$EXTERNALSYM ACN_START}
  ACN_START               = 1;
  {$EXTERNALSYM ACN_STOP}
  ACN_STOP                = 2;

{$EXTERNALSYM Animate_Create}
function Animate_Create(hwndP: HWND; id: HMENU; dwStyle: DWORD; hInstance: HINST): HWND;
{$EXTERNALSYM Animate_Open}
function Animate_Open(hwnd: HWND; szName: PChar): BOOL; inline;
{$EXTERNALSYM Animate_OpenEx}
function Animate_OpenEx(hwnd: HWND; hInst: HINST; szName: PChar): BOOL; inline;
{$EXTERNALSYM Animate_Play}
function Animate_Play(hwnd: HWND; from, _to: Word; rep: UINT): BOOL;
{$EXTERNALSYM Animate_Stop}
function Animate_Stop(hwnd: HWND): BOOL; inline;
{$EXTERNALSYM Animate_Close}
function Animate_Close(hwnd: HWND): BOOL; inline;
{$EXTERNALSYM Animate_Seek}
function Animate_Seek(hwnd: HWND; frame: Word): BOOL; inline;

{ ====== MONTHCAL CONTROL ========= }

const
  {$EXTERNALSYM MONTHCAL_CLASS}
  MONTHCAL_CLASS          = 'SysMonthCal32';

const  
  // Message constants
  {$EXTERNALSYM MCM_FIRST}
  MCM_FIRST             = $1000;
  {$EXTERNALSYM MCM_GETCURSEL}
  MCM_GETCURSEL         = MCM_FIRST + 1;
  {$EXTERNALSYM MCM_SETCURSEL}
  MCM_SETCURSEL         = MCM_FIRST + 2;
  {$EXTERNALSYM MCM_GETMAXSELCOUNT}
  MCM_GETMAXSELCOUNT    = MCM_FIRST + 3;
  {$EXTERNALSYM MCM_SETMAXSELCOUNT}
  MCM_SETMAXSELCOUNT    = MCM_FIRST + 4;
  {$EXTERNALSYM MCM_GETSELRANGE}
  MCM_GETSELRANGE       = MCM_FIRST + 5;
  {$EXTERNALSYM MCM_SETSELRANGE}
  MCM_SETSELRANGE       = MCM_FIRST + 6;
  {$EXTERNALSYM MCM_GETMONTHRANGE}
  MCM_GETMONTHRANGE     = MCM_FIRST + 7;
  {$EXTERNALSYM MCM_SETDAYSTATE}
  MCM_SETDAYSTATE       = MCM_FIRST + 8;
  {$EXTERNALSYM MCM_GETMINREQRECT}
  MCM_GETMINREQRECT     = MCM_FIRST + 9;
  {$EXTERNALSYM MCM_SETCOLOR}
  MCM_SETCOLOR          = MCM_FIRST + 10;
  {$EXTERNALSYM MCM_GETCOLOR}
  MCM_GETCOLOR          = MCM_FIRST + 11;
  {$EXTERNALSYM MCM_SETTODAY}
  MCM_SETTODAY          = MCM_FIRST + 12;
  {$EXTERNALSYM MCM_GETTODAY}
  MCM_GETTODAY          = MCM_FIRST + 13;
  {$EXTERNALSYM MCM_HITTEST}
  MCM_HITTEST           = MCM_FIRST + 14;
  {$EXTERNALSYM MCM_SETFIRSTDAYOFWEEK}
  MCM_SETFIRSTDAYOFWEEK = MCM_FIRST + 15;
  {$EXTERNALSYM MCM_GETFIRSTDAYOFWEEK}
  MCM_GETFIRSTDAYOFWEEK = MCM_FIRST + 16;
  {$EXTERNALSYM MCM_GETRANGE}
  MCM_GETRANGE          = MCM_FIRST + 17;
  {$EXTERNALSYM MCM_SETRANGE}
  MCM_SETRANGE          = MCM_FIRST + 18;
  {$EXTERNALSYM MCM_GETMONTHDELTA}
  MCM_GETMONTHDELTA     = MCM_FIRST + 19;
  {$EXTERNALSYM MCM_SETMONTHDELTA}
  MCM_SETMONTHDELTA     = MCM_FIRST + 20;
  {$EXTERNALSYM MCM_GETMAXTODAYWIDTH}
  MCM_GETMAXTODAYWIDTH  = MCM_FIRST + 21;
  {$EXTERNALSYM MCM_SETUNICODEFORMAT}
  MCM_SETUNICODEFORMAT  = CCM_SETUNICODEFORMAT;
  {$EXTERNALSYM MCM_GETUNICODEFORMAT}
  MCM_GETUNICODEFORMAT  = CCM_GETUNICODEFORMAT;

  // Hit test flags
  {$EXTERNALSYM MCHT_TITLE}
  MCHT_TITLE            = $00010000;
  {$EXTERNALSYM MCHT_CALENDAR}
  MCHT_CALENDAR         = $00020000;
  {$EXTERNALSYM MCHT_TODAYLINK}
  MCHT_TODAYLINK        = $00030000;
  {$EXTERNALSYM MCHT_NEXT}
  MCHT_NEXT             = $01000000;  // these indicate that hitting
  {$EXTERNALSYM MCHT_PREV}
  MCHT_PREV             = $02000000;  // here will go to the next/prev month
  {$EXTERNALSYM MCHT_NOWHERE}
  MCHT_NOWHERE          = $00000000;
  {$EXTERNALSYM MCHT_TITLEBK}
  MCHT_TITLEBK          = MCHT_TITLE;
  {$EXTERNALSYM MCHT_TITLEMONTH}
  MCHT_TITLEMONTH       = MCHT_TITLE or $0001;
  {$EXTERNALSYM MCHT_TITLEYEAR}
  MCHT_TITLEYEAR        = MCHT_TITLE or $0002;
  {$EXTERNALSYM MCHT_TITLEBTNNEXT}
  MCHT_TITLEBTNNEXT     = MCHT_TITLE or MCHT_NEXT or $0003;
  {$EXTERNALSYM MCHT_TITLEBTNPREV}
  MCHT_TITLEBTNPREV     = MCHT_TITLE or MCHT_PREV or $0003;
  {$EXTERNALSYM MCHT_CALENDARBK}
  MCHT_CALENDARBK       = MCHT_CALENDAR;
  {$EXTERNALSYM MCHT_CALENDARDATE}
  MCHT_CALENDARDATE     = MCHT_CALENDAR or $0001;
  {$EXTERNALSYM MCHT_CALENDARDATENEXT}
  MCHT_CALENDARDATENEXT = MCHT_CALENDARDATE or MCHT_NEXT;
  {$EXTERNALSYM MCHT_CALENDARDATEPREV}
  MCHT_CALENDARDATEPREV = MCHT_CALENDARDATE or MCHT_PREV;
  {$EXTERNALSYM MCHT_CALENDARDAY}
  MCHT_CALENDARDAY      = MCHT_CALENDAR or $0002;
  {$EXTERNALSYM MCHT_CALENDARWEEKNUM}
  MCHT_CALENDARWEEKNUM  = MCHT_CALENDAR or $0003;

  // Color codes
  {$EXTERNALSYM MCSC_BACKGROUND}
  MCSC_BACKGROUND       = 0;   // the background color (between months)
  {$EXTERNALSYM MCSC_TEXT}
  MCSC_TEXT             = 1;   // the dates
  {$EXTERNALSYM MCSC_TITLEBK}
  MCSC_TITLEBK          = 2;   // background of the title
  {$EXTERNALSYM MCSC_TITLETEXT}
  MCSC_TITLETEXT        = 3;
  {$EXTERNALSYM MCSC_MONTHBK}
  MCSC_MONTHBK          = 4;   // background within the month cal
  {$EXTERNALSYM MCSC_TRAILINGTEXT}
  MCSC_TRAILINGTEXT     = 5;   // the text color of header & trailing days

  // Notification codes
  {$EXTERNALSYM MCN_SELCHANGE}
  MCN_SELCHANGE         = MCN_FIRST + 1;
  {$EXTERNALSYM MCN_GETDAYSTATE}
  MCN_GETDAYSTATE       = MCN_FIRST + 3;
  {$EXTERNALSYM MCN_SELECT}
  MCN_SELECT            = MCN_FIRST + 4;

  // Style flags
  {$EXTERNALSYM MCS_DAYSTATE}
  MCS_DAYSTATE          = $0001;
  {$EXTERNALSYM MCS_MULTISELECT}
  MCS_MULTISELECT       = $0002;
  {$EXTERNALSYM MCS_WEEKNUMBERS}
  MCS_WEEKNUMBERS       = $0004;
  MCS_NOTODAY_PRE_IE4   = $0008;
  {$EXTERNALSYM MCS_NOTODAYCIRCLE}
  MCS_NOTODAYCIRCLE     = $0008;
  {$EXTERNALSYM MCS_NOTODAY}
  MCS_NOTODAY           = $0010;

  {$EXTERNALSYM GMR_VISIBLE}
  GMR_VISIBLE           = 0;       // visible portion of display
  {$EXTERNALSYM GMR_DAYSTATE}
  GMR_DAYSTATE          = 1;       // above plus the grayed out parts of
                                   // partially displayed months
                                   
type
  // bit-packed array of "bold" info for a month
  // if a bit is on, that day is drawn bold
  {$EXTERNALSYM MONTHDAYSTATE}
  MONTHDAYSTATE = DWORD;
  PMonthDayState = ^TMonthDayState;
  TMonthDayState = MONTHDAYSTATE;

  {$EXTERNALSYM MCHITTESTINFO}
  MCHITTESTINFO = packed record
    cbSize: UINT;
    pt: TPoint;
    uHit: UINT;      // out param
    st: TSystemTime;
  end;
  PMCHitTestInfo = ^TMCHitTestInfo;
  TMCHitTestInfo = MCHITTESTINFO;

  // MCN_SELCHANGE is sent whenever the currently displayed date changes
  // via month change, year change, keyboard navigation, prev/next button
  {$EXTERNALSYM tagNMSELCHANGE}
  tagNMSELCHANGE = packed record
    nmhdr: TNmHdr;  // this must be first, so we don't break WM_NOTIFY
    stSelStart: TSystemTime;
    stSelEnd: TSystemTime;
  end;
  PNMSelChange = ^TNMSelChange;
  TNMSelChange = tagNMSELCHANGE;

  // MCN_GETDAYSTATE is sent for MCS_DAYSTATE controls whenever new daystate
  // information is needed (month or year scroll) to draw bolding information.
  // The app must fill in cDayState months worth of information starting from
  // stStart date. The app may fill in the array at prgDayState or change
  // prgDayState to point to a different array out of which the information
  // will be copied. (similar to tooltips)
  {$EXTERNALSYM tagNMDAYSTATE}
  tagNMDAYSTATE = packed record
    nmhdr: TNmHdr;  // this must be first, so we don't break WM_NOTIFY
    stStart: TSystemTime;
    cDayState: Integer;
    prgDayState: PMonthDayState; // points to cDayState TMONTHDAYSTATEs
  end;
  PNMDayState = ^TNMDayState;
  TNMDayState = tagNMDAYSTATE;

  // MCN_SELECT is sent whenever a selection has occured (via mouse or keyboard)
  {$EXTERNALSYM NMSELECT}
  NMSELECT = tagNMSELCHANGE;
  PNMSelect = ^TNMSelect;
  TNMSelect = NMSELECT;

//   returns FALSE if MCS_MULTISELECT
//   returns TRUE and sets *pst to the currently selected date otherwise
{$EXTERNALSYM MonthCal_GetCurSel}
function MonthCal_GetCurSel(hmc: HWND; var pst: TSystemTime): BOOL; inline;

//   returns FALSE if MCS_MULTISELECT
//   returns TURE and sets the currently selected date to *pst otherwise
{$EXTERNALSYM MonthCal_SetCurSel}
function MonthCal_SetCurSel(hmc: HWND; const pst: TSystemTime): BOOL; inline;

//   returns the maximum number of selectable days allowed
{$EXTERNALSYM MonthCal_GetMaxSelCount}
function MonthCal_GetMaxSelCount(hmc: HWND): DWORD; inline;

//   sets the max number days that can be selected iff MCS_MULTISELECT
{$EXTERNALSYM MonthCal_SetMaxSelCount}
function MonthCal_SetMaxSelCount(hmc: HWND; n: UINT): BOOL; inline;

//   sets rgst[0] to the first day of the selection range
//   sets rgst[1] to the last day of the selection range
{$EXTERNALSYM MonthCal_GetSelRange}
function MonthCal_GetSelRange(hmc: HWND; rgst: PSystemTime): BOOL; inline;

//   selects the range of days from rgst[0] to rgst[1]
{$EXTERNALSYM MonthCal_SetSelRange}
function MonthCal_SetSelRange(hmc: HWND; rgst: PSystemTime): BOOL; inline;

//   if rgst specified, sets rgst[0] to the starting date and
//      and rgst[1] to the ending date of the the selectable (non-grayed)
//      days if GMR_VISIBLE or all the displayed days (including grayed)
//      if GMR_DAYSTATE.
//   returns the number of months spanned by the above range.
{$EXTERNALSYM MonthCal_GetMonthRange}
function MonthCal_GetMonthRange(hmc: HWND; gmr: DWORD; rgst: PSystemTime): DWORD; inline;

//   cbds is the count of DAYSTATE items in rgds and it must be equal
//   to the value returned from MonthCal_GetMonthRange(hmc, GMR_DAYSTATE, NULL)
//   This sets the DAYSTATE bits for each month (grayed and non-grayed
//   days) displayed in the calendar. The first bit in a month's DAYSTATE
//   corresponts to bolding day 1, the second bit affects day 2, etc.
{$EXTERNALSYM MonthCal_SetDayState}
function MonthCal_SetDayState(hmc: HWND; cbds: Integer; const rgds: TNMDayState): BOOL; inline;

//   sets prc the minimal size needed to display one month
{$EXTERNALSYM MonthCal_GetMinReqRect}
function MonthCal_GetMinReqRect(hmc: HWND; var prc: TRect): BOOL; inline;

// set what day is "today"   send NULL to revert back to real date
{$EXTERNALSYM MonthCal_SetToday}
function MonthCal_SetToday(hmc: HWND; const pst: TSystemTime): BOOL; inline;

// get what day is "today"
// returns BOOL for success/failure
{$EXTERNALSYM MonthCal_GetToday}
function MonthCal_GetToday(hmc: HWND; var pst: TSystemTime): BOOL; inline;

// determine what pinfo->pt is over
{$EXTERNALSYM MonthCal_HitTest}
function MonthCal_HitTest(hmc: HWND; var info: TMCHitTestInfo): DWORD; inline;

// set colors to draw control with -- see MCSC_ bits below
{$EXTERNALSYM MonthCal_SetColor}
function MonthCal_SetColor(hmc: HWND; iColor: Integer; clr: TColorRef): TColorRef; inline;

{$EXTERNALSYM MonthCal_GetColor}
function MonthCal_GetColor(hmc: HWND; iColor: Integer): TColorRef; inline;

// set first day of week to iDay:
// 0 for Monday, 1 for Tuesday, ..., 6 for Sunday
// -1 for means use locale info
{$EXTERNALSYM MonthCal_SetFirstDayOfWeek}
function MonthCal_SetFirstDayOfWeek(hmc: HWND; iDay: Integer): Integer; inline;

// DWORD result...  low word has the day.  high word is bool if this is app set
// or not (FALSE == using locale info)
{$EXTERNALSYM MonthCal_GetFirstDayOfWeek}
function MonthCal_GetFirstDayOfWeek(hmc: HWND): Integer; inline;

//   modifies rgst[0] to be the minimum ALLOWABLE systemtime (or 0 if no minimum)
//   modifies rgst[1] to be the maximum ALLOWABLE systemtime (or 0 if no maximum)
//   returns GDTR_MIN|GDTR_MAX if there is a minimum|maximum limit
{$EXTERNALSYM MonthCal_GetRange}
function MonthCal_GetRange(hmc: HWND; rgst: PSystemTime): DWORD; inline;

//   if GDTR_MIN, sets the minimum ALLOWABLE systemtime to rgst[0], otherwise removes minimum
//   if GDTR_MAX, sets the maximum ALLOWABLE systemtime to rgst[1], otherwise removes maximum
//   returns TRUE on success, FALSE on error (such as invalid parameters)
{$EXTERNALSYM Monthcal_SetRange}
function Monthcal_SetRange(hmc: HWND; gdtr: DWORD; rgst: PSystemTime): BOOL; inline;

//   returns the number of months one click on a next/prev button moves by
{$EXTERNALSYM MonthCal_GetMonthDelta}
function MonthCal_GetMonthDelta(hmc: HWND): Integer; inline;

//   sets the month delta to n. n = 0 reverts to moving by a page of months
//   returns the previous value of n.
{$EXTERNALSYM MonthCal_SetMonthDelta}
function MonthCal_SetMonthDelta(hmc: HWND; n: Integer): Integer; inline;

//   sets *psz to the maximum width/height of the "Today" string displayed
//   at the bottom of the calendar (as long as MCS_NOTODAY is not specified)
{$EXTERNALSYM MonthCal_GetMaxTodayWidth}
function MonthCal_GetMaxTodayWidth(hmc: HWND): DWORD; inline;

{$EXTERNALSYM MonthCal_SetUnicodeFormat}
function MonthCal_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): BOOL; inline;

{$EXTERNALSYM MonthCal_GetUnicodeFormat}
function MonthCal_GetUnicodeFormat(hwnd: HWND): BOOL; inline;

{ ====== DATETIMEPICK CONTROL =============== }

const
  {$EXTERNALSYM DATETIMEPICK_CLASS}
  DATETIMEPICK_CLASS = 'SysDateTimePick32';

  // Message constants
  {$EXTERNALSYM DTM_FIRST}
  DTM_FIRST         = $1000;
  {$EXTERNALSYM DTM_GETSYSTEMTIME}
  DTM_GETSYSTEMTIME = DTM_FIRST + 1;
  {$EXTERNALSYM DTM_SETSYSTEMTIME}
  DTM_SETSYSTEMTIME = DTM_FIRST + 2;
  {$EXTERNALSYM DTM_GETRANGE}
  DTM_GETRANGE      = DTM_FIRST + 3;
  {$EXTERNALSYM DTM_SETRANGE}
  DTM_SETRANGE      = DTM_FIRST + 4;
  {$EXTERNALSYM DTM_SETFORMATA}
  DTM_SETFORMATA    = DTM_FIRST + 5;
  {$EXTERNALSYM DTM_SETMCCOLOR}
  DTM_SETMCCOLOR    = DTM_FIRST + 6;
  {$EXTERNALSYM DTM_GETMCCOLOR}
  DTM_GETMCCOLOR    = DTM_FIRST + 7;
  {$EXTERNALSYM DTM_GETMONTHCAL}
  DTM_GETMONTHCAL   = DTM_FIRST + 8;
  {$EXTERNALSYM DTM_SETMCFONT}
  DTM_SETMCFONT     = DTM_FIRST + 9;
  {$EXTERNALSYM DTM_GETMCFONT}
  DTM_GETMCFONT     = DTM_FIRST + 10;
  {$EXTERNALSYM DTM_SETFORMATW}
  DTM_SETFORMATW    = DTM_FIRST + 50;
  {$EXTERNALSYM DTM_SETFORMAT}
  DTM_SETFORMAT     = DTM_SETFORMATA;

  // Style Flags
  {$EXTERNALSYM DTS_UPDOWN}
  DTS_UPDOWN          = $0001;  // use UPDOWN instead of MONTHCAL
  {$EXTERNALSYM DTS_SHOWNONE}
  DTS_SHOWNONE        = $0002;  // allow a NONE selection
  {$EXTERNALSYM DTS_SHORTDATEFORMAT}
  DTS_SHORTDATEFORMAT = $0000;  // use the short date format
                                // (app must forward WM_WININICHANGE messages)
  {$EXTERNALSYM DTS_LONGDATEFORMAT}
  DTS_LONGDATEFORMAT  = $0004;  // use the long date format
                                // (app must forward WM_WININICHANGE messages)
  {$EXTERNALSYM DTS_TIMEFORMAT}
  DTS_TIMEFORMAT      = $0009;  // use the time format
                                // (app must forward WM_WININICHANGE messages)
  {$EXTERNALSYM DTS_APPCANPARSE}
  DTS_APPCANPARSE     = $0010;  // allow user entered strings
                                // (app MUST respond to DTN_USERSTRING)
  {$EXTERNALSYM DTS_RIGHTALIGN}
  DTS_RIGHTALIGN      = $0020;  // right-align popup instead of left-align it

  // Notification codes
  {$EXTERNALSYM DTN_DATETIMECHANGE}
  DTN_DATETIMECHANGE = DTN_FIRST + 1;  // the systemtime has changed
  {$EXTERNALSYM DTN_USERSTRINGA}
  DTN_USERSTRINGA    = DTN_FIRST + 2;  // the user has entered a string
  {$EXTERNALSYM DTN_USERSTRINGW}
  DTN_USERSTRINGW    = DTN_FIRST + 15;
  {$EXTERNALSYM DTN_WMKEYDOWNA}
  DTN_WMKEYDOWNA     = DTN_FIRST + 3;  // modify keydown on app format field (X)
  {$EXTERNALSYM DTN_WMKEYDOWNW}
  DTN_WMKEYDOWNW     = DTN_FIRST + 16;
  {$EXTERNALSYM DTN_FORMATA}
  DTN_FORMATA        = DTN_FIRST + 4;  // query display for app format field (X)
  {$EXTERNALSYM DTN_FORMATW}
  DTN_FORMATW        = DTN_FIRST + 17;
  {$EXTERNALSYM DTN_FORMATQUERYA}
  DTN_FORMATQUERYA   = DTN_FIRST + 5;  // query formatting info for app format field (X)
  {$EXTERNALSYM DTN_FORMATQUERYW}
  DTN_FORMATQUERYW   = DTN_FIRST + 18;
  {$EXTERNALSYM DTN_DROPDOWN}
  DTN_DROPDOWN       = DTN_FIRST + 6;  // MonthCal has dropped down
  {$EXTERNALSYM DTN_CLOSEUP}
  DTN_CLOSEUP        = DTN_FIRST + 7;  // MonthCal is popping up










  {$EXTERNALSYM DTN_USERSTRING}
  DTN_USERSTRING     = DTN_USERSTRINGA;
  {$EXTERNALSYM DTN_WMKEYDOWN}
  DTN_WMKEYDOWN      = DTN_WMKEYDOWNA;
  {$EXTERNALSYM DTN_FORMAT}
  DTN_FORMAT         = DTN_FORMATA;
  {$EXTERNALSYM DTN_FORMATQUERY}
  DTN_FORMATQUERY    = DTN_FORMATQUERYA;


  // Ranges
  {$EXTERNALSYM GDTR_MIN}
  GDTR_MIN = $0001;
  {$EXTERNALSYM GDTR_MAX}
  GDTR_MAX = $0002;

  // Return Values
  {$EXTERNALSYM GDT_ERROR}
  GDT_ERROR = -1;
  {$EXTERNALSYM GDT_VALID}
  GDT_VALID = 0;
  {$EXTERNALSYM GDT_NONE}
  GDT_NONE  = 1;

type
  {$EXTERNALSYM tagNMDATETIMECHANGE}
  tagNMDATETIMECHANGE = packed record
    nmhdr: TNmHdr;
    dwFlags: DWORD;         // GDT_VALID or GDT_NONE
    st: TSystemTime;        // valid iff dwFlags = GDT_VALID
  end;
  PNMDateTimeChange = ^TNMDateTimeChange;
  TNMDateTimeChange = tagNMDATETIMECHANGE;

  {$EXTERNALSYM tagNMDATETIMESTRINGA}
  tagNMDATETIMESTRINGA = packed record
    nmhdr: TNmHdr;
    pszUserString: PAnsiChar;     // string user entered
    st: TSystemTime;           // app fills this in
    dwFlags: DWORD;            // GDT_VALID or GDT_NONE
  end;
  {$EXTERNALSYM tagNMDATETIMESTRINGW}
  tagNMDATETIMESTRINGW = packed record
    nmhdr: TNmHdr;
    pszUserString: PWideChar;     // string user entered
    st: TSystemTime;           // app fills this in
    dwFlags: DWORD;            // GDT_VALID or GDT_NONE
  end;
  {$EXTERNALSYM tagNMDATETIMESTRING}
  tagNMDATETIMESTRING = tagNMDATETIMESTRINGA;
  PNMDateTimeStringA = ^TNMDateTimeStringA;
  PNMDateTimeStringW = ^TNMDateTimeStringW;
  PNMDateTimeString = PNMDateTimeStringA;
  TNMDateTimeStringA = tagNMDATETIMESTRINGA;
  TNMDateTimeStringW = tagNMDATETIMESTRINGW;
  TNMDateTimeString = TNMDateTimeStringA;

  {$EXTERNALSYM tagNMDATETIMEWMKEYDOWNA}
  tagNMDATETIMEWMKEYDOWNA = packed record
    nmhdr: TNmHdr;
    nVirtKey: Integer; // virtual key code of WM_KEYDOWN which MODIFIES an X field
    pszFormat: PAnsiChar; // format substring
    st: TSystemTime;   // current systemtime, app should modify based on key
  end;
  {$EXTERNALSYM tagNMDATETIMEWMKEYDOWNW}
  tagNMDATETIMEWMKEYDOWNW = packed record
    nmhdr: TNmHdr;
    nVirtKey: Integer; // virtual key code of WM_KEYDOWN which MODIFIES an X field
    pszFormat: PWideChar; // format substring
    st: TSystemTime;   // current systemtime, app should modify based on key
  end;
  {$EXTERNALSYM tagNMDATETIMEWMKEYDOWN}
  tagNMDATETIMEWMKEYDOWN = tagNMDATETIMEWMKEYDOWNA;
  PNMDateTimeWMKeyDownA = ^TNMDateTimeWMKeyDownA;
  PNMDateTimeWMKeyDownW = ^TNMDateTimeWMKeyDownW;
  PNMDateTimeWMKeyDown = PNMDateTimeWMKeyDownA;
  TNMDateTimeWMKeyDownA = tagNMDATETIMEWMKEYDOWNA;
  TNMDateTimeWMKeyDownW = tagNMDATETIMEWMKEYDOWNW;
  TNMDateTimeWMKeyDown = TNMDateTimeWMKeyDownA;

  {$EXTERNALSYM tagNMDATETIMEFORMATA}
  tagNMDATETIMEFORMATA = packed record
    nmhdr: TNmHdr;
    pszFormat: PAnsiChar;                // format substring
    st: TSystemTime;                  // current systemtime
    pszDisplay: PAnsiChar;               // string to display
    szDisplay: array[0..63] of AnsiChar; // buffer pszDisplay originally points at
  end;
  {$EXTERNALSYM tagNMDATETIMEFORMATW}
  tagNMDATETIMEFORMATW = packed record
    nmhdr: TNmHdr;
    pszFormat: PWideChar;                // format substring
    st: TSystemTime;                  // current systemtime
    pszDisplay: PWideChar;               // string to display
    szDisplay: array[0..63] of WideChar; // buffer pszDisplay originally points at
  end;
  {$EXTERNALSYM tagNMDATETIMEFORMAT}
  tagNMDATETIMEFORMAT = tagNMDATETIMEFORMATA;
  PNMDateTimeFormatA = ^TNMDateTimeFormatA;
  PNMDateTimeFormatW = ^TNMDateTimeFormatW;
  PNMDateTimeFormat = PNMDateTimeFormatA;
  TNMDateTimeFormatA = tagNMDATETIMEFORMATA;
  TNMDateTimeFormatW = tagNMDATETIMEFORMATW;
  TNMDateTimeFormat = TNMDateTimeFormatA;

  {$EXTERNALSYM tagNMDATETIMEFORMATQUERYA}
  tagNMDATETIMEFORMATQUERYA = packed record
    nmhdr: TNmHdr;
    pszFormat: PAnsiChar; // format substring
    szMax: TSize;      // max bounding rectangle app will use for this format string
  end;
  {$EXTERNALSYM tagNMDATETIMEFORMATQUERYW}
  tagNMDATETIMEFORMATQUERYW = packed record
    nmhdr: TNmHdr;
    pszFormat: PWideChar; // format substring
    szMax: TSize;      // max bounding rectangle app will use for this format string
  end;
  {$EXTERNALSYM tagNMDATETIMEFORMATQUERY}
  tagNMDATETIMEFORMATQUERY = tagNMDATETIMEFORMATQUERYA;
  PNMDateTimeFormatQueryA = ^TNMDateTimeFormatQueryA;
  PNMDateTimeFormatQueryW = ^TNMDateTimeFormatQueryW;
  PNMDateTimeFormatQuery = PNMDateTimeFormatQueryA;
  TNMDateTimeFormatQueryA = tagNMDATETIMEFORMATQUERYA;
  TNMDateTimeFormatQueryW = tagNMDATETIMEFORMATQUERYW;
  TNMDateTimeFormatQuery = TNMDateTimeFormatQueryA;

//   returns GDT_NONE if "none" is selected (DTS_SHOWNONE only)
//   returns GDT_VALID and modifies pst to be the currently selected value
{$EXTERNALSYM DateTime_GetSystemTime}
function DateTime_GetSystemTime(hdp: HWND; var pst: TSystemTime): DWORD; inline;

//   if gd = GDT_NONE, sets datetimepick to None (DTS_SHOWNONE only)
//   if gd = GDT_VALID, sets datetimepick to pst
//   returns TRUE on success, FALSE on error (such as bad params)
{$EXTERNALSYM DateTime_SetSystemTime}
function DateTime_SetSystemTime(hdp: HWND; gd: DWORD; const pst: TSystemTime): BOOL; inline;

//   modifies rgst[0] to be the minimum ALLOWABLE systemtime (or 0 if no minimum)
//   modifies rgst[1] to be the maximum ALLOWABLE systemtime (or 0 if no maximum)
//   returns GDTR_MIN or GDTR_MAX if there is a minimum or maximum limit
{$EXTERNALSYM DateTime_GetRange}
function DateTime_GetRange(hdp: HWND; rgst: PSystemTime): DWORD; inline;

//   if GDTR_MIN, sets the minimum ALLOWABLE systemtime to rgst[0], otherwise removes minimum
//   if GDTR_MAX, sets the maximum ALLOWABLE systemtime to rgst[1], otherwise removes maximum
//   returns TRUE on success, FALSE on error (such as invalid parameters)
{$EXTERNALSYM DateTime_SetRange}
function DateTime_SetRange(hdp: HWND; gdtr: DWORD; rgst: PSystemTime): BOOL; inline;

//   sets the display formatting string to sz (see GetDateFormat and GetTimeFormat for valid formatting chars)
//   NOTE: 'X' is a valid formatting character which indicates that the application
//   will determine how to display information. Such apps must support DTN_WMKEYDOWN,
//   DTN_FORMAT, and DTN_FORMATQUERY.
{$EXTERNALSYM DateTime_SetFormat}
function DateTime_SetFormat(hdp: HWND; sz: PChar): BOOL; inline;
{$EXTERNALSYM DateTime_SetFormatA}
function DateTime_SetFormatA(hdp: HWND; sz: PAnsiChar): BOOL; inline;
{$EXTERNALSYM DateTime_SetFormatW}
function DateTime_SetFormatW(hdp: HWND; sz: PWideChar): BOOL; inline;

{$EXTERNALSYM DateTime_SetMonthCalColor}
function DateTime_SetMonthCalColor(hdp: HWND; iColor: DWORD; clr: TColorRef): TColorRef; inline;

{$EXTERNALSYM DateTime_GetMonthCalColor}
function DateTime_GetMonthCalColor(hdp: HWND; iColor: DWORD): TColorRef; inline;

// returns the HWND of the MonthCal popup window. Only valid
// between DTN_DROPDOWN and DTN_CLOSEUP notifications.
{$EXTERNALSYM DateTime_GetMonthCal}
function DateTime_GetMonthCal(hdp: HWND): HWND; inline;

{$EXTERNALSYM DateTime_SetMonthCalFont}
procedure DateTime_SetMonthCalFont(hdp: HWND; hfont: HFONT; fRedraw: BOOL); inline;

{$EXTERNALSYM DateTime_GetMonthCalFont}
function DateTime_GetMonthCalFont(hdp: HWND): HFONT; inline;

{  ====================== IP Address edit control ============================= }

const
  {$EXTERNALSYM WC_IPADDRESS}
  WC_IPADDRESS         = 'SysIPAddress32';

  // Messages sent to IPAddress controls
  {$EXTERNALSYM IPM_CLEARADDRESS}
  IPM_CLEARADDRESS     = WM_USER+100;  { no parameters }
  {$EXTERNALSYM IPM_SETADDRESS}
  IPM_SETADDRESS       = WM_USER+101;  { lparam = TCP/IP address }
  {$EXTERNALSYM IPM_GETADDRESS}
  IPM_GETADDRESS       = WM_USER+102;  { lresult = # of non black fields.  lparam = LPDWORD for TCP/IP address }
  {$EXTERNALSYM IPM_SETRANGE}
  IPM_SETRANGE         = WM_USER+103;  { wparam = field, lparam = range }
  {$EXTERNALSYM IPM_SETFOCUS}
  IPM_SETFOCUS         = WM_USER+104;  { wparam = field }
  {$EXTERNALSYM IPM_ISBLANK}
  IPM_ISBLANK          = WM_USER+105;  { no parameters }

  {$EXTERNALSYM IPN_FIELDCHANGED}
  IPN_FIELDCHANGED     = IPN_FIRST - 0;

type
  {$EXTERNALSYM tagNMIPADDRESS}
  tagNMIPADDRESS = packed record
    hdr: NMHDR;
    iField: Integer;
    iValue: Integer;
  end;
  PNMIPAddress = ^TNMIPAddress;
  TNMIPAddress = tagNMIPADDRESS;

{ The following is a useful macro for passing the range values in the }
{ IPM_SETRANGE message. }
{$EXTERNALSYM MAKEIPRANGE}
function MAKEIPRANGE(low, high: Byte): LPARAM; inline;

{ And this is a useful macro for making the IP Address to be passed }
{ as a LPARAM. }
{$EXTERNALSYM MAKEIPADDRESS}
function MAKEIPADDRESS(b1, b2, b3, b4: DWORD): LPARAM;

{ Get individual number }
{$EXTERNALSYM FIRST_IPADDRESS}
function FIRST_IPADDRESS(x: DWORD): DWORD; inline;

{$EXTERNALSYM SECOND_IPADDRESS}
function SECOND_IPADDRESS(x: DWORD): DWORD; inline;

{$EXTERNALSYM THIRD_IPADDRESS}
function THIRD_IPADDRESS(x: DWORD): DWORD; inline;

{$EXTERNALSYM FOURTH_IPADDRESS}
function FOURTH_IPADDRESS(x: DWORD): DWORD; inline;

{  ====================== Pager Control ============================= }

const
  { Pager Class Name }
  {$EXTERNALSYM WC_PAGESCROLLER}
  WC_PAGESCROLLER               = 'SysPager';

  { Pager Control Styles }
  {$EXTERNALSYM PGS_VERT}
  PGS_VERT                    = $00000000;
  {$EXTERNALSYM PGS_HORZ}
  PGS_HORZ                    = $00000001;
  {$EXTERNALSYM PGS_AUTOSCROLL}
  PGS_AUTOSCROLL              = $00000002;
  {$EXTERNALSYM PGS_DRAGNDROP}
  PGS_DRAGNDROP               = $00000004;

  { Pager Button State }
  { The scroll can be in one of the following control State }
  {$EXTERNALSYM PGF_INVISIBLE}
  PGF_INVISIBLE        = 0;     { Scroll button is not visible }
  {$EXTERNALSYM PGF_NORMAL}
  PGF_NORMAL           = 1;     { Scroll button is in normal state }
  {$EXTERNALSYM PGF_GRAYED}
  PGF_GRAYED           = 2;     { Scroll button is in grayed state }
  {$EXTERNALSYM PGF_DEPRESSED}
  PGF_DEPRESSED        = 4;     { Scroll button is in depressed state }
  {$EXTERNALSYM PGF_HOT}
  PGF_HOT              = 8;     { Scroll button is in hot state }

  { The following identifiers specifies the button control }
  {$EXTERNALSYM PGB_TOPORLEFT}
  PGB_TOPORLEFT           = 0;
  {$EXTERNALSYM PGB_BOTTOMORRIGHT}
  PGB_BOTTOMORRIGHT       = 1;

  { Pager Control  Messages }
  {$EXTERNALSYM PGM_SETCHILD}
  PGM_SETCHILD                = PGM_FIRST + 1;   { lParam == hwnd }
  {$EXTERNALSYM PGM_RECALCSIZE}
  PGM_RECALCSIZE              = PGM_FIRST + 2;
  {$EXTERNALSYM PGM_FORWARDMOUSE}
  PGM_FORWARDMOUSE            = PGM_FIRST + 3;
  {$EXTERNALSYM PGM_SETBKCOLOR}
  PGM_SETBKCOLOR              = PGM_FIRST + 4;
  {$EXTERNALSYM PGM_GETBKCOLOR}
  PGM_GETBKCOLOR              = PGM_FIRST + 5;
  {$EXTERNALSYM PGM_SETBORDER}
  PGM_SETBORDER              = PGM_FIRST + 6;
  {$EXTERNALSYM PGM_GETBORDER}
  PGM_GETBORDER              = PGM_FIRST + 7;
  {$EXTERNALSYM PGM_SETPOS}
  PGM_SETPOS                  = PGM_FIRST + 8;
  {$EXTERNALSYM PGM_GETPOS}
  PGM_GETPOS                  = PGM_FIRST + 9;
  {$EXTERNALSYM PGM_SETBUTTONSIZE}
  PGM_SETBUTTONSIZE           = PGM_FIRST + 10;
  {$EXTERNALSYM PGM_GETBUTTONSIZE}
  PGM_GETBUTTONSIZE           = PGM_FIRST + 11;
  {$EXTERNALSYM PGM_GETBUTTONSTATE}
  PGM_GETBUTTONSTATE          = PGM_FIRST + 12;
  {$EXTERNALSYM PGM_GETDROPTARGET}
  PGM_GETDROPTARGET           = CCM_GETDROPTARGET;

{$EXTERNALSYM Pager_SetChild}
procedure Pager_SetChild(hwnd: HWND; hwndChild: HWND); inline;
{$EXTERNALSYM Pager_RecalcSize}
procedure Pager_RecalcSize(hwnd: HWND); inline;
{$EXTERNALSYM Pager_ForwardMouse}
procedure Pager_ForwardMouse(hwnd: HWND; bForward: BOOL); inline;
{$EXTERNALSYM Pager_SetBkColor}
function Pager_SetBkColor(hwnd: HWND; clr: COLORREF): COLORREF; inline;
{$EXTERNALSYM Pager_GetBkColor}
function Pager_GetBkColor(hwnd: HWND): COLORREF; inline;
{$EXTERNALSYM Pager_SetBorder}
function Pager_SetBorder(hwnd: HWND; iBorder: Integer): Integer; inline;
{$EXTERNALSYM Pager_GetBorder}
function Pager_GetBorder(hwnd: HWND): Integer; inline;
{$EXTERNALSYM Pager_SetPos}
function Pager_SetPos(hwnd: HWND; iPos: Integer): Integer; inline;
{$EXTERNALSYM Pager_GetPos}
function Pager_GetPos(hwnd: HWND): Integer; inline;
{$EXTERNALSYM Pager_SetButtonSize}
function Pager_SetButtonSize(hwnd: HWND; iSize: Integer): Integer; inline;
{$EXTERNALSYM Pager_GetButtonSize}
function Pager_GetButtonSize(hwnd: HWND): Integer; inline;
{$EXTERNALSYM Pager_GetButtonState}
function Pager_GetButtonState(hwnd: HWND; iButton: Integer): DWORD; inline;
{$EXTERNALSYM Pager_GetDropTarget}
procedure Pager_GetDropTarget(hwnd: HWND; ppdt: Pointer{!!}); inline;

const
  { Pager Control Notification Messages }

  { PGN_SCROLL Notification Message }
  {$EXTERNALSYM PGN_SCROLL}
  PGN_SCROLL              = PGN_FIRST-1;

  {$EXTERNALSYM PGF_SCROLLUP}
  PGF_SCROLLUP            = 1;
  {$EXTERNALSYM PGF_SCROLLDOWN}
  PGF_SCROLLDOWN          = 2;
  {$EXTERNALSYM PGF_SCROLLLEFT}
  PGF_SCROLLLEFT          = 4;
  {$EXTERNALSYM PGF_SCROLLRIGHT}
  PGF_SCROLLRIGHT         = 8;

  { Keys down }
  {$EXTERNALSYM PGK_SHIFT}
  PGK_SHIFT               = 1;
  {$EXTERNALSYM PGK_CONTROL}
  PGK_CONTROL             = 2;
  {$EXTERNALSYM PGK_MENU}
  PGK_MENU                = 4;

type
  { This structure is sent along with PGN_SCROLL notifications }
  {$EXTERNALSYM NMPGSCROLL}
  NMPGSCROLL = packed record
    hdr: NMHDR;
    fwKeys: Word;           { Specifies which keys are down when this notification is send }
    rcParent: TRect;        { Contains Parent Window Rect }
    iDir: Integer;          { Scrolling Direction }
    iXpos: Integer;         { Horizontal scroll position }
    iYpos: Integer;         { Vertical scroll position }
    iScroll: Integer;       { [in/out] Amount to scroll }
  end;
  PNMPGScroll = ^TNMPGScroll;
  TNMPGScroll = NMPGSCROLL;

const
  { PGN_CALCSIZE Notification Message }
  {$EXTERNALSYM PGN_CALCSIZE}
  PGN_CALCSIZE            = PGN_FIRST-2;

  {$EXTERNALSYM PGF_CALCWIDTH}
  PGF_CALCWIDTH           = 1;
  {$EXTERNALSYM PGF_CALCHEIGHT}
  PGF_CALCHEIGHT          = 2;

type
  {$EXTERNALSYM NMPGCALCSIZE}
  NMPGCALCSIZE = packed record
    hdr: NMHDR;
    dwFlag: DWORD;
    iWidth: Integer;
    iHeight: Integer;
  end;
  PNMPGCalcSize = ^TNMPGCalcSize;
  TNMPGCalcSize = NMPGCALCSIZE;

{ ======================  Native Font Control ============================== }

const
  {$EXTERNALSYM WC_NATIVEFONTCTL}
  WC_NATIVEFONTCTL            = 'NativeFontCtl';

  { style definition }
  {$EXTERNALSYM NFS_EDIT}
  NFS_EDIT                    = $0001;
  {$EXTERNALSYM NFS_STATIC}
  NFS_STATIC                  = $0002;
  {$EXTERNALSYM NFS_LISTCOMBO}
  NFS_LISTCOMBO               = $0004;
  {$EXTERNALSYM NFS_BUTTON}
  NFS_BUTTON                  = $0008;
  {$EXTERNALSYM NFS_ALL}
  NFS_ALL                     = $0010;

{ ====== TrackMouseEvent  ================================================== }

const
  {$EXTERNALSYM WM_MOUSEHOVER}
  WM_MOUSEHOVER                       = $02A1;
  {$EXTERNALSYM WM_MOUSELEAVE}
  WM_MOUSELEAVE                       = $02A3;

  {$EXTERNALSYM TME_HOVER}
  TME_HOVER           = $00000001;
  {$EXTERNALSYM TME_LEAVE}
  TME_LEAVE           = $00000002;
  {$EXTERNALSYM TME_QUERY}
  TME_QUERY           = $40000000;
  {$EXTERNALSYM TME_CANCEL}
  TME_CANCEL          = $80000000;

  {$EXTERNALSYM HOVER_DEFAULT}
  HOVER_DEFAULT       = $FFFFFFFF;

type
  {$EXTERNALSYM tagTRACKMOUSEEVENT}
  tagTRACKMOUSEEVENT = packed record
    cbSize: DWORD;
    dwFlags: DWORD;
    hwndTrack: HWND;
    dwHoverTime: DWORD;
  end;
  PTrackMouseEvent = ^TTrackMouseEvent;
  TTrackMouseEvent = tagTRACKMOUSEEVENT;

{ Declare _TrackMouseEvent.  This API tries to use the window manager's }
{ implementation of TrackMouseEvent if it is present, otherwise it emulates. }
{$EXTERNALSYM _TrackMouseEvent}
function _TrackMouseEvent(lpEventTrack: PTrackMouseEvent): BOOL; stdcall;

{ ====== Flat Scrollbar APIs========================================= }

const
  {$EXTERNALSYM WSB_PROP_CYVSCROLL}
  WSB_PROP_CYVSCROLL      = $00000001;
  {$EXTERNALSYM WSB_PROP_CXHSCROLL}
  WSB_PROP_CXHSCROLL      = $00000002;
  {$EXTERNALSYM WSB_PROP_CYHSCROLL}
  WSB_PROP_CYHSCROLL      = $00000004;
  {$EXTERNALSYM WSB_PROP_CXVSCROLL}
  WSB_PROP_CXVSCROLL      = $00000008;
  {$EXTERNALSYM WSB_PROP_CXHTHUMB}
  WSB_PROP_CXHTHUMB       = $00000010;
  {$EXTERNALSYM WSB_PROP_CYVTHUMB}
  WSB_PROP_CYVTHUMB       = $00000020;
  {$EXTERNALSYM WSB_PROP_VBKGCOLOR}
  WSB_PROP_VBKGCOLOR      = $00000040;
  {$EXTERNALSYM WSB_PROP_HBKGCOLOR}
  WSB_PROP_HBKGCOLOR      = $00000080;
  {$EXTERNALSYM WSB_PROP_VSTYLE}
  WSB_PROP_VSTYLE         = $00000100;
  {$EXTERNALSYM WSB_PROP_HSTYLE}
  WSB_PROP_HSTYLE         = $00000200;
  {$EXTERNALSYM WSB_PROP_WINSTYLE}
  WSB_PROP_WINSTYLE       = $00000400;
  {$EXTERNALSYM WSB_PROP_PALETTE}
  WSB_PROP_PALETTE        = $00000800;
  {$EXTERNALSYM WSB_PROP_MASK}
  WSB_PROP_MASK           = $00000FFF;

  {$EXTERNALSYM FSB_FLAT_MODE}
  FSB_FLAT_MODE               = 2;
  {$EXTERNALSYM FSB_ENCARTA_MODE}
  FSB_ENCARTA_MODE            = 1;
  {$EXTERNALSYM FSB_REGULAR_MODE}
  FSB_REGULAR_MODE            = 0;

{$EXTERNALSYM FlatSB_EnableScrollBar}
function FlatSB_EnableScrollBar(hWnd: HWND; wSBflags, wArrows: UINT): BOOL; stdcall;
{$EXTERNALSYM FlatSB_ShowScrollBar}
function FlatSB_ShowScrollBar(hWnd: HWND; wBar: Integer; bShow: BOOL): BOOL; stdcall;

{$EXTERNALSYM FlatSB_GetScrollRange}
function FlatSB_GetScrollRange(hWnd: HWND; nBar: Integer; var lpMinPos,
  lpMaxPos: Integer): BOOL; stdcall;
{$EXTERNALSYM FlatSB_GetScrollInfo}
function FlatSB_GetScrollInfo(hWnd: HWND; BarFlag: Integer;
  var ScrollInfo: TScrollInfo): BOOL; stdcall;
{$EXTERNALSYM FlatSB_GetScrollPos}
function FlatSB_GetScrollPos(hWnd: HWND; nBar: Integer): Integer; stdcall;
{$EXTERNALSYM FlatSB_GetScrollProp}
function FlatSB_GetScrollProp(p1: HWND; propIndex: Integer;
  p3: PInteger): Bool; stdcall;

{$EXTERNALSYM FlatSB_SetScrollPos}
function FlatSB_SetScrollPos(hWnd: HWND; nBar, nPos: Integer;
  bRedraw: BOOL): Integer; stdcall;
{$EXTERNALSYM FlatSB_SetScrollInfo}
function FlatSB_SetScrollInfo(hWnd: HWND; BarFlag: Integer;
  const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;
{$EXTERNALSYM FlatSB_SetScrollRange}
function FlatSB_SetScrollRange(hWnd: HWND; nBar, nMinPos, nMaxPos: Integer;
  bRedraw: BOOL): BOOL; stdcall;
{$EXTERNALSYM FlatSB_SetScrollProp}
function FlatSB_SetScrollProp(p1: HWND; index: Integer; newValue: Integer;
  p4: Bool): Bool; stdcall;

{$EXTERNALSYM InitializeFlatSB}
function InitializeFlatSB(hWnd: HWND): Bool; stdcall;
{$EXTERNALSYM UninitializeFlatSB}
procedure UninitializeFlatSB(hWnd: HWND); stdcall;

implementation

