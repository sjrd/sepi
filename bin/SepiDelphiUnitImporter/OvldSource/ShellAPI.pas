Around line 1194 in Delphi 2010
----------
function SHGetNewLinkInfo(pszLinkTo: PWideChar; pszDir: PWideChar; pszName: PWideChar; 
  var pfMustCopy: BOOL; uFlags: UINT): BOOL; stdcall
{$EXTERNALSYM SHGetNewLinkInfo}
function SHGetNewLinkInfoA(pszLinkTo: PAnsiChar; pszDir: PAnsiChar; pszName: PAnsiChar; 
  var pfMustCopy: BOOL; uFlags: UINT): BOOL; stdcall
{$EXTERNALSYM SHGetNewLinkInfoA}
function SHGetNewLinkInfoW(pszLinkTo: PWideChar; pszDir: PWideChar; pszName: PWideChar; 
  var pfMustCopy: BOOL; uFlags: UINT): BOOL; stdcall
{$EXTERNALSYM SHGetNewLinkInfoW}
----------
function SHGetNewLinkInfo(pszLinkTo: PWideChar; pszDir: PWideChar; pszName: PWideChar; 
  var pfMustCopy: BOOL; uFlags: UINT): BOOL; stdcall;
{$EXTERNALSYM SHGetNewLinkInfo}
function SHGetNewLinkInfoA(pszLinkTo: PAnsiChar; pszDir: PAnsiChar; pszName: PAnsiChar; 
  var pfMustCopy: BOOL; uFlags: UINT): BOOL; stdcall;
{$EXTERNALSYM SHGetNewLinkInfoA}
function SHGetNewLinkInfoW(pszLinkTo: PWideChar; pszDir: PWideChar; pszName: PWideChar; 
  var pfMustCopy: BOOL; uFlags: UINT): BOOL; stdcall;
{$EXTERNALSYM SHGetNewLinkInfoW}
----------

Around line 1370 in Delphi 2010
----------
function ShellMessageBox(hAppInst: HINST;  hWnd: HWND; pcText: PWideChar;
  lpcTitle: PWideChar; fuStyle: UINT): Integer; cdecl; varargs;
{$EXTERNALSYM ShellMessageBox}
function ShellMessageBoxA(hAppInst: HINST;  hWnd: HWND; pcText: PAnsiChar;
  lpcTitle: PAnsiChar; fuStyle: UINT): Integer; cdecl; varargs;
{$EXTERNALSYM ShellMessageBoxA}
function ShellMessageBoxW(hAppInst: HINST;  hWnd: HWND; pcText: PWideChar;
  lpcTitle: PWideChar; fuStyle: UINT): Integer; cdecl; varargs;
{$EXTERNALSYM ShellMessageBoxW}
----------
function ShellMessageBox(hAppInst: HINST;  hWnd: HWND; pcText: PWideChar;
  lpcTitle: PWideChar; fuStyle: UINT): Integer; cdecl;
{$EXTERNALSYM ShellMessageBox}
function ShellMessageBoxA(hAppInst: HINST;  hWnd: HWND; pcText: PAnsiChar;
  lpcTitle: PAnsiChar; fuStyle: UINT): Integer; cdecl;
{$EXTERNALSYM ShellMessageBoxA}
function ShellMessageBoxW(hAppInst: HINST;  hWnd: HWND; pcText: PWideChar;
  lpcTitle: PWideChar; fuStyle: UINT): Integer; cdecl;
{$EXTERNALSYM ShellMessageBoxW}
----------
