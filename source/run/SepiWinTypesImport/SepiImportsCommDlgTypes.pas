{*
  Importe l'unité CommDlg dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsCommDlgTypes;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, ShlObj, CommDlg;

implementation

{ You must not localize any of the strings this unit contains! }

{----------------}
{ tagOFNA import }
{----------------}

function SepiImporttagOFNA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagOFNA', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWND));
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('lpstrFilter', 'PAnsiChar');
    AddField('lpstrCustomFilter', 'PAnsiChar');
    AddField('nMaxCustFilter', System.TypeInfo(DWORD));
    AddField('nFilterIndex', System.TypeInfo(DWORD));
    AddField('lpstrFile', 'PAnsiChar');
    AddField('nMaxFile', System.TypeInfo(DWORD));
    AddField('lpstrFileTitle', 'PAnsiChar');
    AddField('nMaxFileTitle', System.TypeInfo(DWORD));
    AddField('lpstrInitialDir', 'PAnsiChar');
    AddField('lpstrTitle', 'PAnsiChar');
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('nFileOffset', System.TypeInfo(Word));
    AddField('nFileExtension', System.TypeInfo(Word));
    AddField('lpstrDefExt', 'PAnsiChar');
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnHook', '$1');
    AddField('lpTemplateName', 'PAnsiChar');
    AddField('pvReserved', 'Pointer');
    AddField('dwReserved', System.TypeInfo(DWORD));
    AddField('FlagsEx', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{----------------}
{ tagOFNW import }
{----------------}

function SepiImporttagOFNW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagOFNW', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWND));
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('lpstrFilter', 'PWideChar');
    AddField('lpstrCustomFilter', 'PWideChar');
    AddField('nMaxCustFilter', System.TypeInfo(DWORD));
    AddField('nFilterIndex', System.TypeInfo(DWORD));
    AddField('lpstrFile', 'PWideChar');
    AddField('nMaxFile', System.TypeInfo(DWORD));
    AddField('lpstrFileTitle', 'PWideChar');
    AddField('nMaxFileTitle', System.TypeInfo(DWORD));
    AddField('lpstrInitialDir', 'PWideChar');
    AddField('lpstrTitle', 'PWideChar');
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('nFileOffset', System.TypeInfo(Word));
    AddField('nFileExtension', System.TypeInfo(Word));
    AddField('lpstrDefExt', 'PWideChar');
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnHook', '$2');
    AddField('lpTemplateName', 'PWideChar');
    AddField('pvReserved', 'Pointer');
    AddField('dwReserved', System.TypeInfo(DWORD));
    AddField('FlagsEx', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------------}
{ _OFNOTIFYA import }
{-------------------}

function SepiImport_OFNOTIFYA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_OFNOTIFYA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('lpOFN', 'POpenFilenameA');
    AddField('pszFile', 'PAnsiChar');

    Complete;
  end;
end;

{-------------------}
{ _OFNOTIFYW import }
{-------------------}

function SepiImport_OFNOTIFYW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_OFNOTIFYW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('lpOFN', 'POpenFilenameW');
    AddField('pszFile', 'PWideChar');

    Complete;
  end;
end;

{---------------------}
{ _OFNOTIFYEXA import }
{---------------------}

function SepiImport_OFNOTIFYEXA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_OFNOTIFYEXA', True, True,
    TypeInfo(_OFNOTIFYEXA));

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('lpOFN', 'POpenFilenameA');
    AddField('psf', System.TypeInfo(IShellFolder));
    AddField('pidl', 'Pointer');

    Complete;
  end;
end;

{---------------------}
{ _OFNOTIFYEXW import }
{---------------------}

function SepiImport_OFNOTIFYEXW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_OFNOTIFYEXW', True, True,
    TypeInfo(_OFNOTIFYEXW));

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('lpOFN', 'POpenFilenameW');
    AddField('psf', System.TypeInfo(IShellFolder));
    AddField('pidl', 'Pointer');

    Complete;
  end;
end;

{------------------------}
{ tagCHOOSECOLORA import }
{------------------------}

function SepiImporttagCHOOSECOLORA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCHOOSECOLORA', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWND));
    AddField('hInstance', System.TypeInfo(HWND));
    AddField('rgbResult', System.TypeInfo(COLORREF));
    AddField('lpCustColors', '$3');
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnHook', '$4');
    AddField('lpTemplateName', 'PAnsiChar');

    Complete;
  end;
end;

{------------------------}
{ tagCHOOSECOLORW import }
{------------------------}

function SepiImporttagCHOOSECOLORW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCHOOSECOLORW', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWND));
    AddField('hInstance', System.TypeInfo(HWND));
    AddField('rgbResult', System.TypeInfo(COLORREF));
    AddField('lpCustColors', '$5');
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnHook', '$6');
    AddField('lpTemplateName', 'PWideChar');

    Complete;
  end;
end;

{------------------------}
{ tagFINDREPLACEA import }
{------------------------}

function SepiImporttagFINDREPLACEA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagFINDREPLACEA', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWND));
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('lpstrFindWhat', 'PAnsiChar');
    AddField('lpstrReplaceWith', 'PAnsiChar');
    AddField('wFindWhatLen', System.TypeInfo(Word));
    AddField('wReplaceWithLen', System.TypeInfo(Word));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnHook', '$7');
    AddField('lpTemplateName', 'PAnsiChar');

    Complete;
  end;
end;

{------------------------}
{ tagFINDREPLACEW import }
{------------------------}

function SepiImporttagFINDREPLACEW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagFINDREPLACEW', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWND));
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('lpstrFindWhat', 'PWideChar');
    AddField('lpstrReplaceWith', 'PWideChar');
    AddField('wFindWhatLen', System.TypeInfo(Word));
    AddField('wReplaceWithLen', System.TypeInfo(Word));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnHook', '$8');
    AddField('lpTemplateName', 'PWideChar');

    Complete;
  end;
end;

{-----------------------}
{ tagCHOOSEFONTA import }
{-----------------------}

function SepiImporttagCHOOSEFONTA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCHOOSEFONTA', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWnd));
    AddField('hDC', System.TypeInfo(HDC));
    AddField('lpLogFont', 'PLogFontA');
    AddField('iPointSize', System.TypeInfo(Integer));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('rgbColors', System.TypeInfo(COLORREF));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnHook', '$9');
    AddField('lpTemplateName', 'PAnsiChar');
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('lpszStyle', 'PAnsiChar');
    AddField('nFontType', System.TypeInfo(Word));
    AddField('wReserved', System.TypeInfo(Word));
    AddField('nSizeMin', System.TypeInfo(Integer));
    AddField('nSizeMax', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-----------------------}
{ tagCHOOSEFONTW import }
{-----------------------}

function SepiImporttagCHOOSEFONTW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCHOOSEFONTW', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWnd));
    AddField('hDC', System.TypeInfo(HDC));
    AddField('lpLogFont', 'PLogFontW');
    AddField('iPointSize', System.TypeInfo(Integer));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('rgbColors', System.TypeInfo(COLORREF));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnHook', '$10');
    AddField('lpTemplateName', 'PWideChar');
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('lpszStyle', 'PWideChar');
    AddField('nFontType', System.TypeInfo(Word));
    AddField('wReserved', System.TypeInfo(Word));
    AddField('nSizeMin', System.TypeInfo(Integer));
    AddField('nSizeMax', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------}
{ tagPDA import }
{---------------}

function SepiImporttagPDA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPDA', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWND));
    AddField('hDevMode', System.TypeInfo(HGLOBAL));
    AddField('hDevNames', System.TypeInfo(HGLOBAL));
    AddField('hDC', System.TypeInfo(HDC));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('nFromPage', System.TypeInfo(Word));
    AddField('nToPage', System.TypeInfo(Word));
    AddField('nMinPage', System.TypeInfo(Word));
    AddField('nMaxPage', System.TypeInfo(Word));
    AddField('nCopies', System.TypeInfo(Word));
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnPrintHook', '$11');
    AddField('lpfnSetupHook', '$12');
    AddField('lpPrintTemplateName', 'PAnsiChar');
    AddField('lpSetupTemplateName', 'PAnsiChar');
    AddField('hPrintTemplate', System.TypeInfo(HGLOBAL));
    AddField('hSetupTemplate', System.TypeInfo(HGLOBAL));

    Complete;
  end;
end;

{---------------}
{ tagPDW import }
{---------------}

function SepiImporttagPDW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPDW', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hWndOwner', System.TypeInfo(HWND));
    AddField('hDevMode', System.TypeInfo(HGLOBAL));
    AddField('hDevNames', System.TypeInfo(HGLOBAL));
    AddField('hDC', System.TypeInfo(HDC));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('nFromPage', System.TypeInfo(Word));
    AddField('nToPage', System.TypeInfo(Word));
    AddField('nMinPage', System.TypeInfo(Word));
    AddField('nMaxPage', System.TypeInfo(Word));
    AddField('nCopies', System.TypeInfo(Word));
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnPrintHook', '$13');
    AddField('lpfnSetupHook', '$14');
    AddField('lpPrintTemplateName', 'PWideChar');
    AddField('lpSetupTemplateName', 'PWideChar');
    AddField('hPrintTemplate', System.TypeInfo(HGLOBAL));
    AddField('hSetupTemplate', System.TypeInfo(HGLOBAL));

    Complete;
  end;
end;

{--------------------}
{ tagDEVNAMES import }
{--------------------}

function SepiImporttagDEVNAMES(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagDEVNAMES', False, True);

  with Result do
  begin
    AddField('wDriverOffset', System.TypeInfo(Word));
    AddField('wDeviceOffset', System.TypeInfo(Word));
    AddField('wOutputOffset', System.TypeInfo(Word));
    AddField('wDefault', System.TypeInfo(Word));

    Complete;
  end;
end;

{----------------}
{ tagPSDA import }
{----------------}

function SepiImporttagPSDA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPSDA', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hwndOwner', System.TypeInfo(HWND));
    AddField('hDevMode', System.TypeInfo(HGLOBAL));
    AddField('hDevNames', System.TypeInfo(HGLOBAL));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('ptPaperSize', 'TPoint');
    AddField('rtMinMargin', 'TRect');
    AddField('rtMargin', 'TRect');
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnPageSetupHook', '$15');
    AddField('lpfnPagePaintHook', '$16');
    AddField('lpPageSetupTemplateName', 'PAnsiChar');
    AddField('hPageSetupTemplate', System.TypeInfo(HGLOBAL));

    Complete;
  end;
end;

{----------------}
{ tagPSDW import }
{----------------}

function SepiImporttagPSDW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagPSDW', True, True);

  with Result do
  begin
    AddField('lStructSize', System.TypeInfo(DWORD));
    AddField('hwndOwner', System.TypeInfo(HWND));
    AddField('hDevMode', System.TypeInfo(HGLOBAL));
    AddField('hDevNames', System.TypeInfo(HGLOBAL));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('ptPaperSize', 'TPoint');
    AddField('rtMinMargin', 'TRect');
    AddField('rtMargin', 'TRect');
    AddField('hInstance', System.TypeInfo(HINST));
    AddField('lCustData', System.TypeInfo(LPARAM));
    AddField('lpfnPageSetupHook', '$17');
    AddField('lpfnPagePaintHook', '$18');
    AddField('lpPageSetupTemplateName', 'PWideChar');
    AddField('hPageSetupTemplate', System.TypeInfo(HGLOBAL));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'CommDlgTypes',
    ['Windows', 'Messages', 'ShlObjTypes']);

  // Types
  TSepiPointerType.Create(Result, 'POpenFilenameA', 'TOpenFilenameA', True);
  TSepiPointerType.Create(Result, 'POpenFilenameW', 'TOpenFilenameW', True);
  TSepiTypeAlias.Create(Result, 'POpenFilename', 'POpenFilenameA');
  TSepiMethodRefType.Create(Result, '$1',
    'function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagOFNA(Result);
  TSepiMethodRefType.Create(Result, '$2',
    'function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagOFNW(Result);
  TSepiTypeAlias.Create(Result, 'tagOFN', 'tagOFNA');
  TSepiTypeAlias.Create(Result, 'TOpenFilenameA', 'tagOFNA');
  TSepiTypeAlias.Create(Result, 'TOpenFilenameW', 'tagOFNW');
  TSepiTypeAlias.Create(Result, 'TOpenFilename', 'TOpenFilenameA');
  TSepiTypeAlias.Create(Result, 'OPENFILENAMEA', 'tagOFNA');
  TSepiTypeAlias.Create(Result, 'OPENFILENAMEW', 'tagOFNW');
  TSepiTypeAlias.Create(Result, 'OPENFILENAME', 'OPENFILENAMEA');

  // Types
  TSepiPointerType.Create(Result, 'POFNotifyA', 'TOFNotifyA', True);
  TSepiPointerType.Create(Result, 'POFNotifyW', 'TOFNotifyW', True);
  TSepiTypeAlias.Create(Result, 'POFNotify', 'POFNotifyA');
  SepiImport_OFNOTIFYA(Result);
  SepiImport_OFNOTIFYW(Result);
  TSepiTypeAlias.Create(Result, '_OFNOTIFY', '_OFNOTIFYA');
  TSepiTypeAlias.Create(Result, 'TOFNotifyA', '_OFNOTIFYA');
  TSepiTypeAlias.Create(Result, 'TOFNotifyW', '_OFNOTIFYW');
  TSepiTypeAlias.Create(Result, 'TOFNotify', 'TOFNotifyA');
  TSepiTypeAlias.Create(Result, 'OFNOTIFYA', '_OFNOTIFYA');
  TSepiTypeAlias.Create(Result, 'OFNOTIFYW', '_OFNOTIFYW');
  TSepiTypeAlias.Create(Result, 'OFNOTIFY', 'OFNOTIFYA');
  TSepiPointerType.Create(Result, 'POFNotifyExA',
    TypeInfo(TOFNotifyExA), True);
  TSepiPointerType.Create(Result, 'POFNotifyExW',
    TypeInfo(TOFNotifyExW), True);
  TSepiTypeAlias.Create(Result, 'POFNotifyEx', 'POFNotifyExA');
  SepiImport_OFNOTIFYEXA(Result);
  SepiImport_OFNOTIFYEXW(Result);
  TSepiTypeAlias.Create(Result, '_OFNOTIFYEX', TypeInfo(_OFNOTIFYEXA));
  TSepiTypeAlias.Create(Result, 'TOFNotifyExA', TypeInfo(_OFNOTIFYEXA));
  TSepiTypeAlias.Create(Result, 'TOFNotifyExW', TypeInfo(_OFNOTIFYEXW));
  TSepiTypeAlias.Create(Result, 'TOFNotifyEx', TypeInfo(TOFNotifyExA));
  TSepiTypeAlias.Create(Result, 'OFNOTIFYEXA', TypeInfo(_OFNOTIFYEXA));
  TSepiTypeAlias.Create(Result, 'OFNOTIFYEXW', TypeInfo(_OFNOTIFYEXW));
  TSepiTypeAlias.Create(Result, 'OFNOTIFYEX', TypeInfo(OFNOTIFYEXA));

  // Types
  TSepiPointerType.Create(Result, 'PChooseColorA', 'TChooseColorA', True);
  TSepiPointerType.Create(Result, 'PChooseColorW', 'TChooseColorW', True);
  TSepiTypeAlias.Create(Result, 'PChooseColor', 'PChooseColorA');
  TSepiPointerType.Create(Result, '$3', TypeInfo(COLORREF), True);
  TSepiMethodRefType.Create(Result, '$4',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagCHOOSECOLORA(Result);
  TSepiPointerType.Create(Result, '$5', TypeInfo(COLORREF), True);
  TSepiMethodRefType.Create(Result, '$6',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagCHOOSECOLORW(Result);
  TSepiTypeAlias.Create(Result, 'tagCHOOSECOLOR', 'tagCHOOSECOLORA');
  TSepiTypeAlias.Create(Result, 'TChooseColorA', 'tagCHOOSECOLORA');
  TSepiTypeAlias.Create(Result, 'TChooseColorW', 'tagCHOOSECOLORW');
  TSepiTypeAlias.Create(Result, 'TChooseColor', 'TChooseColorA');

  // Types
  TSepiPointerType.Create(Result, 'PFindReplaceA', 'TFindReplaceA', True);
  TSepiPointerType.Create(Result, 'PFindReplaceW', 'TFindReplaceW', True);
  TSepiTypeAlias.Create(Result, 'PFindReplace', 'PFindReplaceA');
  TSepiMethodRefType.Create(Result, '$7',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagFINDREPLACEA(Result);
  TSepiMethodRefType.Create(Result, '$8',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagFINDREPLACEW(Result);
  TSepiTypeAlias.Create(Result, 'tagFINDREPLACE', 'tagFINDREPLACEA');
  TSepiTypeAlias.Create(Result, 'TFindReplaceA', 'tagFINDREPLACEA');
  TSepiTypeAlias.Create(Result, 'TFindReplaceW', 'tagFINDREPLACEW');
  TSepiTypeAlias.Create(Result, 'TFindReplace', 'TFindReplaceA');
  TSepiTypeAlias.Create(Result, 'FINDREPLACEA', 'tagFINDREPLACEA');
  TSepiTypeAlias.Create(Result, 'FINDREPLACEW', 'tagFINDREPLACEW');
  TSepiTypeAlias.Create(Result, 'FINDREPLACE', 'FINDREPLACEA');

  // Types
  TSepiPointerType.Create(Result, 'PChooseFontA', 'TChooseFontA', True);
  TSepiPointerType.Create(Result, 'PChooseFontW', 'TChooseFontW', True);
  TSepiTypeAlias.Create(Result, 'PChooseFont', 'PChooseFontA');
  TSepiMethodRefType.Create(Result, '$9',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagCHOOSEFONTA(Result);
  TSepiMethodRefType.Create(Result, '$10',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagCHOOSEFONTW(Result);
  TSepiTypeAlias.Create(Result, 'tagCHOOSEFONT', 'tagCHOOSEFONTA');
  TSepiTypeAlias.Create(Result, 'TChooseFontA', 'tagCHOOSEFONTA');
  TSepiTypeAlias.Create(Result, 'TChooseFontW', 'tagCHOOSEFONTW');
  TSepiTypeAlias.Create(Result, 'TChooseFont', 'TChooseFontA');

  // Types
  TSepiPointerType.Create(Result, 'PPrintDlgA', 'TPrintDlgA', True);
  TSepiPointerType.Create(Result, 'PPrintDlgW', 'TPrintDlgW', True);
  TSepiTypeAlias.Create(Result, 'PPrintDlg', 'PPrintDlgA');
  TSepiMethodRefType.Create(Result, '$11',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  TSepiMethodRefType.Create(Result, '$12',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagPDA(Result);
  TSepiMethodRefType.Create(Result, '$13',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  TSepiMethodRefType.Create(Result, '$14',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagPDW(Result);
  TSepiTypeAlias.Create(Result, 'tagPD', 'tagPDA');
  TSepiTypeAlias.Create(Result, 'TPrintDlgA', 'tagPDA');
  TSepiTypeAlias.Create(Result, 'TPrintDlgW', 'tagPDW');
  TSepiTypeAlias.Create(Result, 'TPrintDlg', 'TPrintDlgA');

  // Types
  TSepiPointerType.Create(Result, 'PDevNames', 'TDevNames', True);
  SepiImporttagDEVNAMES(Result);
  TSepiTypeAlias.Create(Result, 'TDevNames', 'tagDEVNAMES');
  TSepiTypeAlias.Create(Result, 'DEVNAMES', 'tagDEVNAMES');

  // Types
  TSepiPointerType.Create(Result, 'PPageSetupDlgA', 'TPageSetupDlgA', True);
  TSepiPointerType.Create(Result, 'PPageSetupDlgW', 'TPageSetupDlgW', True);
  TSepiTypeAlias.Create(Result, 'PPageSetupDlg', 'PPageSetupDlgA');
  TSepiMethodRefType.Create(Result, '$15',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  TSepiMethodRefType.Create(Result, '$16',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagPSDA(Result);
  TSepiMethodRefType.Create(Result, '$17',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  TSepiMethodRefType.Create(Result, '$18',
    'function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT',
    False, ccStdCall);
  SepiImporttagPSDW(Result);
  TSepiTypeAlias.Create(Result, 'tagPSD', 'tagPSDA');
  TSepiTypeAlias.Create(Result, 'TPageSetupDlgA', 'tagPSDA');
  TSepiTypeAlias.Create(Result, 'TPageSetupDlgW', 'tagPSDW');
  TSepiTypeAlias.Create(Result, 'TPageSetupDlg', 'TPageSetupDlgA');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('CommDlgTypes', ImportUnit);
end.

