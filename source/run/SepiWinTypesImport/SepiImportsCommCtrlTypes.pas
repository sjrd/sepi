{*
  Importe l'unité CommCtrl dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsCommCtrlTypes;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, CommCtrl;

implementation

{ You must not localize any of the strings this unit contains! }

{------------------------}
{ _PROPSHEETPAGEA import }
{------------------------}

function SepiImport_PROPSHEETPAGEA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PROPSHEETPAGEA', False, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(Longint));
    AddField('dwFlags', System.TypeInfo(Longint));
    AddField('hInstance', System.TypeInfo(THandle));
    AddFieldAfter('pszTemplate', 'PAnsiChar', 'hInstance');
    AddFieldAfter('pResource', 'Pointer', 'hInstance');
    AddFieldAfter('hIcon', System.TypeInfo(THandle), 'pResource');
    AddFieldAfter('pszIcon', 'PAnsiChar', 'pResource');
    AddFieldAfter('pszTitle', 'PAnsiChar', 'pszIcon');
    AddFieldAfter('pfnDlgProc', 'Pointer', 'pszTitle');
    AddFieldAfter('lParam', System.TypeInfo(Longint), 'pfnDlgProc');
    AddFieldAfter('pfnCallback', 'TFNPSPCallbackA', 'lParam');
    AddFieldAfter('pcRefParent', 'PInteger', 'pfnCallback');
    AddFieldAfter('pszHeaderTitle', 'PAnsiChar', 'pcRefParent');
    AddFieldAfter('pszHeaderSubTitle', 'PAnsiChar', 'pszHeaderTitle');

    Complete;
  end;
end;

{------------------------}
{ _PROPSHEETPAGEW import }
{------------------------}

function SepiImport_PROPSHEETPAGEW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PROPSHEETPAGEW', False, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(Longint));
    AddField('dwFlags', System.TypeInfo(Longint));
    AddField('hInstance', System.TypeInfo(THandle));
    AddFieldAfter('pszTemplate', 'PWideChar', 'hInstance');
    AddFieldAfter('pResource', 'Pointer', 'hInstance');
    AddFieldAfter('hIcon', System.TypeInfo(THandle), 'pResource');
    AddFieldAfter('pszIcon', 'PWideChar', 'pResource');
    AddFieldAfter('pszTitle', 'PWideChar', 'pszIcon');
    AddFieldAfter('pfnDlgProc', 'Pointer', 'pszTitle');
    AddFieldAfter('lParam', System.TypeInfo(Longint), 'pfnDlgProc');
    AddFieldAfter('pfnCallback', 'TFNPSPCallbackW', 'lParam');
    AddFieldAfter('pcRefParent', 'PInteger', 'pfnCallback');
    AddFieldAfter('pszHeaderTitle', 'PWideChar', 'pcRefParent');
    AddFieldAfter('pszHeaderSubTitle', 'PWideChar', 'pszHeaderTitle');

    Complete;
  end;
end;

{--------------------------}
{ _PROPSHEETHEADERA import }
{--------------------------}

function SepiImport_PROPSHEETHEADERA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PROPSHEETHEADERA', False, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(Longint));
    AddField('dwFlags', System.TypeInfo(Longint));
    AddField('hwndParent', System.TypeInfo(HWnd));
    AddField('hInstance', System.TypeInfo(THandle));
    AddFieldAfter('hIcon', System.TypeInfo(THandle), 'hInstance');
    AddFieldAfter('pszIcon', 'PAnsiChar', 'hInstance');
    AddFieldAfter('pszCaption', 'PAnsiChar', 'pszIcon');
    AddFieldAfter('nPages', System.TypeInfo(Integer), 'pszCaption');
    AddFieldAfter('nStartPage', System.TypeInfo(Integer), 'nPages');
    AddFieldAfter('pStartPage', 'PAnsiChar', 'nPages');
    AddFieldAfter('ppsp', 'PPropSheetPageA', 'pStartPage');
    AddFieldAfter('phpage', 'Pointer', 'pStartPage');
    AddFieldAfter('pfnCallback', 'TFNPropSheetCallback', 'phpage');
    AddFieldAfter('hbmWatermark', System.TypeInfo(HBITMAP), 'pfnCallback');
    AddFieldAfter('pszbmWatermark', 'PAnsiChar', 'pfnCallback');
    AddFieldAfter('hplWatermark', System.TypeInfo(HPALETTE), 'pszbmWatermark');
    AddFieldAfter('hbmHeader', System.TypeInfo(HBITMAP), 'hplWatermark');
    AddFieldAfter('pszbmHeader', 'PAnsiChar', 'hplWatermark');

    Complete;
  end;
end;

{--------------------------}
{ _PROPSHEETHEADERW import }
{--------------------------}

function SepiImport_PROPSHEETHEADERW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PROPSHEETHEADERW', False, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(Longint));
    AddField('dwFlags', System.TypeInfo(Longint));
    AddField('hwndParent', System.TypeInfo(HWnd));
    AddField('hInstance', System.TypeInfo(THandle));
    AddFieldAfter('hIcon', System.TypeInfo(THandle), 'hInstance');
    AddFieldAfter('pszIcon', 'PWideChar', 'hInstance');
    AddFieldAfter('pszCaption', 'PWideChar', 'pszIcon');
    AddFieldAfter('nPages', System.TypeInfo(Integer), 'pszCaption');
    AddFieldAfter('nStartPage', System.TypeInfo(Integer), 'nPages');
    AddFieldAfter('pStartPage', 'PWideChar', 'nPages');
    AddFieldAfter('ppsp', 'PPropSheetPageW', 'pStartPage');
    AddFieldAfter('phpage', 'Pointer', 'pStartPage');
    AddFieldAfter('pfnCallback', 'TFNPropSheetCallback', 'phpage');
    AddFieldAfter('hbmWatermark', System.TypeInfo(HBITMAP), 'pfnCallback');
    AddFieldAfter('pszbmWatermark', 'PWideChar', 'pfnCallback');
    AddFieldAfter('hplWatermark', System.TypeInfo(HPALETTE), 'pszbmWatermark');
    AddFieldAfter('hbmHeader', System.TypeInfo(HBITMAP), 'hplWatermark');
    AddFieldAfter('pszbmHeader', 'PWideChar', 'hplWatermark');

    Complete;
  end;
end;

{--------------------------------}
{ tagINITCOMMONCONTROLSEX import }
{--------------------------------}

function SepiImporttagINITCOMMONCONTROLSEX(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagINITCOMMONCONTROLSEX',
    True, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(DWORD));
    AddField('dwICC', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-----------------------}
{ tagCOLORSCHEME import }
{-----------------------}

function SepiImporttagCOLORSCHEME(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCOLORSCHEME', True, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(DWORD));
    AddField('clrBtnHighlight', System.TypeInfo(COLORREF));
    AddField('clrBtnShadow', System.TypeInfo(COLORREF));

    Complete;
  end;
end;

{-------------------}
{ tagNMMOUSE import }
{-------------------}

function SepiImporttagNMMOUSE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMMOUSE', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('dwItemSpec', System.TypeInfo(DWORD));
    AddField('dwItemData', System.TypeInfo(DWORD));
    AddField('pt', 'TPoint');
    AddField('dwHitInfo', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{--------------------------}
{ tagNMOBJECTNOTIFY import }
{--------------------------}

function SepiImporttagNMOBJECTNOTIFY(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMOBJECTNOTIFY', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iItem', System.TypeInfo(Integer));
    AddField('piid', 'PGUID');
    AddField('pObject', 'Pointer');
    AddField('hResult', System.TypeInfo(HRESULT));
    AddField('dwFlags', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-----------------}
{ tagNMKEY import }
{-----------------}

function SepiImporttagNMKEY(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMKEY', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('nVKey', System.TypeInfo(UINT));
    AddField('uFlags', System.TypeInfo(UINT));

    Complete;
  end;
end;

{------------------}
{ tagNMCHAR import }
{------------------}

function SepiImporttagNMCHAR(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMCHAR', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('ch', System.TypeInfo(UINT));
    AddField('dwItemPrev', System.TypeInfo(DWORD));
    AddField('dwItemNext', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{----------------------------}
{ tagNMCUSTOMDRAWINFO import }
{----------------------------}

function SepiImporttagNMCUSTOMDRAWINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMCUSTOMDRAWINFO', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('dwDrawStage', System.TypeInfo(DWORD));
    AddField('hdc', System.TypeInfo(HDC));
    AddField('rc', 'TRect');
    AddField('dwItemSpec', System.TypeInfo(DWORD));
    AddField('uItemState', System.TypeInfo(UINT));
    AddField('lItemlParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{--------------------------}
{ tagNMTTCUSTOMDRAW import }
{--------------------------}

function SepiImporttagNMTTCUSTOMDRAW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTTCUSTOMDRAW', True, True);

  with Result do
  begin
    AddField('nmcd', 'TNMCustomDraw');
    AddField('uDrawFlags', System.TypeInfo(UINT));

    Complete;
  end;
end;

{-----------------------------}
{ _IMAGELISTDRAWPARAMS import }
{-----------------------------}

function SepiImport_IMAGELISTDRAWPARAMS(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_IMAGELISTDRAWPARAMS', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('himl', System.TypeInfo(HIMAGELIST));
    AddField('i', System.TypeInfo(Integer));
    AddField('hdcDst', System.TypeInfo(HDC));
    AddField('x', System.TypeInfo(Integer));
    AddField('y', System.TypeInfo(Integer));
    AddField('cx', System.TypeInfo(Integer));
    AddField('cy', System.TypeInfo(Integer));
    AddField('xBitmap', System.TypeInfo(Integer));
    AddField('yBitmap', System.TypeInfo(Integer));
    AddField('rgbBk', System.TypeInfo(COLORREF));
    AddField('rgbFg', System.TypeInfo(COLORREF));
    AddField('fStyle', System.TypeInfo(UINT));
    AddField('dwRop', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------------}
{ _IMAGEINFO import }
{-------------------}

function SepiImport_IMAGEINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_IMAGEINFO', True, True);

  with Result do
  begin
    AddField('hbmImage', System.TypeInfo(HBitmap));
    AddField('hbmMask', System.TypeInfo(HBitmap));
    AddField('Unused1', System.TypeInfo(Integer));
    AddField('Unused2', System.TypeInfo(Integer));
    AddField('rcImage', 'TRect');

    Complete;
  end;
end;

{------------------}
{ _HD_ITEMA import }
{------------------}

function SepiImport_HD_ITEMA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_HD_ITEMA', True, True);

  with Result do
  begin
    AddField('Mask', System.TypeInfo(Cardinal));
    AddField('cxy', System.TypeInfo(Integer));
    AddField('pszText', 'PAnsiChar');
    AddField('hbm', System.TypeInfo(HBITMAP));
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('fmt', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iOrder', System.TypeInfo(Integer));

    Complete;
  end;
end;

{------------------}
{ _HD_ITEMW import }
{------------------}

function SepiImport_HD_ITEMW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_HD_ITEMW', True, True);

  with Result do
  begin
    AddField('Mask', System.TypeInfo(Cardinal));
    AddField('cxy', System.TypeInfo(Integer));
    AddField('pszText', 'PWideChar');
    AddField('hbm', System.TypeInfo(HBITMAP));
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('fmt', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iOrder', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------}
{ _HD_LAYOUT import }
{-------------------}

function SepiImport_HD_LAYOUT(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_HD_LAYOUT', True, True);

  with Result do
  begin
    AddField('Rect', '$1');
    AddField('WindowPos', 'PWindowPos');

    Complete;
  end;
end;

{------------------------}
{ _HD_HITTESTINFO import }
{------------------------}

function SepiImport_HD_HITTESTINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_HD_HITTESTINFO', True, True);

  with Result do
  begin
    AddField('Point', 'TPoint');
    AddField('Flags', System.TypeInfo(Cardinal));
    AddField('Item', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------}
{ tagNMHEADERA import }
{---------------------}

function SepiImporttagNMHEADERA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMHEADERA', True, True);

  with Result do
  begin
    AddField('Hdr', 'TNMHdr');
    AddField('Item', System.TypeInfo(Integer));
    AddField('Button', System.TypeInfo(Integer));
    AddField('PItem', 'PHDItemA');

    Complete;
  end;
end;

{---------------------}
{ tagNMHEADERW import }
{---------------------}

function SepiImporttagNMHEADERW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMHEADERW', True, True);

  with Result do
  begin
    AddField('Hdr', 'TNMHdr');
    AddField('Item', System.TypeInfo(Integer));
    AddField('Button', System.TypeInfo(Integer));
    AddField('PItem', 'PHDItemW');

    Complete;
  end;
end;

{-------------------------}
{ tagNMHDDISPINFOA import }
{-------------------------}

function SepiImporttagNMHDDISPINFOA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMHDDISPINFOA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iItem', System.TypeInfo(Integer));
    AddField('mask', System.TypeInfo(UINT));
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{-------------------------}
{ tagNMHDDISPINFOW import }
{-------------------------}

function SepiImporttagNMHDDISPINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMHDDISPINFOW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iItem', System.TypeInfo(Integer));
    AddField('mask', System.TypeInfo(UINT));
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{------------------}
{ _TBBUTTON import }
{------------------}

function SepiImport_TBBUTTON(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_TBBUTTON', True, True);

  with Result do
  begin
    AddField('iBitmap', System.TypeInfo(Integer));
    AddField('idCommand', System.TypeInfo(Integer));
    AddField('fsState', System.TypeInfo(Byte));
    AddField('fsStyle', System.TypeInfo(Byte));
    AddField('bReserved', '$2');
    AddField('dwData', System.TypeInfo(Longint));
    AddField('iString', System.TypeInfo(Integer));

    Complete;
  end;
end;

{------------------}
{ _COLORMAP import }
{------------------}

function SepiImport_COLORMAP(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_COLORMAP', True, True);

  with Result do
  begin
    AddField('cFrom', System.TypeInfo(TColorRef));
    AddField('cTo', System.TypeInfo(TColorRef));

    Complete;
  end;
end;

{------------------------}
{ _NMTBCUSTOMDRAW import }
{------------------------}

function SepiImport_NMTBCUSTOMDRAW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_NMTBCUSTOMDRAW', True, True);

  with Result do
  begin
    AddField('nmcd', 'TNMCustomDraw');
    AddField('hbrMonoDither', System.TypeInfo(HBRUSH));
    AddField('hbrLines', System.TypeInfo(HBRUSH));
    AddField('hpenLines', System.TypeInfo(HPEN));
    AddField('clrText', System.TypeInfo(COLORREF));
    AddField('clrMark', System.TypeInfo(COLORREF));
    AddField('clrTextHighlight', System.TypeInfo(COLORREF));
    AddField('clrBtnFace', System.TypeInfo(COLORREF));
    AddField('clrBtnHighlight', System.TypeInfo(COLORREF));
    AddField('clrHighlightHotTrack', System.TypeInfo(COLORREF));
    AddField('rcText', 'TRect');
    AddField('nStringBkMode', System.TypeInfo(Integer));
    AddField('nHLStringBkMode', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-----------------------}
{ tagTBADDBITMAP import }
{-----------------------}

function SepiImporttagTBADDBITMAP(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTBADDBITMAP', True, True);

  with Result do
  begin
    AddField('hInst', System.TypeInfo(THandle));
    AddField('nID', System.TypeInfo(UINT));

    Complete;
  end;
end;

{-------------------------}
{ tagTBSAVEPARAMSA import }
{-------------------------}

function SepiImporttagTBSAVEPARAMSA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTBSAVEPARAMSA', True, True);

  with Result do
  begin
    AddField('hkr', System.TypeInfo(THandle));
    AddField('pszSubKey', 'PAnsiChar');
    AddField('pszValueName', 'PAnsiChar');

    Complete;
  end;
end;

{-------------------------}
{ tagTBSAVEPARAMSW import }
{-------------------------}

function SepiImporttagTBSAVEPARAMSW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTBSAVEPARAMSW', True, True);

  with Result do
  begin
    AddField('hkr', System.TypeInfo(THandle));
    AddField('pszSubKey', 'PWideChar');
    AddField('pszValueName', 'PWideChar');

    Complete;
  end;
end;

{---------------------}
{ TBINSERTMARK import }
{---------------------}

function SepiImportTBINSERTMARK(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TBINSERTMARK', True, True);

  with Result do
  begin
    AddField('iButton', System.TypeInfo(Integer));
    AddField('dwFlags', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{------------------------}
{ TBREPLACEBITMAP import }
{------------------------}

function SepiImportTBREPLACEBITMAP(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TBREPLACEBITMAP', True, True);

  with Result do
  begin
    AddField('hInstOld', System.TypeInfo(THandle));
    AddField('nIDOld', System.TypeInfo(Cardinal));
    AddField('hInstNew', System.TypeInfo(THandle));
    AddField('nIDNew', System.TypeInfo(Cardinal));
    AddField('nButtons', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------}
{ TBBUTTONINFOA import }
{----------------------}

function SepiImportTBBUTTONINFOA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TBBUTTONINFOA', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('idCommand', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('fsState', System.TypeInfo(Byte));
    AddField('fsStyle', System.TypeInfo(Byte));
    AddField('cx', System.TypeInfo(Word));
    AddField('lParam', System.TypeInfo(DWORD));
    AddField('pszText', 'PAnsiChar');
    AddField('cchText', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------}
{ TBBUTTONINFOW import }
{----------------------}

function SepiImportTBBUTTONINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TBBUTTONINFOW', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('idCommand', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('fsState', System.TypeInfo(Byte));
    AddField('fsStyle', System.TypeInfo(Byte));
    AddField('cx', System.TypeInfo(Word));
    AddField('lParam', System.TypeInfo(DWORD));
    AddField('pszText', 'PWideChar');
    AddField('cchText', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-----------------------}
{ tagNMTBHOTITEM import }
{-----------------------}

function SepiImporttagNMTBHOTITEM(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTBHOTITEM', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('idOld', System.TypeInfo(Integer));
    AddField('idNew', System.TypeInfo(Integer));
    AddField('dwFlags', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{---------------------------}
{ tagNMTBGETINFOTIPA import }
{---------------------------}

function SepiImporttagNMTBGETINFOTIPA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTBGETINFOTIPA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iItem', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{---------------------------}
{ tagNMTBGETINFOTIPW import }
{---------------------------}

function SepiImporttagNMTBGETINFOTIPW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTBGETINFOTIPW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iItem', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{----------------------}
{ NMTBDISPINFOA import }
{----------------------}

function SepiImportNMTBDISPINFOA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMTBDISPINFOA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('idCommand', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(DWORD));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('pszText', 'PAnsiChar');
    AddField('cchText', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------}
{ NMTBDISPINFOW import }
{----------------------}

function SepiImportNMTBDISPINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMTBDISPINFOW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('idCommand', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(DWORD));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('pszText', 'PWideChar');
    AddField('cchText', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------}
{ tagNMTOOLBARA import }
{----------------------}

function SepiImporttagNMTOOLBARA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTOOLBARA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iItem', System.TypeInfo(Integer));
    AddField('tbButton', 'TTBButton');
    AddField('cchText', System.TypeInfo(Integer));
    AddField('pszText', 'PAnsiChar');

    Complete;
  end;
end;

{----------------------}
{ tagNMTOOLBARW import }
{----------------------}

function SepiImporttagNMTOOLBARW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTOOLBARW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iItem', System.TypeInfo(Integer));
    AddField('tbButton', 'TTBButton');
    AddField('cchText', System.TypeInfo(Integer));
    AddField('pszText', 'PWideChar');

    Complete;
  end;
end;

{---------------------}
{ tagREBARINFO import }
{---------------------}

function SepiImporttagREBARINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagREBARINFO', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('fMask', System.TypeInfo(UINT));
    AddField('himl', System.TypeInfo(HIMAGELIST));

    Complete;
  end;
end;

{--------------------------}
{ tagREBARBANDINFOA import }
{--------------------------}

function SepiImporttagREBARBANDINFOA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagREBARBANDINFOA', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('fMask', System.TypeInfo(UINT));
    AddField('fStyle', System.TypeInfo(UINT));
    AddField('clrFore', System.TypeInfo(TColorRef));
    AddField('clrBack', System.TypeInfo(TColorRef));
    AddField('lpText', 'PAnsiChar');
    AddField('cch', System.TypeInfo(UINT));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('hwndChild', System.TypeInfo(HWnd));
    AddField('cxMinChild', System.TypeInfo(UINT));
    AddField('cyMinChild', System.TypeInfo(UINT));
    AddField('cx', System.TypeInfo(UINT));
    AddField('hbmBack', System.TypeInfo(HBitmap));
    AddField('wID', System.TypeInfo(UINT));
    AddField('cyChild', System.TypeInfo(UINT));
    AddField('cyMaxChild', System.TypeInfo(UINT));
    AddField('cyIntegral', System.TypeInfo(UINT));
    AddField('cxIdeal', System.TypeInfo(UINT));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('cxHeader', System.TypeInfo(UINT));

    Complete;
  end;
end;

{--------------------------}
{ tagREBARBANDINFOW import }
{--------------------------}

function SepiImporttagREBARBANDINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagREBARBANDINFOW', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('fMask', System.TypeInfo(UINT));
    AddField('fStyle', System.TypeInfo(UINT));
    AddField('clrFore', System.TypeInfo(TColorRef));
    AddField('clrBack', System.TypeInfo(TColorRef));
    AddField('lpText', 'PWideChar');
    AddField('cch', System.TypeInfo(UINT));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('hwndChild', System.TypeInfo(HWnd));
    AddField('cxMinChild', System.TypeInfo(UINT));
    AddField('cyMinChild', System.TypeInfo(UINT));
    AddField('cx', System.TypeInfo(UINT));
    AddField('hbmBack', System.TypeInfo(HBitmap));
    AddField('wID', System.TypeInfo(UINT));
    AddField('cyChild', System.TypeInfo(UINT));
    AddField('cyMaxChild', System.TypeInfo(UINT));
    AddField('cyIntegral', System.TypeInfo(UINT));
    AddField('cxIdeal', System.TypeInfo(UINT));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('cxHeader', System.TypeInfo(UINT));

    Complete;
  end;
end;

{----------------------------}
{ tagNMREBARCHILDSIZE import }
{----------------------------}

function SepiImporttagNMREBARCHILDSIZE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMREBARCHILDSIZE', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('uBand', System.TypeInfo(UINT));
    AddField('wID', System.TypeInfo(UINT));
    AddField('rcChild', 'TRect');
    AddField('rcBand', 'TRect');

    Complete;
  end;
end;

{-------------------}
{ tagNMREBAR import }
{-------------------}

function SepiImporttagNMREBAR(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMREBAR', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('uBand', System.TypeInfo(UINT));
    AddField('fStyle', System.TypeInfo(UINT));
    AddField('wID', System.TypeInfo(UINT));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{------------------------}
{ tagNMRBAUTOSIZE import }
{------------------------}

function SepiImporttagNMRBAUTOSIZE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMRBAUTOSIZE', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('fChanged', System.TypeInfo(BOOL));
    AddField('rcTarget', 'TRect');
    AddField('rcActual', 'TRect');

    Complete;
  end;
end;

{------------------------}
{ _RB_HITTESTINFO import }
{------------------------}

function SepiImport_RB_HITTESTINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_RB_HITTESTINFO', True, True);

  with Result do
  begin
    AddField('pt', 'TPoint');
    AddField('flags', System.TypeInfo(UINT));
    AddField('iBand', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------}
{ tagTOOLINFOA import }
{---------------------}

function SepiImporttagTOOLINFOA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTOOLINFOA', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('uFlags', System.TypeInfo(UINT));
    AddField('hwnd', System.TypeInfo(HWND));
    AddField('uId', System.TypeInfo(UINT));
    AddField('Rect', 'TRect');
    AddField('hInst', System.TypeInfo(THandle));
    AddField('lpszText', 'PAnsiChar');
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{---------------------}
{ tagTOOLINFOW import }
{---------------------}

function SepiImporttagTOOLINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTOOLINFOW', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('uFlags', System.TypeInfo(UINT));
    AddField('hwnd', System.TypeInfo(HWND));
    AddField('uId', System.TypeInfo(UINT));
    AddField('Rect', 'TRect');
    AddField('hInst', System.TypeInfo(THandle));
    AddField('lpszText', 'PWideChar');
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{-------------------------}
{ _TT_HITTESTINFOA import }
{-------------------------}

function SepiImport_TT_HITTESTINFOA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_TT_HITTESTINFOA', True, True);

  with Result do
  begin
    AddField('hwnd', System.TypeInfo(HWND));
    AddField('pt', 'TPoint');
    AddField('ti', 'TToolInfoA');

    Complete;
  end;
end;

{-------------------------}
{ _TT_HITTESTINFOW import }
{-------------------------}

function SepiImport_TT_HITTESTINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_TT_HITTESTINFOW', True, True);

  with Result do
  begin
    AddField('hwnd', System.TypeInfo(HWND));
    AddField('pt', 'TPoint');
    AddField('ti', 'TToolInfoW');

    Complete;
  end;
end;

{-------------------------}
{ tagNMTTDISPINFOA import }
{-------------------------}

function SepiImporttagNMTTDISPINFOA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTTDISPINFOA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('lpszText', 'PAnsiChar');
    AddField('szText', '$3');
    AddField('hinst', System.TypeInfo(HINST));
    AddField('uFlags', System.TypeInfo(UINT));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{-------------------------}
{ tagNMTTDISPINFOW import }
{-------------------------}

function SepiImporttagNMTTDISPINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTTDISPINFOW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('lpszText', 'PWideChar');
    AddField('szText', '$4');
    AddField('hinst', System.TypeInfo(HINST));
    AddField('uFlags', System.TypeInfo(UINT));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{------------------------}
{ tagDRAGLISTINFO import }
{------------------------}

function SepiImporttagDRAGLISTINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagDRAGLISTINFO', True, True);

  with Result do
  begin
    AddField('uNotification', System.TypeInfo(UINT));
    AddField('hWnd', System.TypeInfo(HWND));
    AddField('ptCursor', 'TPoint');

    Complete;
  end;
end;

{-----------------}
{ _UDACCEL import }
{-----------------}

function SepiImport_UDACCEL(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_UDACCEL', True, True);

  with Result do
  begin
    AddField('nSec', System.TypeInfo(UINT));
    AddField('nInc', System.TypeInfo(UINT));

    Complete;
  end;
end;

{-------------------}
{ _NM_UPDOWN import }
{-------------------}

function SepiImport_NM_UPDOWN(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_NM_UPDOWN', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('iPos', System.TypeInfo(Integer));
    AddField('iDelta', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------}
{ PBRANGE import }
{----------------}

function SepiImportPBRANGE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'PBRANGE', False, True);

  with Result do
  begin
    AddField('iLow', System.TypeInfo(Integer));
    AddField('iHigh', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------}
{ tagLVITEMA import }
{-------------------}

function SepiImporttagLVITEMA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVITEMA', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('iItem', System.TypeInfo(Integer));
    AddField('iSubItem', System.TypeInfo(Integer));
    AddField('state', System.TypeInfo(UINT));
    AddField('stateMask', System.TypeInfo(UINT));
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('iIndent', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------}
{ tagLVITEMW import }
{-------------------}

function SepiImporttagLVITEMW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVITEMW', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('iItem', System.TypeInfo(Integer));
    AddField('iSubItem', System.TypeInfo(Integer));
    AddField('state', System.TypeInfo(UINT));
    AddField('stateMask', System.TypeInfo(UINT));
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('iIndent', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-----------------------}
{ tagLVFINDINFOA import }
{-----------------------}

function SepiImporttagLVFINDINFOA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVFINDINFOA', True, True);

  with Result do
  begin
    AddField('flags', System.TypeInfo(UINT));
    AddField('psz', 'PAnsiChar');
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('pt', 'TPoint');
    AddField('vkDirection', System.TypeInfo(UINT));

    Complete;
  end;
end;

{-----------------------}
{ tagLVFINDINFOW import }
{-----------------------}

function SepiImporttagLVFINDINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVFINDINFOW', True, True);

  with Result do
  begin
    AddField('flags', System.TypeInfo(UINT));
    AddField('psz', 'PWideChar');
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('pt', 'TPoint');
    AddField('vkDirection', System.TypeInfo(UINT));

    Complete;
  end;
end;

{-------------------------}
{ tagLVHITTESTINFO import }
{-------------------------}

function SepiImporttagLVHITTESTINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVHITTESTINFO', True, True);

  with Result do
  begin
    AddField('pt', 'TPoint');
    AddField('flags', System.TypeInfo(UINT));
    AddField('iItem', System.TypeInfo(Integer));
    AddField('iSubItem', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------}
{ tagLVCOLUMNA import }
{---------------------}

function SepiImporttagLVCOLUMNA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVCOLUMNA', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('fmt', System.TypeInfo(Integer));
    AddField('cx', System.TypeInfo(Integer));
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iSubItem', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iOrder', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------}
{ tagLVCOLUMNW import }
{---------------------}

function SepiImporttagLVCOLUMNW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVCOLUMNW', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('fmt', System.TypeInfo(Integer));
    AddField('cx', System.TypeInfo(Integer));
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iSubItem', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iOrder', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------}
{ tagLVBKIMAGEA import }
{----------------------}

function SepiImporttagLVBKIMAGEA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVBKIMAGEA', True, True);

  with Result do
  begin
    AddField('ulFlags', System.TypeInfo(ULONG));
    AddField('hbm', System.TypeInfo(HBITMAP));
    AddField('pszImage', 'PAnsiChar');
    AddField('cchImageMax', System.TypeInfo(UINT));
    AddField('xOffsetPercent', System.TypeInfo(Integer));
    AddField('yOffsetPercent', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------}
{ tagLVBKIMAGEW import }
{----------------------}

function SepiImporttagLVBKIMAGEW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVBKIMAGEW', True, True);

  with Result do
  begin
    AddField('ulFlags', System.TypeInfo(ULONG));
    AddField('hbm', System.TypeInfo(HBITMAP));
    AddField('pszImage', 'PWideChar');
    AddField('cchImageMax', System.TypeInfo(UINT));
    AddField('xOffsetPercent', System.TypeInfo(Integer));
    AddField('yOffsetPercent', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------}
{ tagNMLISTVIEW import }
{----------------------}

function SepiImporttagNMLISTVIEW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMLISTVIEW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('iItem', System.TypeInfo(Integer));
    AddField('iSubItem', System.TypeInfo(Integer));
    AddField('uNewState', System.TypeInfo(UINT));
    AddField('uOldState', System.TypeInfo(UINT));
    AddField('uChanged', System.TypeInfo(UINT));
    AddField('ptAction', 'TPoint');
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{--------------------------}
{ tagNMITEMACTIVATE import }
{--------------------------}

function SepiImporttagNMITEMACTIVATE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMITEMACTIVATE', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iItem', System.TypeInfo(Integer));
    AddField('iSubItem', System.TypeInfo(Integer));
    AddField('uNewState', System.TypeInfo(UINT));
    AddField('uOldState', System.TypeInfo(UINT));
    AddField('uChanged', System.TypeInfo(UINT));
    AddField('ptAction', 'TPoint');
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('uKeyFlags', System.TypeInfo(UINT));

    Complete;
  end;
end;

{--------------------------}
{ tagNMLVCUSTOMDRAW import }
{--------------------------}

function SepiImporttagNMLVCUSTOMDRAW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMLVCUSTOMDRAW', True, True);

  with Result do
  begin
    AddField('nmcd', 'TNMCustomDraw');
    AddField('clrText', System.TypeInfo(COLORREF));
    AddField('clrTextBk', System.TypeInfo(COLORREF));
    AddField('iSubItem', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------------}
{ tagNMLVCACHEHINT import }
{-------------------------}

function SepiImporttagNMLVCACHEHINT(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMLVCACHEHINT', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('iFrom', System.TypeInfo(Integer));
    AddField('iTo', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------------}
{ tagNMLVFINDITEMA import }
{-------------------------}

function SepiImporttagNMLVFINDITEMA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMLVFINDITEMA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iStart', System.TypeInfo(Integer));
    AddField('lvfi', 'TLVFindInfoA');

    Complete;
  end;
end;

{-------------------------}
{ tagNMLVFINDITEMW import }
{-------------------------}

function SepiImporttagNMLVFINDITEMW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMLVFINDITEMW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iStart', System.TypeInfo(Integer));
    AddField('lvfi', 'TLVFindInfoW');

    Complete;
  end;
end;

{-----------------------------}
{ tagNMLVODSTATECHANGE import }
{-----------------------------}

function SepiImporttagNMLVODSTATECHANGE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMLVODSTATECHANGE', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iFrom', System.TypeInfo(Integer));
    AddField('iTo', System.TypeInfo(Integer));
    AddField('uNewState', System.TypeInfo(UINT));
    AddField('uOldState', System.TypeInfo(UINT));

    Complete;
  end;
end;

{----------------------}
{ tagLVDISPINFO import }
{----------------------}

function SepiImporttagLVDISPINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVDISPINFO', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('item', 'TLVItemA');

    Complete;
  end;
end;

{-----------------------}
{ tagLVDISPINFOW import }
{-----------------------}

function SepiImporttagLVDISPINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVDISPINFOW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('item', 'TLVItemW');

    Complete;
  end;
end;

{---------------------}
{ tagLVKEYDOWN import }
{---------------------}

function SepiImporttagLVKEYDOWN(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagLVKEYDOWN', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('wVKey', System.TypeInfo(Word));
    AddField('flags', System.TypeInfo(UINT));

    Complete;
  end;
end;

{---------------------------}
{ tagNMLVGETINFOTIPA import }
{---------------------------}

function SepiImporttagNMLVGETINFOTIPA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMLVGETINFOTIPA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('dwFlags', System.TypeInfo(DWORD));
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iItem', System.TypeInfo(Integer));
    AddField('iSubItem', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{---------------------------}
{ tagNMLVGETINFOTIPW import }
{---------------------------}

function SepiImporttagNMLVGETINFOTIPW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMLVGETINFOTIPW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('dwFlags', System.TypeInfo(DWORD));
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iItem', System.TypeInfo(Integer));
    AddField('iSubItem', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{------------------}
{ _TREEITEM import }
{------------------}

function SepiImport_TREEITEM(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_TREEITEM', True, True);

  with Result do
  begin

    Complete;
  end;
end;

{-------------------}
{ tagTVITEMA import }
{-------------------}

function SepiImporttagTVITEMA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVITEMA', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('hItem', 'HTreeItem');
    AddField('state', System.TypeInfo(UINT));
    AddField('stateMask', System.TypeInfo(UINT));
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iSelectedImage', System.TypeInfo(Integer));
    AddField('cChildren', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{-------------------}
{ tagTVITEMW import }
{-------------------}

function SepiImporttagTVITEMW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVITEMW', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('hItem', 'HTreeItem');
    AddField('state', System.TypeInfo(UINT));
    AddField('stateMask', System.TypeInfo(UINT));
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iSelectedImage', System.TypeInfo(Integer));
    AddField('cChildren', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{---------------------}
{ tagTVITEMEXA import }
{---------------------}

function SepiImporttagTVITEMEXA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVITEMEXA', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('hItem', 'HTREEITEM');
    AddField('state', System.TypeInfo(UINT));
    AddField('stateMask', System.TypeInfo(UINT));
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iSelectedImage', System.TypeInfo(Integer));
    AddField('cChildren', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('iIntegral', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------}
{ tagTVITEMEXW import }
{---------------------}

function SepiImporttagTVITEMEXW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVITEMEXW', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('hItem', 'HTREEITEM');
    AddField('state', System.TypeInfo(UINT));
    AddField('stateMask', System.TypeInfo(UINT));
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iSelectedImage', System.TypeInfo(Integer));
    AddField('cChildren', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('iIntegral', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------------}
{ tagTVINSERTSTRUCTA import }
{---------------------------}

function SepiImporttagTVINSERTSTRUCTA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVINSERTSTRUCTA', True, True);

  with Result do
  begin
    AddField('hParent', 'HTreeItem');
    AddField('hInsertAfter', 'HTreeItem');
    AddFieldAfter('itemex', 'TTVItemExA', 'hInsertAfter');
    AddFieldAfter('item', 'TTVItemA', 'hInsertAfter');

    Complete;
  end;
end;

{---------------------------}
{ tagTVINSERTSTRUCTW import }
{---------------------------}

function SepiImporttagTVINSERTSTRUCTW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVINSERTSTRUCTW', True, True);

  with Result do
  begin
    AddField('hParent', 'HTreeItem');
    AddField('hInsertAfter', 'HTreeItem');
    AddFieldAfter('itemex', 'TTVItemExW', 'hInsertAfter');
    AddFieldAfter('item', 'TTVItemW', 'hInsertAfter');

    Complete;
  end;
end;

{-------------------------}
{ tagTVHITTESTINFO import }
{-------------------------}

function SepiImporttagTVHITTESTINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVHITTESTINFO', True, True);

  with Result do
  begin
    AddField('pt', 'TPoint');
    AddField('flags', System.TypeInfo(UINT));
    AddField('hItem', 'HTreeItem');

    Complete;
  end;
end;

{--------------------}
{ tagTVSORTCB import }
{--------------------}

function SepiImporttagTVSORTCB(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVSORTCB', True, True);

  with Result do
  begin
    AddField('hParent', 'HTreeItem');
    AddField('lpfnCompare', 'TTVCompare');
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{-----------------------}
{ tagNMTREEVIEWA import }
{-----------------------}

function SepiImporttagNMTREEVIEWA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTREEVIEWA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('action', System.TypeInfo(UINT));
    AddField('itemOld', 'TTVItemA');
    AddField('itemNew', 'TTVItemA');
    AddField('ptDrag', 'TPoint');

    Complete;
  end;
end;

{-----------------------}
{ tagNMTREEVIEWW import }
{-----------------------}

function SepiImporttagNMTREEVIEWW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTREEVIEWW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('action', System.TypeInfo(UINT));
    AddField('itemOld', 'TTVItemW');
    AddField('itemNew', 'TTVItemW');
    AddField('ptDrag', 'TPoint');

    Complete;
  end;
end;

{-----------------------}
{ tagTVDISPINFOA import }
{-----------------------}

function SepiImporttagTVDISPINFOA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVDISPINFOA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('item', 'TTVItemA');

    Complete;
  end;
end;

{-----------------------}
{ tagTVDISPINFOW import }
{-----------------------}

function SepiImporttagTVDISPINFOW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVDISPINFOW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('item', 'TTVItemW');

    Complete;
  end;
end;

{---------------------}
{ tagTVKEYDOWN import }
{---------------------}

function SepiImporttagTVKEYDOWN(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTVKEYDOWN', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('wVKey', System.TypeInfo(Word));
    AddField('flags', System.TypeInfo(UINT));

    Complete;
  end;
end;

{--------------------------}
{ tagNMTVCUSTOMDRAW import }
{--------------------------}

function SepiImporttagNMTVCUSTOMDRAW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTVCUSTOMDRAW', True, True);

  with Result do
  begin
    AddField('nmcd', 'TNMCustomDraw');
    AddField('clrText', System.TypeInfo(COLORREF));
    AddField('clrTextBk', System.TypeInfo(COLORREF));
    AddField('iLevel', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------------}
{ tagNMTVGETINFOTIPA import }
{---------------------------}

function SepiImporttagNMTVGETINFOTIPA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTVGETINFOTIPA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('hItem', 'HTREEITEM');
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{---------------------------}
{ tagNMTVGETINFOTIPW import }
{---------------------------}

function SepiImporttagNMTVGETINFOTIPW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMTVGETINFOTIPW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('hItem', 'HTREEITEM');
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{---------------------------}
{ tagCOMBOBOXEXITEMA import }
{---------------------------}

function SepiImporttagCOMBOBOXEXITEMA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCOMBOBOXEXITEMA', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('iItem', System.TypeInfo(Integer));
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iSelectedImage', System.TypeInfo(Integer));
    AddField('iOverlay', System.TypeInfo(Integer));
    AddField('iIndent', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{---------------------------}
{ tagCOMBOBOXEXITEMW import }
{---------------------------}

function SepiImporttagCOMBOBOXEXITEMW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCOMBOBOXEXITEMW', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('iItem', System.TypeInfo(Integer));
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('iSelectedImage', System.TypeInfo(Integer));
    AddField('iOverlay', System.TypeInfo(Integer));
    AddField('iIndent', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{----------------------}
{ NMCOMBOBOXEXA import }
{----------------------}

function SepiImportNMCOMBOBOXEXA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMCOMBOBOXEXA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('ceItem', 'TComboBoxExItemA');

    Complete;
  end;
end;

{----------------------}
{ NMCOMBOBOXEXW import }
{----------------------}

function SepiImportNMCOMBOBOXEXW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMCOMBOBOXEXW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('ceItem', 'TComboBoxExItemW');

    Complete;
  end;
end;

{------------------------}
{ NMCBEDRAGBEGINA import }
{------------------------}

function SepiImportNMCBEDRAGBEGINA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMCBEDRAGBEGINA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iItemid', System.TypeInfo(Integer));
    AddField('szText', '$5');

    Complete;
  end;
end;

{------------------------}
{ NMCBEDRAGBEGINW import }
{------------------------}

function SepiImportNMCBEDRAGBEGINW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMCBEDRAGBEGINW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('iItemid', System.TypeInfo(Integer));
    AddField('szText', '$6');

    Complete;
  end;
end;

{----------------------}
{ NMCBEENDEDITA import }
{----------------------}

function SepiImportNMCBEENDEDITA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMCBEENDEDITA', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('fChanged', System.TypeInfo(BOOL));
    AddField('iNewSelection', System.TypeInfo(Integer));
    AddField('szText', '$7');
    AddField('iWhy', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------}
{ NMCBEENDEDITW import }
{----------------------}

function SepiImportNMCBEENDEDITW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMCBEENDEDITW', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHdr');
    AddField('fChanged', System.TypeInfo(BOOL));
    AddField('iNewSelection', System.TypeInfo(Integer));
    AddField('szText', '$8');
    AddField('iWhy', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------------}
{ tagTCITEMHEADERA import }
{-------------------------}

function SepiImporttagTCITEMHEADERA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTCITEMHEADERA', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('lpReserved1', System.TypeInfo(UINT));
    AddField('lpReserved2', System.TypeInfo(UINT));
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------------}
{ tagTCITEMHEADERW import }
{-------------------------}

function SepiImporttagTCITEMHEADERW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTCITEMHEADERW', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('lpReserved1', System.TypeInfo(UINT));
    AddField('lpReserved2', System.TypeInfo(UINT));
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------}
{ tagTCITEMA import }
{-------------------}

function SepiImporttagTCITEMA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTCITEMA', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('dwState', System.TypeInfo(UINT));
    AddField('dwStateMask', System.TypeInfo(UINT));
    AddField('pszText', 'PAnsiChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{-------------------}
{ tagTCITEMW import }
{-------------------}

function SepiImporttagTCITEMW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTCITEMW', True, True);

  with Result do
  begin
    AddField('mask', System.TypeInfo(UINT));
    AddField('dwState', System.TypeInfo(UINT));
    AddField('dwStateMask', System.TypeInfo(UINT));
    AddField('pszText', 'PWideChar');
    AddField('cchTextMax', System.TypeInfo(Integer));
    AddField('iImage', System.TypeInfo(Integer));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{-------------------------}
{ tagTCHITTESTINFO import }
{-------------------------}

function SepiImporttagTCHITTESTINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTCHITTESTINFO', True, True);

  with Result do
  begin
    AddField('pt', 'TPoint');
    AddField('flags', System.TypeInfo(UINT));

    Complete;
  end;
end;

{---------------------}
{ tagTCKEYDOWN import }
{---------------------}

function SepiImporttagTCKEYDOWN(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTCKEYDOWN', True, True);

  with Result do
  begin
    AddField('hdr', 'TNMHDR');
    AddField('wVKey', System.TypeInfo(Word));
    AddField('flags', System.TypeInfo(UINT));

    Complete;
  end;
end;

{----------------------}
{ MCHITTESTINFO import }
{----------------------}

function SepiImportMCHITTESTINFO(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'MCHITTESTINFO', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('pt', 'TPoint');
    AddField('uHit', System.TypeInfo(UINT));
    AddField('st', 'TSystemTime');

    Complete;
  end;
end;

{-----------------------}
{ tagNMSELCHANGE import }
{-----------------------}

function SepiImporttagNMSELCHANGE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMSELCHANGE', True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('stSelStart', 'TSystemTime');
    AddField('stSelEnd', 'TSystemTime');

    Complete;
  end;
end;

{----------------------}
{ tagNMDAYSTATE import }
{----------------------}

function SepiImporttagNMDAYSTATE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDAYSTATE', True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('stStart', 'TSystemTime');
    AddField('cDayState', System.TypeInfo(Integer));
    AddField('prgDayState', 'PMonthDayState');

    Complete;
  end;
end;

{----------------------------}
{ tagNMDATETIMECHANGE import }
{----------------------------}

function SepiImporttagNMDATETIMECHANGE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDATETIMECHANGE', True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('dwFlags', System.TypeInfo(DWORD));
    AddField('st', 'TSystemTime');

    Complete;
  end;
end;

{-----------------------------}
{ tagNMDATETIMESTRINGA import }
{-----------------------------}

function SepiImporttagNMDATETIMESTRINGA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDATETIMESTRINGA', True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('pszUserString', 'PAnsiChar');
    AddField('st', 'TSystemTime');
    AddField('dwFlags', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-----------------------------}
{ tagNMDATETIMESTRINGW import }
{-----------------------------}

function SepiImporttagNMDATETIMESTRINGW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDATETIMESTRINGW', True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('pszUserString', 'PWideChar');
    AddField('st', 'TSystemTime');
    AddField('dwFlags', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{--------------------------------}
{ tagNMDATETIMEWMKEYDOWNA import }
{--------------------------------}

function SepiImporttagNMDATETIMEWMKEYDOWNA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDATETIMEWMKEYDOWNA',
    True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('nVirtKey', System.TypeInfo(Integer));
    AddField('pszFormat', 'PAnsiChar');
    AddField('st', 'TSystemTime');

    Complete;
  end;
end;

{--------------------------------}
{ tagNMDATETIMEWMKEYDOWNW import }
{--------------------------------}

function SepiImporttagNMDATETIMEWMKEYDOWNW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDATETIMEWMKEYDOWNW',
    True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('nVirtKey', System.TypeInfo(Integer));
    AddField('pszFormat', 'PWideChar');
    AddField('st', 'TSystemTime');

    Complete;
  end;
end;

{-----------------------------}
{ tagNMDATETIMEFORMATA import }
{-----------------------------}

function SepiImporttagNMDATETIMEFORMATA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDATETIMEFORMATA', True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('pszFormat', 'PAnsiChar');
    AddField('st', 'TSystemTime');
    AddField('pszDisplay', 'PAnsiChar');
    AddField('szDisplay', '$9');

    Complete;
  end;
end;

{-----------------------------}
{ tagNMDATETIMEFORMATW import }
{-----------------------------}

function SepiImporttagNMDATETIMEFORMATW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDATETIMEFORMATW', True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('pszFormat', 'PWideChar');
    AddField('st', 'TSystemTime');
    AddField('pszDisplay', 'PWideChar');
    AddField('szDisplay', '$10');

    Complete;
  end;
end;

{----------------------------------}
{ tagNMDATETIMEFORMATQUERYA import }
{----------------------------------}

function SepiImporttagNMDATETIMEFORMATQUERYA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDATETIMEFORMATQUERYA',
    True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('pszFormat', 'PAnsiChar');
    AddField('szMax', 'TSize');

    Complete;
  end;
end;

{----------------------------------}
{ tagNMDATETIMEFORMATQUERYW import }
{----------------------------------}

function SepiImporttagNMDATETIMEFORMATQUERYW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMDATETIMEFORMATQUERYW',
    True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNmHdr');
    AddField('pszFormat', 'PWideChar');
    AddField('szMax', 'TSize');

    Complete;
  end;
end;

{-----------------------}
{ tagNMIPADDRESS import }
{-----------------------}

function SepiImporttagNMIPADDRESS(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagNMIPADDRESS', True, True);

  with Result do
  begin
    AddField('hdr', 'NMHDR');
    AddField('iField', System.TypeInfo(Integer));
    AddField('iValue', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------}
{ NMPGSCROLL import }
{-------------------}

function SepiImportNMPGSCROLL(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMPGSCROLL', True, True);

  with Result do
  begin
    AddField('hdr', 'NMHDR');
    AddField('fwKeys', System.TypeInfo(Word));
    AddField('rcParent', 'TRect');
    AddField('iDir', System.TypeInfo(Integer));
    AddField('iXpos', System.TypeInfo(Integer));
    AddField('iYpos', System.TypeInfo(Integer));
    AddField('iScroll', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------}
{ NMPGCALCSIZE import }
{---------------------}

function SepiImportNMPGCALCSIZE(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'NMPGCALCSIZE', True, True);

  with Result do
  begin
    AddField('hdr', 'NMHDR');
    AddField('dwFlag', System.TypeInfo(DWORD));
    AddField('iWidth', System.TypeInfo(Integer));
    AddField('iHeight', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------------}
{ tagTRACKMOUSEEVENT import }
{---------------------------}

function SepiImporttagTRACKMOUSEEVENT(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagTRACKMOUSEEVENT', True, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('dwFlags', System.TypeInfo(DWORD));
    AddField('hwndTrack', System.TypeInfo(HWND));
    AddField('dwHoverTime', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'CommCtrlTypes',
    ['Messages', 'Windows', 'ActiveXTypes']);

  // Types
  TSepiTypeAlias.Create(Result, 'HPropSheetPage', 'Pointer');
  TSepiPointerType.Create(Result, 'PPropSheetPageA', 'TPropSheetPageA', True);
  TSepiPointerType.Create(Result, 'PPropSheetPageW', 'TPropSheetPageW', True);
  TSepiTypeAlias.Create(Result, 'PPropSheetPage', 'PPropSheetPageA');
  TSepiMethodRefType.Create(Result, 'LPFNPSPCALLBACKA',
    'function(Wnd: HWnd; Msg: Integer; PPSP: PPropSheetPageA ) : Integer',
    False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'LPFNPSPCALLBACKW',
    'function(Wnd: HWnd; Msg: Integer; PPSP: PPropSheetPageW ) : Integer',
    False, ccStdCall);
  TSepiTypeAlias.Create(Result, 'LPFNPSPCALLBACK', 'LPFNPSPCALLBACKA');
  TSepiTypeAlias.Create(Result, 'TFNPSPCallbackA', 'LPFNPSPCALLBACKA');
  TSepiTypeAlias.Create(Result, 'TFNPSPCallbackW', 'LPFNPSPCALLBACKW');
  TSepiTypeAlias.Create(Result, 'TFNPSPCallback', 'TFNPSPCallbackA');
  SepiImport_PROPSHEETPAGEA(Result);
  SepiImport_PROPSHEETPAGEW(Result);
  TSepiTypeAlias.Create(Result, '_PROPSHEETPAGE', '_PROPSHEETPAGEA');
  TSepiTypeAlias.Create(Result, 'TPropSheetPageA', '_PROPSHEETPAGEA');
  TSepiTypeAlias.Create(Result, 'TPropSheetPageW', '_PROPSHEETPAGEW');
  TSepiTypeAlias.Create(Result, 'TPropSheetPage', 'TPropSheetPageA');
  TSepiTypeAlias.Create(Result, 'PROPSHEETPAGEA', '_PROPSHEETPAGEA');
  TSepiTypeAlias.Create(Result, 'PROPSHEETPAGEW', '_PROPSHEETPAGEW');
  TSepiTypeAlias.Create(Result, 'PROPSHEETPAGE', 'PROPSHEETPAGEA');
  TSepiMethodRefType.Create(Result, 'PFNPROPSHEETCALLBACK',
    'function(Wnd: HWnd; Msg: Integer; LParam: Integer ) : Integer',
    False, ccStdCall);
  TSepiTypeAlias.Create(Result, 'TFNPropSheetCallback',
    'PFNPROPSHEETCALLBACK');
  TSepiPointerType.Create(Result, 'PPropSheetHeaderA',
    'TPropSheetHeaderA', True);
  TSepiPointerType.Create(Result, 'PPropSheetHeaderW',
    'TPropSheetHeaderW', True);
  TSepiTypeAlias.Create(Result, 'PPropSheetHeader', 'PPropSheetHeaderA');
  SepiImport_PROPSHEETHEADERA(Result);
  SepiImport_PROPSHEETHEADERW(Result);
  TSepiTypeAlias.Create(Result, '_PROPSHEETHEADER', '_PROPSHEETHEADERA');
  TSepiTypeAlias.Create(Result, 'TPropSheetHeaderA', '_PROPSHEETHEADERA');
  TSepiTypeAlias.Create(Result, 'TPropSheetHeaderW', '_PROPSHEETHEADERW');
  TSepiTypeAlias.Create(Result, 'TPropSheetHeader', 'TPropSheetHeaderA');
  TSepiMethodRefType.Create(Result, 'LPFNADDPROPSHEETPAGE',
    'function(hPSP: HPropSheetPage; lParam: Longint ) : BOOL',
    False, ccStdCall);
  TSepiTypeAlias.Create(Result, 'TFNAddPropSheetPage', 'LPFNADDPROPSHEETPAGE');
  TSepiMethodRefType.Create(Result, 'LPFNADDPROPSHEETPAGES',
    'function(lpvoid: Pointer; pfn: TFNAddPropSheetPage; lParam: Longint ) : BOOL',
    False, ccStdCall);
  TSepiTypeAlias.Create(Result, 'TFNAddPropSheetPages',
    'LPFNADDPROPSHEETPAGES');

  // Types
  SepiImporttagINITCOMMONCONTROLSEX(Result);
  TSepiPointerType.Create(Result, 'PInitCommonControlsEx',
    'TInitCommonControlsEx', True);
  TSepiTypeAlias.Create(Result, 'TInitCommonControlsEx',
    'tagINITCOMMONCONTROLSEX');

  // Types
  SepiImporttagCOLORSCHEME(Result);
  TSepiPointerType.Create(Result, 'PColorScheme', 'TColorScheme', True);
  TSepiTypeAlias.Create(Result, 'TColorScheme', 'tagCOLORSCHEME');

  // Types
  SepiImporttagNMMOUSE(Result);
  TSepiPointerType.Create(Result, 'PNMMouse', 'TNMMouse', True);
  TSepiTypeAlias.Create(Result, 'TNMMouse', 'tagNMMOUSE');
  TSepiPointerType.Create(Result, 'PNMClick', 'TNMClick', True);
  TSepiTypeAlias.Create(Result, 'TNMClick', 'tagNMMOUSE');
  SepiImporttagNMOBJECTNOTIFY(Result);
  TSepiPointerType.Create(Result, 'PNMObjectNotify', 'TNMObjectNotify', True);
  TSepiTypeAlias.Create(Result, 'TNMObjectNotify', 'tagNMOBJECTNOTIFY');
  SepiImporttagNMKEY(Result);
  TSepiPointerType.Create(Result, 'PNMKey', 'TNMKey', True);
  TSepiTypeAlias.Create(Result, 'TNMKey', 'tagNMKEY');
  SepiImporttagNMCHAR(Result);
  TSepiPointerType.Create(Result, 'PNMChar', 'TNMChar', True);
  TSepiTypeAlias.Create(Result, 'TNMChar', 'tagNMCHAR');

  // Types
  SepiImporttagNMCUSTOMDRAWINFO(Result);
  TSepiPointerType.Create(Result, 'PNMCustomDraw', 'TNMCustomDraw', True);
  TSepiTypeAlias.Create(Result, 'TNMCustomDraw', 'tagNMCUSTOMDRAWINFO');
  SepiImporttagNMTTCUSTOMDRAW(Result);
  TSepiPointerType.Create(Result, 'PNMTTCustomDraw', 'TNMTTCustomDraw', True);
  TSepiTypeAlias.Create(Result, 'TNMTTCustomDraw', 'tagNMTTCUSTOMDRAW');

  // Types
  TSepiTypeAlias.Create(Result, 'HIMAGELIST', TypeInfo(THandle));
  SepiImport_IMAGELISTDRAWPARAMS(Result);
  TSepiPointerType.Create(Result, 'PImageListDrawParams',
    'TImageListDrawParams', True);
  TSepiTypeAlias.Create(Result, 'TImageListDrawParams',
    '_IMAGELISTDRAWPARAMS');

  // Types
  TSepiPointerType.Create(Result, 'PImageInfo', 'TImageInfo', True);
  SepiImport_IMAGEINFO(Result);
  TSepiTypeAlias.Create(Result, 'TImageInfo', '_IMAGEINFO');
  TSepiTypeAlias.Create(Result, 'IMAGEINFO', '_IMAGEINFO');

  // Types
  TSepiPointerType.Create(Result, 'PHDItemA', 'THDItemA', True);
  TSepiPointerType.Create(Result, 'PHDItemW', 'THDItemW', True);
  TSepiTypeAlias.Create(Result, 'PHDItem', 'PHDItemA');
  SepiImport_HD_ITEMA(Result);
  SepiImport_HD_ITEMW(Result);
  TSepiTypeAlias.Create(Result, '_HD_ITEM', '_HD_ITEMA');
  TSepiTypeAlias.Create(Result, 'THDItemA', '_HD_ITEMA');
  TSepiTypeAlias.Create(Result, 'THDItemW', '_HD_ITEMW');
  TSepiTypeAlias.Create(Result, 'THDItem', 'THDItemA');
  TSepiTypeAlias.Create(Result, 'HD_ITEMA', '_HD_ITEMA');
  TSepiTypeAlias.Create(Result, 'HD_ITEMW', '_HD_ITEMW');
  TSepiTypeAlias.Create(Result, 'HD_ITEM', 'HD_ITEMA');

  // Types
  TSepiPointerType.Create(Result, 'PHDLayout', 'THDLayout', True);
  TSepiPointerType.Create(Result, '$1', 'TRect', True);
  SepiImport_HD_LAYOUT(Result);
  TSepiTypeAlias.Create(Result, 'THDLayout', '_HD_LAYOUT');
  TSepiTypeAlias.Create(Result, 'HD_LAYOUT', '_HD_LAYOUT');

  // Types
  TSepiPointerType.Create(Result, 'PHDHitTestInfo', 'THDHitTestInfo', True);
  SepiImport_HD_HITTESTINFO(Result);
  TSepiTypeAlias.Create(Result, 'THDHitTestInfo', '_HD_HITTESTINFO');
  TSepiTypeAlias.Create(Result, 'HD_HITTESTINFO', '_HD_HITTESTINFO');

  // Types
  SepiImporttagNMHEADERA(Result);
  SepiImporttagNMHEADERW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMHEADER', 'tagNMHEADERA');
  TSepiTypeAlias.Create(Result, 'HD_NOTIFYA', 'tagNMHEADERA');
  TSepiTypeAlias.Create(Result, 'HD_NOTIFYW', 'tagNMHEADERW');
  TSepiTypeAlias.Create(Result, 'HD_NOTIFY', 'HD_NOTIFYA');
  TSepiPointerType.Create(Result, 'PHDNotifyA', 'THDNotifyA', True);
  TSepiPointerType.Create(Result, 'PHDNotifyW', 'THDNotifyW', True);
  TSepiTypeAlias.Create(Result, 'PHDNotify', 'PHDNotifyA');
  TSepiTypeAlias.Create(Result, 'THDNotifyA', 'tagNMHEADERA');
  TSepiTypeAlias.Create(Result, 'THDNotifyW', 'tagNMHEADERW');
  TSepiTypeAlias.Create(Result, 'THDNotify', 'THDNotifyA');
  SepiImporttagNMHDDISPINFOA(Result);
  SepiImporttagNMHDDISPINFOW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMHDDISPINFO', 'tagNMHDDISPINFOA');
  TSepiPointerType.Create(Result, 'PNMHDispInfoA', 'TNMHDispInfoA', True);
  TSepiPointerType.Create(Result, 'PNMHDispInfoW', 'TNMHDispInfoW', True);
  TSepiTypeAlias.Create(Result, 'PNMHDispInfo', 'PNMHDispInfoA');
  TSepiTypeAlias.Create(Result, 'TNMHDispInfoA', 'tagNMHDDISPINFOA');
  TSepiTypeAlias.Create(Result, 'TNMHDispInfoW', 'tagNMHDDISPINFOW');
  TSepiTypeAlias.Create(Result, 'TNMHDispInfo', 'TNMHDispInfoA');

  // Types
  TSepiPointerType.Create(Result, 'PTBButton', 'TTBButton', True);
  TSepiArrayType.Create(Result, '$2',
    [1, 2], TypeInfo(Byte), True);
  SepiImport_TBBUTTON(Result);
  TSepiTypeAlias.Create(Result, 'TTBButton', '_TBBUTTON');
  TSepiPointerType.Create(Result, 'PColorMap', 'TColorMap', True);
  SepiImport_COLORMAP(Result);
  TSepiTypeAlias.Create(Result, 'TColorMap', '_COLORMAP');
  TSepiTypeAlias.Create(Result, 'COLORMAP', '_COLORMAP');

  // Types
  SepiImport_NMTBCUSTOMDRAW(Result);
  TSepiPointerType.Create(Result, 'PNMTBCustomDraw', 'TNMTBCustomDraw', True);
  TSepiTypeAlias.Create(Result, 'TNMTBCustomDraw', '_NMTBCUSTOMDRAW');

  // Types
  TSepiPointerType.Create(Result, 'PTBAddBitmap', 'TTBAddBitmap', True);
  SepiImporttagTBADDBITMAP(Result);
  TSepiTypeAlias.Create(Result, 'TTBAddBitmap', 'tagTBADDBITMAP');
  TSepiTypeAlias.Create(Result, 'TBADDBITMAP', 'tagTBADDBITMAP');

  // Types
  TSepiPointerType.Create(Result, 'PTBSaveParamsA', 'TTBSaveParamsA', True);
  TSepiPointerType.Create(Result, 'PTBSaveParamsW', 'TTBSaveParamsW', True);
  TSepiTypeAlias.Create(Result, 'PTBSaveParams', 'PTBSaveParamsA');
  SepiImporttagTBSAVEPARAMSA(Result);
  SepiImporttagTBSAVEPARAMSW(Result);
  TSepiTypeAlias.Create(Result, 'tagTBSAVEPARAMS', 'tagTBSAVEPARAMSA');
  TSepiTypeAlias.Create(Result, 'TTBSaveParamsA', 'tagTBSAVEPARAMSA');
  TSepiTypeAlias.Create(Result, 'TTBSaveParamsW', 'tagTBSAVEPARAMSW');
  TSepiTypeAlias.Create(Result, 'TTBSaveParams', 'TTBSaveParamsA');
  TSepiTypeAlias.Create(Result, 'TBSAVEPARAMSA', 'tagTBSAVEPARAMSA');
  TSepiTypeAlias.Create(Result, 'TBSAVEPARAMSW', 'tagTBSAVEPARAMSW');
  TSepiTypeAlias.Create(Result, 'TBSAVEPARAMS', 'TBSAVEPARAMSA');

  // Types
  SepiImportTBINSERTMARK(Result);
  TSepiPointerType.Create(Result, 'PTBInsertMark', 'TTBInsertMark', True);
  TSepiTypeAlias.Create(Result, 'TTBInsertMark', 'TBINSERTMARK');

  // Types
  SepiImportTBREPLACEBITMAP(Result);
  TSepiPointerType.Create(Result, 'PTBReplaceBitmap',
    'TTBReplaceBitmap', True);
  TSepiTypeAlias.Create(Result, 'TTBReplaceBitmap', 'TBREPLACEBITMAP');

  // Types
  SepiImportTBBUTTONINFOA(Result);
  SepiImportTBBUTTONINFOW(Result);
  TSepiTypeAlias.Create(Result, 'TBBUTTONINFO', 'TBBUTTONINFOA');
  TSepiPointerType.Create(Result, 'PTBButtonInfoA', 'TTBButtonInfoA', True);
  TSepiPointerType.Create(Result, 'PTBButtonInfoW', 'TTBButtonInfoW', True);
  TSepiTypeAlias.Create(Result, 'PTBButtonInfo', 'PTBButtonInfoA');
  TSepiTypeAlias.Create(Result, 'TTBButtonInfoA', 'TBBUTTONINFOA');
  TSepiTypeAlias.Create(Result, 'TTBButtonInfoW', 'TBBUTTONINFOW');
  TSepiTypeAlias.Create(Result, 'TTBButtonInfo', 'TTBButtonInfoA');

  // Types
  SepiImporttagNMTBHOTITEM(Result);
  TSepiPointerType.Create(Result, 'PNMTBHotItem', 'TNMTBHotItem', True);
  TSepiTypeAlias.Create(Result, 'TNMTBHotItem', 'tagNMTBHOTITEM');

  // Types
  SepiImporttagNMTBGETINFOTIPA(Result);
  SepiImporttagNMTBGETINFOTIPW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMTBGETINFOTIP', 'tagNMTBGETINFOTIPA');
  TSepiPointerType.Create(Result, 'PNMTBGetInfoTipA',
    'TNMTBGetInfoTipA', True);
  TSepiPointerType.Create(Result, 'PNMTBGetInfoTipW',
    'TNMTBGetInfoTipW', True);
  TSepiTypeAlias.Create(Result, 'PNMTBGetInfoTip', 'PNMTBGetInfoTipA');
  TSepiTypeAlias.Create(Result, 'TNMTBGetInfoTipA', 'tagNMTBGETINFOTIPA');
  TSepiTypeAlias.Create(Result, 'TNMTBGetInfoTipW', 'tagNMTBGETINFOTIPW');
  TSepiTypeAlias.Create(Result, 'TNMTBGetInfoTip', 'TNMTBGetInfoTipA');

  // Types
  SepiImportNMTBDISPINFOA(Result);
  SepiImportNMTBDISPINFOW(Result);
  TSepiTypeAlias.Create(Result, 'NMTBDISPINFO', 'NMTBDISPINFOA');
  TSepiPointerType.Create(Result, 'PNMTBDispInfoA', 'TNMTBDispInfoA', True);
  TSepiPointerType.Create(Result, 'PNMTBDispInfoW', 'TNMTBDispInfoW', True);
  TSepiTypeAlias.Create(Result, 'PNMTBDispInfo', 'PNMTBDispInfoA');
  TSepiTypeAlias.Create(Result, 'TNMTBDispInfoA', 'NMTBDISPINFOA');
  TSepiTypeAlias.Create(Result, 'TNMTBDispInfoW', 'NMTBDISPINFOW');
  TSepiTypeAlias.Create(Result, 'TNMTBDispInfo', 'TNMTBDispInfoA');

  // Types
  SepiImporttagNMTOOLBARA(Result);
  SepiImporttagNMTOOLBARW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMTOOLBAR', 'tagNMTOOLBARA');
  TSepiPointerType.Create(Result, 'PNMToolBarA', 'TNMToolBarA', True);
  TSepiPointerType.Create(Result, 'PNMToolBarW', 'TNMToolBarW', True);
  TSepiTypeAlias.Create(Result, 'PNMToolBar', 'PNMToolBarA');
  TSepiTypeAlias.Create(Result, 'TNMToolBarA', 'tagNMTOOLBARA');
  TSepiTypeAlias.Create(Result, 'TNMToolBarW', 'tagNMTOOLBARW');
  TSepiTypeAlias.Create(Result, 'TNMToolBar', 'TNMToolBarA');

  // Types
  SepiImporttagREBARINFO(Result);
  TSepiPointerType.Create(Result, 'PReBarInfo', 'TReBarInfo', True);
  TSepiTypeAlias.Create(Result, 'TReBarInfo', 'tagREBARINFO');

  // Types
  SepiImporttagREBARBANDINFOA(Result);
  SepiImporttagREBARBANDINFOW(Result);
  TSepiTypeAlias.Create(Result, 'tagREBARBANDINFO', 'tagREBARBANDINFOA');
  TSepiPointerType.Create(Result, 'PReBarBandInfoA', 'TReBarBandInfoA', True);
  TSepiPointerType.Create(Result, 'PReBarBandInfoW', 'TReBarBandInfoW', True);
  TSepiTypeAlias.Create(Result, 'PReBarBandInfo', 'PReBarBandInfoA');
  TSepiTypeAlias.Create(Result, 'TReBarBandInfoA', 'tagREBARBANDINFOA');
  TSepiTypeAlias.Create(Result, 'TReBarBandInfoW', 'tagREBARBANDINFOW');
  TSepiTypeAlias.Create(Result, 'TReBarBandInfo', 'TReBarBandInfoA');

  // Types
  SepiImporttagNMREBARCHILDSIZE(Result);
  TSepiPointerType.Create(Result, 'PNMReBarChildSize',
    'TNMReBarChildSize', True);
  TSepiTypeAlias.Create(Result, 'TNMReBarChildSize', 'tagNMREBARCHILDSIZE');
  SepiImporttagNMREBAR(Result);
  TSepiPointerType.Create(Result, 'PNMReBar', 'TNMReBar', True);
  TSepiTypeAlias.Create(Result, 'TNMReBar', 'tagNMREBAR');

  // Types
  SepiImporttagNMRBAUTOSIZE(Result);
  TSepiPointerType.Create(Result, 'PNMRBAutoSize', 'TNMRBAutoSize', True);
  TSepiTypeAlias.Create(Result, 'TNMRBAutoSize', 'tagNMRBAUTOSIZE');

  // Types
  SepiImport_RB_HITTESTINFO(Result);
  TSepiPointerType.Create(Result, 'PRBHitTestInfo', 'TRBHitTestInfo', True);
  TSepiTypeAlias.Create(Result, 'TRBHitTestInfo', '_RB_HITTESTINFO');

  // Types
  TSepiPointerType.Create(Result, 'PToolInfoA', 'TToolInfoA', True);
  TSepiPointerType.Create(Result, 'PToolInfoW', 'TToolInfoW', True);
  TSepiTypeAlias.Create(Result, 'PToolInfo', 'PToolInfoA');
  SepiImporttagTOOLINFOA(Result);
  SepiImporttagTOOLINFOW(Result);
  TSepiTypeAlias.Create(Result, 'tagTOOLINFO', 'tagTOOLINFOA');
  TSepiTypeAlias.Create(Result, 'TToolInfoA', 'tagTOOLINFOA');
  TSepiTypeAlias.Create(Result, 'TToolInfoW', 'tagTOOLINFOW');
  TSepiTypeAlias.Create(Result, 'TToolInfo', 'TToolInfoA');
  TSepiTypeAlias.Create(Result, 'TOOLINFOA', 'tagTOOLINFOA');
  TSepiTypeAlias.Create(Result, 'TOOLINFOW', 'tagTOOLINFOW');
  TSepiTypeAlias.Create(Result, 'TOOLINFO', 'TOOLINFOA');

  // Types
  TSepiPointerType.Create(Result, 'PTTHitTestInfoA', 'TTTHitTestInfoA', True);
  TSepiPointerType.Create(Result, 'PTTHitTestInfoW', 'TTTHitTestInfoW', True);
  TSepiTypeAlias.Create(Result, 'PTTHitTestInfo', 'PTTHitTestInfoA');
  SepiImport_TT_HITTESTINFOA(Result);
  SepiImport_TT_HITTESTINFOW(Result);
  TSepiTypeAlias.Create(Result, '_TT_HITTESTINFO', '_TT_HITTESTINFOA');
  TSepiTypeAlias.Create(Result, 'TTTHitTestInfoA', '_TT_HITTESTINFOA');
  TSepiTypeAlias.Create(Result, 'TTTHitTestInfoW', '_TT_HITTESTINFOW');
  TSepiTypeAlias.Create(Result, 'TTTHitTestInfo', 'TTTHitTestInfoA');
  TSepiTypeAlias.Create(Result, 'TTHITTESTINFOA', '_TT_HITTESTINFOA');
  TSepiTypeAlias.Create(Result, 'TTHITTESTINFOW', '_TT_HITTESTINFOW');
  TSepiTypeAlias.Create(Result, 'TTHITTESTINFO', 'TTHITTESTINFOA');

  // Types
  TSepiArrayType.Create(Result, '$3',
    [0, 79], TypeInfo(AnsiChar), True);
  SepiImporttagNMTTDISPINFOA(Result);
  TSepiArrayType.Create(Result, '$4',
    [0, 79], TypeInfo(WideChar), True);
  SepiImporttagNMTTDISPINFOW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMTTDISPINFO', 'tagNMTTDISPINFOA');
  TSepiPointerType.Create(Result, 'PNMTTDispInfoA', 'TNMTTDispInfoA', True);
  TSepiPointerType.Create(Result, 'PNMTTDispInfoW', 'TNMTTDispInfoW', True);
  TSepiTypeAlias.Create(Result, 'PNMTTDispInfo', 'PNMTTDispInfoA');
  TSepiTypeAlias.Create(Result, 'TNMTTDispInfoA', 'tagNMTTDISPINFOA');
  TSepiTypeAlias.Create(Result, 'TNMTTDispInfoW', 'tagNMTTDISPINFOW');
  TSepiTypeAlias.Create(Result, 'TNMTTDispInfo', 'TNMTTDispInfoA');
  TSepiTypeAlias.Create(Result, 'tagTOOLTIPTEXTA', 'tagNMTTDISPINFOA');
  TSepiTypeAlias.Create(Result, 'tagTOOLTIPTEXTW', 'tagNMTTDISPINFOW');
  TSepiTypeAlias.Create(Result, 'tagTOOLTIPTEXT', 'tagTOOLTIPTEXTA');
  TSepiTypeAlias.Create(Result, 'TOOLTIPTEXTA', 'tagNMTTDISPINFOA');
  TSepiTypeAlias.Create(Result, 'TOOLTIPTEXTW', 'tagNMTTDISPINFOW');
  TSepiTypeAlias.Create(Result, 'TOOLTIPTEXT', 'TOOLTIPTEXTA');
  TSepiTypeAlias.Create(Result, 'TToolTipTextA', 'tagNMTTDISPINFOA');
  TSepiTypeAlias.Create(Result, 'TToolTipTextW', 'tagNMTTDISPINFOW');
  TSepiTypeAlias.Create(Result, 'TToolTipText', 'TToolTipTextA');
  TSepiPointerType.Create(Result, 'PToolTipTextA', 'TToolTipTextA', True);
  TSepiPointerType.Create(Result, 'PToolTipTextW', 'TToolTipTextW', True);
  TSepiTypeAlias.Create(Result, 'PToolTipText', 'PToolTipTextA');

  // Types
  TSepiPointerType.Create(Result, 'PDragListInfo', 'TDragListInfo', True);
  SepiImporttagDRAGLISTINFO(Result);
  TSepiTypeAlias.Create(Result, 'TDragListInfo', 'tagDRAGLISTINFO');
  TSepiTypeAlias.Create(Result, 'DRAGLISTINFO', 'tagDRAGLISTINFO');

  // Types
  TSepiPointerType.Create(Result, 'PUDAccel', 'TUDAccel', True);
  SepiImport_UDACCEL(Result);
  TSepiTypeAlias.Create(Result, 'TUDAccel', '_UDACCEL');
  TSepiTypeAlias.Create(Result, 'UDACCEL', '_UDACCEL');

  // Types
  TSepiPointerType.Create(Result, 'PNMUpDown', 'TNMUpDown', True);
  SepiImport_NM_UPDOWN(Result);
  TSepiTypeAlias.Create(Result, 'TNMUpDown', '_NM_UPDOWN');
  TSepiTypeAlias.Create(Result, 'NM_UPDOWN', '_NM_UPDOWN');

  // Types
  SepiImportPBRANGE(Result);
  TSepiPointerType.Create(Result, 'PPBRange', 'TPBRange', True);
  TSepiTypeAlias.Create(Result, 'TPBRange', 'PBRANGE');

  // Types
  TSepiPointerType.Create(Result, 'PLVItemA', 'TLVItemA', True);
  TSepiPointerType.Create(Result, 'PLVItemW', 'TLVItemW', True);
  TSepiTypeAlias.Create(Result, 'PLVItem', 'PLVItemA');
  SepiImporttagLVITEMA(Result);
  SepiImporttagLVITEMW(Result);
  TSepiTypeAlias.Create(Result, 'tagLVITEM', 'tagLVITEMA');
  TSepiTypeAlias.Create(Result, '_LV_ITEMA', 'tagLVITEMA');
  TSepiTypeAlias.Create(Result, '_LV_ITEMW', 'tagLVITEMW');
  TSepiTypeAlias.Create(Result, '_LV_ITEM', '_LV_ITEMA');
  TSepiTypeAlias.Create(Result, 'TLVItemA', 'tagLVITEMA');
  TSepiTypeAlias.Create(Result, 'TLVItemW', 'tagLVITEMW');
  TSepiTypeAlias.Create(Result, 'TLVItem', 'TLVItemA');
  TSepiTypeAlias.Create(Result, 'LV_ITEMA', 'tagLVITEMA');
  TSepiTypeAlias.Create(Result, 'LV_ITEMW', 'tagLVITEMW');
  TSepiTypeAlias.Create(Result, 'LV_ITEM', 'LV_ITEMA');

  // Types
  TSepiPointerType.Create(Result, 'PLVFindInfoA', 'TLVFindInfoA', True);
  TSepiPointerType.Create(Result, 'PLVFindInfoW', 'TLVFindInfoW', True);
  TSepiTypeAlias.Create(Result, 'PLVFindInfo', 'PLVFindInfoA');
  SepiImporttagLVFINDINFOA(Result);
  SepiImporttagLVFINDINFOW(Result);
  TSepiTypeAlias.Create(Result, 'tagLVFINDINFO', 'tagLVFINDINFOA');
  TSepiTypeAlias.Create(Result, '_LV_FINDINFOA', 'tagLVFINDINFOA');
  TSepiTypeAlias.Create(Result, '_LV_FINDINFOW', 'tagLVFINDINFOW');
  TSepiTypeAlias.Create(Result, '_LV_FINDINFO', '_LV_FINDINFOA');
  TSepiTypeAlias.Create(Result, 'TLVFindInfoA', 'tagLVFINDINFOA');
  TSepiTypeAlias.Create(Result, 'TLVFindInfoW', 'tagLVFINDINFOW');
  TSepiTypeAlias.Create(Result, 'TLVFindInfo', 'TLVFindInfoA');
  TSepiTypeAlias.Create(Result, 'LV_FINDINFOA', 'tagLVFINDINFOA');
  TSepiTypeAlias.Create(Result, 'LV_FINDINFOW', 'tagLVFINDINFOW');
  TSepiTypeAlias.Create(Result, 'LV_FINDINFO', 'LV_FINDINFOA');

  // Types
  TSepiPointerType.Create(Result, 'PLVHitTestInfo', 'TLVHitTestInfo', True);
  SepiImporttagLVHITTESTINFO(Result);
  TSepiTypeAlias.Create(Result, 'TLVHitTestInfo', 'tagLVHITTESTINFO');
  TSepiTypeAlias.Create(Result, 'LV_HITTESTINFO', 'tagLVHITTESTINFO');
  TSepiTypeAlias.Create(Result, '_LV_HITTESTINFO', 'tagLVHITTESTINFO');

  // Types
  TSepiPointerType.Create(Result, 'PLVColumnA', 'TLVColumnA', True);
  TSepiPointerType.Create(Result, 'PLVColumnW', 'TLVColumnW', True);
  TSepiTypeAlias.Create(Result, 'PLVColumn', 'PLVColumnA');
  SepiImporttagLVCOLUMNA(Result);
  SepiImporttagLVCOLUMNW(Result);
  TSepiTypeAlias.Create(Result, 'tagLVCOLUMN', 'tagLVCOLUMNA');
  TSepiTypeAlias.Create(Result, '_LV_COLUMNA', 'tagLVCOLUMNA');
  TSepiTypeAlias.Create(Result, '_LV_COLUMNW', 'tagLVCOLUMNW');
  TSepiTypeAlias.Create(Result, '_LV_COLUMN', '_LV_COLUMNA');
  TSepiTypeAlias.Create(Result, 'TLVColumnA', 'tagLVCOLUMNA');
  TSepiTypeAlias.Create(Result, 'TLVColumnW', 'tagLVCOLUMNW');
  TSepiTypeAlias.Create(Result, 'TLVColumn', 'TLVColumnA');
  TSepiTypeAlias.Create(Result, 'LV_COLUMNA', 'tagLVCOLUMNA');
  TSepiTypeAlias.Create(Result, 'LV_COLUMNW', 'tagLVCOLUMNW');
  TSepiTypeAlias.Create(Result, 'LV_COLUMN', 'LV_COLUMNA');

  // Types
  TSepiMethodRefType.Create(Result, 'PFNLVCOMPARE',
    'function(lParam1, lParam2, lParamSort: Integer): Integer',
    False, ccStdCall);
  TSepiTypeAlias.Create(Result, 'TLVCompare', 'PFNLVCOMPARE');

  // Types
  SepiImporttagLVBKIMAGEA(Result);
  SepiImporttagLVBKIMAGEW(Result);
  TSepiTypeAlias.Create(Result, 'tagLVBKIMAGE', 'tagLVBKIMAGEA');
  TSepiPointerType.Create(Result, 'PLVBKImageA', 'TLVBKImageA', True);
  TSepiPointerType.Create(Result, 'PLVBKImageW', 'TLVBKImageW', True);
  TSepiTypeAlias.Create(Result, 'PLVBKImage', 'PLVBKImageA');
  TSepiTypeAlias.Create(Result, 'TLVBKImageA', 'tagLVBKIMAGEA');
  TSepiTypeAlias.Create(Result, 'TLVBKImageW', 'tagLVBKIMAGEW');
  TSepiTypeAlias.Create(Result, 'TLVBKImage', 'TLVBKImageA');

  // Types
  SepiImporttagNMLISTVIEW(Result);
  TSepiTypeAlias.Create(Result, '_NM_LISTVIEW', 'tagNMLISTVIEW');
  TSepiTypeAlias.Create(Result, 'NM_LISTVIEW', 'tagNMLISTVIEW');
  TSepiPointerType.Create(Result, 'PNMListView', 'TNMListView', True);
  TSepiTypeAlias.Create(Result, 'TNMListView', 'tagNMLISTVIEW');
  SepiImporttagNMITEMACTIVATE(Result);
  TSepiPointerType.Create(Result, 'PNMItemActivate', 'TNMItemActivate', True);
  TSepiTypeAlias.Create(Result, 'TNMItemActivate', 'tagNMITEMACTIVATE');

  // Types
  SepiImporttagNMLVCUSTOMDRAW(Result);
  TSepiPointerType.Create(Result, 'PNMLVCustomDraw', 'TNMLVCustomDraw', True);
  TSepiTypeAlias.Create(Result, 'TNMLVCustomDraw', 'tagNMLVCUSTOMDRAW');
  SepiImporttagNMLVCACHEHINT(Result);
  TSepiPointerType.Create(Result, 'PNMLVCacheHint', 'TNMLVCacheHint', True);
  TSepiTypeAlias.Create(Result, 'TNMLVCacheHint', 'tagNMLVCACHEHINT');
  TSepiPointerType.Create(Result, 'PNMCacheHint', 'TNMCacheHint', True);
  TSepiTypeAlias.Create(Result, 'TNMCacheHint', 'tagNMLVCACHEHINT');
  SepiImporttagNMLVFINDITEMA(Result);
  SepiImporttagNMLVFINDITEMW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMLVFINDITEM', 'tagNMLVFINDITEMA');
  TSepiPointerType.Create(Result, 'PNMLVFinditemA', 'TNMLVFinditemA', True);
  TSepiTypeAlias.Create(Result, 'TNMLVFinditemA', 'tagNMLVFINDITEMA');
  TSepiPointerType.Create(Result, 'PNMLVFinditemW', 'TNMLVFinditemW', True);
  TSepiTypeAlias.Create(Result, 'TNMLVFinditemW', 'tagNMLVFINDITEMW');
  TSepiTypeAlias.Create(Result, 'PNMLVFinditem', 'PNMLVFinditemA');
  TSepiPointerType.Create(Result, 'PNMFinditemA', 'TNMFinditemA', True);
  TSepiTypeAlias.Create(Result, 'TNMFinditemA', 'tagNMLVFINDITEMA');
  TSepiPointerType.Create(Result, 'PNMFinditemW', 'TNMFinditemW', True);
  TSepiTypeAlias.Create(Result, 'TNMFinditemW', 'tagNMLVFINDITEMW');
  TSepiTypeAlias.Create(Result, 'PNMFinditem', 'PNMFinditemA');
  SepiImporttagNMLVODSTATECHANGE(Result);
  TSepiPointerType.Create(Result, 'PNMLVODStateChange',
    'TNMLVODStateChange', True);
  TSepiTypeAlias.Create(Result, 'TNMLVODStateChange', 'tagNMLVODSTATECHANGE');

  // Types
  TSepiPointerType.Create(Result, 'PLVDispInfoA', 'TLVDispInfoA', True);
  TSepiPointerType.Create(Result, 'PLVDispInfoW', 'TLVDispInfoW', True);
  TSepiTypeAlias.Create(Result, 'PLVDispInfo', 'PLVDispInfoA');
  SepiImporttagLVDISPINFO(Result);
  TSepiTypeAlias.Create(Result, '_LV_DISPINFO', 'tagLVDISPINFO');
  SepiImporttagLVDISPINFOW(Result);
  TSepiTypeAlias.Create(Result, '_LV_DISPINFOW', 'tagLVDISPINFOW');
  TSepiTypeAlias.Create(Result, 'TLVDispInfoA', 'tagLVDISPINFO');
  TSepiTypeAlias.Create(Result, 'TLVDispInfoW', 'tagLVDISPINFOW');
  TSepiTypeAlias.Create(Result, 'TLVDispInfo', 'TLVDispInfoA');
  TSepiTypeAlias.Create(Result, 'LV_DISPINFOA', 'tagLVDISPINFO');
  TSepiTypeAlias.Create(Result, 'LV_DISPINFOW', 'tagLVDISPINFOW');
  TSepiTypeAlias.Create(Result, 'LV_DISPINFO', 'LV_DISPINFOA');

  // Types
  TSepiPointerType.Create(Result, 'PLVKeyDown', 'TLVKeyDown', True);
  SepiImporttagLVKEYDOWN(Result);
  TSepiTypeAlias.Create(Result, '_LV_KEYDOWN', 'tagLVKEYDOWN');
  TSepiTypeAlias.Create(Result, 'TLVKeyDown', 'tagLVKEYDOWN');
  TSepiTypeAlias.Create(Result, 'LV_KEYDOWN', 'tagLVKEYDOWN');

  // Types
  SepiImporttagNMLVGETINFOTIPA(Result);
  SepiImporttagNMLVGETINFOTIPW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMLVGETINFOTIP', 'tagNMLVGETINFOTIPA');
  TSepiPointerType.Create(Result, 'PNMLVGetInfoTipA',
    'TNMLVGetInfoTipA', True);
  TSepiPointerType.Create(Result, 'PNMLVGetInfoTipW',
    'TNMLVGetInfoTipW', True);
  TSepiTypeAlias.Create(Result, 'PNMLVGetInfoTip', 'PNMLVGetInfoTipA');
  TSepiTypeAlias.Create(Result, 'TNMLVGetInfoTipA', 'tagNMLVGETINFOTIPA');
  TSepiTypeAlias.Create(Result, 'TNMLVGetInfoTipW', 'tagNMLVGETINFOTIPW');
  TSepiTypeAlias.Create(Result, 'TNMLVGetInfoTip', 'TNMLVGetInfoTipA');

  // Types
  TSepiPointerType.Create(Result, 'HTREEITEM', '_TREEITEM', True);
  SepiImport_TREEITEM(Result);

  // Types
  TSepiPointerType.Create(Result, 'PTVItemA', 'TTVItemA', True);
  TSepiPointerType.Create(Result, 'PTVItemW', 'TTVItemW', True);
  TSepiTypeAlias.Create(Result, 'PTVItem', 'PTVItemA');
  SepiImporttagTVITEMA(Result);
  SepiImporttagTVITEMW(Result);
  TSepiTypeAlias.Create(Result, 'tagTVITEM', 'tagTVITEMA');
  TSepiTypeAlias.Create(Result, '_TV_ITEMA', 'tagTVITEMA');
  TSepiTypeAlias.Create(Result, '_TV_ITEMW', 'tagTVITEMW');
  TSepiTypeAlias.Create(Result, '_TV_ITEM', '_TV_ITEMA');
  TSepiTypeAlias.Create(Result, 'TTVItemA', 'tagTVITEMA');
  TSepiTypeAlias.Create(Result, 'TTVItemW', 'tagTVITEMW');
  TSepiTypeAlias.Create(Result, 'TTVItem', 'TTVItemA');
  TSepiTypeAlias.Create(Result, 'TV_ITEMA', 'tagTVITEMA');
  TSepiTypeAlias.Create(Result, 'TV_ITEMW', 'tagTVITEMW');
  TSepiTypeAlias.Create(Result, 'TV_ITEM', 'TV_ITEMA');
  SepiImporttagTVITEMEXA(Result);
  SepiImporttagTVITEMEXW(Result);
  TSepiTypeAlias.Create(Result, 'tagTVITEMEX', 'tagTVITEMEXA');
  TSepiPointerType.Create(Result, 'PTVItemExA', 'TTVItemExA', True);
  TSepiPointerType.Create(Result, 'PTVItemExW', 'TTVItemExW', True);
  TSepiTypeAlias.Create(Result, 'PTVItemEx', 'PTVItemExA');
  TSepiTypeAlias.Create(Result, 'TTVItemExA', 'tagTVITEMEXA');
  TSepiTypeAlias.Create(Result, 'TTVItemExW', 'tagTVITEMEXW');
  TSepiTypeAlias.Create(Result, 'TTVItemEx', 'TTVItemExA');

  // Types
  TSepiPointerType.Create(Result, 'PTVInsertStructA',
    'TTVInsertStructA', True);
  TSepiPointerType.Create(Result, 'PTVInsertStructW',
    'TTVInsertStructW', True);
  TSepiTypeAlias.Create(Result, 'PTVInsertStruct', 'PTVInsertStructA');
  SepiImporttagTVINSERTSTRUCTA(Result);
  SepiImporttagTVINSERTSTRUCTW(Result);
  TSepiTypeAlias.Create(Result, 'tagTVINSERTSTRUCT', 'tagTVINSERTSTRUCTA');
  TSepiTypeAlias.Create(Result, '_TV_INSERTSTRUCTA', 'tagTVINSERTSTRUCTA');
  TSepiTypeAlias.Create(Result, '_TV_INSERTSTRUCTW', 'tagTVINSERTSTRUCTW');
  TSepiTypeAlias.Create(Result, '_TV_INSERTSTRUCT', '_TV_INSERTSTRUCTA');
  TSepiTypeAlias.Create(Result, 'TTVInsertStructA', 'tagTVINSERTSTRUCTA');
  TSepiTypeAlias.Create(Result, 'TTVInsertStructW', 'tagTVINSERTSTRUCTW');
  TSepiTypeAlias.Create(Result, 'TTVInsertStruct', 'TTVInsertStructA');
  TSepiTypeAlias.Create(Result, 'TV_INSERTSTRUCTA', 'tagTVINSERTSTRUCTA');
  TSepiTypeAlias.Create(Result, 'TV_INSERTSTRUCTW', 'tagTVINSERTSTRUCTW');
  TSepiTypeAlias.Create(Result, 'TV_INSERTSTRUCT', 'TV_INSERTSTRUCTA');

  // Types
  TSepiPointerType.Create(Result, 'PTVHitTestInfo', 'TTVHitTestInfo', True);
  SepiImporttagTVHITTESTINFO(Result);
  TSepiTypeAlias.Create(Result, '_TV_HITTESTINFO', 'tagTVHITTESTINFO');
  TSepiTypeAlias.Create(Result, 'TTVHitTestInfo', 'tagTVHITTESTINFO');
  TSepiTypeAlias.Create(Result, 'TV_HITTESTINFO', 'tagTVHITTESTINFO');

  // Types
  TSepiMethodRefType.Create(Result, 'PFNTVCOMPARE',
    'function(lParam1, lParam2, lParamSort: Longint): Integer',
    False, ccStdCall);
  TSepiTypeAlias.Create(Result, 'TTVCompare', 'PFNTVCOMPARE');

  // Types
  SepiImporttagTVSORTCB(Result);
  TSepiTypeAlias.Create(Result, '_TV_SORTCB', 'tagTVSORTCB');
  TSepiTypeAlias.Create(Result, 'TTVSortCB', 'tagTVSORTCB');
  TSepiTypeAlias.Create(Result, 'TV_SORTCB', 'tagTVSORTCB');

  // Types
  TSepiPointerType.Create(Result, 'PNMTreeViewA', 'TNMTreeViewA', True);
  TSepiPointerType.Create(Result, 'PNMTreeViewW', 'TNMTreeViewW', True);
  TSepiTypeAlias.Create(Result, 'PNMTreeView', 'PNMTreeViewA');
  SepiImporttagNMTREEVIEWA(Result);
  SepiImporttagNMTREEVIEWW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMTREEVIEW', 'tagNMTREEVIEWA');
  TSepiTypeAlias.Create(Result, '_NM_TREEVIEWA', 'tagNMTREEVIEWA');
  TSepiTypeAlias.Create(Result, '_NM_TREEVIEWW', 'tagNMTREEVIEWW');
  TSepiTypeAlias.Create(Result, '_NM_TREEVIEW', '_NM_TREEVIEWA');
  TSepiTypeAlias.Create(Result, 'TNMTreeViewA', 'tagNMTREEVIEWA');
  TSepiTypeAlias.Create(Result, 'TNMTreeViewW', 'tagNMTREEVIEWW');
  TSepiTypeAlias.Create(Result, 'TNMTreeView', 'TNMTreeViewA');
  TSepiTypeAlias.Create(Result, 'NM_TREEVIEWA', 'tagNMTREEVIEWA');
  TSepiTypeAlias.Create(Result, 'NM_TREEVIEWW', 'tagNMTREEVIEWW');
  TSepiTypeAlias.Create(Result, 'NM_TREEVIEW', 'NM_TREEVIEWA');

  // Types
  TSepiPointerType.Create(Result, 'PTVDispInfoA', 'TTVDispInfoA', True);
  TSepiPointerType.Create(Result, 'PTVDispInfoW', 'TTVDispInfoW', True);
  TSepiTypeAlias.Create(Result, 'PTVDispInfo', 'PTVDispInfoA');
  SepiImporttagTVDISPINFOA(Result);
  SepiImporttagTVDISPINFOW(Result);
  TSepiTypeAlias.Create(Result, 'tagTVDISPINFO', 'tagTVDISPINFOA');
  TSepiTypeAlias.Create(Result, '_TV_DISPINFOA', 'tagTVDISPINFOA');
  TSepiTypeAlias.Create(Result, '_TV_DISPINFOW', 'tagTVDISPINFOW');
  TSepiTypeAlias.Create(Result, '_TV_DISPINFO', '_TV_DISPINFOA');
  TSepiTypeAlias.Create(Result, 'TTVDispInfoA', 'tagTVDISPINFOA');
  TSepiTypeAlias.Create(Result, 'TTVDispInfoW', 'tagTVDISPINFOW');
  TSepiTypeAlias.Create(Result, 'TTVDispInfo', 'TTVDispInfoA');
  TSepiTypeAlias.Create(Result, 'TV_DISPINFOA', 'tagTVDISPINFOA');
  TSepiTypeAlias.Create(Result, 'TV_DISPINFOW', 'tagTVDISPINFOW');
  TSepiTypeAlias.Create(Result, 'TV_DISPINFO', 'TV_DISPINFOA');

  // Types
  SepiImporttagTVKEYDOWN(Result);
  TSepiTypeAlias.Create(Result, '_TV_KEYDOWN', 'tagTVKEYDOWN');
  TSepiTypeAlias.Create(Result, 'TTVKeyDown', 'tagTVKEYDOWN');
  TSepiTypeAlias.Create(Result, 'TV_KEYDOWN', 'tagTVKEYDOWN');
  SepiImporttagNMTVCUSTOMDRAW(Result);
  TSepiPointerType.Create(Result, 'PNMTVCustomDraw', 'TNMTVCustomDraw', True);
  TSepiTypeAlias.Create(Result, 'TNMTVCustomDraw', 'tagNMTVCUSTOMDRAW');
  SepiImporttagNMTVGETINFOTIPA(Result);
  SepiImporttagNMTVGETINFOTIPW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMTVGETINFOTIP', 'tagNMTVGETINFOTIPA');
  TSepiPointerType.Create(Result, 'PNMTVGetInfoTipA',
    'TNMTVGetInfoTipA', True);
  TSepiPointerType.Create(Result, 'PNMTVGetInfoTipW',
    'TNMTVGetInfoTipW', True);
  TSepiTypeAlias.Create(Result, 'PNMTVGetInfoTip', 'PNMTVGetInfoTipA');
  TSepiTypeAlias.Create(Result, 'TNMTVGetInfoTipA', 'tagNMTVGETINFOTIPA');
  TSepiTypeAlias.Create(Result, 'TNMTVGetInfoTipW', 'tagNMTVGETINFOTIPW');
  TSepiTypeAlias.Create(Result, 'TNMTVGetInfoTip', 'TNMTVGetInfoTipA');

  // Types
  SepiImporttagCOMBOBOXEXITEMA(Result);
  SepiImporttagCOMBOBOXEXITEMW(Result);
  TSepiTypeAlias.Create(Result, 'tagCOMBOBOXEXITEM', 'tagCOMBOBOXEXITEMA');
  TSepiPointerType.Create(Result, 'PComboBoxExItemA',
    'TComboBoxExItemA', True);
  TSepiPointerType.Create(Result, 'PComboBoxExItemW',
    'TComboBoxExItemW', True);
  TSepiTypeAlias.Create(Result, 'PComboBoxExItem', 'PComboBoxExItemA');
  TSepiTypeAlias.Create(Result, 'TComboBoxExItemA', 'tagCOMBOBOXEXITEMA');
  TSepiTypeAlias.Create(Result, 'TComboBoxExItemW', 'tagCOMBOBOXEXITEMW');
  TSepiTypeAlias.Create(Result, 'TComboBoxExItem', 'TComboBoxExItemA');

  // Types
  SepiImportNMCOMBOBOXEXA(Result);
  SepiImportNMCOMBOBOXEXW(Result);
  TSepiTypeAlias.Create(Result, 'NMCOMBOBOXEX', 'NMCOMBOBOXEXA');
  TSepiPointerType.Create(Result, 'PNMComboBoxExA', 'TNMComboBoxExA', True);
  TSepiPointerType.Create(Result, 'PNMComboBoxExW', 'TNMComboBoxExW', True);
  TSepiTypeAlias.Create(Result, 'PNMComboBoxEx', 'PNMComboBoxExA');
  TSepiTypeAlias.Create(Result, 'TNMComboBoxExA', 'NMCOMBOBOXEXA');
  TSepiTypeAlias.Create(Result, 'TNMComboBoxExW', 'NMCOMBOBOXEXW');
  TSepiTypeAlias.Create(Result, 'TNMComboBoxEx', 'TNMComboBoxExA');

  // Types
  TSepiArrayType.Create(Result, '$5',
    [0, CBEMAXSTRLEN - 1], 'AnsiChar', True);
  SepiImportNMCBEDRAGBEGINA(Result);
  TSepiArrayType.Create(Result, '$6',
    [0, CBEMAXSTRLEN - 1], 'WideChar', True);
  SepiImportNMCBEDRAGBEGINW(Result);
  TSepiTypeAlias.Create(Result, 'NMCBEDRAGBEGIN', 'NMCBEDRAGBEGINA');
  TSepiPointerType.Create(Result, 'PNMCBEDragBeginA',
    'TNMCBEDragBeginA', True);
  TSepiPointerType.Create(Result, 'PNMCBEDragBeginW',
    'TNMCBEDragBeginW', True);
  TSepiTypeAlias.Create(Result, 'PNMCBEDragBegin', 'PNMCBEDragBeginA');
  TSepiTypeAlias.Create(Result, 'TNMCBEDragBeginA', 'NMCBEDRAGBEGINA');
  TSepiTypeAlias.Create(Result, 'TNMCBEDragBeginW', 'NMCBEDRAGBEGINW');
  TSepiTypeAlias.Create(Result, 'TNMCBEDragBegin', 'TNMCBEDragBeginA');
  TSepiArrayType.Create(Result, '$7',
    [0, CBEMAXSTRLEN - 1], 'AnsiChar', True);
  SepiImportNMCBEENDEDITA(Result);
  TSepiArrayType.Create(Result, '$8',
    [0, CBEMAXSTRLEN - 1], 'WideChar', True);
  SepiImportNMCBEENDEDITW(Result);
  TSepiTypeAlias.Create(Result, 'NMCBEENDEDIT', 'NMCBEENDEDITA');
  TSepiPointerType.Create(Result, 'PNMCBEEndEditA', 'TNMCBEEndEditA', True);
  TSepiPointerType.Create(Result, 'PNMCBEEndEditW', 'TNMCBEEndEditW', True);
  TSepiTypeAlias.Create(Result, 'PNMCBEEndEdit', 'PNMCBEEndEditA');
  TSepiTypeAlias.Create(Result, 'TNMCBEEndEditA', 'NMCBEENDEDITA');
  TSepiTypeAlias.Create(Result, 'TNMCBEEndEditW', 'NMCBEENDEDITW');
  TSepiTypeAlias.Create(Result, 'TNMCBEEndEdit', 'TNMCBEEndEditA');

  // Types
  TSepiPointerType.Create(Result, 'PTCItemHeaderA', 'TTCItemHeaderA', True);
  TSepiPointerType.Create(Result, 'PTCItemHeaderW', 'TTCItemHeaderW', True);
  TSepiTypeAlias.Create(Result, 'PTCItemHeader', 'PTCItemHeaderA');
  SepiImporttagTCITEMHEADERA(Result);
  SepiImporttagTCITEMHEADERW(Result);
  TSepiTypeAlias.Create(Result, 'tagTCITEMHEADER', 'tagTCITEMHEADERA');
  TSepiTypeAlias.Create(Result, '_TC_ITEMHEADERA', 'tagTCITEMHEADERA');
  TSepiTypeAlias.Create(Result, '_TC_ITEMHEADERW', 'tagTCITEMHEADERW');
  TSepiTypeAlias.Create(Result, '_TC_ITEMHEADER', '_TC_ITEMHEADERA');
  TSepiTypeAlias.Create(Result, 'TTCItemHeaderA', 'tagTCITEMHEADERA');
  TSepiTypeAlias.Create(Result, 'TTCItemHeaderW', 'tagTCITEMHEADERW');
  TSepiTypeAlias.Create(Result, 'TTCItemHeader', 'TTCItemHeaderA');
  TSepiTypeAlias.Create(Result, 'TC_ITEMHEADERA', 'tagTCITEMHEADERA');
  TSepiTypeAlias.Create(Result, 'TC_ITEMHEADERW', 'tagTCITEMHEADERW');
  TSepiTypeAlias.Create(Result, 'TC_ITEMHEADER', 'TC_ITEMHEADERA');
  TSepiPointerType.Create(Result, 'PTCItemA', 'TTCItemA', True);
  TSepiPointerType.Create(Result, 'PTCItemW', 'TTCItemW', True);
  TSepiTypeAlias.Create(Result, 'PTCItem', 'PTCItemA');
  SepiImporttagTCITEMA(Result);
  SepiImporttagTCITEMW(Result);
  TSepiTypeAlias.Create(Result, 'tagTCITEM', 'tagTCITEMA');
  TSepiTypeAlias.Create(Result, '_TC_ITEMA', 'tagTCITEMA');
  TSepiTypeAlias.Create(Result, '_TC_ITEMW', 'tagTCITEMW');
  TSepiTypeAlias.Create(Result, '_TC_ITEM', '_TC_ITEMA');
  TSepiTypeAlias.Create(Result, 'TTCItemA', 'tagTCITEMA');
  TSepiTypeAlias.Create(Result, 'TTCItemW', 'tagTCITEMW');
  TSepiTypeAlias.Create(Result, 'TTCItem', 'TTCItemA');
  TSepiTypeAlias.Create(Result, 'TC_ITEMA', 'tagTCITEMA');
  TSepiTypeAlias.Create(Result, 'TC_ITEMW', 'tagTCITEMW');
  TSepiTypeAlias.Create(Result, 'TC_ITEM', 'TC_ITEMA');

  // Types
  TSepiPointerType.Create(Result, 'PTCHitTestInfo', 'TTCHitTestInfo', True);
  SepiImporttagTCHITTESTINFO(Result);
  TSepiTypeAlias.Create(Result, '_TC_HITTESTINFO', 'tagTCHITTESTINFO');
  TSepiTypeAlias.Create(Result, 'TTCHitTestInfo', 'tagTCHITTESTINFO');
  TSepiTypeAlias.Create(Result, 'TC_HITTESTINFO', 'tagTCHITTESTINFO');
  SepiImporttagTCKEYDOWN(Result);
  TSepiTypeAlias.Create(Result, '_TC_KEYDOWN', 'tagTCKEYDOWN');
  TSepiTypeAlias.Create(Result, 'TTCKeyDown', 'tagTCKEYDOWN');
  TSepiTypeAlias.Create(Result, 'TC_KEYDOWN', 'tagTCKEYDOWN');

  // Types
  TSepiTypeAlias.Create(Result, 'MONTHDAYSTATE', 'DWORD');
  TSepiPointerType.Create(Result, 'PMonthDayState', 'TMonthDayState', True);
  TSepiTypeAlias.Create(Result, 'TMonthDayState', 'MONTHDAYSTATE');
  SepiImportMCHITTESTINFO(Result);
  TSepiPointerType.Create(Result, 'PMCHitTestInfo', 'TMCHitTestInfo', True);
  TSepiTypeAlias.Create(Result, 'TMCHitTestInfo', 'MCHITTESTINFO');
  SepiImporttagNMSELCHANGE(Result);
  TSepiPointerType.Create(Result, 'PNMSelChange', 'TNMSelChange', True);
  TSepiTypeAlias.Create(Result, 'TNMSelChange', 'tagNMSELCHANGE');
  SepiImporttagNMDAYSTATE(Result);
  TSepiPointerType.Create(Result, 'PNMDayState', 'TNMDayState', True);
  TSepiTypeAlias.Create(Result, 'TNMDayState', 'tagNMDAYSTATE');
  TSepiTypeAlias.Create(Result, 'NMSELECT', 'tagNMSELCHANGE');
  TSepiPointerType.Create(Result, 'PNMSelect', 'TNMSelect', True);
  TSepiTypeAlias.Create(Result, 'TNMSelect', 'NMSELECT');

  // Types
  SepiImporttagNMDATETIMECHANGE(Result);
  TSepiPointerType.Create(Result, 'PNMDateTimeChange',
    'TNMDateTimeChange', True);
  TSepiTypeAlias.Create(Result, 'TNMDateTimeChange', 'tagNMDATETIMECHANGE');
  SepiImporttagNMDATETIMESTRINGA(Result);
  SepiImporttagNMDATETIMESTRINGW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMDATETIMESTRING', 'tagNMDATETIMESTRINGA');
  TSepiPointerType.Create(Result, 'PNMDateTimeStringA',
    'TNMDateTimeStringA', True);
  TSepiPointerType.Create(Result, 'PNMDateTimeStringW',
    'TNMDateTimeStringW', True);
  TSepiTypeAlias.Create(Result, 'PNMDateTimeString', 'PNMDateTimeStringA');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeStringA', 'tagNMDATETIMESTRINGA');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeStringW', 'tagNMDATETIMESTRINGW');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeString', 'TNMDateTimeStringA');
  SepiImporttagNMDATETIMEWMKEYDOWNA(Result);
  SepiImporttagNMDATETIMEWMKEYDOWNW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMDATETIMEWMKEYDOWN',
    'tagNMDATETIMEWMKEYDOWNA');
  TSepiPointerType.Create(Result, 'PNMDateTimeWMKeyDownA',
    'TNMDateTimeWMKeyDownA', True);
  TSepiPointerType.Create(Result, 'PNMDateTimeWMKeyDownW',
    'TNMDateTimeWMKeyDownW', True);
  TSepiTypeAlias.Create(Result, 'PNMDateTimeWMKeyDown',
    'PNMDateTimeWMKeyDownA');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeWMKeyDownA',
    'tagNMDATETIMEWMKEYDOWNA');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeWMKeyDownW',
    'tagNMDATETIMEWMKEYDOWNW');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeWMKeyDown',
    'TNMDateTimeWMKeyDownA');
  TSepiArrayType.Create(Result, '$9',
    [0, 63], 'AnsiChar', True);
  SepiImporttagNMDATETIMEFORMATA(Result);
  TSepiArrayType.Create(Result, '$10',
    [0, 63], 'WideChar', True);
  SepiImporttagNMDATETIMEFORMATW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMDATETIMEFORMAT', 'tagNMDATETIMEFORMATA');
  TSepiPointerType.Create(Result, 'PNMDateTimeFormatA',
    'TNMDateTimeFormatA', True);
  TSepiPointerType.Create(Result, 'PNMDateTimeFormatW',
    'TNMDateTimeFormatW', True);
  TSepiTypeAlias.Create(Result, 'PNMDateTimeFormat', 'PNMDateTimeFormatA');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeFormatA', 'tagNMDATETIMEFORMATA');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeFormatW', 'tagNMDATETIMEFORMATW');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeFormat', 'TNMDateTimeFormatA');
  SepiImporttagNMDATETIMEFORMATQUERYA(Result);
  SepiImporttagNMDATETIMEFORMATQUERYW(Result);
  TSepiTypeAlias.Create(Result, 'tagNMDATETIMEFORMATQUERY',
    'tagNMDATETIMEFORMATQUERYA');
  TSepiPointerType.Create(Result, 'PNMDateTimeFormatQueryA',
    'TNMDateTimeFormatQueryA', True);
  TSepiPointerType.Create(Result, 'PNMDateTimeFormatQueryW',
    'TNMDateTimeFormatQueryW', True);
  TSepiTypeAlias.Create(Result, 'PNMDateTimeFormatQuery',
    'PNMDateTimeFormatQueryA');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeFormatQueryA',
    'tagNMDATETIMEFORMATQUERYA');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeFormatQueryW',
    'tagNMDATETIMEFORMATQUERYW');
  TSepiTypeAlias.Create(Result, 'TNMDateTimeFormatQuery',
    'TNMDateTimeFormatQueryA');

  // Types
  SepiImporttagNMIPADDRESS(Result);
  TSepiPointerType.Create(Result, 'PNMIPAddress', 'TNMIPAddress', True);
  TSepiTypeAlias.Create(Result, 'TNMIPAddress', 'tagNMIPADDRESS');

  // Types
  SepiImportNMPGSCROLL(Result);
  TSepiPointerType.Create(Result, 'PNMPGScroll', 'TNMPGScroll', True);
  TSepiTypeAlias.Create(Result, 'TNMPGScroll', 'NMPGSCROLL');

  // Types
  SepiImportNMPGCALCSIZE(Result);
  TSepiPointerType.Create(Result, 'PNMPGCalcSize', 'TNMPGCalcSize', True);
  TSepiTypeAlias.Create(Result, 'TNMPGCalcSize', 'NMPGCALCSIZE');

  // Types
  SepiImporttagTRACKMOUSEEVENT(Result);
  TSepiPointerType.Create(Result, 'PTrackMouseEvent',
    'TTrackMouseEvent', True);
  TSepiTypeAlias.Create(Result, 'TTrackMouseEvent', 'tagTRACKMOUSEEVENT');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('CommCtrlTypes', ImportUnit);
end.

