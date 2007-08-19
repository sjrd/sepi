{*
  Importe l'unité ShlObj dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsShlObjTypes;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Windows, ActiveX, WinInet, ShlObj;

implementation

{ You must not localize any of the strings this unit contains! }

{------------------}
{ _SHITEMID import }
{------------------}

function SepiImport_SHITEMID(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHITEMID', False, True);

  with Result do
  begin
    AddField('cb', System.TypeInfo(Word));
    AddField('abID', '$1');

    Complete;
  end;
end;

{--------------------}
{ _ITEMIDLIST import }
{--------------------}

function SepiImport_ITEMIDLIST(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_ITEMIDLIST', False, True);

  with Result do
  begin
    AddField('mkid', 'TSHItemID');

    Complete;
  end;
end;

{-----------------------------}
{ _CMINVOKECOMMANDINFO import }
{-----------------------------}

function SepiImport_CMINVOKECOMMANDINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_CMINVOKECOMMANDINFO', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('fMask', System.TypeInfo(DWORD));
    AddField('hwnd', System.TypeInfo(HWND));
    AddField('lpVerb', 'LPCSTR');
    AddField('lpParameters', 'LPCSTR');
    AddField('lpDirectory', 'LPCSTR');
    AddField('nShow', System.TypeInfo(Integer));
    AddField('dwHotKey', System.TypeInfo(DWORD));
    AddField('hIcon', System.TypeInfo(THandle));

    Complete;
  end;
end;

{-------------------------------}
{ _CMInvokeCommandInfoEx import }
{-------------------------------}

function SepiImport_CMInvokeCommandInfoEx(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_CMInvokeCommandInfoEx', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('fMask', System.TypeInfo(DWORD));
    AddField('hwnd', System.TypeInfo(HWND));
    AddField('lpVerb', 'LPCSTR');
    AddField('lpParameters', 'LPCSTR');
    AddField('lpDirectory', 'LPCSTR');
    AddField('nShow', System.TypeInfo(Integer));
    AddField('dwHotKey', System.TypeInfo(DWORD));
    AddField('hIcon', System.TypeInfo(THandle));
    AddField('lpTitle', 'LPCSTR');
    AddField('lpVerbW', 'LPCWSTR');
    AddField('lpParametersW', 'LPCWSTR');
    AddField('lpDirectoryW', 'LPCWSTR');
    AddField('lpTitleW', 'LPCWSTR');
    AddField('ptInvoke', 'TPoint');

    Complete;
  end;
end;

{---------------------}
{ IContextMenu import }
{---------------------}

function SepiImportIContextMenu(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IContextMenu));

  with Result do
  begin
    AddMethod('QueryContextMenu',
      'function(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast, uFlags: UINT ) : HResult', ccStdCall);
    AddMethod('InvokeCommand',
      'function(var lpici: TCMInvokeCommandInfo): HResult', ccStdCall);
    AddMethod('GetCommandString',
      'function(idCmd, uType: UINT; pwReserved: PUINT; pszName: LPSTR ; cchMax: UINT ) : HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------}
{ IContextMenu2 import }
{----------------------}

function SepiImportIContextMenu2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IContextMenu2));

  with Result do
  begin
    AddMethod('HandleMenuMsg',
      'function(uMsg: UINT; WParam, LParam: Integer): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------}
{ IContextMenu3 import }
{----------------------}

function SepiImportIContextMenu3(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IContextMenu3));

  with Result do
  begin
    AddMethod('HandleMenuMsg2',
      'function(uMsg: UINT; wParam, lParam: Integer; var lpResult: Integer ) : HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------}
{ IShellExtInit import }
{----------------------}

function SepiImportIShellExtInit(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellExtInit));

  with Result do
  begin
    AddMethod('Initialize',
      'function(pidlFolder: PItemIDList; lpdobj: IDataObject; hKeyProgID: HKEY ) : HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------------}
{ IShellPropSheetExt import }
{---------------------------}

function SepiImportIShellPropSheetExt(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellPropSheetExt));

  with Result do
  begin
    AddMethod('AddPages',
      'function(lpfnAddPage: TFNAddPropSheetPage; lParam: LPARAM): HResult', ccStdCall);
    AddMethod('ReplacePage',
      'function(uPageID: UINT; lpfnReplaceWith: TFNAddPropSheetPage; lParam: LPARAM ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ IPersistFolder import }
{-----------------------}

function SepiImportIPersistFolder(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPersistFolder));

  with Result do
  begin
    AddMethod('Initialize',
      'function(pidl: PItemIDList): HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IPersistFolder2 import }
{------------------------}

function SepiImportIPersistFolder2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IPersistFolder2));

  with Result do
  begin
    AddMethod('GetCurFolder',
      'function(var pidl: PItemIDList): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------}
{ IExtractIconA import }
{----------------------}

function SepiImportIExtractIconA(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IExtractIconA));

  with Result do
  begin
    AddMethod('GetIconLocation',
      'function(uFlags: UINT; szIconFile: PAnsiChar; cchMax: UINT; out piIndex: Integer ; out pwFlags: UINT ) : HResult', ccStdCall);
    AddMethod('Extract',
      'function(pszFile: PAnsiChar; nIconIndex: UINT; out phiconLarge, phiconSmall: HICON ; nIconSize: UINT ) : HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------}
{ IExtractIconW import }
{----------------------}

function SepiImportIExtractIconW(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IExtractIconW));

  with Result do
  begin
    AddMethod('GetIconLocation',
      'function(uFlags: UINT; szIconFile: PWideChar; cchMax: UINT; out piIndex: Integer ; out pwFlags: UINT ) : HResult', ccStdCall);
    AddMethod('Extract',
      'function(pszFile: PWideChar; nIconIndex: UINT; out phiconLarge, phiconSmall: HICON ; nIconSize: UINT ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ IShellIcon import }
{-------------------}

function SepiImportIShellIcon(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellIcon));

  with Result do
  begin
    AddMethod('GetIconOf',
      'function(pidl: PItemIDList; flags: UINT; out IconIndex: Integer ) : HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------------------}
{ IShellIconOverlayIdentifier import }
{------------------------------------}

function SepiImportIShellIconOverlayIdentifier(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellIconOverlayIdentifier));

  with Result do
  begin
    AddMethod('IsMemberOf',
      'function(pwszPath: PWideChar; dwAttrib: DWORD): HResult', ccStdCall);
    AddMethod('GetOverlayInfo',
      'function(pwszIconFile: PWideChar; cchMax: Integer; var pIndex: Integer ; var pdwFlags: DWORD ) : HResult', ccStdCall);
    AddMethod('GetPriority',
      'function(out pIPriority: Integer): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IShellIconOverlay import }
{--------------------------}

function SepiImportIShellIconOverlay(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellIconOverlay));

  with Result do
  begin
    AddMethod('GetOverlayIndex',
      'function(pidl: PItemIDList; out pIndex: Integer): HResult', ccStdCall);
    AddMethod('GetOverlayIconIndex',
      'function(pidl: PItemIDList; out pIconIndex: Integer): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ IShellLinkA import }
{--------------------}

function SepiImportIShellLinkA(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellLinkA));

  with Result do
  begin
    AddMethod('GetPath',
      'function(pszFile: PAnsiChar; cchMaxPath: Integer; var pfd: TWin32FindData ; fFlags: DWORD ) : HResult', ccStdCall);
    AddMethod('GetIDList',
      'function(var ppidl: PItemIDList): HResult', ccStdCall);
    AddMethod('SetIDList',
      'function(pidl: PItemIDList): HResult', ccStdCall);
    AddMethod('GetDescription',
      'function(pszName: PAnsiChar; cchMaxName: Integer): HResult', ccStdCall);
    AddMethod('SetDescription',
      'function(pszName: PAnsiChar): HResult', ccStdCall);
    AddMethod('GetWorkingDirectory',
      'function(pszDir: PAnsiChar; cchMaxPath: Integer): HResult', ccStdCall);
    AddMethod('SetWorkingDirectory',
      'function(pszDir: PAnsiChar): HResult', ccStdCall);
    AddMethod('GetArguments',
      'function(pszArgs: PAnsiChar; cchMaxPath: Integer): HResult', ccStdCall);
    AddMethod('SetArguments',
      'function(pszArgs: PAnsiChar): HResult', ccStdCall);
    AddMethod('GetHotkey',
      'function(var pwHotkey: Word): HResult', ccStdCall);
    AddMethod('SetHotkey',
      'function(wHotkey: Word): HResult', ccStdCall);
    AddMethod('GetShowCmd',
      'function(out piShowCmd: Integer): HResult', ccStdCall);
    AddMethod('SetShowCmd',
      'function(iShowCmd: Integer): HResult', ccStdCall);
    AddMethod('GetIconLocation',
      'function(pszIconPath: PAnsiChar; cchIconPath: Integer; out piIcon: Integer ) : HResult', ccStdCall);
    AddMethod('SetIconLocation',
      'function(pszIconPath: PAnsiChar; iIcon: Integer): HResult', ccStdCall);
    AddMethod('SetRelativePath',
      'function(pszPathRel: PAnsiChar; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('Resolve',
      'function(Wnd: HWND; fFlags: DWORD): HResult', ccStdCall);
    AddMethod('SetPath',
      'function(pszFile: PAnsiChar): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ IShellLinkW import }
{--------------------}

function SepiImportIShellLinkW(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellLinkW));

  with Result do
  begin
    AddMethod('GetPath',
      'function(pszFile: PWideChar; cchMaxPath: Integer; var pfd: TWin32FindData ; fFlags: DWORD ) : HResult', ccStdCall);
    AddMethod('GetIDList',
      'function(var ppidl: PItemIDList): HResult', ccStdCall);
    AddMethod('SetIDList',
      'function(pidl: PItemIDList): HResult', ccStdCall);
    AddMethod('GetDescription',
      'function(pszName: PWideChar; cchMaxName: Integer): HResult', ccStdCall);
    AddMethod('SetDescription',
      'function(pszName: PWideChar): HResult', ccStdCall);
    AddMethod('GetWorkingDirectory',
      'function(pszDir: PWideChar; cchMaxPath: Integer): HResult', ccStdCall);
    AddMethod('SetWorkingDirectory',
      'function(pszDir: PWideChar): HResult', ccStdCall);
    AddMethod('GetArguments',
      'function(pszArgs: PWideChar; cchMaxPath: Integer): HResult', ccStdCall);
    AddMethod('SetArguments',
      'function(pszArgs: PWideChar): HResult', ccStdCall);
    AddMethod('GetHotkey',
      'function(var pwHotkey: Word): HResult', ccStdCall);
    AddMethod('SetHotkey',
      'function(wHotkey: Word): HResult', ccStdCall);
    AddMethod('GetShowCmd',
      'function(out piShowCmd: Integer): HResult', ccStdCall);
    AddMethod('SetShowCmd',
      'function(iShowCmd: Integer): HResult', ccStdCall);
    AddMethod('GetIconLocation',
      'function(pszIconPath: PWideChar; cchIconPath: Integer; out piIcon: Integer ) : HResult', ccStdCall);
    AddMethod('SetIconLocation',
      'function(pszIconPath: PWideChar; iIcon: Integer): HResult', ccStdCall);
    AddMethod('SetRelativePath',
      'function(pszPathRel: PWideChar; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('Resolve',
      'function(Wnd: HWND; fFlags: DWORD): HResult', ccStdCall);
    AddMethod('SetPath',
      'function(pszFile: PWideChar): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------------}
{ IShellExecuteHookA import }
{---------------------------}

function SepiImportIShellExecuteHookA(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellExecuteHookA));

  with Result do
  begin
    AddMethod('Execute',
      'function(var ShellExecuteInfo: TShellExecuteInfo): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------------}
{ IShellExecuteHookW import }
{---------------------------}

function SepiImportIShellExecuteHookW(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellExecuteHookW));

  with Result do
  begin
    AddMethod('Execute',
      'function(var ShellExecuteInfo: TShellExecuteInfo): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ IURLSearchHook import }
{-----------------------}

function SepiImportIURLSearchHook(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IURLSearchHook));

  with Result do
  begin
    AddMethod('Translate',
      'function(lpwszSearchURL: PWideChar; cchBufferSize: DWORD): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ INewShortcutHookA import }
{--------------------------}

function SepiImportINewShortcutHookA(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(INewShortcutHookA));

  with Result do
  begin
    AddMethod('SetReferent',
      'function(pcszReferent: PAnsiChar; Wnd: HWND): HResult', ccStdCall);
    AddMethod('GetReferent',
      'function(pcszReferent: PAnsiChar; cchReferent: Integer): HResult', ccStdCall);
    AddMethod('SetFolder',
      'function(pcszFolder: PAnsiChar; Wnd: HWND): HResult', ccStdCall);
    AddMethod('GetFolder',
      'function(pcszFolder: PAnsiChar; cchFolder: Integer): HResult', ccStdCall);
    AddMethod('GetName',
      'function(pcszName: PAnsiChar; cchName: Integer): HResult', ccStdCall);
    AddMethod('GetExtension',
      'function(pcszExtension: PAnsiChar; cchExtension: Integer): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ INewShortcutHookW import }
{--------------------------}

function SepiImportINewShortcutHookW(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(INewShortcutHookW));

  with Result do
  begin
    AddMethod('SetReferent',
      'function(pcszReferent: PWideChar; Wnd: HWND): HResult', ccStdCall);
    AddMethod('GetReferent',
      'function(pcszReferent: PWideChar; cchReferent: Integer): HResult', ccStdCall);
    AddMethod('SetFolder',
      'function(pcszFolder: PWideChar; Wnd: HWND): HResult', ccStdCall);
    AddMethod('GetFolder',
      'function(pcszFolder: PWideChar; cchFolder: Integer): HResult', ccStdCall);
    AddMethod('GetName',
      'function(pcszName: PWideChar; cchName: Integer): HResult', ccStdCall);
    AddMethod('GetExtension',
      'function(pcszExtension: PWideChar; cchExtension: Integer): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ ICopyHookA import }
{-------------------}

function SepiImportICopyHookA(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ICopyHookA));

  with Result do
  begin
    AddMethod('CopyCallback',
      'function(Wnd: HWND; wFunc, wFlags: UINT; pszSrcFile: PAnsiChar; dwSrcAttribs: DWORD ; pszDestFile: PAnsiChar ; dwDestAttribs: DWORD ) : UINT', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ ICopyHookW import }
{-------------------}

function SepiImportICopyHookW(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ICopyHookW));

  with Result do
  begin
    AddMethod('CopyCallback',
      'function(Wnd: HWND; wFunc, wFlags: UINT; pszSrcFile: PWideChar; dwSrcAttribs: DWORD ; pszDestFile: PWideChar ; dwDestAttribs: DWORD ) : UINT', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ IFileViewerSite import }
{------------------------}

function SepiImportIFileViewerSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IFileViewerSite));

  with Result do
  begin
    AddMethod('SetPinnedWindow',
      'function(Wnd: HWND): HResult', ccStdCall);
    AddMethod('GetPinnedWindow',
      'function(var Wnd: HWND): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ FVSHOWINFO import }
{-------------------}

function SepiImportFVSHOWINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'FVSHOWINFO', True, True,
    TypeInfo(FVSHOWINFO));

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('hwndOwner', System.TypeInfo(HWND));
    AddField('iShow', System.TypeInfo(Integer));
    AddField('dwFlags', System.TypeInfo(DWORD));
    AddField('rect', 'TRECT');
    AddField('punkRel', System.TypeInfo(IUNKNOWN));
    AddField('strNewFile', '$2');

    Complete;
  end;
end;

{---------------------}
{ IFileViewerA import }
{---------------------}

function SepiImportIFileViewerA(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IFileViewerA));

  with Result do
  begin
    AddMethod('ShowInitialize',
      'function(fsi: IFileViewerSite): HResult', ccStdCall);
    AddMethod('Show',
      'function(var pvsi: TFVShowInfo): HResult', ccStdCall);
    AddMethod('PrintTo',
      'function(pszDriver: PAnsiChar; fSuppressUI: BOOL): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IFileViewerW import }
{---------------------}

function SepiImportIFileViewerW(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IFileViewerW));

  with Result do
  begin
    AddMethod('ShowInitialize',
      'function(fsi: IFileViewerSite): HResult', ccStdCall);
    AddMethod('Show',
      'function(var pvsi: TFVShowInfo): HResult', ccStdCall);
    AddMethod('PrintTo',
      'function(pszDriver: PWideChar; fSuppressUI: BOOL): HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ FOLDERSETTINGS import }
{-----------------------}

function SepiImportFOLDERSETTINGS(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'FOLDERSETTINGS', True, True);

  with Result do
  begin
    AddField('ViewMode', System.TypeInfo(UINT));
    AddField('fFlags', System.TypeInfo(UINT));

    Complete;
  end;
end;

{----------------------}
{ IShellBrowser import }
{----------------------}

function SepiImportIShellBrowser(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellBrowser));

  with Result do
  begin
    AddMethod('InsertMenusSB',
      'function(hMenuShared: HMENU; out MenuWidths: TOleMenuGroupWidths ) : HResult', ccStdCall);
    AddMethod('SetMenuSB',
      'function(hMenuShared: HMENU; hOleMenuReserved: HOLEMENU ; hwndActiveObject: HWND ) : HResult', ccStdCall);
    AddMethod('RemoveMenusSB',
      'function(hMenuShared: HMENU): HResult', ccStdCall);
    AddMethod('SetStatusTextSB',
      'function(StatusText: POleStr): HResult', ccStdCall);
    AddMethod('EnableModelessSB',
      'function(Enable: BOOL): HResult', ccStdCall);
    AddMethod('TranslateAcceleratorSB',
      'function(Msg: PMsg; ID: Word): HResult', ccStdCall);
    AddMethod('BrowseObject',
      'function(pidl: PItemIDList; flags: UINT): HResult', ccStdCall);
    AddMethod('GetViewStateStream',
      'function(Mode: DWORD; out Stream: IStream): HResult', ccStdCall);
    AddMethod('GetControlWindow',
      'function(ID: UINT; out Wnd: HWND): HResult', ccStdCall);
    AddMethod('SendControlMsg',
      'function(ID, Msg: UINT; wParm: WPARAM; lParm: LPARAM; var Rslt: LResult ) : HResult', ccStdCall);
    AddMethod('QueryActiveShellView',
      'function(var ShellView: IShellView): HResult', ccStdCall);
    AddMethod('OnViewWindowActive',
      'function(var ShellView: IShellView): HResult', ccStdCall);
    AddMethod('SetToolbarItems',
      'function(TBButton: PTBButton; nButtons, uFlags: UINT ) : HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ ICommDlgBrowser import }
{------------------------}

function SepiImportICommDlgBrowser(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ICommDlgBrowser));

  with Result do
  begin
    AddMethod('OnDefaultCommand',
      'function(const ppshv: IShellView): HResult', ccStdCall);
    AddMethod('OnStateChange',
      'function(const ppshv: IShellView; Change: ULONG): HResult', ccStdCall);
    AddMethod('IncludeObject',
      'function(const ppshv: IShellView; pidl: PItemIDList): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ IShellView import }
{-------------------}

function SepiImportIShellView(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface(Owner.FindMeta('IShellView'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(IShellView));

  with Result do
  begin
    AddMethod('TranslateAccelerator',
      'function(var Msg: TMsg): HResult', ccStdCall);
    AddMethod('EnableModeless',
      'function(Enable: Boolean): HResult', ccStdCall);
    AddMethod('UIActivate',
      'function(State: UINT): HResult', ccStdCall);
    AddMethod('Refresh',
      'function: HResult', ccStdCall);
    AddMethod('CreateViewWindow',
      'function(PrevView: IShellView; var FolderSettings: TFolderSettings ; ShellBrowser: IShellBrowser ; var Rect: TRect ; out Wnd: HWND ) : HResult', ccStdCall);
    AddMethod('DestroyViewWindow',
      'function: HResult', ccStdCall);
    AddMethod('GetCurrentInfo',
      'function(out FolderSettings: TFolderSettings): HResult', ccStdCall);
    AddMethod('AddPropertySheetPages',
      'function(Reseved: DWORD; lpfnAddPage: TFNAddPropSheetPage ; lParam: LPARAM ) : HResult', ccStdCall);
    AddMethod('SaveViewState',
      'function: HResult', ccStdCall);
    AddMethod('SelectItem',
      'function(pidl: PItemIDList; flags: UINT): HResult', ccStdCall);
    AddMethod('GetItemObject',
      'function(Item: UINT; const iid: TIID; var IPtr: Pointer): HResult', ccStdCall);

    Complete;
  end;
end;

{------------------------}
{ _SV2CVW2_PARAMS import }
{------------------------}

function SepiImport_SV2CVW2_PARAMS(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SV2CVW2_PARAMS', False, True,
    TypeInfo(_SV2CVW2_PARAMS));

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('psvPrev', System.TypeInfo(IShellView));
    AddField('pfs', 'PFolderSettings');
    AddField('psbOwner', System.TypeInfo(IShellBrowser));
    AddField('prcView', 'PRect');
    AddField('pvid', 'PShellViewID');
    AddField('hwndView', System.TypeInfo(HWND));

    Complete;
  end;
end;

{--------------------}
{ IShellView2 import }
{--------------------}

function SepiImportIShellView2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellView2));

  with Result do
  begin
    AddMethod('GetView',
      'function(pvid: PShellViewID; uView: ULONG): HResult', ccStdCall);
    AddMethod('CreateViewWindow2',
      'function(SV2CreateParams: PSV2CreateParams): HResult', ccStdCall);
    AddMethod('HandleRename',
      'function(pidlNew: PItemIDList): HResult', ccStdCall);
    AddMethod('SelectAndPositionItem',
      'function(pidlItem: PItemIDList; uFlags: UINT; var Point: TPoint ) : HResult', ccStdCall);

    Complete;
  end;
end;

{----------------}
{ _STRRET import }
{----------------}

function SepiImport_STRRET(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_STRRET', False, True);

  with Result do
  begin
    AddField('uType', System.TypeInfo(UINT));
    AddFieldAfter('pOleStr', 'LPWSTR', 'uType');
    AddFieldAfter('pStr', 'LPSTR', 'uType');
    AddFieldAfter('uOffset', System.TypeInfo(UINT), 'uType');
    AddFieldAfter('cStr', '$3', 'uType');

    Complete;
  end;
end;

{----------------------}
{ _SHELLDETAILS import }
{----------------------}

function SepiImport_SHELLDETAILS(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHELLDETAILS', False, True);

  with Result do
  begin
    AddField('fmt', System.TypeInfo(Integer));
    AddField('cxChar', System.TypeInfo(Integer), True);
    AddField('str', 'STRRET');

    Complete;
  end;
end;

{----------------------}
{ IShellDetails import }
{----------------------}

function SepiImportIShellDetails(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellDetails));

  with Result do
  begin
    AddMethod('GetDetailsOf',
      'function(pidl: PItemIDList; iColumn: UINT; var pDetails: TShellDetails ) : HResult', ccStdCall);
    AddMethod('ColumnClick',
      'function(iColumn: UINT): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ _browseinfoA import }
{---------------------}

function SepiImport_browseinfoA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_browseinfoA', False, True);

  with Result do
  begin
    AddField('hwndOwner', System.TypeInfo(HWND));
    AddField('pidlRoot', 'PItemIDList');
    AddField('pszDisplayName', 'PAnsiChar');
    AddField('lpszTitle', 'PAnsiChar');
    AddField('ulFlags', System.TypeInfo(UINT));
    AddField('lpfn', 'TFNBFFCallBack');
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('iImage', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------}
{ _browseinfoW import }
{---------------------}

function SepiImport_browseinfoW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_browseinfoW', False, True);

  with Result do
  begin
    AddField('hwndOwner', System.TypeInfo(HWND));
    AddField('pidlRoot', 'PItemIDList');
    AddField('pszDisplayName', 'PWideChar');
    AddField('lpszTitle', 'PWideChar');
    AddField('ulFlags', System.TypeInfo(UINT));
    AddField('lpfn', 'TFNBFFCallBack');
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('iImage', System.TypeInfo(Integer));

    Complete;
  end;
end;

{--------------------}
{ IEnumIDList import }
{--------------------}

function SepiImportIEnumIDList(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IEnumIDList));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: ULONG; out rgelt: PItemIDList; var pceltFetched: ULONG ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: ULONG): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out ppenum: IEnumIDList): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IShellFolder import }
{---------------------}

function SepiImportIShellFolder(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellFolder));

  with Result do
  begin
    AddMethod('ParseDisplayName',
      'function(hwndOwner: HWND; pbcReserved: Pointer ; lpszDisplayName: POLESTR ; out pchEaten: ULONG ; out ppidl: PItemIDList ; var dwAttributes: ULONG ) : HResult', ccStdCall);
    AddMethod('EnumObjects',
      'function(hwndOwner: HWND; grfFlags: DWORD; out EnumIDList: IEnumIDList ) : HResult', ccStdCall);
    AddMethod('BindToObject',
      'function(pidl: PItemIDList; pbcReserved: Pointer; const riid: TIID ; out ppvOut ) : HResult', ccStdCall);
    AddMethod('BindToStorage',
      'function(pidl: PItemIDList; pbcReserved: Pointer; const riid: TIID ; out ppvObj ) : HResult', ccStdCall);
    AddMethod('CompareIDs',
      'function(lParam: LPARAM; pidl1, pidl2: PItemIDList ) : HResult', ccStdCall);
    AddMethod('CreateViewObject',
      'function(hwndOwner: HWND; const riid: TIID; out ppvOut ) : HResult', ccStdCall);
    AddMethod('GetAttributesOf',
      'function(cidl: UINT; var apidl: PItemIDList; var rgfInOut: UINT ) : HResult', ccStdCall);
    AddMethod('GetUIObjectOf',
      'function(hwndOwner: HWND; cidl: UINT; var apidl: PItemIDList; const riid: TIID ; prgfInOut: Pointer ; out ppvOut ) : HResult', ccStdCall);
    AddMethod('GetDisplayNameOf',
      'function(pidl: PItemIDList; uFlags: DWORD; var lpName: TStrRet ) : HResult', ccStdCall);
    AddMethod('SetNameOf',
      'function(hwndOwner: HWND; pidl: PItemIDList; lpszName: POLEStr; uFlags: DWORD ; var ppidlOut: PItemIDList ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ tagExtraSearch import }
{-----------------------}

function SepiImporttagExtraSearch(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagExtraSearch', False, True);

  with Result do
  begin
    AddField('guidSearch', 'TGUID');
    AddField('wszFriendlyName', '$4');
    AddField('wszMenuText', '$4', True);
    AddField('wszHelpText', '$5');
    AddField('wszUrl', '$6');
    AddField('wszIcon', '$7');
    AddField('wszGreyIcon', '$7', True);
    AddField('wszClrIcon', '$7', True);

    Complete;
  end;
end;

{-------------------------}
{ IEnumExtraSearch import }
{-------------------------}

function SepiImportIEnumExtraSearch(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IEnumExtraSearch));

  with Result do
  begin
    AddMethod('Next',
      'function(celt: ULONG; out rgelt: PExtraSearch; out pceltFetched: ULONG ) : HResult', ccStdCall);
    AddMethod('Skip',
      'function(celt: ULONG): HResult', ccStdCall);
    AddMethod('Reset',
      'function: HResult', ccStdCall);
    AddMethod('Clone',
      'function(out ppEnum: IEnumExtraSearch): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ SHCOLUMNID import }
{-------------------}

function SepiImportSHCOLUMNID(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'SHCOLUMNID', False, True);

  with Result do
  begin
    AddField('fmtid', 'TGUID');
    AddField('pid', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{----------------------}
{ IShellFolder2 import }
{----------------------}

function SepiImportIShellFolder2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellFolder2));

  with Result do
  begin
    AddMethod('GetDefaultSearchGUID',
      'function(out pguid: TGUID): HResult', ccStdCall);
    AddMethod('EnumSearches',
      'function(out ppEnum: IEnumExtraSearch): HResult', ccStdCall);
    AddMethod('GetDefaultColumn',
      'function(dwRes: DWORD; var pSort: ULONG; var pDisplay: ULONG ) : HResult', ccStdCall);
    AddMethod('GetDefaultColumnState',
      'function(iColumn: UINT; var pcsFlags: DWORD): HResult', ccStdCall);
    AddMethod('GetDetailsEx',
      'function(pidl: PItemIDList; const pscid: SHCOLUMNID; pv: POleVariant ) : HResult', ccStdCall);
    AddMethod('GetDetailsOf',
      'function(pidl: PItemIDList; iColumn: UINT; var psd: TShellDetails ) : HResult', ccStdCall);
    AddMethod('MapNameToSCID',
      'function(pwszName: LPCWSTR; var pscid: TShColumnID): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ IInputObjectSite import }
{-------------------------}

function SepiImportIInputObjectSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IInputObjectSite));

  with Result do
  begin
    AddMethod('OnFocusChangeIS',
      'function(punkObj: IUnknown; fSetFocus: BOOL): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ IInputObject import }
{---------------------}

function SepiImportIInputObject(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IInputObject));

  with Result do
  begin
    AddMethod('UIActivateIO',
      'function(fActivate: BOOL; var lpMsg: TMsg): HResult', ccStdCall);
    AddMethod('HasFocusIO',
      'function: HResult', ccStdCall);
    AddMethod('TranslateAcceleratorIO',
      'function(var lpMsg: TMsg): HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------------}
{ IDockingWindowSite import }
{---------------------------}

function SepiImportIDockingWindowSite(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDockingWindowSite));

  with Result do
  begin
    AddMethod('GetBorderDW',
      'function(punkObj: IUnknown; var prcBorder: TRect): HResult', ccStdCall);
    AddMethod('RequestBorderSpaceDW',
      'function(punkObj: IUnknown; var pbw: TBorderWidths): HResult', ccStdCall);
    AddMethod('SetBorderSpaceDW',
      'function(punkObj: IUnknown; var pbw: TBorderWidths): HResult', ccStdCall);

    Complete;
  end;
end;

{----------------------------}
{ IDockingWindowFrame import }
{----------------------------}

function SepiImportIDockingWindowFrame(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDockingWindowFrame));

  with Result do
  begin
    AddMethod('AddToolbar',
      'function(punkSrc: IUnknown; pwszItem: PWideChar; dwAddFlags: DWORD ) : HResult', ccStdCall);
    AddMethod('RemoveToolbar',
      'function(punkSrc: IUnknown; dwRemoveFlags: DWORD): HResult', ccStdCall);
    AddMethod('FindToolbar',
      'function(pwszItem: PWideChar; const riid: TIID; var ppvObj: Pointer ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ IDockingWindow import }
{-----------------------}

function SepiImportIDockingWindow(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDockingWindow));

  with Result do
  begin
    AddMethod('ShowDW',
      'function(fShow: BOOL): HResult', ccStdCall);
    AddMethod('CloseDW',
      'function(dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('ResizeBorderDW',
      'function(var prcBorder: TRect; punkToolbarSite: IUnknown; fReserved: BOOL ) : HResult', ccStdCall);

    Complete;
  end;
end;

{---------------------}
{ DESKBANDINFO import }
{---------------------}

function SepiImportDESKBANDINFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'DESKBANDINFO', True, True);

  with Result do
  begin
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('ptMinSize', 'TPointL');
    AddField('ptMaxSize', 'TPointL');
    AddField('ptIntegral', 'TPointL');
    AddField('ptActual', 'TPointL');
    AddField('wszTitle', '$8');
    AddField('dwModeFlags', System.TypeInfo(DWORD));
    AddField('crBkgnd', System.TypeInfo(COLORREF));

    Complete;
  end;
end;

{------------------}
{ IDeskBand import }
{------------------}

function SepiImportIDeskBand(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDeskBand));

  with Result do
  begin
    AddMethod('GetBandInfo',
      'function(dwBandID, dwViewMode: DWORD; var pdbi: TDeskBandInfo): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ _tagWALLPAPEROPT import }
{-------------------------}

function SepiImport_tagWALLPAPEROPT(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_tagWALLPAPEROPT', True, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(DWORD));
    AddField('dwStyle', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{--------------------------}
{ _tagCOMPONENTSOPT import }
{--------------------------}

function SepiImport_tagCOMPONENTSOPT(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_tagCOMPONENTSOPT', True, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(DWORD));
    AddField('fEnableComponents', System.TypeInfo(BOOL));
    AddField('fActiveDesktop', System.TypeInfo(BOOL));

    Complete;
  end;
end;

{--------------------}
{ _tagCOMPPOS import }
{--------------------}

function SepiImport_tagCOMPPOS(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_tagCOMPPOS', True, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(DWORD));
    AddField('iLeft', System.TypeInfo(Integer));
    AddField('iTop', System.TypeInfo(Integer));
    AddField('dwWidth', System.TypeInfo(DWORD));
    AddField('dwHeight', System.TypeInfo(DWORD));
    AddField('izIndex', System.TypeInfo(Integer));
    AddField('fCanResize', System.TypeInfo(BOOL));
    AddField('fCanResizeX', System.TypeInfo(BOOL));
    AddField('fCanResizeY', System.TypeInfo(BOOL));
    AddField('iPreferredLeftPercent', System.TypeInfo(Integer));
    AddField('iPreferredTopPercent', System.TypeInfo(Integer));

    Complete;
  end;
end;

{----------------------}
{ _tagCOMPONENT import }
{----------------------}

function SepiImport_tagCOMPONENT(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_tagCOMPONENT', True, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(DWORD));
    AddField('dwID', System.TypeInfo(DWORD));
    AddField('iComponentType', System.TypeInfo(Integer));
    AddField('fChecked', System.TypeInfo(BOOL));
    AddField('fDirty', System.TypeInfo(BOOL));
    AddField('fNoScroll', System.TypeInfo(BOOL));
    AddField('cpPos', 'TCompPos');
    AddField('wszFriendlyName', '$9');
    AddField('wszSource', '$10');
    AddField('wszSubscribedURL', '$11');

    Complete;
  end;
end;

{-----------------------}
{ IActiveDesktop import }
{-----------------------}

function SepiImportIActiveDesktop(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IActiveDesktop));

  with Result do
  begin
    AddMethod('ApplyChanges',
      'function(dwFlags: DWORD): HResult', ccStdCall);
    AddMethod('GetWallpaper',
      'function(pwszWallpaper: PWideChar; cchWallpaper: UINT; dwReserved: DWORD ) : HResult', ccStdCall);
    AddMethod('SetWallpaper',
      'function(pwszWallpaper: PWideChar; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('GetWallpaperOptions',
      'function(var pwpo: TWallPaperOpt; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('SetWallpaperOptions',
      'function(var pwpo: TWallPaperOpt; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('GetPattern',
      'function(pwszPattern: PWideChar; cchPattern: UINT; dwReserved: DWORD ) : HResult', ccStdCall);
    AddMethod('SetPattern',
      'function(pwszPattern: PWideChar; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('GetDesktopItemOptions',
      'function(var pco: TComponentsOpt; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('SetDesktopItemOptions',
      'function(var pco: TComponentsOpt; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('AddDesktopItem',
      'function(var pcomp: TShComponent; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('AddDesktopItemWithUI',
      'function(hwnd: HWND; var pcomp: TShComponent; dwReserved: DWORD ) : HResult', ccStdCall);
    AddMethod('ModifyDesktopItem',
      'function(var pcomp: TShComponent; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('RemoveDesktopItem',
      'function(var pcomp: TShComponent; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('GetDesktopItemCount',
      'function(var lpiCount: Integer; dwReserved: DWORD): HResult', ccStdCall);
    AddMethod('GetDesktopItem',
      'function(nComponent: Integer; var pcomp: TShComponent; dwReserved: DWORD ) : HResult', ccStdCall);
    AddMethod('GetDesktopItemByID',
      'function(dwID: DWORD; var pcomp: TShComponent; dwReserved: DWORD ) : HResult', ccStdCall);
    AddMethod('GenerateDesktopItemHtml',
      'function(pwszFileName: PWideChar; var pcomp: TShComponent ; dwReserved: DWORD ) : HResult', ccStdCall);
    AddMethod('AddUrl',
      'function(hwnd: HWND; pszSource: PWideChar; var pcomp: TShComponent; dwFlags: DWORD ) : HResult', ccStdCall);
    AddMethod('GetDesktopItemBySource',
      'function(pwszSource: PWideChar; var pcomp: TShComponent; dwReserved: DWORD ) : HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ _NRESARRAY import }
{-------------------}

function SepiImport_NRESARRAY(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_NRESARRAY', False, True);

  with Result do
  begin
    AddField('cItems', System.TypeInfo(UINT));
    AddField('nr', '$12');

    Complete;
  end;
end;

{-------------}
{ _IDA import }
{-------------}

function SepiImport_IDA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_IDA', False, True);

  with Result do
  begin
    AddField('cidl', System.TypeInfo(UINT));
    AddField('aoffset', '$13');

    Complete;
  end;
end;

{-------------------------}
{ _FILEDESCRIPTORA import }
{-------------------------}

function SepiImport_FILEDESCRIPTORA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_FILEDESCRIPTORA', False, True);

  with Result do
  begin
    AddField('dwFlags', System.TypeInfo(DWORD));
    AddField('clsid', 'TCLSID');
    AddField('sizel', 'TSize');
    AddField('pointl', 'TPoint');
    AddField('dwFileAttributes', System.TypeInfo(DWORD));
    AddField('ftCreationTime', 'TFileTime');
    AddField('ftLastAccessTime', 'TFileTime');
    AddField('ftLastWriteTime', 'TFileTime');
    AddField('nFileSizeHigh', System.TypeInfo(DWORD));
    AddField('nFileSizeLow', System.TypeInfo(DWORD));
    AddField('cFileName', '$14');

    Complete;
  end;
end;

{-------------------------}
{ _FILEDESCRIPTORW import }
{-------------------------}

function SepiImport_FILEDESCRIPTORW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_FILEDESCRIPTORW', False, True);

  with Result do
  begin
    AddField('dwFlags', System.TypeInfo(DWORD));
    AddField('clsid', 'TCLSID');
    AddField('sizel', 'TSize');
    AddField('pointl', 'TPoint');
    AddField('dwFileAttributes', System.TypeInfo(DWORD));
    AddField('ftCreationTime', 'TFileTime');
    AddField('ftLastAccessTime', 'TFileTime');
    AddField('ftLastWriteTime', 'TFileTime');
    AddField('nFileSizeHigh', System.TypeInfo(DWORD));
    AddField('nFileSizeLow', System.TypeInfo(DWORD));
    AddField('cFileName', '$15');

    Complete;
  end;
end;

{------------------------------}
{ _FILEGROUPDESCRIPTORA import }
{------------------------------}

function SepiImport_FILEGROUPDESCRIPTORA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_FILEGROUPDESCRIPTORA', False, True);

  with Result do
  begin
    AddField('cItems', System.TypeInfo(UINT));
    AddField('fgd', '$16');

    Complete;
  end;
end;

{------------------------------}
{ _FILEGROUPDESCRIPTORW import }
{------------------------------}

function SepiImport_FILEGROUPDESCRIPTORW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_FILEGROUPDESCRIPTORW', False, True);

  with Result do
  begin
    AddField('cItems', System.TypeInfo(UINT));
    AddField('fgd', '$17');

    Complete;
  end;
end;

{-------------------}
{ _DROPFILES import }
{-------------------}

function SepiImport_DROPFILES(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DROPFILES', False, True);

  with Result do
  begin
    AddField('pFiles', System.TypeInfo(DWORD));
    AddField('pt', 'TPoint');
    AddField('fNC', System.TypeInfo(BOOL));
    AddField('fWide', System.TypeInfo(BOOL));

    Complete;
  end;
end;

{---------------------------}
{ IShellChangeNotify import }
{---------------------------}

function SepiImportIShellChangeNotify(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IShellChangeNotify));

  with Result do
  begin
    AddMethod('OnChange',
      'function(lEvent: Longint; var pidl1, pidl2: TItemIDList): HResult', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ IQueryInfo import }
{-------------------}

function SepiImportIQueryInfo(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IQueryInfo));

  with Result do
  begin
    AddMethod('GetInfoTip',
      'function(dwFlags: DWORD; var ppwszTip: PWideChar): HResult', ccStdCall);
    AddMethod('GetInfoFlags',
      'function(out pdwFlags: DWORD): HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------}
{ SHDRAGIMAGE import }
{--------------------}

function SepiImportSHDRAGIMAGE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'SHDRAGIMAGE', False, True);

  with Result do
  begin
    AddField('sizeDragImage', 'SIZE');
    AddField('ptOffset', 'TPoint');
    AddField('hbmpDragImage', System.TypeInfo(HBITMAP));
    AddField('crColorKey', System.TypeInfo(COLORREF));

    Complete;
  end;
end;

{--------------------------}
{ IDropTargetHelper import }
{--------------------------}

function SepiImportIDropTargetHelper(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDropTargetHelper));

  with Result do
  begin
    AddMethod('DragEnter',
      'function(hwndTarget: HWND; const pDataObject: IDataObject; var ppt: TPoint ; dwEffect: DWORD ) : HRESULT', ccStdCall);
    AddMethod('DragLeave',
      'function: HResult', ccStdCall);
    AddMethod('DragOver',
      'function(var ppt: TPoint; dwEffect: DWORD): HRESULT', ccStdCall);
    AddMethod('Drop',
      'function(const pDataObject: IDataObject; var ppt: TPoint; dwEffect: DWORD ) : HRESULT', ccStdCall);
    AddMethod('Show',
      'function(fShow: BOOL): HRESULT', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ IDragSourceHelper import }
{--------------------------}

function SepiImportIDragSourceHelper(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDragSourceHelper));

  with Result do
  begin
    AddMethod('InitializeFromBitmap',
      'function(pshdi: LPSHDRAGIMAGE; const pDataObject: IDataObject ) : HRESULT', ccStdCall);
    AddMethod('InitializeFromWindow',
      'function(hwnd: HWND; var ppt: TPoint; const pDataObject: IDataObject ) : HRESULT', ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ _SHDESCRIPTIONID import }
{-------------------------}

function SepiImport_SHDESCRIPTIONID(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHDESCRIPTIONID', False, True);

  with Result do
  begin
    AddField('dwDescriptionId', System.TypeInfo(DWORD));
    AddField('Id', 'TCLSID');

    Complete;
  end;
end;

{-----------------------}
{ SHELLFLAGSTATE import }
{-----------------------}

function SepiImportSHELLFLAGSTATE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'SHELLFLAGSTATE', True, True);

  with Result do
  begin
    AddField('Data', System.TypeInfo(Word));

    Complete;
  end;
end;

{----------------------}
{ IAutoComplete import }
{----------------------}

function SepiImportIAutoComplete(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IAutoComplete));

  with Result do
  begin
    AddMethod('Init',
      'function(hwndEdit: HWND; punkACL: IUnknown; pwszRegKeyPath: LPCWSTR ; pwszQuickComplete: LPCWSTR ) : HRESULT', ccStdCall);
    AddMethod('Enable',
      'function(fEnable: BOOL): HRESULT', ccStdCall);

    Complete;
  end;
end;

{-----------------------}
{ IAutoComplete2 import }
{-----------------------}

function SepiImportIAutoComplete2(Owner : TSepiMetaUnit) : TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IAutoComplete2));

  with Result do
  begin
    AddMethod('SetOptions',
      'function(dwFlag: DWORD): HRESULT', ccStdCall);
    AddMethod('GetOptions',
      'function(var dwFlag: DWORD): HRESULT', ccStdCall);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ShlObjTypes',
    ['Windows', 'ActiveXTypes', 'CommCtrlTypes', 'ShellAPITypes', {'RegStr',}
     'Messages'{,'WinInet', 'UrlMon'}]);

  // Types
  TSepiPointerType.Create(Result, 'PSHItemID', 'TSHItemID', True);
  TSepiArrayType.Create(Result, '$1',
    [0, 0], TypeInfo(Byte), True);
  SepiImport_SHITEMID(Result);
  TSepiTypeAlias.Create(Result, 'TSHItemID', '_SHITEMID');
  TSepiTypeAlias.Create(Result, 'SHITEMID', '_SHITEMID');
  TSepiPointerType.Create(Result, 'PItemIDList', 'TItemIDList', True);
  SepiImport_ITEMIDLIST(Result);
  TSepiTypeAlias.Create(Result, 'TItemIDList', '_ITEMIDLIST');
  TSepiTypeAlias.Create(Result, 'ITEMIDLIST', '_ITEMIDLIST');

  // Types
  TSepiPointerType.Create(Result, 'PCMInvokeCommandInfo', 'TCMInvokeCommandInfo', True);
  SepiImport_CMINVOKECOMMANDINFO(Result);
  TSepiTypeAlias.Create(Result, 'TCMInvokeCommandInfo', '_CMINVOKECOMMANDINFO');
  TSepiTypeAlias.Create(Result, 'CMINVOKECOMMANDINFO', '_CMINVOKECOMMANDINFO');
  TSepiPointerType.Create(Result, 'PCMInvokeCommandInfoEx', 'TCMInvokeCommandInfoEx', True);
  SepiImport_CMInvokeCommandInfoEx(Result);
  TSepiTypeAlias.Create(Result, 'TCMInvokeCommandInfoEx', '_CMINVOKECOMMANDINFOEX');
  TSepiTypeAlias.Create(Result, 'CMINVOKECOMMANDINFOEX', '_CMINVOKECOMMANDINFOEX');
  SepiImportIContextMenu(Result);
  SepiImportIContextMenu2(Result);
  SepiImportIContextMenu3(Result);

  // Types
  SepiImportIShellExtInit(Result);

  // Types
  SepiImportIShellPropSheetExt(Result);
  SepiImportIPersistFolder(Result);
  SepiImportIPersistFolder2(Result);

  // Types
  SepiImportIExtractIconA(Result);
  SepiImportIExtractIconW(Result);
  TSepiTypeAlias.Create(Result, 'IExtractIcon', TypeInfo(IExtractIconA));
  SepiImportIShellIcon(Result);
  SepiImportIShellIconOverlayIdentifier(Result);

  // Types
  SepiImportIShellIconOverlay(Result);

  // Types
  SepiImportIShellLinkA(Result);
  SepiImportIShellLinkW(Result);
  TSepiTypeAlias.Create(Result, 'IShellLink', TypeInfo(IShellLinkA));
  SepiImportIShellExecuteHookA(Result);
  SepiImportIShellExecuteHookW(Result);
  TSepiTypeAlias.Create(Result, 'IShellExecuteHook', TypeInfo(IShellExecuteHookA));
  SepiImportIURLSearchHook(Result);
  SepiImportINewShortcutHookA(Result);
  SepiImportINewShortcutHookW(Result);
  TSepiTypeAlias.Create(Result, 'INewShortcutHook', TypeInfo(INewShortcutHookA));

  // Types
  SepiImportICopyHookA(Result);
  SepiImportICopyHookW(Result);
  TSepiTypeAlias.Create(Result, 'ICopyHook', TypeInfo(ICopyHookA));

  // Types
  SepiImportIFileViewerSite(Result);

  // Types
  TSepiPointerType.Create(Result, 'PFVShowInfo', TypeInfo(FVSHOWINFO), True);
  TSepiArrayType.Create(Result, '$2',
    [0, MAX_PATH-1], TypeInfo(TOleChar), True);
  SepiImportFVSHOWINFO(Result);
  TSepiTypeAlias.Create(Result, 'TFVShowInfo', TypeInfo(FVSHOWINFO));
  TSepiTypeAlias.Create(Result, 'LPFVSHOWINFO', 'PFVShowInfo');

  // Types
  SepiImportIFileViewerA(Result);
  SepiImportIFileViewerW(Result);
  TSepiTypeAlias.Create(Result, 'IFileViewer', TypeInfo(IFileViewerA));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSHDVIDEnums));

  // Types
  TSepiPointerType.Create(Result, 'PFolderSettings', 'FOLDERSETTINGS', True);
  SepiImportFOLDERSETTINGS(Result);
  TSepiTypeAlias.Create(Result, 'TFolderSettings', 'FOLDERSETTINGS');
  TSepiTypeAlias.Create(Result, 'LPFOLDERSETTINGS', 'PFolderSettings');

  // Types
  TSepiTypeAlias.Create(Result, 'HOLEMENU', TypeInfo(HGLOBAL));
  TSepiInterface.ForwardDecl(Result, TypeInfo(IShellView));
  SepiImportIShellBrowser(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSBSCEnums));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSBOEnums));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSBCMDIDEnums));
  SepiImportICommDlgBrowser(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSVUIAEnums));
  SepiImportIShellView(Result);

  // Types
  TSepiTypeAlias.Create(Result, 'SHELLVIEWID', 'TGUID');
  TSepiTypeAlias.Create(Result, 'TShellViewID', 'SHELLVIEWID');
  TSepiPointerType.Create(Result, 'PShellViewID', 'TShellViewID', True);
  TSepiPointerType.Create(Result, 'PSV2CreateParams', TypeInfo(TSV2CreateParams), True);
  SepiImport_SV2CVW2_PARAMS(Result);
  TSepiTypeAlias.Create(Result, 'TSV2CreateParams', '_SV2CVW2_PARAMS');
  TSepiTypeAlias.Create(Result, 'SV2CVW2_PARAMS', '_SV2CVW2_PARAMS');
  SepiImportIShellView2(Result);

  // Types
  TSepiPointerType.Create(Result, 'PSTRRet', 'TStrRet', True);
  TSepiArrayType.Create(Result, '$3',
    [0, MAX_PATH-1], TypeInfo(Char), True);
  SepiImport_STRRET(Result);
  TSepiTypeAlias.Create(Result, 'TStrRet', '_STRRET');
  TSepiTypeAlias.Create(Result, 'STRRET', '_STRRET');
  TSepiPointerType.Create(Result, 'PShellDetails', 'TShellDetails', True);
  SepiImport_SHELLDETAILS(Result);
  TSepiTypeAlias.Create(Result, 'TShellDetails', '_SHELLDETAILS');
  TSepiTypeAlias.Create(Result, 'SHELLDETAILS', '_SHELLDETAILS');
  SepiImportIShellDetails(Result);

  // Types
  TSepiMethodRefType.Create(Result, 'BFFCALLBACK',
    'function(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TFNBFFCallBack',
    'function(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer', False, ccStdCall);
  TSepiPointerType.Create(Result, 'PBrowseInfoA', 'TBrowseInfoA', True);
  TSepiPointerType.Create(Result, 'PBrowseInfoW', 'TBrowseInfoW', True);
  TSepiTypeAlias.Create(Result, 'PBrowseInfo', 'PBrowseInfoA');
  SepiImport_browseinfoA(Result);
  SepiImport_browseinfoW(Result);
  TSepiTypeAlias.Create(Result, '_browseinfo', '_browseinfoA');
  TSepiTypeAlias.Create(Result, 'TBrowseInfoA', '_browseinfoA');
  TSepiTypeAlias.Create(Result, 'TBrowseInfoW', '_browseinfoW');
  TSepiTypeAlias.Create(Result, 'TBrowseInfo', 'TBrowseInfoA');
  TSepiTypeAlias.Create(Result, 'BROWSEINFOA', '_browseinfoA');
  TSepiTypeAlias.Create(Result, 'BROWSEINFOW', '_browseinfoW');
  TSepiTypeAlias.Create(Result, 'BROWSEINFO', 'BROWSEINFOA');

  // Types
  SepiImportIEnumIDList(Result);

  // Types
  SepiImportIShellFolder(Result);
  TSepiPointerType.Create(Result, 'PExtraSearch', 'TExtraSearch', True);
  TSepiArrayType.Create(Result, '$4',
    [0, 79], TypeInfo(WideChar), True);
  TSepiArrayType.Create(Result, '$5',
    [0, MAX_PATH], TypeInfo(WideChar), True);
  TSepiArrayType.Create(Result, '$6',
    [0, 2047], TypeInfo(WideChar), True);
  TSepiArrayType.Create(Result, '$7',
    [0, MAX_PATH+10], TypeInfo(WideChar), True);
  SepiImporttagExtraSearch(Result);
  TSepiTypeAlias.Create(Result, 'TExtraSearch', 'tagExtraSearch');
  SepiImportIEnumExtraSearch(Result);

  // Types
  TSepiPointerType.Create(Result, 'PShColumnID', 'TShColumnID', True);
  SepiImportSHCOLUMNID(Result);
  TSepiTypeAlias.Create(Result, 'TShColumnID', 'SHCOLUMNID');
  SepiImportIShellFolder2(Result);

  // Types
  SepiImportIInputObjectSite(Result);
  SepiImportIInputObject(Result);
  SepiImportIDockingWindowSite(Result);

  // Types
  SepiImportIDockingWindowFrame(Result);
  SepiImportIDockingWindow(Result);

  // Types
  TSepiArrayType.Create(Result, '$8',
    [0, 255], TypeInfo(WideChar), True);
  SepiImportDESKBANDINFO(Result);
  TSepiPointerType.Create(Result, 'PDeskBandInfo', 'TDeskBandInfo', True);
  TSepiTypeAlias.Create(Result, 'TDeskBandInfo', 'DESKBANDINFO');

  // Types
  SepiImportIDeskBand(Result);

  // Types
  SepiImport_tagWALLPAPEROPT(Result);
  TSepiPointerType.Create(Result, 'PWallPaperOpt', 'TWallPaperOpt', True);
  TSepiTypeAlias.Create(Result, 'TWallPaperOpt', '_tagWALLPAPEROPT');
  SepiImport_tagCOMPONENTSOPT(Result);
  TSepiPointerType.Create(Result, 'PComponentsOpt', 'TComponentsOpt', True);
  TSepiTypeAlias.Create(Result, 'TComponentsOpt', '_tagCOMPONENTSOPT');
  SepiImport_tagCOMPPOS(Result);
  TSepiPointerType.Create(Result, 'PCompPos', 'TCompPos', True);
  TSepiTypeAlias.Create(Result, 'TCompPos', '_tagCOMPPOS');

  // Types
  TSepiArrayType.Create(Result, '$9',
    [0, MAX_PATH - 1], TypeInfo(WideChar), True);
  TSepiArrayType.Create(Result, '$10',
    [0, INTERNET_MAX_URL_LENGTH - 1], TypeInfo(WideChar), True);
  TSepiArrayType.Create(Result, '$11',
    [0, INTERNET_MAX_URL_LENGTH - 1], TypeInfo(WideChar), True);
  SepiImport_tagCOMPONENT(Result);
  TSepiPointerType.Create(Result, 'PShComponent', 'TShComponent', True);
  TSepiTypeAlias.Create(Result, 'TShComponent', '_tagCOMPONENT');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(tagDTI_ADTIWUI));
  SepiImportIActiveDesktop(Result);

  // Types
  TSepiPointerType.Create(Result, 'PNResArray', 'TNResArray', True);
  TSepiArrayType.Create(Result, '$12',
    [0, 0], 'TNetResource', True);
  SepiImport_NRESARRAY(Result);
  TSepiTypeAlias.Create(Result, 'TNResArray', '_NRESARRAY');
  TSepiTypeAlias.Create(Result, 'NRESARRAY', '_NRESARRAY');
  TSepiPointerType.Create(Result, 'PIDA', 'TIDA', True);
  TSepiArrayType.Create(Result, '$13',
    [0, 0], TypeInfo(UINT), True);
  SepiImport_IDA(Result);
  TSepiTypeAlias.Create(Result, 'TIDA', '_IDA');
  TSepiTypeAlias.Create(Result, 'CIDA', '_IDA');

  // Types
  TSepiPointerType.Create(Result, 'PFileDescriptorA', 'TFileDescriptorA', True);
  TSepiPointerType.Create(Result, 'PFileDescriptorW', 'TFileDescriptorW', True);
  TSepiTypeAlias.Create(Result, 'PFileDescriptor', 'PFileDescriptorA');
  TSepiArrayType.Create(Result, '$14',
    [0, MAX_PATH-1], TypeInfo(AnsiChar), True);
  SepiImport_FILEDESCRIPTORA(Result);
  TSepiArrayType.Create(Result, '$15',
    [0, MAX_PATH-1], TypeInfo(WideChar), True);
  SepiImport_FILEDESCRIPTORW(Result);
  TSepiTypeAlias.Create(Result, '_FILEDESCRIPTOR', '_FILEDESCRIPTORA');
  TSepiTypeAlias.Create(Result, 'TFileDescriptorA', '_FILEDESCRIPTORA');
  TSepiTypeAlias.Create(Result, 'TFileDescriptorW', '_FILEDESCRIPTORW');
  TSepiTypeAlias.Create(Result, 'TFileDescriptor', 'TFileDescriptorA');
  TSepiTypeAlias.Create(Result, 'FILEDESCRIPTORA', '_FILEDESCRIPTORA');
  TSepiTypeAlias.Create(Result, 'FILEDESCRIPTORW', '_FILEDESCRIPTORW');
  TSepiTypeAlias.Create(Result, 'FILEDESCRIPTOR', 'FILEDESCRIPTORA');
  TSepiPointerType.Create(Result, 'PFileGroupDescriptorA', 'TFileGroupDescriptorA', True);
  TSepiPointerType.Create(Result, 'PFileGroupDescriptorW', 'TFileGroupDescriptorW', True);
  TSepiTypeAlias.Create(Result, 'PFileGroupDescriptor', 'PFileGroupDescriptorA');
  TSepiArrayType.Create(Result, '$16',
    [0, 0], 'TFileDescriptorA', True);
  SepiImport_FILEGROUPDESCRIPTORA(Result);
  TSepiArrayType.Create(Result, '$17',
    [0, 0], 'TFileDescriptorW', True);
  SepiImport_FILEGROUPDESCRIPTORW(Result);
  TSepiTypeAlias.Create(Result, '_FILEGROUPDESCRIPTOR', '_FILEGROUPDESCRIPTORA');
  TSepiTypeAlias.Create(Result, 'TFileGroupDescriptorA', '_FILEGROUPDESCRIPTORA');
  TSepiTypeAlias.Create(Result, 'TFileGroupDescriptorW', '_FILEGROUPDESCRIPTORW');
  TSepiTypeAlias.Create(Result, 'TFileGroupDescriptor', 'TFileGroupDescriptorA');
  TSepiTypeAlias.Create(Result, 'FILEGROUPDESCRIPTORA', '_FILEGROUPDESCRIPTORA');
  TSepiTypeAlias.Create(Result, 'FILEGROUPDESCRIPTORW', '_FILEGROUPDESCRIPTORW');
  TSepiTypeAlias.Create(Result, 'FILEGROUPDESCRIPTOR', 'FILEGROUPDESCRIPTORA');
  TSepiPointerType.Create(Result, 'PDropFiles', 'TDropFiles', True);
  SepiImport_DROPFILES(Result);
  TSepiTypeAlias.Create(Result, 'TDropFiles', '_DROPFILES');
  TSepiTypeAlias.Create(Result, 'DROPFILES', '_DROPFILES');

  // Types
  SepiImportIShellChangeNotify(Result);
  SepiImportIQueryInfo(Result);

  // Types
  TSepiPointerType.Create(Result, 'LPSHDRAGIMAGE', 'SHDRAGIMAGE', True);
  SepiImportSHDRAGIMAGE(Result);
  TSepiTypeAlias.Create(Result, 'TShDragImage', 'SHDRAGIMAGE');
  TSepiTypeAlias.Create(Result, 'PShDragImage', 'LPSHDRAGIMAGE');

  // Types
  SepiImportIDropTargetHelper(Result);
  SepiImportIDragSourceHelper(Result);

  // Types
  TSepiPointerType.Create(Result, 'PSHDescriptionID', 'TSHDescriptionID', True);
  SepiImport_SHDESCRIPTIONID(Result);
  TSepiTypeAlias.Create(Result, 'TSHDescriptionID', '_SHDESCRIPTIONID');
  TSepiTypeAlias.Create(Result, 'SHDESCRIPTIONID', '_SHDESCRIPTIONID');

  // Types
  SepiImportSHELLFLAGSTATE(Result);
  TSepiPointerType.Create(Result, 'PShellFlagState', 'TShellFlagState', True);
  TSepiTypeAlias.Create(Result, 'TShellFlagState', 'SHELLFLAGSTATE');

  // Types
  SepiImportIAutoComplete(Result);

  // Types
  SepiImportIAutoComplete2(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ShlObjTypes', ImportUnit);
end.

