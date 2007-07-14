{*
  Importe l'unité ShellAPI dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsShellAPITypes;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Windows, ShellAPI;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------------}
{ _DRAGINFOA import }
{-------------------}

function SepiImport_DRAGINFOA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DRAGINFOA', False, True);

  with Result do
  begin
    AddField('uSize', System.TypeInfo(UINT));
    AddField('pt', 'TPoint');
    AddField('fNC', System.TypeInfo(BOOL));
    AddField('lpFileList', 'PAnsiChar');
    AddField('grfKeyState', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------------}
{ _DRAGINFOW import }
{-------------------}

function SepiImport_DRAGINFOW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DRAGINFOW', False, True);

  with Result do
  begin
    AddField('uSize', System.TypeInfo(UINT));
    AddField('pt', 'TPoint');
    AddField('fNC', System.TypeInfo(BOOL));
    AddField('lpFileList', 'PWideChar');
    AddField('grfKeyState', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{--------------------}
{ _AppBarData import }
{--------------------}

function SepiImport_AppBarData(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_AppBarData', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('hWnd', System.TypeInfo(HWND));
    AddField('uCallbackMessage', System.TypeInfo(UINT));
    AddField('uEdge', System.TypeInfo(UINT));
    AddField('rc', 'TRect');
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{-------------------------}
{ _SHFILEOPSTRUCTA import }
{-------------------------}

function SepiImport_SHFILEOPSTRUCTA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHFILEOPSTRUCTA', True, True);

  with Result do
  begin
    AddField('Wnd', System.TypeInfo(HWND));
    AddField('wFunc', System.TypeInfo(UINT));
    AddField('pFrom', 'PAnsiChar');
    AddField('pTo', 'PAnsiChar');
    AddField('fFlags', System.TypeInfo(FILEOP_FLAGS));
    AddField('fAnyOperationsAborted', System.TypeInfo(BOOL));
    AddField('hNameMappings', 'Pointer');
    AddField('lpszProgressTitle', 'PAnsiChar');

    Complete;
  end;
end;

{-------------------------}
{ _SHFILEOPSTRUCTW import }
{-------------------------}

function SepiImport_SHFILEOPSTRUCTW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHFILEOPSTRUCTW', True, True);

  with Result do
  begin
    AddField('Wnd', System.TypeInfo(HWND));
    AddField('wFunc', System.TypeInfo(UINT));
    AddField('pFrom', 'PWideChar');
    AddField('pTo', 'PWideChar');
    AddField('fFlags', System.TypeInfo(FILEOP_FLAGS));
    AddField('fAnyOperationsAborted', System.TypeInfo(BOOL));
    AddField('hNameMappings', 'Pointer');
    AddField('lpszProgressTitle', 'PWideChar');

    Complete;
  end;
end;

{------------------------}
{ _SHNAMEMAPPINGA import }
{------------------------}

function SepiImport_SHNAMEMAPPINGA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHNAMEMAPPINGA', False, True);

  with Result do
  begin
    AddField('pszOldPath', 'PAnsiChar');
    AddField('pszNewPath', 'PAnsiChar');
    AddField('cchOldPath', System.TypeInfo(Integer));
    AddField('cchNewPath', System.TypeInfo(Integer));

    Complete;
  end;
end;

{------------------------}
{ _SHNAMEMAPPINGW import }
{------------------------}

function SepiImport_SHNAMEMAPPINGW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHNAMEMAPPINGW', False, True);

  with Result do
  begin
    AddField('pszOldPath', 'PWideChar');
    AddField('pszNewPath', 'PWideChar');
    AddField('cchOldPath', System.TypeInfo(Integer));
    AddField('cchNewPath', System.TypeInfo(Integer));

    Complete;
  end;
end;

{---------------------------}
{ _SHELLEXECUTEINFOA import }
{---------------------------}

function SepiImport_SHELLEXECUTEINFOA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHELLEXECUTEINFOA', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('fMask', System.TypeInfo(ULONG));
    AddField('Wnd', System.TypeInfo(HWND));
    AddField('lpVerb', 'PAnsiChar');
    AddField('lpFile', 'PAnsiChar');
    AddField('lpParameters', 'PAnsiChar');
    AddField('lpDirectory', 'PAnsiChar');
    AddField('nShow', System.TypeInfo(Integer));
    AddField('hInstApp', System.TypeInfo(HINST));
    AddField('lpIDList', 'Pointer');
    AddField('lpClass', 'PAnsiChar');
    AddField('hkeyClass', System.TypeInfo(HKEY));
    AddField('dwHotKey', System.TypeInfo(DWORD));
    AddField('hIcon', System.TypeInfo(THandle));
    AddField('hProcess', System.TypeInfo(THandle));

    Complete;
  end;
end;

{---------------------------}
{ _SHELLEXECUTEINFOW import }
{---------------------------}

function SepiImport_SHELLEXECUTEINFOW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHELLEXECUTEINFOW', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('fMask', System.TypeInfo(ULONG));
    AddField('Wnd', System.TypeInfo(HWND));
    AddField('lpVerb', 'PWideChar');
    AddField('lpFile', 'PWideChar');
    AddField('lpParameters', 'PWideChar');
    AddField('lpDirectory', 'PWideChar');
    AddField('nShow', System.TypeInfo(Integer));
    AddField('hInstApp', System.TypeInfo(HINST));
    AddField('lpIDList', 'Pointer');
    AddField('lpClass', 'PWideChar');
    AddField('hkeyClass', System.TypeInfo(HKEY));
    AddField('dwHotKey', System.TypeInfo(DWORD));
    AddField('hIcon', System.TypeInfo(THandle));
    AddField('hProcess', System.TypeInfo(THandle));

    Complete;
  end;
end;

{-------------------------}
{ _NOTIFYICONDATAA import }
{-------------------------}

function SepiImport_NOTIFYICONDATAA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_NOTIFYICONDATAA', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('Wnd', System.TypeInfo(HWND));
    AddField('uID', System.TypeInfo(UINT));
    AddField('uFlags', System.TypeInfo(UINT));
    AddField('uCallbackMessage', System.TypeInfo(UINT));
    AddField('hIcon', System.TypeInfo(HICON));
    AddField('szTip', '$1');

    Complete;
  end;
end;

{-------------------------}
{ _NOTIFYICONDATAW import }
{-------------------------}

function SepiImport_NOTIFYICONDATAW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_NOTIFYICONDATAW', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('Wnd', System.TypeInfo(HWND));
    AddField('uID', System.TypeInfo(UINT));
    AddField('uFlags', System.TypeInfo(UINT));
    AddField('uCallbackMessage', System.TypeInfo(UINT));
    AddField('hIcon', System.TypeInfo(HICON));
    AddField('szTip', '$2');

    Complete;
  end;
end;

{---------------------}
{ _SHFILEINFOA import }
{---------------------}

function SepiImport_SHFILEINFOA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHFILEINFOA', False, True);

  with Result do
  begin
    AddField('hIcon', System.TypeInfo(HICON));
    AddField('iIcon', System.TypeInfo(Integer));
    AddField('dwAttributes', System.TypeInfo(DWORD));
    AddField('szDisplayName', '$3');
    AddField('szTypeName', '$4');

    Complete;
  end;
end;

{---------------------}
{ _SHFILEINFOW import }
{---------------------}

function SepiImport_SHFILEINFOW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_SHFILEINFOW', False, True);

  with Result do
  begin
    AddField('hIcon', System.TypeInfo(HICON));
    AddField('iIcon', System.TypeInfo(Integer));
    AddField('dwAttributes', System.TypeInfo(DWORD));
    AddField('szDisplayName', '$5');
    AddField('szTypeName', '$6');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ShellAPITypes',
    ['Windows']);

  // Types
  TSepiTypeAlias.Create(Result, 'HDROP', TypeInfo(Longint));
  TSepiPointerType.Create(Result, 'PPWideChar', 'PWideChar', True);

  // Types
  TSepiPointerType.Create(Result, 'PDragInfoA', '_DRAGINFOA', True);
  TSepiPointerType.Create(Result, 'PDragInfoW', '_DRAGINFOW', True);
  TSepiTypeAlias.Create(Result, 'PDragInfo', 'PDragInfoA');
  SepiImport_DRAGINFOA(Result);
  TSepiTypeAlias.Create(Result, 'TDragInfoA', '_DRAGINFOA');
  TSepiTypeAlias.Create(Result, 'LPDRAGINFOA', 'PDragInfoA');
  SepiImport_DRAGINFOW(Result);
  TSepiTypeAlias.Create(Result, 'TDragInfoW', '_DRAGINFOW');
  TSepiTypeAlias.Create(Result, 'LPDRAGINFOW', 'PDragInfoW');
  TSepiTypeAlias.Create(Result, '_DRAGINFO', '_DRAGINFOA');

  // Types
  TSepiPointerType.Create(Result, 'PAppBarData', 'TAppBarData', True);
  SepiImport_AppBarData(Result);
  TSepiTypeAlias.Create(Result, 'TAppBarData', '_AppBarData');
  TSepiTypeAlias.Create(Result, 'APPBARDATA', '_AppBarData');

  // Types
  TSepiTypeAlias.Create(Result, 'FILEOP_FLAGS', TypeInfo(Word));

  // Types
  TSepiTypeAlias.Create(Result, 'PRINTEROP_FLAGS', 'Word');
  TSepiPointerType.Create(Result, 'PSHFileOpStructA', 'TSHFileOpStructA', True);
  TSepiPointerType.Create(Result, 'PSHFileOpStructW', 'TSHFileOpStructW', True);
  TSepiTypeAlias.Create(Result, 'PSHFileOpStruct', 'PSHFileOpStructA');
  SepiImport_SHFILEOPSTRUCTA(Result);
  SepiImport_SHFILEOPSTRUCTW(Result);
  TSepiTypeAlias.Create(Result, '_SHFILEOPSTRUCT', '_SHFILEOPSTRUCTA');
  TSepiTypeAlias.Create(Result, 'TSHFileOpStructA', '_SHFILEOPSTRUCTA');
  TSepiTypeAlias.Create(Result, 'TSHFileOpStructW', '_SHFILEOPSTRUCTW');
  TSepiTypeAlias.Create(Result, 'TSHFileOpStruct', 'TSHFileOpStructA');
  TSepiTypeAlias.Create(Result, 'SHFILEOPSTRUCTA', '_SHFILEOPSTRUCTA');
  TSepiTypeAlias.Create(Result, 'SHFILEOPSTRUCTW', '_SHFILEOPSTRUCTW');
  TSepiTypeAlias.Create(Result, 'SHFILEOPSTRUCT', 'SHFILEOPSTRUCTA');

  // Types
  TSepiPointerType.Create(Result, 'PSHNameMappingA', 'TSHNameMappingA', True);
  TSepiPointerType.Create(Result, 'PSHNameMappingW', 'TSHNameMappingW', True);
  TSepiTypeAlias.Create(Result, 'PSHNameMapping', 'PSHNameMappingA');
  SepiImport_SHNAMEMAPPINGA(Result);
  SepiImport_SHNAMEMAPPINGW(Result);
  TSepiTypeAlias.Create(Result, '_SHNAMEMAPPING', '_SHNAMEMAPPINGA');
  TSepiTypeAlias.Create(Result, 'TSHNameMappingA', '_SHNAMEMAPPINGA');
  TSepiTypeAlias.Create(Result, 'TSHNameMappingW', '_SHNAMEMAPPINGW');
  TSepiTypeAlias.Create(Result, 'TSHNameMapping', 'TSHNameMappingA');
  TSepiTypeAlias.Create(Result, 'SHNAMEMAPPINGA', '_SHNAMEMAPPINGA');
  TSepiTypeAlias.Create(Result, 'SHNAMEMAPPINGW', '_SHNAMEMAPPINGW');
  TSepiTypeAlias.Create(Result, 'SHNAMEMAPPING', 'SHNAMEMAPPINGA');

  // Types
  TSepiPointerType.Create(Result, 'PShellExecuteInfoA', 'TShellExecuteInfoA', True);
  TSepiPointerType.Create(Result, 'PShellExecuteInfoW', 'TShellExecuteInfoW', True);
  TSepiTypeAlias.Create(Result, 'PShellExecuteInfo', 'PShellExecuteInfoA');
  SepiImport_SHELLEXECUTEINFOA(Result);
  SepiImport_SHELLEXECUTEINFOW(Result);
  TSepiTypeAlias.Create(Result, '_SHELLEXECUTEINFO', '_SHELLEXECUTEINFOA');
  TSepiTypeAlias.Create(Result, 'TShellExecuteInfoA', '_SHELLEXECUTEINFOA');
  TSepiTypeAlias.Create(Result, 'TShellExecuteInfoW', '_SHELLEXECUTEINFOW');
  TSepiTypeAlias.Create(Result, 'TShellExecuteInfo', 'TShellExecuteInfoA');
  TSepiTypeAlias.Create(Result, 'SHELLEXECUTEINFOA', '_SHELLEXECUTEINFOA');
  TSepiTypeAlias.Create(Result, 'SHELLEXECUTEINFOW', '_SHELLEXECUTEINFOW');
  TSepiTypeAlias.Create(Result, 'SHELLEXECUTEINFO', 'SHELLEXECUTEINFOA');

  // Types
  TSepiPointerType.Create(Result, 'PNotifyIconDataA', 'TNotifyIconDataA', True);
  TSepiPointerType.Create(Result, 'PNotifyIconDataW', 'TNotifyIconDataW', True);
  TSepiTypeAlias.Create(Result, 'PNotifyIconData', 'PNotifyIconDataA');
  TSepiArrayType.Create(Result, '$1',
    [0, 63], TypeInfo(AnsiChar), True);
  SepiImport_NOTIFYICONDATAA(Result);
  TSepiArrayType.Create(Result, '$2',
    [0, 63], TypeInfo(WideChar), True);
  SepiImport_NOTIFYICONDATAW(Result);
  TSepiTypeAlias.Create(Result, '_NOTIFYICONDATA', '_NOTIFYICONDATAA');
  TSepiTypeAlias.Create(Result, 'TNotifyIconDataA', '_NOTIFYICONDATAA');
  TSepiTypeAlias.Create(Result, 'TNotifyIconDataW', '_NOTIFYICONDATAW');
  TSepiTypeAlias.Create(Result, 'TNotifyIconData', 'TNotifyIconDataA');
  TSepiTypeAlias.Create(Result, 'NOTIFYICONDATAA', '_NOTIFYICONDATAA');
  TSepiTypeAlias.Create(Result, 'NOTIFYICONDATAW', '_NOTIFYICONDATAW');
  TSepiTypeAlias.Create(Result, 'NOTIFYICONDATA', 'NOTIFYICONDATAA');

  // Types
  TSepiPointerType.Create(Result, 'PSHFileInfoA', 'TSHFileInfoA', True);
  TSepiPointerType.Create(Result, 'PSHFileInfoW', 'TSHFileInfoW', True);
  TSepiTypeAlias.Create(Result, 'PSHFileInfo', 'PSHFileInfoA');
  TSepiArrayType.Create(Result, '$3',
    [0, MAX_PATH-1], TypeInfo(AnsiChar), True);
  TSepiArrayType.Create(Result, '$4',
    [0, 79], TypeInfo(AnsiChar), True);
  SepiImport_SHFILEINFOA(Result);
  TSepiArrayType.Create(Result, '$5',
    [0, MAX_PATH-1], TypeInfo(WideChar), True);
  TSepiArrayType.Create(Result, '$6',
    [0, 79], TypeInfo(WideChar), True);
  SepiImport_SHFILEINFOW(Result);
  TSepiTypeAlias.Create(Result, '_SHFILEINFO', '_SHFILEINFOA');
  TSepiTypeAlias.Create(Result, 'TSHFileInfoA', '_SHFILEINFOA');
  TSepiTypeAlias.Create(Result, 'TSHFileInfoW', '_SHFILEINFOW');
  TSepiTypeAlias.Create(Result, 'TSHFileInfo', 'TSHFileInfoA');
  TSepiTypeAlias.Create(Result, 'SHFILEINFOA', '_SHFILEINFOA');
  TSepiTypeAlias.Create(Result, 'SHFILEINFOW', '_SHFILEINFOW');
  TSepiTypeAlias.Create(Result, 'SHFILEINFO', 'SHFILEINFOA');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ShellAPITypes', ImportUnit);
end.

