{*
  Importe l'unité MultiMon dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsMultiMonTypes;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, MultiMon;

implementation

{ You must not localize any of the strings this unit contains! }

{------------------------}
{ tagMONITORINFOA import }
{------------------------}

function SepiImporttagMONITORINFOA(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagMONITORINFOA', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('rcMonitor', 'TRect');
    AddField('rcWork', 'TRect');
    AddField('dwFlags', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{------------------------}
{ tagMONITORINFOW import }
{------------------------}

function SepiImporttagMONITORINFOW(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagMONITORINFOW', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('rcMonitor', 'TRect');
    AddField('rcWork', 'TRect');
    AddField('dwFlags', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{--------------------------}
{ tagMONITORINFOEXA import }
{--------------------------}

function SepiImporttagMONITORINFOEXA(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagMONITORINFOEXA', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('rcMonitor', 'TRect');
    AddField('rcWork', 'TRect');
    AddField('dwFlags', System.TypeInfo(DWORD));
    AddField('szDevice', '$1');

    Complete;
  end;
end;

{--------------------------}
{ tagMONITORINFOEXW import }
{--------------------------}

function SepiImporttagMONITORINFOEXW(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagMONITORINFOEXW', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(DWORD));
    AddField('rcMonitor', 'TRect');
    AddField('rcWork', 'TRect');
    AddField('dwFlags', System.TypeInfo(DWORD));
    AddField('szDevice', '$2');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'MultiMonTypes',
    ['Windows']);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(HMONITOR));

  // Types
  SepiImporttagMONITORINFOA(Result);
  SepiImporttagMONITORINFOW(Result);
  TSepiTypeAlias.Create(Result, 'tagMONITORINFO', 'tagMONITORINFOA');
  TSepiTypeAlias.Create(Result, 'MONITORINFOA', 'tagMONITORINFOA');
  TSepiTypeAlias.Create(Result, 'MONITORINFOW', 'tagMONITORINFOW');
  TSepiTypeAlias.Create(Result, 'MONITORINFO', 'MONITORINFOA');
  TSepiPointerType.Create(Result, 'LPMONITORINFOA', 'tagMONITORINFOA', True);
  TSepiPointerType.Create(Result, 'LPMONITORINFOW', 'tagMONITORINFOW', True);
  TSepiTypeAlias.Create(Result, 'LPMONITORINFO', 'LPMONITORINFOA');
  TSepiPointerType.Create(Result, 'PMonitorInfoA', 'tagMONITORINFO', True);
  TSepiPointerType.Create(Result, 'PMonitorInfoW', 'tagMONITORINFO', True);
  TSepiTypeAlias.Create(Result, 'PMonitorInfo', 'PMonitorInfoA');
  TSepiTypeAlias.Create(Result, 'TMonitorInfoA', 'tagMONITORINFO');
  TSepiTypeAlias.Create(Result, 'TMonitorInfoW', 'tagMONITORINFO');
  TSepiTypeAlias.Create(Result, 'TMonitorInfo', 'TMonitorInfoA');
  TSepiArrayType.Create(Result, '$1',
    [0, CCHDEVICENAME], TypeInfo(AnsiChar), True);
  SepiImporttagMONITORINFOEXA(Result);
  TSepiArrayType.Create(Result, '$2',
    [0, CCHDEVICENAME], TypeInfo(WideChar), True);
  SepiImporttagMONITORINFOEXW(Result);
  TSepiTypeAlias.Create(Result, 'tagMONITORINFOEX', 'tagMONITORINFOEXA');
  TSepiTypeAlias.Create(Result, 'MONITORINFOEXA', 'tagMONITORINFOEXA');
  TSepiTypeAlias.Create(Result, 'MONITORINFOEXW', 'tagMONITORINFOEXW');
  TSepiTypeAlias.Create(Result, 'MONITORINFOEX', 'MONITORINFOEXA');
  TSepiPointerType.Create(Result, 'LPMONITORINFOEXA', 'tagMONITORINFOEXA', True);
  TSepiPointerType.Create(Result, 'LPMONITORINFOEXW', 'tagMONITORINFOEXW', True);
  TSepiTypeAlias.Create(Result, 'LPMONITORINFOEX', 'LPMONITORINFOEXA');
  TSepiPointerType.Create(Result, 'PMonitorInfoExA', 'tagMONITORINFOEX', True);
  TSepiPointerType.Create(Result, 'PMonitorInfoExW', 'tagMONITORINFOEX', True);
  TSepiTypeAlias.Create(Result, 'PMonitorInfoEx', 'PMonitorInfoExA');
  TSepiTypeAlias.Create(Result, 'TMonitorInfoExA', 'tagMONITORINFOEX');
  TSepiTypeAlias.Create(Result, 'TMonitorInfoExW', 'tagMONITORINFOEX');
  TSepiTypeAlias.Create(Result, 'TMonitorInfoEx', 'TMonitorInfoExA');
  TSepiMethodRefType.Create(Result, 'TMonitorEnumProc',
    'function(hm: HMONITOR; dc: HDC; r: PRect; l: LPARAM): Boolean', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TGetSystemMetrics',
    'function(nIndex: Integer): Integer', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TMonitorFromWindow',
    'function(hWnd: HWND; dwFlags: DWORD): HMONITOR', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TMonitorFromRect',
    'function(lprcScreenCoords: PRect; dwFlags: DWORD): HMONITOR', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TMonitorFromPoint',
    'function(ptScreenCoords: TPoint; dwFlags: DWORD): HMONITOR', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TGetMonitorInfoA',
    'function(hMonitor: HMONITOR; lpMonitorInfo: PMonitorInfoA): Boolean', False, ccStdCall);
  TSepiMethodRefType.Create(Result, 'TGetMonitorInfoW',
    'function(hMonitor: HMONITOR; lpMonitorInfo: PMonitorInfoW): Boolean', False, ccStdCall);
  TSepiTypeAlias.Create(Result, 'TGetMonitorInfo', 'TGetMonitorInfoA');
  TSepiMethodRefType.Create(Result, 'TEnumDisplayMonitors',
    'function(hdc: HDC; lprcIntersect: PRect; lpfnEnumProc: TMonitorEnumProc; lData: LPARAM ) : Boolean', False, ccStdCall);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('MultiMonTypes', ImportUnit);
end.

