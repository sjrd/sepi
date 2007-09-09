{*
  Importe l'unité Messages dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsMessages;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Messages, Windows;

implementation

{ You must not localize any of the strings this unit contains! }

{-----------------}
{ TMessage import }
{-----------------}

function SepiImportTMessage(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TMessage', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddFieldAfter('WParam', System.TypeInfo(Longint), 'Msg');
    AddFieldAfter('LParam', System.TypeInfo(Longint), 'WParam');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'LParam');
    AddFieldAfter('WParamLo', System.TypeInfo(Word), 'Msg');
    AddFieldAfter('WParamHi', System.TypeInfo(Word), 'WParamLo');
    AddFieldAfter('LParamLo', System.TypeInfo(Word), 'WParamHi');
    AddFieldAfter('LParamHi', System.TypeInfo(Word), 'LParamLo');
    AddFieldAfter('ResultLo', System.TypeInfo(Word), 'LParamHi');
    AddFieldAfter('ResultHi', System.TypeInfo(Word), 'ResultLo');

    Complete;
  end;
end;

{--------------------}
{ TWMNoParams import }
{--------------------}

function SepiImportTWMNoParams(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNoParams', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', '$1');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------}
{ TWMKey import }
{---------------}

function SepiImportTWMKey(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMKey', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('CharCode', System.TypeInfo(Word));
    AddField('Unused', System.TypeInfo(Word));
    AddField('KeyData', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------}
{ TWMMouse import }
{-----------------}

function SepiImportTWMMouse(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMouse', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Keys', System.TypeInfo(Longint));
    AddFieldAfter('XPos', System.TypeInfo(Smallint), 'Keys');
    AddFieldAfter('YPos', System.TypeInfo(Smallint), 'XPos');
    AddFieldAfter('Pos', 'TSmallPoint', 'Keys');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'Pos');

    Complete;
  end;
end;

{----------------------}
{ TWMMouseWheel import }
{----------------------}

function SepiImportTWMMouseWheel(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMouseWheel', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Keys', System.TypeInfo(SmallInt));
    AddField('WheelDelta', System.TypeInfo(SmallInt));
    AddFieldAfter('XPos', System.TypeInfo(Smallint), 'WheelDelta');
    AddFieldAfter('YPos', System.TypeInfo(Smallint), 'XPos');
    AddFieldAfter('Pos', 'TSmallPoint', 'WheelDelta');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'Pos');

    Complete;
  end;
end;

{-----------------------}
{ TMSHMouseWheel import }
{-----------------------}

function SepiImportTMSHMouseWheel(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TMSHMouseWheel', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('WheelDelta', System.TypeInfo(Integer));
    AddFieldAfter('XPos', System.TypeInfo(Smallint), 'WheelDelta');
    AddFieldAfter('YPos', System.TypeInfo(Smallint), 'XPos');
    AddFieldAfter('Pos', 'TSmallPoint', 'WheelDelta');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'Pos');

    Complete;
  end;
end;

{------------------------}
{ TWMWindowPosMsg import }
{------------------------}

function SepiImportTWMWindowPosMsg(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMWindowPosMsg', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('WindowPos', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------}
{ TWMScroll import }
{------------------}

function SepiImportTWMScroll(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMScroll', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('ScrollCode', System.TypeInfo(Smallint));
    AddField('Pos', System.TypeInfo(Smallint));
    AddField('ScrollBar', System.TypeInfo(HWND));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMActivate import }
{--------------------}

function SepiImportTWMActivate(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMActivate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Active', System.TypeInfo(Word));
    AddField('Minimized', System.TypeInfo(WordBool));
    AddField('ActiveWindow', System.TypeInfo(HWND));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMActivateApp import }
{-----------------------}

function SepiImportTWMActivateApp(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMActivateApp', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Active', System.TypeInfo(BOOL));
    AddField('ThreadId', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------------}
{ TWMAskCBFormatName import }
{---------------------------}

function SepiImportTWMAskCBFormatName(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMAskCBFormatName', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('NameLen', System.TypeInfo(Word));
    AddField('Unused', System.TypeInfo(Word));
    AddField('FormatName', 'PChar');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMChangeCBChain import }
{-------------------------}

function SepiImportTWMChangeCBChain(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMChangeCBChain', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Remove', System.TypeInfo(HWND));
    AddField('Next', System.TypeInfo(HWND));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMCharToItem import }
{----------------------}

function SepiImportTWMCharToItem(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMCharToItem', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Key', System.TypeInfo(Word));
    AddField('CaretPos', System.TypeInfo(Word));
    AddField('ListBox', System.TypeInfo(HWND));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------------------}
{ TWMChooseFont_GetLogFont import }
{---------------------------------}

function SepiImportTWMChooseFont_GetLogFont(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMChooseFont_GetLogFont', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('LogFont', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMCommand import }
{-------------------}

function SepiImportTWMCommand(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMCommand', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('ItemID', System.TypeInfo(Word));
    AddField('NotifyCode', System.TypeInfo(Word));
    AddField('Ctl', System.TypeInfo(HWND));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMCompacting import }
{----------------------}

function SepiImportTWMCompacting(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMCompacting', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('CompactRatio', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMCompareItem import }
{-----------------------}

function SepiImportTWMCompareItem(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMCompareItem', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Ctl', System.TypeInfo(HWnd));
    AddField('CompareItemStruct', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMCopyData import }
{--------------------}

function SepiImportTWMCopyData(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMCopyData', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('From', System.TypeInfo(HWND));
    AddField('CopyDataStruct', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------}
{ TWMCreate import }
{------------------}

function SepiImportTWMCreate(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMCreate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('CreateStruct', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMCtlColor import }
{--------------------}

function SepiImportTWMCtlColor(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMCtlColor', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('ChildDC', System.TypeInfo(HDC));
    AddField('ChildWnd', System.TypeInfo(HWND));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMDDE_Ack import }
{-------------------}

function SepiImportTWMDDE_Ack(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDDE_Ack', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PostingApp', System.TypeInfo(HWND));
    AddFieldAfter('App', System.TypeInfo(Word), 'PostingApp');
    AddFieldAfter('Topic', System.TypeInfo(Word), 'App');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'Topic');
    AddFieldAfter('PackedVal', System.TypeInfo(Longint), 'PostingApp');

    Complete;
  end;
end;

{----------------------}
{ TWMDDE_Advise import }
{----------------------}

function SepiImportTWMDDE_Advise(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDDE_Advise', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PostingApp', System.TypeInfo(HWND));
    AddField('PackedVal', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMDDE_Data import }
{--------------------}

function SepiImportTWMDDE_Data(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDDE_Data', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PostingApp', System.TypeInfo(HWND));
    AddField('PackedVal', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMDDE_Execute import }
{-----------------------}

function SepiImportTWMDDE_Execute(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDDE_Execute', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PostingApp', System.TypeInfo(HWND));
    AddField('Commands', System.TypeInfo(THandle));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------}
{ TWMDDE_Initiate import }
{------------------------}

function SepiImportTWMDDE_Initiate(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDDE_Initiate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PostingApp', System.TypeInfo(HWND));
    AddField('App', System.TypeInfo(Word));
    AddField('Topic', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMDDE_Poke import }
{--------------------}

function SepiImportTWMDDE_Poke(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDDE_Poke', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PostingApp', System.TypeInfo(HWND));
    AddField('PackedVal', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMDDE_Request import }
{-----------------------}

function SepiImportTWMDDE_Request(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDDE_Request', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PostingApp', System.TypeInfo(HWND));
    AddField('Format', System.TypeInfo(Word));
    AddField('Item', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMDDE_Terminate import }
{-------------------------}

function SepiImportTWMDDE_Terminate(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDDE_Terminate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PostingApp', System.TypeInfo(HWND));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------}
{ TWMDDE_Unadvise import }
{------------------------}

function SepiImportTWMDDE_Unadvise(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDDE_Unadvise', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PostingApp', System.TypeInfo(HWND));
    AddField('Format', System.TypeInfo(Word));
    AddField('Item', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMDeleteItem import }
{----------------------}

function SepiImportTWMDeleteItem(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDeleteItem', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Ctl', System.TypeInfo(HWND));
    AddField('DeleteItemStruct', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMDevModeChange import }
{-------------------------}

function SepiImportTWMDevModeChange(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDevModeChange', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('Device', 'PChar');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMDrawItem import }
{--------------------}

function SepiImportTWMDrawItem(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDrawItem', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Ctl', System.TypeInfo(HWND));
    AddField('DrawItemStruct', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ TWMDropFiles import }
{---------------------}

function SepiImportTWMDropFiles(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDropFiles', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Drop', System.TypeInfo(THANDLE));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------}
{ TWMEnable import }
{------------------}

function SepiImportTWMEnable(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMEnable', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Enabled', System.TypeInfo(LongBool));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMEndSession import }
{----------------------}

function SepiImportTWMEndSession(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMEndSession', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('EndSession', System.TypeInfo(LongBool));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ TWMEnterIdle import }
{---------------------}

function SepiImportTWMEnterIdle(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMEnterIdle', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Source', System.TypeInfo(Longint));
    AddField('IdleWnd', System.TypeInfo(HWND));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMEnterMenuLoop import }
{-------------------------}

function SepiImportTWMEnterMenuLoop(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMEnterMenuLoop', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('IsTrackPopupMenu', System.TypeInfo(LongBool));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMEraseBkgnd import }
{----------------------}

function SepiImportTWMEraseBkgnd(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMEraseBkgnd', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('DC', System.TypeInfo(HDC));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMGetIcon import }
{-------------------}

function SepiImportTWMGetIcon(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMGetIcon', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('BigIcon', System.TypeInfo(Longbool));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMGetMinMaxInfo import }
{-------------------------}

function SepiImportTWMGetMinMaxInfo(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMGetMinMaxInfo', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('MinMaxInfo', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMGetText import }
{-------------------}

function SepiImportTWMGetText(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMGetText', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('TextMax', System.TypeInfo(Integer));
    AddField('Text', 'PChar');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------}
{ TWMHotKey import }
{------------------}

function SepiImportTWMHotKey(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMHotKey', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('HotKey', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------------}
{ TWMHScrollClipboard import }
{----------------------------}

function SepiImportTWMHScrollClipboard(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMHScrollClipboard', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Viewer', System.TypeInfo(HWND));
    AddField('ScrollCode', System.TypeInfo(Word));
    AddField('Pos', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMInitDialog import }
{----------------------}

function SepiImportTWMInitDialog(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMInitDialog', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Focus', System.TypeInfo(HWND));
    AddField('InitParam', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMInitMenu import }
{--------------------}

function SepiImportTWMInitMenu(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMInitMenu', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Menu', System.TypeInfo(HMENU));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMInitMenuPopup import }
{-------------------------}

function SepiImportTWMInitMenuPopup(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMInitMenuPopup', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('MenuPopup', System.TypeInfo(HMENU));
    AddField('Pos', System.TypeInfo(Smallint));
    AddField('SystemMenu', System.TypeInfo(WordBool));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ TWMKillFocus import }
{---------------------}

function SepiImportTWMKillFocus(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMKillFocus', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('FocusedWnd', System.TypeInfo(HWND));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMMDIActivate import }
{-----------------------}

function SepiImportTWMMDIActivate(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMDIActivate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddFieldAfter('ChildWnd', System.TypeInfo(HWND), 'Msg');
    AddFieldAfter('DeactiveWnd', System.TypeInfo(HWND), 'Msg');
    AddFieldAfter('ActiveWnd', System.TypeInfo(HWND), 'DeactiveWnd');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'ActiveWnd');

    Complete;
  end;
end;

{----------------------}
{ TWMMDICascade import }
{----------------------}

function SepiImportTWMMDICascade(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMDICascade', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Cascade', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ TWMMDICreate import }
{---------------------}

function SepiImportTWMMDICreate(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMDICreate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('MDICreateStruct', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMMDIDestroy import }
{----------------------}

function SepiImportTWMMDIDestroy(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMDIDestroy', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Child', System.TypeInfo(HWND));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMMDIMaximize import }
{-----------------------}

function SepiImportTWMMDIMaximize(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMDIMaximize', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Maximize', System.TypeInfo(HWND));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMMDINext import }
{-------------------}

function SepiImportTWMMDINext(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMDINext', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Child', System.TypeInfo(HWND));
    AddField('Next', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMMDIRestore import }
{----------------------}

function SepiImportTWMMDIRestore(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMDIRestore', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('IDChild', System.TypeInfo(HWND));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMMDISetMenu import }
{----------------------}

function SepiImportTWMMDISetMenu(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMDISetMenu', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('MenuFrame', System.TypeInfo(HMENU));
    AddField('MenuWindow', System.TypeInfo(HMENU));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMMDITile import }
{-------------------}

function SepiImportTWMMDITile(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMDITile', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Tile', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMMeasureItem import }
{-----------------------}

function SepiImportTWMMeasureItem(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMeasureItem', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('IDCtl', System.TypeInfo(HWnd));
    AddField('MeasureItemStruct', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMMenuChar import }
{--------------------}

function SepiImportTWMMenuChar(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMenuChar', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('User', System.TypeInfo(Char));
    AddField('Unused', System.TypeInfo(Byte));
    AddField('MenuFlag', System.TypeInfo(Word));
    AddField('Menu', System.TypeInfo(HMENU));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMMenuSelect import }
{----------------------}

function SepiImportTWMMenuSelect(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMenuSelect', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('IDItem', System.TypeInfo(Word));
    AddField('MenuFlag', System.TypeInfo(Word));
    AddField('Menu', System.TypeInfo(HMENU));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMMouseActivate import }
{-------------------------}

function SepiImportTWMMouseActivate(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMouseActivate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('TopLevel', System.TypeInfo(HWND));
    AddField('HitTestCode', System.TypeInfo(Word));
    AddField('MouseMsg', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------}
{ TWMMove import }
{----------------}

function SepiImportTWMMove(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMove', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddFieldAfter('XPos', System.TypeInfo(Smallint), 'Unused');
    AddFieldAfter('YPos', System.TypeInfo(Smallint), 'XPos');
    AddFieldAfter('Pos', 'TSmallPoint', 'Unused');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'Pos');

    Complete;
  end;
end;

{------------------}
{ TWMMoving import }
{------------------}

function SepiImportTWMMoving(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMMoving', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Edge', System.TypeInfo(Integer));
    AddField('DragRect', 'PRect');

    Complete;
  end;
end;

{----------------------}
{ TWMNCActivate import }
{----------------------}

function SepiImportTWMNCActivate(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNCActivate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Active', System.TypeInfo(BOOL));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMNCCalcSize import }
{----------------------}

function SepiImportTWMNCCalcSize(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNCCalcSize', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('CalcValidRects', System.TypeInfo(BOOL));
    AddField('CalcSize_Params', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMNCCreate import }
{--------------------}

function SepiImportTWMNCCreate(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNCCreate', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('CreateStruct', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ TWMNCHitTest import }
{---------------------}

function SepiImportTWMNCHitTest(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNCHitTest', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Longint));
    AddFieldAfter('XPos', System.TypeInfo(Smallint), 'Unused');
    AddFieldAfter('YPos', System.TypeInfo(Smallint), 'XPos');
    AddFieldAfter('Pos', 'TSmallPoint', 'Unused');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'Pos');

    Complete;
  end;
end;

{------------------------}
{ TWMNCHitMessage import }
{------------------------}

function SepiImportTWMNCHitMessage(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNCHitMessage', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('HitTest', System.TypeInfo(Longint));
    AddField('XCursor', System.TypeInfo(Smallint));
    AddField('YCursor', System.TypeInfo(Smallint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMNCPaint import }
{-------------------}

function SepiImportTWMNCPaint(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNCPaint', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('RGN', System.TypeInfo(HRGN));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMNextDlgCtl import }
{----------------------}

function SepiImportTWMNextDlgCtl(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNextDlgCtl', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('CtlFocus', System.TypeInfo(Longint));
    AddField('Handle', System.TypeInfo(WordBool));
    AddField('Unused', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------}
{ TWMNotify import }
{------------------}

function SepiImportTWMNotify(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNotify', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('IDCtrl', System.TypeInfo(Longint));
    AddField('NMHdr', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------}
{ TWMNotifyFormat import }
{------------------------}

function SepiImportTWMNotifyFormat(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMNotifyFormat', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('From', System.TypeInfo(HWND));
    AddField('Command', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------}
{ TWMPaint import }
{-----------------}

function SepiImportTWMPaint(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMPaint', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('DC', System.TypeInfo(HDC));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------------}
{ TWMPaintClipboard import }
{--------------------------}

function SepiImportTWMPaintClipboard(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMPaintClipboard', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Viewer', System.TypeInfo(HWND));
    AddField('PaintStruct', System.TypeInfo(THandle));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------------}
{ TWMPaletteChanged import }
{--------------------------}

function SepiImportTWMPaletteChanged(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMPaletteChanged', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PalChg', System.TypeInfo(HWND));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------------}
{ TWMPaletteIsChanging import }
{-----------------------------}

function SepiImportTWMPaletteIsChanging(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMPaletteIsChanging', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Realize', System.TypeInfo(HWND));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------}
{ TWMParentNotify import }
{------------------------}

function SepiImportTWMParentNotify(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMParentNotify', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddFieldAfter('ChildID', System.TypeInfo(Word), 'Msg');
    AddFieldAfter('ChildWnd', System.TypeInfo(HWnd), 'ChildID');
    AddFieldAfter('Value', System.TypeInfo(Word), 'Msg');
    AddFieldAfter('XPos', System.TypeInfo(Smallint), 'Value');
    AddFieldAfter('YPos', System.TypeInfo(Smallint), 'XPos');
    AddFieldAfter('Value1', System.TypeInfo(Word), 'Msg');
    AddFieldAfter('Value2', System.TypeInfo(Longint), 'Value1');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'Value2');

    Complete;
  end;
end;

{-----------------}
{ TWMPower import }
{-----------------}

function SepiImportTWMPower(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMPower', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('PowerEvt', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------------}
{ TWMQueryEndSession import }
{---------------------------}

function SepiImportTWMQueryEndSession(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMQueryEndSession', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Source', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------}
{ TWMQuit import }
{----------------}

function SepiImportTWMQuit(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMQuit', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('ExitCode', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------}
{ TWMRenderFormat import }
{------------------------}

function SepiImportTWMRenderFormat(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMRenderFormat', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Format', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ TWMSetCursor import }
{---------------------}

function SepiImportTWMSetCursor(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSetCursor', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('CursorWnd', System.TypeInfo(HWND));
    AddField('HitTest', System.TypeInfo(Word));
    AddField('MouseMsg', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TWMSetFocus import }
{--------------------}

function SepiImportTWMSetFocus(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSetFocus', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('FocusedWnd', System.TypeInfo(HWND));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMSetFont import }
{-------------------}

function SepiImportTWMSetFont(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSetFont', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Font', System.TypeInfo(HFONT));
    AddField('Redraw', System.TypeInfo(WordBool));
    AddField('Unused', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ TWMSetHotKey import }
{---------------------}

function SepiImportTWMSetHotKey(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSetHotKey', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Key', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMSetIcon import }
{-------------------}

function SepiImportTWMSetIcon(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSetIcon', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('BigIcon', System.TypeInfo(Longbool));
    AddField('Icon', System.TypeInfo(HICON));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{---------------------}
{ TWMSetRedraw import }
{---------------------}

function SepiImportTWMSetRedraw(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSetRedraw', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Redraw', System.TypeInfo(Longint));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMSetText import }
{-------------------}

function SepiImportTWMSetText(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSetText', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Text', 'PChar');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMShowWindow import }
{----------------------}

function SepiImportTWMShowWindow(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMShowWindow', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Show', System.TypeInfo(BOOL));
    AddField('Status', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------}
{ TWMSize import }
{----------------}

function SepiImportTWMSize(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSize', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('SizeType', System.TypeInfo(Longint));
    AddField('Width', System.TypeInfo(Word));
    AddField('Height', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMSizeClipboard import }
{-------------------------}

function SepiImportTWMSizeClipboard(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSizeClipboard', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Viewer', System.TypeInfo(HWND));
    AddField('RC', System.TypeInfo(THandle));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMSpoolerStatus import }
{-------------------------}

function SepiImportTWMSpoolerStatus(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSpoolerStatus', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('JobStatus', System.TypeInfo(Longint));
    AddField('JobsLeft', System.TypeInfo(Word));
    AddField('Unused', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMStyleChange import }
{-----------------------}

function SepiImportTWMStyleChange(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMStyleChange', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('StyleType', System.TypeInfo(Longint));
    AddField('StyleStruct', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ TWMSysCommand import }
{----------------------}

function SepiImportTWMSysCommand(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSysCommand', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddFieldAfter('ActivateWnd', System.TypeInfo(HWND), 'Msg');
    AddFieldAfter('Key', System.TypeInfo(Word), 'Msg');
    AddFieldAfter('XPos', System.TypeInfo(Smallint), 'Msg');
    AddFieldAfter('YPos', System.TypeInfo(Smallint), 'XPos');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'YPos');

    Complete;
  end;
end;

{-----------------------}
{ TWMSysDeadChar import }
{-----------------------}

function SepiImportTWMSysDeadChar(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSysDeadChar', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('CharCode', System.TypeInfo(Word));
    AddField('Unused', System.TypeInfo(Word));
    AddField('KeyData', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMSystemError import }
{-----------------------}

function SepiImportTWMSystemError(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSystemError', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('ErrSpec', System.TypeInfo(Word));
    AddField('Unused', System.TypeInfo(Longint));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------}
{ TWMTimer import }
{-----------------}

function SepiImportTWMTimer(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMTimer', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('TimerID', System.TypeInfo(Longint));
    AddField('TimerProc', 'TFarProc');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TWMUIState import }
{-------------------}

function SepiImportTWMUIState(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMUIState', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Action', System.TypeInfo(Word));
    AddField('Flags', System.TypeInfo(Word));
    AddField('Unused', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------------}
{ TWMVScrollClipboard import }
{----------------------------}

function SepiImportTWMVScrollClipboard(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMVScrollClipboard', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Viewer', System.TypeInfo(HWND));
    AddField('ScollCode', System.TypeInfo(Word));
    AddField('ThumbPos', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------}
{ TWMWinIniChange import }
{------------------------}

function SepiImportTWMWinIniChange(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMWinIniChange', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('Section', 'PChar');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMSettingChange import }
{-------------------------}

function SepiImportTWMSettingChange(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMSettingChange', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Flag', System.TypeInfo(Integer));
    AddField('Section', 'PChar');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------}
{ TWMHelp import }
{----------------}

function SepiImportTWMHelp(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMHelp', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('Unused', System.TypeInfo(Integer));
    AddField('HelpInfo', 'Pointer');
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------------}
{ TWMDisplayChange import }
{-------------------------}

function SepiImportTWMDisplayChange(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMDisplayChange', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('BitsPerPixel', System.TypeInfo(Integer));
    AddField('Width', System.TypeInfo(Word));
    AddField('Height', System.TypeInfo(Word));
    AddField('Result', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-----------------------}
{ TWMContextMenu import }
{-----------------------}

function SepiImportTWMContextMenu(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMContextMenu', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('hWnd', System.TypeInfo(HWND));
    AddFieldAfter('XPos', System.TypeInfo(Smallint), 'hWnd');
    AddFieldAfter('YPos', System.TypeInfo(Smallint), 'XPos');
    AddFieldAfter('Pos', 'TSmallPoint', 'hWnd');
    AddFieldAfter('Result', System.TypeInfo(Longint), 'Pos');

    Complete;
  end;
end;

{-----------------}
{ TWMPrint import }
{-----------------}

function SepiImportTWMPrint(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TWMPrint', True, True);

  with Result do
  begin
    AddField('Msg', System.TypeInfo(Cardinal));
    AddField('DC', System.TypeInfo(HDC));
    AddField('Flags', System.TypeInfo(Cardinal));
    AddField('Result', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'Messages',
    ['Windows']);

  // Constants
  TSepiConstant.Create(Result, 'WM_NULL', WM_NULL);
  TSepiConstant.Create(Result, 'WM_CREATE', WM_CREATE);
  TSepiConstant.Create(Result, 'WM_DESTROY', WM_DESTROY);
  TSepiConstant.Create(Result, 'WM_MOVE', WM_MOVE);
  TSepiConstant.Create(Result, 'WM_SIZE', WM_SIZE);
  TSepiConstant.Create(Result, 'WM_ACTIVATE', WM_ACTIVATE);
  TSepiConstant.Create(Result, 'WM_SETFOCUS', WM_SETFOCUS);
  TSepiConstant.Create(Result, 'WM_KILLFOCUS', WM_KILLFOCUS);
  TSepiConstant.Create(Result, 'WM_ENABLE', WM_ENABLE);
  TSepiConstant.Create(Result, 'WM_SETREDRAW', WM_SETREDRAW);
  TSepiConstant.Create(Result, 'WM_SETTEXT', WM_SETTEXT);
  TSepiConstant.Create(Result, 'WM_GETTEXT', WM_GETTEXT);
  TSepiConstant.Create(Result, 'WM_GETTEXTLENGTH', WM_GETTEXTLENGTH);
  TSepiConstant.Create(Result, 'WM_PAINT', WM_PAINT);
  TSepiConstant.Create(Result, 'WM_CLOSE', WM_CLOSE);
  TSepiConstant.Create(Result, 'WM_QUERYENDSESSION', WM_QUERYENDSESSION);
  TSepiConstant.Create(Result, 'WM_QUIT', WM_QUIT);
  TSepiConstant.Create(Result, 'WM_QUERYOPEN', WM_QUERYOPEN);
  TSepiConstant.Create(Result, 'WM_ERASEBKGND', WM_ERASEBKGND);
  TSepiConstant.Create(Result, 'WM_SYSCOLORCHANGE', WM_SYSCOLORCHANGE);
  TSepiConstant.Create(Result, 'WM_ENDSESSION', WM_ENDSESSION);
  TSepiConstant.Create(Result, 'WM_SYSTEMERROR', WM_SYSTEMERROR);
  TSepiConstant.Create(Result, 'WM_SHOWWINDOW', WM_SHOWWINDOW);
  TSepiConstant.Create(Result, 'WM_CTLCOLOR', WM_CTLCOLOR);
  TSepiConstant.Create(Result, 'WM_WININICHANGE', WM_WININICHANGE);
  TSepiConstant.Create(Result, 'WM_SETTINGCHANGE', WM_SETTINGCHANGE);
  TSepiConstant.Create(Result, 'WM_DEVMODECHANGE', WM_DEVMODECHANGE);
  TSepiConstant.Create(Result, 'WM_ACTIVATEAPP', WM_ACTIVATEAPP);
  TSepiConstant.Create(Result, 'WM_FONTCHANGE', WM_FONTCHANGE);
  TSepiConstant.Create(Result, 'WM_TIMECHANGE', WM_TIMECHANGE);
  TSepiConstant.Create(Result, 'WM_CANCELMODE', WM_CANCELMODE);
  TSepiConstant.Create(Result, 'WM_SETCURSOR', WM_SETCURSOR);
  TSepiConstant.Create(Result, 'WM_MOUSEACTIVATE', WM_MOUSEACTIVATE);
  TSepiConstant.Create(Result, 'WM_CHILDACTIVATE', WM_CHILDACTIVATE);
  TSepiConstant.Create(Result, 'WM_QUEUESYNC', WM_QUEUESYNC);
  TSepiConstant.Create(Result, 'WM_GETMINMAXINFO', WM_GETMINMAXINFO);
  TSepiConstant.Create(Result, 'WM_PAINTICON', WM_PAINTICON);
  TSepiConstant.Create(Result, 'WM_ICONERASEBKGND', WM_ICONERASEBKGND);
  TSepiConstant.Create(Result, 'WM_NEXTDLGCTL', WM_NEXTDLGCTL);
  TSepiConstant.Create(Result, 'WM_SPOOLERSTATUS', WM_SPOOLERSTATUS);
  TSepiConstant.Create(Result, 'WM_DRAWITEM', WM_DRAWITEM);
  TSepiConstant.Create(Result, 'WM_MEASUREITEM', WM_MEASUREITEM);
  TSepiConstant.Create(Result, 'WM_DELETEITEM', WM_DELETEITEM);
  TSepiConstant.Create(Result, 'WM_VKEYTOITEM', WM_VKEYTOITEM);
  TSepiConstant.Create(Result, 'WM_CHARTOITEM', WM_CHARTOITEM);
  TSepiConstant.Create(Result, 'WM_SETFONT', WM_SETFONT);
  TSepiConstant.Create(Result, 'WM_GETFONT', WM_GETFONT);
  TSepiConstant.Create(Result, 'WM_SETHOTKEY', WM_SETHOTKEY);
  TSepiConstant.Create(Result, 'WM_GETHOTKEY', WM_GETHOTKEY);
  TSepiConstant.Create(Result, 'WM_QUERYDRAGICON', WM_QUERYDRAGICON);
  TSepiConstant.Create(Result, 'WM_COMPAREITEM', WM_COMPAREITEM);
  TSepiConstant.Create(Result, 'WM_GETOBJECT', WM_GETOBJECT);
  TSepiConstant.Create(Result, 'WM_COMPACTING', WM_COMPACTING);
  TSepiConstant.Create(Result, 'WM_COMMNOTIFY', WM_COMMNOTIFY);
  TSepiConstant.Create(Result, 'WM_WINDOWPOSCHANGING', WM_WINDOWPOSCHANGING);
  TSepiConstant.Create(Result, 'WM_WINDOWPOSCHANGED', WM_WINDOWPOSCHANGED);
  TSepiConstant.Create(Result, 'WM_POWER', WM_POWER);
  TSepiConstant.Create(Result, 'WM_COPYDATA', WM_COPYDATA);
  TSepiConstant.Create(Result, 'WM_CANCELJOURNAL', WM_CANCELJOURNAL);
  TSepiConstant.Create(Result, 'WM_NOTIFY', WM_NOTIFY);
  TSepiConstant.Create(Result, 'WM_INPUTLANGCHANGEREQUEST', WM_INPUTLANGCHANGEREQUEST);
  TSepiConstant.Create(Result, 'WM_INPUTLANGCHANGE', WM_INPUTLANGCHANGE);
  TSepiConstant.Create(Result, 'WM_TCARD', WM_TCARD);
  TSepiConstant.Create(Result, 'WM_HELP', WM_HELP);
  TSepiConstant.Create(Result, 'WM_USERCHANGED', WM_USERCHANGED);
  TSepiConstant.Create(Result, 'WM_NOTIFYFORMAT', WM_NOTIFYFORMAT);
  TSepiConstant.Create(Result, 'WM_CONTEXTMENU', WM_CONTEXTMENU);
  TSepiConstant.Create(Result, 'WM_STYLECHANGING', WM_STYLECHANGING);
  TSepiConstant.Create(Result, 'WM_STYLECHANGED', WM_STYLECHANGED);
  TSepiConstant.Create(Result, 'WM_DISPLAYCHANGE', WM_DISPLAYCHANGE);
  TSepiConstant.Create(Result, 'WM_GETICON', WM_GETICON);
  TSepiConstant.Create(Result, 'WM_SETICON', WM_SETICON);
  TSepiConstant.Create(Result, 'WM_NCCREATE', WM_NCCREATE);
  TSepiConstant.Create(Result, 'WM_NCDESTROY', WM_NCDESTROY);
  TSepiConstant.Create(Result, 'WM_NCCALCSIZE', WM_NCCALCSIZE);
  TSepiConstant.Create(Result, 'WM_NCHITTEST', WM_NCHITTEST);
  TSepiConstant.Create(Result, 'WM_NCPAINT', WM_NCPAINT);
  TSepiConstant.Create(Result, 'WM_NCACTIVATE', WM_NCACTIVATE);
  TSepiConstant.Create(Result, 'WM_GETDLGCODE', WM_GETDLGCODE);
  TSepiConstant.Create(Result, 'WM_NCMOUSEMOVE', WM_NCMOUSEMOVE);
  TSepiConstant.Create(Result, 'WM_NCLBUTTONDOWN', WM_NCLBUTTONDOWN);
  TSepiConstant.Create(Result, 'WM_NCLBUTTONUP', WM_NCLBUTTONUP);
  TSepiConstant.Create(Result, 'WM_NCLBUTTONDBLCLK', WM_NCLBUTTONDBLCLK);
  TSepiConstant.Create(Result, 'WM_NCRBUTTONDOWN', WM_NCRBUTTONDOWN);
  TSepiConstant.Create(Result, 'WM_NCRBUTTONUP', WM_NCRBUTTONUP);
  TSepiConstant.Create(Result, 'WM_NCRBUTTONDBLCLK', WM_NCRBUTTONDBLCLK);
  TSepiConstant.Create(Result, 'WM_NCMBUTTONDOWN', WM_NCMBUTTONDOWN);
  TSepiConstant.Create(Result, 'WM_NCMBUTTONUP', WM_NCMBUTTONUP);
  TSepiConstant.Create(Result, 'WM_NCMBUTTONDBLCLK', WM_NCMBUTTONDBLCLK);
  TSepiConstant.Create(Result, 'WM_NCXBUTTONDOWN', WM_NCXBUTTONDOWN);
  TSepiConstant.Create(Result, 'WM_NCXBUTTONUP', WM_NCXBUTTONUP);
  TSepiConstant.Create(Result, 'WM_NCXBUTTONDBLCLK', WM_NCXBUTTONDBLCLK);
  TSepiConstant.Create(Result, 'WM_INPUT', WM_INPUT);
  TSepiConstant.Create(Result, 'WM_KEYFIRST', WM_KEYFIRST);
  TSepiConstant.Create(Result, 'WM_KEYDOWN', WM_KEYDOWN);
  TSepiConstant.Create(Result, 'WM_KEYUP', WM_KEYUP);
  TSepiConstant.Create(Result, 'WM_CHAR', WM_CHAR);
  TSepiConstant.Create(Result, 'WM_DEADCHAR', WM_DEADCHAR);
  TSepiConstant.Create(Result, 'WM_SYSKEYDOWN', WM_SYSKEYDOWN);
  TSepiConstant.Create(Result, 'WM_SYSKEYUP', WM_SYSKEYUP);
  TSepiConstant.Create(Result, 'WM_SYSCHAR', WM_SYSCHAR);
  TSepiConstant.Create(Result, 'WM_SYSDEADCHAR', WM_SYSDEADCHAR);
  TSepiConstant.Create(Result, 'WM_UNICHAR', WM_UNICHAR);
  TSepiConstant.Create(Result, 'WM_KEYLAST', WM_KEYLAST);
  TSepiConstant.Create(Result, 'WM_INITDIALOG', WM_INITDIALOG);
  TSepiConstant.Create(Result, 'WM_COMMAND', WM_COMMAND);
  TSepiConstant.Create(Result, 'WM_SYSCOMMAND', WM_SYSCOMMAND);
  TSepiConstant.Create(Result, 'WM_TIMER', WM_TIMER);
  TSepiConstant.Create(Result, 'WM_HSCROLL', WM_HSCROLL);
  TSepiConstant.Create(Result, 'WM_VSCROLL', WM_VSCROLL);
  TSepiConstant.Create(Result, 'WM_INITMENU', WM_INITMENU);
  TSepiConstant.Create(Result, 'WM_INITMENUPOPUP', WM_INITMENUPOPUP);
  TSepiConstant.Create(Result, 'WM_MENUSELECT', WM_MENUSELECT);
  TSepiConstant.Create(Result, 'WM_MENUCHAR', WM_MENUCHAR);
  TSepiConstant.Create(Result, 'WM_ENTERIDLE', WM_ENTERIDLE);
  TSepiConstant.Create(Result, 'WM_MENURBUTTONUP', WM_MENURBUTTONUP);
  TSepiConstant.Create(Result, 'WM_MENUDRAG', WM_MENUDRAG);
  TSepiConstant.Create(Result, 'WM_MENUGETOBJECT', WM_MENUGETOBJECT);
  TSepiConstant.Create(Result, 'WM_UNINITMENUPOPUP', WM_UNINITMENUPOPUP);
  TSepiConstant.Create(Result, 'WM_MENUCOMMAND', WM_MENUCOMMAND);
  TSepiConstant.Create(Result, 'WM_CHANGEUISTATE', WM_CHANGEUISTATE);
  TSepiConstant.Create(Result, 'WM_UPDATEUISTATE', WM_UPDATEUISTATE);
  TSepiConstant.Create(Result, 'WM_QUERYUISTATE', WM_QUERYUISTATE);
  TSepiConstant.Create(Result, 'WM_CTLCOLORMSGBOX', WM_CTLCOLORMSGBOX);
  TSepiConstant.Create(Result, 'WM_CTLCOLOREDIT', WM_CTLCOLOREDIT);
  TSepiConstant.Create(Result, 'WM_CTLCOLORLISTBOX', WM_CTLCOLORLISTBOX);
  TSepiConstant.Create(Result, 'WM_CTLCOLORBTN', WM_CTLCOLORBTN);
  TSepiConstant.Create(Result, 'WM_CTLCOLORDLG', WM_CTLCOLORDLG);
  TSepiConstant.Create(Result, 'WM_CTLCOLORSCROLLBAR', WM_CTLCOLORSCROLLBAR);
  TSepiConstant.Create(Result, 'WM_CTLCOLORSTATIC', WM_CTLCOLORSTATIC);
  TSepiConstant.Create(Result, 'WM_MOUSEFIRST', WM_MOUSEFIRST);
  TSepiConstant.Create(Result, 'WM_MOUSEMOVE', WM_MOUSEMOVE);
  TSepiConstant.Create(Result, 'WM_LBUTTONDOWN', WM_LBUTTONDOWN);
  TSepiConstant.Create(Result, 'WM_LBUTTONUP', WM_LBUTTONUP);
  TSepiConstant.Create(Result, 'WM_LBUTTONDBLCLK', WM_LBUTTONDBLCLK);
  TSepiConstant.Create(Result, 'WM_RBUTTONDOWN', WM_RBUTTONDOWN);
  TSepiConstant.Create(Result, 'WM_RBUTTONUP', WM_RBUTTONUP);
  TSepiConstant.Create(Result, 'WM_RBUTTONDBLCLK', WM_RBUTTONDBLCLK);
  TSepiConstant.Create(Result, 'WM_MBUTTONDOWN', WM_MBUTTONDOWN);
  TSepiConstant.Create(Result, 'WM_MBUTTONUP', WM_MBUTTONUP);
  TSepiConstant.Create(Result, 'WM_MBUTTONDBLCLK', WM_MBUTTONDBLCLK);
  TSepiConstant.Create(Result, 'WM_MOUSEWHEEL', WM_MOUSEWHEEL);
  TSepiConstant.Create(Result, 'WM_MOUSELAST', WM_MOUSELAST);
  TSepiConstant.Create(Result, 'WM_PARENTNOTIFY', WM_PARENTNOTIFY);
  TSepiConstant.Create(Result, 'WM_ENTERMENULOOP', WM_ENTERMENULOOP);
  TSepiConstant.Create(Result, 'WM_EXITMENULOOP', WM_EXITMENULOOP);
  TSepiConstant.Create(Result, 'WM_NEXTMENU', WM_NEXTMENU);
  TSepiConstant.Create(Result, 'WM_SIZING', WM_SIZING);
  TSepiConstant.Create(Result, 'WM_CAPTURECHANGED', WM_CAPTURECHANGED);
  TSepiConstant.Create(Result, 'WM_MOVING', WM_MOVING);
  TSepiConstant.Create(Result, 'WM_POWERBROADCAST', WM_POWERBROADCAST);
  TSepiConstant.Create(Result, 'WM_DEVICECHANGE', WM_DEVICECHANGE);
  TSepiConstant.Create(Result, 'WM_IME_STARTCOMPOSITION', WM_IME_STARTCOMPOSITION);
  TSepiConstant.Create(Result, 'WM_IME_ENDCOMPOSITION', WM_IME_ENDCOMPOSITION);
  TSepiConstant.Create(Result, 'WM_IME_COMPOSITION', WM_IME_COMPOSITION);
  TSepiConstant.Create(Result, 'WM_IME_KEYLAST', WM_IME_KEYLAST);
  TSepiConstant.Create(Result, 'WM_IME_SETCONTEXT', WM_IME_SETCONTEXT);
  TSepiConstant.Create(Result, 'WM_IME_NOTIFY', WM_IME_NOTIFY);
  TSepiConstant.Create(Result, 'WM_IME_CONTROL', WM_IME_CONTROL);
  TSepiConstant.Create(Result, 'WM_IME_COMPOSITIONFULL', WM_IME_COMPOSITIONFULL);
  TSepiConstant.Create(Result, 'WM_IME_SELECT', WM_IME_SELECT);
  TSepiConstant.Create(Result, 'WM_IME_CHAR', WM_IME_CHAR);
  TSepiConstant.Create(Result, 'WM_IME_REQUEST', WM_IME_REQUEST);
  TSepiConstant.Create(Result, 'WM_IME_KEYDOWN', WM_IME_KEYDOWN);
  TSepiConstant.Create(Result, 'WM_IME_KEYUP', WM_IME_KEYUP);
  TSepiConstant.Create(Result, 'WM_MDICREATE', WM_MDICREATE);
  TSepiConstant.Create(Result, 'WM_MDIDESTROY', WM_MDIDESTROY);
  TSepiConstant.Create(Result, 'WM_MDIACTIVATE', WM_MDIACTIVATE);
  TSepiConstant.Create(Result, 'WM_MDIRESTORE', WM_MDIRESTORE);
  TSepiConstant.Create(Result, 'WM_MDINEXT', WM_MDINEXT);
  TSepiConstant.Create(Result, 'WM_MDIMAXIMIZE', WM_MDIMAXIMIZE);
  TSepiConstant.Create(Result, 'WM_MDITILE', WM_MDITILE);
  TSepiConstant.Create(Result, 'WM_MDICASCADE', WM_MDICASCADE);
  TSepiConstant.Create(Result, 'WM_MDIICONARRANGE', WM_MDIICONARRANGE);
  TSepiConstant.Create(Result, 'WM_MDIGETACTIVE', WM_MDIGETACTIVE);
  TSepiConstant.Create(Result, 'WM_MDISETMENU', WM_MDISETMENU);
  TSepiConstant.Create(Result, 'WM_ENTERSIZEMOVE', WM_ENTERSIZEMOVE);
  TSepiConstant.Create(Result, 'WM_EXITSIZEMOVE', WM_EXITSIZEMOVE);
  TSepiConstant.Create(Result, 'WM_DROPFILES', WM_DROPFILES);
  TSepiConstant.Create(Result, 'WM_MDIREFRESHMENU', WM_MDIREFRESHMENU);
  TSepiConstant.Create(Result, 'WM_MOUSEHOVER', WM_MOUSEHOVER);
  TSepiConstant.Create(Result, 'WM_MOUSELEAVE', WM_MOUSELEAVE);
  TSepiConstant.Create(Result, 'WM_NCMOUSEHOVER', WM_NCMOUSEHOVER);
  TSepiConstant.Create(Result, 'WM_NCMOUSELEAVE', WM_NCMOUSELEAVE);
  TSepiConstant.Create(Result, 'WM_WTSSESSION_CHANGE', WM_WTSSESSION_CHANGE);
  TSepiConstant.Create(Result, 'WM_TABLET_FIRST', WM_TABLET_FIRST);
  TSepiConstant.Create(Result, 'WM_TABLET_LAST', WM_TABLET_LAST);
  TSepiConstant.Create(Result, 'WM_CUT', WM_CUT);
  TSepiConstant.Create(Result, 'WM_COPY', WM_COPY);
  TSepiConstant.Create(Result, 'WM_PASTE', WM_PASTE);
  TSepiConstant.Create(Result, 'WM_CLEAR', WM_CLEAR);
  TSepiConstant.Create(Result, 'WM_UNDO', WM_UNDO);
  TSepiConstant.Create(Result, 'WM_RENDERFORMAT', WM_RENDERFORMAT);
  TSepiConstant.Create(Result, 'WM_RENDERALLFORMATS', WM_RENDERALLFORMATS);
  TSepiConstant.Create(Result, 'WM_DESTROYCLIPBOARD', WM_DESTROYCLIPBOARD);
  TSepiConstant.Create(Result, 'WM_DRAWCLIPBOARD', WM_DRAWCLIPBOARD);
  TSepiConstant.Create(Result, 'WM_PAINTCLIPBOARD', WM_PAINTCLIPBOARD);
  TSepiConstant.Create(Result, 'WM_VSCROLLCLIPBOARD', WM_VSCROLLCLIPBOARD);
  TSepiConstant.Create(Result, 'WM_SIZECLIPBOARD', WM_SIZECLIPBOARD);
  TSepiConstant.Create(Result, 'WM_ASKCBFORMATNAME', WM_ASKCBFORMATNAME);
  TSepiConstant.Create(Result, 'WM_CHANGECBCHAIN', WM_CHANGECBCHAIN);
  TSepiConstant.Create(Result, 'WM_HSCROLLCLIPBOARD', WM_HSCROLLCLIPBOARD);
  TSepiConstant.Create(Result, 'WM_QUERYNEWPALETTE', WM_QUERYNEWPALETTE);
  TSepiConstant.Create(Result, 'WM_PALETTEISCHANGING', WM_PALETTEISCHANGING);
  TSepiConstant.Create(Result, 'WM_PALETTECHANGED', WM_PALETTECHANGED);
  TSepiConstant.Create(Result, 'WM_HOTKEY', WM_HOTKEY);
  TSepiConstant.Create(Result, 'WM_PRINT', WM_PRINT);
  TSepiConstant.Create(Result, 'WM_PRINTCLIENT', WM_PRINTCLIENT);
  TSepiConstant.Create(Result, 'WM_APPCOMMAND', WM_APPCOMMAND);
  TSepiConstant.Create(Result, 'WM_THEMECHANGED', WM_THEMECHANGED);
  TSepiConstant.Create(Result, 'WM_HANDHELDFIRST', WM_HANDHELDFIRST);
  TSepiConstant.Create(Result, 'WM_HANDHELDLAST', WM_HANDHELDLAST);
  TSepiConstant.Create(Result, 'WM_PENWINFIRST', WM_PENWINFIRST);
  TSepiConstant.Create(Result, 'WM_PENWINLAST', WM_PENWINLAST);
  TSepiConstant.Create(Result, 'WM_COALESCE_FIRST', WM_COALESCE_FIRST);
  TSepiConstant.Create(Result, 'WM_COALESCE_LAST', WM_COALESCE_LAST);
  TSepiConstant.Create(Result, 'WM_DDE_FIRST', WM_DDE_FIRST);
  TSepiConstant.Create(Result, 'WM_DDE_INITIATE', WM_DDE_INITIATE);
  TSepiConstant.Create(Result, 'WM_DDE_TERMINATE', WM_DDE_TERMINATE);
  TSepiConstant.Create(Result, 'WM_DDE_ADVISE', WM_DDE_ADVISE);
  TSepiConstant.Create(Result, 'WM_DDE_UNADVISE', WM_DDE_UNADVISE);
  TSepiConstant.Create(Result, 'WM_DDE_ACK', WM_DDE_ACK);
  TSepiConstant.Create(Result, 'WM_DDE_DATA', WM_DDE_DATA);
  TSepiConstant.Create(Result, 'WM_DDE_REQUEST', WM_DDE_REQUEST);
  TSepiConstant.Create(Result, 'WM_DDE_POKE', WM_DDE_POKE);
  TSepiConstant.Create(Result, 'WM_DDE_EXECUTE', WM_DDE_EXECUTE);
  TSepiConstant.Create(Result, 'WM_DDE_LAST', WM_DDE_LAST);
  TSepiConstant.Create(Result, 'WM_APP', WM_APP);
  TSepiConstant.Create(Result, 'WM_USER', WM_USER);

  // Constants
  TSepiConstant.Create(Result, 'BN_CLICKED', BN_CLICKED);
  TSepiConstant.Create(Result, 'BN_PAINT', BN_PAINT);
  TSepiConstant.Create(Result, 'BN_HILITE', BN_HILITE);
  TSepiConstant.Create(Result, 'BN_UNHILITE', BN_UNHILITE);
  TSepiConstant.Create(Result, 'BN_DISABLE', BN_DISABLE);
  TSepiConstant.Create(Result, 'BN_DOUBLECLICKED', BN_DOUBLECLICKED);
  TSepiConstant.Create(Result, 'BN_PUSHED', BN_PUSHED);
  TSepiConstant.Create(Result, 'BN_UNPUSHED', BN_UNPUSHED);
  TSepiConstant.Create(Result, 'BN_DBLCLK', BN_DBLCLK);
  TSepiConstant.Create(Result, 'BN_SETFOCUS', BN_SETFOCUS);
  TSepiConstant.Create(Result, 'BN_KILLFOCUS', BN_KILLFOCUS);

  // Constants
  TSepiConstant.Create(Result, 'BM_GETCHECK', BM_GETCHECK);
  TSepiConstant.Create(Result, 'BM_SETCHECK', BM_SETCHECK);
  TSepiConstant.Create(Result, 'BM_GETSTATE', BM_GETSTATE);
  TSepiConstant.Create(Result, 'BM_SETSTATE', BM_SETSTATE);
  TSepiConstant.Create(Result, 'BM_SETSTYLE', BM_SETSTYLE);
  TSepiConstant.Create(Result, 'BM_CLICK', BM_CLICK);
  TSepiConstant.Create(Result, 'BM_GETIMAGE', BM_GETIMAGE);
  TSepiConstant.Create(Result, 'BM_SETIMAGE', BM_SETIMAGE);

  // Constants
  TSepiConstant.Create(Result, 'LBN_ERRSPACE', LBN_ERRSPACE);
  TSepiConstant.Create(Result, 'LBN_SELCHANGE', LBN_SELCHANGE);
  TSepiConstant.Create(Result, 'LBN_DBLCLK', LBN_DBLCLK);
  TSepiConstant.Create(Result, 'LBN_SELCANCEL', LBN_SELCANCEL);
  TSepiConstant.Create(Result, 'LBN_SETFOCUS', LBN_SETFOCUS);
  TSepiConstant.Create(Result, 'LBN_KILLFOCUS', LBN_KILLFOCUS);

  // Constants
  TSepiConstant.Create(Result, 'LB_ADDSTRING', LB_ADDSTRING);
  TSepiConstant.Create(Result, 'LB_INSERTSTRING', LB_INSERTSTRING);
  TSepiConstant.Create(Result, 'LB_DELETESTRING', LB_DELETESTRING);
  TSepiConstant.Create(Result, 'LB_SELITEMRANGEEX', LB_SELITEMRANGEEX);
  TSepiConstant.Create(Result, 'LB_RESETCONTENT', LB_RESETCONTENT);
  TSepiConstant.Create(Result, 'LB_SETSEL', LB_SETSEL);
  TSepiConstant.Create(Result, 'LB_SETCURSEL', LB_SETCURSEL);
  TSepiConstant.Create(Result, 'LB_GETSEL', LB_GETSEL);
  TSepiConstant.Create(Result, 'LB_GETCURSEL', LB_GETCURSEL);
  TSepiConstant.Create(Result, 'LB_GETTEXT', LB_GETTEXT);
  TSepiConstant.Create(Result, 'LB_GETTEXTLEN', LB_GETTEXTLEN);
  TSepiConstant.Create(Result, 'LB_GETCOUNT', LB_GETCOUNT);
  TSepiConstant.Create(Result, 'LB_SELECTSTRING', LB_SELECTSTRING);
  TSepiConstant.Create(Result, 'LB_DIR', LB_DIR);
  TSepiConstant.Create(Result, 'LB_GETTOPINDEX', LB_GETTOPINDEX);
  TSepiConstant.Create(Result, 'LB_FINDSTRING', LB_FINDSTRING);
  TSepiConstant.Create(Result, 'LB_GETSELCOUNT', LB_GETSELCOUNT);
  TSepiConstant.Create(Result, 'LB_GETSELITEMS', LB_GETSELITEMS);
  TSepiConstant.Create(Result, 'LB_SETTABSTOPS', LB_SETTABSTOPS);
  TSepiConstant.Create(Result, 'LB_GETHORIZONTALEXTENT', LB_GETHORIZONTALEXTENT);
  TSepiConstant.Create(Result, 'LB_SETHORIZONTALEXTENT', LB_SETHORIZONTALEXTENT);
  TSepiConstant.Create(Result, 'LB_SETCOLUMNWIDTH', LB_SETCOLUMNWIDTH);
  TSepiConstant.Create(Result, 'LB_ADDFILE', LB_ADDFILE);
  TSepiConstant.Create(Result, 'LB_SETTOPINDEX', LB_SETTOPINDEX);
  TSepiConstant.Create(Result, 'LB_GETITEMRECT', LB_GETITEMRECT);
  TSepiConstant.Create(Result, 'LB_GETITEMDATA', LB_GETITEMDATA);
  TSepiConstant.Create(Result, 'LB_SETITEMDATA', LB_SETITEMDATA);
  TSepiConstant.Create(Result, 'LB_SELITEMRANGE', LB_SELITEMRANGE);
  TSepiConstant.Create(Result, 'LB_SETANCHORINDEX', LB_SETANCHORINDEX);
  TSepiConstant.Create(Result, 'LB_GETANCHORINDEX', LB_GETANCHORINDEX);
  TSepiConstant.Create(Result, 'LB_SETCARETINDEX', LB_SETCARETINDEX);
  TSepiConstant.Create(Result, 'LB_GETCARETINDEX', LB_GETCARETINDEX);
  TSepiConstant.Create(Result, 'LB_SETITEMHEIGHT', LB_SETITEMHEIGHT);
  TSepiConstant.Create(Result, 'LB_GETITEMHEIGHT', LB_GETITEMHEIGHT);
  TSepiConstant.Create(Result, 'LB_FINDSTRINGEXACT', LB_FINDSTRINGEXACT);
  TSepiConstant.Create(Result, 'LB_SETLOCALE', LB_SETLOCALE);
  TSepiConstant.Create(Result, 'LB_GETLOCALE', LB_GETLOCALE);
  TSepiConstant.Create(Result, 'LB_SETCOUNT', LB_SETCOUNT);
  TSepiConstant.Create(Result, 'LB_INITSTORAGE', LB_INITSTORAGE);
  TSepiConstant.Create(Result, 'LB_ITEMFROMPOINT', LB_ITEMFROMPOINT);
  TSepiConstant.Create(Result, 'LB_MSGMAX', LB_MSGMAX);

  // Constants
  TSepiConstant.Create(Result, 'CBN_ERRSPACE', CBN_ERRSPACE);
  TSepiConstant.Create(Result, 'CBN_SELCHANGE', CBN_SELCHANGE);
  TSepiConstant.Create(Result, 'CBN_DBLCLK', CBN_DBLCLK);
  TSepiConstant.Create(Result, 'CBN_SETFOCUS', CBN_SETFOCUS);
  TSepiConstant.Create(Result, 'CBN_KILLFOCUS', CBN_KILLFOCUS);
  TSepiConstant.Create(Result, 'CBN_EDITCHANGE', CBN_EDITCHANGE);
  TSepiConstant.Create(Result, 'CBN_EDITUPDATE', CBN_EDITUPDATE);
  TSepiConstant.Create(Result, 'CBN_DROPDOWN', CBN_DROPDOWN);
  TSepiConstant.Create(Result, 'CBN_CLOSEUP', CBN_CLOSEUP);
  TSepiConstant.Create(Result, 'CBN_SELENDOK', CBN_SELENDOK);
  TSepiConstant.Create(Result, 'CBN_SELENDCANCEL', CBN_SELENDCANCEL);
  TSepiConstant.Create(Result, 'CB_GETEDITSEL', CB_GETEDITSEL);
  TSepiConstant.Create(Result, 'CB_LIMITTEXT', CB_LIMITTEXT);
  TSepiConstant.Create(Result, 'CB_SETEDITSEL', CB_SETEDITSEL);
  TSepiConstant.Create(Result, 'CB_ADDSTRING', CB_ADDSTRING);
  TSepiConstant.Create(Result, 'CB_DELETESTRING', CB_DELETESTRING);
  TSepiConstant.Create(Result, 'CB_DIR', CB_DIR);
  TSepiConstant.Create(Result, 'CB_GETCOUNT', CB_GETCOUNT);
  TSepiConstant.Create(Result, 'CB_GETCURSEL', CB_GETCURSEL);
  TSepiConstant.Create(Result, 'CB_GETLBTEXT', CB_GETLBTEXT);
  TSepiConstant.Create(Result, 'CB_GETLBTEXTLEN', CB_GETLBTEXTLEN);
  TSepiConstant.Create(Result, 'CB_INSERTSTRING', CB_INSERTSTRING);
  TSepiConstant.Create(Result, 'CB_RESETCONTENT', CB_RESETCONTENT);
  TSepiConstant.Create(Result, 'CB_FINDSTRING', CB_FINDSTRING);
  TSepiConstant.Create(Result, 'CB_SELECTSTRING', CB_SELECTSTRING);
  TSepiConstant.Create(Result, 'CB_SETCURSEL', CB_SETCURSEL);
  TSepiConstant.Create(Result, 'CB_SHOWDROPDOWN', CB_SHOWDROPDOWN);
  TSepiConstant.Create(Result, 'CB_GETITEMDATA', CB_GETITEMDATA);
  TSepiConstant.Create(Result, 'CB_SETITEMDATA', CB_SETITEMDATA);
  TSepiConstant.Create(Result, 'CB_GETDROPPEDCONTROLRECT', CB_GETDROPPEDCONTROLRECT);
  TSepiConstant.Create(Result, 'CB_SETITEMHEIGHT', CB_SETITEMHEIGHT);
  TSepiConstant.Create(Result, 'CB_GETITEMHEIGHT', CB_GETITEMHEIGHT);
  TSepiConstant.Create(Result, 'CB_SETEXTENDEDUI', CB_SETEXTENDEDUI);
  TSepiConstant.Create(Result, 'CB_GETEXTENDEDUI', CB_GETEXTENDEDUI);
  TSepiConstant.Create(Result, 'CB_GETDROPPEDSTATE', CB_GETDROPPEDSTATE);
  TSepiConstant.Create(Result, 'CB_FINDSTRINGEXACT', CB_FINDSTRINGEXACT);
  TSepiConstant.Create(Result, 'CB_SETLOCALE', CB_SETLOCALE);
  TSepiConstant.Create(Result, 'CB_GETLOCALE', CB_GETLOCALE);
  TSepiConstant.Create(Result, 'CB_GETTOPINDEX', CB_GETTOPINDEX);
  TSepiConstant.Create(Result, 'CB_SETTOPINDEX', CB_SETTOPINDEX);
  TSepiConstant.Create(Result, 'CB_GETHORIZONTALEXTENT', CB_GETHORIZONTALEXTENT);
  TSepiConstant.Create(Result, 'CB_SETHORIZONTALEXTENT', CB_SETHORIZONTALEXTENT);
  TSepiConstant.Create(Result, 'CB_GETDROPPEDWIDTH', CB_GETDROPPEDWIDTH);
  TSepiConstant.Create(Result, 'CB_SETDROPPEDWIDTH', CB_SETDROPPEDWIDTH);
  TSepiConstant.Create(Result, 'CB_INITSTORAGE', CB_INITSTORAGE);
  TSepiConstant.Create(Result, 'CB_MSGMAX', CB_MSGMAX);

  // Constants
  TSepiConstant.Create(Result, 'EN_SETFOCUS', EN_SETFOCUS);
  TSepiConstant.Create(Result, 'EN_KILLFOCUS', EN_KILLFOCUS);
  TSepiConstant.Create(Result, 'EN_CHANGE', EN_CHANGE);
  TSepiConstant.Create(Result, 'EN_UPDATE', EN_UPDATE);
  TSepiConstant.Create(Result, 'EN_ERRSPACE', EN_ERRSPACE);
  TSepiConstant.Create(Result, 'EN_MAXTEXT', EN_MAXTEXT);
  TSepiConstant.Create(Result, 'EN_HSCROLL', EN_HSCROLL);
  TSepiConstant.Create(Result, 'EN_VSCROLL', EN_VSCROLL);

  // Constants
  TSepiConstant.Create(Result, 'EM_GETSEL', EM_GETSEL);
  TSepiConstant.Create(Result, 'EM_SETSEL', EM_SETSEL);
  TSepiConstant.Create(Result, 'EM_GETRECT', EM_GETRECT);
  TSepiConstant.Create(Result, 'EM_SETRECT', EM_SETRECT);
  TSepiConstant.Create(Result, 'EM_SETRECTNP', EM_SETRECTNP);
  TSepiConstant.Create(Result, 'EM_SCROLL', EM_SCROLL);
  TSepiConstant.Create(Result, 'EM_LINESCROLL', EM_LINESCROLL);
  TSepiConstant.Create(Result, 'EM_SCROLLCARET', EM_SCROLLCARET);
  TSepiConstant.Create(Result, 'EM_GETMODIFY', EM_GETMODIFY);
  TSepiConstant.Create(Result, 'EM_SETMODIFY', EM_SETMODIFY);
  TSepiConstant.Create(Result, 'EM_GETLINECOUNT', EM_GETLINECOUNT);
  TSepiConstant.Create(Result, 'EM_LINEINDEX', EM_LINEINDEX);
  TSepiConstant.Create(Result, 'EM_SETHANDLE', EM_SETHANDLE);
  TSepiConstant.Create(Result, 'EM_GETHANDLE', EM_GETHANDLE);
  TSepiConstant.Create(Result, 'EM_GETTHUMB', EM_GETTHUMB);
  TSepiConstant.Create(Result, 'EM_LINELENGTH', EM_LINELENGTH);
  TSepiConstant.Create(Result, 'EM_REPLACESEL', EM_REPLACESEL);
  TSepiConstant.Create(Result, 'EM_GETLINE', EM_GETLINE);
  TSepiConstant.Create(Result, 'EM_LIMITTEXT', EM_LIMITTEXT);
  TSepiConstant.Create(Result, 'EM_CANUNDO', EM_CANUNDO);
  TSepiConstant.Create(Result, 'EM_UNDO', EM_UNDO);
  TSepiConstant.Create(Result, 'EM_FMTLINES', EM_FMTLINES);
  TSepiConstant.Create(Result, 'EM_LINEFROMCHAR', EM_LINEFROMCHAR);
  TSepiConstant.Create(Result, 'EM_SETTABSTOPS', EM_SETTABSTOPS);
  TSepiConstant.Create(Result, 'EM_SETPASSWORDCHAR', EM_SETPASSWORDCHAR);
  TSepiConstant.Create(Result, 'EM_EMPTYUNDOBUFFER', EM_EMPTYUNDOBUFFER);
  TSepiConstant.Create(Result, 'EM_GETFIRSTVISIBLELINE', EM_GETFIRSTVISIBLELINE);
  TSepiConstant.Create(Result, 'EM_SETREADONLY', EM_SETREADONLY);
  TSepiConstant.Create(Result, 'EM_SETWORDBREAKPROC', EM_SETWORDBREAKPROC);
  TSepiConstant.Create(Result, 'EM_GETWORDBREAKPROC', EM_GETWORDBREAKPROC);
  TSepiConstant.Create(Result, 'EM_GETPASSWORDCHAR', EM_GETPASSWORDCHAR);
  TSepiConstant.Create(Result, 'EM_SETMARGINS', EM_SETMARGINS);
  TSepiConstant.Create(Result, 'EM_GETMARGINS', EM_GETMARGINS);
  TSepiConstant.Create(Result, 'EM_SETLIMITTEXT', EM_SETLIMITTEXT);
  TSepiConstant.Create(Result, 'EM_GETLIMITTEXT', EM_GETLIMITTEXT);
  TSepiConstant.Create(Result, 'EM_POSFROMCHAR', EM_POSFROMCHAR);
  TSepiConstant.Create(Result, 'EM_CHARFROMPOS', EM_CHARFROMPOS);
  TSepiConstant.Create(Result, 'EM_SETIMESTATUS', EM_SETIMESTATUS);
  TSepiConstant.Create(Result, 'EM_GETIMESTATUS', EM_GETIMESTATUS);

  // Constants
  TSepiConstant.Create(Result, 'SBM_SETPOS', SBM_SETPOS);
  TSepiConstant.Create(Result, 'SBM_GETPOS', SBM_GETPOS);
  TSepiConstant.Create(Result, 'SBM_SETRANGE', SBM_SETRANGE);
  TSepiConstant.Create(Result, 'SBM_SETRANGEREDRAW', SBM_SETRANGEREDRAW);
  TSepiConstant.Create(Result, 'SBM_GETRANGE', SBM_GETRANGE);
  TSepiConstant.Create(Result, 'SBM_ENABLE_ARROWS', SBM_ENABLE_ARROWS);
  TSepiConstant.Create(Result, 'SBM_SETSCROLLINFO', SBM_SETSCROLLINFO);
  TSepiConstant.Create(Result, 'SBM_GETSCROLLINFO', SBM_GETSCROLLINFO);
  TSepiConstant.Create(Result, 'DM_GETDEFID', DM_GETDEFID);
  TSepiConstant.Create(Result, 'DM_SETDEFID', DM_SETDEFID);
  TSepiConstant.Create(Result, 'DM_REPOSITION', DM_REPOSITION);
  TSepiConstant.Create(Result, 'PSM_PAGEINFO', PSM_PAGEINFO);
  TSepiConstant.Create(Result, 'PSM_SHEETINFO', PSM_SHEETINFO);

  // Types
  TSepiPointerType.Create(Result, 'PMessage', 'TMessage', True);
  SepiImportTMessage(Result);
  TSepiArrayType.Create(Result, '$1',
    [0, 3], TypeInfo(Word), True);
  SepiImportTWMNoParams(Result);
  SepiImportTWMKey(Result);
  SepiImportTWMMouse(Result);
  SepiImportTWMMouseWheel(Result);
  SepiImportTMSHMouseWheel(Result);
  SepiImportTWMWindowPosMsg(Result);
  SepiImportTWMScroll(Result);
  SepiImportTWMActivate(Result);
  SepiImportTWMActivateApp(Result);
  SepiImportTWMAskCBFormatName(Result);
  TSepiTypeAlias.Create(Result, 'TWMCancelMode', 'TWMNoParams');
  SepiImportTWMChangeCBChain(Result);
  TSepiTypeAlias.Create(Result, 'TWMChar', 'TWMKey');
  SepiImportTWMCharToItem(Result);
  TSepiTypeAlias.Create(Result, 'TWMChildActivate', 'TWMNoParams');
  SepiImportTWMChooseFont_GetLogFont(Result);
  TSepiTypeAlias.Create(Result, 'TWMClear', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TWMClose', 'TWMNoParams');
  SepiImportTWMCommand(Result);
  SepiImportTWMCompacting(Result);
  SepiImportTWMCompareItem(Result);
  TSepiTypeAlias.Create(Result, 'TWMCopy', 'TWMNoParams');
  SepiImportTWMCopyData(Result);
  SepiImportTWMCreate(Result);
  SepiImportTWMCtlColor(Result);
  TSepiTypeAlias.Create(Result, 'TWMCtlColorBtn', 'TWMCtlColor');
  TSepiTypeAlias.Create(Result, 'TWMCtlColorDlg', 'TWMCtlColor');
  TSepiTypeAlias.Create(Result, 'TWMCtlColorEdit', 'TWMCtlColor');
  TSepiTypeAlias.Create(Result, 'TWMCtlColorListbox', 'TWMCtlColor');
  TSepiTypeAlias.Create(Result, 'TWMCtlColorMsgbox', 'TWMCtlColor');
  TSepiTypeAlias.Create(Result, 'TWMCtlColorScrollbar', 'TWMCtlColor');
  TSepiTypeAlias.Create(Result, 'TWMCtlColorStatic', 'TWMCtlColor');
  TSepiTypeAlias.Create(Result, 'TWMCut', 'TWMNoParams');
  SepiImportTWMDDE_Ack(Result);
  SepiImportTWMDDE_Advise(Result);
  SepiImportTWMDDE_Data(Result);
  SepiImportTWMDDE_Execute(Result);
  SepiImportTWMDDE_Initiate(Result);
  SepiImportTWMDDE_Poke(Result);
  SepiImportTWMDDE_Request(Result);
  SepiImportTWMDDE_Terminate(Result);
  SepiImportTWMDDE_Unadvise(Result);
  TSepiTypeAlias.Create(Result, 'TWMDeadChar', 'TWMChar');
  SepiImportTWMDeleteItem(Result);
  TSepiTypeAlias.Create(Result, 'TWMDestroy', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TWMDestroyClipboard', 'TWMNoParams');
  SepiImportTWMDevModeChange(Result);
  TSepiTypeAlias.Create(Result, 'TWMDrawClipboard', 'TWMNoParams');
  SepiImportTWMDrawItem(Result);
  SepiImportTWMDropFiles(Result);
  SepiImportTWMEnable(Result);
  SepiImportTWMEndSession(Result);
  SepiImportTWMEnterIdle(Result);
  SepiImportTWMEnterMenuLoop(Result);
  TSepiTypeAlias.Create(Result, 'TWMExitMenuLoop', 'TWMEnterMenuLoop');
  SepiImportTWMEraseBkgnd(Result);
  TSepiTypeAlias.Create(Result, 'TWMFontChange', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TWMGetDlgCode', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TWMGetFont', 'TWMNoParams');
  SepiImportTWMGetIcon(Result);
  TSepiTypeAlias.Create(Result, 'TWMGetHotKey', 'TWMNoParams');
  SepiImportTWMGetMinMaxInfo(Result);
  SepiImportTWMGetText(Result);
  TSepiTypeAlias.Create(Result, 'TWMGetTextLength', 'TWMNoParams');
  SepiImportTWMHotKey(Result);
  TSepiTypeAlias.Create(Result, 'TWMHScroll', 'TWMScroll');
  SepiImportTWMHScrollClipboard(Result);
  TSepiTypeAlias.Create(Result, 'TWMIconEraseBkgnd', 'TWMEraseBkgnd');
  SepiImportTWMInitDialog(Result);
  SepiImportTWMInitMenu(Result);
  SepiImportTWMInitMenuPopup(Result);
  TSepiTypeAlias.Create(Result, 'TWMKeyDown', 'TWMKey');
  TSepiTypeAlias.Create(Result, 'TWMKeyUp', 'TWMKey');
  SepiImportTWMKillFocus(Result);
  TSepiTypeAlias.Create(Result, 'TWMLButtonDblClk', 'TWMMouse');
  TSepiTypeAlias.Create(Result, 'TWMLButtonDown', 'TWMMouse');
  TSepiTypeAlias.Create(Result, 'TWMLButtonUp', 'TWMMouse');
  TSepiTypeAlias.Create(Result, 'TWMMButtonDblClk', 'TWMMouse');
  TSepiTypeAlias.Create(Result, 'TWMMButtonDown', 'TWMMouse');
  TSepiTypeAlias.Create(Result, 'TWMMButtonUp', 'TWMMouse');
  SepiImportTWMMDIActivate(Result);
  SepiImportTWMMDICascade(Result);
  SepiImportTWMMDICreate(Result);
  SepiImportTWMMDIDestroy(Result);
  TSepiTypeAlias.Create(Result, 'TWMMDIGetActive', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TWMMDIIconArrange', 'TWMNoParams');
  SepiImportTWMMDIMaximize(Result);
  SepiImportTWMMDINext(Result);
  TSepiTypeAlias.Create(Result, 'TWMMDIRefreshMenu', 'TWMNoParams');
  SepiImportTWMMDIRestore(Result);
  SepiImportTWMMDISetMenu(Result);
  SepiImportTWMMDITile(Result);
  SepiImportTWMMeasureItem(Result);
  SepiImportTWMMenuChar(Result);
  SepiImportTWMMenuSelect(Result);
  SepiImportTWMMouseActivate(Result);
  TSepiTypeAlias.Create(Result, 'TWMMouseMove', 'TWMMouse');
  SepiImportTWMMove(Result);
  SepiImportTWMMoving(Result);
  SepiImportTWMNCActivate(Result);
  SepiImportTWMNCCalcSize(Result);
  SepiImportTWMNCCreate(Result);
  TSepiTypeAlias.Create(Result, 'TWMNCDestroy', 'TWMNoParams');
  SepiImportTWMNCHitTest(Result);
  SepiImportTWMNCHitMessage(Result);
  TSepiTypeAlias.Create(Result, 'TWMNCLButtonDblClk', 'TWMNCHitMessage');
  TSepiTypeAlias.Create(Result, 'TWMNCLButtonDown', 'TWMNCHitMessage');
  TSepiTypeAlias.Create(Result, 'TWMNCLButtonUp', 'TWMNCHitMessage');
  TSepiTypeAlias.Create(Result, 'TWMNCMButtonDblClk', 'TWMNCHitMessage');
  TSepiTypeAlias.Create(Result, 'TWMNCMButtonDown', 'TWMNCHitMessage');
  TSepiTypeAlias.Create(Result, 'TWMNCMButtonUp', 'TWMNCHitMessage');
  TSepiTypeAlias.Create(Result, 'TWMNCMouseMove', 'TWMNCHitMessage');
  SepiImportTWMNCPaint(Result);
  TSepiTypeAlias.Create(Result, 'TWMNCRButtonDblClk', 'TWMNCHitMessage');
  TSepiTypeAlias.Create(Result, 'TWMNCRButtonDown', 'TWMNCHitMessage');
  TSepiTypeAlias.Create(Result, 'TWMNCRButtonUp', 'TWMNCHitMessage');
  SepiImportTWMNextDlgCtl(Result);
  SepiImportTWMNotify(Result);
  SepiImportTWMNotifyFormat(Result);
  SepiImportTWMPaint(Result);
  SepiImportTWMPaintClipboard(Result);
  TSepiTypeAlias.Create(Result, 'TWMPaintIcon', 'TWMNoParams');
  SepiImportTWMPaletteChanged(Result);
  SepiImportTWMPaletteIsChanging(Result);
  SepiImportTWMParentNotify(Result);
  TSepiTypeAlias.Create(Result, 'TWMPaste', 'TWMNoParams');
  SepiImportTWMPower(Result);
  TSepiTypeAlias.Create(Result, 'TWMQueryDragIcon', 'TWMNoParams');
  SepiImportTWMQueryEndSession(Result);
  TSepiTypeAlias.Create(Result, 'TWMQueryNewPalette', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TWMQueryOpen', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TWMQueueSync', 'TWMNoParams');
  SepiImportTWMQuit(Result);
  TSepiTypeAlias.Create(Result, 'TWMRButtonDblClk', 'TWMMouse');
  TSepiTypeAlias.Create(Result, 'TWMRButtonDown', 'TWMMouse');
  TSepiTypeAlias.Create(Result, 'TWMRButtonUp', 'TWMMouse');
  TSepiTypeAlias.Create(Result, 'TWMRenderAllFormats', 'TWMNoParams');
  SepiImportTWMRenderFormat(Result);
  SepiImportTWMSetCursor(Result);
  SepiImportTWMSetFocus(Result);
  SepiImportTWMSetFont(Result);
  SepiImportTWMSetHotKey(Result);
  SepiImportTWMSetIcon(Result);
  SepiImportTWMSetRedraw(Result);
  SepiImportTWMSetText(Result);
  SepiImportTWMShowWindow(Result);
  SepiImportTWMSize(Result);
  SepiImportTWMSizeClipboard(Result);
  SepiImportTWMSpoolerStatus(Result);
  SepiImportTWMStyleChange(Result);
  TSepiTypeAlias.Create(Result, 'TWMStyleChanged', 'TWMStyleChange');
  TSepiTypeAlias.Create(Result, 'TWMStyleChanging', 'TWMStyleChange');
  TSepiTypeAlias.Create(Result, 'TWMSysChar', 'TWMKey');
  TSepiTypeAlias.Create(Result, 'TWMSysColorChange', 'TWMNoParams');
  SepiImportTWMSysCommand(Result);
  SepiImportTWMSysDeadChar(Result);
  TSepiTypeAlias.Create(Result, 'TWMSysKeyDown', 'TWMKey');
  TSepiTypeAlias.Create(Result, 'TWMSysKeyUp', 'TWMKey');
  SepiImportTWMSystemError(Result);
  TSepiTypeAlias.Create(Result, 'TWMTimeChange', 'TWMNoParams');
  SepiImportTWMTimer(Result);
  SepiImportTWMUIState(Result);
  TSepiTypeAlias.Create(Result, 'TWMChangeUIState', 'TWMUIState');
  TSepiTypeAlias.Create(Result, 'TWMUpdateUIState', 'TWMUIState');
  TSepiTypeAlias.Create(Result, 'TWMQueryUIState', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TWMUndo', 'TWMNoParams');
  TSepiTypeAlias.Create(Result, 'TWMVKeyToItem', 'TWMCharToItem');
  TSepiTypeAlias.Create(Result, 'TWMVScroll', 'TWMScroll');
  SepiImportTWMVScrollClipboard(Result);
  TSepiTypeAlias.Create(Result, 'TWMWindowPosChanged', 'TWMWindowPosMsg');
  TSepiTypeAlias.Create(Result, 'TWMWindowPosChanging', 'TWMWindowPosMsg');
  SepiImportTWMWinIniChange(Result);
  SepiImportTWMSettingChange(Result);
  SepiImportTWMHelp(Result);
  SepiImportTWMDisplayChange(Result);
  SepiImportTWMContextMenu(Result);
  SepiImportTWMPrint(Result);
  TSepiTypeAlias.Create(Result, 'TWMPrintClient', 'TWMPrint');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Messages', ImportUnit);
end.

