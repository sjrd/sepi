{*
  Importe l'unité RichEdit dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsRichEdit;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, RichEdit;

implementation

{ You must not localize any of the strings this unit contains! }

{---------------------}
{ TCharFormatA import }
{---------------------}

function SepiImportTCharFormatA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCharFormatA', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('dwMask', System.TypeInfo(Longint));
    AddField('dwEffects', System.TypeInfo(Longint));
    AddField('yHeight', System.TypeInfo(Longint));
    AddField('yOffset', System.TypeInfo(Longint));
    AddField('crTextColor', System.TypeInfo(TColorRef));
    AddField('bCharSet', System.TypeInfo(Byte));
    AddField('bPitchAndFamily', System.TypeInfo(Byte));
    AddField('szFaceName', '$1');

    Complete;
  end;
end;

{---------------------}
{ TCharFormatW import }
{---------------------}

function SepiImportTCharFormatW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCharFormatW', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('dwMask', System.TypeInfo(Longint));
    AddField('dwEffects', System.TypeInfo(Longint));
    AddField('yHeight', System.TypeInfo(Longint));
    AddField('yOffset', System.TypeInfo(Longint));
    AddField('crTextColor', System.TypeInfo(TColorRef));
    AddField('bCharSet', System.TypeInfo(Byte));
    AddField('bPitchAndFamily', System.TypeInfo(Byte));
    AddField('szFaceName', '$2');

    Complete;
  end;
end;

{-------------------}
{ _charrange import }
{-------------------}

function SepiImport_charrange(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_charrange', False, True);

  with Result do
  begin
    AddField('cpMin', System.TypeInfo(Longint));
    AddField('cpMax', System.TypeInfo(Longint));

    Complete;
  end;
end;

{-------------------}
{ TEXTRANGEA import }
{-------------------}

function SepiImportTEXTRANGEA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TEXTRANGEA', False, True);

  with Result do
  begin
    AddField('chrg', 'TCharRange');
    AddField('lpstrText', 'PAnsiChar');

    Complete;
  end;
end;

{-------------------}
{ TEXTRANGEW import }
{-------------------}

function SepiImportTEXTRANGEW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TEXTRANGEW', False, True);

  with Result do
  begin
    AddField('chrg', 'TCharRange');
    AddField('lpstrText', 'PWideChar');

    Complete;
  end;
end;

{--------------------}
{ _editstream import }
{--------------------}

function SepiImport_editstream(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_editstream', False, True);

  with Result do
  begin
    AddField('dwCookie', System.TypeInfo(Longint));
    AddField('dwError', System.TypeInfo(Longint));
    AddField('pfnCallback', 'TEditStreamCallBack');

    Complete;
  end;
end;

{------------------}
{ FINDTEXTA import }
{------------------}

function SepiImportFINDTEXTA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'FINDTEXTA', False, True);

  with Result do
  begin
    AddField('chrg', 'TCharRange');
    AddField('lpstrText', 'PAnsiChar');

    Complete;
  end;
end;

{------------------}
{ FINDTEXTW import }
{------------------}

function SepiImportFINDTEXTW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'FINDTEXTW', False, True);

  with Result do
  begin
    AddField('chrg', 'TCharRange');
    AddField('lpstrText', 'PWideChar');

    Complete;
  end;
end;

{--------------------}
{ FINDTEXTEXA import }
{--------------------}

function SepiImportFINDTEXTEXA(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'FINDTEXTEXA', False, True);

  with Result do
  begin
    AddField('chrg', 'TCharRange');
    AddField('lpstrText', 'PAnsiChar');
    AddField('chrgText', 'TCharRange');

    Complete;
  end;
end;

{--------------------}
{ FINDTEXTEXW import }
{--------------------}

function SepiImportFINDTEXTEXW(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'FINDTEXTEXW', False, True);

  with Result do
  begin
    AddField('chrg', 'TCharRange');
    AddField('lpstrText', 'PWideChar');
    AddField('chrgText', 'TCharRange');

    Complete;
  end;
end;

{---------------------}
{ _formatrange import }
{---------------------}

function SepiImport_formatrange(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_formatrange', False, True);

  with Result do
  begin
    AddField('hdc', System.TypeInfo(HDC));
    AddField('hdcTarget', System.TypeInfo(HDC));
    AddField('rc', 'TRect');
    AddField('rcPage', 'TRect');
    AddField('chrg', 'TCharRange');

    Complete;
  end;
end;

{--------------------}
{ _paraformat import }
{--------------------}

function SepiImport_paraformat(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_paraformat', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('wNumbering', System.TypeInfo(Word));
    AddField('wReserved', System.TypeInfo(Word));
    AddField('dxStartIndent', System.TypeInfo(Longint));
    AddField('dxRightIndent', System.TypeInfo(Longint));
    AddField('dxOffset', System.TypeInfo(Longint));
    AddField('wAlignment', System.TypeInfo(Word));
    AddField('cTabCount', System.TypeInfo(Smallint));
    AddField('rgxTabs', '$3');

    Complete;
  end;
end;

{---------------------}
{ CHARFORMAT2A import }
{---------------------}

function SepiImportCHARFORMAT2A(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'CHARFORMAT2A', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('dwEffects', System.TypeInfo(DWORD));
    AddField('yHeight', System.TypeInfo(Longint));
    AddField('yOffset', System.TypeInfo(Longint));
    AddField('crTextColor', System.TypeInfo(TColorRef));
    AddField('bCharSet', System.TypeInfo(Byte));
    AddField('bPitchAndFamily', System.TypeInfo(Byte));
    AddField('szFaceName', '$4');
    AddField('wWeight', System.TypeInfo(Word));
    AddField('sSpacing', System.TypeInfo(Smallint));
    AddField('crBackColor', System.TypeInfo(TColorRef));
    AddField('lid', System.TypeInfo(LCID));
    AddField('dwReserved', System.TypeInfo(DWORD));
    AddField('sStyle', System.TypeInfo(Smallint));
    AddField('wKerning', System.TypeInfo(Word));
    AddField('bUnderlineType', System.TypeInfo(Byte));
    AddField('bAnimation', System.TypeInfo(Byte));
    AddField('bRevAuthor', System.TypeInfo(Byte));
    AddField('bReserved1', System.TypeInfo(Byte));

    Complete;
  end;
end;

{---------------------}
{ CHARFORMAT2W import }
{---------------------}

function SepiImportCHARFORMAT2W(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'CHARFORMAT2W', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('dwEffects', System.TypeInfo(DWORD));
    AddField('yHeight', System.TypeInfo(Longint));
    AddField('yOffset', System.TypeInfo(Longint));
    AddField('crTextColor', System.TypeInfo(TColorRef));
    AddField('bCharSet', System.TypeInfo(Byte));
    AddField('bPitchAndFamily', System.TypeInfo(Byte));
    AddField('szFaceName', '$5');
    AddField('wWeight', System.TypeInfo(Word));
    AddField('sSpacing', System.TypeInfo(Smallint));
    AddField('crBackColor', System.TypeInfo(TColorRef));
    AddField('lid', System.TypeInfo(LCID));
    AddField('dwReserved', System.TypeInfo(DWORD));
    AddField('sStyle', System.TypeInfo(Smallint));
    AddField('wKerning', System.TypeInfo(Word));
    AddField('bUnderlineType', System.TypeInfo(Byte));
    AddField('bAnimation', System.TypeInfo(Byte));
    AddField('bRevAuthor', System.TypeInfo(Byte));
    AddField('bReserved1', System.TypeInfo(Byte));

    Complete;
  end;
end;

{--------------------}
{ PARAFORMAT2 import }
{--------------------}

function SepiImportPARAFORMAT2(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'PARAFORMAT2', False, True);

  with Result do
  begin
    AddField('cbSize', System.TypeInfo(UINT));
    AddField('dwMask', System.TypeInfo(DWORD));
    AddField('wNumbering', System.TypeInfo(Word));
    AddField('wReserved', System.TypeInfo(Word));
    AddField('dxStartIndent', System.TypeInfo(Longint));
    AddField('dxRightIndent', System.TypeInfo(Longint));
    AddField('dxOffset', System.TypeInfo(Longint));
    AddField('wAlignment', System.TypeInfo(Word));
    AddField('cTabCount', System.TypeInfo(Smallint));
    AddField('rgxTabs', '$6');
    AddField('dySpaceBefore', System.TypeInfo(Longint));
    AddField('dySpaceAfter', System.TypeInfo(Longint));
    AddField('dyLineSpacing', System.TypeInfo(Longint));
    AddField('sStyle', System.TypeInfo(Smallint));
    AddField('bLineSpacingRule', System.TypeInfo(Byte));
    AddField('bCRC', System.TypeInfo(Byte));
    AddField('wShadingWeight', System.TypeInfo(Word));
    AddField('wShadingStyle', System.TypeInfo(Word));
    AddField('wNumberingStart', System.TypeInfo(Word));
    AddField('wNumberingStyle', System.TypeInfo(Word));
    AddField('wNumberingTab', System.TypeInfo(Word));
    AddField('wBorderSpace', System.TypeInfo(Word));
    AddField('wBorderWidth', System.TypeInfo(Word));
    AddField('wBorders', System.TypeInfo(Word));

    Complete;
  end;
end;

{-------------------}
{ _msgfilter import }
{-------------------}

function SepiImport_msgfilter(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_msgfilter', False, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('msg', System.TypeInfo(UINT));
    AddField('wParam', System.TypeInfo(WPARAM));
    AddField('lParam', System.TypeInfo(LPARAM));

    Complete;
  end;
end;

{-----------------}
{ TReqSize import }
{-----------------}

function SepiImportTReqSize(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TReqSize', False, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('rc', 'TRect');

    Complete;
  end;
end;

{-------------------}
{ _selchange import }
{-------------------}

function SepiImport_selchange(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_selchange', False, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('chrg', 'TCharRange');
    AddField('seltyp', System.TypeInfo(Word));

    Complete;
  end;
end;

{----------------------}
{ TEndDropFiles import }
{----------------------}

function SepiImportTEndDropFiles(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TEndDropFiles', False, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('hDrop', System.TypeInfo(THandle));
    AddField('cp', System.TypeInfo(Longint));
    AddField('fProtected', System.TypeInfo(Bool));

    Complete;
  end;
end;

{---------------------}
{ _enprotected import }
{---------------------}

function SepiImport_enprotected(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_enprotected', False, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('msg', System.TypeInfo(UINT));
    AddField('wParam', System.TypeInfo(WPARAM));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('chrg', 'TCharRange');

    Complete;
  end;
end;

{-------------------------}
{ _ensaveclipboard import }
{-------------------------}

function SepiImport_ensaveclipboard(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_ensaveclipboard', False, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('cObjectCount', System.TypeInfo(Longint));
    AddField('cch', System.TypeInfo(Longint));

    Complete;
  end;
end;

{----------------------}
{ ENOLEOPFAILED import }
{----------------------}

function SepiImportENOLEOPFAILED(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'ENOLEOPFAILED', True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('iob', System.TypeInfo(Longint));
    AddField('lOper', System.TypeInfo(Longint));
    AddField('hr', System.TypeInfo(HRESULT));

    Complete;
  end;
end;

{------------------------}
{ OBJECTPOSITIONS import }
{------------------------}

function SepiImportOBJECTPOSITIONS(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'OBJECTPOSITIONS', True, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('cObjectCount', System.TypeInfo(Longint));
    AddField('pcpPositions', 'PLongint');

    Complete;
  end;
end;

{---------------}
{ ENLINK import }
{---------------}

function SepiImportENLINK(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'ENLINK', False, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('msg', System.TypeInfo(UINT));
    AddField('wParam', System.TypeInfo(WPARAM));
    AddField('lParam', System.TypeInfo(LPARAM));
    AddField('chrg', 'TCharRange');

    Complete;
  end;
end;

{-----------------------}
{ _encorrecttext import }
{-----------------------}

function SepiImport_encorrecttext(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_encorrecttext', False, True);

  with Result do
  begin
    AddField('nmhdr', 'TNMHdr');
    AddField('chrg', 'TCharRange');
    AddField('seltyp', System.TypeInfo(Word));

    Complete;
  end;
end;

{---------------------}
{ _punctuation import }
{---------------------}

function SepiImport_punctuation(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_punctuation', False, True);

  with Result do
  begin
    AddField('iSize', System.TypeInfo(UINT));
    AddField('szPunctuation', 'PChar');

    Complete;
  end;
end;

{-------------------}
{ _compcolor import }
{-------------------}

function SepiImport_compcolor(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_compcolor', False, True);

  with Result do
  begin
    AddField('crText', System.TypeInfo(TColorRef));
    AddField('crBackground', System.TypeInfo(TColorRef));
    AddField('dwEffects', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------}
{ _repastespecial import }
{------------------------}

function SepiImport_repastespecial(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_repastespecial', False, True);

  with Result do
  begin
    AddField('dwAspect', System.TypeInfo(DWORD));
    AddField('dwParam', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{------------------}
{ GETTEXTEX import }
{------------------}

function SepiImportGETTEXTEX(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'GETTEXTEX', False, True);

  with Result do
  begin
    AddField('cb', System.TypeInfo(DWORD));
    AddField('flags', System.TypeInfo(DWORD));
    AddField('codepage', System.TypeInfo(UINT));
    AddField('lpDefaultChar', 'LPCSTR');
    AddField('lpUsedDefChar', 'PBOOL');

    Complete;
  end;
end;

{------------------------}
{ GETTEXTLENGTHEX import }
{------------------------}

function SepiImportGETTEXTLENGTHEX(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'GETTEXTLENGTHEX', False, True);

  with Result do
  begin
    AddField('flags', System.TypeInfo(DWORD));
    AddField('codepage', System.TypeInfo(UINT));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'RichEdit',
    ['Messages', 'Windows']);

  // Constants
  TSepiConstant.Create(Result, 'cchTextLimitDefault', cchTextLimitDefault);
  TSepiConstant.Create(Result, 'RICHEDIT_CLASSA', RICHEDIT_CLASSA);
  TSepiConstant.Create(Result, 'RICHEDIT_CLASSW', RICHEDIT_CLASSW);
  TSepiConstant.Create(Result, 'RICHEDIT_CLASS', RICHEDIT_CLASS);
  TSepiConstant.Create(Result, 'RICHEDIT_CLASS10A', RICHEDIT_CLASS10A);
  TSepiConstant.Create(Result, 'WM_CONTEXTMENU', WM_CONTEXTMENU);
  TSepiConstant.Create(Result, 'WM_PRINTCLIENT', WM_PRINTCLIENT);
  TSepiConstant.Create(Result, 'EM_GETLIMITTEXT', EM_GETLIMITTEXT);
  TSepiConstant.Create(Result, 'EM_SCROLLCARET', EM_SCROLLCARET);
  TSepiConstant.Create(Result, 'EM_CANPASTE', EM_CANPASTE);
  TSepiConstant.Create(Result, 'EM_DISPLAYBAND', EM_DISPLAYBAND);
  TSepiConstant.Create(Result, 'EM_EXGETSEL', EM_EXGETSEL);
  TSepiConstant.Create(Result, 'EM_EXLIMITTEXT', EM_EXLIMITTEXT);
  TSepiConstant.Create(Result, 'EM_EXLINEFROMCHAR', EM_EXLINEFROMCHAR);
  TSepiConstant.Create(Result, 'EM_EXSETSEL', EM_EXSETSEL);
  TSepiConstant.Create(Result, 'EM_FINDTEXT', EM_FINDTEXT);
  TSepiConstant.Create(Result, 'EM_FORMATRANGE', EM_FORMATRANGE);
  TSepiConstant.Create(Result, 'EM_GETCHARFORMAT', EM_GETCHARFORMAT);
  TSepiConstant.Create(Result, 'EM_GETEVENTMASK', EM_GETEVENTMASK);
  TSepiConstant.Create(Result, 'EM_GETOLEINTERFACE', EM_GETOLEINTERFACE);
  TSepiConstant.Create(Result, 'EM_GETPARAFORMAT', EM_GETPARAFORMAT);
  TSepiConstant.Create(Result, 'EM_GETSELTEXT', EM_GETSELTEXT);
  TSepiConstant.Create(Result, 'EM_HIDESELECTION', EM_HIDESELECTION);
  TSepiConstant.Create(Result, 'EM_PASTESPECIAL', EM_PASTESPECIAL);
  TSepiConstant.Create(Result, 'EM_REQUESTRESIZE', EM_REQUESTRESIZE);
  TSepiConstant.Create(Result, 'EM_SELECTIONTYPE', EM_SELECTIONTYPE);
  TSepiConstant.Create(Result, 'EM_SETBKGNDCOLOR', EM_SETBKGNDCOLOR);
  TSepiConstant.Create(Result, 'EM_SETCHARFORMAT', EM_SETCHARFORMAT);
  TSepiConstant.Create(Result, 'EM_SETEVENTMASK', EM_SETEVENTMASK);
  TSepiConstant.Create(Result, 'EM_SETOLECALLBACK', EM_SETOLECALLBACK);
  TSepiConstant.Create(Result, 'EM_SETPARAFORMAT', EM_SETPARAFORMAT);
  TSepiConstant.Create(Result, 'EM_SETTARGETDEVICE', EM_SETTARGETDEVICE);
  TSepiConstant.Create(Result, 'EM_STREAMIN', EM_STREAMIN);
  TSepiConstant.Create(Result, 'EM_STREAMOUT', EM_STREAMOUT);
  TSepiConstant.Create(Result, 'EM_GETTEXTRANGE', EM_GETTEXTRANGE);
  TSepiConstant.Create(Result, 'EM_FINDWORDBREAK', EM_FINDWORDBREAK);
  TSepiConstant.Create(Result, 'EM_SETOPTIONS', EM_SETOPTIONS);
  TSepiConstant.Create(Result, 'EM_GETOPTIONS', EM_GETOPTIONS);
  TSepiConstant.Create(Result, 'EM_FINDTEXTEX', EM_FINDTEXTEX);
  TSepiConstant.Create(Result, 'EM_GETWORDBREAKPROCEX', EM_GETWORDBREAKPROCEX);
  TSepiConstant.Create(Result, 'EM_SETWORDBREAKPROCEX', EM_SETWORDBREAKPROCEX);
  TSepiConstant.Create(Result, 'EM_SETUNDOLIMIT', EM_SETUNDOLIMIT);
  TSepiConstant.Create(Result, 'EM_REDO', EM_REDO);
  TSepiConstant.Create(Result, 'EM_CANREDO', EM_CANREDO);
  TSepiConstant.Create(Result, 'EM_GETUNDONAME', EM_GETUNDONAME);
  TSepiConstant.Create(Result, 'EM_GETREDONAME', EM_GETREDONAME);
  TSepiConstant.Create(Result, 'EM_STOPGROUPTYPING', EM_STOPGROUPTYPING);
  TSepiConstant.Create(Result, 'EM_SETTEXTMODE', EM_SETTEXTMODE);
  TSepiConstant.Create(Result, 'EM_GETTEXTMODE', EM_GETTEXTMODE);
  TSepiConstant.Create(Result, 'TM_PLAINTEXT', TM_PLAINTEXT);
  TSepiConstant.Create(Result, 'TM_RICHTEXT', TM_RICHTEXT);
  TSepiConstant.Create(Result, 'TM_SINGLELEVELUNDO', TM_SINGLELEVELUNDO);
  TSepiConstant.Create(Result, 'TM_MULTILEVELUNDO', TM_MULTILEVELUNDO);
  TSepiConstant.Create(Result, 'TM_SINGLECODEPAGE', TM_SINGLECODEPAGE);
  TSepiConstant.Create(Result, 'TM_MULTICODEPAGE', TM_MULTICODEPAGE);
  TSepiConstant.Create(Result, 'EM_AUTOURLDETECT', EM_AUTOURLDETECT);
  TSepiConstant.Create(Result, 'EM_GETAUTOURLDETECT', EM_GETAUTOURLDETECT);
  TSepiConstant.Create(Result, 'EM_SETPALETTE', EM_SETPALETTE);
  TSepiConstant.Create(Result, 'EM_GETTEXTEX', EM_GETTEXTEX);
  TSepiConstant.Create(Result, 'EM_GETTEXTLENGTHEX', EM_GETTEXTLENGTHEX);
  TSepiConstant.Create(Result, 'EM_SETPUNCTUATION', EM_SETPUNCTUATION);
  TSepiConstant.Create(Result, 'EM_GETPUNCTUATION', EM_GETPUNCTUATION);
  TSepiConstant.Create(Result, 'EM_SETWORDWRAPMODE', EM_SETWORDWRAPMODE);
  TSepiConstant.Create(Result, 'EM_GETWORDWRAPMODE', EM_GETWORDWRAPMODE);
  TSepiConstant.Create(Result, 'EM_SETIMECOLOR', EM_SETIMECOLOR);
  TSepiConstant.Create(Result, 'EM_GETIMECOLOR', EM_GETIMECOLOR);
  TSepiConstant.Create(Result, 'EM_SETIMEOPTIONS', EM_SETIMEOPTIONS);
  TSepiConstant.Create(Result, 'EM_GETIMEOPTIONS', EM_GETIMEOPTIONS);
  TSepiConstant.Create(Result, 'EM_CONVPOSITION', EM_CONVPOSITION);
  TSepiConstant.Create(Result, 'EM_SETLANGOPTIONS', EM_SETLANGOPTIONS);
  TSepiConstant.Create(Result, 'EM_GETLANGOPTIONS', EM_GETLANGOPTIONS);
  TSepiConstant.Create(Result, 'EM_GETIMECOMPMODE', EM_GETIMECOMPMODE);
  TSepiConstant.Create(Result, 'IMF_AUTOKEYBOARD', IMF_AUTOKEYBOARD);
  TSepiConstant.Create(Result, 'IMF_AUTOFONT', IMF_AUTOFONT);
  TSepiConstant.Create(Result, 'IMF_IMECANCELCOMPLETE', IMF_IMECANCELCOMPLETE);
  TSepiConstant.Create(Result, 'IMF_IMEALWAYSSENDNOTIFY',
    IMF_IMEALWAYSSENDNOTIFY);
  TSepiConstant.Create(Result, 'ICM_NOTOPEN', ICM_NOTOPEN);
  TSepiConstant.Create(Result, 'ICM_LEVEL3', ICM_LEVEL3);
  TSepiConstant.Create(Result, 'ICM_LEVEL2', ICM_LEVEL2);
  TSepiConstant.Create(Result, 'ICM_LEVEL2_5', ICM_LEVEL2_5);
  TSepiConstant.Create(Result, 'ICM_LEVEL2_SUI', ICM_LEVEL2_SUI);
  TSepiConstant.Create(Result, 'EN_MSGFILTER', EN_MSGFILTER);
  TSepiConstant.Create(Result, 'EN_REQUESTRESIZE', EN_REQUESTRESIZE);
  TSepiConstant.Create(Result, 'EN_SELCHANGE', EN_SELCHANGE);
  TSepiConstant.Create(Result, 'EN_DROPFILES', EN_DROPFILES);
  TSepiConstant.Create(Result, 'EN_PROTECTED', EN_PROTECTED);
  TSepiConstant.Create(Result, 'EN_CORRECTTEXT', EN_CORRECTTEXT);
  TSepiConstant.Create(Result, 'EN_STOPNOUNDO', EN_STOPNOUNDO);
  TSepiConstant.Create(Result, 'EN_IMECHANGE', EN_IMECHANGE);
  TSepiConstant.Create(Result, 'EN_SAVECLIPBOARD', EN_SAVECLIPBOARD);
  TSepiConstant.Create(Result, 'EN_OLEOPFAILED', EN_OLEOPFAILED);
  TSepiConstant.Create(Result, 'EN_OBJECTPOSITIONS', EN_OBJECTPOSITIONS);
  TSepiConstant.Create(Result, 'EN_LINK', EN_LINK);
  TSepiConstant.Create(Result, 'EN_DRAGDROPDONE', EN_DRAGDROPDONE);
  TSepiConstant.Create(Result, 'ENM_NONE', ENM_NONE);
  TSepiConstant.Create(Result, 'ENM_CHANGE', ENM_CHANGE);
  TSepiConstant.Create(Result, 'ENM_UPDATE', ENM_UPDATE);
  TSepiConstant.Create(Result, 'ENM_SCROLL', ENM_SCROLL);
  TSepiConstant.Create(Result, 'ENM_KEYEVENTS', ENM_KEYEVENTS);
  TSepiConstant.Create(Result, 'ENM_MOUSEEVENTS', ENM_MOUSEEVENTS);
  TSepiConstant.Create(Result, 'ENM_REQUESTRESIZE', ENM_REQUESTRESIZE);
  TSepiConstant.Create(Result, 'ENM_SELCHANGE', ENM_SELCHANGE);
  TSepiConstant.Create(Result, 'ENM_DROPFILES', ENM_DROPFILES);
  TSepiConstant.Create(Result, 'ENM_PROTECTED', ENM_PROTECTED);
  TSepiConstant.Create(Result, 'ENM_CORRECTTEXT', ENM_CORRECTTEXT);
  TSepiConstant.Create(Result, 'ENM_SCROLLEVENTS', ENM_SCROLLEVENTS);
  TSepiConstant.Create(Result, 'ENM_DRAGDROPDONE', ENM_DRAGDROPDONE);
  TSepiConstant.Create(Result, 'ENM_IMECHANGE', ENM_IMECHANGE);
  TSepiConstant.Create(Result, 'ENM_LANGCHANGE', ENM_LANGCHANGE);
  TSepiConstant.Create(Result, 'ENM_OBJECTPOSITIONS', ENM_OBJECTPOSITIONS);
  TSepiConstant.Create(Result, 'ENM_LINK', ENM_LINK);
  TSepiConstant.Create(Result, 'ES_SAVESEL', ES_SAVESEL);
  TSepiConstant.Create(Result, 'ES_SUNKEN', ES_SUNKEN);
  TSepiConstant.Create(Result, 'ES_DISABLENOSCROLL', ES_DISABLENOSCROLL);
  TSepiConstant.Create(Result, 'ES_SELECTIONBAR', ES_SELECTIONBAR);
  TSepiConstant.Create(Result, 'ES_NOOLEDRAGDROP', ES_NOOLEDRAGDROP);
  TSepiConstant.Create(Result, 'ES_EX_NOCALLOLEINIT', ES_EX_NOCALLOLEINIT);
  TSepiConstant.Create(Result, 'ES_VERTICAL', ES_VERTICAL);
  TSepiConstant.Create(Result, 'ES_NOIME', ES_NOIME);
  TSepiConstant.Create(Result, 'ES_SELFIME', ES_SELFIME);
  TSepiConstant.Create(Result, 'ECO_AUTOWORDSELECTION', ECO_AUTOWORDSELECTION);
  TSepiConstant.Create(Result, 'ECO_AUTOVSCROLL', ECO_AUTOVSCROLL);
  TSepiConstant.Create(Result, 'ECO_AUTOHSCROLL', ECO_AUTOHSCROLL);
  TSepiConstant.Create(Result, 'ECO_NOHIDESEL', ECO_NOHIDESEL);
  TSepiConstant.Create(Result, 'ECO_READONLY', ECO_READONLY);
  TSepiConstant.Create(Result, 'ECO_WANTRETURN', ECO_WANTRETURN);
  TSepiConstant.Create(Result, 'ECO_SAVESEL', ECO_SAVESEL);
  TSepiConstant.Create(Result, 'ECO_SELECTIONBAR', ECO_SELECTIONBAR);
  TSepiConstant.Create(Result, 'ECO_VERTICAL', ECO_VERTICAL);
  TSepiConstant.Create(Result, 'ECOOP_SET', ECOOP_SET);
  TSepiConstant.Create(Result, 'ECOOP_OR', ECOOP_OR);
  TSepiConstant.Create(Result, 'ECOOP_AND', ECOOP_AND);
  TSepiConstant.Create(Result, 'ECOOP_XOR', ECOOP_XOR);
  TSepiConstant.Create(Result, 'WB_CLASSIFY', WB_CLASSIFY);
  TSepiConstant.Create(Result, 'WB_MOVEWORDLEFT', WB_MOVEWORDLEFT);
  TSepiConstant.Create(Result, 'WB_MOVEWORDRIGHT', WB_MOVEWORDRIGHT);
  TSepiConstant.Create(Result, 'WB_LEFTBREAK', WB_LEFTBREAK);
  TSepiConstant.Create(Result, 'WB_RIGHTBREAK', WB_RIGHTBREAK);
  TSepiConstant.Create(Result, 'WB_MOVEWORDPREV', WB_MOVEWORDPREV);
  TSepiConstant.Create(Result, 'WB_MOVEWORDNEXT', WB_MOVEWORDNEXT);
  TSepiConstant.Create(Result, 'WB_PREVBREAK', WB_PREVBREAK);
  TSepiConstant.Create(Result, 'WB_NEXTBREAK', WB_NEXTBREAK);
  TSepiConstant.Create(Result, 'PC_FOLLOWING', PC_FOLLOWING);
  TSepiConstant.Create(Result, 'PC_LEADING', PC_LEADING);
  TSepiConstant.Create(Result, 'PC_OVERFLOW', PC_OVERFLOW);
  TSepiConstant.Create(Result, 'PC_DELIMITER', PC_DELIMITER);
  TSepiConstant.Create(Result, 'WBF_WORDWRAP', WBF_WORDWRAP);
  TSepiConstant.Create(Result, 'WBF_WORDBREAK', WBF_WORDBREAK);
  TSepiConstant.Create(Result, 'WBF_OVERFLOW', WBF_OVERFLOW);
  TSepiConstant.Create(Result, 'WBF_LEVEL1', WBF_LEVEL1);
  TSepiConstant.Create(Result, 'WBF_LEVEL2', WBF_LEVEL2);
  TSepiConstant.Create(Result, 'WBF_CUSTOM', WBF_CUSTOM);
  TSepiConstant.Create(Result, 'IMF_FORCENONE', IMF_FORCENONE);
  TSepiConstant.Create(Result, 'IMF_FORCEENABLE', IMF_FORCEENABLE);
  TSepiConstant.Create(Result, 'IMF_FORCEDISABLE', IMF_FORCEDISABLE);
  TSepiConstant.Create(Result, 'IMF_CLOSESTATUSWINDOW', IMF_CLOSESTATUSWINDOW);
  TSepiConstant.Create(Result, 'IMF_VERTICAL', IMF_VERTICAL);
  TSepiConstant.Create(Result, 'IMF_FORCEACTIVE', IMF_FORCEACTIVE);
  TSepiConstant.Create(Result, 'IMF_FORCEINACTIVE', IMF_FORCEINACTIVE);
  TSepiConstant.Create(Result, 'IMF_FORCEREMEMBER', IMF_FORCEREMEMBER);
  TSepiConstant.Create(Result, 'IMF_MULTIPLEEDIT', IMF_MULTIPLEEDIT);
  TSepiConstant.Create(Result, 'WBF_CLASS', WBF_CLASS);
  TSepiConstant.Create(Result, 'WBF_ISWHITE', WBF_ISWHITE);
  TSepiConstant.Create(Result, 'WBF_BREAKLINE', WBF_BREAKLINE);
  TSepiConstant.Create(Result, 'WBF_BREAKAFTER', WBF_BREAKAFTER);

  // Types
  TSepiArrayType.Create(Result, '$1',
    [0, LF_FACESIZE - 1], TypeInfo(AnsiChar), True);
  SepiImportTCharFormatA(Result);
  TSepiArrayType.Create(Result, '$2',
    [0, LF_FACESIZE - 1], TypeInfo(WideChar), True);
  SepiImportTCharFormatW(Result);
  TSepiTypeAlias.Create(Result, 'TCharFormat', 'TCharFormatA');

  // Constants
  TSepiConstant.Create(Result, 'CFM_BOLD', CFM_BOLD);
  TSepiConstant.Create(Result, 'CFM_ITALIC', CFM_ITALIC);
  TSepiConstant.Create(Result, 'CFM_UNDERLINE', CFM_UNDERLINE);
  TSepiConstant.Create(Result, 'CFM_STRIKEOUT', CFM_STRIKEOUT);
  TSepiConstant.Create(Result, 'CFM_PROTECTED', CFM_PROTECTED);
  TSepiConstant.Create(Result, 'CFM_LINK', CFM_LINK);
  TSepiConstant.Create(Result, 'CFM_SIZE', CFM_SIZE);
  TSepiConstant.Create(Result, 'CFM_COLOR', CFM_COLOR);
  TSepiConstant.Create(Result, 'CFM_FACE', CFM_FACE);
  TSepiConstant.Create(Result, 'CFM_OFFSET', CFM_OFFSET);
  TSepiConstant.Create(Result, 'CFM_CHARSET', CFM_CHARSET);
  TSepiConstant.Create(Result, 'CFE_BOLD', CFE_BOLD);
  TSepiConstant.Create(Result, 'CFE_ITALIC', CFE_ITALIC);
  TSepiConstant.Create(Result, 'CFE_UNDERLINE', CFE_UNDERLINE);
  TSepiConstant.Create(Result, 'CFE_STRIKEOUT', CFE_STRIKEOUT);
  TSepiConstant.Create(Result, 'CFE_PROTECTED', CFE_PROTECTED);
  TSepiConstant.Create(Result, 'CFE_LINK', CFE_LINK);
  TSepiConstant.Create(Result, 'CFE_AUTOCOLOR', CFE_AUTOCOLOR);
  TSepiConstant.Create(Result, 'yHeightCharPtsMost', yHeightCharPtsMost);
  TSepiConstant.Create(Result, 'SCF_SELECTION', SCF_SELECTION);
  TSepiConstant.Create(Result, 'SCF_WORD', SCF_WORD);
  TSepiConstant.Create(Result, 'SCF_DEFAULT', SCF_DEFAULT);
  TSepiConstant.Create(Result, 'SCF_ALL', SCF_ALL);
  TSepiConstant.Create(Result, 'SCF_USEUIRULES', SCF_USEUIRULES);

  // Types
  SepiImport_charrange(Result);
  TSepiTypeAlias.Create(Result, 'TCharRange', '_charrange');
  TSepiTypeAlias.Create(Result, 'CHARRANGE', '_charrange');
  SepiImportTEXTRANGEA(Result);
  TSepiTypeAlias.Create(Result, 'TTextRangeA', 'TEXTRANGEA');
  SepiImportTEXTRANGEW(Result);
  TSepiTypeAlias.Create(Result, 'TTextRangeW', 'TEXTRANGEW');
  TSepiTypeAlias.Create(Result, 'TEXTRANGE', 'TEXTRANGEA');

  // Types
  TSepiMethodRefType.Create(Result, 'TEditStreamCallBack',
    'function(dwCookie: Longint; pbBuff: PByte; cb: Longint ; var pcb: Longint ) : Longint',
    False, ccStdCall);
  SepiImport_editstream(Result);
  TSepiTypeAlias.Create(Result, 'TEditStream', '_editstream');
  TSepiTypeAlias.Create(Result, 'EDITSTREAM', '_editstream');

  // Constants
  TSepiConstant.Create(Result, 'SF_TEXT', SF_TEXT);
  TSepiConstant.Create(Result, 'SF_RTF', SF_RTF);
  TSepiConstant.Create(Result, 'SF_RTFNOOBJS', SF_RTFNOOBJS);
  TSepiConstant.Create(Result, 'SF_TEXTIZED', SF_TEXTIZED);
  TSepiConstant.Create(Result, 'SF_UNICODE', SF_UNICODE);
  TSepiConstant.Create(Result, 'SFF_SELECTION', SFF_SELECTION);
  TSepiConstant.Create(Result, 'SFF_PLAINRTF', SFF_PLAINRTF);
  TSepiConstant.Create(Result, 'FT_MATCHCASE', FT_MATCHCASE);
  TSepiConstant.Create(Result, 'FT_WHOLEWORD', FT_WHOLEWORD);

  // Types
  SepiImportFINDTEXTA(Result);
  SepiImportFINDTEXTW(Result);
  TSepiTypeAlias.Create(Result, 'FINDTEXT', 'FINDTEXTA');
  TSepiTypeAlias.Create(Result, 'TFindTextA', 'FINDTEXTA');
  TSepiTypeAlias.Create(Result, 'TFindTextW', 'FINDTEXTW');
  TSepiTypeAlias.Create(Result, 'TFindText', 'TFindTextA');
  SepiImportFINDTEXTEXA(Result);
  SepiImportFINDTEXTEXW(Result);
  TSepiTypeAlias.Create(Result, 'FINDTEXTEX', 'FINDTEXTEXA');
  TSepiTypeAlias.Create(Result, 'TFindTextExA', 'FINDTEXTEXA');
  TSepiTypeAlias.Create(Result, 'TFindTextExW', 'FINDTEXTEXW');
  TSepiTypeAlias.Create(Result, 'TFindTextEx', 'TFindTextExA');
  SepiImport_formatrange(Result);
  TSepiTypeAlias.Create(Result, 'TFormatRange', '_formatrange');
  TSepiTypeAlias.Create(Result, 'FORMATRANGE', '_formatrange');

  // Constants
  TSepiConstant.Create(Result, 'MAX_TAB_STOPS', MAX_TAB_STOPS);
  TSepiConstant.Create(Result, 'lDefaultTab', lDefaultTab);

  // Types
  TSepiArrayType.Create(Result, '$3',
    [0, MAX_TAB_STOPS - 1], TypeInfo(Longint), True);
  SepiImport_paraformat(Result);
  TSepiTypeAlias.Create(Result, 'TParaFormat', '_paraformat');
  TSepiTypeAlias.Create(Result, 'PARAFORMAT', '_paraformat');

  // Constants
  TSepiConstant.Create(Result, 'PFM_STARTINDENT', PFM_STARTINDENT);
  TSepiConstant.Create(Result, 'PFM_RIGHTINDENT', PFM_RIGHTINDENT);
  TSepiConstant.Create(Result, 'PFM_OFFSET', PFM_OFFSET);
  TSepiConstant.Create(Result, 'PFM_ALIGNMENT', PFM_ALIGNMENT);
  TSepiConstant.Create(Result, 'PFM_TABSTOPS', PFM_TABSTOPS);
  TSepiConstant.Create(Result, 'PFM_NUMBERING', PFM_NUMBERING);
  TSepiConstant.Create(Result, 'PFM_OFFSETINDENT', PFM_OFFSETINDENT);
  TSepiConstant.Create(Result, 'PFN_BULLET', PFN_BULLET);
  TSepiConstant.Create(Result, 'PFA_LEFT', PFA_LEFT);
  TSepiConstant.Create(Result, 'PFA_RIGHT', PFA_RIGHT);
  TSepiConstant.Create(Result, 'PFA_CENTER', PFA_CENTER);

  // Types
  TSepiArrayType.Create(Result, '$4',
    [0, LF_FACESIZE - 1], TypeInfo(AnsiChar), True);
  SepiImportCHARFORMAT2A(Result);
  TSepiArrayType.Create(Result, '$5',
    [0, LF_FACESIZE - 1], TypeInfo(WideChar), True);
  SepiImportCHARFORMAT2W(Result);
  TSepiTypeAlias.Create(Result, 'CHARFORMAT2', 'CHARFORMAT2A');
  TSepiTypeAlias.Create(Result, 'TCharFormat2A', 'CHARFORMAT2A');
  TSepiTypeAlias.Create(Result, 'TCharFormat2W', 'CHARFORMAT2W');
  TSepiTypeAlias.Create(Result, 'TCharFormat2', 'TCharFormat2A');

  // Constants
  TSepiConstant.Create(Result, 'CFM_EFFECTS', CFM_EFFECTS);
  TSepiConstant.Create(Result, 'CFM_ALL', CFM_ALL);
  TSepiConstant.Create(Result, 'PFM_ALL', PFM_ALL);
  TSepiConstant.Create(Result, 'CFM_SMALLCAPS', CFM_SMALLCAPS);
  TSepiConstant.Create(Result, 'CFM_ALLCAPS', CFM_ALLCAPS);
  TSepiConstant.Create(Result, 'CFM_HIDDEN', CFM_HIDDEN);
  TSepiConstant.Create(Result, 'CFM_OUTLINE', CFM_OUTLINE);
  TSepiConstant.Create(Result, 'CFM_SHADOW', CFM_SHADOW);
  TSepiConstant.Create(Result, 'CFM_EMBOSS', CFM_EMBOSS);
  TSepiConstant.Create(Result, 'CFM_IMPRINT', CFM_IMPRINT);
  TSepiConstant.Create(Result, 'CFM_DISABLED', CFM_DISABLED);
  TSepiConstant.Create(Result, 'CFM_REVISED', CFM_REVISED);
  TSepiConstant.Create(Result, 'CFM_BACKCOLOR', CFM_BACKCOLOR);
  TSepiConstant.Create(Result, 'CFM_LCID', CFM_LCID);
  TSepiConstant.Create(Result, 'CFM_UNDERLINETYPE', CFM_UNDERLINETYPE);
  TSepiConstant.Create(Result, 'CFM_WEIGHT', CFM_WEIGHT);
  TSepiConstant.Create(Result, 'CFM_SPACING', CFM_SPACING);
  TSepiConstant.Create(Result, 'CFM_KERNING', CFM_KERNING);
  TSepiConstant.Create(Result, 'CFM_STYLE', CFM_STYLE);
  TSepiConstant.Create(Result, 'CFM_ANIMATION', CFM_ANIMATION);
  TSepiConstant.Create(Result, 'CFM_REVAUTHOR', CFM_REVAUTHOR);
  TSepiConstant.Create(Result, 'CFE_SUBSCRIPT', CFE_SUBSCRIPT);
  TSepiConstant.Create(Result, 'CFE_SUPERSCRIPT', CFE_SUPERSCRIPT);
  TSepiConstant.Create(Result, 'CFM_SUBSCRIPT', CFM_SUBSCRIPT);
  TSepiConstant.Create(Result, 'CFM_SUPERSCRIPT', CFM_SUPERSCRIPT);
  TSepiConstant.Create(Result, 'CFM_EFFECTS2', CFM_EFFECTS2);
  TSepiConstant.Create(Result, 'CFM_ALL2', CFM_ALL2);
  TSepiConstant.Create(Result, 'CFE_SMALLCAPS', CFE_SMALLCAPS);
  TSepiConstant.Create(Result, 'CFE_ALLCAPS', CFE_ALLCAPS);
  TSepiConstant.Create(Result, 'CFE_HIDDEN', CFE_HIDDEN);
  TSepiConstant.Create(Result, 'CFE_OUTLINE', CFE_OUTLINE);
  TSepiConstant.Create(Result, 'CFE_SHADOW', CFE_SHADOW);
  TSepiConstant.Create(Result, 'CFE_EMBOSS', CFE_EMBOSS);
  TSepiConstant.Create(Result, 'CFE_IMPRINT', CFE_IMPRINT);
  TSepiConstant.Create(Result, 'CFE_DISABLED', CFE_DISABLED);
  TSepiConstant.Create(Result, 'CFE_REVISED', CFE_REVISED);
  TSepiConstant.Create(Result, 'CFE_AUTOBACKCOLOR', CFE_AUTOBACKCOLOR);
  TSepiConstant.Create(Result, 'CFU_CF1UNDERLINE', CFU_CF1UNDERLINE);
  TSepiConstant.Create(Result, 'CFU_INVERT', CFU_INVERT);
  TSepiConstant.Create(Result, 'CFU_UNDERLINEDOTTED', CFU_UNDERLINEDOTTED);
  TSepiConstant.Create(Result, 'CFU_UNDERLINEDOUBLE', CFU_UNDERLINEDOUBLE);
  TSepiConstant.Create(Result, 'CFU_UNDERLINEWORD', CFU_UNDERLINEWORD);
  TSepiConstant.Create(Result, 'CFU_UNDERLINE', CFU_UNDERLINE);
  TSepiConstant.Create(Result, 'CFU_UNDERLINENONE', CFU_UNDERLINENONE);

  // Types
  TSepiArrayType.Create(Result, '$6',
    [0, MAX_TAB_STOPS - 1], TypeInfo(Longint), True);
  SepiImportPARAFORMAT2(Result);
  TSepiTypeAlias.Create(Result, 'TParaFormat2', 'PARAFORMAT2');

  // Constants
  TSepiConstant.Create(Result, 'PFM_SPACEBEFORE', PFM_SPACEBEFORE);
  TSepiConstant.Create(Result, 'PFM_SPACEAFTER', PFM_SPACEAFTER);
  TSepiConstant.Create(Result, 'PFM_LINESPACING', PFM_LINESPACING);
  TSepiConstant.Create(Result, 'PFM_STYLE', PFM_STYLE);
  TSepiConstant.Create(Result, 'PFM_BORDER', PFM_BORDER);
  TSepiConstant.Create(Result, 'PFM_SHADING', PFM_SHADING);
  TSepiConstant.Create(Result, 'PFM_NUMBERINGSTYLE', PFM_NUMBERINGSTYLE);
  TSepiConstant.Create(Result, 'PFM_NUMBERINGTAB', PFM_NUMBERINGTAB);
  TSepiConstant.Create(Result, 'PFM_NUMBERINGSTART', PFM_NUMBERINGSTART);
  TSepiConstant.Create(Result, 'PFM_RTLPARA', PFM_RTLPARA);
  TSepiConstant.Create(Result, 'PFM_KEEP', PFM_KEEP);
  TSepiConstant.Create(Result, 'PFM_KEEPNEXT', PFM_KEEPNEXT);
  TSepiConstant.Create(Result, 'PFM_PAGEBREAKBEFORE', PFM_PAGEBREAKBEFORE);
  TSepiConstant.Create(Result, 'PFM_NOLINENUMBER', PFM_NOLINENUMBER);
  TSepiConstant.Create(Result, 'PFM_NOWIDOWCONTROL', PFM_NOWIDOWCONTROL);
  TSepiConstant.Create(Result, 'PFM_DONOTHYPHEN', PFM_DONOTHYPHEN);
  TSepiConstant.Create(Result, 'PFM_SIDEBYSIDE', PFM_SIDEBYSIDE);
  TSepiConstant.Create(Result, 'PFM_TABLE', PFM_TABLE);
  TSepiConstant.Create(Result, 'PFM_EFFECTS', PFM_EFFECTS);
  TSepiConstant.Create(Result, 'PFM_ALL2', PFM_ALL2);
  TSepiConstant.Create(Result, 'PFE_RTLPARA', PFE_RTLPARA);
  TSepiConstant.Create(Result, 'PFE_KEEP', PFE_KEEP);
  TSepiConstant.Create(Result, 'PFE_KEEPNEXT', PFE_KEEPNEXT);
  TSepiConstant.Create(Result, 'PFE_PAGEBREAKBEFORE', PFE_PAGEBREAKBEFORE);
  TSepiConstant.Create(Result, 'PFE_NOLINENUMBER', PFE_NOLINENUMBER);
  TSepiConstant.Create(Result, 'PFE_NOWIDOWCONTROL', PFE_NOWIDOWCONTROL);
  TSepiConstant.Create(Result, 'PFE_DONOTHYPHEN', PFE_DONOTHYPHEN);
  TSepiConstant.Create(Result, 'PFE_SIDEBYSIDE', PFE_SIDEBYSIDE);
  TSepiConstant.Create(Result, 'PFE_TABLEROW', PFE_TABLEROW);
  TSepiConstant.Create(Result, 'PFE_TABLECELLEND', PFE_TABLECELLEND);
  TSepiConstant.Create(Result, 'PFE_TABLECELL', PFE_TABLECELL);
  TSepiConstant.Create(Result, 'PFA_JUSTIFY', PFA_JUSTIFY);

  // Types
  TSepiPointerType.Create(Result, 'PMsgFilter', 'TMsgFilter', True);
  SepiImport_msgfilter(Result);
  TSepiTypeAlias.Create(Result, 'TMsgFilter', '_msgfilter');
  TSepiTypeAlias.Create(Result, 'MSGFILTER', '_msgfilter');
  TSepiPointerType.Create(Result, 'PReqSize', 'TReqSize', True);
  SepiImportTReqSize(Result);
  TSepiPointerType.Create(Result, 'PSelChange', 'TSelChange', True);
  SepiImport_selchange(Result);
  TSepiTypeAlias.Create(Result, 'TSelChange', '_selchange');
  TSepiTypeAlias.Create(Result, 'SELCHANGE', '_selchange');

  // Constants
  TSepiConstant.Create(Result, 'SEL_EMPTY', SEL_EMPTY);
  TSepiConstant.Create(Result, 'SEL_TEXT', SEL_TEXT);
  TSepiConstant.Create(Result, 'SEL_OBJECT', SEL_OBJECT);
  TSepiConstant.Create(Result, 'SEL_MULTICHAR', SEL_MULTICHAR);
  TSepiConstant.Create(Result, 'SEL_MULTIOBJECT', SEL_MULTIOBJECT);
  TSepiConstant.Create(Result, 'GCM_RIGHTMOUSEDROP', GCM_RIGHTMOUSEDROP);

  // Types
  SepiImportTEndDropFiles(Result);
  TSepiPointerType.Create(Result, 'PENProtected', 'TENProtected', True);
  SepiImport_enprotected(Result);
  TSepiTypeAlias.Create(Result, 'TENProtected', '_enprotected');
  TSepiTypeAlias.Create(Result, 'ENPROTECTED', '_enprotected');
  TSepiPointerType.Create(Result, 'PENSaveClipboard',
    'TENSaveClipboard', True);
  SepiImport_ensaveclipboard(Result);
  TSepiTypeAlias.Create(Result, 'TENSaveClipboard', '_ensaveclipboard');
  TSepiTypeAlias.Create(Result, 'ENSAVECLIPBOARD', '_ensaveclipboard');
  SepiImportENOLEOPFAILED(Result);
  TSepiTypeAlias.Create(Result, 'TENOleOpFailed', 'ENOLEOPFAILED');

  // Constants
  TSepiConstant.Create(Result, 'OLEOP_DOVERB', OLEOP_DOVERB);

  // Types
  SepiImportOBJECTPOSITIONS(Result);
  TSepiTypeAlias.Create(Result, 'TObjectPositions', 'OBJECTPOSITIONS');
  SepiImportENLINK(Result);
  TSepiTypeAlias.Create(Result, 'TENLink', 'ENLINK');
  SepiImport_encorrecttext(Result);
  TSepiTypeAlias.Create(Result, 'TENCorrectText', '_encorrecttext');
  TSepiTypeAlias.Create(Result, 'ENCORRECTTEXT', '_encorrecttext');
  SepiImport_punctuation(Result);
  TSepiTypeAlias.Create(Result, 'TPunctuation', '_punctuation');
  TSepiTypeAlias.Create(Result, 'PUNCTUATION', '_punctuation');
  SepiImport_compcolor(Result);
  TSepiTypeAlias.Create(Result, 'TCompColor', '_compcolor');
  TSepiTypeAlias.Create(Result, 'COMPCOLOR', '_compcolor');

  // Constants
  TSepiConstant.Create(Result, 'CF_RTF', CF_RTF);
  TSepiConstant.Create(Result, 'CF_RTFNOOBJS', CF_RTFNOOBJS);
  TSepiConstant.Create(Result, 'CF_RETEXTOBJ', CF_RETEXTOBJ);

  // Types
  SepiImport_repastespecial(Result);
  TSepiTypeAlias.Create(Result, 'TRepasteSpecial', '_repastespecial');
  TSepiTypeAlias.Create(Result, 'REPASTESPECIAL', '_repastespecial');
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(UNDONAMEID));

  // Constants
  TSepiConstant.Create(Result, 'GT_DEFAULT', GT_DEFAULT);
  TSepiConstant.Create(Result, 'GT_USECRLF', GT_USECRLF);

  // Types
  SepiImportGETTEXTEX(Result);
  TSepiTypeAlias.Create(Result, 'TGetTextEx', 'GETTEXTEX');

  // Constants
  TSepiConstant.Create(Result, 'GTL_DEFAULT', GTL_DEFAULT);
  TSepiConstant.Create(Result, 'GTL_USECRLF', GTL_USECRLF);
  TSepiConstant.Create(Result, 'GTL_PRECISE', GTL_PRECISE);
  TSepiConstant.Create(Result, 'GTL_CLOSE', GTL_CLOSE);
  TSepiConstant.Create(Result, 'GTL_NUMCHARS', GTL_NUMCHARS);
  TSepiConstant.Create(Result, 'GTL_NUMBYTES', GTL_NUMBYTES);

  // Types
  SepiImportGETTEXTLENGTHEX(Result);
  TSepiTypeAlias.Create(Result, 'TGetTextLengthEx', 'GETTEXTLENGTHEX');

  // Constants
  TSepiConstant.Create(Result, 'WCH_EMBEDDING', WCH_EMBEDDING);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('RichEdit', ImportUnit);
end.

