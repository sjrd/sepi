{-------------------------------------------------------------------------------
SepiDelphiCompiler - Example program for Sepi
As an example program, SepiDelphiCompiler is free of any usage. It is released
in the public domain.
-------------------------------------------------------------------------------}

{*
  Importe toutes les unités que Sepi importe en standard
  @author sjrd
  @version 1.0
*}
unit Imports;

interface

uses
  SepiImportsRTLSys, SepiImportsWinTypes, SepiImportsRTLCommon,
  SepiImportsSCL, SepiImportsVCLStd, SepiImportsSVCL, SepiImportsSDL,
  SepiReflectionCore;

procedure LoadAll(SepiRoot: TSepiRoot);

implementation

procedure LoadAll(SepiRoot: TSepiRoot);
begin
  SepiRoot.LoadUnit('SysConst');
  SepiRoot.LoadUnit('SysUtils');
  SepiRoot.LoadUnit('Types');
  SepiRoot.LoadUnit('Windows');
  SepiRoot.LoadUnit('Variants');
  SepiRoot.LoadUnit('VarUtils');
  SepiRoot.LoadUnit('AccCtrl');
  SepiRoot.LoadUnit('AclAPI');
  SepiRoot.LoadUnit('ActiveX');
  SepiRoot.LoadUnit('AspTlb');
  SepiRoot.LoadUnit('COMAdmin');
  SepiRoot.LoadUnit('CommCtrl');
  SepiRoot.LoadUnit('CommDlg');
  SepiRoot.LoadUnit('ComObj');
  SepiRoot.LoadUnit('ComSvcs');
  SepiRoot.LoadUnit('Cpl');
  SepiRoot.LoadUnit('DDEml');
  SepiRoot.LoadUnit('Dlgs');
  SepiRoot.LoadUnit('FlatSB');
  SepiRoot.LoadUnit('ImageHlp');
  SepiRoot.LoadUnit('Imm');
  SepiRoot.LoadUnit('Isapi');
  SepiRoot.LoadUnit('Isapi2');
  SepiRoot.LoadUnit('LZExpand');
  SepiRoot.LoadUnit('Mapi');
  SepiRoot.LoadUnit('Messages');
  SepiRoot.LoadUnit('MMSystem');
  SepiRoot.LoadUnit('msxml');
  SepiRoot.LoadUnit('Mtx');
  SepiRoot.LoadUnit('MultiMon');
  SepiRoot.LoadUnit('Nb30');
  SepiRoot.LoadUnit('Ns30Fix');
  SepiRoot.LoadUnit('Ns35Fix');
  SepiRoot.LoadUnit('Ns36Fix');
  SepiRoot.LoadUnit('Nsapi');
  SepiRoot.LoadUnit('OpenGL');
  SepiRoot.LoadUnit('PenWin');
  SepiRoot.LoadUnit('PsAPI');
  SepiRoot.LoadUnit('RegStr');
  SepiRoot.LoadUnit('RichEdit');
  SepiRoot.LoadUnit('ShellAPI');
  SepiRoot.LoadUnit('SHFolder');
  SepiRoot.LoadUnit('ShlObj');
  SepiRoot.LoadUnit('TlHelp32');
  SepiRoot.LoadUnit('UrlMon');
  SepiRoot.LoadUnit('UxTheme');
  SepiRoot.LoadUnit('WinInet');
  SepiRoot.LoadUnit('WinSock');
  SepiRoot.LoadUnit('WinSpool');
  SepiRoot.LoadUnit('WinSvc');
  SepiRoot.LoadUnit('Classes');
  SepiRoot.LoadUnit('ComConst');
  SepiRoot.LoadUnit('Contnrs');
  SepiRoot.LoadUnit('ConvUtils');
  SepiRoot.LoadUnit('DateUtils');
  SepiRoot.LoadUnit('HelpIntfs');
  SepiRoot.LoadUnit('IniFiles');
  SepiRoot.LoadUnit('Masks');
  SepiRoot.LoadUnit('MaskUtils');
  SepiRoot.LoadUnit('Math');
  SepiRoot.LoadUnit('ObjAuto');
  SepiRoot.LoadUnit('Registry');
  SepiRoot.LoadUnit('RTLConsts');
  SepiRoot.LoadUnit('StdConvs');
  SepiRoot.LoadUnit('StdVCL');
  SepiRoot.LoadUnit('StrUtils');
  SepiRoot.LoadUnit('SyncObjs');
  SepiRoot.LoadUnit('TypInfo');
  SepiRoot.LoadUnit('VarCmplx');
  SepiRoot.LoadUnit('VarConv');
  SepiRoot.LoadUnit('VCLCom');
  SepiRoot.LoadUnit('WideStrings');
  SepiRoot.LoadUnit('WideStrUtils');
  SepiRoot.LoadUnit('ZLib');
  SepiRoot.LoadUnit('ZLibConst');
  SepiRoot.LoadUnit('ScClasses');
  SepiRoot.LoadUnit('ScCompilerMagic');
  SepiRoot.LoadUnit('ScConsoleUtils');
  SepiRoot.LoadUnit('ScConsts');
  SepiRoot.LoadUnit('ScCoroutines');
  SepiRoot.LoadUnit('ScDateTimeUtils');
  SepiRoot.LoadUnit('ScDelphiLanguage');
  SepiRoot.LoadUnit('ScEnumerators');
  SepiRoot.LoadUnit('ScIntegerSets');
  SepiRoot.LoadUnit('ScInterfaces');
  SepiRoot.LoadUnit('ScLists');
  SepiRoot.LoadUnit('ScLOGFile');
  SepiRoot.LoadUnit('ScMaths');
  SepiRoot.LoadUnit('ScMD5');
  SepiRoot.LoadUnit('ScPipes');
  SepiRoot.LoadUnit('ScSerializer');
  SepiRoot.LoadUnit('ScStrUtils');
  SepiRoot.LoadUnit('ScSyncObjs');
  SepiRoot.LoadUnit('ScTypInfo');
  SepiRoot.LoadUnit('ScUtils');
  SepiRoot.LoadUnit('ScWindows');
  SepiRoot.LoadUnit('ScZLib');
  SepiRoot.LoadUnit('ScXML');
  SepiRoot.LoadUnit('ActnList');
  SepiRoot.LoadUnit('ClipBrd');
  SepiRoot.LoadUnit('ComCtrls');
  SepiRoot.LoadUnit('Controls');
  SepiRoot.LoadUnit('Dialogs');
  SepiRoot.LoadUnit('GraphUtil');
  SepiRoot.LoadUnit('ExtCtrls');
  SepiRoot.LoadUnit('Forms');
  SepiRoot.LoadUnit('Graphics');
  SepiRoot.LoadUnit('ImgList');
  SepiRoot.LoadUnit('ListActns');
  SepiRoot.LoadUnit('Menus');
  SepiRoot.LoadUnit('Printers');
  SepiRoot.LoadUnit('StdCtrls');
  SepiRoot.LoadUnit('ToolWin');
  SepiRoot.LoadUnit('SvEdits');
  SepiRoot.LoadUnit('SvImages');
  SepiRoot.LoadUnit('SvLabels');
  SepiRoot.LoadUnit('SdAbout');
  SepiRoot.LoadUnit('SdDialogs');
  SepiRoot.LoadUnit('SdNumber');
  SepiRoot.LoadUnit('SdPassword');
end;

end.

