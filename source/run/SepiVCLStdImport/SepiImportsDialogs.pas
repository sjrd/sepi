{*
  Importe l'unité Dialogs dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsDialogs;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, CommDlg, Printers, Dialogs;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTCommonDialog = class(TCommonDialog)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTOpenDialog = class(TOpenDialog)
  private
    function GetFileName: TFileName;
    function GetFilterIndex: Integer;
    procedure SetHistoryList(Value: TStrings);
    procedure SetInitialDir(const Value: string);
    function DoExecute_0(Func: Pointer): Bool;
    function DoExecute_1(Func: Pointer; ParentWnd: HWND): Bool;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTSaveDialog = class(TSaveDialog)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTColorDialog = class(TColorDialog)
  private
    procedure SetCustomColors(Value: TStrings);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTFontDialog = class(TFontDialog)
  private
    procedure SetFont(Value: TFont);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTPrinterSetupDialog = class(TPrinterSetupDialog)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTPrintDialog = class(TPrintDialog)
  private
    procedure SetNumCopies(Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTPageSetupDialog = class(TPageSetupDialog)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTFindDialog = class(TFindDialog)
  private
    function GetFindText: string;
    function GetLeft: Integer;
    function GetPosition: TPoint;
    function GetTop: Integer;
    procedure SetFindText(const Value: string);
    procedure SetLeft(Value: Integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetTop(Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTReplaceDialog = class(TReplaceDialog)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{----------------------}
{ TCommonDialog import }
{----------------------}

class function TSepiImportsTCommonDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCommonDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCtl3D', System.TypeInfo(Boolean));
    AddField('FDefWndProc', 'Pointer');
    AddField('FHelpContext', System.TypeInfo(THelpContext));
    AddField('FHandle', System.TypeInfo(HWnd));
    AddField('FRedirector', System.TypeInfo(TWinControl));
    AddField('FObjectInstance', 'Pointer');
    AddField('FTemplate', 'PChar');
    AddField('FTemplateModule', System.TypeInfo(HINST));
    AddField('FOnClose', System.TypeInfo(TNotifyEvent));
    AddField('FOnShow', System.TypeInfo(TNotifyEvent));

    AddMethod('WMDestroy', nil,
      'procedure(var Message: TWMDestroy)',
      mlkMessage, False, WM_DESTROY);
    AddMethod('WMInitDialog', nil,
      'procedure(var Message: TWMInitDialog)',
      mlkMessage, False, WM_INITDIALOG);
    AddMethod('WMNCDestroy', nil,
      'procedure(var Message: TWMNCDestroy)',
      mlkMessage, False, WM_NCDESTROY);
    AddMethod('MainWndProc', nil,
      'procedure(var Message: TMessage)');

    CurrentVisibility := mvProtected;

    AddMethod('DoClose', @TSepiImportsTCommonDialog.DoClose,
      'procedure',
      mlkDynamic);
    AddMethod('DoShow', @TSepiImportsTCommonDialog.DoShow,
      'procedure',
      mlkDynamic);
    AddMethod('WndProc', @TSepiImportsTCommonDialog.WndProc,
      'procedure(var Message: TMessage)',
      mlkVirtual);
    AddMethod('MessageHook', @TSepiImportsTCommonDialog.MessageHook,
      'function(var Msg: TMessage): Boolean',
      mlkVirtual);
    AddMethod('TaskModalDialog', @TSepiImportsTCommonDialog.TaskModalDialog,
      'function(DialogFunc: Pointer; var DialogData): Bool',
      mlkVirtual);

    AddProperty('Template', 'property: PChar',
      'FTemplate', 'FTemplate');
    AddProperty('TemplateModule', 'property: HINST',
      'FTemplateModule', 'FTemplateModule');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCommonDialog.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCommonDialog.Destroy,
      'destructor',
      mlkOverride);
    AddOverloadedMethod('Execute', nil,
      'function: Boolean',
      mlkVirtual);
    AddOverloadedMethod('Execute', nil,
      'function(ParentWnd: HWND): Boolean',
      mlkVirtual, True);
    AddMethod('DefaultHandler', @TSepiImportsTCommonDialog.DefaultHandler,
      'procedure(var Message)',
      mlkOverride);

    AddProperty('Handle', 'property: HWnd',
      'FHandle', '');

    CurrentVisibility := mvPublished;

    AddProperty('Ctl3D', 'property: Boolean',
      'FCtl3D', 'FCtl3D',
      NoIndex, Integer(True));
    AddProperty('HelpContext', 'property: THelpContext',
      'FHelpContext', 'FHelpContext',
      NoIndex, 0);
    AddProperty('OnClose', 'property: TNotifyEvent',
      'FOnClose', 'FOnClose');
    AddProperty('OnShow', 'property: TNotifyEvent',
      'FOnShow', 'FOnShow');

    Complete;
  end;
end;

{--------------------}
{ TOpenDialog import }
{--------------------}

function TSepiImportsTOpenDialog.GetFileName: TFileName;
begin
  Result := FileName;
end;

function TSepiImportsTOpenDialog.GetFilterIndex: Integer;
begin
  Result := FilterIndex;
end;

procedure TSepiImportsTOpenDialog.SetHistoryList(Value: TStrings);
begin
  HistoryList := Value;
end;

procedure TSepiImportsTOpenDialog.SetInitialDir(const Value: string);
begin
  InitialDir := Value;
end;

function TSepiImportsTOpenDialog.DoExecute_0(Func: Pointer): Bool;
begin
  Result := DoExecute(Func);
end;

function TSepiImportsTOpenDialog.DoExecute_1(Func: Pointer;
  ParentWnd: HWND): Bool;
begin
  Result := DoExecute(Func, ParentWnd);
end;

class function TSepiImportsTOpenDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
const
  DefaultOptions: TOpenOptions = [ofHideReadOnly, ofEnableSizing];
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TOpenDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHistoryList', System.TypeInfo(TStrings));
    AddField('FOptions', System.TypeInfo(TOpenOptions));
    AddField('FFilter', System.TypeInfo(string));
    AddField('FFilterIndex', System.TypeInfo(Integer));
    AddField('FCurrentFilterIndex', System.TypeInfo(Integer));
    AddField('FInitialDir', System.TypeInfo(string));
    AddField('FTitle', System.TypeInfo(string));
    AddField('FDefaultExt', System.TypeInfo(string));
    AddField('FFileName', System.TypeInfo(TFileName));
    AddField('FFiles', System.TypeInfo(TStrings));
    AddField('FFileEditStyle', System.TypeInfo(TFileEditStyle));
    AddField('FOnSelectionChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnFolderChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnTypeChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnCanClose', System.TypeInfo(TCloseQueryEvent));
    AddField('FOnIncludeItem', System.TypeInfo(TIncludeItemEvent));
    AddField('FOptionsEx', System.TypeInfo(TOpenOptionsEx));

    AddMethod('GetFileName', @TSepiImportsTOpenDialog.GetFileName,
      'function: TFileName');
    AddMethod('GetFilterIndex', @TSepiImportsTOpenDialog.GetFilterIndex,
      'function: Integer');
    AddMethod('ReadFileEditStyle', nil,
      'procedure(Reader: TReader)');
    AddMethod('SetHistoryList', @TSepiImportsTOpenDialog.SetHistoryList,
      'procedure(Value: TStrings)');
    AddMethod('SetInitialDir', @TSepiImportsTOpenDialog.SetInitialDir,
      'procedure(const Value: string)');

    CurrentVisibility := mvProtected;

    AddMethod('CanClose', @TSepiImportsTOpenDialog.CanClose,
      'function(var OpenFileName: TOpenFileName): Boolean');
    AddMethod('DoCanClose', @TSepiImportsTOpenDialog.DoCanClose,
      'function: Boolean',
      mlkDynamic);
    AddOverloadedMethod('DoExecute', @TSepiImportsTOpenDialog.DoExecute_0,
      'function(Func: Pointer): Bool');
    AddOverloadedMethod('DoExecute', @TSepiImportsTOpenDialog.DoExecute_1,
      'function(Func: Pointer; ParentWnd: HWND): Bool');
    AddMethod('DoSelectionChange', @TSepiImportsTOpenDialog.DoSelectionChange,
      'procedure',
      mlkDynamic);
    AddMethod('DoFolderChange', @TSepiImportsTOpenDialog.DoFolderChange,
      'procedure',
      mlkDynamic);
    AddMethod('DoTypeChange', @TSepiImportsTOpenDialog.DoTypeChange,
      'procedure',
      mlkDynamic);
    AddMethod('DoIncludeItem', @TSepiImportsTOpenDialog.DoIncludeItem,
      'procedure(const OFN: TOFNotifyEx; var Include: Boolean)',
      mlkDynamic);
    AddMethod('DefineProperties', @TSepiImportsTOpenDialog.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('GetFileNames', @TSepiImportsTOpenDialog.GetFileNames,
      'procedure(var OpenFileName: TOpenFileName)');
    AddMethod('GetStaticRect', @TSepiImportsTOpenDialog.GetStaticRect,
      'function: TRect',
      mlkVirtual);
    AddMethod('WndProc', @TSepiImportsTOpenDialog.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTOpenDialog.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTOpenDialog.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Execute', @TSepiImportsTOpenDialog.Execute,
      'function(ParentWnd: HWND): Boolean',
      mlkOverride);

    AddProperty('FileEditStyle', 'property: TFileEditStyle',
      'FFileEditStyle', 'FFileEditStyle');
    AddProperty('Files', 'property: TStrings',
      'FFiles', '');
    AddProperty('HistoryList', 'property: TStrings',
      'FHistoryList', 'SetHistoryList');

    CurrentVisibility := mvPublished;

    AddProperty('DefaultExt', 'property: string',
      'FDefaultExt', 'FDefaultExt');
    AddProperty('FileName', 'property: TFileName',
      'GetFileName', 'FFileName');
    AddProperty('Filter', 'property: string',
      'FFilter', 'FFilter');
    AddProperty('FilterIndex', 'property: Integer',
      'GetFilterIndex', 'FFilterIndex',
      NoIndex, 1);
    AddProperty('InitialDir', 'property: string',
      'FInitialDir', 'SetInitialDir');
    AddProperty('Options', 'property: TOpenOptions',
      'FOptions', 'FOptions',
      NoIndex, Integer(DefaultOptions));
    AddProperty('OptionsEx', 'property: TOpenOptionsEx',
      'FOptionsEx', 'FOptionsEx',
      NoIndex, 0);
    AddProperty('Title', 'property: string',
      'FTitle', 'FTitle');
    AddProperty('OnCanClose', 'property: TCloseQueryEvent',
      'FOnCanClose', 'FOnCanClose');
    AddProperty('OnFolderChange', 'property: TNotifyEvent',
      'FOnFolderChange', 'FOnFolderChange');
    AddProperty('OnSelectionChange', 'property: TNotifyEvent',
      'FOnSelectionChange', 'FOnSelectionChange');
    AddProperty('OnTypeChange', 'property: TNotifyEvent',
      'FOnTypeChange', 'FOnTypeChange');
    AddProperty('OnIncludeItem', 'property: TIncludeItemEvent',
      'FOnIncludeItem', 'FOnIncludeItem');

    Complete;
  end;
end;

{--------------------}
{ TSaveDialog import }
{--------------------}

class function TSepiImportsTSaveDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSaveDialog));

  with Result do
  begin
    AddMethod('Execute', @TSepiImportsTSaveDialog.Execute,
      'function(ParentWnd: HWND): Boolean',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ TColorDialog import }
{---------------------}

procedure TSepiImportsTColorDialog.SetCustomColors(Value: TStrings);
begin
  CustomColors := Value;
end;

class function TSepiImportsTColorDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TColorDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FColor', System.TypeInfo(TColor));
    AddField('FOptions', System.TypeInfo(TColorDialogOptions));
    AddField('FCustomColors', System.TypeInfo(TStrings));

    AddMethod('SetCustomColors', @TSepiImportsTColorDialog.SetCustomColors,
      'procedure(Value: TStrings)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTColorDialog.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTColorDialog.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Execute', @TSepiImportsTColorDialog.Execute,
      'function(ParentWnd: HWND): Boolean',
      mlkOverride);

    CurrentVisibility := mvPublished;

    AddProperty('Color', 'property: TColor',
      'FColor', 'FColor',
      NoIndex, Integer(clBlack));
    RedefineProperty('Ctl3D',
      '', '', Integer(True));
    AddProperty('CustomColors', 'property: TStrings',
      'FCustomColors', 'SetCustomColors');
    AddProperty('Options', 'property: TColorDialogOptions',
      'FOptions', 'FOptions',
      NoIndex, 0);

    Complete;
  end;
end;

{--------------------}
{ TFontDialog import }
{--------------------}

procedure TSepiImportsTFontDialog.SetFont(Value: TFont);
begin
  Font := Value;
end;

class function TSepiImportsTFontDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
const
  DefaultOptions: TFontDialogOptions = [fdEffects];
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TFontDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFont', System.TypeInfo(TFont));
    AddField('FDevice', System.TypeInfo(TFontDialogDevice));
    AddField('FOptions', System.TypeInfo(TFontDialogOptions));
    AddField('FOnApply', System.TypeInfo(TFDApplyEvent));
    AddField('FMinFontSize', System.TypeInfo(Integer));
    AddField('FMaxFontSize', System.TypeInfo(Integer));
    AddField('FFontCharsetModified', System.TypeInfo(Boolean));
    AddField('FFontColorModified', System.TypeInfo(Boolean));

    AddMethod('DoApply', nil,
      'procedure(Wnd: HWND)');
    AddMethod('SetFont', @TSepiImportsTFontDialog.SetFont,
      'procedure(Value: TFont)');
    AddMethod('UpdateFromLogFont', nil,
      'procedure(const LogFont: TLogFont)');

    CurrentVisibility := mvProtected;

    AddMethod('Apply', @TSepiImportsTFontDialog.Apply,
      'procedure(Wnd: HWND)',
      mlkDynamic);
    AddMethod('WndProc', @TSepiImportsTFontDialog.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTFontDialog.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTFontDialog.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Execute', @TSepiImportsTFontDialog.Execute,
      'function(ParentWnd: HWND): Boolean',
      mlkOverride);

    CurrentVisibility := mvPublished;

    AddProperty('Font', 'property: TFont',
      'FFont', 'SetFont');
    AddProperty('Device', 'property: TFontDialogDevice',
      'FDevice', 'FDevice',
      NoIndex, Integer(fdScreen));
    AddProperty('MinFontSize', 'property: Integer',
      'FMinFontSize', 'FMinFontSize',
      NoIndex, 0);
    AddProperty('MaxFontSize', 'property: Integer',
      'FMaxFontSize', 'FMaxFontSize',
      NoIndex, 0);
    AddProperty('Options', 'property: TFontDialogOptions',
      'FOptions', 'FOptions',
      NoIndex, Word(DefaultOptions));
    AddProperty('OnApply', 'property: TFDApplyEvent',
      'FOnApply', 'FOnApply');

    Complete;
  end;
end;

{----------------------------}
{ TPrinterSetupDialog import }
{----------------------------}

class function TSepiImportsTPrinterSetupDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPrinterSetupDialog));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Execute', @TSepiImportsTPrinterSetupDialog.Execute,
      'function(ParentWnd: HWND): Boolean',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ TPrintDialog import }
{---------------------}

procedure TSepiImportsTPrintDialog.SetNumCopies(Value: Integer);
begin
  Copies := Value;
end;

class function TSepiImportsTPrintDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPrintDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFromPage', System.TypeInfo(Integer));
    AddField('FToPage', System.TypeInfo(Integer));
    AddField('FCollate', System.TypeInfo(Boolean));
    AddField('FOptions', System.TypeInfo(TPrintDialogOptions));
    AddField('FPrintToFile', System.TypeInfo(Boolean));
    AddField('FPrintRange', System.TypeInfo(TPrintRange));
    AddField('FMinPage', System.TypeInfo(Integer));
    AddField('FMaxPage', System.TypeInfo(Integer));
    AddField('FCopies', System.TypeInfo(Integer));

    AddMethod('SetNumCopies', @TSepiImportsTPrintDialog.SetNumCopies,
      'procedure(Value: Integer)');

    CurrentVisibility := mvPublic;

    AddMethod('Execute', @TSepiImportsTPrintDialog.Execute,
      'function(ParentWnd: HWND): Boolean',
      mlkOverride);

    CurrentVisibility := mvPublished;

    AddProperty('Collate', 'property: Boolean',
      'FCollate', 'FCollate',
      NoIndex, Integer(False));
    AddProperty('Copies', 'property: Integer',
      'FCopies', 'SetNumCopies',
      NoIndex, 0);
    AddProperty('FromPage', 'property: Integer',
      'FFromPage', 'FFromPage',
      NoIndex, 0);
    AddProperty('MinPage', 'property: Integer',
      'FMinPage', 'FMinPage',
      NoIndex, 0);
    AddProperty('MaxPage', 'property: Integer',
      'FMaxPage', 'FMaxPage',
      NoIndex, 0);
    AddProperty('Options', 'property: TPrintDialogOptions',
      'FOptions', 'FOptions',
      NoIndex, 0);
    AddProperty('PrintToFile', 'property: Boolean',
      'FPrintToFile', 'FPrintToFile',
      NoIndex, Integer(False));
    AddProperty('PrintRange', 'property: TPrintRange',
      'FPrintRange', 'FPrintRange',
      NoIndex, Integer(prAllPages));
    AddProperty('ToPage', 'property: Integer',
      'FToPage', 'FToPage',
      NoIndex, 0);

    Complete;
  end;
end;

{-------------------------}
{ TPageSetupDialog import }
{-------------------------}

class function TSepiImportsTPageSetupDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
const
  DefaultOptions: TPageSetupDialogOptions = [psoDefaultMinMargins];
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPageSetupDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOptions', System.TypeInfo(TPageSetupDialogOptions));
    AddField('FMinMarginLeft', System.TypeInfo(Integer));
    AddField('FMinMarginTop', System.TypeInfo(Integer));
    AddField('FMinMarginRight', System.TypeInfo(Integer));
    AddField('FMinMarginBottom', System.TypeInfo(Integer));
    AddField('FMarginLeft', System.TypeInfo(Integer));
    AddField('FMarginTop', System.TypeInfo(Integer));
    AddField('FMarginRight', System.TypeInfo(Integer));
    AddField('FMarginBottom', System.TypeInfo(Integer));
    AddField('FPageWidth', System.TypeInfo(Integer));
    AddField('FPageHeight', System.TypeInfo(Integer));
    AddField('FPageSetupDlgRec', 'TPageSetupDlg');
    AddField('FBeforePaint', System.TypeInfo(TPageSetupBeforePaintEvent));
    AddField('FUnits', System.TypeInfo(TPageMeasureUnits));
    AddField('FOnDrawRetAddress', System.TypeInfo(TPaintPageEvent));
    AddField('FOnDrawMinMargin', System.TypeInfo(TPaintPageEvent));
    AddField('FOnDrawEnvStamp', System.TypeInfo(TPaintPageEvent));
    AddField('FOnDrawFullPage', System.TypeInfo(TPaintPageEvent));
    AddField('FOnDrawGreekText', System.TypeInfo(TPaintPageEvent));
    AddField('FOnDrawMargin', System.TypeInfo(TPaintPageEvent));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPageSetupDialog.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Execute', @TSepiImportsTPageSetupDialog.Execute,
      'function(ParentWnd: HWND): Boolean',
      mlkOverride);
    AddMethod('GetDefaults', @TSepiImportsTPageSetupDialog.GetDefaults,
      'function: Boolean');

    AddProperty('PageSetupDlgRec', 'property: TPageSetupDlg',
      'FPageSetupDlgRec', '');

    CurrentVisibility := mvPublished;

    AddProperty('MinMarginLeft', 'property: Integer',
      'FMinMarginLeft', 'FMinMarginLeft');
    AddProperty('MinMarginTop', 'property: Integer',
      'FMinMarginTop', 'FMinMarginTop');
    AddProperty('MinMarginRight', 'property: Integer',
      'FMinMarginRight', 'FMinMarginRight');
    AddProperty('MinMarginBottom', 'property: Integer',
      'FMinMarginBottom', 'FMinMarginBottom');
    AddProperty('MarginLeft', 'property: Integer',
      'FMarginLeft', 'FMarginLeft');
    AddProperty('MarginTop', 'property: Integer',
      'FMarginTop', 'FMarginTop');
    AddProperty('MarginRight', 'property: Integer',
      'FMarginRight', 'FMarginRight');
    AddProperty('MarginBottom', 'property: Integer',
      'FMarginBottom', 'FMarginBottom');
    AddProperty('Options', 'property: TPageSetupDialogOptions',
      'FOptions', 'FOptions',
      NoIndex, Word(DefaultOptions));
    AddProperty('PageWidth', 'property: Integer',
      'FPageWidth', 'FPageWidth');
    AddProperty('PageHeight', 'property: Integer',
      'FPageHeight', 'FPageHeight');
    AddProperty('Units', 'property: TPageMeasureUnits',
      'FUnits', 'FUnits',
      NoIndex, Integer(pmDefault));
    AddProperty('BeforePaint', 'property: TPageSetupBeforePaintEvent',
      'FBeforePaint', 'FBeforePaint');
    AddProperty('OnDrawFullPage', 'property: TPaintPageEvent',
      'FOnDrawFullPage', 'FOnDrawFullPage');
    AddProperty('OnDrawMinMargin', 'property: TPaintPageEvent',
      'FOnDrawMinMargin', 'FOnDrawMinMargin');
    AddProperty('OnDrawMargin', 'property: TPaintPageEvent',
      'FOnDrawMargin', 'FOnDrawMargin');
    AddProperty('OnDrawGreekText', 'property: TPaintPageEvent',
      'FOnDrawGreekText', 'FOnDrawGreekText');
    AddProperty('OnDrawEnvStamp', 'property: TPaintPageEvent',
      'FOnDrawEnvStamp', 'FOnDrawEnvStamp');
    AddProperty('OnDrawRetAddress', 'property: TPaintPageEvent',
      'FOnDrawRetAddress', 'FOnDrawRetAddress');

    Complete;
  end;
end;

{--------------------}
{ TFindDialog import }
{--------------------}

function TSepiImportsTFindDialog.GetFindText: string;
begin
  Result := FindText;
end;

function TSepiImportsTFindDialog.GetLeft: Integer;
begin
  Result := Left;
end;

function TSepiImportsTFindDialog.GetPosition: TPoint;
begin
  Result := Position;
end;

function TSepiImportsTFindDialog.GetTop: Integer;
begin
  Result := Top;
end;

procedure TSepiImportsTFindDialog.SetFindText(const Value: string);
begin
  FindText := Value;
end;

procedure TSepiImportsTFindDialog.SetLeft(Value: Integer);
begin
  Left := Value;
end;

procedure TSepiImportsTFindDialog.SetPosition(const Value: TPoint);
begin
  Position := Value;
end;

procedure TSepiImportsTFindDialog.SetTop(Value: Integer);
begin
  Top := Value;
end;

class function TSepiImportsTFindDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
const
  DefaultOptions: TFindOptions = [frDown];
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TFindDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOptions', System.TypeInfo(TFindOptions));
    AddField('FPosition', 'TPoint');
    AddField('FFindReplaceFunc', 'TFindReplaceFunc');
    AddField('FOnFind', System.TypeInfo(TNotifyEvent));
    AddField('FOnReplace', System.TypeInfo(TNotifyEvent));
    AddField('FFindHandle', System.TypeInfo(HWnd));
    AddField('FFindReplace', 'TFindReplace');
    AddField('FFindText', '$1');
    AddField('FReplaceText', '$2');

    AddMethod('GetFindText', @TSepiImportsTFindDialog.GetFindText,
      'function: string');
    AddMethod('GetLeft', @TSepiImportsTFindDialog.GetLeft,
      'function: Integer');
    AddMethod('GetPosition', @TSepiImportsTFindDialog.GetPosition,
      'function: TPoint');
    AddMethod('GetReplaceText', nil,
      'function: string');
    AddMethod('GetTop', @TSepiImportsTFindDialog.GetTop,
      'function: Integer');
    AddMethod('SetFindText', @TSepiImportsTFindDialog.SetFindText,
      'procedure(const Value: string)');
    AddMethod('SetLeft', @TSepiImportsTFindDialog.SetLeft,
      'procedure(Value: Integer)');
    AddMethod('SetPosition', @TSepiImportsTFindDialog.SetPosition,
      'procedure(const Value: TPoint)');
    AddMethod('SetReplaceText', nil,
      'procedure(const Value: string)');
    AddMethod('SetTop', @TSepiImportsTFindDialog.SetTop,
      'procedure(Value: Integer)');

    AddProperty('ReplaceText', 'property: string',
      'GetReplaceText', 'SetReplaceText');
    AddProperty('OnReplace', 'property: TNotifyEvent',
      'FOnReplace', 'FOnReplace');

    CurrentVisibility := mvProtected;

    AddMethod('MessageHook', @TSepiImportsTFindDialog.MessageHook,
      'function(var Msg: TMessage): Boolean',
      mlkOverride);
    AddMethod('Find', @TSepiImportsTFindDialog.Find,
      'procedure',
      mlkDynamic);
    AddMethod('Replace', @TSepiImportsTFindDialog.Replace,
      'procedure',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTFindDialog.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTFindDialog.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('CloseDialog', @TSepiImportsTFindDialog.CloseDialog,
      'procedure');
    AddMethod('Execute', @TSepiImportsTFindDialog.Execute,
      'function(ParentWnd: HWND): Boolean',
      mlkOverride);

    AddProperty('Left', 'property: Integer',
      'GetLeft', 'SetLeft');
    AddProperty('Position', 'property: TPoint',
      'GetPosition', 'SetPosition');
    AddProperty('Top', 'property: Integer',
      'GetTop', 'SetTop');

    CurrentVisibility := mvPublished;

    AddProperty('FindText', 'property: string',
      'GetFindText', 'SetFindText');
    AddProperty('Options', 'property: TFindOptions',
      'FOptions', 'FOptions',
      NoIndex, Word(DefaultOptions));
    AddProperty('OnFind', 'property: TNotifyEvent',
      'FOnFind', 'FOnFind');

    Complete;
  end;
end;

{-----------------------}
{ TReplaceDialog import }
{-----------------------}

class function TSepiImportsTReplaceDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TReplaceDialog));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTReplaceDialog.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('ReplaceText');
    RedefineProperty('OnReplace');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'Dialogs',
    ['Windows', 'Messages', 'SysUtils', 'CommDlgTypes', 'Printers', 'Classes',
    'Graphics', 'Controls', 'Forms', 'StdCtrls']);

  // Constants
  TSepiConstant.Create(Result, 'MaxCustomColors', MaxCustomColors);

  // Types
  TSepiImportsTCommonDialog.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOpenOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOpenOptions));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOpenOptionEx));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOpenOptionsEx));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFileEditStyle));
  TSepiTypeAlias.Create(Result, 'TOFNotifyEx', TypeInfo(CommDlg.TOFNotifyEx));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TIncludeItemEvent));
  TSepiImportsTOpenDialog.SepiImport(Result);
  TSepiImportsTSaveDialog.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TColorDialogOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TColorDialogOptions));
  TSepiArrayType.Create(Result, 'TCustomColors',
    [0, MaxCustomColors - 1], TypeInfo(Longint), True);
  TSepiImportsTColorDialog.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontDialogOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontDialogOptions));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontDialogDevice));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFDApplyEvent));
  TSepiImportsTFontDialog.SepiImport(Result);
  TSepiImportsTPrinterSetupDialog.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPrintRange));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPrintDialogOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPrintDialogOptions));
  TSepiImportsTPrintDialog.SepiImport(Result);
  TSepiTypeAlias.Create(Result, 'TPrinterOrientation',
    TypeInfo(Printers.TPrinterOrientation));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPageSetupDialogOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPageSetupDialogOptions));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPrinterKind));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPageType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPageSetupBeforePaintEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPageMeasureUnits));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPaintPageEvent));
  TSepiImportsTPageSetupDialog.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFindOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFindOptions));
  TSepiMethodRefType.Create(Result, 'TFindReplaceFunc',
    'function(var FindReplace: TFindReplace): HWnd', False, ccStdCall);
  TSepiArrayType.Create(Result, '$1',
    [0, 255], TypeInfo(Char), True);
  TSepiArrayType.Create(Result, '$2',
    [0, 255], TypeInfo(Char), True);
  TSepiImportsTFindDialog.SepiImport(Result);
  TSepiImportsTReplaceDialog.SepiImport(Result);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMsgDlgType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMsgDlgBtn));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMsgDlgButtons));

  // Constants
  //TSepiConstant.Create(Result, 'mbYesNo', mbYesNo);
  //TSepiConstant.Create(Result, 'mbYesNoCancel', mbYesNoCancel);
  //TSepiConstant.Create(Result, 'mbYesAllNoAllCancel', mbYesAllNoAllCancel);
  //TSepiConstant.Create(Result, 'mbOKCancel', mbOKCancel);
  //TSepiConstant.Create(Result, 'mbAbortRetryIgnore', mbAbortRetryIgnore);
  //TSepiConstant.Create(Result, 'mbAbortIgnore', mbAbortIgnore);

  // Routines
  TSepiMethod.Create(Result, 'CreateMessageDialog', @CreateMessageDialog,
    'function(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons ) : TForm');
  TSepiMethod.Create(Result, 'MessageDlg', @MessageDlg,
    'function(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons ; HelpCtx: Longint ) : Integer');
  TSepiMethod.Create(Result, 'MessageDlgPos', @MessageDlgPos,
    'function(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons ; HelpCtx: Longint ; X, Y: Integer ) : Integer');
  TSepiMethod.Create(Result, 'MessageDlgPosHelp', @MessageDlgPosHelp,
    'function(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons ; HelpCtx: Longint ; X, Y: Integer ; const HelpFileName: string ) : Integer');
  TSepiMethod.Create(Result, 'ShowMessage', @ShowMessage,
    'procedure(const Msg: string)');
  TSepiMethod.Create(Result, 'ShowMessageFmt', @ShowMessageFmt,
    'procedure(const Msg: string; Params: array of const)');
  TSepiMethod.Create(Result, 'ShowMessagePos', @ShowMessagePos,
    'procedure(const Msg: string; X, Y: Integer)');
  TSepiMethod.Create(Result, 'InputBox', @InputBox,
    'function(const ACaption, APrompt, ADefault: string): string');
  TSepiMethod.Create(Result, 'InputQuery', @InputQuery,
    'function(const ACaption, APrompt: string; var Value: string ) : Boolean');
  TSepiMethod.Create(Result, 'PromptForFileName', @PromptForFileName,
    'function(var AFileName: string; const AFilter: string = ''''; const ADefaultExt: string = '''' ; const ATitle: string = '''' ; const AInitialDir: string = '''' ; SaveDialog: Boolean = False ) : Boolean');

  // Global variables
  TSepiVariable.Create(Result, 'ForceCurrentDirectory',
    ForceCurrentDirectory, TypeInfo(Boolean));

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Dialogs', ImportUnit);
end.

