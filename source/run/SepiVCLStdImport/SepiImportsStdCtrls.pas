{*
  Importe l'unité StdCtrls dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsStdCtrls;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Windows, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTCustomGroupBox = class(TCustomGroupBox)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTGroupBox = class(TGroupBox)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomLabel = class(TCustomLabel)
  private
    function GetTransparent: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetEllipsisPosition(Value: TEllipsisPosition);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetLayout(Value: TTextLayout);
    procedure SetWordWrap(Value: Boolean);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTLabel = class(TLabel)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomEdit = class(TCustomEdit)
  private
    function GetModified: Boolean;
    function GetCanUndo: Boolean;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetHideSelection(Value: Boolean);
    procedure SetMaxLength(Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetOEMConvert(Value: Boolean);
    procedure SetPasswordChar(Value: Char);
    procedure SetReadOnly(Value: Boolean);
    procedure SetSelText(const Value: string);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTEdit = class(TEdit)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomMemo = class(TCustomMemo)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMemo = class(TMemo)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomComboBoxStrings = class(TCustomComboBoxStrings)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomCombo = class(TCustomCombo)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomComboBox = class(TCustomComboBox)
  private
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetSelText(const Value: string);
    procedure SetSorted(Value: Boolean);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTComboBox = class(TComboBox)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTButtonActionLink = class(TButtonActionLink)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTButtonControl = class(TButtonControl)
  private
    procedure SetWordWrap(const Value: Boolean);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTButton = class(TButton)
  private
    procedure SetDefault(Value: Boolean);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomCheckBox = class(TCustomCheckBox)
  private
    procedure SetAlignment(Value: TLeftRight);
    procedure SetState(Value: TCheckBoxState);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCheckBox = class(TCheckBox)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTRadioButton = class(TRadioButton)
  private
    procedure SetAlignment(Value: TLeftRight);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomListBox = class(TCustomListBox)
  private
    function GetItemHeight: Integer;
    function GetTopIndex: Integer;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColumns(Value: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetExtendedSelect(Value: Boolean);
    procedure SetIntegralHeight(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure SetStyle(Value: TListBoxStyle);
    procedure SetTabWidth(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    function GetScrollWidth: Integer;
    procedure SetScrollWidth(const Value: Integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTListBox = class(TListBox)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScrollBar = class(TScrollBar)
  private
    procedure SetKind(Value: TScrollBarKind);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetPageSize(Value: Integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomStaticText = class(TCustomStaticText)
  private
    procedure SetAlignment(Value: TAlignment);
    procedure SetBorderStyle(Value: TStaticBorderStyle);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    function GetTransparent: Boolean;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTStaticText = class(TStaticText)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{------------------------}
{ TCustomGroupBox import }
{------------------------}

class function TSepiImportsTCustomGroupBox.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomGroupBox));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('CMDialogChar', nil,
      'procedure(var Message: TCMDialogChar)',
      mlkMessage, False, CM_DIALOGCHAR);
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('WMSize', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_SIZE);

    CurrentVisibility := mvProtected;

    AddMethod('AdjustClientRect', @TSepiImportsTCustomGroupBox.AdjustClientRect,
      'procedure(var Rect: TRect)',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTCustomGroupBox.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('Paint', @TSepiImportsTCustomGroupBox.Paint,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomGroupBox.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    Complete;
  end;
end;

{------------------}
{ TGroupBox import }
{------------------}

class function TSepiImportsTGroupBox.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TGroupBox));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('Caption');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ParentBackground',
      '', '', Integer(True));
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('OnAlignInsertBefore');
    RedefineProperty('OnAlignPosition');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnGetSiteInfo');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{---------------------}
{ TCustomLabel import }
{---------------------}

function TSepiImportsTCustomLabel.GetTransparent: Boolean;
begin
  Result := Transparent;
end;

procedure TSepiImportsTCustomLabel.SetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TSepiImportsTCustomLabel.SetEllipsisPosition(Value: TEllipsisPosition);
begin
  EllipsisPosition := Value;
end;

procedure TSepiImportsTCustomLabel.SetFocusControl(Value: TWinControl);
begin
  FocusControl := Value;
end;

procedure TSepiImportsTCustomLabel.SetShowAccelChar(Value: Boolean);
begin
  ShowAccelChar := Value;
end;

procedure TSepiImportsTCustomLabel.SetTransparent(Value: Boolean);
begin
  Transparent := Value;
end;

procedure TSepiImportsTCustomLabel.SetLayout(Value: TTextLayout);
begin
  Layout := Value;
end;

procedure TSepiImportsTCustomLabel.SetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

class function TSepiImportsTCustomLabel.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomLabel));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFocusControl', System.TypeInfo(TWinControl));
    AddField('FAlignment', System.TypeInfo(TAlignment));
    AddField('FAutoSize', System.TypeInfo(Boolean));
    AddField('FLayout', System.TypeInfo(TTextLayout));
    AddField('FWordWrap', System.TypeInfo(Boolean));
    AddField('FShowAccelChar', System.TypeInfo(Boolean));
    AddField('FOnMouseLeave', System.TypeInfo(TNotifyEvent));
    AddField('FOnMouseEnter', System.TypeInfo(TNotifyEvent));
    AddField('FTransparentSet', System.TypeInfo(Boolean));
    AddField('FEllipsisPosition', System.TypeInfo(TEllipsisPosition));

    AddMethod('GetTransparent', @TSepiImportsTCustomLabel.GetTransparent,
      'function: Boolean');
    AddMethod('SetAlignment', @TSepiImportsTCustomLabel.SetAlignment,
      'procedure(Value: TAlignment)');
    AddMethod('SetEllipsisPosition', @TSepiImportsTCustomLabel.SetEllipsisPosition,
      'procedure(Value: TEllipsisPosition)');
    AddMethod('SetFocusControl', @TSepiImportsTCustomLabel.SetFocusControl,
      'procedure(Value: TWinControl)');
    AddMethod('SetShowAccelChar', @TSepiImportsTCustomLabel.SetShowAccelChar,
      'procedure(Value: Boolean)');
    AddMethod('SetTransparent', @TSepiImportsTCustomLabel.SetTransparent,
      'procedure(Value: Boolean)');
    AddMethod('SetLayout', @TSepiImportsTCustomLabel.SetLayout,
      'procedure(Value: TTextLayout)');
    AddMethod('SetWordWrap', @TSepiImportsTCustomLabel.SetWordWrap,
      'procedure(Value: Boolean)');
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CMDialogChar', nil,
      'procedure(var Message: TCMDialogChar)',
      mlkMessage, False, CM_DIALOGCHAR);
    AddMethod('CMMouseEnter', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_MOUSEENTER);
    AddMethod('CMMouseLeave', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_MOUSELEAVE);

    CurrentVisibility := mvProtected;

    AddMethod('AdjustBounds', @TSepiImportsTCustomLabel.AdjustBounds,
      'procedure',
      mlkDynamic);
    AddMethod('DoDrawText', @TSepiImportsTCustomLabel.DoDrawText,
      'procedure(var Rect: TRect; Flags: Longint)',
      mlkDynamic);
    AddMethod('GetLabelText', @TSepiImportsTCustomLabel.GetLabelText,
      'function: string',
      mlkVirtual);
    AddMethod('Loaded', @TSepiImportsTCustomLabel.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTCustomLabel.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation )',
      mlkOverride);
    AddMethod('Paint', @TSepiImportsTCustomLabel.Paint,
      'procedure',
      mlkOverride);
    AddMethod('SetAutoSize', @TSepiImportsTCustomLabel.SetAutoSize,
      'procedure(Value: Boolean)',
      mlkOverride);

    AddProperty('Alignment', 'property: TAlignment',
      'FAlignment', 'SetAlignment',
      NoIndex, Integer(taLeftJustify));
    AddProperty('AutoSize', 'property: Boolean',
      'FAutoSize', 'SetAutoSize',
      NoIndex, Integer(True));
    AddProperty('EllipsisPosition', 'property: TEllipsisPosition',
      'FEllipsisPosition', 'SetEllipsisPosition',
      NoIndex, Integer(epNone));
    AddProperty('FocusControl', 'property: TWinControl',
      'FFocusControl', 'SetFocusControl');
    AddProperty('ShowAccelChar', 'property: Boolean',
      'FShowAccelChar', 'SetShowAccelChar',
      NoIndex, Integer(True));
    AddProperty('Transparent', 'property: Boolean',
      'GetTransparent', 'SetTransparent',
      NoIndex, NoDefaultValue, 'FTransparentSet');
    AddProperty('Layout', 'property: TTextLayout',
      'FLayout', 'SetLayout',
      NoIndex, Integer(tlTop));
    AddProperty('WordWrap', 'property: Boolean',
      'FWordWrap', 'SetWordWrap',
      NoIndex, Integer(False));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomLabel.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    RedefineProperty('Caption');
    RedefineProperty('Canvas');
    AddProperty('OnMouseEnter', 'property: TNotifyEvent',
      'FOnMouseEnter', 'FOnMouseEnter');
    AddProperty('OnMouseLeave', 'property: TNotifyEvent',
      'FOnMouseLeave', 'FOnMouseLeave');

    Complete;
  end;
end;

{---------------}
{ TLabel import }
{---------------}

class function TSepiImportsTLabel.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TLabel));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Alignment');
    RedefineProperty('Anchors');
    RedefineProperty('AutoSize');
    RedefineProperty('BiDiMode');
    RedefineProperty('Caption');
    RedefineProperty('Color',
      '', '', NoDefaultValue);
    RedefineProperty('Constraints');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('EllipsisPosition');
    RedefineProperty('Enabled');
    RedefineProperty('FocusControl');
    RedefineProperty('Font');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowAccelChar');
    RedefineProperty('ShowHint');
    RedefineProperty('Transparent');
    RedefineProperty('Layout');
    RedefineProperty('Visible');
    RedefineProperty('WordWrap');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnMouseEnter');
    RedefineProperty('OnMouseLeave');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{--------------------}
{ TCustomEdit import }
{--------------------}

function TSepiImportsTCustomEdit.GetModified: Boolean;
begin
  Result := Modified;
end;

function TSepiImportsTCustomEdit.GetCanUndo: Boolean;
begin
  Result := CanUndo;
end;

procedure TSepiImportsTCustomEdit.SetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TSepiImportsTCustomEdit.SetCharCase(Value: TEditCharCase);
begin
  CharCase := Value;
end;

procedure TSepiImportsTCustomEdit.SetHideSelection(Value: Boolean);
begin
  HideSelection := Value;
end;

procedure TSepiImportsTCustomEdit.SetMaxLength(Value: Integer);
begin
  MaxLength := Value;
end;

procedure TSepiImportsTCustomEdit.SetModified(Value: Boolean);
begin
  Modified := Value;
end;

procedure TSepiImportsTCustomEdit.SetOEMConvert(Value: Boolean);
begin
  OEMConvert := Value;
end;

procedure TSepiImportsTCustomEdit.SetPasswordChar(Value: Char);
begin
  PasswordChar := Value;
end;

procedure TSepiImportsTCustomEdit.SetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TSepiImportsTCustomEdit.SetSelText(const Value: string);
begin
  SelText := Value;
end;

class function TSepiImportsTCustomEdit.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomEdit));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMaxLength', System.TypeInfo(Integer));
    AddField('FBorderStyle', System.TypeInfo(TBorderStyle));
    AddField('FPasswordChar', System.TypeInfo(Char));
    AddField('FReadOnly', System.TypeInfo(Boolean));
    AddField('FAutoSize', System.TypeInfo(Boolean));
    AddField('FAutoSelect', System.TypeInfo(Boolean));
    AddField('FHideSelection', System.TypeInfo(Boolean));
    AddField('FOEMConvert', System.TypeInfo(Boolean));
    AddField('FCharCase', System.TypeInfo(TEditCharCase));
    AddField('FCreating', System.TypeInfo(Boolean));
    AddField('FModified', System.TypeInfo(Boolean));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));

    AddMethod('AdjustHeight', nil,
      'procedure');
    AddMethod('GetModified', @TSepiImportsTCustomEdit.GetModified,
      'function: Boolean');
    AddMethod('GetCanUndo', @TSepiImportsTCustomEdit.GetCanUndo,
      'function: Boolean');
    AddMethod('SetBorderStyle', @TSepiImportsTCustomEdit.SetBorderStyle,
      'procedure(Value: TBorderStyle)');
    AddMethod('SetCharCase', @TSepiImportsTCustomEdit.SetCharCase,
      'procedure(Value: TEditCharCase)');
    AddMethod('SetHideSelection', @TSepiImportsTCustomEdit.SetHideSelection,
      'procedure(Value: Boolean)');
    AddMethod('SetMaxLength', @TSepiImportsTCustomEdit.SetMaxLength,
      'procedure(Value: Integer)');
    AddMethod('SetModified', @TSepiImportsTCustomEdit.SetModified,
      'procedure(Value: Boolean)');
    AddMethod('SetOEMConvert', @TSepiImportsTCustomEdit.SetOEMConvert,
      'procedure(Value: Boolean)');
    AddMethod('SetPasswordChar', @TSepiImportsTCustomEdit.SetPasswordChar,
      'procedure(Value: Char)');
    AddMethod('SetReadOnly', @TSepiImportsTCustomEdit.SetReadOnly,
      'procedure(Value: Boolean)');
    AddMethod('SetSelText', @TSepiImportsTCustomEdit.SetSelText,
      'procedure(const Value: string)');
    AddMethod('UpdateHeight', nil,
      'procedure');
    AddMethod('WMSetFont', nil,
      'procedure(var Message: TWMSetFont)',
      mlkMessage, False, WM_SETFONT);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('CMEnter', nil,
      'procedure(var Message: TCMGotFocus)',
      mlkMessage, False, CM_ENTER);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CNCommand', nil,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, CN_COMMAND);
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);
    AddMethod('WMContextMenu', nil,
      'procedure(var Message: TWMContextMenu)',
      mlkMessage, False, WM_CONTEXTMENU);

    CurrentVisibility := mvProtected;

    AddMethod('Change', @TSepiImportsTCustomEdit.Change,
      'procedure',
      mlkDynamic);
    AddMethod('CreateParams', @TSepiImportsTCustomEdit.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWindowHandle', @TSepiImportsTCustomEdit.CreateWindowHandle,
      'procedure(const Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomEdit.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWnd', @TSepiImportsTCustomEdit.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('DoSetMaxLength', @TSepiImportsTCustomEdit.DoSetMaxLength,
      'procedure(Value: Integer)',
      mlkVirtual);
    AddMethod('GetSelLength', @TSepiImportsTCustomEdit.GetSelLength,
      'function: Integer',
      mlkVirtual);
    AddMethod('GetSelStart', @TSepiImportsTCustomEdit.GetSelStart,
      'function: Integer',
      mlkVirtual);
    AddMethod('GetSelText', @TSepiImportsTCustomEdit.GetSelText,
      'function: string',
      mlkVirtual);
    AddMethod('SetAutoSize', @TSepiImportsTCustomEdit.SetAutoSize,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetSelLength', @TSepiImportsTCustomEdit.SetSelLength,
      'procedure(Value: Integer)',
      mlkVirtual);
    AddMethod('SetSelStart', @TSepiImportsTCustomEdit.SetSelStart,
      'procedure(Value: Integer)',
      mlkVirtual);

    AddProperty('AutoSelect', 'property: Boolean',
      'FAutoSelect', 'FAutoSelect',
      NoIndex, Integer(True));
    AddProperty('AutoSize', 'property: Boolean',
      'FAutoSize', 'SetAutoSize',
      NoIndex, Integer(True));
    AddProperty('BorderStyle', 'property: TBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(bsSingle));
    AddProperty('CharCase', 'property: TEditCharCase',
      'FCharCase', 'SetCharCase',
      NoIndex, Integer(ecNormal));
    AddProperty('HideSelection', 'property: Boolean',
      'FHideSelection', 'SetHideSelection',
      NoIndex, Integer(True));
    AddProperty('MaxLength', 'property: Integer',
      'FMaxLength', 'SetMaxLength',
      NoIndex, 0);
    AddProperty('OEMConvert', 'property: Boolean',
      'FOEMConvert', 'SetOEMConvert',
      NoIndex, Integer(False));
    AddProperty('PasswordChar', 'property: Char',
      'FPasswordChar', 'SetPasswordChar',
      NoIndex, Integer(#0));
    RedefineProperty('ParentColor',
      '', '', Integer(False));
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomEdit.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTCustomEdit.Clear,
      'procedure',
      mlkVirtual);
    AddMethod('ClearSelection', @TSepiImportsTCustomEdit.ClearSelection,
      'procedure');
    AddMethod('CopyToClipboard', @TSepiImportsTCustomEdit.CopyToClipboard,
      'procedure');
    AddMethod('CutToClipboard', @TSepiImportsTCustomEdit.CutToClipboard,
      'procedure');
    AddMethod('DefaultHandler', @TSepiImportsTCustomEdit.DefaultHandler,
      'procedure(var Message)',
      mlkOverride);
    AddMethod('PasteFromClipboard', @TSepiImportsTCustomEdit.PasteFromClipboard,
      'procedure');
    AddMethod('Undo', @TSepiImportsTCustomEdit.Undo,
      'procedure');
    AddMethod('ClearUndo', @TSepiImportsTCustomEdit.ClearUndo,
      'procedure');
    AddMethod('GetSelTextBuf', @TSepiImportsTCustomEdit.GetSelTextBuf,
      'function(Buffer: PChar; BufSize: Integer): Integer',
      mlkVirtual);
    AddMethod('SelectAll', @TSepiImportsTCustomEdit.SelectAll,
      'procedure');
    AddMethod('SetSelTextBuf', @TSepiImportsTCustomEdit.SetSelTextBuf,
      'procedure(Buffer: PChar)');

    AddProperty('CanUndo', 'property: Boolean',
      'GetCanUndo', '');
    AddProperty('Modified', 'property: Boolean',
      'GetModified', 'SetModified');
    AddProperty('ReadOnly', 'property: Boolean',
      'FReadOnly', 'SetReadOnly',
      NoIndex, Integer(False));
    AddProperty('SelLength', 'property: Integer',
      'GetSelLength', 'SetSelLength');
    AddProperty('SelStart', 'property: Integer',
      'GetSelStart', 'SetSelStart');
    AddProperty('SelText', 'property: string',
      'GetSelText', 'SetSelText');
    RedefineProperty('Text');

    CurrentVisibility := mvPublished;

    RedefineProperty('TabStop',
      '', '', Integer(True));

    Complete;
  end;
end;

{--------------}
{ TEdit import }
{--------------}

class function TSepiImportsTEdit.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TEdit));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Anchors');
    RedefineProperty('AutoSelect');
    RedefineProperty('AutoSize');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelKind',
      '', '', Integer(bkNone));
    RedefineProperty('BevelOuter');
    RedefineProperty('BevelWidth');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderStyle');
    RedefineProperty('CharCase');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('HideSelection');
    RedefineProperty('ImeMode');
    RedefineProperty('ImeName');
    RedefineProperty('MaxLength');
    RedefineProperty('OEMConvert');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PasswordChar');
    RedefineProperty('PopupMenu');
    RedefineProperty('ReadOnly');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Text');
    RedefineProperty('Visible');
    RedefineProperty('OnChange');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{--------------------}
{ TCustomMemo import }
{--------------------}

class function TSepiImportsTCustomMemo.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomMemo));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FLines', System.TypeInfo(TStrings));
    AddField('FAlignment', System.TypeInfo(TAlignment));
    AddField('FScrollBars', System.TypeInfo(TScrollStyle));
    AddField('FWordWrap', System.TypeInfo(Boolean));
    AddField('FWantReturns', System.TypeInfo(Boolean));
    AddField('FWantTabs', System.TypeInfo(Boolean));

    AddMethod('WMGetDlgCode', nil,
      'procedure(var Message: TWMGetDlgCode)',
      mlkMessage, False, WM_GETDLGCODE);
    AddMethod('WMNCDestroy', nil,
      'procedure(var Message: TWMNCDestroy)',
      mlkMessage, False, WM_NCDESTROY);

    CurrentVisibility := mvProtected;

    AddMethod('GetCaretPos', @TSepiImportsTCustomMemo.GetCaretPos,
      'function: TPoint',
      mlkVirtual);
    AddMethod('SetCaretPos', @TSepiImportsTCustomMemo.SetCaretPos,
      'procedure(const Value: TPoint)',
      mlkVirtual);
    AddMethod('CreateParams', @TSepiImportsTCustomMemo.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWindowHandle', @TSepiImportsTCustomMemo.CreateWindowHandle,
      'procedure(const Params: TCreateParams)',
      mlkOverride);
    AddMethod('KeyPress', @TSepiImportsTCustomMemo.KeyPress,
      'procedure(var Key: Char)',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTCustomMemo.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('SetAlignment', @TSepiImportsTCustomMemo.SetAlignment,
      'procedure(Value: TAlignment)');
    AddMethod('SetLines', @TSepiImportsTCustomMemo.SetLines,
      'procedure(Value: TStrings)');
    AddMethod('SetScrollBars', @TSepiImportsTCustomMemo.SetScrollBars,
      'procedure(Value: TScrollStyle)');
    AddMethod('SetWordWrap', @TSepiImportsTCustomMemo.SetWordWrap,
      'procedure(Value: Boolean)');

    AddProperty('Alignment', 'property: TAlignment',
      'FAlignment', 'SetAlignment',
      NoIndex, Integer(taLeftJustify));
    AddProperty('ScrollBars', 'property: TScrollStyle',
      'FScrollBars', 'SetScrollBars',
      NoIndex, Integer(ssNone));
    AddProperty('WantReturns', 'property: Boolean',
      'FWantReturns', 'FWantReturns',
      NoIndex, Integer(True));
    AddProperty('WantTabs', 'property: Boolean',
      'FWantTabs', 'FWantTabs',
      NoIndex, Integer(False));
    AddProperty('WordWrap', 'property: Boolean',
      'FWordWrap', 'SetWordWrap',
      NoIndex, Integer(True));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomMemo.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomMemo.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('GetControlsAlignment', @TSepiImportsTCustomMemo.GetControlsAlignment,
      'function: TAlignment',
      mlkOverride);

    AddProperty('CaretPos', 'property: TPoint',
      'GetCaretPos', 'SetCaretPos');
    AddProperty('Lines', 'property: TStrings',
      'FLines', 'SetLines');

    Complete;
  end;
end;

{--------------}
{ TMemo import }
{--------------}

class function TSepiImportsTMemo.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMemo));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Alignment');
    RedefineProperty('Anchors');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelKind',
      '', '', Integer(bkNone));
    RedefineProperty('BevelOuter');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderStyle');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('HideSelection');
    RedefineProperty('ImeMode');
    RedefineProperty('ImeName');
    RedefineProperty('Lines');
    RedefineProperty('MaxLength');
    RedefineProperty('OEMConvert');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ReadOnly');
    RedefineProperty('ScrollBars');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('WantReturns');
    RedefineProperty('WantTabs');
    RedefineProperty('WordWrap');
    RedefineProperty('OnChange');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{-------------------------------}
{ TCustomComboBoxStrings import }
{-------------------------------}

class function TSepiImportsTCustomComboBoxStrings.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomComboBoxStrings));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FComboBox', System.TypeInfo(TCustomCombo));

    CurrentVisibility := mvProtected;

    AddMethod('GetCount', @TSepiImportsTCustomComboBoxStrings.GetCount,
      'function: Integer',
      mlkOverride);
    AddMethod('Get', @TSepiImportsTCustomComboBoxStrings.Get,
      'function(Index: Integer): string',
      mlkOverride);
    AddMethod('GetObject', @TSepiImportsTCustomComboBoxStrings.GetObject,
      'function(Index: Integer): TObject',
      mlkOverride);
    AddMethod('PutObject', @TSepiImportsTCustomComboBoxStrings.PutObject,
      'procedure(Index: Integer; AObject: TObject)',
      mlkOverride);
    AddMethod('SetUpdateState', @TSepiImportsTCustomComboBoxStrings.SetUpdateState,
      'procedure(Updating: Boolean)',
      mlkOverride);

    AddProperty('ComboBox', 'property: TCustomCombo',
      'FComboBox', 'FComboBox');

    CurrentVisibility := mvPublic;

    AddMethod('Clear', @TSepiImportsTCustomComboBoxStrings.Clear,
      'procedure',
      mlkOverride);
    AddMethod('Delete', @TSepiImportsTCustomComboBoxStrings.Delete,
      'procedure(Index: Integer)',
      mlkOverride);
    AddMethod('IndexOf', @TSepiImportsTCustomComboBoxStrings.IndexOf,
      'function(const S: string): Integer',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ TCustomCombo import }
{---------------------}

class function TSepiImportsTCustomCombo.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomCombo'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomCombo));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FMaxLength', System.TypeInfo(Integer));
    AddField('FDropDownCount', System.TypeInfo(Integer));
    AddField('FItemIndex', System.TypeInfo(Integer));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnSelect', System.TypeInfo(TNotifyEvent));
    AddField('FOnDropDown', System.TypeInfo(TNotifyEvent));
    AddField('FOnCloseUp', System.TypeInfo(TNotifyEvent));
    AddField('FItemHeight', System.TypeInfo(Integer));
    AddField('FItems', System.TypeInfo(TStrings));

    AddMethod('WMCreate', nil,
      'procedure(var Message: TWMCreate)',
      mlkMessage, False, WM_CREATE);
    AddMethod('CMCancelMode', nil,
      'procedure(var Message: TCMCancelMode)',
      mlkMessage, False, CM_CANCELMODE);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('CNCommand', nil,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, CN_COMMAND);
    AddMethod('WMDrawItem', nil,
      'procedure(var Message: TWMDrawItem)',
      mlkMessage, False, WM_DRAWITEM);
    AddMethod('WMMeasureItem', nil,
      'procedure(var Message: TWMMeasureItem)',
      mlkMessage, False, WM_MEASUREITEM);
    AddMethod('WMDeleteItem', nil,
      'procedure(var Message: TWMDeleteItem)',
      mlkMessage, False, WM_DELETEITEM);
    AddMethod('WMGetDlgCode', nil,
      'procedure(var Message: TWMGetDlgCode)',
      mlkMessage, False, WM_GETDLGCODE);

    CurrentVisibility := mvProtected;

    AddField('FEditHandle', System.TypeInfo(HWnd));
    AddField('FListHandle', System.TypeInfo(HWnd));
    AddField('FDropHandle', System.TypeInfo(HWnd));
    AddField('FEditInstance', 'Pointer');
    AddField('FDefEditProc', 'Pointer');
    AddField('FListInstance', 'Pointer');
    AddField('FDefListProc', 'Pointer');
    AddField('FDroppingDown', System.TypeInfo(Boolean));
    AddField('FFocusChanged', System.TypeInfo(Boolean));
    AddField('FIsFocused', System.TypeInfo(Boolean));
    AddField('FSaveIndex', System.TypeInfo(Integer));

    AddMethod('AdjustDropDown', @TSepiImportsTCustomCombo.AdjustDropDown,
      'procedure',
      mlkVirtual);
    AddMethod('ComboWndProc', @TSepiImportsTCustomCombo.ComboWndProc,
      'procedure(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer )',
      mlkVirtual);
    AddMethod('CreateWnd', @TSepiImportsTCustomCombo.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('EditWndProc', @TSepiImportsTCustomCombo.EditWndProc,
      'procedure(var Message: TMessage)');
    AddMethod('GetItemsClass', nil,
      'function: TCustomComboBoxStringsClass',
      mlkVirtual, True);
    AddMethod('WndProc', @TSepiImportsTCustomCombo.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);
    AddMethod('GetItemHt', nil,
      'function: Integer',
      mlkVirtual, True);
    AddMethod('SetItemHeight', @TSepiImportsTCustomCombo.SetItemHeight,
      'procedure(Value: Integer)',
      mlkVirtual);
    AddMethod('GetCount', @TSepiImportsTCustomCombo.GetCount,
      'function: Integer',
      mlkOverride);
    AddMethod('GetItemCount', nil,
      'function: Integer',
      mlkVirtual, True);
    AddMethod('GetItemIndex', @TSepiImportsTCustomCombo.GetItemIndex,
      'function: Integer',
      mlkOverride);
    AddMethod('GetDroppedDown', @TSepiImportsTCustomCombo.GetDroppedDown,
      'function: Boolean');
    AddMethod('GetSelLength', @TSepiImportsTCustomCombo.GetSelLength,
      'function: Integer');
    AddMethod('GetSelStart', @TSepiImportsTCustomCombo.GetSelStart,
      'function: Integer');
    AddMethod('ListWndProc', @TSepiImportsTCustomCombo.ListWndProc,
      'procedure(var Message: TMessage)');
    AddMethod('Loaded', @TSepiImportsTCustomCombo.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('Change', @TSepiImportsTCustomCombo.Change,
      'procedure',
      mlkDynamic);
    AddMethod('Select', @TSepiImportsTCustomCombo.Select,
      'procedure',
      mlkDynamic);
    AddMethod('DropDown', @TSepiImportsTCustomCombo.DropDown,
      'procedure',
      mlkDynamic);
    AddMethod('CloseUp', @TSepiImportsTCustomCombo.CloseUp,
      'procedure',
      mlkDynamic);
    AddMethod('DestroyWindowHandle', @TSepiImportsTCustomCombo.DestroyWindowHandle,
      'procedure',
      mlkOverride);
    AddMethod('SetDroppedDown', @TSepiImportsTCustomCombo.SetDroppedDown,
      'procedure(Value: Boolean)');
    AddMethod('SetSelLength', @TSepiImportsTCustomCombo.SetSelLength,
      'procedure(Value: Integer)');
    AddMethod('SetSelStart', @TSepiImportsTCustomCombo.SetSelStart,
      'procedure(Value: Integer)');
    AddMethod('SetMaxLength', @TSepiImportsTCustomCombo.SetMaxLength,
      'procedure(Value: Integer)');
    AddMethod('SetDropDownCount', @TSepiImportsTCustomCombo.SetDropDownCount,
      'procedure(const Value: Integer)',
      mlkVirtual);
    AddMethod('SetItemIndex', @TSepiImportsTCustomCombo.SetItemIndex,
      'procedure(const Value: Integer)',
      mlkOverride);
    AddMethod('SetItems', @TSepiImportsTCustomCombo.SetItems,
      'procedure(const Value: TStrings)',
      mlkVirtual);

    AddProperty('DropDownCount', 'property: Integer',
      'FDropDownCount', 'SetDropDownCount',
      NoIndex, 8);
    AddProperty('EditHandle', 'property: HWnd',
      'FEditHandle', '');
    AddProperty('ItemCount', 'property: Integer',
      'GetItemCount', '');
    AddProperty('ItemHeight', 'property: Integer',
      'GetItemHt', 'SetItemHeight');
    AddProperty('ListHandle', 'property: HWnd',
      'FListHandle', '');
    AddProperty('MaxLength', 'property: Integer',
      'FMaxLength', 'SetMaxLength',
      NoIndex, 0);
    RedefineProperty('ParentColor',
      '', '', Integer(False));
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OnDropDown', 'property: TNotifyEvent',
      'FOnDropDown', 'FOnDropDown');
    AddProperty('OnSelect', 'property: TNotifyEvent',
      'FOnSelect', 'FOnSelect');
    AddProperty('OnCloseUp', 'property: TNotifyEvent',
      'FOnCloseUp', 'FOnCloseUp');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomCombo.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomCombo.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AddItem', @TSepiImportsTCustomCombo.AddItem,
      'procedure(Item: String; AObject: TObject)',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTCustomCombo.Clear,
      'procedure',
      mlkOverride);
    AddMethod('ClearSelection', @TSepiImportsTCustomCombo.ClearSelection,
      'procedure',
      mlkOverride);
    AddMethod('CopySelection', @TSepiImportsTCustomCombo.CopySelection,
      'procedure(Destination: TCustomListControl)',
      mlkOverride);
    AddMethod('DeleteSelected', @TSepiImportsTCustomCombo.DeleteSelected,
      'procedure',
      mlkOverride);
    AddMethod('Focused', @TSepiImportsTCustomCombo.Focused,
      'function: Boolean',
      mlkOverride);
    AddMethod('SelectAll', @TSepiImportsTCustomCombo.SelectAll,
      'procedure',
      mlkOverride);

    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');
    AddProperty('DroppedDown', 'property: Boolean',
      'GetDroppedDown', 'SetDroppedDown');
    AddProperty('Items', 'property: TStrings',
      'FItems', 'SetItems');
    AddProperty('SelLength', 'property: Integer',
      'GetSelLength', 'SetSelLength');
    AddProperty('SelStart', 'property: Integer',
      'GetSelStart', 'SetSelStart');
    RedefineProperty('TabStop',
      '', '', Integer(True));

    Complete;
  end;
end;

{------------------------}
{ TCustomComboBox import }
{------------------------}

procedure TSepiImportsTCustomComboBox.SetCharCase(Value: TEditCharCase);
begin
  CharCase := Value;
end;

procedure TSepiImportsTCustomComboBox.SetSelText(const Value: string);
begin
  SelText := Value;
end;

procedure TSepiImportsTCustomComboBox.SetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

class function TSepiImportsTCustomComboBox.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomComboBox));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAutoComplete', System.TypeInfo(Boolean));
    AddField('FAutoDropDown', System.TypeInfo(Boolean));
    AddField('FLastTime', System.TypeInfo(Cardinal));
    AddField('FFilter', System.TypeInfo(String));
    AddField('FCharCase', System.TypeInfo(TEditCharCase));
    AddField('FSorted', System.TypeInfo(Boolean));
    AddField('FStyle', System.TypeInfo(TComboBoxStyle));
    AddField('FSaveItems', System.TypeInfo(TStringList));
    AddField('FOnDrawItem', System.TypeInfo(TDrawItemEvent));
    AddField('FOnMeasureItem', System.TypeInfo(TMeasureItemEvent));
    AddField('FAutoCloseUp', System.TypeInfo(Boolean));
    AddField('FAutoCompleteDelay', System.TypeInfo(Cardinal));

    AddMethod('SetCharCase', @TSepiImportsTCustomComboBox.SetCharCase,
      'procedure(Value: TEditCharCase)');
    AddMethod('SetSelText', @TSepiImportsTCustomComboBox.SetSelText,
      'procedure(const Value: string)');
    AddMethod('SetSorted', @TSepiImportsTCustomComboBox.SetSorted,
      'procedure(Value: Boolean)');
    AddMethod('WMEraseBkgnd', nil,
      'procedure(var Message: TWMEraseBkgnd)',
      mlkMessage, False, WM_ERASEBKGND);
    AddMethod('CMParentColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTCOLORCHANGED);
    AddMethod('CNDrawItem', nil,
      'procedure(var Message: TWMDrawItem)',
      mlkMessage, False, CN_DRAWITEM);
    AddMethod('CNMeasureItem', nil,
      'procedure(var Message: TWMMeasureItem)',
      mlkMessage, False, CN_MEASUREITEM);
    AddMethod('WMLButtonDown', nil,
      'procedure(var Message: TWMLButtonDown)',
      mlkMessage, False, WM_LBUTTONDOWN);
    AddMethod('WMPaint', nil,
      'procedure(var Message: TWMPaint)',
      mlkMessage, False, WM_PAINT);

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTCustomComboBox.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomComboBox.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWnd', @TSepiImportsTCustomComboBox.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('DrawItem', @TSepiImportsTCustomComboBox.DrawItem,
      'procedure(Index: Integer; Rect: TRect; State: TOwnerDrawState )',
      mlkVirtual);
    AddMethod('GetItemHt', @TSepiImportsTCustomComboBox.GetItemHt,
      'function: Integer',
      mlkOverride);
    AddMethod('GetItemsClass', @TSepiImportsTCustomComboBox.GetItemsClass,
      'function: TCustomComboBoxStringsClass',
      mlkOverride);
    AddMethod('GetSelText', @TSepiImportsTCustomComboBox.GetSelText,
      'function: string');
    AddMethod('KeyPress', @TSepiImportsTCustomComboBox.KeyPress,
      'procedure(var Key: Char)',
      mlkOverride);
    AddMethod('MeasureItem', @TSepiImportsTCustomComboBox.MeasureItem,
      'procedure(Index: Integer; var Height: Integer)',
      mlkVirtual);
    AddMethod('SelectItem', @TSepiImportsTCustomComboBox.SelectItem,
      'function(const AnItem: String): Boolean');
    AddMethod('SetStyle', @TSepiImportsTCustomComboBox.SetStyle,
      'procedure(Value: TComboBoxStyle)',
      mlkVirtual);

    AddProperty('Sorted', 'property: Boolean',
      'FSorted', 'SetSorted',
      NoIndex, Integer(False));
    AddProperty('Style', 'property: TComboBoxStyle',
      'FStyle', 'SetStyle',
      NoIndex, Integer(csDropDown));
    AddProperty('OnDrawItem', 'property: TDrawItemEvent',
      'FOnDrawItem', 'FOnDrawItem');
    AddProperty('OnMeasureItem', 'property: TMeasureItemEvent',
      'FOnMeasureItem', 'FOnMeasureItem');

    AddMethod('WndProc', @TSepiImportsTCustomComboBox.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);
    AddMethod('GetItemCount', @TSepiImportsTCustomComboBox.GetItemCount,
      'function: Integer',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomComboBox.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomComboBox.Destroy,
      'destructor',
      mlkOverride);

    AddProperty('AutoCompleteDelay', 'property: Cardinal',
      'FAutoCompleteDelay', 'FAutoCompleteDelay',
      NoIndex, 500);
    AddProperty('AutoComplete', 'property: Boolean',
      'FAutoComplete', 'FAutoComplete',
      NoIndex, Integer(True));
    AddProperty('AutoCloseUp', 'property: Boolean',
      'FAutoCloseUp', 'FAutoCloseUp',
      NoIndex, Integer(False));
    AddProperty('AutoDropDown', 'property: Boolean',
      'FAutoDropDown', 'FAutoDropDown',
      NoIndex, Integer(False));
    AddProperty('CharCase', 'property: TEditCharCase',
      'FCharCase', 'SetCharCase',
      NoIndex, Integer(ecNormal));
    AddProperty('SelText', 'property: string',
      'GetSelText', 'SetSelText');

    Complete;
  end;
end;

{------------------}
{ TComboBox import }
{------------------}

class function TSepiImportsTComboBox.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComboBox));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('AutoComplete',
      '', '', Integer(True));
    RedefineProperty('AutoCompleteDelay',
      '', '', 500);
    RedefineProperty('AutoDropDown',
      '', '', Integer(False));
    RedefineProperty('AutoCloseUp',
      '', '', Integer(False));
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelKind',
      '', '', Integer(bkNone));
    RedefineProperty('BevelOuter');
    RedefineProperty('Style');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('CharCase');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('DropDownCount');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ImeMode');
    RedefineProperty('ImeName');
    RedefineProperty('ItemHeight');
    RedefineProperty('ItemIndex',
      '', '', -1);
    RedefineProperty('MaxLength');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('Sorted');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Text');
    RedefineProperty('Visible');
    RedefineProperty('OnChange');
    RedefineProperty('OnClick');
    RedefineProperty('OnCloseUp');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnDrawItem');
    RedefineProperty('OnDropDown');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMeasureItem');
    RedefineProperty('OnSelect');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('Items');

    Complete;
  end;
end;

{--------------------------}
{ TButtonActionLink import }
{--------------------------}

class function TSepiImportsTButtonActionLink.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TButtonActionLink));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FClient', System.TypeInfo(TButtonControl));

    AddMethod('AssignClient', @TSepiImportsTButtonActionLink.AssignClient,
      'procedure(AClient: TObject)',
      mlkOverride);
    AddMethod('IsCheckedLinked', @TSepiImportsTButtonActionLink.IsCheckedLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('SetChecked', @TSepiImportsTButtonActionLink.SetChecked,
      'procedure(Value: Boolean)',
      mlkOverride);

    Complete;
  end;
end;

{-----------------------}
{ TButtonControl import }
{-----------------------}

procedure TSepiImportsTButtonControl.SetWordWrap(const Value: Boolean);
begin
  WordWrap := Value;
end;

class function TSepiImportsTButtonControl.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TButtonControl'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TButtonControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FClicksDisabled', System.TypeInfo(Boolean));
    AddField('FWordWrap', System.TypeInfo(Boolean));

    AddMethod('IsCheckedStored', nil,
      'function: Boolean');
    AddMethod('CNCtlColorStatic', nil,
      'procedure(var Message: TWMCtlColorStatic)',
      mlkMessage, False, CN_CTLCOLORSTATIC);
    AddMethod('WMEraseBkGnd', nil,
      'procedure(var Message: TWMEraseBkGnd)',
      mlkMessage, False, WM_ERASEBKGND);
    AddMethod('SetWordWrap', @TSepiImportsTButtonControl.SetWordWrap,
      'procedure(const Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('ActionChange', @TSepiImportsTButtonControl.ActionChange,
      'procedure(Sender: TObject; CheckDefaults: Boolean)',
      mlkOverride);
    AddMethod('GetActionLinkClass', @TSepiImportsTButtonControl.GetActionLinkClass,
      'function: TControlActionLinkClass',
      mlkOverride);
    AddMethod('GetChecked', @TSepiImportsTButtonControl.GetChecked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('SetChecked', @TSepiImportsTButtonControl.SetChecked,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('WndProc', @TSepiImportsTButtonControl.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTButtonControl.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);

    AddProperty('Checked', 'property: Boolean',
      'GetChecked', 'SetChecked',
      NoIndex, Integer(False), 'IsCheckedStored');
    AddProperty('ClicksDisabled', 'property: Boolean',
      'FClicksDisabled', 'FClicksDisabled');
    AddProperty('WordWrap', 'property: Boolean',
      'FWordWrap', 'SetWordWrap',
      NoIndex, Integer(False));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTButtonControl.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    Complete;
  end;
end;

{----------------}
{ TButton import }
{----------------}

procedure TSepiImportsTButton.SetDefault(Value: Boolean);
begin
  Default := Value;
end;

class function TSepiImportsTButton.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TButton));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDefault', System.TypeInfo(Boolean));
    AddField('FCancel', System.TypeInfo(Boolean));
    AddField('FActive', System.TypeInfo(Boolean));
    AddField('FModalResult', System.TypeInfo(TModalResult));

    AddMethod('SetDefault', @TSepiImportsTButton.SetDefault,
      'procedure(Value: Boolean)');
    AddMethod('CMDialogKey', nil,
      'procedure(var Message: TCMDialogKey)',
      mlkMessage, False, CM_DIALOGKEY);
    AddMethod('CMDialogChar', nil,
      'procedure(var Message: TCMDialogChar)',
      mlkMessage, False, CM_DIALOGCHAR);
    AddMethod('CMFocusChanged', nil,
      'procedure(var Message: TCMFocusChanged)',
      mlkMessage, False, CM_FOCUSCHANGED);
    AddMethod('CNCommand', nil,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, CN_COMMAND);
    AddMethod('CNCtlColorBtn', nil,
      'procedure(var Message: TWMCtlColorBtn)',
      mlkMessage, False, CN_CTLCOLORBTN);
    AddMethod('WMEraseBkgnd', nil,
      'procedure(var Message: TWMEraseBkgnd)',
      mlkMessage, False, WM_ERASEBKGND);

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTButton.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTButton.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('SetButtonStyle', @TSepiImportsTButton.SetButtonStyle,
      'procedure(ADefault: Boolean)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTButton.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Click', @TSepiImportsTButton.Click,
      'procedure',
      mlkOverride);
    AddMethod('UseRightToLeftAlignment', @TSepiImportsTButton.UseRightToLeftAlignment,
      'function: Boolean',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('Action');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    AddProperty('Cancel', 'property: Boolean',
      'FCancel', 'FCancel',
      NoIndex, Integer(False));
    RedefineProperty('Caption');
    RedefineProperty('Constraints');
    AddProperty('Default', 'property: Boolean',
      'FDefault', 'SetDefault',
      NoIndex, Integer(False));
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    AddProperty('ModalResult', 'property: TModalResult',
      'FModalResult', 'FModalResult',
      NoIndex, 0);
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop',
      '', '', Integer(True));
    RedefineProperty('Visible');
    RedefineProperty('WordWrap');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{------------------------}
{ TCustomCheckBox import }
{------------------------}

procedure TSepiImportsTCustomCheckBox.SetAlignment(Value: TLeftRight);
begin
  Alignment := Value;
end;

procedure TSepiImportsTCustomCheckBox.SetState(Value: TCheckBoxState);
begin
  State := Value;
end;

class function TSepiImportsTCustomCheckBox.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomCheckBox));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAlignment', System.TypeInfo(TLeftRight));
    AddField('FAllowGrayed', System.TypeInfo(Boolean));
    AddField('FState', System.TypeInfo(TCheckBoxState));

    AddMethod('SetAlignment', @TSepiImportsTCustomCheckBox.SetAlignment,
      'procedure(Value: TLeftRight)');
    AddMethod('SetState', @TSepiImportsTCustomCheckBox.SetState,
      'procedure(Value: TCheckBoxState)');
    AddMethod('WMSize', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_SIZE);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('CMDialogChar', nil,
      'procedure(var Message: TCMDialogChar)',
      mlkMessage, False, CM_DIALOGCHAR);
    AddMethod('CNCommand', nil,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, CN_COMMAND);

    CurrentVisibility := mvProtected;

    AddMethod('Toggle', @TSepiImportsTCustomCheckBox.Toggle,
      'procedure',
      mlkVirtual);
    AddMethod('Click', @TSepiImportsTCustomCheckBox.Click,
      'procedure',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTCustomCheckBox.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomCheckBox.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('GetChecked', @TSepiImportsTCustomCheckBox.GetChecked,
      'function: Boolean',
      mlkOverride);
    AddMethod('SetChecked', @TSepiImportsTCustomCheckBox.SetChecked,
      'procedure(Value: Boolean)',
      mlkOverride);

    AddProperty('Alignment', 'property: TLeftRight',
      'FAlignment', 'SetAlignment',
      NoIndex, Integer(taRightJustify));
    AddProperty('AllowGrayed', 'property: Boolean',
      'FAllowGrayed', 'FAllowGrayed',
      NoIndex, Integer(False));
    AddProperty('State', 'property: TCheckBoxState',
      'FState', 'SetState',
      NoIndex, Integer(cbUnchecked));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomCheckBox.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('GetControlsAlignment', @TSepiImportsTCustomCheckBox.GetControlsAlignment,
      'function: TAlignment',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('TabStop',
      '', '', Integer(True));

    Complete;
  end;
end;

{------------------}
{ TCheckBox import }
{------------------}

class function TSepiImportsTCheckBox.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCheckBox));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Action');
    RedefineProperty('Alignment');
    RedefineProperty('AllowGrayed');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('Caption');
    RedefineProperty('Checked');
    RedefineProperty('Color',
      '', '', NoDefaultValue);
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('State');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('WordWrap');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{---------------------}
{ TRadioButton import }
{---------------------}

procedure TSepiImportsTRadioButton.SetAlignment(Value: TLeftRight);
begin
  Alignment := Value;
end;

class function TSepiImportsTRadioButton.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TRadioButton));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAlignment', System.TypeInfo(TLeftRight));
    AddField('FChecked', System.TypeInfo(Boolean));

    AddMethod('SetAlignment', @TSepiImportsTRadioButton.SetAlignment,
      'procedure(Value: TLeftRight)');
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('CMDialogChar', nil,
      'procedure(var Message: TCMDialogChar)',
      mlkMessage, False, CM_DIALOGCHAR);
    AddMethod('CNCommand', nil,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, CN_COMMAND);

    CurrentVisibility := mvProtected;

    AddMethod('GetChecked', @TSepiImportsTRadioButton.GetChecked,
      'function: Boolean',
      mlkOverride);
    AddMethod('SetChecked', @TSepiImportsTRadioButton.SetChecked,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTRadioButton.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTRadioButton.CreateWnd,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTRadioButton.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('GetControlsAlignment', @TSepiImportsTRadioButton.GetControlsAlignment,
      'function: TAlignment',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('Action');
    AddProperty('Alignment', 'property: TLeftRight',
      'FAlignment', 'SetAlignment',
      NoIndex, Integer(taRightJustify));
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('Caption');
    RedefineProperty('Checked');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('WordWrap');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{-----------------------}
{ TCustomListBox import }
{-----------------------}

function TSepiImportsTCustomListBox.GetItemHeight: Integer;
begin
  Result := ItemHeight;
end;

function TSepiImportsTCustomListBox.GetTopIndex: Integer;
begin
  Result := TopIndex;
end;

procedure TSepiImportsTCustomListBox.SetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TSepiImportsTCustomListBox.SetColumns(Value: Integer);
begin
  Columns := Value;
end;

procedure TSepiImportsTCustomListBox.SetCount(const Value: Integer);
begin
  Count := Value;
end;

procedure TSepiImportsTCustomListBox.SetExtendedSelect(Value: Boolean);
begin
  ExtendedSelect := Value;
end;

procedure TSepiImportsTCustomListBox.SetIntegralHeight(Value: Boolean);
begin
  IntegralHeight := Value;
end;

procedure TSepiImportsTCustomListBox.SetItemHeight(Value: Integer);
begin
  ItemHeight := Value;
end;

procedure TSepiImportsTCustomListBox.SetItems(Value: TStrings);
begin
  Items := Value;
end;

procedure TSepiImportsTCustomListBox.SetSelected(Index: Integer; Value: Boolean);
begin
  Selected[Index] := Value;
end;

procedure TSepiImportsTCustomListBox.SetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TSepiImportsTCustomListBox.SetStyle(Value: TListBoxStyle);
begin
  Style := Value;
end;

procedure TSepiImportsTCustomListBox.SetTabWidth(Value: Integer);
begin
  TabWidth := Value;
end;

procedure TSepiImportsTCustomListBox.SetTopIndex(Value: Integer);
begin
  TopIndex := Value;
end;

function TSepiImportsTCustomListBox.GetScrollWidth: Integer;
begin
  Result := ScrollWidth;
end;

procedure TSepiImportsTCustomListBox.SetScrollWidth(const Value: Integer);
begin
  ScrollWidth := Value;
end;

class function TSepiImportsTCustomListBox.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomListBox));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAutoComplete', System.TypeInfo(Boolean));
    AddField('FCount', System.TypeInfo(Integer));
    AddField('FItems', System.TypeInfo(TStrings));
    AddField('FFilter', System.TypeInfo(String));
    AddField('FLastTime', System.TypeInfo(Cardinal));
    AddField('FBorderStyle', System.TypeInfo(TBorderStyle));
    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FColumns', System.TypeInfo(Integer));
    AddField('FItemHeight', System.TypeInfo(Integer));
    AddField('FOldCount', System.TypeInfo(Integer));
    AddField('FStyle', System.TypeInfo(TListBoxStyle));
    AddField('FIntegralHeight', System.TypeInfo(Boolean));
    AddField('FSorted', System.TypeInfo(Boolean));
    AddField('FExtendedSelect', System.TypeInfo(Boolean));
    AddField('FTabWidth', System.TypeInfo(Integer));
    AddField('FSaveItems', System.TypeInfo(TStringList));
    AddField('FSaveTopIndex', System.TypeInfo(Integer));
    AddField('FSaveItemIndex', System.TypeInfo(Integer));
    AddField('FSaveScrollWidth', System.TypeInfo(Integer));
    AddField('FOnDrawItem', System.TypeInfo(TDrawItemEvent));
    AddField('FOnMeasureItem', System.TypeInfo(TMeasureItemEvent));
    AddField('FOnData', System.TypeInfo(TLBGetDataEvent));
    AddField('FOnDataFind', System.TypeInfo(TLBFindDataEvent));
    AddField('FOnDataObject', System.TypeInfo(TLBGetDataObjectEvent));
    AddField('FAutoCompleteDelay', System.TypeInfo(Cardinal));

    AddMethod('GetItemHeight', @TSepiImportsTCustomListBox.GetItemHeight,
      'function: Integer');
    AddMethod('GetTopIndex', @TSepiImportsTCustomListBox.GetTopIndex,
      'function: Integer');
    AddMethod('LBGetText', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, LB_GETTEXT);
    AddMethod('LBGetTextLen', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, LB_GETTEXTLEN);
    AddMethod('SetBorderStyle', @TSepiImportsTCustomListBox.SetBorderStyle,
      'procedure(Value: TBorderStyle)');
    AddMethod('SetColumnWidth', nil,
      'procedure');
    AddMethod('SetColumns', @TSepiImportsTCustomListBox.SetColumns,
      'procedure(Value: Integer)');
    AddMethod('SetCount', @TSepiImportsTCustomListBox.SetCount,
      'procedure(const Value: Integer)');
    AddMethod('SetExtendedSelect', @TSepiImportsTCustomListBox.SetExtendedSelect,
      'procedure(Value: Boolean)');
    AddMethod('SetIntegralHeight', @TSepiImportsTCustomListBox.SetIntegralHeight,
      'procedure(Value: Boolean)');
    AddMethod('SetItemHeight', @TSepiImportsTCustomListBox.SetItemHeight,
      'procedure(Value: Integer)');
    AddMethod('SetItems', @TSepiImportsTCustomListBox.SetItems,
      'procedure(Value: TStrings)');
    AddMethod('SetSelected', @TSepiImportsTCustomListBox.SetSelected,
      'procedure(Index: Integer; Value: Boolean)');
    AddMethod('SetSorted', @TSepiImportsTCustomListBox.SetSorted,
      'procedure(Value: Boolean)');
    AddMethod('SetStyle', @TSepiImportsTCustomListBox.SetStyle,
      'procedure(Value: TListBoxStyle)');
    AddMethod('SetTabWidth', @TSepiImportsTCustomListBox.SetTabWidth,
      'procedure(Value: Integer)');
    AddMethod('SetTopIndex', @TSepiImportsTCustomListBox.SetTopIndex,
      'procedure(Value: Integer)');
    AddMethod('WMPaint', nil,
      'procedure(var Message: TWMPaint)',
      mlkMessage, False, WM_PAINT);
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('CNCommand', nil,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, CN_COMMAND);
    AddMethod('CNDrawItem', nil,
      'procedure(var Message: TWMDrawItem)',
      mlkMessage, False, CN_DRAWITEM);
    AddMethod('CNMeasureItem', nil,
      'procedure(var Message: TWMMeasureItem)',
      mlkMessage, False, CN_MEASUREITEM);
    AddMethod('WMLButtonDown', nil,
      'procedure(var Message: TWMLButtonDown)',
      mlkMessage, False, WM_LBUTTONDOWN);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('GetScrollWidth', @TSepiImportsTCustomListBox.GetScrollWidth,
      'function: Integer');
    AddMethod('SetScrollWidth', @TSepiImportsTCustomListBox.SetScrollWidth,
      'procedure(const Value: Integer)');

    CurrentVisibility := mvProtected;

    AddField('FMoving', System.TypeInfo(Boolean));

    AddMethod('CreateParams', @TSepiImportsTCustomListBox.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomListBox.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWnd', @TSepiImportsTCustomListBox.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('DoGetData', @TSepiImportsTCustomListBox.DoGetData,
      'function(const Index: Integer): String');
    AddMethod('DoGetDataObject', @TSepiImportsTCustomListBox.DoGetDataObject,
      'function(const Index: Integer): TObject');
    AddMethod('DoFindData', @TSepiImportsTCustomListBox.DoFindData,
      'function(const Data: String): Integer');
    AddMethod('WndProc', @TSepiImportsTCustomListBox.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);
    AddMethod('DragCanceled', @TSepiImportsTCustomListBox.DragCanceled,
      'procedure',
      mlkOverride);
    AddMethod('DrawItem', @TSepiImportsTCustomListBox.DrawItem,
      'procedure(Index: Integer; Rect: TRect; State: TOwnerDrawState )',
      mlkVirtual);
    AddMethod('GetCount', @TSepiImportsTCustomListBox.GetCount,
      'function: Integer',
      mlkOverride);
    AddMethod('GetSelCount', @TSepiImportsTCustomListBox.GetSelCount,
      'function: Integer',
      mlkOverride);
    AddMethod('MeasureItem', @TSepiImportsTCustomListBox.MeasureItem,
      'procedure(Index: Integer; var Height: Integer)',
      mlkVirtual);
    AddMethod('InternalGetItemData', @TSepiImportsTCustomListBox.InternalGetItemData,
      'function(Index: Integer): Longint',
      mlkDynamic);
    AddMethod('InternalSetItemData', @TSepiImportsTCustomListBox.InternalSetItemData,
      'procedure(Index: Integer; AData: Longint)',
      mlkDynamic);
    AddMethod('GetItemData', @TSepiImportsTCustomListBox.GetItemData,
      'function(Index: Integer): LongInt',
      mlkDynamic);
    AddMethod('GetItemIndex', @TSepiImportsTCustomListBox.GetItemIndex,
      'function: Integer',
      mlkOverride);
    AddMethod('GetSelected', @TSepiImportsTCustomListBox.GetSelected,
      'function(Index: Integer): Boolean');
    AddMethod('KeyPress', @TSepiImportsTCustomListBox.KeyPress,
      'procedure(var Key: Char)',
      mlkOverride);
    AddMethod('LoadRecreateItems', @TSepiImportsTCustomListBox.LoadRecreateItems,
      'procedure(RecreateItems: TStrings)',
      mlkVirtual);
    AddMethod('SetItemData', @TSepiImportsTCustomListBox.SetItemData,
      'procedure(Index: Integer; AData: LongInt)',
      mlkDynamic);
    AddMethod('ResetContent', @TSepiImportsTCustomListBox.ResetContent,
      'procedure',
      mlkDynamic);
    AddMethod('SaveRecreateItems', @TSepiImportsTCustomListBox.SaveRecreateItems,
      'procedure(RecreateItems: TStrings)',
      mlkVirtual);
    AddMethod('DeleteString', @TSepiImportsTCustomListBox.DeleteString,
      'procedure(Index: Integer)',
      mlkDynamic);
    AddMethod('SetMultiSelect', @TSepiImportsTCustomListBox.SetMultiSelect,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetItemIndex', @TSepiImportsTCustomListBox.SetItemIndex,
      'procedure(const Value: Integer)',
      mlkOverride);

    AddProperty('BorderStyle', 'property: TBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(bsSingle));
    AddProperty('Columns', 'property: Integer',
      'FColumns', 'SetColumns',
      NoIndex, 0);
    AddProperty('ExtendedSelect', 'property: Boolean',
      'FExtendedSelect', 'SetExtendedSelect',
      NoIndex, Integer(True));
    AddProperty('IntegralHeight', 'property: Boolean',
      'FIntegralHeight', 'SetIntegralHeight',
      NoIndex, Integer(False));
    AddProperty('ItemHeight', 'property: Integer',
      'GetItemHeight', 'SetItemHeight');
    RedefineProperty('ParentColor',
      '', '', Integer(False));
    AddProperty('Sorted', 'property: Boolean',
      'FSorted', 'SetSorted',
      NoIndex, Integer(False));
    AddProperty('Style', 'property: TListBoxStyle',
      'FStyle', 'SetStyle',
      NoIndex, Integer(lbStandard));
    AddProperty('TabWidth', 'property: Integer',
      'FTabWidth', 'SetTabWidth',
      NoIndex, 0);
    AddProperty('OnDrawItem', 'property: TDrawItemEvent',
      'FOnDrawItem', 'FOnDrawItem');
    AddProperty('OnMeasureItem', 'property: TMeasureItemEvent',
      'FOnMeasureItem', 'FOnMeasureItem');
    AddProperty('OnData', 'property: TLBGetDataEvent',
      'FOnData', 'FOnData');
    AddProperty('OnDataObject', 'property: TLBGetDataObjectEvent',
      'FOnDataObject', 'FOnDataObject');
    AddProperty('OnDataFind', 'property: TLBFindDataEvent',
      'FOnDataFind', 'FOnDataFind');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomListBox.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomListBox.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AddItem', @TSepiImportsTCustomListBox.AddItem,
      'procedure(Item: String; AObject: TObject)',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTCustomListBox.Clear,
      'procedure',
      mlkOverride);
    AddMethod('ClearSelection', @TSepiImportsTCustomListBox.ClearSelection,
      'procedure',
      mlkOverride);
    AddMethod('CopySelection', @TSepiImportsTCustomListBox.CopySelection,
      'procedure(Destination: TCustomListControl)',
      mlkOverride);
    AddMethod('DeleteSelected', @TSepiImportsTCustomListBox.DeleteSelected,
      'procedure',
      mlkOverride);
    AddMethod('ItemAtPos', @TSepiImportsTCustomListBox.ItemAtPos,
      'function(Pos: TPoint; Existing: Boolean): Integer');
    AddMethod('ItemRect', @TSepiImportsTCustomListBox.ItemRect,
      'function(Index: Integer): TRect');
    AddMethod('SelectAll', @TSepiImportsTCustomListBox.SelectAll,
      'procedure',
      mlkOverride);

    AddProperty('AutoCompleteDelay', 'property: Cardinal',
      'FAutoCompleteDelay', 'FAutoCompleteDelay',
      NoIndex, 500);
    AddProperty('AutoComplete', 'property: Boolean',
      'FAutoComplete', 'FAutoComplete',
      NoIndex, Integer(True));
    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');
    AddProperty('Count', 'property: Integer',
      'GetCount', 'SetCount');
    AddProperty('Items', 'property: TStrings',
      'FItems', 'SetItems');
    AddProperty('Selected', 'property[Index: Integer]: Boolean',
      'GetSelected', 'SetSelected');
    AddProperty('ScrollWidth', 'property: Integer',
      'GetScrollWidth', 'SetScrollWidth',
      NoIndex, 0);
    AddProperty('TopIndex', 'property: Integer',
      'GetTopIndex', 'SetTopIndex');

    CurrentVisibility := mvPublished;

    RedefineProperty('TabStop',
      '', '', Integer(True));

    Complete;
  end;
end;

{-----------------}
{ TListBox import }
{-----------------}

class function TSepiImportsTListBox.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TListBox));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Style');
    RedefineProperty('AutoComplete');
    RedefineProperty('AutoCompleteDelay');
    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelKind',
      '', '', Integer(bkNone));
    RedefineProperty('BevelOuter');
    RedefineProperty('BevelWidth');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderStyle');
    RedefineProperty('Color');
    RedefineProperty('Columns');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('ExtendedSelect');
    RedefineProperty('Font');
    RedefineProperty('ImeMode');
    RedefineProperty('ImeName');
    RedefineProperty('IntegralHeight');
    RedefineProperty('ItemHeight');
    RedefineProperty('Items');
    RedefineProperty('MultiSelect');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ScrollWidth');
    RedefineProperty('ShowHint');
    RedefineProperty('Sorted');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('TabWidth');
    RedefineProperty('Visible');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnData');
    RedefineProperty('OnDataFind');
    RedefineProperty('OnDataObject');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnDrawItem');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMeasureItem');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{-------------------}
{ TScrollBar import }
{-------------------}

procedure TSepiImportsTScrollBar.SetKind(Value: TScrollBarKind);
begin
  Kind := Value;
end;

procedure TSepiImportsTScrollBar.SetMax(Value: Integer);
begin
  Max := Value;
end;

procedure TSepiImportsTScrollBar.SetMin(Value: Integer);
begin
  Min := Value;
end;

procedure TSepiImportsTScrollBar.SetPosition(Value: Integer);
begin
  Position := Value;
end;

procedure TSepiImportsTScrollBar.SetPageSize(Value: Integer);
begin
  PageSize := Value;
end;

class function TSepiImportsTScrollBar.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScrollBar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FKind', System.TypeInfo(TScrollBarKind));
    AddField('FPosition', System.TypeInfo(Integer));
    AddField('FMin', System.TypeInfo(Integer));
    AddField('FMax', System.TypeInfo(Integer));
    AddField('FPageSize', System.TypeInfo(Integer));
    AddField('FRTLFactor', System.TypeInfo(Integer));
    AddField('FSmallChange', System.TypeInfo(TScrollBarInc));
    AddField('FLargeChange', System.TypeInfo(TScrollBarInc));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnScroll', System.TypeInfo(TScrollEvent));

    AddMethod('DoScroll', nil,
      'procedure(var Message: TWMScroll)');
    AddMethod('NotRightToLeft', nil,
      'function: Boolean');
    AddMethod('SetKind', @TSepiImportsTScrollBar.SetKind,
      'procedure(Value: TScrollBarKind)');
    AddMethod('SetMax', @TSepiImportsTScrollBar.SetMax,
      'procedure(Value: Integer)');
    AddMethod('SetMin', @TSepiImportsTScrollBar.SetMin,
      'procedure(Value: Integer)');
    AddMethod('SetPosition', @TSepiImportsTScrollBar.SetPosition,
      'procedure(Value: Integer)');
    AddMethod('SetPageSize', @TSepiImportsTScrollBar.SetPageSize,
      'procedure(Value: Integer)');
    AddMethod('CNHScroll', nil,
      'procedure(var Message: TWMHScroll)',
      mlkMessage, False, CN_HSCROLL);
    AddMethod('CNVScroll', nil,
      'procedure(var Message: TWMVScroll)',
      mlkMessage, False, CN_VSCROLL);
    AddMethod('CNCtlColorScrollBar', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CN_CTLCOLORSCROLLBAR);
    AddMethod('WMEraseBkgnd', nil,
      'procedure(var Message: TWMEraseBkgnd)',
      mlkMessage, False, WM_ERASEBKGND);

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTScrollBar.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTScrollBar.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('Change', @TSepiImportsTScrollBar.Change,
      'procedure',
      mlkDynamic);
    AddMethod('Scroll', @TSepiImportsTScrollBar.Scroll,
      'procedure(ScrollCode: TScrollCode; var ScrollPos: Integer)',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScrollBar.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('SetParams', @TSepiImportsTScrollBar.SetParams,
      'procedure(APosition, AMin, AMax: Integer)');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    AddProperty('Kind', 'property: TScrollBarKind',
      'FKind', 'SetKind',
      NoIndex, Integer(sbHorizontal));
    AddProperty('LargeChange', 'property: TScrollBarInc',
      'FLargeChange', 'FLargeChange',
      NoIndex, 1);
    AddProperty('Max', 'property: Integer',
      'FMax', 'SetMax',
      NoIndex, 100);
    AddProperty('Min', 'property: Integer',
      'FMin', 'SetMin',
      NoIndex, 0);
    AddProperty('PageSize', 'property: Integer',
      'FPageSize', 'SetPageSize');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    AddProperty('Position', 'property: Integer',
      'FPosition', 'SetPosition',
      NoIndex, 0);
    RedefineProperty('ShowHint');
    AddProperty('SmallChange', 'property: TScrollBarInc',
      'FSmallChange', 'FSmallChange',
      NoIndex, 1);
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop',
      '', '', Integer(True));
    RedefineProperty('Visible');
    RedefineProperty('OnContextPopup');
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    AddProperty('OnScroll', 'property: TScrollEvent',
      'FOnScroll', 'FOnScroll');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{--------------------------}
{ TCustomStaticText import }
{--------------------------}

procedure TSepiImportsTCustomStaticText.SetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TSepiImportsTCustomStaticText.SetBorderStyle(Value: TStaticBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TSepiImportsTCustomStaticText.SetFocusControl(Value: TWinControl);
begin
  FocusControl := Value;
end;

procedure TSepiImportsTCustomStaticText.SetShowAccelChar(Value: Boolean);
begin
  ShowAccelChar := Value;
end;

procedure TSepiImportsTCustomStaticText.SetTransparent(const Value: Boolean);
begin
  Transparent := Value;
end;

function TSepiImportsTCustomStaticText.GetTransparent: Boolean;
begin
  Result := Transparent;
end;

class function TSepiImportsTCustomStaticText.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomStaticText));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAlignment', System.TypeInfo(TAlignment));
    AddField('FAutoSize', System.TypeInfo(Boolean));
    AddField('FBorderStyle', System.TypeInfo(TStaticBorderStyle));
    AddField('FFocusControl', System.TypeInfo(TWinControl));
    AddField('FShowAccelChar', System.TypeInfo(Boolean));

    AddMethod('CNCtlColorStatic', nil,
      'procedure(var Message: TWMCtlColorStatic)',
      mlkMessage, False, CN_CTLCOLORSTATIC);
    AddMethod('CMDialogChar', nil,
      'procedure(var Message: TCMDialogChar)',
      mlkMessage, False, CM_DIALOGCHAR);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);
    AddMethod('AdjustBounds', nil,
      'procedure');
    AddMethod('SetAlignment', @TSepiImportsTCustomStaticText.SetAlignment,
      'procedure(Value: TAlignment)');
    AddMethod('SetBorderStyle', @TSepiImportsTCustomStaticText.SetBorderStyle,
      'procedure(Value: TStaticBorderStyle)');
    AddMethod('SetFocusControl', @TSepiImportsTCustomStaticText.SetFocusControl,
      'procedure(Value: TWinControl)');
    AddMethod('SetShowAccelChar', @TSepiImportsTCustomStaticText.SetShowAccelChar,
      'procedure(Value: Boolean)');
    AddMethod('SetTransparent', @TSepiImportsTCustomStaticText.SetTransparent,
      'procedure(const Value: Boolean)');
    AddMethod('GetTransparent', @TSepiImportsTCustomStaticText.GetTransparent,
      'function: Boolean');

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTCustomStaticText.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTCustomStaticText.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTCustomStaticText.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('SetAutoSize', @TSepiImportsTCustomStaticText.SetAutoSize,
      'procedure(Value: Boolean)',
      mlkOverride);

    AddProperty('Alignment', 'property: TAlignment',
      'FAlignment', 'SetAlignment',
      NoIndex, Integer(taLeftJustify));
    AddProperty('AutoSize', 'property: Boolean',
      'FAutoSize', 'SetAutoSize',
      NoIndex, Integer(True));
    AddProperty('BorderStyle', 'property: TStaticBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(sbsNone));
    AddProperty('FocusControl', 'property: TWinControl',
      'FFocusControl', 'SetFocusControl');
    AddProperty('ShowAccelChar', 'property: Boolean',
      'FShowAccelChar', 'SetShowAccelChar',
      NoIndex, Integer(True));
    AddProperty('Transparent', 'property: Boolean',
      'GetTransparent', 'SetTransparent',
      NoIndex, Integer(True));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomStaticText.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    Complete;
  end;
end;

{--------------------}
{ TStaticText import }
{--------------------}

class function TSepiImportsTStaticText.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TStaticText));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Alignment');
    RedefineProperty('Anchors');
    RedefineProperty('AutoSize');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelKind',
      '', '', Integer(bkNone));
    RedefineProperty('BevelOuter');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderStyle');
    RedefineProperty('Caption');
    RedefineProperty('Color',
      '', '', NoDefaultValue);
    RedefineProperty('Constraints');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('FocusControl');
    RedefineProperty('Font');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowAccelChar');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Transparent');
    RedefineProperty('Visible');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{-------------------}
{ TSelection import }
{-------------------}

function SepiImportTSelection(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TSelection', False, True);

  with Result do
  begin
    AddField('StartPos', System.TypeInfo(Integer));
    AddField('EndPos', System.TypeInfo(Integer), True);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'StdCtrls',
    ['Messages', 'Windows', 'SysUtils', 'Classes', 'Controls', 'Forms', 'Menus', 'Graphics']);

  // Types
  TSepiImportsTCustomGroupBox.SepiImport(Result);
  TSepiImportsTGroupBox.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTextLayout));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEllipsisPosition));
  TSepiImportsTCustomLabel.SepiImport(Result);
  TSepiImportsTLabel.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEditCharCase));
  TSepiImportsTCustomEdit.SepiImport(Result);
  TSepiImportsTEdit.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TScrollStyle));
  TSepiImportsTCustomMemo.SepiImport(Result);
  TSepiImportsTMemo.SepiImport(Result);
  TSepiTypeAlias.Create(Result, 'TOwnerDrawState', TypeInfo(Windows.TOwnerDrawState));
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomCombo));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDrawItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMeasureItemEvent));
  TSepiImportsTCustomComboBoxStrings.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TCustomComboBoxStringsClass', TypeInfo(TCustomComboBoxStrings), True);
  TSepiImportsTCustomCombo.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TComboBoxStyle));
  TSepiImportsTCustomComboBox.SepiImport(Result);
  TSepiImportsTComboBox.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TButtonControl));
  TSepiImportsTButtonActionLink.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TButtonActionLinkClass', TypeInfo(TButtonActionLink), True);
  TSepiImportsTButtonControl.SepiImport(Result);
  TSepiImportsTButton.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCheckBoxState));
  TSepiImportsTCustomCheckBox.SepiImport(Result);
  TSepiImportsTCheckBox.SepiImport(Result);
  TSepiImportsTRadioButton.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TListBoxStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLBGetDataEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLBGetDataObjectEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLBFindDataEvent));
  TSepiImportsTCustomListBox.SepiImport(Result);
  TSepiImportsTListBox.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TScrollCode));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TScrollEvent));
  TSepiImportsTScrollBar.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStaticBorderStyle));
  TSepiImportsTCustomStaticText.SepiImport(Result);
  TSepiImportsTStaticText.SepiImport(Result);
  SepiImportTSelection(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('StdCtrls', ImportUnit);
end.

