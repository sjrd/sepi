{*
  Importe l'unité Menus dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsMenus;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Windows, Classes, Graphics, ImgList, Menus;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEMenuError = class(EMenuError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMenuActionLink = class(TMenuActionLink)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMenuItemEnumerator = class(TMenuItemEnumerator)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMenuItem = class(TMenuItem)
  private
    function GetAction: TBasicAction;
    function GetBitmap: TBitmap;
    procedure SetAction(Value: TBasicAction);
    procedure SetBitmap(Value: TBitmap);
    procedure SetSubMenuImages(Value: TCustomImageList);
    procedure SetAutoHotkeys(const Value: TMenuItemAutoFlag);
    procedure SetAutoLineReduction(const Value: TMenuItemAutoFlag);
    procedure Add_0(Item: TMenuItem);
    procedure Add_1(const AItems: array of TMenuItem);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMenu = class(TMenu)
  private
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure SetWindowHandle(Value: HWND);
    function GetAutoHotkeys: TMenuAutoFlag;
    procedure SetAutoHotkeys(const Value: TMenuAutoFlag);
    function GetAutoLineReduction: TMenuAutoFlag;
    procedure SetAutoLineReduction(const Value: TMenuAutoFlag);
    procedure ParentBiDiModeChanged_0;
    procedure ParentBiDiModeChanged_1(AControl: TObject);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMainMenu = class(TMainMenu)
  private
    procedure SetAutoMerge(Value: Boolean);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTPopupMenu = class(TPopupMenu)
  private
    function GetHelpContext: THelpContext;
    procedure SetHelpContext(Value: THelpContext);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTPopupList = class(TPopupList)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMenuItemStack = class(TMenuItemStack)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{-------------------}
{ EMenuError import }
{-------------------}

class function TSepiImportsEMenuError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EMenuError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ TMenuActionLink import }
{------------------------}

class function TSepiImportsTMenuActionLink.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMenuActionLink));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FClient', System.TypeInfo(TMenuItem));

    AddMethod('AssignClient', @TSepiImportsTMenuActionLink.AssignClient,
      'procedure(AClient: TObject)',
      mlkOverride);
    AddMethod('IsAutoCheckLinked', @TSepiImportsTMenuActionLink.IsAutoCheckLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsCaptionLinked', @TSepiImportsTMenuActionLink.IsCaptionLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsCheckedLinked', @TSepiImportsTMenuActionLink.IsCheckedLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsEnabledLinked', @TSepiImportsTMenuActionLink.IsEnabledLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsHelpContextLinked', @TSepiImportsTMenuActionLink.IsHelpContextLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsHintLinked', @TSepiImportsTMenuActionLink.IsHintLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsGroupIndexLinked', @TSepiImportsTMenuActionLink.IsGroupIndexLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsImageIndexLinked', @TSepiImportsTMenuActionLink.IsImageIndexLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsShortCutLinked', @TSepiImportsTMenuActionLink.IsShortCutLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsVisibleLinked', @TSepiImportsTMenuActionLink.IsVisibleLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsOnExecuteLinked', @TSepiImportsTMenuActionLink.IsOnExecuteLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('SetAutoCheck', @TSepiImportsTMenuActionLink.SetAutoCheck,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetCaption', @TSepiImportsTMenuActionLink.SetCaption,
      'procedure(const Value: string)',
      mlkOverride);
    AddMethod('SetChecked', @TSepiImportsTMenuActionLink.SetChecked,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetEnabled', @TSepiImportsTMenuActionLink.SetEnabled,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetHelpContext', @TSepiImportsTMenuActionLink.SetHelpContext,
      'procedure(Value: THelpContext)',
      mlkOverride);
    AddMethod('SetHint', @TSepiImportsTMenuActionLink.SetHint,
      'procedure(const Value: string)',
      mlkOverride);
    AddMethod('SetImageIndex', @TSepiImportsTMenuActionLink.SetImageIndex,
      'procedure(Value: Integer)',
      mlkOverride);
    AddMethod('SetShortCut', @TSepiImportsTMenuActionLink.SetShortCut,
      'procedure(Value: TShortCut)',
      mlkOverride);
    AddMethod('SetVisible', @TSepiImportsTMenuActionLink.SetVisible,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetOnExecute', @TSepiImportsTMenuActionLink.SetOnExecute,
      'procedure(Value: TNotifyEvent)',
      mlkOverride);

    Complete;
  end;
end;

{----------------------------}
{ TMenuItemEnumerator import }
{----------------------------}

class function TSepiImportsTMenuItemEnumerator.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMenuItemEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FMenuItem', System.TypeInfo(TMenuItem));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMenuItemEnumerator.Create,
      'constructor(AMenuItem: TMenuItem)');
    AddMethod('GetCurrent', @TSepiImportsTMenuItemEnumerator.GetCurrent,
      'function: TMenuItem');
    AddMethod('MoveNext', @TSepiImportsTMenuItemEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: TMenuItem',
      'GetCurrent', '');

    Complete;
  end;
end;

{------------------}
{ TMenuItem import }
{------------------}

function TSepiImportsTMenuItem.GetAction: TBasicAction;
begin
  Result := Action;
end;

function TSepiImportsTMenuItem.GetBitmap: TBitmap;
begin
  Result := Bitmap;
end;

procedure TSepiImportsTMenuItem.SetAction(Value: TBasicAction);
begin
  Action := Value;
end;

procedure TSepiImportsTMenuItem.SetBitmap(Value: TBitmap);
begin
  Bitmap := Value;
end;

procedure TSepiImportsTMenuItem.SetSubMenuImages(Value: TCustomImageList);
begin
  SubMenuImages := Value;
end;

procedure TSepiImportsTMenuItem.SetAutoHotkeys(const Value: TMenuItemAutoFlag);
begin
  AutoHotkeys := Value;
end;

procedure TSepiImportsTMenuItem.SetAutoLineReduction(const Value: TMenuItemAutoFlag);
begin
  AutoLineReduction := Value;
end;

procedure TSepiImportsTMenuItem.Add_0(Item: TMenuItem);
begin
  Add(Item);
end;

procedure TSepiImportsTMenuItem.Add_1(const AItems: array of TMenuItem);
begin
  Add(AItems);
end;

class function TSepiImportsTMenuItem.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TMenuItem'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TMenuItem));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCaption', System.TypeInfo(string));
    AddField('FHandle', System.TypeInfo(HMENU));
    AddField('FChecked', System.TypeInfo(Boolean));
    AddField('FEnabled', System.TypeInfo(Boolean));
    AddField('FDefault', System.TypeInfo(Boolean));
    AddField('FAutoHotkeys', System.TypeInfo(TMenuItemAutoFlag));
    AddField('FAutoLineReduction', System.TypeInfo(TMenuItemAutoFlag));
    AddField('FRadioItem', System.TypeInfo(Boolean));
    AddField('FVisible', System.TypeInfo(Boolean));
    AddField('FGroupIndex', System.TypeInfo(Byte));
    AddField('FImageIndex', System.TypeInfo(TImageIndex));
    AddField('FActionLink', System.TypeInfo(TMenuActionLink));
    AddField('FBreak', System.TypeInfo(TMenuBreak));
    AddField('FBitmap', System.TypeInfo(TBitmap));
    AddField('FCommand', System.TypeInfo(Word));
    AddField('FHelpContext', System.TypeInfo(THelpContext));
    AddField('FHint', System.TypeInfo(string));
    AddField('FItems', System.TypeInfo(TList));
    AddField('FShortCut', System.TypeInfo(TShortCut));
    AddField('FParent', System.TypeInfo(TMenuItem));
    AddField('FMerged', System.TypeInfo(TMenuItem));
    AddField('FMergedWith', System.TypeInfo(TMenuItem));
    AddField('FMenu', System.TypeInfo(TMenu));
    AddField('FStreamedRebuild', System.TypeInfo(Boolean));
    AddField('FImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FSubMenuImages', System.TypeInfo(TCustomImageList));
    AddField('FOnChange', System.TypeInfo(TMenuChangeEvent));
    AddField('FOnClick', System.TypeInfo(TNotifyEvent));
    AddField('FOnDrawItem', System.TypeInfo(TMenuDrawItemEvent));
    AddField('FOnAdvancedDrawItem', System.TypeInfo(TAdvancedMenuDrawItemEvent));
    AddField('FOnMeasureItem', System.TypeInfo(TMenuMeasureItemEvent));
    AddField('FAutoCheck', System.TypeInfo(Boolean));

    AddMethod('AppendTo', nil,
      'procedure(Menu: HMENU; ARightToLeft: Boolean)');
    AddMethod('DoActionChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('ReadShortCutText', nil,
      'procedure(Reader: TReader)');
    AddMethod('MergeWith', nil,
      'procedure(Menu: TMenuItem)');
    AddMethod('RebuildHandle', nil,
      'procedure');
    AddMethod('PopulateMenu', nil,
      'procedure');
    AddMethod('SubItemChanged', nil,
      'procedure(Sender: TObject; Source: TMenuItem; Rebuild: Boolean)');
    AddMethod('TurnSiblingsOff', nil,
      'procedure');
    AddMethod('VerifyGroupIndex', nil,
      'procedure(Position: Integer; Value: Byte)');
    AddMethod('GetAction', @TSepiImportsTMenuItem.GetAction,
      'function: TBasicAction');
    AddMethod('GetBitmap', @TSepiImportsTMenuItem.GetBitmap,
      'function: TBitmap');
    AddMethod('SetAction', @TSepiImportsTMenuItem.SetAction,
      'procedure(Value: TBasicAction)');
    AddMethod('SetBitmap', @TSepiImportsTMenuItem.SetBitmap,
      'procedure(Value: TBitmap)');
    AddMethod('SetSubMenuImages', @TSepiImportsTMenuItem.SetSubMenuImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('InitiateActions', nil,
      'procedure');
    AddMethod('IsCaptionStored', nil,
      'function: Boolean');
    AddMethod('IsCheckedStored', nil,
      'function: Boolean');
    AddMethod('IsEnabledStored', nil,
      'function: Boolean');
    AddMethod('IsHelpContextStored', nil,
      'function: Boolean');
    AddMethod('IsHintStored', nil,
      'function: Boolean');
    AddMethod('IsImageIndexStored', nil,
      'function: Boolean');
    AddMethod('IsOnClickStored', nil,
      'function: Boolean');
    AddMethod('IsShortCutStored', nil,
      'function: Boolean');
    AddMethod('IsVisibleStored', nil,
      'function: Boolean');
    AddMethod('InternalRethinkHotkeys', nil,
      'function(ForceRethink: Boolean): Boolean');
    AddMethod('SetAutoHotkeys', @TSepiImportsTMenuItem.SetAutoHotkeys,
      'procedure(const Value: TMenuItemAutoFlag)');
    AddMethod('InternalRethinkLines', nil,
      'function(ForceRethink: Boolean): Boolean');
    AddMethod('SetAutoLineReduction', @TSepiImportsTMenuItem.SetAutoLineReduction,
      'procedure(const Value: TMenuItemAutoFlag)');

    CurrentVisibility := mvProtected;

    AddMethod('ActionChange', @TSepiImportsTMenuItem.ActionChange,
      'procedure(Sender: TObject; CheckDefaults: Boolean)',
      mlkDynamic);
    AddMethod('AdvancedDrawItem', @TSepiImportsTMenuItem.AdvancedDrawItem,
      'procedure(ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState ; TopLevel: Boolean )',
      mlkVirtual);
    AddMethod('AssignTo', @TSepiImportsTMenuItem.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);
    AddMethod('DefineProperties', @TSepiImportsTMenuItem.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('DoDrawText', @TSepiImportsTMenuItem.DoDrawText,
      'procedure(ACanvas: TCanvas; const ACaption: string; var Rect: TRect ; Selected: Boolean ; Flags: Longint )');
    AddMethod('DrawItem', @TSepiImportsTMenuItem.DrawItem,
      'procedure(ACanvas: TCanvas; ARect: TRect; Selected: Boolean)',
      mlkVirtual);
    AddMethod('GetActionLinkClass', @TSepiImportsTMenuItem.GetActionLinkClass,
      'function: TMenuActionLinkClass',
      mlkDynamic);
    AddMethod('GetHandle', @TSepiImportsTMenuItem.GetHandle,
      'function: HMENU');
    AddMethod('GetCount', @TSepiImportsTMenuItem.GetCount,
      'function: Integer');
    AddMethod('GetChildren', @TSepiImportsTMenuItem.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('GetItem', @TSepiImportsTMenuItem.GetItem,
      'function(Index: Integer): TMenuItem');
    AddMethod('GetMenuIndex', @TSepiImportsTMenuItem.GetMenuIndex,
      'function: Integer');
    AddMethod('GetAutoHotkeys', @TSepiImportsTMenuItem.GetAutoHotkeys,
      'function: Boolean');
    AddMethod('GetAutoLineReduction', @TSepiImportsTMenuItem.GetAutoLineReduction,
      'function: Boolean');
    AddMethod('InsertNewLine', @TSepiImportsTMenuItem.InsertNewLine,
      'function(ABefore: Boolean; AItem: TMenuItem): Integer');
    AddMethod('MeasureItem', @TSepiImportsTMenuItem.MeasureItem,
      'procedure(ACanvas: TCanvas; var Width, Height: Integer)',
      mlkVirtual);
    AddMethod('MenuChanged', @TSepiImportsTMenuItem.MenuChanged,
      'procedure(Rebuild: Boolean)',
      mlkVirtual);
    AddMethod('Loaded', @TSepiImportsTMenuItem.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTMenuItem.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation )',
      mlkOverride);
    AddMethod('SetBreak', @TSepiImportsTMenuItem.SetBreak,
      'procedure(Value: TMenuBreak)');
    AddMethod('SetCaption', @TSepiImportsTMenuItem.SetCaption,
      'procedure(const Value: string)');
    AddMethod('SetChecked', @TSepiImportsTMenuItem.SetChecked,
      'procedure(Value: Boolean)');
    AddMethod('SetChildOrder', @TSepiImportsTMenuItem.SetChildOrder,
      'procedure(Child: TComponent; Order: Integer)',
      mlkOverride);
    AddMethod('SetDefault', @TSepiImportsTMenuItem.SetDefault,
      'procedure(Value: Boolean)');
    AddMethod('SetEnabled', @TSepiImportsTMenuItem.SetEnabled,
      'procedure(Value: Boolean)');
    AddMethod('SetGroupIndex', @TSepiImportsTMenuItem.SetGroupIndex,
      'procedure(Value: Byte)');
    AddMethod('SetImageIndex', @TSepiImportsTMenuItem.SetImageIndex,
      'procedure(Value: TImageIndex)');
    AddMethod('SetMenuIndex', @TSepiImportsTMenuItem.SetMenuIndex,
      'procedure(Value: Integer)');
    AddMethod('SetParentComponent', @TSepiImportsTMenuItem.SetParentComponent,
      'procedure(Value: TComponent)',
      mlkOverride);
    AddMethod('SetRadioItem', @TSepiImportsTMenuItem.SetRadioItem,
      'procedure(Value: Boolean)');
    AddMethod('SetShortCut', @TSepiImportsTMenuItem.SetShortCut,
      'procedure(Value: TShortCut)');
    AddMethod('SetVisible', @TSepiImportsTMenuItem.SetVisible,
      'procedure(Value: Boolean)');
    AddMethod('UpdateItems', @TSepiImportsTMenuItem.UpdateItems,
      'procedure');

    AddProperty('ActionLink', 'property: TMenuActionLink',
      'FActionLink', 'FActionLink');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMenuItem.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTMenuItem.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('InitiateAction', @TSepiImportsTMenuItem.InitiateAction,
      'procedure',
      mlkVirtual);
    AddMethod('Insert', @TSepiImportsTMenuItem.Insert,
      'procedure(Index: Integer; Item: TMenuItem)');
    AddMethod('Delete', @TSepiImportsTMenuItem.Delete,
      'procedure(Index: Integer)');
    AddMethod('Clear', @TSepiImportsTMenuItem.Clear,
      'procedure');
    AddMethod('Click', @TSepiImportsTMenuItem.Click,
      'procedure',
      mlkVirtual);
    AddMethod('Find', @TSepiImportsTMenuItem.Find,
      'function(ACaption: string): TMenuItem');
    AddMethod('IndexOf', @TSepiImportsTMenuItem.IndexOf,
      'function(Item: TMenuItem): Integer');
    AddMethod('IsLine', @TSepiImportsTMenuItem.IsLine,
      'function: Boolean');
    AddMethod('GetEnumerator', @TSepiImportsTMenuItem.GetEnumerator,
      'function: TMenuItemEnumerator');
    AddMethod('GetImageList', @TSepiImportsTMenuItem.GetImageList,
      'function: TCustomImageList');
    AddMethod('GetParentComponent', @TSepiImportsTMenuItem.GetParentComponent,
      'function: TComponent',
      mlkOverride);
    AddMethod('GetParentMenu', @TSepiImportsTMenuItem.GetParentMenu,
      'function: TMenu');
    AddMethod('HasParent', @TSepiImportsTMenuItem.HasParent,
      'function: Boolean',
      mlkOverride);
    AddMethod('NewTopLine', @TSepiImportsTMenuItem.NewTopLine,
      'function: Integer');
    AddMethod('NewBottomLine', @TSepiImportsTMenuItem.NewBottomLine,
      'function: Integer');
    AddMethod('InsertNewLineBefore', @TSepiImportsTMenuItem.InsertNewLineBefore,
      'function(AItem: TMenuItem): Integer');
    AddMethod('InsertNewLineAfter', @TSepiImportsTMenuItem.InsertNewLineAfter,
      'function(AItem: TMenuItem): Integer');
    AddOverloadedMethod('Add', @TSepiImportsTMenuItem.Add_0,
      'procedure(Item: TMenuItem)');
    AddOverloadedMethod('Add', @TSepiImportsTMenuItem.Add_1,
      'procedure(const AItems: array of TMenuItem)');
    AddMethod('Remove', @TSepiImportsTMenuItem.Remove,
      'procedure(Item: TMenuItem)');
    AddMethod('RethinkHotkeys', @TSepiImportsTMenuItem.RethinkHotkeys,
      'function: Boolean');
    AddMethod('RethinkLines', @TSepiImportsTMenuItem.RethinkLines,
      'function: Boolean');

    AddProperty('Command', 'property: Word',
      'FCommand', '');
    AddProperty('Handle', 'property: HMENU',
      'GetHandle', '');
    AddProperty('Count', 'property: Integer',
      'GetCount', '');
    AddProperty('Items', 'property[Index: Integer]: TMenuItem',
      'GetItem', '', True);
    AddProperty('MenuIndex', 'property: Integer',
      'GetMenuIndex', 'SetMenuIndex');
    AddProperty('Parent', 'property: TMenuItem',
      'FParent', '');

    CurrentVisibility := mvPublished;

    AddProperty('Action', 'property: TBasicAction',
      'GetAction', 'SetAction');
    AddProperty('AutoCheck', 'property: Boolean',
      'FAutoCheck', 'FAutoCheck',
      NoIndex, Integer(False));
    AddProperty('AutoHotkeys', 'property: TMenuItemAutoFlag',
      'FAutoHotkeys', 'SetAutoHotkeys',
      NoIndex, Integer(maParent));
    AddProperty('AutoLineReduction', 'property: TMenuItemAutoFlag',
      'FAutoLineReduction', 'SetAutoLineReduction',
      NoIndex, Integer(maParent));
    AddProperty('Bitmap', 'property: TBitmap',
      'GetBitmap', 'SetBitmap');
    AddProperty('Break', 'property: TMenuBreak',
      'FBreak', 'SetBreak',
      NoIndex, Integer(mbNone));
    AddProperty('Caption', 'property: string',
      'FCaption', 'SetCaption',
      NoIndex, NoDefaultValue, 'IsCaptionStored');
    AddProperty('Checked', 'property: Boolean',
      'FChecked', 'SetChecked',
      NoIndex, Integer(False), 'IsCheckedStored');
    AddProperty('SubMenuImages', 'property: TCustomImageList',
      'FSubMenuImages', 'SetSubMenuImages');
    AddProperty('Default', 'property: Boolean',
      'FDefault', 'SetDefault',
      NoIndex, Integer(False));
    AddProperty('Enabled', 'property: Boolean',
      'FEnabled', 'SetEnabled',
      NoIndex, Integer(True), 'IsEnabledStored');
    AddProperty('GroupIndex', 'property: Byte',
      'FGroupIndex', 'SetGroupIndex',
      NoIndex, 0);
    AddProperty('HelpContext', 'property: THelpContext',
      'FHelpContext', 'FHelpContext',
      NoIndex, 0, 'IsHelpContextStored');
    AddProperty('Hint', 'property: string',
      'FHint', 'FHint',
      NoIndex, NoDefaultValue, 'IsHintStored');
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImageIndex',
      NoIndex, -1, 'IsImageIndexStored');
    AddProperty('RadioItem', 'property: Boolean',
      'FRadioItem', 'SetRadioItem',
      NoIndex, Integer(False));
    AddProperty('ShortCut', 'property: TShortCut',
      'FShortCut', 'SetShortCut',
      NoIndex, 0, 'IsShortCutStored');
    AddProperty('Visible', 'property: Boolean',
      'FVisible', 'SetVisible',
      NoIndex, Integer(True), 'IsVisibleStored');
    AddProperty('OnClick', 'property: TNotifyEvent',
      'FOnClick', 'FOnClick',
      NoIndex, NoDefaultValue, 'IsOnClickStored');
    AddProperty('OnDrawItem', 'property: TMenuDrawItemEvent',
      'FOnDrawItem', 'FOnDrawItem');
    AddProperty('OnAdvancedDrawItem', 'property: TAdvancedMenuDrawItemEvent',
      'FOnAdvancedDrawItem', 'FOnAdvancedDrawItem');
    AddProperty('OnMeasureItem', 'property: TMenuMeasureItemEvent',
      'FOnMeasureItem', 'FOnMeasureItem');

    Complete;
  end;
end;

{--------------}
{ TMenu import }
{--------------}

procedure TSepiImportsTMenu.SetBiDiMode(Value: TBiDiMode);
begin
  BiDiMode := Value;
end;

procedure TSepiImportsTMenu.SetOwnerDraw(Value: Boolean);
begin
  OwnerDraw := Value;
end;

procedure TSepiImportsTMenu.SetImages(Value: TCustomImageList);
begin
  Images := Value;
end;

procedure TSepiImportsTMenu.SetParentBiDiMode(Value: Boolean);
begin
  ParentBiDiMode := Value;
end;

procedure TSepiImportsTMenu.SetWindowHandle(Value: HWND);
begin
  WindowHandle := Value;
end;

function TSepiImportsTMenu.GetAutoHotkeys: TMenuAutoFlag;
begin
  Result := AutoHotkeys;
end;

procedure TSepiImportsTMenu.SetAutoHotkeys(const Value: TMenuAutoFlag);
begin
  AutoHotkeys := Value;
end;

function TSepiImportsTMenu.GetAutoLineReduction: TMenuAutoFlag;
begin
  Result := AutoLineReduction;
end;

procedure TSepiImportsTMenu.SetAutoLineReduction(const Value: TMenuAutoFlag);
begin
  AutoLineReduction := Value;
end;

procedure TSepiImportsTMenu.ParentBiDiModeChanged_0;
begin
  ParentBiDiModeChanged;
end;

procedure TSepiImportsTMenu.ParentBiDiModeChanged_1(AControl: TObject);
begin
  ParentBiDiModeChanged(AControl);
end;

class function TSepiImportsTMenu.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TMenu'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TMenu));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FBiDiMode', System.TypeInfo(TBiDiMode));
    AddField('FItems', System.TypeInfo(TMenuItem));
    AddField('FWindowHandle', System.TypeInfo(HWND));
    AddField('FMenuImage', System.TypeInfo(string));
    AddField('FOwnerDraw', System.TypeInfo(Boolean));
    AddField('FParentBiDiMode', System.TypeInfo(Boolean));
    AddField('FImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FImages', System.TypeInfo(TCustomImageList));
    AddField('FOnChange', System.TypeInfo(TMenuChangeEvent));

    AddMethod('SetBiDiMode', @TSepiImportsTMenu.SetBiDiMode,
      'procedure(Value: TBiDiMode)');
    AddMethod('SetOwnerDraw', @TSepiImportsTMenu.SetOwnerDraw,
      'procedure(Value: Boolean)');
    AddMethod('SetImages', @TSepiImportsTMenu.SetImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('SetParentBiDiMode', @TSepiImportsTMenu.SetParentBiDiMode,
      'procedure(Value: Boolean)');
    AddMethod('SetWindowHandle', @TSepiImportsTMenu.SetWindowHandle,
      'procedure(Value: HWND)');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('IsBiDiModeStored', nil,
      'function: Boolean');
    AddMethod('UpdateImage', nil,
      'function: Boolean');
    AddMethod('GetAutoHotkeys', @TSepiImportsTMenu.GetAutoHotkeys,
      'function: TMenuAutoFlag');
    AddMethod('SetAutoHotkeys', @TSepiImportsTMenu.SetAutoHotkeys,
      'procedure(const Value: TMenuAutoFlag)');
    AddMethod('GetAutoLineReduction', @TSepiImportsTMenu.GetAutoLineReduction,
      'function: TMenuAutoFlag');
    AddMethod('SetAutoLineReduction', @TSepiImportsTMenu.SetAutoLineReduction,
      'procedure(const Value: TMenuAutoFlag)');

    CurrentVisibility := mvProtected;

    AddMethod('AdjustBiDiBehavior', @TSepiImportsTMenu.AdjustBiDiBehavior,
      'procedure');
    AddMethod('DoChange', @TSepiImportsTMenu.DoChange,
      'procedure(Source: TMenuItem; Rebuild: Boolean)',
      mlkVirtual);
    AddMethod('DoBiDiModeChanged', @TSepiImportsTMenu.DoBiDiModeChanged,
      'procedure');
    AddMethod('DoGetMenuString', @TSepiImportsTMenu.DoGetMenuString,
      'function(Menu: HMENU; ItemID: UINT; Str: PChar; MaxCount: Integer ; Flag: UINT ) : Integer');
    AddMethod('GetChildren', @TSepiImportsTMenu.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('GetHandle', @TSepiImportsTMenu.GetHandle,
      'function: HMENU',
      mlkVirtual);
    AddMethod('IsOwnerDraw', @TSepiImportsTMenu.IsOwnerDraw,
      'function: Boolean');
    AddMethod('Loaded', @TSepiImportsTMenu.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('MenuChanged', @TSepiImportsTMenu.MenuChanged,
      'procedure(Sender: TObject; Source: TMenuItem; Rebuild: Boolean)',
      mlkVirtual);
    AddMethod('Notification', @TSepiImportsTMenu.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('SetChildOrder', @TSepiImportsTMenu.SetChildOrder,
      'procedure(Child: TComponent; Order: Integer)',
      mlkOverride);
    AddMethod('UpdateItems', @TSepiImportsTMenu.UpdateItems,
      'procedure');

    AddProperty('OnChange', 'property: TMenuChangeEvent',
      'FOnChange', 'FOnChange');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMenu.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('CreateMenuItem', @TSepiImportsTMenu.CreateMenuItem,
      'function: TMenuItem',
      mlkDynamic);
    AddMethod('Destroy', @TSepiImportsTMenu.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('DispatchCommand', @TSepiImportsTMenu.DispatchCommand,
      'function(ACommand: Word): Boolean');
    AddMethod('DispatchPopup', @TSepiImportsTMenu.DispatchPopup,
      'function(AHandle: HMENU): Boolean');
    AddMethod('FindItem', @TSepiImportsTMenu.FindItem,
      'function(Value: Integer; Kind: TFindItemKind): TMenuItem');
    AddMethod('GetHelpContext', @TSepiImportsTMenu.GetHelpContext,
      'function(Value: Integer; ByCommand: Boolean): THelpContext');

    AddProperty('Images', 'property: TCustomImageList',
      'FImages', 'SetImages');

    AddMethod('IsRightToLeft', @TSepiImportsTMenu.IsRightToLeft,
      'function: Boolean');
    AddMethod('IsShortCut', @TSepiImportsTMenu.IsShortCut,
      'function(var Message: TWMKey): Boolean',
      mlkDynamic);
    AddOverloadedMethod('ParentBiDiModeChanged', @TSepiImportsTMenu.ParentBiDiModeChanged_0,
      'procedure');
    AddOverloadedMethod('ParentBiDiModeChanged', @TSepiImportsTMenu.ParentBiDiModeChanged_1,
      'procedure(AControl: TObject)');
    AddMethod('ProcessMenuChar', @TSepiImportsTMenu.ProcessMenuChar,
      'procedure(var Message: TWMMenuChar)');

    AddProperty('AutoHotkeys', 'property: TMenuAutoFlag',
      'GetAutoHotkeys', 'SetAutoHotkeys',
      NoIndex, Integer(maAutomatic));
    AddProperty('AutoLineReduction', 'property: TMenuAutoFlag',
      'GetAutoLineReduction', 'SetAutoLineReduction',
      NoIndex, Integer(maAutomatic));
    AddProperty('BiDiMode', 'property: TBiDiMode',
      'FBiDiMode', 'SetBiDiMode',
      NoIndex, NoDefaultValue, 'IsBiDiModeStored');
    AddProperty('Handle', 'property: HMENU',
      'GetHandle', '');
    AddProperty('OwnerDraw', 'property: Boolean',
      'FOwnerDraw', 'SetOwnerDraw',
      NoIndex, Integer(False));
    AddProperty('ParentBiDiMode', 'property: Boolean',
      'FParentBiDiMode', 'SetParentBiDiMode',
      NoIndex, Integer(True));
    AddProperty('WindowHandle', 'property: HWND',
      'FWindowHandle', 'SetWindowHandle');

    CurrentVisibility := mvPublished;

    AddProperty('Items', 'property: TMenuItem',
      'FItems', '');

    Complete;
  end;
end;

{------------------}
{ TMainMenu import }
{------------------}

procedure TSepiImportsTMainMenu.SetAutoMerge(Value: Boolean);
begin
  AutoMerge := Value;
end;

class function TSepiImportsTMainMenu.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMainMenu));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOle2Menu', System.TypeInfo(HMENU));
    AddField('FAutoMerge', System.TypeInfo(Boolean));

    AddMethod('ItemChanged', nil,
      'procedure');
    AddMethod('SetAutoMerge', @TSepiImportsTMainMenu.SetAutoMerge,
      'procedure(Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('MenuChanged', @TSepiImportsTMainMenu.MenuChanged,
      'procedure(Sender: TObject; Source: TMenuItem; Rebuild: Boolean)',
      mlkOverride);
    AddMethod('GetHandle', @TSepiImportsTMainMenu.GetHandle,
      'function: HMENU',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Merge', @TSepiImportsTMainMenu.Merge,
      'procedure(Menu: TMainMenu)');
    AddMethod('Unmerge', @TSepiImportsTMainMenu.Unmerge,
      'procedure(Menu: TMainMenu)');
    AddMethod('PopulateOle2Menu', @TSepiImportsTMainMenu.PopulateOle2Menu,
      'procedure(SharedMenu: HMenu; const Groups: array of Integer; var Widths: array of Longint )');
    AddMethod('GetOle2AcceleratorTable', @TSepiImportsTMainMenu.GetOle2AcceleratorTable,
      'procedure(var AccelTable: HAccel; var AccelCount: Integer ; const Groups: array of Integer )');
    AddMethod('SetOle2MenuHandle', @TSepiImportsTMainMenu.SetOle2MenuHandle,
      'procedure(Handle: HMENU)');

    CurrentVisibility := mvPublished;

    RedefineProperty('AutoHotkeys');
    RedefineProperty('AutoLineReduction');
    AddProperty('AutoMerge', 'property: Boolean',
      'FAutoMerge', 'SetAutoMerge',
      NoIndex, Integer(False));
    RedefineProperty('BiDiMode');
    RedefineProperty('Images');
    RedefineProperty('OwnerDraw');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('OnChange');

    Complete;
  end;
end;

{-------------------}
{ TPopupMenu import }
{-------------------}

function TSepiImportsTPopupMenu.GetHelpContext: THelpContext;
begin
  Result := HelpContext;
end;

procedure TSepiImportsTPopupMenu.SetHelpContext(Value: THelpContext);
begin
  HelpContext := Value;
end;

class function TSepiImportsTPopupMenu.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPopupMenu));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPopupPoint', 'TPoint');
    AddField('FAlignment', System.TypeInfo(TPopupAlignment));
    AddField('FAutoPopup', System.TypeInfo(Boolean));
    AddField('FPopupComponent', System.TypeInfo(TComponent));
    AddField('FTrackButton', System.TypeInfo(TTrackButton));
    AddField('FMenuAnimation', System.TypeInfo(TMenuAnimation));
    AddField('FOnPopup', System.TypeInfo(TNotifyEvent));

    AddMethod('GetHelpContext', @TSepiImportsTPopupMenu.GetHelpContext,
      'function: THelpContext');
    AddMethod('SetHelpContext', @TSepiImportsTPopupMenu.SetHelpContext,
      'procedure(Value: THelpContext)');
    AddMethod('SetBiDiModeFromPopupControl', nil,
      'procedure');

    CurrentVisibility := mvProtected;

    AddMethod('UseRightToLeftAlignment', @TSepiImportsTPopupMenu.UseRightToLeftAlignment,
      'function: Boolean');
    AddMethod('DoPopup', @TSepiImportsTPopupMenu.DoPopup,
      'procedure(Sender: TObject)',
      mlkVirtual);
    AddMethod('SetPopupPoint', @TSepiImportsTPopupMenu.SetPopupPoint,
      'procedure(APopupPoint: TPoint)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPopupMenu.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTPopupMenu.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Popup', @TSepiImportsTPopupMenu.Popup,
      'procedure(X, Y: Integer)',
      mlkVirtual);

    AddProperty('PopupComponent', 'property: TComponent',
      'FPopupComponent', 'FPopupComponent');
    AddProperty('PopupPoint', 'property: TPoint',
      'FPopupPoint', '');

    CurrentVisibility := mvPublished;

    AddProperty('Alignment', 'property: TPopupAlignment',
      'FAlignment', 'FAlignment',
      NoIndex, Integer(paLeft));
    RedefineProperty('AutoHotkeys');
    RedefineProperty('AutoLineReduction');
    AddProperty('AutoPopup', 'property: Boolean',
      'FAutoPopup', 'FAutoPopup',
      NoIndex, Integer(True));
    RedefineProperty('BiDiMode');
    AddProperty('HelpContext', 'property: THelpContext',
      'GetHelpContext', 'SetHelpContext',
      NoIndex, 0);
    RedefineProperty('Images');
    AddProperty('MenuAnimation', 'property: TMenuAnimation',
      'FMenuAnimation', 'FMenuAnimation',
      NoIndex, 0);
    RedefineProperty('OwnerDraw');
    RedefineProperty('ParentBiDiMode');
    AddProperty('TrackButton', 'property: TTrackButton',
      'FTrackButton', 'FTrackButton',
      NoIndex, Integer(tbRightButton));
    RedefineProperty('OnChange');
    AddProperty('OnPopup', 'property: TNotifyEvent',
      'FOnPopup', 'FOnPopup');

    Complete;
  end;
end;

{-------------------}
{ TPopupList import }
{-------------------}

class function TSepiImportsTPopupList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPopupList));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FWindow', System.TypeInfo(HWND));

    AddMethod('MainWndProc', @TSepiImportsTPopupList.MainWndProc,
      'procedure(var Message: TMessage)');
    AddMethod('WndProc', @TSepiImportsTPopupList.WndProc,
      'procedure(var Message: TMessage)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddProperty('Window', 'property: HWND',
      'FWindow', '');

    AddMethod('Add', @TSepiImportsTPopupList.Add,
      'procedure(Popup: TPopupMenu)');
    AddMethod('Remove', @TSepiImportsTPopupList.Remove,
      'procedure(Popup: TPopupMenu)');

    Complete;
  end;
end;

{-----------------------}
{ TMenuItemStack import }
{-----------------------}

class function TSepiImportsTMenuItemStack.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMenuItemStack));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('ClearItem', @TSepiImportsTMenuItemStack.ClearItem,
      'procedure(AItem: TMenuItem)');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'Menus',
    ['Windows', 'SysUtils', 'Classes', 'Contnrs', 'Messages', 'Graphics', 'ImgList', 'ActnList']);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TMenuItem));
  TSepiImportsEMenuError.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TMenu));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMenuBreak));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMenuChangeEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMenuDrawItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAdvancedMenuDrawItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMenuMeasureItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMenuItemAutoFlag));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMenuAutoFlag));
  TSepiImportsTMenuActionLink.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TMenuActionLinkClass', TypeInfo(TMenuActionLink), True);
  TSepiImportsTMenuItemEnumerator.SepiImport(Result);
  TSepiImportsTMenuItem.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFindItemKind));
  TSepiImportsTMenu.SepiImport(Result);
  TSepiImportsTMainMenu.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPopupAlignment));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTrackButton));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMenuAnimations));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMenuAnimation));
  TSepiImportsTPopupMenu.SepiImport(Result);
  TSepiImportsTPopupList.SepiImport(Result);
  TSepiPointerType.Create(Result, 'PMenuItem', TypeInfo(TMenuItem), True);
  TSepiImportsTMenuItemStack.SepiImport(Result);

  // Global variables
  TSepiVariable.Create(Result, 'PopupList',
    PopupList, TypeInfo(TPopupList));
  TSepiVariable.Create(Result, 'ShortCutItems',
    ShortCutItems, TypeInfo(TMenuItemStack));

  // Routines
  TSepiMetaMethod.Create(Result, 'ShortCut', @ShortCut,
    'function(Key: Word; Shift: TShiftState): TShortCut');
  TSepiMetaMethod.Create(Result, 'ShortCutToKey', @ShortCutToKey,
    'procedure(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState)');
  TSepiMetaMethod.Create(Result, 'ShortCutToText', @ShortCutToText,
    'function(ShortCut: TShortCut): string');
  TSepiMetaMethod.Create(Result, 'TextToShortCut', @TextToShortCut,
    'function(Text: string): TShortCut');
  TSepiMetaMethod.Create(Result, 'ShortCutFromMessage', @ShortCutFromMessage,
    'function(Message: TWMKey): TShortCut');
  TSepiMetaMethod.Create(Result, 'NewMenu', @NewMenu,
    'function(Owner: TComponent; const AName: string; const Items: array of TMenuItem): TMainMenu');
  TSepiMetaMethod.Create(Result, 'NewPopupMenu', @NewPopupMenu,
    'function(Owner: TComponent; const AName: string; Alignment: TPopupAlignment ; AutoPopup: Boolean ; const Items: array of TMenuItem ) : TPopupMenu');
  TSepiMetaMethod.Create(Result, 'NewSubMenu', @NewSubMenu,
    'function(const ACaption: string; hCtx: THelpContext; const AName: string ; const Items: array of TMenuItem ; AEnabled: Boolean = True ) : TMenuItem');
  TSepiMetaMethod.Create(Result, 'NewItem', @NewItem,
    'function(const ACaption: string; AShortCut: TShortCut; AChecked, AEnabled: Boolean ; AOnClick: TNotifyEvent ; hCtx: THelpContext ; const AName: string ) : TMenuItem');
  TSepiMetaMethod.Create(Result, 'NewLine', @NewLine,
    'function: TMenuItem');
  TSepiMetaMethod.Create(Result, 'DrawMenuItem', @DrawMenuItem,
    'procedure(MenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState )');

  // Global variables
  TSepiVariable.Create(Result, 'ValidMenuHotkeys',
    ValidMenuHotkeys, TypeInfo(string));

  // Constants
  TSepiConstant.Create(Result, 'cHotkeyPrefix', cHotkeyPrefix);
  TSepiConstant.Create(Result, 'cLineCaption', cLineCaption);
  TSepiConstant.Create(Result, 'cDialogSuffix', cDialogSuffix);

  // Routines
  TSepiMetaMethod.Create(Result, 'StripHotkey', @StripHotkey,
    'function(const Text: string): string');
  TSepiMetaMethod.Create(Result, 'GetHotkey', @GetHotkey,
    'function(const Text: string): string');
  TSepiMetaMethod.Create(Result, 'AnsiSameCaption', @AnsiSameCaption,
    'function(const Text1, Text2: string): Boolean');
  TSepiMetaMethod.Create(Result, 'IsAltGRPressed', @IsAltGRPressed,
    'function: boolean');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Menus', ImportUnit);
end.

