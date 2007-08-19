{*
  Importe l'unité ActnList dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsActnList;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Classes, ImgList, ActnList;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTContainedAction = class(TContainedAction)
  private
    function GetIndex: Integer;
    procedure SetCategory(const Value: string);
    procedure SetIndex(Value: Integer);
    procedure SetActionList(AActionList: TCustomActionList);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTActionListEnumerator = class(TActionListEnumerator)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomActionList = class(TCustomActionList)
  private
    function GetAction(Index: Integer): TContainedAction;
    function GetActionCount: Integer;
    procedure SetAction(Index: Integer; Value: TContainedAction);
    procedure SetState(const Value: TActionListState);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTActionList = class(TActionList)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTShortCutList = class(TShortCutList)
  private
    function GetShortCuts(Index: Integer): TShortCut;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomAction = class(TCustomAction)
  private
    procedure SetAutoCheck(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetHelpType(Value: THelpType);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetShortCut(Value: TShortCut);
    procedure SetVisible(Value: Boolean);
    function GetSecondaryShortCuts: TShortCutList;
    procedure SetSecondaryShortCuts(const Value: TShortCutList);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTAction = class(TAction)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTActionLink = class(TActionLink)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{-------------------------}
{ TContainedAction import }
{-------------------------}

function TSepiImportsTContainedAction.GetIndex: Integer;
begin
  Result := Index;
end;

procedure TSepiImportsTContainedAction.SetCategory(const Value: string);
begin
  Category := Value;
end;

procedure TSepiImportsTContainedAction.SetIndex(Value: Integer);
begin
  Index := Value;
end;

procedure TSepiImportsTContainedAction.SetActionList(AActionList: TCustomActionList);
begin
  ActionList := AActionList;
end;

class function TSepiImportsTContainedAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TContainedAction));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCategory', System.TypeInfo(string));
    AddField('FActionList', System.TypeInfo(TCustomActionList));

    AddMethod('GetIndex', @TSepiImportsTContainedAction.GetIndex,
      'function: Integer');
    AddMethod('IsCategoryStored', nil,
      'function: Boolean');
    AddMethod('SetCategory', @TSepiImportsTContainedAction.SetCategory,
      'procedure(const Value: string)');
    AddMethod('SetIndex', @TSepiImportsTContainedAction.SetIndex,
      'procedure(Value: Integer)');
    AddMethod('SetActionList', @TSepiImportsTContainedAction.SetActionList,
      'procedure(AActionList: TCustomActionList)');

    CurrentVisibility := mvProtected;

    AddMethod('ReadState', @TSepiImportsTContainedAction.ReadState,
      'procedure(Reader: TReader)',
      mlkOverride);
    AddMethod('SetParentComponent', @TSepiImportsTContainedAction.SetParentComponent,
      'procedure(AParent: TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTContainedAction.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Execute', @TSepiImportsTContainedAction.Execute,
      'function: Boolean',
      mlkOverride);
    AddMethod('GetParentComponent', @TSepiImportsTContainedAction.GetParentComponent,
      'function: TComponent',
      mlkOverride);
    AddMethod('HasParent', @TSepiImportsTContainedAction.HasParent,
      'function: Boolean',
      mlkOverride);
    AddMethod('Update', @TSepiImportsTContainedAction.Update,
      'function: Boolean',
      mlkOverride);

    AddProperty('ActionList', 'property: TCustomActionList',
      'FActionList', 'SetActionList');
    AddProperty('Index', 'property: Integer',
      'GetIndex', 'SetIndex',
      NoIndex, NoDefaultValue, 'False');

    CurrentVisibility := mvPublished;

    AddProperty('Category', 'property: string',
      'FCategory', 'SetCategory',
      NoIndex, NoDefaultValue, 'IsCategoryStored');

    Complete;
  end;
end;

{------------------------------}
{ TActionListEnumerator import }
{------------------------------}

class function TSepiImportsTActionListEnumerator.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TActionListEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FActionList', System.TypeInfo(TCustomActionList));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTActionListEnumerator.Create,
      'constructor(AActionList: TCustomActionList)');
    AddMethod('GetCurrent', @TSepiImportsTActionListEnumerator.GetCurrent,
      'function: TContainedAction');
    AddMethod('MoveNext', @TSepiImportsTActionListEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: TContainedAction',
      'GetCurrent', '');

    Complete;
  end;
end;

{--------------------------}
{ TCustomActionList import }
{--------------------------}

function TSepiImportsTCustomActionList.GetAction(Index: Integer): TContainedAction;
begin
  Result := Actions[Index];
end;

function TSepiImportsTCustomActionList.GetActionCount: Integer;
begin
  Result := ActionCount;
end;

procedure TSepiImportsTCustomActionList.SetAction(Index: Integer; Value: TContainedAction);
begin
  Actions[Index] := Value;
end;

procedure TSepiImportsTCustomActionList.SetState(const Value: TActionListState);
begin
  State := Value;
end;

class function TSepiImportsTCustomActionList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomActionList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomActionList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FActions', System.TypeInfo(TList));
    AddField('FImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FImages', System.TypeInfo(TCustomImageList));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnExecute', System.TypeInfo(TActionEvent));
    AddField('FOnUpdate', System.TypeInfo(TActionEvent));
    AddField('FState', System.TypeInfo(TActionListState));
    AddField('FOnStateChange', System.TypeInfo(TNotifyEvent));

    AddMethod('GetAction', @TSepiImportsTCustomActionList.GetAction,
      'function(Index: Integer): TContainedAction');
    AddMethod('GetActionCount', @TSepiImportsTCustomActionList.GetActionCount,
      'function: Integer');
    AddMethod('SetAction', @TSepiImportsTCustomActionList.SetAction,
      'procedure(Index: Integer; Value: TContainedAction)');
    AddMethod('SetState', @TSepiImportsTCustomActionList.SetState,
      'procedure(const Value: TActionListState)');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');

    CurrentVisibility := mvProtected;

    AddMethod('AddAction', @TSepiImportsTCustomActionList.AddAction,
      'procedure(Action: TContainedAction)');
    AddMethod('RemoveAction', @TSepiImportsTCustomActionList.RemoveAction,
      'procedure(Action: TContainedAction)');
    AddMethod('Change', @TSepiImportsTCustomActionList.Change,
      'procedure',
      mlkVirtual);
    AddMethod('GetChildren', @TSepiImportsTCustomActionList.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTCustomActionList.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation )',
      mlkOverride);
    AddMethod('SetChildOrder', @TSepiImportsTCustomActionList.SetChildOrder,
      'procedure(Component: TComponent; Order: Integer)',
      mlkOverride);
    AddMethod('SetImages', @TSepiImportsTCustomActionList.SetImages,
      'procedure(Value: TCustomImageList)',
      mlkVirtual);

    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OnExecute', 'property: TActionEvent',
      'FOnExecute', 'FOnExecute');
    AddProperty('OnUpdate', 'property: TActionEvent',
      'FOnUpdate', 'FOnUpdate');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomActionList.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomActionList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('ExecuteAction', @TSepiImportsTCustomActionList.ExecuteAction,
      'function(Action: TBasicAction): Boolean',
      mlkOverride);
    AddMethod('GetEnumerator', @TSepiImportsTCustomActionList.GetEnumerator,
      'function: TActionListEnumerator');
    AddMethod('IsShortCut', @TSepiImportsTCustomActionList.IsShortCut,
      'function(var Message: TWMKey): Boolean');
    AddMethod('UpdateAction', @TSepiImportsTCustomActionList.UpdateAction,
      'function(Action: TBasicAction): Boolean',
      mlkOverride);

    AddProperty('Actions', 'property[Index: Integer]: TContainedAction',
      'GetAction', 'SetAction', True);
    AddProperty('ActionCount', 'property: Integer',
      'GetActionCount', '');
    AddProperty('Images', 'property: TCustomImageList',
      'FImages', 'SetImages');
    AddProperty('State', 'property: TActionListState',
      'FState', 'SetState',
      NoIndex, Integer(asNormal));
    AddProperty('OnStateChange', 'property: TNotifyEvent',
      'FOnStateChange', 'FOnStateChange');

    Complete;
  end;
end;

{--------------------}
{ TActionList import }
{--------------------}

class function TSepiImportsTActionList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TActionList));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Images');
    RedefineProperty('State');
    RedefineProperty('OnChange');
    RedefineProperty('OnExecute');
    RedefineProperty('OnStateChange');
    RedefineProperty('OnUpdate');

    Complete;
  end;
end;

{----------------------}
{ TShortCutList import }
{----------------------}

function TSepiImportsTShortCutList.GetShortCuts(Index: Integer): TShortCut;
begin
  Result := ShortCuts[Index];
end;

class function TSepiImportsTShortCutList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TShortCutList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetShortCuts', @TSepiImportsTShortCutList.GetShortCuts,
      'function(Index: Integer): TShortCut');

    CurrentVisibility := mvPublic;

    AddMethod('Add', @TSepiImportsTShortCutList.Add,
      'function(const S: String): Integer',
      mlkOverride);
    AddMethod('IndexOfShortCut', @TSepiImportsTShortCutList.IndexOfShortCut,
      'function(const Shortcut: TShortCut): Integer');

    AddProperty('ShortCuts', 'property[Index: Integer]: TShortCut',
      'GetShortCuts', '');

    Complete;
  end;
end;

{----------------------}
{ TCustomAction import }
{----------------------}

procedure TSepiImportsTCustomAction.SetAutoCheck(Value: Boolean);
begin
  AutoCheck := Value;
end;

procedure TSepiImportsTCustomAction.SetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TSepiImportsTCustomAction.SetChecked(Value: Boolean);
begin
  Checked := Value;
end;

procedure TSepiImportsTCustomAction.SetEnabled(Value: Boolean);
begin
  Enabled := Value;
end;

procedure TSepiImportsTCustomAction.SetGroupIndex(const Value: Integer);
begin
  GroupIndex := Value;
end;

procedure TSepiImportsTCustomAction.SetHelpType(Value: THelpType);
begin
  HelpType := Value;
end;

procedure TSepiImportsTCustomAction.SetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TSepiImportsTCustomAction.SetImageIndex(Value: TImageIndex);
begin
  ImageIndex := Value;
end;

procedure TSepiImportsTCustomAction.SetShortCut(Value: TShortCut);
begin
  ShortCut := Value;
end;

procedure TSepiImportsTCustomAction.SetVisible(Value: Boolean);
begin
  Visible := Value;
end;

function TSepiImportsTCustomAction.GetSecondaryShortCuts: TShortCutList;
begin
  Result := SecondaryShortCuts;
end;

procedure TSepiImportsTCustomAction.SetSecondaryShortCuts(const Value: TShortCutList);
begin
  SecondaryShortCuts := Value;
end;

class function TSepiImportsTCustomAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomAction));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDisableIfNoHandler', System.TypeInfo(Boolean));
    AddField('FCaption', System.TypeInfo(string));
    AddField('FChecking', System.TypeInfo(Boolean));
    AddField('FChecked', System.TypeInfo(Boolean));
    AddField('FEnabled', System.TypeInfo(Boolean));
    AddField('FGroupIndex', System.TypeInfo(Integer));
    AddField('FHelpType', System.TypeInfo(THelpType));
    AddField('FHelpContext', System.TypeInfo(THelpContext));
    AddField('FHelpKeyword', System.TypeInfo(string));
    AddField('FHint', System.TypeInfo(string));
    AddField('FImageIndex', System.TypeInfo(TImageIndex));
    AddField('FShortCut', System.TypeInfo(TShortCut));
    AddField('FVisible', System.TypeInfo(Boolean));
    AddField('FOnHint', System.TypeInfo(THintEvent));
    AddField('FSecondaryShortCuts', System.TypeInfo(TShortCutList));
    AddField('FSavedEnabledState', System.TypeInfo(Boolean));
    AddField('FAutoCheck', System.TypeInfo(Boolean));

    AddMethod('SetAutoCheck', @TSepiImportsTCustomAction.SetAutoCheck,
      'procedure(Value: Boolean)');
    AddMethod('SetCaption', @TSepiImportsTCustomAction.SetCaption,
      'procedure(const Value: string)');
    AddMethod('SetChecked', @TSepiImportsTCustomAction.SetChecked,
      'procedure(Value: Boolean)');
    AddMethod('SetEnabled', @TSepiImportsTCustomAction.SetEnabled,
      'procedure(Value: Boolean)');
    AddMethod('SetGroupIndex', @TSepiImportsTCustomAction.SetGroupIndex,
      'procedure(const Value: Integer)');
    AddMethod('SetHelpType', @TSepiImportsTCustomAction.SetHelpType,
      'procedure(Value: THelpType)');
    AddMethod('SetHint', @TSepiImportsTCustomAction.SetHint,
      'procedure(const Value: string)');
    AddMethod('SetImageIndex', @TSepiImportsTCustomAction.SetImageIndex,
      'procedure(Value: TImageIndex)');
    AddMethod('SetShortCut', @TSepiImportsTCustomAction.SetShortCut,
      'procedure(Value: TShortCut)');
    AddMethod('SetVisible', @TSepiImportsTCustomAction.SetVisible,
      'procedure(Value: Boolean)');
    AddMethod('GetSecondaryShortCuts', @TSepiImportsTCustomAction.GetSecondaryShortCuts,
      'function: TShortCutList');
    AddMethod('SetSecondaryShortCuts', @TSepiImportsTCustomAction.SetSecondaryShortCuts,
      'procedure(const Value: TShortCutList)');
    AddMethod('IsSecondaryShortCutsStored', nil,
      'function: Boolean');

    CurrentVisibility := mvProtected;

    AddField('FImage', System.TypeInfo(TObject));
    AddField('FMask', System.TypeInfo(TObject));

    AddMethod('AssignTo', @TSepiImportsTCustomAction.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);
    AddMethod('SetName', @TSepiImportsTCustomAction.SetName,
      'procedure(const Value: TComponentName)',
      mlkOverride);
    AddMethod('SetHelpContext', @TSepiImportsTCustomAction.SetHelpContext,
      'procedure(Value: THelpContext)',
      mlkVirtual);
    AddMethod('SetHelpKeyword', @TSepiImportsTCustomAction.SetHelpKeyword,
      'procedure(const Value: string)',
      mlkVirtual);
    AddMethod('HandleShortCut', @TSepiImportsTCustomAction.HandleShortCut,
      'function: Boolean',
      mlkVirtual);

    AddProperty('SavedEnabledState', 'property: Boolean',
      'FSavedEnabledState', 'FSavedEnabledState');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomAction.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomAction.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('DoHint', @TSepiImportsTCustomAction.DoHint,
      'function(var HintStr: string): Boolean',
      mlkDynamic);
    AddMethod('Execute', @TSepiImportsTCustomAction.Execute,
      'function: Boolean',
      mlkOverride);

    AddProperty('AutoCheck', 'property: Boolean',
      'FAutoCheck', 'SetAutoCheck',
      NoIndex, Integer(False));
    AddProperty('Caption', 'property: string',
      'FCaption', 'SetCaption');
    AddProperty('Checked', 'property: Boolean',
      'FChecked', 'SetChecked',
      NoIndex, Integer(False));
    AddProperty('DisableIfNoHandler', 'property: Boolean',
      'FDisableIfNoHandler', 'FDisableIfNoHandler',
      NoIndex, Integer(True));
    AddProperty('Enabled', 'property: Boolean',
      'FEnabled', 'SetEnabled',
      NoIndex, Integer(True));
    AddProperty('GroupIndex', 'property: Integer',
      'FGroupIndex', 'SetGroupIndex',
      NoIndex, 0);
    AddProperty('HelpContext', 'property: THelpContext',
      'FHelpContext', 'SetHelpContext',
      NoIndex, 0);
    AddProperty('HelpKeyword', 'property: string',
      'FHelpKeyword', 'SetHelpKeyword');
    AddProperty('HelpType', 'property: THelpType',
      'FHelpType', 'SetHelpType',
      NoIndex, Integer(htKeyword));
    AddProperty('Hint', 'property: string',
      'FHint', 'SetHint');
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImageIndex',
      NoIndex, -1);
    AddProperty('ShortCut', 'property: TShortCut',
      'FShortCut', 'SetShortCut',
      NoIndex, 0);
    AddProperty('SecondaryShortCuts', 'property: TShortCutList',
      'GetSecondaryShortCuts', 'SetSecondaryShortCuts',
      NoIndex, NoDefaultValue, 'IsSecondaryShortCutsStored');
    AddProperty('Visible', 'property: Boolean',
      'FVisible', 'SetVisible',
      NoIndex, Integer(True));
    AddProperty('OnHint', 'property: THintEvent',
      'FOnHint', 'FOnHint');

    Complete;
  end;
end;

{----------------}
{ TAction import }
{----------------}

class function TSepiImportsTAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TAction));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTAction.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('AutoCheck');
    RedefineProperty('Caption');
    RedefineProperty('Checked');
    RedefineProperty('Enabled');
    RedefineProperty('GroupIndex');
    RedefineProperty('HelpContext');
    RedefineProperty('HelpKeyword');
    RedefineProperty('HelpType');
    RedefineProperty('Hint');
    RedefineProperty('ImageIndex');
    RedefineProperty('ShortCut');
    RedefineProperty('SecondaryShortCuts');
    RedefineProperty('Visible');
    RedefineProperty('OnExecute');
    RedefineProperty('OnHint');
    RedefineProperty('OnUpdate');

    Complete;
  end;
end;

{--------------------}
{ TActionLink import }
{--------------------}

class function TSepiImportsTActionLink.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TActionLink));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('IsCaptionLinked', @TSepiImportsTActionLink.IsCaptionLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsCheckedLinked', @TSepiImportsTActionLink.IsCheckedLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsEnabledLinked', @TSepiImportsTActionLink.IsEnabledLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsGroupIndexLinked', @TSepiImportsTActionLink.IsGroupIndexLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsHelpContextLinked', @TSepiImportsTActionLink.IsHelpContextLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsHelpLinked', @TSepiImportsTActionLink.IsHelpLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsHintLinked', @TSepiImportsTActionLink.IsHintLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsImageIndexLinked', @TSepiImportsTActionLink.IsImageIndexLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsShortCutLinked', @TSepiImportsTActionLink.IsShortCutLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('IsVisibleLinked', @TSepiImportsTActionLink.IsVisibleLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('SetAutoCheck', @TSepiImportsTActionLink.SetAutoCheck,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('SetCaption', @TSepiImportsTActionLink.SetCaption,
      'procedure(const Value: string)',
      mlkVirtual);
    AddMethod('SetChecked', @TSepiImportsTActionLink.SetChecked,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('SetEnabled', @TSepiImportsTActionLink.SetEnabled,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('SetGroupIndex', @TSepiImportsTActionLink.SetGroupIndex,
      'procedure(Value: Integer)',
      mlkVirtual);
    AddMethod('SetHelpContext', @TSepiImportsTActionLink.SetHelpContext,
      'procedure(Value: THelpContext)',
      mlkVirtual);
    AddMethod('SetHelpKeyword', @TSepiImportsTActionLink.SetHelpKeyword,
      'procedure(const Value: string)',
      mlkVirtual);
    AddMethod('SetHelpType', @TSepiImportsTActionLink.SetHelpType,
      'procedure(Value: THelpType)',
      mlkVirtual);
    AddMethod('SetHint', @TSepiImportsTActionLink.SetHint,
      'procedure(const Value: string)',
      mlkVirtual);
    AddMethod('SetImageIndex', @TSepiImportsTActionLink.SetImageIndex,
      'procedure(Value: Integer)',
      mlkVirtual);
    AddMethod('SetShortCut', @TSepiImportsTActionLink.SetShortCut,
      'procedure(Value: TShortCut)',
      mlkVirtual);
    AddMethod('SetVisible', @TSepiImportsTActionLink.SetVisible,
      'procedure(Value: Boolean)',
      mlkVirtual);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ActnList',
    ['Classes', 'Messages', 'ImgList', 'Contnrs']);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomActionList));
  TSepiImportsTContainedAction.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TContainedActionClass', TypeInfo(TContainedAction), True);
  TSepiImportsTActionListEnumerator.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TActionEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TActionListState));
  TSepiImportsTCustomActionList.SepiImport(Result);
  TSepiImportsTActionList.SepiImport(Result);
  TSepiImportsTShortCutList.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THintEvent));
  TSepiImportsTCustomAction.SepiImport(Result);
  TSepiImportsTAction.SepiImport(Result);
  TSepiImportsTActionLink.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TActionLinkClass', TypeInfo(TActionLink), True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEnumActionProc));

  // Routines
  TSepiMetaMethod.Create(Result, 'RegisterActions', @RegisterActions,
    'procedure(const CategoryName: string; const AClasses: array of TBasicActionClass ; Resource: TComponentClass )');
  TSepiMetaMethod.Create(Result, 'UnRegisterActions', @UnRegisterActions,
    'procedure(const AClasses: array of TBasicActionClass)');
  TSepiMetaMethod.Create(Result, 'EnumRegisteredActions', @EnumRegisteredActions,
    'procedure(Proc: TEnumActionProc; Info: Pointer)');
  TSepiMetaMethod.Create(Result, 'CreateAction', @CreateAction,
    'function(AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction');

  // Constants
  TSepiMethodRefType.Create(Result, '$1',
    'procedure(const CategoryName: string; const AClasses: array of TBasicActionClass ; Resource: TComponentClass )');
  TSepiVariable.Create(Result, 'RegisterActionsProc',
    @RegisterActionsProc, '$1', True);
  TSepiMethodRefType.Create(Result, '$2',
    'procedure(const AClasses: array of TBasicActionClass)');
  TSepiVariable.Create(Result, 'UnRegisterActionsProc',
    @UnRegisterActionsProc, '$2', True);
  TSepiMethodRefType.Create(Result, '$3',
    'procedure(Proc: TEnumActionProc; Info: Pointer)');
  TSepiVariable.Create(Result, 'EnumRegisteredActionsProc',
    @EnumRegisteredActionsProc, '$3', True);
  TSepiMethodRefType.Create(Result, '$4',
    'function(AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction');
  TSepiVariable.Create(Result, 'CreateActionProc',
    @CreateActionProc, '$4', True);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ActnList', ImportUnit);
end.

