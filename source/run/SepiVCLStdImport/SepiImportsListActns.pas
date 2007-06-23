{*
  Importe l'unité ListActns dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsListActns;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ImgList, ListActns;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTListControlItem = class(TListControlItem)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTListControlItems = class(TListControlItems)
  private
    function GetListItem(const Index: Integer): TListControlItem;
    procedure SetSortType(const Value: TListItemsSortType);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomListAction = class(TCustomListAction)
  private
    procedure SetActive(const Value: Boolean);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetItemIndex(const Value: Integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomVirtualListAction = class(TCustomVirtualListAction)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTVirtualListAction = class(TVirtualListAction)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTStaticListItems = class(TStaticListItems)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomStaticListAction = class(TCustomStaticListAction)
  private
    procedure SetListitems(const Value: TStaticListItems);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTStaticListAction = class(TStaticListAction)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTListActionLink = class(TListActionLink)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{-------------------------}
{ TListControlItem import }
{-------------------------}

class function TSepiImportsTListControlItem.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TListControlItem));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FListControlItems', System.TypeInfo(TListControlItems));

    CurrentVisibility := mvProtected;

    AddField('FCaption', System.TypeInfo(String));
    AddField('FData', 'Pointer');
    AddField('FImageIndex', System.TypeInfo(TImageIndex));

    AddMethod('Changed', @TSepiImportsTListControlItem.Changed,
      'procedure');
    AddMethod('GetDisplayName', @TSepiImportsTListControlItem.GetDisplayName,
      'function: String',
      mlkOverride);
    AddMethod('SetCaption', @TSepiImportsTListControlItem.SetCaption,
      'procedure(const Value: String)',
      mlkVirtual);
    AddMethod('SetData', @TSepiImportsTListControlItem.SetData,
      'procedure(const Value: Pointer)',
      mlkVirtual);
    AddMethod('SetImageIndex', @TSepiImportsTListControlItem.SetImageIndex,
      'procedure(const Value: TImageIndex)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTListControlItem.Create,
      'constructor(Collection: TCollection)',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTListControlItem.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    AddProperty('Data', 'property: Pointer',
      'FData', 'SetData');

    CurrentVisibility := mvPublished;

    AddProperty('Caption', 'property: String',
      'FCaption', 'SetCaption');
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImageIndex',
      NoIndex, -1);

    Complete;
  end;
end;

{--------------------------}
{ TListControlItems import }
{--------------------------}

function TSepiImportsTListControlItems.GetListItem(const Index: Integer): TListControlItem;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTListControlItems.SetSortType(const Value: TListItemsSortType);
begin
  SortType := Value;
end;

class function TSepiImportsTListControlItems.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TListControlItems'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TListControlItems));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCaseSensitive', System.TypeInfo(Boolean));
    AddField('FSortType', System.TypeInfo(TListItemsSortType));
    AddField('FOnCompare', System.TypeInfo(TListCompareEvent));

    AddMethod('ExchangeItems', nil,
      'procedure(Index1, Index2: Integer)');
    AddMethod('GetListItem', @TSepiImportsTListControlItems.GetListItem,
      'function(const Index: Integer): TListControlItem');
    AddMethod('QuickSort', nil,
      'procedure(L, R: Integer; SCompare: TListItemsCompare)');
    AddMethod('SetSortType', @TSepiImportsTListControlItems.SetSortType,
      'procedure(const Value: TListItemsSortType)');

    CurrentVisibility := mvProtected;

    AddMethod('CompareItems', @TSepiImportsTListControlItems.CompareItems,
      'function(I1, I2: TListControlItem): Integer',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTListControlItems.Create,
      'constructor(AOwner: TPersistent; ItemClass: TCollectionItemClass)');
    AddMethod('Add', @TSepiImportsTListControlItems.Add,
      'function: TListControlItem');
    AddMethod('Sort', @TSepiImportsTListControlItems.Sort,
      'procedure');
    AddMethod('CustomSort', @TSepiImportsTListControlItems.CustomSort,
      'procedure(Compare: TListItemsCompare)');

    AddProperty('Items', 'property[const Index: Integer]: TListControlItem',
      'GetListItem', '', True);

    CurrentVisibility := mvPublished;

    AddProperty('CaseSensitive', 'property: Boolean',
      'FCaseSensitive', 'FCaseSensitive',
      NoIndex, Integer(False));
    AddProperty('SortType', 'property: TListItemsSortType',
      'FSortType', 'SetSortType',
      NoIndex, Integer(stNone));
    AddProperty('OnCompare', 'property: TListCompareEvent',
      'FOnCompare', 'FOnCompare');

    Complete;
  end;
end;

{--------------------------}
{ TCustomListAction import }
{--------------------------}

procedure TSepiImportsTCustomListAction.SetActive(const Value: Boolean);
begin
  Active := Value;
end;

procedure TSepiImportsTCustomListAction.SetImages(const Value: TCustomImageList);
begin
  Images := Value;
end;

procedure TSepiImportsTCustomListAction.SetItemIndex(const Value: Integer);
begin
  ItemIndex := Value;
end;

class function TSepiImportsTCustomListAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomListAction'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomListAction));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FActive', System.TypeInfo(Boolean));
    AddField('FActivated', System.TypeInfo(Boolean));
    AddField('FImages', System.TypeInfo(TCustomImageList));
    AddField('FInUpdate', System.TypeInfo(Boolean));
    AddField('FLoadedImages', System.TypeInfo(TCustomImageList));
    AddField('FLoading', System.TypeInfo(Boolean));
    AddField('FOnGetItemCount', System.TypeInfo(TGetItemCountEvent));
    AddField('FOnItemSelected', System.TypeInfo(TItemSelectedEvent));
    AddField('FItemIndex', System.TypeInfo(Integer));

    AddMethod('SetActive', @TSepiImportsTCustomListAction.SetActive,
      'procedure(const Value: Boolean)');
    AddMethod('SetImages', @TSepiImportsTCustomListAction.SetImages,
      'procedure(const Value: TCustomImageList)');
    AddMethod('SetItemIndex', @TSepiImportsTCustomListAction.SetItemIndex,
      'procedure(const Value: Integer)');

    CurrentVisibility := mvProtected;

    AddMethod('GetCount', @TSepiImportsTCustomListAction.GetCount,
      'function: Integer',
      mlkVirtual);
    AddMethod('GetString', @TSepiImportsTCustomListAction.GetString,
      'function(Index: Integer): String',
      mlkVirtual);
    AddMethod('Loaded', @TSepiImportsTCustomListAction.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('SetString', @TSepiImportsTCustomListAction.SetString,
      'procedure(Index: Integer; const Value: String)',
      mlkVirtual);

    AddProperty('Images', 'property: TCustomImageList',
      'FImages', 'SetImages');
    AddProperty('Loading', 'property: Boolean',
      'FLoading', '');
    AddProperty('OnGetItemCount', 'property: TGetItemCountEvent',
      'FOnGetItemCount', 'FOnGetItemCount');
    AddProperty('OnItemSelected', 'property: TItemSelectedEvent',
      'FOnItemSelected', 'FOnItemSelected');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomListAction.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('ExecuteTarget', @TSepiImportsTCustomListAction.ExecuteTarget,
      'procedure(Target: TObject)',
      mlkOverride);
    AddMethod('HandlesTarget', @TSepiImportsTCustomListAction.HandlesTarget,
      'function(Target: TObject): Boolean',
      mlkOverride);

    AddProperty('Active', 'property: Boolean',
      'FActive', 'SetActive',
      NoIndex, Integer(True));
    AddProperty('Count', 'property: Integer',
      'GetCount', '');
    AddProperty('ItemIndex', 'property: Integer',
      'FItemIndex', 'SetItemIndex');
    AddProperty('Strings', 'property[Index: Integer]: String',
      'GetString', 'SetString', True);

    Complete;
  end;
end;

{---------------------------------}
{ TCustomVirtualListAction import }
{---------------------------------}

class function TSepiImportsTCustomVirtualListAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomVirtualListAction));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOnGetItem', System.TypeInfo(TGetVirtualItemEvent));

    CurrentVisibility := mvProtected;

    AddMethod('GetItem', @TSepiImportsTCustomVirtualListAction.GetItem,
      'function(const Index: Integer; var Value: String; var ImageIndex: Integer ; var Data: Pointer ) : Boolean');
    AddMethod('GetString', @TSepiImportsTCustomVirtualListAction.GetString,
      'function(Index: Integer): String',
      mlkOverride);

    CurrentVisibility := mvPublic;

    RedefineProperty('Count');
    AddProperty('OnGetItem', 'property: TGetVirtualItemEvent',
      'FOnGetItem', 'FOnGetItem');

    Complete;
  end;
end;

{---------------------------}
{ TVirtualListAction import }
{---------------------------}

class function TSepiImportsTVirtualListAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TVirtualListAction));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Active');
    RedefineProperty('Caption');
    RedefineProperty('Enabled');
    RedefineProperty('HelpContext');
    RedefineProperty('Hint');
    RedefineProperty('Images');
    RedefineProperty('ItemIndex',
      '', '', -1);
    RedefineProperty('ShortCut');
    RedefineProperty('SecondaryShortCuts');
    RedefineProperty('Visible');
    RedefineProperty('OnGetItem');
    RedefineProperty('OnGetItemCount');
    RedefineProperty('OnItemSelected');
    RedefineProperty('OnHint');

    Complete;
  end;
end;

{-------------------------}
{ TStaticListItems import }
{-------------------------}

class function TSepiImportsTStaticListItems.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TStaticListItems));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStaticListAction', System.TypeInfo(TCustomStaticListAction));

    CurrentVisibility := mvProtected;

    AddMethod('Notify', @TSepiImportsTStaticListItems.Notify,
      'procedure(Item: TCollectionItem; Action: TCollectionNotification )',
      mlkOverride);
    AddMethod('Update', @TSepiImportsTStaticListItems.Update,
      'procedure(Item: TCollectionItem)',
      mlkOverride);

    Complete;
  end;
end;

{--------------------------------}
{ TCustomStaticListAction import }
{--------------------------------}

procedure TSepiImportsTCustomStaticListAction.SetListitems(const Value: TStaticListItems);
begin
  Items := Value;
end;

class function TSepiImportsTCustomStaticListAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomStaticListAction'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomStaticListAction));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FListItems', System.TypeInfo(TStaticListItems));
    AddField('FOnGetItem', System.TypeInfo(TGetItemEvent));

    AddMethod('SetListitems', @TSepiImportsTCustomStaticListAction.SetListitems,
      'procedure(const Value: TStaticListItems)');

    CurrentVisibility := mvProtected;

    AddMethod('GetItemClass', @TSepiImportsTCustomStaticListAction.GetItemClass,
      'function: TListControlItemClass',
      mlkVirtual);
    AddMethod('GetCount', @TSepiImportsTCustomStaticListAction.GetCount,
      'function: Integer',
      mlkOverride);
    AddMethod('GetItem', @TSepiImportsTCustomStaticListAction.GetItem,
      'function(const Index: Integer; AnItem: TListControlItem): Boolean');
    AddMethod('GetString', @TSepiImportsTCustomStaticListAction.GetString,
      'function(Index: Integer): String',
      mlkOverride);
    AddMethod('SetString', @TSepiImportsTCustomStaticListAction.SetString,
      'procedure(Index: Integer; const Value: String)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomStaticListAction.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomStaticListAction.Destroy,
      'destructor',
      mlkOverride);

    RedefineProperty('Count');

    CurrentVisibility := mvPublished;

    RedefineProperty('Active');
    RedefineProperty('Caption');
    RedefineProperty('Enabled');
    RedefineProperty('HelpContext');
    RedefineProperty('Hint');
    RedefineProperty('Images');
    RedefineProperty('ItemIndex',
      '', '', -1);
    AddProperty('Items', 'property: TStaticListItems',
      'FListItems', 'SetListitems');
    RedefineProperty('ShortCut');
    RedefineProperty('SecondaryShortCuts');
    RedefineProperty('Visible');
    AddProperty('OnGetItem', 'property: TGetItemEvent',
      'FOnGetItem', 'FOnGetItem');
    RedefineProperty('OnItemSelected');
    RedefineProperty('OnHint');
    RedefineProperty('OnUpdate');

    Complete;
  end;
end;

{--------------------------}
{ TStaticListAction import }
{--------------------------}

class function TSepiImportsTStaticListAction.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TStaticListAction));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Active');
    RedefineProperty('Caption');
    RedefineProperty('Enabled');
    RedefineProperty('HelpContext');
    RedefineProperty('Hint');
    RedefineProperty('Images');
    RedefineProperty('ItemIndex',
      '', '', -1);
    RedefineProperty('Items');
    RedefineProperty('ShortCut');
    RedefineProperty('SecondaryShortCuts');
    RedefineProperty('Visible');
    RedefineProperty('OnGetItem');
    RedefineProperty('OnItemSelected');
    RedefineProperty('OnHint');
    RedefineProperty('OnUpdate');

    Complete;
  end;
end;

{------------------------}
{ TListActionLink import }
{------------------------}

class function TSepiImportsTListActionLink.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TListActionLink));

  with Result do
  begin
    TSepiMetaOverloadedMethod.Create(Result, 'AddItem', 2);
    CurrentVisibility := mvProtected;

    AddMethod('IsActiveLinked', @TSepiImportsTListActionLink.IsActiveLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('SetActive', @TSepiImportsTListActionLink.SetActive,
      'procedure(const Value: Boolean)',
      mlkVirtual);
    AddMethod('IsImagesLinked', @TSepiImportsTListActionLink.IsImagesLinked,
      'function: Boolean',
      mlkVirtual);
    AddMethod('SetAction', @TSepiImportsTListActionLink.SetAction,
      'procedure(Value: TBasicAction)',
      mlkOverride);
    AddMethod('SetImages', @TSepiImportsTListActionLink.SetImages,
      'procedure(Value: TCustomImageList)',
      mlkVirtual);
    AddMethod('SetItemIndex', @TSepiImportsTListActionLink.SetItemIndex,
      'procedure(const Value: Integer)',
      mlkVirtual);
    AddMethod('OL$AddItem$0', nil,
      'procedure(AnItem: TListControlItem)',
      mlkVirtual);
    AddMethod('OL$AddItem$1', nil,
      'procedure(ACaption: String; AImageIndex: Integer; DataPtr: Pointer )',
      mlkVirtual);
    AddMethod('RefreshControl', @TSepiImportsTListActionLink.RefreshControl,
      'procedure');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ListActns',
    ['Classes', 'Controls', 'ActnList', 'ImgList']);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TListControlItems));
  TSepiImportsTListControlItem.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TListItemsSortType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TListCompareEvent));
  TSepiMethodRefType.Create(Result, 'TListItemsCompare',
    'function(List: TListControlItems; Index1, Index2: Integer ) : Integer');
  TSepiImportsTListControlItems.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomListAction));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetItemCountEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TItemSelectedEvent));
  TSepiImportsTCustomListAction.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetVirtualItemEvent));
  TSepiImportsTCustomVirtualListAction.SepiImport(Result);
  TSepiImportsTVirtualListAction.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomStaticListAction));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TGetItemEvent));
  TSepiImportsTStaticListItems.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TListControlItemClass', TypeInfo(TListControlItem), True);
  TSepiImportsTCustomStaticListAction.SepiImport(Result);
  TSepiImportsTStaticListAction.SepiImport(Result);
  TSepiImportsTListActionLink.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ListActns', ImportUnit);
end.

