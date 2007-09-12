{*
  Importe l'unité ComCtrls dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsComCtrls;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, Messages, SysUtils, Classes, Forms, Graphics,
  Controls, Menus, ImgList, ListActns, CommCtrl, ExtCtrls, ShlObj, ToolWin,
  ComCtrls;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTCustomTabControl = class(TCustomTabControl)
  private
    function GetDisplayRect: TRect;
    function GetTabIndex: Integer;
    procedure SetHotTrack(Value: Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetMultiLine(Value: Boolean);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetRaggedRight(Value: Boolean);
    procedure SetScrollOpposite(Value: Boolean);
    procedure SetStyle(Value: TTabStyle);
    procedure SetTabHeight(Value: Smallint);
    procedure SetTabPosition(Value: TTabPosition);
    procedure SetTabs(Value: TStrings);
    procedure SetTabWidth(Value: Smallint);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTTabControl = class(TTabControl)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTTabSheet = class(TTabSheet)
  private
    function GetPageIndex: Integer;
    function GetTabIndex: Integer;
    procedure SetHighlighted(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetPageControl(APageControl: TPageControl);
    procedure SetPageIndex(Value: Integer);
    procedure SetTabVisible(Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTPageControl = class(TPageControl)
  private
    function GetActivePageIndex: Integer;
    function GetPage(Index: Integer): TTabSheet;
    function GetPageCount: Integer;
    procedure SetActivePageIndex(const Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTStatusPanel = class(TStatusPanel)
  private
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevel(Value: TStatusPanelBevel);
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure SetStyle(Value: TStatusPanelStyle);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTStatusPanels = class(TStatusPanels)
  private
    function GetItem(Index: Integer): TStatusPanel;
    procedure SetItem(Index: Integer; Value: TStatusPanel);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomStatusBar = class(TCustomStatusBar)
  private
    procedure SetPanels(Value: TStatusPanels);
    procedure SetSimplePanel(Value: Boolean);
    procedure SetSimpleText(const Value: string);
    procedure SetSizeGrip(Value: Boolean);
    procedure SetUseSystemFont(const Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTStatusBar = class(TStatusBar)
  private
    function GetOnDrawPanel: TDrawPanelEvent;
    procedure SetOnDrawPanel(const Value: TDrawPanelEvent);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTHeaderSection = class(THeaderSection)
  private
    function GetLeft: Integer;
    function GetRight: Integer;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSize(Value: Boolean);
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure SetStyle(Value: THeaderSectionStyle);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetImageIndex(const Value: TImageIndex);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTHeaderSections = class(THeaderSections)
  private
    function GetItem(Index: Integer): THeaderSection;
    procedure SetItem(Index: Integer; Value: THeaderSection);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomHeaderControl = class(TCustomHeaderControl)
  private
    procedure SetDragReorder(const Value: Boolean);
    procedure SetFullDrag(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetSections(Value: THeaderSections);
    procedure SetStyle(Value: THeaderStyle);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTHeaderControl = class(THeaderControl)
  private
    function GetOnDrawSection: TDrawSectionEvent;
    function GetOnSectionClick: TSectionNotifyEvent;
    function GetOnSectionResize: TSectionNotifyEvent;
    function GetOnSectionTrack: TSectionTrackEvent;
    procedure SetOnDrawSection(const Value: TDrawSectionEvent);
    procedure SetOnSectionClick(const Value: TSectionNotifyEvent);
    procedure SetOnSectionResize(const Value: TSectionNotifyEvent);
    procedure SetOnSectionTrack(const Value: TSectionTrackEvent);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TUnnamed_1 = string[255];

  TSepiImportsTTreeNode = class(TTreeNode)
  private
    function GetAbsoluteIndex: Integer;
    function GetExpanded: Boolean;
    function GetLevel: Integer;
    function GetParent: TTreeNode;
    function GetChildren: Boolean;
    function GetCut: Boolean;
    function GetDropTarget: Boolean;
    function GetFocused: Boolean;
    function GetIndex: Integer;
    function GetItem(Index: Integer): TTreeNode;
    function GetSelected: Boolean;
    function GetCount: Integer;
    function GetTreeView: TCustomTreeView;
    function IsNodeVisible: Boolean;
    procedure SetChildren(Value: Boolean);
    procedure SetCut(Value: Boolean);
    procedure SetData(Value: Pointer);
    procedure SetDropTarget(Value: Boolean);
    procedure SetItem(Index: Integer; Value: TTreeNode);
    procedure SetExpanded(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetOverlayIndex(Value: Integer);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetSelected(Value: Boolean);
    procedure SetStateIndex(Value: Integer);
    procedure SetText(const S: string);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTTreeNodesEnumerator = class(TTreeNodesEnumerator)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTTreeNodes = class(TTreeNodes)
  private
    function GetHandle: HWND;
    function GetNodeFromIndex(Index: Integer): TTreeNode;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsETreeViewError = class(ETreeViewError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomTreeView = class(TCustomTreeView)
  private
    function GetChangeDelay: Integer;
    function GetDropTarget: TTreeNode;
    function GetIndent: Integer;
    function GetSelected: TTreeNode;
    function GetSelectionCount: Cardinal;
    function GetSelection(Index: Integer): TTreeNode;
    function GetTopItem: TTreeNode;
    procedure SetAutoExpand(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetButtonStyle(Value: Boolean);
    procedure SetChangeDelay(Value: Integer);
    procedure SetDropTarget(Value: TTreeNode);
    procedure SetHideSelection(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetIndent(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLineStyle(Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetMultiSelectStyle(const Value: TMultiSelectStyle);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRootStyle(Value: Boolean);
    procedure SetRowSelect(Value: Boolean);
    procedure SetSelected(Value: TTreeNode);
    procedure SetSortType(Value: TSortType);
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetToolTips(Value: Boolean);
    procedure SetTreeNodes(Value: TTreeNodes);
    procedure SetTopItem(Value: TTreeNode);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTTreeView = class(TTreeView)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTTrackBar = class(TTrackBar)
  private
    function GetThumbLength: Integer;
    procedure SetOrientation(Value: TTrackBarOrientation);
    procedure SetPosition(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetFrequency(Value: Integer);
    procedure SetTickStyle(Value: TTickStyle);
    procedure SetTickMarks(Value: TTickMark);
    procedure SetLineSize(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetThumbLength(Value: Integer);
    procedure SetSliderVisible(Value: Boolean);
    procedure SetSelStart(Value: Integer);
    procedure SetSelEnd(Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTProgressBar = class(TProgressBar)
  private
    function GetMin: Integer;
    function GetMax: Integer;
    function GetPosition: Integer;
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetStep(Value: Integer);
    procedure SetOrientation(Value: TProgressBarOrientation);
    procedure SetSmooth(Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTTextAttributes = class(TTextAttributes)
  private
    function GetCharset: TFontCharset;
    function GetColor: TColor;
    function GetConsistentAttributes: TConsistentAttributes;
    function GetHeight: Integer;
    function GetName: TFontName;
    function GetPitch: TFontPitch;
    function GetProtected: Boolean;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    procedure SetCharset(Value: TFontCharset);
    procedure SetColor(Value: TColor);
    procedure SetHeight(Value: Integer);
    procedure SetName(Value: TFontName);
    procedure SetPitch(Value: TFontPitch);
    procedure SetProtected(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TFontStyles);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTParaAttributes = class(TParaAttributes)
  private
    function GetAlignment: TAlignment;
    function GetFirstIndent: Longint;
    function GetLeftIndent: Longint;
    function GetRightIndent: Longint;
    function GetNumbering: TNumberingStyle;
    function GetTab(Index: Byte): Longint;
    function GetTabCount: Integer;
    procedure SetAlignment(Value: TAlignment);
    procedure SetFirstIndent(Value: Longint);
    procedure SetLeftIndent(Value: Longint);
    procedure SetRightIndent(Value: Longint);
    procedure SetNumbering(Value: TNumberingStyle);
    procedure SetTab(Index: Byte; Value: Longint);
    procedure SetTabCount(Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTConversion = class(TConversion)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomRichEdit = class(TCustomRichEdit)
  private
    function GetPlainText: Boolean;
    procedure SetHideScrollBars(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetPlainText(Value: Boolean);
    procedure SetRichEditStrings(Value: TStrings);
    procedure SetDefAttributes(Value: TTextAttributes);
    procedure SetSelAttributes(Value: TTextAttributes);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTRichEdit = class(TRichEdit)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomUpDown = class(TCustomUpDown)
  private
    procedure SetAssociate(Value: TWinControl);
    function GetPosition: Smallint;
    procedure SetMin(Value: Smallint);
    procedure SetMax(Value: Smallint);
    procedure SetIncrement(Value: Integer);
    procedure SetPosition(Value: Smallint);
    procedure SetAlignButton(Value: TUDAlignButton);
    procedure SetOrientation(Value: TUDOrientation);
    procedure SetArrowKeys(Value: Boolean);
    procedure SetThousands(Value: Boolean);
    procedure SetWrap(Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTUpDown = class(TUpDown)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomHotKey = class(TCustomHotKey)
  private
    procedure SetInvalidKeys(Value: THKInvalidKeys);
    procedure SetModifiers(Value: THKModifiers);
    function GetHotKey: TShortCut;
    procedure SetHotKey(Value: TShortCut);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTHotKey = class(THotKey)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTListColumn = class(TListColumn)
  private
    function GetWidth: TWidth;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSize(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetMaxWidth(Value: TWidth);
    procedure SetMinWidth(Value: TWidth);
    procedure SetWidth(Value: TWidth);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTListColumns = class(TListColumns)
  private
    function GetItem(Index: Integer): TListColumn;
    procedure SetItem(Index: Integer; Value: TListColumn);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTListItem = class(TListItem)
  private
    function GetChecked: Boolean;
    function GetHandle: HWND;
    function GetIndex: Integer;
    function GetListView: TCustomListView;
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetChecked(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetData(Value: Pointer);
    procedure SetIndent(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetSubItems(Value: TStrings);
    procedure SetTop(Value: Integer);
    function GetSubItemImage(Index: Integer): Integer;
    procedure SetSubItemImage(Index: Integer; const Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTListItemsEnumerator = class(TListItemsEnumerator)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTListItems = class(TListItems)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTWorkArea = class(TWorkArea)
  private
    procedure SetRect(const Value: TRect);
    procedure SetColor(const Value: TColor);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTWorkAreas = class(TWorkAreas)
  private
    function GetItem(Index: Integer): TWorkArea;
    procedure SetItem(Index: Integer; const Value: TWorkArea);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTIconOptions = class(TIconOptions)
  private
    procedure SetArrangement(Value: TIconArrangement);
    procedure SetAutoArrange(Value: Boolean);
    procedure SetWrapText(Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomListView = class(TCustomListView)
  private
    function GetBoundingRect: TRect;
    function GetColumnFromIndex(Index: Integer): TListColumn;
    function GetDropTarget: TListItem;
    function GetFocused: TListItem;
    function GetSelected: TListItem;
    function GetTopItem: TListItem;
    function GetViewOrigin: TPoint;
    function GetVisibleRowCount: Integer;
    function GetHoverTime: Integer;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColumnClick(Value: Boolean);
    procedure SetColumnHeaders(Value: Boolean);
    procedure SetDropTarget(Value: TListItem);
    procedure SetFocused(Value: TListItem);
    procedure SetHideSelection(Value: Boolean);
    procedure SetIconOptions(Value: TIconOptions);
    procedure SetLargeImages(Value: TCustomImageList);
    procedure SetAllocBy(Value: Integer);
    procedure SetItems(Value: TListItems);
    procedure SetListColumns(Value: TListColumns);
    procedure SetOwnerData(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetShowWorkAreas(const Value: Boolean);
    procedure SetSmallImages(Value: TCustomImageList);
    procedure SetSortType(Value: TSortType);
    procedure SetSelected(Value: TListItem);
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetCheckboxes(Value: Boolean);
    procedure SetFlatScrollBars(Value: Boolean);
    procedure SetFullDrag(Value: Boolean);
    procedure SetGridLines(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackStyles(Value: TListHotTrackStyles);
    procedure SetRowSelect(Value: Boolean);
    procedure SetHoverTime(Value: Integer);
    function GetItemIndex_0(Value: TListItem): Integer;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTListView = class(TListView)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTListViewActionLink = class(TListViewActionLink)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTAnimate = class(TAnimate)
  private
    procedure SetActive(Value: Boolean);
    procedure SetFileName(Value: string);
    procedure SetCenter(Value: Boolean);
    procedure SetCommonAVI(Value: TCommonAVI);
    procedure SetOpen(Value: Boolean);
    procedure SetRepetitions(Value: Integer);
    procedure SetResHandle(Value: THandle);
    procedure SetResId(Value: Integer);
    procedure SetResName(Value: string);
    procedure SetTimers(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetStartFrame(Value: Smallint);
    procedure SetStopFrame(Value: Smallint);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTToolButtonActionLink = class(TToolButtonActionLink)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTToolButton = class(TToolButton)
  private
    function GetIndex: Integer;
    procedure SetDown(Value: Boolean);
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetEnableDropdown(Value: Boolean);
    procedure SetGrouped(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetIndeterminate(Value: Boolean);
    procedure SetMarked(Value: Boolean);
    procedure SetMenuItem(Value: TMenuItem);
    procedure SetStyle(Value: TToolButtonStyle);
    procedure SetWrap(Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTToolBarEnumerator = class(TToolBarEnumerator)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTToolBar = class(TToolBar)
  private
    function GetButton(Index: Integer): TToolButton;
    function GetButtonCount: Integer;
    function GetRowCount: Integer;
    procedure SetList(Value: Boolean);
    procedure SetShowCaptions(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetWrapable(Value: Boolean);
    procedure SetButtonWidth(Value: Integer);
    procedure SetButtonHeight(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetDisabledImages(Value: TCustomImageList);
    procedure SetHotImages(Value: TCustomImageList);
    procedure SetIndent(Value: Integer);
    procedure SetMenu(const Value: TMainMenu);
    procedure SetCustomizable(const Value: Boolean);
    procedure SetHideClippedButtons(const Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTToolBarDockObject = class(TToolBarDockObject)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCoolBand = class(TCoolBand)
  private
    function GetHeight: Integer;
    function GetVisible: Boolean;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetBreak(Value: Boolean);
    procedure SetFixedSize(Value: Boolean);
    procedure SetMinHeight(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetHorizontalOnly(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetFixedBackground(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetControl(Value: TWinControl);
    procedure SetParentColor(Value: Boolean);
    procedure SetParentBitmap(Value: Boolean);
    procedure SetBitmap(Value: TBitmap);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCoolBands = class(TCoolBands)
  private
    function GetItem(Index: Integer): TCoolBand;
    procedure SetItem(Index: Integer; Value: TCoolBand);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCoolBar = class(TCoolBar)
  private
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign);
    procedure SetBands(Value: TCoolBands);
    procedure SetBandBorderStyle(Value: TBorderStyle);
    procedure SetBandMaximize(Value: TCoolBandMaximize);
    procedure SetBitmap(Value: TBitmap);
    procedure SetFixedSize(Value: Boolean);
    procedure SetFixedOrder(Value: Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetShowText(Value: Boolean);
    procedure SetVertical(Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsECommonCalendarError = class(ECommonCalendarError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTMonthCalColors = class(TMonthCalColors)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCommonCalendar = class(TCommonCalendar)
  private
    function GetDate: TDate;
    procedure SetCalColors(Value: TMonthCalColors);
    procedure SetDate(Value: TDate);
    procedure SetDateTime(Value: TDateTime);
    procedure SetEndDate(Value: TDate);
    procedure SetFirstDayOfWeek(Value: TCalDayOfWeek);
    procedure SetMaxDate(Value: TDate);
    procedure SetMaxSelectRange(Value: Integer);
    procedure SetMinDate(Value: TDate);
    procedure SetMonthDelta(Value: Integer);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetShowToday(Value: Boolean);
    procedure SetShowTodayCircle(Value: Boolean);
    procedure SetWeekNumbers(Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEMonthCalError = class(EMonthCalError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTMonthCalendar = class(TMonthCalendar)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEDateTimeError = class(EDateTimeError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTDateTimePicker = class(TDateTimePicker)
  private
    function GetTime: TTime;
    procedure SetCalAlignment(Value: TDTCalAlignment);
    procedure SetChecked(Value: Boolean);
    procedure SetDateMode(Value: TDTDateMode);
    procedure SetDateFormat(Value: TDTDateFormat);
    procedure SetKind(Value: TDateTimeKind);
    procedure SetParseInput(Value: Boolean);
    procedure SetShowCheckbox(Value: Boolean);
    procedure SetTime(Value: TTime);
    procedure SetFormat(const Value: String);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTPageScroller = class(TPageScroller)
  private
    procedure SetAutoScroll(Value: Boolean);
    procedure SetButtonSize(Value: Integer);
    procedure SetControl(Value: TWinControl);
    procedure SetDragScroll(Value: Boolean);
    procedure SetMargin(Value: Integer);
    procedure SetOrientation(Value: TPageScrollerOrientation);
    procedure SetPosition(Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTComboExItem = class(TComboExItem)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTComboExItems = class(TComboExItems)
  private
    function GetComboItem(const Index: Integer): TComboExItem;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTComboBoxExStrings = class(TComboBoxExStrings)
  private
    function GetSortType: TListItemsSortType;
    procedure SetItems(const Value: TComboExItems);
    procedure SetSortType(const Value: TListItemsSortType);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCustomComboBoxEx = class(TCustomComboBoxEx)
  private
    function GetSelText: String;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetSelText(const Value: String);
    procedure SetStyle(Value: TComboBoxExStyle);
    procedure SetItemsEx(const Value: TComboExItems);
    procedure SetStyleEx(const Value: TComboBoxExStyles);
    function GetDropDownCount: Integer;
    procedure SetAutoCompleteOptions(const Value: TAutoCompleteOptions);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTComboBoxEx = class(TComboBoxEx)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTComboBoxExActionLink = class(TComboBoxExActionLink)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{--------------------------}
{ TCustomTabControl import }
{--------------------------}

function TSepiImportsTCustomTabControl.GetDisplayRect: TRect;
begin
  Result := DisplayRect;
end;

function TSepiImportsTCustomTabControl.GetTabIndex: Integer;
begin
  Result := TabIndex;
end;

procedure TSepiImportsTCustomTabControl.SetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TSepiImportsTCustomTabControl.SetImages(Value: TCustomImageList);
begin
  Images := Value;
end;

procedure TSepiImportsTCustomTabControl.SetMultiLine(Value: Boolean);
begin
  MultiLine := Value;
end;

procedure TSepiImportsTCustomTabControl.SetMultiSelect(Value: Boolean);
begin
  MultiSelect := Value;
end;

procedure TSepiImportsTCustomTabControl.SetOwnerDraw(Value: Boolean);
begin
  OwnerDraw := Value;
end;

procedure TSepiImportsTCustomTabControl.SetRaggedRight(Value: Boolean);
begin
  RaggedRight := Value;
end;

procedure TSepiImportsTCustomTabControl.SetScrollOpposite(Value: Boolean);
begin
  ScrollOpposite := Value;
end;

procedure TSepiImportsTCustomTabControl.SetStyle(Value: TTabStyle);
begin
  Style := Value;
end;

procedure TSepiImportsTCustomTabControl.SetTabHeight(Value: Smallint);
begin
  TabHeight := Value;
end;

procedure TSepiImportsTCustomTabControl.SetTabPosition(Value: TTabPosition);
begin
  TabPosition := Value;
end;

procedure TSepiImportsTCustomTabControl.SetTabs(Value: TStrings);
begin
  Tabs := Value;
end;

procedure TSepiImportsTCustomTabControl.SetTabWidth(Value: Smallint);
begin
  TabWidth := Value;
end;

class function TSepiImportsTCustomTabControl.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomTabControl'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomTabControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FHotTrack', System.TypeInfo(Boolean));
    AddField('FImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FImages', System.TypeInfo(TCustomImageList));
    AddField('FMultiLine', System.TypeInfo(Boolean));
    AddField('FMultiSelect', System.TypeInfo(Boolean));
    AddField('FOwnerDraw', System.TypeInfo(Boolean));
    AddField('FRaggedRight', System.TypeInfo(Boolean));
    AddField('FSaveTabIndex', System.TypeInfo(Integer));
    AddField('FSaveTabs', System.TypeInfo(TStringList));
    AddField('FScrollOpposite', System.TypeInfo(Boolean));
    AddField('FStyle', System.TypeInfo(TTabStyle));
    AddField('FTabPosition', System.TypeInfo(TTabPosition));
    AddField('FTabs', System.TypeInfo(TStrings));
    AddField('FTabSize', 'TSmallPoint');
    AddField('FUpdating', System.TypeInfo(Boolean));
    AddField('FSavedAdjustRect', 'TRect');
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnChanging', System.TypeInfo(TTabChangingEvent));
    AddField('FOnDrawTab', System.TypeInfo(TDrawTabEvent));
    AddField('FOnGetImageIndex', System.TypeInfo(TTabGetImageEvent));

    AddMethod('GetDisplayRect', @TSepiImportsTCustomTabControl.GetDisplayRect,
      'function: TRect');
    AddMethod('GetTabIndex', @TSepiImportsTCustomTabControl.GetTabIndex,
      'function: Integer');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('InternalSetMultiLine', nil,
      'function(Value: Boolean): Boolean');
    AddMethod('SetHotTrack', @TSepiImportsTCustomTabControl.SetHotTrack,
      'procedure(Value: Boolean)');
    AddMethod('SetImages', @TSepiImportsTCustomTabControl.SetImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('SetMultiLine', @TSepiImportsTCustomTabControl.SetMultiLine,
      'procedure(Value: Boolean)');
    AddMethod('SetMultiSelect', @TSepiImportsTCustomTabControl.SetMultiSelect,
      'procedure(Value: Boolean)');
    AddMethod('SetOwnerDraw', @TSepiImportsTCustomTabControl.SetOwnerDraw,
      'procedure(Value: Boolean)');
    AddMethod('SetRaggedRight', @TSepiImportsTCustomTabControl.SetRaggedRight,
      'procedure(Value: Boolean)');
    AddMethod('SetScrollOpposite',
      @TSepiImportsTCustomTabControl.SetScrollOpposite,
      'procedure(Value: Boolean)');
    AddMethod('SetStyle', @TSepiImportsTCustomTabControl.SetStyle,
      'procedure(Value: TTabStyle)');
    AddMethod('SetTabHeight', @TSepiImportsTCustomTabControl.SetTabHeight,
      'procedure(Value: Smallint)');
    AddMethod('SetTabPosition', @TSepiImportsTCustomTabControl.SetTabPosition,
      'procedure(Value: TTabPosition)');
    AddMethod('SetTabs', @TSepiImportsTCustomTabControl.SetTabs,
      'procedure(Value: TStrings)');
    AddMethod('SetTabWidth', @TSepiImportsTCustomTabControl.SetTabWidth,
      'procedure(Value: Smallint)');
    AddMethod('TabsChanged', nil,
      'procedure');
    AddMethod('UpdateTabSize', nil,
      'procedure');
    AddMethod('CMFontChanged', nil,
      'procedure(var Message)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CMSysColorChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSCOLORCHANGE);
    AddMethod('CMTabStopChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TABSTOPCHANGED);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('CMDialogChar', nil,
      'procedure(var Message: TCMDialogChar)',
      mlkMessage, False, CM_DIALOGCHAR);
    AddMethod('CNDrawItem', nil,
      'procedure(var Message: TWMDrawItem)',
      mlkMessage, False, CN_DRAWITEM);
    AddMethod('TCMAdjustRect', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, TCM_ADJUSTRECT);
    AddMethod('WMDestroy', nil,
      'procedure(var Message: TWMDestroy)',
      mlkMessage, False, WM_DESTROY);
    AddMethod('WMNotifyFormat', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_NOTIFYFORMAT);
    AddMethod('WMSize', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_SIZE);

    CurrentVisibility := mvProtected;

    AddMethod('AdjustClientRect',
      @TSepiImportsTCustomTabControl.AdjustClientRect,
      'procedure(var Rect: TRect)',
      mlkOverride);
    AddMethod('CanChange', @TSepiImportsTCustomTabControl.CanChange,
      'function: Boolean',
      mlkDynamic);
    AddMethod('CanShowTab', @TSepiImportsTCustomTabControl.CanShowTab,
      'function(TabIndex: Integer): Boolean',
      mlkVirtual);
    AddMethod('Change', @TSepiImportsTCustomTabControl.Change,
      'procedure',
      mlkDynamic);
    AddMethod('CreateParams', @TSepiImportsTCustomTabControl.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomTabControl.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DrawTab', @TSepiImportsTCustomTabControl.DrawTab,
      'procedure(TabIndex: Integer; const Rect: TRect; Active: Boolean)',
      mlkVirtual);
    AddMethod('GetImageIndex', @TSepiImportsTCustomTabControl.GetImageIndex,
      'function(TabIndex: Integer): Integer',
      mlkVirtual);
    AddMethod('Loaded', @TSepiImportsTCustomTabControl.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('UpdateTabImages',
      @TSepiImportsTCustomTabControl.UpdateTabImages,
      'procedure');

    AddProperty('DisplayRect', 'property: TRect',
      'GetDisplayRect', '');
    AddProperty('HotTrack', 'property: Boolean',
      'FHotTrack', 'SetHotTrack',
      NoIndex, Integer(False));
    AddProperty('Images', 'property: TCustomImageList',
      'FImages', 'SetImages');
    AddProperty('MultiLine', 'property: Boolean',
      'FMultiLine', 'SetMultiLine',
      NoIndex, Integer(False));
    AddProperty('MultiSelect', 'property: Boolean',
      'FMultiSelect', 'SetMultiSelect',
      NoIndex, Integer(False));

    AddMethod('Notification', @TSepiImportsTCustomTabControl.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('SetTabIndex', @TSepiImportsTCustomTabControl.SetTabIndex,
      'procedure(Value: Integer)',
      mlkVirtual);

    AddProperty('OwnerDraw', 'property: Boolean',
      'FOwnerDraw', 'SetOwnerDraw',
      NoIndex, Integer(False));
    AddProperty('RaggedRight', 'property: Boolean',
      'FRaggedRight', 'SetRaggedRight',
      NoIndex, Integer(False));
    AddProperty('ScrollOpposite', 'property: Boolean',
      'FScrollOpposite', 'SetScrollOpposite',
      NoIndex, Integer(False));
    AddProperty('Style', 'property: TTabStyle',
      'FStyle', 'SetStyle',
      NoIndex, Integer(tsTabs));
    AddProperty('TabHeight', 'property: Smallint',
      'FTabSize.Y', 'SetTabHeight',
      NoIndex, 0);
    AddProperty('TabIndex', 'property: Integer',
      'GetTabIndex', 'SetTabIndex',
      NoIndex, -1);
    AddProperty('TabPosition', 'property: TTabPosition',
      'FTabPosition', 'SetTabPosition',
      NoIndex, Integer(tpTop));
    AddProperty('Tabs', 'property: TStrings',
      'FTabs', 'SetTabs');
    AddProperty('TabWidth', 'property: Smallint',
      'FTabSize.X', 'SetTabWidth',
      NoIndex, 0);
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OnChanging', 'property: TTabChangingEvent',
      'FOnChanging', 'FOnChanging');
    AddProperty('OnDrawTab', 'property: TDrawTabEvent',
      'FOnDrawTab', 'FOnDrawTab');
    AddProperty('OnGetImageIndex', 'property: TTabGetImageEvent',
      'FOnGetImageIndex', 'FOnGetImageIndex');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomTabControl.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomTabControl.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('IndexOfTabAt', @TSepiImportsTCustomTabControl.IndexOfTabAt,
      'function(X, Y: Integer): Integer');
    AddMethod('GetHitTestInfoAt',
      @TSepiImportsTCustomTabControl.GetHitTestInfoAt,
      'function(X, Y: Integer): THitTests');
    AddMethod('TabRect', @TSepiImportsTCustomTabControl.TabRect,
      'function(Index: Integer): TRect');
    AddMethod('RowCount', @TSepiImportsTCustomTabControl.RowCount,
      'function: Integer');
    AddMethod('ScrollTabs', @TSepiImportsTCustomTabControl.ScrollTabs,
      'procedure(Delta: Integer)');

    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');
    RedefineProperty('TabStop',
      '', '', Integer(True));

    Complete;
  end;
end;

{--------------------}
{ TTabControl import }
{--------------------}

class function TSepiImportsTTabControl.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTabControl));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    RedefineProperty('DisplayRect');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('Constraints');
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('HotTrack');
    RedefineProperty('Images');
    RedefineProperty('MultiLine');
    RedefineProperty('MultiSelect');
    RedefineProperty('OwnerDraw');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('RaggedRight');
    RedefineProperty('ScrollOpposite');
    RedefineProperty('ShowHint');
    RedefineProperty('Style');
    RedefineProperty('TabHeight');
    RedefineProperty('TabOrder');
    RedefineProperty('TabPosition');
    RedefineProperty('Tabs');
    RedefineProperty('TabIndex');
    RedefineProperty('TabStop');
    RedefineProperty('TabWidth');
    RedefineProperty('Visible');
    RedefineProperty('OnChange');
    RedefineProperty('OnChanging');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnDrawTab');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnGetImageIndex');
    RedefineProperty('OnGetSiteInfo');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnResize');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{------------------}
{ TTabSheet import }
{------------------}

function TSepiImportsTTabSheet.GetPageIndex: Integer;
begin
  Result := PageIndex;
end;

function TSepiImportsTTabSheet.GetTabIndex: Integer;
begin
  Result := TabIndex;
end;

procedure TSepiImportsTTabSheet.SetHighlighted(Value: Boolean);
begin
  Highlighted := Value;
end;

procedure TSepiImportsTTabSheet.SetImageIndex(Value: TImageIndex);
begin
  ImageIndex := Value;
end;

procedure TSepiImportsTTabSheet.SetPageControl(APageControl: TPageControl);
begin
  PageControl := APageControl;
end;

procedure TSepiImportsTTabSheet.SetPageIndex(Value: Integer);
begin
  PageIndex := Value;
end;

procedure TSepiImportsTTabSheet.SetTabVisible(Value: Boolean);
begin
  TabVisible := Value;
end;

class function TSepiImportsTTabSheet.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTabSheet));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FImageIndex', System.TypeInfo(TImageIndex));
    AddField('FPageControl', System.TypeInfo(TPageControl));
    AddField('FTabVisible', System.TypeInfo(Boolean));
    AddField('FTabShowing', System.TypeInfo(Boolean));
    AddField('FHighlighted', System.TypeInfo(Boolean));
    AddField('FOnHide', System.TypeInfo(TNotifyEvent));
    AddField('FOnShow', System.TypeInfo(TNotifyEvent));

    AddMethod('GetPageIndex', @TSepiImportsTTabSheet.GetPageIndex,
      'function: Integer');
    AddMethod('GetTabIndex', @TSepiImportsTTabSheet.GetTabIndex,
      'function: Integer');
    AddMethod('SetHighlighted', @TSepiImportsTTabSheet.SetHighlighted,
      'procedure(Value: Boolean)');
    AddMethod('SetImageIndex', @TSepiImportsTTabSheet.SetImageIndex,
      'procedure(Value: TImageIndex)');
    AddMethod('SetPageControl', @TSepiImportsTTabSheet.SetPageControl,
      'procedure(APageControl: TPageControl)');
    AddMethod('SetPageIndex', @TSepiImportsTTabSheet.SetPageIndex,
      'procedure(Value: Integer)');
    AddMethod('SetTabShowing', nil,
      'procedure(Value: Boolean)');
    AddMethod('SetTabVisible', @TSepiImportsTTabSheet.SetTabVisible,
      'procedure(Value: Boolean)');
    AddMethod('UpdateTabShowing', nil,
      'procedure');
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);
    AddMethod('CMShowingChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SHOWINGCHANGED);
    AddMethod('WMNCPaint', nil,
      'procedure(var Message: TWMNCPaint)',
      mlkMessage, False, WM_NCPAINT);
    AddMethod('WMPrintClient', nil,
      'procedure(var Message: TWMPrintClient)',
      mlkMessage, False, WM_PRINTCLIENT);

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTTabSheet.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('DoHide', @TSepiImportsTTabSheet.DoHide,
      'procedure',
      mlkDynamic);
    AddMethod('DoShow', @TSepiImportsTTabSheet.DoShow,
      'procedure',
      mlkDynamic);
    AddMethod('ReadState', @TSepiImportsTTabSheet.ReadState,
      'procedure(Reader: TReader)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTTabSheet.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTTabSheet.Destroy,
      'destructor',
      mlkOverride);

    AddProperty('PageControl', 'property: TPageControl',
      'FPageControl', 'SetPageControl');
    AddProperty('TabIndex', 'property: Integer',
      'GetTabIndex', '');

    CurrentVisibility := mvPublished;

    RedefineProperty('BorderWidth');
    RedefineProperty('Caption');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('Height',
      '', '', 'False');
    AddProperty('Highlighted', 'property: Boolean',
      'FHighlighted', 'SetHighlighted',
      NoIndex, Integer(False));
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImageIndex',
      NoIndex, 0);
    RedefineProperty('Left',
      '', '', 'False');
    RedefineProperty('Constraints');
    AddProperty('PageIndex', 'property: Integer',
      'GetPageIndex', 'SetPageIndex',
      NoIndex, NoDefaultValue, 'False');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    AddProperty('TabVisible', 'property: Boolean',
      'FTabVisible', 'SetTabVisible',
      NoIndex, Integer(True));
    RedefineProperty('Top',
      '', '', 'False');
    RedefineProperty('Visible',
      '', '', 'False');
    RedefineProperty('Width',
      '', '', 'False');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    AddProperty('OnHide', 'property: TNotifyEvent',
      'FOnHide', 'FOnHide');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnResize');
    AddProperty('OnShow', 'property: TNotifyEvent',
      'FOnShow', 'FOnShow');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{---------------------}
{ TPageControl import }
{---------------------}

function TSepiImportsTPageControl.GetActivePageIndex: Integer;
begin
  Result := ActivePageIndex;
end;

function TSepiImportsTPageControl.GetPage(Index: Integer): TTabSheet;
begin
  Result := Pages[Index];
end;

function TSepiImportsTPageControl.GetPageCount: Integer;
begin
  Result := PageCount;
end;

procedure TSepiImportsTPageControl.SetActivePageIndex(const Value: Integer);
begin
  ActivePageIndex := Value;
end;

class function TSepiImportsTPageControl.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TPageControl'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TPageControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPages', System.TypeInfo(TList));
    AddField('FActivePage', System.TypeInfo(TTabSheet));
    AddField('FNewDockSheet', System.TypeInfo(TTabSheet));
    AddField('FUndockingPage', System.TypeInfo(TTabSheet));
    AddField('FInSetActivePage', System.TypeInfo(Boolean));

    AddMethod('ChangeActivePage', nil,
      'procedure(Page: TTabSheet)');
    AddMethod('DeleteTab', nil,
      'procedure(Page: TTabSheet; Index: Integer)');
    AddMethod('GetActivePageIndex',
      @TSepiImportsTPageControl.GetActivePageIndex,
      'function: Integer');
    AddMethod('GetDockClientFromMousePos', nil,
      'function(MousePos: TPoint): TControl');
    AddMethod('GetPage', @TSepiImportsTPageControl.GetPage,
      'function(Index: Integer): TTabSheet');
    AddMethod('GetPageCount', @TSepiImportsTPageControl.GetPageCount,
      'function: Integer');
    AddMethod('InsertPage', nil,
      'procedure(Page: TTabSheet)');
    AddMethod('InsertTab', nil,
      'procedure(Page: TTabSheet)');
    AddMethod('MoveTab', nil,
      'procedure(CurIndex, NewIndex: Integer)');
    AddMethod('RemovePage', nil,
      'procedure(Page: TTabSheet)');
    AddMethod('SetActivePageIndex',
      @TSepiImportsTPageControl.SetActivePageIndex,
      'procedure(const Value: Integer)');
    AddMethod('UpdateTab', nil,
      'procedure(Page: TTabSheet)');
    AddMethod('UpdateTabHighlights', nil,
      'procedure');
    AddMethod('CMDesignHitTest', nil,
      'procedure(var Message: TCMDesignHitTest)',
      mlkMessage, False, CM_DESIGNHITTEST);
    AddMethod('CMDialogKey', nil,
      'procedure(var Message: TCMDialogKey)',
      mlkMessage, False, CM_DIALOGKEY);
    AddMethod('CMDockClient', nil,
      'procedure(var Message: TCMDockClient)',
      mlkMessage, False, CM_DOCKCLIENT);
    AddMethod('CMDockNotification', nil,
      'procedure(var Message: TCMDockNotification)',
      mlkMessage, False, CM_DOCKNOTIFICATION);
    AddMethod('CMUnDockClient', nil,
      'procedure(var Message: TCMUnDockClient)',
      mlkMessage, False, CM_UNDOCKCLIENT);
    AddMethod('WMLButtonDown', nil,
      'procedure(var Message: TWMLButtonDown)',
      mlkMessage, False, WM_LBUTTONDOWN);
    AddMethod('WMLButtonDblClk', nil,
      'procedure(var Message: TWMLButtonDblClk)',
      mlkMessage, False, WM_LBUTTONDBLCLK);
    AddMethod('WMEraseBkGnd', nil,
      'procedure(var Message: TWMEraseBkGnd)',
      mlkMessage, False, WM_ERASEBKGND);

    CurrentVisibility := mvProtected;

    AddMethod('CanShowTab', @TSepiImportsTPageControl.CanShowTab,
      'function(TabIndex: Integer): Boolean',
      mlkOverride);
    AddMethod('Change', @TSepiImportsTPageControl.Change,
      'procedure',
      mlkOverride);
    AddMethod('DoAddDockClient', @TSepiImportsTPageControl.DoAddDockClient,
      'procedure(Client: TControl; const ARect: TRect)',
      mlkOverride);
    AddMethod('DockOver', @TSepiImportsTPageControl.DockOver,
      'procedure(Source: TDragDockObject; X, Y: Integer; State: TDragState ; var Accept: Boolean )',
      mlkOverride);
    AddMethod('DoRemoveDockClient',
      @TSepiImportsTPageControl.DoRemoveDockClient,
      'procedure(Client: TControl)',
      mlkOverride);
    AddMethod('GetChildren', @TSepiImportsTPageControl.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('GetImageIndex', @TSepiImportsTPageControl.GetImageIndex,
      'function(TabIndex: Integer): Integer',
      mlkOverride);
    AddMethod('GetPageFromDockClient',
      @TSepiImportsTPageControl.GetPageFromDockClient,
      'function(Client: TControl): TTabSheet');
    AddMethod('GetSiteInfo', @TSepiImportsTPageControl.GetSiteInfo,
      'procedure(Client: TControl; var InfluenceRect: TRect; MousePos: TPoint ; var CanDock: Boolean )',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTPageControl.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('SetActivePage', @TSepiImportsTPageControl.SetActivePage,
      'procedure(Page: TTabSheet)');
    AddMethod('SetChildOrder', @TSepiImportsTPageControl.SetChildOrder,
      'procedure(Child: TComponent; Order: Integer)',
      mlkOverride);
    AddMethod('SetTabIndex', @TSepiImportsTPageControl.SetTabIndex,
      'procedure(Value: Integer)',
      mlkOverride);
    AddMethod('ShowControl', @TSepiImportsTPageControl.ShowControl,
      'procedure(AControl: TControl)',
      mlkOverride);
    AddMethod('UpdateActivePage', @TSepiImportsTPageControl.UpdateActivePage,
      'procedure',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPageControl.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTPageControl.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('FindNextPage', @TSepiImportsTPageControl.FindNextPage,
      'function(CurPage: TTabSheet; GoForward, CheckTabVisible: Boolean ) : TTabSheet');
    AddMethod('SelectNextPage', @TSepiImportsTPageControl.SelectNextPage,
      'procedure(GoForward: Boolean; CheckTabVisible: Boolean = True)');

    AddProperty('ActivePageIndex', 'property: Integer',
      'GetActivePageIndex', 'SetActivePageIndex');
    AddProperty('PageCount', 'property: Integer',
      'GetPageCount', '');
    AddProperty('Pages', 'property[Index: Integer]: TTabSheet',
      'GetPage', '');

    CurrentVisibility := mvPublished;

    AddProperty('ActivePage', 'property: TTabSheet',
      'FActivePage', 'SetActivePage');
    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('Constraints');
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('HotTrack');
    RedefineProperty('Images');
    RedefineProperty('MultiLine');
    RedefineProperty('OwnerDraw');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('RaggedRight');
    RedefineProperty('ScrollOpposite');
    RedefineProperty('ShowHint');
    RedefineProperty('Style');
    RedefineProperty('TabHeight');
    RedefineProperty('TabIndex',
      '', '', 'False');
    RedefineProperty('TabOrder');
    RedefineProperty('TabPosition');
    RedefineProperty('TabStop');
    RedefineProperty('TabWidth');
    RedefineProperty('Visible');
    RedefineProperty('OnChange');
    RedefineProperty('OnChanging');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnDrawTab');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnGetImageIndex');
    RedefineProperty('OnGetSiteInfo');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnResize');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{---------------------}
{ TStatusPanel import }
{---------------------}

procedure TSepiImportsTStatusPanel.SetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TSepiImportsTStatusPanel.SetBevel(Value: TStatusPanelBevel);
begin
  Bevel := Value;
end;

procedure TSepiImportsTStatusPanel.SetBiDiMode(Value: TBiDiMode);
begin
  BiDiMode := Value;
end;

procedure TSepiImportsTStatusPanel.SetParentBiDiMode(Value: Boolean);
begin
  ParentBiDiMode := Value;
end;

procedure TSepiImportsTStatusPanel.SetStyle(Value: TStatusPanelStyle);
begin
  Style := Value;
end;

procedure TSepiImportsTStatusPanel.SetText(const Value: string);
begin
  Text := Value;
end;

procedure TSepiImportsTStatusPanel.SetWidth(Value: Integer);
begin
  Width := Value;
end;

class function TSepiImportsTStatusPanel.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TStatusPanel'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TStatusPanel));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FText', System.TypeInfo(string));
    AddField('FWidth', System.TypeInfo(Integer));
    AddField('FAlignment', System.TypeInfo(TAlignment));
    AddField('FBevel', System.TypeInfo(TStatusPanelBevel));
    AddField('FBiDiMode', System.TypeInfo(TBiDiMode));
    AddField('FParentBiDiMode', System.TypeInfo(Boolean));
    AddField('FStyle', System.TypeInfo(TStatusPanelStyle));
    AddField('FUpdateNeeded', System.TypeInfo(Boolean));

    AddMethod('SetAlignment', @TSepiImportsTStatusPanel.SetAlignment,
      'procedure(Value: TAlignment)');
    AddMethod('SetBevel', @TSepiImportsTStatusPanel.SetBevel,
      'procedure(Value: TStatusPanelBevel)');
    AddMethod('SetBiDiMode', @TSepiImportsTStatusPanel.SetBiDiMode,
      'procedure(Value: TBiDiMode)');
    AddMethod('SetParentBiDiMode', @TSepiImportsTStatusPanel.SetParentBiDiMode,
      'procedure(Value: Boolean)');
    AddMethod('SetStyle', @TSepiImportsTStatusPanel.SetStyle,
      'procedure(Value: TStatusPanelStyle)');
    AddMethod('SetText', @TSepiImportsTStatusPanel.SetText,
      'procedure(const Value: string)');
    AddMethod('SetWidth', @TSepiImportsTStatusPanel.SetWidth,
      'procedure(Value: Integer)');
    AddMethod('IsBiDiModeStored', nil,
      'function: Boolean');

    CurrentVisibility := mvProtected;

    AddMethod('GetDisplayName', @TSepiImportsTStatusPanel.GetDisplayName,
      'function: string',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTStatusPanel.Create,
      'constructor(Collection: TCollection)',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTStatusPanel.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('ParentBiDiModeChanged',
      @TSepiImportsTStatusPanel.ParentBiDiModeChanged,
      'procedure');
    AddMethod('UseRightToLeftAlignment',
      @TSepiImportsTStatusPanel.UseRightToLeftAlignment,
      'function: Boolean');
    AddMethod('UseRightToLeftReading',
      @TSepiImportsTStatusPanel.UseRightToLeftReading,
      'function: Boolean');

    CurrentVisibility := mvPublished;

    AddProperty('Alignment', 'property: TAlignment',
      'FAlignment', 'SetAlignment',
      NoIndex, Integer(taLeftJustify));
    AddProperty('Bevel', 'property: TStatusPanelBevel',
      'FBevel', 'SetBevel',
      NoIndex, Integer(pbLowered));
    AddProperty('BiDiMode', 'property: TBiDiMode',
      'FBiDiMode', 'SetBiDiMode',
      NoIndex, NoDefaultValue, 'IsBiDiModeStored');
    AddProperty('ParentBiDiMode', 'property: Boolean',
      'FParentBiDiMode', 'SetParentBiDiMode',
      NoIndex, Integer(True));
    AddProperty('Style', 'property: TStatusPanelStyle',
      'FStyle', 'SetStyle',
      NoIndex, Integer(psText));
    AddProperty('Text', 'property: string',
      'FText', 'SetText');
    AddProperty('Width', 'property: Integer',
      'FWidth', 'SetWidth');

    Complete;
  end;
end;

{----------------------}
{ TStatusPanels import }
{----------------------}

function TSepiImportsTStatusPanels.GetItem(Index: Integer): TStatusPanel;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTStatusPanels.SetItem(Index: Integer;
  Value: TStatusPanel);
begin
  Items[Index] := Value;
end;

class function TSepiImportsTStatusPanels.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TStatusPanels'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TStatusPanels));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStatusBar', System.TypeInfo(TCustomStatusBar));

    AddMethod('GetItem', @TSepiImportsTStatusPanels.GetItem,
      'function(Index: Integer): TStatusPanel');
    AddMethod('SetItem', @TSepiImportsTStatusPanels.SetItem,
      'procedure(Index: Integer; Value: TStatusPanel)');

    CurrentVisibility := mvProtected;

    AddMethod('GetOwner', @TSepiImportsTStatusPanels.GetOwner,
      'function: TPersistent',
      mlkOverride);
    AddMethod('Update', @TSepiImportsTStatusPanels.Update,
      'procedure(Item: TCollectionItem)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTStatusPanels.Create,
      'constructor(StatusBar: TCustomStatusBar)');
    AddMethod('Add', @TSepiImportsTStatusPanels.Add,
      'function: TStatusPanel');
    AddMethod('AddItem', @TSepiImportsTStatusPanels.AddItem,
      'function(Item: TStatusPanel; Index: Integer): TStatusPanel');
    AddMethod('Insert', @TSepiImportsTStatusPanels.Insert,
      'function(Index: Integer): TStatusPanel');

    AddProperty('Items', 'property[Index: Integer]: TStatusPanel',
      'GetItem', 'SetItem', True);

    Complete;
  end;
end;

{-------------------------}
{ TCustomStatusBar import }
{-------------------------}

procedure TSepiImportsTCustomStatusBar.SetPanels(Value: TStatusPanels);
begin
  Panels := Value;
end;

procedure TSepiImportsTCustomStatusBar.SetSimplePanel(Value: Boolean);
begin
  SimplePanel := Value;
end;

procedure TSepiImportsTCustomStatusBar.SetSimpleText(const Value: string);
begin
  SimpleText := Value;
end;

procedure TSepiImportsTCustomStatusBar.SetSizeGrip(Value: Boolean);
begin
  SizeGrip := Value;
end;

procedure TSepiImportsTCustomStatusBar.SetUseSystemFont(const Value: Boolean);
begin
  UseSystemFont := Value;
end;

class function TSepiImportsTCustomStatusBar.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomStatusBar'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomStatusBar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPanels', System.TypeInfo(TStatusPanels));
    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FSimpleText', System.TypeInfo(string));
    AddField('FSimplePanel', System.TypeInfo(Boolean));
    AddField('FSizeGrip', System.TypeInfo(Boolean));
    AddField('FSizeGripValid', System.TypeInfo(Boolean), True);
    AddField('FUseSystemFont', System.TypeInfo(Boolean));
    AddField('FAutoHint', System.TypeInfo(Boolean));
    AddField('FOnDrawPanel', System.TypeInfo(TCustomDrawPanelEvent));
    AddField('FOnHint', System.TypeInfo(TNotifyEvent));
    AddField('FOnCreatePanelClass', System.TypeInfo(TSBCreatePanelClassEvent));

    AddMethod('DoRightToLeftAlignment', nil,
      'procedure(var Str: string; AAlignment: TAlignment; ARTLAlignment: Boolean )');
    AddMethod('SetPanels', @TSepiImportsTCustomStatusBar.SetPanels,
      'procedure(Value: TStatusPanels)');
    AddMethod('SetSimplePanel', @TSepiImportsTCustomStatusBar.SetSimplePanel,
      'procedure(Value: Boolean)');
    AddMethod('UpdateSimpleText', nil,
      'procedure');
    AddMethod('SetSimpleText', @TSepiImportsTCustomStatusBar.SetSimpleText,
      'procedure(const Value: string)');
    AddMethod('SetSizeGrip', @TSepiImportsTCustomStatusBar.SetSizeGrip,
      'procedure(Value: Boolean)');
    AddMethod('SyncToSystemFont', nil,
      'procedure');
    AddMethod('UpdatePanel', nil,
      'procedure(Index: Integer; Repaint: Boolean)');
    AddMethod('UpdatePanels', nil,
      'procedure(UpdateRects, UpdateText: Boolean)');
    AddMethod('CMBiDiModeChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BIDIMODECHANGED);
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMParentFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTFONTCHANGED);
    AddMethod('CMSysColorChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSCOLORCHANGE);
    AddMethod('CMWinIniChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_WININICHANGE);
    AddMethod('CMSysFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSFONTCHANGED);
    AddMethod('CNDrawItem', nil,
      'procedure(var Message: TWMDrawItem)',
      mlkMessage, False, CN_DRAWITEM);
    AddMethod('WMEraseBkGnd', nil,
      'procedure(var Message: TWMEraseBkGnd)',
      mlkMessage, False, WM_ERASEBKGND);
    AddMethod('WMGetTextLength', nil,
      'procedure(var Message: TWMGetTextLength)',
      mlkMessage, False, WM_GETTEXTLENGTH);
    AddMethod('WMPaint', nil,
      'procedure(var Message: TWMPaint)',
      mlkMessage, False, WM_PAINT);
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('SetUseSystemFont',
      @TSepiImportsTCustomStatusBar.SetUseSystemFont,
      'procedure(const Value: Boolean)');
    AddMethod('ValidateSizeGrip', nil,
      'procedure(ARecreate: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('ChangeScale', @TSepiImportsTCustomStatusBar.ChangeScale,
      'procedure(M, D: Integer)',
      mlkOverride);
    AddMethod('CreatePanel', @TSepiImportsTCustomStatusBar.CreatePanel,
      'function: TStatusPanel',
      mlkVirtual);
    AddMethod('CreatePanels', @TSepiImportsTCustomStatusBar.CreatePanels,
      'function: TStatusPanels',
      mlkVirtual);
    AddMethod('CreateParams', @TSepiImportsTCustomStatusBar.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomStatusBar.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DoHint', @TSepiImportsTCustomStatusBar.DoHint,
      'function: Boolean',
      mlkVirtual);
    AddMethod('DrawPanel', @TSepiImportsTCustomStatusBar.DrawPanel,
      'procedure(Panel: TStatusPanel; const Rect: TRect)',
      mlkDynamic);
    AddMethod('GetPanelClass', @TSepiImportsTCustomStatusBar.GetPanelClass,
      'function: TStatusPanelClass',
      mlkVirtual);
    AddMethod('IsFontStored', @TSepiImportsTCustomStatusBar.IsFontStored,
      'function: Boolean');
    AddMethod('SetParent', @TSepiImportsTCustomStatusBar.SetParent,
      'procedure(AParent: TWinControl)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomStatusBar.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomStatusBar.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('ExecuteAction', @TSepiImportsTCustomStatusBar.ExecuteAction,
      'function(Action: TBasicAction): Boolean',
      mlkOverride);
    AddMethod('FlipChildren', @TSepiImportsTCustomStatusBar.FlipChildren,
      'procedure(AllLevels: Boolean)',
      mlkOverride);
    AddMethod('SetBounds', @TSepiImportsTCustomStatusBar.SetBounds,
      'procedure(ALeft, ATop, AWidth, AHeight: Integer)',
      mlkOverride);

    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');
    AddProperty('AutoHint', 'property: Boolean',
      'FAutoHint', 'FAutoHint');
    AddProperty('Panels', 'property: TStatusPanels',
      'FPanels', 'SetPanels');
    AddProperty('SimplePanel', 'property: Boolean',
      'FSimplePanel', 'SetSimplePanel');
    AddProperty('SimpleText', 'property: string',
      'FSimpleText', 'SetSimpleText');
    AddProperty('SizeGrip', 'property: Boolean',
      'FSizeGrip', 'SetSizeGrip');
    AddProperty('UseSystemFont', 'property: Boolean',
      'FUseSystemFont', 'SetUseSystemFont');
    AddProperty('OnCreatePanelClass', 'property: TSBCreatePanelClassEvent',
      'FOnCreatePanelClass', 'FOnCreatePanelClass');
    AddProperty('OnHint', 'property: TNotifyEvent',
      'FOnHint', 'FOnHint');
    AddProperty('OnDrawPanel', 'property: TCustomDrawPanelEvent',
      'FOnDrawPanel', 'FOnDrawPanel');

    Complete;
  end;
end;

{-------------------}
{ TStatusBar import }
{-------------------}

function TSepiImportsTStatusBar.GetOnDrawPanel: TDrawPanelEvent;
begin
  Result := OnDrawPanel;
end;

procedure TSepiImportsTStatusBar.SetOnDrawPanel(const Value: TDrawPanelEvent);
begin
  OnDrawPanel := Value;
end;

class function TSepiImportsTStatusBar.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TStatusBar'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TStatusBar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetOnDrawPanel', @TSepiImportsTStatusBar.GetOnDrawPanel,
      'function: TDrawPanelEvent');
    AddMethod('SetOnDrawPanel', @TSepiImportsTStatusBar.SetOnDrawPanel,
      'procedure(const Value: TDrawPanelEvent)');

    CurrentVisibility := mvPublished;

    RedefineProperty('Action');
    RedefineProperty('AutoHint',
      '', '', Integer(False));
    RedefineProperty('Align',
      '', '', Integer(alBottom));
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderWidth');
    RedefineProperty('Color',
      '', '', Integer(clBtnFace));
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font',
      '', '', 'IsFontStored');
    RedefineProperty('Constraints');
    RedefineProperty('Panels');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor',
      '', '', Integer(False));
    RedefineProperty('ParentFont',
      '', '', Integer(False));
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('SimplePanel',
      '', '', Integer(False));
    RedefineProperty('SimpleText');
    RedefineProperty('SizeGrip',
      '', '', Integer(True));
    RedefineProperty('UseSystemFont',
      '', '', Integer(True));
    RedefineProperty('Visible');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnCreatePanelClass');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnHint');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    AddProperty('OnDrawPanel', 'property: TDrawPanelEvent',
      'GetOnDrawPanel', 'SetOnDrawPanel');
    RedefineProperty('OnResize');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{-----------------------}
{ THeaderSection import }
{-----------------------}

function TSepiImportsTHeaderSection.GetLeft: Integer;
begin
  Result := Left;
end;

function TSepiImportsTHeaderSection.GetRight: Integer;
begin
  Result := Right;
end;

procedure TSepiImportsTHeaderSection.SetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TSepiImportsTHeaderSection.SetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TSepiImportsTHeaderSection.SetBiDiMode(Value: TBiDiMode);
begin
  BiDiMode := Value;
end;

procedure TSepiImportsTHeaderSection.SetMaxWidth(Value: Integer);
begin
  MaxWidth := Value;
end;

procedure TSepiImportsTHeaderSection.SetMinWidth(Value: Integer);
begin
  MinWidth := Value;
end;

procedure TSepiImportsTHeaderSection.SetParentBiDiMode(Value: Boolean);
begin
  ParentBiDiMode := Value;
end;

procedure TSepiImportsTHeaderSection.SetStyle(Value: THeaderSectionStyle);
begin
  Style := Value;
end;

procedure TSepiImportsTHeaderSection.SetText(const Value: string);
begin
  Text := Value;
end;

procedure TSepiImportsTHeaderSection.SetWidth(Value: Integer);
begin
  Width := Value;
end;

procedure TSepiImportsTHeaderSection.SetImageIndex(const Value: TImageIndex);
begin
  ImageIndex := Value;
end;

class function TSepiImportsTHeaderSection.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('THeaderSection'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(THeaderSection));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FText', System.TypeInfo(string));
    AddField('FWidth', System.TypeInfo(Integer));
    AddField('FMinWidth', System.TypeInfo(Integer));
    AddField('FMaxWidth', System.TypeInfo(Integer));
    AddField('FAlignment', System.TypeInfo(TAlignment));
    AddField('FStyle', System.TypeInfo(THeaderSectionStyle));
    AddField('FAllowClick', System.TypeInfo(Boolean));
    AddField('FAutoSize', System.TypeInfo(Boolean));
    AddField('FImageIndex', System.TypeInfo(TImageIndex));
    AddField('FBiDiMode', System.TypeInfo(TBiDiMode));
    AddField('FParentBiDiMode', System.TypeInfo(Boolean));

    AddMethod('GetLeft', @TSepiImportsTHeaderSection.GetLeft,
      'function: Integer');
    AddMethod('GetRight', @TSepiImportsTHeaderSection.GetRight,
      'function: Integer');
    AddMethod('IsBiDiModeStored', nil,
      'function: Boolean');
    AddMethod('SetAlignment', @TSepiImportsTHeaderSection.SetAlignment,
      'procedure(Value: TAlignment)');
    AddMethod('SetAutoSize', @TSepiImportsTHeaderSection.SetAutoSize,
      'procedure(Value: Boolean)');
    AddMethod('SetBiDiMode', @TSepiImportsTHeaderSection.SetBiDiMode,
      'procedure(Value: TBiDiMode)');
    AddMethod('SetMaxWidth', @TSepiImportsTHeaderSection.SetMaxWidth,
      'procedure(Value: Integer)');
    AddMethod('SetMinWidth', @TSepiImportsTHeaderSection.SetMinWidth,
      'procedure(Value: Integer)');
    AddMethod('SetParentBiDiMode',
      @TSepiImportsTHeaderSection.SetParentBiDiMode,
      'procedure(Value: Boolean)');
    AddMethod('SetStyle', @TSepiImportsTHeaderSection.SetStyle,
      'procedure(Value: THeaderSectionStyle)');
    AddMethod('SetText', @TSepiImportsTHeaderSection.SetText,
      'procedure(const Value: string)');
    AddMethod('SetWidth', @TSepiImportsTHeaderSection.SetWidth,
      'procedure(Value: Integer)');
    AddMethod('SetImageIndex', @TSepiImportsTHeaderSection.SetImageIndex,
      'procedure(const Value: TImageIndex)');

    CurrentVisibility := mvProtected;

    AddMethod('GetDisplayName', @TSepiImportsTHeaderSection.GetDisplayName,
      'function: string',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTHeaderSection.Create,
      'constructor(Collection: TCollection)',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTHeaderSection.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('ParentBiDiModeChanged',
      @TSepiImportsTHeaderSection.ParentBiDiModeChanged,
      'procedure');
    AddMethod('UseRightToLeftAlignment',
      @TSepiImportsTHeaderSection.UseRightToLeftAlignment,
      'function: Boolean');
    AddMethod('UseRightToLeftReading',
      @TSepiImportsTHeaderSection.UseRightToLeftReading,
      'function: Boolean');

    AddProperty('Left', 'property: Integer',
      'GetLeft', '');
    AddProperty('Right', 'property: Integer',
      'GetRight', '');

    CurrentVisibility := mvPublished;

    AddProperty('Alignment', 'property: TAlignment',
      'FAlignment', 'SetAlignment',
      NoIndex, Integer(taLeftJustify));
    AddProperty('AllowClick', 'property: Boolean',
      'FAllowClick', 'FAllowClick',
      NoIndex, Integer(True));
    AddProperty('AutoSize', 'property: Boolean',
      'FAutoSize', 'SetAutoSize',
      NoIndex, Integer(False));
    AddProperty('BiDiMode', 'property: TBiDiMode',
      'FBiDiMode', 'SetBiDiMode',
      NoIndex, NoDefaultValue, 'IsBiDiModeStored');
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImageIndex');
    AddProperty('MaxWidth', 'property: Integer',
      'FMaxWidth', 'SetMaxWidth',
      NoIndex, 10000);
    AddProperty('MinWidth', 'property: Integer',
      'FMinWidth', 'SetMinWidth',
      NoIndex, 0);
    AddProperty('ParentBiDiMode', 'property: Boolean',
      'FParentBiDiMode', 'SetParentBiDiMode',
      NoIndex, Integer(True));
    AddProperty('Style', 'property: THeaderSectionStyle',
      'FStyle', 'SetStyle',
      NoIndex, Integer(hsText));
    AddProperty('Text', 'property: string',
      'FText', 'SetText');
    AddProperty('Width', 'property: Integer',
      'FWidth', 'SetWidth');

    Complete;
  end;
end;

{------------------------}
{ THeaderSections import }
{------------------------}

function TSepiImportsTHeaderSections.GetItem(Index: Integer): THeaderSection;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTHeaderSections.SetItem(Index: Integer;
  Value: THeaderSection);
begin
  Items[Index] := Value;
end;

class function TSepiImportsTHeaderSections.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(THeaderSections));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHeaderControl', System.TypeInfo(TCustomHeaderControl));

    AddMethod('GetItem', @TSepiImportsTHeaderSections.GetItem,
      'function(Index: Integer): THeaderSection');
    AddMethod('SetItem', @TSepiImportsTHeaderSections.SetItem,
      'procedure(Index: Integer; Value: THeaderSection)');

    CurrentVisibility := mvProtected;

    AddMethod('GetOwner', @TSepiImportsTHeaderSections.GetOwner,
      'function: TPersistent',
      mlkOverride);
    AddMethod('Update', @TSepiImportsTHeaderSections.Update,
      'procedure(Item: TCollectionItem)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTHeaderSections.Create,
      'constructor(HeaderControl: TCustomHeaderControl)');
    AddMethod('Add', @TSepiImportsTHeaderSections.Add,
      'function: THeaderSection');
    AddMethod('AddItem', @TSepiImportsTHeaderSections.AddItem,
      'function(Item: THeaderSection; Index: Integer): THeaderSection');
    AddMethod('Insert', @TSepiImportsTHeaderSections.Insert,
      'function(Index: Integer): THeaderSection');

    AddProperty('Items', 'property[Index: Integer]: THeaderSection',
      'GetItem', 'SetItem', True);

    Complete;
  end;
end;

{-----------------------------}
{ TCustomHeaderControl import }
{-----------------------------}

procedure TSepiImportsTCustomHeaderControl.SetDragReorder(
  const Value: Boolean);
begin
  DragReorder := Value;
end;

procedure TSepiImportsTCustomHeaderControl.SetFullDrag(Value: Boolean);
begin
  FullDrag := Value;
end;

procedure TSepiImportsTCustomHeaderControl.SetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TSepiImportsTCustomHeaderControl.SetSections(Value: THeaderSections);
begin
  Sections := Value;
end;

procedure TSepiImportsTCustomHeaderControl.SetStyle(Value: THeaderStyle);
begin
  Style := Value;
end;

class function TSepiImportsTCustomHeaderControl.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomHeaderControl'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomHeaderControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FSections', System.TypeInfo(THeaderSections));
    AddField('FSectionStream', System.TypeInfo(TMemoryStream));
    AddField('FUpdatingSectionOrder', System.TypeInfo(Boolean));
    AddField('FSectionDragged', System.TypeInfo(Boolean), True);
    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FFromIndex', System.TypeInfo(Integer));
    AddField('FToIndex', System.TypeInfo(Integer), True);
    AddField('FFullDrag', System.TypeInfo(Boolean));
    AddField('FHotTrack', System.TypeInfo(Boolean));
    AddField('FDragReorder', System.TypeInfo(Boolean));
    AddField('FImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FImages', System.TypeInfo(TCustomImageList));
    AddField('FStyle', System.TypeInfo(THeaderStyle));
    AddField('FTrackSection', System.TypeInfo(THeaderSection));
    AddField('FTrackWidth', System.TypeInfo(Integer));
    AddField('FTrackPos', 'TPoint');
    AddField('FOnDrawSection', System.TypeInfo(TCustomDrawSectionEvent));
    AddField('FOnSectionClick', System.TypeInfo(TCustomSectionNotifyEvent));
    AddField('FOnSectionResize', System.TypeInfo(TCustomSectionNotifyEvent));
    AddField('FOnSectionTrack', System.TypeInfo(TCustomSectionTrackEvent));
    AddField('FOnSectionDrag', System.TypeInfo(TSectionDragEvent));
    AddField('FOnSectionEndDrag', System.TypeInfo(TNotifyEvent));
    AddField('FOnCreateSectionClass',
      System.TypeInfo(TCustomHCCreateSectionClassEvent));

    AddMethod('DoSectionDrag', nil,
      'function(FromSection, ToSection: THeaderSection): Boolean');
    AddMethod('DoSectionEndDrag', nil,
      'procedure');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('SetDragReorder',
      @TSepiImportsTCustomHeaderControl.SetDragReorder,
      'procedure(const Value: Boolean)');
    AddMethod('SetFullDrag', @TSepiImportsTCustomHeaderControl.SetFullDrag,
      'procedure(Value: Boolean)');
    AddMethod('SetHotTrack', @TSepiImportsTCustomHeaderControl.SetHotTrack,
      'procedure(Value: Boolean)');
    AddMethod('SetSections', @TSepiImportsTCustomHeaderControl.SetSections,
      'procedure(Value: THeaderSections)');
    AddMethod('SetStyle', @TSepiImportsTCustomHeaderControl.SetStyle,
      'procedure(Value: THeaderStyle)');
    AddMethod('UpdateItem', nil,
      'procedure(Message, Index: Integer)');
    AddMethod('UpdateSection', nil,
      'procedure(Index: Integer)');
    AddMethod('UpdateSections', nil,
      'procedure');
    AddMethod('CMBiDiModeChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BIDIMODECHANGED);
    AddMethod('CNDrawItem', nil,
      'procedure(var Message: TWMDrawItem)',
      mlkMessage, False, CN_DRAWITEM);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('WMLButtonDown', nil,
      'procedure(var Message: TWMLButtonDown)',
      mlkMessage, False, WM_LBUTTONDOWN);
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('WMWindowPosChanged', nil,
      'procedure(var Message: TWMWindowPosChanged)',
      mlkMessage, False, WM_WINDOWPOSCHANGED);

    CurrentVisibility := mvProtected;

    AddMethod('CreateSection', @TSepiImportsTCustomHeaderControl.CreateSection,
      'function: THeaderSection',
      mlkVirtual);
    AddMethod('CreateSections',
      @TSepiImportsTCustomHeaderControl.CreateSections,
      'function: THeaderSections',
      mlkVirtual);
    AddMethod('CreateParams', @TSepiImportsTCustomHeaderControl.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomHeaderControl.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWnd', @TSepiImportsTCustomHeaderControl.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('DrawSection', @TSepiImportsTCustomHeaderControl.DrawSection,
      'procedure(Section: THeaderSection; const Rect: TRect; Pressed: Boolean )',
      mlkDynamic);
    AddMethod('Notification', @TSepiImportsTCustomHeaderControl.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('SectionClick', @TSepiImportsTCustomHeaderControl.SectionClick,
      'procedure(Section: THeaderSection)',
      mlkDynamic);
    AddMethod('SectionDrag', @TSepiImportsTCustomHeaderControl.SectionDrag,
      'procedure(FromSection, ToSection: THeaderSection; var AllowDrag: Boolean)',
      mlkDynamic);
    AddMethod('SectionEndDrag',
      @TSepiImportsTCustomHeaderControl.SectionEndDrag,
      'procedure',
      mlkDynamic);
    AddMethod('SectionResize', @TSepiImportsTCustomHeaderControl.SectionResize,
      'procedure(Section: THeaderSection)',
      mlkDynamic);
    AddMethod('SectionTrack', @TSepiImportsTCustomHeaderControl.SectionTrack,
      'procedure(Section: THeaderSection; Width: Integer; State: TSectionTrackState )',
      mlkDynamic);
    AddMethod('SetImages', @TSepiImportsTCustomHeaderControl.SetImages,
      'procedure(Value: TCustomImageList)',
      mlkVirtual);
    AddMethod('WndProc', @TSepiImportsTCustomHeaderControl.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomHeaderControl.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomHeaderControl.Destroy,
      'destructor',
      mlkOverride);

    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');

    AddMethod('FlipChildren', @TSepiImportsTCustomHeaderControl.FlipChildren,
      'procedure(AllLevels: Boolean)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('Align',
      '', '', Integer(alTop));
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderWidth');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    AddProperty('DragReorder', 'property: Boolean',
      'FDragReorder', 'SetDragReorder',
      NoIndex, Integer(False));
    AddProperty('FullDrag', 'property: Boolean',
      'FFullDrag', 'SetFullDrag');
    AddProperty('HotTrack', 'property: Boolean',
      'FHotTrack', 'SetHotTrack');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    AddProperty('Images', 'property: TCustomImageList',
      'FImages', 'SetImages');
    RedefineProperty('Constraints');
    AddProperty('Sections', 'property: THeaderSections',
      'FSections', 'SetSections');
    AddProperty('Style', 'property: THeaderStyle',
      'FStyle', 'SetStyle');
    AddProperty('OnCreateSectionClass',
      'property: TCustomHCCreateSectionClassEvent',
      'FOnCreateSectionClass', 'FOnCreateSectionClass');
    AddProperty('OnDrawSection', 'property: TCustomDrawSectionEvent',
      'FOnDrawSection', 'FOnDrawSection');
    AddProperty('OnSectionClick', 'property: TCustomSectionNotifyEvent',
      'FOnSectionClick', 'FOnSectionClick');
    AddProperty('OnSectionDrag', 'property: TSectionDragEvent',
      'FOnSectionDrag', 'FOnSectionDrag');
    AddProperty('OnSectionEndDrag', 'property: TNotifyEvent',
      'FOnSectionEndDrag', 'FOnSectionEndDrag');
    AddProperty('OnSectionResize', 'property: TCustomSectionNotifyEvent',
      'FOnSectionResize', 'FOnSectionResize');
    AddProperty('OnSectionTrack', 'property: TCustomSectionTrackEvent',
      'FOnSectionTrack', 'FOnSectionTrack');

    Complete;
  end;
end;

{-----------------------}
{ THeaderControl import }
{-----------------------}

function TSepiImportsTHeaderControl.GetOnDrawSection: TDrawSectionEvent;
begin
  Result := OnDrawSection;
end;

function TSepiImportsTHeaderControl.GetOnSectionClick: TSectionNotifyEvent;
begin
  Result := OnSectionClick;
end;

function TSepiImportsTHeaderControl.GetOnSectionResize: TSectionNotifyEvent;
begin
  Result := OnSectionResize;
end;

function TSepiImportsTHeaderControl.GetOnSectionTrack: TSectionTrackEvent;
begin
  Result := OnSectionTrack;
end;

procedure TSepiImportsTHeaderControl.SetOnDrawSection(
  const Value: TDrawSectionEvent);
begin
  OnDrawSection := Value;
end;

procedure TSepiImportsTHeaderControl.SetOnSectionClick(
  const Value: TSectionNotifyEvent);
begin
  OnSectionClick := Value;
end;

procedure TSepiImportsTHeaderControl.SetOnSectionResize(
  const Value: TSectionNotifyEvent);
begin
  OnSectionResize := Value;
end;

procedure TSepiImportsTHeaderControl.SetOnSectionTrack(
  const Value: TSectionTrackEvent);
begin
  OnSectionTrack := Value;
end;

class function TSepiImportsTHeaderControl.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('THeaderControl'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(THeaderControl));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetOnDrawSection', @TSepiImportsTHeaderControl.GetOnDrawSection,
      'function: TDrawSectionEvent');
    AddMethod('GetOnSectionClick',
      @TSepiImportsTHeaderControl.GetOnSectionClick,
      'function: TSectionNotifyEvent');
    AddMethod('GetOnSectionResize',
      @TSepiImportsTHeaderControl.GetOnSectionResize,
      'function: TSectionNotifyEvent');
    AddMethod('GetOnSectionTrack',
      @TSepiImportsTHeaderControl.GetOnSectionTrack,
      'function: TSectionTrackEvent');
    AddMethod('SetOnDrawSection', @TSepiImportsTHeaderControl.SetOnDrawSection,
      'procedure(const Value: TDrawSectionEvent)');
    AddMethod('SetOnSectionClick',
      @TSepiImportsTHeaderControl.SetOnSectionClick,
      'procedure(const Value: TSectionNotifyEvent)');
    AddMethod('SetOnSectionResize',
      @TSepiImportsTHeaderControl.SetOnSectionResize,
      'procedure(const Value: TSectionNotifyEvent)');
    AddMethod('SetOnSectionTrack',
      @TSepiImportsTHeaderControl.SetOnSectionTrack,
      'procedure(const Value: TSectionTrackEvent)');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align',
      '', '', Integer(alTop));
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderWidth');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('DragReorder');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('FullDrag',
      '', '', Integer(True));
    RedefineProperty('HotTrack',
      '', '', Integer(False));
    RedefineProperty('Images');
    RedefineProperty('Constraints');
    RedefineProperty('Sections');
    RedefineProperty('ShowHint');
    RedefineProperty('Style',
      '', '', Integer(hsButtons));
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('Visible');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnCreateSectionClass');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnResize');
    AddProperty('OnDrawSection', 'property: TDrawSectionEvent',
      'GetOnDrawSection', 'SetOnDrawSection');
    AddProperty('OnSectionClick', 'property: TSectionNotifyEvent',
      'GetOnSectionClick', 'SetOnSectionClick');
    AddProperty('OnSectionResize', 'property: TSectionNotifyEvent',
      'GetOnSectionResize', 'SetOnSectionResize');
    AddProperty('OnSectionTrack', 'property: TSectionTrackEvent',
      'GetOnSectionTrack', 'SetOnSectionTrack');
    RedefineProperty('OnSectionDrag');
    RedefineProperty('OnSectionEndDrag');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{------------------}
{ TNodeInfo import }
{------------------}

function SepiImportTNodeInfo(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TNodeInfo', True, True);

  with Result do
  begin
    AddField('ImageIndex', System.TypeInfo(Integer));
    AddField('SelectedIndex', System.TypeInfo(Integer));
    AddField('StateIndex', System.TypeInfo(Integer));
    AddField('OverlayIndex', System.TypeInfo(Integer));
    AddField('Data', 'Pointer');
    AddField('Count', System.TypeInfo(Integer));
    AddField('Text', System.TypeInfo(TUnnamed_1));

    Complete;
  end;
end;

{----------------------}
{ TNodeDataInfo import }
{----------------------}

function SepiImportTNodeDataInfo(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TNodeDataInfo', True, True);

  with Result do
  begin
    AddField('ImageIndex', System.TypeInfo(Integer));
    AddField('SelectedIndex', System.TypeInfo(Integer));
    AddField('StateIndex', System.TypeInfo(Integer));
    AddField('OverlayIndex', System.TypeInfo(Integer));
    AddField('Data', 'Pointer');
    AddField('Count', System.TypeInfo(Integer));
    AddField('TextLen', System.TypeInfo(Byte));

    Complete;
  end;
end;

{------------------}
{ TTreeNode import }
{------------------}

function TSepiImportsTTreeNode.GetAbsoluteIndex: Integer;
begin
  Result := AbsoluteIndex;
end;

function TSepiImportsTTreeNode.GetExpanded: Boolean;
begin
  Result := Expanded;
end;

function TSepiImportsTTreeNode.GetLevel: Integer;
begin
  Result := Level;
end;

function TSepiImportsTTreeNode.GetParent: TTreeNode;
begin
  Result := Parent;
end;

function TSepiImportsTTreeNode.GetChildren: Boolean;
begin
  Result := HasChildren;
end;

function TSepiImportsTTreeNode.GetCut: Boolean;
begin
  Result := Cut;
end;

function TSepiImportsTTreeNode.GetDropTarget: Boolean;
begin
  Result := DropTarget;
end;

function TSepiImportsTTreeNode.GetFocused: Boolean;
begin
  Result := Focused;
end;

function TSepiImportsTTreeNode.GetIndex: Integer;
begin
  Result := Index;
end;

function TSepiImportsTTreeNode.GetItem(Index: Integer): TTreeNode;
begin
  Result := Item[Index];
end;

function TSepiImportsTTreeNode.GetSelected: Boolean;
begin
  Result := Selected;
end;

function TSepiImportsTTreeNode.GetCount: Integer;
begin
  Result := Count;
end;

function TSepiImportsTTreeNode.GetTreeView: TCustomTreeView;
begin
  Result := TreeView;
end;

function TSepiImportsTTreeNode.IsNodeVisible: Boolean;
begin
  Result := IsVisible;
end;

procedure TSepiImportsTTreeNode.SetChildren(Value: Boolean);
begin
  HasChildren := Value;
end;

procedure TSepiImportsTTreeNode.SetCut(Value: Boolean);
begin
  Cut := Value;
end;

procedure TSepiImportsTTreeNode.SetData(Value: Pointer);
begin
  Data := Value;
end;

procedure TSepiImportsTTreeNode.SetDropTarget(Value: Boolean);
begin
  DropTarget := Value;
end;

procedure TSepiImportsTTreeNode.SetItem(Index: Integer; Value: TTreeNode);
begin
  Item[Index] := Value;
end;

procedure TSepiImportsTTreeNode.SetExpanded(Value: Boolean);
begin
  Expanded := Value;
end;

procedure TSepiImportsTTreeNode.SetFocused(Value: Boolean);
begin
  Focused := Value;
end;

procedure TSepiImportsTTreeNode.SetImageIndex(Value: TImageIndex);
begin
  ImageIndex := Value;
end;

procedure TSepiImportsTTreeNode.SetOverlayIndex(Value: Integer);
begin
  OverlayIndex := Value;
end;

procedure TSepiImportsTTreeNode.SetSelectedIndex(Value: Integer);
begin
  SelectedIndex := Value;
end;

procedure TSepiImportsTTreeNode.SetSelected(Value: Boolean);
begin
  Selected := Value;
end;

procedure TSepiImportsTTreeNode.SetStateIndex(Value: Integer);
begin
  StateIndex := Value;
end;

procedure TSepiImportsTTreeNode.SetText(const S: string);
begin
  Text := S;
end;

class function TSepiImportsTTreeNode.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TTreeNode'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TTreeNode));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOwner', System.TypeInfo(TTreeNodes));
    AddField('FText', System.TypeInfo(string));
    AddField('FData', 'Pointer');
    AddField('FItemId', 'HTreeItem');
    AddField('FImageIndex', System.TypeInfo(TImageIndex));
    AddField('FSelectedIndex', System.TypeInfo(Integer));
    AddField('FOverlayIndex', System.TypeInfo(Integer));
    AddField('FStateIndex', System.TypeInfo(Integer));
    AddField('FDeleting', System.TypeInfo(Boolean));
    AddField('FInTree', System.TypeInfo(Boolean));

    AddMethod('CompareCount', nil,
      'function(CompareMe: Integer): Boolean');
    AddMethod('DoCanExpand', nil,
      'function(Expand: Boolean): Boolean');
    AddMethod('DoExpand', nil,
      'procedure(Expand: Boolean)');
    AddMethod('ExpandItem', nil,
      'procedure(Expand: Boolean; Recurse: Boolean)');
    AddMethod('GetAbsoluteIndex', @TSepiImportsTTreeNode.GetAbsoluteIndex,
      'function: Integer');
    AddMethod('GetExpanded', @TSepiImportsTTreeNode.GetExpanded,
      'function: Boolean');
    AddMethod('GetLevel', @TSepiImportsTTreeNode.GetLevel,
      'function: Integer');
    AddMethod('GetParent', @TSepiImportsTTreeNode.GetParent,
      'function: TTreeNode');
    AddMethod('GetChildren', @TSepiImportsTTreeNode.GetChildren,
      'function: Boolean');
    AddMethod('GetCut', @TSepiImportsTTreeNode.GetCut,
      'function: Boolean');
    AddMethod('GetDropTarget', @TSepiImportsTTreeNode.GetDropTarget,
      'function: Boolean');
    AddMethod('GetFocused', @TSepiImportsTTreeNode.GetFocused,
      'function: Boolean');
    AddMethod('GetIndex', @TSepiImportsTTreeNode.GetIndex,
      'function: Integer');
    AddMethod('GetItem', @TSepiImportsTTreeNode.GetItem,
      'function(Index: Integer): TTreeNode');
    AddMethod('GetSelected', @TSepiImportsTTreeNode.GetSelected,
      'function: Boolean');
    AddMethod('GetCount', @TSepiImportsTTreeNode.GetCount,
      'function: Integer');
    AddMethod('GetTreeView', @TSepiImportsTTreeNode.GetTreeView,
      'function: TCustomTreeView');
    AddMethod('InternalMove', nil,
      'procedure(ParentNode, Node: TTreeNode; HItem: HTreeItem; AddMode: TAddMode )');
    AddMethod('IsEqual', nil,
      'function(Node: TTreeNode): Boolean');
    AddMethod('IsNodeVisible', @TSepiImportsTTreeNode.IsNodeVisible,
      'function: Boolean');
    AddMethod('ReadData', nil,
      'procedure(Stream: TStream; Info: PNodeInfo)');
    AddMethod('ReadNodeData', nil,
      'procedure(Stream: TStream; var Info: TNodeDataInfo)');
    AddMethod('SetChildren', @TSepiImportsTTreeNode.SetChildren,
      'procedure(Value: Boolean)');
    AddMethod('SetCut', @TSepiImportsTTreeNode.SetCut,
      'procedure(Value: Boolean)');
    AddMethod('SetData', @TSepiImportsTTreeNode.SetData,
      'procedure(Value: Pointer)');
    AddMethod('SetDropTarget', @TSepiImportsTTreeNode.SetDropTarget,
      'procedure(Value: Boolean)');
    AddMethod('SetItem', @TSepiImportsTTreeNode.SetItem,
      'procedure(Index: Integer; Value: TTreeNode)');
    AddMethod('SetExpanded', @TSepiImportsTTreeNode.SetExpanded,
      'procedure(Value: Boolean)');
    AddMethod('SetFocused', @TSepiImportsTTreeNode.SetFocused,
      'procedure(Value: Boolean)');
    AddMethod('SetImageIndex', @TSepiImportsTTreeNode.SetImageIndex,
      'procedure(Value: TImageIndex)');
    AddMethod('SetOverlayIndex', @TSepiImportsTTreeNode.SetOverlayIndex,
      'procedure(Value: Integer)');
    AddMethod('SetSelectedIndex', @TSepiImportsTTreeNode.SetSelectedIndex,
      'procedure(Value: Integer)');
    AddMethod('SetSelected', @TSepiImportsTTreeNode.SetSelected,
      'procedure(Value: Boolean)');
    AddMethod('SetStateIndex', @TSepiImportsTTreeNode.SetStateIndex,
      'procedure(Value: Integer)');
    AddMethod('SetText', @TSepiImportsTTreeNode.SetText,
      'procedure(const S: string)');
    AddMethod('WriteNodeData', nil,
      'procedure(Stream: TStream; var Info: TNodeDataInfo)');

    CurrentVisibility := mvProtected;

    AddMethod('GetState', @TSepiImportsTTreeNode.GetState,
      'function(NodeState: TNodeState): Boolean');
    AddMethod('SetState', @TSepiImportsTTreeNode.SetState,
      'procedure(NodeState: TNodeState; Value: Boolean)');
    AddMethod('SetSelectedBit', @TSepiImportsTTreeNode.SetSelectedBit,
      'procedure(Value: Boolean)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTTreeNode.Create,
      'constructor(AOwner: TTreeNodes)');
    AddMethod('Destroy', @TSepiImportsTTreeNode.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AlphaSort', @TSepiImportsTTreeNode.AlphaSort,
      'function(ARecurse: Boolean = False): Boolean');
    AddMethod('Assign', @TSepiImportsTTreeNode.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('Collapse', @TSepiImportsTTreeNode.Collapse,
      'procedure(Recurse: Boolean)');
    AddMethod('CustomSort', @TSepiImportsTTreeNode.CustomSort,
      'function(SortProc: TTVCompare; Data: Longint; ARecurse: Boolean = False): Boolean');
    AddMethod('Delete', @TSepiImportsTTreeNode.Delete,
      'procedure');
    AddMethod('DeleteChildren', @TSepiImportsTTreeNode.DeleteChildren,
      'procedure');
    AddMethod('DisplayRect', @TSepiImportsTTreeNode.DisplayRect,
      'function(TextOnly: Boolean): TRect');
    AddMethod('EditText', @TSepiImportsTTreeNode.EditText,
      'function: Boolean');
    AddMethod('EndEdit', @TSepiImportsTTreeNode.EndEdit,
      'procedure(Cancel: Boolean)');
    AddMethod('Expand', @TSepiImportsTTreeNode.Expand,
      'procedure(Recurse: Boolean)');
    AddMethod('getFirstChild', @TSepiImportsTTreeNode.GetFirstChild,
      'function: TTreeNode');
    AddMethod('GetHandle', @TSepiImportsTTreeNode.GetHandle,
      'function: HWND');
    AddMethod('GetLastChild', @TSepiImportsTTreeNode.GetLastChild,
      'function: TTreeNode');
    AddMethod('GetNext', @TSepiImportsTTreeNode.GetNext,
      'function: TTreeNode');
    AddMethod('GetNextChild', @TSepiImportsTTreeNode.GetNextChild,
      'function(Value: TTreeNode): TTreeNode');
    AddMethod('getNextSibling', @TSepiImportsTTreeNode.getNextSibling,
      'function: TTreeNode');
    AddMethod('GetNextVisible', @TSepiImportsTTreeNode.GetNextVisible,
      'function: TTreeNode');
    AddMethod('GetPrev', @TSepiImportsTTreeNode.GetPrev,
      'function: TTreeNode');
    AddMethod('GetPrevChild', @TSepiImportsTTreeNode.GetPrevChild,
      'function(Value: TTreeNode): TTreeNode');
    AddMethod('getPrevSibling', @TSepiImportsTTreeNode.getPrevSibling,
      'function: TTreeNode');
    AddMethod('GetPrevVisible', @TSepiImportsTTreeNode.GetPrevVisible,
      'function: TTreeNode');
    AddMethod('HasAsParent', @TSepiImportsTTreeNode.HasAsParent,
      'function(Value: TTreeNode): Boolean');
    AddMethod('IndexOf', @TSepiImportsTTreeNode.IndexOf,
      'function(Value: TTreeNode): Integer');
    AddMethod('MakeVisible', @TSepiImportsTTreeNode.MakeVisible,
      'procedure');
    AddMethod('MoveTo', @TSepiImportsTTreeNode.MoveTo,
      'procedure(Destination: TTreeNode; Mode: TNodeAttachMode)',
      mlkVirtual);

    AddProperty('AbsoluteIndex', 'property: Integer',
      'GetAbsoluteIndex', '');

    AddMethod('IsFirstNode', @TSepiImportsTTreeNode.IsFirstNode,
      'function: Boolean');

    AddProperty('Count', 'property: Integer',
      'GetCount', '');
    AddProperty('Cut', 'property: Boolean',
      'GetCut', 'SetCut');
    AddProperty('Data', 'property: Pointer',
      'FData', 'SetData');
    AddProperty('Deleting', 'property: Boolean',
      'FDeleting', '');
    AddProperty('Focused', 'property: Boolean',
      'GetFocused', 'SetFocused');
    AddProperty('DropTarget', 'property: Boolean',
      'GetDropTarget', 'SetDropTarget');
    AddProperty('Selected', 'property: Boolean',
      'GetSelected', 'SetSelected');
    AddProperty('Expanded', 'property: Boolean',
      'GetExpanded', 'SetExpanded');
    AddProperty('Handle', 'property: HWND',
      'GetHandle', '');
    AddProperty('HasChildren', 'property: Boolean',
      'GetChildren', 'SetChildren');
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImageIndex');
    AddProperty('Index', 'property: Integer',
      'GetIndex', '');
    AddProperty('IsVisible', 'property: Boolean',
      'IsNodeVisible', '');
    AddProperty('Item', 'property[Index: Integer]: TTreeNode',
      'GetItem', 'SetItem', True);
    AddProperty('ItemId', 'property: HTreeItem',
      'FItemId', '');
    AddProperty('Level', 'property: Integer',
      'GetLevel', '');
    AddProperty('OverlayIndex', 'property: Integer',
      'FOverlayIndex', 'SetOverlayIndex');
    AddProperty('Owner', 'property: TTreeNodes',
      'FOwner', '');
    AddProperty('Parent', 'property: TTreeNode',
      'GetParent', '');
    AddProperty('SelectedIndex', 'property: Integer',
      'FSelectedIndex', 'SetSelectedIndex');
    AddProperty('StateIndex', 'property: Integer',
      'FStateIndex', 'SetStateIndex');
    AddProperty('Text', 'property: string',
      'FText', 'SetText');
    AddProperty('TreeView', 'property: TCustomTreeView',
      'GetTreeView', '');

    Complete;
  end;
end;

{-----------------------------}
{ TTreeNodesEnumerator import }
{-----------------------------}

class function TSepiImportsTTreeNodesEnumerator.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTreeNodesEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FTreeNodes', System.TypeInfo(TTreeNodes));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTTreeNodesEnumerator.Create,
      'constructor(ATreeNodes: TTreeNodes)');
    AddMethod('GetCurrent', @TSepiImportsTTreeNodesEnumerator.GetCurrent,
      'function: TTreeNode');
    AddMethod('MoveNext', @TSepiImportsTTreeNodesEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: TTreeNode',
      'GetCurrent', '');

    Complete;
  end;
end;

{-------------------}
{ TNodeCache import }
{-------------------}

function SepiImportTNodeCache(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TNodeCache', False, True);

  with Result do
  begin
    AddField('CacheNode', System.TypeInfo(TTreeNode));
    AddField('CacheIndex', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------}
{ TTreeNodes import }
{-------------------}

function TSepiImportsTTreeNodes.GetHandle: HWND;
begin
  Result := Handle;
end;

function TSepiImportsTTreeNodes.GetNodeFromIndex(Index: Integer): TTreeNode;
begin
  Result := Item[Index];
end;

class function TSepiImportsTTreeNodes.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TTreeNodes'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TTreeNodes));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOwner', System.TypeInfo(TCustomTreeView));
    AddField('FUpdateCount', System.TypeInfo(Integer));
    AddField('FNodeCache', 'TNodeCache');
    AddField('FReading', System.TypeInfo(Boolean));

    AddMethod('AddedNode', nil,
      'procedure(Value: TTreeNode)');
    AddMethod('GetHandle', @TSepiImportsTTreeNodes.GetHandle,
      'function: HWND');
    AddMethod('GetNodeFromIndex', @TSepiImportsTTreeNodes.GetNodeFromIndex,
      'function(Index: Integer): TTreeNode');
    AddMethod('ReadData', nil,
      'procedure(Stream: TStream)');
    AddMethod('ReadNodeData', nil,
      'procedure(Stream: TStream)');
    AddMethod('Repaint', nil,
      'procedure(Node: TTreeNode)');
    AddMethod('WriteNodeData', nil,
      'procedure(Stream: TStream)');
    AddMethod('ClearCache', nil,
      'procedure');
    AddMethod('WriteExpandedState', nil,
      'procedure(Stream: TStream)');
    AddMethod('ReadExpandedState', nil,
      'procedure(Stream: TStream)');

    CurrentVisibility := mvProtected;

    AddMethod('AddItem', @TSepiImportsTTreeNodes.AddItem,
      'function(Parent, Target: HTreeItem; const Item: TTVItem; AddMode: TAddMode ) : HTreeItem');
    AddMethod('DefineProperties', @TSepiImportsTTreeNodes.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('CreateItem', @TSepiImportsTTreeNodes.CreateItem,
      'function(Node: TTreeNode): TTVItem');
    AddMethod('GetCount', @TSepiImportsTTreeNodes.GetCount,
      'function: Integer');
    AddMethod('SetItem', @TSepiImportsTTreeNodes.SetItem,
      'procedure(Index: Integer; Value: TTreeNode)');
    AddMethod('SetUpdateState', @TSepiImportsTTreeNodes.SetUpdateState,
      'procedure(Updating: Boolean)');

    AddProperty('Reading', 'property: Boolean',
      'FReading', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTTreeNodes.Create,
      'constructor(AOwner: TCustomTreeView)');
    AddMethod('Destroy', @TSepiImportsTTreeNodes.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AddChildFirst', @TSepiImportsTTreeNodes.AddChildFirst,
      'function(Parent: TTreeNode; const S: string): TTreeNode');
    AddMethod('AddChild', @TSepiImportsTTreeNodes.AddChild,
      'function(Parent: TTreeNode; const S: string): TTreeNode');
    AddMethod('AddChildObjectFirst',
      @TSepiImportsTTreeNodes.AddChildObjectFirst,
      'function(Parent: TTreeNode; const S: string; Ptr: Pointer ) : TTreeNode');
    AddMethod('AddChildObject', @TSepiImportsTTreeNodes.AddChildObject,
      'function(Parent: TTreeNode; const S: string; Ptr: Pointer ) : TTreeNode');
    AddMethod('AddFirst', @TSepiImportsTTreeNodes.AddFirst,
      'function(Sibling: TTreeNode; const S: string): TTreeNode');
    AddMethod('Add', @TSepiImportsTTreeNodes.Add,
      'function(Sibling: TTreeNode; const S: string): TTreeNode');
    AddMethod('AddObjectFirst', @TSepiImportsTTreeNodes.AddObjectFirst,
      'function(Sibling: TTreeNode; const S: string; Ptr: Pointer ) : TTreeNode');
    AddMethod('AddObject', @TSepiImportsTTreeNodes.AddObject,
      'function(Sibling: TTreeNode; const S: string; Ptr: Pointer ) : TTreeNode');
    AddMethod('AddNode', @TSepiImportsTTreeNodes.AddNode,
      'function(Node, Relative: TTreeNode; const S: string; Ptr: Pointer ; Method: TNodeAttachMode ) : TTreeNode');
    AddMethod('AlphaSort', @TSepiImportsTTreeNodes.AlphaSort,
      'function(ARecurse: Boolean = False): Boolean');
    AddMethod('Assign', @TSepiImportsTTreeNodes.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('BeginUpdate', @TSepiImportsTTreeNodes.BeginUpdate,
      'procedure');
    AddMethod('Clear', @TSepiImportsTTreeNodes.Clear,
      'procedure');
    AddMethod('CustomSort', @TSepiImportsTTreeNodes.CustomSort,
      'function(SortProc: TTVCompare; Data: Longint; ARecurse: Boolean = False): Boolean');
    AddMethod('Delete', @TSepiImportsTTreeNodes.Delete,
      'procedure(Node: TTreeNode)');
    AddMethod('EndUpdate', @TSepiImportsTTreeNodes.EndUpdate,
      'procedure');
    AddMethod('GetFirstNode', @TSepiImportsTTreeNodes.GetFirstNode,
      'function: TTreeNode');
    AddMethod('GetEnumerator', @TSepiImportsTTreeNodes.GetEnumerator,
      'function: TTreeNodesEnumerator');
    AddMethod('GetNode', @TSepiImportsTTreeNodes.GetNode,
      'function(ItemId: HTreeItem): TTreeNode');
    AddMethod('Insert', @TSepiImportsTTreeNodes.Insert,
      'function(Sibling: TTreeNode; const S: string): TTreeNode');
    AddMethod('InsertObject', @TSepiImportsTTreeNodes.InsertObject,
      'function(Sibling: TTreeNode; const S: string; Ptr: Pointer ) : TTreeNode');
    AddMethod('InsertNode', @TSepiImportsTTreeNodes.InsertNode,
      'function(Node, Sibling: TTreeNode; const S: string; Ptr: Pointer ) : TTreeNode');

    AddProperty('Count', 'property: Integer',
      'GetCount', '');
    AddProperty('Handle', 'property: HWND',
      'GetHandle', '');
    AddProperty('Item', 'property[Index: Integer]: TTreeNode',
      'GetNodeFromIndex', '', True);
    AddProperty('Owner', 'property: TCustomTreeView',
      'FOwner', '');

    Complete;
  end;
end;

{-----------------------}
{ ETreeViewError import }
{-----------------------}

class function TSepiImportsETreeViewError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ETreeViewError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ TCustomTreeView import }
{------------------------}

function TSepiImportsTCustomTreeView.GetChangeDelay: Integer;
begin
  Result := ChangeDelay;
end;

function TSepiImportsTCustomTreeView.GetDropTarget: TTreeNode;
begin
  Result := DropTarget;
end;

function TSepiImportsTCustomTreeView.GetIndent: Integer;
begin
  Result := Indent;
end;

function TSepiImportsTCustomTreeView.GetSelected: TTreeNode;
begin
  Result := Selected;
end;

function TSepiImportsTCustomTreeView.GetSelectionCount: Cardinal;
begin
  Result := SelectionCount;
end;

function TSepiImportsTCustomTreeView.GetSelection(Index: Integer): TTreeNode;
begin
  Result := Selections[Index];
end;

function TSepiImportsTCustomTreeView.GetTopItem: TTreeNode;
begin
  Result := TopItem;
end;

procedure TSepiImportsTCustomTreeView.SetAutoExpand(Value: Boolean);
begin
  AutoExpand := Value;
end;

procedure TSepiImportsTCustomTreeView.SetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TSepiImportsTCustomTreeView.SetButtonStyle(Value: Boolean);
begin
  ShowButtons := Value;
end;

procedure TSepiImportsTCustomTreeView.SetChangeDelay(Value: Integer);
begin
  ChangeDelay := Value;
end;

procedure TSepiImportsTCustomTreeView.SetDropTarget(Value: TTreeNode);
begin
  DropTarget := Value;
end;

procedure TSepiImportsTCustomTreeView.SetHideSelection(Value: Boolean);
begin
  HideSelection := Value;
end;

procedure TSepiImportsTCustomTreeView.SetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TSepiImportsTCustomTreeView.SetIndent(Value: Integer);
begin
  Indent := Value;
end;

procedure TSepiImportsTCustomTreeView.SetImages(Value: TCustomImageList);
begin
  Images := Value;
end;

procedure TSepiImportsTCustomTreeView.SetLineStyle(Value: Boolean);
begin
  ShowLines := Value;
end;

procedure TSepiImportsTCustomTreeView.SetMultiSelect(const Value: Boolean);
begin
  MultiSelect := Value;
end;

procedure TSepiImportsTCustomTreeView.SetMultiSelectStyle(
  const Value: TMultiSelectStyle);
begin
  MultiSelectStyle := Value;
end;

procedure TSepiImportsTCustomTreeView.SetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TSepiImportsTCustomTreeView.SetRootStyle(Value: Boolean);
begin
  ShowRoot := Value;
end;

procedure TSepiImportsTCustomTreeView.SetRowSelect(Value: Boolean);
begin
  RowSelect := Value;
end;

procedure TSepiImportsTCustomTreeView.SetSelected(Value: TTreeNode);
begin
  Selected := Value;
end;

procedure TSepiImportsTCustomTreeView.SetSortType(Value: TSortType);
begin
  SortType := Value;
end;

procedure TSepiImportsTCustomTreeView.SetStateImages(Value: TCustomImageList);
begin
  StateImages := Value;
end;

procedure TSepiImportsTCustomTreeView.SetToolTips(Value: Boolean);
begin
  ToolTips := Value;
end;

procedure TSepiImportsTCustomTreeView.SetTreeNodes(Value: TTreeNodes);
begin
  Items := Value;
end;

procedure TSepiImportsTCustomTreeView.SetTopItem(Value: TTreeNode);
begin
  TopItem := Value;
end;

class function TSepiImportsTCustomTreeView.SepiImport(
  Owner: TSepiUnit): TSepiClass;
const
  DefaultMultiSelectStyle: TMultiSelectStyle = [msControlSelect];
begin
  Result := TSepiClass(Owner.FindMeta('TCustomTreeView'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomTreeView));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAutoExpand', System.TypeInfo(Boolean));
    AddField('FBorderStyle', System.TypeInfo(TBorderStyle));
    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FCanvasChanged', System.TypeInfo(Boolean));
    AddField('FDefEditProc', 'Pointer');
    AddField('FDragged', System.TypeInfo(Boolean));
    AddField('FDragImage', System.TypeInfo(TDragImageList));
    AddField('FDragNode', System.TypeInfo(TTreeNode));
    AddField('FEditHandle', System.TypeInfo(HWND));
    AddField('FEditInstance', 'Pointer');
    AddField('FHideSelection', System.TypeInfo(Boolean));
    AddField('FHotTrack', System.TypeInfo(Boolean));
    AddField('FImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FImages', System.TypeInfo(TCustomImageList));
    AddField('FLastDropTarget', System.TypeInfo(TTreeNode));
    AddField('FMemStream', System.TypeInfo(TMemoryStream));
    AddField('FRClickNode', System.TypeInfo(TTreeNode));
    AddField('FRightClickSelect', System.TypeInfo(Boolean));
    AddField('FManualNotify', System.TypeInfo(Boolean));
    AddField('FReadOnly', System.TypeInfo(Boolean));
    AddField('FRowSelect', System.TypeInfo(Boolean));
    AddField('FSaveIndex', System.TypeInfo(Integer));
    AddField('FSaveIndent', System.TypeInfo(Integer));
    AddField('FSaveItems', System.TypeInfo(TStringList));
    AddField('FSaveTopIndex', System.TypeInfo(Integer));
    AddField('FShowButtons', System.TypeInfo(Boolean));
    AddField('FShowLines', System.TypeInfo(Boolean));
    AddField('FShowRoot', System.TypeInfo(Boolean));
    AddField('FSortType', System.TypeInfo(TSortType));
    AddField('FStateChanging', System.TypeInfo(Boolean));
    AddField('FStateImages', System.TypeInfo(TCustomImageList));
    AddField('FStateChangeLink', System.TypeInfo(TChangeLink));
    AddField('FToolTips', System.TypeInfo(Boolean));
    AddField('FTreeNodes', System.TypeInfo(TTreeNodes));
    AddField('FWideText', System.TypeInfo(WideString));
    AddField('FMultiSelect', System.TypeInfo(Boolean));
    AddField('FMultiSelectStyle', System.TypeInfo(TMultiSelectStyle));
    AddField('FSelections', System.TypeInfo(TList));
    AddField('FSaveIndexes', System.TypeInfo(TList));
    AddField('FShiftAnchor', System.TypeInfo(TTreeNode));
    AddField('FSelecting', System.TypeInfo(Boolean));
    AddField('FSelectChanged', System.TypeInfo(Boolean), True);
    AddField('FOurFont', System.TypeInfo(Integer));
    AddField('FStockFont', System.TypeInfo(Integer));
    AddField('FCreateWndRestores', System.TypeInfo(Boolean));
    AddField('FOnAdvancedCustomDraw',
      System.TypeInfo(TTVAdvancedCustomDrawEvent));
    AddField('FOnAdvancedCustomDrawItem',
      System.TypeInfo(TTVAdvancedCustomDrawItemEvent));
    AddField('FOnCancelEdit', System.TypeInfo(TTVChangedEvent));
    AddField('FOnChange', System.TypeInfo(TTVChangedEvent));
    AddField('FOnChanging', System.TypeInfo(TTVChangingEvent));
    AddField('FOnCollapsed', System.TypeInfo(TTVExpandedEvent));
    AddField('FOnCollapsing', System.TypeInfo(TTVCollapsingEvent));
    AddField('FOnCompare', System.TypeInfo(TTVCompareEvent));
    AddField('FOnCustomDraw', System.TypeInfo(TTVCustomDrawEvent));
    AddField('FOnCustomDrawItem', System.TypeInfo(TTVCustomDrawItemEvent));
    AddField('FOnDeletion', System.TypeInfo(TTVExpandedEvent));
    AddField('FOnAddition', System.TypeInfo(TTVExpandedEvent));
    AddField('FOnEditing', System.TypeInfo(TTVEditingEvent));
    AddField('FOnEdited', System.TypeInfo(TTVEditedEvent));
    AddField('FOnExpanded', System.TypeInfo(TTVExpandedEvent));
    AddField('FOnExpanding', System.TypeInfo(TTVExpandingEvent));
    AddField('FOnGetImageIndex', System.TypeInfo(TTVExpandedEvent));
    AddField('FOnGetSelectedIndex', System.TypeInfo(TTVExpandedEvent));
    AddField('FOnCreateNodeClass', System.TypeInfo(TTVCreateNodeClassEvent));

    AddMethod('CanvasChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CMDrag', nil,
      'procedure(var Message: TCMDrag)',
      mlkMessage, False, CM_DRAG);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('EditWndProc', nil,
      'procedure(var Message: TMessage)');
    AddMethod('DoDragOver', nil,
      'procedure(Source: TDragObject; X, Y: Integer; CanDrop: Boolean)');
    AddMethod('NodeDeselect', nil,
      'procedure(Index: Integer)');
    AddMethod('NodeSelect', nil,
      'procedure(Node: TTreeNode; At: Integer = 0)');
    AddMethod('FinishSelection', nil,
      'procedure(Node: TTreeNode; ShiftState: TShiftState)');
    AddMethod('ControlSelectNode', nil,
      'procedure(Node: TTreeNode)');
    AddMethod('ShiftSelectNode', nil,
      'procedure(Node: TTreeNode; Backward: Boolean; Deselect: Boolean = True)');
    AddMethod('ControlShiftSelectNode', nil,
      'procedure(Node: TTreeNode; Backward: Boolean)');
    AddMethod('SelectNode', nil,
      'procedure(Node: TTreeNode)');
    AddMethod('GetChangeDelay', @TSepiImportsTCustomTreeView.GetChangeDelay,
      'function: Integer');
    AddMethod('GetDropTarget', @TSepiImportsTCustomTreeView.GetDropTarget,
      'function: TTreeNode');
    AddMethod('GetIndent', @TSepiImportsTCustomTreeView.GetIndent,
      'function: Integer');
    AddMethod('GetNodeFromItem', nil,
      'function(const Item: TTVItem): TTreeNode');
    AddMethod('GetSelected', @TSepiImportsTCustomTreeView.GetSelected,
      'function: TTreeNode');
    AddMethod('GetSelectionCount',
      @TSepiImportsTCustomTreeView.GetSelectionCount,
      'function: Cardinal');
    AddMethod('GetSelection', @TSepiImportsTCustomTreeView.GetSelection,
      'function(Index: Integer): TTreeNode');
    AddMethod('GetTopItem', @TSepiImportsTCustomTreeView.GetTopItem,
      'function: TTreeNode');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('SetAutoExpand', @TSepiImportsTCustomTreeView.SetAutoExpand,
      'procedure(Value: Boolean)');
    AddMethod('SetBorderStyle', @TSepiImportsTCustomTreeView.SetBorderStyle,
      'procedure(Value: TBorderStyle)');
    AddMethod('SetButtonStyle', @TSepiImportsTCustomTreeView.SetButtonStyle,
      'procedure(Value: Boolean)');
    AddMethod('SetChangeDelay', @TSepiImportsTCustomTreeView.SetChangeDelay,
      'procedure(Value: Integer)');
    AddMethod('SetDropTarget', @TSepiImportsTCustomTreeView.SetDropTarget,
      'procedure(Value: TTreeNode)');
    AddMethod('SetHideSelection',
      @TSepiImportsTCustomTreeView.SetHideSelection,
      'procedure(Value: Boolean)');
    AddMethod('SetHotTrack', @TSepiImportsTCustomTreeView.SetHotTrack,
      'procedure(Value: Boolean)');
    AddMethod('SetImageList', nil,
      'procedure(Value: HImageList; Flags: Integer)');
    AddMethod('SetIndent', @TSepiImportsTCustomTreeView.SetIndent,
      'procedure(Value: Integer)');
    AddMethod('SetImages', @TSepiImportsTCustomTreeView.SetImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('SetLineStyle', @TSepiImportsTCustomTreeView.SetLineStyle,
      'procedure(Value: Boolean)');
    AddMethod('SetMultiSelect', @TSepiImportsTCustomTreeView.SetMultiSelect,
      'procedure(const Value: Boolean)');
    AddMethod('SetMultiSelectStyle',
      @TSepiImportsTCustomTreeView.SetMultiSelectStyle,
      'procedure(const Value: TMultiSelectStyle)');
    AddMethod('SetReadOnly', @TSepiImportsTCustomTreeView.SetReadOnly,
      'procedure(Value: Boolean)');
    AddMethod('SetRootStyle', @TSepiImportsTCustomTreeView.SetRootStyle,
      'procedure(Value: Boolean)');
    AddMethod('SetRowSelect', @TSepiImportsTCustomTreeView.SetRowSelect,
      'procedure(Value: Boolean)');
    AddMethod('SetSelected', @TSepiImportsTCustomTreeView.SetSelected,
      'procedure(Value: TTreeNode)');
    AddMethod('SetSortType', @TSepiImportsTCustomTreeView.SetSortType,
      'procedure(Value: TSortType)');
    AddMethod('SetStateImages', @TSepiImportsTCustomTreeView.SetStateImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('SetToolTips', @TSepiImportsTCustomTreeView.SetToolTips,
      'procedure(Value: Boolean)');
    AddMethod('SetTreeNodes', @TSepiImportsTCustomTreeView.SetTreeNodes,
      'procedure(Value: TTreeNodes)');
    AddMethod('SetTopItem', @TSepiImportsTCustomTreeView.SetTopItem,
      'procedure(Value: TTreeNode)');
    AddMethod('OnChangeTimer', nil,
      'procedure(Sender: TObject)');
    AddMethod('WMLButtonDown', nil,
      'procedure(var Message: TWMLButtonDown)',
      mlkMessage, False, WM_LBUTTONDOWN);
    AddMethod('WMNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, WM_NOTIFY);
    AddMethod('WMContextMenu', nil,
      'procedure(var Message: TWMContextMenu)',
      mlkMessage, False, WM_CONTEXTMENU);
    AddMethod('WMCtlColorEdit', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_CTLCOLOREDIT);
    AddMethod('CMSysColorChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSCOLORCHANGE);

    CurrentVisibility := mvProtected;

    AddField('FChangeTimer', System.TypeInfo(TTimer));

    AddMethod('CanEdit', @TSepiImportsTCustomTreeView.CanEdit,
      'function(Node: TTreeNode): Boolean',
      mlkDynamic);
    AddMethod('CanChange', @TSepiImportsTCustomTreeView.CanChange,
      'function(Node: TTreeNode): Boolean',
      mlkDynamic);
    AddMethod('CanCollapse', @TSepiImportsTCustomTreeView.CanCollapse,
      'function(Node: TTreeNode): Boolean',
      mlkDynamic);
    AddMethod('CanExpand', @TSepiImportsTCustomTreeView.CanExpand,
      'function(Node: TTreeNode): Boolean',
      mlkDynamic);
    AddMethod('Change', @TSepiImportsTCustomTreeView.Change,
      'procedure(Node: TTreeNode)',
      mlkDynamic);
    AddMethod('Collapse', @TSepiImportsTCustomTreeView.Collapse,
      'procedure(Node: TTreeNode)',
      mlkDynamic);
    AddMethod('CreateNode', @TSepiImportsTCustomTreeView.CreateNode,
      'function: TTreeNode',
      mlkVirtual);
    AddMethod('CreateNodes', @TSepiImportsTCustomTreeView.CreateNodes,
      'function: TTreeNodes',
      mlkVirtual);
    AddMethod('CreateParams', @TSepiImportsTCustomTreeView.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomTreeView.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('CustomDraw', @TSepiImportsTCustomTreeView.CustomDraw,
      'function(const ARect: TRect; Stage: TCustomDrawStage): Boolean',
      mlkVirtual);
    AddMethod('CustomDrawItem', @TSepiImportsTCustomTreeView.CustomDrawItem,
      'function(Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage ; var PaintImages: Boolean ) : Boolean',
      mlkVirtual);
    AddMethod('Delete', @TSepiImportsTCustomTreeView.Delete,
      'procedure(Node: TTreeNode)',
      mlkDynamic);
    AddMethod('Added', @TSepiImportsTCustomTreeView.Added,
      'procedure(Node: TTreeNode)',
      mlkDynamic);
    AddMethod('DestroyWnd', @TSepiImportsTCustomTreeView.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('DoEndDrag', @TSepiImportsTCustomTreeView.DoEndDrag,
      'procedure(Target: TObject; X, Y: Integer)',
      mlkOverride);
    AddMethod('DoStartDrag', @TSepiImportsTCustomTreeView.DoStartDrag,
      'procedure(var DragObject: TDragObject)',
      mlkOverride);
    AddMethod('Edit', @TSepiImportsTCustomTreeView.Edit,
      'procedure(const Item: TTVItem)',
      mlkDynamic);
    AddMethod('Expand', @TSepiImportsTCustomTreeView.Expand,
      'procedure(Node: TTreeNode)',
      mlkDynamic);
    AddMethod('GetDragImages', @TSepiImportsTCustomTreeView.GetDragImages,
      'function: TDragImageList',
      mlkOverride);
    AddMethod('GetImageIndex', @TSepiImportsTCustomTreeView.GetImageIndex,
      'procedure(Node: TTreeNode)',
      mlkVirtual);
    AddMethod('GetSelectedIndex',
      @TSepiImportsTCustomTreeView.GetSelectedIndex,
      'procedure(Node: TTreeNode)',
      mlkVirtual);
    AddMethod('IsCustomDrawn', @TSepiImportsTCustomTreeView.IsCustomDrawn,
      'function(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean',
      mlkVirtual);
    AddMethod('Loaded', @TSepiImportsTCustomTreeView.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTCustomTreeView.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation )',
      mlkOverride);
    AddMethod('SetDragMode', @TSepiImportsTCustomTreeView.SetDragMode,
      'procedure(Value: TDragMode)',
      mlkOverride);
    AddMethod('WndProc', @TSepiImportsTCustomTreeView.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);
    AddMethod('ValidateSelection',
      @TSepiImportsTCustomTreeView.ValidateSelection,
      'procedure');
    AddMethod('InvalidateSelectionsRects',
      @TSepiImportsTCustomTreeView.InvalidateSelectionsRects,
      'procedure');
    AddMethod('MouseDown', @TSepiImportsTCustomTreeView.MouseDown,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer)',
      mlkOverride);
    AddMethod('DoEnter', @TSepiImportsTCustomTreeView.DoEnter,
      'procedure',
      mlkOverride);
    AddMethod('DoExit', @TSepiImportsTCustomTreeView.DoExit,
      'procedure',
      mlkOverride);

    AddProperty('AutoExpand', 'property: Boolean',
      'FAutoExpand', 'SetAutoExpand',
      NoIndex, Integer(False));
    AddProperty('BorderStyle', 'property: TBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(bsSingle));
    AddProperty('ChangeDelay', 'property: Integer',
      'GetChangeDelay', 'SetChangeDelay',
      NoIndex, 0);
    AddProperty('CreateWndRestores', 'property: Boolean',
      'FCreateWndRestores', 'FCreateWndRestores',
      NoIndex, Integer(True));
    AddProperty('HideSelection', 'property: Boolean',
      'FHideSelection', 'SetHideSelection',
      NoIndex, Integer(True));
    AddProperty('HotTrack', 'property: Boolean',
      'FHotTrack', 'SetHotTrack',
      NoIndex, Integer(False));
    AddProperty('Images', 'property: TCustomImageList',
      'FImages', 'SetImages');
    AddProperty('Indent', 'property: Integer',
      'GetIndent', 'SetIndent');
    AddProperty('Items', 'property: TTreeNodes',
      'FTreeNodes', 'SetTreeNodes');
    AddProperty('MultiSelect', 'property: Boolean',
      'FMultiSelect', 'SetMultiSelect',
      NoIndex, Integer(False));
    AddProperty('MultiSelectStyle', 'property: TMultiSelectStyle',
      'FMultiSelectStyle', 'SetMultiSelectStyle',
      NoIndex, Byte(DefaultMultiSelectStyle));
    AddProperty('ReadOnly', 'property: Boolean',
      'FReadOnly', 'SetReadOnly',
      NoIndex, Integer(False));
    AddProperty('RightClickSelect', 'property: Boolean',
      'FRightClickSelect', 'FRightClickSelect',
      NoIndex, Integer(False));
    AddProperty('RowSelect', 'property: Boolean',
      'FRowSelect', 'SetRowSelect',
      NoIndex, Integer(False));
    AddProperty('ShowButtons', 'property: Boolean',
      'FShowButtons', 'SetButtonStyle',
      NoIndex, Integer(True));
    AddProperty('ShowLines', 'property: Boolean',
      'FShowLines', 'SetLineStyle',
      NoIndex, Integer(True));
    AddProperty('ShowRoot', 'property: Boolean',
      'FShowRoot', 'SetRootStyle',
      NoIndex, Integer(True));
    AddProperty('SortType', 'property: TSortType',
      'FSortType', 'SetSortType',
      NoIndex, Integer(stNone));
    AddProperty('StateImages', 'property: TCustomImageList',
      'FStateImages', 'SetStateImages');
    AddProperty('ToolTips', 'property: Boolean',
      'FToolTips', 'SetToolTips',
      NoIndex, Integer(True));
    AddProperty('OnAddition', 'property: TTVExpandedEvent',
      'FOnAddition', 'FOnAddition');
    AddProperty('OnAdvancedCustomDraw', 'property: TTVAdvancedCustomDrawEvent',
      'FOnAdvancedCustomDraw', 'FOnAdvancedCustomDraw');
    AddProperty('OnAdvancedCustomDrawItem',
      'property: TTVAdvancedCustomDrawItemEvent',
      'FOnAdvancedCustomDrawItem', 'FOnAdvancedCustomDrawItem');
    AddProperty('OnCancelEdit', 'property: TTVChangedEvent',
      'FOnCancelEdit', 'FOnCancelEdit');
    AddProperty('OnChange', 'property: TTVChangedEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OnChanging', 'property: TTVChangingEvent',
      'FOnChanging', 'FOnChanging');
    AddProperty('OnCollapsed', 'property: TTVExpandedEvent',
      'FOnCollapsed', 'FOnCollapsed');
    AddProperty('OnCollapsing', 'property: TTVCollapsingEvent',
      'FOnCollapsing', 'FOnCollapsing');
    AddProperty('OnCompare', 'property: TTVCompareEvent',
      'FOnCompare', 'FOnCompare');
    AddProperty('OnCustomDraw', 'property: TTVCustomDrawEvent',
      'FOnCustomDraw', 'FOnCustomDraw');
    AddProperty('OnCustomDrawItem', 'property: TTVCustomDrawItemEvent',
      'FOnCustomDrawItem', 'FOnCustomDrawItem');
    AddProperty('OnDeletion', 'property: TTVExpandedEvent',
      'FOnDeletion', 'FOnDeletion');
    AddProperty('OnEditing', 'property: TTVEditingEvent',
      'FOnEditing', 'FOnEditing');
    AddProperty('OnEdited', 'property: TTVEditedEvent',
      'FOnEdited', 'FOnEdited');
    AddProperty('OnExpanding', 'property: TTVExpandingEvent',
      'FOnExpanding', 'FOnExpanding');
    AddProperty('OnExpanded', 'property: TTVExpandedEvent',
      'FOnExpanded', 'FOnExpanded');
    AddProperty('OnGetImageIndex', 'property: TTVExpandedEvent',
      'FOnGetImageIndex', 'FOnGetImageIndex');
    AddProperty('OnGetSelectedIndex', 'property: TTVExpandedEvent',
      'FOnGetSelectedIndex', 'FOnGetSelectedIndex');
    AddProperty('OnCreateNodeClass', 'property: TTVCreateNodeClassEvent',
      'FOnCreateNodeClass', 'FOnCreateNodeClass');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomTreeView.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomTreeView.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AlphaSort', @TSepiImportsTCustomTreeView.AlphaSort,
      'function(ARecurse: Boolean = True): Boolean');
    AddMethod('CustomSort', @TSepiImportsTCustomTreeView.CustomSort,
      'function(SortProc: TTVCompare; Data: Longint; ARecurse: Boolean = True): Boolean');
    AddMethod('FullCollapse', @TSepiImportsTCustomTreeView.FullCollapse,
      'procedure');
    AddMethod('FullExpand', @TSepiImportsTCustomTreeView.FullExpand,
      'procedure');
    AddMethod('GetHitTestInfoAt',
      @TSepiImportsTCustomTreeView.GetHitTestInfoAt,
      'function(X, Y: Integer): THitTests');
    AddMethod('GetNodeAt', @TSepiImportsTCustomTreeView.GetNodeAt,
      'function(X, Y: Integer): TTreeNode');
    AddMethod('IsEditing', @TSepiImportsTCustomTreeView.IsEditing,
      'function: Boolean');
    AddMethod('LoadFromFile', @TSepiImportsTCustomTreeView.LoadFromFile,
      'procedure(const FileName: string)');
    AddMethod('LoadFromStream', @TSepiImportsTCustomTreeView.LoadFromStream,
      'procedure(Stream: TStream)');
    AddMethod('SaveToFile', @TSepiImportsTCustomTreeView.SaveToFile,
      'procedure(const FileName: string)');
    AddMethod('SaveToStream', @TSepiImportsTCustomTreeView.SaveToStream,
      'procedure(Stream: TStream)');

    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');
    AddProperty('DropTarget', 'property: TTreeNode',
      'GetDropTarget', 'SetDropTarget');
    AddProperty('Selected', 'property: TTreeNode',
      'GetSelected', 'SetSelected');
    AddProperty('TopItem', 'property: TTreeNode',
      'GetTopItem', 'SetTopItem');

    AddOverloadedMethod('Select', nil,
      'procedure(Node: TTreeNode; ShiftState: TShiftState = [])',
      mlkVirtual);
    AddOverloadedMethod('Select', nil,
      'procedure(const Nodes: array of TTreeNode)',
      mlkVirtual);
    AddOverloadedMethod('Select', nil,
      'procedure(Nodes: TList)',
      mlkVirtual);
    AddMethod('Deselect', @TSepiImportsTCustomTreeView.Deselect,
      'procedure(Node: TTreeNode)',
      mlkVirtual);
    AddMethod('Subselect', @TSepiImportsTCustomTreeView.Subselect,
      'procedure(Node: TTreeNode; Validate: Boolean = False)',
      mlkVirtual);

    AddProperty('SelectionCount', 'property: Cardinal',
      'GetSelectionCount', '');
    AddProperty('Selections', 'property[Index: Integer]: TTreeNode',
      'GetSelection', '');

    AddMethod('ClearSelection', @TSepiImportsTCustomTreeView.ClearSelection,
      'procedure(KeepPrimary: Boolean = False)',
      mlkVirtual);
    AddMethod('GetSelections', @TSepiImportsTCustomTreeView.GetSelections,
      'function(AList: TList): TTreeNode');
    AddMethod('FindNextToSelect',
      @TSepiImportsTCustomTreeView.FindNextToSelect,
      'function: TTreeNode',
      mlkVirtual);

    Complete;
  end;
end;

{------------------}
{ TTreeView import }
{------------------}

class function TSepiImportsTTreeView.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTreeView));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('AutoExpand');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelOuter');
    RedefineProperty('BevelKind',
      '', '', Integer(bkNone));
    RedefineProperty('BevelWidth');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderStyle');
    RedefineProperty('BorderWidth');
    RedefineProperty('ChangeDelay');
    RedefineProperty('Color');
    RedefineProperty('Ctl3D');
    RedefineProperty('Constraints');
    RedefineProperty('DragKind');
    RedefineProperty('DragCursor');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('HideSelection');
    RedefineProperty('HotTrack');
    RedefineProperty('Images');
    RedefineProperty('Indent');
    RedefineProperty('MultiSelect');
    RedefineProperty('MultiSelectStyle');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor',
      '', '', Integer(False));
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ReadOnly');
    RedefineProperty('RightClickSelect');
    RedefineProperty('RowSelect');
    RedefineProperty('ShowButtons');
    RedefineProperty('ShowHint');
    RedefineProperty('ShowLines');
    RedefineProperty('ShowRoot');
    RedefineProperty('SortType');
    RedefineProperty('StateImages');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop',
      '', '', Integer(True));
    RedefineProperty('ToolTips');
    RedefineProperty('Visible');
    RedefineProperty('OnAddition');
    RedefineProperty('OnAdvancedCustomDraw');
    RedefineProperty('OnAdvancedCustomDrawItem');
    RedefineProperty('OnChange');
    RedefineProperty('OnChanging');
    RedefineProperty('OnClick');
    RedefineProperty('OnCollapsed');
    RedefineProperty('OnCollapsing');
    RedefineProperty('OnCompare');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnCreateNodeClass');
    RedefineProperty('OnCustomDraw');
    RedefineProperty('OnCustomDrawItem');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDeletion');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEdited');
    RedefineProperty('OnEditing');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnExpanding');
    RedefineProperty('OnExpanded');
    RedefineProperty('OnGetImageIndex');
    RedefineProperty('OnGetSelectedIndex');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('Items');

    Complete;
  end;
end;

{------------------}
{ TTrackBar import }
{------------------}

function TSepiImportsTTrackBar.GetThumbLength: Integer;
begin
  Result := ThumbLength;
end;

procedure TSepiImportsTTrackBar.SetOrientation(Value: TTrackBarOrientation);
begin
  Orientation := Value;
end;

procedure TSepiImportsTTrackBar.SetPosition(Value: Integer);
begin
  Position := Value;
end;

procedure TSepiImportsTTrackBar.SetMin(Value: Integer);
begin
  Min := Value;
end;

procedure TSepiImportsTTrackBar.SetMax(Value: Integer);
begin
  Max := Value;
end;

procedure TSepiImportsTTrackBar.SetFrequency(Value: Integer);
begin
  Frequency := Value;
end;

procedure TSepiImportsTTrackBar.SetTickStyle(Value: TTickStyle);
begin
  TickStyle := Value;
end;

procedure TSepiImportsTTrackBar.SetTickMarks(Value: TTickMark);
begin
  TickMarks := Value;
end;

procedure TSepiImportsTTrackBar.SetLineSize(Value: Integer);
begin
  LineSize := Value;
end;

procedure TSepiImportsTTrackBar.SetPageSize(Value: Integer);
begin
  PageSize := Value;
end;

procedure TSepiImportsTTrackBar.SetThumbLength(Value: Integer);
begin
  ThumbLength := Value;
end;

procedure TSepiImportsTTrackBar.SetSliderVisible(Value: Boolean);
begin
  SliderVisible := Value;
end;

procedure TSepiImportsTTrackBar.SetSelStart(Value: Integer);
begin
  SelStart := Value;
end;

procedure TSepiImportsTTrackBar.SetSelEnd(Value: Integer);
begin
  SelEnd := Value;
end;

class function TSepiImportsTTrackBar.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTrackBar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOrientation', System.TypeInfo(TTrackBarOrientation));
    AddField('FTickMarks', System.TypeInfo(TTickMark));
    AddField('FTickStyle', System.TypeInfo(TTickStyle));
    AddField('FLineSize', System.TypeInfo(Integer));
    AddField('FPageSize', System.TypeInfo(Integer));
    AddField('FThumbLength', System.TypeInfo(Integer));
    AddField('FSliderVisible', System.TypeInfo(Boolean));
    AddField('FMin', System.TypeInfo(Integer));
    AddField('FMax', System.TypeInfo(Integer));
    AddField('FFrequency', System.TypeInfo(Integer));
    AddField('FPosition', System.TypeInfo(Integer));
    AddField('FSelStart', System.TypeInfo(Integer));
    AddField('FSelEnd', System.TypeInfo(Integer));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));

    AddMethod('GetThumbLength', @TSepiImportsTTrackBar.GetThumbLength,
      'function: Integer');
    AddMethod('SetOrientation', @TSepiImportsTTrackBar.SetOrientation,
      'procedure(Value: TTrackBarOrientation)');
    AddMethod('SetParams', nil,
      'procedure(APosition, AMin, AMax: Integer)');
    AddMethod('SetPosition', @TSepiImportsTTrackBar.SetPosition,
      'procedure(Value: Integer)');
    AddMethod('SetMin', @TSepiImportsTTrackBar.SetMin,
      'procedure(Value: Integer)');
    AddMethod('SetMax', @TSepiImportsTTrackBar.SetMax,
      'procedure(Value: Integer)');
    AddMethod('SetFrequency', @TSepiImportsTTrackBar.SetFrequency,
      'procedure(Value: Integer)');
    AddMethod('SetTickStyle', @TSepiImportsTTrackBar.SetTickStyle,
      'procedure(Value: TTickStyle)');
    AddMethod('SetTickMarks', @TSepiImportsTTrackBar.SetTickMarks,
      'procedure(Value: TTickMark)');
    AddMethod('SetLineSize', @TSepiImportsTTrackBar.SetLineSize,
      'procedure(Value: Integer)');
    AddMethod('SetPageSize', @TSepiImportsTTrackBar.SetPageSize,
      'procedure(Value: Integer)');
    AddMethod('SetThumbLength', @TSepiImportsTTrackBar.SetThumbLength,
      'procedure(Value: Integer)');
    AddMethod('SetSliderVisible', @TSepiImportsTTrackBar.SetSliderVisible,
      'procedure(Value: Boolean)');
    AddMethod('SetSelStart', @TSepiImportsTTrackBar.SetSelStart,
      'procedure(Value: Integer)');
    AddMethod('SetSelEnd', @TSepiImportsTTrackBar.SetSelEnd,
      'procedure(Value: Integer)');
    AddMethod('UpdateSelection', nil,
      'procedure');
    AddMethod('CNHScroll', nil,
      'procedure(var Message: TWMHScroll)',
      mlkMessage, False, CN_HSCROLL);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('CNVScroll', nil,
      'procedure(var Message: TWMVScroll)',
      mlkMessage, False, CN_VSCROLL);
    AddMethod('WMEraseBkGnd', nil,
      'procedure(var Message: TWMEraseBkGnd)',
      mlkMessage, False, WM_ERASEBKGND);

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTTrackBar.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTTrackBar.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWnd', @TSepiImportsTTrackBar.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('Changed', @TSepiImportsTTrackBar.Changed,
      'procedure',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTTrackBar.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('SetTick', @TSepiImportsTTrackBar.SetTick,
      'procedure(Value: Integer)');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('BorderWidth');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Constraints');
    AddProperty('LineSize', 'property: Integer',
      'FLineSize', 'SetLineSize',
      NoIndex, 1);
    AddProperty('Max', 'property: Integer',
      'FMax', 'SetMax',
      NoIndex, 10);
    AddProperty('Min', 'property: Integer',
      'FMin', 'SetMin',
      NoIndex, 0);
    AddProperty('Orientation', 'property: TTrackBarOrientation',
      'FOrientation', 'SetOrientation',
      NoIndex, Integer(trHorizontal));
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentShowHint');
    AddProperty('PageSize', 'property: Integer',
      'FPageSize', 'SetPageSize',
      NoIndex, 2);
    RedefineProperty('PopupMenu');
    AddProperty('Frequency', 'property: Integer',
      'FFrequency', 'SetFrequency',
      NoIndex, 1);
    AddProperty('Position', 'property: Integer',
      'FPosition', 'SetPosition',
      NoIndex, 0);
    AddProperty('SliderVisible', 'property: Boolean',
      'FSliderVisible', 'SetSliderVisible',
      NoIndex, Integer(True));
    AddProperty('SelEnd', 'property: Integer',
      'FSelEnd', 'SetSelEnd',
      NoIndex, 0);
    AddProperty('SelStart', 'property: Integer',
      'FSelStart', 'SetSelStart',
      NoIndex, 0);
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop',
      '', '', Integer(True));
    AddProperty('ThumbLength', 'property: Integer',
      'GetThumbLength', 'SetThumbLength',
      NoIndex, 20);
    AddProperty('TickMarks', 'property: TTickMark',
      'FTickMarks', 'SetTickMarks',
      NoIndex, Integer(tmBottomRight));
    AddProperty('TickStyle', 'property: TTickStyle',
      'FTickStyle', 'SetTickStyle',
      NoIndex, Integer(tsAuto));
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
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{---------------------}
{ TProgressBar import }
{---------------------}

function TSepiImportsTProgressBar.GetMin: Integer;
begin
  Result := Min;
end;

function TSepiImportsTProgressBar.GetMax: Integer;
begin
  Result := Max;
end;

function TSepiImportsTProgressBar.GetPosition: Integer;
begin
  Result := Position;
end;

procedure TSepiImportsTProgressBar.SetMin(Value: Integer);
begin
  Min := Value;
end;

procedure TSepiImportsTProgressBar.SetMax(Value: Integer);
begin
  Max := Value;
end;

procedure TSepiImportsTProgressBar.SetPosition(Value: Integer);
begin
  Position := Value;
end;

procedure TSepiImportsTProgressBar.SetStep(Value: Integer);
begin
  Step := Value;
end;

procedure TSepiImportsTProgressBar.SetOrientation(
  Value: TProgressBarOrientation);
begin
  Orientation := Value;
end;

procedure TSepiImportsTProgressBar.SetSmooth(Value: Boolean);
begin
  Smooth := Value;
end;

class function TSepiImportsTProgressBar.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TProgressBar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('F32BitMode', System.TypeInfo(Boolean));
    AddField('FMin', System.TypeInfo(Integer));
    AddField('FMax', System.TypeInfo(Integer));
    AddField('FPosition', System.TypeInfo(Integer));
    AddField('FStep', System.TypeInfo(Integer));
    AddField('FOrientation', System.TypeInfo(TProgressBarOrientation));
    AddField('FSmooth', System.TypeInfo(Boolean));

    AddMethod('GetMin', @TSepiImportsTProgressBar.GetMin,
      'function: Integer');
    AddMethod('GetMax', @TSepiImportsTProgressBar.GetMax,
      'function: Integer');
    AddMethod('GetPosition', @TSepiImportsTProgressBar.GetPosition,
      'function: Integer');
    AddMethod('SetParams', nil,
      'procedure(AMin, AMax: Integer)');
    AddMethod('SetMin', @TSepiImportsTProgressBar.SetMin,
      'procedure(Value: Integer)');
    AddMethod('SetMax', @TSepiImportsTProgressBar.SetMax,
      'procedure(Value: Integer)');
    AddMethod('SetPosition', @TSepiImportsTProgressBar.SetPosition,
      'procedure(Value: Integer)');
    AddMethod('SetStep', @TSepiImportsTProgressBar.SetStep,
      'procedure(Value: Integer)');
    AddMethod('SetOrientation', @TSepiImportsTProgressBar.SetOrientation,
      'procedure(Value: TProgressBarOrientation)');
    AddMethod('SetSmooth', @TSepiImportsTProgressBar.SetSmooth,
      'procedure(Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTProgressBar.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTProgressBar.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWnd', @TSepiImportsTProgressBar.DestroyWnd,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTProgressBar.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('StepIt', @TSepiImportsTProgressBar.StepIt,
      'procedure');
    AddMethod('StepBy', @TSepiImportsTProgressBar.StepBy,
      'procedure(Delta: Integer)');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('BorderWidth');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Hint');
    RedefineProperty('Constraints');
    AddProperty('Min', 'property: Integer',
      'GetMin', 'SetMin',
      NoIndex, 0);
    AddProperty('Max', 'property: Integer',
      'GetMax', 'SetMax',
      NoIndex, 100);
    AddProperty('Orientation', 'property: TProgressBarOrientation',
      'FOrientation', 'SetOrientation',
      NoIndex, Integer(pbHorizontal));
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    AddProperty('Position', 'property: Integer',
      'GetPosition', 'SetPosition',
      NoIndex, 0);
    AddProperty('Smooth', 'property: Boolean',
      'FSmooth', 'SetSmooth',
      NoIndex, Integer(False));
    AddProperty('Step', 'property: Integer',
      'FStep', 'SetStep',
      NoIndex, 10);
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
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
{ TTextAttributes import }
{------------------------}

function TSepiImportsTTextAttributes.GetCharset: TFontCharset;
begin
  Result := Charset;
end;

function TSepiImportsTTextAttributes.GetColor: TColor;
begin
  Result := Color;
end;

function TSepiImportsTTextAttributes.GetConsistentAttributes:
TConsistentAttributes;
begin
  Result := ConsistentAttributes;
end;

function TSepiImportsTTextAttributes.GetHeight: Integer;
begin
  Result := Height;
end;

function TSepiImportsTTextAttributes.GetName: TFontName;
begin
  Result := Name;
end;

function TSepiImportsTTextAttributes.GetPitch: TFontPitch;
begin
  Result := Pitch;
end;

function TSepiImportsTTextAttributes.GetProtected: Boolean;
begin
  Result := Protected;
end;

function TSepiImportsTTextAttributes.GetSize: Integer;
begin
  Result := Size;
end;

function TSepiImportsTTextAttributes.GetStyle: TFontStyles;
begin
  Result := Style;
end;

procedure TSepiImportsTTextAttributes.SetCharset(Value: TFontCharset);
begin
  Charset := Value;
end;

procedure TSepiImportsTTextAttributes.SetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TSepiImportsTTextAttributes.SetHeight(Value: Integer);
begin
  Height := Value;
end;

procedure TSepiImportsTTextAttributes.SetName(Value: TFontName);
begin
  Name := Value;
end;

procedure TSepiImportsTTextAttributes.SetPitch(Value: TFontPitch);
begin
  Pitch := Value;
end;

procedure TSepiImportsTTextAttributes.SetProtected(Value: Boolean);
begin
  Protected := Value;
end;

procedure TSepiImportsTTextAttributes.SetSize(Value: Integer);
begin
  Size := Value;
end;

procedure TSepiImportsTTextAttributes.SetStyle(Value: TFontStyles);
begin
  Style := Value;
end;

class function TSepiImportsTTextAttributes.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTextAttributes));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('RichEdit', System.TypeInfo(TCustomRichEdit));
    AddField('FType', System.TypeInfo(TAttributeType));

    AddMethod('GetAttributes', nil,
      'procedure(var Format: TCharFormat)');
    AddMethod('GetCharset', @TSepiImportsTTextAttributes.GetCharset,
      'function: TFontCharset');
    AddMethod('GetColor', @TSepiImportsTTextAttributes.GetColor,
      'function: TColor');
    AddMethod('GetConsistentAttributes',
      @TSepiImportsTTextAttributes.GetConsistentAttributes,
      'function: TConsistentAttributes');
    AddMethod('GetHeight', @TSepiImportsTTextAttributes.GetHeight,
      'function: Integer');
    AddMethod('GetName', @TSepiImportsTTextAttributes.GetName,
      'function: TFontName');
    AddMethod('GetPitch', @TSepiImportsTTextAttributes.GetPitch,
      'function: TFontPitch');
    AddMethod('GetProtected', @TSepiImportsTTextAttributes.GetProtected,
      'function: Boolean');
    AddMethod('GetSize', @TSepiImportsTTextAttributes.GetSize,
      'function: Integer');
    AddMethod('GetStyle', @TSepiImportsTTextAttributes.GetStyle,
      'function: TFontStyles');
    AddMethod('SetAttributes', nil,
      'procedure(var Format: TCharFormat)');
    AddMethod('SetCharset', @TSepiImportsTTextAttributes.SetCharset,
      'procedure(Value: TFontCharset)');
    AddMethod('SetColor', @TSepiImportsTTextAttributes.SetColor,
      'procedure(Value: TColor)');
    AddMethod('SetHeight', @TSepiImportsTTextAttributes.SetHeight,
      'procedure(Value: Integer)');
    AddMethod('SetName', @TSepiImportsTTextAttributes.SetName,
      'procedure(Value: TFontName)');
    AddMethod('SetPitch', @TSepiImportsTTextAttributes.SetPitch,
      'procedure(Value: TFontPitch)');
    AddMethod('SetProtected', @TSepiImportsTTextAttributes.SetProtected,
      'procedure(Value: Boolean)');
    AddMethod('SetSize', @TSepiImportsTTextAttributes.SetSize,
      'procedure(Value: Integer)');
    AddMethod('SetStyle', @TSepiImportsTTextAttributes.SetStyle,
      'procedure(Value: TFontStyles)');

    CurrentVisibility := mvProtected;

    AddMethod('InitFormat', @TSepiImportsTTextAttributes.InitFormat,
      'procedure(var Format: TCharFormat)');
    AddMethod('AssignTo', @TSepiImportsTTextAttributes.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTTextAttributes.Create,
      'constructor(AOwner: TCustomRichEdit; AttributeType: TAttributeType)');
    AddMethod('Assign', @TSepiImportsTTextAttributes.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    AddProperty('Charset', 'property: TFontCharset',
      'GetCharset', 'SetCharset');
    AddProperty('Color', 'property: TColor',
      'GetColor', 'SetColor');
    AddProperty('ConsistentAttributes', 'property: TConsistentAttributes',
      'GetConsistentAttributes', '');
    AddProperty('Name', 'property: TFontName',
      'GetName', 'SetName');
    AddProperty('Pitch', 'property: TFontPitch',
      'GetPitch', 'SetPitch');
    AddProperty('Protected', 'property: Boolean',
      'GetProtected', 'SetProtected');
    AddProperty('Size', 'property: Integer',
      'GetSize', 'SetSize');
    AddProperty('Style', 'property: TFontStyles',
      'GetStyle', 'SetStyle');
    AddProperty('Height', 'property: Integer',
      'GetHeight', 'SetHeight');

    Complete;
  end;
end;

{------------------------}
{ TParaAttributes import }
{------------------------}

function TSepiImportsTParaAttributes.GetAlignment: TAlignment;
begin
  Result := Alignment;
end;

function TSepiImportsTParaAttributes.GetFirstIndent: Longint;
begin
  Result := FirstIndent;
end;

function TSepiImportsTParaAttributes.GetLeftIndent: Longint;
begin
  Result := LeftIndent;
end;

function TSepiImportsTParaAttributes.GetRightIndent: Longint;
begin
  Result := RightIndent;
end;

function TSepiImportsTParaAttributes.GetNumbering: TNumberingStyle;
begin
  Result := Numbering;
end;

function TSepiImportsTParaAttributes.GetTab(Index: Byte): Longint;
begin
  Result := Tab[Index];
end;

function TSepiImportsTParaAttributes.GetTabCount: Integer;
begin
  Result := TabCount;
end;

procedure TSepiImportsTParaAttributes.SetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TSepiImportsTParaAttributes.SetFirstIndent(Value: Longint);
begin
  FirstIndent := Value;
end;

procedure TSepiImportsTParaAttributes.SetLeftIndent(Value: Longint);
begin
  LeftIndent := Value;
end;

procedure TSepiImportsTParaAttributes.SetRightIndent(Value: Longint);
begin
  RightIndent := Value;
end;

procedure TSepiImportsTParaAttributes.SetNumbering(Value: TNumberingStyle);
begin
  Numbering := Value;
end;

procedure TSepiImportsTParaAttributes.SetTab(Index: Byte; Value: Longint);
begin
  Tab[Index] := Value;
end;

procedure TSepiImportsTParaAttributes.SetTabCount(Value: Integer);
begin
  TabCount := Value;
end;

class function TSepiImportsTParaAttributes.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TParaAttributes));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('RichEdit', System.TypeInfo(TCustomRichEdit));

    AddMethod('GetAttributes', nil,
      'procedure(var Paragraph: TParaFormat)');
    AddMethod('GetAlignment', @TSepiImportsTParaAttributes.GetAlignment,
      'function: TAlignment');
    AddMethod('GetFirstIndent', @TSepiImportsTParaAttributes.GetFirstIndent,
      'function: Longint');
    AddMethod('GetLeftIndent', @TSepiImportsTParaAttributes.GetLeftIndent,
      'function: Longint');
    AddMethod('GetRightIndent', @TSepiImportsTParaAttributes.GetRightIndent,
      'function: Longint');
    AddMethod('GetNumbering', @TSepiImportsTParaAttributes.GetNumbering,
      'function: TNumberingStyle');
    AddMethod('GetTab', @TSepiImportsTParaAttributes.GetTab,
      'function(Index: Byte): Longint');
    AddMethod('GetTabCount', @TSepiImportsTParaAttributes.GetTabCount,
      'function: Integer');
    AddMethod('InitPara', nil,
      'procedure(var Paragraph: TParaFormat)');
    AddMethod('SetAlignment', @TSepiImportsTParaAttributes.SetAlignment,
      'procedure(Value: TAlignment)');
    AddMethod('SetAttributes', nil,
      'procedure(var Paragraph: TParaFormat)');
    AddMethod('SetFirstIndent', @TSepiImportsTParaAttributes.SetFirstIndent,
      'procedure(Value: Longint)');
    AddMethod('SetLeftIndent', @TSepiImportsTParaAttributes.SetLeftIndent,
      'procedure(Value: Longint)');
    AddMethod('SetRightIndent', @TSepiImportsTParaAttributes.SetRightIndent,
      'procedure(Value: Longint)');
    AddMethod('SetNumbering', @TSepiImportsTParaAttributes.SetNumbering,
      'procedure(Value: TNumberingStyle)');
    AddMethod('SetTab', @TSepiImportsTParaAttributes.SetTab,
      'procedure(Index: Byte; Value: Longint)');
    AddMethod('SetTabCount', @TSepiImportsTParaAttributes.SetTabCount,
      'procedure(Value: Integer)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTParaAttributes.Create,
      'constructor(AOwner: TCustomRichEdit)');
    AddMethod('Assign', @TSepiImportsTParaAttributes.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    AddProperty('Alignment', 'property: TAlignment',
      'GetAlignment', 'SetAlignment');
    AddProperty('FirstIndent', 'property: Longint',
      'GetFirstIndent', 'SetFirstIndent');
    AddProperty('LeftIndent', 'property: Longint',
      'GetLeftIndent', 'SetLeftIndent');
    AddProperty('Numbering', 'property: TNumberingStyle',
      'GetNumbering', 'SetNumbering');
    AddProperty('RightIndent', 'property: Longint',
      'GetRightIndent', 'SetRightIndent');
    AddProperty('Tab', 'property[Index: Byte]: Longint',
      'GetTab', 'SetTab');
    AddProperty('TabCount', 'property: Integer',
      'GetTabCount', 'SetTabCount');

    Complete;
  end;
end;

{--------------------}
{ TConversion import }
{--------------------}

class function TSepiImportsTConversion.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TConversion));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('ConvertReadStream', @TSepiImportsTConversion.ConvertReadStream,
      'function(Stream: TStream; Buffer: PChar; BufSize: Integer): Integer',
      mlkVirtual);
    AddMethod('ConvertWriteStream',
      @TSepiImportsTConversion.ConvertWriteStream,
      'function(Stream: TStream; Buffer: PChar; BufSize: Integer): Integer',
      mlkVirtual);

    Complete;
  end;
end;

{--------------------------}
{ TConversionFormat import }
{--------------------------}

function SepiImportTConversionFormat(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TConversionFormat', False, True,
    TypeInfo(TConversionFormat));

  with Result do
  begin
    AddField('ConversionClass', 'TConversionClass');
    AddField('Extension', System.TypeInfo(string));
    AddField('Next', 'PConversionFormat');

    Complete;
  end;
end;

{----------------------------}
{ TRichEditStreamInfo import }
{----------------------------}

function SepiImportTRichEditStreamInfo(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TRichEditStreamInfo', False, True);

  with Result do
  begin
    AddField('Converter', System.TypeInfo(TConversion));
    AddField('Stream', System.TypeInfo(TStream));

    Complete;
  end;
end;

{------------------------}
{ TCustomRichEdit import }
{------------------------}

function TSepiImportsTCustomRichEdit.GetPlainText: Boolean;
begin
  Result := PlainText;
end;

procedure TSepiImportsTCustomRichEdit.SetHideScrollBars(Value: Boolean);
begin
  HideScrollBars := Value;
end;

procedure TSepiImportsTCustomRichEdit.SetHideSelection(Value: Boolean);
begin
  HideSelection := Value;
end;

procedure TSepiImportsTCustomRichEdit.SetPlainText(Value: Boolean);
begin
  PlainText := Value;
end;

procedure TSepiImportsTCustomRichEdit.SetRichEditStrings(Value: TStrings);
begin
  Lines := Value;
end;

procedure TSepiImportsTCustomRichEdit.SetDefAttributes(Value: TTextAttributes);
begin
  DefAttributes := Value;
end;

procedure TSepiImportsTCustomRichEdit.SetSelAttributes(Value: TTextAttributes);
begin
  SelAttributes := Value;
end;

class function TSepiImportsTCustomRichEdit.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomRichEdit'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomRichEdit));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHideScrollBars', System.TypeInfo(Boolean));
    AddField('FSelAttributes', System.TypeInfo(TTextAttributes));
    AddField('FDefAttributes', System.TypeInfo(TTextAttributes));
    AddField('FParagraph', System.TypeInfo(TParaAttributes));
    AddField('FOldParaAlignment', System.TypeInfo(TAlignment));
    AddField('FScreenLogPixels', System.TypeInfo(Integer));
    AddField('FRichEditStrings', System.TypeInfo(TStrings));
    AddField('FMemStream', System.TypeInfo(TMemoryStream));
    AddField('FOnSelChange', System.TypeInfo(TNotifyEvent));
    AddField('FHideSelection', System.TypeInfo(Boolean));
    AddField('FModified', System.TypeInfo(Boolean));
    AddField('FDefaultConverter', 'TConversionClass');
    AddField('FOnResizeRequest', System.TypeInfo(TRichEditResizeEvent));
    AddField('FOnProtectChange', System.TypeInfo(TRichEditProtectChange));
    AddField('FOnSaveClipboard', System.TypeInfo(TRichEditSaveClipboard));
    AddField('FPageRect', 'TRect');

    AddMethod('CMBiDiModeChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BIDIMODECHANGED);
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('GetPlainText', @TSepiImportsTCustomRichEdit.GetPlainText,
      'function: Boolean');
    AddMethod('ProtectChange', nil,
      'function(StartPos, EndPos: Integer): Boolean');
    AddMethod('SaveClipboard', nil,
      'function(NumObj, NumChars: Integer): Boolean');
    AddMethod('SetHideScrollBars',
      @TSepiImportsTCustomRichEdit.SetHideScrollBars,
      'procedure(Value: Boolean)');
    AddMethod('SetHideSelection',
      @TSepiImportsTCustomRichEdit.SetHideSelection,
      'procedure(Value: Boolean)');
    AddMethod('SetPlainText', @TSepiImportsTCustomRichEdit.SetPlainText,
      'procedure(Value: Boolean)');
    AddMethod('SetRichEditStrings',
      @TSepiImportsTCustomRichEdit.SetRichEditStrings,
      'procedure(Value: TStrings)');
    AddMethod('SetDefAttributes',
      @TSepiImportsTCustomRichEdit.SetDefAttributes,
      'procedure(Value: TTextAttributes)');
    AddMethod('SetSelAttributes',
      @TSepiImportsTCustomRichEdit.SetSelAttributes,
      'procedure(Value: TTextAttributes)');
    AddMethod('WMNCDestroy', nil,
      'procedure(var Message: TWMNCDestroy)',
      mlkMessage, False, WM_NCDESTROY);
    AddMethod('WMSetCursor', nil,
      'procedure(var Message: TWMSetCursor)',
      mlkMessage, False, WM_SETCURSOR);
    AddMethod('WMPaint', nil,
      'procedure(var Message: TWMPaint)',
      mlkMessage, False, WM_PAINT);
    AddMethod('WMSetFont', nil,
      'procedure(var Message: TWMSetFont)',
      mlkMessage, False, WM_SETFONT);
    AddMethod('WMRButtonUp', nil,
      'procedure(var Message: TWMRButtonUp)',
      mlkMessage, False, WM_RBUTTONUP);

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTCustomRichEdit.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomRichEdit.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWnd', @TSepiImportsTCustomRichEdit.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('RequestSize', @TSepiImportsTCustomRichEdit.RequestSize,
      'procedure(const Rect: TRect)',
      mlkVirtual);
    AddMethod('SelectionChange', @TSepiImportsTCustomRichEdit.SelectionChange,
      'procedure',
      mlkDynamic);
    AddMethod('DoSetMaxLength', @TSepiImportsTCustomRichEdit.DoSetMaxLength,
      'procedure(Value: Integer)',
      mlkOverride);
    AddMethod('GetCaretPos', @TSepiImportsTCustomRichEdit.GetCaretPos,
      'function: TPoint',
      mlkOverride);
    AddMethod('SetCaretPos', @TSepiImportsTCustomRichEdit.SetCaretPos,
      'procedure(const Value: TPoint)',
      mlkOverride);
    AddMethod('GetSelLength', @TSepiImportsTCustomRichEdit.GetSelLength,
      'function: Integer',
      mlkOverride);
    AddMethod('GetSelStart', @TSepiImportsTCustomRichEdit.GetSelStart,
      'function: Integer',
      mlkOverride);
    AddMethod('GetSelText', @TSepiImportsTCustomRichEdit.GetSelText,
      'function: string',
      mlkOverride);
    AddMethod('SetSelLength', @TSepiImportsTCustomRichEdit.SetSelLength,
      'procedure(Value: Integer)',
      mlkOverride);
    AddMethod('SetSelStart', @TSepiImportsTCustomRichEdit.SetSelStart,
      'procedure(Value: Integer)',
      mlkOverride);

    AddProperty('HideSelection', 'property: Boolean',
      'FHideSelection', 'SetHideSelection',
      NoIndex, Integer(True));
    AddProperty('HideScrollBars', 'property: Boolean',
      'FHideScrollBars', 'SetHideScrollBars',
      NoIndex, Integer(True));
    AddProperty('Lines', 'property: TStrings',
      'FRichEditStrings', 'SetRichEditStrings');
    AddProperty('OnSaveClipboard', 'property: TRichEditSaveClipboard',
      'FOnSaveClipboard', 'FOnSaveClipboard');
    AddProperty('OnSelectionChange', 'property: TNotifyEvent',
      'FOnSelChange', 'FOnSelChange');
    AddProperty('OnProtectChange', 'property: TRichEditProtectChange',
      'FOnProtectChange', 'FOnProtectChange');
    AddProperty('OnResizeRequest', 'property: TRichEditResizeEvent',
      'FOnResizeRequest', 'FOnResizeRequest');
    AddProperty('PlainText', 'property: Boolean',
      'GetPlainText', 'SetPlainText',
      NoIndex, Integer(False));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomRichEdit.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomRichEdit.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTCustomRichEdit.Clear,
      'procedure',
      mlkOverride);
    AddMethod('FindText', @TSepiImportsTCustomRichEdit.FindText,
      'function(const SearchStr: string; StartPos, Length: Integer ; Options: TSearchTypes ) : Integer');
    AddMethod('GetSelTextBuf', @TSepiImportsTCustomRichEdit.GetSelTextBuf,
      'function(Buffer: PChar; BufSize: Integer): Integer',
      mlkOverride);
    AddMethod('Print', @TSepiImportsTCustomRichEdit.Print,
      'procedure(const Caption: string)',
      mlkVirtual);
    AddMethod('RegisterConversionFormat',
      @TSepiImportsTCustomRichEdit.RegisterConversionFormat,
      'class procedure(const AExtension: string; AConversionClass: TConversionClass )');

    AddProperty('DefaultConverter', 'property: TConversionClass',
      'FDefaultConverter', 'FDefaultConverter');
    AddProperty('DefAttributes', 'property: TTextAttributes',
      'FDefAttributes', 'SetDefAttributes');
    AddProperty('SelAttributes', 'property: TTextAttributes',
      'FSelAttributes', 'SetSelAttributes');
    AddProperty('PageRect', 'property: TRect',
      'FPageRect', 'FPageRect');
    AddProperty('Paragraph', 'property: TParaAttributes',
      'FParagraph', '');

    Complete;
  end;
end;

{------------------}
{ TRichEdit import }
{------------------}

class function TSepiImportsTRichEdit.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TRichEdit));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Alignment');
    RedefineProperty('Anchors');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelOuter');
    RedefineProperty('BevelKind',
      '', '', Integer(bkNone));
    RedefineProperty('BevelWidth');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderStyle');
    RedefineProperty('BorderWidth');
    RedefineProperty('Color');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('HideSelection');
    RedefineProperty('HideScrollBars');
    RedefineProperty('ImeMode');
    RedefineProperty('ImeName');
    RedefineProperty('Constraints');
    RedefineProperty('Lines');
    RedefineProperty('MaxLength');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PlainText');
    RedefineProperty('PopupMenu');
    RedefineProperty('ReadOnly');
    RedefineProperty('ScrollBars');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop',
      '', '', Integer(True));
    RedefineProperty('Visible');
    RedefineProperty('WantTabs');
    RedefineProperty('WantReturns');
    RedefineProperty('WordWrap');
    RedefineProperty('OnChange');
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
    RedefineProperty('OnMouseWheel');
    RedefineProperty('OnMouseWheelDown');
    RedefineProperty('OnMouseWheelUp');
    RedefineProperty('OnProtectChange');
    RedefineProperty('OnResizeRequest');
    RedefineProperty('OnSaveClipboard');
    RedefineProperty('OnSelectionChange');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{----------------------}
{ TCustomUpDown import }
{----------------------}

procedure TSepiImportsTCustomUpDown.SetAssociate(Value: TWinControl);
begin
  Associate := Value;
end;

function TSepiImportsTCustomUpDown.GetPosition: Smallint;
begin
  Result := Position;
end;

procedure TSepiImportsTCustomUpDown.SetMin(Value: Smallint);
begin
  Min := Value;
end;

procedure TSepiImportsTCustomUpDown.SetMax(Value: Smallint);
begin
  Max := Value;
end;

procedure TSepiImportsTCustomUpDown.SetIncrement(Value: Integer);
begin
  Increment := Value;
end;

procedure TSepiImportsTCustomUpDown.SetPosition(Value: Smallint);
begin
  Position := Value;
end;

procedure TSepiImportsTCustomUpDown.SetAlignButton(Value: TUDAlignButton);
begin
  AlignButton := Value;
end;

procedure TSepiImportsTCustomUpDown.SetOrientation(Value: TUDOrientation);
begin
  Orientation := Value;
end;

procedure TSepiImportsTCustomUpDown.SetArrowKeys(Value: Boolean);
begin
  ArrowKeys := Value;
end;

procedure TSepiImportsTCustomUpDown.SetThousands(Value: Boolean);
begin
  Thousands := Value;
end;

procedure TSepiImportsTCustomUpDown.SetWrap(Value: Boolean);
begin
  Wrap := Value;
end;

class function TSepiImportsTCustomUpDown.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomUpDown));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FArrowKeys', System.TypeInfo(Boolean));
    AddField('FAssociate', System.TypeInfo(TWinControl));
    AddField('FMin', System.TypeInfo(Smallint));
    AddField('FMax', System.TypeInfo(Smallint));
    AddField('FIncrement', System.TypeInfo(Integer));
    AddField('FNewValue', System.TypeInfo(Smallint));
    AddField('FNewValueDelta', System.TypeInfo(Smallint));
    AddField('FPosition', System.TypeInfo(Smallint));
    AddField('FThousands', System.TypeInfo(Boolean));
    AddField('FWrap', System.TypeInfo(Boolean));
    AddField('FOnClick', System.TypeInfo(TUDClickEvent));
    AddField('FAlignButton', System.TypeInfo(TUDAlignButton));
    AddField('FOrientation', System.TypeInfo(TUDOrientation));
    AddField('FOnChanging', System.TypeInfo(TUDChangingEvent));
    AddField('FOnChangingEx', System.TypeInfo(TUDChangingEventEx));

    AddMethod('UndoAutoResizing', nil,
      'procedure(Value: TWinControl)');
    AddMethod('SetAssociate', @TSepiImportsTCustomUpDown.SetAssociate,
      'procedure(Value: TWinControl)');
    AddMethod('GetPosition', @TSepiImportsTCustomUpDown.GetPosition,
      'function: SmallInt');
    AddMethod('SetMin', @TSepiImportsTCustomUpDown.SetMin,
      'procedure(Value: SmallInt)');
    AddMethod('SetMax', @TSepiImportsTCustomUpDown.SetMax,
      'procedure(Value: SmallInt)');
    AddMethod('SetIncrement', @TSepiImportsTCustomUpDown.SetIncrement,
      'procedure(Value: Integer)');
    AddMethod('SetPosition', @TSepiImportsTCustomUpDown.SetPosition,
      'procedure(Value: SmallInt)');
    AddMethod('SetAlignButton', @TSepiImportsTCustomUpDown.SetAlignButton,
      'procedure(Value: TUDAlignButton)');
    AddMethod('SetOrientation', @TSepiImportsTCustomUpDown.SetOrientation,
      'procedure(Value: TUDOrientation)');
    AddMethod('SetArrowKeys', @TSepiImportsTCustomUpDown.SetArrowKeys,
      'procedure(Value: Boolean)');
    AddMethod('SetThousands', @TSepiImportsTCustomUpDown.SetThousands,
      'procedure(Value: Boolean)');
    AddMethod('SetWrap', @TSepiImportsTCustomUpDown.SetWrap,
      'procedure(Value: Boolean)');
    AddMethod('CMAllChildrenFlipped', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ALLCHILDRENFLIPPED);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('WMHScroll', nil,
      'procedure(var Message: TWMHScroll)',
      mlkMessage, False, CN_HSCROLL);
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('WMVScroll', nil,
      'procedure(var Message: TWMVScroll)',
      mlkMessage, False, CN_VSCROLL);

    CurrentVisibility := mvProtected;

    AddMethod('DoCanChange', @TSepiImportsTCustomUpDown.DoCanChange,
      'function(NewVal: SmallInt; Delta: SmallInt): Boolean');
    AddMethod('CanChange', @TSepiImportsTCustomUpDown.CanChange,
      'function: Boolean',
      mlkDynamic);
    AddMethod('CreateParams', @TSepiImportsTCustomUpDown.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomUpDown.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTCustomUpDown.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('Click', @TSepiImportsTCustomUpDown.Click,
      'procedure(Button: TUDBtnType)',
      mlkDynamic);

    AddProperty('AlignButton', 'property: TUDAlignButton',
      'FAlignButton', 'SetAlignButton',
      NoIndex, Integer(udRight));
    AddProperty('ArrowKeys', 'property: Boolean',
      'FArrowKeys', 'SetArrowKeys',
      NoIndex, Integer(True));
    AddProperty('Associate', 'property: TWinControl',
      'FAssociate', 'SetAssociate');
    AddProperty('Min', 'property: SmallInt',
      'FMin', 'SetMin',
      NoIndex, 0);
    AddProperty('Max', 'property: SmallInt',
      'FMax', 'SetMax',
      NoIndex, 100);
    AddProperty('Increment', 'property: Integer',
      'FIncrement', 'SetIncrement',
      NoIndex, 1);
    AddProperty('Orientation', 'property: TUDOrientation',
      'FOrientation', 'SetOrientation',
      NoIndex, Integer(udVertical));
    AddProperty('Position', 'property: SmallInt',
      'GetPosition', 'SetPosition',
      NoIndex, 0);
    AddProperty('Thousands', 'property: Boolean',
      'FThousands', 'SetThousands',
      NoIndex, Integer(True));
    AddProperty('Wrap', 'property: Boolean',
      'FWrap', 'SetWrap',
      NoIndex, Integer(False));
    AddProperty('OnChanging', 'property: TUDChangingEvent',
      'FOnChanging', 'FOnChanging');
    AddProperty('OnChangingEx', 'property: TUDChangingEventEx',
      'FOnChangingEx', 'FOnChangingEx');
    AddProperty('OnClick', 'property: TUDClickEvent',
      'FOnClick', 'FOnClick');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomUpDown.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    Complete;
  end;
end;

{----------------}
{ TUpDown import }
{----------------}

class function TSepiImportsTUpDown.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TUpDown));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('AlignButton');
    RedefineProperty('Anchors');
    RedefineProperty('Associate');
    RedefineProperty('ArrowKeys');
    RedefineProperty('Enabled');
    RedefineProperty('Hint');
    RedefineProperty('Min');
    RedefineProperty('Max');
    RedefineProperty('Increment');
    RedefineProperty('Constraints');
    RedefineProperty('Orientation');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('Position');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Thousands');
    RedefineProperty('Visible');
    RedefineProperty('Wrap');
    RedefineProperty('OnChanging');
    RedefineProperty('OnChangingEx');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnClick');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');

    Complete;
  end;
end;

{----------------------}
{ TCustomHotKey import }
{----------------------}

procedure TSepiImportsTCustomHotKey.SetInvalidKeys(Value: THKInvalidKeys);
begin
  InvalidKeys := Value;
end;

procedure TSepiImportsTCustomHotKey.SetModifiers(Value: THKModifiers);
begin
  Modifiers := Value;
end;

function TSepiImportsTCustomHotKey.GetHotKey: TShortCut;
begin
  Result := HotKey;
end;

procedure TSepiImportsTCustomHotKey.SetHotKey(Value: TShortCut);
begin
  HotKey := Value;
end;

class function TSepiImportsTCustomHotKey.SepiImport(
  Owner: TSepiUnit): TSepiClass;
const
  DefaultInvalidKeys: THKInvalidKeys = [hcNone, hcShift];
  DefaultModifiers: THKModifiers = [hkAlt];
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomHotKey));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAutoSize', System.TypeInfo(Boolean));
    AddField('FModifiers', System.TypeInfo(THKModifiers));
    AddField('FInvalidKeys', System.TypeInfo(THKInvalidKeys));
    AddField('FHotKey', System.TypeInfo(Word));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));

    AddMethod('AdjustHeight', nil,
      'procedure');
    AddMethod('SetInvalidKeys', @TSepiImportsTCustomHotKey.SetInvalidKeys,
      'procedure(Value: THKInvalidKeys)');
    AddMethod('SetModifiers', @TSepiImportsTCustomHotKey.SetModifiers,
      'procedure(Value: THKModifiers)');
    AddMethod('UpdateHeight', nil,
      'procedure');
    AddMethod('GetHotKey', @TSepiImportsTCustomHotKey.GetHotKey,
      'function: TShortCut');
    AddMethod('SetHotKey', @TSepiImportsTCustomHotKey.SetHotKey,
      'procedure(Value: TShortCut)');
    AddMethod('ShortCutToHotKey', nil,
      'procedure(Value: TShortCut)');
    AddMethod('HotKeyToShortCut', nil,
      'function(Value: Longint): TShortCut');

    CurrentVisibility := mvProtected;

    AddMethod('CreateParams', @TSepiImportsTCustomHotKey.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomHotKey.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('CNCommand', @TSepiImportsTCustomHotKey.CNCommand,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, CN_COMMAND);
    AddMethod('SetAutoSize', @TSepiImportsTCustomHotKey.SetAutoSize,
      'procedure(Value: Boolean)',
      mlkOverride);

    AddProperty('AutoSize', 'property: Boolean',
      'FAutoSize', 'SetAutoSize',
      NoIndex, Integer(True));
    AddProperty('InvalidKeys', 'property: THKInvalidKeys',
      'FInvalidKeys', 'SetInvalidKeys',
      NoIndex, Byte(DefaultInvalidKeys));
    AddProperty('Modifiers', 'property: THKModifiers',
      'FModifiers', 'SetModifiers',
      NoIndex, Byte(DefaultModifiers));
    AddProperty('HotKey', 'property: TShortCut',
      'GetHotKey', 'SetHotKey',
      NoIndex, $0041);
    RedefineProperty('TabStop',
      '', '', Integer(True));
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomHotKey.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    Complete;
  end;
end;

{----------------}
{ THotKey import }
{----------------}

class function TSepiImportsTHotKey.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(THotKey));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Anchors');
    RedefineProperty('AutoSize');
    RedefineProperty('BiDiMode');
    RedefineProperty('Constraints');
    RedefineProperty('Enabled');
    RedefineProperty('Hint');
    RedefineProperty('HotKey');
    RedefineProperty('InvalidKeys');
    RedefineProperty('Modifiers');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('OnChange');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');

    Complete;
  end;
end;

{--------------------}
{ TListColumn import }
{--------------------}

function TSepiImportsTListColumn.GetWidth: TWidth;
begin
  Result := Width;
end;

procedure TSepiImportsTListColumn.SetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TSepiImportsTListColumn.SetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TSepiImportsTListColumn.SetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TSepiImportsTListColumn.SetImageIndex(Value: TImageIndex);
begin
  ImageIndex := Value;
end;

procedure TSepiImportsTListColumn.SetMaxWidth(Value: TWidth);
begin
  MaxWidth := Value;
end;

procedure TSepiImportsTListColumn.SetMinWidth(Value: TWidth);
begin
  MinWidth := Value;
end;

procedure TSepiImportsTListColumn.SetWidth(Value: TWidth);
begin
  Width := Value;
end;

class function TSepiImportsTListColumn.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TListColumn));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAlignment', System.TypeInfo(TAlignment));
    AddField('FAutoSize', System.TypeInfo(Boolean));
    AddField('FCaption', System.TypeInfo(string));
    AddField('FMaxWidth', System.TypeInfo(TWidth));
    AddField('FMinWidth', System.TypeInfo(TWidth));
    AddField('FImageIndex', System.TypeInfo(TImageIndex));
    AddField('FPrivateWidth', System.TypeInfo(TWidth));
    AddField('FWidth', System.TypeInfo(TWidth));
    AddField('FOrderTag', System.TypeInfo(Integer));
    AddField('FTag', System.TypeInfo(Integer), True);

    AddMethod('DoChange', nil,
      'procedure');
    AddMethod('GetWidth', @TSepiImportsTListColumn.GetWidth,
      'function: TWidth');
    AddMethod('IsWidthStored', nil,
      'function: Boolean');
    AddMethod('ReadData', nil,
      'procedure(Reader: TReader)');
    AddMethod('SetAlignment', @TSepiImportsTListColumn.SetAlignment,
      'procedure(Value: TAlignment)');
    AddMethod('SetAutoSize', @TSepiImportsTListColumn.SetAutoSize,
      'procedure(Value: Boolean)');
    AddMethod('SetCaption', @TSepiImportsTListColumn.SetCaption,
      'procedure(const Value: string)');
    AddMethod('SetImageIndex', @TSepiImportsTListColumn.SetImageIndex,
      'procedure(Value: TImageIndex)');
    AddMethod('SetMaxWidth', @TSepiImportsTListColumn.SetMaxWidth,
      'procedure(Value: TWidth)');
    AddMethod('SetMinWidth', @TSepiImportsTListColumn.SetMinWidth,
      'procedure(Value: TWidth)');
    AddMethod('SetWidth', @TSepiImportsTListColumn.SetWidth,
      'procedure(Value: TWidth)');
    AddMethod('WriteData', nil,
      'procedure(Writer: TWriter)');

    CurrentVisibility := mvProtected;

    AddMethod('DefineProperties', @TSepiImportsTListColumn.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('GetDisplayName', @TSepiImportsTListColumn.GetDisplayName,
      'function: string',
      mlkOverride);
    AddMethod('SetIndex', @TSepiImportsTListColumn.SetIndex,
      'procedure(Value: Integer)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTListColumn.Create,
      'constructor(Collection: TCollection)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTListColumn.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTListColumn.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    AddProperty('WidthType', 'property: TWidth',
      'FWidth', '');

    CurrentVisibility := mvPublished;

    AddProperty('Alignment', 'property: TAlignment',
      'FAlignment', 'SetAlignment',
      NoIndex, Integer(taLeftJustify));
    AddProperty('AutoSize', 'property: Boolean',
      'FAutoSize', 'SetAutoSize',
      NoIndex, Integer(False));
    AddProperty('Caption', 'property: string',
      'FCaption', 'SetCaption');
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImageIndex',
      NoIndex, -1);
    AddProperty('MaxWidth', 'property: TWidth',
      'FMaxWidth', 'SetMaxWidth',
      NoIndex, 0);
    AddProperty('MinWidth', 'property: TWidth',
      'FMinWidth', 'SetMinWidth',
      NoIndex, 0);
    AddProperty('Tag', 'property: Integer',
      'FTag', 'FTag',
      NoIndex, 0);
    AddProperty('Width', 'property: TWidth',
      'GetWidth', 'SetWidth',
      NoIndex, 50, 'IsWidthStored');

    Complete;
  end;
end;

{---------------------}
{ TListColumns import }
{---------------------}

function TSepiImportsTListColumns.GetItem(Index: Integer): TListColumn;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTListColumns.SetItem(Index: Integer; Value: TListColumn);
begin
  Items[Index] := Value;
end;

class function TSepiImportsTListColumns.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TListColumns'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TListColumns));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOwner', System.TypeInfo(TCustomListView));

    AddMethod('GetItem', @TSepiImportsTListColumns.GetItem,
      'function(Index: Integer): TListColumn');
    AddMethod('SetItem', @TSepiImportsTListColumns.SetItem,
      'procedure(Index: Integer; Value: TListColumn)');
    AddMethod('UpdateCols', nil,
      'procedure');

    CurrentVisibility := mvProtected;

    AddMethod('GetOwner', @TSepiImportsTListColumns.GetOwner,
      'function: TPersistent',
      mlkOverride);
    AddMethod('Update', @TSepiImportsTListColumns.Update,
      'procedure(Item: TCollectionItem)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTListColumns.Create,
      'constructor(AOwner: TCustomListView)');
    AddMethod('Add', @TSepiImportsTListColumns.Add,
      'function: TListColumn');
    AddMethod('Owner', @TSepiImportsTListColumns.Owner,
      'function: TCustomListView');

    AddProperty('Items', 'property[Index: Integer]: TListColumn',
      'GetItem', 'SetItem', True);

    Complete;
  end;
end;

{------------------}
{ TListItem import }
{------------------}

function TSepiImportsTListItem.GetChecked: Boolean;
begin
  Result := Checked;
end;

function TSepiImportsTListItem.GetHandle: HWND;
begin
  Result := Handle;
end;

function TSepiImportsTListItem.GetIndex: Integer;
begin
  Result := Index;
end;

function TSepiImportsTListItem.GetListView: TCustomListView;
begin
  Result := ListView;
end;

function TSepiImportsTListItem.GetLeft: Integer;
begin
  Result := Left;
end;

function TSepiImportsTListItem.GetTop: Integer;
begin
  Result := Top;
end;

procedure TSepiImportsTListItem.SetChecked(Value: Boolean);
begin
  Checked := Value;
end;

procedure TSepiImportsTListItem.SetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TSepiImportsTListItem.SetData(Value: Pointer);
begin
  Data := Value;
end;

procedure TSepiImportsTListItem.SetIndent(Value: Integer);
begin
  Indent := Value;
end;

procedure TSepiImportsTListItem.SetLeft(Value: Integer);
begin
  Left := Value;
end;

procedure TSepiImportsTListItem.SetSubItems(Value: TStrings);
begin
  SubItems := Value;
end;

procedure TSepiImportsTListItem.SetTop(Value: Integer);
begin
  Top := Value;
end;

function TSepiImportsTListItem.GetSubItemImage(Index: Integer): Integer;
begin
  Result := SubItemImages[Index];
end;

procedure TSepiImportsTListItem.SetSubItemImage(Index: Integer;
  const Value: Integer);
begin
  SubItemImages[Index] := Value;
end;

class function TSepiImportsTListItem.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TListItem'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TListItem));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOwner', System.TypeInfo(TListItems));
    AddField('FSubItems', System.TypeInfo(TStrings));
    AddField('FData', 'Pointer');
    AddField('FImageIndex', System.TypeInfo(TImageIndex));
    AddField('FIndent', System.TypeInfo(Integer));
    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FOverlayIndex', System.TypeInfo(TImageIndex));
    AddField('FStateIndex', System.TypeInfo(TImageIndex));
    AddField('FCaption', System.TypeInfo(string));
    AddField('FDeleting', System.TypeInfo(Boolean));
    AddField('FProcessedDeleting', System.TypeInfo(Boolean));
    AddField('FChecked', System.TypeInfo(Boolean));

    AddMethod('GetChecked', @TSepiImportsTListItem.GetChecked,
      'function: Boolean');
    AddMethod('GetHandle', @TSepiImportsTListItem.GetHandle,
      'function: HWND');
    AddMethod('GetIndex', @TSepiImportsTListItem.GetIndex,
      'function: Integer');
    AddMethod('GetListView', @TSepiImportsTListItem.GetListView,
      'function: TCustomListView');
    AddMethod('GetLeft', @TSepiImportsTListItem.GetLeft,
      'function: Integer');
    AddMethod('GetState', nil,
      'function(Index: Integer): Boolean');
    AddMethod('GetTop', @TSepiImportsTListItem.GetTop,
      'function: Integer');
    AddMethod('IsEqual', nil,
      'function(Item: TListItem): Boolean');
    AddMethod('SetChecked', @TSepiImportsTListItem.SetChecked,
      'procedure(Value: Boolean)');
    AddMethod('SetCaption', @TSepiImportsTListItem.SetCaption,
      'procedure(const Value: string)');
    AddMethod('SetData', @TSepiImportsTListItem.SetData,
      'procedure(Value: Pointer)');
    AddMethod('SetImage', nil,
      'procedure(Index: Integer; Value: TImageIndex)');
    AddMethod('SetIndent', @TSepiImportsTListItem.SetIndent,
      'procedure(Value: Integer)');
    AddMethod('SetLeft', @TSepiImportsTListItem.SetLeft,
      'procedure(Value: Integer)');
    AddMethod('SetState', nil,
      'procedure(Index: Integer; State: Boolean)');
    AddMethod('SetSubItems', @TSepiImportsTListItem.SetSubItems,
      'procedure(Value: TStrings)');
    AddMethod('SetTop', @TSepiImportsTListItem.SetTop,
      'procedure(Value: Integer)');
    AddMethod('GetSubItemImage', @TSepiImportsTListItem.GetSubItemImage,
      'function(Index: Integer): Integer');
    AddMethod('SetSubItemImage', @TSepiImportsTListItem.SetSubItemImage,
      'procedure(Index: Integer; const Value: Integer)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTListItem.Create,
      'constructor(AOwner: TListItems)');
    AddMethod('Destroy', @TSepiImportsTListItem.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTListItem.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('CancelEdit', @TSepiImportsTListItem.CancelEdit,
      'procedure');
    AddMethod('Delete', @TSepiImportsTListItem.Delete,
      'procedure');
    AddMethod('DisplayRect', @TSepiImportsTListItem.DisplayRect,
      'function(Code: TDisplayCode): TRect');
    AddMethod('EditCaption', @TSepiImportsTListItem.EditCaption,
      'function: Boolean');
    AddMethod('GetPosition', @TSepiImportsTListItem.GetPosition,
      'function: TPoint');
    AddMethod('MakeVisible', @TSepiImportsTListItem.MakeVisible,
      'procedure(PartialOK: Boolean)');
    AddMethod('Update', @TSepiImportsTListItem.Update,
      'procedure');
    AddMethod('SetPosition', @TSepiImportsTListItem.SetPosition,
      'procedure(const Value: TPoint)');
    AddMethod('WorkArea', @TSepiImportsTListItem.WorkArea,
      'function: Integer');

    AddProperty('Caption', 'property: string',
      'FCaption', 'SetCaption');
    AddProperty('Checked', 'property: Boolean',
      'GetChecked', 'SetChecked');
    AddProperty('Cut', 'property: Boolean',
      'GetState', 'SetState',
      0);
    AddProperty('Data', 'property: Pointer',
      'FData', 'SetData');
    AddProperty('Deleting', 'property: Boolean',
      'FDeleting', '');
    AddProperty('DropTarget', 'property: Boolean',
      'GetState', 'SetState',
      1);
    AddProperty('Focused', 'property: Boolean',
      'GetState', 'SetState',
      2);
    AddProperty('Handle', 'property: HWND',
      'GetHandle', '');
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImage',
      0);
    AddProperty('Indent', 'property: Integer',
      'FIndent', 'SetIndent',
      NoIndex, 0);
    AddProperty('Index', 'property: Integer',
      'GetIndex', '');
    AddProperty('Left', 'property: Integer',
      'GetLeft', 'SetLeft');
    AddProperty('ListView', 'property: TCustomListView',
      'GetListView', '');
    AddProperty('Owner', 'property: TListItems',
      'FOwner', '');
    AddProperty('OverlayIndex', 'property: TImageIndex',
      'FOverlayIndex', 'SetImage',
      1);
    AddProperty('Position', 'property: TPoint',
      'GetPosition', 'SetPosition');
    AddProperty('Selected', 'property: Boolean',
      'GetState', 'SetState',
      3);
    AddProperty('StateIndex', 'property: TImageIndex',
      'FStateIndex', 'SetImage',
      2);
    AddProperty('SubItems', 'property: TStrings',
      'FSubItems', 'SetSubItems');
    AddProperty('SubItemImages', 'property[Index: Integer]: Integer',
      'GetSubItemImage', 'SetSubItemImage');
    AddProperty('Top', 'property: Integer',
      'GetTop', 'SetTop');

    Complete;
  end;
end;

{-----------------------------}
{ TListItemsEnumerator import }
{-----------------------------}

class function TSepiImportsTListItemsEnumerator.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TListItemsEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FListItems', System.TypeInfo(TListItems));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTListItemsEnumerator.Create,
      'constructor(AListItems: TListItems)');
    AddMethod('GetCurrent', @TSepiImportsTListItemsEnumerator.GetCurrent,
      'function: TListItem');
    AddMethod('MoveNext', @TSepiImportsTListItemsEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: TListItem',
      'GetCurrent', '');

    Complete;
  end;
end;

{-------------------}
{ TListItems import }
{-------------------}

class function TSepiImportsTListItems.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TListItems'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TListItems));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOwner', System.TypeInfo(TCustomListView));
    AddField('FUpdateCount', System.TypeInfo(Integer));
    AddField('FNoRedraw', System.TypeInfo(Boolean));

    AddMethod('ReadData', nil,
      'procedure(Stream: TStream)');
    AddMethod('ReadItemData', nil,
      'procedure(Stream: TStream)');
    AddMethod('WriteItemData', nil,
      'procedure(Stream: TStream)');

    CurrentVisibility := mvProtected;

    AddMethod('DefineProperties', @TSepiImportsTListItems.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('CreateItem', @TSepiImportsTListItems.CreateItem,
      'function(Index: Integer; ListItem: TListItem): TLVItem');
    AddMethod('GetCount', @TSepiImportsTListItems.GetCount,
      'function: Integer');
    AddMethod('GetHandle', @TSepiImportsTListItems.GetHandle,
      'function: HWND');
    AddMethod('GetItem', @TSepiImportsTListItems.GetItem,
      'function(Index: Integer): TListItem');
    AddMethod('SetCount', @TSepiImportsTListItems.SetCount,
      'procedure(Value: Integer)');
    AddMethod('SetItem', @TSepiImportsTListItems.SetItem,
      'procedure(Index: Integer; Value: TListItem)');
    AddMethod('SetUpdateState', @TSepiImportsTListItems.SetUpdateState,
      'procedure(Updating: Boolean)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTListItems.Create,
      'constructor(AOwner: TCustomListView)');
    AddMethod('Destroy', @TSepiImportsTListItems.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTListItems.Add,
      'function: TListItem');
    AddMethod('AddItem', @TSepiImportsTListItems.AddItem,
      'function(Item: TListItem; Index: Integer = -1): TListItem');
    AddMethod('Assign', @TSepiImportsTListItems.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('BeginUpdate', @TSepiImportsTListItems.BeginUpdate,
      'procedure');
    AddMethod('Clear', @TSepiImportsTListItems.Clear,
      'procedure');
    AddMethod('Delete', @TSepiImportsTListItems.Delete,
      'procedure(Index: Integer)');
    AddMethod('EndUpdate', @TSepiImportsTListItems.EndUpdate,
      'procedure');
    AddMethod('GetEnumerator', @TSepiImportsTListItems.GetEnumerator,
      'function: TListItemsEnumerator');
    AddMethod('IndexOf', @TSepiImportsTListItems.IndexOf,
      'function(Value: TListItem): Integer');
    AddMethod('Insert', @TSepiImportsTListItems.Insert,
      'function(Index: Integer): TListItem');

    AddProperty('Count', 'property: Integer',
      'GetCount', 'SetCount');
    AddProperty('Handle', 'property: HWND',
      'GetHandle', '');
    AddProperty('Item', 'property[Index: Integer]: TListItem',
      'GetItem', 'SetItem', True);
    AddProperty('Owner', 'property: TCustomListView',
      'FOwner', '');

    Complete;
  end;
end;

{------------------}
{ TWorkArea import }
{------------------}

procedure TSepiImportsTWorkArea.SetRect(const Value: TRect);
begin
  Rect := Value;
end;

procedure TSepiImportsTWorkArea.SetColor(const Value: TColor);
begin
  Color := Value;
end;

class function TSepiImportsTWorkArea.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TWorkArea));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FRect', 'TRect');
    AddField('FDisplayName', System.TypeInfo(string));
    AddField('FColor', System.TypeInfo(TColor));

    AddMethod('SetRect', @TSepiImportsTWorkArea.SetRect,
      'procedure(const Value: TRect)');
    AddMethod('SetColor', @TSepiImportsTWorkArea.SetColor,
      'procedure(const Value: TColor)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTWorkArea.Create,
      'constructor(Collection: TCollection)',
      mlkOverride);
    AddMethod('SetDisplayName', @TSepiImportsTWorkArea.SetDisplayName,
      'procedure(const Value: string)',
      mlkOverride);
    AddMethod('GetDisplayName', @TSepiImportsTWorkArea.GetDisplayName,
      'function: string',
      mlkOverride);

    AddProperty('Rect', 'property: TRect',
      'FRect', 'SetRect');
    AddProperty('Color', 'property: TColor',
      'FColor', 'SetColor');

    Complete;
  end;
end;

{-------------------}
{ TWorkAreas import }
{-------------------}

function TSepiImportsTWorkAreas.GetItem(Index: Integer): TWorkArea;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTWorkAreas.SetItem(Index: Integer;
  const Value: TWorkArea);
begin
  Items[Index] := Value;
end;

class function TSepiImportsTWorkAreas.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TWorkAreas));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetItem', @TSepiImportsTWorkAreas.GetItem,
      'function(Index: Integer): TWorkArea');
    AddMethod('SetItem', @TSepiImportsTWorkAreas.SetItem,
      'procedure(Index: Integer; const Value: TWorkArea)');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTWorkAreas.Changed,
      'procedure');
    AddMethod('Update', @TSepiImportsTWorkAreas.Update,
      'procedure(Item: TCollectionItem)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Add', @TSepiImportsTWorkAreas.Add,
      'function: TWorkArea');
    AddMethod('Delete', @TSepiImportsTWorkAreas.Delete,
      'procedure(Index: Integer)');
    AddMethod('Insert', @TSepiImportsTWorkAreas.Insert,
      'function(Index: Integer): TWorkArea');

    AddProperty('Items', 'property[Index: Integer]: TWorkArea',
      'GetItem', 'SetItem', True);

    Complete;
  end;
end;

{---------------------}
{ TIconOptions import }
{---------------------}

procedure TSepiImportsTIconOptions.SetArrangement(Value: TIconArrangement);
begin
  Arrangement := Value;
end;

procedure TSepiImportsTIconOptions.SetAutoArrange(Value: Boolean);
begin
  AutoArrange := Value;
end;

procedure TSepiImportsTIconOptions.SetWrapText(Value: Boolean);
begin
  WrapText := Value;
end;

class function TSepiImportsTIconOptions.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TIconOptions));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FListView', System.TypeInfo(TCustomListView));
    AddField('FArrangement', System.TypeInfo(TIconArrangement));
    AddField('FAutoArrange', System.TypeInfo(Boolean));
    AddField('FWrapText', System.TypeInfo(Boolean));

    AddMethod('SetArrangement', @TSepiImportsTIconOptions.SetArrangement,
      'procedure(Value: TIconArrangement)');
    AddMethod('SetAutoArrange', @TSepiImportsTIconOptions.SetAutoArrange,
      'procedure(Value: Boolean)');
    AddMethod('SetWrapText', @TSepiImportsTIconOptions.SetWrapText,
      'procedure(Value: Boolean)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTIconOptions.Create,
      'constructor(AOwner: TCustomListView)');

    CurrentVisibility := mvPublished;

    AddProperty('Arrangement', 'property: TIconArrangement',
      'FArrangement', 'SetArrangement',
      NoIndex, Integer(iaTop));
    AddProperty('AutoArrange', 'property: Boolean',
      'FAutoArrange', 'SetAutoArrange',
      NoIndex, Integer(False));
    AddProperty('WrapText', 'property: Boolean',
      'FWrapText', 'SetWrapText',
      NoIndex, Integer(True));

    Complete;
  end;
end;

{------------------------}
{ TCustomListView import }
{------------------------}

function TSepiImportsTCustomListView.GetBoundingRect: TRect;
begin
  Result := BoundingRect;
end;

function TSepiImportsTCustomListView.GetColumnFromIndex(Index: Integer):
TListColumn;
begin
  Result := Column[Index];
end;

function TSepiImportsTCustomListView.GetDropTarget: TListItem;
begin
  Result := DropTarget;
end;

function TSepiImportsTCustomListView.GetFocused: TListItem;
begin
  Result := ItemFocused;
end;

function TSepiImportsTCustomListView.GetSelected: TListItem;
begin
  Result := Selected;
end;

function TSepiImportsTCustomListView.GetTopItem: TListItem;
begin
  Result := TopItem;
end;

function TSepiImportsTCustomListView.GetViewOrigin: TPoint;
begin
  Result := ViewOrigin;
end;

function TSepiImportsTCustomListView.GetVisibleRowCount: Integer;
begin
  Result := VisibleRowCount;
end;

function TSepiImportsTCustomListView.GetHoverTime: Integer;
begin
  Result := HoverTime;
end;

procedure TSepiImportsTCustomListView.SetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TSepiImportsTCustomListView.SetColumnClick(Value: Boolean);
begin
  ColumnClick := Value;
end;

procedure TSepiImportsTCustomListView.SetColumnHeaders(Value: Boolean);
begin
  ShowColumnHeaders := Value;
end;

procedure TSepiImportsTCustomListView.SetDropTarget(Value: TListItem);
begin
  DropTarget := Value;
end;

procedure TSepiImportsTCustomListView.SetFocused(Value: TListItem);
begin
  ItemFocused := Value;
end;

procedure TSepiImportsTCustomListView.SetHideSelection(Value: Boolean);
begin
  HideSelection := Value;
end;

procedure TSepiImportsTCustomListView.SetIconOptions(Value: TIconOptions);
begin
  IconOptions := Value;
end;

procedure TSepiImportsTCustomListView.SetLargeImages(Value: TCustomImageList);
begin
  LargeImages := Value;
end;

procedure TSepiImportsTCustomListView.SetAllocBy(Value: Integer);
begin
  AllocBy := Value;
end;

procedure TSepiImportsTCustomListView.SetItems(Value: TListItems);
begin
  Items := Value;
end;

procedure TSepiImportsTCustomListView.SetListColumns(Value: TListColumns);
begin
  Columns := Value;
end;

procedure TSepiImportsTCustomListView.SetOwnerData(Value: Boolean);
begin
  OwnerData := Value;
end;

procedure TSepiImportsTCustomListView.SetOwnerDraw(Value: Boolean);
begin
  OwnerDraw := Value;
end;

procedure TSepiImportsTCustomListView.SetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TSepiImportsTCustomListView.SetShowWorkAreas(const Value: Boolean);
begin
  ShowWorkAreas := Value;
end;

procedure TSepiImportsTCustomListView.SetSmallImages(Value: TCustomImageList);
begin
  SmallImages := Value;
end;

procedure TSepiImportsTCustomListView.SetSortType(Value: TSortType);
begin
  SortType := Value;
end;

procedure TSepiImportsTCustomListView.SetSelected(Value: TListItem);
begin
  Selected := Value;
end;

procedure TSepiImportsTCustomListView.SetStateImages(Value: TCustomImageList);
begin
  StateImages := Value;
end;

procedure TSepiImportsTCustomListView.SetCheckboxes(Value: Boolean);
begin
  Checkboxes := Value;
end;

procedure TSepiImportsTCustomListView.SetFlatScrollBars(Value: Boolean);
begin
  FlatScrollBars := Value;
end;

procedure TSepiImportsTCustomListView.SetFullDrag(Value: Boolean);
begin
  FullDrag := Value;
end;

procedure TSepiImportsTCustomListView.SetGridLines(Value: Boolean);
begin
  GridLines := Value;
end;

procedure TSepiImportsTCustomListView.SetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TSepiImportsTCustomListView.SetHotTrackStyles(
  Value: TListHotTrackStyles);
begin
  HotTrackStyles := Value;
end;

procedure TSepiImportsTCustomListView.SetRowSelect(Value: Boolean);
begin
  RowSelect := Value;
end;

procedure TSepiImportsTCustomListView.SetHoverTime(Value: Integer);
begin
  HoverTime := Value;
end;

function TSepiImportsTCustomListView.GetItemIndex_0(Value: TListItem): Integer;
begin
  Result := GetItemIndex(Value);
end;

class function TSepiImportsTCustomListView.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomListView'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomListView));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FBorderStyle', System.TypeInfo(TBorderStyle));
    AddField('FViewStyle', System.TypeInfo(TViewStyle));
    AddField('FReadOnly', System.TypeInfo(Boolean));
    AddField('FLargeImages', System.TypeInfo(TCustomImageList));
    AddField('FSmallImages', System.TypeInfo(TCustomImageList));
    AddField('FStateImages', System.TypeInfo(TCustomImageList));
    AddField('FDragImage', System.TypeInfo(TDragImageList));
    AddField('FMultiSelect', System.TypeInfo(Boolean));
    AddField('FSortType', System.TypeInfo(TSortType));
    AddField('FColumnClick', System.TypeInfo(Boolean));
    AddField('FShowColumnHeaders', System.TypeInfo(Boolean));
    AddField('FListItems', System.TypeInfo(TListItems));
    AddField('FClicked', System.TypeInfo(Boolean));
    AddField('FRClicked', System.TypeInfo(Boolean));
    AddField('FIconOptions', System.TypeInfo(TIconOptions));
    AddField('FHideSelection', System.TypeInfo(Boolean));
    AddField('FListColumns', System.TypeInfo(TListColumns));
    AddField('FMemStream', System.TypeInfo(TMemoryStream));
    AddField('FOwnerData', System.TypeInfo(Boolean));
    AddField('FOwnerDraw', System.TypeInfo(Boolean));
    AddField('FColStream', System.TypeInfo(TMemoryStream));
    AddField('FCheckStream', System.TypeInfo(TMemoryStream));
    AddField('FEditInstance', 'Pointer');
    AddField('FDefEditProc', 'Pointer');
    AddField('FEditHandle', System.TypeInfo(HWND));
    AddField('FHeaderInstance', 'Pointer');
    AddField('FDefHeaderProc', 'Pointer');
    AddField('FHeaderHandle', System.TypeInfo(HWND));
    AddField('FAllocBy', System.TypeInfo(Integer));
    AddField('FDragIndex', System.TypeInfo(Integer));
    AddField('FLastDropTarget', System.TypeInfo(TListItem));
    AddField('FCheckboxes', System.TypeInfo(Boolean));
    AddField('FFlatScrollBars', System.TypeInfo(Boolean));
    AddField('FFullDrag', System.TypeInfo(Boolean));
    AddField('FGridLines', System.TypeInfo(Boolean));
    AddField('FHotTrack', System.TypeInfo(Boolean));
    AddField('FHotTrackStyles', System.TypeInfo(TListHotTrackStyles));
    AddField('FRowSelect', System.TypeInfo(Boolean));
    AddField('FHoverTime', System.TypeInfo(Integer));
    AddField('FLargeChangeLink', System.TypeInfo(TChangeLink));
    AddField('FSmallChangeLink', System.TypeInfo(TChangeLink));
    AddField('FStateChangeLink', System.TypeInfo(TChangeLink));
    AddField('FSavedSort', System.TypeInfo(TSortType));
    AddField('FReading', System.TypeInfo(Boolean));
    AddField('FCanvasChanged', System.TypeInfo(Boolean));
    AddField('FTempItem', System.TypeInfo(TListItem));
    AddField('FWorkAreas', System.TypeInfo(TWorkAreas));
    AddField('FShowWorkAreas', System.TypeInfo(Boolean));
    AddField('FUpdatingColumnOrder', System.TypeInfo(Boolean));
    AddField('FOurFont', System.TypeInfo(Integer));
    AddField('FStockFont', System.TypeInfo(Integer));
    AddField('FOwnerDataCount', System.TypeInfo(Integer));
    AddField('FOnAdvancedCustomDraw',
      System.TypeInfo(TLVAdvancedCustomDrawEvent));
    AddField('FOnAdvancedCustomDrawItem',
      System.TypeInfo(TLVAdvancedCustomDrawItemEvent));
    AddField('FOnAdvancedCustomDrawSubItem',
      System.TypeInfo(TLVAdvancedCustomDrawSubItemEvent));
    AddField('FOnChange', System.TypeInfo(TLVChangeEvent));
    AddField('FOnChanging', System.TypeInfo(TLVChangingEvent));
    AddField('FOnColumnClick', System.TypeInfo(TLVColumnClickEvent));
    AddField('FOnColumnDragged', System.TypeInfo(TNotifyEvent));
    AddField('FOnColumnRightClick', System.TypeInfo(TLVColumnRClickEvent));
    AddField('FOnCompare', System.TypeInfo(TLVCompareEvent));
    AddField('FOnCustomDraw', System.TypeInfo(TLVCustomDrawEvent));
    AddField('FOnCustomDrawItem', System.TypeInfo(TLVCustomDrawItemEvent));
    AddField('FOnCustomDrawSubItem',
      System.TypeInfo(TLVCustomDrawSubItemEvent));
    AddField('FOnData', System.TypeInfo(TLVOwnerDataEvent));
    AddField('FOnDataFind', System.TypeInfo(TLVOwnerDataFindEvent));
    AddField('FOnDataHint', System.TypeInfo(TLVOwnerDataHintEvent));
    AddField('FOnDataStateChange',
      System.TypeInfo(TLVOwnerDataStateChangeEvent));
    AddField('FOnDeletion', System.TypeInfo(TLVDeletedEvent));
    AddField('FOnDrawItem', System.TypeInfo(TLVDrawItemEvent));
    AddField('FOnEdited', System.TypeInfo(TLVEditedEvent));
    AddField('FOnEditing', System.TypeInfo(TLVEditingEvent));
    AddField('FOnGetImageIndex', System.TypeInfo(TLVNotifyEvent));
    AddField('FOnGetSubItemImage', System.TypeInfo(TLVSubItemImageEvent));
    AddField('FOnInfoTip', System.TypeInfo(TLVInfoTipEvent));
    AddField('FOnInsert', System.TypeInfo(TLVDeletedEvent));
    AddField('FOnSelectItem', System.TypeInfo(TLVSelectItemEvent));
    AddField('FOnCreateItemClass', System.TypeInfo(TLVCreateItemClassEvent));

    AddMethod('AreItemsStored', nil,
      'function: Boolean');
    AddMethod('CanvasChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMCtl3DChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);
    AddMethod('CMDrag', nil,
      'procedure(var Message: TCMDrag)',
      mlkMessage, False, CM_DRAG);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('DoAutoSize', nil,
      'procedure');
    AddMethod('DoDragOver', nil,
      'procedure(Source: TDragObject; X, Y: Integer; CanDrop: Boolean)');
    AddMethod('DrawWorkAreas', nil,
      'procedure');
    AddMethod('EditWndProc', nil,
      'procedure(var Message: TMessage)');
    AddMethod('GetBoundingRect', @TSepiImportsTCustomListView.GetBoundingRect,
      'function: TRect');
    AddMethod('GetColumnFromIndex',
      @TSepiImportsTCustomListView.GetColumnFromIndex,
      'function(Index: Integer): TListColumn');
    AddMethod('GetColumnFromTag', nil,
      'function(Tag: Integer): TListColumn');
    AddMethod('GetDropTarget', @TSepiImportsTCustomListView.GetDropTarget,
      'function: TListItem');
    AddMethod('GetFocused', @TSepiImportsTCustomListView.GetFocused,
      'function: TListItem');
    AddMethod('GetImageIndex', nil,
      'procedure(Item: TListItem)');
    AddMethod('GetSubItemImage', nil,
      'procedure(Item: TListItem; SubItem: Integer; var ImageIndex: Integer)');
    AddMethod('GetItem', nil,
      'function(Value: TLVItem): TListItem');
    AddMethod('GetSelected', @TSepiImportsTCustomListView.GetSelected,
      'function: TListItem');
    AddMethod('GetTopItem', @TSepiImportsTCustomListView.GetTopItem,
      'function: TListItem');
    AddMethod('GetViewOrigin', @TSepiImportsTCustomListView.GetViewOrigin,
      'function: TPoint');
    AddMethod('GetVisibleRowCount',
      @TSepiImportsTCustomListView.GetVisibleRowCount,
      'function: Integer');
    AddMethod('GetHoverTime', @TSepiImportsTCustomListView.GetHoverTime,
      'function: Integer');
    AddMethod('HeaderWndProc', nil,
      'procedure(var Message: TMessage)');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('RestoreChecks', nil,
      'procedure');
    AddMethod('SaveChecks', nil,
      'procedure');
    AddMethod('SetBorderStyle', @TSepiImportsTCustomListView.SetBorderStyle,
      'procedure(Value: TBorderStyle)');
    AddMethod('SetColumnClick', @TSepiImportsTCustomListView.SetColumnClick,
      'procedure(Value: Boolean)');
    AddMethod('SetColumnHeaders',
      @TSepiImportsTCustomListView.SetColumnHeaders,
      'procedure(Value: Boolean)');
    AddMethod('SetDropTarget', @TSepiImportsTCustomListView.SetDropTarget,
      'procedure(Value: TListItem)');
    AddMethod('SetFocused', @TSepiImportsTCustomListView.SetFocused,
      'procedure(Value: TListItem)');
    AddMethod('SetHideSelection',
      @TSepiImportsTCustomListView.SetHideSelection,
      'procedure(Value: Boolean)');
    AddMethod('SetIconOptions', @TSepiImportsTCustomListView.SetIconOptions,
      'procedure(Value: TIconOptions)');
    AddMethod('SetImageList', nil,
      'procedure(Value: HImageList; Flags: Integer)');
    AddMethod('SetLargeImages', @TSepiImportsTCustomListView.SetLargeImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('SetAllocBy', @TSepiImportsTCustomListView.SetAllocBy,
      'procedure(Value: Integer)');
    AddMethod('SetItems', @TSepiImportsTCustomListView.SetItems,
      'procedure(Value: TListItems)');
    AddMethod('SetListColumns', @TSepiImportsTCustomListView.SetListColumns,
      'procedure(Value: TListColumns)');
    AddMethod('SetOwnerData', @TSepiImportsTCustomListView.SetOwnerData,
      'procedure(Value: Boolean)');
    AddMethod('SetOwnerDraw', @TSepiImportsTCustomListView.SetOwnerDraw,
      'procedure(Value: Boolean)');
    AddMethod('SetReadOnly', @TSepiImportsTCustomListView.SetReadOnly,
      'procedure(Value: Boolean)');
    AddMethod('SetShowWorkAreas',
      @TSepiImportsTCustomListView.SetShowWorkAreas,
      'procedure(const Value: Boolean)');
    AddMethod('SetSmallImages', @TSepiImportsTCustomListView.SetSmallImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('SetSortType', @TSepiImportsTCustomListView.SetSortType,
      'procedure(Value: TSortType)');
    AddMethod('SetSelected', @TSepiImportsTCustomListView.SetSelected,
      'procedure(Value: TListItem)');
    AddMethod('SetStateImages', @TSepiImportsTCustomListView.SetStateImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('SetTextBkColor', nil,
      'procedure(Value: TColor)');
    AddMethod('SetTextColor', nil,
      'procedure(Value: TColor)');
    AddMethod('SetCheckboxes', @TSepiImportsTCustomListView.SetCheckboxes,
      'procedure(Value: Boolean)');
    AddMethod('SetFlatScrollBars',
      @TSepiImportsTCustomListView.SetFlatScrollBars,
      'procedure(Value: Boolean)');
    AddMethod('SetFullDrag', @TSepiImportsTCustomListView.SetFullDrag,
      'procedure(Value: Boolean)');
    AddMethod('SetGridLines', @TSepiImportsTCustomListView.SetGridLines,
      'procedure(Value: Boolean)');
    AddMethod('SetHotTrack', @TSepiImportsTCustomListView.SetHotTrack,
      'procedure(Value: Boolean)');
    AddMethod('SetHotTrackStyles',
      @TSepiImportsTCustomListView.SetHotTrackStyles,
      'procedure(Value: TListHotTrackStyles)');
    AddMethod('SetRowSelect', @TSepiImportsTCustomListView.SetRowSelect,
      'procedure(Value: Boolean)');
    AddMethod('SetHoverTime', @TSepiImportsTCustomListView.SetHoverTime,
      'procedure(Value: Integer)');
    AddMethod('ResetExStyles', nil,
      'procedure');
    AddMethod('ValidHeaderHandle', nil,
      'function: Boolean');
    AddMethod('WMLButtonDown', nil,
      'procedure(var Message: TWMLButtonDown)',
      mlkMessage, False, WM_LBUTTONDOWN);
    AddMethod('WMNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, WM_NOTIFY);
    AddMethod('WMParentNotify', nil,
      'procedure(var Message: TWMParentNotify)',
      mlkMessage, False, WM_PARENTNOTIFY);
    AddMethod('WMPaint', nil,
      'procedure(var Message: TWMPaint)',
      mlkMessage, False, WM_PAINT);
    AddMethod('WMVScroll', nil,
      'procedure(var Message: TWMVScroll)',
      mlkMessage, False, WM_VSCROLL);
    AddMethod('WMWindowPosChanged', nil,
      'procedure(var Message: TWMWindowPosChanged)',
      mlkMessage, False, WM_WINDOWPOSCHANGED);
    AddMethod('CNDrawItem', nil,
      'procedure(var Message: TWMDrawItem)',
      mlkMessage, False, CN_DRAWITEM);
    AddMethod('CMHintShow', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_HINTSHOW);
    AddMethod('WMContextMenu', nil,
      'procedure(var Message: TWMContextMenu)',
      mlkMessage, False, WM_CONTEXTMENU);
    AddMethod('WMCtlColorEdit', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_CTLCOLOREDIT);

    CurrentVisibility := mvProtected;

    AddMethod('ActionChange', @TSepiImportsTCustomListView.ActionChange,
      'procedure(Sender: TObject; CheckDefaults: Boolean)',
      mlkOverride);
    AddMethod('CanChange', @TSepiImportsTCustomListView.CanChange,
      'function(Item: TListItem; Change: Integer): Boolean',
      mlkDynamic);
    AddMethod('CanEdit', @TSepiImportsTCustomListView.CanEdit,
      'function(Item: TListItem): Boolean',
      mlkDynamic);
    AddMethod('Change', @TSepiImportsTCustomListView.Change,
      'procedure(Item: TListItem; Change: Integer)',
      mlkDynamic);
    AddMethod('ChangeScale', @TSepiImportsTCustomListView.ChangeScale,
      'procedure(M, D: Integer)',
      mlkOverride);
    AddMethod('ColClick', @TSepiImportsTCustomListView.ColClick,
      'procedure(Column: TListColumn)',
      mlkDynamic);
    AddMethod('ColRightClick', @TSepiImportsTCustomListView.ColRightClick,
      'procedure(Column: TListColumn; Point: TPoint)',
      mlkDynamic);
    AddMethod('ColumnsShowing', @TSepiImportsTCustomListView.ColumnsShowing,
      'function: Boolean');
    AddMethod('CreateListItem', @TSepiImportsTCustomListView.CreateListItem,
      'function: TListItem',
      mlkVirtual);
    AddMethod('CreateListItems', @TSepiImportsTCustomListView.CreateListItems,
      'function: TListItems',
      mlkVirtual);
    AddMethod('CreateParams', @TSepiImportsTCustomListView.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomListView.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('CustomDraw', @TSepiImportsTCustomListView.CustomDraw,
      'function(const ARect: TRect; Stage: TCustomDrawStage): Boolean',
      mlkVirtual);
    AddMethod('CustomDrawItem', @TSepiImportsTCustomListView.CustomDrawItem,
      'function(Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage ) : Boolean',
      mlkVirtual);
    AddMethod('CustomDrawSubItem',
      @TSepiImportsTCustomListView.CustomDrawSubItem,
      'function(Item: TListItem; SubItem: Integer; State: TCustomDrawState ; Stage: TCustomDrawStage ) : Boolean',
      mlkVirtual);
    AddMethod('Delete', @TSepiImportsTCustomListView.Delete,
      'procedure(Item: TListItem)',
      mlkDynamic);
    AddMethod('DestroyWnd', @TSepiImportsTCustomListView.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('DoEndDrag', @TSepiImportsTCustomListView.DoEndDrag,
      'procedure(Target: TObject; X, Y: Integer)',
      mlkOverride);
    AddMethod('DoStartDrag', @TSepiImportsTCustomListView.DoStartDrag,
      'procedure(var DragObject: TDragObject)',
      mlkOverride);
    AddMethod('DoInfoTip', @TSepiImportsTCustomListView.DoInfoTip,
      'procedure(Item: TListItem; var InfoTip: string)',
      mlkVirtual);
    AddMethod('DrawItem', @TSepiImportsTCustomListView.DrawItem,
      'procedure(Item: TListItem; Rect: TRect; State: TOwnerDrawState)',
      mlkVirtual);
    AddMethod('Edit', @TSepiImportsTCustomListView.Edit,
      'procedure(const Item: TLVItem)',
      mlkDynamic);
    AddMethod('MouseUp', @TSepiImportsTCustomListView.MouseUp,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer)',
      mlkOverride);
    AddMethod('OwnerDataFetch', @TSepiImportsTCustomListView.OwnerDataFetch,
      'function(Item: TListItem; Request: TItemRequest): Boolean',
      mlkVirtual);
    AddMethod('OwnerDataFind', @TSepiImportsTCustomListView.OwnerDataFind,
      'function(Find: TItemFind; const FindString: string; const FindPosition: TPoint ; FindData: Pointer ; StartIndex: Integer ; Direction: TSearchDirection ; Wrap: Boolean ) : Integer',
      mlkVirtual);
    AddMethod('OwnerDataHint', @TSepiImportsTCustomListView.OwnerDataHint,
      'function(StartIndex, EndIndex: Integer): Boolean',
      mlkVirtual);
    AddMethod('OwnerDataStateChange',
      @TSepiImportsTCustomListView.OwnerDataStateChange,
      'function(StartIndex, EndIndex: Integer; OldState, NewState : TItemStates ) : Boolean',
      mlkVirtual);
    AddMethod('GetActionLinkClass',
      @TSepiImportsTCustomListView.GetActionLinkClass,
      'function: TControlActionLinkClass',
      mlkOverride);
    AddMethod('GetCount', @TSepiImportsTCustomListView.GetCount,
      'function: Integer',
      mlkOverride);
    AddMethod('GetDragImages', @TSepiImportsTCustomListView.GetDragImages,
      'function: TDragImageList',
      mlkOverride);
    AddOverloadedMethod('GetItemIndex',
      @TSepiImportsTCustomListView.GetItemIndex_0,
      'function(Value: TListItem): Integer');
    AddOverloadedMethod('GetItemIndex', nil,
      'function: Integer',
      mlkOverride);
    AddMethod('GetSelCount', @TSepiImportsTCustomListView.GetSelCount,
      'function: Integer',
      mlkOverride);
    AddMethod('InsertItem', @TSepiImportsTCustomListView.InsertItem,
      'procedure(Item: TListItem)',
      mlkDynamic);
    AddMethod('IsCustomDrawn', @TSepiImportsTCustomListView.IsCustomDrawn,
      'function(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean',
      mlkVirtual);
    AddMethod('Notification', @TSepiImportsTCustomListView.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('SetItemIndex', @TSepiImportsTCustomListView.SetItemIndex,
      'procedure(const Value: Integer)',
      mlkOverride);
    AddMethod('SetMultiSelect', @TSepiImportsTCustomListView.SetMultiSelect,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetViewStyle', @TSepiImportsTCustomListView.SetViewStyle,
      'procedure(Value: TViewStyle)',
      mlkVirtual);
    AddMethod('UpdateColumn', @TSepiImportsTCustomListView.UpdateColumn,
      'procedure(AnIndex: Integer)');
    AddMethod('UpdateColumns', @TSepiImportsTCustomListView.UpdateColumns,
      'procedure');
    AddMethod('WndProc', @TSepiImportsTCustomListView.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);

    AddProperty('BorderStyle', 'property: TBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(bsSingle));
    AddProperty('Columns', 'property: TListColumns',
      'FListColumns', 'SetListColumns');
    AddProperty('ColumnClick', 'property: Boolean',
      'FColumnClick', 'SetColumnClick',
      NoIndex, Integer(True));
    AddProperty('ReadOnly', 'property: Boolean',
      'FReadOnly', 'SetReadOnly');
    AddProperty('HideSelection', 'property: Boolean',
      'FHideSelection', 'SetHideSelection',
      NoIndex, Integer(True));
    AddProperty('IconOptions', 'property: TIconOptions',
      'FIconOptions', 'SetIconOptions');
    AddProperty('Items', 'property: TListItems',
      'FListItems', 'SetItems',
      NoIndex, NoDefaultValue, 'AreItemsStored');
    AddProperty('AllocBy', 'property: Integer',
      'FAllocBy', 'SetAllocBy',
      NoIndex, 0);
    AddProperty('HoverTime', 'property: Integer',
      'GetHoverTime', 'SetHoverTime',
      NoIndex, -1);
    AddProperty('LargeImages', 'property: TCustomImageList',
      'FLargeImages', 'SetLargeImages');
    AddProperty('MultiSelect', 'property: Boolean',
      'FMultiSelect', 'SetMultiSelect',
      NoIndex, Integer(False));
    AddProperty('OwnerData', 'property: Boolean',
      'FOwnerData', 'SetOwnerData',
      NoIndex, Integer(False));
    AddProperty('OwnerDraw', 'property: Boolean',
      'FOwnerDraw', 'SetOwnerDraw',
      NoIndex, Integer(False));
    AddProperty('OnAdvancedCustomDraw', 'property: TLVAdvancedCustomDrawEvent',
      'FOnAdvancedCustomDraw', 'FOnAdvancedCustomDraw');
    AddProperty('OnAdvancedCustomDrawItem',
      'property: TLVAdvancedCustomDrawItemEvent',
      'FOnAdvancedCustomDrawItem', 'FOnAdvancedCustomDrawItem');
    AddProperty('OnAdvancedCustomDrawSubItem',
      'property: TLVAdvancedCustomDrawSubItemEvent',
      'FOnAdvancedCustomDrawSubItem', 'FOnAdvancedCustomDrawSubItem');
    AddProperty('OnChange', 'property: TLVChangeEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OnChanging', 'property: TLVChangingEvent',
      'FOnChanging', 'FOnChanging');
    AddProperty('OnColumnClick', 'property: TLVColumnClickEvent',
      'FOnColumnClick', 'FOnColumnClick');
    AddProperty('OnColumnDragged', 'property: TNotifyEvent',
      'FOnColumnDragged', 'FOnColumnDragged');
    AddProperty('OnColumnRightClick', 'property: TLVColumnRClickEvent',
      'FOnColumnRightClick', 'FOnColumnRightClick');
    AddProperty('OnCompare', 'property: TLVCompareEvent',
      'FOnCompare', 'FOnCompare');
    AddProperty('OnCustomDraw', 'property: TLVCustomDrawEvent',
      'FOnCustomDraw', 'FOnCustomDraw');
    AddProperty('OnCustomDrawItem', 'property: TLVCustomDrawItemEvent',
      'FOnCustomDrawItem', 'FOnCustomDrawItem');
    AddProperty('OnCustomDrawSubItem', 'property: TLVCustomDrawSubItemEvent',
      'FOnCustomDrawSubItem', 'FOnCustomDrawSubItem');
    AddProperty('OnData', 'property: TLVOwnerDataEvent',
      'FOnData', 'FOnData');
    AddProperty('OnDataFind', 'property: TLVOwnerDataFindEvent',
      'FOnDataFind', 'FOnDataFind');
    AddProperty('OnDataHint', 'property: TLVOwnerDataHintEvent',
      'FOnDataHint', 'FOnDataHint');
    AddProperty('OnDataStateChange', 'property: TLVOwnerDataStateChangeEvent',
      'FOnDataStateChange', 'FOnDataStateChange');
    AddProperty('OnDeletion', 'property: TLVDeletedEvent',
      'FOnDeletion', 'FOnDeletion');
    AddProperty('OnDrawItem', 'property: TLVDrawItemEvent',
      'FOnDrawItem', 'FOnDrawItem');
    AddProperty('OnEdited', 'property: TLVEditedEvent',
      'FOnEdited', 'FOnEdited');
    AddProperty('OnEditing', 'property: TLVEditingEvent',
      'FOnEditing', 'FOnEditing');
    AddProperty('OnInfoTip', 'property: TLVInfoTipEvent',
      'FOnInfoTip', 'FOnInfoTip');
    AddProperty('OnInsert', 'property: TLVDeletedEvent',
      'FOnInsert', 'FOnInsert');
    AddProperty('OnGetImageIndex', 'property: TLVNotifyEvent',
      'FOnGetImageIndex', 'FOnGetImageIndex');
    AddProperty('OnGetSubItemImage', 'property: TLVSubItemImageEvent',
      'FOnGetSubItemImage', 'FOnGetSubItemImage');
    AddProperty('OnSelectItem', 'property: TLVSelectItemEvent',
      'FOnSelectItem', 'FOnSelectItem');
    AddProperty('ShowColumnHeaders', 'property: Boolean',
      'FShowColumnHeaders', 'SetColumnHeaders',
      NoIndex, Integer(True));
    AddProperty('ShowWorkAreas', 'property: Boolean',
      'FShowWorkAreas', 'SetShowWorkAreas',
      NoIndex, Integer(False));
    AddProperty('SmallImages', 'property: TCustomImageList',
      'FSmallImages', 'SetSmallImages');
    AddProperty('SortType', 'property: TSortType',
      'FSortType', 'SetSortType',
      NoIndex, Integer(stNone));
    AddProperty('StateImages', 'property: TCustomImageList',
      'FStateImages', 'SetStateImages');
    AddProperty('ViewStyle', 'property: TViewStyle',
      'FViewStyle', 'SetViewStyle',
      NoIndex, Integer(vsIcon));
    AddProperty('OnCreateItemClass', 'property: TLVCreateItemClassEvent',
      'FOnCreateItemClass', 'FOnCreateItemClass');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomListView.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomListView.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AddItem', @TSepiImportsTCustomListView.AddItem,
      'procedure(Item: String; AObject: TObject)',
      mlkOverride);
    AddMethod('AlphaSort', @TSepiImportsTCustomListView.AlphaSort,
      'function: Boolean');
    AddMethod('Arrange', @TSepiImportsTCustomListView.Arrange,
      'procedure(Code: TListArrangement)');
    AddMethod('Clear', @TSepiImportsTCustomListView.Clear,
      'procedure',
      mlkOverride);
    AddMethod('ClearSelection', @TSepiImportsTCustomListView.ClearSelection,
      'procedure',
      mlkOverride);
    AddMethod('CopySelection', @TSepiImportsTCustomListView.CopySelection,
      'procedure(Destination: TCustomListControl)',
      mlkOverride);
    AddMethod('DeleteSelected', @TSepiImportsTCustomListView.DeleteSelected,
      'procedure',
      mlkOverride);
    AddMethod('FindCaption', @TSepiImportsTCustomListView.FindCaption,
      'function(StartIndex: Integer; Value: string; Partial, Inclusive, Wrap: Boolean ) : TListItem');
    AddMethod('FindData', @TSepiImportsTCustomListView.FindData,
      'function(StartIndex: Integer; Value: Pointer; Inclusive, Wrap: Boolean ) : TListItem');
    AddMethod('GetHitTestInfoAt',
      @TSepiImportsTCustomListView.GetHitTestInfoAt,
      'function(X, Y: Integer): THitTests');
    AddMethod('GetItemAt', @TSepiImportsTCustomListView.GetItemAt,
      'function(X, Y: Integer): TListItem');
    AddMethod('GetNearestItem', @TSepiImportsTCustomListView.GetNearestItem,
      'function(Point: TPoint; Direction: TSearchDirection ) : TListItem');
    AddMethod('GetNextItem', @TSepiImportsTCustomListView.GetNextItem,
      'function(StartItem: TListItem; Direction: TSearchDirection ; States: TItemStates ) : TListItem');
    AddMethod('GetSearchString', @TSepiImportsTCustomListView.GetSearchString,
      'function: string');
    AddMethod('IsEditing', @TSepiImportsTCustomListView.IsEditing,
      'function: Boolean');
    AddMethod('SelectAll', @TSepiImportsTCustomListView.SelectAll,
      'procedure',
      mlkOverride);
    AddMethod('Scroll', @TSepiImportsTCustomListView.Scroll,
      'procedure(DX, DY: Integer)');

    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');
    AddProperty('Checkboxes', 'property: Boolean',
      'FCheckboxes', 'SetCheckboxes',
      NoIndex, Integer(False));
    AddProperty('Column', 'property[Index: Integer]: TListColumn',
      'GetColumnFromIndex', '');
    AddProperty('DropTarget', 'property: TListItem',
      'GetDropTarget', 'SetDropTarget');
    AddProperty('FlatScrollBars', 'property: Boolean',
      'FFlatScrollBars', 'SetFlatScrollBars',
      NoIndex, Integer(False));
    AddProperty('FullDrag', 'property: Boolean',
      'FFullDrag', 'SetFullDrag',
      NoIndex, Integer(False));
    AddProperty('GridLines', 'property: Boolean',
      'FGridLines', 'SetGridLines',
      NoIndex, Integer(False));
    AddProperty('HotTrack', 'property: Boolean',
      'FHotTrack', 'SetHotTrack',
      NoIndex, Integer(False));
    AddProperty('HotTrackStyles', 'property: TListHotTrackStyles',
      'FHotTrackStyles', 'SetHotTrackStyles',
      NoIndex, 0);
    AddProperty('ItemFocused', 'property: TListItem',
      'GetFocused', 'SetFocused');
    AddProperty('RowSelect', 'property: Boolean',
      'FRowSelect', 'SetRowSelect',
      NoIndex, Integer(False));
    AddProperty('SelCount', 'property: Integer',
      'GetSelCount', '');
    AddProperty('Selected', 'property: TListItem',
      'GetSelected', 'SetSelected');

    AddMethod('CustomSort', @TSepiImportsTCustomListView.CustomSort,
      'function(SortProc: TLVCompare; lParam: Longint): Boolean');
    AddMethod('StringWidth', @TSepiImportsTCustomListView.StringWidth,
      'function(S: string): Integer');
    AddMethod('UpdateItems', @TSepiImportsTCustomListView.UpdateItems,
      'procedure(FirstIndex, LastIndex: Integer)');

    AddProperty('TopItem', 'property: TListItem',
      'GetTopItem', '');
    AddProperty('ViewOrigin', 'property: TPoint',
      'GetViewOrigin', '');
    AddProperty('VisibleRowCount', 'property: Integer',
      'GetVisibleRowCount', '');
    AddProperty('BoundingRect', 'property: TRect',
      'GetBoundingRect', '');
    AddProperty('WorkAreas', 'property: TWorkAreas',
      'FWorkAreas', '');

    Complete;
  end;
end;

{------------------}
{ TListView import }
{------------------}

class function TSepiImportsTListView.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TListView));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('Action');
    RedefineProperty('Align');
    RedefineProperty('AllocBy');
    RedefineProperty('Anchors');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelOuter');
    RedefineProperty('BevelKind',
      '', '', Integer(bkNone));
    RedefineProperty('BevelWidth');
    RedefineProperty('BiDiMode');
    RedefineProperty('BorderStyle');
    RedefineProperty('BorderWidth');
    RedefineProperty('Checkboxes');
    RedefineProperty('Color');
    RedefineProperty('Columns');
    RedefineProperty('ColumnClick');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('FlatScrollBars');
    RedefineProperty('FullDrag');
    RedefineProperty('GridLines');
    RedefineProperty('HideSelection');
    RedefineProperty('HotTrack');
    RedefineProperty('HotTrackStyles');
    RedefineProperty('HoverTime');
    RedefineProperty('IconOptions');
    RedefineProperty('Items');
    RedefineProperty('LargeImages');
    RedefineProperty('MultiSelect');
    RedefineProperty('OwnerData');
    RedefineProperty('OwnerDraw');
    RedefineProperty('ReadOnly',
      '', '', Integer(False));
    RedefineProperty('RowSelect');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor',
      '', '', Integer(False));
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowColumnHeaders');
    RedefineProperty('ShowWorkAreas');
    RedefineProperty('ShowHint');
    RedefineProperty('SmallImages');
    RedefineProperty('SortType');
    RedefineProperty('StateImages');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop',
      '', '', Integer(True));
    RedefineProperty('ViewStyle');
    RedefineProperty('Visible');
    RedefineProperty('OnAdvancedCustomDraw');
    RedefineProperty('OnAdvancedCustomDrawItem');
    RedefineProperty('OnAdvancedCustomDrawSubItem');
    RedefineProperty('OnChange');
    RedefineProperty('OnChanging');
    RedefineProperty('OnClick');
    RedefineProperty('OnColumnClick');
    RedefineProperty('OnColumnDragged');
    RedefineProperty('OnColumnRightClick');
    RedefineProperty('OnCompare');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnCustomDraw');
    RedefineProperty('OnCustomDrawItem');
    RedefineProperty('OnCustomDrawSubItem');
    RedefineProperty('OnData');
    RedefineProperty('OnDataFind');
    RedefineProperty('OnDataHint');
    RedefineProperty('OnDataStateChange');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDeletion');
    RedefineProperty('OnDrawItem');
    RedefineProperty('OnEdited');
    RedefineProperty('OnEditing');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnGetImageIndex');
    RedefineProperty('OnGetSubItemImage');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnInfoTip');
    RedefineProperty('OnInsert');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnResize');
    RedefineProperty('OnSelectItem');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{----------------------------}
{ TListViewActionLink import }
{----------------------------}

class function TSepiImportsTListViewActionLink.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TListViewActionLink));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddOverloadedMethod('AddItem', nil,
      'procedure(AnItem: TListControlItem)',
      mlkOverride);
    AddOverloadedMethod('AddItem', nil,
      'procedure(ACaption: String; AImageIndex: Integer; DataPtr: Pointer )',
      mlkOverride);
    AddMethod('SetImages', @TSepiImportsTListViewActionLink.SetImages,
      'procedure(Value: TCustomImageList)',
      mlkOverride);

    Complete;
  end;
end;

{-----------------}
{ TAnimate import }
{-----------------}

procedure TSepiImportsTAnimate.SetActive(Value: Boolean);
begin
  Active := Value;
end;

procedure TSepiImportsTAnimate.SetFileName(Value: string);
begin
  FileName := Value;
end;

procedure TSepiImportsTAnimate.SetCenter(Value: Boolean);
begin
  Center := Value;
end;

procedure TSepiImportsTAnimate.SetCommonAVI(Value: TCommonAVI);
begin
  CommonAVI := Value;
end;

procedure TSepiImportsTAnimate.SetOpen(Value: Boolean);
begin
  Open := Value;
end;

procedure TSepiImportsTAnimate.SetRepetitions(Value: Integer);
begin
  Repetitions := Value;
end;

procedure TSepiImportsTAnimate.SetResHandle(Value: THandle);
begin
  ResHandle := Value;
end;

procedure TSepiImportsTAnimate.SetResId(Value: Integer);
begin
  ResId := Value;
end;

procedure TSepiImportsTAnimate.SetResName(Value: string);
begin
  ResName := Value;
end;

procedure TSepiImportsTAnimate.SetTimers(Value: Boolean);
begin
  Timers := Value;
end;

procedure TSepiImportsTAnimate.SetTransparent(Value: Boolean);
begin
  Transparent := Value;
end;

procedure TSepiImportsTAnimate.SetStartFrame(Value: Smallint);
begin
  StartFrame := Value;
end;

procedure TSepiImportsTAnimate.SetStopFrame(Value: Smallint);
begin
  StopFrame := Value;
end;

class function TSepiImportsTAnimate.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TAnimate));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FActive', System.TypeInfo(Boolean));
    AddField('FFileName', System.TypeInfo(string));
    AddField('FCenter', System.TypeInfo(Boolean));
    AddField('FCommonAVI', System.TypeInfo(TCommonAVI));
    AddField('FFrameCount', System.TypeInfo(Integer));
    AddField('FFrameHeight', System.TypeInfo(Integer));
    AddField('FFrameWidth', System.TypeInfo(Integer));
    AddField('FOpen', System.TypeInfo(Boolean));
    AddField('FRecreateNeeded', System.TypeInfo(Boolean));
    AddField('FRepetitions', System.TypeInfo(Integer));
    AddField('FResHandle', System.TypeInfo(THandle));
    AddField('FResId', System.TypeInfo(Integer));
    AddField('FResName', System.TypeInfo(string));
    AddField('FStreamedActive', System.TypeInfo(Boolean));
    AddField('FTimers', System.TypeInfo(Boolean));
    AddField('FTransparent', System.TypeInfo(Boolean));
    AddField('FStartFrame', System.TypeInfo(Smallint));
    AddField('FStopFrame', System.TypeInfo(Smallint));
    AddField('FStopCount', System.TypeInfo(Integer));
    AddField('FOnOpen', System.TypeInfo(TNotifyEvent));
    AddField('FOnClose', System.TypeInfo(TNotifyEvent));
    AddField('FOnStart', System.TypeInfo(TNotifyEvent));
    AddField('FOnStop', System.TypeInfo(TNotifyEvent));

    AddMethod('CheckOpen', nil,
      'procedure');
    AddMethod('InternalClose', nil,
      'function: Boolean');
    AddMethod('InternalOpen', nil,
      'function: Boolean');
    AddMethod('GetAnimateParams', nil,
      'procedure(var Params)');
    AddMethod('GetActualResHandle', nil,
      'function: THandle');
    AddMethod('GetActualResId', nil,
      'function: Integer');
    AddMethod('GetFrameInfo', nil,
      'procedure');
    AddMethod('SetAnimateParams', nil,
      'procedure(const Params)');
    AddMethod('SetActive', @TSepiImportsTAnimate.SetActive,
      'procedure(Value: Boolean)');
    AddMethod('SetFileName', @TSepiImportsTAnimate.SetFileName,
      'procedure(Value: string)');
    AddMethod('SetCenter', @TSepiImportsTAnimate.SetCenter,
      'procedure(Value: Boolean)');
    AddMethod('SetCommonAVI', @TSepiImportsTAnimate.SetCommonAVI,
      'procedure(Value: TCommonAVI)');
    AddMethod('SetOpen', @TSepiImportsTAnimate.SetOpen,
      'procedure(Value: Boolean)');
    AddMethod('SetRepetitions', @TSepiImportsTAnimate.SetRepetitions,
      'procedure(Value: Integer)');
    AddMethod('SetResHandle', @TSepiImportsTAnimate.SetResHandle,
      'procedure(Value: THandle)');
    AddMethod('SetResId', @TSepiImportsTAnimate.SetResId,
      'procedure(Value: Integer)');
    AddMethod('SetResName', @TSepiImportsTAnimate.SetResName,
      'procedure(Value: string)');
    AddMethod('SetTimers', @TSepiImportsTAnimate.SetTimers,
      'procedure(Value: Boolean)');
    AddMethod('SetTransparent', @TSepiImportsTAnimate.SetTransparent,
      'procedure(Value: Boolean)');
    AddMethod('SetStartFrame', @TSepiImportsTAnimate.SetStartFrame,
      'procedure(Value: Smallint)');
    AddMethod('SetStopFrame', @TSepiImportsTAnimate.SetStopFrame,
      'procedure(Value: Smallint)');
    AddMethod('UpdateActiveState', nil,
      'procedure');
    AddMethod('WMNCCalcSize', nil,
      'procedure(var Message: TWMNCCalcSize)',
      mlkMessage, False, WM_NCCALCSIZE);
    AddMethod('WMNCHitTest', nil,
      'procedure(var Message: TWMNCHitTest)',
      mlkMessage, False, WM_NCHITTEST);
    AddMethod('WMNCPaint', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_NCPAINT);
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('WMWindowPosChanged', nil,
      'procedure(var Message: TWMWindowPosChanged)',
      mlkMessage, False, WM_WINDOWPOSCHANGED);
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CNCommand', nil,
      'procedure(var Message: TWMCommand)',
      mlkMessage, False, CN_COMMAND);

    CurrentVisibility := mvProtected;

    AddMethod('CanAutoSize', @TSepiImportsTAnimate.CanAutoSize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTAnimate.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTAnimate.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWnd', @TSepiImportsTAnimate.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('DoOpen', @TSepiImportsTAnimate.DoOpen,
      'procedure',
      mlkVirtual);
    AddMethod('DoClose', @TSepiImportsTAnimate.DoClose,
      'procedure',
      mlkVirtual);
    AddMethod('DoStart', @TSepiImportsTAnimate.DoStart,
      'procedure',
      mlkVirtual);
    AddMethod('DoStop', @TSepiImportsTAnimate.DoStop,
      'procedure',
      mlkVirtual);
    AddMethod('Loaded', @TSepiImportsTAnimate.Loaded,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTAnimate.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    AddProperty('FrameCount', 'property: Integer',
      'FFrameCount', '');
    AddProperty('FrameHeight', 'property: Integer',
      'FFrameHeight', '');
    AddProperty('FrameWidth', 'property: Integer',
      'FFrameWidth', '');
    AddProperty('Open', 'property: Boolean',
      'FOpen', 'SetOpen');

    AddMethod('Play', @TSepiImportsTAnimate.Play,
      'procedure(FromFrame, ToFrame: Word; Count: Integer)');
    AddMethod('Reset', @TSepiImportsTAnimate.Reset,
      'procedure');
    AddMethod('Seek', @TSepiImportsTAnimate.Seek,
      'procedure(Frame: Smallint)');
    AddMethod('Stop', @TSepiImportsTAnimate.Stop,
      'procedure');

    AddProperty('ResHandle', 'property: THandle',
      'FResHandle', 'SetResHandle');
    AddProperty('ResId', 'property: Integer',
      'FResId', 'SetResId');
    AddProperty('ResName', 'property: string',
      'FResName', 'SetResName');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    AddProperty('Active', 'property: Boolean',
      'FActive', 'SetActive',
      NoIndex, Integer(False));
    RedefineProperty('Anchors');
    RedefineProperty('AutoSize',
      '', '', Integer(True));
    RedefineProperty('BorderWidth');
    AddProperty('Center', 'property: Boolean',
      'FCenter', 'SetCenter',
      NoIndex, Integer(True));
    RedefineProperty('Color');
    AddProperty('CommonAVI', 'property: TCommonAVI',
      'FCommonAVI', 'SetCommonAVI',
      NoIndex, Integer(aviNone));
    RedefineProperty('Constraints');
    AddProperty('FileName', 'property: string',
      'FFileName', 'SetFileName');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentShowHint');
    AddProperty('Repetitions', 'property: Integer',
      'FRepetitions', 'SetRepetitions',
      NoIndex, 0);
    RedefineProperty('ShowHint');
    AddProperty('StartFrame', 'property: Smallint',
      'FStartFrame', 'SetStartFrame',
      NoIndex, 1);
    AddProperty('StopFrame', 'property: Smallint',
      'FStopFrame', 'SetStopFrame',
      NoIndex, 0);
    AddProperty('Timers', 'property: Boolean',
      'FTimers', 'SetTimers',
      NoIndex, Integer(False));
    AddProperty('Transparent', 'property: Boolean',
      'FTransparent', 'SetTransparent',
      NoIndex, Integer(True));
    RedefineProperty('Visible');
    AddProperty('OnOpen', 'property: TNotifyEvent',
      'FOnOpen', 'FOnOpen');
    AddProperty('OnClose', 'property: TNotifyEvent',
      'FOnClose', 'FOnClose');
    AddProperty('OnStart', 'property: TNotifyEvent',
      'FOnStart', 'FOnStart');
    AddProperty('OnStop', 'property: TNotifyEvent',
      'FOnStop', 'FOnStop');

    Complete;
  end;
end;

{------------------------------}
{ TToolButtonActionLink import }
{------------------------------}

class function TSepiImportsTToolButtonActionLink.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TToolButtonActionLink));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddField('FClient', System.TypeInfo(TToolButton));

    AddMethod('AssignClient', @TSepiImportsTToolButtonActionLink.AssignClient,
      'procedure(AClient: TObject)',
      mlkOverride);
    AddMethod('IsCheckedLinked',
      @TSepiImportsTToolButtonActionLink.IsCheckedLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsDropdownMenuLinked',
      @TSepiImportsTToolButtonActionLink.IsDropdownMenuLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsEnableDropdownLinked',
      @TSepiImportsTToolButtonActionLink.IsEnableDropdownLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('IsImageIndexLinked',
      @TSepiImportsTToolButtonActionLink.IsImageIndexLinked,
      'function: Boolean',
      mlkOverride);
    AddMethod('SetChecked', @TSepiImportsTToolButtonActionLink.SetChecked,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetDropdownMenu',
      @TSepiImportsTToolButtonActionLink.SetDropdownMenu,
      'procedure(Value: TPopupMenu)',
      mlkOverride);
    AddMethod('SetEnableDropdown',
      @TSepiImportsTToolButtonActionLink.SetEnableDropdown,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetImageIndex',
      @TSepiImportsTToolButtonActionLink.SetImageIndex,
      'procedure(Value: Integer)',
      mlkOverride);

    Complete;
  end;
end;

{--------------------}
{ TToolButton import }
{--------------------}

function TSepiImportsTToolButton.GetIndex: Integer;
begin
  Result := Index;
end;

procedure TSepiImportsTToolButton.SetDown(Value: Boolean);
begin
  Down := Value;
end;

procedure TSepiImportsTToolButton.SetDropdownMenu(Value: TPopupMenu);
begin
  DropdownMenu := Value;
end;

procedure TSepiImportsTToolButton.SetEnableDropdown(Value: Boolean);
begin
  EnableDropdown := Value;
end;

procedure TSepiImportsTToolButton.SetGrouped(Value: Boolean);
begin
  Grouped := Value;
end;

procedure TSepiImportsTToolButton.SetImageIndex(Value: TImageIndex);
begin
  ImageIndex := Value;
end;

procedure TSepiImportsTToolButton.SetIndeterminate(Value: Boolean);
begin
  Indeterminate := Value;
end;

procedure TSepiImportsTToolButton.SetMarked(Value: Boolean);
begin
  Marked := Value;
end;

procedure TSepiImportsTToolButton.SetMenuItem(Value: TMenuItem);
begin
  MenuItem := Value;
end;

procedure TSepiImportsTToolButton.SetStyle(Value: TToolButtonStyle);
begin
  Style := Value;
end;

procedure TSepiImportsTToolButton.SetWrap(Value: Boolean);
begin
  Wrap := Value;
end;

class function TSepiImportsTToolButton.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TToolButton'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TToolButton));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAllowAllUp', System.TypeInfo(Boolean));
    AddField('FAutoSize', System.TypeInfo(Boolean));
    AddField('FDown', System.TypeInfo(Boolean));
    AddField('FGrouped', System.TypeInfo(Boolean));
    AddField('FImageIndex', System.TypeInfo(TImageIndex));
    AddField('FIndeterminate', System.TypeInfo(Boolean));
    AddField('FMarked', System.TypeInfo(Boolean));
    AddField('FMenuItem', System.TypeInfo(TMenuItem));
    AddField('FDropdownMenu', System.TypeInfo(TPopupMenu));
    AddField('FEnableDropdown', System.TypeInfo(Boolean));
    AddField('FWrap', System.TypeInfo(Boolean));
    AddField('FStyle', System.TypeInfo(TToolButtonStyle));
    AddField('FUpdateCount', System.TypeInfo(Integer));

    AddMethod('GetButtonState', nil,
      'function: Byte');
    AddMethod('GetIndex', @TSepiImportsTToolButton.GetIndex,
      'function: Integer');
    AddMethod('IsCheckedStored', nil,
      'function: Boolean');
    AddMethod('IsImageIndexStored', nil,
      'function: Boolean');
    AddMethod('IsWidthStored', nil,
      'function: Boolean');
    AddMethod('SetButtonState', nil,
      'procedure(State: Byte)');
    AddMethod('SetDown', @TSepiImportsTToolButton.SetDown,
      'procedure(Value: Boolean)');
    AddMethod('SetDropdownMenu', @TSepiImportsTToolButton.SetDropdownMenu,
      'procedure(Value: TPopupMenu)');
    AddMethod('SetEnableDropdown', @TSepiImportsTToolButton.SetEnableDropdown,
      'procedure(Value: Boolean)');
    AddMethod('SetGrouped', @TSepiImportsTToolButton.SetGrouped,
      'procedure(Value: Boolean)');
    AddMethod('SetImageIndex', @TSepiImportsTToolButton.SetImageIndex,
      'procedure(Value: TImageIndex)');
    AddMethod('SetIndeterminate', @TSepiImportsTToolButton.SetIndeterminate,
      'procedure(Value: Boolean)');
    AddMethod('SetMarked', @TSepiImportsTToolButton.SetMarked,
      'procedure(Value: Boolean)');
    AddMethod('SetMenuItem', @TSepiImportsTToolButton.SetMenuItem,
      'procedure(Value: TMenuItem)');
    AddMethod('SetStyle', @TSepiImportsTToolButton.SetStyle,
      'procedure(Value: TToolButtonStyle)');
    AddMethod('SetWrap', @TSepiImportsTToolButton.SetWrap,
      'procedure(Value: Boolean)');
    AddMethod('CMEnabledChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ENABLEDCHANGED);
    AddMethod('CMHitTest', nil,
      'procedure(var Message: TCMHitTest)',
      mlkMessage, False, CM_HITTEST);
    AddMethod('CMTextChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_TEXTCHANGED);
    AddMethod('CMVisibleChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_VISIBLECHANGED);

    CurrentVisibility := mvProtected;

    AddField('FToolBar', System.TypeInfo(TToolBar));

    AddMethod('ActionChange', @TSepiImportsTToolButton.ActionChange,
      'procedure(Sender: TObject; CheckDefaults: Boolean)',
      mlkOverride);
    AddMethod('AssignTo', @TSepiImportsTToolButton.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);
    AddMethod('BeginUpdate', @TSepiImportsTToolButton.BeginUpdate,
      'procedure',
      mlkVirtual);
    AddMethod('EndUpdate', @TSepiImportsTToolButton.EndUpdate,
      'procedure',
      mlkVirtual);
    AddMethod('GetActionLinkClass',
      @TSepiImportsTToolButton.GetActionLinkClass,
      'function: TControlActionLinkClass',
      mlkOverride);
    AddMethod('MouseDown', @TSepiImportsTToolButton.MouseDown,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer)',
      mlkOverride);
    AddMethod('MouseMove', @TSepiImportsTToolButton.MouseMove,
      'procedure(Shift: TShiftState; X, Y: Integer)',
      mlkOverride);
    AddMethod('MouseUp', @TSepiImportsTToolButton.MouseUp,
      'procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer )',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTToolButton.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('Paint', @TSepiImportsTToolButton.Paint,
      'procedure',
      mlkOverride);
    AddMethod('RefreshControl', @TSepiImportsTToolButton.RefreshControl,
      'procedure',
      mlkVirtual);
    AddMethod('SetAutoSize', @TSepiImportsTToolButton.SetAutoSize,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetToolBar', @TSepiImportsTToolButton.SetToolBar,
      'procedure(AToolBar: TToolBar)');
    AddMethod('UpdateControl', @TSepiImportsTToolButton.UpdateControl,
      'procedure',
      mlkVirtual);
    AddMethod('ValidateContainer', @TSepiImportsTToolButton.ValidateContainer,
      'procedure(AComponent: TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTToolButton.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('CheckMenuDropdown', @TSepiImportsTToolButton.CheckMenuDropdown,
      'function: Boolean',
      mlkDynamic);
    AddMethod('Click', @TSepiImportsTToolButton.Click,
      'procedure',
      mlkOverride);
    AddMethod('SetBounds', @TSepiImportsTToolButton.SetBounds,
      'procedure(ALeft, ATop, AWidth, AHeight: Integer)',
      mlkOverride);

    AddProperty('Index', 'property: Integer',
      'GetIndex', '');

    CurrentVisibility := mvPublished;

    RedefineProperty('Action');
    AddProperty('AllowAllUp', 'property: Boolean',
      'FAllowAllUp', 'FAllowAllUp',
      NoIndex, Integer(False));
    AddProperty('AutoSize', 'property: Boolean',
      'FAutoSize', 'SetAutoSize',
      NoIndex, Integer(False));
    RedefineProperty('Caption');
    AddProperty('Down', 'property: Boolean',
      'FDown', 'SetDown',
      NoIndex, Integer(False), 'IsCheckedStored');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    AddProperty('DropdownMenu', 'property: TPopupMenu',
      'FDropdownMenu', 'SetDropdownMenu');
    RedefineProperty('Enabled');
    AddProperty('EnableDropdown', 'property: Boolean',
      'FEnableDropdown', 'SetEnableDropdown',
      NoIndex, Integer(False));
    AddProperty('Grouped', 'property: Boolean',
      'FGrouped', 'SetGrouped',
      NoIndex, Integer(False));
    RedefineProperty('Height',
      '', '', 'False');
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImageIndex',
      NoIndex, -1, 'IsImageIndexStored');
    AddProperty('Indeterminate', 'property: Boolean',
      'FIndeterminate', 'SetIndeterminate',
      NoIndex, Integer(False));
    AddProperty('Marked', 'property: Boolean',
      'FMarked', 'SetMarked',
      NoIndex, Integer(False));
    AddProperty('MenuItem', 'property: TMenuItem',
      'FMenuItem', 'SetMenuItem');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    AddProperty('Wrap', 'property: Boolean',
      'FWrap', 'SetWrap',
      NoIndex, Integer(False));
    RedefineProperty('ShowHint');
    AddProperty('Style', 'property: TToolButtonStyle',
      'FStyle', 'SetStyle',
      NoIndex, Integer(tbsButton));
    RedefineProperty('Visible');
    RedefineProperty('Width',
      '', '', 'IsWidthStored');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
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

{---------------------------}
{ TToolBarEnumerator import }
{---------------------------}

class function TSepiImportsTToolBarEnumerator.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TToolBarEnumerator));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FIndex', System.TypeInfo(Integer));
    AddField('FToolBar', System.TypeInfo(TToolBar));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTToolBarEnumerator.Create,
      'constructor(AToolBar: TToolBar)');
    AddMethod('GetCurrent', @TSepiImportsTToolBarEnumerator.GetCurrent,
      'function: TToolButton');
    AddMethod('MoveNext', @TSepiImportsTToolBarEnumerator.MoveNext,
      'function: Boolean');

    AddProperty('Current', 'property: TToolButton',
      'GetCurrent', '');

    Complete;
  end;
end;

{-----------------}
{ TToolBar import }
{-----------------}

function TSepiImportsTToolBar.GetButton(Index: Integer): TToolButton;
begin
  Result := Buttons[Index];
end;

function TSepiImportsTToolBar.GetButtonCount: Integer;
begin
  Result := ButtonCount;
end;

function TSepiImportsTToolBar.GetRowCount: Integer;
begin
  Result := RowCount;
end;

procedure TSepiImportsTToolBar.SetList(Value: Boolean);
begin
  List := Value;
end;

procedure TSepiImportsTToolBar.SetShowCaptions(Value: Boolean);
begin
  ShowCaptions := Value;
end;

procedure TSepiImportsTToolBar.SetFlat(Value: Boolean);
begin
  Flat := Value;
end;

procedure TSepiImportsTToolBar.SetTransparent(Value: Boolean);
begin
  Transparent := Value;
end;

procedure TSepiImportsTToolBar.SetWrapable(Value: Boolean);
begin
  Wrapable := Value;
end;

procedure TSepiImportsTToolBar.SetButtonWidth(Value: Integer);
begin
  ButtonWidth := Value;
end;

procedure TSepiImportsTToolBar.SetButtonHeight(Value: Integer);
begin
  ButtonHeight := Value;
end;

procedure TSepiImportsTToolBar.SetImages(Value: TCustomImageList);
begin
  Images := Value;
end;

procedure TSepiImportsTToolBar.SetDisabledImages(Value: TCustomImageList);
begin
  DisabledImages := Value;
end;

procedure TSepiImportsTToolBar.SetHotImages(Value: TCustomImageList);
begin
  HotImages := Value;
end;

procedure TSepiImportsTToolBar.SetIndent(Value: Integer);
begin
  Indent := Value;
end;

procedure TSepiImportsTToolBar.SetMenu(const Value: TMainMenu);
begin
  Menu := Value;
end;

procedure TSepiImportsTToolBar.SetCustomizable(const Value: Boolean);
begin
  Customizable := Value;
end;

procedure TSepiImportsTToolBar.SetHideClippedButtons(const Value: Boolean);
begin
  HideClippedButtons := Value;
end;

class function TSepiImportsTToolBar.SepiImport(
  Owner: TSepiUnit): TSepiClass;
const
  DefaultEdgeBorders: TEdgeBorders = [ebTop];
begin
  Result := TSepiClass(Owner.FindMeta('TToolBar'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TToolBar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FButtonWidth', System.TypeInfo(Integer));
    AddField('FButtonHeight', System.TypeInfo(Integer));
    AddField('FButtons', System.TypeInfo(TList));
    AddField('FCaption', System.TypeInfo(string));
    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FCanvasChanged', System.TypeInfo(Boolean));
    AddField('FCustomizable', System.TypeInfo(Boolean));
    AddField('FCustomizing', System.TypeInfo(Boolean));
    AddField('FShowCaptions', System.TypeInfo(Boolean));
    AddField('FList', System.TypeInfo(Boolean));
    AddField('FFlat', System.TypeInfo(Boolean));
    AddField('FTransparent', System.TypeInfo(Boolean));
    AddField('FTransparentSet', System.TypeInfo(Boolean));
    AddField('FWrapable', System.TypeInfo(Boolean));
    AddField('FImages', System.TypeInfo(TCustomImageList));
    AddField('FImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FDisabledImages', System.TypeInfo(TCustomImageList));
    AddField('FDisabledImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FHotImages', System.TypeInfo(TCustomImageList));
    AddField('FHotImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FIndent', System.TypeInfo(Integer));
    AddField('FNewStyle', System.TypeInfo(Boolean));
    AddField('FNullBitmap', System.TypeInfo(TBitmap));
    AddField('FOldHandle', System.TypeInfo(HBitmap));
    AddField('FRestoring', System.TypeInfo(Boolean));
    AddField('FUpdateCount', System.TypeInfo(Integer));
    AddField('FHeightMargin', System.TypeInfo(Integer));
    AddField('FSeparators', System.TypeInfo(Integer));
    AddField('FOnAdvancedCustomDraw',
      System.TypeInfo(TTBAdvancedCustomDrawEvent));
    AddField('FOnAdvancedCustomDrawButton',
      System.TypeInfo(TTBAdvancedCustomDrawBtnEvent));
    AddField('FOnCustomDraw', System.TypeInfo(TTBCustomDrawEvent));
    AddField('FOnCustomDrawButton', System.TypeInfo(TTBCustomDrawBtnEvent));
    AddField('FOnCustomizeCanDelete', System.TypeInfo(TTBCustomizeQueryEvent));
    AddField('FOnCustomizeCanInsert', System.TypeInfo(TTBCustomizeQueryEvent));
    AddField('FOnCustomizeNewButton', System.TypeInfo(TTBNewButtonEvent));
    AddField('FOnCustomized', System.TypeInfo(TNotifyEvent));
    AddField('FOnCustomizeDelete', System.TypeInfo(TTBButtonEvent));
    AddField('FOnCustomizeAdded', System.TypeInfo(TTBButtonEvent));
    AddField('FOnCustomizing', System.TypeInfo(TNotifyEvent));
    AddField('FOnCustomizeReset', System.TypeInfo(TNotifyEvent));
    AddField('FCaptureChangeCancels', System.TypeInfo(Boolean));
    AddField('FInMenuLoop', System.TypeInfo(Boolean));
    AddField('FTempMenu', System.TypeInfo(TPopupMenu));
    AddField('FButtonMenu', System.TypeInfo(TMenuItem));
    AddField('FMenuButton', System.TypeInfo(TToolButton));
    AddField('FMenuResult', System.TypeInfo(Boolean));
    AddField('FMenuDropped', System.TypeInfo(Boolean));
    AddField('FMenu', System.TypeInfo(TMainMenu));
    AddField('FCustomizeKeyName', System.TypeInfo(string));
    AddField('FCustomizeValueName', System.TypeInfo(string));
    AddField('FOurFont', System.TypeInfo(Integer));
    AddField('FStockFont', System.TypeInfo(Integer));
    AddField('FHideClippedButtons', System.TypeInfo(Boolean));

    AddMethod('ButtonIndex', nil,
      'function(OldIndex, ALeft, ATop: Integer): Integer');
    AddMethod('CanvasChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('DoGetButton', nil,
      'function(NMToolbar: PNMToolbar): Boolean');
    AddMethod('LoadImages', nil,
      'procedure(AImages: TCustomImageList)');
    AddMethod('GetButton', @TSepiImportsTToolBar.GetButton,
      'function(Index: Integer): TToolButton');
    AddMethod('GetButtonCount', @TSepiImportsTToolBar.GetButtonCount,
      'function: Integer');
    AddMethod('GetButtonSize', nil,
      'procedure(var AWidth, AHeight: Integer)');
    AddMethod('GetRowCount', @TSepiImportsTToolBar.GetRowCount,
      'function: Integer');
    AddMethod('SetList', @TSepiImportsTToolBar.SetList,
      'procedure(Value: Boolean)');
    AddMethod('SetShowCaptions', @TSepiImportsTToolBar.SetShowCaptions,
      'procedure(Value: Boolean)');
    AddMethod('SetFlat', @TSepiImportsTToolBar.SetFlat,
      'procedure(Value: Boolean)');
    AddMethod('SetTransparent', @TSepiImportsTToolBar.SetTransparent,
      'procedure(Value: Boolean)');
    AddMethod('SetWrapable', @TSepiImportsTToolBar.SetWrapable,
      'procedure(Value: Boolean)');
    AddMethod('InsertButton', nil,
      'procedure(Control: TControl)');
    AddMethod('RemoveButton', nil,
      'procedure(Control: TControl)');
    AddMethod('RefreshButton', nil,
      'function(Index: Integer): Boolean');
    AddMethod('UpdateButton', nil,
      'procedure(Index: Integer)');
    AddMethod('UpdateButtons', nil,
      'procedure');
    AddMethod('UpdateButtonState', nil,
      'procedure(Index: Integer)');
    AddMethod('UpdateButtonStates', nil,
      'procedure');
    AddMethod('UpdateItem', nil,
      'function(Message, FromIndex, ToIndex: Integer): Boolean');
    AddMethod('UpdateItem2', nil,
      'function(Message, FromIndex, ToIndex: Integer): Boolean');
    AddMethod('ClearTempMenu', nil,
      'procedure');
    AddMethod('CreateButtons', nil,
      'procedure(NewWidth, NewHeight: Integer)');
    AddMethod('SetButtonWidth', @TSepiImportsTToolBar.SetButtonWidth,
      'procedure(Value: Integer)');
    AddMethod('SetButtonHeight', @TSepiImportsTToolBar.SetButtonHeight,
      'procedure(Value: Integer)');
    AddMethod('UpdateImages', nil,
      'procedure');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('SetImageList', nil,
      'procedure(Value: HImageList)');
    AddMethod('SetImages', @TSepiImportsTToolBar.SetImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('DisabledImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('SetDisabledImageList', nil,
      'procedure(Value: HImageList)');
    AddMethod('SetDisabledImages', @TSepiImportsTToolBar.SetDisabledImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('HotImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('SetHotImageList', nil,
      'procedure(Value: HImageList)');
    AddMethod('SetHotImages', @TSepiImportsTToolBar.SetHotImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('SetIndent', @TSepiImportsTToolBar.SetIndent,
      'procedure(Value: Integer)');
    AddMethod('SetMenu', @TSepiImportsTToolBar.SetMenu,
      'procedure(const Value: TMainMenu)');
    AddMethod('AdjustControl', nil,
      'procedure(Control: TControl)');
    AddMethod('RecreateButtons', nil,
      'procedure');
    AddMethod('RecreateButtonsFromToolbar', nil,
      'procedure');
    AddMethod('BeginUpdate', nil,
      'procedure');
    AddMethod('EndUpdate', nil,
      'procedure');
    AddMethod('ResizeButtons', nil,
      'procedure');
    AddMethod('SaveButtons', nil,
      'procedure(Save: Boolean)');
    AddMethod('InternalButtonCount', nil,
      'function: Integer');
    AddMethod('ReorderButton', nil,
      'function(OldIndex, ALeft, ATop: Integer): Integer');
    AddMethod('WMCaptureChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_CAPTURECHANGED);
    AddMethod('WMEraseBkgnd', nil,
      'procedure(var Message: TWMEraseBkgnd)',
      mlkMessage, False, WM_ERASEBKGND);
    AddMethod('WMGetDlgCode', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_GETDLGCODE);
    AddMethod('WMGetText', nil,
      'procedure(var Message: TWMGetText)',
      mlkMessage, False, WM_GETTEXT);
    AddMethod('WMGetTextLength', nil,
      'procedure(var Message: TWMGetTextLength)',
      mlkMessage, False, WM_GETTEXTLENGTH);
    AddMethod('WMKeyDown', nil,
      'procedure(var Message: TWMKeyDown)',
      mlkMessage, False, WM_KEYDOWN);
    AddMethod('WMNotifyFormat', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_NOTIFYFORMAT);
    AddMethod('WMSetText', nil,
      'procedure(var Message: TWMSetText)',
      mlkMessage, False, WM_SETTEXT);
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('WMSysChar', nil,
      'procedure(var Message: TWMSysChar)',
      mlkMessage, False, WM_SYSCHAR);
    AddMethod('WMSysCommand', nil,
      'procedure(var Message: TWMSysCommand)',
      mlkMessage, False, WM_SYSCOMMAND);
    AddMethod('WMWindowPosChanged', nil,
      'procedure(var Message: TWMWindowPosChanged)',
      mlkMessage, False, WM_WINDOWPOSCHANGED);
    AddMethod('WMWindowPosChanging', nil,
      'procedure(var Message: TWMWindowPosChanging)',
      mlkMessage, False, WM_WINDOWPOSCHANGING);
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMControlChange', nil,
      'procedure(var Message: TCMControlChange)',
      mlkMessage, False, CM_CONTROLCHANGE);
    AddMethod('CMDialogChar', nil,
      'procedure(var Message: TCMDialogChar)',
      mlkMessage, False, CM_DIALOGCHAR);
    AddMethod('CMEnabledChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_ENABLEDCHANGED);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CMParentColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTCOLORCHANGED);
    AddMethod('CNChar', nil,
      'procedure(var Message: TWMChar)',
      mlkMessage, False, CN_CHAR);
    AddMethod('CNSysKeyDown', nil,
      'procedure(var Message: TWMSysKeyDown)',
      mlkMessage, False, CN_SYSKEYDOWN);
    AddMethod('CMSysFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSFONTCHANGED);
    AddMethod('CNDropDownClosed', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CN_DROPDOWNCLOSED);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('SetCustomizable', @TSepiImportsTToolBar.SetCustomizable,
      'procedure(const Value: Boolean)');
    AddMethod('SetHideClippedButtons',
      @TSepiImportsTToolBar.SetHideClippedButtons,
      'procedure(const Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('AlignControls', @TSepiImportsTToolBar.AlignControls,
      'procedure(AControl: TControl; var Rect: TRect)',
      mlkOverride);
    AddMethod('CanAutoSize', @TSepiImportsTToolBar.CanAutoSize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('CancelMenu', @TSepiImportsTToolBar.CancelMenu,
      'procedure',
      mlkDynamic);
    AddMethod('ChangeScale', @TSepiImportsTToolBar.ChangeScale,
      'procedure(M, D: Integer)',
      mlkOverride);
    AddMethod('CheckMenuDropdown', @TSepiImportsTToolBar.CheckMenuDropdown,
      'function(Button: TToolButton): Boolean',
      mlkDynamic);
    AddMethod('ClickButton', @TSepiImportsTToolBar.ClickButton,
      'procedure(Button: TToolButton)',
      mlkDynamic);
    AddMethod('CreateParams', @TSepiImportsTToolBar.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTToolBar.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('CustomDraw', @TSepiImportsTToolBar.CustomDraw,
      'function(const ARect: TRect; Stage: TCustomDrawStage): Boolean',
      mlkVirtual);
    AddMethod('CustomDrawButton', @TSepiImportsTToolBar.CustomDrawButton,
      'function(Button: TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage ; var Flags: TTBCustomDrawFlags ) : Boolean',
      mlkVirtual);
    AddMethod('DoQueryInsert', @TSepiImportsTToolBar.DoQueryInsert,
      'function(Index: Integer): Boolean',
      mlkVirtual);
    AddMethod('DoQueryDelete', @TSepiImportsTToolBar.DoQueryDelete,
      'function(Index: Integer): Boolean',
      mlkVirtual);
    AddMethod('FindButtonFromAccel', @TSepiImportsTToolBar.FindButtonFromAccel,
      'function(Accel: Word): TToolButton');
    AddMethod('GetChildren', @TSepiImportsTToolBar.GetChildren,
      'procedure(Proc: TGetChildProc; Root: TComponent)',
      mlkOverride);
    AddMethod('InitMenu', @TSepiImportsTToolBar.InitMenu,
      'procedure(Button: TToolButton)',
      mlkDynamic);
    AddMethod('IsCustomDrawn', @TSepiImportsTToolBar.IsCustomDrawn,
      'function(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean',
      mlkVirtual);
    AddMethod('Loaded', @TSepiImportsTToolBar.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTToolBar.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation )',
      mlkOverride);
    AddMethod('RepositionButton', @TSepiImportsTToolBar.RepositionButton,
      'procedure(Index: Integer)');
    AddMethod('RepositionButtons', @TSepiImportsTToolBar.RepositionButtons,
      'procedure(Index: Integer)');
    AddMethod('WndProc', @TSepiImportsTToolBar.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);
    AddMethod('WrapButtons', @TSepiImportsTToolBar.WrapButtons,
      'function(var NewWidth, NewHeight: Integer): Boolean');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTToolBar.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTToolBar.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('FlipChildren', @TSepiImportsTToolBar.FlipChildren,
      'procedure(AllLevels: Boolean)',
      mlkOverride);
    AddMethod('GetEnumerator', @TSepiImportsTToolBar.GetEnumerator,
      'function: TToolBarEnumerator');
    AddMethod('TrackMenu', @TSepiImportsTToolBar.TrackMenu,
      'function(Button: TToolButton): Boolean',
      mlkDynamic);

    AddProperty('ButtonCount', 'property: Integer',
      'GetButtonCount', '');
    AddProperty('Buttons', 'property[Index: Integer]: TToolButton',
      'GetButton', '');
    AddProperty('Canvas', 'property: TCanvas',
      'FCanvas', '');
    AddProperty('CustomizeKeyName', 'property: string',
      'FCustomizeKeyName', 'FCustomizeKeyName');
    AddProperty('CustomizeValueName', 'property: string',
      'FCustomizeValueName', 'FCustomizeValueName');
    AddProperty('RowCount', 'property: Integer',
      'GetRowCount', '');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align',
      '', '', Integer(alTop));
    RedefineProperty('Anchors');
    RedefineProperty('AutoSize');
    RedefineProperty('BorderWidth');
    AddProperty('ButtonHeight', 'property: Integer',
      'FButtonHeight', 'SetButtonHeight',
      NoIndex, 22);
    AddProperty('ButtonWidth', 'property: Integer',
      'FButtonWidth', 'SetButtonWidth',
      NoIndex, 23);
    RedefineProperty('Caption');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    AddProperty('Customizable', 'property: Boolean',
      'FCustomizable', 'SetCustomizable',
      NoIndex, Integer(False));
    AddProperty('DisabledImages', 'property: TCustomImageList',
      'FDisabledImages', 'SetDisabledImages');
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('EdgeBorders',
      '', '', Byte(DefaultEdgeBorders));
    RedefineProperty('EdgeInner');
    RedefineProperty('EdgeOuter');
    RedefineProperty('Enabled');
    AddProperty('Flat', 'property: Boolean',
      'FFlat', 'SetFlat',
      NoIndex, Integer(False));
    RedefineProperty('Font');
    RedefineProperty('Height',
      '', '', 32);
    AddProperty('HideClippedButtons', 'property: Boolean',
      'FHideClippedButtons', 'SetHideClippedButtons',
      NoIndex, Integer(False));
    AddProperty('HotImages', 'property: TCustomImageList',
      'FHotImages', 'SetHotImages');
    AddProperty('Images', 'property: TCustomImageList',
      'FImages', 'SetImages');
    AddProperty('Indent', 'property: Integer',
      'FIndent', 'SetIndent',
      NoIndex, 0);
    AddProperty('List', 'property: Boolean',
      'FList', 'SetList',
      NoIndex, Integer(False));
    AddProperty('Menu', 'property: TMainMenu',
      'FMenu', 'SetMenu');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    AddProperty('ShowCaptions', 'property: Boolean',
      'FShowCaptions', 'SetShowCaptions',
      NoIndex, Integer(False));
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    AddProperty('Transparent', 'property: Boolean',
      'FTransparent', 'SetTransparent',
      NoIndex, NoDefaultValue, 'FTransparentSet');
    RedefineProperty('Visible');
    AddProperty('Wrapable', 'property: Boolean',
      'FWrapable', 'SetWrapable',
      NoIndex, Integer(True));
    AddProperty('OnAdvancedCustomDraw', 'property: TTBAdvancedCustomDrawEvent',
      'FOnAdvancedCustomDraw', 'FOnAdvancedCustomDraw');
    AddProperty('OnAdvancedCustomDrawButton',
      'property: TTBAdvancedCustomDrawBtnEvent',
      'FOnAdvancedCustomDrawButton', 'FOnAdvancedCustomDrawButton');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    AddProperty('OnCustomDraw', 'property: TTBCustomDrawEvent',
      'FOnCustomDraw', 'FOnCustomDraw');
    AddProperty('OnCustomDrawButton', 'property: TTBCustomDrawBtnEvent',
      'FOnCustomDrawButton', 'FOnCustomDrawButton');
    AddProperty('OnCustomizeAdded', 'property: TTBButtonEvent',
      'FOnCustomizeAdded', 'FOnCustomizeAdded');
    AddProperty('OnCustomizeCanInsert', 'property: TTBCustomizeQueryEvent',
      'FOnCustomizeCanInsert', 'FOnCustomizeCanInsert');
    AddProperty('OnCustomizeCanDelete', 'property: TTBCustomizeQueryEvent',
      'FOnCustomizeCanDelete', 'FOnCustomizeCanDelete');
    AddProperty('OnCustomized', 'property: TNotifyEvent',
      'FOnCustomized', 'FOnCustomized');
    AddProperty('OnCustomizeDelete', 'property: TTBButtonEvent',
      'FOnCustomizeDelete', 'FOnCustomizeDelete');
    AddProperty('OnCustomizing', 'property: TNotifyEvent',
      'FOnCustomizing', 'FOnCustomizing');
    AddProperty('OnCustomizeNewButton', 'property: TTBNewButtonEvent',
      'FOnCustomizeNewButton', 'FOnCustomizeNewButton');
    AddProperty('OnCustomizeReset', 'property: TNotifyEvent',
      'FOnCustomizeReset', 'FOnCustomizeReset');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDragDrop');
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
    RedefineProperty('OnResize');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{---------------------------}
{ TToolBarDockObject import }
{---------------------------}

class function TSepiImportsTToolBarDockObject.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TToolBarDockObject));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FEraseDockRect', 'TRect');
    AddField('FErase', System.TypeInfo(Boolean));

    CurrentVisibility := mvProtected;

    AddMethod('AdjustDockRect', @TSepiImportsTToolBarDockObject.AdjustDockRect,
      'procedure(ARect: TRect)',
      mlkOverride);
    AddMethod('DrawDragDockImage',
      @TSepiImportsTToolBarDockObject.DrawDragDockImage,
      'procedure',
      mlkOverride);
    AddMethod('EraseDragDockImage',
      @TSepiImportsTToolBarDockObject.EraseDragDockImage,
      'procedure',
      mlkOverride);
    AddMethod('GetDragCursor', @TSepiImportsTToolBarDockObject.GetDragCursor,
      'function(Accepted: Boolean; X, Y: Integer): TCursor',
      mlkOverride);
    AddMethod('ToolDockImage', @TSepiImportsTToolBarDockObject.ToolDockImage,
      'function(Erase: Boolean): Boolean',
      mlkVirtual);

    Complete;
  end;
end;

{------------------}
{ TCoolBand import }
{------------------}

function TSepiImportsTCoolBand.GetHeight: Integer;
begin
  Result := Height;
end;

function TSepiImportsTCoolBand.GetVisible: Boolean;
begin
  Result := Visible;
end;

procedure TSepiImportsTCoolBand.SetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TSepiImportsTCoolBand.SetBreak(Value: Boolean);
begin
  Break := Value;
end;

procedure TSepiImportsTCoolBand.SetFixedSize(Value: Boolean);
begin
  FixedSize := Value;
end;

procedure TSepiImportsTCoolBand.SetMinHeight(Value: Integer);
begin
  MinHeight := Value;
end;

procedure TSepiImportsTCoolBand.SetMinWidth(Value: Integer);
begin
  MinWidth := Value;
end;

procedure TSepiImportsTCoolBand.SetVisible(Value: Boolean);
begin
  Visible := Value;
end;

procedure TSepiImportsTCoolBand.SetHorizontalOnly(Value: Boolean);
begin
  HorizontalOnly := Value;
end;

procedure TSepiImportsTCoolBand.SetImageIndex(Value: TImageIndex);
begin
  ImageIndex := Value;
end;

procedure TSepiImportsTCoolBand.SetFixedBackground(Value: Boolean);
begin
  FixedBackground := Value;
end;

procedure TSepiImportsTCoolBand.SetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TSepiImportsTCoolBand.SetControl(Value: TWinControl);
begin
  Control := Value;
end;

procedure TSepiImportsTCoolBand.SetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TSepiImportsTCoolBand.SetParentBitmap(Value: Boolean);
begin
  ParentBitmap := Value;
end;

procedure TSepiImportsTCoolBand.SetBitmap(Value: TBitmap);
begin
  Bitmap := Value;
end;

procedure TSepiImportsTCoolBand.SetText(const Value: string);
begin
  Text := Value;
end;

procedure TSepiImportsTCoolBand.SetWidth(Value: Integer);
begin
  Width := Value;
end;

class function TSepiImportsTCoolBand.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCoolBand));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FBorderStyle', System.TypeInfo(TBorderStyle));
    AddField('FBreak', System.TypeInfo(Boolean));
    AddField('FFixedSize', System.TypeInfo(Boolean));
    AddField('FVisible', System.TypeInfo(Boolean));
    AddField('FHorizontalOnly', System.TypeInfo(Boolean));
    AddField('FImageIndex', System.TypeInfo(TImageIndex));
    AddField('FFixedBackground', System.TypeInfo(Boolean));
    AddField('FMinHeight', System.TypeInfo(Integer));
    AddField('FMinWidth', System.TypeInfo(Integer));
    AddField('FColor', System.TypeInfo(TColor));
    AddField('FControl', System.TypeInfo(TWinControl));
    AddField('FParentColor', System.TypeInfo(Boolean));
    AddField('FParentBitmap', System.TypeInfo(Boolean));
    AddField('FBitmap', System.TypeInfo(TBitmap));
    AddField('FText', System.TypeInfo(string));
    AddField('FWidth', System.TypeInfo(Integer));
    AddField('FDDB', System.TypeInfo(TBitmap));
    AddField('FID', System.TypeInfo(Integer));

    AddMethod('CoolBar', nil,
      'function: TCoolBar');
    AddMethod('IsColorStored', nil,
      'function: Boolean');
    AddMethod('IsBitmapStored', nil,
      'function: Boolean');
    AddMethod('BitmapChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('GetHeight', @TSepiImportsTCoolBand.GetHeight,
      'function: Integer');
    AddMethod('GetVisible', @TSepiImportsTCoolBand.GetVisible,
      'function: Boolean');
    AddMethod('SetBorderStyle', @TSepiImportsTCoolBand.SetBorderStyle,
      'procedure(Value: TBorderStyle)');
    AddMethod('SetBreak', @TSepiImportsTCoolBand.SetBreak,
      'procedure(Value: Boolean)');
    AddMethod('SetFixedSize', @TSepiImportsTCoolBand.SetFixedSize,
      'procedure(Value: Boolean)');
    AddMethod('SetMinHeight', @TSepiImportsTCoolBand.SetMinHeight,
      'procedure(Value: Integer)');
    AddMethod('SetMinWidth', @TSepiImportsTCoolBand.SetMinWidth,
      'procedure(Value: Integer)');
    AddMethod('SetVisible', @TSepiImportsTCoolBand.SetVisible,
      'procedure(Value: Boolean)');
    AddMethod('SetHorizontalOnly', @TSepiImportsTCoolBand.SetHorizontalOnly,
      'procedure(Value: Boolean)');
    AddMethod('SetImageIndex', @TSepiImportsTCoolBand.SetImageIndex,
      'procedure(Value: TImageIndex)');
    AddMethod('SetFixedBackground', @TSepiImportsTCoolBand.SetFixedBackground,
      'procedure(Value: Boolean)');
    AddMethod('SetColor', @TSepiImportsTCoolBand.SetColor,
      'procedure(Value: TColor)');
    AddMethod('SetControl', @TSepiImportsTCoolBand.SetControl,
      'procedure(Value: TWinControl)');
    AddMethod('SetParentColor', @TSepiImportsTCoolBand.SetParentColor,
      'procedure(Value: Boolean)');
    AddMethod('SetParentBitmap', @TSepiImportsTCoolBand.SetParentBitmap,
      'procedure(Value: Boolean)');
    AddMethod('SetBitmap', @TSepiImportsTCoolBand.SetBitmap,
      'procedure(Value: TBitmap)');
    AddMethod('SetText', @TSepiImportsTCoolBand.SetText,
      'procedure(const Value: string)');
    AddMethod('SetWidth', @TSepiImportsTCoolBand.SetWidth,
      'procedure(Value: Integer)');

    CurrentVisibility := mvProtected;

    AddMethod('GetDisplayName', @TSepiImportsTCoolBand.GetDisplayName,
      'function: string',
      mlkOverride);
    AddMethod('ParentColorChanged', @TSepiImportsTCoolBand.ParentColorChanged,
      'procedure',
      mlkDynamic);
    AddMethod('ParentBitmapChanged',
      @TSepiImportsTCoolBand.ParentBitmapChanged,
      'procedure',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCoolBand.Create,
      'constructor(Collection: TCollection)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCoolBand.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTCoolBand.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    AddProperty('Height', 'property: Integer',
      'GetHeight', '');

    CurrentVisibility := mvPublished;

    AddProperty('Bitmap', 'property: TBitmap',
      'FBitmap', 'SetBitmap',
      NoIndex, NoDefaultValue, 'IsBitmapStored');
    AddProperty('BorderStyle', 'property: TBorderStyle',
      'FBorderStyle', 'SetBorderStyle',
      NoIndex, Integer(bsNone));
    AddProperty('Break', 'property: Boolean',
      'FBreak', 'SetBreak',
      NoIndex, Integer(True));
    AddProperty('Color', 'property: TColor',
      'FColor', 'SetColor',
      NoIndex, Integer(clBtnFace), 'IsColorStored');
    AddProperty('Control', 'property: TWinControl',
      'FControl', 'SetControl');
    AddProperty('FixedBackground', 'property: Boolean',
      'FFixedBackground', 'SetFixedBackground',
      NoIndex, Integer(True));
    AddProperty('FixedSize', 'property: Boolean',
      'FFixedSize', 'SetFixedSize',
      NoIndex, Integer(False));
    AddProperty('HorizontalOnly', 'property: Boolean',
      'FHorizontalOnly', 'SetHorizontalOnly',
      NoIndex, Integer(False));
    AddProperty('ImageIndex', 'property: TImageIndex',
      'FImageIndex', 'SetImageIndex');
    AddProperty('MinHeight', 'property: Integer',
      'FMinHeight', 'SetMinHeight',
      NoIndex, 25);
    AddProperty('MinWidth', 'property: Integer',
      'FMinWidth', 'SetMinWidth',
      NoIndex, 0);
    AddProperty('ParentColor', 'property: Boolean',
      'FParentColor', 'SetParentColor',
      NoIndex, Integer(True));
    AddProperty('ParentBitmap', 'property: Boolean',
      'FParentBitmap', 'SetParentBitmap',
      NoIndex, Integer(True));
    AddProperty('Text', 'property: string',
      'FText', 'SetText');
    AddProperty('Visible', 'property: Boolean',
      'GetVisible', 'SetVisible',
      NoIndex, Integer(True));
    AddProperty('Width', 'property: Integer',
      'FWidth', 'SetWidth');

    Complete;
  end;
end;

{-------------------}
{ TCoolBands import }
{-------------------}

function TSepiImportsTCoolBands.GetItem(Index: Integer): TCoolBand;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTCoolBands.SetItem(Index: Integer; Value: TCoolBand);
begin
  Items[Index] := Value;
end;

class function TSepiImportsTCoolBands.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCoolBands));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCoolBar', System.TypeInfo(TCoolBar));
    AddField('FVisibleCount', System.TypeInfo(LongWord));

    AddMethod('GetItem', @TSepiImportsTCoolBands.GetItem,
      'function(Index: Integer): TCoolBand');
    AddMethod('SetItem', @TSepiImportsTCoolBands.SetItem,
      'procedure(Index: Integer; Value: TCoolBand)');

    CurrentVisibility := mvProtected;

    AddMethod('GetOwner', @TSepiImportsTCoolBands.GetOwner,
      'function: TPersistent',
      mlkOverride);
    AddMethod('Update', @TSepiImportsTCoolBands.Update,
      'procedure(Item: TCollectionItem)',
      mlkOverride);
    AddMethod('HaveGraphic', @TSepiImportsTCoolBands.HaveGraphic,
      'function: Boolean');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCoolBands.Create,
      'constructor(CoolBar: TCoolBar)');
    AddMethod('Add', @TSepiImportsTCoolBands.Add,
      'function: TCoolBand');
    AddMethod('FindBand', @TSepiImportsTCoolBands.FindBand,
      'function(AControl: TControl): TCoolBand');

    AddProperty('CoolBar', 'property: TCoolBar',
      'FCoolBar', '');
    AddProperty('Items', 'property[Index: Integer]: TCoolBand',
      'GetItem', 'SetItem', True);

    Complete;
  end;
end;

{-----------------}
{ TCoolBar import }
{-----------------}

function TSepiImportsTCoolBar.GetAlign: TAlign;
begin
  Result := Align;
end;

procedure TSepiImportsTCoolBar.SetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TSepiImportsTCoolBar.SetBands(Value: TCoolBands);
begin
  Bands := Value;
end;

procedure TSepiImportsTCoolBar.SetBandBorderStyle(Value: TBorderStyle);
begin
  BandBorderStyle := Value;
end;

procedure TSepiImportsTCoolBar.SetBandMaximize(Value: TCoolBandMaximize);
begin
  BandMaximize := Value;
end;

procedure TSepiImportsTCoolBar.SetBitmap(Value: TBitmap);
begin
  Bitmap := Value;
end;

procedure TSepiImportsTCoolBar.SetFixedSize(Value: Boolean);
begin
  FixedSize := Value;
end;

procedure TSepiImportsTCoolBar.SetFixedOrder(Value: Boolean);
begin
  FixedOrder := Value;
end;

procedure TSepiImportsTCoolBar.SetImages(Value: TCustomImageList);
begin
  Images := Value;
end;

procedure TSepiImportsTCoolBar.SetShowText(Value: Boolean);
begin
  ShowText := Value;
end;

procedure TSepiImportsTCoolBar.SetVertical(Value: Boolean);
begin
  Vertical := Value;
end;

class function TSepiImportsTCoolBar.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCoolBar'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCoolBar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FBands', System.TypeInfo(TCoolBands));
    AddField('FBandBorderStyle', System.TypeInfo(TBorderStyle));
    AddField('FBandMaximize', System.TypeInfo(TCoolBandMaximize));
    AddField('FBitmap', System.TypeInfo(TBitmap));
    AddField('FCaptionFont', System.TypeInfo(TFont));
    AddField('FCaptionFontHeight', System.TypeInfo(Integer));
    AddField('FDDB', System.TypeInfo(TBitmap));
    AddField('FFixedSize', System.TypeInfo(Boolean));
    AddField('FFixedOrder', System.TypeInfo(Boolean));
    AddField('FImages', System.TypeInfo(TCustomImageList));
    AddField('FImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FShowText', System.TypeInfo(Boolean));
    AddField('FVertical', System.TypeInfo(Boolean));
    AddField('FTrackDrag', 'TSmallPoint');
    AddField('FUpdateCount', System.TypeInfo(Integer));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));

    AddMethod('BeginUpdate', nil,
      'procedure');
    AddMethod('BitmapChanged', nil,
      'procedure(Sender: TObject)');
    AddMethod('EndUpdate', nil,
      'procedure');
    AddMethod('IsAutoSized', nil,
      'function: Boolean');
    AddMethod('IsBackgroundDirty', nil,
      'function: Boolean');
    AddMethod('GetAlign', @TSepiImportsTCoolBar.GetAlign,
      'function: TAlign');
    AddMethod('GetCaptionFont', nil,
      'function: HFONT');
    AddMethod('GetCaptionFontHeight', nil,
      'function: Integer');
    AddMethod('GetCaptionSize', nil,
      'function(Band: TCoolBand): Integer');
    AddMethod('GetRowHeight', nil,
      'function(Index: Integer): Integer');
    AddMethod('RefreshControl', nil,
      'procedure(Band: TCoolBand)');
    AddMethod('SetAlign', @TSepiImportsTCoolBar.SetAlign,
      'procedure(Value: TAlign)');
    AddMethod('SetBands', @TSepiImportsTCoolBar.SetBands,
      'procedure(Value: TCoolBands)');
    AddMethod('SetBandBorderStyle', @TSepiImportsTCoolBar.SetBandBorderStyle,
      'procedure(Value: TBorderStyle)');
    AddMethod('SetBandMaximize', @TSepiImportsTCoolBar.SetBandMaximize,
      'procedure(Value: TCoolBandMaximize)');
    AddMethod('SetBitmap', @TSepiImportsTCoolBar.SetBitmap,
      'procedure(Value: TBitmap)');
    AddMethod('SetFixedSize', @TSepiImportsTCoolBar.SetFixedSize,
      'procedure(Value: Boolean)');
    AddMethod('SetFixedOrder', @TSepiImportsTCoolBar.SetFixedOrder,
      'procedure(Value: Boolean)');
    AddMethod('SetImageList', nil,
      'procedure(Value: HImageList)');
    AddMethod('SetImages', @TSepiImportsTCoolBar.SetImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('SetShowText', @TSepiImportsTCoolBar.SetShowText,
      'procedure(Value: Boolean)');
    AddMethod('SetVertical', @TSepiImportsTCoolBar.SetVertical,
      'procedure(Value: Boolean)');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('PtInGripRect', nil,
      'function(const Pos: TPoint; var Band: TCoolBand): Integer');
    AddMethod('ReadBands', nil,
      'function: Boolean');
    AddMethod('UpdateItem', nil,
      'function(Message, FromIndex, ToIndex: Integer): Boolean');
    AddMethod('UpdateBand', nil,
      'procedure(Index: Integer)');
    AddMethod('UpdateBands', nil,
      'procedure');
    AddMethod('WMCaptureChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_CAPTURECHANGED);
    AddMethod('WMEraseBkgnd', nil,
      'procedure(var Message: TWMEraseBkgnd)',
      mlkMessage, False, WM_ERASEBKGND);
    AddMethod('WMLButtonDown', nil,
      'procedure(var Message: TWMLButtonDown)',
      mlkMessage, False, WM_LBUTTONDOWN);
    AddMethod('WMLButtonUp', nil,
      'procedure(var Message: TWMLButtonUp)',
      mlkMessage, False, WM_LBUTTONUP);
    AddMethod('WMNotifyFormat', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_NOTIFYFORMAT);
    AddMethod('WMSetCursor', nil,
      'procedure(var Message: TWMSetCursor)',
      mlkMessage, False, WM_SETCURSOR);
    AddMethod('WMSize', nil,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMControlChange', nil,
      'procedure(var Message: TCMControlChange)',
      mlkMessage, False, CM_CONTROLCHANGE);
    AddMethod('CMDesignHitTest', nil,
      'procedure(var Message: TCMDesignHitTest)',
      mlkMessage, False, CM_DESIGNHITTEST);
    AddMethod('CNBandChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CN_BANDCHANGE);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('CMSysColorChange', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSCOLORCHANGE);
    AddMethod('CMSysFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_SYSFONTCHANGED);
    AddMethod('CMWinIniChange', nil,
      'procedure(var Message: TWMWinIniChange)',
      mlkMessage, False, CM_WININICHANGE);

    CurrentVisibility := mvProtected;

    AddMethod('AlignControls', @TSepiImportsTCoolBar.AlignControls,
      'procedure(AControl: TControl; var Rect: TRect)',
      mlkOverride);
    AddMethod('CanAutoSize', @TSepiImportsTCoolBar.CanAutoSize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('Change', @TSepiImportsTCoolBar.Change,
      'procedure',
      mlkDynamic);
    AddMethod('CreateParams', @TSepiImportsTCoolBar.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCoolBar.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('GetPalette', @TSepiImportsTCoolBar.GetPalette,
      'function: HPALETTE',
      mlkOverride);
    AddMethod('HitTest', @TSepiImportsTCoolBar.HitTest,
      'function(const Pos: TPoint): TCoolBand');
    AddMethod('Loaded', @TSepiImportsTCoolBar.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTCoolBar.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('WndProc', @TSepiImportsTCoolBar.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);
    AddMethod('PaintWindow', @TSepiImportsTCoolBar.PaintWindow,
      'procedure(DC: HDC)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCoolBar.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCoolBar.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('FlipChildren', @TSepiImportsTCoolBar.FlipChildren,
      'procedure(AllLevels: Boolean)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('Align',
      'GetAlign', 'SetAlign', Integer(alTop));
    RedefineProperty('Anchors');
    RedefineProperty('AutoSize');
    AddProperty('BandBorderStyle', 'property: TBorderStyle',
      'FBandBorderStyle', 'SetBandBorderStyle',
      NoIndex, Integer(bsSingle));
    AddProperty('BandMaximize', 'property: TCoolBandMaximize',
      'FBandMaximize', 'SetBandMaximize',
      NoIndex, Integer(bmClick));
    AddProperty('Bands', 'property: TCoolBands',
      'FBands', 'SetBands');
    RedefineProperty('BorderWidth');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('EdgeBorders');
    RedefineProperty('EdgeInner');
    RedefineProperty('EdgeOuter');
    RedefineProperty('Enabled');
    AddProperty('FixedSize', 'property: Boolean',
      'FFixedSize', 'SetFixedSize',
      NoIndex, Integer(False));
    AddProperty('FixedOrder', 'property: Boolean',
      'FFixedOrder', 'SetFixedOrder',
      NoIndex, Integer(False));
    RedefineProperty('Font');
    AddProperty('Images', 'property: TCustomImageList',
      'FImages', 'SetImages');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    AddProperty('Bitmap', 'property: TBitmap',
      'FBitmap', 'SetBitmap');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    AddProperty('ShowText', 'property: Boolean',
      'FShowText', 'SetShowText',
      NoIndex, Integer(True));
    AddProperty('Vertical', 'property: Boolean',
      'FVertical', 'SetVertical',
      NoIndex, Integer(False));
    RedefineProperty('Visible');
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnGetSiteInfo');
    RedefineProperty('OnMouseActivate');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnResize');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnUnDock');

    Complete;
  end;
end;

{-----------------------------}
{ ECommonCalendarError import }
{-----------------------------}

class function TSepiImportsECommonCalendarError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ECommonCalendarError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ TMonthCalColors import }
{------------------------}

class function TSepiImportsTMonthCalColors.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMonthCalColors));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('Owner', System.TypeInfo(TCommonCalendar));
    AddField('FBackColor', System.TypeInfo(TColor));
    AddField('FTextColor', System.TypeInfo(TColor));
    AddField('FTitleBackColor', System.TypeInfo(TColor));
    AddField('FTitleTextColor', System.TypeInfo(TColor));
    AddField('FMonthBackColor', System.TypeInfo(TColor));
    AddField('FTrailingTextColor', System.TypeInfo(TColor));

    AddMethod('SetColor', nil,
      'procedure(Index: Integer; Value: TColor)');
    AddMethod('SetAllColors', nil,
      'procedure');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMonthCalColors.Create,
      'constructor(AOwner: TCommonCalendar)');
    AddMethod('Assign', @TSepiImportsTMonthCalColors.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    AddProperty('BackColor', 'property: TColor',
      'FBackColor', 'SetColor',
      0, Integer(clWindow));
    AddProperty('TextColor', 'property: TColor',
      'FTextColor', 'SetColor',
      1, Integer(clWindowText));
    AddProperty('TitleBackColor', 'property: TColor',
      'FTitleBackColor', 'SetColor',
      2, Integer(clActiveCaption));
    AddProperty('TitleTextColor', 'property: TColor',
      'FTitleTextColor', 'SetColor',
      3, Integer(clWhite));
    AddProperty('MonthBackColor', 'property: TColor',
      'FMonthBackColor', 'SetColor',
      4, Integer(clWhite));
    AddProperty('TrailingTextColor', 'property: TColor',
      'FTrailingTextColor', 'SetColor',
      5, Integer(clInactiveCaptionText));

    Complete;
  end;
end;

{------------------------}
{ TCommonCalendar import }
{------------------------}

function TSepiImportsTCommonCalendar.GetDate: TDate;
begin
  Result := Date;
end;

procedure TSepiImportsTCommonCalendar.SetCalColors(Value: TMonthCalColors);
begin
  CalColors := Value;
end;

procedure TSepiImportsTCommonCalendar.SetDate(Value: TDate);
begin
  Date := Value;
end;

procedure TSepiImportsTCommonCalendar.SetDateTime(Value: TDateTime);
begin
  DateTime := Value;
end;

procedure TSepiImportsTCommonCalendar.SetEndDate(Value: TDate);
begin
  EndDate := Value;
end;

procedure TSepiImportsTCommonCalendar.SetFirstDayOfWeek(Value: TCalDayOfWeek);
begin
  FirstDayOfWeek := Value;
end;

procedure TSepiImportsTCommonCalendar.SetMaxDate(Value: TDate);
begin
  MaxDate := Value;
end;

procedure TSepiImportsTCommonCalendar.SetMaxSelectRange(Value: Integer);
begin
  MaxSelectRange := Value;
end;

procedure TSepiImportsTCommonCalendar.SetMinDate(Value: TDate);
begin
  MinDate := Value;
end;

procedure TSepiImportsTCommonCalendar.SetMonthDelta(Value: Integer);
begin
  MonthDelta := Value;
end;

procedure TSepiImportsTCommonCalendar.SetMultiSelect(Value: Boolean);
begin
  MultiSelect := Value;
end;

procedure TSepiImportsTCommonCalendar.SetShowToday(Value: Boolean);
begin
  ShowToday := Value;
end;

procedure TSepiImportsTCommonCalendar.SetShowTodayCircle(Value: Boolean);
begin
  ShowTodayCircle := Value;
end;

procedure TSepiImportsTCommonCalendar.SetWeekNumbers(Value: Boolean);
begin
  WeekNumbers := Value;
end;

class function TSepiImportsTCommonCalendar.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCommonCalendar'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCommonCalendar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCalColors', System.TypeInfo(TMonthCalColors));
    AddField('FCalExceptionClass', 'ExceptClass');
    AddField('FDateTime', System.TypeInfo(TDateTime));
    AddField('FEndDate', System.TypeInfo(TDate));
    AddField('FFirstDayOfWeek', System.TypeInfo(TCalDayOfWeek));
    AddField('FMaxDate', System.TypeInfo(TDate));
    AddField('FMaxSelectRange', System.TypeInfo(Integer));
    AddField('FMinDate', System.TypeInfo(TDate));
    AddField('FMonthDelta', System.TypeInfo(Integer));
    AddField('FMultiSelect', System.TypeInfo(Boolean));
    AddField('FShowToday', System.TypeInfo(Boolean));
    AddField('FShowTodayCircle', System.TypeInfo(Boolean));
    AddField('FWeekNumbers', System.TypeInfo(Boolean));
    AddField('FOnGetMonthInfo', System.TypeInfo(TOnGetMonthInfoEvent));

    AddMethod('DoStoreEndDate', nil,
      'function: Boolean');
    AddMethod('DoStoreMaxDate', nil,
      'function: Boolean');
    AddMethod('DoStoreMinDate', nil,
      'function: Boolean');
    AddMethod('GetDate', @TSepiImportsTCommonCalendar.GetDate,
      'function: TDate');
    AddMethod('SetCalColors', @TSepiImportsTCommonCalendar.SetCalColors,
      'procedure(Value: TMonthCalColors)');
    AddMethod('SetDate', @TSepiImportsTCommonCalendar.SetDate,
      'procedure(Value: TDate)');
    AddMethod('SetDateTime', @TSepiImportsTCommonCalendar.SetDateTime,
      'procedure(Value: TDateTime)');
    AddMethod('SetEndDate', @TSepiImportsTCommonCalendar.SetEndDate,
      'procedure(Value: TDate)');
    AddMethod('SetFirstDayOfWeek',
      @TSepiImportsTCommonCalendar.SetFirstDayOfWeek,
      'procedure(Value: TCalDayOfWeek)');
    AddMethod('SetMaxDate', @TSepiImportsTCommonCalendar.SetMaxDate,
      'procedure(Value: TDate)');
    AddMethod('SetMaxSelectRange',
      @TSepiImportsTCommonCalendar.SetMaxSelectRange,
      'procedure(Value: Integer)');
    AddMethod('SetMinDate', @TSepiImportsTCommonCalendar.SetMinDate,
      'procedure(Value: TDate)');
    AddMethod('SetMonthDelta', @TSepiImportsTCommonCalendar.SetMonthDelta,
      'procedure(Value: Integer)');
    AddMethod('SetMultiSelect', @TSepiImportsTCommonCalendar.SetMultiSelect,
      'procedure(Value: Boolean)');
    AddMethod('SetRange', nil,
      'procedure(MinVal, MaxVal: TDate)');
    AddMethod('SetSelectedRange', nil,
      'procedure(Date, EndDate: TDate)');
    AddMethod('SetShowToday', @TSepiImportsTCommonCalendar.SetShowToday,
      'procedure(Value: Boolean)');
    AddMethod('SetShowTodayCircle',
      @TSepiImportsTCommonCalendar.SetShowTodayCircle,
      'procedure(Value: Boolean)');
    AddMethod('SetWeekNumbers', @TSepiImportsTCommonCalendar.SetWeekNumbers,
      'procedure(Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('CheckEmptyDate', @TSepiImportsTCommonCalendar.CheckEmptyDate,
      'procedure',
      mlkVirtual);
    AddMethod('CheckValidDate', @TSepiImportsTCommonCalendar.CheckValidDate,
      'procedure(Value: TDate)',
      mlkVirtual);
    AddMethod('CreateWnd', @TSepiImportsTCommonCalendar.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('GetCalendarHandle', nil,
      'function: HWND',
      mlkVirtual, True);
    AddMethod('GetCalStyles', @TSepiImportsTCommonCalendar.GetCalStyles,
      'function: DWORD',
      mlkVirtual);
    AddMethod('MsgSetCalColors', nil,
      'function(ColorIndex: Integer; ColorValue: TColor): Boolean',
      mlkVirtual, True);
    AddMethod('MsgSetDateTime', nil,
      'function(Value: TSystemTime): Boolean',
      mlkVirtual, True);
    AddMethod('MsgSetRange', nil,
      'function(Flags: Integer; SysTime: PSystemTime): Boolean',
      mlkVirtual, True);

    AddProperty('CalColors', 'property: TMonthCalColors',
      'FCalColors', 'SetCalColors');
    AddProperty('CalendarHandle', 'property: HWND',
      'GetCalendarHandle', '');
    AddProperty('CalExceptionClass', 'property: ExceptClass',
      'FCalExceptionClass', 'FCalExceptionClass');
    AddProperty('Date', 'property: TDate',
      'GetDate', 'SetDate');
    AddProperty('DateTime', 'property: TDateTime',
      'FDateTime', 'SetDateTime');
    AddProperty('EndDate', 'property: TDate',
      'FEndDate', 'SetEndDate',
      NoIndex, NoDefaultValue, 'DoStoreEndDate');
    AddProperty('FirstDayOfWeek', 'property: TCalDayOfWeek',
      'FFirstDayOfWeek', 'SetFirstDayOfWeek',
      NoIndex, Integer(dowLocaleDefault));
    AddProperty('MaxDate', 'property: TDate',
      'FMaxDate', 'SetMaxDate',
      NoIndex, NoDefaultValue, 'DoStoreMaxDate');
    AddProperty('MaxSelectRange', 'property: Integer',
      'FMaxSelectRange', 'SetMaxSelectRange',
      NoIndex, 31);
    AddProperty('MinDate', 'property: TDate',
      'FMinDate', 'SetMinDate',
      NoIndex, NoDefaultValue, 'DoStoreMinDate');
    AddProperty('MonthDelta', 'property: Integer',
      'FMonthDelta', 'SetMonthDelta',
      NoIndex, 1);
    AddProperty('MultiSelect', 'property: Boolean',
      'FMultiSelect', 'SetMultiSelect',
      NoIndex, Integer(False));
    AddProperty('ShowToday', 'property: Boolean',
      'FShowToday', 'SetShowToday',
      NoIndex, Integer(True));
    AddProperty('ShowTodayCircle', 'property: Boolean',
      'FShowTodayCircle', 'SetShowTodayCircle',
      NoIndex, Integer(True));
    AddProperty('WeekNumbers', 'property: Boolean',
      'FWeekNumbers', 'SetWeekNumbers',
      NoIndex, Integer(False));
    AddProperty('OnGetMonthInfo', 'property: TOnGetMonthInfoEvent',
      'FOnGetMonthInfo', 'FOnGetMonthInfo');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCommonCalendar.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCommonCalendar.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('BoldDays', @TSepiImportsTCommonCalendar.BoldDays,
      'procedure(Days: array of LongWord; var MonthBoldInfo: LongWord)');

    Complete;
  end;
end;

{-----------------------}
{ EMonthCalError import }
{-----------------------}

class function TSepiImportsEMonthCalError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EMonthCalError));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ TMonthCalendar import }
{-----------------------}

class function TSepiImportsTMonthCalendar.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMonthCalendar));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);

    CurrentVisibility := mvProtected;

    AddMethod('CanAutoSize', @TSepiImportsTMonthCalendar.CanAutoSize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('ConstrainedResize',
      @TSepiImportsTMonthCalendar.ConstrainedResize,
      'procedure(var MinWidth, MinHeight, MaxWidth, MaxHeight : Integer )',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTMonthCalendar.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('GetCalendarHandle',
      @TSepiImportsTMonthCalendar.GetCalendarHandle,
      'function: HWND',
      mlkOverride);
    AddMethod('MsgSetCalColors', @TSepiImportsTMonthCalendar.MsgSetCalColors,
      'function(ColorIndex: Integer; ColorValue: TColor): Boolean',
      mlkOverride);
    AddMethod('MsgSetDateTime', @TSepiImportsTMonthCalendar.MsgSetDateTime,
      'function(Value: TSystemTime): Boolean',
      mlkOverride);
    AddMethod('MsgSetRange', @TSepiImportsTMonthCalendar.MsgSetRange,
      'function(Flags: Integer; SysTime: PSystemTime): Boolean',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMonthCalendar.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('AutoSize');
    RedefineProperty('BorderWidth');
    RedefineProperty('BiDiMode');
    RedefineProperty('CalColors');
    RedefineProperty('Constraints');
    RedefineProperty('MultiSelect');
    RedefineProperty('Date');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('EndDate');
    RedefineProperty('FirstDayOfWeek');
    RedefineProperty('Font');
    RedefineProperty('ImeMode');
    RedefineProperty('ImeName');
    RedefineProperty('MaxDate');
    RedefineProperty('MaxSelectRange');
    RedefineProperty('MinDate');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('ShowToday');
    RedefineProperty('ShowTodayCircle');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Visible');
    RedefineProperty('WeekNumbers');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnGetMonthInfo');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{-----------------------}
{ EDateTimeError import }
{-----------------------}

class function TSepiImportsEDateTimeError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EDateTimeError));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ TDateTimePicker import }
{------------------------}

function TSepiImportsTDateTimePicker.GetTime: TTime;
begin
  Result := Time;
end;

procedure TSepiImportsTDateTimePicker.SetCalAlignment(Value: TDTCalAlignment);
begin
  CalAlignment := Value;
end;

procedure TSepiImportsTDateTimePicker.SetChecked(Value: Boolean);
begin
  Checked := Value;
end;

procedure TSepiImportsTDateTimePicker.SetDateMode(Value: TDTDateMode);
begin
  DateMode := Value;
end;

procedure TSepiImportsTDateTimePicker.SetDateFormat(Value: TDTDateFormat);
begin
  DateFormat := Value;
end;

procedure TSepiImportsTDateTimePicker.SetKind(Value: TDateTimeKind);
begin
  Kind := Value;
end;

procedure TSepiImportsTDateTimePicker.SetParseInput(Value: Boolean);
begin
  ParseInput := Value;
end;

procedure TSepiImportsTDateTimePicker.SetShowCheckbox(Value: Boolean);
begin
  ShowCheckbox := Value;
end;

procedure TSepiImportsTDateTimePicker.SetTime(Value: TTime);
begin
  Time := Value;
end;

procedure TSepiImportsTDateTimePicker.SetFormat(const Value: String);
begin
  Format := Value;
end;

class function TSepiImportsTDateTimePicker.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDateTimePicker));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCalAlignment', System.TypeInfo(TDTCalAlignment));
    AddField('FChanging', System.TypeInfo(Boolean));
    AddField('FChecked', System.TypeInfo(Boolean));
    AddField('FDateFormat', System.TypeInfo(TDTDateFormat));
    AddField('FDateMode', System.TypeInfo(TDTDateMode));
    AddField('FDroppedDown', System.TypeInfo(Boolean));
    AddField('FKind', System.TypeInfo(TDateTimeKind));
    AddField('FLastChange', 'TSystemTime');
    AddField('FParseInput', System.TypeInfo(Boolean));
    AddField('FShowCheckbox', System.TypeInfo(Boolean));
    AddField('FOnUserInput', System.TypeInfo(TDTParseInputEvent));
    AddField('FOnCloseUp', System.TypeInfo(TNotifyEvent));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnDropDown', System.TypeInfo(TNotifyEvent));
    AddField('FFormat', System.TypeInfo(String));

    AddMethod('AdjustHeight', nil,
      'procedure');
    AddMethod('GetTime', @TSepiImportsTDateTimePicker.GetTime,
      'function: TTime');
    AddMethod('SetCalAlignment', @TSepiImportsTDateTimePicker.SetCalAlignment,
      'procedure(Value: TDTCalAlignment)');
    AddMethod('SetChecked', @TSepiImportsTDateTimePicker.SetChecked,
      'procedure(Value: Boolean)');
    AddMethod('SetDateMode', @TSepiImportsTDateTimePicker.SetDateMode,
      'procedure(Value: TDTDateMode)');
    AddMethod('SetDateFormat', @TSepiImportsTDateTimePicker.SetDateFormat,
      'procedure(Value: TDTDateFormat)');
    AddMethod('SetKind', @TSepiImportsTDateTimePicker.SetKind,
      'procedure(Value: TDateTimeKind)');
    AddMethod('SetParseInput', @TSepiImportsTDateTimePicker.SetParseInput,
      'procedure(Value: Boolean)');
    AddMethod('SetShowCheckbox', @TSepiImportsTDateTimePicker.SetShowCheckbox,
      'procedure(Value: Boolean)');
    AddMethod('SetTime', @TSepiImportsTDateTimePicker.SetTime,
      'procedure(Value: TTime)');
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMFontChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_FONTCHANGED);
    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('SetFormat', @TSepiImportsTDateTimePicker.SetFormat,
      'procedure(const Value: String)');

    CurrentVisibility := mvProtected;

    AddMethod('CheckEmptyDate', @TSepiImportsTDateTimePicker.CheckEmptyDate,
      'procedure',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTDateTimePicker.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTDateTimePicker.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('Change', @TSepiImportsTDateTimePicker.Change,
      'procedure',
      mlkDynamic);
    AddMethod('GetCalendarHandle',
      @TSepiImportsTDateTimePicker.GetCalendarHandle,
      'function: HWND',
      mlkOverride);
    AddMethod('MsgSetCalColors', @TSepiImportsTDateTimePicker.MsgSetCalColors,
      'function(ColorIndex: Integer; ColorValue: TColor): Boolean',
      mlkOverride);
    AddMethod('MsgSetDateTime', @TSepiImportsTDateTimePicker.MsgSetDateTime,
      'function(Value: TSystemTime): Boolean',
      mlkOverride);
    AddMethod('MsgSetRange', @TSepiImportsTDateTimePicker.MsgSetRange,
      'function(Flags: Integer; SysTime: PSystemTime): Boolean',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTDateTimePicker.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    RedefineProperty('DateTime');
    AddProperty('DroppedDown', 'property: Boolean',
      'FDroppedDown', '');

    CurrentVisibility := mvPublished;

    RedefineProperty('Anchors');
    RedefineProperty('BevelEdges');
    RedefineProperty('BevelInner');
    RedefineProperty('BevelOuter');
    RedefineProperty('BevelKind',
      '', '', Integer(bkNone));
    RedefineProperty('BevelWidth');
    RedefineProperty('BiDiMode');
    AddProperty('CalAlignment', 'property: TDTCalAlignment',
      'FCalAlignment', 'SetCalAlignment',
      NoIndex, Integer(dtaLeft));
    RedefineProperty('CalColors');
    RedefineProperty('Constraints');
    RedefineProperty('Date');
    AddProperty('Format', 'property: String',
      'FFormat', 'SetFormat');
    AddProperty('Time', 'property: TTime',
      'GetTime', 'SetTime');
    AddProperty('ShowCheckbox', 'property: Boolean',
      'FShowCheckbox', 'SetShowCheckbox',
      NoIndex, Integer(False));
    AddProperty('Checked', 'property: Boolean',
      'FChecked', 'SetChecked',
      NoIndex, Integer(True));
    RedefineProperty('Color',
      '', '', Integer(clWindow), 'True');
    AddProperty('DateFormat', 'property: TDTDateFormat',
      'FDateFormat', 'SetDateFormat',
      NoIndex, Integer(dfShort));
    AddProperty('DateMode', 'property: TDTDateMode',
      'FDateMode', 'SetDateMode',
      NoIndex, Integer(dmComboBox));
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ImeMode');
    RedefineProperty('ImeName');
    AddProperty('Kind', 'property: TDateTimeKind',
      'FKind', 'SetKind',
      NoIndex, Integer(dtkDate));
    RedefineProperty('MaxDate');
    RedefineProperty('MinDate');
    AddProperty('ParseInput', 'property: Boolean',
      'FParseInput', 'SetParseInput',
      NoIndex, Integer(False));
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor',
      '', '', Integer(False));
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop',
      '', '', Integer(True));
    RedefineProperty('Visible');
    RedefineProperty('OnClick');
    AddProperty('OnCloseUp', 'property: TNotifyEvent',
      'FOnCloseUp', 'FOnCloseUp');
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    RedefineProperty('OnContextPopup');
    AddProperty('OnDropDown', 'property: TNotifyEvent',
      'FOnDropDown', 'FOnDropDown');
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
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    AddProperty('OnUserInput', 'property: TDTParseInputEvent',
      'FOnUserInput', 'FOnUserInput');

    Complete;
  end;
end;

{----------------------}
{ TPageScroller import }
{----------------------}

procedure TSepiImportsTPageScroller.SetAutoScroll(Value: Boolean);
begin
  AutoScroll := Value;
end;

procedure TSepiImportsTPageScroller.SetButtonSize(Value: Integer);
begin
  ButtonSize := Value;
end;

procedure TSepiImportsTPageScroller.SetControl(Value: TWinControl);
begin
  Control := Value;
end;

procedure TSepiImportsTPageScroller.SetDragScroll(Value: Boolean);
begin
  DragScroll := Value;
end;

procedure TSepiImportsTPageScroller.SetMargin(Value: Integer);
begin
  Margin := Value;
end;

procedure TSepiImportsTPageScroller.SetOrientation(Value:
  TPageScrollerOrientation);
begin
  Orientation := Value;
end;

procedure TSepiImportsTPageScroller.SetPosition(Value: Integer);
begin
  Position := Value;
end;

class function TSepiImportsTPageScroller.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPageScroller));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAutoScroll', System.TypeInfo(Boolean));
    AddField('FButtonSize', System.TypeInfo(Integer));
    AddField('FControl', System.TypeInfo(TWinControl));
    AddField('FDragScroll', System.TypeInfo(Boolean));
    AddField('FMargin', System.TypeInfo(Integer));
    AddField('FOrientation', System.TypeInfo(TPageScrollerOrientation));
    AddField('FPosition', System.TypeInfo(Integer));
    AddField('FPreferredSize', System.TypeInfo(Integer));
    AddField('FOnScroll', System.TypeInfo(TPageScrollEvent));

    AddMethod('CNNotify', nil,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('DoSetControl', nil,
      'procedure(Value: TWinControl)');
    AddMethod('SetAutoScroll', @TSepiImportsTPageScroller.SetAutoScroll,
      'procedure(Value: Boolean)');
    AddMethod('SetButtonSize', @TSepiImportsTPageScroller.SetButtonSize,
      'procedure(Value: Integer)');
    AddMethod('SetControl', @TSepiImportsTPageScroller.SetControl,
      'procedure(Value: TWinControl)');
    AddMethod('SetDragScroll', @TSepiImportsTPageScroller.SetDragScroll,
      'procedure(Value: Boolean)');
    AddMethod('SetMargin', @TSepiImportsTPageScroller.SetMargin,
      'procedure(Value: Integer)');
    AddMethod('SetOrientation', @TSepiImportsTPageScroller.SetOrientation,
      'procedure(Value: TPageScrollerOrientation)');
    AddMethod('SetPosition', @TSepiImportsTPageScroller.SetPosition,
      'procedure(Value: Integer)');
    AddMethod('UpdatePreferredSize', nil,
      'procedure');
    AddMethod('WMNCHitTest', nil,
      'procedure(var Message: TWMNCHitTest)',
      mlkMessage, False, WM_NCHITTEST);
    AddMethod('CMControlChange', nil,
      'procedure(var Message: TCMControlChange)',
      mlkMessage, False, CM_CONTROLCHANGE);
    AddMethod('CMColorChanged', nil,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);

    CurrentVisibility := mvProtected;

    AddMethod('AlignControls', @TSepiImportsTPageScroller.AlignControls,
      'procedure(AControl: TControl; var Rect: TRect)',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTPageScroller.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTPageScroller.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTPageScroller.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation)',
      mlkOverride);
    AddMethod('Scroll', @TSepiImportsTPageScroller.Scroll,
      'procedure(Shift: TShiftState; X, Y: Integer; Orientation: TPageScrollerOrientation ; var Delta: Integer )',
      mlkDynamic);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPageScroller.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('GetButtonState', @TSepiImportsTPageScroller.GetButtonState,
      'function(Button: TPageScrollerButton): TPageScrollerButtonState');

    CurrentVisibility := mvPublished;

    RedefineProperty('Align');
    RedefineProperty('Anchors');
    AddProperty('AutoScroll', 'property: Boolean',
      'FAutoScroll', 'SetAutoScroll',
      NoIndex, Integer(False));
    RedefineProperty('BorderWidth');
    AddProperty('ButtonSize', 'property: Integer',
      'FButtonSize', 'SetButtonSize',
      NoIndex, 12);
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    AddProperty('Control', 'property: TWinControl',
      'FControl', 'SetControl');
    RedefineProperty('DockSite');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    AddProperty('DragScroll', 'property: Boolean',
      'FDragScroll', 'SetDragScroll',
      NoIndex, Integer(True));
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    AddProperty('Margin', 'property: Integer',
      'FMargin', 'SetMargin',
      NoIndex, 0);
    AddProperty('Orientation', 'property: TPageScrollerOrientation',
      'FOrientation', 'SetOrientation',
      NoIndex, Integer(soHorizontal));
    RedefineProperty('ParentBackground',
      '', '', Integer(True));
    RedefineProperty('ParentColor');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    AddProperty('Position', 'property: Integer',
      'FPosition', 'SetPosition',
      NoIndex, 0);
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop',
      '', '', Integer(True));
    RedefineProperty('Visible');
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
    RedefineProperty('OnMouseWheel');
    RedefineProperty('OnResize');
    AddProperty('OnScroll', 'property: TPageScrollEvent',
      'FOnScroll', 'FOnScroll');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');

    Complete;
  end;
end;

{---------------------}
{ TComboExItem import }
{---------------------}

class function TSepiImportsTComboExItem.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComboExItem));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FSelectedImageIndex', System.TypeInfo(TImageIndex));
    AddField('FOverlayImageIndex', System.TypeInfo(TImageIndex));
    AddField('FIndent', System.TypeInfo(Integer));

    CurrentVisibility := mvProtected;

    AddMethod('SetOverlayImageIndex',
      @TSepiImportsTComboExItem.SetOverlayImageIndex,
      'procedure(const Value: TImageIndex)',
      mlkVirtual);
    AddMethod('SetSelectedImageIndex',
      @TSepiImportsTComboExItem.SetSelectedImageIndex,
      'procedure(const Value: TImageIndex)',
      mlkVirtual);
    AddMethod('SetCaption', @TSepiImportsTComboExItem.SetCaption,
      'procedure(const Value: String)',
      mlkOverride);
    AddMethod('SetData', @TSepiImportsTComboExItem.SetData,
      'procedure(const Value: Pointer)',
      mlkOverride);
    AddMethod('SetDisplayName', @TSepiImportsTComboExItem.SetDisplayName,
      'procedure(const Value: String)',
      mlkOverride);
    AddMethod('SetImageIndex', @TSepiImportsTComboExItem.SetImageIndex,
      'procedure(const Value: TImageIndex)',
      mlkOverride);
    AddMethod('SetIndex', @TSepiImportsTComboExItem.SetIndex,
      'procedure(Value: Integer)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Assign', @TSepiImportsTComboExItem.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    AddProperty('Indent', 'property: Integer',
      'FIndent', 'FIndent',
      NoIndex, -1);
    AddProperty('OverlayImageIndex', 'property: TImageIndex',
      'FOverlayImageIndex', 'SetOverlayImageIndex',
      NoIndex, -1);
    AddProperty('SelectedImageIndex', 'property: TImageIndex',
      'FSelectedImageIndex', 'SetSelectedImageIndex',
      NoIndex, -1);

    Complete;
  end;
end;

{----------------------}
{ TComboExItems import }
{----------------------}

function TSepiImportsTComboExItems.GetComboItem(
  const Index: Integer): TComboExItem;
begin
  Result := ComboItems[Index];
end;

class function TSepiImportsTComboExItems.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComboExItems));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetComboItem', @TSepiImportsTComboExItems.GetComboItem,
      'function(const Index: Integer): TComboExItem');

    CurrentVisibility := mvProtected;

    AddMethod('Notify', @TSepiImportsTComboExItems.Notify,
      'procedure(Item: TCollectionItem; Action: TCollectionNotification )',
      mlkOverride);
    AddMethod('SetItem', @TSepiImportsTComboExItems.SetItem,
      'procedure(const Index: Integer)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Add', @TSepiImportsTComboExItems.Add,
      'function: TComboExItem');
    AddMethod('AddItem', @TSepiImportsTComboExItems.AddItem,
      'function(const Caption: String; const ImageIndex, SelectedImageIndex, OverlayImageIndex , Indent : Integer ; Data: Pointer ) : TComboExItem');
    AddMethod('Insert', @TSepiImportsTComboExItems.Insert,
      'function(Index: Integer): TComboExItem');

    AddProperty('ComboItems', 'property[const Index: Integer]: TComboExItem',
      'GetComboItem', '');

    Complete;
  end;
end;

{---------------------------}
{ TComboBoxExStrings import }
{---------------------------}

function TSepiImportsTComboBoxExStrings.GetSortType: TListItemsSortType;
begin
  Result := SortType;
end;

procedure TSepiImportsTComboBoxExStrings.SetItems(const Value: TComboExItems);
begin
  ItemsEx := Value;
end;

procedure TSepiImportsTComboBoxExStrings.SetSortType(
  const Value: TListItemsSortType);
begin
  SortType := Value;
end;

class function TSepiImportsTComboBoxExStrings.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComboBoxExStrings));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FItems', System.TypeInfo(TComboExItems));

    AddMethod('GetSortType', @TSepiImportsTComboBoxExStrings.GetSortType,
      'function: TListItemsSortType');
    AddMethod('SetItems', @TSepiImportsTComboBoxExStrings.SetItems,
      'procedure(const Value: TComboExItems)');
    AddMethod('SetSortType', @TSepiImportsTComboBoxExStrings.SetSortType,
      'procedure(const Value: TListItemsSortType)');

    CurrentVisibility := mvProtected;

    AddMethod('GetItemsClass', @TSepiImportsTComboBoxExStrings.GetItemsClass,
      'function: TComboExItemsClass',
      mlkVirtual);
    AddMethod('GetItemClass', @TSepiImportsTComboBoxExStrings.GetItemClass,
      'function: TComboExItemClass',
      mlkVirtual);
    AddMethod('PutObject', @TSepiImportsTComboBoxExStrings.PutObject,
      'procedure(Index: Integer; AObject: TObject)',
      mlkOverride);
    AddMethod('SetUpdateState', @TSepiImportsTComboBoxExStrings.SetUpdateState,
      'procedure(Updating: Boolean)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTComboBoxExStrings.Create,
      'constructor(Owner: TCustomComboBoxEx)');
    AddMethod('Destroy', @TSepiImportsTComboBoxExStrings.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTComboBoxExStrings.Add,
      'function(const S: String): Integer',
      mlkOverride);
    AddMethod('AddItem', @TSepiImportsTComboBoxExStrings.AddItem,
      'function(const Caption: String; const ImageIndex, SelectedImageIndex, OverlayImageIndex , Indent : Integer ; Data: Pointer ) : TComboExItem');
    AddMethod('AddObject', @TSepiImportsTComboBoxExStrings.AddObject,
      'function(const S: String; AObject: TObject): Integer',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTComboBoxExStrings.Clear,
      'procedure',
      mlkOverride);
    AddMethod('Delete', @TSepiImportsTComboBoxExStrings.Delete,
      'procedure(Index: Integer)',
      mlkOverride);
    AddMethod('Exchange', @TSepiImportsTComboBoxExStrings.Exchange,
      'procedure(Index1: Integer; Index2: Integer)',
      mlkOverride);
    AddMethod('Get', @TSepiImportsTComboBoxExStrings.Get,
      'function(Index: Integer): String',
      mlkOverride);
    AddMethod('GetCapacity', @TSepiImportsTComboBoxExStrings.GetCapacity,
      'function: Integer',
      mlkOverride);
    AddMethod('GetCount', @TSepiImportsTComboBoxExStrings.GetCount,
      'function: Integer',
      mlkOverride);
    AddMethod('GetObject', @TSepiImportsTComboBoxExStrings.GetObject,
      'function(Index: Integer): TObject',
      mlkOverride);
    AddMethod('IndexOf', @TSepiImportsTComboBoxExStrings.IndexOf,
      'function(const S: String): Integer',
      mlkOverride);
    AddMethod('IndexOfName', @TSepiImportsTComboBoxExStrings.IndexOfName,
      'function(const Name: String): Integer',
      mlkOverride);
    AddMethod('Insert', @TSepiImportsTComboBoxExStrings.Insert,
      'procedure(Index: Integer; const S: String)',
      mlkOverride);
    AddMethod('Move', @TSepiImportsTComboBoxExStrings.Move,
      'procedure(CurIndex: Integer; NewIndex: Integer)',
      mlkOverride);

    AddProperty('SortType', 'property: TListItemsSortType',
      'GetSortType', 'SetSortType');
    AddProperty('ItemsEx', 'property: TComboExItems',
      'FItems', 'SetItems');

    Complete;
  end;
end;

{--------------------------}
{ TCustomComboBoxEx import }
{--------------------------}

function TSepiImportsTCustomComboBoxEx.GetSelText: String;
begin
  Result := SelText;
end;

procedure TSepiImportsTCustomComboBoxEx.SetImages(
  const Value: TCustomImageList);
begin
  Images := Value;
end;

procedure TSepiImportsTCustomComboBoxEx.SetSelText(const Value: String);
begin
  SelText := Value;
end;

procedure TSepiImportsTCustomComboBoxEx.SetStyle(Value: TComboBoxExStyle);
begin
  Style := Value;
end;

procedure TSepiImportsTCustomComboBoxEx.SetItemsEx(const Value: TComboExItems);
begin
  ItemsEx := Value;
end;

procedure TSepiImportsTCustomComboBoxEx.SetStyleEx(
  const Value: TComboBoxExStyles);
begin
  StyleEx := Value;
end;

function TSepiImportsTCustomComboBoxEx.GetDropDownCount: Integer;
begin
  Result := DropDownCount;
end;

procedure TSepiImportsTCustomComboBoxEx.SetAutoCompleteOptions(
  const Value: TAutoCompleteOptions);
begin
  AutoCompleteOptions := Value;
end;

class function TSepiImportsTCustomComboBoxEx.SepiImport(
  Owner: TSepiUnit): TSepiClass;
const
  DefaultAutoCompleteOptions: TAutoCompleteOptions = [acoAutoAppend];
begin
  Result := TSepiClass(Owner.FindMeta('TCustomComboBoxEx'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomComboBoxEx));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FAutoCompleteIntf', System.TypeInfo(IAutoComplete));
    AddField('FAutoCompleteOptions', System.TypeInfo(TAutoCompleteOptions));
    AddField('FComboBoxExHandle', System.TypeInfo(HWND));
    AddField('FComboBoxExDefProc', 'Pointer');
    AddField('FComboBoxExInstance', 'Pointer');
    AddField('FImageChangeLink', System.TypeInfo(TChangeLink));
    AddField('FImages', System.TypeInfo(TCustomImageList));
    AddField('FMemStream', System.TypeInfo(TCollection));
    AddField('FReading', System.TypeInfo(Boolean));
    AddField('FStyle', System.TypeInfo(TComboBoxExStyle));
    AddField('FStyleEx', System.TypeInfo(TComboBoxExStyles));
    AddField('FItemsEx', System.TypeInfo(TComboExItems));
    AddField('FOnBeginEdit', System.TypeInfo(TNotifyEvent));
    AddField('FOnEndEdit', System.TypeInfo(TNotifyEvent));

    AddMethod('GetSelText', @TSepiImportsTCustomComboBoxEx.GetSelText,
      'function: String');
    AddMethod('ImageListChange', nil,
      'procedure(Sender: TObject)');
    AddMethod('SetImages', @TSepiImportsTCustomComboBoxEx.SetImages,
      'procedure(const Value: TCustomImageList)');
    AddMethod('SetSelText', @TSepiImportsTCustomComboBoxEx.SetSelText,
      'procedure(const Value: String)');
    AddMethod('SetStyle', @TSepiImportsTCustomComboBoxEx.SetStyle,
      'procedure(Value: TComboBoxExStyle)');
    AddMethod('SetItemsEx', @TSepiImportsTCustomComboBoxEx.SetItemsEx,
      'procedure(const Value: TComboExItems)');
    AddMethod('SetStyleEx', @TSepiImportsTCustomComboBoxEx.SetStyleEx,
      'procedure(const Value: TComboBoxExStyles)');
    AddMethod('IsItemsExStored', nil,
      'function: Boolean');
    AddMethod('GetDropDownCount',
      @TSepiImportsTCustomComboBoxEx.GetDropDownCount,
      'function: Integer');
    AddMethod('UpdateAutoComplete', nil,
      'procedure');
    AddMethod('SetAutoCompleteOptions',
      @TSepiImportsTCustomComboBoxEx.SetAutoCompleteOptions,
      'procedure(const Value: TAutoCompleteOptions)');

    CurrentVisibility := mvProtected;

    AddMethod('ActionChange', @TSepiImportsTCustomComboBoxEx.ActionChange,
      'procedure(Sender: TObject; CheckDefaults: Boolean)',
      mlkOverride);
    AddMethod('CMColorChanged', @TSepiImportsTCustomComboBoxEx.CMColorChanged,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_COLORCHANGED);
    AddMethod('CMParentColorChanged',
      @TSepiImportsTCustomComboBoxEx.CMParentColorChanged,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_PARENTCOLORCHANGED);
    AddMethod('CNNotify', @TSepiImportsTCustomComboBoxEx.CNNotify,
      'procedure(var Message: TWMNotify)',
      mlkMessage, False, CN_NOTIFY);
    AddMethod('ComboExWndProc', @TSepiImportsTCustomComboBoxEx.ComboExWndProc,
      'procedure(var Message: TMessage)');
    AddMethod('CreateParams', @TSepiImportsTCustomComboBoxEx.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('CreateWnd', @TSepiImportsTCustomComboBoxEx.CreateWnd,
      'procedure',
      mlkOverride);
    AddMethod('DestroyWnd', @TSepiImportsTCustomComboBoxEx.DestroyWnd,
      'procedure',
      mlkOverride);
    AddMethod('GetActionLinkClass',
      @TSepiImportsTCustomComboBoxEx.GetActionLinkClass,
      'function: TControlActionLinkClass',
      mlkOverride);
    AddMethod('GetItemsClass', @TSepiImportsTCustomComboBoxEx.GetItemsClass,
      'function: TCustomComboBoxStringsClass',
      mlkOverride);
    AddMethod('GetItemCount', @TSepiImportsTCustomComboBoxEx.GetItemCount,
      'function: Integer',
      mlkOverride);
    AddMethod('GetItemHt', @TSepiImportsTCustomComboBoxEx.GetItemHt,
      'function: Integer',
      mlkOverride);
    AddMethod('Notification', @TSepiImportsTCustomComboBoxEx.Notification,
      'procedure(AComponent: TComponent; Operation: TOperation )',
      mlkOverride);
    AddMethod('SetDropDownCount',
      @TSepiImportsTCustomComboBoxEx.SetDropDownCount,
      'procedure(const Value: Integer)',
      mlkOverride);
    AddMethod('WMLButtonDown', @TSepiImportsTCustomComboBoxEx.WMLButtonDown,
      'procedure(var Message: TWMLButtonDown)',
      mlkMessage, False, WM_LBUTTONDOWN);
    AddMethod('WndProc', @TSepiImportsTCustomComboBoxEx.WndProc,
      'procedure(var Message: TMessage)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomComboBoxEx.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTCustomComboBoxEx.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Focused', @TSepiImportsTCustomComboBoxEx.Focused,
      'function: Boolean',
      mlkOverride);

    AddProperty('AutoCompleteOptions', 'property: TAutoCompleteOptions',
      'FAutoCompleteOptions', 'SetAutoCompleteOptions',
      NoIndex, Byte(DefaultAutoCompleteOptions));
    AddProperty('DropDownCount', 'property: Integer',
      'GetDropDownCount', 'SetDropDownCount');
    AddProperty('Images', 'property: TCustomImageList',
      'FImages', 'SetImages');
    AddProperty('ItemsEx', 'property: TComboExItems',
      'FItemsEx', 'SetItemsEx',
      NoIndex, NoDefaultValue, 'IsItemsExStored');
    AddProperty('SelText', 'property: string',
      'GetSelText', 'SetSelText');
    AddProperty('Style', 'property: TComboBoxExStyle',
      'FStyle', 'SetStyle',
      NoIndex, Integer(csExDropDown));
    AddProperty('StyleEx', 'property: TComboBoxExStyles',
      'FStyleEx', 'SetStyleEx',
      NoIndex, 0);
    AddProperty('OnBeginEdit', 'property: TNotifyEvent',
      'FOnBeginEdit', 'FOnBeginEdit');
    AddProperty('OnEndEdit', 'property: TNotifyEvent',
      'FOnEndEdit', 'FOnEndEdit');

    Complete;
  end;
end;

{--------------------}
{ TComboBoxEx import }
{--------------------}

class function TSepiImportsTComboBoxEx.SepiImport(
  Owner: TSepiUnit): TSepiClass;
const
  DefaultAutoCompleteOptions: TAutoCompleteOptions = [acoAutoAppend];
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComboBoxEx));

  with Result do
  begin
    CurrentVisibility := mvPublished;

    RedefineProperty('AutoCompleteOptions',
      '', '', Byte(DefaultAutoCompleteOptions));
    RedefineProperty('ItemsEx');
    RedefineProperty('Style');
    RedefineProperty('StyleEx');
    RedefineProperty('Action');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('Color');
    RedefineProperty('Constraints');
    RedefineProperty('Ctl3D');
    RedefineProperty('DragCursor');
    RedefineProperty('DragKind');
    RedefineProperty('DragMode');
    RedefineProperty('Enabled');
    RedefineProperty('Font');
    RedefineProperty('ImeMode');
    RedefineProperty('ImeName');
    RedefineProperty('ItemHeight');
    RedefineProperty('MaxLength');
    RedefineProperty('ParentBiDiMode');
    RedefineProperty('ParentColor');
    RedefineProperty('ParentCtl3D');
    RedefineProperty('ParentFont');
    RedefineProperty('ParentShowHint');
    RedefineProperty('PopupMenu');
    RedefineProperty('ShowHint');
    RedefineProperty('TabOrder');
    RedefineProperty('TabStop');
    RedefineProperty('Text');
    RedefineProperty('Visible');
    RedefineProperty('OnBeginEdit');
    RedefineProperty('OnChange');
    RedefineProperty('OnClick');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnDropDown');
    RedefineProperty('OnEndEdit');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnEndDrag');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnSelect');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('Images');
    RedefineProperty('DropDownCount');

    Complete;
  end;
end;

{------------------------------}
{ TComboBoxExActionLink import }
{------------------------------}

class function TSepiImportsTComboBoxExActionLink.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComboBoxExActionLink));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddOverloadedMethod('AddItem', nil,
      'procedure(AnItem: TListControlItem)',
      mlkOverride);
    AddOverloadedMethod('AddItem', nil,
      'procedure(ACaption: String; AImageIndex: Integer; DataPtr: Pointer )',
      mlkOverride);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ComCtrls',
    ['Messages', 'Windows', 'SysUtils', 'CommCtrlTypes', 'Classes', 'Controls',
    'Forms', 'Menus', 'Graphics', 'StdCtrls', 'RichEdit', 'ToolWin', 'ImgList',
    'ExtCtrls', 'ListActns', 'ShlObjTypes']);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THitTest));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THitTests));
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomTabControl));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTabChangingEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTabPosition));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTabStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDrawTabEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTabGetImageEvent));
  TSepiImportsTCustomTabControl.SepiImport(Result);
  TSepiImportsTTabControl.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TPageControl));
  TSepiImportsTTabSheet.SepiImport(Result);
  TSepiImportsTPageControl.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomStatusBar));
  TSepiClass.ForwardDecl(Result, TypeInfo(TStatusPanel));
  TSepiClass.ForwardDecl(Result, TypeInfo(TStatusPanels));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStatusPanelStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStatusPanelBevel));
  TSepiMetaClass.Create(Result, 'TStatusPanelClass',
    TypeInfo(TStatusPanel), True);
  TSepiImportsTStatusPanel.SepiImport(Result);
  TSepiImportsTStatusPanels.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCustomDrawPanelEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSBCreatePanelClassEvent));
  TSepiImportsTCustomStatusBar.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TStatusBar));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDrawPanelEvent));
  TSepiImportsTStatusBar.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCustomDrawTarget));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCustomDrawStage));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCustomDrawState));
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomHeaderControl));
  TSepiClass.ForwardDecl(Result, TypeInfo(THeaderControl));
  TSepiClass.ForwardDecl(Result, TypeInfo(THeaderSection));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THeaderSectionStyle));
  TSepiMetaClass.Create(Result, 'THeaderSectionClass',
    TypeInfo(THeaderSection), True);
  TSepiImportsTHeaderSection.SepiImport(Result);
  TSepiImportsTHeaderSections.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSectionTrackState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCustomDrawSectionEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCustomSectionNotifyEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCustomSectionTrackEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSectionDragEvent));
  TSepiType.LoadFromTypeInfo(Result,
    TypeInfo(TCustomHCCreateSectionClassEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THeaderStyle));
  TSepiImportsTCustomHeaderControl.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDrawSectionEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSectionNotifyEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSectionTrackEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THCCreateSectionClassEvent));
  TSepiImportsTHeaderControl.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomTreeView));
  TSepiClass.ForwardDecl(Result, TypeInfo(TTreeNode));
  TSepiClass.ForwardDecl(Result, TypeInfo(TTreeNodes));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TNodeState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TNodeAttachMode));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAddMode));
  TSepiPointerType.Create(Result, 'PNodeInfo', 'TNodeInfo', True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_1));
  SepiImportTNodeInfo(Result);
  SepiImportTNodeDataInfo(Result);
  TSepiMetaClass.Create(Result, 'TTreeNodeClass', TypeInfo(TTreeNode), True);
  TSepiImportsTTreeNode.SepiImport(Result);
  TSepiImportsTTreeNodesEnumerator.SepiImport(Result);
  TSepiPointerType.Create(Result, 'PNodeCache', 'TNodeCache', True);
  SepiImportTNodeCache(Result);
  TSepiImportsTTreeNodes.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSortType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMultiSelectStyles));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMultiSelectStyle));
  TSepiImportsETreeViewError.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVChangingEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVChangedEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVEditingEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVEditedEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVExpandingEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVCollapsingEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVExpandedEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVCompareEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVCustomDrawEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVCustomDrawItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVAdvancedCustomDrawEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVAdvancedCustomDrawItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTVCreateNodeClassEvent));
  TSepiImportsTCustomTreeView.SepiImport(Result);
  TSepiImportsTTreeView.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTrackBarOrientation));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTickMark));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTickStyle));
  TSepiImportsTTrackBar.SepiImport(Result);
  TSepiTypeAlias.Create(Result, 'TProgressRange', TypeInfo(Integer));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TProgressBarOrientation));
  TSepiImportsTProgressBar.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomRichEdit));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAttributeType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConsistentAttribute));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TConsistentAttributes));
  TSepiImportsTTextAttributes.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TNumberingStyle));
  TSepiImportsTParaAttributes.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TRichEditResizeEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TRichEditProtectChange));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TRichEditSaveClipboard));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSearchType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSearchTypes));
  TSepiImportsTConversion.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TConversionClass',
    TypeInfo(TConversion), True);
  TSepiPointerType.Create(Result, 'PConversionFormat',
    TypeInfo(TConversionFormat), True);
  SepiImportTConversionFormat(Result);
  TSepiPointerType.Create(Result, 'PRichEditStreamInfo',
    'TRichEditStreamInfo', True);
  SepiImportTRichEditStreamInfo(Result);
  TSepiImportsTCustomRichEdit.SepiImport(Result);
  TSepiImportsTRichEdit.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUDAlignButton));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUDOrientation));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUDBtnType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUpDownDirection));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUDClickEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUDChangingEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUDChangingEventEx));
  TSepiImportsTCustomUpDown.SepiImport(Result);
  TSepiImportsTUpDown.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THKModifier));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THKModifiers));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THKInvalidKey));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(THKInvalidKeys));
  TSepiImportsTCustomHotKey.SepiImport(Result);
  TSepiImportsTHotKey.SepiImport(Result);

  // Constants
  TSepiConstant.Create(Result, 'ColumnHeaderWidth', ColumnHeaderWidth);
  TSepiConstant.Create(Result, 'ColumnTextWidth', ColumnTextWidth);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TListColumns));
  TSepiClass.ForwardDecl(Result, TypeInfo(TListItem));
  TSepiClass.ForwardDecl(Result, TypeInfo(TListItems));
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomListView));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TWidth));
  TSepiImportsTListColumn.SepiImport(Result);
  TSepiImportsTListColumns.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDisplayCode));
  TSepiMetaClass.Create(Result, 'TListItemClass', TypeInfo(TListItem), True);
  TSepiImportsTListItem.SepiImport(Result);
  TSepiImportsTListItemsEnumerator.SepiImport(Result);
  TSepiImportsTListItems.SepiImport(Result);
  TSepiImportsTWorkArea.SepiImport(Result);
  TSepiImportsTWorkAreas.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TIconArrangement));
  TSepiImportsTIconOptions.SepiImport(Result);
  TSepiTypeAlias.Create(Result, 'TOwnerDrawState',
    TypeInfo(Windows.TOwnerDrawState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TListArrangement));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TViewStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TItemState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TItemStates));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TItemChange));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TItemFind));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSearchDirection));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TListHotTrackStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TListHotTrackStyles));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TItemRequests));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TItemRequest));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVDeletedEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVEditingEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVEditedEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVChangeEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVChangingEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVColumnClickEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVColumnRClickEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVCompareEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVNotifyEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVSelectItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVDrawItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVCustomDrawEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVCustomDrawItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVCustomDrawSubItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVAdvancedCustomDrawEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVAdvancedCustomDrawItemEvent));
  TSepiType.LoadFromTypeInfo(Result,
    TypeInfo(TLVAdvancedCustomDrawSubItemEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVOwnerDataEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVOwnerDataFindEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVOwnerDataHintEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVOwnerDataStateChangeEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVSubItemImageEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVInfoTipEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLVCreateItemClassEvent));
  TSepiImportsTCustomListView.SepiImport(Result);
  TSepiImportsTListView.SepiImport(Result);
  TSepiImportsTListViewActionLink.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCommonAVI));
  TSepiImportsTAnimate.SepiImport(Result);

  // Constants
  TSepiConstant.Create(Result, 'CN_DROPDOWNCLOSED', CN_DROPDOWNCLOSED);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TToolButtonStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TToolButtonState));
  TSepiClass.ForwardDecl(Result, TypeInfo(TToolBar));
  TSepiClass.ForwardDecl(Result, TypeInfo(TToolButton));
  TSepiImportsTToolButtonActionLink.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TToolButtonActionLinkClass',
    TypeInfo(TToolButtonActionLink), True);
  TSepiImportsTToolButton.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTBCustomDrawFlags));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTBCustomDrawEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTBCustomDrawBtnEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTBAdvancedCustomDrawEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTBAdvancedCustomDrawBtnEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTBCustomizeQueryEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTBNewButtonEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTBButtonEvent));
  TSepiImportsTToolBarEnumerator.SepiImport(Result);
  TSepiImportsTToolBar.SepiImport(Result);
  TSepiImportsTToolBarDockObject.SepiImport(Result);

  // Constants
  TSepiConstant.Create(Result, 'CN_BANDCHANGE', CN_BANDCHANGE);

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TCoolBar));
  TSepiImportsTCoolBand.SepiImport(Result);
  TSepiImportsTCoolBands.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCoolBandMaximize));
  TSepiImportsTCoolBar.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCommonCalendar));
  TSepiImportsECommonCalendarError.SepiImport(Result);
  TSepiImportsTMonthCalColors.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCalDayOfWeek));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOnGetMonthInfoEvent));
  TSepiImportsTCommonCalendar.SepiImport(Result);
  TSepiImportsEMonthCalError.SepiImport(Result);
  TSepiImportsTMonthCalendar.SepiImport(Result);
  TSepiImportsEDateTimeError.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDateTimeKind));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDTDateMode));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDTDateFormat));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDTCalAlignment));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDTParseInputEvent));
  TSepiTypeAlias.Create(Result, 'TDateTimeColors', TypeInfo(TMonthCalColors));
  TSepiImportsTDateTimePicker.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPageScrollerOrientation));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPageScrollerButton));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPageScrollerButtonState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPageScrollEvent));
  TSepiImportsTPageScroller.SepiImport(Result);
  TSepiImportsTComboExItem.SepiImport(Result);
  TSepiImportsTComboExItems.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TComboExItemsClass',
    TypeInfo(TComboExItems), True);
  TSepiMetaClass.Create(Result, 'TComboExItemClass',
    TypeInfo(TComboExItem), True);
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomComboBoxEx));
  TSepiImportsTComboBoxExStrings.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TComboBoxExStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TComboBoxExStyleEx));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TComboBoxExStyles));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAutoCompleteOption));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TAutoCompleteOptions));
  TSepiImportsTCustomComboBoxEx.SepiImport(Result);
  TSepiImportsTComboBoxEx.SepiImport(Result);
  TSepiImportsTComboBoxExActionLink.SepiImport(Result);

  // Routines
  TSepiMethod.Create(Result, 'InitCommonControl', @InitCommonControl,
    'function(CC: Integer): Boolean');
  TSepiMethod.Create(Result, 'CheckCommonControl', @CheckCommonControl,
    'procedure(CC: Integer)');

  // Constants
  TSepiConstant.Create(Result, 'ComCtlVersionIE3', ComCtlVersionIE3);
  TSepiConstant.Create(Result, 'ComCtlVersionIE4', ComCtlVersionIE4);
  TSepiConstant.Create(Result, 'ComCtlVersionIE401', ComCtlVersionIE401);
  TSepiConstant.Create(Result, 'ComCtlVersionIE5', ComCtlVersionIE5);
  TSepiConstant.Create(Result, 'ComCtlVersionIE501', ComCtlVersionIE501);
  TSepiConstant.Create(Result, 'ComCtlVersionIE6', ComCtlVersionIE6);

  // Routines
  TSepiMethod.Create(Result, 'GetComCtlVersion', @GetComCtlVersion,
    'function: Integer');
  TSepiMethod.Create(Result, 'CheckToolMenuDropdown', @CheckToolMenuDropdown,
    'procedure(ToolButton: TToolButton)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ComCtrls', ImportUnit);
end.

