{*
  Importe l'unité ImgList dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsImgList;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Windows, Classes, Graphics, CommCtrl, ImgList;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTChangeLink = class(TChangeLink)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTCustomImageList = class(TCustomImageList)
  private
    function GetCount: Integer;
    function GetBkColor: TColor;
    function GetHandle: HImageList;
    procedure SetBkColor(Value: TColor);
    procedure SetDrawingStyle(Value: TDrawingStyle);
    procedure SetHandle(Value: HImageList);
    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure Draw_0(Canvas: TCanvas; X, Y, Index: Integer; Enabled: Boolean = True );
    procedure Draw_1(Canvas: TCanvas; X, Y, Index: Integer; ADrawingStyle: TDrawingStyle ; AImageType: TImageType ; Enabled: Boolean = True );
    procedure DrawOverlay_0(Canvas: TCanvas; X, Y: Integer; ImageIndex: Integer ; Overlay: TOverlay ; Enabled: Boolean = True );
    procedure DrawOverlay_1(Canvas: TCanvas; X, Y: Integer; ImageIndex: Integer ; Overlay: TOverlay ; ADrawingStyle: TDrawingStyle ; AImageType: TImageType ; Enabled: Boolean = True );
    procedure GetIcon_0(Index: Integer; Image: TIcon);
    procedure GetIcon_1(Index: Integer; Image: TIcon; ADrawingStyle: TDrawingStyle; AImageType: TImageType );
    function GetInstRes_0(Instance: THandle; ResType: TResType; const Name: string; Width: Integer ; LoadFlags: TLoadResources ; MaskColor: TColor ) : Boolean;
    function GetInstRes_1(Instance: THandle; ResType: TResType; ResID: DWORD; Width: Integer ; LoadFlags: TLoadResources ; MaskColor: TColor ) : Boolean;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{--------------------}
{ TChangeLink import }
{--------------------}

class function TSepiImportsTChangeLink.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TChangeLink));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FSender', System.TypeInfo(TCustomImageList));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTChangeLink.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Change', @TSepiImportsTChangeLink.Change,
      'procedure',
      mlkDynamic);

    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    AddProperty('Sender', 'property: TCustomImageList',
      'FSender', 'FSender');

    Complete;
  end;
end;

{-------------------------}
{ TCustomImageList import }
{-------------------------}

function TSepiImportsTCustomImageList.GetCount: Integer;
begin
  Result := Count;
end;

function TSepiImportsTCustomImageList.GetBkColor: TColor;
begin
  Result := BkColor;
end;

function TSepiImportsTCustomImageList.GetHandle: HImageList;
begin
  Result := Handle;
end;

procedure TSepiImportsTCustomImageList.SetBkColor(Value: TColor);
begin
  BkColor := Value;
end;

procedure TSepiImportsTCustomImageList.SetDrawingStyle(Value: TDrawingStyle);
begin
  DrawingStyle := Value;
end;

procedure TSepiImportsTCustomImageList.SetHandle(Value: HImageList);
begin
  Handle := Value;
end;

procedure TSepiImportsTCustomImageList.SetHeight(Value: Integer);
begin
  Height := Value;
end;

procedure TSepiImportsTCustomImageList.SetWidth(Value: Integer);
begin
  Width := Value;
end;

procedure TSepiImportsTCustomImageList.Draw_0(Canvas: TCanvas; X, Y, Index: Integer; Enabled: Boolean = True );
begin
  Draw(Canvas, X, Y, Index, Enabled);
end;

procedure TSepiImportsTCustomImageList.Draw_1(Canvas: TCanvas; X, Y, Index: Integer; ADrawingStyle: TDrawingStyle ; AImageType: TImageType ; Enabled: Boolean = True );
begin
  Draw(Canvas, X, Y, Index, ADrawingStyle, AImageType, Enabled);
end;

procedure TSepiImportsTCustomImageList.DrawOverlay_0(Canvas: TCanvas; X, Y: Integer; ImageIndex: Integer ; Overlay: TOverlay ; Enabled: Boolean = True );
begin
  DrawOverlay(Canvas, X, Y, ImageIndex, Overlay, Enabled);
end;

procedure TSepiImportsTCustomImageList.DrawOverlay_1(Canvas: TCanvas; X, Y: Integer; ImageIndex: Integer ; Overlay: TOverlay ; ADrawingStyle: TDrawingStyle ; AImageType: TImageType ; Enabled: Boolean = True );
begin
  DrawOverlay(Canvas, X, Y, ImageIndex, Overlay, ADrawingStyle, AImageType, Enabled);
end;

procedure TSepiImportsTCustomImageList.GetIcon_0(Index: Integer; Image: TIcon);
begin
  GetIcon(Index, Image);
end;

procedure TSepiImportsTCustomImageList.GetIcon_1(Index: Integer; Image: TIcon; ADrawingStyle: TDrawingStyle; AImageType: TImageType );
begin
  GetIcon(Index, Image, ADrawingStyle, AImageType);
end;

function TSepiImportsTCustomImageList.GetInstRes_0(Instance: THandle; ResType: TResType; const Name: string; Width: Integer ; LoadFlags: TLoadResources ; MaskColor: TColor ) : Boolean;
begin
  Result := GetInstRes(Instance, ResType, Name, Width, LoadFlags, MaskColor);
end;

function TSepiImportsTCustomImageList.GetInstRes_1(Instance: THandle; ResType: TResType; ResID: DWORD; Width: Integer ; LoadFlags: TLoadResources ; MaskColor: TColor ) : Boolean;
begin
  Result := GetInstRes(Instance, ResType, ResID, Width, LoadFlags, MaskColor);
end;

class function TSepiImportsTCustomImageList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TCustomImageList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TCustomImageList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHeight', System.TypeInfo(Integer));
    AddField('FWidth', System.TypeInfo(Integer));
    AddField('FAllocBy', System.TypeInfo(Integer));
    AddField('FHandle', System.TypeInfo(HImageList));
    AddField('FDrawingStyle', System.TypeInfo(TDrawingStyle));
    AddField('FMasked', System.TypeInfo(Boolean));
    AddField('FShareImages', System.TypeInfo(Boolean));
    AddField('FImageType', System.TypeInfo(TImageType));
    AddField('FBkColor', System.TypeInfo(TColor));
    AddField('FBlendColor', System.TypeInfo(TColor));
    AddField('FClients', System.TypeInfo(TList));
    AddField('FBitmap', System.TypeInfo(TBitmap));
    AddField('FMonoBitmap', System.TypeInfo(TBitmap));
    AddField('FChanged', System.TypeInfo(Boolean));
    AddField('FUpdateCount', System.TypeInfo(Integer));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));

    AddMethod('BeginUpdate', nil,
      'procedure');
    AddMethod('EndUpdate', nil,
      'procedure');
    AddMethod('InitBitmap', nil,
      'procedure');
    AddMethod('CheckImage', nil,
      'procedure(Image: TGraphic)');
    AddMethod('CopyImages', nil,
      'procedure(Value: HImageList; Index: Integer = -1)');
    AddMethod('CreateImageList', nil,
      'procedure');
    AddMethod('Equal', nil,
      'function(IL: TCustomImageList): Boolean');
    AddMethod('FreeHandle', nil,
      'procedure');
    AddMethod('GetCount', @TSepiImportsTCustomImageList.GetCount,
      'function: Integer');
    AddMethod('GetBitmapHandle', nil,
      'function(Bitmap: HBITMAP): HBITMAP');
    AddMethod('GetBkColor', @TSepiImportsTCustomImageList.GetBkColor,
      'function: TColor');
    AddMethod('GetHandle', @TSepiImportsTCustomImageList.GetHandle,
      'function: HImageList');
    AddMethod('GetImageHandle', nil,
      'function(Image, ImageDDB: TBitmap): HBITMAP');
    AddMethod('InsertImage', nil,
      'procedure(Index: Integer; Image, Mask: TBitmap; MaskColor: TColor)');
    AddMethod('InternalGetInstRes', nil,
      'function(Instance: THandle; ResType: TResType; Name: PChar ; Width: Integer ; LoadFlags: TLoadResources ; MaskColor: TColor ) : Boolean');
    AddMethod('SetBkColor', @TSepiImportsTCustomImageList.SetBkColor,
      'procedure(Value: TColor)');
    AddMethod('SetDrawingStyle', @TSepiImportsTCustomImageList.SetDrawingStyle,
      'procedure(Value: TDrawingStyle)');
    AddMethod('SetHandle', @TSepiImportsTCustomImageList.SetHandle,
      'procedure(Value: HImageList)');
    AddMethod('SetHeight', @TSepiImportsTCustomImageList.SetHeight,
      'procedure(Value: Integer)');
    AddMethod('SetNewDimensions', nil,
      'procedure(Value: HImageList)');
    AddMethod('SetWidth', @TSepiImportsTCustomImageList.SetWidth,
      'procedure(Value: Integer)');
    AddMethod('ReadD2Stream', nil,
      'procedure(Stream: TStream)');
    AddMethod('ReadD3Stream', nil,
      'procedure(Stream: TStream)');

    CurrentVisibility := mvProtected;

    AddMethod('AssignTo', @TSepiImportsTCustomImageList.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);
    AddMethod('Change', @TSepiImportsTCustomImageList.Change,
      'procedure',
      mlkDynamic);
    AddMethod('DefineProperties', @TSepiImportsTCustomImageList.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('DoDraw', @TSepiImportsTCustomImageList.DoDraw,
      'procedure(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal ; Enabled: Boolean = True )',
      mlkVirtual);
    AddMethod('GetImages', @TSepiImportsTCustomImageList.GetImages,
      'procedure(Index: Integer; Image, Mask: TBitmap)');
    AddMethod('HandleNeeded', @TSepiImportsTCustomImageList.HandleNeeded,
      'procedure');
    AddMethod('Initialize', @TSepiImportsTCustomImageList.Initialize,
      'procedure',
      mlkVirtual);
    AddMethod('ReadData', @TSepiImportsTCustomImageList.ReadData,
      'procedure(Stream: TStream)',
      mlkVirtual);
    AddMethod('WriteData', @TSepiImportsTCustomImageList.WriteData,
      'procedure(Stream: TStream)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCustomImageList.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);
    AddMethod('CreateSize', @TSepiImportsTCustomImageList.CreateSize,
      'constructor(AWidth, AHeight: Integer)');
    AddMethod('Destroy', @TSepiImportsTCustomImageList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTCustomImageList.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTCustomImageList.Add,
      'function(Image, Mask: TBitmap): Integer');
    AddMethod('AddIcon', @TSepiImportsTCustomImageList.AddIcon,
      'function(Image: TIcon): Integer');
    AddMethod('AddImage', @TSepiImportsTCustomImageList.AddImage,
      'function(Value: TCustomImageList; Index: Integer): Integer');
    AddMethod('AddImages', @TSepiImportsTCustomImageList.AddImages,
      'procedure(Value: TCustomImageList)');
    AddMethod('AddMasked', @TSepiImportsTCustomImageList.AddMasked,
      'function(Image: TBitmap; MaskColor: TColor): Integer');
    AddMethod('Clear', @TSepiImportsTCustomImageList.Clear,
      'procedure');
    AddMethod('Delete', @TSepiImportsTCustomImageList.Delete,
      'procedure(Index: Integer)');
    AddOverloadedMethod('Draw', @TSepiImportsTCustomImageList.Draw_0,
      'procedure(Canvas: TCanvas; X, Y, Index: Integer; Enabled: Boolean = True )');
    AddOverloadedMethod('Draw', @TSepiImportsTCustomImageList.Draw_1,
      'procedure(Canvas: TCanvas; X, Y, Index: Integer; ADrawingStyle: TDrawingStyle ; AImageType: TImageType ; Enabled: Boolean = True )');
    AddOverloadedMethod('DrawOverlay', @TSepiImportsTCustomImageList.DrawOverlay_0,
      'procedure(Canvas: TCanvas; X, Y: Integer; ImageIndex: Integer ; Overlay: TOverlay ; Enabled: Boolean = True )');
    AddOverloadedMethod('DrawOverlay', @TSepiImportsTCustomImageList.DrawOverlay_1,
      'procedure(Canvas: TCanvas; X, Y: Integer; ImageIndex: Integer ; Overlay: TOverlay ; ADrawingStyle: TDrawingStyle ; AImageType: TImageType ; Enabled: Boolean = True )');
    AddMethod('FileLoad', @TSepiImportsTCustomImageList.FileLoad,
      'function(ResType: TResType; const Name: string; MaskColor: TColor ) : Boolean');
    AddMethod('GetBitmap', @TSepiImportsTCustomImageList.GetBitmap,
      'function(Index: Integer; Image: TBitmap): Boolean');
    AddMethod('GetHotSpot', @TSepiImportsTCustomImageList.GetHotSpot,
      'function: TPoint',
      mlkVirtual);
    AddOverloadedMethod('GetIcon', @TSepiImportsTCustomImageList.GetIcon_0,
      'procedure(Index: Integer; Image: TIcon)');
    AddOverloadedMethod('GetIcon', @TSepiImportsTCustomImageList.GetIcon_1,
      'procedure(Index: Integer; Image: TIcon; ADrawingStyle: TDrawingStyle; AImageType: TImageType )');
    AddMethod('GetImageBitmap', @TSepiImportsTCustomImageList.GetImageBitmap,
      'function: HBITMAP');
    AddMethod('GetMaskBitmap', @TSepiImportsTCustomImageList.GetMaskBitmap,
      'function: HBITMAP');
    AddMethod('GetResource', @TSepiImportsTCustomImageList.GetResource,
      'function(ResType: TResType; const Name: string; Width: Integer ; LoadFlags: TLoadResources ; MaskColor: TColor ) : Boolean');
    AddOverloadedMethod('GetInstRes', @TSepiImportsTCustomImageList.GetInstRes_0,
      'function(Instance: THandle; ResType: TResType; const Name: string; Width: Integer ; LoadFlags: TLoadResources ; MaskColor: TColor ) : Boolean');
    AddOverloadedMethod('GetInstRes', @TSepiImportsTCustomImageList.GetInstRes_1,
      'function(Instance: THandle; ResType: TResType; ResID: DWORD; Width: Integer ; LoadFlags: TLoadResources ; MaskColor: TColor ) : Boolean');
    AddMethod('HandleAllocated', @TSepiImportsTCustomImageList.HandleAllocated,
      'function: Boolean');
    AddMethod('Insert', @TSepiImportsTCustomImageList.Insert,
      'procedure(Index: Integer; Image, Mask: TBitmap)');
    AddMethod('InsertIcon', @TSepiImportsTCustomImageList.InsertIcon,
      'procedure(Index: Integer; Image: TIcon)');
    AddMethod('InsertMasked', @TSepiImportsTCustomImageList.InsertMasked,
      'procedure(Index: Integer; Image: TBitmap; MaskColor: TColor)');
    AddMethod('Move', @TSepiImportsTCustomImageList.Move,
      'procedure(CurIndex, NewIndex: Integer)');
    AddMethod('Overlay', @TSepiImportsTCustomImageList.Overlay,
      'function(ImageIndex: Integer; Overlay: TOverlay): Boolean');
    AddMethod('RegisterChanges', @TSepiImportsTCustomImageList.RegisterChanges,
      'procedure(Value: TChangeLink)');
    AddMethod('ResourceLoad', @TSepiImportsTCustomImageList.ResourceLoad,
      'function(ResType: TResType; const Name: string; MaskColor: TColor ) : Boolean');
    AddMethod('ResInstLoad', @TSepiImportsTCustomImageList.ResInstLoad,
      'function(Instance: THandle; ResType: TResType; const Name: string ; MaskColor: TColor ) : Boolean');
    AddMethod('Replace', @TSepiImportsTCustomImageList.Replace,
      'procedure(Index: Integer; Image, Mask: TBitmap)');
    AddMethod('ReplaceIcon', @TSepiImportsTCustomImageList.ReplaceIcon,
      'procedure(Index: Integer; Image: TIcon)');
    AddMethod('ReplaceMasked', @TSepiImportsTCustomImageList.ReplaceMasked,
      'procedure(Index: Integer; NewImage: TBitmap; MaskColor: TColor)');
    AddMethod('UnRegisterChanges', @TSepiImportsTCustomImageList.UnRegisterChanges,
      'procedure(Value: TChangeLink)');

    AddProperty('Count', 'property: Integer',
      'GetCount', '');
    AddProperty('Handle', 'property: HImageList',
      'GetHandle', 'SetHandle');

    CurrentVisibility := mvPublic;

    AddProperty('AllocBy', 'property: Integer',
      'FAllocBy', 'FAllocBy',
      NoIndex, 4);
    AddProperty('BlendColor', 'property: TColor',
      'FBlendColor', 'FBlendColor',
      NoIndex, clNone);
    AddProperty('BkColor', 'property: TColor',
      'GetBkColor', 'SetBkColor',
      NoIndex, clNone);
    AddProperty('DrawingStyle', 'property: TDrawingStyle',
      'FDrawingStyle', 'SetDrawingStyle',
      NoIndex, Integer(dsNormal));
    AddProperty('Height', 'property: Integer',
      'FHeight', 'SetHeight',
      NoIndex, 16);
    AddProperty('ImageType', 'property: TImageType',
      'FImageType', 'FImageType',
      NoIndex, Integer(itImage));
    AddProperty('Masked', 'property: Boolean',
      'FMasked', 'FMasked',
      NoIndex, Integer(True));
    AddProperty('ShareImages', 'property: Boolean',
      'FShareImages', 'FShareImages',
      NoIndex, Integer(False));
    AddProperty('Width', 'property: Integer',
      'FWidth', 'SetWidth',
      NoIndex, 16);
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ImgList',
    ['Windows', 'Classes', 'Graphics']);

  // CommCtrl
  TSepiTypeAlias.Create(Result, 'HImageList', TypeInfo(HImageList));

  // Types
  TSepiClass.ForwardDecl(Result, TypeInfo(TCustomImageList));
  TSepiImportsTChangeLink.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDrawingStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TImageType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TResType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TOverlay));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLoadResource));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLoadResources));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TImageIndex));
  TSepiImportsTCustomImageList.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ImgList', ImportUnit);
end.

