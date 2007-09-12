{*
  Importe l'unité Graphics dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsGraphics;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, Classes, Graphics;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEInvalidGraphic = class(EInvalidGraphic)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEInvalidGraphicOperation = class(EInvalidGraphicOperation)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTGraphicsObject = class(TGraphicsObject)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTFont = class(TFont)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTPen = class(TPen)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTBrush = class(TBrush)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTFontRecall = class(TFontRecall)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTPenRecall = class(TPenRecall)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTBrushRecall = class(TBrushRecall)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCanvas = class(TCanvas)
  private
    function GetCanvasOrientation: TCanvasOrientation;
    function GetClipRect: TRect;
    function GetHandle: HDC;
    function GetPenPos: TPoint;
    function GetPixel(X, Y: Integer): TColor;
    procedure SetBrush(Value: TBrush);
    procedure SetFont(Value: TFont);
    procedure SetHandle(Value: HDC);
    procedure SetPen(Value: TPen);
    procedure SetPenPos(Value: TPoint);
    procedure SetPixel(X, Y: Integer; Value: TColor);
    procedure Ellipse_0(X1, Y1, X2, Y2: Integer);
    procedure Ellipse_1(const Rect: TRect);
    procedure Rectangle_0(X1, Y1, X2, Y2: Integer);
    procedure Rectangle_1(const Rect: TRect);
    procedure TextRect_0(Rect: TRect; X, Y: Integer; const Text: string);
    procedure TextRect_1(var Rect: TRect; var Text: string;
      TextFormat: TTextFormat = []);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTGraphic = class(TGraphic)
  private
    procedure SetModified(Value: Boolean);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTPicture = class(TPicture)
  private
    function GetBitmap: TBitmap;
    function GetHeight: Integer;
    function GetIcon: TIcon;
    function GetMetafile: TMetafile;
    function GetWidth: Integer;
    procedure SetBitmap(Value: TBitmap);
    procedure SetGraphic(Value: TGraphic);
    procedure SetIcon(Value: TIcon);
    procedure SetMetafile(Value: TMetafile);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTMetafileCanvas = class(TMetafileCanvas)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTSharedImage = class(TSharedImage)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTMetafileImage = class(TMetafileImage)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTMetafile = class(TMetafile)
  private
    function GetAuthor: String;
    function GetDesc: String;
    function GetHandle: HENHMETAFILE;
    function GetInch: Word;
    function GetMMHeight: Integer;
    function GetMMWidth: Integer;
    procedure SetHandle(Value: HENHMETAFILE);
    procedure SetInch(Value: Word);
    procedure SetMMHeight(Value: Integer);
    procedure SetMMWidth(Value: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTBitmapImage = class(TBitmapImage)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTBitmap = class(TBitmap)
  private
    function GetCanvas: TCanvas;
    function GetHandleType: TBitmapHandleType;
    function GetMonochrome: Boolean;
    function GetPixelFormat: TPixelFormat;
    function GetScanline(Row: Integer): Pointer;
    function GetTransparentColor: TColor;
    procedure SetHandle(Value: HBITMAP);
    procedure SetMaskHandle(Value: HBITMAP);
    procedure SetMonochrome(Value: Boolean);
    procedure SetPixelFormat(Value: TPixelFormat);
    procedure SetTransparentColor(Value: TColor);
    procedure SetTransparentMode(Value: TTransparentMode);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTIconImage = class(TIconImage)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTIcon = class(TIcon)
  private
    function GetHandle: HICON;
    procedure SetHandle(Value: HICON);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{----------------------}
{ TCursorOrIcon import }
{----------------------}

function SepiImportTCursorOrIcon(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCursorOrIcon', True, True);

  with Result do
  begin
    AddField('Reserved', System.TypeInfo(Word));
    AddField('wType', System.TypeInfo(Word));
    AddField('Count', System.TypeInfo(Word));

    Complete;
  end;
end;

{-----------------}
{ TIconRec import }
{-----------------}

function SepiImportTIconRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TIconRec', True, True);

  with Result do
  begin
    AddField('Width', System.TypeInfo(Byte));
    AddField('Height', System.TypeInfo(Byte));
    AddField('Colors', System.TypeInfo(Word));
    AddField('Reserved1', System.TypeInfo(Word));
    AddField('Reserved2', System.TypeInfo(Word));
    AddField('DIBSize', System.TypeInfo(Longint));
    AddField('DIBOffset', System.TypeInfo(Longint));

    Complete;
  end;
end;

{------------------------}
{ EInvalidGraphic import }
{------------------------}

class function TSepiImportsEInvalidGraphic.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidGraphic));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------------------}
{ EInvalidGraphicOperation import }
{---------------------------------}

class function TSepiImportsEInvalidGraphicOperation.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidGraphicOperation));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------}
{ TResData import }
{-----------------}

function SepiImportTResData(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TResData', False, True);

  with Result do
  begin
    AddField('Handle', System.TypeInfo(THandle));

    Complete;
  end;
end;

{------------------}
{ TFontData import }
{------------------}

function SepiImportTFontData(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TFontData', False, True);

  with Result do
  begin
    AddField('Handle', System.TypeInfo(HFont));
    AddField('Height', System.TypeInfo(Integer));
    AddField('Orientation', System.TypeInfo(Integer));
    AddField('Pitch', System.TypeInfo(TFontPitch));
    AddField('Style', System.TypeInfo(TFontStylesBase));
    AddField('Charset', System.TypeInfo(TFontCharset));
    AddField('Name', System.TypeInfo(TFontDataName));

    Complete;
  end;
end;

{-----------------}
{ TPenData import }
{-----------------}

function SepiImportTPenData(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TPenData', False, True);

  with Result do
  begin
    AddField('Handle', System.TypeInfo(HPen));
    AddField('Color', System.TypeInfo(TColor));
    AddField('Width', System.TypeInfo(Integer));
    AddField('Style', System.TypeInfo(TPenStyle));

    Complete;
  end;
end;

{-------------------}
{ TBrushData import }
{-------------------}

function SepiImportTBrushData(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TBrushData', False, True);

  with Result do
  begin
    AddField('Handle', System.TypeInfo(HBrush));
    AddField('Color', System.TypeInfo(TColor));
    AddField('Bitmap', System.TypeInfo(TBitmap));
    AddField('Style', System.TypeInfo(TBrushStyle));

    Complete;
  end;
end;

{------------------}
{ TResource import }
{------------------}

function SepiImportTResource(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TResource', False, True);

  with Result do
  begin
    AddField('Next', 'PResource');
    AddField('RefCount', System.TypeInfo(Integer));
    AddField('Handle', System.TypeInfo(THandle));
    AddField('HashCode', System.TypeInfo(Word));
    AddFieldAfter('Data', 'TResData', 'HashCode');
    AddFieldAfter('Font', 'TFontData', 'HashCode');
    AddFieldAfter('Pen', 'TPenData', 'HashCode');
    AddFieldAfter('Brush', 'TBrushData', 'HashCode');

    Complete;
  end;
end;

{------------------------}
{ TGraphicsObject import }
{------------------------}

class function TSepiImportsTGraphicsObject.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TGraphicsObject));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FResource', 'PResource');
    AddField('FOwnerLock', 'PRTLCriticalSection');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTGraphicsObject.Changed,
      'procedure',
      mlkDynamic);
    AddMethod('Lock', @TSepiImportsTGraphicsObject.Lock,
      'procedure');
    AddMethod('Unlock', @TSepiImportsTGraphicsObject.Unlock,
      'procedure');

    CurrentVisibility := mvPublic;

    AddMethod('HandleAllocated', @TSepiImportsTGraphicsObject.HandleAllocated,
      'function: Boolean');

    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OwnerCriticalSection', 'property: PRTLCriticalSection',
      'FOwnerLock', 'FOwnerLock');

    Complete;
  end;
end;

{------------------------}
{ IChangeNotifier import }
{------------------------}

function SepiImportIChangeNotifier(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IChangeNotifier));

  with Result do
  begin
    AddMethod('Changed',
      'procedure', ccRegister);

    Complete;
  end;
end;

{--------------}
{ TFont import }
{--------------}

class function TSepiImportsTFont.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TFont));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FColor', System.TypeInfo(TColor));
    AddField('FPixelsPerInch', System.TypeInfo(Integer));
    AddField('FNotify', System.TypeInfo(IChangeNotifier));

    AddMethod('GetData', nil,
      'procedure(var FontData: TFontData)');
    AddMethod('SetData', nil,
      'procedure(const FontData: TFontData)');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTFont.Changed,
      'procedure',
      mlkOverride);
    AddMethod('GetHandle', @TSepiImportsTFont.GetHandle,
      'function: HFont');
    AddMethod('GetHeight', @TSepiImportsTFont.GetHeight,
      'function: Integer');
    AddMethod('GetName', @TSepiImportsTFont.GetName,
      'function: TFontName');
    AddMethod('GetOrientation', @TSepiImportsTFont.GetOrientation,
      'function: Integer');
    AddMethod('GetPitch', @TSepiImportsTFont.GetPitch,
      'function: TFontPitch');
    AddMethod('GetSize', @TSepiImportsTFont.GetSize,
      'function: Integer');
    AddMethod('GetStyle', @TSepiImportsTFont.GetStyle,
      'function: TFontStyles');
    AddMethod('GetCharset', @TSepiImportsTFont.GetCharset,
      'function: TFontCharset');
    AddMethod('SetColor', @TSepiImportsTFont.SetColor,
      'procedure(const Value: TColor)');
    AddMethod('SetHandle', @TSepiImportsTFont.SetHandle,
      'procedure(const Value: HFont)');
    AddMethod('SetHeight', @TSepiImportsTFont.SetHeight,
      'procedure(const Value: Integer)');
    AddMethod('SetOrientation', @TSepiImportsTFont.SetOrientation,
      'procedure(const Value: Integer)');
    AddMethod('SetName', @TSepiImportsTFont.SetName,
      'procedure(const Value: TFontName)');
    AddMethod('SetPitch', @TSepiImportsTFont.SetPitch,
      'procedure(const Value: TFontPitch)');
    AddMethod('SetSize', @TSepiImportsTFont.SetSize,
      'procedure(const Value: Integer)');
    AddMethod('SetStyle', @TSepiImportsTFont.SetStyle,
      'procedure(const Value: TFontStyles)');
    AddMethod('SetCharset', @TSepiImportsTFont.SetCharset,
      'procedure(const Value: TFontCharset)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTFont.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTFont.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTFont.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    AddProperty('FontAdapter', 'property: IChangeNotifier',
      'FNotify', 'FNotify');
    AddProperty('Handle', 'property: HFont',
      'GetHandle', 'SetHandle');
    AddProperty('PixelsPerInch', 'property: Integer',
      'FPixelsPerInch', 'FPixelsPerInch');

    CurrentVisibility := mvPublished;

    AddProperty('Charset', 'property: TFontCharset',
      'GetCharset', 'SetCharset');
    AddProperty('Color', 'property: TColor',
      'FColor', 'SetColor');
    AddProperty('Height', 'property: Integer',
      'GetHeight', 'SetHeight');
    AddProperty('Name', 'property: TFontName',
      'GetName', 'SetName');
    AddProperty('Orientation', 'property: Integer',
      'GetOrientation', 'SetOrientation',
      NoIndex, 0);
    AddProperty('Pitch', 'property: TFontPitch',
      'GetPitch', 'SetPitch',
      NoIndex, Integer(fpDefault));
    AddProperty('Size', 'property: Integer',
      'GetSize', 'SetSize');
    AddProperty('Style', 'property: TFontStyles',
      'GetStyle', 'SetStyle');

    Complete;
  end;
end;

{-------------}
{ TPen import }
{-------------}

class function TSepiImportsTPen.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPen));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMode', System.TypeInfo(TPenMode));

    AddMethod('GetData', nil,
      'procedure(var PenData: TPenData)');
    AddMethod('SetData', nil,
      'procedure(const PenData: TPenData)');

    CurrentVisibility := mvProtected;

    AddMethod('GetColor', @TSepiImportsTPen.GetColor,
      'function: TColor');
    AddMethod('SetColor', @TSepiImportsTPen.SetColor,
      'procedure(Value: TColor)');
    AddMethod('GetHandle', @TSepiImportsTPen.GetHandle,
      'function: HPen');
    AddMethod('SetHandle', @TSepiImportsTPen.SetHandle,
      'procedure(Value: HPen)');
    AddMethod('SetMode', @TSepiImportsTPen.SetMode,
      'procedure(Value: TPenMode)');
    AddMethod('GetStyle', @TSepiImportsTPen.GetStyle,
      'function: TPenStyle');
    AddMethod('SetStyle', @TSepiImportsTPen.SetStyle,
      'procedure(Value: TPenStyle)');
    AddMethod('GetWidth', @TSepiImportsTPen.GetWidth,
      'function: Integer');
    AddMethod('SetWidth', @TSepiImportsTPen.SetWidth,
      'procedure(Value: Integer)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPen.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTPen.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTPen.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    AddProperty('Handle', 'property: HPen',
      'GetHandle', 'SetHandle');

    CurrentVisibility := mvPublished;

    AddProperty('Color', 'property: TColor',
      'GetColor', 'SetColor',
      NoIndex, clBlack);
    AddProperty('Mode', 'property: TPenMode',
      'FMode', 'SetMode',
      NoIndex, Integer(pmCopy));
    AddProperty('Style', 'property: TPenStyle',
      'GetStyle', 'SetStyle',
      NoIndex, Integer(psSolid));
    AddProperty('Width', 'property: Integer',
      'GetWidth', 'SetWidth',
      NoIndex, 1);

    Complete;
  end;
end;

{---------------}
{ TBrush import }
{---------------}

class function TSepiImportsTBrush.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBrush));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetData', nil,
      'procedure(var BrushData: TBrushData)');
    AddMethod('SetData', nil,
      'procedure(const BrushData: TBrushData)');

    CurrentVisibility := mvProtected;

    AddMethod('GetBitmap', @TSepiImportsTBrush.GetBitmap,
      'function: TBitmap');
    AddMethod('SetBitmap', @TSepiImportsTBrush.SetBitmap,
      'procedure(Value: TBitmap)');
    AddMethod('GetColor', @TSepiImportsTBrush.GetColor,
      'function: TColor');
    AddMethod('SetColor', @TSepiImportsTBrush.SetColor,
      'procedure(Value: TColor)');
    AddMethod('GetHandle', @TSepiImportsTBrush.GetHandle,
      'function: HBrush');
    AddMethod('SetHandle', @TSepiImportsTBrush.SetHandle,
      'procedure(Value: HBrush)');
    AddMethod('GetStyle', @TSepiImportsTBrush.GetStyle,
      'function: TBrushStyle');
    AddMethod('SetStyle', @TSepiImportsTBrush.SetStyle,
      'procedure(Value: TBrushStyle)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBrush.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTBrush.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTBrush.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);

    AddProperty('Bitmap', 'property: TBitmap',
      'GetBitmap', 'SetBitmap');
    AddProperty('Handle', 'property: HBrush',
      'GetHandle', 'SetHandle');

    CurrentVisibility := mvPublished;

    AddProperty('Color', 'property: TColor',
      'GetColor', 'SetColor',
      NoIndex, clWhite);
    AddProperty('Style', 'property: TBrushStyle',
      'GetStyle', 'SetStyle',
      NoIndex, Integer(bsSolid));

    Complete;
  end;
end;

{--------------------}
{ TFontRecall import }
{--------------------}

class function TSepiImportsTFontRecall.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TFontRecall));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTFontRecall.Create,
      'constructor(AFont: TFont)');

    Complete;
  end;
end;

{-------------------}
{ TPenRecall import }
{-------------------}

class function TSepiImportsTPenRecall.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPenRecall));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPenRecall.Create,
      'constructor(APen: TPen)');

    Complete;
  end;
end;

{---------------------}
{ TBrushRecall import }
{---------------------}

class function TSepiImportsTBrushRecall.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBrushRecall));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBrushRecall.Create,
      'constructor(ABrush: TBrush)');

    Complete;
  end;
end;

{----------------}
{ TCanvas import }
{----------------}

function TSepiImportsTCanvas.GetCanvasOrientation: TCanvasOrientation;
begin
  Result := CanvasOrientation;
end;

function TSepiImportsTCanvas.GetClipRect: TRect;
begin
  Result := ClipRect;
end;

function TSepiImportsTCanvas.GetHandle: HDC;
begin
  Result := Handle;
end;

function TSepiImportsTCanvas.GetPenPos: TPoint;
begin
  Result := PenPos;
end;

function TSepiImportsTCanvas.GetPixel(X, Y: Integer): TColor;
begin
  Result := Pixels[X, Y];
end;

procedure TSepiImportsTCanvas.SetBrush(Value: TBrush);
begin
  Brush := Value;
end;

procedure TSepiImportsTCanvas.SetFont(Value: TFont);
begin
  Font := Value;
end;

procedure TSepiImportsTCanvas.SetHandle(Value: HDC);
begin
  Handle := Value;
end;

procedure TSepiImportsTCanvas.SetPen(Value: TPen);
begin
  Pen := Value;
end;

procedure TSepiImportsTCanvas.SetPenPos(Value: TPoint);
begin
  PenPos := Value;
end;

procedure TSepiImportsTCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  Pixels[X, Y] := Value;
end;

procedure TSepiImportsTCanvas.Ellipse_0(X1, Y1, X2, Y2: Integer);
begin
  Ellipse(X1, Y1, X2, Y2);
end;

procedure TSepiImportsTCanvas.Ellipse_1(const Rect: TRect);
begin
  Ellipse(Rect);
end;

procedure TSepiImportsTCanvas.Rectangle_0(X1, Y1, X2, Y2: Integer);
begin
  Rectangle(X1, Y1, X2, Y2);
end;

procedure TSepiImportsTCanvas.Rectangle_1(const Rect: TRect);
begin
  Rectangle(Rect);
end;

procedure TSepiImportsTCanvas.TextRect_0(Rect: TRect; X, Y: Integer;
  const Text: string);
begin
  TextRect(Rect, X, Y, Text);
end;

procedure TSepiImportsTCanvas.TextRect_1(var Rect: TRect;
  var Text: string; TextFormat: TTextFormat = []);
begin
  TextRect(Rect, Text, TextFormat);
end;

class function TSepiImportsTCanvas.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCanvas));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHandle', System.TypeInfo(HDC));
    AddField('State', System.TypeInfo(TCanvasState));
    AddField('FFont', System.TypeInfo(TFont));
    AddField('FPen', System.TypeInfo(TPen));
    AddField('FBrush', System.TypeInfo(TBrush));
    AddField('FPenPos', 'TPoint');
    AddField('FCopyMode', System.TypeInfo(TCopyMode));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnChanging', System.TypeInfo(TNotifyEvent));
    AddField('FLock', 'TRTLCriticalSection');
    AddField('FLockCount', System.TypeInfo(Integer));
    AddField('FTextFlags', System.TypeInfo(Longint));

    AddMethod('CreateBrush', nil,
      'procedure');
    AddMethod('CreateFont', nil,
      'procedure');
    AddMethod('CreatePen', nil,
      'procedure');
    AddMethod('BrushChanged', nil,
      'procedure(ABrush: TObject)');
    AddMethod('DeselectHandles', nil,
      'procedure');
    AddMethod('GetCanvasOrientation',
      @TSepiImportsTCanvas.GetCanvasOrientation,
      'function: TCanvasOrientation');
    AddMethod('GetClipRect', @TSepiImportsTCanvas.GetClipRect,
      'function: TRect');
    AddMethod('GetHandle', @TSepiImportsTCanvas.GetHandle,
      'function: HDC');
    AddMethod('GetPenPos', @TSepiImportsTCanvas.GetPenPos,
      'function: TPoint');
    AddMethod('GetPixel', @TSepiImportsTCanvas.GetPixel,
      'function(X, Y: Integer): TColor');
    AddMethod('FontChanged', nil,
      'procedure(AFont: TObject)');
    AddMethod('PenChanged', nil,
      'procedure(APen: TObject)');
    AddMethod('SetBrush', @TSepiImportsTCanvas.SetBrush,
      'procedure(Value: TBrush)');
    AddMethod('SetFont', @TSepiImportsTCanvas.SetFont,
      'procedure(Value: TFont)');
    AddMethod('SetHandle', @TSepiImportsTCanvas.SetHandle,
      'procedure(Value: HDC)');
    AddMethod('SetPen', @TSepiImportsTCanvas.SetPen,
      'procedure(Value: TPen)');
    AddMethod('SetPenPos', @TSepiImportsTCanvas.SetPenPos,
      'procedure(Value: TPoint)');
    AddMethod('SetPixel', @TSepiImportsTCanvas.SetPixel,
      'procedure(X, Y: Integer; Value: TColor)');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTCanvas.Changed,
      'procedure',
      mlkVirtual);
    AddMethod('Changing', @TSepiImportsTCanvas.Changing,
      'procedure',
      mlkVirtual);
    AddMethod('CreateHandle', @TSepiImportsTCanvas.CreateHandle,
      'procedure',
      mlkVirtual);
    AddMethod('RequiredState', @TSepiImportsTCanvas.RequiredState,
      'procedure(ReqState: TCanvasState)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCanvas.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTCanvas.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Arc', @TSepiImportsTCanvas.Arc,
      'procedure(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer)');
    AddMethod('BrushCopy', @TSepiImportsTCanvas.BrushCopy,
      'procedure(const Dest: TRect; Bitmap: TBitmap; const Source: TRect ; Color: TColor )');
    AddMethod('Chord', @TSepiImportsTCanvas.Chord,
      'procedure(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer)');
    AddMethod('CopyRect', @TSepiImportsTCanvas.CopyRect,
      'procedure(const Dest: TRect; Canvas: TCanvas; const Source: TRect )');
    AddMethod('Draw', @TSepiImportsTCanvas.Draw,
      'procedure(X, Y: Integer; Graphic: TGraphic)');
    AddMethod('DrawFocusRect', @TSepiImportsTCanvas.DrawFocusRect,
      'procedure(const Rect: TRect)');
    AddOverloadedMethod('Ellipse', @TSepiImportsTCanvas.Ellipse_0,
      'procedure(X1, Y1, X2, Y2: Integer)');
    AddOverloadedMethod('Ellipse', @TSepiImportsTCanvas.Ellipse_1,
      'procedure(const Rect: TRect)');
    AddMethod('FillRect', @TSepiImportsTCanvas.FillRect,
      'procedure(const Rect: TRect)');
    AddMethod('FloodFill', @TSepiImportsTCanvas.FloodFill,
      'procedure(X, Y: Integer; Color: TColor; FillStyle: TFillStyle)');
    AddMethod('FrameRect', @TSepiImportsTCanvas.FrameRect,
      'procedure(const Rect: TRect)');
    AddMethod('HandleAllocated', @TSepiImportsTCanvas.HandleAllocated,
      'function: Boolean');
    AddMethod('LineTo', @TSepiImportsTCanvas.LineTo,
      'procedure(X, Y: Integer)');
    AddMethod('Lock', @TSepiImportsTCanvas.Lock,
      'procedure');
    AddMethod('MoveTo', @TSepiImportsTCanvas.MoveTo,
      'procedure(X, Y: Integer)');
    AddMethod('Pie', @TSepiImportsTCanvas.Pie,
      'procedure(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer)');
    AddMethod('Polygon', @TSepiImportsTCanvas.Polygon,
      'procedure(const Points: array of TPoint)');
    AddMethod('Polyline', @TSepiImportsTCanvas.Polyline,
      'procedure(const Points: array of TPoint)');
    AddMethod('PolyBezier', @TSepiImportsTCanvas.PolyBezier,
      'procedure(const Points: array of TPoint)');
    AddMethod('PolyBezierTo', @TSepiImportsTCanvas.PolyBezierTo,
      'procedure(const Points: array of TPoint)');
    AddOverloadedMethod('Rectangle', @TSepiImportsTCanvas.Rectangle_0,
      'procedure(X1, Y1, X2, Y2: Integer)');
    AddOverloadedMethod('Rectangle', @TSepiImportsTCanvas.Rectangle_1,
      'procedure(const Rect: TRect)');
    AddMethod('Refresh', @TSepiImportsTCanvas.Refresh,
      'procedure');
    AddMethod('RoundRect', @TSepiImportsTCanvas.RoundRect,
      'procedure(X1, Y1, X2, Y2, X3, Y3: Integer)');
    AddMethod('StretchDraw', @TSepiImportsTCanvas.StretchDraw,
      'procedure(const Rect: TRect; Graphic: TGraphic)');
    AddMethod('TextExtent', @TSepiImportsTCanvas.TextExtent,
      'function(const Text: string): TSize');
    AddMethod('TextHeight', @TSepiImportsTCanvas.TextHeight,
      'function(const Text: string): Integer');
    AddMethod('TextOut', @TSepiImportsTCanvas.TextOut,
      'procedure(X, Y: Integer; const Text: string)');
    AddOverloadedMethod('TextRect', @TSepiImportsTCanvas.TextRect_0,
      'procedure(Rect: TRect; X, Y: Integer; const Text: string)');
    AddOverloadedMethod('TextRect', @TSepiImportsTCanvas.TextRect_1,
      'procedure(var Rect: TRect; var Text: string; TextFormat: TTextFormat = [])');
    AddMethod('TextWidth', @TSepiImportsTCanvas.TextWidth,
      'function(const Text: string): Integer');
    AddMethod('TryLock', @TSepiImportsTCanvas.TryLock,
      'function: Boolean');
    AddMethod('Unlock', @TSepiImportsTCanvas.Unlock,
      'procedure');

    AddProperty('ClipRect', 'property: TRect',
      'GetClipRect', '');
    AddProperty('Handle', 'property: HDC',
      'GetHandle', 'SetHandle');
    AddProperty('LockCount', 'property: Integer',
      'FLockCount', '');
    AddProperty('CanvasOrientation', 'property: TCanvasOrientation',
      'GetCanvasOrientation', '');
    AddProperty('PenPos', 'property: TPoint',
      'GetPenPos', 'SetPenPos');
    AddProperty('Pixels', 'property[X, Y: Integer]: TColor',
      'GetPixel', 'SetPixel');
    AddProperty('TextFlags', 'property: Longint',
      'FTextFlags', 'FTextFlags');
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OnChanging', 'property: TNotifyEvent',
      'FOnChanging', 'FOnChanging');

    CurrentVisibility := mvPublished;

    AddProperty('Brush', 'property: TBrush',
      'FBrush', 'SetBrush');
    AddProperty('CopyMode', 'property: TCopyMode',
      'FCopyMode', 'FCopyMode',
      NoIndex, cmSrcCopy);
    AddProperty('Font', 'property: TFont',
      'FFont', 'SetFont');
    AddProperty('Pen', 'property: TPen',
      'FPen', 'SetPen');

    Complete;
  end;
end;

{-----------------}
{ TGraphic import }
{-----------------}

procedure TSepiImportsTGraphic.SetModified(Value: Boolean);
begin
  Modified := Value;
end;

class function TSepiImportsTGraphic.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TGraphic'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TGraphic));

  with Result do
  begin
    AddInterface(System.TypeInfo(IStreamPersist));

    CurrentVisibility := mvPrivate;

    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FOnProgress', System.TypeInfo(TProgressEvent));
    AddField('FModified', System.TypeInfo(Boolean));
    AddField('FTransparent', System.TypeInfo(Boolean));
    AddField('FPaletteModified', System.TypeInfo(Boolean));

    AddMethod('SetModified', @TSepiImportsTGraphic.SetModified,
      'procedure(Value: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTGraphic.Changed,
      'procedure(Sender: TObject)',
      mlkVirtual);
    AddMethod('DefineProperties', @TSepiImportsTGraphic.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('Draw', nil,
      'procedure(ACanvas: TCanvas; const Rect: TRect)',
      mlkVirtual, True);
    AddMethod('Equals', @TSepiImportsTGraphic.Equals,
      'function(Graphic: TGraphic): Boolean',
      mlkVirtual);
    AddMethod('GetEmpty', nil,
      'function: Boolean',
      mlkVirtual, True);
    AddMethod('GetHeight', nil,
      'function: Integer',
      mlkVirtual, True);
    AddMethod('GetPalette', @TSepiImportsTGraphic.GetPalette,
      'function: HPALETTE',
      mlkVirtual);
    AddMethod('GetTransparent', @TSepiImportsTGraphic.GetTransparent,
      'function: Boolean',
      mlkVirtual);
    AddMethod('GetWidth', nil,
      'function: Integer',
      mlkVirtual, True);
    AddMethod('Progress', @TSepiImportsTGraphic.Progress,
      'procedure(Sender: TObject; Stage: TProgressStage; PercentDone: Byte ; RedrawNow: Boolean ; const R: TRect ; const Msg: string )',
      mlkDynamic);
    AddMethod('ReadData', @TSepiImportsTGraphic.ReadData,
      'procedure(Stream: TStream)',
      mlkVirtual);
    AddMethod('SetHeight', nil,
      'procedure(Value: Integer)',
      mlkVirtual, True);
    AddMethod('SetPalette', @TSepiImportsTGraphic.SetPalette,
      'procedure(Value: HPALETTE)',
      mlkVirtual);
    AddMethod('SetTransparent', @TSepiImportsTGraphic.SetTransparent,
      'procedure(Value: Boolean)',
      mlkVirtual);
    AddMethod('SetWidth', nil,
      'procedure(Value: Integer)',
      mlkVirtual, True);
    AddMethod('WriteData', @TSepiImportsTGraphic.WriteData,
      'procedure(Stream: TStream)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTGraphic.Create,
      'constructor',
      mlkVirtual);
    AddMethod('LoadFromFile', @TSepiImportsTGraphic.LoadFromFile,
      'procedure(const Filename: string)',
      mlkVirtual);
    AddMethod('SaveToFile', @TSepiImportsTGraphic.SaveToFile,
      'procedure(const Filename: string)',
      mlkVirtual);
    AddMethod('LoadFromStream', nil,
      'procedure(Stream: TStream)',
      mlkVirtual, True);
    AddMethod('SaveToStream', nil,
      'procedure(Stream: TStream)',
      mlkVirtual, True);
    AddMethod('LoadFromClipboardFormat', nil,
      'procedure(AFormat: Word; AData: THandle; APalette: HPALETTE )',
      mlkVirtual, True);
    AddMethod('SaveToClipboardFormat', nil,
      'procedure(var AFormat: Word; var AData: THandle; var APalette: HPALETTE )',
      mlkVirtual, True);

    AddProperty('Empty', 'property: Boolean',
      'GetEmpty', '');
    AddProperty('Height', 'property: Integer',
      'GetHeight', 'SetHeight');
    AddProperty('Modified', 'property: Boolean',
      'FModified', 'SetModified');
    AddProperty('Palette', 'property: HPALETTE',
      'GetPalette', 'SetPalette');
    AddProperty('PaletteModified', 'property: Boolean',
      'FPaletteModified', 'FPaletteModified');
    AddProperty('Transparent', 'property: Boolean',
      'GetTransparent', 'SetTransparent');
    AddProperty('Width', 'property: Integer',
      'GetWidth', 'SetWidth');
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OnProgress', 'property: TProgressEvent',
      'FOnProgress', 'FOnProgress');

    Complete;
  end;
end;

{-----------------}
{ TPicture import }
{-----------------}

function TSepiImportsTPicture.GetBitmap: TBitmap;
begin
  Result := Bitmap;
end;

function TSepiImportsTPicture.GetHeight: Integer;
begin
  Result := Height;
end;

function TSepiImportsTPicture.GetIcon: TIcon;
begin
  Result := Icon;
end;

function TSepiImportsTPicture.GetMetafile: TMetafile;
begin
  Result := Metafile;
end;

function TSepiImportsTPicture.GetWidth: Integer;
begin
  Result := Width;
end;

procedure TSepiImportsTPicture.SetBitmap(Value: TBitmap);
begin
  Bitmap := Value;
end;

procedure TSepiImportsTPicture.SetGraphic(Value: TGraphic);
begin
  Graphic := Value;
end;

procedure TSepiImportsTPicture.SetIcon(Value: TIcon);
begin
  Icon := Value;
end;

procedure TSepiImportsTPicture.SetMetafile(Value: TMetafile);
begin
  Metafile := Value;
end;

class function TSepiImportsTPicture.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPicture));

  with Result do
  begin
    AddInterface(System.TypeInfo(IStreamPersist));

    CurrentVisibility := mvPrivate;

    AddField('FGraphic', System.TypeInfo(TGraphic));
    AddField('FOnChange', System.TypeInfo(TNotifyEvent));
    AddField('FNotify', System.TypeInfo(IChangeNotifier));
    AddField('FOnProgress', System.TypeInfo(TProgressEvent));

    AddMethod('ForceType', nil,
      'procedure(GraphicType: TGraphicClass)');
    AddMethod('GetBitmap', @TSepiImportsTPicture.GetBitmap,
      'function: TBitmap');
    AddMethod('GetHeight', @TSepiImportsTPicture.GetHeight,
      'function: Integer');
    AddMethod('GetIcon', @TSepiImportsTPicture.GetIcon,
      'function: TIcon');
    AddMethod('GetMetafile', @TSepiImportsTPicture.GetMetafile,
      'function: TMetafile');
    AddMethod('GetWidth', @TSepiImportsTPicture.GetWidth,
      'function: Integer');
    AddMethod('ReadData', nil,
      'procedure(Stream: TStream)');
    AddMethod('SetBitmap', @TSepiImportsTPicture.SetBitmap,
      'procedure(Value: TBitmap)');
    AddMethod('SetGraphic', @TSepiImportsTPicture.SetGraphic,
      'procedure(Value: TGraphic)');
    AddMethod('SetIcon', @TSepiImportsTPicture.SetIcon,
      'procedure(Value: TIcon)');
    AddMethod('SetMetafile', @TSepiImportsTPicture.SetMetafile,
      'procedure(Value: TMetafile)');
    AddMethod('WriteData', nil,
      'procedure(Stream: TStream)');

    CurrentVisibility := mvProtected;

    AddMethod('AssignTo', @TSepiImportsTPicture.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);
    AddMethod('Changed', @TSepiImportsTPicture.Changed,
      'procedure(Sender: TObject)',
      mlkDynamic);
    AddMethod('DefineProperties', @TSepiImportsTPicture.DefineProperties,
      'procedure(Filer: TFiler)',
      mlkOverride);
    AddMethod('Progress', @TSepiImportsTPicture.Progress,
      'procedure(Sender: TObject; Stage: TProgressStage; PercentDone: Byte ; RedrawNow: Boolean ; const R: TRect ; const Msg: string )',
      mlkDynamic);
    AddMethod('LoadFromStream', @TSepiImportsTPicture.LoadFromStream,
      'procedure(Stream: TStream)');
    AddMethod('SaveToStream', @TSepiImportsTPicture.SaveToStream,
      'procedure(Stream: TStream)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPicture.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTPicture.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('LoadFromFile', @TSepiImportsTPicture.LoadFromFile,
      'procedure(const Filename: string)');
    AddMethod('SaveToFile', @TSepiImportsTPicture.SaveToFile,
      'procedure(const Filename: string)');
    AddMethod('LoadFromClipboardFormat',
      @TSepiImportsTPicture.LoadFromClipboardFormat,
      'procedure(AFormat: Word; AData: THandle; APalette: HPALETTE )');
    AddMethod('SaveToClipboardFormat',
      @TSepiImportsTPicture.SaveToClipboardFormat,
      'procedure(var AFormat: Word; var AData: THandle; var APalette: HPALETTE )');
    AddMethod('SupportsClipboardFormat',
      @TSepiImportsTPicture.SupportsClipboardFormat,
      'class function(AFormat: Word): Boolean');
    AddMethod('Assign', @TSepiImportsTPicture.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('RegisterFileFormat', @TSepiImportsTPicture.RegisterFileFormat,
      'class procedure(const AExtension, ADescription: string; AGraphicClass: TGraphicClass )');
    AddMethod('RegisterFileFormatRes',
      @TSepiImportsTPicture.RegisterFileFormatRes,
      'class procedure(const AExtension: String; ADescriptionResID: Integer ; AGraphicClass: TGraphicClass )');
    AddMethod('RegisterClipboardFormat',
      @TSepiImportsTPicture.RegisterClipboardFormat,
      'class procedure(AFormat: Word; AGraphicClass: TGraphicClass )');
    AddMethod('UnregisterGraphicClass',
      @TSepiImportsTPicture.UnregisterGraphicClass,
      'class procedure(AClass: TGraphicClass)');

    AddProperty('Bitmap', 'property: TBitmap',
      'GetBitmap', 'SetBitmap');
    AddProperty('Graphic', 'property: TGraphic',
      'FGraphic', 'SetGraphic');
    AddProperty('PictureAdapter', 'property: IChangeNotifier',
      'FNotify', 'FNotify');
    AddProperty('Height', 'property: Integer',
      'GetHeight', '');
    AddProperty('Icon', 'property: TIcon',
      'GetIcon', 'SetIcon');
    AddProperty('Metafile', 'property: TMetafile',
      'GetMetafile', 'SetMetafile');
    AddProperty('Width', 'property: Integer',
      'GetWidth', '');
    AddProperty('OnChange', 'property: TNotifyEvent',
      'FOnChange', 'FOnChange');
    AddProperty('OnProgress', 'property: TProgressEvent',
      'FOnProgress', 'FOnProgress');

    Complete;
  end;
end;

{------------------------}
{ TMetafileCanvas import }
{------------------------}

class function TSepiImportsTMetafileCanvas.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMetafileCanvas));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMetafile', System.TypeInfo(TMetafile));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMetafileCanvas.Create,
      'constructor(AMetafile: TMetafile; ReferenceDevice: HDC)');
    AddMethod('CreateWithComment',
      @TSepiImportsTMetafileCanvas.CreateWithComment,
      'constructor(AMetafile: TMetafile; ReferenceDevice: HDC; const CreatedBy, Description: String )');
    AddMethod('Destroy', @TSepiImportsTMetafileCanvas.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ TSharedImage import }
{---------------------}

class function TSepiImportsTSharedImage.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSharedImage));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FRefCount', System.TypeInfo(Integer));

    CurrentVisibility := mvProtected;

    AddMethod('Reference', @TSepiImportsTSharedImage.Reference,
      'procedure');
    AddMethod('Release', @TSepiImportsTSharedImage.Release,
      'procedure');
    AddMethod('FreeHandle', nil,
      'procedure',
      mlkVirtual, True);

    AddProperty('RefCount', 'property: Integer',
      'FRefCount', '');

    Complete;
  end;
end;

{-----------------------}
{ TMetafileImage import }
{-----------------------}

class function TSepiImportsTMetafileImage.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMetafileImage));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHandle', System.TypeInfo(HENHMETAFILE));
    AddField('FWidth', System.TypeInfo(Integer));
    AddField('FHeight', System.TypeInfo(Integer));
    AddField('FPalette', System.TypeInfo(HPALETTE));
    AddField('FInch', System.TypeInfo(Word));
    AddField('FTempWidth', System.TypeInfo(Integer));
    AddField('FTempHeight', System.TypeInfo(Integer));

    CurrentVisibility := mvProtected;

    AddMethod('FreeHandle', @TSepiImportsTMetafileImage.FreeHandle,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTMetafileImage.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{------------------}
{ TMetafile import }
{------------------}

function TSepiImportsTMetafile.GetAuthor: String;
begin
  Result := CreatedBy;
end;

function TSepiImportsTMetafile.GetDesc: String;
begin
  Result := Description;
end;

function TSepiImportsTMetafile.GetHandle: HENHMETAFILE;
begin
  Result := Handle;
end;

function TSepiImportsTMetafile.GetInch: Word;
begin
  Result := Inch;
end;

function TSepiImportsTMetafile.GetMMHeight: Integer;
begin
  Result := MMHeight;
end;

function TSepiImportsTMetafile.GetMMWidth: Integer;
begin
  Result := MMWidth;
end;

procedure TSepiImportsTMetafile.SetHandle(Value: HENHMETAFILE);
begin
  Handle := Value;
end;

procedure TSepiImportsTMetafile.SetInch(Value: Word);
begin
  Inch := Value;
end;

procedure TSepiImportsTMetafile.SetMMHeight(Value: Integer);
begin
  MMHeight := Value;
end;

procedure TSepiImportsTMetafile.SetMMWidth(Value: Integer);
begin
  MMWidth := Value;
end;

class function TSepiImportsTMetafile.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TMetafile'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TMetafile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FImage', System.TypeInfo(TMetafileImage));
    AddField('FEnhanced', System.TypeInfo(Boolean));

    AddMethod('GetAuthor', @TSepiImportsTMetafile.GetAuthor,
      'function: String');
    AddMethod('GetDesc', @TSepiImportsTMetafile.GetDesc,
      'function: String');
    AddMethod('GetHandle', @TSepiImportsTMetafile.GetHandle,
      'function: HENHMETAFILE');
    AddMethod('GetInch', @TSepiImportsTMetafile.GetInch,
      'function: Word');
    AddMethod('GetMMHeight', @TSepiImportsTMetafile.GetMMHeight,
      'function: Integer');
    AddMethod('GetMMWidth', @TSepiImportsTMetafile.GetMMWidth,
      'function: Integer');
    AddMethod('NewImage', nil,
      'procedure');
    AddMethod('SetHandle', @TSepiImportsTMetafile.SetHandle,
      'procedure(Value: HENHMETAFILE)');
    AddMethod('SetInch', @TSepiImportsTMetafile.SetInch,
      'procedure(Value: Word)');
    AddMethod('SetMMHeight', @TSepiImportsTMetafile.SetMMHeight,
      'procedure(Value: Integer)');
    AddMethod('SetMMWidth', @TSepiImportsTMetafile.SetMMWidth,
      'procedure(Value: Integer)');
    AddMethod('UniqueImage', nil,
      'procedure');

    CurrentVisibility := mvProtected;

    AddMethod('GetEmpty', @TSepiImportsTMetafile.GetEmpty,
      'function: Boolean',
      mlkOverride);
    AddMethod('GetHeight', @TSepiImportsTMetafile.GetHeight,
      'function: Integer',
      mlkOverride);
    AddMethod('GetPalette', @TSepiImportsTMetafile.GetPalette,
      'function: HPALETTE',
      mlkOverride);
    AddMethod('GetWidth', @TSepiImportsTMetafile.GetWidth,
      'function: Integer',
      mlkOverride);
    AddMethod('Draw', @TSepiImportsTMetafile.Draw,
      'procedure(ACanvas: TCanvas; const Rect: TRect)',
      mlkOverride);
    AddMethod('ReadData', @TSepiImportsTMetafile.ReadData,
      'procedure(Stream: TStream)',
      mlkOverride);
    AddMethod('ReadEMFStream', @TSepiImportsTMetafile.ReadEMFStream,
      'procedure(Stream: TStream)');
    AddMethod('ReadWMFStream', @TSepiImportsTMetafile.ReadWMFStream,
      'procedure(Stream: TStream; Length: Longint)');
    AddMethod('SetHeight', @TSepiImportsTMetafile.SetHeight,
      'procedure(Value: Integer)',
      mlkOverride);
    AddMethod('SetTransparent', @TSepiImportsTMetafile.SetTransparent,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetWidth', @TSepiImportsTMetafile.SetWidth,
      'procedure(Value: Integer)',
      mlkOverride);
    AddMethod('TestEMF', @TSepiImportsTMetafile.TestEMF,
      'function(Stream: TStream): Boolean');
    AddMethod('WriteData', @TSepiImportsTMetafile.WriteData,
      'procedure(Stream: TStream)',
      mlkOverride);
    AddMethod('WriteEMFStream', @TSepiImportsTMetafile.WriteEMFStream,
      'procedure(Stream: TStream)');
    AddMethod('WriteWMFStream', @TSepiImportsTMetafile.WriteWMFStream,
      'procedure(Stream: TStream)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMetafile.Create,
      'constructor',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTMetafile.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTMetafile.Clear,
      'procedure');
    AddMethod('HandleAllocated', @TSepiImportsTMetafile.HandleAllocated,
      'function: Boolean');
    AddMethod('LoadFromStream', @TSepiImportsTMetafile.LoadFromStream,
      'procedure(Stream: TStream)',
      mlkOverride);
    AddMethod('SaveToFile', @TSepiImportsTMetafile.SaveToFile,
      'procedure(const Filename: String)',
      mlkOverride);
    AddMethod('SaveToStream', @TSepiImportsTMetafile.SaveToStream,
      'procedure(Stream: TStream)',
      mlkOverride);
    AddMethod('LoadFromClipboardFormat',
      @TSepiImportsTMetafile.LoadFromClipboardFormat,
      'procedure(AFormat: Word; AData: THandle; APalette: HPALETTE )',
      mlkOverride);
    AddMethod('SaveToClipboardFormat',
      @TSepiImportsTMetafile.SaveToClipboardFormat,
      'procedure(var AFormat: Word; var AData: THandle; var APalette: HPALETTE )',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTMetafile.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('ReleaseHandle', @TSepiImportsTMetafile.ReleaseHandle,
      'function: HENHMETAFILE');

    AddProperty('CreatedBy', 'property: String',
      'GetAuthor', '');
    AddProperty('Description', 'property: String',
      'GetDesc', '');
    AddProperty('Enhanced', 'property: Boolean',
      'FEnhanced', 'FEnhanced',
      NoIndex, Integer(True));
    AddProperty('Handle', 'property: HENHMETAFILE',
      'GetHandle', 'SetHandle');
    AddProperty('MMWidth', 'property: Integer',
      'GetMMWidth', 'SetMMWidth');
    AddProperty('MMHeight', 'property: Integer',
      'GetMMHeight', 'SetMMHeight');
    AddProperty('Inch', 'property: Word',
      'GetInch', 'SetInch');

    Complete;
  end;
end;

{---------------------}
{ TBitmapImage import }
{---------------------}

class function TSepiImportsTBitmapImage.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBitmapImage));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHandle', System.TypeInfo(HBITMAP));
    AddField('FMaskHandle', System.TypeInfo(HBITMAP));
    AddField('FPalette', System.TypeInfo(HPALETTE));
    AddField('FDIBHandle', System.TypeInfo(HBITMAP));
    AddField('FDIB', 'TDIBSection');
    AddField('FSaveStream', System.TypeInfo(TMemoryStream));
    AddField('FOS2Format', System.TypeInfo(Boolean));
    AddField('FHalftone', System.TypeInfo(Boolean));

    CurrentVisibility := mvProtected;

    AddMethod('FreeHandle', @TSepiImportsTBitmapImage.FreeHandle,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTBitmapImage.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{----------------}
{ TBitmap import }
{----------------}

function TSepiImportsTBitmap.GetCanvas: TCanvas;
begin
  Result := Canvas;
end;

function TSepiImportsTBitmap.GetHandleType: TBitmapHandleType;
begin
  Result := HandleType;
end;

function TSepiImportsTBitmap.GetMonochrome: Boolean;
begin
  Result := Monochrome;
end;

function TSepiImportsTBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := PixelFormat;
end;

function TSepiImportsTBitmap.GetScanline(Row: Integer): Pointer;
begin
  Result := ScanLine[Row];
end;

function TSepiImportsTBitmap.GetTransparentColor: TColor;
begin
  Result := TransparentColor;
end;

procedure TSepiImportsTBitmap.SetHandle(Value: HBITMAP);
begin
  Handle := Value;
end;

procedure TSepiImportsTBitmap.SetMaskHandle(Value: HBITMAP);
begin
  MaskHandle := Value;
end;

procedure TSepiImportsTBitmap.SetMonochrome(Value: Boolean);
begin
  Monochrome := Value;
end;

procedure TSepiImportsTBitmap.SetPixelFormat(Value: TPixelFormat);
begin
  PixelFormat := Value;
end;

procedure TSepiImportsTBitmap.SetTransparentColor(Value: TColor);
begin
  TransparentColor := Value;
end;

procedure TSepiImportsTBitmap.SetTransparentMode(Value: TTransparentMode);
begin
  TransparentMode := Value;
end;

class function TSepiImportsTBitmap.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TBitmap'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TBitmap));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FImage', System.TypeInfo(TBitmapImage));
    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FIgnorePalette', System.TypeInfo(Boolean));
    AddField('FMaskBitsValid', System.TypeInfo(Boolean));
    AddField('FMaskValid', System.TypeInfo(Boolean));
    AddField('FTransparentColor', System.TypeInfo(TColor));
    AddField('FTransparentMode', System.TypeInfo(TTransparentMode));

    AddMethod('Changing', nil,
      'procedure(Sender: TObject)');
    AddMethod('CopyImage', nil,
      'procedure(AHandle: HBITMAP; APalette: HPALETTE; DIB: TDIBSection)');
    AddMethod('DIBNeeded', nil,
      'procedure');
    AddMethod('FreeContext', nil,
      'procedure');
    AddMethod('GetCanvas', @TSepiImportsTBitmap.GetCanvas,
      'function: TCanvas');
    AddMethod('GetHandle', nil,
      'function: HBITMAP',
      mlkVirtual);
    AddMethod('GetHandleType', @TSepiImportsTBitmap.GetHandleType,
      'function: TBitmapHandleType');
    AddMethod('GetMaskHandle', nil,
      'function: HBITMAP',
      mlkVirtual);
    AddMethod('GetMonochrome', @TSepiImportsTBitmap.GetMonochrome,
      'function: Boolean');
    AddMethod('GetPixelFormat', @TSepiImportsTBitmap.GetPixelFormat,
      'function: TPixelFormat');
    AddMethod('GetScanline', @TSepiImportsTBitmap.GetScanline,
      'function(Row: Integer): Pointer');
    AddMethod('GetTransparentColor', @TSepiImportsTBitmap.GetTransparentColor,
      'function: TColor');
    AddMethod('NewImage', nil,
      'procedure(NewHandle: HBITMAP; NewPalette: HPALETTE; const NewDIB: TDIBSection ; OS2Format: Boolean ; RLEStream: TStream = nil )');
    AddMethod('ReadStream', nil,
      'procedure(Stream: TStream; Size: Longint)');
    AddMethod('ReadDIB', nil,
      'procedure(Stream: TStream; ImageSize: LongWord; bmf: PBitmapFileHeader = nil)');
    AddMethod('SetHandle', @TSepiImportsTBitmap.SetHandle,
      'procedure(Value: HBITMAP)');
    AddMethod('SetHandleType', nil,
      'procedure(Value: TBitmapHandleType)',
      mlkVirtual);
    AddMethod('SetMaskHandle', @TSepiImportsTBitmap.SetMaskHandle,
      'procedure(Value: HBITMAP)');
    AddMethod('SetMonochrome', @TSepiImportsTBitmap.SetMonochrome,
      'procedure(Value: Boolean)');
    AddMethod('SetPixelFormat', @TSepiImportsTBitmap.SetPixelFormat,
      'procedure(Value: TPixelFormat)');
    AddMethod('SetTransparentColor', @TSepiImportsTBitmap.SetTransparentColor,
      'procedure(Value: TColor)');
    AddMethod('SetTransparentMode', @TSepiImportsTBitmap.SetTransparentMode,
      'procedure(Value: TTransparentMode)');
    AddMethod('TransparentColorStored', nil,
      'function: Boolean');
    AddMethod('WriteStream', nil,
      'procedure(Stream: TStream; WriteSize: Boolean)');

    CurrentVisibility := mvProtected;

    AddMethod('Changed', @TSepiImportsTBitmap.Changed,
      'procedure(Sender: TObject)',
      mlkOverride);
    AddMethod('Draw', @TSepiImportsTBitmap.Draw,
      'procedure(ACanvas: TCanvas; const Rect: TRect)',
      mlkOverride);
    AddMethod('GetEmpty', @TSepiImportsTBitmap.GetEmpty,
      'function: Boolean',
      mlkOverride);
    AddMethod('GetHeight', @TSepiImportsTBitmap.GetHeight,
      'function: Integer',
      mlkOverride);
    AddMethod('GetPalette', @TSepiImportsTBitmap.GetPalette,
      'function: HPALETTE',
      mlkOverride);
    AddMethod('GetWidth', @TSepiImportsTBitmap.GetWidth,
      'function: Integer',
      mlkOverride);
    AddMethod('HandleNeeded', @TSepiImportsTBitmap.HandleNeeded,
      'procedure');
    AddMethod('MaskHandleNeeded', @TSepiImportsTBitmap.MaskHandleNeeded,
      'procedure');
    AddMethod('PaletteNeeded', @TSepiImportsTBitmap.PaletteNeeded,
      'procedure');
    AddMethod('ReadData', @TSepiImportsTBitmap.ReadData,
      'procedure(Stream: TStream)',
      mlkOverride);
    AddMethod('SetHeight', @TSepiImportsTBitmap.SetHeight,
      'procedure(Value: Integer)',
      mlkOverride);
    AddMethod('SetPalette', @TSepiImportsTBitmap.SetPalette,
      'procedure(Value: HPALETTE)',
      mlkOverride);
    AddMethod('SetWidth', @TSepiImportsTBitmap.SetWidth,
      'procedure(Value: Integer)',
      mlkOverride);
    AddMethod('WriteData', @TSepiImportsTBitmap.WriteData,
      'procedure(Stream: TStream)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBitmap.Create,
      'constructor',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTBitmap.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTBitmap.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('Dormant', @TSepiImportsTBitmap.Dormant,
      'procedure');
    AddMethod('FreeImage', @TSepiImportsTBitmap.FreeImage,
      'procedure');
    AddMethod('HandleAllocated', @TSepiImportsTBitmap.HandleAllocated,
      'function: Boolean');
    AddMethod('LoadFromClipboardFormat',
      @TSepiImportsTBitmap.LoadFromClipboardFormat,
      'procedure(AFormat: Word; AData: THandle; APalette: HPALETTE )',
      mlkOverride);
    AddMethod('LoadFromStream', @TSepiImportsTBitmap.LoadFromStream,
      'procedure(Stream: TStream)',
      mlkOverride);
    AddMethod('LoadFromResourceName',
      @TSepiImportsTBitmap.LoadFromResourceName,
      'procedure(Instance: THandle; const ResName: String)');
    AddMethod('LoadFromResourceID', @TSepiImportsTBitmap.LoadFromResourceID,
      'procedure(Instance: THandle; ResID: Integer)');
    AddMethod('Mask', @TSepiImportsTBitmap.Mask,
      'procedure(TransparentColor: TColor)');
    AddMethod('ReleaseHandle', @TSepiImportsTBitmap.ReleaseHandle,
      'function: HBITMAP');
    AddMethod('ReleaseMaskHandle', @TSepiImportsTBitmap.ReleaseMaskHandle,
      'function: HBITMAP');
    AddMethod('ReleasePalette', @TSepiImportsTBitmap.ReleasePalette,
      'function: HPALETTE');
    AddMethod('SaveToClipboardFormat',
      @TSepiImportsTBitmap.SaveToClipboardFormat,
      'procedure(var Format: Word; var Data: THandle; var APalette: HPALETTE )',
      mlkOverride);
    AddMethod('SaveToStream', @TSepiImportsTBitmap.SaveToStream,
      'procedure(Stream: TStream)',
      mlkOverride);

    AddProperty('Canvas', 'property: TCanvas',
      'GetCanvas', '');
    AddProperty('Handle', 'property: HBITMAP',
      'GetHandle', 'SetHandle');
    AddProperty('HandleType', 'property: TBitmapHandleType',
      'GetHandleType', 'SetHandleType');
    AddProperty('IgnorePalette', 'property: Boolean',
      'FIgnorePalette', 'FIgnorePalette');
    AddProperty('MaskHandle', 'property: HBITMAP',
      'GetMaskHandle', 'SetMaskHandle');
    AddProperty('Monochrome', 'property: Boolean',
      'GetMonochrome', 'SetMonochrome');
    AddProperty('PixelFormat', 'property: TPixelFormat',
      'GetPixelFormat', 'SetPixelFormat');
    AddProperty('ScanLine', 'property[Row: Integer]: Pointer',
      'GetScanLine', '');
    AddProperty('TransparentColor', 'property: TColor',
      'GetTransparentColor', 'SetTransparentColor');
    AddProperty('TransparentMode', 'property: TTransparentMode',
      'FTransparentMode', 'SetTransparentMode',
      NoIndex, Integer(tmAuto));

    Complete;
  end;
end;

{-------------------}
{ TIconImage import }
{-------------------}

class function TSepiImportsTIconImage.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TIconImage));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHandle', System.TypeInfo(HICON));
    AddField('FMemoryImage', System.TypeInfo(TCustomMemoryStream));
    AddField('FSize', 'TPoint');

    CurrentVisibility := mvProtected;

    AddMethod('FreeHandle', @TSepiImportsTIconImage.FreeHandle,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTIconImage.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{--------------}
{ TIcon import }
{--------------}

function TSepiImportsTIcon.GetHandle: HICON;
begin
  Result := Handle;
end;

procedure TSepiImportsTIcon.SetHandle(Value: HICON);
begin
  Handle := Value;
end;

class function TSepiImportsTIcon.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TIcon'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TIcon));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FImage', System.TypeInfo(TIconImage));
    AddField('FRequestedSize', 'TPoint');

    AddMethod('GetHandle', @TSepiImportsTIcon.GetHandle,
      'function: HICON');
    AddMethod('HandleNeeded', nil,
      'procedure');
    AddMethod('ImageNeeded', nil,
      'procedure');
    AddMethod('NewImage', nil,
      'procedure(NewHandle: HICON; NewImage: TMemoryStream)');
    AddMethod('SetHandle', @TSepiImportsTIcon.SetHandle,
      'procedure(Value: HICON)');

    CurrentVisibility := mvProtected;

    AddMethod('Draw', @TSepiImportsTIcon.Draw,
      'procedure(ACanvas: TCanvas; const Rect: TRect)',
      mlkOverride);
    AddMethod('GetEmpty', @TSepiImportsTIcon.GetEmpty,
      'function: Boolean',
      mlkOverride);
    AddMethod('GetHeight', @TSepiImportsTIcon.GetHeight,
      'function: Integer',
      mlkOverride);
    AddMethod('GetWidth', @TSepiImportsTIcon.GetWidth,
      'function: Integer',
      mlkOverride);
    AddMethod('SetHeight', @TSepiImportsTIcon.SetHeight,
      'procedure(Value: Integer)',
      mlkOverride);
    AddMethod('SetTransparent', @TSepiImportsTIcon.SetTransparent,
      'procedure(Value: Boolean)',
      mlkOverride);
    AddMethod('SetWidth', @TSepiImportsTIcon.SetWidth,
      'procedure(Value: Integer)',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTIcon.Create,
      'constructor',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTIcon.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTIcon.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('HandleAllocated', @TSepiImportsTIcon.HandleAllocated,
      'function: Boolean');
    AddMethod('LoadFromClipboardFormat',
      @TSepiImportsTIcon.LoadFromClipboardFormat,
      'procedure(AFormat: Word; AData: THandle; APalette: HPALETTE )',
      mlkOverride);
    AddMethod('LoadFromResourceName', @TSepiImportsTIcon.LoadFromResourceName,
      'procedure(Instance: THandle; const ResName: String)');
    AddMethod('LoadFromResourceID', @TSepiImportsTIcon.LoadFromResourceID,
      'procedure(Instance: THandle; ResID: Integer)');
    AddMethod('LoadFromStream', @TSepiImportsTIcon.LoadFromStream,
      'procedure(Stream: TStream)',
      mlkOverride);
    AddMethod('ReleaseHandle', @TSepiImportsTIcon.ReleaseHandle,
      'function: HICON');
    AddMethod('SaveToClipboardFormat',
      @TSepiImportsTIcon.SaveToClipboardFormat,
      'procedure(var Format: Word; var Data: THandle; var APalette: HPALETTE )',
      mlkOverride);
    AddMethod('SaveToStream', @TSepiImportsTIcon.SaveToStream,
      'procedure(Stream: TStream)',
      mlkOverride);

    AddProperty('Handle', 'property: HICON',
      'GetHandle', 'SetHandle');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'Graphics',
    ['Windows', 'SysUtils', 'Classes']);

  // Types
  TSepiPointerType.Create(Result, 'PColor', TypeInfo(TColor), True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TColor));

  // Constants
  TSepiConstant.Create(Result, 'clSystemColor', clSystemColor);
  TSepiConstant.Create(Result, 'clScrollBar', clScrollBar);
  TSepiConstant.Create(Result, 'clBackground', clBackground);
  TSepiConstant.Create(Result, 'clActiveCaption', clActiveCaption);
  TSepiConstant.Create(Result, 'clInactiveCaption', clInactiveCaption);
  TSepiConstant.Create(Result, 'clMenu', clMenu);
  TSepiConstant.Create(Result, 'clWindow', clWindow);
  TSepiConstant.Create(Result, 'clWindowFrame', clWindowFrame);
  TSepiConstant.Create(Result, 'clMenuText', clMenuText);
  TSepiConstant.Create(Result, 'clWindowText', clWindowText);
  TSepiConstant.Create(Result, 'clCaptionText', clCaptionText);
  TSepiConstant.Create(Result, 'clActiveBorder', clActiveBorder);
  TSepiConstant.Create(Result, 'clInactiveBorder', clInactiveBorder);
  TSepiConstant.Create(Result, 'clAppWorkSpace', clAppWorkSpace);
  TSepiConstant.Create(Result, 'clHighlight', clHighlight);
  TSepiConstant.Create(Result, 'clHighlightText', clHighlightText);
  TSepiConstant.Create(Result, 'clBtnFace', clBtnFace);
  TSepiConstant.Create(Result, 'clBtnShadow', clBtnShadow);
  TSepiConstant.Create(Result, 'clGrayText', clGrayText);
  TSepiConstant.Create(Result, 'clBtnText', clBtnText);
  TSepiConstant.Create(Result, 'clInactiveCaptionText', clInactiveCaptionText);
  TSepiConstant.Create(Result, 'clBtnHighlight', clBtnHighlight);
  TSepiConstant.Create(Result, 'cl3DDkShadow', cl3DDkShadow);
  TSepiConstant.Create(Result, 'cl3DLight', cl3DLight);
  TSepiConstant.Create(Result, 'clInfoText', clInfoText);
  TSepiConstant.Create(Result, 'clInfoBk', clInfoBk);
  TSepiConstant.Create(Result, 'clHotLight', clHotLight);
  TSepiConstant.Create(Result, 'clGradientActiveCaption',
    clGradientActiveCaption);
  TSepiConstant.Create(Result, 'clGradientInactiveCaption',
    clGradientInactiveCaption);
  TSepiConstant.Create(Result, 'clMenuHighlight', clMenuHighlight);
  TSepiConstant.Create(Result, 'clMenuBar', clMenuBar);
  TSepiConstant.Create(Result, 'clBlack', clBlack);
  TSepiConstant.Create(Result, 'clMaroon', clMaroon);
  TSepiConstant.Create(Result, 'clGreen', clGreen);
  TSepiConstant.Create(Result, 'clOlive', clOlive);
  TSepiConstant.Create(Result, 'clNavy', clNavy);
  TSepiConstant.Create(Result, 'clPurple', clPurple);
  TSepiConstant.Create(Result, 'clTeal', clTeal);
  TSepiConstant.Create(Result, 'clGray', clGray);
  TSepiConstant.Create(Result, 'clSilver', clSilver);
  TSepiConstant.Create(Result, 'clRed', clRed);
  TSepiConstant.Create(Result, 'clLime', clLime);
  TSepiConstant.Create(Result, 'clYellow', clYellow);
  TSepiConstant.Create(Result, 'clBlue', clBlue);
  TSepiConstant.Create(Result, 'clFuchsia', clFuchsia);
  TSepiConstant.Create(Result, 'clAqua', clAqua);
  TSepiConstant.Create(Result, 'clLtGray', clLtGray);
  TSepiConstant.Create(Result, 'clDkGray', clDkGray);
  TSepiConstant.Create(Result, 'clWhite', clWhite);
  TSepiConstant.Create(Result, 'StandardColorsCount', StandardColorsCount);
  TSepiConstant.Create(Result, 'clMoneyGreen', clMoneyGreen);
  TSepiConstant.Create(Result, 'clSkyBlue', clSkyBlue);
  TSepiConstant.Create(Result, 'clCream', clCream);
  TSepiConstant.Create(Result, 'clMedGray', clMedGray);
  TSepiConstant.Create(Result, 'ExtendedColorsCount', ExtendedColorsCount);
  TSepiConstant.Create(Result, 'clNone', clNone);
  TSepiConstant.Create(Result, 'clDefault', clDefault);
  TSepiConstant.Create(Result, 'clWebSnow', clWebSnow);
  TSepiConstant.Create(Result, 'clWebFloralWhite', clWebFloralWhite);
  TSepiConstant.Create(Result, 'clWebLavenderBlush', clWebLavenderBlush);
  TSepiConstant.Create(Result, 'clWebOldLace', clWebOldLace);
  TSepiConstant.Create(Result, 'clWebIvory', clWebIvory);
  TSepiConstant.Create(Result, 'clWebCornSilk', clWebCornSilk);
  TSepiConstant.Create(Result, 'clWebBeige', clWebBeige);
  TSepiConstant.Create(Result, 'clWebAntiqueWhite', clWebAntiqueWhite);
  TSepiConstant.Create(Result, 'clWebWheat', clWebWheat);
  TSepiConstant.Create(Result, 'clWebAliceBlue', clWebAliceBlue);
  TSepiConstant.Create(Result, 'clWebGhostWhite', clWebGhostWhite);
  TSepiConstant.Create(Result, 'clWebLavender', clWebLavender);
  TSepiConstant.Create(Result, 'clWebSeashell', clWebSeashell);
  TSepiConstant.Create(Result, 'clWebLightYellow', clWebLightYellow);
  TSepiConstant.Create(Result, 'clWebPapayaWhip', clWebPapayaWhip);
  TSepiConstant.Create(Result, 'clWebNavajoWhite', clWebNavajoWhite);
  TSepiConstant.Create(Result, 'clWebMoccasin', clWebMoccasin);
  TSepiConstant.Create(Result, 'clWebBurlywood', clWebBurlywood);
  TSepiConstant.Create(Result, 'clWebAzure', clWebAzure);
  TSepiConstant.Create(Result, 'clWebMintcream', clWebMintcream);
  TSepiConstant.Create(Result, 'clWebHoneydew', clWebHoneydew);
  TSepiConstant.Create(Result, 'clWebLinen', clWebLinen);
  TSepiConstant.Create(Result, 'clWebLemonChiffon', clWebLemonChiffon);
  TSepiConstant.Create(Result, 'clWebBlanchedAlmond', clWebBlanchedAlmond);
  TSepiConstant.Create(Result, 'clWebBisque', clWebBisque);
  TSepiConstant.Create(Result, 'clWebPeachPuff', clWebPeachPuff);
  TSepiConstant.Create(Result, 'clWebTan', clWebTan);
  TSepiConstant.Create(Result, 'clWebYellow', clWebYellow);
  TSepiConstant.Create(Result, 'clWebDarkOrange', clWebDarkOrange);
  TSepiConstant.Create(Result, 'clWebRed', clWebRed);
  TSepiConstant.Create(Result, 'clWebDarkRed', clWebDarkRed);
  TSepiConstant.Create(Result, 'clWebMaroon', clWebMaroon);
  TSepiConstant.Create(Result, 'clWebIndianRed', clWebIndianRed);
  TSepiConstant.Create(Result, 'clWebSalmon', clWebSalmon);
  TSepiConstant.Create(Result, 'clWebCoral', clWebCoral);
  TSepiConstant.Create(Result, 'clWebGold', clWebGold);
  TSepiConstant.Create(Result, 'clWebTomato', clWebTomato);
  TSepiConstant.Create(Result, 'clWebCrimson', clWebCrimson);
  TSepiConstant.Create(Result, 'clWebBrown', clWebBrown);
  TSepiConstant.Create(Result, 'clWebChocolate', clWebChocolate);
  TSepiConstant.Create(Result, 'clWebSandyBrown', clWebSandyBrown);
  TSepiConstant.Create(Result, 'clWebLightSalmon', clWebLightSalmon);
  TSepiConstant.Create(Result, 'clWebLightCoral', clWebLightCoral);
  TSepiConstant.Create(Result, 'clWebOrange', clWebOrange);
  TSepiConstant.Create(Result, 'clWebOrangeRed', clWebOrangeRed);
  TSepiConstant.Create(Result, 'clWebFirebrick', clWebFirebrick);
  TSepiConstant.Create(Result, 'clWebSaddleBrown', clWebSaddleBrown);
  TSepiConstant.Create(Result, 'clWebSienna', clWebSienna);
  TSepiConstant.Create(Result, 'clWebPeru', clWebPeru);
  TSepiConstant.Create(Result, 'clWebDarkSalmon', clWebDarkSalmon);
  TSepiConstant.Create(Result, 'clWebRosyBrown', clWebRosyBrown);
  TSepiConstant.Create(Result, 'clWebPaleGoldenrod', clWebPaleGoldenrod);
  TSepiConstant.Create(Result, 'clWebLightGoldenrodYellow',
    clWebLightGoldenrodYellow);
  TSepiConstant.Create(Result, 'clWebOlive', clWebOlive);
  TSepiConstant.Create(Result, 'clWebForestGreen', clWebForestGreen);
  TSepiConstant.Create(Result, 'clWebGreenYellow', clWebGreenYellow);
  TSepiConstant.Create(Result, 'clWebChartreuse', clWebChartreuse);
  TSepiConstant.Create(Result, 'clWebLightGreen', clWebLightGreen);
  TSepiConstant.Create(Result, 'clWebAquamarine', clWebAquamarine);
  TSepiConstant.Create(Result, 'clWebSeaGreen', clWebSeaGreen);
  TSepiConstant.Create(Result, 'clWebGoldenRod', clWebGoldenRod);
  TSepiConstant.Create(Result, 'clWebKhaki', clWebKhaki);
  TSepiConstant.Create(Result, 'clWebOliveDrab', clWebOliveDrab);
  TSepiConstant.Create(Result, 'clWebGreen', clWebGreen);
  TSepiConstant.Create(Result, 'clWebYellowGreen', clWebYellowGreen);
  TSepiConstant.Create(Result, 'clWebLawnGreen', clWebLawnGreen);
  TSepiConstant.Create(Result, 'clWebPaleGreen', clWebPaleGreen);
  TSepiConstant.Create(Result, 'clWebMediumAquamarine', clWebMediumAquamarine);
  TSepiConstant.Create(Result, 'clWebMediumSeaGreen', clWebMediumSeaGreen);
  TSepiConstant.Create(Result, 'clWebDarkGoldenRod', clWebDarkGoldenRod);
  TSepiConstant.Create(Result, 'clWebDarkKhaki', clWebDarkKhaki);
  TSepiConstant.Create(Result, 'clWebDarkOliveGreen', clWebDarkOliveGreen);
  TSepiConstant.Create(Result, 'clWebDarkgreen', clWebDarkgreen);
  TSepiConstant.Create(Result, 'clWebLimeGreen', clWebLimeGreen);
  TSepiConstant.Create(Result, 'clWebLime', clWebLime);
  TSepiConstant.Create(Result, 'clWebSpringGreen', clWebSpringGreen);
  TSepiConstant.Create(Result, 'clWebMediumSpringGreen',
    clWebMediumSpringGreen);
  TSepiConstant.Create(Result, 'clWebDarkSeaGreen', clWebDarkSeaGreen);
  TSepiConstant.Create(Result, 'clWebLightSeaGreen', clWebLightSeaGreen);
  TSepiConstant.Create(Result, 'clWebPaleTurquoise', clWebPaleTurquoise);
  TSepiConstant.Create(Result, 'clWebLightCyan', clWebLightCyan);
  TSepiConstant.Create(Result, 'clWebLightBlue', clWebLightBlue);
  TSepiConstant.Create(Result, 'clWebLightSkyBlue', clWebLightSkyBlue);
  TSepiConstant.Create(Result, 'clWebCornFlowerBlue', clWebCornFlowerBlue);
  TSepiConstant.Create(Result, 'clWebDarkBlue', clWebDarkBlue);
  TSepiConstant.Create(Result, 'clWebIndigo', clWebIndigo);
  TSepiConstant.Create(Result, 'clWebMediumTurquoise', clWebMediumTurquoise);
  TSepiConstant.Create(Result, 'clWebTurquoise', clWebTurquoise);
  TSepiConstant.Create(Result, 'clWebCyan', clWebCyan);
  TSepiConstant.Create(Result, 'clWebAqua', clWebAqua);
  TSepiConstant.Create(Result, 'clWebPowderBlue', clWebPowderBlue);
  TSepiConstant.Create(Result, 'clWebSkyBlue', clWebSkyBlue);
  TSepiConstant.Create(Result, 'clWebRoyalBlue', clWebRoyalBlue);
  TSepiConstant.Create(Result, 'clWebMediumBlue', clWebMediumBlue);
  TSepiConstant.Create(Result, 'clWebMidnightBlue', clWebMidnightBlue);
  TSepiConstant.Create(Result, 'clWebDarkTurquoise', clWebDarkTurquoise);
  TSepiConstant.Create(Result, 'clWebCadetBlue', clWebCadetBlue);
  TSepiConstant.Create(Result, 'clWebDarkCyan', clWebDarkCyan);
  TSepiConstant.Create(Result, 'clWebTeal', clWebTeal);
  TSepiConstant.Create(Result, 'clWebDeepskyBlue', clWebDeepskyBlue);
  TSepiConstant.Create(Result, 'clWebDodgerBlue', clWebDodgerBlue);
  TSepiConstant.Create(Result, 'clWebBlue', clWebBlue);
  TSepiConstant.Create(Result, 'clWebNavy', clWebNavy);
  TSepiConstant.Create(Result, 'clWebDarkViolet', clWebDarkViolet);
  TSepiConstant.Create(Result, 'clWebDarkOrchid', clWebDarkOrchid);
  TSepiConstant.Create(Result, 'clWebMagenta', clWebMagenta);
  TSepiConstant.Create(Result, 'clWebFuchsia', clWebFuchsia);
  TSepiConstant.Create(Result, 'clWebDarkMagenta', clWebDarkMagenta);
  TSepiConstant.Create(Result, 'clWebMediumVioletRed', clWebMediumVioletRed);
  TSepiConstant.Create(Result, 'clWebPaleVioletRed', clWebPaleVioletRed);
  TSepiConstant.Create(Result, 'clWebBlueViolet', clWebBlueViolet);
  TSepiConstant.Create(Result, 'clWebMediumOrchid', clWebMediumOrchid);
  TSepiConstant.Create(Result, 'clWebMediumPurple', clWebMediumPurple);
  TSepiConstant.Create(Result, 'clWebPurple', clWebPurple);
  TSepiConstant.Create(Result, 'clWebDeepPink', clWebDeepPink);
  TSepiConstant.Create(Result, 'clWebLightPink', clWebLightPink);
  TSepiConstant.Create(Result, 'clWebViolet', clWebViolet);
  TSepiConstant.Create(Result, 'clWebOrchid', clWebOrchid);
  TSepiConstant.Create(Result, 'clWebPlum', clWebPlum);
  TSepiConstant.Create(Result, 'clWebThistle', clWebThistle);
  TSepiConstant.Create(Result, 'clWebHotPink', clWebHotPink);
  TSepiConstant.Create(Result, 'clWebPink', clWebPink);
  TSepiConstant.Create(Result, 'clWebLightSteelBlue', clWebLightSteelBlue);
  TSepiConstant.Create(Result, 'clWebMediumSlateBlue', clWebMediumSlateBlue);
  TSepiConstant.Create(Result, 'clWebLightSlateGray', clWebLightSlateGray);
  TSepiConstant.Create(Result, 'clWebWhite', clWebWhite);
  TSepiConstant.Create(Result, 'clWebLightgrey', clWebLightgrey);
  TSepiConstant.Create(Result, 'clWebGray', clWebGray);
  TSepiConstant.Create(Result, 'clWebSteelBlue', clWebSteelBlue);
  TSepiConstant.Create(Result, 'clWebSlateBlue', clWebSlateBlue);
  TSepiConstant.Create(Result, 'clWebSlateGray', clWebSlateGray);
  TSepiConstant.Create(Result, 'clWebWhiteSmoke', clWebWhiteSmoke);
  TSepiConstant.Create(Result, 'clWebSilver', clWebSilver);
  TSepiConstant.Create(Result, 'clWebDimGray', clWebDimGray);
  TSepiConstant.Create(Result, 'clWebMistyRose', clWebMistyRose);
  TSepiConstant.Create(Result, 'clWebDarkSlateBlue', clWebDarkSlateBlue);
  TSepiConstant.Create(Result, 'clWebDarkSlategray', clWebDarkSlategray);
  TSepiConstant.Create(Result, 'clWebGainsboro', clWebGainsboro);
  TSepiConstant.Create(Result, 'clWebDarkGray', clWebDarkGray);
  TSepiConstant.Create(Result, 'clWebBlack', clWebBlack);
  TSepiConstant.Create(Result, 'WebColorsCount', WebColorsCount);

  // Constants
  TSepiConstant.Create(Result, 'cmBlackness', cmBlackness);
  TSepiConstant.Create(Result, 'cmDstInvert', cmDstInvert);
  TSepiConstant.Create(Result, 'cmMergeCopy', cmMergeCopy);
  TSepiConstant.Create(Result, 'cmMergePaint', cmMergePaint);
  TSepiConstant.Create(Result, 'cmNotSrcCopy', cmNotSrcCopy);
  TSepiConstant.Create(Result, 'cmNotSrcErase', cmNotSrcErase);
  TSepiConstant.Create(Result, 'cmPatCopy', cmPatCopy);
  TSepiConstant.Create(Result, 'cmPatInvert', cmPatInvert);
  TSepiConstant.Create(Result, 'cmPatPaint', cmPatPaint);
  TSepiConstant.Create(Result, 'cmSrcAnd', cmSrcAnd);
  TSepiConstant.Create(Result, 'cmSrcCopy', cmSrcCopy);
  TSepiConstant.Create(Result, 'cmSrcErase', cmSrcErase);
  TSepiConstant.Create(Result, 'cmSrcInvert', cmSrcInvert);
  TSepiConstant.Create(Result, 'cmSrcPaint', cmSrcPaint);
  TSepiConstant.Create(Result, 'cmWhiteness', cmWhiteness);
  TSepiConstant.Create(Result, 'rc3_StockIcon', rc3_StockIcon);
  TSepiConstant.Create(Result, 'rc3_Icon', rc3_Icon);
  TSepiConstant.Create(Result, 'rc3_Cursor', rc3_Cursor);

  // Types
  TSepiPointerType.Create(Result, 'PCursorOrIcon', 'TCursorOrIcon', True);
  SepiImportTCursorOrIcon(Result);
  TSepiPointerType.Create(Result, 'PIconRec', 'TIconRec', True);
  SepiImportTIconRec(Result);
  TSepiTypeAlias.Create(Result, 'HMETAFILE', TypeInfo(THandle));
  TSepiTypeAlias.Create(Result, 'HENHMETAFILE', TypeInfo(THandle));
  TSepiImportsEInvalidGraphic.SepiImport(Result);
  TSepiImportsEInvalidGraphicOperation.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TGraphic));
  TSepiClass.ForwardDecl(Result, TypeInfo(TBitmap));
  TSepiClass.ForwardDecl(Result, TypeInfo(TIcon));
  TSepiClass.ForwardDecl(Result, TypeInfo(TMetafile));
  SepiImportTResData(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontPitch));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontName));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontCharset));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontDataName));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontStyles));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFontStylesBase));
  SepiImportTFontData(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPenStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPenMode));
  SepiImportTPenData(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBrushStyle));
  SepiImportTBrushData(Result);
  TSepiPointerType.Create(Result, 'PResource', 'TResource', True);
  SepiImportTResource(Result);
  TSepiImportsTGraphicsObject.SepiImport(Result);
  SepiImportIChangeNotifier(Result);
  TSepiImportsTFont.SepiImport(Result);
  TSepiImportsTPen.SepiImport(Result);
  TSepiImportsTBrush.SepiImport(Result);
  TSepiImportsTFontRecall.SepiImport(Result);
  TSepiImportsTPenRecall.SepiImport(Result);
  TSepiImportsTBrushRecall.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFillStyle));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFillMode));
  TSepiTypeAlias.Create(Result, 'TCopyMode', TypeInfo(Longint));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCanvasStates));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCanvasState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCanvasOrientation));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTextFormats));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTextFormat));
  TSepiImportsTCanvas.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TProgressStage));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TProgressEvent));
  TSepiImportsTGraphic.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TGraphicClass', TypeInfo(TGraphic), True);
  TSepiImportsTPicture.SepiImport(Result);
  TSepiImportsTMetafileCanvas.SepiImport(Result);
  TSepiImportsTSharedImage.SepiImport(Result);
  TSepiImportsTMetafileImage.SepiImport(Result);
  TSepiImportsTMetafile.SepiImport(Result);
  TSepiImportsTBitmapImage.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBitmapHandleType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPixelFormat));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTransparentMode));
  TSepiImportsTBitmap.SepiImport(Result);
  TSepiImportsTIconImage.SepiImport(Result);
  TSepiImportsTIcon.SepiImport(Result);

  // Global variables
  TSepiVariable.Create(Result, 'DefFontData',
    DefFontData, 'TFontData');

  // Global variables
  TSepiVariable.Create(Result, 'SystemPalette16',
    SystemPalette16, TypeInfo(HPalette));

  // Global variables
  TSepiVariable.Create(Result, 'DDBsOnly',
    DDBsOnly, TypeInfo(Boolean));

  // Routines
  TSepiMethod.Create(Result, 'GraphicFilter', @GraphicFilter,
    'function(GraphicClass: TGraphicClass): string');
  TSepiMethod.Create(Result, 'GraphicExtension', @GraphicExtension,
    'function(GraphicClass: TGraphicClass): string');
  TSepiMethod.Create(Result, 'GraphicFileMask', @GraphicFileMask,
    'function(GraphicClass: TGraphicClass): string');
  TSepiMethod.Create(Result, 'ColorToRGB', @ColorToRGB,
    'function(Color: TColor): Longint');
  TSepiMethod.Create(Result, 'ColorToString', @ColorToString,
    'function(Color: TColor): string');
  TSepiMethod.Create(Result, 'StringToColor', @StringToColor,
    'function(const S: string): TColor');
  TSepiMethod.Create(Result, 'GetColorValues', @GetColorValues,
    'procedure(Proc: TGetStrProc)');
  TSepiMethod.Create(Result, 'ColorToIdent', @ColorToIdent,
    'function(Color: Longint; var Ident: string): Boolean');
  TSepiMethod.Create(Result, 'IdentToColor', @IdentToColor,
    'function(const Ident: string; var Color: Longint): Boolean');
  TSepiMethod.Create(Result, 'GetCharsetValues', @GetCharsetValues,
    'procedure(Proc: TGetStrProc)');
  TSepiMethod.Create(Result, 'CharsetToIdent', @CharsetToIdent,
    'function(Charset: Longint; var Ident: string): Boolean');
  TSepiMethod.Create(Result, 'IdentToCharset', @IdentToCharset,
    'function(const Ident: string; var Charset: Longint): Boolean');
  TSepiMethod.Create(Result, 'GetDIBSizes', @GetDIBSizes,
    'procedure(Bitmap: HBITMAP; var InfoHeaderSize: DWORD; var ImageSize: DWORD )');
  TSepiMethod.Create(Result, 'GetDIB', @GetDIB,
    'function(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits): Boolean');
  TSepiMethod.Create(Result, 'CopyPalette', @CopyPalette,
    'function(Palette: HPALETTE): HPALETTE');
  TSepiMethod.Create(Result, 'PaletteChanged', @PaletteChanged,
    'procedure');
  TSepiMethod.Create(Result, 'FreeMemoryContexts', @FreeMemoryContexts,
    'procedure');
  TSepiMethod.Create(Result, 'GetDefFontCharSet', @GetDefFontCharSet,
    'function: TFontCharSet');
  TSepiMethod.Create(Result, 'TransparentStretchBlt', @TransparentStretchBlt,
    'function(DstDC: HDC; DstX, DstY, DstW, DstH: Integer; SrcDC: HDC ; SrcX, SrcY, SrcW, SrcH: Integer ; MaskDC: HDC ; MaskX, MaskY : Integer ) : Boolean');
  TSepiMethod.Create(Result, 'CreateMappedBmp', @CreateMappedBmp,
    'function(Handle: HBITMAP; const OldColors, NewColors: array of TColor): HBITMAP');
  TSepiMethod.Create(Result, 'CreateMappedRes', @CreateMappedRes,
    'function(Instance: THandle; ResName: PChar; const OldColors, NewColors: array of TColor): HBITMAP');
  TSepiMethod.Create(Result, 'CreateGrayMappedBmp', @CreateGrayMappedBmp,
    'function(Handle: HBITMAP): HBITMAP');
  TSepiMethod.Create(Result, 'CreateGrayMappedRes', @CreateGrayMappedRes,
    'function(Instance: THandle; ResName: PChar): HBITMAP');
  TSepiMethod.Create(Result, 'AllocPatternBitmap', @AllocPatternBitmap,
    'function(BkColor, FgColor: TColor): TBitmap');
  TSepiMethod.Create(Result, 'BytesPerScanline', @BytesPerScanline,
    'function(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Graphics', ImportUnit);
end.

