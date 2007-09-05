{*
  Importe l'unité ToolWin dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsToolWin;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Messages, Controls, ToolWin;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTToolWindow = class(TToolWindow)
  private
    procedure SetEdgeBorders(Value: TEdgeBorders);
    procedure SetEdgeInner(Value: TEdgeStyle);
    procedure SetEdgeOuter(Value: TEdgeStyle);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTToolDockObject = class(TToolDockObject)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTToolDockForm = class(TToolDockForm)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{--------------------}
{ TToolWindow import }
{--------------------}

procedure TSepiImportsTToolWindow.SetEdgeBorders(Value: TEdgeBorders);
begin
  EdgeBorders := Value;
end;

procedure TSepiImportsTToolWindow.SetEdgeInner(Value: TEdgeStyle);
begin
  EdgeInner := Value;
end;

procedure TSepiImportsTToolWindow.SetEdgeOuter(Value: TEdgeStyle);
begin
  EdgeOuter := Value;
end;

class function TSepiImportsTToolWindow.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
const
  DefaultEdgeBorders : TEdgeBorders = [ebLeft, ebTop, ebRight, ebBottom];
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TToolWindow));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FEdgeBorders', System.TypeInfo(TEdgeBorders));
    AddField('FEdgeInner', System.TypeInfo(TEdgeStyle));
    AddField('FEdgeOuter', System.TypeInfo(TEdgeStyle));

    AddMethod('SetEdgeBorders', @TSepiImportsTToolWindow.SetEdgeBorders,
      'procedure(Value: TEdgeBorders)');
    AddMethod('SetEdgeInner', @TSepiImportsTToolWindow.SetEdgeInner,
      'procedure(Value: TEdgeStyle)');
    AddMethod('SetEdgeOuter', @TSepiImportsTToolWindow.SetEdgeOuter,
      'procedure(Value: TEdgeStyle)');

    CurrentVisibility := mvProtected;

    AddMethod('NCPaint', @TSepiImportsTToolWindow.NCPaint,
      'procedure(DC: HDC)',
      mlkVirtual);
    AddMethod('WMNCCalcSize', @TSepiImportsTToolWindow.WMNCCalcSize,
      'procedure(var Message: TWMNCCalcSize)',
      mlkMessage, False, WM_NCCALCSIZE);
    AddMethod('WMNCPaint', @TSepiImportsTToolWindow.WMNCPaint,
      'procedure(var Message: TMessage)',
      mlkMessage, False, WM_NCPAINT);
    AddMethod('CMBorderChanged', @TSepiImportsTToolWindow.CMBorderChanged,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_BORDERCHANGED);
    AddMethod('CMCtl3DChanged', @TSepiImportsTToolWindow.CMCtl3DChanged,
      'procedure(var Message: TMessage)',
      mlkMessage, False, CM_CTL3DCHANGED);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTToolWindow.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    AddProperty('EdgeBorders', 'property: TEdgeBorders',
      'FEdgeBorders', 'SetEdgeBorders',
      NoIndex, Byte(DefaultEdgeBorders));
    AddProperty('EdgeInner', 'property: TEdgeStyle',
      'FEdgeInner', 'SetEdgeInner',
      NoIndex, Integer(esRaised));
    AddProperty('EdgeOuter', 'property: TEdgeStyle',
      'FEdgeOuter', 'SetEdgeOuter',
      NoIndex, Integer(esLowered));

    Complete;
  end;
end;

{------------------------}
{ TToolDockObject import }
{------------------------}

class function TSepiImportsTToolDockObject.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TToolDockObject));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('AdjustDockRect', @TSepiImportsTToolDockObject.AdjustDockRect,
      'procedure(ARect: TRect)',
      mlkOverride);
    AddMethod('DrawDragDockImage', @TSepiImportsTToolDockObject.DrawDragDockImage,
      'procedure',
      mlkOverride);
    AddMethod('EraseDragDockImage', @TSepiImportsTToolDockObject.EraseDragDockImage,
      'procedure',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTToolDockObject.Create,
      'constructor(AControl: TControl)',
      mlkOverride);

    Complete;
  end;
end;

{----------------------}
{ TToolDockForm import }
{----------------------}

class function TSepiImportsTToolDockForm.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TToolDockForm));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPrevWidth', System.TypeInfo(Integer));
    AddField('FPrevHeight', System.TypeInfo(Integer));
    AddField('FSizingAdjustH', System.TypeInfo(Integer));
    AddField('FSizingAdjustW', System.TypeInfo(Integer));
    AddField('FSizingOrientation', System.TypeInfo(TSizingOrientation));
    AddField('FUpdatingSize', System.TypeInfo(Boolean));

    CurrentVisibility := mvProtected;

    AddMethod('CanResize', @TSepiImportsTToolDockForm.CanResize,
      'function(var NewWidth, NewHeight: Integer): Boolean',
      mlkOverride);
    AddMethod('CreateParams', @TSepiImportsTToolDockForm.CreateParams,
      'procedure(var Params: TCreateParams)',
      mlkOverride);
    AddMethod('DoAddDockClient', @TSepiImportsTToolDockForm.DoAddDockClient,
      'procedure(Client: TControl; const ARect: TRect)',
      mlkOverride);
    AddMethod('WMNCCreate', @TSepiImportsTToolDockForm.WMNCCreate,
      'procedure(var Message: TWMNCCreate)',
      mlkMessage, False, WM_NCCREATE);
    AddMethod('WMNCHitTest', @TSepiImportsTToolDockForm.WMNCHitTest,
      'procedure(var Message: TWMNCHitTest)',
      mlkMessage, False, WM_NCHITTEST);
    AddMethod('WMNCLButtonDown', @TSepiImportsTToolDockForm.WMNCLButtonDown,
      'procedure(var Message: TWMNCLButtonDown)',
      mlkMessage, False, WM_NCLBUTTONDOWN);
    AddMethod('WMSize', @TSepiImportsTToolDockForm.WMSize,
      'procedure(var Message: TWMSize)',
      mlkMessage, False, WM_SIZE);
    AddMethod('WMSysCommand', @TSepiImportsTToolDockForm.WMSysCommand,
      'procedure(var Message: TWMSysCommand)',
      mlkMessage, False, WM_SYSCOMMAND);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTToolDockForm.Create,
      'constructor(AOwner: TComponent)',
      mlkOverride);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ToolWin',
    ['Windows', 'Messages', 'Classes', 'Controls', 'Forms']);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEdgeBorder));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEdgeBorders));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEdgeStyle));
  TSepiImportsTToolWindow.SepiImport(Result);
  TSepiImportsTToolDockObject.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSizingOrientation));
  TSepiImportsTToolDockForm.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ToolWin', ImportUnit);
end.

