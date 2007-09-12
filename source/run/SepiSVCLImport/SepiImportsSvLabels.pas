{*
  Importe l'unité SvLabels dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsSvLabels;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Menus, Controls, SvLabels;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTSvCustomURLLabel = class(TSvCustomURLLabel)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTSvURLLabel = class(TSvURLLabel)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{--------------------------}
{ TSvCustomURLLabel import }
{--------------------------}

class function TSepiImportsTSvCustomURLLabel.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSvCustomURLLabel));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FURL', System.TypeInfo(string));
    AddField('Menu', System.TypeInfo(TPopupMenu));

    AddMethod('RealURL', nil,
      'function: string');
    AddMethod('MenuCopyClick', nil,
      'procedure(Sender : TObject)');
    AddMethod('MenuRunClick', nil,
      'procedure(Sender : TObject)');
    AddMethod('IsFontStored', nil,
      'function: boolean');
    AddMethod('IsPopupMenuStored', nil,
      'function: boolean');

    CurrentVisibility := mvProtected;

    RedefineProperty('Cursor',
      '', '', Integer(crHandPoint));
    RedefineProperty('Font',
      '', '', 'IsFontStored');
    RedefineProperty('PopupMenu',
      '', '', 'IsPopupMenuStored');
    AddProperty('URL', 'property: string',
      'FURL', 'FURL');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTSvCustomURLLabel.Create,
      'constructor(AOwner : TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTSvCustomURLLabel.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Click', @TSepiImportsTSvCustomURLLabel.Click,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{--------------------}
{ TSvURLLabel import }
{--------------------}

class function TSepiImportsTSvURLLabel.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSvURLLabel));

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
    RedefineProperty('URL');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'SvLabels',
    ['Controls', 'Graphics', 'StdCtrls', 'Menus', 'ClipBrd',
    'Classes', 'ScUtils']);

  // Constants
  TSepiConstant.Create(Result, 'sCopyCaption', sCopyCaption);
  TSepiConstant.Create(Result, 'sCopyHint', sCopyHint);
  TSepiConstant.Create(Result, 'sRunCaption', sRunCaption);
  TSepiConstant.Create(Result, 'sRunHint', sRunHint);

  // Types
  TSepiImportsTSvCustomURLLabel.SepiImport(Result);
  TSepiImportsTSvURLLabel.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('SvLabels', ImportUnit);
end.

