{*
  Importe l'unité SvEdits dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsSvEdits;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, SvEdits;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTSvCustomNumberEdit = class(TSvCustomNumberEdit)
  private
    function GetValue: Double;
    procedure SetValue(New: Double);
    function GetInt: Integer;
    procedure SetInt(New: Integer);
    procedure SetPrefix(New: string);
    procedure SetSuffix(New: string);
    procedure SetAllowNegative(New: Boolean);
    procedure SetAllowFrac(New: Boolean);
    procedure SetSeparator(New: Char);
    procedure SetHasMinValue(New: Boolean);
    procedure SetHasMaxValue(New: Boolean);
    procedure SetMinValue(New: Double);
    procedure SetMaxValue(New: Double);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTSvNumberEdit = class(TSvNumberEdit)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{----------------------------}
{ TSvCustomNumberEdit import }
{----------------------------}

function TSepiImportsTSvCustomNumberEdit.GetValue: Double;
begin
  Result := Value;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetValue(New: Double);
begin
  Value := New;
end;

function TSepiImportsTSvCustomNumberEdit.GetInt: Integer;
begin
  Result := Int;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetInt(New: Integer);
begin
  Int := New;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetPrefix(New: string);
begin
  Prefix := New;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetSuffix(New: string);
begin
  Suffix := New;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetAllowNegative(New: Boolean);
begin
  AllowNegative := New;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetAllowFrac(New: Boolean);
begin
  AllowFrac := New;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetSeparator(New: Char);
begin
  Separator := New;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetHasMinValue(New: Boolean);
begin
  HasMinValue := New;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetHasMaxValue(New: Boolean);
begin
  HasMaxValue := New;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetMinValue(New: Double);
begin
  MinValue := New;
end;

procedure TSepiImportsTSvCustomNumberEdit.SetMaxValue(New: Double);
begin
  MaxValue := New;
end;

class function TSepiImportsTSvCustomNumberEdit.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSvCustomNumberEdit));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FReadingValue', System.TypeInfo(Double));
    AddField('FPrefix', System.TypeInfo(string));
    AddField('FSuffix', System.TypeInfo(string));
    AddField('FAllowNegative', System.TypeInfo(Boolean));
    AddField('FAllowFrac', System.TypeInfo(Boolean));
    AddField('FSeparator', System.TypeInfo(Char));
    AddField('FHasMinValue', System.TypeInfo(Boolean));
    AddField('FHasMaxValue', System.TypeInfo(Boolean));
    AddField('FMinValue', System.TypeInfo(Double));
    AddField('FMaxValue', System.TypeInfo(Double));

    AddMethod('GetValue', @TSepiImportsTSvCustomNumberEdit.GetValue,
      'function: Double');
    AddMethod('SetValue', @TSepiImportsTSvCustomNumberEdit.SetValue,
      'procedure(New : Double)');
    AddMethod('GetInt', @TSepiImportsTSvCustomNumberEdit.GetInt,
      'function: integer');
    AddMethod('SetInt', @TSepiImportsTSvCustomNumberEdit.SetInt,
      'procedure(New : integer)');
    AddMethod('SetPrefix', @TSepiImportsTSvCustomNumberEdit.SetPrefix,
      'procedure(New : string)');
    AddMethod('SetSuffix', @TSepiImportsTSvCustomNumberEdit.SetSuffix,
      'procedure(New : string)');
    AddMethod('SetAllowNegative',
      @TSepiImportsTSvCustomNumberEdit.SetAllowNegative,
      'procedure(New : boolean)');
    AddMethod('SetAllowFrac', @TSepiImportsTSvCustomNumberEdit.SetAllowFrac,
      'procedure(New : boolean)');
    AddMethod('SetSeparator', @TSepiImportsTSvCustomNumberEdit.SetSeparator,
      'procedure(New : Char)');
    AddMethod('SetHasMinValue',
      @TSepiImportsTSvCustomNumberEdit.SetHasMinValue,
      'procedure(New : boolean)');
    AddMethod('SetHasMaxValue',
      @TSepiImportsTSvCustomNumberEdit.SetHasMaxValue,
      'procedure(New : boolean)');
    AddMethod('SetMinValue', @TSepiImportsTSvCustomNumberEdit.SetMinValue,
      'procedure(New : Double)');
    AddMethod('SetMaxValue', @TSepiImportsTSvCustomNumberEdit.SetMaxValue,
      'procedure(New : Double)');
    AddMethod('SelEnd', nil,
      'function: integer');

    CurrentVisibility := mvProtected;

    AddMethod('KeyDown', @TSepiImportsTSvCustomNumberEdit.KeyDown,
      'procedure(var Key: Word; Shift: TShiftState)',
      mlkOverride);
    AddMethod('KeyPress', @TSepiImportsTSvCustomNumberEdit.KeyPress,
      'procedure(var Key: Char)',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTSvCustomNumberEdit.Loaded,
      'procedure',
      mlkOverride);

    AddProperty('Int', 'property: integer',
      'GetInt', 'SetInt');
    RedefineProperty('AutoSelect',
      '', '', Integer(False));
    AddProperty('Value', 'property: Double',
      'GetValue', 'SetValue');
    AddProperty('Prefix', 'property: string',
      'FPrefix', 'SetPrefix');
    AddProperty('Suffix', 'property: string',
      'FSuffix', 'SetSuffix');
    AddProperty('AllowNegative', 'property: boolean',
      'FAllowNegative', 'SetAllowNegative',
      NoIndex, Integer(False));
    AddProperty('AllowFrac', 'property: boolean',
      'FAllowFrac', 'SetAllowFrac',
      NoIndex, Integer(False));
    AddProperty('Separator', 'property: Char',
      'FSeparator', 'SetSeparator',
      NoIndex, Integer(','));
    AddProperty('HasMinValue', 'property: boolean',
      'FHasMinValue', 'SetHasMinValue',
      NoIndex, Integer(False));
    AddProperty('HasMaxValue', 'property: boolean',
      'FHasMaxValue', 'SetHasMaxValue',
      NoIndex, Integer(False));
    AddProperty('MinValue', 'property: Double',
      'FMinValue', 'SetMinValue');
    AddProperty('MaxValue', 'property: Double',
      'FMaxValue', 'SetMaxValue');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTSvCustomNumberEdit.Create,
      'constructor(AOwner : TComponent)',
      mlkOverride);

    Complete;
  end;
end;

{----------------------}
{ TSvNumberEdit import }
{----------------------}

class function TSepiImportsTSvNumberEdit.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSvNumberEdit));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    RedefineProperty('Int');

    CurrentVisibility := mvPublished;

    RedefineProperty('AutoSelect');
    RedefineProperty('AutoSize');
    RedefineProperty('BorderStyle');
    RedefineProperty('HideSelection');
    RedefineProperty('ReadOnly');
    RedefineProperty('TabOrder');
    RedefineProperty('Action');
    RedefineProperty('Align');
    RedefineProperty('Anchors');
    RedefineProperty('BiDiMode');
    RedefineProperty('Constraints');
    RedefineProperty('DockOrientation');
    RedefineProperty('Enabled');
    RedefineProperty('FloatingDockSiteClass');
    RedefineProperty('ParentColor');
    RedefineProperty('ShowHint');
    RedefineProperty('Visible');
    RedefineProperty('Value');
    RedefineProperty('Prefix');
    RedefineProperty('Suffix');
    RedefineProperty('AllowNegative');
    RedefineProperty('AllowFrac');
    RedefineProperty('Separator');
    RedefineProperty('HasMinValue');
    RedefineProperty('HasMaxValue');
    RedefineProperty('MinValue');
    RedefineProperty('MaxValue');
    RedefineProperty('OnChange');
    RedefineProperty('OnClick');
    RedefineProperty('OnDblClick');
    RedefineProperty('OnMouseDown');
    RedefineProperty('OnMouseMove');
    RedefineProperty('OnMouseUp');
    RedefineProperty('OnMouseWheel');
    RedefineProperty('OnMouseWheelDown');
    RedefineProperty('OnMouseWheelUp');
    RedefineProperty('OnKeyDown');
    RedefineProperty('OnKeyPress');
    RedefineProperty('OnKeyUp');
    RedefineProperty('OnEnter');
    RedefineProperty('OnExit');
    RedefineProperty('OnDockDrop');
    RedefineProperty('OnDockOver');
    RedefineProperty('OnGetSiteInfo');
    RedefineProperty('OnUnDock');
    RedefineProperty('OnCanResize');
    RedefineProperty('OnConstrainedResize');
    RedefineProperty('OnResize');
    RedefineProperty('OnContextPopup');
    RedefineProperty('OnDragDrop');
    RedefineProperty('OnDragOver');
    RedefineProperty('OnStartDock');
    RedefineProperty('OnEndDock');
    RedefineProperty('OnStartDrag');
    RedefineProperty('OnEndDrag');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'SvEdits',
    ['Windows', 'Messages', 'SysUtils', 'Classes', 'ScUtils', 'StdCtrls']);

  // Types
  TSepiImportsTSvCustomNumberEdit.SepiImport(Result);
  TSepiImportsTSvNumberEdit.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('SvEdits', ImportUnit);
end.

