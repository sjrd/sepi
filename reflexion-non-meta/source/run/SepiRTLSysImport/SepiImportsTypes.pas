{*
  Importe l'unité Types dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsTypes;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Types;

implementation

{ You must not localize any of the strings this unit contains! }

{---------------}
{ TPoint import }
{---------------}

function SepiImportTPoint(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TPoint', True, True);

  with Result do
  begin
    AddField('X', System.TypeInfo(Longint));
    AddField('Y', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------}
{ TRect import }
{--------------}

function SepiImportTRect(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TRect', True, True);

  with Result do
  begin
    AddFieldAfter('Left', System.TypeInfo(Longint), '');
    AddField('Top', System.TypeInfo(Longint), True);
    AddField('Right', System.TypeInfo(Longint), True);
    AddField('Bottom', System.TypeInfo(Longint), True);
    AddFieldAfter('TopLeft', 'TPoint', '');
    AddField('BottomRight', 'TPoint', True);

    Complete;
  end;
end;

{----------------}
{ tagSIZE import }
{----------------}

function SepiImporttagSIZE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSIZE', True, True);

  with Result do
  begin
    AddField('cx', System.TypeInfo(Longint));
    AddField('cy', System.TypeInfo(Longint));

    Complete;
  end;
end;

{--------------------}
{ TSmallPoint import }
{--------------------}

function SepiImportTSmallPoint(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TSmallPoint', True, True);

  with Result do
  begin
    AddField('x', System.TypeInfo(SmallInt));
    AddField('y', System.TypeInfo(SmallInt));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function SmallPoint_0(X, Y: Integer): TSmallPoint;
begin
  Result := SmallPoint(X, Y);
end;

function SmallPoint_1(XY: LongWord): TSmallPoint;
begin
  Result := SmallPoint(XY);
end;

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'Types', []);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TIntegerDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCardinalDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TWordDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSmallIntDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TByteDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TShortIntDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TInt64DynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLongWordDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSingleDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDoubleDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBooleanDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TStringDynArray));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TWideStringDynArray));
  TSepiPointerType.Create(Result, 'PPoint', 'TPoint', True);
  SepiImportTPoint(Result);
  TSepiTypeAlias.Create(Result, 'tagPOINT', 'TPoint');
  TSepiPointerType.Create(Result, 'PRect', 'TRect', True);
  SepiImportTRect(Result);
  TSepiPointerType.Create(Result, 'PSize', 'TSize', True);
  SepiImporttagSIZE(Result);
  TSepiTypeAlias.Create(Result, 'TSize', 'tagSIZE');
  TSepiTypeAlias.Create(Result, 'SIZE', 'tagSIZE');
  TSepiPointerType.Create(Result, 'PSmallPoint', 'TSmallPoint', True);
  SepiImportTSmallPoint(Result);
  TSepiTypeAlias.Create(Result, 'DWORD', TypeInfo(LongWord));

  // Constants
  TSepiConstant.Create(Result, 'RT_RCDATA', LongWord(RT_RCDATA), 'PChar');

  // Routines
  TSepiMetaMethod.Create(Result, 'EqualRect', @EqualRect,
    'function(const R1, R2: TRect): Boolean');
  TSepiMetaMethod.Create(Result, 'Rect', @Rect,
    'function(Left, Top, Right, Bottom: Integer): TRect');
  TSepiMetaMethod.Create(Result, 'Bounds', @Bounds,
    'function(ALeft, ATop, AWidth, AHeight: Integer): TRect');
  TSepiMetaMethod.Create(Result, 'Point', @Point,
    'function(X, Y: Integer): TPoint');
  TSepiMetaOverloadedMethod.Create(Result, 'SmallPoint');
  TSepiMetaMethod.Create(Result, 'OL$SmallPoint$0', @SmallPoint_0,
    'function(X, Y: Integer): TSmallPoint');
  TSepiMetaMethod.Create(Result, 'OL$SmallPoint$1', @SmallPoint_1,
    'function(XY: LongWord): TSmallPoint');
  TSepiMetaMethod.Create(Result, 'PtInRect', @PtInRect,
    'function(const Rect: TRect; const P: TPoint): Boolean');
  TSepiMetaMethod.Create(Result, 'IntersectRect', @IntersectRect,
    'function(out Rect: TRect; const R1, R2: TRect): Boolean');
  TSepiMetaMethod.Create(Result, 'UnionRect', @UnionRect,
    'function(out Rect: TRect; const R1, R2: TRect): Boolean');
  TSepiMetaMethod.Create(Result, 'IsRectEmpty', @IsRectEmpty,
    'function(const Rect: TRect): Boolean');
  TSepiMetaMethod.Create(Result, 'OffsetRect', @OffsetRect,
    'function(var Rect: TRect; DX: Integer; DY: Integer): Boolean');
  TSepiMetaMethod.Create(Result, 'CenterPoint', @CenterPoint,
    'function(const Rect: TRect): TPoint');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TValueRelationship));

  // Constants
  TSepiConstant.Create(Result, 'LessThanValue', LessThanValue,
    TypeInfo(TValueRelationship));
  TSepiConstant.Create(Result, 'EqualsValue', EqualsValue,
    TypeInfo(TValueRelationship));
  TSepiConstant.Create(Result, 'GreaterThanValue', GreaterThanValue,
    TypeInfo(TValueRelationship));

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Types', ImportUnit);
end.

