{*
  Importe l'unité System dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsSystem;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
var I : integer;

    IntegerType : TSepiIntegerType;
    PointerType : TSepiIntegerType;
{    DoubleType : TSepiDoubleType;
    StringType : TSepiStringType;
    ShortStringType : TSepiStringType;
    PCharType : TSepiStringType;
    BooleanType : TSepiEnumType;}

    HRESULTType : TSepiIntegerType;

    //RecordType : TSepiRecordType;

    {ObjectType : TSepiObjectType;
    ClassType : TSepiClassType;}

    //TObjectClass : TSepiObjectType;
begin
  Result := TSepiMetaUnit.Create(Root, 'System');

  { Base types }
  IntegerType     := TSepiIntegerType.RegisterTypeInfo(
    Result, TypeInfo(integer));
  PointerType     := TSepiIntegerType.Create(Result, 'Pointer');
  {DoubleType      :=} TSepiFloatType .Create(Result, 'double');
  {StringType      :=} TSepiStringType .Create(Result, 'string');
  {ShortStringType :=} TSepiShortStringType .Create(Result, 'ShortString');
  {PCharType       :=} TSepiStringType .Create(Result, 'PChar');
  {BooleanType     :=} TSepiEnumType.Create(Result, 'Boolean', BooleanIdents);

  { Types declared in System.pas }
  HRESULTType := TSepiIntegerType.Create(Result, 'HRESULT');
  TSepiFloatType.Create(Result, 'TDateTime');
  TSepiIntegerType.Create(Result, 'THandle');

  { TGUID record }
  with TSepiRecordType.Create(Result, 'TGUID') do
  begin
    for I := 1 to 4 do
      AddField('D'+Chr(I+48), IntegerType);
  end;

  { TMethod record }
  with TSepiRecordType.Create(Result, 'TMethod') do
  begin
    AddField('Code', PointerType);
    AddField('Data', PointerType);
  end;

  { TObject class }
  {TObjectClass := ...}

  { Result constants }
  I := S_OK;
  TSepiConstant.Create(Result, 'S_OK', IntegerType).Value.WriteBuffer(I, 4);
  I := S_FALSE;
  TSepiConstant.Create(Result, 'S_FALSE', IntegerType).Value.WriteBuffer(I, 4);
  I := E_NOINTERFACE;
  TSepiConstant.Create(Result, 'E_NOINTERFACE',
    HRESULTType).Value.WriteBuffer(I, 4);
  I := E_UNEXPECTED;
  TSepiConstant.Create(Result, 'E_UNEXPECTED',
    HRESULTType).Value.WriteBuffer(I, 4);
  I := E_NOTIMPL;
  TSepiConstant.Create(Result, 'E_NOTIMPL',
    HRESULTType).Value.WriteBuffer(I, 4);

  { ... }
end;

initialization
  SepiRegisterImportedUnit('System', ImportUnit);
end.

