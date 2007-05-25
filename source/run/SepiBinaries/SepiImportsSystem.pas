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
var PointerType : TSepiPointerType;
    TGUIDRecord : TSepiRecordType;
begin
  Result := TSepiMetaUnit.Create(Root, 'System');

  // Integer types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Integer));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Cardinal));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(ShortInt));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(SmallInt));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(LongInt));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Int64));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Byte));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Word));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(LongWord));

  // Character types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Char));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(AnsiChar));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(WideChar));

  // Boolean types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Boolean));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(ByteBool));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(WordBool));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(LongBool));

  // Float types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Single));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Double));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Extended));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Currency));

  // String types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(string));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(ShortString));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(AnsiString));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(WideString));

  // Pointer types
  PointerType := TSepiPointerType.Create(Result, 'Pointer', nil);
  TSepiPointerType.Create(Result, 'PChar', TypeInfo(Char));

  { Types declared in System.pas }
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(HRESULT));

  { TGUID record }
  TGUIDRecord := TSepiRecordType.Create(Result, 'TGUID', True, True);
  with TGUIDRecord do
  begin
    AddField('D1', System.TypeInfo(LongWord));
    AddField('D2', System.TypeInfo(Word));
    AddField('D3', System.TypeInfo(Word));
    AddField('D4', TSepiArrayType.Create(Result, 'TGUID$D4$TYPE', [0, 7],
      Root.FindType(System.TypeInfo(Byte))));
  end;
  TSepiPointerType.Create(Result, 'PGUID', TGUIDRecord);

  { TMethod record }
  with TSepiRecordType.Create(Result, 'TMethod', False, True) do
  begin
    AddField('Code', PointerType);
    AddField('Data', PointerType);
  end;

  { ... }
end;

initialization
  SepiRegisterImportedUnit('System', ImportUnit);
end.

