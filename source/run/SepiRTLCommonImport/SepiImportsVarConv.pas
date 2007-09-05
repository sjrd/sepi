{*
  Importe l'unité VarConv dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsVarConv;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, VarConv, ConvUtils;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function VarConvertCreate_0(const AValue: Double; const AType: TConvType): Variant;
begin
  Result := VarConvertCreate(AValue, AType);
end;

function VarConvertCreate_1(const AValue: string): Variant;
begin
  Result := VarConvertCreate(AValue);
end;

function VarAsConvert_0(const AValue: Variant): Variant;
begin
  Result := VarAsConvert(AValue);
end;

function VarAsConvert_1(const AValue: Variant; const AType: TConvType): Variant;
begin
  Result := VarAsConvert(AValue, AType);
end;

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'VarConv',
    ['SysUtils', 'Variants', 'ConvUtils']);

  // Routines
  TSepiMetaOverloadedMethod.Create(Result, 'VarConvertCreate');
  TSepiMetaMethod.Create(Result, 'OL$VarConvertCreate$0', @VarConvertCreate_0,
    'function(const AValue: Double; const AType: TConvType): Variant');
  TSepiMetaMethod.Create(Result, 'OL$VarConvertCreate$1', @VarConvertCreate_1,
    'function(const AValue: string): Variant');
  TSepiMetaMethod.Create(Result, 'VarConvert', @VarConvert,
    'function: TVarType');
  TSepiMetaMethod.Create(Result, 'VarIsConvert', @VarIsConvert,
    'function(const AValue: Variant): Boolean');
  TSepiMetaOverloadedMethod.Create(Result, 'VarAsConvert');
  TSepiMetaMethod.Create(Result, 'OL$VarAsConvert$0', @VarAsConvert_0,
    'function(const AValue: Variant): Variant');
  TSepiMetaMethod.Create(Result, 'OL$VarAsConvert$1', @VarAsConvert_1,
    'function(const AValue: Variant; const AType: TConvType): Variant');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('VarConv', ImportUnit);
end.

