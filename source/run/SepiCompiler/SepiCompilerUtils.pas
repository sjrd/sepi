unit SepiCompilerUtils;

interface

uses
  SysUtils, Classes, SepiReflectionCore, SepiOrdTypes, SepiSystemUnit,
  SepiExpressions;

function ConstValueAsInt64(const Value: ISepiReadableValue): Int64;

procedure TryAndConvertValues(SystemUnit: TSepiSystemUnit;
  var LowerValue, HigherValue: ISepiReadableValue);

implementation

{-----------------}
{ Global routines }
{-----------------}

{*
  Lit une valeur constante comme un Int64
  @param Value   Valeur à lire
  @return Valeur sous forme d'Int64
*}
function ConstValueAsInt64(const Value: ISepiReadableValue): Int64;
var
  IntegerType: TSepiIntegerType;
begin
  if Value.ValueType is TSepiInt64Type then
    Result := Int64(Value.ConstValuePtr^)
  else
  begin
    IntegerType := TSepiIntegerType(Value.ValueType);

    if IntegerType.Signed then
      Result := IntegerType.ValueAsInteger(Value.ConstValuePtr^)
    else
      Result := IntegerType.ValueAsCardinal(Value.ConstValuePtr^);
  end;
end;

{*
  Essaie de convertir les valeurs pour qu'elles aient le même type
  @param SystemUnit    Unité système
  @param LowerValue    Valeur basse
  @param HigherValue   Valeur haute
*}
procedure TryAndConvertValues(SystemUnit: TSepiSystemUnit;
  var LowerValue, HigherValue: ISepiReadableValue);
var
  LowerType, HigherType, CommonType: TSepiType;
  IntLowerValue, IntHigherValue: Int64;
begin
  LowerType := LowerValue.ValueType;
  HigherType := HigherValue.ValueType;

  if (LowerType is TSepiEnumType) and (HigherType is TSepiEnumType) then
  begin
    // Enumeration types

    CommonType := TSepiEnumType(LowerType).BaseType;

    if TSepiEnumType(HigherType).BaseType = CommonType then
    begin
      if LowerType <> CommonType then
        LowerValue := TSepiCastOperator.CastValue(
          CommonType, LowerValue) as ISepiReadableValue;

      if HigherType <> CommonType then
        HigherValue := TSepiCastOperator.CastValue(
          CommonType, HigherValue) as ISepiReadableValue;
    end;
  end else
  begin
    // Integer or char types

    if ((LowerType is TSepiIntegerType) or (LowerType is TSepiInt64Type)) and
      ((HigherType is TSepiIntegerType) or (HigherType is TSepiInt64Type)) then
    begin
      // Integer types

      IntLowerValue := ConstValueAsInt64(LowerValue);
      IntHigherValue := ConstValueAsInt64(HigherValue);

      if (Integer(IntLowerValue) = IntLowerValue) and
        (Integer(IntHigherValue) = IntHigherValue) then
        CommonType := SystemUnit.Integer
      else if (Cardinal(IntLowerValue) = IntLowerValue) and
        (Cardinal(IntHigherValue) = IntHigherValue) then
        CommonType := SystemUnit.Cardinal
      else
        CommonType := SystemUnit.Int64;
    end else if (LowerType is TSepiCharType) and
      (HigherType is TSepiCharType) then
    begin
      // Char types

      if LowerType.Size >= HigherType.Size then
        CommonType := LowerType
      else
        CommonType := HigherType;
    end else
    begin
      // Error
      Exit;
    end;

    if LowerType <> CommonType then
      LowerValue := TSepiConvertOperation.ConvertValue(
        CommonType, LowerValue);

    if HigherType <> CommonType then
      HigherValue := TSepiConvertOperation.ConvertValue(
        CommonType, HigherValue) as ISepiReadableValue;
  end;
end;

end.

