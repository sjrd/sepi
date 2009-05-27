{-------------------------------------------------------------------------------
SepiExplorer - Example program for Sepi
As an example program, SepiExplorer is free of any usage. It is released in the
public domain.
-------------------------------------------------------------------------------}

{*
  Informations sur les types Sepi
  @author sjrd
  @version 1.0
*}
unit TypesInfo;

interface

uses
  SysUtils, StrUtils, TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes,
  SepiArrayTypes, SepiMembers, ScUtils, MembersInfo;

procedure PrintTypeInfo(Output: TOutputWriter; SepiType: TSepiType);
procedure PrintConstInfo(Output: TOutputWriter; Constant: TSepiConstant);
procedure PrintVarInfo(Output: TOutputWriter; Variable: TSepiVariable);

procedure PrintMetaInfo(Output: TOutputWriter; Meta: TSepiMeta);

implementation

procedure PrintIntInfo(Output: TOutputWriter; IntType: TSepiIntegerType);
begin
  Output.Write('From ' + IntToStr(IntType.MinValue) + ' to ' +
    IntToStr(IntType.MaxValue));
  if IntType.Signed then
    Output.WriteLn(' (signed)')
  else
    Output.WriteLn(' (unsigned)');
end;

procedure PrintCharInfo(Output: TOutputWriter; CharType: TSepiCharType);
begin
  Output.Write('From ' + IntToStr(CharType.MinValue) + ' to ' +
    IntToStr(CharType.MaxValue));
  if CharType.IsUnicode then
    Output.WriteLn(' (unicode)')
  else
    Output.WriteLn(' (ansi)');
end;

procedure PrintInt64Info(Output: TOutputWriter; Int64Type: TSepiInt64Type);
begin
  Output.WriteLn('From ' + IntToStr(Int64Type.MinValue) + ' to ' +
    IntToStr(Int64Type.MaxValue));
end;

procedure PrintFloatInfo(Output: TOutputWriter; FloatType: TSepiFloatType);
begin
  Output.WriteLn('Float type: ' +
    GetEnumName(TypeInfo(TFloatType), Integer(FloatType.FloatType)));
end;

procedure PrintBooleanInfo(Output: TOutputWriter;
  BooleanType: TSepiBooleanType);
begin
  Output.WriteLn('Boolean kind: ' +
    GetEnumName(TypeInfo(TBooleanKind), Integer(BooleanType.BooleanKind)));
end;

procedure PrintEnumInfo(Output: TOutputWriter; EnumType: TSepiEnumType);
var
  Value: Integer;
begin
  Output.Write('From ' + IntToStr(EnumType.MinValue) + ' to ' +
    IntToStr(EnumType.MaxValue));
  Output.WriteLn(' (based on ' + EnumType.BaseType.Name + ')');

  Output.Write('  (');
  for Value := EnumType.MinValue to EnumType.MaxValue-1 do
    Output.Write(EnumType.Names[Value] + ', ');
  Output.WriteLn(EnumType.Names[EnumType.MaxValue] + ')');
end;

procedure PrintSetInfo(Output: TOutputWriter; SetType: TSepiSetType);
begin
  Output.WriteLn('Component type: ' + SetType.CompType.Name);
end;

procedure PrintPointerInfo(Output: TOutputWriter;
  PointerType: TSepiPointerType);
begin
  Output.Write('Points to: ');
  if Assigned(PointerType.PointTo) then
    Output.WriteLn(PointerType.PointTo.Name)
  else
    Output.WriteLn('(void)');
end;

procedure PrintShortStringInfo(Output: TOutputWriter;
  ShortStringType: TSepiShortStringType);
begin
  Output.WriteLn('Max length: ' + IntToStr(ShortStringType.MaxLength));
end;

procedure PrintStringInfo(Output: TOutputWriter; StringType: TSepiStringType);
begin
  Output.WriteLn('Unicode: ' + BooleanIdents[StringType.IsUnicode]);
end;

procedure PrintArrayInfo(Output: TOutputWriter;
  ArrayType: TSepiStaticArrayType);
begin
  Output.Write('array[' + IntToStr(ArrayType.LowerBound) + ', ' +
    IntToStr(ArrayType.HigherBound));
  Output.WriteLn('] of ' + ArrayType.ElementType.Name);
end;

procedure PrintDynArrayInfo(Output: TOutputWriter;
  DynArrayType: TSepiDynArrayType);
begin
  Output.WriteLn('array of ' + DynArrayType.ElementType.Name);
end;

procedure PrintRecordInfo(Output: TOutputWriter; RecordType: TSepiRecordType);
var
  I: Integer;
begin
  if RecordType.IsPacked then
    Output.Write('packed ');
  Output.WriteLn('record');

  for I := 0 to RecordType.ChildCount-1 do
    if RecordType.Children[I] is TSepiField then
      PrintFieldInfo(Output, TSepiField(RecordType.Children[I]));

  Output.WriteLn('end;');
end;

procedure PrintInterfaceInfo(Output: TOutputWriter; IntfType: TSepiInterface);
var
  I: Integer;
  Meta: TSepiMeta;
begin
  if IntfType.IsDispInterface then
    Output.WriteLn('dispinterface')
  else if IntfType.Parent = nil then
    Output.WriteLn('interface')
  else
    Output.WriteLn('interface(' + IntfType.Parent.Name + ')');
  if IntfType.HasGUID then
    Output.WriteLn('  [''' + GUIDToString(IntfType.GUID) + ''']');

  for I := 0 to IntfType.ChildCount-1 do
  begin
    Meta := IntfType.Children[I];
    if Meta is TSepiMethod then
      PrintMethodInfo(Output, TSepiMethod(Meta))
    else if Meta is TSepiProperty then
      PrintPropertyInfo(Output, TSepiProperty(Meta));
  end;

  Output.WriteLn('end;');
end;

procedure PrintClassInfo(Output: TOutputWriter; SepiClass: TSepiClass);
var
  OldVisib: TMemberVisibility;
  I: Integer;
  Meta: TSepiMeta;
begin
  if SepiClass.Parent = nil then
    Output.WriteLn('class')
  else
  begin
    Output.Write('class(' + SepiClass.Parent.Name);
    for I := 0 to SepiClass.InterfaceCount-1 do
      Output.Write(', ' + SepiClass.Interfaces[I].Name);
    Output.WriteLn(')');
  end;
  OldVisib := mvPublished;

  for I := 0 to SepiClass.ChildCount-1 do
  begin
    Meta := SepiClass.Children[I];
    if Meta.Visibility <> OldVisib then
    begin
      Output.WriteLn(LowerCase(Copy(
        GetEnumName(TypeInfo(TMemberVisibility), Integer(Meta.Visibility)),
        3, MaxInt)));
      OldVisib := Meta.Visibility;
    end;

    if Meta is TSepiField then
      PrintFieldInfo(Output, TSepiField(Meta))
    else if Meta is TSepiMethod then
      PrintMethodInfo(Output, TSepiMethod(Meta))
    else if Meta is TSepiProperty then
      PrintPropertyInfo(Output, TSepiProperty(Meta));
  end;

  Output.WriteLn('end;');
end;

procedure PrintMetaClassInfo(Output: TOutputWriter; MetaClass: TSepiMetaClass);
begin
  Output.WriteLn('class of ' + MetaClass.SepiClass.Name);
end;

procedure PrintMethodRefInfo(Output: TOutputWriter;
  MethodRef: TSepiMethodRefType);
begin
  PrintSignature(Output, MethodRef.Signature);
  Output.WriteLn;
end;

procedure PrintTypeInfo(Output: TOutputWriter; SepiType: TSepiType);
begin
  Output.Write('  ' + GetEnumName(TypeInfo(TTypeKind), Integer(SepiType.Kind)) +
    ';'#9);
  if not SepiType.Native then
    Output.Write('not ');
  Output.Write('native; '#9);
  Output.Write(IntToStr(SepiType.Size) + ' bytes-wide');
  if SepiType.NeedInit then
    Output.Write(';'#9'needs initialization');
  Output.Write(';'#9'alignment: ' + IntToStr(SepiType.Alignment));
  Output.WriteLn;

  Output.Write('  ParamByAddr: ');
  Output.Write(BooleanIdents[SepiType.ParamBehavior.AlwaysByAddress]);
  Output.Write(#9'ParamOnStack: ');
  Output.Write(BooleanIdents[SepiType.ParamBehavior.AlwaysByStack]);
  Output.Write(#9'Result as: ');
  Output.Write(GetEnumName(
    TypeInfo(TSepiTypeResultBehavior), Integer(SepiType.ResultBehavior)));
  Output.WriteLn;

  if SepiType is TSepiIntegerType then
    PrintIntInfo(Output, TSepiIntegerType(SepiType))
  else if SepiType is TSepiCharType then
    PrintCharInfo(Output, TSepiCharType(SepiType))
  else if SepiType is TSepiInt64Type then
    PrintInt64Info(Output, TSepiInt64Type(SepiType))
  else if SepiType is TSepiFloatType then
    PrintFloatInfo(Output, TSepiFloatType(SepiType))
  else if SepiType is TSepiBooleanType then
    PrintBooleanInfo(Output, TSepiBooleanType(SepiType))
  else if SepiType is TSepiEnumType then
    PrintEnumInfo(Output, TSepiEnumType(SepiType))
  else if SepiType is TSepiSetType then
    PrintSetInfo(Output, TSepiSetType(SepiType))
  else if SepiType is TSepiPointerType then
    PrintPointerInfo(Output, TSepiPointerType(SepiType))

  else if SepiType is TSepiShortStringType then
    PrintShortStringInfo(Output, TSepiShortStringType(SepiType))
  else if SepiType is TSepiStringType then
    PrintStringInfo(Output, TSepiStringType(SepiType))

  else if SepiType is TSepiStaticArrayType then
    PrintArrayInfo(Output, TSepiStaticArrayType(SepiType))
  else if SepiType is TSepiDynArrayType then
    PrintDynArrayInfo(Output, TSepiDynArrayType(SepiType))

  else if SepiType is TSepiRecordType then
    PrintRecordInfo(Output, TSepiRecordType(SepiType))
  else if SepiType is TSepiInterface then
    PrintInterfaceInfo(Output, TSepiInterface(SepiType))
  else if SepiType is TSepiClass then
    PrintClassInfo(Output, TSepiClass(SepiType))
  else if SepiType is TSepiMetaClass then
    PrintMetaClassInfo(Output, TSepiMetaClass(SepiType))
  else if SepiType is TSepiMethodRefType then
    PrintMethodRefInfo(Output, TSepiMethodRefType(SepiType));
end;

procedure PrintValue(Output: TOutputWriter; Value: Pointer;
  ValueType: TSepiType);
begin
  case ValueType.Kind of
    tkUnknown:
      if ValueType is TSepiPointerType then
      begin
        if Assigned(TSepiPointerType(ValueType).PointTo) and
          (TSepiPointerType(ValueType).PointTo.TypeInfo = TypeInfo(Char)) then
          Output.WriteLn(PChar(Value^))
        else
          Output.WriteLn('$'+IntToHex(PLongWord(Value)^, 8));
      end else
        Output.WriteLn('(unknown)');
    tkInteger:
      case ValueType.TypeData.OrdType of
        otUByte: Output.WriteLn(IntToStr(PByte(Value)^));
        otSByte: Output.WriteLn(IntToStr(PShortInt(Value)^));
        otUWord: Output.WriteLn(IntToStr(PWord(Value)^));
        otSWord: Output.WriteLn(IntToStr(PSmallInt(Value)^));
        otULong: Output.WriteLn(IntToStr(Int64(PLongWord(Value)^)));
        otSLong: Output.WriteLn(IntToStr(PLongInt(Value)^));
      end;
    tkInt64: Output.WriteLn(IntToStr(PInt64(Value)^));
    tkFloat:
      case TSepiFloatType(ValueType).FloatType of
        ftSingle: Output.WriteLn(FloatToStr(PSingle(Value)^));
        ftDouble: Output.WriteLn(FloatToStr(PDouble(Value)^));
        ftExtended: Output.WriteLn(FloatToStr(PExtended(Value)^));
        ftComp: Output.WriteLn(FloatToStr(PComp(Value)^));
        ftCurr: Output.WriteLn(FloatToStr(PCurrency(Value)^));
      end;
    tkLString: Output.WriteLn(string(Value^));
    tkEnumeration:
      case ValueType.Size of
        1: Output.WriteLn(GetEnumName(ValueType.TypeInfo, PByte(Value)^));
        2: Output.WriteLn(GetEnumName(ValueType.TypeInfo, PWord(Value)^));
      end;
  else
    Output.WriteLn('(unknown)');
  end;
end;

procedure PrintConstInfo(Output: TOutputWriter; Constant: TSepiConstant);
begin
  with Constant do
  begin
    Output.Write('const ' + Name + ' (' + ConstType.Name + ') = ');
    PrintValue(Output, ValuePtr, ConstType);
  end;
end;

procedure PrintVarInfo(Output: TOutputWriter; Variable: TSepiVariable);
begin
  with Variable do
  begin
    if IsConst then
      Output.Write('const ')
    else
      Output.Write('var ');
    Output.Write(Name + ': ' + VarType.Name + ' = ');
    PrintValue(Output, Value, VarType);
  end;
end;

procedure PrintMetaInfo(Output: TOutputWriter; Meta: TSepiMeta);
begin
  Output.WriteLn(Meta.Name + ': ' + Meta.ClassName);

  if Meta is TSepiType then
    PrintTypeInfo(Output, TSepiType(Meta))
  else if Meta is TSepiTypeAlias then
    Output.WriteLn('-> ' + TSepiTypeAlias(Meta).Dest.Name)
  else if Meta is TSepiConstant then
    PrintConstInfo(Output, TSepiConstant(Meta))
  else if Meta is TSepiVariable then
    PrintVarInfo(Output, TSepiVariable(Meta))
  else if Meta is TSepiField then
    PrintFieldInfo(Output, TSepiField(Meta))
  else if Meta is TSepiMethod then
    PrintMethodInfo(Output, TSepiMethod(Meta))
  else if Meta is TSepiProperty then
    PrintPropertyInfo(Output, TSepiProperty(Meta));
end;

end.

