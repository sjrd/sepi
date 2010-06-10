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
  SepiArrayTypes, SepiMembers, ScUtils, ScMD5, MembersInfo;

procedure PrintTypeInfo(Output: TOutputWriter; SepiType: TSepiType);
procedure PrintConstInfo(Output: TOutputWriter; Constant: TSepiConstant);
procedure PrintVarInfo(Output: TOutputWriter; Variable: TSepiVariable);

procedure PrintValue(Output: TOutputWriter; Value: Pointer;
  ValueType: TSepiType);

procedure PrintComponentInfo(Output: TOutputWriter; Component: TSepiComponent);

implementation

procedure PrintIntInfo(Output: TOutputWriter; IntType: TSepiIntegerType);
begin
  Output.Write(IntType.Description);
  if IntType.Signed then
    Output.WriteLn(' (signed)')
  else
    Output.WriteLn(' (unsigned)');
end;

procedure PrintCharInfo(Output: TOutputWriter; CharType: TSepiCharType);
begin
  Output.Write(CharType.Description);
  if CharType.IsUnicode then
    Output.WriteLn(' (unicode)')
  else
    Output.WriteLn(' (ansi)');
end;

procedure PrintInt64Info(Output: TOutputWriter; Int64Type: TSepiInt64Type);
begin
  Output.WriteLn(Int64Type.Description);
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
begin
  Output.Write('From ' + IntToStr(EnumType.MinValue) + ' to ' +
    IntToStr(EnumType.MaxValue));
  Output.WriteLn(' (based on ' + EnumType.BaseType.Name + ')');
  Output.WriteLn(EnumType.Description);
end;

procedure PrintFakeEnumInfo(Output: TOutputWriter; EnumType: TSepiFakeEnumType);
begin
  Output.WriteLn('From ' + IntToStr(EnumType.MinValue) + ' to ' +
    IntToStr(EnumType.MaxValue));
  Output.WriteLn(EnumType.Description);
end;

procedure PrintSetInfo(Output: TOutputWriter; SetType: TSepiSetType);
begin
  Output.WriteLn(SetType.Description);
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
  Output.WriteLn('String kind: ' + GetEnumName(TypeInfo(TSepiStringKind),
    Ord(StringType.StringKind)));

  if StringType.StringKind = skAnsiString then
    Output.WriteLn('Code page: ' + IntToStr(StringType.CodePage));
end;

procedure PrintArrayInfo(Output: TOutputWriter;
  ArrayType: TSepiStaticArrayType);
begin
  Output.WriteLn(ArrayType.Description);
end;

procedure PrintDynArrayInfo(Output: TOutputWriter;
  DynArrayType: TSepiDynArrayType);
begin
  Output.WriteLn(DynArrayType.Description);
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
  Component: TSepiComponent;
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
    Component := IntfType.Children[I];
    if Component is TSepiMethod then
      PrintMethodInfo(Output, TSepiMethod(Component))
    else if Component is TSepiProperty then
      PrintPropertyInfo(Output, TSepiProperty(Component));
  end;

  Output.WriteLn('end;');
end;

procedure PrintClassInfo(Output: TOutputWriter; SepiClass: TSepiClass);
var
  OldVisib: TMemberVisibility;
  I: Integer;
  Component: TSepiComponent;
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
    Component := SepiClass.Children[I];
    if Component.Visibility <> OldVisib then
    begin
      Output.WriteLn(LowerCase(Copy(
        GetEnumName(TypeInfo(TMemberVisibility), Integer(Component.Visibility)),
        3, MaxInt)));
      OldVisib := Component.Visibility;
    end;

    if Component is TSepiField then
      PrintFieldInfo(Output, TSepiField(Component))
    else if Component is TSepiMethod then
      PrintMethodInfo(Output, TSepiMethod(Component))
    else if Component is TSepiProperty then
      PrintPropertyInfo(Output, TSepiProperty(Component));
  end;

  Output.WriteLn('end;');
end;

procedure PrintMetaClassInfo(Output: TOutputWriter; MetaClass: TSepiMetaClass);
begin
  Output.WriteLn(MetaClass.Description);
end;

procedure PrintMethodRefInfo(Output: TOutputWriter;
  MethodRef: TSepiMethodRefType);
begin
  PrintSignature(Output, MethodRef.Signature);
  Output.WriteLn(';');
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
  else if SepiType is TSepiFakeEnumType then
    PrintFakeEnumInfo(Output, TSepiFakeEnumType(SepiType))
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
          (TSepiPointerType(ValueType).PointTo.TypeInfo = TypeInfo(Char)) and
          (Cardinal(Value^) > $100) then
          Output.Write(PChar(Value^))
        else
          Output.Write('$'+IntToHex(PLongWord(Value)^, 8));
      end else if ValueType is TSepiFakeEnumType then
      begin
        Output.Write(IntToStr(
          TSepiFakeEnumType(ValueType).ValueAsInteger(Value^)));
      end else
        Output.Write('(unknown)');
    tkInteger:
      case ValueType.TypeData.OrdType of
        otUByte: Output.Write(IntToStr(PByte(Value)^));
        otSByte: Output.Write(IntToStr(PShortInt(Value)^));
        otUWord: Output.Write(IntToStr(PWord(Value)^));
        otSWord: Output.Write(IntToStr(PSmallInt(Value)^));
        otULong: Output.Write(IntToStr(Int64(PLongWord(Value)^)));
        otSLong: Output.Write(IntToStr(PLongInt(Value)^));
      end;
    tkInt64: Output.Write(IntToStr(PInt64(Value)^));
    tkFloat:
      case TSepiFloatType(ValueType).FloatType of
        ftSingle: Output.Write(FloatToStr(PSingle(Value)^));
        ftDouble: Output.Write(FloatToStr(PDouble(Value)^));
        ftExtended: Output.Write(FloatToStr(PExtended(Value)^));
        ftComp: Output.Write(FloatToStr(PComp(Value)^));
        ftCurr: Output.Write(FloatToStr(PCurrency(Value)^));
      end;
    tkLString: Output.Write(AnsiString(Value^));
    tkWString: Output.Write(WideString(Value^));
    tkEnumeration:
      case ValueType.Size of
        1: Output.Write(GetEnumName(ValueType.TypeInfo, PByte(Value)^));
        2: Output.Write(GetEnumName(ValueType.TypeInfo, PWord(Value)^));
      end;
    {$IF Declared(tkUString)}
    tkUString: Output.Write(UnicodeString(Value^));
    {$IFEND}
  else
    Output.Write('(unknown)');
  end;
end;

procedure PrintConstInfo(Output: TOutputWriter; Constant: TSepiConstant);
begin
  with Constant do
  begin
    Output.Write('const ' + Name + ' (' + ConstType.DisplayName + ') = ');
    PrintValue(Output, ValuePtr, ConstType);
    Output.WriteLn;
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
    Output.Write(Name + ': ' + VarType.DisplayName + ' = ');
    PrintValue(Output, Value, VarType);
    Output.WriteLn;
  end;
end;

procedure PrintComponentInfo(Output: TOutputWriter; Component: TSepiComponent);
begin
  Output.Write(Component.Name + ': ' + Component.ClassName + ' - ');
  Output.Write(VisibilityStrings[Component.Visibility]);
  Output.WriteLn(' - Digest: ' + MD5DigestToStr(Component.Digest));

  if Component is TSepiType then
    PrintTypeInfo(Output, TSepiType(Component))
  else if Component is TSepiTypeAlias then
    Output.WriteLn('-> ' + TSepiTypeAlias(Component).Dest.Name)
  else if Component is TSepiConstant then
    PrintConstInfo(Output, TSepiConstant(Component))
  else if Component is TSepiVariable then
    PrintVarInfo(Output, TSepiVariable(Component))
  else if Component is TSepiField then
    PrintFieldInfo(Output, TSepiField(Component))
  else if Component is TSepiMethod then
    PrintMethodInfo(Output, TSepiMethod(Component))
  else if Component is TSepiProperty then
    PrintPropertyInfo(Output, TSepiProperty(Component));
end;

end.

