unit SepiCompilerUtils;

interface

uses
  SysUtils, Classes, SepiReflectionCore, SepiOrdTypes, SepiSystemUnit,
  SepiExpressions, SepiParseTrees, SepiLexerUtils, SepiParserUtils,
  SepiCompilerErrors, SepiCompiler, SepiCompilerConsts;

function ConstValueAsInt64(const Value: ISepiReadableValue): Int64;

procedure TryAndConvertValues(SystemUnit: TSepiSystemUnit;
  var LowerValue, HigherValue: ISepiReadableValue);

function CompileSepiSource(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList; SourceFile: TStrings;
  const DestFileName: TFileName; RootNodeClass: TSepiParseTreeRootNodeClass;
  RootSymbolClass: TSepiSymbolClass; LexerClass: TSepiCustomLexerClass;
  ParserClass: TSepiCustomParserClass): TSepiUnit;

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

{*
  Compile un fichier source Sepi
  @param SepiRoot          Racine Sepi
  @param Errors            Gestionnaire d'erreurs
  @param SourceFile        Source à compiler
  @param DestFileName      Nom du fichier de sortie
  @param RootNodeClass     Classe du noeud racine
  @param RootSymbolClass   Classe de symboles du noeud racine
  @param LexerClass        Classe de l'analyseur lexical
  @param ParserClass       Classe de l'analyseur syntaxique
  @return Unité Sepi compilée
*}
function CompileSepiSource(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList; SourceFile: TStrings;
  const DestFileName: TFileName; RootNodeClass: TSepiParseTreeRootNodeClass;
  RootSymbolClass: TSepiSymbolClass; LexerClass: TSepiCustomLexerClass;
  ParserClass: TSepiCustomParserClass): TSepiUnit;
var
  DestFile: TStream;
  RootNode: TSepiParseTreeRootNode;
  Compiler: TSepiUnitCompiler;
begin
  // Silence the compiler warning
  Result := nil;

  DestFile := nil;
  RootNode := nil;
  try
    // Actually compile the source file
    RootNode := RootNodeClass.Create(RootSymbolClass, SepiRoot, Errors);
    try
      ParserClass.Parse(RootNode, LexerClass.Create(Errors,
        SourceFile.Text, Errors.CurrentFileName));
    except
      on Error: ESepiCompilerFatalError do
        raise;
      on Error: Exception do
      begin
        Errors.MakeError(Error.Message, ekFatalError,
          RootNode.FindRightMost.SourcePos);
      end;
    end;

    // Check for errors
    Errors.CheckForErrors;

    // Fetch Sepi unit compiler and unit
    Compiler := RootNode.UnitCompiler;
    Result := Compiler.SepiUnit;

    // Compile and write compiled unit to destination stream
    try
      DestFile := TFileStream.Create(DestFileName, fmCreate);
      Compiler.WriteToStream(DestFile);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenDestFile, [DestFileName]),
          ekFatalError);
      on Error: ESepiCompilerFatalError do
        raise;
      on Error: Exception do
        Errors.MakeError(Error.Message, ekFatalError);
    end;
  finally
    RootNode.Free;
    DestFile.Free;
  end;
end;

end.

