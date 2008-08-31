{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------}

{*
  Instructions de haut niveau Sepi
  @author sjrd
  @version 1.0
*}
unit SepiInstructions;

interface

uses
  SysUtils, TypInfo, SepiOrdTypes, SepiOpCodes, SepiCompiler,
  SepiAsmInstructions, SepiExpressions, SepiCompilerErrors, SepiCompilerConsts;

type
  {*
    Instruction if..then..else
    @author sjrd
    @version 1.0
  *}
  TSepiIfThenElse = class(TSepiInstruction)
  private
    FTestValue: ISepiReadableValue;           /// Valeur de test du if
    FTrueInstructions: TSepiInstructionList;  /// Instructions si True
    FFalseInstructions: TSepiInstructionList; /// Instructions si False
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    property TestValue: ISepiReadableValue read FTestValue write FTestValue;
    property TrueInstructions: TSepiInstructionList read FTrueInstructions;
    property FalseInstructions: TSepiInstructionList read FFalseInstructions;
  end;

  {*
    Instruction while..do ou do..while
    Attention, la version do..while de cette instruction n'est pas un
    repeat..until : la boucle est toujours effectuée tant que la condition du
    test est *vraie*. Si vous avez besoin d'un repeat..until, encapsulez la
    condition du test dans une opération unaire not.
    @author sjrd
    @version 1.0
  *}
  TSepiWhile = class(TSepiInstruction)
  private
    /// Si True, évalue la condition en fin de boucle (do..while)
    FTestAtEnd: Boolean;

    FTestValue: ISepiReadableValue;          /// Valeur de test du while
    FLoopInstructions: TSepiInstructionList; /// Instructions dans la boucle
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      ATestAtEnd: Boolean = False);

    property TestAtEnd: Boolean read FTestAtEnd;

    property TestValue: ISepiReadableValue read FTestValue write FTestValue;
    property LoopInstructions: TSepiInstructionList read FLoopInstructions;
  end;

  {*
    Instruction for..do
    Il s'agit bien ici d'une instruction for..do à la Delphi. Avec une variable,
    une borne de départ et d'arrivée, qui sont évaluées une seule fois avant la
    boucle. Pour compiler des boucles for à la C, utilisez plutôt TSepiWhile.
    @author sjrd
    @version 1.0
  *}
  TSepiFor = class(TSepiInstruction)
  private
    FControlVar: TSepiLocalVar;      /// Variable de contrôle
    FStartValue: ISepiReadableValue; /// Valeur de départ du for
    FEndValue: ISepiReadableValue;   /// Valeur de fin du for
    FIsDownTo: Boolean;              /// True pour un downto
    FAutoConvert: Boolean;           /// Autorise les conversions automatiques

    FLoopInstructions: TSepiInstructionList; /// Instructions dans la boucle

    function CheckValues: Boolean;
    procedure CompileReadBounds(out HigherBoundVar: TSepiLocalVar);
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AAutoConvert: Boolean = True);

    property ControlVar: TSepiLocalVar read FControlVar write FControlVar;
    property StartValue: ISepiReadableValue read FStartValue write FStartValue;
    property EndValue: ISepiReadableValue read FEndValue write FEndValue;
    property IsDownTo: Boolean read FIsDownTo write FIsDownTo;
    property AutoConvert: Boolean read FAutoConvert write FAutoConvert;

    property LoopInstructions: TSepiInstructionList read FLoopInstructions;
  end;

  {*
    Instruction try..except
    @author sjrd
    @version 1.0
  *}
  TSepiTryExcept = class(TSepiInstruction)
  private
    FAsmInstruction: TSepiAsmTryExcept;        /// Instruction TRYE
    FTryInstructions: TSepiInstructionList;    /// Instructions dans le try
    FExceptInstructions: TSepiInstructionList; /// Instructions dans le except

    function GetExceptObject: TSepiMemoryReference;
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    property TryInstructions: TSepiInstructionList read FTryInstructions;
    property ExceptInstructions: TSepiInstructionList read FExceptInstructions;
    property ExceptObject: TSepiMemoryReference read GetExceptObject;
  end;

  {*
    Instruction try..finally
    @author sjrd
    @version 1.0
  *}
  TSepiTryFinally = class(TSepiInstruction)
  private
    FAsmInstruction: TSepiAsmTryFinally;        /// Instruction TRYF
    FTryInstructions: TSepiInstructionList;     /// Instructions dans le try
    FFinallyInstructions: TSepiInstructionList; /// Instructions dans le finally
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    property TryInstructions: TSepiInstructionList read FTryInstructions;
    property FinallyInstructions: TSepiInstructionList
      read FFinallyInstructions;
  end;

procedure CompileTestValue(Compiler: TSepiMethodCompiler;
  TestValue: ISepiReadableValue; var Destination: TSepiMemoryReference;
  out TestIsConst, TestConstValue: Boolean);

implementation

{*
  Compile le test d'une valeur booléenne
  @param Compiler         Compilateur de méthode
  @param TestValue        Valeur du test
  @param Destination      En entrée et/ou sortie : référence mémoire du résultat
  @param TestIsConst      En sortie : True si le teste est une constante
  @param TestConstValue   En sortie, si TestIsConst : valeur constante du test
*}
procedure CompileTestValue(Compiler: TSepiMethodCompiler;
  TestValue: ISepiReadableValue; var Destination: TSepiMemoryReference;
  out TestIsConst, TestConstValue: Boolean);
var
  TestInstructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager;
begin
  if (TestValue.ValueType is TSepiBooleanType) and
    (TSepiBooleanType(TestValue.ValueType).BooleanKind = bkBoolean) then
  begin
    TestIsConst := TestValue.IsConstant;

    if TestIsConst then
    begin
      TestConstValue := Boolean(TestValue.ConstValuePtr^);
      (TestValue as ISepiExpression).MakeError(Format(STestValueIsAlways,
        [BooleanIdents[TestConstValue]]), ekWarning);
    end else
    begin
      TestConstValue := False;
      TempVars := TSepiTempVarsLifeManager.Create;
      try
        TestInstructions := TSepiInstructionList.Create(Compiler);
        try
          TestValue.CompileRead(Compiler, TestInstructions, Destination,
            TempVars);
          TestInstructions.Compile;
        finally
          TempVars.EndAllLifes(TestInstructions.GetCurrentEndRef);
        end;
      finally
        TempVars.Free;
      end;
    end;
  end else
  begin
    (TestValue as ISepiExpression).MakeError(Format(STypeMismatch,
      [PTypeInfo(TypeInfo(Boolean)).Name, TestValue.ValueType.Name]));
    TestIsConst := False;
    TestConstValue := True;

    if Destination = nil then
    begin
      Destination := TSepiMemoryReference.Create(Compiler,
        aoAcceptAllConsts, 1);
      Destination.SetSpace(msConstant);
      Destination.SetConstant(TestConstValue);
      Destination.Seal;
    end;
  end;
end;

{-----------------------}
{ TSepiIfThenElse class }
{-----------------------}

{*
  Crée une instruction if..then.else
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiIfThenElse.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FTrueInstructions := TSepiInstructionList.Create(MethodCompiler);
  FFalseInstructions := TSepiInstructionList.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIfThenElse.CustomCompile;
var
  TestMemory: TSepiMemoryReference;
  TestIsConst, TestConstValue: Boolean;
  TestJumpInstr: TSepiAsmCondJump;
  JumpInstr: TSepiAsmJump;
begin
  TestMemory := nil;
  try
    // Test the value
    CompileTestValue(MethodCompiler, TestValue, TestMemory,
      TestIsConst, TestConstValue);

    // Compile instructions
    if TestIsConst then
    begin
      // TODO In if Constant then..else, compile other instruction list anyway
      if TestConstValue then
        TrueInstructions.Compile
      else
        FalseInstructions.Compile;
    end else
    begin
      TestJumpInstr := TSepiAsmCondJump.Create(MethodCompiler, False);
      TestJumpInstr.SourcePos := SourcePos;
      TestJumpInstr.Test.Assign(TestMemory);

      if TrueInstructions.Count = 0 then
      begin
        if FalseInstructions.Count > 0 then
        begin
          TestJumpInstr.IfTrue := True;
          TestJumpInstr.Destination.InstructionRef :=
            FalseInstructions.AfterRef;
          TestJumpInstr.Compile;
          TrueInstructions.Compile;
          FalseInstructions.Compile;
        end;
      end else
      begin
        TestJumpInstr.Destination.InstructionRef :=
          FalseInstructions.BeforeRef;
        TestJumpInstr.Compile;
        TrueInstructions.Compile;

        if FalseInstructions.Count > 0 then
        begin
          JumpInstr := TSepiAsmJump.Create(MethodCompiler);
          JumpInstr.SourcePos := SourcePos;
          JumpInstr.Destination.InstructionRef := FalseInstructions.AfterRef;
          JumpInstr.Compile;
        end;

        FalseInstructions.Compile;
      end;
    end;
  finally
    TestMemory.Free;
  end;
end;

{------------------}
{ TSepiWhile class }
{------------------}

{*
  Crée une instruction while..do ou do..while
  @param AMethodCompiler   Compilateur de méthode
  @param ATestAtEnd        True pour avoir le test à la fin (do..while)
*}
constructor TSepiWhile.Create(AMethodCompiler: TSepiMethodCompiler;
  ATestAtEnd: Boolean = False);
begin
  inherited Create(AMethodCompiler);

  FTestAtEnd := ATestAtEnd;

  FLoopInstructions := TSepiInstructionList.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TSepiWhile.CustomCompile;
var
  TestMemory: TSepiMemoryReference;
  TestIsConst, TestConstValue: Boolean;
  TestJumpInstr: TSepiAsmCondJump;
  JumpInstr: TSepiAsmJump;
begin
  TestMemory := nil;
  try
    // If test is at beginning, that's now
    if not TestAtEnd then
    begin
      CompileTestValue(MethodCompiler, TestValue, TestMemory,
        TestIsConst, TestConstValue);

      if TestIsConst and (not TestConstValue) then
        Exit; // TODO Should still compile instructions, but not really

      if not TestIsConst then
      begin
        TestJumpInstr := TSepiAsmCondJump.Create(MethodCompiler, False);
        TestJumpInstr.SourcePos := SourcePos;
        TestJumpInstr.Destination.InstructionRef := AfterRef;
        TestJumpInstr.Test.Assign(TestMemory);
        TestJumpInstr.Compile;
      end;
    end;

    // Compile loop instructions
    LoopInstructions.Compile;

    // If test was at beginning, go back
    if not TestAtEnd then
    begin
      JumpInstr := TSepiAsmJump.Create(MethodCompiler);
      JumpInstr.SourcePos := SourcePos;
      JumpInstr.Destination.InstructionRef := BeforeRef;
      JumpInstr.Compile;
    end else
    begin
      // Otherwise, test is at end: well, that's now
      CompileTestValue(MethodCompiler, TestValue, TestMemory,
        TestIsConst, TestConstValue);

      if TestIsConst and (not TestConstValue) then
        Exit;

      if TestIsConst then
      begin
        // Always repeat
        JumpInstr := TSepiAsmJump.Create(MethodCompiler);
        JumpInstr.SourcePos := SourcePos;
        JumpInstr.Destination.InstructionRef := BeforeRef;
        JumpInstr.Compile;
      end else
      begin
        // Go back to beginning if value is True
        TestJumpInstr := TSepiAsmCondJump.Create(MethodCompiler, True);
        TestJumpInstr.SourcePos := SourcePos;
        TestJumpInstr.Destination.InstructionRef := BeforeRef;
        TestJumpInstr.Test.Assign(TestMemory);
        TestJumpInstr.Compile;
      end;
    end;
  finally
    TestMemory.Free;
  end;
end;

{----------------}
{ TSepiFor class }
{----------------}

{*
  Crée une instruction for..do
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiFor.Create(AMethodCompiler: TSepiMethodCompiler;
  AAutoConvert: Boolean = True);
begin
  inherited Create(AMethodCompiler);

  FAutoConvert := AAutoConvert;
  FLoopInstructions := TSepiInstructionList.Create(MethodCompiler);
end;

{*
  Vérifie les valeurs
*}
function TSepiFor.CheckValues: Boolean;
begin
  Result := True;

  if (not (ControlVar.VarType is TSepiOrdType)) and
    (not (ControlVar.VarType is TSepiInt64Type)) then
  begin
    MakeError(SOrdinalTypeRequired);
    Result := False;
  end else
  begin
    if AutoConvert then
    begin
      if StartValue.ValueType <> ControlVar.VarType then
        StartValue := TSepiConvertOperation.ConvertValue(
          ControlVar.VarType, StartValue);

      if EndValue.ValueType <> ControlVar.VarType then
        EndValue := TSepiConvertOperation.ConvertValue(
          ControlVar.VarType, EndValue);
    end else
    begin
      if StartValue.ValueType <> ControlVar.VarType then
      begin
        (StartValue as ISepiExpression).MakeError(Format(STypeMismatch,
          [ControlVar.VarType.Name, StartValue.ValueType.Name]));
        Result := False;
      end;

      if EndValue.ValueType <> ControlVar.VarType then
      begin
        (EndValue as ISepiExpression).MakeError(Format(STypeMismatch,
          [ControlVar.VarType.Name, EndValue.ValueType.Name]));
        Result := False;
      end;
    end;
  end;
end;

{*
  Compile les instructions de lecture des bornes
  @param HigherBoundVar   En sortie : variable locale de borne supérieure
*}
procedure TSepiFor.CompileReadBounds(out HigherBoundVar: TSepiLocalVar);
var
  ReadBoundsInstructions: TSepiInstructionList;
begin
  ReadBoundsInstructions := TSepiInstructionList.Create(MethodCompiler);

  HigherBoundVar := MethodCompiler.Locals.AddTempVar(ControlVar.VarType);
  HigherBoundVar.HandleLife;
  HigherBoundVar.Life.AddInstrInterval(
    ReadBoundsInstructions.AfterRef, AfterRef);

  // Read and store start value to control var
  (TSepiLocalVarValue.MakeValue(MethodCompiler,
    ControlVar) as ISepiWritableValue).CompileWrite(MethodCompiler,
    ReadBoundsInstructions, StartValue);

  // Read and store end value to higher bound temp var
  (TSepiLocalVarValue.MakeValue(MethodCompiler,
    HigherBoundVar) as ISepiWritableValue).CompileWrite(MethodCompiler,
    ReadBoundsInstructions, EndValue);

  ReadBoundsInstructions.Compile;
end;

{*
  [@inheritDoc]
*}
procedure TSepiFor.CustomCompile;
var
  HigherBoundVar, TestResultVar: TSepiLocalVar;
  BaseType: TSepiBaseType;
  CompareInstr: TSepiAsmCompare;
  TestJumpInstr: TSepiAsmCondJump;
  IncDecInstr: TSepiAsmOperation;
  JumpInstr: TSepiAsmJump;
begin
  if ControlVar.IsLifeHandled then
    ControlVar.Life.AddInstrInterval(BeforeRef, AfterRef);

  // Check values
  if not CheckValues then
  begin
    LoopInstructions.Compile;
    Exit;
  end;

  // Identify base type that will be used for comparison and inc/dec
  if not SepiTypeToBaseType(ControlVar.VarType, BaseType) then
  begin
    case ControlVar.VarType.Size of
      1: BaseType := btByte;
      2: BaseType := btWord;
    else
      BaseType := btDWord;
    end;
  end;

  CompileReadBounds(HigherBoundVar);

  TestResultVar := MethodCompiler.Locals.AddTempVar(
    MethodCompiler.SepiMethod.Root.FindType(TypeInfo(Boolean)));
  TestResultVar.HandleLife;

  // Create ASM instructions
  TestJumpInstr := TSepiAsmCondJump.Create(MethodCompiler, True);
  JumpInstr := TSepiAsmJump.Create(MethodCompiler);
  if IsDownTo then
  begin
    CompareInstr := TSepiAsmCompare.Create(MethodCompiler,
      ocCompLower, BaseType);
    IncDecInstr := TSepiAsmOperation.Create(MethodCompiler,
      ocSelfDec, BaseType);
  end else
  begin
    CompareInstr := TSepiAsmCompare.Create(MethodCompiler,
      ocCompGreater, BaseType);
    IncDecInstr := TSepiAsmOperation.Create(MethodCompiler,
      ocSelfInc, BaseType);
  end;

  // Compile loop

  CompareInstr.Destination.SetSpace(TestResultVar);
  CompareInstr.Left.SetSpace(ControlVar);
  CompareInstr.Right.SetSpace(HigherBoundVar);
  CompareInstr.Compile;

  TestJumpInstr.Destination.InstructionRef := AfterRef;
  TestJumpInstr.Test.SetSpace(TestResultVar);
  TestJumpInstr.Compile;

  LoopInstructions.Compile;

  IncDecInstr.Destination.SetSpace(ControlVar);
  IncDecInstr.Compile;

  JumpInstr.Destination.InstructionRef := CompareInstr.BeforeRef;
  JumpInstr.Compile;
end;

{----------------------}
{ TSepiTryExcept class }
{----------------------}

{*
  Crée une instruction try..except
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiTryExcept.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FAsmInstruction := TSepiAsmTryExcept.Create(MethodCompiler);
  FTryInstructions := TSepiInstructionList.Create(MethodCompiler);
  FExceptInstructions := TSepiInstructionList.Create(MethodCompiler);

  FAsmInstruction.EndOfTry.InstructionRef := TryInstructions.AfterRef;
  FAsmInstruction.EndOfExcept.InstructionRef := ExceptInstructions.AfterRef;
end;

{*
  Objet exception
  @return Référence mémoire où stocker l'objet exception
*}
function TSepiTryExcept.GetExceptObject: TSepiMemoryReference;
begin
  Result := FAsmInstruction.ExceptObject;
end;

{*
  [@inheritDoc]
*}
procedure TSepiTryExcept.CustomCompile;
begin
  FAsmInstruction.Compile;
  FTryInstructions.Compile;
  FExceptInstructions.Compile;
end;

{-----------------------}
{ TSepiTryFinally class }
{-----------------------}

{*
  Crée une instruction try..finally
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiTryFinally.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FAsmInstruction := TSepiAsmTryFinally.Create(MethodCompiler);
  FTryInstructions := TSepiInstructionList.Create(MethodCompiler);
  FFinallyInstructions := TSepiInstructionList.Create(MethodCompiler);

  FAsmInstruction.EndOfTry.InstructionRef := TryInstructions.AfterRef;
  FAsmInstruction.EndOfFinally.InstructionRef := FinallyInstructions.AfterRef;
end;

{*
  [@inheritDoc]
*}
procedure TSepiTryFinally.CustomCompile;
begin
  FAsmInstruction.Compile;
  FTryInstructions.Compile;
  FFinallyInstructions.Compile;
end;

end.

