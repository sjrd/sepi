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
  SysUtils, Contnrs, TypInfo, ScUtils, SepiOrdTypes, SepiStrTypes, SepiMembers,
  SepiOpCodes, SepiCompiler, SepiAsmInstructions, SepiExpressions,
  SepiCompilerErrors, SepiCompilerConsts;

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
    Clause d'un case of
    @author sjrd
    @version 1.0
  *}
  TSepiCaseOfClause = class(TObject)
  private
    FValue: ISepiReadableValue;          /// Valeur associée à cette clause
    FInstructions: TSepiInstructionList; /// Instructions de cette clause
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    property Value: ISepiReadableValue read FValue write FValue;
    property Instructions: TSepiInstructionList read FInstructions;
  end;

  {*
    Instruction case of
    @author sjrd
    @version 1.0
  *}
  TSepiCaseOf = class(TSepiInstruction)
  private
    FTestValue: ISepiReadableValue;          /// Valeur à tester
    FClauses: TObjectList;                   /// Clauses de test
    FElseInstructions: TSepiInstructionList; /// Instructions dans le else

    procedure ReadTestValue(out TestValueVar: TSepiLocalVar);

    function GetClauseCount: Integer;
    function GetClauses(Index: Integer): TSepiCaseOfClause;
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    function AddClause: TSepiCaseOfClause;

    property TestValue: ISepiReadableValue read FTestValue write FTestValue;
    property ClauseCount: Integer read GetClauseCount;
    property Clauses[Index: Integer]: TSepiCaseOfClause read GetClauses;
    property ElseInstructions: TSepiInstructionList read FElseInstructions;
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

    function UseTempVar(AType: TSepiOrdType): TSepiLocalVar;

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
    FTryInstructions: TSepiInstructionList;    /// Instructions dans le try
    FExceptInstructions: TSepiInstructionList; /// Instructions dans le except

    FExceptObjectVar: TSepiLocalVar; /// Variable qui stockera l'objet exception
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    function UseTempVar: TSepiLocalVar;

    property TryInstructions: TSepiInstructionList read FTryInstructions;
    property ExceptInstructions: TSepiInstructionList read FExceptInstructions;
    property ExceptObjectVar: TSepiLocalVar
      read FExceptObjectVar write FExceptObjectVar;
  end;

  {*
    Instruction try..finally
    @author sjrd
    @version 1.0
  *}
  TSepiTryFinally = class(TSepiInstruction)
  private
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

  {*
    Instruction d'assignation :=
    @author sjrd
    @version 1.0
  *}
  TSepiAssignment = class(TSepiInstruction)
  private
    FDestination: ISepiWritableValue; /// Destination de l'assignation
    FSource: ISepiReadableValue;      /// Source de l'assignation

    FAutoConvert: Boolean; /// Autorise les conversions automatiques
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AAutoConvert: Boolean = True);

    property Destination: ISepiWritableValue
      read FDestination write FDestination;
    property Source: ISepiReadableValue read FSource write FSource;

    property AutoConvert: Boolean read FAutoConvert write FAutoConvert;
  end;

  {*
    Instruction d'exécution d'une expression (exécutable)
    @author sjrd
    @version 1.0
  *}
  TSepiExecuteExpression = class(TSepiInstruction)
  private
    FExecutable: ISepiExecutable; /// Invocable
  protected
    procedure CustomCompile; override;
  public
    property Executable: ISepiExecutable read FExecutable write FExecutable;
  end;

  {*
    Instruction raise
    @author sjrd
    @version 1.0
  *}
  TSepiRaise = class(TSepiInstruction)
  private
    FExceptionValue: ISepiReadableValue; /// Valeur exception
  protected
    procedure CustomCompile; override;
  public
    property ExceptionValue: ISepiReadableValue
      read FExceptionValue write FExceptionValue;
  end;

  {*
    Instruction reraise
    @author sjrd
    @version 1.0
  *}
  TSepiReraise = class(TSepiInstruction)
  protected
    procedure CustomCompile; override;
  end;

  {*
    Instruction multi-on
    @author sjrd
    @version 1.0
  *}
  TSepiMultiOn = class(TSepiInstruction)
  private
    FExceptObjectVar: TSepiLocalVar;         /// Variable de l'objet exception
    FOnClauses: TObjectList;                 /// Clauses on
    FElseInstructions: TSepiInstructionList; /// Instructions dans le else
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    function AddOnClause(AExceptionClass: TSepiClass): TSepiInstructionList;

    property ExceptObjectVar: TSepiLocalVar
      read FExceptObjectVar write FExceptObjectVar;
    property ElseInstructions: TSepiInstructionList read FElseInstructions;
  end;

  {*
    Classe de base pour les instructions de jump spécial
    @author sjrd
    @version 1.0
  *}
  TSepiSpecialJump = class(TSepiInstruction)
  protected
    procedure CompileJump(Destination: TSepiInstructionRef);
  end;

  {*
    Instruction continue
    @author sjrd
    @version 1.0
  *}
  TSepiContinue = class(TSepiSpecialJump)
  protected
    procedure CustomCompile; override;
  end;

  {*
    Instruction break
    @author sjrd
    @version 1.0
  *}
  TSepiBreak = class(TSepiSpecialJump)
  protected
    procedure CustomCompile; override;
  end;

  {*
    Instruction exit
    @author sjrd
    @version 1.0
  *}
  TSepiExit = class(TSepiSpecialJump)
  protected
    procedure CustomCompile; override;
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
      [Compiler.SystemUnit.Boolean.DisplayName,
      TestValue.ValueType.DisplayName]));
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

{-------------------------}
{ TSepiCaseOfClause class }
{-------------------------}

{*
  Crée une clause de case of
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiCaseOfClause.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create;

  FInstructions := TSepiInstructionList.Create(AMethodCompiler);
end;

{-------------------}
{ TSepiCaseOf class }
{-------------------}

{*
  Crée une instruction case of
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiCaseOf.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FClauses := TObjectList.Create;
  FElseInstructions := TSepiInstructionList.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiCaseOf.Destroy;
begin
  FClauses.Free;

  inherited;
end;

{*
  Lit la valeur de test
  @param TestValueVar   En sortie : variable stockant la valeur de test
*}
procedure TSepiCaseOf.ReadTestValue(out TestValueVar: TSepiLocalVar);
var
  ReadTestValueInstructions: TSepiInstructionList;
begin
  ReadTestValueInstructions := TSepiInstructionList.Create(MethodCompiler);

  // Configure life of test value temp var
  TestValueVar := MethodCompiler.Locals.AddTempVar(TestValue.ValueType);
  TestValueVar.HandleLife;
  TestValueVar.Life.AddInstrInterval(
    ReadTestValueInstructions.AfterRef, AfterRef);

  // Read and store test value
  (TSepiLocalVarValue.MakeValue(MethodCompiler,
    TestValueVar) as ISepiWritableValue).CompileWrite(MethodCompiler,
    ReadTestValueInstructions, TestValue);

  ReadTestValueInstructions.Compile;
end;

{*
  Nombre de clauses
  @return Nombre de clauses
*}
function TSepiCaseOf.GetClauseCount: Integer;
begin
  Result := FClauses.Count;
end;

{*
  Tableau zero-based des clauses
  @param Index   Index compris entre 0 inclus et ClauseCount exclus
  @return Clause à l'index spécifié
*}
function TSepiCaseOf.GetClauses(Index: Integer): TSepiCaseOfClause;
begin
  Result := TSepiCaseOfClause(FClauses[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCaseOf.CustomCompile;
var
  TestValueVar: TSepiLocalVar;
  TestValueVarValue, ClauseTestValue: ISepiReadableValue;
  AllInstructions, CurInstructions: TSepiInstructionList;
  I: Integer;
  Clause: TSepiCaseOfClause;
  IfInstr: TSepiIfThenElse;
begin
  // Ordinal type required
  if not (TestValue.ValueType is TSepiOrdType) then
  begin
    (TestValue as ISepiExpression).MakeError(SOrdinalTypeRequired);
    TestValue := TSepiErroneousValue.MakeReplacementValue(
      TestValue as ISepiExpression);
  end;

  // Read test value
  ReadTestValue(TestValueVar);
  TestValueVarValue := TSepiLocalVarValue.MakeValue(MethodCompiler,
    TestValueVar) as ISepiReadableValue;

  // Prepare instruction lists
  AllInstructions := TSepiInstructionList.Create(MethodCompiler);
  CurInstructions := AllInstructions;

  // Compile tests
  for I := 0 to ClauseCount-1 do
  begin
    Clause := Clauses[I];

    // Const expression required
    if not Clause.Value.IsConstant then
      (Clause.Value as ISepiExpression).MakeError(SConstExpressionRequired);

    // Make if test value
    if Clause.Value.ValueType is TSepiSetType then
    begin
      ClauseTestValue := TSepiInSetOperation.MakeOperation(
        TestValueVarValue, Clause.Value);
    end else
    begin
      ClauseTestValue := TSepiBinaryOperation.MakeOperation(opCmpEQ,
        TestValueVarValue, Clause.Value);
    end;

    // Make if instruction
    IfInstr := TSepiIfThenElse.Create(MethodCompiler);
    IfInstr.TestValue := ClauseTestValue;
    IfInstr.TrueInstructions.Add(Clause.Instructions);
    CurInstructions.Add(IfInstr);

    // Enter the else part of the if instruction
    CurInstructions := IfInstr.FalseInstructions;
  end;

  // Make else instructions
  CurInstructions.Add(ElseInstructions);

  // Compile all instructions
  AllInstructions.Compile;
end;

{*
  Ajoute une clause
  @return Clause ajoutée
*}
function TSepiCaseOf.AddClause: TSepiCaseOfClause;
begin
  Result := TSepiCaseOfClause.Create(MethodCompiler);
  FClauses.Add(Result);
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
    // First create the test-jump instr: we will need a reference to it
    TestJumpInstr := TSepiAsmCondJump.Create(MethodCompiler, TestAtEnd);

    // If test is at beginning, that's now
    if not TestAtEnd then
    begin
      CompileTestValue(MethodCompiler, TestValue, TestMemory,
        TestIsConst, TestConstValue);

      if TestIsConst and (not TestConstValue) then
        Exit; // TODO Should still compile instructions, but not really

      if not TestIsConst then
      begin
        TestJumpInstr.SourcePos := SourcePos;
        TestJumpInstr.Destination.InstructionRef := AfterRef;
        TestJumpInstr.Test.Assign(TestMemory);
        TestJumpInstr.Compile;
      end;
    end;

    // Compile loop instructions
    MethodCompiler.EnterLoop(TestJumpInstr.BeforeRef, Self.AfterRef);
    try
      LoopInstructions.Compile;
    finally
      MethodCompiler.LeaveLoop;
    end;

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
  @param AAutoConvert      Autorise les conversions automatiques (défaut = True)
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
          [ControlVar.VarType.DisplayName, StartValue.ValueType.DisplayName]));
        Result := False;
      end;

      if EndValue.ValueType <> ControlVar.VarType then
      begin
        (EndValue as ISepiExpression).MakeError(Format(STypeMismatch,
          [ControlVar.VarType.DisplayName, EndValue.ValueType.DisplayName]));
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

  TestResultVar := MethodCompiler.Locals.AddTempVar(
    MethodCompiler.SystemUnit.Boolean);
  TestResultVar.HandleLife;
  TestResultVar.Life.AddInstrInterval(CompareInstr.AfterRef,
    TestJumpInstr.BeforeRef);

  // Compile loop

  CompareInstr.Destination.SetSpace(TestResultVar);
  CompareInstr.Left.SetSpace(ControlVar);
  CompareInstr.Right.SetSpace(HigherBoundVar);
  CompareInstr.Compile;

  TestJumpInstr.Destination.InstructionRef := AfterRef;
  TestJumpInstr.Test.SetSpace(TestResultVar);
  TestJumpInstr.Compile;

  MethodCompiler.EnterLoop(IncDecInstr.BeforeRef, Self.AfterRef);
  try
    LoopInstructions.Compile;
  finally
    MethodCompiler.LeaveLoop;
  end;

  IncDecInstr.Destination.SetSpace(ControlVar);
  IncDecInstr.Compile;

  JumpInstr.Destination.InstructionRef := CompareInstr.BeforeRef;
  JumpInstr.Compile;
end;

{*
  Utilise une variable temporaire comme variable de contrôle
  @param AType   Type de la variable temporaire de contrôle
  @return Variable temporaire créée (= ControlVar)
*}
function TSepiFor.UseTempVar(AType: TSepiOrdType): TSepiLocalVar;
begin
  Result := MethodCompiler.Locals.AddTempVar(AType);
  Result.HandleLife;
  Result.Life.AddInstrInterval(BeforeRef, AfterRef);

  ControlVar := Result;
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

  FTryInstructions := TSepiInstructionList.Create(MethodCompiler);
  FExceptInstructions := TSepiInstructionList.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TSepiTryExcept.CustomCompile;
var
  TryExceptInstr: TSepiAsmTryExcept;
begin
  if (ExceptObjectVar <> nil) and
    (not (ExceptObjectVar.VarType is TSepiClass)) then
  begin
    MakeError(SClassTypeRequired);
    ExceptObjectVar := nil;
  end;

  TryExceptInstr := TSepiAsmTryExcept.Create(MethodCompiler);
  TryExceptInstr.EndOfTry.InstructionRef := TryInstructions.AfterRef;
  TryExceptInstr.EndOfExcept.InstructionRef := ExceptInstructions.AfterRef;

  if ExceptObjectVar <> nil then
    TryExceptInstr.ExceptObject.SetSpace(ExceptObjectVar);

  TryExceptInstr.Compile;
  TryInstructions.Compile;
  ExceptInstructions.Compile;
end;

{*
  Utilise une variable temporaire comme variable pour l'objet exception
  @return Variable temporaire créée (= ExceptObjectVar)
*}
function TSepiTryExcept.UseTempVar: TSepiLocalVar;
begin
  if ExceptObjectVar = nil then
  begin
    ExceptObjectVar := MethodCompiler.Locals.AddTempVar(
      MethodCompiler.SystemUnit.TObject);
    ExceptObjectVar.HandleLife;
    ExceptObjectVar.Life.AddInstrInterval(ExceptInstructions.BeforeRef,
      ExceptInstructions.AfterRef);
  end;

  Result := ExceptObjectVar;
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

  FTryInstructions := TSepiInstructionList.Create(MethodCompiler);
  FFinallyInstructions := TSepiInstructionList.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TSepiTryFinally.CustomCompile;
var
  TryFinallyInstr: TSepiAsmTryFinally;
begin
  TryFinallyInstr := TSepiAsmTryFinally.Create(MethodCompiler);
  TryFinallyInstr.EndOfTry.InstructionRef := TryInstructions.AfterRef;
  TryFinallyInstr.EndOfFinally.InstructionRef := FinallyInstructions.AfterRef;

  TryFinallyInstr.Compile;
  TryInstructions.Compile;
  FinallyInstructions.Compile;
end;

{-----------------------}
{ TSepiAssignment class }
{-----------------------}

{*
  Crée une instruction d'assignation :=
  @param AMethodCompiler   Compilateur de méthode
  @param AAutoConvert      Autorise les conversions automatiques (défaut = True)
*}
constructor TSepiAssignment.Create(AMethodCompiler: TSepiMethodCompiler;
  AAutoConvert: Boolean = True);
begin
  inherited Create(AMethodCompiler);

  FAutoConvert := AAutoConvert;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAssignment.CustomCompile;
var
  Instructions: TSepiInstructionList;
begin
  if not Source.ValueType.Equals(Destination.ValueType) then
  begin
    if AutoConvert then
    begin
      Source := TSepiConvertOperation.ConvertValue(
        Destination.ValueType, Source);
    end else
    begin
      (Source as ISepiExpression).MakeError(Format(STypeMismatch,
        [Destination.ValueType.DisplayName, Source.ValueType.DisplayName]));
      Exit;
    end;
  end;

  Instructions := TSepiInstructionList.Create(MethodCompiler);
  Destination.CompileWrite(MethodCompiler, Instructions, Source);
  Instructions.Compile;
end;

{------------------------------}
{ TSepiExecuteExpression class }
{------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiExecuteExpression.CustomCompile;
var
  Instructions: TSepiInstructionList;
begin
  Instructions := TSepiInstructionList.Create(MethodCompiler);
  Executable.CompileExecute(MethodCompiler, Instructions);
  Instructions.Compile;
end;

{------------------}
{ TSepiRaise class }
{------------------}

{*
  [@inheritDoc]
*}
procedure TSepiRaise.CustomCompile;
var
  Instructions: TSepiInstructionList;
  ExceptionMemory: TSepiMemoryReference;
  RaiseInstr: TSepiAsmRaise;
  TempVars: TSepiTempVarsLifeManager;
begin
  if not (ExceptionValue.ValueType is TSepiClass) then
  begin
    (ExceptionValue as ISepiExpression).MakeError(SClassTypeRequired);
    Exit;
  end;

  Instructions := TSepiInstructionList.Create(MethodCompiler);

  RaiseInstr := TSepiAsmRaise.Create(MethodCompiler);

  ExceptionMemory := nil;
  TempVars := TSepiTempVarsLifeManager.Create;
  try
    ExceptionValue.CompileRead(MethodCompiler, Instructions, ExceptionMemory,
      TempVars);

    RaiseInstr.ExceptObject.Assign(ExceptionMemory);
  finally
    TempVars.EndAllLifes(Instructions.AfterRef);
    TempVars.Free;

    ExceptionMemory.Free;
  end;

  Instructions.Compile;
  RaiseInstr.Compile;
end;

{--------------------}
{ TSepiReraise class }
{--------------------}

{*
  @author sjrd
  @version 1.0
*}
procedure TSepiReraise.CustomCompile;
begin
  TSepiAsmReraise.Create(MethodCompiler).Compile;
end;

{--------------------}
{ TSepiMultiOn class }
{--------------------}

{*
  Crée une instruction multi-on
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiMultiOn.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOnClauses := TObjectList.Create(False);
  FElseInstructions := TSepiInstructionList.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiMultiOn.Destroy;
begin
  FOnClauses.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMultiOn.CustomCompile;
var
  MultiOnInstr: TSepiAsmMultiOn;
  I, LastNonEmptyOne: Integer;
  ExceptionClass: TSepiClass;
  Instructions: TSepiInstructionList;
begin
  MultiOnInstr := TSepiAsmMultiOn.Create(MethodCompiler);
  MultiOnInstr.SourcePos := SourcePos;
  MultiOnInstr.ExceptObject.SetSpace(ExceptObjectVar);

  LastNonEmptyOne := -1;

  for I := 0 to FOnClauses.Count div 2 - 1 do
  begin
    ExceptionClass := TSepiClass(FOnClauses[2*I]);
    Instructions := TSepiInstructionList(FOnClauses[2*I+1]);

    if Instructions.Count > 0 then
      LastNonEmptyOne := I;

    with MultiOnInstr.AddOnClause(ExceptionClass) do
    begin
      if Instructions.Count = 0 then
        InstructionRef := AfterRef
      else
        InstructionRef := Instructions.BeforeRef;
    end;
  end;

  MultiOnInstr.Compile;

  ElseInstructions.Compile;
  if LastNonEmptyOne >= 0 then
  begin
    with TSepiAsmJump.Create(MethodCompiler) do
    begin
      Destination.InstructionRef := Self.AfterRef;
      Compile;
    end;
  end;

  for I := 0 to FOnClauses.Count div 2 - 1 do
  begin
    Instructions := TSepiInstructionList(FOnClauses[2*I+1]);
    Instructions.Compile;

    if (Instructions.Count > 0) and (I < LastNonEmptyOne) then
    begin
      with TSepiAsmJump.Create(MethodCompiler) do
      begin
        Destination.InstructionRef := Self.AfterRef;
        Compile;
      end;
    end;
  end;
end;

{*
  Ajoute une clause on
  @param AExceptionClass   Classe d'exception à tester
  @return Liste d'instructions pour cette classe d'exception
*}
function TSepiMultiOn.AddOnClause(
  AExceptionClass: TSepiClass): TSepiInstructionList;
begin
  Result := TSepiInstructionList.Create(MethodCompiler);
  FOnClauses.Add(AExceptionClass);
  FOnClauses.Add(Result);
end;

{------------------------}
{ TSepiSpecialJump class }
{------------------------}

{*
  Compile l'instruction jump
  @param Destination   Destination du jump
*}
procedure TSepiSpecialJump.CompileJump(Destination: TSepiInstructionRef);
var
  Instruction: TSepiAsmJump;
begin
  Instruction := TSepiAsmJump.Create(MethodCompiler);
  Instruction.Destination.InstructionRef := Destination;
  Instruction.Compile;
end;

{---------------------}
{ TSepiContinue class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TSepiContinue.CustomCompile;
begin
  if MethodCompiler.ContinueRef = nil then
    MakeError(SContinueAllowedOnlyInLoop)
  else
    CompileJump(MethodCompiler.ContinueRef);
end;

{------------------}
{ TSepiBreak class }
{------------------}

{*
  [@inheritDoc]
*}
procedure TSepiBreak.CustomCompile;
begin
  if MethodCompiler.BreakRef = nil then
    MakeError(SBreakAllowedOnlyInLoop)
  else
    CompileJump(MethodCompiler.BreakRef);
end;

{-----------------}
{ TSepiExit class }
{-----------------}

{*
  [@inheritDoc]
*}
procedure TSepiExit.CustomCompile;
begin
  CompileJump(MethodCompiler.Instructions.AfterRef);
end;

end.

