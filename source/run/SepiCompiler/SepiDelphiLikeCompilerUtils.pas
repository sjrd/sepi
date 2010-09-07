{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  Sébastien Doeraene
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

Linking this library statically or dynamically with other modules is making a
combined work based on this library.  Thus, the terms and conditions of the GNU
General Public License cover the whole combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.
-------------------------------------------------------------------------------}

{*
  Utilitaires pour les compilateurs Sepi de langages ressemblant à Delphi
  @author sjrd
  @version 1.0
*}
unit SepiDelphiLikeCompilerUtils;

interface

uses
  Windows, SysUtils, StrUtils, TypInfo, ScUtils, SepiReflectionCore,
  SepiOrdTypes, SepiStrTypes, SepiArrayTypes, SepiMembers, SepiSystemUnit,
  SepiCompiler, SepiExpressions, SepiInstructions, SepiCompilerConsts,
  SepiCompilerUtils;

type
  {*
    Type de saut spécial existant en Delphi
  *}
  TSepiSpecialJumpKind = (sjkContinue, sjkBreak, sjkExit);

  {*
    Pseudo-routine de test d'identificateur
    Ces pseudo-routines (en Delphi : Defined et Declared) sont utilisées par
    les instructions du pré-processuer $IF. Aucune implémentation de cette
    interface n'est disponible dans cette unité : les pré-processeurs les
    définiront selon leur mode de fonctionnement.
    @author sjrd
    @version 1.0
  *}
  ISepiIdentifierTestPseudoRoutine = interface(ISepiReadableValue)
    ['{3A4B9066-D284-4690-A4DF-9AF59BEEA79B}']

    function GetIdentifier: string;
    procedure SetIdentifier(const Value: string);

    procedure Complete;

    property Identifier: string read GetIdentifier write SetIdentifier;
  end;

  {*
    Pseudo-routine compilée en une valeur
    @author sjrd
    @version 1.0
  *}
  TSepiValueBackedPseudoRoutine = class(TSepiCustomWithParams,
    ISepiWantingParams, ISepiValue, ISepiReadableValue, ISepiWritableValue,
    ISepiAddressableValue)
  private
    FBackingValue: ISepiValue; /// Valeur implémentant la pseudo-routine
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetValueType: TSepiType;
    function GetAddressType: TSepiType;

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    procedure CompileWrite(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Source: ISepiReadableValue);

    procedure CompileLoadAddress(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property BackingValue: ISepiValue read FBackingValue write FBackingValue;
  end;

  {*
    Pseudo-routine avec exactement un paramètre, et compilée en une valeur
    @author sjrd
    @version 1.0
  *}
  TSepiOneParamValueBackedPseudoRoutine = class(TSepiValueBackedPseudoRoutine)
  private
    function GetOperand: ISepiExpression;
  protected
    procedure CompleteParams; override;

    property Operand: ISepiExpression read GetOperand;
  end;

  {*
    Pseudo-routine compilée en une expression exécutable
    @author sjrd
    @version 1.0
  *}
  TSepiExecutableBackedPseudoRoutine = class(TSepiCustomWithParams,
    ISepiWantingParams, ISepiExecutable)
  private
    FBackingExecutable: ISepiExecutable; /// Implémentation de la pseudo-routine
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);

    property BackingExecutable: ISepiExecutable
      read FBackingExecutable write FBackingExecutable;
  end;

  {*
    Option d'une opération sur un type
    - tooTypeOperand : accepte un opérande qui est un type
    - tooValueTypeOperand : accepte un opérande valeur pour en extraire le type
    - tooStrValueOperand : accepte un opérande chaîne
    - tooArrayValueOperand : accepte un opérande tableau
    - tooNilIfNoTypeInfo : pour TypeInfo, accepte un type sans RTTI -> nil
  *}
  TSepiTypeOperationOption = (
    tooTypeOperand, tooValueTypeOperand, tooStrValueOperand,
    tooArrayValueOperand, tooNilIfNoTypeInfo
  );

  /// Options d'une opération sur un type
  TSepiTypeOperationOptions = set of TSepiTypeOperationOption;

const
  /// Options des opérations sur les types utilisées en Delphi
  tooDelphiOptions = [tooTypeOperand..tooArrayValueOperand];

type
  {*
    Pseudo-routine d'opération sur un type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeOperationPseudoRoutine = class(TSepiOneParamValueBackedPseudoRoutine)
  private
    FKind: TSepiTypeOperation;           /// Type d'opération
    FOptions: TSepiTypeOperationOptions; /// Options
  protected
    function MakeTypeExpression(SepiType: TSepiType): ISepiTypeExpression;

    procedure CompleteTypeOperation(
      const TypeExpression: ISepiTypeExpression); virtual;
    procedure CompleteStrOperation(const StrValue: ISepiReadableValue); virtual;
    procedure CompleteArrayBound(const ArrayValue: ISepiReadableValue); virtual;

    procedure CompleteParams; override;
  public
    constructor Create(AKind: TSepiTypeOperation;
      AOptions: TSepiTypeOperationOptions = tooDelphiOptions);

    property Kind: TSepiTypeOperation read FKind;
    property Options: TSepiTypeOperationOptions read FOptions;
  end;

  {*
    Pseudo-routine de transtypage
    @author sjrd
    @version 1.0
  *}
  TSepiCastPseudoRoutine = class(TSepiOneParamValueBackedPseudoRoutine)
  private
    FCastOperator: TSepiCastOperator; /// Opération sous-jacente
  protected
    procedure CompleteParams; override;
  public
    constructor Create(DestType: TSepiType);
    constructor CreateOrd;
    constructor CreateChr;
  end;

  {*
    Pseudo-routine de transtypage ou de conversion
    Lorsque la conversion est possible, elle est utilisée. Sinon un transtypage
    est utilisé.
    @author sjrd
    @version 1.0
  *}
  TSepiCastOrConvertPseudoRoutine = class(TSepiOneParamValueBackedPseudoRoutine)
  private
    FDestType: TSepiType; /// Type de destination

    function IsConversionAllowedBeforeCast(DestType, SourceType: TSepiType;
      out MiddleType: TSepiType): Boolean;
    function IsConversionAllowedAfterCast(DestType, SourceType: TSepiType;
      out MiddleType: TSepiType): Boolean;
  protected
    procedure CompleteParams; override;
  public
    constructor Create(ADestType: TSepiType);

    property DestType: TSepiType read FDestType;
  end;

  {*
    Pseudo-routine SetLength
    @author sjrd
    @version 1.0
  *}
  TSepiSetLengthPseudoRoutine = class(TSepiExecutableBackedPseudoRoutine)
  protected
    procedure CompleteParams; override;
  end;

  {*
    Pseudo-routine Copy
    @author sjrd
    @version 1.0
  *}
  TSepiCopyPseudoRoutine = class(TSepiValueBackedPseudoRoutine)
  protected
    procedure CompleteParams; override;
  end;

  {*
    Pseudo-routine de jump spécial (Continue, Break ou Exit)
    @author sjrd
    @version 1.0
  *}
  TSepiSpecialJumpPseudoRoutine = class(TSepiCustomExpressionPart,
    ISepiExecutable)
  private
    FKind: TSepiSpecialJumpKind; /// Type de saut spécial
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);
  public
    constructor Create(AKind: TSepiSpecialJumpKind);

    property Kind: TSepiSpecialJumpKind read FKind;
  end;

  {*
    Règles sémantiques du langage Delphi
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiLanguageRules = class(TSepiLanguageRules)
  protected
    procedure AddComponentToExpression(const Expression: ISepiExpression;
      Component: TSepiComponent); virtual;
    function AddPseudoRoutineToExpression(const Expression: ISepiExpression;
      const Identifier: string): Boolean; virtual;

    function AddClassIntfMemberToExpression(const Expression: ISepiExpression;
      Context: TSepiComponent; const BaseValue: ISepiReadableValue;
      Member: TSepiMember): Boolean; virtual;

    function ClassIntfMemberSelection(const Expression: ISepiExpression;
      Context: TSepiComponent; const BaseValue: ISepiReadableValue;
      const FieldName: string): Boolean; virtual;
  public
    function ResolveIdent(Context: TSepiComponent;
      const Identifier: string): ISepiExpression; override;

    function ResolveIdentInMethod(Compiler: TSepiMethodCompiler;
      const Identifier: string): ISepiExpression; override;

    function FieldSelection(Context: TSepiComponent;
      const BaseExpression: ISepiExpression;
      const FieldName: string): ISepiExpression; override;
  end;

const
  /// Noms des opérations sur des types
  TypeOperationNames: array[TSepiTypeOperation] of string = (
    'SizeOf', 'TypeInfo', 'Low', 'High', 'Length'
  );

  /// Nom de la pseudo-routine Ord
  OrdName = 'Ord';

  /// Nom de la pseudo-routine Chr
  ChrName = 'Chr';

  /// Nom de la pseudo-routine SetLength
  SetLengthName = 'SetLength';

  /// Nom de la pseudo-routine Copy
  CopyName = 'Copy';

  /// Nom des pseudo-routines de jump spécial
  SpecialJumpNames: array[TSepiSpecialJumpKind] of string = (
    'Continue', 'Break', 'Exit'
  );

implementation

{*
  Ajoute un composant à une expression
  @param Expression   Expression
  @param Component    Composant (peut être nil)
*}
procedure TSepiDelphiLanguageRules.AddComponentToExpression(
  const Expression: ISepiExpression; Component: TSepiComponent);
var
  MetaClassValue: TSepiMetaClassValue;
begin
  if Component = nil then
    Exit;

  // Simply the component
  ISepiExpressionPart(
    TSepiComponentExpression.Create(Component)).AttachToExpression(Expression);

  if Component is TSepiType then
  begin
    // Type
    ISepiExpressionPart(TSepiTypeExpression.Create(
      TSepiType(Component))).AttachToExpression(Expression);

    if Component is TSepiClass then
    begin
      MetaClassValue := TSepiMetaClassValue.Create(TSepiClass(Component));
      ISepiExpressionPart(MetaClassValue).AttachToExpression(Expression);
      MetaClassValue.Complete;
    end;
  end else if Component is TSepiConstant then
  begin
    // Constant
    ISepiExpressionPart(TSepiTrueConstValue.Create(
      TSepiConstant(Component))).AttachToExpression(Expression);
  end else if Component is TSepiVariable then
  begin
    // Variable
    ISepiExpressionPart(TSepiVariableValue.Create(
      TSepiVariable(Component))).AttachToExpression(Expression);
  end else if Component is TSepiMethodBase then
  begin
    // Method
    ISepiExpressionPart(TSepiMethodCall.Create(
      TSepiMethod(Component))).AttachToExpression(Expression);
  end;
end;

{*
  Ajoute une pseudo-routine à une expression
  @param Expression   Expression
  @param Identifier   Identificateur de la pseudo-routine
  @return True si une pseudo-routine a été trouvée, False sinon
*}
function TSepiDelphiLanguageRules.AddPseudoRoutineToExpression(
  const Expression: ISepiExpression; const Identifier: string): Boolean;
var
  TypeOpIndex, SpecialJumpIndex: Integer;
begin
  Result := True;

  // Type operation pseudo-routine
  TypeOpIndex := AnsiIndexText(Identifier, TypeOperationNames);
  if TypeOpIndex >= 0 then
  begin
    ISepiExpressionPart(TSepiTypeOperationPseudoRoutine.Create(
      TSepiTypeOperation(TypeOpIndex))).AttachToExpression(Expression);
    Exit;
  end;

  // Ord pseudo-routine
  if AnsiSameText(Identifier, OrdName) then
  begin
    ISepiExpressionPart(TSepiCastPseudoRoutine.CreateOrd).AttachToExpression(
      Expression);
    Exit;
  end;

  // Chr pseudo-routine
  if AnsiSameText(Identifier, ChrName) then
  begin
    ISepiExpressionPart(TSepiCastPseudoRoutine.CreateChr).AttachToExpression(
      Expression);
    Exit;
  end;

  // SetLength pseudo-routine
  if AnsiSameText(Identifier, SetLengthName) then
  begin
    ISepiExpressionPart(TSepiSetLengthPseudoRoutine.Create).AttachToExpression(
      Expression);
    Exit;
  end;

  // Copy pseudo-routine
  if AnsiSameText(Identifier, CopyName) then
  begin
    ISepiExpressionPart(TSepiCopyPseudoRoutine.Create).AttachToExpression(
      Expression);
    Exit;
  end;

  // Special jump pseudo-routine
  SpecialJumpIndex := AnsiIndexText(Identifier, SpecialJumpNames);
  if SpecialJumpIndex >= 0 then
  begin
    ISepiExpressionPart(TSepiSpecialJumpPseudoRoutine.Create(
      TSepiSpecialJumpKind(SpecialJumpIndex))).AttachToExpression(Expression);
    Exit;
  end;

  Result := False;
end;

{*
  Ajoute un membre d'une classe ou d'une interface à une expression
  @param Expression   Expression
  @param BaseValue    Valeur classe ou interface de base
  @param Member       Membre à ajouter
  @return True si réussi, False sinon
*}
function TSepiDelphiLanguageRules.AddClassIntfMemberToExpression(
  const Expression: ISepiExpression; Context: TSepiComponent;
  const BaseValue: ISepiReadableValue; Member: TSepiMember): Boolean;
begin
  Result := False;

  if Member is TSepiField then
  begin
    // Field
    ISepiExpressionPart(TSepiObjectFieldValue.Create(
      BaseValue, TSepiField(Member))).AttachToExpression(Expression);

    Result := True;
  end else if Member is TSepiMethodBase then
  begin
    // Method
    ISepiExpressionPart(TSepiMethodCall.Create(TSepiMethodBase(Member),
      BaseValue)).AttachToExpression(Expression);

    Result := True;
  end else if Member is TSepiProperty then
  begin
    // Property
    Assert(Member is TSepiProperty);
    ISepiExpressionPart(TSepiPropertyValue.Create(BaseValue,
      TSepiProperty(Member))).AttachToExpression(Expression);

    Result := True;
  end;
end;

{*
  Sélection de champ d'une valeur classe, meta-classe ou interface
  @param Context       Contexte Sepi depuis lequel chercher
  @param BaseValue     Valeur de base
  @param FieldName     Nom du champ
  @param Expression    Expression destination
  @return True si réussi, False sinon
*}
function TSepiDelphiLanguageRules.ClassIntfMemberSelection(
  const Expression: ISepiExpression; Context: TSepiComponent;
  const BaseValue: ISepiReadableValue; const FieldName: string): Boolean;
const
  mkClassMethods = [mkClassProcedure, mkClassFunction];
  mkConstrClassMethods = [mkConstructor] + mkClassMethods;
var
  ContainerType: TSepiContainerType;
  Member: TSepiMember;
begin
  Result := False;

  // Fetch container type
  if BaseValue.ValueType is TSepiMetaClass then
    ContainerType := TSepiMetaClass(BaseValue.ValueType).SepiClass
  else
    ContainerType := BaseValue.ValueType as TSepiContainerType;

  // Fetch member
  Member := ContainerType.LookForMember(FieldName, Context);
  if Member = nil then
    Exit;

  // Add the member to the expression
  Result := AddClassIntfMemberToExpression(Expression, Context, BaseValue,
    Member);
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiLanguageRules.ResolveIdent(Context: TSepiComponent;
  const Identifier: string): ISepiExpression;
var
  Component: TSepiComponent;
begin
  Result := TSepiExpression.Create(UnitCompiler);

  // Component
  Component := Context.LookFor(Identifier);
  if Component <> nil then
  begin
    AddComponentToExpression(Result, Component);
    Exit;
  end;

  // Pseudo-routine
  if AddPseudoRoutineToExpression(Result, Identifier) then
    Exit;

  Result := nil;
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiLanguageRules.ResolveIdentInMethod(
  Compiler: TSepiMethodCompiler; const Identifier: string): ISepiExpression;
var
  LocalVar: TSepiLocalVar;
  ComponentOwner, Component: TSepiComponent;
  SelfExpression, FieldExpression: ISepiExpression;
begin
  Result := TSepiExpression.Create(Compiler);

  // Local variable
  LocalVar := Compiler.Locals.GetVarByName(Identifier);
  if LocalVar <> nil then
  begin
    ISepiExpressionPart(
      TSepiLocalVarValue.Create(LocalVar)).AttachToExpression(Result);
    Exit;
  end;

  // Component
  if Compiler.HasLocalNamespace then
    ComponentOwner := Compiler.LocalNamespace
  else
    ComponentOwner := Compiler.SepiMethod;
  Component := ComponentOwner.LookFor(Identifier);

  // Component in the method itself
  if (Component <> nil) and (Component.Owner = ComponentOwner) then
  begin
    AddComponentToExpression(Result, Component);
    Exit;
  end;

  // Field selection on Self
  if Compiler.SepiMethod.Signature.SelfParam <> nil then
  begin
    LocalVar := Compiler.Locals.SelfVar;
    SelfExpression := TSepiExpression.Create(Result);
    ISepiExpressionPart(
      TSepiLocalVarValue.Create(LocalVar)).AttachToExpression(SelfExpression);
    FieldExpression := FieldSelection(Compiler.SepiMethod,
      SelfExpression, Identifier);

    if FieldExpression <> nil then
    begin
      Result := FieldExpression;
      Exit;
    end;
  end;

  // Component out of the method itself
  if Component <> nil then
  begin
    AddComponentToExpression(Result, Component);
    Exit;
  end;

  // Pseudo-routine
  if AddPseudoRoutineToExpression(Result, Identifier) then
    Exit;

  Result := nil;
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiLanguageRules.FieldSelection(Context: TSepiComponent;
  const BaseExpression: ISepiExpression;
  const FieldName: string): ISepiExpression;
var
  CancelResult: Boolean;
  ComponentExpression: ISepiComponentExpression;
  Component: TSepiComponent;
  Value: ISepiValue;
  ReadableValue: ISepiReadableValue;
begin
  Result := TSepiExpression.Create(BaseExpression);
  CancelResult := True;

  // Component child
  if Supports(BaseExpression, ISepiComponentExpression,
    ComponentExpression) then
  begin
    Component := ComponentExpression.Component.GetComponent(FieldName);

    if Component <> nil then
    begin
      AddComponentToExpression(Result, Component);
      CancelResult := False;
    end;
  end;

  // Record field
  if Supports(BaseExpression, ISepiValue, Value) then
  begin
    if Value.ValueType is TSepiRecordType then
    begin
      Component := TSepiRecordType(Value.ValueType).GetComponent(FieldName);

      if Component is TSepiField then
      begin
        ISepiExpressionPart(TSepiRecordFieldValue.Create(
          Value, TSepiField(Component))).AttachToExpression(Result);
        CancelResult := False;
      end;
    end;
  end;

  // Class or interface method
  if Supports(BaseExpression, ISepiReadableValue, ReadableValue) then
  begin
    if (ReadableValue.ValueType is TSepiClass) or
      (ReadableValue.ValueType is TSepiMetaClass) or
      (ReadableValue.ValueType is TSepiInterface) then
    begin
      if ClassIntfMemberSelection(Result, Context, ReadableValue,
        FieldName) then
        CancelResult := False;
    end;
  end;

  if CancelResult then
    Result := nil;
end;

{-------------------------------------}
{ TSepiValueBackedPseudoRoutine class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
function TSepiValueBackedPseudoRoutine.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if SameGUID(IID, ISepiWantingParams) and ParamsCompleted then
    Result := E_NOINTERFACE
  else if (SameGUID(IID, ISepiValue) or SameGUID(IID, ISepiReadableValue) or
    SameGUID(IID, ISepiWritableValue) or
    SameGUID(IID, ISepiAddressableValue)) and
    (not Supports(BackingValue, IID)) then
    Result := E_NOINTERFACE
  else
    Result := inherited QueryInterface(IID, Obj);
end;

{*
  [@inheritDoc]
*}
procedure TSepiValueBackedPseudoRoutine.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  if not ParamsCompleted then
    Expression.Attach(ISepiWantingParams, AsExpressionPart);

  if BackingValue <> nil then
  begin
    Expression.Attach(ISepiValue, AsExpressionPart);

    if Supports(BackingValue, ISepiReadableValue) then
      Expression.Attach(ISepiReadableValue, AsExpressionPart);

    if Supports(BackingValue, ISepiWritableValue) then
      Expression.Attach(ISepiWritableValue, AsExpressionPart);

    if Supports(BackingValue, ISepiAddressableValue) then
      Expression.Attach(ISepiAddressableValue, AsExpressionPart);
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiValueBackedPseudoRoutine.GetValueType: TSepiType;
begin
  Result := BackingValue.ValueType;
end;

{*
  [@inheritDoc]
*}
function TSepiValueBackedPseudoRoutine.GetAddressType: TSepiType;
begin
  Result := (BackingValue as ISepiAddressableValue).AddressType;
end;

{*
  [@inheritDoc]
*}
function TSepiValueBackedPseudoRoutine.GetIsConstant: Boolean;
begin
  Result := (BackingValue as ISepiReadableValue).IsConstant;
end;

{*
  [@inheritDoc]
*}
function TSepiValueBackedPseudoRoutine.GetConstValuePtr: Pointer;
begin
  Result := (BackingValue as ISepiReadableValue).ConstValuePtr;
end;

{*
  [@inheritDoc]
*}
procedure TSepiValueBackedPseudoRoutine.CompileRead(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
begin
  (BackingValue as ISepiReadableValue).CompileRead(Compiler, Instructions,
    Destination, TempVars);
end;

{*
  [@inheritDoc]
*}
procedure TSepiValueBackedPseudoRoutine.CompileWrite(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  Source: ISepiReadableValue);
begin
  (BackingValue as ISepiWritableValue).CompileWrite(Compiler, Instructions,
    Source);
end;

{*
  [@inheritDoc]
*}
procedure TSepiValueBackedPseudoRoutine.CompileLoadAddress(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
begin
  (BackingValue as ISepiAddressableValue).CompileLoadAddress(Compiler,
    Instructions, Destination, TempVars);
end;

{---------------------------------------------}
{ TSepiOneParamValueBackedPseudoRoutine class }
{---------------------------------------------}

{*
  Opérande de la pseudo-routine
  @return Le premier paramètre s'il y en a, ou nil s'il n'y a aucun paramètre
*}
function TSepiOneParamValueBackedPseudoRoutine.GetOperand: ISepiExpression;
begin
  if ParamCount = 0 then
    Result := nil
  else
    Result := Params[0];
end;

{*
  [@inheritDoc]
*}
procedure TSepiOneParamValueBackedPseudoRoutine.CompleteParams;
begin
  inherited;

  if ParamCount < 1 then
    MakeError(SNotEnoughActualParameters)
  else if ParamCount > 1 then
    MakeError(STooManyActualParameters);
end;

{------------------------------------------}
{ TSepiExecutableBackedPseudoRoutine class }
{------------------------------------------}

{*
  [@inheritDoc]
*}
function TSepiExecutableBackedPseudoRoutine.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if SameGUID(IID, ISepiWantingParams) and ParamsCompleted then
    Result := E_NOINTERFACE
  else if SameGUID(IID, ISepiExecutable) and (not ParamsCompleted) then
    Result := E_NOINTERFACE
  else
    Result := inherited QueryInterface(IID, Obj);
end;

{*
  [@inheritDoc]
*}
procedure TSepiExecutableBackedPseudoRoutine.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  if not ParamsCompleted then
    Expression.Attach(ISepiWantingParams, AsExpressionPart);

  if BackingExecutable <> nil then
    Expression.Attach(ISepiExecutable, AsExpressionPart);
end;

{*
  [@inheritDoc]
*}
procedure TSepiExecutableBackedPseudoRoutine.CompileExecute(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList);
begin
  BackingExecutable.CompileExecute(Compiler, Instructions);
end;

{---------------------------------------}
{ TSepiTypeOperationPseudoRoutine class }
{---------------------------------------}

{*
  Crée une pseudo-routine opération sur un type
  @param AOperation         Opération
  @param ANilIfNoTypeInfo   Si True, TypeInfo peut renvoyer nil
*}
constructor TSepiTypeOperationPseudoRoutine.Create(AKind: TSepiTypeOperation;
  AOptions: TSepiTypeOperationOptions = tooDelphiOptions);
const
  OptionsWhomAtLeastOne = [
    tooTypeOperand, tooValueTypeOperand, tooStrValueOperand,
    tooArrayValueOperand
  ];
begin
  inherited Create;

  FKind := AKind;
  FOptions := AOptions;

  if Kind <> toLength then
    Exclude(FOptions, tooStrValueOperand);
  if not (Kind in [toLowerBound, toHigherBound, toLength]) then
    Exclude(FOptions, tooArrayValueOperand);

  Assert(FOptions * OptionsWhomAtLeastOne <> []);
end;

{*
  Construit une expression pour un type
  @param SepiType   Type
  @return Expression représentant le type
*}
function TSepiTypeOperationPseudoRoutine.MakeTypeExpression(
  SepiType: TSepiType): ISepiTypeExpression;
begin
  Result := TSepiTypeExpression.Create(SepiType);
  Result.AttachToExpression(TSepiExpression.Create(Expression));
end;

{*
  Complète une opération sur un type
  @param TypeExpression   Expression de type
*}
procedure TSepiTypeOperationPseudoRoutine.CompleteTypeOperation(
  const TypeExpression: ISepiTypeExpression);
begin
  BackingValue := TSepiTypeOperationValue.MakeTypeOperationValue(Kind,
    TypeExpression, tooNilIfNoTypeInfo in Options);
end;

{*
  Complète une opération sur une chaîne de caractères
  @param StrValue   Valeur chaîne
*}
procedure TSepiTypeOperationPseudoRoutine.CompleteStrOperation(
  const StrValue: ISepiReadableValue);
begin
  Assert(Kind = toLength);

  BackingValue := TSepiStringLengthValue.MakeStringLengthValue(StrValue);
end;

{*
  Complète une opération sur un tableau
  @param ArrayValue   Valeur tableau
*}
procedure TSepiTypeOperationPseudoRoutine.CompleteArrayBound(
  const ArrayValue: ISepiReadableValue);
const
  TypeOperationToArrayBound: array[toLowerBound..toLength] of
    TSepiArrayBoundKind = (abkLow, abkHigh, abkLength);
begin
  Assert(Kind in [toLowerBound, toHigherBound, toLength]);

  BackingValue := TSepiArrayBoundValue.MakeArrayBoundValue(
    TypeOperationToArrayBound[Kind], ArrayValue);
end;

{*
  [@inheritDoc]
*}
procedure TSepiTypeOperationPseudoRoutine.CompleteParams;
var
  TypeExpression: ISepiTypeExpression;
  Value: ISepiValue;
  ReadableValue: ISepiReadableValue;
  ErrorIndex: Byte;
begin
  inherited;

  if Operand <> nil then
  begin
    // Operand is a type expression
    if (tooTypeOperand in Options) and
      Supports(Operand, ISepiTypeExpression, TypeExpression) then
    begin
      CompleteTypeOperation(TypeExpression);
    end else

    // Operand is a string/array value
    if (Options * [tooStrValueOperand, tooArrayValueOperand] <> []) and
      Supports(Operand, ISepiReadableValue, ReadableValue) then
    begin
      // Operand is a string value
      if (tooStrValueOperand in Options) and
        (ReadableValue.ValueType is TSepiStringType) then
      begin
        CompleteStrOperation(ReadableValue);
      end else

      // Operand is an array value
      if (tooArrayValueOperand in Options) and
        (ReadableValue.ValueType is TSepiArrayType) then
      begin
        CompleteArrayBound(ReadableValue);
      end;
    end else

    // Operand is a value that we interpret as a type expression
    if (tooValueTypeOperand in Options) and
      Supports(Operand, ISepiValue, Value) then
    begin
      CompleteTypeOperation(MakeTypeExpression(Value.ValueType));
    end;
  end;

  // Error handling
  if BackingValue = nil then
  begin
    // Issue error message
    ErrorIndex := 0;
    if tooStrValueOperand in Options then
      Inc(ErrorIndex, 1);
    if tooArrayValueOperand in Options then
      Inc(ErrorIndex, 2);
    if Options * [tooTypeOperand, tooValueTypeOperand] <> [] then
      Inc(ErrorIndex, 4);

    case ErrorIndex of
      1: MakeError(SStringTypeRequired);
      2: MakeError(SArrayTypeRequired);
      3: MakeError(SStringOrArrayTypeRequired);
      4: MakeError(STypeIdentifierRequired);
      5: MakeError(SStringTypeOrTypeIdentifierRequired);
      6: MakeError(SArrayTypeOrTypeIdentifierRequired);
      7: MakeError(SStringOrArrayTypeOrTypeIdentifierRequired);
    end;

    // Make fake value
    FKind := toSizeOf;
    CompleteTypeOperation(MakeTypeExpression(UnitCompiler.SystemUnit.Integer));
  end;

  AttachToExpression(Expression);
end;

{------------------------------}
{ TSepiCastPseudoRoutine class }
{------------------------------}

{*
  Crée une pseudo-routine opérateur de transtypage
  @param ADestType   Type de destination
*}
constructor TSepiCastPseudoRoutine.Create(DestType: TSepiType);
begin
  inherited Create;

  FCastOperator := TSepiCastOperator.Create(DestType);
end;

{*
  Crée une pseudo-routine opérateur de transtypage Ord
*}
constructor TSepiCastPseudoRoutine.CreateOrd;
begin
  inherited Create;

  FCastOperator := TSepiCastOperator.CreateOrd;
end;

{*
  Crée une pseudo-routine opérateur de transtypage Chr
*}
constructor TSepiCastPseudoRoutine.CreateChr;
begin
  inherited Create;

  FCastOperator := TSepiCastOperator.CreateChr;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCastPseudoRoutine.CompleteParams;
var
  Param: ISepiExpression;
  Value: ISepiValue;
begin
  inherited;

  BackingValue := FCastOperator;
  BackingValue.AttachToExpression(TSepiExpression.Create(Expression));

  Param := Operand;
  if Param <> nil then
  begin
    if Supports(Param, ISepiValue, Value) then
      FCastOperator.Operand := Value
    else
      Param.MakeError(SValueRequired);
  end;

  if FCastOperator.Operand = nil then
  begin
    FCastOperator.Operand := TSepiErroneousValue.Create(SepiRoot);
    FCastOperator.Operand.AttachToExpression(
      TSepiExpression.Create(Expression));
  end;

  FCastOperator.Complete;

  AttachToExpression(Expression);
end;

{---------------------------------------}
{ TSepiCastOrConvertPseudoRoutine class }
{---------------------------------------}

{*
  Crée une pseudo-routine de transtypage ou conversion
  @param ADestType   Type de destination
*}
constructor TSepiCastOrConvertPseudoRoutine.Create(ADestType: TSepiType);
begin
  inherited Create;

  FDestType := ADestType;
end;

{*
  Teste si une conversion est autorisée avant de faire un cast
  @param DestType     Type destination
  @param SourceType   Type source
  @param MiddleType   En sortie : type transitoire pour la conversion
  @return True si la conversion est autorisée, False sinon
*}
function TSepiCastOrConvertPseudoRoutine.IsConversionAllowedBeforeCast(
  DestType, SourceType: TSepiType; out MiddleType: TSepiType): Boolean;
begin
  Result := IsConversionAllowedAfterCast(SourceType, DestType, MiddleType);
end;

{*
  Teste si une conversion est autorisée après avoir fait un cast
  @param DestType     Type destination
  @param SourceType   Type source
  @param MiddleType   En sortie : type transitoire pour la conversion
  @return True si la conversion est autorisée, False sinon
*}
function TSepiCastOrConvertPseudoRoutine.IsConversionAllowedAfterCast(
  DestType, SourceType: TSepiType; out MiddleType: TSepiType): Boolean;
begin
  Result := False;

  if (DestType is TSepiIntegerType) or (DestType is TSepiInt64Type) then
  begin
    if SourceType is TSepiOrdType then
    begin
      MiddleType := SystemUnit.GetSystemType(TSepiOrdType(SourceType).OrdType);
      Result := True;
    end else if (SourceType is TSepiPointerType) or
      (SourceType is TSepiClass) then
    begin
      MiddleType := SystemUnit.Cardinal;
      Result := True;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCastOrConvertPseudoRoutine.CompleteParams;
var
  Source: ISepiValue;
  ReadableSource: ISepiReadableValue;
  SourceType, MiddleType: TSepiType;
begin
  inherited;

  if not Supports(Operand, ISepiValue, Source) then
  begin
    // Complete failure because operand is not a value
    if Operand <> nil then
      Operand.MakeError(SValueRequired);

    BackingValue := TSepiErroneousValue.Create(DestType);
    BackingValue.AttachToExpression(TSepiExpression.Create(Expression));
  end else if Source.ValueType is TSepiUntypedType then
  begin
    // Give a type to an untyped variable
    BackingValue := TSepiCastOperator.CastValue(DestType, Source, True);
  end else
  begin
    // OK, we have a value source to work on
    if not Supports(Source, ISepiReadableValue, ReadableSource) then
      ReadableSource := nil;
    SourceType := Source.ValueType;

    if (ReadableSource <> nil) and
      TSepiConvertOperation.ConversionExists(DestType, ReadableSource) then
    begin
      // Convertion is possible
      BackingValue := TSepiConvertOperation.ConvertValue(
        DestType, ReadableSource);
    end else
    begin
      // No conversion possible : cast

      // Optional conversion in addition to the cast
      if (SourceType.Size <> DestType.Size) and (ReadableSource <> nil) then
      begin
        if IsConversionAllowedBeforeCast(DestType, SourceType, MiddleType) then
        begin
          // Convert then cast
          Source := TSepiConvertOperation.ConvertValue(
            MiddleType, ReadableSource);
          BackingValue := TSepiCastOperator.CastValue(DestType, Source);
        end else if IsConversionAllowedAfterCast(DestType, SourceType,
          MiddleType) then
        begin
          // Cast then convert
          Source := TSepiCastOperator.CastValue(MiddleType, Source);
          BackingValue := TSepiConvertOperation.ConvertValue(DestType,
            Source as ISepiReadableValue);
        end else
        begin
          // Just cast
          BackingValue := TSepiCastOperator.CastValue(DestType, Source);
        end;
      end else
      begin
        // Just cast
        BackingValue := TSepiCastOperator.CastValue(DestType, Source);
      end;
    end;
  end;

  AttachToExpression(Expression);
end;

{-----------------------------------}
{ TSepiSetLengthPseudoRoutine class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiSetLengthPseudoRoutine.CompleteParams;
var
  FirstValue: ISepiValue;
  Dimensions: array of ISepiReadableValue;
  I: Integer;
begin
  inherited;

  if ParamCount < 2 then
  begin
    MakeError(SNotEnoughActualParameters);
    Exit;
  end;

  if not Supports(Params[0], ISepiValue, FirstValue) then
  begin
    Params[0].MakeError(SStringOrDynArrayTypeRequired);
    Exit;
  end;

  SetLength(Dimensions, ParamCount-1);
  for I := 0 to ParamCount-2 do
    RequireReadableValue(Params[I+1], Dimensions[I]);

  if FirstValue.ValueType is TSepiStringType then
  begin
    if ParamCount > 2 then
      Params[2].MakeError(STooManyActualParameters);

    BackingExecutable := TSepiStrSetLengthExpression.MakeStrSetLengthExpression(
      FirstValue, Dimensions[0]);
  end else if FirstValue.ValueType is TSepiDynArrayType then
  begin
    BackingExecutable :=
      TSepiDynArraySetLengthExpression.MakeDynArraySetLengthExpression(
        FirstValue, Dimensions);
  end else
  begin
    Params[0].MakeError(SStringOrDynArrayTypeRequired);
  end;
end;

{------------------------------}
{ TSepiCopyPseudoRoutine class }
{------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiCopyPseudoRoutine.CompleteParams;
var
  SourceValue, IndexValue, CountValue: ISepiReadableValue;
begin
  inherited;

  if ParamCount = 0 then
  begin
    MakeError(SNotEnoughActualParameters);
    Exit;
  end;

  RequireReadableValue(Params[0], SourceValue);
  if ParamCount >= 2 then
    RequireReadableValue(Params[1], IndexValue);
  if ParamCount >= 3 then
    RequireReadableValue(Params[2], CountValue);

  if ParamCount = 2 then
  begin
    MakeError(SNotEnoughActualParameters);
    CountValue := TSepiErroneousValue.MakeReplacementValue(Expression,
      UnitCompiler.SystemUnit.Integer);
  end else if ParamCount > 3 then
  begin
    MakeError(STooManyActualParameters);
  end;

  if SourceValue.ValueType is TSepiStringType then
  begin
    if ParamCount = 1 then
    begin
      MakeError(SNotEnoughActualParameters);
      IndexValue := TSepiErroneousValue.MakeReplacementValue(Expression,
        UnitCompiler.SystemUnit.Integer);
      CountValue := IndexValue;
    end;

    BackingValue := TSepiStrCopyExpression.MakeStrCopy(SourceValue,
      IndexValue, CountValue);
  end else if SourceValue.ValueType is TSepiDynArrayType then
  begin
    if ParamCount = 1 then
    begin
      BackingValue := TSepiDynArrayCopyExpression.MakeDynArrayCopy(SourceValue);
    end else
    begin
      BackingValue := TSepiDynArrayCopyExpression.MakeDynArrayCopyRange(
        SourceValue, IndexValue, CountValue);
    end;
  end else
  begin
    Params[0].MakeError(SStringOrDynArrayTypeRequired);
  end;
end;

{-------------------------------------}
{ TSepiSpecialJumpPseudoRoutine class }
{-------------------------------------}

{*
  Crée la pseudo-routine de jump spécial
  @param AKind   Type de saut spécial
*}
constructor TSepiSpecialJumpPseudoRoutine.Create(AKind: TSepiSpecialJumpKind);
begin
  inherited Create;
  FKind := AKind;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSpecialJumpPseudoRoutine.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  Expression.Attach(ISepiExecutable, AsExpressionPart);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSpecialJumpPseudoRoutine.CompileExecute(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList);
begin
  case Kind of
    sjkContinue: Instructions.Add(TSepiContinue.Create(Compiler));
    sjkBreak:    Instructions.Add(TSepiBreak.Create(Compiler));
    sjkExit:     Instructions.Add(TSepiExit.Create(Compiler));
  end;
end;

end.

