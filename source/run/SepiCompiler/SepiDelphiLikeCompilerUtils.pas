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
  Utilitaires pour les compilateurs Sepi de langages ressemblant à Delphi
  @author sjrd
  @version 1.0
*}
unit SepiDelphiLikeCompilerUtils;

interface

uses
  Windows, SysUtils, StrUtils, TypInfo, SepiReflectionCore, SepiMembers,
  SepiCompiler, SepiExpressions;

type
  {*
    Pseudo-routine d'opération sur un type
    @author sjrd
    @version 1.0
  *}
  ISepiTypeOperationPseudoRoutine = interface(ISepiReadableValue)
    ['{B2E927FC-5C5D-4E60-8A73-A5B5FF1FCBB3}']

    function GetOperand: ISepiTypeExpression;
    procedure SetOperand(const Value: ISepiTypeExpression);

    procedure Complete;

    property Operand: ISepiTypeExpression read GetOperand write SetOperand;
  end;

  {*
    Pseudo-routine de transtypage
    @author sjrd
    @version 1.0
  *}
  ISepiCastPseudoRoutine = interface(ISepiValue)
    ['{48556A76-A490-4F37-98C5-0930D931D4E1}']

    function GetOperand: ISepiValue;
    procedure SetOperand(const Value: ISepiValue);

    procedure Complete;

    property Operand: ISepiValue read GetOperand write SetOperand;
  end;

  {*
    Pseudo-routine d'opération sur un type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeOperationPseudoRoutine = class(TSepiTypeOperationValue,
    ISepiTypeOperationPseudoRoutine)
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetOperand: ISepiTypeExpression;
    procedure SetOperand(const Value: ISepiTypeExpression);
  end;

  {*
    Pseudo-routine de transtypage
    @author sjrd
    @version 1.0
  *}
  TSepiCastPseudoRoutine = class(TSepiCastOperator, ISepiCastPseudoRoutine)
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetOperand: ISepiValue;
    procedure SetOperand(const Value: ISepiValue);
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

procedure AddMetaToExpression(const Expression: ISepiExpression;
  Meta: TSepiMeta);
function AddPseudoRoutineToExpression(const Expression: ISepiExpression;
  const Identifier: string): Boolean;

function UnitResolveIdent(UnitCompiler: TSepiUnitCompiler;
  const Identifier: string): ISepiExpression;
function MethodResolveIdent(Compiler: TSepiMethodCompiler;
  const Identifier: string): ISepiExpression;

function FieldSelection(SepiContext: TSepiMeta;
  const BaseExpression: ISepiExpression;
  const FieldName: string): ISepiExpression;

implementation

{*
  Ajoute un meta à une expression selon les mêmes règles qu'en Delphi
  @param Expression   Expression
  @param Meta         Meta (peut être nil)
*}
procedure AddMetaToExpression(const Expression: ISepiExpression;
  Meta: TSepiMeta);
var
  MetaClassValue: TSepiMetaClassValue;
begin
  if Meta = nil then
    Exit;

  // Simply the meta
  ISepiExpressionPart(
    TSepiMetaExpression.Create(Meta)).AttachToExpression(Expression);

  if Meta is TSepiType then
  begin
    // Type
    ISepiExpressionPart(TSepiTypeExpression.Create(
      TSepiType(Meta))).AttachToExpression(Expression);

    if Meta is TSepiClass then
    begin
      MetaClassValue := TSepiMetaClassValue.Create(TSepiClass(Meta));
      ISepiExpressionPart(MetaClassValue).AttachToExpression(Expression);
      MetaClassValue.Complete;
    end;
  end else if Meta is TSepiConstant then
  begin
    // Constant
    ISepiExpressionPart(TSepiTrueConstValue.Create(
      TSepiConstant(Meta))).AttachToExpression(Expression);
  end else if Meta is TSepiVariable then
  begin
    // Variable
    ISepiExpressionPart(TSepiVariableValue.Create(
      TSepiVariable(Meta))).AttachToExpression(Expression);
  end else if Meta is TSepiMethod then
  begin
    // Method
    ISepiExpressionPart(TSepiMethodCall.Create(
      TSepiMethod(Meta))).AttachToExpression(Expression);
  end else if Meta is TSepiOverloadedMethod then
  begin
    // Method
    ISepiExpressionPart(TSepiMethodCall.Create(
      TSepiOverloadedMethod(Meta))).AttachToExpression(Expression);
  end;
end;

{*
  Ajoute une pseudo-routine à une expression
  @param Expression   Expression
  @param Identifier   Identificateur de la pseudo-routine
  @return True si une pseudo-routine a été trouvée, False sinon
*}
function AddPseudoRoutineToExpression(const Expression: ISepiExpression;
  const Identifier: string): Boolean;
var
  TypeOpIndex: Integer;
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

  Result := False;
end;

{*
  Résoud un identificateur dans le contexte d'une unité
  @param UnitCompiler   Compilateur d'unité
  @param Identifier     Identificateur recherché
  @return Expression représentant l'identificateur, ou nil si non trouvé
*}
function UnitResolveIdent(UnitCompiler: TSepiUnitCompiler;
  const Identifier: string): ISepiExpression;
var
  Meta: TSepiMeta;
begin
  Result := TSepiExpression.Create(UnitCompiler);

  // Meta
  Meta := UnitCompiler.SepiUnit.LookFor(Identifier);
  if Meta <> nil then
  begin
    AddMetaToExpression(Result, Meta);
    Exit;
  end;

  // Pseudo-routine
  if AddPseudoRoutineToExpression(Result, Identifier) then
    Exit;

  Result := nil;
end;

{*
  Résoud un identificateur dans le contexte d'une méthode
  @param Compiler     Compilateur de méthode
  @param Identifier   Identificateur recherché
  @return Expression représentant l'identificateur, ou nil si non trouvé
*}
function MethodResolveIdent(Compiler: TSepiMethodCompiler;
  const Identifier: string): ISepiExpression;
var
  LocalVar: TSepiLocalVar;
  Meta: TSepiMeta;
begin
  Result := TSepiExpression.Create(Compiler);

  // Variables locales
  LocalVar := Compiler.Locals.GetVarByName(Identifier);
  if LocalVar <> nil then
  begin
    ISepiExpressionPart(
      TSepiLocalVarValue.Create(LocalVar)).AttachToExpression(Result);
    Exit;
  end;

  // Meta
  Meta := Compiler.SepiMethod.LookFor(Identifier);
  if Meta <> nil then
  begin
    AddMetaToExpression(Result, Meta);
    Exit;
  end;

  // Pseudo-routine
  if AddPseudoRoutineToExpression(Result, Identifier) then
    Exit;

  Result := nil;
end;

{*
  Sélection de champ d'une valeur classe, meta-classe ou interface
  @param SepiContext   Contexte Sepi depuis lequel chercher
  @param BaseValue     Valeur de base
  @param FieldName     Nom du champ
  @param Expression    Expression destination
  @return True si réussi, False sinon
*}
function ClassIntfMemberSelection(SepiContext: TSepiMeta;
  const BaseValue: ISepiReadableValue; const FieldName: string;
  const Expression: ISepiExpression): Boolean;
const
  mkClassMethods = [mkClassProcedure, mkClassFunction];
  mkConstrClassMethods = [mkConstructor] + mkClassMethods;
var
  Value: ISepiReadableValue;
  ContainerType: TSepiType;
  FromClass: TSepiMeta;
  Member: TSepiMeta;
begin
  Result := False;
  Value := BaseValue;

  // Fetch container value
  ContainerType := Value.ValueType;
  if ContainerType is TSepiMetaClass then
    ContainerType := TSepiMetaClass(ContainerType).SepiClass;

  // Fetch member
  if ContainerType is TSepiClass then
  begin
    // Set FromClass
    FromClass := SepiContext;
    while (FromClass <> nil) and (not (FromClass is TSepiClass)) do
      FromClass := FromClass.Owner;

    Member := TSepiClass(ContainerType).LookForMember(
      FieldName, SepiContext.OwningUnit, TSepiClass(FromClass));
  end else if ContainerType is TSepiInterface then
  begin
    Member := TSepiInterface(ContainerType).LookForMember(FieldName);
  end else
  begin
    Assert(False);
    Member := nil;
  end;

  // Exit if no member found, or if it is not a member
  if Member = nil then
    Exit;
  if (not (Member is TSepiField)) and (not (Member is TSepiMethod)) and
    (not (Member is TSepiOverloadedMethod)) and
    (not (Member is TSepiProperty)) then
    Exit;

  // OK
  if Member is TSepiField then
  begin
    // Field
    ISepiExpressionPart(TSepiObjectFieldValue.Create(
      Value, TSepiField(Member))).AttachToExpression(Expression);
  end else if Member is TSepiMethod then
  begin
    // Method
    ISepiExpressionPart(TSepiMethodCall.Create(TSepiMethod(Member),
      Value)).AttachToExpression(Expression);
  end else if Member is TSepiOverloadedMethod then
  begin
    // Overloaded method
    ISepiExpressionPart(TSepiMethodCall.Create(TSepiOverloadedMethod(Member),
      Value)).AttachToExpression(Expression);
  end else
  begin
    // Property
    Assert(Member is TSepiProperty);
    ISepiExpressionPart(TSepiPropertyValue.Create(Value,
      TSepiProperty(Member))).AttachToExpression(Expression);
  end;

  Result := True;
end;

{*
  Sélection de champ d'une expression selon les règles du Delphi
  @param SepiContext      Contexte Sepi depuis lequel chercher
  @param BaseExpression   Expression de base
  @param FieldName        Nom du champ
  @return Expression représentant le champ sélectionné
*}
function FieldSelection(SepiContext: TSepiMeta;
  const BaseExpression: ISepiExpression;
  const FieldName: string): ISepiExpression;
var
  CancelResult: Boolean;
  MetaExpression: ISepiMetaExpression;
  Meta: TSepiMeta;
  Value: ISepiValue;
  ReadableValue: ISepiReadableValue;
begin
  Result := TSepiExpression.Create(BaseExpression);
  CancelResult := True;

  // Meta child
  if Supports(BaseExpression, ISepiMetaExpression, MetaExpression) then
  begin
    Meta := MetaExpression.Meta.GetMeta(FieldName);

    if Meta <> nil then
    begin
      AddMetaToExpression(Result, Meta);
      CancelResult := False;
    end;
  end;

  // Record field
  if Supports(BaseExpression, ISepiValue, Value) then
  begin
    if Value.ValueType is TSepiRecordType then
    begin
      Meta := TSepiRecordType(Value.ValueType).GetMeta(FieldName);

      if Meta is TSepiField then
      begin
        ISepiExpressionPart(TSepiRecordFieldValue.Create(
          Value, TSepiField(Meta))).AttachToExpression(Result);
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
      if ClassIntfMemberSelection(SepiContext, ReadableValue,
        FieldName, Result) then
        CancelResult := False;
    end;
  end;

  if CancelResult then
    Result := nil;
end;

{---------------------------------------}
{ TSepiTypeOperationPseudoRoutine class }
{---------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiTypeOperationPseudoRoutine.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;
  Expression.Attach(ISepiTypeOperationPseudoRoutine, AsExpressionPart);

  if Operand <> nil then
    inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiTypeOperationPseudoRoutine.GetOperand: ISepiTypeExpression;
begin
  Result := Operand;
end;

{*
  [@inheritDoc]
*}
procedure TSepiTypeOperationPseudoRoutine.SetOperand(
  const Value: ISepiTypeExpression);
begin
  Operand := Value;
end;

{------------------------------}
{ TSepiCastPseudoRoutine class }
{------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiCastPseudoRoutine.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;
  Expression.Attach(ISepiCastPseudoRoutine, AsExpressionPart);

  if Operand <> nil then
    inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiCastPseudoRoutine.GetOperand: ISepiValue;
begin
  Result := Operand;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCastPseudoRoutine.SetOperand(const Value: ISepiValue);
begin
  Operand := Value;
end;

end.

