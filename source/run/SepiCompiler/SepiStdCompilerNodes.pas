{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  S�bastien Doeraene
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
  Classes de compilation standard d'une unit� Sepi
  @author sjrd
  @version 1.0
*}
unit SepiStdCompilerNodes;

interface

{$ASSERTIONS ON}

uses
  Windows, Types, SysUtils, Classes, StrUtils, TypInfo, SysConst, ScUtils,
  ScStrUtils, ScDelphiLanguage,
  SepiCore,
  SepiReflectionCore, SepiMembers, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiOpCodes,
  SepiCompiler, SepiCompilerErrors, SepiExpressions, SepiInstructions,
  SepiCompilerConsts,
  SepiParseTrees, SepiCompilerUtils;

type
  {*
    Noeud section uses
    Chaque enfant d'un noeud section uses doit �tre un noeud dont le AsText est
    un nom d'unit�. Chaque unit� ainsi r�f�renc�e est ajout�e aux uses de
    l'unit� en cours de compilation.
    @author sjrd
    @version 1.0
  *}
  TSepiUsesNode = class(TSepiNonTerminal)
  private
    function IsRedeclared(const UnitName: string): Boolean;
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Identificateur qualifi�
    Chaque enfant d'un identificateur qualifi� est un des qualificateurs. Le
    AsText d'un TSepiQualifiedIdent est la liaison des AsText de ses enfants,
    avec des '.' entre chaque.
    @author sjrd
    @version 1.0
  *}
  TSepiQualifiedIdentNode = class(TSepiNonTerminal)
  protected
    function GetAsText: string; override;
  end;

  {*
    Noeud d'expression d'initialisation
    Une expression d'initialisation est une expression constante qui initialise
    une valeur connue � la compilation, et dont le type est d�j� connu.
    Avant son BeginParsing, la m�thode SetValueTypeAndPtr doit �tre appel�e pour
    sp�cifier quel est le type de valeur attendu, et o� placer le r�sultat.
    Les types accept�s par une instance particuli�re de
    TSepiInitializationExpressionNode peuvent �tre test�s avec IsValidType. Par
    d�faut, TSepiInitializationExpressionNode accepte tous les types non-nil.
    @author sjrd
    @version 1.0
  *}
  TSepiInitializationExpressionNode = class(TSepiNonTerminal)
  private
    FValueType: TSepiType; /// Type de valeur
    FValuePtr: Pointer;    /// Pointeur sur la valeur � initialiser

    FOwnsValue: Boolean; /// True si poss�de la valeur dans FValuePtr
  public
    destructor Destroy; override;

    procedure BeginParsing; override;

    function IsValidType(AValueType: TSepiType): Boolean; virtual;

    procedure SetValueTypeAndPtr(AValueType: TSepiType;
      AValuePtr: Pointer = nil);

    property ValueType: TSepiType read FValueType;
    property ValuePtr: Pointer read FValuePtr;
  end;

  {*
    Noeud expression
    @author sjrd
    @version 1.0
  *}
  TSepiExpressionNode = class(TSepiNonTerminal)
  private
    FExpression: ISepiExpression; /// Expression compil�e

    function TryAndForceType(var Value: ISepiValue;
      ValueType: TSepiType): Boolean;
    function TryAndConvert(var Value: ISepiValue;
      ValueType: TSepiType): Boolean;
    function TryAndMatchType(var Value: ISepiValue; ValueType: TSepiType;
      AllowConvertion: Boolean): Boolean;
    procedure RequireType(var Value: ISepiValue; ValueType: TSepiType;
      AllowConvertion: Boolean);
  protected
    procedure SetExpression(const AExpression: ISepiExpression);
    procedure SetExpressionPart(const ExpressionPart: ISepiExpressionPart);

    function MakeErroneousValue(ValueType: TSepiType = nil): ISepiReadableValue;

    function ValidateExpression: Boolean; virtual;
    procedure MakeErroneousExpression; virtual;
  public
    procedure EndParsing; override;

    function AsExpressionPart(const IID: TGUID; out Intf;
      const ErrorMsg: string = ''): Boolean;

    function AsValue(ValueType: TSepiType = nil;
      AllowConvertion: Boolean = True): ISepiValue;
    function AsReadableValue(ValueType: TSepiType = nil;
      AllowConvertion: Boolean = True): ISepiReadableValue;
    function AsMeta: TSepiMeta;
    function AsType: TSepiType;

    property Expression: ISepiExpression read FExpression;
  end;

  {*
    Noeud expression qui vaut la m�me expression que son unique fils
    Un exemple d'un tel noeud est l'expression parenth�s�e, qui se d�crit comme
      '('* Expression ')'*
    Sa fonction est uniquement syntaxique, et sa valeur se r�cup�re directement
    de son unique fils.
    @author sjrd
    @version 1.0
  *}
  TSepiSameAsChildExpressionNode = class(TSepiExpressionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud valeur
    @author sjrd
    @version 1.0
  *}
  TSepiValueNode = class(TSepiExpressionNode)
  protected
    function ValidateExpression: Boolean; override;
  end;

  {*
    Noeud expression calcul� par un arbre d'op�rations binaires
    @author sjrd
    @version 1.0
  *}
  TSepiBinaryOpTreeNode = class(TSepiExpressionNode)
  private
    function FindSubTreeOpIndex(Lower, Higher: Integer): Integer;
    function CompileSubTree(Lower, Higher: Integer): ISepiExpression;
  protected
    function CompileTree: ISepiExpression;
  public
    procedure EndParsing; override;
  end;

  {*
    Sens d'�valuation d'un op�rateur binaire
    - bodLeftToRight : �valu� de gauche � droite (comme la soustraction) ;
    - bodRightToLeft : �valu� de droite � gauche (comme le = du C).
  *}
  TSepiBinaryOpDirection = (bodLeftToRight, bodRightToLeft);

  {*
    Noeud repr�sentant un op�rateur binaire
    @author sjrd
    @version 1.0
  *}
  TSepiBinaryOpNode = class(TSepiNonTerminal)
  protected
    function GetPriority: Integer; virtual;
    function GetDirection: TSepiBinaryOpDirection; virtual;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; virtual; abstract;

    property Priority: Integer read GetPriority;
    property Direction: TSepiBinaryOpDirection read GetDirection;
  end;

  {*
    Noeud repr�sentant un op�rateur unaire
    @author sjrd
    @version 1.0
  *}
  TSepiUnaryOpNode = class(TSepiNonTerminal)
  public
    function MakeOperation(
      const Operand: ISepiExpression): ISepiExpression; virtual; abstract;
  end;

  {*
    Noeud repr�sentant une constante litt�rale
    @author sjrd
    @version 1.0
  *}
  TSepiLiteralConstNode = class(TSepiExpressionNode)
  protected
    function CompileAsValue: ISepiValue; virtual; abstract;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud repr�sentant une constante litt�rale enti�re
    @author sjrd
    @version 1.0
  *}
  TSepiConstIntegerNode = class(TSepiLiteralConstNode)
  protected
    function CompileAsInteger: Int64; virtual;

    function CompileAsValue: ISepiValue; override;
  end;

  {*
    Noeud repr�sentant une constante litt�rale flottante
    @author sjrd
    @version 1.0
  *}
  TSepiConstFloatNode = class(TSepiLiteralConstNode)
  protected
    function CompileAsExtended: Extended; virtual;

    function CompileAsValue: ISepiValue; override;
  end;

  {*
    Noeud repr�sentant une constante litt�rale cha�ne de caract�res
    @author sjrd
    @version 1.0
  *}
  TSepiConstStringNode = class(TSepiLiteralConstNode)
  protected
    function CompileAsString: string; virtual;

    function CompileAsValue: ISepiValue; override;
  end;

  {*
    Noeud repr�sentant un identificateur � r�soudre
    @author sjrd
    @version 1.0
  *}
  TSepiIdentifierExpressionNode = class(TSepiExpressionNode)
  protected
    procedure CompileIdentifier; virtual;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud repr�sentant la valeur sp�ciale nil
    @author sjrd
    @version 1.0
  *}
  TSepiNilValueNode = class(TSepiExpressionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud repr�sentant une expression d'appel inherited
    @author sjrd
    @version 1.0
  *}
  TSepiInheritedExpressionNode = class(TSepiExpressionNode)
  protected
    function RequireClassOrObjectMethod: Boolean;

    function GetMethodName: string; virtual;
    function FindMethod: TSepiMeta; virtual;

    function MakeCallable(SepiMethod: TSepiMeta;
      const SelfParam: ISepiReadableValue): ISepiCallable; virtual;

    function CompileCallable: ISepiCallable; virtual;
  public
    procedure EndParsing; override;
  end;

implementation

{------------------------}
{ TUsesSectionNode class }
{------------------------}

{*
  Teste si un nom d'unit� est red�clar�
*}
function TSepiUsesNode.IsRedeclared(const UnitName: string): Boolean;
var
  I: Integer;
begin
  // Same as the current unit name?
  if AnsiSameText(UnitName, SepiUnit.Name) then
  begin
    Result := True;
    Exit;
  end;

  // Already using this unit?
  for I := 0 to SepiUnit.UsedUnitCount-1 do
  begin
    if AnsiSameText(UnitName, SepiUnit.UsedUnits[I].Name) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

{*
  [@inheritDoc]
*}
procedure TSepiUsesNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  UnitName: string;
begin
  UnitName := Child.AsText;

  if IsRedeclared(UnitName) then
    Child.MakeError(SRedeclaredIdentifier)
  else
    SepiUnit.MoreUses([UnitName]);

  inherited;
end;

{-------------------------------}
{ TSepiQualifiedIdentNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
function TSepiQualifiedIdentNode.GetAsText: string;
var
  I: Integer;
begin
  Result := Children[0].AsText;
  for I := 1 to ChildCount-1 do
    Result := Result + '.' + Children[I].AsText;
end;

{-----------------------------------------}
{ TSepiInitializationExpressionNode class }
{-----------------------------------------}

{*
  [@inheritDoc]
*}
destructor TSepiInitializationExpressionNode.Destroy;
begin
  if FOwnsValue then
    ValueType.DisposeValue(FValuePtr);

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiInitializationExpressionNode.BeginParsing;
begin
  inherited;

  Assert(ValueType <> nil);
end;

{*
  Teste si un type est valide pour cet initialiseur
  @param AValueType   Type � tester
  @return True si le type est valide, False sinon
*}
function TSepiInitializationExpressionNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := AValueType <> nil;
end;

{*
  Indique le type de valeur attendu, et o� stocker le r�sultat
  Si AValuePtr vaut nil, une nouvelle valeur du type AValueType sera cr��e, et
  lib�r�e � la destruction de cet objet.
  Cette m�thode doit �tre appel�e exactement une fois, avant BeginParsing.
  @param AValueType   Type de valeur attendu (doit �tre valide)
  @param AValuePtr    Pointeur o� stocker le r�sultat (peut �tre nil)
*}
procedure TSepiInitializationExpressionNode.SetValueTypeAndPtr(
  AValueType: TSepiType; AValuePtr: Pointer = nil);
begin
  Assert((FValueType = nil) and IsValidType(AValueType));

  FValueType := AValueType;
  FOwnsValue := AValuePtr = nil;

  if FOwnsValue then
    FValuePtr := ValueType.NewValue
  else
    FValuePtr := AValuePtr;
end;

{---------------------------}
{ TSepiExpressionNode class }
{---------------------------}

{*
  Tente de forcer le type d'une valeur
  @param Value       Valeur dont forcer le type
  @param ValueType   Type requis
  @return True en cas de succ�s, False sinon
*}
function TSepiExpressionNode.TryAndForceType(var Value: ISepiValue;
  ValueType: TSepiType): Boolean;
var
  TypeForceableValue: ISepiTypeForceableValue;
begin
  Result := Supports(Value, ISepiTypeForceableValue, TypeForceableValue) and
    TypeForceableValue.CanForceType(ValueType);

  if Result then
    TypeForceableValue.ForceType(ValueType);
end;

{*
  Tente de convertir une valeur en un type donn�
  @param Value       Valeur � convertir
  @param ValueType   Type requis
  @return True en cas de succ�s, False sinon
*}
function TSepiExpressionNode.TryAndConvert(var Value: ISepiValue;
  ValueType: TSepiType): Boolean;
var
  ReadableValue: ISepiReadableValue;
begin
  Result := Supports(Value, ISepiReadableValue, ReadableValue) and
    TSepiConvertOperation.ConvertionExists(ValueType, Value.ValueType);

  if Result then
    Value := TSepiConvertOperation.ConvertValue(ValueType, ReadableValue);
end;

{*
  Tente de faire conrrespondre le type d'une valeur � un type donn�
  @param Value             Valeur � convertir
  @param ValueType         Type requis
  @param AllowConvertion   Autoriser des conversion pour obtenir le type requis
  @return True en cas de succ�s, False sinon
*}
function TSepiExpressionNode.TryAndMatchType(var Value: ISepiValue;
  ValueType: TSepiType; AllowConvertion: Boolean): Boolean;
begin
  Result := (Value.ValueType = ValueType) or
    ((Value.ValueType <> nil) and ValueType.Equals(Value.ValueType)) or
    TryAndForceType(Value, ValueType);

  Value.Finalize;

  if (not Result) and AllowConvertion then
    Result := TryAndConvert(Value, ValueType);
end;

{*
  Exige qu'une variable ait un type donn�
  @param Value             Valeur � convertir
  @param ValueType         Type requis
  @param AllowConvertion   Autoriser des conversion pour obtenir le type requis
*}
procedure TSepiExpressionNode.RequireType(var Value: ISepiValue;
  ValueType: TSepiType; AllowConvertion: Boolean);
begin
  if not TryAndMatchType(Value, ValueType, AllowConvertion) then
    MakeError(Format(STypeMismatch, [ValueType.Name, Value.ValueType.Name]));
end;

{*
  Modifie l'expression
  @param AExpression   Nouvelle expression
*}
procedure TSepiExpressionNode.SetExpression(const AExpression: ISepiExpression);
begin
  FExpression := AExpression;
end;

{*
  Modifie l'expression comme nouvelle expression partielle
  @param ExpressionPart   Nouvelle expression partielle
*}
procedure TSepiExpressionNode.SetExpressionPart(
  const ExpressionPart: ISepiExpressionPart);
begin
  SetExpression(MakeExpression);
  ExpressionPart.AttachToExpression(Expression);
end;

{*
  Construit une valeur erron�e
  @param ValueType   Type de valeur erron�e (peut �tre nil)
  @return Valeur construite
*}
function TSepiExpressionNode.MakeErroneousValue(
  ValueType: TSepiType = nil): ISepiReadableValue;
begin
  if ValueType = nil then
    Result := TSepiErroneousValue.Create(SepiRoot)
  else
    Result := TSepiErroneousValue.Create(ValueType);
end;

{*
  V�rifie la validit� de l'expression compil�e
  ValidateExpression n'est appel�e que si Expression <> nil.
  @return True si l'expression est valide, False sinon
*}
function TSepiExpressionNode.ValidateExpression: Boolean;
begin
  Result := True;
end;

{*
  Construit une expression erron�e
*}
procedure TSepiExpressionNode.MakeErroneousExpression;
begin
  SetExpressionPart(MakeErroneousValue);
end;

{*
  [@inheritDoc]
*}
procedure TSepiExpressionNode.EndParsing;
begin
  if (Expression = nil) or (not ValidateExpression) then
  begin
    MakeErroneousExpression;
    Assert(Expression <> nil);
  end;

  if (Expression.SourcePos.Line = 0) and (Expression.SourcePos.Col = 0) then
    Expression.SourcePos := SourcePos;

  inherited;
end;

{*
  Lit l'expression comme un type donn� de partie d'expression
  Si l'expression n'est du type requis, et si ErrorMsg est non vide, un message
  d'erreur est �mis, et AsExpressionPart renvoie False.
  @param IID        Type de partie d'expression requis
  @param Intf       En sortie : partie d'expression demand�e, si succ�s
  @param ErrorMsg   Message d'erreur en cas d'�chec
  @return True en cas de succ�s, False sinon
*}
function TSepiExpressionNode.AsExpressionPart(const IID: TGUID; out Intf;
  const ErrorMsg: string = ''): Boolean;
begin
  Result := Supports(Expression, IID, Intf);

  if (not Result) and (ErrorMsg <> '') then
    MakeError(ErrorMsg);
end;

{*
  Lit l'expression en tant que valeur
  @param ValueType         Type de valeur attendu (nil = tout type)
  @param AllowConvertion   Autoriser des conversion pour obtenir le type requis
  @return Valeur repr�sent�e par l'expression (ou nil en cas d'erreur)
*}
function TSepiExpressionNode.AsValue(ValueType: TSepiType = nil;
  AllowConvertion: Boolean = True): ISepiValue;
begin
  if not AsExpressionPart(ISepiValue, Result, SValueRequired) then
    Exit;

  if ValueType <> nil then
    RequireType(Result, ValueType, AllowConvertion);
end;

{*
  Lit l'expression en tant que valeur qui peut �tre lue
  @param ValueType         Type de valeur attendu (nil = tout type)
  @param AllowConvertion   Autoriser des conversion pour obtenir le type requis
  @return Valeur repr�sent�e par l'expression (ou nil en cas d'erreur)
*}
function TSepiExpressionNode.AsReadableValue(ValueType: TSepiType = nil;
  AllowConvertion: Boolean = True): ISepiReadableValue;
var
  Value: ISepiValue;
begin
  Value := AsValue(ValueType, AllowConvertion);

  if not Supports(Value, ISepiReadableValue, Result) then
    MakeError(SReadableValueRequired);
end;

{*
  Lit l'expression en tant que meta
  @return Meta repr�sent� par l'expression (ou nil en cas d'erreur)
*}
function TSepiExpressionNode.AsMeta: TSepiMeta;
var
  MetaExpression: ISepiMetaExpression;
begin
  if AsExpressionPart(ISepiMetaExpression, MetaExpression, SMetaRequired) then
    Result := MetaExpression.Meta
  else
    Result := nil;
end;

{*
  Lit l'expression en tant que type
  @return Type repr�sent� par l'expression (ou System.Integer en cas d'erreur)
*}
function TSepiExpressionNode.AsType: TSepiType;
var
  TypeExpression: ISepiTypeExpression;
begin
  if AsExpressionPart(ISepiTypeExpression, TypeExpression,
    STypeIdentifierRequired) then
    Result := TypeExpression.ExprType
  else
    Result := SystemUnit.Integer;
end;

{--------------------------------------}
{ TSepiSameAsChildExpressionNode class }
{--------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiSameAsChildExpressionNode.EndParsing;
begin
  Assert(ChildCount = 1);
  
  SetExpression((Children[0] as TSepiExpressionNode).Expression);

  inherited;
end;

{----------------------}
{ TSepiValueNode class }
{----------------------}

{*
  [@inheritDoc]
*}
function TSepiValueNode.ValidateExpression: Boolean;
begin
  Result := Supports(Expression, ISepiValue);
end;

{-----------------------------}
{ TSepiBinaryOpTreeNode class }
{-----------------------------}

{*
  Trouve l'op�rateur majeur d'un sous-arbre
  L'op�rateur majeur est celui de plus basse priorit� dans le sous-arbre. En cas
  d'�galit�, c'est celui de droite pour une �valuation left-to-right ; et c'est
  celui de gauche pour une �valuation right-to-left.
  @param Lower    Index bas des symboles du sous-arbre
  @param Higher   Index haut des symboles du sous-arbre
  @return Index de l'op�rateur majeur parmi les enfants de ce noeud
*}
function TSepiBinaryOpTreeNode.FindSubTreeOpIndex(
  Lower, Higher: Integer): Integer;
var
  OpIndex: Integer;
  MinPriority: Integer;
  OpChild: TSepiBinaryOpNode;
begin
  Result := -1;
  MinPriority := MaxInt;

  OpIndex := Lower+1;
  while OpIndex < Higher do
  begin
    OpChild := Children[OpIndex] as TSepiBinaryOpNode;

    if OpChild.Priority <= MinPriority then
    begin
      Result := OpIndex;
      MinPriority := OpChild.Priority;

      // In right-to-left, priority must strictly decrease to be taken into
      // account.
      if OpChild.Direction = bodRightToLeft then
        Dec(MinPriority);
    end;

    Inc(OpIndex, 2);
  end;

  Assert(Result > 0);
end;

{*
  Compile un sous-arbre de l'expression
  @param Lower    Index bas des symboles du sous-arbre
  @param Higher   Index haut des symboles du sous-arbre
  @return Expression repr�sentant le sous-arbre
*}
function TSepiBinaryOpTreeNode.CompileSubTree(
  Lower, Higher: Integer): ISepiExpression;
var
  OpIndex: Integer;
  LeftExpression, RightExpression: ISepiExpression;
begin
  if Lower = Higher then
    Result := (Children[Lower] as TSepiExpressionNode).Expression
  else
  begin
    OpIndex := FindSubTreeOpIndex(Lower, Higher);

    LeftExpression := CompileSubTree(Lower, OpIndex-1);
    RightExpression := CompileSubTree(OpIndex+1, Higher);

    Result := TSepiBinaryOpNode(Children[OpIndex]).MakeOperation(
      LeftExpression, RightExpression);
  end;
end;

{*
  Compile l'arbre de l'expression
  @return Expression repr�sentant l'arbre
*}
function TSepiBinaryOpTreeNode.CompileTree: ISepiExpression;
begin
  Assert(ChildCount mod 2 <> 0);

  Result := CompileSubTree(0, ChildCount-1);
end;

{*
  [@inheritDoc]
*}
procedure TSepiBinaryOpTreeNode.EndParsing;
begin
  if Expression = nil then
    SetExpression(CompileTree);

  inherited;
end;

{-------------------------}
{ TSepiBinaryOpNode class }
{-------------------------}

{*
  Priorit� de cet op�rateur
  @return Priorit� de cet op�rateur
*}
function TSepiBinaryOpNode.GetPriority: Integer;
begin
  Result := 1;
end;

{*
  Direction d'�valuation de cet op�rateur
  Afin que l'�valuation d'expressions soit coh�rente, tous les op�rateurs
  poss�dant la m�me priorit� doivent �galement �tre �valu�s dans la m�me
  direction.
  @return Direction d'�valuation de cet op�rateur
*}
function TSepiBinaryOpNode.GetDirection: TSepiBinaryOpDirection;
begin
  Result := bodLeftToRight;
end;

{-----------------------------}
{ TSepiLiteralConstNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiLiteralConstNode.EndParsing;
var
  Value: ISepiValue;
begin
  Value := CompileAsValue;

  if Value <> nil then
  begin
    SetExpression(MakeExpression);
    Value.AttachToExpression(Expression);
  end;

  inherited;
end;

{-----------------------------}
{ TSepiConstIntegerNode class }
{-----------------------------}

{*
  Compile la valeur sous forme d'entier
  En cas d'erreur, peut renvoyer une valeur arbitraire, 0 le plus souvent.
  @return Valeur repr�sent�e par le litt�ral
*}
function TSepiConstIntegerNode.CompileAsInteger: Int64;
begin
  if not TryStrToInt64(AsText, Result) then
  begin
    MakeError(Format(SInvalidInteger, [AsText]));
    Result := 0;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiConstIntegerNode.CompileAsValue: ISepiValue;
begin
  Result := TSepiTrueConstValue.Create(SepiRoot, CompileAsInteger);
end;

{---------------------------}
{ TSepiConstFloatNode class }
{---------------------------}

{*
  Compile la valeur sous forme de nombre � virgule flottante
  En cas d'erreur, peut renvoyer une valeur arbitraire, 0 le plus souvent.
  @return Valeur repr�sent�e par le litt�ral
*}
function TSepiConstFloatNode.CompileAsExtended: Extended;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.ThousandSeparator := #0;
  FormatSettings.DecimalSeparator := '.';

  if not TryStrToFloat(AsText, Result, FormatSettings) then
  begin
    MakeError(Format(SInvalidFloat, [AsText]));
    Result := 0.0;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiConstFloatNode.CompileAsValue: ISepiValue;
begin
  Result := TSepiTrueConstValue.Create(SepiRoot, CompileAsExtended);
end;

{----------------------------}
{ TSepiConstStringNode class }
{----------------------------}

{*
  Compile la valeur sous forme de cha�ne de caract�res
  En cas d'erreur, peut renvoyer une valeur arbitraire, '' le plus souvent.
  @return Valeur repr�sent�e par le litt�ral
*}
function TSepiConstStringNode.CompileAsString: string;
begin
  Result := StrRepresToStr(AsText);
end;

{*
  [@inheritDoc]
*}
function TSepiConstStringNode.CompileAsValue: ISepiValue;
var
  StrValue: string;
begin
  StrValue := CompileAsString;

  if Length(StrValue) = 1 then
    Result := TSepiTrueConstValue.Create(SepiRoot, StrValue[1])
  else
    Result := TSepiTrueConstValue.Create(SepiRoot, StrValue);
end;

{-------------------------------------}
{ TSepiIdentifierExpressionNode class }
{-------------------------------------}

{*
  Compile l'identificateur dans Expression
*}
procedure TSepiIdentifierExpressionNode.CompileIdentifier;
begin
  SetExpression(ResolveIdent(AsText));

  CheckIdentFound(Expression, AsText, Self);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIdentifierExpressionNode.EndParsing;
begin
  CompileIdentifier;

  inherited;
end;

{-------------------------}
{ TSepiNilValueNode class }
{-------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiNilValueNode.EndParsing;
begin
  SetExpression(MakeExpression);

  ISepiExpressionPart(TSepiNilValue.Create(SepiRoot)).AttachToExpression(
    Expression);

  inherited;
end;

{------------------------------------}
{ TSepiInheritedExpressionNode class }
{------------------------------------}

{*
  V�rifie qu'on est bien en train de compiler une m�thode
  @return True en cas de succ�s, False sinon
*}
function TSepiInheritedExpressionNode.RequireClassOrObjectMethod: Boolean;
begin
  Result := (MethodCompiler <> nil) and
    (MethodCompiler.SepiMethod.Owner is TSepiClass);

  if not Result then
    MakeError(SInheritNeedClassOrObjectMethod);
end;

{*
  Nom de la m�thode h�rit�e � appeler
  @return Nom de la m�thode h�rit�e � appeler
*}
function TSepiInheritedExpressionNode.GetMethodName: string;
begin
  Result := AsText;
end;

{*
  Trouve la m�thode h�rit�e � appeler
  @return M�thode � appeler (TSepiMethod et TSepiOverloadedMethod sont valides)
*}
function TSepiInheritedExpressionNode.FindMethod: TSepiMeta;
var
  CurrentClass, ParentClass: TSepiClass;
begin
  CurrentClass := MethodCompiler.SepiMethod.Owner as TSepiClass;
  ParentClass := CurrentClass.Parent;

  Result := ParentClass.LookForMember(GetMethodName,
    CurrentClass.OwningUnit, CurrentClass);

  CheckIdentFound(Result, GetMethodName, Self);
end;

{*
  Construit un callable
  @param SepiMethod   M�thode � appeler
  @param SelfVar      Variable locale Self
*}
function TSepiInheritedExpressionNode.MakeCallable(SepiMethod: TSepiMeta;
  const SelfParam: ISepiReadableValue): ISepiCallable;
const
  ForceStaticCall = True;
  FreeParamIsAlwaysFalse = False;
begin
  if SepiMethod is TSepiMethod then
  begin
    Result := TSepiMethodCall.Create(TSepiMethod(SepiMethod),
      SelfParam, ForceStaticCall, FreeParamIsAlwaysFalse);
  end else if SepiMethod is TSepiOverloadedMethod then
  begin
    Result := TSepiMethodCall.Create(TSepiOverloadedMethod(SepiMethod),
      SelfParam, ForceStaticCall, FreeParamIsAlwaysFalse);
  end else
  begin
    MakeError(SMethodRequired);
  end;
end;

{*
  Compile l'appel inherited comme un callable
  @return Callable repr�sent� par l'appel inherited (ou nil en cas d'erreur)
*}
function TSepiInheritedExpressionNode.CompileCallable: ISepiCallable;
var
  SepiMethod: TSepiMeta;
  SelfParam: ISepiValue;
begin
  if not RequireClassOrObjectMethod then
    Exit;

  SepiMethod := FindMethod;

  if SepiMethod <> nil then
  begin
    SelfParam := TSepiLocalVarValue.MakeValue(MethodCompiler,
      MethodCompiler.Locals.SelfVar);

    Result := MakeCallable(SepiMethod, SelfParam as ISepiReadableValue);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiInheritedExpressionNode.EndParsing;
var
  Callable: ISepiCallable;
begin
  Callable := CompileCallable;

  if Callable <> nil then
  begin
    SetExpression(MakeExpression);
    Callable.AttachToExpression(Expression);
  end;

  inherited;
end;

end.
