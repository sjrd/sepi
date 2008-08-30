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
  Expressions Sepi
  @author sjrd
  @version 1.0
*}
unit SepiExpressions;

interface

uses
  SysUtils, TypInfo, ScUtils, ScInterfaces, SepiReflectionCore, SepiOrdTypes,
  SepiStrTypes, SepiArrayTypes, SepiMembers, SepiOpCodes, SepiRuntimeOperations,
  SepiCompiler, SepiCompilerErrors, SepiCompilerConsts, SepiAsmInstructions;

type
  /// Opération Sepi
  TSepiOperation = opAdd..opCmpGE;

  {*
    Opération Sepi sur un type
    - toSizeOf : SizeOf(TypeName)
    - toTypeInfo : TypeInfo(TypeName)
    - toLowerBound : Low(TypeName)
    - toHigherBound : High(TypeName)
    - toLength : Length(TypeName)
  *}
  TSepiTypeOperation = (
    toSizeOf, toTypeInfo, toLowerBound, toHigherBound, toLength
  );

const
  /// Toutes les opérations valides
  opAllOps = [Low(TSepiOperation)..High(TSepiOperation)];

  /// Opérations unaires
  opUnaryOps = [opNegate, opNot];

  /// Operations binaires (dont comparaisons)
  opBinaryOps = opAllOps - opUnaryOps;

  /// Opérations de comparaison
  opComparisonOps = [opCmpEQ..opCmpGE];

type
  {*
    Valeur qui peut être lue à l'exécution
    @author sjrd
    @version 1.0
  *}
  ISepiReadableValue = interface(ISepiExpressionPart)
    ['{D7207F63-22F1-41C7-8366-2D4A87DD5200}']

    function GetValueType: TSepiType;

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property ValueType: TSepiType read GetValueType;

    property IsConstant: Boolean read GetIsConstant;
    property ConstValuePtr: Pointer read GetConstValuePtr;
  end;

  {*
    Valeur qui peut être modifiée à l'exécution
    @author sjrd
    @version 1.0
  *}
  ISepiWritableValue = interface(ISepiExpressionPart)
    ['{E5E9C42F-E9A2-4B13-AE79-E1229013007D}']

    function GetValueType: TSepiType;

    procedure CompileWrite(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Source: ISepiReadableValue);

    property ValueType: TSepiType read GetValueType;
  end;

  {*
    Valeur dont on peut récupérer l'adresse à l'exécution
    @author sjrd
    @version 1.0
  *}
  ISepiAddressableValue = interface(ISepiExpressionPart)
    ['{096E1AD8-04D6-4DA1-9426-A307C7965F73}']

    function GetAddressType: TSepiType;

    procedure CompileLoadAddress(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property AddressType: TSepiType read GetAddressType;
  end;

  {*
    Valeur à accès direct (par opposition à une valeur à calculer)
    @author sjrd
    @version 1.0
  *}
  ISepiDirectValue = interface(ISepiExpressionPart)
    ['{12678933-C5A4-4BBA-9BBF-3E3BF30B4AD5}']

    function GetValueType: TSepiType;

    property ValueType: TSepiType read GetValueType;
  end;

  {*
    Expression qui peut être appelée
    @author sjrd
    @version 1.0
  *}
  ISepiCallable = interface(ISepiExpressionPart)
    ['{CD26F811-9B78-4F42-ACD7-82B1FB93EA3F}']

    function GetParamCount: Integer;
    function GetParams(Index: Integer): ISepiExpression;
    function GetParamsCompleted: Boolean;

    procedure AddParam(Param: ISepiExpression);
    procedure CompleteParams;

    function GetReturnType: TSepiType;

    procedure CompileNoResult(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiExpression read GetParams;
    property ParamsCompleted: Boolean read GetParamsCompleted;

    property ReturnType: TSepiType read GetReturnType;
  end;

  {*
    Propriété
    @author sjrd
    @version 1.0
  *}
  ISepiProperty = interface(ISepiExpressionPart)
    ['{9C5A0B51-BDB2-45FA-B2D4-179763CB7F16}']

    function GetParamCount: Integer;
    function GetParams(Index: Integer): ISepiExpression;
    procedure SetParams(Index: Integer; Value: ISepiExpression);
    function GetParamsCompleted: Boolean;

    procedure CompleteParams;

    function GetValueType: TSepiType;

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiExpression
      read GetParams write SetParams;
    property ParamsCompleted: Boolean read GetParamsCompleted;

    property ValueType: TSepiType read GetValueType;
  end;

  {*
    Expression nil
    @author sjrd
    @version 1.0
  *}
  ISepiNilValue = interface(ISepiExpressionPart)
    ['{2574C3DC-87FE-426C-931D-5230267A1573}']
  end;

  {*
    Expression qui représente un meta
    @author sjrd
    @version 1.0
  *}
  ISepiMetaExpression = interface(ISepiExpressionPart)
    ['{444E8E70-1173-44E5-AA91-5B28629A9AA4}']

    function GetMeta: TSepiMeta;

    property Meta: TSepiMeta read GetMeta;
  end;

  {*
    Expression qui représente un type
    @author sjrd
    @version 1.0
  *}
  ISepiTypeExpression = interface(ISepiExpressionPart)
    ['{F8DB8648-9F7B-421A-9BEC-0C4AEC9D1C56}']

    function GetType: TSepiType;

    property ExprType: TSepiType read GetType;
  end;

  {*
    Implémentation de base de ISepiExpressionPart
    La majorité des classes devant implémenter ISepiExpressionPart devraient
    hériter de cette classe.
    @author sjrd
    @version 1.0
  *}
  TSepiCustomExpressionPart = class(TDynamicallyLinkedObject,
    ISepiExpressionPart)
  private
    FExpression: Pointer; /// Référence faible à l'expression contrôleur

    FSepiRoot: TSepiRoot;                 /// Racine Sepi
    FUnitCompiler: TSepiUnitCompiler;     /// Compilateur d'unité
    FMethodCompiler: TSepiMethodCompiler; /// Compilateur de méthode

    function GetExpression: ISepiExpression;
  protected
    procedure AttachTo(const Controller: IInterface); override;
    procedure DetachFromController; override;

    procedure AttachToExpression(
      const Expression: ISepiExpression); virtual; abstract;

    procedure MakeError(const Msg: string; Kind: TSepiErrorKind = ekError);

    property Expression: ISepiExpression read GetExpression;

    property SepiRoot: TSepiRoot read FSepiRoot;
    property UnitCompiler: TSepiUnitCompiler read FUnitCompiler;
    property MethodCompiler: TSepiMethodCompiler read FMethodCompiler;
  end;

  {*
    Implémentation de base de ISepiDirectValue
    Bien que, syntaxiquement, cette classe implémente les trois interfaces
    ISepiReadableValue, ISepiWritableValue et ISepiAddressableValue, les
    classes descendantes peuvent activer et désactiver celles-ci à volonté au
    moyen des propriétés IsReadable, IsWritable et IsAddressable respectivement.
    @author sjrd
    @version 1.0
  *}
  TSepiCustomDirectValue = class(TSepiCustomExpressionPart, ISepiDirectValue,
    ISepiReadableValue, ISepiWritableValue, ISepiAddressableValue)
  private
    FValueType: TSepiType; /// Type de la valeur

    FIsReadable: Boolean;    /// Indique si la valeur peut être lue
    FIsWritable: Boolean;    /// Indique si la valeur peut être écrite
    FIsAddressable: Boolean; /// Indique si la valeur peut être adressée

    FConstValuePtr: Pointer; /// Pointeur sur la valeur constante
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetValueType: TSepiType;
    procedure SetValueType(AType: TSepiType);
    function GetAddressType: TSepiType;

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; virtual;
      abstract;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); virtual;

    procedure CompileWrite(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Source: ISepiReadableValue); virtual;

    procedure CompileLoadAddress(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); virtual;

    property IsReadable: Boolean read FIsReadable write FIsReadable;
    property IsWritable: Boolean read FIsWritable write FIsWritable;
    property IsAddressable: Boolean read FIsAddressable write FIsAddressable;

    property IsConstant: Boolean read GetIsConstant;
    property ConstValuePtr: Pointer read FConstValuePtr write FConstValuePtr;
  public
    property ValueType: TSepiType read FValueType;
    property AddressType: TSepiType read GetAddressType;
  end;

  {*
    Classe de base pour des valeurs à calculer
    @author sjrd
    @version 1.0
  *}
  TSepiCustomComputedValue = class(TSepiCustomExpressionPart,
    ISepiReadableValue)
  private
    FValueType: TSepiType; /// Type de valeur

    FConstValuePtr: Pointer; /// Pointeur sur la valeur constante
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetValueType: TSepiType;
    procedure SetValueType(AType: TSepiType);

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    procedure AllocateConstant;

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); virtual; abstract;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property IsConstant: Boolean read GetIsConstant;
    property ConstValuePtr: Pointer read FConstValuePtr;
  public
    destructor Destroy; override;

    property ValueType: TSepiType read FValueType;
  end;

  {*
    Valeur constante (vraie constante ou constante litérale)
    @author sjrd
    @version 1.0
  *}
  TSepiTrueConstValue = class(TSepiCustomDirectValue)
  private
    FConstant: TSepiConstant;

    constructor Create(AType: TSepiType); overload;
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    constructor Create(Constant: TSepiConstant); overload;

    constructor Create(SepiRoot: TSepiRoot; Value: Int64); overload;
    constructor Create(SepiRoot: TSepiRoot; Value: Extended); overload;

    constructor Create(SepiRoot: TSepiRoot; Value: AnsiChar); overload;
    constructor Create(SepiRoot: TSepiRoot; Value: WideChar); overload;

    constructor Create(SepiRoot: TSepiRoot; const Value: AnsiString); overload;
    constructor Create(SepiRoot: TSepiRoot; const Value: WideString); overload;

    destructor Destroy; override;

    property Constant: TSepiConstant read FConstant;
    property ConstValuePtr;
  end;

  {*
    Valeur variable globale
    @author sjrd
    @version 1.0
  *}
  TSepiVariableValue = class(TSepiCustomDirectValue)
  private
    FVariable: TSepiVariable;
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    constructor Create(AVariable: TSepiVariable);

    property Variable: TSepiVariable read FVariable;
  end;

  {*
    Valeur variable locale
    @author sjrd
    @version 1.0
  *}
  TSepiLocalVarValue = class(TSepiCustomDirectValue)
  private
    FLocalVar: TSepiLocalVar;
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    constructor Create(ALocalVar: TSepiLocalVar);

    property LocalVar: TSepiLocalVar read FLocalVar;
  end;

  {*
    Opérateur de transtypage
    @author sjrd
    @version 1.0
  *}
  TSepiCastOperator = class(TSepiCustomDirectValue)
  private
    FOperand: ISepiExpression; /// Expression source
    FForceCast: Boolean;       /// True force même avec des tailles différentes

    procedure CollapseConsts;
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;

    procedure CompileWrite(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Source: ISepiReadableValue); override;

    procedure CompileLoadAddress(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    constructor Create(DestType: TSepiType;
      const AOperand: ISepiExpression = nil);

    procedure Complete;

    class function CastExpression(DestType: TSepiType;
      const Expression: ISepiExpression;
      ForceCast: Boolean = False): ISepiExpression;

    property Operand: ISepiExpression read FOperand write FOperand;
    property ForceCast: Boolean read FForceCast write FForceCast;
  end;

  {*
    Opération de conversion
    @author sjrd
    @version 1.0
  *}
  TSepiConvertOperation = class(TSepiCustomComputedValue)
  private
    FSource: ISepiReadableValue; /// Expression source

    procedure CollapseConsts;
  protected
    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    constructor Create(DestType: TSepiType;
      const ASource: ISepiReadableValue = nil);

    procedure Complete;

    class function ConvertionExists(DestType, SrcType: TSepiType): Boolean;
    class function ConvertValue(DestType: TSepiType;
      const Value: ISepiReadableValue): ISepiReadableValue;

    property Source: ISepiReadableValue read FSource write FSource;
  end;

  {*
    Opération arithmétique ou logique
    @author sjrd
    @version 1.0
  *}
  TSepiArithmeticLogicOperation = class(TSepiCustomComputedValue)
  private
    FOperation: TSepiOperation;
  protected
    procedure ErrorTypeNotApplicable;

    function GetOpCode(SelfOp: Boolean): TSepiOpCode;
  public
    constructor Create(AOperation: TSepiOperation);

    property Operation: TSepiOperation read FOperation;
  end;

  {*
    Opération unaire
    @author sjrd
    @version 1.0
  *}
  TSepiUnaryOperation = class(TSepiArithmeticLogicOperation)
  private
    FOperand: ISepiReadableValue; /// Opérande

    procedure CheckType;
    procedure CollapseConsts;
  protected
    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    procedure Complete;

    property Operand: ISepiReadableValue read FOperand write FOperand;
  end;

  {*
    Opération binaire (dont comparaisons)
    @author sjrd
    @version 1.0
  *}
  TSepiBinaryOperation = class(TSepiArithmeticLogicOperation)
  private
    FLeftOperand: ISepiReadableValue;  /// Opérande de gauche
    FRightOperand: ISepiReadableValue; /// Opérande de droite

    /// Si True, les opérandes sont convertis automatiquement
    FAutoConvert: Boolean;

    function CheckNonBaseTypes(LeftType, RightType: TSepiType): TSepiBaseType;
    procedure CheckTypesShift(LeftBaseType, RightBaseType: TSepiBaseType;
      BaseTypes: TSepiBaseTypes);
    procedure CheckTypesComparison(LeftBaseType, RightBaseType: TSepiBaseType;
      BaseTypes: TSepiBaseTypes);
    procedure CheckTypesArithmetic(LeftBaseType, RightBaseType: TSepiBaseType;
      BaseTypes: TSepiBaseTypes);
    procedure CheckTypes;

    procedure CollapseConsts;
  protected
    procedure ErrorTypeMismatch;

    function DefaultSepiTypeFor(BaseType: TSepiBaseType): TSepiType;

    function ConvertLeftOperand(NewType: TSepiBaseType): TSepiType;
    function ConvertRightOperand(NewType: TSepiBaseType): TSepiType;

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    constructor Create(AOperation: TSepiOperation;
      AAutoConvert: Boolean = True);

    procedure Complete;

    class function SmallestCommonType(LeftType, RightType: TSepiBaseType;
      out CommonType: TSepiBaseType): Boolean;

    property LeftOperand: ISepiReadableValue
      read FLeftOperand write FLeftOperand;
    property RightOperand: ISepiReadableValue
      read FRightOperand write FRightOperand;

    property AutoConvert: Boolean read FAutoConvert write FAutoConvert;
  end;

  {*
    Opérateur d'adressage
    @author sjrd
    @version 1.0
  *}
  TSepiAddressOfValue = class(TSepiCustomComputedValue)
  private
    FOperand: ISepiAddressableValue; /// Opérande
  protected
    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    procedure Complete;

    property Operand: ISepiAddressableValue read FOperand write FOperand;
  end;

  {*
    Opérateur de déréférencement
    @author sjrd
    @version 1.0
  *}
  TSepiDereferenceValue = class(TSepiCustomDirectValue)
  private
    FOperand: ISepiReadableValue; /// Opérande
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    procedure Complete;

    property Operand: ISepiReadableValue read FOperand write FOperand;
  end;

  {*
    Opération sur un type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeOperationValue = class(TSepiCustomComputedValue)
  private
    FOperation: TSepiTypeOperation; /// Opération
    FOperand: ISepiTypeExpression;  /// Opérande
    FNilIfNoTypeInfo: Boolean;      /// Si True, TypeInfo peut renvoyer nil

    procedure CheckOperand;
    procedure CompleteSizeOf;
    procedure CompleteTypeInfo;
    procedure CompleteArrayBound;
    procedure CompleteOrdinalBound;
  protected
    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    constructor Create(AOperation: TSepiTypeOperation;
      ANilIfNoTypeInfo: Boolean = False);

    procedure Complete;

    property Operation: TSepiTypeOperation read FOperation;
    property Operand: ISepiTypeExpression read FOperand write FOperand;
    property NilIfNoTypeInfo: Boolean
      read FNilIfNoTypeInfo write FNilIfNoTypeInfo;
  end;

  {*
    Valeur nil
    @author sjrd
    @version 1.0
  *}
  TSepiNilValue = class(TSepiCustomExpressionPart, ISepiNilValue)
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;
  end;

  {*
    Expression représentant un meta
    @author sjrd
    @version 1.0
  *}
  TSepiMetaExpression = class(TSepiCustomExpressionPart, ISepiMetaExpression)
  private
    FMeta: TSepiMeta; /// Meta
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetMeta: TSepiMeta;
  public
    constructor Create(AMeta: TSepiMeta);

    property Meta: TSepiMeta read FMeta;
  end;

  {*
    Expression représentant un type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeExpression = class(TSepiCustomExpressionPart, ISepiTypeExpression)
  private
    FType: TSepiType; /// Type
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetType: TSepiType;
  public
    constructor Create(AType: TSepiType);

    property ExprType: TSepiType read FType;
  end;

procedure NeedDestination(var Destination: TSepiMemoryReference;
  ValueType: TSepiType; Compiler: TSepiMethodCompiler;
  TempVars: TSepiTempVarsLifeManager; BeginLifeAt: TSepiInstructionRef);

function IsZeroMemory(Address: Pointer; Size: Integer): Boolean;

function SepiTypeToBaseType(SepiType: TSepiType;
  out BaseType: TSepiBaseType): Boolean; overload;
function SepiTypeToBaseType(SepiType: TSepiType): TSepiBaseType; overload;

function DefaultSepiTypeFor(BaseType: TSepiBaseType;
  SepiRoot: TSepiRoot): TSepiType;

implementation

const
  /// Zéro mémoire
  ZeroMemory: array[0..SizeOf(Variant)-1] of Byte = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  );

  /// Table de conversion d'opération en son OpCode
  OperationToOpCode: array[TSepiOperation] of TSepiOpCode = (
    ocOtherAdd, ocOtherSubtract, ocOtherMultiply, ocOtherDivide, ocOtherIntDiv,
    ocOtherModulus, ocOtherShl, ocOtherShr, ocOtherAnd, ocOtherOr, ocOtherXor,
    ocNope, ocOtherNeg, ocOtherNot, ocCompEquals, ocCompNotEquals, ocCompLower,
    ocCompLowerEq, ocCompGreater, ocCompGreaterEq
  );

var
  BaseTypeToDefaultTypeInfo: array[TSepiBaseType] of PTypeInfo;

{-----------------}
{ Global routines }
{-----------------}

{*
  Exige une destination mémoire valide
  Si Destination vaut nil en entrée, cette routine allouera une variable
  temporaire du type demandé pour avoir une référence mémoire valide.
  @param Destination   Destination mémoire
  @param ValueType     Type de valeur à mettre dans la destination
  @param Compiler      Compilateur
  @param TempVars      Gestionnaire de vie de variables temporaires
  @param BeginLifeAt   Début de la vie de la variable temporaire
*}
procedure NeedDestination(var Destination: TSepiMemoryReference;
  ValueType: TSepiType; Compiler: TSepiMethodCompiler;
  TempVars: TSepiTempVarsLifeManager; BeginLifeAt: TSepiInstructionRef);
var
  TempVar: TSepiLocalVar;
begin
  if Destination <> nil then
    Exit;

  TempVar := Compiler.Locals.AddTempVar(ValueType);
  TempVars.BeginLife(TempVar, BeginLifeAt);

  Destination := TSepiMemoryReference.Create(Compiler);
  Destination.SetSpace(TempVar);
  Destination.Seal;
end;

{*
  Teste si une valeur est un zéro mémoire, tel qu'on peut utiliser msZero
  Une valeur est un zéro mémoire si et seulement si sa taille n'est pas plus
  grande qu'un Variant et que tous ses octets sont nuls.
  @param Address   Adresse mémoire de la valeur
  @param Size      Taille de la valeur
  @return True si la valeur est un zéro mémoire, False sinon
*}
function IsZeroMemory(Address: Pointer; Size: Integer): Boolean;
begin
  if Size > SizeOf(Variant) then
    Result := False
  else
  begin
    while Size > 0 do
    begin
      if PByte(Address)^ <> 0 then
      begin
        Result := False;
        Exit;
      end;

      Inc(Cardinal(Address));
      Dec(Size);
    end;

    Result := True;
  end;
end;

{*
  Récupère le type de base correspondant à un type Sepi
  @param SepiType   Type Sepi
  @param BaseType   En sortie : type de base correspondant, si existe
  @return False si le type Sepi n'admet pas de type de base, True sinon
*}
function SepiTypeToBaseType(SepiType: TSepiType;
  out BaseType: TSepiBaseType): Boolean;
const
  OrdTypeToBaseType: array[TOrdType] of TSepiBaseType = (
    btShortint, btByte, btSmallint, btWord, btLongint, btDWord
  );
  FloatTypeToBaseType: array[TFloatType] of TSepiBaseType = (
    btSingle, btDouble, btExtended, btComp, btCurrency
  );
  IsUnicodeToBaseType: array[Boolean] of TSepiBaseType = (
    btAnsiStr, btWideStr
  );
begin
  Result := True;

  if (SepiType is TSepiIntegerType) or (SepiType is TSepiCharType) then
    BaseType := OrdTypeToBaseType[GetTypeData(SepiType.TypeInfo).OrdType]
  else if SepiType is TSepiInt64Type then
    BaseType := btInt64
  else if SepiType is TSepiFloatType then
    BaseType := FloatTypeToBaseType[TSepiFloatType(SepiType).FloatType]
  else if (SepiType is TSepiBooleanType) and
    (TSepiBooleanType(SepiType).BooleanKind = bkBoolean) then
    BaseType := btBoolean
  else if SepiType is TSepiStringType then
    BaseType := IsUnicodeToBaseType[TSepiStringType(SepiType).IsUnicode]
  else if SepiType is TSepiVariantType then
    BaseType := btVariant
  else
    Result := False;
end;

{*
  Récupère le type de base correspondant à un type Sepi
  @param SepiType   Type Sepi
  @return Type de base correspondant
*}
function SepiTypeToBaseType(SepiType: TSepiType): TSepiBaseType;
begin
  if not SepiTypeToBaseType(SepiType, Result) then
    raise ESepiCompilerError.CreateFmt(STypeIsNotBaseType, [SepiType.Name]);
end;

{*
  Initialise le tableau BaseTypeToDefaultTypeInfo
*}
procedure InitBaseTypeToDefaultTypeInfo;
begin
  BaseTypeToDefaultTypeInfo[btBoolean ] := TypeInfo(Boolean);
  BaseTypeToDefaultTypeInfo[btByte    ] := TypeInfo(Byte);
  BaseTypeToDefaultTypeInfo[btWord    ] := TypeInfo(Word);
  BaseTypeToDefaultTypeInfo[btDWord   ] := TypeInfo(LongWord);
  BaseTypeToDefaultTypeInfo[btShortint] := TypeInfo(Shortint);
  BaseTypeToDefaultTypeInfo[btSmallint] := TypeInfo(Smallint);
  BaseTypeToDefaultTypeInfo[btLongint ] := TypeInfo(Longint);
  BaseTypeToDefaultTypeInfo[btInt64   ] := TypeInfo(Int64);
  BaseTypeToDefaultTypeInfo[btSingle  ] := TypeInfo(Single);
  BaseTypeToDefaultTypeInfo[btDouble  ] := TypeInfo(Double);
  BaseTypeToDefaultTypeInfo[btExtended] := TypeInfo(Extended);
  BaseTypeToDefaultTypeInfo[btComp    ] := TypeInfo(Comp);
  BaseTypeToDefaultTypeInfo[btCurrency] := TypeInfo(Currency);
  BaseTypeToDefaultTypeInfo[btAnsiStr ] := TypeInfo(AnsiString);
  BaseTypeToDefaultTypeInfo[btWideStr ] := TypeInfo(WideString);
  BaseTypeToDefaultTypeInfo[btVariant ] := TypeInfo(Variant);
end;

{*
  Trouve le type Sepi par défaut correspondant à un type de base
  @param BaseType   Type de base
  @param SepiRoot   Racine Sepi
  @return Type Sepi correspondant
*}
function DefaultSepiTypeFor(BaseType: TSepiBaseType;
  SepiRoot: TSepiRoot): TSepiType;
begin
  Result := SepiRoot.FindType(BaseTypeToDefaultTypeInfo[BaseType]);
end;

{---------------------------------}
{ TSepiCustomExpressionPart class }
{---------------------------------}

{*
  Expression à laquelle est rattachée cette part d'expression
*}
function TSepiCustomExpressionPart.GetExpression: ISepiExpression;
begin
  Result := ISepiExpression(FExpression);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomExpressionPart.AttachTo(const Controller: IInterface);
var
  ExprController: ISepiExpression;
begin
  inherited;

  if Controller.QueryInterface(ISepiExpression, ExprController) = 0 then
  begin
    FExpression := Pointer(ExprController);
    FSepiRoot := ExprController.SepiRoot;
    FUnitCompiler := ExprController.UnitCompiler;
    FMethodCompiler := ExprController.MethodCompiler;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomExpressionPart.DetachFromController;
begin
  inherited;

  if Controller = nil then
  begin
    FExpression := nil;
    FSepiRoot := nil;
    FUnitCompiler := nil;
    FMethodCompiler := nil;
  end;
end;

{*
  Produit une erreur de compilation au niveau de cette expression
  @param Msg    Message de l'erreur
  @param Kind   Type d'erreur (défaut = ekError)
*}
procedure TSepiCustomExpressionPart.MakeError(const Msg: string;
  Kind: TSepiErrorKind = ekError);
begin
  Expression.MakeError(Msg, Kind);
end;

{------------------------------}
{ TSepiCustomDirectValue class }
{------------------------------}

{*
  [@inheritDoc]
*}
function TSepiCustomDirectValue.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if SameGUID(IID, ISepiReadableValue) and (not IsReadable) then
    Result := E_NOINTERFACE
  else if SameGUID(IID, ISepiWritableValue) and (not IsWritable) then
    Result := E_NOINTERFACE
  else if SameGUID(IID, ISepiAddressableValue) and (not IsAddressable) then
    Result := E_NOINTERFACE
  else
    Result := inherited QueryInterface(IID, Obj);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomDirectValue.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  if IsReadable then
    Expression.Attach(ISepiReadableValue, AsExpressionPart);
  if IsWritable then
    Expression.Attach(ISepiWritableValue, AsExpressionPart);
  if IsAddressable then
    Expression.Attach(ISepiAddressableValue, AsExpressionPart);
end;

{*
  [@inheritDoc]
*}
function TSepiCustomDirectValue.GetValueType: TSepiType;
begin
  Result := FValueType;
end;

{*
  Renseigne le type de la valeur
  @param AType   Type de la valeur
*}
procedure TSepiCustomDirectValue.SetValueType(AType: TSepiType);
begin
  FValueType := AType;
end;

{*
  [@inheritDoc]
*}
function TSepiCustomDirectValue.GetAddressType: TSepiType;
begin
  Result := SepiRoot.FindType('System.Pointer');
end;

{*
  [@inheritDoc]
*}
function TSepiCustomDirectValue.GetIsConstant: Boolean;
begin
  Result := FConstValuePtr <> nil;
end;

{*
  [@inheritDoc]
*}
function TSepiCustomDirectValue.GetConstValuePtr: Pointer;
begin
  Result := FConstValuePtr;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomDirectValue.CompileRead(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
begin
  Destination := CompileAsMemoryRef(Compiler, Instructions, TempVars);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomDirectValue.CompileWrite(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; Source: ISepiReadableValue);
var
  TempVars: TSepiTempVarsLifeManager;
  Destination, TempDest: TSepiMemoryReference;
  MoveInstr: TSepiAsmMove;
begin
  if Source.ValueType <> ValueType then
  begin
    MakeError(Format(STypeMismatch, [Source.ValueType.Name, ValueType.Name]));
    Exit;
  end;

  TempVars := TSepiTempVarsLifeManager.Create;
  try
    Destination := CompileAsMemoryRef(Compiler, Instructions, TempVars);
    TempDest := Destination;
    try
      Destination.Seal;
      Source.CompileRead(Compiler, Instructions, TempDest, TempVars);

      TempVars.EndAllLifes(Instructions.GetCurrentEndRef);

      if TempDest <> Destination then
      begin
        MoveInstr := TSepiAsmMove.Create(Compiler, ValueType);
        MoveInstr.SourcePos := Expression.SourcePos;
        MoveInstr.Destination.Assign(Destination);
        MoveInstr.Source.Assign(TempDest);
        Instructions.Add(MoveInstr);
      end;
    finally
      if TempDest <> Destination then
        TempDest.Free;
      Destination.Free;
    end;
  finally
    TempVars.EndAllLifes(Instructions.GetCurrentEndRef);
    TempVars.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomDirectValue.CompileLoadAddress(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
var
  MemoryRef: TSepiMemoryReference;
  LoadAddressInstr: TSepiAsmLoadAddress;
begin
  MemoryRef := CompileAsMemoryRef(Compiler, Instructions, TempVars);
  try
    if MemoryRef.CanRemoveDereference then
    begin
      Destination := TSepiMemoryReference.Clone(MemoryRef);
      Destination.RemoveDereference;
      Destination.Seal;
    end else
    begin
      TempVars.EndAllLifes(Instructions.GetCurrentEndRef);

      LoadAddressInstr := TSepiAsmLoadAddress.Create(Compiler);
      LoadAddressInstr.SourcePos := Expression.SourcePos;

      NeedDestination(Destination, AddressType, Compiler, TempVars,
        LoadAddressInstr.AfterRef);

      LoadAddressInstr.Destination.Assign(Destination);
      LoadAddressInstr.Source.Assign(MemoryRef);
      Instructions.Add(LoadAddressInstr);
    end;
  finally
    MemoryRef.Free;
  end;
end;

{--------------------------------}
{ TSepiCustomComputedValue class }
{--------------------------------}

{*
  [@inheritDoc]
*}
destructor TSepiCustomComputedValue.Destroy;
begin
  if ConstValuePtr <> nil then
    ValueType.DisposeValue(ConstValuePtr);

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomComputedValue.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;
  Expression.Attach(ISepiReadableValue, AsExpressionPart);
end;

{*
  Type de valeur
  @return Type de valeur
*}
function TSepiCustomComputedValue.GetValueType: TSepiType;
begin
  Result := FValueType;
end;

{*
  Renseigne le type de valeur
  @param AType   Type de valeur
*}
procedure TSepiCustomComputedValue.SetValueType(AType: TSepiType);
begin
  FValueType := AType;
end;

{*
  [@inheritDoc]
*}
function TSepiCustomComputedValue.GetIsConstant: Boolean;
begin
  Result := FConstValuePtr <> nil;
end;

{*
  [@inheritDoc]
*}
function TSepiCustomComputedValue.GetConstValuePtr: Pointer;
begin
  Result := FConstValuePtr;
end;

{*
  Alloue la constante et devient par là un ISepiConstantValue
  La constante est initialisée à 0
*}
procedure TSepiCustomComputedValue.AllocateConstant;
begin
  if FConstValuePtr = nil then
  begin
    GetMem(FConstValuePtr, ValueType.Size);
    FillChar(FConstValuePtr^, ValueType.Size, 0);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomComputedValue.CompileRead(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
begin
  if ConstValuePtr = nil then
    CompileCompute(Compiler, Instructions, Destination, TempVars)
  else
  begin
    Destination := TSepiMemoryReference.Create(Compiler, aoAcceptAllConsts,
      ValueType.Size);

    if IsZeroMemory(ConstValuePtr, ValueType.Size) then
      Destination.SetSpace(msZero)
    else if ValueType.NeedInit then
    begin
      Destination.SetSpace(Compiler.MakeUnnamedTrueConst(
        ValueType, ConstValuePtr^));
    end else
    begin
      Destination.SetSpace(msConstant);
      Destination.SetConstant(ConstValuePtr^);
    end;

    Destination.Seal;
  end;
end;

{---------------------}
{ TSepiTrueConstValue }
{---------------------}

{*
  Tronc commun des créations de constantes litérales
  @param AType   Type de la constante
*}
constructor TSepiTrueConstValue.Create(AType: TSepiType);
begin
  inherited Create;

  SetValueType(AType);
  ConstValuePtr := ValueType.NewValue;

  IsReadable := True;
end;

{*
  Crée une expression représentant une vraie constante
  @param Constant   Vraie constante représentée
*}
constructor TSepiTrueConstValue.Create(Constant: TSepiConstant);
begin
  inherited Create;

  FConstant := Constant;
  SetValueType(Constant.ConstType);
  ConstValuePtr := Constant.ValuePtr;

  IsReadable := True;
end;

{*
  Crée une constante litérale entière
  @param SepiRoot   Racine Sepi (pour trouver le type Sepi)
  @param Value      Valeur de la constante
*}
constructor TSepiTrueConstValue.Create(SepiRoot: TSepiRoot;
  Value: Int64);
var
  ATypeInfo: PTypeInfo;
begin
  if (Value < Int64(-MaxInt-1)) or (Value > Int64(MaxInt)) then
    ATypeInfo := TypeInfo(Int64)
  else if Value < 0 then
  begin
    case IntegerSize(Value) of
      1: ATypeInfo := TypeInfo(Shortint);
      2: ATypeInfo := TypeInfo(Smallint);
      4: ATypeInfo := TypeInfo(Longint);
    else
      Assert(False);
      ATypeInfo := TypeInfo(Int64);
    end;
  end else
  begin
    case CardinalSize(Value) of
      1: ATypeInfo := TypeInfo(Byte);
      2: ATypeInfo := TypeInfo(Word);
      4: ATypeInfo := TypeInfo(LongWord);
    else
      Assert(False);
      ATypeInfo := TypeInfo(Int64);
    end;
  end;

  Create(SepiRoot.FindType(ATypeInfo));

  Int64(ConstValuePtr^) := Value;
end;

{*
  Crée une constante litérale flottante
  @param SepiRoot   Racine Sepi (pour trouver le type Sepi)
  @param Value      Valeur de la constante
*}
constructor TSepiTrueConstValue.Create(SepiRoot: TSepiRoot;
  Value: Extended);
begin
  Create(SepiRoot.FindType(TypeInfo(Extended)));

  Extended(ConstValuePtr^) := Value;
end;

{*
  Crée une constante litérale caractère ANSI
  @param SepiRoot   Racine Sepi (pour trouver le type Sepi)
  @param Value      Valeur de la constante
*}
constructor TSepiTrueConstValue.Create(SepiRoot: TSepiRoot;
  Value: AnsiChar);
begin
  Create(SepiRoot.FindType(TypeInfo(AnsiChar)));

  AnsiChar(ConstValuePtr^) := Value;
end;

{*
  Crée une constante litérale caractère Unicode
  @param SepiRoot   Racine Sepi (pour trouver le type Sepi)
  @param Value      Valeur de la constante
*}
constructor TSepiTrueConstValue.Create(SepiRoot: TSepiRoot;
  Value: WideChar);
begin
  Create(SepiRoot.FindType(TypeInfo(WideChar)));

  WideChar(ConstValuePtr^) := Value;
end;

{*
  Crée une constante litérale chaîne ANSI
  @param SepiRoot   Racine Sepi (pour trouver le type Sepi)
  @param Value      Valeur de la constante
*}
constructor TSepiTrueConstValue.Create(SepiRoot: TSepiRoot;
  const Value: AnsiString);
begin
  Create(SepiRoot.FindType(TypeInfo(AnsiString)));

  AnsiString(ConstValuePtr^) := Value;
end;

{*
  Crée une constante litérale chaîne Unicode
  @param SepiRoot   Racine Sepi (pour trouver le type Sepi)
  @param Value      Valeur de la constante
*}
constructor TSepiTrueConstValue.Create(SepiRoot: TSepiRoot;
  const Value: WideString);
begin
  Create(SepiRoot.FindType(TypeInfo(WideString)));

  WideString(ConstValuePtr^) := Value;
end;

{*
  [@inheritDoc]
*}
destructor TSepiTrueConstValue.Destroy;
begin
  if FConstant = nil then
    ValueType.DisposeValue(ConstValuePtr);

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiTrueConstValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
begin
  Result := TSepiMemoryReference.Create(Compiler, aoAcceptAllConsts,
    ValueType.Size);
  try
    if IsZeroMemory(ConstValuePtr, ValueType.Size) then
      Result.SetSpace(msZero)
    else if Constant <> nil then
    begin
      Result.SetSpace(Constant);
    end else if ValueType.NeedInit then
    begin
      Result.SetSpace(Compiler.MakeUnnamedTrueConst(
        ValueType, ConstValuePtr^));
    end else
    begin
      Result.SetSpace(msConstant);
      Result.SetConstant(ConstValuePtr^);
    end;

    Result.Seal;
  except
    Result.Free;
    raise;
  end;
end;

{--------------------------}
{ TSepiVariableValue class }
{--------------------------}

{*
  Crée une valeur variable globale
  @param AVariable   Variable
*}
constructor TSepiVariableValue.Create(AVariable: TSepiVariable);
begin
  inherited Create;

  FVariable := AVariable;
  SetValueType(Variable.VarType);

  IsReadable := True;
  IsWritable := not Variable.IsConst;
  IsAddressable := True;
end;

{*
  [@inheritDoc]
*}
function TSepiVariableValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
begin
  Result := TSepiMemoryReference.Create(Compiler, [aoAcceptAddressedConst]);
  try
    Result.SetSpace(Variable);
    Result.Seal;
  except
    Result.Free;
    raise;
  end;
end;

{--------------------------}
{ TSepiLocalVarValue class }
{--------------------------}

{*
  Crée une valeur variable locale
  @param ALocalVar   LocalVar
*}
constructor TSepiLocalVarValue.Create(ALocalVar: TSepiLocalVar);
begin
  inherited Create;

  FLocalVar := ALocalVar;
  SetValueType(LocalVar.VarType);

  IsReadable := ValueType <> nil;
  IsWritable := (ValueType <> nil) and (not LocalVar.IsConstant);
  IsAddressable := True;
end;

{*
  [@inheritDoc]
*}
function TSepiLocalVarValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
begin
  Result := TSepiMemoryReference.Create(Compiler, [aoAcceptAddressedConst]);
  try
    Result.SetSpace(LocalVar);
    Result.Seal;
  except
    Result.Free;
    raise;
  end;
end;

{-------------------------}
{ TSepiCastOperator class }
{-------------------------}

constructor TSepiCastOperator.Create(DestType: TSepiType;
  const AOperand: ISepiExpression = nil);
begin
  inherited Create;

  SetValueType(DestType);
  FOperand := AOperand;
end;

{*
  Pliage des constantes
*}
procedure TSepiCastOperator.CollapseConsts;
var
  ConstOp: ISepiReadableValue;
  OpType: TSepiType;
begin
  // Handle nil constant
  if Supports(Operand, ISepiNilValue) then
  begin
    ConstValuePtr := @ZeroMemory;
    Exit;
  end;

  // If operand is not a constant value, exit
  if Operand.QueryInterface(ISepiReadableValue, ConstOp) <> 0 then
    Exit;
  if not ConstOp.IsConstant then
    Exit;

  // Can't collapse a cast if operand type is unknown
  OpType := ConstOp.ValueType;
  if OpType = nil then
    Exit;

  // Can't collapse a cast on a need-init-type (unless operand is zero-memory)
  if (OpType.NeedInit or ValueType.NeedInit) and
    (not IsZeroMemory(ConstOp.ConstValuePtr, OpType.Size)) then
    Exit;

  // Do the job
  ConstValuePtr := ConstOp.ConstValuePtr;
end;

{*
  [@inheritDoc]
*}
function TSepiCastOperator.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
begin
  Assert(False);
  Result := nil;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCastOperator.CompileRead(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
begin
  (Operand as ISepiReadableValue).CompileRead(Compiler, Instructions,
    Destination, TempVars);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCastOperator.CompileWrite(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; Source: ISepiReadableValue);
begin
  (Operand as ISepiWritableValue).CompileWrite(Compiler, Instructions,
    Source);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCastOperator.CompileLoadAddress(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
begin
  (Operand as ISepiAddressableValue).CompileLoadAddress(Compiler, Instructions,
    Destination, TempVars);
end;

{*
  Complète le transtypage
*}
procedure TSepiCastOperator.Complete;
var
  OpType: TSepiType;
begin
  Assert(Operand <> nil);

  OpType := nil;

  if Supports(Operand, ISepiNilValue) then
    IsReadable := True;

  if Supports(Operand, ISepiReadableValue) then
  begin
    IsReadable := True;
    OpType := (Operand as ISepiReadableValue).ValueType;
  end;

  if Supports(Operand, ISepiWritableValue) then
  begin
    IsWritable := (OpType = nil) or
      ((Operand as ISepiReadableValue).ValueType = OpType);
    if OpType = nil then
      OpType := (Operand as ISepiReadableValue).ValueType;
  end;

  if Supports(Operand, ISepiAddressableValue) then
    IsAddressable := True;

  if (OpType <> nil) and (OpType.Size <> ValueType.Size) then
  begin
    if (OpType.Size < ValueType.Size) or (not ForceCast) then
    begin
      MakeError(Format(SInvalidCast, [OpType.Name, ValueType.Name]));

      IsReadable := False;
      IsWritable := False;
      IsAddressable := False;

      Exit;
    end;
  end;

  CollapseConsts;
end;

{*
  Transtype une expression
  @param DestType     Type de destination
  @param Expression   Expression à transtyper
  @return Expression transtypée
*}
class function TSepiCastOperator.CastExpression(DestType: TSepiType;
  const Expression: ISepiExpression;
  ForceCast: Boolean = False): ISepiExpression;
var
  CastOp: TSepiCastOperator;
  CastOpIntf: ISepiExpressionPart;
begin
  CastOp := TSepiCastOperator.Create(DestType, Expression);
  CastOpIntf := CastOp;
  CastOp.ForceCast := ForceCast;
  CastOp.Complete;

  Result := TSepiExpression.Create(Expression);

  if CastOp.IsReadable then
    Result.Attach(ISepiReadableValue, CastOpIntf);
  if CastOp.IsWritable then
    Result.Attach(ISepiWritableValue, CastOpIntf);
  if CastOp.IsAddressable then
    Result.Attach(ISepiAddressableValue, CastOpIntf);
end;

{-----------------------------}
{ TSepiConvertOperation class }
{-----------------------------}

{*
  Crée une opération de conversion
  @param DestType   Type de destination
  @param ASource    Expression source
*}
constructor TSepiConvertOperation.Create(DestType: TSepiType;
  const ASource: ISepiReadableValue = nil);
begin
  inherited Create;

  SetValueType(DestType);
  FSource := ASource;
end;

{*
  Pliage des constantes
*}
procedure TSepiConvertOperation.CollapseConsts;
var
  SrcType: TSepiType;
  DestBase, SrcBase: TSepiBaseType;
begin
  if not Source.IsConstant then
    Exit;

  SrcType := Source.ValueType;

  // Only CVRT convertions can be collapsed
  if (not SepiTypeToBaseType(ValueType, DestBase)) or
    (not SepiTypeToBaseType(SrcType, SrcBase)) then
    Exit;

  AllocateConstant;
  Convert(DestBase, SrcBase, ConstValuePtr^, Source.ConstValuePtr^);
end;

{*
  [@inheritDoc]
*}
procedure TSepiConvertOperation.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  SrcType: TSepiType;
  DestBase, SrcBase: TSepiBaseType;
  SourceDestination: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  ConvertInstr: TSepiAsmConvert;
begin
  SrcType := Source.ValueType;

  if SrcType is TSepiClass then
  begin
    if ValueType is TSepiClass then
    begin
      // This is a pure assignment
      Source.CompileRead(Compiler, Instructions, Destination, TempVars);
    end else
    begin
      // Here we must take into account the offset of the interface hidden field
      Assert(ValueType is TSepiInterface);

      // TODO Convertir une classe en interface
      raise EAssertionFailed.Create('Can''t convert class to interface');
    end;
  end else if SrcType is TSepiInterface then
  begin
    Assert(ValueType is TSepiInterface);

    // Pure assignment
    Source.CompileRead(Compiler, Instructions, Destination, TempVars);
  end else
  begin
    DestBase := SepiTypeToBaseType(ValueType);
    SrcBase := SepiTypeToBaseType(SrcType);

    if DestBase = SrcBase then
    begin
      // Pure assignment
      Source.CompileRead(Compiler, Instructions, Destination, TempVars);
    end else
    begin
      // CVRT convertion

      SourceDestination := nil;
      try
        // Read source
        SrcTempVars := TSepiTempVarsLifeManager.Create;
        try
          Source.CompileRead(Compiler, Instructions, SourceDestination,
            SrcTempVars);
        finally
          SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
          SrcTempVars.Free;
        end;

        // Make CVRT instruction
        ConvertInstr := TSepiAsmConvert.Create(Compiler, DestBase, SrcBase);
        ConvertInstr.SourcePos := Expression.SourcePos;

        NeedDestination(Destination, ValueType, Compiler, TempVars,
          ConvertInstr.AfterRef);

        ConvertInstr.Destination.Assign(Destination);
        ConvertInstr.Source.Assign(SourceDestination);
        Instructions.Add(ConvertInstr);
      finally
        SourceDestination.Free;
      end;
    end;
  end;
end;

{*
  Complète l'opération
*}
procedure TSepiConvertOperation.Complete;
begin
  Assert(FSource <> nil);

  if not ConvertionExists(ValueType, Source.ValueType) then
  begin
    MakeError(Format(STypeMismatch, [ValueType, Source.ValueType]));
    Exit;
  end;

  CollapseConsts;
end;

{*
  Teste si une conversion existe et est possible depuis un type vers un autre
  @param DestType   Type de destination
  @param SrcType    Type source
  @return True si une conversion est possible, False sinon
*}
class function TSepiConvertOperation.ConvertionExists(
  DestType, SrcType: TSepiType): Boolean;
var
  DestBase, SrcBase: TSepiBaseType;
begin
  // Trivial case: types are equal
  if DestType = SrcType then
  begin
    Result := True;
    Exit;
  end;

  // Convertions between classes and/or interfaces
  if SrcType is TSepiClass then
  begin
    if DestType is TSepiClass then
      Result := TSepiClass(SrcType).ClassInheritsFrom(TSepiClass(DestType))
    else if DestType is TSepiInterface then
      Result := TSepiClass(SrcType).ClassImplementsInterface(
        TSepiInterface(DestType))
    else
      Result := False;

    Exit;
  end else if SrcType is TSepiInterface then
  begin
    if DestType is TSepiInterface then
      Result := TSepiInterface(SrcType).IntfInheritsFrom(
        TSepiInterface(DestType))
    else
      Result := False;

    Exit;
  end;

  // Convertions via the CVRT instruction
  if SepiTypeToBaseType(DestType, DestBase) and
    SepiTypeToBaseType(SrcType, SrcBase) and
    SepiRuntimeOperations.ConversionExists(SrcBase, DestBase) then
  begin
    Result := True;
    Exit;
  end;

  // Nothing found
  Result := False;
end;

{*
  Convertit une valeur
  @param DestType   Type de destination
  @param Value      Valeur à convertir
  @return Valeur convertie
*}
class function TSepiConvertOperation.ConvertValue(DestType: TSepiType;
  const Value: ISepiReadableValue): ISepiReadableValue;
var
  ConvertOp: TSepiConvertOperation;
  NewExpr: ISepiExpression;
begin
  ConvertOp := TSepiConvertOperation.Create(DestType, Value);
  ConvertOp.Complete;
  Result := ConvertOp;

  NewExpr := TSepiExpression.Create(Value as ISepiExpression);
  NewExpr.Attach(ISepiReadableValue, Result);
end;

{-------------------------------------}
{ TSepiArithmeticLogicOperation class }
{-------------------------------------}

{*
  Crée l'opération
  @param AOperation    Opération
*}
constructor TSepiArithmeticLogicOperation.Create(AOperation: TSepiOperation);
begin
  inherited Create;
  FOperation := AOperation;
end;

{*
  Produit une erreur Opération non applicable à ce type d'opérande
*}
procedure TSepiArithmeticLogicOperation.ErrorTypeNotApplicable;
begin
  MakeError(SOperationNotApplicableToType);
end;

{*
  Obtient l'OpCode correspondant à cette opération
  @param SelfOp   Si True, utilise une instruction réflexive
  @return OpCode de l'opération
*}
function TSepiArithmeticLogicOperation.GetOpCode(SelfOp: Boolean): TSepiOpCode;
const
  OperationToOpCode: array[TSepiOperation] of TSepiOpCode = (
    ocOtherAdd, ocOtherSubtract, ocOtherMultiply, ocOtherDivide, ocOtherIntDiv,
    ocOtherModulus, ocOtherShl, ocOtherShr, ocOtherAnd, ocOtherOr, ocOtherXor,
    ocNope, ocOtherNeg, ocOtherNot, ocCompEquals, ocCompNotEquals, ocCompLower,
    ocCompLowerEq, ocCompGreater, ocCompGreaterEq
  );
begin
  Result := OperationToOpCode[Operation];
  if SelfOp and (not Operation in opComparisonOps) then
    Dec(Result, ocOtherAdd-ocSelfAdd);
end;

{---------------------------}
{ TSepiUnaryOperation class }
{---------------------------}

{*
  Vérification de type
*}
procedure TSepiUnaryOperation.CheckType;
begin
  if Operation = opNot then
  begin
    if not ((ValueType is TSepiIntegerType) or
      (ValueType is TSepiBooleanType)) then
      ErrorTypeNotApplicable;
  end else
  begin
    if not (ValueType.Kind in [tkInteger, tkFloat, tkVariant, tkInt64]) then
      ErrorTypeNotApplicable;
  end;
end;

{*
  Pliage des constantes
*}
procedure TSepiUnaryOperation.CollapseConsts;
begin
  if not Operand.IsConstant then
    Exit;

  AllocateConstant;
  UnaryOp(GetOpCode(False), SepiTypeToBaseType(ValueType),
    ConstValuePtr^, Operand.ConstValuePtr^);
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnaryOperation.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  TempDest: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  SrcTempVar: TSepiLocalVar;
  OpInstr: TSepiAsmOperation;
begin
  TempDest := Destination;
  try
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      // Read operand into destination or temporary destination
      Operand.CompileRead(Compiler, Instructions, TempDest, SrcTempVars);

      // Complicated trick to optimize instruction count and size
      if Destination = nil then
      begin
        SrcTempVar := SrcTempVars.FindVarFromMemoryRef(TempDest);

        if SrcTempVar = nil then
        begin
          NeedDestination(Destination, ValueType, Compiler, TempVars,
            Instructions.GetCurrentEndRef);
        end else
        begin
          TempVars.Acquire(SrcTempVars.Extract(SrcTempVar));
          Destination := TempDest;
        end;
      end;
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Create operation instruction
    OpInstr := TSepiAsmOperation.Create(Compiler,
      GetOpCode(TempDest = Destination), SepiTypeToBaseType(ValueType));
    OpInstr.SourcePos := Expression.SourcePos;

    OpInstr.Destination.Assign(Destination);
    if TempDest <> Destination then
      OpInstr.Left.Assign(TempDest);

    Instructions.Add(OpInstr);
  finally
    if TempDest <> Destination then
      TempDest.Free;
  end;
end;

{*
  Complète l'opération
  L'opération doit avoir été attachée à une expression auparavant, pour pouvoir
  bénéficier du contexte de celle-ci.
*}
procedure TSepiUnaryOperation.Complete;
begin
  Assert(Expression <> nil);
  Assert(Operand <> nil);

  SetValueType(Operand.ValueType);
  CheckType;
  CollapseConsts;
end;

{----------------------------}
{ TSepiBinaryOperation class }
{----------------------------}

{*
  Crée l'opération
  @param AOperation     Opération
  @param AAutoConvert   Valeur initiale de AutoConvert (défaut = True)
*}
constructor TSepiBinaryOperation.Create(AOperation: TSepiOperation;
  AAutoConvert: Boolean = True);
begin
  inherited Create(AOperation);

  FAutoConvert := AAutoConvert;
end;

{*
  Vérification de types pour des types non de base
  @param LeftType    Type de gauche
  @param RightType   Type de droite
  @return Type commun
*}
function TSepiBinaryOperation.CheckNonBaseTypes(
  LeftType, RightType: TSepiType): TSepiBaseType;
begin
  { The only valid operation on non-base types is equal/not-equal, if and
    only if operands are compatible with each other. }

  // Only equal/not-equal comparisons
  if not (Operation in [opCmpEQ, opCmpNE]) then
    ErrorTypeNotApplicable;

  // Only same type class
  if LeftType.ClassType <> RightType.ClassType then
    ErrorTypeMismatch;

  { Pointers/classes/interfaces/meta-classes are always compatible for
    equality test. }
  if (LeftType is TSepiPointerType) or (LeftType is TSepiClass) or
    (LeftType is TSepiInterface) or (LeftType is TSepiMetaClass) then
    // OK
  else if LeftType is TSepiEnumType then
  begin
    // Enum types must have the same base type
    if TSepiEnumType(LeftType).BaseType <>
      TSepiEnumType(RightType).BaseType then
      ErrorTypeMismatch;
    // OK
  end else if LeftType is TSepiSetType then
  begin
    // Set types must have the same component type
    if TSepiSetType(LeftType).CompType <> TSepiSetType(RightType).CompType then
      ErrorTypeMismatch;
    // OK
  end else if LeftType is TSepiMethodRefType then
  begin
    // Method-ref types must have equal signatures
    if not TSepiMethodRefType(LeftType).Signature.Equals(
      TSepiMethodRefType(RightType).Signature) then
      ErrorTypeMismatch;
    // OK
  end else
    ErrorTypeNotApplicable;

  // Find correct base type
  case LeftType.Size of
    1: Result := btByte;
    2: Result := btWord;
    4: Result := btDWord;
    8: Result := btInt64;
  else
    ErrorTypeNotApplicable;
    Result := btByte;
  end;
end;

{*
  Vérification de types pour une opération shift
  @param LeftType    Type de gauche
  @param RightType   Type de droites
  @param BaseTypes   Types de gauche et droite
*}
procedure TSepiBinaryOperation.CheckTypesShift(
  LeftBaseType, RightBaseType: TSepiBaseType; BaseTypes: TSepiBaseTypes);
begin
  { Shift operations must have either 2 Variant operands, or 2 integer
    operands. In this case, the right operand must be converted to a Byte. }

  if btVariant in BaseTypes then
  begin
    { There is at least one Variant operand: convert the other one to Variant
      as well. }
    if LeftBaseType <> btVariant then
      ConvertLeftOperand(btVariant)
    else if RightBaseType <> btVariant then
      ConvertRightOperand(btVariant);

    SetValueType(LeftOperand.ValueType);
  end else
  begin
    { There is no Variant operand: both must be integer. }
    if not (BaseTypes <= btIntegers) then
      ErrorTypeNotApplicable;

    if RightBaseType <> btByte then
      ConvertRightOperand(btByte);

    SetValueType(LeftOperand.ValueType);
  end;
end;

{*
  Vérification de types pour une opération arithmétique
  @param LeftType    Type de gauche
  @param RightType   Type de droites
  @param BaseTypes   Types de gauche et droite
*}
procedure TSepiBinaryOperation.CheckTypesArithmetic(
  LeftBaseType, RightBaseType: TSepiBaseType; BaseTypes: TSepiBaseTypes);
var
  CommonType: TSepiBaseType;
begin
  { Arithmetic operations must have equal-type operands, maybe via conversion,
    and the result type is the common type. }

  if not SmallestCommonType(LeftBaseType, RightBaseType, CommonType) then
    ErrorTypeMismatch;

  // Some operations are not applicable to some types
  if CommonType = btBoolean then
  begin
    if not (Operation in [opAnd, opOr, opXor]) then
      ErrorTypeNotApplicable;
  end else if CommonType in btIntegers then
  begin
    if Operation = opDivide then
      ErrorTypeNotApplicable;
  end else if CommonType in btFloats then
  begin
    if Operation in [opIntDivide, opModulus] then
      ErrorTypeNotApplicable;
  end else if CommonType in btStrings then
  begin
    if Operation <> opAdd then
      ErrorTypeNotApplicable;
  end;

  // Cast operands
  if LeftBaseType <> CommonType then
    ConvertLeftOperand(CommonType);
  if RightBaseType <> CommonType then
    ConvertRightOperand(CommonType);

  // Set expression type
  if LeftBaseType = CommonType then
    SetValueType(LeftOperand.ValueType)
  else if RightBaseType = CommonType then
    SetValueType(RightOperand.ValueType)
  else
    SetValueType(DefaultSepiTypeFor(CommonType));
end;

{*
  Vérification de types pour une opération de comparaison
  @param LeftType    Type de gauche
  @param RightType   Type de droites
  @param BaseTypes   Types de gauche et droite
*}
procedure TSepiBinaryOperation.CheckTypesComparison(
  LeftBaseType, RightBaseType: TSepiBaseType; BaseTypes: TSepiBaseTypes);
var
  CommonType: TSepiBaseType;
begin
  { Comparison operations must have equal-type operands, maybe via conversion,
    and the result type is always Boolean. }

  if not SmallestCommonType(LeftBaseType, RightBaseType, CommonType) then
    ErrorTypeMismatch;

  if LeftBaseType <> CommonType then
    ConvertLeftOperand(CommonType);
  if RightBaseType <> CommonType then
    ConvertRightOperand(CommonType);

  SetValueType(DefaultSepiTypeFor(btBoolean));
end;

{*
  Vérification de types
*}
procedure TSepiBinaryOperation.CheckTypes;
var
  LeftType, RightType, CommonType: TSepiType;
  LeftBaseType, RightBaseType: TSepiBaseType;
  BaseTypes: TSepiBaseTypes;
begin
  LeftType := LeftOperand.ValueType;
  RightType := RightOperand.ValueType;

  // If no auto-convertion, types must be strictly equal
  if (not AutoConvert) and (LeftType <> RightType) then
    ErrorTypeMismatch;

  // Non-base types must be checked first, and then cast to a base type
  if not (SepiTypeToBaseType(LeftType, LeftBaseType) and
    SepiTypeToBaseType(RightType, RightBaseType)) then
  begin
    LeftBaseType := CheckNonBaseTypes(LeftType, RightType);
    RightBaseType := LeftBaseType;
    CommonType := DefaultSepiTypeFor(LeftBaseType);

    LeftOperand := TSepiCastOperator.CastExpression(CommonType,
      LeftOperand as ISepiExpression) as ISepiReadableValue;
    RightOperand := TSepiCastOperator.CastExpression(CommonType,
      RightOperand as ISepiExpression) as ISepiReadableValue;
  end;

  // Check base types compatibility
  BaseTypes := [];
  Include(BaseTypes, LeftBaseType);
  Include(BaseTypes, RightBaseType);

  if Operation in [opShiftLeft, opShiftRight] then
    CheckTypesShift(LeftBaseType, RightBaseType, BaseTypes)
  else if Operation in opComparisonOps then
    CheckTypesComparison(LeftBaseType, RightBaseType, BaseTypes)
  else
    CheckTypesArithmetic(LeftBaseType, RightBaseType, BaseTypes);
end;

{*
  Pliage des constantes
*}
procedure TSepiBinaryOperation.CollapseConsts;
var
  OpCode: TSepiOpCode;
  VarType: TSepiBaseType;
begin
  if (not LeftOperand.IsConstant) or (not RightOperand.IsConstant) then
    Exit;

  AllocateConstant;

  OpCode := OperationToOpCode[Operation];
  VarType := SepiTypeToBaseType(LeftOperand.ValueType);

  if Operation in opComparisonOps then
  begin
    Compare(OpCode, VarType, ConstValuePtr^,
      LeftOperand.ConstValuePtr^, RightOperand.ConstValuePtr^)
  end else
  begin
    BinaryOp(OpCode, VarType, ConstValuePtr^,
      LeftOperand.ConstValuePtr^, RightOperand.ConstValuePtr^);
  end;
end;

{*
  Produit une erreur Types incompatibles
*}
procedure TSepiBinaryOperation.ErrorTypeMismatch;
begin
  MakeError(Format(STypeMismatch,
    [LeftOperand.ValueType.Name, RightOperand.ValueType.Name]));
end;

{*
  Type Sepi par défaut pour un type de base
  @param BaseType   Type de base
  @return Type Sepi par défaut correspondant
*}
function TSepiBinaryOperation.DefaultSepiTypeFor(
  BaseType: TSepiBaseType): TSepiType;
begin
  Result := SepiExpressions.DefaultSepiTypeFor(BaseType, SepiRoot);
end;

{*
  Convertit l'opérande de gauche
  @param NewType   Type de base en lequel convertir l'opérande
  @return Nouveau type Sepi de l'opérande
*}
function TSepiBinaryOperation.ConvertLeftOperand(
  NewType: TSepiBaseType): TSepiType;
begin
  Result := DefaultSepiTypeFor(NewType);
  LeftOperand := TSepiConvertOperation.ConvertValue(Result, LeftOperand);
end;

{*
  Convertit l'opérande de droite
  @param NewType   Type de base en lequel convertir l'opérande
  @return Nouveau type Sepi de l'opérande
*}
function TSepiBinaryOperation.ConvertRightOperand(
  NewType: TSepiBaseType): TSepiType;
begin
  Result := DefaultSepiTypeFor(NewType);
  RightOperand := TSepiConvertOperation.ConvertValue(Result, RightOperand);
end;

{*
  [@inheritDoc]
*}
procedure TSepiBinaryOperation.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  LeftMemory, RightMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  OpInstr: TSepiAsmOperation;
  CmpInstr: TSepiAsmCompare;
begin
  LeftMemory := nil;
  RightMemory := nil;
  try
    // Read operands
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      LeftOperand.CompileRead(Compiler, Instructions, LeftMemory, SrcTempVars);
      RightOperand.CompileRead(Compiler, Instructions, RightMemory,
        SrcTempVars);
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Make instruction
    if Operation in opComparisonOps then
    begin
      CmpInstr := TSepiAsmCompare.Create(Compiler, GetOpCode(False),
        SepiTypeToBaseType(LeftOperand.ValueType));
      CmpInstr.SourcePos := Expression.SourcePos;

      NeedDestination(Destination, ValueType, Compiler, TempVars,
        CmpInstr.AfterRef);

      CmpInstr.Destination.Assign(Destination);
      CmpInstr.Left.Assign(LeftMemory);
      CmpInstr.Right.Assign(RightMemory);

      Instructions.Add(CmpInstr);
    end else
    begin
      OpInstr := TSepiAsmOperation.Create(Compiler, GetOpCode(False),
        SepiTypeToBaseType(ValueType));
      OpInstr.SourcePos := Expression.SourcePos;

      NeedDestination(Destination, ValueType, Compiler, TempVars,
        OpInstr.AfterRef);

      OpInstr.Destination.Assign(Destination);
      OpInstr.Left.Assign(LeftMemory);
      OpInstr.Right.Assign(RightMemory);

      Instructions.Add(OpInstr);
    end;
  finally
    LeftMemory.Free;
    RightMemory.Free;
  end;
end;

{*
  Complète l'opération
  L'opération doit avoir été attachée à une expression auparavant, pour pouvoir
  bénéficier du contexte de celle-ci.
*}
procedure TSepiBinaryOperation.Complete;
begin
  Assert(Expression <> nil);
  Assert((LeftOperand <> nil) and (RightOperand <> nil));

  CheckTypes;
  CollapseConsts;
end;

{*
  Trouve le plus petit type commun à deux types de base
  @param LeftType     Type de gauche
  @param RightType    Type de droite
  @param CommonType   En sortie : Type commun
  @return True si un type commun a pu être trouvé, False sinon
*}
class function TSepiBinaryOperation.SmallestCommonType(
  LeftType, RightType: TSepiBaseType; out CommonType: TSepiBaseType): Boolean;
const
  NumResults: array[btByte..btCurrency, btByte..btCurrency] of TSepiBaseType = (
    { btByte, btWord, btDWord, btShortint, btSmallint, btLongint,
        btInt64, btSingle, btDouble, btExtended, btComp, btCurrency }
// btByte
    (btByte, btWord, btDWord, btSmallint, btSmallint, btLongint,
      btInt64, btSingle, btDouble, btExtended, btComp, btCurrency),
// btWord
    (btWord, btWord, btDWord, btLongint, btLongint, btLongint,
      btInt64, btSingle, btDouble, btExtended, btComp, btCurrency),
// btDWord
    (btDWord, btDWord, btDWord, btInt64, btInt64, btInt64,
      btInt64, btSingle, btDouble, btExtended, btComp, btCurrency),
// btShortint
    (btSmallint, btLongint, btInt64, btShortint, btSmallint, btLongint,
      btInt64, btSingle, btDouble, btExtended, btComp, btCurrency),
// btSmallint
    (btSmallint, btLongint, btInt64, btSmallint, btSmallint, btLongint,
      btInt64, btSingle, btDouble, btExtended, btComp, btCurrency),
// btLongint
    (btLongint, btLongint, btInt64, btLongint, btLongint, btLongint,
      btInt64, btSingle, btDouble, btExtended, btComp, btCurrency),
// btInt64
    (btInt64, btInt64, btInt64, btInt64, btInt64, btInt64,
      btInt64, btSingle, btDouble, btExtended, btComp, btDouble),
// btSingle
    (btSingle, btSingle, btSingle, btSingle, btSingle, btSingle,
      btSingle, btSingle, btDouble, btExtended, btDouble, btDouble),
// btDouble
    (btDouble, btDouble, btDouble, btDouble, btDouble, btDouble,
      btDouble, btDouble, btDouble, btExtended, btDouble, btDouble),
// btExtended
    (btExtended, btExtended, btExtended, btExtended, btExtended, btExtended,
      btExtended, btExtended, btExtended, btExtended, btExtended, btExtended),
// btComp
    (btComp, btComp, btComp, btComp, btComp, btComp,
      btDouble, btDouble, btDouble, btExtended, btComp, btDouble),
// btCurrency
    (btCurrency, btCurrency, btCurrency, btCurrency, btCurrency, btCurrency,
      btDouble, btDouble, btDouble, btExtended, btDouble, btCurrency)
  );
var
  Types: TSepiBaseTypes;
begin
  Types := [LeftType, RightType];

  if btVariant in Types then
  begin
    // Variant + anything = Variant
    CommonType := btVariant;
  end else if btBoolean in Types then
  begin
    // Boolean only matches with Boolean
    if Types <> [btBoolean] then
    begin
      Result := False;
      Exit;
    end;

    CommonType := btBoolean;
  end else if Types * [btAnsiStr, btWideStr] <> [] then
  begin
    // Strings only matches with themselves - prefer wide string
    if not (Types <= [btAnsiStr, btWideStr]) then
    begin
      Result := False;
      Exit;
    end;

    if btWideStr in Types then
      CommonType := btWideStr
    else
      CommonType := btAnsiStr;
  end else
  begin
    // For numbers, use the table
    CommonType := NumResults[LeftType, RightType];
  end;

  Result := True;
end;

{---------------------------}
{ TSepiAddressOfValue class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiAddressOfValue.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
begin
   Operand.CompileLoadAddress(Compiler, Instructions, Destination, TempVars);
end;

{*
  Complète l'opérateur d'adressage
*}
procedure TSepiAddressOfValue.Complete;
begin
  Assert(Operand <> nil);

  SetValueType(Operand.AddressType);
end;

{-----------------------------}
{ TSepiDereferenceValue class }
{-----------------------------}

{*
  [@inheritDoc]
*}
function TSepiDereferenceValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
var
  OperandMemory: TSepiMemoryReference;
begin
  OperandMemory := nil;
  try
    Operand.CompileRead(Compiler, Instructions, OperandMemory, TempVars);
    Result := TSepiMemoryReference.Clone(OperandMemory);
    try
      Result.AddOperation(adSimple);
      Result.Seal;
    except
      Result.Free;
      raise;
    end;
  finally
    OperandMemory.Free;
  end;
end;

{*
  Complète l'opérateur de déréférencement
*}
procedure TSepiDereferenceValue.Complete;
var
  OpType: TSepiType;
begin
  Assert(Operand <> nil);

  OpType := Operand.ValueType;
  if OpType is TSepiPointerType then
    SetValueType(TSepiPointerType(OpType).PointTo)
  else
    MakeError(SNeedPointerType);
end;

{-------------------------------}
{ TSepiTypeOperationValue class }
{-------------------------------}

{*
  Crée l'opération
  @param AOperation         Opération
  @param ANilIfNoTypeInfo   Si True, TypeInfo peut renvoyer nil
*}
constructor TSepiTypeOperationValue.Create(AOperation: TSepiTypeOperation;
  ANilIfNoTypeInfo: Boolean);
begin
  inherited Create;

  FOperation := AOperation;
  FNilIfNoTypeInfo := ANilIfNoTypeInfo;
end;

{*
  Vérifie si l'opérande est valide
*}
procedure TSepiTypeOperationValue.CheckOperand;
var
  OpType: TSepiType;
begin
  OpType := Operand.ExprType;

  case Operation of
    toTypeInfo:
    begin
      if (not NilIfNoTypeInfo) and (OpType.TypeInfo = nil) then
        MakeError(STypeHasNoTypeInfo);
    end;
    toLowerBound, toHigherBound:
    begin
      if not ((OpType is TSepiOrdType) or (OpType is TSepiInt64Type) or
        (OpType is TSepiArrayType)) then
        MakeError(SOrdinalOrArrayTypeRequired);
    end;
    toLength:
    begin
      if not (OpType is TSepiArrayType) then
        MakeError(SArrayTypeRequired);
    end;
  else
    Assert(False);
  end;
end;

{*
  Complète un SizeOf
*}
procedure TSepiTypeOperationValue.CompleteSizeOf;
begin
  SetValueType(SepiRoot.FindType(TypeInfo(Integer)));
  AllocateConstant;

  Integer(ConstValuePtr^) := Operand.ExprType.Size;
end;

{*
  Complète un TypeInfo
*}
procedure TSepiTypeOperationValue.CompleteTypeInfo;
begin
  SetValueType(SepiRoot.FindType('System.Pointer'));

  if Operand.ExprType.TypeInfo = nil then
    AllocateConstant; // zero-initialized
end;

{*
  Complète une borne de tableau
*}
procedure TSepiTypeOperationValue.CompleteArrayBound;
var
  ArrayType: TSepiArrayType;
  Bound: Integer;
begin
  ArrayType := Operand.ExprType as TSepiArrayType;

  SetValueType(ArrayType.IndexType);
  AllocateConstant;

  case Operation of
    toLowerBound:  Bound := ArrayType.LowerBound;
    toHigherBound: Bound := ArrayType.HigherBound;
    toLength:      Bound := ArrayType.ArrayLength;
  else
    Assert(False);
    Exit;
  end;

  Move(Bound, ConstValuePtr^, ValueType.Size);
end;

{*
  Complète une borne d'ordinal
*}
procedure TSepiTypeOperationValue.CompleteOrdinalBound;
var
  Int64Type: TSepiInt64Type;
  OrdType: TSepiOrdType;
  OrdBound: Integer;
begin
  SetValueType(Operand.ExprType);
  AllocateConstant;

  if Operand.ExprType is TSepiInt64Type then
  begin
    Int64Type := TSepiInt64Type(Operand.ExprType);

    if Operation = toLowerBound then
      Int64(ConstValuePtr^) := Int64Type.MinValue
    else
      Int64(ConstValuePtr^) := Int64Type.MaxValue;
  end else
  begin
    OrdType := Operand.ExprType as TSepiOrdType;

    if Operation = toLowerBound then
      OrdBound := OrdType.MinValue
    else
      OrdBound := OrdType.MaxValue;

    Move(OrdBound, ConstValuePtr^, OrdType.Size);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiTypeOperationValue.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  OpType: TSepiType;
  GetTypeInfoInstr: TSepiAsmGetRunInfo;
begin
  OpType := Operand.ExprType;

  Assert((Operation = toTypeInfo) and (OpType.TypeInfo <> nil));

  GetTypeInfoInstr := TSepiAsmGetRunInfo.Create(Compiler, ocGetTypeInfo);
  GetTypeInfoInstr.SourcePos := Expression.SourcePos;

  NeedDestination(Destination, ValueType, Compiler, TempVars,
    Instructions.GetCurrentEndRef);

  GetTypeInfoInstr.Destination.Assign(Destination);
  GetTypeInfoInstr.SetReference(OpType);
  Instructions.Add(GetTypeInfoInstr);
end;

{*
  Complète l'opération
  L'opération doit avoir été attachée à une expression auparavant, pour pouvoir
  bénéficier du contexte de celle-ci.
*}
procedure TSepiTypeOperationValue.Complete;
begin
  Assert(Operand <> nil);

  CheckOperand;

  case Operation of
    toSizeOf: CompleteSizeOf;
    toTypeInfo: CompleteTypeInfo;
    toLength: CompleteArrayBound;
  else
    if Operand.ExprType is TSepiArrayType then
      CompleteArrayBound
    else
      CompleteOrdinalBound;
  end;
end;

{---------------------}
{ TSepiNilValue class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TSepiNilValue.AttachToExpression(const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;
  Expression.Attach(ISepiNilValue, AsExpressionPart);
end;

{---------------------------}
{ TSepiMetaExpression class }
{---------------------------}

{*
  Crée une expression représentant un meta
  @param AMeta   Meta représenté
*}
constructor TSepiMetaExpression.Create(AMeta: TSepiMeta);
begin
  inherited Create;

  FMeta := AMeta;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaExpression.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;
  Expression.Attach(ISepiMetaExpression, AsExpressionPart);
end;

{*
  Meta
  @return Meta
*}
function TSepiMetaExpression.GetMeta: TSepiMeta;
begin
  Result := FMeta;
end;

{---------------------------}
{ TSepiTypeExpression class }
{---------------------------}

{*
  Crée une expression représentant un type
  @param AType   Type représenté
*}
constructor TSepiTypeExpression.Create(AType: TSepiType);
begin
  inherited Create;

  FType := AType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiTypeExpression.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;
  Expression.Attach(ISepiTypeExpression, AsExpressionPart);
end;

{*
  Type
  @return Type
*}
function TSepiTypeExpression.GetType: TSepiType;
begin
  Result := FType;
end;

initialization
  InitBaseTypeToDefaultTypeInfo;
end.

