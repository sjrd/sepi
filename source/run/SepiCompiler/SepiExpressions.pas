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
  SysUtils, Classes, Math, TypInfo, ScUtils, ScInterfaces, ScCompilerMagic,
  ScDelphiLanguage, ScIntegerSets, SepiReflectionCore, SepiOrdTypes,
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
    Valeur
    @author sjrd
    @version 1.0
  *}
  ISepiValue = interface(ISepiExpressionPart)
    ['{20D185F7-7022-4B7F-9F94-2EDEBF2EFB1E}']

    procedure Finalize;

    function GetValueType: TSepiType;

    property ValueType: TSepiType read GetValueType;
  end;

  {*
    Valeur dont le type peut être forcé
    @author sjrd
    @version 1.0
  *}
  ISepiTypeForceableValue = interface(ISepiValue)
    ['{661E099B-4CAE-4F34-B29B-B2A91CE4CAC1}']

    function CanForceType(AValueType: TSepiType;
      Explicit: Boolean = False): Boolean;
    procedure ForceType(AValueType: TSepiType);
  end;

  {*
    Valeur ensemble dont le type d'ensemble ou d'élément peut être forcé
    @author sjrd
    @version 1.0
  *}
  ISepiTypeForceableSetValue = interface(ISepiTypeForceableValue)
    ['{27DD7646-15E4-41E9-8E82-681D533A419E}']

    function GetIsEmpty: Boolean;
    function GetCompKind: TTypeKind;
    function GetCompType: TSepiOrdType;
    function GetSetType: TSepiSetType;

    function GetLowerBound: Integer;
    function GetHigherBound: Integer;

    function CanForceCompType(ACompType: TSepiOrdType): Boolean;
    procedure ForceCompType(ACompType: TSepiOrdType);

    property IsEmpty: Boolean read GetIsEmpty;
    property CompKind: TTypeKind read GetCompKind;
    property CompType: TSepiOrdType read GetCompType;
    property SetType: TSepiSetType read GetSetType;

    property LowerBound: Integer read GetLowerBound;
    property HigherBound: Integer read GetHigherBound;
  end;

  {*
    Valeur qui peut être lue à l'exécution
    @author sjrd
    @version 1.0
  *}
  ISepiReadableValue = interface(ISepiValue)
    ['{D7207F63-22F1-41C7-8366-2D4A87DD5200}']

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property IsConstant: Boolean read GetIsConstant;
    property ConstValuePtr: Pointer read GetConstValuePtr;
  end;

  {*
    Valeur qui peut être modifiée à l'exécution
    @author sjrd
    @version 1.0
  *}
  ISepiWritableValue = interface(ISepiValue)
    ['{E5E9C42F-E9A2-4B13-AE79-E1229013007D}']

    procedure CompileWrite(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Source: ISepiReadableValue);
  end;

  {*
    Valeur dont on peut récupérer l'adresse à l'exécution
    @author sjrd
    @version 1.0
  *}
  ISepiAddressableValue = interface(ISepiValue)
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
  ISepiDirectValue = interface(ISepiValue)
    ['{12678933-C5A4-4BBA-9BBF-3E3BF30B4AD5}']
  end;

  {*
    Expression qui peut être appelée
    @author sjrd
    @version 1.0
  *}
  ISepiCallable = interface(ISepiExpressionPart)
    ['{CD26F811-9B78-4F42-ACD7-82B1FB93EA3F}']

    function GetParamCount: Integer;
    function GetParams(Index: Integer): ISepiValue;
    function GetParamsCompleted: Boolean;

    procedure AddParam(const Value: ISepiValue);
    procedure CompleteParams;

    function GetReturnType: TSepiType;

    procedure CompileNoResult(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiValue read GetParams;
    property ParamsCompleted: Boolean read GetParamsCompleted;

    property ReturnType: TSepiType read GetReturnType;
  end;

  {*
    Propriété
    @author sjrd
    @version 1.0
  *}
  ISepiProperty = interface(ISepiValue)
    ['{9C5A0B51-BDB2-45FA-B2D4-179763CB7F16}']

    function GetParamCount: Integer;
    function GetParams(Index: Integer): ISepiValue;
    procedure SetParams(Index: Integer; const Value: ISepiValue);
    function GetParamsCompleted: Boolean;

    procedure CompleteParams;

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiValue
      read GetParams write SetParams;
    property ParamsCompleted: Boolean read GetParamsCompleted;
  end;

  {*
    Constructeur d'ensemble
    @author sjrd
    @version 1.0
  *}
  ISepiSetBuilder = interface(ISepiTypeForceableSetValue)
    ['{335E8CD4-677F-41EA-8567-3CDD2CB2B651}']

    procedure AddSingle(const Value: ISepiReadableValue);
    procedure AddRange(const Lower, Higher: ISepiReadableValue);

    procedure Complete;
  end;

  {*
    Expression nil
    @author sjrd
    @version 1.0
  *}
  ISepiNilValue = interface(ISepiValue)
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
  TSepiCustomDirectValue = class(TSepiCustomExpressionPart, ISepiValue,
    ISepiDirectValue, ISepiReadableValue, ISepiWritableValue,
    ISepiAddressableValue)
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

    procedure Finalize; virtual;

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
  TSepiCustomComputedValue = class(TSepiCustomExpressionPart, ISepiValue,
    ISepiReadableValue)
  private
    FValueType: TSepiType; /// Type de valeur

    FConstValuePtr: Pointer; /// Pointeur sur la valeur constante
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure Finalize; virtual;

    function GetValueType: TSepiType;
    procedure SetValueType(AType: TSepiType);

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    procedure ErrorTypeNotApplicable;
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

    constructor Create(ValueType: TSepiOrdType; Value: Integer); overload;

    constructor Create(SepiRoot: TSepiRoot; Value: Int64); overload;

    constructor Create(SepiRoot: TSepiRoot; Value: Extended); overload;

    constructor Create(SepiRoot: TSepiRoot; Value: AnsiChar); overload;
    constructor Create(SepiRoot: TSepiRoot; Value: WideChar); overload;

    constructor Create(SepiRoot: TSepiRoot; const Value: AnsiString); overload;
    constructor Create(SepiRoot: TSepiRoot; const Value: WideString); overload;

    destructor Destroy; override;

    class function MakeValue(UnitCompiler: TSepiUnitCompiler;
      Constant: TSepiConstant): ISepiReadableValue; overload;
    class function MakeValue(Compiler: TSepiMethodCompiler;
      Constant: TSepiConstant): ISepiReadableValue; overload;

    class function MakeOrdinalValue(UnitCompiler: TSepiUnitCompiler;
      ValueType: TSepiOrdType; Value: Integer): ISepiReadableValue; overload;
    class function MakeOrdinalValue(Compiler: TSepiMethodCompiler;
      ValueType: TSepiOrdType; Value: Integer): ISepiReadableValue; overload;

    class function MakeIntegerLiteral(UnitCompiler: TSepiUnitCompiler;
      Value: Int64): ISepiReadableValue; overload;
    class function MakeIntegerLiteral(Compiler: TSepiMethodCompiler;
      Value: Int64): ISepiReadableValue; overload;

    class function MakeExtendedLiteral(UnitCompiler: TSepiUnitCompiler;
      Value: Extended): ISepiReadableValue; overload;
    class function MakeExtendedLiteral(Compiler: TSepiMethodCompiler;
      Value: Extended): ISepiReadableValue; overload;

    class function MakeCharLiteral(UnitCompiler: TSepiUnitCompiler;
      Value: AnsiChar): ISepiReadableValue; overload;
    class function MakeCharLiteral(Compiler: TSepiMethodCompiler;
      Value: AnsiChar): ISepiReadableValue; overload;
    class function MakeCharLiteral(UnitCompiler: TSepiUnitCompiler;
      Value: WideChar): ISepiReadableValue; overload;
    class function MakeCharLiteral(Compiler: TSepiMethodCompiler;
      Value: WideChar): ISepiReadableValue; overload;

    class function MakeStringLiteral(UnitCompiler: TSepiUnitCompiler;
      const Value: AnsiString): ISepiReadableValue; overload;
    class function MakeStringLiteral(Compiler: TSepiMethodCompiler;
      const Value: AnsiString): ISepiReadableValue; overload;
    class function MakeStringLiteral(UnitCompiler: TSepiUnitCompiler;
      const Value: WideString): ISepiReadableValue; overload;
    class function MakeStringLiteral(Compiler: TSepiMethodCompiler;
      const Value: WideString): ISepiReadableValue; overload;

    property Constant: TSepiConstant read FConstant;
    property ConstValuePtr;
  end;

  {*
    Valeur meta-classe
    @author sjrd
    @version 1.0
  *}
  TSepiMetaClassValue = class(TSepiCustomComputedValue)
  private
    FSepiClass: TSepiClass; /// Classe Sepi
  protected
    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    constructor Create(ASepiClass: TSepiClass = nil);

    procedure Complete;

    class function MakeValue(Compiler: TSepiMethodCompiler;
      SepiClass: TSepiClass): ISepiReadableValue;

    property SepiClass: TSepiClass read FSepiClass write FSepiClass;
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

    class function MakeValue(UnitCompiler: TSepiUnitCompiler;
      Variable: TSepiVariable): ISepiValue; overload;
    class function MakeValue(Compiler: TSepiMethodCompiler;
      Variable: TSepiVariable): ISepiValue; overload;

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

    class function MakeValue(Compiler: TSepiMethodCompiler;
      LocalVar: TSepiLocalVar): ISepiValue;

    property LocalVar: TSepiLocalVar read FLocalVar;
  end;

  {*
    Opérateur de transtypage
    @author sjrd
    @version 1.0
  *}
  TSepiCastOperator = class(TSepiCustomDirectValue)
  private
    FIsOrd: Boolean;      /// Indique si c'est un cast de type Ord
    FIsChr: Boolean;      /// Indique si c'est un cast de type Chr
    FOperand: ISepiValue; /// Valeur source
    FForceCast: Boolean;  /// True force même avec des tailles différentes

    procedure HandleOrdChr;
    procedure CollapseConsts;

    function InternalCastValue: ISepiValue;
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
      const AOperand: ISepiValue = nil);
    constructor CreateOrd(const AOperand: ISepiValue = nil);
    constructor CreateChr(const AOperand: ISepiValue = nil);

    procedure Complete;

    class function CastValue(DestType: TSepiType;
      const Value: ISepiValue; ForceCast: Boolean = False): ISepiValue;
    class function CastValueOrd(const Value: ISepiValue): ISepiValue;
    class function CastValueChr(const Value: ISepiValue): ISepiValue;

    property IsOrd: Boolean read FIsOrd;
    property IsChr: Boolean read FIsChr;
    property Operand: ISepiValue read FOperand write FOperand;
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

    class function ConvertionExists(
      DestType, SrcType: TSepiType): Boolean; overload;
    class function ConvertionExists(DestType: TSepiType;
      const Source: ISepiReadableValue): Boolean; overload;
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
    FOperation: TSepiOperation; /// Opération
  protected
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

    class function MakeOperation(Operation: TSepiOperation;
      const Operand: ISepiReadableValue): ISepiReadableValue;

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

    class function MakeOperation(Operation: TSepiOperation;
      const LeftOperand, RightOperand: ISepiReadableValue;
      AutoConvert: Boolean = True): ISepiReadableValue;

    property LeftOperand: ISepiReadableValue
      read FLeftOperand write FLeftOperand;
    property RightOperand: ISepiReadableValue
      read FRightOperand write FRightOperand;

    property AutoConvert: Boolean read FAutoConvert write FAutoConvert;
  end;

  {*
    Opération sur des ensembles
    Bien que syntaxiquement, cette classe implémente les interfaces
    ISepiTypeForceableValue et ISepiTypeForceableSetValue, ces deux interfaces
    ne sont effectivement supportées à l'exécution que si les deux opérandes
    implémentent l'interface ISepiTypeForceableSetValue.
    @author sjrd
    @version 1.0
  *}
  TSepiSetOperation = class(TSepiCustomComputedValue,
    ISepiTypeForceableValue, ISepiTypeForceableSetValue)
  private
    FOperation: TSepiOperation;        /// Opération
    FLeftOperand: ISepiReadableValue;  /// Opérande de gauche
    FRightOperand: ISepiReadableValue; /// Opérande de droite

    FLeftForceable: ISepiTypeForceableSetValue;  /// Opérande gauche forçable
    FRightForceable: ISepiTypeForceableSetValue; /// Opérande droite forçable

    function IsTypeForceable: Boolean;

    procedure ForceTypesTogether;

    procedure CheckTypes;
    procedure CollapseConsts;

    function GetIsEmpty: Boolean;
    function GetCompKind: TTypeKind;
    function GetCompType: TSepiOrdType;
    function GetSetType: TSepiSetType;

    function GetLowerBound: Integer;
    function GetHigherBound: Integer;

    procedure SetLeftOperand(const Value: ISepiReadableValue);
    procedure SetRightOperand(const Value: ISepiReadableValue);
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure Finalize; override;

    procedure ErrorTypeMismatch;

    function GetOpCode(SelfOp: Boolean): TSepiOpCode;

    function CanForceCompType(ACompType: TSepiOrdType): Boolean;
    function CanForceType(AValueType: TSepiType;
      Explicit: Boolean = False): Boolean;
    procedure ForceCompType(ACompType: TSepiOrdType);
    procedure ForceType(AValueType: TSepiType);

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;

    property LeftForceable: ISepiTypeForceableSetValue read FLeftForceable;
    property RightForceable: ISepiTypeForceableSetValue read FRightForceable;
  public
    constructor Create(AOperation: TSepiOperation);

    procedure Complete;

    class function MakeOperation(Operation: TSepiOperation;
      const LeftOperand, RightOperand: ISepiReadableValue): ISepiReadableValue;

    property Operation: TSepiOperation read FOperation;

    property LeftOperand: ISepiReadableValue
      read FLeftOperand write SetLeftOperand;
    property RightOperand: ISepiReadableValue
      read FRightOperand write SetRightOperand;
  end;

  {*
    Intervalle de valeurs dans un constructeur d'ensemble
    @author sjrd
    @version 1.0
  *}
  TSepiSetBuilderRange = record
    LowerValue: ISepiReadableValue;  /// Valeur basse
    HigherValue: ISepiReadableValue; /// Valeur haute
  end;

  {*
    Constructeur d'ensemble
    @author sjrd
    @version 1.0
  *}
  TSepiSetBuilder = class(TSepiCustomComputedValue, ISepiSetBuilder,
    ISepiTypeForceableValue, ISepiTypeForceableSetValue)
  private
    FCompKind: TTypeKind;    /// Sorte de type des éléments
    FCompType: TSepiOrdType; /// Type d'élément
    FSetType: TSepiSetType;  /// Type de l'ensemble

    FConstantValues: TScIntegerSet;         /// Valeurs constantes
    FSingles: array of ISepiReadableValue;  /// Valeurs simples
    FRanges: array of TSepiSetBuilderRange; /// Intervalles de valeur

    FLowerBound: Integer;  /// Borne inférieure courante
    FHigherBound: Integer; /// Borne supérieure courante

    procedure ExportConstantValues(var ConstSet);

    procedure CollapseConsts;

    function GetIsEmpty: Boolean;
    function GetCompKind: TTypeKind;
    function GetCompType: TSepiOrdType;
    function GetSetType: TSepiSetType;

    function GetLowerBound: Integer;
    function GetHigherBound: Integer;
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure Finalize; override;

    procedure SetValueType(AType: TSepiType); reintroduce;

    function AddType(ACompType: TSepiOrdType): Boolean;

    procedure AddConstInterval(const Lower, Higher: ISepiReadableValue);

    procedure AddSingle(const Value: ISepiReadableValue);
    procedure AddRange(const Lower, Higher: ISepiReadableValue);

    procedure SetCompType(ACompType: TSepiOrdType); overload;
    procedure SetCompType(ACompTypeInfo: PTypeInfo); overload;
    procedure SetSetType(ASetType: TSepiSetType);

    function CanForceCompType(ACompType: TSepiOrdType): Boolean;
    function CanForceType(AValueType: TSepiType;
      Explicit: Boolean = False): Boolean;
    procedure ForceCompType(ACompType: TSepiOrdType);
    procedure ForceType(AValueType: TSepiType);

    procedure Complete;

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    constructor Create;
    destructor Destroy; override;

    property IsEmpty: Boolean read GetIsEmpty;
    property CompKind: TTypeKind read FCompKind;
    property CompType: TSepiOrdType read FCompType;
    property SetType: TSepiSetType read FSetType;

    property LowerBound: Integer read FLowerBound;
    property HigherBound: Integer read FHigherBound;
  end;

  {*
    Opérateur Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiOperator = class(TObject)
  public
    class function ForceOneToAnother(const Left: ISepiTypeForceableValue;
      const Right: ISepiReadableValue): Boolean; overload;
    class function ForceOneToAnother(
      const Left, Right: ISepiReadableValue): Boolean; overload;

    class function MakeUnaryOperation(Operation: TSepiOperation;
      const Operand: ISepiReadableValue): ISepiReadableValue;

    class function MakeBinaryOperation(Operation: TSepiOperation;
      const LeftOperand, RightOperand: ISepiReadableValue;
     AutoConvert: Boolean = True): ISepiReadableValue;
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

    class function MakeAddressOf(
      const Operand: ISepiAddressableValue): ISepiReadableValue;

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
    constructor Create;

    procedure Complete;

    class function MakeDereference(
      const Operand: ISepiReadableValue): ISepiValue;

    property Operand: ISepiReadableValue read FOperand write FOperand;
  end;

  {*
    Valeur élément de de tableau
    @author sjrd
    @version 1.0
  *}
  TSepiArrayItemValue = class(TSepiCustomDirectValue)
  private
    FArrayValue: ISepiValue;         /// Valeur tableau
    FIndexValue: ISepiReadableValue; /// Valeur index

    FArrayType: TSepiArrayType; /// Type du tableau
    FIndexType: TSepiOrdType;   /// Type de l'index
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;

    property ArrayType: TSepiArrayType read FArrayType;
    property IndexType: TSepiOrdType read FIndexType;
  public
    procedure Complete;

    class function MakeArrayItemValue(const ArrayValue: ISepiValue;
      const IndexValue: ISepiReadableValue): ISepiValue;

    property ArrayValue: ISepiValue read FArrayValue write FArrayValue;
    property IndexValue: ISepiReadableValue read FIndexValue write FIndexValue;
  end;

  {*
    Valeur champ de record
    @author sjrd
    @version 1.0
  *}
  TSepiRecordFieldValue = class(TSepiCustomDirectValue)
  private
    FRecordValue: ISepiValue; /// Valeur record
    FField: TSepiField;       /// Champ
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    constructor Create(const ARecordValue: ISepiValue; AField: TSepiField);

    property RecordValue: ISepiValue read FRecordValue;
    property Field: TSepiField read FField;
  end;

  {*
    Valeur champ d'un objet
    @author sjrd
    @version 1.0
  *}
  TSepiObjectFieldValue = class(TSepiCustomDirectValue)
  private
    FObjectValue: ISepiReadableValue; /// Valeur objet
    FField: TSepiField;               /// Champ
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    constructor Create(const AObjectValue: ISepiReadableValue;
      AField: TSepiField);

    property ObjectValue: ISepiReadableValue read FObjectValue;
    property Field: TSepiField read FField;
  end;

  {*
    Classe de base pour les expressions qui ont des paramètres
    @author sjrd
    @version 1.0
  *}
  TSepiCustomWithParams = class(TSepiCustomExpressionPart)
  private
    FParams: IInterfaceList; /// Liste des paramètres

    FParamsCompleted: Boolean; /// True si les paramètres sont clôturés
  protected
    function GetParamCount: Integer;
    procedure SetParamCount(Value: Integer);
    function GetParams(Index: Integer): ISepiValue;
    procedure SetParams(Index: Integer; const Value: ISepiValue); virtual;
    procedure AddParam(const Value: ISepiValue); virtual;

    function GetParamsCompleted: Boolean;

    procedure CompleteParams; virtual;

    function MatchesSignature(Signature: TSepiSignature;
      MakeErrors: Boolean = True): Boolean;

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiValue read GetParams write SetParams;
    property ParamsCompleted: Boolean read FParamsCompleted;
  public
    constructor Create(AInitParamCount: Integer = 0);
  end;

  {*
    Classe de base pour les expressions qui peuvent être invoquées
    @author sjrd
    @version 1.0
  *}
  TSepiCustomCallable = class(TSepiCustomWithParams, ISepiCallable, ISepiValue,
    ISepiReadableValue)
  private
    FSignature: TSepiSignature; /// Signature de l'appel

    procedure CompileParam(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; SignatureParam: TSepiParam;
      ParamValue: ISepiValue; InstrParamMemory: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager; FreeParamValue: Boolean = False);
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure SetSignature(ASignature: TSepiSignature;
      APrepareParams: Boolean = False);

    procedure Finalize; virtual;

    function GetReturnType: TSepiType; virtual;
    function GetValueType: TSepiType;

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    procedure CompileParams(CallInstr: TSepiAsmCall;
      Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
      Destination: TSepiMemoryReference;
      const SelfValue: ISepiReadableValue = nil;
      SelfMem: TSepiMemoryReference = nil; FreeParamValue: Boolean = False);

    procedure CompileCall(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      Destination: TSepiMemoryReference); virtual; abstract;

    procedure CompileNoResult(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);
    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property Signature: TSepiSignature read FSignature;
    property ReturnType: TSepiType read GetReturnType;
  public
    constructor Create(ASignature: TSepiSignature = nil;
      APrepareParams: Boolean = False);
  end;

  {*
    Appel de méthode
    @author sjrd
    @version 1.0
  *}
  TSepiMethodCall = class(TSepiCustomCallable)
  private
    FMethod: TSepiMethod;                     /// Méthode à appeler
    FOverloadedMethod: TSepiOverloadedMethod; /// Méthode surchargée à appeler
    FSelfValue: ISepiReadableValue;           /// Valeur Self
    FForceStaticCall: Boolean;                /// Force un appel statique

    FFreeParamValue: Boolean; /// Valeur de l'éventuel paramètre $Free

    procedure CheckSelfValueType;
  protected
    function GetReturnType: TSepiType; override;

    procedure CompleteParams; override;

    procedure CompileCall(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      Destination: TSepiMemoryReference); override;
  public
    constructor Create(AMethod: TSepiMethod;
      const ASelfValue: ISepiReadableValue = nil;
      AForceStaticCall: Boolean = False;
      AFreeParamValue: Boolean = False); overload;
    constructor Create(AOverloadedMethod: TSepiOverloadedMethod;
      const ASelfValue: ISepiReadableValue = nil;
      AForceStaticCall: Boolean = False;
      AFreeParamValue: Boolean = False); overload;

    property Method: TSepiMethod read FMethod;
    property OverloadedMethod: TSepiOverloadedMethod read FOverloadedMethod;
    property SelfValue: ISepiReadableValue read FSelfValue write FSelfValue;
    property ForceStaticCall: Boolean
      read FForceStaticCall write FForceStaticCall;
    property FreeParamValue: Boolean read FFreeParamValue write FFreeParamValue;
  end;

  {*
    Appel de référence de méthode
    @author sjrd
    @version 1.0
  *}
  TSepiMethodRefCall = class(TSepiCustomCallable)
  private
    FMethodRefValue: ISepiReadableValue; /// Valeur référence de méthode
  protected
    procedure CompileCall(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      Destination: TSepiMemoryReference); override;
  public
    procedure CompleteParams; override;

    constructor Create(const AMethodRefValue: ISepiReadableValue = nil;
      AComplete: Boolean = False);

    procedure Complete;

    property MethodRefValue: ISepiReadableValue
      read FMethodRefValue write FMethodRefValue;
  end;

  {*
    Valeur propriété
    Bien que, syntaxiquement, cette classe implémente les deux interfaces
    ISepiReadableValue et ISepiWritableValue, ces implémentations ne sont
    effectives que si la propriété sous-jacente peut être lue et écrite
    respectivement.
  *}
  TSepiPropertyValue = class(TSepiCustomWithParams, ISepiProperty, ISepiValue,
    ISepiReadableValue, ISepiWritableValue)
  private
    FObjectValue: ISepiReadableValue; /// Valeur objet
    FProperty: TSepiProperty;         /// Propriété

    function MakeIndexValue(Compiler: TSepiMethodCompiler;
      Method: TSepiMethod): ISepiReadableValue;
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure Finalize; virtual;

    function GetValueType: TSepiType;

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    procedure CompleteParams; override;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); virtual;

    procedure CompileWrite(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Source: ISepiReadableValue); virtual;
  public
    constructor Create(const AObjectValue: ISepiReadableValue;
      AProperty: TSepiProperty);

    property ObjectValue: ISepiReadableValue read FObjectValue;
    property ObjectProperty: TSepiProperty read FProperty;
    property ValueType: TSepiType read GetValueType;
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
  TSepiNilValue = class(TSepiCustomExpressionPart, ISepiValue, ISepiNilValue)
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure Finalize;

    function GetValueType: TSepiType;
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
  CharIsUnicodeToBaseType: array[Boolean] of TSepiBaseType = (
    btAnsiChar, btWideChar
  );
  StrIsUnicodeToBaseType: array[Boolean] of TSepiBaseType = (
    btAnsiStr, btWideStr
  );
begin
  Result := True;

  if SepiType is TSepiIntegerType then
    BaseType := OrdTypeToBaseType[GetTypeData(SepiType.TypeInfo).OrdType]
  else if SepiType is TSepiInt64Type then
    BaseType := btInt64
  else if SepiType is TSepiFloatType then
    BaseType := FloatTypeToBaseType[TSepiFloatType(SepiType).FloatType]
  else if (SepiType is TSepiBooleanType) and
    (TSepiBooleanType(SepiType).BooleanKind = bkBoolean) then
    BaseType := btBoolean
  else if SepiType is TSepiCharType then
    BaseType := CharIsUnicodeToBaseType[TSepiCharType(SepiType).IsUnicode]
  else if SepiType is TSepiStringType then
    BaseType := StrIsUnicodeToBaseType[TSepiStringType(SepiType).IsUnicode]
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

  Expression.Attach(ISepiValue, AsExpressionPart);

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
procedure TSepiCustomDirectValue.Finalize;
begin
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
  Result := UnitCompiler.GetPointerType(ValueType);
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
  Expression.Attach(ISepiValue, AsExpressionPart);
  Expression.Attach(ISepiReadableValue, AsExpressionPart);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomComputedValue.Finalize;
begin
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
  Produit une erreur Opération non applicable à ce type d'opérande
*}
procedure TSepiCustomComputedValue.ErrorTypeNotApplicable;
begin
  MakeError(SOperationNotApplicableToType);
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
    else if ValueType.NeedInit or (ValueType is TSepiSetType) then
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
  Crée une constante ordinale
  @param ValueType   Type de valeur
  @param Value       Valeur de la constante
*}
constructor TSepiTrueConstValue.Create(ValueType: TSepiOrdType;
  Value: Integer);
begin
  Create(ValueType);

  Move(Value, ConstValuePtr^, ValueType.Size);
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

{*
  Construit une expression valeur pour une constante globale dans une unité
  @param UnitCompiler   Compilateur d'unité
  @param Constant       Constante globale
  @return Valeur représentant la constante globale
*}
class function TSepiTrueConstValue.MakeValue(UnitCompiler: TSepiUnitCompiler;
  Constant: TSepiConstant): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(Constant);
  Result.AttachToExpression(TSepiExpression.Create(UnitCompiler));
end;

{*
  Construit une expression valeur pour une constante globale dans une méthode
  @param Compiler   Compilateur de méthode
  @param Constant   Constante globale
  @return Valeur représentant la constante globale
*}
class function TSepiTrueConstValue.MakeValue(Compiler: TSepiMethodCompiler;
  Constant: TSepiConstant): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(Constant);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{*
  Construit une expression constante ordinale dans une unité
  @param UnitCompiler   Compilateur d'unité
  @param ValueType      Type de valeur
  @param Value          Valeur
  @return Valeur ordinale constante
*}
class function TSepiTrueConstValue.MakeOrdinalValue(
  UnitCompiler: TSepiUnitCompiler; ValueType: TSepiOrdType;
  Value: Integer): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(ValueType, Value);
  Result.AttachToExpression(TSepiExpression.Create(UnitCompiler));
end;

{*
  Construit une expression constante ordinale dans une méthode
  @param Compiler    Compilateur de méthode
  @param ValueType   Type de valeur
  @param Value       Valeur
  @return Valeur ordinale constante
*}
class function TSepiTrueConstValue.MakeOrdinalValue(
  Compiler: TSepiMethodCompiler; ValueType: TSepiOrdType;
  Value: Integer): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(ValueType, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{*
  Construit une expression valeur pour un littéral entier dans une unité
  @param UnitCompiler   Compilateur d'unité
  @param Value          Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeIntegerLiteral(
  UnitCompiler: TSepiUnitCompiler; Value: Int64): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(UnitCompiler.SepiUnit.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(UnitCompiler));
end;

{*
  Construit une expression valeur pour un littéral entier dans une méthode
  @param Compiler   Compilateur de méthode
  @param Value      Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeIntegerLiteral(
  Compiler: TSepiMethodCompiler; Value: Int64): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(Compiler.SepiMethod.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{*
  Construit une expression valeur pour un littéral flottant dans une unité
  @param UnitCompiler   Compilateur d'unité
  @param Value          Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeExtendedLiteral(
  UnitCompiler: TSepiUnitCompiler; Value: Extended): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(UnitCompiler.SepiUnit.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(UnitCompiler));
end;

{*
  Construit une expression valeur pour un littéral flottant dans une méthode
  @param Compiler   Compilateur de méthode
  @param Value      Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeExtendedLiteral(
  Compiler: TSepiMethodCompiler; Value: Extended): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(Compiler.SepiMethod.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{*
  Construit une expression valeur pour un littéral caractère dans une unité
  @param UnitCompiler   Compilateur d'unité
  @param Value          Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeCharLiteral(
  UnitCompiler: TSepiUnitCompiler; Value: AnsiChar): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(UnitCompiler.SepiUnit.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(UnitCompiler));
end;

{*
  Construit une expression valeur pour un littéral caractère dans une méthode
  @param Compiler   Compilateur de méthode
  @param Value      Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeCharLiteral(
  Compiler: TSepiMethodCompiler; Value: AnsiChar): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(Compiler.SepiMethod.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{*
  Construit une expression valeur pour un littéral caractère dans une unité
  @param UnitCompiler   Compilateur d'unité
  @param Value          Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeCharLiteral(
  UnitCompiler: TSepiUnitCompiler; Value: WideChar): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(UnitCompiler.SepiUnit.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(UnitCompiler));
end;

{*
  Construit une expression valeur pour un littéral caractère dans une méthode
  @param Compiler   Compilateur de méthode
  @param Value      Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeCharLiteral(
  Compiler: TSepiMethodCompiler; Value: WideChar): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(Compiler.SepiMethod.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{*
  Construit une expression valeur pour un littéral chaîne dans une unité
  @param UnitCompiler   Compilateur d'unité
  @param Value          Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeStringLiteral(
  UnitCompiler: TSepiUnitCompiler; const Value: AnsiString): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(UnitCompiler.SepiUnit.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(UnitCompiler));
end;

{*
  Construit une expression valeur pour un littéral chaîne dans une méthode
  @param Compiler   Compilateur de méthode
  @param Value      Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeStringLiteral(
  Compiler: TSepiMethodCompiler; const Value: AnsiString): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(Compiler.SepiMethod.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{*
  Construit une expression valeur pour un littéral chaîne dans une unité
  @param UnitCompiler   Compilateur d'unité
  @param Value          Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeStringLiteral(
  UnitCompiler: TSepiUnitCompiler; const Value: WideString): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(UnitCompiler.SepiUnit.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(UnitCompiler));
end;

{*
  Construit une expression valeur pour un littéral chaîne dans une méthode
  @param Compiler   Compilateur de méthode
  @param Value      Valeur littérale
  @return Valeur représentant la constante littérale
*}
class function TSepiTrueConstValue.MakeStringLiteral(
  Compiler: TSepiMethodCompiler; const Value: WideString): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(Compiler.SepiMethod.Root, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{---------------------------}
{ TSepiMetaClassValue class }
{---------------------------}

{*
  Crée une valeur meta-classe
  @param ASepiClass   Classe Sepi
*}
constructor TSepiMetaClassValue.Create(ASepiClass: TSepiClass = nil);
begin
  inherited Create;

  FSepiClass := ASepiClass;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaClassValue.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  Instruction: TSepiAsmGetRunInfo;
begin
  NeedDestination(Destination, ValueType, Compiler, TempVars,
    Instructions.GetCurrentEndRef);

  Instruction := TSepiAsmGetRunInfo.Create(Compiler, ocGetDelphiClass);
  Instructions.Add(Instruction);
  Instruction.SourcePos := Expression.SourcePos;
  Instruction.Destination.Assign(Destination);
  Instruction.SetReference(SepiClass);
end;

{*
  Complète la valeur
  La valeur doit auparavant avoir été liée à une expression, pour bénéficier de
  son contexte.
*}
procedure TSepiMetaClassValue.Complete;
begin
  Assert(SepiClass <> nil);

  SetValueType(UnitCompiler.GetMetaClass(SepiClass));
end;

{*
  Construit une valeur meta-classe
  @param SepiClass   Classe Sepi
*}
class function TSepiMetaClassValue.MakeValue(Compiler: TSepiMethodCompiler;
  SepiClass: TSepiClass): ISepiReadableValue;
var
  MetaClassValue: TSepiMetaClassValue;
begin
  MetaClassValue := TSepiMetaClassValue.Create(SepiClass);
  Result := MetaClassValue;
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
  MetaClassValue.Complete;
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

{*
  Construit une expression valeur pour une variable globale dans une unité
  @param UnitCompiler   Compilateur d'unité
  @param Variable       Variable globale
  @return Valeur représentant la variable globale
*}
class function TSepiVariableValue.MakeValue(UnitCompiler: TSepiUnitCompiler;
  Variable: TSepiVariable): ISepiValue;
begin
  Result := TSepiVariableValue.Create(Variable);
  Result.AttachToExpression(TSepiExpression.Create(UnitCompiler));
end;

{*
  Construit une expression valeur pour une variable globale dans une méthode
  @param Compiler   Compilateur de méthode
  @param Variable   Variable globale
  @return Valeur représentant la variable globale
*}
class function TSepiVariableValue.MakeValue(Compiler: TSepiMethodCompiler;
  Variable: TSepiVariable): ISepiValue;
begin
  Result := TSepiVariableValue.Create(Variable);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
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

{*
  Construit une expression valeur pour une variable locale
  @param Compiler   Compilateur de méthode
  @param LocalVar   Variable locale
  @return Valeur représentant la variable locale
*}
class function TSepiLocalVarValue.MakeValue(Compiler: TSepiMethodCompiler;
  LocalVar: TSepiLocalVar): ISepiValue;
begin
  Result := TSepiLocalVarValue.Create(LocalVar);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{-------------------------}
{ TSepiCastOperator class }
{-------------------------}

constructor TSepiCastOperator.Create(DestType: TSepiType;
  const AOperand: ISepiValue = nil);
begin
  inherited Create;

  SetValueType(DestType);
  FOperand := AOperand;
end;

{*
  Crée une opération de conversion Ord
  @param ASource   Expression source
*}
constructor TSepiCastOperator.CreateOrd(const AOperand: ISepiValue = nil);
begin
  inherited Create;

  FIsOrd := True;
  FOperand := AOperand;
end;

{*
  Crée une opération de conversion Chr
  @param ASource   Expression source
*}
constructor TSepiCastOperator.CreateChr(const AOperand: ISepiValue = nil);
begin
  inherited Create;

  FIsChr := True;
  FOperand := AOperand;
end;

{*
  Gère le cas Ord et le cas Chr
*}
procedure TSepiCastOperator.HandleOrdChr;
var
  OpType: TSepiType;
  DestTypeInfo: PTypeInfo;
begin
  if not (IsOrd or IsChr) then
    Exit;

  OpType := Operand.ValueType;

  if not (OpType is TSepiOrdType) then
  begin
    MakeError(SOrdinalTypeRequired);
    Exit;
  end;

  if IsChr then
  begin
    FForceCast := True;

    if OpType.Size = 1 then
      DestTypeInfo := TypeInfo(AnsiChar)
    else
      DestTypeInfo := TypeInfo(WideChar);
  end else
  begin
    case TSepiOrdType(OpType).TypeData.OrdType of
      otSByte: DestTypeInfo := TypeInfo(Shortint);
      otUByte: DestTypeInfo := TypeInfo(Byte);
      otSWord: DestTypeInfo := TypeInfo(Smallint);
      otUWord: DestTypeInfo := TypeInfo(Word);
      otSLong: DestTypeInfo := TypeInfo(Longint);
      otULong: DestTypeInfo := TypeInfo(LongWord);
    else
      Assert(False);
      DestTypeInfo := nil;
    end;
  end;

  SetValueType(SepiRoot.FindType(DestTypeInfo));
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

  // If the operand is already of the good type, collapsing const is OK
  if ConstOp.ValueType = ValueType then
  begin
    ConstValuePtr := ConstOp.ConstValuePtr;
    Exit;
  end;

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
  Tronc commun des fonctions de classe CastValue, CastValueOrd et CastValueChr
  @return Valeur transtypée
*}
function TSepiCastOperator.InternalCastValue: ISepiValue;
begin
  Result := Self;
  Result.AttachToExpression(
    TSepiExpression.Create(Operand as ISepiExpression));
  Complete;
  Result.AttachToExpression(Expression);
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
  TypeForceableOp: ISepiTypeForceableValue;
  OpType: TSepiType;
begin
  Assert(Operand <> nil);

  // If the operand may be forced (explicitly) to be a ValueType, just do that
  if (Operand.ValueType <> ValueType) and (ValueType <> nil) and
    Supports(Operand, ISepiTypeForceableValue, TypeForceableOp) then
  begin
    if TypeForceableOp.CanForceType(ValueType, True) then
      TypeForceableOp.ForceType(ValueType);
  end;

  // Finalize operand
  Operand.Finalize;
  OpType := Operand.ValueType;

  // Handle Ord or Chr cast - exit if failed
  HandleOrdChr;
  if ValueType = nil then
    Exit;

  // Update access flags and check types
  if Supports(Operand, ISepiNilValue) then
    IsReadable := True
  else
  begin
    IsReadable := Supports(Operand, ISepiReadableValue);
    IsWritable := Supports(Operand, ISepiWritableValue);
    IsAddressable := Supports(Operand, ISepiAddressableValue);

    if (OpType <> nil) and (OpType.Size <> ValueType.Size) then
    begin
      if (OpType.Size < ValueType.Size) or (not ForceCast) then
      begin
        MakeError(Format(SInvalidCast, [OpType.Name, ValueType.Name]));
        Exit;
      end;
    end;
  end;

  // Collapse constants
  CollapseConsts;
end;

{*
  Transtype une valeur
  @param DestType   Type de destination
  @param Value      Valeur à transtyper
  @return Valeur transtypée
*}
class function TSepiCastOperator.CastValue(DestType: TSepiType;
  const Value: ISepiValue; ForceCast: Boolean = False): ISepiValue;
var
  CastOp: TSepiCastOperator;
begin
  CastOp := TSepiCastOperator.Create(DestType, Value);
  CastOp.ForceCast := ForceCast;
  Result := CastOp.InternalCastValue;
end;

{*
  Transtype une valeur comme Ord
  @param Value   Valeur à transtyper
  @return Valeur transtypée
*}
class function TSepiCastOperator.CastValueOrd(
  const Value: ISepiValue): ISepiValue;
begin
  Result := TSepiCastOperator.CreateOrd(Value).InternalCastValue;
end;

{*
  Transtype une valeur comme Chr
  @param Value   Valeur à transtyper
  @return Valeur transtypée
*}
class function TSepiCastOperator.CastValueChr(
  const Value: ISepiValue): ISepiValue;
begin
  Result := TSepiCastOperator.CreateChr(Value).InternalCastValue;
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

  // If the source is already of the good type, just copy the constant
  if SrcType = ValueType then
  begin
    AllocateConstant;
    ValueType.CopyData(Source.ConstValuePtr^, ConstValuePtr^);
    Exit;
  end;

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

  if SrcType = ValueType then
  begin
    // Pure assignment
    Source.CompileRead(Compiler, Instructions, Destination, TempVars);
  end else if SrcType is TSepiClass then
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
var
  TypeForceableSrc: ISepiTypeForceableValue;
begin
  Assert(FSource <> nil);

  if not ConvertionExists(ValueType, Source) then
  begin
    MakeError(Format(STypeMismatch, [ValueType, Source.ValueType]));
    Exit;
  end;

  if (Source.ValueType <> ValueType) and
    Supports(Source, ISepiTypeForceableValue, TypeForceableSrc) and
    TypeForceableSrc.CanForceType(ValueType) then
  begin
    TypeForceableSrc.ForceType(ValueType);
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
  Teste si une conversion existe et est possible d'une valeur en un type donné
  @param DestType   Type de destination
  @param Source     Valeur source
  @return True si une conversion est possible, False sinon
*}
class function TSepiConvertOperation.ConvertionExists(DestType: TSepiType;
  const Source: ISepiReadableValue): Boolean;
var
  SrcType: TSepiType;
  TypeForceableSrc: ISepiTypeForceableValue;
begin
  SrcType := Source.ValueType;

  if SrcType = DestType then
    Result := True
  else if Supports(Source, ISepiTypeForceableValue, TypeForceableSrc) and
    TypeForceableSrc.CanForceType(DestType) then
    Result := True
  else
    Result := ConvertionExists(DestType, SrcType);
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
begin
  ConvertOp := TSepiConvertOperation.Create(DestType, Value);
  ConvertOp.Complete;
  Result := ConvertOp;

  Result.AttachToExpression(TSepiExpression.Create(Value as ISepiExpression));
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
    begin
      ErrorTypeNotApplicable;
      Operand := TSepiTrueConstValue.MakeIntegerLiteral(UnitCompiler, 0);
      SetValueType(Operand.ValueType);
    end;
  end else
  begin
    if not (ValueType.Kind in [tkInteger, tkFloat, tkVariant, tkInt64]) then
    begin
      ErrorTypeNotApplicable;
      Operand := TSepiTrueConstValue.MakeIntegerLiteral(UnitCompiler, 0);
      SetValueType(Operand.ValueType);
    end;
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
var
  BaseType: TSepiBaseType;
  NewOpType: TSepiType;
begin
  Assert(Expression <> nil);
  Assert(Operand <> nil);

  Operand.Finalize;

  if (Operation = opNegate) and (Operand.ValueType is TSepiIntegerType) and
    (not TSepiIntegerType(Operand.ValueType).Signed) then
  begin
    BaseType := SepiTypeToBaseType(Operand.ValueType);
    TSepiBinaryOperation.SmallestCommonType(BaseType, btShortint, BaseType);

    NewOpType := DefaultSepiTypeFor(BaseType, Operand.ValueType.Root);
    Operand := TSepiConvertOperation.ConvertValue(NewOpType, Operand);
  end;

  SetValueType(Operand.ValueType);
  CheckType;
  CollapseConsts;
end;

{*
  Construit une opération unaire
  @param Operation   Opération
  @param Operand     Opérande
  @return Valeur représentant l'opération
*}
class function TSepiUnaryOperation.MakeOperation(Operation: TSepiOperation;
  const Operand: ISepiReadableValue): ISepiReadableValue;
var
  UnaryOp: TSepiUnaryOperation;
begin
  UnaryOp := TSepiUnaryOperation.Create(Operation);
  Result := UnaryOp;
  Result.AttachToExpression(TSepiExpression.Create(Operand as ISepiExpression));
  UnaryOp.Operand := Operand;
  UnaryOp.Complete;
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

    LeftOperand := TSepiCastOperator.CastValue(CommonType,
      LeftOperand) as ISepiReadableValue;
    RightOperand := TSepiCastOperator.CastValue(CommonType,
      RightOperand) as ISepiReadableValue;
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
  FloatType: TSepiFloatType;
  LeftExtValue: Extended;
  OpCode: TSepiOpCode;
  VarType: TSepiBaseType;
begin
  if (not LeftOperand.IsConstant) or (not RightOperand.IsConstant) then
    Exit;

  AllocateConstant;

  { Special handling of x / 0.0, because compiler accept this but at runtime,
    this raises an exception. }
  if (Operation = opDivide) and (ValueType is TSepiFloatType) then
  begin
    FloatType := TSepiFloatType(ValueType);

    if FloatType.ValueAsExtended(RightOperand.ConstValuePtr^) = 0.0 then
    begin
      LeftExtValue := FloatType.ValueAsExtended(LeftOperand.ConstValuePtr^);

      if LeftExtValue > 0.0 then
        FloatType.SetValueAsExtended(ConstValuePtr^, 1.0 / 0.0)
      else if LeftExtValue < 0.0 then
        FloatType.SetValueAsExtended(ConstValuePtr^, -1.0 / 0.0)
      else
        FloatType.SetValueAsExtended(ConstValuePtr^, 0.0 / 0.0);

      Exit;
    end;
  end;

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

  TSepiOperator.ForceOneToAnother(LeftOperand, RightOperand);

  LeftOperand.Finalize;
  RightOperand.Finalize;

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

  StrResults: array[btAnsiChar..btWideStr, btAnsiChar..btWideStr] of
      TSepiBaseType = (
    (btAnsiChar, btWideChar, btAnsiStr, btWideStr),
    (btWideChar, btWideChar, btWideStr, btWideStr),
    (btAnsiStr , btWideStr , btAnsiStr, btWideStr),
    (btWideStr , btWideStr , btWideStr, btWideStr)
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
  end else if Types * [btAnsiChar..btWideStr] <> [] then
  begin
    // Strings only matches with themselves - prefer wide and prefer string
    if not (Types <= [btAnsiChar..btWideStr]) then
    begin
      Result := False;
      Exit;
    end;

    CommonType := StrResults[LeftType, RightType];
  end else
  begin
    // For numbers, use the table
    CommonType := NumResults[LeftType, RightType];
  end;

  Result := True;
end;

{*
  Construit une opération binaire
  @param Operation      Opération
  @param LeftOperand    Opérande de gauche
  @param RightOperand   Opérande de droite
  @param AutoConvert    True permet d'effectuer des conversions automatiques
  @return Valeur représentant l'opération
*}
class function TSepiBinaryOperation.MakeOperation(Operation: TSepiOperation;
  const LeftOperand, RightOperand: ISepiReadableValue;
  AutoConvert: Boolean = True): ISepiReadableValue;
var
  BinaryOp: TSepiBinaryOperation;
begin
  BinaryOp := TSepiBinaryOperation.Create(Operation, AutoConvert);
  Result := BinaryOp;
  Result.AttachToExpression(
    TSepiExpression.Create(LeftOperand as ISepiExpression));
  BinaryOp.LeftOperand := LeftOperand;
  BinaryOp.RightOperand := RightOperand;
  BinaryOp.Complete;
end;

{-------------------------}
{ TSepiSetOperation class }
{-------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiSetOperation.Create(AOperation: TSepiOperation);
begin
  inherited Create;

  FOperation := AOperation;
end;

{*
  Teste si le type de cette opération peut être forcé
*}
function TSepiSetOperation.IsTypeForceable: Boolean;
begin
  Result := (not (Operation in opComparisonOps)) and
    (LeftForceable <> nil) and (RightForceable <> nil);
end;

{*
  Force les types des deux opérandes pour les mettre d'accord
*}
procedure TSepiSetOperation.ForceTypesTogether;
begin
  if TSepiOperator.ForceOneToAnother(LeftOperand, RightOperand) then
    Exit;

  if (LeftForceable = nil) or (RightForceable = nil) then
    Exit;

  // TODO
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOperation.CheckTypes;
var
  LeftType, RightType: TSepiType;
begin
  LeftType := LeftOperand.ValueType;
  RightType := LeftOperand.ValueType;

  if (not (LeftType is TSepiSetType)) or (not (RightType is TSepiSetType)) then
  begin
    ErrorTypeNotApplicable;
    Exit;
  end;

  if TSepiSetType(LeftType).CompType <> TSepiSetType(RightType).CompType then
  begin
    ErrorTypeMismatch;
    Exit;
  end;

  if Operation in opComparisonOps then
    SetValueType(SepiRoot.FindType(TypeInfo(Boolean)))
  else
    SetValueType(LeftType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOperation.CollapseConsts;
var
  SetSize: Integer;
  LeftOp, RightOp: Pointer;
  BoolResult: PBoolean;
begin
  if (not LeftOperand.IsConstant) or (not RightOperand.IsConstant) then
    Exit;

  AllocateConstant;

  SetSize := LeftOperand.ValueType.Size;

  LeftOp := LeftOperand.ConstValuePtr;
  RightOp := RightOperand.ConstValuePtr;
  BoolResult := PBoolean(ConstValuePtr);

  if Operation in [opAdd, opSubtract, opMultiply] then
    Move(LeftOp^, ConstValuePtr^, SetSize);

  case Operation of
    opAdd:
      SetUnion(ConstValuePtr^, RightOp^, SetSize);
    opSubtract:
      SetSub(ConstValuePtr^, RightOp^, SetSize);
    opMultiply:
      SetIntersect(ConstValuePtr^, RightOp^, SetSize);
    opCmpEQ:
      BoolResult^ := SetEquals(LeftOp^, RightOp^, SetSize);
    opCmpNE:
      BoolResult^ := not SetEquals(LeftOp^, RightOp^, SetSize);
    opCmpLE:
      BoolResult^ := SetContained(LeftOp^, RightOp^, SetSize);
  else
    Assert(False);
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.GetIsEmpty: Boolean;
begin
  Assert(IsTypeForceable);
  Result := False;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.GetCompKind: TTypeKind;
begin
  Assert(IsTypeForceable);
  Result := LeftForceable.CompKind;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.GetCompType: TSepiOrdType;
begin
  Assert(IsTypeForceable);
  Result := LeftForceable.CompType;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.GetSetType: TSepiSetType;
begin
  Assert(IsTypeForceable);
  Result := LeftForceable.SetType;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.GetLowerBound: Integer;
begin
  Assert(IsTypeForceable);
  Result := Min(LeftForceable.LowerBound, RightForceable.LowerBound);
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.GetHigherBound: Integer;
begin
  Assert(IsTypeForceable);
  Result := Max(LeftForceable.HigherBound, RightForceable.HigherBound);
end;

{*
  Modifie l'opérande de gauche
  @param Value   Nouvel opérande de gauche
*}
procedure TSepiSetOperation.SetLeftOperand(const Value: ISepiReadableValue);
begin
  FLeftOperand := Value;

  if not Supports(Value, ISepiTypeForceableSetValue, FLeftForceable) then
    FLeftForceable := nil;
end;

{*
  Modifie l'opérande de droite
  @param Value   Nouvel opérande de droite
*}
procedure TSepiSetOperation.SetRightOperand(const Value: ISepiReadableValue);
begin
  FRightOperand := Value;

  if not Supports(Value, ISepiTypeForceableSetValue, FRightForceable) then
    FRightForceable := nil;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if SameGUID(IID, ISepiTypeForceableValue) or
    SameGUID(IID, ISepiTypeForceableSetValue) then
  begin
    if not IsTypeForceable then
    begin
      Result := E_NOINTERFACE;
      Exit;
    end;
  end;

  Result := inherited QueryInterface(IID, Obj);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOperation.Finalize;
begin
  LeftOperand.Finalize;
  RightOperand.Finalize;

  if ConstValuePtr = nil then
    CollapseConsts;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOperation.ErrorTypeMismatch;
begin
  MakeError(Format(STypeMismatch,
    [LeftOperand.ValueType.Name, RightOperand.ValueType.Name]));
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.GetOpCode(SelfOp: Boolean): TSepiOpCode;
begin
  case Operation of
    opAdd:
      if SelfOp then
        Result := ocSetSelfUnion
      else
        Result := ocSetOtherUnion;
    opSubtract:
      if SelfOp then
        Result := ocSetSelfSubtract
      else
        Result := ocSetOtherSubtract;
    opMultiply:
      if SelfOp then
        Result := ocSetSelfIntersect
      else
        Result := ocSetOtherIntersect;
    opCmpEQ:
      Result := ocSetEquals;
    opCmpNE:
      Result := ocSetNotEquals;
    opCmpLE:
      Result := ocSetContained;
  else
    Assert(False);
    Result := ocNope;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.CanForceCompType(ACompType: TSepiOrdType): Boolean;
begin
  Assert(IsTypeForceable);

  Result := LeftForceable.CanForceCompType(ACompType) and
    RightForceable.CanForceCompType(ACompType);
end;

{*
  [@inheritDoc]
*}
function TSepiSetOperation.CanForceType(AValueType: TSepiType;
  Explicit: Boolean = False): Boolean;
begin
  Assert(IsTypeForceable);

  Result := LeftForceable.CanForceType(AValueType) and
    RightForceable.CanForceType(AValueType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOperation.ForceCompType(ACompType: TSepiOrdType);
begin
  Assert(IsTypeForceable);

  LeftForceable.ForceCompType(ACompType);
  RightForceable.ForceCompType(ACompType);

  SetValueType(LeftForceable.ValueType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOperation.ForceType(AValueType: TSepiType);
begin
  Assert(IsTypeForceable);

  LeftForceable.ForceType(AValueType);
  RightForceable.ForceType(AValueType);

  SetValueType(LeftForceable.ValueType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOperation.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  LeftMemory, RightMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  OpInstr: TSepiAsmSetOperation;
  CmpInstr: TSepiAsmSetCompare;
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
      CmpInstr := TSepiAsmSetCompare.Create(Compiler, GetOpCode(False),
        LeftOperand.ValueType.Size);
      CmpInstr.SourcePos := Expression.SourcePos;

      NeedDestination(Destination, ValueType, Compiler, TempVars,
        CmpInstr.AfterRef);

      CmpInstr.Destination.Assign(Destination);
      CmpInstr.Left.Assign(LeftMemory);
      CmpInstr.Right.Assign(RightMemory);

      Instructions.Add(CmpInstr);
    end else
    begin
      OpInstr := TSepiAsmSetOperation.Create(Compiler, GetOpCode(False),
        ValueType.Size);
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
  [@inheritDoc]
*}
procedure TSepiSetOperation.Complete;
begin
  Assert(Expression <> nil);
  Assert((LeftOperand <> nil) and (RightOperand <> nil));

  ForceTypesTogether;
  CheckTypes;
end;

{*
  [@inheritDoc]
*}
class function TSepiSetOperation.MakeOperation(Operation: TSepiOperation;
  const LeftOperand, RightOperand: ISepiReadableValue): ISepiReadableValue;
var
  SetOp: TSepiSetOperation;
begin
  SetOp := TSepiSetOperation.Create(Operation);
  Result := SetOp;
  Result.AttachToExpression(
    TSepiExpression.Create(LeftOperand as ISepiExpression));
  SetOp.LeftOperand := LeftOperand;
  SetOp.RightOperand := RightOperand;
  SetOp.Complete;
end;

{-----------------------}
{ TSepiSetBuilder class }
{-----------------------}

{*
  [@inheritDoc]
*}
constructor TSepiSetBuilder.Create;
begin
  inherited Create;

  FCompKind := tkUnknown;
  FCompType := nil;
  FSetType := nil;

  FConstantValues := TScIntegerSet.Create;

  FLowerBound := MaxInt;
  FHigherBound := -MaxInt-1;
end;

{*
  [@inheritDoc]
*}
destructor TSepiSetBuilder.Destroy;
begin
  FConstantValues.Free;

  inherited;
end;

{*
  Indique si l'ensemble est vide
  @return True si l'ensemble est vide, False sinon
*}
function TSepiSetBuilder.GetIsEmpty: Boolean;
begin
  Result := FConstantValues.IsEmpty and (Length(FSingles) = 0) and
    (Length(FRanges) = 0);
end;

{*
  Sorte de type des éléments
  @return Sorte de type des éléments
*}
function TSepiSetBuilder.GetCompKind: TTypeKind;
begin
  Result := FCompKind;
end;

{*
  Type des éléments
  @return Type des éléments
*}
function TSepiSetBuilder.GetCompType: TSepiOrdType;
begin
  Result := FCompType;
end;

{*
  Type de l'ensemble
  @return Type de l'ensemble
*}
function TSepiSetBuilder.GetSetType: TSepiSetType;
begin
  Result := FSetType;
end;

{*
  Borne inférieure courante
  @return Borne inférieure courante
*}
function TSepiSetBuilder.GetLowerBound: Integer;
begin
  Result := FLowerBound;
end;

{*
  Borne supérieure courante
  @return Borne supérieure courante
*}
function TSepiSetBuilder.GetHigherBound: Integer;
begin
  Result := FHigherBound;
end;

{*
  Exporte les valeurs constantes dans un ensemble
  @param ConstSet   Ensemble destination (de type SetType)
*}
procedure TSepiSetBuilder.ExportConstantValues(var ConstSet);
var
  Lo: Integer;
  Enum: TScIntegerSetEnumerator;
  Value: Integer;
begin
  FillChar(ConstSet, SetType.Size, 0);
  Lo := SetType.CompType.MinValue;

  Enum := FConstantValues.GetEnumerator;
  try
    while Enum.MoveNext do
    begin
      Value := Enum.Current;
      SetBit(ConstSet, Value - Lo);
    end;
  finally
    Enum.Free;
  end;
end;

{*
  Pliage des constantes
*}
procedure TSepiSetBuilder.CollapseConsts;
begin
  if (Length(FSingles) > 0) or (Length(FRanges) > 0) then
    Exit;

  AllocateConstant;
  ExportConstantValues(ConstValuePtr^);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetBuilder.AttachToExpression(const Expression: ISepiExpression);
begin
  inherited;

  Expression.Attach(ISepiSetBuilder, ISepiSetBuilder(Self));
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetBuilder.Finalize;
begin
  // Untyped empty set are not supported
  if FCompKind = tkUnknown then
  begin
    MakeError(SUntypedEmptySetNotSupported);
    SetCompType(TypeInfo(Byte));
  end;

  Complete;

  if ConstValuePtr = nil then
    CollapseConsts;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetBuilder.SetValueType(AType: TSepiType);
begin
  SetSetType(AType as TSepiSetType);
end;

{*
  Ajoute un type de composant à l'ensemble en construction
  @param ACompType   Type de composant à ajouter
  @return True en cas de succès, False si ACompType est incompatible
*}
function TSepiSetBuilder.AddType(ACompType: TSepiOrdType): Boolean;
var
  ACompKind: TTypeKind;
begin
  // Fetch standardized comp kind
  if ACompType.Kind = tkWChar then
    ACompKind := tkChar
  else
    ACompKind := ACompType.Kind;

  // Check comp kind
  Result := ACompKind in [tkInteger, tkChar, tkEnumeration];

  if not Result then
  begin
    ErrorTypeNotApplicable;
    Exit;
  end;

  // Match added comp type with previously stored comp type
  if ACompType is TSepiEnumType then
  begin
    // Enumeration comp types must have equal base types
    ACompType := TSepiEnumType(ACompType).BaseType;
    if CompType = nil then
      SetCompType(ACompType)
    else if ACompType <> CompType then
      Result := False;
  end else
  begin
    // Other comp types must only be of the same kind
    if FCompKind = tkUnknown then
      FCompKind := ACompKind
    else if ACompKind <> FCompKind then
      Result := False;
  end;

  if not Result then
    MakeError(Format(STypeMismatch, [CompType.Name, ACompType.Name]));
end;

{*
  Ajoute un intervalle constant à l'ensemble en construction
  @param Lower    Borne inférieure de l'intervalle
  @param Higher   Borne supérieure de l'intervalle
*}
procedure TSepiSetBuilder.AddConstInterval(
  const Lower, Higher: ISepiReadableValue);
var
  LowerType, HigherType: TSepiOrdType;
  LowerValue, HigherValue: Integer;
begin
  LowerType := Lower.ValueType as TSepiOrdType;
  HigherType := Higher.ValueType as TSepiOrdType;

  LowerValue := LowerType.ValueAsInteger(Lower.ConstValuePtr^);
  HigherValue := HigherType.ValueAsInteger(Higher.ConstValuePtr^);

  if LowerValue <= HigherValue then
  begin
    FConstantValues.AddInterval(LowerValue, HigherValue);

    if LowerValue < FLowerBound then
      FLowerBound := LowerValue;
    if HigherValue > FHigherBound then
      FHigherBound := HigherValue;
  end;
end;

{*
  Ajoute un élément dans l'ensemble en construction
  @param Value   Valeur à ajouter
*}
procedure TSepiSetBuilder.AddSingle(const Value: ISepiReadableValue);
var
  ValueType: TSepiOrdType;
  Index: Integer;
begin
  if not (Value.ValueType is TSepiOrdType) then
    ErrorTypeNotApplicable
  else
  begin
    ValueType := TSepiOrdType(Value.ValueType);

    if AddType(ValueType) then
    begin
      if Value.IsConstant then
      begin
        // Add constant value
        Self.AddConstInterval(Value, Value);
      end else
      begin
        // Add variable value
        Index := Length(FSingles);
        SetLength(FSingles, Index+1);
        FSingles[Index] := Value;

        if ValueType.MinValue < FLowerBound then
          FLowerBound := ValueType.MinValue;
        if ValueType.MaxValue > FHigherBound then
          FHigherBound := ValueType.MaxValue;
      end;
    end;
  end;
end;

{*
  Ajoute un intervalle de valeur à l'ensemble en construction
  @param Lower    Borne inférieure de l'intervalle
  @param Higher   Borne supérieure de l'intervalle
*}
procedure TSepiSetBuilder.AddRange(const Lower, Higher: ISepiReadableValue);
var
  LowerType, HigherType: TSepiOrdType;
  Index: Integer;
begin
  if not ((Lower.ValueType is TSepiOrdType) and
    (Higher.ValueType is TSepiOrdType)) then
    ErrorTypeNotApplicable
  else
  begin
    LowerType := TSepiOrdType(Lower.ValueType);
    HigherType := TSepiOrdType(Higher.ValueType);

    if AddType(LowerType) and AddType(HigherType) then
    begin
      if Lower.IsConstant and Higher.IsConstant then
      begin
        // Add constant interval
        AddConstInterval(Lower, Higher);
      end else
      begin
        // Add variable interval
        Index := Length(FRanges);
        SetLength(FRanges, Index+1);
        FRanges[Index].LowerValue := Lower;
        FRanges[Index].HigherValue := Higher;

        if LowerType.MinValue < FLowerBound then
          FLowerBound := HigherType.MinValue;
        if HigherType.MaxValue > FHigherBound then
          FHigherBound := HigherType.MaxValue;
      end;
    end;
  end;
end;

{*
  Définit le type des éléments
  Cette méthode ne peut être appelée qu'une fois, avec un type d'éléments
  compatible avec d'éventuelles valeurs déjà insérées.
  @param ACompType   Type des éléments
*}
procedure TSepiSetBuilder.SetCompType(ACompType: TSepiOrdType);
begin
  Assert((FCompKind = tkUnknown) or (ACompType.Kind = FCompKind));

  // The comp type must include every possible value in the set
  if (ACompType.MinValue > LowerBound) or
    (ACompType.MaxValue < HigherBound) then
  begin
    MakeError(Format(SCompTypeTooNarrow, [ACompType.Name]));
  end else
  begin
    FCompKind := ACompType.Kind;
    FCompType := ACompType;
  end;
end;

{*
  Définit le type des éléments
  Cette méthode ne peut être appelée qu'une fois, avec un type d'éléments
  compatible avec d'éventuelles valeurs déjà insérées.
  @param ACompTypeInfo   RTTI du type des éléments
*}
procedure TSepiSetBuilder.SetCompType(ACompTypeInfo: PTypeInfo);
begin
  SetCompType(SepiRoot.FindType(ACompTypeInfo) as TSepiOrdType);
end;

{*
  Définit le type de l'ensemble
  Cette méthode ne peut être appelée qu'une fois, avec un type ensemble
  compatible avec d'éventuelles valeurs déjà insérées.
  @param ASetType   Type ensemble
*}
procedure TSepiSetBuilder.SetSetType(ASetType: TSepiSetType);
begin
  if ASetType.CompType <> CompType then
    SetCompType(ASetType.CompType);

  inherited SetValueType(ASetType);
  FSetType := ASetType;
end;

{*
  Teste si l'ensemble peut être forcé à un type d'élément donné
  @param ACompType   Type d'élément à tester
  @return True si le type est valide, False sinon
*}
function TSepiSetBuilder.CanForceCompType(ACompType: TSepiOrdType): Boolean;
begin
  Result := False;

  // Check type kind
  if (FCompKind <> tkUnknown) and (ACompType.Kind <> FCompKind) and
    (not ((ACompType.Kind = tkWChar) and (FCompKind = tkChar))) then
    Exit;

  // Check enum base type
  if (FCompKind = tkEnumeration) and
    (TSepiEnumType(ACompType).BaseType <>
    TSepiEnumType(FCompType).BaseType) then
    Exit;

  // Check range
  if (ACompType.MinValue > LowerBound) or
    (ACompType.MaxValue < HigherBound) then
    Exit;

  Result := True;
end;

{*
  Teste si l'ensemble peut être forcé à un type donné
  @param AValueType   Type à tester
  @return True si le type est valide, False sinon
*}
function TSepiSetBuilder.CanForceType(AValueType: TSepiType;
  Explicit: Boolean = False): Boolean;
begin
  Result := (AValueType is TSepiSetType) and
    CanForceCompType(TSepiSetType(AValueType).CompType);
end;

{*
  Force le type d'élément à un type donné
  Cette méthode ne peut être appelée que si CanForceCompType renvoie True pour
  le type ACompType.
  @param ACompType   Type auquel forcer les éléments
*}
procedure TSepiSetBuilder.ForceCompType(ACompType: TSepiOrdType);
begin
  if CanForceCompType(ACompType) then
    SetCompType(ACompType as TSepiOrdType)
  else
    Assert(False);
end;

{*
  Force le type de l'ensemble à un type donné
  Cette méthode ne peut être appelée que si CanForceType renvoie True pour le
  type AValueType.
  @param AValueType   Type auquel forcer l'ensemble
*}
procedure TSepiSetBuilder.ForceType(AValueType: TSepiType);
begin
  if CanForceType(AValueType) then
    SetSetType(AValueType as TSepiSetType)
  else
    Assert(False);
end;

{*
  Complète l'ensemble en construction
*}
procedure TSepiSetBuilder.Complete;
begin
  if FCompKind = tkUnknown then
  begin
    // Empty set - can't do anything
    Exit;
  end else if FHigherBound - FLowerBound >= 256 then
  begin
    // Range is too wide
    MakeError(SSetRangeTooWide);
    if FCompKind = tkChar then
      SetCompType(TypeInfo(AnsiChar))
    else
      SetCompType(TypeInfo(Byte));
  end else
  begin
    // OK, that's good
    if FCompKind <> tkEnumeration then
    begin
      // We need find the most appropriate comp type
      if (FCompType <> nil) and
        (FCompType.MaxValue-FCompType.MinValue < 256) then
      begin
        // Nothing to do
      end else if (FLowerBound >= 0) and (FHigherBound < 256) then
      begin
        // Canonical byte- or char-set
        if FCompKind = tkChar then
          SetCompType(TypeInfo(AnsiChar))
        else
          SetCompType(TypeInfo(Byte));
      end else
      begin
        // Build a dedicated comp type
        if FCompKind = tkChar then
          SetCompType(TSepiCharType.Create(UnitCompiler.SepiUnit, '',
            WideChar(FLowerBound), WideChar(FHigherBound)))
        else
          SetCompType(TSepiIntegerType.Create(UnitCompiler.SepiUnit, '',
            FLowerBound, FHigherBound));
      end;
    end else
    begin
      // CompType is already set
      Assert(FCompType is TSepiEnumType);

      if FCompType.MaxValue >= 256 then
      begin
        // Canonical set is too wide, build a dedicated one
        FCompType := TSepiEnumType.Create(UnitCompiler.SepiUnit, '',
          TSepiEnumType(FCompType), LowerBound, HigherBound);
      end;
    end;
  end;

  // When I come here, CompType is always valid
  Assert((CompType <> nil) and (CompType.MaxValue-CompType.MinValue < 256));

  // Build the set type
  if (FSetType = nil) or (FSetType.CompType <> CompType) then
    SetSetType(UnitCompiler.MakeSetType(CompType));
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetBuilder.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  ConstSet: TSepiConstant;
  SetType: TSepiSetType;
  First: Boolean;
  I: Integer;
  Lower, Higher, Single: ISepiReadableValue;
  LowerMemory, HigherMemory, SingleMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  SetRangeInstr: TSepiAsmSetRange;
  MoveInstr: TSepiAsmMove;
  UnionInstr: TSepiAsmSetOperation;
  SetElemInstr: TSepiAsmSetElem;
begin
  // Create constant
  if FConstantValues.IsEmpty then
    ConstSet := nil
  else
  begin
    ConstSet := TSepiConstant.Create(Compiler.SepiMethod, '', ValueType);
    ExportConstantValues(ConstSet.ValuePtr^);
  end;

  SetType := ValueType as TSepiSetType;
  First := True;

  // Compute ranges
  for I := 0 to Length(FRanges)-1 do
  begin
    Lower := FRanges[I].LowerValue;
    Higher := FRanges[I].HigherValue;

    LowerMemory := nil;
    HigherMemory := nil;
    try
      // Read Lower and Higher
      SrcTempVars := TSepiTempVarsLifeManager.Create;
      try
        Lower.CompileRead(Compiler, Instructions, LowerMemory, SrcTempVars);
        Higher.CompileRead(Compiler, Instructions, HigherMemory, SrcTempVars);
      finally
        SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
        SrcTempVars.Free;
      end;

      // Make instruction
      SetRangeInstr := TSepiAsmSetRange.Create(Compiler, SetType.Size,
        not First);
      SetRangeInstr.SourcePos := Expression.SourcePos;

      NeedDestination(Destination, ValueType, Compiler, TempVars,
        SetRangeInstr.AfterRef);

      SetRangeInstr.Destination.Assign(Destination);
      SetRangeInstr.LowerBound.Assign(LowerMemory);
      SetRangeInstr.HigherBound.Assign(HigherMemory);

      Instructions.Add(SetRangeInstr);

      // The next one will not be first
      First := False;
    finally
      LowerMemory.Free;
      HigherMemory.Free;
    end;
  end;

  // Compute constant part
  if not FConstantValues.IsEmpty then
  begin
    if First then
    begin
      // Copy ConstSet into Destination
      MoveInstr := TSepiAsmMove.Create(Compiler, SetType);
      MoveInstr.SourcePos := Expression.SourcePos;

      NeedDestination(Destination, ValueType, Compiler, TempVars,
        MoveInstr.AfterRef);

      MoveInstr.Destination.Assign(Destination);
      MoveInstr.Source.SetSpace(ConstSet);
      MoveInstr.Source.Seal;

      Instructions.Add(MoveInstr);

      // The next one will not be first
      First := False;
    end else
    begin
      // Union Destination with ConstSet
      UnionInstr := TSepiAsmSetOperation.Create(Compiler, ocSetSelfUnion,
        SetType.Size);
      UnionInstr.SourcePos := Expression.SourcePos;

      UnionInstr.Destination.Assign(Destination);
      UnionInstr.Right.SetSpace(ConstSet);
      UnionInstr.Right.Seal;

      Instructions.Add(UnionInstr);
    end;
  end;

  // Compute singles
  for I := 0 to Length(FSingles)-1 do
  begin
    Single := FSingles[I];

    SingleMemory := nil;
    try
      // Read Single
      SrcTempVars := TSepiTempVarsLifeManager.Create;
      try
        Single.CompileRead(Compiler, Instructions, SingleMemory, SrcTempVars);
      finally
        SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
        SrcTempVars.Free;
      end;

      // Make instruction
      SetElemInstr := TSepiAsmSetElem.Create(Compiler, SetType.Size,
        not First);
      SetElemInstr.SourcePos := Expression.SourcePos;

      NeedDestination(Destination, ValueType, Compiler, TempVars,
        SetElemInstr.AfterRef);

      SetElemInstr.Destination.Assign(Destination);
      SetElemInstr.Element.Assign(SingleMemory);

      Instructions.Add(SetElemInstr);

      // The next one will not be first
      First := False;
    finally
      SingleMemory.Free;
    end;
  end;

  Assert(not First);
end;

{---------------------}
{ TSepiOperator class }
{---------------------}

{*
  Force (implicitement) un opérande à avoir le type du second
  @param Left    Opérande dont forcer le type
  @param Right   Opérande à imiter
*}
class function TSepiOperator.ForceOneToAnother(
  const Left: ISepiTypeForceableValue;
  const Right: ISepiReadableValue): Boolean;
var
  DestType: TSepiType;
begin
  DestType := Right.ValueType;

  if DestType = nil then
    Result := False
  else if Left.ValueType = DestType then
    Result := True
  else
  begin
    Result := Left.CanForceType(DestType);
    if Result then
      Left.ForceType(DestType);
  end;
end;

{*
  Force (implicitement) un des opérandes à avoir le type de l'autre
  Si les deux opérandes peuvent être forcés l'un à l'autre mutuellement, cette
  méthode choisit de forcer le type l'opérande droit à celui de l'opérande
  gauche.
  @param Left    Opérande gauche
  @param Right   Opérande droit
*}
class function TSepiOperator.ForceOneToAnother(
  const Left, Right: ISepiReadableValue): Boolean;
var
  Forceable: ISepiTypeForceableValue;
begin
  if (Left.ValueType = Right.ValueType) and (Left.ValueType <> nil) then
    Result := True
  else if Supports(Right, ISepiTypeForceableValue, Forceable) and
    ForceOneToAnother(Forceable, Left) then
    Result := True
  else if Supports(Left, ISepiTypeForceableValue, Forceable) and
    ForceOneToAnother(Forceable, Right) then
    Result := True
  else
    Result := False;
end;

{*
  Construit une opération unaire
  @param Operation   Opération
  @param Operand     Opérande
  @return Valeur représentant l'opération
*}
class function TSepiOperator.MakeUnaryOperation(Operation: TSepiOperation;
  const Operand: ISepiReadableValue): ISepiReadableValue;
begin
  Result := TSepiUnaryOperation.MakeOperation(Operation, Operand);
end;

{*
  Construit une opération binaire
  @param Operation      Opération
  @param LeftOperand    Opérande de gauche
  @param RightOperand   Opérande de droite
  @param AutoConvert    True permet d'effectuer des conversions automatiques
  @return Valeur représentant l'opération
*}
class function TSepiOperator.MakeBinaryOperation(Operation: TSepiOperation;
  const LeftOperand, RightOperand: ISepiReadableValue;
  AutoConvert: Boolean = True): ISepiReadableValue;
const
  SetOperations = [opAdd, opSubtract, opMultiply, opCmpEQ, opCmpNE, opCmpLE];
var
  LeftType, RightType: TSepiType;
  LeftIsSet, RightIsSet: Boolean;
  Expression: ISepiExpression;
begin
  ForceOneToAnother(LeftOperand, RightOperand);

  LeftType := LeftOperand.ValueType;
  RightType := RightOperand.ValueType;

  // Make the appropriate operation
  LeftIsSet := (LeftType is TSepiSetType) or
    Supports(LeftOperand, ISepiTypeForceableSetValue);
  RightIsSet := (RightType is TSepiSetType) or
    Supports(RightOperand, ISepiTypeForceableSetValue);

  if LeftIsSet and RightIsSet then
  begin
    if Operation in SetOperations then
      Result := TSepiSetOperation.MakeOperation(Operation,
        LeftOperand, RightOperand);
  end else
  begin
    Result := TSepiBinaryOperation.MakeOperation(Operation,
      LeftOperand, RightOperand, AutoConvert);
  end;

  // Error message and invalid value into Result
  if Result = nil then
  begin
    Expression := LeftOperand as ISepiExpression;
    Expression.MakeError(SOperationNotApplicableToType);

    if Operation in opComparisonOps then
      Result := TSepiTrueConstValue.MakeOrdinalValue(Expression.UnitCompiler,
        Expression.SepiRoot.FindType(TypeInfo(Boolean)) as TSepiOrdType, 1)
    else
      Result := TSepiTrueConstValue.MakeIntegerLiteral(
        Expression.UnitCompiler, 0);
  end;
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

  Operand.Finalize;

  SetValueType(Operand.AddressType);
end;

{*
  Construit un opérateur d'adressage
  @param Operand   Opérande
  @return Valeur représentant l'opérateur d'adressage
*}
class function TSepiAddressOfValue.MakeAddressOf(
  const Operand: ISepiAddressableValue): ISepiReadableValue;
var
  AddressOf: TSepiAddressOfValue;
begin
  AddressOf := TSepiAddressOfValue.Create;
  Result := AddressOf;
  Result.AttachToExpression(TSepiExpression.Create(Operand as ISepiExpression));
  AddressOf.Operand := Operand;
  AddressOf.Complete;
end;

{-----------------------------}
{ TSepiDereferenceValue class }
{-----------------------------}

{*
  Crée une opération de déréférencement
*}
constructor TSepiDereferenceValue.Create;
begin
  inherited Create;

  IsReadable := True;
  IsWritable := True;
  IsAddressable := True;
end;

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
  Doit avoir été attaché à une expression au préalable, pour bénéficier du
  contexte.
*}
procedure TSepiDereferenceValue.Complete;
var
  OpType: TSepiType;
begin
  Assert(Operand <> nil);

  Operand.Finalize;

  OpType := Operand.ValueType;
  if OpType is TSepiPointerType then
    SetValueType(TSepiPointerType(OpType).PointTo)
  else if OpType is TSepiClass then
    SetValueType(UnitCompiler.GetMetaClass(TSepiClass(OpType)))
  else
    MakeError(SPointerTypeRequired);
end;

{*
  Construit un déréférencement
  @param Operand   Opérande
  @return Valeur déréférencée
*}
class function TSepiDereferenceValue.MakeDereference(
  const Operand: ISepiReadableValue): ISepiValue;
var
  Dereference: TSepiDereferenceValue;
begin
  Dereference := TSepiDereferenceValue.Create;
  Result := Dereference;
  Result.AttachToExpression(TSepiExpression.Create(Operand as ISepiExpression));
  Dereference.Operand := Operand;
  Dereference.Complete;
end;

{---------------------------}
{ TSepiArrayItemValue class }
{---------------------------}

{*
  [@inheritDoc]
*}
function TSepiArrayItemValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
const
  OrdTypeToOperation: array[TOrdType] of TSepiAddressOperation = (
    aoPlusConstTimesMemShortint, aoPlusConstTimesMemByte,
    aoPlusConstTimesMemSmallint, aoPlusConstTimesMemWord,
    aoPlusConstTimesMemLongint, aoPlusConstTimesMemLongWord
  );
var
  IndexValue: ISepiReadableValue;
  IntIndexType: TSepiIntegerType;
  IsDynamic: Boolean;
  SourceMemory, IndexMemory: TSepiMemoryReference;
  ConstIndex, ConstFactor: Integer;
begin
  IndexValue := Self.IndexValue;
  IsDynamic := ArrayType is TSepiDynArrayType;

  ConstFactor := ValueType.Size;

  if not (IndexValue.ValueType is TSepiIntegerType) then
    IndexValue := TSepiCastOperator.CastValueOrd(
      IndexValue) as ISepiReadableValue;
  IntIndexType := IndexValue.ValueType as TSepiIntegerType;

  SourceMemory := nil;
  IndexMemory := nil;
  try
    { Accessing items of static arrays or dynamic arrays are quite the same,
      yet subtly different.  Not only must a dynamic array be dereferenced,
      but writing an item of such an array requires *reading* the array value;
      while writing an item of a static array requires *addressing* the array
      value.  In both cases, the array value must be writable for allowance.
      Comments about accessing a record field also apply to static arrays. }

    // In both cases, first try and read the array value - otherwise address it
    if IsReadable then
    begin
      (ArrayValue as ISepiReadableValue).CompileRead(Compiler, Instructions,
        SourceMemory, TempVars);
    end else
    begin
      { For a dynamic array, if you cannot read the array, you can't do
        anything with its items.  Therefore, there is no way you could get
        here. }
      Assert(not IsDynamic);

      (ArrayValue as ISepiAddressableValue).CompileLoadAddress(Compiler,
        Instructions, SourceMemory, TempVars);
    end;

    // For static arrays, ConstIndex must counter the LowerBound effect
    if IsDynamic then
      ConstIndex := 0
    else
      ConstIndex := - TSepiStaticArrayType(ArrayType).LowerBound;

    // If the index value is constant, use ConstIndex, otherwise read it
    if IndexValue.IsConstant then
      Inc(ConstIndex, IntIndexType.ValueAsInteger(IndexValue.ConstValuePtr^))
    else
      IndexValue.CompileRead(Compiler, Instructions, IndexMemory, TempVars);

    // Finally, construct the memory reference for the item value
    Result := TSepiMemoryReference.Clone(SourceMemory);
    try
      // Dereference dynamic arrays, and static arrays which must be addressed
      if IsDynamic or (not IsReadable) then
        Result.AddOperation(adSimple);

      // Add the constant and variable offsets

      if ConstIndex <> 0 then
        Result.AddOperation(aoPlusConstShortint, ConstFactor * ConstIndex);

      if IndexMemory <> nil then
      begin
        Result.AddOperation(OrdTypeToOperation[IntIndexType.TypeData.OrdType],
          ConstFactor).Assign(IndexMemory);
      end;

      Result.Seal;
    except
      Result.Free;
      raise;
    end;
  finally
    IndexMemory.Free;
    SourceMemory.Free;
  end;
end;

{*
  Complète la valeur
  Doit avoir été attaché à une expression au préalable, pour bénéficier du
  contexte.
*}
procedure TSepiArrayItemValue.Complete;
begin
  Assert(ArrayValue <> nil);
  Assert(IndexValue <> nil);

  ArrayValue.Finalize;
  IndexValue.Finalize;

  // Fetch types
  FArrayType := ArrayValue.ValueType as TSepiArrayType;
  FIndexType := ArrayType.IndexType;
  SetValueType(ArrayType.ElementType);

  // Convert index
  if IndexValue.ValueType <> IndexType then
    IndexValue := TSepiConvertOperation.ConvertValue(IndexType, IndexValue);

  // Set accesses
  IsReadable := Supports(ArrayValue, ISepiReadableValue);
  if ArrayType is TSepiStaticArrayType then
  begin
    IsAddressable := Supports(ArrayValue, ISepiAddressableValue);
    IsWritable := IsAddressable and Supports(ArrayValue, ISepiWritableValue);
  end else
  begin
    IsAddressable := IsReadable;
    IsWritable := IsReadable and Supports(ArrayValue, ISepiWritableValue);
  end;
end;

{*
  Construit une valeur élément de tableau
  @param ArrayValue   Valeur tableau
  @param IndexValue   Valeur index
*}
class function TSepiArrayItemValue.MakeArrayItemValue(
  const ArrayValue: ISepiValue;
  const IndexValue: ISepiReadableValue): ISepiValue;
var
  ArrayItemValue: TSepiArrayItemValue;
begin
  ArrayItemValue := TSepiArrayItemValue.Create;
  Result := ArrayItemValue;
  Result.AttachToExpression(TSepiExpression.Create(
    ArrayValue as ISepiExpression));
  ArrayItemValue.ArrayValue := ArrayValue;
  ArrayItemValue.IndexValue := IndexValue;
  ArrayItemValue.Complete;
  Result.AttachToExpression(Result as ISepiExpression);
end;

{-----------------------------}
{ TSepiRecordFieldValue class }
{-----------------------------}

{*
  Crée une valeur champ
  @param ARecordValue   Conteneur du champ
  @param AField         Champ
*}
constructor TSepiRecordFieldValue.Create(const ARecordValue: ISepiValue;
  AField: TSepiField);
begin
  inherited Create;

  FRecordValue := ARecordValue;
  FField := AField;

  RecordValue.Finalize;

  Assert(Field.Owner = RecordValue.ValueType);
  Assert(Field.Owner is TSepiRecordType);

  SetValueType(Field.FieldType);

  IsReadable := Supports(RecordValue, ISepiReadableValue);
  IsAddressable := Supports(RecordValue, ISepiAddressableValue);
  IsWritable := IsAddressable and Supports(RecordValue, ISepiWritableValue);
end;

{*
  [@inheritDoc]
*}
function TSepiRecordFieldValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
var
  SourceMemory: TSepiMemoryReference;
begin
  SourceMemory := nil;
  try
    { While accessing a record field, we should always privilege *reading* the
      record, then adding the field offset, even when the eventual purpose is
      to write it.  Indeed, we know a record field can be written only if the
      underlying record can be written (for allowance) *and* addressed (for
      faisability).  And since addressing a value that can be read is always
      reading + LoadAddress instruction, it is more efficient to only read the
      record. }

    if IsReadable then
    begin
      (RecordValue as ISepiReadableValue).CompileRead(Compiler, Instructions,
        SourceMemory, TempVars);
    end else
    begin
      (RecordValue as ISepiAddressableValue).CompileLoadAddress(Compiler,
        Instructions, SourceMemory, TempVars);
    end;

    Result := TSepiMemoryReference.Clone(SourceMemory);
    try
      if not IsReadable then
        Result.AddOperation(adSimple);
      Result.AddOperation(aoPlusConstShortint, Field.Offset);
      Result.Seal;
    except
      Result.Free;
      raise;
    end;
  finally
    SourceMemory.Free;
  end;
end;

{-----------------------------}
{ TSepiObjectFieldValue class }
{-----------------------------}

{*
  Crée une valeur champ d'objet
  @param AObjectValue   Valeur objet
  @param AField         Champ
*}
constructor TSepiObjectFieldValue.Create(const AObjectValue: ISepiReadableValue;
  AField: TSepiField);
begin
  inherited Create;

  FObjectValue := AObjectValue;
  FField := AField;

  ObjectValue.Finalize;

  Assert((Field.Owner is TSepiClass) and (ObjectValue.ValueType is TSepiClass));
  Assert(TSepiClass(ObjectValue.ValueType).ClassInheritsFrom(
    TSepiClass(Field.Owner)));

  SetValueType(Field.FieldType);

  IsReadable := True;
  IsWritable := True;
  IsAddressable := True;
end;

{*
  [@inheritDoc]
*}
function TSepiObjectFieldValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
var
  SourceMemory: TSepiMemoryReference;
begin
  SourceMemory := nil;
  try
    { To access a class field, we must read the object value, then
      dereference it and add the field offset. }

    SourceMemory := nil;
    ObjectValue.CompileRead(Compiler, Instructions,
      SourceMemory, TempVars);

    Result := TSepiMemoryReference.Clone(SourceMemory);
    try
      Result.AddOperation(adSimple, aoPlusConstShortint, Field.Offset);
      Result.Seal;
    except
      Result.Free;
      raise;
    end;
  finally
    SourceMemory.Free;
  end;
end;

{-----------------------------}
{ TSepiCustomWithParams class }
{-----------------------------}

{*
  Crée une expression à paramètres
  @param AInitParamCount   Nombre initial de paramètres (défaut = 0)
*}
constructor TSepiCustomWithParams.Create(AInitParamCount: Integer = 0);
begin
  inherited Create;

  FParams := TInterfaceList.Create;
  FParams.Count := AInitParamCount;
end;

{*
  Nombre de paramètres
  @return Nombre de paramètres
*}
function TSepiCustomWithParams.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

{*
  Modifie le nombre de paramètres
  @param Value   Nouveau nombre de paramètres
*}
procedure TSepiCustomWithParams.SetParamCount(Value: Integer);
begin
  FParams.Count := Value;
end;

{*
  Tableau zero-based des paramètres
  @param Index   Index dans le tableau des paramètres
  @return Paramètre à l'index spécifié
*}
function TSepiCustomWithParams.GetParams(Index: Integer): ISepiValue;
begin
  Result := FParams[Index] as ISepiValue;
end;

{*
  Modifie un paramètre
  @param Index   Index du paramètre à modifié
  @param Value   Nouvelle valeur du paramètre
*}
procedure TSepiCustomWithParams.SetParams(Index: Integer;
  const Value: ISepiValue);
begin
  if ParamsCompleted then
    raise ESepiCompilerError.Create(SParamsAlreadyCompleted);

  FParams[Index] := Value;
end;

{*
  Ajoute un paramètre
  @param Value   Paramètre à ajouter
*}
procedure TSepiCustomWithParams.AddParam(const Value: ISepiValue);
begin
  if ParamsCompleted then
    raise ESepiCompilerError.Create(SParamsAlreadyCompleted);

  FParams.Add(Value);
end;

{*
  Complète les paramètres
  Une fois complétés, il n'est plus possible de modifier ou d'ajouter de
  paramètre.
*}
procedure TSepiCustomWithParams.CompleteParams;
var
  I: Integer;
begin
  if FParamsCompleted then
    Exit;

  FParamsCompleted := True;
  for I := 0 to ParamCount-1 do
    Params[I].Finalize;
end;

{*
  Teste si les paramètres correspondent à une signature
  @param Signature    Signature à tester
  @param MakeErrors   Si True (par défaut), génère des erreurs de compilation
*}
function TSepiCustomWithParams.MatchesSignature(Signature: TSepiSignature;
  MakeErrors: Boolean = True): Boolean;
var
  MinParamCount, I: Integer;
  NotEnoughParams, TooManyParams: Boolean;
  SignatureParam: TSepiParam;
  ParamValue: ISepiValue;
  ParamExpression: ISepiExpression;

  procedure Error(const Msg: string);
  begin
    Result := False;

    if MakeErrors then
      ParamExpression.MakeError(Msg)
    else
      raise EAbort.Create(Msg);
  end;

begin
  // Check param count
  if ParamCount < Signature.ParamCount then
  begin
    Result := False;
    MinParamCount := ParamCount;
    NotEnoughParams := True;
    TooManyParams := False;
  end else if ParamCount > Signature.ParamCount then
  begin
    Result := False;
    MinParamCount := Signature.ParamCount;
    NotEnoughParams := False;
    TooManyParams := True;
  end else
  begin
    Result := True;
    MinParamCount := ParamCount;
    NotEnoughParams := False;
    TooManyParams := False;
  end;

  // Check param types
  try
    for I := 0 to MinParamCount-1 do
    begin
      SignatureParam := Signature.Params[I];
      ParamValue := Params[I];
      ParamExpression := ParamValue as ISepiExpression;

      if SignatureParam.OpenArray then
        raise EAssertionFailed.Create('Open arrays not supported yet');

      if SignatureParam.ParamType = nil then
      begin
        // Untyped parameter: actual parameter must be addressable
        if not Supports(ParamValue, ISepiAddressableValue) then
          Error(SVarValueRequired);
      end else
      begin
        // Typed parameter: must be readable and types must be compatible

        // Must be readable
        if not Supports(ParamValue, ISepiReadableValue) then
        begin
          Error(SValueCantBeRead);
          Continue;
        end;

        // If by reference, must also be writable and addressable
        if SignatureParam.ByRef then
        begin
          if (not Supports(ParamValue, ISepiWritableValue)) or
            (not Supports(ParamValue, ISepiAddressableValue)) then
          begin
            Error(SVarValueRequired);
            Continue;
          end;
        end;

        // Types must be compatible
        if ParamValue.ValueType <> SignatureParam.ParamType then
        begin
          if TSepiConvertOperation.ConvertionExists(SignatureParam.ParamType,
            ParamValue.ValueType) then
          begin
            { Even if a convertion exists, var and out parameters must have
              exactly the same type as in declaration. }
            if SignatureParam.ByRef then
              Error(SVarParamTypeMustBeStrictlyEqual)
            else
            begin
              // Make the conversion now, no need to wait
              ParamValue := TSepiConvertOperation.ConvertValue(
                SignatureParam.ParamType, ParamValue as ISepiReadableValue);
              FParams[I] := ParamValue;
            end;
          end else
          begin
            Error(Format(STypeMismatch,
              [SignatureParam.ParamType.Name, ParamValue.ValueType.Name]));
          end;
        end;
      end;
    end;
  except
    on Error: EAbort do;
  end;

  // Make errors for param count mismatch
  if MakeErrors then
  begin
    if NotEnoughParams then
      MakeError(SNotEnoughActualParameters)
    else if TooManyParams then
      MakeError(STooManyActualParameters);
  end;
end;

{*
  Indique si les paramètres sont complétés
  @return True si les paramètres sont complétés, False sinon
*}
function TSepiCustomWithParams.GetParamsCompleted: Boolean;
begin
  Result := FParamsCompleted;
end;

{---------------------------}
{ TSepiCustomCallable class }
{---------------------------}

{*
  Crée une expression qui peut être invoquée
  @param ASignature       Signature (défaut = nil)
  @param APrepareParams   Si True, les paramètres sont préparés
*}
constructor TSepiCustomCallable.Create(ASignature: TSepiSignature = nil;
  APrepareParams: Boolean = False);
begin
  if APrepareParams and (ASignature <> nil) then
    inherited Create(ASignature.ParamCount)
  else
    inherited Create;

  FSignature := ASignature;
end;

{*
  Compile un paramètre
  @param Compiler           Compilateur de méthode
  @param Instructions       Liste d'instructions
  @param SignatureParam     Paramètre de la signature
  @param ParamValue         Valeur du paramètre
  @param Destination        Référence mémoire à la variable résultat
  @param InstrParamMemory   Référence mémoire au paramètre dans l'instruction
  @param TempVars           Gestionnaire de vie des variables temporaires
  @param FreeParamValue     Valeur à donner au paramètre $Free (défaut = False)
*}
procedure TSepiCustomCallable.CompileParam(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; SignatureParam: TSepiParam;
  ParamValue: ISepiValue; InstrParamMemory: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager; FreeParamValue: Boolean = False);
var
  ParamMemory: TSepiMemoryReference;
  AllocParamValue: Boolean;
  ReadableParam: ISepiReadableValue;
begin
  ParamMemory := nil;
  try
    case SignatureParam.HiddenKind of
      // Paramètre normal
      hpNormal:
      begin
        if SignatureParam.OpenArray then
          raise EAssertionFailed.Create(
            'Open array parameters not supported');

        if Supports(ParamValue, ISepiReadableValue, ReadableParam) then
        begin
          ReadableParam.CompileRead(Compiler, Instructions, ParamMemory,
            TempVars);
        end else
        begin
          (ParamValue as ISepiAddressableValue).CompileLoadAddress(
            Compiler, Instructions, ParamMemory, TempVars);
          ParamMemory := TSepiMemoryReference.Clone(ParamMemory);
          ParamMemory.AddOperation(adSimple);
          ParamMemory.Seal;
        end;

        InstrParamMemory.Assign(ParamMemory);
      end;

      // Paramètre Self
      hpSelf:
      begin
        Assert(ParamValue <> nil);

        (ParamValue as ISepiReadableValue).CompileRead(Compiler, Instructions,
          ParamMemory, TempVars);
        InstrParamMemory.Assign(ParamMemory);
      end;

      // Paramètre $Alloc - True ssi SelfValue.ValueType is TSepiMetaClass
      hpAlloc:
      begin
        Assert(ParamValue <> nil);

        AllocParamValue := ParamValue.ValueType is TSepiMetaClass;

        InstrParamMemory.SetSpace(msConstant);
        InstrParamMemory.SetConstant(AllocParamValue);
      end;

      // Paramètre $Free
      hpFree:
      begin
        InstrParamMemory.SetSpace(msConstant);
        InstrParamMemory.SetConstant(FreeParamValue);
      end;

      // Paramètre High$XXX
      hpOpenArrayHighValue:
      begin
        raise EAssertionFailed.Create(
          'Open array parameters not supported');
      end;
    end;
  finally
    ParamMemory.Free;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiCustomCallable.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if (SameGUID(IID, ISepiValue) or SameGUID(IID, ISepiReadableValue)) and
    (ReturnType = nil) then
    Result := E_NOINTERFACE
  else
    Result := inherited QueryInterface(IID, Obj);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomCallable.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  Expression.Attach(ISepiCallable, AsExpressionPart);

  if ReturnType <> nil then
  begin
    Expression.Attach(ISepiValue, AsExpressionPart);
    Expression.Attach(ISepiReadableValue, AsExpressionPart);
  end;
end;

{*
  Renseigne la signature
  @param ASignature   Signature
*}
procedure TSepiCustomCallable.SetSignature(ASignature: TSepiSignature;
  APrepareParams: Boolean = False);
begin
  if FSignature <> nil then
    raise ESepiCompilerError.Create(SSignatureAlreadyKnown);

  FSignature := ASignature;
  if APrepareParams then
    SetParamCount(ASignature.ParamCount);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomCallable.Finalize;
begin
end;

{*
  [@inheritDoc]
*}
function TSepiCustomCallable.GetReturnType: TSepiType;
begin
  if Signature = nil then
    Result := nil
  else
    Result := Signature.ReturnType;
end;

{*
  [@inheritDoc]
*}
function TSepiCustomCallable.GetValueType: TSepiType;
begin
  Result := ReturnType;
end;

{*
  [@inheritDoc]
*}
function TSepiCustomCallable.GetIsConstant: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
function TSepiCustomCallable.GetConstValuePtr: Pointer;
begin
  Result := nil;
end;

{*
  Compile les paramètres dans une instruction assembleur CALL
  Lorsqu'il y a un paramètre Self, et que SelfValue <> nil, c'est SelfValue qui
  est utilisé pour l'évaluer, et SelfMem est modifié en sortie pour pointer sur
  le résultat de cette évaluation (et ne doit donc pas être scellé). Mais si
  SelfValue = nil, alors SelfMem est utilisé pour le paramètre, et il doit
  avoir été scellé avant.
  @param CallInstr        Instruction CALL
  @param Compiler         Compilateur de méthode
  @param Instructions     Liste d'instructions
  @param Destination      Référence mémoire à la variable résultat
  @param SelfValue        Valeur pour le paramètre caché Self
  @param SelfMem          Référence mémoire au paramètre Self
  @param FreeParamValue   Valeur à donner au paramètre $Free (défaut = False)
*}
procedure TSepiCustomCallable.CompileParams(CallInstr: TSepiAsmCall;
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  Destination: TSepiMemoryReference; const SelfValue: ISepiReadableValue = nil;
  SelfMem: TSepiMemoryReference = nil; FreeParamValue: Boolean = False);
var
  Parameters: TSepiAsmCallParams;
  TempVars: TSepiTempVarsLifeManager;
  I, RealParamIndex: Integer;
  SignatureParam: TSepiParam;
  ParamValue: ISepiValue;
  InstrParamMemory: TSepiMemoryReference;
begin
  // If parameters are not valid, don't try anything
  if not Self.MatchesSignature(Signature) then
    Exit;

  // Prepare parameters
  CallInstr.Prepare(Signature);
  Parameters := CallInstr.Parameters;

  // Set result memory space
  if Parameters.Result <> nil then
  begin
    if Destination = nil then
      Parameters.Result.SetSpace(msNoResult)
    else
      Parameters.Result.Assign(Destination);
  end;

  // Compile parameters
  TempVars := TSepiTempVarsLifeManager.Create;
  try
    RealParamIndex := -1;

    for I := 0 to Signature.ActualParamCount-1 do
    begin
      SignatureParam := Signature.ActualParams[I];

      if SignatureParam.HiddenKind = hpNormal then
        Inc(RealParamIndex);

      InstrParamMemory := Parameters.Parameters[I].MemoryRef;

      if (SignatureParam.HiddenKind = hpSelf) and (SelfValue = nil) then
      begin
        Assert(SelfMem.IsSealed);
        InstrParamMemory.Assign(SelfMem);
      end else
      begin
        if SignatureParam.HiddenKind in [hpNormal, hpOpenArrayHighValue] then
          ParamValue := Params[RealParamIndex]
        else if SignatureParam.HiddenKind in [hpSelf, hpAlloc] then
          ParamValue := SelfValue
        else
          ParamValue := nil;

        CompileParam(Compiler, Instructions, SignatureParam, ParamValue,
          InstrParamMemory, TempVars, FreeParamValue);

        if (SignatureParam.HiddenKind = hpSelf) and (SelfMem <> nil) then
          SelfMem.Assign(InstrParamMemory);
      end;
    end;
  finally
    TempVars.EndAllLifes(Instructions.GetCurrentEndRef);
    TempVars.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomCallable.CompileNoResult(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList);
var
  DestTempVar: TSepiLocalVar;
  Destination: TSepiMemoryReference;
begin
  DestTempVar := nil;
  Destination := nil;
  try
    if ReturnType.SafeResultBehavior = rbParameter then
    begin
      DestTempVar := Compiler.Locals.AddTempVar(ReturnType);
      DestTempVar.HandleLife;
      DestTempVar.Life.BeginInstrInterval(Instructions.GetCurrentEndRef);

      Destination := TSepiMemoryReference.Create(Compiler);
      Destination.SetSpace(DestTempVar);
      Destination.Seal;
    end;

    CompileCall(Compiler, Instructions, Destination);

    if DestTempVar <> nil then
      DestTempVar.Life.EndInstrInterval(Instructions.GetCurrentEndRef);
  finally
    Destination.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomCallable.CompileRead(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
begin
  NeedDestination(Destination, ReturnType, Compiler, TempVars,
    Instructions.GetCurrentEndRef);

  CompileCall(Compiler, Instructions, Destination);
end;

{-----------------------}
{ TSepiMethodCall class }
{-----------------------}

{*
  Crée une expression d'invocation de méthode
  @param AMethod            Méthode à appeler
  @param ASelfValue         Valeur Self (défaut = nil)
  @param AForceStaticCall   Force un appel statique (défaut = False)
*}
constructor TSepiMethodCall.Create(AMethod: TSepiMethod;
  const ASelfValue: ISepiReadableValue = nil;
  AForceStaticCall: Boolean = False; AFreeParamValue: Boolean = False);
begin
  inherited Create(AMethod.Signature);

  FMethod := AMethod;
  FSelfValue := ASelfValue;
  FForceStaticCall := AForceStaticCall;

  FFreeParamValue := AFreeParamValue;
end;

{*
  Crée une expression d'invocation de méthode surchargée
  @param AOverloadedMethod   Méthode surchargée à appeler
  @param ASelfValue          Valeur Self (défaut = nil)
  @param AForceStaticCall    Force un appel statique (défaut = False)
*}
constructor TSepiMethodCall.Create(AOverloadedMethod: TSepiOverloadedMethod;
  const ASelfValue: ISepiReadableValue = nil;
  AForceStaticCall: Boolean = False; AFreeParamValue: Boolean = False);
begin
  inherited Create;

  FOverloadedMethod := AOverloadedMethod;
  FSelfValue := ASelfValue;
  FForceStaticCall := AForceStaticCall;

  FFreeParamValue := AFreeParamValue;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodCall.CheckSelfValueType;
const
  mkValidOnClass = [mkConstructor, mkClassProcedure, mkClassFunction];
begin
  if SelfValue = nil then
    Exit;

  // Can''t call an object method on a class value
  if SelfValue.ValueType is TSepiMetaClass then
  begin
    if not (Signature.Kind in mkValidOnClass) then
    begin
      MakeError(SCallPatternOnlyOnClassMethod);
      Exit;
    end;
  end;

  // Call a class method on an object: auto-dereference
  if SelfValue.ValueType is TSepiClass then
  begin
    if Signature.Kind in [mkClassProcedure, mkClassFunction] then
    begin
      SelfValue := TSepiDereferenceValue.MakeDereference(
        SelfValue) as ISepiReadableValue;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiMethodCall.GetReturnType: TSepiType;
begin
  if Signature = nil then
    Result := nil
  else if (Signature.Kind = mkConstructor) and (SelfValue <> nil) and
    (SelfValue.ValueType is TSepiMetaClass) then
    Result := TSepiMetaClass(SelfValue.ValueType).SepiClass
  else
    Result := Signature.ReturnType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodCall.CompleteParams;
var
  I: Integer;
  AMethod: TSepiMethod;
begin
  inherited;

  if Method <> nil then
    MatchesSignature(Signature)
  else
  begin
    for I := 0 to OverloadedMethod.MethodCount-1 do
    begin
      AMethod := OverloadedMethod.Methods[I];

      if MatchesSignature(AMethod.Signature, False) then
      begin
        FMethod := AMethod;
        SetSignature(AMethod.Signature);
        Exit;
      end;
    end;

    MakeError(SNoMatchingOverloadedMethod);
  end;

  if SelfValue <> nil then
    SelfValue.Finalize;
  CheckSelfValueType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodCall.CompileCall(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; Destination: TSepiMemoryReference);
var
  IsStaticCall: Boolean;
  CallInstr: TSepiAsmRefCall;
  SelfMem: TSepiMemoryReference;
begin
  IsStaticCall := (Method.LinkKind = mlkStatic) or
    (ForceStaticCall and (Method.LinkKind <> mlkInterface));

  if IsStaticCall then
  begin
    CallInstr := TSepiAsmStaticCall.Create(Compiler);
    SelfMem := nil;
  end else
  begin
    CallInstr := TSepiAsmDynamicCall.Create(Compiler);
    SelfMem := TSepiAsmDynamicCall(CallInstr).SelfMem;
  end;

  CallInstr.SourcePos := Expression.SourcePos;
  CallInstr.SetMethod(Method, False);

  CompileParams(CallInstr, Compiler, Instructions, Destination,
    SelfValue, SelfMem, FreeParamValue);

  Instructions.Add(CallInstr);
end;

{--------------------------}
{ TSepiMethodRefCall class }
{--------------------------}

{*
  Crée une expression d'invocation de méthode
  @param AMethodRefValue   Référence de méthode
*}
constructor TSepiMethodRefCall.Create(
  const AMethodRefValue: ISepiReadableValue = nil; AComplete: Boolean = False);
begin
  inherited Create;

  FMethodRefValue := AMethodRefValue;

  if AComplete then
    Complete;
end;

{*
  Complète l'appel de référence de méthode
  L'appel doit avoir été lié à une expression auparavant, pour bénéficier de
  son contexte.
*}
procedure TSepiMethodRefCall.Complete;
begin
  MethodRefValue.Finalize;
  SetSignature((MethodRefValue.ValueType as TSepiMethodRefType).Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodRefCall.CompleteParams;
begin
  inherited;

  MatchesSignature(Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodRefCall.CompileCall(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; Destination: TSepiMemoryReference);
var
  CallInstr: TSepiAsmAddressCall;
  MethodRefMemory, SelfMemory: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager;
begin
  CallInstr := TSepiAsmAddressCall.Create(Compiler);
  CallInstr.SourcePos := Expression.SourcePos;

  MethodRefMemory := nil;
  SelfMemory := nil;
  TempVars := TSepiTempVarsLifeManager.Create;
  try
    MethodRefValue.CompileRead(Compiler, Instructions, MethodRefMemory,
      TempVars);

    CallInstr.Address.Assign(MethodRefMemory);

    if Signature.SelfParam <> nil then
    begin
      SelfMemory := TSepiMemoryReference.Clone(MethodRefMemory);
      SelfMemory.AddOperation(aoPlusConstShortint, SizeOf(Pointer));
    end;

    CompileParams(CallInstr, Compiler, Instructions, Destination, nil,
      SelfMemory);

    Instructions.Add(CallInstr);
  finally
    TempVars.EndAllLifes(CallInstr.BeforeRef);
    TempVars.Free;

    SelfMemory.Free;
    MethodRefMemory.Free;
  end;
end;

{--------------------------}
{ TSepiPropertyValue class }
{--------------------------}

{*
  Crée une valeur propriété
  @param AObjectValue   Valeur objet
  @param AProperty      Propriété
*}
constructor TSepiPropertyValue.Create(const AObjectValue: ISepiReadableValue;
  AProperty: TSepiProperty);
begin
  inherited Create;

  FObjectValue := AObjectValue;
  FProperty := AProperty;

  ObjectValue.Finalize;

  SetParamCount(FProperty.Signature.ParamCount);
  if FProperty.Signature.ParamCount = 0 then
    CompleteParams;
end;

{*
  Construit une valeur représentant l'index de la propriété
*}
function TSepiPropertyValue.MakeIndexValue(
  Compiler: TSepiMethodCompiler; Method: TSepiMethod): ISepiReadableValue;
var
  Signature: TSepiSignature;
  IndexType: TSepiType;
begin
  if FProperty.Index = NoIndex then
  begin
    Result := nil;
    Exit;
  end;

  Signature := Method.Signature;
  IndexType := Signature.Params[Signature.ParamCount-1].ParamType;

  Result := TSepiTrueConstValue.MakeOrdinalValue(Compiler,
    IndexType as TSepiOrdType, FProperty.Index);
end;

{*
  [@inheritDoc]
*}
function TSepiPropertyValue.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if SameGUID(IID, ISepiReadableValue) and
    (FProperty.ReadAccess.Kind = pakNone) then
    Result := E_NOINTERFACE
  else if SameGUID(IID, ISepiWritableValue) and
    (FProperty.WriteAccess.Kind = pakNone) then
    Result := E_NOINTERFACE
  else
    Result := inherited QueryInterface(IID, Obj);
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyValue.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  Expression.Attach(ISepiProperty, AsExpressionPart);
  Expression.Attach(ISepiValue, AsExpressionPart);

  if FProperty.ReadAccess.Kind <> pakNone then
    Expression.Attach(ISepiReadableValue, AsExpressionPart);
  if FProperty.WriteAccess.Kind <> pakNone then
    Expression.Attach(ISepiWritableValue, AsExpressionPart);
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyValue.Finalize;
begin
end;

{*
  [@inheritDoc]
*}
function TSepiPropertyValue.GetValueType: TSepiType;
begin
  Result := FProperty.PropType;
end;

{*
  [@inheritDoc]
*}
function TSepiPropertyValue.GetIsConstant: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
function TSepiPropertyValue.GetConstValuePtr: Pointer;
begin
  Result := nil;
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyValue.CompleteParams;
begin
  inherited;

  MatchesSignature(FProperty.Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyValue.CompileRead(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  RightValue: ISepiReadableValue;
  Method: TSepiMethod;
  Callable: ISepiCallable;
  I: Integer;
  IndexValue: ISepiReadableValue;
begin
  if FProperty.ReadAccess.Kind = pakField then
  begin
    RightValue := TSepiObjectFieldValue.Create(ObjectValue,
      FProperty.ReadAccess.Field);
  end else
  begin
    Method := FProperty.ReadAccess.Method;
    Callable := TSepiMethodCall.Create(Method, ObjectValue);

    for I := 0 to ParamCount-1 do
      Callable.AddParam(Params[I]);

    IndexValue := MakeIndexValue(Compiler, Method);
    if IndexValue <> nil then
      Callable.AddParam(IndexValue);

    Callable.CompleteParams;

    RightValue := Callable as ISepiReadableValue;
  end;

  RightValue.AttachToExpression(TSepiExpression.Create(Expression));
  RightValue.CompileRead(Compiler, Instructions, Destination, TempVars);
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyValue.CompileWrite(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; Source: ISepiReadableValue);
var
  LeftValue: ISepiWritableValue;
  Method: TSepiMethod;
  Callable: ISepiCallable;
  I: Integer;
  IndexValue: ISepiReadableValue;
begin
  if FProperty.ReadAccess.Kind = pakField then
  begin
    LeftValue := TSepiObjectFieldValue.Create(ObjectValue,
      FProperty.ReadAccess.Field);
    LeftValue.AttachToExpression(TSepiExpression.Create(Expression));
    LeftValue.CompileWrite(Compiler, Instructions, Source);
  end else
  begin
    Method := FProperty.ReadAccess.Method;
    Callable := TSepiMethodCall.Create(Method, ObjectValue);

    for I := 0 to ParamCount-1 do
      Callable.AddParam(Params[I]);

    IndexValue := MakeIndexValue(Compiler, Method);
    if IndexValue <> nil then
      Callable.AddParam(IndexValue);

    Callable.AddParam(Source);

    Callable.CompleteParams;
    Callable.AttachToExpression(TSepiExpression.Create(Expression));
    Callable.CompileNoResult(Compiler, Instructions);
  end;
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
    toSizeOf:
    begin
      // Always valid
    end;
    toTypeInfo:
    begin
      if (not NilIfNoTypeInfo) and (OpType.TypeInfo = nil) then
        MakeError(STypeHasNoTypeInfo);
    end;
    toLowerBound, toHigherBound:
    begin
      if not ((OpType is TSepiOrdType) or (OpType is TSepiInt64Type) or
        (OpType is TSepiStaticArrayType)) then
        MakeError(SOrdinalOrArrayTypeRequired);
    end;
    toLength:
    begin
      if not (OpType is TSepiStaticArrayType) then
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
  ArrayType: TSepiStaticArrayType;
  Bound: Integer;
begin
  ArrayType := Operand.ExprType as TSepiStaticArrayType;

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
    if Operand.ExprType is TSepiStaticArrayType then
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
  Expression.Attach(ISepiValue, AsExpressionPart);
  Expression.Attach(ISepiNilValue, AsExpressionPart);
end;

{*
  [@inheritDoc]
*}
procedure TSepiNilValue.Finalize;
begin
end;

{*
  [@inheritDoc]
*}
function TSepiNilValue.GetValueType: TSepiType;
begin
  Result := nil;
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

