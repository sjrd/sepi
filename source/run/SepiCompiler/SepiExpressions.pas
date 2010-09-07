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
  Expressions Sepi
  @author sjrd
  @version 1.0
*}
unit SepiExpressions;

interface

uses
  SysUtils, Classes, Math, TypInfo, ScUtils, ScInterfaces, ScCompilerMagic,
  ScDelphiLanguage, ScIntegerSets, ScTypInfo, SepiReflectionCore, SepiOrdTypes,
  SepiStrTypes, SepiArrayTypes, SepiMembers, SepiSystemUnit, SepiOpCodes,
  SepiRuntimeOperations, SepiCompiler, SepiCompilerErrors, SepiCompilerConsts,
  SepiAsmInstructions;

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
    Expression qui peut être exécutée
    @author sjrd
    @version 1.0
  *}
  ISepiExecutable = interface(ISepiExpressionPart)
    ['{5AA4242D-0E43-4938-8BE1-612AC8FCC6C3}']

    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);
  end;

  {*
    Valeur
    @author sjrd
    @version 1.0
  *}
  ISepiValue = interface(ISepiExpressionPart)
    ['{20D185F7-7022-4B7F-9F94-2EDEBF2EFB1E}']

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
    Valeur tableau ouvert
    @author sjrd
    @version 1.0
  *}
  ISepiOpenArrayValue = interface(ISepiExpressionPart)
    ['{8040104A-E942-407F-B17A-7B5C90C963C5}']

    function GetElementType: TSepiType;

    function CanForceElementType(AElementType: TSepiType): Boolean;
    procedure ForceElementType(AElementType: TSepiType);

    procedure CompileReadOpenArray(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      var OpenArrayDest, HighValueDest: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property ElementType: TSepiType read GetElementType;
  end;

  {*
    Constructeur de tableau ouvert
    @author sjrd
    @version 1.0
  *}
  ISepiOpenArrayBuilder = interface(ISepiExpressionPart)
    ['{335E8CD4-677F-41EA-8567-3CDD2CB2B651}']

    procedure AddItem(const AItem: ISepiReadableValue);

    procedure Complete;
  end;

  {*
    Expression qui peut être appelée
    @author sjrd
    @version 1.0
  *}
  ISepiWantingParams = interface(ISepiExpressionPart)
    ['{CD26F811-9B78-4F42-ACD7-82B1FB93EA3F}']

    function GetParamCount: Integer;
    function GetParams(Index: Integer): ISepiExpression;
    function GetParamsCompleted: Boolean;

    procedure AddParam(const Value: ISepiExpression);
    procedure CompleteParams;

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiExpression read GetParams;
    property ParamsCompleted: Boolean read GetParamsCompleted;
  end;

  {*
    Propriété
    @author sjrd
    @version 1.0
  *}
  ISepiProperty = interface(ISepiValue)
    ['{9C5A0B51-BDB2-45FA-B2D4-179763CB7F16}']

    function GetParamCount: Integer;
    function GetParams(Index: Integer): ISepiExpression;
    procedure SetParams(Index: Integer; const Value: ISepiExpression);
    function GetParamsCompleted: Boolean;

    procedure CompleteParams;

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiExpression
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
    Expression qui représente un meta
    @author sjrd
    @version 1.0
  *}
  ISepiComponentExpression = interface(ISepiExpressionPart)
    ['{444E8E70-1173-44E5-AA91-5B28629A9AA4}']

    function GetComponent: TSepiComponent;

    property Component: TSepiComponent read GetComponent;
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
    Constructeur d'ensemble ou de tableau ouvert
    @author sjrd
    @version 1.0
  *}
  ISepiSetOrOpenArrayBuilder = interface(ISepiExpressionPart)
    ['{3AE1F41D-BBF6-4716-90D9-3EB32CB3720B}']

    procedure AddSingle(const Value: ISepiReadableValue);
    procedure AddRange(const Lower, Higher: ISepiReadableValue);

    procedure Complete;
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
    FSystemUnit: TSepiSystemUnit;         /// Unité System
    FBaseCompiler: TSepiCompilerBase;     /// Compilateur de base
    FUnitCompiler: TSepiUnitCompiler;     /// Compilateur d'unité
    FMethodCompiler: TSepiMethodCompiler; /// Compilateur de méthode
    FLanguageRules: TSepiLanguageRules;   /// Règles du langage utilisé

    function GetExpression: ISepiExpression;
  protected
    procedure AttachTo(const Controller: IInterface); override;
    procedure DetachFromController; override;

    procedure AttachToExpression(
      const Expression: ISepiExpression); virtual; abstract;

    procedure MakeError(const Msg: string; Kind: TSepiErrorKind = ekError);

    property Expression: ISepiExpression read GetExpression;

    property SepiRoot: TSepiRoot read FSepiRoot;
    property SystemUnit: TSepiSystemUnit read FSystemUnit;
    property BaseCompiler: TSepiCompilerBase read FBaseCompiler;
    property UnitCompiler: TSepiUnitCompiler read FUnitCompiler;
    property MethodCompiler: TSepiMethodCompiler read FMethodCompiler;
    property LanguageRules: TSepiLanguageRules read FLanguageRules;
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
  TSepiCustomComputedValue = class(TSepiCustomExpressionPart, ISepiValue,
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
    Classe de base pour les expressions exécutables
    @author sjrd
    @version 1.0
  *}
  TSepiCustomExecutable = class(TSepiCustomExpressionPart, ISepiExecutable)
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList); virtual; abstract;
  end;

  {*
    Valeur constante
    @author sjrd
    @version 1.0
  *}
  TSepiTrueConstValue = class(TSepiCustomDirectValue)
  private
    FConstant: TSepiConstant; /// Constante
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    constructor Create(AType: TSepiType); overload;
    constructor Create(Constant: TSepiConstant); overload;
    destructor Destroy; override;

    class function MakeValue(Compiler: TSepiCompilerBase;
      ValueType: TSepiType; const Value): ISepiReadableValue; overload;

    class function MakeValue(Compiler: TSepiCompilerBase;
      Constant: TSepiConstant): ISepiReadableValue; overload;

    property Constant: TSepiConstant read FConstant;
    property ConstValuePtr;
  end;

  {*
    Valeur constante litérale
    @author sjrd
    @version 1.0
  *}
  TSepiLiteralValue = class(TSepiCustomDirectValue)
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    constructor Create(AType: TSepiType);
  end;

  {*
    Valeur constante litérale entière
    @author sjrd
    @version 1.0
  *}
  TSepiIntegerLiteralValue = class(TSepiLiteralValue, ISepiTypeForceableValue)
  private
    FValue: Int64; /// Valeur constante
  protected
    function CanForceType(AValueType: TSepiType;
      Explicit: Boolean = False): Boolean;
    procedure ForceType(AValueType: TSepiType);
  public
    constructor Create(ASepiRoot: TSepiRoot; AValue: Int64);

    class function MakeValue(Compiler: TSepiCompilerBase;
      Value: Int64): ISepiReadableValue; overload;
    class function MakeValue(Compiler: TSepiCompilerBase;
      Value: Int64; ValueType: TSepiType): ISepiReadableValue; overload;

    property Value: Int64 read FValue;
  end;

  {*
    Valeur constante litérale flottante
    @author sjrd
    @version 1.0
  *}
  TSepiFloatLiteralValue = class(TSepiLiteralValue, ISepiTypeForceableValue)
  private
    FValue: Extended;            /// Valeur constante
    FConstValueBuffer: Extended; /// Buffer pour la valeur constante typée
  protected
    function CanForceType(AValueType: TSepiType;
      Explicit: Boolean = False): Boolean;
    procedure ForceType(AValueType: TSepiType);
  public
    constructor Create(ASepiRoot: TSepiRoot; AValue: Extended);

    class function MakeValue(Compiler: TSepiCompilerBase;
      Value: Extended): ISepiReadableValue;

    property Value: Extended read FValue;
  end;

  {*
    Valeur constante litérale chaîne
    @author sjrd
    @version 1.0
  *}
  TSepiStringLiteralValue = class(TSepiLiteralValue, ISepiTypeForceableValue)
  private
    FValue: string;       /// Valeur constante
    FPCharValue: Pointer; /// Valeur constante PChar
  protected
    function CanForceType(AValueType: TSepiType;
      Explicit: Boolean = False): Boolean;
    procedure ForceType(AValueType: TSepiType);
  public
    constructor Create(ASepiRoot: TSepiRoot; const AValue: string);
    destructor Destroy; override;

    class function MakeValue(Compiler: TSepiCompilerBase;
      const Value: string): ISepiReadableValue;

    property Value: string read FValue;
  end;

  {*
    Valeur erronée
    @author sjrd
    @version 1.0
  *}
  TSepiErroneousValue = class(TSepiCustomDirectValue, ISepiTypeForceableValue)
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;

    function CanForceType(AValueType: TSepiType;
      Explicit: Boolean = False): Boolean;
    procedure ForceType(AValueType: TSepiType);
  public
    constructor Create(ASepiRoot: TSepiRoot); overload;
    constructor Create(AValueType: TSepiType); overload;

    class function MakeReplacementValue(const OrigExpr: ISepiExpression;
      ValueType: TSepiType = nil): ISepiReadableValue;
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
    FVariable: TSepiVariable; /// Variable représentée
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    constructor Create(AVariable: TSepiVariable);

    class function MakeValue(Compiler: TSepiCompilerBase;
      Variable: TSepiVariable): ISepiValue;

    property Variable: TSepiVariable read FVariable;
  end;

  {*
    Valeur variable locale
    @author sjrd
    @version 1.0
  *}
  TSepiLocalVarValue = class(TSepiCustomDirectValue)
  private
    FLocalVar: TSepiLocalVar; /// Variable locale représentée
  protected
    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;
  public
    constructor Create(ALocalVar: TSepiLocalVar);

    class function MakeValue(Compiler: TSepiCompilerBase;
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

    class function ConversionExists(
      DestType, SrcType: TSepiType): Boolean; overload;
    class function ConversionExists(DestType: TSepiType;
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

    procedure CompileReadOperandInto(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Destination: TSepiMemoryReference;
      const Operand: ISepiReadableValue);

    procedure CompileShortCircuitAndOr(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); virtual;

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
    Opération in (appartenance à un ensemble)
    @author sjrd
    @version 1.0
  *}
  TSepiInSetOperation = class(TSepiCustomComputedValue)
  private
    FItemOperand: ISepiReadableValue; /// Opérande élément
    FSetOperand: ISepiReadableValue;  /// Opérande ensemble

    procedure CheckTypes;

    procedure CollapseConsts;
  protected
    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    constructor Create(ASepiRoot: TSepiRoot);

    procedure Complete;

    class function MakeOperation(const ItemOperand: ISepiReadableValue;
      const SetOperand: ISepiReadableValue): ISepiReadableValue;

    property ItemOperand: ISepiReadableValue
      read FItemOperand write FItemOperand;
    property SetOperand: ISepiReadableValue read FSetOperand write FSetOperand;
  end;

  {*
    Classe de base pour les opérations is et as
    @author sjrd
    @version 1.0
  *}
  TSepiIsAsOperation = class(TSepiCustomComputedValue)
  private
    FObjectOperand: ISepiReadableValue; /// Opérande objet
    FClassOperand: ISepiReadableValue;  /// Opérande classe

    procedure CheckTypes;
  protected
    procedure UpdateValueType; virtual;
  public
    constructor Create; virtual;

    procedure Complete;

    class function MakeOperation(const ObjectOperand: ISepiReadableValue;
      const ClassOperand: ISepiReadableValue): ISepiReadableValue;

    property ObjectOperand: ISepiReadableValue
      read FObjectOperand write FObjectOperand;
    property ClassOperand: ISepiReadableValue
      read FClassOperand write FClassOperand;
  end;

  {*
    Opération is
    @author sjrd
    @version 1.0
  *}
  TSepiIsOperation = class(TSepiIsAsOperation)
  protected
    procedure UpdateValueType; override;

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  end;

  {*
    Opération as
    @author sjrd
    @version 1.0
  *}
  TSepiAsOperation = class(TSepiIsAsOperation)
  protected
    procedure UpdateValueType; override;

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  end;

  {*
    Opération d'assignation
    @author sjrd
    @version 1.0
  *}
  TSepiAssignmentOperation = class(TSepiCustomExecutable)
  private
    FDestination: ISepiWritableValue; /// Destination
    FSource: ISepiReadableValue;      /// Source

    /// Si True, l'opérande source est automatiquement converti
    FAutoConvert: Boolean;
  protected
    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList); override;
  public
    constructor Create(AAutoConvert: Boolean = True);

    procedure Complete;

    class function MakeOperation(const Destination: ISepiWritableValue;
      const Source: ISepiReadableValue;
      AutoConvert: Boolean = True): ISepiExecutable;

    property Destination: ISepiWritableValue
      read FDestination write FDestination;
    property Source: ISepiReadableValue read FSource write FSource;

    property AutoConvert: Boolean read FAutoConvert write FAutoConvert;
  end;

  {*
    Appel inherited pur (même nom de méthode et mêmes paramètres)
    @author sjrd
    @version 1.0
  *}
  TSepiPureInheritedCall = class(TSepiCustomExecutable)
  protected
    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList); override;
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
    constructor Create(UnitCompiler: TSepiUnitCompiler);
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
    Valeur caractère d'une chaîne
    @author sjrd
    @version 1.0
  *}
  TSepiStringCharValue = class(TSepiCustomDirectValue)
  private
    FStringValue: ISepiValue;        /// Valeur chaîne
    FIndexValue: ISepiReadableValue; /// Valeur index

    FStringType: TSepiStringType; /// Type de chaîne

    FWriting: Boolean; /// True ssi dans CompileWrite
  protected
    procedure CompileUniqueStringCall(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      StringMemoryRef: TSepiMemoryReference);

    function CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference; override;

    procedure CompileWrite(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Source: ISepiReadableValue); override;

    property StringType: TSepiStringType read FStringType;
  public
    procedure Complete;

    class function MakeStringCharValue(const StringValue: ISepiValue;
      const IndexValue: ISepiReadableValue): ISepiValue;

    property StringValue: ISepiValue read FStringValue write FStringValue;
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
    Longueur d'une chaîne de caractères
    @author sjrd
    @version 1.0
  *}
  TSepiStringLengthValue = class(TSepiCustomComputedValue)
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

    class function MakeStringLengthValue(
      const Operand: ISepiReadableValue): ISepiReadableValue;

    property Operand: ISepiReadableValue read FOperand write FOperand;
  end;

  {*
    Type de borne de tableau
    - abkLow : Borne inférieure ;
    - abkHigh : Borne supérieure ;
    - abkLength : Longueur du tableau.
  *}
  TSepiArrayBoundKind = (abkLow, abkHigh, abkLength);

  {*
    Borne d'un tableau (Low, High ou Length)
    @author sjrd
    @version 1.0
  *}
  TSepiArrayBoundValue = class(TSepiCustomComputedValue)
  private
    FKind: TSepiArrayBoundKind;   /// Type de borne
    FOperand: ISepiReadableValue; /// Opérande

    FArrayType: TSepiArrayType; /// Type tableau de l'opérande

    procedure CheckTypes;
    procedure CollapseConsts;
  protected
    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;

    procedure CompileComputeDynArray(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); virtual;

    procedure CompileComputeOpenArray(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); virtual;

    property ArrayType: TSepiArrayType read FArrayType;
  public
    constructor Create(AKind: TSepiArrayBoundKind);

    procedure Complete;

    class function MakeArrayBoundValue(Kind: TSepiArrayBoundKind;
      const Operand: ISepiReadableValue): ISepiReadableValue;

    property Kind: TSepiArrayBoundKind read FKind;
    property Operand: ISepiReadableValue read FOperand write FOperand;
  end;

  {*
    Expression de changement de la longueur d'une chaîne de caractères
    @author sjrd
    @version 1.0
  *}
  TSepiStrSetLengthExpression = class(TSepiCustomExecutable)
  private
    FStrValue: ISepiValue;               /// Chaîne dont modifier la longueur
    FNewLengthValue: ISepiReadableValue; /// Nouvelle longueur

    procedure CheckTypes;
  protected
    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList); override;
  public
    procedure Complete;

    class function MakeStrSetLengthExpression(const StrValue: ISepiValue;
      const NewLengthValue: ISepiReadableValue): ISepiExecutable;

    property StrValue: ISepiValue read FStrValue write FStrValue;
    property NewLengthValue: ISepiReadableValue
      read FNewLengthValue write FNewLengthValue;
  end;

  {*
    Expression de changement de la longueur d'un tableau dynamique
    @author sjrd
    @version 1.0
  *}
  TSepiDynArraySetLengthExpression = class(TSepiCustomExecutable)
  private
    FDynArrayValue: ISepiValue;  /// Chaîne dont modifier la longueur
    FDimCount: Integer;          /// Nombre de dimensions
    FDimensions: IInterfaceList; /// Dimensions

    procedure CheckTypes;

    function GetDimensions(Index: Integer): ISepiReadableValue;
    procedure SetDimensions(Index: Integer; const Value: ISepiReadableValue);
  protected
    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList); override;
  public
    constructor Create(ADimCount: Integer = 1);

    procedure Complete;

    class function MakeDynArraySetLengthExpression(
      const DynArrayValue: ISepiValue;
      const Dimensions: array of ISepiReadableValue): ISepiExecutable;

    property DynArrayValue: ISepiValue read FDynArrayValue write FDynArrayValue;
    property DimCount: Integer read FDimCount;
    property Dimensions[Index: Integer]: ISepiReadableValue
      read GetDimensions write SetDimensions;
    property FirstDimension: ISepiReadableValue index 0
      read GetDimensions write SetDimensions;
  end;

  {*
    Expression copie d'une partie d'une chaîne de caractères
    @author sjrd
    @version 1.0
  *}
  TSepiStrCopyExpression = class(TSepiCustomComputedValue)
  private
    FSourceStr: ISepiReadableValue;  /// Chaîne source
    FIndexValue: ISepiReadableValue; /// Valeur de l'index
    FCountValue: ISepiReadableValue; /// Valeur du nombre de caractères à copier

    procedure CheckTypes;
    procedure CollapseConsts;
  protected
    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    procedure Complete;

    class function MakeStrCopy(const SourceStr: ISepiReadableValue;
      const IndexValue, CountValue: ISepiReadableValue): ISepiReadableValue;

    property SourceStr: ISepiReadableValue read FSourceStr write FSourceStr;
    property IndexValue: ISepiReadableValue read FIndexValue write FIndexValue;
    property CountValue: ISepiReadableValue read FCountValue write FCountValue;
  end;

  {*
    Expression copie d'une partie d'une chaîne de caractères
    @author sjrd
    @version 1.0
  *}
  TSepiDynArrayCopyExpression = class(TSepiCustomComputedValue)
  private
    FIsRange: Boolean; /// Indique si c'est la version range

    FSourceArray: ISepiReadableValue; /// Tableau source
    FIndexValue: ISepiReadableValue;  /// Valeur de l'index
    FCountValue: ISepiReadableValue;  /// Valeur du nombre d'éléments à copier

    procedure CheckTypes;
  protected
    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    constructor Create(AIsRange: Boolean);

    procedure Complete;

    class function MakeDynArrayCopy(
      const SourceArray: ISepiReadableValue): ISepiReadableValue; overload;
    class function MakeDynArrayCopyRange(const SourceArray: ISepiReadableValue;
      const IndexValue, CountValue: ISepiReadableValue):
      ISepiReadableValue; overload;

    property IsRange: Boolean read FIsRange;

    property SourceArray: ISepiReadableValue
      read FSourceArray write FSourceArray;
    property IndexValue: ISepiReadableValue read FIndexValue write FIndexValue;
    property CountValue: ISepiReadableValue read FCountValue write FCountValue;
  end;

  {*
    Expression d'inclusion ou exclusion d'un élément dans un ensemble
    @author sjrd
    @version 1.0
  *}
  TSepiSetIncludeExcludeExpression = class(TSepiCustomExecutable)
  private
    FIsInclude: Boolean; /// True pour Include, False pour Exclude

    FSetValue: ISepiValue;          /// Chaîne dont modifier la longueur
    FItemValue: ISepiReadableValue; /// Nouvelle longueur

    procedure CheckTypes;
  protected
    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList); override;
  public
    constructor Create(AIsInclude: Boolean);

    procedure Complete;

    class function MakeSetIncludeExcludeExpression(IsInclude: Boolean;
      const SetValue: ISepiValue;
      const ItemValue: ISepiReadableValue): ISepiExecutable;

    class function MakeSetIncludeExpression(const SetValue: ISepiValue;
      const ItemValue: ISepiReadableValue): ISepiExecutable;

    class function MakeSetExcludeExpression(const SetValue: ISepiValue;
      const ItemValue: ISepiReadableValue): ISepiExecutable;

    property IsInclude: Boolean read FIsInclude;

    property SetValue: ISepiValue read FSetValue write FSetValue;
    property ItemValue: ISepiReadableValue read FItemValue write FItemValue;
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
    function GetParams(Index: Integer): ISepiExpression;
    procedure SetParams(Index: Integer; const Value: ISepiExpression);

    procedure ClearParams;
    procedure AddParam(const Value: ISepiExpression);
    procedure DeleteParam(Index: Integer);

    function GetParamsCompleted: Boolean;

    procedure CompleteParams; virtual;

    function MatchesSignature(Signature: TSepiSignature;
      MakeErrors: Boolean = True): Boolean;

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiExpression
      read GetParams write SetParams;
    property ParamsCompleted: Boolean read FParamsCompleted;
  public
    constructor Create(AInitParamCount: Integer = 0);
  end;

  {*
    Classe de base pour les expressions qui peuvent être invoquées
    @author sjrd
    @version 1.0
  *}
  TSepiCustomCallable = class(TSepiCustomWithParams, ISepiWantingParams,
    ISepiExecutable, ISepiValue, ISepiReadableValue)
  private
    FSignature: TSepiSignature; /// Signature de l'appel

    procedure CompileParam(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; SignatureParam: TSepiParam;
      ParamValue: ISepiValue; InstrParamMemory: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager; FreeParamValue: Boolean = False);

    procedure CompileOpenArrayParam(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; SignatureParam: TSepiParam;
      const OpenArray: ISepiOpenArrayValue;
      InstrOpenArrayMemory, InstrHighValueMemory: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure SetSignature(ASignature: TSepiSignature;
      APrepareParams: Boolean = False);

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

    procedure CompileExecute(Compiler: TSepiMethodCompiler;
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
    constructor Create(AMethod: TSepiMethodBase;
      const ASelfValue: ISepiReadableValue = nil;
      AForceStaticCall: Boolean = False;
      AFreeParamValue: Boolean = False);

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

    procedure CompleteParams; override;
  public
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
    Valeur tableau ouvert construit à partir d'une valeur tableau
    @author sjrd
    @version 1.0
  *}
  TSepiOpenArrayFromArrayValue = class(TSepiCustomExpressionPart,
    ISepiOpenArrayValue)
  private
    FArrayValue: ISepiReadableValue; /// Valeur tableau à avoir

    procedure CheckTypes;
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetElementType: TSepiType;

    function CanForceElementType(AElementType: TSepiType): Boolean;
    procedure ForceElementType(AElementType: TSepiType);

    procedure CompileReadOpenArray(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      var OpenArrayDest, HighValueDest: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property ElementType: TSepiType read GetElementType;
  public
    procedure Complete;

    class function MakeOpenArrayValue(
      const ArrayValue: ISepiReadableValue): ISepiOpenArrayValue;

    property ArrayValue: ISepiReadableValue read FArrayValue write FArrayValue;
  end;

  {*
    Valeur tableau ouvert construit sur place
    @author sjrd
    @version 1.0
  *}
  TSepiOpenArrayBuilder = class(TSepiCustomExpressionPart,
    ISepiOpenArrayBuilder, ISepiOpenArrayValue)
  private
    FCompleted: Boolean;     /// Indique si le constructeur est complété
    FVarRecType: TSepiType;  /// Type TVarRec
    FElementType: TSepiType; /// Type des éléments
    FItems: IInterfaceList;  /// Éléments du tableau ouvert

    function GetVType(var ValueType: TSepiType): Byte;

    procedure CompileReadVarRecItem(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; const VarRecValue: ISepiWritableValue;
      const AItemValue: ISepiReadableValue; TempVars: TSepiTempVarsLifeManager);

    function GetItemCount: Integer;
    function GetItems(Index: Integer): ISepiReadableValue;
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure AddItem(const AItem: ISepiReadableValue);
    procedure Complete;

    function GetElementType: TSepiType;

    function CanForceElementType(AElementType: TSepiType): Boolean;
    procedure ForceElementType(AElementType: TSepiType);

    procedure CompileReadOpenArray(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      var OpenArrayDest, HighValueDest: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property ElementType: TSepiType read FElementType;

    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: ISepiReadableValue read GetItems;
  public
    constructor Create(ASepiRoot: TSepiRoot);

    property Completed: Boolean read FCompleted;
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

    class function MakeTypeOperationValue(Operation: TSepiTypeOperation;
      const Operand: ISepiTypeExpression;
      NilIfNoTypeInfo: Boolean = False): ISepiReadableValue;

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
  TSepiNilValue = class(TSepiCustomExpressionPart, ISepiValue,
    ISepiReadableValue, ISepiTypeForceableValue)
  private
    FValueType: TSepiType; /// Type de valeur
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetValueType: TSepiType;

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    function CanForceType(AValueType: TSepiType;
      Explicit: Boolean = False): Boolean;
    procedure ForceType(AValueType: TSepiType);
  public
    constructor Create(SepiRoot: TSepiRoot);

    class function MakeValue(Compiler: TSepiCompilerBase): ISepiReadableValue;
  end;

  {*
    Expression représentant un meta
    @author sjrd
    @version 1.0
  *}
  TSepiComponentExpression = class(TSepiCustomExpressionPart,
    ISepiComponentExpression)
  private
    FComponent: TSepiComponent; /// Component
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    function GetComponent: TSepiComponent;
  public
    constructor Create(AComponent: TSepiComponent);

    property Component: TSepiComponent read FComponent;
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

  {*
    Constructeur d'ensemble ou de tableau ouvert
    @author sjrd
    @version 1.0
  *}
  TSepiSetOrOpenArrayBuilder = class(TSepiCustomExpressionPart,
    ISepiSetOrOpenArrayBuilder, ISepiValue, ISepiReadableValue,
    ISepiTypeForceableValue, ISepiOpenArrayValue)
  private
    FCompleted: Boolean;                    /// True si complété
    FSingles: array of ISepiReadableValue;  /// Valeurs simples
    FRanges: array of TSepiSetBuilderRange; /// Intervalles de valeur

    FOpenArrayAvailable: Boolean; /// Indique si un tableau ouvert est possible

    FSetBuilder: ISepiSetBuilder;             /// Constructeur d'ensemble
    FOpenArrayBuilder: ISepiOpenArrayBuilder; /// Constructeur de tableau ouvert

    FSetBuilderCompleted: Boolean;       /// Indique si l'ensemble est complété
    FOpenArrayBuilderCompleted: Boolean; /// Indique si le tableau est complété

    procedure NeedSetBuilder;
    procedure NeedOpenArrayBuilder;
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure AttachToExpression(const Expression: ISepiExpression); override;

    // ISepiSetOrOpenArrayBuilder

    procedure AddSingle(const Value: ISepiReadableValue);
    procedure AddRange(const Lower, Higher: ISepiReadableValue);

    procedure Complete;

    // ISepiValue, ISepiReadableValue and ISepiTypeForceableValue

    function GetValueType: TSepiType;

    function CanForceType(AValueType: TSepiType; Explicit: Boolean): Boolean;
    procedure ForceType(AValueType: TSepiType);

    function GetIsConstant: Boolean;
    function GetConstValuePtr: Pointer;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    // ISepiOpenArrayValue

    function GetElementType: TSepiType;

    function CanForceElementType(AElementType: TSepiType): Boolean;
    procedure ForceElementType(AElementType: TSepiType);

    procedure CompileReadOpenArray(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList;
      var OpenArrayDest, HighValueDest: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    // Properties

    property Completed: Boolean read FCompleted;

    property SetBuilder: ISepiSetBuilder read FSetBuilder;
    property OpenArrayBuilder: ISepiOpenArrayBuilder read FOpenArrayBuilder;
  public
    constructor Create(UnitCompiler: TSepiUnitCompiler);
  end;

  {*
    Expression wrapper pour une instruction
    @author sjrd
    @version 1.0
  *}
  TSepiInstructionExpression = class(TSepiCustomExpressionPart,
    ISepiExecutable)
  private
    FInstruction: TSepiInstruction; /// Instruction wrappée
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure CompileExecute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);
  public
    constructor Create(AInstruction: TSepiInstruction);

    property Instruction: TSepiInstruction read FInstruction;
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

function SmallestIntegerTypeFor(SepiRoot: TSepiRoot; Value: Int64): TSepiType;

implementation

uses
  SepiCompilerUtils;

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
  StringKindToBaseType: array[TSepiStringKind] of TSepiBaseType = (
    btAnsiStr, btWideStr, btUnicodeStr
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
    BaseType := StringKindToBaseType[TSepiStringType(SepiType).StringKind]
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
    raise ESepiCompilerError.CreateFmt(STypeIsNotBaseType,
      [SepiType.DisplayName]);
end;

{*
  Trouve le type Sepi par défaut correspondant à un type de base
  @param BaseType   Type de base
  @param SepiRoot   Racine Sepi
  @return Type Sepi correspondant
*}
function DefaultSepiTypeFor(BaseType: TSepiBaseType;
  SepiRoot: TSepiRoot): TSepiType;
var
  SystemUnit: TSepiSystemUnit;
begin
  SystemUnit := SepiRoot.SystemUnit as TSepiSystemUnit;

  case BaseType of
    btBoolean:    Result := SystemUnit.Boolean;
    btByte:       Result := SystemUnit.Byte;
    btWord:       Result := SystemUnit.Word;
    btDWord:      Result := SystemUnit.LongWord;
    btShortint:   Result := SystemUnit.Shortint;
    btSmallint:   Result := SystemUnit.Smallint;
    btLongint:    Result := SystemUnit.Longint;
    btInt64:      Result := SystemUnit.Int64;
    btSingle:     Result := SystemUnit.Single;
    btDouble:     Result := SystemUnit.Double;
    btExtended:   Result := SystemUnit.Extended;
    btComp:       Result := SystemUnit.Comp;
    btCurrency:   Result := SystemUnit.Currency;
    btAnsiStr:    Result := SystemUnit.AnsiString;
    btWideStr:    Result := SystemUnit.WideString;
    btUnicodeStr: Result := SystemUnit.UnicodeString;
    btVariant:    Result := SystemUnit.Variant;
  else
    Assert(False);
    Result := nil;
  end;
end;

{*
  Plus petit type entier pouvant contenir l'entier spécifié
  @param SepiRoot   Racine Sepi (pour trouver le type Sepi)
  @param Value      Valeur entière
  @return Plus petit type entier pouvant contenir l'entier Value
*}
function SmallestIntegerTypeFor(SepiRoot: TSepiRoot; Value: Int64): TSepiType;
var
  SystemUnit: TSepiSystemUnit;
begin
  SystemUnit := SepiRoot.SystemUnit as TSepiSystemUnit;

  if (Value < Int64(-MaxInt-1)) or (Value > Int64(MaxInt)) then
    Result := SystemUnit.Int64
  else if Value < 0 then
  begin
    case IntegerSize(Value) of
      1: Result := SystemUnit.Shortint;
      2: Result := SystemUnit.Smallint;
      4: Result := SystemUnit.Longint;
    else
      Assert(False);
      Result := SystemUnit.Int64;
    end;
  end else
  begin
    case CardinalSize(Value) of
      1: Result := SystemUnit.Byte;
      2: Result := SystemUnit.Word;
      4: Result := SystemUnit.LongWord;
    else
      Assert(False);
      Result := SystemUnit.Int64;
    end;
  end;
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
    FSystemUnit := FSepiRoot.SystemUnit as TSepiSystemUnit;
    FBaseCompiler := ExprController.BaseCompiler;
    FUnitCompiler := ExprController.UnitCompiler;
    FMethodCompiler := ExprController.MethodCompiler;
    FLanguageRules := ExprController.LanguageRules;
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
    FSystemUnit := nil;
    FBaseCompiler := nil;
    FUnitCompiler := nil;
    FMethodCompiler := nil;
    FLanguageRules := nil;
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
  Assert(Expression <> nil);
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
  if (not Source.ValueType.Equals(ValueType)) and
    (not (ValueType is TSepiUntypedType)) then
  begin
    MakeError(Format(STypeMismatch,
      [Source.ValueType.DisplayName, ValueType.DisplayName]));
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
        MoveInstr := TSepiAsmMove.Create(Compiler, Source.ValueType);
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
  Type de valeur
  @return Type de valeur
*}
function TSepiCustomComputedValue.GetValueType: TSepiType;
begin
  Result := FValueType;
end;

{*
  Renseigne le type de valeur
  Si la constante avait déjà été allouée, et que AType n'est pas égal (au sens
  de Equals) à l'ancien type de valeur, alors elle est désallouée.
  @param AType   Type de valeur
*}
procedure TSepiCustomComputedValue.SetValueType(AType: TSepiType);
begin
  if (ConstValuePtr <> nil) and (not AType.Equals(ValueType)) then
  begin
    ValueType.DisposeValue(ConstValuePtr);
    FConstValuePtr := nil;
  end;

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
  Alloue la constante
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

{-----------------------------}
{ TSepiCustomExecutable class }
{-----------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiCustomExecutable.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  Expression.Attach(ISepiExecutable, AsExpressionPart);
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
  Construit une expression valeur pour une valeur quelconque
  @param Compiler    Compilateur
  @param ValueType   Type de la valeur
  @param Value       Valeur
  @return Valeur représentant la valeur Value de type ValueType
*}
class function TSepiTrueConstValue.MakeValue(Compiler: TSepiCompilerBase;
  ValueType: TSepiType; const Value): ISepiReadableValue;
var
  TrueConstValue: TSepiTrueConstValue;
begin
  TrueConstValue := TSepiTrueConstValue.Create(ValueType);
  ValueType.CopyData(Value, TrueConstValue.ConstValuePtr^);
  Result := TrueConstValue;
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{*
  Construit une expression valeur pour une constante globale
  @param Compiler   Compilateur
  @param Constant   Constante globale
  @return Valeur représentant la constante globale
*}
class function TSepiTrueConstValue.MakeValue(Compiler: TSepiCompilerBase;
  Constant: TSepiConstant): ISepiReadableValue;
begin
  Result := TSepiTrueConstValue.Create(Constant);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{-------------------}
{ TSepiLiteralValue }
{-------------------}

{*
  Crée une constante littérale
  @param AType   Type de la constante
*}
constructor TSepiLiteralValue.Create(AType: TSepiType);
begin
  inherited Create;

  SetValueType(AType);

  IsReadable := True;
end;

{*
  [@inheritDoc]
*}
function TSepiLiteralValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
begin
  Result := TSepiMemoryReference.Create(Compiler, aoAcceptAllConsts,
    ValueType.Size);
  try
    if IsZeroMemory(ConstValuePtr, ValueType.Size) then
      Result.SetSpace(msZero)
    else if ValueType.NeedInit then
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

{--------------------------------}
{ TSepiIntegerLiteralValue class }
{--------------------------------}

{*
  Crée une valeur constante litérale entière
  @param ASepiRoot   Racine Sepi
  @param AValue      Valeur constante
*}
constructor TSepiIntegerLiteralValue.Create(ASepiRoot: TSepiRoot;
  AValue: Int64);
begin
  if (AValue < -MaxInt-1) or (AValue > MaxInt) then
    inherited Create(TSepiSystemUnit.Get(ASepiRoot).Int64)
  else
    inherited Create(TSepiSystemUnit.Get(ASepiRoot).Integer);

  FValue := AValue;
  ConstValuePtr := @FValue;
end;

{*
  [@inheritDoc]
*}
function TSepiIntegerLiteralValue.CanForceType(AValueType: TSepiType;
  Explicit: Boolean): Boolean;
begin
  if not IsIntegerType(AValueType) then
    Result := False
  else if Explicit then
    Result := True
  else if AValueType is TSepiIntegerType then
    Result := TSepiIntegerType(AValueType).InRange(Value)
  else
    Result := TSepiInt64Type(AValueType).InRange(Value);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIntegerLiteralValue.ForceType(AValueType: TSepiType);
begin
  Assert(CanForceType(AValueType, True));

  SetValueType(AValueType);
end;

{*
  Construit une valeur constante litérale entière
  @param Compiler   Compilateur
  @param Value      Valeur constante
  @return Valeur constante litérale entière
*}
class function TSepiIntegerLiteralValue.MakeValue(Compiler: TSepiCompilerBase;
  Value: Int64): ISepiReadableValue;
begin
  Result := TSepiIntegerLiteralValue.Create(Compiler.SepiRoot, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{*
  Construit une valeur constante litérale entière
  @param Compiler    Compilateur
  @param Value       Valeur constante
  @param ValueType   Type de la valeur
  @return Valeur constante litérale entière
*}
class function TSepiIntegerLiteralValue.MakeValue(Compiler: TSepiCompilerBase;
  Value: Int64; ValueType: TSepiType): ISepiReadableValue;
begin
  Result := MakeValue(Compiler, Value);
  (Result as ISepiTypeForceableValue).ForceType(ValueType);
end;

{------------------------------}
{ TSepiFloatLiteralValue class }
{------------------------------}

{*
  Crée une valeur constante litérale flottante
  @param ASepiRoot   Racine Sepi
  @param AValue      Valeur constante
*}
constructor TSepiFloatLiteralValue.Create(ASepiRoot: TSepiRoot;
  AValue: Extended);
begin
  inherited Create(TSepiSystemUnit.Get(ASepiRoot).Extended);

  FValue := AValue;
  FConstValueBuffer := FValue;
  ConstValuePtr := @FConstValueBuffer;
end;

{*
  [@inheritDoc]
*}
function TSepiFloatLiteralValue.CanForceType(AValueType: TSepiType;
  Explicit: Boolean): Boolean;
begin
  Result := AValueType is TSepiFloatType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiFloatLiteralValue.ForceType(AValueType: TSepiType);
begin
  Assert(CanForceType(AValueType, True));

  SetValueType(AValueType);

  case TSepiFloatType(ValueType).FloatType of
    ftSingle:   Single  (ConstValuePtr^) := FValue;
    ftDouble:   Double  (ConstValuePtr^) := FValue;
    ftExtended: Extended(ConstValuePtr^) := FValue;
    ftComp:     Comp    (ConstValuePtr^) := FValue;
    ftCurr:     Currency(ConstValuePtr^) := FValue;
  else
    Assert(False);
  end;
end;

{*
  Construit une valeur constante litérale flottante
  @param Compiler   Compilateur
  @param Value      Valeur constante
  @return Valeur constante litérale entière
*}
class function TSepiFloatLiteralValue.MakeValue(Compiler: TSepiCompilerBase;
  Value: Extended): ISepiReadableValue;
begin
  Result := TSepiFloatLiteralValue.Create(Compiler.SepiRoot, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{-------------------------------}
{ TSepiStringLiteralValue class }
{-------------------------------}

{*
  Crée une valeur constante litérale chaîne
  @param ASepiRoot   Racine Sepi
  @param AValue      Valeur constante
*}
constructor TSepiStringLiteralValue.Create(ASepiRoot: TSepiRoot;
  const AValue: string);
var
  SystemUnit: TSepiSystemUnit;
begin
  SystemUnit := TSepiSystemUnit.Get(ASepiRoot);

  inherited Create(SystemUnit.LongString);

  FValue := AValue;

  ConstValuePtr := ValueType.NewValue;
  string(ConstValuePtr^) := FValue;

  if Length(Value) = 1 then
  begin
    if Value[1] <= Chr($FF) then
      ForceType(SystemUnit.AnsiChar)
    else
      ForceType(SystemUnit.WideChar);
  end;
end;

{*
  [@inheritDoc]
*}
destructor TSepiStringLiteralValue.Destroy;
begin
  if FPCharValue <> nil then
    FreeMem(FPCharValue);

  if ConstValuePtr <> nil then
    ValueType.DisposeValue(ConstValuePtr);

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiStringLiteralValue.CanForceType(AValueType: TSepiType;
  Explicit: Boolean): Boolean;
begin
  if AValueType is TSepiCharType then
    Result := Length(Value) = 1
  else if AValueType is TSepiPointerType then
    Result := TSepiPointerType(AValueType).PointTo is TSepiCharType
  else
    Result := (AValueType is TSepiStringType) or
      (AValueType is TSepiShortStringType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiStringLiteralValue.ForceType(AValueType: TSepiType);
var
  AnsiValue: AnsiString;
  WideValue: WideString;
  Size, Len, MaxLength: Integer;
begin
  Assert(CanForceType(AValueType, True));

  if FPCharValue <> nil then
    FreeMem(FPCharValue);

  ValueType.DisposeValue(ConstValuePtr);
  SetValueType(AValueType);
  ConstValuePtr := ValueType.NewValue;

  case ValueType.Kind of
    // PChar
    tkPointerOrUnknown:
    begin
      case (ValueType as TSepiPointerType).PointTo.Kind of
        tkChar:
        begin
          AnsiValue := AnsiString(Value);
          Size := SizeOf(AnsiChar) * (Length(AnsiValue)+1);
          GetMem(FPCharValue, Size);
          Move(AnsiValue[1], FPCharValue^, Size);
        end;

        tkWChar:
        begin
          WideValue := WideString(Value);
          Size := SizeOf(WideChar) * (Length(WideValue)+1);
          GetMem(FPCharValue, Size);
          Move(WideValue[1], FPCharValue^, Size);
        end;
      else
        Assert(False);
      end;
    end;

    // Characters
    tkChar: AnsiChar(ConstValuePtr^) := AnsiChar(Value[1]);
    tkWChar: WideChar(ConstValuePtr^) := WideChar(Value[1]);

    // Short string
    tkString:
    begin
      AnsiValue := AnsiString(Value);
      Len := Length(AnsiValue);
      MaxLength := (ValueType as TSepiShortStringType).MaxLength;

      if Len > MaxLength then
      begin
        MakeError(Format(SShortStringTooLong, [Len, MaxLength]));
        AnsiValue := Copy(AnsiValue, 1, MaxLength);
      end;

      ShortString(ConstValuePtr^) := AnsiValue;
    end;

    {$IFNDEF UNICODE}
    // Long strings in non-Unicode mode
    tkLString: AnsiString(ConstValuePtr^) := AnsiString(Value);
    tkWString: WideString(ConstValuePtr^) := WideString(Value);
    {$ELSE}
    // Long strings in Unicode mode
    tkLString:
      LStrFromUStr(PAnsiString(ConstValuePtr)^, Value,
        TSepiStringType(ValueType).CodePage);
    tkWString: WideString(ConstValuePtr^) := WideString(Value);
    tkUString: UnicodeString(ConstValuePtr^) := Value;
    {$ENDIF}
  else
    Assert(False);
  end;
end;

{*
  Construit une valeur constante litérale chaîne
  @param Compiler   Compilateur
  @param Value      Valeur constante
  @return Valeur constante litérale chaîne
*}
class function TSepiStringLiteralValue.MakeValue(Compiler: TSepiCompilerBase;
  const Value: string): ISepiReadableValue;
begin
  Result := TSepiStringLiteralValue.Create(Compiler.SepiRoot, Value);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{---------------------------}
{ TSepiErroneousValue class }
{---------------------------}

{*
  Crée une valeur erronée d'un type inconnu
  @param ASepiRoot   RacineSepi
*}
constructor TSepiErroneousValue.Create(ASepiRoot: TSepiRoot);
begin
  Create((ASepiRoot.SystemUnit as TSepiSystemUnit).Integer);
end;

{*
  Crée une valeur erronée d'un type donné
  @param AValueType   Type de la valeur erronée
*}
constructor TSepiErroneousValue.Create(AValueType: TSepiType);
begin
  inherited Create;

  SetValueType(AValueType);
  ConstValuePtr := @ZeroMemory;

  IsReadable := True;
end;

{*
  [@inheritDoc]
*}
function TSepiErroneousValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
begin
  Result := TSepiMemoryReference.Create(Compiler, [aoAcceptZero]);
  Result.SetSpace(msZero);
  Result.Seal;
end;

{*
  [@inheritDoc]
*}
function TSepiErroneousValue.CanForceType(AValueType: TSepiType;
  Explicit: Boolean): Boolean;
begin
  Result := True;
end;

{*
  [@inheritDoc]
*}
procedure TSepiErroneousValue.ForceType(AValueType: TSepiType);
begin
  SetValueType(AValueType);
end;

{*
  Construit une valeur erronée pour remplacer une expression non valide
  @param OrigExpr    Expression à remplacer
  @param ValueType   Type de la valeur erronée
  @return Valeur erronée créée
*}
class function TSepiErroneousValue.MakeReplacementValue(
  const OrigExpr: ISepiExpression;
  ValueType: TSepiType = nil): ISepiReadableValue;
begin
  if ValueType = nil then
    ValueType := OrigExpr.UnitCompiler.SystemUnit.Integer;

  Result := TSepiErroneousValue.Create(ValueType);
  Result.AttachToExpression(TSepiExpression.Create(OrigExpr));
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
  Construit une expression valeur pour une variable globale
  @param Compiler   Compilateur
  @param Variable   Variable globale
  @return Valeur représentant la variable globale
*}
class function TSepiVariableValue.MakeValue(Compiler: TSepiCompilerBase;
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

  IsReadable := True;
  IsWritable := not LocalVar.IsConstant;
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
class function TSepiLocalVarValue.MakeValue(Compiler: TSepiCompilerBase;
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
  SystemUnit: TSepiSystemUnit;
begin
  if not (IsOrd or IsChr) then
    Exit;

  OpType := Operand.ValueType;

  if not (OpType is TSepiOrdType) then
  begin
    MakeError(SOrdinalTypeRequired);
    Exit;
  end;

  SystemUnit := UnitCompiler.SystemUnit;

  if IsChr then
  begin
    FForceCast := True;

    if OpType.Size = 1 then
      SetValueType(SystemUnit.AnsiChar)
    else
      SetValueType(SystemUnit.WideChar);
  end else
  begin
    case TSepiOrdType(OpType).TypeData.OrdType of
      otSByte: SetValueType(SystemUnit.Shortint);
      otUByte: SetValueType(SystemUnit.Byte);
      otSWord: SetValueType(SystemUnit.Smallint);
      otUWord: SetValueType(SystemUnit.Word);
      otSLong: SetValueType(SystemUnit.Longint);
      otULong: SetValueType(SystemUnit.LongWord);
    else
      Assert(False);
    end;
  end;
end;

{*
  Pliage des constantes
*}
procedure TSepiCastOperator.CollapseConsts;
var
  ConstOp: ISepiReadableValue;
  OpType: TSepiType;
begin
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
  if OpType is TSepiUntypedType then
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
  if not (Operand.ValueType is TSepiUntypedType) then
  begin
    Source := TSepiCastOperator.CastValue(Operand.ValueType,
      Source) as ISepiReadableValue;
  end;

  (Operand as ISepiWritableValue).CompileWrite(Compiler, Instructions, Source);
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

  // Get operand type
  OpType := Operand.ValueType;

  // Handle Ord or Chr cast - exit if failed
  HandleOrdChr;
  if ValueType = nil then
    Exit;

  // Update access flags
  IsReadable := Supports(Operand, ISepiReadableValue);
  IsWritable := Supports(Operand, ISepiWritableValue);
  IsAddressable := Supports(Operand, ISepiAddressableValue);

  // Check types
  if (not (OpType is TSepiUntypedType)) and (OpType.Size <> ValueType.Size) then
  begin
    if (OpType.Size < ValueType.Size) or (not ForceCast) then
    begin
      MakeError(Format(SInvalidCast,
        [OpType.DisplayName, ValueType.DisplayName]));
      Exit;
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

  if SrcType.Equals(ValueType) then
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
  end else if SrcType is TSepiMetaClass then
  begin
    Assert(ValueType is TSepiMetaClass);

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

  if not ConversionExists(ValueType, Source) then
  begin
    MakeError(Format(STypeMismatch,
      [ValueType.DisplayName, Source.ValueType.DisplayName]));
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
class function TSepiConvertOperation.ConversionExists(
  DestType, SrcType: TSepiType): Boolean;
var
  DestBase, SrcBase: TSepiBaseType;
begin
  // Trivial case: types are equal
  if DestType.Equals(SrcType) then
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
  end else if SrcType is TSepiMetaClass then
  begin
    if DestType is TSepiMetaClass then
      Result := DestType.CompatibleWith(SrcType)
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
class function TSepiConvertOperation.ConversionExists(DestType: TSepiType;
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
    Result := ConversionExists(DestType, SrcType);
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

  Result := ConvertOp;
  Result.AttachToExpression(TSepiExpression.Create(Value as ISepiExpression));

  ConvertOp.Complete;
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
  if SelfOp and (not (Operation in opComparisonOps)) then
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
    if not ((ValueType is TSepiIntegerType) or (ValueType is TSepiInt64Type) or
      (ValueType is TSepiBooleanType)) then
    begin
      ErrorTypeNotApplicable;
      Operand := TSepiErroneousValue.MakeReplacementValue(
        Operand as ISepiExpression, SystemUnit.Boolean);
      SetValueType(Operand.ValueType);
    end;
  end else
  begin
    if not (ValueType.Kind in [tkInteger, tkFloat, tkVariant, tkInt64]) then
    begin
      ErrorTypeNotApplicable;
      Operand := TSepiErroneousValue.MakeReplacementValue(
        Operand as ISepiExpression, SystemUnit.Integer);
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
  if (not AutoConvert) and (not LeftType.Equals(RightType)) then
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
    [LeftOperand.ValueType.DisplayName, RightOperand.ValueType.DisplayName]));
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
  Compile la lecture d'un opérande dans une référence mémoire spécifique
  @param Compiler       Compilateur
  @param Instructions   Liste d'instructions
  @param Destination    Référence mémoire où stocker le résultat
  @param Operand        Opérande à lire
*}
procedure TSepiBinaryOperation.CompileReadOperandInto(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  Destination: TSepiMemoryReference; const Operand: ISepiReadableValue);
var
  OperandMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  MoveInstr: TSepiAsmMove;
begin
  OperandMemory := Destination;
  try
    // Read operand
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      Operand.CompileRead(Compiler, Instructions, OperandMemory, SrcTempVars);
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Copy to destination
    if OperandMemory <> Destination then
    begin
      MoveInstr := TSepiAsmMove.Create(Compiler, ValueType);
      MoveInstr.Destination.Assign(Destination);
      MoveInstr.Source.Assign(OperandMemory);

      Instructions.Add(MoveInstr);
    end;
  finally
    if OperandMemory <> Destination then
      OperandMemory.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiBinaryOperation.CompileShortCircuitAndOr(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
var
  JumpInstr: TSepiAsmCondJump;
begin
  // In the general case, we need a totally independant variable, unfortunately
  Destination := nil;
  NeedDestination(Destination, ValueType, Compiler, TempVars,
    Instructions.GetCurrentEndRef);

  CompileReadOperandInto(Compiler, Instructions, Destination, LeftOperand);

  JumpInstr := TSepiAsmCondJump.Create(Compiler, Operation = opOr);
  JumpInstr.Test.Assign(Destination);
  Instructions.Add(JumpInstr);

  CompileReadOperandInto(Compiler, Instructions, Destination, RightOperand);

  JumpInstr.Destination.InstructionRef := Instructions.GetCurrentEndRef;
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
  if (Operation in [opAnd, opOr]) and (ValueType is TSepiBooleanType) then
  begin
    CompileShortCircuitAndOr(Compiler, Instructions, Destination, TempVars);
    Exit;
  end;

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

  StrResults: array[btAnsiChar..btUnicodeStr, btAnsiChar..btUnicodeStr] of
      TSepiBaseType = (
    (btAnsiChar  , btWideChar  , btAnsiStr   , btWideStr   , btUnicodeStr),
    (btWideChar  , btWideChar  , btWideStr   , btWideStr   , btUnicodeStr),
    (btAnsiStr   , btWideStr   , btAnsiStr   , btWideStr   , btUnicodeStr),
    (btWideStr   , btWideStr   , btWideStr   , btWideStr   , btUnicodeStr),
    (btUnicodeStr, btUnicodeStr, btUnicodeStr, btUnicodeStr, btUnicodeStr)
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
  end else if Types * btCharsAndStrings <> [] then
  begin
    // Strings only matches with themselves - prefer wide and prefer string
    if not (Types <= btCharsAndStrings) then
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

{---------------------------}
{ TSepiInSetOperation class }
{---------------------------}

{*
  Crée une opération is ou as
*}
constructor TSepiInSetOperation.Create(ASepiRoot: TSepiRoot);
begin
  inherited Create;

  SetValueType(TSepiSystemUnit(ASepiRoot.SystemUnit).Boolean);
end;

{*
  Vérifie le type des opérandes
*}
procedure TSepiInSetOperation.CheckTypes;
var
  TempItemType, TempSetType: TSepiType;
  TempExpression: ISepiExpression;
  ItemType: TSepiOrdType;
  SetType: TSepiSetType;
  ForceableSet: ISepiTypeForceableSetValue;
begin
  TempItemType := ItemOperand.ValueType;
  TempSetType := SetOperand.ValueType;

  // Check item type

  if not (TempItemType is TSepiOrdType) then
  begin
    TempExpression := ItemOperand as ISepiExpression;

    if TempSetType is TSepiSetType then
      TempItemType := TSepiSetType(TempSetType).CompType
    else
      TempItemType := UnitCompiler.SystemUnit.Byte;

    TempExpression.MakeError(SOrdinalTypeRequired);
    ItemOperand := TSepiErroneousValue.Create(TempItemType);
    ItemOperand.AttachToExpression(TSepiExpression.Create(TempExpression));
  end;

  ItemType := TempItemType as TSepiOrdType;

  // Check set type

  if not (TempSetType is TSepiSetType) then
  begin
    TempExpression := SetOperand as ISepiExpression;

    TempSetType := UnitCompiler.MakeSetType(ItemType);

    TempExpression.MakeError(SSetTypeRequired);
    SetOperand := TSepiErroneousValue.Create(TempSetType);
    SetOperand.AttachToExpression(TSepiExpression.Create(TempExpression));
  end;

  SetType := TempSetType as TSepiSetType;

  // Check compatibility between types

  if not SetType.CompType.Equals(ItemType) then
  begin
    if TSepiConvertOperation.ConversionExists(SetType.CompType,
      ItemOperand) then
    begin
      ItemOperand := TSepiConvertOperation.ConvertValue(SetType.CompType,
        ItemOperand);
    end else if Supports(SetOperand, ISepiTypeForceableSetValue,
      ForceableSet) and
      ForceableSet.CanForceCompType(ItemType) then
    begin
      ForceableSet.ForceCompType(ItemType);
    end else
    begin
      TempExpression := ItemOperand as ISepiExpression;

      TempExpression.MakeError(Format(STypeMismatch,
        [ItemType.DisplayName, SetType.CompType.DisplayName]));
      ItemOperand := TSepiErroneousValue.Create(SetType.CompType);
      ItemOperand.AttachToExpression(TSepiExpression.Create(TempExpression));
    end;
  end;
end;

{*
  Pliage des constantes
*}
procedure TSepiInSetOperation.CollapseConsts;
type
  TSet = set of Byte;
begin
  if (not ItemOperand.IsConstant) or (not SetOperand.IsConstant) then
    Exit;

  AllocateConstant;

  Boolean(ConstValuePtr^) :=
    Byte(ItemOperand.ConstValuePtr^) in TSet(SetOperand.ConstValuePtr^);
end;

{*
  [@inheritDoc]
*}
procedure TSepiInSetOperation.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  ItemMemory, SetMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  Instruction: TSepiAsmSetIn;
begin
  ItemMemory := nil;
  SetMemory := nil;
  try
    // Read operands
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      ItemOperand.CompileRead(Compiler, Instructions, ItemMemory,
        SrcTempVars);
      SetOperand.CompileRead(Compiler, Instructions, SetMemory,
        SrcTempVars);
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Make instruction
    Instruction := TSepiAsmSetIn.Create(Compiler);
    Instruction.SourcePos := Expression.SourcePos;

    NeedDestination(Destination, ValueType, Compiler, TempVars,
      Instruction.AfterRef);

    Instruction.Destination.Assign(Destination);
    Instruction.SetValue.Assign(SetMemory);
    Instruction.Element.Assign(ItemMemory);

    Instructions.Add(Instruction);
  finally
    ItemMemory.Free;
    SetMemory.Free;
  end;
end;

{*
  Complète l'opération
  L'opération doit avoir été attachée à une expression auparavant, pour pouvoir
  bénéficier du contexte de celle-ci.
*}
procedure TSepiInSetOperation.Complete;
begin
  Assert(Expression <> nil);
  Assert((ItemOperand <> nil) and (SetOperand <> nil));

  CheckTypes;
  CollapseConsts;
end;

{*
  Construit une opération binaire
  @param LeftOperand    Opérande de gauche
  @param RightOperand   Opérande de droite
  @return Valeur représentant l'opération
*}
class function TSepiInSetOperation.MakeOperation(const ItemOperand,
  SetOperand: ISepiReadableValue): ISepiReadableValue;
var
  Operation: TSepiInSetOperation;
begin
  Operation := Create((ItemOperand as ISepiExpression).SepiRoot);
  Result := Operation;
  Result.AttachToExpression(
    TSepiExpression.Create(ItemOperand as ISepiExpression));
  Operation.ItemOperand := ItemOperand;
  Operation.SetOperand := SetOperand;
  Operation.Complete;
end;

{--------------------------}
{ TSepiIsAsOperation class }
{--------------------------}

{*
  Crée une opération is ou as
*}
constructor TSepiIsAsOperation.Create;
begin
  inherited Create;
end;

{*
  Vérifie le type des opérandes
*}
procedure TSepiIsAsOperation.CheckTypes;
var
  ObjectType, ClassType: TSepiType;
  TempExpression: ISepiExpression;
begin
  ObjectType := ObjectOperand.ValueType;
  ClassType := ClassOperand.ValueType;

  if not (ObjectType is TSepiClass) then
  begin
    TempExpression := ObjectOperand as ISepiExpression;

    TempExpression.MakeError(SClassTypeRequired);
    ObjectOperand := TSepiErroneousValue.Create(
      UnitCompiler.SystemUnit.TObject);
    ObjectOperand.AttachToExpression(TSepiExpression.Create(TempExpression));
  end;

  if not (ClassType is TSepiMetaClass) then
  begin
    TempExpression := ClassOperand as ISepiExpression;

    TempExpression.MakeError(SMetaClassTypeRequired);
    ClassOperand := TSepiErroneousValue.Create(
      UnitCompiler.SystemUnit.TClass);
    ClassOperand.AttachToExpression(TSepiExpression.Create(TempExpression));
  end;
end;

{*
  Met à jour le type de valeur suite à la complétion de l'opération
*}
procedure TSepiIsAsOperation.UpdateValueType;
begin
end;

{*
  Complète l'opération
  L'opération doit avoir été attachée à une expression auparavant, pour pouvoir
  bénéficier du contexte de celle-ci.
*}
procedure TSepiIsAsOperation.Complete;
begin
  Assert(Expression <> nil);
  Assert((ObjectOperand <> nil) and (ClassOperand <> nil));

  CheckTypes;
  UpdateValueType;
end;

{*
  Construit une opération binaire
  @param LeftOperand    Opérande de gauche
  @param RightOperand   Opérande de droite
  @return Valeur représentant l'opération
*}
class function TSepiIsAsOperation.MakeOperation(const ObjectOperand,
  ClassOperand: ISepiReadableValue): ISepiReadableValue;
var
  Operation: TSepiIsAsOperation;
begin
  Operation := Create;
  Result := Operation;
  Result.AttachToExpression(
    TSepiExpression.Create(ObjectOperand as ISepiExpression));
  Operation.ObjectOperand := ObjectOperand;
  Operation.ClassOperand := ClassOperand;
  Operation.Complete;
end;

{------------------------}
{ TSepiIsOperation class }
{------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiIsOperation.UpdateValueType;
begin
  SetValueType(UnitCompiler.SystemUnit.Boolean);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIsOperation.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  ObjectMemory, ClassMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  Instruction: TSepiAsmIsClass;
begin
  ObjectMemory := nil;
  ClassMemory := nil;
  try
    // Read operands
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      ObjectOperand.CompileRead(Compiler, Instructions, ObjectMemory,
        SrcTempVars);
      ClassOperand.CompileRead(Compiler, Instructions, ClassMemory,
        SrcTempVars);
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Make instruction
    Instruction := TSepiAsmIsClass.Create(Compiler);
    Instruction.SourcePos := Expression.SourcePos;

    NeedDestination(Destination, ValueType, Compiler, TempVars,
      Instruction.AfterRef);

    Instruction.Destination.Assign(Destination);
    Instruction.MemObject.Assign(ObjectMemory);
    Instruction.MemClass.Assign(ClassMemory);

    Instructions.Add(Instruction);
  finally
    ObjectMemory.Free;
    ClassMemory.Free;
  end;
end;

{------------------------}
{ TSepiAsOperation class }
{------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiAsOperation.UpdateValueType;
begin
  SetValueType((ClassOperand.ValueType as TSepiMetaClass).SepiClass);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsOperation.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  ClassMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  Instruction: TSepiAsmAsClass;
begin
  // Never write into actual destination before the as test is done
  Destination := nil;

  ClassMemory := nil;
  try
    // Read operands
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      ObjectOperand.CompileRead(Compiler, Instructions, Destination, TempVars);
      ClassOperand.CompileRead(Compiler, Instructions, ClassMemory,
        SrcTempVars);
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Make instruction
    Instruction := TSepiAsmAsClass.Create(Compiler);
    Instruction.SourcePos := Expression.SourcePos;

    Instruction.MemObject.Assign(Destination);
    Instruction.MemClass.Assign(ClassMemory);

    Instructions.Add(Instruction);
  finally
    ClassMemory.Free;
  end;
end;

{--------------------------------}
{ TSepiAssignmentOperation class }
{--------------------------------}

{*
  Crée une opération d'assignation
  @param AAutoConvert   Si True, l'opérande source est automatiquement converti
*}
constructor TSepiAssignmentOperation.Create(AAutoConvert: Boolean = True);
begin
  inherited Create;

  FAutoConvert := AAutoConvert;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAssignmentOperation.CompileExecute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList);
begin
  Destination.CompileWrite(Compiler, Instructions, Source);
end;

{*
  Complète l'opération
*}
procedure TSepiAssignmentOperation.Complete;
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

      Source := TSepiErroneousValue.Create(Destination.ValueType);
      Source.AttachToExpression(TSepiExpression.Create(
        Source as ISepiExpression));
    end;
  end;
end;

{*
  Construit une opération d'assignation
  @param Destination   Destination
  @param Source        Source
  @param AutoConvert   Si True, l'opérande source est automatiquement converti
  @return Expression exécutable représentant l'opération
*}
class function TSepiAssignmentOperation.MakeOperation(
  const Destination: ISepiWritableValue; const Source: ISepiReadableValue;
  AutoConvert: Boolean): ISepiExecutable;
var
  Assignment: TSepiAssignmentOperation;
begin
  Assignment := TSepiAssignmentOperation.Create(AutoConvert);
  Result := Assignment;
  Result.AttachToExpression(
    TSepiExpression.Create(Destination as ISepiExpression));
  Assignment.Destination := Destination;
  Assignment.Source := Source;
  Assignment.Complete;
end;

{------------------------------}
{ TSepiPureInheritedCall class }
{------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiPureInheritedCall.CompileExecute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList);
const
  ForceStaticCall = True;
var
  InheritedMethod: TSepiMethod;
  SelfParam, ParamValue: ISepiValue;
  WantingParams: ISepiWantingParams;
  Signature: TSepiSignature;
  I: Integer;
  CallValue: ISepiReadableValue;
  ResultValue: ISepiWritableValue;
  Executable: ISepiExecutable;
begin
  if not (Compiler.SepiMethod.Owner is TSepiClass) then
  begin
    MakeError(SInheritNeedClassOrObjectMethod);
    Exit;
  end;

  InheritedMethod := Compiler.SepiMethod.InheritedMethod;

  if InheritedMethod = nil then
  begin
    MakeError(SMethodNotFoundInBaseClass);
    Exit;
  end;

  SelfParam := TSepiLocalVarValue.MakeValue(Compiler,
    Compiler.Locals.SelfVar);

  WantingParams := TSepiMethodCall.Create(InheritedMethod,
    SelfParam as ISepiReadableValue, ForceStaticCall);
  WantingParams.AttachToExpression(TSepiExpression.Create(Compiler));

  Signature := Compiler.SepiMethod.Signature;
  for I := 0 to Signature.ParamCount-1 do
  begin
    ParamValue := TSepiLocalVarValue.MakeValue(Compiler,
      Compiler.Locals.GetVarByName(Signature.Params[I].Name));
    WantingParams.AddParam(ParamValue as ISepiExpression);
  end;

  WantingParams.CompleteParams;

  if Signature.ReturnType <> nil then
  begin
    CallValue := WantingParams as ISepiReadableValue;
    ResultValue := TSepiLocalVarValue.MakeValue(Compiler,
      Compiler.Locals.ResultVar) as ISepiWritableValue;
    Executable := TSepiAssignmentOperation.MakeOperation(
      ResultValue, CallValue) as ISepiExecutable;
  end else
  begin
    Executable := WantingParams as ISepiExecutable;
  end;

  Executable.CompileExecute(Compiler, Instructions);
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
    SetValueType((SepiRoot.SystemUnit as TSepiSystemUnit).Boolean)
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
  if ConstValuePtr <> nil then
    Exit;
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
procedure TSepiSetOperation.ErrorTypeMismatch;
begin
  MakeError(Format(STypeMismatch,
    [LeftOperand.ValueType.DisplayName, RightOperand.ValueType.DisplayName]));
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

  CollapseConsts;
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

  CollapseConsts;
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

  CollapseConsts;
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
constructor TSepiSetBuilder.Create(UnitCompiler: TSepiUnitCompiler);
begin
  inherited Create;

  FCompKind := tkUnknown;
  FCompType := nil;
  FSetType := nil;

  FConstantValues := TScIntegerSet.Create;

  FLowerBound := MaxInt;
  FHigherBound := -MaxInt-1;

  inherited SetValueType(UnitCompiler.GetEmptySetType);
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
  if (ConstValuePtr <> nil) or (Length(FSingles) > 0) or
    (Length(FRanges) > 0) then
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
    MakeError(Format(STypeMismatch,
      [CompType.DisplayName, ACompType.DisplayName]));
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
        AddConstInterval(Value, Value);
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
    MakeError(Format(SCompTypeTooNarrow, [ACompType.DisplayName]));
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
  begin
    SetCompType(ACompType as TSepiOrdType);
    Complete;
  end else
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
  begin
    SetSetType(AValueType as TSepiSetType);
    CollapseConsts;
  end else
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
    AllocateConstant;
    Exit;
  end else if FHigherBound - FLowerBound >= 256 then
  begin
    // Range is too wide
    MakeError(SSetRangeTooWide);
    if FCompKind = tkChar then
      SetCompType(UnitCompiler.SystemUnit.AnsiChar)
    else
      SetCompType(UnitCompiler.SystemUnit.Byte);
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
          SetCompType(UnitCompiler.SystemUnit.AnsiChar)
        else
          SetCompType(UnitCompiler.SystemUnit.Byte);
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

  // Collapse constants
  CollapseConsts;
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
      Result := TSepiErroneousValue.MakeReplacementValue(Expression,
        Expression.UnitCompiler.SystemUnit.Boolean)
    else
      Result := TSepiErroneousValue.MakeReplacementValue(Expression);
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
  { In this context, open arrays may be treated as static arrays with a zero
    lower bound. }
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
    if ArrayType is TSepiStaticArrayType then
      ConstIndex := - TSepiStaticArrayType(ArrayType).LowerBound
    else
      ConstIndex := 0;

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
        Result.AddOperation(OrdTypeToOperation[IntIndexType.OrdType],
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

  // Fetch types
  FArrayType := ArrayValue.ValueType as TSepiArrayType;
  FIndexType := ArrayType.IndexType;
  SetValueType(ArrayType.ElementType);

  // Convert index
  if IndexValue.ValueType <> IndexType then
    IndexValue := TSepiConvertOperation.ConvertValue(IndexType, IndexValue);

  // Set accesses
  IsReadable := Supports(ArrayValue, ISepiReadableValue);
  if ArrayType is TSepiDynArrayType then
  begin
    IsAddressable := IsReadable;
    IsWritable := IsReadable and Supports(ArrayValue, ISepiWritableValue);
  end else
  begin
    IsAddressable := Supports(ArrayValue, ISepiAddressableValue);
    IsWritable := IsAddressable and Supports(ArrayValue, ISepiWritableValue);
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

{----------------------------}
{ TSepiStringCharValue class }
{----------------------------}

{*
  Compile l'appel à UniqueString
  @param Compiler          Compilateur
  @param Instructions      Instructions
  @param StringMemoryRef   Référence mémoire de la chaîne
*}
procedure TSepiStringCharValue.CompileUniqueStringCall(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  StringMemoryRef: TSepiMemoryReference);
const {don't localize}
  StringKindToMethodName: array[TSepiStringKind] of string =
    ('@UniqueStringA', '@UniqueStringW', '@UniqueStringU');
var
  CallInstr: TSepiAsmStaticCall;
begin
  // Call UniqueString
  CallInstr := TSepiAsmStaticCall.Create(Compiler);
  CallInstr.SetMethod(SystemUnit.FindComponent(
    StringKindToMethodName[StringType.StringKind]) as TSepiMethod);
  CallInstr.Parameters.Parameters[0].MemoryRef.Clone(StringMemoryRef);
  Instructions.Add(CallInstr);
end;

{*
  [@inheritDoc]
*}
function TSepiStringCharValue.CompileAsMemoryRef(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList;
  TempVars: TSepiTempVarsLifeManager): TSepiMemoryReference;
const
  OrdTypeToOperation: array[TOrdType] of TSepiAddressOperation = (
    aoPlusConstTimesMemShortint, aoPlusConstTimesMemByte,
    aoPlusConstTimesMemSmallint, aoPlusConstTimesMemWord,
    aoPlusConstTimesMemLongint, aoPlusConstTimesMemLongWord
  );
var
  IndexType: TSepiIntegerType;
  SourceMemory, IndexMemory: TSepiMemoryReference;
  ConstIndex, ConstFactor: Integer;
begin
  IndexType := IndexValue.ValueType as TSepiIntegerType;
  ConstFactor := ValueType.Size;

  SourceMemory := nil;
  IndexMemory := nil;
  try
    { Accessing a single character of a string always requires to *read* the
      string value. }

    // Read the string value
    (StringValue as ISepiReadableValue).CompileRead(Compiler, Instructions,
      SourceMemory, TempVars);

    // Call UniqueString if we are writing (copy-on-write)
    if FWriting then
      CompileUniqueStringCall(Compiler, Instructions, SourceMemory);

    // Unfortunately, string indices begin at 1
    ConstIndex := -1;

    // If the index value is constant, use ConstIndex, otherwise read it
    if IndexValue.IsConstant then
      Inc(ConstIndex, IndexType.ValueAsInteger(IndexValue.ConstValuePtr^))
    else
      IndexValue.CompileRead(Compiler, Instructions, IndexMemory, TempVars);

    // Finally, construct the memory reference for the item value
    Result := TSepiMemoryReference.Clone(SourceMemory);
    try
      // Dereference
      Result.AddOperation(adSimple);

      // Add the constant and variable offsets

      if ConstIndex <> 0 then // may be 0 if index is a constant 1
        Result.AddOperation(aoPlusConstShortint, ConstFactor * ConstIndex);

      if IndexMemory <> nil then
      begin
        Result.AddOperation(OrdTypeToOperation[IndexType.OrdType],
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
  [@inheritDoc]
*}
procedure TSepiStringCharValue.CompileWrite(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; Source: ISepiReadableValue);
begin
  FWriting := True;
  try
    inherited;
  finally
    FWriting := False;
  end;
end;

{*
  Complète la valeur
  Doit avoir été attaché à une expression au préalable, pour bénéficier du
  contexte.
*}
procedure TSepiStringCharValue.Complete;
begin
  Assert(StringValue <> nil);
  Assert(IndexValue <> nil);

  // Fetch types
  FStringType := StringValue.ValueType as TSepiStringType;
  SetValueType(StringType.CharType);

  // Convert index
  if not (IndexValue.ValueType is TSepiIntegerType) then
  begin
    IndexValue := TSepiConvertOperation.ConvertValue(SystemUnit.Integer,
      IndexValue);
  end;

  // Set accesses
  IsReadable := Supports(StringValue, ISepiReadableValue);
  IsWritable := IsReadable and Supports(StringValue, ISepiWritableValue) and
    Supports(StringValue, ISepiAddressableValue);
  IsAddressable := IsReadable;
end;

{*
  Construit une valeur élément de tableau
  @param StringValue   Valeur tableau
  @param IndexValue   Valeur index
*}
class function TSepiStringCharValue.MakeStringCharValue(
  const StringValue: ISepiValue;
  const IndexValue: ISepiReadableValue): ISepiValue;
var
  StringCharValue: TSepiStringCharValue;
begin
  StringCharValue := TSepiStringCharValue.Create;
  Result := StringCharValue;
  Result.AttachToExpression(TSepiExpression.Create(
    StringValue as ISepiExpression));
  StringCharValue.StringValue := StringValue;
  StringCharValue.IndexValue := IndexValue;
  StringCharValue.Complete;
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

{------------------------------}
{ TSepiStringLengthValue class }
{------------------------------}

{*
  Vérification de type
*}
procedure TSepiStringLengthValue.CheckType;
begin
  if not (Operand.ValueType is TSepiStringType) then
  begin
    MakeError(SStringTypeRequired);
    Operand := TSepiErroneousValue.MakeReplacementValue(
      Operand as ISepiExpression,
      (SepiRoot.SystemUnit as TSepiSystemUnit).LongString);
  end;
end;

{*
  Pliage des constantes
*}
procedure TSepiStringLengthValue.CollapseConsts;
begin
  if not Operand.IsConstant then
    Exit;

  AllocateConstant;

  case (Operand.ValueType as TSepiStringType).StringKind of
    skAnsiString:
      PInteger(ConstValuePtr)^ := Length(AnsiString(Operand.ConstValuePtr^));
    skWideString:
      PInteger(ConstValuePtr)^ := Length(WideString(Operand.ConstValuePtr^));
    {$IFDEF UNICODE}
    skUnicodeString:
      PInteger(ConstValuePtr)^ := Length(UnicodeString(Operand.ConstValuePtr^));
    {$ENDIF}
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiStringLengthValue.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
const
  StringKindToOpCode: array[TSepiStringKind] of TSepiOpCode = (
    ocAnsiStrLength, ocWideStrLength, ocUnicodeStrLength
  );
var
  SourceMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  Instr: TSepiAsmValueToIntStdFunction;
begin
  SourceMemory := nil;
  try
    // Read operand
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      Operand.CompileRead(Compiler, Instructions, SourceMemory, SrcTempVars);
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Make instruction
    Instr := TSepiAsmValueToIntStdFunction.Create(Compiler,
      StringKindToOpCode[(Operand.ValueType as TSepiStringType).StringKind]);
    Instr.SourcePos := Expression.SourcePos;

    NeedDestination(Destination, ValueType, Compiler, TempVars, Instr.AfterRef);

    Instr.Destination.Assign(Destination);
    Instr.Value.Assign(SourceMemory);

    Instructions.Add(Instr);
  finally
    SourceMemory.Free;
  end;
end;

{*
  Complète l'opération
  L'opération doit avoir été attachée à une expression auparavant, pour pouvoir
  bénéficier du contexte de celle-ci.
*}
procedure TSepiStringLengthValue.Complete;
begin
  Assert(Expression <> nil);
  Assert(Operand <> nil);

  SetValueType((SepiRoot.SystemUnit as TSepiSystemUnit).Integer);

  CheckType;
  CollapseConsts;
end;

{*
  Construit une valeur longueur de chaîne de caractères
  @param Operand    Opérande
  @return Valeur représentant la longueur de la chaîne de caractères
*}
class function TSepiStringLengthValue.MakeStringLengthValue(
  const Operand: ISepiReadableValue): ISepiReadableValue;
var
  StrLenValue: TSepiStringLengthValue;
begin
  StrLenValue := TSepiStringLengthValue.Create;
  Result := StrLenValue;
  Result.AttachToExpression(TSepiExpression.Create(Operand as ISepiExpression));
  StrLenValue.Operand := Operand;
  StrLenValue.Complete;
end;

{----------------------------}
{ TSepiArrayBoundValue class }
{----------------------------}

{*
  Crée une valeur borne de tableau
  @param AKind   Type de borne
*}
constructor TSepiArrayBoundValue.Create(AKind: TSepiArrayBoundKind);
begin
  inherited Create;

  FKind := AKind;
end;

{*
  Vérification de type
*}
procedure TSepiArrayBoundValue.CheckTypes;
begin
  if Operand.ValueType is TSepiArrayType then
  begin
    FArrayType := TSepiArrayType(Operand.ValueType);

    if Kind = abkLength then
      SetValueType(UnitCompiler.SystemUnit.Integer)
    else
      SetValueType(ArrayType.IndexType);
  end else
  begin
    MakeError(SArrayTypeRequired);
    Operand := nil;
    SetValueType(UnitCompiler.SystemUnit.Integer);
  end;
end;

{*
  Pliage des constantes
*}
procedure TSepiArrayBoundValue.CollapseConsts;
var
  Bound: Integer;
begin
  if ArrayType is TSepiStaticArrayType then
  begin
    // Static arrays have constant bounds
    AllocateConstant;

    with TSepiStaticArrayType(ArrayType) do
    begin
      case Self.Kind of
        abkLow:    Bound := LowerBound;
        abkHigh:   Bound := HigherBound;
        abkLength: Bound := ArrayLength;
      else
        Assert(False);
        Bound := 0;
      end;
    end;

    ValueType.CopyData(Bound, ConstValuePtr^);
  end else if Kind = abkLow then
  begin
    // Low is always zero for other types of arrays
    AllocateConstant;
  end;

  // Other bounds are not constant
end;

{*
  [@inheritDoc]
*}
procedure TSepiArrayBoundValue.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
begin
  Assert(Kind in [abkHigh, abkLength]);

  if ArrayType is TSepiDynArrayType then
    CompileComputeDynArray(Compiler, Instructions, Destination, TempVars)
  else if ArrayType is TSepiOpenArrayType then
    CompileComputeOpenArray(Compiler, Instructions, Destination, TempVars)
  else // static arrays have always constant bounds
    Assert(False);
end;

{*
  [@inheritDoc]
*}
procedure TSepiArrayBoundValue.CompileComputeDynArray(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
var
  SourceMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  Instr: TSepiAsmValueToIntStdFunction;
begin
  SourceMemory := nil;
  try
    // Read operand
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      Operand.CompileRead(Compiler, Instructions, SourceMemory, SrcTempVars);
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Make instruction
    Instr := TSepiAsmValueToIntStdFunction.Create(Compiler,
      IIF(Kind = abkLength, ocDynArrayLength, ocDynArrayHigh));
    Instr.SourcePos := Expression.SourcePos;

    NeedDestination(Destination, ValueType, Compiler, TempVars, Instr.AfterRef);

    Instr.Destination.Assign(Destination);
    Instr.Value.Assign(SourceMemory);

    Instructions.Add(Instr);
  finally
    SourceMemory.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiArrayBoundValue.CompileComputeOpenArray(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
var
  HighValueMemory: TSepiMemoryReference;
  IncInstr: TSepiAsmOperation;
begin
  HighValueMemory := TSepiMemoryReference.Create(Compiler, aoAcceptAllConsts,
    SizeOf(Integer));
  HighValueMemory.SetSpace(TSepiOpenArrayType(ArrayType).HighVarName);
  HighValueMemory.Seal;

  if Kind = abkHigh then
  begin
    Destination := HighValueMemory;
  end else if Kind = abkLength then
  begin
    NeedDestination(Destination, ValueType, Compiler, TempVars,
      Instructions.GetCurrentEndRef);

    IncInstr := TSepiAsmOperation.Create(Compiler, ocOtherInc, btLongint);
    IncInstr.SourcePos := Expression.SourcePos;

    IncInstr.Destination.Assign(Destination);
    IncInstr.Source.Assign(HighValueMemory);

    Instructions.Add(IncInstr);
  end else
  begin
    Assert(False);
  end;
end;

{*
  Complète l'opération
  L'opération doit avoir été attachée à une expression auparavant, pour pouvoir
  bénéficier du contexte de celle-ci.
*}
procedure TSepiArrayBoundValue.Complete;
begin
  Assert(Expression <> nil);
  Assert(Operand <> nil);

  CheckTypes;

  if Operand = nil then
    AllocateConstant
  else
    CollapseConsts;
end;

{*
  Construit une valeur longueur de chaîne de caractères
  @param Operand    Opérande
  @return Valeur représentant la longueur de la chaîne de caractères
*}
class function TSepiArrayBoundValue.MakeArrayBoundValue(
  Kind: TSepiArrayBoundKind;
  const Operand: ISepiReadableValue): ISepiReadableValue;
var
  ArrayBoundValue: TSepiArrayBoundValue;
begin
  ArrayBoundValue := TSepiArrayBoundValue.Create(Kind);
  Result := ArrayBoundValue;
  Result.AttachToExpression(TSepiExpression.Create(Operand as ISepiExpression));
  ArrayBoundValue.Operand := Operand;
  ArrayBoundValue.Complete;
end;

{-----------------------------------}
{ TSepiStrSetLengthExpression class }
{-----------------------------------}

{*
  Vérification de type
*}
procedure TSepiStrSetLengthExpression.CheckTypes;
var
  StrExpression: ISepiExpression;
begin
  StrExpression := StrValue as ISepiExpression;

  // String operand

  if not (StrValue.ValueType is TSepiStringType) then
  begin
    StrExpression.MakeError(SStringTypeRequired);
    StrValue := TSepiErroneousValue.MakeReplacementValue(StrExpression,
      UnitCompiler.SystemUnit.LongString);
  end else if not (Supports(StrValue, ISepiReadableValue) and
    Supports(StrValue, ISepiWritableValue) and
    Supports(StrValue, ISepiAddressableValue)) then
  begin
    StrExpression.MakeError(SVarValueRequired);
    StrValue := TSepiErroneousValue.MakeReplacementValue(StrExpression,
      UnitCompiler.SystemUnit.LongString);
  end;

  // New length operand

  if not NewLengthValue.ValueType.Equals(UnitCompiler.SystemUnit.Integer) then
  begin
    NewLengthValue := TSepiConvertOperation.ConvertValue(
      UnitCompiler.SystemUnit.Integer, NewLengthValue);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiStrSetLengthExpression.CompileExecute(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList);
const
  StringKindToOpCode: array[TSepiStringKind] of TSepiOpCode = (
    ocAnsiStrSetLength, ocWideStrSetLength, ocUnicodeStrSetLength
  );
var
  ReadableStrValue: ISepiReadableValue;
  StrMemory, NewLengthMemory: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager;
  OpCode: TSepiOpCode;
  AsmInstr: TSepiAsmStrSetLength;
begin
  ReadableStrValue := StrValue as ISepiReadableValue;

  StrMemory := nil;
  NewLengthMemory := nil;
  TempVars := TSepiTempVarsLifeManager.Create;
  try
    // Read operands

    ReadableStrValue.CompileRead(Compiler, Instructions, StrMemory, TempVars);
    NewLengthValue.CompileRead(Compiler, Instructions, NewLengthMemory,
      TempVars);

    TempVars.EndAllLifes(Instructions.GetCurrentEndRef);

    // Make asm instruction

    OpCode := StringKindToOpCode[
      TSepiStringType(StrValue.ValueType).StringKind];

    AsmInstr := TSepiAsmStrSetLength.Create(Compiler, OpCode);
    AsmInstr.SourcePos := Expression.SourcePos;
    AsmInstr.StrValue.Assign(StrMemory);
    AsmInstr.NewLength.Assign(NewLengthMemory);

    Instructions.Add(AsmInstr);
  finally
    StrMemory.Free;
    NewLengthMemory.Free;
    TempVars.Free;
  end;
end;

{*
  Complète l'expression
*}
procedure TSepiStrSetLengthExpression.Complete;
begin
  Assert(StrValue <> nil);
  Assert(NewLengthValue <> nil);

  CheckTypes;
end;

{*
  Construit une expression de changement de la longueur d'une chaîne
  @param StrValue         Valeur chaîne
  @param NewLengthValue   Nouvelle longueur de la chaîne
*}
class function TSepiStrSetLengthExpression.MakeStrSetLengthExpression(
  const StrValue: ISepiValue;
  const NewLengthValue: ISepiReadableValue): ISepiExecutable;
var
  StrSetLength: TSepiStrSetLengthExpression;
begin
  StrSetLength := TSepiStrSetLengthExpression.Create;
  Result := StrSetLength;
  Result.AttachToExpression(TSepiExpression.Create(
    StrValue as ISepiExpression));
  StrSetLength.StrValue := StrValue;
  StrSetLength.NewLengthValue := NewLengthValue;
  StrSetLength.Complete;
end;

{----------------------------------------}
{ TSepiDynArraySetLengthExpression class }
{----------------------------------------}

{*
  Crée une expression de changement de la longueur d'un tableau dynamique
  @param ADimCount   Nombre de dimensions (> 0)
*}
constructor TSepiDynArraySetLengthExpression.Create(ADimCount: Integer = 1);
begin
  Assert(ADimCount > 0);

  inherited Create;

  FDimCount := ADimCount;
  FDimensions := TInterfaceList.Create;
  FDimensions.Count := DimCount;
end;

{*
  Vérification de type
*}
procedure TSepiDynArraySetLengthExpression.CheckTypes;
var
  DynArrayExpression: ISepiExpression;
  I, MaxDimCount: Integer;
  SubType: TSepiType;
begin
  DynArrayExpression := DynArrayValue as ISepiExpression;

  // String operand

  if not (DynArrayValue.ValueType is TSepiDynArrayType) then
  begin
    DynArrayExpression.MakeError(SDynArrayTypeRequired);
    DynArrayValue := nil;
  end else if not (Supports(DynArrayValue, ISepiReadableValue) and
    Supports(DynArrayValue, ISepiWritableValue) and
    Supports(DynArrayValue, ISepiAddressableValue)) then
  begin
    DynArrayExpression.MakeError(SVarValueRequired);
    DynArrayValue := nil;
  end;

  // Dimensions

  for I := 0 to DimCount-1 do
  begin
    if not Dimensions[I].ValueType.Equals(UnitCompiler.SystemUnit.Integer) then
    begin
      Dimensions[I] := TSepiConvertOperation.ConvertValue(
        UnitCompiler.SystemUnit.Integer, Dimensions[I]);
    end;
  end;

  if DynArrayValue <> nil then
  begin
    MaxDimCount := 0;
    SubType := DynArrayValue.ValueType;

    while SubType is TSepiDynArrayType do
    begin
      Inc(MaxDimCount);
      SubType := TSepiDynArrayType(SubType).ElementType;
    end;

    if DimCount > MaxDimCount then
    begin
      (Dimensions[MaxDimCount] as ISepiExpression).MakeError(
        STooManyActualParameters);
      FDimCount := MaxDimCount;
      FDimensions.Count := MaxDimCount;
    end;
  end;
end;

{*
  Tableau zero-based des dimensions
  @param Index   Index compris entre 0 inclus et DimCount exclu
  @return Dimension à l'index spécifié
*}
function TSepiDynArraySetLengthExpression.GetDimensions(
  Index: Integer): ISepiReadableValue;
begin
  Result := FDimensions[Index] as ISepiReadableValue;
end;

{*
  Modifie le tableau zero-based des dimensions
  @param Index   Index compris entre 0 inclus et DimCount exclu
  @param Value   Nouvelle valeur pour la dimension à l'index spécifié
*}
procedure TSepiDynArraySetLengthExpression.SetDimensions(Index: Integer;
  const Value: ISepiReadableValue);
begin
  FDimensions[Index] := Value;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDynArraySetLengthExpression.CompileExecute(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList);
var
  ReadableDynArrayValue: ISepiReadableValue;
  DynArrayMemory: TSepiMemoryReference;
  DimensionsMemory: array of TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager;
  AsmInstr: TSepiAsmDynArraySetLength;
  I: Integer;
begin
  ReadableDynArrayValue := DynArrayValue as ISepiReadableValue;

  DynArrayMemory := nil;
  SetLength(DimensionsMemory, DimCount);
  FillChar(DimensionsMemory[0], DimCount*SizeOf(TSepiMemoryReference), 0);
  TempVars := TSepiTempVarsLifeManager.Create;
  try
    // Read operands

    ReadableDynArrayValue.CompileRead(Compiler, Instructions, DynArrayMemory,
      TempVars);
    for I := 0 to DimCount-1 do
      Dimensions[I].CompileRead(Compiler, Instructions, DimensionsMemory[I],
        TempVars);

    TempVars.EndAllLifes(Instructions.GetCurrentEndRef);

    // Make asm instruction

    AsmInstr := TSepiAsmDynArraySetLength.Create(Compiler,
      DynArrayValue.ValueType as TSepiDynArrayType, DimCount);
    AsmInstr.SourcePos := Expression.SourcePos;
    AsmInstr.DynArrayValue.Assign(DynArrayMemory);
    for I := 0 to DimCount-1 do
      AsmInstr.Dimensions[I].Assign(DimensionsMemory[I]);

    Instructions.Add(AsmInstr);
  finally
    DynArrayMemory.Free;
    for I := 0 to DimCount-1 do
      DimensionsMemory[I].Free;
    TempVars.Free;
  end;
end;

{*
  Complète l'expression
*}
procedure TSepiDynArraySetLengthExpression.Complete;
begin
  Assert(DynArrayValue <> nil);
  Assert(FirstDimension <> nil);

  CheckTypes;
end;

{*
  Construit une expression de changement de la longueur d'un tableau dynamique
  @param DynArrayValue   Valeur tableau dynamique
  @param Dimensions      Nouvelles longueurs des dimensions
*}
class function TSepiDynArraySetLengthExpression.MakeDynArraySetLengthExpression(
  const DynArrayValue: ISepiValue;
  const Dimensions: array of ISepiReadableValue): ISepiExecutable;
var
  DynArraySetLength: TSepiDynArraySetLengthExpression;
  I: Integer;
begin
  DynArraySetLength := TSepiDynArraySetLengthExpression.Create(
    Length(Dimensions));
  Result := DynArraySetLength;
  Result.AttachToExpression(TSepiExpression.Create(
    DynArrayValue as ISepiExpression));
  DynArraySetLength.DynArrayValue := DynArrayValue;
  for I := Low(Dimensions) to High(Dimensions) do
    DynArraySetLength.Dimensions[I] := Dimensions[I];
  DynArraySetLength.Complete;
end;

{------------------------------}
{ TSepiStrCopyExpression class }
{------------------------------}

{*
  Vérification de type
*}
procedure TSepiStrCopyExpression.CheckTypes;
begin
  // Source string
  if not (SourceStr.ValueType is TSepiStringType) then
  begin
    (SourceStr as ISepiExpression).MakeError(SStringTypeRequired);
    SourceStr := TSepiErroneousValue.MakeReplacementValue(
      SourceStr as ISepiExpression, UnitCompiler.SystemUnit.LongString);
  end;

  // Index
  if not IndexValue.ValueType.Equals(UnitCompiler.SystemUnit.Integer) then
  begin
    IndexValue := TSepiConvertOperation.ConvertValue(
      UnitCompiler.SystemUnit.Integer, IndexValue);
  end;

  // Count
  if not CountValue.ValueType.Equals(UnitCompiler.SystemUnit.Integer) then
  begin
    CountValue := TSepiConvertOperation.ConvertValue(
      UnitCompiler.SystemUnit.Integer, CountValue);
  end;
end;

{*
  Pliage des constantes
*}
procedure TSepiStrCopyExpression.CollapseConsts;
var
  DestPtr, SrcPtr: Pointer;
  Index, Count: Integer;
begin
  if not (SourceStr.IsConstant and IndexValue.IsConstant and
    CountValue.IsConstant) then
    Exit;

  AllocateConstant;

  DestPtr := ConstValuePtr;
  SrcPtr := SourceStr.ConstValuePtr;
  Index := Integer(IndexValue.ConstValuePtr^);
  Count := Integer(CountValue.ConstValuePtr^);

  case (ValueType as TSepiStringType).StringKind of
    skAnsiString:
      AnsiString(DestPtr^) := Copy(AnsiString(SrcPtr^), Index, Count);
    skWideString:
      WideString(DestPtr^) := Copy(WideString(SrcPtr^), Index, Count);

    {$IFDEF UNICODE}
    skUnicodeString:
      UnicodeString(DestPtr^) := Copy(UnicodeString(SrcPtr^), Index, Count);
    {$ENDIF}
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiStrCopyExpression.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
const
  StringKindToOpCode: array[TSepiStringKind] of TSepiOpCode = (
    ocAnsiStrCopy, ocWideStrCopy, ocUnicodeStrCopy
  );
var
  SourceStrMemory, IndexMemory, CountMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  Instruction: TSepiAsmStrCopy;
begin
  SourceStrMemory := nil;
  IndexMemory := nil;
  CountMemory := nil;
  try
    // Read operands
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      SourceStr.CompileRead(Compiler, Instructions, SourceStrMemory,
        SrcTempVars);
      IndexValue.CompileRead(Compiler, Instructions, IndexMemory, SrcTempVars);
      CountValue.CompileRead(Compiler, Instructions, CountMemory, SrcTempVars);
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Make instruction
    Instruction := TSepiAsmStrCopy.Create(Compiler,
      StringKindToOpCode[TSepiStringType(ValueType).StringKind]);
    Instruction.SourcePos := Expression.SourcePos;

    NeedDestination(Destination, ValueType, Compiler, TempVars,
      Instruction.AfterRef);

    Instruction.Destination.Assign(Destination);
    Instruction.Source.Assign(SourceStrMemory);
    Instruction.IndexValue.Assign(IndexMemory);
    Instruction.CountValue.Assign(CountMemory);

    Instructions.Add(Instruction);
  finally
    SourceStrMemory.Free;
    IndexMemory.Free;
    CountMemory.Free;
  end;
end;

{*
  Complète l'expression
*}
procedure TSepiStrCopyExpression.Complete;
begin
  Assert(SourceStr <> nil);
  Assert(IndexValue <> nil);
  Assert(CountValue <> nil);

  CheckTypes;
  SetValueType(SourceStr.ValueType);

  CollapseConsts;
end;

{*
  Construit une expression copie d'une partie de chaîne de caractères
  @param SourceStr   Chaîne source
  @param IndexValue   Valeur index de début de la copie
  @param CountValue   Valeur nombre de caractères à copier
*}
class function TSepiStrCopyExpression.MakeStrCopy(
  const SourceStr: ISepiReadableValue;
  const IndexValue, CountValue: ISepiReadableValue): ISepiReadableValue;
var
  StrCopy: TSepiStrCopyExpression;
begin
  StrCopy := TSepiStrCopyExpression.Create;
  Result := StrCopy;
  Result.AttachToExpression(TSepiExpression.Create(
    SourceStr as ISepiExpression));
  StrCopy.SourceStr := SourceStr;
  StrCopy.IndexValue := IndexValue;
  StrCopy.CountValue := CountValue;
  StrCopy.Complete;
end;

{-----------------------------------}
{ TSepiDynArrayCopyExpression class }
{-----------------------------------}

{*
  Crée une expression copie de tableau dynamique
  @param AIsRange   Indique si c'est la version range
*}
constructor TSepiDynArrayCopyExpression.Create(AIsRange: Boolean);
begin
  inherited Create;

  FIsRange := AIsRange;
end;

{*
  Vérification de type
*}
procedure TSepiDynArrayCopyExpression.CheckTypes;
begin
  // Source array
  if not (SourceArray.ValueType is TSepiDynArrayType) then
  begin
    (SourceArray as ISepiExpression).MakeError(SDynArrayTypeRequired);
    SourceArray := TSepiErroneousValue.MakeReplacementValue(
      SourceArray as ISepiExpression,
      TSepiDynArrayType.Create(UnitCompiler.SepiUnit, '',
      UnitCompiler.SystemUnit.Integer));
  end;

  if IsRange then
  begin
    // Index
    if not IndexValue.ValueType.Equals(UnitCompiler.SystemUnit.Integer) then
    begin
      IndexValue := TSepiConvertOperation.ConvertValue(
        UnitCompiler.SystemUnit.Integer, IndexValue);
    end;

    // Count
    if not CountValue.ValueType.Equals(UnitCompiler.SystemUnit.Integer) then
    begin
      CountValue := TSepiConvertOperation.ConvertValue(
        UnitCompiler.SystemUnit.Integer, CountValue);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDynArrayCopyExpression.CompileCompute(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
var
  SourceArrayMemory, IndexMemory, CountMemory: TSepiMemoryReference;
  SrcTempVars: TSepiTempVarsLifeManager;
  Instruction: TSepiAsmDynArrayCopy;
begin
  SourceArrayMemory := nil;
  IndexMemory := nil;
  CountMemory := nil;
  try
    // Read operands
    SrcTempVars := TSepiTempVarsLifeManager.Create;
    try
      SourceArray.CompileRead(Compiler, Instructions, SourceArrayMemory,
        SrcTempVars);

      if IsRange then
      begin
        IndexValue.CompileRead(Compiler, Instructions, IndexMemory,
          SrcTempVars);
        CountValue.CompileRead(Compiler, Instructions, CountMemory,
          SrcTempVars);
      end;
    finally
      SrcTempVars.EndAllLifes(Instructions.GetCurrentEndRef);
      SrcTempVars.Free;
    end;

    // Make instruction
    Instruction := TSepiAsmDynArrayCopy.Create(Compiler,
      ValueType as TSepiDynArrayType, IsRange);
    Instruction.SourcePos := Expression.SourcePos;

    NeedDestination(Destination, ValueType, Compiler, TempVars,
      Instruction.AfterRef);

    Instruction.Destination.Assign(Destination);
    Instruction.Source.Assign(SourceArrayMemory);

    if IsRange then
    begin
      Instruction.IndexValue.Assign(IndexMemory);
      Instruction.CountValue.Assign(CountMemory);
    end;

    Instructions.Add(Instruction);
  finally
    SourceArrayMemory.Free;
    IndexMemory.Free;
    CountMemory.Free;
  end;
end;

{*
  Complète l'expression
*}
procedure TSepiDynArrayCopyExpression.Complete;
begin
  Assert(SourceArray <> nil);
  Assert(IsRange = (IndexValue <> nil));
  Assert(IsRange = (CountValue <> nil));

  CheckTypes;
  SetValueType(SourceArray.ValueType);
end;

{*
  Construit une expression copie d'une partie de chaîne de caractères
  @param SourceArray   Tableau source
  @param IndexValue    Valeur index de début de la copie
  @param CountValue    Valeur nombre de caractères à copier
*}
class function TSepiDynArrayCopyExpression.MakeDynArrayCopy(
  const SourceArray: ISepiReadableValue): ISepiReadableValue;
var
  DynArrayCopy: TSepiDynArrayCopyExpression;
begin
  DynArrayCopy := TSepiDynArrayCopyExpression.Create(False);
  Result := DynArrayCopy;
  Result.AttachToExpression(TSepiExpression.Create(
    SourceArray as ISepiExpression));
  DynArrayCopy.SourceArray := SourceArray;
  DynArrayCopy.Complete;
end;

{*
  Construit une expression copie d'une partie de chaîne de caractères
  @param SourceArray   Tableau source
  @param IndexValue    Valeur index de début de la copie
  @param CountValue    Valeur nombre de caractères à copier
*}
class function TSepiDynArrayCopyExpression.MakeDynArrayCopyRange(
  const SourceArray: ISepiReadableValue;
  const IndexValue, CountValue: ISepiReadableValue): ISepiReadableValue;
var
  DynArrayCopy: TSepiDynArrayCopyExpression;
begin
  DynArrayCopy := TSepiDynArrayCopyExpression.Create(True);
  Result := DynArrayCopy;
  Result.AttachToExpression(TSepiExpression.Create(
    SourceArray as ISepiExpression));
  DynArrayCopy.SourceArray := SourceArray;
  DynArrayCopy.IndexValue := IndexValue;
  DynArrayCopy.CountValue := CountValue;
  DynArrayCopy.Complete;
end;

{----------------------------------------}
{ TSepiSetIncludeExcludeExpression class }
{----------------------------------------}

{*
  Crée une instance de TSepiSetIncludeExcludeExpression
  @param AIsInclude   True pour Include, False pour Exclude
*}
constructor TSepiSetIncludeExcludeExpression.Create(AIsInclude: Boolean);
begin
  inherited Create;

  FIsInclude := AIsInclude;
end;

{*
  Vérification de type
*}
procedure TSepiSetIncludeExcludeExpression.CheckTypes;
var
  SetExpression: ISepiExpression;
  SetType: TSepiSetType;
  ItemType: TSepiOrdType;
begin
  SetExpression := SetValue as ISepiExpression;

  // Set operand

  if not (SetValue.ValueType is TSepiSetType) then
  begin
    // Not a set
    SetExpression.MakeError(SSetTypeRequired);
    SetValue := TSepiErroneousValue.MakeReplacementValue(SetExpression,
      UnitCompiler.MakeSetType(SystemUnit.Byte));
  end else if not (Supports(SetValue, ISepiReadableValue) and
    Supports(SetValue, ISepiWritableValue) and
    Supports(SetValue, ISepiAddressableValue)) then
  begin
    // Not a variable
    SetExpression.MakeError(SVarValueRequired);
    SetValue := TSepiErroneousValue.MakeReplacementValue(SetExpression,
      UnitCompiler.MakeSetType(SystemUnit.Byte));
  end;

  SetType := SetValue.ValueType as TSepiSetType;
  ItemType := SetType.CompType;

  // Item operand

  if not ItemValue.ValueType.Equals(ItemType) then
    ItemValue := TSepiConvertOperation.ConvertValue(ItemType, ItemValue);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetIncludeExcludeExpression.CompileExecute(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList);
var
  ReadableSetValue: ISepiReadableValue;
  SetMemory, ItemMemory: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager;
  AsmInstr: TSepiAsmSetIncludeExclude;
begin
  ReadableSetValue := SetValue as ISepiReadableValue;

  SetMemory := nil;
  ItemMemory := nil;
  TempVars := TSepiTempVarsLifeManager.Create;
  try
    // Read operands

    ReadableSetValue.CompileRead(Compiler, Instructions, SetMemory, TempVars);
    ItemValue.CompileRead(Compiler, Instructions, ItemMemory, TempVars);

    TempVars.EndAllLifes(Instructions.GetCurrentEndRef);

    // Make asm instruction

    AsmInstr := TSepiAsmSetIncludeExclude.Create(Compiler, IsInclude);
    AsmInstr.SourcePos := Expression.SourcePos;
    AsmInstr.Destination.Assign(SetMemory);
    AsmInstr.Element.Assign(ItemMemory);

    Instructions.Add(AsmInstr);
  finally
    SetMemory.Free;
    ItemMemory.Free;
    TempVars.Free;
  end;
end;

{*
  Complète l'expression
*}
procedure TSepiSetIncludeExcludeExpression.Complete;
begin
  Assert(SetValue <> nil);
  Assert(ItemValue <> nil);

  CheckTypes;
end;

{*
  Construit une expression d'inclusion ou exclusion d'un élément d'un ensemble
  @param IsInclude   True pour inclure l'élément, False pour l'exclure
  @param SetValue    Valeur ensemble
  @param ItemValue   Élément à inclure ou exclure
*}
class function TSepiSetIncludeExcludeExpression.MakeSetIncludeExcludeExpression(
  IsInclude: Boolean; const SetValue: ISepiValue;
  const ItemValue: ISepiReadableValue): ISepiExecutable;
var
  SetIncludeExclude: TSepiSetIncludeExcludeExpression;
begin
  SetIncludeExclude := TSepiSetIncludeExcludeExpression.Create(IsInclude);
  Result := SetIncludeExclude;
  Result.AttachToExpression(TSepiExpression.Create(
    SetValue as ISepiExpression));
  SetIncludeExclude.SetValue := SetValue;
  SetIncludeExclude.ItemValue := ItemValue;
  SetIncludeExclude.Complete;
end;

{*
  Construit une expression d'inclusion d'un élément d'un ensemble
  @param SetValue    Valeur ensemble
  @param ItemValue   Élément à inclure
*}
class function TSepiSetIncludeExcludeExpression.MakeSetIncludeExpression(
  const SetValue: ISepiValue;
  const ItemValue: ISepiReadableValue): ISepiExecutable;
begin
  Result := MakeSetIncludeExcludeExpression(True, SetValue, ItemValue);
end;

{*
  Construit une expression d'exclusion d'un élément d'un ensemble
  @param SetValue    Valeur ensemble
  @param ItemValue   Élément à exclure
*}
class function TSepiSetIncludeExcludeExpression.MakeSetExcludeExpression(
  const SetValue: ISepiValue;
  const ItemValue: ISepiReadableValue): ISepiExecutable;
begin
  Result := MakeSetIncludeExcludeExpression(False, SetValue, ItemValue);
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
function TSepiCustomWithParams.GetParams(Index: Integer): ISepiExpression;
begin
  Result := FParams[Index] as ISepiExpression;
end;

{*
  Modifie un paramètre
  @param Index   Index du paramètre à modifié
  @param Value   Nouvelle valeur du paramètre
*}
procedure TSepiCustomWithParams.SetParams(Index: Integer;
  const Value: ISepiExpression);
begin
  if ParamsCompleted then
    raise ESepiCompilerError.Create(SParamsAlreadyCompleted);

  FParams[Index] := Value;
end;

{*
  Efface tous les paramètres
*}
procedure TSepiCustomWithParams.ClearParams;
begin
  if ParamsCompleted then
    raise ESepiCompilerError.Create(SParamsAlreadyCompleted);

  FParams.Clear;
end;

{*
  Ajoute un paramètre
  @param Value   Paramètre à ajouter
*}
procedure TSepiCustomWithParams.AddParam(const Value: ISepiExpression);
begin
  if ParamsCompleted then
    raise ESepiCompilerError.Create(SParamsAlreadyCompleted);

  FParams.Add(Value);
end;

{*
  Supprime un paramètre
  @param Index   Index du paramètre à supprimer
*}
procedure TSepiCustomWithParams.DeleteParam(Index: Integer);
begin
  if ParamsCompleted then
    raise ESepiCompilerError.Create(SParamsAlreadyCompleted);

  FParams.Delete(Index);
end;

{*
  Complète les paramètres
  Une fois complétés, il n'est plus possible de modifier ou d'ajouter de
  paramètre.
*}
procedure TSepiCustomWithParams.CompleteParams;
begin
  if FParamsCompleted then
    Exit;

  FParamsCompleted := True;
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
  ParamExpression: ISepiExpression;
  ParamValue: ISepiValue;

  procedure Error(const Msg: string);
  begin
    Result := False;

    if MakeErrors then
      ParamExpression.MakeError(Msg)
    else
      Abort;
  end;

begin
  // Check param count
  if ParamCount < Signature.ParamCount then
  begin
    Result := True;
    for I := ParamCount to Signature.ParamCount-1 do
    begin
      if not Signature.Params[I].HasDefaultValue then
      begin
        Result := False;
        Break;
      end;
    end;

    MinParamCount := ParamCount;
    NotEnoughParams := not Result;
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
      ParamExpression := Params[I];

      if SignatureParam.OpenArray then
      begin
        if not LanguageRules.ConvertionToOpenArrayExists(
          SignatureParam.ElementType, ParamExpression) then
        begin
          // Error: open array required
          Error(Format(SOpenArrayRequired,
            [SignatureParam.ParamType.DisplayName]));
        end;
      end else if not Supports(ParamExpression, ISepiValue, ParamValue) then
      begin
        // Value required
        Error(SValueRequired);
      end else if SignatureParam.IsUntyped then
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

        if SignatureParam.ByRef then
        begin
          // If by reference, must also be writable and addressable ...
          if (not Supports(ParamValue, ISepiWritableValue)) or
            (not Supports(ParamValue, ISepiAddressableValue)) then
          begin
            Error(SVarValueRequired);
          end else
          // ... and their types must be strictly equal
          if not ParamValue.ValueType.Equals(SignatureParam.ParamType) then
          begin
            Error(SVarParamTypeMustBeStrictlyEqual);
          end;
        end else
        begin
          // Otherwise, types must be compatible
          if not TSepiConvertOperation.ConversionExists(
            SignatureParam.ParamType, ParamValue as ISepiReadableValue) then
          begin
            Error(Format(STypeMismatch,
              [SignatureParam.ParamType.DisplayName,
              ParamValue.ValueType.DisplayName]));
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
        Assert(not SignatureParam.OpenArray);

        if ParamValue = nil then
        begin
          Assert(SignatureParam.HasDefaultValue);
          ParamValue := TSepiTrueConstValue.MakeValue(Compiler,
            SignatureParam.ParamType, SignatureParam.DefaultValuePtr^);
        end;

        if Supports(ParamValue, ISepiReadableValue, ReadableParam) then
        begin
          if (not ReadableParam.ValueType.Equals(SignatureParam.ParamType)) and
            (not SignatureParam.IsUntyped) then
          begin
            ReadableParam := TSepiConvertOperation.ConvertValue(
              SignatureParam.ParamType, ReadableParam);
          end;

          ReadableParam.CompileRead(Compiler, Instructions, ParamMemory,
            TempVars);

          InstrParamMemory.Assign(ParamMemory);
        end else
        begin
          (ParamValue as ISepiAddressableValue).CompileLoadAddress(
            Compiler, Instructions, ParamMemory, TempVars);

          InstrParamMemory.Assign(ParamMemory, False);
          InstrParamMemory.AddOperation(adSimple);
          InstrParamMemory.Seal;
        end;
      end;

      // Paramètre Self
      hpSelf:
      begin
        Assert(ParamValue <> nil);

        (ParamValue as ISepiReadableValue).CompileRead(Compiler, Instructions,
          ParamMemory, TempVars);
        InstrParamMemory.Assign(ParamMemory);
      end;

      // Résultat - rien à faire ici
      hpResult:
      begin
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
    else
      Assert(False);
    end;
  finally
    ParamMemory.Free;
  end;
end;

{*
  Compile un paramètre tableau ouvert
  @param Compiler               Compilateur de méthode
  @param Instructions           Liste d'instructions
  @param SignatureParam         Paramètre de la signature
  @param OpenArray              Tableau ouvert
  @param InstrOpenArrayMemory   Référence mémoire au paramètre tableau ouvert
  @param InstrHighValueMemory   Référence mémoire au paramètre High
  @param TempVars               Gestionnaire de vie des variables temporaires
*}
procedure TSepiCustomCallable.CompileOpenArrayParam(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  SignatureParam: TSepiParam; const OpenArray: ISepiOpenArrayValue;
  InstrOpenArrayMemory, InstrHighValueMemory: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  OpenArrayMemory, HighValueMemory: TSepiMemoryReference;
begin
  Assert(SignatureParam.OpenArray);

  OpenArrayMemory := nil;
  HighValueMemory := nil;
  try
    OpenArray.CompileReadOpenArray(Compiler, Instructions, OpenArrayMemory,
      HighValueMemory, TempVars);

    InstrOpenArrayMemory.Assign(OpenArrayMemory);
    InstrHighValueMemory.Assign(HighValueMemory);
  finally
    OpenArrayMemory.Free;
    HighValueMemory.Free;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiCustomCallable.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if SameGUID(IID, ISepiWantingParams) and ParamsCompleted then
    Result := E_NOINTERFACE
  else if SameGUID(IID, ISepiExecutable) and (not ParamsCompleted) then
    Result := E_NOINTERFACE
  else if (SameGUID(IID, ISepiValue) or SameGUID(IID, ISepiReadableValue)) and
    ((not ParamsCompleted) or (ReturnType = nil)) then
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

  if not ParamsCompleted then
    Expression.Attach(ISepiWantingParams, AsExpressionPart)
  else
  begin
    Expression.Attach(ISepiExecutable, AsExpressionPart);

    if ReturnType <> nil then
    begin
      Expression.Attach(ISepiValue, AsExpressionPart);
      Expression.Attach(ISepiReadableValue, AsExpressionPart);
    end;
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
  OpenArray: ISepiOpenArrayValue;
begin
  // If parameters are not valid, don't try anything
  if not MatchesSignature(Signature) then
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

      if SignatureParam.HiddenKind = hpOpenArrayHighValue then
        Continue;

      if SignatureParam.HiddenKind = hpNormal then
        Inc(RealParamIndex);

      InstrParamMemory := Parameters.Parameters[I].MemoryRef;

      if (SignatureParam.HiddenKind = hpSelf) and (SelfValue = nil) then
      begin
        Assert(SelfMem.IsSealed);
        InstrParamMemory.Assign(SelfMem);
      end else if SignatureParam.OpenArray then
      begin
        OpenArray := LanguageRules.ConvertToOpenArray(
          SignatureParam.ElementType,
          Params[RealParamIndex]) as ISepiOpenArrayValue;

        CompileOpenArrayParam(Compiler, Instructions, SignatureParam,
          OpenArray, InstrParamMemory, Parameters.Parameters[I+1].MemoryRef,
          TempVars);
      end else
      begin
        if SignatureParam.HiddenKind = hpNormal then
        begin
          if RealParamIndex < ParamCount then
            ParamValue := Params[RealParamIndex] as ISepiValue
          else
            ParamValue := nil;
        end else if SignatureParam.HiddenKind in [hpSelf, hpAlloc] then
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
procedure TSepiCustomCallable.CompileExecute(Compiler: TSepiMethodCompiler;
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
  Assert(ReturnType <> nil);

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
  @param AFreeParamValue    Valeur à donner au paramètre $Free (défaut = False)
*}
constructor TSepiMethodCall.Create(AMethod: TSepiMethodBase;
  const ASelfValue: ISepiReadableValue = nil;
  AForceStaticCall: Boolean = False; AFreeParamValue: Boolean = False);
begin
  if AMethod is TSepiMethod then
  begin
    inherited Create(TSepiMethod(AMethod).Signature);
    FMethod := TSepiMethod(AMethod);
  end else
  begin
    inherited Create;
    FOverloadedMethod := AMethod as TSepiOverloadedMethod;
  end;

  FSelfValue := ASelfValue;
  FForceStaticCall := AForceStaticCall;

  FFreeParamValue := AFreeParamValue;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodCall.CheckSelfValueType;
const
  skNoSelfValue = [skStaticProcedure, skStaticFunction];
  skAlwaysOnClass = [skClassProcedure, skClassFunction];
  skValidOnClass = [skConstructor] + skAlwaysOnClass;
begin
  if SelfValue = nil then
    Exit;

  // When calling a static method, we don't care about the Self value
  if Signature.Kind in skNoSelfValue then
  begin
    FSelfValue := nil;
    Exit;
  end;

  // Can''t call an object method on a class value
  if SelfValue.ValueType is TSepiMetaClass then
  begin
    if not (Signature.Kind in skValidOnClass) then
    begin
      MakeError(SCallPatternOnlyOnClassMethod);
      Exit;
    end;
  end;

  // Call a class method on an object: auto-dereference
  if SelfValue.ValueType is TSepiClass then
  begin
    if Signature.Kind in skAlwaysOnClass then
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
  else if (Signature.Kind = skConstructor) and (SelfValue <> nil) and
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
        Break;
      end;
    end;

    if Method = nil then
      MakeError(SNoMatchingOverloadedMethod);
  end;

  CheckSelfValueType;

  if Expression <> nil then
    AttachToExpression(Expression);
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
  Crée une expression d'invocation de référence méthode
  @param AMethodRefValue   Référence de méthode
  @param AComplete         Si True, complète immédiatement
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
  SetSignature((MethodRefValue.ValueType as TSepiMethodRefType).Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodRefCall.CompleteParams;
begin
  inherited;

  if MatchesSignature(Signature) and (Expression <> nil) then
    AttachToExpression(Expression);
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
      SelfMemory.Seal;
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
  Index: Integer;
  Signature: TSepiSignature;
  IndexType: TSepiType;
begin
  Index := FProperty.Index;

  if Index = NoIndex then
  begin
    Result := nil;
    Exit;
  end;

  Signature := Method.Signature;
  IndexType := Signature.Params[Signature.ParamCount-1].ParamType;

  Result := TSepiTrueConstValue.MakeValue(Compiler,
    IndexType as TSepiOrdType, Index);
end;

{*
  [@inheritDoc]
*}
function TSepiPropertyValue.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if SameGUID(IID, ISepiValue) and (not ParamsCompleted) then
    Result := E_NOINTERFACE
  else if SameGUID(IID, ISepiReadableValue) and
    ((not ParamsCompleted) or (FProperty.ReadAccess.Kind = pakNone)) then
    Result := E_NOINTERFACE
  else if SameGUID(IID, ISepiWritableValue) and
    ((not ParamsCompleted) or (FProperty.WriteAccess.Kind = pakNone)) then
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

  if ParamsCompleted then
  begin
    Expression.Attach(ISepiValue, AsExpressionPart);

    if FProperty.ReadAccess.Kind <> pakNone then
      Expression.Attach(ISepiReadableValue, AsExpressionPart);
    if FProperty.WriteAccess.Kind <> pakNone then
      Expression.Attach(ISepiWritableValue, AsExpressionPart);
  end;
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
var
  I: Integer;
  AllParamsAssigned: Boolean;
begin
  inherited;

  AllParamsAssigned := True;
  for I := 0 to ParamCount-1 do
    if Params[I] = nil then
      AllParamsAssigned := False;

  if AllParamsAssigned then
  begin
    if MatchesSignature(FProperty.Signature) and (Expression <> nil) then
      AttachToExpression(Expression);
  end else
    MakeError(SNotEnoughActualParameters);
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyValue.CompileRead(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  RightExpression: ISepiExpression;
  RightValue: ISepiReadableValue;
  Method: TSepiMethod;
  MethodCall: ISepiWantingParams;
  I: Integer;
  IndexValue: ISepiReadableValue;
begin
  RightExpression := TSepiExpression.Create(Expression);

  if FProperty.ReadAccess.Kind = pakField then
  begin
    RightValue := TSepiObjectFieldValue.Create(ObjectValue,
      FProperty.ReadAccess.Field);
    RightValue.AttachToExpression(RightExpression);
  end else if FProperty.ReadAccess.Kind = pakMethod then
  begin
    Method := FProperty.ReadAccess.Method;
    MethodCall := TSepiMethodCall.Create(Method, ObjectValue);
    MethodCall.AttachToExpression(RightExpression);

    for I := 0 to ParamCount-1 do
      MethodCall.AddParam(Params[I]);

    IndexValue := MakeIndexValue(Compiler, Method);
    if IndexValue <> nil then
      MethodCall.AddParam(IndexValue as ISepiExpression);

    MethodCall.CompleteParams;

    RightValue := MethodCall as ISepiReadableValue;
  end else if FProperty.ReadAccess.Kind = pakClassField then
  begin
    RightValue := TSepiVariableValue.Create(FProperty.ReadAccess.ClassField);
    RightValue.AttachToExpression(RightExpression);
  end else
  begin
    Assert(False);
  end;

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
  MethodCall: ISepiWantingParams;
  I: Integer;
  IndexValue: ISepiReadableValue;
begin
  if FProperty.WriteAccess.Kind = pakField then
  begin
    LeftValue := TSepiObjectFieldValue.Create(ObjectValue,
      FProperty.WriteAccess.Field);
    LeftValue.AttachToExpression(TSepiExpression.Create(Expression));
    LeftValue.CompileWrite(Compiler, Instructions, Source);
  end else if FProperty.WriteAccess.Kind = pakMethod then
  begin
    Method := FProperty.WriteAccess.Method;
    MethodCall := TSepiMethodCall.Create(Method, ObjectValue);
    MethodCall.AttachToExpression(TSepiExpression.Create(Expression));

    for I := 0 to ParamCount-1 do
      MethodCall.AddParam(Params[I]);

    IndexValue := MakeIndexValue(Compiler, Method);
    if IndexValue <> nil then
      MethodCall.AddParam(IndexValue as ISepiExpression);

    MethodCall.AddParam(Source as ISepiExpression);

    MethodCall.CompleteParams;

    (MethodCall as ISepiExecutable).CompileExecute(Compiler, Instructions);
  end else if FProperty.WriteAccess.Kind = pakClassField then
  begin
    LeftValue := TSepiVariableValue.Create(FProperty.WriteAccess.ClassField);
    LeftValue.AttachToExpression(TSepiExpression.Create(Expression));
    LeftValue.CompileWrite(Compiler, Instructions, Source);
  end else
  begin
    Assert(False);
  end;
end;

{------------------------------------}
{ TSepiOpenArrayFromArrayValue class }
{------------------------------------}

{*
  Vérifie les types
*}
procedure TSepiOpenArrayFromArrayValue.CheckTypes;
begin
  if not (ArrayValue.ValueType is TSepiArrayType) then
    (ArrayValue as ISepiExpression).MakeError(SArrayTypeRequired);
end;

{*
  [@inheritDoc]
*}
procedure TSepiOpenArrayFromArrayValue.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  Expression.Attach(ISepiOpenArrayValue, AsExpressionPart);
end;

{*
  [@inheritDoc]
*}
function TSepiOpenArrayFromArrayValue.GetElementType: TSepiType;
begin
  Result := (ArrayValue.ValueType as TSepiArrayType).ElementType;
end;

{*
  [@inheritDoc]
*}
function TSepiOpenArrayFromArrayValue.CanForceElementType(
  AElementType: TSepiType): Boolean;
begin
  Result := AElementType.Equals(ElementType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiOpenArrayFromArrayValue.ForceElementType(
  AElementType: TSepiType);
begin
  Assert(CanForceElementType(AElementType));
end;

{*
  [@inheritDoc]
*}
procedure TSepiOpenArrayFromArrayValue.CompileReadOpenArray(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var OpenArrayDest, HighValueDest: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  ArrayType: TSepiArrayType;
  DynArrayHighInstr: TSepiAsmValueToIntStdFunction;
  TempOpenArrayDest: TSepiMemoryReference;
begin
  ArrayType := ArrayValue.ValueType as TSepiArrayType;

  ArrayValue.CompileRead(Compiler, Instructions, OpenArrayDest, TempVars);

  if ArrayType is TSepiStaticArrayType then
  begin
    HighValueDest := TSepiMemoryReference.Create(Compiler, aoAcceptAllConsts,
      SizeOf(Integer));
    HighValueDest.SetAsConst(TSepiStaticArrayType(ArrayType).ArrayLength-1);
    HighValueDest.Seal;
  end else if ArrayType is TSepiDynArrayType then
  begin
    NeedDestination(HighValueDest,
      (SepiRoot.SystemUnit as TSepiSystemUnit).Integer, Compiler, TempVars,
      Instructions.GetCurrentEndRef);

    DynArrayHighInstr := TSepiAsmValueToIntStdFunction.Create(Compiler,
      ocDynArrayHigh);
    DynArrayHighInstr.Destination.Assign(HighValueDest);
    DynArrayHighInstr.Value.Assign(OpenArrayDest);
    Instructions.Add(DynArrayHighInstr);

    if not OpenArrayDest.IsSealed then
      OpenArrayDest.AddOperation(adSimple)
    else
    begin
      TempOpenArrayDest := OpenArrayDest;
      OpenArrayDest := nil;
      try
        OpenArrayDest := TSepiMemoryReference.Clone(TempOpenArrayDest);
        OpenArrayDest.AddOperation(adSimple);
        OpenArrayDest.Seal;
      finally
        TempOpenArrayDest.Free;
      end;
    end;
  end else if ArrayType is TSepiOpenArrayType then
  begin
    HighValueDest := TSepiMemoryReference.Create(Compiler, aoAcceptAllConsts,
      SizeOf(Integer));
    HighValueDest.SetSpace(TSepiOpenArrayType(ArrayType).HighVarName);
    HighValueDest.Seal;
  end else
  begin
    Assert(False);
  end;
end;

{*
  Complète la valeur
*}
procedure TSepiOpenArrayFromArrayValue.Complete;
begin
  Assert(ArrayValue <> nil);

  CheckTypes;
end;

{*
  Construit une valeur tableau ouvert sur base d'une valeur tableau
  @param ArrayValue   Valeur tableau
  @return Valeur tableau ouvert
*}
class function TSepiOpenArrayFromArrayValue.MakeOpenArrayValue(
  const ArrayValue: ISepiReadableValue): ISepiOpenArrayValue;
var
  OpenArrayValue: TSepiOpenArrayFromArrayValue;
begin
  OpenArrayValue := TSepiOpenArrayFromArrayValue.Create;
  Result := OpenArrayValue;
  Result.AttachToExpression(TSepiExpression.Create(
    ArrayValue as ISepiExpression));
  OpenArrayValue.ArrayValue := ArrayValue;
  OpenArrayValue.Complete;
end;

{-----------------------------}
{ TSepiOpenArrayBuilder class }
{-----------------------------}

{*
  Crée une instance de TSepiOpenArrayBuilder
  @param ASepiRoot   Racine Sepi
*}
constructor TSepiOpenArrayBuilder.Create(ASepiRoot: TSepiRoot);
begin
  inherited Create;

  FVarRecType := (ASepiRoot.SystemUnit as TSepiSystemUnit).TVarRec;
  FElementType := FVarRecType;
  FItems := TInterfaceList.Create;
end;

{*
  Détermine le VType correspondant à un type Sepi
  GetVType ajuste également le paramètre ValueType pour qu'il corresponde
  exactement au type de valeur attendu, si nécessaire.
  @param ValueType   Type de valeur
  @return VType correspondant
*}
function TSepiOpenArrayBuilder.GetVType(var ValueType: TSepiType): Byte;
const
  vtError = $FF;
var
  SystemUnit: TSepiSystemUnit;
begin
  SystemUnit := ValueType.Root.SystemUnit as TSepiSystemUnit;

  case ValueType.Kind of
    tkUnknown {$IF Declared(tkPointer)}, tkPointer, tkClassRef {$IFEND}:
    begin
      if ValueType is TSepiMetaClass then
        Result := vtClass
      else if ValueType = SystemUnit.PAnsiChar then
        Result := vtPChar
      else if ValueType = SystemUnit.PWideChar then
        Result := vtPWideChar
      else if ValueType is TSepiPointerType then
        Result := vtPointer
      else
        Result := vtError;
    end;

    tkInteger:
    begin
      Result := vtInteger;
      ValueType := SystemUnit.Integer;
    end;

    tkEnumeration:
    begin
      if ValueType is TSepiBooleanType then
      begin
        Result := vtBoolean;
        ValueType := SystemUnit.Boolean;
      end else
        Result := vtError;
    end;

    tkFloat:
    begin
      case (ValueType as TSepiFloatType).FloatType of
        ftCurr: Result := vtCurrency;
        ftComp: Result := vtError;
      else
        Result := vtExtended;
        ValueType := SystemUnit.Extended;
      end;
    end;

    tkChar: Result := vtChar;
    tkString: Result := vtString;
    tkClass: Result := vtObject;
    tkWChar: Result := vtWideChar;
    tkLString: Result := vtAnsiString;
    tkWString: Result := vtWideString;
    tkVariant: Result := vtVariant;
    tkInterface: Result := vtInterface;
    tkInt64: Result := vtInt64;

    {$IF Declared(tkUString)}
    tkUString: Result := vtUnicodeString;
    {$IFEND}
  else
    Result := vtError;
  end;
end;

{*
  Compile la lecture d'un élément de tableau ouvert de const (donc TVarRec)
  @param Compiler       Compilateur
  @param Instructions   Liste d'instructions
  @param VarRecValue    Valeur de type TVarRec destination
  @param AItemValue     Valeur de l'élément à enregistrer dans VarRecValue
  @param TempVars       Gestionnaire de variables temporaires
*}
procedure TSepiOpenArrayBuilder.CompileReadVarRecItem(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  const VarRecValue: ISepiWritableValue; const AItemValue: ISepiReadableValue;
  TempVars: TSepiTempVarsLifeManager);
const
  {$IF not Declared(vtUnicodeString)}
  vtUnicodeString = vtInt64 + 1;
  {$IFEND}
  vtLast = vtUnicodeString;

  VTypeToFieldName: array[0..vtLast] of string = (
    'VInteger', 'VBoolean', 'VChar', 'VExtended', 'VString', 'VPointer',
    'VPChar', 'VObject', 'VClass', 'VWideChar', 'VPWideChar', 'VAnsiString',
    'VCurrency', 'VVariant', 'VInterface', 'VWideString', 'VInt64',
    'VUnicodeString'
  );
  VTypeFieldName = 'VType';
  ByAddressVTypes = [vtExtended, vtString, vtVariant, vtInt64];
  AsPointerVTypes = [vtAnsiString, vtInterface, vtWideString, vtUnicodeString];
  NeedTempVarVTypes = ByAddressVTypes + AsPointerVTypes;
var
  SystemUnit: TSepiSystemUnit;
  ItemValue: ISepiReadableValue;
  ItemExpression: ISepiExpression;
  ValueType: TSepiType;
  VType: Byte;
  FieldValue, VTypeValue: ISepiWritableValue;
  TempVar: TSepiLocalVar;
  TempVarValue: ISepiValue;
begin
  // Get VType and fix ValueType
  SystemUnit := SepiRoot.SystemUnit as TSepiSystemUnit;
  ItemValue := AItemValue;
  ItemExpression := ItemValue as ISepiExpression;
  ValueType := ItemValue.ValueType;
  VType := GetVType(ValueType);

  // Check for invalid value type
  if VType > vtLast then
  begin
    (ItemValue as ISepiExpression).MakeError(SInvalidArrayOfConstItem);
    Exit;
  end;

  // Make FieldValue
  FieldValue := TSepiRecordFieldValue.Create(VarRecValue,
    FVarRecType.FindComponent(VTypeToFieldName[VType]) as TSepiField);
  FieldValue.AttachToExpression(TSepiExpression.Create(ItemExpression));

  // Make VTypeValue
  VTypeValue := TSepiRecordFieldValue.Create(VarRecValue,
    FVarRecType.FindComponent(VTypeFieldName) as TSepiField);
  VTypeValue.AttachToExpression(TSepiExpression.Create(ItemExpression));

  // Convert item value to expected value type
  if not ItemValue.ValueType.Equals(ValueType) then
  begin
    Assert(TSepiConvertOperation.ConversionExists(ValueType, ItemValue));
    ItemValue := TSepiConvertOperation.ConvertValue(ValueType, ItemValue);
  end;

  // Write item value into FieldValue
  if VType in NeedTempVarVTypes then
  begin
    TempVar := Compiler.Locals.AddTempVar(ValueType);
    TempVars.BeginLife(TempVar, Instructions.GetCurrentEndRef);
    TempVarValue := TSepiLocalVarValue.MakeValue(Compiler, TempVar);

    (TempVarValue as ISepiWritableValue).CompileWrite(Compiler, Instructions,
      ItemValue);

    if VType in ByAddressVTypes then
    begin
      TempVarValue := TSepiAddressOfValue.MakeAddressOf(
        TempVarValue as ISepiAddressableValue);
    end else
    begin
      TempVarValue := TSepiCastOperator.CastValue(SystemUnit.Pointer,
        TempVarValue);
    end;

    FieldValue.CompileWrite(Compiler, Instructions,
      TempVarValue as ISepiReadableValue);
  end else
  begin
    FieldValue.CompileWrite(Compiler, Instructions, ItemValue);
  end;

  // Write VType constant into VTypeValue
  VTypeValue.CompileWrite(Compiler, Instructions,
    TSepiIntegerLiteralValue.MakeValue(Compiler, VType, VTypeValue.ValueType));
end;

{*
  Nombre d'éléments
  @return Nombre d'éléments
*}
function TSepiOpenArrayBuilder.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

{*
  Tableau zero-based des éléments
  @param Index   Index compris entre 0 inclus et ItemCount exlu
*}
function TSepiOpenArrayBuilder.GetItems(Index: Integer): ISepiReadableValue;
begin
  Result := FItems[Index] as ISepiReadableValue;
end;

{*
  [@inheritDoc]
*}
function TSepiOpenArrayBuilder.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if SameGUID(IID, ISepiOpenArrayBuilder) and Completed then
    Result := E_NOINTERFACE
  else if SameGUID(IID, ISepiOpenArrayValue) and (not Completed) then
    Result := E_NOINTERFACE
  else
    Result := inherited QueryInterface(IID, Obj);
end;

{*
  [@inheritDoc]
*}
procedure TSepiOpenArrayBuilder.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  if Completed then
  begin
    Expression.Attach(ISepiOpenArrayValue, AsExpressionPart);
    Expression.Detach(ISepiOpenArrayBuilder);
  end else
  begin
    Expression.Attach(ISepiOpenArrayBuilder, AsExpressionPart);
    Expression.Detach(ISepiOpenArrayValue);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiOpenArrayBuilder.AddItem(const AItem: ISepiReadableValue);
begin
  Assert(not Completed);

  FItems.Add(AItem);
end;

{*
  [@inheritDoc]
*}
procedure TSepiOpenArrayBuilder.Complete;
begin
  Assert(not Completed);

  FCompleted := True;
end;

{*
  [@inheritDoc]
*}
function TSepiOpenArrayBuilder.GetElementType: TSepiType;
begin
  Result := FElementType;
end;

{*
  [@inheritDoc]
*}
function TSepiOpenArrayBuilder.CanForceElementType(
  AElementType: TSepiType): Boolean;
begin
  Result := True;
end;

{*
  [@inheritDoc]
*}
procedure TSepiOpenArrayBuilder.ForceElementType(
  AElementType: TSepiType);
begin
  FElementType := AElementType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiOpenArrayBuilder.CompileReadOpenArray(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var OpenArrayDest, HighValueDest: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  StaticArrayType: TSepiStaticArrayType;
  StaticArrayVar: TSepiLocalVar;
  StaticArrayValue: ISepiValue;
  I: Integer;
  Item, IndexValue: ISepiReadableValue;
  ItemValue: ISepiWritableValue;
begin
  // Make a temporary variable to store the array

  StaticArrayType := TSepiStaticArrayType.Create(Compiler.LocalNamespace, '',
    (SepiRoot.SystemUnit as TSepiSystemUnit).Integer, 0, ItemCount-1,
    ElementType);

  StaticArrayVar := Compiler.Locals.AddTempVar(StaticArrayType);
  TempVars.BeginLife(StaticArrayVar, Instructions.GetCurrentEndRef);

  // Build the array

  StaticArrayValue := TSepiLocalVarValue.MakeValue(Compiler, StaticArrayVar);
  (StaticArrayValue as ISepiExpression).SourcePos := Expression.SourcePos;

  for I := 0 to ItemCount-1 do
  begin
    Item := Items[I];
    IndexValue := TSepiIntegerLiteralValue.MakeValue(Compiler, I);
    ItemValue := TSepiArrayItemValue.MakeArrayItemValue(StaticArrayValue,
      IndexValue) as ISepiWritableValue;

    if FElementType = FVarRecType then
      CompileReadVarRecItem(Compiler, Instructions, ItemValue, Item, TempVars)
    else
      ItemValue.CompileWrite(Compiler, Instructions, Item);
  end;

  // Fill memory spaces: OpenArrayDest and HighValueDest

  OpenArrayDest := TSepiMemoryReference.Create(Compiler);
  OpenArrayDest.SetSpace(StaticArrayVar);
  OpenArrayDest.Seal;

  HighValueDest := TSepiMemoryReference.Create(Compiler, aoAcceptAllConsts,
    SizeOf(Integer));
  HighValueDest.SetAsConst(ItemCount-1);
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
  SetValueType(UnitCompiler.SystemUnit.Integer);
  AllocateConstant;

  Integer(ConstValuePtr^) := Operand.ExprType.Size;
end;

{*
  Complète un TypeInfo
*}
procedure TSepiTypeOperationValue.CompleteTypeInfo;
begin
  SetValueType(UnitCompiler.SystemUnit.Pointer);

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

{*
  Construit une valeur opération de type
  @param Operation       Opération à effectuer sur le type
  @param Operand         Opérande
  @param NilIfTypeInfo   Si True, TypeInfo peut valoir nil
*}
class function TSepiTypeOperationValue.MakeTypeOperationValue(
  Operation: TSepiTypeOperation; const Operand: ISepiTypeExpression;
  NilIfNoTypeInfo: Boolean = False): ISepiReadableValue;
var
  TypeOperation: TSepiTypeOperationValue;
begin
  TypeOperation := TSepiTypeOperationValue.Create(Operation, NilIfNoTypeInfo);
  Result := TypeOperation;
  Result.AttachToExpression(TSepiExpression.Create(Operand as ISepiExpression));
  TypeOperation.Operand := Operand;
  TypeOperation.Complete;
end;

{---------------------}
{ TSepiNilValue class }
{---------------------}

constructor TSepiNilValue.Create(SepiRoot: TSepiRoot);
begin
  inherited Create;

  FValueType := (SepiRoot.SystemUnit as TSepiSystemUnit).Pointer;
end;

{*
  [@inheritDoc]
*}
procedure TSepiNilValue.AttachToExpression(const Expression: ISepiExpression);
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
function TSepiNilValue.GetValueType: TSepiType;
begin
  Result := FValueType;
end;

{*
  [@inheritDoc]
*}
function TSepiNilValue.GetIsConstant: Boolean;
begin
  Result := True;
end;

{*
  [@inheritDoc]
*}
function TSepiNilValue.GetConstValuePtr: Pointer;
begin
  Result := @ZeroMemory;
end;

{*
  [@inheritDoc]
*}
procedure TSepiNilValue.CompileRead(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
begin
  Destination := TSepiMemoryReference.Create(MethodCompiler, [aoAcceptZero]);
  Destination.SetSpace(msZero);
  Destination.Seal;
end;

{*
  [@inheritDoc]
*}
function TSepiNilValue.CanForceType(AValueType: TSepiType;
  Explicit: Boolean = False): Boolean;
begin
  Result := (AValueType is TSepiPointerType) or (AValueType is TSepiClass) or
    (AValueType is TSepiMetaClass) or (AValueType is TSepiInterface) or
    (AValueType is TSepiStringType) or (AValueType is TSepiDynArrayType) or
    (AValueType is TSepiMethodRefType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiNilValue.ForceType(AValueType: TSepiType);
begin
  if CanForceType(AValueType) then
    FValueType := AValueType
  else
    Assert(False);
end;

{*
  Construit une valeur nil
  @param Compiler   Compilateur
  @return Valeur nil créée
*}
class function TSepiNilValue.MakeValue(
  Compiler: TSepiCompilerBase): ISepiReadableValue;
begin
  Result := TSepiNilValue.Create(Compiler.SepiRoot);
  Result.AttachToExpression(TSepiExpression.Create(Compiler));
end;

{--------------------------------}
{ TSepiComponentExpression class }
{--------------------------------}

{*
  Crée une expression représentant un meta
  @param AComponent   Component représenté
*}
constructor TSepiComponentExpression.Create(AComponent: TSepiComponent);
begin
  inherited Create;

  FComponent := AComponent;
end;

{*
  [@inheritDoc]
*}
procedure TSepiComponentExpression.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;
  Expression.Attach(ISepiComponentExpression, AsExpressionPart);
end;

{*
  Component
  @return Component
*}
function TSepiComponentExpression.GetComponent: TSepiComponent;
begin
  Result := FComponent;
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

{----------------------------------}
{ TSepiSetOrOpenArrayBuilder class }
{----------------------------------}

{*
  Crée une instance de TSepiSetOrOpenArrayBuilder
  @param UnitCompiler   Compilateur d'unité
*}
constructor TSepiSetOrOpenArrayBuilder.Create(UnitCompiler: TSepiUnitCompiler);
begin
  inherited Create;

  FSetBuilder := TSepiSetBuilder.Create(UnitCompiler);
  FOpenArrayBuilder := TSepiOpenArrayBuilder.Create(UnitCompiler.SepiRoot);

  FOpenArrayAvailable := True;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.NeedSetBuilder;
var
  I: Integer;
begin
  if FSetBuilderCompleted then
    Exit;

  for I := 0 to Length(FSingles)-1 do
    SetBuilder.AddSingle(FSingles[I]);

  for I := 0 to Length(FRanges)-1 do
    SetBuilder.AddRange(FRanges[I].LowerValue, FRanges[I].HigherValue);

  SetBuilder.Complete;

  FSetBuilderCompleted := True;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.NeedOpenArrayBuilder;
var
  I: Integer;
begin
  Assert(FOpenArrayAvailable);

  if FOpenArrayBuilderCompleted then
    Exit;

  for I := 0 to Length(FSingles)-1 do
    OpenArrayBuilder.AddItem(FSingles[I]);

  OpenArrayBuilder.Complete;

  FOpenArrayBuilderCompleted := True;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOrOpenArrayBuilder.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if SameGUID(IID, ISepiTypeForceableSetValue) then
  begin
    NeedSetBuilder;
    Result := FSetBuilder.QueryInterface(IID, Obj);
  end else if SameGUID(IID, ISepiOpenArrayValue) and
    (not FOpenArrayAvailable) then
  begin
    Result := E_NOINTERFACE;
  end else
  begin
    Result := inherited QueryInterface(IID, Obj);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  Expression.Attach(ISepiSetOrOpenArrayBuilder, AsExpressionPart);

  if not Supports(SetBuilder, ISepiExpression) then
  begin
    SetBuilder.AttachToExpression(TSepiExpression.Create(Expression));
    OpenArrayBuilder.AttachToExpression(TSepiExpression.Create(Expression));
  end;

  if Completed then
  begin
    Expression.Attach(ISepiValue, AsExpressionPart);
    Expression.Attach(ISepiReadableValue, AsExpressionPart);

    if FOpenArrayAvailable then
      Expression.Attach(ISepiOpenArrayValue, AsExpressionPart);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.AddSingle(const Value: ISepiReadableValue);
var
  Len: Integer;
begin
  Assert(not Completed);

  Len := Length(FSingles);
  SetLength(FSingles, Len+1);
  FSingles[Len] := Value;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.AddRange(
  const Lower, Higher: ISepiReadableValue);
var
  Len: Integer;
begin
  Assert(not Completed);

  Len := Length(FRanges);
  SetLength(FRanges, Len+1);
  FRanges[Len].LowerValue := Lower;
  FRanges[Len].HigherValue := Higher;

  FOpenArrayAvailable := False;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.Complete;
begin
  Assert(not Completed);
  FCompleted := True;

  AttachToExpression(Expression);
end;

{*
  [@inheritDoc]
*}
function TSepiSetOrOpenArrayBuilder.GetValueType: TSepiType;
begin
  Result := FSetBuilder.ValueType;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOrOpenArrayBuilder.CanForceType(AValueType: TSepiType;
  Explicit: Boolean): Boolean;
begin
  Result := AValueType is TSepiSetType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.ForceType(AValueType: TSepiType);
begin
  Assert(CanForceType(AValueType, False));

  NeedSetBuilder;

  if SetBuilder.CanForceType(AValueType) then
    SetBuilder.ForceType(AValueType)
  else
  begin
    MakeError(Format(STypeMismatch, [AValueType.DisplayName,
      SetBuilder.ValueType.DisplayName]));

    FSetBuilder := TSepiSetBuilder.Create(UnitCompiler);
    FSetBuilder.AttachToExpression(TSepiExpression.Create(Expression));
    FSetBuilder.Complete;

    Assert(SetBuilder.CanForceType(AValueType));
    SetBuilder.ForceType(AValueType);
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOrOpenArrayBuilder.GetIsConstant: Boolean;
begin
  NeedSetBuilder;

  Result := (SetBuilder as ISepiReadableValue).IsConstant;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOrOpenArrayBuilder.GetConstValuePtr: Pointer;
begin
  NeedSetBuilder;

  Result := (SetBuilder as ISepiReadableValue).ConstValuePtr;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.CompileRead(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
begin
  NeedSetBuilder;

  (SetBuilder as ISepiReadableValue).CompileRead(Compiler, Instructions,
    Destination, TempVars);
end;

{*
  [@inheritDoc]
*}
function TSepiSetOrOpenArrayBuilder.GetElementType: TSepiType;
begin
  if FOpenArrayBuilderCompleted then
    Result := (OpenArrayBuilder as ISepiOpenArrayValue).ElementType
  else
    Result := (SepiRoot.SystemUnit as TSepiSystemUnit).TVarRec;
end;

{*
  [@inheritDoc]
*}
function TSepiSetOrOpenArrayBuilder.CanForceElementType(
  AElementType: TSepiType): Boolean;
begin
  Result := True;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.ForceElementType(AElementType: TSepiType);
begin
  NeedOpenArrayBuilder;

  (OpenArrayBuilder as ISepiOpenArrayValue).ForceElementType(AElementType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilder.CompileReadOpenArray(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var OpenArrayDest, HighValueDest: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
begin
  NeedOpenArrayBuilder;

  (OpenArrayBuilder as ISepiOpenArrayValue).CompileReadOpenArray(Compiler,
    Instructions, OpenArrayDest, HighValueDest, TempVars);
end;

{----------------------------------}
{ TSepiInstructionExpression class }
{----------------------------------}

{*
  Crée une expression wrapper pour une instruction
  @param AInstruction   Instruction wrappée
*}
constructor TSepiInstructionExpression.Create(AInstruction: TSepiInstruction);
begin
  inherited Create;

  FInstruction := AInstruction;
end;

{*
  [@inheritDoc]
*}
procedure TSepiInstructionExpression.AttachToExpression(
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
procedure TSepiInstructionExpression.CompileExecute(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList);
begin
  Assert(Instruction.MethodCompiler = Compiler);
  Instructions.Add(Instruction);
end;

end.

