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
  Instructions assembleur Sepi
  @author sjrd
  @version 1.0
*}
unit SepiAsmInstructions;

interface

uses
  Windows, Classes, SysUtils, TypInfo, SepiReflectionCore, SepiMembers,
  SepiArrayTypes, SepiOpCodes, SepiCompiler, SepiReflectionConsts,
  SepiCompilerConsts;

type
  {*
    Paramètres invalide dans une instruction CALL
  *}
  ESepiInvalidParamsError = class(ESepiCompilerError);

  {*
    Taille de données invalide pour un MOVE
  *}
  ESepiInvalidDataSizeError = class(ESepiCompilerError);

  {*
    Instruction NOP
    @author sjrd
    @version 1.0
  *}
  TSepiAsmNope = class(TSepiAsmInstr)
  end;

  {*
    Instruction JUMP
    @author sjrd
    @version 1.0
  *}
  TSepiAsmJump = class(TSepiAsmInstr)
  private
    FDestination: TSepiJumpDest; /// Destination du JUMP
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiJumpDest read FDestination;
  end;

  {*
    Instruction JIT ou JIF (JUMP conditionnel)
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCondJump = class(TSepiAsmInstr)
  private
    FIfTrue: Boolean;            /// True donne un JIT, False donne un JIF
    FDestination: TSepiJumpDest; /// Destination du JUMP
    FTest: TSepiMemoryReference; /// Condition du saut
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AIfTrue: Boolean = True);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property IfTrue: Boolean read FIfTrue write FIfTrue;
    property Destination: TSepiJumpDest read FDestination;
    property Test: TSepiMemoryReference read FTest;
  end;

  {*
    Paramètre d'une instruction CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCallParam = class(TObject)
  private
    FName: string;                    /// Nom du paramètre
    FSepiStackOffset: Integer;        /// Offset dans la pile Sepi
    FParamSize: TSepiParamSize;       /// Taille du paramètre
    FStackUsage: Integer;             /// Nombre de DWord utilisés dans la pile
    FMemoryRef: TSepiMemoryReference; /// Référence mémoire

    FSize: Integer; /// Taille écrite dans le flux
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      ASepiStackOffset: Integer; AParamSize: TSepiParamSize;
      const AName: string = '');
    destructor Destroy; override;

    procedure Make;
    procedure WriteToStream(Stream: TStream);

    property Name: string read FName;
    property SepiStackOffset: Integer read FSepiStackOffset;
    property ParamSize: TSepiParamSize read FParamSize;
    property StackUsage: Integer read FStackUsage;
    property MemoryRef: TSepiMemoryReference read FMemoryRef;

    property Size: Integer read FSize;
  end;

  {*
    Paramètres d'une instruction CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCallParams = class(TObject)
  private
    FMethodCompiler: TSepiMethodCompiler; /// Compilateur de méthode

    FParameters: array of TSepiAsmCallParam; /// Paramètres
    FResult: TSepiMemoryReference;           /// Résultat

    FSize: Integer; /// Taille écrite dans le flux

    procedure SortParameters;

    function GetCount: Integer;
    function GetParameters(Index: Integer): TSepiAsmCallParam;
    function GetParamByName(const Name: string): TSepiMemoryReference;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Prepare(Signature: TSepiSignature);

    function AddParam(SepiStackOffset: Integer;
      ParamSize: TSepiParamSize): TSepiMemoryReference;

    procedure Make;
    procedure WriteToStream(Stream: TStream);

    property MethodCompiler: TSepiMethodCompiler read FMethodCompiler;

    property Count: Integer read GetCount;
    property Parameters[Index: Integer]: TSepiAsmCallParam
      read GetParameters;
    property ParamByName[const Name: string]: TSepiMemoryReference
      read GetParamByName; default;
    property Result: TSepiMemoryReference read FResult;

    property Size: Integer read FSize;
  end;

  {*
    Instruction CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCall = class(TSepiAsmInstr)
  private
    FParameters: TSepiAsmCallParams; /// Paramètres
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Prepare(Signature: TSepiSignature); overload; virtual;
    procedure Prepare(Component: TSepiComponent); overload;
    procedure Prepare(const ComponentName: string); overload;

    procedure Make; override;

    property Parameters: TSepiAsmCallParams read FParameters;
  end;

  {*
    Instruction Address CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmAddressCall = class(TSepiAsmCall)
  private
    FCallingConvention: TCallingConvention;   /// Convention d'appel
    FRegUsage: Byte;                          /// Utilisation des registres
    FResultBehavior: TSepiTypeResultBehavior; /// Comportement du résultat
    FOrdResultSize: Byte;                     /// Taille d'un résultat ordinal

    FAddress: TSepiMemoryReference; /// Adresse de la méthode à appeler
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Prepare(Signature: TSepiSignature); override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property CallingConvention: TCallingConvention
      read FCallingConvention write FCallingConvention;
    property RegUsage: Byte read FRegUsage write FRegUsage;
    property ResultBehavior: TSepiTypeResultBehavior
      read FResultBehavior write FResultBehavior;
    property OrdResultSize: Byte read FOrdResultSize write FOrdResultSize;

    property Address: TSepiMemoryReference read FAddress;
  end;

  {*
    Instruction CALL avec une référence à la méthode
    @author sjrd
    @version 1.0
  *}
  TSepiAsmRefCall = class(TSepiAsmCall)
  private
    FMethodRef: Integer; /// Référence à la méthode
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    procedure SetMethod(Method: TSepiMethod;
      PrepareParams: Boolean = True); overload;
    procedure SetMethod(const MethodName: string;
      PrepareParams: Boolean = True); overload;

    property MethodRef: Integer read FMethodRef write FMethodRef;
  end;

  {*
    Instruction Static CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmStaticCall = class(TSepiAsmRefCall)
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;
  end;

  {*
    Instruction Dynamic CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmDynamicCall = class(TSepiAsmRefCall)
  private
    FSelfMem: TSepiMemoryReference; /// Référence mémoire au Self
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property SelfMem: TSepiMemoryReference read FSelfMem;
  end;

  {*
    Instruction LEA
    @author sjrd
    @version 1.0
  *}
  TSepiAsmLoadAddress = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FSource: TSepiMemoryReference;      /// Source
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property Source: TSepiMemoryReference read FSource;
  end;

  {*
    Instruction MOVE
    @author sjrd
    @version 1.0
  *}
  TSepiAsmMove = class(TSepiAsmInstr)
  private
    FDataSize: Word;      /// Taille des données à copier
    FDataType: TSepiType; /// Type des données à copier (si requis)

    FDestination: TSepiMemoryReference; /// Destination
    FSource: TSepiMemoryReference;      /// Source
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      ADataSize: Word); overload;
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      ADataType: TSepiType); overload;
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property DataSize: Word read FDataSize;
    property DataType: TSepiType read FDataType;

    property Destination: TSepiMemoryReference read FDestination;
    property Source: TSepiMemoryReference read FSource;
  end;

  {*
    Instruction CVRT
    @author sjrd
    @version 1.0
  *}
  TSepiAsmConvert = class(TSepiAsmInstr)
  private
    FToType: TSepiBaseType;   /// Type de la destination
    FFromType: TSepiBaseType; /// Type de la source

    FDestination: TSepiMemoryReference; /// Destination
    FSource: TSepiMemoryReference;      /// Source
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AToType, AFromType: TSepiBaseType);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property ToType: TSepiBaseType read FToType;
    property FromType: TSepiBaseType read FFromType;

    property Destination: TSepiMemoryReference read FDestination;
    property Source: TSepiMemoryReference read FSource;
  end;

  {*
    Instruction opération
    @author sjrd
    @version 1.0
  *}
  TSepiAsmOperation = class(TSepiAsmInstr)
  private
    FVarType: TSepiBaseType; /// Type des variables
    FUseLeft: Boolean;       /// Utilise un opérande gauche
    FUseRight: Boolean;      /// Utilise un opérande droit

    FDestination: TSepiMemoryReference; /// Destination
    FLeft: TSepiMemoryReference;        /// Opérande gauche
    FRight: TSepiMemoryReference;       /// Opérande droit
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AOpCode: TSepiOpCode; AVarType: TSepiBaseType);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property VarType: TSepiBaseType read FVarType;
    property UseLeft: Boolean read FUseLeft;
    property UseRight: Boolean read FUseRight;

    property Destination: TSepiMemoryReference read FDestination;
    property Left: TSepiMemoryReference read FLeft;
    property Right: TSepiMemoryReference read FRight;

    /// Opérande unique
    property Source: TSepiMemoryReference read FLeft;
  end;

  {*
    Instruction de comparaison
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCompare = class(TSepiAsmInstr)
  private
    FVarType: TSepiBaseType; /// Type des variables

    FDestination: TSepiMemoryReference; /// Destination
    FLeft: TSepiMemoryReference;        /// Opérande gauche
    FRight: TSepiMemoryReference;       /// Opérande droit
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AOpCode: TSepiOpCode; AVarType: TSepiBaseType);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property VarType: TSepiBaseType read FVarType;

    property Destination: TSepiMemoryReference read FDestination;
    property Left: TSepiMemoryReference read FLeft;
    property Right: TSepiMemoryReference read FRight;
  end;

  {*
    Instruction GTI, GDC ou GMC
    @author sjrd
    @version 1.0
  *}
  TSepiAsmGetRunInfo = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FReference: Integer;                /// Référence
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AOpCode: TSepiOpCode);
    destructor Destroy; override;

    procedure SetReference(Reference: TSepiComponent); overload;
    procedure SetReference(const RefName: string); overload;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property Reference: Integer read FReference write FReference;
  end;

  {*
    Instruction IS
    @author sjrd
    @version 1.0
  *}
  TSepiAsmIsClass = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FMemObject: TSepiMemoryReference;   /// Objet mémoire
    FMemClass: TSepiMemoryReference;    /// Classe mémoire
    FClassRef: Integer;                 /// Référence à une classe (si msZero)
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property MemObject: TSepiMemoryReference read FMemObject;
    property MemClass: TSepiMemoryReference read FMemClass;
    property ClassRef: Integer read FClassRef write FClassRef;
  end;

  {*
    Instruction AS
    @author sjrd
    @version 1.0
  *}
  TSepiAsmAsClass = class(TSepiAsmInstr)
  private
    FMemObject: TSepiMemoryReference; /// Objet mémoire
    FMemClass: TSepiMemoryReference;  /// Classe mémoire
    FClassRef: Integer;               /// Référence à une classe (si msZero)
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property MemObject: TSepiMemoryReference read FMemObject;
    property MemClass: TSepiMemoryReference read FMemClass;
    property ClassRef: Integer read FClassRef write FClassRef;
  end;

  {*
    Instruction RAISE
    @author sjrd
    @version 1.0
  *}
  TSepiAsmRaise = class(TSepiAsmInstr)
  private
    FExceptObject: TSepiMemoryReference; /// Objet exception
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property ExceptObject: TSepiMemoryReference read FExceptObject;
  end;

  {*
    Instruction RERS
    @author sjrd
    @version 1.0
  *}
  TSepiAsmReraise = class(TSepiAsmInstr)
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
  end;

  {*
    Instruction TRYE
    @author sjrd
    @version 1.0
  *}
  TSepiAsmTryExcept = class(TSepiAsmInstr)
  private
    FEndOfTry: TSepiJumpDest;            /// Fin du try
    FEndOfExcept: TSepiJumpDest;         /// Fin du except
    FExceptObject: TSepiMemoryReference; /// Objet exception
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property EndOfTry: TSepiJumpDest read FEndOfTry;
    property EndOfExcept: TSepiJumpDest read FEndOfExcept;
    property ExceptObject: TSepiMemoryReference read FExceptObject;
  end;

  {*
    Instruction TRYF
    @author sjrd
    @version 1.0
  *}
  TSepiAsmTryFinally = class(TSepiAsmInstr)
  private
    FEndOfTry: TSepiJumpDest;     /// Fin du try
    FEndOfFinally: TSepiJumpDest; /// Fin du finally
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property EndOfTry: TSepiJumpDest read FEndOfTry;
    property EndOfFinally: TSepiJumpDest read FEndOfFinally;
  end;

  {*
    Clause ON d'un MultiOn
    @author sjrd
    @version 1.0
  *}
  TSepiAsmOnClause = record
    ClassRef: Integer;          /// Référence à une classe
    Destination: TSepiJumpDest; /// Destination du ON
  end;

  {*
    Instruction ON
    @author sjrd
    @version 1.0
  *}
  TSepiAsmMultiOn = class(TSepiAsmInstr)
  private
    FExceptObject: TSepiMemoryReference; /// Objet exception

    FOnClauses: array of TSepiAsmOnClause; /// Clauses ON

    function GetOnClauseCount: Integer;
    function GetOnClauses(Index: Integer): TSepiAsmOnClause;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    function AddOnClause(AClassRef: Integer): TSepiJumpDest; overload;
    function AddOnClause(SepiClass: TSepiClass): TSepiJumpDest; overload;
    function AddOnClause(const ClassName: string): TSepiJumpDest; overload;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property ExceptObject: TSepiMemoryReference read FExceptObject;

    property OnClauseCount: Integer read GetOnClauseCount;
    property OnClauses[Index: Integer]: TSepiAsmOnClause read GetOnClauses;
  end;

  {*
    Instruction SINC ou SEXC
    @author sjrd
    @version 1.0
  *}
  TSepiAsmSetIncludeExclude = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FElement: TSepiMemoryReference;     /// Élément
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AInclude: Boolean);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property Element: TSepiMemoryReference read FElement;
  end;

  {*
    Instruction SIN
    @author sjrd
    @version 1.0
  *}
  TSepiAsmSetIn = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FSetValue: TSepiMemoryReference;    /// Ensemble
    FElement: TSepiMemoryReference;     /// Élément
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property SetValue: TSepiMemoryReference read FSetValue;
    property Element: TSepiMemoryReference read FElement;
  end;

  {*
    Instruction SELE
    @author sjrd
    @version 1.0
  *}
  TSepiAsmSetElem = class(TSepiAsmInstr)
  private
    FSetSize: Byte;                     /// Taille du set
    FUnion: Boolean;                    /// Indique si c'est un Union-Elem (Inc)
    FDestination: TSepiMemoryReference; /// Destination
    FElement: TSepiMemoryReference;     /// Élément
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      ASetSize: Byte; AUnion: Boolean = False);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property SetSize: Byte read FSetSize;
    property Union: Boolean read FUnion;
    property Destination: TSepiMemoryReference read FDestination;
    property Element: TSepiMemoryReference read FElement;
  end;

  {*
    Instruction SRNG ou SUR
    @author sjrd
    @version 1.0
  *}
  TSepiAsmSetRange = class(TSepiAsmInstr)
  private
    FSetSize: Byte;                     /// Taille du set
    FUnion: Boolean;                    /// Indique si c'est un Union-Range
    FDestination: TSepiMemoryReference; /// Destination
    FLowerBound: TSepiMemoryReference;  /// Borne inférieure
    FHigherBound: TSepiMemoryReference; /// Borne supérieure
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      ASetSize: Byte; AUnion: Boolean = False);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property SetSize: Byte read FSetSize;
    property Union: Boolean read FUnion;
    property Destination: TSepiMemoryReference read FDestination;
    property LowerBound: TSepiMemoryReference read FLowerBound;
    property HigherBound: TSepiMemoryReference read FHigherBound;
  end;

  {*
    Instruction de comparaison de sets
    @author sjrd
    @version 1.0
  *}
  TSepiAsmSetCompare = class(TSepiAsmInstr)
  private
    FSetSize: Byte; /// Taille des sets

    FDestination: TSepiMemoryReference; /// Destination
    FLeft: TSepiMemoryReference;        /// Opérande gauche
    FRight: TSepiMemoryReference;       /// Opérande droit
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AOpCode: TSepiOpCode; ASetSize: Byte);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property SetSize: Byte read FSetSize;

    property Destination: TSepiMemoryReference read FDestination;
    property Left: TSepiMemoryReference read FLeft;
    property Right: TSepiMemoryReference read FRight;
  end;

  {*
    Instruction opération sur des sets
    @author sjrd
    @version 1.0
  *}
  TSepiAsmSetOperation = class(TSepiAsmInstr)
  private
    FSetSize: Byte;    /// Taille des sets
    FUseLeft: Boolean; /// Utilise un opérande gauche

    FDestination: TSepiMemoryReference; /// Destination
    FLeft: TSepiMemoryReference;        /// Opérande gauche
    FRight: TSepiMemoryReference;       /// Opérande droit
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AOpCode: TSepiOpCode; ASetSize: Byte);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property SetSize: Byte read FSetSize;
    property UseLeft: Boolean read FUseLeft;

    property Destination: TSepiMemoryReference read FDestination;
    property Left: TSepiMemoryReference read FLeft;
    property Right: TSepiMemoryReference read FRight;
  end;

  {*
    Instruction SEXP
    @author sjrd
    @version 1.0
  *}
  TSepiAsmSetExpand = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FSource: TSepiMemoryReference;      /// Source
    FLowerByte: TSepiMemoryReference;   /// Octet inférieur dans le packed
    FHigherByte: TSepiMemoryReference;  /// Octet supérieur dans le packed
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property Source: TSepiMemoryReference read FSource;
    property LowerBound: TSepiMemoryReference read FLowerByte;
    property HigherBound: TSepiMemoryReference read FHigherByte;
  end;

  {*
    Instruction ASL, WSL, DAL ou DAH
    @author sjrd
    @version 1.0
  *}
  TSepiAsmValueToIntStdFunction = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FValue: TSepiMemoryReference;       /// Valeur
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AOpCode: TSepiOpCode);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property Value: TSepiMemoryReference read FValue;
  end;

  {*
    Instruction ASSL ou WSSL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmStrSetLength = class(TSepiAsmInstr)
  private
    FStrValue: TSepiMemoryReference;  /// Valeur chaîne
    FNewLength: TSepiMemoryReference; /// Nouvelle longueur de la chaîne
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      AOpCode: TSepiOpCode);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property StrValue: TSepiMemoryReference read FStrValue;
    property NewLength: TSepiMemoryReference read FNewLength;
  end;

  {*
    Instruction DASL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmDynArraySetLength = class(TSepiAsmInstr)
  private
    FDynArrayType: TSepiDynArrayType;           /// Type du tableau
    FDynArrayValue: TSepiMemoryReference;       /// Tableau
    FDimCount: Integer;                         /// Nombre de dimensions
    FDimensions: array of TSepiMemoryReference; /// Dimensions

    function GetDimensions(Index: Integer): TSepiMemoryReference;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      ADynArrayType: TSepiDynArrayType; ADimCount: Integer = 1);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property DynArrayType: TSepiDynArrayType read FDynArrayType;
    property DynArrayValue: TSepiMemoryReference read FDynArrayValue;
    property DimCount: Integer read FDimCount;
    property Dimensions[Index: Integer]: TSepiMemoryReference
      read GetDimensions;
    property FirstDimension: TSepiMemoryReference index 0 read GetDimensions;
  end;

implementation

const
  /// Nombre maximum de paramètres pour avoir un taille en Byte (sinon Word)
  MaxParamCountForByteSize = 255 div 3; // 3 is max param size (Extended)

  /// OpCodes d'opérations unaires sur soi-même
  SelfUnaryOps = [ocSelfInc..ocSelfNeg];

  /// OpCodes d'opérations binaires sur soi-même
  SelfBinaryOps = [ocSelfAdd..ocSelfXor];

  /// OpCodes d'opérations unaires sur un autre
  OtherUnaryOps = [ocOtherInc..ocOtherNeg];

  /// OpCodes d'opérations binaires sur un autre
  OtherBinaryOps = [ocOtherAdd..ocOtherXor];

  /// OpCodes d'opérations
  OperationsOpCodes =
    SelfUnaryOps + SelfBinaryOps + OtherUnaryOps + OtherBinaryOps;

  /// OpCodes de comparaison de sets
  SetComparisonOpCodes = [ocSetEquals..ocSetContained];

  /// OpCodes d'opérations sur des sets sur soi-même
  SetSelfOps = [ocSetSelfIntersect..ocSetSelfSubtract];

  /// OpCodes d'opérations sur des sets sur un autre
  SetOtherOps = [ocSetOtherIntersect..ocSetOtherSubtract];

  /// OpCodes d'opérations sur des sets
  SetOperationsOpCodes = SetSelfOps + SetOtherOps;

{--------------------}
{ TSepiAsmJump class }
{--------------------}

{*
  Crée une instruction JUMP
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmJump.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocJump;

  FDestination := TSepiJumpDest.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmJump.Destroy;
begin
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmJump.Make;
begin
  Destination.Make;
  FSize := SizeOf(TSepiOpCode) + SizeOf(Smallint);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmJump.WriteToStream(Stream: TStream);
begin
  inherited;
  Destination.WriteToStream(Stream, EndPosition);
end;

{------------------------}
{ TSepiAsmCondJump class }
{------------------------}

{*
  Crée une instruction JIT ou JIF
  @param AMethodCompiler   Compilateur de méthode
  @param AIfTrue            True donne un JIT plutôt qu'un JIF
*}
constructor TSepiAsmCondJump.Create(AMethodCompiler: TSepiMethodCompiler;
  AIfTrue: Boolean = True);
begin
  inherited Create(AMethodCompiler);

  FIfTrue := AIfTrue;
  FDestination := TSepiJumpDest.Create(MethodCompiler);
  FTest := TSepiMemoryReference.Create(MethodCompiler, aoAcceptAllConsts,
    SizeOf(Boolean));
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCondJump.Destroy;
begin
  FTest.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCondJump.Make;
begin
  if IfTrue then
    FOpCode := ocJumpIfTrue
  else
    FOpCode := ocJumpIfFalse;

  Destination.Make;
  Test.Make;

  FSize := SizeOf(TSepiOpCode) + SizeOf(Smallint) + Test.Size;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCondJump.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream, EndPosition);
  Test.WriteToStream(Stream);
end;

{-------------------------}
{ TSepiAsmCallParam class }
{-------------------------}

{*
  Crée un paramètre
  @param AMethodCompiler   Compilateur de méthode Sepi
  @param ASepiStackOffset   Offset dans la pile Sepi
  @param AParamSize         Taille de paramètre
*}
constructor TSepiAsmCallParam.Create(AMethodCompiler: TSepiMethodCompiler;
  ASepiStackOffset: Integer; AParamSize: TSepiParamSize;
  const AName: string = '');
begin
  inherited Create;

  FName := AName;
  FSepiStackOffset := ASepiStackOffset;
  FParamSize := AParamSize;
  FMemoryRef := TSepiMemoryReference.Create(AMethodCompiler,
    aoAcceptAllConsts, ParamSize);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCallParam.Destroy;
begin
  FMemoryRef.Free;

  inherited;
end;

{*
  Construit le paramètre
*}
procedure TSepiAsmCallParam.Make;
const
  StackUsages: array[0..10] of Integer = (
    1 {address}, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3
  );
begin
  FSepiStackOffset := FSepiStackOffset and $FFFFFFFC;
  FStackUsage := StackUsages[FParamSize];

  MemoryRef.Make;
  FSize := SizeOf(TSepiParamSize) + MemoryRef.Size;
end;

{*
  Ecrit le paramètre dans un flux
  @param Stream   Flux de destination
*}
procedure TSepiAsmCallParam.WriteToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FParamSize, SizeOf(TSepiParamSize));
  MemoryRef.WriteToStream(Stream);
end;

{--------------------------}
{ TSepiAsmCallParams class }
{--------------------------}

{*
  Crée une liste de paramètres d'intruction CALL
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmCallParams.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create;

  FMethodCompiler := AMethodCompiler;

  FResult := TSepiMemoryReference.Create(MethodCompiler, [aoZeroAsNil]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCallParams.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FParameters)-1 do
    FParameters[I].Free;

  inherited;
end;

{*
  Trie les paramètres selon l'ordre de leurs propriétés SepiStackOffset
*}
procedure TSepiAsmCallParams.SortParameters;
var
  I, J, ParamCount, Index: Integer;
  OrderedParams: array of TSepiAsmCallParam;
  Param, OldParam: TSepiAsmCallParam;
begin
  ParamCount := Length(FParameters);
  SetLength(OrderedParams, 3*ParamCount);
  FillChar(OrderedParams[0], 3*ParamCount*SizeOf(TSepiAsmCallParams), 0);

  // Fill OrderedParams and check for multiple params with same offset

  for I := 0 to ParamCount - 1 do
  begin
    Param := FParameters[I];
    Index := Param.SepiStackOffset div 4;

    for J := Index to Index + Param.StackUsage - 1 do
    begin
      if J >= Length(OrderedParams) then
        raise ESepiInvalidParamsError.CreateRes(
          @SParamsSepiStackOffsetsDontFollow);

      if OrderedParams[J] <> nil then
        raise ESepiInvalidParamsError.CreateRes(
          @SMultipleParamsWithSameSepiStackOffset);

      OrderedParams[J] := Param;
    end;
  end;

  // Rewrite FParameters

  OldParam := nil;
  J := 0;
  for I := 0 to 3 * ParamCount - 1 do
  begin
    Param := OrderedParams[I];
    if Param = nil then
      Break;
    if Param = OldParam then
      Continue;

    FParameters[J] := Param;
    Inc(J);
    OldParam := Param;
  end;

  // Ensure there is no forgotten parameters (it would be a hole in offsets)

  if J < ParamCount then
    raise ESepiInvalidParamsError.CreateRes(
      @SParamsSepiStackOffsetsDontFollow);
end;

{*
  Nombre de paramètres
  @return Nombre de paramètres
*}
function TSepiAsmCallParams.GetCount: Integer;
begin
  Result := Length(FParameters);
end;

{*
  Tableau zero-based des paramètres
  @param Index   Index d'un paramètre
  @return Paramètre à l'index spécifié
*}
function TSepiAsmCallParams.GetParameters(Index: Integer): TSepiAsmCallParam;
begin
  Result := FParameters[Index];
end;

{*
  Tableau des références mémoires des paramètres préparés indexés par leurs noms
  @param Name   Nom du paramètre
  @return Référence mémoire du paramètre
  @throws ESepiComponentNotFoundError Le paramètre n'a pas été trouvé
*}
function TSepiAsmCallParams.GetParamByName(
  const Name: string): TSepiMemoryReference;
var
  I: Integer;
begin
  for I := 0 to Length(FParameters)-1 do
  begin
    if AnsiSameText(FParameters[I].Name, Name) then
    begin
      Result := FParameters[I].MemoryRef;
      Exit;
    end;
  end;

  raise ESepiComponentNotFoundError.CreateFmt(SSepiComponentNotFound, [Name]);
end;

{*
  Prépare les paramètres en fonction d'une signature
  Préparer les paramètres a de multiples avantages. Il ne faut plus se
  préoccuper des offsets et des tailles des paramètres, ni de leur éventuel
  passage par adresse. De plus, pour les résultats qui sont passés comme
  paramètres, le fait de préparer les paramètres assigne à la propriété Result
  la référence mémoire à ce paramètre. Il n'y a donc plus de différences entre
  un résultat passé par adresse ou pas.
  @param Signature   Signature
*}
procedure TSepiAsmCallParams.Prepare(Signature: TSepiSignature);
var
  I: Integer;
  Param: TSepiParam;
  ParamSize: TSepiParamSize;
begin
  for I := 0 to Length(FParameters)-1 do
    FParameters[I].Free;
  SetLength(FParameters, Signature.ActualParamCount);

  for I := 0 to Signature.ActualParamCount-1 do
  begin
    Param := Signature.ActualParams[I];

    if Param.CallInfo.ByAddress then
      ParamSize := psByAddress
    else
      ParamSize := Param.ParamType.Size;

    FParameters[I] := TSepiAsmCallParam.Create(MethodCompiler,
      Param.CallInfo.SepiStackOffset, ParamSize, Param.Name);

    if Param.HiddenKind = hpResult then
      FResult := FParameters[I].MemoryRef;
  end;
end;

{*
  Ajoute un paramètre
  @param SepiStackOffset   Offset dans la pile Sepi
  @param ParamSize         Taille de paramètre
*}
function TSepiAsmCallParams.AddParam(SepiStackOffset: Integer;
  ParamSize: TSepiParamSize): TSepiMemoryReference;
var
  Index: Integer;
begin
  Index := Length(FParameters);
  SetLength(FParameters, Index+1);

  FParameters[Index] := TSepiAsmCallParam.Create(MethodCompiler,
    SepiStackOffset, ParamSize);
  Result := FParameters[Index].MemoryRef;
end;

{*
  Construit la liste des paramètres
*}
procedure TSepiAsmCallParams.Make;
var
  I, ParamCount: Integer;
begin
  // Make parameters
  FSize := 0;
  ParamCount := Length(FParameters);
  for I := 0 to ParamCount-1 do
  begin
    FParameters[I].Make;
    Inc(FSize, FParameters[I].Size);
  end;

  // Order parameters following their SepiStackOffset property
  SortParameters;

  // Make result
  Result.Make;
  Inc(FSize, Result.Size);

  // Head bytes
  if ParamCount > MaxParamCountForByteSize then
    Inc(FSize, SizeOf(Byte) + SizeOf(Word))
  else
    Inc(FSize, 2*SizeOf(Byte));
end;

{*
  Ecrit les paramètres dans un flux
  @param Stream   Flux de destination
*}
procedure TSepiAsmCallParams.WriteToStream(Stream: TStream);
var
  I, ParamCount, ParamsSize: Integer;
begin
  // Parameter count

  ParamCount := Length(FParameters);
  Stream.WriteBuffer(ParamCount, SizeOf(Byte));

  // Parameters size

  if ParamCount = 0 then
    ParamsSize := 0
  else
  begin
    with FParameters[ParamCount-1] do
      ParamsSize := SepiStackOffset div 4 + StackUsage;
  end;

  if ParamCount > MaxParamCountForByteSize then
    Stream.WriteBuffer(ParamsSize, SizeOf(Word))
  else
    Stream.WriteBuffer(ParamsSize, SizeOf(Byte));

  // Parameters

  for I := 0 to ParamCount-1 do
    FParameters[I].WriteToStream(Stream);

  // Result

  FResult.WriteToStream(Stream);
end;

{--------------------}
{ TSepiAsmCall class }
{--------------------}

{*
  Crée une instruction CALL
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmCall.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FParameters := TSepiAsmCallParams.Create(MethodCompiler)
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCall.Destroy;
begin
  FParameters.Free;

  inherited;
end;

{*
  Prépare les paramètres en fonction d'une signature
  @param Signature   Signature
*}
procedure TSepiAsmCall.Prepare(Signature: TSepiSignature);
begin
  Parameters.Prepare(Signature);
end;

{*
  Prépare les paramètres en fonction de la signature d'un meta
  @param Component   Méthode ou type référence de méthode
*}
procedure TSepiAsmCall.Prepare(Component: TSepiComponent);
begin
  if Component is TSepiMethod then
    Prepare(TSepiMethod(Component).Signature)
  else if Component is TSepiMethodRefType then
    Prepare(TSepiMethodRefType(Component).Signature)
  else
    raise ESepiCompilerError.CreateResFmt(@SObjectMustHaveASignature,
      [Component.GetFullName]);
end;

{*
  Prépare les paramètres en fonction de la signature d'un meta
  @param Signature   Signature
*}
procedure TSepiAsmCall.Prepare(const ComponentName: string);
var
  Component: TSepiComponent;
begin
  Component := MethodCompiler.SepiMethod.LookFor(ComponentName);

  if Component = nil then
    raise ESepiComponentNotFoundError.CreateFmt(
      SSepiComponentNotFound, [ComponentName]);

  Prepare(Component);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCall.Make;
begin
  Parameters.Make;
  FSize := SizeOf(TSepiOpCode) + Parameters.Size;
end;

{---------------------------}
{ TSepiAsmAddressCall class }
{---------------------------}

{*
  Crée une instruction Basic CALL
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmAddressCall.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocAddressCall;

  FCallingConvention := ccRegister;
  FRegUsage := 0;
  FResultBehavior := rbNone;
  FOrdResultSize := 0;

  FAddress := TSepiMemoryReference.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmAddressCall.Destroy;
begin
  FAddress.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAddressCall.Prepare(Signature: TSepiSignature);
begin
  inherited;

  CallingConvention := Signature.CallingConvention;
  RegUsage := Signature.RegUsage;
  ResultBehavior := Signature.ReturnType.SafeResultBehavior;

  if ResultBehavior = rbOrdinal then
    OrdResultSize := Signature.ReturnType.Size
  else
    OrdResultSize := 0;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAddressCall.Make;
begin
  inherited;

  Address.Make;

  Inc(FSize, SizeOf(TSepiCallSettings));
  if ResultBehavior = rbOrdinal then
    Inc(FSize, 1);
  Inc(FSize, Address.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAddressCall.WriteToStream(Stream: TStream);
var
  CallSettings: TSepiCallSettings;
begin
  inherited;

  CallSettings := CallSettingsEncode(CallingConvention, RegUsage,
    ResultBehavior);
  Stream.WriteBuffer(CallSettings, SizeOf(TSepiCallSettings));

  if ResultBehavior = rbOrdinal then
    Stream.WriteBuffer(FOrdResultSize, 1);

  Address.WriteToStream(Stream);
  Parameters.WriteToStream(Stream);
end;

{-----------------------}
{ TSepiAsmRefCall class }
{-----------------------}

{*
  Crée une instruction CALL avec une référence à la méthode
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmRefCall.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FMethodRef := 0;
end;

{*
  Renseigne la méthode à appeler
  @param Method     Méthode à appeler
  @param APrepare   Si True, prépare les paramètres
*}
procedure TSepiAsmRefCall.SetMethod(Method: TSepiMethod;
  PrepareParams: Boolean = True);
begin
  FMethodRef := MethodCompiler.UnitCompiler.MakeReference(Method);

  if PrepareParams then
    Prepare(Method.Signature);
end;

{*
  Renseigne la méthode à appeler
  @param Method     Méthode à appeler
  @param APrepare   Si True, prépare les paramètres
*}
procedure TSepiAsmRefCall.SetMethod(const MethodName: string;
  PrepareParams: Boolean = True);
var
  Method: TSepiMethod;
begin
  Method := MethodCompiler.SepiMethod.LookFor(MethodName) as TSepiMethod;

  if Method = nil then
    raise ESepiComponentNotFoundError.CreateFmt(
      SSepiComponentNotFound, [MethodName]);

  SetMethod(Method, PrepareParams);
end;

{--------------------------}
{ TSepiAsmStaticCall class }
{--------------------------}

{*
  Crée une instruction Static CALL
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmStaticCall.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocStaticCall;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmStaticCall.Make;
begin
  inherited;

  Inc(FSize, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmStaticCall.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FMethodRef, SizeOf(Integer));

  Parameters.WriteToStream(Stream);
end;

{---------------------------}
{ TSepiAsmDynamicCall class }
{---------------------------}

{*
  Crée une instruction Dynamic CALL
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmDynamicCall.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocDynamicCall;

  FSelfMem := TSepiMemoryReference.Create(MethodCompiler, [aoAcceptZero]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmDynamicCall.Destroy;
begin
  FSelfMem.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmDynamicCall.Make;
begin
  inherited;

  SelfMem.Make;

  Inc(FSize, SizeOf(Integer));
  Inc(FSize, SelfMem.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmDynamicCall.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FMethodRef, SizeOf(Integer));

  SelfMem.WriteToStream(Stream);
  Parameters.WriteToStream(Stream);
end;

{---------------------------}
{ TSepiAsmLoadAddress class }
{---------------------------}

{*
  Crée une instruction LEA
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmLoadAddress.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocLoadAddress;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FSource := TSepiMemoryReference.Create(MethodCompiler,
    [aoAcceptAddressedConst]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmLoadAddress.Destroy;
begin
  FSource.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmLoadAddress.Make;
begin
  inherited;

  Destination.Make;
  Source.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, Source.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmLoadAddress.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  Source.WriteToStream(Stream);
end;

{--------------------}
{ TSepiAsmMove class }
{--------------------}

{*
  Crée une instruction MOVE non typée
  @param AMethodCompiler   Compilateur de méthode
  @param ADataSize          Taille des données à copier
*}
constructor TSepiAsmMove.Create(AMethodCompiler: TSepiMethodCompiler;
  ADataSize: Word);
const
  SmallDataSizeOpCodes: array[1..10] of TSepiOpCode = (
    ocMoveByte, ocMoveWord, ocMoveSome, ocMoveDWord, ocMoveSome,
    ocMoveSome, ocMoveSome, ocMoveQWord, ocMoveSome, ocMoveExt
  );
begin
  if ADataSize = 0 then
    raise ESepiInvalidDataSizeError.CreateRes(@SInvalidDataSize);

  inherited Create(AMethodCompiler);

  FDataSize := ADataSize;
  FDataType := nil;

  case DataSize of
    1..10: FOpCode := SmallDataSizeOpCodes[DataSize];
    11..255: FOpCode := ocMoveSome;
  else
    FOpCode := ocMoveMany;
  end;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  if DataSize <= SizeOf(Variant) then
  begin
    FSource := TSepiMemoryReference.Create(MethodCompiler, aoAcceptAllConsts,
      DataSize);
  end else
  begin
    FSource := TSepiMemoryReference.Create(MethodCompiler,
      [aoAcceptAddressedConst]);
  end;
end;

{*
  Crée une instruction MOVE typée
  @param AMethodCompiler   Compilateur de méthode
  @param ADataType          Type des données à copier
*}
constructor TSepiAsmMove.Create(AMethodCompiler: TSepiMethodCompiler;
  ADataType: TSepiType);
begin
  if (not ADataType.NeedInit) and
    (CardinalSize(ADataType.Size) <= SizeOf(Word)) then
  begin
    Create(AMethodCompiler, ADataType.Size);
    Exit;
  end;

  inherited Create(AMethodCompiler);

  FDataSize := ADataType.Size;
  FDataType := ADataType;

  case DataType.Kind of
    tkLString: FOpCode := ocMoveAnsiStr;
    tkWString: FOpCode := ocMoveWideStr;
    tkInterface: FOpCode := ocMoveIntf;
    tkVariant: FOpCode := ocMoveVariant;
  else
    FOpCode := ocMoveOther;
  end;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);

  if DataType.Kind in [tkLString, tkWString] then
  begin
    FSource := TSepiMemoryReference.Create(MethodCompiler,
      aoAcceptNonCodeConsts);
  end else if DataType.Kind in [tkInterface, tkVariant] then
    FSource := TSepiMemoryReference.Create(MethodCompiler, [aoAcceptZero])
  else
    FSource := TSepiMemoryReference.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmMove.Destroy;
begin
  FSource.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmMove.Make;
begin
  inherited;

  case OpCode of
    ocMoveSome: Inc(FSize, SizeOf(Byte));
    ocMoveMany: Inc(FSize, SizeOf(Word));
    ocMoveOther: Inc(FSize, SizeOf(Integer));
  end;

  Destination.Make;
  Source.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, Source.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmMove.WriteToStream(Stream: TStream);
var
  DataTypeRef: Integer;
begin
  inherited;

  case OpCode of
    ocMoveSome: Stream.WriteBuffer(FDataSize, SizeOf(Byte));
    ocMoveMany: Stream.WriteBuffer(FDataSize, SizeOf(Word));
    ocMoveOther:
    begin
      DataTypeRef := UnitCompiler.MakeReference(DataType);
      Stream.WriteBuffer(DataTypeRef, SizeOf(Integer));
    end;
  end;

  Destination.WriteToStream(Stream);
  Source.WriteToStream(Stream);
end;

{-----------------------}
{ TSepiAsmConvert class }
{-----------------------}

{*
  Crée une instruction CVRT
  @param AMethodCompiler   Compilateur de méthode
  @param AToType            Type de destination
  @param AFromType          Type de la source
*}
constructor TSepiAsmConvert.Create(AMethodCompiler: TSepiMethodCompiler;
  AToType, AFromType: TSepiBaseType);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocConvert;

  FToType := AToType;
  FFromType := AFromType;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FSource := TSepiMemoryReference.Create(MethodCompiler, aoAcceptAllConsts,
    BaseTypeConstSizes[FromType]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmConvert.Destroy;
begin
  FSource.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmConvert.Make;
begin
  inherited;

  Inc(FSize, 2*SizeOf(TSepiBaseType));

  Destination.Make;
  Source.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, Source.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmConvert.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FToType, SizeOf(TSepiBaseType));
  Stream.WriteBuffer(FFromType, SizeOf(TSepiBaseType));

  Destination.WriteToStream(Stream);
  Source.WriteToStream(Stream);
end;

{-------------------------}
{ TSepiAsmOperation class }
{-------------------------}

{*
  Crée une instruction opération
  @param AMethodCompiler   Compilateur de méthode
  @param AOpCode            OpCode de l'instruction (de type opération)
  @param AVarType           Type des variables
*}
constructor TSepiAsmOperation.Create(AMethodCompiler: TSepiMethodCompiler;
  AOpCode: TSepiOpCode; AVarType: TSepiBaseType);
begin
  if not (AOpCode in OperationsOpCodes) then
    RaiseInvalidOpCode;

  inherited Create(AMethodCompiler);

  FOpCode := AOpCode;

  FVarType := AVarType;
  FUseLeft := False;
  FUseRight := False;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);

  if OpCode in SelfUnaryOps then
  begin
    FLeft := FDestination;
    FRight := FDestination;
  end else if OpCode in SelfBinaryOps then
  begin
    FUseRight := True;

    FLeft := FDestination;

    if OpCode in [ocSelfShl, ocSelfShr] then
    begin
      FRight := TSepiMemoryReference.Create(MethodCompiler,
        aoAcceptAllConsts, 1);
    end else
    begin
      FRight := TSepiMemoryReference.Create(MethodCompiler, aoAcceptAllConsts,
        BaseTypeConstSizes[VarType]);
    end;
  end else if OpCode in OtherUnaryOps then
  begin
    FUseLeft := True;

    FLeft := TSepiMemoryReference.Create(MethodCompiler, aoAcceptAllConsts,
      BaseTypeConstSizes[VarType]);
    FRight := FLeft;
  end else if OpCode in OtherBinaryOps then
  begin
    FUseLeft := True;
    FUseRight := True;

    FLeft := TSepiMemoryReference.Create(MethodCompiler, aoAcceptAllConsts,
      BaseTypeConstSizes[VarType]);

    if OpCode in [ocOtherShl, ocOtherShr] then
    begin
      FRight := TSepiMemoryReference.Create(MethodCompiler,
        aoAcceptAllConsts, 1);
    end else
    begin
      FRight := TSepiMemoryReference.Create(MethodCompiler, aoAcceptAllConsts,
        BaseTypeConstSizes[VarType]);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmOperation.Destroy;
begin
  FDestination.Free;
  if FUseLeft then
    FLeft.Free;
  if FUseRight then
    FRight.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmOperation.Make;
begin
  inherited;

  Inc(FSize, SizeOf(TSepiBaseType));

  Destination.Make;
  Inc(FSize, Destination.Size);

  if UseLeft then
  begin
    Left.Make;
    Inc(FSize, Left.Size);
  end;

  if UseRight then
  begin
    Right.Make;
    Inc(FSize, Right.Size);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmOperation.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FVarType, SizeOf(TSepiBaseType));

  Destination.WriteToStream(Stream);
  if UseLeft then
    Left.WriteToStream(Stream);
  if UseRight then
    Right.WriteToStream(Stream);
end;

{-----------------------}
{ TSepiAsmCompare class }
{-----------------------}

{*
  Crée une instruction de comparaison
  @param AMethodCompiler   Compilateur de méthode
  @param AOpCode            OpCode de l'instruction (de type comparaison)
  @param AVarType           Type des variables
*}
constructor TSepiAsmCompare.Create(AMethodCompiler: TSepiMethodCompiler;
  AOpCode: TSepiOpCode; AVarType: TSepiBaseType);
begin
  if not (AOpCode in [ocCompEquals..ocCompGreaterEq]) then
    RaiseInvalidOpCode;

  inherited Create(AMethodCompiler);

  FOpCode := AOpCode;

  FVarType := AVarType;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FLeft := TSepiMemoryReference.Create(MethodCompiler, aoAcceptAllConsts,
    BaseTypeConstSizes[VarType]);
  FRight := TSepiMemoryReference.Create(MethodCompiler, aoAcceptAllConsts,
    BaseTypeConstSizes[VarType]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCompare.Destroy;
begin
  FDestination.Free;
  FLeft.Free;
  FRight.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCompare.Make;
begin
  inherited;

  Inc(FSize, SizeOf(TSepiBaseType));

  Destination.Make;
  Left.Make;
  Right.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, Left.Size);
  Inc(FSize, Right.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCompare.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FVarType, SizeOf(TSepiBaseType));

  Destination.WriteToStream(Stream);
  Left.WriteToStream(Stream);
  Right.WriteToStream(Stream);
end;

{---------------------------}
{ TSepiAsmGetRunInfo class }
{---------------------------}

{*
  Crée une instruction GTI, GDC ou GMC
  @param AMethodCompiler   Compilateur de méthode
  @param AOpCode            OpCode de l'instruction (GTI, GDC ou GMC)
*}
constructor TSepiAsmGetRunInfo.Create(AMethodCompiler: TSepiMethodCompiler;
  AOpCode: TSepiOpCode);
begin
  if not (AOpCode in [ocGetTypeInfo, ocGetDelphiClass, ocGetMethodCode]) then
    RaiseInvalidOpCode;

  inherited Create(AMethodCompiler);

  FOpCode := AOpCode;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FReference := 0;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmGetRunInfo.Destroy;
begin
  FDestination.Free;

  inherited;
end;

{*
  Assigne la référence
  @param Reference   Référence
*}
procedure TSepiAsmGetRunInfo.SetReference(Reference: TSepiComponent);
begin
  FReference := MethodCompiler.UnitCompiler.MakeReference(Reference);
end;

{*
  Assigne la référence
  @param RefName   Nom de la référence
*}
procedure TSepiAsmGetRunInfo.SetReference(const RefName: string);
begin
  SetReference(MethodCompiler.SepiMethod.LookFor(RefName));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmGetRunInfo.Make;
begin
  inherited;

  Destination.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmGetRunInfo.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  Stream.WriteBuffer(FReference, SizeOf(Integer));
end;

{-----------------------}
{ TSepiAsmIsClass class }
{-----------------------}

{*
  Crée une instruction IS
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmIsClass.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocIsClass;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FMemObject := TSepiMemoryReference.Create(MethodCompiler, [aoAcceptZero]);
  FMemClass := TSepiMemoryReference.Create(MethodCompiler, [aoZeroAsNil]);
  FClassRef := 0;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmIsClass.Destroy;
begin
  FDestination.Free;
  FMemObject.Free;
  FMemClass.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmIsClass.Make;
begin
  inherited;

  Destination.Make;
  MemObject.Make;
  MemClass.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, MemObject.Size);
  Inc(FSize, MemClass.Size);

  if MemClass.Space = msZero then
    Inc(FSize, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmIsClass.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  MemObject.WriteToStream(Stream);
  MemClass.WriteToStream(Stream);

  if MemClass.Space = msZero then
    Stream.WriteBuffer(FClassRef, SizeOf(Integer));
end;

{-----------------------}
{ TSepiAsmAsClass class }
{-----------------------}

{*
  Crée une instruction AS
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmAsClass.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocAsClass;

  FMemObject := TSepiMemoryReference.Create(MethodCompiler, [aoAcceptZero]);
  FMemClass := TSepiMemoryReference.Create(MethodCompiler, [aoZeroAsNil]);
  FClassRef := 0;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmAsClass.Destroy;
begin
  FMemObject.Free;
  FMemClass.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAsClass.Make;
begin
  inherited;

  MemObject.Make;
  MemClass.Make;

  Inc(FSize, MemObject.Size);
  Inc(FSize, MemClass.Size);

  if MemClass.Space = msZero then
    Inc(FSize, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAsClass.WriteToStream(Stream: TStream);
begin
  inherited;

  MemObject.WriteToStream(Stream);
  MemClass.WriteToStream(Stream);

  if MemClass.Space = msZero then
    Stream.WriteBuffer(FClassRef, SizeOf(Integer));
end;

{---------------------}
{ TSepiAsmRaise class }
{---------------------}

{*
  Crée une instruction RAISE
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmRaise.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocRaise;

  FExceptObject := TSepiMemoryReference.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmRaise.Destroy;
begin
  FExceptObject.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmRaise.Make;
begin
  inherited;

  ExceptObject.Make;

  Inc(FSize, ExceptObject.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmRaise.WriteToStream(Stream: TStream);
begin
  inherited;

  ExceptObject.WriteToStream(Stream);
end;

{-----------------------}
{ TSepiAsmReraise class }
{-----------------------}

{*
  Crée une instruction RERS
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmReraise.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocReraise;
end;

{-------------------------}
{ TSepiAsmTryExcept class }
{-------------------------}

{*
  Crée une instruction TRYE
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmTryExcept.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocTryExcept;

  FEndOfTry := TSepiJumpDest.Create(MethodCompiler);
  FEndOfExcept := TSepiJumpDest.Create(MethodCompiler);
  FExceptObject := TSepiMemoryReference.Create(MethodCompiler, [aoZeroAsNil]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmTryExcept.Destroy;
begin
  FExceptObject.Free;
  FEndOfExcept.Free;
  FEndOfTry.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryExcept.Make;
begin
  inherited;

  Inc(FSize, 2*SizeOf(Word));

  EndOfTry.Make;
  EndOfExcept.Make;

  ExceptObject.Make;
  Inc(FSize, ExceptObject.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryExcept.WriteToStream(Stream: TStream);
var
  ClauseSize: Integer;
begin
  inherited;

  ClauseSize := EndOfTry.InstructionRef.Position - EndPosition;
  Stream.WriteBuffer(ClauseSize, SizeOf(Word));

  ClauseSize :=
      EndOfExcept.InstructionRef.Position - EndOfTry.InstructionRef.Position;
  Stream.WriteBuffer(ClauseSize, SizeOf(Word));

  ExceptObject.WriteToStream(Stream);
end;

{--------------------------}
{ TSepiAsmTryFinally class }
{--------------------------}

{*
  Crée une instruction TRYF
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmTryFinally.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocTryFinally;

  FEndOfTry := TSepiJumpDest.Create(MethodCompiler);
  FEndOfFinally := TSepiJumpDest.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmTryFinally.Destroy;
begin
  FEndOfFinally.Free;
  FEndOfTry.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryFinally.Make;
begin
  inherited;

  Inc(FSize, 2*SizeOf(Word));

  EndOfTry.Make;
  EndOfFinally.Make;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryFinally.WriteToStream(Stream: TStream);
var
  ClauseSize: Integer;
begin
  inherited;

  ClauseSize := EndOfTry.InstructionRef.Position - EndPosition;
  Stream.WriteBuffer(ClauseSize, SizeOf(Word));

  ClauseSize :=
      EndOfFinally.InstructionRef.Position - EndOfTry.InstructionRef.Position;
  Stream.WriteBuffer(ClauseSize, SizeOf(Word));
end;

{-----------------------}
{ TSepiAsmMultiOn class }
{-----------------------}

{*
  Crée une instruction ON
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmMultiOn.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocMultiOn;

  FExceptObject := TSepiMemoryReference.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmMultiOn.Destroy;
begin
  FExceptObject.Free;

  inherited;
end;

{*
  Nombre de clauses ON
  @return Nombre de clauses ON
*}
function TSepiAsmMultiOn.GetOnClauseCount: Integer;
begin
  Result := Length(FOnClauses);
end;

{*
  Tableau zero-based des clauses ON
  @param Index   Index d'une clause
  @return Clause à l'index spécifié
*}
function TSepiAsmMultiOn.GetOnClauses(Index: Integer): TSepiAsmOnClause;
begin
  Result := FOnClauses[Index];
end;

{*
  Ajoute une clause ON
  @param AClassRef   Référence à la classe d'exception
  @return Destination du ON
*}
function TSepiAsmMultiOn.AddOnClause(AClassRef: Integer): TSepiJumpDest;
var
  Index: Integer;
begin
  Index := Length(FOnClauses);
  SetLength(FOnClauses, Index+1);

  with FOnClauses[Index] do
  begin
    ClassRef := AClassRef;
    Destination := TSepiJumpDest.Create(MethodCompiler);
    Result := Destination;
  end;
end;

{*
  Ajoute une clause ON
  @param SepiClass   Classe d'exception
  @return Destination du ON
*}
function TSepiAsmMultiOn.AddOnClause(SepiClass: TSepiClass): TSepiJumpDest;
begin
  Result := AddOnClause(
    MethodCompiler.UnitCompiler.MakeReference(SepiClass));
end;

{*
  Ajoute une clause ON
  @param ClassName   Nom de la classe d'exception
  @return Destination du ON
*}
function TSepiAsmMultiOn.AddOnClause(const ClassName: string): TSepiJumpDest;
var
  SepiClass: TSepiClass;
begin
  SepiClass := MethodCompiler.SepiMethod.LookFor(ClassName) as TSepiClass;

  if SepiClass = nil then
    raise ESepiComponentNotFoundError.CreateFmt(
      SSepiComponentNotFound, [ClassName]);

  Result := AddOnClause(SepiClass);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmMultiOn.Make;
var
  I: Integer;
begin
  inherited;

  ExceptObject.Make;

  for I := 0 to Length(FOnClauses)-1 do
    FOnClauses[I].Destination.Make;

  Inc(FSize, ExceptObject.Size + SizeOf(Byte));
  Inc(FSize, Length(FOnClauses) * (SizeOf(Integer)+SizeOf(Smallint)));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmMultiOn.WriteToStream(Stream: TStream);
var
  I, Count: Integer;
begin
  inherited;

  ExceptObject.WriteToStream(Stream);

  Count := Length(FOnClauses);
  Stream.WriteBuffer(Count, SizeOf(Byte));

  for I := 0 to Count-1 do
  begin
    with FOnClauses[I] do
    begin
      Stream.WriteBuffer(ClassRef, SizeOf(Integer));
      Destination.WriteToStream(Stream,
        EndPosition - (Count-I-1) * (SizeOf(Integer) + SizeOf(Smallint)));
    end;
  end;
end;

{---------------------------------}
{ TSepiAsmSetIncludeExclude class }
{---------------------------------}

{*
  Crée une instruction set-include ou set-exclude
  @param AMethodCompiler   Compilateur de méthode
  @param AInclude          True pour Include, False pour Exclude
*}
constructor TSepiAsmSetIncludeExclude.Create(
  AMethodCompiler: TSepiMethodCompiler; AInclude: Boolean);
begin
  inherited Create(AMethodCompiler);

  if AInclude then
    FOpCode := ocSetInclude
  else
    FOpCode := ocSetExclude;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FElement := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SizeOf(Byte));
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmSetIncludeExclude.Destroy;
begin
  FElement.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetIncludeExclude.Make;
begin
  inherited;

  Destination.Make;
  Inc(FSize, Destination.Size);

  Element.Make;
  Inc(FSize, Element.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetIncludeExclude.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  Element.WriteToStream(Stream);
end;

{---------------------}
{ TSepiAsmSetIn class }
{---------------------}

{*
  Crée une instruction set-in
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmSetIn.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocSetIn;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FSetValue := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptNonCodeConsts);
  FElement := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SizeOf(Byte));
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmSetIn.Destroy;
begin
  FElement.Free;
  FSetValue.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetIn.Make;
begin
  inherited;

  Destination.Make;
  Inc(FSize, Destination.Size);

  SetValue.Make;
  Inc(FSize, SetValue.Size);

  Element.Make;
  Inc(FSize, Element.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetIn.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  SetValue.WriteToStream(Stream);
  Element.WriteToStream(Stream);
end;

{-----------------------}
{ TSepiAsmSetElem class }
{-----------------------}

{*
  Crée une instruction set-elem
  @param AMethodCompiler   Compilateur de méthode
  @param ASetSize          Taille du set
  @param AUnion            True pour faire un Union-Elem (équivalent à Inc)
*}
constructor TSepiAsmSetElem.Create(AMethodCompiler: TSepiMethodCompiler;
  ASetSize: Byte; AUnion: Boolean = False);
begin
  inherited Create(AMethodCompiler);

  if AUnion then
    FOpCode := ocSetInclude
  else
    FOpCode := ocSetElem;

  FSetSize := ASetSize;
  FUnion := AUnion;
  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FElement := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SizeOf(Byte));
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmSetElem.Destroy;
begin
  FElement.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetElem.Make;
begin
  inherited;

  if not Union then
    Inc(FSize, SizeOf(Byte));

  Destination.Make;
  Inc(FSize, Destination.Size);

  Element.Make;
  Inc(FSize, Element.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetElem.WriteToStream(Stream: TStream);
begin
  inherited;

  if not Union then
    Stream.WriteBuffer(FSetSize, SizeOf(Byte));

  Destination.WriteToStream(Stream);
  Element.WriteToStream(Stream);
end;

{------------------------}
{ TSepiAsmSetRange class }
{------------------------}

{*
  Crée une instruction set-range
  @param AMethodCompiler   Compilateur de méthode
  @param ASetSize          Taille du set
  @param AUnion            True pour faire un Union-Range
*}
constructor TSepiAsmSetRange.Create(AMethodCompiler: TSepiMethodCompiler;
  ASetSize: Byte; AUnion: Boolean = False);
begin
  inherited Create(AMethodCompiler);

  if AUnion then
    FOpCode := ocSetUnionRange
  else
    FOpCode := ocSetRange;

  FSetSize := ASetSize;
  FUnion := AUnion;
  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FLowerBound := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SizeOf(Byte));
  FHigherBound := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SizeOf(Byte));
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmSetRange.Destroy;
begin
  FLowerBound.Free;
  FHigherBound.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetRange.Make;
begin
  inherited;

  Inc(FSize, SizeOf(Byte));

  Destination.Make;
  Inc(FSize, Destination.Size);

  LowerBound.Make;
  Inc(FSize, LowerBound.Size);
  HigherBound.Make;
  Inc(FSize, HigherBound.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetRange.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FSetSize, SizeOf(Byte));

  Destination.WriteToStream(Stream);
  LowerBound.WriteToStream(Stream);
  HigherBound.WriteToStream(Stream);
end;

{--------------------------}
{ TSepiAsmSetCompare class }
{--------------------------}

{*
  Crée une instruction de comparaison
  @param AMethodCompiler   Compilateur de méthode
  @param AOpCode           OpCode de l'instruction (de type comparaison de sets)
*}
constructor TSepiAsmSetCompare.Create(AMethodCompiler: TSepiMethodCompiler;
  AOpCode: TSepiOpCode; ASetSize: Byte);
begin
  if not (AOpCode in SetComparisonOpCodes) then
    RaiseInvalidOpCode;

  inherited Create(AMethodCompiler);

  FOpCode := AOpCode;

  FSetSize := ASetSize;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FLeft := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SetSize);
  FRight := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SetSize);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmSetCompare.Destroy;
begin
  FDestination.Free;
  FLeft.Free;
  FRight.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetCompare.Make;
begin
  inherited;

  Inc(FSize, SizeOf(Byte));

  Destination.Make;
  Left.Make;
  Right.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, Left.Size);
  Inc(FSize, Right.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetCompare.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FSetSize, SizeOf(Byte));

  Destination.WriteToStream(Stream);
  Left.WriteToStream(Stream);
  Right.WriteToStream(Stream);
end;

{----------------------------}
{ TSepiAsmSetOperation class }
{----------------------------}

{*
  Crée une instruction opération
  @param AMethodCompiler   Compilateur de méthode
  @param AOpCode           OpCode de l'instruction (de type opération)
  @param ASetSize          Taille des sets
*}
constructor TSepiAsmSetOperation.Create(AMethodCompiler: TSepiMethodCompiler;
  AOpCode: TSepiOpCode; ASetSize: Byte);
begin
  if not (AOpCode in SetOperationsOpCodes) then
    RaiseInvalidOpCode;

  inherited Create(AMethodCompiler);

  FOpCode := AOpCode;

  FSetSize := ASetSize;
  FUseLeft := OpCode in SetOtherOps;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);

  if UseLeft then
    FLeft := TSepiMemoryReference.Create(MethodCompiler,
      aoAcceptAllConsts, SetSize)
  else
    FLeft := FDestination;

  FRight := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SetSize);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmSetOperation.Destroy;
begin
  FDestination.Free;
  if FUseLeft then
    FLeft.Free;
  FRight.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetOperation.Make;
begin
  inherited;

  Inc(FSize, SizeOf(Byte));

  Destination.Make;
  Inc(FSize, Destination.Size);

  if UseLeft then
  begin
    Left.Make;
    Inc(FSize, Left.Size);
  end;

  Right.Make;
  Inc(FSize, Right.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetOperation.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FSetSize, SizeOf(Byte));

  Destination.WriteToStream(Stream);
  if UseLeft then
    Left.WriteToStream(Stream);
  Right.WriteToStream(Stream);
end;

{-------------------------}
{ TSepiAsmSetExpand class }
{-------------------------}

{*
  Crée une instruction set-expand
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmSetExpand.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocSetRange;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FSource := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptNonCodeConsts);
  FLowerByte := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SizeOf(Byte));
  FHigherByte := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SizeOf(Byte));
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmSetExpand.Destroy;
begin
  FLowerByte.Free;
  FHigherByte.Free;
  FSource.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetExpand.Make;
begin
  inherited;

  Destination.Make;
  Inc(FSize, Destination.Size);
  Source.Make;
  Inc(FSize, Source.Size);

  LowerBound.Make;
  Inc(FSize, LowerBound.Size);
  HigherBound.Make;
  Inc(FSize, HigherBound.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmSetExpand.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  Source.WriteToStream(Stream);
  LowerBound.WriteToStream(Stream);
  HigherBound.WriteToStream(Stream);
end;

{-------------------------------------}
{ TSepiAsmValueToIntStdFunction class }
{-------------------------------------}

{*
  Crée une instruction ASL, WSL, DAL ou DAH
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmValueToIntStdFunction.Create(
  AMethodCompiler: TSepiMethodCompiler; AOpCode: TSepiOpCode);
begin
  Assert(AOpCode in [ocAnsiStrLength..ocDynArrayHigh]);

  inherited Create(AMethodCompiler);

  FOpCode := AOpCode;

  FDestination := TSepiMemoryReference.Create(MethodCompiler);
  FValue := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptNonCodeConsts);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmValueToIntStdFunction.Destroy;
begin
  FValue.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmValueToIntStdFunction.Make;
begin
  inherited;

  Destination.Make;
  Inc(FSize, Destination.Size);
  Value.Make;
  Inc(FSize, Value.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmValueToIntStdFunction.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  Value.WriteToStream(Stream);
end;

{----------------------------}
{ TSepiAsmStrSetLength class }
{----------------------------}

{*
  Crée une instruction ASSL ou WSSL
  @param AMethodCompiler   Compilateur de méthode
  @param AOpCode           OpCode (in [ocAnsiStrSetLength..ocWideStrSetLength])
*}
constructor TSepiAsmStrSetLength.Create(
  AMethodCompiler: TSepiMethodCompiler; AOpCode: TSepiOpCode);
begin
  Assert(AOpCode in [ocAnsiStrSetLength..ocWideStrSetLength]);

  inherited Create(AMethodCompiler);

  FOpCode := AOpCode;

  FStrValue := TSepiMemoryReference.Create(MethodCompiler);
  FNewLength := TSepiMemoryReference.Create(MethodCompiler,
    aoAcceptAllConsts, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmStrSetLength.Destroy;
begin
  FNewLength.Free;
  FStrValue.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmStrSetLength.Make;
begin
  inherited;

  StrValue.Make;
  Inc(FSize, StrValue.Size);
  NewLength.Make;
  Inc(FSize, NewLength.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmStrSetLength.WriteToStream(Stream: TStream);
begin
  inherited;

  StrValue.WriteToStream(Stream);
  NewLength.WriteToStream(Stream);
end;

{---------------------------------}
{ TSepiAsmDynArraySetLength class }
{---------------------------------}

{*
  Crée une instruction DASL
  @param AMethodCompiler   Compilateur de méthode
  @param ADimCount         Nombre de dimensions (> 0)
*}
constructor TSepiAsmDynArraySetLength.Create(
  AMethodCompiler: TSepiMethodCompiler; ADynArrayType: TSepiDynArrayType;
  ADimCount: Integer = 1);
var
  I: Integer;
begin
  Assert(ADimCount > 0);

  inherited Create(AMethodCompiler);

  FOpCode := ocDynArraySetLength;

  FDynArrayType := ADynArrayType;
  FDynArrayValue := TSepiMemoryReference.Create(MethodCompiler);

  SetLength(FDimensions, ADimCount);
  FillChar(FDimensions[0], ADimCount*SizeOf(TSepiMemoryReference), 0);
  FDimCount := ADimCount;

  for I := 0 to DimCount-1 do
    FDimensions[I] := TSepiMemoryReference.Create(MethodCompiler,
      aoAcceptAllConsts, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmDynArraySetLength.Destroy;
var
  I: Integer;
begin
  for I := 0 to DimCount-1 do
    FDimensions[I].Free;

  FDynArrayValue.Free;

  inherited;
end;

{*
  Tableau zero-based des dimensions
  @param Index   Index compris entre 0 inclus et DimCount exclu
  @return Référence mémoire sur la valeur de la dimension d'index spécifié
*}
function TSepiAsmDynArraySetLength.GetDimensions(
  Index: Integer): TSepiMemoryReference;
begin
  Result := FDimensions[Index];
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmDynArraySetLength.Make;
var
  I: Integer;
begin
  inherited;

  Inc(FSize, SizeOf(Integer));
  DynArrayValue.Make;
  Inc(FSize, DynArrayValue.Size);
  Inc(FSize, SizeOf(Byte));

  for I := 0 to DimCount-1 do
  begin
    Dimensions[I].Make;
    Inc(FSize, Dimensions[I].Size);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmDynArraySetLength.WriteToStream(Stream: TStream);
var
  DynArrayTypeRef, I: Integer;
begin
  inherited;

  DynArrayTypeRef := UnitCompiler.MakeReference(DynArrayType);
  Stream.WriteBuffer(DynArrayTypeRef, SizeOf(Integer));

  DynArrayValue.WriteToStream(Stream);
  Stream.WriteBuffer(FDimCount, SizeOf(Byte));

  for I := 0 to DimCount-1 do
    Dimensions[I].WriteToStream(Stream);
end;

end.

