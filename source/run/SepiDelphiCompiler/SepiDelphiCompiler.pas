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
  Classes de compilation d'une unité Delphi dans Sepi
  @author sjrd
  @version 1.0
*}
unit SepiDelphiCompiler;

interface

uses
  Windows, Types, SysUtils, Classes, StrUtils, TypInfo, SysConst, ScUtils,
  ScStrUtils, ScDelphiLanguage, SepiReflectionCore, SepiMembers, SepiOrdTypes,
  SepiStrTypes, SepiArrayTypes, SepiDelphiLexer, SepiDelphiParser,
  SepiCompilerErrors, SepiParseTrees, SepiCompiler, SepiCore,
  SepiCompilerConsts, SepiExpressions, SepiDelphiCompilerConsts, SepiOpCodes,
  SepiDelphiLikeCompilerUtils, SepiLL1ParserUtils, SepiInstructions,
  SepiCompilerUtils, SepiStdCompilerNodes;

type
  {*
    Noeud racine
    @author sjrd
    @version 1.0
  *}
  TRootNode = class(TSepiParseTreeRootNode)
  private
    FMinEnumSize: TSepiMinEnumSize; /// Taille minimale d'énumération

    procedure CDMMinEnumSize(var Msg: TCDMMinEnumSize); message CDM_MINENUMSIZE;
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    constructor Create(AClass: TSepiSymbolClass; ASepiRoot: TSepiRoot;
      AErrors: TSepiCompilerErrorList); override;

    procedure EndParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property MinEnumSize: TSepiMinEnumSize read FMinEnumSize;
  end;

  {*
    Noeud interface
    @author sjrd
    @version 1.0
  *}
  TInterfaceNode = class(TSepiNonTerminal)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud implémentation
    @author sjrd
    @version 1.0
  *}
  TImplementationNode = class(TSepiNonTerminal)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud section uses
    @author sjrd
    @version 1.0
  *}
  TUsesSectionNode = class(TSepiUsesNode)
  end;

  {*
    Noeud d'expression d'initialisation
    @author sjrd
    @version 1.0
  *}
  TInitializationExpressionNode = class(TSepiInitializationExpressionNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;

    function IsValidType(AValueType: TSepiType): Boolean; override;
  end;

  {*
    Noeud d'expression d'initialisation d'un tableau
    @author sjrd
    @version 1.0
  *}
  TArrayInitializationNode = class(TSepiInitializationExpressionNode)
  private
    FArrayType: TSepiStaticArrayType; /// Type de valeur comme type tableau
    FElementType: TSepiType;          /// Type des éléments du tableau
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    function IsValidType(AValueType: TSepiType): Boolean; override;

    property ArrayType: TSepiStaticArrayType read FArrayType;
    property ElementType: TSepiType read FElementType;
  end;

  {*
    Noeud d'expression d'initialisation d'un record
    @author sjrd
    @version 1.0
  *}
  TRecordInitializationNode = class(TSepiInitializationExpressionNode)
  private
    FRecordType: TSepiRecordType; /// Type de valeur comme type record
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;

    function IsValidType(AValueType: TSepiType): Boolean; override;

    property RecordType: TSepiRecordType read FRecordType write FRecordType;
  end;

  {*
    Noeud d'expression d'initialisation d'un GUID
    @author sjrd
    @version 1.0
  *}
  TGUIDInitializationNode = class(TSepiInitializationExpressionNode)
  private
    FRecordType: TSepiRecordType; /// Type de valeur comme type record
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;

    function IsValidType(AValueType: TSepiType): Boolean; override;

    property RecordType: TSepiRecordType read FRecordType write FRecordType;
  end;

  {*
    Noeud d'expression d'initialisation d'un autre type que tableau ou record
    @author sjrd
    @version 1.0
  *}
  TOtherInitializationNode = class(TSepiInitializationExpressionNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure EndParsing; override;

    function IsValidType(AValueType: TSepiType): Boolean; override;
  end;

  {*
    Noeud expression constante
    @author sjrd
    @version 1.0
  *}
  TConstExpressionNode = class(TSepiSameAsChildExpressionNode)
  private
    FAcceptType: Boolean; /// Indique si peut accepter un identificateur de type

    FValueType: TSepiType; /// Type de valeur censée être contenue dans ce noeud
  protected
    function ValidateExpression: Boolean; override;
  public
    function CompileConst(var Value;
      ValueType: TSepiType): Boolean; virtual;

    property AcceptType: Boolean read FAcceptType write FAcceptType;

    property ValueType: TSepiType read FValueType write FValueType;
  end;

  {*
    Noeud expression constante ou type
    @author sjrd
    @version 1.0
  *}
  TConstOrTypeNode = class(TConstExpressionNode)
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); override;
  end;

  {*
    Noeud opérateur unaire
    @author sjrd
    @version 1.0
  *}
  TUnaryOpNode = class(TSepiUnaryOpNode)
  public
    function MakeOperation(
      const Operand: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud opérateur binaire
    @author sjrd
    @version 1.0
  *}
  TBinaryOpNode = class(TSepiBinaryOpNode)
  private
    function HandlePointerArithm(
      var LeftValue, RightValue: ISepiReadableValue): TSepiType;
  protected
    function GetPriority: Integer; override;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud expression simple (sans opérateurs binaires)
    @author sjrd
    @version 1.0
  *}
  TSingleExprNode = class(TSepiExpressionWithModifiersNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud valeur ensemble
    @author sjrd
    @version 1.0
  *}
  TSetValueNode = class(TSepiExpressionNode)
  private
    FSetBuilder: ISepiSetBuilder; /// Constructeur d'ensemble
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;
  end;

  {*
    Classe de base pour les noeuds qui suivent une expression single
    @author sjrd
    @version 1.0
  *}
  TNextExprNode = class(TSepiExpressionModifierNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud paramètres réels
    @author sjrd
    @version 1.0
  *}
  TParametersNode = class(TNextExprNode)
  private
    procedure CompileIdentifierTest(
      const PseudoRoutine: ISepiIdentifierTestPseudoRoutine);
    procedure CompileCastOrConvert(DestType: TSepiType);
    procedure CompileCast(const PseudoRoutine: ISepiCastPseudoRoutine);
    procedure CompileTypeOperation(
      const TypeOperation: ISepiTypeOperationPseudoRoutine;
      const TypeExpression: ISepiTypeExpression);
    procedure CompileCall(const Callable: ISepiCallable);
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;
  end;

  {*
    Noeud déclaration de type
    @author sjrd
    @version 1.0
  *}
  TTypeDeclNode = class(TSepiNonTerminal)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud déclaration de constante
    @author sjrd
    @version 1.0
  *}
  TConstantDeclNode = class(TSepiNonTerminal)
  private
    FName: string;            /// Nom de la constante
    FConstType: TSepiType;    /// Type de la constante
    FConstant: TSepiConstant; /// Constante Sepi (peut être nil)
    FConstVar: TSepiVariable; /// Variable constante Sepi (peut être nil)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    property Name: string read FName;
    property ConstType: TSepiType read FConstType;
    property Constant: TSepiConstant read FConstant;
    property ConstVar: TSepiVariable read FConstVar;
  end;

  {*
    Noeud déclaration de variable
    @author sjrd
    @version 1.0
  *}
  TVariableDeclNode = class(TSepiNonTerminal)
  private
    FNames: TStrings;         /// Nom de la variable
    FVarType: TSepiType;      /// Type de la variable
    FVariable: TSepiVariable; /// Variable Sepi
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    property Names: TStrings read FNames;
    property VarType: TSepiType read FVarType;
    property Variable: TSepiVariable read FVariable;
  end;

  {*
    Noeud type de routine
    @author sjrd
    @version 1.0
  *}
  TRoutineKindNode = class(TSepiSignatureKindNode)
  protected
    function GetKind: TSepiSignatureKind; override;
  end;

  {*
    Noeud descripteur d'un clone de type
    @author sjrd
    @version 1.0
  *}
  TTypeCloneNode = class(TSepiTypeDefinitionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type intervalle ou énumération
  *}
  TRangeOrEnumTypeNode = class(TSepiTypeDefinitionNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud descripteur d'un type intervalle
    @author sjrd
    @version 1.0
  *}
  TRangeTypeNode = class(TSepiTypeDefinitionNode)
  private
    function MakeType(const TypeName: string; BaseType: TSepiType;
      const LowerValue, HigherValue): TSepiType;
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type énumération
    @author sjrd
    @version 1.0
  *}
  TEnumTypeNode = class(TSepiTypeDefinitionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type ensemble
    @author sjrd
    @version 1.0
  *}
  TSetTypeNode = class(TSepiTypeDefinitionNode)
  protected
    procedure MakeErroneousType; override;

    function IsValidCompType(CompType: TSepiType): Boolean;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type chaîne courte
    @author sjrd
    @version 1.0
  *}
  TStringTypeNode = class(TSepiTypeDefinitionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type pointeur
    @author sjrd
    @version 1.0
  *}
  TPointerTypeNode = class(TSepiTypeDefinitionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type tableau
    @author sjrd
    @version 1.0
  *}
  TArrayTypeNode = class(TSepiTypeDefinitionNode)
  private
    function RangeDefinition(const TypeName: string;
      RangeNode: TSepiParseTreeNode; ElementType: TSepiType): TSepiStaticArrayType;
    function TypedDefinition(const TypeName: string;
      IndexNode: TConstExpressionNode; ElementType: TSepiType): TSepiStaticArrayType;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type tableau packed
    @author sjrd
    @version 1.0
  *}
  TPackedArrayTypeNode = class(TArrayTypeNode)
  end;

  {*
    Noeud descripteur d'un type record
    @author sjrd
    @version 1.0
  *}
  TRecordTypeNode = class(TSepiTypeDefinitionNode)
  private
    FIsPacked: Boolean; /// Indique si le record doit être packed

    FRecordType: TSepiRecordType; /// Type record
  protected
    function GetSepiContext: TSepiMeta; override;

    property IsPacked: Boolean read FIsPacked write FIsPacked;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property RecordType: TSepiRecordType read FRecordType;
  end;

  {*
    Noeud descripteur d'un type packed record
    @author sjrd
    @version 1.0
  *}
  TPackedRecordTypeNode = class(TRecordTypeNode)
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); override;
  end;

  {*
    Noeud descripteur d'un type classe
    @author sjrd
    @version 1.0
  *}
  TClassTypeNode = class(TSepiTypeDefinitionNode)
  private
    FIsForwardClass: Boolean; /// True si c'est une classe forwardée
    FIsClass: Boolean;        /// True si c'est une classe
    FIsMetaClass: Boolean;    /// True si c'est une meta-classe

    FSepiClass: TSepiClass;         /// Classe Sepi compilée
    FSepiMetaClass: TSepiMetaClass; /// Meta-classe compilée

    procedure CreateClass(ParentClass: TSepiClass = nil);
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    function GetSepiContext: TSepiMeta; override;
  public
    procedure EndParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property IsForwardClass: Boolean read FIsForwardClass;
    property IsClass: Boolean read FIsClass;
    property IsMetaClass: Boolean read FIsMetaClass;

    property SepiClass: TSepiClass read FSepiClass;
    property SepiMetaClass: TSepiMetaClass read FSepiMetaClass;
  end;

  {*
    Noeud descripteur d'un type interface
    @author sjrd
    @version 1.0
  *}
  TInterfaceTypeNode = class(TSepiTypeDefinitionNode)
  private
    FIsDispIntf: Boolean;        /// True si c'est une dispinterface
    FParentIntf: TSepiInterface; /// Interface parent
    FGUID: TGUID;                /// GUID de l'interface
    FSepiIntf: TSepiInterface;   /// Interface Sepi compilée

    procedure CreateInterface;
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    function GetSepiContext: TSepiMeta; override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property IsDispIntf: Boolean read FIsDispIntf;
    property ParentIntf: TSepiInterface read FParentIntf;
    property GUID: TGUID read FGUID;
    property SepiIntf: TSepiInterface read FSepiIntf;
  end;

  {*
    Noeud GUID d'interface
    @author sjrd
    @version 1.0
  *}
  TInterfaceGUIDNode = class(TSepiInitializationExpressionNode)
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure SetValuePtr(AValuePtr: PGUID);
  end;

  {*
    Noeud descripteur d'un type référence de méthode
    @author sjrd
    @version 1.0
  *}
  TMethodRefTypeNode = class(TSepiTypeDefinitionNode)
  private
    FSignature: TSepiSignature; /// Signature
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Signature: TSepiSignature read FSignature;
  end;

  {*
    Noeud liste de membres
    @author sjrd
    @version 1.0
  *}
  TMemberListNode = class(TSepiNonTerminal)
  private
    FOwner: TSepiType;
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    property Owner: TSepiType read FOwner write FOwner;
  end;

  {*
    Noeud d'un membre d'un type composite
    @author sjrd
    @version 1.0
  *}
  TMemberNode = class(TSepiNonTerminal)
  private
    FOwner: TSepiType; /// Type propriétaire
  public
    property Owner: TSepiType read FOwner write FOwner;
  end;

  {*
    Noeud d'un champ
    @author sjrd
    @version 1.0
  *}
  TFieldNode = class(TMemberNode)
  private
    FFieldType: TSepiType;  /// Type des champs
    FLastField: TSepiField; /// Dernier champ compilé
  public
    procedure EndParsing; override;

    property FieldType: TSepiType read FFieldType;
    property LastField: TSepiField read FLastField;
  end;

  {*
    Noeud d'une méthode
    @author sjrd
    @version 1.0
  *}
  TMethodNode = class(TMemberNode)
  private
    FName: string;              /// Nom de la routine
    FSignature: TSepiSignature; /// Signature
    FLinkKind: TMethodLinkKind; /// Type de liaison
    FAbstract: Boolean;         /// Indique si la méthode est abstraite
    FMsgID: Integer;            /// Message intercepté
    FIsOverloaded: Boolean;     /// True si surchargée

    FIsIntfMethodRedirector: Boolean; /// True si redirecteur de méthode

    procedure HandleIntfMethodRedirector(
      IntfNode, IntfMethodNode, RedirectorNode: TSepiParseTreeNode);
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Name: string read FName;
    property Signature: TSepiSignature read FSignature;
    property LinkKind: TMethodLinkKind read FLinkKind;
    property IsAbstract: Boolean read FAbstract;
    property MsgID: Integer read FMsgID;
    property IsOverloaded: Boolean read FIsOverloaded;
    property IsIntfMethodRedirector: Boolean read FIsIntfMethodRedirector;
  end;

  {*
    Noeud d'une propriété
    @author sjrd
    @version 1.0
  *}
  TPropertyNode = class(TMemberNode)
  private
    FName: string; /// Nom de la routine

    FRedefine: Boolean;         /// True si c'est une redéfinition
    FPrevious: TSepiProperty;   /// Déclaration précédente à redéfinir
    FSignature: TSepiSignature; /// Signature

    FReadAccess: TSepiMeta;            /// Accesseur en lecture
    FWriteAccess: TSepiMeta;           /// Accesseur en écriture
    FIndex: ISepiReadableValue;        /// Index
    FDefaultValue: ISepiReadableValue; /// Valeur par défaut
    FNoDefault: Boolean;               /// True s'il y a eu un nodefault
    FStorage: TSepiPropertyStorage;    /// Spécificateur de stockage
    FIsDefault: Boolean;               /// True si c'est la propriété par défaut

    function CheckFieldAccess(Node: TSepiParseTreeNode;
      Field: TSepiField): Boolean;
    function CheckMethodAccess(Node: TSepiParseTreeNode; Method: TSepiMethod;
      IsWriteAccess: Boolean): Boolean;

    procedure HandleAccess(Node: TSepiParseTreeNode; IsWriteAccess: Boolean);
    procedure HandleIndex(Node: TSepiParseTreeNode);
    procedure HandleDefaultValue(Node: TSepiParseTreeNode);
    procedure HandleStorage(Node: TSepiParseTreeNode);
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Name: string read FName;

    property Redefine: Boolean read FRedefine;
    property Previous: TSepiProperty read FPrevious;
    property Signature: TSepiSignature read FSignature;

    property ReadAccess: TSepiMeta read FReadAccess;
    property WriteAccess: TSepiMeta read FWriteAccess;
    property Index: ISepiReadableValue read FIndex;
    property DefaultValue: ISepiReadableValue read FDefaultValue;
    property NoDefault: Boolean read FNoDefault;
    property Storage: TSepiPropertyStorage read FStorage;
    property IsDefault: Boolean read FIsDefault;
  end;

  {*
    Noeud d'une visibilité
    @author sjrd
    @version 1.0
  *}
  TVisibilityNode = class(TMemberNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud paramètre
    @author sjrd
    @version 1.0
  *}
  TParamNode = class(TSepiSignatureBuilderNode)
  private
    FKind: TSepiParamKind;   /// Type de paramètre
    FNames: TStringDynArray; /// Noms des paramètres
    FOpenArray: Boolean;     /// True si c'est un tableau ouvert
    FType: TSepiType;        /// Type du paramètre
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure EndParsing; override;

    property Kind: TSepiParamKind read FKind;
    property Names: TStringDynArray read FNames;
    property OpenArray: Boolean read FOpenArray;
    property ParamType: TSepiType read FType;
  end;

  {*
    Noeud type de retour ou type de la propriété
    @author sjrd
    @version 1.0
  *}
  TReturnTypeNode = class(TSepiSignatureBuilderNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud d'une information de propriété
    @author sjrd
    @version 1.0
  *}
  TPropInfoNode = class(TSepiNonTerminal)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud méthode dans l'implémentation
    @author sjrd
    @version 1.0
  *}
  TMethodImplNode = class(TSepiNonTerminal)
  private
    FName: string;              /// Nom de la routine
    FSignature: TSepiSignature; /// Signature
    FIsOverloaded: Boolean;     /// True si surchargée

    FOverloaded: TSepiOverloadedMethod; /// Méthode surchargée correspondante
    FSepiMethod: TSepiMethod;           /// Méthode correspondante
    FJustDeclared: Boolean;             /// Indique si déclaré dans ce noeud

    procedure DeclareMethod;
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    procedure BeginParsing; override;

    property Name: string read FName;
    property Signature: TSepiSignature read FSignature;
    property IsOverloaded: Boolean read FIsOverloaded;

    property Overloaded: TSepiOverloadedMethod read FOverloaded;
    property SepiMethod: TSepiMethod read FSepiMethod;
  end;

  {*
    Noeud signature dans l'implémentation
    @author sjrd
    @version 1.0
  *}
  TMethodImplDeclNode = class(TSepiNonTerminal)
  private
    FName: string;              /// Nom de la routine
    FSignature: TSepiSignature; /// Signature
    FIsOverloaded: Boolean;     /// True si surchargée
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    property Name: string read FName;
    property Signature: TSepiSignature read FSignature write FSignature;
    property IsOverloaded: Boolean read FIsOverloaded;
  end;

  {*
    Noeud corps de méthode
    @author sjrd
    @version 1.0
  *}
  TMethodBodyNode = class(TSepiNonTerminal)
  private
    FSepiMethod: TSepiMethod;       /// Méthode Sepi implémentée dans ce corps
    FCompiler: TSepiMethodCompiler; /// Compilateur de la méthode
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;

    function GetMethodCompiler: TSepiMethodCompiler; override;
    function GetSepiContext: TSepiMeta; override;
  public
    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    procedure BeginParsing; override;
    procedure EndParsing; override;

    property SepiMethod: TSepiMethod read FSepiMethod write FSepiMethod;
  end;

  {*
    Noeud déclaration de variable locale
    @author sjrd
    @version 1.0
  *}
  TLocalVarNode = class(TSepiNonTerminal)
  private
    FNames: TStrings;
    FVarType: TSepiType;
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    property Names: TStrings read FNames;
    property VarType: TSepiType read FVarType;
  end;

  {*
    Classe de base pour les noeuds instruction
    @author sjrd
    @version 1.0
  *}
  TInstructionNode = class(TSepiNonTerminal)
  private
    FInstructionList: TSepiInstructionList; /// Liste d'instructions contenante
  public
    property InstructionList: TSepiInstructionList
      read FInstructionList write FInstructionList;
  end;

  {*
    Noeud "pas d'instruction"
    @author sjrd
    @version 1.0
  *}
  TNoInstructionNode = class(TInstructionNode)
  end;

  {*
    Liste d'instructions
    @author sjrd
    @version 1.0
  *}
  TInstructionListNode = class(TInstructionNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Bloc begin..end
    @author sjrd
    @version 1.0
  *}
  TBeginEndBlockNode = class(TInstructionListNode)
  end;

  {*
    Instruction if..then..else
    @author sjrd
    @version 1.0
  *}
  TIfThenElseInstructionNode = class(TInstructionNode)
  private
    FInstruction: TSepiIfThenElse; /// Instruction
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Instruction: TSepiIfThenElse read FInstruction;
  end;

  {*
    Instruction while..do ou repeat..until
    @author sjrd
    @version 1.0
  *}
  TWhileInstructionNode = class(TInstructionNode)
  private
    FIsRepeat: Boolean;       /// True si c'est un repeat..until
    FInstruction: TSepiWhile; /// Instruction
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); override;

    procedure BeginParsing; override;
    procedure EndParsing; override;

    property IsRepeat: Boolean read FIsRepeat;
    property Instruction: TSepiWhile read FInstruction;
  end;

  {*
    Instruction for..do
    @author sjrd
    @version 1.0
  *}
  TForInstructionNode = class(TInstructionNode)
  private
    FInstruction: TSepiFor; /// Instruction
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Instruction: TSepiFor read FInstruction;
  end;

  {*
    Instruction try..except..end ou try..finally..end
    @author sjrd
    @version 1.0
  *}
  TTryInstructionNode = class(TInstructionNode)
  private
    FTryInstructions: TSepiInstructionList; /// Instructions dans le try
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;

    property TryInstructions: TSepiInstructionList read FTryInstructions;
  end;

  {*
    Suite d'une instruction try..except..end ou try..finally..end
    @author sjrd
    @version 1.0
  *}
  TNextTryInstructionNode = class(TInstructionNode)
  private
    FTryInstructions: TSepiInstructionList; /// Instructions dans le try
  public
    property TryInstructions: TSepiInstructionList
      read FTryInstructions write FTryInstructions;
  end;

  {*
    Clause except d'un try..except.end
    @author sjrd
    @version 1.0
  *}
  TExceptClauseNode = class(TNextTryInstructionNode)
  private
    FInstruction: TSepiTryExcept; /// Instruction
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Instruction: TSepiTryExcept read FInstruction;
  end;

  {*
    Instruction multi-on (valide uniquement dans un except)
    @author sjrd
    @version 1.0
  *}
  TMultiOnNode = class(TInstructionNode)
  private
    FInstruction: TSepiMultiOn; /// Instruction
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Instruction: TSepiMultiOn read FInstruction;
  end;

  {*
    Clause on d'un multi-on
    @author sjrd
    @version 1.0
  *}
  TOnClauseNode = class(TInstructionNode)
  private
    FInstruction: TSepiMultiOn; /// Instruction multi-on contenante

    FExceptObjectVarName: string;   /// Nom de la variable objet exception
    FExceptObjectClass: TSepiClass; /// Classe d'exception capturée
    FExceptObjectVar: TSepiLocalVar; /// Variable objet exception du bon type

    procedure ParseExceptObjectClass(Node: TSepiParseTreeNode);
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property Instruction: TSepiMultiOn read FInstruction;

    property ExceptObjectVarName: string read FExceptObjectVarName;
    property ExceptObjectClass: TSepiClass read FExceptObjectClass;
    property ExceptObjectVar: TSepiLocalVar read FExceptObjectVar;
  end;

  {*
    Clause else d'un multi-on
    @author sjrd
    @version 1.0
  *}
  TMultiOnElseClauseNode = class(TInstructionListNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Clause finally d'un try..except.end
    @author sjrd
    @version 1.0
  *}
  TFinallyClauseNode = class(TNextTryInstructionNode)
  private
    FInstruction: TSepiTryFinally; /// Instruction
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Instruction: TSepiTryFinally read FInstruction;
  end;

  {*
    Instruction raise
    @author sjrd
    @version 1.0
  *}
  TRaiseInstructionNode = class(TInstructionNode)
  private
    procedure MakeRaiseInstruction;
    procedure MakeReraiseInstruction;
  public
    procedure EndParsing; override;
  end;

  {*
    Instruction Expression ou Expression := Expression
    @author sjrd
    @version 1.0
  *}
  TExpressionInstructionNode = class(TInstructionNode)
  private
    procedure CompileCall;
    procedure CompileAssignment;
  public
    procedure EndParsing; override;
  end;

function CompileDelphiSource(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList; SourceFile: TStrings;
  const DestFileName: TFileName): TSepiUnit;

implementation

{-----------------}
{ Global routines }
{-----------------}

{*
  Compile un fichier source Delphi
  @param SepiRoot       Racine Sepi
  @param Errors         Gestionnaire d'erreurs
  @param SourceFile     Source à compiler
  @param DestFileName   Nom du fichier de sortie
  @return Unité Sepi compilée (interface seulement, pas d'implémentation)
*}
function CompileDelphiSource(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList; SourceFile: TStrings;
  const DestFileName: TFileName): TSepiUnit;
begin
  Result := SepiCompilerUtils.CompileSepiSource(SepiRoot, Errors, SourceFile,
    DestFileName, TRootNode, ntSource, TSepiDelphiLexer, TSepiDelphiParser);
end;

{*
  Initialise le tableau NonTerminalClasses
*}
procedure InitNonTerminalClasses;
begin
  NonTerminalClasses[ntSource]         := TRootNode;
  NonTerminalClasses[ntInterface]      := TInterfaceNode;
  NonTerminalClasses[ntImplementation] := TImplementationNode;
  NonTerminalClasses[ntUsesSection]    := TUsesSectionNode;

  NonTerminalClasses[ntCommaIdentDeclList] := TSepiIdentifierDeclListNode;
  NonTerminalClasses[ntQualifiedIdent]     := TSepiQualifiedIdentNode;
  NonTerminalClasses[ntIdentifierDecl]     := TSepiIdentifierDeclarationNode;

  NonTerminalClasses[ntTypeDecl]    := TTypeDeclNode;
  NonTerminalClasses[ntConstDecl]   := TConstantDeclNode;
  NonTerminalClasses[ntGlobalVar]   := TVariableDeclNode;
  NonTerminalClasses[ntRoutineDecl] := TSepiMethodDeclarationNode;

  NonTerminalClasses[ntInitializationExpression] :=
    TInitializationExpressionNode;
  NonTerminalClasses[ntArrayInitialization]  := TArrayInitializationNode;
  NonTerminalClasses[ntRecordInitialization] := TRecordInitializationNode;
  NonTerminalClasses[ntGUIDInitialization]   := TGUIDInitializationNode;
  NonTerminalClasses[ntOtherInitialization]  := TOtherInitializationNode;

  NonTerminalClasses[ntExpression]              := TSepiBinaryOpTreeNode;
  NonTerminalClasses[ntExpressionNoEquals]      := TSepiBinaryOpTreeNode;
  NonTerminalClasses[ntConstExpression]         := TConstExpressionNode;
  NonTerminalClasses[ntConstExpressionNoEquals] := TConstExpressionNode;
  NonTerminalClasses[ntConstOrType]             := TConstOrTypeNode;
  NonTerminalClasses[ntConstOrTypeNoEquals]     := TConstOrTypeNode;

  NonTerminalClasses[ntBinaryOp]         := TBinaryOpNode;
  NonTerminalClasses[ntBinaryOpNoEquals] := TBinaryOpNode;
  NonTerminalClasses[ntUnaryOp]          := TUnaryOpNode;

  NonTerminalClasses[ntSingleExpr]        := TSingleExprNode;
  NonTerminalClasses[ntParenthesizedExpr] := TSepiSameAsChildExpressionNode;
  NonTerminalClasses[ntUnaryOpExpr]       := TSepiUnaryOperationNode;

  NonTerminalClasses[ntSingleValue]           := TSepiSameAsChildExpressionNode;
  NonTerminalClasses[ntIntegerConst]          := TSepiConstIntegerNode;
  NonTerminalClasses[ntFloatConst]            := TSepiConstFloatNode;
  NonTerminalClasses[ntStringConst]           := TSepiConstStringNode;
  NonTerminalClasses[ntIdentifierSingleValue] := TSepiIdentifierExpressionNode;
  NonTerminalClasses[ntInheritedSingleValue]  := TSepiInheritedExpressionNode;
  NonTerminalClasses[ntNilValue]              := TSepiNilValueNode;
  NonTerminalClasses[ntSetValue]              := TSetValueNode;

  NonTerminalClasses[ntParameters]        := TParametersNode;
  NonTerminalClasses[ntArrayIndices]      := TSepiArrayIndicesModifierNode;
  NonTerminalClasses[ntFieldSelection]    := TSepiFieldSelectionModifierNode;
  NonTerminalClasses[ntDereference]       := TSepiDereferenceModifierNode;

  NonTerminalClasses[ntCloneDesc]         := TTypeCloneNode;
  NonTerminalClasses[ntRangeOrEnumDesc]   := TRangeOrEnumTypeNode;
  NonTerminalClasses[ntRangeDesc]         := TRangeTypeNode;
  NonTerminalClasses[ntEnumDesc]          := TEnumTypeNode;
  NonTerminalClasses[ntSetDesc]           := TSetTypeNode;
  NonTerminalClasses[ntStringDesc]        := TStringTypeNode;
  NonTerminalClasses[ntPointerDesc]       := TPointerTypeNode;
  NonTerminalClasses[ntArrayDesc]         := TArrayTypeNode;
  NonTerminalClasses[ntPackedArrayDesc]   := TPackedArrayTypeNode;
  NonTerminalClasses[ntRecordDesc]        := TRecordTypeNode;
  NonTerminalClasses[ntPackedRecordDesc]  := TPackedRecordTypeNode;
  NonTerminalClasses[ntClassDesc]         := TClassTypeNode;
  NonTerminalClasses[ntInterfaceDesc]     := TInterfaceTypeNode;
  NonTerminalClasses[ntDispInterfaceDesc] := TInterfaceTypeNode;
  NonTerminalClasses[ntEventDesc]         := TMethodRefTypeNode;

  NonTerminalClasses[ntRecordContents]     := TSepiRecordContentsNode;
  NonTerminalClasses[ntRecordCaseBlock]    := TSepiRecordContentsNode;
  NonTerminalClasses[ntRecordCaseContents] := TSepiRecordContentsNode;

  NonTerminalClasses[ntClassMemberLists]    := TMemberListNode;
  NonTerminalClasses[ntInterfaceGUID]       := TInterfaceGUIDNode;
  NonTerminalClasses[ntInterfaceMemberList] := TMemberListNode;

  NonTerminalClasses[ntRoutineKind]           := TRoutineKindNode;
  NonTerminalClasses[ntMethodNameDeclaration] :=
    TSepiUncheckedIdentifierDeclNode;
  NonTerminalClasses[ntMethodSignatureBeta]   := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntCallingConventionBeta] := TSepiCallingConventionNode;
  NonTerminalClasses[ntOverloadMarker]        := TSepiOverloadMarkerNode;

  NonTerminalClasses[ntRecordField]     := TSepiRecordFieldNode;
  NonTerminalClasses[ntRecordCaseField] := TSepiRecordFieldNode;
  NonTerminalClasses[ntField]           := TFieldNode;
  NonTerminalClasses[ntMethodDecl]      := TMethodNode;
  NonTerminalClasses[ntPropertyDecl]    := TPropertyNode;
  NonTerminalClasses[ntVisibility]      := TVisibilityNode;

  NonTerminalClasses[ntMethodSignature]   := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntPropertySignature] := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntParam]             := TParamNode;
  NonTerminalClasses[ntReturnType]        := TReturnTypeNode;
  NonTerminalClasses[ntPropType]          := TReturnTypeNode;
  NonTerminalClasses[ntPropInfo]          := TPropInfoNode;

  NonTerminalClasses[ntMethodImpl]     := TMethodImplNode;
  NonTerminalClasses[ntMethodImplDecl] := TMethodImplDeclNode;
  NonTerminalClasses[ntMethodBody]     := TMethodBodyNode;
  NonTerminalClasses[ntLocalVar]       := TLocalVarNode;

  NonTerminalClasses[ntInstructionList]       := TInstructionListNode;
  NonTerminalClasses[ntNoInstruction]         := TNoInstructionNode;
  NonTerminalClasses[ntBeginEndBlock]         := TBeginEndBlockNode;
  NonTerminalClasses[ntIfThenElseInstruction] := TIfThenElseInstructionNode;
  NonTerminalClasses[ntWhileInstruction]      := TWhileInstructionNode;
  NonTerminalClasses[ntRepeatInstruction]     := TWhileInstructionNode;
  NonTerminalClasses[ntForInstruction]        := TForInstructionNode;
  NonTerminalClasses[ntTryInstruction]        := TTryInstructionNode;
  NonTerminalClasses[ntExceptClause]          := TExceptClauseNode;
  NonTerminalClasses[ntMultiOn]               := TMultiOnNode;
  NonTerminalClasses[ntOnClause]              := TOnClauseNode;
  NonTerminalClasses[ntMultiOnElseClause]     := TMultiOnElseClauseNode;
  NonTerminalClasses[ntFinallyClause]         := TFinallyClauseNode;
  NonTerminalClasses[ntRaiseInstruction]      := TRaiseInstructionNode;
  NonTerminalClasses[ntExpressionInstruction] := TExpressionInstructionNode;
end;

{-----------------}
{ TRootNode class }
{-----------------}

{*
  [@inheritDoc]
*}
constructor TRootNode.Create(AClass: TSepiSymbolClass;
  ASepiRoot: TSepiRoot; AErrors: TSepiCompilerErrorList);
begin
  inherited;

  FMinEnumSize := mesByte;
end;

{*
  Gestionnaire de message CDM_MINENUMSIZE
  @param Msg   Message
*}
procedure TRootNode.CDMMinEnumSize(var Msg: TCDMMinEnumSize);
begin
  FMinEnumSize := Msg.MinEmumSize;
end;

{*
  [@inheritDoc]
*}
procedure TRootNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child.SymbolClass = ntIdentifier then
    SetSepiUnit(TSepiUnit.Create(SepiRoot, Child.AsText, []));

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TRootNode.EndParsing;
begin
  SepiUnit.Complete;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TRootNode.ResolveIdent(const Identifier: string): ISepiExpression;
begin
  Result := UnitResolveIdent(UnitCompiler, Identifier);
end;

{----------------------}
{ TInterfaceNode class }
{----------------------}

{*
  [@inheritDoc]
*}
procedure TInterfaceNode.BeginParsing;
begin
  SepiUnit.CurrentVisibility := mvPublic;
end;

{---------------------------}
{ TImplementationNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TImplementationNode.BeginParsing;
begin
  SepiUnit.CurrentVisibility := mvPrivate;
end;

{-------------------------------------}
{ TInitializationExpressionNode class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
procedure TInitializationExpressionNode.BeginParsing;
begin
  inherited;

  if ValueType is TSepiStaticArrayType then
    SetSymbolClass(ntArrayInitializationExpression)
  else if ValueType is TSepiRecordType then
  begin
    if ValueType = SystemUnit.TGUID then
      SetSymbolClass(ntGUIDInitializationExpression)
    else
      SetSymbolClass(ntRecordInitializationExpression);
  end else
    SetSymbolClass(ntOtherInitializationExpression);
end;

{*
  [@inheritDoc]
*}
procedure TInitializationExpressionNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
var
  InitChild: TSepiInitializationExpressionNode;
begin
  inherited;

  InitChild := Child as TSepiInitializationExpressionNode;
  InitChild.SetValueTypeAndPtr(ValueType, ValuePtr);
end;

{*
  [@inheritDoc]
*}
function TInitializationExpressionNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := (AValueType <> nil) and not (AValueType is TSepiDynArrayType);
end;

{--------------------------------}
{ TArrayInitializationNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TArrayInitializationNode.BeginParsing;
begin
  inherited;

  FArrayType := ValueType as TSepiStaticArrayType;
  FElementType := ArrayType.ElementType;
end;

{*
  [@inheritDoc]
*}
procedure TArrayInitializationNode.ChildBeginParsing(Child: TSepiParseTreeNode);
var
  InitChild: TSepiInitializationExpressionNode;
begin
  inherited;

  InitChild := Child as TSepiInitializationExpressionNode;

  if ChildCount > ArrayType.ArrayLength then
    InitChild.SetValueTypeAndPtr(ElementType)
  else
  begin
    InitChild.SetValueTypeAndPtr(ElementType,
      Pointer(Cardinal(ValuePtr) + (ChildCount-1) * ElementType.Size));
  end;
end;

{*
  [@inheritDoc]
*}
procedure TArrayInitializationNode.EndParsing;
begin
  if ChildCount <> ArrayType.ArrayLength then
  begin
    MakeError(Format(SElementCountMismatch,
      [ArrayType.ArrayLength, ChildCount]));
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TArrayInitializationNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := AValueType is TSepiStaticArrayType;
end;

{---------------------------------}
{ TRecordInitializationNode class }
{---------------------------------}

{*
  [@inheritDoc]
*}
procedure TRecordInitializationNode.BeginParsing;
begin
  inherited;

  FRecordType := ValueType as TSepiRecordType;
end;

{*
  [@inheritDoc]
*}
procedure TRecordInitializationNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
var
  ValueChild: TSepiInitializationExpressionNode;
  FieldName: string;
  Field: TSepiField;
begin
  inherited;

  if not (Child is TSepiInitializationExpressionNode) then
    Exit;

  ValueChild := TSepiInitializationExpressionNode(Child);
  FieldName := Children[ChildCount-2].AsText;
  Field := RecordType.GetMeta(FieldName) as TSepiField;

  if not CheckIdentFound(Field, FieldName, Children[ChildCount-2]) then
    ValueChild.SetValueTypeAndPtr(SystemUnit.Integer)
  else
  begin
    ValueChild.SetValueTypeAndPtr(Field.FieldType,
      Pointer(Cardinal(ValuePtr) + Field.Offset));
  end;
end;

{*
  [@inheritDoc]
*}
function TRecordInitializationNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := AValueType is TSepiRecordType;
end;

{-------------------------------}
{ TGUIDInitializationNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
procedure TGUIDInitializationNode.BeginParsing;
begin
  inherited;

  FRecordType := ValueType as TSepiRecordType;
end;

{*
  [@inheritDoc]
*}
procedure TGUIDInitializationNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TRecordInitializationNode then
    TRecordInitializationNode(Child).SetValueTypeAndPtr(ValueType, ValuePtr);
end;

{*
  [@inheritDoc]
*}
procedure TGUIDInitializationNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
begin
  if Child.SymbolClass = tkStringCst then
  begin
    try
      TGUID(ValuePtr^) := StringToGUID(StrRepresToStr(Child.AsText));
    except
      on Error: EConvertError do
        Child.MakeError(Error.Message);
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TGUIDInitializationNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := AValueType = SystemUnit.TGUID;
end;

{--------------------------------}
{ TOtherInitializationNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TOtherInitializationNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TConstExpressionNode).ValueType := ValueType;
end;

{*
  [@inheritDoc]
*}
procedure TOtherInitializationNode.EndParsing;
var
  ReadableValue: ISepiReadableValue;
begin
  ReadableValue := (Children[0] as TConstExpressionNode).AsValue(
    ValueType) as ISepiReadableValue;

  if (ValuePtr <> nil) and (ReadableValue <> nil) then
    ValueType.CopyData(ReadableValue.ConstValuePtr^, ValuePtr^);
end;

{*
  [@inheritDoc]
*}
function TOtherInitializationNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := (AValueType <> nil) and
    not ((AValueType is TSepiArrayType) or (AValueType is TSepiRecordType));
end;

{----------------------------}
{ TConstExpressionNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
function TConstExpressionNode.ValidateExpression: Boolean;
var
  Value: ISepiValue;
  ReadableValue: ISepiReadableValue;
begin
  if AcceptType and Supports(Expression, ISepiTypeExpression) then
    Result := True
  else
  begin
    Value := AsValue(ValueType);
    if Value = nil then
      Result := False
    else
    begin
      if ValueType = nil then
        Value.Finalize;

      Result := Supports(Value, ISepiReadableValue, ReadableValue) and
        ReadableValue.IsConstant;

      if not Result then
        MakeError(SConstExpressionRequired);
    end;
  end;
end;

{*
  Compile une valeur constante
  @param Value       En sortie : valeur compilée
  @param ValueType   Type de la valeur
  @return True si la compilation est réussie, False en cas d'erreur
*}
function TConstExpressionNode.CompileConst(var Value;
  ValueType: TSepiType): Boolean;
var
  ReadableValue: ISepiReadableValue;
begin
  ReadableValue := AsReadableValue(ValueType);

  if ReadableValue = nil then
    Result := False
  else
  begin
    Assert(ReadableValue.IsConstant);

    ValueType.CopyData(ReadableValue.ConstValuePtr^, Value);
    Result := True;
  end;
end;

{------------------------}
{ TConstOrTypeNode class }
{------------------------}

{*
  [@inheritDoc]
*}
constructor TConstOrTypeNode.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition);
begin
  inherited;

  AcceptType := True;
end;

{--------------------}
{ TUnaryOpNode class }
{--------------------}

{*
  [@inheritDoc]
*}
function TUnaryOpNode.MakeOperation(
  const Operand: ISepiExpression): ISepiExpression;
var
  OpValue, Value: ISepiReadableValue;
  AddrValue: ISepiAddressableValue;
  Operation: TSepiOperation;
begin
  Result := nil;

  if Children[0].SymbolClass = tkAt then
  begin
    if not Supports(Operand, ISepiAddressableValue, AddrValue) then
    begin
      Operand.MakeError(SAddressableValueRequired);
      Exit;
    end;

    Value := TSepiAddressOfValue.MakeAddressOf(AddrValue);
    Result := Value as ISepiExpression;
  end else
  begin
    if not Supports(Operand, ISepiReadableValue, OpValue) then
    begin
      Operand.MakeError(SReadableValueRequired);
      OpValue := TSepiTrueConstValue.MakeIntegerLiteral(UnitCompiler, 0);
    end;

    case Children[0].SymbolClass of
      tkMinus: Operation := opNegate;
      tkNot:   Operation := opNot;
    else
      Result := Operand;
      Exit;
    end;

    Value := TSepiOperator.MakeUnaryOperation(Operation, OpValue);
    Result := Value as ISepiExpression;
  end;

  Result.SourcePos := SourcePos;
end;

{---------------------}
{ TBinaryOpNode class }
{---------------------}

function TBinaryOpNode.HandlePointerArithm(
  var LeftValue, RightValue: ISepiReadableValue): TSepiType;
var
  LeftType, RightType: TSepiType;
  IsLeft: Boolean;
  PointerValue: ISepiReadableValue;
  PointTo: TSepiType;
begin
  Result := nil;

  LeftType := LeftValue.ValueType;
  RightType := RightValue.ValueType;

  if ((LeftType is TSepiPointerType) and (RightType is TSepiIntegerType)) or
    ((LeftType is TSepiIntegerType) and (RightType is TSepiPointerType)) then
  begin
    IsLeft := LeftType is TSepiPointerType;

    if IsLeft then
      PointerValue := LeftValue
    else
      PointerValue := RightValue;

    PointTo := TSepiPointerType(PointerValue.ValueType).PointTo;

    if (PointTo.Kind in [TypInfo.tkInteger, tkChar]) and
      (PointTo.TypeData.OrdType = otUByte) then
    begin
      Result := PointerValue.ValueType;

      PointerValue := TSepiCastOperator.CastValue(
        SystemUnit.Integer, PointerValue) as ISepiReadableValue;

      if IsLeft then
        LeftValue := PointerValue
      else
        RightValue := PointerValue;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
function TBinaryOpNode.GetPriority: Integer;
begin
  case Children[0].SymbolClass of
    tkEquals, tkLowerThan, tkLowerEq,
      tkGreaterThan, tkGreaterEq, tkNotEqual: Result := 1;
    tkOr, tkAnd, tkXor: Result := 2;
    tkPlus, tkMinus: Result := 3;
  else
    Result := 4;
  end;
end;

{*
  [@inheritDoc]
*}
function TBinaryOpNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  Operation: TSepiOperation;
  LeftValue, RightValue, Value: ISepiReadableValue;
  RecastTo: TSepiType;
begin
  case Children[0].SymbolClass of
    tkPlus:        Operation := opAdd;
    tkMinus:       Operation := opSubtract;
    tkTimes:       Operation := opMultiply;
    tkDivide:      Operation := opDivide;
    tkDiv:         Operation := opIntDivide;
    tkMod:         Operation := opModulus;
    tkShl:         Operation := opShiftLeft;
    tkShr:         Operation := opShiftRight;
    tkOr:          Operation := opOr;
    tkAnd:         Operation := opAnd;
    tkXor:         Operation := opXor;
    tkEquals:      Operation := opCmpEQ;
    tkLowerThan:   Operation := opCmpLT;
    tkLowerEq:     Operation := opCmpLE;
    tkGreaterThan: Operation := opCmpGT;
    tkGreaterEq:   Operation := opCmpGE;
    tkNotEqual:    Operation := opCmpNE;
  else
    Assert(False);
    Operation := 0;
  end;

  // Check operands

  if not Supports(Left, ISepiReadableValue, LeftValue) then
  begin
    Left.MakeError(SReadableValueRequired);
    LeftValue := nil;
  end;

  if not Supports(Right, ISepiReadableValue, RightValue) then
  begin
    Right.MakeError(SReadableValueRequired);
    RightValue := nil;
  end;

  // Handle pointer arithmetic with PChar and PByte
  if Operation in [opAdd, opSubtract] then
    RecastTo := HandlePointerArithm(LeftValue, RightValue)
  else
    RecastTo := nil;

  if (LeftValue <> nil) and (RightValue <> nil) then
  begin
    // Handle Integer / Integer with at least one constant
    if (Operation = opDivide) and
      ([LeftValue.ValueType.Kind, RightValue.ValueType.Kind] *
      [TypInfo.tkFloat, tkVariant] = []) then
    begin
      if LeftValue.IsConstant then
        LeftValue := TSepiConvertOperation.ConvertValue(
          SystemUnit.Extended, LeftValue);
      if RightValue.IsConstant then
        RightValue := TSepiConvertOperation.ConvertValue(
          SystemUnit.Extended, RightValue);
    end;

    // Make operation
    Value := TSepiOperator.MakeBinaryOperation(Operation,
      LeftValue, RightValue);
  end else
  begin
    // Error cases
    if LeftValue <> nil then
      Value := LeftValue
    else if RightValue <> nil then
      Value := RightValue
    else
      Value := TSepiErroneousValue.Create(SepiRoot);
  end;

  if RecastTo <> nil then
    Value := TSepiCastOperator.CastValue(RecastTo, Value) as ISepiReadableValue;

  Result := Value as ISepiExpression;
  Result.SourcePos := SourcePos;
end;

{-----------------------}
{ TSingleExprNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
procedure TSingleExprNode.EndParsing;
var
  Callable: ISepiCallable;
begin
  if Supports(Expression, ISepiCallable, Callable) and
    (not Callable.ParamsCompleted) then
    Callable.CompleteParams;

  inherited;
end;

{---------------------}
{ TSetValueNode class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TSetValueNode.BeginParsing;
begin
  inherited;

  SetExpression(MakeExpression);
  FSetBuilder := TSepiSetBuilder.Create;
  FSetBuilder.AttachToExpression(Expression);
end;

{*
  [@inheritDoc]
*}
procedure TSetValueNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  Single, Lower, Higher: ISepiExpression;
  SingleValue, LowerValue, HigherValue: ISepiReadableValue;
begin
  if Child.ChildCount = 1 then
  begin
    // Single value

    Single := (Child.Children[0] as TSepiExpressionNode).Expression;

    if not Supports(Single, ISepiReadableValue, SingleValue) then
      Single.MakeError(SReadableValueRequired)
    else
      FSetBuilder.AddSingle(SingleValue);
  end else
  begin
    // Range

    Lower := (Child.Children[0] as TSepiExpressionNode).Expression;
    Higher := (Child.Children[1] as TSepiExpressionNode).Expression;

    if not Supports(Lower, ISepiReadableValue, LowerValue) then
      Lower.MakeError(SReadableValueRequired)
    else if not Supports(Higher, ISepiReadableValue, HigherValue) then
      Higher.MakeError(SReadableValueRequired)
    else
      FSetBuilder.AddRange(LowerValue, HigherValue);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSetValueNode.EndParsing;
begin
  FSetBuilder.Complete;

  inherited;
end;

{---------------------}
{ TNextExprNode class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TNextExprNode.EndParsing;
var
  ReadableValue: ISepiReadableValue;
begin
  if Supports(Expression, ISepiReadableValue, ReadableValue) then
  begin
    if ReadableValue.ValueType is TSepiMethodRefType then
    begin
      ISepiExpressionPart(TSepiMethodRefCall.Create(
        ReadableValue, True)).AttachToExpression(Expression);
      ReadableValue.AttachToExpression(Expression);
    end;
  end;

  inherited;
end;

{-----------------------}
{ TParametersNode class }
{-----------------------}

{*
  Compile une pseudo-routine de test d'identificateur
*}
procedure TParametersNode.CompileIdentifierTest(
  const PseudoRoutine: ISepiIdentifierTestPseudoRoutine);
begin
  PseudoRoutine.Identifier := Children[0].AsText;
  PseudoRoutine.Complete;

  SetExpression(Base);
  PseudoRoutine.AttachToExpression(Expression);
end;

{*
  Compile un transtypage ou une conversion
  @param DestType   Type de destination
*}
procedure TParametersNode.CompileCastOrConvert(DestType: TSepiType);
var
  Source: ISepiValue;
  ReadableSource: ISepiReadableValue;
  SourceType: TSepiType;
begin
  if ChildCount <> 1 then
    MakeError(SOneParamRequiredForCast);

  if ChildCount = 0 then
    Exit;

  if not Supports((Children[0] as TSepiExpressionNode).Expression,
    ISepiValue, Source) then
  begin
    Children[0].MakeError(SValueRequired);
    Exit;
  end;

  if not Supports(Source, ISepiReadableValue, ReadableSource) then
    ReadableSource := nil;
  SourceType := Source.ValueType;

  if (ReadableSource <> nil) and
    TSepiConvertOperation.ConvertionExists(DestType, SourceType) then
  begin
    SetExpression(TSepiConvertOperation.ConvertValue(
      DestType, ReadableSource) as ISepiExpression);
  end else
  begin
    if (SourceType <> nil) and (SourceType.Size <> DestType.Size) and
      ((DestType is TSepiPointerType) or (DestType is TSepiClass)) and
      (SourceType is TSepiIntegerType) and (ReadableSource <> nil) then
    begin
      Source := TSepiConvertOperation.ConvertValue(
        SystemUnit.Integer, ReadableSource);
    end;

    SetExpression(TSepiCastOperator.CastValue(
      DestType, Source) as ISepiExpression);
  end;
end;

{*
  Compile un appel de pseudo-routine dont l'argument est un type
  @param TypeOperation    Pseudo-routine
  @param TypeExpression   Expression argument
*}
procedure TParametersNode.CompileCast(
  const PseudoRoutine: ISepiCastPseudoRoutine);
begin
  if ChildCount <> 1 then
    MakeError(SOneParamRequiredForCast);

  if ChildCount = 0 then
    Exit;

  PseudoRoutine.Operand := (Children[0] as TSepiExpressionNode).AsValue;
  PseudoRoutine.Complete;

  SetExpression(Base);
  PseudoRoutine.AttachToExpression(Expression);
end;

{*
  Compile un appel de pseudo-routine dont l'argument est un type
  @param TypeOperation    Pseudo-routine
  @param TypeExpression   Expression argument
*}
procedure TParametersNode.CompileTypeOperation(
  const TypeOperation: ISepiTypeOperationPseudoRoutine;
  const TypeExpression: ISepiTypeExpression);
begin
  TypeOperation.Operand := TypeExpression;
  TypeOperation.Complete;

  SetExpression(Base);
  TypeOperation.AttachToExpression(Expression);
end;

{*
  Compile un appel de méthode
  @param Callable   Expression invocable
*}
procedure TParametersNode.CompileCall(const Callable: ISepiCallable);
var
  I: Integer;
  Expression: ISepiExpression;
  Value: ISepiValue;
begin
  for I := 0 to ChildCount-1 do
  begin
    Expression := (Children[I] as TSepiExpressionNode).Expression;
    if not Supports(Expression, ISepiValue, Value) then
      Expression.MakeError(SValueRequired)
    else
      Callable.AddParam(Value);
  end;

  Callable.CompleteParams;
  Callable.AttachToExpression(Base);
  SetExpression(Base);
end;

{*
  [@inheritDoc]
*}
procedure TParametersNode.BeginParsing;
var
  Callable: ISepiCallable;
  ReadableValue: ISepiReadableValue;
begin
  inherited;

  if Supports(Base, ISepiCallable, Callable) and
    Supports(Base, ISepiReadableValue, ReadableValue) and
    (ReadableValue.ValueType is TSepiMethodRefType) then
  begin
    Base.Detach(ISepiValue);
    Base.Detach(ISepiReadableValue);
    Base.Detach(ISepiWritableValue);
    Base.Detach(ISepiAddressableValue);

    Callable.AttachToExpression(Base);
  end;

  if Supports(Base, ISepiIdentifierTestPseudoRoutine) then
    SetSymbolClass(ntIdentTestParam);
end;

{*
  [@inheritDoc]
*}
procedure TParametersNode.EndParsing;
var
  IdentifierTestPseudoRoutine: ISepiIdentifierTestPseudoRoutine;
  CastPseudoRoutine: ISepiCastPseudoRoutine;
  TypeExpression: ISepiTypeExpression;
  TypeOperation: ISepiTypeOperationPseudoRoutine;
  Callable: ISepiCallable;
begin
  if Supports(Base, ISepiIdentifierTestPseudoRoutine,
    IdentifierTestPseudoRoutine) then
    CompileIdentifierTest(IdentifierTestPseudoRoutine)
  else if Supports(Base, ISepiCastPseudoRoutine, CastPseudoRoutine) then
    CompileCast(CastPseudoRoutine)
  else if Supports(Base, ISepiTypeExpression, TypeExpression) then
    CompileCastOrConvert(TypeExpression.ExprType)
  else if Supports(Base, ISepiTypeOperationPseudoRoutine, TypeOperation) and
    (ChildCount = 1) and
    Supports((Children[0] as TSepiExpressionNode).Expression,
      ISepiTypeExpression, TypeExpression) then
    CompileTypeOperation(TypeOperation, TypeExpression)
  else if Supports(Base, ISepiCallable, Callable) and
    (not Callable.ParamsCompleted) then
    CompileCall(Callable)
  else
  begin
    if Supports(Base, ISepiTypeOperationPseudoRoutine) then
      Children[0].MakeError(STypeIdentifierRequired)
    else
      Base.MakeError(SCallableRequired);
  end;

  inherited;
end;

{---------------------}
{ TTypeDeclNode class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TTypeDeclNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiTypeDefinitionNode then
    TSepiTypeDefinitionNode(Child).SetTypeName(
      (Children[0] as TSepiIdentifierDeclarationNode).Identifier);
end;

{-------------------------}
{ TConstantDeclNode class }
{-------------------------}

{*
  [@inheritDoc]
*}
procedure TConstantDeclNode.ChildBeginParsing(Child: TSepiParseTreeNode);
var
  InitChild: TInitializationExpressionNode;
begin
  inherited;

  if Child is TInitializationExpressionNode then
  begin
    InitChild := TInitializationExpressionNode(Child);
    InitChild.SetValueTypeAndPtr(ConstVar.VarType, ConstVar.Value);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TConstantDeclNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  ReadableValue: ISepiReadableValue;
begin
  if Child is TSepiIdentifierDeclarationNode then
    FName := TSepiIdentifierDeclarationNode(Child).Identifier
  else if Child is TSepiTypeNode then
  begin
    FConstType := TSepiTypeNode(Child).SepiType;
    FConstVar := TSepiVariable.Create(SepiContext, Name, ConstType, True);
  end else if Child is TConstExpressionNode then
  begin
    ReadableValue := TConstExpressionNode(Child).AsValue as ISepiReadableValue;

    if ReadableValue = nil then
      FConstant := TSepiConstant.Create(SepiContext, Name, 0)
    else
      FConstant := TSepiConstant.Create(SepiContext, Name,
        ReadableValue.ValueType, ReadableValue.ConstValuePtr^);
  end;

  inherited;
end;

{-------------------------}
{ TVariableDeclNode class }
{-------------------------}

{*
  [@inheritDoc]
*}
destructor TVariableDeclNode.Destroy;
begin
  FNames.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TVariableDeclNode.ChildBeginParsing(Child: TSepiParseTreeNode);
var
  InitChild: TInitializationExpressionNode;
begin
  inherited;

  if Child is TInitializationExpressionNode then
  begin
    Assert(Variable <> nil);

    InitChild := TInitializationExpressionNode(Child);
    InitChild.SetValueTypeAndPtr(Variable.VarType, Variable.Value);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TVariableDeclNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  I: Integer;
begin
  if Child is TSepiIdentifierDeclarationNode then
  begin
    if FNames = nil then
      FNames := TStringList.Create;

    Names.Add(TSepiIdentifierDeclarationNode(Child).Identifier);
  end else if Child is TSepiTypeNode then
  begin
    FVarType := TSepiTypeNode(Child).SepiType;

    if Names.Count = 1 then
      FVariable := TSepiVariable.Create(SepiContext, Names[0], VarType)
    else
    begin
      for I := 0 to Names.Count-1 do
        TSepiVariable.Create(SepiContext, Names[I], VarType);
    end;
  end;

  inherited;
end;

{------------------------}
{ TRoutineKindNode class }
{------------------------}

{*
  [@inheritDoc]
*}
function TRoutineKindNode.GetKind: TSepiSignatureKind;
begin
  if Children[0].SymbolClass = tkProcedure then
    Result := skStaticProcedure
  else
    Result := skStaticFunction;
end;

{----------------------}
{ TTypeCloneNode class }
{----------------------}

{*
  [@inheritDoc]
*}
procedure TTypeCloneNode.EndParsing;
var
  OldType: TSepiType;
begin
  OldType := TSepiType(LookForOrError(Children[0], TSepiType,
    STypeIdentifierRequired));

  if OldType <> nil then
  begin
    try
      SetSepiType(TSepiTypeClass(OldType.ClassType).Clone(
        SepiContext, TypeName, OldType));
    except
      on Error: ESepiError do
      begin
        MakeError(Error.Message);
        SetSepiType(UnitCompiler.MakeErroneousTypeAlias(TypeName));
      end;
    end;
  end;

  inherited;
end;

{----------------------------}
{ TRangeOrEnumTypeNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TRangeOrEnumTypeNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TSepiTypeDefinitionNode).SetTypeName(TypeName);
end;

{*
  [@inheritDoc]
*}
procedure TRangeOrEnumTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  SetSepiType((Child as TSepiTypeDefinitionNode).SepiType);

  inherited;
end;

{----------------------}
{ TRangeTypeNode class }
{----------------------}

{*
  Construit le type
  @param TypeName      Nom du type à créer
  @param BaseType      Type de base
  @param LowerValue    Valeur basse
  @param HigherValue   Valeur haute
*}
function TRangeTypeNode.MakeType(const TypeName: string; BaseType: TSepiType;
  const LowerValue, HigherValue): TSepiType;
var
  EnumBaseType: TSepiEnumType;
begin
  if BaseType is TSepiIntegerType then
  begin
    Result := TSepiIntegerType.Create(SepiContext, TypeName,
      Integer(LowerValue), Integer(HigherValue));
  end else if BaseType is TSepiInt64Type then
  begin
    Result := TSepiInt64Type.Create(SepiContext, TypeName,
      Int64(LowerValue), Int64(HigherValue));
  end else if BaseType is TSepiCharType then
  begin
    if TSepiCharType(BaseType).IsUnicode then
      Result := TSepiCharType.Create(SepiContext, TypeName,
        WideChar(LowerValue), WideChar(HigherValue), True)
    else
      Result := TSepiCharType.Create(SepiContext, TypeName,
        WideChar(AnsiChar(LowerValue)), WideChar(AnsiChar(HigherValue)));
  end else if BaseType is TSepiEnumType then
  begin
    EnumBaseType := TSepiEnumType(BaseType);

    Result := TSepiEnumType.Create(SepiContext, TypeName, EnumBaseType,
      EnumBaseType.ValueAsInteger(LowerValue),
      EnumBaseType.ValueAsInteger(HigherValue));
  end else
  begin
    MakeError(SOrdinalTypeRequired);
    Result := nil;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TRangeTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  Expression: ISepiExpression;
  TypeExpression: ISepiTypeExpression;
begin
  if ChildCount = 1 then
  begin
    Expression := (Children[0] as TConstExpressionNode).Expression;

    if Supports(Expression, ISepiTypeExpression, TypeExpression) then
    begin
      SetSepiType(TypeExpression.ExprType);

      if TypeName <> '' then
        TSepiTypeAlias.Create(SepiContext, TypeName, SepiType);
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TRangeTypeNode.EndParsing;
var
  LowerValue, HigherValue: ISepiReadableValue;
begin
  if ChildCount > 1 then
  begin
    LowerValue  := (Children[0] as TConstExpressionNode).AsReadableValue;
    HigherValue := (Children[1] as TConstExpressionNode).AsReadableValue;

    if (LowerValue <> nil) and (HigherValue <> nil) then
    begin
      // Try and convert value types
      TryAndConvertValues(SystemUnit, LowerValue, HigherValue);

      // Some checks
      if LowerValue.ValueType <> HigherValue.ValueType then
      begin
        MakeError(Format(STypeMismatch,
          [LowerValue.ValueType, HigherValue.ValueType]));
      end else
      begin
        // OK, we're good
        SetSepiType(MakeType(TypeName, LowerValue.ValueType,
          LowerValue.ConstValuePtr^, HigherValue.ConstValuePtr^));
      end;
    end;
  end;

  inherited;
end;

{---------------------}
{ TEnumTypeNode class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TEnumTypeNode.EndParsing;
var
  I: Integer;
  Values: array of string;
begin
  SetLength(Values, ChildCount);

  for I := 0 to ChildCount-1 do
    Values[I] := Children[I].AsText;

  SetSepiType(TSepiEnumType.Create(SepiContext, TypeName, Values,
    (RootNode as TRootNode).MinEnumSize));

  inherited;
end;

{--------------------}
{ TSetTypeNode class }
{--------------------}

{*
  [@inheritDoc]
*}
procedure TSetTypeNode.MakeErroneousType;
begin
  SetSepiType(TSepiSetType.Create(SepiContext, TypeName, SystemUnit.Byte));
end;

{*
  Teste si un type donné est valide comme type d'élément d'ensemble
  @param CompType   Type de composant à tester
  @return True s'il est valide, False sinon
*}
function TSetTypeNode.IsValidCompType(CompType: TSepiType): Boolean;
begin
  Result := (CompType is TSepiOrdType) and
    (TSepiOrdType(CompType).MaxValue - TSepiOrdType(CompType).MinValue < 256);
end;

{*
  [@inheritDoc]
*}
procedure TSetTypeNode.EndParsing;
var
  CompType: TSepiType;
begin
  CompType := (Children[0] as TSepiTypeNode).SepiType;

  if IsValidCompType(CompType) then
  begin
    SetSepiType(TSepiSetType.Create(SepiContext, TypeName,
      TSepiOrdType(CompType)));
  end else
    Children[0].MakeError(SWrongSetCompType);

  inherited;
end;

{-----------------------}
{ TStringTypeNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
procedure TStringTypeNode.EndParsing;
var
  MaxLength: Integer;
begin
  if ChildCount > 1 then
  begin
    if not (Children[1] as TConstExpressionNode).CompileConst(
      MaxLength, SystemUnit.Integer) then
      MaxLength := 255;

    SetSepiType(TSepiShortStringType.Create(SepiContext, TypeName, MaxLength));
  end else
  begin
    SetSepiType(SystemUnit.LongString);

    if TypeName <> '' then
      TSepiTypeAlias.Create(SepiContext, TypeName, SepiType);
  end;

  inherited;
end;

{------------------------}
{ TPointerTypeNode class }
{------------------------}

{*
  [@inheritDoc]
*}
procedure TPointerTypeNode.EndParsing;
var
  PointedName: string;
  PointedType: TSepiType;
begin
  PointedName := Children[0].AsText;
  PointedType := TSepiType(LookFor(Children[0], TSepiType));

  if PointedType <> nil then
    SetSepiType(TSepiPointerType.Create(SepiContext, TypeName, PointedType))
  else
    SetSepiType(TSepiPointerType.Create(SepiContext, TypeName, PointedName));

  inherited;
end;

{----------------------}
{ TArrayTypeNode class }
{----------------------}

{*
  Crée un tableau statique d'après une définition par intervalle
  @param TypeName      Nom du type tableau à créer
  @param RangeNode     Noeud intervalle
  @param ElementType   Type des éléments
  @return Type tableau créé
*}
function TArrayTypeNode.RangeDefinition(const TypeName: string;
  RangeNode: TSepiParseTreeNode; ElementType: TSepiType): TSepiStaticArrayType;
var
  LowerValue, HigherValue: ISepiReadableValue;
begin
  LowerValue :=
    (RangeNode.Children[0] as TConstExpressionNode).AsReadableValue;
  HigherValue :=
    (RangeNode.Children[1] as TConstExpressionNode).AsReadableValue;

  Result := nil;

  if (LowerValue <> nil) and (HigherValue <> nil) then
  begin
    // Try and convert value types
    TryAndConvertValues(SystemUnit, LowerValue, HigherValue);

    // Some checks
    if LowerValue.ValueType <> HigherValue.ValueType then
    begin
      RangeNode.MakeError(Format(STypeMismatch,
        [LowerValue.ValueType, HigherValue.ValueType]));
    end else if not (LowerValue.ValueType is TSepiOrdType) then
    begin
      RangeNode.MakeError(SOrdinalTypeRequired);
    end else
    begin
      // OK, we're good
      Result := TSepiStaticArrayType.Create(SepiContext, TypeName,
        TSepiOrdType(LowerValue.ValueType), ConstValueAsInt64(LowerValue),
        ConstValueAsInt64(HigherValue), ElementType);
    end;
  end;

  if Result = nil then
  begin
    Result := TSepiStaticArrayType.Create(SepiContext, TypeName,
      SystemUnit.Integer, 0, 0, ElementType);
  end;
end;

{*
  Crée un tableau statique d'après une définition par type
  @param TypeName      Nom du type tableau à créer
  @param IndexType     Type de l'index
  @param ElementType   Type des éléments
  @return Type tableau créé
*}
function TArrayTypeNode.TypedDefinition(const TypeName: string;
  IndexNode: TConstExpressionNode;
  ElementType: TSepiType): TSepiStaticArrayType;
var
  NodeType: TSepiType;
  IndexType: TSepiOrdType;
begin
  NodeType := IndexNode.AsType;

  Result := nil;

  if not (NodeType is TSepiOrdType) then
  begin
    IndexNode.MakeError(SOrdinalTypeRequired);
  end else
  begin
    IndexType := TSepiOrdType(NodeType);
    Result := TSepiStaticArrayType.Create(SepiContext, TypeName, IndexType,
      IndexType.MinValue, IndexType.MaxValue, ElementType);
  end;

  if Result = nil then
  begin
    Result := TSepiStaticArrayType.Create(SepiContext, TypeName,
      SystemUnit.Integer, 0, 0, ElementType);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TArrayTypeNode.EndParsing;
var
  ElementType: TSepiType;
  I: Integer;
  RangeNode: TSepiParseTreeNode;
  SubTypeName: string;
begin
  ElementType := (Children[1] as TSepiTypeNode).SepiType;

  with Children[0] do
  begin
    if ChildCount = 0 then
    begin
      // Dynamic array
      SetSepiType(TSepiDynArrayType.Create(SepiContext, TypeName, ElementType));
    end else
    begin
      // Static array

      for I := ChildCount-1 downto 0 do
      begin
        RangeNode := Children[I];

        if I = 0 then
          SubTypeName := TypeName
        else
          SubTypeName := '';

        if RangeNode.ChildCount > 1 then
          ElementType := RangeDefinition(SubTypeName, RangeNode, ElementType)
        else
          ElementType := TypedDefinition(SubTypeName,
            RangeNode.Children[0] as TConstExpressionNode, ElementType);
      end;

      SetSepiType(ElementType);
    end;
  end;

  inherited;
end;

{-----------------------}
{ TRecordTypeNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
function TRecordTypeNode.GetSepiContext: TSepiMeta;
begin
  if RecordType <> nil then
    Result := RecordType
  else
    Result := inherited GetSepiContext;
end;

{*
  [@inheritDoc]
*}
procedure TRecordTypeNode.BeginParsing;
begin
  inherited;

  FRecordType := TSepiRecordType.Create(SepiContext, TypeName, IsPacked);
end;

{*
  [@inheritDoc]
*}
procedure TRecordTypeNode.EndParsing;
begin
  RecordType.Complete;
  SetSepiType(RecordType);

  inherited;
end;

{-----------------------------}
{ TPackedRecordTypeNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
constructor TPackedRecordTypeNode.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition);
begin
  inherited;

  IsPacked := True;
end;

{----------------------}
{ TClassTypeNode class }
{----------------------}

{*
  [@inheritDoc]
*}
procedure TClassTypeNode.CreateClass(ParentClass: TSepiClass = nil);
var
  Context: TSepiMeta;
begin
  Context := SepiContext; // Save context before FSepiClass is set
  FSepiClass := Context.GetMeta(TypeName) as TSepiClass;

  if FSepiClass <> nil then
    FSepiClass.Create(Context, TypeName, ParentClass)
  else
    FSepiClass := TSepiClass.Create(Context, TypeName, ParentClass);
end;

{*
  [@inheritDoc]
*}
function TClassTypeNode.GetSepiContext: TSepiMeta;
begin
  if SepiClass <> nil then
    Result := SepiClass
  else
    Result := inherited GetSepiContext;
end;

{*
  [@inheritDoc]
*}
procedure TClassTypeNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child.SymbolClass = tkOf then
    FIsMetaClass := True
  else if Child is TMemberListNode then
  begin
    Assert((not IsMetaClass) or (not IsForwardClass));

    if not IsClass then
    begin
      FIsClass := True;
      CreateClass;
    end;

    TMemberListNode(Child).Owner := SepiClass;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TClassTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  I: Integer;
  ParentClass, ReferencedClass: TSepiClass;
  ImplementedIntf: TSepiInterface;
begin
  if (Child.SymbolClass = ntClassHeritage) and (Child.ChildCount > 0) then
  begin
    FIsClass := True;

    // Parent class
    ParentClass := TSepiClass(LookForOrError(Child.Children[0],
      TSepiClass, SClassTypeRequired));

    // Create class
    CreateClass(ParentClass);

    // Implemented interfaces
    for I := 1 to Child.ChildCount-1 do
    begin
      ImplementedIntf := TSepiInterface(LookForOrError(Child.Children[I],
        TSepiInterface, SInterfaceTypeRequired));

      if ImplementedIntf <> nil then
        SepiClass.AddInterface(ImplementedIntf);
    end;
  end else if Child.SymbolClass = ntQualifiedIdent then
  begin
    // Make meta-class
    Assert(IsMetaClass);

    ReferencedClass := TSepiClass(LookForOrError(Child,
      TSepiClass, SClassTypeRequired));

    if ReferencedClass = nil then
      ReferencedClass := SystemUnit.TObject;

    FSepiMetaClass := TSepiMetaClass.Create(SepiContext, TypeName,
      ReferencedClass);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TClassTypeNode.EndParsing;
begin
  if (not IsClass) and (not IsMetaClass) then
  begin
    // Forward declaration
    FIsForwardClass := True;
    FSepiClass := TSepiClass.ForwardDecl(SepiContext, TypeName);
  end else if IsClass then
  begin
    // Complete class
    SepiClass.Complete;
  end;

  if IsMetaClass then
    SetSepiType(SepiMetaClass)
  else
    SetSepiType(SepiClass);

  inherited;
end;

{*
  [@inheritDoc]
*}
function TClassTypeNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
var
  Meta: TSepiMeta;
begin
  if SepiClass <> nil then
  begin
    Meta := SepiContext.LookFor(Identifier);
    if Meta <> nil then
    begin
      Result := MakeExpression;
      AddMetaToExpression(Result, Meta);
      Exit;
    end;
  end;

  Result := inherited ResolveIdent(Identifier);
end;

{--------------------------}
{ TInterfaceTypeNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
procedure TInterfaceTypeNode.CreateInterface;
var
  Context: TSepiMeta;
begin
  Context := SepiContext; // Save context before FSepiIntf is set
  FSepiIntf := Context.GetMeta(TypeName) as TSepiInterface;

  if FSepiIntf <> nil then
    FSepiIntf.Create(Context, TypeName, ParentIntf, GUID, IsDispIntf)
  else
    FSepiIntf := TSepiInterface.Create(Context, TypeName, ParentIntf, GUID,
      IsDispIntf);
end;

{*
  [@inheritDoc]
*}
function TInterfaceTypeNode.GetSepiContext: TSepiMeta;
begin
  if SepiIntf <> nil then
    Result := SepiIntf
  else
    Result := inherited GetSepiContext;
end;

{*
  [@inheritDoc]
*}
procedure TInterfaceTypeNode.BeginParsing;
begin
  inherited;

  FIsDispIntf := SymbolClass = ntDispInterfaceDesc;
end;

{*
  [@inheritDoc]
*}
procedure TInterfaceTypeNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TInterfaceGUIDNode then
  begin
    TInterfaceGUIDNode(Child).SetValuePtr(@FGUID);
  end else if Child is TMemberListNode then
  begin
    CreateInterface;
    TMemberListNode(Child).Owner := SepiIntf;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TInterfaceTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child.SymbolClass = ntInterfaceHeritage then
  begin
    if Child.ChildCount > 0 then
    begin
      FParentIntf := TSepiInterface(LookForOrError(Child,
        TSepiInterface, SInterfaceTypeRequired));
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TInterfaceTypeNode.EndParsing;
begin
  if SepiIntf = nil then
  begin
    // Forward declaration
    FSepiIntf := TSepiInterface.ForwardDecl(SepiContext, TypeName);
  end else
  begin
    // Complete interface
    SepiIntf.Complete;
  end;

  SetSepiType(SepiIntf);

  inherited;
end;

{*
  [@inheritDoc]
*}
function TInterfaceTypeNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
var
  Meta: TSepiMeta;
begin
  if SepiIntf <> nil then
  begin
    Meta := SepiContext.LookFor(Identifier);
    if Meta <> nil then
    begin
      Result := MakeExpression;
      AddMetaToExpression(Result, Meta);
      Exit;
    end;
  end;

  Result := inherited ResolveIdent(Identifier);
end;

{--------------------------}
{ TInterfaceGUIDNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
procedure TInterfaceGUIDNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  ConstExpr: TConstExpressionNode;
  GUIDStr: string;
begin
  ConstExpr := Child as TConstExpressionNode;

  if ConstExpr.AsReadableValue.ValueType = SystemUnit.TGUID then
  begin
    ConstExpr.CompileConst(ValuePtr^, SystemUnit.TGUID);
  end else if ConstExpr.CompileConst(GUIDStr, SystemUnit.LongString) then
  begin
    try
      TGUID(ValuePtr^) := StringToGUID(GUIDStr);
    except
      on Error: EConvertError do
        MakeError(Error.Message);
    end;
  end;

  inherited;
end;

{*
  Indique où stocker le résultat
  Si AValuePtr vaut nil, une nouvelle valeur de type TGUID sera créée, et
  libérée à la destruction de cet objet.
  Cette méthode doit être appelée exactement une fois, avant BeginParsing.
  @param AValuePtr   Pointeur où stocker le résultat (peut être nil)
*}
procedure TInterfaceGUIDNode.SetValuePtr(AValuePtr: PGUID);
begin
  SetValueTypeAndPtr(SystemUnit.TGUID, AValuePtr);
end;

{--------------------------}
{ TMethodRefTypeNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
destructor TMethodRefTypeNode.Destroy;
begin
  FSignature.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TMethodRefTypeNode.BeginParsing;
begin
  inherited;

  FSignature := TSepiSignature.CreateConstructing(SepiUnit);
end;

{*
  [@inheritDoc]
*}
procedure TMethodRefTypeNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiSignatureBuilderNode then
    TSepiSignatureBuilderNode(Child).SetSignature(Signature);
end;

{*
  [@inheritDoc]
*}
procedure TMethodRefTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  case Child.SymbolClass of
    // Routine kind
    ntRoutineKind:
    begin
      case Child.Children[0].SymbolClass of
        tkProcedure: Signature.Kind := skStaticProcedure;
        tkFunction:  Signature.Kind := skStaticFunction;
      end;
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TMethodRefTypeNode.EndParsing;
const
  IgnoredModifiers: array[0..2] of string = (
    'assembler', 'platform', 'deprecated'
  );
var
  Index: Integer;
  ModifiersNode, ModifierNode: TSepiParseTreeNode;
begin
  ModifiersNode := Children[2];
  while ModifiersNode.ChildCount > 0 do
  begin
    ModifierNode := ModifiersNode.Children[0];

    Index := AnsiIndexText(ModifierNode.AsText, CallingConventionStrings);

    if Index >= 0 then
    begin
      // Calling convention
      Signature.CallingConvention := TCallingConvention(Index);
    end else if ModifierNode.SymbolClass = ntEventIsOfObject then
    begin
      // of object - do not duplicate
      if Signature.Kind = skStaticProcedure then
        Signature.Kind := skObjectProcedure
      else if Signature.Kind = skStaticFunction then
        Signature.Kind := skObjectFunction
      else
      begin
        ModifierNode.MakeError(Format(
          SDuplicateModifier, [ModifierNode.AsText]));
      end;
    end else if AnsiIndexText(ModifierNode.AsText, IgnoredModifiers) = -1 then
    begin
      // Unknown modifier
      ModifierNode.MakeError(Format(SUnknownMethodRefModifier,
        [ModifierNode.AsText]), ekWarning);
    end;

    ModifiersNode := ModifiersNode.Children[1];
  end;

  Signature.Complete;
  SetSepiType(TSepiMethodRefType.Create(SepiContext, TypeName, Signature));

  inherited;
end;

{-----------------------}
{ TMemberListNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
procedure TMemberListNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TMemberNode).Owner := Owner;
end;

{------------------}
{ TFieldNode class }
{------------------}

{*
  [@inheritDoc]
*}
procedure TFieldNode.EndParsing;
var
  IdentList: TSepiIdentifierDeclListNode;
  I: Integer;
begin
  IdentList := Children[0] as TSepiIdentifierDeclListNode;
  FFieldType := (Children[1] as TSepiTypeNode).SepiType;

  Assert(Owner is TSepiClass);

  for I := 0 to IdentList.IdentifierCount-1 do
    FLastField := TSepiClass(Owner).AddField(
      IdentList.Identifiers[I], FieldType, I > 0);

  inherited;
end;

{-------------------}
{ TMethodNode class }
{-------------------}

{*
  [@inheritDoc]
*}
destructor TMethodNode.Destroy;
begin
  FSignature.Free;

  inherited;
end;

{*
  Construit un redirecteur de méthode d'interface
  @param IntfNode         Noeud de l'interface
  @param IntfMethodNode   Noeud de la méthode d'interface
  @param RedirectorNode   Noeud du redirecteur
*}
procedure TMethodNode.HandleIntfMethodRedirector(
  IntfNode, IntfMethodNode, RedirectorNode: TSepiParseTreeNode);
var
  SepiClass: TSepiClass;
  Intf: TSepiInterface;
  IntfMethod: TSepiMethod;
begin
  SepiClass := Signature.Context as TSepiClass;

  // Read interface node

  Intf := TSepiInterface(LookForOrError(IntfNode, TSepiInterface,
    SInterfaceTypeRequired));
  if Intf = nil then
    Exit;

  if not SepiClass.ClassImplementsInterface(Intf) then
  begin
    IntfNode.MakeError(Format(SClassDoesNotImplementIntf,
      [SepiClass.Name, Intf.Name]));
    Exit;
  end;

  // Read interface method node

  IntfMethod := Intf.LookForMember(IntfMethodNode.AsText) as TSepiMethod;
  if not CheckIdentFound(IntfMethod, IntfMethodNode.AsText, IntfMethodNode) then
    Exit;

  // Create redirector

  SepiClass.AddIntfMethodRedirector(IntfMethod, RedirectorNode.AsText);
end;

{*
  [@inheritDoc]
*}
procedure TMethodNode.BeginParsing;
begin
  inherited;

  FSignature := TSepiSignature.CreateConstructing(SepiUnit, Owner);
end;

{*
  [@inheritDoc]
*}
procedure TMethodNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiSignatureBuilderNode then
    TSepiSignatureBuilderNode(Child).SetSignature(Signature);
end;

{*
  [@inheritDoc]
*}
procedure TMethodNode.ChildEndParsing(Child: TSepiParseTreeNode);
const
  IgnoredModifiers: array[0..5] of string = (
    'deprecated', 'inline', 'assembler', 'platform', 'reintroduce', 'dispid'
  );
var
  Str: string;
  Temp: Integer;
begin
  if Child is TSepiIdentifierDeclarationNode then
    FName := TSepiIdentifierDeclarationNode(Child).Identifier;

  case Child.SymbolClass of
    // Type de routine
    ntRoutineKind:
    begin
      case Child.Children[0].SymbolClass of
        tkProcedure: Signature.Kind := skStaticProcedure;
        tkFunction:  Signature.Kind := skStaticFunction;
      end;
    end;

    // Type de méthode
    ntMethodKind:
    begin
      case Child.Children[0].SymbolClass of
        tkProcedure:   Signature.Kind := skObjectProcedure;
        tkFunction:    Signature.Kind := skObjectFunction;
        tkConstructor: Signature.Kind := skConstructor;
        tkDestructor:  Signature.Kind := skDestructor;
      else
        case Child.Children[1].SymbolClass of
          tkProcedure: Signature.Kind := skClassProcedure;
          tkFunction:  Signature.Kind := skClassFunction;
        end;
      end;
    end;

    // Redirecteur de méthode d'interface
    ntIntfMethodRedirector:
    begin
      FIsIntfMethodRedirector := True;
      HandleIntfMethodRedirector(Children[1], Child.Children[0],
        Child.Children[1]);
    end;

    // Modificateur
    ntRoutineModifier, ntMethodModifier:
    begin
      Str := Child.Children[0].AsText;
      Temp := AnsiIndexText(Str, LinkKindStrings);

      if Temp >= 0 then
      begin
        // Link kind
        FLinkKind := TMethodLinkKind(Temp);

        // Store message ID
        if LinkKind = mlkMessage then
          (Child.Children[1] as TConstExpressionNode).CompileConst(
            FMsgID, SystemUnit.Integer);
      end else
      begin
        Temp := AnsiIndexText(Str, CallingConventionStrings);
        if Temp >= 0 then
        begin
          // Calling convention
          Signature.CallingConvention := TCallingConvention(Temp);
        end else
        begin
          // Other modifier
          if LowerCase(Str) = 'abstract' then
          begin
            // abstract
            FAbstract := True;
          end else if LowerCase(Str) = 'overload' then
          begin
            // overload
            if Signature.Context is TSepiInterface then
              Child.MakeError(SIntfMethodCantBeOverloaded)
            else
              FIsOverloaded := True;
          end else if AnsiIndexText(Str, IgnoredModifiers) = -1 then
          begin
            // Unknown modifier
            Child.MakeError(Format(SUnknownMethodModifier, [Str]), ekWarning);
          end;
        end;
      end;
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TMethodNode.EndParsing;
begin
  if not IsIntfMethodRedirector then
  begin
    Signature.Complete;

    // Create method
    if Signature.Context is TSepiClass then
    begin
      // Class method
      if (not IsOverloaded) and (LinkKind = mlkOverride) and
        (TSepiClass(Owner).LookForMember(Name) is TSepiOverloadedMethod) then
        FIsOverloaded := True;

      if IsOverloaded then
        TSepiClass(Owner).AddOverloadedMethod(Name, nil, Signature, LinkKind,
          IsAbstract, MsgID)
      else
        TSepiClass(Owner).AddMethod(Name, nil, Signature, LinkKind,
          IsAbstract, MsgID)
    end else if Signature.Context is TSepiInterface then
    begin
      // Interface method - can't be overloaded
      TSepiInterface(Owner).AddMethod(Name, Signature);
    end else if Signature.Context = nil then
    begin
      // Routine
      if IsOverloaded then
        TSepiMethod.CreateOverloaded(SepiUnit, Name, nil, Signature)
      else
        TSepiMethod.Create(SepiUnit, Name, nil, Signature);
    end else
      Assert(False);
  end;

  inherited;
end;

{---------------------}
{ TPropertyNode class }
{---------------------}

{*
  [@inheritDoc]
*}
destructor TPropertyNode.Destroy;
begin
  FSignature.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TPropertyNode.BeginParsing;
begin
  inherited;

  FSignature := TSepiSignature.CreateConstructing(SepiUnit, Owner);
  FSignature.Kind := skProperty;
end;

{*
  Vérifie la validité d'un accès par champ
  @param Field   Champ accesseur
  @return True si l'accès est valide, False sinon
*}
function TPropertyNode.CheckFieldAccess(Node: TSepiParseTreeNode;
  Field: TSepiField): Boolean;
begin
  if Signature.ParamCount > 0 then
  begin
    Node.MakeError(SParametersMismatch);
    Result := False;
  end else if Field.FieldType <> Signature.ReturnType then
  begin
    Node.MakeError(Format(STypeMismatch,
      [Field.FieldType.Name, Signature.ReturnType.Name]));
    Result := False;
  end else
  begin
    Result := True;
  end;
end;

{*
  Vérifie la validité d'un accès par méthode
  @param Method              Méthode accesseur
  @param RequiredSignature   Signature attendue
  @return True si l'accès est valide, False sinon
*}
function TPropertyNode.CheckMethodAccess(Node: TSepiParseTreeNode;
  Method: TSepiMethod; IsWriteAccess: Boolean): Boolean;
var
  ParamCount, I: Integer;
  Param: TSepiParam;
begin
  ParamCount := Signature.ParamCount + Byte(Index <> nil) + Byte(IsWriteAccess);

  if Method.Signature.ParamCount <> ParamCount then
    Result := False
  else
  begin
    Result := True;

    // Check parameters
    for I := 0 to Signature.ParamCount-1 do
    begin
      if not Method.Signature.Params[I].Equals(Signature.Params[I],
        pcoCompatibility) then
      begin
        Result := False;
        Break;
      end;
    end;

    // Check index
    if Result and (Index <> nil) then
    begin
      Param := Method.Signature.Params[Signature.ParamCount];

      if (Param.Kind in [pkVar, pkOut]) or (Param.ParamType = nil) or
        (not Param.ParamType.CompatibleWith(Index.ValueType)) then
        Result := False;
    end;

    // Check property type
    if Result then
    begin
      if IsWriteAccess then
      begin
        Param := Method.Signature.Params[Method.Signature.ParamCount-1];

        if (Param.Kind in [pkVar, pkOut]) or
          (Param.ParamType <> Signature.ReturnType) then
          Result := False;
      end else
      begin
        if Method.Signature.ReturnType <> Signature.ReturnType then
          Result := False;
      end;
    end;
  end;

  if not Result then
    Node.MakeError(SParametersMismatch);
end;

{*
  Gère l'accès en lecture
  @param Node   Noeud d'informations
*}
procedure TPropertyNode.HandleAccess(Node: TSepiParseTreeNode;
  IsWriteAccess: Boolean);
var
  Access: TSepiMeta;
begin
  if Owner is TSepiClass then
    Access := TSepiClass(Owner).LookForMember(Node.AsText)
  else
    Access := (Owner as TSepiInterface).LookForMember(Node.AsText);

  if not CheckIdentFound(Access, Node.AsText, Node) then
    Exit;

  if Access is TSepiField then
  begin
    // Field access
    if not CheckFieldAccess(Node, TSepiField(Access)) then
      Exit;
  end else if Access is TSepiMethod then
  begin
    // Method access
    if not CheckMethodAccess(Node, TSepiMethod(Access), IsWriteAccess) then
      Exit;
  end else
  begin
    // Error
    Node.MakeError(SFieldOrMethodRequired);
    Exit;
  end;

  if IsWriteAccess then
    FWriteAccess := Access
  else
    FReadAccess := Access;
end;

{*
  Gère l'index
  @param Node   Noeud d'informations
*}
procedure TPropertyNode.HandleIndex(Node: TSepiParseTreeNode);
begin
  FIndex := (Node as TConstExpressionNode).AsValue as ISepiReadableValue;

  if not (Index.ValueType is TSepiOrdType) then
  begin
    Node.MakeError(SOrdinalTypeRequired);
    FIndex := nil;
  end;
end;

{*
  Gère la valeur par défaut
  @param Node   Noeud d'informations
*}
procedure TPropertyNode.HandleDefaultValue(Node: TSepiParseTreeNode);
begin
  if Node = nil then
    FDefaultValue := nil;
  // TODO Handle default value
end;

{*
  Gère le spécificateur de stockage
  @param Node   Noeud d'informations
*}
procedure TPropertyNode.HandleStorage(Node: TSepiParseTreeNode);
begin
  // TODO Handle storage
end;

{*
  [@inheritDoc]
*}
procedure TPropertyNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiSignatureBuilderNode then
    TSepiSignatureBuilderNode(Child).SetSignature(Signature);
end;

{*
  [@inheritDoc]
*}
procedure TPropertyNode.ChildEndParsing(Child: TSepiParseTreeNode);
const
  IgnoredModifiers: array[0..0] of string = (
    'deprecated'
  );
var
  Str: string;
  I: Integer;
begin
  if Child is TSepiIdentifierDeclarationNode then
    FName := TSepiIdentifierDeclarationNode(Child).Identifier;

  case Child.SymbolClass of
    // Signature
    ntPropertySignature:
    begin
      Signature.Complete;
    end;

    // Marqueur de redéfinition
    ntRedefineMarker:
    begin
      FRedefine := True;

      if Owner is TSepiClass then
        FPrevious := TSepiClass(Owner).LookForMember(Name) as TSepiProperty
      else
        FPrevious := TSepiInterface(Owner).LookForMember(Name) as TSepiProperty;

      if Previous = nil then
      begin
        Child.MakeError(SPropertyNotFoundInBaseClass);
        Signature.ReturnType := SystemUnit.Integer;
      end else
      begin
        for I := 0 to Previous.Signature.ParamCount-1 do
          TSepiParam.Clone(Signature, Previous.Signature.Params[I]);
        Signature.ReturnType := Previous.Signature.ReturnType;

        if Previous.Index <> NoIndex then
        begin
          FIndex := TSepiTrueConstValue.MakeOrdinalValue(UnitCompiler,
            SystemUnit.Integer, Previous.Index);
        end;

        if Previous.DefaultValue <> NoDefaultValue then
        begin
          FDefaultValue := TSepiTrueConstValue.MakeOrdinalValue(UnitCompiler,
            Signature.ReturnType as TSepiOrdType, Previous.DefaultValue);
        end;
      end;

      Signature.Complete;
    end;

    // Prop-info
    ntPropInfo:
    begin
      case Child.Children[0].SymbolClass of
        tkRead:      HandleAccess      (Child.Children[1], False);
        tkWrite:     HandleAccess      (Child.Children[1], True);
        tkIndex:     HandleIndex       (Child.Children[1]);
        tkDefault:   HandleDefaultValue(Child.Children[1]);
        tkNoDefault: HandleDefaultValue(nil);
        tkStored:    HandleStorage     (Child.Children[1]);
      end;
    end;

    // Modificateur
    ntPropertyModifier:
    begin
      Str := Child.Children[0].AsText;

      if LowerCase(Str) = 'default' then
      begin
        // default
        if IsDefault then
          Child.MakeError(Format(SDuplicateModifier, [Str]))
        else if (Owner is TSepiClass) and
          (TSepiClass(Owner).DefaultProperty <> nil) and
          (TSepiClass(Owner).DefaultProperty.Owner = Owner) then
          Child.MakeError(SDuplicateDefaultProperty)
        else if Signature.ParamCount = 0 then
          Child.MakeError(SArrayPropertyRequired)
        else
          FIsDefault := True;
      end else if AnsiIndexText(Str, IgnoredModifiers) = -1 then
      begin
        // Unknown modifier
        Child.MakeError(Format(SUnknownMethodModifier, [Str]), ekWarning);
      end;
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TPropertyNode.EndParsing;
var
  IntDefaultValue, AccessIndex: Integer;
begin
  if Index = nil then
    AccessIndex := NoIndex
  else
    AccessIndex := TSepiOrdType(Index.ValueType).ValueAsInteger(
      Index.ConstValuePtr^);

  // Create the property
  if Owner is TSepiClass then
  begin
    if DefaultValue = nil then
      IntDefaultValue := NoDefaultValue
    else
      IntDefaultValue := TSepiOrdType(DefaultValue.ValueType).ValueAsInteger(
        DefaultValue.ConstValuePtr^);

    // Class property
    if Redefine then
    begin
      // Redefine property
      TSepiClass(Owner).RedefineProperty(Name, ReadAccess, WriteAccess,
        IntDefaultValue, Storage);
    end else
    begin
      // New property
      TSepiClass(Owner).AddProperty(Name, Signature, ReadAccess, WriteAccess,
        AccessIndex, IntDefaultValue, Storage, IsDefault)
    end;
  end else if Owner is TSepiInterface then
  begin
    // Interface property
    TSepiInterface(Owner).AddProperty(Name, Signature, ReadAccess, WriteAccess,
      AccessIndex, IsDefault);
  end else
    Assert(False);

  inherited;
end;

{-----------------------}
{ TVisibilityNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
procedure TVisibilityNode.EndParsing;
begin
  (Owner as TSepiClass).CurrentVisibility :=
    TMemberVisibility(AnsiIndexText(AsText, VisibilityStrings));

  inherited;
end;

{------------------}
{ TParamNode class }
{------------------}

{*
  [@inheritDoc]
*}
procedure TParamNode.ChildBeginParsing(Child: TSepiParseTreeNode);
var
  InitChild: TInitializationExpressionNode;
begin
  inherited;

  if Child is TInitializationExpressionNode then
  begin
    InitChild := TInitializationExpressionNode(Child);

    if OpenArray then
    begin
      Child.MakeError(SOpenArrayParamCantHaveDefaultValue);
      InitChild.SetValueTypeAndPtr(SystemUnit.Integer);
    end else if Length(Names) > 1 then
    begin
      Child.MakeError(SMultiNameParamCantHaveDefaultValue);
      InitChild.SetValueTypeAndPtr(SystemUnit.Integer);
    end else
    begin
      InitChild.SetValueTypeAndPtr(ParamType);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TParamNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  I: Integer;
begin
  case Child.SymbolClass of
    // Type de paramètre
    ntParamKind:
    begin
      if Child.ChildCount > 0 then
      begin
        case Child.Children[0].SymbolClass of
          tkConst: FKind := pkConst;
          tkVar:   FKind := pkVar;
          tkOut:   FKind := pkOut;
        end;
      end;
    end;

    // Noms des paramètres
    ntParamNameList:
    begin
      SetLength(FNames, Child.ChildCount);
      for I := 0 to Child.ChildCount-1 do
        FNames[I] := Child.Children[I].AsText;
    end;

    // Open array
    ntParamIsArray:
    begin
      FOpenArray := True;
    end;

    // Type du paramètre
    ntParamType, ntParamArrayType:
    begin
      if Child.Children[0].SymbolClass = ntQualifiedIdent then
      begin
        FType := TSepiType(LookForOrError(Child.Children[0], TSepiType,
          STypeIdentifierRequired));
        if FType = nil then
          FType := SystemUnit.Integer;
      end;
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TParamNode.EndParsing;
var
  I: Integer;
begin
  for I := 0 to Length(Names)-1 do
    TSepiParam.Create(Signature, Names[I], ParamType, Kind, OpenArray);

  inherited;
end;

{-----------------------}
{ TReturnTypeNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
procedure TReturnTypeNode.EndParsing;
var
  Kind: TSepiSignatureKind;
  ReturnType: TSepiType;
begin
  Kind := Signature.Kind;

  if Kind in skWithReturnType then
  begin
    // Return type required
    if ChildCount > 0 then
    begin
      ReturnType := TSepiType(LookForOrError(Children[0], TSepiType,
        STypeIdentifierRequired));
    end else
    begin
      MakeError(SReturnTypeRequired);
      ReturnType := nil;
    end;

    if ReturnType = nil then
      Signature.ReturnType := SystemUnit.Integer
    else
      Signature.ReturnType := ReturnType;
  end else
  begin
    // Return type forbidden
    if ChildCount > 0 then
      MakeError(SReturnTypeForbidden);
  end;

  inherited;
end;

{---------------------}
{ TPropInfoNode class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TPropInfoNode.ChildBeginParsing(Child: TSepiParseTreeNode);
var
  PropertyType: TSepiType;
begin
  inherited;

  if (ChildCount = 2) and (Children[0].SymbolClass = tkDefault) then
  begin
    PropertyType := (Parent as TPropertyNode).Signature.ReturnType;
    (Children[1] as TConstExpressionNode).ValueType := PropertyType;
  end;
end;

{-----------------------}
{ TMethodImplNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
destructor TMethodImplNode.Destroy;
begin
  FSignature.Free;

  inherited;
end;

{*
  Déclare la méthode (si autorisé)
*}
procedure TMethodImplNode.DeclareMethod;
begin
  // Methods must always be declared before being implemented
  if Pos('.', Name) > 0 then
  begin
    MakeError(Format(SMethodNotDeclared, [Name]));
    Exit;
  end;

  // If there is an overloaded method, the overload directive must be set
  if (Overloaded <> nil) and (not IsOverloaded) then
  begin
    MakeError(Format(SMethodMustBeOverloaded, [Name]));
    FIsOverloaded := True;
  end;

  // Declare the method
  FJustDeclared := True;
  if IsOverloaded then
    FSepiMethod := TSepiMethod.CreateOverloaded(SepiUnit, Name, nil, Signature)
  else
    FSepiMethod := TSepiMethod.Create(SepiUnit, Name, nil, Signature);
end;

{*
  [@inheritDoc]
*}
procedure TMethodImplNode.BeginParsing;
begin
  inherited;

  FSignature := TSepiSignature.CreateConstructing(SepiUnit);
end;

{*
  [@inheritDoc]
*}
procedure TMethodImplNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TMethodImplDeclNode then
  begin
    // Signature dans l'implémentation
    TMethodImplDeclNode(Child).Signature := Signature;
  end else if Child.SymbolClass = ntForwardMarker then
  begin
    // Marqueur forward - ce doit être la première déclaration
    if not Self.FJustDeclared then
      MakeError(Format(SRedeclaredIdentifier, [Name]));
  end else if Child is TMethodBodyNode then
  begin
    // Corps de la méthode - implémentation réelle
    TMethodBodyNode(Child).SepiMethod := SepiMethod;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TMethodImplNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  TempMethod: TSepiMeta;
begin
  if Child is TMethodImplDeclNode then
  begin
    FName := TMethodImplDeclNode(Child).Name;
    FIsOverloaded := TMethodImplDeclNode(Child).IsOverloaded;

    // Find method
    TempMethod := SepiUnit.GetMeta(Name);

    // Update signature kind for a method implementation
    if (TempMethod <> nil) and (TempMethod.Owner is TSepiClass) then
    begin
      if Signature.Kind = skStaticProcedure then
        Signature.Kind := skObjectProcedure
      else if Signature.Kind = skStaticFunction then
        Signature.Kind := skObjectFunction;
    end;

    // Handle overloads
    if TempMethod is TSepiOverloadedMethod then
    begin
      FOverloaded := TSepiOverloadedMethod(TempMethod);
      Signature.Complete;
      FSepiMethod := Overloaded.FindMethod(Signature);
    end else if TempMethod is TSepiMethod then
    begin
      FSepiMethod := TSepiMethod(TempMethod);
      Signature.CallingConvention := SepiMethod.Signature.CallingConvention;
      Signature.Complete;

      // Check signature
      if IsOverloaded then
      begin
        Child.MakeError(Format(SPreviousDeclWasNotOverload,
          [SepiMethod.Name]));
        FIsOverloaded := False;
      end else if not SepiMethod.Signature.Equals(
        Signature, scoDeclaration) then
      begin
        Child.MakeError(Format(SDeclarationDiffersFromPreviousOne,
          [SepiMethod.Name]));
      end;
    end else
    begin
      Signature.Complete;
    end;

    // Declare method if needed
    if FSepiMethod = nil then
      DeclareMethod;

    // Check that the method has not already been implemented
    if SepiMethod <> nil then
    begin
      if UnitCompiler.FindMethodCompiler(SepiMethod) <> nil then
      begin
        Child.MakeError(Format(SMethodAlreadyImplemented, [SepiMethod.Name]));
        FSepiMethod := nil;
      end;
    end;
  end;

  inherited;
end;

{---------------------------}
{ TMethodImplDeclNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TMethodImplDeclNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiSignatureBuilderNode then
    TSepiSignatureBuilderNode(Child).SetSignature(Signature);
end;

{*
  [@inheritDoc]
*}
procedure TMethodImplDeclNode.ChildEndParsing(Child: TSepiParseTreeNode);
const
  IgnoredModifiers: array[0..3] of string = (
    'deprecated', 'inline', 'assembler', 'platform'
  );
var
  Str: string;
  Temp: Integer;
begin
  case Child.SymbolClass of
    // Type de routine/méthode
    ntMethodKind:
    begin
      case Child.Children[0].SymbolClass of
        tkProcedure:   Signature.Kind := skStaticProcedure;
        tkFunction:    Signature.Kind := skStaticFunction;
        tkConstructor: Signature.Kind := skConstructor;
        tkDestructor:  Signature.Kind := skDestructor;
      else
        case Child.Children[1].SymbolClass of
          tkProcedure: Signature.Kind := skClassProcedure;
          tkFunction:  Signature.Kind := skClassFunction;
        end;
      end;
    end;

    // Nom de la routine
    ntQualifiedIdent:
    begin
      FName := Child.AsText;
    end;

    // Modificateur
    ntRoutineModifier:
    begin
      Str := Child.AsText;
      Temp := AnsiIndexText(Str, CallingConventionStrings);
      if Temp >= 0 then
      begin
        // Calling convention
        Signature.CallingConvention := TCallingConvention(Temp);
      end else
      begin
        // Other modifier
        if LowerCase(Str) = 'overload' then
          FIsOverloaded := True
        else if AnsiIndexText(Str, IgnoredModifiers) = -1 then
          Child.MakeError(Format(SUnknownMethodModifier, [Str]), ekWarning);
      end;
    end;
  end;

  inherited;
end;

{-----------------------}
{ TMethodBodyNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
function TMethodBodyNode.GetMethodCompiler: TSepiMethodCompiler;
begin
  Result := FCompiler;
end;

{*
  [@inheritDoc]
*}
function TMethodBodyNode.GetSepiContext: TSepiMeta;
begin
  if FCompiler <> nil then
    Result := FCompiler.LocalNamespace
  else
    Result := inherited GetSepiContext;
end;

{*
  [@inheritDoc]
*}
function TMethodBodyNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  Result := MethodResolveIdent(MethodCompiler, Identifier);
end;

{*
  [@inheritDoc]
*}
procedure TMethodBodyNode.BeginParsing;
begin
  inherited;

  FCompiler := UnitCompiler.FindMethodCompiler(SepiMethod, True);
end;

{*
  [@inheritDoc]
*}
procedure TMethodBodyNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TInstructionNode then
    TInstructionNode(Child).InstructionList := MethodCompiler.Instructions;
end;

{*
  [@inheritDoc]
*}
procedure TMethodBodyNode.EndParsing;
begin
  FCompiler.Complete;

  inherited;
end;

{---------------------}
{ TLocalVarNode class }
{---------------------}

{*
  [@inheritDoc]
*}
destructor TLocalVarNode.Destroy;
begin
  FNames.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TLocalVarNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TLocalVarNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  I: Integer;
begin
  if Child is TSepiIdentifierDeclarationNode then
  begin
    if FNames = nil then
      FNames := TStringList.Create;

    Names.Add(TSepiIdentifierDeclarationNode(Child).Identifier);
  end else if Child is TSepiTypeNode then
  begin
    FVarType := TSepiTypeNode(Child).SepiType;

    for I := 0 to Names.Count-1 do
      MethodCompiler.Locals.AddLocalVar(Names[I], VarType);
  end;

  inherited;
end;

{----------------------------}
{ TInstructionListNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TInstructionListNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TInstructionNode).InstructionList := InstructionList;
end;

{----------------------------------}
{ TIfThenElseInstructionNode class }
{----------------------------------}

{*
  [@inheritDoc]
*}
procedure TIfThenElseInstructionNode.BeginParsing;
begin
  inherited;

  FInstruction := TSepiIfThenElse.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TIfThenElseInstructionNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
const
  TrueBranchChild = 1;
  FalseBranchChild = 2;
begin
  inherited;

  case Child.IndexAsChild of
    TrueBranchChild:
      (Child as TInstructionNode).InstructionList :=
        Instruction.TrueInstructions;
    FalseBranchChild:
      (Child as TInstructionNode).InstructionList :=
        Instruction.FalseInstructions;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TIfThenElseInstructionNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  TestValue: ISepiValue;
  ReadableValue: ISepiReadableValue;
begin
  if Child is TSepiExpressionNode then
  begin
    TestValue := TSepiExpressionNode(Child).AsValue(SystemUnit.Boolean);

    if Supports(TestValue, ISepiReadableValue, ReadableValue) then
      Instruction.TestValue := ReadableValue
    else
      Child.MakeError(SReadableValueRequired);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TIfThenElseInstructionNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{-----------------------------}
{ TWhileInstructionNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
constructor TWhileInstructionNode.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition);
begin
  inherited;

  FIsRepeat := SymbolClass = ntRepeatInstruction;
end;

{*
  [@inheritDoc]
*}
procedure TWhileInstructionNode.BeginParsing;
begin
  inherited;

  FInstruction := TSepiWhile.Create(MethodCompiler, IsRepeat);
end;

{*
  [@inheritDoc]
*}
procedure TWhileInstructionNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TInstructionNode then
    TInstructionNode(Child).InstructionList := Instruction.LoopInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TWhileInstructionNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  TestValue: ISepiValue;
  ReadableValue: ISepiReadableValue;
begin
  if Child is TSepiExpressionNode then
  begin
    TestValue := TSepiExpressionNode(Child).AsValue(SystemUnit.Boolean);

    if Supports(TestValue, ISepiReadableValue, ReadableValue) then
    begin
      if IsRepeat then
        ReadableValue := TSepiUnaryOperation.MakeOperation(
          opNot, ReadableValue);

      Instruction.TestValue := ReadableValue;
    end else
      Child.MakeError(SReadableValueRequired);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TWhileInstructionNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{---------------------------}
{ TForInstructionNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TForInstructionNode.BeginParsing;
begin
  inherited;

  FInstruction := TSepiFor.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TForInstructionNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TInstructionNode then
    TInstructionNode(Child).InstructionList := Instruction.LoopInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TForInstructionNode.ChildEndParsing(Child: TSepiParseTreeNode);
const
  ControlVarChild = 0;
  StartValueChild = 1;
  ToDownToChild = 2;
  EndValueChild = 3;
var
  ControlVar: TSepiLocalVar;
  BoundValue: ISepiValue;
  ReadableValue: ISepiReadableValue;
begin
  case Child.IndexAsChild of
    // Control variable
    ControlVarChild:
    begin
      ControlVar := MethodCompiler.Locals.GetVarByName(Child.AsText);
      if ControlVar = nil then
      begin
        Child.MakeError(SLocalVarNameRequired);
        ControlVar := MethodCompiler.Locals.AddTempVar(SystemUnit.Integer);
      end;

      Instruction.ControlVar := ControlVar;
    end;

    // Start and end values
    StartValueChild, EndValueChild:
    begin
      BoundValue := (Child as TSepiExpressionNode).AsValue;

      if Supports(BoundValue, ISepiReadableValue, ReadableValue) then
      begin
        if Child.IndexAsChild = StartValueChild then
          Instruction.StartValue := ReadableValue
        else
          Instruction.EndValue := ReadableValue;
      end else
        Child.MakeError(SReadableValueRequired);
    end;

    // 'to' or 'downto' keyword
    ToDownToChild:
    begin
      Instruction.IsDownTo := (Child.SymbolClass = tkDownTo);
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TForInstructionNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{----------------------------------}
{ TExpressionInstructionNode class }
{----------------------------------}

{*
  Compile un appel
*}
procedure TExpressionInstructionNode.CompileCall;
var
  Instruction: TSepiCall;
  Callable: ISepiCallable;
begin
  if Supports((Children[0] as TSepiExpressionNode).Expression,
    ISepiCallable, Callable) then
  begin
    Instruction := TSepiCall.Create(MethodCompiler);
    Instruction.Callable := Callable;
    InstructionList.Add(Instruction);
  end else
  begin
    Children[0].MakeError(SCallableRequired);
  end;
end;

{*
  Compile une assignation
*}
procedure TExpressionInstructionNode.CompileAssignment;
var
  Instruction: TSepiAssignment;
  DestValue: ISepiWritableValue;
  SourceValue: ISepiReadableValue;
begin
  if not Supports((Children[0] as TSepiExpressionNode).AsValue,
    ISepiWritableValue, DestValue) then
  begin
    Children[0].MakeError(SWritableValueRequired);
  end else if not Supports((Children[1] as TSepiExpressionNode).AsValue,
    ISepiReadableValue, SourceValue) then
  begin
    Children[1].MakeError(SReadableValueRequired);
  end else
  begin
    Instruction := TSepiAssignment.Create(MethodCompiler);
    Instruction.Destination := DestValue;
    Instruction.Source := SourceValue;
    InstructionList.Add(Instruction);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TExpressionInstructionNode.EndParsing;
begin
  if ChildCount = 1 then
    CompileCall
  else
    CompileAssignment;

  inherited;
end;

{---------------------------}
{ TTryInstructionNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TTryInstructionNode.BeginParsing;
begin
  inherited;

  FTryInstructions := TSepiInstructionList.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TTryInstructionNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TNextTryInstructionNode then
  begin
    TNextTryInstructionNode(Child).TryInstructions := TryInstructions;
    TNextTryInstructionNode(Child).InstructionList := InstructionList;
  end else
    (Child as TInstructionNode).InstructionList := TryInstructions;
end;

{-------------------------}
{ TExceptClauseNode class }
{-------------------------}

{*
  [@inheritDoc]
*}
procedure TExceptClauseNode.BeginParsing;
var
  I: Integer;
begin
  inherited;

  FInstruction := TSepiTryExcept.Create(MethodCompiler);

  for I := 0 to TryInstructions.Count-1 do
    Instruction.TryInstructions.Add(TryInstructions[I]);
end;

{*
  [@inheritDoc]
*}
procedure TExceptClauseNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TInstructionNode).InstructionList :=
    Instruction.ExceptInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TExceptClauseNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{--------------------}
{ TMultiOnNode class }
{--------------------}

{*
  [@inheritDoc]
*}
procedure TMultiOnNode.BeginParsing;
begin
  inherited;

  FInstruction := TSepiMultiOn.Create(MethodCompiler);
  FInstruction.ExceptObjectVar :=
    (Parent as TExceptClauseNode).Instruction.UseTempVar;
end;

{*
  [@inheritDoc]
*}
procedure TMultiOnNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TMultiOnElseClauseNode then
    TMultiOnElseClauseNode(Child).InstructionList :=
      Instruction.ElseInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TMultiOnNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{---------------------}
{ TOnClauseNode class }
{---------------------}

{*
  Analyse la classe d'exception gérée par cette clause
  @param Node   Noeud qui contient le nom de la classe
*}
procedure TOnClauseNode.ParseExceptObjectClass(Node: TSepiParseTreeNode);
begin
  // Class handled in this on clause
  FExceptObjectClass := TSepiClass(LookForOrError(Node, TSepiClass,
    SClassTypeRequired));

  // Recover from error
  if FExceptObjectClass = nil then
    FExceptObjectClass := SystemUnit.TObject;

  // Create clause
  InstructionList := Instruction.AddOnClause(ExceptObjectClass);
end;

{*
  [@inheritDoc]
*}
procedure TOnClauseNode.BeginParsing;
begin
  inherited;

  FInstruction := (Parent as TMultiOnNode).Instruction;
end;

{*
  [@inheritDoc]
*}
procedure TOnClauseNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TInstructionNode then
  begin
    if ExceptObjectClass = nil then
    begin
      // There was no variable name
      ParseExceptObjectClass(Children[0]);
    end;

    TInstructionNode(Child).InstructionList := InstructionList;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TOnClauseNode.ChildEndParsing(Child: TSepiParseTreeNode);
const
  VarNameNode = 0;
  ExceptClassNode = 1;
begin
  if Child.SymbolClass = ntQualifiedIdent then
  begin
    case Child.IndexAsChild of
      VarNameNode:
      begin
        // Exception object variable name
        FExceptObjectVarName := Child.AsText;
      end;

      ExceptClassNode:
      begin
        ParseExceptObjectClass(Child);

        if Pos('.', ExceptObjectVarName) > 0 then
        begin
          Children[0].MakeError(SIdentifierRequired);
          FExceptObjectVarName := GetFirstToken(ExceptObjectVarName, '.');
        end;

        // Create exception object variable, with the good type
        FExceptObjectVar := MethodCompiler.Locals.AddAbsolute(
          ExceptObjectVarName, ExceptObjectClass, Instruction.ExceptObjectVar);
      end;
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TOnClauseNode.EndParsing;
begin
  inherited;
end;

{*
  [@inheritDoc]
*}
function TOnClauseNode.ResolveIdent(const Identifier: string): ISepiExpression;
begin
  if AnsiSameText(Identifier, ExceptObjectVarName) then
  begin
    // This is the except object variable
    Result := TSepiLocalVarValue.MakeValue(MethodCompiler,
      ExceptObjectVar) as ISepiExpression;
  end else
  begin
    // Inherited behavior
    Result := inherited ResolveIdent(Identifier);
  end;
end;

{------------------------------}
{ TMultiOnElseClauseNode class }
{------------------------------}

{*
  [@inheritDoc]
*}
procedure TMultiOnElseClauseNode.EndParsing;
begin
  if ChildCount = 0 then
    InstructionList.Add(TSepiReraise.Create(MethodCompiler));

  inherited;
end;

{--------------------------}
{ TFinallyClauseNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
procedure TFinallyClauseNode.BeginParsing;
var
  I: Integer;
begin
  inherited;

  FInstruction := TSepiTryFinally.Create(MethodCompiler);

  for I := 0 to TryInstructions.Count-1 do
    Instruction.TryInstructions.Add(TryInstructions[I]);
end;

{*
  [@inheritDoc]
*}
procedure TFinallyClauseNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TInstructionNode).InstructionList :=
    Instruction.FinallyInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TFinallyClauseNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{-----------------------------}
{ TRaiseInstructionNode class }
{-----------------------------}

{*
  Construit l'instruction raise avec un objet
*}
procedure TRaiseInstructionNode.MakeRaiseInstruction;
var
  Instruction: TSepiRaise;
  ExceptionValue: ISepiValue;
  ReadableValue: ISepiReadableValue;
begin
  // Get exception value
  ExceptionValue := (Children[0] as TSepiExpressionNode).AsValue(
    SystemUnit.TObject);

  // As readable value
  if not Supports(ExceptionValue, ISepiReadableValue, ReadableValue) then
  begin
    (ExceptionValue as ISepiExpression).MakeError(SClassTypeRequired);
    Exit;
  end;

  // Make instruction
  Instruction := TSepiRaise.Create(MethodCompiler);
  Instruction.ExceptionValue := ReadableValue;
  InstructionList.Add(Instruction);
end;

{*
  Construit l'instruction raise sans objet (reraise)
*}
procedure TRaiseInstructionNode.MakeReraiseInstruction;
var
  Instruction: TSepiReraise;
begin
  Instruction := TSepiReraise.Create(MethodCompiler);
  InstructionList.Add(Instruction);
end;

{*
  [@inheritDoc]
*}
procedure TRaiseInstructionNode.EndParsing;
begin
  if ChildCount > 0 then
    MakeRaiseInstruction
  else
    MakeReraiseInstruction;

  inherited;
end;

initialization
  InitNonTerminalClasses;
end.

