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
    Noeud expression constante ou type
    @author sjrd
    @version 1.0
  *}
  TConstOrTypeNode = class(TSepiConstExpressionNode)
  private
    function GetIsType: Boolean;
  protected
    function ValidateExpression: Boolean; override;
  public
    property IsType: Boolean read GetIsType;
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
  TBinaryOpNode = class(TSepiDelphiLikeBinaryOpNode)
  protected
    function GetOperation: TSepiOperation; override;
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
      RangeNode: TSepiParseTreeNode;
      ElementType: TSepiType): TSepiStaticArrayType;
    function TypedDefinition(const TypeName: string;
      IndexNode: TSepiConstExpressionNode;
      ElementType: TSepiType): TSepiStaticArrayType;
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
    Noeud marqueur of object
    @author sjrd
    @version 1.0
  *}
  TOfObjectMarkerNode = class(TSepiNonTerminal)
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
  NonTerminalClasses[ntUsesSection]    := TSepiUsesNode;

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
  NonTerminalClasses[ntConstExpression]         := TSepiConstExpressionNode;
  NonTerminalClasses[ntConstExpressionNoEquals] := TSepiConstExpressionNode;
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
  NonTerminalClasses[ntSetValue]              := TSepiSetValueNode;

  NonTerminalClasses[ntParameters]        := TParametersNode;
  NonTerminalClasses[ntArrayIndices]      := TSepiArrayIndicesModifierNode;
  NonTerminalClasses[ntFieldSelection]    := TSepiFieldSelectionModifierNode;
  NonTerminalClasses[ntDereference]       := TSepiDereferenceModifierNode;

  NonTerminalClasses[ntTypeName] := TSepiTypeNameNode;

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

  NonTerminalClasses[ntEventModifiers]  := TSepiChildThroughNonTerminal;
  NonTerminalClasses[ntEventIsOfObject] := TOfObjectMarkerNode;

  NonTerminalClasses[ntRecordContents]     := TSepiRecordContentsNode;
  NonTerminalClasses[ntRecordCaseBlock]    := TSepiRecordContentsNode;
  NonTerminalClasses[ntRecordCaseContents] := TSepiRecordContentsNode;

  NonTerminalClasses[ntInterfaceGUID] := TInterfaceGUIDNode;

  NonTerminalClasses[ntRecordField]          := TSepiRecordFieldNode;
  NonTerminalClasses[ntRecordCaseField]      := TSepiRecordFieldNode;
  NonTerminalClasses[ntField]                := TSepiClassFieldNode;
  NonTerminalClasses[ntIntfMethodRedirector] := TSepiIntfMethodRedirectorNode;
  NonTerminalClasses[ntMethodDecl]           := TSepiMethodDeclarationNode;
  NonTerminalClasses[ntPropertyDecl]         := TSepiPropertyNode;
  NonTerminalClasses[ntVisibility]           := TSepiChangeVisibilityNode;

  NonTerminalClasses[ntMethodNameDeclaration] :=
    TSepiUncheckedIdentifierDeclNode;
  NonTerminalClasses[ntRoutineKind]       := TSepiSignatureKindNode;
  NonTerminalClasses[ntMethodKind]        := TSepiSignatureKindNode;
  NonTerminalClasses[ntCallingConvention] := TSepiCallingConventionNode;
  NonTerminalClasses[ntMethodLinkKind]    := TSepiMethodLinkKindNode;
  NonTerminalClasses[ntAbstractMarker]    := TSepiAbstractMarkerNode;
  NonTerminalClasses[ntOverloadMarker]    := TSepiOverloadMarkerNode;

  NonTerminalClasses[ntMethodSignature]   := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntPropertySignature] := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntParam]             := TSepiParamNode;
  NonTerminalClasses[ntParamKind]         := TSepiParamKindNode;
  NonTerminalClasses[ntParamNameList]     := TSepiIdentifierDeclListNode;
  NonTerminalClasses[ntParamName]         := TSepiParamNameNode;
  NonTerminalClasses[ntParamIsArray]      := TSepiParamIsArrayMarkerNode;
  NonTerminalClasses[ntReturnType]        := TSepiSignatureReturnTypeNode;
  NonTerminalClasses[ntPropType]          := TSepiSignatureReturnTypeNode;

  NonTerminalClasses[ntRedefineMarker]   := TSepiPropRedefineMarkerNode;
  NonTerminalClasses[ntPropReadAccess]   := TSepiPropReadAccessNode;
  NonTerminalClasses[ntPropWriteAccess]  := TSepiPropWriteAccessNode;
  NonTerminalClasses[ntPropIndex]        := TSepiPropIndexNode;
  NonTerminalClasses[ntPropDefaultValue] := TSepiPropDefaultValueNode;
  NonTerminalClasses[ntPropStorage]      := TSepiPropStorageNode;
  NonTerminalClasses[ntDefaultMarker]    := TSepiPropDefaultMarkerNode;

  NonTerminalClasses[ntMethodImpl]       := TSepiMethodImplementationNode;
  NonTerminalClasses[ntMethodImplHeader] := TSepiMethodImplHeaderNode;
  NonTerminalClasses[ntForwardMarker]    := TSepiForwardMarkerNode;
  NonTerminalClasses[ntMethodBody]       := TSepiMethodBodyNode;
  NonTerminalClasses[ntLocalVar]         := TSepiLocalVarNode;

  NonTerminalClasses[ntInstructionList]       := TSepiInstructionListNode;
  NonTerminalClasses[ntNoInstruction]         := TSepiNoInstructionNode;
  NonTerminalClasses[ntBeginEndBlock]         := TSepiBeginEndBlockNode;
  NonTerminalClasses[ntIfThenElseInstruction] := TSepiIfThenElseInstructionNode;
  NonTerminalClasses[ntWhileInstruction]      := TSepiWhileInstructionNode;
  NonTerminalClasses[ntRepeatInstruction] :=
    TSepiRepeatUntilInstructionNode;
  NonTerminalClasses[ntForInstruction]        := TSepiForInstructionNode;
  NonTerminalClasses[ntForControlVar]         := TSepiForControlVarNode;
  NonTerminalClasses[ntForTo]                 := TSepiForToNode;
  NonTerminalClasses[ntForDownTo]             := TSepiForDownToNode;
  NonTerminalClasses[ntTryInstruction]        := TSepiTryInstructionNode;
  NonTerminalClasses[ntExceptClause]          := TSepiExceptClauseNode;
  NonTerminalClasses[ntMultiOn]               := TSepiMultiOnNode;
  NonTerminalClasses[ntOnClause]              := TSepiOnClauseNode;
  NonTerminalClasses[ntExceptionVarAndType]   := TSepiExceptionVarAndTypeNode;
  NonTerminalClasses[ntMultiOnElseClause]     := TSepiMultiOnElseClauseNode;
  NonTerminalClasses[ntFinallyClause]         := TSepiFinallyClauseNode;
  NonTerminalClasses[ntRaiseInstruction]      := TSepiRaiseInstructionNode;
  NonTerminalClasses[ntExpressionInstruction] := TSepiExpressionInstructionNode;
  NonTerminalClasses[ntCallInstruction]       := TSepiCallInstructionNode;
  NonTerminalClasses[ntAssignmentInstruction] := TSepiAssignmentInstructionNode;
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

  (Child as TSepiConstExpressionNode).ValueType := ValueType;
end;

{*
  [@inheritDoc]
*}
procedure TOtherInitializationNode.EndParsing;
var
  ReadableValue: ISepiReadableValue;
begin
  ReadableValue := (Children[0] as TSepiConstExpressionNode).AsValue(
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

{------------------------}
{ TConstOrTypeNode class }
{------------------------}

{*
  Indique si c'est un type
  @return True si c'est un type, False sinon
*}
function TConstOrTypeNode.GetIsType: Boolean;
begin
  Result := Supports(Expression, ISepiTypeExpression);
end;

{*
  [@inheritDoc]
*}
function TConstOrTypeNode.ValidateExpression: Boolean;
begin
  Result := IsType or (inherited ValidateExpression);
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

{*
  [@inheritDoc]
*}
function TBinaryOpNode.GetOperation: TSepiOperation;
const
  SymbolClassToOperation: array[tkPlus..tkNotEqual] of TSepiOperation = (
    opAdd, opSubtract, opMultiply, opDivide, opIntDivide, opModulus,
    opShiftLeft, opShiftRight, opOr, opAnd, opXor, 0,
    opCmpLT, opCmpLE, opCmpGT, opCmpGE, opCmpNE
  );
var
  SymbolClass: TSepiSymbolClass;
begin
  SymbolClass := Children[0].SymbolClass;

  Assert(SymbolClass in ([tkEquals, tkPlus..tkNotEqual]-[tkNot]));

  if SymbolClass = tkEquals then
    Result := opCmpEQ
  else
    Result := SymbolClassToOperation[SymbolClass];
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
    Assert(ConstVar <> nil);

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
  end else if Child is TSepiConstExpressionNode then
  begin
    ReadableValue := TSepiConstExpressionNode(Child).AsReadableValue;
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
    Expression := (Children[0] as TSepiConstExpressionNode).Expression;

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
    LowerValue  := (Children[0] as TSepiConstExpressionNode).AsReadableValue;
    HigherValue := (Children[1] as TSepiConstExpressionNode).AsReadableValue;

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
    MaxLength := (Children[1] as TSepiConstExpressionNode).AsInteger(255);

    if (MaxLength < 0) or (MaxLength > 255) then
    begin
      Children[1].MakeError(SBadStringLength);
      MaxLength := 255;
    end;

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
    (RangeNode.Children[0] as TSepiConstExpressionNode).AsReadableValue;
  HigherValue :=
    (RangeNode.Children[1] as TSepiConstExpressionNode).AsReadableValue;

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
  IndexNode: TSepiConstExpressionNode;
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
            RangeNode.Children[0] as TSepiConstExpressionNode, ElementType);
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
  else if Child.SymbolClass = ntClassMemberLists then
  begin
    Assert((not IsMetaClass) or (not IsForwardClass));

    if not IsClass then
    begin
      FIsClass := True;
      CreateClass;
    end;
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
  end else if Child.SymbolClass = ntInterfaceMemberList then
  begin
    CreateInterface;
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
  ConstExpr: TSepiConstExpressionNode;
  GUIDStr: string;
begin
  ConstExpr := Child as TSepiConstExpressionNode;

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
  if Child is TOfObjectMarkerNode then
  begin
    case Signature.Kind of
      skStaticProcedure:
        Signature.Kind := skObjectProcedure;
      skStaticFunction:
        Signature.Kind := skObjectFunction;
    else
      Child.MakeError(SDuplicatedOfObjectMarker);
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TMethodRefTypeNode.EndParsing;
begin
  Signature.Complete;
  SetSepiType(TSepiMethodRefType.Create(SepiContext, TypeName, Signature));

  inherited;
end;

initialization
  InitNonTerminalClasses;
end.

