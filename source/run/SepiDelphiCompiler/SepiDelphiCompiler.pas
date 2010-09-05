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
  Classes de compilation d'une unité Delphi dans Sepi
  @author sjrd
  @version 1.0
*}
unit SepiDelphiCompiler;

interface

uses
  Windows, Types, SysUtils, Classes, StrUtils, TypInfo, SysConst, ScUtils,
  ScStrUtils, ScDelphiLanguage, SepiReflectionCore, SepiMembers, SepiOrdTypes,
  SepiStrTypes, SepiArrayTypes, SepiSystemUnit, SepiDelphiLexer,
  SepiDelphiParser, SepiCompilerErrors, SepiParseTrees, SepiCompiler, SepiCore,
  SepiCompilerConsts, SepiExpressions, SepiDelphiCompilerConsts, SepiOpCodes,
  SepiDelphiLikeCompilerUtils, SepiLL1ParserUtils, SepiInstructions,
  SepiCompilerUtils, SepiStdCompilerNodes;

type
  {*
    Noeud racine
    @author sjrd
    @version 1.0
  *}
  TDelphiRootNode = class(TSepiParseTreeRootNode)
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
  TDelphiInterfaceNode = class(TSepiNonTerminal)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud implémentation
    @author sjrd
    @version 1.0
  *}
  TDelphiImplementationNode = class(TSepiNonTerminal)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud d'expression d'initialisation
    @author sjrd
    @version 1.0
  *}
  TDelphiInitializationExpressionNode = class(TSepiInitializationExpressionNode)
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
  TDelphiArrayInitializationNode = class(TSepiInitializationExpressionNode)
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
  TDelphiRecordInitializationNode = class(TSepiInitializationExpressionNode)
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
  TDelphiGUIDInitializationNode = class(TSepiInitializationExpressionNode)
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
  TDelphiOtherInitializationNode = class(TSepiInitializationExpressionNode)
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
  TDelphiConstOrTypeNode = class(TSepiConstExpressionNode)
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
  TDelphiUnaryOpNode = class(TSepiDelphiLikeUnaryOpNode)
  protected
    function GetOperation: TSepiOperation; override;
  end;

  {*
    Noeud opérateur binaire
    @author sjrd
    @version 1.0
  *}
  TDelphiBinaryOpNode = class(TSepiDelphiLikeBinaryOpNode)
  protected
    function GetOperation: TSepiOperation; override;
  end;

  {*
    Noeud expression simple (sans opérateurs binaires)
    @author sjrd
    @version 1.0
  *}
  TDelphiSingleExprNode = class(TSepiExpressionWithModifiersNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud paramètres réels
    @author sjrd
    @version 1.0
  *}
  TDelphiParametersNode = class(TSepiDelphiLikeParametersNode)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud déclaration de type
    @author sjrd
    @version 1.0
  *}
  TDelphiTypeDeclNode = class(TSepiNonTerminal)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud déclaration de constante
    @author sjrd
    @version 1.0
  *}
  TDelphiConstantDeclNode = class(TSepiNonTerminal)
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
  TDelphiVariableDeclNode = class(TSepiNonTerminal)
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
  TDelphiTypeCloneNode = class(TSepiTypeDefinitionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type intervalle ou énumération
  *}
  TDelphiRangeOrEnumTypeNode = class(TSepiTypeDefinitionNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud descripteur d'un type intervalle
    @author sjrd
    @version 1.0
  *}
  TDelphiRangeTypeNode = class(TSepiTypeDefinitionNode)
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
  TDelphiEnumTypeNode = class(TSepiTypeDefinitionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type fausse énumération
    @author sjrd
    @version 1.0
  *}
  TDelphiFakeEnumTypeNode = class(TSepiTypeDefinitionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'une valeur d'un type fausse énumération
    @author sjrd
    @version 1.0
  *}
  TDelphiFakeEnumValueNode = class(TSepiNonTerminal)
  private
    FValue: TSepiFakeEnumValue; /// Valeur
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    property Value: TSepiFakeEnumValue read FValue;
  end;

  {*
    Noeud descripteur d'un type ensemble
    @author sjrd
    @version 1.0
  *}
  TDelphiSetTypeNode = class(TSepiTypeDefinitionNode)
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
  TDelphiStringTypeNode = class(TSepiTypeDefinitionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type pointeur
    @author sjrd
    @version 1.0
  *}
  TDelphiPointerTypeNode = class(TSepiTypeDefinitionNode)
  private
    function GetPointedType: TSepiType;

    procedure CMNotifyTypeCreated(var Msg: TCMNotifyTypeCreated);
      message CM_NOTIFYTYPECREATED;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type tableau
    @author sjrd
    @version 1.0
  *}
  TDelphiArrayTypeNode = class(TSepiTypeDefinitionNode)
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
  TDelphiPackedArrayTypeNode = class(TDelphiArrayTypeNode)
  end;

  {*
    Noeud descripteur d'un type record
    @author sjrd
    @version 1.0
  *}
  TDelphiRecordTypeNode = class(TSepiTypeDefinitionNode)
  private
    FIsPacked: Boolean; /// Indique si le record doit être packed

    FRecordType: TSepiRecordType; /// Type record
  protected
    function GetSepiContext: TSepiComponent; override;

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
  TDelphiPackedRecordTypeNode = class(TDelphiRecordTypeNode)
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); override;
  end;

  {*
    Noeud descripteur d'un type classe
    @author sjrd
    @version 1.0
  *}
  TDelphiClassTypeNode = class(TSepiTypeDefinitionNode)
  private
    FIsForwardClass: Boolean; /// True si c'est une classe forwardée
    FIsClass: Boolean;        /// True si c'est une classe
    FIsMetaClass: Boolean;    /// True si c'est une meta-classe

    FSepiClass: TSepiClass;         /// Classe Sepi compilée
    FSepiMetaClass: TSepiMetaClass; /// Component-classe compilée

    procedure CreateClass(ParentClass: TSepiClass = nil);
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    function GetSepiContext: TSepiComponent; override;
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
  TDelphiInterfaceTypeNode = class(TSepiTypeDefinitionNode)
  private
    FIsDispIntf: Boolean;        /// True si c'est une dispinterface
    FParentIntf: TSepiInterface; /// Interface parent
    FGUID: TGUID;                /// GUID de l'interface
    FSepiIntf: TSepiInterface;   /// Interface Sepi compilée

    procedure CreateInterface;
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    function GetSepiContext: TSepiComponent; override;
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
  TDelphiInterfaceGUIDNode = class(TSepiInitializationExpressionNode)
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
  TDelphiMethodRefTypeNode = class(TSepiTypeDefinitionNode)
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
  TDelphiOfObjectMarkerNode = class(TSepiNonTerminal)
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
    DestFileName, TDelphiRootNode, ntSource, TSepiDelphiLexer,
    TSepiDelphiParser);
end;

{*
  Initialise le tableau NonTerminalClasses
*}
procedure InitNonTerminalClasses;
begin
  NonTerminalClasses[ntSource]         := TDelphiRootNode;
  NonTerminalClasses[ntInterface]      := TDelphiInterfaceNode;
  NonTerminalClasses[ntImplementation] := TDelphiImplementationNode;
  NonTerminalClasses[ntUsesSection]    := TSepiUsesNode;

  NonTerminalClasses[ntCommaIdentDeclList] := TSepiIdentifierDeclListNode;
  NonTerminalClasses[ntQualifiedIdent]     := TSepiQualifiedIdentNode;
  NonTerminalClasses[ntIdentifierDecl]     := TSepiIdentifierDeclarationNode;

  NonTerminalClasses[ntTypeDecl]    := TDelphiTypeDeclNode;
  NonTerminalClasses[ntConstDecl]   := TDelphiConstantDeclNode;
  NonTerminalClasses[ntGlobalVar]   := TDelphiVariableDeclNode;
  NonTerminalClasses[ntRoutineDecl] := TSepiMethodDeclarationNode;

  NonTerminalClasses[ntInitializationExpression] :=
    TDelphiInitializationExpressionNode;
  NonTerminalClasses[ntArrayInitialization]  := TDelphiArrayInitializationNode;
  NonTerminalClasses[ntRecordInitialization] := TDelphiRecordInitializationNode;
  NonTerminalClasses[ntGUIDInitialization]   := TDelphiGUIDInitializationNode;
  NonTerminalClasses[ntOtherInitialization]  := TDelphiOtherInitializationNode;

  NonTerminalClasses[ntExpression]              := TSepiBinaryOpTreeNode;
  NonTerminalClasses[ntExpressionNoEquals]      := TSepiBinaryOpTreeNode;
  NonTerminalClasses[ntConstExpression]         := TSepiConstExpressionNode;
  NonTerminalClasses[ntConstExpressionNoEquals] := TSepiConstExpressionNode;
  NonTerminalClasses[ntConstOrType]             := TDelphiConstOrTypeNode;
  NonTerminalClasses[ntConstOrTypeNoEquals]     := TDelphiConstOrTypeNode;

  NonTerminalClasses[ntArithmeticLogicOp]         := TDelphiBinaryOpNode;
  NonTerminalClasses[ntArithmeticLogicOpNoEquals] := TDelphiBinaryOpNode;
  NonTerminalClasses[ntInOperation]               := TSepiInSetOperationNode;
  NonTerminalClasses[ntIsOperation]               := TSepiIsOperationNode;
  NonTerminalClasses[ntAsOperation]               := TSepiAsOperationNode;
  NonTerminalClasses[ntUnaryOp]                   := TDelphiUnaryOpNode;
  NonTerminalClasses[ntAddressOfOp]               := TSepiAddressOfOpNode;

  NonTerminalClasses[ntSingleExpr]        := TDelphiSingleExprNode;
  NonTerminalClasses[ntParenthesizedExpr] := TSepiSameAsChildExpressionNode;
  NonTerminalClasses[ntUnaryOpExpr]       := TSepiUnaryOperationNode;

  NonTerminalClasses[ntSingleValue]           := TSepiSameAsChildExpressionNode;
  NonTerminalClasses[ntIntegerConst]          := TSepiConstIntegerNode;
  NonTerminalClasses[ntFloatConst]            := TSepiConstFloatNode;
  NonTerminalClasses[ntStringConst]           := TSepiConstStringNode;
  NonTerminalClasses[ntIdentifierSingleValue] := TSepiIdentifierExpressionNode;
  NonTerminalClasses[ntInheritedExpression]   := TSepiInheritedExpressionNode;
  NonTerminalClasses[ntPureInheritedExpression] :=
    TSepiPureInheritedExpressionNode;
  NonTerminalClasses[ntNilValue]              := TSepiNilValueNode;
  NonTerminalClasses[ntSetValue]              := TSepiSetValueNode;
  NonTerminalClasses[ntCaseOfSetValue]        := TSepiSetValueNode;

  NonTerminalClasses[ntUnaryOpModifier]       := TSepiUnaryOpModifierNode;
  NonTerminalClasses[ntDereferenceOp]         := TSepiDereferenceOpNode;
  NonTerminalClasses[ntParameters]            := TDelphiParametersNode;
  NonTerminalClasses[ntSetOrOpenArrayBuilder] := TSepiSetOrOpenArrayBuilderNode;
  NonTerminalClasses[ntArrayIndices]          := TSepiArrayIndicesModifierNode;
  NonTerminalClasses[ntFieldSelection] := TSepiFieldSelectionModifierNode;

  NonTerminalClasses[ntTypeName] := TSepiTypeNameNode;

  NonTerminalClasses[ntCloneDesc]         := TDelphiTypeCloneNode;
  NonTerminalClasses[ntRangeOrEnumDesc]   := TDelphiRangeOrEnumTypeNode;
  NonTerminalClasses[ntRangeDesc]         := TDelphiRangeTypeNode;
  NonTerminalClasses[ntEnumDesc]          := TDelphiEnumTypeNode;
  NonTerminalClasses[ntFakeEnumDesc]      := TDelphiFakeEnumTypeNode;
  NonTerminalClasses[ntFakeEnumValue]     := TDelphiFakeEnumValueNode;
  NonTerminalClasses[ntSetDesc]           := TDelphiSetTypeNode;
  NonTerminalClasses[ntStringDesc]        := TDelphiStringTypeNode;
  NonTerminalClasses[ntPointerDesc]       := TDelphiPointerTypeNode;
  NonTerminalClasses[ntArrayDesc]         := TDelphiArrayTypeNode;
  NonTerminalClasses[ntPackedArrayDesc]   := TDelphiPackedArrayTypeNode;
  NonTerminalClasses[ntRecordDesc]        := TDelphiRecordTypeNode;
  NonTerminalClasses[ntPackedRecordDesc]  := TDelphiPackedRecordTypeNode;
  NonTerminalClasses[ntClassDesc]         := TDelphiClassTypeNode;
  NonTerminalClasses[ntInterfaceDesc]     := TDelphiInterfaceTypeNode;
  NonTerminalClasses[ntDispInterfaceDesc] := TDelphiInterfaceTypeNode;
  NonTerminalClasses[ntEventDesc]         := TDelphiMethodRefTypeNode;

  NonTerminalClasses[ntEventModifiers]  := TSepiChildThroughNonTerminal;
  NonTerminalClasses[ntEventIsOfObject] := TDelphiOfObjectMarkerNode;

  NonTerminalClasses[ntRecordContents]     := TSepiRecordContentsNode;
  NonTerminalClasses[ntRecordCaseBlock]    := TSepiRecordContentsNode;
  NonTerminalClasses[ntRecordCaseHeader]   := TSepiRecordCaseHeaderNode;
  NonTerminalClasses[ntRecordCaseContents] := TSepiRecordContentsNode;

  NonTerminalClasses[ntInterfaceGUID] := TDelphiInterfaceGUIDNode;

  NonTerminalClasses[ntRecordField]          := TSepiRecordFieldNode;
  NonTerminalClasses[ntRecordCaseField]      := TSepiRecordFieldNode;
  NonTerminalClasses[ntClassClassMethodProp] := TSepiClassMemberDefinitionNode;
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
  NonTerminalClasses[ntStaticMarker]      := TSepiStaticMarkerNode;

  NonTerminalClasses[ntMethodSignature]   := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntPropertySignature] := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntParam]             := TSepiParamNode;
  NonTerminalClasses[ntParamKind]         := TSepiParamKindNode;
  NonTerminalClasses[ntParamNameList]     := TSepiIdentifierDeclListNode;
  NonTerminalClasses[ntParamName]         := TSepiParamNameNode;
  NonTerminalClasses[ntParamIsArray]      := TSepiParamIsArrayMarkerNode;
  NonTerminalClasses[ntReturnType]        := TSepiSignatureReturnTypeNode;
  NonTerminalClasses[ntPropType]          := TSepiSignatureReturnTypeNode;

  NonTerminalClasses[ntPropertyKind]     := TSepiPropertyKindNode;
  NonTerminalClasses[ntRedefineMarker]   := TSepiPropRedefineMarkerNode;
  NonTerminalClasses[ntPropReadAccess]   := TSepiPropReadAccessNode;
  NonTerminalClasses[ntPropWriteAccess]  := TSepiPropWriteAccessNode;
  NonTerminalClasses[ntPropIndex]        := TSepiPropIndexNode;
  NonTerminalClasses[ntPropDefaultValue] := TSepiPropDefaultValueNode;
  NonTerminalClasses[ntPropStorage]      := TSepiPropStorageNode;
  NonTerminalClasses[ntDefaultMarker]    := TSepiPropDefaultMarkerNode;

  NonTerminalClasses[ntMethodImpl]       := TSepiMethodImplementationNode;
  NonTerminalClasses[ntMethodImplHeader] := TSepiMethodImplHeaderNode;
  NonTerminalClasses[ntMethodImplKind]   := TSepiSignatureKindNode;
  NonTerminalClasses[ntForwardMarker]    := TSepiForwardMarkerNode;
  NonTerminalClasses[ntMethodBody]       := TSepiMethodBodyNode;
  NonTerminalClasses[ntLocalVar]         := TSepiLocalVarNode;

  NonTerminalClasses[ntInstructionList]       := TSepiInstructionListNode;
  NonTerminalClasses[ntNoInstruction]         := TSepiNoInstructionNode;
  NonTerminalClasses[ntBeginEndBlock]         := TSepiBeginEndBlockNode;
  NonTerminalClasses[ntIfThenElseInstruction] := TSepiIfThenElseInstructionNode;
  NonTerminalClasses[ntCaseOfInstruction]     := TSepiCaseOfInstructionNode;
  NonTerminalClasses[ntCaseOfClause]          := TSepiCaseOfClauseNode;
  NonTerminalClasses[ntCaseOfElseClause]      := TSepiInstructionListNode;
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
  NonTerminalClasses[ntWithInstruction]       := TSepiWithInstructionNode;
  NonTerminalClasses[ntInnerWith]             := TSepiWithInstructionNode;

  NonTerminalClasses[ntExpressionInstruction] :=
    TSepiExecuteExpressionInstructionNode;
  NonTerminalClasses[ntExecutableExpression] := TSepiBinaryOpTreeNode;
  NonTerminalClasses[ntAssignmentOp]         := TSepiAssignmentOpNode;
end;

{-----------------------}
{ TDelphiRootNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
constructor TDelphiRootNode.Create(AClass: TSepiSymbolClass;
  ASepiRoot: TSepiRoot; AErrors: TSepiCompilerErrorList);
begin
  inherited;

  FMinEnumSize := mesByte;
end;

{*
  Gestionnaire de message CDM_MINENUMSIZE
  @param Msg   Message
*}
procedure TDelphiRootNode.CDMMinEnumSize(var Msg: TCDMMinEnumSize);
begin
  FMinEnumSize := Msg.MinEmumSize;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiRootNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  UnitName: string;
  SepiUnit: TSepiUnit;
begin
  if Child.SymbolClass = ntIdentifier then
  begin
    UnitName := Child.AsText;

    if AnsiSameText(UnitName, SystemUnitName) then
    begin
      Assert(SepiRoot.ChildCount = 0);
      SepiUnit := TSepiSystemUnit.Create(SepiRoot);
      TSepiSystemUnit(SepiUnit).CreateBuiltins;
    end else
    begin
      SepiUnit := TSepiUnit.Create(SepiRoot, UnitName, []);
    end;

    SetSepiUnit(SepiUnit, TSepiDelphiLanguageRules);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiRootNode.EndParsing;
begin
  SepiUnit.Complete;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TDelphiRootNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  Result := LanguageRules.ResolveIdent(SepiContext, Identifier);
end;

{----------------------------}
{ TDelphiInterfaceNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiInterfaceNode.BeginParsing;
begin
  SepiUnit.CurrentVisibility := mvPublic;
end;

{---------------------------------}
{ TDelphiImplementationNode class }
{---------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiImplementationNode.BeginParsing;
begin
  SepiUnit.CurrentVisibility := mvPrivate;
end;

{-------------------------------------------}
{ TDelphiInitializationExpressionNode class }
{-------------------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiInitializationExpressionNode.BeginParsing;
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
procedure TDelphiInitializationExpressionNode.ChildBeginParsing(
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
function TDelphiInitializationExpressionNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := (AValueType <> nil) and not (AValueType is TSepiDynArrayType);
end;

{--------------------------------------}
{ TDelphiArrayInitializationNode class }
{--------------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiArrayInitializationNode.BeginParsing;
begin
  inherited;

  FArrayType := ValueType as TSepiStaticArrayType;
  FElementType := ArrayType.ElementType;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiArrayInitializationNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
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
procedure TDelphiArrayInitializationNode.EndParsing;
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
function TDelphiArrayInitializationNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := AValueType is TSepiStaticArrayType;
end;

{---------------------------------------}
{ TDelphiRecordInitializationNode class }
{---------------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiRecordInitializationNode.BeginParsing;
begin
  inherited;

  FRecordType := ValueType as TSepiRecordType;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiRecordInitializationNode.ChildBeginParsing(
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
  Field := RecordType.GetComponent(FieldName) as TSepiField;

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
function TDelphiRecordInitializationNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := AValueType is TSepiRecordType;
end;

{-------------------------------------}
{ TDelphiGUIDInitializationNode class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiGUIDInitializationNode.BeginParsing;
begin
  inherited;

  FRecordType := ValueType as TSepiRecordType;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiGUIDInitializationNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TDelphiRecordInitializationNode then
    TDelphiRecordInitializationNode(Child).SetValueTypeAndPtr(
      ValueType, ValuePtr)
  else if Child is TSepiConstExpressionNode then
    TSepiConstExpressionNode(Child).ValueType := SystemUnit.LongString;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiGUIDInitializationNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
var
  GUIDStr: string;
begin
  if Child is TSepiConstExpressionNode then
  begin
    TSepiConstExpressionNode(Child).CompileConst(GUIDStr);
    try
      TGUID(ValuePtr^) := StringToGUID(GUIDStr);
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
function TDelphiGUIDInitializationNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := AValueType = SystemUnit.TGUID;
end;

{--------------------------------------}
{ TDelphiOtherInitializationNode class }
{--------------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiOtherInitializationNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TSepiConstExpressionNode).ValueType := ValueType;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiOtherInitializationNode.EndParsing;
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
function TDelphiOtherInitializationNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := (AValueType <> nil) and
    not ((AValueType is TSepiArrayType) or (AValueType is TSepiRecordType));
end;

{------------------------------}
{ TDelphiConstOrTypeNode class }
{------------------------------}

{*
  Indique si c'est un type
  @return True si c'est un type, False sinon
*}
function TDelphiConstOrTypeNode.GetIsType: Boolean;
begin
  Result := Supports(Expression, ISepiTypeExpression);
end;

{*
  [@inheritDoc]
*}
function TDelphiConstOrTypeNode.ValidateExpression: Boolean;
begin
  Result := IsType or (inherited ValidateExpression);
end;

{--------------------------}
{ TDelphiUnaryOpNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
function TDelphiUnaryOpNode.GetOperation: TSepiOperation;
begin
  case Children[0].SymbolClass of
    tkMinus:
      Result := opNegate;
    tkNot:
      Result := opNot;
  else
    Result := opAdd;
  end;
end;

{---------------------------}
{ TDelphiBinaryOpNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
function TDelphiBinaryOpNode.GetOperation: TSepiOperation;
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

{-----------------------------}
{ TDelphiSingleExprNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiSingleExprNode.EndParsing;
var
  WantingParams: ISepiWantingParams;
begin
  if Supports(Expression, ISepiWantingParams, WantingParams) and
    (not WantingParams.ParamsCompleted) then
    WantingParams.CompleteParams;

  inherited;
end;

{-----------------------------}
{ TDelphiParametersNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiParametersNode.BeginParsing;
begin
  inherited;

  if Supports(Base, ISepiIdentifierTestPseudoRoutine) then
    SetSymbolClass(ntIdentTestParam);
end;

{---------------------------}
{ TDelphiTypeDeclNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiTypeDeclNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiTypeDefinitionNode then
    TSepiTypeDefinitionNode(Child).SetTypeName(
      (Children[0] as TSepiIdentifierDeclarationNode).Identifier);
end;

{-------------------------------}
{ TDelphiConstantDeclNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiConstantDeclNode.ChildBeginParsing(Child: TSepiParseTreeNode);
var
  InitChild: TDelphiInitializationExpressionNode;
begin
  inherited;

  if Child is TDelphiInitializationExpressionNode then
  begin
    Assert(ConstVar <> nil);

    InitChild := TDelphiInitializationExpressionNode(Child);
    InitChild.SetValueTypeAndPtr(ConstVar.VarType, ConstVar.Value);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiConstantDeclNode.ChildEndParsing(Child: TSepiParseTreeNode);
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

    if ReadableValue.ValueType is TSepiCompilerTransientType then
    begin
      Child.MakeError(SCompilerTransientTypeForbidden);
      ReadableValue := TSepiErroneousValue.Create(SepiRoot);
      ReadableValue.AttachToExpression(MakeExpression);
    end;

    FConstant := TSepiConstant.Create(SepiContext, Name,
      ReadableValue.ValueType, ReadableValue.ConstValuePtr^);
  end;

  inherited;
end;

{-------------------------------}
{ TDelphiVariableDeclNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
destructor TDelphiVariableDeclNode.Destroy;
begin
  FNames.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiVariableDeclNode.ChildBeginParsing(Child: TSepiParseTreeNode);
var
  InitChild: TDelphiInitializationExpressionNode;
begin
  inherited;

  if Child is TDelphiInitializationExpressionNode then
  begin
    Assert(Variable <> nil);

    InitChild := TDelphiInitializationExpressionNode(Child);
    InitChild.SetValueTypeAndPtr(Variable.VarType, Variable.Value);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiVariableDeclNode.ChildEndParsing(Child: TSepiParseTreeNode);
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
procedure TDelphiTypeCloneNode.EndParsing;
var
  OldType: TSepiType;
begin
  OldType := TSepiType(LookForOrError(Children[0], TSepiType,
    STypeIdentifierRequired));

  if OldType <> nil then
  begin
    if ChildCount >= 2 then
    begin
      // Definition of an AnsiString with a given code page
      if OldType <> SystemUnit.AnsiString then
        Children[1].MakeError(SCodePageSpecificationForbidden);

      SetSepiType(TSepiStringType.Create(SepiContext, TypeName, skAnsiString,
        (Children[1] as TSepiConstExpressionNode).AsInteger));
    end else
    begin
      // Regular clone
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
  end;

  inherited;
end;

{----------------------------------}
{ TDelphiRangeOrEnumTypeNode class }
{----------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiRangeOrEnumTypeNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TSepiTypeDefinitionNode).SetTypeName(TypeName);
end;

{*
  [@inheritDoc]
*}
procedure TDelphiRangeOrEnumTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  SetSepiType((Child as TSepiTypeDefinitionNode).SepiType);

  inherited;
end;

{----------------------------}
{ TDelphiRangeTypeNode class }
{----------------------------}

{*
  Construit le type
  @param TypeName      Nom du type à créer
  @param BaseType      Type de base
  @param LowerValue    Valeur basse
  @param HigherValue   Valeur haute
*}
function TDelphiRangeTypeNode.MakeType(const TypeName: string;
  BaseType: TSepiType; const LowerValue, HigherValue): TSepiType;
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
procedure TDelphiRangeTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
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
procedure TDelphiRangeTypeNode.EndParsing;
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

{---------------------------}
{ TDelphiEnumTypeNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiEnumTypeNode.EndParsing;
var
  I: Integer;
  Values: array of string;
begin
  SetLength(Values, ChildCount);

  for I := 0 to ChildCount-1 do
    Values[I] := Children[I].AsText;

  SetSepiType(TSepiEnumType.Create(SepiContext, TypeName, Values,
    (RootNode as TDelphiRootNode).MinEnumSize));

  inherited;
end;

{-------------------------------}
{ TDelphiFakeEnumTypeNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiFakeEnumTypeNode.EndParsing;
var
  I: Integer;
  Values: TSepiFakeEnumValueDynArray;
begin
  SetLength(Values, ChildCount);

  for I := 0 to ChildCount-1 do
    Values[I] := (Children[I] as TDelphiFakeEnumValueNode).Value;

  SetSepiType(TSepiFakeEnumType.Create(SepiContext, TypeName, Values,
    (RootNode as TDelphiRootNode).MinEnumSize));

  inherited;
end;

{--------------------------------}
{ TDelphiFakeEnumValueNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiFakeEnumValueNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if ChildCount = 1 then
  begin
    FValue.Name := Child.AsText;
    FValue.Value := -1;
  end else
  begin
    FValue.Value := (Child as TSepiConstExpressionNode).AsInteger;
  end;

  inherited;
end;

{--------------------------}
{ TDelphiSetTypeNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiSetTypeNode.MakeErroneousType;
begin
  SetSepiType(TSepiSetType.Create(SepiContext, TypeName, SystemUnit.Byte));
end;

{*
  Teste si un type donné est valide comme type d'élément d'ensemble
  @param CompType   Type de composant à tester
  @return True s'il est valide, False sinon
*}
function TDelphiSetTypeNode.IsValidCompType(CompType: TSepiType): Boolean;
begin
  Result := (CompType is TSepiOrdType) and
    (TSepiOrdType(CompType).MaxValue - TSepiOrdType(CompType).MinValue < 256);
end;

{*
  [@inheritDoc]
*}
procedure TDelphiSetTypeNode.EndParsing;
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

{-----------------------------}
{ TDelphiStringTypeNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiStringTypeNode.EndParsing;
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

{------------------------------}
{ TDelphiPointerTypeNode class }
{------------------------------}

{*
  Récupère le type pointé
  @return Type pointé, ou nil si non trouvé
*}
function TDelphiPointerTypeNode.GetPointedType: TSepiType;
begin
  Result := TSepiType(LookFor(Children[0], TSepiType));
end;

{*
  Gestionnaire du message CM_NOTIFYTYPECREATED
  @param Msg   Message
*}
procedure TDelphiPointerTypeNode.CMNotifyTypeCreated(
  var Msg: TCMNotifyTypeCreated);
var
  PointedType: TSepiType;
begin
  PointedType := GetPointedType;

  if PointedType <> nil then
  begin
    TSepiPointerType(SepiType).Create(SepiContext, TypeName, PointedType);
    RootNode.UnregisterMessageHandler(CM_NOTIFYTYPECREATED, Self);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiPointerTypeNode.EndParsing;
var
  PointedType: TSepiType;
begin
  PointedType := GetPointedType;

  if PointedType <> nil then
    SetSepiType(TSepiPointerType.Create(SepiContext, TypeName, PointedType))
  else
  begin
    SetSepiType(TSepiPointerType.ForwardDecl(SepiContext, TypeName));
    RootNode.RegisterMessageHandler(CM_NOTIFYTYPECREATED, Self);
  end;

  inherited;
end;

{----------------------------}
{ TDelphiArrayTypeNode class }
{----------------------------}

{*
  Crée un tableau statique d'après une définition par intervalle
  @param TypeName      Nom du type tableau à créer
  @param RangeNode     Noeud intervalle
  @param ElementType   Type des éléments
  @return Type tableau créé
*}
function TDelphiArrayTypeNode.RangeDefinition(const TypeName: string;
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
function TDelphiArrayTypeNode.TypedDefinition(const TypeName: string;
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
procedure TDelphiArrayTypeNode.EndParsing;
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

{-----------------------------}
{ TDelphiRecordTypeNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
function TDelphiRecordTypeNode.GetSepiContext: TSepiComponent;
begin
  if RecordType <> nil then
    Result := RecordType
  else
    Result := inherited GetSepiContext;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiRecordTypeNode.BeginParsing;
begin
  inherited;

  FRecordType := TSepiRecordType.Create(SepiContext, TypeName, IsPacked);
end;

{*
  [@inheritDoc]
*}
procedure TDelphiRecordTypeNode.EndParsing;
begin
  RecordType.Complete;
  SetSepiType(RecordType);

  inherited;
end;

{-----------------------------------}
{ TDelphiPackedRecordTypeNode class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
constructor TDelphiPackedRecordTypeNode.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition);
begin
  inherited;

  IsPacked := True;
end;

{----------------------------}
{ TDelphiClassTypeNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiClassTypeNode.CreateClass(ParentClass: TSepiClass = nil);
var
  Context: TSepiComponent;
begin
  Context := SepiContext; // Save context before FSepiClass is set
  FSepiClass := Context.GetComponent(TypeName) as TSepiClass;

  if FSepiClass <> nil then
    FSepiClass.Create(Context, TypeName, ParentClass)
  else
    FSepiClass := TSepiClass.Create(Context, TypeName, ParentClass);
end;

{*
  [@inheritDoc]
*}
function TDelphiClassTypeNode.GetSepiContext: TSepiComponent;
begin
  if SepiClass <> nil then
    Result := SepiClass
  else
    Result := inherited GetSepiContext;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiClassTypeNode.ChildBeginParsing(Child: TSepiParseTreeNode);
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
procedure TDelphiClassTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  I: Integer;
  ParentClass: TSepiClass;
  ImplementedIntf: TSepiInterface;
  ReferencedClass: TSepiComponent;
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

    ReferencedClass := LookFor(Child);

    if ReferencedClass = nil then
    begin
      // Auto forward decl
      ReferencedClass := TSepiClass.ForwardDecl(SepiContext, Child.AsText);
    end else if not (ReferencedClass is TSepiClass) then
    begin
      Child.MakeError(SClassTypeRequired);
      ReferencedClass := SystemUnit.TObject;
    end;

    FSepiMetaClass := TSepiMetaClass.Create(SepiContext, TypeName,
      TSepiClass(ReferencedClass));
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiClassTypeNode.EndParsing;
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
function TDelphiClassTypeNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  if SepiClass <> nil then
    Result := LanguageRules.ResolveIdent(SepiContext, Identifier)
  else
    Result := inherited ResolveIdent(Identifier);
end;

{--------------------------------}
{ TDelphiInterfaceTypeNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiInterfaceTypeNode.CreateInterface;
var
  Context: TSepiComponent;
begin
  Context := SepiContext; // Save context before FSepiIntf is set
  FSepiIntf := Context.GetComponent(TypeName) as TSepiInterface;

  if FSepiIntf <> nil then
    FSepiIntf.Create(Context, TypeName, ParentIntf, GUID, IsDispIntf)
  else
    FSepiIntf := TSepiInterface.Create(Context, TypeName, ParentIntf, GUID,
      IsDispIntf);
end;

{*
  [@inheritDoc]
*}
function TDelphiInterfaceTypeNode.GetSepiContext: TSepiComponent;
begin
  if SepiIntf <> nil then
    Result := SepiIntf
  else
    Result := inherited GetSepiContext;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiInterfaceTypeNode.BeginParsing;
begin
  inherited;

  FIsDispIntf := SymbolClass = ntDispInterfaceDesc;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiInterfaceTypeNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TDelphiInterfaceGUIDNode then
  begin
    TDelphiInterfaceGUIDNode(Child).SetValuePtr(@FGUID);
  end else if Child.SymbolClass = ntInterfaceMemberList then
  begin
    CreateInterface;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiInterfaceTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
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
procedure TDelphiInterfaceTypeNode.EndParsing;
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
function TDelphiInterfaceTypeNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  if SepiIntf <> nil then
    Result := LanguageRules.ResolveIdent(SepiContext, Identifier)
  else
    Result := inherited ResolveIdent(Identifier);
end;

{--------------------------------}
{ TDelphiInterfaceGUIDNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TDelphiInterfaceGUIDNode.ChildEndParsing(Child: TSepiParseTreeNode);
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
procedure TDelphiInterfaceGUIDNode.SetValuePtr(AValuePtr: PGUID);
begin
  SetValueTypeAndPtr(SystemUnit.TGUID, AValuePtr);
end;

{--------------------------------}
{ TDelphiMethodRefTypeNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
destructor TDelphiMethodRefTypeNode.Destroy;
begin
  FSignature.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TDelphiMethodRefTypeNode.BeginParsing;
begin
  inherited;

  FSignature := TSepiSignature.CreateConstructing(SepiUnit);
end;

{*
  [@inheritDoc]
*}
procedure TDelphiMethodRefTypeNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiSignatureBuilderNode then
    TSepiSignatureBuilderNode(Child).SetSignature(Signature);
end;

{*
  [@inheritDoc]
*}
procedure TDelphiMethodRefTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TDelphiOfObjectMarkerNode then
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
procedure TDelphiMethodRefTypeNode.EndParsing;
begin
  Signature.Complete;
  SetSepiType(TSepiMethodRefType.Create(SepiContext, TypeName, Signature));

  inherited;
end;

initialization
  InitNonTerminalClasses;
end.

