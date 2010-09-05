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
  Classes de compilation standard d'une unité Sepi
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
  SepiParseTrees, SepiCompilerUtils, SepiDelphiLikeCompilerUtils;

const
  CM_NOTIFYTYPECREATED = $11;

type
  TCMNotifyTypeCreated = record
    MsgID: Word;         /// ID du message - CM_NOTIFYTYPECREATED
    SepiType: TSepiType; /// Type créé
  end;

  {*
    Noeud section uses
    Chaque enfant d'un noeud section uses doit être un noeud dont le AsText est
    un nom d'unité. Chaque unité ainsi référencée est ajoutée aux uses de
    l'unité en cours de compilation.
    @author sjrd
    @version 1.0
  *}
  TSepiUsesNode = class(TSepiNonTerminal)
  protected
    function IsRedeclared(const UnitName: string): Boolean; virtual;
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Identificateur qualifié
    Chaque enfant d'un identificateur qualifié est un des qualificateurs. Le
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
    une valeur connue à la compilation, et dont le type est déjà connu.
    Avant son BeginParsing, la méthode SetValueTypeAndPtr doit être appelée pour
    spécifier quel est le type de valeur attendu, et où placer le résultat.
    Les types acceptés par une instance particulière de
    TSepiInitializationExpressionNode peuvent être testés avec IsValidType. Par
    défaut, TSepiInitializationExpressionNode accepte tous les types non-nil.
    @author sjrd
    @version 1.0
  *}
  TSepiInitializationExpressionNode = class(TSepiNonTerminal)
  private
    FValueType: TSepiType; /// Type de valeur
    FValuePtr: Pointer;    /// Pointeur sur la valeur à initialiser

    FOwnsValue: Boolean; /// True si possède la valeur dans FValuePtr
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
    FExpression: ISepiExpression; /// Expression compilée

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
    function AsComponent: TSepiComponent;
    function AsType: TSepiType;

    property Expression: ISepiExpression read FExpression;
  end;

  {*
    Noeud expression qui vaut la même expression que son unique fils
    Un exemple d'un tel noeud est l'expression parenthésée, qui se décrit comme
      '('* Expression ')'*
    Sa fonction est uniquement syntaxique, et sa valeur se récupère directement
    de son unique fils.
    @author sjrd
    @version 1.0
  *}
  TSepiSameAsChildExpressionNode = class(TSepiExpressionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud expression constante
    @author sjrd
    @version 1.0
  *}
  TSepiConstExpressionNode = class(TSepiSameAsChildExpressionNode)
  private
    FValueType: TSepiType; /// Type de valeur censée être contenue dans ce noeud
  protected
    function ValidateExpression: Boolean; override;
    procedure MakeErroneousExpression; override;
  public
    function CompileConst(var Value;
      ValueType: TSepiType = nil): Boolean; virtual;

    function AsInteger(DefaultValue: Integer = 0): Integer;

    property ValueType: TSepiType read FValueType write FValueType;
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
    Sens d'évaluation d'un opérateur binaire
    - bodLeftToRight : évalué de gauche à droite (comme la soustraction) ;
    - bodRightToLeft : évalué de droite à gauche (comme le = du C).
  *}
  TSepiBinaryOpDirection = (bodLeftToRight, bodRightToLeft);

  {*
    Noeud représentant un opérateur binaire
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
    Noeud représentant un opérateur binaire du style Delphi
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiLikeBinaryOpNode = class(TSepiBinaryOpNode)
  protected
    function TryAndHandlePointerArithm(
      const PtrValue, IntValue: ISepiReadableValue;
      out Expression: ISepiExpression): Boolean; virtual;

    procedure HandleIntDivIntTrick(
      var LeftValue, RightValue: ISepiReadableValue); virtual;

    function GetPriority: Integer; override;
    function GetOperation: TSepiOperation; virtual; abstract;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;

    property Operation: TSepiOperation read GetOperation;
  end;

  {*
    Noeud représentant un opérateur in
    @author sjrd
    @version 1.0
  *}
  TSepiInSetOperationNode = class(TSepiBinaryOpNode)
  protected
    function GetPriority: Integer; override;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud représentant un opérateur is
    @author sjrd
    @version 1.0
  *}
  TSepiIsOperationNode = class(TSepiBinaryOpNode)
  protected
    function GetPriority: Integer; override;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud représentant un opérateur as
    @author sjrd
    @version 1.0
  *}
  TSepiAsOperationNode = class(TSepiBinaryOpNode)
  protected
    function GetPriority: Integer; override;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud représentant un opérateur binaire d'assignation
    @author sjrd
    @version 1.0
  *}
  TSepiAssignmentOpNode = class(TSepiBinaryOpNode)
  protected
    function GetPriority: Integer; override;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud expression calculé par un arbre d'opérations binaires
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
    Noeud représentant un opérateur unaire
    @author sjrd
    @version 1.0
  *}
  TSepiUnaryOpNode = class(TSepiNonTerminal)
  public
    function MakeOperation(
      const Operand: ISepiExpression): ISepiExpression; virtual; abstract;
  end;

  {*
    Noeud représentant un opérateur unaire du style Delphi
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiLikeUnaryOpNode = class(TSepiUnaryOpNode)
  protected
    function GetOperation: TSepiOperation; virtual; abstract;
  public
    function MakeOperation(
      const Operand: ISepiExpression): ISepiExpression; override;

    property Operation: TSepiOperation read GetOperation;
  end;

  {*
    Noeud représentant un opérateur unaire de déréférencement
    @author sjrd
    @version 1.0
  *}
  TSepiDereferenceOpNode = class(TSepiUnaryOpNode)
  public
    function MakeOperation(
      const Operand: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud représentant un opérateur unaire d'adressage
    @author sjrd
    @version 1.0
  *}
  TSepiAddressOfOpNode = class(TSepiUnaryOpNode)
  public
    function MakeOperation(
      const Operand: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud expression calculé par une opération unaire
    Un noeud de ce type doit posséder exactement deux enfants. L'un des deux
    doit être de type TSepiUnaryOpNode et l'autre de type TSepiExpressionNode.
    @author sjrd
    @version 1.0
  *}
  TSepiUnaryOperationNode = class(TSepiExpressionNode)
  protected
    function CompileOperation(OperatorNode: TSepiUnaryOpNode;
      OperandNode: TSepiExpressionNode): ISepiExpression;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud représentant une constante littérale
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
    Noeud représentant une constante littérale entière
    @author sjrd
    @version 1.0
  *}
  TSepiConstIntegerNode = class(TSepiLiteralConstNode)
  protected
    function CompileAsInteger: Int64; virtual;

    function CompileAsValue: ISepiValue; override;
  end;

  {*
    Noeud représentant une constante littérale flottante
    @author sjrd
    @version 1.0
  *}
  TSepiConstFloatNode = class(TSepiLiteralConstNode)
  protected
    function CompileAsExtended: Extended; virtual;

    function CompileAsValue: ISepiValue; override;
  end;

  {*
    Noeud représentant une constante littérale chaîne de caractères
    @author sjrd
    @version 1.0
  *}
  TSepiConstStringNode = class(TSepiLiteralConstNode)
  protected
    function CompileAsString: string; virtual;

    function CompileAsValue: ISepiValue; override;
  end;

  {*
    Noeud représentant un identificateur à résoudre
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
    Noeud représentant la valeur spéciale nil
    @author sjrd
    @version 1.0
  *}
  TSepiNilValueNode = class(TSepiExpressionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud valeur ensemble
    @author sjrd
    @version 1.0
  *}
  TSepiSetValueNode = class(TSepiExpressionNode)
  private
    FSetBuilder: ISepiSetBuilder; /// Constructeur d'ensemble
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    property SetBuilder: ISepiSetBuilder read FSetBuilder;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;
  end;

  {*
    Noeud constructeur d'ensemble ou de tableau ouvert
    @author sjrd
    @version 1.0
  *}
  TSepiSetOrOpenArrayBuilderNode = class(TSepiExpressionNode)
  private
    FBuilder: ISepiSetOrOpenArrayBuilder; /// Constructeur
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    property Builder: ISepiSetOrOpenArrayBuilder read FBuilder;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;
  end;

  {*
    Noeud représentant une expression d'appel inherited
    @author sjrd
    @version 1.0
  *}
  TSepiInheritedExpressionNode = class(TSepiExpressionNode)
  protected
    function RequireClassOrObjectMethod: Boolean;

    function GetMethodName: string; virtual;
    function FindMethod: TSepiMethodBase; virtual;

    function MakeMethodCall(SepiMethod: TSepiMethodBase;
      const SelfParam: ISepiReadableValue): ISepiWantingParams; virtual;

    function CompileMethodCall: ISepiWantingParams; virtual;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud représentant une expression d'appel inherited pur
    @author sjrd
    @version 1.0
  *}
  TSepiPureInheritedExpressionNode = class(TSepiExpressionNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud représentant une expression avec des modificateurs
    Le premier enfant d'un noeud de ce type est une expression classique, de
    type TSepiExpressionNode. Les suivants sont des "modifieurs", de type
    TSepiExpressionModifierNode, qui modifient successivement les expressions
    intermédiaire.
    Par exemple, s'il y a trois enfants, le résultat est l'application du
    modifieur 2 sur une expression intermédiaire, qui est l'application du
    modifieur 1 sur l'expression de l'enfant 0.
    Des modifieurs courants sont des indices de tableaux, des paramètres d'appel
    de méthode ou une sélection d'un champ.
    @author sjrd
    @version 1.0
  *}
  TSepiExpressionWithModifiersNode = class(TSepiExpressionNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud modifieur d'expression
    Une modifieur d'expression est un noeud qui part d'une expression existante
    et la modifie. L'expression de base doit être renseignée avec SetBase avant
    BeginParsing.
    @author sjrd
    @version 1.0
  *}
  TSepiExpressionModifierNode = class(TSepiExpressionNode)
  private
    FBase: ISepiExpression; /// Expression de base à modifier
  protected
    property Base: ISepiExpression read FBase;
  public
    procedure SetBase(const ABase: ISepiExpression);
  end;

  {*
    Noeud modificateur d'expression sur base d'un opérateur unaire
    Un noeud de ce type doit avoir un unique enfant de type TSepiUnaryOpNode.
    @author sjrd
    @version 1.0
  *}
  TSepiUnaryOpModifierNode = class(TSepiExpressionModifierNode)
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud paramètres réels
    @author sjrd
    @version 1.0
  *}
  TSepiParametersNode = class(TSepiExpressionModifierNode)
  protected
    procedure CompileParams(const WantingParams: ISepiWantingParams); virtual;

    procedure Compile; virtual;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud paramètres réels de style Delphi
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiLikeParametersNode = class(TSepiParametersNode)
  protected
    procedure CompileIdentifierTest(
      const PseudoRoutine: ISepiIdentifierTestPseudoRoutine);

    procedure Compile; override;
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud index de tableau ou de propriété tableau
    @author sjrd
    @version 1.0
  *}
  TSepiArrayIndicesModifierNode = class(TSepiExpressionModifierNode)
  protected
    procedure CompileProperty(const Prop: ISepiProperty); virtual;
    procedure CompileArrayItem(const BaseValue: ISepiValue); virtual;
    procedure CompileDefaultProperty(
      const ObjectValue: ISepiReadableValue); virtual;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud sélection de champ
    @author sjrd
    @version 1.0
  *}
  TSepiFieldSelectionModifierNode = class(TSepiExpressionModifierNode)
  protected
    function MakeFieldSelection: ISepiExpression; virtual;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud représentant une déclaration d'un identificateur
    TSepiIdentifierDeclarationNode prend en charge la vérification de l'unicité
    de l'identificateur, et son remplacement en cas d'erreur.
    La vérification se fait au moyen de IsRedeclared. De plus, si le parent d'un
    tel noeud est une instance de TSepiIdentifierDeclListNode, sont considérés
    les identificateurs déjà déclarés dans la liste, tels que testés avec la
    fonction TSepiIdentifierDeclListNode.IsDeclared.
    @author sjrd
    @version 1.0
  *}
  TSepiIdentifierDeclarationNode = class(TSepiNonTerminal)
  private
    FIdentifier: string; /// Identificateur (si vide, AsText est utilisé)

    function IsRedeclaredInCurrentList: Boolean;

    function GetIdentifier: string;
  protected
    procedure SetIdentifier(const AIdentifier: string);

    function IsRedeclared: Boolean; virtual;
    procedure MakeErroneousName; virtual;
  public
    procedure EndParsing; override;

    property Identifier: string read GetIdentifier;
  end;

  {*
    Noeud représentant une déclaration d'un identificateur non vérifié
    @author sjrd
    @version 1.0
  *}
  TSepiUncheckedIdentifierDeclNode = class(TSepiIdentifierDeclarationNode)
  protected
    function IsRedeclared: Boolean; override;
  end;

  {*
    Noeud représentant une liste de déclarations d'identificateurs
    Tous les enfants d'une instance de TSepiIdentifierDeclListNode doivent être
    des instances de TSepiIdentifierDeclarationNode.
    @author sjrd
    @version 1.0
  *}
  TSepiIdentifierDeclListNode = class(TSepiNonTerminal)
  private
    FIdentifierCount: Integer; /// Nombre d'identificateurs

    function GetIdentifiers(Index: Integer): string;
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    function IsDeclared(const Identifier: string): Boolean; virtual;

    procedure GetIdentifierList(out IdentifierList: TStringDynArray);

    property IdentifierCount: Integer read FIdentifierCount;
    property Identifiers[Index: Integer]: string read GetIdentifiers;
  end;

  {*
    Noeud représentant un changement de visibilité
    @author sjrd
    @version 1.0
  *}
  TSepiChangeVisibilityNode = class(TSepiNonTerminal)
  protected
    function GetVisibility: TMemberVisibility; virtual;
  public
    procedure EndParsing; override;

    property Visibility: TMemberVisibility read GetVisibility;
  end;

  {*
    Noeud d'un champ d'une classe
    @author sjrd
    @version 1.0
  *}
  TSepiClassFieldNode = class(TSepiNonTerminal)
  private
    FFieldType: TSepiType;  /// Type des champs
    FLastField: TSepiField; /// Dernier champ compilé
  public
    procedure EndParsing; override;

    property FieldType: TSepiType read FFieldType;
    property LastField: TSepiField read FLastField;
  end;

  {*
    Noeud marqueur que ses enfants sont déclarés comme membres de classe
    @author sjrd
    @version 1.0
  *}
  TSepiClassMemberDefinitionNode = class(TSepiNonTerminal)
  end;

  {*
    Classe de base pour les noeuds qui doivent construire une signature
    TSepiSignatureBuilderNode ne crée pas elle-même d'instance de
    TSepiSignature. Elle est prévue pour construire une signature créée par un
    noeud parent, et renseignée au moyen de SetSignature (avant BeginParsing).
    Certaines sous-classes de TSepiSignatureBuilderNode acceptent que Signature
    ne soit pas renseignée. Dans ce cas, elles n'ont aucun effet en elles-mêmes,
    mais proposent souvent des propriétés permettant de savoir quel aurait été
    son effet si Signature avait été renseigné.
    TSepiSignatureBuilderNode transfère aussi sa signature à tous ses enfants
    qui sont des instances de TSepiSignatureBuilderNode.
    leur effet.
    @author sjrd
    @version 1.0
  *}
  TSepiSignatureBuilderNode = class(TSepiNonTerminal)
  private
    FSignature: TSepiSignature; /// Signature à construire
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;

    property Signature: TSepiSignature read FSignature;
  public
    procedure SetSignature(ASignature: TSepiSignature);
  end;

  {*
    Noeud représentant un type de signature
    Cette classe admet que Signature ne soit pas renseignée.
    @author sjrd
    @version 1.0
  *}
  TSepiSignatureKindNode = class(TSepiSignatureBuilderNode)
  protected
    function GetKind: TSepiSignatureKind; virtual;
  public
    procedure EndParsing; override;

    property Kind: TSepiSignatureKind read GetKind;
  end;

  {*
    Noeud représentant un type de signature pour une propriété
    Cette classe admet que Signature ne soit pas renseignée.
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyKindNode = class(TSepiSignatureKindNode)
  protected
    function GetKind: TSepiSignatureKind; override;
  end;

  {*
    Noeud représentant la convention d'appel d'une signature
    Cette classe admet que Signature ne soit pas renseignée.
    @author sjrd
    @version 1.0
  *}
  TSepiCallingConventionNode = class(TSepiSignatureBuilderNode)
  protected
    function GetCallingConvention: TCallingConvention; virtual;
  public
    procedure EndParsing; override;

    property CallingConvention: TCallingConvention read GetCallingConvention;
  end;

  {*
    Noeud paramètre
    @author sjrd
    @version 1.0
  *}
  TSepiParamNode = class(TSepiSignatureBuilderNode)
  private
    FKind: TSepiParamKind;     /// Type de paramètre
    FNames: TStringDynArray;   /// Noms des paramètres
    FOpenArray: Boolean;       /// True si c'est un tableau ouvert
    FType: TSepiType;          /// Type du paramètre
    FDefaultValuePtr: Pointer; /// Pointeur sur la valeur par défaut
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    procedure EndParsing; override;

    property Kind: TSepiParamKind read FKind;
    property Names: TStringDynArray read FNames;
    property OpenArray: Boolean read FOpenArray;
    property ParamType: TSepiType read FType;
  end;

  {*
    Noeud type de paramètre
    @author sjrd
    @version 1.0
  *}
  TSepiParamKindNode = class(TSepiNonTerminal)
  protected
    function GetKind: TSepiParamKind; virtual;
  public
    property Kind: TSepiParamKind read GetKind;
  end;

  {*
    Noeud nom d'un paramètre
    @author sjrd
    @version 1.0
  *}
  TSepiParamNameNode = class(TSepiIdentifierDeclarationNode)
  private
    FSignature: TSepiSignature; /// Signature
  protected
    function IsRedeclared: Boolean; override;
    procedure MakeErroneousName; override;

    property Signature: TSepiSignature read FSignature;
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud marqueur qu'un paramètre est un tableau ouvert
    @author sjrd
    @version 1.0
  *}
  TSepiParamIsArrayMarkerNode = class(TSepiNonTerminal)
  end;

  {*
    Noeud représentant le type de retour d'une signature
    @author sjrd
    @version 1.0
  *}
  TSepiSignatureReturnTypeNode = class(TSepiSignatureBuilderNode)
  private
    procedure AdaptSignatureKindToProcedure;
    procedure AdaptSignatureKindToFunction;
    procedure AdaptSignatureKindIfNeeded;
  protected
    function MustAdaptSignatureKind: Boolean; virtual;

    function GetTypeName: string;

    procedure CompileNoReturnType; virtual;
    function CompileReturnType: TSepiType; virtual;

    property TypeName: string read GetTypeName;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud déclaration de méthode
    @author sjrd
    @version 1.0
  *}
  TSepiMethodDeclarationNode = class(TSepiNonTerminal)
  private
    FName: string;              /// Nom de la méthode
    FSignature: TSepiSignature; /// Signature
    FLinkKind: TMethodLinkKind; /// Type de liaison
    FAbstract: Boolean;         /// Indique si la méthode est abstraite
    FMsgID: Integer;            /// Message intercepté
    FIsOverloaded: Boolean;     /// True si surchargée
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
  end;

  {*
    Noeud représentant le type de liaison d'une méthode
    @author sjrd
    @version 1.0
  *}
  TSepiMethodLinkKindNode = class(TSepiNonTerminal)
  protected
    function GetLinkKind: TMethodLinkKind; virtual;
    function GetMessageID: Integer; virtual;

    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    property LinkKind: TMethodLinkKind read GetLinkKind;
    property MessageID: Integer read GetMessageID;
  end;

  {*
    Marqueur abstract
    @author sjrd
    @version 1.0
  *}
  TSepiAbstractMarkerNode = class(TSepiNonTerminal)
  end;

  {*
    Marqueur overload
    @author sjrd
    @version 1.0
  *}
  TSepiOverloadMarkerNode = class(TSepiNonTerminal)
  end;

  {*
    Marqueur static
    @author sjrd
    @version 1.0
  *}
  TSepiStaticMarkerNode = class(TSepiNonTerminal)
  end;

  {*
    Marqueur forward
    @author sjrd
    @version 1.0
  *}
  TSepiForwardMarkerNode = class(TSepiNonTerminal)
  end;

  {*
    Noeud propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyNode = class(TSepiNonTerminal)
  private
    FBuilder: TSepiPropertyBuilder; /// Constructeur de propriété
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Builder: TSepiPropertyBuilder read FBuilder;
  end;

  {*
    Classe de base pour les noeuds qui construisent une propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyBuilderNode = class(TSepiSignatureBuilderNode)
  private
    FBuilder: TSepiPropertyBuilder; /// Noeud propriété
  protected
    property Builder: TSepiPropertyBuilder read FBuilder;
  public
    procedure SetBuilder(ABuilder: TSepiPropertyBuilder);
  end;

  {*
    Noeud marqueur qu'une propriété est redéfinie
    @author sjrd
    @version 1.0
  *}
  TSepiPropRedefineMarkerNode = class(TSepiPropertyBuilderNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud accesseur en lecture d'une propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropReadAccessNode = class(TSepiPropertyBuilderNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud accesseur en écriture d'une propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropWriteAccessNode = class(TSepiPropertyBuilderNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud index d'une propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropIndexNode = class(TSepiPropertyBuilderNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud valeur par défaut d'une propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropDefaultValueNode = class(TSepiPropertyBuilderNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud spécificateur de stockage d'une propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropStorageNode = class(TSepiPropertyBuilderNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud marqueur qu'une propriété est la propriété par défaut
    @author sjrd
    @version 1.0
  *}
  TSepiPropDefaultMarkerNode = class(TSepiPropertyBuilderNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud redirecteur de méthode d'interface
    Ce type de noeud doit toujours avoir en contexte une classe
    @author sjrd
    @version 1.0
  *}
  TSepiIntfMethodRedirectorNode = class(TSepiNonTerminal)
  private
    FSepiClass: TSepiClass; /// Classe englobante
  protected
    function CompileInterface: TSepiInterface; virtual;
    function CompileIntfMethod(SepiIntf: TSepiInterface): TSepiMethod; virtual;
    function GetRedirectorName: string; virtual;

    procedure CompileMethodRedirector; virtual;

    property SepiClass: TSepiClass read FSepiClass;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;
  end;

  {*
    Noeud représentant un type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeNode = class(TSepiNonTerminal)
  private
    FSepiType: TSepiType; /// Type représenté
  protected
    procedure SetSepiType(ASepiType: TSepiType);

    procedure MakeErroneousType; virtual;
  public
    procedure EndParsing; override;

    property SepiType: TSepiType read FSepiType;
  end;

  {*
    Noeud identifiant un type par son nom
    @author sjrd
    @version 1.0
  *}
  TSepiTypeNameNode = class(TSepiTypeNode)
  public
    procedure EndParsing; override;

    property TypeName: string read GetAsText;
  end;

  {*
    Noeud représentant une définition de type
    @author sjrd
    @version 1.0
  *}
  TSepiTypeDefinitionNode = class(TSepiTypeNode)
  private
    FTypeName: string; /// Nom du type à définir (peut être '')

    function GetIsAnonymous: Boolean;
  protected
    procedure MakeErroneousType; override;
  public
    procedure SetTypeName(const ATypeName: string);

    procedure EndParsing; override;

    property IsAnonymous: Boolean read GetIsAnonymous;
    property TypeName: string read FTypeName;
  end;

  {*
    Noeud descripteur de contenu de record
    Le contexte Sepi d'un tel noeud doit toujours être de type TSepiRecordType.
    @author sjrd
    @version 1.0
  *}
  TSepiRecordContentsNode = class(TSepiNonTerminal)
  private
    FAfterField: string; /// Nom du champ après lequel se placer
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    property AfterField: string read FAfterField;
  public
    procedure BeginParsing; override;

    procedure SetAfterField(const AAfterField: string);
  end;

  {*
    Noeud header de case dans un record
    Le contexte Sepi d'un tel noeud doit toujours être de type TSepiRecordType.
    @author sjrd
    @version 1.0
  *}
  TSepiRecordCaseHeaderNode = class(TSepiRecordContentsNode)
  end;

  {*
    Noeud d'un champ de record
    Le contexte Sepi d'un tel noeud doit toujours être de type TSepiRecordType.
    @author sjrd
    @version 1.0
  *}
  TSepiRecordFieldNode = class(TSepiNonTerminal)
  private
    FAfterField: string;    /// Champ après lequel se placer
    FLastField: TSepiField; /// Dernier champ compilé
  protected
    property AfterField: string read FAfterField;
  public
    procedure EndParsing; override;

    procedure SetAfterField(const AAfterField: string);

    property LastField: TSepiField read FLastField;
  end;

  {*
    Noeud méthode dans l'implémentation
    @author sjrd
    @version 1.0
  *}
  TSepiMethodImplementationNode = class(TSepiNonTerminal)
  private
    FSepiMethod: TSepiMethod; /// Méthode correspondante
    FJustDeclared: Boolean;   /// Indique si déclaré dans ce noeud
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    property SepiMethod: TSepiMethod read FSepiMethod;
    property JustDeclared: Boolean read FJustDeclared;
  end;

  {*
    Noeud signature dans l'implémentation
    @author sjrd
    @version 1.0
  *}
  TSepiMethodImplHeaderNode = class(TSepiNonTerminal)
  private
    FName: string;              /// Nom de la routine
    FSignature: TSepiSignature; /// Signature
    FIsOverloaded: Boolean;     /// True si surchargée

    FOverloaded: TSepiOverloadedMethod; /// Méthode surchargée correspondante

    FSepiMethod: TSepiMethod; /// Méthode Sepi
    FJustDeclared: Boolean;   /// True si la méthode a été déclarée par ce noeud
  protected
    procedure DeclareMethod; virtual;

    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    property Name: string read FName;
    property Signature: TSepiSignature read FSignature;
    property IsOverloaded: Boolean read FIsOverloaded;

    property Overloaded: TSepiOverloadedMethod read FOverloaded;
  public
    destructor Destroy; override;

    procedure BeginParsing; override;
    procedure EndParsing; override;

    property SepiMethod: TSepiMethod read FSepiMethod;
    property JustDeclared: Boolean read FJustDeclared;
  end;

  {*
    Noeud du corps d'une méthode
    @author sjrd
    @version 1.0
  *}
  TSepiMethodBodyNode = class(TSepiNonTerminal)
  private
    FSepiMethod: TSepiMethod;       /// Méthode Sepi implémentée dans ce corps
    FCompiler: TSepiMethodCompiler; /// Compilateur de la méthode
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;

    function GetMethodCompiler: TSepiMethodCompiler; override;
    function GetSepiContext: TSepiComponent; override;
  public
    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    procedure EndParsing; override;

    procedure SetSepiMethod(ASepiMethod: TSepiMethod);

    property SepiMethod: TSepiMethod read FSepiMethod;
  end;

  {*
    Noeud représentant le nom d'une variable locale à rechercher
    @author sjrd
    @version 1.0
  *}
  TSepiLocalVarNameNode = class(TSepiNonTerminal)
  private
    FLocalVar: TSepiLocalVar; /// Variable locale
  protected
    procedure SetLocalVar(ALocalVar: TSepiLocalVar);

    function GetLocalVarName: string; virtual;
    function CompileLocalVar: TSepiLocalVar; virtual;

    procedure MakeErroneousLocalVar; virtual;
  public
    procedure EndParsing; override;

    property LocalVarName: string read GetLocalVarName;
    property LocalVar: TSepiLocalVar read FLocalVar;
  end;

  {*
    Noeud déclaration de variable locale
    @author sjrd
    @version 1.0
  *}
  TSepiLocalVarNode = class(TSepiNonTerminal)
  private
    FNamesNode: TSepiIdentifierDeclListNode; /// Noeud des noms de variables
    FVarType: TSepiType;                     /// Type des variables
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    property NamesNode: TSepiIdentifierDeclListNode read FNamesNode;
    property VarType: TSepiType read FVarType;
  public
    procedure EndParsing; override;
  end;

  {*
    Classe de base pour les noeuds instruction
    @author sjrd
    @version 1.0
  *}
  TSepiInstructionNode = class(TSepiNonTerminal)
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
  TSepiNoInstructionNode = class(TSepiInstructionNode)
  end;

  {*
    Liste d'instructions
    @author sjrd
    @version 1.0
  *}
  TSepiInstructionListNode = class(TSepiInstructionNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Bloc begin..end
    @author sjrd
    @version 1.0
  *}
  TSepiBeginEndBlockNode = class(TSepiInstructionListNode)
  end;

  {*
    Instruction if..then..else
    @author sjrd
    @version 1.0
  *}
  TSepiIfThenElseInstructionNode = class(TSepiInstructionNode)
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
    Instruction case of
    @author sjrd
    @version 1.0
  *}
  TSepiCaseOfInstructionNode = class(TSepiInstructionNode)
  private
    FInstruction: TSepiCaseOf; /// Instruction
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Instruction: TSepiCaseOf read FInstruction;
  end;

  {*
    Clause on d'un case of
    Le parent d'un noeud de ce type doit toujours être une instance de
    TSepiCaseOfNode.
    @author sjrd
    @version 1.0
  *}
  TSepiCaseOfClauseNode = class(TSepiInstructionNode)
  private
    FInstruction: TSepiCaseOf;  /// Instruction case of contenante
    FClause: TSepiCaseOfClause; /// Clause représentée par ce noeud
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;

    property Instruction: TSepiCaseOf read FInstruction;
    property Clause: TSepiCaseOfClause read FClause;
  end;

  {*
    Instruction while..do ou similaire
    @author sjrd
    @version 1.0
  *}
  TSepiWhileInstructionNode = class(TSepiInstructionNode)
  private
    FTestAtEnd: Boolean;  /// True si le test est à la fin
    FInvertTest: Boolean; /// True si le test doit être inversé (not Test)

    FInstruction: TSepiWhile; /// Instruction
  protected
    procedure Configure(ATestAtEnd, AInvertTest: Boolean);

    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property TestAtEnd: Boolean read FTestAtEnd;
    property InvertTest: Boolean read FInvertTest;

    property Instruction: TSepiWhile read FInstruction;
  end;

  {*
    Instruction do..while
    @author sjrd
    @version 1.0
  *}
  TSepiDoWhileInstructionNode = class(TSepiWhileInstructionNode)
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); override;
  end;

  {*
    Instruction repeat..until (comme do..while mais avec un test inversé)
    @author sjrd
    @version 1.0
  *}
  TSepiRepeatUntilInstructionNode = class(TSepiWhileInstructionNode)
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); override;
  end;

  {*
    Instruction for..do
    @author sjrd
    @version 1.0
  *}
  TSepiForInstructionNode = class(TSepiInstructionNode)
  private
    FInstruction: TSepiFor; /// Instruction
  protected
    function MakeErroneousBoundValue: ISepiReadableValue; virtual;

    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property Instruction: TSepiFor read FInstruction;
  end;

  {*
    Noeud représentant la variable de contrôle d'une boucle for
    Le parent d'un noeud ce type doit être une instance de
    TSepiForInstructionNode.
    @author sjrd
    @version 1.0
  *}
  TSepiForControlVarNode = class(TSepiLocalVarNameNode)
  protected
    procedure MakeErroneousLocalVar; override;
  end;

  {*
    Classe de base pour des noeuds indiquant qu'un for est 'to' ou 'downto'
    @author sjrd
    @version 1.0
  *}
  TSepiForToDownToNode = class(TSepiNonTerminal)
  protected
    {*
      Indique si le for englobant est 'to' ou 'downto'
      @return True s'il est 'downto', False s'il est 'to'
    *}
    function GetIsDownTo: Boolean; virtual; abstract;
  public
    property IsDownTo: Boolean read GetIsDownTo;
  end;

  {*
    Noeud indiquant que le for englobant est 'to'
    @author sjrd
    @version 1.0
  *}
  TSepiForToNode = class(TSepiForToDownToNode)
  protected
    function GetIsDownTo: Boolean; override;
  end;

  {*
    Noeud indiquant que le for englobant est 'downto'
    @author sjrd
    @version 1.0
  *}
  TSepiForDownToNode = class(TSepiForToDownToNode)
  protected
    function GetIsDownTo: Boolean; override;
  end;

  {*
    Instruction try..except..end ou try..finally..end
    @author sjrd
    @version 1.0
  *}
  TSepiTryInstructionNode = class(TSepiInstructionNode)
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
  TSepiExceptFinallyClauseNode = class(TSepiInstructionNode)
  private
    FTryInstructions: TSepiInstructionList; /// Instructions dans le try
  protected
    property TryInstructions: TSepiInstructionList read FTryInstructions;
  public
    procedure SetTryInstructions(ATryInstructions: TSepiInstructionList);
  end;

  {*
    Clause except d'un try..except.end
    @author sjrd
    @version 1.0
  *}
  TSepiExceptClauseNode = class(TSepiExceptFinallyClauseNode)
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
  TSepiMultiOnNode = class(TSepiInstructionNode)
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
    Le parent d'un noeud de ce type doit toujours être une instance de
    TSepiMultiOnNode.
    @author sjrd
    @version 1.0
  *}
  TSepiOnClauseNode = class(TSepiInstructionNode)
  private
    FInstruction: TSepiMultiOn; /// Instruction multi-on contenante

    FExceptObjectVarName: string;    /// Nom de la variable objet exception
    FExceptObjectClass: TSepiClass;  /// Classe d'exception capturée
    FExceptObjectVar: TSepiLocalVar; /// Variable objet exception du bon type
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property Instruction: TSepiMultiOn read FInstruction;

    property ExceptObjectVarName: string read FExceptObjectVarName;
    property ExceptObjectClass: TSepiClass read FExceptObjectClass;
    property ExceptObjectVar: TSepiLocalVar read FExceptObjectVar;
  end;

  {*
    Noeud avec éventuellement un identificateur, mais toujours un type
    @author sjrd
    @version 1.0
  *}
  TSepiMaybeIdentAndTypeNode = class(TSepiTypeNode)
  private
    FIdentifier: string; /// Identificateur (peut être '')
  protected
    procedure SetIdentifier(const AIdentifier: string);

    procedure CompileIdentifier(Child: TSepiParseTreeNode); virtual;
    procedure CompileType(Child: TSepiParseTreeNode); virtual;
  public
    procedure EndParsing; override;

    property Identifier: string read FIdentifier;
  end;

  {*
    Noeud représentant le nom et le type de la variable exception d'un 'on'
    @author sjrd
    @version 1.0
  *}
  TSepiExceptionVarAndTypeNode = class(TSepiMaybeIdentAndTypeNode)
  protected
    procedure CompileType(Child: TSepiParseTreeNode); override;

    procedure MakeErroneousType; override;
  end;

  {*
    Clause else d'un multi-on
    @author sjrd
    @version 1.0
  *}
  TSepiMultiOnElseClauseNode = class(TSepiInstructionListNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Clause finally d'un try..except.end
    @author sjrd
    @version 1.0
  *}
  TSepiFinallyClauseNode = class(TSepiExceptFinallyClauseNode)
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
  TSepiRaiseInstructionNode = class(TSepiInstructionNode)
  private
    procedure MakeRaiseInstruction;
    procedure MakeReraiseInstruction;
  public
    procedure EndParsing; override;
  end;

  {*
    Instruction d'exécution d'une expression (exécutable)
    @author sjrd
    @version 1.0
  *}
  TSepiExecuteExpressionInstructionNode = class(TSepiInstructionNode)
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Instruction with
    @author sjrd
    @version 1.0
  *}
  TSepiWithInstructionNode = class(TSepiInstructionNode)
  private
    FWithVar: TSepiLocalVar;        /// Variable with
    FWithValue: ISepiReadableValue; /// Valeur de la variable with
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property WithVar: TSepiLocalVar read FWithVar;
    property WithValue: ISepiReadableValue read FWithValue;
  end;

implementation

{------------------------}
{ TUsesSectionNode class }
{------------------------}

{*
  Teste si un nom d'unité est redéclaré
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
  @param AValueType   Type à tester
  @return True si le type est valide, False sinon
*}
function TSepiInitializationExpressionNode.IsValidType(
  AValueType: TSepiType): Boolean;
begin
  Result := AValueType <> nil;
end;

{*
  Indique le type de valeur attendu, et où stocker le résultat
  Si AValuePtr vaut nil, une nouvelle valeur du type AValueType sera créée, et
  libérée à la destruction de cet objet.
  Cette méthode doit être appelée exactement une fois, avant BeginParsing.
  @param AValueType   Type de valeur attendu (doit être valide)
  @param AValuePtr    Pointeur où stocker le résultat (peut être nil)
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
  @return True en cas de succès, False sinon
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
  Tente de convertir une valeur en un type donné
  @param Value       Valeur à convertir
  @param ValueType   Type requis
  @return True en cas de succès, False sinon
*}
function TSepiExpressionNode.TryAndConvert(var Value: ISepiValue;
  ValueType: TSepiType): Boolean;
var
  ReadableValue: ISepiReadableValue;
begin
  Result := Supports(Value, ISepiReadableValue, ReadableValue) and
    TSepiConvertOperation.ConversionExists(ValueType, Value.ValueType);

  if Result then
    Value := TSepiConvertOperation.ConvertValue(ValueType, ReadableValue);
end;

{*
  Tente de faire conrrespondre le type d'une valeur à un type donné
  @param Value             Valeur à convertir
  @param ValueType         Type requis
  @param AllowConvertion   Autoriser des conversion pour obtenir le type requis
  @return True en cas de succès, False sinon
*}
function TSepiExpressionNode.TryAndMatchType(var Value: ISepiValue;
  ValueType: TSepiType; AllowConvertion: Boolean): Boolean;
begin
  Result := (Value.ValueType = ValueType) or
    ((Value.ValueType <> nil) and ValueType.Equals(Value.ValueType)) or
    TryAndForceType(Value, ValueType);

  if (not Result) and AllowConvertion then
    Result := TryAndConvert(Value, ValueType);
end;

{*
  Exige qu'une variable ait un type donné
  @param Value             Valeur à convertir
  @param ValueType         Type requis
  @param AllowConvertion   Autoriser des conversion pour obtenir le type requis
*}
procedure TSepiExpressionNode.RequireType(var Value: ISepiValue;
  ValueType: TSepiType; AllowConvertion: Boolean);
begin
  if not TryAndMatchType(Value, ValueType, AllowConvertion) then
    MakeError(Format(STypeMismatch,
      [ValueType.DisplayName, Value.ValueType.DisplayName]));
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
  Construit une valeur erronée
  @param ValueType   Type de valeur erronée (peut être nil)
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
  Vérifie la validité de l'expression compilée
  ValidateExpression n'est appelée que si Expression <> nil.
  @return True si l'expression est valide, False sinon
*}
function TSepiExpressionNode.ValidateExpression: Boolean;
begin
  Result := True;
end;

{*
  Construit une expression erronée
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
  Lit l'expression comme un type donné de partie d'expression
  Si l'expression n'est du type requis, et si ErrorMsg est non vide, un message
  d'erreur est émis, et AsExpressionPart renvoie False.
  @param IID        Type de partie d'expression requis
  @param Intf       En sortie : partie d'expression demandée, si succès
  @param ErrorMsg   Message d'erreur en cas d'échec
  @return True en cas de succès, False sinon
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
  @return Valeur représentée par l'expression (ou nil en cas d'erreur)
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
  Lit l'expression en tant que valeur qui peut être lue
  @param ValueType         Type de valeur attendu (nil = tout type)
  @param AllowConvertion   Autoriser des conversion pour obtenir le type requis
  @return Valeur représentée par l'expression (ou nil en cas d'erreur)
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
  @return Component représenté par l'expression (ou nil en cas d'erreur)
*}
function TSepiExpressionNode.AsComponent: TSepiComponent;
var
  ComponentExpression: ISepiComponentExpression;
begin
  if AsExpressionPart(ISepiComponentExpression, ComponentExpression, SComponentRequired) then
    Result := ComponentExpression.Component
  else
    Result := nil;
end;

{*
  Lit l'expression en tant que type
  @return Type représenté par l'expression (ou System.Integer en cas d'erreur)
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

{--------------------------------}
{ TSepiConstExpressionNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
function TSepiConstExpressionNode.ValidateExpression: Boolean;
var
  Value: ISepiValue;
  ReadableValue: ISepiReadableValue;
begin
  Value := AsValue(ValueType);
  if Value = nil then
    Result := False
  else
  begin
    Result := Supports(Value, ISepiReadableValue, ReadableValue) and
      ReadableValue.IsConstant;

    if not Result then
      MakeError(SConstExpressionRequired)
    else if ValueType <> nil then
      SetExpression(Value as ISepiExpression);
  end;
end;

{*
  Construit une expression erronée
*}
procedure TSepiConstExpressionNode.MakeErroneousExpression;
begin
  SetExpressionPart(MakeErroneousValue(ValueType));
end;

{*
  Compile une valeur constante
  @param Value       En sortie : valeur compilée
  @param ValueType   Type de la valeur (si nil, utilise la propriété ValueType)
  @return True si la compilation est réussie, False en cas d'erreur
*}
function TSepiConstExpressionNode.CompileConst(var Value;
  ValueType: TSepiType = nil): Boolean;
var
  ReadableValue: ISepiReadableValue;
begin
  if ValueType = nil then
  begin
    ValueType := FValueType;
    Assert(ValueType <> nil);
  end;

  ReadableValue := AsReadableValue(ValueType);
  Result := (ReadableValue <> nil) and ReadableValue.IsConstant;

  if Result then
    ValueType.CopyData(ReadableValue.ConstValuePtr^, Value);
end;

{*
  Lit l'expression en tant que valeur entière constante
  @param DefaultValue   Valeur par défaut
  @return L'expression comme valeur entière, ou DefaultValue en cas d'erreur
*}
function TSepiConstExpressionNode.AsInteger(DefaultValue: Integer = 0): Integer;
begin
  if not CompileConst(Result, SystemUnit.Integer) then
    Result := DefaultValue;
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

{-------------------------}
{ TSepiBinaryOpNode class }
{-------------------------}

{*
  Priorité de cet opérateur
  @return Priorité de cet opérateur
*}
function TSepiBinaryOpNode.GetPriority: Integer;
begin
  Result := 1;
end;

{*
  Direction d'évaluation de cet opérateur
  Afin que l'évaluation d'expressions soit cohérente, tous les opérateurs
  possédant la même priorité doivent également être évalués dans la même
  direction.
  @return Direction d'évaluation de cet opérateur
*}
function TSepiBinaryOpNode.GetDirection: TSepiBinaryOpDirection;
begin
  Result := bodLeftToRight;
end;

{-----------------------------------}
{ TSepiDelphiLikeBinaryOpNode class }
{-----------------------------------}

{*
  Essaie d'appliquer une opération d'arithmétique des pointeurs
  @param PtrValue     Valeur supposée pointeur
  @param IntValue     Valeur supposée entière
  @param Expression   En sortie : résultat, si réussi
  @return True en cas de succès, False sinon
*}
function TSepiDelphiLikeBinaryOpNode.TryAndHandlePointerArithm(
  const PtrValue, IntValue: ISepiReadableValue;
  out Expression: ISepiExpression): Boolean;
var
  PtrType: TSepiPointerType;
  TempValue: ISepiReadableValue;
begin
  Result := False;

  if not (Operation in [opAdd, opSubtract]) then
    Exit;
  if not (PtrValue.ValueType is TSepiPointerType) then
    Exit;
  if not (IntValue.ValueType is TSepiIntegerType) then
    Exit;

  PtrType := TSepiPointerType(PtrValue.ValueType);

  if (not (PtrType.PointTo.Kind in [tkInteger, tkChar])) or
    (PtrType.PointTo.TypeData.OrdType <> otUByte) then
    Exit;

  TempValue := TSepiCastOperator.CastValue(SystemUnit.Integer,
    PtrValue) as ISepiReadableValue;
  TempValue := TSepiBinaryOperation.MakeOperation(Operation,
    TempValue, IntValue);
  TempValue := TSepiCastOperator.CastValue(PtrType,
    TempValue) as ISepiReadableValue;

  Expression := TempValue as ISepiExpression;
  Result := True;
end;

{*
  Gère le cas spécial du Int/Int avec au moins une constante
  @param LeftValue    Valeur gauche
  @param RightValue   Valeur droite
*}
procedure TSepiDelphiLikeBinaryOpNode.HandleIntDivIntTrick(
  var LeftValue, RightValue: ISepiReadableValue);
var
  LeftTypeKind, RightTypeKind: TTypeKind;
begin
  // Test appliability

  if Operation <> opDivide then
    Exit;

  LeftTypeKind := LeftValue.ValueType.Kind;
  RightTypeKind := RightValue.ValueType.Kind;

  if [LeftTypeKind, RightTypeKind] * [tkFloat, tkVariant] <> [] then
    Exit;

  // Apply the trick

  if LeftValue.IsConstant then
    LeftValue := TSepiConvertOperation.ConvertValue(
      SystemUnit.Extended, LeftValue);

  if RightValue.IsConstant then
    RightValue := TSepiConvertOperation.ConvertValue(
      SystemUnit.Extended, RightValue);
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiLikeBinaryOpNode.GetPriority: Integer;
begin
  case Operation of
    opCmpEQ, opCmpLT, opCmpLE, opCmpGT, opCmpGE, opCmpNE: Result := 1;
    opOr, opAnd, opXor: Result := 2;
    opAdd, opSubtract: Result := 3;
  else
    Result := 4;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiLikeBinaryOpNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  LeftValue, RightValue: ISepiReadableValue;
begin
  RequireReadableValue(Left, LeftValue);
  RequireReadableValue(Right, RightValue);

  if not (TryAndHandlePointerArithm(LeftValue, RightValue, Result) or
    TryAndHandlePointerArithm(RightValue, LeftValue, Result)) then
  begin
    HandleIntDivIntTrick(LeftValue, RightValue);

    Result := TSepiOperator.MakeBinaryOperation(Operation,
      LeftValue, RightValue) as ISepiExpression;
  end;

  Result.SourcePos := SourcePos;
end;

{-------------------------------}
{ TSepiInSetOperationNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
function TSepiInSetOperationNode.GetPriority: Integer;
begin
  Result := 2;
end;

{*
  [@inheritDoc]
*}
function TSepiInSetOperationNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  LeftValue, RightValue: ISepiReadableValue;
begin
  RequireReadableValue(Left, LeftValue);
  RequireReadableValue(Right, RightValue);

  Result := TSepiInSetOperation.MakeOperation(LeftValue,
    RightValue) as ISepiExpression;
  Result.SourcePos := SourcePos;
end;

{----------------------------}
{ TSepiIsOperationNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
function TSepiIsOperationNode.GetPriority: Integer;
begin
  Result := 2;
end;

{*
  [@inheritDoc]
*}
function TSepiIsOperationNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  LeftValue, RightValue: ISepiReadableValue;
begin
  RequireReadableValue(Left, LeftValue);
  RequireReadableValue(Right, RightValue);

  Result := TSepiIsOperation.MakeOperation(LeftValue,
    RightValue) as ISepiExpression;
  Result.SourcePos := SourcePos;
end;

{----------------------------}
{ TSepiAsOperationNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
function TSepiAsOperationNode.GetPriority: Integer;
begin
  Result := 2;
end;

{*
  [@inheritDoc]
*}
function TSepiAsOperationNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  LeftValue, RightValue: ISepiReadableValue;
begin
  RequireReadableValue(Left, LeftValue);
  RequireReadableValue(Right, RightValue);

  Result := TSepiAsOperation.MakeOperation(LeftValue,
    RightValue) as ISepiExpression;
  Result.SourcePos := SourcePos;
end;

{-----------------------------}
{ TSepiAssignmentOpNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
function TSepiAssignmentOpNode.GetPriority: Integer;
begin
  Result := 0;
end;

{*
  [@inheritDoc]
*}
function TSepiAssignmentOpNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  LeftValue: ISepiWritableValue;
  RightValue: ISepiReadableValue;
begin
  RequireWritableValue(Left, LeftValue);
  RequireReadableValue(Right, RightValue);

  if LeftValue = nil then
  begin
    Result := RightValue as ISepiExpression;
    Exit;
  end;

  Result := TSepiAssignmentOperation.MakeOperation(
    LeftValue, RightValue) as ISepiExpression;
  Result.SourcePos := SourcePos;
end;

{-----------------------------}
{ TSepiBinaryOpTreeNode class }
{-----------------------------}

{*
  Trouve l'opérateur majeur d'un sous-arbre
  L'opérateur majeur est celui de plus basse priorité dans le sous-arbre. En cas
  d'égalité, c'est celui de droite pour une évaluation left-to-right ; et c'est
  celui de gauche pour une évaluation right-to-left.
  @param Lower    Index bas des symboles du sous-arbre
  @param Higher   Index haut des symboles du sous-arbre
  @return Index de l'opérateur majeur parmi les enfants de ce noeud
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
  @return Expression représentant le sous-arbre
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
  @return Expression représentant l'arbre
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

{----------------------------------}
{ TSepiDelphiLikeUnaryOpNode class }
{----------------------------------}

{*
  [@inheritDoc]
*}
function TSepiDelphiLikeUnaryOpNode.MakeOperation(
  const Operand: ISepiExpression): ISepiExpression;
var
  Value: ISepiReadableValue;
begin
  RequireReadableValue(Operand, Value);

  if Operation <> opAdd then
    Value := TSepiOperator.MakeUnaryOperation(Operation, Value);

  Result := Value as ISepiExpression;
  Result.SourcePos := SourcePos;
end;

{------------------------------}
{ TSepiDereferenceOpNode class }
{------------------------------}

{*
  [@inheritDoc]
*}
function TSepiDereferenceOpNode.MakeOperation(
  const Operand: ISepiExpression): ISepiExpression;
var
  Value: ISepiReadableValue;
begin
  if Supports(Operand, ISepiReadableValue, Value) and
    (Value.ValueType is TSepiPointerType) then
  begin
    Result := TSepiDereferenceValue.MakeDereference(Value) as ISepiExpression;
  end else
  begin
    Operand.MakeError(SPointerTypeRequired);
    Result := nil;
  end;
end;

{----------------------------}
{ TSepiAddressOfOpNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
function TSepiAddressOfOpNode.MakeOperation(
  const Operand: ISepiExpression): ISepiExpression;
var
  MethodRefValue: ISepiReadableValue;
  AddrValue: ISepiAddressableValue;
begin
  if Supports(Operand, ISepiReadableValue, MethodRefValue) and
    (MethodRefValue.ValueType is TSepiMethodRefType) then
  begin
    Result := TSepiCastOperator.CastValue(SystemUnit.Pointer,
      MethodRefValue, True) as ISepiExpression;
  end else if Supports(Operand, ISepiAddressableValue, AddrValue) then
  begin
    Result := TSepiAddressOfValue.MakeAddressOf(AddrValue) as ISepiExpression;
  end else
  begin
    Operand.MakeError(SAddressableValueRequired);
    Result := nil;
  end;
end;

{-------------------------------}
{ TSepiUnaryOperationNode class }
{-------------------------------}

{*
  Compile l'arbre de l'expression
  @return Expression représentant l'arbre
*}
function TSepiUnaryOperationNode.CompileOperation(
  OperatorNode: TSepiUnaryOpNode;
  OperandNode: TSepiExpressionNode): ISepiExpression;
begin
  Result := OperatorNode.MakeOperation(OperandNode.Expression);
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnaryOperationNode.EndParsing;
begin
  if Expression = nil then
  begin
    Assert(ChildCount = 2);

    if (Children[0] is TSepiUnaryOpNode) and
      (Children[1] is TSepiExpressionNode) then
    begin
      SetExpression(CompileOperation(TSepiUnaryOpNode(Children[0]),
        TSepiExpressionNode(Children[1])));
    end else if (Children[0] is TSepiExpressionNode) and
      (Children[1] is TSepiUnaryOpNode) then
    begin
      SetExpression(CompileOperation(TSepiUnaryOpNode(Children[1]),
        TSepiExpressionNode(Children[0])));
    end else
    begin
      Assert(False);
    end;
  end;

  inherited;
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
  @return Valeur représentée par le littéral
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
  Result := TSepiIntegerLiteralValue.Create(SepiRoot, CompileAsInteger);
end;

{---------------------------}
{ TSepiConstFloatNode class }
{---------------------------}

{*
  Compile la valeur sous forme de nombre à virgule flottante
  En cas d'erreur, peut renvoyer une valeur arbitraire, 0 le plus souvent.
  @return Valeur représentée par le littéral
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
  Result := TSepiFloatLiteralValue.Create(SepiRoot, CompileAsExtended);
end;

{----------------------------}
{ TSepiConstStringNode class }
{----------------------------}

{*
  Compile la valeur sous forme de chaîne de caractères
  En cas d'erreur, peut renvoyer une valeur arbitraire, '' le plus souvent.
  @return Valeur représentée par le littéral
*}
function TSepiConstStringNode.CompileAsString: string;
begin
  Result := StrRepresToStr(AsText);
end;

{*
  [@inheritDoc]
*}
function TSepiConstStringNode.CompileAsValue: ISepiValue;
begin
  Result := TSepiStringLiteralValue.Create(SepiRoot, CompileAsString);
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

{-------------------------}
{ TSepiSetValueNode class }
{-------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiSetValueNode.BeginParsing;
begin
  inherited;

  SetExpression(MakeExpression);
  FSetBuilder := TSepiSetBuilder.Create(UnitCompiler);
  FSetBuilder.AttachToExpression(Expression);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetValueNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  SingleValue, LowerValue, HigherValue: ISepiReadableValue;
begin
  if Child.ChildCount = 1 then
  begin
    // Single value

    SingleValue := (Child.Children[0] as TSepiExpressionNode).AsReadableValue;

    if SingleValue <> nil then
      SetBuilder.AddSingle(SingleValue);
  end else
  begin
    // Range

    LowerValue := (Child.Children[0] as TSepiExpressionNode).AsReadableValue;
    HigherValue := (Child.Children[1] as TSepiExpressionNode).AsReadableValue;

    if (LowerValue <> nil) and (HigherValue <> nil) then
      SetBuilder.AddRange(LowerValue, HigherValue);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetValueNode.EndParsing;
begin
  SetBuilder.Complete;

  inherited;
end;

{--------------------------------------}
{ TSepiSetOrOpenArrayBuilderNode class }
{--------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilderNode.BeginParsing;
begin
  inherited;

  SetExpression(MakeExpression);
  FBuilder := TSepiSetOrOpenArrayBuilder.Create(UnitCompiler);
  FBuilder.AttachToExpression(Expression);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilderNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
var
  SingleValue, LowerValue, HigherValue: ISepiReadableValue;
begin
  if Child.ChildCount = 1 then
  begin
    // Single value

    SingleValue := (Child.Children[0] as TSepiExpressionNode).AsReadableValue;

    if SingleValue <> nil then
      Builder.AddSingle(SingleValue);
  end else
  begin
    // Range

    LowerValue := (Child.Children[0] as TSepiExpressionNode).AsReadableValue;
    HigherValue := (Child.Children[1] as TSepiExpressionNode).AsReadableValue;

    if (LowerValue <> nil) and (HigherValue <> nil) then
      Builder.AddRange(LowerValue, HigherValue);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSetOrOpenArrayBuilderNode.EndParsing;
begin
  Builder.Complete;

  inherited;
end;

{------------------------------------}
{ TSepiInheritedExpressionNode class }
{------------------------------------}

{*
  Vérifie qu'on est bien en train de compiler une méthode
  @return True en cas de succès, False sinon
*}
function TSepiInheritedExpressionNode.RequireClassOrObjectMethod: Boolean;
begin
  Result := (MethodCompiler <> nil) and
    (MethodCompiler.SepiMethod.Owner is TSepiClass);

  if not Result then
    MakeError(SInheritNeedClassOrObjectMethod);
end;

{*
  Nom de la méthode héritée à appeler
  @return Nom de la méthode héritée à appeler
*}
function TSepiInheritedExpressionNode.GetMethodName: string;
begin
  Result := AsText;
end;

{*
  Trouve la méthode héritée à appeler
  @return Méthode à appeler, ou nil en cas d'erreur
*}
function TSepiInheritedExpressionNode.FindMethod: TSepiMethodBase;
var
  CurrentClass, ParentClass: TSepiClass;
  Member: TSepiMember;
begin
  Result := nil;

  CurrentClass := MethodCompiler.SepiMethod.Owner as TSepiClass;
  ParentClass := CurrentClass.Parent;

  Member := ParentClass.LookForMember(GetMethodName, CurrentClass);
  if not CheckIdentFound(Member, GetMethodName, Self) then
    Exit;

  if Member is TSepiMethodBase then
    Result := TSepiMethodBase(Member)
  else
    MakeError(SMethodRequired);
end;

{*
  Construit l'appel de la méthode
  @param SepiMethod   Méthode à appeler
  @param SelfVar      Variable locale Self
  @return Appel de la méthode, ou nil en cas d'erreur
*}
function TSepiInheritedExpressionNode.MakeMethodCall(
  SepiMethod: TSepiMethodBase;
  const SelfParam: ISepiReadableValue): ISepiWantingParams;
const
  ForceStaticCall = True;
  FreeParamIsAlwaysFalse = False;
begin
  Result := TSepiMethodCall.Create(SepiMethod, SelfParam,
    ForceStaticCall, FreeParamIsAlwaysFalse);
end;

{*
  Compile l'appel inherited comme un wanting-params
  @return Appel de méthode inherited (ou nil en cas d'erreur)
*}
function TSepiInheritedExpressionNode.CompileMethodCall: ISepiWantingParams;
var
  SepiMethod: TSepiMethodBase;
  SelfParam: ISepiValue;
begin
  if not RequireClassOrObjectMethod then
    Exit;

  SepiMethod := FindMethod;

  if SepiMethod <> nil then
  begin
    SelfParam := TSepiLocalVarValue.MakeValue(MethodCompiler,
      MethodCompiler.Locals.SelfVar);

    Result := MakeMethodCall(SepiMethod, SelfParam as ISepiReadableValue);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiInheritedExpressionNode.EndParsing;
var
  MethodCall: ISepiWantingParams;
begin
  MethodCall := CompileMethodCall;

  if MethodCall <> nil then
  begin
    SetExpression(MakeExpression);
    MethodCall.AttachToExpression(Expression);
  end;

  inherited;
end;

{----------------------------------------}
{ TSepiPureInheritedExpressionNode class }
{----------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiPureInheritedExpressionNode.EndParsing;
begin
  SetExpression(MakeExpression);
  ISepiExpressionPart(TSepiPureInheritedCall.Create).AttachToExpression(
    Expression);

  inherited;
end;

{----------------------------------------}
{ TSepiExpressionWithModifiersNode class }
{----------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiExpressionWithModifiersNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiExpressionModifierNode then
    TSepiExpressionModifierNode(Child).SetBase(Expression);
end;

{*
  [@inheritDoc]
*}
procedure TSepiExpressionWithModifiersNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
begin
  SetExpression((Child as TSepiExpressionNode).Expression);

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiExpressionWithModifiersNode.EndParsing;
begin
  Assert(ChildCount > 0);

  inherited;
end;

{-----------------------------------}
{ TSepiExpressionModifierNode class }
{-----------------------------------}

{*
  Spécifie l'expression de base à modifier
  @param ABase   Expression de base à modifier
*}
procedure TSepiExpressionModifierNode.SetBase(const ABase: ISepiExpression);
begin
  FBase := ABase;
end;

{--------------------------------}
{ TSepiUnaryOpModifierNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiUnaryOpModifierNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiUnaryOpNode then
    SetExpression(TSepiUnaryOpNode(Child).MakeOperation(Base));

  inherited;
end;

{---------------------------}
{ TSepiParametersNode class }
{---------------------------}

{*
  Compile les paramètres d'une expression requérant des paramètres
  @param WantingParams   Expression requérant des paramètres
*}
procedure TSepiParametersNode.CompileParams(
  const WantingParams: ISepiWantingParams);
var
  I: Integer;
begin
  SetExpression(Base);

  for I := 0 to ChildCount-1 do
    WantingParams.AddParam((Children[I] as TSepiExpressionNode).Expression);

  WantingParams.CompleteParams;
  Expression.Detach(ISepiWantingParams);
  WantingParams.AttachToExpression(Expression);
end;

{*
  Compile les paramètres
*}
procedure TSepiParametersNode.Compile;
var
  WantingParams: ISepiWantingParams;
begin
  if Supports(Base, ISepiWantingParams, WantingParams) then
    CompileParams(WantingParams)
  else
    Base.MakeError(SCallableRequired);
end;

{*
  [@inheritDoc]
*}
procedure TSepiParametersNode.EndParsing;
begin
  Compile;

  inherited;
end;

{-------------------------------------}
{ TSepiDelphiLikeParametersNode class }
{-------------------------------------}

{*
  Compile une pseudo-routine de test d'identificateur
  @param PseudoRoutine   Pseudo-routine à compiler
*}
procedure TSepiDelphiLikeParametersNode.CompileIdentifierTest(
  const PseudoRoutine: ISepiIdentifierTestPseudoRoutine);
begin
  Assert(ChildCount = 1);

  PseudoRoutine.Identifier := Children[0].AsText;
  PseudoRoutine.Complete;

  SetExpression(Base);
  Expression.Detach(ISepiIdentifierTestPseudoRoutine);
  PseudoRoutine.AttachToExpression(Expression);
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiLikeParametersNode.Compile;
var
  IdentTestPseudoRoutine: ISepiIdentifierTestPseudoRoutine;
begin
  if Supports(Base, ISepiIdentifierTestPseudoRoutine,
    IdentTestPseudoRoutine) then
    CompileIdentifierTest(IdentTestPseudoRoutine)
  else
    inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiLikeParametersNode.BeginParsing;
var
  MethodRefValue: ISepiReadableValue;
  TypeExpression: ISepiTypeExpression;
begin
  inherited;

  // Method ref call
  if Supports(Base, ISepiReadableValue, MethodRefValue) and
    (MethodRefValue.ValueType is TSepiMethodRefType) then
  begin
    SetBase(TSepiExpression.Create(Base));

    ISepiExpressionPart(TSepiMethodRefCall.Create(
      MethodRefValue, True)).AttachToExpression(Base);
  end;

  // Cast or convert from the type name
  if Supports(Base, ISepiTypeExpression, TypeExpression) then
  begin
    SetBase(TSepiExpression.Create(Base));

    ISepiExpressionPart(TSepiCastOrConvertPseudoRoutine.Create(
      TypeExpression.ExprType)).AttachToExpression(Base);
  end;
end;

{-------------------------------------}
{ TSepiArrayIndicesModifierNode class }
{-------------------------------------}

{*
  Compile les indices d'une propriété de type tableau
  @param Prop   Propriété
*}
procedure TSepiArrayIndicesModifierNode.CompileProperty(
  const Prop: ISepiProperty);
var
  Count, I: Integer;
begin
  if ChildCount < Prop.ParamCount then
    Count := ChildCount
  else
    Count := Prop.ParamCount;

  for I := 0 to Count-1 do
    Prop.Params[I] := (Children[I] as TSepiExpressionNode).Expression;
  for I := Count to Prop.ParamCount-1 do
    Prop.Params[I] := MakeErroneousValue as ISepiExpression;

  if ChildCount < Prop.ParamCount then
    MakeError(SNotEnoughActualParameters)
  else if ChildCount > Prop.ParamCount then
    MakeError(STooManyActualParameters);

  Prop.CompleteParams;

  SetExpression(Base);
end;

{*
  Compile l'accès à un élément de tableau
  @param BaseValue   Valeur tableau de base
*}
procedure TSepiArrayIndicesModifierNode.CompileArrayItem(
  const BaseValue: ISepiValue);
var
  CurrentValue: ISepiValue;
  IndexValue: ISepiReadableValue;
  I: Integer;
  Child: TSepiExpressionNode;
  BaseType: TSepiType;
begin
  CurrentValue := BaseValue;

  for I := 0 to ChildCount-1 do
  begin
    Child := Children[0] as TSepiExpressionNode;
    BaseType := CurrentValue.ValueType;

    if not (BaseType is TSepiArrayType) then
    begin
      Child.MakeError(STooManyArrayIndices);
      Break;
    end else
    begin
      if Supports(Child.Expression, ISepiReadableValue, IndexValue) then
      begin
        CurrentValue := TSepiArrayItemValue.MakeArrayItemValue(
          CurrentValue, IndexValue);
      end else
      begin
        Child.Expression.MakeError(SReadableValueRequired);
        Break;
      end;
    end;
  end;

  SetExpression(CurrentValue as ISepiExpression);
end;

{*
  Compile les indices de la propriété par défaut d'un objet
  @param ObjectValue   Valeur objet
*}
procedure TSepiArrayIndicesModifierNode.CompileDefaultProperty(
  const ObjectValue: ISepiReadableValue);
var
  Prop: TSepiProperty;
begin
  Prop := TSepiClass(ObjectValue.ValueType).DefaultProperty;

  SetBase(TSepiExpression.Create(Base));

  ISepiExpressionPart(TSepiPropertyValue.Create(ObjectValue,
    Prop)).AttachToExpression(Base);

  CompileProperty(Base as ISepiProperty);
end;

{*
  [@inheritDoc]
*}
procedure TSepiArrayIndicesModifierNode.EndParsing;
var
  Prop: ISepiProperty;
  BaseValue: ISepiValue;
  ObjectValue: ISepiReadableValue;
begin
  if Expression = nil then
  begin
    if Supports(Base, ISepiProperty, Prop) and (not Prop.ParamsCompleted) then
    begin
      // Array property
      CompileProperty(Prop);
    end else if Supports(Base, ISepiValue, BaseValue) and
      (BaseValue.ValueType is TSepiArrayType) then
    begin
      // True array indices
      CompileArrayItem(BaseValue);
    end else if Supports(Base, ISepiReadableValue, ObjectValue) and
      (ObjectValue.ValueType is TSepiClass) and
      (TSepiClass(ObjectValue.ValueType).DefaultProperty <> nil) then
    begin
      // Default array property
      CompileDefaultProperty(ObjectValue);
    end else
    begin
      // Error
      Base.MakeError(SArrayOrArrayPropRequired);
    end;
  end;

  inherited;
end;

{---------------------------------------}
{ TSepiFieldSelectionModifierNode class }
{---------------------------------------}

function TSepiFieldSelectionModifierNode.MakeFieldSelection: ISepiExpression;
begin
  Result := LanguageRules.FieldSelection(SepiContext, Base, Children[0].AsText);
end;

{*
  [@inheritDoc]
*}
procedure TSepiFieldSelectionModifierNode.EndParsing;
begin
  if Expression = nil then
  begin
    SetExpression(MakeFieldSelection);
    CheckIdentFound(Expression, Children[0].AsText, Children[0]);
  end;

  inherited;
end;

{--------------------------------------}
{ TSepiIdentifierDeclarationNode class }
{--------------------------------------}

{*
  Teste si cet identificateur est redéclaré dans sa liste courante
  @return True s'il est redéclaré, False sinon
*}
function TSepiIdentifierDeclarationNode.IsRedeclaredInCurrentList: Boolean;
begin
  Result := (Parent is TSepiIdentifierDeclListNode) and
    TSepiIdentifierDeclListNode(Parent).IsDeclared(Identifier);
end;

{*
  Identificateur représenté par ce noeud
  @return Identificateur représenté par ce noeud
*}
function TSepiIdentifierDeclarationNode.GetIdentifier: string;
begin
  if FIdentifier = '' then
    Result := AsText
  else
    Result := FIdentifier;
end;

{*
  Modifie l'identificateur représenté par ce noeud
  @param AIdentifier   Nouvel identificateur
*}
procedure TSepiIdentifierDeclarationNode.SetIdentifier(
  const AIdentifier: string);
begin
  FIdentifier := AIdentifier;
end;

{*
  Teste si cet identificateur est redéclaré
  @return True s'il est redéclaré, False sinon
*}
function TSepiIdentifierDeclarationNode.IsRedeclared: Boolean;
var
  FirstDecl: TSepiComponent;
begin
  if (MethodCompiler <> nil) and
    (MethodCompiler.Locals.GetVarByName(Identifier) <> nil) then
  begin
    Result := True;
  end else
  begin
    FirstDecl := SepiContext.GetComponent(Identifier);
    Result := (FirstDecl <> nil) and (not FirstDecl.IsForward);
  end;
end;

{*
  Construit un identificateur erroné
*}
procedure TSepiIdentifierDeclarationNode.MakeErroneousName;
begin
  SetIdentifier(SepiContext.MakeUnnamedChildName);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIdentifierDeclarationNode.EndParsing;
begin
  if IsRedeclared or IsRedeclaredInCurrentList then
  begin
    MakeError(SRedeclaredIdentifier);
    MakeErroneousName;
  end;

  inherited;
end;

{----------------------------------------}
{ TSepiUncheckedIdentifierDeclNode class }
{----------------------------------------}

{*
  [@inheritDoc]
*}
function TSepiUncheckedIdentifierDeclNode.IsRedeclared: Boolean;
begin
  Result := False;
end;

{-----------------------------------}
{ TSepiIdentifierDeclListNode class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiIdentifierDeclListNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  Assert(Child is TSepiIdentifierDeclarationNode);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIdentifierDeclListNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
begin
  Inc(FIdentifierCount);

  inherited;
end;

{*
  Tableau zero-based des identificateurs
  @param Index   Index d'un identificateur
  @return L'identificateur à l'index donné
*}
function TSepiIdentifierDeclListNode.GetIdentifiers(Index: Integer): string;
begin
  Result := TSepiIdentifierDeclarationNode(Children[Index]).Identifier;
end;

{*
  Teste si un identificateur donné est déclaré dans cette liste
  @param Identifier   Identificateur à tester
  @return True si l'identificateur est déclaré, False sinon
*}
function TSepiIdentifierDeclListNode.IsDeclared(
  const Identifier: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to IdentifierCount-1 do
  begin
    if AnsiSameText(Identifier, Identifiers[I]) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

{*
  Obtient une liste des identificateurs
  @param IdentifierList   En sortie : liste des identificateurs
*}
procedure TSepiIdentifierDeclListNode.GetIdentifierList(
  out IdentifierList: TStringDynArray);
var
  I: Integer;
begin
  SetLength(IdentifierList, IdentifierCount);

  for I := 0 to IdentifierCount-1 do
    IdentifierList[I] := Identifiers[I];
end;

{---------------------------------}
{ TSepiChangeVisibilityNode class }
{---------------------------------}

{*
  Nouvelle visibilité
  @return Nouvelle visibilité
*}
function TSepiChangeVisibilityNode.GetVisibility: TMemberVisibility;
var
  OrdVisibility: Integer;
begin
  OrdVisibility := AnsiIndexText(AsText, VisibilityStrings);

  if OrdVisibility < 0 then
    Result := mvPublic
  else
    Result := TMemberVisibility(OrdVisibility);
end;

{*
  [@inheritDoc]
*}
procedure TSepiChangeVisibilityNode.EndParsing;
begin
  SepiContext.CurrentVisibility := Visibility;

  inherited;
end;

{---------------------------}
{ TSepiClassFieldNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiClassFieldNode.EndParsing;
var
  IdentList: TSepiIdentifierDeclListNode;
  FieldType: TSepiType;
  SepiClass: TSepiClass;
  I: Integer;
begin
  IdentList := Children[0] as TSepiIdentifierDeclListNode;
  FieldType := (Children[1] as TSepiTypeNode).SepiType;

  SepiClass := SepiContext as TSepiClass;

  for I := 0 to IdentList.IdentifierCount-1 do
    SepiClass.AddField(IdentList.Identifiers[I], FieldType, I > 0);

  inherited;
end;

{---------------------------------}
{ TSepiSignatureBuilderNode class }
{---------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiSignatureBuilderNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiSignatureBuilderNode then
    TSepiSignatureBuilderNode(Child).SetSignature(Signature);
end;

{*
  Renseigne la signature à construire
  @param ASignature   Signature à construire
*}
procedure TSepiSignatureBuilderNode.SetSignature(ASignature: TSepiSignature);
begin
  FSignature := ASignature;
end;

{------------------------------}
{ TSepiSignatureKindNode class }
{------------------------------}

{*
  Type de signature représentée par ce noeud
  @return Type de signature représentée par ce noeud
*}
function TSepiSignatureKindNode.GetKind: TSepiSignatureKind;
var
  StrKind: string;
  OrdKind: Integer;
begin
  StrKind := AsText;
  OrdKind := AnsiIndexText(StrKind, SignatureKindStrings);

  if OrdKind < 0 then
  begin
    if IsAncestor(TSepiClassMemberDefinitionNode) then
      StrKind := 'class '+StrKind
    else if (SepiContext is TSepiClass) or (SepiContext is TSepiInterface) then
      StrKind := 'object '+AsText
    else
      StrKind := 'static '+StrKind;

    OrdKind := AnsiIndexText(StrKind, SignatureKindStrings);
  end;

  if OrdKind < 0 then
    Result := skStaticProcedure
  else
    Result := TSepiSignatureKind(OrdKind);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSignatureKindNode.EndParsing;
begin
  if Signature <> nil then
    Signature.Kind := Kind;

  inherited;
end;

{------------------------------}
{ TSepiPropertyKindNode class }
{------------------------------}

{*
  [@inheritDoc]
*}
function TSepiPropertyKindNode.GetKind: TSepiSignatureKind;
begin
  if IsAncestor(TSepiClassMemberDefinitionNode) then
    Result := skClassProperty
  else
    Result := skProperty;
end;

{----------------------------------}
{ TSepiCallingConventionNode class }
{----------------------------------}

{*
  Type de signature représentée par ce noeud
  @return Type de signature représentée par ce noeud
*}
function TSepiCallingConventionNode.GetCallingConvention: TCallingConvention;
var
  OrdCallingConvention: Integer;
begin
  OrdCallingConvention := AnsiIndexText(AsText, CallingConventionStrings);

  if OrdCallingConvention < 0 then
    Result := ccRegister
  else
    Result := TCallingConvention(OrdCallingConvention);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCallingConventionNode.EndParsing;
begin
  if Signature <> nil then
    Signature.CallingConvention := CallingConvention;

  inherited;
end;

{----------------------}
{ TSepiParamNode class }
{----------------------}

{*
  [@inheritDoc]
*}
destructor TSepiParamNode.Destroy;
begin
  if FDefaultValuePtr <> nil then
    ParamType.DisposeValue(FDefaultValuePtr);

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiParamNode.ChildBeginParsing(Child: TSepiParseTreeNode);
var
  InitChild: TSepiInitializationExpressionNode;
begin
  inherited;

  if Child is TSepiInitializationExpressionNode then
  begin
    InitChild := TSepiInitializationExpressionNode(Child);

    if OpenArray then
    begin
      Child.MakeError(SOpenArrayParamCantHaveDefaultValue);
      InitChild.SetValueTypeAndPtr(SystemUnit.Integer);
    end else if Length(Names) > 1 then
    begin
      Child.MakeError(SMultiNameParamCantHaveDefaultValue);
      InitChild.SetValueTypeAndPtr(SystemUnit.Integer);
    end else if (Kind in [pkVar, pkOut]) or (ParamType = nil) then
    begin
      Child.MakeError(SByRefParamCantHaveDefaultValue);
      InitChild.SetValueTypeAndPtr(ParamType);
    end else
    begin
      FDefaultValuePtr := ParamType.NewValue;
      InitChild.SetValueTypeAndPtr(ParamType, FDefaultValuePtr);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiParamNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiParamKindNode then
    FKind := TSepiParamKindNode(Child).Kind
  else if Child is TSepiIdentifierDeclListNode then
    TSepiIdentifierDeclListNode(Child).GetIdentifierList(FNames)
  else if Child is TSepiParamIsArrayMarkerNode then
    FOpenArray := True
  else if Child is TSepiTypeNode then
    FType := TSepiTypeNode(Child).SepiType;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiParamNode.EndParsing;
var
  I: Integer;
  Param: TSepiParam;
begin
  Assert(Length(Names) > 0);

  for I := 0 to Length(Names)-1 do
  begin
    Param := TSepiParam.Create(Signature, Names[I], ParamType, Kind, OpenArray);

    if FDefaultValuePtr <> nil then
    begin
      Param.AllocDefaultValue;
      ParamType.CopyData(FDefaultValuePtr^, Param.DefaultValuePtr^);
    end;
  end;

  inherited;
end;

{--------------------------}
{ TSepiParamKindNode class }
{--------------------------}

{*
  Type de paramètre
  @return Type de paramètre
*}
function TSepiParamKindNode.GetKind: TSepiParamKind;
var
  OrdKind: Integer;
begin
  OrdKind := AnsiIndexText(AsText, ParamKindStrings);

  if OrdKind < 0 then
    Result := pkValue
  else
    Result := TSepiParamKind(OrdKind);
end;

{--------------------------}
{ TSepiParamNameNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiParamNameNode.BeginParsing;
var
  Node: TSepiNonTerminal;
begin
  inherited;

  Node := Self;

  while (Node <> nil) and (not (Node is TSepiSignatureBuilderNode)) do
    Node := Node.Parent;

  Assert(Node <> nil);
  FSignature := TSepiSignatureBuilderNode(Node).Signature;
end;

{*
  [@inheritDoc]
*}
function TSepiParamNameNode.IsRedeclared: Boolean;
begin
  Result := Signature.GetParam(Identifier) <> nil;
end;

{*
  [@inheritDoc]
*}
procedure TSepiParamNameNode.MakeErroneousName;
var
  I: Integer;
begin
  inherited;

  I := 1;
  while Signature.GetParam('$'+IntToStr(I)) <> nil do
    Inc(I);

  SetIdentifier('$'+IntToStr(I));
end;

{------------------------------}
{ TSepiSignatureReturnTypeNode }
{------------------------------}

{*
  Adapte le type de signature en procédure (sans valeur de retour)
*}
procedure TSepiSignatureReturnTypeNode.AdaptSignatureKindToProcedure;
begin
  case Signature.Kind of
    skStaticFunction:
      Signature.Kind := skStaticProcedure;
    skObjectFunction:
      Signature.Kind := skObjectProcedure;
    skClassFunction:
      Signature.Kind := skClassProcedure;
  end;
end;

{*
  Adapte le type de signature en fonction (avec valeur de retour)
*}
procedure TSepiSignatureReturnTypeNode.AdaptSignatureKindToFunction;
begin
  case Signature.Kind of
    skStaticProcedure:
      Signature.Kind := skStaticFunction;
    skObjectProcedure:
      Signature.Kind := skObjectFunction;
    skClassProcedure:
      Signature.Kind := skClassFunction;
  end;
end;

{*
  Adapte le type de signature si nécessaire
*}
procedure TSepiSignatureReturnTypeNode.AdaptSignatureKindIfNeeded;
begin
  if MustAdaptSignatureKind then
  begin
    if TypeName = '' then
      AdaptSignatureKindToProcedure
    else
      AdaptSignatureKindToFunction;
  end;
end;

{*
  Si True, adapte Signature.Kind en fonction la présence d'un type de retour
  Si False, la présence d'un type de retour doit se conformer à la valeur
  courante de Signature.Kind.
  Par défaut, AdaptSignatureKind renvoie False, ce qui correspond au
  comportement du langage Delphi.
  @return True s'il faut adapter Signature.Kind, False sinon.
*}
function TSepiSignatureReturnTypeNode.MustAdaptSignatureKind: Boolean;
begin
  Result := False;
end;

{*
  Nom du type de retour
  Le nom du type de retour est toujours la valeur de AsText. Si vous voulez
  modifier ce comportement dans une classe de base, surchargez AsText.
  @return Nom du type de retour, ou '' si absent
*}
function TSepiSignatureReturnTypeNode.GetTypeName: string;
begin
  Result := AsText;
end;

{*
  Compile le fait qu'il n'y a pas de type de retour
  S'il y a un type de retour, émet une erreur.
*}
procedure TSepiSignatureReturnTypeNode.CompileNoReturnType;
begin
  if TypeName <> '' then
    MakeError(SReturnTypeForbidden);

  Signature.ReturnType := nil;
end;

{*
  Compile le type de retour qui doit être présent
  Si le type de retour est absent ou invalide, émet une erreur.
  @return Type de retour
*}
function TSepiSignatureReturnTypeNode.CompileReturnType: TSepiType;
begin
  if TypeName = '' then
  begin
    MakeError(SReturnTypeRequired);
    Result := SystemUnit.Integer;
  end else
  begin
    Result := TSepiType(LookForSelfTextOrError(
      TSepiType, STypeIdentifierRequired));

    if Result = nil then
      Result := SystemUnit.Integer;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiSignatureReturnTypeNode.EndParsing;
begin
  AdaptSignatureKindIfNeeded;

  if Signature.Kind in skWithReturnType then
    Signature.ReturnType := CompileReturnType
  else
    CompileNoReturnType;

  inherited;
end;

{----------------------------------}
{ TSepiMethodDeclarationNode class }
{----------------------------------}

{*
  [@inheritDoc]
*}
destructor TSepiMethodDeclarationNode.Destroy;
begin
  FSignature.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodDeclarationNode.BeginParsing;
begin
  inherited;

  if SepiContext is TSepiInterface then
    FLinkKind := mlkInterface;

  FSignature := TSepiSignature.CreateConstructing(SepiContext);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodDeclarationNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiSignatureBuilderNode then
    TSepiSignatureBuilderNode(Child).SetSignature(Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodDeclarationNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiIdentifierDeclarationNode then
  begin
    // Method name
    FName := TSepiIdentifierDeclarationNode(Child).Identifier;
  end else if Child is TSepiOverloadMarkerNode then
  begin
    // Overloaded method
    FIsOverloaded := True;
  end else if Child is TSepiStaticMarkerNode then
  begin
    // Static marker, i.e. class method without a Self parameter
    case Signature.Kind of
      skClassProcedure: Signature.Kind := skStaticProcedure;
      skClassFunction: Signature.Kind := skStaticFunction;
    else
      Child.MakeError(SStaticMarkerOnlyWithClassMethods);
    end;
  end else if Child is TSepiMethodLinkKindNode then
  begin
    // Link kind - not valid in interface
    if LinkKind = mlkInterface then
      Child.MakeError(SIntfMethodCantChangeLinkKind)
    else if LinkKind <> mlkStatic then
      Child.MakeError(SDuplicatedLinkKind)
    else
    begin
      FLinkKind := TSepiMethodLinkKindNode(Child).LinkKind;
      FMsgID := TSepiMethodLinkKindNode(Child).MessageID;
    end;
  end else if Child is TSepiAbstractMarkerNode then
  begin
    // Abstract method - valid only when LinkKind is virtual or dynamic
    if IsAbstract then
      Child.MakeError(SDuplicatedAbstractMarker)
    else if not (LinkKind in [mlkVirtual, mlkDynamic]) then
      Child.MakeError(SVirtualOrDynamicMethodRequired)
    else
      FAbstract := True;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodDeclarationNode.EndParsing;
begin
  Signature.Complete;

  if (not IsOverloaded) and (LinkKind = mlkOverride) then
  begin
    if TSepiInheritableContainerType(SepiContext).LookForMember(
      Name) is TSepiOverloadedMethod then
      FIsOverloaded := True;
  end;

  if IsOverloaded then
  begin
    TSepiMethod.CreateOverloaded(SepiContext, Name, nil, Signature,
      LinkKind, IsAbstract, MsgID);
  end else
  begin
    TSepiMethod.Create(SepiContext, Name, nil, Signature,
      LinkKind, IsAbstract, MsgID);
  end;

  inherited;
end;

{-------------------------------}
{ TSepiMethodLinkKindNode class }
{-------------------------------}

{*
  Type de liaison
  @return Type de liaison
*}
function TSepiMethodLinkKindNode.GetLinkKind: TMethodLinkKind;
var
  OrdLinkKind: Integer;
begin
  OrdLinkKind := AnsiIndexText(Children[0].AsText, LinkKindStrings);

  if OrdLinkKind < 0 then
    Result := mlkStatic
  else
    Result := TMethodLinkKind(OrdLinkKind);
end;

{*
  ID du message intercepté
  @return ID du message intercepté
*}
function TSepiMethodLinkKindNode.GetMessageID: Integer;
begin
  if LinkKind <> mlkMessage then
    Result := 0
  else
    Result := (Children[1] as TSepiConstExpressionNode).AsInteger;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodLinkKindNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiConstExpressionNode then
    TSepiConstExpressionNode(Child).ValueType := SystemUnit.Integer;
end;

{-------------------------}
{ TSepiPropertyNode class }
{-------------------------}

{*
  [@inheritDoc]
*}
destructor TSepiPropertyNode.Destroy;
begin
  FBuilder.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyNode.BeginParsing;
begin
  inherited;

  FBuilder := TSepiPropertyBuilder.Create(SepiContext);
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiPropertyBuilderNode then
    TSepiPropertyBuilderNode(Child).SetBuilder(Builder)
  else if Child is TSepiSignatureBuilderNode then
    TSepiSignatureBuilderNode(Child).SetSignature(Builder.Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiIdentifierDeclarationNode then
    Builder.Name := TSepiIdentifierDeclarationNode(Child).Identifier;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyNode.EndParsing;
begin
  Builder.Build;

  inherited;
end;

{--------------------------------}
{ TSepiPropertyBuilderNode class }
{--------------------------------}

{*
  Spécifie le constructeur de propriété
  @param ABuilder   Constructeur de propriété
*}
procedure TSepiPropertyBuilderNode.SetBuilder(ABuilder: TSepiPropertyBuilder);
begin
  FBuilder := ABuilder;
  SetSignature(Builder.Signature);
end;

{-----------------------------------}
{ TSepiPropRedefineMarkerNode class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiPropRedefineMarkerNode.EndParsing;
begin
  Builder.Redefine(Self);

  inherited;
end;

{-------------------------------}
{ TSepiPropReadAccessNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiPropReadAccessNode.EndParsing;
begin
  Builder.SetReadAccess(AsText, Self);

  inherited;
end;

{--------------------------------}
{ TSepiPropWriteAccessNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiPropWriteAccessNode.EndParsing;
begin
  Builder.SetWriteAccess(AsText, Self);

  inherited;
end;

{--------------------------}
{ TSepiPropIndexNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiPropIndexNode.EndParsing;
begin
  Builder.SetIndex((Children[0] as TSepiConstExpressionNode).AsReadableValue,
    Self);

  inherited;
end;

{---------------------------------}
{ TSepiPropDefaultValueNode class }
{---------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiPropDefaultValueNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiConstExpressionNode then
    TSepiConstExpressionNode(Child).ValueType := Signature.ReturnType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropDefaultValueNode.EndParsing;
begin
  if ChildCount = 0 then
    Builder.SetNoDefault(Self)
  else
    Builder.SetDefaultValue(
      (Children[0] as TSepiConstExpressionNode).AsReadableValue, Self);

  inherited;
end;

{----------------------------}
{ TSepiPropStorageNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiPropStorageNode.EndParsing;
begin
  Builder.SetStorage((Children[0] as TSepiExpressionNode).Expression, Self);

  inherited;
end;

{----------------------------------}
{ TSepiPropDefaultMarkerNode class }
{----------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiPropDefaultMarkerNode.EndParsing;
begin
  Builder.SetIsDefault(Self);

  inherited;
end;

{-------------------------------------}
{ TSepiIntfMethodRedirectorNode class }
{-------------------------------------}

{*
  Compile l'interface concernée
  @return Interface concernée
*}
function TSepiIntfMethodRedirectorNode.CompileInterface: TSepiInterface;
begin
  Result := TSepiInterface(LookForOrError(Children[0], TSepiInterface,
    SInterfaceTypeRequired));

  if (Result <> nil) and (not SepiClass.ClassImplementsInterface(Result)) then
  begin
    Children[0].MakeError(Format(SClassDoesNotImplementIntf,
      [SepiClass.Name, Result.Name]));

    Result := nil;
  end;
end;

{*
  Compile la méthode d'interface à rediriger
  @return Méthode d'interface à rediriger
*}
function TSepiIntfMethodRedirectorNode.CompileIntfMethod(
  SepiIntf: TSepiInterface): TSepiMethod;
var
  MethodName: string;
begin
  MethodName := Children[1].AsText;

  Result := SepiIntf.LookForMember(MethodName) as TSepiMethod;
  CheckIdentFound(Result, MethodName, Children[1]);
end;

{*
  Nom de la méthode destination
  @return Nom de la méthode destination
*}
function TSepiIntfMethodRedirectorNode.GetRedirectorName: string;
begin
  Result := Children[2].AsText;
end;

{*
  Compile le redirecteur
*}
procedure TSepiIntfMethodRedirectorNode.CompileMethodRedirector;
var
  SepiIntf: TSepiInterface;
  IntfMethod: TSepiMethod;
begin
  SepiIntf := CompileInterface;
  if SepiIntf = nil then
    Exit;

  IntfMethod := CompileIntfMethod(SepiIntf);
  if IntfMethod = nil then
    Exit;

  SepiClass.AddIntfMethodRedirector(SepiIntf, IntfMethod, GetRedirectorName);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIntfMethodRedirectorNode.BeginParsing;
begin
  inherited;

  FSepiClass := SepiContext as TSepiClass;
end;

{*
  [@inheritDoc]
*}
procedure TSepiIntfMethodRedirectorNode.EndParsing;
begin
  CompileMethodRedirector;

  inherited;
end;

{---------------------}
{ TSepiTypeNode class }
{---------------------}

{*
  Renseigne le type représenté
  @param ASepiType   Type représenté
*}
procedure TSepiTypeNode.SetSepiType(ASepiType: TSepiType);
begin
  FSepiType := ASepiType;
end;

{*
  Construit un type erroné
*}
procedure TSepiTypeNode.MakeErroneousType;
begin
  SetSepiType(UnitCompiler.GetErroneousType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiTypeNode.EndParsing;
begin
  if SepiType = nil then
    MakeErroneousType;

  inherited;
end;

{-------------------------}
{ TSepiTypeNameNode class }
{-------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiTypeNameNode.EndParsing;
begin
  SetSepiType(TSepiType(LookForSelfTextOrError(
    TSepiType, STypeIdentifierRequired)));

  inherited;
end;

{-------------------------------}
{ TSepiTypeDefinitionNode class }
{-------------------------------}

{*
  Indique si le type à définir est anonyme
  @return True si le type est anonyme, False sinon
*}
function TSepiTypeDefinitionNode.GetIsAnonymous: Boolean;
begin
  Result := TypeName = '';
end;

{*
  [@inheritDoc]
*}
procedure TSepiTypeDefinitionNode.MakeErroneousType;
begin
  SetSepiType(UnitCompiler.MakeErroneousTypeAlias(TypeName));
end;

{*
  Spécifie le nom type à définir
  @param ATypeName   Nom du type à définir
*}
procedure TSepiTypeDefinitionNode.SetTypeName(const ATypeName: string);
begin
  FTypeName := ATypeName;
end;

{*
  [@inheritDoc]
*}
procedure TSepiTypeDefinitionNode.EndParsing;
var
  Msg: TCMNotifyTypeCreated;
begin
  inherited;

  Msg.MsgID := CM_NOTIFYTYPECREATED;
  Msg.SepiType := SepiType;
  RootNode.Dispatch(Msg);
end;

{-------------------------------}
{ TSepiRecordContentsNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiRecordContentsNode.BeginParsing;
begin
  inherited;

  Assert(SepiContext is TSepiRecordType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRecordContentsNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiRecordFieldNode then
  begin
    if ChildCount = 1 then
      TSepiRecordFieldNode(Child).SetAfterField(AfterField)
    else
      TSepiRecordFieldNode(Child).SetAfterField('~');
  end else if Child is TSepiRecordContentsNode then
  begin
    TSepiRecordContentsNode(Child).SetAfterField(AfterField);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRecordContentsNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiRecordFieldNode then
  begin
    if TSepiRecordFieldNode(Child).LastField <> nil then
      FAfterField := TSepiRecordFieldNode(Child).LastField.Name;
  end else if Child is TSepiRecordCaseHeaderNode then
  begin
    FAfterField := TSepiRecordCaseHeaderNode(Child).AfterField;
  end;

  inherited;
end;

{*
  Spécifie le nom du champ après lequel placer les champs enfants de ce noeud
  @param AAfterField   Nom du champ
*}
procedure TSepiRecordContentsNode.SetAfterField(const AAfterField: string);
begin
  FAfterField := AAfterField;
end;

{----------------------------}
{ TSepiRecordFieldNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiRecordFieldNode.EndParsing;
var
  RecordType: TSepiRecordType;
  IdentList: TSepiIdentifierDeclListNode;
  FieldType: TSepitype;
  I: Integer;
begin
  RecordType := SepiContext as TSepiRecordType;
  IdentList := Children[0] as TSepiIdentifierDeclListNode;
  FieldType := (Children[1] as TSepiTypeNode).SepiType;

  if FieldType <> nil then
  begin
    if AfterField = '~' then
      FLastField := RecordType.AddField(
        IdentList.Identifiers[0], FieldType)
    else
      FLastField := RecordType.AddFieldAfter(
        IdentList.Identifiers[0], FieldType, AfterField);

    for I := 1 to IdentList.IdentifierCount-1 do
      FLastField := RecordType.AddField(
        IdentList.Identifiers[I], FieldType, True);
  end;

  inherited;
end;

{*
  Spécifie le nom du champ après lequel placer les champs enfants de ce noeud
  @param AAfterField   Nom du champ
*}
procedure TSepiRecordFieldNode.SetAfterField(const AAfterField: string);
begin
  FAfterField := AAfterField;
end;

{-------------------------------------}
{ TSepiMethodImplementationNode class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiMethodImplementationNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiForwardMarkerNode then
  begin
    // Forward marker - must be just declared
    if not JustDeclared then
      MakeError(Format(SRedeclaredIdentifier, [SepiMethod.Name]));
  end else if Child is TSepiMethodBodyNode then
  begin
    // Method body - actual implementation
    TSepiMethodBodyNode(Child).SetSepiMethod(SepiMethod);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodImplementationNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
begin
  if Child is TSepiMethodImplHeaderNode then
  begin
    FSepiMethod := TSepiMethodImplHeaderNode(Child).SepiMethod;
    FJustDeclared := TSepiMethodImplHeaderNode(Child).JustDeclared;
  end;

  inherited;
end;

{---------------------------------}
{ TSepiMethodImplHeaderNode class }
{---------------------------------}

{*
  [@inheritDoc]
*}
destructor TSepiMethodImplHeaderNode.Destroy;
begin
  FSignature.Free;

  inherited;
end;

{*
  Déclare la méthode (si autorisé)
*}
procedure TSepiMethodImplHeaderNode.DeclareMethod;
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
  begin
    FSepiMethod := TSepiMethod.CreateOverloaded(SepiContext,
      Name, nil, Signature);
  end else
  begin
    FSepiMethod := TSepiMethod.Create(SepiContext,
      Name, nil, Signature);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodImplHeaderNode.BeginParsing;
begin
  inherited;

  FSignature := TSepiSignature.CreateConstructing(SepiContext);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodImplHeaderNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiSignatureBuilderNode then
    TSepiSignatureBuilderNode(Child).SetSignature(Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodImplHeaderNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiQualifiedIdentNode then
    FName := TSepiQualifiedIdentNode(Child).AsText
  else if Child is TSepiOverloadMarkerNode then
    FIsOverloaded := True;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodImplHeaderNode.EndParsing;
var
  TempMethod: TSepiComponent;
begin
  // Find method
  TempMethod := SepiContext.GetComponent(Name);

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
      MakeError(Format(SPreviousDeclWasNotOverload, [SepiMethod.Name]));
      FIsOverloaded := False;
    end else if not SepiMethod.Signature.Equals(
      Signature, scoDeclaration) then
    begin
      MakeError(Format(SDeclarationDiffersFromPreviousOne, [SepiMethod.Name]));
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
      MakeError(Format(SMethodAlreadyImplemented, [SepiMethod.Name]));
      FSepiMethod := nil;
    end;
  end;

  // Ensure SepiMethod <> nil
  if SepiMethod = nil then
    FSepiMethod := TSepiMethod.Create(SepiContext, '', nil, Signature);

  inherited;
end;

{---------------------------}
{ TSepiMethodBodyNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
function TSepiMethodBodyNode.GetMethodCompiler: TSepiMethodCompiler;
begin
  Result := FCompiler;
end;

{*
  [@inheritDoc]
*}
function TSepiMethodBodyNode.GetSepiContext: TSepiComponent;
begin
  if FCompiler <> nil then
    Result := FCompiler.LocalNamespace
  else
    Result := inherited GetSepiContext;
end;

{*
  [@inheritDoc]
*}
function TSepiMethodBodyNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  Result := LanguageRules.ResolveIdentInMethod(MethodCompiler, Identifier);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodBodyNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiInstructionNode then
    TSepiInstructionNode(Child).InstructionList := MethodCompiler.Instructions;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodBodyNode.EndParsing;
begin
  FCompiler.Complete;

  inherited;
end;

{*
  Spécifie la méthode Sepi dont le corps est compilé dans ce noeud
  Cette méthode doit être appelée une fois avant BeginParsing.
  @param ASepiMethod   Méthode à compiler
*}
procedure TSepiMethodBodyNode.SetSepiMethod(ASepiMethod: TSepiMethod);
begin
  FSepiMethod := ASepiMethod;

  if SepiMethod = nil then
    FCompiler := nil
  else
    FCompiler := UnitCompiler.FindMethodCompiler(SepiMethod, True);
end;

{-----------------------------}
{ TSepiLocalVarNameNode class }
{-----------------------------}

{*
  Modifie la variable locale
  @param ALocalVar   Variable locale
*}
procedure TSepiLocalVarNameNode.SetLocalVar(ALocalVar: TSepiLocalVar);
begin
  FLocalVar := ALocalVar;
end;

{*
  Nom de la variable locale
  @return Nom de la variable locale
*}
function TSepiLocalVarNameNode.GetLocalVarName: string;
begin
  Result := AsText;
end;

{*
  Compile la variable locale
  @return Variable locale
*}
function TSepiLocalVarNameNode.CompileLocalVar: TSepiLocalVar;
begin
  Result := MethodCompiler.Locals.GetVarByName(LocalVarName);

  if Result = nil then
    MakeError(SLocalVarNameRequired);
end;

{*
  Construit une variable erronée
*}
procedure TSepiLocalVarNameNode.MakeErroneousLocalVar;
begin
  SetLocalVar(MethodCompiler.Locals.AddTempVar(SystemUnit.Integer));
end;

{*
  [@inheritDoc]
*}
procedure TSepiLocalVarNameNode.EndParsing;
begin
  if LocalVar = nil then
  begin
    SetLocalVar(CompileLocalVar);
    if LocalVar = nil then
      MakeErroneousLocalVar;
  end;

  inherited;
end;

{-------------------------}
{ TSepiLocalVarNode class }
{-------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiLocalVarNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiIdentifierDeclListNode then
    FNamesNode := TSepiIdentifierDeclListNode(Child)
  else if Child is TSepiTypeNode then
    FVarType := TSepiTypeNode(Child).SepiType;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiLocalVarNode.EndParsing;
var
  I: Integer;
begin
  Assert((NamesNode <> nil) and (VarType <> nil));

  for I := 0 to NamesNode.IdentifierCount-1 do
    MethodCompiler.Locals.AddLocalVar(NamesNode.Identifiers[I], VarType);

  inherited;
end;

{--------------------------------}
{ TSepiInstructionListNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiInstructionListNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TSepiInstructionNode).InstructionList := InstructionList;
end;

{--------------------------------------}
{ TSepiIfThenElseInstructionNode class }
{--------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiIfThenElseInstructionNode.BeginParsing;
begin
  inherited;

  FInstruction := TSepiIfThenElse.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TSepiIfThenElseInstructionNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
const
  TrueBranchChild = 1;
  FalseBranchChild = 2;
begin
  inherited;

  case Child.IndexAsChild of
    TrueBranchChild:
      (Child as TSepiInstructionNode).InstructionList :=
        Instruction.TrueInstructions;
    FalseBranchChild:
      (Child as TSepiInstructionNode).InstructionList :=
        Instruction.FalseInstructions;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiIfThenElseInstructionNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
begin
  if Child is TSepiExpressionNode then
  begin
    Instruction.TestValue :=
      TSepiExpressionNode(Child).AsReadableValue(SystemUnit.Boolean);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiIfThenElseInstructionNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{-----------------------}
{ TSepiCaseOfNode class }
{-----------------------}

{*
  [@inheritDoc]
*}
procedure TSepiCaseOfInstructionNode.BeginParsing;
begin
  inherited;

  FInstruction := TSepiCaseOf.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCaseOfInstructionNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiInstructionListNode then
    TSepiInstructionListNode(Child).InstructionList :=
      Instruction.ElseInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCaseOfInstructionNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiExpressionNode then
    Instruction.TestValue := TSepiExpressionNode(Child).AsReadableValue;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCaseOfInstructionNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{-----------------------------}
{ TSepiCaseOfClauseNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiCaseOfClauseNode.BeginParsing;
begin
  inherited;

  FInstruction := (Parent as TSepiCaseOfInstructionNode).Instruction;
  FClause := FInstruction.AddClause;

  InstructionList := Clause.Instructions;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCaseOfClauseNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiInstructionNode then
    TSepiInstructionNode(Child).InstructionList := InstructionList;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCaseOfClauseNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiExpressionNode then
    Clause.Value := TSepiExpressionNode(Child).AsReadableValue;

  inherited;
end;

{---------------------------------}
{ TSepiWhileInstructionNode class }
{---------------------------------}

{*
  Configure le type de boucle
  @param ATestAtEnd    True si le test est à la fin
  @param AInvertTest   True si le test doit être inverté (not Test)
*}
procedure TSepiWhileInstructionNode.Configure(ATestAtEnd, AInvertTest: Boolean);
begin
  FTestAtEnd := ATestAtEnd;
  FInvertTest := AInvertTest;
end;

{*
  [@inheritDoc]
*}
procedure TSepiWhileInstructionNode.BeginParsing;
begin
  inherited;

  FInstruction := TSepiWhile.Create(MethodCompiler, TestAtEnd);
end;

{*
  [@inheritDoc]
*}
procedure TSepiWhileInstructionNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiInstructionNode then
    TSepiInstructionNode(Child).InstructionList := Instruction.LoopInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TSepiWhileInstructionNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  TestValue: ISepiReadableValue;
begin
  if Child is TSepiExpressionNode then
  begin
    TestValue := TSepiExpressionNode(Child).AsReadableValue(SystemUnit.Boolean);

    if TestValue <> nil then
    begin
      if InvertTest then
        TestValue := TSepiUnaryOperation.MakeOperation(opNot, TestValue);

      Instruction.TestValue := TestValue;
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiWhileInstructionNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{-----------------------------------}
{ TSepiDoWhileInstructionNode class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiDoWhileInstructionNode.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition);
begin
  inherited;

  Configure(True, False);
end;

{---------------------------------------}
{ TSepiRepeatUntilInstructionNode class }
{---------------------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiRepeatUntilInstructionNode.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition);
begin
  inherited;

  Configure(True, True);
end;

{-------------------------------}
{ TSepiForInstructionNode class }
{-------------------------------}

{*
  Construit une valeur de borne erronée
  @return Valeur construite
*}
function TSepiForInstructionNode.MakeErroneousBoundValue: ISepiReadableValue;
begin
  Result := TSepiErroneousValue.Create(SepiRoot);
end;

{*
  [@inheritDoc]
*}
procedure TSepiForInstructionNode.BeginParsing;
begin
  inherited;

  FInstruction := TSepiFor.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TSepiForInstructionNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiInstructionNode then
    TSepiInstructionNode(Child).InstructionList := Instruction.LoopInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TSepiForInstructionNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  BoundValue: ISepiReadableValue;
begin
  if Child is TSepiForControlVarNode then
    Instruction.ControlVar := TSepiForControlVarNode(Child).LocalVar;

  if Child is TSepiExpressionNode then
  begin
    Assert(Instruction.ControlVar <> nil);
    BoundValue := TSepiExpressionNode(Child).AsReadableValue(
      Instruction.ControlVar.VarType);

    if BoundValue = nil then
    begin
      BoundValue := MakeErroneousBoundValue;
      BoundValue.AttachToExpression(
        TSepiForInstructionNode(Child).MakeExpression);
    end;

    if Instruction.StartValue = nil then
      Instruction.StartValue := BoundValue
    else if Instruction.EndValue = nil then
      Instruction.EndValue := BoundValue
    else
      Assert(False);
  end;

  if Child is TSepiForToDownToNode then
    Instruction.IsDownTo := TSepiForToDownToNode(Child).IsDownTo;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiForInstructionNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{------------------------------}
{ TSepiForControlVarNode class }
{------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiForControlVarNode.MakeErroneousLocalVar;
begin
  SetLocalVar((Parent as TSepiForInstructionNode).Instruction.UseTempVar(
    SystemUnit.Integer));
end;

{----------------------}
{ TSepiForToNode class }
{----------------------}

{*
  [@inheritDoc]
*}
function TSepiForToNode.GetIsDownTo: Boolean;
begin
  Result := False;
end;

{--------------------------}
{ TSepiForDownToNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
function TSepiForDownToNode.GetIsDownTo: Boolean;
begin
  Result := True;
end;

{-------------------------------}
{ TSepiTryInstructionNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiTryInstructionNode.BeginParsing;
begin
  inherited;

  FTryInstructions := TSepiInstructionList.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
procedure TSepiTryInstructionNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiExceptFinallyClauseNode then
  begin
    TSepiExceptFinallyClauseNode(Child).SetTryInstructions(TryInstructions);
    TSepiExceptFinallyClauseNode(Child).InstructionList := InstructionList;
  end else
    (Child as TSepiInstructionNode).InstructionList := TryInstructions;
end;

{------------------------------------}
{ TSepiExceptFinallyClauseNode class }
{------------------------------------}

{*
  Spécifie la liste des instructions dans le try
  @param ATryInstructions   Liste des instructions dans le try
*}
procedure TSepiExceptFinallyClauseNode.SetTryInstructions(
  ATryInstructions: TSepiInstructionList);
begin
  FTryInstructions := ATryInstructions;
end;

{-----------------------------}
{ TSepiExceptClauseNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiExceptClauseNode.BeginParsing;
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
procedure TSepiExceptClauseNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TSepiInstructionNode).InstructionList :=
    Instruction.ExceptInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TSepiExceptClauseNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{------------------------}
{ TSepiMultiOnNode class }
{------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiMultiOnNode.BeginParsing;
begin
  inherited;

  FInstruction := TSepiMultiOn.Create(MethodCompiler);
  FInstruction.ExceptObjectVar :=
    (Parent as TSepiExceptClauseNode).Instruction.UseTempVar;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMultiOnNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiMultiOnElseClauseNode then
    TSepiMultiOnElseClauseNode(Child).InstructionList :=
      Instruction.ElseInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMultiOnNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{-------------------------}
{ TSepiOnClauseNode class }
{-------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiOnClauseNode.BeginParsing;
begin
  inherited;

  FInstruction := (Parent as TSepiMultiOnNode).Instruction;
end;

{*
  [@inheritDoc]
*}
procedure TSepiOnClauseNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiInstructionNode then
    TSepiInstructionNode(Child).InstructionList := InstructionList;
end;

{*
  [@inheritDoc]
*}
procedure TSepiOnClauseNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TSepiExceptionVarAndTypeNode then
  begin
    FExceptObjectVarName := TSepiExceptionVarAndTypeNode(Child).Identifier;
    FExceptObjectClass :=
      TSepiExceptionVarAndTypeNode(Child).SepiType as TSepiClass;

    InstructionList := Instruction.AddOnClause(ExceptObjectClass);

    if ExceptObjectVarName <> '' then
    begin
      FExceptObjectVar := MethodCompiler.Locals.AddAbsolute(
        '', ExceptObjectClass, Instruction.ExceptObjectVar);
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiOnClauseNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
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

{----------------------------------}
{ TSepiMaybeIdentAndTypeNode class }
{----------------------------------}

{*
  Modifie l'identificateur
*}
procedure TSepiMaybeIdentAndTypeNode.SetIdentifier(const AIdentifier: string);
begin
  FIdentifier := AIdentifier;
end;

{*
  Compile le noeud identificateur
  @param Child   Noeud identificateur
  @return Identificateur
*}
procedure TSepiMaybeIdentAndTypeNode.CompileIdentifier(
  Child: TSepiParseTreeNode);
begin
  if Child.ChildCount > 1 then
    Child.MakeError(SIdentifierRequired)
  else
    SetIdentifier(Child.AsText);
end;

{*
  Compile le noeud type
  @param Child   Noeud type
  @return Type
*}
procedure TSepiMaybeIdentAndTypeNode.CompileType(Child: TSepiParseTreeNode);
begin
  SetSepiType(TSepiType(LookForOrError(Child, TSepiType,
    STypeIdentifierRequired)));
end;

{*
  [@inheritDoc]
*}
procedure TSepiMaybeIdentAndTypeNode.EndParsing;
begin
  if ChildCount = 1 then
    CompileType(Children[0])
  else
  begin
    CompileIdentifier(Children[0]);
    CompileType(Children[1]);
  end;

  inherited;
end;

{------------------------------------}
{ TSepiExceptionVarAndTypeNode class }
{------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiExceptionVarAndTypeNode.CompileType(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if (SepiType <> nil) and (not (SepiType is TSepiClass)) then
  begin
    Child.MakeError(SClassTypeRequired);
    SetSepiType(nil);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiExceptionVarAndTypeNode.MakeErroneousType;
begin
  SetSepiType(SystemUnit.TObject);
end;

{----------------------------------}
{ TSepiMultiOnElseClauseNode class }
{----------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiMultiOnElseClauseNode.EndParsing;
begin
  if ChildCount = 0 then
    InstructionList.Add(TSepiReraise.Create(MethodCompiler));

  inherited;
end;

{------------------------------}
{ TSepiFinallyClauseNode class }
{------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiFinallyClauseNode.BeginParsing;
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
procedure TSepiFinallyClauseNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TSepiInstructionNode).InstructionList :=
    Instruction.FinallyInstructions;
end;

{*
  [@inheritDoc]
*}
procedure TSepiFinallyClauseNode.EndParsing;
begin
  InstructionList.Add(Instruction);

  inherited;
end;

{---------------------------------}
{ TSepiRaiseInstructionNode class }
{---------------------------------}

{*
  Construit l'instruction raise avec un objet
*}
procedure TSepiRaiseInstructionNode.MakeRaiseInstruction;
var
  Instruction: TSepiRaise;
  ExceptionValue: ISepiReadableValue;
begin
  // Get exception value
  ExceptionValue := (Children[0] as TSepiExpressionNode).AsReadableValue(
    SystemUnit.TObject);

  // Make instruction
  if ExceptionValue <> nil then
  begin
    Instruction := TSepiRaise.Create(MethodCompiler);
    Instruction.ExceptionValue := ExceptionValue;
    InstructionList.Add(Instruction);
  end;
end;

{*
  Construit l'instruction raise sans objet (reraise)
*}
procedure TSepiRaiseInstructionNode.MakeReraiseInstruction;
var
  Instruction: TSepiReraise;
begin
  Instruction := TSepiReraise.Create(MethodCompiler);
  InstructionList.Add(Instruction);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRaiseInstructionNode.EndParsing;
begin
  if ChildCount > 0 then
    MakeRaiseInstruction
  else
    MakeReraiseInstruction;

  inherited;
end;

{---------------------------------------------}
{ TSepiExecuteExpressionInstructionNode class }
{---------------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiExecuteExpressionInstructionNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
var
  Expression: ISepiExpression;
  Executable: ISepiExecutable;
  Instruction: TSepiExecuteExpression;
begin
  Expression := (Child as TSepiExpressionNode).Expression;

  if Supports(Expression, ISepiExecutable, Executable) then
  begin
    Instruction := TSepiExecuteExpression.Create(MethodCompiler);
    Instruction.Executable := Executable;
    InstructionList.Add(Instruction);
  end else
  begin
    Expression.MakeError(SExecutableRequired);
  end;

  inherited;
end;

{--------------------------------}
{ TSepiWithInstructionNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiWithInstructionNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiInstructionNode then
    TSepiInstructionNode(Child).InstructionList := InstructionList;
end;

{*
  [@inheritDoc]
*}
procedure TSepiWithInstructionNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  AssignInstr: TSepiAssignment;
begin
  if Child is TSepiExpressionNode then
  begin
    FWithValue := TSepiExpressionNode(Child).AsReadableValue;

    // Check type
    if not (FWithValue.ValueType is TSepiContainerType) then
    begin
      Child.MakeError(SContainerTypeRequired);
      FWithValue := TSepiErroneousValue.MakeReplacementValue(
        FWithValue as ISepiExpression, SystemUnit.TObject);
    end;

    // Build with var and read it
    FWithVar := MethodCompiler.Locals.AddTempVar(FWithValue.ValueType);
    AssignInstr := TSepiAssignment.Create(MethodCompiler);
    AssignInstr.Destination := TSepiLocalVarValue.MakeValue(
      MethodCompiler, FWithVar) as ISepiWritableValue;
    AssignInstr.Source := FWithValue;
    AssignInstr.SourcePos := SourcePos;
    InstructionList.Add(AssignInstr);

    // Get new with value
    FWithValue := TSepiLocalVarValue.MakeValue(MethodCompiler,
      FWithVar) as ISepiReadableValue;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiWithInstructionNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  if WithValue <> nil then
    Result := LanguageRules.FieldSelection(SepiContext,
      WithValue as ISepiExpression, Identifier);

  if Result = nil then
    Result := inherited ResolveIdent(Identifier);
end;

end.

