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
  SysUtils, Classes, StrUtils, TypInfo, SysConst, ScUtils, ScDelphiLanguage,
  SepiReflectionCore, SepiMembers, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiDelphiLexer, SepiDelphiParser, SepiCompilerErrors, SepiParseTrees,
  SepiCompiler, SepiCore, SepiCompilerConsts, SepiExpressions,
  SepiDelphiCompilerConsts, SepiOpCodes, SepiDelphiLikeCompilerUtils,
  SepiLL1ParserUtils, Types, Windows, SepiInstructions;

type
  TRootNode = class;

  {*
    Noeud d'un source Delphi
    @author sjrd
    @version 1.0
  *}
  TDelphiSourceNode = class(TSepiNonTerminal)
  private
    function GetRootNode: TRootNode;
  protected
    function ValueAsInt64(const Value: ISepiReadableValue): Int64;
    procedure TryAndConvertValues(
      var LowerValue, HigherValue: ISepiReadableValue);
  public
    function LookFor(const Name: string): TSepiMeta; overload; virtual;
    function LookFor(Node: TSepiParseTreeNode;
      RequiredClass: SepiReflectionCore.TSepiMetaClass;
      const ErrorMsg: string): TSepiMeta; overload;

    property RootNode: TRootNode read GetRootNode;
  end;

  {*
    Noeud racine
    @author sjrd
    @version 1.0
  *}
  TRootNode = class(TSepiParseTreeRootNode)
  private
    FMinemumSize: TSepiEnumMinemumSize; /// Taille minimale d'énumération
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure EndParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property MinemumSize: TSepiEnumMinemumSize read FMinemumSize;
  end;

  {*
    Noeud interface
    @author sjrd
    @version 1.0
  *}
  TInterfaceNode = class(TDelphiSourceNode)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud implémentation
    @author sjrd
    @version 1.0
  *}
  TImplementationNode = class(TDelphiSourceNode)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud section uses
    @author sjrd
    @version 1.0
  *}
  TUsesSectionNode = class(TDelphiSourceNode)
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Identificateur qualifié
    @author sjrd
    @version 1.0
  *}
  TQualifiedIdentNode = class(TDelphiSourceNode)
  protected
    function GetAsText: string; override;
  end;

  {*
    Noeud d'expression d'initialisation
    @author sjrd
    @version 1.0
  *}
  TInitializationExpressionNode = class(TDelphiSourceNode)
  private
    /// Pointeur sur la valeur à initialiser (peut être nil en cas d'erreur)
    FValuePtr: Pointer;
    FValueType: TSepiType; /// Type de valeur
  public
    procedure BeginParsing; override;

    property ValuePtr: Pointer read FValuePtr write FValuePtr;
    property ValueType: TSepiType read FValueType write FValueType;
  end;

  {*
    Sous-noeud d'expression d'initialisation
    @author sjrd
    @version 1.0
  *}
  TInitializationExprSubNode = class(TDelphiSourceNode)
  private
    /// Pointeur sur la valeur à initialiser (peut être nil en cas d'erreur)
    FValuePtr: Pointer;
    FValueType: TSepiType; /// Type de valeur
  public
    procedure BeginParsing; override;

    property ValuePtr: Pointer read FValuePtr;
    property ValueType: TSepiType read FValueType;
  end;

  {*
    Noeud d'expression d'initialisation d'un tableau
    @author sjrd
    @version 1.0
  *}
  TArrayInitializationNode = class(TInitializationExprSubNode)
  private
    FArrayType: TSepiStaticArrayType; /// Type de valeur comme type tableau
    FElementType: TSepiType;          /// Type des éléments du tableau
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property ArrayType: TSepiStaticArrayType read FArrayType;
    property ElementType: TSepiType read FElementType;
  end;

  {*
    Noeud d'expression d'initialisation d'un record
    @author sjrd
    @version 1.0
  *}
  TRecordInitializationNode = class(TInitializationExprSubNode)
  private
    FRecordType: TSepiRecordType; /// Type de valeur comme type record
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;

    property RecordType: TSepiRecordType read FRecordType write FRecordType;
  end;

  {*
    Noeud d'expression d'initialisation d'un GUID
    @author sjrd
    @version 1.0
  *}
  TGUIDInitializationNode = class(TInitializationExprSubNode)
  private
    FRecordType: TSepiRecordType; /// Type de valeur comme type record
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;

    property RecordType: TSepiRecordType read FRecordType write FRecordType;
  end;

  {*
    Noeud d'expression d'initialisation d'un autre type que tableau ou record
    @author sjrd
    @version 1.0
  *}
  TOtherInitializationNode = class(TInitializationExprSubNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud qui peut être compilé en une expression
    @author sjrd
    @version 1.0
  *}
  TExprNode = class(TDelphiSourceNode)
  private
    FExpression: ISepiExpression; /// Expression compilée
  public
    procedure EndParsing; override;

    function AsValue(ValueType: TSepiType = nil;
      AllowConvertion: Boolean = True): ISepiValue;
    function AsType: TSepiType;

    property Expression: ISepiExpression read FExpression write FExpression;
  end;

  {*
    Noeud expression
    @author sjrd
    @version 1.0
  *}
  TExpressionNode = class(TExprNode)
  private
    function CompileTree(Lower, Higher: Integer): ISepiExpression;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud expression constante
    @author sjrd
    @version 1.0
  *}
  TConstExpressionNode = class(TExprNode)
  private
    FAcceptType: Boolean; /// Indique si peut accepter un identificateur de type
  public
    procedure EndParsing; override;

    function CompileConst(var Value;
      ValueType: TSepiType): Boolean; overload; virtual;
    function CompileConst(var Value;
      ValueTypeInfo: PTypeInfo): Boolean; overload;

    property AcceptType: Boolean read FAcceptType write FAcceptType;
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
  TUnaryOpNode = class(TDelphiSourceNode)
  public
    function MakeExpression(const Operand: ISepiExpression): ISepiExpression;
  end;

  {*
    Noeud opérateur binaire
    @author sjrd
    @version 1.0
  *}
  TBinaryOpNode = class(TDelphiSourceNode)
  private
    function HandlePointerArithm(
      var LeftValue, RightValue: ISepiReadableValue): TSepiType;

    function GetPriority: Integer;
  public
    function MakeExpression(
      const LeftExpression, RightExpression: ISepiExpression): ISepiExpression;

    property Priority: Integer read GetPriority;
  end;

  {*
    Noeud expression simple (sans opérateurs binaires)
    @author sjrd
    @version 1.0
  *}
  TSingleExprNode = class(TExprNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud expression parenthésée
    @author sjrd
    @version 1.0
  *}
  TParenthesizedExprNode = class(TExprNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud valeur simple
    @author sjrd
    @version 1.0
  *}
  TSingleValueNode = class(TExprNode)
  private
    function MakeIntegerLiteral(Value: Int64): ISepiReadableValue;
    function MakeExtendedLiteral(Value: Extended): ISepiReadableValue;
    function MakeCharLiteral(Value: AnsiChar): ISepiReadableValue; overload;
    function MakeCharLiteral(Value: WideChar): ISepiReadableValue; overload;
    function MakeStringLiteral(
      const Value: AnsiString): ISepiReadableValue; overload;
    function MakeStringLiteral(
      const Value: WideString): ISepiReadableValue; overload;

    procedure HandleInherited(Child: TSepiParseTreeNode);
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud valeur ensemble
  *}
  TSetValueNode = class(TExprNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Classe de base pour les noeuds qui suivent une expression single
    @author sjrd
    @version 1.0
  *}
  TNextExprNode = class(TExprNode)
  private
    FBase: ISepiExpression; /// Expression de base
  public
    procedure EndParsing; override;

    property Base: ISepiExpression read FBase write FBase;
  end;

  {*
    Noeud paramètres réels
    @author sjrd
    @version 1.0
  *}
  TParametersNode = class(TNextExprNode)
  private
    procedure CompileCastOrConvert(DestType: TSepiType);
    procedure CompileCast(const PseudoRoutine: ISepiCastPseudoRoutine);
    procedure CompileTypeOperation(
      const TypeOperation: ISepiTypeOperationPseudoRoutine;
      const TypeExpression: ISepiTypeExpression);
    procedure CompileCall(const Callable: ISepiCallable);
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud index de tableau
    @author sjrd
    @version 1.0
  *}
  TArrayIndicesNode = class(TNextExprNode)
  private
    procedure CompileProperty(const Prop: ISepiProperty);
    procedure CompileArrayItem(const BaseValue: ISepiValue);
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud sélection de champ
    @author sjrd
    @version 1.0
  *}
  TFieldSelectionNode = class(TNextExprNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud déréférencement
    @author sjrd
    @version 1.0
  *}
  TDereferenceNode = class(TNextExprNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur de type
    @author sjrd
    @version 1.0
  *}
  TTypeDescNode = class(TDelphiSourceNode)
  private
    FTypeName: string;     /// Nom du type
    FIsAnonymous: Boolean; /// Indique si le type est anonyme
    FIsPacked: Boolean;    /// Indique si le type est packed

    FSepiType: TSepiType; /// Type Sepi compilé
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property TypeName: string read FTypeName write FTypeName;
    property IsAnonymous: Boolean read FIsAnonymous;
    property IsPacked: Boolean read FIsPacked;

    property SepiType: TSepiType read FSepiType;
  end;

  {*
    Noeud d'élément d'une section (type, constante, variable ou routine)
  *}
  TSectionItemNode = class(TDelphiSourceNode)
  end;

  {*
    Noeud déclaration de type
    @author sjrd
    @version 1.0
  *}
  TTypeDeclNode = class(TSectionItemNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud déclaration de constante
    @author sjrd
    @version 1.0
  *}
  TConstantDeclNode = class(TSectionItemNode)
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
  TVariableDeclNode = class(TSectionItemNode)
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
    Noeud déclaration de routine
    @author sjrd
    @version 1.0
  *}
  TRoutineDeclNode = class(TSectionItemNode)
  private
    FName: string;              /// Nom de la routine
    FSignature: TSepiSignature; /// Signature
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
    property IsOverloaded: Boolean read FIsOverloaded;
  end;

  {*
    Classe de base pour les descripteurs de types spécialisés
    @author sjrd
    @version 1.0
  *}
  TTypeDescriptorNode = class(TDelphiSourceNode)
  private
    FTypeName: string;  /// Nom du type
    FIsPacked: Boolean; /// Indique si le type doit être packed

    FSepiType: TSepiType; /// Type Sepi compilé
  public
    property TypeName: string read FTypeName write FTypeName;
    property IsPacked: Boolean read FIsPacked write FIsPacked;

    property SepiType: TSepiType read FSepiType write FSepiType;
  end;

  {*
    Noeud descripteur d'un clone de type
    @author sjrd
    @version 1.0
  *}
  TTypeCloneNode = class(TTypeDescriptorNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type intervalle ou énumération
  *}
  TRangeOrEnumTypeNode = class(TTypeDescriptorNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type intervalle
    @author sjrd
    @version 1.0
  *}
  TRangeTypeNode = class(TTypeDescriptorNode)
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
  TEnumTypeNode = class(TTypeDescriptorNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type ensemble
    @author sjrd
    @version 1.0
  *}
  TSetTypeNode = class(TTypeDescriptorNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type chaîne courte
    @author sjrd
    @version 1.0
  *}
  TStringTypeNode = class(TTypeDescriptorNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type pointeur
    @author sjrd
    @version 1.0
  *}
  TPointerTypeNode = class(TTypeDescriptorNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type tableau
    @author sjrd
    @version 1.0
  *}
  TArrayTypeNode = class(TTypeDescriptorNode)
  private
    function RangeDefinition(const TypeName: string;
      RangeNode: TSepiParseTreeNode; ElementType: TSepiType): TSepiStaticArrayType;
    function TypedDefinition(const TypeName: string;
      IndexNode: TConstExpressionNode; ElementType: TSepiType): TSepiStaticArrayType;
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud descripteur d'un type record
    @author sjrd
    @version 1.0
  *}
  TRecordTypeNode = class(TTypeDescriptorNode)
  private
    FRecordType: TSepiRecordType; /// Type record
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    property RecordType: TSepiRecordType read FRecordType;
  end;

  {*
    Noeud descripteur de contenu de record
    @author sjrd
    @version 1.0
  *}
  TRecordContentsNode = class(TDelphiSourceNode)
  private
    FRecordType: TSepiRecordType; /// Type record
    FAfterField: string;          /// Nom du champ après lequel se placer
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    property RecordType: TSepiRecordType read FRecordType write FRecordType;
    property AfterField: string read FAfterField write FAfterField;
  end;

  {*
    Noeud descripteur d'un type classe
    @author sjrd
    @version 1.0
  *}
  TClassTypeNode = class(TTypeDescriptorNode)
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
  TInterfaceTypeNode = class(TTypeDescriptorNode)
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
    Noeud descripteur d'un type référence de méthode
    @author sjrd
    @version 1.0
  *}
  TMethodRefTypeNode = class(TTypeDescriptorNode)
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
  TMemberListNode = class(TDelphiSourceNode)
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
  TMemberNode = class(TDelphiSourceNode)
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
    FAfterField: string;    /// Champ après lequel se placer (record seulement)
    FNames: TStrings;       /// Noms des champs
    FFieldType: TSepiType;  /// Type des champs
    FLastField: TSepiField; /// Dernier champ compilé
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    property AfterField: string read FAfterField write FAfterField;
    property Names: TStrings read FNames;
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
    Noeud en rapport à une signature
    @author sjrd
    @version 1.0
  *}
  TInSignatureNode = class(TDelphiSourceNode)
  private
    FSignature: TSepiSignature; /// Signature à compiler
  public
    property Signature: TSepiSignature read FSignature write FSignature;
  end;

  {*
    Noeud signature
    @author sjrd
    @version 1.0
  *}
  TSignatureNode = class(TInSignatureNode)
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud paramètre
    @author sjrd
    @version 1.0
  *}
  TParamNode = class(TInSignatureNode)
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
  TReturnTypeNode = class(TInSignatureNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud méthode dans l'implémentation
    @author sjrd
    @version 1.0
  *}
  TMethodImplNode = class(TSectionItemNode)
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
  TMethodImplDeclNode = class(TSectionItemNode)
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
  TMethodBodyNode = class(TDelphiSourceNode)
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

    property SepiMethod: TSepiMethod read FSepiMethod write FSepiMethod;
  end;

  {*
    Noeud déclaration de variable locale
    @author sjrd
    @version 1.0
  *}
  TLocalVarNode = class(TSectionItemNode)
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
  TInstructionNode = class(TDelphiSourceNode)
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

implementation

{-----------------}
{ Global routines }
{-----------------}

{*
  Initialise le tableau NonTerminalClasses
*}
procedure InitNonTerminalClasses;
begin
  NonTerminalClasses[ntSource]         := TRootNode;
  NonTerminalClasses[ntInterface]      := TInterfaceNode;
  NonTerminalClasses[ntImplementation] := TImplementationNode;
  NonTerminalClasses[ntUsesSection]    := TUsesSectionNode;

  NonTerminalClasses[ntQualifiedIdent] := TQualifiedIdentNode;

  NonTerminalClasses[ntTypeDesc]    := TTypeDescNode;
  NonTerminalClasses[ntTypeDecl]    := TTypeDeclNode;
  NonTerminalClasses[ntConstDecl]   := TConstantDeclNode;
  NonTerminalClasses[ntGlobalVar]   := TVariableDeclNode;
  NonTerminalClasses[ntRoutineDecl] := TRoutineDeclNode;

  NonTerminalClasses[ntInitializationExpression] :=
    TInitializationExpressionNode;
  NonTerminalClasses[ntArrayInitialization]  := TArrayInitializationNode;
  NonTerminalClasses[ntRecordInitialization] := TRecordInitializationNode;
  NonTerminalClasses[ntGUIDInitialization]   := TGUIDInitializationNode;
  NonTerminalClasses[ntOtherInitialization]  := TOtherInitializationNode;

  NonTerminalClasses[ntExpression]      := TExpressionNode;
  NonTerminalClasses[ntConstExpression] := TConstExpressionNode;
  NonTerminalClasses[ntConstOrType]     := TConstOrTypeNode;

  NonTerminalClasses[ntBinaryOp] := TBinaryOpNode;
  NonTerminalClasses[ntUnaryOp]  := TUnaryOpNode;

  NonTerminalClasses[ntSingleExpr]        := TSingleExprNode;
  NonTerminalClasses[ntParenthesizedExpr] := TParenthesizedExprNode;
  NonTerminalClasses[ntSingleValue]       := TSingleValueNode;
  NonTerminalClasses[ntSetValue]          := TSetValueNode;
  NonTerminalClasses[ntParameters]        := TParametersNode;
  NonTerminalClasses[ntArrayIndices]      := TArrayIndicesNode;
  NonTerminalClasses[ntFieldSelection]    := TFieldSelectionNode;
  NonTerminalClasses[ntDereference]       := TDereferenceNode;

  NonTerminalClasses[ntCloneDesc]         := TTypeCloneNode;
  NonTerminalClasses[ntRangeOrEnumDesc]   := TRangeOrEnumTypeNode;
  NonTerminalClasses[ntRangeDesc]         := TRangeTypeNode;
  NonTerminalClasses[ntEnumDesc]          := TEnumTypeNode;
  NonTerminalClasses[ntSetDesc]           := TSetTypeNode;
  NonTerminalClasses[ntStringDesc]        := TStringTypeNode;
  NonTerminalClasses[ntPointerDesc]       := TPointerTypeNode;
  NonTerminalClasses[ntArrayDesc]         := TArrayTypeNode;
  NonTerminalClasses[ntRecordDesc]        := TRecordTypeNode;
  NonTerminalClasses[ntClassDesc]         := TClassTypeNode;
  NonTerminalClasses[ntInterfaceDesc]     := TInterfaceTypeNode;
  NonTerminalClasses[ntDispInterfaceDesc] := TInterfaceTypeNode;
  NonTerminalClasses[ntEventDesc]         := TMethodRefTypeNode;

  NonTerminalClasses[ntRecordContents]     := TRecordContentsNode;
  NonTerminalClasses[ntRecordCaseBlock]    := TRecordContentsNode;
  NonTerminalClasses[ntRecordCaseContents] := TRecordContentsNode;

  NonTerminalClasses[ntClassMemberLists]    := TMemberListNode;
  NonTerminalClasses[ntInterfaceMemberList] := TMemberListNode;

  NonTerminalClasses[ntField]           := TFieldNode;
  NonTerminalClasses[ntRecordCaseField] := TFieldNode;
  NonTerminalClasses[ntMethodDecl]      := TMethodNode;
  NonTerminalClasses[ntPropertyDecl]    := TPropertyNode;
  NonTerminalClasses[ntVisibility]      := TVisibilityNode;

  NonTerminalClasses[ntMethodSignature]   := TSignatureNode;
  NonTerminalClasses[ntPropertySignature] := TSignatureNode;
  NonTerminalClasses[ntParam]             := TParamNode;
  NonTerminalClasses[ntReturnType]        := TReturnTypeNode;
  NonTerminalClasses[ntPropType]          := TReturnTypeNode;

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
  NonTerminalClasses[ntFinallyClause]         := TFinallyClauseNode;
  NonTerminalClasses[ntRaiseInstruction]      := TRaiseInstructionNode;
  NonTerminalClasses[ntExpressionInstruction] := TExpressionInstructionNode;
end;

{*
  Vérifie qu'un identificateur a bien été trouvé, et produit une erreur sinon
  @param Expression   Expression à vérifier
  @param Identifier   Identificateur, pour le message d'erreur
  @param Node         Symbole de grammaire, pour la position
  @return True si le type a été trouvé, False sinon
*}
function CheckIdentFound(const Expression: ISepiExpression;
  const Identifier: string; Node: TSepiParseTreeNode): Boolean; overload;
begin
  Result := Expression <> nil;
  if not Result then
    Node.MakeError(Format(SIdentifierNotFound, [Identifier]));
end;

{*
  Vérifie qu'un identificateur a bien été trouvé, et produit une erreur sinon
  @param Meta         Meta à vérifier
  @param Identifier   Identificateur, pour le message d'erreur
  @param Node         Symbole de grammaire, pour la position
  @return True si le type a été trouvé, False sinon
*}
function CheckIdentFound(Meta: TSepiMeta;
  const Identifier: string; Node: TSepiParseTreeNode): Boolean; overload;
begin
  Result := Meta <> nil;
  if not Result then
    Node.MakeError(Format(SIdentifierNotFound, [Identifier]));
end;

{*
  Vérifie que des types correspondent, et produit une erreur sinon
  @param AssignedType     Type de la variable assignée
  @param ExpressionType   Type de l'expression
  @param Node             Symbole de grammaire, pour la position
  @return True si les types correspondent, False sinon
*}
function CheckTypeMatch(AssignedType, ExpressionType: TSepiType;
  Node: TSepiParseTreeNode): Boolean;
begin
  Result := AssignedType.CompatibleWith(ExpressionType);
  if not Result then
    Node.MakeError(
      Format(STypeMismatch, [AssignedType.Name, ExpressionType.Name]));
end;

{-------------------------}
{ TDelphiSourceNode class }
{-------------------------}

function TDelphiSourceNode.GetRootNode: TRootNode;
begin
  Result := TSepiNonTerminal(Self).RootNode as TRootNode;
end;

{*
  Lit une valeur comme un Int64
  @param Value   Valeur à lire
  @return Valeur sous forme d'Int64
*}
function TDelphiSourceNode.ValueAsInt64(const Value: ISepiReadableValue): Int64;
var
  IntegerType: TSepiIntegerType;
begin
  if Value.ValueType is TSepiInt64Type then
    Result := Int64(Value.ConstValuePtr^)
  else begin
    IntegerType := TSepiIntegerType(Value.ValueType);

    if IntegerType.Signed then
      Result := IntegerType.ValueAsInteger(Value.ConstValuePtr^)
    else
      Result := IntegerType.ValueAsCardinal(Value.ConstValuePtr^);
  end;
end;

{*
  Essaie de convertir les valeurs pour qu'elles aient le même type
  @param LowerValue    Valeur basse
  @param HigherValue   Valeur haute
*}
procedure TDelphiSourceNode.TryAndConvertValues(
  var LowerValue, HigherValue: ISepiReadableValue);
var
  LowerType, HigherType, CommonType: TSepiType;
  IntLowerValue, IntHigherValue: Int64;
begin
  LowerType := LowerValue.ValueType;
  HigherType := HigherValue.ValueType;

  if (LowerType is TSepiEnumType) and (HigherType is TSepiEnumType) then
  begin
    // Enumeration types

    CommonType := TSepiEnumType(LowerType).BaseType;

    if TSepiEnumType(HigherType).BaseType = CommonType then
    begin
      if LowerType <> CommonType then
        LowerValue := TSepiCastOperator.CastValue(
          CommonType, LowerValue) as ISepiReadableValue;

      if HigherType <> CommonType then
        HigherValue := TSepiCastOperator.CastValue(
          CommonType, HigherValue) as ISepiReadableValue;
    end;
  end else
  begin
    // Integer or char types

    if ((LowerType is TSepiIntegerType) or (LowerType is TSepiInt64Type)) and
      ((HigherType is TSepiIntegerType) or (HigherType is TSepiInt64Type)) then
    begin
      // Integer types

      IntLowerValue := ValueAsInt64(LowerValue);
      IntHigherValue := ValueAsInt64(HigherValue);

      if (Integer(IntLowerValue) = IntLowerValue) and
        (Integer(IntHigherValue) = IntHigherValue) then
        CommonType := SepiRoot.FindType(TypeInfo(Integer))
      else if (Cardinal(IntLowerValue) = IntLowerValue) and
        (Cardinal(IntHigherValue) = IntHigherValue) then
        CommonType := SepiRoot.FindType(TypeInfo(Cardinal))
      else
        CommonType := SepiRoot.FindType(TypeInfo(Int64));
    end else if (LowerType is TSepiCharType) and
      (HigherType is TSepiCharType) then
    begin
      // Char types

      if LowerType.Size >= HigherType.Size then
        CommonType := LowerType
      else
        CommonType := HigherType;
    end else
    begin
      // Error
      Exit;
    end;

    if LowerType <> CommonType then
      LowerValue := TSepiConvertOperation.ConvertValue(
        CommonType, LowerValue);

    if HigherType <> CommonType then
      HigherValue := TSepiConvertOperation.ConvertValue(
        CommonType, HigherValue) as ISepiReadableValue;
  end;
end;

{*
  Recherche un meta dans le contexte de ce noeud
  @param Name   Nom du meta
  @return Meta trouvé, ou nil si non trouvé
*}
function TDelphiSourceNode.LookFor(const Name: string): TSepiMeta;
begin
  Result := SepiContext.LookFor(Name);
end;

{*
  Recherche un meta d'un type particulier
  @param Node            Noeud dont le texte est le nom du meta à rechercher
  @param RequiredClass   Classe de meta requise
  @param ErrorMsg        Message d'erreur si mauvaise classe de meta
*}
function TDelphiSourceNode.LookFor(Node: TSepiParseTreeNode;
  RequiredClass: SepiReflectionCore.TSepiMetaClass;
  const ErrorMsg: string): TSepiMeta;
var
  Ancestor: TSepiParseTreeNode;
  Meta: TSepiMeta;
begin
  Ancestor := Node;
  while (Ancestor <> nil) and (not (Ancestor is TDelphiSourceNode)) do
    Ancestor := Ancestor.Parent;

  if Ancestor = nil then
    Meta := nil
  else
    Meta := TDelphiSourceNode(Ancestor).LookFor(Node.AsText);

  if CheckIdentFound(Meta, Node.AsText, Node) and
    (not (Meta is RequiredClass)) then
  begin
    Node.MakeError(ErrorMsg);
    Meta := nil;
  end;

  Result := TSepiType(Meta);
end;

{-----------------}
{ TRootNode class }
{-----------------}

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

{------------------------}
{ TUsesSectionNode class }
{------------------------}

{*
  [@inheritDoc]
*}
procedure TUsesSectionNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  UnitName: string;
  Redeclared: Boolean;
  I: Integer;
begin
  UnitName := Child.AsText;
  Redeclared := False;

  if AnsiSameText(UnitName, SepiUnit.Name) then
    Redeclared := True
  else
  begin
    for I := 0 to SepiUnit.UsedUnitCount-1 do
    begin
      if AnsiSameText(UnitName, SepiUnit.UsedUnits[I].Name) then
      begin
        Redeclared := True;
        Break;
      end;
    end;
  end;

  if Redeclared then
    Child.MakeError(SRedeclaredIdentifier)
  else
    SepiUnit.MoreUses([UnitName]);

  inherited;
end;

{---------------------------}
{ TQualifiedIdentNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
function TQualifiedIdentNode.GetAsText: string;
var
  I: Integer;
begin
  Result := Children[0].AsText;
  for I := 1 to ChildCount-1 do
    Result := Result + '.' + Children[I].AsText;
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
    if AnsiSameText(ValueType.GetFullName, 'System.TGUID') then
      SetSymbolClass(ntGUIDInitializationExpression)
    else
      SetSymbolClass(ntRecordInitializationExpression);
  end else
    SetSymbolClass(ntOtherInitializationExpression);
end;

{----------------------------------}
{ TInitializationExprSubNode class }
{----------------------------------}

{*
  [@inheritDoc]
*}
procedure TInitializationExprSubNode.BeginParsing;
begin
  inherited;

  if Parent is TInitializationExprSubNode then
  begin
    with TInitializationExprSubNode(Parent) do
    begin
      Self.FValuePtr := ValuePtr;
      Self.FValueType := ValueType;
    end;
  end else
  begin
    with Parent as TInitializationExpressionNode do
    begin
      Self.FValuePtr := ValuePtr;
      Self.FValueType := ValueType;
    end;
  end;
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
  InitChild: TInitializationExpressionNode;
begin
  inherited;

  InitChild := Child as TInitializationExpressionNode;

  InitChild.ValueType := ElementType;

  if (ValuePtr = nil) or (ChildCount > ArrayType.ArrayLength) then
    InitChild.ValuePtr := nil
  else
    InitChild.ValuePtr :=
      Pointer(Cardinal(ValuePtr) + (ChildCount-1) * ElementType.Size);
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
  ValueChild: TInitializationExpressionNode;
  FieldName: string;
  Field: TSepiField;
begin
  inherited;

  if not (Child is TInitializationExpressionNode) then
    Exit;

  ValueChild := TInitializationExpressionNode(Child);
  FieldName := Children[ChildCount-2].AsText;
  Field := RecordType.GetMeta(FieldName) as TSepiField;

  if not CheckIdentFound(Field, FieldName, Children[ChildCount-2]) then
    ValueChild.ValueType := SepiRoot.FindType(TypeInfo(Integer))
  else
  begin
    ValueChild.ValuePtr := Pointer(Cardinal(ValuePtr) + Field.Offset);
    ValueChild.ValueType := Field.FieldType;
  end;
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
    TRecordInitializationNode(Child).RecordType := RecordType;
end;

{*
  [@inheritDoc]
*}
procedure TGUIDInitializationNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
begin
  if Child.SymbolClass = lexStringCst then
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

{--------------------------------}
{ TOtherInitializationNode class }
{--------------------------------}

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

{-----------------}
{ TExprNode class }
{-----------------}

{*
  [@inheritDoc]
*}
procedure TExprNode.EndParsing;
begin
  if Expression = nil then
  begin
    if MethodCompiler = nil then
    begin
      Expression := TSepiTrueConstValue.MakeIntegerLiteral(
        UnitCompiler, 0) as ISepiExpression;
    end else
    begin
      Expression := TSepiTrueConstValue.MakeIntegerLiteral(
        MethodCompiler, 0) as ISepiExpression;
    end;
  end;

  if (Expression.SourcePos.Line = 0) and (Expression.SourcePos.Col = 0) then
    Expression.SourcePos := SourcePos;

  inherited;
end;

{*
  EndParsing l'expression comme valeur
  @param ValueType         Type de valeur attendu (défaut = nil, peu importe)
  @param AllowConvertion   Autoriser les conversions pour avoir le bon type
  @return Valeur compilée (ou nil en cas d'erreur supposée récupérable)
*}
function TExprNode.AsValue(ValueType: TSepiType = nil;
  AllowConvertion: Boolean = True): ISepiValue;
var
  ReadableValue: ISepiReadableValue;
begin
  if not Supports(Expression, ISepiValue, Result) then
  begin
    MakeError(SValueRequired);
    Result := nil;
  end else if (ValueType <> nil) and (Result.ValueType <> ValueType) then
  begin
    if Supports(Result, ISepiNilValue) then
    begin
      if (ValueType is TSepiPointerType) or (ValueType is TSepiClass) or
        (ValueType is TSepiMetaClass) or (ValueType is TSepiInterface) or
        (ValueType is TSepiStringType) or (ValueType is TSepiDynArrayType) or
        (ValueType is TSepiMethodRefType) then
      begin
        Result := TSepiCastOperator.CastValue(ValueType, Result);
        Exit;
      end;
    end;

    if AllowConvertion and
      Supports(Result, ISepiReadableValue, ReadableValue) and
      TSepiConvertOperation.ConvertionExists(ValueType, Result.ValueType) then
    begin
      Result := TSepiConvertOperation.ConvertValue(ValueType, ReadableValue);
    end else
    begin
      MakeError(Format(STypeMismatch, [ValueType.Name, Result.ValueType.Name]));
      Result := nil;
    end;
  end else if (ValueType = nil) and Supports(Result, ISepiNilValue) then
  begin
    Result := TSepiCastOperator.CastValue(
      SepiRoot.FindType('System.Pointer'), Result);
  end;
end;

{*
  EndParsing l'expression comme type
  @return Type compilé, ou type Integer en cas d'erreur
*}
function TExprNode.AsType: TSepiType;
var
  TypeExpression: ISepiTypeExpression;
begin
  if Supports(Expression, ISepiTypeExpression, TypeExpression) then
    Result := TypeExpression.ExprType
  else
  begin
    MakeError(STypeIdentifierRequired);
    Result := SepiRoot.FindType(TypeInfo(Integer));
  end;
end;

{-----------------------}
{ TExpressionNode class }
{-----------------------}

{*
  EndParsing un sous-arbre de l'expression
  @param Lower    Index bas des symboles du sous-arbre
  @param Higher   Index haut des symboles du sous-arbre
  @return Expression représentant le sous-arbre
*}
function TExpressionNode.CompileTree(Lower, Higher: Integer): ISepiExpression;
var
  OpIndex, Priority, I: Integer;
  LeftExpression, RightExpression: ISepiExpression;
begin
  if Lower = Higher then
  begin
    Result := (Children[Lower] as TSingleExprNode).Expression;
    Exit;
  end;

  OpIndex := -1;
  Priority := MaxInt;
  I := Lower+1;

  while I < Higher do
  begin
    if (Children[I] as TBinaryOpNode).Priority <= Priority then
    begin
      OpIndex := I;
      Priority := TBinaryOpNode(Children[I]).Priority;
    end;

    Inc(I, 2);
  end;

  LeftExpression := CompileTree(Lower, OpIndex-1);
  RightExpression := CompileTree(OpIndex+1, Higher);

  Result := TBinaryOpNode(Children[OpIndex]).MakeExpression(
    LeftExpression, RightExpression);
end;

{*
  [@inheritDoc]
*}
procedure TExpressionNode.EndParsing;
begin
  Expression := CompileTree(0, ChildCount-1);

  inherited;
end;

{----------------------------}
{ TConstExpressionNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TConstExpressionNode.EndParsing;
var
  Value: ISepiReadableValue;
begin
  Expression := (Children[0] as TExpressionNode).Expression;

  if ((not AcceptType) or (not Supports(Expression, ISepiTypeExpression))) and
    ((not Supports(Expression, ISepiReadableValue, Value)) or
    (not Value.IsConstant)) and
    (not Supports(Expression, ISepiNilValue)) then
  begin
    MakeError(SConstExpressionRequired);
    Value := TSepiTrueConstValue.MakeIntegerLiteral(UnitCompiler, 0);
    Expression := Value as ISepiExpression;
  end;

  inherited;
end;

{*
  EndParsing une valeur constante
  @param Value       En sortie : valeur compilée
  @param ValueType   Type de la valeur
  @return True si la compilation est réussie, False en cas d'erreur
*}
function TConstExpressionNode.CompileConst(var Value;
  ValueType: TSepiType): Boolean;
var
  ReadableValue: ISepiReadableValue;
begin
  ReadableValue := AsValue(ValueType) as ISepiReadableValue;

  if ReadableValue = nil then
    Result := False
  else
  begin
    Assert(ReadableValue.IsConstant);

    ValueType.CopyData(ReadableValue.ConstValuePtr^, Value);
    Result := True;
  end;
end;

{*
  EndParsing une valeur constante
  @param Value           En sortie : valeur compilée
  @param ValueTypeInfo   RTTI du type de la valeur
  @return True si la compilation est réussie, False en cas d'erreur
*}
function TConstExpressionNode.CompileConst(var Value;
  ValueTypeInfo: PTypeInfo): Boolean;
begin
  Result := CompileConst(Value, SepiRoot.FindType(ValueTypeInfo));
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
  Construit une expression sur un opérateur unaire
  @param Operand   Opérande
  @return Expression construite
*}
function TUnaryOpNode.MakeExpression(
  const Operand: ISepiExpression): ISepiExpression;
var
  OpValue, Value: ISepiReadableValue;
  AddrValue: ISepiAddressableValue;
  Operation: TSepiOperation;
begin
  Result := nil;

  if Children[0].SymbolClass = lexAt then
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
      lexMinus: Operation := opNegate;
      lexNot:   Operation := opNot;
    else
      Result := Operand;
      Exit;
    end;

    Value := TSepiUnaryOperation.MakeOperation(Operation, OpValue);
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

    if (PointTo.Kind in [tkInteger, tkChar]) and
      (PointTo.TypeData.OrdType = otUByte) then
    begin
      Result := PointerValue.ValueType;

      PointerValue := TSepiCastOperator.CastValue(
        SepiRoot.FindType(TypeInfo(Integer)),
        PointerValue) as ISepiReadableValue;

      if IsLeft then
        LeftValue := PointerValue
      else
        RightValue := PointerValue;
    end;
  end;
end;

{*
  Priorité de l'opérateur binaire
  @return Priorité de l'opérateur binaire
*}
function TBinaryOpNode.GetPriority: Integer;
begin
  case Children[0].SymbolClass of
    lexOr, lexAnd, lexXor: Result := 1;
    lexPlus, lexMinus: Result := 2;
  else
    Result := 3;
  end;
end;

{*
  Construit une expression sur un opérateur unaire
  @param Operand   Opérande
  @return Expression construite
*}
function TBinaryOpNode.MakeExpression(
  const LeftExpression, RightExpression: ISepiExpression): ISepiExpression;
var
  Operation: TSepiOperation;
  LeftValue, RightValue, Value: ISepiReadableValue;
  RecastTo: TSepiType;
begin
  case Children[0].SymbolClass of
    lexPlus:   Operation := opAdd;
    lexMinus:  Operation := opSubtract;
    lexTimes:  Operation := opMultiply;
    lexDivide: Operation := opDivide;
    lexDiv:    Operation := opIntDivide;
    lexMod:    Operation := opModulus;
    lexShl:    Operation := opShiftLeft;
    lexShr:    Operation := opShiftRight;
    lexOr:     Operation := opOr;
    lexAnd:    Operation := opAnd;
    lexXor:    Operation := opXor;
  else
    Assert(False);
    Operation := 0;
  end;

  // Check operands

  if not Supports(LeftExpression, ISepiReadableValue, LeftValue) then
  begin
    LeftExpression.MakeError(SReadableValueRequired);
    LeftValue := nil;
  end;

  if not Supports(RightExpression, ISepiReadableValue, RightValue) then
  begin
    RightExpression.MakeError(SReadableValueRequired);
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
      [tkFloat, tkVariant] = []) then
    begin
      if LeftValue.IsConstant then
        LeftValue := TSepiConvertOperation.ConvertValue(
          SepiRoot.FindType(TypeInfo(Extended)), LeftValue);
      if RightValue.IsConstant then
        RightValue := TSepiConvertOperation.ConvertValue(
          SepiRoot.FindType(TypeInfo(Extended)), RightValue);
    end;

    // Make operation
    Value := TSepiBinaryOperation.MakeOperation(Operation,
      LeftValue, RightValue);
  end else
  begin
    // Error cases
    if LeftValue <> nil then
      Value := LeftValue
    else if RightValue <> nil then
      Value := RightValue
    else
      Value := TSepiTrueConstValue.MakeIntegerLiteral(UnitCompiler, 0);
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
procedure TSingleExprNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TNextExprNode then
    TNextExprNode(Child).Base := Expression;
end;

{*
  [@inheritDoc]
*}
procedure TSingleExprNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if not (Children[0] is TUnaryOpNode) then
    Expression := (Child as TExprNode).Expression;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSingleExprNode.EndParsing;
var
  Callable: ISepiCallable;
begin
  if Children[0] is TUnaryOpNode then
  begin
    Expression := (Children[1] as TExprNode).Expression;
    Expression := TUnaryOpNode(Children[0]).MakeExpression(Expression);
  end else
  begin
    if Supports(Expression, ISepiCallable, Callable) and
      (not Callable.ParamsCompleted) then
      Callable.CompleteParams;
  end;

  inherited;
end;

{------------------------------}
{ TParenthesizedExprNode class }
{------------------------------}

{*
  [@inheritDoc]
*}
procedure TParenthesizedExprNode.EndParsing;
begin
  Expression := (Children[0] as TExprNode).Expression;

  inherited;
end;

{------------------------}
{ TSingleValueNode class }
{------------------------}

{*
  Construit un littéral entier
  @param Value   Valeur du littéral
  @return Valeur pouvant être lue représentant le littéral
*}
function TSingleValueNode.MakeIntegerLiteral(Value: Int64): ISepiReadableValue;
var
  Compiler: TSepiMethodCompiler;
begin
  Compiler := MethodCompiler;

  if Compiler <> nil then
    Result := TSepiTrueConstValue.MakeIntegerLiteral(Compiler, Value)
  else
    Result := TSepiTrueConstValue.MakeIntegerLiteral(UnitCompiler, Value);

  (Result as ISepiExpression).SourcePos := SourcePos;
end;

{*
  Construit un littéral flottant
  @param Value   Valeur du littéral
  @return Valeur pouvant être lue représentant le littéral
*}
function TSingleValueNode.MakeExtendedLiteral(
  Value: Extended): ISepiReadableValue;
var
  Compiler: TSepiMethodCompiler;
begin
  Compiler := MethodCompiler;

  if Compiler <> nil then
    Result := TSepiTrueConstValue.MakeExtendedLiteral(Compiler, Value)
  else
    Result := TSepiTrueConstValue.MakeExtendedLiteral(UnitCompiler, Value);

  (Result as ISepiExpression).SourcePos := SourcePos;
end;

{*
  Construit un littéral caractère ANSI
  @param Value   Valeur du littéral
  @return Valeur pouvant être lue représentant le littéral
*}
function TSingleValueNode.MakeCharLiteral(Value: AnsiChar): ISepiReadableValue;
var
  Compiler: TSepiMethodCompiler;
begin
  Compiler := MethodCompiler;

  if Compiler <> nil then
    Result := TSepiTrueConstValue.MakeCharLiteral(Compiler, Value)
  else
    Result := TSepiTrueConstValue.MakeCharLiteral(UnitCompiler, Value);

  (Result as ISepiExpression).SourcePos := SourcePos;
end;

{*
  Construit un littéral caractère Unicode
  @param Value   Valeur du littéral
  @return Valeur pouvant être lue représentant le littéral
*}
function TSingleValueNode.MakeCharLiteral(Value: WideChar): ISepiReadableValue;
var
  Compiler: TSepiMethodCompiler;
begin
  Compiler := MethodCompiler;

  if Compiler <> nil then
    Result := TSepiTrueConstValue.MakeCharLiteral(Compiler, Value)
  else
    Result := TSepiTrueConstValue.MakeCharLiteral(UnitCompiler, Value);

  (Result as ISepiExpression).SourcePos := SourcePos;
end;

{*
  Construit un littéral chaîne longue ANSI
  @param Value   Valeur du littéral
  @return Valeur pouvant être lue représentant le littéral
*}
function TSingleValueNode.MakeStringLiteral(
  const Value: AnsiString): ISepiReadableValue;
var
  Compiler: TSepiMethodCompiler;
begin
  Compiler := MethodCompiler;

  if Compiler <> nil then
    Result := TSepiTrueConstValue.MakeStringLiteral(Compiler, Value)
  else
    Result := TSepiTrueConstValue.MakeStringLiteral(UnitCompiler, Value);

  (Result as ISepiExpression).SourcePos := SourcePos;
end;

{*
  Construit un littéral chaîne longue Unicode
  @param Value   Valeur du littéral
  @return Valeur pouvant être lue représentant le littéral
*}
function TSingleValueNode.MakeStringLiteral(
  const Value: WideString): ISepiReadableValue;
var
  Compiler: TSepiMethodCompiler;
begin
  Compiler := MethodCompiler;

  if Compiler <> nil then
    Result := TSepiTrueConstValue.MakeStringLiteral(Compiler, Value)
  else
    Result := TSepiTrueConstValue.MakeStringLiteral(UnitCompiler, Value);

  (Result as ISepiExpression).SourcePos := SourcePos;
end;

{*
  Gère un appel inherited
  @param Child   Enfant qui porte le nom à appeler
*}
procedure TSingleValueNode.HandleInherited(Child: TSepiParseTreeNode);
const
  ForceStaticCall = True;
  FreeParamIsAlwaysFalse = False;
var
  Name: string;
  Method: TSepiMethod;
  SepiClass, ParentClass: TSepiClass;
  Member: TSepiMeta;
  SelfParam: ISepiValue;
  Callable: ISepiCallable;
begin
  Name := Child.AsText;

  // Fetch method
  if not (SepiContext is TSepiMethod) then
  begin
    Child.MakeError(SInheritNeedClassOrObjectMethod);
    Exit;
  end;
  Method := TSepiMethod(SepiContext);

  // Fetch class
  if not (Method.Owner is TSepiClass) then
  begin
    Child.MakeError(SInheritNeedClassOrObjectMethod);
    Exit;
  end;
  SepiClass := TSepiClass(Method.Owner);

  // Fetch parent class
  ParentClass := SepiClass.Parent;
  Assert(ParentClass <> nil); // Sepi never compiles TObject implementation

  // Look for the member in the parent class
  Member := ParentClass.LookForMember(Name, Method.OwningUnit, SepiClass);
  if not CheckIdentFound(Member, Name, Child) then
    Exit;

  // Fetch Self param
  SelfParam := TSepiLocalVarValue.MakeValue(MethodCompiler,
    MethodCompiler.Locals.GetVarByName(HiddenParamNames[hpSelf]));

  // Make sure the member is a method - or an overloaded method
  if Member is TSepiMethod then
  begin
    Callable := TSepiMethodCall.Create(TSepiMethod(Member),
      SelfParam as ISepiReadableValue,
      ForceStaticCall, FreeParamIsAlwaysFalse);
  end else if Member is TSepiOverloadedMethod then
  begin
    Callable := TSepiMethodCall.Create(TSepiOverloadedMethod(Member),
      SelfParam as ISepiReadableValue,
      ForceStaticCall, FreeParamIsAlwaysFalse);
  end else
  begin
    Child.MakeError(SMethodRequired);
    Exit;
  end;

  Expression := MakeExpression;
  Callable.AttachToExpression(Expression);
end;

{*
  [@inheritDoc]
*}
procedure TSingleValueNode.EndParsing;
var
  Child: TSepiParseTreeNode;
  Text: string;
  IntValue: Int64;
  FloatValue: Extended;
  StrValue: string;
  Value: ISepiValue;
begin
  Child := Children[0];
  Text := Child.AsText;

  case Child.SymbolClass of
    // Littéral entier
    lexInteger:
    begin
      if not TryStrToInt64(Text, IntValue) then
      begin
        Child.MakeError(Format(SInvalidInteger, [Text]));
        IntValue := 0;
      end;

      Value := MakeIntegerLiteral(StrToInt(Text));
      Expression := Value as ISepiExpression;
    end;

    // Littéral flottant
    lexFloat:
    begin
      if not TryStrToFloat(Text, FloatValue) then
      begin
        Child.MakeError(Format(SInvalidFloat, [Text]));
        FloatValue := 0.0;
      end;

      Value := MakeExtendedLiteral(FloatValue);
      Expression := Value as ISepiExpression;
    end;

    // Littéral chaîne
    lexStringCst:
    begin
      StrValue := StrRepresToStr(Text);

      if Length(StrValue) = 1 then
        Value := MakeCharLiteral(StrValue[1])
      else
        Value := MakeStringLiteral(StrValue);

      Expression := Value as ISepiExpression;
    end;

    // Identificateur
    ntIdentifier:
    begin
      Expression := ResolveIdent(Text);
      if not CheckIdentFound(Expression, Text, Child) then
        Expression := MakeIntegerLiteral(0) as ISepiExpression;
      Expression.SourcePos := Child.SourcePos;
    end;

    // Inherited call
    lexInherited:
    begin
      HandleInherited(Children[1]);
    end;

    // nil
    lexNil:
    begin
      Expression := TSepiExpression.Create(UnitCompiler);
      ISepiExpressionPart(TSepiNilValue.Create).AttachToExpression(Expression);
      Expression.SourcePos := Child.SourcePos;
    end;
  else
    // Récupérer depuis l'enfant
    Expression := (Child as TExprNode).Expression;
  end;

  inherited;
end;

{---------------------}
{ TSetValueNode class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TSetValueNode.EndParsing;
begin
  MakeError('Can''t compile set values yet!');
  Expression := TSepiTrueConstValue.MakeIntegerLiteral(
    UnitCompiler, 0) as ISepiExpression;

  inherited;
end;

{---------------------}
{ TNextExprNode class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TNextExprNode.EndParsing;
begin
  if Expression = nil then
    Expression := TSepiTrueConstValue.MakeIntegerLiteral(
      UnitCompiler, 0) as ISepiExpression;

  inherited;
end;

{-----------------------}
{ TParametersNode class }
{-----------------------}

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

  if not Supports((Children[0] as TExpressionNode).Expression,
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
    Expression := TSepiConvertOperation.ConvertValue(
      DestType, ReadableSource) as ISepiExpression;
  end else
  begin
    if (SourceType <> nil) and (SourceType.Size <> DestType.Size) and
      ((DestType is TSepiPointerType) or (DestType is TSepiClass)) and
      (SourceType is TSepiIntegerType) and (ReadableSource <> nil) then
    begin
      Source := TSepiConvertOperation.ConvertValue(
        SepiRoot.FindType(TypeInfo(Integer)), ReadableSource);
    end;

    Expression := TSepiCastOperator.CastValue(
      DestType, Source) as ISepiExpression;
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

  PseudoRoutine.Operand := (Children[0] as TExpressionNode).AsValue;
  PseudoRoutine.Complete;

  Expression := Base;
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

  Expression := Base;
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
    Expression := (Children[I] as TExpressionNode).Expression;
    if not Supports(Expression, ISepiValue, Value) then
      Expression.MakeError(SValueRequired)
    else
      Callable.AddParam(Value);
  end;

  Callable.CompleteParams;
  Callable.AttachToExpression(Base);
  Self.Expression := Base;
end;

{*
  [@inheritDoc]
*}
procedure TParametersNode.EndParsing;
var
  CastPseudoRoutine: ISepiCastPseudoRoutine;
  TypeExpression: ISepiTypeExpression;
  TypeOperation: ISepiTypeOperationPseudoRoutine;
  Callable: ISepiCallable;
begin
  if Supports(Base, ISepiCastPseudoRoutine, CastPseudoRoutine) then
    CompileCast(CastPseudoRoutine)
  else if Supports(Base, ISepiTypeExpression, TypeExpression) then
    CompileCastOrConvert(TypeExpression.ExprType)
  else if Supports(Base, ISepiTypeOperationPseudoRoutine, TypeOperation) and
    (ChildCount = 1) and
    Supports((Children[0] as TExpressionNode).Expression,
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

{-------------------------}
{ TArrayIndicesNode class }
{-------------------------}

{*
  Compile les indices d'une propriété de type tableau
  @param Prop   Propriété
*}
procedure TArrayIndicesNode.CompileProperty(const Prop: ISepiProperty);
var
  Count, I: Integer;
  Value: ISepiValue;
begin
  if ChildCount < Prop.ParamCount then
    Count := ChildCount
  else
    Count := Prop.ParamCount;

  for I := 0 to Count-1 do
  begin
    if Supports((Children[I] as TExpressionNode).Expression,
      ISepiValue, Value) then
      Prop.Params[I] := Value
    else
      Children[I].MakeError(SValueRequired);
  end;

  if ChildCount < Prop.ParamCount then
    MakeError(SNotEnoughActualParameters)
  else if ChildCount > Prop.ParamCount then
    MakeError(STooManyActualParameters);

  Expression := Base;
end;

{*
  Compile l'accès à un élément de tableau
  @param BaseValue   Valeur tableau de base
*}
procedure TArrayIndicesNode.CompileArrayItem(const BaseValue: ISepiValue);
var
  CurrentValue: ISepiValue;
  IndexValue: ISepiReadableValue;
  I: Integer;
  Child: TExpressionNode;
  BaseType: TSepiType;
begin
  CurrentValue := BaseValue;

  for I := 0 to ChildCount-1 do
  begin
    Child := Children[0] as TExpressionNode;
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

  Expression := CurrentValue as ISepiExpression;
end;

{*
  [@inheritDoc]
*}
procedure TArrayIndicesNode.EndParsing;
var
  Prop: ISepiProperty;
  BaseValue: ISepiValue;
begin
  if Supports(Base, ISepiProperty, Prop) and (not Prop.ParamsCompleted) then
    CompileProperty(Prop)
  else if Supports(Base, ISepiValue, BaseValue) and
    (BaseValue.ValueType is TSepiArrayType) then
    CompileArrayItem(BaseValue)
  else
    Base.MakeError(SArrayOrArrayPropRequired);

  inherited;
end;

{---------------------------}
{ TFieldSelectionNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TFieldSelectionNode.EndParsing;
begin
  Expression := FieldSelection(SepiContext, Base, Children[0].AsText);
  if Expression = nil then
    Children[0].MakeError(SIdentifierNotFound);

  inherited;
end;

{------------------------}
{ TDereferenceNode class }
{------------------------}

{*
  [@inheritDoc]
*}
procedure TDereferenceNode.EndParsing;
var
  Value: ISepiReadableValue;
begin
  if Supports(Base, ISepiReadableValue, Value) and
    (Value.ValueType is TSepiPointerType) then
  begin
    Expression := TSepiDereferenceValue.MakeDereference(
      Value) as ISepiExpression;
  end else
  begin
    MakeError(SPointerTypeRequired);
  end;

  inherited;
end;

{---------------------}
{ TTypeDescNode class }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TTypeDescNode.BeginParsing;
begin
  inherited;

  FIsAnonymous := TypeName = '';
end;

{*
  [@inheritDoc]
*}
procedure TTypeDescNode.ChildBeginParsing(Child: TSepiParseTreeNode);
var
  Descriptor: TTypeDescriptorNode;
begin
  inherited;

  if Child is TTypeDescriptorNode then
  begin
    Descriptor := TTypeDescriptorNode(Child);
    Descriptor.TypeName := TypeName;
    Descriptor.IsPacked := IsPacked;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TTypeDescNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child.SymbolClass = lexPacked then
    FIsPacked := True
  else
    FSepiType := (Child as TTypeDescriptorNode).SepiType;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TTypeDescNode.EndParsing;
begin
  if (Children[ChildCount-1] as TTypeDescriptorNode).SepiType = nil then
    FSepiType := UnitCompiler.MakeErroneousTypeAlias(TypeName)
  else
    FSepiType := TTypeDescriptorNode(Children[ChildCount-1]).SepiType;

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

  if Child is TTypeDescNode then
    TTypeDescNode(Child).TypeName := Children[0].AsText;
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
    InitChild.ValuePtr := ConstVar.Value;
    InitChild.ValueType := ConstVar.VarType;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TConstantDeclNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  ReadableValue: ISepiReadableValue;
begin
  if Child.SymbolClass = ntIdentifier then
    FName := Child.AsText
  else if Child is TTypeDescNode then
  begin
    FConstType := TTypeDescNode(Child).SepiType;
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
    InitChild.ValuePtr := Variable.Value;
    InitChild.ValueType := Variable.VarType;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TVariableDeclNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  I: Integer;
begin
  if Child.SymbolClass = ntIdentifier then
  begin
    if FNames = nil then
      FNames := TStringList.Create;

    Names.Add(Child.AsText);
  end else if Child is TTypeDescNode then
  begin
    FVarType := TTypeDescNode(Child).SepiType;

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
{ TRoutineDeclNode class }
{------------------------}

{*
  [@inheritDoc]
*}
destructor TRoutineDeclNode.Destroy;
begin
  FSignature.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TRoutineDeclNode.BeginParsing;
begin
  inherited;

  FSignature := TSepiSignature.CreateConstructing(SepiUnit);
end;

{*
  [@inheritDoc]
*}
procedure TRoutineDeclNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSignatureNode then
    TSignatureNode(Child).Signature := Signature;
end;

{*
  [@inheritDoc]
*}
procedure TRoutineDeclNode.ChildEndParsing(Child: TSepiParseTreeNode);
const
  IgnoredModifiers: array[0..3] of string = (
    'deprecated', 'inline', 'assembler', 'platform'
  );
var
  Str: string;
  Temp: Integer;
begin
  case Child.SymbolClass of
    // Type de routine
    ntRoutineKind:
    begin
      if Child.Children[0].SymbolClass = lexProcedure then
        Signature.Kind := mkUnitProcedure
      else
        Signature.Kind := mkUnitFunction;
    end;

    // Nom de la routine
    ntIdentifier:
    begin
      FName := Child.AsText;
    end;

    // Modificateur
    ntRoutineModifier:
    begin
      Str := Child.AsText;
      Temp := AnsiIndexText(Str, CallingConventionNames);
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

{*
  [@inheritDoc]
*}
procedure TRoutineDeclNode.EndParsing;
begin
  Signature.Complete;

  if IsOverloaded then
    TSepiMethod.CreateOverloaded(SepiContext, Name, nil, Signature)
  else
    TSepiMethod.Create(SepiContext, Name, nil, Signature);

  inherited;
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
  OldType := TSepiType(LookFor(Children[0], TSepiType,
    STypeIdentifierRequired));

  if OldType = nil then
  begin
    SepiType := UnitCompiler.MakeErroneousTypeAlias(TypeName);
  end else
  begin
    try
      SepiType := TSepiTypeClass(OldType.ClassType).Clone(
        SepiContext, TypeName, OldType);
    except
      on Error: ESepiError do
      begin
        MakeError(Error.Message);
        SepiType := UnitCompiler.MakeErroneousTypeAlias(TypeName);
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

  with Child as TTypeDescriptorNode do
  begin
    TypeName := Self.TypeName;
    IsPacked := Self.IsPacked;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TRangeOrEnumTypeNode.EndParsing;
begin
  SepiType := (Children[0] as TTypeDescriptorNode).SepiType;

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
      Integer(LowerValue), Integer(HigherValue),
      TSepiIntegerType(BaseType).Signed);
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
      SepiType := TypeExpression.ExprType;

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
    LowerValue :=
      (Children[0] as TConstExpressionNode).AsValue as ISepiReadableValue;
    HigherValue :=
      (Children[1] as TConstExpressionNode).AsValue as ISepiReadableValue;

    if (LowerValue <> nil) and (HigherValue <> nil) then
    begin
      // Try and convert value types
      TryAndConvertValues(LowerValue, HigherValue);

      // Some checks
      if LowerValue.ValueType <> HigherValue.ValueType then
      begin
        MakeError(Format(STypeMismatch,
          [LowerValue.ValueType, HigherValue.ValueType]));
      end else
      begin
        // OK, we're good
        SepiType := MakeType(TypeName, LowerValue.ValueType,
          LowerValue.ConstValuePtr^, HigherValue.ConstValuePtr^);
      end;
    end;
  end;

  if SepiType = nil then
    SepiType := UnitCompiler.MakeErroneousTypeAlias(TypeName);

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

  SepiType := TSepiEnumType.Create(SepiContext, TypeName, Values,
    RootNode.MinemumSize);

  inherited;
end;

{--------------------}
{ TSetTypeNode class }
{--------------------}

{*
  [@inheritDoc]
*}
procedure TSetTypeNode.EndParsing;
var
  CompType: TSepiType;
  OrdCompType: TSepiOrdType absolute CompType;
begin
  CompType := (Children[0] as TTypeDescNode).SepiType;

  if (not (CompType is TSepiOrdType)) or
    (OrdCompType.MaxValue - OrdCompType.MinValue >= 256) then
  begin
    Children[0].MakeError(SWrongSetCompType);
    SepiType := TSepiSetType.Create(SepiContext, TypeName, TypeInfo(Byte));
  end else
    SepiType := TSepiSetType.Create(SepiContext, TypeName, OrdCompType);

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
      MaxLength, TypeInfo(Integer)) then
      MaxLength := 255;

    SepiType := TSepiShortStringType.Create(SepiContext, TypeName, MaxLength);
  end else
  begin
    SepiType := SepiRoot.FindType(TypeInfo(string));

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
  PointedType := RootNode.SepiUnit.LookFor(PointedName) as TSepiType;

  if PointedType <> nil then
    SepiType := TSepiPointerType.Create(SepiContext, TypeName, PointedType)
  else
    SepiType := TSepiPointerType.Create(SepiContext, TypeName, PointedName);

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
  LowerValue := (RangeNode.Children[0] as
    TConstExpressionNode).AsValue as ISepiReadableValue;
  HigherValue := (RangeNode.Children[1] as
    TConstExpressionNode).AsValue as ISepiReadableValue;

  Result := nil;

  if (LowerValue <> nil) and (HigherValue <> nil) then
  begin
    // Try and convert value types
    TryAndConvertValues(LowerValue, HigherValue);

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
        TSepiOrdType(LowerValue.ValueType), ValueAsInt64(LowerValue),
        ValueAsInt64(HigherValue), ElementType);
    end;
  end;

  if Result = nil then
  begin
    Result := TSepiStaticArrayType.Create(SepiContext, TypeName,
      TSepiOrdType(SepiRoot.FindType(TypeInfo(Integer))), 0, 0, ElementType);
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
      TSepiOrdType(SepiRoot.FindType(TypeInfo(Integer))), 0, 0, ElementType);
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
  ElementType := (Children[1] as TTypeDescNode).SepiType;

  with Children[0] do
  begin
    if ChildCount = 0 then
    begin
      // Dynamic array
      SepiType := TSepiDynArrayType.Create(SepiContext, TypeName, ElementType);
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

      SepiType := ElementType;
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
procedure TRecordTypeNode.BeginParsing;
begin
  inherited;

  FRecordType := TSepiRecordType.Create(SepiContext, TypeName, IsPacked);
end;

{*
  [@inheritDoc]
*}
procedure TRecordTypeNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Children[0] as TRecordContentsNode).RecordType := RecordType;
end;

{*
  [@inheritDoc]
*}
procedure TRecordTypeNode.EndParsing;
begin
  RecordType.Complete;
  SepiType := RecordType;

  inherited;
end;

{---------------------------}
{ TRecordContentsNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TRecordContentsNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TFieldNode then
  begin
    with TFieldNode(Child) do
    begin
      Owner := RecordType;

      if Self.ChildCount = 1 then
        AfterField := Self.AfterField
      else
        AfterField := '~';
    end;
  end else if Child is TRecordContentsNode then
  begin
    TRecordContentsNode(Child).RecordType := RecordType;
    TRecordContentsNode(Child).AfterField := AfterField;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TRecordContentsNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child is TFieldNode then
  begin
    if TFieldNode(Child).LastField <> nil then
      AfterField := TFieldNode(Child).LastField.Name;
  end;

  inherited;
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

  if Child.SymbolClass = lexOf then
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
    ParentClass := TSepiClass(LookFor(Child.Children[0],
      TSepiClass, SClassTypeRequired));

    // Create class
    CreateClass(ParentClass);

    // Implemented interfaces
    for I := 1 to Child.ChildCount-1 do
    begin
      ImplementedIntf := TSepiInterface(LookFor(Child.Children[I],
        TSepiInterface, SInterfaceTypeRequired));

      if ImplementedIntf <> nil then
        SepiClass.AddInterface(ImplementedIntf);
    end;
  end else if Child.SymbolClass = ntQualifiedIdent then
  begin
    // Make meta-class
    Assert(IsMetaClass);

    ReferencedClass := TSepiClass(LookFor(Child,
      TSepiClass, SClassTypeRequired));

    if ReferencedClass = nil then
      ReferencedClass := TSepiClass(SepiRoot.FindType(TypeInfo(TObject)));

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
    SepiType := SepiMetaClass
  else
    SepiType := SepiClass;

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
    Meta := LookFor(Identifier);
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

  if Child is TMemberListNode then
  begin
    CreateInterface;
    TMemberListNode(Child).Owner := SepiIntf;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TInterfaceTypeNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  GUIDStr: string;
  GUID: TGUID;
begin
  if Child.SymbolClass = ntInterfaceHeritage then
  begin
    if Child.ChildCount > 0 then
    begin
      FParentIntf := TSepiInterface(LookFor(Child,
        TSepiInterface, SInterfaceTypeRequired));
    end;
  end else if Child.SymbolClass = ntInterfaceGUID then
  begin
    if Child.ChildCount > 0 then
    begin
      try
        if (Child.Children[0] as TConstExpressionNode).CompileConst(
          GUIDStr, SepiRoot.FindType(TypeInfo(string))) then
        begin
          GUID := StringToGUID(GUIDStr);
        end;
      except
        on Error: EConvertError do
          Child.MakeError(Error.Message);
      end;
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

  SepiType := SepiIntf;

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
    Meta := LookFor(Identifier);
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

  if Child is TSignatureNode then
    TSignatureNode(Child).Signature := Signature;
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
        lexProcedure: Signature.Kind := mkUnitProcedure;
        lexFunction:  Signature.Kind := mkUnitFunction;
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

    Index := AnsiIndexText(ModifierNode.AsText, CallingConventionNames);

    if Index >= 0 then
    begin
      // Calling convention
      Signature.CallingConvention := TCallingConvention(Index);
    end else if ModifierNode.SymbolClass = ntEventIsOfObject then
    begin
      // of object - do not duplicate
      if Signature.Kind = mkUnitProcedure then
        Signature.Kind := mkProcedure
      else if Signature.Kind = mkUnitFunction then
        Signature.Kind := mkFunction
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
  SepiType := TSepiMethodRefType.Create(SepiContext, TypeName, Signature);

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
destructor TFieldNode.Destroy;
begin
  FNames.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TFieldNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  I: Integer;
begin
  if Child.SymbolClass = ntIdentifier then
  begin
    if FNames = nil then
      FNames := TStringList.Create;

    if (Owner.GetMeta(Child.AsText) <> nil) or
      (Names.IndexOf(Child.AsText) >= 0) then
    begin
      Child.MakeError(SRedeclaredIdentifier);
      Names.Add('');
    end else
      Names.Add(Child.AsText);
  end else if Child is TTypeDescNode then
  begin
    FFieldType := TTypeDescNode(Child).SepiType;

    if FieldType <> nil then
    begin
      if Owner is TSepiRecordType then
      begin
        if AfterField = '~' then
          FLastField := TSepiRecordType(Owner).AddField(
            Names[0], FieldType)
        else
          FLastField := TSepiRecordType(Owner).AddFieldAfter(
            Names[0], FieldType, AfterField);

        for I := 1 to Names.Count-1 do
          FLastField := TSepiRecordType(Owner).AddField(
            Names[I], FieldType, True);
      end else
      begin
        Assert(Owner is TSepiClass);

        for I := 0 to Names.Count-1 do
          FLastField := TSepiClass(Owner).AddField(Names[I], FieldType, I > 0);
      end;
    end;
  end;

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

  Intf := TSepiInterface(LookFor(IntfNode, TSepiInterface,
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

  if Child is TSignatureNode then
    TSignatureNode(Child).Signature := Signature;
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
  case Child.SymbolClass of
    // Type de routine
    ntRoutineKind:
    begin
      case Child.Children[0].SymbolClass of
        lexProcedure: Signature.Kind := mkUnitProcedure;
        lexFunction:  Signature.Kind := mkUnitFunction;
      end;
    end;

    // Type de méthode
    ntMethodKind:
    begin
      case Child.Children[0].SymbolClass of
        lexProcedure:   Signature.Kind := mkProcedure;
        lexFunction:    Signature.Kind := mkFunction;
        lexConstructor: Signature.Kind := mkConstructor;
        lexDestructor:  Signature.Kind := mkDestructor;
      else
        case Child.Children[1].SymbolClass of
          lexProcedure: Signature.Kind := mkClassProcedure;
          lexFunction:  Signature.Kind := mkClassFunction;
        end;
      end;
    end;

    // Nom de la routine
    ntIdentifier:
    begin
      FName := Child.AsText;
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
      Temp := AnsiIndexText(Str, LinkKindNames);

      if Temp >= 0 then
      begin
        // Link kind
        FLinkKind := TMethodLinkKind(Temp);

        // Store message ID
        if LinkKind = mlkMessage then
          (Child.Children[1] as TConstExpressionNode).CompileConst(
            FMsgID, TypeInfo(Integer));
      end else
      begin
        Temp := AnsiIndexText(Str, CallingConventionNames);
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
  FSignature.Kind := mkProperty;
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
      if not Method.Signature.Params[I].Equals(Signature.Params[I]) then
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

  if Child is TSignatureNode then
    TSignatureNode(Child).Signature := Signature;
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
  case Child.SymbolClass of
    // Nom de la propriété
    ntIdentifier:
    begin
      FName := Child.AsText;
    end;

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
        Signature.ReturnType := SepiRoot.FindType(TypeInfo(Integer));
      end else
      begin
        for I := 0 to Previous.Signature.ParamCount-1 do
          TSepiParam.Clone(Signature, Previous.Signature.Params[I]);
        Signature.ReturnType := Previous.Signature.ReturnType;

        if Previous.Index <> NoIndex then
        begin
          FIndex := TSepiTrueConstValue.MakeOrdinalValue(UnitCompiler,
            SepiRoot.FindType(TypeInfo(Integer)) as TSepiOrdType,
            Previous.Index);
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
        lexRead:      HandleAccess      (Child.Children[1], False);
        lexWrite:     HandleAccess      (Child.Children[1], True);
        lexIndex:     HandleIndex       (Child.Children[1]);
        lexDefault:   HandleDefaultValue(Child.Children[1]);
        lexNoDefault: HandleDefaultValue(nil);
        lexStored:    HandleStorage     (Child.Children[1]);
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
        else // TODO Should check for multiple default properties
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
    TMemberVisibility(AnsiIndexText(AsText, Visibilities));

  inherited;
end;

{----------------------}
{ TSignatureNode class }
{----------------------}

{*
  [@inheritDoc]
*}
procedure TSignatureNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TInSignatureNode).Signature := Signature;
end;

{------------------}
{ TParamNode class }
{------------------}

{*
  [@inheritDoc]
*}
procedure TParamNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TInitializationExpressionNode then
  begin
    TInitializationExpressionNode(Child).ValueType :=
      SepiRoot.FindType(TypeInfo(Integer));

    if OpenArray then
      Child.MakeError(SOpenArrayParamCantHaveDefaultValue)
    else if Length(Names) > 1 then
      Child.MakeError(SMultiNameParamCantHaveDefaultValue)
    else
    begin
      (Child as TInitializationExpressionNode).ValueType := ParamType;
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
          lexConst: FKind := pkConst;
          lexVar:   FKind := pkVar;
          lexOut:   FKind := pkOut;
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
        FType := TSepiType(LookFor(Child.Children[0], TSepiType,
          STypeIdentifierRequired));
        if FType = nil then
          FType := SepiRoot.FindType(TypeInfo(Integer));
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
  Kind: TMethodKind;
  ReturnType: TSepiType;
begin
  Kind := Signature.Kind;

  if (Kind = mkFunction) or (Kind = mkClassFunction) or
    (Kind = mkUnitFunction) or (Kind = mkProperty) then
  begin
    // Return type required
    if ChildCount > 0 then
    begin
      ReturnType := TSepiType(LookFor(Children[0], TSepiType,
        STypeIdentifierRequired));
    end else
    begin
      MakeError(SReturnTypeRequired);
      ReturnType := nil;
    end;

    if ReturnType = nil then
      Signature.ReturnType := SepiRoot.FindType(TypeInfo(Integer))
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
    TempMethod := LookFor(Name);

    // Update signature kind for a method implementation
    if (TempMethod <> nil) and (TempMethod.Owner is TSepiClass) then
    begin
      if Signature.Kind = mkUnitProcedure then
        Signature.Kind := mkProcedure
      else if Signature.Kind = mkUnitFunction then
        Signature.Kind := mkFunction;
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

  if Child is TSignatureNode then
    TSignatureNode(Child).Signature := Signature;
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
    // Type de routine
    ntMethodKind:
    begin
      if Child.Children[0].SymbolClass = lexProcedure then
        Signature.Kind := mkUnitProcedure
      else
        Signature.Kind := mkUnitFunction;
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
      Temp := AnsiIndexText(Str, CallingConventionNames);
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
  if SepiMethod <> nil then
    Result := SepiMethod
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
  if Child.SymbolClass = ntIdentifier then
  begin
    if FNames = nil then
      FNames := TStringList.Create;

    Names.Add(Child.AsText);
  end else if Child is TTypeDescNode then
  begin
    FVarType := TTypeDescNode(Child).SepiType;

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
  if Child is TExpressionNode then
  begin
    TestValue :=
      TExpressionNode(Child).AsValue(SepiRoot.FindType(TypeInfo(Boolean)));

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

  FIsRepeat := SymbolClass = ntWhileInstruction;
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
  if Child is TExpressionNode then
  begin
    TestValue :=
      TExpressionNode(Child).AsValue(SepiRoot.FindType(TypeInfo(Boolean)));

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
        ControlVar := MethodCompiler.Locals.AddTempVar(
          SepiRoot.FindType(TypeInfo(Integer)));
      end;

      Instruction.ControlVar := ControlVar;
    end;

    // Start and end values
    StartValueChild, EndValueChild:
    begin
      BoundValue := (Child as TExpressionNode).AsValue;

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
      Instruction.IsDownTo := (Child.SymbolClass = lexDownTo);
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
  if Supports((Children[0] as TExpressionNode).Expression,
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
  if not Supports((Children[0] as TExpressionNode).AsValue,
    ISepiWritableValue, DestValue) then
  begin
    Children[0].MakeError(SWritableValueRequired);
  end else if not Supports((Children[1] as TExpressionNode).AsValue,
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
  ExceptionValue := (Children[0] as TExpressionNode).AsValue(
    SepiRoot.FindType(TypeInfo(TObject)));

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

