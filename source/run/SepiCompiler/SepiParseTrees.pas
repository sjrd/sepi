{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  S�bastien Doeraene
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
  Arbres syntaxiques Sepi
  @author sjrd
  @version 1.0
*}
unit SepiParseTrees;

interface

{$ASSERTIONS ON}

{$D-,L-}

uses
  Windows, SysUtils, Classes, Contnrs, RTLConsts, SepiReflectionCore,
  SepiSystemUnit, SepiCompiler, SepiCompilerErrors, SepiCompilerConsts;

type
  TSepiNonTerminal = class;
  TSepiParseTreeRootNode = class;

  {*
    Classe de symbole
  *}
  TSepiSymbolClass = type Smallint;

  {*
    Noeud d'un arbre syntaxique Sepi
    TSepiParseTreeNode est la classe de base de tous les types de noeud d'un
    arbre syntaxique. Ses deux classes filles TSepiTerminal et TSepiNonTerminal
    impl�mentent les m�canismes propres aux terminaux et non-terminaux,
    respectivement. Vous ne devriez normalement pas �crire d'autre classe fille
    de TSepiParseTreeNode, mais plut�t de ces deux filles-l�.
    @author sjrd
    @version 1.0
  *}
  TSepiParseTreeNode = class
  private
    FParent: TSepiNonTerminal;       /// Noeud parent dans l'arbre syntaxique
    FClass: TSepiSymbolClass;        /// Classe de symbole
    FSourcePos: TSepiSourcePosition; /// Position dans le source

    FRootNode: TSepiParseTreeRootNode;  /// Noeud racine de l'arbre syntaxique
    FSyntacticParent: TSepiNonTerminal; /// Parent syntaxique

    procedure DoAncestorChanged;

    function GetIndexAsChild: Integer;

    function GetCompiler: TSepiCompilerBase;
    function GetUnitCompiler: TSepiUnitCompiler;
    function GetSepiRoot: TSepiRoot;
    function GetSepiUnit: TSepiUnit;
    function GetLanguageRules: TSepiLanguageRules;
    function GetSystemUnit: TSepiSystemUnit;
  protected
    procedure SetSymbolClass(Value: TSepiSymbolClass);
    procedure SetSyntacticParent(Value: TSepiNonTerminal);

    function MakeExpression: ISepiExpression;

    function GetChildCount: Integer; virtual;
    function GetChildren(Index: Integer): TSepiParseTreeNode; virtual;

    procedure AddToParent(Index: Integer); virtual;
    procedure RemoveFromParent; virtual;

    procedure AncestorChanged; virtual;

    function GetMethodCompiler: TSepiMethodCompiler; virtual;
    function GetSepiContext: TSepiComponent; virtual;

    {*
      Version texte du contenu du symbole grammatical
      @return Contenu textuel du symbole grammatical
    *}
    function GetAsText: string; virtual; abstract;
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); overload;
    constructor Create(AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); overload;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure DefaultHandler(var Message); override;

    procedure Move(NewParent: TSepiNonTerminal; Index: Integer = -1);
    function FindRightMost: TSepiParseTreeNode;

    function IsAncestor(Ancestor: TSepiParseTreeNode): Boolean; overload;
    function IsAncestor(AncestorClass: TClass): Boolean; overload;

    function ResolveIdent(const Identifier: string): ISepiExpression; virtual;

    function ResolveIdentOrError(Node: TSepiParseTreeNode): ISepiExpression;

    function LookFor(Node: TSepiParseTreeNode): TSepiComponent; overload;
    function LookFor(Node: TSepiParseTreeNode;
      RequiredClass: TSepiComponentClass): TSepiComponent; overload;

    function LookForSelfText: TSepiComponent; overload;
    function LookForSelfText(
      RequiredClass: TSepiComponentClass): TSepiComponent; overload;

    function LookForOrError(Node: TSepiParseTreeNode): TSepiComponent; overload;
    function LookForOrError(Node: TSepiParseTreeNode;
      RequiredClass: TSepiComponentClass;
      const BadClassErrorMsg: string): TSepiComponent; overload;

    function LookForSelfTextOrError: TSepiComponent; overload;
    function LookForSelfTextOrError(
      RequiredClass: TSepiComponentClass;
      const BadClassErrorMsg: string): TSepiComponent; overload;

    procedure MakeError(const ErrorMsg: string;
      Kind: TSepiErrorKind = ekError);

    procedure BeginParsing; virtual;
    procedure EndParsing; virtual;

    property Parent: TSepiNonTerminal read FParent;
    property SymbolClass: TSepiSymbolClass read FClass;
    property RootNode: TSepiParseTreeRootNode read FRootNode;

    property SyntacticParent: TSepiNonTerminal read FSyntacticParent;

    property SourcePos: TSepiSourcePosition read FSourcePos;
    property FileName: TFileName read FSourcePos.FileName;
    property Line: Integer read FSourcePos.Line;
    property Col: Integer read FSourcePos.Col;

    property IndexAsChild: Integer read GetIndexAsChild;
    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: TSepiParseTreeNode read GetChildren;

    property Compiler: TSepiCompilerBase read GetCompiler;
    property UnitCompiler: TSepiUnitCompiler read GetUnitCompiler;
    property MethodCompiler: TSepiMethodCompiler read GetMethodCompiler;
    property SepiRoot: TSepiRoot read GetSepiRoot;
    property SepiUnit: TSepiUnit read GetSepiUnit;
    property SepiContext: TSepiComponent read GetSepiContext;
    property LanguageRules: TSepiLanguageRules read GetLanguageRules;
    property SystemUnit: TSepiSystemUnit read GetSystemUnit;

    property AsText: string read GetAsText;
  end;

  {*
    Terminal
    @author sjrd
    @version 1.0
  *}
  TSepiTerminal = class(TSepiParseTreeNode)
  private
    FRepresentation: string; /// Repr�sentation du terminal dans le source
  protected
    function GetAsText: string; override;
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition;
      const ARepresentation: string); overload; virtual;
    constructor Create(AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition;
      const ARepresentation: string); overload;
    constructor Clone(Source: TSepiTerminal; AParent: TSepiNonTerminal = nil);

    procedure Parse;

    property Representation: string read FRepresentation;
  end;

  /// Classe de TSepiTerminal
  TSepiTerminalClass = class of TSepiTerminal;

  {*
    Non-terminal
    @author sjrd
    @version 1.0
  *}
  TSepiNonTerminal = class(TSepiParseTreeNode)
  private
    FChildren: TObjectList; /// Liste des enfants
  protected
    procedure AddChild(Index: Integer; Child: TSepiParseTreeNode); virtual;
    procedure RemoveChild(Child: TSepiParseTreeNode); virtual;

    function GetChildCount: Integer; override;
    function GetChildren(Index: Integer): TSepiParseTreeNode; override;

    function GetAsText: string; override;

    procedure ChildBeginParsing(Child: TSepiParseTreeNode); virtual;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); virtual;
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); overload; virtual;
    constructor Create(AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); overload;
    destructor Destroy; override;

    function IndexOf(Child: TSepiParseTreeNode): Integer;
  end;

  /// Classe de TSepiNonTerminal
  TSepiNonTerminalClass = class of TSepiNonTerminal;

  {*
    Enregistrement d'un gestionnaire de message
    @author sjrd
    @version 1.0
  *}
  TMessageHandlerRec = record
    MsgID: Word;      /// ID du message g�r�
    Handler: TObject; /// Objet qui g�re ce message
  end;

  /// Tableau dynamique de TMessageHandlerRec
  TMessageHandlerRecDynArray = array of TMessageHandlerRec;

  {*
    Noeud racine d'un arbre syntaxique Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiParseTreeRootNode = class(TSepiNonTerminal)
  private
    FUnitCompiler: TSepiUnitCompiler; /// Compilateur d'unit�
    FSepiRoot: TSepiRoot;             /// Racine Sepi
    FSepiUnit: TSepiUnit;             /// Unit� Sepi en cours de compilation

    FErrors: TSepiCompilerErrorList; /// Gestionnaire d'erreurs de compilation

    FMessageHandlers: TMessageHandlerRecDynArray; /// Gestionnaires de messages
  protected
    procedure SetUnitCompiler(AUnitCompiler: TSepiUnitCompiler);
    procedure SetSepiUnit(ASepiUnit: TSepiUnit;
      LanguageRulesClass: TSepiLanguageRulesClass);
  public
    constructor Create(AClass: TSepiSymbolClass; ASepiRoot: TSepiRoot;
      AErrors: TSepiCompilerErrorList); virtual;

    procedure DefaultHandler(var Msg); override;

    procedure RegisterMessageHandler(MsgID: Word; Handler: TObject);
    procedure UnregisterMessageHandler(MsgID: Word; Handler: TObject);

    property UnitCompiler: TSepiUnitCompiler read FUnitCompiler;
    property SepiRoot: TSepiRoot read FSepiRoot;
    property SepiUnit: TSepiUnit read FSepiUnit;

    property Errors: TSepiCompilerErrorList read FErrors;
  end;

  /// Classe de TSepiParseTreeRootNode
  TSepiParseTreeRootNodeClass = class of TSepiParseTreeRootNode;

  {*
    Non-terminal cach�, que le parent ne conna�t pas
    Attention ! Puisque le parent ne conna�t pas un non-terminal cach�, il n'est
    pas en mesure de le lib�rer non plus. Tout non-terminal cach� doit assurer
    sa propre destruction, par exemple dans EndParsing.
    @author sjrd
    @version 1.0
  *}
  TSepiHiddenNonTerminal = class(TSepiNonTerminal)
  private
    FIndexAsChild: Integer; /// Index parmi ses fr�res (guess only)
  protected
    procedure AddToParent(Index: Integer); override;
    procedure RemoveFromParent; override;

    procedure SetIndexAsChild(Value: Integer);
  public
    constructor Create(AParent: TSepiNonTerminal; AClass: TSepiSymbolClass;
      const ASourcePos: TSepiSourcePosition); override;

    procedure BeginParsing; override;
    procedure EndParsing; override;

    property IndexAsChild: Integer read FIndexAsChild;
  end;

function CheckIdentFound(const Expression: ISepiExpression;
  const Identifier: string; Node: TSepiParseTreeNode): Boolean; overload;
function CheckIdentFound(Component: TSepiComponent;
  const Identifier: string; Node: TSepiParseTreeNode): Boolean; overload;

function CheckTypeMatch(AssignedType, ExpressionType: TSepiType;
  Node: TSepiParseTreeNode): Boolean;

implementation

{-----------------}
{ Global routines }
{-----------------}

{*
  V�rifie qu'un identificateur a bien �t� trouv�, et produit une erreur sinon
  @param Expression   Expression � v�rifier
  @param Identifier   Identificateur, pour le message d'erreur
  @param Node         Symbole de grammaire, pour la position
  @return True si l'identificateur a �t� trouv�, False sinon
*}
function CheckIdentFound(const Expression: ISepiExpression;
  const Identifier: string; Node: TSepiParseTreeNode): Boolean; overload;
begin
  Result := Expression <> nil;
  if not Result then
    Node.MakeError(Format(SIdentifierNotFound, [Identifier]));
end;

{*
  V�rifie qu'un identificateur a bien �t� trouv�, et produit une erreur sinon
  @param Component         Component � v�rifier
  @param Identifier   Identificateur, pour le message d'erreur
  @param Node         Symbole de grammaire, pour la position
  @return True si l'identificateur a �t� trouv�, False sinon
*}
function CheckIdentFound(Component: TSepiComponent;
  const Identifier: string; Node: TSepiParseTreeNode): Boolean; overload;
begin
  Result := Component <> nil;
  if not Result then
    Node.MakeError(Format(SIdentifierNotFound, [Identifier]));
end;

{*
  V�rifie que des types correspondent, et produit une erreur sinon
  @param AssignedType     Type de la variable assign�e
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

{--------------------------}
{ TSepiParseTreeNode class }
{--------------------------}

{*
  Cr�e un noeud de l'arbre syntaxique
  @param AParent     Parent dans l'arbre syntaxique
  @param AClass      Classe de symbole
  @param APosition   Position dans le source
*}
constructor TSepiParseTreeNode.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition);
begin
  inherited Create;

  FParent := AParent;
  FClass := AClass;
  FSourcePos := ASourcePos;
end;

{*
  Cr�e un noeud de l'arbre syntaxique sans parent
  @param AClass      Classe de symbole
  @param APosition   Position dans le source
*}
constructor TSepiParseTreeNode.Create(AClass: TSepiSymbolClass;
  const ASourcePos: TSepiSourcePosition);
begin
  Create(nil, AClass, ASourcePos);
end;

{*
  Notifie qu'un anc�tre a chang�
*}
procedure TSepiParseTreeNode.DoAncestorChanged;
var
  I: Integer;
begin
  if Self is TSepiParseTreeRootNode then
    FRootNode := TSepiParseTreeRootNode(Self)
  else if Parent = nil then
    FRootNode := nil
  else
    FRootNode := Parent.RootNode;

  AncestorChanged;

  for I := 0 to ChildCount-1 do
    Children[I].DoAncestorChanged;
end;

{*
  Index parmi ses fr�res
  @return Index parmi les enfants de son parent, ou -1 si n'a pas de parent
*}
function TSepiParseTreeNode.GetIndexAsChild: Integer;
begin
  if Parent <> nil then
    Result := Parent.IndexOf(Self)
  else
    Result := -1;
end;

{*
  Compilateur d'unit�
  @return Compilateur d'unit�, si rattach� � un noeud racine, nil sinon
*}
function TSepiParseTreeNode.GetUnitCompiler: TSepiUnitCompiler;
begin
  if RootNode <> nil then
    Result := RootNode.UnitCompiler
  else
    Result := nil;
end;

{*
  Compilateur
  @return Compilateur, si rattach� � un noeud racine, nil sinon
*}
function TSepiParseTreeNode.GetCompiler: TSepiCompilerBase;
begin
  Result := GetMethodCompiler;
  if Result = nil then
    Result := GetUnitCompiler;
end;

{*
  Racine Sepi
  @return Racine Sepi, si rattach� � un noeud racine, nil sinon
*}
function TSepiParseTreeNode.GetSepiRoot: TSepiRoot;
begin
  if RootNode <> nil then
    Result := RootNode.SepiRoot
  else
    Result := nil;
end;

{*
  Unit� Sepi en cours de compilation
  @return Unit� Sepi, si rattach� � un noeud racine, nil sinon
*}
function TSepiParseTreeNode.GetSepiUnit: TSepiUnit;
begin
  if RootNode <> nil then
    Result := RootNode.SepiUnit
  else
    Result := nil;
end;

{*
  R�gles du langage utilis�
  @return R�gles du langage utilis�, si rattach� � un noeud racine, nil sinon
*}
function TSepiParseTreeNode.GetLanguageRules: TSepiLanguageRules;
begin
  if RootNode <> nil then
    Result := RootNode.UnitCompiler.LanguageRules
  else
    Result := nil;
end;

{*
  Unit� System
  @return Unit� System, si rattach� � un noeud racine, nil sinon
*}
function TSepiParseTreeNode.GetSystemUnit: TSepiSystemUnit;
begin
  if RootNode <> nil then
    Result := RootNode.UnitCompiler.SystemUnit
  else
    Result := nil;
end;

{*
  Modifie la classe de symbole
  @param Value   Nouvelle classe de symbole
*}
procedure TSepiParseTreeNode.SetSymbolClass(Value: TSepiSymbolClass);
begin
  FClass := Value;
end;

{*
  Modifie le parent syntaxique
  @param Value   Nouveau parent syntaxique
*}
procedure TSepiParseTreeNode.SetSyntacticParent(Value: TSepiNonTerminal);
begin
  FSyntacticParent := Value;
end;

{*
  Construit une expression, vide, dans le contexte de ce noeud
  @return Expression construite
*}
function TSepiParseTreeNode.MakeExpression: ISepiExpression;
begin
  Result := TSepiExpression.Create(Compiler);
  Result.SourcePos := SourcePos;
end;

{*
  Nombre d'enfants
  @return Nombre d'enfants
*}
function TSepiParseTreeNode.GetChildCount: Integer;
begin
  Result := 0;
end;

{*
  Tableau zero-based des enfants
  @param Index   Index d'un enfant
  @return Enfant � l'index sp�cifi�
*}
function TSepiParseTreeNode.GetChildren(Index: Integer): TSepiParseTreeNode;
begin
  raise EListError.CreateFmt(SListIndexError, [Index]);
end;

{*
  Ajoute le noeud � son parent
  @param Index   Index o� l'ajouter (-1 pour l'ajouter � la fin)
*}
procedure TSepiParseTreeNode.AddToParent(Index: Integer);
begin
  if SyntacticParent = nil then
    SetSyntacticParent(Parent);

  Parent.AddChild(Index, Self);
end;

{*
  Retire le noeud de son parent
*}
procedure TSepiParseTreeNode.RemoveFromParent;
begin
  Parent.RemoveChild(Self);
end;

{*
  M�thode de notification appel�e lorsqu'un anc�tre a �t� modifi�
  Lorsque cette m�thode est appel�e, RootNode a d�j� �t� modifi� pour refl�ter
  la propri�t� RootNode du parent. Et apr�s cet appel, les enfants seront
  notifi�s � leur tour du changement d'un anc�tre.
*}
procedure TSepiParseTreeNode.AncestorChanged;
begin
end;

{*
  Compilateur de m�thode (peut �tre nil)
  L'impl�mentation par d�faut, dans TSepiParseTreeNode, transf�re la requ�te au
  noeud parent.
  @return Compilateur de m�thode (peut �tre nil)
*}
function TSepiParseTreeNode.GetMethodCompiler: TSepiMethodCompiler;
begin
  if Parent <> nil then
    Result := Parent.MethodCompiler
  else
    Result := nil;
end;

{*
  Contexte Sepi
  L'impl�mentation par d�faut, dans TSepiParseTreeNode, transf�re la requ�te au
  noeud parent.
  @return Contexte Sepi
*}
function TSepiParseTreeNode.GetSepiContext: TSepiComponent;
begin
  if Parent <> nil then
    Result := Parent.SepiContext
  else
    Result := SepiUnit;
end;

{*
  [@inheritDoc]
*}
procedure TSepiParseTreeNode.AfterConstruction;
begin
  inherited;

  if Parent <> nil then
    AddToParent(-1);

  DoAncestorChanged;
end;

{*
  [@inheritDoc]
*}
procedure TSepiParseTreeNode.BeforeDestruction;
begin
  inherited;

  if Parent <> nil then
    RemoveFromParent;
end;

{*
  [@inheritDoc]
*}
procedure TSepiParseTreeNode.DefaultHandler(var Message);
begin
  if Parent <> nil then
    Parent.Dispatch(Message);
end;

{*
  D�place un noeud dans l'arbre syntaxique
  @param NewParent   Nouveau parent
  @param Index       Index dans le parent (-1 pour l'ajouter � la fin)
*}
procedure TSepiParseTreeNode.Move(NewParent: TSepiNonTerminal;
  Index: Integer = -1);
begin
  if (Parent = nil) and (NewParent = nil) then
    Exit;

  if Parent = NewParent then
    Parent.FChildren.Move(Parent.FChildren.IndexOf(Self), Index)
  else
  begin
    if Parent <> nil then
      RemoveFromParent;

    FParent := NewParent;

    if Parent <> nil then
      AddToParent(Index);

    DoAncestorChanged;
  end;
end;

{*
  Trouve le noeud de la plus � droite de ce sous-arbre syntaxique
  Dans la plupart des analyseurs, il s'agit du dernier noeud analys�.
  @return Noeud le plus � droite de ce sous-arbre syntaxique
*}
function TSepiParseTreeNode.FindRightMost: TSepiParseTreeNode;
begin
  Result := Self;

  while (Result is TSepiNonTerminal) and
    (TSepiNonTerminal(Result).ChildCount > 0) do
  begin
    with TSepiNonTerminal(Result) do
      Result := Children[ChildCount-1];
  end;
end;

{*
  Teste si ce noeud a pour anc�tre un noeud donn�
  @param Ancestor   Anc�tre potentiel
  @return True si Ancestor est un anc�tre de ce noeud, False sinon
*}
function TSepiParseTreeNode.IsAncestor(Ancestor: TSepiParseTreeNode): Boolean;
begin
  Result := (Ancestor = Self) or
    ((Parent <> nil) and Parent.IsAncestor(Ancestor));
end;

{*
  Teste si ce noeud a pour anc�tre une classe de noeud donn�e
  @param AncestorClasse   Classe d'un anc�tre potentiel
  @return True si un anc�tre de ce noeud est un AncestorClass, False sinon
*}
function TSepiParseTreeNode.IsAncestor(AncestorClass: TClass): Boolean;
begin
  Result := (Self is AncestorClass) or
    ((Parent <> nil) and Parent.IsAncestor(AncestorClass));
end;

{*
  R�soud un identificateur dans le contexte de ce noeud
  L'impl�mentation par d�faut dans TSepiParseTreeNode transf�re la requ�te au
  noeud parent.
  @param Identifier   Identificateur recherch�
  @return Expression repr�sentant l'identificateur, ou nil si non trouv�
*}
function TSepiParseTreeNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  if Parent <> nil then
    Result := Parent.ResolveIdent(Identifier)
  else
    Result := nil;
end;

{*
  R�soud un identificateur dans le contexte de ce noeud
  �met un message d'erreur si l'identificateur n'a pas �t� trouv�.
  @param Node   Noeud dont le texte est l'identificateur � rechercher
  @return Expression repr�sentant l'identificateur, ou nil si non trouv�
*}
function TSepiParseTreeNode.ResolveIdentOrError(
  Node: TSepiParseTreeNode): ISepiExpression;
begin
  Result := ResolveIdent(Node.AsText);
  CheckIdentFound(Result, Node.AsText, Node);
end;

{*
  Recherche un meta dans le contexte de ce noeud
  @param Node   Noeud dont le texte est le nom du meta � rechercher
  @return Component trouv�, ou nil si non trouv�
*}
function TSepiParseTreeNode.LookFor(Node: TSepiParseTreeNode): TSepiComponent;
begin
  Result := SepiContext.LookFor(Node.AsText);
end;

{*
  Recherche un meta d'un type particulier dans le contexte de ce noeud
  @param Node            Noeud dont le texte est le nom du meta � rechercher
  @param RequiredClass   Classe de meta requise
  @return Component trouv�, ou nil si non trouv� ou pas de la bonne classe
*}
function TSepiParseTreeNode.LookFor(Node: TSepiParseTreeNode;
  RequiredClass: TSepiComponentClass): TSepiComponent;
begin
  Result := LookFor(Node);

  if not (Result is RequiredClass) then
    Result := nil;
end;

{*
  Recherche un meta dont le nom est le texte de ce noeud
  @param Node   Noeud dont le texte est le nom du meta � rechercher
  @return Component trouv�, ou nil si non trouv�
*}
function TSepiParseTreeNode.LookForSelfText: TSepiComponent;
begin
  Result := LookFor(Self);
end;

{*
  Recherche un meta d'un type particulier dont le nom est le texte de ce noeud
  @param RequiredClass   Classe de meta requise
  @return Component trouv�, ou nil si non trouv� ou pas de la bonne classe
*}
function TSepiParseTreeNode.LookForSelfText(
  RequiredClass: TSepiComponentClass): TSepiComponent;
begin
  Result := LookFor(Self, RequiredClass);
end;

{*
  Recherche un meta dans le contexte de ce noeud
  �met un message d'erreur si le meta n'a pas �t� trouv�.
  @param Node   Noeud dont le texte est le nom du meta � rechercher
  @return Component trouv�, ou nil si non trouv�
*}
function TSepiParseTreeNode.LookForOrError(
  Node: TSepiParseTreeNode): TSepiComponent;
begin
  Result := LookFor(Node);
  CheckIdentFound(Result, Node.AsText, Node);
end;

{*
  Recherche un meta d'un type particulier dans le contexte de ce noeud
  �met un message d'erreur si le meta n'a pas �t� trouv�, ou s'il n'est pas du
  type requis.
  @param Node               Noeud dont le texte est le nom du meta � rechercher
  @param RequiredClass      Classe de meta requise
  @param BadClassErrorMsg   Message d'erreur si mauvaise classe de meta
  @return Component trouv�, ou nil si non trouv� ou pas de la bonne classe
*}
function TSepiParseTreeNode.LookForOrError(Node: TSepiParseTreeNode;
  RequiredClass: TSepiComponentClass;
  const BadClassErrorMsg: string): TSepiComponent;
begin
  Result := LookForOrError(Node);

  if (Result <> nil) and (not (Result is RequiredClass)) then
  begin
    Node.MakeError(BadClassErrorMsg);
    Result := nil;
  end;
end;

{*
  Recherche un meta dont le nom est le texte de ce noeud
  �met un message d'erreur si le meta n'a pas �t� trouv�.
  @param Node   Noeud dont le texte est le nom du meta � rechercher
  @return Component trouv�, ou nil si non trouv�
*}
function TSepiParseTreeNode.LookForSelfTextOrError: TSepiComponent;
begin
  Result := LookForOrError(Self);
end;

{*
  Recherche un meta d'un type particulier dont le nom est le texte de ce noeud
  �met un message d'erreur si le meta n'a pas �t� trouv�, ou s'il n'est pas du
  type requis.
  @param RequiredClass      Classe de meta requise
  @param BadClassErrorMsg   Message d'erreur si mauvaise classe de meta
  @return Component trouv�, ou nil si non trouv� ou pas de la bonne classe
*}
function TSepiParseTreeNode.LookForSelfTextOrError(
  RequiredClass: TSepiComponentClass;
  const BadClassErrorMsg: string): TSepiComponent;
begin
  Result := LookForOrError(Self, RequiredClass, BadClassErrorMsg);
end;

{*
  Produit une erreur au niveau de ce noeud
  @param ErrorMsg   Message d'erreur
  @param Kind       Type d'erreur (d�faut = ekError)
*}
procedure TSepiParseTreeNode.MakeError(const ErrorMsg: string;
  Kind: TSepiErrorKind = ekError);
begin
  RootNode.Errors.MakeError(ErrorMsg, Kind, SourcePos);
end;

{*
  Commence l'analyse du noeud
*}
procedure TSepiParseTreeNode.BeginParsing;
begin
  if Parent <> nil then
    Parent.ChildBeginParsing(Self);
end;

{*
  Termine l'analyse du noeud
*}
procedure TSepiParseTreeNode.EndParsing;
begin
  if Parent <> nil then
    Parent.ChildEndParsing(Self);
end;

{---------------------}
{ TSepiTerminal class }
{---------------------}

{*
  Cr�e un nouveau terminal
  @param AParent          Parent dans l'arbre syntaxique
  @param AClass           Symbol class
  @param ASourcePos       Position dans le source
  @param ARepresentation  R�pr�sentation textuelle dans le source
*}
constructor TSepiTerminal.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition;
  const ARepresentation: string);
begin
  inherited Create(AParent, AClass, ASourcePos);

  FRepresentation := ARepresentation;
end;

{*
  Cr�e un nouveau terminal sans parent
  @param AClass           Symbol class
  @param ASourcePos       Position dans le source
  @param ARepresentation  R�pr�sentation textuelle dans le source
*}
constructor TSepiTerminal.Create(AClass: TSepiSymbolClass;
  const ASourcePos: TSepiSourcePosition; const ARepresentation: string);
begin
  Create(nil, AClass, ASourcePos, ARepresentation);
end;

{*
  Cl�ne un terminal
  @param Source    Terminal � recopier
  @param AParent   Parent du nouveau terminal
*}
constructor TSepiTerminal.Clone(Source: TSepiTerminal;
  AParent: TSepiNonTerminal = nil);
begin
  Create(AParent, Source.SymbolClass, Source.SourcePos, Source.Representation);
end;

{*
  [@inheritDoc]
*}
function TSepiTerminal.GetAsText: string;
begin
  Result := Representation;
end;

{*
  Raccourci pour appeler BeginParsing puis EndParsing
*}
procedure TSepiTerminal.Parse;
begin
  BeginParsing;
  EndParsing;
end;

{------------------------}
{ TSepiNonTerminal class }
{------------------------}

{*
  Cr�e un nouveau non-terminal
  @param AParent      Parent dans l'arbre syntaxique
  @param AClass       Classe de symbole
  @param ASourcePos   Position dans le source
*}
constructor TSepiNonTerminal.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition);
begin
  inherited Create(AParent, AClass, ASourcePos);

  FChildren := TObjectList.Create(False);
end;

{*
  Cr�e un nouveau non-terminal sans parent
  @param AClass       Classe de symbole
  @param ASourcePos   Position dans le source
*}
constructor TSepiNonTerminal.Create(AClass: TSepiSymbolClass;
  const ASourcePos: TSepiSourcePosition);
begin
  Create(nil, AClass, ASourcePos);
end;

{*
  [@inheritDoc]
*}
destructor TSepiNonTerminal.Destroy;
begin
  while FChildren.Count > 0 do
    FChildren[0].Free;
  FChildren.Free;

  inherited Destroy;
end;

{*
  Ajoute un enfant
  @param Child   Enfant � ajouter
*}
procedure TSepiNonTerminal.AddChild(Index: Integer; Child: TSepiParseTreeNode);
begin
  if Index < 0 then
    FChildren.Add(Child)
  else
    FChildren.Insert(Index, Child);
end;

{*
  Retire un enfant
  @param Child   Enfant � retirer
*}
procedure TSepiNonTerminal.RemoveChild(Child: TSepiParseTreeNode);
begin
  FChildren.Remove(Child);
end;

{*
  [@inheritDoc]
*}
function TSepiNonTerminal.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

{*
  [@inheritDoc]
*}
function TSepiNonTerminal.GetChildren(Index: Integer): TSepiParseTreeNode;
begin
  Result := TSepiParseTreeNode(FChildren[Index]);
end;

{*
  [@inheritDoc]
*}
function TSepiNonTerminal.GetAsText: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ChildCount-1 do
  begin
    if (Result <> '') and ((Children[I].Line <> Line) or
      (Children[I].Col - Col > Length(Result))) then
      Result := Result + ' ';
    Result := Result + Children[I].AsText;
  end;
end;

{*
  M�thode de notification appel�e lorsqu'un enfant commence son analyse
  @param Child   Enfant qui commence son analyse
*}
procedure TSepiNonTerminal.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
end;

{*
  M�thode de notification appel�e lorsqu'un enfant termine son analyse
  @param Child   Enfant qui termine son analyse
*}
procedure TSepiNonTerminal.ChildEndParsing(Child: TSepiParseTreeNode);
begin
end;

{*
  Cherche la position d'un enfant
  @param Child   Enfant recherch�
  @return Index de l'enfant parmi ses fr�res, ou -1 si non trouv�
*}
function TSepiNonTerminal.IndexOf(Child: TSepiParseTreeNode): Integer;
begin
  Result := FChildren.IndexOf(Child);
end;

{------------------------------}
{ TSepiParseTreeRootNode class }
{------------------------------}

{*
  Cr�e un noeud racine d'arbre syntaxique
  @param AClass      Classe de symbole
  @param ASepiRoot   Racine Sepi
  @param AErrors     Gestionnaire d'erreurs de compilation
*}
constructor TSepiParseTreeRootNode.Create(AClass: TSepiSymbolClass;
  ASepiRoot: TSepiRoot; AErrors: TSepiCompilerErrorList);
begin
  inherited Create(AClass, SepiNoPosition);

  FSepiRoot := ASepiRoot;
  FErrors := AErrors;
end;

{*
  Renseigne le compilateur d'unit�, et donc aussi l'unit� Sepi
  @param AUnitCompiler   Compilateur d'unit�
*}
procedure TSepiParseTreeRootNode.SetUnitCompiler(
  AUnitCompiler: TSepiUnitCompiler);
begin
  FUnitCompiler := AUnitCompiler;
  FSepiUnit := AUnitCompiler.SepiUnit;
end;

{*
  Renseigne l'unit� Sepi, et cr�e un compilateur d'unit�
  @param ASepiUnit   Unit� Sepi � compiler
*}
procedure TSepiParseTreeRootNode.SetSepiUnit(ASepiUnit: TSepiUnit;
  LanguageRulesClass: TSepiLanguageRulesClass);
begin
  FUnitCompiler := TSepiUnitCompiler.Create(Errors, ASepiUnit,
    LanguageRulesClass);
  FSepiUnit := ASepiUnit;
end;

{*
  [@inheritDoc]
*}
procedure TSepiParseTreeRootNode.DefaultHandler(var Msg);
var
  I: Integer;
begin
  for I := 0 to Length(FMessageHandlers)-1 do
  begin
    with FMessageHandlers[I] do
      if (MsgID = TDispatchMessage(Msg).MsgID) and (Handler <> nil) then
        Handler.Dispatch(Msg);
  end;
end;

{*
  Enregistre un gestionnaire de message
  @param MsgID     ID du message g�r�
  @param Handler   Objet qui g�re le message
*}
procedure TSepiParseTreeRootNode.RegisterMessageHandler(MsgID: Word;
  Handler: TObject);
var
  I, Len: Integer;
begin
  Len := Length(FMessageHandlers);

  // First registration
  if Len = 0 then
  begin
    SetLength(FMessageHandlers, 4);
    FillChar(FMessageHandlers[0], 4*SizeOf(TMessageHandlerRec), 0);

    FMessageHandlers[0].MsgID := MsgID;
    FMessageHandlers[0].Handler := Handler;
    Exit;
  end;

  // Reuse an empty bucket
  for I := 0 to Len-1 do
  begin
    if FMessageHandlers[I].Handler = nil then
    begin
      FMessageHandlers[I].MsgID := MsgID;
      FMessageHandlers[I].Handler := Handler;
      Exit;
    end;
  end;

  // Grow the array
  SetLength(FMessageHandlers, 2*Len);
  FillChar(FMessageHandlers[Len], Len*SizeOf(TMessageHandlerRec), 0);

  FMessageHandlers[Len].MsgID := MsgID;
  FMessageHandlers[Len].Handler := Handler;
end;

{*
  Enregistre un gestionnaire de message
  @param MsgID     ID du message g�r�
  @param Handler   Objet qui g�re le message
*}
procedure TSepiParseTreeRootNode.UnregisterMessageHandler(MsgID: Word;
  Handler: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(FMessageHandlers)-1 do
  begin
    if (FMessageHandlers[I].MsgID = MsgID) and
      (FMessageHandlers[I].Handler = Handler) then
    begin
      FMessageHandlers[I].Handler := nil;
      Exit;
    end;
  end;
end;

{------------------------------}
{ TSepiHiddenNonTerminal class }
{------------------------------}

{*
  Cr�e le non-terminal
*}
constructor TSepiHiddenNonTerminal.Create(AParent: TSepiNonTerminal;
  AClass: TSepiSymbolClass; const ASourcePos: TSepiSourcePosition);
begin
  inherited;

  FIndexAsChild := -1;
end;

{*
  [@inheritDoc]
*}
procedure TSepiHiddenNonTerminal.AddToParent(Index: Integer);
begin
  if SyntacticParent = nil then
    SetSyntacticParent(Parent);

  if Index < 0 then
    FIndexAsChild := Parent.ChildCount
  else
    FIndexAsChild := Index;
end;

{*
  [@inheritDoc]
*}
procedure TSepiHiddenNonTerminal.RemoveFromParent;
begin
  FIndexAsChild := -1;
end;

{*
  Modifie la valeur de la propri�t� IndexAsChild
  @param Value   Nouvelle valeur
*}
procedure TSepiHiddenNonTerminal.SetIndexAsChild(Value: Integer);
begin
  FIndexAsChild := Value;
end;

{*
  [@inheritDoc]
*}
procedure TSepiHiddenNonTerminal.BeginParsing;
begin
end;

{*
  [@inheritDoc]
*}
procedure TSepiHiddenNonTerminal.EndParsing;
begin
end;

end.

