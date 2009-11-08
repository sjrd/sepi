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
  Utilitaires d'analyse syntaxique LL(1) pour Sepi
  @author sjrd
  @version 1.0
*}
unit SepiLL1ParserUtils;

interface

{$D-,L-}

uses
  SysUtils, Contnrs, SepiParseTrees, SepiCompilerErrors, SepiCompilerConsts,
  SepiLexerUtils, SepiParserUtils;

type
  {*
    Erreur de parser LL(1) Sepi
  *}
  ESepiLL1ParserError = class(Exception);

  {*
    Identificateur de règle (contenu des tables d'analyse)
  *}
  TRuleID = type Smallint;

  {*
    Pile d'un analyse syntaxique LL(1)
    @author sjrd
    @version 1.0
  *}
  TSepiLL1ParsingStack = class(TObject)
  private
    FStack: TStack;     /// Pile interne
    FTryCount: Integer; /// Indique le nombre d'imbrication de try

    function GetEmpty: Boolean;
    function GetIsInTry: Boolean;
  public
    constructor Create(StartSymbol: TSepiSymbolClass);
    destructor Destroy; override;

    procedure Push(Symbol: TSepiSymbolClass);
    function Pop: TSepiSymbolClass;

    procedure PushTry(Tag: TObject);
    procedure PopTry;
    function UnwindTry: TObject;

    property Empty: Boolean read GetEmpty;
    property IsInTry: Boolean read GetIsInTry;
  end;

  {*
    Faux non-terminal
    Un faux non-terminal ne s'attache pas à son parent, et se libère
    automatiquement dès que son analyse est terminée.
    @author sjrd
    @version 1.0
  *}
  TSepiFakeNonTerminal = class(TSepiHiddenNonTerminal)
  public
    procedure EndParsing; override;
  end;

  {*
    Non-terminal "child-through"
    Un non-terminal "child-through" redirige tous les enfants qui lui sont
    attachés vers son parent, à la position qu'il occupe - ou plutôt, qu'il
    devrait occuper, lui-même étant invisible.
    L'utilisation de non-terminaux "child-through" est un moyen particulièrement
    simple d'implémenter des grammaires EBNF avec un analyseur BNF.
    @author sjrd
    @version 1.0
  *}
  TSepiChildThroughNonTerminal = class(TSepiHiddenNonTerminal)
  protected
    procedure AddChild(Index: Integer; Child: TSepiParseTreeNode); override;
  public
    procedure EndParsing; override;
  end;

  {*
    Procédure de push d'un choix sur la pile prédictive
  *}
  TPushChoiceProc = procedure of object;

  {*
    Tag marquant un try dans la pile prédictive
    Sauf cas exceptionnels, vous ne devriez pas vous servir directement de
    TTryTag. Cette classe est utilisée en interne par TSepiCustomLL1Parser pour
    marquer un try dans sa pile prédictive.
    @author sjrd
    @version 1.0
  *}
  TTryTag = class(TObject)
  private
    FBookmark: TSepiLexerBookmark; /// Marque-page de l'analyseur lexical
    FAltRule: TRuleID;             /// Règle alternative
    FCurrent: TSepiNonTerminal;    /// Non-terminal courant sur ce try
  public
    constructor Create(ABookmark: TSepiLexerBookmark; AAltRule: TRuleID;
      ACurrent: TSepiNonTerminal);
    destructor Destroy; override;

    property Bookmark: TSepiLexerBookmark read FBookmark;
    property AltRule: TRuleID read FAltRule;
    property Current: TSepiNonTerminal read FCurrent;
  end;

  {*
    Classe de base pour les analyseurs syntaxiques LL(1) Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiCustomLL1Parser = class(TSepiCustomParser)
  private
    FStack: TSepiLL1ParsingStack; /// Pile prédictive

    FCurrent: TSepiNonTerminal; /// Non-terminal courant

    procedure SetCurrent(Value: TSepiNonTerminal);

    procedure BeginFakeNonTerminal;
    procedure BackToParent;
    procedure PopTry;
    procedure BeginRootNode(RootNode: TSepiNonTerminal);
    procedure ParseTerminal(Symbol: TSepiSymbolClass);
    procedure BeginNonTerminal(Symbol: TSepiSymbolClass);
    procedure BeginNonTerminalParsing;

    property Stack: TSepiLL1ParsingStack read FStack;
  protected
    /// Tableau des procédures qui push les choix sur la pile prédictive
    PushChoiceProcs: array of TPushChoiceProc;

    procedure PushSymbol(Symbol: TSepiSymbolClass);
    procedure PushFakeSymbol(Symbol: TSepiSymbolClass);
    procedure PushBackToParent;
    procedure PushTry(AltRule: TRuleID);

    procedure PushEmptyChoice;

    procedure UnwindTry;

    procedure UnwindTryOrSyntaxError(const Expected: string); overload;
    procedure UnwindTryOrSyntaxError(
      ExpectedSymbol: TSepiSymbolClass); overload;

    function IsTerminal(Symbol: TSepiSymbolClass): Boolean; virtual; abstract;
    function IsNonTerminal(
      Symbol: TSepiSymbolClass): Boolean; virtual; abstract;

    procedure InitPushChoiceProcs; virtual;

    function GetExpectedString(
      ExpectedSymbol: TSepiSymbolClass): string; virtual; abstract;

    function GetParsingTable(NonTerminalClass: TSepiSymbolClass;
      TerminalClass: TSepiSymbolClass): TRuleID; virtual; abstract;

    function GetNonTerminalClass(
      Symbol: TSepiSymbolClass): TSepiNonTerminalClass; virtual; abstract;
    function CreateNonTerminal(Parent: TSepiNonTerminal;
      Symbol: TSepiSymbolClass;
      const SourcePos: TSepiSourcePosition): TSepiNonTerminal;

    procedure InternalParse(RootNode: TSepiNonTerminal); override;

    property Current: TSepiNonTerminal read FCurrent;
  public
    constructor Create(ALexer: TSepiCustomLexer); override;
    destructor Destroy; override;
  end;

implementation

const
  scNextChildIsFake = -2; /// Code spécial : le prochain enfant est un fake
  scBackToParent = -3;    /// Code spécial : retourner au parent
  scPopTry = -4;          /// Code spécial : pop un try-tag depuis la pile
  scRootNode = -5;        /// Code spécial : noeud racine

  /// Sentinelle pour un try dans un pile de parser LL(1)
  scTrySentinel = Pointer($FFFFFFFF);

{----------------------------}
{ TSepiLL1ParsingStack class }
{----------------------------}

{*
  Crée une nouvelle pile d'analyse syntaxique LL(1)
  @param StartSymbol   Symbole de départ de la grammaire
*}
constructor TSepiLL1ParsingStack.Create(StartSymbol: TSepiSymbolClass);
begin
  inherited Create;

  FStack := TStack.Create;
  Push(StartSymbol);
end;

{*
  [@inheritDoc]
*}
destructor TSepiLL1ParsingStack.Destroy;
begin
  FStack.Free;

  inherited Destroy;
end;

{*
  Teste si la pile est vide
  @return True si la pile est vide, False sinon
*}
function TSepiLL1ParsingStack.GetEmpty: Boolean;
begin
  Result := FStack.Count = 0;
end;

{*
  Indique si la pile est dans un try
  @return True si la pile est dans un try, False sinon
*}
function TSepiLL1ParsingStack.GetIsInTry: Boolean;
begin
  Result := FTryCount > 0;
end;

{*
  Push un symbole sur la pile
  @param Symbol   Symbole à pusher sur la pile
*}
procedure TSepiLL1ParsingStack.Push(Symbol: TSepiSymbolClass);
begin
  FStack.Push(Pointer(Symbol));
end;

{*
  Pop un symbole du haut de la pile
  @return Symbole poppé
*}
function TSepiLL1ParsingStack.Pop: TSepiSymbolClass;
begin
  if FStack.Peek = scTrySentinel then
    raise ESepiLL1ParserError.Create(STopOfStackIsNotASymbol);

  Result := TSepiSymbolClass(FStack.Pop);
end;

{*
  Push un try sur la pile
  @param Tag   Tag à apposer au try
*}
procedure TSepiLL1ParsingStack.PushTry(Tag: TObject);
begin
  FStack.Push(Pointer(Tag));
  FStack.Push(scTrySentinel);
  Inc(FTryCount);
end;

{*
  Pop un try du sommet de la pile et libère son tag
*}
procedure TSepiLL1ParsingStack.PopTry;
begin
  if FStack.Peek <> scTrySentinel then
    raise ESepiLL1ParserError.Create(STopOfStackIsNotATry);
  FStack.Pop; // try sentinel
  TObject(FStack.Pop).Free; // alternative rule
  Dec(FTryCount);
end;

{*
  Déroule la pile jusqu'à trouver un try, le retire et récupère le tag
  @return Tag apposé au try
*}
function TSepiLL1ParsingStack.UnwindTry: TObject;
begin
  if not IsInTry then
    raise ESepiLL1ParserError.Create(SNotInTry);

  while FStack.Pop <> scTrySentinel do;
  Result := TObject(FStack.Pop);
  Dec(FTryCount);
end;

{----------------------------}
{ TSepiFakeNonTerminal class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiFakeNonTerminal.EndParsing;
begin
  inherited;
  Free;
end;

{------------------------------------}
{ TSepiChildThroughNonTerminal class }
{------------------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiChildThroughNonTerminal.AddChild(Index: Integer;
  Child: TSepiParseTreeNode);
begin
  inherited;

  Child.Move(Parent, IndexAsChild);

  if not (Parent is TSepiChildThroughNonTerminal) then
    SetIndexAsChild(IndexAsChild + 1);
end;

{*
  [@inheritDoc]
*}
procedure TSepiChildThroughNonTerminal.EndParsing;
var
  Index, I: Integer;
begin
  inherited;

  Index := IndexAsChild;
  for I := ChildCount-1 downto 0 do
    Children[I].Move(Parent, Index);
  Free;
end;

{---------------}
{ TTryTag class }
{---------------}

{*
  Crée un tag de try
  @param ABookmark   Marque-page de l'analyseur lexical
  @param AAltRule    Règle alternative
  @param ACurrent    Non-terminal courant sur ce try
*}
constructor TTryTag.Create(ABookmark: TSepiLexerBookmark; AAltRule: TRuleID;
  ACurrent: TSepiNonTerminal);
begin
  inherited Create;

  FBookmark := ABookmark;
  FAltRule := AAltRule;
  FCurrent := ACurrent;
end;

{*
  [@inheritDoc]
*}
destructor TTryTag.Destroy;
begin
  FBookmark.Free;

  inherited;
end;

{----------------------------}
{ TSepiCustomLL1Parser class }
{----------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiCustomLL1Parser.Create(ALexer: TSepiCustomLexer);
begin
  inherited;

  FStack := TSepiLL1ParsingStack.Create(scRootNode);

  InitPushChoiceProcs;
end;

{*
  [@inheritDoc]
*}
destructor TSepiCustomLL1Parser.Destroy;
begin
  FStack.Free;

  inherited;
end;

procedure TSepiCustomLL1Parser.SetCurrent(Value: TSepiNonTerminal);
begin
  FCurrent := Value;
  Lexer.Context := Value;
end;

{*
  Commence un emballage dans un faux non-terminal
*}
procedure TSepiCustomLL1Parser.BeginFakeNonTerminal;
begin
  SetCurrent(TSepiFakeNonTerminal.Create(Current, scNextChildIsFake,
    CurTerminal.SourcePos));

  Current.BeginParsing;
end;

{*
  Retourne au parent
*}
procedure TSepiCustomLL1Parser.BackToParent;
var
  Temp: TSepiNonTerminal;
begin
  Temp := Current;
  SetCurrent(Temp.SyntacticParent);
  Temp.EndParsing;
end;

{*
  Pop un try de la pile prédictive
*}
procedure TSepiCustomLL1Parser.PopTry;
begin
  Stack.PopTry;
end;

{*
  Commence l'analyse du noeud racine
  @param RootNode   Noeud racine
*}
procedure TSepiCustomLL1Parser.BeginRootNode(RootNode: TSepiNonTerminal);
begin
  Assert(Current = nil);

  SetCurrent(RootNode);
  BeginNonTerminalParsing;
end;

{*
  Analyse un terminal
  @param Symbol   Symbole de terminal attendu
*}
procedure TSepiCustomLL1Parser.ParseTerminal(Symbol: TSepiSymbolClass);
begin
  if CurTerminal.SymbolClass <> Symbol then
    UnwindTryOrSyntaxError(Symbol)
  else
  begin
    TSepiTerminalClass(CurTerminal.ClassType).Clone(
      CurTerminal, Current).Parse;
    Lexer.NextTerminal;
  end;
end;

{*
  Commence l'analyse d'un non-terminal
  @param Symbol   Symbole de non-terminal attendu
*}
procedure TSepiCustomLL1Parser.BeginNonTerminal(Symbol: TSepiSymbolClass);
begin
  Assert(Current <> nil);

  SetCurrent(CreateNonTerminal(Current, Symbol, CurTerminal.SourcePos));
  BeginNonTerminalParsing;
end;

{*
  Commence l'analyse du non-terminal courant
*}
procedure TSepiCustomLL1Parser.BeginNonTerminalParsing;
var
  Rule: TRuleID;
begin
  Current.BeginParsing; // This could modify Current.SymbolClass!

  Rule := GetParsingTable(Current.SymbolClass, CurTerminal.SymbolClass);

  if Rule < 0 then
    UnwindTryOrSyntaxError(Current.SymbolClass)
  else
    PushChoiceProcs[Rule];
end;

{*
  Push un symbole grammatical sur la pile prédictive
  @param Symbol   Symbole à placer
*}
procedure TSepiCustomLL1Parser.PushSymbol(Symbol: TSepiSymbolClass);
begin
  Stack.Push(Symbol);
end;

{*
  Push un faux symbole grammatical sur la pile prédictive
  Un faux symbole sera emballé dans un TSepiFakeNonTerminal, et n'apparaîtra
  donc pas réellement dans l'arbre syntaxique.
  @param Symbol   Symbole à placer
*}
procedure TSepiCustomLL1Parser.PushFakeSymbol(Symbol: TSepiSymbolClass);
begin
  Stack.Push(scBackToParent);
  Stack.Push(Symbol);
  Stack.Push(scNextChildIsFake);
end;

{*
  Push un code indiquant de remonter au parent sur la pile prédictive
*}
procedure TSepiCustomLL1Parser.PushBackToParent;
begin
  Stack.Push(scBackToParent);
end;

{*
  Push un try sur la pile prédictive
  @param Current   Non-terminal courant
  @param AltRule   Règle alternative
*}
procedure TSepiCustomLL1Parser.PushTry(AltRule: TRuleID);
begin
  Stack.PushTry(TTryTag.Create(Lexer.MakeBookmark, AltRule, Current));
  Stack.Push(scPopTry);
end;

{*
  Push un choix vide sur la pile prédictive
*}
procedure TSepiCustomLL1Parser.PushEmptyChoice;
begin
  PushBackToParent;
end;

{*
  Déroule le try courant
*}
procedure TSepiCustomLL1Parser.UnwindTry;
var
  TryTag: TTryTag;
  I: Integer;
begin
  TryTag := TTryTag(Stack.UnwindTry);
  try
    Lexer.ResetToBookmark(TryTag.Bookmark, False);
    PushChoiceProcs[TryTag.AltRule];
    SetCurrent(TryTag.Current);

    for I := Current.ChildCount-1 downto 0 do
      Current.Children[I].Free;
  finally
    TryTag.Free;
  end;
end;

{*
  Déroule le try courant s'il y en a un, sinon produit une erreur de syntaxe
  @param Expected   Représentation de ce qui était attendu, si erreur
  @return Non-terminal de reprise après le try déroule, le cas échéant
*}
procedure TSepiCustomLL1Parser.UnwindTryOrSyntaxError(const Expected: string);
begin
  if Stack.IsInTry then
    UnwindTry
  else
    MakeSyntaxError(Expected);
end;

{*
  Déroule le try courant s'il y en a un, sinon produit une erreur de syntaxe
  @param ExpectedSymbol   Symbole attendu, si erreur
  @return Non-terminal de reprise après le try déroule, le cas échéant
*}
procedure TSepiCustomLL1Parser.UnwindTryOrSyntaxError(
  ExpectedSymbol: TSepiSymbolClass);
begin
  if Stack.IsInTry then
    UnwindTry
  else
    MakeSyntaxError(GetExpectedString(ExpectedSymbol));
end;

{*
  Initialise le tableau PushChoiceProcs
*}
procedure TSepiCustomLL1Parser.InitPushChoiceProcs;
begin
  if Length(PushChoiceProcs) = 0 then
    SetLength(PushChoiceProcs, 1);

  PushChoiceProcs[0] := PushEmptyChoice;
end;

{*
  Crée un non-terminal de la bonne classe
  @param Parent      Parent du non-terminal à créer
  @param Symbol      Symbole du non-terminal
  @param SourcePos   Position du non-terminal dans le source
  @return Non-terminal créé
*}
function TSepiCustomLL1Parser.CreateNonTerminal(Parent: TSepiNonTerminal;
  Symbol: TSepiSymbolClass;
  const SourcePos: TSepiSourcePosition): TSepiNonTerminal;
begin
  Result := GetNonTerminalClass(Symbol).Create(Parent, Symbol, SourcePos);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomLL1Parser.InternalParse(RootNode: TSepiNonTerminal);
var
  Symbol: TSepiSymbolClass;
begin
  SetCurrent(nil);
  Lexer.Context := RootNode;

  Lexer.NextTerminal;

  while not Stack.Empty do
  begin
    Symbol := Stack.Pop;

    case Symbol of
      scNextChildIsFake:
        BeginFakeNonTerminal;
      scBackToParent:
        BackToParent;
      scPopTry:
        PopTry;
      scRootNode:
        BeginRootNode(RootNode);
    else
      if IsTerminal(Symbol) then
        ParseTerminal(Symbol)
      else if IsNonTerminal(Symbol) then
        BeginNonTerminal(Symbol)
      else
        Assert(False);
    end;
  end;
end;

end.

