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
  Utilitaires d'analyse syntaxique LL(1) pour Sepi
  @author sjrd
  @version 1.0
*}
unit SepiLL1ParserUtils;

interface

{$D-,L-}

uses
  SysUtils, Contnrs, SepiParseTrees, SepiCompilerConsts;

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

implementation

const
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

end.

