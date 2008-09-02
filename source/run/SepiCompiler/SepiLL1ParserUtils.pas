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

uses
  Contnrs, SepiParseTrees;

type
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
    FStack: TStack; /// Pile interne

    function GetEmpty: Boolean;
  public
    constructor Create(StartSymbol: TSepiSymbolClass);
    destructor Destroy; override;

    procedure Push(Symbol: TSepiSymbolClass);
    function Pop: TSepiSymbolClass;

    property Empty: Boolean read GetEmpty;
  end;

  {*
    Faux non-terminal
    Un faux non-terminal, lorsque son analyse est terminée, transfère tous ses
    enfants à son parent, avant de se libérer lui-même.
    C'est un moyen particulièrement simple d'implémenter des grammaires EBNF
    avec un analyseur BNF.
    @author sjrd
    @version 1.0
  *}
  TSepiFakeNonTerminal = class(TSepiNonTerminal)
  public
    procedure EndParsing; override;
  end;

implementation

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
  Result := TSepiSymbolClass(FStack.Pop);
end;

{----------------------------}
{ TSepiFakeNonTerminal class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiFakeNonTerminal.EndParsing;
var
  Index, I: Integer;
begin
  Index := IndexAsChild;
  for I := ChildCount-1 downto 0 do
    Children[I].Move(Parent, Index);
  Free;
end;

end.

