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
  Utilitaires d'analyse syntaxique Sepi
  @author sjrd
  @version 1.0
*}
unit SepiParserUtils;

interface

{$D-,L-}

uses
  SysUtils, Classes, SepiCore, SepiCompilerErrors, SepiParseTrees,
  SepiCompilerConsts, SepiLexerUtils;

type
  {*
    Classe de base pour les analyseurs syntaxiques Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiCustomParser = class(TObject)
  private
    FErrors: TSepiCompilerErrorList; /// Erreurs de compilation
    FLexer: TSepiCustomLexer;        /// Analyseur lexical source

    function GetCurTerminal: TSepiTerminal;
  protected
    procedure MakeSyntaxError(const Expected: string);

    {*
      Crée un analyseur lexical par défaut pour cet analyseur syntaxique
      @param Code       Code à analyser
      @param FileName   Nom du fichier
      @return Analyseur lexical créé
    *}
    class function CreateDefaultLexer(Errors: TSepiCompilerErrorList;
      const Code: string;
      const FileName: string = ''): TSepiCustomLexer; virtual; abstract;

    {*
      Analyse syntaxique proprement dite
      @param RootNode   Noeud racine de l'arbre syntaxique
    *}
    procedure InternalParse(RootNode: TSepiNonTerminal); virtual; abstract;

    property Lexer: TSepiCustomLexer read FLexer;
  public
    constructor Create(ALexer: TSepiCustomLexer); virtual;
    destructor Destroy; override;

    class procedure Parse(RootNode: TSepiNonTerminal;
      Lexer: TSepiCustomLexer); overload;
    class procedure Parse(RootNode: TSepiNonTerminal;
      const Code: string; const FileName: string = ''); overload;

    property Errors: TSepiCompilerErrorList read FErrors;

    property CurTerminal: TSepiTerminal read GetCurTerminal;
  end;

  /// Classe de TSepiCustomParser
  TSepiCustomParserClass = class of TSepiCustomParser;

implementation

{-------------------------}
{ TSepiCustomParser class }
{-------------------------}

{*
  Crée un analyseur syntaxique
  @param ALexer   Analyseur lexical source
*}
constructor TSepiCustomParser.Create(ALexer: TSepiCustomLexer);
begin
  inherited Create;

  FErrors := ALexer.Errors;
  FLexer := ALexer;
end;

{*
  [@inheritDoc]
*}
destructor TSepiCustomParser.Destroy;
begin
  FLexer.Free;

  inherited Destroy;
end;

{*
  Terminal courant
  @return Terminal courant
*}
function TSepiCustomParser.GetCurTerminal: TSepiTerminal;
begin
  Result := Lexer.CurTerminal;
end;

{*
  Produit une erreur syntaxique
  @param Expected   Représentation de ce qui était attendu
*}
procedure TSepiCustomParser.MakeSyntaxError(const Expected: string);
begin
  Errors.MakeError(Format(SSyntaxError,
    [Expected, CurTerminal.Representation]), ekFatalError,
    CurTerminal.SourcePos);
end;

{*
  Analyse un code source
  @param RootNode   Noeud racine de l'arbre syntaxique à produire
  @param Lexer      Analyseur lexical source
*}
class procedure TSepiCustomParser.Parse(RootNode: TSepiNonTerminal;
  Lexer: TSepiCustomLexer);
begin
  with Create(Lexer) do
  try
    InternalParse(RootNode);
  finally
    Free;
  end;
end;

{*
  Analyse un code source, en utilisant l'analyseur lexical par défaut
  @param RootNode   Noeud racine de l'arbre syntaxique à produire
  @param Code       Code source à analyser
*}
class procedure TSepiCustomParser.Parse(RootNode: TSepiNonTerminal;
  const Code: string; const FileName: string = '');
begin
  Parse(RootNode, CreateDefaultLexer(RootNode.RootNode.Errors, Code, FileName));
end;

end.

