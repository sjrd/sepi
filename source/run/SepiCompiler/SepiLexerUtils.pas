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
  Utilitaires d'analyse lexicale Sepi
  @author sjrd
  @version 1.0
*}
unit SepiLexerUtils;

interface

{$D-,L-}

uses
  SysUtils, Classes, ScUtils, SepiCore, SepiCompilerErrors, SepiParseTrees,
  SepiCompilerConsts;

const
  tkEof = 0;     /// Lexème fin de fichier
  tkBlank = 1;   /// Lexème blanc
  tkComment = 2; /// Lexème commentaire

type
  {*
    Fonction d'analyse d'un terminal
    @return True si un véritable terminal a été analysé, False sinon
  *}
  TSepiLexingProc = procedure of object;

  {*
    Erreur d'analyse lexicale
    @author sjrd
    @version 1.0
  *}
  ESepiLexicalError = class(ESepiError);

  {*
    Marque dans le source - utilisé pour retourner en arrière dans le code
    @author sjrd
    @version 1.0
  *}
  TSepiLexerBookmark = class(TObject)
  end;

  {*
    Classe de base pour les analyseurs lexicaux Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiCustomLexer = class(TObject)
  private
    FErrors: TSepiCompilerErrorList; /// Erreurs de compilation

    FExcludedTokens: TSysByteSet; /// Types de lexèmes à exclure complètement

    FContext: TSepiNonTerminal; /// Contexte courant (peut être nil)

    procedure SetExcludedTokens(const Value: TSysByteSet);
  protected
    procedure MakeError(const ErrorMsg: string;
      Kind: TSepiErrorKind = ekError);

    function GetCurrentPos: TSepiSourcePosition; virtual; abstract;
    function GetCurTerminal: TSepiTerminal; virtual; abstract;
    procedure SetContext(Value: TSepiNonTerminal); virtual;

    procedure DoNextTerminal; virtual; abstract;
  public
    constructor Create(AErrors: TSepiCompilerErrorList; const ACode: string;
      const AFileName: TFileName = ''); virtual;

    procedure NextTerminal;

    function MakeBookmark: TSepiLexerBookmark; virtual;
    procedure ResetToBookmark(Bookmark: TSepiLexerBookmark;
      FreeBookmark: Boolean = True); virtual;

    property Errors: TSepiCompilerErrorList read FErrors;

    property ExcludedTokens: TSysByteSet
      read FExcludedTokens write SetExcludedTokens;

    property CurrentPos: TSepiSourcePosition read GetCurrentPos;
    property CurTerminal: TSepiTerminal read GetCurTerminal;

    property Context: TSepiNonTerminal read FContext write SetContext;
  end;

  {*
    Bookmark pour TSepiBaseLexer
    @author sjrd
    @version 1.0
  *}
  TSepiBaseLexerBookmark = class(TSepiLexerBookmark)
  private
    FCursor: Integer;                 /// Index courant dans le source
    FCurrentPos: TSepiSourcePosition; /// Position courante
    FNextPos: TSepiSourcePosition;    /// Prochaine position
    FCurTerminal: TSepiTerminal;      /// Dernier terminal analysé
  public
    constructor Create(ACursor: Integer;
      const ACurrentPos, ANextPos: TSepiSourcePosition;
      ACurTerminal: TSepiTerminal);
    destructor Destroy; override;

    property Cursor: Integer read FCursor;
    property CurrentPos: TSepiSourcePosition read FCurrentPos;
    property NextPos: TSepiSourcePosition read FNextPos;
    property CurTerminal: TSepiTerminal read FCurTerminal;
  end;

  {*
    Classe de base pour les analyseurs lexicaux Sepi de base (non composites)
    @author sjrd
    @version 1.0
  *}
  TSepiBaseLexer = class(TSepiCustomLexer)
  private
    FCode: string;                    /// Code source à analyser
    FCursor: Integer;                 /// Index courant dans le source
    FCurrentPos: TSepiSourcePosition; /// Position courante
    FNextPos: TSepiSourcePosition;    /// Prochaine position
    FCurTerminal: TSepiTerminal;      /// Dernier terminal analysé
  protected
    function GetCurrentPos: TSepiSourcePosition; override;
    function GetCurTerminal: TSepiTerminal; override;

    procedure TerminalParsed(SymbolClass: TSepiSymbolClass;
      const Representation: string);

    procedure CursorForward(Amount: Integer = 1);

    procedure IdentifyKeyword(const Key: string;
      var SymbolClass: TSepiSymbolClass); virtual;
  public
    constructor Create(AErrors: TSepiCompilerErrorList; const ACode: string;
      const AFileName: TFileName = ''); override;
    destructor Destroy; override;

    function MakeBookmark: TSepiLexerBookmark; override;
    procedure ResetToBookmark(ABookmark: TSepiLexerBookmark;
      FreeBookmark: Boolean = True); override;

    property Code: string read FCode;
    property Cursor: Integer read FCursor;

    property CurrentPos: TSepiSourcePosition read FCurrentPos;
    property CurTerminal: TSepiTerminal read FCurTerminal;
  end;

  /// Classe de TSepiCustomLexerClass
  TSepiCustomLexerClass = class of TSepiCustomLexer;

  {*
    Classe de base pour les analyseurs lexicaux Sepi écrits à la main
    @author sjrd
    @version 1.0
  *}
  TSepiCustomManualLexer = class(TSepiBaseLexer)
  protected
    /// Tableau des fonctions d'analyse indexé par les caractères de début
    LexingProcs: array[#0..#255] of TSepiLexingProc;

    procedure DoNextTerminal; override;

    procedure InitLexingProcs; virtual;

    procedure ActionUnknown;
    procedure ActionEof;
    procedure ActionBlank;
  public
    constructor Create(AErrors: TSepiCompilerErrorList; const ACode: string;
      const AFileName: TFileName = ''); override;
  end;

const
  BlankChars = [#9, #10, #13, ' '];

implementation

{------------------------}
{ TSepiCustomLexer class }
{------------------------}

{*
  Crée un analyseur lexical
  @param AErrors     Erreurs de compilation
  @param ACode       Code source à analyser
  @param AFileName   Nom du fichier source
*}
constructor TSepiCustomLexer.Create(AErrors: TSepiCompilerErrorList;
  const ACode: string; const AFileName: TFileName = '');
begin
  inherited Create;

  FErrors := AErrors;
  FExcludedTokens := [tkBlank, tkComment];
end;

{*
  Modifie les types de lexèmes à exclure complètement
  @param Value   Nouvel ensemble de lexèmes à exclure (tkEof non pris en compte)
*}
procedure TSepiCustomLexer.SetExcludedTokens(const Value: TSysByteSet);
begin
  FExcludedTokens := Value;
  Exclude(FExcludedTokens, tkEof);
end;

{*
  Produit une erreur
  @param ErrorMsg   Message d'erreur
  @param Kind       Type d'erreur (défaut = Erreur)
*}
procedure TSepiCustomLexer.MakeError(const ErrorMsg: string;
  Kind: TSepiErrorKind = ekError);
begin
  Errors.MakeError(ErrorMsg, Kind, CurrentPos);
end;

{*
  Modifie le noeud contexte courant
  @param Value   Nouveau noeud contexte
*}
procedure TSepiCustomLexer.SetContext(Value: TSepiNonTerminal);
begin
  FContext := Value;
end;

{*
  Passe au lexème suivant
*}
procedure TSepiCustomLexer.NextTerminal;
begin
  repeat
    DoNextTerminal;
  until not (CurTerminal.SymbolClass in ExcludedTokens);
end;

{*
  Construit un marque-page à la position courante
  @return Le marque-page construit
*}
function TSepiCustomLexer.MakeBookmark: TSepiLexerBookmark;
begin
  Result := TSepiLexerBookmark.Create;
end;

{*
  Retourne dans le code source à la position d'un marque-page
  @param Bookmark       Marque-page
  @param FreeBookmark   Si True, le marque-page est ensuite détruit
*}
procedure TSepiCustomLexer.ResetToBookmark(Bookmark: TSepiLexerBookmark;
  FreeBookmark: Boolean = True);
begin
  if FreeBookmark then
    Bookmark.Free;
end;

{------------------------------}
{ TSepiBaseLexerBookmark class }
{------------------------------}

{*
  Crée un marque-page
  @param ACursor        Index courant dans le source
  @param ACurrentPos    Position courante
  @param ANextPos       Prochaine position
  @param ACurTerminal   Dernier terminal analysé
*}
constructor TSepiBaseLexerBookmark.Create(ACursor: Integer;
  const ACurrentPos, ANextPos: TSepiSourcePosition;
  ACurTerminal: TSepiTerminal);
begin
  inherited Create;

  FCursor := ACursor;
  FCurrentPos := ACurrentPos;
  FNextPos := ANextPos;

  FCurTerminal := TSepiTerminal.Clone(ACurTerminal);
end;

{*
  [@inheritDoc]
*}
destructor TSepiBaseLexerBookmark.Destroy;
begin
  FCurTerminal.Free;

  inherited;
end;

{----------------------}
{ TSepiBaseLexer class }
{----------------------}

{*
  Crée un analyseur lexical
  @param AErrors     Erreurs de compilation
  @param ACode       Code source à analyser
  @param AFileName   Nom du fichier source
*}
constructor TSepiBaseLexer.Create(AErrors: TSepiCompilerErrorList;
  const ACode: string; const AFileName: TFileName = '');
begin
  inherited;

  FCode := ACode + #0;
  FCursor := 1;

  if AFileName = '' then
    FNextPos.FileName := AErrors.CurrentFileName
  else
    FNextPos.FileName := AFileName;
  FNextPos.Line := 1;
  FNextPos.Col := 1;

  FCurrentPos := FNextPos;

  FCurTerminal := nil;
end;

{*
  [@inheritDoc]
*}
destructor TSepiBaseLexer.Destroy;
begin
  FreeAndNil(FCurTerminal);

  inherited;
end;

{*
  Indique qu'un terminal a été analysé
  @param SymbolClass      Class de symbole
  @param Representation   Représentation du terminal
*}
procedure TSepiBaseLexer.TerminalParsed(SymbolClass: TSepiSymbolClass;
  const Representation: string);
begin
  FreeAndNil(FCurTerminal);

  FCurTerminal := TSepiTerminal.Create(SymbolClass, CurrentPos,
    Representation);
  FCurrentPos := FNextPos;
end;

{*
  Avance le curseur
  @param Amount   Nombre de caractères à passer (défaut = 1)
*}
procedure TSepiBaseLexer.CursorForward(Amount: Integer = 1);
var
  I: Integer;
begin
  Assert(Amount >= 0);

  for I := FCursor to FCursor+Amount-1 do
  begin
    if (I > 0) and (Code[I] = #10) and (Code[I-1] = #13) then
      // skip
    else if (Code[I] = #13) or (Code[I] = #10) then
    begin
      Inc(FNextPos.Line);
      FNextPos.Col := 1;
    end else
      Inc(FNextPos.Col);
  end;

  Inc(FCursor, Amount);
end;

{*
  Identifie un mot-clef
  @param Key           Mot-clef éventuel à identifier
  @param SymbolClass   À modifier selon la classe du mot-clef
*}
procedure TSepiBaseLexer.IdentifyKeyword(const Key: string;
  var SymbolClass: TSepiSymbolClass);
begin
end;

{*
  [@inheritDoc]
*}
function TSepiBaseLexer.GetCurrentPos: TSepiSourcePosition;
begin
  Result := FCurrentPos;
end;

{*
  [@inheritDoc]
*}
function TSepiBaseLexer.GetCurTerminal: TSepiTerminal;
begin
  Result := FCurTerminal;
end;

{*
  [@inheritDoc]
*}
function TSepiBaseLexer.MakeBookmark: TSepiLexerBookmark;
begin
  Result := TSepiBaseLexerBookmark.Create(FCursor, FCurrentPos, FNextPos,
    FCurTerminal);
end;

{*
  [@inheritDoc]
*}
procedure TSepiBaseLexer.ResetToBookmark(ABookmark: TSepiLexerBookmark;
  FreeBookmark: Boolean = True);
var
  Bookmark: TSepiBaseLexerBookmark;
begin
  Bookmark := ABookmark as TSepiBaseLexerBookmark;

  FCursor := Bookmark.Cursor;
  FCurrentPos := Bookmark.CurrentPos;
  FNextPos := Bookmark.NextPos;

  FreeAndNil(FCurTerminal);
  FCurTerminal := TSepiTerminal.Clone(Bookmark.CurTerminal);

  inherited;
end;

{------------------------------}
{ TSepiCustomManualLexer class }
{------------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiCustomManualLexer.Create(AErrors: TSepiCompilerErrorList;
  const ACode: string; const AFileName: TFileName = '');
begin
  inherited;

  InitLexingProcs;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCustomManualLexer.DoNextTerminal;
begin
  LexingProcs[Code[Cursor]];
end;

{*
  Initialise le tableau LexingProcs
*}
procedure TSepiCustomManualLexer.InitLexingProcs;
var
  C: Char;
begin
  for C := #0 to #255 do
  begin
    if C = #0 then
      LexingProcs[C] := ActionEof
    else if C in BlankChars then
      LexingProcs[C] := ActionBlank
    else
      LexingProcs[C] := ActionUnknown;
  end;
end;

{*
  Action pour un caractère inconnu - déclenche une erreur lexicale
  @return Ne retourne jamais
  @raise ESepiLexicalError
*}
procedure TSepiCustomManualLexer.ActionUnknown;
begin
  MakeError(Format(SBadSourceCharacter, [Code[Cursor]]), ekFatalError);
end;

{*
  Analise un caractère de fin de fichier
  @return True - la fin de fichier est bien réelle
*}
procedure TSepiCustomManualLexer.ActionEof;
begin
  TerminalParsed(tkEof, SEndOfFile);
end;

{*
  Analise un blanc
  @return False - les blancs ne sont pas de véritables lexèmes
*}
procedure TSepiCustomManualLexer.ActionBlank;
begin
  while Code[Cursor] in BlankChars do
    CursorForward;

  TerminalParsed(tkBlank, ' ');
end;

end.

