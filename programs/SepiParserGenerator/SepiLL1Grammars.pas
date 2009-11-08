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

unit SepiLL1Grammars;

interface

uses
  SysUtils, Classes, StrUtils, Contnrs, Math, ScStrUtils, ScIntegerSets;

type
  {*
    Classe de symbole
  *}
  TSymbolClass = type Word;

  TGramSymbol = class;
  TTerminal = class;
  TNonTerminal = class;
  TChoiceEnd = class;
  TChoice = class;
  TGrammar = class;

  {*
    Type de non-terminal priv
  *}
  TPrivNonTerminalKind = (ntkZeroMany, ntkOneMany, ntkZeroOne);

  {*
    Ensemble des PREM
    @author sjrd
    @version 1.0
  *}
  TPremSet = class(TScIntegerSet)
  private
    FEmpty: Boolean;

    procedure SetEmpty(Value: Boolean);
  public
    function AddPrem(Source: TPremSet; WithEmpty: Boolean = True): Boolean;

    property Empty: Boolean read FEmpty write SetEmpty;
  end;

  {*
    Ensemble des SUIV
    @author sjrd
    @version 1.0
  *}
  TSuivSet = class(TScIntegerSet)
  public
    function AddPrem(Source: TPremSet): Boolean;
    function AddSuiv(Source: TSuivSet): Boolean;
  end;

  {*
    Ensemble Choisir-Si
    @author sjrd
    @version 1.0
  *}
  TChooseIfSet = class(TScIntegerSet)
  public
    function ConflictWith(Other: TChooseIfSet): Boolean;
  end;

  {*
    Conflit LL(1)
    @author sjrd
    @version 1.0
  *}
  TLL1Conflict = class
  private
    FNonTerminal: TNonTerminal;
    FChoice1: Integer;
    FChoice2: Integer;
  public
    constructor Create(ANonTerminal: TNonTerminal;
      AChoice1, AChoice2: Integer);
    property NonTerminal: TNonTerminal read FNonTerminal;
    property Choice1: Integer read FChoice1;
    property Choice2: Integer read FChoice2;
  end;

  {*
    Table de transition
    @author sjrd
    @version 1.0
  *}
  TParsingTable = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FTable: array of array of TChoice;

    function GetTable(NonTerminal, Terminal: TSymbolClass): TChoice;
  public
    constructor Create(Grammar: TGrammar);
    destructor Destroy; override;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Table[NonTerminal, Terminal: TSymbolClass]: TChoice
      read GetTable; default;
  end;

  {*
    Symbole grammatical
    @author sjrd
    @version 1.0
  *}
  TGramSymbol = class
  private
    FID: TSymbolClass;
    FStrID: string;
    FName: string;
    FPremSet: TPremSet;
  public
    constructor Create(AID: TSymbolClass; Str: string);
    destructor Destroy; override;

    property ID: TSymbolClass read FID;
    property StrID: string read FStrID;
    property Name: string read FName;
    property PremSet: TPremSet read FPremSet;
  end;

  {*
    Terminal
    @author sjrd
    @version 1.0
  *}
  TTerminal = class(TGramSymbol)
  public
    constructor Create(AID: TSymbolClass; Str: string);
  end;

  {*
    Non-terminal
    @author sjrd
    @version 1.0
  *}
  TNonTerminal = class(TGramSymbol)
  private
    FSimplify: Boolean;
    FShowAsText: Boolean;
    FSuccessiveTries: Boolean;
    FSuivSet: TSuivSet;
    FChoices: TObjectList;

    procedure AddChoice(Choice: TChoice);
    procedure Complete;

    function GetChoiceCount: Integer;
    function GetChoices(Index: Integer): TChoice;
  public
    constructor Create(AID: TSymbolClass; Str: string);
    destructor Destroy; override;

    property Simplify: Boolean read FSimplify;
    property ShowAsText: Boolean read FShowAsText;
    property SuccessiveTries: Boolean read FSuccessiveTries;
    property SuivSet: TSuivSet read FSuivSet;
    property ChoiceCount: Integer read GetChoiceCount;
    property Choices[Index: Integer]: TChoice read GetChoices;
  end;

  {*
    Fin de choix
    @author sjrd
    @version 1.0
  *}
  TChoiceEnd = class
  private
    FEmpty: Boolean;
    FItems: TObjectList;
    FPremSet: TPremSet;
    FChoiceEnd: TChoiceEnd;

    function GetItemCount: Integer;
    function GetItems(Index: Integer): TGramSymbol;
  public
    constructor Create;
    constructor CreateAsPartOf(ChoiceEnd: TChoiceEnd);
    destructor Destroy; override;

    function Equals(Right: TChoiceEnd): Boolean;

    property Empty: Boolean read FEmpty;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TGramSymbol read GetItems;
    property First: TGramSymbol index 0 read GetItems;
    property PremSet: TPremSet read FPremSet;
    property ChoiceEnd: TChoiceEnd read FChoiceEnd;
  end;

  {*
    Choix
    @author sjrd
    @version 1.0
  *}
  TChoice = class(TChoiceEnd)
  private
    FItemsToRemove: array of Boolean;
    FNonTerminals: TObjectList;
    FChooseIf: array of TChooseIfSet;
    FID: Integer;
    FPriority: Integer;
    FIsTry: Boolean;
    FAlternativeChoice: TChoice;

    procedure AddNonTerminal(NT: TNonTerminal);
    procedure SetAlternativeChoice(AltChoice: TChoice);

    function GetItemsToRemove(Index: Integer): Boolean;
    function GetNonTerminalCount: Integer;
    function GetNonTerminals(Index: Integer): TNonTerminal;
    function GetChooseIf(NT: TNonTerminal): TChooseIfSet;
  public
    constructor Create;
    constructor CreateEmpty;
    constructor CreateFromDef(AID: Integer; Str: string;
      Grammar: TGrammar);
    destructor Destroy; override;

    property ItemsToRemove[Index: Integer]: Boolean
      read GetItemsToRemove;
    property NonTerminalCount: Integer read GetNonTerminalCount;
    property NonTerminals[Index: Integer]: TNonTerminal read GetNonTerminals;
    property ChooseIf[NT: TNonTerminal]: TChooseIfSet read GetChooseIf;
    property ID: Integer read FID;
    property Priority: Integer read FPriority;
    property IsTry: Boolean read FIsTry;
    property AlternativeChoice: TChoice read FAlternativeChoice;
  end;

  {*
    Grammaire
    @author sjrd
    @version 1.0
  *}
  TGrammar = class
  private
    FTerminals: TObjectList;
    FFirstNonTerminalID: Integer;
    FNonTerminals: TObjectList;
    FChoiceEnds: TObjectList;
    FChoiceCount: Integer;
    FLL1Conflicts: TObjectList;

    function ChoiceEndExists(var ChoiceEnd: TChoiceEnd): Boolean;

    function MakePrivNonTerminal(Strings: TStrings; No: Integer;
      Kind: TPrivNonTerminalKind; const Contents: string): string;
    procedure LoadNonTerminals(Strings: TStrings);
    procedure LoadChoiceEnds(Strings: TStrings);
    procedure ComputePremSets;
    procedure ComputeSuivSets;
    procedure ComputeChooseIf;
    procedure CheckForConflicts;

    function GetTerminalCount: Integer;
    function GetTerminals(Index: Integer): TTerminal;
    function GetNonTerminalCount: Integer;
    function GetNonTerminals(Index: Integer): TNonTerminal;
    function GetChoiceEndCount: Integer;
    function GetChoiceEnds(Index: Integer): TChoiceEnd;
    function GetChoices(Index: Integer): TChoice;
    function GetLL1ConflictCount: Integer;
    function GetLL1Conflicts(Index: Integer): TLL1Conflict;
  public
    constructor Create(FileName: TFileName);
    destructor Destroy; override;

    function FindSymbol(Str: string): TGramSymbol; overload;
    function FindSymbol(ID: Integer): TGramSymbol; overload;

    property TerminalCount: Integer read GetTerminalCount;
    property Terminals[Index: Integer]: TTerminal read GetTerminals;
    property FirstNonTerminalID: Integer read FFirstNonTerminalID;
    property NonTerminalCount: Integer read GetNonTerminalCount;
    property NonTerminals[Index: Integer]: TNonTerminal read GetNonTerminals;
    property ChoiceEndCount: Integer read GetChoiceEndCount;
    property ChoiceEnds[Index: Integer]: TChoiceEnd read GetChoiceEnds;
    property ChoiceCount: Integer read FChoiceCount;
    property Choices[Index: Integer]: TChoice read GetChoices;
    property EmptyChoice: TChoice index 0 read GetChoices;
    property LL1ConflictCount: Integer read GetLL1ConflictCount;
    property LL1Conflicts[Index: Integer]: TLL1Conflict read GetLL1Conflicts;
  end;

implementation

{----------------}
{ TPremSet class }
{----------------}

{*
  Modifie la propriété Empty
  @param Value   Nouvelle valeur de la propriété Empty
*}
procedure TPremSet.SetEmpty(Value: Boolean);
begin
  if Value <> FEmpty then
  begin
    FEmpty := Value;
    Modified := True;
  end;
end;

{*
  Adds the contents of another PREM set to this PREM set
  @param Source      Source PREM set
  @param WithEmpty   True for including the empty flag, False otherwise
  @return True if the set has changed, False otherwise
*}
function TPremSet.AddPrem(Source: TPremSet; WithEmpty: Boolean = True): Boolean;
begin
  Modified := False;

  Union(Source);
  if WithEmpty and Source.Empty then
    Empty := True;

  Result := Modified;
end;

{----------------}
{ TSuitSet class }
{----------------}

{*
  Adds the contents of a PREM set to this SUIV set
  @param Source   Source SUIV set
  @return True if the set has changed, False otherwise
*}
function TSuivSet.AddPrem(Source: TPremSet): Boolean;
begin
  Modified := False;

  Union(Source);

  Result := Modified;
end;

{*
  Adds the contents of another SUIV set to this SUIV set
  @param Source   Source SUIV set
  @return True if the set has changed, False otherwise
*}
function TSuivSet.AddSuiv(Source: TSuivSet): Boolean;
begin
  Modified := False;

  Union(Source);

  Result := Modified;
end;

{--------------------}
{ TChooseIfSet class }
{--------------------}

{*
  Test whether this set conflicts with another one
  @param Other   Other Choose-If set to compare with
  @return True if sets conflict with each other, False otherwise
*}
function TChooseIfSet.ConflictWith(Other: TChooseIfSet): Boolean;
var
  Temp: TChooseIfSet;
begin
  Temp := TChooseIfSet.Clone(Self);
  try
    Temp.Subtract(Other);
    Result := Temp.Modified;
  finally
    Temp.Free;
  end;
end;

{--------------------}
{ TLL1Conflict class }
{--------------------}

constructor TLL1Conflict.Create(ANonTerminal: TNonTerminal;
  AChoice1, AChoice2: Integer);
begin
  inherited Create;
  FNonTerminal := ANonTerminal;
  FChoice1 := AChoice1;
  FChoice2 := AChoice2;
end;

{---------------------}
{ TParsingTable class }
{---------------------}

constructor TParsingTable.Create(Grammar: TGrammar);
var
  I, J: Integer;
  NonTerminal: TNonTerminal;
  Choice: TChoice;
  ChooseIf: TScIntegerSet;
  Enumerator: TScIntegerSetEnumerator;
begin
  inherited Create;
  FWidth := Grammar.NonTerminalCount;
  FHeight := Grammar.TerminalCount;

  SetLength(FTable, Width);
  for I := 0 to Width-1 do
  begin
    SetLength(FTable[I], Height);
    for J := 0 to Height-1 do
      FTable[I][J] := nil;
  end;

  for I := 0 to Grammar.NonTerminalCount-1 do
  begin
    NonTerminal := Grammar.NonTerminals[I];
    for J := 0 to NonTerminal.ChoiceCount-1 do
    begin
      Choice := NonTerminal.Choices[J];
      ChooseIf := Choice.ChooseIf[NonTerminal];

      Enumerator := ChooseIf.GetEnumerator;
      try
        while Enumerator.MoveNext do
          if FTable[I][Enumerator.Current] = nil then
            FTable[I][Enumerator.Current] := Choice;
      finally
        Enumerator.Free;
      end;
    end;
  end;
end;

destructor TParsingTable.Destroy;
var
  I: Integer;
begin
  for I := 0 to Width-1 do
    SetLength(FTable[I], 0);
  SetLength(FTable, 0);
  inherited Destroy;
end;

function TParsingTable.GetTable(
  NonTerminal, Terminal: TSymbolClass): TChoice;
begin
  Result := FTable[NonTerminal-Height, Terminal];
end;

{-------------------}
{ TGramSymbol class }
{-------------------}

constructor TGramSymbol.Create(AID: TSymbolClass; Str: string);
begin
  inherited Create;

  FID := AID;
  if SplitToken(Str, #9, FStrID, FName) then
    FName := GetFirstToken(FName, #9);

  FPremSet := TPremSet.Create;
end;

{*
  [@inheritDoc]
*}
destructor TGramSymbol.Destroy;
begin
  FPremSet.Free;

  inherited;
end;

{-----------------}
{ TTerminal class }
{-----------------}

constructor TTerminal.Create(AID: TSymbolClass; Str: string);
begin
  inherited;

  PremSet.Include(ID);
end;

{--------------------}
{ TNonTerminal class }
{--------------------}

constructor TNonTerminal.Create(AID: TSymbolClass; Str: string);
begin
  inherited;
  FSimplify := GetXToken(Str, #9, 3) = 'Simplify';
  FShowAsText := GetXToken(Str, #9, 3) = 'AsText';
  FSuccessiveTries := GetXToken(Str, #9, 3) = 'SuccessiveTries';
  FSuivSet := TSuivSet.Create;
  FChoices := TObjectList.Create(False);
end;

destructor TNonTerminal.Destroy;
begin
  FChoices.Free;
  FSuivSet.Free;
  inherited;
end;

procedure TNonTerminal.AddChoice(Choice: TChoice);
begin
  FChoices.Add(Choice);
  Choice.AddNonTerminal(Self);
end;

procedure TNonTerminal.Complete;
var
  I: Integer;
begin
  if not SuccessiveTries then
    Exit;

  for I := 0 to ChoiceCount-2 do
    Choices[I].SetAlternativeChoice(Choices[I+1]);
end;

function TNonTerminal.GetChoiceCount: Integer;
begin
  Result := FChoices.Count;
end;

function TNonTerminal.GetChoices(Index: Integer): TChoice;
begin
  Result := TChoice(FChoices[Index]);
end;

{------------------}
{ TChoiceEnd class }
{------------------}

constructor TChoiceEnd.Create;
begin
  inherited;
  FEmpty := False;
  FItems := TObjectList.Create(False);
  FPremSet := TPremSet.Create;
  FChoiceEnd := nil;
end;

constructor TChoiceEnd.CreateAsPartOf(ChoiceEnd: TChoiceEnd);
var
  I: Integer;
begin
  Create;
  for I := 1 to ChoiceEnd.ItemCount-1 do
    FItems.Add(ChoiceEnd.Items[I]);
end;

destructor TChoiceEnd.Destroy;
begin
  FPremSet.Free;
  FItems.Free;
  inherited;
end;

function TChoiceEnd.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TChoiceEnd.GetItems(Index: Integer): TGramSymbol;
begin
  Result := TGramSymbol(FItems[Index]);
end;

function TChoiceEnd.Equals(Right: TChoiceEnd): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ItemCount <> Right.ItemCount then
    Exit;
  for I := 0 to ItemCount-1 do
    if Items[I] <> Right.Items[I] then
      Exit;
  Result := True;
end;

{---------------}
{ TChoice class }
{---------------}

constructor TChoice.Create;
begin
  inherited;

  FNonTerminals := TObjectList.Create(False);
  FID := 0;
  FPriority := 0;
end;

constructor TChoice.CreateEmpty;
begin
  Create;
  FEmpty := True;
  PremSet.Empty := True;
end;

constructor TChoice.CreateFromDef(AID: Integer; Str: string;
  Grammar: TGrammar);
var
  I, J: Integer;
  StrSymb: string;
  ASupprimer: Boolean;
begin
  Create;
  Str := Trim(Str)+' ';
  I := 1;

  if TryStrToInt(GetFirstToken(Str, ' '), FPriority) then
    Inc(I, Pos(' ', Str));

  while I <= Length(Str) do
  begin
    J := PosEx(' ', Str, I);
    StrSymb := Copy(Str, I, J-I);

    ASupprimer := StrSymb[Length(StrSymb)] = '*';
    if ASupprimer then
      SetLength(StrSymb, Length(StrSymb)-1);

    FItems.Add(Grammar.FindSymbol(StrSymb));

    SetLength(FItemsToRemove, FItems.Count);
    FItemsToRemove[Length(FItemsToRemove)-1] := ASupprimer;

    if ASupprimer and (FItems.Last is TNonTerminal) and
      TNonTerminal(FItems.Last).Simplify then
    begin
      raise Exception.CreateFmt(
        'Ne peut supprimer le non-terminal %s qui doit se simplifier',
        [TNonTerminal(FItems.Last).StrID]);
    end;

    I := J+1;
  end;

  FID := AID;
end;

destructor TChoice.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FChooseIf)-1 do
    FChooseIf[I].Free;

  FNonTerminals.Free;
  inherited;
end;

procedure TChoice.AddNonTerminal(NT: TNonTerminal);
begin
  FNonTerminals.Add(NT);
  SetLength(FChooseIf, NonTerminalCount);
  FChooseIf[NonTerminalCount-1] := TChooseIfSet.Create;
end;

procedure TChoice.SetAlternativeChoice(AltChoice: TChoice);
begin
  if Empty then
    raise Exception.Create('Un choix vide ne peut être un try');

  FIsTry := True;
  FAlternativeChoice := AltChoice
end;

function TChoice.GetItemsToRemove(Index: Integer): Boolean;
begin
  Result := FItemsToRemove[Index];
end;

function TChoice.GetNonTerminalCount: Integer;
begin
  Result := FNonTerminals.Count;
end;

function TChoice.GetNonTerminals(Index: Integer): TNonTerminal;
begin
  Result := TNonTerminal(FNonTerminals[Index]);
end;

function TChoice.GetChooseIf(NT: TNonTerminal): TChooseIfSet;
begin
  Result := FChooseIf[FNonTerminals.IndexOf(NT)];
end;

{----------------}
{ TGrammar class }
{----------------}

constructor TGrammar.Create(FileName: TFileName);
var
  Fichier: TStrings;
  I: Integer;
begin
  inherited Create;
  FTerminals := TObjectList.Create;
  FFirstNonTerminalID := 0;
  FNonTerminals := TObjectList.Create;
  FChoiceEnds := TObjectList.Create;
  FChoiceCount := 0;
  FLL1Conflicts := TObjectList.Create;

  Fichier := TStringList.Create;
  try
    Fichier.LoadFromFile(FileName);

    // On ignore le texte avant le premier #####
    I := Fichier.IndexOf('#####')+1;

    // Création des Terminals
    while Fichier[I] <> '#####' do
    begin
      if Fichier[I] <> '' then
        FTerminals.Add(TTerminal.Create(TerminalCount, Fichier[I]));
      Inc(I);
    end;

    // Suppression des infos devenues inutiles
    while I >= 0 do
    begin
      Fichier.Delete(I);
      Dec(I);
    end;
    I := 0;
    while I < Fichier.Count do
      if Fichier[I] = '' then
        Fichier.Delete(I)
      else
        Inc(I);

    // Préparation de l'ID du premier non-terminal
    FFirstNonTerminalID := TerminalCount;

    // Chargement des non-Terminals et des fins de Choices
    LoadNonTerminals(Fichier);
    LoadChoiceEnds(Fichier);

    // Compléter les non-Terminals
    for I := 0 to NonTerminalCount-1 do
      NonTerminals[I].Complete;

    // Calcul des PremSet, SuivSet et ChooseIf
    ComputePremSets;
    ComputeSuivSets;
    ComputeChooseIf;
    CheckForConflicts;
  finally
    Fichier.Free;
  end;
end;

destructor TGrammar.Destroy;
begin
  FLL1Conflicts.Free;
  FChoiceEnds.Free;
  FNonTerminals.Free;
  FTerminals.Free;
  inherited Destroy;
end;

function TGrammar.ChoiceEndExists(var ChoiceEnd: TChoiceEnd): Boolean;
var
  I: Integer;
begin
  for I := 0 to ChoiceEndCount-1 do
  begin
    if ChoiceEnds[I].Equals(ChoiceEnd) then
    begin
      ChoiceEnd.Free;
      ChoiceEnd := ChoiceEnds[I];
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function PosExEnd(const SubStr, Str: string; Offset: Integer = 1): Integer;
begin
  Result := PosEx(SubStr, Str, Offset);
  if Result = 0 then
    Result := Length(Str)+1;
end;

function MinPosExEnd(const SubStr: array of string; const Str: string;
  Offset: Integer = 1): Integer;
var
  I: Integer;
begin
  Result := Length(Str)+1;
  for I := Low(SubStr) to High(SubStr) do
    Result := Min(Result, PosExEnd(SubStr[I], Str, Offset));
end;

function TGrammar.MakePrivNonTerminal(Strings: TStrings; No: Integer;
  Kind: TPrivNonTerminalKind; const Contents: string): string;
var
  Name: string;
begin
  Name := 'Priv'+IntToStr(No);

  if Kind = ntkOneMany then
    Result := Contents + ' ' + Name
  else
    Result := Name;

  Strings.Add(Format('nt%s'#9'%0:s'#9'Simplify', [Name]));
  if Kind = ntkZeroOne then
    Strings.Add(#9 + Contents)
  else
    Strings.Add(#9 + Contents + ' ' + Name);
  Strings.Add(#9'@');
end;

procedure TGrammar.LoadNonTerminals(Strings: TStrings);
var
  I, BeginPos, EndPos, Len, PrivCount: Integer;
  Line: string;
  PrivKind: TPrivNonTerminalKind;
begin
  I := 0;
  PrivCount := 0;
  while I < Strings.Count do
  begin
    Line := Strings[I];

    if Line[1] <> #9 then
    begin
      // Non-terminal
      Strings.Objects[I] := TNonTerminal.Create(
        FirstNonTerminalID+NonTerminalCount, Line);
      FNonTerminals.Add(Strings.Objects[I]);
    end else
    begin
      // Choice - map EBNF things to new simplifying non-terminals
      Line[1] := ' ';
      Line := Line + ' ';

      while True do
      begin
        BeginPos := Pos(' (', Line);
        if BeginPos = 0 then
          Break;
        EndPos := MinPosExEnd([')* ', ')+ ', ')? '], Line, BeginPos);
        if EndPos > Length(Line) then
          Break;

        Inc(BeginPos);
        Inc(EndPos);
        Len := EndPos-BeginPos+1;

        case Line[EndPos] of
          '*': PrivKind := ntkZeroMany;
          '+': PrivKind := ntkOneMany;
        else
          PrivKind := ntkZeroOne;
        end;

        Line := StuffString(Line, BeginPos, Len,
          MakePrivNonTerminal(Strings, PrivCount, PrivKind,
          Copy(Line, BeginPos+1, Len-3)));
        Inc(PrivCount);
      end;

      Strings[I] := #9 + Copy(Line, 2, Length(Line)-2);
    end;

    Inc(I);
  end;
end;

procedure TGrammar.LoadChoiceEnds(Strings: TStrings);
var
  I: Integer;
  NonTerminal: TNonTerminal;
  ChoiceEnd: TChoiceEnd;
begin
  // ChoiceEnd de Choices vide
  FChoiceEnds.Add(TChoice.CreateEmpty);

  // Créer tous les Choices des non-Terminals
  NonTerminal := nil;
  for I := 0 to Strings.Count-1 do
  begin
    if Strings.Objects[I] <> nil then
      NonTerminal := TNonTerminal(Strings.Objects[I])
    else
    begin
      if Trim(Strings[I]) = '@' then
      begin
        NonTerminal.AddChoice(EmptyChoice);
      end else
      begin
        ChoiceEnd := TChoice.CreateFromDef(ChoiceEndCount, Strings[I], Self);
        FChoiceEnds.Add(ChoiceEnd);
        NonTerminal.AddChoice(TChoice(ChoiceEnd));
      end;
    end;
  end;
  FChoiceCount := ChoiceEndCount;

  // Créer toutes les fins de Choices
  I := 1;
  while I < ChoiceEndCount do
  begin
    ChoiceEnd := TChoiceEnd.CreateAsPartOf(ChoiceEnds[I]);
    if not ChoiceEndExists(ChoiceEnd) then
      FChoiceEnds.Add(ChoiceEnd);
    ChoiceEnds[I].FChoiceEnd := ChoiceEnd;
    Inc(I);
  end;
end;

// Algorithme de fermeture de calcul des PremSet
procedure TGrammar.ComputePremSets;
var
  Change: Boolean;
  I, J: Integer;
  NonTerminal: TNonTerminal;
  ChoiceEnd: TChoiceEnd;
begin
  repeat
    Change := False;

    // Règle 1
    for I := 0 to NonTerminalCount-1 do
    begin
      NonTerminal := NonTerminals[I];
      for J := 0 to NonTerminal.ChoiceCount-1 do
      begin
        if NonTerminal.PremSet.AddPrem(NonTerminal.Choices[J].PremSet) then
          Change := True;
      end;
    end;

    for I := 1 to ChoiceEndCount-1 do
    begin
      ChoiceEnd := ChoiceEnds[I];

      // Règle 2
      if ChoiceEnd.PremSet.AddPrem(ChoiceEnd.First.PremSet, False) then
        Change := True;

      // Règle 3
      if ChoiceEnd.First.PremSet.Empty then
        if ChoiceEnd.PremSet.AddPrem(ChoiceEnd.ChoiceEnd.PremSet) then
          Change := True;
    end;
  until not Change;
end;

// Algorithme de fermeture de calcul des SuivSet
procedure TGrammar.ComputeSuivSets;
var
  Change: Boolean;
  I, J: Integer;
  ChoiceEnd: TChoiceEnd;
  NonTerminal, Premier: TNonTerminal;
begin
  repeat
    Change := False;

    for I := 0 to NonTerminalCount-1 do
    begin
      NonTerminal := NonTerminals[I];
      for J := 0 to NonTerminal.ChoiceCount-1 do
      begin
        ChoiceEnd := NonTerminal.Choices[J];
        while not ChoiceEnd.Empty do
        begin
          if ChoiceEnd.First is TNonTerminal then
          begin
            Premier := TNonTerminal(ChoiceEnd.First);

            // Règle 1
            if Premier.SuivSet.AddPrem(ChoiceEnd.ChoiceEnd.PremSet) then
              Change := True;

            // Règle 2
            if ChoiceEnd.ChoiceEnd.PremSet.Empty then
              if Premier.SuivSet.AddSuiv(NonTerminal.SuivSet) then
                Change := True;
          end;
          ChoiceEnd := ChoiceEnd.ChoiceEnd;
        end;
      end;
    end;
  until not Change;
end;

procedure TGrammar.ComputeChooseIf;
var
  I, J: Integer;
begin
  for I := 0 to ChoiceCount-1 do
  begin
    with Choices[I] do
    begin
      for J := 0 to NonTerminalCount-1 do
      begin
        FChooseIf[J].Assign(PremSet);
        if PremSet.Empty then
          FChooseIf[J].Union(NonTerminals[J].SuivSet);
      end;
    end;
  end;
end;

procedure TGrammar.CheckForConflicts;
var
  I, J, K: Integer;
  NonTerminal: TNonTerminal;
  JChooseIf, KChooseIf: TChooseIfSet;
begin
  for I := 0 to NonTerminalCount-1 do
  begin
    NonTerminal := NonTerminals[I];
    if NonTerminal.SuccessiveTries then
      Continue;

    with NonTerminal do
    begin
      for J := 0 to ChoiceCount-2 do
      begin
        for K := J+1 to ChoiceCount-1 do
        begin
          JChooseIf := Choices[J].ChooseIf[NonTerminal];
          KChooseIf := Choices[K].ChooseIf[NonTerminal];

          if JChooseIf.ConflictWith(KChooseIf) then
          begin
            if Choices[J].Priority > Choices[K].Priority then
              KChooseIf.Subtract(JChooseIf)
            else if Choices[K].Priority > Choices[J].Priority then
              JChooseIf.Subtract(KChooseIf)
            else
              FLL1Conflicts.Add(TLL1Conflict.Create(NonTerminal, J, K));
          end;
        end;
      end;
    end;
  end;
end;

function TGrammar.GetTerminalCount: Integer;
begin
  Result := FTerminals.Count;
end;

function TGrammar.GetTerminals(Index: Integer): TTerminal;
begin
  Result := TTerminal(FTerminals[Index]);
end;

function TGrammar.GetNonTerminalCount: Integer;
begin
  Result := FNonTerminals.Count;
end;

function TGrammar.GetNonTerminals(Index: Integer): TNonTerminal;
begin
  Result := TNonTerminal(FNonTerminals[Index]);
end;

function TGrammar.GetChoiceEndCount: Integer;
begin
  Result := FChoiceEnds.Count;
end;

function TGrammar.GetChoiceEnds(Index: Integer): TChoiceEnd;
begin
  Result := TChoiceEnd(FChoiceEnds[Index]);
end;

function TGrammar.GetChoices(Index: Integer): TChoice;
begin
  Result := TChoice(FChoiceEnds[Index]);
end;

function TGrammar.GetLL1ConflictCount: Integer;
begin
  Result := FLL1Conflicts.Count;
end;

function TGrammar.GetLL1Conflicts(Index: Integer): TLL1Conflict;
begin
  Result := TLL1Conflict(FLL1Conflicts[Index]);
end;

function TGrammar.FindSymbol(Str: string): TGramSymbol;
var
  I: Integer;
begin
  if Str[1] = '''' then
  begin
    Str := Copy(Str, 2, Length(Str)-2);
    for I := 0 to TerminalCount-1 do
    begin
      if Terminals[I].Name = Str then
      begin
        Result := Terminals[I];
        Exit;
      end;
    end;

    raise Exception.CreateFmt(
      'Impossible de trouver le symbole ''%s''', [Str]);
  end else
  begin
    for I := 0 to TerminalCount-1 do
    begin
      if Terminals[I].StrID = Str then
      begin
        Result := Terminals[I];
        Exit;
      end;
    end;

    for I := 0 to NonTerminalCount-1 do
    begin
      if NonTerminals[I].Name = Str then
      begin
        Result := NonTerminals[I];
        Exit;
      end;
    end;
    
    raise Exception.CreateFmt('Impossible de trouver le symbole %s', [Str]);
  end;
end;

function TGrammar.FindSymbol(ID: Integer): TGramSymbol;
begin
  if ID >= FirstNonTerminalID then
    Result := NonTerminals[ID-FirstNonTerminalID]
  else
    Result := Terminals[ID];
end;

end.

