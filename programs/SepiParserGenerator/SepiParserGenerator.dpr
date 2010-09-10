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
  Générateur de compilateur pour Sepi
  @author sjrd
  @version 1.0
*}
program SepiParserGenerator;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  ScUtils,
  ScIntegerSets,
  ScConsoleUtils,
  ScTypInfo,
  SepiCompilerErrors,
  GeneratorOptions in 'GeneratorOptions.pas',
  GeneratorUtils in 'GeneratorUtils.pas',
  SepiLL1Grammars in 'SepiLL1Grammars.pas';

resourcestring
  SSourceFileDoesntExist = 'Le fichier source %s n''existe pas';
  SSepiInternalError = 'Erreur interne : %s';
  SLL1ConflictOnNonTerminal =
    'Conflit LL(1) sur le non-terminal %s entre ses choix %d et %d';

{-----------------}
{ Report routines }
{-----------------}

procedure MakeTerminalList(Grammar: TGrammar; Report: TStrings);
var
  I: Integer;
begin
  Report.Add('LISTE DES TERMINAUX');
  Report.Add('');
  for I := 0 to Grammar.TerminalCount-1 do
    with Grammar.Terminals[I] do
      Report.Add(Format('%s %s', [StrID, Name]));
  Report.Add('');
end;

procedure MakeNonTerminalList(Grammar: TGrammar; Report: TStrings);
var
  I, J, K: Integer;
  Str: string;
begin
  Report.Add('LISTE DES NON-TERMINAUX');
  Report.Add('');

  for I := 0 to Grammar.NonTerminalCount-1 do
  begin
    with Grammar.NonTerminals[I] do
    begin
      Report.Add(Format('%s %s', [StrID, Name]));
      for J := 0 to ChoiceCount-1 do
      begin
        with Choices[J] do
        begin
          if Empty then
            Report.Add('    @')
          else
          begin
            Str := '   ';
            for K := 0 to ItemCount-1 do
              with Items[K] do
                Str := Str + ' ' + StrID;
            Report.Add(Str);
          end;
        end;
      end;
    end;
  end;
  Report.Add('');
end;

procedure MakeChoiceEndList(Grammar: TGrammar; Report: TStrings);
var
  I, J: Integer;
  Str: string;
begin
  Report.Add('LISTE DES FINS DE CHOIX');
  Report.Add('');
  for I := 0 to Grammar.ChoiceEndCount-1 do
  begin
    with Grammar.ChoiceEnds[I] do
    begin
      if Empty then
        Report.Add('@')
      else
      begin
        Str := '';
        for J := 0 to ItemCount-1 do
          with Items[J] do
            Str := Str + ' ' + StrID;
        Delete(Str, 1, 1);
        Report.Add(Str);
      end;
    end;
  end;
  Report.Add('');
end;

procedure MakePremSetList(Grammar: TGrammar; Report: TStrings);
var
  I, J: Integer;
  Str: string;
  Enumerator: TScIntegerSetEnumerator;
begin
  Report.Add('ENSEMBLES DES PREM DES NON-TERMINAUX ET DES CHOIX');
  Report.Add('');
  for I := 0 to Grammar.NonTerminalCount-1 do
  begin
    with Grammar.NonTerminals[I] do
    begin
      Str := Format('PREM(%s) =', [StrID]);
      Enumerator := PremSet.GetEnumerator;
      try
        while Enumerator.MoveNext do
          Str := Str + ' ' + Grammar.FindSymbol(Enumerator.Current).StrID;
      finally
        Enumerator.Free;
      end;
      if PremSet.Empty then
        Str := Str + ' @';
      Report.Add(Str);

      for J := 0 to ChoiceCount-1 do
      begin
        with Choices[J] do
        begin
          Str := Format('    PREM(%s.%d) =', [StrID, J+1]);
          Enumerator := PremSet.GetEnumerator;
          try
            while Enumerator.MoveNext do
              Str := Str + ' ' + Grammar.FindSymbol(Enumerator.Current).StrID;
          finally
            Enumerator.Free;
          end;
          if PremSet.Empty then
            Str := Str + ' @';
          Report.Add(Str);
        end;
      end;
    end;
  end;
  Report.Add('');
end;

procedure MakeSuivSetList(Grammar: TGrammar; Report: TStrings);
var
  I: Integer;
  Str: string;
  Enumerator: TScIntegerSetEnumerator;
begin
  Report.Add('ENSEMBLES DES SUIV DES NON-TERMINAUX');
  Report.Add('');
  for I := 0 to Grammar.NonTerminalCount-1 do
  begin
    with Grammar.NonTerminals[I] do
    begin
      Str := Format('SUIV(%s) =', [StrID]);
      Enumerator := SuivSet.GetEnumerator;
      try
        while Enumerator.MoveNext do
          Str := Str + ' ' + Grammar.FindSymbol(Enumerator.Current).StrID;
      finally
        Enumerator.Free;
      end;
      Report.Add(Str);
    end;
  end;
  Report.Add('');
end;

procedure MakeChooseIfSetList(Grammar: TGrammar; Report: TStrings);
var
  I, J, K: Integer;
  NonTerminal: TNonTerminal;
  Str: string;
  ChooseIfSet: TChooseIfSet;
begin
  Report.Add('ENSEMBLES CHOISIR SI DES CHOIX DES NON-TERMINAUX');
  Report.Add('');
  for I := 0 to Grammar.NonTerminalCount-1 do
  begin
    NonTerminal := Grammar.NonTerminals[I];
    with NonTerminal do
    begin
      Report.Add(StrID);
      for J := 0 to ChoiceCount-1 do
      begin
        with Choices[J] do
        begin
          if Empty then
            Str := '@'
          else
          begin
            Str := '';
            for K := 0 to ItemCOunt-1 do
              with Items[K] do
                Str := Str + ' ' + StrID;
            Delete(Str, 1, 1);
          end;
          Str := Format('    ChoisirSi(%s) =', [Str]);
          ChooseIfSet := ChooseIf[NonTerminal];
          for K := 0 to Grammar.TerminalCount +
            Grammar.NonTerminalCount - 1 do
            if ChooseIfSet.Exists(K) then
              Str := Str + ' ' + Grammar.FindSymbol(K).StrID;
          Report.Add(Str);
        end;
      end;
    end;
  end;
  Report.Add('');
end;

procedure MakeLL1ConflictList(Grammar: TGrammar; Report: TStrings);
var
  I: Integer;
begin
  Report.Add('CONFLITS LL(1) DÉTECTÉS');
  Report.Add('');
  if Grammar.LL1ConflictCount = 0 then
    Report.Add('Aucun conflit LL(1)')
  else
  begin
    for I := 0 to Grammar.LL1ConflictCount-1 do
      with Grammar.LL1Conflicts[I] do
        Report.Add(Format('%s %d %d',
          [NonTerminal.StrID, Choice1+1, Choice2+1]));
  end;
  Report.Add('');
end;

{---------------------}
{ Functional routines }
{---------------------}

procedure CheckLL1Conflicts(Grammar: TGrammar; Errors: TSepiCompilerErrorList);
var
  I: Integer;
begin
  for I := 0 to Grammar.LL1ConflictCount-1 do
  begin
    with Grammar.LL1Conflicts[I] do
      Errors.MakeError(Format(SLL1ConflictOnNonTerminal,
        [NonTerminal.StrID, Choice1+1, Choice2+1]));
  end;

  Errors.CheckForErrors;
end;

procedure HandleNonTerminals(Grammar: TGrammar; Generator: TGenerator);
var
  I: Integer;
  NonTerminal: TNonTerminal;
begin
  for I := 0 to Grammar.NonTerminalCount-1 do
  begin
    NonTerminal := Grammar.NonTerminals[I];

    // Declaration
    Generator.NonTerminalConsts.Add(Format(
      '  %s = %d; // %s',
      [NonTerminal.StrID, NonTerminal.ID, NonTerminal.Name]));

    // Symbol class name
    Generator.InitNodeNames.Add(Format(
      '  SymbolClassNames[%s] := %s;',
      [NonTerminal.StrID, StrToStrRepres(NonTerminal.StrID)]));

    // Simplify-set and show-as-text set
    if NonTerminal.Simplify then
      Generator.ClassesToSimplify.Include(NonTerminal.ID);
  end;
end;

procedure MakeParsingTable(Grammar: TGrammar; Generator: TGenerator);
var
  ParsingTable: TParsingTable;
  I, J, ChoiceID: Integer;
  Str: string;
  Choice: TChoice;
begin
  ParsingTable := TParsingTable.Create(Grammar);
  try
    for I := 0 to Grammar.NonTerminalCount-1 do
    begin
      Str := '    (';

      for J := 0 to Grammar.TerminalCount-1 do
      begin
        Choice := ParsingTable[Grammar.NonTerminals[I].ID,
          Grammar.Terminals[J].ID];

        if Choice = nil then
          ChoiceID := -1
        else
          ChoiceID := Choice.ID;

        Str := Str + Format('%3d, ', [ChoiceID]);
      end;

      Str[Length(Str)-1] := ')';
      if I = Grammar.NonTerminalCount-1 then
        Delete(Str, Length(Str), 1)
      else
        Str[Length(Str)] := ',';

      Generator.ParsingTable.Add(Str);
    end;
  finally
    ParsingTable.Free;
  end;
end;

procedure MakePushChoiceProcs(Grammar: TGrammar; Generator: TGenerator);
var
  I, J: Integer;
begin
  for I := 1 to Grammar.ChoiceCount-1 do // 1 because empty choice is inherited
  begin
    with Grammar.Choices[I] do
    begin
      // Declaration
      Generator.PushChoiceProcsDecls.Add(Format(
        '    procedure PushChoice%d;', [I]));

      // Method header
      Generator.PushChoiceProcs.Add('');
      Generator.PushChoiceProcs.Add(Format(
        'procedure TSepiLanguageParser.PushChoice%d;', [I]));
      Generator.PushChoiceProcs.Add('begin');

      // Try
      if IsTry then
      begin
        Generator.PushChoiceProcs.Add(Format(
          '  PushTry(%d);', [AlternativeChoice.ID]));
      end;

      // Back-to parent
      Generator.PushChoiceProcs.Add('  PushBackToParent;');

      // Actual choice: components
      for J := ItemCount-1 downto 0 do
      begin
        Generator.PushChoiceProcs.Add(Format('  Push%sSymbol(%s);',
          [IIF(ItemsToRemove[J], 'Fake', ''), Items[J].StrID]));
      end;

      // Method footer
      Generator.PushChoiceProcs.Add('end;');

      // Method-table initialization
      Generator.InitPushChoiceProcs.Add(Format(
        '  PushChoiceProcs[%d] := PushChoice%0:d;', [I]));
    end;
  end;
end;

{*
  Génère le parser
  @param Errors    Gestionnaire d'erreurs
  @param Options   Options du générateur
*}
procedure GenerateParser(Errors: TSepiCompilerErrorList;
  Options: TGeneratorOptions);
var
  Grammar: TGrammar;
  Report: TStrings;
  Generator: TGenerator;
begin
  if not FileExists(Options.SourceFileName) then
    Errors.MakeError(Format(SSourceFileDoesntExist, [Options.SourceFileName]),
      ekFatalError);

  Grammar := nil;
  Report := nil;
  Generator := nil;
  try
    // Create used objects
    Grammar := TGrammar.Create(Options.SourceFileName);
    Report := TStringList.Create;
    Generator := TGenerator.Create(Options.LanguageName,
      Options.DestDir + 'Sepi' + Options.LanguageName + 'Parser.pas');

    // Some trivial assignments for the generator
    Generator.ChoiceCount := Grammar.ChoiceCount;
    Generator.FirstNonTerminal := Grammar.FirstNonTerminalID;
    Generator.LastNonTerminal :=
      Grammar.FirstNonTerminalID + Grammar.NonTerminalCount - 1;

    // Report
    MakeTerminalList(Grammar, Report);
    MakeNonTerminalList(Grammar, Report);
    MakeChoiceEndList(Grammar, Report);
    MakePremSetList(Grammar, Report);
    MakeSuivSetList(Grammar, Report);
    MakeChooseIfSetList(Grammar, Report);
    MakeLL1ConflictList(Grammar, Report);

    // Save report
    Report.SaveToFile(ChangeFileExt(Options.SourceFileName, '.txt'));

    // Functional process
    CheckLL1Conflicts(Grammar, Errors);
    HandleNonTerminals(Grammar, Generator);
    MakeParsingTable(Grammar, Generator);
    MakePushChoiceProcs(Grammar, Generator);

    // Generate parser
    Generator.Generate;
  finally
    Report.Free;
    Generator.Free;
    Grammar.Free;
  end;
end;

{*
  Affiche une erreur à la console
  @param Self     Ignoré
  @param Sender   Objet qui a déclenché l'erreur
  @param Error    Erreur ajoutée
*}
procedure ErrorAdded(Self, Sender: TObject; Error: TSepiCompilerError);
begin
  WriteLn(ErrOutput, Error.AsText);
end;

{*
  Programme principal
*}
procedure Main;
var
  Options: TGeneratorOptions;
  Errors: TSepiCompilerErrorList;
begin
  try
    Options := TGeneratorOptions.Create;
    try
      Errors := TSepiCompilerErrorList.Create;
      try
        @Errors.OnAddError := @ErrorAdded;
        try
          GenerateParser(Errors, Options);
        except
          on Error: ESepiCompilerFatalError do;
          on Error: Exception do
            Errors.Add(TSepiCompilerError.Create(
              Format(SSepiInternalError, [Error.Message]), ekFatalError));
          on Error: TSepiCompilerError do
            Errors.Add(Error);
          on Error: TObject do
            Errors.Add(TSepiCompilerError.Create(
              Format(SSepiInternalError, [Error.ClassName]), ekFatalError));
        end;
      finally
        Errors.Free;
      end;
    finally
      Options.Free;
    end;
  except
    on Error: ECommandLineParsingException do
    begin
      WriteLn(Error.Message);
      Exit;
    end;
  end;
end;

begin
  Main;

  if FindCmdLineSwitch('w', ['-'], False) or
    FindCmdLineSwitch('-wait', ['-'], False) then
    ReadLn;
end.

