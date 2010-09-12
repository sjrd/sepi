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
  Importeur d'unité native Delphi dans Sepi
  @author sjrd
  @version 1.0
*}
program SepiDelphiUnitImporter;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Classes,
  Registry,
  StrUtils,
  TypInfo,
  SepiReflectionCore,
  SepiMembers,
  SepiSystemUnit,
  SepiRuntime,
  SepiCompiler,
  ScUtils,
  ScStrUtils,
  ScLists,
  ScConsoleUtils,
  SepiCompilerErrors,
  SepiParseTrees,
  SepiDisassembler,
  SepiLexerUtils,
  SepiDelphiLexer,
  SepiDelphiParser,
  SepiDelphiCompiler,
  ImporterConsts in 'ImporterConsts.pas',
  ImporterContext in 'ImporterContext.pas',
  ImporterOptions in 'ImporterOptions.pas',
  ImporterProducer in 'ImporterProducer.pas',
  ImporterTemplates in 'ImporterTemplates.pas';

procedure ErrorAdded(Self, Sender: TObject; Error: TSepiCompilerError);
begin
  WriteLn(ErrOutput, Error.AsText);
end;

type
  TFileNames = record
    Source: TFileName;
    Cache: TFileName;
    Dest: TFileName;
    Resource: TFileName;
    RCFile: TFileName;
    LazyLoad: Boolean;
  end;

  TFileNamesArray = array of TFileNames;

{*
  Décompose une ligne d'un CSV pour produire les noms de fichiers à traiter
*}
function GetFileNames(Context: TImporterContext;
  const Line: string): TFileNames;
const // Don't localize
  CSVSep = ';';
  SepiImportsStr = 'SepiImports';
var
  LazyLoadIdx: Integer;
  UnitName: string;
begin
  with Result do
  begin
    // Tokenize line

    Source := GetXToken(Line, CSVSep, 1);
    LazyLoadIdx := GetEnumValue(TypeInfo(Boolean), GetXToken(Line, CSVSep, 2));
    Dest := GetXToken(Line, CSVSep, 3);

    // Fill in the record

    Source := Context.SearchBDSFile(Source);
    if Source = '' then
    begin
      if Context.SkipIfNotExists then
        Exit;

      Context.Errors.MakeError(Format(SCantFindSourceFile,
        [GetXToken(Line, CSVSep, 1)]), ekFatalError);
    end;

    UnitName := ChangeFileExt(ExtractFileName(Source), '');
    Cache := Context.CacheDir + UnitName + CompiledIntfExt;

    if LazyLoadIdx >= 0 then
      LazyLoad := Boolean(LazyLoadIdx)
    else
      LazyLoad := Context.ProduceLazyLoad;

    if Dest = '' then
      Dest := Context.OutputDir + SepiImportsStr + UnitName + PascalExt;

    Resource := Context.ResourcesDir + UnitName + CompiledIntfExt;
    RCFile := Context.ResourcesDir + SepiImportsStr + UnitName + RCExt;
  end;
end;

{*
  Charge une unité
  @param Context    Contexte de compilation
  @param Sender     Racine Sepi qui demande le chargement d'une unité
  @param UnitName   Nom de l'unité à charger
  @return L'unité chargée, ou nil si l'unité n'est pas trouvée
*}
function LoadUnit(Context: TImporterContext; Sender: TSepiRoot;
  const UnitName: string): TSepiUnit;
var
  UnitFileName: string;
  Stream: TStream;
  LazyLoad: Boolean;
begin
  UnitFileName := Context.SearchSepiFile(UnitName + CompiledIntfExt);

  if UnitFileName <> '' then
  begin
    Stream := TFileStream.Create(UnitFileName, fmOpenRead);
    LazyLoad := Stream.Size > MaxSizeBeforeLazyLoad;
    try
      if AnsiSameText(UnitName, SystemUnitName) then
        Result := TSepiSystemUnit.LoadFromStream(Sender, Stream)
      else
      begin
        Result := TSepiUnit.LoadFromStream(Sender, Stream, LazyLoad);
        if LazyLoad then
          Result.AcquireObjResource(Stream);
      end;
    finally
      Stream.Free;
    end;
  end else
    Result := nil;
end;

{*
  Apply the overloads on a source file
  @param SourceFile   Source file
  @param Overloads    Overloads to apply to this source file
*}
procedure ApplyOverloads(Context: TImporterContext;
  SourceFile, Overloads: TStrings);
const
  Delimiter = '----------';
var
  Source: string;
  Index: Integer;
  FromText, ToText: string;
  FromLineCount, ToLineCount: Integer;

  procedure ReadBlock(var Text: string; var LineCount: Integer);
  begin
    Text := '';
    LineCount := 0;

    while (Index < Overloads.Count) and (Overloads[Index] <> Delimiter) do
    begin
      Text := Text + Overloads[Index] + SourceFile.LineBreak;
      Inc(LineCount);
      Inc(Index);
    end;

    if Index >= Overloads.Count then
      Context.Errors.MakeError(SBadlyFormedOverloadFile);

    Inc(Index);
  end;

begin
  Source := SourceFile.Text;

  Index := 0;
  while Index < Overloads.Count do
  begin
    Index := StringsOps.IndexOf(Overloads, Delimiter, Index)+1;
    if Index <= 0 then
      Break;

    ReadBlock(FromText, FromLineCount);
    ReadBlock(ToText, ToLineCount);

    // Pad ToText so that it has the same number of lines as FromText
    while ToLineCount < FromLineCount do
    begin
      ToText := ToText + SourceFile.LineBreak;
      Inc(ToLineCount);
    end;

    // Warning when padding cannot be done
    if ToLineCount > FromLineCount then
      Context.Errors.MakeError(SOverloadHasMoreLinesThanOriginal, ekWarning);

    Source := AnsiReplaceStr(Source, FromText, ToText);
  end;

  SourceFile.Text := Source;
end;

{*
  Charge un fichier source Delphi
  @param Context      Contexte d'importation
  @param FileNames    Noms des fichiers
  @param SourceFile   Liste de chaînes destination
*}
procedure LoadSourceFile(Context: TImporterContext; const FileNames: TFileNames;
  SourceFile: TStrings);
var
  OverloadFileName: TFileName;
  Overloads: TStrings;
begin
  try
    SourceFile.LoadFromFile(FileNames.Source);
  except
    on EStreamError do
      Context.Errors.MakeError(Format(SCantOpenSourceFile,
        [FileNames.Source]), ekFatalError);
  end;

  OverloadFileName := Context.OverloadDir + ExtractFileName(FileNames.Source);
  if not FileExists(OverloadFileName) then
    Exit;

  Overloads := TStringList.Create;
  try
    Overloads.LoadFromFile(OverloadFileName);
    ApplyOverloads(Context, SourceFile, Overloads);
  finally
    Overloads.Free;
  end;
end;

{*
  Gestionnaire d'événement OnNeedFile des analyseurs lexicaux
  @param Context    Contexte de compilation
  @param Sender     Analyseur syntaxique qui a besoin d'un fichier
  @param FileName   Nom du fichier recherché
*}
procedure LexerNeedFile(Context: TImporterContext; Sender: TSepiCustomLexer;
  var FileName: TFileName);
var
  TestFileName: TFileName;
begin
  if FileExists(FileName) then
    Exit;

  TestFileName := IncludeTrailingPathDelimiter(
    ExtractFileDir(Sender.CurrentPos.FileName)) + FileName;

  if FileExists(TestFileName) then
  begin
    FileName := TestFileName;
    Exit;
  end;

  TestFileName := Context.SearchSepiFile(FileName);
  if TestFileName <> '' then
    FileName := TestFileName;
end;

{*
  Trouve le noeud le plus à droite d'un sous-arbre syntaxique
  @param Root   Racine du sous-arbre
  @return Noeud le plus à droite du sous-arbre
*}
function FindRightMostNode(Root: TSepiParseTreeNode): TSepiParseTreeNode;
begin
  Result := Root;

  while (Result is TSepiNonTerminal) and
    (TSepiNonTerminal(Result).ChildCount > 0) do
  begin
    with TSepiNonTerminal(Result) do
      Result := Children[ChildCount-1];
  end;
end;

{*
  Compile un fichier ressource .rc avec brcc32
  @param Context      Contexte d'importation
  @param RCFileName   Nom du fichier .rc à compiler
*}
procedure CompileResource(Context: TImporterContext;
  const RCFileName: TFileName);
const // don't localize
  ResCompilerName = 'brcc32.exe';
  PathEnvVar = 'PATH';
var
  ProgramName, CommandLine: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CreateProcessResult: Boolean;
begin
  // Find brcc32 program
  ProgramName := FileSearch(ResCompilerName,
    GetEnvironmentVariable(PathEnvVar));
  if ProgramName = '' then
  begin
    Context.Errors.MakeError(Format(SCantFindProgram, [ResCompilerName]));
    Exit;
  end;

  // Build command line
  CommandLine := '"' + ProgramName + '" "' + RCFileName + '"';

  // Create the brcc32 process
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);
  CreateProcessResult := CreateProcess(PChar(ProgramName), PChar(CommandLine),
    nil, nil, False, CREATE_NO_WINDOW, nil, nil, StartupInfo, ProcessInfo);

  // Check success
  if not CreateProcessResult then
  begin
    // Couldn't create the compiler process - report the error
    Context.Errors.MakeError(Format(SCantCompileRCFile, [RCFileName]));
    Exit;
  end;

  // Wait for termination
  WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
end;

{*
  Effectue l'import d'un fichier source Delphi
  @param Context     Contexte d'importation
  @param SepiRoot    Racine Sepi
  @param FileNames   Noms des fichiers
*}
procedure ProcessFileImport(Context: TImporterContext;
  SepiRoot: TSepiRoot; const FileNames: TFileNames);
var
  Errors: TSepiCompilerErrorList;
  SourceFile, DestFile, RCFile: TStrings;
  ResourceFile: TStream;
  RootNode: TDelphiRootNode;
  Lexer: TSepiDelphiLexer;
  SepiUnit: TSepiUnit;
  I: Integer;
begin
  Errors := Context.Errors;

  SourceFile := nil;
  ResourceFile := nil;
  DestFile := nil;
  RCFile := nil;
  RootNode := nil;
  try
    SourceFile := TStringList.Create;
    DestFile := TStringList.Create;
    RCFile := TStringList.Create;

    // Update current file name
    Errors.CurrentFileName := FileNames.Source;

    // Create source file stream (PAS file)
    LoadSourceFile(Context, FileNames, SourceFile);

    // Create resource file stream (SCI file)
    try
      ResourceFile := TFileStream.Create(FileNames.Resource, fmCreate);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenDestFile, [FileNames.Resource]),
          ekFatalError);
    end;

    // Actually compile the source code
    RootNode := TDelphiRootNode.Create(ntSource, SepiRoot, Errors);
    try
      RootNode.IsImporter := True;

      Lexer := TSepiDelphiInterfaceLexer.Create(Errors, SourceFile.Text,
        Errors.CurrentFileName);

      Lexer.OnNeedFile := TSepiLexerNeedFileEvent(MakeMethod(
        @LexerNeedFile, Context));

      TSepiDelphiParser.Parse(RootNode, Lexer);
    except
      on Error: ESepiCompilerFatalError do
        raise;
      on Error: Exception do
      begin
        Errors.MakeError(Error.Message, ekFatalError,
          FindRightMostNode(RootNode).SourcePos);
      end;
    end;

    // Fetch Sepi unit
    SepiUnit := RootNode.SepiUnit;

    // Destroy compile-time types (TSepiRuntimeUnit won't do it for us here)
    for I := 0 to SepiUnit.ChildCount-1 do
    begin
      if SepiUnit.Children[I].ClassType = TSepiComponent then
      begin
        SepiUnit.Children[I].Free;
        Break;
      end;
    end;

    // If exclude-routines switch was present, delete all routines
    if Context.ExcludeRoutines then
    begin
      for I := SepiUnit.ChildCount-1 downto 0 do
      begin
        if (SepiUnit.Children[I] is TSepiMethod) or
          (SepiUnit.Children[I] is TSepiOverloadedMethod) then
          SepiUnit.Children[I].Free;
      end;
    end;

    // Produce importer source files (PAS and RC files)
    DestFile.Text := ProduceImporter(SepiUnit, '', FileNames.LazyLoad);
    RCFile.Add(Format('SepiImports%s RCDATA %s',
      [SepiUnit.Name, ExtractFileName(FileNames.Resource)]));

    // Save importer source files (PAS and RS files)
    SepiUnit.SaveToStream(ResourceFile);
    RCFile.SaveToFile(FileNames.RCFile);

    // Save importer resource file (SCI file)
    try
      DestFile.SaveToFile(FileNames.Dest);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenDestFile, [FileNames.Dest]),
          ekFatalError);
    end;
  finally
    RootNode.Free;
    DestFile.Free;
    RCFile.Free;
    ResourceFile.Free;
    SourceFile.Free;
  end;

  // Compile resource file (RC -> RES via brcc32)
  CompileResource(Context, FileNames.RCFile);

  // Save a copy of the compiled unit into our cache
  CopyFile(PChar(FileNames.Resource), PChar(FileNames.Cache), False);

  // Let the user know we finished that file
  WriteLn(ExtractFileName(FileNames.Source), ' done');
end;

{*
  Effectue l'import d'un fichier
  Le fichier à importer peut être directement un .pas (import rapide), ou un
  fichier .csv détaillant une liste de fichiers .pas avec leurs options
  respectives.
  @param Context           Contexte de compilation
  @param SourceFileName    Nom du fichier source à traiter
*}
procedure ProcessImport(Context: TImporterContext;
  const SourceFileName: TFileName);
var
  SourceFile: TStrings;
  SepiRoot: TSepiRoot;
  FileNames: TFileNamesArray;
  I: Integer;
begin
  SepiRoot := TSepiRoot.Create;
  try
    SepiRoot.OnLoadUnit := TSepiLoadUnitEvent(MakeMethod(
      @LoadUnit, Context));

    SourceFile := TStringList.Create;
    try
      if AnsiSameText(ExtractFileExt(SourceFileName), PascalExt) then
      begin
        // Quick import support
        SourceFile.Add(SourceFileName);
      end else
      begin
        // Read source file
        try
          SourceFile.LoadFromFile(SourceFileName);
        except
          on Error: EStreamError do
            Context.Errors.MakeError(
              Format(SCantOpenSourceFile, [SourceFileName]), ekFatalError);
        end;
      end;

      // Parse source file
      SetLength(FileNames, SourceFile.Count);
      for I := 0 to SourceFile.Count-1 do
        FileNames[I] := GetFileNames(Context, SourceFile[I]);

      // Process
      for I := 0 to Length(FileNames)-1 do
      begin
        if FileNames[I].Source = '' then
          Continue;

        ProcessFileImport(Context, SepiRoot, FileNames[I]);

        { While Sepi compiler is in development, we don't want errors to
          block us. }
        //Context.Errors.CheckForErrors;
      end;
    finally
      SourceFile.Free;
    end;
  finally
    SepiRoot.Free;
  end;
end;

{*
  Programme principal
*}
procedure Main;
var
  Options: TOptions;
  Errors: TSepiCompilerErrorList;
  Context: TImporterContext;
  I: Integer;
  FileName: TFileName;
begin
  Errors := nil;
  Context := nil;

  // Load command line options
  try
    Options := TOptions.Create;
  except
    on Error: ECommandLineParsingException do
    begin
      WriteLn(Error.Message);
      if FindCmdLineSwitch('w', ['-'], False) or
        FindCmdLineSwitch('-wait', ['-'], False) then
        ReadLn;
      Exit;
    end;
  end;

  try
    Errors := TSepiCompilerErrorList.Create;
    @Errors.OnAddError := @ErrorAdded;

    try
      // Load importer context
      try
        Context := TImporterContext.Create(Errors, Options.BDSVersion);

        Context.CacheDir := Context.ReplaceMacros(Options.CacheDir);
        Context.OutputDir := Context.ReplaceMacros(Options.OutputDir);
        Context.ResourcesDir := Context.ReplaceMacros(Options.ResourcesDir);

        Context.BDSBrowsingPath := Context.BDSBrowsingPath;
        Context.SepiBrowsingPath :=
          Options.CacheDir + PathSep + Options.OutputDir;

        Context.SkipIfNotExists := Options.SkipIfNotExists;
        Context.ProduceLazyLoad := Options.ProduceLazyLoad;
        Context.ExcludeRoutines := Options.ExcludeRoutines;
      except
        on Error: EBDSVersionNotInstalled do
          Errors.MakeError(Error.Message, ekFatalError);
      end;

      // Process each file in turn
      for I := 0 to Options.FileNames.Count-1 do
      begin
        FileName := Options.FileNames[I];
        ProcessImport(Context, FileName);
      end;
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

    if Options.WaitWhenFinished then
      ReadLn;
  finally
    Context.Free;
    Errors.Free;
    Options.Free;
  end;
end;

begin
  Main;
end.

