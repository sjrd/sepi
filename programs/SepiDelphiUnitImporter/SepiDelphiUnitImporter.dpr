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
  SepiRuntime,
  SepiCompiler,
  ScUtils,
  ScStrUtils,
  ScDelphiLanguage,
  ScConsoleUtils,
  SepiCompilerErrors,
  SepiParseTrees,
  SepiDisassembler,
  SepiDelphiLexer,
  SepiDelphiParser,
  SepiDelphiCompiler,
  ImporterContext in 'ImporterContext.pas',
  ImporterConsts in 'ImporterConsts.pas',
  ImporterProducer in 'ImporterProducer.pas',
  ImporterOptions in 'ImporterOptions.pas',
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
      Context.Errors.MakeError(Format(SCantFindSourceFile,
        [GetXToken(Line, CSVSep, 1)]), ekFatalError);

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
      Result := TSepiUnit.LoadFromStream(Sender, Stream, LazyLoad);
      if LazyLoad then
        Result.AcquireObjResource(Stream);
    finally
      Stream.Free;
    end;
  end else
    Result := nil;
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
  RootNode: TRootNode;
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
    Errors.CurrentFileName := ExtractFileName(FileNames.Source);

    // Create source file stream (PAS file)
    try
      SourceFile.LoadFromFile(FileNames.Source);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenSourceFile, [FileNames.Source]),
          ekFatalError);
    end;

    // Create resource file stream (SCI file)
    try
      ResourceFile := TFileStream.Create(FileNames.Resource, fmCreate);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenDestFile, [FileNames.Resource]),
          ekFatalError);
    end;

    // Actually compile the source code
    RootNode := TRootNode.Create(ntSource, SepiRoot, Errors);
    try
      TParser.Parse(RootNode, TLexer.Create(Errors, SourceFile.Text,
        Errors.CurrentFileName, True));
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
      if SepiUnit.Children[I].ClassType = TSepiMeta then
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
  WriteLn(ExtractFileName(FileNames.Source), ' terminé');
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

        Context.BDSBrowsingPath := Dir + PathSep + Context.BDSBrowsingPath;
        Context.SepiBrowsingPath :=
          Dir + PathSep + Options.CacheDir + PathSep + Options.OutputDir;

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
  DecimalSeparator := '.';

  Main;
end.

