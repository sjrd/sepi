program SepiDelphiCompiler;

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
  SepiDelphiCompiler;

resourcestring
  SCantOpenSourceFile = 'Impossible de créer le fichier source %s';
  SCantOpenDestFile = 'Impossible de créer le fichier de sortie %s';
  SSepiInternalError = 'Erreur interne : %s';
  SCantCompileRCFile = 'Ne peut compiler le fichier de ressources %s';

const
  PascalExt = '.pas';
  CompiledIntfExt = '.sci';
  CompiledUnitExt = '.scu';
  RCExt = '.rc';
  TextExt = '.txt';
  MaxSizeBeforeLazyLoad = 100*1024;

procedure DebugMsg(const Msg: string);
begin
  OutputDebugString(PChar(TimeToStr(Time) + ' ' + Msg));
end;

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
    Info: TFileName;
    LazyLoad: Boolean;
  end;

  TFileNamesArray = array of TFileNames;

function GetFileNames(Context: TCompilerContext;
  const Line: string; DefaultLazyLoad: Boolean = False): TFileNames;
const // Don't localize
  SepiImportsStr = 'SepiImports';
var
  LazyLoadIdx: Integer;
  UnitName: string;
begin
  with Result do
  begin
    Source := GetXToken(Line, ';', 1);
    LazyLoadIdx := GetEnumValue(TypeInfo(Boolean), GetXToken(Line, ';', 2));
    Dest := GetXToken(Line, ';', 3);

    UnitName := ChangeFileExt(ExtractFileName(Source), '');
    Cache := Context.CacheDir + UnitName + CompiledIntfExt;

    if LazyLoadIdx >= 0 then
      LazyLoad := Boolean(LazyLoadIdx)
    else
      LazyLoad := DefaultLazyLoad;

    if Dest = '' then
      Dest := Context.OutputDir + SepiImportsStr + UnitName + PascalExt;

    Resource := Context.ResourceDir + UnitName + CompiledIntfExt;
    RCFile := Context.ResourceDir + SepiImportsStr + UnitName + RCExt;

    Info := ChangeFileExt(Cache, TextExt);

    Source := Context.SearchBDSFile(Source);
    if Source = '' then
      Source := GetXToken(Line, ';', 1);
  end;
end;

{*
  Charge une unité
  @param Context    Contexte de compilation
  @param Sender     Racine Sepi qui demande le chargement d'une unité
  @param UnitName   Nom de l'unité à charger
  @return L'unité chargée, ou nil si l'unité n'est pas trouvée
*}
function LoadUnit(Context: TCompilerContext; Sender: TSepiRoot;
  const UnitName: string): TSepiUnit;
var
  UnitFileName: string;
  Stream: TStream;
  LazyLoad: Boolean;
  RuntimeUnit: TSepiRuntimeUnit;
begin
  UnitFileName := Context.SearchSepiFile(UnitName + CompiledIntfExt);

  if UnitFileName <> '' then
  begin
    Stream := TFileStream.Create(UnitFileName, fmOpenRead);
    LazyLoad := Stream.Size > MaxSizeBeforeLazyLoad;
    try
      DebugMsg('Début du chargement de l''unité '+UnitName);
      Result := TSepiUnit.LoadFromStream(Sender, Stream, LazyLoad);
      DebugMsg('Fin du chargement de l''unité '+UnitName);
    finally
      if not LazyLoad then
        Stream.Free;
    end;
  end else
  begin
    UnitFileName := Context.SearchSepiFile(UnitName + CompiledUnitExt);
    if UnitFileName <> '' then
    begin
      Stream := TFileStream.Create(UnitFileName, fmOpenRead);
      try
        DebugMsg('Début du chargement de l''unité '+UnitName);
        RuntimeUnit := TSepiRuntimeUnit.Create(Sender, Stream);
        Result := RuntimeUnit.SepiUnit;
        DebugMsg('Fin du chargement de l''unité '+UnitName);
      finally
        Stream.Free;
      end;
    end else
      Result := nil;
  end;
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
  Effectue la compilation d'un fichier
  @param Errors           Liste d'erreurs
  @param Context          Contexte de compilation
  @param SourceFileName   Nom du fichier source à compiler
*}
procedure ProcessCompile(Errors: TSepiCompilerErrorList;
  Context: TCompilerContext; const SourceFileName: TFileName);
var
  UnitName: string;
  FileNames: TFileNames;
  SepiRoot: TSepiRoot;
  SourceFile, InfoFile: TStrings;
  DestFile: TStream;
  RootNode: TRootNode;
  Compiler: TSepiUnitCompiler;
  SepiUnit: TSepiUnit;
begin
  UnitName := ChangeFileExt(ExtractFileName(SourceFileName), '');

  with FileNames do
  begin
    Source := Context.SearchSepiFile(SourceFileName);

    if Source = '' then
      Errors.MakeError(Format(SCantOpenSourceFile, [SourceFileName]),
        ekFatalError);

    Dest := Context.OutputDir + UnitName + CompiledUnitExt;
    Info := Context.CacheDir + UnitName + '.txt';
  end;

  SourceFile := nil;
  DestFile := nil;
  InfoFile := nil;
  RootNode := nil;
  SepiRoot := TSepiRoot.Create;
  try
    SepiRoot.OnLoadUnit := TSepiLoadUnitEvent(MakeMethod(
      @LoadUnit, Context));

    SourceFile := TStringList.Create;
    InfoFile := TStringList.Create;

    Errors.CurrentFileName := ExtractFileName(FileNames.Source);

    DebugMsg('Début de l''analyse du fichier ' +
      ExtractFileName(FileNames.Source));

    try
      SourceFile.LoadFromFile(FileNames.Source);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenSourceFile, [FileNames.Source]),
          ekFatalError);
    end;

    try
      DestFile := TFileStream.Create(FileNames.Dest, fmCreate);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenDestFile, [FileNames.Dest]),
          ekFatalError);
    end;

    DebugMsg('Début de la compilation de l''interface');

    RootNode := TRootNode.Create(ntSource, SepiRoot, Errors);
    try
      TParser.Parse(RootNode, TLexer.Create(Errors, SourceFile.Text,
        Errors.CurrentFileName));
    except
      on Error: ESepiCompilerFatalError do
        raise;
      on Error: Exception do
      begin
        Errors.MakeError(Error.Message, ekFatalError,
          FindRightMostNode(RootNode).SourcePos);
      end;
    end;

    Compiler := RootNode.UnitCompiler;
    SepiUnit := RootNode.SepiUnit;

    DebugMsg('Compilation et enregistrement de l''unité compilée');

    Compiler.WriteToStream(DestFile);

    DebugMsg('Production et enregistrement des informations');

    ProduceUnitInfo(InfoFile, SepiUnit);
    InfoFile.SaveToFile(FileNames.Info);

    DebugMsg('Terminé');

    WriteLn(ExtractFileName(FileNames.Source), ' terminé');
  finally
    RootNode.Free;
    DestFile.Free;
    InfoFile.Free;
    SourceFile.Free;
    SepiRoot.Free;
  end;
end;

{*
  Compile un fichier ressource .rc avec brcc32
  @param Errors       Liste d'erreurs
  @param RCFileName   Nom du fichier .rc à compiler
*}
procedure CompileResource(Errors: TSepiCompilerErrorList;
  const RCFileName: TFileName);
var
  ProgramName, CommandLine: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  ProgramName := FileSearch('brcc32.exe', GetEnvironmentVariable('PATH'));
  CommandLine := '"' + ProgramName + '" "' + RCFileName + '"';

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);

  if not CreateProcess(PChar(ProgramName), PChar(CommandLine),
    nil, nil, False, CREATE_NO_WINDOW, nil, nil,
    StartupInfo, ProcessInfo) then
  begin
    // Couldn't create the compiler process - report the error
    Errors.MakeError(Format(SCantCompileRCFile, [RCFileName]));
    Exit;
  end;

  // Wait for termination
  WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
end;

{*
  Effectue l'import d'un fichier source Delphi
  @param Context     Contexte de compilation
  @param Errors      Liste d'erreurs
  @param SepiRoot    Racine Sepi
  @param FileNames   Noms des fichiers
*}
procedure ProcessFileImport(Context: TCompilerContext;
  Errors: TSepiCompilerErrorList; SepiRoot: TSepiRoot;
  const FileNames: TFileNames);
var
  SourceFile, DestFile, RCFile, InfoFile: TStrings;
  ResourceFile: TStream;
  RootNode: TRootNode;
  SepiUnit: TSepiUnit;
  I: Integer;
begin
  SourceFile := nil;
  ResourceFile := nil;
  DestFile := nil;
  RCFile := nil;
  InfoFile := nil;
  RootNode := nil;
  try
    SourceFile := TStringList.Create;
    DestFile := TStringList.Create;
    RCFile := TStringList.Create;
    InfoFile := TStringList.Create;

    Errors.CurrentFileName := ExtractFileName(FileNames.Source);

    DebugMsg('Début de l''analyse du fichier ' +
      ExtractFileName(FileNames.Source));

    try
      SourceFile.LoadFromFile(FileNames.Source);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenSourceFile, [FileNames.Source]),
          ekFatalError);
    end;

    try
      ResourceFile := TFileStream.Create(FileNames.Resource, fmCreate);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenDestFile, [FileNames.Resource]),
          ekFatalError);
    end;

    DebugMsg('Début de la compilation de l''interface');

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

    SepiUnit := RootNode.SepiUnit;

    DebugMsg('Recherche et suppression des types de compilation');

    for I := 0 to SepiUnit.ChildCount-1 do
    begin
      if SepiUnit.Children[I].ClassType = TSepiMeta then
      begin
        SepiUnit.Children[I].Free;
        Break;
      end;
    end;

    if Context.ExcludeRoutines then
    begin
      DebugMsg('Suppression des routines');

      for I := SepiUnit.ChildCount-1 downto 0 do
      begin
        if (SepiUnit.Children[I] is TSepiMethod) or
          (SepiUnit.Children[I] is TSepiOverloadedMethod) then
          SepiUnit.Children[I].Free;
      end;
    end;

    DebugMsg('Début de la production de l''importeur');

    DestFile.Text := ProduceImporter(SepiUnit, '', FileNames.LazyLoad);
    RCFile.Add(Format('SepiImports%s RCDATA %s',
      [SepiUnit.Name, ExtractFileName(FileNames.Resource)]));

    DebugMsg('Début de la production des informations');

    ProduceUnitInfo(InfoFile, SepiUnit);

    DebugMsg('Enregistrement des fichiers');

    SepiUnit.SaveToStream(ResourceFile);
    RCFile.SaveToFile(FileNames.RCFile);
    InfoFile.SaveToFile(FileNames.Info);

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
    InfoFile.Free;
    SourceFile.Free;
  end;

  DebugMsg('Compilation de la ressource');

  CompileResource(Errors, FileNames.RCFile);

  DebugMsg('Recopie de l''interface dans le cache');

  CopyFile(PChar(FileNames.Resource), PChar(FileNames.Cache), False);

  DebugMsg('Terminé');

  WriteLn(ExtractFileName(FileNames.Source), ' terminé');
end;

{*
  Effectue l'import d'un fichier
  @param Errors            Liste d'erreurs
  @param Context           Contexte de compilation
  @param SourceFileName    Nom du fichier source à compiler
  @param ProduceLazyLoad   Produit des importateurs en lazy-load
*}
procedure ProcessImport(Errors: TSepiCompilerErrorList;
  Context: TCompilerContext; const SourceFileName: TFileName;
  ProduceLazyLoad: Boolean = False);
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

    // Quick import support
    if AnsiSameText(ExtractFileExt(SourceFileName), PascalExt) then
    begin
      ProcessFileImport(Context, Errors, SepiRoot,
        GetFileNames(Context, SourceFileName, ProduceLazyLoad));
      Exit;
    end;

    // CSV-file import
    SourceFile := TStringList.Create;
    try
      // Read source file
      try
        SourceFile.LoadFromFile(SourceFileName);
      except
        on Error: EStreamError do
          Errors.MakeError(Format(SCantOpenSourceFile, [SourceFileName]),
            ekFatalError);
      end;

      // Parse source file
      SetLength(FileNames, SourceFile.Count);
      for I := 0 to SourceFile.Count-1 do
        FileNames[I] := GetFileNames(Context, SourceFile[I], ProduceLazyLoad);

      // Process
      for I := 0 to Length(FileNames)-1 do
      begin
        ProcessFileImport(Context, Errors, SepiRoot, FileNames[I]);
        //Errors.CheckForErrors;
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
var
  Options: TOptions;
  Errors: TSepiCompilerErrorList;
  Context: TCompilerContext;
  I: Integer;
  FileName: TFileName;
begin
  DecimalSeparator := '.';

  Errors := nil;
  Context := nil;

  try
    Options := TOptions.Create;
  except
    on Error: ECommandLineParsingException do
    begin
      WriteLn(Error.Message);
      if FindCmdLineSwitch('w', ['-'], False) then
        ReadLn;
      Exit;
    end;
  end;

  try
    Errors := TSepiCompilerErrorList.Create;
    @Errors.OnAddError := @ErrorAdded;

    try
      try
        Context := TCompilerContext.Create(Options.BDSVersion);

        Context.CacheDir := Context.ReplaceMacros(Options.CacheDir);
        Context.OutputDir := Context.ReplaceMacros(Options.OutputDir);
        Context.ResourceDir := Context.ReplaceMacros(Options.ResourceDir);

        Context.BDSBrowsingPath := Dir + PathSep + Context.BDSBrowsingPath;
        Context.SepiBrowsingPath :=
          Dir + PathSep + Options.CacheDir + PathSep + Options.OutputDir;

        Context.ExcludeRoutines := Options.ExcludeRoutines;
      except
        on Error: EBDSVersionNotInstalled do
          Errors.MakeError(Error.Message, ekFatalError);
      end;

      for I := 0 to Options.FileNames.Count-1 do
      begin
        FileName := Options.FileNames[I];
        case Options.Action of
          paCompile:
            ProcessCompile(Errors, Context, FileName);
          paImport:
            ProcessImport(Errors, Context, FileName, Options.ProduceLazyLoad);
        end;
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
end.

