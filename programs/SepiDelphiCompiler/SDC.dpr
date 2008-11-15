{-------------------------------------------------------------------------------
SepiDelphiCompiler - Example program for Sepi
As an example program, SepiDelphiCompiler is free of any usage. It is released
in the public domain.
-------------------------------------------------------------------------------}

{*
  Programme exemple de Sepi qui compile une unité Delphi via Sepi
  @author sjrd
  @version 1.0
*}
program SDC;

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
  SDCConsts in 'SDCConsts.pas',
  CompilerContext in 'CompilerContext.pas',
  CompilerOptions in 'CompilerOptions.pas',
  Imports in 'Imports.pas';

procedure ErrorAdded(Self, Sender: TObject; Error: TSepiCompilerError);
begin
  WriteLn(ErrOutput, Error.AsText);
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
      Result := TSepiUnit.LoadFromStream(Sender, Stream, LazyLoad);
      if LazyLoad then
        Result.AcquireObjResource(Stream);
    finally
      Stream.Free;
    end;
  end else
  begin
    UnitFileName := Context.SearchSepiFile(UnitName + CompiledUnitExt);
    if UnitFileName <> '' then
    begin
      Stream := TFileStream.Create(UnitFileName, fmOpenRead);
      try
        RuntimeUnit := TSepiRuntimeUnit.Create(Sender, Stream);
        Result := RuntimeUnit.SepiUnit;
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
  @param Context          Contexte de compilation
  @param SourceFileName   Nom du fichier source à compiler
*}
procedure ProcessCompile(Context: TCompilerContext;
  const SourceFileName: TFileName);
var
  Errors: TSepiCompilerErrorList;
  SrcFileName, DestFileName: TFileName;
  UnitName: string;
  SepiRoot: TSepiRoot;
  SourceFile: TStrings;
  DestFile: TStream;
  RootNode: TRootNode;
  Compiler: TSepiUnitCompiler;
begin
  Errors := Context.Errors;

  // Find source file
  SrcFileName := Context.SearchSourceFile(SourceFileName);
  if SrcFileName = '' then
    Errors.MakeError(Format(SCantFindSourceFile, [SourceFileName]),
      ekFatalError);

  // Set unit name and destination file name
  UnitName := ChangeFileExt(ExtractFileName(SourceFileName), '');
  DestFileName := Context.OutputDir + UnitName + CompiledUnitExt;

  SourceFile := nil;
  DestFile := nil;
  RootNode := nil;
  SepiRoot := TSepiRoot.Create;
  try
    SepiRoot.OnLoadUnit := TSepiLoadUnitEvent(MakeMethod(
      @LoadUnit, Context));

    SourceFile := TStringList.Create;

    // Update current file name
    Errors.CurrentFileName := ExtractFileName(SrcFileName);

    // Load source file
    try
      SourceFile.LoadFromFile(SrcFileName);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenSourceFile, [SrcFileName]),
          ekFatalError);
    end;

    // Create destination file stream
    try
      DestFile := TFileStream.Create(DestFileName, fmCreate);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenDestFile, [DestFileName]),
          ekFatalError);
    end;

    // Actually compile the source file
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

    // Check for errors
    Errors.CheckForErrors;

    // Fetch Sepi unit compiler
    Compiler := RootNode.UnitCompiler;

    // Compile and write compiled unit to destination stream
    Compiler.WriteToStream(DestFile);

    // Let the user know we've finished this file
    WriteLn(ExtractFileName(SrcFileName), ' terminé');
  finally
    RootNode.Free;
    DestFile.Free;
    SourceFile.Free;
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
  Context: TCompilerContext;
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
      // Load context
      Context := TCompilerContext.Create(Errors);

      Context.SepiBrowsingPath := Options.SepiBrowsingPath;
      Context.SourceBrowsingPath := Options.SourceBrowsingPath;

      Context.OutputDir := Context.ReplaceMacros(Options.OutputDir);

      // Process each file in turn
      for I := 0 to Options.FileNames.Count-1 do
      begin
        FileName := Options.FileNames[I];
        ProcessCompile(Context, FileName);
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

