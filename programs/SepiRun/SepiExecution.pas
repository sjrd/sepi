{-------------------------------------------------------------------------------
SepiRun - Example program for Sepi
As an example program, SepiRun is free of any usage. It is released in the
public domain.
-------------------------------------------------------------------------------}

{*
  Exécution d'une unité Sepi
  @author sjrd
  @version 1.0
*}
unit SepiExecution;

interface

uses
  SysUtils, Classes, ScUtils, ScDelphiLanguage, SepiReflectionCore, SepiMembers,
  SepiRuntime, SepiDisassembler;

const
  /// Extension d'une unité Sepi compilée
  CompiledUnitExt = '.scu';

type
  {*
    Contexte d'exécution d'un programme Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiExecution = class(TObject)
  private
    FSepiRoot: TSepiRoot; /// Racine Sepi

    FBrowsingPath: string; /// Chemin de recherche
    FUseDebugger: Boolean; /// Indique s'il faut utiliser un debugger
  public
    constructor Create;
    destructor Destroy; override;

    function SearchFile(const FileName: TFileName): TFileName;
    function LoadUnit(Sender: TSepiRoot; const UnitName: string): TSepiUnit;

    procedure ExtraSimpleDebug(Context: TSepiRuntimeContext);

    procedure Execute(const UnitName: string);

    property SepiRoot: TSepiRoot read FSepiRoot;

    property BrowsingPath: string read FBrowsingPath write FBrowsingPath;
    property UseDebugger: Boolean read FUseDebugger write FUseDebugger;
  end;

implementation

{*
  Crée un contexte d'exécution de programme Sepi
*}
constructor TSepiExecution.Create;
begin
  inherited Create;

  FSepiRoot := TSepiRoot.Create;

  FBrowsingPath := Dir;
end;

{*
  [@inheritDoc]
*}
destructor TSepiExecution.Destroy;
begin
  FSepiRoot.Free;
  inherited;
end;

{*
  Recherche un fichier dans le chemin de recherche
  @param FileName   Fichier à rechercher
  @return Nom complet du fichier, ou une chaîne vide si non trouvé
*}
function TSepiExecution.SearchFile(const FileName: TFileName): TFileName;
begin
  Result := FileSearch(FileName, BrowsingPath);
end;

{*
  Charge une unité depuis son nom
  @param Sender     Racine Sepi qui déclenche ce gestionnaire
  @param UnitName   Nom de l'unité à charger
  @return Unité chargée, ou nil si non trouvée
*}
function TSepiExecution.LoadUnit(Sender: TSepiRoot;
  const UnitName: string): TSepiUnit;
var
  UnitFileName: string;
  Stream: TStream;
  RuntimeUnit: TSepiRuntimeUnit;
begin
  UnitFileName := SearchFile(UnitName + CompiledUnitExt);

  if UnitFileName <> '' then
  begin
    Stream := TFileStream.Create(UnitFileName, fmOpenRead);
    try
      RuntimeUnit := TSepiRuntimeUnit.Create(Sender, Stream);
      Result := RuntimeUnit.SepiUnit;

      if UseDebugger then
        RuntimeUnit.OnDebug := ExtraSimpleDebug;
    finally
      Stream.Free;
    end;
  end else
    Result := nil;
end;

{*
  Gestionnaire de débogage extrêmement simple
  @param Context   Contexte d'exécution
*}
procedure TSepiExecution.ExtraSimpleDebug(Context: TSepiRuntimeContext);
var
  Instructions: TStrings;
begin
  Instructions := TStringList.Create;
  try
    with TSepiDisassembler.Create do
    try
      Disassemble(Context.NextInstruction, Instructions, Context.RuntimeUnit,
        0, 1);
    finally
      Free;
    end;

    if Instructions.Count > 0 then
    begin
      Write(#9, Instructions[0]);
      {$IFDEF DEBUGGER_WAITS}
        ReadLn;
      {$ELSE}
        WriteLn;
      {$ENDIF}
    end;
  finally
    Instructions.Free;
  end;
end;

{*
  Exécute une unité
  @param UnitName   Nom de l'unité à charger et exécuter
*}
procedure TSepiExecution.Execute(const UnitName: string);
const
  MainMethodName = 'Main';
var
  SepiRoot: TSepiRoot;
  SepiUnit: TSepiUnit;
  MainMethod: TSepiMethod;
  EntryPoint: TProcedure;
begin
  SepiRoot := TSepiRoot.Create;
  try
    SepiRoot.OnLoadUnit := LoadUnit;

    SepiUnit := SepiRoot.LoadUnit(UnitName);
    MainMethod := SepiUnit.FindComponent(MainMethodName) as TSepiMethod;
    EntryPoint := TProcedure(MainMethod.Code);
    EntryPoint;
  finally
    SepiRoot.Free;
  end;
end;

end.

