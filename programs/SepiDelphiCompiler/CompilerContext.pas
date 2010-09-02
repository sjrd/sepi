{-------------------------------------------------------------------------------
SepiDelphiCompiler - Example program for Sepi
As an example program, SepiDelphiCompiler is free of any usage. It is released
in the public domain.
-------------------------------------------------------------------------------}

{*
  Contexte de compilation
  @author sjrd
  @version 1.0
*}
unit CompilerContext;

interface

uses
  Windows, SysUtils, Classes, StrUtils, Registry, SepiCompilerErrors;

type
  {*
    Contexte de compilation
    @author sjrd
    @version 1.0
  *}
  TCompilerContext = class(TObject)
  private
    FErrors: TSepiCompilerErrorList; /// Gestionnaire d'erreurs

    FDirectoryConsts: TStrings;  /// Constantes représentant des répertoires
    FSepiBrowsingPath: string;   /// Chemin de recherche Sepi
    FSourceBrowsingPath: string; /// Chemin de recherche des sources

    FOutputDir: TFileName; /// Dossier de destination des unités compilées

    procedure LoadEnvironmentStrings;
  public
    constructor Create(AErrors: TSepiCompilerErrorList);
    destructor Destroy; override;

    function ReplaceMacros(const Str: string): string;

    function SearchFile(const FileName: TFileName;
      const BrowsingPath: string): TFileName;
    function SearchSepiFile(const FileName: TFileName): TFileName;
    function SearchSourceFile(const FileName: TFileName): TFileName;

    property Errors: TSepiCompilerErrorList read FErrors;

    property DirectoryConsts: TStrings read FDirectoryConsts;
    property SepiBrowsingPath: string
      read FSepiBrowsingPath write FSepiBrowsingPath;
    property SourceBrowsingPath: string
      read FSourceBrowsingPath write FSourceBrowsingPath;

    property OutputDir: TFileName read FOutputDir write FOutputDir;
  end;

implementation

{*
  Crée le contexte de compilation
  @param AErrors   Gestionnaire d'erreurs à utiliser
*}
constructor TCompilerContext.Create(AErrors: TSepiCompilerErrorList);
begin
  inherited Create;

  FErrors := AErrors;

  FDirectoryConsts := TStringList.Create;

  LoadEnvironmentStrings;
end;

{*
  [@inheritDoc]
*}
destructor TCompilerContext.Destroy;
begin
  FDirectoryConsts.Free;

  inherited;
end;

{*
  Charge les variables d'environnement
*}
procedure TCompilerContext.LoadEnvironmentStrings;
var
  EnvBlock, EnvStrings: PChar;
  EnvString: string;
begin
  EnvBlock := GetEnvironmentStrings;
  try
    EnvStrings := EnvBlock;
    while EnvStrings^ <> #0 do
    begin
      EnvString := string(EnvStrings);
      Inc(EnvStrings, Length(EnvString) + 1);
      FDirectoryConsts.Add(EnvString);
    end;
  finally
    FreeEnvironmentStrings(EnvBlock);
  end;
end;

{*
  Remplace toutes les macros par leurs valeurs
  @param Str   Chaîne source
  @return Str dans laquelle chaque $(X) est remplacé par la valeur de X
*}
function TCompilerContext.ReplaceMacros(const Str: string): string;
var
  I: Integer;
begin
  Result := Str;

  for I := 0 to DirectoryConsts.Count-1 do
    Result := AnsiReplaceText(Result,
      '$('+DirectoryConsts.Names[I]+')', DirectoryConsts.ValueFromIndex[I]);

  Result := AnsiReplaceStr(Result, PathDelim+PathDelim, PathDelim);
end;

{*
  Cherche un fichier dans un chemin de recherche
  Le chemin de recherche peut contenir des constantes de la forme $(X) où X est
  un nom parmi les noms de DirectoryConsts.
  @param FileName       Nom du fichier à chercher
  @param BrowsingPath   Chemin de recherche
  @return Nom du fichier complet trouvé, où une chaîne vide si non trouvé
*}
function TCompilerContext.SearchFile(const FileName: TFileName;
  const BrowsingPath: string): TFileName;
begin
  Result := FileSearch(FileName, ReplaceMacros(BrowsingPath));
end;

{*
  Cherche un fichier dans le chemin de recherche Sepi
  @param FileName   Nom du fichier à chercher
  @return Nom du fichier complet trouvé, où une chaîne vide si non trouvé
*}
function TCompilerContext.SearchSepiFile(const FileName: TFileName): TFileName;
begin
  Result := SearchFile(FileName, SepiBrowsingPath);
end;

{*
  Cherche un fichier dans le chemin de recherche des sources
  @param FileName   Nom du fichier à chercher
  @return Nom du fichier complet trouvé, où une chaîne vide si non trouvé
*}
function TCompilerContext.SearchSourceFile(
  const FileName: TFileName): TFileName;
begin
  Result := SearchFile(FileName, SourceBrowsingPath);
end;

end.

