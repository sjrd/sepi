{-------------------------------------------------------------------------------
SepiExplorer - Example program for Sepi
As an example program, SepiExplorer is free of any usage. It is released in the
public domain.
-------------------------------------------------------------------------------}

{*
  Options de l'explorateur
  @author sjrd
  @version 1.0
*}
unit ExplorerOptions;

interface

uses
  Types, Classes, SysUtils, IniFiles, ScUtils, ScStrUtils, ExplorerConsts;

type
  {*
    Options enregistrées de l'explorateur
    @author sjrd
    @version 1.0
  *}
  TExplorerOptions = class
  private
    FFileName: TFileName; /// Nom du fichier où sont sockées les options

    FBrowsingPath: string; /// Chemin de recherche des unités Sepi

    FAliases: TStrings; /// Alias d'unité

    procedure LoadFromFile;
    procedure SaveToFile;
  protected
    property FileName: TFileName read FFileName;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function SearchFile(const FileName: TFileName): TFileName;

    function ResolveAlias(const Alias: string;
      out Names: TStringDynArray): Boolean;

    property BrowsingPath: string read FBrowsingPath write FBrowsingPath;

    property Aliases: TStrings read FAliases;
  end;

implementation

{*
  [@inheritDoc]
*}
constructor TExplorerOptions.Create;
begin
  inherited Create;

  FFileName := ChangeFileExt(ParamStr(0), IniExt);

  FAliases := TStringList.Create;
end;

{*
  [@inheritDoc]
*}
destructor TExplorerOptions.Destroy;
begin
  FAliases.Free;

  inherited;
end;

{*
  Charge les options depuis le fichier d'options
*}
procedure TExplorerOptions.LoadFromFile;
var
  IniFile: TMemIniFile;
  AliasesNames: TStrings;
  I: Integer;
begin
  if not FileExists(FileName) then
    Exit;

  IniFile := TMemIniFile.Create(FileName);
  try
    BrowsingPath := IniFile.ReadString('Sepi', 'BrowsingPath', '');

    AliasesNames := TStringList.Create;
    try
      IniFile.ReadSection('Aliases', AliasesNames);

      for I := 0 to AliasesNames.Count-1 do
        if AliasesNames[I] <> '' then
          Aliases.Values[AliasesNames[I]] :=
            IniFile.ReadString('Aliases', AliasesNames[I], '');
    finally
      AliasesNames.Free;
    end;
  finally
    IniFile.Free;
  end;
end;

{*
  Enregistre les options dans le fichier d'options
*}
procedure TExplorerOptions.SaveToFile;
var
  IniFile: TMemIniFile;
  I: Integer;
begin
  IniFile := TMemIniFile.Create(FileName);
  try
    IniFile.WriteString('Sepi', 'BrowsingPath', BrowsingPath);

    for I := 0 to Aliases.Count-1 do
      IniFile.WriteString('Aliases', Aliases.Names[I],
        Aliases.ValueFromIndex[I]);

    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TExplorerOptions.AfterConstruction;
begin
  inherited;

  LoadFromFile;
end;

{*
  [@inheritDoc]
*}
procedure TExplorerOptions.BeforeDestruction;
begin
  inherited;

  SaveToFile;
end;

{*
  Recherche un fichier dans un chemin de recherche
  @param FileName   Nom du fichier recherché
  @return Nom complet du fichier, ou une chaîne vide si non trouvé
*}
function TExplorerOptions.SearchFile(const FileName: TFileName): TFileName;
begin
  Result := FileSearch(FileName, BrowsingPath);
end;

{*
  Résoud un alias d'unités
  @param Alias   Alias à résoudre
  @param Names   En sortie : tableau des unités référencées
  @return True si Alias était bien un alias, False sinon
*}
function TExplorerOptions.ResolveAlias(const Alias: string;
  out Names: TStringDynArray): Boolean;
var
  StrNames: string;
begin
  StrNames := Aliases.Values[Alias];
  Result := StrNames <> '';

  if Result then
    Names := SplitTokenAll(StrNames, ',');
end;

end.

