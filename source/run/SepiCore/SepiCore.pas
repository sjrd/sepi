{*
  Définit les classes coeur de Sepi
  @author sjrd
  @version 1.0
*}
unit SepiCore;

interface

uses
  Windows, SysUtils, Classes, Registry, SepiConsts, Dialogs;

type
  {*
    Exception relative à la technologie Sepi
  *}
  ESepiError = class(Exception);

  {*
    Générée lorsqu'une opération requise n'est pas encore implémentée
  *}
  ESepiUnsupportedFeatureException = class(ESepiError);

  {*
    Version Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiVersion = record
    MajVersion: Integer; /// Version majeure
    MinVersion: Integer; /// Version mineure
  end;

  {*
    Classe maître universelle de Sepi
    @author sjrd
    @version 1.0
  *}
  TSepi = class
  private
    FName: string;          /// Nom du projet Sepi
    FVersion: TSepiVersion; /// Version courante de Sepi
    FAuthor: string;        /// Auteur de Sepi
    FAuthorEMail: string;   /// Adresse e-mail de l'auteur
    FWebSite: string;       /// Site Web de Sepi
    FPath: string;          /// Dossier d'installation des run-time Sepi

    FormatPackageFileName: string; /// Format des noms de fichiers package
    FPackages: TList;              /// Liste des packages chargés

    function FindPackage(FileName: TFileName): Pointer;
    function GetPackHandle(FileName: TFileName): HMODULE;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadPackage(FileName: TFileName): HMODULE; overload;
    procedure UnloadPackage(FileName: TFileName); overload;

    property Name: string read FName;
    property Version: TSepiVersion read FVersion;
    property Author: string read FAuthor;
    property AuthorEMail: string read FAuthorEMail;
    property WebSite: string read FWebSite;
    property Path: string read FPath;

    property PackageHandle[FileName: TFileName]: HMODULE read GetPackHandle;
  end;

const {don't localize}
  SepiEnvVarName = 'SEPI'; /// Nom de la variable d'environnement de Sepi

var
  Sepi: TSepi; /// Objet maître universel de Sepi - instance unique de TSepi

implementation

const
  SepiFormatPackageFileName = 'Packages'+PathDelim+'%s.bpl'; {don't localize}

type
  {*
    Pointeur vers TSepiPackage
  *}
  PSepiPackage = ^TSepiPackage;

  {*
    Informations sur un package chargé par Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiPackage = record
    FileName: string[20];
    Handle: HMODULE;
    Counter: Integer;
  end;

{--------------}
{ Classe TSepi }
{--------------}

{*
  Crée l'unique instance de TSepi
*}
constructor TSepi.Create;
begin
  if Assigned(Sepi) then
    raise ESepiError.Create(sSepiInstanceAlreadyExists);

  inherited;

  FName := sSepiName;
  FVersion.MajVersion := SepiMajVersion;
  FVersion.MinVersion := SepiMinVersion;
  FAuthor := sSepiAuthor;
  FAuthorEMail := sSepiAuthorEMail;
  FWebSite := sSepiWebSite;

  FPath := IncludeTrailingPathDelimiter(
    GetEnvironmentVariable(SepiEnvVarName));

  FormatPackageFileName := Path+SepiFormatPackageFileName;
  FPackages := TList.Create;
end;

{*
  Détruit l'instance
*}
destructor TSepi.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPackages.Count-1 do
    SysUtils.UnloadPackage(PSepiPackage(FPackages[I]).Handle);
  FPackages.Free;
  inherited;
end;

{*
  Trouve un package par son nom
  @param FileName   Nom du package à trouver
  @return Un pointeur sur les informations sur ce package
*}
function TSepi.FindPackage(FileName: TFileName): Pointer;
var
  I: Integer;
begin
  {$IFDEF MSWINDOWS} FileName := AnsiLowerCase(FileName); {$ENDIF}
  for I := 0 to FPackages.Count-1 do
  begin
    if PSepiPackage(FPackages[I]).FileName = FileName then
    begin
      Result := FPackages[I];
      Exit;
    end;
  end;
  Result := nil;
end;

{*
  Handles des packages chargés indexés par leurs noms respectifs
  @param FileName   Nom d'un package
  @return Handle du package, ou 0 si celui-ci n'a jamais été chargé
*}
function TSepi.GetPackHandle(FileName: TFileName): HMODULE;
var
  Package: PSepiPackage;
begin
  Package := PSepiPackage(FindPackage(FileName));
  if Assigned(Package) then
    Result := Package.Handle
  else
    Result := 0;
end;

{*
  Charge un package
  Tout appel à LoadPackage doit être compensé par un appel à UnloadPackage.
  @param FileName   Nom du package à charger
  @return Handle du package nouvellement chargé
*}
function TSepi.LoadPackage(FileName: TFileName): HMODULE;
var
  Package: PSepiPackage;
begin
  Package := PSepiPackage(FindPackage(FileName));
  if Assigned(Package) then
  begin
    Inc(Package.Counter);
    Result := Package.Handle;
  end else
  begin
    Result := SysUtils.LoadPackage(Format(FormatPackageFileName, [FileName]));
    if Result <> 0 then
    begin
      GetMem(Package, SizeOf(TSepiPackage));
      Package.FileName := FileName;
      Package.Handle := Result;
      Package.Counter := 1;
      FPackages.Add(Package);
    end;
  end;
end;

{*
  Décharge un package
  Tout appel à UnloadPackage doit être compensé par un appel à LoadPackage.
  @param FileName   Nom du package à décharger
*}
procedure TSepi.UnloadPackage(FileName: TFileName);
var
  Package: PSepiPackage;
begin
  Package := PSepiPackage(FindPackage(FileName));
  if Assigned(Package) then
  begin
    Dec(Package.Counter);
    if Package.Counter = 0 then
    begin
      FPackages.Remove(Package);
      SysUtils.UnloadPackage(Package.Handle);
      FreeMem(Package, SizeOf(TSepiPackage));
    end;
  end;
end;

initialization
  Sepi := TSepi.Create;
finalization
  Sepi.Free;
end.

