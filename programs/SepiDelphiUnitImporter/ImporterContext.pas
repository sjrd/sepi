{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  S�bastien Doeraene
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
  Contexte d'importation
  @author sjrd
  @version 1.0
*}
unit ImporterContext;

interface

uses
  Windows, SysUtils, Classes, StrUtils, Registry, SepiCompilerErrors,
  ImporterConsts, ScUtils;

type
  EBDSVersionNotInstalled = class(Exception);

  {*
    Contexte d'importation
    @author sjrd
    @version 1.0
  *}
  TImporterContext = class(TObject)
  private
    FErrors: TSepiCompilerErrorList; /// Gestionnaire d'erreurs

    FBDSVendor: string;  /// Nom du revendeur de BDS dans la BdR
    FBDSVersion: string; /// Version de BDS dans la BdR

    FBDSRootDir: string;        /// Dossier racine de BDS
    FDirectoryConsts: TStrings; /// Constantes repr�sentant des r�pertoires
    FBDSBrowsingPath: string;   /// Chemin de recherche BDS
    FSepiBrowsingPath: string;  /// Chemin de recherche Sepi

    FOverloadDir: TFileName;  /// Dossier des overloads pour les fichiers lus
    FCacheDir: TFileName;     /// Dossier de cache
    FOutputDir: TFileName;    /// Dossier de destination
    FResourcesDir: TFileName; /// Dossier de destination des ressources

    FSkipIfNotExists: Boolean; /// Ignorer les unit�s non trouv�es
    FProduceLazyLoad: Boolean; /// Produire du code lazy-load
    FExcludeRoutines: Boolean; /// Exclure les routines

    class function FindVendor(Errors: TSepiCompilerErrorList;
      const BDSVersion: string): string;

    procedure LoadEnvironmentStrings;
    procedure LoadFromRegistry;
  public
    constructor Create(AErrors: TSepiCompilerErrorList;
      const ABDSVendor, ABDSVersion: string); overload;
    constructor Create(AErrors: TSepiCompilerErrorList;
      const ABDSVersion: string); overload;
    destructor Destroy; override;

    function ReplaceMacros(const Str: string): string;

    function SearchFile(const FileName: TFileName;
      const BrowsingPath: string): TFileName;
    function SearchBDSFile(const FileName: TFileName): TFileName;
    function SearchSepiFile(const FileName: TFileName): TFileName;

    property Errors: TSepiCompilerErrorList read FErrors;

    property BDSVendor: string read FBDSVendor;
    property BDSVersion: string read FBDSVersion;

    property BDSRootDir: string read FBDSRootDir;
    property DirectoryConsts: TStrings read FDirectoryConsts;
    property BDSBrowsingPath: string
      read FBDSBrowsingPath write FBDSBrowsingPath;
    property SepiBrowsingPath: string
      read FSepiBrowsingPath write FSepiBrowsingPath;

    property OverloadDir: TFileName read FOverloadDir write FOverloadDir;
    property CacheDir: TFileName read FCacheDir write FCacheDir;
    property OutputDir: TFileName read FOutputDir write FOutputDir;
    property ResourcesDir: TFileName read FResourcesDir write FResourcesDir;

    property SkipIfNotExists: Boolean
      read FSkipIfNotExists write FSkipIfNotExists;
    property ProduceLazyLoad: Boolean
      read FProduceLazyLoad write FProduceLazyLoad;
    property ExcludeRoutines: Boolean
      read FExcludeRoutines write FExcludeRoutines;
  end;

const
  /// Format de la clef racine de BDS dans la base de registres
  BDSRootKeyFmt = '\Software\%s\BDS\%s\';

  /// Format du dossier des overloads
  OverloadDirBase = 'OvldSource\';

implementation

{*
  Cr�e le contexte de compilation
  @param AErrors       Gestionnaire d'erreurs � utiliser
  @param ABDSVendor    Nom du revendeur de BDS dans la BdR
  @param ABDSVersion   Version de BDS
  @raise EBDSVersionNotInstalled Cette version de BDS n'est pas install�e
*}
constructor TImporterContext.Create(AErrors: TSepiCompilerErrorList;
  const ABDSVendor, ABDSVersion: string);
begin
  inherited Create;

  FErrors := AErrors;

  FBDSVendor := ABDSVendor;
  FBDSVersion := ABDSVersion;

  FDirectoryConsts := TStringList.Create;

  LoadEnvironmentStrings;
  LoadFromRegistry;

  FOverloadDir := Dir + OverloadDirBase;
end;

{*
  Cr�e le contexte de compilation
  Dans cette variante, le nom du revendeur de BDS est d�tect� automatiquement
  d'apr�s la version
  @param AErrors       Gestionnaire d'erreurs � utiliser
  @param ABDSVersion   Version de BDS
  @raise EBDSVersionNotInstalled Cette version de BDS n'est pas install�e
*}
constructor TImporterContext.Create(AErrors: TSepiCompilerErrorList;
  const ABDSVersion: string);
begin
  Create(AErrors, FindVendor(AErrors, ABDSVersion), ABDSVersion);
end;

{*
  [@inheritDoc]
*}
destructor TImporterContext.Destroy;
begin
  FDirectoryConsts.Free;

  inherited;
end;

{*
  Trouve le revendeur d'une version de BDS donn�e
  @param Errors       Gestionnaire d'erreurs
  @param BDSVersion   Version de BDS
  @return Revendeur de cette version
  @raise EBDSVersionNotInstalled Cette version de BDS n'est pas install�e
*}
class function TImporterContext.FindVendor(Errors: TSepiCompilerErrorList;
  const BDSVersion: string): string;
const
  SoftwareKey = '\Software\';
var
  VendorList: TStrings;
  I: Integer;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(SoftwareKey, False);

    VendorList := TStringList.Create;
    try
      // Test every vendor found in HKEY_LOCAL_MACHINE\Software\
      GetKeyNames(VendorList);
      for I := 0 to VendorList.Count-1 do
      begin
        Result := VendorList[I];
        if KeyExists(Format(BDSRootKeyFmt, [Result, BDSVersion])) then
          Exit;
      end;

      // None contained BDS\Version: it's not installed
      Errors.MakeError(Format(SBDSVersionNotInstalled, [BDSVersion]),
        ekFatalError);
    finally
      VendorList.Free;
    end;
  finally
    Free;
  end;
end;

{*
  Charge les variables d'environnement
*}
procedure TImporterContext.LoadEnvironmentStrings;
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
  Lit les infos sur BDS depuis la base de registres
  @raise EBDSVersionNotInstalled Cette version de BDS n'est pas install�e
*}
procedure TImporterContext.LoadFromRegistry;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;

    // Open root key
    if not OpenKey(Format(BDSRootKeyFmt, [BDSVendor, BDSVersion]), False) then
      raise EBDSVersionNotInstalled.CreateFmt(SBDSVersionNotInstalled,
        [BDSVendor, BDSVersion]);

    // Fetch BDS root dir
    FBDSRootDir := ReadString('RootDir'); {don't localize}
    FDirectoryConsts.Values['BDS'] := BDSRootDir; {don't localize}

    // Fetch browsing path
    if OpenKey('Library', False) then
    begin
      FBDSBrowsingPath := ReadString('Browsing Path') + PathSep +
        ReadString('Search Path');
    end;

    // Fetch user browsing path
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(Format(BDSRootKeyFmt, [BDSVendor, BDSVersion]) + 'Library',
      False) then
    begin
      FBDSBrowsingPath := ReadString('Browsing Path') + PathSep +
        ReadString('Search Path') + PathSep + FBDSBrowsingPath;
    end;
  finally
    Free;
  end;
end;

{*
  Remplace toutes les macros par leurs valeurs
  @param Str   Cha�ne source
  @return Str dans laquelle chaque $(X) est remplac� par la valeur de X
*}
function TImporterContext.ReplaceMacros(const Str: string): string;
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
  Le chemin de recherche peut contenir des constantes de la forme $(X) o� X est
  un nom parmi les noms de DirectoryConsts.
  @param FileName       Nom du fichier � chercher
  @param BrowsingPath   Chemin de recherche
  @return Nom du fichier complet trouv�, o� une cha�ne vide si non trouv�
*}
function TImporterContext.SearchFile(const FileName: TFileName;
  const BrowsingPath: string): TFileName;
begin
  Result := FileSearch(FileName, ReplaceMacros(BrowsingPath));
end;

{*
  Cherche un fichier dans le chemin de recherche BDS
  @param FileName   Nom du fichier � chercher
  @return Nom du fichier complet trouv�, o� une cha�ne vide si non trouv�
*}
function TImporterContext.SearchBDSFile(const FileName: TFileName): TFileName;
begin
  Result := SearchFile(FileName, BDSBrowsingPath);
end;

{*
  Cherche un fichier dans le chemin de recherche Sepi
  @param FileName   Nom du fichier � chercher
  @return Nom du fichier complet trouv�, o� une cha�ne vide si non trouv�
*}
function TImporterContext.SearchSepiFile(const FileName: TFileName): TFileName;
begin
  Result := SearchFile(FileName, SepiBrowsingPath);
end;

end.

