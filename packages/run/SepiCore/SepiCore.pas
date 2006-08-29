unit SepiCore;

interface

uses
  Windows, SysUtils, Classes, Registry, SepiConsts, Dialogs;

type
  ESepiError = class(Exception);

  TSepiVersion = record
    MajVersion : integer;
    MinVersion : integer;
  end;

  TSepi = class
  private
    FName : string;
    FVersion : TSepiVersion;
    FAuthor : string;
    FAuthorEMail : string;
    FWebSite : string;
    FPath : string;

    FormatPackageFileName : string;
    FPackages : TList;

    function FindPackage(FileName : TFileName) : Pointer;
    function GetPackHandle(FileName : TFileName) : HMODULE;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadPackage(FileName : TFileName) : HMODULE; overload;
    procedure UnloadPackage(FileName : TFileName); overload;

    property Name : string read FName;
    property Version : TSepiVersion read FVersion;
    property Author : string read FAuthor;
    property AuthorEMail : string read FAuthorEMail;
    property WebSite : string read FWebSite;
    property Path : string read FPath;

    property PackageHandle[FileName : TFileName] : HMODULE read GetPackHandle;
  end;

var
  Sepi : TSepi;

implementation

const
  SepiFormatPackageFileName = 'Packages'+PathDelim+'%s.bpl'; {don't localize}

type
  PSepiPackage = ^TSepiPackage;
  TSepiPackage = record
    FileName : string[20];
    Handle : HMODULE;
    Counter : integer;
  end;

////////////////////
/// Classe TSepi ///
////////////////////

constructor TSepi.Create;
begin
  inherited;
  FName := sSepiName;
  FVersion.MajVersion := SepiMajVersion;
  FVersion.MinVersion := SepiMinVersion;
  FAuthor := sSepiAuthor;
  FAuthorEMail := sSepiAuthorEMail;
  FWebSite := sSepiWebSite;

  FPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('SEPI')); {don't localize}

  FormatPackageFileName := Path+SepiFormatPackageFileName;
  FPackages := TList.Create;
end;

destructor TSepi.Destroy;
var I : integer;
begin
  for I := 0 to FPackages.Count-1 do
    SysUtils.UnloadPackage(PSepiPackage(FPackages[I]).Handle);
  FPackages.Free;
  inherited;
end;

function TSepi.FindPackage(FileName : TFileName) : Pointer;
var I : integer;
begin
  {$IFDEF MSWINDOWS} FileName := AnsiLowerCase(FileName); {$ENDIF}
  for I := 0 to FPackages.Count-1 do
  begin
    if PSepiPackage(FPackages[I]).FileName = FileName then
    begin
      Result := FPackages[I];
      exit;
    end;
  end;
  Result := nil;
end;

function TSepi.GetPackHandle(FileName : TFileName) : HMODULE;
var Package : PSepiPackage;
begin
  Package := PSepiPackage(FindPackage(FileName));
  if Assigned(Package) then Result := Package.Handle else Result := 0;
end;

function TSepi.LoadPackage(FileName : TFileName) : HMODULE;
var Package : PSepiPackage;
begin
  Package := PSepiPackage(FindPackage(FileName));
  if Assigned(Package) then
  begin
    inc(Package.Counter);
    Result := Package.Handle;
  end else
  begin
    Result := SysUtils.LoadPackage(Format(FormatPackageFileName, [FileName]));
    if Result <> 0 then
    begin
      GetMem(Package, sizeof(TSepiPackage));
      Package.FileName := FileName;
      Package.Handle := Result;
      Package.Counter := 1;
      FPackages.Add(Package);
    end;
  end;
end;

procedure TSepi.UnloadPackage(FileName : TFileName);
var Package : PSepiPackage;
begin
  Package := PSepiPackage(FindPackage(FileName));
  if Assigned(Package) then
  begin
    dec(Package.Counter);
    if Package.Counter = 0 then
    begin
      FPackages.Remove(Package);
      SysUtils.UnloadPackage(Package.Handle);
      FreeMem(Package, sizeof(TSepiPackage));
    end;
  end;
end;

initialization
  Sepi := TSepi.Create;
finalization
  Sepi.Free;
end.

