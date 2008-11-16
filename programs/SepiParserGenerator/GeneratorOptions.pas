unit GeneratorOptions;

interface

uses
  SysUtils, ScConsoleUtils;

type
  {*
    Options du générateur
    @author sjrd
    @version 1.0
  *}
  TGeneratorOptions = class(TCommandLine)
  private
    FSourceFileName: TFileName; /// Nom du fichier source
    FDestDir: TFileName;        /// Nom du dossier de destination
    FLanguageName: string;      /// Nom du langage à compiler
  public
    constructor Create;

    procedure AfterConstruction; override;

    property SourceFileName: TFileName read FSourceFileName;
    property DestDir: TFileName read FDestDir;
    property LanguageName: string read FLanguageName;
  end;

implementation

{*
  Crée les options du générateur
*}
constructor TGeneratorOptions.Create;
var
  Option: TCommandLineOption;
begin
  inherited Create(['-']);

  Option := TStringOption.Create('source', ['s'], @FSourceFileName);
  AddOption(Option);
  AddOption(TOptionAlias.Create('input', ['i'], Option));

  Option := TStringOption.Create('destination', ['d'], @FDestDir);
  AddOption(Option);
  AddOption(TOptionAlias.Create('output', ['o'], Option));
  AddOption(TOptionAlias.Create('dest', [], Option));

  Option := TStringOption.Create('language', ['l'], @FLanguageName);
  AddOption(Option);
end;

{*
  [@inheritDoc]
*}
procedure TGeneratorOptions.AfterConstruction;
begin
  inherited;

  Parse;

  FDestDir := IncludeTrailingPathDelimiter(FDestDir);
end;

end.

