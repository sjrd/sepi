program Sepi;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  ScUtils,
  ScConsoleUtils,
  SepiReflectionCore,
  SepiRuntime,
  SepiDisassembler,
  SepiExecution in 'SepiExecution.pas',
  RunOptions in 'RunOptions.pas',
  Imports in 'Imports.pas';

procedure Main;
var
  Execution: TSepiExecution;
  Options: TSepiRunOptions;
  UnitName: string;
begin
  Execution := TSepiExecution.Create;
  try
    // Load command line options
    Options := TSepiRunOptions.Create;
    try
      Execution.BrowsingPath := Options.BrowsingPath;
      Execution.UseDebugger := Options.UseDebugger;
      UnitName := Options.UnitName;
    finally
      Options.Free;
    end;

    // Execute
    Execution.Execute(UnitName);
  finally
    Execution.Free;
  end;
end;

begin
  try
    Main;
  except
    on Error: ECommandLineParsingException do
      WriteLn(ErrOutput, Error.Message);
    on Error: Exception do
      WriteLn(ErrOutput, Error.ClassName, ': ', Error.Message);
  end;

  if FindCmdLineSwitch('w', ['-'], False) or
    FindCmdLineSwitch('-wait', ['-'], False) then
    ReadLn;
end.

