{*
  Programme SepiOptions
  SepiOptions permet de consulter et de modifier les options d'installation des
  run-time de Sepi.
  @author S�bastien Jean Robert Doeraene
  @version 1.0
*}
program SepiOptions;

uses
  Forms,
  SepiOptionsMain in 'SepiOptionsMain.pas' {FormMain},
  SepiOptionsConsts in 'SepiOptionsConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Options Sepi';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

