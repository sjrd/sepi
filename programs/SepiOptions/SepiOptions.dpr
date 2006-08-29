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

