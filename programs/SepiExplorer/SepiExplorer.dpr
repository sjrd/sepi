{-------------------------------------------------------------------------------
SepiExplorer - Example program for Sepi
As an example program, SepiExplorer is free of any usage. It is released in the
public domain.
-------------------------------------------------------------------------------}

{*
  Programme exemple de Sepi qui permet de visualiser le contenu des unités Sepi
  @author sjrd
  @version 1.0
*}
program SepiExplorer;

uses
  Forms,
  SepiExplorerMain in 'SepiExplorerMain.pas' {ExplorerForm},
  ExplorerOptions in 'ExplorerOptions.pas',
  ExplorerConsts in 'ExplorerConsts.pas',
  MetaExplorer in 'MetaExplorer.pas' {FrameMetaExplorer: TFrame},
  TypesInfo in 'TypesInfo.pas',
  MembersInfo in 'MembersInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Explorateur Sepi';
  Application.CreateForm(TExplorerForm, ExplorerForm);
  Application.Run;
end.

