{*
  Programme SepiEdit
  SepiEdit est un éditeur générique pour toutes sortes de fichiers source Sepi.
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
program SepiEdit;

uses
  Forms;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Editeur Sepi';
  Application.Run;
end.

