{*
  Programme SepiEdit
  SepiEdit est un �diteur g�n�rique pour toutes sortes de fichiers source Sepi.
  @author S�bastien Jean Robert Doeraene
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

