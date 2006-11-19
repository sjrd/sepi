{*
  Empêche la création par Delphi de sa fenêtre secrète
  Insérez cette unité en toute première position de la clause uses de l'unité
  program (avant Forms). Vous vous débarrasserez ainsi de la fenêtre secrête.
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit ScNoSecretWindow;

interface

implementation

initialization
  // Simule une DLL
  IsLibrary := True;
end.
 
