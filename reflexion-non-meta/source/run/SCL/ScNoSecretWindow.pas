{*
  Emp�che la cr�ation par Delphi de sa fen�tre secr�te
  Ins�rez cette unit� en toute premi�re position de la clause uses de l'unit�
  program (avant Forms). Vous vous d�barrasserez ainsi de la fen�tre secr�te.
  @author sjrd
  @version 1.0
*}
unit ScNoSecretWindow;

interface

implementation

initialization
  // Simule une DLL
  IsLibrary := True;
end.
 
