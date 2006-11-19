{*
  Enregistre les composants de la SDL dans la palette d'outils de Delphi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SDLReg;

interface

uses
  Classes, SdDialogs;

procedure Register;

implementation

{*
  Enregistre les composants de la SDL dans la palette d'outils de Delphi
*}
procedure Register;
begin
  RegisterComponents('SDL',
  [
  // SdDialogs
    TSdPasswordDialog, TSdAboutDialog, TSdNumberDialog
  ]);
end;

end.

