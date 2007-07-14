{*
  Enregistre les composants de SepiCore dans la palette d'outils de Delphi
  @author sjrd
  @version 1.0
*}
unit SepiCoreReg;

interface

uses
  Classes, SepiAbout;

procedure Register;

implementation

{*
  Enregistre les composants de SepiCore dans la palette d'outils de Delphi
*}
procedure Register;
begin
  RegisterComponents('Sepi', [TSepiAboutDialog]);
end;

end.

