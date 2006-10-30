{*
  Enregistre les composants de la SVCL dans la palette d'outils de Delphi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SVCLReg;

interface

uses
  Classes, SvEdits, SvImages, SvLabels;

procedure Register;

implementation

{*
  Enregistre les composants de la SVCL dans la palette d'outils de Delphi
*}
procedure Register;
begin
  RegisterComponents('SVCL',
  [
  // SvEdits
    TSvNumberEdit,
  // SvImages
    TSvDropImage,
  // SvLabels
    TSvURLLabel
  ]);
end;

end.

