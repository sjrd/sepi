unit SVCLReg;

interface

uses
  Classes, SvEdits, SvImages, SvLabels;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SVCL',
  [
  // Svdits
    TSvNumberEdit,
  // SvImages
    TSvDropImage,
  // SvLabels
    TSvURLLabel
  ]);
end;

end.

