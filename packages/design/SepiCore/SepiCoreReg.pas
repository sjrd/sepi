unit SepiCoreReg;

interface

uses
  Classes, SepiAbout;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Sepi', [TSepiAboutDialog]);
end;

end.
