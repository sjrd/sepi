unit SDLReg;

interface

uses
  Classes, SdDialogs;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SDL',
  [
  // SdDialogs
    TSdPasswordDialog, TSdAboutDialog
  ]);
end;

end.
