unit SdPassword;

interface

uses
{$IFDEF MSWINDOWS}
  Forms, StdCtrls, Buttons, Controls,
{$ENDIF}
{$IFDEF LINUX}
  QForms, QStdCtrls, QButtons, QControls,
{$ENDIF}
  ScUtils, ScConsts, Classes;

type
  TSdPasswordForm = class(TForm)
    LabelPrompt: TLabel;
    EditPassword: TEdit;
    BoutonOK: TBitBtn;
    BoutonAnnuler: TBitBtn;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    class function QueryPassword : string; overload;
    class function QueryPassword(Password : string; ShowErrorMes : boolean = True) : boolean; overload;
  end;

implementation

{$R *.dfm}

class function TSdPasswordForm.QueryPassword : string;
begin
  with Create(Application) do
  try
    ActiveControl := EditPassWord;
    if ShowModal <> mrOK then Result := '' else
      Result := EditPassWord.Text;
  finally
    Release;
  end;
end;

class function TSdPasswordForm.QueryPassword(Password : string; ShowErrorMes : boolean = True) : boolean;
var Passwd : string;
begin
  if Password = '' then Passwd := '' else
    Passwd := QueryPassword;
  Result := Passwd = Password;
  if (not Result) and ShowErrorMes and (Passwd <> '') then
    ShowDialog(sScWrongPassword, sScWrongPassword, dtError, dbOK);
end;

end.
