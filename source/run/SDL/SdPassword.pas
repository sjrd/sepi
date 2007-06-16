{*
  Boîte de dialogue de saisie de mot de passe
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SdPassword;

interface

uses
{$IFDEF MSWINDOWS}
  Forms, StdCtrls, Buttons, Controls,
{$ENDIF}
{$IFDEF LINUX}
  QForms, QStdCtrls, QButtons, QControls,
{$ENDIF}
  Classes;

type
  {*
    Boîte de dialogue de saisie de mot de passe
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
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
    class function QueryPassword(Password : string;
      ShowErrorMes : boolean = True) : boolean; overload;
  end;

implementation

{$R *.dfm}

uses
  SdDialogs, ScConsts;

{*
  Demande un mot de passe à l'utilisateur
  @return Le mot de passe qu'a saisi l'utilisateur
*}
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

{*
  Demande un mot de passe à l'utilisateur
  @param Password       Mot de passe correct
  @param ShowErrorMes   Indique s'il faut notifier sur erreur
  @return True si l'utilisateur a saisi le bon mot de passe, False sinon
*}
class function TSdPasswordForm.QueryPassword(Password : string;
  ShowErrorMes : boolean = True) : boolean;
var Passwd : string;
begin
  if Password = '' then Passwd := '' else
    Passwd := QueryPassword;
  Result := Passwd = Password;
  if (not Result) and ShowErrorMes and (Passwd <> '') then
    ShowDialog(sScWrongPassword, sScWrongPassword, dtError, dbOK);
end;

end.

