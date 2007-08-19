{*
  Bo�te de dialogue de saisie de mot de passe
  @author sjrd
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
    Bo�te de dialogue de saisie de mot de passe
    @author sjrd
    @version 1.0
  *}
  TSdPasswordForm = class(TForm)
    LabelPrompt: TLabel;    /// Label pour l'invite
    EditPassword: TEdit;    /// Edit pour le mot de passe
    BoutonOK: TBitBtn;      /// Bouton OK
    BoutonAnnuler: TBitBtn; /// Bouton Annuler
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    class function QueryPassword : string; overload;
    class function QueryPassword(Password : string;
      ShowErrorMes : boolean = True) : boolean; overload;
  end;

implementation

{$R *.dfm}

uses
  SdDialogs, ScConsts;

{*
  Demande un mot de passe � l'utilisateur
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
  Demande un mot de passe � l'utilisateur
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

