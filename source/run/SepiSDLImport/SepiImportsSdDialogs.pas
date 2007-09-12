{*
  Importe l'unit� SdDialogs dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsSdDialogs;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Graphics, SdDialogs;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTSdPasswordDialog = class(TSdPasswordDialog)
  private
    function Execute_0: Boolean;
    function Execute_1(Password: string;
      ShowErrorMes: Boolean = True): Boolean;
    function Execute_2(ShowErrorMes: Boolean): Boolean;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTSdAboutDialog = class(TSdAboutDialog)
  private
    procedure SetProgramIcon(New: TIcon);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTSdNumberDialog = class(TSdNumberDialog)
  private
    function Execute_0(const ATitle, APrompt: string;
      ADefault, AMin, AMax: Integer): Integer;
    function Execute_1(ADefault, AMin, AMax: Integer): Integer;
    function Execute_2(const ATitle, APrompt: string;
      AMin, AMax: Integer): Integer;
    function Execute_3(AMin, AMax: Integer): Integer;
    function Execute_4: Integer;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{--------------------------}
{ TSdPasswordDialog import }
{--------------------------}

function TSepiImportsTSdPasswordDialog.Execute_0: Boolean;
begin
  Result := Execute;
end;

function TSepiImportsTSdPasswordDialog.Execute_1(Password: string;
  ShowErrorMes: Boolean = True): Boolean;
begin
  Result := Execute(Password, ShowErrorMes);
end;

function TSepiImportsTSdPasswordDialog.Execute_2(ShowErrorMes: Boolean): Boolean;
begin
  Result := Execute(ShowErrorMes);
end;

class function TSepiImportsTSdPasswordDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSdPasswordDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPassword', System.TypeInfo(string));
    AddField('FShowErrorMes', System.TypeInfo(Boolean));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTSdPasswordDialog.Create,
      'constructor(AOwner : TComponent)',
      mlkOverride);
    AddOverloadedMethod('Execute', @TSepiImportsTSdPasswordDialog.Execute_0,
      'function: boolean');
    AddOverloadedMethod('Execute', @TSepiImportsTSdPasswordDialog.Execute_1,
      'function(Password : string; ShowErrorMes : boolean = True ) : boolean');
    AddOverloadedMethod('Execute', @TSepiImportsTSdPasswordDialog.Execute_2,
      'function(ShowErrorMes : boolean) : boolean');

    CurrentVisibility := mvPublished;

    AddProperty('Password', 'property: string',
      'FPassword', 'FPassword');
    AddProperty('ShowErrorMes', 'property: boolean',
      'FShowErrorMes', 'FShowErrorMes',
      NoIndex, Integer(True));

    Complete;
  end;
end;

{-----------------------}
{ TSdAboutDialog import }
{-----------------------}

procedure TSepiImportsTSdAboutDialog.SetProgramIcon(New: TIcon);
begin
  ProgramIcon := New;
end;

class function TSepiImportsTSdAboutDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSdAboutDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FTitle', System.TypeInfo(string));
    AddField('FProgramIcon', System.TypeInfo(TIcon));
    AddField('FProgramName', System.TypeInfo(string));
    AddField('FVersion', System.TypeInfo(string));
    AddField('FProgramVersion', System.TypeInfo(string));
    AddField('FAuthor', System.TypeInfo(string));
    AddField('FAuthorName', System.TypeInfo(string));
    AddField('FAuthorEMail', System.TypeInfo(string));
    AddField('FWebSite', System.TypeInfo(string));

    AddMethod('SetProgramIcon', @TSepiImportsTSdAboutDialog.SetProgramIcon,
      'procedure(New : TIcon)');
    AddMethod('IsTitleStored', nil,
      'function: boolean');
    AddMethod('IsVersionStored', nil,
      'function: boolean');
    AddMethod('IsProgramVersionStored', nil,
      'function: boolean');
    AddMethod('IsAuthorStored', nil,
      'function: boolean');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTSdAboutDialog.Create,
      'constructor(AOwner : TComponent)',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTSdAboutDialog.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Execute', @TSepiImportsTSdAboutDialog.Execute,
      'procedure');

    CurrentVisibility := mvPublished;

    AddProperty('Title', 'property: string',
      'FTitle', 'FTitle',
      NoIndex, NoDefaultValue, 'IsTitleStored');
    AddProperty('ProgramIcon', 'property: TIcon',
      'FProgramIcon', 'SetProgramIcon');
    AddProperty('ProgramName', 'property: string',
      'FProgramName', 'FProgramName');
    AddProperty('Version', 'property: string',
      'FVersion', 'FVersion',
      NoIndex, NoDefaultValue, 'IsVersionStored');
    AddProperty('ProgramVersion', 'property: string',
      'FProgramVersion', 'FProgramVersion',
      NoIndex, NoDefaultValue, 'IsProgramVersionStored');
    AddProperty('Author', 'property: string',
      'FAuthor', 'FAuthor',
      NoIndex, NoDefaultValue, 'IsAuthorStored');
    AddProperty('AuthorName', 'property: string',
      'FAuthorName', 'FAuthorName');
    AddProperty('AuthorEMail', 'property: string',
      'FAuthorEMail', 'FAuthorEMail');
    AddProperty('WebSite', 'property: string',
      'FWebSite', 'FWebSite');

    Complete;
  end;
end;

{------------------------}
{ TSdNumberDialog import }
{------------------------}

function TSepiImportsTSdNumberDialog.Execute_0(const ATitle, APrompt: string;
  ADefault, AMin, AMax: Integer): Integer;
begin
  Result := Execute(ATitle, APrompt, ADefault, AMin, AMax);
end;

function TSepiImportsTSdNumberDialog.Execute_1(ADefault, AMin, AMax: Integer): Integer;
begin
  Result := Execute(ADefault, AMin, AMax);
end;

function TSepiImportsTSdNumberDialog.Execute_2(const ATitle, APrompt: string;
  AMin, AMax: Integer): Integer;
begin
  Result := Execute(ATitle, APrompt, AMin, AMax);
end;

function TSepiImportsTSdNumberDialog.Execute_3(AMin, AMax: Integer): Integer;
begin
  Result := Execute(AMin, AMax);
end;

function TSepiImportsTSdNumberDialog.Execute_4: Integer;
begin
  Result := Execute;
end;

class function TSepiImportsTSdNumberDialog.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSdNumberDialog));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FTitle', System.TypeInfo(string));
    AddField('FPrompt', System.TypeInfo(string));
    AddField('FValue', System.TypeInfo(Integer));
    AddField('FMin', System.TypeInfo(Integer));
    AddField('FMax', System.TypeInfo(Integer));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTSdNumberDialog.Create,
      'constructor(AOwner : TComponent)',
      mlkOverride);
    AddOverloadedMethod('Execute', @TSepiImportsTSdNumberDialog.Execute_0,
      'function(const ATitle, APrompt : string; ADefault, AMin, AMax : integer ) : integer');
    AddOverloadedMethod('Execute', @TSepiImportsTSdNumberDialog.Execute_1,
      'function(ADefault, AMin, AMax : integer) : integer');
    AddOverloadedMethod('Execute', @TSepiImportsTSdNumberDialog.Execute_2,
      'function(const ATitle, APrompt : string; AMin, AMax : integer ) : integer');
    AddOverloadedMethod('Execute', @TSepiImportsTSdNumberDialog.Execute_3,
      'function(AMin, AMax : integer) : integer');
    AddOverloadedMethod('Execute', @TSepiImportsTSdNumberDialog.Execute_4,
      'function: integer');

    CurrentVisibility := mvPublished;

    AddProperty('Title', 'property: string',
      'FTitle', 'FTitle');
    AddProperty('Prompt', 'property: string',
      'FPrompt', 'FPrompt');
    AddProperty('Value', 'property: integer',
      'FValue', 'FValue');
    AddProperty('Min', 'property: integer',
      'FMin', 'FMin');
    AddProperty('Max', 'property: integer',
      'FMax', 'FMax');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function QueryPassword_0: string;
begin
  Result := QueryPassword;
end;

function QueryPassWord_1(Password: string;
  ShowErrorMes: Boolean = True): Boolean;
begin
  Result := QueryPassWord(Password, ShowErrorMes);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'SdDialogs',
    ['Classes', 'Dialogs', 'Controls', 'Graphics']);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDialogType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDialogButtons));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDialogResult));
  TSepiImportsTSdPasswordDialog.SepiImport(Result);
  TSepiImportsTSdAboutDialog.SepiImport(Result);
  TSepiImportsTSdNumberDialog.SepiImport(Result);

  // Routines
  TSepiMethod.Create(Result, 'ShowMes', @ShowMes,
    'function(const Title, Text : string; Flags : LongWord ) : integer');
  TSepiMethod.Create(Result, 'ShowDialog', @ShowDialog,
    'function(const Title, Text : string; DlgType : TDialogType = dtInformation ; DlgButtons : TDialogButtons = dbOK ; DefButton : Byte = 1 ; AddFlags : LongWord = 0 ) : TDialogResult');
  TSepiMethod.Create(Result, 'ShowDialogRadio', @ShowDialogRadio,
    'function(const Title, Text : string; DlgType : TMsgDlgType; DlgButtons : TMsgDlgButtons ; DefButton : TModalResult ; const RadioTitles : array of string ; var Selected : integer ; OverButtons : boolean = False ) : Word');
  TSepiMethod.CreateOverloaded(Result, 'QueryPassword', @QueryPassword_0,
    'function: string');
  TSepiMethod.CreateOverloaded(Result, 'QueryPassWord', @QueryPassWord_1,
    'function(Password : string; ShowErrorMes : boolean = True ) : boolean');
  TSepiMethod.Create(Result, 'ShowAbout', @ShowAbout,
    'procedure(Title : string; ProgramIcon : TIcon; ProgramName : string; ProgramVersion : string ; Author : string ; AuthorEMail : string = '''' ; WebSite : string = '''' )');
  TSepiMethod.Create(Result, 'QueryNumber', @QueryNumber,
    'function(const Title, Prompt : string; Default, Min, Max : integer ) : integer');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('SdDialogs', ImportUnit);
end.

