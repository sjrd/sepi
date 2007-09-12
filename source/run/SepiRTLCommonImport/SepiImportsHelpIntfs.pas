{*
  Importe l'unité HelpIntfs dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsHelpIntfs;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, HelpIntfs;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEHelpSystemException = class(EHelpSystemException)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{----------------------}
{ IHelpSelector import }
{----------------------}

function SepiImportIHelpSelector(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IHelpSelector));

  with Result do
  begin
    AddMethod('SelectKeyword',
      'function(Keywords: TStrings) : Integer', ccRegister);
    AddMethod('TableOfContents',
      'function(Contents: TStrings): Integer', ccRegister);

    Complete;
  end;
end;

{-----------------------}
{ IHelpSelector2 import }
{-----------------------}

function SepiImportIHelpSelector2(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IHelpSelector2));

  with Result do
  begin
    AddMethod('SelectContext',
      'function(Viewers: TStrings): Integer', ccRegister);

    Complete;
  end;
end;

{--------------------}
{ IHelpSystem import }
{--------------------}

function SepiImportIHelpSystem(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IHelpSystem));

  with Result do
  begin
    AddMethod('ShowHelp',
      'procedure(const HelpKeyword, HelpFileName: String)', ccRegister);
    AddMethod('ShowContextHelp',
      'procedure(const ContextID: Longint; const HelpFileName: String)',
      ccRegister);
    AddMethod('ShowTableOfContents',
      'procedure', ccRegister);
    AddMethod('ShowTopicHelp',
      'procedure(const Topic, HelpFileName: String)', ccRegister);
    AddMethod('AssignHelpSelector',
      'procedure(Selector: IHelpSelector)', ccRegister);
    AddMethod('Hook',
      'function(Handle: Longint; HelpFile: String; Comand: Word; Data: Longint): Boolean',
      ccRegister);

    Complete;
  end;
end;

{---------------------}
{ IHelpSystem2 import }
{---------------------}

function SepiImportIHelpSystem2(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IHelpSystem2));

  with Result do
  begin
    AddMethod('UnderstandsKeyword',
      'function(const HelpKeyword, HelpFileName: String): Boolean',
      ccRegister);

    Complete;
  end;
end;

{--------------------------}
{ ICustomHelpViewer import }
{--------------------------}

function SepiImportICustomHelpViewer(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ICustomHelpViewer));

  with Result do
  begin
    AddMethod('GetViewerName',
      'function: String', ccRegister);
    AddMethod('UnderstandsKeyword',
      'function(const HelpString: String): Integer', ccRegister);
    AddMethod('GetHelpStrings',
      'function(const HelpString: String): TStringList', ccRegister);
    AddMethod('CanShowTableOfContents',
      'function: Boolean', ccRegister);
    AddMethod('ShowTableOfContents',
      'procedure', ccRegister);
    AddMethod('ShowHelp',
      'procedure(const HelpString: String)', ccRegister);
    AddMethod('NotifyID',
      'procedure(const ViewerID: Integer)', ccRegister);
    AddMethod('SoftShutDown',
      'procedure', ccRegister);
    AddMethod('ShutDown',
      'procedure', ccRegister);

    Complete;
  end;
end;

{----------------------------}
{ IExtendedHelpViewer import }
{----------------------------}

function SepiImportIExtendedHelpViewer(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IExtendedHelpViewer));

  with Result do
  begin
    AddMethod('UnderstandsTopic',
      'function(const Topic: String): Boolean', ccRegister);
    AddMethod('DisplayTopic',
      'procedure(const Topic: String)', ccRegister);
    AddMethod('UnderstandsContext',
      'function(const ContextID: Integer; const HelpFileName: String ) : Boolean',
      ccRegister);
    AddMethod('DisplayHelpByContext',
      'procedure(const ContextID: Integer; const HelpFileName: String )',
      ccRegister);

    Complete;
  end;
end;

{------------------------------}
{ ISpecialWinHelpViewer import }
{------------------------------}

function SepiImportISpecialWinHelpViewer(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(ISpecialWinHelpViewer));

  with Result do
  begin
    AddMethod('CallWinHelp',
      'function(Handle: LongInt; const HelpFile: String; Command: Word; Data: LongInt ) : Boolean',
      ccRegister);

    Complete;
  end;
end;

{-------------------------}
{ IHelpSystemFlags import }
{-------------------------}

function SepiImportIHelpSystemFlags(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IHelpSystemFlags));

  with Result do
  begin
    AddMethod('GetUseDefaultTopic',
      'function: Boolean', ccRegister);
    AddMethod('SetUseDefaultTopic',
      'procedure(AValue: Boolean)', ccRegister);

    Complete;
  end;
end;

{---------------------}
{ IHelpManager import }
{---------------------}

function SepiImportIHelpManager(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IHelpManager));

  with Result do
  begin
    AddMethod('GetHandle',
      'function: LongInt', ccRegister);
    AddMethod('GetHelpFile',
      'function: String', ccRegister);
    AddMethod('Release',
      'procedure(const ViewerID: Integer)', ccRegister);

    Complete;
  end;
end;

{-----------------------------}
{ EHelpSystemException import }
{-----------------------------}

class function TSepiImportsEHelpSystemException.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EHelpSystemException));

  with Result do
  begin

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function GetHelpSystem_0(out System: IHelpSystem): Boolean;
begin
  Result := GetHelpSystem(System);
end;

function GetHelpSystem_1(out System: IHelpSystem2): Boolean;
begin
  Result := GetHelpSystem(System);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'HelpIntfs',
    ['SysUtils', 'Classes']);

  // Types
  SepiImportIHelpSelector(Result);
  SepiImportIHelpSelector2(Result);
  SepiImportIHelpSystem(Result);
  SepiImportIHelpSystem2(Result);
  SepiImportICustomHelpViewer(Result);
  SepiImportIExtendedHelpViewer(Result);
  SepiImportISpecialWinHelpViewer(Result);
  SepiImportIHelpSystemFlags(Result);
  SepiImportIHelpManager(Result);
  TSepiImportsEHelpSystemException.SepiImport(Result);

  // Routines
  TSepiMethod.Create(Result, 'RegisterViewer', @RegisterViewer,
    'function(const newViewer: ICustomHelpViewer; out Manager: IHelpManager ) : Integer');
  TSepiOverloadedMethod.Create(Result, 'GetHelpSystem');
  TSepiMethod.Create(Result, 'OL$GetHelpSystem$0', @GetHelpSystem_0,
    'function(out System: IHelpSystem): Boolean');
  TSepiMethod.Create(Result, 'OL$GetHelpSystem$1', @GetHelpSystem_1,
    'function(out System: IHelpSystem2): Boolean');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('HelpIntfs', ImportUnit);
end.

