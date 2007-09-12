{*
  Importe l'unité SysUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsSysUtils;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, SysUtils;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TUnnamed_8 = array of TLangRec;

  TSepiImportsTLanguages = class(TLanguages)
  private
    function GetExt(Index: Integer): string;
    function GetID(Index: Integer): string;
    function GetLCID(Index: Integer): LCID;
    function GetName(Index: Integer): string;
    function GetNameFromLocaleID(ID: LCID): string;
    function GetNameFromLCID(const ID: string): string;
    function GetCount: Integer;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsException = class(Exception)
  private
    constructor CreateRes_0(Ident: Integer);
    constructor CreateRes_1(ResStringRec: PResStringRec);
    constructor CreateResFmt_0(Ident: Integer; const Args: array of const);
    constructor CreateResFmt_1(ResStringRec: PResStringRec;
      const Args: array of const);
    constructor CreateResHelp_0(Ident: Integer; AHelpContext: Integer);
    constructor CreateResHelp_1(ResStringRec: PResStringRec;
      AHelpContext: Integer);
    constructor CreateResFmtHelp_0(ResStringRec: PResStringRec;
      const Args: array of const; AHelpContext: Integer);
    constructor CreateResFmtHelp_1(Ident: Integer;
      const Args: array of const; AHelpContext: Integer);
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEAbort = class(EAbort)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEHeapException = class(EHeapException)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEOutOfMemory = class(EOutOfMemory)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEInOutError = class(EInOutError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEExternal = class(EExternal)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEExternalException = class(EExternalException)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEIntError = class(EIntError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEDivByZero = class(EDivByZero)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsERangeError = class(ERangeError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEIntOverflow = class(EIntOverflow)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEMathError = class(EMathError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEInvalidOp = class(EInvalidOp)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEZeroDivide = class(EZeroDivide)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEOverflow = class(EOverflow)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEUnderflow = class(EUnderflow)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEInvalidPointer = class(EInvalidPointer)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEInvalidCast = class(EInvalidCast)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEConvertError = class(EConvertError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEAccessViolation = class(EAccessViolation)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEPrivilege = class(EPrivilege)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEControlC = class(EControlC)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEVariantError = class(EVariantError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEPropReadOnly = class(EPropReadOnly)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEPropWriteOnly = class(EPropWriteOnly)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEAssertionFailed = class(EAssertionFailed)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEAbstractError = class(EAbstractError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEIntfCastError = class(EIntfCastError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEInvalidContainer = class(EInvalidContainer)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEInvalidInsert = class(EInvalidInsert)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEPackageError = class(EPackageError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEOSError = class(EOSError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsESafecallException = class(ESafecallException)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TUnnamed_20 = array of String;

  TUnnamed_21 = array of String;

  TUnnamed_23 = set of Char;

  TSepiImportsTSimpleRWSync = class(TSimpleRWSync)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTThreadLocalCounter = class(TThreadLocalCounter)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTMultiReadExclusiveWriteSynchronizer =
    class(TMultiReadExclusiveWriteSynchronizer)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{----------------}
{ WordRec import }
{----------------}

function SepiImportWordRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'WordRec', True, True);

  with Result do
  begin
    AddFieldAfter('Lo', System.TypeInfo(Byte), '');
    AddField('Hi', System.TypeInfo(Byte), True);
    AddFieldAfter('Bytes', '$1', '');

    Complete;
  end;
end;

{----------------}
{ LongRec import }
{----------------}

function SepiImportLongRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'LongRec', True, True);

  with Result do
  begin
    AddFieldAfter('Lo', System.TypeInfo(Word), '');
    AddField('Hi', System.TypeInfo(Word), True);
    AddFieldAfter('Words', '$2', '');
    AddFieldAfter('Bytes', '$3', '');

    Complete;
  end;
end;

{-----------------}
{ Int64Rec import }
{-----------------}

function SepiImportInt64Rec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'Int64Rec', True, True);

  with Result do
  begin
    AddFieldAfter('Lo', System.TypeInfo(Cardinal), '');
    AddField('Hi', System.TypeInfo(Cardinal), True);
    AddFieldAfter('Cardinals', '$4', '');
    AddFieldAfter('Words', '$5', '');
    AddFieldAfter('Bytes', '$6', '');

    Complete;
  end;
end;

{-------------------}
{ TSearchRec import }
{-------------------}

function SepiImportTSearchRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TSearchRec', False, True,
    TypeInfo(TSearchRec));

  with Result do
  begin
    AddField('Time', System.TypeInfo(Integer));
    AddField('Size', System.TypeInfo(Integer));
    AddField('Attr', System.TypeInfo(Integer));
    AddField('Name', System.TypeInfo(TFileName));
    AddField('ExcludeAttr', System.TypeInfo(Integer));
    AddField('FindHandle', System.TypeInfo(THandle));
    AddField('FindData', 'TWin32FindData');

    Complete;
  end;
end;

{------------------}
{ TFloatRec import }
{------------------}

function SepiImportTFloatRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TFloatRec', True, True);

  with Result do
  begin
    AddField('Exponent', System.TypeInfo(Smallint));
    AddField('Negative', System.TypeInfo(Boolean));
    AddField('Digits', '$7');

    Complete;
  end;
end;

{-------------------}
{ TTimeStamp import }
{-------------------}

function SepiImportTTimeStamp(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TTimeStamp', False, True);

  with Result do
  begin
    AddField('Time', System.TypeInfo(Integer));
    AddField('Date', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------}
{ TSysLocale import }
{-------------------}

function SepiImportTSysLocale(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TSysLocale', True, True);

  with Result do
  begin
    AddField('DefaultLCID', System.TypeInfo(Integer));
    AddField('PriLangID', System.TypeInfo(Integer));
    AddField('SubLangID', System.TypeInfo(Integer));
    AddField('FarEast', System.TypeInfo(Boolean));
    AddField('MiddleEast', System.TypeInfo(Boolean));

    Complete;
  end;
end;

{-----------------}
{ TLangRec import }
{-----------------}

function SepiImportTLangRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TLangRec', True, True,
    TypeInfo(TLangRec));

  with Result do
  begin
    AddField('FName', System.TypeInfo(string));
    AddField('FLCID', System.TypeInfo(LCID));
    AddField('FExt', System.TypeInfo(string));

    Complete;
  end;
end;

{-------------------}
{ TLanguages import }
{-------------------}

function TSepiImportsTLanguages.GetExt(Index: Integer): string;
begin
  Result := Ext[Index];
end;

function TSepiImportsTLanguages.GetID(Index: Integer): string;
begin
  Result := ID[Index];
end;

function TSepiImportsTLanguages.GetLCID(Index: Integer): LCID;
begin
  Result := LocaleID[Index];
end;

function TSepiImportsTLanguages.GetName(Index: Integer): string;
begin
  Result := Name[Index];
end;

function TSepiImportsTLanguages.GetNameFromLocaleID(ID: LCID): string;
begin
  Result := NameFromLocaleID[ID];
end;

function TSepiImportsTLanguages.GetNameFromLCID(const ID: string): string;
begin
  Result := NameFromLCID[ID];
end;

function TSepiImportsTLanguages.GetCount: Integer;
begin
  Result := Count;
end;

class function TSepiImportsTLanguages.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TLanguages));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FSysLangs', System.TypeInfo(TUnnamed_8));

    AddMethod('LocalesCallback', nil,
      'function(LocaleID: PChar): Integer',
      ccStdCall);
    AddMethod('GetExt', @TSepiImportsTLanguages.GetExt,
      'function(Index: Integer): string');
    AddMethod('GetID', @TSepiImportsTLanguages.GetID,
      'function(Index: Integer): string');
    AddMethod('GetLCID', @TSepiImportsTLanguages.GetLCID,
      'function(Index: Integer): LCID');
    AddMethod('GetName', @TSepiImportsTLanguages.GetName,
      'function(Index: Integer): string');
    AddMethod('GetNameFromLocaleID',
      @TSepiImportsTLanguages.GetNameFromLocaleID,
      'function(ID: LCID): string');
    AddMethod('GetNameFromLCID', @TSepiImportsTLanguages.GetNameFromLCID,
      'function(const ID: string): string');
    AddMethod('GetCount', @TSepiImportsTLanguages.GetCount,
      'function: integer');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTLanguages.Create,
      'constructor');
    AddMethod('IndexOf', @TSepiImportsTLanguages.IndexOf,
      'function(ID: LCID): Integer');

    AddProperty('Count', 'property: Integer',
      'GetCount', '');
    AddProperty('Name', 'property[Index: Integer]: string',
      'GetName', '');
    AddProperty('NameFromLocaleID', 'property[ID: LCID]: string',
      'GetNameFromLocaleID', '');
    AddProperty('NameFromLCID', 'property[const ID: string]: string',
      'GetNameFromLCID', '');
    AddProperty('ID', 'property[Index: Integer]: string',
      'GetID', '');
    AddProperty('LocaleID', 'property[Index: Integer]: LCID',
      'GetLCID', '');
    AddProperty('Ext', 'property[Index: Integer]: string',
      'GetExt', '');

    Complete;
  end;
end;

{------------------}
{ Exception import }
{------------------}

constructor TSepiImportsException.CreateRes_0(Ident: Integer);
begin
  CreateRes(Ident);
end;

constructor TSepiImportsException.CreateRes_1(ResStringRec: PResStringRec);
begin
  CreateRes(ResStringRec);
end;

constructor TSepiImportsException.CreateResFmt_0(Ident: Integer;
  const Args: array of const);
begin
  CreateResFmt(Ident, Args);
end;

constructor TSepiImportsException.CreateResFmt_1(ResStringRec: PResStringRec;
  const Args: array of const);
begin
  CreateResFmt(ResStringRec, Args);
end;

constructor TSepiImportsException.CreateResHelp_0(Ident: Integer;
  AHelpContext: Integer);
begin
  CreateResHelp(Ident, AHelpContext);
end;

constructor TSepiImportsException.CreateResHelp_1(ResStringRec: PResStringRec;
  AHelpContext: Integer);
begin
  CreateResHelp(ResStringRec, AHelpContext);
end;

constructor TSepiImportsException.CreateResFmtHelp_0(
  ResStringRec: PResStringRec;
  const Args: array of const; AHelpContext: Integer);
begin
  CreateResFmtHelp(ResStringRec, Args, AHelpContext);
end;

constructor TSepiImportsException.CreateResFmtHelp_1(Ident: Integer;
  const Args: array of const; AHelpContext: Integer);
begin
  CreateResFmtHelp(Ident, Args, AHelpContext);
end;

class function TSepiImportsException.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(Exception));

  with Result do
  begin
    TSepiOverloadedMethod.Create(Result, 'CreateRes');
    TSepiOverloadedMethod.Create(Result, 'CreateResFmt');
    TSepiOverloadedMethod.Create(Result, 'CreateResHelp');
    TSepiOverloadedMethod.Create(Result, 'CreateResFmtHelp');
    CurrentVisibility := mvPrivate;

    AddField('FMessage', System.TypeInfo(string));
    AddField('FHelpContext', System.TypeInfo(Integer));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsException.Create,
      'constructor(const Msg: string)');
    AddMethod('CreateFmt', @TSepiImportsException.CreateFmt,
      'constructor(const Msg: string; const Args: array of const)');
    AddMethod('OL$CreateRes$0', @TSepiImportsException.CreateRes_0,
      'constructor(Ident: Integer)');
    AddMethod('OL$CreateRes$1', @TSepiImportsException.CreateRes_1,
      'constructor(ResStringRec: PResStringRec)');
    AddMethod('OL$CreateResFmt$0', @TSepiImportsException.CreateResFmt_0,
      'constructor(Ident: Integer; const Args: array of const)');
    AddMethod('OL$CreateResFmt$1', @TSepiImportsException.CreateResFmt_1,
      'constructor(ResStringRec: PResStringRec; const Args: array of const)');
    AddMethod('CreateHelp', @TSepiImportsException.CreateHelp,
      'constructor(const Msg: string; AHelpContext: Integer)');
    AddMethod('CreateFmtHelp', @TSepiImportsException.CreateFmtHelp,
      'constructor(const Msg: string; const Args: array of const; AHelpContext: Integer )');
    AddMethod('OL$CreateResHelp$0', @TSepiImportsException.CreateResHelp_0,
      'constructor(Ident: Integer; AHelpContext: Integer)');
    AddMethod('OL$CreateResHelp$1', @TSepiImportsException.CreateResHelp_1,
      'constructor(ResStringRec: PResStringRec; AHelpContext: Integer)');
    AddMethod('OL$CreateResFmtHelp$0',
      @TSepiImportsException.CreateResFmtHelp_0,
      'constructor(ResStringRec: PResStringRec; const Args: array of const; AHelpContext: Integer )');
    AddMethod('OL$CreateResFmtHelp$1',
      @TSepiImportsException.CreateResFmtHelp_1,
      'constructor(Ident: Integer; const Args: array of const; AHelpContext: Integer )');

    AddProperty('HelpContext', 'property: Integer',
      'FHelpContext', 'FHelpContext');
    AddProperty('Message', 'property: string',
      'FMessage', 'FMessage');

    Complete;
  end;
end;

{---------------}
{ EAbort import }
{---------------}

class function TSepiImportsEAbort.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EAbort));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ EHeapException import }
{-----------------------}

class function TSepiImportsEHeapException.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EHeapException));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('AllowFree', System.TypeInfo(Boolean));

    CurrentVisibility := mvPublic;

    AddMethod('FreeInstance', @TSepiImportsEHeapException.FreeInstance,
      'procedure',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ EOutOfMemory import }
{---------------------}

class function TSepiImportsEOutOfMemory.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOutOfMemory));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------}
{ EInOutError import }
{--------------------}

class function TSepiImportsEInOutError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInOutError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('ErrorCode', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------------------}
{ TExceptionRecord import }
{-------------------------}

function SepiImportTExceptionRecord(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TExceptionRecord', False, True);

  with Result do
  begin
    AddField('ExceptionCode', System.TypeInfo(Cardinal));
    AddField('ExceptionFlags', System.TypeInfo(Cardinal));
    AddField('ExceptionRecord', 'PExceptionRecord');
    AddField('ExceptionAddress', 'Pointer');
    AddField('NumberParameters', System.TypeInfo(Cardinal));
    AddField('ExceptionInformation', '$9');

    Complete;
  end;
end;

{------------------}
{ EExternal import }
{------------------}

class function TSepiImportsEExternal.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EExternal));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('ExceptionRecord', 'PExceptionRecord');

    Complete;
  end;
end;

{---------------------------}
{ EExternalException import }
{---------------------------}

class function TSepiImportsEExternalException.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EExternalException));

  with Result do
  begin

    Complete;
  end;
end;

{------------------}
{ EIntError import }
{------------------}

class function TSepiImportsEIntError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EIntError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------}
{ EDivByZero import }
{-------------------}

class function TSepiImportsEDivByZero.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EDivByZero));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------}
{ ERangeError import }
{--------------------}

class function TSepiImportsERangeError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ERangeError));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------}
{ EIntOverflow import }
{---------------------}

class function TSepiImportsEIntOverflow.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EIntOverflow));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------}
{ EMathError import }
{-------------------}

class function TSepiImportsEMathError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EMathError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------}
{ EInvalidOp import }
{-------------------}

class function TSepiImportsEInvalidOp.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidOp));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------}
{ EZeroDivide import }
{--------------------}

class function TSepiImportsEZeroDivide.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EZeroDivide));

  with Result do
  begin

    Complete;
  end;
end;

{------------------}
{ EOverflow import }
{------------------}

class function TSepiImportsEOverflow.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOverflow));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------}
{ EUnderflow import }
{-------------------}

class function TSepiImportsEUnderflow.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EUnderflow));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ EInvalidPointer import }
{------------------------}

class function TSepiImportsEInvalidPointer.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidPointer));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------}
{ EInvalidCast import }
{---------------------}

class function TSepiImportsEInvalidCast.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidCast));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------}
{ EConvertError import }
{----------------------}

class function TSepiImportsEConvertError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EConvertError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------------}
{ EAccessViolation import }
{-------------------------}

class function TSepiImportsEAccessViolation.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EAccessViolation));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------}
{ EPrivilege import }
{-------------------}

class function TSepiImportsEPrivilege.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EPrivilege));

  with Result do
  begin

    Complete;
  end;
end;

{------------------}
{ EControlC import }
{------------------}

class function TSepiImportsEControlC.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EControlC));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------}
{ EVariantError import }
{----------------------}

class function TSepiImportsEVariantError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EVariantError));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------}
{ EPropReadOnly import }
{----------------------}

class function TSepiImportsEPropReadOnly.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EPropReadOnly));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ EPropWriteOnly import }
{-----------------------}

class function TSepiImportsEPropWriteOnly.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EPropWriteOnly));

  with Result do
  begin

    Complete;
  end;
end;

{-------------------------}
{ EAssertionFailed import }
{-------------------------}

class function TSepiImportsEAssertionFailed.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EAssertionFailed));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ EAbstractError import }
{-----------------------}

class function TSepiImportsEAbstractError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EAbstractError));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ EIntfCastError import }
{-----------------------}

class function TSepiImportsEIntfCastError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EIntfCastError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------------}
{ EInvalidContainer import }
{--------------------------}

class function TSepiImportsEInvalidContainer.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidContainer));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ EInvalidInsert import }
{-----------------------}

class function TSepiImportsEInvalidInsert.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EInvalidInsert));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------}
{ EPackageError import }
{----------------------}

class function TSepiImportsEPackageError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EPackageError));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------}
{ EOSError import }
{-----------------}

class function TSepiImportsEOSError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOSError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('ErrorCode', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{---------------------------}
{ ESafecallException import }
{---------------------------}

class function TSepiImportsESafecallException.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ESafecallException));

  with Result do
  begin

    Complete;
  end;
end;

{------------------------}
{ TFormatSettings import }
{------------------------}

function SepiImportTFormatSettings(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TFormatSettings', False, True,
    TypeInfo(TFormatSettings));

  with Result do
  begin
    AddField('CurrencyFormat', System.TypeInfo(Byte));
    AddField('NegCurrFormat', System.TypeInfo(Byte));
    AddField('ThousandSeparator', System.TypeInfo(Char));
    AddField('DecimalSeparator', System.TypeInfo(Char));
    AddField('CurrencyDecimals', System.TypeInfo(Byte));
    AddField('DateSeparator', System.TypeInfo(Char));
    AddField('TimeSeparator', System.TypeInfo(Char));
    AddField('ListSeparator', System.TypeInfo(Char));
    AddField('CurrencyString', System.TypeInfo(string));
    AddField('ShortDateFormat', System.TypeInfo(string));
    AddField('LongDateFormat', System.TypeInfo(string));
    AddField('TimeAMString', System.TypeInfo(string));
    AddField('TimePMString', System.TypeInfo(string));
    AddField('ShortTimeFormat', System.TypeInfo(string));
    AddField('LongTimeFormat', System.TypeInfo(string));
    AddField('ShortMonthNames', '$14');
    AddField('LongMonthNames', '$15');
    AddField('ShortDayNames', '$16');
    AddField('LongDayNames', '$17');
    AddField('TwoDigitYearCenturyWindow', System.TypeInfo(Word));

    Complete;
  end;
end;

{-----------------------}
{ IReadWriteSync import }
{-----------------------}

function SepiImportIReadWriteSync(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IReadWriteSync));

  with Result do
  begin
    AddMethod('BeginRead',
      'procedure', ccRegister);
    AddMethod('EndRead',
      'procedure', ccRegister);
    AddMethod('BeginWrite',
      'function: Boolean', ccRegister);
    AddMethod('EndWrite',
      'procedure', ccRegister);

    Complete;
  end;
end;

{----------------------}
{ TSimpleRWSync import }
{----------------------}

class function TSepiImportsTSimpleRWSync.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSimpleRWSync));

  with Result do
  begin
    AddInterface(System.TypeInfo(IReadWriteSync));

    CurrentVisibility := mvPrivate;

    AddField('FLock', 'TRTLCriticalSection');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTSimpleRWSync.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTSimpleRWSync.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('BeginRead', @TSepiImportsTSimpleRWSync.BeginRead,
      'procedure');
    AddMethod('EndRead', @TSepiImportsTSimpleRWSync.EndRead,
      'procedure');
    AddMethod('BeginWrite', @TSepiImportsTSimpleRWSync.BeginWrite,
      'function: Boolean');
    AddMethod('EndWrite', @TSepiImportsTSimpleRWSync.EndWrite,
      'procedure');

    Complete;
  end;
end;

{--------------------}
{ TThreadInfo import }
{--------------------}

function SepiImportTThreadInfo(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TThreadInfo', False, True);

  with Result do
  begin
    AddField('Next', 'PThreadInfo');
    AddField('ThreadID', System.TypeInfo(Cardinal));
    AddField('Active', System.TypeInfo(Integer));
    AddField('RecursionCount', System.TypeInfo(Cardinal));

    Complete;
  end;
end;

{----------------------------}
{ TThreadLocalCounter import }
{----------------------------}

class function TSepiImportsTThreadLocalCounter.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TThreadLocalCounter));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHashTable', '$25');

    AddMethod('HashIndex', nil,
      'function: Byte');
    AddMethod('Recycle', nil,
      'function: PThreadInfo');

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTThreadLocalCounter.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Open', @TSepiImportsTThreadLocalCounter.Open,
      'procedure(var Thread: PThreadInfo)');
    AddMethod('Delete', @TSepiImportsTThreadLocalCounter.Delete,
      'procedure(var Thread: PThreadInfo)');
    AddMethod('Close', @TSepiImportsTThreadLocalCounter.Close,
      'procedure(var Thread: PThreadInfo)');

    Complete;
  end;
end;

{---------------------------------------------}
{ TMultiReadExclusiveWriteSynchronizer import }
{---------------------------------------------}

class function TSepiImportsTMultiReadExclusiveWriteSynchronizer.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMultiReadExclusiveWriteSynchronizer));

  with Result do
  begin
    AddInterface(System.TypeInfo(IReadWriteSync));

    CurrentVisibility := mvPrivate;

    AddField('FSentinel', System.TypeInfo(Integer));
    AddField('FReadSignal', System.TypeInfo(THandle));
    AddField('FWriteSignal', System.TypeInfo(THandle));
    AddField('FWaitRecycle', System.TypeInfo(Cardinal));
    AddField('FWriteRecursionCount', System.TypeInfo(Cardinal));
    AddField('tls', System.TypeInfo(TThreadLocalCounter));
    AddField('FWriterID', System.TypeInfo(Cardinal));
    AddField('FRevisionLevel', System.TypeInfo(Cardinal));

    AddMethod('BlockReaders', nil,
      'procedure');
    AddMethod('UnblockReaders', nil,
      'procedure');
    AddMethod('UnblockOneWriter', nil,
      'procedure');
    AddMethod('WaitForReadSignal', nil,
      'procedure');
    AddMethod('WaitForWriteSignal', nil,
      'procedure');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMultiReadExclusiveWriteSynchronizer.Create,
      'constructor');
    AddMethod('Destroy',
      @TSepiImportsTMultiReadExclusiveWriteSynchronizer.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('BeginRead',
      @TSepiImportsTMultiReadExclusiveWriteSynchronizer.BeginRead,
      'procedure');
    AddMethod('EndRead',
      @TSepiImportsTMultiReadExclusiveWriteSynchronizer.EndRead,
      'procedure');
    AddMethod('BeginWrite',
      @TSepiImportsTMultiReadExclusiveWriteSynchronizer.BeginWrite,
      'function: Boolean');
    AddMethod('EndWrite',
      @TSepiImportsTMultiReadExclusiveWriteSynchronizer.EndWrite,
      'procedure');

    AddProperty('RevisionLevel', 'property: Cardinal',
      'FRevisionLevel', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function UpperCase_0(const S: string): string;
begin
  Result := UpperCase(S);
end;

function UpperCase_1(const S: string; LocaleOptions: TLocaleOptions): string;
begin
  Result := UpperCase(S, LocaleOptions);
end;

function LowerCase_0(const S: string): string;
begin
  Result := LowerCase(S);
end;

function LowerCase_1(const S: string; LocaleOptions: TLocaleOptions): string;
begin
  Result := LowerCase(S, LocaleOptions);
end;

function CompareStr_0(const S1, S2: string): Integer;
begin
  Result := CompareStr(S1, S2);
end;

function CompareStr_1(const S1, S2: string;
  LocaleOptions: TLocaleOptions): Integer;
begin
  Result := CompareStr(S1, S2, LocaleOptions);
end;

function SameStr_0(const S1, S2: string): Boolean;
begin
  Result := SameStr(S1, S2);
end;

function SameStr_1(const S1, S2: string;
  LocaleOptions: TLocaleOptions): Boolean;
begin
  Result := SameStr(S1, S2, LocaleOptions);
end;

function CompareText_0(const S1, S2: string): Integer;
begin
  Result := CompareText(S1, S2);
end;

function CompareText_1(const S1, S2: string;
  LocaleOptions: TLocaleOptions): Integer;
begin
  Result := CompareText(S1, S2, LocaleOptions);
end;

function SameText_0(const S1, S2: string): Boolean;
begin
  Result := SameText(S1, S2);
end;

function SameText_1(const S1, S2: string;
  LocaleOptions: TLocaleOptions): Boolean;
begin
  Result := SameText(S1, S2, LocaleOptions);
end;

function Trim_0(const S: string): string;
begin
  Result := Trim(S);
end;

function Trim_1(const S: WideString): WideString;
begin
  Result := Trim(S);
end;

function TrimLeft_0(const S: string): string;
begin
  Result := TrimLeft(S);
end;

function TrimLeft_1(const S: WideString): WideString;
begin
  Result := TrimLeft(S);
end;

function TrimRight_0(const S: string): string;
begin
  Result := TrimRight(S);
end;

function TrimRight_1(const S: WideString): WideString;
begin
  Result := TrimRight(S);
end;

function IntToStr_0(Value: Integer): string;
begin
  Result := IntToStr(Value);
end;

function IntToStr_1(Value: Int64): string;
begin
  Result := IntToStr(Value);
end;

function IntToHex_0(Value: Integer; Digits: Integer): string;
begin
  Result := IntToHex(Value, Digits);
end;

function IntToHex_1(Value: Int64; Digits: Integer): string;
begin
  Result := IntToHex(Value, Digits);
end;

function FileCreate_0(const FileName: string): Integer;
begin
  Result := FileCreate(FileName);
end;

function FileCreate_1(const FileName: string; Rights: Integer): Integer;
begin
  Result := FileCreate(FileName, Rights);
end;

function FileSeek_0(Handle, Offset, Origin: Integer): Integer;
begin
  Result := FileSeek(Handle, Offset, Origin);
end;

function FileSeek_1(Handle: Integer; const Offset: Int64;
  Origin: Integer): Int64;
begin
  Result := FileSeek(Handle, Offset, Origin);
end;

function FileSetDate_0(const FileName: string; Age: Integer): Integer;
begin
  Result := FileSetDate(FileName, Age);
end;

function FileSetDate_1(Handle: Integer; Age: Integer): Integer;
begin
  Result := FileSetDate(Handle, Age);
end;

function Format_0(const Format: string; const Args: array of const): string;
begin
  Result := SysUtils.Format(Format, Args);
end;

function Format_1(const Format: string; const Args: array of const;
  const FormatSettings: TFormatSettings): string;
begin
  Result := SysUtils.Format(Format, Args, FormatSettings);
end;

procedure FmtStr_0(var Result: string; const Format: string;
  const Args: array of const);
begin
  FmtStr(Result, Format, Args);
end;

procedure FmtStr_1(var Result: string; const Format: string;
  const Args: array of const; const FormatSettings: TFormatSettings);
begin
  FmtStr(Result, Format, Args, FormatSettings);
end;

function StrFmt_0(Buffer, Format: PChar; const Args: array of const): PChar;
begin
  Result := StrFmt(Buffer, Format, Args);
end;

function StrFmt_1(Buffer, Format: PChar; const Args: array of const;
  const FormatSettings: TFormatSettings): PChar;
begin
  Result := StrFmt(Buffer, Format, Args, FormatSettings);
end;

function StrLFmt_0(Buffer: PChar; MaxBufLen: Cardinal;
  Format: PChar; const Args: array of const): PChar;
begin
  Result := StrLFmt(Buffer, MaxBufLen, Format, Args);
end;

function StrLFmt_1(Buffer: PChar; MaxBufLen: Cardinal;
  Format: PChar; const Args: array of const;
  const FormatSettings: TFormatSettings): PChar;
begin
  Result := StrLFmt(Buffer, MaxBufLen, Format, Args, FormatSettings);
end;

function FormatBuf_0(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;
begin
  Result := FormatBuf(Buffer, BufLen, Format, FmtLen, Args);
end;

function FormatBuf_1(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const FormatSettings: TFormatSettings): Cardinal;
begin
  Result := FormatBuf(Buffer, BufLen, Format, FmtLen, Args, FormatSettings);
end;

function WideFormat_0(const Format: WideString;
  const Args: array of const): WideString;
begin
  Result := WideFormat(Format, Args);
end;

function WideFormat_1(const Format: WideString; const Args: array of const;
  const FormatSettings: TFormatSettings): WideString;
begin
  Result := WideFormat(Format, Args, FormatSettings);
end;

procedure WideFmtStr_0(var Result: WideString; const Format: WideString;
  const Args: array of const);
begin
  WideFmtStr(Result, Format, Args);
end;

procedure WideFmtStr_1(var Result: WideString; const Format: WideString;
  const Args: array of const; const FormatSettings: TFormatSettings);
begin
  WideFmtStr(Result, Format, Args, FormatSettings);
end;

function WideFormatBuf_0(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;
begin
  Result := WideFormatBuf(Buffer, BufLen, Format, FmtLen, Args);
end;

function WideFormatBuf_1(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const FormatSettings: TFormatSettings): Cardinal;
begin
  Result := WideFormatBuf(Buffer, BufLen, Format, FmtLen, Args,
    FormatSettings);
end;

function FloatToStr_0(Value: Extended): string;
begin
  Result := FloatToStr(Value);
end;

function FloatToStr_1(Value: Extended;
  const FormatSettings: TFormatSettings): string;
begin
  Result := FloatToStr(Value, FormatSettings);
end;

function CurrToStr_0(Value: Currency): string;
begin
  Result := CurrToStr(Value);
end;

function CurrToStr_1(Value: Currency;
  const FormatSettings: TFormatSettings): string;
begin
  Result := CurrToStr(Value, FormatSettings);
end;

function FloatToStrF_0(Value: Extended; Format: TFloatFormat;
  Precision, Digits: Integer): string;
begin
  Result := FloatToStrF(Value, Format, Precision, Digits);
end;

function FloatToStrF_1(Value: Extended; Format: TFloatFormat;
  Precision, Digits: Integer; const FormatSettings: TFormatSettings): string;
begin
  Result := FloatToStrF(Value, Format, Precision, Digits, FormatSettings);
end;

function CurrToStrF_0(Value: Currency; Format: TFloatFormat;
  Digits: Integer): string;
begin
  Result := CurrToStrF(Value, Format, Digits);
end;

function CurrToStrF_1(Value: Currency; Format: TFloatFormat;
  Digits: Integer; const FormatSettings: TFormatSettings): string;
begin
  Result := CurrToStrF(Value, Format, Digits, FormatSettings);
end;

function FloatToText_0(BufferArg: PChar; const Value;
  ValueType: TFloatValue; Format: TFloatFormat;
  Precision, Digits: Integer): Integer;
begin
  Result := FloatToText(BufferArg, Value, ValueType, Format,
    Precision, Digits);
end;

function FloatToText_1(BufferArg: PChar; const Value;
  ValueType: TFloatValue; Format: TFloatFormat; Precision, Digits: Integer;
  const FormatSettings: TFormatSettings): Integer;
begin
  Result := FloatToText(BufferArg, Value, ValueType, Format,
    Precision, Digits, FormatSettings);
end;

function FormatFloat_0(const Format: string; Value: Extended): string;
begin
  Result := FormatFloat(Format, Value);
end;

function FormatFloat_1(const Format: string; Value: Extended;
  const FormatSettings: TFormatSettings): string;
begin
  Result := FormatFloat(Format, Value, FormatSettings);
end;

function FormatCurr_0(const Format: string; Value: Currency): string;
begin
  Result := FormatCurr(Format, Value);
end;

function FormatCurr_1(const Format: string; Value: Currency;
  const FormatSettings: TFormatSettings): string;
begin
  Result := FormatCurr(Format, Value, FormatSettings);
end;

function FloatToTextFmt_0(Buf: PChar; const Value;
  ValueType: TFloatValue; Format: PChar): Integer;
begin
  Result := FloatToTextFmt(Buf, Value, ValueType, Format);
end;

function FloatToTextFmt_1(Buf: PChar; const Value;
  ValueType: TFloatValue; Format: PChar;
  const FormatSettings: TFormatSettings): Integer;
begin
  Result := FloatToTextFmt(Buf, Value, ValueType, Format, FormatSettings);
end;

function StrToFloat_0(const S: string): Extended;
begin
  Result := StrToFloat(S);
end;

function StrToFloat_1(const S: string;
  const FormatSettings: TFormatSettings): Extended;
begin
  Result := StrToFloat(S, FormatSettings);
end;

function StrToFloatDef_0(const S: string; const Default: Extended): Extended;
begin
  Result := StrToFloatDef(S, Default);
end;

function StrToFloatDef_1(const S: string; const Default: Extended;
  const FormatSettings: TFormatSettings): Extended;
begin
  Result := StrToFloatDef(S, Default, FormatSettings);
end;

function TryStrToFloat_0(const S: string; out Value: Extended): Boolean;
begin
  Result := TryStrToFloat(S, Value);
end;

function TryStrToFloat_1(const S: string; out Value: Extended;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToFloat(S, Value, FormatSettings);
end;

function TryStrToFloat_2(const S: string; out Value: Double): Boolean;
begin
  Result := TryStrToFloat(S, Value);
end;

function TryStrToFloat_3(const S: string; out Value: Double;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToFloat(S, Value, FormatSettings);
end;

function TryStrToFloat_4(const S: string; out Value: Single): Boolean;
begin
  Result := TryStrToFloat(S, Value);
end;

function TryStrToFloat_5(const S: string; out Value: Single;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToFloat(S, Value, FormatSettings);
end;

function StrToCurr_0(const S: string): Currency;
begin
  Result := StrToCurr(S);
end;

function StrToCurr_1(const S: string;
  const FormatSettings: TFormatSettings): Currency;
begin
  Result := StrToCurr(S, FormatSettings);
end;

function StrToCurrDef_0(const S: string; const Default: Currency): Currency;
begin
  Result := StrToCurrDef(S, Default);
end;

function StrToCurrDef_1(const S: string; const Default: Currency;
  const FormatSettings: TFormatSettings): Currency;
begin
  Result := StrToCurrDef(S, Default, FormatSettings);
end;

function TryStrToCurr_0(const S: string; out Value: Currency): Boolean;
begin
  Result := TryStrToCurr(S, Value);
end;

function TryStrToCurr_1(const S: string; out Value: Currency;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToCurr(S, Value, FormatSettings);
end;

function TextToFloat_0(Buffer: PChar; var Value;
  ValueType: TFloatValue): Boolean;
begin
  Result := TextToFloat(Buffer, Value, ValueType);
end;

function TextToFloat_1(Buffer: PChar; var Value; ValueType: TFloatValue;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TextToFloat(Buffer, Value, ValueType, FormatSettings);
end;

function DateToStr_0(const DateTime: TDateTime): string;
begin
  Result := DateToStr(DateTime);
end;

function DateToStr_1(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
begin
  Result := DateToStr(DateTime, FormatSettings);
end;

function TimeToStr_0(const DateTime: TDateTime): string;
begin
  Result := TimeToStr(DateTime);
end;

function TimeToStr_1(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
begin
  Result := TimeToStr(DateTime, FormatSettings);
end;

function DateTimeToStr_0(const DateTime: TDateTime): string;
begin
  Result := DateTimeToStr(DateTime);
end;

function DateTimeToStr_1(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
begin
  Result := DateTimeToStr(DateTime, FormatSettings);
end;

function StrToDate_0(const S: string): TDateTime;
begin
  Result := StrToDate(S);
end;

function StrToDate_1(const S: string;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  Result := StrToDate(S, FormatSettings);
end;

function StrToDateDef_0(const S: string;
  const Default: TDateTime): TDateTime;
begin
  Result := StrToDateDef(S, Default);
end;

function StrToDateDef_1(const S: string; const Default: TDateTime;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  Result := StrToDateDef(S, Default, FormatSettings);
end;

function TryStrToDate_0(const S: string; out Value: TDateTime): Boolean;
begin
  Result := TryStrToDate(S, Value);
end;

function TryStrToDate_1(const S: string; out Value: TDateTime;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToDate(S, Value, FormatSettings);
end;

function StrToTime_0(const S: string): TDateTime;
begin
  Result := StrToTime(S);
end;

function StrToTime_1(const S: string;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  Result := StrToTime(S, FormatSettings);
end;

function StrToTimeDef_0(const S: string;
  const Default: TDateTime): TDateTime;
begin
  Result := StrToTimeDef(S, Default);
end;

function StrToTimeDef_1(const S: string; const Default: TDateTime;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  Result := StrToTimeDef(S, Default, FormatSettings);
end;

function TryStrToTime_0(const S: string; out Value: TDateTime): Boolean;
begin
  Result := TryStrToTime(S, Value);
end;

function TryStrToTime_1(const S: string; out Value: TDateTime;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToTime(S, Value, FormatSettings);
end;

function StrToDateTime_0(const S: string): TDateTime;
begin
  Result := StrToDateTime(S);
end;

function StrToDateTime_1(const S: string;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  Result := StrToDateTime(S, FormatSettings);
end;

function StrToDateTimeDef_0(const S: string;
  const Default: TDateTime): TDateTime;
begin
  Result := StrToDateTimeDef(S, Default);
end;

function StrToDateTimeDef_1(const S: string; const Default: TDateTime;
  const FormatSettings: TFormatSettings): TDateTime;
begin
  Result := StrToDateTimeDef(S, Default, FormatSettings);
end;

function TryStrToDateTime_0(const S: string; out Value: TDateTime): Boolean;
begin
  Result := TryStrToDateTime(S, Value);
end;

function TryStrToDateTime_1(const S: string; out Value: TDateTime;
  const FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToDateTime(S, Value, FormatSettings);
end;

function FormatDateTime_0(const Format: string; DateTime: TDateTime): string;
begin
  Result := FormatDateTime(Format, DateTime);
end;

function FormatDateTime_1(const Format: string; DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
begin
  Result := FormatDateTime(Format, DateTime, FormatSettings);
end;

procedure DateTimeToString_0(var Result: string; const Format: string;
  DateTime: TDateTime);
begin
  DateTimeToString(Result, Format, DateTime);
end;

procedure DateTimeToString_1(var Result: string; const Format: string;
  DateTime: TDateTime; const FormatSettings: TFormatSettings);
begin
  DateTimeToString(Result, Format, DateTime, FormatSettings);
end;

function WrapText_0(const Line, BreakStr: string;
  const BreakChars: TSysCharSet; MaxCol: Integer): string;
begin
  Result := WrapText(Line, BreakStr, BreakChars, MaxCol);
end;

function WrapText_1(const Line: string; MaxCol: Integer = 45): string;
begin
  Result := WrapText(Line, MaxCol);
end;

function FindCmdLineSwitch_0(const Switch: string; const Chars: TSysCharSet;
  IgnoreCase: Boolean): Boolean;
begin
  Result := FindCmdLineSwitch(Switch, Chars, IgnoreCase);
end;

function FindCmdLineSwitch_1(const Switch: string): Boolean;
begin
  Result := FindCmdLineSwitch(Switch);
end;

function FindCmdLineSwitch_2(const Switch: string;
  IgnoreCase: Boolean): Boolean;
begin
  Result := FindCmdLineSwitch(Switch, IgnoreCase);
end;

function Supports_0(const Instance: IInterface; const IID: TGUID;
  out Intf): Boolean;
begin
  Result := Supports(Instance, IID, Intf);
end;

function Supports_1(const Instance: TObject; const IID: TGUID;
  out Intf): Boolean;
begin
  Result := Supports(Instance, IID, Intf);
end;

function Supports_2(const Instance: IInterface; const IID: TGUID): Boolean;
begin
  Result := Supports(Instance, IID);
end;

function Supports_3(const Instance: TObject; const IID: TGUID): Boolean;
begin
  Result := Supports(Instance, IID);
end;

function Supports_4(const AClass: TClass; const IID: TGUID): Boolean;
begin
  Result := Supports(AClass, IID);
end;

procedure RaiseLastOSError_0;
begin
  RaiseLastOSError;
end;

procedure RaiseLastOSError_1(LastError: Integer);
begin
  RaiseLastOSError(LastError);
end;

function GetEnvironmentVariable_0(const Name: string): string;
begin
  Result := GetEnvironmentVariable(Name);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'SysUtils',
    ['Windows', 'SysConst']);

  // Constants
  TSepiConstant.Create(Result, 'fmOpenRead', fmOpenRead);
  TSepiConstant.Create(Result, 'fmOpenWrite', fmOpenWrite);
  TSepiConstant.Create(Result, 'fmOpenReadWrite', fmOpenReadWrite);
  TSepiConstant.Create(Result, 'fmShareCompat', fmShareCompat);
  TSepiConstant.Create(Result, 'fmShareExclusive', fmShareExclusive);
  TSepiConstant.Create(Result, 'fmShareDenyWrite', fmShareDenyWrite);
  TSepiConstant.Create(Result, 'fmShareDenyRead', fmShareDenyRead);
  TSepiConstant.Create(Result, 'fmShareDenyNone', fmShareDenyNone);
  TSepiConstant.Create(Result, 'faReadOnly', faReadOnly);
  TSepiConstant.Create(Result, 'faHidden', faHidden);
  TSepiConstant.Create(Result, 'faSysFile', faSysFile);
  TSepiConstant.Create(Result, 'faDirectory', faDirectory);
  TSepiConstant.Create(Result, 'faArchive', faArchive);
  TSepiConstant.Create(Result, 'faSymLink', faSymLink);
  TSepiConstant.Create(Result, 'faAnyFile', faAnyFile);
  TSepiConstant.Create(Result, 'HoursPerDay', HoursPerDay);
  TSepiConstant.Create(Result, 'MinsPerHour', MinsPerHour);
  TSepiConstant.Create(Result, 'SecsPerMin', SecsPerMin);
  TSepiConstant.Create(Result, 'MSecsPerSec', MSecsPerSec);
  TSepiConstant.Create(Result, 'MinsPerDay', MinsPerDay);
  TSepiConstant.Create(Result, 'SecsPerDay', SecsPerDay);
  TSepiConstant.Create(Result, 'MSecsPerDay', MSecsPerDay);
  TSepiConstant.Create(Result, 'DateDelta', DateDelta);
  TSepiConstant.Create(Result, 'UnixDateDelta', UnixDateDelta);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSysCharSet));
  //TSepiType.LoadFromTypeInfo(Result, TypeInfo(TIntegerSet));
  TSepiArrayType.Create(Result, '$1',
    [0, 1], TypeInfo(Byte), True);
  SepiImportWordRec(Result);
  TSepiArrayType.Create(Result, '$2',
    [0, 1], TypeInfo(Word), True);
  TSepiArrayType.Create(Result, '$3',
    [0, 3], TypeInfo(Byte), True);
  SepiImportLongRec(Result);
  TSepiArrayType.Create(Result, '$4',
    [0, 1], TypeInfo(Cardinal), True);
  TSepiArrayType.Create(Result, '$5',
    [0, 3], TypeInfo(Word), True);
  TSepiArrayType.Create(Result, '$6',
    [0, 7], TypeInfo(Byte), True);
  SepiImportInt64Rec(Result);
  TSepiPointerType.Create(Result, 'PByteArray', 'TByteArray', True);
  TSepiArrayType.Create(Result, 'TByteArray',
    [0, 32767], TypeInfo(Byte), True);
  TSepiPointerType.Create(Result, 'PWordArray', 'TWordArray', True);
  TSepiArrayType.Create(Result, 'TWordArray',
    [0, 16383], TypeInfo(Word), True);
  TSepiMethodRefType.Create(Result, 'TProcedure',
    'procedure');
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFileName));
  SepiImportTSearchRec(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFloatValue));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFloatFormat));
  TSepiArrayType.Create(Result, '$7',
    [0, 20], TypeInfo(Char), True);
  SepiImportTFloatRec(Result);
  SepiImportTTimeStamp(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMbcsByteType));
  SepiImportTSysLocale(Result);
  SepiImportTLangRec(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_8));
  TSepiImportsTLanguages.SepiImport(Result);
  TSepiImportsException.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'ExceptClass', TypeInfo(Exception), True);
  TSepiImportsEAbort.SepiImport(Result);
  TSepiImportsEHeapException.SepiImport(Result);
  TSepiImportsEOutOfMemory.SepiImport(Result);
  TSepiImportsEInOutError.SepiImport(Result);
  TSepiPointerType.Create(Result, 'PExceptionRecord',
    'TExceptionRecord', True);
  TSepiArrayType.Create(Result, '$9',
    [0, 14], TypeInfo(Cardinal), True);
  SepiImportTExceptionRecord(Result);
  TSepiImportsEExternal.SepiImport(Result);
  TSepiImportsEExternalException.SepiImport(Result);
  TSepiImportsEIntError.SepiImport(Result);
  TSepiImportsEDivByZero.SepiImport(Result);
  TSepiImportsERangeError.SepiImport(Result);
  TSepiImportsEIntOverflow.SepiImport(Result);
  TSepiImportsEMathError.SepiImport(Result);
  TSepiImportsEInvalidOp.SepiImport(Result);
  TSepiImportsEZeroDivide.SepiImport(Result);
  TSepiImportsEOverflow.SepiImport(Result);
  TSepiImportsEUnderflow.SepiImport(Result);
  TSepiImportsEInvalidPointer.SepiImport(Result);
  TSepiImportsEInvalidCast.SepiImport(Result);
  TSepiImportsEConvertError.SepiImport(Result);
  TSepiImportsEAccessViolation.SepiImport(Result);
  TSepiImportsEPrivilege.SepiImport(Result);
  TSepiImportsEControlC.SepiImport(Result);
  TSepiImportsEVariantError.SepiImport(Result);
  TSepiImportsEPropReadOnly.SepiImport(Result);
  TSepiImportsEPropWriteOnly.SepiImport(Result);
  TSepiImportsEAssertionFailed.SepiImport(Result);
  TSepiImportsEAbstractError.SepiImport(Result);
  TSepiImportsEIntfCastError.SepiImport(Result);
  TSepiImportsEInvalidContainer.SepiImport(Result);
  TSepiImportsEInvalidInsert.SepiImport(Result);
  TSepiImportsEPackageError.SepiImport(Result);
  TSepiImportsEOSError.SepiImport(Result);
  TSepiImportsESafecallException.SepiImport(Result);

  // Global variables
  TSepiVariable.Create(Result, 'EmptyStr',
    EmptyStr, TypeInfo(string));
  TSepiVariable.Create(Result, 'NullStr',
    NullStr, 'PString');
  TSepiVariable.Create(Result, 'EmptyWideStr',
    EmptyWideStr, TypeInfo(WideString));
  TSepiVariable.Create(Result, 'NullWideStr',
    NullWideStr, 'PWideString');
  TSepiVariable.Create(Result, 'Win32Platform',
    Win32Platform, TypeInfo(Integer));
  TSepiVariable.Create(Result, 'Win32MajorVersion',
    Win32MajorVersion, TypeInfo(Integer));
  TSepiVariable.Create(Result, 'Win32MinorVersion',
    Win32MinorVersion, TypeInfo(Integer));
  TSepiVariable.Create(Result, 'Win32BuildNumber',
    Win32BuildNumber, TypeInfo(Integer));
  TSepiVariable.Create(Result, 'Win32CSDVersion',
    Win32CSDVersion, TypeInfo(string));

  // Routines
  TSepiMethod.Create(Result, 'CheckWin32Version', @CheckWin32Version,
    'function(AMajor: Integer; AMinor: Integer = 0): Boolean');
  TSepiMethod.Create(Result, 'GetFileVersion', @GetFileVersion,
    'function(const AFileName: string): Cardinal');

  // Global variables
  TSepiVariable.Create(Result, 'CurrencyString',
    CurrencyString, TypeInfo(string));
  TSepiVariable.Create(Result, 'CurrencyFormat',
    CurrencyFormat, TypeInfo(Byte));
  TSepiVariable.Create(Result, 'NegCurrFormat',
    NegCurrFormat, TypeInfo(Byte));
  TSepiVariable.Create(Result, 'ThousandSeparator',
    ThousandSeparator, TypeInfo(Char));
  TSepiVariable.Create(Result, 'DecimalSeparator',
    DecimalSeparator, TypeInfo(Char));
  TSepiVariable.Create(Result, 'CurrencyDecimals',
    CurrencyDecimals, TypeInfo(Byte));
  TSepiVariable.Create(Result, 'DateSeparator',
    DateSeparator, TypeInfo(Char));
  TSepiVariable.Create(Result, 'ShortDateFormat',
    ShortDateFormat, TypeInfo(string));
  TSepiVariable.Create(Result, 'LongDateFormat',
    LongDateFormat, TypeInfo(string));
  TSepiVariable.Create(Result, 'TimeSeparator',
    TimeSeparator, TypeInfo(Char));
  TSepiVariable.Create(Result, 'TimeAMString',
    TimeAMString, TypeInfo(string));
  TSepiVariable.Create(Result, 'TimePMString',
    TimePMString, TypeInfo(string));
  TSepiVariable.Create(Result, 'ShortTimeFormat',
    ShortTimeFormat, TypeInfo(string));
  TSepiVariable.Create(Result, 'LongTimeFormat',
    LongTimeFormat, TypeInfo(string));
  TSepiArrayType.Create(Result, '$10',
    [1, 12], TypeInfo(string), True);
  TSepiVariable.Create(Result, 'ShortMonthNames',
    ShortMonthNames, '$10');
  TSepiArrayType.Create(Result, '$11',
    [1, 12], TypeInfo(string), True);
  TSepiVariable.Create(Result, 'LongMonthNames',
    LongMonthNames, '$11');
  TSepiArrayType.Create(Result, '$12',
    [1, 7], TypeInfo(string), True);
  TSepiVariable.Create(Result, 'ShortDayNames',
    ShortDayNames, '$12');
  TSepiArrayType.Create(Result, '$13',
    [1, 7], TypeInfo(string), True);
  TSepiVariable.Create(Result, 'LongDayNames',
    LongDayNames, '$13');
  TSepiVariable.Create(Result, 'SysLocale',
    SysLocale, 'TSysLocale');
  TSepiVariable.Create(Result, 'TwoDigitYearCenturyWindow',
    TwoDigitYearCenturyWindow, TypeInfo(Word));
  TSepiVariable.Create(Result, 'ListSeparator',
    ListSeparator, TypeInfo(Char));

  // Types
  TSepiArrayType.Create(Result, '$14',
    [1, 12], TypeInfo(string), True);
  TSepiArrayType.Create(Result, '$15',
    [1, 12], TypeInfo(string), True);
  TSepiArrayType.Create(Result, '$16',
    [1, 7], TypeInfo(string), True);
  TSepiArrayType.Create(Result, '$17',
    [1, 7], TypeInfo(string), True);
  SepiImportTFormatSettings(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TLocaleOptions));

  // Constants
  TSepiConstant.Create(Result, 'MaxEraCount', MaxEraCount);

  // Global variables
  TSepiArrayType.Create(Result, '$18',
    [1, MaxEraCount], TypeInfo(string), True);
  TSepiVariable.Create(Result, 'EraNames',
    EraNames, '$18');
  TSepiArrayType.Create(Result, '$19',
    [1, MaxEraCount], TypeInfo(Integer), True);
  TSepiVariable.Create(Result, 'EraYearOffsets',
    EraYearOffsets, '$19');

  // Constants
  TSepiConstant.Create(Result, 'PathDelim', PathDelim);
  TSepiConstant.Create(Result, 'DriveDelim', DriveDelim);
  TSepiConstant.Create(Result, 'PathSep', PathSep);

  // Routines
  TSepiMethod.Create(Result, 'Languages', @Languages,
    'function: TLanguages');
  TSepiMethod.Create(Result, 'AllocMem', @AllocMem,
    'function(Size: Cardinal): Pointer');
  TSepiMethod.Create(Result, 'AddExitProc', @AddExitProc,
    'procedure(Proc: TProcedure)');
  TSepiMethod.CreateOverloaded(Result, 'UpperCase', @UpperCase_0,
    'function(const S: string): string');
  TSepiMethod.CreateOverloaded(Result, 'UpperCase', @UpperCase_1,
    'function(const S: string; LocaleOptions: TLocaleOptions): string');
  TSepiMethod.CreateOverloaded(Result, 'LowerCase', @LowerCase_0,
    'function(const S: string): string');
  TSepiMethod.CreateOverloaded(Result, 'LowerCase', @LowerCase_1,
    'function(const S: string; LocaleOptions: TLocaleOptions): string');
  TSepiMethod.CreateOverloaded(Result, 'CompareStr', @CompareStr_0,
    'function(const S1, S2: string): Integer');
  TSepiMethod.CreateOverloaded(Result, 'CompareStr', @CompareStr_1,
    'function(const S1, S2: string; LocaleOptions: TLocaleOptions): Integer');
  TSepiMethod.CreateOverloaded(Result, 'SameStr', @SameStr_0,
    'function(const S1, S2: string): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'SameStr', @SameStr_1,
    'function(const S1, S2: string; LocaleOptions: TLocaleOptions): Boolean');
  TSepiMethod.Create(Result, 'CompareMem', @CompareMem,
    'function(P1, P2: Pointer; Length: Integer): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'CompareText', @CompareText_0,
    'function(const S1, S2: string): Integer');
  TSepiMethod.CreateOverloaded(Result, 'CompareText', @CompareText_1,
    'function(const S1, S2: string; LocaleOptions: TLocaleOptions): Integer');
  TSepiMethod.CreateOverloaded(Result, 'SameText', @SameText_0,
    'function(const S1, S2: string): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'SameText', @SameText_1,
    'function(const S1, S2: string; LocaleOptions: TLocaleOptions): Boolean');
  TSepiMethod.Create(Result, 'AnsiUpperCase', @AnsiUpperCase,
    'function(const S: string): string');
  TSepiMethod.Create(Result, 'AnsiLowerCase', @AnsiLowerCase,
    'function(const S: string): string');
  TSepiMethod.Create(Result, 'AnsiCompareStr', @AnsiCompareStr,
    'function(const S1, S2: string): Integer');
  TSepiMethod.Create(Result, 'AnsiSameStr', @AnsiSameStr,
    'function(const S1, S2: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiCompareText', @AnsiCompareText,
    'function(const S1, S2: string): Integer');
  TSepiMethod.Create(Result, 'AnsiSameText', @AnsiSameText,
    'function(const S1, S2: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiStrComp', @AnsiStrComp,
    'function(S1, S2: PChar): Integer');
  TSepiMethod.Create(Result, 'AnsiStrIComp', @AnsiStrIComp,
    'function(S1, S2: PChar): Integer');
  TSepiMethod.Create(Result, 'AnsiStrLComp', @AnsiStrLComp,
    'function(S1, S2: PChar; MaxLen: Cardinal): Integer');
  TSepiMethod.Create(Result, 'AnsiStrLIComp', @AnsiStrLIComp,
    'function(S1, S2: PChar; MaxLen: Cardinal): Integer');
  TSepiMethod.Create(Result, 'AnsiStrLower', @AnsiStrLower,
    'function(Str: PChar): PChar');
  TSepiMethod.Create(Result, 'AnsiStrUpper', @AnsiStrUpper,
    'function(Str: PChar): PChar');
  TSepiMethod.Create(Result, 'AnsiLastChar', @AnsiLastChar,
    'function(const S: string): PChar');
  TSepiMethod.Create(Result, 'AnsiStrLastChar', @AnsiStrLastChar,
    'function(P: PChar): PChar');
  TSepiMethod.Create(Result, 'WideUpperCase', @WideUpperCase,
    'function(const S: WideString): WideString');
  TSepiMethod.Create(Result, 'WideLowerCase', @WideLowerCase,
    'function(const S: WideString): WideString');
  TSepiMethod.Create(Result, 'WideCompareStr', @WideCompareStr,
    'function(const S1, S2: WideString): Integer');
  TSepiMethod.Create(Result, 'WideSameStr', @WideSameStr,
    'function(const S1, S2: WideString): Boolean');
  TSepiMethod.Create(Result, 'WideCompareText', @WideCompareText,
    'function(const S1, S2: WideString): Integer');
  TSepiMethod.Create(Result, 'WideSameText', @WideSameText,
    'function(const S1, S2: WideString): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'Trim', @Trim_0,
    'function(const S: string): string');
  TSepiMethod.CreateOverloaded(Result, 'Trim', @Trim_1,
    'function(const S: WideString): WideString');
  TSepiMethod.CreateOverloaded(Result, 'TrimLeft', @TrimLeft_0,
    'function(const S: string): string');
  TSepiMethod.CreateOverloaded(Result, 'TrimLeft', @TrimLeft_1,
    'function(const S: WideString): WideString');
  TSepiMethod.CreateOverloaded(Result, 'TrimRight', @TrimRight_0,
    'function(const S: string): string');
  TSepiMethod.CreateOverloaded(Result, 'TrimRight', @TrimRight_1,
    'function(const S: WideString): WideString');
  TSepiMethod.Create(Result, 'QuotedStr', @QuotedStr,
    'function(const S: string): string');
  TSepiMethod.Create(Result, 'AnsiQuotedStr', @AnsiQuotedStr,
    'function(const S: string; Quote: Char): string');
  TSepiMethod.Create(Result, 'AnsiExtractQuotedStr', @AnsiExtractQuotedStr,
    'function(var Src: PChar; Quote: Char): string');
  TSepiMethod.Create(Result, 'AnsiDequotedStr', @AnsiDequotedStr,
    'function(const S: string; AQuote: Char): string');
  TSepiMethod.Create(Result, 'AdjustLineBreaks', @AdjustLineBreaks,
    'function(const S: string; Style: TTextLineBreakStyle = tlbsCRLF ) : string');
  TSepiMethod.Create(Result, 'IsValidIdent', @IsValidIdent,
    'function(const Ident: string; AllowDots: Boolean = False): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'IntToStr', @IntToStr_0,
    'function(Value: Integer): string');
  TSepiMethod.CreateOverloaded(Result, 'IntToStr', @IntToStr_1,
    'function(Value: Int64): string');
  TSepiMethod.CreateOverloaded(Result, 'IntToHex', @IntToHex_0,
    'function(Value: Integer; Digits: Integer): string');
  TSepiMethod.CreateOverloaded(Result, 'IntToHex', @IntToHex_1,
    'function(Value: Int64; Digits: Integer): string');
  TSepiMethod.Create(Result, 'StrToInt', @StrToInt,
    'function(const S: string): Integer');
  TSepiMethod.Create(Result, 'StrToIntDef', @StrToIntDef,
    'function(const S: string; Default: Integer): Integer');
  TSepiMethod.Create(Result, 'TryStrToInt', @TryStrToInt,
    'function(const S: string; out Value: Integer): Boolean');
  TSepiMethod.Create(Result, 'StrToInt64', @StrToInt64,
    'function(const S: string): Int64');
  TSepiMethod.Create(Result, 'StrToInt64Def', @StrToInt64Def,
    'function(const S: string; const Default: Int64): Int64');
  TSepiMethod.Create(Result, 'TryStrToInt64', @TryStrToInt64,
    'function(const S: string; out Value: Int64): Boolean');

  // Global variables
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_20));
  TSepiVariable.Create(Result, 'TrueBoolStrs',
    TrueBoolStrs, TypeInfo(TUnnamed_20));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_21));
  TSepiVariable.Create(Result, 'FalseBoolStrs',
    FalseBoolStrs, TypeInfo(TUnnamed_21));

  // Constants
  TSepiConstant.Create(Result, 'DefaultTrueBoolStr', DefaultTrueBoolStr);
  TSepiConstant.Create(Result, 'DefaultFalseBoolStr', DefaultFalseBoolStr);

  // Routines
  TSepiMethod.Create(Result, 'StrToBool', @StrToBool,
    'function(const S: string): Boolean');
  TSepiMethod.Create(Result, 'StrToBoolDef', @StrToBoolDef,
    'function(const S: string; const Default: Boolean): Boolean');
  TSepiMethod.Create(Result, 'TryStrToBool', @TryStrToBool,
    'function(const S: string; out Value: Boolean): Boolean');
  TSepiMethod.Create(Result, 'BoolToStr', @BoolToStr,
    'function(B: Boolean; UseBoolStrs: Boolean = False): string');
  TSepiMethod.Create(Result, 'LoadStr', @LoadStr,
    'function(Ident: Integer): string');
  TSepiMethod.Create(Result, 'FmtLoadStr', @FmtLoadStr,
    'function(Ident: Integer; const Args: array of const): string');
  TSepiMethod.Create(Result, 'FileOpen', @FileOpen,
    'function(const FileName: string; Mode: LongWord): Integer');
  TSepiMethod.CreateOverloaded(Result, 'FileCreate', @FileCreate_0,
    'function(const FileName: string): Integer');
  TSepiMethod.CreateOverloaded(Result, 'FileCreate', @FileCreate_1,
    'function(const FileName: string; Rights: Integer): Integer');
  TSepiMethod.Create(Result, 'FileRead', @FileRead,
    'function(Handle: Integer; var Buffer; Count: LongWord): Integer');
  TSepiMethod.Create(Result, 'FileWrite', @FileWrite,
    'function(Handle: Integer; const Buffer; Count: LongWord): Integer');
  TSepiMethod.CreateOverloaded(Result, 'FileSeek', @FileSeek_0,
    'function(Handle, Offset, Origin: Integer): Integer');
  TSepiMethod.CreateOverloaded(Result, 'FileSeek', @FileSeek_1,
    'function(Handle: Integer; const Offset: Int64; Origin: Integer): Int64');
  TSepiMethod.Create(Result, 'FileClose', @FileClose,
    'procedure(Handle: Integer)');
  TSepiMethod.Create(Result, 'FileAge', @FileAge,
    'function(const FileName: string): Integer');
  TSepiMethod.Create(Result, 'FileExists', @FileExists,
    'function(const FileName: string): Boolean');
  TSepiMethod.Create(Result, 'DirectoryExists', @DirectoryExists,
    'function(const Directory: string): Boolean');
  TSepiMethod.Create(Result, 'ForceDirectories', @ForceDirectories,
    'function(Dir: string): Boolean');
  TSepiMethod.Create(Result, 'FindFirst', @FindFirst,
    'function(const Path: string; Attr: Integer; var F: TSearchRec ) : Integer');
  TSepiMethod.Create(Result, 'FindNext', @FindNext,
    'function(var F: TSearchRec): Integer');
  TSepiMethod.Create(Result, 'FindClose', @FindClose,
    'procedure(var F: TSearchRec)');
  TSepiMethod.Create(Result, 'FileGetDate', @FileGetDate,
    'function(Handle: Integer): Integer');
  TSepiMethod.CreateOverloaded(Result, 'FileSetDate', @FileSetDate_0,
    'function(const FileName: string; Age: Integer): Integer');
  TSepiMethod.CreateOverloaded(Result, 'FileSetDate', @FileSetDate_1,
    'function(Handle: Integer; Age: Integer): Integer');
  TSepiMethod.Create(Result, 'FileGetAttr', @FileGetAttr,
    'function(const FileName: string): Integer');
  TSepiMethod.Create(Result, 'FileSetAttr', @FileSetAttr,
    'function(const FileName: string; Attr: Integer): Integer');
  TSepiMethod.Create(Result, 'FileIsReadOnly', @FileIsReadOnly,
    'function(const FileName: string): Boolean');
  TSepiMethod.Create(Result, 'FileSetReadOnly', @FileSetReadOnly,
    'function(const FileName: string; ReadOnly: Boolean): Boolean');
  TSepiMethod.Create(Result, 'DeleteFile', @DeleteFile,
    'function(const FileName: string): Boolean');
  TSepiMethod.Create(Result, 'RenameFile', @RenameFile,
    'function(const OldName, NewName: string): Boolean');
  TSepiMethod.Create(Result, 'ChangeFileExt', @ChangeFileExt,
    'function(const FileName, Extension: string): string');
  TSepiMethod.Create(Result, 'ExtractFilePath', @ExtractFilePath,
    'function(const FileName: string): string');
  TSepiMethod.Create(Result, 'ExtractFileDir', @ExtractFileDir,
    'function(const FileName: string): string');
  TSepiMethod.Create(Result, 'ExtractFileDrive', @ExtractFileDrive,
    'function(const FileName: string): string');
  TSepiMethod.Create(Result, 'ExtractFileName', @ExtractFileName,
    'function(const FileName: string): string');
  TSepiMethod.Create(Result, 'ExtractFileExt', @ExtractFileExt,
    'function(const FileName: string): string');
  TSepiMethod.Create(Result, 'ExpandFileName', @ExpandFileName,
    'function(const FileName: string): string');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFilenameCaseMatch));

  // Routines
  TSepiMethod.Create(Result, 'ExpandFileNameCase', @ExpandFileNameCase,
    'function(const FileName: string; out MatchFound: TFilenameCaseMatch ) : string');
  TSepiMethod.Create(Result, 'ExpandUNCFileName', @ExpandUNCFileName,
    'function(const FileName: string): string');
  TSepiMethod.Create(Result, 'ExtractRelativePath', @ExtractRelativePath,
    'function(const BaseName, DestName: string): string');
  TSepiMethod.Create(Result, 'ExtractShortPathName', @ExtractShortPathName,
    'function(const FileName: string): string');
  TSepiMethod.Create(Result, 'FileSearch', @FileSearch,
    'function(const Name, DirList: string): string');
  TSepiMethod.Create(Result, 'DiskFree', @DiskFree,
    'function(Drive: Byte): Int64');
  TSepiMethod.Create(Result, 'DiskSize', @DiskSize,
    'function(Drive: Byte): Int64');
  TSepiMethod.Create(Result, 'FileDateToDateTime', @FileDateToDateTime,
    'function(FileDate: Integer): TDateTime');
  TSepiMethod.Create(Result, 'DateTimeToFileDate', @DateTimeToFileDate,
    'function(DateTime: TDateTime): Integer');
  TSepiMethod.Create(Result, 'GetCurrentDir', @GetCurrentDir,
    'function: string');
  TSepiMethod.Create(Result, 'SetCurrentDir', @SetCurrentDir,
    'function(const Dir: string): Boolean');
  TSepiMethod.Create(Result, 'CreateDir', @CreateDir,
    'function(const Dir: string): Boolean');
  TSepiMethod.Create(Result, 'RemoveDir', @RemoveDir,
    'function(const Dir: string): Boolean');
  TSepiMethod.Create(Result, 'StrLen', @StrLen,
    'function(const Str: PChar): Cardinal');
  TSepiMethod.Create(Result, 'StrEnd', @StrEnd,
    'function(const Str: PChar): PChar');
  TSepiMethod.Create(Result, 'StrMove', @StrMove,
    'function(Dest: PChar; const Source: PChar; Count: Cardinal): PChar');
  TSepiMethod.Create(Result, 'StrCopy', @StrCopy,
    'function(Dest: PChar; const Source: PChar): PChar');
  TSepiMethod.Create(Result, 'StrECopy', @StrECopy,
    'function(Dest:PChar; const Source: PChar): PChar');
  TSepiMethod.Create(Result, 'StrLCopy', @StrLCopy,
    'function(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar');
  TSepiMethod.Create(Result, 'StrPCopy', @StrPCopy,
    'function(Dest: PChar; const Source: string): PChar');
  TSepiMethod.Create(Result, 'StrPLCopy', @StrPLCopy,
    'function(Dest: PChar; const Source: string; MaxLen: Cardinal ) : PChar');
  TSepiMethod.Create(Result, 'StrCat', @StrCat,
    'function(Dest: PChar; const Source: PChar): PChar');
  TSepiMethod.Create(Result, 'StrLCat', @StrLCat,
    'function(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar');
  TSepiMethod.Create(Result, 'StrComp', @StrComp,
    'function(const Str1, Str2: PChar): Integer');
  TSepiMethod.Create(Result, 'StrIComp', @StrIComp,
    'function(const Str1, Str2: PChar): Integer');
  TSepiMethod.Create(Result, 'StrLComp', @StrLComp,
    'function(const Str1, Str2: PChar; MaxLen: Cardinal): Integer');
  TSepiMethod.Create(Result, 'StrLIComp', @StrLIComp,
    'function(const Str1, Str2: PChar; MaxLen: Cardinal): Integer');
  TSepiMethod.Create(Result, 'StrScan', @StrScan,
    'function(const Str: PChar; Chr: Char): PChar');
  TSepiMethod.Create(Result, 'StrRScan', @StrRScan,
    'function(const Str: PChar; Chr: Char): PChar');
  TSepiMethod.Create(Result, 'StrPos', @StrPos,
    'function(const Str1, Str2: PChar): PChar');
  TSepiMethod.Create(Result, 'StrUpper', @StrUpper,
    'function(Str: PChar): PChar');
  TSepiMethod.Create(Result, 'StrLower', @StrLower,
    'function(Str: PChar): PChar');
  TSepiMethod.Create(Result, 'StrPas', @StrPas,
    'function(const Str: PChar): string');
  TSepiMethod.Create(Result, 'StrAlloc', @StrAlloc,
    'function(Size: Cardinal): PChar');
  TSepiMethod.Create(Result, 'StrBufSize', @StrBufSize,
    'function(const Str: PChar): Cardinal');
  TSepiMethod.Create(Result, 'StrNew', @StrNew,
    'function(const Str: PChar): PChar');
  TSepiMethod.Create(Result, 'StrDispose', @StrDispose,
    'procedure(Str: PChar)');
  TSepiMethod.CreateOverloaded(Result, 'Format', @Format_0,
    'function(const Format: string; const Args: array of const ) : string');
  TSepiMethod.CreateOverloaded(Result, 'Format', @Format_1,
    'function(const Format: string; const Args: array of const; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'FmtStr', @FmtStr_0,
    'procedure(var Result: string; const Format: string; const Args: array of const )');
  TSepiMethod.CreateOverloaded(Result, 'FmtStr', @FmtStr_1,
    'procedure(var Result: string; const Format: string; const Args: array of const ; const FormatSettings: TFormatSettings )');
  TSepiMethod.CreateOverloaded(Result, 'StrFmt', @StrFmt_0,
    'function(Buffer, Format: PChar; const Args: array of const ) : PChar');
  TSepiMethod.CreateOverloaded(Result, 'StrFmt', @StrFmt_1,
    'function(Buffer, Format: PChar; const Args: array of const; const FormatSettings: TFormatSettings ) : PChar');
  TSepiMethod.CreateOverloaded(Result, 'StrLFmt', @StrLFmt_0,
    'function(Buffer: PChar; MaxBufLen: Cardinal; Format: PChar; const Args: array of const ) : PChar');
  TSepiMethod.CreateOverloaded(Result, 'StrLFmt', @StrLFmt_1,
    'function(Buffer: PChar; MaxBufLen: Cardinal; Format: PChar; const Args: array of const ; const FormatSettings: TFormatSettings ) : PChar');
  TSepiMethod.CreateOverloaded(Result, 'FormatBuf', @FormatBuf_0,
    'function(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal ; const Args: array of const ) : Cardinal');
  TSepiMethod.CreateOverloaded(Result, 'FormatBuf', @FormatBuf_1,
    'function(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal ; const Args: array of const ; const FormatSettings: TFormatSettings ) : Cardinal');
  TSepiMethod.CreateOverloaded(Result, 'WideFormat', @WideFormat_0,
    'function(const Format: WideString; const Args: array of const ) : WideString');
  TSepiMethod.CreateOverloaded(Result, 'WideFormat', @WideFormat_1,
    'function(const Format: WideString; const Args: array of const ; const FormatSettings: TFormatSettings ) : WideString');
  TSepiMethod.CreateOverloaded(Result, 'WideFmtStr', @WideFmtStr_0,
    'procedure(var Result: WideString; const Format: WideString; const Args: array of const )');
  TSepiMethod.CreateOverloaded(Result, 'WideFmtStr', @WideFmtStr_1,
    'procedure(var Result: WideString; const Format: WideString; const Args: array of const ; const FormatSettings: TFormatSettings )');
  TSepiMethod.CreateOverloaded(Result, 'WideFormatBuf', @WideFormatBuf_0,
    'function(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal ; const Args: array of const ) : Cardinal');
  TSepiMethod.CreateOverloaded(Result, 'WideFormatBuf', @WideFormatBuf_1,
    'function(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal ; const Args: array of const ; const FormatSettings: TFormatSettings ) : Cardinal');
  TSepiMethod.CreateOverloaded(Result, 'FloatToStr', @FloatToStr_0,
    'function(Value: Extended): string');
  TSepiMethod.CreateOverloaded(Result, 'FloatToStr', @FloatToStr_1,
    'function(Value: Extended; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'CurrToStr', @CurrToStr_0,
    'function(Value: Currency): string');
  TSepiMethod.CreateOverloaded(Result, 'CurrToStr', @CurrToStr_1,
    'function(Value: Currency; const FormatSettings: TFormatSettings ) : string');

  // Constants
  TSepiVariable.Create(Result, 'MinCurrency',
    MinCurrency, TypeInfo(Currency), True);
  TSepiVariable.Create(Result, 'MaxCurrency',
    MaxCurrency, TypeInfo(Currency), True);

  // Routines
  TSepiMethod.Create(Result, 'FloatToCurr', @FloatToCurr,
    'function(const Value: Extended): Currency');
  TSepiMethod.Create(Result, 'TryFloatToCurr', @TryFloatToCurr,
    'function(const Value: Extended; out AResult: Currency): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'FloatToStrF', @FloatToStrF_0,
    'function(Value: Extended; Format: TFloatFormat; Precision, Digits: Integer ) : string');
  TSepiMethod.CreateOverloaded(Result, 'FloatToStrF', @FloatToStrF_1,
    'function(Value: Extended; Format: TFloatFormat; Precision, Digits: Integer ; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'CurrToStrF', @CurrToStrF_0,
    'function(Value: Currency; Format: TFloatFormat; Digits: Integer ) : string');
  TSepiMethod.CreateOverloaded(Result, 'CurrToStrF', @CurrToStrF_1,
    'function(Value: Currency; Format: TFloatFormat; Digits: Integer ; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'FloatToText', @FloatToText_0,
    'function(BufferArg: PChar; const Value; ValueType: TFloatValue; Format: TFloatFormat ; Precision, Digits: Integer ) : Integer');
  TSepiMethod.CreateOverloaded(Result, 'FloatToText', @FloatToText_1,
    'function(BufferArg: PChar; const Value; ValueType: TFloatValue; Format: TFloatFormat ; Precision, Digits: Integer ; const FormatSettings: TFormatSettings ) : Integer');
  TSepiMethod.CreateOverloaded(Result, 'FormatFloat', @FormatFloat_0,
    'function(const Format: string; Value: Extended): string');
  TSepiMethod.CreateOverloaded(Result, 'FormatFloat', @FormatFloat_1,
    'function(const Format: string; Value: Extended; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'FormatCurr', @FormatCurr_0,
    'function(const Format: string; Value: Currency): string');
  TSepiMethod.CreateOverloaded(Result, 'FormatCurr', @FormatCurr_1,
    'function(const Format: string; Value: Currency; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'FloatToTextFmt', @FloatToTextFmt_0,
    'function(Buf: PChar; const Value; ValueType: TFloatValue; Format: PChar ) : Integer');
  TSepiMethod.CreateOverloaded(Result, 'FloatToTextFmt', @FloatToTextFmt_1,
    'function(Buf: PChar; const Value; ValueType: TFloatValue; Format: PChar ; const FormatSettings: TFormatSettings ) : Integer');
  TSepiMethod.CreateOverloaded(Result, 'StrToFloat', @StrToFloat_0,
    'function(const S: string): Extended');
  TSepiMethod.CreateOverloaded(Result, 'StrToFloat', @StrToFloat_1,
    'function(const S: string; const FormatSettings: TFormatSettings ) : Extended');
  TSepiMethod.CreateOverloaded(Result, 'StrToFloatDef', @StrToFloatDef_0,
    'function(const S: string; const Default: Extended ) : Extended');
  TSepiMethod.CreateOverloaded(Result, 'StrToFloatDef', @StrToFloatDef_1,
    'function(const S: string; const Default: Extended; const FormatSettings: TFormatSettings ) : Extended');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToFloat', @TryStrToFloat_0,
    'function(const S: string; out Value: Extended): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToFloat', @TryStrToFloat_1,
    'function(const S: string; out Value: Extended; const FormatSettings: TFormatSettings ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToFloat', @TryStrToFloat_2,
    'function(const S: string; out Value: Double): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToFloat', @TryStrToFloat_3,
    'function(const S: string; out Value: Double; const FormatSettings: TFormatSettings ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToFloat', @TryStrToFloat_4,
    'function(const S: string; out Value: Single): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToFloat', @TryStrToFloat_5,
    'function(const S: string; out Value: Single; const FormatSettings: TFormatSettings ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'StrToCurr', @StrToCurr_0,
    'function(const S: string): Currency');
  TSepiMethod.CreateOverloaded(Result, 'StrToCurr', @StrToCurr_1,
    'function(const S: string; const FormatSettings: TFormatSettings ) : Currency');
  TSepiMethod.CreateOverloaded(Result, 'StrToCurrDef', @StrToCurrDef_0,
    'function(const S: string; const Default: Currency ) : Currency');
  TSepiMethod.CreateOverloaded(Result, 'StrToCurrDef', @StrToCurrDef_1,
    'function(const S: string; const Default: Currency; const FormatSettings: TFormatSettings ) : Currency');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToCurr', @TryStrToCurr_0,
    'function(const S: string; out Value: Currency): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToCurr', @TryStrToCurr_1,
    'function(const S: string; out Value: Currency; const FormatSettings: TFormatSettings ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TextToFloat', @TextToFloat_0,
    'function(Buffer: PChar; var Value; ValueType: TFloatValue ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TextToFloat', @TextToFloat_1,
    'function(Buffer: PChar; var Value; ValueType: TFloatValue; const FormatSettings: TFormatSettings ) : Boolean');
  TSepiMethod.Create(Result, 'FloatToDecimal', @FloatToDecimal,
    'procedure(var Result: TFloatRec; const Value; ValueType: TFloatValue ; Precision, Decimals: Integer )');
  TSepiMethod.Create(Result, 'DateTimeToTimeStamp', @DateTimeToTimeStamp,
    'function(DateTime: TDateTime): TTimeStamp');
  TSepiMethod.Create(Result, 'TimeStampToDateTime', @TimeStampToDateTime,
    'function(const TimeStamp: TTimeStamp): TDateTime');
  TSepiMethod.Create(Result, 'MSecsToTimeStamp', @MSecsToTimeStamp,
    'function(MSecs: Comp): TTimeStamp');
  TSepiMethod.Create(Result, 'TimeStampToMSecs', @TimeStampToMSecs,
    'function(const TimeStamp: TTimeStamp): Comp');
  TSepiMethod.Create(Result, 'EncodeDate', @EncodeDate,
    'function(Year, Month, Day: Word): TDateTime');
  TSepiMethod.Create(Result, 'EncodeTime', @EncodeTime,
    'function(Hour, Min, Sec, MSec: Word): TDateTime');
  TSepiMethod.Create(Result, 'TryEncodeDate', @TryEncodeDate,
    'function(Year, Month, Day: Word; out Date: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'TryEncodeTime', @TryEncodeTime,
    'function(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'DecodeDate', @DecodeDate,
    'procedure(const DateTime: TDateTime; var Year, Month, Day: Word)');
  TSepiMethod.Create(Result, 'DecodeDateFully', @DecodeDateFully,
    'function(const DateTime: TDateTime; var Year, Month, Day, DOW : Word ) : Boolean');
  TSepiMethod.Create(Result, 'DecodeTime', @DecodeTime,
    'procedure(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word)');
  TSepiMethod.Create(Result, 'DateTimeToSystemTime', @DateTimeToSystemTime,
    'procedure(const DateTime: TDateTime; var SystemTime: TSystemTime)');
  TSepiMethod.Create(Result, 'SystemTimeToDateTime', @SystemTimeToDateTime,
    'function(const SystemTime: TSystemTime): TDateTime');
  TSepiMethod.Create(Result, 'DayOfWeek', @DayOfWeek,
    'function(const DateTime: TDateTime): Word');
  TSepiMethod.Create(Result, 'Date', @Date,
    'function: TDateTime');
  TSepiMethod.Create(Result, 'Time', @Time,
    'function: TDateTime');
  TSepiMethod.Create(Result, 'GetTime', @GetTime,
    'function: TDateTime');
  TSepiMethod.Create(Result, 'Now', @Now,
    'function: TDateTime');
  TSepiMethod.Create(Result, 'CurrentYear', @CurrentYear,
    'function: Word');
  TSepiMethod.Create(Result, 'IncMonth', @IncMonth,
    'function(const DateTime: TDateTime; NumberOfMonths: Integer = 1): TDateTime');
  TSepiMethod.Create(Result, 'IncAMonth', @IncAMonth,
    'procedure(var Year, Month, Day: Word; NumberOfMonths: Integer = 1)');
  TSepiMethod.Create(Result, 'ReplaceTime', @ReplaceTime,
    'procedure(var DateTime: TDateTime; const NewTime: TDateTime)');
  TSepiMethod.Create(Result, 'ReplaceDate', @ReplaceDate,
    'procedure(var DateTime: TDateTime; const NewDate: TDateTime)');
  TSepiMethod.Create(Result, 'IsLeapYear', @IsLeapYear,
    'function(Year: Word): Boolean');

  // Types
  TSepiPointerType.Create(Result, 'PDayTable', 'TDayTable', True);
  TSepiArrayType.Create(Result, 'TDayTable',
    [1, 12], TypeInfo(Word), True);

  // Constants
  TSepiArrayType.Create(Result, '$22',
    [Integer(Low(Boolean)), Integer(High(Boolean))], 'TDayTable', True);
  TSepiVariable.Create(Result, 'MonthDays',
    MonthDays, '$22', True);

  // Routines
  TSepiMethod.CreateOverloaded(Result, 'DateToStr', @DateToStr_0,
    'function(const DateTime: TDateTime): string');
  TSepiMethod.CreateOverloaded(Result, 'DateToStr', @DateToStr_1,
    'function(const DateTime: TDateTime; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'TimeToStr', @TimeToStr_0,
    'function(const DateTime: TDateTime): string');
  TSepiMethod.CreateOverloaded(Result, 'TimeToStr', @TimeToStr_1,
    'function(const DateTime: TDateTime; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'DateTimeToStr', @DateTimeToStr_0,
    'function(const DateTime: TDateTime): string');
  TSepiMethod.CreateOverloaded(Result, 'DateTimeToStr', @DateTimeToStr_1,
    'function(const DateTime: TDateTime; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'StrToDate', @StrToDate_0,
    'function(const S: string): TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'StrToDate', @StrToDate_1,
    'function(const S: string; const FormatSettings: TFormatSettings ) : TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'StrToDateDef', @StrToDateDef_0,
    'function(const S: string; const Default: TDateTime ) : TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'StrToDateDef', @StrToDateDef_1,
    'function(const S: string; const Default: TDateTime; const FormatSettings: TFormatSettings ) : TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToDate', @TryStrToDate_0,
    'function(const S: string; out Value: TDateTime): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToDate', @TryStrToDate_1,
    'function(const S: string; out Value: TDateTime; const FormatSettings: TFormatSettings ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'StrToTime', @StrToTime_0,
    'function(const S: string): TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'StrToTime', @StrToTime_1,
    'function(const S: string; const FormatSettings: TFormatSettings ) : TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'StrToTimeDef', @StrToTimeDef_0,
    'function(const S: string; const Default: TDateTime ) : TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'StrToTimeDef', @StrToTimeDef_1,
    'function(const S: string; const Default: TDateTime; const FormatSettings: TFormatSettings ) : TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToTime', @TryStrToTime_0,
    'function(const S: string; out Value: TDateTime): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToTime', @TryStrToTime_1,
    'function(const S: string; out Value: TDateTime; const FormatSettings: TFormatSettings ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'StrToDateTime', @StrToDateTime_0,
    'function(const S: string): TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'StrToDateTime', @StrToDateTime_1,
    'function(const S: string; const FormatSettings: TFormatSettings ) : TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'StrToDateTimeDef', @StrToDateTimeDef_0,
    'function(const S: string; const Default: TDateTime ) : TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'StrToDateTimeDef', @StrToDateTimeDef_1,
    'function(const S: string; const Default: TDateTime; const FormatSettings: TFormatSettings ) : TDateTime');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToDateTime', @TryStrToDateTime_0,
    'function(const S: string; out Value: TDateTime ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'TryStrToDateTime', @TryStrToDateTime_1,
    'function(const S: string; out Value: TDateTime; const FormatSettings: TFormatSettings ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'FormatDateTime', @FormatDateTime_0,
    'function(const Format: string; DateTime: TDateTime ) : string');
  TSepiMethod.CreateOverloaded(Result, 'FormatDateTime', @FormatDateTime_1,
    'function(const Format: string; DateTime: TDateTime; const FormatSettings: TFormatSettings ) : string');
  TSepiMethod.CreateOverloaded(Result, 'DateTimeToString', @DateTimeToString_0,
    'procedure(var Result: string; const Format: string; DateTime: TDateTime )');
  TSepiMethod.CreateOverloaded(Result, 'DateTimeToString', @DateTimeToString_1,
    'procedure(var Result: string; const Format: string; DateTime: TDateTime ; const FormatSettings: TFormatSettings )');

  // Constants
  TSepiVariable.Create(Result, 'MinDateTime',
    MinDateTime, TypeInfo(TDateTime), True);
  TSepiVariable.Create(Result, 'MaxDateTime',
    MaxDateTime, TypeInfo(TDateTime), True);

  // Routines
  TSepiMethod.Create(Result, 'FloatToDateTime', @FloatToDateTime,
    'function(const Value: Extended): TDateTime');
  TSepiMethod.Create(Result, 'TryFloatToDateTime', @TryFloatToDateTime,
    'function(const Value: Extended; out AResult: TDateTime): Boolean');
  TSepiMethod.Create(Result, 'SysErrorMessage', @SysErrorMessage,
    'function(ErrorCode: Integer): string');
  TSepiMethod.Create(Result, 'GetLocaleStr', @GetLocaleStr,
    'function(Locale, LocaleType: Integer; const Default: string): string');
  TSepiMethod.Create(Result, 'GetLocaleChar', @GetLocaleChar,
    'function(Locale, LocaleType: Integer; Default: Char): Char');
  TSepiMethod.Create(Result, 'GetFormatSettings', @GetFormatSettings,
    'procedure');
  TSepiMethod.Create(Result, 'GetLocaleFormatSettings',
    @GetLocaleFormatSettings,
    'procedure(LCID: Integer; var FormatSettings: TFormatSettings )');
  TSepiMethod.Create(Result, 'Sleep', @Sleep,
    'procedure(milliseconds: Cardinal)');
  TSepiMethod.Create(Result, 'GetModuleName', @GetModuleName,
    'function(Module: HMODULE): string');
  TSepiMethod.Create(Result, 'ExceptionErrorMessage', @ExceptionErrorMessage,
    'function(ExceptObject: TObject; ExceptAddr: Pointer; Buffer: PChar ; Size: Integer ) : Integer');
  TSepiMethod.Create(Result, 'ShowException', @ShowException,
    'procedure(ExceptObject: TObject; ExceptAddr: Pointer)');
  TSepiMethod.Create(Result, 'Abort', @Abort,
    'procedure');
  TSepiMethod.Create(Result, 'OutOfMemoryError', @OutOfMemoryError,
    'procedure');
  TSepiMethod.Create(Result, 'Beep', @Beep,
    'procedure');

  // Global variables
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_23));
  TSepiVariable.Create(Result, 'LeadBytes',
    LeadBytes, TypeInfo(TUnnamed_23));

  // Routines
  TSepiMethod.Create(Result, 'ByteType', @ByteType,
    'function(const S: string; Index: Integer): TMbcsByteType');
  TSepiMethod.Create(Result, 'StrByteType', @StrByteType,
    'function(Str: PChar; Index: Cardinal): TMbcsByteType');
  TSepiMethod.Create(Result, 'ByteToCharLen', @ByteToCharLen,
    'function(const S: string; MaxLen: Integer): Integer');
  TSepiMethod.Create(Result, 'CharToByteLen', @CharToByteLen,
    'function(const S: string; MaxLen: Integer): Integer');
  TSepiMethod.Create(Result, 'ByteToCharIndex', @ByteToCharIndex,
    'function(const S: string; Index: Integer): Integer');
  TSepiMethod.Create(Result, 'CharToByteIndex', @CharToByteIndex,
    'function(const S: string; Index: Integer): Integer');
  TSepiMethod.Create(Result, 'StrCharLength', @StrCharLength,
    'function(const Str: PChar): Integer');
  TSepiMethod.Create(Result, 'StrNextChar', @StrNextChar,
    'function(const Str: PChar): PChar');
  TSepiMethod.Create(Result, 'CharLength', @CharLength,
    'function(const S: String; Index: Integer): Integer');
  TSepiMethod.Create(Result, 'NextCharIndex', @NextCharIndex,
    'function(const S: String; Index: Integer): Integer');
  TSepiMethod.Create(Result, 'IsPathDelimiter', @IsPathDelimiter,
    'function(const S: string; Index: Integer): Boolean');
  TSepiMethod.Create(Result, 'IsDelimiter', @IsDelimiter,
    'function(const Delimiters, S: string; Index: Integer): Boolean');
  TSepiMethod.Create(Result, 'IncludeTrailingPathDelimiter',
    @IncludeTrailingPathDelimiter,
    'function(const S: string): string');
  TSepiMethod.Create(Result, 'IncludeTrailingBackslash',
    @IncludeTrailingBackslash,
    'function(const S: string): string');
  TSepiMethod.Create(Result, 'ExcludeTrailingPathDelimiter',
    @ExcludeTrailingPathDelimiter,
    'function(const S: string): string');
  TSepiMethod.Create(Result, 'ExcludeTrailingBackslash',
    @ExcludeTrailingBackslash,
    'function(const S: string): string');
  TSepiMethod.Create(Result, 'LastDelimiter', @LastDelimiter,
    'function(const Delimiters, S: string): Integer');
  TSepiMethod.Create(Result, 'AnsiCompareFileName', @AnsiCompareFileName,
    'function(const S1, S2: string): Integer');
  TSepiMethod.Create(Result, 'SameFileName', @SameFileName,
    'function(const S1, S2: string): Boolean');
  TSepiMethod.Create(Result, 'AnsiLowerCaseFileName', @AnsiLowerCaseFileName,
    'function(const S: string): string');
  TSepiMethod.Create(Result, 'AnsiUpperCaseFileName', @AnsiUpperCaseFileName,
    'function(const S: string): string');
  TSepiMethod.Create(Result, 'AnsiPos', @AnsiPos,
    'function(const Substr, S: string): Integer');
  TSepiMethod.Create(Result, 'AnsiStrPos', @AnsiStrPos,
    'function(Str, SubStr: PChar): PChar');
  TSepiMethod.Create(Result, 'AnsiStrRScan', @AnsiStrRScan,
    'function(Str: PChar; Chr: Char): PChar');
  TSepiMethod.Create(Result, 'AnsiStrScan', @AnsiStrScan,
    'function(Str: PChar; Chr: Char): PChar');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TReplaceFlags));

  // Routines
  TSepiMethod.Create(Result, 'StringReplace', @StringReplace,
    'function(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags ) : string');
  TSepiMethod.CreateOverloaded(Result, 'WrapText', @WrapText_0,
    'function(const Line, BreakStr: string; const BreakChars: TSysCharSet; MaxCol: Integer ) : string');
  TSepiMethod.CreateOverloaded(Result, 'WrapText', @WrapText_1,
    'function(const Line: string; MaxCol: Integer = 45): string');

  // Constants
  //TSepiConstant.Create(Result, 'SwitchChars', SwitchChars);

  // Routines
  TSepiMethod.CreateOverloaded(Result, 'FindCmdLineSwitch',
    @FindCmdLineSwitch_0,
    'function(const Switch: string; const Chars: TSysCharSet; IgnoreCase: Boolean ) : Boolean');
  TSepiMethod.CreateOverloaded(Result, 'FindCmdLineSwitch',
    @FindCmdLineSwitch_1,
    'function(const Switch: string): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'FindCmdLineSwitch',
    @FindCmdLineSwitch_2,
    'function(const Switch: string; IgnoreCase: Boolean): Boolean');
  TSepiMethod.Create(Result, 'FreeAndNil', @FreeAndNil,
    'procedure(var Obj)');
  TSepiMethod.CreateOverloaded(Result, 'Supports', @Supports_0,
    'function(const Instance: IInterface; const IID: TGUID; out Intf): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'Supports', @Supports_1,
    'function(const Instance: TObject; const IID: TGUID; out Intf): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'Supports', @Supports_2,
    'function(const Instance: IInterface; const IID: TGUID): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'Supports', @Supports_3,
    'function(const Instance: TObject; const IID: TGUID): Boolean');
  TSepiMethod.CreateOverloaded(Result, 'Supports', @Supports_4,
    'function(const AClass: TClass; const IID: TGUID): Boolean');
  TSepiMethod.Create(Result, 'CreateGUID', @CreateGUID,
    'function(out Guid: TGUID): HResult');
  TSepiMethod.Create(Result, 'StringToGUID', @StringToGUID,
    'function(const S: string): TGUID');
  TSepiMethod.Create(Result, 'GUIDToString', @GUIDToString,
    'function(const GUID: TGUID): string');
  TSepiMethod.Create(Result, 'IsEqualGUID', @IsEqualGUID,
    'function(const guid1, guid2: TGUID): Boolean');

  // Constants
  TSepiConstant.Create(Result, 'pfNeverBuild', pfNeverBuild);
  TSepiConstant.Create(Result, 'pfDesignOnly', pfDesignOnly);
  TSepiConstant.Create(Result, 'pfRunOnly', pfRunOnly);
  TSepiConstant.Create(Result, 'pfIgnoreDupUnits', pfIgnoreDupUnits);
  TSepiConstant.Create(Result, 'pfModuleTypeMask', pfModuleTypeMask);
  TSepiConstant.Create(Result, 'pfExeModule', pfExeModule);
  TSepiConstant.Create(Result, 'pfPackageModule', pfPackageModule);
  TSepiConstant.Create(Result, 'pfProducerMask', pfProducerMask);
  TSepiConstant.Create(Result, 'pfV3Produced', pfV3Produced);
  TSepiConstant.Create(Result, 'pfProducerUndefined', pfProducerUndefined);
  TSepiConstant.Create(Result, 'pfBCB4Produced', pfBCB4Produced);
  TSepiConstant.Create(Result, 'pfDelphi4Produced', pfDelphi4Produced);
  TSepiConstant.Create(Result, 'pfLibraryModule', pfLibraryModule);

  // Constants
  TSepiConstant.Create(Result, 'ufMainUnit', ufMainUnit);
  TSepiConstant.Create(Result, 'ufPackageUnit', ufPackageUnit);
  TSepiConstant.Create(Result, 'ufWeakUnit', ufWeakUnit);
  TSepiConstant.Create(Result, 'ufOrgWeakUnit', ufOrgWeakUnit);
  TSepiConstant.Create(Result, 'ufImplicitUnit', ufImplicitUnit);
  TSepiConstant.Create(Result, 'ufWeakPackageUnit', ufWeakPackageUnit);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TNameType));
  TSepiMethodRefType.Create(Result, 'TPackageInfoProc',
    'procedure(const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer)');

  // Routines
  TSepiMethod.Create(Result, 'LoadPackage', @LoadPackage,
    'function(const Name: string): HMODULE');
  TSepiMethod.Create(Result, 'UnloadPackage', @UnloadPackage,
    'procedure(Module: HMODULE)');
  TSepiMethod.Create(Result, 'GetPackageInfo', @GetPackageInfo,
    'procedure(Module: HMODULE; Param: Pointer; var Flags: Integer; InfoProc: TPackageInfoProc )');
  TSepiMethod.Create(Result, 'GetPackageDescription', @GetPackageDescription,
    'function(ModuleName: PChar): string');
  TSepiMethod.Create(Result, 'InitializePackage', @InitializePackage,
    'procedure(Module: HMODULE)');
  TSepiMethod.Create(Result, 'FinalizePackage', @FinalizePackage,
    'procedure(Module: HMODULE)');
  TSepiMethod.CreateOverloaded(Result, 'RaiseLastOSError', @RaiseLastOSError_0,
    'procedure');
  TSepiMethod.CreateOverloaded(Result, 'RaiseLastOSError', @RaiseLastOSError_1,
    'procedure(LastError: Integer)');
  TSepiMethod.Create(Result, 'Win32Check', @Win32Check,
    'function(RetVal: BOOL): BOOL');

  // Types
  TSepiMethodRefType.Create(Result, 'TTerminateProc',
    'function: Boolean');

  // Routines
  TSepiMethod.Create(Result, 'AddTerminateProc', @AddTerminateProc,
    'procedure(TermProc: TTerminateProc)');
  TSepiMethod.Create(Result, 'CallTerminateProcs', @CallTerminateProcs,
    'function: Boolean');
  TSepiMethod.Create(Result, 'GDAL', @GDAL,
    'function: LongWord');
  TSepiMethod.Create(Result, 'RCS', @RCS,
    'procedure');
  TSepiMethod.Create(Result, 'RPR', @RPR,
    'procedure');

  // Global variables
  TSepiVariable.Create(Result, 'HexDisplayPrefix',
    HexDisplayPrefix, TypeInfo(string));

  // Global variables
  TSepiMethodRefType.Create(Result, '$24',
    'function(Directory: PChar; var FreeAvailable, TotalSpace : TLargeInteger ; TotalFree: PLargeInteger ) : Bool',
    False, ccStdCall);
  TSepiVariable.Create(Result, 'GetDiskFreeSpaceEx',
    @GetDiskFreeSpaceEx, '$24');

  // Routines
  TSepiMethod.Create(Result, 'SafeLoadLibrary', @SafeLoadLibrary,
    'function(const FileName: string; ErrorMode: UINT = SEM_NOOPENFILEERRORBOX ) : HMODULE');

  // Types
  SepiImportIReadWriteSync(Result);
  TSepiImportsTSimpleRWSync.SepiImport(Result);

  // Types
  TSepiPointerType.Create(Result, 'PThreadInfo', 'TThreadInfo', True);
  SepiImportTThreadInfo(Result);
  TSepiArrayType.Create(Result, '$25',
    [0, 15], 'PThreadInfo', True);
  TSepiImportsTThreadLocalCounter.SepiImport(Result);

  // Types
  TSepiImportsTMultiReadExclusiveWriteSynchronizer.SepiImport(Result);

  // Types
  TSepiTypeAlias.Create(Result, 'TMREWSync',
    TypeInfo(TMultiReadExclusiveWriteSynchronizer));

  // Routines
  TSepiMethod.CreateOverloaded(Result, 'GetEnvironmentVariable',
    @GetEnvironmentVariable_0,
    'function(const Name: string): string');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('SysUtils', ImportUnit);
end.

