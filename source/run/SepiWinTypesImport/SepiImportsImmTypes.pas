{*
  Importe l'unité Imm dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsImmTypes;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, Imm;

implementation

{ You must not localize any of the strings this unit contains! }

{---------------------------}
{ tagCOMPOSITIONFORM import }
{---------------------------}

function SepiImporttagCOMPOSITIONFORM(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCOMPOSITIONFORM', False, True);

  with Result do
  begin
    AddField('dwStyle', System.TypeInfo(DWORD));
    AddField('ptCurrentPos', 'TPOINT');
    AddField('rcArea', 'TRECT');

    Complete;
  end;
end;

{-------------------------}
{ tagCANDIDATEFORM import }
{-------------------------}

function SepiImporttagCANDIDATEFORM(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCANDIDATEFORM', False, True);

  with Result do
  begin
    AddField('dwIndex', System.TypeInfo(DWORD));
    AddField('dwStyle', System.TypeInfo(DWORD));
    AddField('ptCurrentPos', 'TPOINT');
    AddField('rcArea', 'TRECT');

    Complete;
  end;
end;

{-------------------------}
{ tagCANDIDATELIST import }
{-------------------------}

function SepiImporttagCANDIDATELIST(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagCANDIDATELIST', False, True);

  with Result do
  begin
    AddField('dwSize', System.TypeInfo(DWORD));
    AddField('dwStyle', System.TypeInfo(DWORD));
    AddField('dwCount', System.TypeInfo(DWORD));
    AddField('dwSelection', System.TypeInfo(DWORD));
    AddField('dwPageStart', System.TypeInfo(DWORD));
    AddField('dwPageSize', System.TypeInfo(DWORD));
    AddField('dwOffset', '$1');

    Complete;
  end;
end;

{-------------------------}
{ tagREGISTERWORDA import }
{-------------------------}

function SepiImporttagREGISTERWORDA(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagREGISTERWORDA', False, True);

  with Result do
  begin
    AddField('lpReading', 'PAnsiChar');
    AddField('lpWord', 'PAnsiChar');

    Complete;
  end;
end;

{-------------------------}
{ tagREGISTERWORDW import }
{-------------------------}

function SepiImporttagREGISTERWORDW(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagREGISTERWORDW', False, True);

  with Result do
  begin
    AddField('lpReading', 'PWideChar');
    AddField('lpWord', 'PWideChar');

    Complete;
  end;
end;

{---------------------}
{ tagSTYLEBUFA import }
{---------------------}

function SepiImporttagSTYLEBUFA(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSTYLEBUFA', False, True);

  with Result do
  begin
    AddField('dwStyle', System.TypeInfo(DWORD));
    AddField('szDescription', '$2');

    Complete;
  end;
end;

{---------------------}
{ tagSTYLEBUFW import }
{---------------------}

function SepiImporttagSTYLEBUFW(Owner : TSepiUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'tagSTYLEBUFW', False, True);

  with Result do
  begin
    AddField('dwStyle', System.TypeInfo(DWORD));
    AddField('szDescription', '$3');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ImmTypes',
    ['Windows']);

  // Types
  TSepiTypeAlias.Create(Result, 'HIMC', TypeInfo(Integer));
  TSepiPointerType.Create(Result, 'PCompositionForm', 'TCompositionForm', True);
  SepiImporttagCOMPOSITIONFORM(Result);
  TSepiTypeAlias.Create(Result, 'TCompositionForm', 'tagCOMPOSITIONFORM');
  TSepiTypeAlias.Create(Result, 'COMPOSITIONFORM', 'tagCOMPOSITIONFORM');
  TSepiPointerType.Create(Result, 'PCandidateForm', 'TCandidateForm', True);
  SepiImporttagCANDIDATEFORM(Result);
  TSepiTypeAlias.Create(Result, 'TCandidateForm', 'tagCANDIDATEFORM');
  TSepiTypeAlias.Create(Result, 'CANDIDATEFORM', 'tagCANDIDATEFORM');
  TSepiPointerType.Create(Result, 'PCandidateList', 'TCandidateList', True);
  TSepiArrayType.Create(Result, '$1',
    [1, 1], TypeInfo(DWORD), True);
  SepiImporttagCANDIDATELIST(Result);
  TSepiTypeAlias.Create(Result, 'TCandidateList', 'tagCANDIDATELIST');
  TSepiTypeAlias.Create(Result, 'CANDIDATELIST', 'tagCANDIDATELIST');
  TSepiPointerType.Create(Result, 'PRegisterWordA', 'TRegisterWordA', True);
  TSepiPointerType.Create(Result, 'PRegisterWordW', 'TRegisterWordW', True);
  TSepiTypeAlias.Create(Result, 'PRegisterWord', 'PRegisterWordA');
  SepiImporttagREGISTERWORDA(Result);
  SepiImporttagREGISTERWORDW(Result);
  TSepiTypeAlias.Create(Result, 'tagREGISTERWORD', 'tagREGISTERWORDA');
  TSepiTypeAlias.Create(Result, 'TRegisterWordA', 'tagREGISTERWORDA');
  TSepiTypeAlias.Create(Result, 'TRegisterWordW', 'tagREGISTERWORDW');
  TSepiTypeAlias.Create(Result, 'TRegisterWord', 'TRegisterWordA');
  TSepiTypeAlias.Create(Result, 'REGISTERWORDA', 'tagREGISTERWORDA');
  TSepiTypeAlias.Create(Result, 'REGISTERWORDW', 'tagREGISTERWORDW');
  TSepiTypeAlias.Create(Result, 'REGISTERWORD', 'REGISTERWORDA');

  // Types
  TSepiPointerType.Create(Result, 'PStyleBufA', 'TStyleBufA', True);
  TSepiPointerType.Create(Result, 'PStyleBufW', 'TStyleBufW', True);
  TSepiTypeAlias.Create(Result, 'PStyleBuf', 'PStyleBufA');
  TSepiArrayType.Create(Result, '$2',
    [0, STYLE_DESCRIPTION_SIZE-1], TypeInfo(AnsiChar), True);
  SepiImporttagSTYLEBUFA(Result);
  TSepiArrayType.Create(Result, '$3',
    [0, STYLE_DESCRIPTION_SIZE-1], TypeInfo(WideChar), True);
  SepiImporttagSTYLEBUFW(Result);
  TSepiTypeAlias.Create(Result, 'tagSTYLEBUF', 'tagSTYLEBUFA');
  TSepiTypeAlias.Create(Result, 'TStyleBufA', 'tagSTYLEBUFA');
  TSepiTypeAlias.Create(Result, 'TStyleBufW', 'tagSTYLEBUFW');
  TSepiTypeAlias.Create(Result, 'TStyleBuf', 'TStyleBufA');
  TSepiTypeAlias.Create(Result, 'STYLEBUFA', 'tagSTYLEBUFA');
  TSepiTypeAlias.Create(Result, 'STYLEBUFW', 'tagSTYLEBUFW');
  TSepiTypeAlias.Create(Result, 'STYLEBUF', 'STYLEBUFA');

  // Types
  TSepiMethodRefType.Create(Result, 'RegisterWordEnumProcA',
    'Function(lpReading: PAnsiChar; dwStyle: DWORD; lpszString: PAnsiChar; lpData: pointer): integer');
  TSepiMethodRefType.Create(Result, 'RegisterWordEnumProcW',
    'Function(lpReading: PWideChar; dwStyle: DWORD; lpszString: PWideChar; lpData: pointer): integer');
  TSepiTypeAlias.Create(Result, 'RegisterWordEnumProc', 'RegisterWordEnumProcA');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ImmTypes', ImportUnit);
end.

