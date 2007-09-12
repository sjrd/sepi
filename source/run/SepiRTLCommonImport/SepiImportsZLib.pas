{*
  Importe l'unité ZLib dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsZLib;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ZLib, Classes;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTCustomZlibStream = class(TCustomZlibStream)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTCompressionStream = class(TCompressionStream)
  private
    function GetCompressionRate: Single;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTDecompressionStream = class(TDecompressionStream)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEZlibError = class(EZlibError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsECompressionError = class(ECompressionError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsEDecompressionError = class(EDecompressionError)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{--------------------}
{ TZStreamRec import }
{--------------------}

function SepiImportTZStreamRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TZStreamRec', True, True);

  with Result do
  begin
    AddField('next_in', 'PChar');
    AddField('avail_in', System.TypeInfo(Integer));
    AddField('total_in', System.TypeInfo(Integer));
    AddField('next_out', 'PChar');
    AddField('avail_out', System.TypeInfo(Integer));
    AddField('total_out', System.TypeInfo(Integer));
    AddField('msg', 'PChar');
    AddField('internal', 'Pointer');
    AddField('zalloc', 'TAlloc');
    AddField('zfree', 'TFree');
    AddField('AppData', 'Pointer');
    AddField('data_type', System.TypeInfo(Integer));
    AddField('adler', System.TypeInfo(Integer));
    AddField('reserved', System.TypeInfo(Integer));

    Complete;
  end;
end;

{--------------------------}
{ TCustomZlibStream import }
{--------------------------}

class function TSepiImportsTCustomZlibStream.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCustomZlibStream));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FStrm', System.TypeInfo(TStream));
    AddField('FStrmPos', System.TypeInfo(Integer));
    AddField('FOnProgress', System.TypeInfo(TNotifyEvent));
    AddField('FZRec', 'TZStreamRec');
    AddField('FBuffer', '$1');

    CurrentVisibility := mvProtected;

    AddMethod('Progress', @TSepiImportsTCustomZlibStream.Progress,
      'procedure(Sender: TObject)',
      mlkDynamic);

    AddProperty('OnProgress', 'property: TNotifyEvent',
      'FOnProgress', 'FOnProgress');

    AddMethod('Create', @TSepiImportsTCustomZlibStream.Create,
      'constructor(Strm: TStream)');

    Complete;
  end;
end;

{---------------------------}
{ TCompressionStream import }
{---------------------------}

function TSepiImportsTCompressionStream.GetCompressionRate: Single;
begin
  Result := CompressionRate;
end;

class function TSepiImportsTCompressionStream.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TCompressionStream));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetCompressionRate',
      @TSepiImportsTCompressionStream.GetCompressionRate,
      'function: Single');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTCompressionStream.Create,
      'constructor(CompressionLevel: TCompressionLevel; Dest: TStream)');
    AddMethod('Destroy', @TSepiImportsTCompressionStream.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Read', @TSepiImportsTCompressionStream.Read,
      'function(var Buffer; Count: Longint): Longint',
      mlkOverride);
    AddMethod('Write', @TSepiImportsTCompressionStream.Write,
      'function(const Buffer; Count: Longint): Longint',
      mlkOverride);
    AddMethod('Seek', @TSepiImportsTCompressionStream.Seek,
      'function(Offset: Longint; Origin: Word): Longint',
      mlkOverride);

    AddProperty('CompressionRate', 'property: Single',
      'GetCompressionRate', '');
    RedefineProperty('OnProgress');

    Complete;
  end;
end;

{-----------------------------}
{ TDecompressionStream import }
{-----------------------------}

class function TSepiImportsTDecompressionStream.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDecompressionStream));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTDecompressionStream.Create,
      'constructor(Source: TStream)');
    AddMethod('Destroy', @TSepiImportsTDecompressionStream.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Read', @TSepiImportsTDecompressionStream.Read,
      'function(var Buffer; Count: Longint): Longint',
      mlkOverride);
    AddMethod('Write', @TSepiImportsTDecompressionStream.Write,
      'function(const Buffer; Count: Longint): Longint',
      mlkOverride);
    AddMethod('Seek', @TSepiImportsTDecompressionStream.Seek,
      'function(Offset: Longint; Origin: Word): Longint',
      mlkOverride);

    RedefineProperty('OnProgress');

    Complete;
  end;
end;

{-------------------}
{ EZlibError import }
{-------------------}

class function TSepiImportsEZlibError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EZlibError));

  with Result do
  begin

    Complete;
  end;
end;

{--------------------------}
{ ECompressionError import }
{--------------------------}

class function TSepiImportsECompressionError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ECompressionError));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------------}
{ EDecompressionError import }
{----------------------------}

class function TSepiImportsEDecompressionError.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EDecompressionError));

  with Result do
  begin

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ZLib',
    ['SysUtils', 'Classes']);

  // Types
  TSepiMethodRefType.Create(Result, 'TAlloc',
    'function(AppData: Pointer; Items, Size: Integer): Pointer',
    False, ccRegister);
  TSepiMethodRefType.Create(Result, 'TFree',
    'procedure(AppData, Block: Pointer)', False, ccRegister);
  SepiImportTZStreamRec(Result);
  TSepiArrayType.Create(Result, '$1',
    [Integer(Low(Word)), Integer(High(Word))], TypeInfo(Char), True);
  TSepiImportsTCustomZlibStream.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TCompressionLevel));
  TSepiImportsTCompressionStream.SepiImport(Result);
  TSepiImportsTDecompressionStream.SepiImport(Result);

  // Routines
  TSepiMethod.Create(Result, 'CompressBuf', @CompressBuf,
    'procedure(const InBuf: Pointer; InBytes: Integer; out OutBuf: Pointer ; out OutBytes: Integer )');
  TSepiMethod.Create(Result, 'DecompressBuf', @DecompressBuf,
    'procedure(const InBuf: Pointer; InBytes: Integer; OutEstimate: Integer ; out OutBuf: Pointer ; out OutBytes: Integer )');
  TSepiMethod.Create(Result, 'DecompressToUserBuf', @DecompressToUserBuf,
    'procedure(const InBuf: Pointer; InBytes: Integer; const OutBuf: Pointer ; BufSize: Integer )');

  // Constants
  TSepiConstant.Create(Result, 'zlib_Version', zlib_Version);
  TSepiConstant.Create(Result, 'Z_NO_FLUSH', Z_NO_FLUSH);
  TSepiConstant.Create(Result, 'Z_PARTIAL_FLUSH', Z_PARTIAL_FLUSH);
  TSepiConstant.Create(Result, 'Z_SYNC_FLUSH', Z_SYNC_FLUSH);
  TSepiConstant.Create(Result, 'Z_FULL_FLUSH', Z_FULL_FLUSH);
  TSepiConstant.Create(Result, 'Z_FINISH', Z_FINISH);
  TSepiConstant.Create(Result, 'Z_OK', Z_OK);
  TSepiConstant.Create(Result, 'Z_STREAM_END', Z_STREAM_END);
  TSepiConstant.Create(Result, 'Z_NEED_DICT', Z_NEED_DICT);
  TSepiConstant.Create(Result, 'Z_ERRNO', Z_ERRNO);
  TSepiConstant.Create(Result, 'Z_STREAM_ERROR', Z_STREAM_ERROR);
  TSepiConstant.Create(Result, 'Z_DATA_ERROR', Z_DATA_ERROR);
  TSepiConstant.Create(Result, 'Z_MEM_ERROR', Z_MEM_ERROR);
  TSepiConstant.Create(Result, 'Z_BUF_ERROR', Z_BUF_ERROR);
  TSepiConstant.Create(Result, 'Z_VERSION_ERROR', Z_VERSION_ERROR);
  TSepiConstant.Create(Result, 'Z_NO_COMPRESSION', Z_NO_COMPRESSION);
  TSepiConstant.Create(Result, 'Z_BEST_SPEED', Z_BEST_SPEED);
  TSepiConstant.Create(Result, 'Z_BEST_COMPRESSION', Z_BEST_COMPRESSION);
  TSepiConstant.Create(Result, 'Z_DEFAULT_COMPRESSION', Z_DEFAULT_COMPRESSION);
  TSepiConstant.Create(Result, 'Z_FILTERED', Z_FILTERED);
  TSepiConstant.Create(Result, 'Z_HUFFMAN_ONLY', Z_HUFFMAN_ONLY);
  TSepiConstant.Create(Result, 'Z_DEFAULT_STRATEGY', Z_DEFAULT_STRATEGY);
  TSepiConstant.Create(Result, 'Z_BINARY', Z_BINARY);
  TSepiConstant.Create(Result, 'Z_ASCII', Z_ASCII);
  TSepiConstant.Create(Result, 'Z_UNKNOWN', Z_UNKNOWN);
  TSepiConstant.Create(Result, 'Z_DEFLATED', Z_DEFLATED);

  // Types
  TSepiImportsEZlibError.SepiImport(Result);
  TSepiImportsECompressionError.SepiImport(Result);
  TSepiImportsEDecompressionError.SepiImport(Result);

  // Routines
  TSepiMethod.Create(Result, 'zlibAllocMem', @zlibAllocMem,
    'function(AppData: Pointer; Items, Size: Integer): Pointer');
  TSepiMethod.Create(Result, 'zlibFreeMem', @zlibFreeMem,
    'procedure(AppData, Block: Pointer)');
  TSepiMethod.Create(Result, 'deflateInit_', @deflateInit_,
    'function(var strm: TZStreamRec; level: Integer; version: PChar; recsize: Integer ) : Integer');
  TSepiMethod.Create(Result, 'deflate', @deflate,
    'function(var strm: TZStreamRec; flush: Integer): Integer');
  TSepiMethod.Create(Result, 'deflateEnd', @deflateEnd,
    'function(var strm: TZStreamRec): Integer');
  TSepiMethod.Create(Result, 'inflateInit_', @inflateInit_,
    'function(var strm: TZStreamRec; version: PChar; recsize: Integer ) : Integer');
  TSepiMethod.Create(Result, 'inflate', @inflate,
    'function(var strm: TZStreamRec; flush: Integer): Integer');
  TSepiMethod.Create(Result, 'inflateEnd', @inflateEnd,
    'function(var strm: TZStreamRec): Integer');
  TSepiMethod.Create(Result, 'inflateReset', @inflateReset,
    'function(var strm: TZStreamRec): Integer');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ZLib', ImportUnit);
end.

