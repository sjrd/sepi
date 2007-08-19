{*
  Importe l'unité WideStrUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsWideStrUtils;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, WideStrUtils, Classes;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function HasUTF8BOM_0(S : TStream) : boolean;
begin
  Result := HasUTF8BOM(S);
end;

function HasUTF8BOM_1(S : AnsiString) : boolean;
begin
  Result := HasUTF8BOM(S);
end;

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'WideStrUtils',
    ['Windows', 'SysUtils', 'Classes']);

  // Routines
  TSepiMetaMethod.Create(Result, 'WStrAlloc', @WStrAlloc,
    'function(Size: Cardinal): PWideChar');
  TSepiMetaMethod.Create(Result, 'WStrBufSize', @WStrBufSize,
    'function(const Str: PWideChar): Cardinal');
  TSepiMetaMethod.Create(Result, 'WStrNew', @WStrNew,
    'function(const Str: PWideChar): PWideChar');
  TSepiMetaMethod.Create(Result, 'WStrDispose', @WStrDispose,
    'procedure(Str: PWideChar)');
  TSepiMetaMethod.Create(Result, 'WStrLen', @WStrLen,
    'function(const Str: PWideChar): Cardinal');
  TSepiMetaMethod.Create(Result, 'WStrEnd', @WStrEnd,
    'function(const Str: PWideChar): PWideChar');
  TSepiMetaMethod.Create(Result, 'WStrCopy', @WStrCopy,
    'function(Dest: PWideChar; const Source: PWideChar): PWideChar');
  TSepiMetaMethod.Create(Result, 'WStrLCopy', @WStrLCopy,
    'function(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar');
  TSepiMetaMethod.Create(Result, 'WStrPCopy', @WStrPCopy,
    'function(Dest: PWideChar; const Source: WideString): PWideChar');
  TSepiMetaMethod.Create(Result, 'WStrPLCopy', @WStrPLCopy,
    'function(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar');
  TSepiMetaMethod.Create(Result, 'UTF8LowerCase', @UTF8LowerCase,
    'function(const S: UTF8string): UTF8string');
  TSepiMetaMethod.Create(Result, 'UTF8UpperCase', @UTF8UpperCase,
    'function(const S: UTF8string): UTF8string');
  TSepiMetaMethod.Create(Result, 'AnsiToUtf8Ex', @AnsiToUtf8Ex,
    'function(const S: string; const cp : integer): UTF8String');
  TSepiMetaMethod.Create(Result, 'Utf8ToAnsiEx', @Utf8ToAnsiEx,
    'function(const S: UTF8String; const cp : integer): string');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEncodeType));

  // Routines
  TSepiMetaMethod.Create(Result, 'DetectUTF8Encoding', @DetectUTF8Encoding,
    'function(const s : UTF8String): TEncodeType');
  TSepiMetaMethod.Create(Result, 'IsUTF8String', @IsUTF8String,
    'function(const s : UTF8String): Boolean');
  TSepiMetaMethod.Create(Result, 'HasExtendCharacter', @HasExtendCharacter,
    'function(const s : UTF8String): Boolean');
  TSepiMetaOverloadedMethod.Create(Result, 'HasUTF8BOM');
  TSepiMetaMethod.Create(Result, 'OL$HasUTF8BOM$0', @HasUTF8BOM_0,
    'function(S : TStream) : boolean');
  TSepiMetaMethod.Create(Result, 'OL$HasUTF8BOM$1', @HasUTF8BOM_1,
    'function(S : AnsiString) : boolean');
  TSepiMetaMethod.Create(Result, 'ConvertStreamFromAnsiToUTF8', @ConvertStreamFromAnsiToUTF8,
    'procedure(Src, Dst : TStream; cp : integer = CP_ACP)');
  TSepiMetaMethod.Create(Result, 'WideQuotedStr', @WideQuotedStr,
    'function(const S: WideString; Quote: WideChar): WideString');
  TSepiMetaMethod.Create(Result, 'WideExtractQuotedStr', @WideExtractQuotedStr,
    'function(var Src: PWideChar; Quote: WideChar): WideString');
  TSepiMetaMethod.Create(Result, 'WideStringReplace', @WideStringReplace,
    'function(const S, OldPattern, NewPattern: Widestring; Flags: TReplaceFlags ) : Widestring');
  TSepiMetaMethod.Create(Result, 'WideReplaceStr', @WideReplaceStr,
    'function(const AText, AFromText, AToText: WideString): WideString');
  TSepiMetaMethod.Create(Result, 'WideReplaceText', @WideReplaceText,
    'function(const AText, AFromText, AToText: WideString): WideString');
  TSepiMetaMethod.Create(Result, 'LoadWideStr', @LoadWideStr,
    'function(Ident: Integer): WideString');
  TSepiMetaMethod.Create(Result, 'LoadResWideString', @LoadResWideString,
    'function(ResStringRec: PResStringRec): WideString');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(CharSet));

  // Routines
  TSepiMetaMethod.Create(Result, 'inOpSet', @inOpSet,
    'function( W : WideChar; sets : CharSet ) : boolean');
  TSepiMetaMethod.Create(Result, 'inOpArray', @inOpArray,
    'function( W : WideChar; sets : array of WideChar ) : boolean');

  // Constants
  TSepiArrayType.Create(Result, '$1',
    [1, 3], TypeInfo(char), True);
  TSepiVariable.Create(Result, 'sUTF8BOMString',
     sUTF8BOMString, '$1', True);

  // Routines
  TSepiMetaMethod.Create(Result, 'IsUTF8LeadByte', @IsUTF8LeadByte,
    'function(Lead: Char): Boolean');
  TSepiMetaMethod.Create(Result, 'IsUTF8TrailByte', @IsUTF8TrailByte,
    'function(Lead: Char): Boolean');
  TSepiMetaMethod.Create(Result, 'UTF8CharSize', @UTF8CharSize,
    'function(Lead: Char): Integer');
  TSepiMetaMethod.Create(Result, 'UTF8CharLength', @UTF8CharLength,
    'function(Lead: Char): Integer');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('WideStrUtils', ImportUnit);
end.

