{*
  Importe l'unité WideStrUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsWideStrUtils;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, WideStrUtils, Classes;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function HasUTF8BOM_0(S: TStream): Boolean;
begin
  Result := HasUTF8BOM(S);
end;

function HasUTF8BOM_1(S: AnsiString): Boolean;
begin
  Result := HasUTF8BOM(S);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'WideStrUtils',
    ['Windows', 'SysUtils', 'Classes']);

  // Routines
  TSepiMethod.Create(Result, 'WStrAlloc', @WStrAlloc,
    'function(Size: Cardinal): PWideChar');
  TSepiMethod.Create(Result, 'WStrBufSize', @WStrBufSize,
    'function(const Str: PWideChar): Cardinal');
  TSepiMethod.Create(Result, 'WStrNew', @WStrNew,
    'function(const Str: PWideChar): PWideChar');
  TSepiMethod.Create(Result, 'WStrDispose', @WStrDispose,
    'procedure(Str: PWideChar)');
  TSepiMethod.Create(Result, 'WStrLen', @WStrLen,
    'function(const Str: PWideChar): Cardinal');
  TSepiMethod.Create(Result, 'WStrEnd', @WStrEnd,
    'function(const Str: PWideChar): PWideChar');
  TSepiMethod.Create(Result, 'WStrCopy', @WStrCopy,
    'function(Dest: PWideChar; const Source: PWideChar): PWideChar');
  TSepiMethod.Create(Result, 'WStrLCopy', @WStrLCopy,
    'function(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar');
  TSepiMethod.Create(Result, 'WStrPCopy', @WStrPCopy,
    'function(Dest: PWideChar; const Source: WideString): PWideChar');
  TSepiMethod.Create(Result, 'WStrPLCopy', @WStrPLCopy,
    'function(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar');
  TSepiMethod.Create(Result, 'UTF8LowerCase', @UTF8LowerCase,
    'function(const S: UTF8string): UTF8string');
  TSepiMethod.Create(Result, 'UTF8UpperCase', @UTF8UpperCase,
    'function(const S: UTF8string): UTF8string');
  TSepiMethod.Create(Result, 'AnsiToUtf8Ex', @AnsiToUtf8Ex,
    'function(const S: string; const cp : integer): UTF8String');
  TSepiMethod.Create(Result, 'Utf8ToAnsiEx', @Utf8ToAnsiEx,
    'function(const S: UTF8String; const cp : integer): string');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEncodeType));

  // Routines
  TSepiMethod.Create(Result, 'DetectUTF8Encoding', @DetectUTF8Encoding,
    'function(const s : UTF8String): TEncodeType');
  TSepiMethod.Create(Result, 'IsUTF8String', @IsUTF8String,
    'function(const s : UTF8String): Boolean');
  TSepiMethod.Create(Result, 'HasExtendCharacter', @HasExtendCharacter,
    'function(const s : UTF8String): Boolean');
  TSepiOverloadedMethod.Create(Result, 'HasUTF8BOM');
  TSepiMethod.Create(Result, 'OL$HasUTF8BOM$0', @HasUTF8BOM_0,
    'function(S : TStream) : boolean');
  TSepiMethod.Create(Result, 'OL$HasUTF8BOM$1', @HasUTF8BOM_1,
    'function(S : AnsiString) : boolean');
  TSepiMethod.Create(Result, 'ConvertStreamFromAnsiToUTF8',
    @ConvertStreamFromAnsiToUTF8,
    'procedure(Src, Dst : TStream; cp : integer = CP_ACP)');
  TSepiMethod.Create(Result, 'WideQuotedStr', @WideQuotedStr,
    'function(const S: WideString; Quote: WideChar): WideString');
  TSepiMethod.Create(Result, 'WideExtractQuotedStr', @WideExtractQuotedStr,
    'function(var Src: PWideChar; Quote: WideChar): WideString');
  TSepiMethod.Create(Result, 'WideStringReplace', @WideStringReplace,
    'function(const S, OldPattern, NewPattern: Widestring; Flags: TReplaceFlags ) : Widestring');
  TSepiMethod.Create(Result, 'WideReplaceStr', @WideReplaceStr,
    'function(const AText, AFromText, AToText: WideString): WideString');
  TSepiMethod.Create(Result, 'WideReplaceText', @WideReplaceText,
    'function(const AText, AFromText, AToText: WideString): WideString');
  TSepiMethod.Create(Result, 'LoadWideStr', @LoadWideStr,
    'function(Ident: Integer): WideString');
  TSepiMethod.Create(Result, 'LoadResWideString', @LoadResWideString,
    'function(ResStringRec: PResStringRec): WideString');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(CharSet));

  // Routines
  TSepiMethod.Create(Result, 'inOpSet', @inOpSet,
    'function( W : WideChar; sets : CharSet ) : boolean');
  TSepiMethod.Create(Result, 'inOpArray', @inOpArray,
    'function( W : WideChar; sets : array of WideChar ) : boolean');

  // Constants
  TSepiArrayType.Create(Result, '$1',
    [1, 3], TypeInfo(char), True);
  TSepiVariable.Create(Result, 'sUTF8BOMString',
    sUTF8BOMString, '$1', True);

  // Routines
  TSepiMethod.Create(Result, 'IsUTF8LeadByte', @IsUTF8LeadByte,
    'function(Lead: Char): Boolean');
  TSepiMethod.Create(Result, 'IsUTF8TrailByte', @IsUTF8TrailByte,
    'function(Lead: Char): Boolean');
  TSepiMethod.Create(Result, 'UTF8CharSize', @UTF8CharSize,
    'function(Lead: Char): Integer');
  TSepiMethod.Create(Result, 'UTF8CharLength', @UTF8CharLength,
    'function(Lead: Char): Integer');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('WideStrUtils', ImportUnit);
end.

