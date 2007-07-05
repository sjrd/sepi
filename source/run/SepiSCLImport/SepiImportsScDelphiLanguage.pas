{*
  Importe l'unité ScDelphiLanguage dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsScDelphiLanguage;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ScDelphiLanguage;

implementation

{ You must not localize any of the strings this unit contains! }

{------------------------}
{ TJmpInstruction import }
{------------------------}

function SepiImportTJmpInstruction(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TJmpInstruction', True, True);

  with Result do
  begin
    AddField('OpCode', System.TypeInfo(Byte));
    AddField('Argument', System.TypeInfo(integer));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScDelphiLanguage',
    ['SysUtils', 'TypInfo', 'ScUtils']);

  // Types
  SepiImportTJmpInstruction(Result);

  // Routines
  TSepiMetaMethod.Create(Result, 'CorrectIdentifier', @CorrectIdentifier,
    'function(const Ident : string) : boolean');
  TSepiMetaMethod.Create(Result, 'SetBit', @SetBit,
    'procedure(var Value : integer; const Bit : Byte)');
  TSepiMetaMethod.Create(Result, 'ClearBit', @ClearBit,
    'procedure(var Value : integer; const Bit : Byte)');
  TSepiMetaMethod.Create(Result, 'ToggleBit', @ToggleBit,
    'procedure(var Value : integer; const Bit : Byte)');
  TSepiMetaMethod.Create(Result, 'TestBit', @TestBit,
    'function(const Value : integer; const Bit : Byte) : boolean');
  TSepiMetaMethod.Create(Result, 'GetMethodFromName', @GetMethodFromName,
    'function(Obj : TObject; const MethodName : ShortString ) : TMethod');
  TSepiMetaMethod.Create(Result, 'GetClassVirtualCode', @GetClassVirtualCode,
    'function(AClass : TClass; VMTOffset : integer) : Pointer');
  TSepiMetaMethod.Create(Result, 'GetClassVirtualMethod', @GetClassVirtualMethod,
    'function(AClass : TClass; VMTOffset : integer) : TMethod');
  TSepiMetaMethod.Create(Result, 'GetObjectVirtualCode', @GetObjectVirtualCode,
    'function(AObject : TObject; VMTOffset : integer) : Pointer');
  TSepiMetaMethod.Create(Result, 'GetObjectVirtualMethod', @GetObjectVirtualMethod,
    'function(AObject : TObject; VMTOffset : integer ) : TMethod');
  TSepiMetaMethod.Create(Result, 'GetClassDynamicCode', @GetClassDynamicCode,
    'function(AClass : TClass; DMTIndex : integer) : Pointer');
  TSepiMetaMethod.Create(Result, 'GetClassDynamicMethod', @GetClassDynamicMethod,
    'function(AClass : TClass; DMTIndex : integer) : TMethod');
  TSepiMetaMethod.Create(Result, 'GetObjectDynamicCode', @GetObjectDynamicCode,
    'function(AObject : TObject; DMTIndex : integer) : Pointer');
  TSepiMetaMethod.Create(Result, 'GetObjectDynamicMethod', @GetObjectDynamicMethod,
    'function(AObject : TObject; DMTIndex : integer ) : TMethod');
  TSepiMetaMethod.Create(Result, 'StrToStrRepres', @StrToStrRepres,
    'function(const Str : string; ExcludedChars : TSysCharSet = [] ) : string');
  TSepiMetaMethod.Create(Result, 'StrRepresToStr', @StrRepresToStr,
    'function(Str : string) : string');
  TSepiMetaMethod.Create(Result, 'CharToCharRepres', @CharToCharRepres,
    'function(Chr : Char; ExcludedChars : TSysCharSet = [] ) : string');
  TSepiMetaMethod.Create(Result, 'CharRepresToChar', @CharRepresToChar,
    'function(Str : string) : Char');
  TSepiMetaMethod.Create(Result, 'CharSetToStr', @CharSetToStr,
    'function(const CharSet : TSysCharSet) : string');
  TSepiMetaMethod.Create(Result, 'StrToCharSet', @StrToCharSet,
    'function(Str : string) : TSysCharSet');
  TSepiMetaMethod.Create(Result, 'EnumSetToStr', @EnumSetToStr,
    'function(const EnumSet; TypeInfo : PTypeInfo) : string');
  TSepiMetaMethod.Create(Result, 'StrToEnumSet', @StrToEnumSet,
    'procedure(const Str : string; TypeInfo : PTypeInfo; out EnumSet)');
  TSepiMetaMethod.Create(Result, 'ExplicitInitialize', @ExplicitInitialize,
    'procedure(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1 )');
  TSepiMetaMethod.Create(Result, 'ExplicitFinalize', @ExplicitFinalize,
    'procedure(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1 )');
  TSepiMetaMethod.Create(Result, 'SkipPackedShortString', @SkipPackedShortString,
    'function(Value : PShortstring) : Pointer');
  TSepiMetaMethod.Create(Result, 'JmpArgument', @JmpArgument,
    'function(JmpAddress, JmpDest : Pointer) : integer');
  TSepiMetaMethod.Create(Result, 'MakeJmp', @MakeJmp,
    'procedure(var Instruction; Dest : Pointer)');
  TSepiMetaMethod.Create(Result, 'MakeCall', @MakeCall,
    'procedure(var Instruction; Dest : Pointer)');
  TSepiMetaMethod.Create(Result, 'MakeProcOfRegisterMethod', @MakeProcOfRegisterMethod,
    'function(const Method : TMethod; UsedRegCount : Byte ) : Pointer');
  TSepiMetaMethod.Create(Result, 'MakeProcOfStdCallMethod', @MakeProcOfStdCallMethod,
    'function(const Method : TMethod) : Pointer');
  TSepiMetaMethod.Create(Result, 'MakeProcOfPascalMethod', @MakeProcOfPascalMethod,
    'function(const Method : TMethod) : Pointer');
  TSepiMetaMethod.Create(Result, 'MakeProcOfCDeclMethod', @MakeProcOfCDeclMethod,
    'function(const Method : TMethod) : Pointer');
  TSepiMetaMethod.Create(Result, 'ClearCDeclCallInfo', @ClearCDeclCallInfo,
    'procedure');
  TSepiMetaMethod.Create(Result, 'FreeProcOfMethod', @FreeProcOfMethod,
    'procedure(Proc : Pointer)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScDelphiLanguage', ImportUnit);
end.

