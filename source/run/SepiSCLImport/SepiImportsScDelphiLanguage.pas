{*
  Importe l'unité ScDelphiLanguage dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScDelphiLanguage;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScDelphiLanguage;

implementation

{ You must not localize any of the strings this unit contains! }

{------------------------}
{ TJmpInstruction import }
{------------------------}

function SepiImportTJmpInstruction(Owner : TSepiUnit) : TSepiRecordType;
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

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ScDelphiLanguage',
    ['SysUtils', 'TypInfo', 'ScUtils']);

  // Types
  SepiImportTJmpInstruction(Result);

  // Routines
  TSepiMethod.Create(Result, 'CorrectIdentifier', @CorrectIdentifier,
    'function(const Ident : string) : boolean');
  TSepiMethod.Create(Result, 'SetBit', @SetBit,
    'procedure(var Value : integer; const Bit : Byte)');
  TSepiMethod.Create(Result, 'ClearBit', @ClearBit,
    'procedure(var Value : integer; const Bit : Byte)');
  TSepiMethod.Create(Result, 'ToggleBit', @ToggleBit,
    'procedure(var Value : integer; const Bit : Byte)');
  TSepiMethod.Create(Result, 'TestBit', @TestBit,
    'function(const Value : integer; const Bit : Byte) : boolean');
  TSepiMethod.Create(Result, 'GetMethodFromName', @GetMethodFromName,
    'function(Obj : TObject; const MethodName : ShortString ) : TMethod');
  TSepiMethod.Create(Result, 'GetClassVirtualCode', @GetClassVirtualCode,
    'function(AClass : TClass; VMTOffset : integer) : Pointer');
  TSepiMethod.Create(Result, 'GetClassVirtualMethod', @GetClassVirtualMethod,
    'function(AClass : TClass; VMTOffset : integer) : TMethod');
  TSepiMethod.Create(Result, 'GetObjectVirtualCode', @GetObjectVirtualCode,
    'function(AObject : TObject; VMTOffset : integer) : Pointer');
  TSepiMethod.Create(Result, 'GetObjectVirtualMethod', @GetObjectVirtualMethod,
    'function(AObject : TObject; VMTOffset : integer ) : TMethod');
  TSepiMethod.Create(Result, 'GetClassDynamicCode', @GetClassDynamicCode,
    'function(AClass : TClass; DMTIndex : integer) : Pointer');
  TSepiMethod.Create(Result, 'GetClassDynamicMethod', @GetClassDynamicMethod,
    'function(AClass : TClass; DMTIndex : integer) : TMethod');
  TSepiMethod.Create(Result, 'GetObjectDynamicCode', @GetObjectDynamicCode,
    'function(AObject : TObject; DMTIndex : integer) : Pointer');
  TSepiMethod.Create(Result, 'GetObjectDynamicMethod', @GetObjectDynamicMethod,
    'function(AObject : TObject; DMTIndex : integer ) : TMethod');
  TSepiMethod.Create(Result, 'StrToStrRepres', @StrToStrRepres,
    'function(const Str : string; ExcludedChars : TSysCharSet = [] ) : string');
  TSepiMethod.Create(Result, 'StrRepresToStr', @StrRepresToStr,
    'function(Str : string) : string');
  TSepiMethod.Create(Result, 'CharToCharRepres', @CharToCharRepres,
    'function(Chr : Char; ExcludedChars : TSysCharSet = [] ) : string');
  TSepiMethod.Create(Result, 'CharRepresToChar', @CharRepresToChar,
    'function(Str : string) : Char');
  TSepiMethod.Create(Result, 'CharSetToStr', @CharSetToStr,
    'function(const CharSet : TSysCharSet) : string');
  TSepiMethod.Create(Result, 'StrToCharSet', @StrToCharSet,
    'function(Str : string) : TSysCharSet');
  TSepiMethod.Create(Result, 'EnumSetToStr', @EnumSetToStr,
    'function(const EnumSet; TypeInfo : PTypeInfo) : string');
  TSepiMethod.Create(Result, 'StrToEnumSet', @StrToEnumSet,
    'procedure(const Str : string; TypeInfo : PTypeInfo; out EnumSet)');
  TSepiMethod.Create(Result, 'SkipPackedShortString', @SkipPackedShortString,
    'function(Value : PShortstring) : Pointer');
  TSepiMethod.Create(Result, 'JmpArgument', @JmpArgument,
    'function(JmpAddress, JmpDest : Pointer) : integer');
  TSepiMethod.Create(Result, 'MakeJmp', @MakeJmp,
    'procedure(var Instruction; Dest : Pointer)');
  TSepiMethod.Create(Result, 'MakeCall', @MakeCall,
    'procedure(var Instruction; Dest : Pointer)');
  TSepiMethod.Create(Result, 'MakeProcOfRegisterMethod', @MakeProcOfRegisterMethod,
    'function(const Method : TMethod; UsedRegCount : Byte ) : Pointer');
  TSepiMethod.Create(Result, 'MakeProcOfStdCallMethod', @MakeProcOfStdCallMethod,
    'function(const Method : TMethod) : Pointer');
  TSepiMethod.Create(Result, 'MakeProcOfPascalMethod', @MakeProcOfPascalMethod,
    'function(const Method : TMethod) : Pointer');
  TSepiMethod.Create(Result, 'MakeProcOfCDeclMethod', @MakeProcOfCDeclMethod,
    'function(const Method : TMethod) : Pointer');
  TSepiMethod.Create(Result, 'ClearCDeclCallInfo', @ClearCDeclCallInfo,
    'procedure');
  TSepiMethod.Create(Result, 'FreeProcOfMethod', @FreeProcOfMethod,
    'procedure(Proc : Pointer)');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScDelphiLanguage', ImportUnit);
end.

