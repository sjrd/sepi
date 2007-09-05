{*
  Importe l'unité ScCompilerMagic dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScCompilerMagic;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScCompilerMagic;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScCompilerMagic',
    ['TypInfo']);

  // Routines
  TSepiMetaMethod.Create(Result, 'AbstractError', @AbstractError,
    'procedure');
  TSepiMetaMethod.Create(Result, 'Initialize', @Initialize,
    'procedure(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1)');
  TSepiMetaMethod.Create(Result, 'Finalize', @Finalize,
    'procedure(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1)');
  TSepiMetaMethod.Create(Result, 'CopyArray', @CopyArray,
    'procedure(Dest, Source, TypeInfo : Pointer; Count : integer)');
  TSepiMetaMethod.Create(Result, 'CopyRecord', @CopyRecord,
    'procedure(Dest, Source, TypeInfo : Pointer)');
  TSepiMetaMethod.Create(Result, 'DynArrayCopy', @DynArrayCopy,
    'procedure(Source : Pointer; TypeInfo : Pointer; var Dest : Pointer )');
  TSepiMetaMethod.Create(Result, 'DynArrayCopyRange', @DynArrayCopyRange,
    'procedure(Source : Pointer; TypeInfo : Pointer; Index, Count : integer ; var Dest : Pointer )');
  TSepiMetaMethod.Create(Result, 'CompilerMagicRoutineAddress', @CompilerMagicRoutineAddress,
    'function( CompilerMagicRoutineAlias : Pointer ) : Pointer');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScCompilerMagic', ImportUnit);
end.

