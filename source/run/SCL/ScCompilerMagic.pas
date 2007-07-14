{*
  Propose une série d'alias aux routines cachées de System
  Ces routines sont celles connues sous le nom de "compiler magic".
  @author sjrd
  @version 1.0
*}
unit ScCompilerMagic;

interface

uses
  TypInfo;

procedure AbstractError;

procedure Initialize(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1);
procedure Finalize(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1);

procedure CopyArray(Dest, Source, TypeInfo : Pointer; Count : integer);
procedure CopyRecord(Dest, Source, TypeInfo : Pointer);
procedure DynArrayCopy(Source : Pointer; TypeInfo : Pointer;
  var Dest : Pointer);
procedure DynArrayCopyRange(Source : Pointer; TypeInfo : Pointer;
  Index, Count : integer; var Dest : Pointer);

function CompilerMagicRoutineAddress(
  CompilerMagicRoutineAlias : Pointer) : Pointer;

implementation

{*
  Déclenche une erreur abstraite - alias de @AbstractError
*}
procedure AbstractError;
asm
        JMP     System.@AbstractError
end;

{*
  Initialise une variable - alias de @InitializeArray
  @param Value      Variable à initialiser
  @param TypeInfo   RTTI du type de la variable
  @param Count      Nombre d'éléments dans la variable
*}
procedure Initialize(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1);
asm
        JMP     System.@InitializeArray
end;

{*
  Finalise une variable - alias de @FinalizeArray
  @param Value      Variable à finaliser
  @param TypeInfo   RTTI du type de la variable
  @param Count      Nombre d'éléments dans la variable
*}
procedure Finalize(var Value; TypeInfo : PTypeInfo; Count : Cardinal = 1);
asm
        JMP     System.@FinalizeArray
end;

{*
  Copie un tableau statique
  @param Dest       Pointeur sur le tableau destination
  @param Source     Pointeur sur le tableau source
  @param TypeInfo   RTTI du type des éléments du tableau
  @param Count      Nombre d'éléments dans le tableau
*}
procedure CopyArray(Dest, Source, TypeInfo : Pointer; Count : integer);
asm
        JMP     System.@CopyArray
end;

{*
  Copie un record
  @param Dest       Pointeur sur le record destination
  @param Source     Pointeur sur le record source
  @param TypeInfo   RTTI du type record
*}
procedure CopyRecord(Dest, Source, TypeInfo : Pointer);
asm
        JMP     System.@CopyRecord
end;

{*
  Copie un tableau dynamique
  @param Source     Tableau source sous forme de pointeur
  @param TypeInfo   RTTI du type tableau dynamique
  @param Dest       Tableau destination sous forme de pointeur
*}
procedure DynArrayCopy(Source : Pointer; TypeInfo : Pointer;
  var Dest : Pointer);
asm
        JMP     System.@DynArrayCopy
end;

{*
  Copie une partie d'un tableau dynamique dans un autre
  @param Source     Tableau source sous forme de pointeur
  @param TypeInfo   RTTI du type tableau dynamique
  @param Index      Index du premier élément à copier
  @param Count      Nombre d'éléments à copier
  @param Dest       Tableau destination sous forme de pointeur
*}
procedure DynArrayCopyRange(Source : Pointer; TypeInfo : Pointer;
  Index, Count : integer; var Dest : Pointer);
asm
        JMP     System.@DynArrayCopyRange
end;

{*
  Détermine l'adresse réelle d'une routine de "compiler magic"
  Cette routine n'est valide qu'avec les alias de l'unité ScCompilerMagic, ou à
  défaut avec d'autres alias se contentant d'un JMP sur la véritable routine.
  @param CompilerMagicRoutineAlias   Pointeur sur le code d'un alias de routine
  @return Pointeur sur le code de la routine réelle
*}
function CompilerMagicRoutineAddress(
  CompilerMagicRoutineAlias : Pointer) : Pointer;
begin
  Assert(PByte(CompilerMagicRoutineAlias)^ = $E9); // JMP op code
  inc(Integer(CompilerMagicRoutineAlias));
  Result := Pointer(Integer(CompilerMagicRoutineAlias) +
    PInteger(CompilerMagicRoutineAlias)^ + 4);
end;

end.

