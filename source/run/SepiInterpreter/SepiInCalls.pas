{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------}

{*
  Gestion des appels entrant dans Sepi
  @author sjrd
  @version 1.0
*}
unit SepiInCalls;

interface

uses
  TypInfo, ScDelphiLanguage, SepiReflectionCore, SepiMembers, SepiOrdTypes,
  SepiRuntime;

function MakeInCallCode(RuntimeMethod: TSepiRuntimeMethod;
  ResultType: TSepiType; CallingConvention: TCallingConvention;
  SepiStackUsage: Word; RegUsage: Word = 0): Pointer;

implementation

{---------------}
{ Result thunks }
{---------------}

{*
  Thunk pour les routines sans résultat
*}
procedure NoneThunk(Method: TObject; Parameters: Pointer);
asm
        XOR     ECX,ECX
        CALL    TSepiRuntimeMethod.Invoke
end;

{*
  Thunk pour les routines renvoyant un ordinal ou assimilé (via EAX)
*}
function OrdinalThunk(Method: TObject; Parameters: Pointer): Integer;
asm
        PUSH    EAX
        MOV     ECX,ESP
        CALL    TSepiRuntimeMethod.Invoke
        POP     EAX
end;

{*
  Thunk pour les routines renvoyant un Int64 (via EDX:EAX)
*}
function Int64Thunk(Method: TObject; Parameters: Pointer): Int64;
asm
        SUB     ESP,8
        MOV     ECX,ESP
        CALL    TSepiRuntimeMethod.Invoke
        POP     EAX
        POP     EDX
end;

{*
  Thunk pour les routines renvoyant un Single (via ST(0))
*}
function SingleThunk(Method: TObject; Parameters: Pointer): Single;
asm
        PUSH    EAX
        MOV     ECX,ESP
        CALL    TSepiRuntimeMethod.Invoke
        FLD     DWORD PTR [ESP]
        POP     EAX
end;

{*
  Thunk pour les routines renvoyant un Double (via ST(0))
*}
function DoubleThunk(Method: TObject; Parameters: Pointer): Double;
asm
        SUB     ESP,8
        MOV     ECX,ESP
        CALL    TSepiRuntimeMethod.Invoke
        FLD     QWORD PTR [ESP]
        ADD     ESP,8
end;

{*
  Thunk pour les routines renvoyant un Extended (via ST(0))
*}
function ExtendedThunk(Method: TObject; Parameters: Pointer): Extended;
asm
        SUB     ESP,12
        MOV     ECX,ESP
        CALL    TSepiRuntimeMethod.Invoke
        FLD     TBYTE PTR [ESP]
        ADD     ESP,12
end;

{*
  Thunk pour les routines renvoyant un Currency ou un Comp (via ST(0))
*}
function CurrencyThunk(Method: TObject; Parameters: Pointer): Currency;
asm
        SUB     ESP,8
        MOV     ECX,ESP
        CALL    TSepiRuntimeMethod.Invoke
        FILD    QWORD PTR [ESP]
        ADD     ESP,8
end;

{*
  Thunk pour les constructeurs
*}
function ConstructorThunk(Method: TObject; Parameters: Pointer): TObject;
asm
        { -> EAX  Method
             EDX  Pointer to parameters
                  [EDX] is Self pointer
                  [EDX+4] is the Alloc hidden parameter
          <- EAX  Constructed instance }

        PUSH    EBX
        MOV     EBX,EDX
        MOV     ECX,EAX

        // Allocate instance

        MOV     EDX,[EBX+4]
        TEST    DL,DL
        JZ      @@noClassCreate
        MOV     EAX,[EBX]
        SUB     ESP,$10
        CALL    System.@ClassCreate // keeps ECX
        MOV     [EBX],EAX           // update Self parameter
@@noClassCreate:

        // Invoke constructor code

        MOV     EAX,ECX
        MOV     EDX,EBX
        XOR     ECX,ECX
        CALL    TSepiRuntimeMethod.Invoke

        // Execute the AfterConstruction method

        MOV     EDX,[EBX+4]
        TEST    DL,DL
        JZ      @@noAfterConstruction
        MOV     EAX,[EBX]
        CALL    System.@AfterConstruction
        POP     DWORD PTR FS:[0]
        ADD     ESP,$0C
@@noAfterConstruction:

        MOV     EAX,[EBX]
        POP     EBX
end;

{*
  Thunk pour les destructeurs
*}
procedure DestructorThunk(Method: TObject; Parameters: Pointer);
asm
        { -> EAX  Method
             EDX  Pointer to parameters
                  [EDX] is Self pointer
                  [EDX+4] is the Free hidden parameter }

        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EDX
        MOV     ESI,EAX

        // Execute the BeforeDestruction method

        MOV     EAX,[EBX]
        MOV     EDX,[EBX+4]
        CALL    System.@BeforeDestruction

        // Invoke the destructor code

        MOV     EAX,ESI
        MOV     EDX,EBX
        XOR     ECX,ECX
        CALL    TSepiRuntimeMethod.Invoke

        // Free instance

        MOV     EDX,[EBX+4]
        TEST    DL,DL
        JZ      @@noClassDestroy
        MOV     EAX,[EBX]
        CALL    System.@ClassDestroy
@@noClassDestroy:

        POP     ESI
        POP     EBX
end;

{-----------------}
{ Function makers }
{-----------------}

{*
  Construit le code natif d'un appel entrant avec registres dans Sepi
  @param RuntimeMethod    Méthode d'exécution (TSepiRuntimeMethod)
  @param ResultThunk      Thunk de gestion du résultat
  @param SepiStackUsage   Taille que Sepi utilise pour la pile
  @param RegUsage         Nombre de registres utilisés en natif
  @return Pointeur sur le code natif construit
*}
function MakeCodeWithRegs(RuntimeMethod, ResultThunk: Pointer;
  SepiStackUsage, RegUsage: Word): Pointer;

type
  PRegisterRedirector = ^TRegisterRedirector;
  TRegisterRedirector = packed record
    Reserved: array[0..8] of Byte;
    ObjAddress: Pointer;
    CallThunk: TJmpInstruction;
    Ret: Byte;
    StackUsage: Word;
  end;

const
  Skips: array[1..3] of Integer = (3, 2, 0);
  FirstByte: array[1..3] of Byte = ($59, $59, $87);

  Code: array[0..SizeOf(TRegisterRedirector)-1] of Byte = (
    // only for RegUsage = 3
    $87, $0C, $24,           // XCHG    ECX,[ESP]
    // RegUsage > 1
    $52,                     // PUSH    EDX
    // Common code
    $50,                     // PUSH    EAX
    $8B, $D4,                // MOV     EDX,ESP
    $51,                     // PUSH    ECX
    $B8, $FF, $FF, $FF, $FF, // MOV     EAX,ObjAddress
    $E8, $EE, $EE, $EE, $EE, // CALL    ResultThunk
    $C2, $DD, $DD            // RET     StackUsage
  );

var
  Skip, Size: Integer;
begin
  Skip := Skips[RegUsage];
  Size := SizeOf(Code) - Skip;
  GetMem(Result, Size);
  Move(Code[Skip], Result^, Size);

  PByte(Result)^ := FirstByte[RegUsage];
  with PRegisterRedirector(Integer(Result) - Skip)^ do
  begin
    ObjAddress := RuntimeMethod;
    MakeCall(CallThunk, ResultThunk);
    StackUsage := SepiStackUsage;
  end;
end;

{*
  Construit le code natif d'un appel entrant sans registre dans Sepi
  @param RuntimeMethod    Méthode d'exécution (TSepiRuntimeMethod)
  @param ResultThunk      Thunk de gestion du résultat
  @param SepiStackUsage   Taille que Sepi utilise pour la pile
  @return Pointeur sur le code natif construit
*}
function MakeCodeWithoutRegs(RuntimeMethod, ResultThunk: Pointer;
  SepiStackUsage: Word): Pointer;

type
  POtherRedirector = ^TOtherRedirector;
  TOtherRedirector = packed record
    LoadStackAddress: LongWord;
    MovEAX: Byte;
    ObjAddress: Pointer;
    CallThunk: TJmpInstruction;
    Ret: Byte;
    StackUsage: Word;
  end;

const
  Code: array[0..SizeOf(TOtherRedirector)-1] of Byte = (
    $8D, $54, $24, $04,      // LEA     EDX,[ESP+4]
    $B8, $FF, $FF, $FF, $FF, // MOV     EAX,ObjAddress
    $E8, $EE, $EE, $EE, $EE, // CALL    ResultThunk
    $C2, $DD, $DD            // RET     StackUsage
  );
  SimpleRet = $C3;

var
  Size: Integer;
begin
  Size := SizeOf(Code);
  if SepiStackUsage = 0 then
    Dec(Size, 2);

  GetMem(Result, Size);
  Move(Code[0], Result^, Size);

  with POtherRedirector(Result)^ do
  begin
    ObjAddress := RuntimeMethod;
    MakeCall(CallThunk, ResultThunk);

    if SepiStackUsage = 0 then
      Ret := SimpleRet
    else
      StackUsage := SepiStackUsage;
  end;
end;

{-------------------------}
{ MakeInCallCode function }
{-------------------------}

{*
  Construit le code natif d'un appel entrant dans Sepi
  @param RuntimeMethod       Méthode d'exécution (TSepiRuntimeMethod)
  @param ResultType          Type du résultat
  @param CallingConvention   Convention d'appel native
  @param SepiStackUsage      Taille que Sepi utilise pour la pile
  @param RegUsage            Nombre de registres utilisés en natif
  @return Pointeur sur le code natif construit
*}
function MakeInCallCode(RuntimeMethod: TSepiRuntimeMethod;
  ResultType: TSepiType; CallingConvention: TCallingConvention;
  SepiStackUsage: Word; RegUsage: Word = 0): Pointer;
const
  ResultThunks: array[TSepiTypeResultBehavior] of Pointer = (
    @NoneThunk, @OrdinalThunk, @Int64Thunk, @SingleThunk, @DoubleThunk,
    @ExtendedThunk, @CurrencyThunk, @NoneThunk
  );
var
  ResultThunk: Pointer;
begin
  // Find the appropriate result thunk
  case RuntimeMethod.SepiMethod.Signature.Kind of
    skConstructor: ResultThunk := @ConstructorThunk;
    skDestructor: ResultThunk := @DestructorThunk;
  else
    ResultThunk := ResultThunks[ResultType.SafeResultBehavior];
  end;

  // Use the appropriate function maker
  case CallingConvention of
    ccRegister:
    begin
      if RegUsage > 0 then
      begin
        Result := MakeCodeWithRegs(RuntimeMethod, ResultThunk,
          SepiStackUsage, RegUsage);
      end else
      begin
        Result := MakeCodeWithoutRegs(RuntimeMethod, ResultThunk,
          SepiStackUsage);
      end;
    end;
    ccCDecl: Result := MakeCodeWithoutRegs(RuntimeMethod, ResultThunk, 0);
  else
    Result := MakeCodeWithoutRegs(RuntimeMethod, ResultThunk, SepiStackUsage);
  end;
end;

end.

