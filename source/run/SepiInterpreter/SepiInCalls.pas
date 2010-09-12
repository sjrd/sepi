{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  Sébastien Doeraene
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

Linking this library statically or dynamically with other modules is making a
combined work based on this library.  Thus, the terms and conditions of the GNU
General Public License cover the whole combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.
-------------------------------------------------------------------------------}

{*
  Gestion des appels entrant dans Sepi
  @author sjrd
  @version 1.0
*}
unit SepiInCalls;

interface

uses
  TypInfo, ScLowLevel, SepiReflectionCore, SepiMembers, SepiOrdTypes,
  SepiRuntime;

function MakeInCallCode(RuntimeMethod: TSepiRuntimeMethod;
  ResultType: TSepiType; CallingConvention: TCallingConvention;
  SepiStackUsage: Word; RegUsage: Word = 0): Pointer;

procedure MakeRoutineRefFromMethodRef(MethodRefType: TSepiMethodRefType;
  out RoutineRef: IInterface; const MethodRef: TMethod);

implementation

type
  {*
    Interface représentant une référence de routine
    @author sjrd
    @version 1.0
  *}
  IRegisterRoutineReference = interface(IInterface)
    procedure Invoke; register;
  end;

  {*
    Interface représentant une référence de routine
    @author sjrd
    @version 1.0
  *}
  INonRegisterRoutineReference = interface(IInterface)
    procedure Invoke; stdcall;
  end;

  {*
    Classe représentant une référence de routine qui pointe sur une méthode
    @author sjrd
    @version 1.0
  *}
  TAbstractMethodRefBackedRoutineReference = class(TInterfacedObject)
  private
    FMethod: TMethod; /// Référence à la méthode à appeler
  public
    constructor Create(const AMethod: TMethod);

    property Method: TMethod read FMethod;
  end;

  {*
    Classe représentant une référence de routine qui pointe sur une méthode
    @author sjrd
    @version 1.0
  *}
  TRegisterMethodRefBackedRoutineReference = class(
    TAbstractMethodRefBackedRoutineReference, IRegisterRoutineReference)
  protected
    procedure Invoke; register;
  end;

  {*
    Classe représentant une référence de routine qui pointe sur une méthode
    @author sjrd
    @version 1.0
  *}
  TNonRegisterMethodRefBackedRoutineReference = class(
    TAbstractMethodRefBackedRoutineReference, INonRegisterRoutineReference)
  protected
    procedure Invoke; stdcall;
  end;

  {*
    Classe représentant une référence de routine qui pointe sur une routine
    @author sjrd
    @version 1.0
  *}
  TAbstractRoutineBackedRoutineReference = class(TInterfacedObject)
  private
    FCode: Pointer; /// Adresse de la routine à appeler
  public
    constructor Create(ACode: Pointer);

    property Code: Pointer read FCode;
  end;

  {*
    Classe représentant une référence de routine qui pointe sur une routine
    @author sjrd
    @version 1.0
  *}
  TRegisterRoutineBackedRoutineReferenceNoStack = class(
    TAbstractRoutineBackedRoutineReference, IRegisterRoutineReference)
  protected
    procedure Invoke; register;
  end;

  {*
    Classe représentant une référence de routine qui pointe sur une routine
    @author sjrd
    @version 1.0
  *}
  TRegisterRoutineBackedRoutineReferenceWithStack = class(
    TAbstractRoutineBackedRoutineReference, IRegisterRoutineReference)
  private
    FMoveStackSize: Integer;
  protected
    procedure Invoke; register;
  public
    constructor Create(ACode: Pointer; Signature: TSepiSignature);
  end;

  {*
    Classe représentant une référence de routine qui pointe sur une routine
    @author sjrd
    @version 1.0
  *}
  TStdCallRoutineBackedRoutineReference = class(
    TAbstractRoutineBackedRoutineReference, INonRegisterRoutineReference)
  protected
    procedure Invoke; stdcall;
  end;

  {*
    Structure représentant le thunk de code de retour de la méthode
    @author sjrd
    @version 1.0
  *}
  TCDeclReturnThunkCode = packed record
    MovECXObj: Byte;
    ObjAddress: Pointer;
    Jump: TJmpInstruction;
  end;

  {*
    Classe représentant une référence de routine qui pointe sur une routine
    @author sjrd
    @version 1.0
  *}
  TCDeclRoutineBackedRoutineReference = class(
    TAbstractRoutineBackedRoutineReference, INonRegisterRoutineReference)
  private
    FMoveStackCount: Integer;
  protected
    procedure Invoke; stdcall;
  public
    constructor Create(ACode: Pointer; Signature: TSepiSignature);
  end;

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

{------------------------------------------------}
{ TAbstractMethodRefBackedRoutineReference class }
{------------------------------------------------}

{*
  Crée une instance de TAbstractMethodRefBackedRoutineReference
  @param AMethod   Méthode à appeler
*}
constructor TAbstractMethodRefBackedRoutineReference.Create(
  const AMethod: TMethod);
begin
  inherited Create;

  FMethod := AMethod;
end;

{------------------------------------------------}
{ TRegisterMethodRefBackedRoutineReference class }
{------------------------------------------------}

{*
  [@inheritDoc]
*}
procedure TRegisterMethodRefBackedRoutineReference.Invoke;
asm
        { -> EAX Self }
        PUSH    DWORD PTR [EAX+12]
        ADD     ESP,4
        MOV     EAX,[EAX+16]
        JMP     DWORD PTR [ESP-4]
end;

{---------------------------------------------------}
{ TNonRegisterMethodRefBackedRoutineReference class }
{---------------------------------------------------}

{*
  [@inheritDoc]
*}
procedure TNonRegisterMethodRefBackedRoutineReference.Invoke;
asm
        POP     EBP // cancel the effect of asm keyword with stdcall

        { -> [ESP+4] Self }
        MOV     EAX,[ESP+4]
        MOV     ECX,[EAX+16]
        MOV     [ESP+4],ECX
        JMP     DWORD PTR [EAX+12]
end;

{----------------------------------------------}
{ TAbstractRoutineBackedRoutineReference class }
{----------------------------------------------}

{*
  Crée une instance de TAbstractRoutineBackedRoutineReference
  @param ACode   Code la routine à appeler
*}
constructor TAbstractRoutineBackedRoutineReference.Create(ACode: Pointer);
begin
  inherited Create;

  FCode := ACode;
end;

{-----------------------------------------------------}
{ TRegisterRoutineBackedRoutineReferenceNoStack class }
{-----------------------------------------------------}

{*
  [@inheritDoc]
*}
procedure TRegisterRoutineBackedRoutineReferenceNoStack.Invoke;
asm
        { -> EAX Self }
        XCHG    EAX,EDX
        XCHG    EDX,ECX
        JMP     DWORD PTR [ECX+12]
end;

{-------------------------------------------------------}
{ TRegisterRoutineBackedRoutineReferenceWithStack class }
{-------------------------------------------------------}

{*
  Crée une instance de TRegisterRoutineBackedRoutineReferenceWithStack
  @param ACode       Adresse de la routine à appeler
  @param Signature   Signature du TSepiMethodRefType
*}
constructor TRegisterRoutineBackedRoutineReferenceWithStack.Create(
  ACode: Pointer; Signature: TSepiSignature);
var
  ECXParamIndex, StackParamIndex: Integer;
begin
  inherited Create(ACode);

  with Signature do
  begin
    // Find the index of the parameter that is transmitted via ECX
    ECXParamIndex := 0;
    while ActualParams[ECXParamIndex].CallInfo.Place <> ppECX do
      Inc(ECXParamIndex);

    // Find the last parameter before this one that is stored on the stack
    StackParamIndex := ECXParamIndex;
    while (StackParamIndex >= 0) and
      (ActualParams[StackParamIndex].CallInfo.Place <> ppStack) do
      Dec(StackParamIndex);

    // The offset of this parameter is the size of the stack to be moved
    if StackParamIndex >= 0 then
      FMoveStackSize := ActualParams[StackParamIndex].CallInfo.StackOffset
    else
      FMoveStackSize := StackUsage;
  end;

  Inc(FMoveStackSize, 5*4); // 4 registers and the return address
end;

{*
  [@inheritDoc]
*}
procedure TRegisterRoutineBackedRoutineReferenceWithStack.Invoke;
type
  TMe = TRegisterRoutineBackedRoutineReferenceWithStack;
asm
        { -> EAX Self }

        // Save registers

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        // Shift registers

        MOV     EBX,EAX
        MOV     EAX,EDX
        MOV     EDX,ECX

        // Extract the future value of ECX into EBP

        MOV     ECX,[EBX].TMe.FMoveStackSize
        MOV     EBP,[ESP+ECX]

        // Move stack

        LEA     ESI,[ESP+ECX-4]
        LEA     EDI,[ESP+ECX]
        SHR     ECX,2
        STD
        REP     MOVSD
        CLD
        POP     ECX // get rid of the empty bucket

        // Set the new value of ECX, and reload registers

        MOV     ECX,EBP
        POP     EBP
        POP     EDI
        POP     ESI

        // Finalize

        MOV     EBX,[EBX].TMe.FCode
        MOV     [ESP-4],EBX
        POP     EBX
        JMP     DWORD PTR [ESP-8]
end;

{---------------------------------------------}
{ TStdCallRoutineBackedRoutineReference class }
{---------------------------------------------}

{*
  [@inheritDoc]
*}
procedure TStdCallRoutineBackedRoutineReference.Invoke;
asm
        POP     EBP // cancel the effect of asm keyword with stdcall

        { -> [ESP+4] Self }
        POP     EAX
        XCHG    EAX,[ESP]
        JMP     DWORD PTR [EAX+12]
end;

{-------------------------------------------}
{ TCDeclRoutineBackedRoutineReference class }
{-------------------------------------------}

{*
  Crée une instance de TCDeclRoutineBackedRoutineReference
  @param ACode       Adresse de la routine à appeler
  @param Signature   Signature du TSepiMethodRefType
*}
constructor TCDeclRoutineBackedRoutineReference.Create(
  ACode: Pointer; Signature: TSepiSignature);
begin
  inherited Create(ACode);

  FMoveStackCount := Signature.StackUsage div 4;
end;

{*
  [@inheritDoc]
*}
procedure TCDeclRoutineBackedRoutineReference.Invoke;
type
  TMe = TCDeclRoutineBackedRoutineReference;
asm
        POP     EBP // cancel the effect of asm keyword with stdcall

        { -> [ESP+4] Self }

        MOV     EAX,[ESP+4]
        MOV     ECX,[EAX].TMe.FMoveStackCount
        JECXZ   @@noStackMove

        // Move the stack

        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[ESP+16]
        LEA     EDI,[ESP+12]
        REP     MOVSD
        POP     EDI
        POP     ESI
@@noStackMove:

        // Jump to routine

        JMP     [EAX].TMe.FCode
end;

{---------------------------------------}
{ MakeRoutineRefFromMethodRef procedure }
{---------------------------------------}

procedure MakeRoutineRefFromMethodRef(MethodRefType: TSepiMethodRefType;
  out RoutineRef: IInterface; const MethodRef: TMethod);
var
  Signature: TSepiSignature;
  RegisterRoutineRef: IRegisterRoutineReference;
  NonRegisterRoutineRef: INonRegisterRoutineReference;
begin
  Signature := MethodRefType.Signature;

  Assert(Signature.Kind in [skStaticProcedure..skObjectFunction],
    'Unsupported signature kind for RRFMR');

  if Signature.CallingConvention = ccRegister then
  begin
    if Signature.Kind in [skObjectProcedure..skObjectFunction] then
    begin
      RegisterRoutineRef :=
        TRegisterMethodRefBackedRoutineReference.Create(MethodRef);
    end else if Signature.RegUsage < 3 then
    begin
      RegisterRoutineRef :=
        TRegisterRoutineBackedRoutineReferenceNoStack.Create(MethodRef.Code);
    end else
    begin
      RegisterRoutineRef :=
        TRegisterRoutineBackedRoutineReferenceWithStack.Create(MethodRef.Code,
          Signature);
    end;

    RoutineRef := RegisterRoutineRef;
  end else
  begin
    if Signature.Kind in [skObjectProcedure..skObjectFunction] then
    begin
      NonRegisterRoutineRef :=
        TNonRegisterMethodRefBackedRoutineReference.Create(MethodRef);
    end else if Signature.CallingConvention = ccCDecl then
    begin
      NonRegisterRoutineRef :=
        TCDeclRoutineBackedRoutineReference.Create(MethodRef.Code, Signature);
    end else
    begin
      NonRegisterRoutineRef :=
        TStdCallRoutineBackedRoutineReference.Create(MethodRef.Code);
    end;

    RoutineRef := NonRegisterRoutineRef;
  end;
end;

end.

