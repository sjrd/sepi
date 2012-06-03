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
  Définit des routines de traitement de bas niveau
  @author sjrd
  @version 1.0
*}
unit ScLowLevel;
{$i ..\..\source\Sepi.inc}
interface

type
  {*
    Instruction JMP ou CALL
    @author sjrd
    @version 1.0
  *}
  TJmpInstruction = packed record
    OpCode: Byte;      /// OpCode
    Argument: Integer; /// Destination
  end;

procedure SetBit(var Value; Bit: LongWord);
procedure ClearBit(var Value; Bit: LongWord);
procedure ToggleBit(var Value; Bit: LongWord);
function TestBit(const Value; Bit: LongWord): Boolean;

function GetClassVirtualCode(AClass: TClass; VMTOffset: Integer): Pointer;
function GetClassVirtualMethod(AClass: TClass; VMTOffset: Integer): TMethod;
function GetObjectVirtualCode(AObject: TObject;
  VMTOffset: Integer): Pointer;
function GetObjectVirtualMethod(AObject: TObject;
  VMTOffset: Integer): TMethod;

function GetClassDynamicCode(AClass: TClass; DMTIndex: Integer): Pointer;
function GetClassDynamicMethod(AClass: TClass; DMTIndex: Integer): TMethod;
function GetObjectDynamicCode(AObject: TObject; DMTIndex: Integer): Pointer;
function GetObjectDynamicMethod(AObject: TObject;
  DMTIndex: Integer): TMethod;

function JmpArgument(JmpAddress, JmpDest: Pointer): Integer;
procedure MakeJmp(var Instruction; Dest: Pointer);
procedure MakeCall(var Instruction; Dest: Pointer);

function MakeProcOfRegisterMethod(const Method: TMethod;
  UsedRegCount: Byte; MoveStackCount: Word = 0): Pointer;
function MakeProcOfStdCallMethod(const Method: TMethod): Pointer;
function MakeProcOfPascalMethod(const Method: TMethod): Pointer;
function MakeProcOfCDeclMethod(const Method: TMethod): Pointer;
procedure ClearCDeclCallInfo;
procedure FreeProcOfMethod(Proc: Pointer);

implementation

type
  /// Pointeur vers TCDeclCallInfo
  PCDeclCallInfo = ^TCDeclCallInfo;

  {*
    Informations de contexte sur l'appel d'une méthode cdecl
    @author sjrd
    @version 1.0
  *}
  TCDeclCallInfo = packed record
    Previous: PCDeclCallInfo; /// Pointeur vers le contexte précédent
    StackPointer: Pointer;    /// Valeur de ESP au moment de l'appel
    ReturnAddress: Pointer;   /// Adresse de retour de l'appel
  end;

threadvar
  /// Liste des infos sur les appels cdecl (spécifique à chaque thread)
  CDeclCallInfoList: PCDeclCallInfo;

{*
  Trouve la dernière info de routine cdecl valide
  @param StackPointer   Valeur du registre esp au moment de l'appel
  @param AllowSame      True pour permettre un StackPointer égal, False sinon
  @return Pointeur sur la dernière info de routine cdecl valide
*}
function GetLastValidCDeclCallInfo(StackPointer: Pointer;
  AllowSame: Boolean): PCDeclCallInfo;
var
  Previous: PCDeclCallInfo;
begin
  Result := CDeclCallInfoList;
  while (Result <> nil) and
    (Cardinal(Result.StackPointer) <= Cardinal(StackPointer)) do
  begin
    if AllowSame and (Result.StackPointer = StackPointer) then
      Break;
    Previous := Result.Previous;
    Dispose(Result);
    Result := Previous;
  end;
end;

{*
  S'assure que toutes les informations de routines cdecl sont supprimées
  Cette routine devrait être appelée à la fin de chaque thread susceptible
  d'utiliser des routines issues de méthodes cdecl.
  Si ce n'est pas possible, ce n'est pas dramatique, mais l'application
  s'expose à de légères fuites mémoire en cas d'exception à l'intérieur de
  telles méthodes.
  Pour le thread principal, ce n'est pas nécessaire, le code de finalisation de
  ScLowLevel.pas s'en charge.
*}
procedure ClearCDeclCallInfo;
begin
  GetLastValidCDeclCallInfo(Pointer($FFFFFFFF), False);
  CDeclCallInfoList := nil;
end;

{*
  Ajoute une adresse de retour
  @param StackPointer    Valeur du registre ESP
  @param ReturnAddress   Adresse à ajouter
*}
procedure StoreCDeclReturnAddress(
  StackPointer, ReturnAddress: Pointer); stdcall;
var
  LastInfo, CurInfo: PCDeclCallInfo;
begin
  LastInfo := GetLastValidCDeclCallInfo(StackPointer, False);

  New(CurInfo);
  CurInfo.Previous := LastInfo;
  CurInfo.StackPointer := StackPointer;
  CurInfo.ReturnAddress := ReturnAddress;
  CDeclCallInfoList := CurInfo;
end;

{*
  Récupère une adresse de retour
  @param StackPointer   Valeur du registre ESP
  @return L'adresse de retour qui a été ajoutée en dernier
*}
function GetCDeclReturnAddress(StackPointer: Pointer): Pointer; register;
var
  LastInfo: PCDeclCallInfo;
begin
  LastInfo := GetLastValidCDeclCallInfo(StackPointer, True);

  if (LastInfo = nil) or (LastInfo.StackPointer <> StackPointer) then
  begin
    CDeclCallInfoList := LastInfo;
    Assert(False);
  end;

  CDeclCallInfoList := LastInfo.Previous;
  Result := LastInfo.ReturnAddress;
  Dispose(LastInfo);
end;

{*
  Positionne un bit à 1
  @param Value   Valeur à modifier
  @param Bit     Index du bit à modifier
  @author waskol - extended by sjrd
*}
procedure SetBit(var Value; Bit: LongWord);
asm
        { -> EAX        Value address
             EDX        Bit index     }
        BTS     [EAX],EDX
end;

{*
  Positionne un bit à 0
  @param Value   Valeur à modifier
  @param Bit     Index du bit à modifier
  @author waskol - extended by sjrd
*}
procedure ClearBit(var Value; Bit: LongWord);
asm
        { -> EAX        Value address
             EDX        Bit index     }
        BTR     [EAX],EDX
end;

{*
  Inverse un bit
  @param Value   Valeur à modifier
  @param Bit     Index du bit à modifier
  @author waskol - extended by sjrd
*}
procedure ToggleBit(var Value; Bit: LongWord);
asm
        { -> EAX        Value address
             EDX        Bit index     }
        BTC     [EAX],EDX
end;

{*
  Teste la valeur d'un bit
  @param Value   Valeur à tester
  @param Bit     Index du bit à tester
  @return True si le bit est à 1, False sinon
  @author waskol - extended by sjrd
*}
function TestBit(const Value; Bit: LongWord): Boolean;
asm
        { -> EAX        Value address
             EDX        Bit index
          <- AL         Result        }
        BT      [EAX],EDX
        SETB    AL
end;

{*
  Trouve le code d'une méthode virtuelle, pour une classe
  @param AClass      Classe concernée
  @param VMTOffset   VMT offset de la méthode
  @return Pointeur sur le code de la méthode
*}
function GetClassVirtualCode(AClass: TClass; VMTOffset: Integer): Pointer;
asm
        { ->    EAX     Pointer to class  }
        {       EDX     DMTIndex          }
        { <-    EAX     Pointer to method }
        MOV     EAX,[EAX+EDX]
end;

{*
  Trouve une méthode virtuelle, pour une classe
  @param AClass      Classe concernée
  @param VMTOffset   VMT offset de la méthode
  @return Méthode correspondante
*}
function GetClassVirtualMethod(AClass: TClass; VMTOffset: Integer): TMethod;
begin
  Result.Data := AClass;
  Result.Code := GetClassVirtualCode(AClass, VMTOffset);
end;

{*
  Trouve le code d'une méthode virtuelle, pour un objet
  @param AObject     Objet concerné
  @param VMTOffset   VMT offset de la méthode
  @return Pointeur sur le code de la méthode
*}
function GetObjectVirtualCode(AObject: TObject;
  VMTOffset: Integer): Pointer;
asm
        { ->    EAX     Pointer to object }
        {       EDX     DMTIndex          }
        { <-    EAX     Pointer to method }
        MOV     EAX,[EAX]
        MOV     EAX,[EAX+EDX]
end;

{*
  Trouve une virtuelle dynamique, pour un objet
  @param AObject     Objet concerné
  @param VMTOffset   VMT offset de la méthode
  @return Méthode correspondante
*}
function GetObjectVirtualMethod(AObject: TObject;
  VMTOffset: Integer): TMethod;
begin
  Result.Data := AObject;
  Result.Code := GetObjectVirtualCode(AObject, VMTOffset);
end;

{*
  Trouve le code d'une méthode dynamique, pour une classe
  @param AClass     Classe concernée
  @param DMTIndex   DMT index de la méthode
  @return Pointeur sur le code de la méthode
  @throws EAbstractError La classe n'implémente pas la méthode recherchée
*}
function GetClassDynamicCode(AClass: TClass; DMTIndex: Integer): Pointer;
asm
        { ->    EAX     Pointer to class  }
        {       EDX     DMTIndex          }
        { <-    EAX     Pointer to method }
// TODO FPC        CALL    System.@FindDynaClass
end;

{*
  Trouve une méthode dynamique, pour une classe
  @param AClass     Classe concernée
  @param DMTIndex   DMT index de la méthode
  @return Méthode correspondante
  @throws EAbstractError La classe n'implémente pas la méthode recherchée
*}
function GetClassDynamicMethod(AClass: TClass; DMTIndex: Integer): TMethod;
begin
  Result.Data := AClass;
  Result.Code := GetClassDynamicCode(AClass, DMTIndex);
end;

{*
  Trouve le code d'une méthode dynamique, pour un objet
  @param AObject    Objet concerné
  @param DMTIndex   DMT index de la méthode
  @return Pointeur sur le code de la méthode
  @throws EAbstractError La classe n'implémente pas la méthode recherchée
*}
function GetObjectDynamicCode(AObject: TObject; DMTIndex: Integer): Pointer;
asm
        { ->    EAX     Pointer to object }
        {       EDX     DMTIndex          }
        { <-    EAX     Pointer to method }
        MOV     EAX,[EAX]
 // TODO FPC       CALL    System.@FindDynaClass
end;

{*
  Trouve une méthode dynamique, pour un objet
  @param AObject    Objet concerné
  @param DMTIndex   DMT index de la méthode
  @return Méthode correspondante
  @throws EAbstractError La classe n'implémente pas la méthode recherchée
*}
function GetObjectDynamicMethod(AObject: TObject;
  DMTIndex: Integer): TMethod;
begin
  Result.Data := AObject;
  Result.Code := GetObjectDynamicCode(AObject, DMTIndex);
end;

{*
  Calcule l'argument d'une instruction JMP ou CALL
  @param JmpAddress   Adresse de l'instruction JMP
  @param JmpDest      Destination du JMP
  @return Argument à donner au JMP
*}
function JmpArgument(JmpAddress, JmpDest: Pointer): Integer;
asm
        { -> EAX Address of the jump instruction }
        { -> EDX Pointer to destination          }
        { -> Return value = EDX - EAX - 5        }

        NEG     EAX
        ADD     EAX,EDX
        SUB     EAX,5
end;

{*
  Construit une instruction JMP
  @param Instruction   Instruction (minimum 5 octets)
  @param Dest          Destination du JMP
*}
procedure MakeJmp(var Instruction; Dest: Pointer);
asm
        { -> EAX Pointer to a TJmpInstruction record }
        { -> EDX Pointer to destination              }

        MOV     BYTE PTR [EAX],$E9
        SUB     EDX,EAX
        SUB     EDX,5
        MOV     [EAX+1],EDX
end;

{*
  Construit une instruction CALL
  @param Instruction   Instruction (minimum 5 octets)
  @param Dest          Destination du CALL
*}
procedure MakeCall(var Instruction; Dest: Pointer);
asm
        { -> EAX Pointer to a TJmpInstruction record }
        { -> EDX Pointer to destination              }

        MOV     BYTE PTR [EAX],$E8
        SUB     EDX,EAX
        SUB     EDX,5
        MOV     [EAX+1],EDX
end;

{*
  Construit une routine équivalente à une méthode register, version courte
  @param Method           Méthode à convertir
  @param UsedRegCount     Nombre de registres utilisés dans l'appel de procédure
  @param MoveStackCount   Nombre de cases de pile empilées après ECX
  @return Pointeur vers le code de la procédure créée
*}
function MakeShortProcOfRegisterMethod(const Method: TMethod;
  UsedRegCount: Byte; MoveStackCount: Word): Pointer;

const
  MoveStackItem: LongWord = $00244C87; // 874C24 xx   XCHG ECX,[ESP+xx]
  MoveRegisters: array[0..7] of Byte = (
    $87, $0C, $24, // XCHG    ECX,[ESP]
    $51,           // PUSH    ECX
    $8B, $CA,      // MOV     ECX,EDX
    $8B, $D0       // MOV     EDX,EAX
  );

type
  PRegisterRedirector = ^TRegisterRedirector;
  TRegisterRedirector = packed record
    MovEAXObj: Byte;
    ObjAddress: Pointer;
    Jump: TJmpInstruction;
  end;

var
  MoveStackSize: Integer;
  MoveRegSize: Integer;
  InstrPtr: Pointer;
  I: Cardinal;
begin
  if UsedRegCount >= 3 then
    UsedRegCount := 4;
  MoveRegSize := 2*UsedRegCount;
  MoveStackSize := 4*MoveStackCount;

  GetMem(Result, MoveStackSize + MoveRegSize + SizeOf(TRegisterRedirector));
  InstrPtr := Result;

  for I := MoveStackCount downto 1 do
  begin
    // I shl 26 => I*4 in the most significant byte (kind of $ I*4 00 00 00)
    PLongWord(InstrPtr)^ := (I shl 26) or MoveStackItem;
    Inc(Integer(InstrPtr), 4);
  end;

  Move(MoveRegisters[SizeOf(MoveRegisters) - MoveRegSize],
    InstrPtr^, MoveRegSize);
  Inc(Integer(InstrPtr), MoveRegSize);

  with PRegisterRedirector(InstrPtr)^ do
  begin
    MovEAXObj := $B8;
    ObjAddress := Method.Data;
    MakeJmp(Jump, Method.Code);
  end;
end;

{*
  Construit une routine équivalente à une méthode register, version longue
  @param Method           Méthode à convertir
  @param MoveStackCount   Nombre de cases de pile empilées après ECX
  @return Pointeur vers le code de la procédure créée
*}
function MakeLongProcOfRegisterMethod(const Method: TMethod;
  MoveStackCount: Word): Pointer;

type
  PRegisterRedirector = ^TRegisterRedirector;
  TRegisterRedirector = packed record
    Reserved1: array[0..8] of Byte;
    MoveStackCount4: LongWord;
    Reserved2: array[13..20] of Byte;
    FourMoveStackCount1: LongWord;
    Reserved3: array[25..29] of Byte;
    ObjAddress: Pointer;
    Jump: TJmpInstruction;
  end;

const
  Code: array[0..SizeOf(TRegisterRedirector)-1] of Byte = (
    $56,                     // PUSH    ESI
    $57,                     // PUSH    EDI
    $51,                     // PUSH    ECX

    $8B, $F4,                // MOV     ESI,ESP
    $51,                     // PUSH    ECX
    $8B, $FC,                // MOV     EDI,ESP
    $B9, $FF, $FF, $FF, $FF, // MOV     ECX,MoveStackCount+4

    $F3, $A5,                // REP     MOVSD

    $59,                     // POP     ECX
    $5F,                     // POP     EDI
    $5E,                     // POP     ESI

    $89, $8C, $24, $EE, $EE, $EE, $EE,
    // MOV     [ESP + 4*(MoveStackCount+1)],ECX
    $8B, $CA,                // MOV     ECX,EDX
    $8B, $D0,                // MOV     EDX,EAX
    $B8, $DD, $DD, $DD, $DD, // MOV     EAX,0
    $E9, $CC, $CC, $CC, $CC  // JMP     MethodAddress
  );

begin
  GetMem(Result, SizeOf(Code));
  Move(Code[0], Result^, SizeOf(Code));

  with PRegisterRedirector(Result)^ do
  begin
    MoveStackCount4 := MoveStackCount+4;
    FourMoveStackCount1 := 4 * (MoveStackCount+1);
    ObjAddress := Method.Data;
    MakeJmp(Jump, Method.Code);
  end;
end;

{*
  Construit une routine équivalente à une méthode register
  MakeProcOfMethod permet d'obtenir un pointeur sur une routine, construite
  dynamiquement, qui équivaut à une méthode. Ce qui signifie que la routine
  renvoyée commence par ajouter un paramètre supplémentaire, avant d'appeler
  la méthode initiale.
  La procédure devra être libérée avec FreeProcOfMethod une fois utilisée.
  Vous devez déterminer UsedRegCount et MoveStackCount d'après la délcaration de
  la *procédure*. UsedRegCount est le nombre de registres utilisés pour la
  transmission des paramètres (dans l'ordre EAX, EDX et ECX). Si les trois sont
  utilisés, le paramètre MoveStackCount doit renseigner le nombre de "cases" de
  pile (de doubles mots) utilisés par les paramètres déclarés *après* le
  paramètre transmis dans ECX.
  @param Method           Méthode à convertir
  @param UsedRegCount     Nombre de registres utilisés dans l'appel de procédure
  @param MoveStackCount   Nombre de cases de pile empilées après ECX
  @return Pointeur vers le code de la procédure créée
*}
function MakeProcOfRegisterMethod(const Method: TMethod;
  UsedRegCount: Byte; MoveStackCount: Word = 0): Pointer;
begin
  Assert((MoveStackCount = 0) or (UsedRegCount >= 3));

  if MoveStackCount <= 8 then
    Result := MakeShortProcOfRegisterMethod(
      Method, UsedRegCount, MoveStackCount)
  else
    Result := MakeLongProcOfRegisterMethod(Method, MoveStackCount);
end;

{*
  Construit une routine équivalente à une méthode stdcall
  MakeProcOfMethod permet d'obtenir un pointeur sur une routine, construite
  dynamiquement, qui équivaut à une méthode. Ce qui signifie que la routine
  renvoyée commence par ajouter un paramètre supplémentaire, avant d'appeler
  la méthode initiale.
  La procédure devra être libérée avec FreeProcOfMethod une fois utilisée.
  @param Method   Méthode à convertir
  @return Pointeur vers le code de la procédure créée
*}
function MakeProcOfStdCallMethod(const Method: TMethod): Pointer;
type
  PStdCallRedirector = ^TStdCallRedirector;
  TStdCallRedirector = packed record
    PopEAX: Byte;
    PushObj: Byte;
    ObjAddress: Pointer;
    PushEAX: Byte;
    Jump: TJmpInstruction;
  end;
begin
  GetMem(Result, SizeOf(TStdCallRedirector));
  with PStdCallRedirector(Result)^ do
  begin
    PopEAX := $58;
    PushObj := $68;
    ObjAddress := Method.Data;
    PushEAX := $50;
    MakeJmp(Jump, Method.Code);
  end;
end;

{*
  Construit une routine équivalente à une méthode pascal
  MakeProcOfMethod permet d'obtenir un pointeur sur une routine, construite
  dynamiquement, qui équivaut à une méthode. Ce qui signifie que la routine
  renvoyée commence par ajouter un paramètre supplémentaire, avant d'appeler
  la méthode initiale.
  La procédure devra être libérée avec FreeProcOfMethod une fois utilisée.
  @param Method   Méthode à convertir
  @return Pointeur vers le code de la procédure créée
*}
function MakeProcOfPascalMethod(const Method: TMethod): Pointer;
begin
  Result := MakeProcOfStdCallMethod(Method);
end;

{*
  Construit une routine équivalente à une méthode cdecl
  MakeProcOfMethod permet d'obtenir un pointeur sur une routine, construite
  dynamiquement, qui équivaut à une méthode. Ce qui signifie que la routine
  renvoyée commence par ajouter un paramètre supplémentaire, avant d'appeler
  la méthode initiale.
  La procédure devra être libérée avec FreeProcOfMethod une fois utilisée.
  @param Method   Méthode à convertir
  @return Pointeur vers le code de la procédure créée
*}
function MakeProcOfCDeclMethod(const Method: TMethod): Pointer;

type
  PCDeclRedirector = ^TCDeclRedirector;
  TCDeclRedirector = packed record
    PushESP: Byte;
    CallStoreAddress: TJmpInstruction;
    PushObj: Byte;
    ObjAddress: Pointer;
    CallMethod: TJmpInstruction;
    MovESPEAX: array[0..2] of Byte;
    MovEAXESP: array[0..1] of Byte;
    PushEDX: Byte;
    CallGetAddress: TJmpInstruction;
    PopEDX: Byte;
    Xchg: array[0..2] of Byte;
    Ret: Byte;
  end;

const
  Code: array[0..SizeOf(TCDeclRedirector)-1] of Byte = (
    $54,                     // PUSH    ESP
    $E8, $FF, $FF, $FF, $FF, // CALL    StoreCDeclReturnAddress
    $68, $EE, $EE, $EE, $EE, // PUSH    ObjAddress
    $E8, $DD, $DD, $DD, $DD, // CALL    MethodCode
    $89, $04, $24,           // MOV     [ESP],EAX
    $8B, $C4,                // MOV     EAX,ESP
    $52,                     // PUSH    EDX
    $E8, $CC, $CC, $CC, $CC, // CALL    GetCDeclReturnAddress
    $5A,                     // POP     EDX
    $87, $04, $24,           // XCHG    EAX,[ESP]
    $C3                      // RET
  );

begin
  GetMem(Result, SizeOf(Code));
  Move(Code[0], Result^, SizeOf(Code));

  with PCDeclRedirector(Result)^ do
  begin
    MakeCall(CallStoreAddress, @StoreCDeclReturnAddress);
    ObjAddress := Method.Data;
    MakeCall(CallMethod, Method.Code);
    MakeCall(CallGetAddress, @GetCDeclReturnAddress);
  end;
end;

{*
  Libère une routine construite avec une des MakeProcOfMethod
  @param Proc   Routine à libérer
*}
procedure FreeProcOfMethod(Proc: Pointer);
begin
  FreeMem(Proc);
end;

initialization
finalization
  ClearCDeclCallInfo;
end.

