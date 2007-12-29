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
  Gestion des appels sortant de Sepi
  @author sjrd
  @version 1.0
*}
unit SepiOutCalls;

interface

uses
  SepiReflectionCore, SepiMembers, SepiOpCodes;

procedure SepiCallOut(Address: Pointer; CallingConvention: TCallingConvention;
  Parameters: Pointer; ParamsSize: Integer; RegUsage: Word;
  ResultBehavior: TSepiTypeResultBehavior; Result: Pointer = nil);

implementation

{--------------------------------------}
{ Syntactic trick to get float results }
{--------------------------------------}

function GetSingleResult: Single;
asm
end;

function GetDoubleResult: Double;
asm
end;

function GetExtendedResult: Extended;
asm
end;

function GetCurrencyResult: Currency;
asm
end;

{--------------}
{ Main routine }
{--------------}

{*
  Appel sortant de Sepi
  Attention ! Le paramètre ResultBehavior ne doit valoir rbNone que si la
  routine appelée est bel et bien une procédure (sans résultat). Si c'est une
  fonction, vous devez donner la bonne valeur, même si vous ne voulez pas
  récupérer le résultat.
  @param Address             Pointeur sur le code de la procédure à appeler
  @param CallingConvention   Convention d'appel à utiliser
  @param Parameters          Pointeur sur les paramètres
  @param ParamsSize          Taille des paramètres
  @param RegUsage            Nombre de registres utilsés
  @param ResultBehavior      Comportement du résultat
  @param Result              Pointeur sur le résultat (défaut = nil)
*}
procedure SepiCallOut(Address: Pointer; CallingConvention: TCallingConvention;
  Parameters: Pointer; ParamsSize: Integer; RegUsage: Word;
  ResultBehavior: TSepiTypeResultBehavior; Result: Pointer = nil);

type
  TRegisters = packed record
    case Integer of
      0: (EAX, EDX, ECX: Longint);
      2: (OrdinalRes: Longint);
      3: (Int64Res: Int64);
  end;

var
  RegsSize: Integer;
  Registers: TRegisters;
  TopOfStack: Pointer;
begin
  // Extract registers
  RegsSize := 4*RegUsage;
  Move(Parameters^, Registers, RegsSize);
  Dec(ParamsSize, RegsSize);
  Inc(Integer(Parameters), RegsSize);

  // Place other parameters on stack
  asm
        SUB     ESP,ParamsSize
        MOV     TopOfStack,ESP
  end;
  Move(Parameters^, TopOfStack^, ParamsSize);

  // Actual call
  asm
        MOV     EAX,DWORD PTR Registers[0]
        MOV     EDX,DWORD PTR Registers[4]
        MOV     ECX,DWORD PTR Registers[8]
        CALL    Address
        MOV     DWORD PTR Registers[4],EDX
        MOV     DWORD PTR Registers[0],EAX
  end;

  // Release parameters - only with cdecl calling convention
  if CallingConvention = ccCDecl then
  asm
        ADD     ESP,ParamsSize
  end;

  // Handle result
  if Assigned(Result) then
  begin
    // Get result
    case ResultBehavior of
      rbNone, // if Result is specified, it must be a construtor, so ordinal
      rbOrdinal:  Longint (Result^) := Registers.OrdinalRes;
      rbInt64:    Int64   (Result^) := Registers.Int64Res;
      rbSingle:   Single  (Result^) := GetSingleResult;
      rbDouble:   Double  (Result^) := GetDoubleResult;
      rbExtended: Extended(Result^) := GetExtendedResult;
      rbCurrency: Currency(Result^) := GetCurrencyResult;
    end;
  end else
  begin
    // Discard result if float
    if ResultBehavior in [rbSingle, rbDouble, rbExtended, rbCurrency] then
      GetExtendedResult;
  end;
end;

end.

