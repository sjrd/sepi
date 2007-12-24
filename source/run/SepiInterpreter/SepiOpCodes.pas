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
  Types et constantes définissant les OpCodes de Sepi
  @author sjrd
  @version 1.0
*}
unit SepiOpCodes;

interface

uses
  SysUtils;

resourcestring
  SInvalidOpCode = 'OpCode invalide';

type
  {*
    Déclenchée si l'interpréteur rencontre un OpCode invalide
    @author sjrd
    @version 1.0
  *}
  ESepiInvalidOpCode = class(Exception);

  /// Type d'un OpCode
  TSepiOpCode = type Byte;

  /// Type d'adressage
  TSepiMemoryRef = type Byte;

  {*
    Endroit d'adressage
    - mpConstant : la variable est une constante, stockée juste derrière
    - mpLocalsByte : variable locale, offset d'un octet
    - mpLocalsWord : variable locale, offset de deux octets
    - mpParamsByte : paramètre, offset d'un octet
    - mpParamsWord : paramètre, offset de deux octets
    - mpPreparedParamsByte : paramètres en préparation, offset d'un octet
    - mpPreparedParamsWord : paramètres en préparation, offset de deux octet
    - mpGlobalVar : référence à une TSepiVariable
  *}
  TSepiMemoryPlace = (
    mpConstant, mpLocalsByte, mpLocalsWord, mpParamsByte, mpParamsWord,
    mpPreparedParamsByte, mpPreparedParamsWord, mpGlobalVar
  );

  {*
    Type de déréférencement
    - dkNone : pas de déréférencement
    - dkSimple : simple pointeur, sans offset supplémentaire
    - dkPlusShortint : pointeur avec un offset Shortint
    - dkPlusSmallint : pointeur avec un offset Smallint
    - dkPlusLongint : pointeur avec un offset Longint
    - dkDouble : double déréférencement (Pointer^^) sans offset supplémentaire
  *}
  TSepiDereferenceKind = (
    dkNone, dkSimple, dkPlusShortint, dkPlusSmallint, dkPlusLongint, dkDouble
  );

  {*
    Types de données de base que gère l'interpréteur Sepi
  *}
  TSepiBaseType = (
    btBoolean, btByte, btWord, btDWord, btQWord, btShortint, btSmallint,
    btLongint, btInt64, btSingle, btDouble, btExtended, btComp, btCurrency,
    btAnsiStr, btWideStr, btVariant
  );

  {*
    Type de destination d'un JUMP
    jdkShortint, jdkSmallint et jdkLongint ont des valeurs relatives par
    rapport à la fin de l'instruction de JUMP.
    jdkMemory est une valeur pointeur absolue lue en mémoire
  *}
  TSepiJumpDestKind = (jdkShortint, jdkSmallint, jdkLongint, jdkMemory);

const
  MemPlaceMask = $0F; /// Memory place mask
  MemDerefMask = $F0; /// Memory dereference mask
  MemDerefShift = 4;  /// Memory dereference shift

const
  {
    Mem := TSepiMemoryPlace + Value [+ DerefValue]
    Value :=
      Constant           -> Constant of the relevant type
      LocalsByte         -> Byte offset in local vars
      LocalsWord         -> Word offset in local vars
      ParamsByte         -> Byte offset in parameters
      ParamsWord         -> Word offset in parameters
      PreparedParamsByte -> Byte offset in prepared parameters
      PreparedParamsWord -> Word offset in prepared parameters
      GlobalVar          -> Variable reference
    Dest := TSepiJumpestKind + DestValue
    DestValue :=
      Shortint -> Shortint relative value
      Smallint -> Smallint relative value
      Longint  -> Longint relative value
      Memory   -> Mem (absolute)
    Type := TSepiBaseType
  }

  // No category
  ocNope        = TSepiOpCode($00); /// NOP
  ocExtended    = TSepiOpCode($01); /// Instruction étendue (non utilisé)

  // Flow control (destinations are relative to end of instruction)
  ocJump          = TSepiOpCode($02); /// JUMP Dest
  ocJumpIfTrue    = TSepiOpCode($03); /// JIT  Dest, Test
  ocJumpIfFalse   = TSepiOpCode($04); /// JIF  Dest, Test
  ocReturn        = TSepiOpCode($05); /// RET
  ocPrepareParams = TSepiOpCode($06); /// PRPA Word-Size
  ocCall          = TSepiOpCode($07); /// CALL Method-Ref
  ocCallResult    = TSepiOpCode($08); /// CALL Method-Ref Result

  // Memory moves
  ocLoadAddress = TSepiOpCode($10); /// LEA   Dest, Src
  ocMoveByte    = TSepiOpCode($11); /// MOVB  Dest, Src
  ocMoveWord    = TSepiOpCode($12); /// MOVW  Dest, Src
  ocMoveDWord   = TSepiOpCode($13); /// MOVD  Dest, Src
  ocMoveQWord   = TSepiOpCode($14); /// MOVQ  Dest, Src
  ocMoveExt     = TSepiOpCode($15); /// MOVE  Dest, Src
  ocMoveAnsiStr = TSepiOpCode($16); /// MOVAS Dest, Src
  ocMoveWideStr = TSepiOpCode($17); /// MOVWS Dest, Src
  ocMoveVariant = TSepiOpCode($18); /// MOVV  Dest, Src
  ocMoveIntf    = TSepiOpCode($19); /// MOVI  Dest, Src
  ocMoveSome    = TSepiOpCode($1A); /// MOVS  Byte-Count, Dest, Src
  ocMoveMany    = TSepiOpCode($1B); /// MOVM  Word-Count, Dest, Src
  ocMoveOther   = TSepiOpCode($1C); /// MOVO  Type-Ref, Dest, Src
  ocConvert     = TSepiOpCode($1D); /// CVRT  Type, Type, Mem, Mem

  // Self dest unary operations
  ocSelfInc = TSepiOpCode($20); /// INC Type, Var
  ocSelfDec = TSepiOpCode($21); /// DEC Type, Var
  ocSelfNot = TSepiOpCode($22); /// NOT Type, Var
  ocSelfNeg = TSepiOpCode($23); /// NEG Type, Var

  // Self dest binary operations
  ocSelfAdd      = TSepiOpCode($24); /// ADD Type, Var, Value
  ocSelfSubtract = TSepiOpCode($25); /// SUB Type, Var, Value
  ocSelfMultiply = TSepiOpCode($26); /// MUL Type, Var, Value
  ocSelfDivide   = TSepiOpCode($27); /// DIV Type, Var, Value
  ocSelfIntDiv   = TSepiOpCode($28); /// DIV Type, Var, Value
  ocSelfModulus  = TSepiOpCode($29); /// MOD Type, Var, Value
  ocSelfShl      = TSepiOpCode($2A); /// SHL Type, Var, Value
  ocSelfShr      = TSepiOpCode($2B); /// SHR Type, Var, Value
  ocSelfSal      = TSepiOpCode($2A); /// SAL Type, Var, Value
  ocSelfSar      = TSepiOpCode($2C); /// SAR Type, Var, Value
  ocSelfAnd      = TSepiOpCode($2D); /// AND Type, Var, Value
  ocSelfOr       = TSepiOpCode($2E); /// OR  Type, Var, Value
  ocSelfXor      = TSepiOpCode($2F); /// XOR Type, Var, Value

  // Other dest unary operations
  ocOtherInc = TSepiOpCode($30); /// INC Type, Dest, Value
  ocOtherDec = TSepiOpCode($31); /// DEC Type, Dest, Value
  ocOtherNot = TSepiOpCode($32); /// NOT Type, Dest, Value
  ocOtherNeg = TSepiOpCode($33); /// NEG Type, Dest, Value

  // Other dest binary operations
  ocOtherAdd      = TSepiOpCode($34); /// ADD Type, Dest, Left, Right
  ocOtherSubtract = TSepiOpCode($35); /// SUB Type, Dest, Left, Right
  ocOtherMultiply = TSepiOpCode($36); /// MUL Type, Dest, Left, Right
  ocOtherDivide   = TSepiOpCode($37); /// DIV Type, Dest, Left, Right
  ocOtherIntDiv   = TSepiOpCode($38); /// DIV Type, Dest, Left, Right
  ocOtherModulus  = TSepiOpCode($39); /// MOD Type, Dest, Left, Right
  ocOtherShl      = TSepiOpCode($3A); /// SHL Type, Dest, Left, Right
  ocOtherShr      = TSepiOpCode($3B); /// SHR Type, Dest, Left, Right
  ocOtherSal      = TSepiOpCode($3A); /// SAL Type, Dest, Left, Right
  ocOtherSar      = TSepiOpCode($3C); /// SAR Type, Dest, Left, Right
  ocOtherAnd      = TSepiOpCode($3D); /// AND Type, Dest, Left, Right
  ocOtherOr       = TSepiOpCode($3E); /// OR  Type, Dest, Left, Right
  ocOtherXor      = TSepiOpCode($3F); /// XOR Type, Dest, Left, Right

  // Comparisons
  ocCompEquals    = TSepiOpCode($40); /// EQ  Type, Dest, Left, Right
  ocCompNotEquals = TSepiOpCode($41); /// NEQ Type, Dest, Left, Right
  ocCompLower     = TSepiOpCode($42); /// LT  Type, Dest, Left, Right
  ocCompGreater   = TSepiOpCode($43); /// GT  Type, Dest, Left, Right
  ocCompLowerEq   = TSepiOpCode($44); /// LE  Type, Dest, Left, Right
  ocCompGreaterEq = TSepiOpCode($45); /// GE  Type, Dest, Left, Right

procedure RaiseInvalidOpCode;

implementation

{*
  Déclenche une exception OpCode invalide
*}
procedure RaiseInvalidOpCode;

  function ReturnAddress: Pointer;
  asm
        MOV     EAX,[ESP]
  end;

begin
  raise ESepiInvalidOpCode.CreateRes(@SInvalidOpCode) at ReturnAddress;
end;

end.

