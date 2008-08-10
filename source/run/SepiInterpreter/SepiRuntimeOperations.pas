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
  Opérations sur les variables à l'exécution
  @author sjrd
  @version 1.0
*}
unit SepiRuntimeOperations;

interface

uses
  SepiOpCodes;

procedure UnaryOp(OpCode: TSepiOpCode; VarType: TSepiBaseType;
  var Dest; const Source);
procedure BinaryOp(OpCode: TSepiOpCode; VarType: TSepiBaseType;
  var Dest; const Left, Right);

procedure Compare(OpCode: TSepiOpCode; VarType: TSepiBaseType;
  var Dest; const Left, Right);

procedure Convert(ToType, FromType: TSepiBaseType; var Dest; const Source);

function ConversionExists(SrcBaseType, DestBaseType: TSepiBaseType): Boolean;

implementation

type
  TConvertProc = procedure(DestType: TSepiBaseType; var Dest; const Source);

var
  ConvertProcs: array[TSepiBaseType] of TConvertProc;

{*
  Opération unaire
  @param OpCode    OpCode
  @param VarType   Type de variable
  @param Dest      Destination
  @param Source    Source
*}
procedure UnaryOp(OpCode: TSepiOpCode; VarType: TSepiBaseType;
  var Dest; const Source);
begin
  case OpCode of
    ocSelfInc, ocOtherInc:
    begin
      case VarType of
        btByte, btShortint: Shortint(Dest) := Shortint(Source)+1;
        btWord, btSmallint: Smallint(Dest) := Smallint(Source)+1;
        btDWord, btLongint: Longint (Dest) := Longint (Source)+1;
        btComp:             Comp    (Dest) := Comp    (Source)+1;
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfDec, ocOtherDec:
    begin
      case VarType of
        btByte, btShortint: Shortint(Dest) := Shortint(Source)-1;
        btWord, btSmallint: Smallint(Dest) := Smallint(Source)-1;
        btDWord, btLongint: Longint (Dest) := Longint (Source)-1;
        btComp:             Comp    (Dest) := Comp    (Source)-1;
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfNot, ocOtherNot:
    begin
      case VarType of
        btBoolean:          Boolean (Dest) := not Boolean (Source);
        btByte, btShortint: Shortint(Dest) := not Shortint(Source);
        btWord, btSmallint: Smallint(Dest) := not Smallint(Source);
        btDWord, btLongint: Longint (Dest) := not Longint (Source);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfNeg, ocOtherNeg:
    begin
      case VarType of
        btByte, btShortint: Shortint(Dest) := -Shortint(Source);
        btWord, btSmallint: Smallint(Dest) := -Smallint(Source);
        btDWord, btLongint: Longint (Dest) := -Longint (Source);

        btSingle:   Single  (Dest) := -Single  (Source);
        btDouble:   Double  (Dest) := -Double  (Source);
        btExtended: Extended(Dest) := -Extended(Source);
        btComp:     Comp    (Dest) := -Comp    (Source);
        btCurrency: Currency(Dest) := -Currency(Source);
        btVariant:  Variant (Dest) := -Variant (Source);
      else
        RaiseInvalidOpCode;
      end;
    end;
  else
    RaiseInvalidOpCode;
  end
end;

{*
  Opération binaire
  @param OpCode    OpCode
  @param VarType   Type de variable
  @param Dest      Destination
  @param Left      Partie gauche
  @param Right     Partie droite
*}
procedure BinaryOp(OpCode: TSepiOpCode; VarType: TSepiBaseType;
  var Dest; const Left, Right);
begin
  case OpCode of
    ocSelfAdd, ocOtherAdd:
    begin
      case VarType of
        btByte, btShortint: Shortint(Dest) := Shortint(Left) + Shortint(Right);
        btWord, btSmallint: Smallint(Dest) := Smallint(Left) + Smallint(Right);
        btDWord, btLongint: Longint (Dest) := Longint (Left) + Longint (Right);
        btInt64:            Int64   (Dest) := Int64   (Left) + Int64   (Right);

        btSingle:   Single    (Dest) := Single    (Left) + Single    (Right);
        btDouble:   Double    (Dest) := Double    (Left) + Double    (Right);
        btExtended: Extended  (Dest) := Extended  (Left) + Extended  (Right);
        btComp:     Comp      (Dest) := Comp      (Left) + Comp      (Right);
        btCurrency: Currency  (Dest) := Currency  (Left) + Currency  (Right);
        btAnsiStr:  AnsiString(Dest) := AnsiString(Left) + AnsiString(Right);
        btWideStr:  WideString(Dest) := WideString(Left) + WideString(Right);
        btVariant:  Variant   (Dest) := Variant   (Left) + Variant   (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfSubtract, ocOtherSubtract:
    begin
      case VarType of
        btByte, btShortint: Shortint(Dest) := Shortint(Left) - Shortint(Right);
        btWord, btSmallint: Smallint(Dest) := Smallint(Left) - Smallint(Right);
        btDWord, btLongint: Longint (Dest) := Longint (Left) - Longint (Right);
        btInt64:            Int64   (Dest) := Int64   (Left) - Int64   (Right);

        btSingle:   Single  (Dest) := Single  (Left) - Single  (Right);
        btDouble:   Double  (Dest) := Double  (Left) - Double  (Right);
        btExtended: Extended(Dest) := Extended(Left) - Extended(Right);
        btComp:     Comp    (Dest) := Comp    (Left) - Comp    (Right);
        btCurrency: Currency(Dest) := Currency(Left) - Currency(Right);
        btVariant:  Variant (Dest) := Variant (Left) - Variant (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfMultiply, ocOtherMultiply:
    begin
      case VarType of
        btByte:     Byte    (Dest) := Byte    (Left) * Byte    (Right);
        btShortint: Shortint(Dest) := Shortint(Left) * Shortint(Right);
        btWord:     Word    (Dest) := Word    (Left) * Word    (Right);
        btSmallint: Smallint(Dest) := Smallint(Left) * Smallint(Right);
        btDWord:    LongWord(Dest) := LongWord(Left) * LongWord(Right);
        btLongint:  Longint (Dest) := Longint (Left) * Longint (Right);
        btInt64:    Int64   (Dest) := Int64   (Left) * Int64   (Right);

        btSingle:   Single  (Dest) := Single  (Left) * Single  (Right);
        btDouble:   Double  (Dest) := Double  (Left) * Double  (Right);
        btExtended: Extended(Dest) := Extended(Left) * Extended(Right);
        btComp:     Comp    (Dest) := Comp    (Left) * Comp    (Right);
        btCurrency: Currency(Dest) := Currency(Left) * Currency(Right);
        btVariant:  Variant (Dest) := Variant (Left) * Variant (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfDivide, ocOtherDivide:
    begin
      case VarType of
        btSingle:   Single  (Dest) := Single  (Left) / Single  (Right);
        btDouble:   Double  (Dest) := Double  (Left) / Double  (Right);
        btExtended: Extended(Dest) := Extended(Left) / Extended(Right);
        btComp:     Comp    (Dest) := Comp    (Left) / Comp    (Right);
        btCurrency: Currency(Dest) := Currency(Left) / Currency(Right);
        btVariant:  Variant (Dest) := Variant (Left) / Variant (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfIntDiv, ocOtherIntDiv:
    begin
      case VarType of
        btByte:     Byte    (Dest) := Byte    (Left) div Byte    (Right);
        btShortint: Shortint(Dest) := Shortint(Left) div Shortint(Right);
        btWord:     Word    (Dest) := Word    (Left) div Word    (Right);
        btSmallint: Smallint(Dest) := Smallint(Left) div Smallint(Right);
        btDWord:    LongWord(Dest) := LongWord(Left) div LongWord(Right);
        btLongint:  Longint (Dest) := Longint (Left) div Longint (Right);
        btInt64:    Int64   (Dest) := Int64   (Left) div Int64   (Right);
        btVariant:  Variant (Dest) := Variant (Left) div Variant (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfModulus, ocOtherModulus:
    begin
      case VarType of
        btByte:     Byte    (Dest) := Byte    (Left) mod Byte    (Right);
        btShortint: Shortint(Dest) := Shortint(Left) mod Shortint(Right);
        btWord:     Word    (Dest) := Word    (Left) mod Word    (Right);
        btSmallint: Smallint(Dest) := Smallint(Left) mod Smallint(Right);
        btDWord:    LongWord(Dest) := LongWord(Left) mod LongWord(Right);
        btLongint:  Longint (Dest) := Longint (Left) mod Longint (Right);
        btInt64:    Int64   (Dest) := Int64   (Left) mod Int64   (Right);
        btVariant:  Variant (Dest) := Variant (Left) mod Variant (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfShl, ocOtherShl:
    begin
      case VarType of
        btByte, btShortint: Shortint(Dest) := Shortint(Left) shl Byte(Right);
        btWord, btSmallint: Smallint(Dest) := Smallint(Left) shl Byte(Right);
        btDWord, btLongint: Longint (Dest) := Longint (Left) shl Byte(Right);
        btInt64:            Int64   (Dest) := Int64   (Left) shl Byte(Right);

        btVariant: Variant(Dest) := Variant(Left) shl Variant(Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfShr, ocOtherShr:
    begin
      case VarType of
        btByte, btShortint: Shortint(Dest) := Shortint(Left) shr Byte(Right);
        btWord, btSmallint: Smallint(Dest) := Smallint(Left) shr Byte(Right);
        btDWord, btLongint: Longint (Dest) := Longint (Left) shr Byte(Right);
        btInt64:            Int64   (Dest) := Int64   (Left) shr Byte(Right);

        btVariant: Variant(Dest) := Variant(Left) shr Variant(Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfAnd, ocOtherAnd:
    begin
      case VarType of
        btBoolean: Boolean(Dest) := Boolean(Left) and Boolean(Right);

        btByte, btShortint:
          Shortint(Dest) := Shortint(Left) and Shortint(Right);
        btWord, btSmallint:
          Smallint(Dest) := Smallint(Left) and Smallint(Right);

        btDWord, btLongint: Longint(Dest) := Longint(Left) and Longint(Right);
        btInt64:            Int64  (Dest) := Int64  (Left) and Int64  (Right);

        btVariant: Variant(Dest) := Variant(Left) and Variant(Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfOr, ocOtherOr:
    begin
      case VarType of
        btBoolean: Boolean(Dest) := Boolean(Left) or Boolean(Right);

        btByte, btShortint:
          Shortint(Dest) := Shortint(Left) or Shortint(Right);
        btWord, btSmallint:
          Smallint(Dest) := Smallint(Left) or Smallint(Right);

        btDWord, btLongint: Longint(Dest) := Longint(Left) or Longint(Right);
        btInt64:            Int64  (Dest) := Int64  (Left) or Int64  (Right);

        btVariant: Variant(Dest) := Variant(Left) or Variant(Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocSelfXor, ocOtherXor:
    begin
      case VarType of
        btBoolean: Boolean(Dest) := Boolean(Left) xor Boolean(Right);

        btByte, btShortint:
          Shortint(Dest) := Shortint(Left) xor Shortint(Right);
        btWord, btSmallint:
          Smallint(Dest) := Smallint(Left) xor Smallint(Right);

        btDWord, btLongint: Longint(Dest) := Longint(Left) xor Longint(Right);
        btInt64:            Int64  (Dest) := Int64  (Left) xor Int64  (Right);

        btVariant: Variant(Dest) := Variant(Left) xor Variant(Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Comparaison de valeurs
  @param OpCode    OpCode
  @param VarType   Type des valeurs
  @param Dest      Destination
  @param Left      Partie gauche de la comparaison
  @param Right     Partie droite de la comparaison
*}
procedure Compare(OpCode: TSepiOpCode; VarType: TSepiBaseType;
  var Dest; const Left, Right);
begin
  case OpCode of
    ocCompEquals:
    begin
      case VarType of
        btBoolean: Boolean(Dest) := Boolean(Left) = Boolean(Right);

        btByte, btShortint: Boolean(Dest) := Shortint(Left) = Shortint(Right);
        btWord, btSmallint: Boolean(Dest) := Smallint(Left) = Smallint(Right);
        btDWord, btLongint: Boolean(Dest) := Longint (Left) = Longint (Right);
        btInt64:            Boolean(Dest) := Int64   (Left) = Int64   (Right);

        btSingle:   Boolean(Dest) := Single  (Left) = Single  (Right);
        btDouble:   Boolean(Dest) := Double  (Left) = Double  (Right);
        btExtended: Boolean(Dest) := Extended(Left) = Extended(Right);
        btComp:     Boolean(Dest) := Comp    (Left) = Comp    (Right);
        btCurrency: Boolean(Dest) := Currency(Left) = Currency(Right);

        btAnsiStr: Boolean(Dest) := AnsiString(Left) = AnsiString(Right);
        btWideStr: Boolean(Dest) := WideString(Left) = WideString(Right);
        btVariant: Boolean(Dest) := Variant   (Left) = Variant   (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocCompNotEquals:
    begin
      case VarType of
        btBoolean: Boolean(Dest) := Boolean(Left) <> Boolean(Right);

        btByte, btShortint: Boolean(Dest) := Shortint(Left) <> Shortint(Right);
        btWord, btSmallint: Boolean(Dest) := Smallint(Left) <> Smallint(Right);
        btDWord, btLongint: Boolean(Dest) := Longint (Left) <> Longint (Right);
        btInt64:            Boolean(Dest) := Int64   (Left) <> Int64   (Right);

        btSingle:   Boolean(Dest) := Single  (Left) <> Single  (Right);
        btDouble:   Boolean(Dest) := Double  (Left) <> Double  (Right);
        btExtended: Boolean(Dest) := Extended(Left) <> Extended(Right);
        btComp:     Boolean(Dest) := Comp    (Left) <> Comp    (Right);
        btCurrency: Boolean(Dest) := Currency(Left) <> Currency(Right);

        btAnsiStr: Boolean(Dest) := AnsiString(Left) <> AnsiString(Right);
        btWideStr: Boolean(Dest) := WideString(Left) <> WideString(Right);
        btVariant: Boolean(Dest) := Variant   (Left) <> Variant   (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocCompLower:
    begin
      case VarType of
        btBoolean: Boolean(Dest) := Boolean(Left) < Boolean(Right);

        btByte:     Boolean(Dest) := Byte    (Left) < Byte    (Right);
        btShortint: Boolean(Dest) := Shortint(Left) < Shortint(Right);
        btWord:     Boolean(Dest) := Word    (Left) < Word    (Right);
        btSmallint: Boolean(Dest) := Smallint(Left) < Smallint(Right);
        btDWord:    Boolean(Dest) := LongWord(Left) < LongWord(Right);
        btLongint:  Boolean(Dest) := Longint (Left) < Longint (Right);
        btInt64:    Boolean(Dest) := Int64   (Left) < Int64   (Right);

        btSingle:   Boolean(Dest) := Single  (Left) < Single  (Right);
        btDouble:   Boolean(Dest) := Double  (Left) < Double  (Right);
        btExtended: Boolean(Dest) := Extended(Left) < Extended(Right);
        btComp:     Boolean(Dest) := Comp    (Left) < Comp    (Right);
        btCurrency: Boolean(Dest) := Currency(Left) < Currency(Right);

        btAnsiStr: Boolean(Dest) := AnsiString(Left) < AnsiString(Right);
        btWideStr: Boolean(Dest) := WideString(Left) < WideString(Right);
        btVariant: Boolean(Dest) := Variant   (Left) < Variant   (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocCompGreater:
    begin
      case VarType of
        btBoolean: Boolean(Dest) := Boolean(Left) > Boolean(Right);

        btByte:     Boolean(Dest) := Byte    (Left) > Byte    (Right);
        btShortint: Boolean(Dest) := Shortint(Left) > Shortint(Right);
        btWord:     Boolean(Dest) := Word    (Left) > Word    (Right);
        btSmallint: Boolean(Dest) := Smallint(Left) > Smallint(Right);
        btDWord:    Boolean(Dest) := LongWord(Left) > LongWord(Right);
        btLongint:  Boolean(Dest) := Longint (Left) > Longint (Right);
        btInt64:    Boolean(Dest) := Int64   (Left) > Int64   (Right);

        btSingle:   Boolean(Dest) := Single  (Left) > Single  (Right);
        btDouble:   Boolean(Dest) := Double  (Left) > Double  (Right);
        btExtended: Boolean(Dest) := Extended(Left) > Extended(Right);
        btComp:     Boolean(Dest) := Comp    (Left) > Comp    (Right);
        btCurrency: Boolean(Dest) := Currency(Left) > Currency(Right);

        btAnsiStr: Boolean(Dest) := AnsiString(Left) > AnsiString(Right);
        btWideStr: Boolean(Dest) := WideString(Left) > WideString(Right);
        btVariant: Boolean(Dest) := Variant   (Left) > Variant   (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocCompLowerEq:
    begin
      case VarType of
        btBoolean: Boolean(Dest) := Boolean(Left) <= Boolean(Right);

        btByte:     Boolean(Dest) := Byte    (Left) <= Byte    (Right);
        btShortint: Boolean(Dest) := Shortint(Left) <= Shortint(Right);
        btWord:     Boolean(Dest) := Word    (Left) <= Word    (Right);
        btSmallint: Boolean(Dest) := Smallint(Left) <= Smallint(Right);
        btDWord:    Boolean(Dest) := LongWord(Left) <= LongWord(Right);
        btLongint:  Boolean(Dest) := Longint (Left) <= Longint (Right);
        btInt64:    Boolean(Dest) := Int64   (Left) <= Int64   (Right);

        btSingle:   Boolean(Dest) := Single  (Left) <= Single  (Right);
        btDouble:   Boolean(Dest) := Double  (Left) <= Double  (Right);
        btExtended: Boolean(Dest) := Extended(Left) <= Extended(Right);
        btComp:     Boolean(Dest) := Comp    (Left) <= Comp    (Right);
        btCurrency: Boolean(Dest) := Currency(Left) <= Currency(Right);

        btAnsiStr: Boolean(Dest) := AnsiString(Left) <= AnsiString(Right);
        btWideStr: Boolean(Dest) := WideString(Left) <= WideString(Right);
        btVariant: Boolean(Dest) := Variant   (Left) <= Variant   (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
    ocCompGreaterEq:
    begin
      case VarType of
        btBoolean: Boolean(Dest) := Boolean(Left) >= Boolean(Right);

        btByte:     Boolean(Dest) := Byte    (Left) >= Byte    (Right);
        btShortint: Boolean(Dest) := Shortint(Left) >= Shortint(Right);
        btWord:     Boolean(Dest) := Word    (Left) >= Word    (Right);
        btSmallint: Boolean(Dest) := Smallint(Left) >= Smallint(Right);
        btDWord:    Boolean(Dest) := LongWord(Left) >= LongWord(Right);
        btLongint:  Boolean(Dest) := Longint (Left) >= Longint (Right);
        btInt64:    Boolean(Dest) := Int64   (Left) >= Int64   (Right);

        btSingle:   Boolean(Dest) := Single  (Left) >= Single  (Right);
        btDouble:   Boolean(Dest) := Double  (Left) >= Double  (Right);
        btExtended: Boolean(Dest) := Extended(Left) >= Extended(Right);
        btComp:     Boolean(Dest) := Comp    (Left) >= Comp    (Right);
        btCurrency: Boolean(Dest) := Currency(Left) >= Currency(Right);

        btAnsiStr: Boolean(Dest) := AnsiString(Left) >= AnsiString(Right);
        btWideStr: Boolean(Dest) := WideString(Left) >= WideString(Right);
        btVariant: Boolean(Dest) := Variant   (Left) >= Variant   (Right);
      else
        RaiseInvalidOpCode;
      end;
    end;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard
  @param ToType     Type de destination
  @param FromType   Type source
  @param Dest       Destination
  @param Source     Source
*}
procedure Convert(ToType, FromType: TSepiBaseType; var Dest; const Source);
begin
  ConvertProcs[FromType](ToType, Dest, Source);
end;

{*
  Conversion standard depuis un Boolean
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromBoolean(DestType: TSepiBaseType; var Dest;
  var Source: Boolean);
begin
  case DestType of
    btBoolean:  Boolean (Dest) := Boolean (Source);
    btByte:     Byte    (Dest) := Byte    (Source);
    btWord:     Word    (Dest) := Word    (Source);
    btDWord:    LongWord(Dest) := LongWord(Source);
    btShortint: Shortint(Dest) := Shortint(Source);
    btSmallint: Smallint(Dest) := Smallint(Source);
    btLongint:  Longint (Dest) := Longint (Source);
    btInt64:    Int64   (Dest) := Int64   (Source);
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Byte
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromByte(DestType: TSepiBaseType; var Dest;
  var Source: Byte);
begin
  case DestType of
    btBoolean:  Boolean (Dest) := ByteBool(Source);
    btByte:     Byte    (Dest) := Byte    (Source);
    btWord:     Word    (Dest) := Word    (Source);
    btDWord:    LongWord(Dest) := LongWord(Source);
    btShortint: Shortint(Dest) := Shortint(Source);
    btSmallint: Smallint(Dest) := Smallint(Source);
    btLongint:  Longint (Dest) := Longint (Source);
    btInt64:    Int64   (Dest) := Int64   (Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Word
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromWord(DestType: TSepiBaseType; var Dest;
  var Source: Word);
begin
  case DestType of
    btBoolean:  Boolean (Dest) := WordBool(Source);
    btByte:     Byte    (Dest) := Byte    (Source);
    btWord:     Word    (Dest) := Word    (Source);
    btDWord:    LongWord(Dest) := LongWord(Source);
    btShortint: Shortint(Dest) := Shortint(Source);
    btSmallint: Smallint(Dest) := Smallint(Source);
    btLongint:  Longint (Dest) := Longint (Source);
    btInt64:    Int64   (Dest) := Int64   (Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un LongWord
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromLongWord(DestType: TSepiBaseType; var Dest;
  var Source: LongWord);
begin
  case DestType of
    btBoolean:  Boolean (Dest) := LongBool(Source);
    btByte:     Byte    (Dest) := Byte    (Source);
    btWord:     Word    (Dest) := Word    (Source);
    btDWord:    LongWord(Dest) := LongWord(Source);
    btShortint: Shortint(Dest) := Shortint(Source);
    btSmallint: Smallint(Dest) := Smallint(Source);
    btLongint:  Longint (Dest) := Longint (Source);
    btInt64:    Int64   (Dest) := Int64   (Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Shortint
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromShortint(DestType: TSepiBaseType; var Dest;
  var Source: Shortint);
begin
  case DestType of
    btBoolean:  Boolean (Dest) := ByteBool(Source);
    btByte:     Byte    (Dest) := Byte    (Source);
    btWord:     Word    (Dest) := Word    (Source);
    btDWord:    LongWord(Dest) := LongWord(Source);
    btShortint: Shortint(Dest) := Shortint(Source);
    btSmallint: Smallint(Dest) := Smallint(Source);
    btLongint:  Longint (Dest) := Longint (Source);
    btInt64:    Int64   (Dest) := Int64   (Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Smallint
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromSmallint(DestType: TSepiBaseType; var Dest;
  var Source: Smallint);
begin
  case DestType of
    btBoolean:  Boolean (Dest) := WordBool(Source);
    btByte:     Byte    (Dest) := Byte    (Source);
    btWord:     Word    (Dest) := Word    (Source);
    btDWord:    LongWord(Dest) := LongWord(Source);
    btShortint: Shortint(Dest) := Shortint(Source);
    btSmallint: Smallint(Dest) := Smallint(Source);
    btLongint:  Longint (Dest) := Longint (Source);
    btInt64:    Int64   (Dest) := Int64   (Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Longint
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromLongint(DestType: TSepiBaseType; var Dest;
  var Source: Longint);
begin
  case DestType of
    btBoolean:  Boolean (Dest) := LongBool(Source);
    btByte:     Byte    (Dest) := Byte    (Source);
    btWord:     Word    (Dest) := Word    (Source);
    btDWord:    LongWord(Dest) := LongWord(Source);
    btShortint: Shortint(Dest) := Shortint(Source);
    btSmallint: Smallint(Dest) := Smallint(Source);
    btLongint:  Longint (Dest) := Longint (Source);
    btInt64:    Int64   (Dest) := Int64   (Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Int64
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromInt64(DestType: TSepiBaseType; var Dest;
  var Source: Int64);
begin
  case DestType of
    btBoolean:  Boolean (Dest) := Boolean (Source);
    btByte:     Byte    (Dest) := Byte    (Source);
    btWord:     Word    (Dest) := Word    (Source);
    btDWord:    LongWord(Dest) := LongWord(Source);
    btShortint: Shortint(Dest) := Shortint(Source);
    btSmallint: Smallint(Dest) := Smallint(Source);
    btLongint:  Longint (Dest) := Longint (Source);
    btInt64:    Int64   (Dest) := Int64   (Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Single
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromSingle(DestType: TSepiBaseType; var Dest;
  var Source: Single);
begin
  case DestType of
    btByte:     Byte    (Dest) := Trunc(Source);
    btWord:     Word    (Dest) := Trunc(Source);
    btDWord:    LongWord(Dest) := Trunc(Source);
    btShortint: Shortint(Dest) := Trunc(Source);
    btSmallint: Smallint(Dest) := Trunc(Source);
    btLongint:  Longint (Dest) := Trunc(Source);
    btInt64:    Int64   (Dest) := Trunc(Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Double
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromDouble(DestType: TSepiBaseType; var Dest;
  var Source: Double);
begin
  case DestType of
    btByte:     Byte    (Dest) := Trunc(Source);
    btWord:     Word    (Dest) := Trunc(Source);
    btDWord:    LongWord(Dest) := Trunc(Source);
    btShortint: Shortint(Dest) := Trunc(Source);
    btSmallint: Smallint(Dest) := Trunc(Source);
    btLongint:  Longint (Dest) := Trunc(Source);
    btInt64:    Int64   (Dest) := Trunc(Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Extended
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromExtended(DestType: TSepiBaseType; var Dest;
  var Source: Extended);
begin
  case DestType of
    btByte:     Byte    (Dest) := Trunc(Source);
    btWord:     Word    (Dest) := Trunc(Source);
    btDWord:    LongWord(Dest) := Trunc(Source);
    btShortint: Shortint(Dest) := Trunc(Source);
    btSmallint: Smallint(Dest) := Trunc(Source);
    btLongint:  Longint (Dest) := Trunc(Source);
    btInt64:    Int64   (Dest) := Trunc(Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Comp
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromComp(DestType: TSepiBaseType; var Dest;
  var Source: Comp);
begin
  case DestType of
    btByte:     Byte    (Dest) := Trunc(Source);
    btWord:     Word    (Dest) := Trunc(Source);
    btDWord:    LongWord(Dest) := Trunc(Source);
    btShortint: Shortint(Dest) := Trunc(Source);
    btSmallint: Smallint(Dest) := Trunc(Source);
    btLongint:  Longint (Dest) := Trunc(Source);
    btInt64:    Int64   (Dest) := Trunc(Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Currency
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromCurrency(DestType: TSepiBaseType; var Dest;
  var Source: Currency);
begin
  case DestType of
    btByte:     Byte    (Dest) := Trunc(Source);
    btWord:     Word    (Dest) := Trunc(Source);
    btDWord:    LongWord(Dest) := Trunc(Source);
    btShortint: Shortint(Dest) := Trunc(Source);
    btSmallint: Smallint(Dest) := Trunc(Source);
    btLongint:  Longint (Dest) := Trunc(Source);
    btInt64:    Int64   (Dest) := Trunc(Source);
    btSingle:   Single  (Dest) := Source;
    btDouble:   Double  (Dest) := Source;
    btExtended: Extended(Dest) := Source;
    btComp:     Comp    (Dest) := Source;
    btCurrency: Currency(Dest) := Source;
    btVariant:  Variant (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis une AnsiString
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromAnsiString(DestType: TSepiBaseType; var Dest;
  var Source: AnsiString);
begin
  case DestType of
    btAnsiStr:  AnsiString(Dest) := AnsiString(Source);
    btWideStr:  WideString(Dest) := WideString(Source);
    btVariant:  Variant   (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis une WideString
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromWideString(DestType: TSepiBaseType; var Dest;
  var Source: WideString);
begin
  case DestType of
    btAnsiStr:  AnsiString(Dest) := AnsiString(Source);
    btWideStr:  WideString(Dest) := WideString(Source);
    btVariant:  Variant   (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Conversion standard depuis un Variant
  @param DestType   Type de destination
  @param Dest       Destination
  @param Source     Source
*}
procedure ConvertFromVariant(DestType: TSepiBaseType; var Dest;
  var Source: Variant);
begin
  case DestType of
    btBoolean:  Boolean   (Dest) := Source;
    btByte:     Byte      (Dest) := Source;
    btWord:     Word      (Dest) := Source;
    btDWord:    LongWord  (Dest) := Source;
    btShortint: Shortint  (Dest) := Source;
    btSmallint: Smallint  (Dest) := Source;
    btLongint:  Longint   (Dest) := Source;
    btInt64:    Int64     (Dest) := Source;
    btSingle:   Single    (Dest) := Source;
    btDouble:   Double    (Dest) := Source;
    btExtended: Extended  (Dest) := Source;
    btComp:     Comp      (Dest) := Source;
    btCurrency: Currency  (Dest) := Source;
    btAnsiStr:  AnsiString(Dest) := Source;
    btWideStr:  WideString(Dest) := Source;
    btVariant:  Variant   (Dest) := Source;
  else
    RaiseInvalidOpCode;
  end;
end;

function ConversionExists(SrcBaseType, DestBaseType: TSepiBaseType): Boolean;
const
  BoolFriendlyTypes = [
    btBoolean, btByte, btWord, btDWord, btShortint, btSmallint, btLongint,
    btInt64
  ];
var
  BaseTypes: set of TSepiBaseType;
begin
  Result := True;

  // Converting any type to itself is possible
  if SrcBaseType = DestBaseType then
    Exit;

  // Variant always supports conversion
  if (SrcBaseType = btVariant) or (DestBaseType = btVariant) then
    Exit;

  BaseTypes := [];
  Include(BaseTypes, SrcBaseType);
  Include(BaseTypes, DestBaseType);

  // String types convert between each other, but not with anything else
  if BaseTypes = [btAnsiStr, btWideStr] then
    Exit;
  if BaseTypes * [btAnsiStr, btWideStr] <> [] then
  begin
    Result := False;
    Exit;
  end;

  // Boolean type convert with integer types, but not with anything else
  if btBoolean in BaseTypes then
  begin
    Result := BaseTypes - BoolFriendlyTypes = [];
    Exit;
  end;

  // All other cases are number to number conversions, which are supported
end;

{*
  Initialise les ConvertProcs
*}
procedure InitConvertProcs;
begin
  @ConvertProcs[btBoolean]  := @ConvertFromBoolean;
  @ConvertProcs[btByte]     := @ConvertFromByte;
  @ConvertProcs[btWord]     := @ConvertFromWord;
  @ConvertProcs[btDWord]    := @ConvertFromLongWord;
  @ConvertProcs[btShortint] := @ConvertFromShortint;
  @ConvertProcs[btSmallint] := @ConvertFromSmallint;
  @ConvertProcs[btLongint]  := @ConvertFromLongint;
  @ConvertProcs[btInt64]    := @ConvertFromInt64;
  @ConvertProcs[btSingle]   := @ConvertFromSingle;
  @ConvertProcs[btDouble]   := @ConvertFromDouble;
  @ConvertProcs[btExtended] := @ConvertFromExtended;
  @ConvertProcs[btComp]     := @ConvertFromComp;
  @ConvertProcs[btCurrency] := @ConvertFromCurrency;
  @ConvertProcs[btAnsiStr]  := @ConvertFromAnsiString;
  @ConvertProcs[btWideStr]  := @ConvertFromWideString;
  @ConvertProcs[btVariant]  := @ConvertFromVariant;
end;

initialization
  InitConvertProcs;
end.

