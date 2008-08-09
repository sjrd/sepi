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
  Désassembleur Sepi
  @author sjrd
  @version 1.0
*}
unit SepiDisassembler;

interface

uses
  SysUtils, Classes, ScClasses, SepiReflectionCore, SepiMembers, SepiOpCodes,
  SepiRuntime;

type
  {*
    Désassembleur Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiDisassembler = class(TObject)
  private
    RuntimeUnit: TSepiRuntimeUnit;       /// Unité d'exécution
    Instructions: TAbsoluteMemoryStream; /// Instructions à désassembler

    // Op-code functions

    function UnknownOpCode(OpCode: TSepiOpCode): string;

    function OpCodeNope(OpCode: TSepiOpCode): string;
    function OpCodeJump(OpCode: TSepiOpCode): string;
    function OpCodeJumpIf(OpCode: TSepiOpCode): string;

    function OpCodeAddressCall(OpCode: TSepiOpCode): string;
    function OpCodeStaticCall(OpCode: TSepiOpCode): string;
    function OpCodeDynamicCall(OpCode: TSepiOpCode): string;

    function OpCodeLoadAddress(OpCode: TSepiOpCode): string;
    function OpCodeSimpleMove(OpCode: TSepiOpCode): string;
    function OpCodeMoveFixed(OpCode: TSepiOpCode): string;
    function OpCodeMoveOther(OpCode: TSepiOpCode): string;
    function OpCodeConvert(OpCode: TSepiOpCode): string;

    function OpCodeSelfUnaryOp(OpCode: TSepiOpCode): string;
    function OpCodeSelfBinaryOp(OpCode: TSepiOpCode): string;
    function OpCodeOtherUnaryOp(OpCode: TSepiOpCode): string;
    function OpCodeOtherBinaryOp(OpCode: TSepiOpCode): string;

    function OpCodeCompare(OpCode: TSepiOpCode): string;

    function OpCodeGetRuntimeInfo(OpCode: TSepiOpCode): string;

    function OpCodeIsClass(OpCode: TSepiOpCode): string;
    function OpCodeAsClass(OpCode: TSepiOpCode): string;

    function OpCodeRaise(OpCode: TSepiOpCode): string;
    function OpCodeReraise(OpCode: TSepiOpCode): string;
    function OpCodeTryExcept(OpCode: TSepiOpCode): string;
    function OpCodeTryFinally(OpCode: TSepiOpCode): string;
    function OpCodeMultiOn(OpCode: TSepiOpCode): string;

    // Other methods

    function ReadRef: string;
    function ReadBaseAddress(Options: TSepiAddressOptions; ConstSize: Integer;
      MemorySpace: TSepiMemorySpace): string;
    procedure ReadAddressOperation(var Address: string);
    function ReadAddress(Options: TSepiAddressOptions = [];
      ConstSize: Integer = 0): string;
    function ReadClassValue: string;
    function ReadParams: string;

    function DisassembleInstruction: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Disassemble(Code: Pointer; Result: TStrings;
      RunUnit: TSepiRuntimeUnit; CodeSize, MaxInstructions: Integer);
  end;

implementation

type
  /// Mnémonique d'un OpCode
  TOpCodeName = string[5];

type
  /// Méthode de traitement d'un OpCode
  TOpCodeArgsFunc = function(Self: TSepiDisassembler;
    OpCode: TSepiOpCode): string;

var
  /// Tableau des méthodes de traitement des OpCodes
  OpCodeArgsFuncs: array[TSepiOpCode] of TOpCodeArgsFunc;

const
  MaxKnownOpCode = ocMultiOn; /// Plus grand OpCode connu

  /// Nom des OpCodes
  { Don't localize any of these strings! }
  OpCodeNames: array[ocNope..MaxKnownOpCode] of TOpCodeName = (
    // No category
    'NOP', 'EXT',
    // Flow control
    'JUMP', 'JIT', 'JIF',
    // Calls
    'CALL', 'CALL', 'CALL', '', '', '', '', '', '', '', '',
    // Memory moves
    'LEA', 'MOVB', 'MOVW', 'MOVD', 'MOVQ', 'MOVE', 'MOVAS', 'MOVWS', 'MOVV',
    'MOVI', 'MOVS', 'MOVM', 'MOVO', 'CVRT', '', '',
    // Self dest unary operations
    'INC', 'DEC', 'NOT', 'NEG',
    // Self dest binary operations
    'ADD', 'SUB', 'MUL', 'DIV', 'IDV', 'MOD', 'SHL', 'SHR', 'SAR',
    'AND', 'OR', 'XOR',
    // Other dest unary operations
    'INC', 'DEC', 'NOT', 'NEG',
    // Other dest binary operations
    'ADD', 'SUB', 'MUL', 'DIV', 'IDV', 'MOD', 'SHL', 'SHR', 'SAR',
    'AND', 'OR', 'XOR',
    // Comparisons
    'EQ', 'NEQ', 'LT', 'GT', 'LE', 'GE',
    '', '', '', '', '', '', '', '', '', '',
    // Compile time objects which must be read at runtime in Sepi
    'GTI', 'GDC', 'GMC',
    // is and as operators
    'IS', 'AS', '', '', '', '', '', '', '', '', '', '', '',
    // Exception handling
    'RAISE', 'RERS', 'TRYE', 'TRYF', 'ON'
  );

  /// Virgule
  Comma = ', '; {don't localize}

  /// Nom des conventions d'appel
  CallingConventionNames: array[TCallingConvention] of string = (
    'register', 'cdecl', 'pascal', 'stdcall', 'safecall' {don't localize}
  );

  /// Nom des comportements de résultat
  ResultBehaviorNames: array[TSepiTypeResultBehavior] of string = (
    'none', 'ordinal', 'int64', 'single', 'double', 'extended', 'currency',
    'parameter'
  );

  /// Nom des types de données de base de Sepi
  BaseTypeNames: array[TSepiBaseType] of string = (
    'Boolean', 'Byte', 'Word', 'DWord', 'QWord', 'Shortint', 'Smallint',
    'Longint', 'Int64', 'Single', 'Double', 'Extended', 'Comp', 'Currency',
    'AnsiStr', 'WideStr', 'Variant'
  );

{*
  Initialise les OpCodeArgsFuncs
*}
procedure InitOpCodeArgsFuncs;
var
  I: TSepiOpCode;
begin
  for I := $00 to $FF do
    @OpCodeArgsFuncs[I] := @TSepiDisassembler.UnknownOpCode;

  // No category
  @OpCodeArgsFuncs[ocNope]     := @TSepiDisassembler.OpCodeNope;
  @OpCodeArgsFuncs[ocExtended] := @TSepiDisassembler.UnknownOpCode;

  // Flow control
  @OpCodeArgsFuncs[ocJump]        := @TSepiDisassembler.OpCodeJump;
  @OpCodeArgsFuncs[ocJumpIfTrue]  := @TSepiDisassembler.OpCodeJumpIf;
  @OpCodeArgsFuncs[ocJumpIfFalse] := @TSepiDisassembler.OpCodeJumpIf;

  // Calls
  @OpCodeArgsFuncs[ocAddressCall] := @TSepiDisassembler.OpCodeAddressCall;
  @OpCodeArgsFuncs[ocStaticCall]  := @TSepiDisassembler.OpCodeStaticCall;
  @OpCodeArgsFuncs[ocDynamicCall] := @TSepiDisassembler.OpCodeDynamicCall;

  // Memory moves
  @OpCodeArgsFuncs[ocLoadAddress] := @TSepiDisassembler.OpCodeLoadAddress;
  for I := ocMoveByte to ocMoveIntf do
    @OpCodeArgsFuncs[I] := @TSepiDisassembler.OpCodeSimpleMove;
  @OpCodeArgsFuncs[ocMoveSome]  := @TSepiDisassembler.OpCodeMoveFixed;
  @OpCodeArgsFuncs[ocMoveMany]  := @TSepiDisassembler.OpCodeMoveFixed;
  @OpCodeArgsFuncs[ocMoveOther] := @TSepiDisassembler.OpCodeMoveOther;
  @OpCodeArgsFuncs[ocConvert]   := @TSepiDisassembler.OpCodeConvert;

  // Self dest unary operations
  for I := ocSelfInc to ocSelfNeg do
    @OpCodeArgsFuncs[I] := @TSepiDisassembler.OpCodeSelfUnaryOp;

  // Self dest binary operations
  for I := ocSelfAdd to ocSelfXor do
    @OpCodeArgsFuncs[I] := @TSepiDisassembler.OpCodeSelfBinaryOp;

  // Other dest unary operations
  for I := ocOtherInc to ocOtherNeg do
    @OpCodeArgsFuncs[I] := @TSepiDisassembler.OpCodeOtherUnaryOp;

  // Other dest binary operations
  for I := ocOtherAdd to ocOtherXor do
    @OpCodeArgsFuncs[I] := @TSepiDisassembler.OpCodeOtherBinaryOp;

  // Comparisons
  for I := ocCompEquals to ocCompGreaterEq do
    @OpCodeArgsFuncs[I] := @TSepiDisassembler.OpCodeCompare;

  // Compile time objects which must be read at runtime in Sepi
  @OpCodeArgsFuncs[ocGetTypeInfo]    := @TSepiDisassembler.OpCodeGetRuntimeInfo;
  @OpCodeArgsFuncs[ocGetDelphiClass] := @TSepiDisassembler.OpCodeGetRuntimeInfo;
  @OpCodeArgsFuncs[ocGetMethodCode]  := @TSepiDisassembler.OpCodeGetRuntimeInfo;

  // is and as operators
  @OpCodeArgsFuncs[ocIsClass] := @TSepiDisassembler.OpCodeIsClass;
  @OpCodeArgsFuncs[ocAsClass] := @TSepiDisassembler.OpCodeAsClass;

  // Exception handling
  @OpCodeArgsFuncs[ocRaise]      := @TSepiDisassembler.OpCodeRaise;
  @OpCodeArgsFuncs[ocReraise]    := @TSepiDisassembler.OpCodeReraise;
  @OpCodeArgsFuncs[ocTryExcept]  := @TSepiDisassembler.OpCodeTryExcept;
  @OpCodeArgsFuncs[ocTryFinally] := @TSepiDisassembler.OpCodeTryFinally;
  @OpCodeArgsFuncs[ocMultiOn]    := @TSepiDisassembler.OpCodeMultiOn;
end;

{-------------------------}
{ TSepiDisassembler class }
{-------------------------}

{*
  Crée un désassembleur Sepi
*}
constructor TSepiDisassembler.Create;
begin
  inherited Create;

  Instructions := TAbsoluteMemoryStream.Create;
end;

{*
  [@inheritDoc]
*}
destructor TSepiDisassembler.Destroy;
begin
  Instructions.Free;

  inherited;
end;

{*
  OpCode inconnu
  @param OpCode   OpCode
*}
function TSepiDisassembler.UnknownOpCode(OpCode: TSepiOpCode): string;
begin
  RaiseInvalidOpCode;
  Result := ''; // avoid compiler warning
end;

{*
  OpCode Nope
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeNope(OpCode: TSepiOpCode): string;
begin
  Result := '';
end;

{*
  OpCode Jump
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeJump(OpCode: TSepiOpCode): string;
var
  Offset: Smallint;
begin
  Instructions.ReadBuffer(Offset, SizeOf(Smallint));
  Result := '$' + IntToHex(Instructions.Position + Offset, 8);
end;

{*
  OpCode Jump If (True or False)
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeJumpIf(OpCode: TSepiOpCode): string;
var
  Offset: Smallint;
  TestPtr: string;
begin
  Instructions.ReadBuffer(Offset, SizeOf(Smallint));
  TestPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Boolean));

  Result := '$' + IntToHex(Instructions.Position + Offset, 8) + Comma +
    TestPtr;
end;

{*
  OpCode BasicCall
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeAddressCall(OpCode: TSepiOpCode): string;
var
  CallSettings: TSepiCallSettings;
  CallingConvention: TCallingConvention;
  RegUsage: Byte;
  ResultBehavior: TSepiTypeResultBehavior;
  AddressPtr: string;
  Parameters: string;
begin
  // Read the instruction
  Instructions.ReadBuffer(CallSettings, 1);
  CallSettingsDecode(CallSettings, CallingConvention,
    RegUsage, ResultBehavior);
  AddressPtr := ReadAddress;
  Parameters := ReadParams;

  // Format arguments
  Result := Format('(%s, %d, %s) %s%s', {don't localize}
    [CallingConventionNames[CallingConvention], RegUsage,
    ResultBehaviorNames[ResultBehavior], AddressPtr, Parameters]);
end;

{*
  OpCode StaticCall
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeStaticCall(OpCode: TSepiOpCode): string;
var
  Method: string;
  Parameters: string;
begin
  // Read the instruction
  Method := ReadRef;
  Parameters := ReadParams;

  // Format arguments
  Result := Format('%s%s', {don't localize}
    [Method, Parameters]);
end;

{*
  OpCode DynamicCall
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeDynamicCall(OpCode: TSepiOpCode): string;
var
  Method: string;
  SelfPtr: string;
  Parameters: string;
begin
  // Read the instruction
  Method := ReadRef;
  SelfPtr := ReadAddress([aoAcceptZero]);
  Parameters := ReadParams;

  // Format arguments
  Result := Format('%s -> %s%s', {don't localize}
    [SelfPtr, Method, Parameters]);
end;

{*
  OpCode LoadAddress
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeLoadAddress(OpCode: TSepiOpCode): string;
var
  DestPtr: string;
  SourcePtr: string;
begin
  DestPtr := ReadAddress;
  SourcePtr := ReadAddress;

  Result := DestPtr + Comma + SourcePtr;
end;

{*
  OpCode Move
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeSimpleMove(OpCode: TSepiOpCode): string;
const
  ConstSizes: array[ocMoveByte..ocMoveWideStr] of Integer = (
    1, 2, 4, 8, 10, 0, 0
  );
var
  DestPtr: string;
  SourcePtr: string;
begin
  DestPtr := ReadAddress;
  if OpCode in [ocMoveVariant, ocMoveIntf] then
    SourcePtr := ReadAddress([aoAcceptZero])
  else
    SourcePtr := ReadAddress(aoAcceptAllConsts, ConstSizes[OpCode]);

  Result := DestPtr + Comma + SourcePtr;
end;

{*
  OpCode MoveSome et MoveMany
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeMoveFixed(OpCode: TSepiOpCode): string;
var
  Count: Integer;
  DestPtr: string;
  SourcePtr: string;
begin
  // Read count
  Count := 0;
  if OpCode = ocMoveSome then
    Instructions.ReadBuffer(Count, 1)
  else
    Instructions.ReadBuffer(Count, 2);

  // Read dest and source
  DestPtr := ReadAddress;
  if Count <= SizeOf(Variant) then
    SourcePtr := ReadAddress(aoAcceptAllConsts, Count)
  else
    SourcePtr := ReadAddress([aoAcceptAddressedConst]);

  // Format arguments
  Result := Format('%d, %s, %s', {don't localize}
    [Count, DestPtr, SourcePtr]);
end;

{*
  OpCode MoveOther
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeMoveOther(OpCode: TSepiOpCode): string;
var
  VarType: string;
  DestPtr: string;
  SourcePtr: string;
begin
  // Read instruction
  VarType := ReadRef;
  DestPtr := ReadAddress;
  SourcePtr := ReadAddress{(SepiType.Size)};

  // Format arguments
  Result := Format('%s, %s, %s', {don't localize}
    [VarType, DestPtr, SourcePtr]);
end;

{*
  OpCode Convert
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeConvert(OpCode: TSepiOpCode): string;
var
  ToType, FromType: TSepiBaseType;
  DestPtr, SourcePtr: string;
begin
  Instructions.ReadBuffer(ToType, SizeOf(TSepiBaseType));
  Instructions.ReadBuffer(FromType, SizeOf(TSepiBaseType));
  DestPtr := ReadAddress;
  SourcePtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[FromType]);

  Result := Format('(%s, %s), %s, %s', {don't localize}
    [BaseTypeNames[ToType], BaseTypeNames[FromType], DestPtr, SourcePtr]);
end;

{*
  Opérations unaires sur soi-même
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeSelfUnaryOp(OpCode: TSepiOpCode): string;
var
  VarType: TSepiBaseType;
  VarPtr: string;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  VarPtr := ReadAddress;

  Result := Format('(%s) %s', {don't localize}
    [BaseTypeNames[VarType], VarPtr]);
end;

{*
  Opérations binaires sur soi-même
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeSelfBinaryOp(OpCode: TSepiOpCode): string;
var
  VarType: TSepiBaseType;
  VarPtr, ValuePtr: string;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  VarPtr := ReadAddress;
  if OpCode in [ocSelfShl, ocSelfShr, ocSelfSar] then
    ValuePtr := ReadAddress(aoAcceptAllConsts, 1)
  else
    ValuePtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);

  Result := Format('(%s) %s, %s', {don't localize}
    [BaseTypeNames[VarType], VarPtr, ValuePtr]);
end;

{*
  Opérations unaires sur un autre
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeOtherUnaryOp(OpCode: TSepiOpCode): string;
var
  VarType: TSepiBaseType;
  DestPtr, ValuePtr: string;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  DestPtr := ReadAddress;
  ValuePtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);

  Result := Format('(%s) %s, %s', {don't localize}
    [BaseTypeNames[VarType], DestPtr, ValuePtr]);
end;

{*
  Opérations binaires sur un autre
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeOtherBinaryOp(OpCode: TSepiOpCode): string;
var
  VarType: TSepiBaseType;
  DestPtr, LeftPtr, RightPtr: string;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  DestPtr := ReadAddress;
  LeftPtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);
  if OpCode in [ocOtherShl, ocOtherShr, ocOtherSar] then
    RightPtr := ReadAddress(aoAcceptAllConsts, 1)
  else
    RightPtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);

  Result := Format('(%s) %s, %s, %s', {don't localize}
    [BaseTypeNames[VarType], DestPtr, LeftPtr, RightPtr]);
end;

{*
  OpCode de comparaison
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeCompare(OpCode: TSepiOpCode): string;
var
  VarType: TSepiBaseType;
  DestPtr, LeftPtr, RightPtr: string;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  DestPtr := ReadAddress;
  LeftPtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);
  RightPtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);

  Result := Format('(%s) %s, %s, %s', {don't localize}
    [BaseTypeNames[VarType], DestPtr, LeftPtr, RightPtr]);
end;

{*
  OpCode GetTypeInfo, GetDelphiClass ou GetMethodCode
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeGetRuntimeInfo(OpCode: TSepiOpCode): string;
var
  DestPtr: string;
  Reference: string;
begin
  DestPtr := ReadAddress;
  Reference := ReadRef;

  Result := Format('%s, %s', {don't localize}
    [DestPtr, Reference]);
end;

{*
  OpCode IsClass
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeIsClass(OpCode: TSepiOpCode): string;
var
  DestPtr: string;
  ObjectPtr: string;
  DelphiClass: string;
begin
  DestPtr := ReadAddress;
  ObjectPtr := ReadAddress([aoAcceptZero]);
  DelphiClass := ReadClassValue;

  Result := Format('%s, %s, %s', {don't localize}
    [DestPtr, ObjectPtr, DelphiClass]);
end;

{*
  OpCode AsClass
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeAsClass(OpCode: TSepiOpCode): string;
var
  ObjectPtr: string;
  DelphiClass: string;
begin
  ObjectPtr := ReadAddress([aoAcceptZero]);
  DelphiClass := ReadClassValue;

  Result := Format('%s, %s', {don't localize}
    [ObjectPtr, DelphiClass]);
end;

{*
  OpCode Raise
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeRaise(OpCode: TSepiOpCode): string;
begin
  Result := ReadAddress;
end;

{*
  OpCode Reraise
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeReraise(OpCode: TSepiOpCode): string;
begin
  Result := '';
end;

{*
  OpCode BeginTryExcept
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeTryExcept(OpCode: TSepiOpCode): string;
var
  TrySize, ExceptSize: Word;
  ExceptObjectPtr: string;
begin
  // Read instruction
  Instructions.ReadBuffer(TrySize, SizeOf(Word));
  Instructions.ReadBuffer(ExceptSize, SizeOf(Word));
  ExceptObjectPtr := ReadAddress([aoZeroAsNil]);

  // Format arguments
  Result := Format('%d, %d', [TrySize, ExceptSize]); {don't localize}
  if ExceptObjectPtr <> '' then
    Result := Result + Comma + ExceptObjectPtr;
end;

{*
  OpCode BeginTryFinally
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeTryFinally(OpCode: TSepiOpCode): string;
var
  TrySize, FinallySize: Word;
begin
  // Read instruction
  Instructions.ReadBuffer(TrySize, SizeOf(Word));
  Instructions.ReadBuffer(FinallySize, SizeOf(Word));

  // Format arguments
  Result := Format('%d, %d', [TrySize, FinallySize]); {don't localize}
end;

{*
  OpCode MultiOn
  @param OpCode   OpCode
*}
function TSepiDisassembler.OpCodeMultiOn(OpCode: TSepiOpCode): string;
var
  I, Count: Integer;
  ClassName: string;
  Offset: Smallint;
  Dest: Integer;
begin
  // Read object pointer and count
  Result := ReadAddress;
  Count := 0;
  Instructions.ReadBuffer(Count, 1);

  // Read pairs Class/Offset
  for I := 0 to Count-1 do
  begin
    ClassName := ReadRef;
    Instructions.ReadBuffer(Offset, SizeOf(Smallint));

    Dest := Integer(Instructions.PointerPos) + Offset;
    Result := Result + Comma + ClassName + ':$' + IntToHex(Dest, 8);
  end;
end;

{*
  Lit une référence depuis les instructions
*}
function TSepiDisassembler.ReadRef: string;
var
  Meta: TSepiMeta;
  Ref: Integer;
begin
  if Assigned(RuntimeUnit) then
  begin
    RuntimeUnit.ReadRef(Instructions, Meta);
    Result := Meta.GetFullName;
  end else
  begin
    Instructions.ReadBuffer(Ref, 4);
    Result := IntToStr(Ref);
  end;
end;

{*
  Lit une adresse de base depuis les instructions
  @param Options       Options de lecture d'addresse
  @param ConstSize     Taille d'une constante dans le code
  @param MemorySpace   Espace d'adressage
  @return Adresse de base lue
*}
function TSepiDisassembler.ReadBaseAddress(Options: TSepiAddressOptions;
  ConstSize: Integer; MemorySpace: TSepiMemorySpace): string;
const // don't localize
  LocalsName = 'LO:';
  ParamsName = 'PA:';
  ZeroName = '0';
var
  I: Integer;
  ByteOffset: Byte;
  WordOffset: Word;
begin
  // Read base address
  case MemorySpace of
    msZero:
    begin
      if aoZeroAsNil in Options then
      begin
        // Treat constant as nil return value
        Result := '';
      end else
      begin
        if not (aoAcceptZero in Options) then
          RaiseInvalidOpCode;
        Result := ZeroName;
      end;
    end;
    msConstant:
    begin
      // Read the constant directly into the code
      if not (aoAcceptConstInCode in Options) then
        RaiseInvalidOpCode;

      Result := '$'; {don't localize}
      for I := 0 to ConstSize-1 do
      begin
        Instructions.ReadBuffer(ByteOffset, 1);
        Result := Result + IntToHex(ByteOffset, 2);
      end;
    end;
    msLocalsBase:
    begin
      // Local variables, no offset
      Result := LocalsName + ZeroName;
    end;
    msLocalsByte:
    begin
      // Local variables, byte-offset
      Instructions.ReadBuffer(ByteOffset, 1);
      Result := LocalsName + IntToStr(ByteOffset);
    end;
    msLocalsWord:
    begin
      // Local variables, word-offset
      Instructions.ReadBuffer(WordOffset, 2);
      Result := LocalsName + IntToStr(WordOffset);
    end;
    msParamsBase:
    begin
      // Parameters, no offset
      Result := ParamsName + ZeroName;
    end;
    msParamsByte:
    begin
      // Parameters, byte-offset
      Instructions.ReadBuffer(ByteOffset, 1);
      Result := ParamsName + IntToStr(ByteOffset);
    end;
    msParamsWord:
    begin
      // Parameters, word-offset
      Instructions.ReadBuffer(WordOffset, 2);
      Result := ParamsName + IntToStr(WordOffset);
    end;
    msTrueConst:
    begin
      // Reference to TSepiConstant
      if not (aoAcceptTrueConst in Options) then
        RaiseInvalidOpCode;
      Result := ReadRef;
    end;
    msVariable:
    begin
      // Reference to TSepiVariable
      Result := ReadRef;
    end;
  else
    RaiseInvalidOpCode;
    Result := ''; // avoid compiler warning
  end;
end;

{*
  Lit une opération sur une adresse et l'applique à une adresse donnée
  @param Address   Adresse à modifier
*}
procedure TSepiDisassembler.ReadAddressOperation(var Address: string);
var
  IntAddress: Integer absolute Address;
  AddrDerefAndOp: TSepiAddressDerefAndOp;
  AddrDereference: TSepiAddressDereference;
  AddrOperation: TSepiAddressOperation;
  ShortOffset: Shortint;
  SmallOffset: Smallint;
  LongOffset: Longint;
  OffsetPtr: string;
  ShortFactor: Shortint;
begin
  // Read deref and op
  Instructions.ReadBuffer(AddrDerefAndOp, SizeOf(TSepiAddressDerefAndOp));
  AddressDerefAndOpDecode(AddrDerefAndOp, AddrDereference, AddrOperation);

  // Handle dereference
  case AddrDereference of
    adNone: ;
    adSimple: Address := '('+Address+')^';  {don't localize}
    adDouble: Address := '('+Address+')^^'; {don't localize}
  else
    RaiseInvalidOpCode;
  end;

  // Handle operation
  case AddrOperation of
    aoNone: ;
    aoPlusConstShortint:
    begin
      // Read a Shortint from code, and add it to the address
      Instructions.ReadBuffer(ShortOffset, 1);
      Address := Address + '+' + IntToStr(ShortOffset);
    end;
    aoPlusConstSmallint:
    begin
      // Read a Smallint from code, and add it to the address
      Instructions.ReadBuffer(SmallOffset, 1);
      Address := Address + '+' + IntToStr(SmallOffset);
    end;
    aoPlusConstLongint:
    begin
      // Read a Longint from code, and add it to the address
      Instructions.ReadBuffer(LongOffset, 1);
      Address := Address + '+' + IntToStr(LongOffset);
    end;
    aoPlusMemShortint:
    begin
      // Read a Shortint from memory, and add it to the address
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Shortint));
      Address := Address + '+(' + OffsetPtr + ')';
    end;
    aoPlusMemSmallint:
    begin
      // Read a Smallint from memory, and add it to the address
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Smallint));
      Address := Address + '+(' + OffsetPtr + ')';
    end;
    aoPlusMemLongint:
    begin
      // Read a Longint from memory, and add it to the address
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Longint));
      Address := Address + '+(' + OffsetPtr + ')';
    end;
    aoPlusConstTimesMemShortint:
    begin
      { Read a Shortint from code and a Shortint from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(ShortFactor, 1);
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Shortint));
      Address := Address + '+' + IntToStr(ShortFactor) + '*(' + OffsetPtr + ')';
    end;
    aoPlusConstTimesMemSmallint:
    begin
      { Read a Shortint from code and a Smallint from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(ShortFactor, 1);
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Smallint));
      Address := Address + '+' + IntToStr(ShortFactor) + '*(' + OffsetPtr + ')';
    end;
    aoPlusConstTimesMemLongint:
    begin
      { Read a Shortint from code and a Longint from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(ShortFactor, 1);
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Longint));
      Address := Address + '+' + IntToStr(ShortFactor) + '*(' + OffsetPtr + ')';
    end;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Lit l'adresse d'une zone mémoire depuis le flux d'instructions
  @param Options     Options de lecture d'addresse
  @param ConstSize   Taille d'une constante dans le code
*}
function TSepiDisassembler.ReadAddress(Options: TSepiAddressOptions = [];
  ConstSize: Integer = 0): string;
var
  MemoryRef: TSepiMemoryRef;
  MemorySpace: TSepiMemorySpace;
  OpCount: Integer;
  I: Integer;
begin
  // Read memory reference
  Instructions.ReadBuffer(MemoryRef, SizeOf(TSepiMemoryRef));
  MemoryRefDecode(MemoryRef, MemorySpace, OpCount);

  // Read base address
  Result := ReadBaseAddress(Options, ConstSize, MemorySpace);

  // Check for nil result
  if Result = '' then
  begin
    if OpCount <> 0 then
      RaiseInvalidOpCode;
    Exit;
  end;

  // Handle operations
  for I := 0 to OpCount-1 do
    ReadAddressOperation(Result);
end;

{*
  Lit une valeur de type classe (TClass)
  @return Classe lue
*}
function TSepiDisassembler.ReadClassValue: string;
begin
  Result := ReadAddress([aoZeroAsNil]);

  if Result = '' then
    Result := ReadRef;
end;

{*
  Lit les paramètres et le résultat d'un CALL
  @return Paramètres et résultat
*}
function TSepiDisassembler.ReadParams: string;

const
  MaxParamCountForByteSize = 255 div 3; // 3 is max param size (Extended)
  MaxParamSize = 10;
  TrueParamSizes: array[0..10] of Integer = (
    4 {address}, 4, 4, 4, 4, 8, 8, 8, 8, 12, 12
  );

var
  ParamCount: Integer;
  ParamsSize: Integer;
  ParamIndex: Integer;
  ParamSize: TSepiParamSize;
  Params: string;
  ResultPtr: string;
begin
  // Read param count
  ParamCount := 0;
  Instructions.ReadBuffer(ParamCount, 1);

  // Read params size (in 4 bytes in code)
  ParamsSize := 0;
  if ParamCount <= MaxParamCountForByteSize then
    Instructions.ReadBuffer(ParamsSize, 1)
  else
    Instructions.ReadBuffer(ParamsSize, 2);
  ParamsSize := ParamsSize*4;

  // Read parameters
  Params := '';
  for ParamIndex := 0 to ParamCount-1 do
  begin
    // Read param size
    Instructions.ReadBuffer(ParamSize, 1);
    if ParamSize > MaxParamSize then
      RaiseInvalidOpCode;

    // Read parameter
    if ParamIndex > 0 then
      Params := Params + Comma;

    if ParamSize = psByAddress then
      Params := Params + '@' + ReadAddress {don't localize}
    else
      Params := Params + ReadAddress(aoAcceptAllConsts, ParamSize);
  end;

  // Read result
  ResultPtr := ReadAddress([aoZeroAsNil]);

  // Format result
  if Params <> '' then
    Params := '(' + Params + ')'; {don't localize}
  if ResultPtr <> '' then
    ResultPtr := ': ' + ResultPtr; {don't localize}

  Result := Params + ResultPtr;
end;

{*
  Désassemble une instruction
*}
function TSepiDisassembler.DisassembleInstruction: string;
var
  OpCode: TSepiOpCode;
  Name, Args: string;
begin
  // Read OpCode
  Instructions.ReadBuffer(OpCode, SizeOf(TSepiOpCode));

  // Get arguments and name
  Args := OpCodeArgsFuncs[OpCode](Self, OpCode);
  Name := OpCodeNames[OpCode];

  // Format result
  if Args = '' then
    Result := Name
  else
    Result := Format('%-8s%s', [Name, Args]); {don't localize}
end;

{*
  Désassemble un code
  Vous pouvez passer 0 comme paramètre CodeSize (resp. MaxInstructions) pour
  lever la limitation de taille de code (resp. de nombre d'instructions).
  En sortie, la liste de chaînes Result a une entrée supplémentaire pour chaque
  instruction désassemblée : la chaîne est une version textuelle de
  l'instructions, l'objet est l'adresse de début de l'instruction.
  @param Code              Pointeur sur le code à désassembler
  @param Result            Liste de chaînes où stocker le résultat
  @param RunUnit           Unité d'exécution
  @param CodeSize          Taille maximum du code à désassembler
  @param MaxInstructions   Nombre maximum d'instructions à désassembler
*}
procedure TSepiDisassembler.Disassemble(Code: Pointer; Result: TStrings;
  RunUnit: TSepiRuntimeUnit; CodeSize, MaxInstructions: Integer);
var
  MaxCode: Pointer;
  InstructionPos: Pointer;
begin
  // Read parameters
  if CodeSize <= 0 then
    MaxCode := Pointer($FFFFFFFF)
  else
    MaxCode := Pointer(Integer(Code) + CodeSize);

  if MaxInstructions <= 0 then
    MaxInstructions := MaxInt;

  RuntimeUnit := RunUnit;
  Instructions.PointerPos := Code;

  // Disassemble
  try
    while (Cardinal(Instructions.PointerPos) < Cardinal(MaxCode)) and
      (MaxInstructions > 0) do
    begin
      InstructionPos := Instructions.PointerPos;
      Result.AddObject(DisassembleInstruction, TObject(InstructionPos));
      Dec(MaxInstructions);
    end;
  except
    on Error: ESepiInvalidOpCode do;
  end;
end;

initialization
  InitOpCodeArgsFuncs;
end.

