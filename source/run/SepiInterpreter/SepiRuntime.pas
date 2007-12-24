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
  Interpréteur Sepi
  @author sjrd
  @version 1.0
*}
unit SepiRuntime;

interface

uses
  SysUtils, Classes, Contnrs, TypInfo, ScUtils, ScClasses, ScTypInfo,
  SepiReflectionCore, SepiMembers, SepiReflectionConsts, SepiOpCodes,
  SepiRuntimeOperations;

type
  TSepiRuntimeMethod = class;

  {*
    Unité d'exécution Sepi
    A sa création, une instance de TSepiRuntimeUnit crée une unité Sepi de type
    TSepiUnit. Ensuite, l'unité d'exécution se lie à la l'unité Sepi, de sorte
    qu'à la destruction de cette dernière, l'instance de TSepiRuntimeUnit sera
    détruite également.
    Ainsi, vous devez créer l'instance de TSepiRuntimeUnit, mais détruire
    l'instance de TSepiUnit.
    @author sjrd
    @version 1.0
  *}
  TSepiRuntimeUnit = class(TObject)
  private
    FMethods: TObjectList;           /// Méthodes
    FSepiUnit: TSepiUnit;            /// Unité Sepi
    FReferences: array of TSepiMeta; /// Références internes

    function FindMethod(const QName: string): TSepiRuntimeMethod;
    procedure GetMethodCode(Sender: TObject; var Code: Pointer;
      var CodeHandler: TObject);
  public
    constructor Create(SepiRoot: TSepiRoot; Stream: TStream); overload;
    constructor Create(SepiRoot: TSepiRoot;
      const FileName: TFileName); overload;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure ReadRef(Stream: TStream; out Ref);

    property SepiUnit: TSepiUnit read FSepiUnit;
  end;

  {*
    Méthode d'exécution Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiRuntimeMethod = class(TObject)
  private
    FFullName: string;              /// Nom complet
    FRuntimeUnit: TSepiRuntimeUnit; /// Unité d'exécution
    FSepiMethod: TSepiMethod;       /// Méthode Sepi correspondante

    FParamsSize: Integer; /// Taille des paramètres
    FLocalsSize: Integer; /// Taille des variables locales
    FCodeSize: Integer;   /// Taille du code
    FCode: Pointer;       /// Code

    FResultSize: Integer;       /// Taille du résultat
    FResultTypeInfo: PTypeInfo; /// RTTI du résultat

    FNativeCode: Pointer; /// Code natif

    procedure SetSepiMethod(AMethod: TSepiMethod);
  public
    constructor Create(AUnit: TSepiRuntimeUnit; Stream: TStream);
    destructor Destroy; override;

    procedure Invoke(Parameters: Pointer; Result: Pointer = nil);

    property FullName: string read FFullName;
    property RuntimeUnit: TSepiRuntimeUnit read FRuntimeUnit;
    property SepiMethod: TSepiMethod read FSepiMethod;

    property Code: Pointer read FCode;
    property NativeCode: Pointer read FNativeCode;
  end;

  {*
    Contexte d'exécution Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiRuntimeContext = class(TObject)
  private
    RuntimeUnit: TSepiRuntimeUnit;       /// Unité d'exécution
    Instructions: TAbsoluteMemoryStream; /// Instructions
    Parameters: Pointer;                 /// Paramètres
    Locals: Pointer;                     /// Variables locales
    PreparedParams: Pointer;             /// Paramètres en préparation
    PreparedParamsSize: Integer;         /// Taille des paramètres préparés

    // Op-code procs

    procedure UnknownOpCode(OpCode: TSepiOpCode);

    procedure OpCodeNope(OpCode: TSepiOpCode);
    procedure OpCodeJump(OpCode: TSepiOpCode);
    procedure OpCodeJumpIf(OpCode: TSepiOpCode);
    procedure OpCodePrepareParams(OpCode: TSepiOpCode);
    procedure OpCodeCall(OpCode: TSepiOpCode);

    procedure OpCodeLoadAddress(OpCode: TSepiOpCode);
    procedure OpCodeSimpleMove(OpCode: TSepiOpCode);
    procedure OpCodeMoveFixed(OpCode: TSepiOpCode);
    procedure OpCodeMoveOther(OpCode: TSepiOpCode);
    procedure OpCodeConvert(OpCode: TSepiOpCode);

    procedure OpCodeSelfUnaryOp(OpCode: TSepiOpCode);
    procedure OpCodeSelfBinaryOp(OpCode: TSepiOpCode);
    procedure OpCodeOtherUnaryOp(OpCode: TSepiOpCode);
    procedure OpCodeOtherBinaryOp(OpCode: TSepiOpCode);

    procedure OpCodeCompare(OpCode: TSepiOpCode);

    // Other methods

    function ReadAddress(ConstSize: Integer = 0): Pointer;
    procedure ReadJumpDest(out Value: Integer; out Origin: Word);
  public
    constructor Create(ARuntimeUnit: TSepiRuntimeUnit;
      AInstructions, AParameters, ALocals: Pointer);
    destructor Destroy; override;

    procedure Execute;
  end;

implementation

uses
  SepiInCalls, SepiOutCalls;

type
  TOpCodeProc = procedure(Self: TSepiRuntimeContext; OpCode: TSepiOpCode);

var
  OpCodeProcs: array[TSepiOpCode] of TOpCodeProc;

const
  BaseTypeConstSizes: array[TSepiBaseType] of Integer = (
    1, 1, 2, 4, 8, 1, 2, 4, 8, 4, 8, 10, 8, 8, 4, 4, 0
  );

{*
  Initialise les OpCodeProcs
*}
procedure InitOpCodeProcs;
var
  I: TSepiOpCode;
begin
  for I := $00 to $FF do
    @OpCodeProcs[I] := @TSepiRuntimeContext.UnknownOpCode;

  // No category
  @OpCodeProcs[ocNope]     := @TSepiRuntimeContext.OpCodeNope;
  @OpCodeProcs[ocExtended] := @TSepiRuntimeContext.UnknownOpCode;

  // Flow control
  @OpCodeProcs[ocJump]          := @TSepiRuntimeContext.OpCodeJump;
  @OpCodeProcs[ocJumpIfTrue]    := @TSepiRuntimeContext.OpCodeJumpIf;
  @OpCodeProcs[ocJumpIfFalse]   := @TSepiRuntimeContext.OpCodeJumpIf;
  @OpCodeProcs[ocReturn]        := @TSepiRuntimeContext.OpCodeNope;
  @OpCodeProcs[ocPrepareParams] := @TSepiRuntimeContext.OpCodePrepareParams;
  @OpCodeProcs[ocCall]          := @TSepiRuntimeContext.OpCodeCall;
  @OpCodeProcs[ocCallResult]    := @TSepiRuntimeContext.OpCodeCall;

  // Memory moves
  @OpCodeProcs[ocLoadAddress] := @TSepiRuntimeContext.OpCodeLoadAddress;
  for I := ocMoveByte to ocMoveIntf do
    @OpCodeProcs[I] := @TSepiRuntimeContext.OpCodeSimpleMove;
  @OpCodeProcs[ocMoveSome]  := @TSepiRuntimeContext.OpCodeMoveFixed;
  @OpCodeProcs[ocMoveMany]  := @TSepiRuntimeContext.OpCodeMoveFixed;
  @OpCodeProcs[ocMoveOther] := @TSepiRuntimeContext.OpCodeMoveOther;
  @OpCodeProcs[ocConvert]   := @TSepiRuntimeContext.OpCodeConvert;

  // Self dest unary operations
  for I := ocSelfInc to ocSelfNeg do
    @OpCodeProcs[I] := @TSepiRuntimeContext.OpCodeSelfUnaryOp;

  // Self dest binary operations
  for I := ocSelfAdd to ocSelfXor do
    @OpCodeProcs[I] := @TSepiRuntimeContext.OpCodeSelfBinaryOp;

  // Other dest unary operations
  for I := ocOtherInc to ocOtherNeg do
    @OpCodeProcs[I] := @TSepiRuntimeContext.OpCodeOtherUnaryOp;

  // Other dest binary operations
  for I := ocOtherAdd to ocOtherXor do
    @OpCodeProcs[I] := @TSepiRuntimeContext.OpCodeOtherBinaryOp;

  // Comparisons
  for I := ocCompEquals to ocCompGreaterEq do
    @OpCodeProcs[I] := @TSepiRuntimeContext.OpCodeCompare;
end;

{------------------------}
{ TSepiRuntimeUnit class }
{------------------------}

{*
  Charge une unité d'exécution depuis un flux
  @param SepiRoot   Racine Sepi
  @param Stream     Flux
*}
constructor TSepiRuntimeUnit.Create(SepiRoot: TSepiRoot; Stream: TStream);
var
  I, Count: Integer;
begin
  inherited Create;

  // Read methods
  FMethods := TObjectList.Create;
  Stream.ReadBuffer(Count, SizeOf(Integer));
  for I := 0 to Count-1 do
    FMethods.Add(TSepiRuntimeMethod.Create(Self, Stream));

  // Load Sepi unit
  FSepiUnit := TSepiUnit.LoadFromStream(SepiRoot, Stream, GetMethodCode);

  // Read references
  Stream.ReadBuffer(Count, SizeOf(Integer));
  SetLength(FReferences, Count);
  for I := 0 to Count-1 do
    FReferences[I] := SepiRoot.FindMeta(ReadStrFromStream(Stream));
end;

{*
  Charge une unité d'exécution depuis un fichier
  @param SepiRoot   Racine Sepi
  @param FileName   Nom du fichier
*}
constructor TSepiRuntimeUnit.Create(SepiRoot: TSepiRoot;
  const FileName: TFileName);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Create(SepiRoot, Stream);
  finally
    Stream.Free;
  end;
end;

{*
  [@inheritDoc]
*}
destructor TSepiRuntimeUnit.Destroy;
begin
  FMethods.Free;

  inherited;
end;

{*
  Cherche une méthode d'exécution depuis son nom
  @param QName   Nom complètement qualifié de la méthode
  @return Méthode d'exécution correspondante
  @throws ESepiMetaNotFoundError La méthode n'a pas été trouvée
*}
function TSepiRuntimeUnit.FindMethod(const QName: string): TSepiRuntimeMethod;
var
  I: Integer;
begin
  for I := 0 to FMethods.Count-1 do
  begin
    Result := TSepiRuntimeMethod(FMethods[I]);
    if AnsiSameText(Result.FullName, QName) then
      Exit;
  end;

  raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound, [QName]);
end;

{*
  Obtient le code d'une méthode
  @param Sender   Méthode Sepi dont on cherche le code
  @param Code     En sortie : code de la méthode
*}
procedure TSepiRuntimeUnit.GetMethodCode(Sender: TObject; var Code: Pointer;
  var CodeHandler: TObject);
var
  SepiMethod: TSepiMethod;
  Method: TSepiRuntimeMethod;
begin
  SepiMethod := Sender as TSepiMethod;
  Method := FindMethod(SepiMethod.GetFullName);

  Method.SetSepiMethod(SepiMethod);
  Code := Method.NativeCode;
  CodeHandler := Method;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRuntimeUnit.AfterConstruction;
begin
  inherited;

  SepiUnit.AddObjResource(Self);
end;

{*
  Lit une référence depuis un flux
  @param Stream   Flux
  @param Ref      En sortie : référence lue
*}
procedure TSepiRuntimeUnit.ReadRef(Stream: TStream; out Ref);
var
  Index: Integer;
begin
  Stream.ReadBuffer(Index, 4);
  TSepiMeta(Ref) := FReferences[Index];
end;

{--------------------------}
{ TSepiRuntimeMethod class }
{--------------------------}

{*
  Charge une méthode d'exécution depuis un flux
  @param Stream   Flux
*}
constructor TSepiRuntimeMethod.Create(AUnit: TSepiRuntimeUnit;
  Stream: TStream);
begin
  inherited Create;

  FFullName := ReadStrFromStream(Stream);
  FRuntimeUnit := AUnit;
  FSepiMethod := nil;

  Stream.ReadBuffer(FParamsSize, 4);
  Stream.ReadBuffer(FLocalsSize, 4);
  Stream.ReadBuffer(FCodeSize, 4);

  GetMem(FCode, FCodeSize);
  Stream.ReadBuffer(FCode^, FCodeSize);

  FResultSize := 0;
  FResultTypeInfo := nil;

  FNativeCode := nil;
end;

{*
  [@inheritDoc]
*}
destructor TSepiRuntimeMethod.Destroy;
begin
  if Assigned(FNativeCode) then
    FreeMem(FNativeCode);
  FreeMem(FCode);
  
  inherited;
end;

{*
  Renseigne la méthode Sepi correspondante
  @param AMethod   Méthode Sepi
*}
procedure TSepiRuntimeMethod.SetSepiMethod(AMethod: TSepiMethod);
var
  ResultType: TSepiType;
begin
  FSepiMethod := AMethod;

  // Update result information
  if FResultSize = 0 then
  begin
    ResultType := FSepiMethod.Signature.ReturnType;
    if (ResultType <> nil) and (ResultType.ResultBehavior <> rbParameter) then
    begin
      FResultSize := ResultType.Size;
      FResultTypeInfo := ResultType.TypeInfo;
    end;
  end;

  // Make native code
  with SepiMethod, Signature do
  begin
    FNativeCode := MakeInCallCode(Self, ReturnType, CallingConvention,
      SepiStackUsage, RegUsage);
  end;
end;

{*
  Invoque la méthode
  Le pointeur sur le résultat Result ne doit être renseigné que s'il s'agit
  bien d'une fonction, et que le résultat n'est pas transmis par adresse en
  tant que paramètre.
  @param Parameters   Pointeur sur les paramètres
  @param Result       Pointeur sur le résultat (défaut = nil)
*}
procedure TSepiRuntimeMethod.Invoke(Parameters: Pointer;
  Result: Pointer = nil);
var
  LocalsSize: Integer;
  Locals: Pointer;
  Context: TSepiRuntimeContext;
begin
  LocalsSize := FLocalsSize;

  asm
        SUB     ESP,LocalsSize
        MOV     Locals,ESP
  end;

  Context := TSepiRuntimeContext.Create(RuntimeUnit, Code, Parameters, Locals);
  try
    Context.Execute;
  finally
    Context.Free;
  end;

  if Assigned(Result) then
    CopyData(Locals^, Result^, FResultSize, FResultTypeInfo);

  asm
        ADD     ESP,LocalsSize
  end;
end;

{---------------------------}
{ TSepiRuntimeContext class }
{---------------------------}

{*
  Crée un contexte d'exécution Sepi
  @param ARuntimeUnit    Unité d'exécution
  @param AInstructions   Pointeur sur le code Sepi
  @param AParameters     Paramètres
  @param ALocals         Variables locales
*}
constructor TSepiRuntimeContext.Create(ARuntimeUnit: TSepiRuntimeUnit;
  AInstructions, AParameters, ALocals: Pointer);
begin
  inherited Create;

  RuntimeUnit := ARuntimeUnit;
  Instructions := TAbsoluteMemoryStream.Create(AInstructions);
  Parameters := AParameters;
  Locals := ALocals;
  PreparedParams := nil;
  PreparedParamsSize := 0;
end;

{*
  [@inheritDoc]
*}
destructor TSepiRuntimeContext.Destroy;
begin
  if Assigned(PreparedParams) then
    FreeMem(PreparedParams);
  Instructions.Free;

  inherited;
end;

{*
  OpCode inconnu
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.UnknownOpCode(OpCode: TSepiOpCode);
begin
  RaiseInvalidOpCode;
end;

{*
  OpCode Nope
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeNope(OpCode: TSepiOpCode);
begin
  // Do nothing
end;

{*
  OpCode Jump
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeJump(OpCode: TSepiOpCode);
var
  Offset: Integer;
  Origin: Word;
begin
  ReadJumpDest(Offset, Origin);
  Instructions.Seek(Offset, Origin);
end;

{*
  OpCode Jump If (True or False)
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeJumpIf(OpCode: TSepiOpCode);
var
  Offset: Integer;
  Origin: Word;
  TestPtr: PBoolean;
begin
  ReadJumpDest(Offset, Origin);
  TestPtr := ReadAddress(SizeOf(Boolean));

  case OpCode of
    ocJumpIfTrue:
      if TestPtr^ then
        Instructions.Seek(Offset, Origin);
    ocJumpIfFalse:
      if not TestPtr^ then
        Instructions.Seek(Offset, Origin);
  end;
end;

{*
  OpCode PrepareParams
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodePrepareParams(OpCode: TSepiOpCode);
var
  Size: Word;
begin
  if Assigned(PreparedParams) then
    RaiseInvalidOpCode;

  Instructions.ReadBuffer(Size, SizeOf(Word));
  GetMem(PreparedParams, Size);
  PreparedParamsSize := Size;
end;

{*
  OpCode Call
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeCall(OpCode: TSepiOpCode);
var
  SepiMethod: TSepiMethod;
  Result: Pointer;
  ResultBehavior: TSepiTypeResultBehavior;
  RuntimeMethod: TSepiRuntimeMethod;
begin
  // Read the instruction
  RuntimeUnit.ReadRef(Instructions, SepiMethod);
  if OpCode = ocCallResult then
    Result := ReadAddress
  else
    Result := nil;

  // Effective call
  if SepiMethod.CodeHandler is TSepiRuntimeMethod then
  begin
    RuntimeMethod := TSepiRuntimeMethod(SepiMethod.CodeHandler);
    RuntimeMethod.Invoke(PreparedParams, Result);
  end else
  begin
    with SepiMethod, Signature do
    begin
      if ReturnType = nil then
        ResultBehavior := rbParameter
      else
        ResultBehavior := ReturnType.ResultBehavior;

      SepiCallOut(Code, CallingConvention, PreparedParams, PreparedParamsSize,
        RegUsage, ResultBehavior, Result);
    end;
  end;

  // Release prepared parameters
  if Assigned(PreparedParams) then
  begin
    FreeMem(PreparedParams);
    PreparedParams := nil;
    PreparedParamsSize := 0;
  end;
end;

{*
  OpCode LoadAddress
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeLoadAddress(OpCode: TSepiOpCode);
var
  DestPtr: PPointer;
  SourcePtr: Pointer;
begin
  DestPtr := ReadAddress;
  SourcePtr := ReadAddress;

  DestPtr^ := SourcePtr;
end;

{*
  OpCode Move
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSimpleMove(OpCode: TSepiOpCode);
const
  ConstSizes: array[ocMoveByte..ocMoveIntf] of Integer = (
    1, 2, 4, 8, 10, 4, 4, 0, 4
  );
var
  DestPtr: Pointer;
  SourcePtr: Pointer;
begin
  DestPtr := ReadAddress;
  SourcePtr := ReadAddress(ConstSizes[OpCode]);

  case OpCode of
    ocMoveByte:    Shortint  (DestPtr^) := Shortint  (SourcePtr^);
    ocMoveWord:    Smallint  (DestPtr^) := Smallint  (SourcePtr^);
    ocMoveDWord:   Longint   (DestPtr^) := Longint   (SourcePtr^);
    ocMoveQWord:   Int64     (DestPtr^) := Int64     (SourcePtr^);
    ocMoveExt:     Extended  (DestPtr^) := Extended  (SourcePtr^);
    ocMoveAnsiStr: AnsiString(DestPtr^) := AnsiString(SourcePtr^);
    ocMoveWideStr: WideString(DestPtr^) := WideString(SourcePtr^);
    ocMoveVariant: Variant   (DestPtr^) := Variant   (SourcePtr^);
    ocMoveIntf:    IInterface(DestPtr^) := IInterface(SourcePtr^);
  end;
end;

{*
  OpCode MoveSome et MoveMany
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeMoveFixed(OpCode: TSepiOpCode);
var
  Count: Integer;
  DestPtr: Pointer;
  SourcePtr: Pointer;
begin
  Count := 0;
  if OpCode = ocMoveSome then
    Instructions.ReadBuffer(Count, 1)
  else
    Instructions.ReadBuffer(Count, 2);

  DestPtr := ReadAddress;
  SourcePtr := ReadAddress(Count);

  Move(SourcePtr^, DestPtr^, Count);
end;

{*
  OpCode MoveOther
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeMoveOther(OpCode: TSepiOpCode);
var
  SepiType: TSepiType;
  DestPtr: Pointer;
  SourcePtr: Pointer;
begin
  RuntimeUnit.ReadRef(Instructions, SepiType);

  DestPtr := ReadAddress;
  SourcePtr := ReadAddress(SepiType.Size);

  CopyData(SourcePtr^, DestPtr^, SepiType.Size, SepiType.TypeInfo);
end;

{*
  OpCode Convert
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeConvert(OpCode: TSepiOpCode);
var
  ToType, FromType: TSepiBaseType;
  DestPtr, SourcePtr: Pointer;
begin
  Instructions.ReadBuffer(ToType, SizeOf(TSepiBaseType));
  Instructions.ReadBuffer(FromType, SizeOf(TSepiBaseType));
  DestPtr := ReadAddress;
  SourcePtr := ReadAddress(BaseTypeConstSizes[FromType]);

  Convert(ToType, FromType, DestPtr^, SourcePtr^);
end;

{*
  Opérations unaires sur soi-même
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSelfUnaryOp(OpCode: TSepiOpCode);
var
  VarType: TSepiBaseType;
  VarPtr: Pointer;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  VarPtr := ReadAddress;

  UnaryOp(OpCode, VarType, VarPtr^, VarPtr^);
end;

{*
  Opérations binaires sur soi-même
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSelfBinaryOp(OpCode: TSepiOpCode);
var
  VarType: TSepiBaseType;
  VarPtr, ValuePtr: Pointer;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  VarPtr := ReadAddress;
  if OpCode in [ocSelfShl, ocSelfShr, ocSelfSar] then
    ValuePtr := ReadAddress(1)
  else
    ValuePtr := ReadAddress(BaseTypeConstSizes[VarType]);

  BinaryOp(OpCode, VarType, VarPtr^, VarPtr^, ValuePtr^);
end;

{*
  Opérations unaires sur un autre
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeOtherUnaryOp(OpCode: TSepiOpCode);
var
  VarType: TSepiBaseType;
  DestPtr, ValuePtr: Pointer;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  DestPtr := ReadAddress;
  ValuePtr := ReadAddress(BaseTypeConstSizes[VarType]);

  UnaryOp(OpCode, VarType, DestPtr^, ValuePtr^);
end;

{*
  Opérations binaires sur un autre
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeOtherBinaryOp(OpCode: TSepiOpCode);
var
  VarType: TSepiBaseType;
  DestPtr, LeftPtr, RightPtr: Pointer;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  DestPtr := ReadAddress;
  LeftPtr := ReadAddress(BaseTypeConstSizes[VarType]);
  if OpCode in [ocOtherShl, ocOtherShr, ocOtherSar] then
    RightPtr := ReadAddress(1)
  else
    RightPtr := ReadAddress(BaseTypeConstSizes[VarType]);

  BinaryOp(OpCode, VarType, DestPtr^, LeftPtr^, RightPtr^);
end;

{*
  OpCode de comparaison
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeCompare(OpCode: TSepiOpCode);
var
  VarType: TSepiBaseType;
  DestPtr, LeftPtr, RightPtr: Pointer;
begin
  Instructions.ReadBuffer(VarType, SizeOf(TSepiBaseType));
  DestPtr := ReadAddress;
  LeftPtr := ReadAddress(BaseTypeConstSizes[VarType]);
  RightPtr := ReadAddress(BaseTypeConstSizes[VarType]);

  Compare(OpCode, VarType, DestPtr^, LeftPtr^, RightPtr^);
end;

{*
  Lit l'adresse d'une zone mémoire depuis le flux d'instructions
  @param ConstSize   Taille d'une constante (0 n'accepte pas les constantes)
*}
function TSepiRuntimeContext.ReadAddress(ConstSize: Integer = 0): Pointer;
var
  MemoryRef: TSepiMemoryRef;
  MemPlace: TSepiMemoryPlace;
  DerefKind: TSepiDereferenceKind;
  ByteOffset: Shortint;
  WordOffset: Shortint;
  LongOffset: Longint;
  GlobalVar: TSepiVariable;
begin
  Result := nil;

  // Read address space
  Instructions.ReadBuffer(MemoryRef, SizeOf(TSepiMemoryRef));
  MemPlace := TSepiMemoryPlace(MemoryRef and MemPlaceMask);
  DerefKind := TSepiDereferenceKind(
    (MemoryRef and MemDerefMask) shr MemDerefShift);

  // Read address
  case MemPlace of
    mpConstant:
    begin
      if (ConstSize <= 0) or (DerefKind <> dkNone) then
        RaiseInvalidOpCode;
      Result := Pointer(Integer(Instructions.Position));
      Instructions.Seek(ConstSize, soFromCurrent);
    end;
    mpLocalsByte:
    begin
      Instructions.ReadBuffer(ByteOffset, 1);
      Result := Locals;
      Inc(Integer(Result), ByteOffset);
    end;
    mpLocalsWord:
    begin
      Instructions.ReadBuffer(WordOffset, 2);
      Result := Locals;
      Inc(Integer(Result), WordOffset);
    end;
    mpParamsByte:
    begin
      Instructions.ReadBuffer(ByteOffset, 1);
      Result := Parameters;
      Inc(Integer(Result), ByteOffset);
    end;
    mpParamsWord:
    begin
      Instructions.ReadBuffer(WordOffset, 2);
      Result := Parameters;
      Inc(Integer(Result), WordOffset);
    end;
    mpPreparedParamsByte:
    begin
      Instructions.ReadBuffer(ByteOffset, 1);
      Result := PreparedParams;
      Inc(Integer(Result), ByteOffset);
    end;
    mpPreparedParamsWord:
    begin
      Instructions.ReadBuffer(WordOffset, 2);
      Result := PreparedParams;
      Inc(Integer(Result), WordOffset);
    end;
    mpGlobalVar:
    begin
      RuntimeUnit.ReadRef(Instructions, GlobalVar);
      Result := GlobalVar.Value;
    end;
  else
    RaiseInvalidOpCode;
  end;

  // Handle dereference kind
  if DerefKind <> dkNone then
  begin
    Result := PPointer(Result)^;

    case DerefKind of
      dkPlusShortint:
      begin
        Instructions.ReadBuffer(ByteOffset, 1);
        Inc(Integer(Result), ByteOffset);
      end;
      dkPlusSmallint:
      begin
        Instructions.ReadBuffer(WordOffset, 2);
        Inc(Integer(Result), WordOffset);
      end;
      dkPlusLongint:
      begin
        Instructions.ReadBuffer(LongOffset, 4);
        Inc(Integer(Result), LongOffset);
      end;
      dkDouble:
      begin
        Result := PPointer(Result)^;
      end;
    end;
  end;
end;

{*
  Lit une destination de Jump
  @param Value    En sortie : valeur de déplacement
  @param Origin   En sortie : orgine du déplacement (voir TStream.Seek)
*}
procedure TSepiRuntimeContext.ReadJumpDest(out Value: Integer;
  out Origin: Word);
var
  DestKind: TSepiJumpDestKind;
  ByteOffset: Shortint;
  WordOffset: Smallint;
  ValuePtr: PLongint;
begin
  Instructions.ReadBuffer(DestKind, SizeOf(TSepiJumpDestKind));

  case DestKind of
    jdkShortint:
    begin
      Instructions.ReadBuffer(ByteOffset, 1);
      Value := ByteOffset;
      Origin := soFromCurrent;
    end;
    jdkSmallint:
    begin
      Instructions.ReadBuffer(WordOffset, 2);
      Value := WordOffset;
      Origin := soFromCurrent;
    end;
    jdkLongint:
    begin
      Instructions.ReadBuffer(Value, 4);
      Origin := soFromCurrent;
    end;
    jdkMemory:
    begin
      ValuePtr := ReadAddress(SizeOf(Pointer));
      Value := ValuePtr^;
      Origin := soFromBeginning;
    end;
  end;
end;

{*
  Exécute les instructions jusqu'à un RET
*}
procedure TSepiRuntimeContext.Execute;
var
  OpCode: TSepiOpCode;
begin
  repeat
    Instructions.ReadBuffer(OpCode, SizeOf(TSepiOpCode));
    OpCodeProcs[OpCode](Self, OpCode);
  until OpCode = ocReturn;
end;

initialization
  InitOpCodeProcs;
end.

