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
  Interpréteur Sepi
  @author sjrd
  @version 1.0
*}
unit SepiRuntime;

interface

uses
  Windows, Types, SysUtils, Classes, Contnrs, TypInfo, ScUtils, ScStrUtils,
  ScClasses, ScTypInfo, ScLowLevel, ScCompilerMagic, SepiReflectionCore,
  SepiMembers, SepiReflectionConsts, SepiOpCodes, SepiRuntimeOperations;

type
  TSepiRuntimeMethod = class;
  TSepiRuntimeContext = class;

  {*
    Type d'un événement déclenché pour le débogage d'un code Sepi
    @param Context   Contexte d'exécution du débogage
  *}
  TSepiDebugEvent = procedure(Context: TSepiRuntimeContext) of object;

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
    FMethods: TObjectList;                /// Méthodes
    FSepiUnit: TSepiUnit;                 /// Unité Sepi
    FReferences: array of TSepiComponent; /// Références internes

    FOnDebug: TSepiDebugEvent; /// Evénément de débogage

    function FindMethod(const QName: string): TSepiRuntimeMethod;
    procedure GetMethodCode(Sender: TObject; var Code: Pointer;
      var CodeHandler: TObject);

    function GetMethodCount: Integer;
    function GetMethods(Index: Integer): TSepiRuntimeMethod;
  public
    constructor Create(SepiRoot: TSepiRoot; Stream: TStream); overload;
    constructor Create(SepiRoot: TSepiRoot;
      const FileName: TFileName); overload;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure ReadRef(Stream: TStream; out Ref);

    property SepiUnit: TSepiUnit read FSepiUnit;
    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TSepiRuntimeMethod read GetMethods;

    property OnDebug: TSepiDebugEvent read FOnDebug write FOnDebug;
  end;

  {*
    Informations de recopiage d'un paramètre local
    @author sjrd
    @version 1.0
  *}
  TLocalParam = record
    StackOffset: Integer; /// Offset dans la pile
    LocalOffset: Integer; /// Offset dans les paramètres locaux
    Size: Integer;        /// Taille du paramètre
  end;

  /// Informations de recopiage des paramètres locaux
  TLocalParams = array of TLocalParam;

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

    FParamsAddRef: PTypeInfo;    /// Infos sur les paramètres à référencer
    FLocalParamsSize: Integer;   /// Taille des paramètres recopiés en local
    FLocalParams: TLocalParams;  /// Infos de recopiage local des paramètres
    FLocalParamsInfo: PTypeInfo; /// Infos sur les paramètres recopiés en local

    FResultSize: Integer; /// Taille du résultat

    FLocalsInfo: PTypeInfo; /// Informations sur les variables locales

    FNativeCode: Pointer; /// Code natif

    procedure MakeParamsAddRef;
    procedure MakeLocalParams;
    procedure SetSepiMethod(AMethod: TSepiMethod);
    procedure ReadLocalsInfo(Stream: TStream);
  public
    constructor Create(AUnit: TSepiRuntimeUnit; Stream: TStream);
    destructor Destroy; override;

    procedure Invoke(Parameters: Pointer; Result: Pointer = nil);

    property FullName: string read FFullName;
    property RuntimeUnit: TSepiRuntimeUnit read FRuntimeUnit;
    property SepiMethod: TSepiMethod read FSepiMethod;

    property Code: Pointer read FCode;
    property CodeSize: Integer read FCodeSize;
    property NativeCode: Pointer read FNativeCode;

    property ParamsSize: Integer read FParamsSize;
    property LocalsSize: Integer read FLocalsSize;
  end;

  {*
    Contexte d'exécution Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiRuntimeContext = class(TObject)
  private
    FRuntimeMethod: TSepiRuntimeMethod; /// Méthode d'exécution
    FRuntimeUnit: TSepiRuntimeUnit;     /// Unité d'exécution

    FInstructions: TAbsoluteMemoryStream; /// Instructions

    FParameters: Pointer; /// Paramètres
    FLocals: Pointer;     /// Variables locales

    // Op-code procs

    procedure UnknownOpCode(OpCode: TSepiOpCode);

    procedure OpCodeNope(OpCode: TSepiOpCode);
    procedure OpCodeJump(OpCode: TSepiOpCode);
    procedure OpCodeJumpIf(OpCode: TSepiOpCode);

    procedure OpCodeAddressCall(OpCode: TSepiOpCode);
    procedure OpCodeStaticCall(OpCode: TSepiOpCode);
    procedure OpCodeDynamicCall(OpCode: TSepiOpCode);

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

    procedure OpCodeGetTypeInfo(OpCode: TSepiOpCode);
    procedure OpCodeGetDelphiClass(OpCode: TSepiOpCode);
    procedure OpCodeGetMethodCode(OpCode: TSepiOpCode);

    procedure OpCodeIsClass(OpCode: TSepiOpCode);
    procedure OpCodeAsClass(OpCode: TSepiOpCode);

    procedure OpCodeRaise(OpCode: TSepiOpCode);
    procedure OpCodeReraise(OpCode: TSepiOpCode);
    procedure OpCodeTryExcept(OpCode: TSepiOpCode);
    procedure OpCodeTryFinally(OpCode: TSepiOpCode);
    procedure OpCodeMultiOn(OpCode: TSepiOpCode);

    procedure OpCodeSetIncludeExclude(OpCode: TSepiOpCode);
    procedure OpCodeSetIn(OpCode: TSepiOpCode);
    procedure OpCodeSetElem(OpCode: TSepiOpCode);
    procedure OpCodeSetRange(OpCode: TSepiOpCode);
    procedure OpCodeSetCmpOp(OpCode: TSepiOpCode);
    procedure OpCodeSetSelfOp(OpCode: TSepiOpCode);
    procedure OpCodeSetOtherOp(OpCode: TSepiOpCode);
    procedure OpCodeSetExpand(OpCode: TSepiOpCode);

    procedure OpCodeValueToIntStdFunction(OpCode: TSepiOpCode);
    procedure OpCodeStrSetLength(OpCode: TSepiOpCode);
    procedure OpCodeDynArraySetLength(OpCode: TSepiOpCode);
    procedure OpCodeStrCopy(OpCode: TSepiOpCode);
    procedure OpCodeDynArrayCopy(OpCode: TSepiOpCode);
    procedure OpCodeDynArrayCopyRange(OpCode: TSepiOpCode);

    procedure OpCodeRoutineRefFromMethodRef(OpCode: TSepiOpCode);
    procedure OpCodeIntfFromClass(OpCode: TSepiOpCode);

    // Other methods

    function ReadBaseAddress(Options: TSepiAddressOptions; ConstSize: Integer;
      MemorySpace: TSepiMemorySpace): Pointer;
    procedure ReadAddressOperation(var Address: Pointer);
    function ReadAddress(Options: TSepiAddressOptions = [];
      ConstSize: Integer = 0): Pointer;
    function ReadClassValue: TClass;
    procedure ReadParamsAndCall(Address: Pointer;
      CallingConvention: TCallingConvention; RegUsage: Byte;
      ResultBehavior: TSepiTypeResultBehavior; OrdResultSize: Byte);

    // Access methods

    function GetNextInstruction: Pointer;

    // Properties

    property Instructions: TAbsoluteMemoryStream read FInstructions;
  public
    constructor Create(ARuntimeMethod: TSepiRuntimeMethod;
      AInstructions: TAbsoluteMemoryStream; AParameters, ALocals: Pointer);
    { If you add a destructor to this class, please read the big comment in
      TSepiRuntimeMethod.Invoke! }

    procedure Execute(BlockBegin, BlockEnd: Pointer);

    property RuntimeMethod: TSepiRuntimeMethod read FRuntimeMethod;
    property RuntimeUnit: TSepiRuntimeUnit read FRuntimeUnit;

    property NextInstruction: Pointer read GetNextInstruction;

    property Parameters: Pointer read FParameters;
    property Locals: Pointer read FLocals;
  end;

implementation

uses
  SepiInCalls;

type
  /// Méthode de traitement d'un OpCode
  TOpCodeProc = procedure(Self: TSepiRuntimeContext; OpCode: TSepiOpCode);

  /// Set le plus grand possible
  TSet = set of Byte;

  /// Type chaîne Unicode, si existant dans cette version de Delphi
  UnicodeStrDef = string;

var
  /// Tableau des méthodes de traitement des OpCodes
  OpCodeProcs: array[TSepiOpCode] of TOpCodeProc;

const
  /// Zero memory, big enough to accept a Variant
  ZeroMemory: array[0..3] of Integer = (0, 0, 0, 0);

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
  @OpCodeProcs[ocJump]        := @TSepiRuntimeContext.OpCodeJump;
  @OpCodeProcs[ocJumpIfTrue]  := @TSepiRuntimeContext.OpCodeJumpIf;
  @OpCodeProcs[ocJumpIfFalse] := @TSepiRuntimeContext.OpCodeJumpIf;

  // Calls
  @OpCodeProcs[ocAddressCall] := @TSepiRuntimeContext.OpCodeAddressCall;
  @OpCodeProcs[ocStaticCall]  := @TSepiRuntimeContext.OpCodeStaticCall;
  @OpCodeProcs[ocDynamicCall] := @TSepiRuntimeContext.OpCodeDynamicCall;

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

  // Compile time objects which must be read at runtime in Sepi
  @OpCodeProcs[ocGetTypeInfo]    := @TSepiRuntimeContext.OpCodeGetTypeInfo;
  @OpCodeProcs[ocGetDelphiClass] := @TSepiRuntimeContext.OpCodeGetDelphiClass;
  @OpCodeProcs[ocGetMethodCode]  := @TSepiRuntimeContext.OpCodeGetMethodCode;

  // is and as operators
  @OpCodeProcs[ocIsClass] := @TSepiRuntimeContext.OpCodeIsClass;
  @OpCodeProcs[ocAsClass] := @TSepiRuntimeContext.OpCodeAsClass;

  // Exception handling
  @OpCodeProcs[ocRaise]      := @TSepiRuntimeContext.OpCodeRaise;
  @OpCodeProcs[ocReraise]    := @TSepiRuntimeContext.OpCodeReraise;
  @OpCodeProcs[ocTryExcept]  := @TSepiRuntimeContext.OpCodeTryExcept;
  @OpCodeProcs[ocTryFinally] := @TSepiRuntimeContext.OpCodeTryFinally;
  @OpCodeProcs[ocMultiOn]    := @TSepiRuntimeContext.OpCodeMultiOn;

  // Set operations
  @OpCodeProcs[ocSetInclude] := @TSepiRuntimeContext.OpCodeSetIncludeExclude;
  @OpCodeProcs[ocSetExclude] := @TSepiRuntimeContext.OpCodeSetIncludeExclude;
  @OpCodeProcs[ocSetIn]             := @TSepiRuntimeContext.OpCodeSetIn;
  @OpCodeProcs[ocSetElem]           := @TSepiRuntimeContext.OpCodeSetElem;
  @OpCodeProcs[ocSetRange]          := @TSepiRuntimeContext.OpCodeSetRange;
  @OpCodeProcs[ocSetUnionRange]     := @TSepiRuntimeContext.OpCodeSetRange;
  @OpCodeProcs[ocSetEquals]         := @TSepiRuntimeContext.OpCodeSetCmpOp;
  @OpCodeProcs[ocSetNotEquals]      := @TSepiRuntimeContext.OpCodeSetCmpOp;
  @OpCodeProcs[ocSetContained]      := @TSepiRuntimeContext.OpCodeSetCmpOp;
  @OpCodeProcs[ocSetSelfIntersect]  := @TSepiRuntimeContext.OpCodeSetSelfOp;
  @OpCodeProcs[ocSetSelfUnion]      := @TSepiRuntimeContext.OpCodeSetSelfOp;
  @OpCodeProcs[ocSetSelfSubtract]   := @TSepiRuntimeContext.OpCodeSetSelfOp;
  @OpCodeProcs[ocSetOtherIntersect] := @TSepiRuntimeContext.OpCodeSetOtherOp;
  @OpCodeProcs[ocSetOtherUnion]     := @TSepiRuntimeContext.OpCodeSetOtherOp;
  @OpCodeProcs[ocSetOtherSubtract]  := @TSepiRuntimeContext.OpCodeSetOtherOp;
  @OpCodeProcs[ocSetExpand]         := @TSepiRuntimeContext.OpCodeSetExpand;

  // Standard Delphi functions
  @OpCodeProcs[ocAnsiStrLength] :=
    @TSepiRuntimeContext.OpCodeValueToIntStdFunction;
  @OpCodeProcs[ocWideStrLength] :=
    @TSepiRuntimeContext.OpCodeValueToIntStdFunction;
  @OpCodeProcs[ocUnicodeStrLength] :=
    @TSepiRuntimeContext.OpCodeValueToIntStdFunction;
  @OpCodeProcs[ocDynArrayLength] :=
    @TSepiRuntimeContext.OpCodeValueToIntStdFunction;
  @OpCodeProcs[ocDynArrayHigh] :=
    @TSepiRuntimeContext.OpCodeValueToIntStdFunction;
  @OpCodeProcs[ocAnsiStrSetLength] :=
    @TSepiRuntimeContext.OpCodeStrSetLength;
  @OpCodeProcs[ocWideStrSetLength] :=
    @TSepiRuntimeContext.OpCodeStrSetLength;
  @OpCodeProcs[ocUnicodeStrSetLength] :=
    @TSepiRuntimeContext.OpCodeStrSetLength;
  @OpCodeProcs[ocDynArraySetLength] :=
    @TSepiRuntimeContext.OpCodeDynArraySetLength;
  @OpCodeProcs[ocAnsiStrCopy] :=
    @TSepiRuntimeContext.OpCodeStrCopy;
  @OpCodeProcs[ocWideStrCopy] :=
    @TSepiRuntimeContext.OpCodeStrCopy;
  @OpCodeProcs[ocUnicodeStrCopy] :=
    @TSepiRuntimeContext.OpCodeStrCopy;
  @OpCodeProcs[ocDynArrayCopy] :=
    @TSepiRuntimeContext.OpCodeDynArrayCopy;
  @OpCodeProcs[ocDynArrayCopyRange] :=
    @TSepiRuntimeContext.OpCodeDynArrayCopyRange;

  // Miscellaneous instructions
  @OpCodeProcs[ocRoutineRefFromMethodRef] :=
    @TSepiRuntimeContext.OpCodeRoutineRefFromMethodRef;
  @OpCodeProcs[ocIntfFromClass] := @TSepiRuntimeContext.OpCodeIntfFromClass;
end;

{*
  Alloue les RTTI d'un type record anonyme
  @param FieldCount   Nombre de champ requérant une initialisation
  @return Pointeur sur les RTTI du type
*}
function AllocRecordTypeInfo(FieldCount: Integer): PTypeInfo;
var
  Size: Integer;
begin
  Size := 2 + SizeOf(TRecordTypeData) + (FieldCount-1)*SizeOf(TManagedField);
  GetMem(Result, Size);
  PWord(Result)^ := Word(tkRecord); // second byte is name length (so 0)
end;

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
  Digest: TSepiDigest;
begin
  inherited Create;

  // Read methods
  FMethods := TObjectList.Create;
  Stream.ReadBuffer(Count, SizeOf(Integer));
  FMethods.Capacity := Count;
  for I := 0 to Count-1 do
    FMethods.Add(TSepiRuntimeMethod.Create(Self, Stream));

  // Load Sepi unit
  FSepiUnit := TSepiUnit.LoadFromStream(SepiRoot, Stream, False, GetMethodCode);

  // Read references
  Stream.ReadBuffer(Count, SizeOf(Integer));
  SetLength(FReferences, Count);
  for I := 0 to Count-1 do
  begin
    FReferences[I] := SepiRoot.FindComponent(ReadStrFromStream(Stream));
    Stream.ReadBuffer(Digest, SizeOf(TSepiDigest));

    if not FReferences[I].CheckDigest(Digest) then
      raise ESepiIncompatibleUsedUnitError.CreateFmt(
        SSepiIncompatibleUsedUnitIncompatibleComponent,
        [SepiUnit.Name, FReferences[I].OwningUnit.Name, FReferences[I].Name]);
  end;

  // Read locals information
  for I := 0 to FMethods.Count-1 do
    TSepiRuntimeMethod(FMethods[I]).ReadLocalsInfo(Stream);
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
  @throws ESepiComponentNotFoundError La méthode n'a pas été trouvée
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

  raise ESepiComponentNotFoundError.CreateFmt(SSepiComponentNotFound, [QName]);
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
  UnitName, MethodName: string;
  Method: TSepiRuntimeMethod;
begin
  SepiMethod := Sender as TSepiMethod;
  if not SplitToken(SepiMethod.GetFullName, '.', UnitName, MethodName) then
    MethodName := UnitName;
  Method := FindMethod(MethodName);

  Method.SetSepiMethod(SepiMethod);
  Code := Method.NativeCode;
  CodeHandler := Method;
end;

{*
  Nombre de méthodes
  @return Nombre de méthodes
*}
function TSepiRuntimeUnit.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
end;

{*
  Tableau zero-based des méthodes
  @param Index   Index d'une méthode
  @return Méthode à l'index spécifié
*}
function TSepiRuntimeUnit.GetMethods(Index: Integer): TSepiRuntimeMethod;
begin
  Result := TSepiRuntimeMethod(FMethods[Index]);
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
  TSepiComponent(Ref) := FReferences[Index];
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

  Stream.ReadBuffer(FParamsSize, 4);
  Stream.ReadBuffer(FLocalsSize, 4);
  Stream.ReadBuffer(FCodeSize, 4);

  GetMem(FCode, FCodeSize);
  Stream.ReadBuffer(FCode^, FCodeSize);
end;

{*
  [@inheritDoc]
*}
destructor TSepiRuntimeMethod.Destroy;
begin
  if Assigned(FNativeCode) then
    FreeMem(FNativeCode);
  if Assigned(FLocalsInfo) then
    FreeMem(FLocalsInfo);
  if Assigned(FLocalParamsInfo) then
    FreeMem(FLocalParamsInfo);
  if Assigned(FParamsAddRef) then
    FreeMem(FParamsAddRef);
  FreeMem(FCode);

  inherited;
end;

{*
  Construit les informations de référencement des paramètres
*}
procedure TSepiRuntimeMethod.MakeParamsAddRef;
var
  Signature: TSepiSignature;
  I, Count: Integer;
  AddRefInfo: array of TManagedField;
begin
  Signature := SepiMethod.Signature;

  // Register parameters which need add ref

  SetLength(AddRefInfo, Signature.ActualParamCount);
  Count := 0;

  for I := Signature.ActualParamCount-1 downto 0 do
  begin
    with Signature.ActualParams[I] do
    begin
      { An "add ref" is required if the three following conditions meet:
        - The parameter is a value parameter (not const/var/out) ;
        - It is transmitted by value (and not by address, e.g. big records) ;
        - Its type requires initialization. }
      if (Kind = pkValue) and (not CallInfo.ByAddress) and
        ParamType.IsManaged then
      begin
        with AddRefInfo[Count] do
        begin
          TypeRef := ParamType.TypeInfoRef;
          FldOffset := CallInfo.SepiStackOffset;
        end;

        Inc(Count);
      end;
    end;
  end;

  // If Count = 0, don't allocate anything

  if Count = 0 then
    Exit;

  // Allocate and fill in type information

  FParamsAddRef := AllocRecordTypeInfo(Count);
  with PRecordTypeData(Integer(FParamsAddRef)+2)^ do
  begin
    Size := FParamsSize;
    ManagedCount := Count;
    Move(AddRefInfo[0], ManagedFields[0], Count * SizeOf(TManagedField));
  end;
end;

{*
  Construit les informations de recopie locale des paramètres
*}
procedure TSepiRuntimeMethod.MakeLocalParams;
var
  Signature: TSepiSignature;
  I, Count: Integer;
  LocalParamsInfo: array of TManagedField;
begin
  Signature := SepiMethod.Signature;

  // Register parameters which need copying

  SetLength(LocalParamsInfo, Signature.ActualParamCount);
  Count := 0;
  FLocalParamsSize := 0;
  SetLength(FLocalParams, Signature.ActualParamCount);

  for I := Signature.ActualParamCount-1 downto 0 do
  begin
    with Signature.ActualParams[I] do
    begin
      { Copying is required if the three following conditions meet:
        - The parameter is a value parameter (not const/var/out);
        - It is not an open array;
        - It is transmitted by address (e.g. big records). }
      if (Kind = pkValue) and (not OpenArray) and CallInfo.ByAddress then
      begin
        with FLocalParams[Count] do
        begin
          StackOffset := CallInfo.SepiStackOffset;
          LocalOffset := FLocalParamsSize;
          Size := ParamType.Size;
        end;

        with LocalParamsInfo[Count] do
        begin
          TypeRef := ParamType.TypeInfoRef;
          FldOffset := FLocalParamsSize;
        end;

        Inc(FLocalParamsSize, ParamType.Size);
        Inc(Count);
      end;
    end;
  end;

  // Pack FLocalParams array

  SetLength(FLocalParams, Count);

  // If Count = 0, don't allocate anything

  if Count = 0 then
    Exit;

  // Allocate and fill in type information

  FLocalParamsInfo := AllocRecordTypeInfo(Count);
  with PRecordTypeData(Integer(FLocalParamsInfo)+2)^ do
  begin
    Size := FLocalParamsSize;
    ManagedCount := Count;
    Move(LocalParamsInfo[0], ManagedFields[0], Count * SizeOf(TManagedField));
  end;
end;

{*
  Renseigne la méthode Sepi correspondante
  @param AMethod   Méthode Sepi
*}
procedure TSepiRuntimeMethod.SetSepiMethod(AMethod: TSepiMethod);
var
  Signature: TSepiSignature;
  ResultType: TSepiType;
begin
  FSepiMethod := AMethod;
  Signature := FSepiMethod.Signature;

  // Set up local params information
  MakeParamsAddRef;
  MakeLocalParams;

  // Set up result information
  ResultType := Signature.ReturnType;
  if not (ResultType.SafeResultBehavior in [rbNone, rbParameter]) then
    FResultSize := ResultType.Size;

  // Make native code
  with Signature do
  begin
    FNativeCode := MakeInCallCode(Self, ReturnType, CallingConvention,
      SepiStackUsage, RegUsage);
  end;
end;

{*
  Lit les informations d'init/finit des variables locales depuis un flux
  @param Stream   Flux source
*}
procedure TSepiRuntimeMethod.ReadLocalsInfo(Stream: TStream);
var
  Count, I: Integer;
  SepiType: TSepiType;
begin
  // Read count
  Stream.ReadBuffer(Count, SizeOf(Integer));
  if Count = 0 then
    Exit;

  // Allocate type information
  FLocalsInfo := AllocRecordTypeInfo(Count);

  // Fill type data
  with PRecordTypeData(Integer(FLocalsInfo)+2)^ do
  begin
    Size := FLocalsSize;
    ManagedCount := Count;

    for I := 0 to Count-1 do
    begin
      RuntimeUnit.ReadRef(Stream, SepiType);
      ManagedFields[I].TypeRef := SepiType.TypeInfoRef;
      Stream.ReadBuffer(ManagedFields[I].FldOffset, SizeOf(Integer));
    end;
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
  ContextSize, InstructionsSize: Integer;
  Context: TSepiRuntimeContext;
  Instructions: TAbsoluteMemoryStream;
  AllLocalsSize, I: Integer;
  Locals, LocalParams: Pointer;
  ParamsPPtr: PPointer;
  LocalsPtr: Pointer;
begin
  // Fetch context and instructions instance size
  ContextSize := TSepiRuntimeContext.InstanceSize;
  InstructionsSize := TAbsoluteMemoryStream.InstanceSize;

  // Allocate locals on stack
  AllLocalsSize := ContextSize + InstructionsSize +
    FLocalsSize + FLocalParamsSize;
  asm
        SUB     ESP,AllLocalsSize
        MOV     Locals,ESP
  end;
  LocalParams := Pointer(Integer(Locals) + FLocalsSize);
  Pointer(Context) := Pointer(Integer(LocalParams) + FLocalParamsSize);
  Pointer(Instructions) := Pointer(Integer(Context) + ContextSize);

  // Initialize local variables
  if Assigned(FLocalsInfo) then
    Initialize(Locals^, FLocalsInfo);

  // Add references to parameters
  if Assigned(FParamsAddRef) then
    AddRef(Parameters^, FParamsAddRef);

  // Copy local parameters
  if FLocalParamsSize <> 0 then
  begin
    // Copy data and change references
    for I := Length(FLocalParams)-1 downto 0 do
    begin
      with FLocalParams[I] do
      begin
        ParamsPPtr := PPointer(Integer(Parameters) + StackOffset);
        LocalsPtr := Pointer(Integer(LocalParams) + LocalOffset);
        Move(ParamsPPtr^^, LocalsPtr^, Size);
        ParamsPPtr^ := LocalsPtr;
      end;
    end;

    // Add references
    if Assigned(FLocalParamsInfo) then
      AddRef(LocalParams^, FLocalParamsInfo);
  end;

  // Initialize context and instructions
  TSepiRuntimeContext.InitInstance(Context);
  TAbsoluteMemoryStream.InitInstance(Instructions);
  { Sure, this is dirty. As a matter of fact, we use TSepiRuntimeContext and
    TAbsoluteMemoryStream as though they were advanced records. Since none of
    them have got a destructor, this happens to be safe.
    Using this hack, the Sepi interpreter never allocates memory on the heap
    during execution by itself (the interpreted code could do it). This is all
    about having a fast execution. }

  try
    // Execute the method code
    Instructions.Create(Code);
    Context.Create(Self, Instructions, Parameters, Locals);
    Context.Execute(Code, Pointer(Cardinal(Code)+CodeSize));

    // Fetch result, if required (a result never requires initialization)
    if Assigned(Result) then
      Move(Locals^, Result^, FResultSize);
  finally
    // Finalize all local variables (including value parameters)
    if Assigned(FLocalParamsInfo) then
      Finalize(LocalParams^, FLocalParamsInfo);
    if Assigned(FParamsAddRef) then
      Finalize(Parameters^, FParamsAddRef);
    if Assigned(FLocalsInfo) then
      Finalize(Locals^, FLocalsInfo);

    // Finalize context and instructions
    Context.CleanupInstance;
    Instructions.CleanupInstance;
  end;

  // Deallocate locals from stack
  asm
        ADD     ESP,AllLocalsSize
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
constructor TSepiRuntimeContext.Create(ARuntimeMethod: TSepiRuntimeMethod;
  AInstructions: TAbsoluteMemoryStream; AParameters, ALocals: Pointer);
begin
  inherited Create;

  FRuntimeMethod := ARuntimeMethod;
  FRuntimeUnit := FRuntimeMethod.RuntimeUnit;
  FInstructions := AInstructions;
  FParameters := AParameters;
  FLocals := ALocals;
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
  Offset: Smallint;
begin
  Instructions.ReadBuffer(Offset, SizeOf(Smallint));
  Instructions.Seek(Offset, soFromCurrent);
end;

{*
  OpCode Jump If (True or False)
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeJumpIf(OpCode: TSepiOpCode);
var
  Offset: Smallint;
  TestPtr: PBoolean;
begin
  Instructions.ReadBuffer(Offset, SizeOf(Smallint));
  TestPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Boolean));

  case OpCode of
    ocJumpIfTrue:
      if TestPtr^ then
        Instructions.Seek(Offset, soFromCurrent);
    ocJumpIfFalse:
      if not TestPtr^ then
        Instructions.Seek(Offset, soFromCurrent);
  end;
end;

{*
  OpCode BasicCall
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeAddressCall(OpCode: TSepiOpCode);
var
  CallSettings: TSepiCallSettings;
  CallingConvention: TCallingConvention;
  RegUsage: Byte;
  ResultBehavior: TSepiTypeResultBehavior;
  OrdResultSize: Byte;
  AddressPtr: PPointer;
begin
  // Read the instruction
  Instructions.ReadBuffer(CallSettings, 1);
  CallSettingsDecode(CallSettings, CallingConvention,
    RegUsage, ResultBehavior);
  if ResultBehavior = rbOrdinal then
    Instructions.ReadBuffer(OrdResultSize, 1)
  else
    OrdResultSize := 0;
  AddressPtr := ReadAddress; // it would be foolish to have a constant here

  // Read params and call
  ReadParamsAndCall(AddressPtr^, CallingConvention, RegUsage, ResultBehavior,
    OrdResultSize);
end;

{*
  OpCode StaticCall
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeStaticCall(OpCode: TSepiOpCode);
var
  SepiMethod: TSepiMethod;
  ResultBehavior: TSepiTypeResultBehavior;
  OrdResultSize: Byte;
begin
  // Read the instruction
  RuntimeUnit.ReadRef(Instructions, SepiMethod);

  // Read params and call
  with SepiMethod, Signature do
  begin
    ResultBehavior := ReturnType.SafeResultBehavior;
    if ResultBehavior = rbOrdinal then
      OrdResultSize := ReturnType.Size
    else
      OrdResultSize := 0;

    ReadParamsAndCall(Code, CallingConvention, RegUsage,
      ResultBehavior, OrdResultSize);
  end;
end;

{*
  OpCode DynamicCall
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeDynamicCall(OpCode: TSepiOpCode);
var
  SepiMethod: TSepiMethod;
  SelfValue: Pointer;
  SelfClass: TClass;
  Address: Pointer;
  ResultBehavior: TSepiTypeResultBehavior;
  OrdResultSize: Byte;
begin
  // Read the instruction
  RuntimeUnit.ReadRef(Instructions, SepiMethod);
  SelfValue := PPointer(ReadAddress([aoAcceptZero]))^;

  // Find code address
  if SepiMethod.LinkKind = mlkInterface then
  begin
    // SelfValue is an interface, thus points to an IMT
    Address := PPointer(Integer(SelfValue^) + SepiMethod.IMTOffset)^;
  end else
  begin
    // SelfValue is an objet or a class

    // Find the class
    case SepiMethod.Signature.Kind of
      skObjectProcedure, skObjectFunction, skDestructor:
        SelfClass := TObject(SelfValue).ClassType;
      skConstructor, skClassProcedure, skClassFunction:
        SelfClass := TClass(SelfValue);
    else
      RaiseInvalidOpCode;
      SelfClass := nil; // avoid compiler warning
    end;

    // Find the method code
    case SepiMethod.LinkKind of
      mlkStatic: Address := SepiMethod.Code;
      mlkVirtual:
        Address := GetClassVirtualCode(SelfClass, SepiMethod.VMTOffset);
      mlkDynamic, mlkMessage:
        Address := GetClassDynamicCode(SelfClass, SepiMethod.DMTIndex);
    else
      RaiseInvalidOpCode;
      Address := nil; // avoid compiler warning
    end;
  end;

  // Read params and call
  with SepiMethod.Signature do
  begin
    ResultBehavior := ReturnType.SafeResultBehavior;
    if ResultBehavior = rbOrdinal then
      OrdResultSize := ReturnType.Size
    else
      OrdResultSize := 0;

    ReadParamsAndCall(Address, CallingConvention, RegUsage,
      ResultBehavior, OrdResultSize);
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
  SourcePtr := ReadAddress([aoAcceptAddressedConst]);

  DestPtr^ := SourcePtr;
end;

{*
  OpCode Move
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSimpleMove(OpCode: TSepiOpCode);
const
  ConstSizes: array[ocMoveByte..ocMoveWideStr] of Integer = (
    1, 2, 4, 8, 10, 0, 0
  );
var
  DestPtr: Pointer;
  SourcePtr: Pointer;
begin
  DestPtr := ReadAddress;
  if OpCode in [ocMoveVariant, ocMoveIntf] then
    SourcePtr := ReadAddress([aoAcceptZero])
  else
    SourcePtr := ReadAddress(aoAcceptAllConsts, ConstSizes[OpCode]);

  case OpCode of
    ocMoveByte:       Shortint     (DestPtr^) := Shortint     (SourcePtr^);
    ocMoveWord:       Smallint     (DestPtr^) := Smallint     (SourcePtr^);
    ocMoveDWord:      Longint      (DestPtr^) := Longint      (SourcePtr^);
    ocMoveQWord:      Int64        (DestPtr^) := Int64        (SourcePtr^);
    ocMoveExt:        Extended     (DestPtr^) := Extended     (SourcePtr^);
    ocMoveAnsiStr:    AnsiString   (DestPtr^) := AnsiString   (SourcePtr^);
    ocMoveWideStr:    WideString   (DestPtr^) := WideString   (SourcePtr^);
    ocMoveUnicodeStr: UnicodeStrDef(DestPtr^) := UnicodeStrDef(SourcePtr^);
    ocMoveVariant:    Variant      (DestPtr^) := Variant      (SourcePtr^);
    ocMoveIntf:       IInterface   (DestPtr^) := IInterface   (SourcePtr^);
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

  if Count <= SizeOf(ZeroMemory) then
    SourcePtr := ReadAddress(aoAcceptAllConsts, Count)
  else
    SourcePtr := ReadAddress([aoAcceptAddressedConst]);

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
  SourcePtr := ReadAddress(aoAcceptAllConsts, SepiType.Size);

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
  SourcePtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[FromType]);

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
  if OpCode in [ocSelfShl, ocSelfShr] then
    ValuePtr := ReadAddress(aoAcceptAllConsts, 1)
  else
    ValuePtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);

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
  ValuePtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);

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
  LeftPtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);
  if OpCode in [ocOtherShl, ocOtherShr] then
    RightPtr := ReadAddress(aoAcceptAllConsts, 1)
  else
    RightPtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);

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
  LeftPtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);
  RightPtr := ReadAddress(aoAcceptAllConsts, BaseTypeConstSizes[VarType]);

  Compare(OpCode, VarType, DestPtr^, LeftPtr^, RightPtr^);
end;

{*
  OpCode GetTypeInfo
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeGetTypeInfo(OpCode: TSepiOpCode);
var
  DestPtr: PPTypeInfo;
  SepiType: TSepiType;
begin
  DestPtr := ReadAddress;
  RuntimeUnit.ReadRef(Instructions, SepiType);

  DestPtr^ := SepiType.TypeInfo;
end;

{*
  OpCode GetDelphiClass
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeGetDelphiClass(OpCode: TSepiOpCode);
var
  DestPtr: ^TClass;
  SepiClass: TSepiClass;
begin
  DestPtr := ReadAddress;
  RuntimeUnit.ReadRef(Instructions, SepiClass);

  DestPtr^ := SepiClass.DelphiClass;
end;

{*
  OpCode GetMethodCode
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeGetMethodCode(OpCode: TSepiOpCode);
var
  DestPtr: PPointer;
  SepiMethod: TSepiMethod;
begin
  DestPtr := ReadAddress;
  RuntimeUnit.ReadRef(Instructions, SepiMethod);

  DestPtr^ := SepiMethod.Code;
end;

{*
  OpCode IsClass
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeIsClass(OpCode: TSepiOpCode);
var
  DestPtr: PBoolean;
  ObjectPtr: ^TObject;
  DelphiClass: TClass;
begin
  DestPtr := ReadAddress;
  ObjectPtr := ReadAddress([aoAcceptZero]);
  DelphiClass := ReadClassValue;

  DestPtr^ := ObjectPtr^ is DelphiClass;
end;

{*
  OpCode AsClass
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeAsClass(OpCode: TSepiOpCode);
var
  ObjectPtr: ^TObject;
  DelphiClass: TClass;
begin
  ObjectPtr := ReadAddress([aoAcceptZero]);
  DelphiClass := ReadClassValue;

  TObject(DelphiClass) := // just give something to the Delphi syntax
    ObjectPtr^ as DelphiClass;
end;

{*
  OpCode Raise
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeRaise(OpCode: TSepiOpCode);
var
  ExceptObjectPtr: Pointer;
begin
  ExceptObjectPtr := ReadAddress;

  raise TObject(ExceptObjectPtr^);
end;

{*
  OpCode Reraise
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeReraise(OpCode: TSepiOpCode);
begin
  raise TObject(AcquireExceptionObject);
end;

{*
  OpCode BeginTryExcept
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeTryExcept(OpCode: TSepiOpCode);
var
  TrySize, ExceptSize: Word;
  ExceptObjectPtr: ^TObject;
  TryPos, ExceptPos, EndPos: Pointer;
begin
  // Read instruction
  Instructions.ReadBuffer(TrySize, SizeOf(Word));
  Instructions.ReadBuffer(ExceptSize, SizeOf(Word));
  ExceptObjectPtr := ReadAddress([aoZeroAsNil]);

  // Compute block boundaries
  TryPos := Instructions.PointerPos;
  ExceptPos := Pointer(Cardinal(TryPos) + TrySize);
  EndPos := Pointer(Cardinal(ExceptPos) + ExceptSize);

  try
    // Execute try block
    Execute(TryPos, ExceptPos);

    // Skip except block
    if Instructions.PointerPos = ExceptPos then
      Instructions.PointerPos := EndPos;
  except
    // Set exception object, if required
    if Assigned(ExceptObjectPtr) then
      ExceptObjectPtr^ := ExceptObject;

    // Execute except block
    Instructions.PointerPos := ExceptPos;
    Execute(ExceptPos, EndPos);
  end;
end;

{*
  OpCode BeginTryFinally
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeTryFinally(OpCode: TSepiOpCode);
var
  TrySize, FinallySize: Word;
  TryPos, FinallyPos, EndPos, SavedPos: Pointer;
begin
  // Read instruction
  Instructions.ReadBuffer(TrySize, SizeOf(Word));
  Instructions.ReadBuffer(FinallySize, SizeOf(Word));

  // Compute block boundaries
  TryPos := Instructions.PointerPos;
  FinallyPos := Pointer(Cardinal(TryPos) + TrySize);
  EndPos := Pointer(Cardinal(FinallyPos) + FinallySize);

  try
    // Execute try block
    Execute(TryPos, FinallyPos);

    // Skip finally block
    if Instructions.PointerPos = FinallyPos then
      Instructions.PointerPos := EndPos;
  finally
    // Execute finally block and get back to current position
    SavedPos := Instructions.PointerPos;
    Instructions.PointerPos := FinallyPos;
    Execute(FinallyPos, EndPos);
    Instructions.PointerPos := SavedPos;
  end;
end;

{*
  OpCode MultiOn
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeMultiOn(OpCode: TSepiOpCode);
var
  Obj: TObject;
  Count: Integer;
  SepiClass: TSepiClass;
  Offset: Smallint;
begin
  // Read object pointer and count
  Obj := TObject(ReadAddress^);
  Count := 0;
  Instructions.ReadBuffer(Count, 1);

  // Look for a matching class, and jump if found
  while Count > 0 do
  begin
    RuntimeUnit.ReadRef(Instructions, SepiClass);

    if Obj is SepiClass.DelphiClass then
    begin
      // Found
      Instructions.ReadBuffer(Offset, SizeOf(Smallint));
      Instructions.Seek(Offset, soFromCurrent);
      Break;
    end else
    begin
      // Not found
      Instructions.Seek(SizeOf(Smallint), soFromCurrent);
    end;

    Dec(Count);
  end;
end;

{*
  OpCode SetInclude ou SetExclude
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSetIncludeExclude(OpCode: TSepiOpCode);
var
  SetPtr: ^TSet;
  ElemPtr: PByte;
begin
  // Read arguments
  SetPtr := ReadAddress;
  ElemPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Byte));

  // Execute the instruction
  if OpCode = ocSetInclude then
    Include(SetPtr^, ElemPtr^)
  else
    Exclude(SetPtr^, ElemPtr^);
end;

{*
  OpCode SetIn
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSetIn(OpCode: TSepiOpCode);
var
  DestPtr: PBoolean;
  SetPtr: ^TSet;
  ElemPtr: PByte;
begin
  // Read arguments
  DestPtr := ReadAddress;
  SetPtr := ReadAddress(aoAcceptNonCodeConsts);
  ElemPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Byte));

  // Execute the instruction
  DestPtr^ := ElemPtr^ in SetPtr^;
end;

{*
  OpCode SetElem
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSetElem(OpCode: TSepiOpCode);
var
  SetSize: Byte;
  SetPtr: Pointer;
  ElemPtr: PByte;
begin
  // Read arguments
  Instructions.ReadBuffer(SetSize, SizeOf(Byte));
  SetPtr := ReadAddress;
  ElemPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Byte));

  // Execute instruction
  SetElem(SetPtr^, ElemPtr^, SetSize);
end;

{*
  OpCode SetRange
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSetRange(OpCode: TSepiOpCode);
var
  SetSize: Byte;
  SetPtr: Pointer;
  LoPtr, HiPtr: PByte;
  TempSet: TSet;
begin
  // Read arguments
  Instructions.ReadBuffer(SetSize, SizeOf(Byte));
  SetPtr := ReadAddress;
  LoPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Byte));
  HiPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Byte));

  // Execute instruction
  if OpCode = ocSetRange then
    SetRange(SetPtr^, LoPtr^, HiPtr^, SetSize)
  else
  begin
    SetRange(TempSet, LoPtr^, HiPtr^, SetSize);
    SetUnion(SetPtr^, TempSet, SetSize);
  end;
end;

{*
  OpCode SetEquals, SetNotEquals ou SetContained
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSetCmpOp(OpCode: TSepiOpCode);
var
  SetSize: Byte;
  DestPtr: PBoolean;
  LeftSetPtr, RightSetPtr: Pointer;
begin
  // Read arguments
  Instructions.ReadBuffer(SetSize, SizeOf(Byte));
  DestPtr := ReadAddress;
  LeftSetPtr := ReadAddress(aoAcceptAllConsts, SetSize);
  RightSetPtr := ReadAddress(aoAcceptAllConsts, SetSize);

  // Execute instruction
  case OpCode of
    ocSetEquals:
      DestPtr^ := SetEquals(LeftSetPtr^, RightSetPtr^, SetSize);
    ocSetNotEquals:
      DestPtr^ := not SetEquals(LeftSetPtr^, RightSetPtr^, SetSize);
    ocSetContained:
      DestPtr^ := SetContained(LeftSetPtr^, RightSetPtr^, SetSize);
  else
    Assert(False);
  end;
end;

{*
  OpCode SetSelfIntersect, SetSelfUnion ou SetSelfSubtract
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSetSelfOp(OpCode: TSepiOpCode);
var
  SetSize: Byte;
  DestPtr, SourcePtr: Pointer;
begin
  // Read arguments
  Instructions.ReadBuffer(SetSize, SizeOf(Byte));
  DestPtr := ReadAddress;
  SourcePtr := ReadAddress(aoAcceptAllConsts, SetSize);

  // Execute instruction
  case OpCode of
    ocSetSelfIntersect:
      SetIntersect(DestPtr^, SourcePtr^, SetSize);
    ocSetSelfUnion:
      SetUnion(DestPtr^, SourcePtr^, SetSize);
    ocSetSelfSubtract:
      SetSub(DestPtr^, SourcePtr^, SetSize);
  else
    Assert(False);
  end;
end;

{*
  OpCode SetOtherIntersect, SetOtherUnion, SetOtherSubract
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSetOtherOp(OpCode: TSepiOpCode);
var
  SetSize: Byte;
  DestPtr, LeftPtr, RightPtr: Pointer;
  TempSet: TSet;
begin
  // Read arguments
  Instructions.ReadBuffer(SetSize, SizeOf(Byte));
  DestPtr := ReadAddress;
  LeftPtr := ReadAddress(aoAcceptAllConsts, SetSize);
  RightPtr := ReadAddress(aoAcceptAllConsts, SetSize);

  // Execute instruction

  Move(LeftPtr^, TempSet, SetSize);

  case OpCode of
    ocSetOtherIntersect:
      SetIntersect(TempSet, RightPtr^, SetSize);
    ocSetOtherUnion:
      SetUnion(TempSet, RightPtr^, SetSize);
    ocSetOtherSubtract:
      SetSub(TempSet, RightPtr^, SetSize);
  else
    Assert(False);
  end;

  Move(TempSet, DestPtr^, SetSize);
end;

{*
  OpCode SetExpand
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeSetExpand(OpCode: TSepiOpCode);
var
  DestPtr, SourcePtr: Pointer;
  Lo, Hi: Byte;
begin
  // Read arguments
  DestPtr := ReadAddress;
  SourcePtr := ReadAddress(aoAcceptNonCodeConsts);
  Instructions.ReadBuffer(Lo, SizeOf(Byte));
  Instructions.ReadBuffer(Hi, SizeOf(Byte));

  // Execute instruction
  SetExpand(SourcePtr^, DestPtr^, Lo, Hi);
end;

{*
  OpCode ASL, WSL, DAL et DAH
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeValueToIntStdFunction(OpCode: TSepiOpCode);
var
  DestPtr: PInteger;
  ValuePtr: Pointer;
begin
  // Read arguments
  DestPtr := ReadAddress;
  ValuePtr := ReadAddress(aoAcceptNonCodeConsts);

  // Execute instruction
  case OpCode of
    ocAnsiStrLength: DestPtr^ := Length(AnsiString(ValuePtr^));
    ocWideStrLength: DestPtr^ := Length(WideString(ValuePtr^));
    ocUnicodeStrLength: DestPtr^ := Length(UnicodeStrDef(ValuePtr^));
    ocDynArrayLength: DestPtr^ := DynArrayLength(ValuePtr^);
    ocDynArrayHigh: DestPtr^ := DynArrayHigh(ValuePtr^);
  end;
end;

{*
  OpCode ASSL et WSSL
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeStrSetLength(OpCode: TSepiOpCode);
var
  StrPtr: Pointer;
  LenPtr: PInteger;
begin
  // Read arguments
  StrPtr := ReadAddress;
  LenPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Integer));

  // Execute instruction
  case OpCode of
    ocAnsiStrSetLength: SetLength(AnsiString(StrPtr^), LenPtr^);
    ocWideStrSetLength: SetLength(WideString(StrPtr^), LenPtr^);
    ocUnicodeStrSetLength: SetLength(UnicodeStrDef(StrPtr^), LenPtr^);
  end;
end;

{*
  OpCode DASL
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeDynArraySetLength(OpCode: TSepiOpCode);
var
  SepiType: TSepiType;
  DynArrayPtr: Pointer;
  DimCount, I: Integer;
  Dimensions: TIntegerDynArray;
  DimPtr: PInteger;
begin
  // Read arguments
  RuntimeUnit.ReadRef(Instructions, SepiType);
  DynArrayPtr := ReadAddress;
  DimCount := 0;
  Instructions.ReadBuffer(DimCount, 1);

  // Read dimensions
  SetLength(Dimensions, DimCount);
  for I := 0 to DimCount-1 do
  begin
    DimPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Integer));
    Dimensions[I] := DimPtr^;
  end;

  // Execute instruction
  DynArraySetLength(Pointer(DynArrayPtr^), SepiType.TypeInfo, DimCount,
    @Dimensions[0]);
end;

{*
  OpCode ASCP et WSCP
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeStrCopy(OpCode: TSepiOpCode);
var
  DestPtr, SrcPtr: Pointer;
  IndexPtr, CountPtr: PInteger;
  Index, Count: Integer;
begin
  // Read arguments
  DestPtr := ReadAddress;
  SrcPtr := ReadAddress(aoAcceptNonCodeConsts);
  IndexPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Integer));
  CountPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Integer));

  Index := IndexPtr^;
  Count := CountPtr^;

  // Execute instruction
  case OpCode of
    ocAnsiStrCopy:
      AnsiString(DestPtr^) := Copy(AnsiString(SrcPtr^), Index, Count);
    ocWideStrCopy:
      WideString(DestPtr^) := Copy(WideString(SrcPtr^), Index, Count);
    ocUnicodeStrCopy:
      UnicodeStrDef(DestPtr^) := Copy(UnicodeStrDef(SrcPtr^), Index, Count);
  end;
end;

{*
  OpCode DACP version tableau entier
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeDynArrayCopy(OpCode: TSepiOpCode);
var
  SepiType: TSepiType;
  DestPtr, SrcPtr: PPointer;
begin
  // Read arguments
  RuntimeUnit.ReadRef(Instructions, SepiType);
  DestPtr := ReadAddress;
  SrcPtr := ReadAddress(aoAcceptNonCodeConsts);

  // Execute instruction
  DynArrayCopy(SrcPtr^, SepiType.TypeInfo, DestPtr^);
end;

{*
  OpCode DACP version range
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeDynArrayCopyRange(OpCode: TSepiOpCode);
var
  SepiType: TSepiType;
  DestPtr, SrcPtr: PPointer;
  IndexPtr, CountPtr: PInteger;
begin
  // Read arguments
  RuntimeUnit.ReadRef(Instructions, SepiType);
  DestPtr := ReadAddress;
  SrcPtr := ReadAddress(aoAcceptNonCodeConsts);
  IndexPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Integer));
  CountPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Integer));

  // Execute instruction
  DynArrayCopyRange(SrcPtr^, SepiType.TypeInfo, IndexPtr^, CountPtr^, DestPtr^);
end;

{*
  OpCode RRFMR
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeRoutineRefFromMethodRef(
  OpCode: TSepiOpCode);
var
  SignatureComponent: TSepiComponent;
  Signature: TSepiSignature;
  DestPtr, SrcPtr: PPointer;
  MethodRef: TMethod;
begin
  // Read arguments
  RuntimeUnit.ReadRef(Instructions, SignatureComponent);
  DestPtr := ReadAddress;
  SrcPtr := ReadAddress(aoAcceptNonCodeConsts);

  // Extract signature
  if SignatureComponent is TSepiMethod then
    Signature := TSepiMethod(SignatureComponent).Signature
  else if SignatureComponent is TSepiMethodRefType then
    Signature := TSepiMethodRefType(SignatureComponent).Signature
  else
  begin
    RaiseInvalidOpCode;
    Signature := nil; // avoid compiler warning
  end;

  // Read method ref
  Move(SrcPtr^, MethodRef,
    IIF(Signature.Kind in skWithSelfParam, SizeOf(TMethod), SizeOf(Pointer)));

  // Execute instruction
  MakeRoutineRefFromMethodRef(Signature, IInterface(DestPtr^), MethodRef);
end;

{*
  OpCode IFC
  @param OpCode   OpCode
*}
procedure TSepiRuntimeContext.OpCodeIntfFromClass(OpCode: TSepiOpCode);
var
  Offset: Integer;
  DestPtr: ^IInterface;
  SourcePtr: PInteger;
  SrcIntf: Integer;
begin
  // Read operands
  Instructions.ReadBuffer(Offset, 4);
  DestPtr := ReadAddress;
  SourcePtr := ReadAddress([aoAcceptAddressedConst]);

  // Execute instruction
  SrcIntf := SourcePtr^ + Offset;
  DestPtr^ := IInterface(SrcIntf);
end;

{*
  Lit une adresse de base depuis les instructions
  @param Options       Options de lecture d'addresse
  @param ConstSize     Taille d'une constante dans le code
  @param MemorySpace   Espace d'adressage
  @return Adresse de base lue
*}
function TSepiRuntimeContext.ReadBaseAddress(Options: TSepiAddressOptions;
  ConstSize: Integer; MemorySpace: TSepiMemorySpace): Pointer;
var
  ByteOffset: Byte;
  WordOffset: Word;
  GlobalConst: TSepiConstant;
  GlobalVar: TSepiVariable;
begin
  // Read base address
  case MemorySpace of
    msZero:
    begin
      if aoZeroAsNil in Options then
      begin
        // Treat constant as nil return value
        Result := nil;
      end else
      begin
        if not (aoAcceptZero in Options) then
          RaiseInvalidOpCode;
        Result := @ZeroMemory;
      end;
    end;
    msConstant:
    begin
      // Read the constant directly into the code
      if (not (aoAcceptConstInCode in Options)) or (ConstSize <= 0) then
        RaiseInvalidOpCode;
      Result := Instructions.PointerPos;
      Instructions.Seek(ConstSize, soFromCurrent);
    end;
    msLocalsBase:
    begin
      // Local variables, no offset
      Result := Locals;
    end;
    msLocalsByte:
    begin
      // Local variables, byte-offset
      Instructions.ReadBuffer(ByteOffset, 1);
      Result := Locals;
      Inc(Integer(Result), ByteOffset);
    end;
    msLocalsWord:
    begin
      // Local variables, word-offset
      Instructions.ReadBuffer(WordOffset, 2);
      Result := Locals;
      Inc(Integer(Result), WordOffset);
    end;
    msParamsBase:
    begin
      // Parameters, no offset
      Result := Parameters;
    end;
    msParamsByte:
    begin
      // Parameters, byte-offset
      Instructions.ReadBuffer(ByteOffset, 1);
      Result := Parameters;
      Inc(Integer(Result), ByteOffset);
    end;
    msParamsWord:
    begin
      // Parameters, word-offset
      Instructions.ReadBuffer(WordOffset, 2);
      Result := Parameters;
      Inc(Integer(Result), WordOffset);
    end;
    msTrueConst:
    begin
      // Reference to TSepiConstant
      if not (aoAcceptTrueConst in Options) then
        RaiseInvalidOpCode;
      RuntimeUnit.ReadRef(Instructions, GlobalConst);
      Result := GlobalConst.ValuePtr;
    end;
    msVariable:
    begin
      // Reference to TSepiVariable
      RuntimeUnit.ReadRef(Instructions, GlobalVar);
      Result := GlobalVar.Value;
      if GlobalVar.IsConst and (not (aoAcceptAddressedConst in Options)) then
        RaiseInvalidOpCode;
    end;
  else
    RaiseInvalidOpCode;
    Result := nil; // avoid compiler warning
  end;
end;

{*
  Lit une opération sur une adresse et l'applique à une adresse donnée
  @param Address   Adresse à modifier
*}
procedure TSepiRuntimeContext.ReadAddressOperation(var Address: Pointer);
var
  IntAddress: Integer absolute Address;
  AddrDerefAndOp: TSepiAddressDerefAndOp;
  AddrDereference: TSepiAddressDereference;
  AddrOperation: TSepiAddressOperation;
  ShortOffset: Shortint;
  SmallOffset: Smallint;
  LongOffset: Longint;
  OffsetPtr: Pointer;
  ByteFactor: Byte;
  LongFactor: Longint;
begin
  // Read deref and op
  Instructions.ReadBuffer(AddrDerefAndOp, SizeOf(TSepiAddressDerefAndOp));
  AddressDerefAndOpDecode(AddrDerefAndOp, AddrDereference, AddrOperation);

  // Handle dereference
  case AddrDereference of
    adNone: ;
    adSimple: Address := PPointer(Address)^;
    adDouble: Address := PPointer(PPointer(Address)^)^;
  else
    RaiseInvalidOpCode;
  end;

  // Handle operation
  case AddrOperation of
    aoNone: ;
    aoPlusConstShortint:
    begin
      // Read a Shortint from code, and add it to the address
      Instructions.ReadBuffer(ShortOffset, SizeOf(Shortint));
      Inc(IntAddress, ShortOffset);
    end;
    aoPlusConstSmallint:
    begin
      // Read a Smallint from code, and add it to the address
      Instructions.ReadBuffer(SmallOffset, SizeOf(Smallint));
      Inc(IntAddress, SmallOffset);
    end;
    aoPlusConstLongint:
    begin
      // Read a Longint from code, and add it to the address
      Instructions.ReadBuffer(LongOffset, SizeOf(Longint));
      Inc(IntAddress, LongOffset);
    end;
    aoPlusMemShortint:
    begin
      // Read a Shortint from memory, and add it to the address
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Shortint));
      Inc(IntAddress, PShortint(OffsetPtr)^);
    end;
    aoPlusMemSmallint:
    begin
      // Read a Smallint from memory, and add it to the address
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Smallint));
      Inc(IntAddress, PSmallint(OffsetPtr)^);
    end;
    aoPlusMemLongint:
    begin
      // Read a Longint from memory, and add it to the address
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Longint));
      Inc(IntAddress, PLongint(OffsetPtr)^);
    end;
    aoPlusConstTimesMemShortint:
    begin
      { Read a Byte from code and a Shortint from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(ByteFactor, 1);
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Shortint));
      Inc(IntAddress, ByteFactor * PShortint(OffsetPtr)^);
    end;
    aoPlusConstTimesMemSmallint:
    begin
      { Read a Byte from code and a Smallint from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(ByteFactor, 1);
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Smallint));
      Inc(IntAddress, ByteFactor * PSmallint(OffsetPtr)^);
    end;
    aoPlusConstTimesMemLongint:
    begin
      { Read a Byte from code and a Longint from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(ByteFactor, 1);
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Longint));
      Inc(IntAddress, ByteFactor * PLongint(OffsetPtr)^);
    end;
    aoPlusConstTimesMemByte:
    begin
      { Read a Byte from code and a Byte from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(ByteFactor, 1);
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Byte));
      Inc(IntAddress, ByteFactor * PByte(OffsetPtr)^);
    end;
    aoPlusConstTimesMemWord:
    begin
      { Read a Byte from code and a Word from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(ByteFactor, 1);
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Word));
      Inc(IntAddress, ByteFactor * PWord(OffsetPtr)^);
    end;
    aoPlusConstTimesMemLongWord:
    begin
      { Read a Byte from code and a LongWord from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(ByteFactor, 1);
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(LongWord));
      Inc(IntAddress, ByteFactor * PLongWord(OffsetPtr)^);
    end;
    aoPlusLongConstTimesMemShortint:
    begin
      { Read a Longint from code and a Shortint from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(LongFactor, SizeOf(Longint));
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Shortint));
      Inc(IntAddress, LongFactor * PShortint(OffsetPtr)^);
    end;
    aoPlusLongConstTimesMemSmallint:
    begin
      { Read a Longint from code and a Smallint from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(LongFactor, SizeOf(Longint));
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Smallint));
      Inc(IntAddress, LongFactor * PSmallint(OffsetPtr)^);
    end;
    aoPlusLongConstTimesMemLongint:
    begin
      { Read a Longint from code and a Longint from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(LongFactor, SizeOf(Longint));
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Longint));
      Inc(IntAddress, LongFactor * PLongint(OffsetPtr)^);
    end;
    aoPlusLongConstTimesMemByte:
    begin
      { Read a Longint from code and a Byte from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(LongFactor, SizeOf(Longint));
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Byte));
      Inc(IntAddress, LongFactor * PByte(OffsetPtr)^);
    end;
    aoPlusLongConstTimesMemWord:
    begin
      { Read a Longint from code and a Word from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(LongFactor, SizeOf(Longint));
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(Word));
      Inc(IntAddress, LongFactor * PWord(OffsetPtr)^);
    end;
    aoPlusLongConstTimesMemLongWord:
    begin
      { Read a Longint from code and a LongWord from memory. Then, multiply
        them and add the result to the address. }
      Instructions.ReadBuffer(LongFactor, SizeOf(Longint));
      OffsetPtr := ReadAddress(aoAcceptAllConsts, SizeOf(LongWord));
      Inc(IntAddress, LongFactor * PLongWord(OffsetPtr)^);
    end;
  else
    RaiseInvalidOpCode;
  end;
end;

{*
  Lit l'adresse d'une zone mémoire depuis le flux d'instructions
  @param Options     Options de lecture d'addresse
  @param ConstSize   Taille d'une constante dans le code
  @return Adresse lue
*}
function TSepiRuntimeContext.ReadAddress(Options: TSepiAddressOptions = [];
  ConstSize: Integer = 0): Pointer;
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
  if Result = nil then
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
function TSepiRuntimeContext.ReadClassValue: TClass;
var
  ClassPtr: Pointer;
  SepiClass: TSepiClass;
begin
  ClassPtr := ReadAddress([aoZeroAsNil]);

  if ClassPtr = nil then
  begin
    RuntimeUnit.ReadRef(Instructions, SepiClass);
    Result := SepiClass.DelphiClass;
  end else
    Result := TClass(ClassPtr^);
end;

{*
  Lit les paramètres (et résultat) depuis les instructions et fait l'appel
  @param Address             Pointeur sur le code de la procédure à appeler
  @param CallingConvention   Convention d'appel à utiliser
  @param RegUsage            Nombre de registres utilsés
  @param ResultBehavior      Comportement du résultat
  @param OrdResultSize       Si résultat ordinal, taille en octets de celui-ci
*}
procedure TSepiRuntimeContext.ReadParamsAndCall(Address: Pointer;
  CallingConvention: TCallingConvention; RegUsage: Byte;
  ResultBehavior: TSepiTypeResultBehavior; OrdResultSize: Byte);

type
  TOrdResult = packed record
    case Integer of
      0: (EAX, EDX: Longint);
      2: (OrdinalRes: Longint);
      3: (Int64Res: Int64);
  end;

const
  MaxParamCountForByteSize = 255 div 3; // 3 is max param size (Extended)
  MaxParamSize = 10;
  TrueParamSizes: array[0..10] of Integer = (
    4 {address}, 4, 4, 4, 4, 8, 8, 8, 8, 12, 12
  );

var
  ParamCount: Integer;
  ParamsSize: Integer;
  Parameters: Pointer;
  ParamPos, ParamIndex: Integer;
  ParamSize: TSepiParamSize;
  ParamPtr: Pointer;
  Result: Pointer;
  OrdResult: TOrdResult;
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

  // Allocate parameters on stack
  asm
        SUB     ESP,ParamsSize
        MOV     Parameters,ESP
  end;

  // Read parameters
  ParamPos := 0;
  for ParamIndex := 0 to ParamCount-1 do
  begin
    // Read param size
    Instructions.ReadBuffer(ParamSize, 1);
    if ParamSize > MaxParamSize then
      RaiseInvalidOpCode;

    // Read parameter
    if ParamSize = psByAddress then
    begin
      PPointer(Integer(Parameters)+ParamPos)^ :=
        ReadAddress(aoAcceptNonCodeConsts);
      Inc(ParamPos, SizeOf(Pointer));
    end else
    begin
      ParamPtr := ReadAddress(aoAcceptAllConsts, ParamSize);
      Move(ParamPtr^, Pointer(Integer(Parameters)+ParamPos)^, ParamSize);
      Inc(ParamPos, TrueParamSizes[ParamSize]);
    end;
  end;

  // Read result
  Result := ReadAddress([aoZeroAsNil]);

  // Actual call
  asm
        MOV     CL,RegUsage
        TEST    CL,CL
        JZ      @@doCall
        POP     EAX
        DEC     CL
        JZ      @@doCall
        POP     EDX
        DEC     CL
        JZ      @@doCall
        POP     ECX
@@doCall:
        CALL    Address
        MOV     DWORD PTR OrdResult[4],EDX
        MOV     DWORD PTR OrdResult[0],EAX
  end;

  // Release parameters - only with cdecl calling convention
  // Note: RegUsage is always 0 in this case, thus ParamsSize is still valid
  if CallingConvention = ccCDecl then
  asm
        ADD     ESP,ParamsSize
  end;

  // Handle result
  if Assigned(Result) then
  begin
    // Get result
    case ResultBehavior of
      // if rbNone and Result is specified, it must be a construtor, so TObject
      rbNone:     Longint (Result^) := OrdResult.OrdinalRes;
      rbInt64:    Int64   (Result^) := OrdResult.Int64Res;
      rbSingle:   Single  (Result^) := GetSingleResult;
      rbDouble:   Double  (Result^) := GetDoubleResult;
      rbExtended: Extended(Result^) := GetExtendedResult;
      rbCurrency: Currency(Result^) := GetCurrencyResult;

      rbOrdinal: Move(OrdResult.OrdinalRes, Result^, OrdResultSize);
    end;
  end else
  begin
    // Discard result if float
    if ResultBehavior in [rbSingle, rbDouble, rbExtended, rbCurrency] then
      GetExtendedResult;
  end;
end;

{*
  Pointeur sur la prochaine instruction à exécuter
  @return Pointeur sur la prochaine instruction à exécuter
*}
function TSepiRuntimeContext.GetNextInstruction: Pointer;
begin
  Result := Instructions.PointerPos;
end;

{*
  Exécute les instructions jusqu'à sortir du bloc
  @param BlockBegin   Début du bloc
  @param BlockEnd     Fin du bloc
*}
procedure TSepiRuntimeContext.Execute(BlockBegin, BlockEnd: Pointer);
var
  OpCode: TSepiOpCode;
begin
  while (Cardinal(Instructions.PointerPos) < Cardinal(BlockEnd)) and
    (Cardinal(Instructions.PointerPos) >= Cardinal(BlockBegin)) do
  begin
    // Debug event
    if Assigned(RuntimeUnit.OnDebug) then
      RuntimeUnit.OnDebug(Self);

    // Execute instruction
    Instructions.ReadBuffer(OpCode, SizeOf(TSepiOpCode));
    OpCodeProcs[OpCode](Self, OpCode);
  end;
end;

initialization
  InitOpCodeProcs;
end.

